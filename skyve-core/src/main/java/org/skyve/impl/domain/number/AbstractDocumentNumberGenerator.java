package org.skyve.impl.domain.number;

import java.util.Formatter;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.app.admin.DocumentNumber;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.number.NumberGenerator;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;

public abstract class AbstractDocumentNumberGenerator implements NumberGenerator {
	@SuppressWarnings("static-method")
	protected String getNextNumber(Persistence pers,
									String prefix,
									String moduleName,
									String documentName,
									String fieldName,
									int numberLength) {
		User user = pers.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(AppConstants.ADMIN_MODULE_NAME);
		Document document = module.getDocument(customer, AppConstants.DOCUMENT_NUMBER_DOCUMENT_NAME);
		String nextNumber = "0";
		String lastNumber = "0";

		DocumentNumber dN = null;
		try {
			DocumentQuery qN = pers.newDocumentQuery(AppConstants.ADMIN_MODULE_NAME, AppConstants.DOCUMENT_NUMBER_DOCUMENT_NAME);
			qN.getFilter().addEquals(AppConstants.MODULE_NAME_ATTRIBUTE_NAME, moduleName);
			qN.getFilter().addEquals(AppConstants.DOCUMENT_NAME_ATTRIBUTE_NAME, documentName);
			qN.getFilter().addEquals(AppConstants.SEQUENCE_NAME_ATTRIBUTE_NAME, fieldName);

			// temporarily escalate access to the Document Number sequences
			pers.setDocumentPermissionScopes(DocumentPermissionScope.customer);
			// bean or the interface
			List<DocumentNumber> num = null;
			try {
				num = qN.beanResults();
			}
			finally {
				pers.resetDocumentPermissionScopes();
			}

			if (num.isEmpty()) {
				// Check if sequence name is a field in that table
				boolean isField = false;
				for (Attribute attribute : document.getAttributes()) {
					if (attribute.getName().equals(fieldName)) {
						isField = true;
						break;
					}
				}

				if (isField) {
					// first hit - go lookup max number from table
					DocumentQuery query = pers.newDocumentQuery(moduleName, documentName);
					query.addAggregateProjection(AggregateFunction.Max, fieldName, "MaxNumber");

					List<Bean> beans = query.projectedResults();
					if (! beans.isEmpty()) {
						Object o = Binder.get(beans.get(0), "MaxNumber");
						if (o instanceof Integer) {
							lastNumber = ((Integer) Binder.get(beans.get(0), "MaxNumber")).toString();
						}
						else {
							lastNumber = (String) Binder.get(beans.get(0), "MaxNumber");
						}
					}
				}

				// create a new document number record
				try {
					dN = document.newInstance(user);
					dN.setModuleName(moduleName);
					dN.setDocumentName(documentName);
					dN.setSequenceName(fieldName);
				}
				catch (Exception e) {
					throw new DomainException("Could not instantiate a new document number record", e);
				}
			}
			else {
				dN = num.get(0);
				dN = pers.retrieveAndLock(document, dN.getBizId()); // issue a row-level lock
				lastNumber = dN.getDocumentNumber();
			}
			// just update from the document Number
			nextNumber = incrementAlpha(prefix, lastNumber, numberLength);
			dN.setDocumentNumber(nextNumber);

			pers.preMerge(document, dN);
			pers.upsertBeanTuple(dN);
			pers.postMerge(document, dN);
		}
		finally {
			if (dN != null) {
				pers.evictCached(dN);
			}
		}

		return nextNumber;
	}

	/**
	 * Returns the next alpha value - ie A00A1 becomes A00A2 etc
	 *
	 * @param suppliedPrefix
	 *        - if the sequence value has a known prefix before the number,
	 *        eg INV0001 has a prefix of "INV"
	 * @param lastNumber
	 *        - the number to increment
	 * @param numberLength
	 *        - the minimum length of the number when specified as a string
	 *
	 * @return - the next number
	 */
	protected static String incrementAlpha(String suppliedPrefix, String lastNumber, int numberLength) {
		String newNumber = "";
		String nonNumeric = lastNumber;
		Integer value = Integer.valueOf(1);
		String prefix;
		if (suppliedPrefix != null) {
			prefix = suppliedPrefix;
		} else {
			prefix = "";
		}

		if (lastNumber != null) {
			String[] parts = (new StringBuilder(" ").append(lastNumber)).toString().split("\\D\\d+$");

			// cater for alpha prefix
			if (parts.length > 0 && parts[0].length() < lastNumber.length()) {
				String numberPart = lastNumber.substring(parts[0].length(), lastNumber.length());
				nonNumeric = lastNumber.substring(0, parts[0].length());

				value = Integer.valueOf(Integer.parseInt(numberPart) + 1);

				// cater for purely numeric prefix
			} else if (prefix.matches("^\\d+$") && lastNumber.matches("^\\d+$") && !"0".equals(lastNumber)) {
				int len = prefix.length();
				value = Integer.valueOf(Integer.parseInt(lastNumber.substring(len)) + 1);
				nonNumeric = prefix;

				// cater for numeric only
			} else if (lastNumber.matches("^\\d+$")) {
				nonNumeric = prefix;
				value = Integer.valueOf(Integer.parseInt(lastNumber) + 1);
			}
		} else {
			nonNumeric = prefix;
		}

		// now put prefix and value together
		int newLength = (nonNumeric.length() + value.toString().length() > numberLength
				? nonNumeric.length() + value.toString().length()
				: numberLength);

		StringBuilder sb = new StringBuilder(newLength + 1);
		try (Formatter f = new Formatter(sb)) {
			newNumber = nonNumeric
					+ f.format(new StringBuilder("%1$").append(newLength - nonNumeric.length()).append("s").toString(),
							value.toString()).toString().replace(" ", "0");
		}

		return newNumber;
	}
}
