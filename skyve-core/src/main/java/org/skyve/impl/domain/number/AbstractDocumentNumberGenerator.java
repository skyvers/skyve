package org.skyve.impl.domain.number;

import java.util.Formatter;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.app.admin.DocumentNumber;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.UniqueConstraintViolationException;
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

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Base implementation of {@link NumberGenerator} that reads and increments sequence
 * counters stored in the {@code DocumentNumber} admin document.
 *
 * <p>The counter for a {@code (moduleName, documentName, fieldName)} triple is
 * persisted in the database. On first access the implementation bootstraps from
 * the maximum value already present in the field's column so that generated numbers
 * never collide with existing data.
 *
 * <p>Threading: thread-safe, and safe across a cluster of servers sharing one database.
 * Instances hold no state; mutual exclusion is in the database, through the
 * {@link Persistence} supplied to {@link #getNextNumber}. An existing row is read with a
 * pessimistic write lock ({@link Persistence#retrieveAndLock}: {@code SELECT ... FOR UPDATE}
 * or the dialect's equivalent), so concurrent transactions increment it one at a time, each
 * from the number the previous one committed, and each holds the lock until its transaction
 * ends.
 *
 * <p>Two transactions that both find no row both try to create it. The {@code DocumentNumber}
 * document's {@code uniqueSequence} constraint makes the second wait (up to the database's
 * lock timeout) until the first ends; if the first committed, the second's check fails with a
 * {@link UniqueConstraintViolationException}, which is caught, and the second then locks and
 * increments the row the first created. The constraint check reads committed data, so this
 * holds at the READ COMMITTED isolation level, the default for H2, PostgreSQL, Oracle and
 * SQL Server.
 *
 * <p>Subclasses must implement {@link NumberGenerator#next} using the
 * {@link #getNextNumber} helper, supplying the appropriate
 * {@link org.skyve.persistence.Persistence} instance.
 *
 * @see DocumentNumberGenerator
 * @see NumberGeneratorStaticSingleton
 */
public abstract class AbstractDocumentNumberGenerator implements NumberGenerator {
	private static final String NUMERIC_PATTERN = "^\\d+$";

	/**
	 * Takes the next number of a sequence: increments its {@code DocumentNumber} row,
	 * creating the row on first use, and returns the new value.
	 *
	 * <p>Precondition: {@code pers} has an active transaction.
	 *
	 * <p>Side effects: inserts or updates the sequence's {@code DocumentNumber} row through
	 * {@code pers}, so the number is committed or rolled back with {@code pers}'s
	 * transaction, and evicts the row from {@code pers}'s first-level cache.
	 *
	 * <p>Threading: concurrent calls, in any threads or servers sharing the database, return
	 * distinct numbers; see the class comment. The call blocks while another transaction
	 * holds the sequence's row lock, or is creating its row, until that transaction ends,
	 * and from then on holds that lock itself until {@code pers}'s transaction ends.
	 *
	 * @param pers the persistence whose transaction takes the number
	 * @param prefix the prefix of the sequence's values, eg {@code "INV"} for {@code INV0001},
	 *        or {@code null} for none
	 * @param moduleName the module of the numbered document
	 * @param documentName the numbered document
	 * @param fieldName the field or sequence name
	 * @param numberLength the minimum length of the number, zero-padded after the prefix
	 * @return the next number of the sequence
	 * @throws UniqueConstraintViolationException if creating the sequence's row conflicts with
	 *         another transaction's row a second time, as when the row is deleted while in use
	 */
	protected @Nonnull String getNextNumber(@Nonnull Persistence pers,
											@Nullable String prefix,
											@Nonnull String moduleName,
											@Nonnull String documentName,
											@Nonnull String fieldName,
											int numberLength) {
		return getNextNumber(pers, prefix, moduleName, documentName, fieldName, numberLength, true);
	}

	/**
	 * Takes the next number of a sequence as {@link #getNextNumber(Persistence, String, String, String, String, int)}
	 * does, the second attempt finding the row another transaction created first.
	 *
	 * @param retryCreation whether a conflicting creation is followed by a second attempt
	 *        ({@code true} on the first attempt) or throws ({@code false} on the second)
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	private @Nonnull String getNextNumber(@Nonnull Persistence pers,
											@Nullable String prefix,
											@Nonnull String moduleName,
											@Nonnull String documentName,
											@Nonnull String fieldName,
											int numberLength,
											boolean retryCreation) {
		User user = pers.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(AppConstants.ADMIN_MODULE_NAME);
		Document document = module.getDocument(customer, AppConstants.DOCUMENT_NUMBER_DOCUMENT_NAME);
		String nextNumber = "0";
		String lastNumber = "0";

		DocumentNumber dN = null;
		try {
			// temporarily escalate access to the Document Number sequences
			@SuppressWarnings("null") // always returns a list
			@Nonnull List<DocumentNumber> num = pers.withDocumentPermissionScopes(DocumentPermissionScope.customer, p -> {
				DocumentQuery qN = pers.newDocumentQuery(AppConstants.ADMIN_MODULE_NAME, AppConstants.DOCUMENT_NUMBER_DOCUMENT_NAME);
				qN.getFilter().addEquals(AppConstants.MODULE_NAME_ATTRIBUTE_NAME, moduleName);
				qN.getFilter().addEquals(AppConstants.DOCUMENT_NAME_ATTRIBUTE_NAME, documentName);
				qN.getFilter().addEquals(AppConstants.SEQUENCE_NAME_ATTRIBUTE_NAME, fieldName);
				return qN.beanResults();
			});

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
						if (o != null) {
							lastNumber = o.toString();
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

			try {
				pers.preMerge(document, dN);
			}
			catch (UniqueConstraintViolationException e) {
				// A new row fails the check when another transaction created this sequence's row after
				// the query above found none. The check waited for that transaction to commit, so the
				// row can now be read and locked: take the number from it.
				if (retryCreation) {
					return getNextNumber(pers, prefix, moduleName, documentName, fieldName, numberLength, false);
				}
				throw e;
			}
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
	protected static @Nonnull String incrementAlpha(@Nullable String suppliedPrefix, @Nullable String lastNumber, int numberLength) {
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
			} else if (prefix.matches(NUMERIC_PATTERN) && lastNumber.matches(NUMERIC_PATTERN) && !"0".equals(lastNumber)) {
				int len = prefix.length();
				value = Integer.valueOf(Integer.parseInt(lastNumber.substring(len)) + 1);
				nonNumeric = prefix;

				// cater for numeric only
			} else if (lastNumber.matches(NUMERIC_PATTERN)) {
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
