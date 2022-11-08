package org.skyve.impl.web.faces.actions;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.domain.messages.AccessException;
import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.view.widget.bound.input.CompleteType;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.Util;

public class CompleteAction<T extends Bean> extends FacesAction<List<String>> {
	private FacesView<T> facesView;
	private String query;
	private String binding;
	private CompleteType complete;
	
	public CompleteAction(FacesView<T> facesView,
							String query,
							String binding,
							CompleteType complete) {
		this.facesView = facesView;
		this.query = Util.processStringValue(query);
		this.binding = binding;
		this.complete = complete;
	}

	@Override
	public List<String> callback() throws Exception {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("CompleteAction - EXECUTE complete " + query + " for binding " + binding);
		AbstractPersistence persistence = AbstractPersistence.get();
		Bean bean = ActionUtil.getTargetBeanForView(facesView);
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Document document = null;
		Attribute attribute = null;
		String attributeName = binding;
		
    	// if binding is compound, get the parent bean and adjust the attributeName
    	// prefer the module and document name determination polymorphically from the bean, otherwise use the metadata
    	int lastDotIndex = binding.lastIndexOf('.');
		if (lastDotIndex >= 0) {
			Bean formBean = bean;
			bean = (Bean) BindUtil.get(bean, binding.substring(0, lastDotIndex));
			if (bean == null) { // no bean so use metadata
				Module module = customer.getModule(formBean.getBizModule());
				document = module.getDocument(customer, formBean.getBizDocument());
				TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
				document = target.getDocument();
				attribute = target.getAttribute(); // could be null for an implicit attribute
				attributeName = binding.substring(lastDotIndex + 1);
			}
			else {
				attributeName = binding.substring(lastDotIndex + 1);
				Module module = customer.getModule(bean.getBizModule());
				document = module.getDocument(customer, bean.getBizDocument());
				attribute = document.getAttribute(attributeName); // could be null for an implicit attribute
			}
		}
		else {
			Module module = customer.getModule(bean.getBizModule());
			document = module.getDocument(customer, bean.getBizDocument());
			attribute = document.getAttribute(attributeName); // could be null for an implicit attribute
		}

		if ((attribute == null) && (! BindUtil.isImplicit(attributeName))) {
			throw new DomainException("Mal-formed URL");
		}

		List<String> result = Collections.emptyList();

		if (complete == CompleteType.previous) {
			final String userName = user.getName();
			final String moduleName = document.getOwningModuleName();
			final String documentName = document.getName();
			if (! user.canAccess(UserAccess.previousComplete(moduleName, documentName, binding), facesView.getUxUi().getName())) {
				UtilImpl.LOGGER.warning("User " + userName + " cannot access document view previous complete " + moduleName + '.' + documentName + " for " + binding);
				UtilImpl.LOGGER.info("If this user already has a document privilege, check if they were navigated to this page/resource programatically or by means other than the menu or views and need to be granted access via an <accesses> stanza in the module or view XML.");
				throw new AccessException("the previous data", userName);
			}
			
	    	if (! user.canReadDocument(document)) {
				throw new SecurityException("read this data", userName);
			}

			if (document.isPersistable()) { // persistent document
				if ((attribute == null) || // implicit attribute or
						attribute.isPersistent()) { // explicit and persistent attribute
					DocumentQuery q = persistence.newDocumentQuery(moduleName, documentName);
					q.addBoundProjection(attributeName, attributeName);
					q.setDistinct(true);
					if (query != null) {
						q.getFilter().addLike(attributeName, new StringBuilder(query.length() + 2).append("%").append(query).append("%").toString());
					}
					// NB return Object as the type could be anything
					List<Object> results = q.setMaxResults(100).scalarResults(Object.class);
					result = new ArrayList<>(results.size());
					for (Object value : results) {
						if (value != null) {
							result.add(value.toString());
						}
					}
				}
			}
		}
		else {
        	CustomerImpl internalCustomer = (CustomerImpl) customer;
			boolean vetoed = internalCustomer.interceptBeforeComplete(attributeName, query, bean);
			if (! vetoed) {
				Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);
				if (bizlet != null) {
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "complete", "Entering " + bizlet.getClass().getName() + ".complete: " + attributeName + ", " + query + ", " + bean);
					result = bizlet.complete(attributeName, query, bean);
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "complete", "Exiting " + bizlet.getClass().getName() + ".complete: " + attributeName + ", " + query + ", " + bean);
				}
				internalCustomer.interceptAfterComplete(attributeName, query, bean, result);
			}
		}

	    return result;
	}
}
