package org.skyve.impl.web.faces.actions;

import java.util.Collections;
import java.util.List;
import java.util.logging.Level;

import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
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
import org.skyve.util.Binder.TargetMetaData;

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
		this.query = query;
		this.binding = binding;
		this.complete = complete;
	}

	@Override
	public List<String> callback() throws Exception {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("CompleteAction - EXECUTE complete " + query + " for binding " + binding);

		AbstractPersistence persistence = AbstractPersistence.get();
		Bean targetBean = ActionUtil.getTargetBeanForViewAndCollectionBinding(facesView, null, null);
    	User user = persistence.getUser();
    	Customer customer = user.getCustomer();
    	Module targetModule = customer.getModule(targetBean.getBizModule());
		Document targetDocument = targetModule.getDocument(customer, targetBean.getBizDocument());
		
		List<String> result = Collections.emptyList();

		TargetMetaData target = BindUtil.getMetaDataForBinding(customer, targetModule, targetDocument, binding);
		if (target != null) {
			String attributeName = binding;
			Attribute targetAttribute = target.getAttribute();
			if (targetAttribute != null) {
				attributeName = targetAttribute.getName();
			}
			else { // implicit attribute
				int lastDotIndex = binding.lastIndexOf('.');
				if (lastDotIndex >= 0) {
					attributeName = binding.substring(lastDotIndex + 1);
				}
			}

			targetDocument = target.getDocument();

			if (complete == CompleteType.previous) {
		    	if (! user.canReadDocument(targetDocument)) {
					throw new SecurityException("read this data", user.getName());
				}
	
				StringBuilder bizQL = new StringBuilder(128);
		        bizQL.append("select distinct bean.").append(attributeName);
		        bizQL.append(" as value from {").append(targetDocument.getOwningModuleName()).append('.');
		        bizQL.append(targetDocument.getName()).append("} as bean");
				if (query != null) {
					bizQL.append(" where bean.").append(attributeName).append(" like '%").append(query).append("%'");
				}
				
				result = persistence.newBizQL(bizQL.toString()).setMaxResults(100).scalarResults(String.class);
			}
			else {
	        	CustomerImpl internalCustomer = (CustomerImpl) customer;
				boolean vetoed = internalCustomer.interceptBeforeComplete(attributeName, query, targetBean);
				if (! vetoed) {
					Bizlet<Bean> bizlet = ((DocumentImpl) targetDocument).getBizlet(customer);
					if (bizlet != null) {
						if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "complete", "Entering " + bizlet.getClass().getName() + ".complete: " + attributeName + ", " + query + ", " + targetBean);
						result = bizlet.complete(attributeName, query, targetBean);
						if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "complete", "Exiting " + bizlet.getClass().getName() + ".complete: " + attributeName + ", " + query + ", " + targetBean);
					}
					internalCustomer.interceptAfterComplete(attributeName, query, targetBean, result);
				}

			}
		}

	    return result;
	}
}
