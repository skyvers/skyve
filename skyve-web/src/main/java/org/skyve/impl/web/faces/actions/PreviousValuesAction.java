package org.skyve.impl.web.faces.actions;

import java.util.Collections;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.Binder.TargetMetaData;

public class PreviousValuesAction<T extends Bean> extends FacesAction<List<String>> {
	private FacesView<T> facesView;
	private String query;
	private String binding;
	
	public PreviousValuesAction(FacesView<T> facesView,
								String query,
								String binding) {
		this.facesView = facesView;
		this.query = query;
		this.binding = binding;
	}

	@Override
	public List<String> callback() throws Exception {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("PreviousValuesAction - EXECUTE find " + query + " for binding " + binding);

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
			targetDocument = target.getDocument();

	    	if (! user.canReadDocument(targetDocument)) {
				throw new SecurityException("read this data", user.getName());
			}

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
			
			StringBuilder bizQL = new StringBuilder(128);
	        bizQL.append("select distinct bean.").append(attributeName);
	        bizQL.append(" as value from {").append(targetDocument.getOwningModuleName()).append('.');
	        bizQL.append(targetDocument.getName()).append("} as bean");
			if (query != null) {
				bizQL.append(" where bean.").append(attributeName).append(" like '%").append(query).append("%'");
			}
			
			result = persistence.newBizQL(bizQL.toString()).setMaxResults(100).scalarResults(String.class);
		}

	    return result;
	}
}
