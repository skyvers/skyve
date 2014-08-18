package org.skyve.wildcat.web.faces.actions;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.Query;
import org.skyve.metadata.user.User;
import org.skyve.util.Binder;
import org.skyve.wildcat.web.AbstractWebContext;
import org.skyve.wildcat.web.WebUtil;
import org.skyve.wildcat.web.faces.FacesUtil;
import org.skyve.wildcat.web.faces.beans.FacesView;

public class ActionUtil {
	/**
	 * Disallow instantiation
	 */
	private ActionUtil() {
		// nothing to see here
	}
	
    public static Bean getTargetBeanForViewAndCollectionBinding(FacesView<? extends Bean> facesView,
    																String collectionName,
    																String elementBizId)
    throws Exception {
    	Bean result = facesView.getBean();
    	
    	if (result != null) { // hopefully never
	    	String viewBinding = facesView.getViewBinding();
	    	if (viewBinding != null) {
	    		result = (Bean) Binder.get(result, viewBinding);
	    	}
			if (collectionName != null) {
				result = Binder.getElementInCollection(result, collectionName, elementBizId);
			}
    	}
    	
    	return result;
    }
    
    static <T extends Bean> void setTargetBeanForViewAndCollectionBinding(FacesView<T> facesView, String collectionName, T newValue)
	throws Exception {
    	Bean bean = facesView.getBean();
    	if (bean != null) { // hopefully never
	    	String viewBinding = facesView.getViewBinding();
	    	
	    	if (collectionName != null) {
	    		StringBuilder collectionBinding = new StringBuilder(64);
	    		if (viewBinding != null) {
	    			collectionBinding.append(viewBinding).append('.');
	    		}
	    		collectionBinding.append(collectionName).append('(').append(newValue.getBizId()).append(')');
	    		Binder.set(bean, collectionBinding.toString(), newValue);
	    	}
	    	else {
		    	if (viewBinding == null) {
		    		facesView.setBean(newValue);
		    	}
		    	else {
		    		Binder.set(bean, viewBinding, newValue);
		    	}
	    	}
    	}
    }
    
    static final void redirect(FacesView<? extends Bean> facesView, Bean currentBean)
    throws Exception {
		// ensure that the proper conversation is stashed in the webContext object
		AbstractWebContext webContext = facesView.getWebContext();
		WebUtil.putConversationInCache(webContext);
		
		// Put the view in the session
		facesView.dehydrate();
		ExternalContext ec = FacesContext.getCurrentInstance().getExternalContext();
		ec.getSessionMap().put(FacesUtil.MANAGED_BEAN_NAME_KEY, facesView);

		// perform the redirect
		StringBuilder outcome = new StringBuilder(64);
		outcome.append(org.skyve.util.Util.getWildcatContextUrl()).append("/?a=e&m=");
		outcome.append(currentBean.getBizModule()).append("&d=").append(currentBean.getBizDocument());
		ec.redirect(outcome.toString());
    }
    
    static Query getQuery(final String bizModule, final String queryName)
	throws MetaDataException {
		User user = CORE.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(bizModule);
		Query query = module.getQuery(queryName);
		if (query == null) {
			query = module.getDocumentDefaultQuery(customer, queryName);
		}

		return query;
    }
}
