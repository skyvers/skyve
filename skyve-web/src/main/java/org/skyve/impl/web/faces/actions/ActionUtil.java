package org.skyve.impl.web.faces.actions;

import java.util.Deque;

import org.apache.commons.lang3.StringUtils;
import org.primefaces.PrimeFaces;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.User;
import org.skyve.util.Binder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;

public class ActionUtil {

    private static final Logger LOGGER = LoggerFactory.getLogger(ActionUtil.class);

	/**
	 * Disallow instantiation
	 */
	private ActionUtil() {
		// nothing to see here
	}
	
	public static Bean getTargetBeanForView(FacesView facesView)
	throws Exception {
		return getTargetBeanForViewAndReferenceBinding(facesView, null);
	}

	public static Bean getTargetBeanForViewAndReferenceBinding(FacesView facesView,
																String referenceBinding)
	throws Exception {
		return getTargetBeanForViewAndReferenceBinding(facesView, referenceBinding, null);
	}

	public static Bean getTargetBeanForViewAndReferenceBinding(FacesView facesView,
    															String referenceBinding,
																String elementBizId)
    throws Exception {
    	Bean result = facesView.getBean();
    	
    	if (result != null) { // hopefully never
	    	String viewBinding = facesView.getViewBinding();
	    	if (viewBinding != null) {
	    		result = (Bean) Binder.get(result, viewBinding);
	    	}
			if (referenceBinding != null) {
				if (elementBizId != null) {
					result = Binder.getElementInCollection(result, referenceBinding, elementBizId);
				}
				else {
					result = (Bean) Binder.get(result, referenceBinding);
				}
			}
    	}
    	else {
    		LOGGER.warn("ActionUtil.getTargetBeanForViewAndReferenceBinding: FacesView.getBean() yields null");
    	}
    	
    	return result;
    }

    static <T extends Bean> void setTargetBeanForViewAndCollectionBinding(FacesView facesView, String collectionName, T newValue)
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
    	else {
    		LOGGER.warn("ActionUtil.setTargetBeanForViewAndCollectionBinding: FacesView.getBean() yields null");
    	}
    }
    
    static final void redirectViewScopedConversation(FacesView facesView, boolean zoomIn)
    throws Exception {
		// ensure that the proper conversation is stashed in the webContext object
		AbstractWebContext webContext = facesView.getWebContext();
		StateUtil.cacheConversation(webContext);

		// Get the view's bean
		Bean contextBean = facesView.getBean();
		
		// Put the view in the session (keep post render state if required)
		// NB The view will be dehydrated in the SkyveFacesPhaseListener (after Bizlet.postRender() is called - so it has a webContext)
		Bizlet<? extends Bean> postRenderBizlet = facesView.getPostRenderBizlet();
		Bean postRenderBean = facesView.getPostRenderBean();
		facesView.setPostRender(postRenderBizlet, postRenderBean);
		ExternalContext ec = FacesContext.getCurrentInstance().getExternalContext();
		ec.getSessionMap().put(FacesUtil.MANAGED_BEAN_NAME_KEY, facesView);

		// perform the redirect
		StringBuilder outcome = new StringBuilder(64);
		outcome.append(org.skyve.util.Util.getSkyveContextUrl()).append("/?a=e&m=");
		outcome.append(contextBean.getBizModule()).append("&d=").append(contextBean.getBizDocument());
		if (contextBean.isPersisted()) {
			outcome.append("&i=").append(contextBean.getBizId());
		}
		Deque<String> zoomInBindings = facesView.getZoomInBindings();
		if ((zoomInBindings != null) && (! zoomInBindings.isEmpty())) {
			outcome.append("&b=").append(StringUtils.join(zoomInBindings, ','));
		}

		if (zoomIn) {
			PrimeFaces.current().executeScript("SKYVE.PF.pushHistory()");
		}
		else {
			PrimeFaces.current().executeScript("SKYVE.PF.popHistory(false)");
		}
		ec.redirect(outcome.toString());
    }
    
    public static MetaDataQueryDefinition getMetaDataQuery(final String bizModule, final String queryName) {
		User user = CORE.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(bizModule);
		MetaDataQueryDefinition query = module.getMetaDataQuery(queryName);
		if (query == null) {
			query = module.getDocumentDefaultQuery(customer, queryName);
		}

		return query;
    }
}
