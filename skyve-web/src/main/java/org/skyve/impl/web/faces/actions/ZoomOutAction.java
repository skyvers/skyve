package org.skyve.impl.web.faces.actions;

import java.util.logging.Level;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.ValidationUtil;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.Binder;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

/**
 * Strip the last term off the view binding
 */
public class ZoomOutAction<T extends Bean> extends FacesAction<Void> {
	private FacesView<T> facesView;
	public ZoomOutAction(FacesView<T> facesView) {
		this.facesView = facesView;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public Void callback() throws Exception {
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info("ZoomOutAction by zoom in binding of " + facesView.getZoomInBinding() + " with view binding of " + facesView.getViewBinding());

		if (FacesAction.validateRequiredFields()) {
			// Call the bizlet
			Bean elementBean = ActionUtil.getTargetBeanForViewAndCollectionBinding(facesView, null, null);
			User user = CORE.getUser();
			Customer customer = user.getCustomer();
			Module elementModule = customer.getModule(elementBean.getBizModule());
			Document elementDocument = elementModule.getDocument(customer, elementBean.getBizDocument());
			Bizlet<Bean> bizlet = ((DocumentImpl) elementDocument).getBizlet(customer);

			WebContext webContext = facesView.getWebContext();
			CustomerImpl internalCustomer = (CustomerImpl) customer;
			boolean vetoed = internalCustomer.interceptBeforePreExecute(ImplicitActionName.ZoomOut, elementBean, null, webContext);
			if (! vetoed) {
				if (bizlet != null) {
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Entering " + bizlet.getClass().getName() + ".preExecute: " + ImplicitActionName.ZoomOut + ", " + elementBean + ", null, " + webContext);
					elementBean = bizlet.preExecute(ImplicitActionName.ZoomOut, elementBean, null, webContext);
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Exiting " + bizlet.getClass().getName() + ".preExecute: " + elementBean);
				}
				internalCustomer.interceptAfterPreExecute(ImplicitActionName.ZoomOut, elementBean, null, webContext);
			}
	
			if (elementBean.isNotPersisted() && (! user.canCreateDocument(elementDocument))) {
				throw new SecurityException("create this data", user.getName());
			}
	
			ValidationUtil.validateBeanAgainstDocument(elementDocument, elementBean);
			if (bizlet != null) {
				ValidationUtil.validateBeanAgainstBizlet(bizlet, elementBean);
			}
			// Set the current bean back in the collection
			ActionUtil.setTargetBeanForViewAndCollectionBinding(facesView, null, (T) elementBean);

			// Sort the owning collection
			String viewBinding = facesView.getViewBinding();
			int lastCollectionindex = viewBinding.lastIndexOf("ElementById(");
			String collectionBinding = viewBinding.substring(0, lastCollectionindex);
			Bean currentBean = facesView.getWebContext().getCurrentBean();
			Module currentModule = customer.getModule(currentBean.getBizModule());
			Binder.sortCollectionByMetaData(currentBean,
												customer,
												currentModule,
												currentModule.getDocument(customer, currentBean.getBizDocument()),
												collectionBinding);

			// now zoom out to owning view
			zoomOut(facesView);
		}
		
		return null;
	}
	
	static void zoomOut(FacesView<? extends Bean> facesView) throws Exception {
		String viewBinding = facesView.getViewBinding();
		String zoomInBinding = facesView.getZoomInBinding();
		String newViewBinding = null;

		// remove the zoom out binding expression from the view binding
		if (viewBinding != null) {
        	// if no zoom in binding, remove the last binding term if there is a dot
    		if (zoomInBinding == null) {
	    		int lastDotIndex = viewBinding.lastIndexOf('.');
				if (lastDotIndex > 0) {
					newViewBinding = viewBinding.substring(0, lastDotIndex);
    				if (newViewBinding.isEmpty()) {
    					newViewBinding = null;
    				}
				}
    			// otherwise remove the view binding altogether as we are at the outer-most level.
    		}
    		else { // there is a zoom in binding, remove the binding
    			if (viewBinding.endsWith(zoomInBinding)) {
    				newViewBinding = viewBinding.substring(0, viewBinding.length() - zoomInBinding.length());
    				if (newViewBinding.isEmpty()) {
    					newViewBinding = null;
    				}
    			}
    			// otherwise remove the view binding altogether as we are at the outer-most level.
    		}
    	}
    	facesView.setViewBinding(newViewBinding);

    	Bean parentBean = ActionUtil.getTargetBeanForViewAndCollectionBinding(facesView, null, null);
		ActionUtil.redirect(facesView, parentBean);
	}
}
