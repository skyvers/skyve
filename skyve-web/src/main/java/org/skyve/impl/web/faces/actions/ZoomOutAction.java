package org.skyve.impl.web.faces.actions;

import java.util.Stack;
import java.util.logging.Level;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.ValidationUtil;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.views.FacesView;
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
public class ZoomOutAction extends FacesAction<Void> {
	private FacesView facesView;
	public ZoomOutAction(FacesView facesView) {
		this.facesView = facesView;
	}
	
	@Override
	public Void callback() throws Exception {
		Stack<String> zoomInBindings = facesView.getZoomInBindings();
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info(String.format("ZoomOutAction by zoom in binding of %s with view binding of %s",
																	zoomInBindings.isEmpty() ? "null" : zoomInBindings.peek(),
																	facesView.getViewBinding()));
		// We cannot do security tests at this point because ZoomOut is the only way out of the UI.
		// Create privilege is checked at instantiation time - in AddAction and EditAction.

		if (FacesAction.validateRequiredFields()) {
			// Call the bizlet
			// Find the Collection/InverseMany element or association/inverseOne
			Bean referenceBean = ActionUtil.getTargetBeanForView(facesView);
			User user = CORE.getUser();
			Customer customer = user.getCustomer();
			Module referenceModule = customer.getModule(referenceBean.getBizModule());
			Document referenceDocument = referenceModule.getDocument(customer, referenceBean.getBizDocument());
			Bizlet<Bean> bizlet = ((DocumentImpl) referenceDocument).getBizlet(customer);

			WebContext webContext = facesView.getWebContext();
			CustomerImpl internalCustomer = (CustomerImpl) customer;
			boolean vetoed = internalCustomer.interceptBeforePreExecute(ImplicitActionName.ZoomOut, referenceBean, null, webContext);
			if (! vetoed) {
				if (bizlet != null) {
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Entering " + bizlet.getClass().getName() + ".preExecute: " + ImplicitActionName.ZoomOut + ", " + referenceBean + ", null, " + webContext);
					referenceBean = bizlet.preExecute(ImplicitActionName.ZoomOut, referenceBean, null, webContext);
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Exiting " + bizlet.getClass().getName() + ".preExecute: " + referenceBean);
				}
				internalCustomer.interceptAfterPreExecute(ImplicitActionName.ZoomOut, referenceBean, null, webContext);

				ValidationUtil.validateBeanAgainstDocument(referenceDocument, referenceBean);
				if (bizlet != null) {
					ValidationUtil.validateBeanAgainstBizlet(bizlet, referenceBean);
				}
				// Set the current bean back in the collection
				ActionUtil.setTargetBeanForViewAndCollectionBinding(facesView, null, referenceBean);

				// Sort the owning collection (if this is a collection element binding)
				String viewBinding = facesView.getViewBinding();
				// NB Is a collection element binding if the last "ElementById(" exists and is further towards
				// the end of the binding than the last '.' (if that exists)
				int lastDotIndex = viewBinding.lastIndexOf('.');
				int lastCollectionindex = viewBinding.lastIndexOf("ElementById(");
				if ((lastCollectionindex >= 0) && (lastDotIndex < lastCollectionindex)) {
					String collectionBinding = viewBinding.substring(0, lastCollectionindex);
					Bean currentBean = facesView.getWebContext().getCurrentBean();
					Module currentModule = customer.getModule(currentBean.getBizModule());
					Binder.sortCollectionByMetaData(currentBean,
														customer,
														currentModule,
														currentModule.getDocument(customer, currentBean.getBizDocument()),
														collectionBinding);
				}
				
				// now zoom out to owning view
				zoomOut(facesView, internalCustomer);
			}
		}
		
		return null;
	}
	
	static void zoomOut(FacesView facesView, CustomerImpl internalCustomer) throws Exception {
		String viewBinding = facesView.getViewBinding();
		Stack<String> zoomInBindings = facesView.getZoomInBindings();
		String zoomInBinding = zoomInBindings.isEmpty() ? null : zoomInBindings.pop();
		String newViewBinding = null;

		if (UtilImpl.FACES_TRACE) Util.LOGGER.info(String.format("zoomOut - popped zoomInBinding = %s, viewBinding = %s",
																	zoomInBinding,
																	viewBinding));

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
    				// if we have some view binding left, there must be a dot between, we need to remove the dot too,
    				// so take an extra char away
    				if (viewBinding.length() > zoomInBinding.length()) {
    					newViewBinding = viewBinding.substring(0, viewBinding.length() - zoomInBinding.length() - 1);
    				}
    				// else zoom in binding and view bindings are the same length, so leave the new view binding null
    			}
    			// otherwise leave the new view binding null as we are at the outer-most level.
    		}
    	}
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info("zoomOut - newViewBinding = " + newViewBinding);
		facesView.setViewBinding(newViewBinding);

		Bean currentBean = ActionUtil.getTargetBeanForView(facesView);
		Module m = internalCustomer.getModule(currentBean.getBizModule());
		Document d = m.getDocument(internalCustomer, currentBean.getBizDocument());
		Bizlet<? extends Bean> b = d.getBizlet(internalCustomer);
		facesView.setPostRender(b, currentBean);
		
 		ActionUtil.redirectViewScopedConversation(facesView, false);
	}
}
