package org.skyve.wildcat.web.faces.actions;

import java.util.logging.Level;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;
import org.skyve.wildcat.domain.messages.SecurityException;
import org.skyve.wildcat.metadata.model.document.DocumentImpl;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.ValidationUtil;
import org.skyve.wildcat.web.faces.FacesAction;
import org.skyve.wildcat.web.faces.beans.FacesView;

/**
 * Strip the last term off the view binding
 */
public class ZoomOutAction extends FacesAction<Void> {
	private FacesView<? extends Bean> facesView;
	public ZoomOutAction(FacesView<? extends Bean> facesView) {
		this.facesView = facesView;
	}
	
	@Override
	public Void callback() throws Exception {
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info("ZoomOutAction");

		// Call the bizlet
		Bean currentBean = ActionUtil.getTargetBeanForViewAndCollectionBinding(facesView, null, null);
		User user = CORE.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(currentBean.getBizModule());
		Document document = module.getDocument(customer, currentBean.getBizDocument());
		Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);
		if (bizlet != null) {
			if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Entering " + bizlet.getClass().getName() + ".preExecute: " + ImplicitActionName.ZoomOut + ", " + currentBean + ", null, " +  facesView.getWebContext());
			currentBean = bizlet.preExecute(ImplicitActionName.ZoomOut, currentBean, null, facesView.getWebContext());
			if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Exiting " + bizlet.getClass().getName() + ".preExecute: " + currentBean);
		}

		if (currentBean.isNotPersisted() && (! user.canCreateDocument(document))) {
			throw new SecurityException("create this data", user.getName());
		}

		ValidationUtil.validateBeanAgainstDocument(document, currentBean);
		if (bizlet != null) {
			ValidationUtil.validateBeanAgainstBizlet(bizlet, currentBean);
		}

		zoomOut(facesView);
		
		return null;
	}
	
	static void zoomOut(FacesView<? extends Bean> facesView) throws Exception {
		String viewBinding = facesView.getViewBinding();
    	String newViewBinding = null;

    	// remove the last binding term if there is a dot,
		// otherwise remove the view binding altogether as we are at the outer-most level.
    	if (viewBinding != null) {
	    	int lastDotIndex = viewBinding.lastIndexOf('.');
			if (lastDotIndex > 0) {
				newViewBinding = viewBinding.substring(0, lastDotIndex);
			}
    	}
    	facesView.setViewBinding(newViewBinding);

    	Bean parentBean = ActionUtil.getTargetBeanForViewAndCollectionBinding(facesView, null, null);
		ActionUtil.redirect(facesView, parentBean);
	}
}
