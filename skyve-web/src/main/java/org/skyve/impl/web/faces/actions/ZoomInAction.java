package org.skyve.impl.web.faces.actions;

import java.util.logging.Level;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

public class ZoomInAction extends FacesAction<Void> {
	private FacesView<? extends Bean> facesView;
	private String listBinding;
	private String bizId;
	public ZoomInAction(FacesView<? extends Bean> facesView, String listBinding, String bizId) {
		this.facesView = facesView;
		this.listBinding = listBinding;
		this.bizId = bizId;
	}
	
	@Override
	public Void callback() throws Exception {
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info("ZoomInAction - listBinding=" + listBinding + " : bizId=" + bizId);

		if (FacesAction.validateRequiredFields()) {
			String viewBinding = facesView.getViewBinding();
			Bean parentBean = facesView.getCurrentBean().getBean();
			
			StringBuilder sb = new StringBuilder(64);
			sb.append(listBinding).append("ElementById(").append(bizId).append(')');
			facesView.getZoomInBindings().push(sb.toString());
			if (UtilImpl.FACES_TRACE) Util.LOGGER.info("Push ZoomInBinding " + sb.toString());
			if (viewBinding != null) {
				sb.insert(0, '.').insert(0, viewBinding);
			}
			facesView.setViewBinding(sb.toString());
			if (UtilImpl.FACES_TRACE) Util.LOGGER.info("Set ViewBinding " + sb.toString());
	
			Bean currentBean = ActionUtil.getTargetBeanForViewAndCollectionBinding(facesView, null, null);
	
			// Call the bizlet
			Customer customer = CORE.getUser().getCustomer();
			Module collectionModule = customer.getModule(currentBean.getBizModule());
			Document collectionDocument = collectionModule.getDocument(customer, currentBean.getBizDocument());

			WebContext webContext = facesView.getWebContext();
			CustomerImpl internalCustomer = (CustomerImpl) customer;
			boolean vetoed = internalCustomer.interceptBeforePreExecute(ImplicitActionName.Edit, currentBean, parentBean, webContext);
			if (! vetoed) {
				Bizlet<Bean> bizlet = ((DocumentImpl) collectionDocument).getBizlet(customer);
				if (bizlet != null) {
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Entering " + bizlet.getClass().getName() + ".preExecute: " + ImplicitActionName.Edit + ", " + currentBean + ", " + facesView.getBean() + ", " + webContext);
					currentBean = bizlet.preExecute(ImplicitActionName.Edit, currentBean, parentBean, webContext);
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Exiting " + bizlet.getClass().getName() + ".preExecute: " + currentBean);
				}
				internalCustomer.interceptAfterPreExecute(ImplicitActionName.Edit, currentBean, parentBean, webContext);
			}
	
			ActionUtil.redirect(facesView, currentBean);
		}
		
		return null;
	}
}
