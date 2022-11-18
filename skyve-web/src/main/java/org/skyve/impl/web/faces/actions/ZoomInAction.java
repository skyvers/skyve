package org.skyve.impl.web.faces.actions;

import java.util.logging.Level;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
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
import org.skyve.metadata.user.User;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

public class ZoomInAction extends FacesAction<Void> {
	private FacesView<? extends Bean> facesView;
	private String binding;
	private String bizId;
	public ZoomInAction(FacesView<? extends Bean> facesView, String dataWidgetBinding, String bizId) {
		this.facesView = facesView;
		this.binding = dataWidgetBinding;
		this.bizId = bizId;
	}
	
	public ZoomInAction(FacesView<? extends Bean> facesView, String referenceBinding) {
		this.facesView = facesView;
		this.binding = referenceBinding;
	}
	
	@Override
	public Void callback() throws Exception {
		StringBuilder sb = new StringBuilder(64);
		sb.append("ZoomInAction - binding=").append(binding).append(" : bizId=").append(bizId);
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info(sb.toString());

		// We can't check for update privilege here as we don't know if the zoom in is read-only or not.
		// Its up to the app coder to disable the UI if appropriate.
		if (FacesAction.validateRequiredFields()) {
			String viewBinding = facesView.getViewBinding();
			Bean parentBean = facesView.getCurrentBean().getBean();
			
			sb.setLength(0);
			sb.append(binding);
			if (bizId != null) {
				sb.append("ElementById(").append(bizId).append(')');
			}
			facesView.getZoomInBindings().push(sb.toString());
			if (UtilImpl.FACES_TRACE) Util.LOGGER.info("Push ZoomInBinding " + sb.toString());
			if (viewBinding != null) {
				sb.insert(0, '.').insert(0, viewBinding);
			}
			facesView.setViewBinding(sb.toString());
			if (UtilImpl.FACES_TRACE) Util.LOGGER.info("Set ViewBinding " + sb.toString());
	
			Bean currentBean = ActionUtil.getTargetBeanForView(facesView);
			if (currentBean == null) { // instantiate one
				User u = facesView.getUser();
				Customer c = u.getCustomer();
				Module m = c.getModule(parentBean.getBizModule());
				Document d = m.getDocument(c, parentBean.getBizDocument());
				currentBean = (Bean) BindUtil.instantiateAndGet(u, m, d, parentBean, sb.toString());
			}

			// Call the bizlet
			Customer customer = CORE.getUser().getCustomer();
			Module referenceModule = customer.getModule(currentBean.getBizModule());
			Document referenceDocument = referenceModule.getDocument(customer, currentBean.getBizDocument());

			WebContext webContext = facesView.getWebContext();
			CustomerImpl internalCustomer = (CustomerImpl) customer;
			boolean vetoed = internalCustomer.interceptBeforePreExecute(ImplicitActionName.Edit, currentBean, parentBean, webContext);
			if (! vetoed) {
				Bizlet<Bean> bizlet = ((DocumentImpl) referenceDocument).getBizlet(customer);
				if (bizlet != null) {
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Entering " + bizlet.getClass().getName() + ".preExecute: " + ImplicitActionName.Edit + ", " + currentBean + ", " + facesView.getBean() + ", " + webContext);
					currentBean = bizlet.preExecute(ImplicitActionName.Edit, currentBean, parentBean, webContext);
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Exiting " + bizlet.getClass().getName() + ".preExecute: " + currentBean);
				}
				internalCustomer.interceptAfterPreExecute(ImplicitActionName.Edit, currentBean, parentBean, webContext);

				// We want to call post render on the zoomed in bean
				facesView.setPostRender(bizlet, currentBean);

				ActionUtil.redirectViewScopedConversation(facesView, true);
			}
		}
		
		return null;
	}
}
