package org.skyve.wildcat.web.faces.actions;

import java.util.Map;
import java.util.logging.Level;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.wildcat.domain.messages.SecurityException;
import org.skyve.wildcat.metadata.model.document.DocumentImpl;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.web.AbstractWebContext;
import org.skyve.wildcat.web.faces.FacesAction;
import org.skyve.wildcat.web.faces.FacesUtil;
import org.skyve.wildcat.web.faces.FacesWebContext;
import org.skyve.wildcat.web.faces.beans.FacesView;

public class EditAction<T extends Bean> extends FacesAction<Void> {
	private FacesView<T> facesView = null;
	public EditAction(FacesView<T> facesView) {
		this.facesView = facesView;
	}

	@Override
	@SuppressWarnings("unchecked")
	public Void callback() throws Exception {
		String bizModule = facesView.getBizModuleParameter();
		if (bizModule == null) {
			throw new IllegalStateException("bizModule is required");
		}
		String bizDocument = facesView.getBizDocumentParameter();
		if (bizDocument == null) {
			throw new IllegalStateException("bizDocument is required");
		}
		ExternalContext ec = FacesContext.getCurrentInstance().getExternalContext();
		User user = CORE.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(bizModule);
		Document document = module.getDocument(customer, bizDocument);
		String bizId = facesView.getBizIdParameter();
		T bean = null;
		AbstractWebContext webContext = null;
		if (bizId == null) {
			Map<String, Object> session = ec.getSessionMap();
			if (session.containsKey(FacesUtil.MANAGED_BEAN_NAME_KEY)) {
				FacesView<? extends Bean> sessionView = (FacesView<? extends Bean>) session.remove(FacesUtil.MANAGED_BEAN_NAME_KEY);
				facesView.setViewBinding(sessionView.getViewBinding());
				facesView.getHistory().addAll(sessionView.getHistory());
				webContext = sessionView.getWebContext();
				bean = (T) webContext.getCurrentBean();
			}
			else {
				bean = document.newInstance(user);
				
				// this is for the cancel, ok and delete buttons
				String referer = ec.getRequestHeaderMap().get("referer");
				facesView.getHistory().push(referer);
//System.out.println("PUSH REFERER OF " + referer + " yields " + facesView.getHistory().size());
				webContext = new FacesWebContext();
				webContext.setConversation(AbstractPersistence.get());
				webContext.setCurrentBean(bean);

				Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);
				if (bizlet != null) {
					UtilImpl.LOGGER.info("PRE-EXECUTE on " + ImplicitActionName.New);
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Entering " + bizlet.getClass().getName() + ".preExecute: " + ImplicitActionName.New + ", " + bean + ", null, " + ", " + webContext);
	    			bean = (T) bizlet.preExecute(ImplicitActionName.New, bean, null, webContext);
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Exiting " + bizlet.getClass().getName() + ".preExecute: " + bean);
				}
			}
		}
		else {
			AbstractPersistence persistence = AbstractPersistence.get();
			bean = persistence.retrieve(document, bizId, false);

    		if (! user.canReadBean(bean.getBizId(), 
									bean.getBizModule(), 
									bean.getBizDocument(), 
									bean.getBizCustomer(), 
									bean.getBizDataGroupId(), 
									bean.getBizUserId())) {
    			throw new SecurityException("this data", user.getName());
    		}
    		
			// this is for the cancel, ok and delete buttons
			String referer = ec.getRequestHeaderMap().get("referer");
			facesView.getHistory().push(referer);
//System.out.println("PUSH REFERER OF " + referer + " yields " + facesView.getHistory().size());
			webContext = new FacesWebContext();
			webContext.setConversation(persistence);
			webContext.setCurrentBean(bean);

			Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);
			if (bizlet != null) {
				UtilImpl.LOGGER.info("PRE-EXECUTE on " + ImplicitActionName.Edit);
				if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Entering " + bizlet.getClass().getName() + ".preExecute: " + ImplicitActionName.Edit + ", " + bean + ", null, " + ", " + webContext);
    			bean = (T) bizlet.preExecute(ImplicitActionName.Edit, bean, null, webContext);
				if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Exiting " + bizlet.getClass().getName() + ".preExecute: " + bean);
    		}
		}
		facesView.setWebContext(webContext);
		facesView.setBean(bean);
		
		return null;
	}
}
