package org.skyve.impl.web.faces.actions;

import java.util.Map;
import java.util.SortedMap;
import java.util.logging.Level;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;

import org.primefaces.PrimeFaces;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.FacesWebContext;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.impl.web.service.smartclient.SmartClientEditServlet;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;

public class EditAction<T extends Bean> extends FacesAction<Void> {
	private FacesView<T> facesView = null;
	public EditAction(FacesView<T> facesView) {
		this.facesView = facesView;
	}

	@Override
	@SuppressWarnings("unchecked")
	public Void callback() throws Exception {
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info("EditAction");

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
		if (! user.canAccessDocument(document)) {
			throw new SecurityException(bizDocument + " in module " + bizModule, user.getName());
		}
		
		String bizId = facesView.getBizIdParameter();
		T bean = null;
		AbstractWebContext webContext = null;
		
		try {
			if (bizId == null) {
				Map<String, Object> session = ec.getSessionMap();
				// This is executed from a redirect from a data grid add or zoom in, or from a subsequent zoom out or remove.
				// See ActionUtil.redirectViewScopedConversation().
				if (session.containsKey(FacesUtil.MANAGED_BEAN_NAME_KEY)) {
					FacesView<? extends Bean> sessionView = (FacesView<? extends Bean>) session.remove(FacesUtil.MANAGED_BEAN_NAME_KEY);
					facesView.setViewBinding(sessionView.getViewBinding());
					facesView.getZoomInBindings().addAll(sessionView.getZoomInBindings());
					webContext = sessionView.getWebContext();
					bean = (T) webContext.getCurrentBean();
					if (bean.isPersisted()) {
						PrimeFaces.current().executeScript("SKYVE.PF.restoreHistory()");
					}
				}
				else {
					// No security check is required as we are at the top of the conversation
					// If the user doesn't have create privilege, it will be stopped in SaveAction.
					bean = document.newInstance(user);
					
					SortedMap<String, Object> parameters = SmartClientEditServlet.collectRequestParameters((HttpServletRequest) ec.getRequest());
					SmartClientEditServlet.applyNewParameters(customer,
																user,
																AbstractPersistence.get(), 
																module, 
																document, 
																bean, 
																parameters,
																facesView.getUxUi().getName());
					
					webContext = new FacesWebContext();
					webContext.setConversation(AbstractPersistence.get());
					webContext.setCurrentBean(bean);
	
					CustomerImpl internalCustomer = (CustomerImpl) customer;
					boolean vetoed = internalCustomer.interceptBeforePreExecute(ImplicitActionName.New, bean, null, webContext);
					if (! vetoed) {
						Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);
						if (bizlet != null) {
							if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Entering " + bizlet.getClass().getName() + ".preExecute: " + ImplicitActionName.New + ", " + bean + ", null, " + ", " + webContext);
			    			bean = (T) bizlet.preExecute(ImplicitActionName.New, bean, null, webContext);
							if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Exiting " + bizlet.getClass().getName() + ".preExecute: " + bean);
						}
						internalCustomer.interceptAfterPreExecute(ImplicitActionName.New, bean, null, webContext);
					}
				}
			}
			else {
				AbstractPersistence persistence = AbstractPersistence.get();
				bean = persistence.retrieve(document, bizId);
				// NB bean can be null if it wasn't found in the retrieve above
				if (bean != null) {
		    		if (! user.canReadBean(bean.getBizId(), 
											bean.getBizModule(), 
											bean.getBizDocument(), 
											bean.getBizCustomer(), 
											bean.getBizDataGroupId(), 
											bean.getBizUserId())) {
		    			throw new SecurityException("this data", user.getName());
		    		}
				}
				
				// We can't check for update privilege here as we don't know if the zoom in is read-only or not.
				// Its up to the app coder to disable the UI if appropriate.
	
				webContext = new FacesWebContext();
				webContext.setConversation(persistence);
				webContext.setCurrentBean(bean);
	
				CustomerImpl internalCustomer = (CustomerImpl) customer;
				boolean vetoed = internalCustomer.interceptBeforePreExecute(ImplicitActionName.Edit, bean, null, webContext);
				if (! vetoed) {
					Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);
					if (bizlet != null) {
						if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Entering " + bizlet.getClass().getName() + ".preExecute: " + ImplicitActionName.Edit + ", " + bean + ", null, " + ", " + webContext);
		    			bean = (T) bizlet.preExecute(ImplicitActionName.Edit, bean, null, webContext);
						if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preExecute", "Exiting " + bizlet.getClass().getName() + ".preExecute: " + bean);
					}
					internalCustomer.interceptAfterPreExecute(ImplicitActionName.Edit, bean, null, webContext);
	    		}
			}
		}
		// ensure that we always do this stuff even if an exception occurs 
		// so that faces has a chance to render the current page and error off the FaceView.
		finally {
			facesView.setWebContext(webContext);
			facesView.setBean(bean);
		}
		
		return null;
	}
}
