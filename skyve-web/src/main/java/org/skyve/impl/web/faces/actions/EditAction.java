package org.skyve.impl.web.faces.actions;

import java.util.Map;
import java.util.SortedMap;
import java.util.Stack;
import java.util.logging.Level;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang3.StringUtils;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.domain.messages.AccessException;
import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.WebUtil;
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
import org.skyve.metadata.user.UserAccess;
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

		T bean = null;
		AbstractWebContext webContext = null;
		User user = CORE.getUser();
		Customer customer = user.getCustomer();
		
		try {
			ExternalContext ec = FacesContext.getCurrentInstance().getExternalContext();
			Map<String, Object> session = ec.getSessionMap();
			// This is executed from a redirect from a data grid add or zoom in, or from a subsequent zoom out or remove.
			// See ActionUtil.redirectViewScopedConversation().
			if (session.containsKey(FacesUtil.MANAGED_BEAN_NAME_KEY)) {
				FacesView<? extends Bean> sessionView = (FacesView<? extends Bean>) session.remove(FacesUtil.MANAGED_BEAN_NAME_KEY);
				String viewBinding = sessionView.getViewBinding();
				facesView.setViewBinding(viewBinding);
				facesView.getZoomInBindings().addAll(sessionView.getZoomInBindings());
				webContext = sessionView.getWebContext();
				bean = (T) webContext.getCurrentBean();

				Bean current = (viewBinding == null) ? bean : (T) BindUtil.get(bean, facesView.getViewBinding());
				if (current == null) { // should never happen
					throw new IllegalStateException("current is null");
				}
				
				final String bizModule = current.getBizModule();
				final String bizDocument = current.getBizDocument();

				if (! user.canAccess(UserAccess.singular(bizModule, bizDocument), facesView.getUxUi().getName())) {
					final String userName = user.getName();
					UtilImpl.LOGGER.warning("User " + userName + " cannot access document view " + bizModule + '.' + bizDocument);
					UtilImpl.LOGGER.info("If this user already has a document privilege, check if they were navigated to this page/resource programatically or by means other than the menu or views and need to be granted access via an <accesses> stanza in the module or view XML.");
					throw new AccessException("this page", userName);
				}
				
				facesView.setBizModuleParameter(bizModule);
				facesView.setBizDocumentParameter(bizDocument);
			}
			else {
				final String bizModule = facesView.getBizModuleParameter();
				if (bizModule == null) {
					throw new IllegalStateException("bizModule is required");
				}
				final String bizDocument = facesView.getBizDocumentParameter();
				if (bizDocument == null) {
					throw new IllegalStateException("bizDocument is required");
				}

				if (! user.canAccess(UserAccess.singular(bizModule, bizDocument), facesView.getUxUi().getName())) {
					final String userName = user.getName();
					UtilImpl.LOGGER.warning("User " + userName + " cannot access document view " + bizModule + '.' + bizDocument);
					UtilImpl.LOGGER.info("If this user already has a document privilege, check if they were navigated to this page/resource programatically or by means other than the menu or views and need to be granted access via an <accesses> stanza in the module or view XML.");
					throw new AccessException("this page", userName);
				}

				Module module = customer.getModule(bizModule);
				Document document = module.getDocument(customer, bizDocument);
				if (! user.canAccessDocument(document)) {
					throw new SecurityException(bizDocument + " in module " + bizModule, user.getName());
				}
				
				// No bizId or not a persistent document
				String bizId = facesView.getBizIdParameter();
				if (bizId == null) {
					// No security check is required as we are at the top of the conversation
					// If the user doesn't have create privilege, it will be stopped in SaveAction.
					bean = document.newInstance(user);

					webContext = new FacesWebContext();
					webContext.setConversation(AbstractPersistence.get());
					webContext.setCurrentBean(bean);
	
					String bindingParameter = facesView.getBindingParameter();
					if (bindingParameter != null) {
						setupViewForZoomIn(bean, bindingParameter);
					}
					else {
						SortedMap<String, Object> parameters = SmartClientEditServlet.collectRequestParameters((HttpServletRequest) ec.getRequest());
						SmartClientEditServlet.applyNewParameters(customer,
																	user,
																	AbstractPersistence.get(), 
																	module, 
																	document, 
																	bean, 
																	parameters,
																	facesView.getUxUi().getName());
						
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
							
							// We want to call post render
							facesView.setPostRender(bizlet, bean);
						}
					}
				}
				else {
					AbstractPersistence persistence = AbstractPersistence.get();
					try {
						// NB can throw NoResultsException or SecurityException
						bean = (T) WebUtil.findReferencedBean(document, bizId, persistence, null, webContext);
					}
					finally {
						webContext = new FacesWebContext();
						webContext.setConversation(persistence);
						webContext.setCurrentBean(bean);
					}
					
					// NB bean can be null if it wasn't found in the retrieve above
					String bindingParameter = facesView.getBindingParameter();
					if (bindingParameter != null) {
						setupViewForZoomIn(bean, bindingParameter);
					}
					// We can't check for update privilege here as we don't know if the zoom in is read-only or not.
					// Its up to the app coder to disable the UI if appropriate.
					else {
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
							
							// We want to call post render
							facesView.setPostRender(bizlet, bean);
			    		}
					}
				}
			}
		}
		// ensure that we always do this stuff even if an exception occurs 
		// so that faces has a chance to render the current page and error off the FacesView.
		finally {
			facesView.setWebContext(webContext);
			facesView.setBean(bean);
		}
		
		return null;
	}
	
	/**
	 * If we can get the binding then set the view up to be zoomed into the binding
	 * @param bean
	 * @param bindingParameter
	 */
	private void setupViewForZoomIn(T bean, String bindingParameter) {
		// Only set up the view to be zoomed in if we can grab the binding
		Bean current = null;
		String viewBinding = bindingParameter.replace(',', '.');
		try {
			current = (Bean) BindUtil.get(bean, viewBinding);
		}
		catch (Exception e) {
			// Failed to get the current bean - current is null
			UtilImpl.LOGGER.info("EditAction:- Could not get binding " + viewBinding + " in bean " + bean + " : " + e.getMessage());
		}
		if (current != null) { // successful binding get
			Stack<String> zoomInBindings = facesView.getZoomInBindings();
			String[] bindings = StringUtils.split(bindingParameter, ',');
			for (String binding : bindings) {
				zoomInBindings.add(binding);
			}
			facesView.setViewBinding(viewBinding);
			facesView.setBizModuleParameter(current.getBizModule());
			facesView.setBizDocumentParameter(current.getBizDocument());
		}
	}
}
