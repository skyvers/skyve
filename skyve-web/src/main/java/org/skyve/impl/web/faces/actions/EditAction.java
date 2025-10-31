package org.skyve.impl.web.faces.actions;

import java.util.Deque;
import java.util.Map;
import java.util.SortedMap;

import org.apache.commons.lang3.StringUtils;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.SecurityException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.WebUtil;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.FacesWebContext;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.impl.web.service.smartclient.SmartClientEditServlet;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.HttpServletRequest;

public class EditAction extends FacesAction<Void> {

    private static final Logger LOGGER = LoggerFactory.getLogger(EditAction.class);
    private static final Logger FACES_LOGGER = Category.FACES.logger();
    private static final Logger BIZLET_LOGGER = Category.BIZLET.logger();

	private FacesView facesView = null;
	public EditAction(FacesView facesView) {
		this.facesView = facesView;
	}

	@Override
	public Void callback() throws Exception {
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.info("EditAction");

		Bean bean = null;
		AbstractWebContext webContext = null;
		User user = CORE.getUser();
		Customer customer = user.getCustomer();
		
		try {
			ExternalContext ec = FacesContext.getCurrentInstance().getExternalContext();
			Map<String, Object> session = ec.getSessionMap();
			// This is executed from a redirect from a data grid add or zoom in, or from a subsequent zoom out or remove.
			// See ActionUtil.redirectViewScopedConversation().
			if (session.containsKey(FacesUtil.MANAGED_BEAN_NAME_KEY)) {
				FacesView sessionView = (FacesView) session.remove(FacesUtil.MANAGED_BEAN_NAME_KEY);
				String viewBinding = sessionView.getViewBinding();
				facesView.setViewBinding(viewBinding);
				// Add session view zoom in bindings to view zoom in bindings in head first order first
				facesView.getZoomInBindings().addAll(sessionView.getZoomInBindings());
				webContext = sessionView.getWebContext();
				bean = webContext.getCurrentBean();

				Bean current = (viewBinding == null) ? bean : (Bean) BindUtil.get(bean, facesView.getViewBinding());
				if (current == null) { // should never happen
					throw new IllegalStateException("current is null");
				}
				
				final String bizModule = current.getBizModule();
				final String bizDocument = current.getBizDocument();

				EXT.checkAccess(user, UserAccess.singular(bizModule, bizDocument), facesView.getUxUi().getName());
				
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

				EXT.checkAccess(user, UserAccess.singular(bizModule, bizDocument), facesView.getUxUi().getName());

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
								if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Entering " + bizlet.getClass().getName() + ".preExecute: " + ImplicitActionName.New + ", " + bean + ", null, " + ", " + webContext);
				    			bean = bizlet.preExecute(ImplicitActionName.New, bean, null, webContext);
								if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Exiting " + bizlet.getClass().getName() + ".preExecute: " + bean);
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
						bean = WebUtil.findReferencedBean(document, bizId, persistence, null, webContext);
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
								if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Entering " + bizlet.getClass().getName() + ".preExecute: " + ImplicitActionName.Edit + ", " + bean + ", null, " + ", " + webContext);
				    			bean = bizlet.preExecute(ImplicitActionName.Edit, bean, null, webContext);
								if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Exiting " + bizlet.getClass().getName() + ".preExecute: " + bean);
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
	private void setupViewForZoomIn(Bean bean, String bindingParameter) {
		// Only set up the view to be zoomed in if we can grab the binding
		Bean current = null;
		String viewBinding = bindingParameter.replace(',', '.');
		try {
			current = (Bean) BindUtil.get(bean, viewBinding);
		}
		catch (Exception e) {
			// Failed to get the current bean - current is null
			LOGGER.info("EditAction:- Could not get binding {} in bean {} : {}", viewBinding, bean, e.getMessage(), e);
		}
		if (current != null) { // successful binding get
			Deque<String> zoomInBindings = facesView.getZoomInBindings();
			String[] bindings = StringUtils.split(bindingParameter, ',');
			for (String binding : bindings) {
				zoomInBindings.add(binding); // add to the tail
			}
			facesView.setViewBinding(viewBinding);
			facesView.setBizModuleParameter(current.getBizModule());
			facesView.setBizDocumentParameter(current.getBizDocument());
		}
	}
}
