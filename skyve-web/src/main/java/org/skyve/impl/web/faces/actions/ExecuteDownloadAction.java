package org.skyve.impl.web.faces.actions;

import org.primefaces.PrimeFaces;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.SecurityException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.WebUtil;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.OWASP;
import org.skyve.util.logging.Category;
import org.skyve.web.WebContext;
import org.slf4j.Logger;

import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.HttpServletRequest;

/**
 * /download?_n=<action>&_doc=<module.document>&_c=<webId>&_ctim=<millis> and optionally &_b=<view binding>
 */
public class ExecuteDownloadAction extends FacesAction<Void> {

    private static final Logger FACES_LOGGER = Category.FACES.logger();

	private FacesView facesView;
	private String actionName;
	private String dataWidgetBinding;
	private String elementBizId;
	
	/**
	 * Creates an action that prepares and triggers a document download.
	 *
	 * @param facesView the current Faces view context
	 * @param actionName the metadata action name
	 * @param dataWidgetBinding the optional binding for inline list actions
	 * @param elementBizId the optional selected element business ID
	 */
	public ExecuteDownloadAction(FacesView facesView,
									String actionName,
									String dataWidgetBinding,
									String elementBizId) {
		this.facesView = facesView;
		this.actionName = actionName;
		this.dataWidgetBinding = dataWidgetBinding;
		this.elementBizId = elementBizId;
	}

	@Override
	public Void callback() throws Exception {
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.info("ExecuteDownloadAction - EXECUTE ACTION {} {}", actionName, ((dataWidgetBinding != null) ? ("for data widget " + dataWidgetBinding + " with selected row " + elementBizId) : ""));

		AbstractPersistence persistence = AbstractPersistence.get();
		Bean targetBean = ActionUtil.getTargetBeanForViewAndReferenceBinding(facesView, dataWidgetBinding, elementBizId);
    	User user = persistence.getUser();
    	Customer customer = user.getCustomer();
    	Module targetModule = customer.getModule(targetBean.getBizModule());
		Document targetDocument = targetModule.getDocument(customer, targetBean.getBizDocument());
		HttpServletRequest request = (HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest();
		View view = targetDocument.getView(UserAgent.getSelection(request).getUxUi().getName(),
											customer, 
											targetBean.isCreated() ? ViewType.edit.toString() : ViewType.create.toString());
    	Action action = view.getAction(actionName);
    	Boolean clientValidation = action.getClientValidation();
		boolean clientValidationEnabled = ! Boolean.FALSE.equals(clientValidation);
		if (UtilImpl.FACES_TRACE) {
			FACES_LOGGER.info("ExecuteActionAction - client validation = {}", Boolean.valueOf(clientValidationEnabled));
		}
	    String resourceName = action.getResourceName();
    	
		if (! user.canExecuteAction(targetDocument, resourceName)) {
			throw new SecurityException(resourceName, user.getName());
		}

		if (! clientValidationEnabled || FacesAction.validateRequiredFields()) {
			WebContext webContext = facesView.getWebContext();

			DownloadAction<Bean> downloadAction = targetDocument.getDownloadAction(customer, resourceName, true);
			downloadAction.prepare(targetBean, webContext);
			
			String url = WebUtil.getDownloadActionUrl(actionName,
														targetModule.getName(),
														targetDocument.getName(),
														webContext.getWebId(),
														facesView.getViewBinding(),
														dataWidgetBinding,
														elementBizId);
			PrimeFaces.current().executeScript("window.location.assign('" + OWASP.escapeJsString(url) + "')");
			
			// We want to call post render
			facesView.setPostRender(targetDocument.getBizlet(customer), targetBean);
		}

	    return null;
	}
}
