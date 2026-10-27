package org.skyve.impl.web.faces.actions;

import org.primefaces.PrimeFaces;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.SecurityException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.ViewWebContext;
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

	/**
	 * Checks the user's privilege, then, when the required fields are valid or the action turns client validation off,
	 * prepares the download and sends the browser to its URL.
	 *
	 * <p>Side effects: runs the action's {@code prepare()}, which may change the target bean; records the download on
	 * the view's web context, so that {@code DownloadServlet} serves it even when preparing it disabled this action;
	 * queues the script that requests the download; and schedules the bizlet's post render for the target bean.
	 *
	 * @return {@code null}
	 * @throws SecurityException if the user may not execute the action
	 * @throws Exception if preparing the download fails
	 */
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
			AbstractWebContext webContext = facesView.getWebContext();

			DownloadAction<Bean> downloadAction = targetDocument.getDownloadAction(customer, resourceName, true);
			downloadAction.prepare(targetBean, webContext);
			String viewBinding = facesView.getViewBinding();
			((ViewWebContext) webContext).recordPreparedDownload(actionName,
														targetModule.getName(), targetDocument.getName(),
														viewBinding, dataWidgetBinding, elementBizId);
			
			String url = WebUtil.getDownloadActionUrl(actionName,
														targetModule.getName(),
														targetDocument.getName(),
														webContext.getWebId(),
														viewBinding,
														dataWidgetBinding,
														elementBizId);
			PrimeFaces.current().executeScript("window.location.assign('" + OWASP.escapeJsStringWithHtmlFormatting(url) + "')");
			
			// We want to call post render
			facesView.setPostRender(targetDocument.getBizlet(customer), targetBean);
		}

	    return null;
	}
}
