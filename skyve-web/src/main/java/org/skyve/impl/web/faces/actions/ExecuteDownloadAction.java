package org.skyve.impl.web.faces.actions;

import org.primefaces.PrimeFaces;
import org.skyve.domain.Bean;
import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.WebUtil;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.web.WebContext;

/**
 * /download?_n=<action>&_doc=<module.document>&_c=<webId>&_ctim=<millis> and optionally &_b=<view binding>
 */
public class ExecuteDownloadAction<T extends Bean> extends FacesAction<Void> {
	private FacesView<T> facesView;
	private String actionName;
	private String dataWidgetBinding;
	private String elementBizId;
	
	public ExecuteDownloadAction(FacesView<T> facesView,
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
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("ExecuteDownloadAction - EXECUTE ACTION " + actionName + ((dataWidgetBinding != null) ? (" for data widget " + dataWidgetBinding + " with selected row " + elementBizId) : ""));

		AbstractPersistence persistence = AbstractPersistence.get();
		Bean targetBean = ActionUtil.getTargetBeanForViewAndReferenceBinding(facesView, dataWidgetBinding, elementBizId);
    	User user = persistence.getUser();
    	Customer customer = user.getCustomer();
    	Module targetModule = customer.getModule(targetBean.getBizModule());
		Document targetDocument = targetModule.getDocument(customer, targetBean.getBizDocument());
		View view = targetDocument.getView(facesView.getUxUi().getName(), 
											customer, 
											targetBean.isCreated() ? ViewType.edit.toString() : ViewType.create.toString());
    	Action action = view.getAction(actionName);
    	Boolean clientValidation = action.getClientValidation();
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("ExecuteActionAction - client validation = " + (! Boolean.FALSE.equals(clientValidation)));
    	String resourceName = action.getResourceName();
    	
		if (! user.canExecuteAction(targetDocument, resourceName)) {
			throw new SecurityException(resourceName, user.getName());
		}

		if (Boolean.FALSE.equals(clientValidation) || FacesAction.validateRequiredFields()) {
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
			PrimeFaces.current().executeScript("window.location.assign(\"" + url + "\")");
		}

	    return null;
	}
}
