package org.skyve.impl.web.faces.actions;

import org.skyve.domain.Bean;
import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.web.WebContext;

public class ExecuteActionAction<T extends Bean> extends FacesAction<Void> {
	private FacesView<T> facesView;
	private String actionName;
	
	private String collectionName;
	private String elementBizId;
	
	public ExecuteActionAction(FacesView<T> facesView,
								String actionName,
								String collectionName,
								String elementBizId) {
		this.facesView = facesView;
		this.actionName = actionName;
		this.collectionName = collectionName;
		this.elementBizId = elementBizId;
	}

	@Override
	@SuppressWarnings("unchecked")
	public Void callback() throws Exception {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("ExecuteActionAction - EXECUTE ACTION " + actionName + ((collectionName != null) ? (" for grid " + collectionName + " with selected row " + elementBizId) : ""));

		AbstractPersistence persistence = AbstractPersistence.get();
		Bean targetBean = ActionUtil.getTargetBeanForViewAndCollectionBinding(facesView, collectionName, elementBizId);
    	User user = persistence.getUser();
    	Customer customer = user.getCustomer();
    	Module targetModule = customer.getModule(targetBean.getBizModule());
		Document targetDocument = targetModule.getDocument(customer, targetBean.getBizDocument());
		View view = targetDocument.getView(facesView.getUxUi().getName(), customer, targetBean.isCreated() ? ViewType.edit : ViewType.create);
    	Action action = view.getAction(actionName);
    	Boolean clientValidation = action.getClientValidation();
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("ExecuteActionAction - client validation = " + (! Boolean.FALSE.equals(clientValidation)));
    	String resourceName = action.getResourceName();
    	
		if (! user.canExecuteAction(targetDocument, resourceName)) {
			throw new SecurityException(resourceName, user.getName());
		}

		ServerSideAction<Bean> serverSideAction = (ServerSideAction<Bean>) action.getServerSideAction(customer, targetDocument);
	    if (Boolean.FALSE.equals(clientValidation) || FacesAction.validateRequiredFields()) {
			CustomerImpl internalCustomer = (CustomerImpl) customer;
			WebContext webContext = facesView.getWebContext();
			boolean vetoed = internalCustomer.interceptBeforeServerSideAction(targetDocument,
																				resourceName,
																				targetBean,
																				webContext);
			if (! vetoed) {
				ServerSideActionResult<Bean> result = serverSideAction.execute(targetBean, webContext);
				internalCustomer.interceptAfterServerSideAction(targetDocument, resourceName, result, webContext);
				ActionUtil.setTargetBeanForViewAndCollectionBinding(facesView, collectionName, (T) result.getBean());
			}
		}

	    return null;
	}
}
