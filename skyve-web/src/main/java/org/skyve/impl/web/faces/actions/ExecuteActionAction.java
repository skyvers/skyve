package org.skyve.impl.web.faces.actions;

import org.skyve.domain.Bean;
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
import org.skyve.util.Util;
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
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info("ExecuteActionAction - actionName=" + actionName + " : collectionName=" + collectionName + " : elementBizId=" + elementBizId);

		AbstractPersistence persistence = AbstractPersistence.get();
		Bean targetBean = ActionUtil.getTargetBeanForViewAndCollectionBinding(facesView, collectionName, elementBizId);
    	User user = persistence.getUser();
    	Customer customer = user.getCustomer();
    	Module targetModule = customer.getModule(targetBean.getBizModule());
		Document targetDocument = targetModule.getDocument(customer, targetBean.getBizDocument());
		View view = targetDocument.getView(facesView.getUxUi().getName(), customer, targetBean.isCreated() ? ViewType.edit : ViewType.create);
    	Action action = view.getAction(actionName);
		ServerSideAction<Bean> serverSideAction = (ServerSideAction<Bean>) action.getServerSideAction(customer, targetDocument);
	    if (Boolean.FALSE.equals(action.getClientValidation())) {
	    	ServerSideActionResult result = serverSideAction.execute(targetBean, facesView.getWebContext());
	    	ActionUtil.setTargetBeanForViewAndCollectionBinding(facesView, collectionName, (T) result.getBean());
	    }
	    else {
			if (FacesAction.validateRequiredFields()) {
				CustomerImpl internalCustomer = (CustomerImpl) customer;
				WebContext webContext = facesView.getWebContext();
				boolean vetoed = internalCustomer.interceptBeforeServerSideAction(targetDocument,
																					actionName,
																					targetBean,
																					webContext);
				if (! vetoed) {
					ServerSideActionResult result = serverSideAction.execute(targetBean, webContext);
					internalCustomer.interceptAfterServerSideAction(targetDocument, actionName, result, webContext);
					ActionUtil.setTargetBeanForViewAndCollectionBinding(facesView, collectionName, (T) result.getBean());
				}
			}	    	
		}

	    return null;
	}
}
