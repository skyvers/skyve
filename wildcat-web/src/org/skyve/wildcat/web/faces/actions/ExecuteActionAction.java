package org.skyve.wildcat.web.faces.actions;

import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.web.faces.FacesAction;
import org.skyve.wildcat.web.faces.beans.FacesView;

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
    	AbstractPersistence persistence = AbstractPersistence.get();
		Bean targetBean = ActionUtil.getTargetBeanForViewAndCollectionBinding(facesView, collectionName, elementBizId);
    	User user = persistence.getUser();
    	Customer customer = user.getCustomer();
    	Module targetModule = customer.getModule(targetBean.getBizModule());
		Document targetDocument = targetModule.getDocument(customer, targetBean.getBizDocument());
		View view = targetDocument.getView(facesView.getUxUi(), customer, targetBean.isCreated() ? ViewType.edit : ViewType.create);
    	Action action = view.getAction(actionName);

		ServerSideAction<Bean> serverSideAction = (ServerSideAction<Bean>) action.getServerSideAction(customer, targetDocument);
    	ServerSideActionResult result = serverSideAction.execute(targetBean, facesView.getWebContext());
    	
    	ActionUtil.setTargetBeanForViewAndCollectionBinding(facesView, collectionName, (T) result.getBean());
    	
    	return null;
	}
}
