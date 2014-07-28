package org.skyve.wildcat.web.faces.actions;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Binder;
import org.skyve.wildcat.web.faces.FacesAction;
import org.skyve.wildcat.web.faces.beans.FacesView;

public class SetTitleAction extends FacesAction<Void> {
	private FacesView<? extends Bean> facesView;
	public SetTitleAction(FacesView<? extends Bean> facesView) {
		this.facesView = facesView;
	}
	
	@Override
	public Void callback() throws Exception {
       	if (facesView.getBean() != null) {
	    	Customer customer = CORE.getUser().getCustomer();
			Bean targetBean = ActionUtil.getTargetBeanForViewAndCollectionBinding(facesView, null, null);
	    	Module targetModule = customer.getModule(targetBean.getBizModule());
			Document targetDocument = targetModule.getDocument(customer, targetBean.getBizDocument());
			View view = targetDocument.getView(facesView.getUxUi(), customer, targetBean.isCreated() ? ViewType.edit : ViewType.create);
			facesView.setTitle(Binder.formatMessage(customer, view.getTitle(), targetBean));
	    }
		
		return null;
	}
}
