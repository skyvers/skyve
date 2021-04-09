package org.skyve.impl.web.faces.actions;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Binder;
import org.skyve.util.Util;

public class SetTitleAction extends FacesAction<Void> {
	private FacesView<? extends Bean> facesView;
	public SetTitleAction(FacesView<? extends Bean> facesView) {
		this.facesView = facesView;
	}
	
	@Override
	public Void callback() throws Exception {
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info("SetTitleAction");

		if (facesView.getBean() != null) {
			User user = CORE.getUser();
			Customer customer = user.getCustomer();
			Bean targetBean = ActionUtil.getTargetBeanForViewAndCollectionBinding(facesView, null, null);
	    	Module targetModule = customer.getModule(targetBean.getBizModule());
			Document targetDocument = targetModule.getDocument(customer, targetBean.getBizDocument());
			View view = targetDocument.getView(facesView.getUxUi().getName(), 
												customer, 
												targetBean.isCreated() ? 
													ViewType.edit.toString() : 
													ViewType.create.toString());
			facesView.setTitle(Binder.formatMessage(view.getLocalisedTitle(), targetBean));
	    }
		else {
        	UtilImpl.LOGGER.warning("SetTitleAction: FacesView.getBean() yields null");
		}
		
		return null;
	}
}
