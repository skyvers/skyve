package org.skyve.wildcat.web.faces.actions;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.web.WebAction;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.web.faces.FacesAction;
import org.skyve.wildcat.web.faces.beans.FacesView;

public class PreRenderAction<T extends Bean> extends FacesAction<Void> {
	private FacesView<T> facesView;
	public PreRenderAction(FacesView<T> facesView) {
		this.facesView = facesView;
	}

	@Override
	public Void callback() throws Exception {
//System.out.println("GO " + this);
		AbstractPersistence persistence = (AbstractPersistence) CORE.getPersistence();
		org.skyve.wildcat.metadata.user.User internalUser = (org.skyve.wildcat.metadata.user.User) persistence.getUser();
		Customer customer = internalUser.getCustomer();

		facesView.initialise(customer, internalUser);

		WebAction webAction = facesView.getWebActionParameter();
		if (webAction == null) {
			// view type is set by the harness if no module or document is sent in the request
			ViewType viewType = facesView.getViewType();
			if (viewType == null) {
				webAction = (facesView.getQueryNameParameter() != null) ? WebAction.g : WebAction.e;
			}
			else {
				if (ViewType.edit.equals(viewType)) {
					webAction = WebAction.e;
				}
				else {
					facesView.setQueryNameParameter(facesView.getBizDocumentParameter());
					facesView.setBizDocumentParameter(null);
					webAction = WebAction.g;
				}
			}
			facesView.setWebActionParameter(webAction);
		}

System.out.println("GO " + facesView.getWebActionParameter() + " : " + facesView.getBizModuleParameter() + " : " + facesView.getBizDocumentParameter() + " : " + facesView.getQueryNameParameter());
		switch (webAction) {
		case e:
			new EditAction<>(facesView).callback(); // execute without error trapping
//System.out.println("EDIT BEAN = " + FacesView.this.getBean());
			new SetTitleAction(facesView).callback(); // execute without error trapping
			break;
		case g:
			new PopulateAction(facesView).callback(); // execute without error trapping
			break;
		default:
			break;
		}

		return null;
	}
}
