package org.skyve.impl.web.faces.actions;

import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.router.UxUiSelector;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;
import org.skyve.web.UserAgentType;
import org.skyve.web.WebAction;

public class PreRenderAction<T extends Bean> extends FacesAction<Void> {
	private FacesView<T> facesView;
	public PreRenderAction(FacesView<T> facesView) {
		this.facesView = facesView;
	}

	@Override
	public Void callback() throws Exception {
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info("PreRenderAction");

		// Set the UX/UI and user agent type
		HttpServletRequest request = (HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest();
		UserAgentType userAgentType = facesView.getUserAgentType();
		if (userAgentType == null) {
			userAgentType = UserAgent.getType(request);
			facesView.setUserAgentType(userAgentType);
		}
		if (facesView.getUxUi() == null) {
			Router router = CORE.getRepository().getRouter();
			UxUi uxui = ((UxUiSelector) router.getUxuiSelector()).select(userAgentType, request);
			facesView.setUxUi(uxui);
		}
		
		User user = CORE.getUser();
		if (user == null) {
			return null;
		}

		facesView.initialise();
		WebAction webAction = facesView.getWebActionParameter();

		if (UtilImpl.FACES_TRACE) Util.LOGGER.info("PreRenderAction - GO a=" + webAction + 
													" : m=" + facesView.getBizModuleParameter() + 
													" : d=" + facesView.getBizDocumentParameter() + 
													" : q=" + facesView.getQueryNameParameter() + 
													" : i=" + facesView.getBizIdParameter());
		switch (webAction) {
		case e:
			new EditAction<>(facesView).callback(); // execute without error trapping
			new SetTitleAction(facesView).callback(); // execute without error trapping
			break;
		case l:
			new PopulateAction(facesView).callback(); // execute without error trapping
			break;
		default:
			break;
		}

		return null;
	}
}
