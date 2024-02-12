package org.skyve.impl.web.faces.actions;

import org.skyve.CORE;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;
import org.skyve.web.WebAction;

import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.HttpServletRequest;

public class PreRenderColdHitAction extends FacesAction<Void> {
	private FacesView facesView;
	public PreRenderColdHitAction(FacesView facesView) {
		this.facesView = facesView;
	}

	@Override
	public Void callback() throws Exception {
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info("PreRenderColdHitAction");

		// Set the UX/UI and user agent type
		HttpServletRequest request = (HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest();
		if (facesView.getUserAgentType() == null) {
			facesView.setUserAgentType(UserAgent.getType(request));
		}
		if (facesView.getUxUi() == null) {
			facesView.setUxUi(UserAgent.getUxUi(request));
		}
		
		User user = CORE.getUser();
		if (user == null) {
			return null;
		}

		facesView.initialise();
		WebAction webAction = facesView.getWebActionParameter();

		StringBuilder log = new StringBuilder(128);
		log.append("ColdHit - a=").append(webAction);
		log.append(" : m=").append(facesView.getBizModuleParameter());
		log.append(" : d=").append(facesView.getBizDocumentParameter());
		log.append(" : q=").append(facesView.getQueryNameParameter());
		log.append(" : i=").append(facesView.getBizIdParameter());
		Util.LOGGER.info(log.toString());
		switch (webAction) {
		case e:
			new EditAction(facesView).callback(); // execute without error trapping
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
