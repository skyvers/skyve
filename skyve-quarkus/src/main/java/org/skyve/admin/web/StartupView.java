package org.skyve.admin.web;

import java.io.IOException;

import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.util.Util;
import org.skyve.web.WebAction;

import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Named;
import modules.admin.domain.Startup;
import router.DefaultUxUiSelector;

@ViewScoped
@Named("skyveStartup")
public class StartupView extends FacesView {
	private static final long serialVersionUID = -5794022492296214306L;

	@Override
	public void preRender() {
		if (! FacesContext.getCurrentInstance().isPostback()) {
			setWebActionParameter(WebAction.e);
			setBizModuleParameter(Startup.MODULE_NAME);
			setBizDocumentParameter(Startup.DOCUMENT_NAME);
		}
		super.preRender();
	}
	
	@Override
	public void action(String actionName, String dataWidgetBinding, String bizId) {
		super.action(actionName, dataWidgetBinding, bizId);
		try {
			// put an attribute on the user's session so this screen isn't shown again for this session
			ExternalContext ec = FacesContext.getCurrentInstance().getExternalContext();
			ec.getSessionMap().put(DefaultUxUiSelector.DISMISS_STARTUP, Boolean.TRUE);
			ec.redirect(Util.getSkyveContextUrl() + "/");
		}
		catch (@SuppressWarnings("unused") IOException e) {
			// do nothing
		}
	}
}
