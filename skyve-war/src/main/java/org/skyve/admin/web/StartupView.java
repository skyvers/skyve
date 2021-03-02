package org.skyve.admin.web;

import java.io.IOException;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.util.Util;
import org.skyve.web.WebAction;

import modules.admin.domain.Startup;
import router.DefaultUxUiSelector;

@ViewScoped
@ManagedBean(name = "skyveStartup")
public class StartupView extends FacesView<Startup> {
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
