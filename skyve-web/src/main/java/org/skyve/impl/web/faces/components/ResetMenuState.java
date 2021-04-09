package org.skyve.impl.web.faces.components;

import java.io.IOException;

import javax.el.ELContext;
import javax.faces.component.FacesComponent;
import javax.faces.component.UIComponentBase;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.WebUtil;
import org.skyve.impl.web.faces.beans.Menu;

@FacesComponent(ResetMenuState.COMPONENT_TYPE)
public class ResetMenuState extends UIComponentBase {
	public static final String COMPONENT_TYPE = "org.skyve.impl.web.faces.components.ResetMenuState";

	@Override
	public String getFamily() {
		return "resetMenuState";
	}

	@Override
	public void encodeBegin(FacesContext context) throws IOException {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("Menu State Cookies deleted...");

		ELContext elc = context.getELContext();
		Menu menu = (Menu) elc.getELResolver().getValue(elc, null, "menu");
		if (menu != null) {
			menu.resetState();
		}

		ExternalContext ec = context.getExternalContext();
		WebUtil.deleteMenuCookies((HttpServletRequest) ec.getRequest(), (HttpServletResponse) ec.getResponse());
	}
}
