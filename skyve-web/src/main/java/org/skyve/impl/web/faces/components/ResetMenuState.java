package org.skyve.impl.web.faces.components;

import java.io.IOException;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.WebUtil;
import org.skyve.impl.web.faces.views.MenuView;

import jakarta.el.ELContext;
import jakarta.faces.component.FacesComponent;
import jakarta.faces.component.UIComponentBase;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

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
		MenuView menu = (MenuView) elc.getELResolver().getValue(elc, null, "menu");
		if (menu != null) {
			menu.resetState();
		}

		ExternalContext ec = context.getExternalContext();
		WebUtil.deleteMenuCookies((HttpServletRequest) ec.getRequest(), (HttpServletResponse) ec.getResponse());
	}
}
