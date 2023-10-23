package org.skyve.impl.web.faces.renderers;

import java.io.IOException;
import java.util.Map;

import javax.faces.application.Application;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.primefaces.behavior.ajax.AjaxBehavior;
import org.primefaces.component.panel.Panel;
import org.primefaces.component.panel.PanelRenderer;

public class SkyvePanelRenderer extends PanelRenderer {
	@Override
	public void encodeBegin(FacesContext context, UIComponent component) throws IOException {
		Panel panel = (Panel) component;
		String clientId = panel.getClientId(context);

		Map<String, String> params = context.getExternalContext().getRequestParameterMap();
		String moduleName = params.get("m");
		String documentName = params.get("d");
		String panelUniqueName = moduleName + "_" + documentName + "_" + clientId;

		Boolean collapsed = (Boolean) context.getExternalContext().getSessionMap().get(panelUniqueName);
		if (collapsed != null) {
			panel.setCollapsed(collapsed.booleanValue());
		}

		// Saving the state of the panel toggle into the session when the user toggle
		// the panel by this ajax to call the decode method
		Application a = context.getApplication();
		AjaxBehavior ajax = (AjaxBehavior) a.createBehavior(AjaxBehavior.BEHAVIOR_ID);
		StringBuilder start = new StringBuilder(64);
		start.append("console.log('#');");
		ajax.setOnstart(start.toString());
		panel.addClientBehavior("toggle", ajax);

		super.encodeBegin(context, component);
	}

	@Override
	public void decode(FacesContext context, UIComponent component) {
		Panel panel = (Panel) component;
		String clientId = panel.getClientId(context);
		Map<String, String> params = context.getExternalContext().getRequestParameterMap();
		String referer = context.getExternalContext().getRequestHeaderMap().get("Referer");
		referer = referer.substring(referer.indexOf("?") + 1);
		String[] urlParams = referer.split("&");
		String moduleName = "";
		String documentName = "";
		for (String param : urlParams) {
			String paramName = param.split("=")[0];
			String paramValue = param.split("=")[1];
			if (paramName.equals("m")) {
				moduleName = paramValue;
			} else if (paramName.equals("d")) {
				documentName = paramValue;
			}
		}
		String panelUniqueName = moduleName + "_" + documentName + "_" + clientId;
		// Store toggle state
		String collapsedParam = params.get(clientId + "_collapsed");

		if ("true".equals(collapsedParam) || "false".equals(collapsedParam)) {
			context.getExternalContext().getSessionMap().put(panelUniqueName, Boolean.valueOf(collapsedParam));
		}
		super.decode(context, component);
	}
}
