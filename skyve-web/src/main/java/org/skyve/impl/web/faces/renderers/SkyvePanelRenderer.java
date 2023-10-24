package org.skyve.impl.web.faces.renderers;

import java.io.IOException;
import java.util.Map;

import javax.faces.application.Application;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

import org.primefaces.behavior.ajax.AjaxBehavior;
import org.primefaces.component.panel.Panel;
import org.primefaces.component.panel.PanelRenderer;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.beans.FacesView;

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
		
		String moduleName 			= "";
		String documentName 		= "";		
		UIViewRoot uiViewRoot 		= context.getViewRoot();
		
		if (uiViewRoot != null) {
			String managedBeanName = (String) uiViewRoot.getAttributes().get(FacesUtil.MANAGED_BEAN_NAME_KEY);
			if (managedBeanName != null) {
				FacesView<?> view = FacesUtil.getManagedBean(managedBeanName);
				moduleName   = view.getBizModuleParameter();
				documentName = view.getBizDocumentParameter();
			}
		}
		Panel panel 				= (Panel) component;
		String clientId 			= panel.getClientId(context);
		
		Map<String, String> params 	= context.getExternalContext().getRequestParameterMap();
		String panelUniqueName 		= moduleName + "_" + documentName + "_" + clientId;
		String collapsedParam 		= params.get(clientId + "_collapsed");

		if ("true".equals(collapsedParam) || "false".equals(collapsedParam)) {
			context.getExternalContext().getSessionMap().put(panelUniqueName, Boolean.valueOf(collapsedParam));
		}
		super.decode(context, component);
	}
}
