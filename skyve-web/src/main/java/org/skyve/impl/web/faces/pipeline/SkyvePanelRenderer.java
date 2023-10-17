package org.skyve.impl.web.faces.pipeline;

import java.io.IOException;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.primefaces.component.menu.Menu;
import org.primefaces.component.panel.Panel;
import org.primefaces.component.panel.PanelRenderer;

public class SkyvePanelRenderer extends PanelRenderer {

	@Override
	public void encodeBegin(FacesContext context, UIComponent component) throws IOException {
		Panel panel 		= (Panel) component;
		String clientId 	= panel.getClientId(context);
		String collapsed 	= context.getExternalContext().getSessionMap().get(clientId) + "";
		
		if (collapsed != null) {
			panel.setCollapsed(Boolean.parseBoolean(collapsed));
		}
		
		super.encodeBegin(context, component);
	}

	@Override
	public void decode(FacesContext context, UIComponent component) {

		Panel panel 				= (Panel) component;
		String clientId 			= panel.getClientId(context);
		Map<String, String> params 	= context.getExternalContext().getRequestParameterMap();

		// Store toggle state
		String collapsedParam = params.get(clientId + "_collapsed");
		
		if ("true".equals(collapsedParam) || "false".equals(collapsedParam)) 
		{
			context.getExternalContext().getSessionMap().put(clientId, collapsedParam);
		}
		super.decode(context, component);
	}

}
