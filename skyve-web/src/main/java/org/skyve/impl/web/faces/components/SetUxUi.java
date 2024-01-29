package org.skyve.impl.web.faces.components;

import java.io.IOException;

import org.skyve.impl.web.UserAgent;

import jakarta.faces.component.FacesComponent;
import jakarta.faces.component.UIComponentBase;
import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.HttpServletRequest;

@FacesComponent(SetUxUi.COMPONENT_TYPE)
public class SetUxUi extends UIComponentBase {
	public static final String COMPONENT_TYPE = "org.skyve.impl.web.faces.components.SetUxUi";

	@Override
	public String getFamily() {
		return "setUxUi";
	}

	
	@Override
	public void encodeBegin(FacesContext context) throws IOException {
		// Set the UX/UI and user agent type
		HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();
		try {
			UserAgent.getUxUi(request); // sets the request attribute
		}
		catch (Exception e) {
			throw new IllegalStateException(e);
		}
	}
}
