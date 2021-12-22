package org.skyve.impl.web.faces.components;

import java.io.IOException;

import javax.faces.component.FacesComponent;
import javax.faces.component.UIComponentBase;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;

import org.skyve.impl.web.UserAgent;

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
