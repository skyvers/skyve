package org.skyve.impl.web.faces.components;

import java.io.IOException;

import javax.faces.component.FacesComponent;
import javax.faces.component.UIComponentBase;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;

import org.skyve.CORE;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.router.UxUiSelector;
import org.skyve.web.UserAgentType;

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
		UserAgentType userAgentType = UserAgent.getType(request);
		request.setAttribute(FacesUtil.USER_AGENT_TYPE_KEY, userAgentType);
		Router router = CORE.getRepository().getRouter();
		UxUi uxui;
		try {
			uxui = ((UxUiSelector) router.getUxuiSelector()).select(userAgentType, request);
		}
		catch (Exception e) {
			throw new IllegalStateException(e);
		}
		request.setAttribute(AbstractWebContext.UXUI, uxui);
	}
}