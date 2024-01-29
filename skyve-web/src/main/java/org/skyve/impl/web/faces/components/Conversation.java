package org.skyve.impl.web.faces.components;

import java.io.IOException;
import java.util.Map;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesUtil;

import jakarta.faces.component.FacesComponent;
import jakarta.faces.component.UIComponentBase;
import jakarta.faces.context.FacesContext;

@FacesComponent(Conversation.COMPONENT_TYPE)
public class Conversation extends UIComponentBase {
	public static final String COMPONENT_TYPE = "org.skyve.impl.web.faces.components.Conversation";

	@Override
	public String getFamily() {
		return "conversation";
	}

	@Override
	public void encodeBegin(FacesContext context) throws IOException {
		Map<String, Object> attributes = getAttributes();
		final String managedBeanName = (String) attributes.get("managedBean");

		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("Conversation - " + managedBeanName + " is the subject of the conversation.");

		FacesContext.getCurrentInstance().getViewRoot().getAttributes().put(FacesUtil.MANAGED_BEAN_NAME_KEY, managedBeanName);
	}
}