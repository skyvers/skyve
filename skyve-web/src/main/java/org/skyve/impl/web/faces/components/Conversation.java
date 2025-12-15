package org.skyve.impl.web.faces.components;

import java.io.IOException;
import java.util.Map;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;

import jakarta.faces.component.FacesComponent;
import jakarta.faces.component.UIComponentBase;
import jakarta.faces.context.FacesContext;

@FacesComponent(Conversation.COMPONENT_TYPE)
public class Conversation extends UIComponentBase {
	public static final String COMPONENT_TYPE = "org.skyve.impl.web.faces.components.Conversation";

    private static final Logger FACES_LOGGER = Category.FACES.logger();

	@Override
	public String getFamily() {
		return "conversation";
	}

	@Override
	public void encodeBegin(FacesContext context) throws IOException {
		Map<String, Object> attributes = getAttributes();
		final String managedBeanName = (String) attributes.get("managedBean");

		if (UtilImpl.FACES_TRACE) FACES_LOGGER.info("Conversation - {} is the subject of the conversation.", managedBeanName);

		FacesContext.getCurrentInstance().getViewRoot().getAttributes().put(FacesUtil.MANAGED_BEAN_NAME_KEY, managedBeanName);
	}
}