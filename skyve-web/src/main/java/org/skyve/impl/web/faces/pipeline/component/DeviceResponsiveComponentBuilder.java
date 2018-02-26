package org.skyve.impl.web.faces.pipeline.component;

import javax.faces.component.UIComponent;

import org.primefaces.component.commandbutton.CommandButton;
import org.skyve.impl.web.UserAgent.UserAgentType;
import org.skyve.metadata.controller.ImplicitActionName;

public class DeviceResponsiveComponentBuilder extends ResponsiveComponentBuilder {
	/**
	 * No spacers rendered for phones.
	 */
	@Override
	public UIComponent spacer(UIComponent component, org.skyve.impl.metadata.view.widget.Spacer spacer) {
		if (component != null) {
			return component;
		}

		if (UserAgentType.phone.equals(userAgentType)) {
			return null;
		}
		return super.spacer(component, spacer);
	}
	
	/**
	 * Buttons as wide as their layouts allow on phones.
	 */
	@Override
	protected CommandButton actionButton(String title, 
											String iconStyleClass,
											String tooltip, 
											ImplicitActionName implicitActionName,
											String actionName, 
											boolean inline, 
											String listBinding, 
											String listVar,
											Integer pixelWidth, 
											Integer pixelHeight,
											Boolean clientValidation, 
											String confirmationText, 
											String disabled, 
											String invisible,
											String processOverride,
											String updateOverride) {
		if (UserAgentType.phone.equals(userAgentType)) {
			return super.actionButton(title, 
										iconStyleClass,
										tooltip, 
										implicitActionName, 
										actionName, 
										inline, 
										listBinding, 
										listVar,
										null, 
										null,
										clientValidation, 
										confirmationText, 
										disabled, 
										invisible,
										processOverride,
										updateOverride);
		}

		return super.actionButton(title, 
									iconStyleClass,
									tooltip, 
									implicitActionName, 
									actionName, 
									inline, 
									listBinding, 
									listVar,
									pixelWidth, 
									pixelHeight,
									clientValidation, 
									confirmationText, 
									disabled, 
									invisible,
									processOverride,
									updateOverride);
	}
}
