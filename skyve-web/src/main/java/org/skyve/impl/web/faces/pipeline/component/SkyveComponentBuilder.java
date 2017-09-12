package org.skyve.impl.web.faces.pipeline.component;

import org.primefaces.component.commandbutton.CommandButton;
import org.primefaces.component.spacer.Spacer;
import org.skyve.impl.web.UserAgent.UserAgentType;
import org.skyve.metadata.controller.ImplicitActionName;

public class SkyveComponentBuilder extends ResponsiveComponentBuilder {
	/**
	 * No spacers rendered for phones.
	 */
	@Override
	public Spacer spacer(org.skyve.impl.metadata.view.widget.Spacer spacer) {
		if (UserAgentType.phone.equals(userAgentType)) {
			return null;
		}
		return super.spacer(spacer);
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
											Integer pixelWidth, 
											Integer pixelHeight,
											Boolean clientValidation, 
											String confirmationText, 
											String disabled, 
											String invisible) {
		if (UserAgentType.phone.equals(userAgentType)) {
			return super.actionButton(title, 
										iconStyleClass,
										tooltip, 
										implicitActionName, 
										actionName, 
										inline, 
										listBinding, 
										null, 
										null,
										clientValidation, 
										confirmationText, 
										disabled, 
										invisible);
		}

		return super.actionButton(title, 
									iconStyleClass,
									tooltip, 
									implicitActionName, 
									actionName, 
									inline, 
									listBinding, 
									pixelWidth, 
									pixelHeight,
									clientValidation, 
									confirmationText, 
									disabled, 
									invisible);
}
}
