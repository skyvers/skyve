package org.skyve.impl.web.faces.pipeline.component;

import org.primefaces.component.commandbutton.CommandButton;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.view.Action;
import org.skyve.web.UserAgentType;

import jakarta.faces.component.UIComponent;

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
											String dataWidgetBinding, 
											String dataWidgetVar,
											Integer pixelWidth, 
											Integer pixelHeight,
											Boolean clientValidation, 
											String confirmationText, 
											String disabled, 
											String formDisabled,
											String invisible,
											String processOverride,
											String updateOverride,
											boolean canDelete) {
		if (UserAgentType.phone.equals(userAgentType)) {
			return super.actionButton(title, 
										iconStyleClass,
										tooltip, 
										implicitActionName, 
										actionName, 
										inline, 
										dataWidgetBinding, 
										dataWidgetVar,
										null, 
										null,
										clientValidation, 
										confirmationText, 
										disabled, 
										formDisabled,
										invisible,
										processOverride,
										updateOverride,
										canDelete);
		}

		return super.actionButton(title, 
									iconStyleClass,
									tooltip, 
									implicitActionName, 
									actionName, 
									inline, 
									dataWidgetBinding, 
									dataWidgetVar,
									pixelWidth, 
									pixelHeight,
									clientValidation, 
									confirmationText, 
									disabled, 
									formDisabled,
									invisible,
									processOverride,
									updateOverride,
									canDelete);
	}
	
	/**
	 * Use a dialog instead of an overlay panel for phones.
	 */
	@Override
	public UIComponent upload(UIComponent component, 
								String label,
								String iconStyleClass,
								String toolTip,
								String confirmationText,
								Action action) {
		if (component != null) {
			return component;
		}

		return uploadButton(label,
								iconStyleClass,
								toolTip,
								action.getName(),
								null,
								null,
								action.getClientValidation(),
								confirmationText,
								action.getDisabledConditionName(),
								null,
								action.getInvisibleConditionName(),
								UserAgentType.phone.equals(userAgentType));
	}

	/**
	 * Use a dialog instead of an overlay panel for phones.
	 */
	@Override
	public UIComponent uploadButton(UIComponent component,
									String label, 
									String iconStyleClass, 
									String toolTip,
									String confirmationText, 
									Button button, 
									String formDisabledConditionName, 
									Action action) {
		if (component != null) {
			return component;
		}

		return uploadButton(label,
								iconStyleClass,
								toolTip,
								action.getName(),
								button.getPixelWidth(),
								button.getPixelHeight(),
								action.getClientValidation(),
								confirmationText,
								action.getDisabledConditionName(),
								formDisabledConditionName,
								action.getInvisibleConditionName(),
								UserAgentType.phone.equals(userAgentType));
	}
}
