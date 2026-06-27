package org.skyve.impl.web.faces.pipeline.component;

import org.primefaces.component.commandbutton.CommandButton;
import org.skyve.impl.generate.ViewRenderer;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.bound.input.ContentCapture;
import org.skyve.impl.metadata.view.widget.bound.input.ContentDisplay;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.view.Action;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nonnull;
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
	 * Creates an action button, forcing phone buttons to occupy the layout width.
	 *
	 * <p>Side effects: delegates to the tabular action-button helper after replacing
	 * explicit dimensions with {@code null} on phones. {@code title} and
	 * {@code confirmationText} remain raw and carry their nullable escape flags to
	 * the PrimeFaces output boundary; {@code tooltip} is assigned raw to the
	 * component title property.
	 */
	@Override
	protected CommandButton actionButton(EscapableText title,
											String iconStyleClass,
											String tooltip, 
											ImplicitActionName implicitActionName,
											String actionName, 
											boolean inline, 
											String dataWidgetBinding, 
											String dataWidgetVar,
											Integer pixelWidth, 
											Integer pixelHeight,
											EscapableText confirmationText,
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
									confirmationText, 
									disabled, 
									formDisabled,
									invisible,
									processOverride,
									updateOverride,
									canDelete);
	}
	
	/**
	 * Creates an upload action using a dialog instead of an overlay panel on phones.
	 *
	 * <p>Side effects: delegates to the upload-button helper. The resolved label and
	 * confirmation text remain raw with their metadata escape flags; tooltip text is
	 * assigned raw to the component title property.
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

		return uploadButton(EscapableText.of(label, ViewRenderer.shouldEscape(action.getEscapeDisplayName())),
								iconStyleClass,
								toolTip,
								action.getName(),
								null,
								null,
								action.getClientValidation(),
								EscapableText.of(confirmationText, ViewRenderer.shouldEscape(action.getEscapeConfirm())),
								action.getDisabledConditionName(),
								null,
								action.getInvisibleConditionName(),
								resolveActionUploadCapture(action),
								UserAgentType.phone.equals(userAgentType));
	}

	/**
	 * Creates an upload button using a dialog instead of an overlay panel on phones.
	 *
	 * <p>Side effects: delegates to the upload-button helper. The resolved label and
	 * confirmation text remain raw with their metadata escape flags; tooltip text is
	 * assigned raw to the component title property.
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

		return uploadButton(EscapableText.of(label, ViewRenderer.shouldEscape(action.getEscapeDisplayName())),
								iconStyleClass,
								toolTip,
								action.getName(),
								button.getPixelWidth(),
								button.getPixelHeight(),
								action.getClientValidation(),
								EscapableText.of(confirmationText, ViewRenderer.shouldEscape(action.getEscapeConfirm())),
								action.getDisabledConditionName(),
								formDisabledConditionName,
								action.getInvisibleConditionName(),
								resolveActionUploadCapture(action),
								UserAgentType.phone.equals(userAgentType));
	}

	@Override
	protected boolean useGeometryDialog() {
		return UserAgentType.phone.equals(userAgentType);
	}

	@Override
	protected boolean useContentUploadDialog(boolean image, @Nonnull ContentDisplay display, @Nonnull ContentCapture capture) {
		return UserAgentType.phone.equals(userAgentType) || super.useContentUploadDialog(image, display, capture);
	}
}
