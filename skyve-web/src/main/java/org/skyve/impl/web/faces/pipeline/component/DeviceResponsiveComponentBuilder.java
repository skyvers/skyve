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
import jakarta.annotation.Nullable;
import jakarta.faces.component.UIComponent;

public class DeviceResponsiveComponentBuilder extends ResponsiveComponentBuilder {
	/**
	 * No spacers rendered for phones.
	 */
	@Override
	public @Nonnull UIComponent spacer(@Nullable UIComponent component, @Nonnull org.skyve.impl.metadata.view.widget.Spacer spacer) {
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
	protected @Nonnull CommandButton actionButton(@Nullable EscapableText title,
													@Nullable String iconStyleClass,
													@Nullable String tooltip, 
													@Nullable ImplicitActionName implicitActionName,
													@Nullable String actionName, 
													boolean inline, 
													@Nullable String dataWidgetBinding, 
													@Nullable String dataWidgetVar,
													@Nullable Integer pixelWidth, 
													@Nullable Integer pixelHeight,
													@Nullable EscapableText confirmationText,
													@Nullable String disabled, 
													@Nullable String formDisabled,
													@Nullable String invisible,
													@Nullable String processOverride,
													@Nullable String updateOverride,
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
	public @Nonnull UIComponent upload(@Nullable UIComponent component, 
										@Nullable String label,
										@Nullable String iconStyleClass,
										@Nullable String toolTip,
										@Nullable String confirmationText,
										@Nonnull Action action) {
		if (component != null) {
			return component;
		}

		ContentCapture capture = resolveActionUploadCapture(action);
		return uploadButton(EscapableText.of(label, ViewRenderer.shouldEscape(action.getEscapeDisplayName())),
								iconStyleClass,
								toolTip,
								action.getName(),
								null,
								null,
								action.getClientValidation(),
								EscapableText.of(confirmationText, ViewRenderer.shouldEscape(action.getEscapeConfirm())),
								action.getDisabledCondition(),
								null,
								action.getInvisibleCondition(),
								capture,
								UserAgentType.phone.equals(userAgentType) || useActionUploadDialog(capture));
	}

	/**
	 * Creates an upload button using a dialog instead of an overlay panel on phones.
	 *
	 * <p>Side effects: delegates to the upload-button helper. The resolved label and
	 * confirmation text remain raw with their metadata escape flags; tooltip text is
	 * assigned raw to the component title property.
	 */
	@Override
	public @Nonnull UIComponent uploadButton(@Nullable UIComponent component,
											@Nullable String label, 
											@Nullable String iconStyleClass, 
											@Nullable String toolTip,
											@Nullable String confirmationText, 
											@Nonnull Button button, 
											@Nullable String formDisabledCondition, 
											@Nonnull Action action) {
		if (component != null) {
			return component;
		}

		ContentCapture capture = resolveActionUploadCapture(action);
		return uploadButton(EscapableText.of(label, ViewRenderer.shouldEscape(action.getEscapeDisplayName())),
								iconStyleClass,
								toolTip,
								action.getName(),
								button.getPixelWidth(),
								button.getPixelHeight(),
								action.getClientValidation(),
								EscapableText.of(confirmationText, ViewRenderer.shouldEscape(action.getEscapeConfirm())),
								action.getDisabledCondition(),
								formDisabledCondition,
								action.getInvisibleCondition(),
								capture,
								UserAgentType.phone.equals(userAgentType) || useActionUploadDialog(capture));
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
