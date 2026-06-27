package org.skyve.impl.web.faces.views;

import java.io.Serializable;
import java.util.Objects;

import org.skyve.impl.metadata.view.widget.bound.input.ContentCapture;
import org.skyve.impl.metadata.view.widget.bound.input.ContentDisplay;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.util.OWASP;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Sanitised invocation state for the unified non-BizPort upload dialog.
 *
 * <p>The state derives internal page mode and upload-limit category from validated
 * route metadata. Client-provided mode is deliberately rejected so callers cannot
 * bypass the metadata/action upload contract by selecting arbitrary upload limits.
 *
 * <p>Threading: immutable and thread-safe.
 */
public final class UnifiedUploadState implements Serializable{
	private static final long serialVersionUID = 3601007643860852285L;

	/**
	 * Upload route owner.
	 */
	@SuppressWarnings("java:S115") // Values intentionally match lower-case URL/metadata values.
	public enum UploadKind {
		/**
		 * A managed content/image field upload.
		 */
		boundContent,

		/**
		 * A document upload action.
		 */
		action
	}

	/**
	 * User-selected upload affordance.
	 */
	@SuppressWarnings("java:S115") // Values intentionally match lower-case URL/metadata values.
	public enum UploadAffordance {
		/**
		 * Normal file picker.
		 */
		generic,

		/**
		 * Still-image/camera capture.
		 */
		camera,

		/**
		 * Video capture or picker.
		 */
		video
	}

	/**
	 * Internal page rendering mode.
	 */
	@SuppressWarnings("java:S115") // Values intentionally match lower-case URL/metadata values.
	public enum UploadMode {
		/**
		 * Generic file upload controls.
		 */
		file,

		/**
		 * Image/camera upload controls.
		 */
		image,

		/**
		 * Video upload controls.
		 */
		video,

		/**
		 * Multiple valid upload choices are available.
		 */
		mixed
	}

	/**
	 * Upload configuration bucket.
	 */
	@SuppressWarnings("java:S115") // Values intentionally match lower-case upload configuration keys.
	public enum UploadCategory {
		/**
		 * Action-file upload configuration.
		 */
		file,

		/**
		 * Managed-content upload configuration.
		 */
		content,

		/**
		 * Image upload/crop/camera configuration.
		 */
		image,

		/**
		 * Video upload/capture configuration.
		 */
		video
	}

	private final UploadKind uploadKind;
	private final String context;
	private final String binding;
	private final String contentBinding;
	private final String action;
	private final ContentDisplay display;
	private final ContentCapture capture;
	private final UploadMode mode;
	private final String companion;
	private final String callbackTarget;

	/**
	 * Creates immutable state after route values have been sanitised and validated.
	 *
	 * @param uploadKind upload route owner
	 * @param context web conversation/context identifier
	 * @param binding optional object binding
	 * @param contentBinding bound content/image attribute for managed-content uploads
	 * @param action upload action name for action uploads
	 * @param display resolved content display mode
	 * @param capture resolved capture mode
	 * @param companion optional auto-mode companion field name
	 * @param callbackTarget optional renderer callback target
	 */
	@SuppressWarnings("java:S107") // State object mirrors the upload invocation contract fields one-to-one.
	private UnifiedUploadState(@Nonnull UploadKind uploadKind,
								@Nonnull String context,
								@Nullable String binding,
								@Nullable String contentBinding,
								@Nullable String action,
								@Nonnull ContentDisplay display,
								@Nonnull ContentCapture capture,
								@Nullable String companion,
								@Nullable String callbackTarget) {
		this.uploadKind = uploadKind;
		this.context = context;
		this.binding = binding;
		this.contentBinding = contentBinding;
		this.action = action;
		this.display = display;
		this.capture = capture;
		this.companion = companion;
		this.callbackTarget = callbackTarget;
		mode = deriveMode(uploadKind, display, capture);
	}

	/**
	 * Builds a sanitised unified upload state from raw route values.
	 *
	 * @param uploadKindValue upload route owner value
	 * @param contextValue web conversation/context identifier
	 * @param bindingValue optional object binding
	 * @param contentBindingValue bound content/image attribute for managed-content uploads
	 * @param actionValue upload action name for action uploads
	 * @param displayValue content display value; defaults to {@link ContentDisplay#auto}
	 * @param captureValue capture value; defaults to {@link ContentCapture#none}
	 * @param clientModeValue client-supplied mode; must be blank because mode is derived
	 * @param companionValue optional auto-mode companion field name
	 * @param callbackTargetValue optional renderer callback target
	 * @return immutable upload invocation state
	 */
	@SuppressWarnings("java:S107") // Route parser accepts the upload invocation contract parameters directly.
	public static @Nonnull UnifiedUploadState fromRoute(@Nullable String uploadKindValue,
															@Nullable String contextValue,
															@Nullable String bindingValue,
															@Nullable String contentBindingValue,
															@Nullable String actionValue,
															@Nullable String displayValue,
															@Nullable String captureValue,
															@Nullable String clientModeValue,
															@Nullable String companionValue,
															@Nullable String callbackTargetValue) {
		String clientMode = clean(clientModeValue);
		if (clientMode != null) {
			throw new IllegalArgumentException("Upload mode is derived from metadata and cannot be supplied by the client.");
		}

		UploadKind uploadKind = parseRequiredEnum(UploadKind.class, uploadKindValue, "uploadKind");
		ContentDisplay display = parseEnum(ContentDisplay.class, displayValue, ContentDisplay.auto, "display");
		ContentCapture capture = parseEnum(ContentCapture.class, captureValue, ContentCapture.none, "capture");
		validateDisplayCapture(display, capture);

		String context = require("context", contextValue);
		String binding = clean(bindingValue);
		String contentBinding = clean(contentBindingValue);
		String action = clean(actionValue);
		if (UploadKind.boundContent.equals(uploadKind)) {
			if (contentBinding == null) {
				throw new IllegalArgumentException("contentBinding is required for bound content uploads.");
			}
			action = null;
		}
		else if (action == null) {
			throw new IllegalArgumentException("action is required for action uploads.");
		}
		else {
			contentBinding = null;
		}

		return new UnifiedUploadState(uploadKind,
										context,
										binding,
										contentBinding,
										action,
										display,
										capture,
										clean(companionValue),
										clean(callbackTargetValue));
	}

	/**
	 * Returns the upload category for a selected upload affordance.
	 *
	 * @param affordance selected affordance
	 * @return upload configuration category
	 */
	public @Nonnull UploadCategory categoryFor(@Nonnull UploadAffordance affordance) {
		Objects.requireNonNull(affordance, "affordance");
		if (! isAffordanceAllowed(affordance)) {
			throw new IllegalArgumentException("Upload affordance " + affordance + " is not allowed for " + display + " / " + capture + ".");
		}
		if (UploadKind.action.equals(uploadKind)) {
			if (UploadAffordance.camera.equals(affordance)) {
				return UploadCategory.image;
			}
			if (UploadAffordance.video.equals(affordance)) {
				return UploadCategory.video;
			}
			return UploadCategory.file;
		}
		if (UploadAffordance.camera.equals(affordance) || ContentDisplay.image.equals(display)) {
			return UploadCategory.image;
		}
		if (UploadAffordance.video.equals(affordance) || ContentDisplay.video.equals(display)) {
			return UploadCategory.video;
		}
		return UploadCategory.content;
	}

	/**
	 * Indicates whether an affordance is allowed by this state.
	 *
	 * @param affordance selected affordance
	 * @return {@code true} when allowed
	 */
	public boolean isAffordanceAllowed(@Nonnull UploadAffordance affordance) {
		Objects.requireNonNull(affordance, "affordance");
		if (UploadAffordance.generic.equals(affordance)) {
			return ContentCapture.none.equals(capture) || ContentCapture.all.equals(capture);
		}
		if (UploadAffordance.camera.equals(affordance)) {
			return (ContentCapture.camera.equals(capture) || ContentCapture.all.equals(capture)) &&
					! ContentDisplay.video.equals(display);
		}
		return (ContentCapture.video.equals(capture) || ContentCapture.all.equals(capture)) &&
				! ContentDisplay.image.equals(display);
	}

	/**
	 * Returns whether this state targets a bound managed-content field or an upload action.
	 *
	 * @return upload route owner
	 */
	public @Nonnull UploadKind getUploadKind() {
		return uploadKind;
	}

	/**
	 * Returns the web conversation/context identifier.
	 *
	 * @return web conversation/context identifier
	 */
	public @Nonnull String getContext() {
		return context;
	}

	/**
	 * Returns the optional object binding used to locate a nested bean before upload handling.
	 *
	 * @return object binding, or {@code null} when uploading against the current bean
	 */
	public @Nullable String getBinding() {
		return binding;
	}

	/**
	 * Returns the target content/image attribute for bound content uploads.
	 *
	 * @return content binding, or {@code null} for action uploads
	 */
	public @Nullable String getContentBinding() {
		return contentBinding;
	}

	/**
	 * Returns the upload action name for action uploads.
	 *
	 * @return upload action name, or {@code null} for bound content uploads
	 */
	public @Nullable String getAction() {
		return action;
	}

	/**
	 * Returns the resolved content display mode.
	 *
	 * @return resolved display mode
	 */
	public @Nonnull ContentDisplay getDisplay() {
		return display;
	}

	/**
	 * Returns the resolved capture mode.
	 *
	 * @return resolved capture mode
	 */
	public @Nonnull ContentCapture getCapture() {
		return capture;
	}

	/**
	 * Returns the internal upload-page mode derived from display and capture metadata.
	 *
	 * @return derived upload-page mode
	 */
	public @Nonnull UploadMode getMode() {
		return mode;
	}

	/**
	 * Returns the optional media-kind companion field name for auto display mode.
	 *
	 * @return companion field name, or {@code null} when no companion update is required
	 */
	public @Nullable String getCompanion() {
		return companion;
	}

	/**
	 * Returns the optional renderer callback target.
	 *
	 * @return callback target, or {@code null} when the caller did not provide one
	 */
	public @Nullable String getCallbackTarget() {
		return callbackTarget;
	}

	/**
	 * Derives the internal page mode from validated metadata state.
	 *
	 * @param uploadKind upload route owner
	 * @param display resolved content display mode
	 * @param capture resolved capture mode
	 * @return derived upload-page mode
	 */
	private static @Nonnull UploadMode deriveMode(@Nonnull UploadKind uploadKind,
													@Nonnull ContentDisplay display,
													@Nonnull ContentCapture capture) {
		if (ContentCapture.all.equals(capture)) {
			if (ContentDisplay.image.equals(display)) {
				return UploadMode.image;
			}
			if (ContentDisplay.video.equals(display)) {
				return UploadMode.video;
			}
			return UploadMode.mixed;
		}
		if (ContentCapture.camera.equals(capture)) {
			return UploadMode.image;
		}
		if (ContentCapture.video.equals(capture)) {
			return UploadMode.video;
		}
		if (UploadKind.boundContent.equals(uploadKind)) {
			if (ContentDisplay.image.equals(display)) {
				return UploadMode.image;
			}
			if (ContentDisplay.video.equals(display)) {
				return UploadMode.video;
			}
		}
		return UploadMode.file;
	}

	/**
	 * Rejects display/capture combinations that cannot produce a coherent upload intent.
	 *
	 * @param display resolved content display mode
	 * @param capture resolved capture mode
	 */
	private static void validateDisplayCapture(@Nonnull ContentDisplay display, @Nonnull ContentCapture capture) {
		if (ContentDisplay.image.equals(display) && ContentCapture.video.equals(capture)) {
			throw new IllegalArgumentException("capture=video is not valid for display=image.");
		}
		if (ContentDisplay.video.equals(display) && ContentCapture.camera.equals(capture)) {
			throw new IllegalArgumentException("capture=camera is not valid for display=video.");
		}
	}

	/**
	 * Cleans and requires a route value.
	 *
	 * @param name route value name used in exception text
	 * @param value raw route value
	 * @return cleaned route value
	 */
	private static @Nonnull String require(@Nonnull String name, @Nullable String value) {
		String result = clean(value);
		if (result == null) {
			throw new IllegalArgumentException(name + " is required.");
		}
		return result;
	}

	/**
	 * Normalises and sanitises a raw route value.
	 *
	 * @param value raw route value
	 * @return sanitised value, or {@code null} when blank
	 */
	private static @Nullable String clean(@Nullable String value) {
		return OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(value));
	}

	/**
	 * Parses a route enum value after sanitisation.
	 *
	 * @param enumType enum class to parse
	 * @param value raw route value
	 * @param defaultValue value returned when the raw route value is blank
	 * @param name route value name used in exception text
	 * @return parsed enum value or the supplied default
	 */
	private static <T extends Enum<T>> T parseEnum(@Nonnull Class<T> enumType,
													@Nullable String value,
													@Nonnull T defaultValue,
													@Nonnull String name) {
		String cleanValue = clean(value);
		if (cleanValue == null) {
			return defaultValue;
		}
		return parseEnumValue(enumType, cleanValue, name);
	}

	/**
	 * Parses a required route enum value after sanitisation.
	 *
	 * @param enumType enum class to parse
	 * @param value raw route value
	 * @param name route value name used in exception text
	 * @return parsed enum value
	 */
	private static <T extends Enum<T>> T parseRequiredEnum(@Nonnull Class<T> enumType,
															@Nullable String value,
															@Nonnull String name) {
		String cleanValue = clean(value);
		if (cleanValue == null) {
			throw new IllegalArgumentException(name + " is required.");
		}
		return parseEnumValue(enumType, cleanValue, name);
	}

	/**
	 * Parses a nonblank enum value.
	 *
	 * @param enumType enum class to parse
	 * @param cleanValue sanitised nonblank route value
	 * @param name route value name used in exception text
	 * @return parsed enum value
	 */
	private static <T extends Enum<T>> T parseEnumValue(@Nonnull Class<T> enumType,
															@Nonnull String cleanValue,
															@Nonnull String name) {
		try {
			return Enum.valueOf(enumType, cleanValue);
		}
		catch (IllegalArgumentException e) {
			throw new IllegalArgumentException("Invalid " + name + " value: " + cleanValue, e);
		}
	}
}
