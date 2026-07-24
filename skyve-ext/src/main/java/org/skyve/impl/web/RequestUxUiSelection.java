package org.skyve.impl.web;

import java.util.Objects;

import org.skyve.metadata.router.UxUi;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Captures the progressively resolved device and UX/UI selection for one HTTP request.
 *
 * <p>The effective device type and emulation flag are fixed at construction. Skyve can retain that
 * partial selection after device detection, then assign the selected {@link UxUi} exactly once
 * when request path, principal, and session state are ready for UX/UI resolution. Instances are
 * request-confined values. The selected {@code UxUi} is retained by reference because configured
 * UX/UI instances are application-owned configuration objects.
 *
 * @since 10.0
 */
public final class RequestUxUiSelection {
	private final @Nonnull UserAgentType userAgentType;
	private final boolean emulated;
	private @Nullable UxUi uxui;

	/**
	 * Creates a partial request selection containing effective device detection.
	 *
	 * @param userAgentType effective device type; must not be {@code null}
	 * @param emulated whether the device type was supplied through emulation
	 */
	RequestUxUiSelection(@Nonnull UserAgentType userAgentType, boolean emulated) {
		this.userAgentType = Objects.requireNonNull(userAgentType);
		this.emulated = emulated;
	}

	/**
	 * Creates a complete request selection.
	 *
	 * @param userAgentType effective device type; must not be {@code null}
	 * @param emulated whether the device type was supplied through emulation
	 * @param uxui selected application UX/UI; must not be {@code null}
	 */
	public RequestUxUiSelection(@Nonnull UserAgentType userAgentType, boolean emulated, @Nonnull UxUi uxui) {
		this(userAgentType, emulated);
		complete(uxui);
	}

	/**
	 * Returns the effective device type.
	 *
	 * @return the selected type; never {@code null}
	 */
	public @Nonnull UserAgentType getUserAgentType() {
		return userAgentType;
	}

	/**
	 * Indicates whether the device type came from the emulation parameter.
	 *
	 * @return {@code true} when emulation is active
	 */
	public boolean isEmulated() {
		return emulated;
	}

	/**
	 * Returns the selected application UX/UI.
	 *
	 * @return the selected configuration object; never {@code null}
	 * @throws IllegalStateException if internal request processing has not completed this selection
	 */
	public @Nonnull UxUi getUxUi() {
		UxUi result = uxui;
		if (result == null) {
			throw new IllegalStateException("Request UX/UI selection is not complete");
		}
		return result;
	}

	/**
	 * Indicates whether UX/UI resolution has completed.
	 *
	 * @return {@code true} when the selected UX/UI has been assigned
	 */
	boolean isComplete() {
		return uxui != null;
	}

	/**
	 * Assigns the selected UX/UI exactly once.
	 *
	 * @param selectedUxUi selected application UX/UI; must not be {@code null}
	 * @throws IllegalStateException if this selection is already complete
	 */
	void complete(@Nonnull UxUi selectedUxUi) {
		uxui = selectedUxUi;
	}
}
