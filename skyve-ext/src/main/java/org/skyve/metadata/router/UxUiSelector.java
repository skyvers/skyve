package org.skyve.metadata.router;

import org.skyve.impl.metadata.repository.router.TaggingUxUiSelector;
import org.skyve.metadata.MetaDataException;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nonnull;
import jakarta.servlet.http.HttpServletRequest;

/**
 * Selects the UX/UI theme (layout set) for a given HTTP request and device type.
 *
 * <p>Skyve supports multiple UX/UI renderers (e.g. {@code external}, {@code mobile},
 * {@code desktop}) that map to different Faces template directories and view file sets.
 * An application provides a single {@code UxUiSelector} implementation (typically via
 * a customer override) to determine which renderer to use for each request.
 * Skyve invokes emulation directly when requested. For normal requests it evaluates router
 * direct metadata before invoking request-dependent selector policy.
 *
 * <p>Two selection modes are supported:
 * <ul>
 *   <li>{@link #select} — normal operation: the device type is detected from the
 *       real {@code User-Agent} header.</li>
 *   <li>{@link #emulate} — preview mode: the device type is simulated (e.g. from a
 *       query parameter). The default implementation delegates to {@link #select}.</li>
 * </ul>
 *
 * @see org.skyve.web.UserAgentType
 * @see UxUi
 */
public interface UxUiSelector extends TaggingUxUiSelector {
	/**
	 * Selects the UX/UI renderer to use for a normal (non-preview) request based on
	 * the detected user-agent type and the HTTP request context.
	 *
	 * @param userAgentType the device type inferred from the real {@code User-Agent} header;
	 *                      never {@code null}
	 * @param request       the current HTTP servlet request; never {@code null}
	 * @return the selected UX/UI descriptor; never {@code null}
	 */
	public @Nonnull UxUi select(@Nonnull UserAgentType userAgentType, @Nonnull HttpServletRequest request);

	/**
	 * Resolves a trusted configured metadata name to its application-owned UX/UI object.
	 *
	 * <p>This operation is request-independent and must preserve the exact identity of the
	 * configured object. Implementations are shared and must therefore make lookup thread-safe.
	 * The default fails fast so selectors remain source-compatible when their router declares no
	 * direct declarations.
	 *
	 * @param name trusted configured metadata name; must not be {@code null}
	 * @return the configured application-owned object; never {@code null}
	 * @throws MetaDataException if the name is unknown or this selector does not support named
	 *                           resolution
	 * @since 10.0
	 */
	default @Nonnull UxUi resolve(@Nonnull String name) {
		throw new MetaDataException("UX/UI selector cannot resolve trusted metadata name " + name + '.');
	}

	/**
	 * Selects the UX/UI renderer to use when the device type is being emulated (e.g. in
	 * preview or developer mode).
	 *
	 * <p>The default implementation delegates to {@link #select}, effectively treating
	 * emulation the same as normal selection. Override to apply preview-specific logic.
	 *
	 * @param userAgentType the simulated device type; never {@code null}
	 * @param request       the current HTTP servlet request; never {@code null}
	 * @return the selected UX/UI descriptor; never {@code null}
	 */
	default @Nonnull UxUi emulate(@Nonnull UserAgentType userAgentType, @Nonnull HttpServletRequest request) {
		return select(userAgentType, request);
	}
}
