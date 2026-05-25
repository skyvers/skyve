package org.skyve.metadata.router;

import org.skyve.impl.metadata.repository.router.TaggingUxUiSelector;
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
	 * Called to determine what UxUi to return when the user agent / device type is detected / determined (not previewing but normal system operation)
	 * 
	 * @param userAgentType
	 * @param request
	 * @return
	 */
	public @Nonnull UxUi select(@Nonnull UserAgentType userAgentType, @Nonnull HttpServletRequest request);
	
	/**
	 * Called to determine what UxUi to return when the user agent / device type is emulated (in preview mode)
	 * 
	 * @param userAgentType
	 * @param request
	 * @return
	 */
	default public @Nonnull UxUi emulate(@Nonnull UserAgentType userAgentType, @Nonnull HttpServletRequest request) {
		return select(userAgentType, request);
	}
}
