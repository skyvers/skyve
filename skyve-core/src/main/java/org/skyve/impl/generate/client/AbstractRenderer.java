package org.skyve.impl.generate.client;

import org.skyve.web.UserAgentType;

/**
 * Provides shared state for client code renderers that accumulate generated
 * source fragments.
 *
 * <p>Renderers use the configured user-agent profile to tailor generated output
 * for the active client target.
 */
public abstract class AbstractRenderer {
	/**
	 * The current user-agent profile used by renderer implementations when
	 * generating client-specific output.
	 */
	protected UserAgentType userAgentType;
	
	/**
	 * Sets the target user-agent profile for subsequent renderer output.
	 *
	 * <p>Side effects: mutates renderer state used by later render operations.
	 *
	 * @param userAgentType the resolved user-agent classification; must not be {@code null}
	 */
	public void setUserAgentType(UserAgentType userAgentType) {
		this.userAgentType = userAgentType;
	}
}
