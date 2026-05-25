package org.skyve.impl.generate.client;

import org.skyve.web.UserAgentType;

/**
 * Base class for client code renderers that accumulate generated source
 * fragments.
 *
 * <p>Provides shared helper methods used by component and layout renderers.
 */
public abstract class AbstractRenderer {
	protected UserAgentType userAgentType;
	
	public void setUserAgentType(UserAgentType userAgentType) {
		this.userAgentType = userAgentType;
	}
}
