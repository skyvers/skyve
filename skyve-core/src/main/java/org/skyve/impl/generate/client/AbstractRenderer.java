package org.skyve.impl.generate.client;

import org.skyve.web.UserAgentType;

public abstract class AbstractRenderer {
	protected UserAgentType userAgentType;
	
	public void setUserAgentType(UserAgentType userAgentType) {
		this.userAgentType = userAgentType;
	}
}
