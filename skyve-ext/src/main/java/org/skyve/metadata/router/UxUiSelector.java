package org.skyve.metadata.router;

import javax.servlet.http.HttpServletRequest;

import org.skyve.impl.metadata.repository.router.TaggingUxUiSelector;
import org.skyve.impl.web.UserAgentType;

public interface UxUiSelector extends TaggingUxUiSelector {
	public UxUi select(UserAgentType userAgentType, HttpServletRequest request);
}
