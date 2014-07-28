package router;

import javax.servlet.http.HttpServletRequest;

import org.skyve.wildcat.web.UserAgent;
import org.skyve.wildcat.web.UserAgent.UserAgentType;
import org.skyve.metadata.router.UxUiSelector;

public class DefaultUxUiSelector implements UxUiSelector {
	@Override
	public String select(HttpServletRequest request) {
		UserAgentType type = UserAgent.getType(request);
		switch (type) {
			case phone:
				return "phone";
			case tablet:
				return "tablet";
			case desktop:
				return "desktop";
			default:
				return "public";
		}
	}
}
