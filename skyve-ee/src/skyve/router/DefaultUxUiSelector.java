package router;

import javax.servlet.http.HttpServletRequest;

import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.UserAgent.UserAgentType;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.router.UxUiSelector;

public class DefaultUxUiSelector implements UxUiSelector {
	private static final UxUi PHONE = new UxUi("phone", "casablanca");
	private static final UxUi TABLET = new UxUi("tablet", "omega");
	private static final UxUi DESKTOP = new UxUi("desktop", "omega");
	private static final UxUi PUBLIC = new UxUi("public", "casablanca");
	
	@Override
	public UxUi select(HttpServletRequest request) {
		UserAgentType type = UserAgent.getType(request);
		switch (type) {
			case phone:
				return PHONE;
			case tablet:
				return TABLET;
			case desktop:
				return DESKTOP;
			default:
				return PUBLIC;
		}
	}
}
