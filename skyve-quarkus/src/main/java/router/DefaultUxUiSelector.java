package router;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.web.UserAgentType;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.router.UxUiSelector;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;
import modules.admin.domain.Startup;

public class DefaultUxUiSelector implements UxUiSelector {
	public static final String DISMISS_STARTUP = "DISMISS_STARTUP";

	private static final Map<String, UxUi> uxuis = new TreeMap<>();
	static {
		uxuis.put(UxUis.PHONE.getName(), UxUis.PHONE);
		uxuis.put(UxUis.TABLET.getName(), UxUis.TABLET);
		uxuis.put(UxUis.DESKTOP.getName(), UxUis.DESKTOP);
		uxuis.put(UxUis.EXTERNAL.getName(), UxUis.EXTERNAL);
		uxuis.put(UxUis.STARTUP.getName(), UxUis.STARTUP);
	}

	@Override
	public UxUi select(UserAgentType userAgentType, HttpServletRequest request) {
		// public pages are destined for external UX/UI always
		if (request.getUserPrincipal() == null) {
			return UxUis.EXTERNAL;
		}

		HttpSession session = request.getSession(false);

		// check if this is the first login
		User user = (session == null) ? null : (User) session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
		if (user != null && user.isInRole(Startup.MODULE_NAME, "SecurityAdministrator") && UtilImpl.SHOW_SETUP) {
			// check the user has not already dismissed the startup page this session
			Object dismissed = session != null ? session.getAttribute(DISMISS_STARTUP) : null;
			if (! Boolean.TRUE.equals(dismissed)) {
				Util.LOGGER.info("ROUTING TO STARTUP");
				return UxUis.STARTUP;
			}
		}

		String uxuiName = (session != null) ? (String) session.getAttribute(AbstractWebContext.UXUI) : null;
		if (uxuiName != null) {
			UxUi uxui = uxuis.get(uxuiName);
			if (uxui != null) {
				return uxui;
			}
		}

		switch (userAgentType) {
			case phone:
				return UxUis.PHONE;
			case tablet:
				return UxUis.TABLET;
			// uncomment to use Smart Client by default for desktop user agents
			//case desktop:
			//return UxUis.DESKTOP;
			default:
				return UxUis.EXTERNAL;
		}
	}
	
	@Override
	public UxUi emulate(UserAgentType userAgentType, HttpServletRequest request) {
		switch (userAgentType) {
			case phone:
				return UxUis.PHONE;
			case tablet:
				return UxUis.TABLET;
			case desktop:
				return UxUis.DESKTOP;
			default:
				return UxUis.EXTERNAL;
		}
	}
}
