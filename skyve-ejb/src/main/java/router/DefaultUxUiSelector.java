package router;

import java.util.Map;
import java.util.TreeMap;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.web.UserAgentType;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.router.UxUiSelector;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.domain.Startup;

import static router.UxUis.*;

public class DefaultUxUiSelector implements UxUiSelector {
	public static final String DISMISS_STARTUP = "DISMISS_STARTUP";

	private static final Map<String, UxUi> uxuis = new TreeMap<>();
	static {
		uxuis.put(PHONE.getName(), PHONE);
		uxuis.put(TABLET.getName(), TABLET);
		uxuis.put(DESKTOP.getName(), DESKTOP);
		uxuis.put(EXTERNAL.getName(), EXTERNAL);
		uxuis.put(STARTUP.getName(), STARTUP);
	}

	@Override
	public UxUi select(UserAgentType userAgentType, HttpServletRequest request) {
		// public pages are destined for external UX/UI always
		if (request.getUserPrincipal() == null) {
			return EXTERNAL;
		}

		HttpSession session = request.getSession(false);

		// check if this is the first login
		User user = (session == null) ? null : (User) session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
		if (user != null && user.isInRole(Startup.MODULE_NAME, "SecurityAdministrator") && UtilImpl.SHOW_SETUP) {
			// check the user has not already dismissed the startup page this session
			Object dismissed = session != null ? session.getAttribute(DISMISS_STARTUP) : null;
			if (! Boolean.TRUE.equals(dismissed)) {
				Util.LOGGER.info("ROUTING TO STARTUP");
				return STARTUP;
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
				return PHONE;
			case tablet:
				return TABLET;
			// uncomment to use Smart Client by default for desktop user agents
			//case desktop:
			//return DESKTOP;
			default:
				return EXTERNAL;
		}
	}
	
	@Override
	public UxUi emulate(UserAgentType userAgentType, HttpServletRequest request) {
		switch (userAgentType) {
			case phone:
				return PHONE;
			case tablet:
				return TABLET;
			case desktop:
				return DESKTOP;
			default:
				return EXTERNAL;
		}
	}
}
