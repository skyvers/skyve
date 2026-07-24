package router;

import java.util.Map;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.router.UxUiSelector;
import org.skyve.metadata.user.User;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.skyve.web.UserAgentType;
import org.skyve.web.WebContext;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;
import modules.admin.domain.Startup;

/**
 * Selects the effective UX/UI profile for incoming web requests.
 */
public class DefaultUxUiSelector implements UxUiSelector {
	private static final long serialVersionUID = -1248035215927799615L;

	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(DefaultUxUiSelector.class);

	/**
	 * Session key that records whether the startup wizard has been dismissed.
	 */
	public static final String DISMISS_STARTUP = "DISMISS_STARTUP";

	private static final Map<String, UxUi> UXUIS = Map.of(
			UxUis.PHONE.getName(), UxUis.PHONE,
			UxUis.TABLET.getName(), UxUis.TABLET,
			UxUis.DESKTOP.getName(), UxUis.DESKTOP,
			UxUis.EXTERNAL.getName(), UxUis.EXTERNAL,
			UxUis.STARTUP.getName(), UxUis.STARTUP);

	/**
	 * Resolves a router-owned UX/UI name to the configured reference object.
	 *
	 * @param name trusted router metadata name; must not be {@code null}
	 * @return the exact configured UX/UI object; never {@code null}
	 * @throws MetaDataException if the configured registry has no such name
	 */
	@Override
	public @Nonnull UxUi resolve(@Nonnull String name) {
		UxUi result = UXUIS.get(name);
		if (result == null) {
			throw new MetaDataException("Unknown configured UX/UI name " + name + '.');
		}
		return result;
	}

	/**
	 * Selects the UX/UI profile for the current request.
	 *
	 * @param userAgentType the detected user-agent category
	 * @param request the current servlet request
	 * @return the selected UX/UI profile
	 */
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
			@SuppressWarnings("null") // session cannot be null here as user is not null
			Object dismissed = session.getAttribute(DISMISS_STARTUP);
			if (! Boolean.TRUE.equals(dismissed)) {
				LOGGER.info("ROUTING TO STARTUP");
				return UxUis.STARTUP;
			}
		}

		String uxuiName = (session != null) ? (String) session.getAttribute(AbstractWebContext.UXUI_SESSION_ATTRIBUTE_NAME) : null;
		if (uxuiName != null) {
			UxUi uxui = UXUIS.get(uxuiName);
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

	/**
	 * Selects the emulated UX/UI profile for preview and testing scenarios.
	 *
	 * @param userAgentType the requested user-agent category
	 * @param request the current servlet request
	 * @return the emulated UX/UI profile
	 */
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
