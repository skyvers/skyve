package router;

import java.util.Map;
import java.util.TreeMap;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.UserAgentType;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.router.UxUiSelector;

public class DefaultUxUiSelector implements UxUiSelector {
	public static final UxUi PHONE = new UxUi("phone", "nova-light");
	public static final UxUi TABLET = new UxUi("tablet", "nova-light");
	public static final UxUi DESKTOP = new UxUi("desktop", "casablanca");
	public static final UxUi EXTERNAL = new UxUi("external", "nova-light");
	private static final Map<String, UxUi> uxuis = new TreeMap<>();
	static {
		uxuis.put(PHONE.getName(), PHONE);
		uxuis.put(TABLET.getName(), TABLET);
		uxuis.put(DESKTOP.getName(), DESKTOP);
		uxuis.put(EXTERNAL.getName(), EXTERNAL);
	}
	
	@Override
	public UxUi select(UserAgentType userAgentType, HttpServletRequest request) {
		HttpSession session = request.getSession(false);
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
			case desktop:
				return DESKTOP;
			default:
				return EXTERNAL;
		}
	}
}
