package org.skyve.impl.web;

import java.util.Collections;
import java.util.Map;
import java.util.TreeMap;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;

import org.skyve.domain.messages.DomainException;
import org.skyve.util.Util;
import org.skyve.web.UserAgentType;

import com.blueconic.browscap.BrowsCapField;
import com.blueconic.browscap.Capabilities;
import com.blueconic.browscap.UserAgentParser;
import com.blueconic.browscap.UserAgentService;

public class UserAgent {
	/**
	 * Prevent instantiation
	 */
	private UserAgent() {
		// nothing to see here
	}
	
	private static UserAgentParser parser = null;
	static {
		try {
//			parser = new UserAgentService().loadParser(Arrays.asList(BrowsCapField.values()));
			parser = new UserAgentService().loadParser(Collections.singleton(BrowsCapField.DEVICE_TYPE));
		}
		catch (Exception e) {
			throw new DomainException("Cannot initialise Browscap.", e);
		}
	}

	private static Map<String, UserAgentType> typeCache = new TreeMap<>();

	public static UserAgentType getType(HttpServletRequest request) {
		boolean touchEnabled = false;
		
		// See if the UserAgentType is supplied as a cookie (from device.xhtml)
		Cookie[] cookies = request.getCookies();
		if (cookies != null) {
			for (int i = 0, l = cookies.length; i < l; i++) {
				Cookie cookie = cookies[i];
				String cookieName = cookie.getName();
				if ("touch".equals(cookieName)) {
					if ("1".equals(cookie.getValue())) {
						touchEnabled = true;
					}
				}
				else if ("UserAgentType".equals(cookieName)) {
					String userAgentTypeCookieValue = Util.processStringValue(cookie.getValue());
					if (userAgentTypeCookieValue != null) {
						return UserAgentType.valueOf(userAgentTypeCookieValue);
					}
				}
			}
		}
		
		// Try the User-Agent header
		String agentString = request.getHeader("User-Agent");
		if (agentString == null) {
			agentString = "";
		}

		UserAgentType result = typeCache.get(agentString);

		if (result == null) {
			result = UserAgentType.other;
			
			Capabilities capabilities = parser.parse(agentString);
			if (capabilities != null) {
				String deviceType = capabilities.getDeviceType();
				if (deviceType != null) {
					if ("Desktop".equals(deviceType)) {
						result = UserAgentType.desktop;
					}
					else if ("Tablet".equals(deviceType)) {
						result = UserAgentType.tablet;
					}
					else if (deviceType.startsWith("Mobile")) {
						result = UserAgentType.phone;
					}
				}
			}

			typeCache.put(agentString, result);
		}
		
		if ((result == UserAgentType.desktop) && touchEnabled) {
			result = UserAgentType.tablet;
		}
		
		return result;
	}
}
