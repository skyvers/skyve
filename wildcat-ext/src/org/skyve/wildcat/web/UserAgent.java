package org.skyve.wildcat.web;

import java.util.Map;
import java.util.TreeMap;

import javax.servlet.http.HttpServletRequest;

import net.sf.uadetector.ReadableUserAgent;
import net.sf.uadetector.UserAgentStringParser;
import net.sf.uadetector.service.UADetectorServiceFactory;

public class UserAgent {
	/**
	 * Prevent instantiation
	 */
	private UserAgent() {
		// nothing to see here
	}
	
	public static enum UserAgentType {
		phone(true), tablet(true), desktop(false), other(false);
		
		private boolean mobile;
		private UserAgentType(boolean mobile) {
			this.mobile = mobile;
		}
		
		public boolean isMobile() {
			return mobile;
		}
	}

	private static Map<String, UserAgentType> cache = new TreeMap<>();

	public static UserAgentType getType(HttpServletRequest request) {
		String agentString = request.getHeader("User-Agent");
		if (agentString == null) {
			agentString = "";
		}

		UserAgentType result = cache.get(agentString);
		if (result == null) {
			UserAgentStringParser parser = UADetectorServiceFactory.getResourceModuleParser();
			ReadableUserAgent agent = parser.parse(agentString);
			
			switch (agent.getDeviceCategory().getCategory()) {
				case PERSONAL_COMPUTER:
					result = UserAgentType.desktop;
					break;
				case TABLET:
					result = UserAgentType.tablet;
					break;
				case SMARTPHONE:
				case WEARABLE_COMPUTER:
					result = UserAgentType.phone;
					break;
				default:
					result = UserAgentType.other;
			}

			// Manually detect galaxy tabs - they are not phones
			if ( UserAgentType.phone.equals(result) && agentString.contains("SCH-I800")) {
				result = UserAgentType.tablet;
			}

			cache.put(agentString, result);
		}
		
		return result;
	}
}
