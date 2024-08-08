package org.skyve.util;

import org.apache.commons.text.StringTokenizer;

import jakarta.servlet.http.HttpServletRequest;

public class SecurityUtil {

	/**
	 * Returns the source IP for the parsed {@link HttpServletRequest}
	 * 
	 * @param request
	 * @return source IP
	 */
	public static String getSourceIpAddress(HttpServletRequest request) {
		String xForwardedForHeader = request.getHeader("X-Forwarded-For");
		if (xForwardedForHeader == null) {
			return request.getRemoteAddr();
		}

		// As of https://en.wikipedia.org/wiki/X-Forwarded-For
		// The general format of the field is: X-Forwarded-For: client, proxy1, proxy2 ...
		// we only want the client
		return new StringTokenizer(xForwardedForHeader, ",").nextToken()
				.trim();
	}

	/**
	 * Returns the first line of the stack trace for the parsed {@link Exception}
	 * <br/>
	 * Returns null if no stack trace exists
	 * 
	 * @param e
	 * @return provenance
	 */
	public static String getProvenance(Exception e) {
		StackTraceElement[] stackTrace = e.getStackTrace();
		if (stackTrace.length > 0) {
			StackTraceElement firstElement = stackTrace[0];
			return firstElement.toString();
		}
		return null;
	}
}