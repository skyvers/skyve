package org.skyve.impl.web;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Holds thread-local HttpServletRequestResponses.
 * This class is called from EXT.
 */
public class WebContainer {

	private static final ThreadLocal<HttpServletRequestResponse> requestResponse = new ThreadLocal<>();

	public static void setHttpServletRequestResponse(@Nonnull HttpServletRequest request, @Nonnull HttpServletResponse response) {
		requestResponse.set(new HttpServletRequestResponse(request, response));
	}

	public static @Nullable HttpServletRequestResponse getHttpServletRequestResponse() {
		return requestResponse.get();
	}

	public static void clear() {
		requestResponse.remove();
	}
}