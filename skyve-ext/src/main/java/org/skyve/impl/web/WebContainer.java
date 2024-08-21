package org.skyve.impl.web;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

public class WebContainer {

	private static final ThreadLocal<HttpServletRequestResponse> requestResponse = new ThreadLocal<>();

	public static void setHttpServletRequestResponse(HttpServletRequest request, HttpServletResponse response) {
		requestResponse.set(new HttpServletRequestResponse(request, response));
	}

	public static HttpServletRequestResponse getHttpServletRequestResponse() {
		return requestResponse.get();
	}

	public static void clear() {
		requestResponse.remove();
	}
}