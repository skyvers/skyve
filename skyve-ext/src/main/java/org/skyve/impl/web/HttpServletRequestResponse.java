package org.skyve.impl.web;

import jakarta.annotation.Nonnull;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Holds the HTTP Request and Response for the current thread.
 * This can be obtained via EXT.getHttpServletRequest() and EXT.getHttpServletResponse().
 */
public class HttpServletRequestResponse {

	@Nonnull private HttpServletRequest request;
	@Nonnull private HttpServletResponse response;

	HttpServletRequestResponse(@Nonnull HttpServletRequest request, @Nonnull HttpServletResponse response) {
		this.request = request;
		this.response = response;
	}

	/**
	 * Get the HTTP Request for the current thread.
	 * @return	The request.
	 */
	public @Nonnull HttpServletRequest getRequest() {
		return request;
	}

	/**
	 * Get the HTTP Response for the current thread.
	 * @return	The request.
	 */
	public @Nonnull HttpServletResponse getResponse() {
		return response;
	}
}
