package org.skyve.impl.web.filter.rest;

import java.io.IOException;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Rejects secured REST requests unconditionally with HTTP 403 (Forbidden).
 *
 * <p>Requests that match configured unsecured path prefixes are delegated unchanged via
 * {@link #doUnsecuredFilter(jakarta.servlet.ServletRequest, jakarta.servlet.ServletResponse, jakarta.servlet.FilterChain)}.
 *
 * <p>Side effects: writes an HTTP error response body and status when the request is not unsecured.
 *
 * <p>Threading: stateless filter implementation with no mutable instance fields.
 */
public class ForbiddenFilter extends AbstractRestFilter {
	/**
	 * Handles a single request by either bypassing configured unsecured paths or returning HTTP 403.
	 *
	 * @param request the incoming servlet request
	 * @param response the outgoing servlet response
	 * @param chain the filter chain for unsecured pass-through requests
	 * @throws IOException if writing the response fails
	 * @throws ServletException if downstream unsecured filter processing fails
	 */
	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
	throws IOException, ServletException {
		if (super.doUnsecuredFilter(request, response, chain)) {
			return;
		}
    	error(null, (HttpServletResponse) response, HttpServletResponse.SC_FORBIDDEN, realm, "Forbidden");
	}
}
