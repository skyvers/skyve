package org.skyve.impl.web.filter;

import java.io.IOException;
import java.util.Enumeration;

import org.skyve.util.Util;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletResponse;

public class ResponseHeaderFilter implements Filter {
	public static final String SECURITY_HEADERS_FILTER_NAME = "SecurityHeadersFilter";
	private static FilterConfig SECURITY_HEADERS_FILTER_CONFIG = null;
	
	private FilterConfig fc;

	@Override
	public void init(FilterConfig config) throws ServletException {
		if (SECURITY_HEADERS_FILTER_NAME.equals(config.getFilterName())) {
			SECURITY_HEADERS_FILTER_CONFIG = config;
		}
		fc = config;
	}

	@Override
	public void destroy() {
		if (SECURITY_HEADERS_FILTER_NAME.equals(fc.getFilterName())) {
			SECURITY_HEADERS_FILTER_CONFIG = null;
		}
		fc = null;
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
	throws IOException, ServletException {
		applyHeaders(fc, (HttpServletResponse) response);

		// pass the request/response on
		chain.doFilter(request, response);
	}
	
	// set the provided HTTP response parameters
	private static void applyHeaders(FilterConfig fc, HttpServletResponse httpResponse) {
		for (Enumeration<String> e = fc.getInitParameterNames(); e.hasMoreElements();) {
			String headerName = e.nextElement();
			if ("Expires".equals(headerName)) {
				httpResponse.setDateHeader(headerName, System.currentTimeMillis() + Long.parseLong(fc.getInitParameter(headerName)));
			}
			// only apply HSTS header if defined and we are using TLS
			else if ("Strict-Transport-Security".equals(headerName)) {
				if (Util.isSecureUrl()) {
					httpResponse.setHeader(headerName, fc.getInitParameter(headerName));
				}
			}
			else {
				httpResponse.setHeader(headerName, fc.getInitParameter(headerName));
			}
		}
	}
	
	/**
	 * Used in error.jsp to apply security headers as web container error processing does not pass through the web app's filters.
	 * @param httpResponse	The response to apply the headers to.
	 */
	public static void applySecurityHeaders(HttpServletResponse httpResponse) {
		applyHeaders(SECURITY_HEADERS_FILTER_CONFIG, httpResponse);
	}
}
