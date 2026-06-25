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

/**
 * Filters inbound or outbound requests before downstream web processing.
 */
public class ResponseHeaderFilter implements Filter {
	public static final String SECURITY_HEADERS_FILTER_NAME = "SecurityHeadersFilter";
	
	private FilterConfig fc;

	/**
	 * Captures filter configuration used to apply response headers for this filter mapping.
	 *
	 * @param config filter configuration containing response header settings
	 * @throws ServletException when filter initialization fails
	 */
	@Override
	public void init(FilterConfig config) throws ServletException {
		fc = config;
	}

	/**
	 * Clears cached filter configuration references on shutdown.
	 */
	@Override
	public void destroy() {
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
			else if ("Content-Security-Policy".equals(headerName)) {
				String csp = fc.getInitParameter(headerName);
				if (! Util.isSecureUrl()) {
					csp = csp.replace("upgrade-insecure-requests;", "");
				}
				httpResponse.setHeader(headerName, csp);
			}
			else {
				httpResponse.setHeader(headerName, fc.getInitParameter(headerName));
			}
		}
	}
}
