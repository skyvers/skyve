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
	private FilterConfig fc;

	@Override
	public void init(FilterConfig config) throws ServletException {
		fc = config;
	}

	@Override
	public void destroy() {
		fc = null;
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
	throws IOException, ServletException {
		HttpServletResponse httpResponse = (HttpServletResponse) response;

		// set the provided HTTP response parameters
		for (Enumeration<?> e = fc.getInitParameterNames(); e.hasMoreElements();) {
			String headerName = (String) e.nextElement();
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

		// pass the request/response on
		chain.doFilter(request, httpResponse);
	}
}
