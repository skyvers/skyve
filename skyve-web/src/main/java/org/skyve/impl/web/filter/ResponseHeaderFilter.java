package org.skyve.impl.web.filter;

import java.io.IOException;
import java.util.Enumeration;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletResponse;

import org.skyve.util.Util;

public class ResponseHeaderFilter implements Filter {
	FilterConfig fc;

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
				httpResponse.addDateHeader("Expires", System.currentTimeMillis() + Long.parseLong(fc.getInitParameter(headerName)));
			}
			// only apply HSTS header if defined and we are using TLS
			else if ("Strict-Transport-Security".equals(headerName)) {
				if (Util.isSecureUrl()) {
					httpResponse.addHeader(headerName, fc.getInitParameter(headerName));
				}
			}
			else {
				httpResponse.addHeader(headerName, fc.getInitParameter(headerName));
			}
		}

		// pass the request/response on
		chain.doFilter(request, httpResponse);
	}
}
