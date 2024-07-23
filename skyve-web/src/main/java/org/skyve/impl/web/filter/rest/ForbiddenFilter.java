package org.skyve.impl.web.filter.rest;

import java.io.IOException;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletResponse;

public class ForbiddenFilter extends AbstractRestFilter {
	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
	throws IOException, ServletException {
		if (super.doUnsecuredFilter(request, response, chain)) {
			return;
		}
    	error(null, (HttpServletResponse) response, HttpServletResponse.SC_FORBIDDEN, realm, "Forbidden");
	}
}
