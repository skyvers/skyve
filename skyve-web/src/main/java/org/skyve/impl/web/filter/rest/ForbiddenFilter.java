package org.skyve.impl.web.filter.rest;

import java.io.IOException;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletResponse;

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
