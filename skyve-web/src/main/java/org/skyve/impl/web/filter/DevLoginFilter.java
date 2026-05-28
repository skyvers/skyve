package org.skyve.impl.web.filter;

import java.io.IOException;
import java.security.Principal;

import org.skyve.util.Util;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;

/**
 * Used to login automatically using the init parameters when developing web 2 (vue) clients with node etc.
 */
public class DevLoginFilter implements Filter {
	private static final String CUSTOMER_NAME = "customer";
	private static final String USER_NAME = "user";
	private static final String PASSWORD = "password";
	
	private String user;
	private String userPassword;
	
	/**
	 * Reads required development login credentials from filter init parameters.
	 *
	 * @param config filter configuration containing customer, user, and password parameters
	 * @throws ServletException when any required parameter is missing
	 */
	@Override
	public void init(FilterConfig config) throws ServletException {
		String customerName = Util.processStringValue(config.getInitParameter(CUSTOMER_NAME));
		if (customerName == null) {
			throw new ServletException(CUSTOMER_NAME + " init parameter is required");
		}
		String userName = Util.processStringValue(config.getInitParameter(USER_NAME));
		if (userName == null) {
			throw new ServletException(USER_NAME + " init parameter is required");
		}
		user = customerName + '/' + userName;
		userPassword = Util.processStringValue(config.getInitParameter(PASSWORD));
		if (userPassword == null) {
			throw new ServletException(PASSWORD + " init parameter is required");
		}
	}

	/**
	 * Clears cached development credentials when the filter is destroyed.
	 */
	@Override
	public void destroy() {
		user = null;
		userPassword = null;
	}
	
	/**
	 * Performs transparent login for unauthenticated requests using configured development credentials.
	 *
	 * @param request inbound servlet request
	 * @param response outbound servlet response
	 * @param chain downstream filter chain
	 * @throws IOException when request processing fails due to I/O issues
	 * @throws ServletException when authentication or downstream processing fails
	 */
	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
	throws IOException, ServletException {
		HttpServletRequest hsr = (HttpServletRequest) request;
		Principal userPrincipal = hsr.getUserPrincipal();
		if (userPrincipal == null) {
			hsr.login(user, userPassword);
		}
		chain.doFilter(request, response);
	}
}
