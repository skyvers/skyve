package org.skyve.impl.web;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.util.Util;

/**
 * This servlet is not secured by the web container.
 * If there is no security principal then it forwards to the login page,
 * which is potentially customized per customer.
 * If there is a security principal already then it forwards to Util.HOME_URI.
 * 
 * @author mike
 */
public class HomeServlet extends HttpServlet {
	/**
	 * For Serialization.
	 */
	private static final long serialVersionUID = 1L;

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		processRequest(request, response);
	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		processRequest(request, response);
	}
	
	private static void processRequest(HttpServletRequest request, HttpServletResponse response)
	throws IOException {
		response.setContentType(MimeType.html.toString());
		response.setCharacterEncoding(Util.UTF8);
		
		// Stop cookie/request header injection by checking the customer name
		String customerName = request.getParameter(AbstractWebContext.CUSTOMER_COOKIE_NAME);
		if (customerName != null) {
			// This will throw if the customerName value ain't a customer name
			try {
				CORE.getRepository().getCustomer(customerName);
			}
			catch (@SuppressWarnings("unused") Exception e) {
				customerName = null;
			}
		}

		Cookie cookie = new Cookie(AbstractWebContext.CUSTOMER_COOKIE_NAME, 
									(customerName == null) ? "" : customerName);
		cookie.setPath("/");
		cookie.setHttpOnly(true);
		cookie.setSecure(Util.isSecureUrl());
		if (customerName == null) {
			cookie.setMaxAge(0); // remove the cookie
		}
		response.addCookie(cookie);

		response.sendRedirect(response.encodeRedirectURL(Util.getHomeUrl()));
	}
}
