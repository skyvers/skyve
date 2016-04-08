package org.skyve.wildcat.web;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

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
		
		String customerName = request.getParameter(AbstractWebContext.CUSTOMER_COOKIE_NAME);
		Cookie cookie = new Cookie(AbstractWebContext.CUSTOMER_COOKIE_NAME, 
									(customerName == null) ? "" : customerName);
		cookie.setPath("/");
		if (customerName == null) {
			cookie.setMaxAge(0); // remove the cookie
		}
		response.addCookie(cookie);

		response.sendRedirect(response.encodeRedirectURL(Util.getHomeUrl()));
	}
}
