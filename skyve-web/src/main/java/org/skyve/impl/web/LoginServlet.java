package org.skyve.impl.web;

import java.io.IOException;

import org.skyve.CORE;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;

import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

/**
 * This servlet is not secured by the web container.
 * If there is no security principal then it forwards to the login page,
 * which is potentially customized per customer.
 * If there is a security principal already then it forwards to Util.HOME_URI.
 * 
 * @author mike
 */
public class LoginServlet extends HttpServlet {
	/**
	 * For Serialization.
	 */
	private static final long serialVersionUID = 1L;

	public static final String LOGIN_PATH = "/login";
	public static final String LOGGED_OUT_PATH = "/loggedOut";
	
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
	throws ServletException, IOException {
		String customerName = null;
		HttpSession session = request.getSession(false);
		if (session != null) {
			User user = (User) session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
			if (user != null) {
				try {
					customerName = user.getCustomer().getName();
				}
				catch (MetaDataException e) {
					throw new ServletException("Could not obtain the customer name", e);
				}
			}
		}

		if (customerName == null) { // no-one is logged in
			customerName = WebUtil.determineCustomerWithoutSession(request);
		}

		String servletPath = request.getServletPath();
		try {
			Repository repository = CORE.getRepository();

			if (LOGIN_PATH.equals(servletPath)) {
				String url = "/pages/login.jsp";
				if (customerName != null) {
					Customer customer = repository.getCustomer(customerName);
					if (customer != null) {
						String value = customer.getLoginResources().getLoginPageURL();
						if (value != null) {
							url = value;
						}
					}
				}

				// forward to jsp
				RequestDispatcher rd = request.getRequestDispatcher(url);
				rd.forward(request, response);
			}
			else if (LOGGED_OUT_PATH.equals(servletPath)) {
				String url = "/pages/loggedOut.jsp";
				if (customerName != null) {
					Customer customer = repository.getCustomer(customerName);
					if (customer != null) {
						String value = customer.getLoginResources().getLoggedOutPageURL();
						if (value != null) {
							url = value;
						}
					}
				}

				// forward to jsp
				RequestDispatcher rd = request.getRequestDispatcher(url);
				rd.forward(request, response);
			}
		}
		catch (MetaDataException e) {
			throw new ServletException("Could not obtain the customer for name " + customerName, e);
		}
	}
}
