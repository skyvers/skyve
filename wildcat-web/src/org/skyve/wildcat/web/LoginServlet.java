package org.skyve.wildcat.web;

import java.io.IOException;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;
import org.skyve.wildcat.metadata.repository.AbstractRepository;

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

	private static final String LOGIN_PATH = "/login";
	private static final String LOGGED_OUT_PATH = "/loggedOut";
	private static final String LOGIN_ERROR_PATH = "/loginError";
	private static final String SMART_CLIENT_JAVASCRIPT_LOGIN = "/smartClientJavascriptLogin";
	
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
			AbstractRepository repository = AbstractRepository.get();

			if (LOGIN_PATH.equals(servletPath)) {
				String url = "/pages/login.jsp";
				if (customerName != null) {
					Customer customer = repository.getCustomer(customerName);
					String value = customer.getLoginResources().getLoginPageURL();
					if (value != null) {
						url = value;
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
					String value = customer.getLoginResources().getLoggedOutPageURL();
					if (value != null) {
						url = value;
					}
				}

				// forward to jsp
				RequestDispatcher rd = request.getRequestDispatcher(url);
				rd.forward(request, response);
			}
			else if (LOGIN_ERROR_PATH.equals(servletPath)) {
				String url = "/pages/loginError.jsp";
				if (customerName != null) {
					Customer customer = repository.getCustomer(customerName);
					String value = customer.getLoginResources().getLoginErrorPageURL();
					if (value != null) {
						url = value;
					}
				}

				// forward to jsp
				RequestDispatcher rd = request.getRequestDispatcher(url);
				rd.forward(request, response);
			}
			else if (SMART_CLIENT_JAVASCRIPT_LOGIN.equals(servletPath)) {
				String url = "/desktop/reloginFlow.js";
				if (customerName != null) {
					Customer customer = repository.getCustomer(customerName);
					String value = customer.getLoginResources().getSmartClientJavascriptURL();
					if (value != null) {
						url = value;
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
