<%@ page session="false" language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ page import="java.security.Principal"%>
<%@ page import="java.util.Enumeration"%>
<%@ page import="jakarta.servlet.http.Cookie"%>
<%@ page import="org.skyve.metadata.customer.Customer"%>
<%@ page import="org.skyve.metadata.user.User"%>
<%@ page import="org.skyve.metadata.repository.ProvidedRepository"%>
<%@ page import="org.skyve.metadata.router.UxUi"%>
<%@ page import="org.skyve.metadata.router.UxUiSelector"%>
<%@ page import="org.skyve.metadata.view.View.ViewType"%>
<%@ page import="org.skyve.util.Util"%>
<%@ page import="org.skyve.web.WebAction"%>
<%@ page import="org.skyve.web.WebContext"%>
<%@ page import="org.skyve.impl.metadata.repository.ProvidedRepositoryFactory"%>
<%@ page import="org.skyve.impl.metadata.repository.router.Router"%>
<%@ page import="org.skyve.impl.metadata.repository.router.RouteCriteria"%>
<%@ page import="org.skyve.impl.persistence.AbstractPersistence"%>
<%@ page import="org.skyve.impl.web.AbstractWebContext"%>
<%@ page import="org.skyve.impl.web.UserAgent"%>
<%@ page import="org.skyve.web.UserAgentType"%>
<%@ page import="org.skyve.impl.web.WebUtil"%>
<%@ page import="org.skyve.impl.util.UtilImpl"%>

<%
	// Stop cookie/request header injection by checking the valid customer name
	ProvidedRepository repository = ProvidedRepositoryFactory.get();
	String customerName = request.getParameter(AbstractWebContext.CUSTOMER_COOKIE_NAME);
	if (customerName != null) {
		// This will throw or return null if the customerName value ain't a customer name
		try {
			Customer customer = repository.getCustomer(customerName);
			if (customer == null) {
				customerName = null;
			}
		}
		catch (Exception e) {
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

	String a = request.getParameter("a"); // action
	WebAction webAction = null;
	if (a != null) {
		webAction = WebAction.valueOf(a);
	}
	String m = request.getParameter("m"); // module
	String d = request.getParameter("d"); // document
	String q = request.getParameter("q"); // query
	String i = request.getParameter("i"); // id
	String c = request.getParameter("c"); // customer
	String b = request.getParameter("b"); // binding

	// Get (and set) the user agent type (if required - could have been set by device.jsp)
	UserAgentType userAgentType = UserAgent.getType(request);

	Router router = repository.getRouter();

	RouteCriteria criteria = new RouteCriteria();
	criteria.setDocumentName(d);
	criteria.setModuleName(m);
	criteria.setQueryName(q);
	criteria.setWebAction(webAction);
	if (WebAction.e.equals(webAction)) { // editing
		criteria.setViewType((i == null) ? ViewType.create : ViewType.edit);
	}
	
	// If we make it here without a principal, then we should either continue to a public unsecured page or login.
	String userName = null;
	Principal userPrincipal = request.getUserPrincipal();
	boolean canonicalised = false;
	if (userPrincipal == null) {
		customerName = (UtilImpl.CUSTOMER == null) ? c : UtilImpl.CUSTOMER;
		criteria.setCustomerName(customerName);
		try {
			criteria.canonicalise(null, b);
			canonicalised = true;
		}
		catch (Exception e) {
			throw new IllegalStateException("Malformed URL cannot be canonicalised", e);
		}
		
		// Determine the UX/UI without a user principal
		UxUi uxui = UserAgent.getUxUi(request);
		String uxuiName = (uxui == null) ? "" : uxui.getName();
		// Now determine if the outcome URL is unsecured or not.
		String outcomeUrl = router.selectOutcomeUrl(uxuiName, criteria);
		if (outcomeUrl == null) {
			UtilImpl.LOGGER.severe("The route criteria " + criteria + " for uxui " + uxuiName + " did not produce an outcome URL");
			throw new ServletException("The route criteria " + criteria + " for uxui " + uxuiName + " did not produce an outcome URL");
		}
		if (router.isUnsecured(outcomeUrl)) {
			if (customerName == null) {
				throw new IllegalStateException("Malformed URL - this URL must have a 'c' parameter");
			}
			else {
				// This will throw or return null if the customerName value ain't a valid customer name
				try {
					Customer customer = repository.getCustomer(customerName);
					if (customer == null) {
						throw new IllegalStateException("Malformed URL - this URL must have a 'c' parameter with a valid customer");
					}
				}
				catch (Exception e) {
					throw new IllegalStateException("Malformed URL - this URL must have a 'c' parameter with a valid customer", e);
				}
			}
			userName = repository.retrievePublicUserName(customerName);
			if (userName == null) {
				response.sendRedirect(response.encodeRedirectURL(Util.getBaseUrl() + "pages/noPublicUser.jsp"));
				return;
			}
			userName = customerName + "/" + userName;
		}
		else {
			String queryString = request.getQueryString();
			if (queryString == null) {
				response.sendRedirect(response.encodeRedirectURL(Util.getLoginUrl()));
			}
			else {
				// Remove the customer parameter before redirecting as its now a cookie
				StringBuilder sb = new StringBuilder(64);
				Enumeration<String> en = request.getParameterNames();
				while (en.hasMoreElements()) {
					String name = en.nextElement();
					if (! AbstractWebContext.CUSTOMER_COOKIE_NAME.equals(name)) {
						sb.append((sb.length() == 0) ? '?' : '&');
						sb.append(name).append('=');
						sb.append(request.getParameter(name));
					}
				}
				response.sendRedirect(response.encodeRedirectURL(Util.getBaseUrl() + "loggedIn.jsp" + sb.toString()));
			}
			return;
		}
	}
	else {
		userName = userPrincipal.getName();
	}
	
	if (userName != null) { // we have a logged in user or at least 1 to assert
		// Get the user
		HttpSession session = request.getSession(false);
		User user = (session == null) ? null : (User) session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
		if (user == null) { // if the user is not established yet (but we've logged in or have a public user...)
			AbstractPersistence persistence = AbstractPersistence.get();
			try {
				persistence.begin();
				user = WebUtil.processUserPrincipalForRequest(request, userName);
			}
			finally {
				if (persistence != null) {
					persistence.commit(true);
				}
			}
		}

		if ((user != null) && user.isPasswordChangeRequired()) {
			response.sendRedirect(response.encodeRedirectURL(Util.getBaseUrl() + "pages/changePassword.jsp"));
			return;
		}
		
		// Set the extra criterion if user is defined
		if (user != null) {
			criteria.setCustomerName(user.getCustomerName());
			criteria.setDataGroupId(user.getDataGroupId());
			criteria.setUserId(user.getId());
		}

		if (! canonicalised) {
			try {
				criteria.canonicalise((user == null) ? null : user.getCustomer(), b);
			}
			catch (Exception e) {
				throw new IllegalStateException("Malformed URL cannot be canonicalised", e);
			}
		}
		
		// Determine the UX/UI with the user principal
		UxUi uxui = UserAgent.getUxUi(request);
		String uxuiName = (uxui == null) ? "" : uxui.getName();
		// Determine the route
		String outcomeUrl = router.selectOutcomeUrl(uxuiName, criteria);
		if (UtilImpl.COMMAND_TRACE) {
			UtilImpl.LOGGER.info(String.format("home.jsp - Route uxui=%s,c=%s,dg=%s,d=%s,m=%s,q=%s,a=%s to %s",
													uxuiName,
													criteria.getCustomerName(),
													criteria.getDataGroupId(),
													criteria.getDocumentName(),
													criteria.getModuleName(),
													criteria.getQueryName(),
													a,
													outcomeUrl));
		}
		if (outcomeUrl == null) {
			UtilImpl.LOGGER.severe("The route criteria " + criteria + " for uxui " + uxuiName + " did not produce an outcome URL");
			throw new ServletException("The route criteria " + criteria + " for uxui " + uxuiName + " did not produce an outcome URL");
		}
			
		// forward
		request.getRequestDispatcher(outcomeUrl).forward(request, response);
	}
%>
