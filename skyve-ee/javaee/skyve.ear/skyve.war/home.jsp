<%@ page language="java"%>
<%@ page import="java.security.Principal"%>
<%@ page import="javax.servlet.http.Cookie"%>
<%@ page import="org.skyve.CORE"%>
<%@ page import="org.skyve.metadata.user.User"%>
<%@ page import="org.skyve.metadata.repository.Repository"%>
<%@ page import="org.skyve.metadata.router.UxUi"%>
<%@ page import="org.skyve.metadata.router.UxUiSelector"%>
<%@ page import="org.skyve.metadata.view.View.ViewType"%>
<%@ page import="org.skyve.util.Util"%>
<%@ page import="org.skyve.web.WebAction"%>
<%@ page import="org.skyve.web.WebContext"%>
<%@ page import="org.skyve.impl.metadata.repository.router.Router"%>
<%@ page import="org.skyve.impl.metadata.repository.router.RouteCriteria"%>
<%@ page import="org.skyve.impl.persistence.AbstractPersistence"%>
<%@ page import="org.skyve.impl.web.AbstractWebContext"%>
<%@ page import="org.skyve.impl.web.UserAgent"%>
<%@ page import="org.skyve.impl.web.UserAgent.UserAgentType"%>
<%@ page import="org.skyve.impl.web.WebUtil"%>
<%@ page import="org.skyve.impl.web.faces.FacesUtil"%>

<%
	// Stop cookie/request header injection by checking the customer name
	Repository repository = CORE.getRepository();
	String customerName = request.getParameter(AbstractWebContext.CUSTOMER_COOKIE_NAME);
	if (customerName != null) {
		// This will throw if the customerName value ain't a customer name
		try {
			repository.getCustomer(customerName);
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
	String m = request.getParameter("m"); // module
	String d = request.getParameter("d"); // document
	String q = request.getParameter("q"); // query
	String i = request.getParameter("i"); // id

	// NB Some app servers can detect if a welcome URL is secured and will forward to the login form.
	// Some do not, if we make it here without a principal, then we should redirect directly to home.jsp
	Principal userPrincipal = request.getUserPrincipal();
	if (userPrincipal == null) { // some welcome files send redirect, some server-side forward
		StringBuilder sb = new StringBuilder(64);
		sb.append(Util.getHomeUrl()).append("home.jsp");
		if ((m != null) && (d != null)) {
			sb.append("?m=").append(m);
			sb.append("&d=").append(d);
			if (a != null) {
				sb.append("&a").append(a);
			}
			if (q != null) {
				sb.append("&q").append(q);
			}
			if (i != null) {
				sb.append("&i=").append(i);
			}
		}
		response.sendRedirect(response.encodeRedirectURL(sb.toString()));
	}
	else if (customerName != null) {
		response.sendRedirect(response.encodeRedirectURL(Util.getHomeUrl()));
	}
	else {
		// Get the user
		User user = (User) request.getSession().getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
		if (user == null) { // if the user is not established yet (but we've logged in...)
			AbstractPersistence persistence = AbstractPersistence.get();
			try {
				persistence.begin();
				user = WebUtil.processUserPrincipalForRequest(request, userPrincipal.getName(), true);
			}
			finally {
				if (persistence != null) {
					persistence.commit(true);
				}
			}
		}

		if ((user != null) && user.isPasswordChangeRequired()) {
			response.sendRedirect(response.encodeRedirectURL(Util.getHomeUrl() + "pages/changePassword.jsp"));
		}
		
		// Set the UX/UI and user agent type
		UserAgentType userAgentType = UserAgent.getType(request);
		request.setAttribute(FacesUtil.USER_AGENT_TYPE_KEY, userAgentType);
		Router router = repository.getRouter();
		UxUi uxui = ((UxUiSelector) router.getUxuiSelector()).select(userAgentType, request);
		request.setAttribute(FacesUtil.UX_UI_KEY, uxui);
		
		// Determine the route
		RouteCriteria criteria = new RouteCriteria();
		WebAction webAction = null;
		criteria.setCustomerName(user.getCustomerName());
		criteria.setDataGroupId(user.getDataGroupId());
		criteria.setDocumentName(d);
		criteria.setModuleName(m);
		criteria.setQueryName(q);
		criteria.setUserId(user.getId());
		if (a != null) {
			webAction = WebAction.valueOf(a);
			criteria.setWebAction(webAction);
		}
		if (WebAction.e.equals(webAction)) { // editing
			criteria.setViewType((i == null) ? ViewType.create : ViewType.edit);
		}
		String outcomeUrl = router.selectOutcomeUrl(uxui.getName(), criteria);
		if (outcomeUrl == null) {
			throw new ServletException("The route criteria " + criteria + " for uxui " + uxui + " did not produce an outcome URL");
		}
			
		// forward
		request.getRequestDispatcher(outcomeUrl).forward(request, response);
	}
%>
