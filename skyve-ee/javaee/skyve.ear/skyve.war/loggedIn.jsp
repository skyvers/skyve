<%@ page language="java"%>
<%@ page import="org.skyve.util.Util"%>

<%
	String queryString = request.getQueryString();
	String outcomeUrl = "/home.jsp";
	if (queryString != null) {
		outcomeUrl += '?' + queryString;
	}
	Util.LOGGER.info("Server-side forward to " + outcomeUrl);
	request.getRequestDispatcher(outcomeUrl).forward(request, response);
%>
