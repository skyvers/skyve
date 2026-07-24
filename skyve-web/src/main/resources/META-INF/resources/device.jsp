<%@ page session="false" language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ page import="jakarta.servlet.http.HttpServletResponse"%>
<%@ page import="org.skyve.impl.web.AbstractWebContext"%>
<%@ page import="org.skyve.impl.web.UserAgent"%>
<%@ page import="org.skyve.impl.web.WebUtil"%>
<%
	try {
		UserAgent.consumeDevicePreviewCommand(request);
	}
	catch (Exception e) {
		response.sendError(HttpServletResponse.SC_BAD_REQUEST, e.getMessage());
		return;
	}

	StringBuilder targetUrl = new StringBuilder(request.getContextPath()).append('/');
	WebUtil.appendRequestParameters(targetUrl, request, AbstractWebContext.EMULATED_USER_AGENT_TYPE_PARAMETER);

	response.setHeader("Cache-Control", "no-store");
	response.setStatus(HttpServletResponse.SC_SEE_OTHER);
	response.setHeader("Location", response.encodeRedirectURL(targetUrl.toString()));
%>
