<%@ page session="false" language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ page import="org.skyve.impl.web.AbstractWebContext"%>
<%@ page import="org.skyve.util.Util"%>
<%@ page import="org.skyve.web.UserAgentType"%>
<%@ page import="org.slf4j.LoggerFactory"%>
<%@ page import="org.slf4j.Logger"%>

<%! static final Logger logger = LoggerFactory.getLogger("org.skyve.jsp.device"); %>

<%
	UserAgentType type = UserAgentType.valueOf(request.getParameter("ua"));
	request.setAttribute(AbstractWebContext.USER_AGENT_TYPE_KEY, type);
	request.setAttribute(AbstractWebContext.EMULATED_USER_AGENT_TYPE_KEY, Boolean.TRUE);
	
	String queryString = request.getQueryString();
	String outcomeUrl = "/home.jsp";
	if (queryString != null) {
		outcomeUrl += '?' + queryString;
	}

	logger.info("Server-side forward to " + outcomeUrl);
	request.getRequestDispatcher(outcomeUrl).forward(request, response);
%>
