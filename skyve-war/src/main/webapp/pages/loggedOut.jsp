<%@page session="false" language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@page import="java.security.Principal"%>
<%@page import="java.util.Locale"%>
<%@page import="org.skyve.domain.messages.SkyveException"%>
<%@page import="org.skyve.impl.util.UtilImpl"%>						
<%@page import="org.skyve.impl.web.UserAgent"%>
<%@page import="org.skyve.impl.web.WebUtil"%>
<%@page import="org.skyve.metadata.user.User"%>
<%@page import="org.skyve.util.Util"%>
<%@page import="org.skyve.web.WebContext"%>
<%
	String basePath = Util.getSkyveContextUrl() + "/";
	boolean mobile = UserAgent.getType(request).isMobile();
	String referer = WebUtil.getRefererHeader(request);
	User user = null;
	HttpSession session = request.getSession(false); // don't make a new one
	if (session != null) {
		user = (User) session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
	}
	Locale locale = (user == null) ? request.getLocale() : user.getLocale();
%>
<!DOCTYPE html>
<html dir="<%=Util.isRTL(locale) ? "rtl" : "ltr"%>">
	<head>
		<!-- Standard Meta -->
	    <meta charset="utf-8" />
	    <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1" />
		<meta http-equiv="pragma" content="no-cache" />
		<meta http-equiv="cache-control" content="private,no-cache,no-store" />
		<meta http-equiv="expires" content="0" />
	    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0">

		<!-- Site Properties -->	    
		<title><%=Util.i18n("page.logout.title", locale)%></title>
		<base href="<%=basePath%>" />

		<% if (mobile) { %>
			<meta name="format-detection" content="telephone=no" />
			<meta name="format-detection" content="email=no">
		<% } %>

		<%@include file="fragments/favicon.html" %>
		<link rel="stylesheet" href="semantic24/semantic.min.css">
		
		<%@include file="fragments/styles.html" %>
		<%@include file="fragments/backgroundImage.html" %>
	</head>
	<body>
		<%
			WebUtil.logout(request, response);
		%>
		
		<div class="ui middle aligned center aligned grid">
		    <div class="column">
		    	<div style="text-align: center; margin: 0 auto; margin-bottom: 10px;">
		    		<%@include file="fragments/logo.html" %>
		    	</div>
		    	
		        <div class="ui large form">
		            <div class="ui segment">
		            	<div class="ui header">
		            		<%=Util.i18n("page.logout.banner", locale)%>
		            	</div>
						<% if ((referer == null) || referer.contains("/login") || referer.contains("/pages/")) { // no referer or came from the login or other jsp page %>
							<% if (UtilImpl.CUSTOMER == null) { %>
								<a href="<%=Util.getBaseUrl()%><%=(user == null) ? "" : ("?customer=" + user.getCustomerName())%>" class="ui fluid large blue submit button"><%=Util.i18n("page.login.submit.label", locale)%></a>
							<% } else { %>
								<a href="<%=Util.getBaseUrl()%>" class="ui fluid large blue submit button"><%=Util.i18n("page.login.submit.label", locale)%></a>
							<% } %>
						<% } else { %>
							<a href="<%=referer%>" class="ui fluid large blue submit button"><%=Util.i18n("page.login.banner", locale)%></a>
						<% } %>
		            </div>
		        </div>
		    </div>
		</div>
	</body>
</html>
