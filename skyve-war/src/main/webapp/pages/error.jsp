<%@page session="false" isErrorPage="true" language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@page import="java.security.Principal"%>
<%@page import="java.util.Locale"%>
<%@page import="org.skyve.domain.messages.SkyveException"%>
<%@page import="org.skyve.metadata.user.User"%>
<%@page import="org.skyve.util.Util"%>
<%@page import="org.skyve.impl.web.UserAgent"%>
<%@page import="org.skyve.impl.web.WebUtil"%>
<%@page import="org.skyve.impl.web.filter.ResponseHeaderFilter"%>
<%
	// The web container error processing does not pass the error page the web app's filters 
	ResponseHeaderFilter.applySecurityHeaders(response);
	
	String basePath = Util.getSkyveContextUrl() + "/";
	boolean mobile = UserAgent.getType(request).isMobile();
	Principal p = request.getUserPrincipal();
	User user = null;
	try {
		user = WebUtil.processUserPrincipalForRequest(request, (p == null) ? null : p.getName());
	}
	catch (SkyveException e) {
		// The principal name is not a user in the database - continue on...
	}
	Locale locale = (user == null) ? request.getLocale() : user.getLocale();
	if (locale == null) {
		locale = Locale.ENGLISH;
	}
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
		<title><%=Util.i18n("page.error.title", locale)%></title>
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
		<div class="ui middle aligned center aligned grid">
		    <div class="column">
		    	<div style="text-align: center; margin: 0 auto; margin-bottom: 10px;">
		    		<%@include file="fragments/logo.html" %>
		    	</div>
		    	
		        <div class="ui large form">
		            <div class="ui segment">
		            	<div class="ui red message">
		            		<div class="ui header">
		            			<%=Util.i18n("page.error.banner", locale)%>
		            		</div>
		            		<div class="field">
		            			<%=Util.i18n("page.error.explanation", locale)%>
		            		</div>
		            	</div>
						<div class="field">
							<a href="<%=Util.getBaseUrl()%>" class="ui fluid large blue submit button"><%=Util.i18n("page.loginError.retry", locale)%></a>
						</div>
						<div class="field">
							<a href="mailto:<%=org.skyve.util.Util.getSupportEmailAddress()%>?subject=Exception Report&body=<%=(exception == null) ? Util.i18n("page.error.noMessage", locale) : exception.getLocalizedMessage()%> for <%=(request.getUserPrincipal() != null) ? request.getUserPrincipal().getName() : Util.i18n("page.error.notLoggedIn", locale)%> at <%=new java.util.Date()%>"
									class="ui fluid large blue basic button">
								<i class="envelope icon"></i><%=Util.i18n("page.loginError.report", locale)%>
							</a>
					 	</div>
		            </div>
		        </div>
		    </div>
		</div>
	</body>
</html>
