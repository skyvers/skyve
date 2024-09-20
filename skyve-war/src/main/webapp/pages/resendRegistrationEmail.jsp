<%@page session="false" language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@page import="java.sql.ResultSet"%>
<%@page import="java.sql.PreparedStatement"%>
<%@page import="java.sql.Connection"%>
<%@page import="java.util.logging.Level"%>
<%@page import="org.skyve.EXT"%>
<%@page import="java.security.Principal"%>
<%@page import="java.util.Locale"%>
<%@page import="java.util.UUID"%>
<%@page import="org.skyve.impl.util.UtilImpl"%>
<%@page import="org.skyve.impl.web.UserAgent"%>
<%@page import="org.skyve.impl.web.WebUtil"%>
<%@page import="org.skyve.metadata.user.User"%>
<%@page import="org.skyve.util.Util"%>
<%
	String basePath = Util.getSkyveContextUrl() + "/";
	String customer = WebUtil.determineCustomerWithoutSession(request);
	boolean mobile = UserAgent.getType(request).isMobile();
	Principal p = request.getUserPrincipal();
	User user = WebUtil.processUserPrincipalForRequest(request, (p == null) ? null : p.getName());
	Locale locale = (user == null) ? request.getLocale() : user.getLocale();

	// This is a postback, process it and move on
	String customerValue = request.getParameter("customer");
	String userIdValue = request.getParameter("userId");
	
	boolean postback = (userIdValue != null);
	if (postback) {
		try {
			WebUtil.sendRegistrationEmail(userIdValue);
		}
		catch (Exception e) {
			// don't stop - we need to give nothing away
			UtilImpl.LOGGER.log(Level.SEVERE, 
									String.format("Send Registration Email failed for customer=%s and userId=%s", customerValue, userIdValue),
									e);
		}
	}
%>
<!DOCTYPE html>
<html dir="<%=Util.isRTL(locale) ? "rtl" : "ltr"%>">
	<head>
		<!-- Standard Meta -->
	    <meta charset="utf-8" />
	    <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1" />
		<meta http-equiv="refresh" content="300; url=<%=basePath%>loggedOut" />
		<meta http-equiv="pragma" content="no-cache" />
		<meta http-equiv="cache-control" content="private,no-cache,no-store" />
		<meta http-equiv="expires" content="0" />
	    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0">
	    
	    <!-- Site Properties -->
		<title><%=Util.i18n("page.resendRegistrationEmail.title", locale)%></title>
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
		            	<% if (postback) { %>
			            	<div class="ui header">
			            		<%=Util.i18n("page.resendRegistrationEmail.banner", locale)%>
			            	</div>
						<% } else { %>
							<div class="ui red message">
			            		<div class="ui header">
			            			<%=Util.i18n("page.error.banner", locale)%>
			            		</div>
			            		<div class="field">
			            			<%=Util.i18n("page.error.explanation", locale)%>
			            		</div>
			            	</div>
		            	<% } %>
	                	<% if (UtilImpl.CUSTOMER == null) { %>
		            		<a href="<%=Util.getBaseUrl()%><%=(user == null) ? "" : ("?customer=" + user.getCustomerName())%>" class="ui fluid large blue submit button"><%=Util.i18n("page.login.submit.label", locale)%></a>
		                <% } else { %>
		            		<a href="<%=Util.getBaseUrl()%>" class="ui fluid large blue submit button"><%=Util.i18n("page.login.submit.label", locale)%></a>
		                <% } %>
		            </div>
		        </div>
		    </div>
		</div>
	</body>
</html>
