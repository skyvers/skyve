<%@page session="false" isErrorPage="true" language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@page import="java.util.Locale"%>
<%@page import="org.skyve.CORE"%>
<%@page import="org.skyve.metadata.customer.Customer"%>
<%@page import="org.skyve.util.Util"%>
<%@page import="org.skyve.impl.web.UserAgent"%>
<%@page import="org.skyve.impl.web.WebUtil"%>
<%
	String basePath = Util.getSkyveContextUrl() + "/";
	boolean mobile = UserAgent.getType(request).isMobile();
	String referer = WebUtil.getRefererHeader(request);
	
	// Determine the locale
	String customer = WebUtil.determineCustomerWithoutSession(request);
	Locale locale = request.getLocale();
	if (customer != null) {
		try {
			Customer c = CORE.getRepository().getCustomer(customer);
			if (c != null) {
				String languageTag = c.getLanguageTag();
				if (languageTag != null) {
					locale = Locale.forLanguageTag(languageTag);
				}
			}
		}
		catch (Exception e) {
			// cannot get locale - do nothing
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
		<title><%=Util.i18n("page.expired.title", locale)%></title>
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
		    	<%@include file="fragments/noscript.html" %>
		    	
		        <div class="ui large form">
		            <div class="ui segment">
		            	<div class="ui header">
		            		<%=Util.i18n("page.expired.banner", locale)%>
		            	</div>
		            	<div class="field">
		            		<%=Util.i18n("page.expired.explanation", locale)%>
		            	</div>
		            	
						<% if ((referer == null) || referer.contains("/login") || referer.contains("/pages/")) { // no referer or came from the login or other jsp page %>
							<a href="<%=Util.getBaseUrl()%>" class="ui fluid large blue submit button"><%=Util.i18n("page.loginError.retry", locale)%></a>
						<% } else { %>
							<a href="<%=referer%>" class="ui fluid large blue submit button"><%=Util.i18n("page.loginError.retry", locale)%></a>
						<% } %>
		            </div>
		        </div>
		    </div>
		</div>
	</body>
</html>
