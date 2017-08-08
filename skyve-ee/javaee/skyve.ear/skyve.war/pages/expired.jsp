<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ page isErrorPage="true"%>
<%@page import="java.util.Locale"%>
<%@page import="org.skyve.CORE"%>
<%@page import="org.skyve.metadata.customer.Customer"%>
<%@page import="org.skyve.util.Util"%>
<%@page import="org.skyve.impl.web.UserAgent"%>
<%@page import="org.skyve.impl.web.WebUtil"%>
<%
	String basePath = Util.getSkyveContextUrl() + "/";
	boolean mobile = UserAgent.getType(request).isMobile();
	String referer = request.getHeader("Referer");
	
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
		<meta http-equiv="X-UA-Compatible" content="IE=EDGE" />
		<title><%=Util.i18n("page.expired.title", locale)%></title>
		<base href="<%=basePath%>" />

		<meta http-equiv="pragma" content="no-cache" />
		<meta http-equiv="cache-control" content="private,no-cache,no-store" />
		<meta http-equiv="expires" content="0" />

		<% if (mobile) { %>
			<meta name="format-detection" content="telephone=no" />
			<meta name="viewport" content="width=device-width,initial-scale=1.0,maximum-scale=1.0,minimum-scale=1.0,user-scalable=no" />
		<% } %>

		<link rel="icon" type="image/png" href="images/window/skyve_fav.png" />
		<link rel="apple-touch-icon" href="images/window/skyve_fav.png">
		<link rel="stylesheet" type="text/css" href="css/basic-min.css" />
		<link rel="stylesheet" type="text/css" href="css/simple-grid-min.css" />
	</head>
	<body>
		<div class="container">
			<%@include file="fragments/logo.html" %>
			<%@include file="fragments/noscript.html" %>
			<div class="row">
				<div class="col-3 col-2-md hidden-sm"></div>
				<div class="col-6 col-8-md col-12-sm">
					<form name="changeForm" method="post" onsubmit="return testMandatoryFields(this)">
						<div class="loginTable" style="width:100%;">
							<div class="row">
								<div class="col-12 center">
									<span class="subhead"><%=Util.i18n("page.expired.banner", locale)%></span>
								</div>
							</div>
							<div class="row">
								<div class="col-12 center">
									<%=Util.i18n("page.expired.explanation", locale)%>
								</div>
							</div>
							<div class="row">
								<div class="col-1 col-2-sm"></div>
								<div class="col-10 col-8-sm center">
									<div class="buttonDiv">
									<% if (referer == null) { %>
										<a href="<%=request.getContextPath()%><%=org.skyve.util.Util.getHomeUri()%>"><%=Util.i18n("page.loginError.retry", locale)%></a>
									<% } else { %>
										<a href="<%=referer%>"><%=Util.i18n("page.loginError.retry", locale)%></a>
									<% } %>
									</div>
								</div>
							</div>
						</div>
					</form>
				</div>
			</div>
		</div>
	</body>
</html>
