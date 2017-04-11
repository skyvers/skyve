<%@page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@page isErrorPage="true"%>
<%@page import="java.util.Locale"%>
<%@page import="org.skyve.util.Util"%>
<%@page import="org.skyve.impl.web.UserAgent"%>
<%
	String basePath = Util.getSkyveContextUrl() + "/";
	boolean mobile = UserAgent.getType(request).isMobile();
	String referer = request.getHeader("Referer");
	Locale locale = request.getLocale();
%>
<!DOCTYPE html>
<html dir="<%=Util.isRTL(locale) ? "rtl" : "ltr"%>">
	<head>
		<meta http-equiv="X-UA-Compatible" content="IE=EDGE" />
		<title><%=Util.i18n("page.loginError.title", locale)%></title>
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
			<div class="row">
				<div class="col-3 col-2-md hidden-sm"></div>
				<div class="col-6 col-8-md col-12-sm">
					<form>
						<div class="loginTable" style="width:100%;">
							<div class="row">
								<div class="col-12 center">
									<span class="subhead"><%=Util.i18n("page.loginError.banner", locale)%></span>
								</div>
							</div>
							<div class="row">
								<div class="col-12 center">
									<% if (request.getUserPrincipal() != null) { %>
										<%=Util.i18n("page.loginError.alreadyLoggedIn", locale)%>
									<% } else { %>
										<%=Util.i18n("page.loginError.invalid", locale)%>
									<% } %>
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
