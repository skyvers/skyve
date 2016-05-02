<%@page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@page isErrorPage="true"%>
<%@page import="java.util.Locale"%>
<%@page import="org.skyve.util.Util"%>
<%@page import="org.skyve.wildcat.web.UserAgent"%>
<%
	String basePath = Util.getWildcatContextUrl() + "/";
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
		<link rel="stylesheet" type="text/css" href="css/basic-min.css" />
	</head>
	<body>
		<table class="logo" align="center">
			<tr height="150px">
				<td>
					<img src="images/skyve_inv.png" alt="Skyve" />
				</td>
			</tr>
		</table>
		<table align="center">
			<tr>
				<td>
					<div class="loginTable">
						<table style="text-align: center; border-spacing: 10px;">
							<tr>
								<td>
									<div style="font-size:28px;">
										<%=Util.i18n("page.loginError.banner", locale)%>
									</div>
								</td>
							</tr>
							<tr align="center">
								<td style="font-size:18px" >
									<% if (request.getUserPrincipal() != null) { %>
										<%=Util.i18n("page.loginError.alreadyLoggedIn", locale)%>
									<% } else { %>
										<%=Util.i18n("page.loginError.invalid", locale)%>
									<% } %>
								</td>
							</tr>
							<tr>
								<td style="text-align: center;">
									<div class="buttonDiv">
									<% if (referer == null) { %>
									<a href="<%=request.getContextPath()%><%=org.skyve.util.Util.getHomeUri()%>"><%=Util.i18n("page.loginError.retry", locale)%></a>
									<% } else { %>
										<a href="<%=referer%>"><%=Util.i18n("page.loginError.retry", locale)%></a>
									<% } %>
									</div>
								</td>
							</tr>
						</table>
					</div>
				</td>
			</tr>
		</table>
	</body>
</html>
