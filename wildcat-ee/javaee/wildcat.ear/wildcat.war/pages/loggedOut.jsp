<%@page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@page import="java.security.Principal"%>
<%@page import="java.util.Locale"%>
<%@page import="org.skyve.metadata.user.User"%>
<%@page import="org.skyve.util.Util"%>
<%@page import="org.skyve.wildcat.web.UserAgent"%>
<%@page import="org.skyve.wildcat.web.WebUtil"%>
<%
	String basePath = request.getScheme() + "://" + request.getServerName() + ":" + request.getServerPort() + request.getContextPath() + "/";
	boolean mobile = UserAgent.getType(request).isMobile();
	String referer = request.getHeader("Referer");
	Principal p = request.getUserPrincipal();
	User user = WebUtil.processUserPrincipalForRequest(request, (p == null) ? null : p.getName(), true);
	Locale locale = (user == null) ? request.getLocale() : user.getLocale();
%>
<!DOCTYPE html>
<html dir="<%=Util.isRTL(locale) ? "rtl" : "ltr"%>">
	<head>
		<meta http-equiv="X-UA-Compatible" content="IE=EDGE" />
		<title>WILDCAT: Signed-out</title>
		<base href="<%=basePath%>" />

		<meta http-equiv="pragma" content="no-cache" />
		<meta http-equiv="cache-control" content="private,no-cache,no-store" />
		<meta http-equiv="expires" content="0" />

		<% if (mobile) { %>
			<meta name="format-detection" content="telephone=no" />
			<meta name="viewport" content="width=device-width,initial-scale=1.0,maximum-scale=1.0,minimum-scale=1.0,user-scalable=no" />
		<% } %>

		<link rel="icon" type="image/png" href="images/window/WILDCAT_fav.png" />
		<link rel="stylesheet" type="text/css" href="css/basic-min.css" />
	</head>
	<body>
		<table class="logo" align="center">
			<tr height="150px">
				<td><img src="images/WILDCAT_rev.png" alt="Get Organized" /></td>
			</tr>
		</table>
		<%
			request.getSession().invalidate();
			request.logout();
			// remove all cookies too
			Cookie[] cookies = request.getCookies();
			if (cookies != null && cookies.length > 0) {
				for (Cookie cookie : cookies) {
					cookie.setValue("-");
					cookie.setMaxAge(0);
					response.addCookie(cookie);
				}
			}
		%>
		<form>
			<table align="center">
				<tr>
					<td>
						<div class="loginTable">
							<table align="center" style="border-spacing: 10px;">
								<tr>
									<td style=";text-align:center">
										<div style="font-size:28px;">You are signed out</div>
									</td>
								</tr>
								<tr>
									<td style="text-align: center;">
										<div class="buttonDiv">
											<%
												if (referer == null) {
											%>
											<a href="<%=request.getContextPath()%><%=org.skyve.util.Util.getHomeUri()%>">Sign in</a>
											<%
												} else {
											%>
											<a href="<%=referer%>">Sign in</a>
											<%
												}
											%>
										</div>
									</td>
								</tr>
							</table>
						</div>
					</td>
				</tr>
			</table>
		</form>
	</body>
</html>
