<%@ page language="java"%>
<%@ page isErrorPage="true"%>
<%@ page import="org.skyve.wildcat.web.UserAgent"%>
<%
	String basePath = request.getScheme() + "://" + request.getServerName() + ":" + 
						request.getServerPort() + request.getContextPath() + "/";
	boolean mobile = UserAgent.getType(request).isMobile();
	String referer = request.getHeader("Referer");
%>
<!DOCTYPE html>
<html>
	<head>
		<meta http-equiv="X-UA-Compatible" content="IE=EDGE" />
		<title>WILDCAT: Sign-in Error</title>
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
				<td>
					<img src="images/WILDCAT_rev.png" alt="WILDCAT" />
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
										Sign in error!
									</div>
								</td>
							</tr>
							<tr align="center">
								<td style="font-size:18px" >
									<%if (request.getUserPrincipal() != null) {
									%>
									You are currently signed in as <em><%=request.getUserPrincipal()%></em>
									<br />
									<br />
									To access this functionality you need to sign in as another user with the correct permissions
									<br />
									<br />
									<%} else {
									%>
										Invalid username and/or password
									<%}
									%>
								</td>
							</tr>
							<tr>
								<td style="text-align: center;">
									<div class="buttonDiv">
									<% if (referer == null) { %>
									<a href="<%=request.getContextPath()%><%=org.skyve.util.Util.getHomeUri()%>">Try again</a>
									<% } else { %>
										<a href="<%=referer%>">Try Again</a>
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
