<%@ page language="java"%>
<%@ page isErrorPage="true"%>
<%@ page import="org.skyve.wildcat.web.UserAgent"%>
<%
	String basePath = request.getScheme() + "://" + request.getServerName() + ":" + 
						request.getServerPort() + request.getContextPath() + "/";
	boolean mobile = UserAgent.getType(request).isMobile();
	String referer = request.getHeader("Referer");
%>
<html>
	<head>
		<title>Biz Hub: Session Ended</title>
		<base href="<%=basePath%>" />

		<meta http-equiv="pragma" content="no-cache" />
		<meta http-equiv="cache-control" content="private,no-cache,no-store" />
		<meta http-equiv="expires" content="0" />

		<% if (mobile) { %>
			<meta name="format-detection" content="telephone=no" />
			<meta name="viewport" content="width=device-width,initial-scale=1.0,maximum-scale=1.0,minimum-scale=1.0,user-scalable=no" />
		<% } %>

		<link rel="icon" type="image/png" href="images/window/BizHub16.png" />
		<link rel="stylesheet" type="text/css" href="css/basic-min.css" />
	</head>
	<body>
		<table class="logo" align="center">
			<tr height="150px">
				<td>
					<img src="images/bizhub_logo4.jpg" alt="Get Organized" />
				</td>
			</tr>
		</table>
		<table align="center">
			<tr>
				<td>
					<div class="x-box" style="width:100%;">
						<div class="x-box-tl">
							<div class="x-box-tr">
								<div class="x-box-tc">
								</div>
							</div>
						</div>
						<div class="x-box-ml">
							<div class="x-box-mr">
								<div class="x-box-mc">
									<table align="center">
										<tr>
											<td>
												<strong style="color:FireBrick;">
													Session Expired
												</strong>
											</td>
										</tr>
									</table>
									<table class="borderTable" width="300px" align="center">
										<tr align="center">
											<td style="color:FireBrick;font-size:14" >
												Your session has expired due to inactivity.
											</td>
										</tr>
									</table>
									<br>
									<table align="center">
										<tr>
											<td background="images/blankbutton2.jpg" height=35 width=70 align=center>
												<% if (referer == null) { %>
												<a href="<%=request.getContextPath()%><%=org.skyve.util.Util.getHomeUri()%>">Start over</a>
												<% } else { %>
													<a href="<%=referer%>">Start over</a>
												<% } %>
											</td>
										</tr>
									</table>
									<div></div>
								</div>
							</div>
						</div>
						<div class="x-box-bl">
							<div class="x-box-br">
								<div class="x-box-bc">
								</div>
							</div>
						</div>
					</div>
				</td>
			</tr>
		</table>
	</body>
</html>
