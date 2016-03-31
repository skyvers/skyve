<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ page isErrorPage="true"%>
<%@page import="java.util.Locale"%>
<%@page import="org.skyve.CORE"%>
<%@page import="org.skyve.metadata.customer.Customer"%>
<%@page import="org.skyve.util.Util"%>
<%@page import="org.skyve.wildcat.web.UserAgent"%>
<%@page import="org.skyve.wildcat.web.WebUtil"%>
<%
	String basePath = request.getScheme() + "://" + request.getServerName() + ":" + 
						request.getServerPort() + request.getContextPath() + "/";
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
		<title>WILDCAT: Session ended</title>
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
								<td style="text-align:center">
									<div style="font-size:28px;">
										Session expired!
									</div>
								</td>
							</tr>
							<tr>
								<td >
									<div style="font-size:18px;">
									Your session has expired due to inactivity.
									</div>
								</td>
							</tr>
							<tr>
								<td style="text-align: center">
									<div class="buttonDiv">
									<% if (referer == null) { %>
									<a href="<%=request.getContextPath()%><%=org.skyve.util.Util.getHomeUri()%>">Start over</a>
									<% } else { %>
										<a href="<%=referer%>">Start over</a>
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
