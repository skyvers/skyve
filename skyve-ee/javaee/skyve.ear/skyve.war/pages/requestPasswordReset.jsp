<%@page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
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
	String referer = request.getHeader("Referer");
	Principal p = request.getUserPrincipal();
	User user = WebUtil.processUserPrincipalForRequest(request, (p == null) ? null : p.getName(), true);
	Locale locale = (user == null) ? request.getLocale() : user.getLocale();

	// This is a postback, process it and move on
	String customerValue = request.getParameter("customer");
	String emailValue = request.getParameter("email");
	String oldCaptcha = (String) session.getAttribute("g-recaptcha-response");
	String newCaptcha = Util.processStringValue(request.getParameter("g-recaptcha-response"));
	boolean postback = (emailValue != null) && (newCaptcha != null) && (! newCaptcha.equals(oldCaptcha));
	if (postback) {
		session.setAttribute("g-recaptcha-response", newCaptcha);
		try {
			WebUtil.requestPasswordReset(customerValue, emailValue);
		}
		catch (Exception e) {
			// don't stop - we need to give nothing away
			UtilImpl.LOGGER.log(Level.SEVERE, 
									String.format("Password Reset Request Failed for customer=%s and email=%s", customerValue, emailValue),
									e);
		}
	}
%>
<!DOCTYPE html>
<html dir="<%=Util.isRTL(locale) ? "rtl" : "ltr"%>">
	<head>
		<meta http-equiv="X-UA-Compatible" content="IE=EDGE" />
		<title><%=Util.i18n("page.requestPasswordReset.title", locale)%></title>
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
		<style type="text/css">
			.instructions {
				max-width: 450px;
				font-size: 1.0rem;
			}
			.instructions a {
				font-size: 1.0rem;
			}
		</style>
		
		<script type="text/javascript">
			<!--
			function testMandatoryFields(form) {
				if (form.customer.value.length < 1) {
					alert('<%=Util.i18n("page.login.customer.error.required", locale)%>');
					form.customer.focus();
					return false;
				}
				else if (form.email.value.length < 1) {
					alert('<%=Util.i18n("page.requestPasswordReset.email.error.required", locale)%>');
					form.email.focus();
					return false;
				}
				else {
					form.action = '<%=basePath + "pages/requestPasswordReset.jsp"%>';
					return true;
				}
			}
			-->
		</script>
		<script src='https://www.google.com/recaptcha/api.js'></script>
	</head>
	<body style="background:white">
		<div class="container">
			<%@include file="fragments/logo.html" %>
			<div class="row">
				<div class="col-3 col-2-md hidden-sm"></div>
				<div class="col-6 col-8-md col-12-sm">
					<% if (postback) { %>
						<div class="loginTable" style="width:100%;">
							<div class="row">
								<div class="col-12 center">
									<span class="subhead"><%=Util.i18n("page.requestPasswordReset.complete.banner", locale)%></span>
								</div>
							</div>
							<div class="row">
								<div class="col-12 center">
									<span class="instructions"><%=Util.i18n("page.requestPasswordReset.complete.message", locale)%></span>
								</div>
							</div>
							<div class="row">
								<div class="col-1 col-2-sm"></div>
								<div class="col-10 col-8-sm center">
									<div class="buttonDiv">
										<a href="<%=request.getContextPath()%><%=Util.getHomeUri()%>"><%=Util.i18n("page.login.submit.label", locale)%></a>
									</div>
								</div>
							</div>
						</div>
					<% } else { %>
						<form method="post" onsubmit="return testMandatoryFields(this)">
							<div class="loginTable" style="width:100%;">
								<div class="row">
									<div class="col-12 center">
										<span class="subhead"><%=Util.i18n("page.requestPasswordReset.banner", locale)%></span>
									</div>
								</div>
								<div class="row">
									<div class="col-12">
										<div class="instructions">
											<%=Util.i18n("page.requestPasswordReset.message", locale)%>
										</div>
									</div>
								</div>
								<% if (customer == null) { %>
								<div class="row">
									<div class="col-4-sm right">
										<label for="customer"><%=Util.i18n("page.login.customer.label", locale)%></label>
									</div>
									<div class="col-7-sm">
										<input type="text" spellcheck="false" autocapitalize="none" autocomplete="off" autocorrect="none" name="customer">
									</div>
								</div>
								<% } %>
								<div class="row">
									<div class="col-4-sm right">
										<label for="email"><%=Util.i18n("page.requestPasswordReset.email.label", locale)%></label>
									</div>
									<div class="col-7-sm">
										<% if (customer != null) { %>
											<input type="hidden" name="customer" value="<%=customer%>" />
										<% } %>
										<input type="text" spellcheck="false" autocapitalize="none" autocomplete="off" autocorrect="none" name="email">
									</div>
								</div>
								<div class="row">
									<div class="col-12">
										<!-- A table to brute force the captcha to centre as it is an iframe -->
										<table>
											<tr>
												<td style="width:50%" />
												<td>
													<div class="g-recaptcha" data-sitekey="<%=UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY%>"></div>
												</td>
												<td style="width:50%" />
											</tr>
										</table>
									</div>
								</div>
								<div class="row">
									<div class="col-1 col-2-sm"></div>
									<div class="col-10 col-8-sm center">
										<input type="submit" value="<%=Util.i18n("page.requestPasswordReset.submit.label", locale)%>" />
									</div>
								</div>
								<div class="row">
									<div class="col-1 col-2-sm"></div>
									<div class="col-10 col-8-sm center" style="font-size: 11px;">
										<a href="<%=request.getContextPath()%><%=Util.getHomeUri()%><%=(user == null) ? "" : (String.format("home?customer=%s", user.getCustomerName()))%>"><%=Util.i18n("page.login.submit.label", locale)%></a>
									</div>
								</div>
							</div>
						</form>
					<% } %>
				</div>
			</div>
		</div>
	</body>
</html>
