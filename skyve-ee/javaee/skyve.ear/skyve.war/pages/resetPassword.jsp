<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>

<%@ page import="java.security.Principal"%>
<%@ page import="java.util.Locale"%>

<%@ page import="org.skyve.impl.util.UtilImpl"%>
<%@ page import="org.skyve.impl.web.UserAgent"%>
<%@ page import="org.skyve.impl.web.WebUtil"%>
<%@ page import="org.skyve.metadata.user.User"%>
<%@ page import="org.skyve.util.Util"%>
<%@ page import="org.skyve.web.WebContext"%>
<%
	final String newPasswordFieldName = "newPassword";
	final String confirmPasswordFieldName = "confirmPassword";
	
	String basePath = Util.getSkyveContextUrl() + "/";
	boolean mobile = UserAgent.getType(request).isMobile();
	Locale locale = request.getLocale();

	String passwordChangeErrorMessage = null;
	String newPasswordValue = request.getParameter(newPasswordFieldName);
	String confirmPasswordValue = request.getParameter(confirmPasswordFieldName);
	String passwordResetToken = request.getParameter("t");

	if (passwordResetToken == null) {
		passwordChangeErrorMessage = Util.i18n("page.resetPassword.link.error", locale);
	}
	// This is a postback, process it and move on
	else if ((newPasswordValue != null) && (confirmPasswordValue != null)) {
		passwordChangeErrorMessage = WebUtil.resetPassword(passwordResetToken, newPasswordValue, confirmPasswordValue);
		if (passwordChangeErrorMessage == null) {
			response.sendRedirect(response.encodeRedirectURL(Util.getHomeUrl() + "home.jsp"));
			return;
		}
	}
%>
<!DOCTYPE html>
<html dir="<%=Util.isRTL(locale) ? "rtl" : "ltr"%>">
	<head>
		<meta http-equiv="X-UA-Compatible" content="IE=EDGE" />
		<title><%=Util.i18n("page.resetPassword.title", locale)%></title>
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
		<script type="text/javascript">
			<!--
			function testMandatoryFields(form) {
				var newPasswordValue = form.<%=newPasswordFieldName%>.value;
				var confirmPasswordValue = form.<%=confirmPasswordFieldName%>.value;
				if (newPasswordValue.length < 1) {
					alert('<%=Util.i18n("page.changePassword.newPassword.error.required", locale)%>');
					form.<%=newPasswordFieldName%>.focus();
					return false;
				}
				else if (confirmPasswordValue.length < 1) {
					alert('<%=Util.i18n("page.changePassword.confirmPassword.error.required", locale)%>');
					form.<%=confirmPasswordFieldName%>.focus();
					return false;
				}
				else if (newPasswordValue != confirmPasswordValue) {
					alert('<%=Util.i18n("page.changePassword.noPasswordMatch.error.required", locale)%>');
					form.<%=confirmPasswordFieldName%>.focus();
					return false;
				}
			}
			-->
		</script>
	</head>
	<% if (passwordChangeErrorMessage != null) { %>
	<body style="background:white" onload="document.forms['changeForm'].elements['<%=newPasswordFieldName%>'].focus();alert('<%=passwordChangeErrorMessage%>');">
	<% } else { %>
	<body style="background:white" onload="document.forms['changeForm'].elements['<%=newPasswordFieldName%>'].focus()">
	<% } %>
		<div class="container">
			<%@include file="fragments/logo.html" %>
			<%@include file="fragments/noscript.html" %>
			<div class="row">
				<div class="col-3 col-2-md hidden-sm"></div>
				<div class="col-6 col-8-md col-12-sm">
					<form name="changeForm" method="post" onsubmit="return testMandatoryFields(this)">
						<input type="hidden" name="t" value="<%=passwordResetToken%>">
						<div class="loginTable" style="width:100%;">
							<div class="row">
								<div class="col-12 center">
									<span class="subhead"><%=Util.i18n("page.changePassword.message", locale)%></span>
								</div>
							</div>
							<div class="row">
								<div class="col-4-sm right">
									<label for="newPassword"><%=Util.i18n("page.changePassword.newPassword.label", locale)%></label>
								</div>
								<div class="col-6-sm">
									<input type="password" spellcheck="false" autocapitalize="none" autocomplete="off" autocorrect="none" name="<%=newPasswordFieldName%>">
								</div>
							</div>
							<div class="row">
								<div class="col-4-sm right">
									<label for="confirmPassword"><%=Util.i18n("page.changePassword.confirmPassword.label", locale)%></label>
								</div>
								<div class="col-6-sm">
									<input type="password" spellcheck="false" autocapitalize="none" autocomplete="off" autocorrect="none" name="<%=confirmPasswordFieldName%>">
								</div>
							</div>
							<div class="row">
								<div class="col-1 col-2-sm"></div>
								<div class="col-10 col-8-sm center">
									<div class="g-recaptcha" data-sitekey="<%=UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY%>"></div>
								</div>
							</div>
							<div class="row">
								<div class="col-1 col-2-sm"></div>
								<div class="col-10 col-8-sm center">
									<input type="submit" value="<%=Util.i18n("page.changePassword.submit.label", locale)%>" />
								</div>
							</div>
						</div>
					</form>
				</div>
			</div>
		</div>
	</body>
</html>
