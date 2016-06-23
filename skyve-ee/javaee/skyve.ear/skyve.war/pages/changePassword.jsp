<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>

<%@ page import="java.security.Principal"%>
<%@ page import="java.util.Locale"%>
<%@ page import="org.apache.commons.codec.binary.Base64"%>

<%@ page import="org.skyve.impl.web.UserAgent"%>
<%@ page import="org.skyve.impl.web.WebUtil"%>
<%@ page import="org.skyve.metadata.user.User"%>
<%@ page import="org.skyve.util.Util"%>
<%@ page import="org.skyve.web.WebContext"%>
<%
	final String newPasswordFieldName = "newPassword";
	final String confirmPasswordFieldName = "confirmPassword";
	
	User user = null;
	
	Principal p = request.getUserPrincipal();
	if (p == null) { // not logged in
		response.sendRedirect(response.encodeRedirectURL(Util.getHomeUrl() + "home.jsp"));
		return;
	}
	else {
		// Get the user
		user = (User) request.getSession().getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
		if (user == null) { // if the user is not established yet (but we've logged in...)
			response.sendRedirect(response.encodeRedirectURL(Util.getHomeUrl() + "home.jsp"));
			return;
		}
	}

	// This is a postback, process it and move on
	String passwordChangeErrorMessage = null;
	String newPasswordValue = request.getParameter(newPasswordFieldName);
	String confirmPasswordValue = request.getParameter(newPasswordFieldName);
	if ((newPasswordValue != null) && (confirmPasswordValue != null)) {
		passwordChangeErrorMessage = WebUtil.makePasswordChange(user, newPasswordValue);
		if (passwordChangeErrorMessage == null) {
			request.getSession().setAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME, user);
			response.sendRedirect(response.encodeRedirectURL(Util.getHomeUrl() + "home.jsp"));
			return;
		}
	}
	
	String basePath = Util.getSkyveContextUrl() + "/";
	boolean mobile = UserAgent.getType(request).isMobile();
	Locale locale = user.getLocale();

	String fontSize = (mobile ? "18px" : "18px");
	String fieldWidth = (mobile ? "120px" : "120px");
%>
<!DOCTYPE html>
<html dir="<%=Util.isRTL(locale) ? "rtl" : "ltr"%>">
	<head>
		<meta http-equiv="X-UA-Compatible" content="IE=EDGE" />
		<title><%=Util.i18n("page.changePassword.title", locale)%></title>
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
		<body onload="document.forms['changeForm'].elements['<%=newPasswordFieldName%>'].focus();alert('<%=passwordChangeErrorMessage%>');">
	<% } else { %>
		<body onload="document.forms['changeForm'].elements['<%=newPasswordFieldName%>'].focus()">
	<% } %>
		<table class="logo" align="center">
			<tr height="150px">
				<td>
					<img src="images/skyve.png" alt="Skyve" />
				</td>
			</tr>
		</table>

		<form name="changeForm" method="post" onsubmit="return testMandatoryFields(this)">
			<table align="center">
				<tr>
					<td>
						<div class="loginTable" style="width:100%;">
							<div >
								<div >
									<div >
										&nbsp;
									</div>
								</div>
							</div>
							<div >
								<div >
									<div >
										<table style="border-spacing: 10px;">
											<!-- warn user if javascript is not enabled -->
											<noscript>
											<tr>
												<td>
													<center>
														<p>
															<b><%=Util.i18n("page.changePassword.javascriptDisabled", locale)%></b>
														</p>
													</center>
												</td>
											</tr>
											</noscript>
											<tr>
												<td>
													<table align="center">
														<tr>
															<% if (! mobile) { %>
															<td>
																&nbsp;
															</td>
															<% } %>
															<td>
																<table>
																	<tr>
																		<td style="font-size:18px">
																			<%=Util.i18n("page.changePassword.newPassword.label", locale)%>
																		</td>
																		<td>
																			<input type="password" style="font-size:<%=fontSize%>;width:<%=fieldWidth%>" autocorrect="off" autocapitalize="off" name="<%=newPasswordFieldName%>">
																		</td>
																	</tr>
																	<tr>
																		<td style="font-size:18px">
																			<%=Util.i18n("page.changePassword.confirmPassword.label", locale)%>
																		</td>
																		<td>
																			<input type="password" style="font-size:<%=fontSize%>;width:<%=fieldWidth%>" name="<%=confirmPasswordFieldName%>">
																		</td>
																	</tr>
																</table>
															</td>
															<td>
															</td>
														</tr>
														<tr>
															<td colspan="2">
																<br/>
																<input type="submit" value="<%=Util.i18n("page.changePassword.submit.label", locale)%>" style="font-size:<%=fontSize%>" />
															</td>
														</tr>
													</table>
												</td>
											</tr>
										</table>
										<div></div>
									</div>
								</div>
							</div>
							<div>
								<div>
									<div>
										&nbsp;
									</div>
								</div>
							</div>
						</div>
					</td>
				</tr>
			</table>
		</form>
	</body>
</html>
