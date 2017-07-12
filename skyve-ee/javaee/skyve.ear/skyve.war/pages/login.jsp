<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ page import="java.util.Locale"%>
<%@ page import="org.skyve.CORE"%>
<%@ page import="org.skyve.metadata.customer.Customer"%>
<%@ page import="org.skyve.util.Util"%>
<%@ page import="org.skyve.web.WebContext"%>
<%@ page import="org.skyve.impl.web.WebUtil"%>
<%@ page import="org.skyve.impl.web.UserAgent"%>
<%
	String basePath = Util.getSkyveContextUrl() + "/";
	String customer = WebUtil.determineCustomerWithoutSession(request);

	// Clear the user object from the session if it exists
	// If there is a public user set, this will ensure it doesn't get in the way.
	if (session != null) {
		Object user = session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
		if (user != null) {
			session.removeAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
		}
	}
	
	// Determine the locale
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
	
	String customerFieldName = "customer";
	String userFieldName = "user";

	boolean mobile = UserAgent.getType(request).isMobile();
	String fontSize = (mobile ? "18px" : "18px");
	String fieldWidth = (mobile ? "120px" : "120px");
%>
<!DOCTYPE html>
<html dir="<%=Util.isRTL(locale) ? "rtl" : "ltr"%>">
	<head>
		<meta http-equiv="X-UA-Compatible" content="IE=EDGE" />
		<meta http-equiv="refresh" content="300; url=<%=basePath%>loggedOut" />
		<title><%=Util.i18n("page.login.title", locale)%></title>
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
				if (form.customer.value.length < 1) {
					alert('<%=Util.i18n("page.login.customer.error.required", locale)%>');
					form.customer.focus();
					return false;
				}
				else if (form.user.value.length < 1) {
					alert('<%=Util.i18n("page.login.user.error.required", locale)%>');
					form.user.focus();
					return false;
				}
				else if (form.j_password.value.length < 1) {
					alert('<%=Util.i18n("page.login.password.error.required", locale)%>');
					form.j_password.focus();
					return false;
				}
				else {
					var hidden = document.createElement('input');
					hidden.setAttribute('type', 'hidden');
					hidden.setAttribute('name', 'j_username');
					hidden.setAttribute('value', form.customer.value + "/" + form.user.value);
					form.appendChild(hidden);
					form.action = 'j_security_check';
					return true;
				}
			}
			-->
		</script>
	</head>
	<body onload="document.forms['loginForm'].elements['<%=(customer == null) ? customerFieldName : userFieldName%>'].focus()">
		<SCRIPT>//'"]]>>isc_loginRequired
		if (!window.isc && document.domain && document.domain.indexOf(".") != -1 
			&& !(new RegExp("^(\\d{1,3}\\.){3}\\d{1,3}$").test(document.domain))) 
		{
		    
		    var set = false;
		    while (document.domain.indexOf(".") != -1) {
		        try {
		            if (window.opener && window.opener.isc) break;
		            if (window.top.isc) break;
		            
		            if (!set) { document.domain = document.domain; set = true; }
		            else { document.domain = document.domain.replace(/.*?\./, ''); }
		        } catch (e) {
		            try {
		                if (!set) { document.domain = document.domain; set = true }
		                else { document.domain = document.domain.replace(/.*?\./, ''); }
		            } catch (ee) {
		                break;
		            }
		        }
		    } 
		}
		
		var isc = top.isc ? top.isc : window.opener ? window.opener.isc : null;
		if (isc && isc.RPCManager) isc.RPCManager.delayCall("handleLoginRequired", [window]);
		</SCRIPT>

		<div class="container">
			<%@include file="fragments/logo.html" %>
			<%@include file="fragments/noscript.html" %>
			<div class="row">
				<div class="col-3 col-2-md hidden-sm"></div>
				<div class="col-6 col-8-md col-12-sm">
					<form name="loginForm" method="post" onsubmit="return testMandatoryFields(this)">
						<div class="loginTable" style="width:100%;">
							<div class="row">
								<div class="col-12 center">
									<% if (request.getUserPrincipal() != null) { %>
										<span class="subhead"><%=Util.i18n("page.login.alreadyLoggedIn", locale, request.getUserPrincipal().getName())%></span>
									<% } else { %>
										<span class="subhead"><%=Util.i18n("page.login.banner", locale)%></span>
									<% } %>
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
									<label for="user"><%=Util.i18n("page.login.user.label", locale)%></label>
								</div>
								<div class="col-7-sm">
									<% if (customer != null) { %>
										<input type="hidden" name="customer" value="<%=customer%>" />
									<% } %>
									<input type="text" spellcheck="false" autocapitalize="none" autocomplete="off" autocorrect="none" name="user">
								</div>
							</div>
							<div class="row">
								<div class="col-4-sm right">
									<label for="j_password"><%=Util.i18n("page.login.password.label", locale)%></label>
								</div>
								<div class="col-7-sm">
									<input type="password" spellcheck="false" autocapitalize="none" autocomplete="off" autocorrect="none" name="j_password">
								</div>
							</div>
							<div class="row">
								<div class="col-1 col-2-sm"></div>
								<div class="col-10 col-8-sm center">
									<input type="submit" value="<%=Util.i18n("page.login.submit.label", locale)%>" />
								</div>
							</div>
						</div>
					</form>
				</div>
			</div>
		</div>
	</body>
</html>
