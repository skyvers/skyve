<%@ page session="false" language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>

<%@ page import="java.security.Principal"%>
<%@ page import="java.util.Locale"%>

<%@ page import="org.skyve.impl.util.UtilImpl"%>
<%@ page import="org.skyve.impl.web.UserAgent"%>
<%@ page import="org.skyve.impl.web.WebUtil"%>
<%@ page import="org.skyve.impl.web.AbstractWebContext"%>
<%@ page import="org.skyve.metadata.user.User"%>
<%@ page import="org.skyve.metadata.view.TextOutput.Sanitisation"%>
<%@ page import="org.skyve.util.OWASP"%>
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
	String passwordResetToken = OWASP.sanitise(Sanitisation.text, request.getParameter("t"));

	if (passwordResetToken == null) {
		passwordChangeErrorMessage = Util.i18n("page.resetPassword.link.error", locale);
	}
	// This is a postback, process it and move on
	else if ((newPasswordValue != null) && (confirmPasswordValue != null)) {
		passwordChangeErrorMessage = WebUtil.resetPassword(passwordResetToken, newPasswordValue, confirmPasswordValue);
		if (passwordChangeErrorMessage == null) {
			
			String redirectURL = response.encodeRedirectURL(Util.getHomeUrl() + "home.jsp");

			String customerName = request.getParameter(AbstractWebContext.CUSTOMER_COOKIE_NAME);
			if (customerName != null && !customerName.isBlank()) {
				redirectURL = redirectURL + "?" + AbstractWebContext.CUSTOMER_COOKIE_NAME + "=" + customerName;
			}
			
			response.sendRedirect(redirectURL);
			return;
		}
	}
%>
<!DOCTYPE html>
<html dir="<%=Util.isRTL(locale) ? "rtl" : "ltr"%>">
	<head>
		<!-- Standard Meta -->
	    <meta charset="utf-8" />
	    <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1" />
		<meta http-equiv="refresh" content="300; url=<%=basePath%>loggedOut" />
		<meta http-equiv="pragma" content="no-cache" />
		<meta http-equiv="cache-control" content="private,no-cache,no-store" />
		<meta http-equiv="expires" content="0" />
	    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0">
	    
	    <!-- Site Properties -->
		<title><%=Util.i18n("page.resetPassword.title", locale)%></title>
		<base href="<%=basePath%>" />
		
		<% if (mobile) { %>
			<meta name="format-detection" content="telephone=no" />
			<meta name="format-detection" content="email=no">
		<% } %>
		
		<%@include file="fragments/favicon.html" %>
		<link rel="stylesheet" href="semantic24/semantic.min.css">
		
		<%@include file="fragments/styles.html" %>
		<%@include file="fragments/backgroundImage.html" %>

		<script type="text/javascript" src="semantic24/jquery.slim.min.js"></script>
		<script type="text/javascript" src="semantic24/components/form.min.js"></script>
		<script type="text/javascript" src="semantic24/components/transition.min.js"></script>

		<script type="text/javascript">
			<!--
			function testMandatoryFields(form) {
				if($('.ui.form').form('is valid')) {
					return true;
				}
				
				return false;
			}
			
			$(document)
			.ready(function() {
			    $('.ui.form')
			    .form({
			        fields: {
			        	<%=newPasswordFieldName%>: {
			        		identifier: '<%=newPasswordFieldName%>',
			        		rules: [
			        			{
			        				type: 'empty',
			        				prompt: '<%=Util.i18n("page.changePassword.newPassword.error.required", locale)%>'
			        			}
			        		]
			        	},
			        	<%=confirmPasswordFieldName%>: {
			        		identifier: '<%=confirmPasswordFieldName%>',
			        		rules: [
			        			{
			        				type: 'empty',
			        				prompt: '<%=Util.i18n("page.changePassword.confirmPassword.error.required", locale)%>'
			        			},
			        			{
			        				type: 'match[<%=newPasswordFieldName%>]',
			        				prompt: '<%=Util.i18n("page.changePassword.noPasswordMatch.error.required", locale)%>'
			        			}
			        		]
			        	}
			        }
			    });
			});
			-->
		</script>
	</head>
	<% if (passwordChangeErrorMessage != null) { %>
	<body style="background:white" onload="document.forms['changeForm'].elements['<%=newPasswordFieldName%>'].focus();alert('<%=passwordChangeErrorMessage%>');">
	<% } else { %>
	<body style="background:white" onload="document.forms['changeForm'].elements['<%=newPasswordFieldName%>'].focus()">
	<% } %>
		<div class="ui middle aligned center aligned grid">
		    <div class="column">
		    	<div style="text-align: center; margin: 0 auto; margin-bottom: 10px;">
		    		<%@include file="fragments/logo.html" %>
		    	</div>
		    	<%@include file="fragments/noscript.html" %>
		    	
		    	<form name="changeForm" method="post" onsubmit="return testMandatoryFields(this)" class="ui large form">
					<input type="hidden" name="t" value="<%=passwordResetToken%>" />
		    	
		    		<div class="ui segment">
			    		<div class="ui header">
			    			<%=Util.i18n("page.changePassword.message", locale)%>
			    		</div>
		    			<div class="field">
		                    <div class="ui left icon input">
		                        <i class="lock icon"></i>
		                        <input type="password" name="<%=newPasswordFieldName%>" spellcheck="false" autocapitalize="none" autocomplete="off" autocorrect="none" placeholder="<%=Util.i18n("page.changePassword.newPassword.label", locale)%>" />
		                    </div>
		                </div>
		                <div class="field">
		                    <div class="ui left icon input">
		                        <i class="lock icon"></i>
		                        <input type="password" name="<%=confirmPasswordFieldName%>" spellcheck="false" autocapitalize="none" autocomplete="off" autocorrect="none" placeholder="<%=Util.i18n("page.changePassword.confirmPassword.label", locale)%>" />
		                    </div>
		                </div>
		                <div class="field">
							<div class="g-recaptcha" data-sitekey="<%=UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY%>"></div>
		                </div>
	                	<input type="submit" value="<%=Util.i18n("page.changePassword.submit.label", locale)%>" class="ui fluid large blue submit button" />
	                </div>
	                
	                <div class="ui error message">
		            	<%-- javascript form validation is inserted here --%> 
		            </div>
				</form>
		    </div>
		</div>
	</body>
</html>
