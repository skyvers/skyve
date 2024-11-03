<%@ page session="false" language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>

<%@ page import="java.security.Principal"%>
<%@ page import="java.util.Locale"%>

<%@ page import="org.skyve.EXT"%>
<%@ page import="org.skyve.impl.util.UtilImpl"%>
<%@ page import="org.skyve.impl.security.HIBPPasswordValidator"%>
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
	
	// Captcha checking
	boolean recaptchaSet = (UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY != null || UtilImpl.CLOUDFLARE_TURNSTILE_SITE_KEY != null);
	boolean googleRecaptchaUsed = false;
	boolean cloudflareTurnstileUsed = false;
	String siteKey = null;
	if(recaptchaSet) {
		siteKey = UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY != null ? UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY : UtilImpl.CLOUDFLARE_TURNSTILE_SITE_KEY;
		if(UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY != null){
			googleRecaptchaUsed = true;
		} else {
			cloudflareTurnstileUsed = true;
		}
	}

	String passwordChangeErrorMessage = null;
	String newPasswordValue = request.getParameter(newPasswordFieldName);
	String confirmPasswordValue = request.getParameter(confirmPasswordFieldName);
	String passwordResetToken = OWASP.sanitise(Sanitisation.text, request.getParameter("t"));
	String captcha = Util.processStringValue(request.getParameter("g-recaptcha-response"));
	
    HttpSession session = request.getSession();
    
    Boolean breachedPasswordWarningShown = null;
    if (UtilImpl.CHECK_FOR_BREACHED_PASSWORD) {
    	// Check if the 'Password Breached' warning has been shown before for this password
    	if (newPasswordValue != null) {
    		breachedPasswordWarningShown = (Boolean) session.getAttribute("breachedPasswordWarningShown");
			String hashedPreviousPasswordPrefix = (String) session.getAttribute("hashedPreviousPasswordPrefix");
			String hashedNewPasswordPrefix = HIBPPasswordValidator.hashPassword(newPasswordValue).substring(0, 5);
			if (breachedPasswordWarningShown == null || hashedPreviousPasswordPrefix == null || !hashedNewPasswordPrefix.equals(hashedPreviousPasswordPrefix)) {
		        breachedPasswordWarningShown = Boolean.FALSE;
		        session.setAttribute("breachedPasswordWarningShown", breachedPasswordWarningShown);
		        session.setAttribute("hashedPreviousPasswordPrefix", hashedNewPasswordPrefix);
		    }
    	}
    }	
    
 	// Check if password is breached
    if (Boolean.FALSE.equals(breachedPasswordWarningShown) && newPasswordValue != null && HIBPPasswordValidator.isPasswordPwned(newPasswordValue)) {
        passwordChangeErrorMessage = Util.i18n("warning.breachedPasswordConfirm", locale);
        session.setAttribute("breachedPasswordWarningShown", Boolean.TRUE);
    }
	// Check if missing token
    else if (passwordResetToken == null) {
		passwordChangeErrorMessage = Util.i18n("page.resetPassword.link.error", locale);
	}
	// This is a postback, process it and move on
	else if ((newPasswordValue != null) && 
				(confirmPasswordValue != null) && 
				// Either no captcha used or the captcha has been completed
				((siteKey == null) || (captcha != null))) {
		// Remove warning flag after processing (if existing)
		session.removeAttribute("breachedPasswordWarningShown");
		session.removeAttribute("hashedPreviousPasswordPrefix");

		if (WebUtil.validateRecaptcha(captcha)) {
			passwordChangeErrorMessage = WebUtil.resetPassword(passwordResetToken, newPasswordValue, confirmPasswordValue);
			if (passwordChangeErrorMessage == null) {
				String redirectURL = response.encodeRedirectURL(Util.getBaseUrl() + "home.jsp");
	
				String customerName = request.getParameter(AbstractWebContext.CUSTOMER_COOKIE_NAME);
				if (customerName != null && !customerName.isBlank()) {
					redirectURL = redirectURL + "?" + AbstractWebContext.CUSTOMER_COOKIE_NAME + "=" + customerName;
				}
				
				response.sendRedirect(redirectURL);
				return;
			}
		}
		else {
			UtilImpl.LOGGER.severe("Recaptcha failed validation");
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
		<script type="text/javascript" src="skyve/prime/skyve-min.js"></script>
		
		<!-- Password strength estimator -->
		<script type="text/javascript" src="zxcvbn/zxcvbn-4.4.2-min.js"></script>
		<link type="text/css" rel="stylesheet" href="zxcvbn/strength-bar-min.css"/>
		
		<%-- Add script based on captcha type set --%>
		<% if (googleRecaptchaUsed) { %>
			<script src='https://www.google.com/recaptcha/api.js'></script>
		<% } else if(cloudflareTurnstileUsed) {%>
			<script src="https://challenges.cloudflare.com/turnstile/v0/api.js?compat=recaptcha" async defer></script>
		<% } %>

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
			
			// Strength indicator
			document.addEventListener('DOMContentLoaded', function() {
				var password = document.getElementById('password');
			    var progressBar = document.getElementById('progress-bar');
			    var strength = document.getElementById('password-strength-text');
			    var warning = document.getElementById('password-strength-warning');
			    var suggestions = document.getElementById('password-strength-suggestions');

			    password.addEventListener('input', function() {
			        var val = password.value;
			        var result = zxcvbn(val);
			        
					// Update progress bar styling
			        progressBar.style.width = SKYVE.Util.progressBarPower[result.score];
			        progressBar.style.backgroundColor = SKYVE.Util.progressBarColour[result.score];

			        // Update the text indicators
			        if (val !== "") {
			            strength.innerHTML = "Strength: <strong>" + SKYVE.Util.passwordStrength[result.score] + "</strong>";

			            // Show/hide the warning and suggestions
			            if (result.feedback.warning) {
			                warning.innerHTML = result.feedback.warning;
			            } else {
			                warning.innerHTML = "";
			            }

			            if (result.feedback.suggestions.length > 0) {
			                suggestions.innerHTML = result.feedback.suggestions.join(' ');
			            } else {
			                suggestions.innerHTML = "";
			            }
			        } else {
			            strength.innerHTML = "";
			            warning.innerHTML = "";
			            suggestions.innerHTML = "";
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
		                        <input type="password" name="<%=newPasswordFieldName%>" id="password" spellcheck="false" autocapitalize="none" autocomplete="off" autocorrect="none" placeholder="<%=Util.i18n("page.changePassword.newPassword.label", locale)%>" />
		                    </div>
		                    <div class="progress-bar-container">
					            <div id="progress-bar"></div>
					        </div>
							<div class="feedback" id="password-strength-text"></div>
							<div class="feedback" id="password-strength-warning"></div>
							<div class="feedback" id="password-strength-suggestions"></div>
		                </div>
		                <div class="field">
		                    <div class="ui left icon input">
		                        <i class="lock icon"></i>
		                        <input type="password" name="<%=confirmPasswordFieldName%>" spellcheck="false" autocapitalize="none" autocomplete="off" autocorrect="none" placeholder="<%=Util.i18n("page.changePassword.confirmPassword.label", locale)%>" />
		                    </div>
		                </div>
						<% if (siteKey != null) { %>
							<div class="field">
								<!-- A table to brute force the captcha to centre as it is an iframe -->
								<table>
									<tr>
										<td style="width:50%" />
										<td>
											<div class="g-recaptcha" data-sitekey="<%=siteKey%>"></div>
										</td>
										<td style="width:50%" />
									</tr>
								</table>
							</div>
			            <% } %>
	                	<input type="submit" value="<%=Util.i18n("page.changePassword.submit.label", locale)%>" class="ui fluid large blue submit button" />

		                <div style="margin-top: 5px;">
		                	<a href="<%=Util.getBaseUrl()%>" class="ui fluid basic large button"><%=Util.i18n("page.login.submit.label", locale)%></a>
		                </div>
	                </div>
	                
	                <div class="ui error message">
		            	<%-- javascript form validation is inserted here --%> 
		            </div>
				</form>
		    </div>
		</div>
	</body>
</html>