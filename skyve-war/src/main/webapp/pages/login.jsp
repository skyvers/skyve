<%@ page session="false" language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ page import="java.util.Locale"%>
<%@ page import="org.skyve.CORE"%>
<%@ page import="org.skyve.metadata.customer.Customer"%>
<%@ page import="org.skyve.util.Util"%>
<%@ page import="org.skyve.impl.util.UtilImpl"%>
<%@ page import="org.skyve.web.WebContext"%>
<%@ page import="org.skyve.impl.web.WebUtil"%>
<%@ page import="org.skyve.impl.web.UserAgent"%>
<%@ page import="org.skyve.impl.web.spring.TwoFactorAuthForwardHandler"%>
<%@ page import="org.skyve.impl.web.spring.TwoFactorAuthPushFilter"%>
<%
	String tfaToken = UtilImpl.processStringValue((String) request.getAttribute(TwoFactorAuthPushFilter.TWO_FACTOR_TOKEN_ATTRIBUTE));
	boolean show2FA = (tfaToken != null);
	boolean rmChecked = false;
	String user = null;
	boolean error2FA = false;
	

	String basePath = Util.getSkyveContextUrl() + "/";
	String customer = WebUtil.determineCustomerWithoutSession(request);
	
	// Check if this was a login error
	boolean loginError = (request.getParameter("error") != null);

	if (show2FA) {	
		customer = (String) request.getAttribute(TwoFactorAuthPushFilter.CUSTOMER_ATTRIBUTE);
		error2FA = "1".equals(request.getAttribute(TwoFactorAuthForwardHandler.TWO_FACTOR_AUTH_ERROR_ATTRIBUTE));
		user = (String) request.getAttribute(TwoFactorAuthPushFilter.USER_ATTRIBUTE);
		rmChecked = Boolean.TRUE.equals(request.getAttribute(TwoFactorAuthPushFilter.REMEMBER_ATTRIBUTE));
	}
	
	String rememberMeChecked = rmChecked ? "checked" : "";
	
	// Clear the user object from the session if it exists
	// If there is a public user set, this will ensure it doesn't get in the way.

	HttpSession session = request.getSession(false);
	if (session != null) {
		// NB do not invalidate the session here because the post login success redirect page is stored in the session.
		session.removeAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
	}
	
	// NB Remove the menu cookies here so that the post login redirect URL can establish the menu expanded state
	WebUtil.deleteMenuCookies(request, response);
	
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
	
	// is self-registration enabled
	boolean allowRegistration = (UtilImpl.ACCOUNT_ALLOW_SELF_REGISTRATION && (! show2FA));
	
	String passwordEmptyError = show2FA ? 
							Util.i18n("page.login.password.error.2FACode.required", locale) : 
							Util.i18n("page.login.password.error.required", locale);
	
	String loginBanner = show2FA ? 
					Util.i18n("page.login.2FACode.banner", locale) :
					Util.i18n("page.login.banner", locale);
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
		<title><%=Util.i18n("page.login.title", locale)%></title>
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

		<script type="text/javascript">
			function testMandatoryFields(form) {
				if($('.ui.form').form('is valid')) {
					var hidden = document.createElement('input');
					hidden.setAttribute('type', 'hidden');
					hidden.setAttribute('name', 'username');
					hidden.setAttribute('value', form.customer.value + "/" + form.user.value);
					form.appendChild(hidden);
					form.action = 'loginAttempt';
					return true;
				}
				
				return false;
			}
			
			$(document)
			.ready(function() {
			    $('.ui.form')
			    .form({
			        fields: {
			        	customer: {
			        		identifier: 'customer',
			        		rules: [
			        			{
			        				type: 'empty',
			        				prompt: '<%=Util.i18n("page.login.customer.error.required", locale)%>'
			        			}
			        		]
			        	},
			            username: {
			                identifier  : 'user',
			                rules: [
			                    {
			                        type   : 'empty',
			                        prompt : '<%=Util.i18n("page.login.user.error.required", locale)%>'
			                    },
			                ]
			            },
			            password: {
			                identifier  : 'password',
			                rules: [
			                    {
			                        type   : 'empty',
			                        prompt : '<%=passwordEmptyError%>'
			                    }
			                ]
			            }
			        }
			    });
			    SKYVE.Util.setTouchCookie();
			});
		</script>
	</head>
	<body onload="document.forms['loginForm'].elements['<%=(customer == null) ? customerFieldName : userFieldName%>'].focus()">
		<SCRIPT>//'"]]>>isc_loginRequired
		var isc = top.isc ? top.isc : window.opener ? window.opener.isc : null;
		if (isc && isc.RPCManager) isc.RPCManager.delayCall("handleLoginRequired", [window]);
		</SCRIPT>
		<div class="ui middle aligned center aligned grid">
		    <div class="column">
		    	<div style="text-align: center; margin: 0 auto; margin-bottom: 10px;">
		    		<%@include file="fragments/logo.html" %>
		    	</div>
		    	
            	<%@include file="fragments/noscript.html" %>
		        <% if (loginError) { %>
            		<div class="ui error message">
	            		<div class="header"><%=Util.i18n("page.loginError.banner", locale)%></div>
			        	<p>
			        		<%=Util.i18n("page.loginError.invalid", locale)%>
			        		<% if (UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD > 0) { %>
			        			<div style="text-align:left;">
				        			<%=Util.i18n("page.loginError.attempts", locale, String.valueOf(UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD), String.valueOf(UtilImpl.ACCOUNT_LOCKOUT_DURATION_MULTIPLE_IN_SECONDS))%>
				        		</div>
				        	<% } %>
			        	</p>
            		</div>
				<% } %>
				<% if (error2FA) { %>
					<div class="ui error message">
	            		<div class="header"><%=Util.i18n("page.loginError.banner", locale)%></div>
			        	<p>
			        		<%=Util.i18n("page.loginError.2FACode.invalid", locale)%>
			        		<% if (UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD > 0) { %>
			        			<div style="text-align:left;">
				        			<%=Util.i18n("page.loginError.attempts", locale, String.valueOf(UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD), String.valueOf(UtilImpl.ACCOUNT_LOCKOUT_DURATION_MULTIPLE_IN_SECONDS))%>
				        		</div>
				        	<% } %>
			        	</p>
            		</div>
				<% } %>
		        <form name="loginForm" method="post" onsubmit="return testMandatoryFields(this)" class="ui large form">
		            <div class="ui segment">
		            	<div class="ui header">
		            		<% if (request.getUserPrincipal() != null) { %>
								<%=Util.i18n("page.login.alreadyLoggedIn", locale, request.getUserPrincipal().getName())%>
							<% } else { %>
								<%=loginBanner%>
							<% } %>
		            	</div>
						<% if (customer == null) { %>
							<div class="field">
								<div class="ui left icon input">
									<i class="building icon"></i>
									<input type="text" id="customer" name="customer" spellcheck="false" autocapitalize="none" autocomplete="off" autocorrect="none" placeholder="<%=Util.i18n("page.login.customer.label", locale)%>">
								</div>
							</div>
						<% } %>
		                <div class="field">
		                <% if (show2FA) { %>
		                	<input type="hidden" id="user" name="user" value="<%=user%>"/>
		                	<input type="hidden" id="customer" name="customer" value="<%=customer%>"/>
		                <% } else { %>
		                    <div class="ui left icon input">
	                    	<% if(allowRegistration) { %>
	                    		<i class="envelope icon"></i>
	                    		<input type="text" inputmode="email" id="user" name="user" spellcheck="false" autocapitalize="none" autocomplete="off" autocorrect="none" placeholder="<%=Util.i18n("page.login.email.label", locale)%>">
	                    	<% } else { %>
		                        <i class="user icon"></i>
		                        <input type="text" id="user" name="user" spellcheck="false" autocapitalize="none" autocomplete="off" autocorrect="none" placeholder="<%=Util.i18n("page.login.user.label", locale)%>">
							<% } %>
							<% if (customer != null) { %>
								<input type="hidden" name="customer" value="<%=customer%>" />
							<% } %>
		                    </div>
		                <% } %>
		                </div>
		                <div class="field">
		                	<% if (show2FA) { %>
								<div class="ui left icon input">
			                        <i class="lock icon"></i>
			                        <input type="text" id="password" name="password" spellcheck="false" autocapitalize="none" autocomplete="off" autocorrect="none" inputmode="numeric" placeholder="<%=Util.i18n("page.login.2FACode.label", locale)%>"/>
			                    </div>
			                    <input type="password" id="tfaToken" name="tfaToken" hidden="true" value="<%=tfaToken%>"/>
							<% } else { %>
			                    <div class="ui left icon input">
			                        <i class="lock icon"></i>
			                        <input type="password" id="password" name="password" spellcheck="false" autocapitalize="none" autocomplete="off" autocorrect="none" placeholder="<%=Util.i18n("page.login.password.label", locale)%>">
			                    </div>
		                    <% } %>
		                </div>
		                <div class="equal width fields">
			                <div class="field" style="text-align: left">
			                	<div class="ui checkbox">
			                		<input type="checkbox" tabindex="0" class="hidden" id="remember" name="remember" <%=rememberMeChecked%>>
									<label for="remember"><%=Util.i18n("page.login.remember.label", locale)%></label>
								</div>
							</div>
								<div class="field" style="text-align: right;">
									<a href="<%=basePath%>pages/requestPasswordReset.jsp"><%=Util.i18n("page.login.reset.label", locale)%></a>
		    					</div>
    					</div>
						<input type="submit" value="<%=Util.i18n("page.login.submit.label", locale)%>" class="ui fluid large blue submit button" />
						
						<% if (show2FA) { %>
							<div style="margin-top: 5px;">
			                	<a href="<%=Util.getBaseUrl()%>" class="ui fluid basic large button"><%=Util.i18n("page.login.2FACode.return.label", locale)%></a>
			                </div>
		                <% } %>
		            </div>

					<% if ((UtilImpl.AUTHENTICATION_GOOGLE_CLIENT_ID != null) || (UtilImpl.AUTHENTICATION_FACEBOOK_CLIENT_ID != null) || (UtilImpl.AUTHENTICATION_GITHUB_CLIENT_ID != null) || (UtilImpl.AUTHENTICATION_AZUREAD_TENANT_ID != null)) { %>
					<div class="ui segment">
						<div class="ui grid">
							<div class="sixteen wide column">
								<p style="text-align: left;">Sign in with</p>
							</div>
							<% if (UtilImpl.AUTHENTICATION_GOOGLE_CLIENT_ID != null) { %>
							<div class="eight wide column" style="padding-top: 0;">
								<div class="ui fluid labeled button">
									<div class="ui red button">
										<i class="google icon"></i>
									</div>
									<a href="oauth2/authorization/google" class="ui basic red left pointing label">Google</a>
								</div>
							</div>
							<% } %>
							<% if (UtilImpl.AUTHENTICATION_FACEBOOK_CLIENT_ID != null) { %>
							<div class="eight wide column" style="padding-top: 0;">
								<div class="ui fluid labeled button">
									<div class="ui blue button">
										<i class="facebook f icon"></i>
									</div>
									<a href="oauth2/authorization/facebook" class="ui basic blue left pointing label">Facebook</a>
								</div>
							</div>
							<% } %>
							<% if (UtilImpl.AUTHENTICATION_GITHUB_CLIENT_ID != null) { %>
							<div class="eight wide column" style="padding-top: 0;">
								<div class="ui fluid labeled button">
									<div class="ui gray button">
										<i class="github icon"></i>
									</div>
									<a href="oauth2/authorization/github" class="ui basic gray left pointing label">GitHub</a>
								</div>
							</div>
							<% } %>
							<% if (UtilImpl.AUTHENTICATION_AZUREAD_TENANT_ID != null) { %>
							<div class="eight wide column" style="padding-top: 0;">
								<div class="ui fluid labeled button">
									<div class="ui gray button">
										<i class="microsoft icon"></i>
									</div>
									<a href="oauth2/authorization/microsoft" class="ui basic gray left pointing label">Azure AD</a>
								</div>
							</div>
							<% } %>							
						</div>
					</div>
					<% } %>

					<div class="ui error message">
		            	<%-- javascript form validation is inserted here --%> 
		            </div>
		        </form>
		        <% if (customer != null) { %>
					<% if (allowRegistration) { %>
						<div class="ui message">
							<%=Util.i18n("page.login.register.label.pre", locale)%> <a href="?a=e&m=admin&d=SelfRegistration"><%=Util.i18n("page.login.register.label.post", locale)%></a>
					    </div>	        
				    <% } %>
				<% } %>
		    </div>
		</div>
	</body>
</html>
