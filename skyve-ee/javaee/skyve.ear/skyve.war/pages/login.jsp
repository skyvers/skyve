<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ page import="java.util.Locale"%>
<%@ page import="org.skyve.CORE"%>
<%@ page import="org.skyve.metadata.customer.Customer"%>
<%@ page import="org.skyve.util.Util"%>
<%@ page import="org.skyve.impl.util.UtilImpl"%>
<%@ page import="org.skyve.web.WebContext"%>
<%@ page import="org.skyve.impl.web.WebUtil"%>
<%@ page import="org.skyve.impl.web.UserAgent"%>
<%
	String basePath = Util.getSkyveContextUrl() + "/";
	String customer = WebUtil.determineCustomerWithoutSession(request);

	// Check if this was a login error
	boolean loginError = (request.getParameter("error") != null);

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
	
	// is self-registration enabled
	boolean allowRegistration = UtilImpl.ACCOUNT_ALLOW_SELF_REGISTRATION;
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
		<script type="text/javascript" src="prime/skyve-min.js"></script>

		<script type="text/javascript">
			<!--
			function testMandatoryFields(form) {
				if($('.ui.form').form('is valid')) {
					var hidden = document.createElement('input');
					hidden.setAttribute('type', 'hidden');
					hidden.setAttribute('name', 'username');
					hidden.setAttribute('value', form.customer.value + "/" + form.user.value);
					form.appendChild(hidden);
					form.action = 'login';
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
			                        prompt : '<%=Util.i18n("page.login.password.error.required", locale)%>'
			                    }
			                ]
			            }
			        }
			    });
			    SKYVE.Util.setTouchCookie();
			});
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
		        <form name="loginForm" method="post" onsubmit="return testMandatoryFields(this)" class="ui large form">
		            <div class="ui segment">
		            	<div class="ui header">
		            		<% if (request.getUserPrincipal() != null) { %>
								<%=Util.i18n("page.login.alreadyLoggedIn", locale, request.getUserPrincipal().getName())%>
							<% } else { %>
								<%=Util.i18n("page.login.banner", locale)%>
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
		                    <div class="ui left icon input">
	                    	<% if(allowRegistration) { %>
	                    		<i class="envelope icon"></i>
	                    		<input type="text" id="user" name="user" spellcheck="false" autocapitalize="none" autocomplete="off" autocorrect="none" placeholder="<%=Util.i18n("page.login.email.label", locale)%>">
	                    	<% } else { %>
		                        <i class="user icon"></i>
		                        <input type="text" id="user" name="user" spellcheck="false" autocapitalize="none" autocomplete="off" autocorrect="none" placeholder="<%=Util.i18n("page.login.user.label", locale)%>">
							<% } %>
							<% if (customer != null) { %>
								<input type="hidden" name="customer" value="<%=customer%>" />
							<% } %>
		                    </div>
		                </div>
		                <div class="field">
		                    <div class="ui left icon input">
		                        <i class="lock icon"></i>
		                        <input type="password" id="password" name="password" spellcheck="false" autocapitalize="none" autocomplete="off" autocorrect="none" placeholder="<%=Util.i18n("page.login.password.label", locale)%>">
		                    </div>
		                </div>
		                <div class="equal width fields">
			                <div class="field" style="text-align: left">
			                	<div class="ui checkbox">
			                		<input type="checkbox" tabindex="0" class="hidden" id="remember" name="remember">
									<label for="remember"><%=Util.i18n("page.login.remember.label", locale)%></label>
								</div>
							</div>
							<div class="field" style="text-align: right;">
								<a href="<%=basePath%>pages/requestPasswordReset.jsp"><%=Util.i18n("page.login.reset.label", locale)%></a>
	    					</div>
    					</div>
						<input type="submit" value="<%=Util.i18n("page.login.submit.label", locale)%>" class="ui fluid large blue submit button" />
		            </div>
		            
		            <div class="ui error message">
		            	<%-- javascript form validation is inserted here --%> 
		            </div>
		        </form>
		        <% if(allowRegistration && customer != null) { %>
				<div class="ui message">
					<%=Util.i18n("page.login.register.label.pre", locale)%> <a href="?a=e&m=admin&d=SelfRegistration"><%=Util.i18n("page.login.register.label.post", locale)%></a>
			    </div>	        
			    <% } %>
		    </div>
		</div>
	</body>
</html>
