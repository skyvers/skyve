<%@page session="false" language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
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
	Principal p = request.getUserPrincipal();
	User user = WebUtil.processUserPrincipalForRequest(request, (p == null) ? null : p.getName(), true);
	Locale locale = (user == null) ? request.getLocale() : user.getLocale();

	// This is a postback, process it and move on
	String customerValue = request.getParameter("customer");
	String emailValue = request.getParameter("email");
	String oldCaptcha = null;
	HttpSession session = request.getSession(false);
	if (session != null) {
		oldCaptcha = (String) session.getAttribute("g-recaptcha-response");
	}
	String newCaptcha = Util.processStringValue(request.getParameter("g-recaptcha-response"));
	boolean recaptchaSet = (UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY!=null);
	
	boolean postback = (emailValue != null) && (newCaptcha != null) && (! newCaptcha.equals(oldCaptcha));
	if (postback) {
		request.getSession().setAttribute("g-recaptcha-response", newCaptcha);
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
		<!-- Standard Meta -->
	    <meta charset="utf-8" />
	    <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1" />
		<meta http-equiv="refresh" content="300; url=<%=basePath%>loggedOut" />
		<meta http-equiv="pragma" content="no-cache" />
		<meta http-equiv="cache-control" content="private,no-cache,no-store" />
		<meta http-equiv="expires" content="0" />
	    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0">
	    
	    <!-- Site Properties -->
		<title><%=Util.i18n("page.requestPasswordReset.title", locale)%></title>
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
					form.action = '<%=basePath + "pages/requestPasswordReset.jsp"%>';
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
			            email: {
			                identifier  : 'email',
			                rules: [
			                    {
			                        type   : 'empty',
			                        prompt : '<%=Util.i18n("page.requestPasswordReset.email.error.required", locale)%>'
			                    },
			                ]
			            }
			        }
			    });
			});
			-->
		</script>
		<script src='https://www.google.com/recaptcha/api.js'></script>
	</head>
	<body>
		<div class="ui middle aligned center aligned grid">
		    <div class="column">
		    	<div style="text-align: center; margin: 0 auto; margin-bottom: 10px;">
		    		<%@include file="fragments/logo.html" %>
		    	</div>
		    	
		    	<% if (postback) { %>
			    	<div class="ui large form">
			            <div class="ui segment">
			            	<div class="ui header">
			            		<%=Util.i18n("page.requestPasswordReset.complete.banner", locale)%>
			            	</div>
			            	<div class="field">
			            		<%=Util.i18n("page.requestPasswordReset.complete.message", locale)%>
			            	</div>
			            	<a href="<%=request.getContextPath()%><%=Util.getHomeUri()%>" class="ui fluid large blue submit button"><%=Util.i18n("page.login.submit.label", locale)%></a>
			            </div>
			        </div>
		    	<% } else { %>
		    		<form method="post" onsubmit="return testMandatoryFields(this)" class="ui large form">
			    		<div class="ui segment">

				    		<div class="ui header">
				    			<%=Util.i18n("page.requestPasswordReset.banner", locale)%>
				    		</div>

							<% if (recaptchaSet) { %>
			    			<div class="field">
								<%=Util.i18n("page.requestPasswordReset.message", locale)%>
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
			                        <i class="user icon"></i>
			                        <input type="text" name="email" spellcheck="false" autocapitalize="none" autocomplete="off" autocorrect="none" placeholder="<%=Util.i18n("page.requestPasswordReset.email.label", locale)%>">
			                        <% if (customer != null) { %>
										<input type="hidden" name="customer" value="<%=customer%>" />
									<% } %>
			                    </div>
			                </div>

			                <div class="field">
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
		                	<input type="submit" value="<%=Util.i18n("page.requestPasswordReset.submit.label", locale)%>" class="ui fluid large blue submit button" />
			                <% } else { %>
			                <div class="field">
		                		<%=Util.i18n("page.resetPassword.recaptchaNotConfiguredMessage", locale)%>
			                </div>
			                <% } %>
			                
			                <div style="margin-top: 5px;">
			                	<% if (UtilImpl.CUSTOMER == null) { %>
				                	<a href="<%=request.getContextPath()%><%=Util.getHomeUri()%><%=(user == null) ? "" : ("?customer=" + user.getCustomerName())%>" class="ui fluid basic large button"><%=Util.i18n("page.login.submit.label", locale)%></a>
				                <% } else { %>
				                	<a href="<%=request.getContextPath()%><%=Util.getHomeUri()%>" class="ui fluid basic large button"><%=Util.i18n("page.login.submit.label", locale)%></a>
				                <% } %>
			                </div>
		                </div>
		                
		                <div class="ui error message">
			            	<%-- javascript form validation is inserted here --%> 
			            </div>
					</form>
		    	<% } %>
		    </div>
		</div>
	</body>
</html>
