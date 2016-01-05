<%@ page language="java"%>
<%@ page import="org.skyve.wildcat.web.WebUtil"%>
<%@ page import="org.skyve.wildcat.web.UserAgent"%>
<%
	String basePath = request.getScheme() + "://" + request.getServerName() + ":" + 
						request.getServerPort() + request.getContextPath() + "/";
	String customer = WebUtil.determineCustomerWithoutSession(request);
	String customerFieldName = "customer";
	String userFieldName = "user";

	boolean mobile = UserAgent.getType(request).isMobile();
	String fontSize = (mobile ? "18px" : "18px");
	String fieldWidth = (mobile ? "120px" : "120px");
%>
<html>
	<head>
		<title>WILDCAT: Sign-in</title>
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

		<script type="text/javascript">
			<!--
			function testMandatoryFields(form) {
				if (form.customer.value.length < 1) {
					alert('Please enter your Customer name');
					form.customer.focus();
					return false;
				}
				else if (form.user.value.length < 1) {
					alert('Please enter your User name');
					form.user.focus();
					return false;
				}
				else if (form.j_password.value.length < 1) {
					alert('Please enter your Password');
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
		<table class="logo" align="center">
			<tr height="128px">
				<td>
					<img src="images/WILDCAT_rev.png" alt="WILDCAT" />
				</td>
			</tr>
		</table>

		<form name="loginForm" method="post" onsubmit="return testMandatoryFields(this)">
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
															<b>Warning: JavaScript is not enabled. The WILDCAT application requires that JavaScript be enabled.</b>
														</p>
													</center>
												</td>
											</tr>
											</noscript>
											<tr>
												<td style="font-size:14;text-align:center">
													<%if (request.getUserPrincipal() != null) {
													%>
													You are logged in as <em><%=request.getUserPrincipal()%></em>. To access this functionality you need to login as another user with the correct permissions.
													<%} else {
													%>
													<div style="font-size:28;">Please sign in</div>
													<%}
													%>
												</td>
											</tr>
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
																	<% if (customer == null) { %>
																		<tr>
																			<td style="font-size:18">
																				Customer
																			</td>
																			<td>
																				<input type="text" style="font-size:<%=fontSize%>;width:<%=fieldWidth%>" autocorrect="off" autocapitalize="off" name="customer">
																			</td>
																		</tr>
																	<% } %>
																	<tr>
																		<td style="font-size:18">
																			Username
																		</rd>
																		<td>
																			<% if (customer != null) { %>
																				<input type="hidden" name="customer" value="<%=customer%>" />
																			<% } %>
																			<input type="text" style="font-size:<%=fontSize%>;width:<%=fieldWidth%>" autocorrect="off" autocapitalize="off" name="user">
																		</td>
																	</tr>
																	<tr>
																		<td style="font-size:18">
																			Password
																		</td>
																		<td>
																			<input type="password" style="font-size:<%=fontSize%>;width:<%=fieldWidth%>" name="j_password">
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
																<input type="submit" value="Sign in" style="font-size:<%=fontSize%>" />
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
