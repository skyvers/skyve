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
	String fontSize = (mobile ? "18px" : "11px");
	String fieldWidth = (mobile ? "120px" : "80px");
%>
<html>
	<head>
		<title>Biz Hub Login</title>
		<base href="<%=basePath%>" />
		
		<meta http-equiv="pragma" content="no-cache" />
		<meta http-equiv="cache-control" content="private,no-cache,no-store" />
		<meta http-equiv="expires" content="0" />
		
		<% if (mobile) { %>
			<meta name="format-detection" content="telephone=no" />
			<meta name="viewport" content="width=device-width,initial-scale=1.0,maximum-scale=1.0,minimum-scale=1.0,user-scalable=no" />
		<% } %>

		<link rel="icon" type="image/png" href="images/window/BizHub16.png" />
		<link rel="stylesheet" type="text/css" href="css/basic-min.css" />

		<script language="JavaScript">
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
			<tr height="150px">
				<td>
					<img src="images/bizhub_logo4.jpg" alt="Get Organized" />
				</td>
			</tr>
		</table>

		<form name="loginForm" method="post" onsubmit="return testMandatoryFields(this)">
			<table align="center">
				<tr>
					<td>
						<div class="x-box" style="width:100%;">
							<div class="x-box-tl">
								<div class="x-box-tr">
									<div class="x-box-tc">
									</div>
								</div>
							</div>
							<div class="x-box-ml">
								<div class="x-box-mr">
									<div class="x-box-mc">
										<table>
											<!-- warn user if javascript is not enabled -->
											<noscript>
											<tr>
												<td>
													<center>
														<p>
															<b>Warning: JavaScript is not enabled. The Biz Hub application requires that JavaScript be enabled.</b>
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
														<strong>Please enter your credentials</strong>
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
																<img src="images/loginSecurityLock.gif" alt="secure" title="This is a Secure Area" />
															</td>
															<% } %>
															<td>
																<table>
																	<% if (customer == null) { %>
																		<tr>
																			<th style="font-size:14">
																				Customer
																			</th>
																			<td>
																				<input type="text" style="font-size:<%=fontSize%>;width:<%=fieldWidth%>" autocorrect="off" autocapitalize="off" name="customer">
																			</td>
																		</tr>
																	<% } %>
																	<tr>
																		<th style="font-size:14">
																			Username
																		</th>
																		<td>
																			<% if (customer != null) { %>
																				<input type="hidden" name="customer" value="<%=customer%>" />
																			<% } %>
																			<input type="text" style="font-size:<%=fontSize%>;width:<%=fieldWidth%>" autocorrect="off" autocapitalize="off" name="user">
																		</td>
																	</tr>
																	<tr>
																		<th style="font-size:14">
																			Password
																		</th>
																		<td>
																			<input type="password" style="font-size:<%=fontSize%>;width:<%=fieldWidth%>" name="j_password">
																		</td>
																	</tr>
																	<tr>
																		<th></th>
																		<td>
																			<input type="submit" value="Login" style="font-size:<%=fontSize%>" />
																		</td>
																	</tr>
																</table>
															</td>
															<td>
															</td>
														</tr>
													</table>
												</td>
											</tr>
											<tr>
												<td>
													<table width="100%">
														<tr>
															<td align=left>
																<a href="http://www.bizhub.com.au/biz/contact.html" target="_blank"> Problems logging in? </a>
															</td>
															<td align=right>
																<a href="http://www.bizhub.com.au/biz/TermsOfUse.html" target="_blank"> Terms of Use </a>
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
							<div class="x-box-bl">
								<div class="x-box-br">
									<div class="x-box-bc">
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
