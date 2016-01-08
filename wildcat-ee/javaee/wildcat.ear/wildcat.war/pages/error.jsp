<%@ page language="java"%>
<%@ page isErrorPage="true"%>
<%@ page import="org.skyve.wildcat.web.UserAgent"%>
<%
	String basePath = request.getScheme() + "://" + request.getServerName() + ":" + 
						request.getServerPort() + request.getContextPath() + "/";
	boolean mobile = UserAgent.getType(request).isMobile();
%>
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="X-UA-Compatible" content="IE=EDGE" />
<title>WILDCAT: Problem</title>
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
</head>
<body>
<form>
<table align="center" height="100%" >
	<tr><td height="30%">&nbsp;</td></tr>

	<tr bgColor="#4682B4">
		<td align="center" height="5%"><strong style="color:#FFFFFF"><font face="Tahoma">Request Unsuccessful</font></strong></td>
	</tr>
	<tr bgColor="#4682B4">
		<td style="color:#FFFFFF" align="center" height="10%">
			<font size="2" face="Tahoma">An error occurred while processing your request.
				<br/>
				<br/>
					Return <a href="<%=request.getContextPath()%><%=org.skyve.util.Util.getHomeUri()%>"
					target="_top" style="color:white">here</a> and try again, 
					or <a href="mailto:info@bizhub.com.au?subject=Exception Report&body=<%=(exception == null) ? "[No Message]" : exception.getLocalizedMessage()%> 
					for <%=(request.getUserPrincipal() != null) ? request.getUserPrincipal().getName() : "[Not Logged In]"%> 
					 <%=new java.util.Date()%>" style="color:white">report the problem</a>.
			</font> 
		</td>
	</tr>
	<tr><td >&nbsp;</td></tr>

</table>

</form>
</body>
</html>
