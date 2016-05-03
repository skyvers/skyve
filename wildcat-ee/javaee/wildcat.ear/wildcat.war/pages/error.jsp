<%@page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@page isErrorPage="true"%>
<%@page import="java.security.Principal"%>
<%@page import="java.util.Locale"%>
<%@page import="org.skyve.metadata.user.User"%>
<%@page import="org.skyve.util.Util"%>
<%@page import="org.skyve.impl.web.UserAgent"%>
<%@page import="org.skyve.impl.web.WebUtil"%>
<%
	String basePath = Util.getSkyveContextUrl() + "/";
	boolean mobile = UserAgent.getType(request).isMobile();
	Principal p = request.getUserPrincipal();
	User user = WebUtil.processUserPrincipalForRequest(request, (p == null) ? null : p.getName(), true);
	Locale locale = (user == null) ? request.getLocale() : user.getLocale();
%>
<!DOCTYPE html>
<html dir="<%=Util.isRTL(locale) ? "rtl" : "ltr"%>">
<head>
<meta http-equiv="X-UA-Compatible" content="IE=EDGE" />
<title><%=Util.i18n("page.error.title", locale)%></title>
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
</head>
<body>
<form>
<table align="center" height="100%" >
	<tr><td height="30%">&nbsp;</td></tr>

	<tr bgColor="#4682B4">
		<td align="center" height="5%"><strong style="color:#FFFFFF"><font face="Tahoma"><%=Util.i18n("page.error.banner", locale)%></font></strong></td>
	</tr>
	<tr bgColor="#4682B4">
		<td style="color:#FFFFFF" align="center" height="10%">
			<font size="2" face="Tahoma">
				<%=Util.i18n("page.error.explanation", locale)%>
				<br/>
				<br/>
					<%=Util.i18n("page.loginError.retry", locale)%>
					<a href="<%=request.getContextPath()%><%=org.skyve.util.Util.getHomeUri()%>" target="_top" style="color:white"><%=Util.i18n("page.error.retry.link", locale)%></a> 
					<%=Util.i18n("page.error.alternative", locale)%> 
					<a href="mailto:info@bizhub.com.au?subject=Exception Report&body=<%=(exception == null) ? Util.i18n("page.loginError.noMessage", locale) : exception.getLocalizedMessage()%> 
					for <%=(request.getUserPrincipal() != null) ? request.getUserPrincipal().getName() : Util.i18n("page.loginError.notLoggedIn", locale)%> 
					 <%=new java.util.Date()%>" style="color:white"><%=Util.i18n("page.loginError.report", locale)%></a>
			</font> 
		</td>
	</tr>
	<tr><td >&nbsp;</td></tr>

</table>

</form>
</body>
</html>
