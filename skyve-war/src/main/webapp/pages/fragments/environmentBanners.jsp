<%-- Environment banners shared with the Faces templates and desktop - see WebUtil.getEnvironmentBannersHtml().
	The JSPs load neither basic-min.css nor prime-min.css, so link the standalone banner styles. --%>
<% 
String skyveEnvironmentBanners = org.skyve.impl.web.WebUtil.getEnvironmentBannersHtml();
if (skyveEnvironmentBanners != null) { %>
		<link rel="stylesheet" href="skyve/css/env-banner-min.css?v=<%=org.skyve.impl.util.UtilImpl.WEB_RESOURCE_FILE_VERSION%>">
		<%=skyveEnvironmentBanners%>
<% } %>
