<%@ page language="java"%>
<%@page import="org.skyve.wildcat.util.UtilImpl" %>
<%@page import="org.skyve.wildcat.web.upload.AbstractUploadServlet" %>

<%
	String path = request.getContextPath();
	String basePath = request.getScheme() + "://" + 
						request.getServerName() + ":" + request.getServerPort() + path;
	String moduleDotDocument = request.getParameter(AbstractUploadServlet.MODULE_DOT_DOCUMENT_NAME);
	String dataGroupId = request.getParameter(AbstractUploadServlet.DATA_GROUP_ID_NAME);
	String userId = request.getParameter(AbstractUploadServlet.USER_ID_NAME);
	String id = request.getParameter(AbstractUploadServlet.DOCUMENT_ID_NAME);
	String binding = request.getParameter(AbstractUploadServlet.BINDING_NAME);
	String bizport = request.getParameter(AbstractUploadServlet.BIZPORT_NAME);
	String contentId = request.getParameter(AbstractUploadServlet.CONTENT_ID_NAME);
	request.getSession().setAttribute(AbstractUploadServlet.MODULE_DOT_DOCUMENT_NAME, moduleDotDocument);
	request.getSession().setAttribute(AbstractUploadServlet.DATA_GROUP_ID_NAME, dataGroupId);
	request.getSession().setAttribute(AbstractUploadServlet.USER_ID_NAME, userId);
	request.getSession().setAttribute(AbstractUploadServlet.DOCUMENT_ID_NAME, id);
	request.getSession().setAttribute(AbstractUploadServlet.BINDING_NAME, binding);
	request.getSession().setAttribute(AbstractUploadServlet.BIZPORT_NAME, bizport);
	request.getSession().setAttribute(AbstractUploadServlet.CONTENT_ID_NAME, contentId);
%>
<html>
	<head>
		<title>Biz Hub File Picker</title>
		<script language="javascript" type="text/javascript" src="<%=path%>/desktop/upload-min.js?v=<%=UtilImpl.JAVASCRIPT_FILE_VERSION%>"></script>

		<link rel="icon" type="image/png" href="images/window/BizHub16.png">
		<link rel="stylesheet" type="text/css" href="<%=basePath%>/css/basic-min.css">
	</head>
	<body>
		<fieldset>
			<legend>Upload</legend>
			<!-- This i-frame is this height to enable display of errors -->
			<iframe frameborder="0" width="100%" height="450px" src="<%=basePath%>/pages/upload/filePicker.jsp?bizport=true"></iframe>
		</fieldset>
		<fieldset>
			<legend>Progress</legend>
			<iframe frameborder="0" width="100%" height="115px" id="progress" src="<%=basePath%>/pages/upload/blank.jsp"></iframe>
		</fieldset>
	</body>
</html>