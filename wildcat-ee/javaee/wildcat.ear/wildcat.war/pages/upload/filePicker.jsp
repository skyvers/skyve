<%@ page language="java"%>
<%@page import="org.skyve.wildcat.util.UtilImpl" %>
<%@page import="org.skyve.wildcat.web.upload.AbstractUploadServlet" %>
<%
	String path = request.getContextPath();
	String basePath = request.getScheme() + "://" + 
						request.getServerName() + ":" + request.getServerPort() + path;
	String moduleDotDocument = (String) request.getSession().getAttribute(AbstractUploadServlet.MODULE_DOT_DOCUMENT_NAME);
	String dataGroupId = (String) request.getSession().getAttribute(AbstractUploadServlet.DATA_GROUP_ID_NAME);
	String userId = (String) request.getSession().getAttribute(AbstractUploadServlet.USER_ID_NAME);
	String id = (String) request.getSession().getAttribute(AbstractUploadServlet.DOCUMENT_ID_NAME);
	String binding = (String) request.getSession().getAttribute(AbstractUploadServlet.BINDING_NAME);
	String bizport = (String) request.getSession().getAttribute(AbstractUploadServlet.BIZPORT_NAME);
%>
<html>
	<head>
		<title>Biz Hub File Picker</title>
		<script language="javascript" type="text/javascript" src="<%=path%>/desktop/upload-min.js?v=<%=UtilImpl.JAVASCRIPT_FILE_VERSION%>"></script>

		<link rel="icon" type="image/png" href="images/window/BizHub16.png">
		<link rel="stylesheet" type="text/css" href="<%=basePath%>/css/basic-min.css">
	</head>
	<body>
<% if (request.getParameter(AbstractUploadServlet.BIZPORT_NAME) == null) { %>
		<form action="<%=basePath%>/upload" enctype="multipart/form-data" method="POST">
<% } else { %>
		<form action="<%=basePath%>/bizimport" enctype="multipart/form-data" method="POST">
<% } %>
			<input type="hidden" name="<%=AbstractUploadServlet.MODULE_DOT_DOCUMENT_NAME%>" value="<%=moduleDotDocument%>">
			<input type="hidden" name="<%=AbstractUploadServlet.DATA_GROUP_ID_NAME%>" value="<%=dataGroupId%>">
			<input type="hidden" name="<%=AbstractUploadServlet.USER_ID_NAME%>" value="<%=userId%>">
			<input type="hidden" name="<%=AbstractUploadServlet.DOCUMENT_ID_NAME%>" value="<%=id%>">
			<input type="hidden" name="<%=AbstractUploadServlet.BINDING_NAME%>" value="<%=binding%>">
			<input type="hidden" name="<%=AbstractUploadServlet.BIZPORT_NAME%>" value="<%=bizport%>">
			<input type="file" 
					size="50" 
					class="textfield" 
					onkeypress="event.cancelBubble=true;" 
					name="myFile" 
					id="myFile" />
			<input title="Upload selected file to the current working directory" 
					type="Submit" 
					class="button" 
					name="<%=AbstractUploadServlet.SUBMIT_NAME%>"
					value="<%=AbstractUploadServlet.UPLOAD_FILES%>"
					onClick="javascript:BizHub.upload.submitProgress('<%=path%>/pages/upload/progress.jsp')" />
		</form>
	</body>
</html>
