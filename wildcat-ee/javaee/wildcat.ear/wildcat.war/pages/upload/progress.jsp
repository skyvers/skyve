<%@ page language="java"%>
<%@page import="org.skyve.wildcat.web.upload.*,
				java.util.*,
                java.net.*,
                java.text.*"
%>
<%
//The refresh time in seconds of the upload monitor window
final int UPLOAD_MONITOR_REFRESH = 2;
%>
<%
	try
	{
		String path = request.getContextPath();
		String basePath = request.getScheme() + "://" + 
							request.getServerName() + ":" + request.getServerPort() + path;
	
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
	<meta http-equiv="content-type" content="text/html; charset=ISO-8859-1">
	<meta name="robots" content="noindex">
	<meta http-equiv="expires" content="0">
	<meta http-equiv="pragma" content="no-cache">

	<link rel="icon" type="image/png" href="images/window/BizHub16.png">
	<link rel="stylesheet" type="text/css" href="<%=basePath%>/css/basic-min.css">
	<style type="text/css">
		BODY { font-family:Verdana, Arial, Helvetica, sans-serif; font-size: 8pt; color: #666666;}
	</style><%
		String fname = request.getParameter("uplMonitor");
		//First opening
		boolean first = false;
		if (request.getParameter("first") != null) first = true;
		UploadInfo info = new UploadInfo();
		if (!first) {
			info = UploadMonitor.getInfo(fname);
			if (info == null) {
				//Windows
				int posi = fname.lastIndexOf("\\");
				if (posi != -1) info = UploadMonitor.getInfo(fname.substring(posi + 1));
			}
			if (info == null) {
				//Unix
				int posi = fname.lastIndexOf("/");
				if (posi != -1) info = UploadMonitor.getInfo(fname.substring(posi + 1));
			}
		}
		if (info.aborted) {
			UploadMonitor.remove(fname);
	%>
</head>
<body>
<b>Upload of <%=fname%></b><br><br>
Upload aborted.</body>
</html><%
		}
		else if (info.totalSize != info.currSize || info.currSize == 0) {
	%>
<% String utf8 = "UTF-8"; %>
<META HTTP-EQUIV="Refresh" CONTENT="<%=UPLOAD_MONITOR_REFRESH%>;URL=<%=request.getRequestURI()%>?uplMonitor=<%=URLEncoder.encode(fname, utf8)%>">
</head>
<body>
<b>Upload of <%=fname%></b><br><br>
<center>
<table height="20px" width="90%" bgcolor="#eeeeee" style="border:1px solid #cccccc"><tr>
<td bgcolor="blue" width="<%=info.getPercent()%>%"></td><td width="<%=100-info.getPercent()%>%"></td>
</tr></table></center>
<%=AbstractUploadServlet.convertFileSize(info.currSize)%> from <%=AbstractUploadServlet.convertFileSize(info.totalSize)%>
(<%=info.getPercent()%>%) uploaded (Speed: <%=info.getUploadRate()%>).<br>
Time: <%=info.getTimeElapsed()%> from <%=info.getTimeEstimated()%>
</body>
</html><%
		}
		else {
			UploadMonitor.remove(fname);
			%>
</head>
<body>
<b>Upload of <%=fname%></b><br><br>
Upload finished.
<table class="logo" align="center">
<tr height="50px">
		<td><img src="../../images/loading.gif"/></td>
		<td>Processing the data.</td>
	</tr>
</table>
</body>
</html><%
		}
	}
	catch (Throwable t)
	{
%>
</head>
<body>
<b>Upload Failed.</b><br><br>
</body>
</html><%
	}
%>