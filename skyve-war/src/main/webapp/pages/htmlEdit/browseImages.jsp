<%@page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@page import="java.security.Principal"%>
<%@page import="java.util.Locale"%>
<%@page import="org.skyve.metadata.user.User"%>
<%@page import="org.skyve.util.Util"%>
<%@page import="org.skyve.impl.util.UtilImpl"%>
<%@page import="org.skyve.impl.web.UserAgent"%>
<%@page import="org.skyve.impl.web.WebUtil"%>
<%
	String basePath = Util.getSkyveContextUrl() + "/";
	boolean mobile = UserAgent.getType(request).isMobile();
	Principal p = request.getUserPrincipal();
	User user = WebUtil.processUserPrincipalForRequest(request, (p == null) ? null : p.getName());
	Locale locale = (user == null) ? request.getLocale() : user.getLocale();
%>
<!DOCTYPE html>
<html dir="<%=Util.isRTL(locale) ? "rtl" : "ltr"%>">
	<head>
		<meta http-equiv="X-UA-Compatible" content="IE=EDGE" />
		<meta http-equiv="Content-Type" CONTENT="text/html; CHARSET=utf-8">
		<link rel="icon" type="image/png" href="images/window/skyve_fav.png">
		<title><%=Util.i18n("page.htmlEdit.browsImages.title", locale)%></title>
		<link rel="stylesheet" type="text/css" href="../../skyve/css/basic-min.css"/>
	</head>
	<body BGCOLOR='#e0e0e0' MARGINHEIGHT=0 MARGINWIDTH=0 LEFTMARGIN=0 TOPMARGIN=0>
		<script type="text/javascript">var isomorphicDir='../../<%=UtilImpl.SMART_CLIENT_DIR%>/modules/ISC_Containers.js"></script>
		<script type="text/javascript" src="../../<%=UtilImpl.SMART_CLIENT_DIR%>/system/modules/ISC_Grids.js"></script>
		<script type="text/javascript" src="../../<%=UtilImpl.SMART_CLIENT_DIR%>/system/modules/ISC_Forms.js"></script>
		<script type="text/javascript" src="../../<%=UtilImpl.SMART_CLIENT_DIR%>/system/modules/ISC_DataBinding.js"></script>
		<script type="text/javascript" src="../../<%=UtilImpl.SMART_CLIENT_DIR%>/system/modules/ISC_Calendar.js"></script>
		<script type="text/javascript" src="../../<%=UtilImpl.SMART_CLIENT_DIR%>/system/modules/ISC_RichTextEditor.js"></script>
		<script type="text/javascript" src="../../<%=UtilImpl.SMART_CLIENT_DIR%>/skins/Enterprise/load_skin.js"></script>
		<script type=text/javascript>
			data = [
			    {
			        image: "skyve.png", 
			        name: "Skyve Logo"
			    }
			];
			imageURLPrefix = '../../../images/';
			TileGrid.create({
				autoDraw: true,
				tileLayoutPolicy: 'flow',
			    tileWidth: 200,
			    tileHeight: 200,
			    height: "100%",
			    width: "100%",
			    showAllRecords: true,
			    data: data,
			    fields: [
			        {name:"image", type:"image", imageURLPrefix: imageURLPrefix},
			        {name:"name"}
			    ],
				recordClick: function(viewer, tile, record) {
					window.opener.CKEDITOR.tools.callFunction(<%=request.getParameter("CKEditorFuncNum")%>, 'images/' + record.image); //imageURLPrefix + record.image);
					window.close();
				}
			});
		</script>
	</body>
</html>
