<%@ page language="java"%>
<%@page import="org.skyve.wildcat.util.UtilImpl" %>
<!DOCTYPE html>
<HTML>
	<HEAD>
		<meta http-equiv="X-UA-Compatible" content="IE=EDGE" />
		<META HTTP-EQUIV="Content-Type" CONTENT="text/html; CHARSET=utf-8">
		<link rel="icon" type="image/png" href="images/window/WILDCAT_fav.png">
		<TITLE>Browse Images</TITLE>
		<link rel="stylesheet" type="text/css" href="../../css/basic.css"/>
	</HEAD>
	<BODY BGCOLOR='#e0e0e0' MARGINHEIGHT=0 MARGINWIDTH=0 LEFTMARGIN=0 TOPMARGIN=0>
		<script type="text/javascript">var isomorphicDir='../../<%=UtilImpl.SMART_CLIENT_DIR%>/modules/ISC_Containers.js"></script>
		<script type="text/javascript" src="../../<%=UtilImpl.SMART_CLIENT_DIR%>/system/modules/ISC_Grids.js"></script>
		<script type="text/javascript" src="../../<%=UtilImpl.SMART_CLIENT_DIR%>/system/modules/ISC_Forms.js"></script>
		<script type="text/javascript" src="../../<%=UtilImpl.SMART_CLIENT_DIR%>/system/modules/ISC_DataBinding.js"></script>
		<script type="text/javascript" src="../../<%=UtilImpl.SMART_CLIENT_DIR%>/system/modules/ISC_Calendar.js"></script>
		<script type="text/javascript" src="../../<%=UtilImpl.SMART_CLIENT_DIR%>/system/modules/ISC_RichTextEditor.js"></script>
		<script type="text/javascript" src="../../<%=UtilImpl.SMART_CLIENT_DIR%>/skins/wildcat/load_skin.js"></script>
<%--
		<script type="text/javascript">var isomorphicDir='../../<%=UtilImpl.SMART_CLIENT_DIR%>/';</script>
		<script type="text/javascript" src="../../<%=UtilImpl.SMART_CLIENT_DIR%>/client/modules/ISC_Core.js"></script>
		<script type="text/javascript" src="../../<%=UtilImpl.SMART_CLIENT_DIR%>/client/modules/ISC_Foundation.js"></script>
		<script type="text/javascript" src="../../<%=UtilImpl.SMART_CLIENT_DIR%>/client/modules/ISC_Containers.js"></script>
		<script type="text/javascript" src="../../<%=UtilImpl.SMART_CLIENT_DIR%>/client/modules/ISC_Grids.js"></script>
		<script type="text/javascript" src="../../<%=UtilImpl.SMART_CLIENT_DIR%>/client/modules/ISC_Forms.js"></script>
		<script type="text/javascript" src="../../<%=UtilImpl.SMART_CLIENT_DIR%>/client/modules/ISC_DataBinding.js"></script>
		<script type="text/javascript" src="../../<%=UtilImpl.SMART_CLIENT_DIR%>/client/modules/ISC_Calendar.js"></script>
		<script type="text/javascript" src="../../<%=UtilImpl.SMART_CLIENT_DIR%>/client/modules/ISC_RichTextEditor.js"></script>
		<script type="text/javascript" src="../../<%=UtilImpl.SMART_CLIENT_DIR%>/skins/wildcat/load_skin.js"></script>
--%>
		<script type=text/javascript>
			data = [
			    {
			        image: "WILDCAT_soft.png", 
			        name: "WILDCAT Logo"
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
