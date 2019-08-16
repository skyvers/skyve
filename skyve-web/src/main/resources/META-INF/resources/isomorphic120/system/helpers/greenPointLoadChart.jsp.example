<%@ page import = "com.gp.api.jsp.MxServerComponent" %>
<%@ page import = "com.gp.api.jsp.MxChartDescription"%>
<%@ page import="com.isomorphic.servlet.*" %>
<%
    MxServerComponent  svr = MxServerComponent.getDefaultInstance(application);

    MxChartDescription myChart = svr.newImageSpec();
    myChart.width = new Integer(request.getParameter("width")).intValue();
    myChart.height = new Integer(request.getParameter("height")).intValue();
    myChart.type = request.getParameter("type");
    myChart.style = request.getParameter("style");
    myChart.model = request.getParameter("model");
    //RequestContext.staticLog.warn(svr.getImageTag(myChart,"/isomorphic/system/helpers/getImage.jsp?image="));
    out.write(svr.getImageTag(myChart, request.getParameter("helperDir")+"greenPointGetImage.jsp?image="));
%>