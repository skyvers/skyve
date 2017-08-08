<%@ page language="java"%>
<%@ page import="org.skyve.impl.util.UtilImpl" %>
<%@ page import="org.skyve.util.Util" %>

<% 
	String path = Util.getSkyveContext();
	String basePath = Util.getSkyveContextUrl() + "/";

	StringBuilder onload = new StringBuilder(128);
	String latitudeBinding = request.getParameter("_latitude");
	String latitude = (latitudeBinding != null) ? request.getParameter(latitudeBinding) : null;
	if ((latitude != null) && latitude.trim().isEmpty()) {
		latitude = null;
	}
	String longitudeBinding = request.getParameter("_longitude");
	String longitude = (longitudeBinding != null) ? request.getParameter(longitudeBinding) : null;
	if ((longitude != null) && longitude.trim().isEmpty()) {
		longitude = null;
	}
	String descriptionBinding = request.getParameter("_description");
	String description = (descriptionBinding != null) ? request.getParameter(descriptionBinding) : null;
	if ((description != null) && description.trim().isEmpty()) {
		description = null;
	}
	String addressBinding = request.getParameter("_address");
	String address = (addressBinding != null) ? request.getParameter(addressBinding) : null;
	if ((address != null) && address.trim().isEmpty()) {
		address = null;
	}
	String cityBinding = request.getParameter("_city");
	String city = (cityBinding != null) ? request.getParameter(cityBinding) : null;
	if ((city != null) && city.trim().isEmpty()) {
		city = null;
	}
	String stateBinding = request.getParameter("_state");
	String state = (stateBinding != null) ? request.getParameter(stateBinding) : null;
	if ((state != null) && state.trim().isEmpty()) {
		state = null;
	}
	String postcodeBinding = request.getParameter("_postcode");
	String postcode = (postcodeBinding != null) ? request.getParameter(postcodeBinding) : null;
	if ((postcode != null) && postcode.trim().isEmpty()) {
		postcode = null;
	}
	String countryBinding = request.getParameter("_country");
	String country = (countryBinding != null) ? request.getParameter(countryBinding) : null;
	if ((country != null) && country.trim().isEmpty()) {
		country = null;
	}
	
	if ((latitude != null) && (longitude != null)) {
		onload.append("var latlng=new google.maps.LatLng(").append(latitude).append(',').append(longitude).append(");");
		onload.append("Map._map.setCenter(latlng);");
		onload.append("Map._map.setZoom(11);");
		onload.append("Map.openInfoWindow(latlng,'");
		if (description != null) {
			onload.append(description);
		}
		onload.append("');");
	}
	else if (description != null) {
		onload.append("Map.geocode({address:'").append(description).append("'});");
	}
	else if ((address != null) || (city != null) || (state != null) || (postcode != null) || (country != null)) {
		onload.append("Map.geocode({address:'");
		if (address != null) {
			onload.append(address).append(' ');
		}
		if (city != null) {
			onload.append(city).append(' ');
		}
		if (state != null) {
			onload.append(state).append(' ');
		}
		if (postcode != null) {
			onload.append(postcode).append(' ');
		}
		if (country != null) {
			onload.append(country).append(' ');
		}
		
		onload.setLength(onload.length() - 1);
		onload.append("'});");
	}
%>

<!DOCTYPE html>
<html>
	<head> 
		<meta http-equiv="X-UA-Compatible" content="IE=EDGE" />
		<meta name="viewport" content="initial-scale=1.0, user-scalable=no" />
		<script type="text/javascript" src="https://maps.googleapis.com/maps/api/js?v=3&sensor=false&region=AU&key=<%=UtilImpl.GOOGLE_MAPS_V3_API_KEY%>"></script>
<%--
		<script type="text/javascript" src="http://maps.google.com/maps/api/js?sensor=false&region=AU&key=<%=UtilImpl.GOOGLE_MAPS_V3_API_KEY%>"></script> 
--%>
		<script language="javascript" type="text/javascript" src="<%=path%>/desktop/geolocate-min.js?v=<%=UtilImpl.WEB_RESOURCE_FILE_VERSION%>"></script>
		<script type="text/javascript">
			Map.pick = function() {
				var opener = window.parent.isc.WindowStack.getOpener();
				<% if (latitudeBinding != null) { %>
					opener._vm.setValue('<%=latitudeBinding%>', Map.lat);
				<% } %>
				<% if (longitudeBinding != null) { %>
					opener._vm.setValue('<%=longitudeBinding%>', Map.lng);
				<% } %>
				<% if (descriptionBinding != null) { %>
					opener._vm.setValue('<%=descriptionBinding%>', Map.description);
				<% } %>
				<% if (addressBinding != null) { %>
					opener._vm.setValue('<%=addressBinding%>', Map.address);
				<% } %>
				<% if (cityBinding != null) { %>
					opener._vm.setValue('<%=cityBinding%>', Map.city);
				<% } %>
				<% if (stateBinding != null) { %>
					opener._vm.setValue('<%=stateBinding%>', Map.state);
				<% } %>
				<% if (postcodeBinding != null) { %>
					opener._vm.setValue('<%=postcodeBinding%>', Map.postcode);
				<% } %>
				<% if (countryBinding != null) { %>
					opener._vm.setValue('<%=countryBinding%>', Map.country);
				<% } %>
		
				window.parent.isc.WindowStack.popoff(false);
			};
		</script>
	</head>

	<!-- if no search then use zoom and center given else call Map.geocode() and exclude zoom and center parameters -->
	<body onload="Map.init({zoom: 5, center: new google.maps.LatLng(-26,133.5), mapTypeId: google.maps.MapTypeId.HYBRID});<%=onload.toString()%>">
		<div id="map" style="width:100%; height:100%"></div>
	</body>
</html>