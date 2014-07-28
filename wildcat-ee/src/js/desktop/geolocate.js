var Map = {
	_geocoder: null,
	_infoWindow: null,
	_map: null,

	lat: null,
	lng: null,
	description: null,
	address: '',
	city: '',
	state: '',
	postcode: '',
	country: '',

	onClick: function(event) {
		Map.geocode({latLng: event.latLng});
	},
	
	openInfoWindow: function(latLng, description) {
		Map.lat = latLng.lat().toFixed(5);
		Map.lng = latLng.lng().toFixed(5);
		Map.description = description;
		var content = '(' + Map.lat + ', ' + Map.lng + ')' + '<br/>';
		if (description) {
			content += description + '<br/>';
		}
		content += '<br/><input type="button" value="Pick" onclick="Map.pick();"/>';
		Map._infoWindow.setContent(content);
		Map._infoWindow.setPosition(latLng);
		Map._infoWindow.open(Map._map);
	},
	
	geocode: function(query) {
		Map._geocoder.geocode(query, function(results, status) {
			Map.description = null;
			Map.address = '';
			Map.city = '';
			Map.state = '';
			Map.postcode = '';
			Map.country = '';
			
			if (status == google.maps.GeocoderStatus.OK) {
				var latLng = results[0].geometry.location;
				if (query.latLng) {
					latLng = query.latLng;
				}

				Map._map.fitBounds(results[0].geometry.viewport);
				
				for (var i = 0; i < results[0].address_components.length; i++) {
					var address_component = results[0].address_components[i];
					for (var j = 0; j < address_component.types.length; j++) {
						var type = address_component.types[j];
						//alert(type + ' ' + address_component.long_name);
						if ((type == 'premise') || (type == 'street_address') || (type == 'street_number')) {
							Map.address = address_component.long_name;
						}
						if (type == 'route') {
							Map.address += ' ' + address_component.long_name;
						}
						if (type == 'locality') {
							Map.city = address_component.long_name;
						}
						if (type == 'establishment') { // RAAF Edinburgh
							Map.address = address_component.long_name;
						}
						if ((type == 'administrative_area_level_1') || (type == 'administrative_area_level_2') || (type == 'administrative_area_level_3')) {
							Map.state = address_component.short_name;
						}
						if (type == 'country') {
							Map.country = address_component.long_name;
						}
						if (type == 'postal_code') {
							Map.postcode = address_component.long_name;
						}
					}
				}
				
				Map.openInfoWindow(latLng, results[0].formatted_address);
			}
			else 
				if (status == google.maps.GeocoderStatus.ZERO_RESULTS) {
					alert("No results found.");
				}
				else {
					alert("Geocode was not successful for the following reason: " + status);
				}
		});
	},

	init: function(config) {
		Map._map = new google.maps.Map(document.getElementById('map'), config); 
		
		// Create a single instance of the InfoWindow object which will be shared
		// by all Map objects to display information to the user.
		Map._infoWindow = new google.maps.InfoWindow();
		  
		Map._geocoder = new google.maps.Geocoder();

		Map._searchForm = document.createElement('DIV');
		Map._searchForm.innerHTML = '<b>Click on the map <br/> or type in a search below <br/> to find a location.</b><p>' + 
										'<form onsubmit="Map.geocode({address: document.getElementById(\'address\').value});return false;"><input id="address" type="text"><input type="button" value="Search" onclick="Map.geocode({address: document.getElementById(\'address\').value});"/></form>';
		Map._searchForm.style.fontFamily = 'Verdana,Geneva,sans-serif';
		Map._searchForm.style.fontSize = '12px';
		Map._searchForm.style.backgroundColor = 'white';
		Map._searchForm.style.borderStyle = 'solid';
		Map._searchForm.style.borderWidth = '2px';
		Map._searchForm.style.padding = '2px';
		Map._searchForm.style.margin = '5px';
		Map._searchForm.style.textAlign = 'center';
		Map._searchForm.index = 1;
		Map._map.controls[google.maps.ControlPosition.TOP].push(Map._searchForm);

		// Make the info window close when clicking anywhere on the map.
		google.maps.event.addListener(Map._map, 'click', Map.onClick);
	}
};
