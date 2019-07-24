SKYVE.BizMap = function() {
	var displays = {};

	var refresh = function(display, fit) {
		$.get(display.url, function(data) {
			SKYVE.Util.scatterGMap(display, data, true, false);
		});
	};
	
	// public
	return {
		create: function(options) {
			var mapOptions = {
				zoom: 4,
				center: new google.maps.LatLng(-26,133.5),
				mapTypeId: google.maps.MapTypeId.ROADMAP
			};

			var display = displays[options.elementId];
			if (display) {
				if (display.webmap) {
					mapOptions.zoom = display.webmap.getZoom();
					mapOptions.center = display.webmap.getCenter();
					mapOptions.mapTypeId = display.webmap.getMapTypeId();
				}
			}
			else {
				display = {_objects:{}, 
							click: function(overlay, event) {
								SKYVE.BizMap.click(this, overlay, event);
							}};
				displays[options.elementId] = display;
			}
			display.infoWindow = new google.maps.InfoWindow({content: ''});

	/* TODO reinstate
				var control = document.createElement('DIV');
				control.id = this.ID + '_form';
				control.style.width = '300px';
	*/
			display.webmap = new google.maps.Map(document.getElementById(options.elementId), mapOptions);
	/* TODO reinstate
				this.webmap.controls[google.maps.ControlPosition.TOP].push(control);
	*/

			var url = SKYVE.Util.CONTEXT_URL + 'map?';
			if (options.modelName) {
				url += '_c=' + options._c + '&_m=' + options.modelName;
			}
			else if (options.queryName) {
				url += '_mod=' + options.moduleName + '&_q=' + options.queryName + '&_geo=' + options.geometryBinding;
			}
			display.url = url;
			refresh(display, true);
//				this.delayCall('_addForm', null, 1000);
			return display;
		},
		
		get: function(elementId) {
			return displays[elementId];
		},
		
		click: function(display, overlay, event) {
	    	var contents = overlay.infoMarkup;
	    	contents += '<br/><br/><input type="button" value="Zoom" onclick="window.location=\'' + SKYVE.Util.CONTEXT_URL;
			contents += '?m=' + overlay.mod + '&d=' + overlay.doc + '&i=' + overlay.bizId + "'\"/>";
	    	if (overlay.getPosition) {
				display.infoWindow.open(this.webmap, overlay);
	    		display.infoWindow.setContent(contents);
	    	}
	    	else if (overlay.getPath) {
				var bounds = new google.maps.LatLngBounds();
				var path = overlay.getPath();
				for (var k = 0, n = path.getLength(); k < n; k++) {
					bounds.extend(path.getAt(k));
				}
				var ne = bounds.getNorthEast();
				var sw = bounds.getSouthWest();
				
				display.infoWindow.setPosition(event.latLng);
	    		display.infoWindow.open(display.webmap);
	    		display.infoWindow.setContent(contents);
	    	}
	    }
	}
}();
