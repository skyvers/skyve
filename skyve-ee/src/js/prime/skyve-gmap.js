SKYVE.BizMap = function() {
	var displays = {};
    var wkt = new Wkt.Wkt();

	var refresh = function(display, fit) {
		if (! display._refreshRequired) { // refresh was switched off in the UI
			return;
		}
		if (display._refreshing) { // already triggered a refresh - waiting on XHR response
			return;
		}

		// ensure that only 1 refresh at a time occurs
		display._refreshing = true;

		var extents = '';
		if (display.loading == 'lazy') {
            wkt.fromObject(bounds.getNorthEast());
            extents = '&_ne=' + wkt.write();
            wkt.fromObject(bounds.getSouthWest());
            extents += '&_sw=' + wkt.write();
		}
		$.get(display.url + extents, function(data) {
			try {
				SKYVE.GMap.scatter(display, data, fit, true);
			}
			finally {
				display._refreshing = false;
			}
		});
	};
	
	// public
	return {
		create: function(options) {
			var mapOptions = {
				zoom: 1,
				center: new google.maps.LatLng(0, 0),
				mapTypeId: google.maps.MapTypeId.ROADMAP
			};

			var display = displays[options.elementId];
			if (display) {
				if (display.webmap) {
					mapOptions.zoom = display.webmap.getZoom();
					mapOptions.center = display.webmap.getCenter();
					mapOptions.mapTypeId = display.webmap.getMapTypeId();
				}
				if (display._intervalId) {
					clearInterval(display._intervalId);
					display._intervalId = null;
				}
			}
			else {
				display = {
					_objects: {},
					_overlays: [], 
					refreshTime: options.refreshTime,
					_refreshRequired: true, // set via the map UI
					_refreshing: false, // stop multiple refreshes
					_intervalId: null, // the interval to stop on refresh checkbox click
					click: function(overlay, event) {
						SKYVE.BizMap.click(this, overlay, event);
					},
					rerender: function() {
						refresh(this, false);
					}
				};
				displays[options.elementId] = display;
			}
			display.infoWindow = new google.maps.InfoWindow({content: ''});

			display.webmap = new google.maps.Map(SKYVE.PF.getByIdEndsWith(options.elementId)[0], mapOptions);

			if (options.showRefresh) {
				SKYVE.GMap.refreshControls(display);
			}

			if (options.loading === 'lazy') {
				google.maps.event.addListener(display.webmap, 'zoom_changed', function() {
					if (! display._refreshing) {  // dont refresh if fitting bounds in a refresh already
						refresh(display, false);
					}
	            });
	    	    google.maps.event.addListener(display.webmap, 'dragend', function() {
	    	    	refresh(display, false);
	            });
			}
			var url = SKYVE.Util.CONTEXT_URL + 'map?';
			if (options.modelName) {
				url += '_c=' + options._c + '&_m=' + options.modelName;
			}
			else if (options.queryName) {
				url += '_mod=' + options.moduleName + '&_q=' + options.queryName + '&_geo=' + options.geometryBinding;
			}
			display.url = url;
			refresh(display, true);

			if ((display.refreshTime > 0) && display._refreshRequired) {
				display._intervalId = setInterval(display.rerender.bind(display), display.refreshTime * 1000);
			}
			
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

SKYVE.BizMapPicker = function() {
	var displays = {};
    var wkt = new Wkt.Wkt();

	// public
	return {
		create: function(options) {
			var mapOptions = {
				zoom: SKYVE.Util.mapZoom,
				center: SKYVE.GMap.centre(),
				mapTypeId: google.maps.MapTypeId.ROADMAP,
				mapTypeControlOptions: {
	            	style: google.maps.MapTypeControlStyle.DROPDOWN_MENU
	            }
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
				display = {
					_objects: {},
					_overlays: [],
					setFieldValue: function(wktValue) {
			            SKYVE.PF.setTextValue(elementId + '_value', wktValue);
					}
				};
				displays[options.elementId] = display;
			}
			
			var elementId = SKYVE.PF.getByIdEndsWith(options.elementId).attr('id');
			display.webmap = new google.maps.Map(document.getElementById(elementId), mapOptions);

			if (! options.disabled) {
				SKYVE.GMap.drawingTools(display);
				SKYVE.GMap.geoLocator(display);
			}
			
        	SKYVE.GMap.clear(display);

        	// delay the mapIt call because even though the maps API is synchronous, sometimes the
			// maps JS calls seem to beat the initialisation of the map.
			//setTimeout(function() {isc.BizMap.loadGMap(callback)}, 100);
        	SKYVE.GMap.scatterValue(display, SKYVE.PF.getTextValue(elementId + '_value'));
		}
	}
}();
