SKYVE.BizMap = function() {
	var displays = {};
    var wkt = new Wkt.Wkt();

	var refresh = function(display, fit) {
		// NB can't check if the refresh is switched off here as we need it to fire always for lazy loading
		if (display._refreshing) { // already triggered a refresh - waiting on XHR response
			return;
		}

		// ensure that only 1 refresh at a time occurs
		display._refreshing = true;

		var extents = '';
		if (display.loading === 'lazy') {
			var bounds = display.webmap.getBounds();
			if (bounds) {
	            wkt.fromObject(bounds.getNorthEast());
	            extents = '&_ne=' + wkt.write();
	            wkt.fromObject(bounds.getSouthWest());
	            extents += '&_sw=' + wkt.write();
			}
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
				mapTypeId: eval(SKYVE.Util.mapLayers)
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
					_objects: null,
					refreshTime: options.refreshTime,
					loading: options.loading, // lazy or eager
					_refreshRequired: true, // set via the map UI
					_refreshing: false, // stop multiple refreshes
					_intervalId: null, // the interval to stop on refresh checkbox click
					click: function(overlay, event) {
						SKYVE.BizMap.click(this, overlay, event);
					},
					rerender: function() {
						refresh(this, false);
					},
					moduleName: options.moduleName,
					queryName: options.queryName,
					geometryBinding: options.geometryBinding,
					documentName: options.documentName,
					modelName: options.modelName
				};
				displays[options.elementId] = display;
			}

			// if there is an entry in session storage, use it
			var inSessionStorage = false;
			if (display.documentName) {
				var key = display.moduleName + '_' + display.documentName + '_' + display.modelName;
				var value = sessionStorage.getItem(key);
				if (value) {
					inSessionStorage = true;
					var settings = JSON.parse(value);
					mapOptions.center = settings.centre;
					mapOptions.zoom = settings.zoom;
					sessionStorage.removeItem(key);
				}
			}
			else {
				var key = display.moduleName + '_' + display.queryName + '_' + display.geometryBinding;
				var value = sessionStorage.getItem(key);
				if (value) {
					inSessionStorage = true;
					var settings = JSON.parse(value);
					mapOptions.center = settings.centre;
					mapOptions.zoom = settings.zoom;
					sessionStorage.removeItem(key);
				}
			}

			display._objects = {}; // if we are building a new map, there will be no overlays, so clear our state
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
			refresh(display, (! inSessionStorage)); // fit to bounds if not in sessionStorage

			if ((display.refreshTime > 0) && display._refreshRequired) {
				display._intervalId = setInterval(display.rerender.bind(display), display.refreshTime * 1000);
			}
			
			return display;
		},
		
		get: function(elementId) {
			return displays[elementId];
		},
		
		click: function(display, overlay, event) {
			if (display.documentName) {
				sessionStorage.setItem(display.moduleName + '_' + display.documentName + '_' + display.modelName,
										'{"centre":' + JSON.stringify(display.webmap.getCenter().toJSON()) + 
											',"zoom":' + display.webmap.getZoom() + '}');
			}
			else {
				sessionStorage.setItem(display.moduleName + '_' + display.queryName + '_' + display.geometryBinding,
										'{"centre":' + JSON.stringify(display.webmap.getCenter().toJSON()) + 
											',"zoom":' + display.webmap.getZoom() + '}');
			}
			
			var contents = overlay.infoMarkup;
	    	contents += '<br/><br/><input type="button" value="Zoom" onclick="window.location=\'' + SKYVE.Util.CONTEXT_URL;
			contents += '?m=' + overlay.mod + '&d=' + overlay.doc + '&i=' + overlay.bizId + "'\"/>";
	    	if (overlay.getPosition) {
				display.infoWindow.open(display.webmap, overlay);
	    		display.infoWindow.setContent(contents);
	    	}
	    	else if (overlay.getPath) {
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
				mapTypeId: eval(SKYVE.Util.mapLayers),
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
					_overlays: [],
					setFieldValue: function(wktValue) {
			            var element = SKYVE.PF.getTextElement(elementId + '_value');
			            element.val(wktValue);
			            element.change(); // trigger on change event here for any skyve event handling to occur
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
        	var textElement = SKYVE.PF.getTextElement(elementId + '_value')
			// Make text read-only to stop "change" events fired every char press
			// as we don't want to send malformed WKT to the server
        	textElement.attr('readonly', true);
        	SKYVE.GMap.scatterValue(display, textElement.val());
		}
	}
}();
