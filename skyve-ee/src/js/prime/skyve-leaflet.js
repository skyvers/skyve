SKYVE.BizMap = function() {
	var displays = {};

	var refresh = function(display, fit) {
		// NB can't check if the refresh is switched off here as we need it to fire always for lazy loading
		if (display._refreshing) { // already triggered a refresh - waiting on XHR response
			return;
		}

		// ensure that only 1 refresh at a time occurs
		display._refreshing = true;

		var extents = '';
		if (display.loading === 'lazy') {
			var bounds = display.webmap.wrapLatLngBounds(display.webmap.getBounds());
			var point = bounds.getNorthEast();
			extents = '&_ne=POINT(' + point.lng + ' ' + point.lat + ')';
			point = bounds.getSouthWest();
            extents += '&_sw=POINT(' + point.lng + ' ' + point.lat + ')';
		}
		$.get(display.url + extents, function(data) {
			try {
				SKYVE.Leaflet.scatter(display, data, fit, true);
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
				center: [0, 0],
				fullscreenControl: {
					pseudoFullscreen: false
				},
			    layers: [
					L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
						maxZoom: 19
					})
                ]
			};

			var display = displays[options.elementId];
			if (display) {
				if (display.webmap) {
					mapOptions.zoom = display.webmap.getZoom();
					mapOptions.center = display.webmap.getCenter();
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
					click: function(layer) {
						return SKYVE.BizMap.click(this, layer);
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

			display._objects = {}; // if we are building a new map, there will be no layers, so clear our state
			display.webmap = L.map(SKYVE.PF.getByIdEndsWith(options.elementId)[0], mapOptions);

			if (options.showRefresh) {
				SKYVE.Leaflet.refreshControls(display);
			}
			if (options.loading === 'lazy') {
				display.webmap.on('zoomend', function(event) {
					if (! display._refreshing) { // don't refresh if fitting bounds in a refresh already
	            		refresh(display, false);
	            	}
				});
				display.webmap.on('moveend', function(event) {
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
		
		click: function(display, layer) {
			if (display.documentName) {
				sessionStorage.setItem(display.moduleName + '_' + display.documentName + '_' + display.modelName,
										'{"centre":' + JSON.stringify(display.webmap.getCenter()) + 
											',"zoom":' + display.webmap.getZoom() + '}');
			}
			else {
				sessionStorage.setItem(display.moduleName + '_' + display.queryName + '_' + display.geometryBinding,
										'{"centre":' + JSON.stringify(display.webmap.getCenter()) + 
											',"zoom":' + display.webmap.getZoom() + '}');
			}
			
			var result = layer.zoomData.infoMarkup;
	    	result += '<br/><br/><input type="button" value="Zoom" onclick="window.location=\'' + SKYVE.Util.CONTEXT_URL;
			result += '?m=' + layer.zoomData.mod + '&d=' + layer.zoomData.doc + '&i=' + layer.zoomData.bizId + "'\"/>";
			return result;
		}
	}
}();

SKYVE.BizMapPicker = function() {
	var displays = {};

	// public
	return {
		create: function(options) {
			var mapOptions = {
				zoom: SKYVE.Util.mapZoom,
				center: SKYVE.Leaflet.centre(),
				editable: true,
				fullscreenControl: {
					pseudoFullscreen: false
				},
                layers: [
					L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
						maxZoom: 19
					})
                ]
			};

			var display = displays[options.elementId];
			if (display) {
				if (display.webmap) {
					mapOptions.zoom = display.webmap.getZoom();
					mapOptions.center = display.webmap.getCenter();
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
			display.webmap = L.map(document.getElementById(elementId), mapOptions);

			if (! options.disabled) {
				SKYVE.Leaflet.drawingTools(display);
				SKYVE.Leaflet.geoLocator(display);
			}
			
        	SKYVE.Leaflet.clear(display);
        	var textElement = SKYVE.PF.getTextElement(elementId + '_value')
			// Make text read-only to stop "change" events fired every char press
			// as we don't want to send malformed WKT to the server
        	textElement.attr('readonly', true);
        	SKYVE.Leaflet.scatterValue(display, textElement.val());
		}
	}
}();
