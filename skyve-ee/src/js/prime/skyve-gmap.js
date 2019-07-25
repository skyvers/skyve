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
				display = {_objects: {},
							_overlays: [], 
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

SKYVE.BizMapPicker = function() {
	var displays = {};
	
	// public
	return {
		create: function(options) {
			var mapOptions = {
				zoom: 4,
				center: new google.maps.LatLng(-26,133.5),
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
				display = {_objects: {}, _overlays: []};
				displays[options.elementId] = display;
			}
			
			display.webmap = new google.maps.Map(document.getElementById(options.elementId), mapOptions);

			if (! options.disabled) {
				var drawingDefaults = {
	                editable: true,
	                strokeColor: '#990000',
	                fillColor: '#EEFFCC',
	                fillOpacity: 0.6
	            };
				display.webmap.drawingManager = new google.maps.drawing.DrawingManager({
	            	drawingControlOptions: {
	                    position: google.maps.ControlPosition.LEFT_BOTTOM,
	                    defaults: drawingDefaults,
	                    drawingModes: SKYVE.Util.gmapDrawingModes(options.drawingTools)
	                },
	                markerOptions: drawingDefaults,
	                polygonOptions: drawingDefaults,
	                polylineOptions: drawingDefaults,
	                rectangleOptions: drawingDefaults
	            });
	            display.webmap.drawingManager.setMap(display.webmap);
	            
	            google.maps.event.addListener(display.webmap.drawingManager, 'overlaycomplete', function (event) {
	            	SKYVE.Util.clearGMap(display);
	
	                // Set the drawing mode to "pan" (the hand) so users can immediately edit
	                this.setDrawingMode(null);
	
	                display._overlays.push(event.overlay);
	                var wkt = new Wkt.Wkt();
	                wkt.fromObject(event.overlay);
	                var wktValue = wkt.write();
	                $('#' + options.elementId + '_hidden').val(wktValue);
	            });
			}
			
        	SKYVE.Util.clearGMap(display);

        	// delay the mapIt call because even though the maps API is synchronous, sometimes the
			// maps JS calls seem to beat the initialisation of the map.
			//setTimeout(function() {isc.BizMap.loadGMap(callback)}, 100);
        	SKYVE.Util.scatterGMapValue(display, $('#' + options.elementId + '_hidden').val());
		}
	}
}();
