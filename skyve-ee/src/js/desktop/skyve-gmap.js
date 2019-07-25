isc.ClassFactory.defineClass("BizMap", "Canvas");
isc.BizMap.addClassMethods({
	loadingGMap: false,
	loadGMap: function(callback) {
		if (isc.BizMap.loadingGMap) {
			setTimeout(function() {isc.BizMap.loadGMap(callback)}, 100);
		}
		else if (window.google && window.google.maps) {
			callback();
		}
		else {
			isc.BizMap.loadingGMap = true;
			SKYVE.Util.loadJS('wicket/wicket.js?v=' + SKYVE.Util.v, function() {
				SKYVE.Util.loadJS('wicket/wicket-gmap3.js?v=' + SKYVE.Util.v, function() {
					var url = 'https://maps.googleapis.com/maps/api/js?v=3&libraries=drawing';
					if (SKYVE.Util.googleMapsV3ApiKey) {
						url += '&key=' + SKYVE.Util.googleMapsV3ApiKey;
					}
					SKYVE.Util.loadJS(url, function() {
						isc.BizMap.loadingGMap = false;
						callback();
					});
				});
			});
		}
	},
	
	v: 0,
	initialise: function() {
		eval(isc.BizMap.id + '.build()');
	}
});
isc.BizMap.addMethods({
	init: function(config) {
		this._refreshTime = 10;
		this._refreshRequired = true; // set via the map UI
		this._refreshing = false; // stop multiple refreshes
		this.width = '100%';
		this.height = '100%';
		this.styleName = 'googleMapDivParent',
		this.ID = 'bizMap' + isc.BizMap.v++;
		this.redrawOnResize = false;
		this.Super("init", arguments);
		this._objects = {};
	},

	getInnerHTML: function() {
		return '<div id="' + this.ID + '_map" style="margin:0;padding:0;height:100%">Loading Map...</div>';
	},

	draw: function() {
		if (window.google && window.google.maps) {
			if (! this.isDrawn()) {
				this.build();
				return this.Super('draw', arguments);
			}
		}
		else {
			isc.BizMap.id = this.ID;
			isc.BizMap.loadGMap(isc.BizMap.initialise);
			return this.Super('draw', arguments);
		}
	},

	setDataSource: function(dataSourceID) {
		if (window.google && window.google.maps && this.webmap) {
			if (this._view) {
				this._modelName = dataSourceID;

				this._moduleName = null;
				this._queryName = null;
				this._geometryBinding = null;

				// assign this map to the edit view _grids property if this map is on a view
				var grids = this._view._grids[dataSourceID];
				if (grids) {} else {
					grids = {};
					this._view._grids[dataSourceID] = grids;
				}
				grids[this.getID()] = this;
			}
			else {
				var underscoreIndex = dataSourceID.indexOf('_');
				this._moduleName = dataSourceID.substring(0, underscoreIndex);
				this._queryName = dataSourceID.substring(underscoreIndex + 1);
				underscoreIndex = this._queryName.indexOf('_');
				this._geometryBinding = this._queryName.substring(underscoreIndex + 1);
				this._queryName = this._queryName.substring(0, underscoreIndex);

				this._modelName = null;
			}
			this._refresh(true, false);
		}
		else {
			this.delayCall('setDataSource', arguments, 100);
		}
	},
	
	build: function() {
		if (this.isDrawn()) {
			var mapOptions = {
				zoom: 4,
				center: new google.maps.LatLng(-26,133.5),
				mapTypeId: google.maps.MapTypeId.ROADMAP
			};

			if (this.webmap) {
				mapOptions.zoom = this.webmap.getZoom();
				mapOptions.center = this.webmap.getCenter();
				mapOptions.mapTypeId = this.webmap.getMapTypeId();
			}

			this.infoWindow = new google.maps.InfoWindow({content: ''});

/* TODO reinstate
			var control = document.createElement('DIV');
			control.id = this.ID + '_form';
			control.style.width = '300px';
*/
			this.webmap = new google.maps.Map(document.getElementById(this.ID + '_map'), mapOptions);
/* TODO reinstate
			this.webmap.controls[google.maps.ControlPosition.TOP].push(control);
*/
			this._refresh(true, false);
			this.delayCall('_addForm', null, 1000);
		}
		else {
			this.delayCall('build', null, 100);
		}
	},
	
	_addForm: function() {
/* TODO reinstate
		var me = this;
		isc.DynamicForm.create({
			autoDraw: true,
			htmlElement: this.ID + '_form',
			position: 'relative',
			width: 260,
			numCols: 4,
			colWidths: [130, 50, 10, 70],
			backgroundColor: "white",
			border: "1px solid #c0c0c0",
			showShadow: true,
			shadowSoftness: 10,
			shadowOffset: 0,
		    fields: [
		        {name: 'refreshTime',
		        	title: "Refresh Time (secs)",
		        	required: true,
		        	editorType: "spinner",
		        	defaultValue: 10,
		        	min: 5,
		        	max: 600,
		        	step: 1,
		        	width: 50,
		        	changed: function(form, item, value) {
		        		me._refreshTime = parseInt(value);
		        	}},
		        {name: 'refresh',
		        	title: "Refresh",
		        	type: "checkbox",
		        	defaultValue: true,
		        	changed: function(form, item, value) {
		        		me._refreshRequired = value;
		        	}}
		    ]
		});
*/
	},

	rerender: function() {
		this._refresh(false, false);
	},
	
	resume: function() {
		this._zoomed = false;
	},
	
	_refresh: function(fit, auto) {
/* TODO reinstate
		if (auto) {
			this.delayCall('_refresh', [false, true], this._refreshTime * 1000);
		}
		else if (this._called) {} else {
			this._called = true;
			this.delayCall('_refresh', [false, true], this._refreshTime * 1000);
		}
*/
		if (! this._refreshRequired) { // map UI has refresh checked off
			return;
		}
		if (this._zoomed) { // operator is zoomed-in so no point refreshing this now
			return;
		}
		if (this._refreshing) { // already triggered a refresh - waiting on XHR response
			return;
		}
		if (! this.isDrawn()) { // widget isn't even drawn yet
			return;
		}
		if (! this.isVisible()) { // widget is invisible (from condition on the UI)
			return;
		}
		
		var wkt = new Wkt.Wkt();
		var url = SKYVE.Util.CONTEXT_URL + 'map?';
/*
		// set the map bounds
		var bounds = wkt.write(this.webmap.getBounds());
		alert(bounds)
		var nw = new google.maps.LatLng(
        	    this.webmap.getBounds().getNorthEast().lat(),
        	    this.webmap.getBounds().getSouthWest().lng()
        	);
*/
		if (this._view) {
			if (this._modelName) {
				var instance = this._view.gather(false);
				url += '_c=' + instance._c + '&_m=' + this._modelName;
			}
			else {
				return;
			}
		}
		else if (this._queryName) {
			url += '_mod=' + this._moduleName + '&_q=' + this._queryName + '&_geo=' + this._geometryBinding;
		}
		else {
			return;
		}

		// ensure that only 1 refresh at a time occurs
		this._refreshing = true;

		var me = this;
		isc.RPCManager.sendRequest({
			showPrompt: true,
			evalResult: true,
			actionURL: url,
			httpMethod: 'GET',
			callback: function(rpcResponse, data, rpcRequest) {
				SKYVE.Util.scatterGMap(me, data, fit, auto);
			}
		});
	},
	
	click: function(overlay, event) {
    	var contents = overlay.infoMarkup;
    	contents += '<br/><br/><input type="button" value="Zoom" onclick="' + this.ID + '.zoom(';
    	if (overlay.getPosition) {
    		var p = overlay.getPosition();
    		contents += p.lat() + ',' + p.lng() + "," + p.lat() + ',' + p.lng() + ",'"; 
			contents += overlay.mod + "','" + overlay.doc + "','" + overlay.bizId + "')\"/>";
    		
			this.infoWindow.open(this.webmap, overlay);
    		this.infoWindow.setContent(contents);
    	}
    	else if (overlay.getPath) {
			var bounds = new google.maps.LatLngBounds();
			var path = overlay.getPath();
			for (var k = 0, n = path.getLength(); k < n; k++) {
				bounds.extend(path.getAt(k));
			}
			var ne = bounds.getNorthEast();
			var sw = bounds.getSouthWest();
			
			contents += ne.lat() + ',' + sw.lng() + "," + sw.lat() + ',' + ne.lng() + ",'";
			contents += this.mod + "','" + this.doc + "','" + this.bizId + "')\"/>";

			this.infoWindow.setPosition(event.latLng);
    		this.infoWindow.open(this.webmap);
    		this.infoWindow.setContent(contents);
    	}
	},
	
	zoom: function(topLeftLat, topLeftLng, bottomRightLat, bottomRightLng, bizModule, bizDocument, bizId) {
		this._zoomed = true; // indicates that we don't want refreshes as we are zoomed on an overlay
		
		var scale = Math.pow(2, this.webmap.getZoom());
    	var nw = new google.maps.LatLng(
    	    this.webmap.getBounds().getNorthEast().lat(),
    	    this.webmap.getBounds().getSouthWest().lng()
    	);
    	var worldCoordinateNW = this.webmap.getProjection().fromLatLngToPoint(nw);
    	var topLeftPosition = new google.maps.LatLng(topLeftLat, topLeftLng);
    	var topLeftWorldCoordinate = this.webmap.getProjection().fromLatLngToPoint(topLeftPosition);
    	var bottomRightPosition = new google.maps.LatLng(bottomRightLat, bottomRightLng);
    	var bottomRightWorldCoordinate = this.webmap.getProjection().fromLatLngToPoint(bottomRightPosition);

		var pageRect = this.getPageRect();
		var x = Math.floor((topLeftWorldCoordinate.x - worldCoordinateNW.x) * scale) + pageRect[0];
		var y = Math.floor((topLeftWorldCoordinate.y - worldCoordinateNW.y) * scale) + pageRect[1]; 
		var width = Math.floor((bottomRightWorldCoordinate.x - worldCoordinateNW.x) * scale) + pageRect[0] - x;
		var height = Math.floor((bottomRightWorldCoordinate.y - worldCoordinateNW.y) * scale) + pageRect[1] - y;
		
		var me = this;
		isc.BizUtil.getEditView(bizModule, 
									bizDocument,
									function(view) { // the view
										isc.WindowStack.popup([x, y, width, height], "Edit", true, [view]);
										view.editInstance(bizId, null, null);
										me.infoWindow.close();
									});

	}
});

isc.ClassFactory.defineClass("BizMapPicker", "HTMLFlow");
isc.BizMapPicker.addClassMethods({
	v: 0,
	initialise: function() {
		eval(isc.BizMapPicker.id + '.build()');
	}
});
isc.BizMapPicker.addMethods({
	init: function(config) {
		this.width = '100%';
		this.height = '100%';
		this.styleName = 'googleMapDivParent';
		this.ID = 'bizMapPicker' + isc.BizMapPicker.v++;
		this.contents = '<div id="' + this.ID + '_map" style="margin:0;padding:0;height:100%">Loading Map...</div>';
		this.Super("init", arguments);
		this._overlays = [];
		this.field = config.field;
		this.drawingTools = config.drawingTools;
		
		if (window.google && window.google.maps) {
			this.build();
		}
		else {
			isc.BizMapPicker.id = this.ID;
			isc.BizMap.loadGMap(isc.BizMapPicker.initialise);
		}
	},

    mapIt: function() {
    	var value = this.field.getValue();
    	SKYVE.Util.scatterGMapValue(this, value);
    },

    clearIt: function () {
    	SKYVE.Util.clearGMap(this);
    },

	build: function() {
		if (this.isDrawn()) {
			var mapOptions = {
				zoom: 4,
				center: new google.maps.LatLng(-26,133.5),
				mapTypeId: google.maps.MapTypeId.ROADMAP,
				mapTypeControlOptions: {
            		style: google.maps.MapTypeControlStyle.DROPDOWN_MENU
            	}
			};
			this.webmap = new google.maps.Map(document.getElementById(this.ID + '_map'), mapOptions);

			if (! this.field.isDisabled()) {
				var drawingDefaults = {
	                    editable: true,
	                    strokeColor: '#990000',
	                    fillColor: '#EEFFCC',
	                    fillOpacity: 0.6
	            };

				this.webmap.drawingManager = new google.maps.drawing.DrawingManager({
	            	drawingControlOptions: {
	                    position: google.maps.ControlPosition.LEFT_BOTTOM,
	                    defaults: drawingDefaults,
	                    drawingModes: SKYVE.Util.gmapDrawingModes(this.drawingTools)
	                },
	                markerOptions: drawingDefaults,
	                polygonOptions: drawingDefaults,
	                polylineOptions: drawingDefaults,
	                rectangleOptions: drawingDefaults
	            });
	            this.webmap.drawingManager.setMap(this.webmap);

	            var me = this;
	            
	            google.maps.event.addListener(this.webmap.drawingManager, 'overlaycomplete', function (event) {
	                me.clearIt();

	                // Set the drawing mode to "pan" (the hand) so users can immediately edit
                	this.setDrawingMode(null);

	                me._overlays.push(event.overlay);
	                var wkt = new Wkt.Wkt();
	                wkt.fromObject(event.overlay);
	                var wktValue = wkt.write();
	                me.field.setValueFromPicker(wktValue);
	            });
			}

			this.clearIt();
			// delay the mapIt call because even though the maps API is synchronous, sometimes the
			// maps JS calls seem to beat the initialisation of the map.
			this.delayCall('mapIt', null, 100);
		}
		else {
			this.delayCall('build', null, 100);
		}
	}
});
