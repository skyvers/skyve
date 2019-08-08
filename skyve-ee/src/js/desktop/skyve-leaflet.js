isc.ClassFactory.defineClass("BizMap", "Canvas");
isc.BizMap.addClassMethods({
	loadingLeaflet: false,
	loadLeaflet: function(callback) {
		if (isc.BizMap.loadingLeaflet) {
			setTimeout(function() {isc.BizMap.loadLeaflet(callback)}, 100);
		}
		else if (window.L) {
			callback();
		}
		else {
			isc.BizMap.loadingLeaflet = true;
			SKYVE.Util.loadCSS('leaflet/leaflet.css?v=' + SKYVE.Util.v, function() {
				SKYVE.Util.loadJS('leaflet/leaflet.js?v=' + SKYVE.Util.v, function() {
					SKYVE.Util.loadJS('leaflet/Path.Drag.js?v=' + SKYVE.Util.v, function() {
						SKYVE.Util.loadJS('leaflet/Leaflet.Editable.js?v=' + SKYVE.Util.v, function() {
							SKYVE.Util.loadCSS('leaflet/leaflet.fullscreen.css?v=' + SKYVE.Util.v, function() {
								SKYVE.Util.loadJS('leaflet/Leaflet.fullscreen.min.js?v=' + SKYVE.Util.v, function() {
									isc.BizMap.loadingLeaflet = false;
									callback();
								});
							});
						});
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
	// params loading, refreshTime, showRefresh
	init: function(config) {
		this._refreshRequired = true; // set via the map UI
		this._refreshing = false; // stop multiple refreshes
		this._zoomed = false; // indicates that we don't want refreshes as we are zoomed on an overlay
		this.width = '100%';
		this.height = '100%';
		this.styleName = 'googleMapDivParent',
		this.ID = 'bizMap' + isc.BizMap.v++;
		this.redrawOnResize = false;
		this.Super("init", arguments);
		this._objects = {};
		this._intervalId = null; // the interval to stop on refresh checkbox click
	},

	getInnerHTML: function() {
		return '<div id="' + this.ID + '_map" style="margin:0;padding:0;height:100%">Loading Map...</div>';
	},

	draw: function() {
		if (window.L) {
			if (! this.isDrawn()) {
				this.build();
				return this.Super('draw', arguments);
			}
		}
		else {
			isc.BizMap.id = this.ID;
			isc.BizMap.loadLeaflet(isc.BizMap.initialise);
			return this.Super('draw', arguments);
		}
	},

	setDataSource: function(dataSourceID) {
		if (window.L && this.webmap) {
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
			this._refresh(true);
		}
		else {
			this.delayCall('setDataSource', arguments, 100);
		}
	},
	
	build: function() {
		if (this.isDrawn()) {
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

			if (this.webmap) {
				mapOptions.zoom = this.webmap.getZoom();
				mapOptions.center = this.webmap.getCenter();
			}

			this._objects = {}; // if we are building a new map, there will be no overlays, so clear our state
//			this.infoWindow = new google.maps.InfoWindow({content: ''});
			var element = document.getElementById(this.ID + '_map');
			element.innerHTML = '';
			this.webmap = L.map(element, mapOptions);

			if (this.showRefresh) {
				SKYVE.Leaflet.refreshControls(this);
			}

			if (this.loading == 'lazy') {
				var me = this;
				this.webmap.on('zoomend', function(event) {
	            	if (! me._refreshing) { // don't refresh if fitting bounds in a refresh already
	            		me._refresh(false);
	            	}
				});
				this.webmap.on('moveend', function(event) {
	            	me._refresh(false);
	            });
			}
			
			this._refresh(true);

			if (this._intervalId) {
				clearInterval(this._intervalId);
				this._intervalId = null;
			}
			if ((this.refreshTime > 0) && this._refreshRequired) {
				this._intervalId = setInterval(this.rerender.bind(this), this.refreshTime * 1000);
			}
		}
		else {
			this.delayCall('build', null, 100);
		}
	},

	rerender: function() {
		this._refresh(false);
	},
	
	resume: function() {
		this._zoomed = false;
	},
	
	_refresh: function(fit) {
		if (! this._refreshRequired) { // refresh was switched off in the UI
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
		if (! this.isVisible()) { // widget is invisible (from condition on the UI or UI is not displayed at the moment)
			return;
		}
		
		var url = SKYVE.Util.CONTEXT_URL + 'map?';
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

		// add the bounds if we are in lazy loading mode
		var extents = '';
		if (this.loading == 'lazy') {
			if (this.webmap) {
				var bounds = this.webmap.getBounds();
				var point = bounds.getNorthEast();
				extents = '&_ne=POINT(' + point.lng + ' ' + point.lat + ')';
				point = bounds.getSouthWest();
	            extents += '&_sw=POINT(' + point.lng + ' ' + point.lat + ')';
			}
		}
		
		var me = this;
		isc.RPCManager.sendRequest({
			showPrompt: true,
			evalResult: true,
			actionURL: url + extents,
			httpMethod: 'GET',
			callback: function(rpcResponse, data, rpcRequest) {
				try {
					SKYVE.Leaflet.scatter(me, data, fit, true);
				}
				finally {
					me._refreshing = false;
				}
			}
		});
	},
	
	click: function(overlay, event) {
/*
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
			contents += overlay.mod + "','" + overlay.doc + "','" + overlay.bizId + "')\"/>";

			this.infoWindow.setPosition(event.latLng);
    		this.infoWindow.open(this.webmap);
    		this.infoWindow.setContent(contents);
    	}
    	else if (overlay.getBounds) {
			var bounds = overlay.getBounds();
			var ne = bounds.getNorthEast();
			var sw = bounds.getSouthWest();
			
			contents += ne.lat() + ',' + sw.lng() + "," + sw.lat() + ',' + ne.lng() + ",'";
			contents += overlay.mod + "','" + overlay.doc + "','" + overlay.bizId + "')\"/>";

			this.infoWindow.setPosition(event.latLng);
    		this.infoWindow.open(this.webmap);
    		this.infoWindow.setContent(contents);
    	}
*/
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
		
		if (window.L) {
			this.build();
		}
		else {
			isc.BizMapPicker.id = this.ID;
			isc.BizMap.loadLeaflet(isc.BizMapPicker.initialise);
		}
	},
	
	setDisabled: function(disabled) {
		SKYVE.Leaflet.setDisabled(this, disabled);
	},

    mapIt: function() {
    	var value = this.field.getValue();
    	SKYVE.Leaflet.scatterValue(this, value);
    },

    setFieldValue: function(wktValue) {
        this.field.setValueFromPicker(wktValue);
    },
    
	build: function() {
		if (this.isDrawn()) {
			if (! this.webmap) {
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
				var element = document.getElementById(this.ID + '_map');
				element.innerHTML = '';
				this.webmap = L.map(element, mapOptions);
	
				SKYVE.Leaflet.drawingTools(this);
				SKYVE.Leaflet.geoLocator(this);
				SKYVE.Leaflet.setDisabled(this, this.field.isDisabled());
			}	
			SKYVE.Leaflet.clear(this);
			this.mapIt();
		}
		else {
			this.delayCall('build', null, 100);
		}
	}
});
