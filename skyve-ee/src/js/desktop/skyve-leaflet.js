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
			    layers: eval(SKYVE.Util.mapLayers)
			};

			if (this.webmap) {
				mapOptions.zoom = this.webmap.getZoom();
				mapOptions.center = this.webmap.getCenter();
			}

			this._objects = {}; // if we are building a new map, there will be no overlays, so clear our state
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
		// NB can't check if the refresh is switched off here as we need it to fire always for lazy loading
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
				var bounds = this.webmap.wrapLatLngBounds(this.webmap.getBounds());
				if (bounds) {
					var point = bounds.getNorthEast();
					extents = '&_ne=POINT(' + point.lng + ' ' + point.lat + ')';
					point = bounds.getSouthWest();
		            extents += '&_sw=POINT(' + point.lng + ' ' + point.lat + ')';
				}
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
	
	click: function(layer) {
		var result = '';
		if (layer.zoomData) {
			this._selectedLayer = layer;
			
			var result = layer.zoomData.infoMarkup;
	    	result += '<br/><br/><input type="button" value="Zoom" onclick="' + this.ID + '.zoom()\"/>';
		}
		return result;
	},
	
	zoom: function() {
		this._zoomed = true; // indicates that we don't want refreshes as we are zoomed on an overlay

		var pageRect = this.getPageRect();
		var topLeft = null;
		var bottomRight = null;
		if (this._selectedLayer.getBounds) { // polylines and polygons
			var bounds = this._selectedLayer.getBounds();
			var nw = bounds.getNorthWest();
			var se = bounds.getSouthEast();
			topLeft = this.webmap.latLngToContainerPoint(nw);
			bottomRight = this.webmap.latLngToContainerPoint(se);
		}
		else { // markers
			var coordinates = this._selectedLayer.feature.geometry.coordinates;
			var point = this.webmap.latLngToContainerPoint(L.latLng(coordinates[1], coordinates[0]));
			topLeft = L.point(point.x - 10, point.y - 10);
			bottomRight = L.point(point.x + 10, point.y + 10);
		}
		
		var me = this;
		isc.BizUtil.getEditView(this._selectedLayer.zoomData.mod, 
									this._selectedLayer.zoomData.doc,
									function(view) { // the view
										// constrain to max dimensions of the map container
										isc.WindowStack.popup([
											pageRect[0] + Math.max(topLeft.x, 0), 
											pageRect[1] + Math.max(topLeft.y, 0),
											Math.min(bottomRight.x - topLeft.x, bottomRight.x, pageRect[2]),
											Math.min(bottomRight.y - topLeft.y, bottomRight.y, pageRect[3])
										], "Edit", true, [view]);
										view.editInstance(me._selectedLayer.zoomData.bizId, null, null);
										me._selectedLayer.closePopup();
										me._selectedLayer = null;
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
	                layers: eval(SKYVE.Util.mapLayers)
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
