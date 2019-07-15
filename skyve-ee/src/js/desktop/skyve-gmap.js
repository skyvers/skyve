isc.ClassFactory.defineClass("BizMap", "Canvas");
isc.BizMap.addClassMethods({
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
			SKYVE.Util.loadJS('wicket/wicket.js?v=' + SKYVE.Util.v, function() {
				SKYVE.Util.loadJS('wicket/wicket-gmap3.js?v=' + SKYVE.Util.v, function() {
					if (SKYVE.Util.googleMapsV3ApiKey) {
						SKYVE.Util.loadJS('https://maps.googleapis.com/maps/api/js?v=3&libraries=drawing&callback=isc.BizMap.initialise&key=' +
											SKYVE.Util.googleMapsV3ApiKey);
					}
					else {
						SKYVE.Util.loadJS('https://maps.googleapis.com/maps/api/js?v=3&libraries=drawing&callback=isc.BizMap.initialise');
					}
				});
			});
			return this.Super('draw', arguments);
		}
	},

	setDataSource: function(dataSourceID) {
		if (window.google && window.google.maps && this._map) {
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

			if (this._map) {
				mapOptions.zoom = this._map.getZoom();
				mapOptions.center = this._map.getCenter();
				mapOptions.mapTypeId = this._map.getMapTypeId();
			}

			this._infoWindow = new google.maps.InfoWindow({content: ''});

/* TODO reinstate
			var control = document.createElement('DIV');
			control.id = this.ID + '_form';
			control.style.width = '300px';
*/
			this._map = new google.maps.Map(document.getElementById(this.ID + '_map'), mapOptions);
/* TODO reinstate
			this._map.controls[google.maps.ControlPosition.TOP].push(control);
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
		var url = isc.BizUtil.URL_PREFIX + 'map?';
/*
		// set the map bounds
		var bounds = wkt.write(this._map.getBounds());
		alert(bounds)
		var nw = new google.maps.LatLng(
        	    this._map.getBounds().getNorthEast().lat(),
        	    this._map.getBounds().getSouthWest().lng()
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
				// ensure that only 1 refresh at a time occurs
				// NB switch this off first thing in case there is an error in the code below
				me._refreshing = false;

				var items = data.items;
				
				if (auto) {
					// remove overlays not present in the data
					for (var bizId in me._objects) {
						if (! items.containsProperty('bizId', bizId)) {
							var deletedObject = me._objects[bizId];
							for (var i = 0, l = deletedObject.overlays.length; i < l; i++) {
								deletedObject.overlays[i].setMap(null);
								deletedObject.overlays[i] = null;
							}
							delete deletedObject['overlays'];
							delete me._objects[bizId];
						}
					}
				}
				else {
					// remove all overlays
					for (var bizId in me._objects) {
						var deletedObject = me._objects[bizId];
						for (var i = 0, l = deletedObject.overlays.length; i < l; i++) {
							deletedObject.overlays[i].setMap(null);
							deletedObject.overlays[i] = null;
						}
						delete deletedObject['overlays'];
						delete me._objects[bizId];
					}
				}

				// add/update overlays from the data
				for (var i = 0, l = items.length; i < l; i++) {
					var item = items[i];

					var object = me._objects[item.bizId];
					if (object) {
						// if the wkts have changed delete the overlay and recreate it
						var same = (object.overlays.length == item.features.length);
						if (same) {
							for (var j = 0, m = object.overlays.length; j < m; j++) {
								if (object.overlays[j].geometry !== item.features[j].geometry) {
									same = false;
									break;
								}
							}
						}
						if (! same) {
							for (var j = 0, m = object.overlays.length; j < m; j++) {
								object.overlays[j].setMap(null);
								object.overlays[j] = null;
							}
							delete object['overlays'];
							delete me._objects[bizId];
							object = null;
						}
					}
					if (object) {} else {
						object = {overlays: []};
						for (var j = 0, m = item.features.length; j < m; j++) {
							var feature = item.features[j];

							try { // Catch any malformed WKT strings
					        	wkt.read(feature.geometry);
					        }
					        catch (e) {
					            if (e.name === 'WKTError') {
					                alert(feature.geometry + ' is invalid WKT.');
					                continue;
					            }
					        }
					        var props = {editable: feature.editable};
					        if (feature.strokeColour) {
					        	props.strokeColor = feature.strokeColour;
					        }
					        if (feature.fillColour) {
					        	props.fillColor = feature.fillColour;
					        }
					        if (feature.fillOpacity) {
					        	props.fillOpacity = feature.fillOpacity;
					        }
					        if (feature.iconDynamicImageName) {
					        	props.icon = {url: 'image?_n=' + feature.iconDynamicImageName};
					        	if (feature.iconAnchorX && feature.iconAnchorY) {
					        		props.icon.anchor = new google.maps.Point(feature.iconAnchorX, feature.iconAnchorY);
					        		props.icon.origin = new google.maps.Point(0,0);
					        	}
					        }
					        
					        var overlay = wkt.toObject(props);
					        object.overlays.push(overlay);
		                	overlay.setMap(me._map);

		                	if (feature.zoomable) { // can show the info window for zooming
			                	overlay.bizId = item.bizId;
			                	overlay.geometry = feature.geometry;
					            overlay.fromTimestamp = item.fromTimestamp;
					            overlay.toTimestamp = item.toTimestamp;
					            overlay.photoId = item.photoId;
					            overlay.mod = item.moduleName;
					            overlay.doc = item.documentName;
					            overlay.infoMarkup = item.infoMarkup;
						        google.maps.event.addListener(overlay, 'click', function(event) {
						        	var contents = this.infoMarkup;
						        	contents += '<p/><input type="button" value="Zoom" onclick="' + me.ID + '.zoom(';
						        	if (this.getPosition) {
						        		var p = this.getPosition();
						        		contents += p.lat() + ',' + p.lng() + "," + p.lat() + ',' + p.lng() + ",'"; 
										contents += this.mod + "','" + this.doc + "','" + this.bizId + "')\"/>";
						        		
										me._infoWindow.open(me._map, this);
						        		me._infoWindow.setContent(contents);
						        	}
						        	else if (this.getPath) {
										var bounds = new google.maps.LatLngBounds();
										var path = this.getPath();
										for (var k = 0, n = path.getLength(); k < n; k++) {
											bounds.extend(path.getAt(k));
										}
										var ne = bounds.getNorthEast();
										var sw = bounds.getSouthWest();
			        					
										contents += ne.lat() + ',' + sw.lng() + "," + sw.lat() + ',' + ne.lng() + ",'";
										contents += this.mod + "','" + this.doc + "','" + this.bizId + "')\"/>";

										me._infoWindow.setPosition(event.latLng);
						        		me._infoWindow.open(me._map);
						        		me._infoWindow.setContent(contents);
						        	}
						        });
					        }
						}
				        
//				        if (Wkt.isArray(overlay)) { // Distinguish multigeometries (Arrays) from objects
//				        	for (i in obj) {
//				                if (obj.hasOwnProperty(i) && ! Wkt.isArray(obj[i])) {
//				                	obj[i].bizId = datum.bizId;
//				                	obj[i].setMap(me._map);
//									me._objects[obj[i].bizId] = obj[i];
//				                }
//				            }
//				        }
//				        else {
//				            obj.setMap(me._map); // Add it to the map
//				            this._objects.push(obj);
//				        }
//
//						overlay = new google.maps.Marker({
//							bizId: datum.bizId,
//							position: latlng,
//				            map: me._map
//				        });
						me._objects[item.bizId] = object;
					}
				}

				if (fit) {
					var bounds = new google.maps.LatLngBounds();
					var someOverlays = false;
					for (var id in me._objects) {
						someOverlays = true;
						var object = me._objects[id];
						var overlays = object.overlays;
						for (var i = 0, l = overlays.length; i < l; i++) {
							var overlay = overlays[i];
				            if (overlay.getPath) {
					            // For Polygons and Polylines - fit the bounds to the vertices
								var path = overlay.getPath();
								for (var j = 0, m = path.getLength(); j < m; j++) {
									bounds.extend(path.getAt(j));
								}
				            }
				            else if (overlay.getPosition) {
				            	bounds.extend(overlay.getPosition());
				            }
						}
					}

					if (someOverlays) {
						// Don't zoom in too far on only one marker
					    if (bounds.getNorthEast().equals(bounds.getSouthWest())) {
					       var extendPoint1 = new google.maps.LatLng(bounds.getNorthEast().lat() + 0.01, bounds.getNorthEast().lng() + 0.01);
					       var extendPoint2 = new google.maps.LatLng(bounds.getNorthEast().lat() - 0.01, bounds.getNorthEast().lng() - 0.01);
					       bounds.extend(extendPoint1);
					       bounds.extend(extendPoint2);
					    }

						me._map.fitBounds(bounds);
					}
				}
			}
		});
	},
	
	zoom: function(topLeftLat, topLeftLng, bottomRightLat, bottomRightLng, bizModule, bizDocument, bizId) {
		this._zoomed = true; // indicates that we don't want refreshes as we are zoomed on an overlay
		
		var scale = Math.pow(2, this._map.getZoom());
    	var nw = new google.maps.LatLng(
    	    this._map.getBounds().getNorthEast().lat(),
    	    this._map.getBounds().getSouthWest().lng()
    	);
    	var worldCoordinateNW = this._map.getProjection().fromLatLngToPoint(nw);
    	var topLeftPosition = new google.maps.LatLng(topLeftLat, topLeftLng);
    	var topLeftWorldCoordinate = this._map.getProjection().fromLatLngToPoint(topLeftPosition);
    	var bottomRightPosition = new google.maps.LatLng(bottomRightLat, bottomRightLng);
    	var bottomRightWorldCoordinate = this._map.getProjection().fromLatLngToPoint(bottomRightPosition);

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
										me._infoWindow.close();
									});

	}
});
