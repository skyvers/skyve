// Override this method so that saving on Chrome when not connected to the internet will work correctly
isc.Offline.isOffline = function() {
	return false;
};
isc.setAutoDraw(false);
isc.RPCManager.fetchDataPrompt = "Contacting Server...";
isc.RPCManager.saveDataPrompt = "Contacting Server...";
isc.RPCManager.removeDataPrompt = "Contacting Server...";
isc.RPCManager.handleError = function (response, request) {
	if (isc.isA.String(response.data)) {
		isc.warn(response.data);
		return false;
	}
	else {
		return this.Super("handleError", arguments);
	}
};
Date.setShortDisplayFormat("toEuropeanShortDate");
Date.setNormalDisplayFormat("toEuropeanShortDate");
Date.setInputFormat("DMY");

// Fix DnD for list grids
isc.ListGrid.addProperties({
	recordDrop: function (dropRecords, targetRecord, index, sourceWidget) {
		if (this.getID() == sourceWidget.getID()) {
			var dupdata = this.data.slice();
			dupdata.slideList(dropRecords, index);
			this.setData(dupdata);
			this.markForRedraw();
		}
		else {
			this.transferRecords(dropRecords, targetRecord, this.canReorderRecords ? index: null, sourceWidget);  
		}  
		
		if (this.recordsDropped) {
			this.recordsDropped(dropRecords, index, this, sourceWidget);
		}
		
		return false;    
	},
	
	getFilterEditorType: function(field) {
	    // Simple case: support explicit filterEditorType on the field
	    if (field.filterEditorType != null) return field.filterEditorType;

	    // TODO: reimplement this once RecordEditor correctly returns AdvancedCriteria
//	    if (isc.SimpleType.inheritsFrom(field.type, "date") && this.getDataSource() && 
//	        this.getDataSource().supportsAdvancedCriteria()) 
//	    {
//	        return "MiniDateRangeItem";
//	    }

	    // filter editor config is basically picked up from field defaults and explicit
	    // field.filterEditorProperties.
	    // If a a field specifies an explicit filterEditorType or a filterEditorProperties block with
	    // an explicit editor type, respect it.
	    // Otherwise if a field specifies an explicit editorType, respect that
	    // Otherwise generate the editor type based on data type in the normal way
	    // A couple of exceptions:
	    // - override canEdit with canFilter, so we don't get a staticTextItem in the field
	    
	    // - clear out field.length: we don't want to show the long editor type (text area) in our
	    //   filter editor
	    var filterEditorConfig = isc.addProperties ({}, field,
	                                                 {canEdit:field.canFilter !== false,
	                                                  length:null});
	    
	    // the _constructor property can come from XML -> JS conversion, and matches the 
	    // XML tag name for the field element.
	    // Don't attempt to use this to determine DynamicForm editor type - it's likely to be
	    // ListGridField or similar which shouldn't effect the generated form item type.
	    if (filterEditorConfig._constructor != null) delete filterEditorConfig._constructor;
	    if (field.filterEditorType != null) filterEditorConfig.editorType = field.filterEditorType;
	    isc.addProperties(filterEditorConfig, field.filterEditorProperties);
	    var type = isc.DynamicForm.getEditorType(filterEditorConfig, this);
	    return type;
	}
});

var resizeTimerEvent = null;

// register for page resize event to allow for resize of window stack
isc.Page.setEvent('resize', function() {
	if (isc.WindowStack) {
		if (resizeTimerEvent) {
			isc.Timer.clearTimeout(resizeTimerEvent);
		}
		resizeTimerEvent = isc.Timer.setTimeout(isc.WindowStack.resize, 50);
	}
});

// register new search operator types for spatial queries
isc.DataSource.addSearchOperator({ID:'gEquals',
	title: 'Equals',
	fieldTypes:['geometry'],
	valueType: 'fieldType',
	requiresServer: false,
	condition: function(value, record, fieldName, criterion, operator) {
		return true;
	},
	compareCriteria: function(newCriterion, oldCriterion) {
		return -1;
	}
});
isc.DataSource.addSearchOperator({ID:'gDisjoint',
	title: 'Disjoint',
	fieldTypes:['geometry'],
	valueType: 'fieldType',
	requiresServer: false,
	condition: function(value, record, fieldName, criterion, operator) {
		return true;
	},
	compareCriteria: function(newCriterion, oldCriterion) {
		return -1;
	}
});
isc.DataSource.addSearchOperator({ID:'gIntersects',
	title: 'Intersects',
	fieldTypes:['geometry'],
	valueType: 'fieldType',
	requiresServer: false,
	condition: function(value, record, fieldName, criterion, operator) {
		return true;
	},
	compareCriteria: function(newCriterion, oldCriterion) {
		return -1;
	}
});
isc.DataSource.addSearchOperator({ID:'gTouches',
	title: 'Touches',
	fieldTypes:['geometry'],
	valueType: 'fieldType',
	requiresServer: false,
	compareCriteria: function(newCriterion, oldCriterion) {
		return -1;
	}});
isc.DataSource.addSearchOperator({ID:'gCrosses',
	title: 'Crosses',
	fieldTypes:['geometry'],
	valueType: 'fieldType',
	requiresServer: false,
	condition: function(value, record, fieldName, criterion, operator) {
		return true;
	},
	compareCriteria: function(newCriterion, oldCriterion) {
		return -1;
	}
});
isc.DataSource.addSearchOperator({ID:'gWithin',
	title: 'Within',
	fieldTypes:['geometry'],
	valueType: 'fieldType',
	requiresServer: false,
	compareCriteria: function(newCriterion, oldCriterion) {
		return -1;
	}
});
isc.DataSource.addSearchOperator({ID:'gContains',
	title: 'Contains',
	fieldTypes:['geometry'],
	valueType: 'fieldType',
	requiresServer: false,
	condition: function(value, record, fieldName, criterion, operator) {
		return true;
	},
	compareCriteria: function(newCriterion, oldCriterion) {
		return -1;
	}
});
isc.DataSource.addSearchOperator({ID:'gOverlaps',
	title: 'Overlaps',
	fieldTypes:['geometry'],
	valueType: 'fieldType',
	requiresServer: false,
	condition: function(value, record, fieldName, criterion, operator) {
		return true;
	},
	compareCriteria: function(newCriterion, oldCriterion) {
		return -1;
	}
});

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
			isc.BizUtil.loadJS('wicket/wicket.js?v=' + isc.BizUtil.version, function() {
				isc.BizUtil.loadJS('wicket/wicket-gmap3.js?v=' + isc.BizUtil.version, function() {
					isc.BizUtil.loadJS('https://maps.googleapis.com/maps/api/js?v=3&libraries=drawing&callback=isc.BizMap.initialise&key=' +
										isc.BizUtil.googleMapsV3ApiKey);
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

// TODO Leave as modals for now and see how error tightening on server goes
//isc.RPCManager.promptStyle = "cursor";

// utility class
isc.defineClass("BizUtil");
isc.BizUtil.addClassProperties({
	// this is the page's URL prefix
	URL_PREFIX: window.location + "",

	// Map of modules loaded -> views -> used & unused views
	_modules: {},
	
	// List of picklists to use (for pick views)
	_unusedPickLists: [],
	
	// Data source for the "previous values" mechanism on text fields.
	PREVIOUS_VALUES_DATA_SOURCE: isc.RestDataSource.create({
		dataFormat: 'json',
		dataURL: "smartprev",
		fields: [{name: 'value', type: 'text'}]
	})
});
isc.BizUtil.URL_PREFIX = isc.BizUtil.URL_PREFIX.substring(0, isc.BizUtil.URL_PREFIX.lastIndexOf("/") + 1);
isc.BizUtil.addClassMethods({
	_currentView: null, // the view currently displayed

	getCurrentView: function() {
		return isc.BizUtil._currentView;
	},

	loadJS: function(scriptPath, callback) {
	    var scriptNode = document.createElement('SCRIPT');
	    scriptNode.type = 'text/javascript';
	    scriptNode.src = scriptPath;

	    if (callback != null) {
		    if (scriptNode.readyState) { // IE, incl. IE9
		    	scriptNode.onreadystatechange = function() {
		    		if (scriptNode.readyState == "loaded" || scriptNode.readyState == "complete") {
		    			scriptNode.onreadystatechange = null;
		    			callback();
		    		}
		    	};
		    } 
		    else { // Other browsers
		    	scriptNode.onload = callback;
		    }
	    }
	    
	    var headNode = document.getElementsByTagName('HEAD');
	    if (headNode[0] != null) {
	        headNode[0].appendChild(scriptNode);
	    }
	},

	// Change something like [{name: 'poo', operator: 'equals', value 'wee'}] filter params to 
	// {poo: 'wee'} request params for the list servlet
	//
	// requestParams - sent to list servlet - this is what is populated
	// filterParams - set usually on the config object of a widget
	// view - the associated view
	addFilterRequestParams: function(requestParams, filterParams, view) {
		var instance = view.gather(false); // no validate
		for (var i = 0, l = filterParams.length; i < l; i++) {
			var filterParam = filterParams[i];
			var value = view.toDisplay(filterParam.value, instance);
			requestParams[filterParam.name] = value;
		}
	},
	
	// Add extra criteria defined in filterParams to the criteria parameter given
	// Add filterParams like [{fileName: 'poo', operator: 'equals', value 'wee'}] to the criteria
	//
	// criteria - simple or advanced criteria object
	// filterParams - set usually on the config object of a widget
	// view - the associated view
	// return - an advanced criteria object with the extra filter parameters and'd
	completeFilterCriteria: function(criteria, filterParams, view) {
		// NB criteria can come through as undefined from SC framework on occasions
		var result = isc.addProperties({}, criteria); // make a defensive copy
		
		// convert simple criteria to advanced criteria
		if (result.operator) {} else {
			result = isc.DataSource.convertCriteria(result, 'substring');
		}

		result = {_constructor: 'AdvancedCriteria', operator: 'and', criteria:[result]};
		 
		var instance = view.gather(false); // no validate
		for (var i = 0, l = filterParams.length; i < l; i++) {
			var filterParam = filterParams[i];
			var value = view.toDisplay(filterParam.value, instance);
			result.criteria.add({fieldName: filterParam.name, operator: filterParam.operator, value: value});
		}

		return result;
	},
	
	// returns an IButton
	createImageButton: function(icon, // src relative to isomorphic directory - use ../images/ etc
								hasDisabledIcon, // true to look for disabled icon ie icon_Disabled
								tooltip, // the tooltip to add to the button
								click) { // function to call when clicked
		return isc.IButton.create({
			width: 24,
			icon: icon,
			iconAlign: "center",
			showDisabledIcon: hasDisabledIcon,
			canHover: true,
			getHoverHTML: function() {return tooltip;},
			click: click
		});
	},
	
	// returns a HLayout that represents a split button
	createSplitButton: function(buttonTitle, // title of main action button
									buttonIcon, // src relative to isomorphic directory - use ../images/ etc
									buttonHasDisabledIcon, // true to look for disabled icon ie icon_Disabled
									buttonTooltip, // the tooltip to add to the button
									buttonClick, // function to call when clicked
									splitTooltip, // the tooltip to add to the split
									splitTarget, // a canvas sent to checkIf() and enableIf() within the splitItems
									splitItems) { // array of MenuItem defns including the click functions
		return isc.HLayout.create({
			height: 22,
			membersMargin: 1,
			members:[
				isc.IButton.create({
					height: 22,
					autoFit: true,
					title: buttonTitle,
					icon: buttonIcon,
					showDisabledIcon: buttonHasDisabledIcon,
					canHover: true,
					getHoverHTML: function() {return buttonTooltip;},
					click: buttonClick
				}),
				isc.MenuButton.create({
					title: '',
					width: 23,
					alignMenuLeft: false,
					canHover: true,
					getHoverHTML: function() {return splitTooltip;},
					menu: isc.Menu.create({
					    autoDraw: false,
					    showShadow: true,
					    shadowDepth: 10,
					    target: splitTarget,
					    data: splitItems
					})
				})
			]
		});
	},
	
	createUploadButton: function(contentFormItem) { // the item this upload button will live in
		return isc.BizUtil.createSplitButton(
			'Upload', 
			null, 
			false, 
			'Upload content', 
			function() {
				var instance = contentFormItem.form._view.gather(false);
				var url = 'contentUpload.xhtml?_n=' + contentFormItem.name.replaceAll('_', '.') + 
							'&_c=' + instance._c;
				if (contentFormItem.form._view._b) {
					url += '&_b=' + contentFormItem.form._view._b.replaceAll('_', '.');
				}
				isc.WindowStack.popup(null,
										"Upload Content",
										true,
										[isc.HTMLPane.create({
											contentsType: 'page',
											contents: 'Loading Page...',
											contentsURL: url
										})]);
			},
			'Other Options', 
			null,
			[{title: 'Clear', 
				icon: "icons/delete.png",
				click: function(event) {
					contentFormItem.setValue(null);
				}}]);
	},
	
	createGeoLocator: function(editView,
								latitudeBinding, 
								longitudeBinding,
								descriptionBinding,
								addressBinding,
								cityBinding,
								stateBinding,
								postcodeBinding,
								countryBinding) {
		return isc.IButton.create({
			height: 22,
			autoFit: true,
			title: "Map",
			canHover: true,
			getHoverHTML: function() {return "Select or search for a map location";},
			click: function() {
				var instance = editView.gather(false);
				var url = isc.BizUtil.URL_PREFIX;
				url += 'pages/map/geolocate.jsp?';
				if (latitudeBinding) {
					var latitudeValue = instance[latitudeBinding];
					url += '_latitude=' + latitudeBinding + '&' + latitudeBinding + '=' + (latitudeValue ? latitudeValue : '') + '&';
				}
				if (longitudeBinding) {
					var longitudeValue = instance[longitudeBinding];
					url += '_longitude=' + longitudeBinding + '&' + longitudeBinding + '=' + (longitudeValue ? longitudeValue : '') + '&';
				}
				if (descriptionBinding) {
					var descriptionValue = instance[descriptionBinding];
					url += '_description=' + descriptionBinding + '&' + descriptionBinding + '=' + (descriptionValue ? descriptionValue : '') + '&';
				}
				if (addressBinding) {
					var addressValue = instance[addressBinding];
					url += '_address=' + addressBinding + '&' + addressBinding + '=' + (addressValue ? addressValue : '') + '&';
				}
				if (cityBinding) {
					var cityValue = instance[cityBinding];
					url += '_city=' + cityBinding + '&' + cityBinding + '=' + (cityValue ? cityValue : '') + '&';
				}
				if (stateBinding) {
					var stateValue = instance[stateBinding];
					url += '_state=' + stateBinding + '&' + stateBinding + '=' + (stateValue ? stateValue : '') + '&';
				}
				if (postcodeBinding) {
					var postcodeValue = instance[postcodeBinding];
					url += '_postcode=' + postcodeBinding + '&' + postcodeBinding + '=' + (postcodeValue ? postcodeValue : '') + '&';
				}
				if (countryBinding) {
					var countryValue = instance[countryBinding];
					url += '_country=' + countryBinding + '&' + countryBinding + '=' + (countryValue ? countryValue : '') + '&';
				}
				
				isc.WindowStack.popup(null,
										"Geo Locate",
										true,
										[isc.HTMLPane.create({
											contentsType: 'page',
											contents: 'Loading Page...',
											contentsURL: url
										})]);
			}
		});
	},
	
	// returns an edit view
	getEditView: function(moduleName, 
							documentName,
							onViewCreated) { // function with view as an argument
		// place a module cache entry
		if (isc.BizUtil._modules[moduleName]) {
			// do nothing - already exists
		}
		else {
			isc.BizUtil._modules[moduleName] = {};
			window[moduleName] = {};
		}
		
		var view = null;
		
		// get one off the unused list
		var documentEntry = isc.BizUtil._modules[moduleName][documentName];
		if (documentEntry) { // have a document entry
			// grab an unused view if available
			view = documentEntry._unused.pop();

			if (view) { // there was an unused one
				// do nothing - already taken unused one off
			}
			else { // no unused one
				view = eval(moduleName + '.create' + documentName + '()');
				view._moduleName = moduleName;
				view._documentName = documentName;
			}
			// put this one on the used list
			isc.BizUtil._modules[moduleName][documentName]._used.push(view);
			onViewCreated(view);
		}
		else {
			isc.RPCManager.sendRequest({
				showPrompt: true,
				evalResult: true,
				actionURL: isc.BizUtil.URL_PREFIX + "smartgen" + "?_mod=" + moduleName  + "&_doc=" + documentName,
				callback: function(rpcResponse, data, rpcRequest) {
					// create the document entry structure
					isc.BizUtil._modules[moduleName][documentName] = {};
					isc.BizUtil._modules[moduleName][documentName]._used = []; // list of views in use
					isc.BizUtil._modules[moduleName][documentName]._unused = []; // list of views created but not in use

					// do a recursive call now that we have loaded the necessary javascript
					isc.BizUtil.getEditView(moduleName, documentName, onViewCreated);
				}
			 });
		}
	},
	
	relinquishEditView: function(view) {
		var documentEntry = isc.BizUtil._modules[view._moduleName][view._documentName];
		documentEntry._used.remove(view);
		documentEntry._unused.push(view);
	},
	
	getPickList: function(lookupDescription, filterParams, view) {
		var result = isc.BizUtil._unusedPickLists.pop();
		if (result) {
		}
		else {
			result = isc.BizListGrid.create({isPickList: true});
		}
		result.setLookup(lookupDescription, filterParams, view);
		
		return result;
	},
	
	// put the pickList back onto the stack
	relinquishPickList: function(pickList) {
// TODO - why arent the fields defined when I used a cached BizListGrid
//		isc.BizUtil._unusedPickLists.push(pickList);
		pickList.destroy();
	},
	
	createListGrid: function() {
		return isc.BizListGrid.create({margin: 2});
	},
	
	createCalendar: function() {
		return isc.Calendar.create({width: '100%',
									height: '100%',
									scrollToWorkDay: true,
									data: []});
	},
	
	createTreeGrid: function() {
		return isc.BizListGrid.create({margin: 2, isTree: true});
	},
	
	createMap: function() {
		return isc.BizMap.create();
	},
	
	popupFrame: function(url, name, width, height) {
		var win = window.open(url,
								name, 
								'width=' + width + ',height=' + height + ',resizable=yes,scrollbars=no,toolbar=no,location=no,directories=no,status=yes,menubar=no,copyhistory=no'); 
		win.focus();
	},
	
	growl: function(msgs, // [{severity:'info/warn/error/fatal', summary:'summary', detail:'detail'}]
						life, // number of millis
						sticky) { // true/false
		PrimeFaces.cw('Growl', 'growl', {
			id: 'growl', 
			widgetVar: 'growl', 
			life: (life ? life : 6000), 
			sticky: (sticky ? sticky : false), 
			msgs: msgs 
		});
	}
});
