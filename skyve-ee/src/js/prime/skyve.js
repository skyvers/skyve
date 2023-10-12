// SKYVE name space definition
SKYVE = {};

SKYVE.Util = function() {
	var context = window.location + '';
	context = context.substring(0, context.lastIndexOf("/") + 1);
	
	var load = function(node, callback) {
	    if (callback != null) {
		    if (node.readyState) { // IE, incl. IE9
		    	node.onreadystatechange = function() {
		    		if (node.readyState == "loaded" || node.readyState == "complete") {
		    			node.onreadystatechange = null;
		    			callback();
		    		}
		    	};
		    } 
		    else { // Other browsers
		    	node.onload = callback;
		    }
	    }
	    
	    var headNode = document.getElementsByTagName('HEAD');
	    if (headNode[0] != null) {
	        headNode[0].appendChild(node);
	    }
	};
	
	// public methods
	return {
		customer: null,
		v: null,
		googleMapsV3ApiKey: null,
		ckEditorConfigFileUrl: null,
		mapCentre: null,
		mapZoom: 1,
		CONTEXT_URL: context,
		allowedReportFormats: null,
		
		loadJS: function(scriptPath, callback) {
		    var scriptNode = document.createElement('SCRIPT');
		    scriptNode.type = 'text/javascript';
		    scriptNode.src = scriptPath;
		    load(scriptNode, callback);
		},
		
		loadCSS: function(cssPath, callback) {
	        var cssNode = document.createElement('LINK');
	        cssNode.type = 'text/css';
	        cssNode.rel = 'stylesheet';
	        cssNode.href = cssPath;
		    load(cssNode, callback);
		},

	    geoLocate: function(callback) { // a function that takes the wktString of the geolocation
	    	if (navigator.geolocation) {
		    	$(function(){PrimeFaces.cw("Growl","growl",{id:"growl",widgetVar:"growl",msgs:[{summary:'GeoLocating', detail: 'Please wait...', severity: 'info'}]});});
	            navigator.geolocation.getCurrentPosition(
	                function(position) {
				    	$(function(){PrimeFaces.cw("Growl","growl",{id:"growl",widgetVar:"growl",msgs:[{summary:'GeoLocating', detail: 'Done', severity: 'info'}]});});
	                	callback('POINT (' + position.coords.longitude + ' ' + position.coords.latitude + ')');
	                },
	                function(error) {
				    	$(function(){PrimeFaces.cw("Growl","growl",{id:"growl",widgetVar:"growl",msgs:[{summary:'GeoLocating', detail: error.message, severity: 'warn'}]});});
	                },
	                {enableHighAccuracy: true
	            });
	    	}
        },
        
        setTouchCookie: function() {
        	var hasTouchScreen = false;
        	if ('maxTouchPoints' in navigator) { 
        	    hasTouchScreen = navigator.maxTouchPoints > 0;
        	}
        	else if ('msMaxTouchPoints' in navigator) {
        	    hasTouchScreen = navigator.msMaxTouchPoints > 0; 
        	}
        	else {
        	    var mQ = window.matchMedia && matchMedia('(pointer:coarse)');
        	    if (mQ && mQ.media === '(pointer:coarse)') {
        	        hasTouchScreen = !!mQ.matches;
        	    }
        	    else if ('orientation' in window) {
        	        hasTouchScreen = true; // deprecated, but good fallback
        	    }
        	}
        	if (hasTouchScreen) {
        		document.cookie = "touch=1; path=/";
        	}
        	else {
        		document.cookie = "touch=0; path=/";
        	}
        }
	}
}();

SKYVE.GMap = function() {
	var wkt = null;

	var drawingModes = function(drawingTools) { // the drawing tools specified on the geometryMap widget
		var result = null;
		if (drawingTools == 'point') {
			result = [google.maps.drawing.OverlayType.MARKER];
		}
		else if (drawingTools == 'line') {
			result = [google.maps.drawing.OverlayType.POLYLINE];
		}
		else if (drawingTools == 'polygon') {
			result = [google.maps.drawing.OverlayType.POLYGON, google.maps.drawing.OverlayType.RECTANGLE];
		}
		else if (drawingTools == 'pointAndLine') {
			result = [google.maps.drawing.OverlayType.MARKER, google.maps.drawing.OverlayType.POLYLINE];
		}
		else if (drawingTools == 'pointAndPolygon') {
			result = [google.maps.drawing.OverlayType.MARKER, google.maps.drawing.OverlayType.POLYGON, google.maps.drawing.OverlayType.RECTANGLE];
		}
		else if (drawingTools == 'lineAndPolygon') {
			result = [google.maps.drawing.OverlayType.POLYLINE, google.maps.drawing.OverlayType.POLYGON, google.maps.drawing.OverlayType.RECTANGLE];
		}
		else {
			result = [google.maps.drawing.OverlayType.MARKER, 
						google.maps.drawing.OverlayType.POLYLINE,
						google.maps.drawing.OverlayType.POLYGON,
						google.maps.drawing.OverlayType.RECTANGLE];
		}
		return result;
    };

	// public methods
	return {
		scatter: function(display, // the display object that holds the map and other state variables
							data, // the response from the map servlet to scatter
							fit, // fit bounds
							delta) { // only remove or update if changed
			// instantiate WKT if it hasn't been already (at this point Wkt script is loaded)
			if (! wkt) {
				wkt = new Wkt.Wkt()
			}
			
			var items = data.items;
			// if there is no data, there is probably an error, so just bug out
			if (! items) {
				return;
			}
			
			if (delta) {
				// remove overlays not present in the data
				for (var bizId in display._objects) {
					var found = false;
					for (var i = 0, l = items.length; i < l; i++) {
						if (items[i].bizId === bizId) {
							found = true;
							break;
						}
					}
					if (! found) {
						var deletedObject = display._objects[bizId];
						for (var i = 0, l = deletedObject.overlays.length; i < l; i++) {
							deletedObject.overlays[i].setMap(null);
							deletedObject.overlays[i] = null;
						}
						delete deletedObject['overlays'];
						delete display._objects[bizId];
					}
				}
			}
			else {
				// remove all overlays
				for (var bizId in display._objects) {
					var deletedObject = display._objects[bizId];
					for (var i = 0, l = deletedObject.overlays.length; i < l; i++) {
						deletedObject.overlays[i].setMap(null);
						deletedObject.overlays[i] = null;
					}
					delete deletedObject['overlays'];
					delete display._objects[bizId];
				}
			}

			// add/update overlays from the data
			for (var i = 0, l = items.length; i < l; i++) {
				var item = items[i];

				var object = display._objects[item.bizId];
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
						delete display._objects[bizId];
						object = null;
					}
				}
				if (! object) { // object could have been nulled just above 
					object = {overlays: []};
					for (var j = 0, m = item.features.length; j < m; j++) {
						var feature = item.features[j];

						try { // Catch any malformed WKT strings
				        	wkt.read(feature.geometry);
				        }
				        catch (e) {
			                alert(feature.geometry + ' is invalid WKT.');
			                continue;
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
				        if (feature.iconRelativeFilePath) {
				        	props.icon = {url: SKYVE.Util.CONTEXT_URL + 
				        							'resources?_n=' + feature.iconRelativeFilePath + 
				        							'&_doc=' + data._doc};
				        	if (feature.iconAnchorX && feature.iconAnchorY) {
				        		props.icon.anchor = new google.maps.Point(feature.iconAnchorX, feature.iconAnchorY);
				        		props.icon.origin = new google.maps.Point(0,0);
				        	}
				        }
				        
				        var overlay = wkt.toObject(props);
				        object.overlays.push(overlay);
	                	overlay.setMap(display.webmap);

	                	if (feature.zoomable) { // can show the info window for zooming
	                		overlay.bizId = item.bizId;
		                	overlay.geometry = feature.geometry;
				            overlay.fromTimestamp = item.fromTimestamp;
				            overlay.toTimestamp = item.toTimestamp;
				            overlay.mod = item.moduleName;
				            overlay.doc = item.documentName;
				            overlay.infoMarkup = item.infoMarkup;
					        google.maps.event.addListener(overlay, 'click', function(event) {
					        	display.click(this, event);
					        });
				        }
					}
					display._objects[item.bizId] = object;
				}
			}

			if (fit) {
				var bounds = new google.maps.LatLngBounds();
				var someOverlays = false;
				for (var id in display._objects) {
					someOverlays = true;
					var object = display._objects[id];
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
		                if (display.webmap.getZoom() < 15) {
		                    display.webmap.setZoom(15);
		                }
		            	if (overlay.getPosition !== undefined && typeof overlay.getPosition === 'function') {
		            		display.webmap.setCenter(bounds.getNorthEast());
		                }
				    }
				    else {
				    	display.webmap.fitBounds(bounds);
				    }
				}
			}
		},

		centre: function() {
			// instantiate WKT if it hasn't been already (at this point Wkt script is loaded)
			if (! wkt) {
				wkt = new Wkt.Wkt()
			}

			var result = null;
			if (SKYVE.Util.mapCentre) {
				try { // Catch any malformed WKT strings
		        	wkt.read(SKYVE.Util.mapCentre);
		        	var coord = wkt.components[0];
		        	result = new google.maps.LatLng(coord.y, coord.x);
				}
		        catch (e) {
		        	console.log(SKYVE.Util.mapCentre + " is malformed WKT Point format");
		        	SKYVE.Util.mapCentre = null;
		        	result = new google.maps.LatLng(0, 0);
		        }
			}
			else {
	        	result = new google.maps.LatLng(0, 0);
			}
			return result;
		},
		
	    scatterValue: function(display, // the display object that holds the map and other state variables
	    						value) { // the WKT string value to display
			// instantiate WKT if it hasn't been already (at this point Wkt script is loaded)
			if (! wkt) {
				wkt = new Wkt.Wkt()
			}

			if (! value) {
				return;
			}

	        try { // Catch any malformed WKT strings
	        	wkt.read(value);
	        }
	        catch (e) {
	            if (e.name === 'WKTError') {
	                alert('The WKT string is invalid.');
	                return;
	            }
	        }

	        var obj = wkt.toObject(display.webmap.defaults);
	        
	        if (wkt.type === 'polygon' || wkt.type === 'linestring') {
	        }
			else {
	            if (obj.setEditable) {
	            	obj.setEditable(false);
            	}
	        }

	        if (Wkt.isArray(obj)) { // Distinguish multigeometries (Arrays) from objects
	        	for (i in obj) {
	                if (obj.hasOwnProperty(i) && (! Wkt.isArray(obj[i]))) {
	                    obj[i].setMap(display.webmap);
	                    display._overlays.push(obj[i]);
	                }
	            }
	        }
	        else {
	            obj.setMap(display.webmap); // Add it to the map
	            display._overlays.push(obj);
	        }

	        // Pan the map to the feature
	        if (obj.getBounds !== undefined && typeof obj.getBounds === 'function') {
	        	// For objects that have defined bounds or a way to get them
	            display.webmap.fitBounds(obj.getBounds());
	        }
	        else {
	            if (obj.getPath !== undefined && typeof obj.getPath === 'function') {
		            // For Polygons and Polylines - fit the bounds to the vertices
					var bounds = new google.maps.LatLngBounds();
					var path = obj.getPath();
					for (var i = 0, l = path.getLength(); i < l; i++) {
						bounds.extend(path.getAt(i));
					}
					display.webmap.fitBounds(bounds);
	            }
	            else { // But points (Markers) are different
	                if (display.webmap.getZoom() < 15) {
	                    display.webmap.setZoom(15);
	                }
	            	if (obj.getPosition !== undefined && typeof obj.getPosition === 'function') {
	            		display.webmap.setCenter(obj.getPosition());
	                }
	            }
	        }
	    },

	    clear: function(display) { // the display object that holds the map and other state variables
	        for (var i = 0, l = display._overlays.length; i < l; i++) {
	            display._overlays[i].setMap(null);
	        }
	        display._overlays.length = 0;
	    },
	    
	    drawingTools: function(display) {
			// instantiate WKT if it hasn't been already (at this point Wkt script is loaded)
			if (! wkt) {
				wkt = new Wkt.Wkt()
			}

			display.webmap.drawingManager = new google.maps.drawing.DrawingManager({
            	drawingControlOptions: {
                    position: google.maps.ControlPosition.LEFT_BOTTOM,
                    drawingModes: drawingModes(display.drawingTools)
                },
            });
            display.webmap.drawingManager.setMap(display.webmap);

            google.maps.event.addListener(display.webmap.drawingManager, 'overlaycomplete', function(event) {
            	SKYVE.GMap.clear(display);

                // Set the drawing mode to "pan" (the hand) so users can immediately edit
            	this.setDrawingMode(null);

                display._overlays.push(event.overlay);
                wkt.fromObject(event.overlay);
                var wktValue = wkt.write();
                display.setFieldValue(wktValue);
            });
	    },
	    
	    geoLocator: function(display) {
            if (navigator.geolocation) {
        		var control = document.createElement('DIV');
				control.style.backgroundColor = '#fff';
				control.style.border = '2px solid #fff';
				control.style.borderRadius = '3px';
				control.style.boxShadow = '0 2px 6px rgba(0,0,0,.3)';
				control.style.cursor = 'pointer';
				control.style.margin = '10px';
				control.style.padding = '5px';
				control.style.textAlign = 'center';
				control.title = 'Click to set your current position from your GPS';
				control.innerHTML = '<i class="fa fa-dot-circle-o"></i>';
				control.index = 1;
				control.addEventListener('click', function() {
					navigator.geolocation.getCurrentPosition(
						function(position) {
					    	SKYVE.GMap.clear(display);
					        // Set the drawing mode to "pan" (the hand) so users can immediately edit
					    	display.webmap.setDrawingMode(null);
					    	var position = {lat: position.coords.latitude, lng: position.coords.longitude};
					    	var marker = new google.maps.Marker({
					        	position: position,
					            map: display.webmap
					        });
			                display._overlays.push(marker);
					    	display.setFieldValue('POINT (' + position.coords.longitude + ' ' + position.coords.latitude + ')');
					        display.webmap.setZoom(15);
							display.webmap.setCenter(position);
						},
						function(error) {
							alert(error.message);
						},
						{enableHighAccuracy : true
					});
				});
				display.webmap.geolocator = control;
				display.webmap.controls[google.maps.ControlPosition.LEFT].push(control);
            }
	    },
	    
	    setDisabled: function(display, disabled) {
	    	if (display.webmap) {
		    	if (display.webmap.drawingManager) {
		    		display.webmap.drawingManager.setOptions({
		    			drawingControl: (! disabled)
		    		});
		    	}
		    	if (display.webmap.geolocator) {
		    		display.webmap.geolocator.style.display = disabled ? 'none' : 'block';
		    	}
	    	}
	    },

	    refreshControls: function(display) {
			var control = document.createElement('DIV');
			control.style.backgroundColor = '#fff';
			control.style.border = '2px solid #fff';
			control.style.borderRadius = '3px';
			control.style.boxShadow = '0 2px 6px rgba(0,0,0,.3)';
			control.style.cursor = 'pointer';
			control.style.margin = '10px';
			control.style.padding = '5px';
			control.style.textAlign = 'center';
			control.title = 'Click to set the refresh rate of the map';
			control.innerHTML = '<input type="number" min="0" max="500" step="1" value="' + display.refreshTime + '" size="3" />' +
									'<input type="checkbox"' + (display._refreshRequired ? ' checked' : '') + '><label>Refresh</label>';
			control.index = 1;
			control.children[0].addEventListener('change', function() {
				display.refreshTime = this.value;
				if (display._refreshRequired) {
					if (display._intervalId) {
						clearInterval(display._intervalId);
					}
					display._intervalId = setInterval(display.rerender.bind(display), display.refreshTime * 1000);
				}
			});
			control.children[1].addEventListener('click', function() {
				display._refreshRequired = this.checked;
				if (display._intervalId) {
					clearInterval(display._intervalId);
					display._intervalId = null;
				}
				if (display._refreshRequired) {
					display.rerender();
					display._intervalId = setInterval(display.rerender.bind(display), display.refreshTime * 1000);
				}
			});

			display.webmap.controls[google.maps.ControlPosition.LEFT_BOTTOM].push(control);
	    }
	}
}();

SKYVE.Leaflet = function() {
	/*
	 * The following is from https://github.com/mapbox/wellknown
	 */

	var numberRegexp = /[-+]?([0-9]*\.[0-9]+|[0-9]+)([eE][-+]?[0-9]+)?/;
	// Matches sequences like '100 100' or '100 100 100'.
	var tuples = new RegExp('^' + numberRegexp.source + '(\\s' + numberRegexp.source + '){1,}');

	/*
	 * Parse WKT and return GeoJSON.
	 *
	 * @param {string} _ A WKT geometry
	 * @return {?Object} A GeoJSON geometry object
	 */
	function parse (input) {
	  var parts = input.split(';');
	  var _ = parts.pop();
	  var srid = (parts.shift() || '').split('=').pop();

	  var i = 0;

	  function $ (re) {
	    var match = _.substring(i).match(re);
	    if (!match) return null;
	    else {
	      i += match[0].length;
	      return match[0];
	    }
	  }

	  function crs (obj) {
	    if (obj && srid.match(/\d+/)) {
	      obj.crs = {
	        type: 'name',
	        properties: {
	          name: 'urn:ogc:def:crs:EPSG::' + srid
	        }
	      };
	    }

	    return obj;
	  }

	  function white () { $(/^\s*/); }

	  function multicoords () {
	    white();
	    var depth = 0;
	    var rings = [];
	    var stack = [rings];
	    var pointer = rings;
	    var elem;

	    while (elem =
	           $(/^(\()/) ||
	             $(/^(\))/) ||
	               $(/^(,)/) ||
	                 $(tuples)) {
	      if (elem === '(') {
	        stack.push(pointer);
	        pointer = [];
	        stack[stack.length - 1].push(pointer);
	        depth++;
	      } else if (elem === ')') {
	        // For the case: Polygon(), ...
	        if (pointer.length === 0) return null;

	        pointer = stack.pop();
	        // the stack was empty, input was malformed
	        if (!pointer) return null;
	        depth--;
	        if (depth === 0) break;
	      } else if (elem === ',') {
	        pointer = [];
	        stack[stack.length - 1].push(pointer);
	      } else if (!elem.split(/\s/g).some(isNaN)) {
	        Array.prototype.push.apply(pointer, elem.split(/\s/g).map(parseFloat));
	      } else {
	        return null;
	      }
	      white();
	    }

	    if (depth !== 0) return null;

	    return rings;
	  }

	  function coords () {
	    var list = [];
	    var item;
	    var pt;
	    while (pt =
	           $(tuples) ||
	             $(/^(,)/)) {
	      if (pt === ',') {
	        list.push(item);
	        item = [];
	      } else if (!pt.split(/\s/g).some(isNaN)) {
	        if (!item) item = [];
	        Array.prototype.push.apply(item, pt.split(/\s/g).map(parseFloat));
	      }
	      white();
	    }

	    if (item) list.push(item);
	    else return null;

	    return list.length ? list : null;
	  }

	  function point () {
	    if (!$(/^(point(\sz)?)/i)) return null;
	    white();
	    if (!$(/^(\()/)) return null;
	    var c = coords();
	    if (!c) return null;
	    white();
	    if (!$(/^(\))/)) return null;
	    return {
	      type: 'Point',
	      coordinates: c[0]
	    };
	  }

	  function multipoint () {
	    if (!$(/^(multipoint)/i)) return null;
	    white();
	    var newCoordsFormat = _
	      .substring(_.indexOf('(') + 1, _.length - 1)
	      .replace(/\(/g, '')
	      .replace(/\)/g, '');
	    _ = 'MULTIPOINT (' + newCoordsFormat + ')';
	    var c = multicoords();
	    if (!c) return null;
	    white();
	    return {
	      type: 'MultiPoint',
	      coordinates: c
	    };
	  }

	  function multilinestring () {
	    if (!$(/^(multilinestring)/i)) return null;
	    white();
	    var c = multicoords();
	    if (!c) return null;
	    white();
	    return {
	      type: 'MultiLineString',
	      coordinates: c
	    };
	  }

	  function linestring () {
	    if (!$(/^(linestring(\sz)?)/i)) return null;
	    white();
	    if (!$(/^(\()/)) return null;
	    var c = coords();
	    if (!c) return null;
	    if (!$(/^(\))/)) return null;
	    return {
	      type: 'LineString',
	      coordinates: c
	    };
	  }

	  function polygon () {
	    if (!$(/^(polygon(\sz)?)/i)) return null;
	    white();
	    var c = multicoords();
	    if (!c) return null;
	    return {
	      type: 'Polygon',
	      coordinates: c
	    };
	  }

	  function multipolygon () {
	    if (!$(/^(multipolygon)/i)) return null;
	    white();
	    var c = multicoords();
	    if (!c) return null;
	    return {
	      type: 'MultiPolygon',
	      coordinates: c
	    };
	  }

	  function geometrycollection () {
	    var geometries = [];
	    var geometry;

	    if (!$(/^(geometrycollection)/i)) return null;
	    white();

	    if (!$(/^(\()/)) return null;
	    while (geometry = root()) {
	      geometries.push(geometry);
	      white();
	      $(/^(,)/);
	      white();
	    }
	    if (!$(/^(\))/)) return null;

	    return {
	      type: 'GeometryCollection',
	      geometries: geometries
	    };
	  }

	  function root () {
	    return point() ||
	      linestring() ||
	      polygon() ||
	      multipoint() ||
	      multilinestring() ||
	      multipolygon() ||
	      geometrycollection();
	  }

	  return crs(root());
	}

	/**
	 * Stringifies a GeoJSON object into WKT
	 */
	function stringify (gj) {
	  if (gj.type === 'Feature') {
	    gj = gj.geometry;
	  }

	  function pairWKT (c) {
	    return c.join(' ');
	  }

	  function ringWKT (r) {
	    return r.map(pairWKT).join(', ');
	  }

	  function ringsWKT (r) {
	    return r.map(ringWKT).map(wrapParens).join(', ');
	  }

	  function multiRingsWKT (r) {
	    return r.map(ringsWKT).map(wrapParens).join(', ');
	  }

	  function wrapParens (s) { return '(' + s + ')'; }

	  switch (gj.type) {
	    case 'Point':
	      return 'POINT (' + pairWKT(gj.coordinates) + ')';
	    case 'LineString':
	      return 'LINESTRING (' + ringWKT(gj.coordinates) + ')';
	    case 'Polygon':
	      return 'POLYGON (' + ringsWKT(gj.coordinates) + ')';
	    case 'MultiPoint':
	      return 'MULTIPOINT (' + ringWKT(gj.coordinates) + ')';
	    case 'MultiPolygon':
	      return 'MULTIPOLYGON (' + multiRingsWKT(gj.coordinates) + ')';
	    case 'MultiLineString':
	      return 'MULTILINESTRING (' + ringsWKT(gj.coordinates) + ')';
	    case 'GeometryCollection':
	      return 'GEOMETRYCOLLECTION (' + gj.geometries.map(stringify).join(', ') + ')';
	    default:
	      throw new Error('stringify requires a valid GeoJSON Feature or geometry object as input');
	  }
	};
	/*
	 * The above is from https://github.com/mapbox/wellknown
	 */

	// public methods
	return {
		scatter: function(display, // the display object that holds the map and other state variables
							data, // the response from the map servlet to scatter
							fit, // fit bounds
							delta) { // only remove or update if changed
			var items = data.items;
			// if there is no data, there is probably an error, so just bug out
			if (! items) {
				return;
			}
			
			if (delta) {
				// remove overlays not present in the data
				for (var bizId in display._objects) {
					var found = false;
					for (var i = 0, l = items.length; i < l; i++) {
						if (items[i].bizId === bizId) {
							found = true;
							break;
						}
					}
					if (! found) {
						var deletedObject = display._objects[bizId];
						for (var i = 0, l = deletedObject.overlays.length; i < l; i++) {
							display.webmap.removeLayer(deletedObject.overlays[i]);
							deletedObject.overlays[i] = null;
						}
						delete deletedObject['overlays'];
						delete display._objects[bizId];
					}
				}
			}
			else {
				// remove all overlays
				for (var bizId in display._objects) {
					var deletedObject = display._objects[bizId];
					for (var i = 0, l = deletedObject.overlays.length; i < l; i++) {
						display.webmap.removeLayer(deletedObject.overlays[i]);
						deletedObject.overlays[i] = null;
					}
					delete deletedObject['overlays'];
					delete display._objects[bizId];
				}
			}

			// add/update overlays from the data
			for (var i = 0, l = items.length; i < l; i++) {
				var item = items[i];

				var object = display._objects[item.bizId];
				if (object) {
					// if the wkts have changed delete the overlay and recreate it
					var same = (object.overlays.length == item.features.length);
					if (same) {
						for (var j = 0, m = object.overlays.length; j < m; j++) {
							if (object.overlays[j].getLayers()[0].zoomData.geometry !== item.features[j].geometry) {
								same = false;
								break;
							}
						}
					}
					if (! same) {
						for (var j = 0, m = object.overlays.length; j < m; j++) {
							display.webmap.removeLayer(object.overlays[j]);
							object.overlays[j] = null;
						}
						delete object['overlays'];
						delete display._objects[bizId];
						object = null;
					}
				}
				if (! object) { // object could have been nulled just above 
					object = {overlays: []};
					for (var j = 0, m = item.features.length; j < m; j++) {
						var itemFeature = item.features[j];
						
						var geometry = parse(itemFeature.geometry);
						geometry.properties = {editable: itemFeature.editable};
						if (itemFeature.strokeColour) {
							geometry.properties.color = itemFeature.strokeColour;
						}
						if (itemFeature.fillColour) {
							geometry.properties.fillColor = itemFeature.fillColour;
						}
						if (itemFeature.fillOpacity) {
							geometry.properties.fillOpacity = itemFeature.fillOpacity;
						}
						if (itemFeature.iconRelativeFilePath) {
							var icon = {
							    iconUrl: 'resources?_n=' + itemFeature.iconRelativeFilePath + '&_doc=' + data._doc,
							};
							if (itemFeature.iconAnchorX && itemFeature.iconAnchorY) {
								icon.iconAnchor = [itemFeature.iconAnchorX, itemFeature.iconAnchorY];
							}
							geometry.properties.icon = icon;
						}

						var overlay = L.geoJson(geometry, {
			        		pointToLayer: function(point, latlng) {
			        			var properties = point.properties;
			        	    	delete point.properties;

			        	    	if (properties.icon) {
			        	    		properties.icon = L.icon(properties.icon);
			        	    	}
			        		    return L.marker(latlng, properties);
			        		},
			        		style: function(feature) {
			        	    	var properties = feature.geometry.properties;
			        	    	delete feature.geometry.properties;
			        	    	
			        	    	return properties;
			        	    },
			        	    onEachFeature: function(feature, layer) {
								if (itemFeature.zoomable) { // can show the info window for zooming
									layer.zoomData = {bizId: item.bizId,
														geometry: itemFeature.geometry,
														fromTimestamp: item.fromTimestamp,
														toTimestamp: item.toTimestamp,
														mod: item.moduleName,
														doc: item.documentName,
														infoMarkup: item.infoMarkup};
									layer.bindPopup(function(layer) {
							        	return display.click(layer);
									});
								}
			        	    }
			        	});
				        object.overlays.push(overlay);
	                	display.webmap.addLayer(overlay);
					}
					display._objects[item.bizId] = object;
				}
			}

			if (fit) {
				var bounds = L.latLngBounds();
				var someOverlays = false;
				for (var id in display._objects) {
					someOverlays = true;
					var object = display._objects[id];
					var overlays = object.overlays;
					for (var i = 0, l = overlays.length; i < l; i++) {
						var overlay = overlays[i];
						bounds.extend(overlay.getBounds());
					}
				}

				if (someOverlays) {
					// Don't zoom in too far on only one marker
			    	display.webmap.fitBounds(bounds, {maxZoom: 15});
				}
			}
		},
		
		centre: function() {
			var result = null;
			if (SKYVE.Util.mapCentre) {
				try { // Catch any malformed WKT strings
					var centre = parse(SKYVE.Util.mapCentre);
					result = [centre.coordinates[1], centre.coordinates[0]];
				}
		        catch (e) {
		        	console.log(SKYVE.Util.mapCentre + " is malformed WKT Point format");
		        	SKYVE.Util.mapCentre = null;
		        	result = [0, 0];
		        }
			}
			else {
	        	result = [0, 0];
			}
			return result;
		},

	    scatterValue: function(display, // the display object that holds the map and other state variables
	    						value) { // the WKT string value to display
			if (! value) {
				return;
			}

			var obj = null;
	        try { // Catch any malformed WKT strings
	        	obj = L.geoJson(parse(value));
	        }
	        catch (e) {
	        	console.log('The WKT string ' + value + ' is invalid.');
                return;
	        }

            obj.addTo(display.webmap); // Add it to the map
            display._overlays.push(obj);

	        // Pan the map to the feature
            display.webmap.fitBounds(obj.getBounds(), {maxZoom: 15});
	    },

	    clear: function(display) { // the display object that holds the map and other state variables
	        for (var i = 0, l = display._overlays.length; i < l; i++) {
	            display.webmap.removeLayer(display._overlays[i]);
	        }
	        display._overlays.length = 0;
	    },
	    
	    drawingTools: function(display) {
			var drawingTools = display.drawingTools;
			
			L.EditControl = L.Control.extend({
				options: {
					position: 'topright'
				},
				onAdd: function (map) {
					var container = L.DomUtil.create('div', 'leaflet-control leaflet-bar');

					var link = null;
					if ((! drawingTools ) || 
							(drawingTools == 'point') || 
							(drawingTools == 'pointAndLine') || 
							(drawingTools == 'pointAndPolygon')) {
						link = L.DomUtil.create('a', '', container);
						link.href = '#';
						link.title = 'Create a new marker';
						link.innerHTML = '<i class="fa fa-map-marker"></i>';
						L.DomEvent.on(link, 'click', L.DomEvent.stop).on(link, 'click', function () {
							window.LAYER = display.webmap.editTools.startMarker.call(map.editTools);
						}, this);
					}
					if ((! drawingTools ) || 
							(drawingTools == 'line') || 
							(drawingTools == 'pointAndLine') || 
							(drawingTools == 'lineAndPolygon')) {
						link = L.DomUtil.create('a', '', container);
						link.href = '#';
						link.title = 'Create a new line';
						link.innerHTML = '<i class="fa fa-share-alt"></i>';
						L.DomEvent.on(link, 'click', L.DomEvent.stop).on(link, 'click', function () {
							window.LAYER = display.webmap.editTools.startPolyline.call(map.editTools);
						}, this);
					}
					if ((! drawingTools ) || 
							(drawingTools == 'polygon') || 
							(drawingTools == 'pointAndPolygon') || 
							(drawingTools == 'lineAndPolygon')) {
						link = L.DomUtil.create('a', '', container);
						link.href = '#';
						link.title = 'Create a new rectangle';
						link.innerHTML = '<i class="fa fa-square-o"></i>';
						L.DomEvent.on(link, 'click', L.DomEvent.stop).on(link, 'click', function () {
							window.LAYER = display.webmap.editTools.startRectangle.call(map.editTools);
						}, this);
	
						link = L.DomUtil.create('a', '', container);
						link.href = '#';
						link.title = 'Create a new polygon';
						link.innerHTML = '<i class="fa fa-star-o"></i>';
						L.DomEvent.on(link, 'click', L.DomEvent.stop).on(link, 'click', function () {
							window.LAYER = display.webmap.editTools.startPolygon.call(map.editTools);
						}, this);
					}
					return container;
				}
			});
			display.webmap.editControl = new L.EditControl();
			display.webmap.addControl(display.webmap.editControl);
			display.webmap.on('editable:drawing:commit', function(event) {
            	SKYVE.Leaflet.clear(display);

                // Stop editing mode so users can immediately edit
				event.layer.toggleEdit();

                display._overlays.push(event.layer);
                display.setFieldValue(stringify(event.layer.toGeoJSON(12)));
		    });
	    },
	    
	    geoLocator: function(display) {
            if (navigator.geolocation) {
            	L.GeoControl = L.Control.extend({
    				options: {
    					position: 'bottomleft'
    				},
    				onAdd: function (map) {
    					var container = L.DomUtil.create('div', 'leaflet-control leaflet-bar');

    					var link = L.DomUtil.create('a', '', container);
    					link.href = '#';
    					link.title = 'Click to set your current position from your GPS';
    					link.innerHTML = '<i class="fa fa-dot-circle-o"></i>';
    					L.DomEvent.on(link, 'click', L.DomEvent.stop).on(link, 'click', function () {
					        // Stop drawing
    						if (display.webmap.editTools.drawing()) {
    							// this will call the commit event above ready to be replaced by the geolocation
    							display.webmap.editTools.commitDrawing();
    						}

					    	navigator.geolocation.getCurrentPosition(
								function(position) {
							    	SKYVE.Leaflet.clear(display);
							    	
							    	var latlng = [position.coords.latitude, position.coords.longitude];
						    		var marker = L.marker(latlng);
						    		marker.addTo(display.webmap);
					                display._overlays.push(marker);
							    	display.setFieldValue('POINT (' + position.coords.longitude + ' ' + position.coords.latitude + ')');
							        display.webmap.setZoom(15);
									display.webmap.panTo(latlng);
								},
								function(error) {
									alert(error.message);
								},
								{enableHighAccuracy : true
							});
    					}, this);
    					return container;
    				}
    			});
    			display.webmap.geoControl = new L.GeoControl();
    			display.webmap.addControl(display.webmap.geoControl);
            }
	    },

	    setDisabled: function(display, disabled) {
	    	if (display.webmap) {
	    		if (display.webmap.editControl) {
	    			display.webmap.editControl.getContainer().style.display = disabled ? 'none' : 'block';
	    		}
		    	if (display.webmap.geoControl) {
		    		display.webmap.geoControl.getContainer().style.display = disabled ? 'none' : 'block';
		    	}
	    	}
	    },

	    refreshControls: function(display) {
        	L.GeoControl = L.Control.extend({
				options: {
					position: 'bottomleft'
				},
				onAdd: function (map) {
					var container = L.DomUtil.create('div', 'leaflet-control');

					container.innerHTML = '<input type="number" min="0" max="500" step="1" value="' + display.refreshTime + '" size="3" />' +
											'<input type="checkbox"' + (display._refreshRequired ? ' checked' : '') + '><label>Refresh</label>';
					
					container.children[0].addEventListener('change', function() {
						display.refreshTime = this.value;
						if (display._refreshRequired) {
							if (display._intervalId) {
								clearInterval(display._intervalId);
							}
							display._intervalId = setInterval(display.rerender.bind(display), display.refreshTime * 1000);
						}
					});
					container.children[1].addEventListener('click', function() {
						display._refreshRequired = this.checked;
						if (display._intervalId) {
							clearInterval(display._intervalId);
							display._intervalId = null;
						}
						if (display._refreshRequired) {
							display.rerender();
							display._intervalId = setInterval(display.rerender.bind(display), display.refreshTime * 1000);
						}
					});

					return container;
				}
			});
			display.webmap.addControl(new L.GeoControl());
	    }
	}
}();
SKYVE.PF = function() {
	// block multiple load attempts of google/leaflet maps JS libs.
	var loadingMap = false;
	
	var getSessionHistory = function() {
		var result = sessionStorage.sessionHistory;
		if (result) {
			result = JSON.parse(result);
		}
		else {
			result = [];
		}
		return result;
	};

	// public
	return {
		getById: function(id) {
			return $(PrimeFaces.escapeClientId(id));
		},
		
		getByIdEndsWith: function(id) {
			return $('[id$="' + id + '"]');
		},
		
		contentOverlayOnShow: function(id, url) {
			SKYVE.PF.getById(id + '_iframe').attr('src', url);
		},
		
		contentOverlayOnHide: function(id) {
			SKYVE.PF.getById(id + '_iframe').attr('src','')
		},
		
		afterContentUpload: function(binding, contentId, modoc, fileName) {
			top.$('[id$="_' + binding + '"]').val(contentId);
			var url = 'content?_n=' + contentId + '&_doc=' + modoc + '&_b=' + binding.replace(/\_/g, '.');
			top.$('[id$="_' + binding + '_link"]').attr('href', url).text(fileName).attr('onclick', 'return true');
			top.$('[id$="_' + binding + '_image"]').attr('src', url);
			top.PF(binding + 'Overlay').hide();
		},
		
		clearContentImage: function(binding) {
			$('[id$="_' + binding + '"]').val('');
			$('[id$="_' + binding + '_image"]').attr('src','images/blank.gif');
		},
		
		clearContentLink: function(binding) {
			$('[id$="_' + binding + '"]').val('');
			$('[id$="_' + binding + '_link"]').attr('href','javascript:void(0)').text('<Empty>').attr('onclick', 'return false');
		},
		
		tabChange: function(moduleName, documentName, id, index) {
			sessionStorage['tab_' + moduleName + '_' + documentName + '_' + id] = index;
			if (SKYVE.BizMap) {
				SKYVE.BizMap.resizeAll();
			}
			if (SKYVE.BizMapPicker) {
				SKYVE.BizMapPicker.resizeAll();
			}
		},
		
		getTextElement: function(id) {
			return SKYVE.PF.getById(id);
		},

		getTextValue: function(id) {
			return SKYVE.PF.getTextElement(id).val();
		},
		
		setTextValue: function(id, value) {
			SKYVE.PF.getTextElement(id).val(value);
		},

		getPasswordElement: function(id) {
			return SKYVE.PF.getById(id + 'password');
		},
		
		getPasswordValue: function(id) {
			return SKYVE.PF.getPasswordElement(id).val();
		},
		
		setPasswordValue: function(id, value) {
			SKYVE.PF.getPasswordElement(id).val(value);
		},

		// for selecting values and getting the selected value, use the PF SelectOneMenu API through widgetVar
		getComboElement: function(id) {
			return SKYVE.PF.getById(id);
		},

		// to perform a lookup, use the AutoComplete API through widgetVar
		getLookupElement: function(id) {
			return SKYVE.PF.getById(id);
		},
		
		getLookupValue: function(id) {
			return SKYVE.PF.getById(id + '_hinput').val();
		},
		
		setLookupValue: function(id, value) {
			SKYVE.PF.getById(id + '_hinput').val(value);
		},
		
		getLookupDescription: function(id) {
			return SKYVE.PF.getById(id + '_input').val();
		},
		
		setLookupDescription: function(id, value) {
			SKYVE.PF.getById(id + '_input').val(value);
		},
		
		getCheckboxElement: function(id) {
			return SKYVE.PF.getById(id);
		},
		
		getCheckboxValue: function(id) {
			var value = SKYVE.PF.getById(id + '_input').val();
			if (value == '0') {
				return null;
			}
			else if (value == '1') {
				return true;
			}
			else if (value == '2') {
				return false;
			}
			else {
				return SKYVE.PF.getById(id + '_input').is(":checked");
			}
		},
		
		setCheckboxValue: function(id, trueOrFalse) {
			SKYVE.PF.getById(id + '_input').prop('checked', trueOrFalse);

			var outerDiv = SKYVE.PF.getById(id);
			var innerDiv = outerDiv.find('.ui-chkbox-box');
			var innerSpan = innerDiv.find('.ui-chkbox-icon')
			if (trueOrFalse) {
				innerDiv.addClass('ui-state-active');
				innerSpan.addClass('ui-icon ui-icon-check')
			}
			else {
				innerDiv.removeClass('ui-state-active');
				innerSpan.removeClass('ui-icon ui-icon-check')
			}
		},

		// Called in the <head/> of PF pages when cold navigation to existing page.
		// ie Not from menu of conversation navigation
		establishHistory: function() {
			var sessionHistory = getSessionHistory();
			if (sessionHistory.length == 0) {
				var url = window.location.href;
				if (url.match(/.*[\?|\&]a\=e.*/) ||
						url.match(/.*[\?|\&]a\=e\&.*/)) {
					var referrer = document.referrer;
					if ((! referrer) || (referrer == '') || (referrer == url)) {
						referrer = SKYVE.Util.CONTEXT_URL;
					}
					sessionHistory.push(referrer);
					sessionStorage.sessionHistory = JSON.stringify(sessionHistory);
				}
			}
			//console.log('establish history ' + sessionStorage.sessionHistory);
		},
		
		// Called from the menu links to reset the history.
		startHistory: function(url) {
			sessionStorage.sessionHistory = '[]';
			window.location.assign(url);
			//console.log('start history ' + sessionStorage.sessionHistory);
		},
		
		// Called from New/Zoom In actions on DataGrids/ListGrids
		pushHistory: function(url) { // url is optional and browser wont change location if not defined
			var sessionHistory = getSessionHistory();
			sessionHistory.push(window.location.href);
			sessionStorage.sessionHistory = JSON.stringify(sessionHistory);
			if (url) {
				window.location.assign(url);
			}
			//console.log('push history ' + sessionStorage.sessionHistory);
		},
		
		// Called from OK, Delete and Cancel buttons on top-level edit views and
		// Zoom Out button on zoomed-in edit views.
		popHistory: function(redirect) {
			var sessionHistory = getSessionHistory();
			if (sessionHistory.length > 0) {
				var url = sessionHistory.pop();
				sessionStorage.sessionHistory = JSON.stringify(sessionHistory);
				if (redirect) {
					window.location.assign(url);
				}
			}
			//console.log('pop history ' + sessionStorage.sessionHistory);
		},
		
		// Called from Save button and from action buttons
		// when a bean was not persistent and becomes persistent from the button push.
		// It replaces the "new" URL (no i param) with an "edit" URL through the history stack.
		saveHistory: function(bizModule, bizDocument, bizId) {
			var sessionHistory = getSessionHistory();
			var historyChanged = false;
			for (var i = 0; i < sessionHistory.length; i++) {
				var url = sessionHistory[i];
				
				// NB can't just use indexOf() here - the entire URL parameter must match
				var tokens = url.split(/\?|\&/);
				var moduleParameterToken = 'm=' + bizModule;
				var documentParameterToken = 'd=' + bizDocument;
				var actionParameterFound = false;
				var moduleParameterFound = false;
				var documentParameterFound = false;
				var bizIdParameterFound = false;
				for (var j = 0; j < tokens.length; j++) {
					var token = tokens[j];
					if (token == 'a=e') {
						actionParameterFound = true;
					}
					else if (token == moduleParameterToken) {
						moduleParameterFound = true;
					}
					else if (token == documentParameterToken) {
						documentParameterFound = true;
					}
					else if (token.startsWith('i=')) {
						bizIdParameterFound = true;
					}
				}
				if (actionParameterFound && 
						moduleParameterFound && 
						documentParameterFound &&
						bizIdParameterFound) {
					url += '&i=' + bizId;
					sessionHistory[i] = url;
					historyChanged = true;
				}
			}
			if (historyChanged) {
				sessionStorage.sessionHistory = JSON.stringify(sessionHistory);
			}
			
			if (window.history) {
				if (history.replaceState) {
					var url = window.location.href + '&i=' + bizId;
					history.replaceState({}, '', url);
				}
			}
			//console.log('save history ' + sessionStorage.sessionHistory);
		},
		
        toggleFilters: function(dataTableId) {
            var hiddenClass = 'hiddenFilter';
            // test for element that ends with the dataTableId as it may be in a naming container
            var dataTable = $('[id$="' + dataTableId + '"]');
			if (dataTable != null) {
				var toggleClass = function() {
                    var filter = $(this);
                    if (filter.hasClass(hiddenClass)) {
                        filter.removeClass(hiddenClass);
                    } else {
                        filter.addClass(hiddenClass);
                    }
				};
				dataTable.find('.ui-filter-column').each(toggleClass);
				dataTable.find('.ui-column-customfilter').each(toggleClass);
			}
		},
		
		onPushMessage: function(pushMessage) {
			var growls = [];

			for (var i = 0, l = pushMessage.length; i < l; i++) {
				var m = pushMessage[i];
				if (m.type == 'g') {
					growls.push({severity: m.severity, summary: m.message});
				}
				else if (m.type == 'm') {
					alert(m.message);
				}
				else if (m.type == 'r') {
					pushRerender();
				}
				else if (m.type == 'j') {
					window[m.method](m.argument);
				}
			}
			
			if (growls.length > 0) {
				PrimeFaces.cw('Growl', 'pushGrowl', {
					id: 'pushGrowl', 
					widgetVar: 'pushGrowl',
					life: 6000, 
					sticky: false, 
					msgs: growls 
				});
			}
		},
		
		gmap: function(options) {
			if (loadingMap) {
				setTimeout(function() {SKYVE.PF.gmap(options)}, 100);
			}
			else if (window.google && window.google.maps && window.SKYVE.BizMapPicker) {
				if (options.queryName || options.modelName) {
					return SKYVE.BizMap.create(options);
				}
				return SKYVE.BizMapPicker.create(options);
			}
			else {
				loadingMap = true;

				SKYVE.Util.loadJS('wicket/wicket.js?v=' + SKYVE.Util.v, function() {
					SKYVE.Util.loadJS('wicket/wicket-gmap3.js?v=' + SKYVE.Util.v, function() {
						var url = 'https://maps.googleapis.com/maps/api/js?v=3&libraries=drawing';
						if (SKYVE.Util.googleMapsV3ApiKey) {
							url += '&key=' + SKYVE.Util.googleMapsV3ApiKey;
						}
						SKYVE.Util.loadJS(url, function() {
							SKYVE.Util.loadJS('prime/skyve-gmap-min.js?v=' + SKYVE.Util.v, function() {
								loadingMap = false;
								if (options.queryName || options.modelName) {
									return SKYVE.BizMap.create(options);
								}
								return SKYVE.BizMapPicker.create(options);
							});
						});
					});
				});
			}
		},
		
		leaflet: function(options) {
			if (loadingMap) {
				setTimeout(function() {SKYVE.PF.leaflet(options)}, 100);
			}
			else if (window.L && window.SKYVE.BizMapPicker) {
				if (options.queryName || options.modelName) {
					return SKYVE.BizMap.create(options);
				}
				return SKYVE.BizMapPicker.create(options);
			}
			else {
				loadingMap = true;

				SKYVE.Util.loadCSS('leaflet/leaflet.css?v=' + SKYVE.Util.v, function() {
					SKYVE.Util.loadJS('leaflet/leaflet.js?v=' + SKYVE.Util.v, function() {
						SKYVE.Util.loadJS('leaflet/Path.Drag.js?v=' + SKYVE.Util.v, function() {
							SKYVE.Util.loadJS('leaflet/Leaflet.Editable.js?v=' + SKYVE.Util.v, function() {
								SKYVE.Util.loadCSS('leaflet/leaflet.fullscreen.css?v=' + SKYVE.Util.v, function() {
									SKYVE.Util.loadJS('leaflet/Leaflet.fullscreen.min.js?v=' + SKYVE.Util.v, function() {
										SKYVE.Util.loadJS('prime/skyve-leaflet-min.js?v=' + SKYVE.Util.v, function() {
											loadingMap = false;
											if (options.queryName || options.modelName) {
												return SKYVE.BizMap.create(options);
											}
											return SKYVE.BizMapPicker.create(options);
										});
									});
								});
							});
						});
					});
				});
			}
		},

		// This is called by PF charts to lose the 2:1 aspect ratio.
		chartExtender: function() {
			if (! this.cfg.config.options) {
				this.cfg.config.options = {};
			}
			this.cfg.config.options.responsive = true;
			this.cfg.config.options.maintainAspectRatio = false;
		}
	};
}();
