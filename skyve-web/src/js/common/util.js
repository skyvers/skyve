// SKYVE namespace definition
if (!window.SKYVE) {
	window.SKYVE = {};
}

SKYVE.Util = (function () {
	const context = `${window.location}`.substring(
		0,
		`${window.location}`.lastIndexOf("/") + 1,
	);

	/**
	 * Loads a resource (JS or CSS) and appends it to the document head.
	 * @param {HTMLElement} node - the resource node to load.
	 * @param {Function} callback - callback function to execute after loading.
	 */
	const load = function (node, callback) {
		if (callback !== null) {
			if (node.readyState) {
				// IE, incl. IE9
				node.onreadystatechange = function () {
					if (node.readyState === "loaded" || node.readyState === "complete") {
						node.onreadystatechange = null;
						callback();
					}
				};
			} else {
				// Other browsers
				node.onload = callback;
			}
		}

		const headNode = document.getElementsByTagName("HEAD")[0];
		if (headNode) {
			headNode.appendChild(node);
		}
	};

	// Public methods
	return {
		customer: null,
		v: null,
		googleMapsV3ApiKey: null,
		ckEditorConfigFileUrl: null,
		mapCentre: null,
		mapZoom: 1,
		CONTEXT_URL: context,
		allowedReportFormats: null,

		/**
		 * Loads a JavaScript file.
		 * @param {string} scriptPath - the path to the JavaScript file.
		 * @param {Function} callback - callback function to execute after loading.
		 */
		loadJS: function (scriptPath, callback) {
			const scriptNode = document.createElement("SCRIPT");
			scriptNode.type = "text/javascript";
			scriptNode.src = scriptPath;
			load(scriptNode, callback);
		},

		/**
		 * Loads a CSS file.
		 * @param {string} cssPath - the path to the CSS file.
		 * @param {Function} callback - callback function to execute after loading.
		 */
		loadCSS: function (cssPath, callback) {
			const cssNode = document.createElement("LINK");
			cssNode.type = "text/css";
			cssNode.rel = "stylesheet";
			cssNode.href = cssPath;
			load(cssNode, callback);
		},

		/**
		 * Retrieves the current geolocation and executes a callback with the WKT string.
		 * @param {Function} callback - callback function to execute with the WKT string.
		 */
		geoLocate: function (callback) {
			if (navigator.geolocation) {
				$(function () {
					PrimeFaces.cw("Growl", "growl", {
						id: "growl",
						widgetVar: "growl",
						msgs: [
							{
								summary: "GeoLocating",
								detail: "Please wait...",
								severity: "info",
							},
						],
					});
				});

				navigator.geolocation.getCurrentPosition(
					function (position) {
						$(function () {
							PrimeFaces.cw("Growl", "growl", {
								id: "growl",
								widgetVar: "growl",
								msgs: [
									{ summary: "GeoLocating", detail: "Done", severity: "info" },
								],
							});
						});
						callback(
							`POINT (${position.coords.longitude} ${position.coords.latitude})`,
						);
					},
					function (error) {
						$(function () {
							PrimeFaces.cw("Growl", "growl", {
								id: "growl",
								widgetVar: "growl",
								msgs: [
									{
										summary: "GeoLocating",
										detail: error.message,
										severity: "warn",
									},
								],
							});
						});
					},
					{ enableHighAccuracy: true },
				);
			}
		},

		/**
		 * Sets a cookie indicating whether the device has a touch screen.
		 */
		setTouchCookie: function () {
			let hasTouchScreen = false;
			if ("maxTouchPoints" in navigator) {
				hasTouchScreen = navigator.maxTouchPoints > 0;
			} else if ("msMaxTouchPoints" in navigator) {
				hasTouchScreen = navigator.msMaxTouchPoints > 0;
			} else {
				const mQ = window.matchMedia && matchMedia("(pointer:coarse)");
				if (mQ && mQ.media === "(pointer:coarse)") {
					hasTouchScreen = !!mQ.matches;
				} else if ("orientation" in window) {
					hasTouchScreen = true; // deprecated, but good fallback
				}
			}
			document.cookie = `touch=${hasTouchScreen ? 1 : 0}; path=/`;
		},

		// Password strength estimation
		passwordStrength: {
			0: "Worst",
			1: "Bad",
			2: "Weak",
			3: "Good",
			4: "Strong",
		},
		progressBarPower: {
			0: "1%",
			1: "25%",
			2: "50%",
			3: "75%",
			4: "100%",
		},
		progressBarColour: {
			0: "#D73F40",
			1: "#DC6551",
			2: "#F2B84F",
			3: "#BDE952",
			4: "#3ba62f",
		},
	};
})();

SKYVE.GMap = (function () {
	let wkt = null;

	/**
	 * Determines the drawing modes based on the specified drawing tools.
	 * @param {string} drawingTools - the drawing tools specified on the geometryMap widget.
	 * @returns {Array<google.maps.drawing.OverlayType>} - an array of drawing modes.
	 */
	const drawingModes = function (drawingTools) {
		let result = null;
		switch (drawingTools) {
			case "point":
				result = [google.maps.drawing.OverlayType.MARKER];
				break;
			case "line":
				result = [google.maps.drawing.OverlayType.POLYLINE];
				break;
			case "polygon":
				result = [
					google.maps.drawing.OverlayType.POLYGON,
					google.maps.drawing.OverlayType.RECTANGLE,
				];
				break;
			case "pointAndLine":
				result = [
					google.maps.drawing.OverlayType.MARKER,
					google.maps.drawing.OverlayType.POLYLINE,
				];
				break;
			case "pointAndPolygon":
				result = [
					google.maps.drawing.OverlayType.MARKER,
					google.maps.drawing.OverlayType.POLYGON,
					google.maps.drawing.OverlayType.RECTANGLE,
				];
				break;
			case "lineAndPolygon":
				result = [
					google.maps.drawing.OverlayType.POLYLINE,
					google.maps.drawing.OverlayType.POLYGON,
					google.maps.drawing.OverlayType.RECTANGLE,
				];
				break;
			default:
				result = [
					google.maps.drawing.OverlayType.MARKER,
					google.maps.drawing.OverlayType.POLYLINE,
					google.maps.drawing.OverlayType.POLYGON,
					google.maps.drawing.OverlayType.RECTANGLE,
				];
		}
		return result;
	};

	// Public methods
	return {
		/**
		 * Scatters data points on the map.
		 * @param {Object} display - the display object that holds the map and other state variables.
		 * @param {Object} data - the response from the map servlet to scatter.
		 * @param {boolean} fit - whether to fit the map bounds to the data.
		 * @param {boolean} delta - whether to only remove or update if changed.
		 */
		scatter: function (display, data, fit, delta) {
			// Instantiate WKT if it hasn't been already (at this point Wkt script is loaded)
			if (!wkt) {
				wkt = new Wkt.Wkt();
			}

			const items = data.items;
			// If there is no data, there is probably an error, so just bug out
			if (!items) {
				return;
			}

			if (delta) {
				// Remove overlays not present in the data
				for (const bizId in display._objects) {
					if (Object.prototype.hasOwnProperty.call(display._objects, bizId)) {
						let found = false;
						for (let i = 0, l = items.length; i < l; i++) {
							if (items[i].bizId === bizId) {
								found = true;
								break;
							}
						}
						if (!found) {
							const deletedObject = display._objects[bizId];
							for (let i = 0, l = deletedObject.overlays.length; i < l; i++) {
								deletedObject.overlays[i].setMap(null);
								deletedObject.overlays[i] = null;
							}
							delete deletedObject.overlays;
							delete display._objects[bizId];
						}
					}
				}
			} else {
				// Remove all overlays
				for (const bizId in display._objects) {
					if (Object.prototype.hasOwnProperty.call(display._objects, bizId)) {
						const deletedObject = display._objects[bizId];
						for (let i = 0, l = deletedObject.overlays.length; i < l; i++) {
							deletedObject.overlays[i].setMap(null);
							deletedObject.overlays[i] = null;
						}
						delete deletedObject.overlays;
						delete display._objects[bizId];
					}
				}
			}

			// Add/update overlays from the data
			for (let i = 0, l = items.length; i < l; i++) {
				const item = items[i];

				let object = display._objects[item.bizId];
				if (object) {
					// If the WKTs have changed, delete the overlay and recreate it
					let same = object.overlays.length === item.features.length;
					if (same) {
						for (let j = 0, m = object.overlays.length; j < m; j++) {
							if (object.overlays[j].geometry !== item.features[j].geometry) {
								same = false;
								break;
							}
						}
					}
					if (!same) {
						for (let j = 0, m = object.overlays.length; j < m; j++) {
							object.overlays[j].setMap(null);
							object.overlays[j] = null;
						}
						delete object.overlays;
						delete display._objects[bizId];
						object = null;
					}
				}
				if (!object) {
					// Object could have been nulled just above
					object = { overlays: [] };
					for (let j = 0, m = item.features.length; j < m; j++) {
						const feature = item.features[j];

						try {
							// Catch any malformed WKT strings
							wkt.read(feature.geometry);
						} catch (e) {
							alert(`${feature.geometry} is invalid WKT.`);
							continue;
						}

						const props = { editable: feature.editable };
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
							props.icon = {
								url: `${SKYVE.Util.CONTEXT_URL}resources?_n=${feature.iconRelativeFilePath}&_doc=${data._doc}`,
							};
							if (feature.iconAnchorX && feature.iconAnchorY) {
								props.icon.anchor = new google.maps.Point(
									feature.iconAnchorX,
									feature.iconAnchorY,
								);
								props.icon.origin = new google.maps.Point(0, 0);
							}
						}

						const overlay = wkt.toObject(props);
						object.overlays.push(overlay);
						overlay.setMap(display.webmap);

						if (feature.zoomable) {
							// Can show the info window for zooming
							overlay.bizId = item.bizId;
							overlay.geometry = feature.geometry;
							overlay.fromTimestamp = item.fromTimestamp;
							overlay.toTimestamp = item.toTimestamp;
							overlay.mod = item.moduleName;
							overlay.doc = item.documentName;
							overlay.infoMarkup = item.infoMarkup;
							google.maps.event.addListener(overlay, "click", function (event) {
								display.click(this, event);
							});
						}
					}
					display._objects[item.bizId] = object;
				}
			}

			if (fit) {
				const bounds = new google.maps.LatLngBounds();
				let someOverlays = false;
				for (const id in display._objects) {
					if (Object.prototype.hasOwnProperty.call(display._objects, id)) {
						someOverlays = true;
						const object = display._objects[id];
						const overlays = object.overlays;
						for (let i = 0, l = overlays.length; i < l; i++) {
							const overlay = overlays[i];
							if (overlay.getPath) {
								// For polygons and polylines - fit the bounds to the vertices
								const path = overlay.getPath();
								for (let j = 0, m = path.getLength(); j < m; j++) {
									bounds.extend(path.getAt(j));
								}
							} else if (overlay.getPosition) {
								bounds.extend(overlay.getPosition());
							}
						}
					}
				}

				if (someOverlays) {
					// Don't zoom in too far on only one marker
					if (bounds.getNorthEast().equals(bounds.getSouthWest())) {
						if (display.webmap.getZoom() < 15) {
							display.webmap.setZoom(15);
						}
						if (
							overlay.getPosition !== undefined &&
							typeof overlay.getPosition === "function"
						) {
							display.webmap.setCenter(bounds.getNorthEast());
						}
					} else {
						display.webmap.fitBounds(bounds);
					}
				}
			}
		},

		/**
		 * Centers the map based on the configured map center.
		 * @returns {google.maps.LatLng} - the center coordinates of the map.
		 */
		centre: function () {
			// Instantiate WKT if it hasn't been already (at this point Wkt script is loaded)
			if (!wkt) {
				wkt = new Wkt.Wkt();
			}

			let result = null;
			if (SKYVE.Util.mapCentre) {
				try {
					// Catch any malformed WKT strings
					wkt.read(SKYVE.Util.mapCentre);
					const coord = wkt.components[0];
					result = new google.maps.LatLng(coord.y, coord.x);
				} catch (e) {
					console.log(`${SKYVE.Util.mapCentre} is malformed WKT Point format`);
					SKYVE.Util.mapCentre = null;
					result = new google.maps.LatLng(0, 0);
				}
			} else {
				result = new google.maps.LatLng(0, 0);
			}
			return result;
		},

		/**
		 * Displays a WKT string value on the map.
		 * @param {Object} display - the display object that holds the map and other state variables.
		 * @param {string} value - the WKT string value to display.
		 */
		scatterValue: function (display, value) {
			// Instantiate WKT if it hasn't been already (at this point Wkt script is loaded)
			if (!wkt) {
				wkt = new Wkt.Wkt();
			}

			// If no value is provided, return early
			if (!value) {
				return;
			}

			try {
				// Catch any malformed WKT strings
				wkt.read(value);
			} catch (e) {
				if (e.name === "WKTError") {
					alert("The WKT string is invalid.");
					return;
				}
			}

			// Convert the WKT to a Google Maps object
			const obj = wkt.toObject(display.webmap.defaults);

			// Handle specific geometry types
			if (wkt.type !== "polygon" && wkt.type !== "linestring") {
				if (obj.setEditable) {
					obj.setEditable(false); // Disable editing for non-polygon/linestring geometries
				}
			}

			// Handle multi-geometries (arrays) and single geometries
			if (Wkt.isArray(obj)) {
				// Iterate through multi-geometry objects
				for (const key in obj) {
					if (
						Object.prototype.hasOwnProperty.call(obj, key) &&
						!Wkt.isArray(obj[key])
					) {
						obj[key].setMap(display.webmap); // Add each geometry to the map
						display._overlays.push(obj[key]); // Track the overlay
					}
				}
			} else {
				obj.setMap(display.webmap); // Add the single geometry to the map
				display._overlays.push(obj); // Track the overlay
			}

			// Pan the map to the feature
			if (obj.getBounds !== undefined && typeof obj.getBounds === "function") {
				// For objects with defined bounds (e.g., rectangles, circles)
				display.webmap.fitBounds(obj.getBounds());
			} else {
				if (obj.getPath !== undefined && typeof obj.getPath === "function") {
					// For polygons and polylines - fit the bounds to the vertices
					const bounds = new google.maps.LatLngBounds();
					const path = obj.getPath();
					for (let i = 0, l = path.getLength(); i < l; i++) {
						bounds.extend(path.getAt(i));
					}
					display.webmap.fitBounds(bounds);
				} else {
					// For points (markers)
					if (display.webmap.getZoom() < 15) {
						display.webmap.setZoom(15); // Ensure a minimum zoom level
					}
					if (
						obj.getPosition !== undefined &&
						typeof obj.getPosition === "function"
					) {
						display.webmap.setCenter(obj.getPosition()); // Center the map on the marker
					}
				}
			}
		},

		/**
		 * Clears all overlays from the map.
		 * @param {Object} display - the display object that holds the map and other state variables.
		 */
		clear: function (display) {
			// Remove each overlay from the map
			for (let i = 0, l = display._overlays.length; i < l; i++) {
				display._overlays[i].setMap(null);
			}
			// Reset the overlays array
			display._overlays.length = 0;
		},

		/**
		 * Initializes drawing tools on the map.
		 * @param {Object} display - the display object that holds the map and other state variables.
		 */
		drawingTools: function (display) {
			// Instantiate WKT if it hasn't been already (at this point Wkt script is loaded)
			if (!wkt) {
				wkt = new Wkt.Wkt();
			}

			// Initialize the drawing manager with the specified drawing modes
			display.webmap.drawingManager = new google.maps.drawing.DrawingManager({
				drawingControlOptions: {
					position: google.maps.ControlPosition.LEFT_BOTTOM,
					drawingModes: drawingModes(display.drawingTools), // Get the allowed drawing modes
				},
			});

			// Add the drawing manager to the map
			display.webmap.drawingManager.setMap(display.webmap);

			// Listen for the completion of a drawing operation
			google.maps.event.addListener(
				display.webmap.drawingManager,
				"overlaycomplete",
				function (event) {
					// Clear existing overlays from the map
					SKYVE.GMap.clear(display);

					// Set the drawing mode to "pan" (the hand) so users can immediately edit
					this.setDrawingMode(null);

					// Add the new overlay to the map and track it
					display._overlays.push(event.overlay);

					// Convert the overlay to a WKT string and update the field value
					wkt.fromObject(event.overlay);
					const wktValue = wkt.write();
					display.setFieldValue(wktValue);
				},
			);
		},

		/**
		 * Adds a geolocation control to the map, allowing users to set their current position.
		 * @param {Object} display - the display object that holds the map and other state variables.
		 */
		geoLocator: function (display) {
			if (navigator.geolocation) {
				// Create a custom control for geolocation
				const control = document.createElement("DIV");
				Object.assign(control.style, {
					backgroundColor: "#fff",
					border: "2px solid #fff",
					borderRadius: "3px",
					boxShadow: "0 2px 6px rgba(0,0,0,.3)",
					cursor: "pointer",
					margin: "10px",
					padding: "5px",
					textAlign: "center",
				});
				control.title = "Click to set your current position from your GPS";
				control.innerHTML = '<i class="fa-solid fa-location-crosshairs"></i>';
				control.index = 1;

				// Add a click event listener to the control
				control.addEventListener("click", function () {
					navigator.geolocation.getCurrentPosition(
						function (position) {
							// Clear existing overlays from the map
							SKYVE.GMap.clear(display);

							// Set the drawing mode to "pan" (the hand) so users can immediately edit
							display.webmap.drawingManager.setDrawingMode(null);

							// Create a marker at the user's current position
							const pos = {
								lat: position.coords.latitude,
								lng: position.coords.longitude,
							};
							const marker = new google.maps.Marker({
								position: pos,
								map: display.webmap,
							});

							// Track the marker and update the field value with the WKT string
							display._overlays.push(marker);
							display.setFieldValue(`POINT (${pos.lng} ${pos.lat})`);

							// Zoom and center the map on the marker
							display.webmap.setZoom(15);
							display.webmap.setCenter(pos);
						},
						function (error) {
							alert(error.message); // Handle geolocation errors
						},
						{ enableHighAccuracy: true },
					);
				});

				// Add the control to the map
				display.webmap.geolocator = control;
				display.webmap.controls[google.maps.ControlPosition.LEFT].push(control);
			}
		},

		/**
		 * Enables or disables the drawing tools and geolocation control.
		 * @param {Object} display - the display object that holds the map and other state variables.
		 * @param {boolean} disabled - whether to disable the controls.
		 */
		setDisabled: function (display, disabled) {
			if (display.webmap) {
				// Disable or enable the drawing manager
				if (display.webmap.drawingManager) {
					display.webmap.drawingManager.setOptions({
						drawingControl: !disabled,
					});
				}

				// Hide or show the geolocation control
				if (display.webmap.geolocator) {
					display.webmap.geolocator.style.display = disabled ? "none" : "block";
				}
			}
		},

		/**
		 * Adds refresh controls to the map, allowing users to set the refresh rate and toggle auto-refresh.
		 * @param {Object} display - the display object that holds the map and other state variables.
		 */
		refreshControls: function (display) {
			// Create a custom control for refresh settings
			const control = document.createElement("DIV");
			Object.assign(control.style, {
				backgroundColor: "#fff",
				border: "2px solid #fff",
				borderRadius: "3px",
				boxShadow: "0 2px 6px rgba(0,0,0,.3)",
				cursor: "pointer",
				margin: "10px",
				padding: "5px",
				textAlign: "center",
			});
			control.title = "Click to set the refresh rate of the map";
			control.innerHTML = `
						        <input type="number" min="0" max="500" step="1" value="${display.refreshTime}" size="3" />
						        <input type="checkbox" ${display._refreshRequired ? "checked" : ""}><label>Refresh</label>
						    `;
			control.index = 1;

			// Add event listener for the refresh time input
			control.children[0].addEventListener("change", function () {
				display.refreshTime = Number(this.value); // Update the refresh time
				if (display._refreshRequired) {
					if (display._intervalId) {
						clearInterval(display._intervalId); // Clear the existing interval
					}
					// Set a new interval with the updated refresh time
					display._intervalId = setInterval(
						display.rerender.bind(display),
						display.refreshTime * 1000,
					);
				}
			});

			// Add event listener for the refresh toggle checkbox
			control.children[1].addEventListener("click", function () {
				display._refreshRequired = this.checked; // Update the refresh required flag
				if (display._intervalId) {
					clearInterval(display._intervalId); // Clear the existing interval
					display._intervalId = null;
				}
				if (display._refreshRequired) {
					display.rerender(); // Trigger an immediate rerender
					// Set a new interval with the current refresh time
					display._intervalId = setInterval(
						display.rerender.bind(display),
						display.refreshTime * 1000,
					);
				}
			});

			// Add the control to the map
			display.webmap.controls[google.maps.ControlPosition.LEFT_BOTTOM].push(
				control,
			);
		},
	};
})();

SKYVE.Leaflet = (function () {
	const numberRegexp = /[-+]?([0-9]*\.[0-9]+|[0-9]+)([eE][-+]?[0-9]+)?/;
	const tuples = new RegExp(
		"^" + numberRegexp.source + "(\\s" + numberRegexp.source + "){1,}",
	); // Matches sequences like '100 100' or '100 100 100'

	/**
	 * Parses a WKT string and returns a GeoJSON geometry object.
	 * @param {string} input - the WKT geometry string to parse.
	 * @returns {?Object} a GeoJSON geometry object, or null if parsing fails.
	 */
	function parse(input) {
		const parts = input.split(";");
		let wktString = parts.pop(); // Extract the WKT string
		const srid = (parts.shift() || "").split("=").pop(); // Extract the SRID if present

		let i = 0; // Current position in the WKT string

		/**
		 * Matches a regular expression at the current position in the WKT string.
		 * @param {RegExp} re - the regular expression to match.
		 * @returns {?string} the matched string, or null if no match is found.
		 */
		function $(re) {
			const match = wktString.substring(i).match(re);
			if (!match) return null;
			i += match[0].length; // Advance the position
			return match[0];
		}

		/**
		 * Adds a CRS (Coordinate Reference System) to a GeoJSON object if an SRID is present.
		 * @param {Object} obj - the GeoJSON object.
		 * @returns {Object} the GeoJSON object with an optional CRS.
		 */
		function crs(obj) {
			if (obj && srid.match(/\d+/)) {
				obj.crs = {
					type: "name",
					properties: {
						name: `urn:ogc:def:crs:EPSG::${srid}`,
					},
				};
			}
			return obj;
		}

		/**
		 * Skips whitespace in the WKT string.
		 */
		function white() {
			$(/^\s*/);
		}

		/**
		 * Parses multi-coordinates (e.g., for polygons or multi-geometries).
		 * @returns {?Array<Array<number>>} an array of coordinate rings, or null if parsing fails.
		 */
		function multicoords() {
			white();
			let depth = 0; // Tracks nesting depth for parentheses
			const rings = []; // Stores the parsed coordinate rings
			const stack = [rings]; // Tracks the current context for nested coordinates
			let pointer = rings; // Points to the current coordinate array
			let elem;

			// Parse coordinates until the end of the WKT string
			while (
				(elem =
					$(/^(\()/) || // Match opening parenthesis
					$(/^(\))/) || // Match closing parenthesis
					$(/^(,)/) || // Match comma separator
					$(tuples)) // Match coordinate tuples
			) {
				if (elem === "(") {
					// Start a new nested coordinate array
					stack.push(pointer);
					pointer = [];
					stack[stack.length - 1].push(pointer);
					depth++;
				} else if (elem === ")") {
					// End the current nested coordinate array
					if (pointer.length === 0) return null; // Handle empty rings
					pointer = stack.pop();
					if (!pointer) return null; // Handle malformed input
					depth--;
					if (depth === 0) break; // Exit if we've closed all nested levels
				} else if (elem === ",") {
					// Start a new coordinate array at the same nesting level
					pointer = [];
					stack[stack.length - 1].push(pointer);
				} else if (!elem.split(/\s/g).some(isNaN)) {
					// Parse coordinate tuples (e.g., "10 20")
					Array.prototype.push.apply(
						pointer,
						elem.split(/\s/g).map(parseFloat),
					);
				} else {
					return null; // Handle invalid coordinates
				}
				white(); // Skip any whitespace
			}

			if (depth !== 0) return null; // Ensure all parentheses are closed

			return rings;
		}

		/**
		 * Parses coordinates from the WKT string.
		 * @returns {?Array<Array<number>>} an array of coordinate arrays, or null if parsing fails.
		 */
		function coords() {
			const list = []; // Stores the parsed coordinate arrays
			let item = []; // Stores the current coordinate array
			let pt;

			// Parse coordinates until the end of the WKT string
			while (
				(pt =
					$(tuples) || // Match coordinate tuples (e.g., "10 20")
					$(/^(,)/)) // Match comma separator
			) {
				if (pt === ",") {
					// Start a new coordinate array
					list.push(item);
					item = [];
				} else if (!pt.split(/\s/g).some(isNaN)) {
					// Parse coordinate tuples (e.g., "10 20")
					if (!item) item = [];
					Array.prototype.push.apply(item, pt.split(/\s/g).map(parseFloat));
				}
				white(); // Skip any whitespace
			}

			// Add the last coordinate array to the list
			if (item) list.push(item);
			else return null; // Handle empty coordinates

			return list.length ? list : null; // Return null if no coordinates were parsed
		}

		/**
		 * Parses a WKT Point geometry and returns a GeoJSON Point object.
		 * @returns {?Object} a GeoJSON Point object, or null if parsing fails.
		 */
		function point() {
			// Match the "POINT" keyword (case-insensitive)
			if (!$(/^(point(\sz)?)/i)) return null;
			white(); // Skip any whitespace

			// Match the opening parenthesis
			if (!$(/^(\()/)) return null;

			// Parse the coordinates
			const c = coords();
			if (!c) return null; // Handle invalid coordinates
			white(); // Skip any whitespace

			// Match the closing parenthesis
			if (!$(/^(\))/)) return null;

			// Return the GeoJSON Point object
			return {
				type: "Point",
				coordinates: c[0], // Use the first coordinate array
			};
		}

		/**
		 * Parses a WKT MultiPoint geometry and returns a GeoJSON MultiPoint object.
		 * @returns {?Object} a GeoJSON MultiPoint object, or null if parsing fails.
		 */
		function multipoint() {
			// Match the "MULTIPOINT" keyword (case-insensitive)
			if (!$(/^(multipoint)/i)) return null;
			white(); // Skip any whitespace

			// Reformat the coordinates to match the standard WKT format
			const newCoordsFormat = wktString
				.substring(wktString.indexOf("(") + 1, wktString.length - 1)
				.replace(/\(/g, "")
				.replace(/\)/g, "");
			wktString = `MULTIPOINT (${newCoordsFormat})`;

			// Parse the coordinates
			const c = multicoords();
			if (!c) return null; // Handle invalid coordinates
			white(); // Skip any whitespace

			// Return the GeoJSON MultiPoint object
			return {
				type: "MultiPoint",
				coordinates: c,
			};
		}

		/**
		 * Parses a WKT MultiLineString geometry and returns a GeoJSON MultiLineString object.
		 * @returns {?Object} a GeoJSON MultiLineString object, or null if parsing fails.
		 */
		function multilinestring() {
			// Match the "MULTILINESTRING" keyword (case-insensitive)
			if (!$(/^(multilinestring)/i)) return null;
			white(); // Skip any whitespace

			// Parse the coordinates
			const c = multicoords();
			if (!c) return null; // Handle invalid coordinates
			white(); // Skip any whitespace

			// Return the GeoJSON MultiLineString object
			return {
				type: "MultiLineString",
				coordinates: c,
			};
		}

		/**
		 * Parses a WKT LineString geometry and returns a GeoJSON LineString object.
		 * @returns {?Object} a GeoJSON LineString object, or null if parsing fails.
		 */
		function linestring() {
			// Match the "LINESTRING" keyword (case-insensitive)
			if (!$(/^(linestring(\sz)?)/i)) return null;
			white(); // Skip any whitespace

			// Match the opening parenthesis
			if (!$(/^(\()/)) return null;

			// Parse the coordinates
			const c = coords();
			if (!c) return null; // Handle invalid coordinates

			// Match the closing parenthesis
			if (!$(/^(\))/)) return null;

			// Return the GeoJSON LineString object
			return {
				type: "LineString",
				coordinates: c,
			};
		}

		/**
		 * Parses a WKT Polygon geometry and returns a GeoJSON Polygon object.
		 * @returns {?Object} a GeoJSON Polygon object, or null if parsing fails.
		 */
		function polygon() {
			// Match the "POLYGON" keyword (case-insensitive)
			if (!$(/^(polygon(\sz)?)/i)) return null;
			white(); // Skip any whitespace

			// Parse the coordinates
			const c = multicoords();
			if (!c) return null; // Handle invalid coordinates

			// Return the GeoJSON Polygon object
			return {
				type: "Polygon",
				coordinates: c,
			};
		}

		/**
		 * Parses a WKT MultiPolygon geometry and returns a GeoJSON MultiPolygon object.
		 * @returns {?Object} a GeoJSON MultiPolygon object, or null if parsing fails.
		 */
		function multipolygon() {
			// Match the "MULTIPOLYGON" keyword (case-insensitive)
			if (!$(/^(multipolygon)/i)) return null;
			white(); // Skip any whitespace

			// Parse the coordinates
			const c = multicoords();
			if (!c) return null; // Handle invalid coordinates

			// Return the GeoJSON MultiPolygon object
			return {
				type: "MultiPolygon",
				coordinates: c,
			};
		}

		/**
		 * Parses a WKT GeometryCollection and returns a GeoJSON GeometryCollection object.
		 * @returns {?Object} a GeoJSON GeometryCollection object, or null if parsing fails.
		 */
		function geometrycollection() {
			const geometries = []; // Stores the parsed geometries
			let geometry;

			// Match the "GEOMETRYCOLLECTION" keyword (case-insensitive)
			if (!$(/^(geometrycollection)/i)) return null;
			white(); // Skip any whitespace

			// Match the opening parenthesis
			if (!$(/^(\()/)) return null;

			// Parse geometries until the end of the WKT string
			while ((geometry = root())) {
				geometries.push(geometry); // Add the parsed geometry to the collection
				white(); // Skip any whitespace
				$(/^(,)/); // Match the comma separator
				white(); // Skip any whitespace
			}

			// Match the closing parenthesis
			if (!$(/^(\))/)) return null;

			// Return the GeoJSON GeometryCollection object
			return {
				type: "GeometryCollection",
				geometries: geometries,
			};
		}

		/**
		 * Parses the root geometry from the WKT string.
		 * @returns {?Object} a GeoJSON geometry object, or null if parsing fails.
		 */
		function root() {
			return (
				point() || // Try parsing a Point
				linestring() || // Try parsing a LineString
				polygon() || // Try parsing a Polygon
				multipoint() || // Try parsing a MultiPoint
				multilinestring() || // Try parsing a MultiLineString
				multipolygon() || // Try parsing a MultiPolygon
				geometrycollection() // Try parsing a GeometryCollection
			);
		}

		return crs(root());
	}

	/**
	 * Converts a GeoJSON object into a WKT string.
	 * @param {Object} gj - the GeoJSON object to convert.
	 * @returns {string} the WKT representation of the GeoJSON object.
	 * @throws {Error} if the input is not a valid GeoJSON Feature or geometry object.
	 */
	function stringify(gj) {
		// Handle GeoJSON Feature objects by extracting the geometry
		if (gj.type === "Feature") {
			gj = gj.geometry;
		}

		/**
		 * Converts a coordinate pair into a WKT string.
		 * @param {Array<number>} c - the coordinate pair (e.g., [10, 20]).
		 * @returns {string} the WKT representation of the coordinate pair.
		 */
		function pairWKT(c) {
			return c.join(" ");
		}

		/**
		 * Converts a ring of coordinates into a WKT string.
		 * @param {Array<Array<number>>} r - the ring of coordinates.
		 * @returns {string} the WKT representation of the ring.
		 */
		function ringWKT(r) {
			return r.map(pairWKT).join(", ");
		}

		/**
		 * Converts multiple rings of coordinates into a WKT string.
		 * @param {Array<Array<Array<number>>>} r - the rings of coordinates.
		 * @returns {string} the WKT representation of the rings.
		 */
		function ringsWKT(r) {
			return r.map(ringWKT).map(wrapParens).join(", ");
		}

		/**
		 * Converts multiple sets of rings into a WKT string.
		 * @param {Array<Array<Array<Array<number>>>>} r - the sets of rings.
		 * @returns {string} the WKT representation of the sets of rings.
		 */
		function multiRingsWKT(r) {
			return r.map(ringsWKT).map(wrapParens).join(", ");
		}

		/**
		 * Wraps a string in parentheses.
		 * @param {string} s - the string to wrap.
		 * @returns {string} the wrapped string.
		 */
		function wrapParens(s) {
			return `(${s})`;
		}

		// Convert the GeoJSON object to WKT based on its type
		switch (gj.type) {
			case "Point":
				return `POINT (${pairWKT(gj.coordinates)})`;
			case "LineString":
				return `LINESTRING (${ringWKT(gj.coordinates)})`;
			case "Polygon":
				return `POLYGON (${ringsWKT(gj.coordinates)})`;
			case "MultiPoint":
				return `MULTIPOINT (${ringWKT(gj.coordinates)})`;
			case "MultiPolygon":
				return `MULTIPOLYGON (${multiRingsWKT(gj.coordinates)})`;
			case "MultiLineString":
				return `MULTILINESTRING (${ringsWKT(gj.coordinates)})`;
			case "GeometryCollection":
				return `GEOMETRYCOLLECTION (${gj.geometries.map(stringify).join(", ")})`;
			default:
				throw new Error(
					"stringify requires a valid GeoJSON Feature or geometry object as input",
				);
		}
	}

	// Public methods
	return {
		/**
		 * Scatters data points on the map, updating or removing overlays as needed.
		 * @param {Object} display - the display object that holds the map and other state variables.
		 * @param {Object} data - the response from the map servlet to scatter.
		 * @param {boolean} fit - whether to fit the map bounds to the data.
		 * @param {boolean} delta - whether to only remove or update if changed.
		 */
		scatter: function (display, data, fit, delta) {
			const items = data.items;

			// If there is no data, there is probably an error, so just return early
			if (!items) {
				return;
			}

			if (delta) {
				// Remove overlays not present in the data
				for (const bizId in display._objects) {
					if (Object.prototype.hasOwnProperty.call(display._objects, bizId)) {
						let found = false;
						for (let i = 0, l = items.length; i < l; i++) {
							if (items[i].bizId === bizId) {
								found = true;
								break;
							}
						}
						if (!found) {
							const deletedObject = display._objects[bizId];
							for (let i = 0, l = deletedObject.overlays.length; i < l; i++) {
								display.webmap.removeLayer(deletedObject.overlays[i]);
								deletedObject.overlays[i] = null;
							}
							delete deletedObject.overlays;
							delete display._objects[bizId];
						}
					}
				}
			} else {
				// Remove all overlays
				for (const bizId in display._objects) {
					if (Object.prototype.hasOwnProperty.call(display._objects, bizId)) {
						const deletedObject = display._objects[bizId];
						for (let i = 0, l = deletedObject.overlays.length; i < l; i++) {
							display.webmap.removeLayer(deletedObject.overlays[i]);
							deletedObject.overlays[i] = null;
						}
						delete deletedObject.overlays;
						delete display._objects[bizId];
					}
				}
			}

			// Add/update overlays from the data
			for (let i = 0, l = items.length; i < l; i++) {
				const item = items[i];

				let object = display._objects[item.bizId];
				if (object) {
					// If the WKTs have changed, delete the overlay and recreate it
					let same = object.overlays.length === item.features.length;
					if (same) {
						for (let j = 0, m = object.overlays.length; j < m; j++) {
							if (
								object.overlays[j].getLayers()[0].zoomData.geometry !==
								item.features[j].geometry
							) {
								same = false;
								break;
							}
						}
					}
					if (!same) {
						for (let j = 0, m = object.overlays.length; j < m; j++) {
							display.webmap.removeLayer(object.overlays[j]);
							object.overlays[j] = null;
						}
						delete object.overlays;
						delete display._objects[item.bizId];
						object = null;
					}
				}
				if (!object) {
					// Object could have been nulled just above
					object = { overlays: [] };
					for (let j = 0, m = item.features.length; j < m; j++) {
						const itemFeature = item.features[j];

						const geometry = parse(itemFeature.geometry);
						geometry.properties = { editable: itemFeature.editable };
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
							const icon = {
								iconUrl: `resources?_n=${itemFeature.iconRelativeFilePath}&_doc=${data._doc}`,
							};
							if (itemFeature.iconAnchorX && itemFeature.iconAnchorY) {
								icon.iconAnchor = [
									itemFeature.iconAnchorX,
									itemFeature.iconAnchorY,
								];
							}
							geometry.properties.icon = icon;
						}

						const overlay = L.geoJson(geometry, {
							pointToLayer(point, latlng) {
								const properties = point.properties;
								delete point.properties;

								if (properties.icon) {
									properties.icon = L.icon(properties.icon);
								}
								return L.marker(latlng, properties);
							},
							style: function (feature) {
								const properties = feature.geometry.properties;
								delete feature.geometry.properties;
								return properties;
							},
							onEachFeature: function (feature, layer) {
								if (itemFeature.zoomable) {
									// Can show the info window for zooming
									layer.zoomData = {
										bizId: item.bizId,
										geometry: itemFeature.geometry,
										fromTimestamp: item.fromTimestamp,
										toTimestamp: item.toTimestamp,
										mod: item.moduleName,
										doc: item.documentName,
										infoMarkup: item.infoMarkup,
									};
									layer.bindPopup(function (layer) {
										return display.click(layer);
									});
								}
							},
						});
						object.overlays.push(overlay);
						display.webmap.addLayer(overlay);
					}
					display._objects[item.bizId] = object;
				}
			}

			if (fit) {
				const bounds = L.latLngBounds();
				let someOverlays = false;
				for (const id in display._objects) {
					if (Object.prototype.hasOwnProperty.call(display._objects, id)) {
						someOverlays = true;
						const object = display._objects[id];
						const overlays = object.overlays;
						for (let i = 0, l = overlays.length; i < l; i++) {
							const overlay = overlays[i];
							bounds.extend(overlay.getBounds());
						}
					}
				}

				if (someOverlays) {
					// Don't zoom in too far on only one marker
					display.webmap.fitBounds(bounds, { maxZoom: 15 });
				}
			}
		},

		/**
		 * Retrieves the center coordinates of the map based on the configured map center.
		 * @returns {Array<number>} the center coordinates as [latitude, longitude], or [0, 0] if invalid.
		 */
		centre: function () {
			let result = [0, 0]; // Default center coordinates

			if (SKYVE.Util.mapCentre) {
				try {
					// Parse the WKT string to get the center coordinates
					const centre = parse(SKYVE.Util.mapCentre);
					result = [centre.coordinates[1], centre.coordinates[0]]; // [latitude, longitude]
				} catch (e) {
					console.log(`${SKYVE.Util.mapCentre} is malformed WKT Point format`);
					SKYVE.Util.mapCentre = null; // Reset the map center if invalid
				}
			}

			return result;
		},

		/**
		 * Displays a WKT string value on the map.
		 * @param {Object} display - the display object that holds the map and other state variables.
		 * @param {string} value - the WKT string value to display.
		 */
		scatterValue: function (display, value) {
			// If no value is provided, return early
			if (!value) {
				return;
			}

			let obj = null;
			try {
				// Parse the WKT string and create a GeoJSON layer
				obj = L.geoJson(parse(value));
			} catch (e) {
				console.log(`The WKT string ${value} is invalid.`);
				return;
			}

			// Add the GeoJSON layer to the map and track it
			obj.addTo(display.webmap);
			display._overlays.push(obj);

			// Pan the map to the feature
			display.webmap.fitBounds(obj.getBounds(), { maxZoom: 15 });
		},

		/**
		 * Clears all overlays from the map.
		 * @param {Object} display - the display object that holds the map and other state variables.
		 */
		clear: function (display) {
			// Remove each overlay from the map
			for (let i = 0, l = display._overlays.length; i < l; i++) {
				display.webmap.removeLayer(display._overlays[i]);
			}
			// Reset the overlays array
			display._overlays.length = 0;
		},

		/**
		 * Initializes drawing tools on the map based on the specified drawing tools.
		 * @param {Object} display - the display object that holds the map and other state variables.
		 */
		drawingTools: function (display) {
			const drawingTools = display.drawingTools;

			/**
			 * Custom Leaflet control for drawing tools.
			 */
			L.EditControl = L.Control.extend({
				options: {
					position: "topright", // Position of the control on the map
				},

				/**
				 * Creates the control container and adds drawing tool buttons.
				 * @param {L.Map} map - the Leaflet map instance.
				 * @returns {HTMLElement} the control container.
				 */
				onAdd: function (map) {
					const container = L.DomUtil.create(
						"div",
						"leaflet-control leaflet-bar",
					);

					// Add marker tool if enabled
					if (
						!drawingTools ||
						drawingTools === "point" ||
						drawingTools === "pointAndLine" ||
						drawingTools === "pointAndPolygon"
					) {
						const markerLink = L.DomUtil.create("a", "", container);
						markerLink.href = "#";
						markerLink.title = "Create a new marker";
						markerLink.innerHTML = '<i class="fa-solid fa-location-dot"></i>';
						L.DomEvent.on(markerLink, "click", L.DomEvent.stop).on(
							markerLink,
							"click",
							function () {
								window.LAYER = display.webmap.editTools.startMarker.call(
									map.editTools,
								);
							},
							this,
						);
					}

					// Add polyline tool if enabled
					if (
						!drawingTools ||
						drawingTools === "line" ||
						drawingTools === "pointAndLine" ||
						drawingTools === "lineAndPolygon"
					) {
						const polylineLink = L.DomUtil.create("a", "", container);
						polylineLink.href = "#";
						polylineLink.title = "Create a new line";
						polylineLink.innerHTML =
							'<i class="fa-solid fa-diagram-project"></i>';
						L.DomEvent.on(polylineLink, "click", L.DomEvent.stop).on(
							polylineLink,
							"click",
							function () {
								window.LAYER = display.webmap.editTools.startPolyline.call(
									map.editTools,
								);
							},
							this,
						);
					}

					// Add polygon and rectangle tools if enabled
					if (
						!drawingTools ||
						drawingTools === "polygon" ||
						drawingTools === "pointAndPolygon" ||
						drawingTools === "lineAndPolygon"
					) {
						const rectangleLink = L.DomUtil.create("a", "", container);
						rectangleLink.href = "#";
						rectangleLink.title = "Create a new rectangle";
						rectangleLink.innerHTML =
							'<i class="fa-solid fa-vector-square"></i>';
						L.DomEvent.on(rectangleLink, "click", L.DomEvent.stop).on(
							rectangleLink,
							"click",
							function () {
								window.LAYER = display.webmap.editTools.startRectangle.call(
									map.editTools,
								);
							},
							this,
						);

						const polygonLink = L.DomUtil.create("a", "", container);
						polygonLink.href = "#";
						polygonLink.title = "Create a new polygon";
						polygonLink.innerHTML = '<i class="fa-solid fa-draw-polygon"></i>';
						L.DomEvent.on(polygonLink, "click", L.DomEvent.stop).on(
							polygonLink,
							"click",
							function () {
								window.LAYER = display.webmap.editTools.startPolygon.call(
									map.editTools,
								);
							},
							this,
						);
					}

					return container;
				},
			});

			// Add the drawing control to the map
			display.webmap.editControl = new L.EditControl();
			display.webmap.addControl(display.webmap.editControl);

			// Handle the completion of a drawing operation
			display.webmap.on("editable:drawing:commit", function (event) {
				SKYVE.Leaflet.clear(display); // Clear existing overlays

				// Stop editing mode so users can immediately edit
				event.layer.toggleEdit();

				// Add the new overlay to the map and update the field value
				display._overlays.push(event.layer);
				display.setFieldValue(stringify(event.layer.toGeoJSON(12)));
			});
		},

		/**
		 * Adds a geolocation control to the map, allowing users to set their current position using GPS.
		 * @param {Object} display - the display object that holds the map and other state variables.
		 */
		geoLocator: function (display) {
			if (navigator.geolocation) {
				/**
				 * Custom Leaflet control for geolocation.
				 */
				L.GeoControl = L.Control.extend({
					options: {
						position: "bottomleft", // Position of the control on the map
					},

					/**
					 * Creates the control container and adds the geolocation button.
					 * @param {L.Map} map - The Leaflet map instance.
					 * @returns {HTMLElement} The control container.
					 */
					onAdd: function (map) {
						const container = L.DomUtil.create(
							"div",
							"leaflet-control leaflet-bar",
						);

						// Create the geolocation button
						const link = L.DomUtil.create("a", "", container);
						link.href = "#";
						link.title = "Click to set your current position from your GPS";
						link.innerHTML = '<i class="fa-solid fa-location-crosshairs"></i>';

						// Add a click event listener to the button
						L.DomEvent.on(link, "click", L.DomEvent.stop).on(
							link,
							"click",
							function () {
								// Stop any active drawing
								if (display.webmap.editTools.drawing()) {
									display.webmap.editTools.commitDrawing();
								}

								// Get the user's current position
								navigator.geolocation.getCurrentPosition(
									function (position) {
										// Clear existing overlays from the map
										SKYVE.Leaflet.clear(display);

										// Create a marker at the user's current position
										const latlng = [
											position.coords.latitude,
											position.coords.longitude,
										];
										const marker = L.marker(latlng);
										marker.addTo(display.webmap);

										// Track the marker and update the field value with the WKT string
										display._overlays.push(marker);
										display.setFieldValue(
											`POINT (${position.coords.longitude} ${position.coords.latitude})`,
										);

										// Zoom and center the map on the marker
										display.webmap.setZoom(15);
										display.webmap.panTo(latlng);
									},
									function (error) {
										alert(error.message); // Handle geolocation errors
									},
									{ enableHighAccuracy: true }, // Enable high accuracy for GPS
								);
							},
							this,
						);

						return container;
					},
				});

				// Add the geolocation control to the map
				display.webmap.geoControl = new L.GeoControl();
				display.webmap.addControl(display.webmap.geoControl);
			}
		},

		/**
		 * Enables or disables the drawing and geolocation controls on the map.
		 * @param {Object} display - the display object that holds the map and other state variables.
		 * @param {boolean} disabled - whether to disable the controls.
		 */
		setDisabled: function (display, disabled) {
			if (display.webmap) {
				// Disable or enable the edit control (drawing tools)
				if (display.webmap.editControl) {
					display.webmap.editControl.getContainer().style.display = disabled
						? "none"
						: "block";
				}

				// Disable or enable the geolocation control
				if (display.webmap.geoControl) {
					display.webmap.geoControl.getContainer().style.display = disabled
						? "none"
						: "block";
				}
			}
		},

		/**
		 * Adds refresh controls to the map, allowing users to set the refresh rate and toggle auto-refresh.
		 * @param {Object} display - the display object that holds the map and other state variables.
		 */
		refreshControls: function (display) {
			/**
			 * Custom Leaflet control for refresh settings.
			 */
			L.GeoControl = L.Control.extend({
				options: {
					position: "bottomleft", // Position of the control on the map
				},

				/**
				 * Creates the control container and adds refresh settings.
				 * @param {L.Map} map - The Leaflet map instance.
				 * @returns {HTMLElement} The control container.
				 */
				onAdd: function (map) {
					const container = L.DomUtil.create("div", "leaflet-control");

					// Add the refresh time input and checkbox
					container.innerHTML = `
		                <input type="number" min="0" max="500" step="1" value="${display.refreshTime}" size="3" />
		                <input type="checkbox" ${display._refreshRequired ? "checked" : ""}><label>Refresh</label>
		            `;

					// Add event listener for the refresh time input
					container.children[0].addEventListener("change", function () {
						display.refreshTime = Number(this.value); // Update the refresh time
						if (display._refreshRequired) {
							if (display._intervalId) {
								clearInterval(display._intervalId); // Clear the existing interval
							}
							// Set a new interval with the updated refresh time
							display._intervalId = setInterval(
								display.rerender.bind(display),
								display.refreshTime * 1000,
							);
						}
					});

					// Add event listener for the refresh toggle checkbox
					container.children[1].addEventListener("click", function () {
						display._refreshRequired = this.checked; // Update the refresh required flag
						if (display._intervalId) {
							clearInterval(display._intervalId); // Clear the existing interval
							display._intervalId = null;
						}
						if (display._refreshRequired) {
							display.rerender(); // Trigger an immediate rerender
							// Set a new interval with the current refresh time
							display._intervalId = setInterval(
								display.rerender.bind(display),
								display.refreshTime * 1000,
							);
						}
					});

					return container;
				},
			});

			// Add the refresh control to the map
			display.webmap.addControl(new L.GeoControl());
		},
	};
})();
