/**
 * Implements the BizMap UI component for Google Maps.
 * Extends Canvas from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizMap", "Canvas");

isc.BizMap.addClassMethods({
	loadingGMap: false,

	/**
	 * Loads Google Maps API if not already loaded.
	 * @param {Function} callback - function to execute after loading the API.
	 */
	loadGMap: function (callback) {
		if (this.loadingGMap) {
			setTimeout(() => this.loadGMap(callback), 100);
		} else if (window.google?.maps) {
			callback();
		} else {
			this.loadingGMap = true;
			SKYVE.Util.loadJS(`wicket/wicket.js?v=${SKYVE.Util.v}`, () => {
				SKYVE.Util.loadJS(`wicket/wicket-gmap3.js?v=${SKYVE.Util.v}`, () => {
					let url = "https://maps.googleapis.com/maps/api/js?v=3&libraries=drawing";
					if (SKYVE.Util.googleMapsV3ApiKey) {
						url += `&key=${SKYVE.Util.googleMapsV3ApiKey}`;
					}
					SKYVE.Util.loadJS(url, () => {
						this.loadingGMap = false;
						callback();
					});
				});
			});
		}
	},

	v: 0,

	/**
	 * Initializes the BizMap instance.
	 */
	initialise: function () {
		eval(`${isc.BizMap.id}.build()`);
	},
});

isc.BizMap.addMethods({
	/**
	 * Initializes the map component with default settings.
	 * @param {Object} config - configuration settings for the map.
	 */
	init: function (config) {
		this._refreshRequired = true; // Set via the map UI
		this._refreshing = false; // Stop multiple refreshes
		this._zoomed = false; // We do not want refreshes when zoomed in
		this.width = "100%";
		this.height = "100%";
		this.styleName = "googleMapDivParent";
		this.ID = `bizMap${isc.BizMap.v++}`;
		this.redrawOnResize = false;
		this.Super("init", arguments);
		this._objects = {};
		this._intervalId = null; // Interval to stop on refresh checkbox click
	},

	/**
	 * Returns the inner HTML for the map container.
	 * @returns {string} the HTML string for the map container.
	 */
	getInnerHTML: function () {
		return `<div id="${this.ID}_map" style="margin:0;padding:0;height:100%">Loading Map...</div>`;
	},

	/**
	 * Draws the map component, ensuring Google Maps API is loaded.
	 */
	draw: function () {
		if (window.google?.maps) {
			if (!this.isDrawn()) {
				this.build();
				return this.Super("draw", arguments);
			}
		} else {
			isc.BizMap.id = this.ID;
			isc.BizMap.loadGMap(() => isc.BizMap.initialise());
			return this.Super("draw", arguments);
		}
	},

	/**
	 * Sets the data source for the map.
	 * @param {string} dataSourceID - the ID of the data source.
	 */
	setDataSource: function (dataSourceID) {
		if (window.google?.maps && this.webmap) {
			if (this._view) {
				this._modelName = dataSourceID;
				this._moduleName = null;
				this._queryName = null;
				this._geometryBinding = null;

				this._view._grids[dataSourceID] ||= {};
				this._view._grids[dataSourceID][this.getID()] = this;
			} else {
				const underscoreIndex = dataSourceID.indexOf("_");
				this._moduleName = dataSourceID.substring(0, underscoreIndex);
				this._queryName = dataSourceID.substring(underscoreIndex + 1);
				this._geometryBinding = this._queryName.split("_").pop();
				this._queryName = this._queryName.split("_")[0];

				this._modelName = null;
			}
			this._refresh(true);
		} else {
			this.delayCall("setDataSource", arguments, 100);
		}
	},

	/**
	 * Builds the map.
	 */
	build: function () {
		if (this.isDrawn()) {
			const mapOptions = {
				zoom: this.webmap?.getZoom() || 1,
				center: this.webmap?.getCenter() || new google.maps.LatLng(0, 0),
				mapTypeId: this.webmap?.getMapTypeId() || eval(SKYVE.Util.mapLayers),
			};

			this._objects = {}; // If we are building a new map, clear state
			this.infoWindow = new google.maps.InfoWindow({ content: "" });
			this.webmap = new google.maps.Map(
				document.getElementById(`${this.ID}_map`),
				mapOptions,
			);

			if (this.showRefresh) SKYVE.GMap.refreshControls(this);

			if (this.loading === "lazy") {
				this.webmap.addListener("zoom_changed", () => {
					if (!this._refreshing) this._refresh(false);
				});
				this.webmap.addListener("dragend", () => this._refresh(false));
			}

			this._refresh(true);

			if (this._intervalId) {
				clearInterval(this._intervalId);
				this._intervalId = null;
			}

			if (this.refreshTime > 0 && this._refreshRequired) {
				this._intervalId = setInterval(
					this.rerender.bind(this),
					this.refreshTime * 1000,
				);
			}
		} else {
			this.delayCall("build", null, 100);
		}
	},

	/**
	 * Rerenders the map by refreshing it.
	 */
	rerender: function () {
		this._refresh(false);
	},

	/**
	 * Resumes map refresh operations.
	 */
	resume: function () {
		this._zoomed = false;
	},

	/**
	 * Refreshes the map with new data.
	 * @param {boolean} fit - whether to fit the refreshed data within view.
	 */
	_refresh: function (fit) {
		if (
			this._zoomed || // Operator is zoomed in
			this._refreshing || // Already triggered a refresh
			!this.isDrawn() || // Widget is not drawn
			!this.isVisible() // Widget is not visible (from condition in UI or UI not displayed atm)
		)
			return;

		this._refreshing = true;
		let url = `${SKYVE.Util.CONTEXT_URL}map?`;

		if (this._view) {
			if (this._modelName) {
				const instance = this._view.gather(false);
				url += `_c=${instance._c}&_m=${this._modelName}`;
			} else {
				return;
			}
		} else if (this._queryName) {
			url += `_mod=${this._moduleName}&_q=${this._queryName}&_geo=${this._geometryBinding}`;
		} else {
			return;
		}

		let extents = "";
		if (this.loading === "lazy" && this.webmap?.getBounds()) {
			const bounds = this.webmap.getBounds();
			const wkt = new Wkt.Wkt();
			wkt.fromObject(bounds.getNorthEast());
			extents = `&_ne=${wkt.write()}`;
			wkt.fromObject(bounds.getSouthWest());
			extents += `&_sw=${wkt.write()}`;
		}

		isc.RPCManager.sendRequest({
			showPrompt: true,
			evalResult: true,
			actionURL: url + extents,
			httpMethod: "GET",
			callback: (rpcResponse, data) => {
				try {
					SKYVE.GMap.scatter(this, data, fit, true);
				} finally {
					this._refreshing = false;
				}
			},
		});
	},

	/**
	 * Handles click events on map overlays and displays an info window with a zoom button.
	 *
	 * @param {google.maps.OverlayView} overlay - the overlay that was clicked.
	 * @param {google.maps.MapMouseEvent} event - the event that triggered the click.
	 */
	click: function (overlay, event) {
		let contents = overlay.infoMarkup;
		contents += `<br/><br/><input type="button" value="Zoom" onclick="${this.ID}.zoom(`;

		if (overlay.getPosition) {
			const p = overlay.getPosition();
			contents += `${p.lat()},${p.lng()},${p.lat()},${p.lng()},'${overlay.mod}','${overlay.doc}','${overlay.bizId}')"/>`;

			this.infoWindow.open(this.webmap, overlay);
			this.infoWindow.setContent(contents);
		} else if (overlay.getPath) {
			const bounds = new google.maps.LatLngBounds();
			const path = overlay.getPath();

			for (let k = 0, n = path.getLength(); k < n; k++) {
				bounds.extend(path.getAt(k));
			}

			const ne = bounds.getNorthEast();
			const sw = bounds.getSouthWest();

			contents += `${ne.lat()},${sw.lng()},${sw.lat()},${ne.lng()},'${overlay.mod}','${overlay.doc}','${overlay.bizId}')"/>`;

			this.infoWindow.setPosition(event.latLng);
			this.infoWindow.open(this.webmap);
			this.infoWindow.setContent(contents);
		} else if (overlay.getBounds) {
			const bounds = overlay.getBounds();
			const ne = bounds.getNorthEast();
			const sw = bounds.getSouthWest();

			contents += `${ne.lat()},${sw.lng()},${sw.lat()},${ne.lng()},'${overlay.mod}','${overlay.doc}','${overlay.bizId}')"/>`;

			this.infoWindow.setPosition(event.latLng);
			this.infoWindow.open(this.webmap);
			this.infoWindow.setContent(contents);
		}
	},

	/**
	 * Zooms into a specific region of the map and opens an editing window for a business document.
	 *
	 * @param {number} topLeftLat - latitude of the top-left corner.
	 * @param {number} topLeftLng - longitude of the top-left corner.
	 * @param {number} bottomRightLat - latitude of the bottom-right corner.
	 * @param {number} bottomRightLng - longitude of the bottom-right corner.
	 * @param {string} bizModule - the business module identifier.
	 * @param {string} bizDocument - the business document identifier.
	 * @param {string} bizId - the unique identifier of the business document.
	 */
	zoom: function (
		topLeftLat,
		topLeftLng,
		bottomRightLat,
		bottomRightLng,
		bizModule,
		bizDocument,
		bizId,
	) {
		this._zoomed = true; // Prevents unnecessary refreshes when zoomed in

		const scale = Math.pow(2, this.webmap.getZoom());
		const nw = new google.maps.LatLng(
			this.webmap.getBounds().getNorthEast().lat(),
			this.webmap.getBounds().getSouthWest().lng(),
		);

		const worldCoordinateNW = this.webmap.getProjection().fromLatLngToPoint(nw);
		const topLeftWorldCoordinate = this.webmap
			.getProjection()
			.fromLatLngToPoint(new google.maps.LatLng(topLeftLat, topLeftLng));
		const bottomRightWorldCoordinate = this.webmap
			.getProjection()
			.fromLatLngToPoint(new google.maps.LatLng(bottomRightLat, bottomRightLng));

		const pageRect = this.getPageRect();
		const x =
			Math.floor((topLeftWorldCoordinate.x - worldCoordinateNW.x) * scale) +
			pageRect[0];
		const y =
			Math.floor((topLeftWorldCoordinate.y - worldCoordinateNW.y) * scale) +
			pageRect[1];
		const width =
			Math.floor((bottomRightWorldCoordinate.x - worldCoordinateNW.x) * scale) +
			pageRect[0] -
			x;
		const height =
			Math.floor((bottomRightWorldCoordinate.y - worldCoordinateNW.y) * scale) +
			pageRect[1] -
			y;

		isc.BizUtil.getEditView(bizModule, bizDocument, (view) => {
			isc.WindowStack.popup([x, y, width, height], "Edit", true, [view]);
			view.editInstance(bizId, null, null);
			this.infoWindow.close();
		});
	},
});

/**
 * Implements the BizMapPicker UI component.
 * Extends HTMLFlow from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizMapPicker", "HTMLFlow");

isc.BizMapPicker.addClassMethods({
	v: 0,

	/**
	 * Initializes the BizMapPicker by building the map.
	 */
	initialise: function () {
		eval(`${isc.BizMapPicker.id}.build()`);
	},
});

isc.BizMapPicker.addMethods({
	/**
	 * Initializes the map picker component.
	 * @param {Object} config - configuration object.
	 */
	init: function (config) {
		this.width = "100%";
		this.height = "100%";
		this.styleName = "googleMapDivParent";
		this.ID = `bizMapPicker${isc.BizMapPicker.v++}`;
		this.contents = `<div id="${this.ID}_map" style="margin:0;padding:0;height:100%">Loading Map...</div>`;
		this.Super("init", arguments);
		this._overlays = [];
		this.field = config.field;
		this.drawingTools = config.drawingTools;

		if (window.google && window.google.maps) {
			this.build();
		} else {
			isc.BizMapPicker.id = this.ID;
			isc.BizMap.loadGMap(isc.BizMapPicker.initialise);
		}
	},

	/**
	 * Enables or disables the map picker.
	 * @param {boolean} disabled - whether to disable the component.
	 */
	setDisabled: function (disabled) {
		SKYVE.GMap.setDisabled(this, disabled);
	},

	/**
	 * Maps the current field value onto the map.
	 */
	mapIt: function () {
		const value = this.field.getValue();
		SKYVE.GMap.scatterValue(this, value);
	},

	/**
	 * Sets the field value from a Well-Known Text (WKT) representation.
	 * @param {string} wktValue - the WKT value to set.
	 */
	setFieldValue: function (wktValue) {
		this.field.setValueFromPicker(wktValue);
	},

	/**
	 * Builds and initializes the Google Map.
	 */
	build: function () {
		if (this.isDrawn()) {
			const mapOptions = {
				zoom: SKYVE.Util.mapZoom,
				center: SKYVE.GMap.centre(),
				mapTypeId: eval(SKYVE.Util.mapLayers),
				mapTypeControlOptions: {
					style: google.maps.MapTypeControlStyle.DROPDOWN_MENU,
				},
			};

			this.webmap = new google.maps.Map(
				document.getElementById(`${this.ID}_map`),
				mapOptions,
			);

			SKYVE.GMap.drawingTools(this);
			SKYVE.GMap.geoLocator(this);
			SKYVE.GMap.setDisabled(this, this.field.isDisabled());
			SKYVE.GMap.clear(this);

			// Delay `mapIt` call to ensure the map has fully initialized
			this.delayCall("mapIt", null, 100);
		} else {
			this.delayCall("build", null, 100);
		}
	},
});
