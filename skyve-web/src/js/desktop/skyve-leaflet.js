/**
 * Implements the BizMap UI component for Leaflet.
 * Extends Canvas from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizMap", "Canvas");

isc.BizMap.addClassMethods({
	loadingLeaflet: false,

	/**
	 * Loads Leaflet library and its dependencies asynchronously.
	 * @param {Function} callback - the function to execute once Leaflet is loaded.
	 */
	loadLeaflet: function (callback) {
		if (isc.BizMap.loadingLeaflet) {
			setTimeout(() => isc.BizMap.loadLeaflet(callback), 100);
		} else if (window.L) {
			callback();
		} else {
			isc.BizMap.loadingLeaflet = true;
			SKYVE.Util.loadCSS(`leaflet/leaflet.css?v=${SKYVE.Util.v}`, () => {
				SKYVE.Util.loadJS(`leaflet/leaflet.js?v=${SKYVE.Util.v}`, () => {
					SKYVE.Util.loadJS(`leaflet/Path.Drag.js?v=${SKYVE.Util.v}`, () => {
						SKYVE.Util.loadJS(`leaflet/Leaflet.Editable.js?v=${SKYVE.Util.v}`, () => {
							SKYVE.Util.loadCSS(
								`leaflet/leaflet.fullscreen.css?v=${SKYVE.Util.v}`,
								() => {
									SKYVE.Util.loadJS(
										`leaflet/Leaflet.fullscreen.min.js?v=${SKYVE.Util.v}`,
										() => {
											isc.BizMap.loadingLeaflet = false;
											callback();
										},
									);
								},
							);
						});
					});
				});
			});
		}
	},

	v: 0,

	/**
	 * Initialises the BizMap instance.
	 */
	initialise: function () {
		eval(`${isc.BizMap.id}.build()`);
	},
});

isc.BizMap.addMethods({
	/**
	 * Initialises the map with the given configuration.
	 * @param {Object} config - configuration object for the map.
	 */
	init: function (config) {
		this._refreshRequired = true;
		this._refreshing = false;
		this._zoomed = false;
		this.width = "100%";
		this.height = "100%";
		this.styleName = "googleMapDivParent";
		this.ID = `bizMap${isc.BizMap.v++}`;
		this.redrawOnResize = false;
		this.Super("init", arguments);
		this._objects = {};
		this._intervalId = null;
	},

	/**
	 * Returns the inner HTML for the map container.
	 * @returns {string} - the HTML string for the map container.
	 */
	getInnerHTML: function () {
		return `<div id="${this.ID}_map" style="margin:0;padding:0;height:100%">Loading Map...</div>`;
	},

	/**
	 * Draws the map if Leaflet is loaded, otherwise loads Leaflet and then draws.
	 */
	draw: function () {
		if (window.L) {
			if (!this.isDrawn()) {
				this.build();
				return this.Super("draw", arguments);
			}
		} else {
			isc.BizMap.id = this.ID;
			isc.BizMap.loadLeaflet(isc.BizMap.initialise);
			return this.Super("draw", arguments);
		}
	},

	/**
	 * Sets the data source for the map.
	 * @param {string} dataSourceID - the ID of the data source.
	 */
	setDataSource: function (dataSourceID) {
		if (window.L && this.webmap) {
			if (this._view) {
				this._modelName = dataSourceID;
				this._moduleName = null;
				this._queryName = null;
				this._geometryBinding = null;

				const grids = this._view._grids[dataSourceID] || {};
				this._view._grids[dataSourceID] = grids;
				grids[this.getID()] = this;
			} else {
				const underscoreIndex = dataSourceID.indexOf("_");
				this._moduleName = dataSourceID.substring(0, underscoreIndex);
				this._queryName = dataSourceID.substring(underscoreIndex + 1);
				const secondUnderscoreIndex = this._queryName.indexOf("_");
				this._geometryBinding = this._queryName.substring(
					secondUnderscoreIndex + 1,
				);
				this._queryName = this._queryName.substring(0, secondUnderscoreIndex);
				this._modelName = null;
			}
			this._refresh(true);
		} else {
			this.delayCall("setDataSource", arguments, 100);
		}
	},

	/**
	 * Builds the map with the specified options.
	 */
	build: function () {
		if (this.isDrawn()) {
			const mapOptions = {
				zoom: 1,
				center: [0, 0],
				fullscreenControl: {
					pseudoFullscreen: false,
				},
				layers: eval(SKYVE.Util.mapLayers),
			};

			if (this.webmap) {
				mapOptions.zoom = this.webmap.getZoom();
				mapOptions.center = this.webmap.getCenter();
			}

			this._objects = {};
			const element = document.getElementById(`${this.ID}_map`);
			element.innerHTML = "";
			this.webmap = L.map(element, mapOptions);

			if (this.showRefresh) {
				SKYVE.Leaflet.refreshControls(this);
			}

			if (this.loading === "lazy") {
				this.webmap.on("zoomend", () => {
					if (!this._refreshing) {
						this._refresh(false);
					}
				});
				this.webmap.on("moveend", () => {
					this._refresh(false);
				});
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
			return;
		}
	},

	/**
	 * Rerenders the map.
	 */
	rerender: function () {
		this._refresh(false);
	},

	/**
	 * Resumes the map from a paused state.
	 */
	resume: function () {
		this._zoomed = false;
	},

	/**
	 * Refreshes the map data.
	 * @param {boolean} fit - whether to fit the map to the bounds.
	 */
	_refresh: function (fit) {
		if (
			this._zoomed || // Operator is zoomed in
			this._refreshing || // Already triggered a refresh
			!this.isDrawn() || // Widget is not drawn
			!this.isVisible() // Widget is not visible (from condition in UI or UI not displayed atm)
		) {
			return;
		}

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

		this._refreshing = true;

		let extents = "";
		if (this.loading === "lazy" && this.webmap) {
			const bounds = this.webmap.wrapLatLngBounds(this.webmap.getBounds());
			if (bounds) {
				const ne = bounds.getNorthEast();
				const sw = bounds.getSouthWest();
				extents = `&_ne=POINT(${ne.lng} ${ne.lat})&_sw=POINT(${sw.lng} ${sw.lat})`;
			}
		}

		isc.RPCManager.sendRequest({
			showPrompt: true,
			evalResult: true,
			actionURL: url + extents,
			httpMethod: "GET",
			callback: (rpcResponse, data, rpcRequest) => {
				try {
					SKYVE.Leaflet.scatter(this, data, fit, true);
				} finally {
					this._refreshing = false;
				}
			},
		});
	},

	/**
	 * Handles click events on map layers.
	 * @param {Object} layer - the layer that was clicked.
	 * @returns {string} - the HTML string for the popup content.
	 */
	click: function (layer) {
		if (layer.zoomData) {
			this._selectedLayer = layer;
			return `${layer.zoomData.infoMarkup}<br/><br/><input type="button" value="Zoom" onclick="${this.ID}.zoom()"/>`;
		}
		return "";
	},

	/**
	 * Zooms the map to the selected layer.
	 */
	zoom: function () {
		this._zoomed = true; // Indicate that we don't want refreshes

		const pageRect = this.getPageRect();
		let topLeft = null;
		let bottomRight = null;

		if (this._selectedLayer.getBounds) {
			// Polylines, polygons
			const bounds = this._selectedLayer.getBounds();
			const nw = bounds.getNorthWest();
			const se = bounds.getSouthEast();
			topLeft = this.webmap.latLngToContainerPoint(nw);
			bottomRight = this.webmap.latLngToContainerPoint(se);
		} else {
			// Markers
			const coordinates = this._selectedLayer.feature.geometry.coordinates;
			const point = this.webmap.latLngToContainerPoint(
				L.latLng(coordinates[1], coordinates[0]),
			);
			topLeft = L.point(point.x - 10, point.y - 10);
			bottomRight = L.point(point.x + 10, point.y + 10);
		}

		isc.BizUtil.getEditView(
			this._selectedLayer.zoomData.mod,
			this._selectedLayer.zoomData.doc,
			(view) => {
				isc.WindowStack.popup(
					[
						pageRect[0] + Math.max(topLeft.x, 0),
						pageRect[1] + Math.max(topLeft.y, 0),
						Math.min(bottomRight.x - topLeft.x, bottomRight.x, pageRect[2]),
						Math.min(bottomRight.y - topLeft.y, bottomRight.y, pageRect[3]),
					],
					"Edit",
					true,
					[view],
				);
				view.editInstance(this._selectedLayer.zoomData.bizId, null, null);
				this._selectedLayer.closePopup();
				this._selectedLayer = null;
			},
		);
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
	 * Initialises the BizMapPicker instance.
	 */
	initialise: function () {
		eval(`${isc.BizMapPicker.id}.build()`);
	},
});

isc.BizMapPicker.addMethods({
	/**
	 * Initialises the map picker with the given configuration.
	 * @param {Object} config - configuration object for the map picker.
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

		if (window.L) {
			this.build();
		} else {
			isc.BizMapPicker.id = this.ID;
			isc.BizMap.loadLeaflet(isc.BizMapPicker.initialise);
		}
	},

	/**
	 * Sets the disabled state of the map picker.
	 * @param {boolean} disabled - whether the map picker should be disabled.
	 */
	setDisabled: function (disabled) {
		SKYVE.Leaflet.setDisabled(this, disabled);
	},

	/**
	 * Maps the current field value.
	 */
	mapIt: function () {
		const value = this.field.getValue();
		SKYVE.Leaflet.scatterValue(this, value);
	},

	/**
	 * Sets the field value from the map picker.
	 * @param {string} wktValue - the WKT value to set.
	 */
	setFieldValue: function (wktValue) {
		this.field.setValueFromPicker(wktValue);
	},

	/**
	 * Builds the map picker.
	 */
	build: function () {
		if (this.isDrawn()) {
			if (!this.webmap) {
				const mapOptions = {
					zoom: SKYVE.Util.mapZoom,
					center: SKYVE.Leaflet.centre(),
					editable: true,
					fullscreenControl: {
						pseudoFullscreen: false,
					},
					layers: eval(SKYVE.Util.mapLayers),
				};
				const element = document.getElementById(`${this.ID}_map`);
				element.innerHTML = "";
				this.webmap = L.map(element, mapOptions);

				SKYVE.Leaflet.drawingTools(this);
				SKYVE.Leaflet.geoLocator(this);
				SKYVE.Leaflet.setDisabled(this, this.field.isDisabled());
			}
			SKYVE.Leaflet.clear(this);
			this.mapIt();
		} else {
			this.delayCall("build", null, 100);
			return;
		}
	},
});
