/**
 *  Defines the ListView UI.
 */
isc.defineClass("ListView");

isc.ListView.addClassProperties({
	_header: null,

	/**
	 * The contents (layout) of the entire list.
	 * @type {isc.VLayout}
	 */
	contents: isc.VLayout.create({
		width: "100%",
		height: "100%",
		overflow: "auto",
		membersMargin: 2,
		layoutMargin: 2,
		margin: 2,

		/**
		 * Re-renders the currently visible view.
		 */
		rerender: function () {
			const views = ["_grid", "_calendar", "_tree", "_map"];
			for (const view of views) {
				if (isc.ListView[view]?.isVisible()) {
					isc.ListView[view].refresh?.();
					break;
				}
			}
		},

		/**
		 * Resumes auto-refresh for the visible view.
		 */
		resume: function () {
			// const views = ["_grid", "_calendar", "_tree", "_map"];
			const views = ["_map"];
			for (const view of views) {
				if (isc.ListView[view]?.isVisible()) {
					isc.ListView[view].resume?.();
					break;
				}
			}
		},
	}),

	_grid: null,
	_calendar: null,
	_tree: null,
	_map: null,

	/**
	 * Handles dropping a portlet into the layout.
	 * @type {isc.PortalLayout}
	 */
	_portal: isc.PortalLayout.create({
		width: "100%",
		height: "100%",

		/**
		 * Determines the portlet to drop into the layout.
		 * @param {Object} dragTarget - the dragged item.
		 * @param {number} colNum - the column number.
		 * @param {number} rowNum - the row number.
		 * @param {number} rowOffset - the row offset.
		 * @returns {isc.Portlet}
		 */
		getDropPortlet: function (dragTarget, colNum, rowNum, rowOffset) {
			if (dragTarget.isA("TreeGrid") && dragTarget.getID().endsWith("Tree")) {
				const leaf = dragTarget.getDragData()[0];
				const portlet = isc.Portlet.create({ title: leaf.desc, items: [] });

				const componentMap = {
					grid: isc.BizUtil.createListGrid,
					cal: isc.BizUtil.createCalendar,
					tree: isc.BizUtil.createTreeGrid,
					map: isc.BizUtil.createMap,
				};

				if (leaf.ref === "edit") {
					isc.BizUtil.getEditView(dragTarget.data.root.name, leaf.name, (view) => {
						portlet.addItem(view);
						view.newInstance(null, null, null, null, (data, success) => {
							if (success) {
								view.hideMember(view._header);
							}
						});
					});
				} else if (componentMap[leaf.ref]) {
					const component = componentMap[leaf.ref]();
					portlet.addItem(component);
					component.setDataSource(
						`${dragTarget.data.root.name}_${leaf.name}`,
						leaf.config,
					);
				} else {
					alert(`Menu ref of ${leaf.ref} is unknown`);
				}
				return portlet;
			}

			return this.Super("getDropPortlet", arguments);
		},
	}),

	/**
	 * Sets the heading of the list view.
	 * @param {string} title - the title text.
	 * @param {string} [icon] - the icon URL.
	 * @param {string} [fontIcon] - the font icon class.
	 * @param {string} modoc - the document module.
	 */
	_setHeading: function (title, icon, fontIcon, modoc) {
		const iconMarkup = icon
			? `<img style="width:32px;height:32px" src="resources?_doc=${modoc}&_n=${icon}&v=${SKYVE.Util.v}"/>`
			: fontIcon
				? `<i style="padding-left:5px;font-size:28px;width:32px !important" class="titleBar bizhubFontIcon ${fontIcon}"></i>`
				: "";

		// Initialised here so that isc.BizUtil user properties will have been set in page script
		if (!isc.ListView._header) {
			isc.ListView._header = isc.BizHeader.create();
			isc.ListView.contents.addMember(isc.ListView._header, 0);
		}

		isc.ListView._header.replace(iconMarkup, title, "", "");
	},

	/**
	 * Sets the data source for the list view grid.
	 * @param {string} ID - the identifier for the data source.
	 * @param {Object} menuConfig - the menu configuration options.
	 */
	setGridDataSource: function (ID, menuConfig) {
		if (!this._grid) {
			this._grid = isc.BizUtil.createListGrid();
			this.contents.addMember(this._grid);
		}
		if (this._calendar) {
			this.contents.hideMember(this._calendar);
		}
		if (this._tree) {
			this.contents.hideMember(this._tree);
		}
		if (this._map) {
			this.contents.hideMember(this._map);
		}

		this.contents.hideMember(this._portal);
		this.contents.showMember(this._grid);

		const ds = this._getDataSource(ID);
		const title = this._grid.setDataSource(ds, menuConfig);
		this._setHeading(title, ds.icon, ds.fontIcon, ds.modoc);
	},

	/**
	 * Sets the data source for the list view calendar.
	 * @param {string} ID - the identifier for the data source.
	 * @param {Object} menuConfig - the menu configuration options.
	 */
	setCalendarDataSource: function (ID, menuConfig) {
		if (!this._calendar) {
			this._calendar = isc.BizUtil.createCalendar();
			this.contents.addMember(this._calendar);
		}
		if (this._grid) {
			this.contents.hideMember(this._grid);
		}
		if (this._tree) {
			this.contents.hideMember(this._tree);
		}
		if (this._map) {
			this.contents.hideMember(this._map);
		}
		this.contents.hideMember(this._portal);
		this.contents.showMember(this._calendar);

		const ds = this._getDataSource(ID);
		this._calendar.setDataSource(ds);
		this._setHeading("NOT IMPLEMENTED", ds.icon, ds.fontIcon, ds.modoc);
	},

	/**
	 * Sets the data source for the list view tree.
	 * @param {string} ID - the identifier for the data source.
	 * @param {Object} menuConfig - the menu configuration options.
	 */
	setTreeDataSource: function (ID, menuConfig) {
		if (!this._tree) {
			this._tree = isc.BizUtil.createTreeGrid();
			this.contents.addMember(this._tree);
		}
		if (this._grid) {
			this.contents.hideMember(this._grid);
		}
		if (this._calendar) {
			this.contents.hideMember(this._calendar);
		}
		if (this._map) {
			this.contents.hideMember(this._map);
		}
		this.contents.hideMember(this._portal);
		this.contents.showMember(this._tree);

		const ds = this._getDataSource(ID);
		const title = this._tree.setDataSource(ds, menuConfig);
		this._setHeading(title, ds.icon, ds.fontIcon, ds.modoc);
	},

	/**
	 * Displays the portal view by hiding other view components and showing the portal.
	 */
	showPortal: function () {
		if (!this._grid) {
			this.contents.hideMember(this._grid);
		}
		if (!this._calendar) {
			this.contents.hideMember(this._calendar);
		}
		if (!this._tree) {
			this.contents.hideMember(this._tree);
		}
		if (!this._map) {
			this.contents.hideMember(this._map);
		}
		this.contents.showMember(this._portal);

		this.setHeading(
			"DASHBOARD",
			"shared/icons/Home.png",
			"fa-solid fa-house fa-2x",
			"",
		);
	},

	/**
	 * Sets the data source for the list view map.
	 * @param {string} ID - the identifier for the data source.
	 * @param {Object} menuConfig - the menu configuration options.
	 */
	setMapDataSource: function (ID, menuConfig) {
		if (!this._map) {
			this._map = isc.BizUtil.createMap();
			this.contents.addMember(this._map);
		}
		if (this._grid) {
			this.contents.hideMember(this._grid);
		}
		if (this._calendar) {
			this.contents.hideMember(this._calendar);
		}
		if (this._tree) {
			this.contents.hideMember(this._tree);
		}
		this.contents.hideMember(this._portal);
		this.contents.showMember(this._map);

		this._map.setDataSource(ID);
		this._setHeading(
			"MAP",
			"shared/icons/Home.png",
			"fa-solid fa-globe fa-2x",
			"",
		);
	},

	/**
	 * Initializes a view if it doesn't exist and hides other views.
	 * @param {string} viewName - the name of the view to initialize.
	 * @param {Function} createFunction - a function that creates the view.
	 * @private
	 */
	_initializeView: function (viewName, createFunction) {
		if (!this[viewName]) {
			this[viewName] = createFunction();
			this.contents.addMember(this[viewName]);
		}

		// Ensure heading and portal are part of the contents
		if (!this.contents.hasMember(this._heading)) {
			this.contents.addMember(this._heading);
		}
		if (!this.contents.hasMember(this._portal)) {
			this.contents.addMember(this._portal);
			this.contents.hideMember(this._portal);
		}

		// Hide other views
		["_grid", "_calendar", "_tree", "_map", "_portal"].forEach((view) => {
			if (view !== viewName) {
				this.contents.hideMember(this[view]);
			}
		});

		this.contents.showMember(this[viewName]);
	},

	/**
	 * Updates a view with a data source and sets the heading.
	 * @param {string} ID - the identifier for the data source.
	 * @param {Object} menuConfig - the menu configuration options.
	 * @param {string} viewName - the name of the view being updated.
	 * @param {string} [defaultTitle=""] - the default title if none is provided.
	 * @private
	 */
	_updateView: function (ID, menuConfig, viewName, defaultTitle = "") {
		const ds = this._getDataSource(ID);
		const title = this[viewName].setDataSource(ds, menuConfig) || defaultTitle;
		this._setHeading(title, ds.icon, ds.fontIcon, ds.modoc);
	},

	/**
	 * Retrieves a data source safely.
	 * @param {string} ID - the identifier for the data source.
	 * @returns {Object|null} the data source object, or null if not found.
	 * @private
	 */
	_getDataSource: function (ID) {
		return typeof window[ID] !== "undefined" ? window[ID] : null;
	},
});

isc.ListView.contents.addMember(isc.ListView._portal);
isc.ListView.contents.hideMember(isc.ListView._portal);
