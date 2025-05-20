/**
 * Implements the BizListGrid UI component.
 * Extends BizGrid as defined in datagrid.js.
 */
isc.ClassFactory.defineClass("BizListGrid", "BizGrid");

isc.BizListGrid.addProperties({
	summaryType: "", // The type of aggregate function selected
	tagId: null, // The tag currently active
	_dataSource: null, // Current data source {with setDataSource()}
	_toolbar: null, // The toolbar
	_advancedFilter: null, // The filter builder
	_summaryGrid: null, // The summary grid
	_flagForm: null,
	_flagDialog: null,

	// Buttons that are enabled disabled on grid row selection
	_newButton: null,
	_zoomButton: null,
	_popoutButton: null,
	_editButton: null,
	_pickButton: null,

	// Buttons that are enable disabled based on other factors
	_chartButton: null,

	// Switches to turn off tool buttons or menu items
	showAdd: true,
	showZoom: true,
	showEdit: true,
	showRemove: true,
	showDeselect: true,
	showExport: true,
	showChart: true,
	showFilter: true,
	showSummary: true,
	showSnap: true,
	showTag: true,

	autoPopulate: true, // Auto fetch from the data source
	_lookup: null, // Lookup control when this is used as a picklist
});

isc.BizListGrid.addMethods({
	/**
	 * Initialises the widget.
	 *
	 * Configuration has 4 properties available:
	 * 1. ListView
	 * 2. PickList
	 * 3. ListGrid
	 * 4. TreeGrid
	 *
	 * @param {Object} config - the configuration object.
	 */
	initWidget: function (config) {
		this.Super("initWidget", arguments);

		const me = this; // Required as parent and child scope is required

		this._config = config;

		// Flag dialog configuration
		this._flagDialog = isc.Window.create({
			autoCenter: true,
			autoSize: true,
			isModal: true,
			showModalMask: true,
			canDragReposition: true,
			canDragResize: false,
			showShadow: true,
			shadowSoftness: 10,
			shadowOffset: 0,
			title: "Flag",
			headerIconDefaults: { src: "flag.gif", width: 22, height: 22 },
			showMaximizeButton: false,
			showMinimizeButton: false,
			showHeaderIcon: true,
			items: [
				isc.DynamicForm.create({
					padding: 0,
					margin: 5,
					useAllDataSourceFields: false,
					numCols: 2,
					items: [], // This form is built in ListGrid.setDataSource()
				}),
			],
		});

		/**
		 * Retrieves the filter criteria based on the current state of the filter UI.
		 * If an advanced filter is selected, it will get the criteria from the advanced filter.
		 * Otherwise, it retrieves the basic filter criteria from the grid's filter editor.
		 *
		 * @returns {Object} - the complete filter criteria.
		 */
		const getAllCriteria = () => {
			// Get the list criteria for advanced or header filter as appropriate
			let criteria = this._advancedFilter.toggleButton.selected
				? this._advancedFilter.getCriteria()
				: this.grid.getFilterEditorCriteria(true);

			// Remove _display_ prefix from any criteria
			isc.BizUtil.convertFilterCriteria(criteria);

			// If params are defined, ensure they are added to the filter criteria
			if (config?.params) {
				criteria = isc.BizUtil.completeFilterCriteria(
					criteria,
					config.params,
					this._view,
				);
			}

			return criteria;
		};

		/**
		 * Creates a new menu item with specific properties.
		 * @returns {Object} the new menu item configuration.
		 */
		const newItem = {
			title: "New",
			icon: "icons/new.png",

			/**
			 * Determines if the menu item should be enabled based on certain conditions.
			 * @param {Object} target - the target of the action.
			 * @param {Object} menu - the menu containing the item.
			 * @param {Object} item - the menu item.
			 * @returns {boolean} true if the item should be enabled; false otherwise.
			 */
			enableIf: (target, menu, item) => {
				return !this._disabled && this.canCreate && this.canAdd;
			},

			/**
			 * Handles the click event for the menu item.
			 * It checks for unsaved changes and, if no changes exist, initiates a zoom action.
			 * @returns {void}
			 */
			click: () => {
				if (config) {
					let contConv = config.contConv || false;

					if (contConv) {
						const changedOnServer = this._view.gather(false)._changed;
						if (changedOnServer || this._view._vm.valuesHaveChanged()) {
							isc.say(
								`There are unsaved changes in the ${this._view._singular}. Save your changes to the ${this._view._singular} first.`,
								null,
								{ title: "Unsaved Changes!" },
							);
							return;
						}
					}

					const newParams = {};
					if (config.params) {
						isc.BizUtil.addFilterRequestParams(
							newParams,
							config.params,
							this._view,
						);
					}
					this.zoom(true, contConv, newParams);
				} else {
					this.zoom(true, false);
				}
			},
		};

		// The zoom item configuration, including the title, icon, and actions for enabling and clicking.
		this._zoomItem = {
			title: "Zoom",
			icon: "icons/zoom.gif",

			/**
			 * Determines if the "Zoom" menu item should be enabled based on certain conditions.
			 * @param {Object} target - the target of the action.
			 * @param {Object} menu - the menu containing the item.
			 * @param {Object} item - the menu item.
			 * @returns {boolean} true if the item should be enabled; false otherwise.
			 */
			enableIf: (target, menu, item) => {
				return this.canZoom && !this.aggregate && this.grid.anySelected();
			},

			/**
			 * Handles the click event for the "Zoom" menu item.
			 * It checks for unsaved changes and, if no changes exist, initiates a zoom action.
			 * @returns {void}
			 */
			click: () => {
				if (config && config.contConv) {
					const changedOnServer = this._view.gather(false)._changed;

					// Check for unsaved changes
					if (changedOnServer || this._view._vm.valuesHaveChanged()) {
						isc.say(
							`There are unsaved changes in the ${this._view._singular}. Save your changes to the ${this._view._singular} first.`,
							null,
							{ title: "Unsaved Changes!" },
						);
					} else {
						this.zoom(false, true); // Trigger zoom action
					}
				} else {
					this.zoom(false, false); // Trigger zoom action without conditions
				}
			},
		};

		// The popout item configuration, including the title, icon, and actions for enabling and clicking.
		const popoutItem = {
			title: "Popout",
			icon: "icons/popout.png",

			/**
			 * Determines if the "Popout" menu item should be enabled based on certain conditions.
			 * @param {Object} target - the target of the action.
			 * @param {Object} menu - the menu containing the item.
			 * @param {Object} item - the menu item.
			 * @returns {boolean} true if the item should be enabled; false otherwise.
			 */
			enableIf: (target, menu, item) => {
				return (
					this.canZoom &&
					!this.aggregate &&
					!(config && config.contConv) &&
					this.grid.anySelected()
				);
			},

			/**
			 * Handles the click event for the "Popout" menu item.
			 * It constructs a URL with event parameters and opens it in a new browser window.
			 * @returns {void}
			 */
			click: () => {
				const url = `?a=e&m=${this._eventRecord.bizModule}&d=${this._eventRecord.bizDocument}&i=${this._eventRecord.bizId}`;
				window.open(url, "_blank").focus();
			},
		};

		// The edit item configuration, including the title, icon, and actions for enabling and clicking.
		const editItem = {
			title: "Edit",
			icon: "icons/edit.png",

			/**
			 * Determines if the "Edit" menu item should be enabled based on certain conditions.
			 * @param {Object} target - the target of the action.
			 * @param {Object} menu - the menu containing the item.
			 * @param {Object} item - the menu item.
			 * @returns {boolean} true if the item should be enabled; false otherwise.
			 */
			enableIf: (target, menu, item) => {
				return (
					!this._disabled &&
					this.canUpdate &&
					this.canEdit &&
					this.grid.anySelected()
				);
			},

			/**
			 * Handles the click event for the "Edit" menu item.
			 * It saves the grid request properties, checks for unsaved changes, and starts editing.
			 * @returns {void}
			 */
			click: () => {
				if (this.grid.anySelected()) {
					this.grid.saveRequestProperties =
						this.grid.saveRequestProperties || {};
					this.grid.saveRequestProperties.params =
						this.grid.saveRequestProperties.params || {};
					this.grid.saveRequestProperties.params._csrf = this._csrf;

					// Ensure that embedded list grids use their parent view's conversation to edit data
					// in the same way as when zooming in
					if (this._view) {
						// this is an embedded list grid
						const instance = this._view.gather(false); // don't validate
						if (config && config.contConv) {
							if (instance._changed || this._view._vm.valuesHaveChanged()) {
								isc.say(
									`There are unsaved changes in the ${this._view._singular}. Save your changes to the ${this._view._singular} first.`,
									null,
									{ title: "Unsaved Changes!" },
								);
							} else {
								this.grid.saveRequestProperties.params._cc = "";
							}
						}
						this.grid.saveRequestProperties.params._c = instance._c;
						this.grid.startEditing(this._eventRowNum, this._eventColNum);
					} else {
						delete this.grid.saveRequestProperties.params._c;
						this.grid.startEditing(this._eventRowNum, this._eventColNum);
					}
				}
			},
		};

		// The pick item configuration, including the title, icon, and actions for enabling and clicking.
		const pickItem = {
			title: "Pick",
			icon: "icons/select.png",

			/**
			 * Determines if the "Pick" menu item should be enabled based on certain conditions.
			 * @param {Object} target - the target of the action.
			 * @param {Object} menu - the menu containing the item.
			 * @param {Object} item - the menu item.
			 * @returns {boolean} true if the item should be enabled; false otherwise.
			 */
			enableIf: (target, menu, item) => !this._disabled,

			/**
			 * Handles the click event for the "Pick" menu item.
			 * It invokes the pick method with the lookup object.
			 * @returns {void}
			 */
			click: () => {
				this.pick(this._lookup);
			},
		};

		/**
		 * Creates a toolbar button with the specified properties and actions.
		 *
		 * @param {Object} item - the item configuration, which includes the icon, title, and click action.
		 * @param {boolean} isEnabled - whether the button should be enabled initially.
		 * @param {string} tooltip - the tooltip to display when the button is hovered.
		 * @param {Function} onClick - the function to be invoked when the button is clicked.
		 * @returns {Object} the created button object.
		 */
		const createToolbarButton = function (item, isEnabled, tooltip, onClick) {
			const button = isc.BizUtil.createImageButton(
				item.icon,
				isEnabled,
				tooltip,
				onClick,
			);
			if (!isEnabled) {
				button.setDisabled(true);
			}
			return button;
		};

		// Create toolbar buttons
		this._newButton = createToolbarButton(
			newItem,
			true,
			"<b>New</b> record.",
			newItem.click,
		);
		this._zoomButton = createToolbarButton(
			this._zoomItem,
			true,
			"<b>Zoom</b> into record.",
			this._zoomItem.click,
		);
		this._popoutButton = createToolbarButton(
			popoutItem,
			true,
			"<b>Popout</b> record.",
			popoutItem.click,
		);
		this._editButton = createToolbarButton(
			editItem,
			true,
			"<b>Edit</b> a record inline.",
			editItem.click,
		);
		this._pickButton = createToolbarButton(
			pickItem,
			true,
			"<b>Pick</b> this record.",
			pickItem.click,
		);

		// Disable the zoom, popout, edit, and pick buttons initially
		this._zoomButton.setDisabled(true);
		this._popoutButton.setDisabled(true);
		this._editButton.setDisabled(true);
		this._pickButton.setDisabled(true);

		/**
		 * Clears the filter criteria from the grid and the advanced filter.
		 *
		 * @function
		 */
		const clearFilter = () => {
			this.grid.setFilterEditorCriteria({});
			this._advancedFilter.clearCriteria();
			this.refresh();
		};

		/**
		 * Refreshes the current view or grid.
		 *
		 * @function
		 */
		const refresh = () => {
			this.refresh();
		};

		/**
		 * Configuration for the "Clear Filter" item.
		 * @type {Object}
		 */
		const clearFilterItem = {
			title: "Clear Filter",
			icon: "icons/filter_delete.png",
			click: () => {
				this.clearFilter();
			},
		};

		/**
		 * Configuration for the "Refresh" item.
		 * @type {Object}
		 */
		const refreshItem = {
			title: "Refresh",
			icon: "icons/refresh.png",
			click: () => {
				this.refresh();
			},
		};

		/**
		 * Exports data by organizing selected and unselected fields based on their visibility and field state.
		 * It also applies the filter criteria and initiates the export process.
		 *
		 * @function
		 */
		const exportData = () => {
			// Get the field names, excluding hidden fields
			const fieldNames = this._dataSource.getFieldNames(true);
			const unselectedFields = [
				{ name: "bizFlagComment", title: "Flag", line: 1, width: 100 },
			];
			const selectedFields = [];

			// Retrieve the field state from the grid
			const fieldState = this.getFieldState();

			// Iterate over field names to classify them into selected and unselected fields
			fieldNames.forEach((fieldName, i) => {
				if (fieldName !== "bizTagged" && fieldName !== "bizFlagComment") {
					const field = this._dataSource.getField(fieldName);
					const dataGridField = this.grid.getField(fieldName);
					const align = dataGridField ? dataGridField.align : "center";

					// Add field to selected or unselected based on its visibility
					if (
						fieldState[i] &&
						(fieldState[i].visible === undefined ||
							fieldState[i].visible === null ||
							fieldState[i].visible)
					) {
						selectedFields.push({
							name: fieldName,
							title: field.title,
							line: 1,
							width: fieldState[i].width,
							align: align,
						});
					} else {
						unselectedFields.push({
							name: fieldName,
							title: field.title,
							line: 1,
							width: 100,
							align: align,
						});
					}
				}
			});

			// Get all filter criteria
			const allCriteria = getAllCriteria();

			// Initiate the export with the selected and unselected fields and filter criteria
			isc.ReportDialog.popupExport(
				this._dataSource.ID,
				this._view ? this._view.gather(false)._c : null,
				allCriteria,
				this.tagId,
				unselectedFields,
				selectedFields,
			);
		};

		/**
		 * Defines an item for exporting data with a conditional prompt based on the number of rows.
		 * If the row count exceeds certain thresholds, a confirmation dialog is shown before proceeding with the export.
		 *
		 * @type {Object}
		 */
		const exportItem = {
			title: "Export Data...",
			icon: "icons/export.png",
			/**
			 * Handles the click event for the export item.
			 * Displays a prompt based on the number of rows in the grid before triggering the data export.
			 *
			 * @function
			 */
			click: () => {
				const count = this.grid.getTotalRows();

				// Check if row count exceeds thresholds and show confirmation prompt accordingly
				if (count > 10000) {
					isc.ask(
						`There are ${count} rows in this list to export which could take more than 1 minute! Do you want to continue?`,
						(value) => {
							if (value) {
								exportData();
							}
						},
					);
				} else if (count > 1000) {
					isc.ask(
						`There are ${count} rows to export which may take a few seconds. Do you want to continue?`,
						(value) => {
							if (value) {
								exportData();
							}
						},
					);
				} else {
					exportData();
				}
			},
		};

		/**
		 * Collects filter parameters and triggers a chart dialog popup with the specified data.
		 *
		 * @function
		 */
		const chartData = () => {
			// Get all filter criteria
			const allCriteria = getAllCriteria();

			// Trigger the chart dialog popup with the necessary parameters
			isc.ChartDialog.popupChart(
				this._dataSource, // Data source for the chart
				this._view ? this._view.gather(false)._c : null, // View criteria, if available
				allCriteria, // Filter criteria
				this.tagId, // Tag ID for the data
				this._dataSource.fields, // Data source fields
			);
		};

		/**
		 * Chart data item configuration for the toolbar.
		 *
		 * @type {Object}
		 */
		const chartItem = {
			title: "Chart Data...",
			icon: "icons/chart.png",

			/**
			 * Determines if the chart item should be enabled based on the data source and configuration.
			 * It is enabled if the data source exists, is not a tree, and the data source ID does not contain '__'.
			 *
			 * @param {Object} target - the target of the menu item.
			 * @param {Object} menu - the menu to which this item belongs.
			 * @param {Object} item - the chart item itself.
			 * @returns {boolean} - whether the item should be enabled.
			 */
			enableIf: (target, menu, item) => {
				return (
					this._dataSource &&
					!this._config.isTree &&
					!this._dataSource.ID.includes("__")
				);
			},

			/**
			 * Handles the click event for the chart item. It shows a confirmation dialog based on the number of rows
			 * to be charted, and if confirmed, triggers the charting functionality.
			 *
			 * @function
			 */
			click: () => {
				const count = this.grid.getTotalRows();

				// Display a confirmation dialog based on the number of rows
				if (count > 10000) {
					isc.ask(
						`There are ${count} rows in this list to chart which could take more than 1 minute!  Do you want to continue?`,
						(value) => {
							if (value) {
								chartData();
							}
						},
					);
				} else if (count > 1000) {
					isc.ask(
						`There are ${count} rows to chart which may take a few seconds.  Do you want to continue?`,
						(value) => {
							if (value) {
								chartData();
							}
						},
					);
				} else {
					chartData();
				}
			},
		};

		// Create chart button
		this._chartButton = createToolbarButton(
			chartItem,
			true,
			"<b>Chart</b> this data.",
			chartItem.click,
		);
		this._chartButton.setDisabled(true); // Disable initially

		let contextMenuData = config && config.isPickList ? [pickItem] : [];
		if (this.showAdd) {
			contextMenuData.add(newItem);
		}
		if (this.showZoom) {
			contextMenuData.add(this._zoomItem);
			contextMenuData.add(popoutItem);
		}
		if (this.showEdit) {
			contextMenuData.add(editItem);
		}
		if (this.showRemove) {
			contextMenuData.add(this.deleteSelectionItem);
		}
		if (contextMenuData.length > 0) {
			contextMenuData.add({ isSeparator: true });
		}
		if (this.showDeselect) {
			contextMenuData.add(this.clearSelectionItem);
		}
		if (this.showFilter) {
			if (!this._config.isTree) {
				contextMenuData.add(clearFilterItem);
			}
		}
		contextMenuData.add(refreshItem);
		if (this.showExport || this.showChart) {
			contextMenuData.add({ isSeparator: true });
			if (this.showExport) {
				contextMenuData.add(exportItem);
			}
			if (this.showChart) {
				contextMenuData.add(chartItem);
			}
		}

		// Create the context menu of the BizListGrid
		this._contextMenu = isc.Menu.create({
			showShadow: true,
			shadowDepth: 10,
			data: contextMenuData,
		});

		// Initialize the advanced filter with configuration
		this._advancedFilter = isc.AdvancedFilter.create({
			filterableComponent: this,
			filterableComponentConfig: config,
		});

		/**
		 * Binds the click event of the toggle button to trigger the `toggleButtonClick` method of the advanced filter instance.
		 */
		this._advancedFilter.toggleButton.click = () => {
			this._advancedFilter.toggleButtonClick();
		};

		// Create and initialize the snap menu for the BizListGrid
		this._snapMenu = isc.Menu.create({
			showShadow: true,
			shadowDepth: 10,
			canSelectParentItems: true,
			data: [],
		});

		/**
		 * Creates and initializes the Snap Menu button with associated functionality.
		 */
		this._snapMenuButton = isc.ToolStripMenuButton.create({
			autoFit: true, // Automatically adjusts the button size to fit the title and icon
			padding: 3, // Padding around the content inside the button
			title: "No Snapshot", // The title displayed on the button
			menu: this._snapMenu, // The associated menu for the button

			/**
			 * Handler for the click event. Sends an RPC request to fetch snapshot data and populate the menu.
			 *
			 * @param {Object} args - arguments passed to the click handler.
			 */
			click: function () {
				const params = {
					a: "L",
					d: me._dataSource.ID,
					t: "sc",
					_csrf: me._csrf,
				};
				isc.RPCManager.sendRequest({
					showPrompt: false,
					evalResult: true,
					useSimpleHttp: true,
					httpMethod: "GET",
					params: params,
					actionURL: SKYVE.Util.CONTEXT_URL + "smartsnap",
					callback: function (rpcResponse, data, rpcRequest) {
						if (rpcResponse.status >= 0) {
							// success
							// Assign the CSRF Token from the response header
							me._csrf = rpcResponse.httpHeaders["x-csrf-token"];

							// Build the menu dynamically based on the fetched data
							const menu = [
								{
									title: "New Snapshot",
									icon: "icons/snap_add.png",
									click: function () {
										me._snapMenuButton._newSnap();
									},
								},
								{ isSeparator: true },
								{
									title: "No Snapshot",
									click: function () {
										me._snapMenuButton._setSnap(null);
									},
								},
							];

							// Populate menu with snapshots if data exists
							if (data) {
								for (let i = 0, l = data.length; i < l; i++) {
									menu.add({ isSeparator: true });
									const snap = data[i];
									const enabled = me.snapId == snap.bizId;
									const entry = {
										title: snap.name,
										icon: "icons/snap.png",
										click:
											me._snapMenuButton.ID +
											"._setSnap('" +
											snap.bizId +
											"','" +
											snap.name +
											"','" +
											JSON.stringify(snap.snapshot.criteria) +
											"','" +
											snap.snapshot.advancedCriteriaStyle +
											"','" +
											snap.snapshot.fieldState +
											"','" +
											snap.snapshot.sortState +
											"','" +
											snap.snapshot.groupState +
											"','" +
											snap.snapshot.summaryType +
											"')",
										submenu: [
											{
												title:
													"Update Snapshot" +
													(enabled ? "" : " (Select the Snapshot first)"),
												icon: "icons/snap_edit.png",
												click:
													me._snapMenuButton.ID +
													"._updateSnap('" +
													snap.bizId +
													"')",
												enabled: enabled,
											},
											{
												title: "Delete Snapshot",
												icon: "icons/snap_delete.png",
												click:
													me._snapMenuButton.ID +
													"._deleteSnap('" +
													snap.bizId +
													"')",
											},
										],
									};
									menu.add(entry);
								}
							}
							me._snapMenu.setData(menu);
						}
					},
				});

				// Call the Super class click handler
				this.Super("click", arguments);
			},
		});

		/**
		 * Creates a new Snapshot by prompting the user to enter a name.
		 *
		 * @function
		 */
		this._snapMenuButton._newSnap = () => {
			// Prompt the user to enter a snapshot name
			isc.askForValue(
				"Enter the new Snapshot name",
				(value) => {
					if (value) {
						// Prepare the parameters for the RPC request
						const params = {
							a: "N",
							n: value,
							d: this._dataSource.ID,
							t: "sc",
							_csrf: this._csrf,
							s: {
								criteria: this._advancedFilter.toggleButton.selected
									? this._advancedFilter.getCriteria()
									: this.grid.getFilterEditorCriteria(true),
								advancedCriteriaStyle: this._advancedFilter.getStyle(),
								fieldState: this.grid.getFieldState(),
								sortState: this.grid.getSortState(),
								groupState: this.grid.getGroupState(),
								summaryType: this.summaryType,
							},
						};

						// Send the RPC request to create the new snapshot
						isc.RPCManager.sendRequest({
							showPrompt: true,
							evalResult: true,
							useSimpleHttp: true,
							httpMethod: "POST",
							params: params,
							actionURL: SKYVE.Util.CONTEXT_URL + "smartsnap",
							callback: (rpcResponse, data, rpcRequest) => {
								if (rpcResponse.status >= 0) {
									// success
									// Assign the CSRF Token from the response header
									this._csrf = rpcResponse.httpHeaders["x-csrf-token"];

									// Update the snapId and menu button title
									this.snapId = data.bizId;
									this._snapMenuButton.setTitle(value);
								}
							},
						});
					}
				},
				{ width: 300 },
			);
		};

		/**
		 * Sets the snapshot configuration for the grid and updates the UI accordingly.
		 *
		 * @param {string} snapId - the ID of the snapshot to be applied.
		 * @param {string} title - the title of the snapshot.
		 * @param {string} criteria - the criteria in JSON format to be applied to the filter.
		 * @param {string} advancedCriteriaStyle - the style for advanced criteria.
		 * @param {object} fieldState - the field state to be applied to the grid.
		 * @param {object} sortState - the sort state to be applied to the grid.
		 * @param {object} groupState - the group state to be applied to the grid.
		 * @param {string} summaryType - the summary type to be used in the grid.
		 */
		this._snapMenuButton._setSnap = (
			snapId,
			title,
			criteria,
			advancedCriteriaStyle,
			fieldState,
			sortState,
			groupState,
			summaryType,
		) => {
			// Set the snapshot ID and update the title
			this.snapId = snapId;
			this._snapMenuButton.setTitle(title ? title : "No Snapshot");

			// Parse criteria if defined, or set as an empty object
			criteria = criteria ? JSON.parse(criteria) : {};

			// Handle advanced criteria
			if (criteria.operator) {
				this._advancedFilter.toggleButton.select();
				this._advancedFilter.toggleButtonClick();
				this._advancedFilter.setStyle(
					advancedCriteriaStyle ? advancedCriteriaStyle : "radio",
				);

				// Clear the filter and apply advanced criteria
				this.grid.setFilterEditorCriteria({});
				this._advancedFilter.setCriteria(criteria);
			} else {
				// Clear the criteria for simple filtering
				this._advancedFilter.clearCriteria();
				this._advancedFilter.toggleButton.deselect();
				this._advancedFilter.toggleButtonClick();

				// Apply simple filter criteria
				this.grid.setFilterEditorCriteria({});
				this.grid.setFilterEditorCriteria(criteria);
				this.grid.setCriteria({});
				this.grid.setFilterEditorCriteria(criteria);
			}

			// Apply field, sort, and group states (handling undefined cases)
			this.grid.setFieldState(fieldState || null);
			this.grid.setSortState(sortState || null);
			this.grid.setGroupState(groupState || null);

			// Set the summary type and apply it to the summary grid
			this.summaryType = summaryType || "";
			this._summaryGrid.data[0].bizFlagComment = this.summaryType;

			// Refresh the grid to apply all changes
			this.refresh();
		};

		/**
		 * Updates the snapshot configuration for the grid based on the provided snapshot ID.
		 * Sends an RPC request to update the snapshot and applies the associated settings.
		 *
		 * @param {string} snapId - the ID of the snapshot to be updated.
		 */
		this._snapMenuButton._updateSnap = (snapId) => {
			isc.RPCManager.sendRequest({
				showPrompt: true,
				evalResult: true,
				useSimpleHttp: true,
				httpMethod: "POST",
				params: {
					a: "U",
					i: snapId,
					t: "sc",
					_csrf: this._csrf,
					s: {
						criteria: this._advancedFilter.toggleButton.selected
							? this._advancedFilter.getCriteria()
							: this.grid.getFilterEditorCriteria(true),
						advancedCriteriaStyle: this._advancedFilter.getStyle(),
						fieldState: this.grid.getFieldState(),
						sortState: this.grid.getSortState(),
						groupState: this.grid.getGroupState(),
						summaryType: this.summaryType,
					},
				},
				actionURL: SKYVE.Util.CONTEXT_URL + "smartsnap",
				callback: (rpcResponse, data, rpcRequest) => {
					if (rpcResponse.status >= 0) {
						// Success
						// Assign the CSRF Token from the response header
						this._csrf = rpcResponse.httpHeaders["x-csrf-token"];
					}
				},
			});
		};

		/**
		 * Deletes the snapshot identified by the given snapshot ID.
		 *
		 * @param {string} snapId - the ID of the snapshot to be deleted.
		 */
		this._snapMenuButton._deleteSnap = (snapId) => {
			isc.ask("Do you want to delete this Snapshot?", (value) => {
				if (value) {
					isc.RPCManager.sendRequest({
						showPrompt: true,
						evalResult: true,
						useSimpleHttp: true,
						httpMethod: "POST",
						params: { a: "D", i: snapId, _csrf: this._csrf },
						actionURL: SKYVE.Util.CONTEXT_URL + "smartsnap",
						callback: (rpcResponse, data, rpcRequest) => {
							if (rpcResponse.status >= 0) {
								// success
								// Assign the CSRF Token from the response header
								this._csrf = rpcResponse.httpHeaders["x-csrf-token"];

								// Reset selected snapshot (if it was selected before deletion)
								if (this.snapId === snapId) {
									this._snapMenuButton._setSnap(null);
								}
							}
						},
					});
				}
			});
		};

		/**
		 * Clears the currently selected snapshot.
		 */
		this._clearSnap = () => {
			this.snapId = null;
			this._snapMenuButton.setTitle("No Snapshot");
		};

		// Create and initialise the the tags menu in the BizListGrid
		this._tagsMenu = isc.Menu.create({
			showShadow: true,
			shadowDepth: 10,
			canSelectParentItems: true,
			data: [],
		});

		// Create and initialise the the tags menu button in the BizListGrid
		this._tagsMenuButton = isc.ToolStripMenuButton.create({
			autoFit: true,
			padding: 3,
			title: "No Tag",
			menu: this._tagsMenu,
			click: function () {
				const params = {
					a: "L",
					ID: me._tagsMenuButton.ID,
					_csrf: me._csrf,
				};

				// Add tagId to parameters if it exists
				if (me.tagId) {
					params.t = me.tagId;
				}

				isc.RPCManager.sendRequest({
					showPrompt: false,
					evalResult: true,
					useSimpleHttp: true,
					httpMethod: "POST",
					params: params,
					actionURL: SKYVE.Util.CONTEXT_URL + "smarttag",
					callback: function (rpcResponse, data, rpcRequest) {
						if (rpcResponse.status >= 0) {
							// success
							// Assign the CSRF Token from the response header
							me._csrf = rpcResponse.httpHeaders["x-csrf-token"];
							me._tagsMenu.setData(data);
						}
					},
				});

				// Call the superclass click method
				this.Super("click", arguments);
			},
		});

		/**
		 * Prompts the user to enter a new tag name, and then sends a request to create the new tag.
		 * On successful creation, updates the tag menu button title and refreshes the page.
		 *
		 * @function
		 */
		this._tagsMenuButton.newTag = () => {
			// Prompt the user to enter a new tag name
			isc.askForValue(
				"Enter the new tag name",
				(value) => {
					if (value) {
						isc.RPCManager.sendRequest({
							showPrompt: true,
							evalResult: true,
							useSimpleHttp: true,
							httpMethod: "POST",
							params: {
								a: "N",
								n: value,
								ID: this._tagsMenuButton.ID,
								_csrf: this._csrf,
							},
							actionURL: SKYVE.Util.CONTEXT_URL + "smarttag",
							callback: (rpcResponse, data) => {
								if (rpcResponse.status >= 0) {
									// success
									// Assign the CSRF Token from the response header
									this._csrf = rpcResponse.httpHeaders["x-csrf-token"];

									this.tagId = data.bizId;
									this._tagsMenuButton.setTitle(value);
									this.refresh();
								}
							},
						});
					}
				},
				{ width: 300 },
			);
		};

		/**
		 * Sets the tag ID and title for the tag menu button, and refreshes the page.
		 *
		 * @function
		 * @name _tagsMenuButton.setTag
		 * @param {string} tagId - the ID of the tag to set.
		 * @param {string} title - the title of the tag to set.
		 */
		this._tagsMenuButton.setTag = (tagId, title) => {
			// Set the tag ID and update the title of the tags menu button
			this.tagId = tagId;
			this._tagsMenuButton.setTitle(title);

			// Refresh the UI to reflect the changes
			this.refresh();
		};

		/**
		 * Performs various operations on tags, such as clearing, deleting, tagging, and untagging.
		 *
		 * @function
		 * @name _tagsMenuButton.tagOp
		 * @param {string} tagId - the ID of the tag to operate on.
		 * @param {string} action - the action to perform on the tag. Possible actions:
		 *   - 'L' for listing all tags.
		 *   - 'T' for tagging all items in the list.
		 *   - 'U' for untagging all items in the list.
		 *   - 'C' for clearing all tagged data in the tag.
		 *   - 'N' for creating a new tag.
		 *   - 'D' for deleting an existing tag.
		 */
		this._tagsMenuButton.tagOp = (tagId, action) => {
			const askConfirmation = (message, callback) => {
				isc.ask(message, (value) => {
					if (value) {
						callback();
					}
				});
			};

			switch (action) {
				case "C": // Clear
					askConfirmation(
						"Do you want to clear all tagged data from this tag?",
						() => privateTagOp(tagId, action),
					);
					break;
				case "D": // Delete
					askConfirmation("Do you want to delete this tag?", () =>
						privateTagOp(tagId, action),
					);
					break;
				case "T":
				case "U": {
					// Tag/Untag
					const rowCount = this.grid.getTotalRows();
					let confirmationMessage = `There are ${rowCount} rows to ${action === "U" ? "un" : ""}tag`;

					if (rowCount > 10000) {
						confirmationMessage +=
							" which could take more than 1 minute! Do you want to continue?";
					} else if (rowCount > 1000) {
						confirmationMessage +=
							" which may take a few seconds. Do you want to continue?";
					} else {
						privateTagOp(tagId, action);
						return;
					}

					askConfirmation(confirmationMessage, () =>
						privateTagOp(tagId, action),
					);
					break;
				}
				default:
					privateTagOp(tagId, action);
			}
		};

		/**
		 * Performs an operation on a tag, such as tagging, untagging, or deleting.
		 *
		 * @function
		 * @name privateTagOp
		 * @param {string} tagId - the ID of the tag to operate on.
		 * @param {string} action - the action to perform on the tag. Possible actions:
		 *   - 'T' for tagging.
		 *   - 'U' for untagging.
		 *   - 'D' for deleting the tag.
		 */
		const privateTagOp = (tagId, action) => {
			const params = {
				a: action,
				t: tagId,
				_csrf: this._csrf,
			};

			// Add criteria for 'T' (tag) or 'U' (untag) actions
			if (["T", "U"].includes(action)) {
				params.d = this._dataSource.ID;
				const criteria = getAllCriteria();
				params.c = criteria;

				// Include view-specific criteria if available
				if (this._view) {
					params._c = this._view.gather(false)._c;
				}
			}

			isc.RPCManager.sendRequest({
				showPrompt: true,
				evalResult: true,
				useSimpleHttp: true,
				httpMethod: "POST",
				params: params,
				actionURL: SKYVE.Util.CONTEXT_URL + "smarttag",
				callback: (rpcResponse, data) => {
					if (rpcResponse.status >= 0) {
						// Success
						// Assign the CSRF Token from the response header
						this._csrf = rpcResponse.httpHeaders["x-csrf-token"];

						// If deleting the tag, reset the tag menu button title
						if (action === "D") {
							this._tagsMenuButton.setTag(null, "No Tag");
						}

						this.refresh();
					}
				},
			});
		};

		// Configure the toolbar with buttons based on visibility settings
		const toolStripMembers =
			config && config.isPickList ? [me._pickButton] : [];
		if (this.showAdd) {
			toolStripMembers.add(this._newButton);
		}
		if (this.showZoom) {
			toolStripMembers.add(this._zoomButton);
			toolStripMembers.add(this._popoutButton);
		}
		if (this.showEdit) {
			toolStripMembers.add(this._editButton);
		}
		if (this.showRemove) {
			toolStripMembers.add(this.deleteSelectionButton);
		}
		if (toolStripMembers.length > 0) {
			toolStripMembers.add("separator");
		}
		if (this.showDeselect) {
			toolStripMembers.add(
				isc.BizUtil.createImageButton(
					this.clearSelectionItem.icon,
					false,
					"<b>Deselect</b> all.",
					this.clearSelectionItem.click,
				),
			);
		}
		if (this.showFilter) {
			if (!this._config.isTree) {
				toolStripMembers.add(
					isc.BizUtil.createImageButton(
						clearFilterItem.icon,
						false,
						"<b>Clear filter</b> criteria.",
						clearFilterItem.click,
					),
				);
			}
		}
		toolStripMembers.add(
			isc.BizUtil.createImageButton(
				refreshItem.icon,
				false,
				"<b>Refresh</b> table data.",
				refreshItem.click,
			),
		);
		if (this.showFilter) {
			if (!this._config.isTree) {
				toolStripMembers.addList([
					"separator",
					this._advancedFilter.toggleButton,
				]);
			}
		}
		if (!config || !config.isPickList) {
			if (this.showExport || this.showChart) {
				toolStripMembers.add("separator");
				if (this.showExport) {
					toolStripMembers.add(
						isc.BizUtil.createImageButton(
							exportItem.icon,
							false,
							"<b>Export</b> table data.",
							exportItem.click,
						),
					);
				}
				if (this.showChart) {
					toolStripMembers.add(this._chartButton);
				}
			}
			if (this.showSnap) {
				toolStripMembers.addList([
					"separator",
					isc.Label.create({
						width: 60,
						contents: "Snapshot:",
					}),
					this._snapMenuButton,
				]);
			}
			if (this.showTag) {
				toolStripMembers.addList([
					"separator",
					isc.Label.create({
						width: 30,
						contents: "Tag:",
					}),
					this._tagsMenuButton,
				]);
			}
		}

		// Create the toolbar with the configured members
		this._toolbar = isc.ToolStrip.create({
			membersMargin: 2,
			layoutMargin: 2,
			width: "100%",
			overflow: "hidden",
			members: toolStripMembers,
		});

		// Configure the summary grid for the toolbar
		this._summaryGrid = isc.ListGrid.create({
			editByCell: true,
			canEditCell: function (rowNum, colNum) {
				return colNum === 0;
			},
			rowClick: function () {
				this.selectRecord(0, false);
				return false;
			},
			rowDoubleClick: function () {
				this.selectRecord(0, false);
				return false;
			},
			rowContextClick: function () {
				this.selectRecord(0, false);
				return false;
			},
			height: 28,
			leaveScrollbarGap: true,
			autoFetchData: false,
			autoFitData: null,
			showHeader: false,
			showEmptyMessage: false,
			bodyOverflow: "hidden",
		});

		// Add grid and toolbar to view based on configuration
		if (!this._config.isRepeater) {
			this.addMember(this._toolbar);
			this.addMember(this._advancedFilter);
		}

		this.addMember(this.grid);

		if (!this._config.isTree) {
			if (!this._config.isRepeater) {
				this.addMember(this._summaryGrid);
				if (!this.showSummary) {
					this.hideMember(this._summaryGrid);
				}
			}
		}

		// Assign flag form after grid construction
		this._flagForm = this._flagDialog.items[0];

		// Handle grid configuration for forms
		if (config && config.name) {
			let grids = this._view._grids[config.name];
			if (!grids) {
				grids = {};
				this._view._grids[config.name] = grids;
			}
			grids[this.getID()] = this;
		}
	},

	/**
	 * Checks if the current object has a data source associated with it.
	 * This is determined by checking if the grid object is not null.
	 *
	 * @function
	 * @name hasDataSource
	 * @returns {boolean} true if the grid object is not null, indicating a data source is available; false otherwise.
	 */
	hasDataSource: function () {
		return this.grid !== null;
	},

	/**
	 * Creates a grid with configuration and fields.
	 * @param {Object} config - configuration object for the grid.
	 * @param {Array} fields - fields to display in the grid.
	 * @param {boolean} canExpandRecords - flag indicating if records can expand.
	 */
	_createGrid: function (config, fields, canExpandRecords) {
		const me = this; // Required as parent and child scope is required

		const gridConfig = {
			autoDraw: true,
			height: "*",
			minHeight: 100,
			leaveScrollbarGap: true,
			autoFetchData: false,
			useAllDataSourceFields: true,
			showHeader: true,
			headerHeight: 30,
			showFilterEditor:
				this.showFilter &&
				!this._config.isTree &&
				!this._config.isRepeater &&
				!this.aggregate &&
				!this._advancedFilter.toggleButton.selected,
			canShowFilterEditor: false, // remove header context menu to show/hide filter row
			allowFilterOperators: false, // Remove header context menu to allow selection of operators other than the default
			filterByCell: false, // Ensure return/enter key or filter button click required to filter
			selectionType: "single",
			alternateRecordStyles: true,
			canEdit: true,
			dataSource: this._dataSource,
			fields: fields,
			editEvent: "none",
			neverValidate: false,
			validateByCell: true,
			saveByCell: false,
			validateOnChange: false,
			canHover: this._config.isRepeater ? false : true,
			canReorderFields: false,
			autoSaveEdits: true,
			modalEditing: true,
			canFreezeFields: false,
			contextMenu: this._config.isRepeater ? null : this._contextMenu,
			showRollOver: true,
			recordEnabledProperty: "_enabled",
			canExpandRecords: canExpandRecords,
			expansionMode: "details",

			/**
			 * Handles row click event.
			 * @param {Object} record - the record for the clicked row.
			 * @param {number} rowNum - the row number of the clicked row.
			 * @param {number} colNum - the column number of the clicked row.
			 * @returns {boolean} - returns true to allow normal click processing.
			 */
			rowClick: function (record, rowNum, colNum) {
				if (me.isRepeater) return;
				if (record && record.bizId) {
					me._eventRecord = record;
					me._eventRowNum = rowNum;
					me._eventColNum = colNum;
				}
				return this.Super("rowClick", arguments);
			},

			/**
			 * Handles row context click event.
			 * @param {Object} record - the record for the clicked row.
			 * @param {number} rowNum - the row number of the clicked row.
			 * @param {number} colNum - the column number of the clicked row.
			 * @returns {boolean} - returns true if the context menu should be shown.
			 */
			rowContextClick: function (record, rowNum, colNum) {
				if (me.isRepeater) return false;
				if (record && record.bizId) {
					this.deselectAllRecords();
					me._eventRecord = record;
					me._eventRowNum = rowNum;
					me._eventColNum = colNum;
					this.selectSingleRecord(record);
					return true;
				}
				return false;
			},

			/**
			 * Handles row double-click event.
			 * @param {Object} record - the record for the clicked row.
			 * @param {number} rowNum - the row number of the clicked row.
			 * @param {number} colNum - the column number of the clicked row.
			 * @returns {boolean} - returns true to allow normal double-click processing.
			 */
			rowDoubleClick: (record, rowNum, colNum) => {
				if (this.isRepeater) return true;
				if (record && record.bizId) {
					this._eventRecord = record;
					this._eventRowNum = rowNum;
					this._eventColNum = colNum;

					if (config && config.isPickList) {
						this.pick(this._lookup);
					} else if (this.showZoom && this.canZoom && !this.aggregate) {
						if (this._view && this._view._blurry) {
							this._view._blurry = this._zoomItem;
						} else {
							this._zoomItem.click();
						}
					}
				}
				return true;
			},

			/**
			 * Determines whether a record can be selected.
			 * @param {Object} record - the record to check for selection.
			 * @returns {boolean} - returns true if the record can be selected.
			 */
			canSelectRecord: (record) => !this.isRepeater,

			/**
			 * Handles the selection change event.
			 * @param {Object} record - the selected or deselected record.
			 * @param {boolean} state - the selection state (true for selected, false for deselected).
			 */
			selectionChanged: function (record, state) {
				if (this.anySelected()) {
					const zoomDisabled = me.aggregate || !me.canZoom;
					me._zoomButton.setDisabled(zoomDisabled);
					me._popoutButton.setDisabled(
						zoomDisabled || (config && config.contConv),
					);
					me._editButton.setDisabled(
						me._disabled || !me.canUpdate || !me.canEdit,
					);
					me._pickButton.setDisabled(me._disabled);
					me.deleteSelectionButton.setDisabled(
						me._disabled || !me.canDelete || !me.canRemove,
					);
				} else {
					me._zoomButton.setDisabled(true);
					me._popoutButton.setDisabled(true);
					me._editButton.setDisabled(true);
					me._pickButton.setDisabled(true);
					me.deleteSelectionButton.setDisabled(true);
				}
				me._newButton.setDisabled(me._disabled || !me.canCreate || !me.canAdd);
			},

			/**
			 * Updates the selected record binding.
			 * @param {Object} record - the selected record.
			 * @param {Array} recordList - list of selected records.
			 */
			selectionUpdated: (record, recordList) => {
				if (this.selectedIdBinding) {
					if (this.selectedIdTrackChanges) {
						this._view._vm.setValue(
							this.selectedIdBinding,
							record ? record.bizId : null,
						);
					} else {
						const changes = this._view._vm.valuesHaveChanged();
						this._view._vm.setValue(
							this.selectedIdBinding,
							record ? record.bizId : null,
						);
						if (!changes) this._view._vm.rememberValues();
					}
				}

				if (this.bizSelected) {
					if (this.showZoom && this.canZoom && !this.aggregate) {
						if (isc.RPCManager.requestsArePending()) {
							if (this._view) {
								this._view._blurry = null;
							}
						} else {
							if (this._view) {
								this._view._blurry = true;
							}
						}
						this.delayCall("bizSelected", [], this.doubleClickDelay);
					} else {
						this.bizSelected();
					}
				}
			},

			/**
			 * Handles the completion of an edit operation.
			 * @param {number} rowNum - the row number of the edited row.
			 * @param {number} colNum - the column number of the edited column.
			 * @param {Object} newValues - the new values for the edited row.
			 * @param {Object} oldValues - the old values for the edited row.
			 * @param {Object} editCompletionEvent - the event for edit completion.
			 * @param {Object} dsResponse - the response from the data source.
			 */
			editComplete: (
				rowNum,
				colNum,
				newValues,
				oldValues,
				editCompletionEvent,
				dsResponse,
			) => {
				if (dsResponse.status >= 0) {
					// Success
					// Assign the CSRF Token from the response header
					this._csrf = dsResponse.httpHeaders["x-csrf-token"];

					if (this.bizEdited) {
						this.bizEdited();
					}
				}
			},

			/**
			 * Customizes the filter data request parameters.
			 * @param {Object} criteria - the filter criteria.
			 * @param {Function} callback - the callback to call after filtering.
			 * @param {Object} requestProperties - the properties for the request.
			 */
			filterData: function (criteria, callback, requestProperties) {
				let result = criteria || {};
				
				requestProperties = requestProperties || {};
				requestProperties.params = requestProperties.params || {};

				if (me._csrf) requestProperties.params._csrf = me._csrf;

				if (!config.isTree) {
					requestProperties.params._summary = me.summaryType;
				}

				if (me.showTag) {
					requestProperties.params._tagId = me.tagId;
				}

				if (me._view) {
					requestProperties.params._c = me._view.gather(false)._c;
					if (me._view._b) {
						requestProperties.params._b = me._view._b;
					}
				}

				if (config && (config.contConv || config.isPickList)) {
					requestProperties.params._cc = "";
				}

				let editorCriteria = me._advancedFilter.toggleButton.selected
					? me._advancedFilter.me()
					: me.grid.getFilterEditorCriteria(true);
				if (!editorCriteria) editorCriteria = {};

				isc.BizUtil.convertFilterCriteria(result);

				if (config && config.params) {
					result = isc.BizUtil.completeFilterCriteria(
						editorCriteria,
						config.params,
						me._view,
					);
				}

				this.Super("filterData", [result, callback, requestProperties]);

				if (!me._advancedFilter.toggleButton.selected) {
					me.grid.setFilterEditorCriteria(editorCriteria);
				}
			},

			dataProperties: {
				/**
				 * Transforms the data received from the data source.
				 * @param {Array} newData - the new data to transform.
				 * @returns {Array} - the transformed data.
				 */
				transformData: (newData, dsResponse) => {
					// Only process for fetch/filter operations with successful response
					if (
						dsResponse.context?.operationType === "fetch" &&
						dsResponse.status === isc.RPCResponse.STATUS_SUCCESS
					) {
						// Update CSRF token
						this._csrf = dsResponse.httpHeaders["x-csrf-token"];

						// Define summary fields configuration
						const summaryFields = [
							{
								name: "bizFlagComment",
								type: "enum",
								valueMap: ["", "Count", "Avg", "Sum", "Min", "Max"],
								width: 70,
								change: (form, item, value) => {
									this.summaryType = value;
									this.grid.invalidateCache();
									const filterCriteria = this._advancedFilter.toggleButton
										.selected
										? this._advancedFilter.getCriteria()
										: this.grid.getFilterEditorCriteria(true);
									this.grid.filterData(filterCriteria);
								},
							},
						];

						// Configure summary fields based on data source fields
						const fieldNames = this._dataSource.getFieldNames(true);
						summaryFields.length = fieldNames.length - 1;

						fieldNames.forEach((fieldName, i) => {
							if (fieldName !== "bizTagged" && fieldName !== "bizFlagComment") {
								const field = this._dataSource.getField(fieldName);
								let fieldType = "float";
								let editorType = null;

								if (this.summaryType === "Min" || this.summaryType === "Max") {
									fieldType = field.type;
									const excludedTypes = [
										"comboBox",
										"enum",
										"select",
										"bizLookupDescription",
										"boolean",
									];
									if (!excludedTypes.includes(fieldType)) {
										editorType = field.editorType;
									}
								}

								summaryFields[i - 1] = {
									name: fieldName,
									type: fieldType,
									editorType,
									canEdit: false,
									...(fieldType === "float" && {
										formatCellValue: (value) =>
											isc.isA.Boolean(value) ? null : value,
									}),
								};
							}
						});

						this._summaryGrid.setFields(summaryFields);

						// Extract and set summary data
						const summaryData = newData.pop();
						this._summaryGrid.setData([summaryData]);

						// Update grid state
						this.grid.fieldStateChanged();
						this._summaryGrid.startEditing(0, 0, true);
						this._summaryGrid.selectRecord(0, false);
					}
					return newData;
				},
			},

			/**
			 * Determines if a cell can be edited.
			 */
			canEditCell: function (rowNum, colNum) {
				const baseColumnOffset = me.showTag ? 1 : 0;
				return (
					!me._disabled &&
					colNum > baseColumnOffset &&
					this.Super("canEditCell", arguments)
				);
			},

			/**
			 * Handles the change of a field.
			 */
			fieldStateChanged: () => {
				const fieldState = this.getFieldState();

				if (this.showTag) {
					// Combine widths for tag and flag columns
					const expansionOffset = this.grid.canExpandRecords ? 32 : 0;
					fieldState[1] = {
						name: "bizFlagComment",
						width: fieldState[0].width + fieldState[1].width + expansionOffset,
					};
				} else {
					fieldState[1] = {
						name: "bizFlagComment",
						width: fieldState[1].width + (this.grid.canExpandRecords ? 32 : 0),
					};
				}

				fieldState.removeAt(0); // Remove bizTagged
				this._summaryGrid.setFieldState(fieldState);
			},

			scrolled: () => {
				this._summaryGrid.body?.scrollTo(this.grid.body.getScrollLeft(), 0);
			},

			/**
			 * Returns the CSS text for a cell.
			 */
			getCellCSSText: function (record, rowNum, colNum) {
				if (record?.bizTagged) {
					return "font-weight:bold;background-color:#B8D1EA;";
				}
				return this.Super("getCellCSSText", arguments);
			},
		};

		// Configure repeater-specific settings
		if (config.isRepeater) {
			Object.assign(gridConfig, {
				showRollOver: false,
				showSelectedStyle: false,
				showEmptyMessage: false,
				baseStyle: "",
				showHeader: config.showColumnHeaders ?? true,
				bodyBackgroundColor: "white",
				...(!config.showGrid && {
					border: "none",
					bodyBackgroundColor: "#F9F9F9",
				}),
			});
		}

		// Set empty message for non-auto-populate grids
		if (!this.autoPopulate) {
			gridConfig.emptyMessage = "No items shown. Filter the grid.";
		}

		// Apply custom cell height if specified
		if (this.cellHeight) {
			gridConfig.cellHeight = this.cellHeight;
		}

		// Merge additional grid configuration if provided
		if (config.gridConfig) {
			Object.assign(gridConfig, config.gridConfig);
		}

		// Create appropriate grid type based on configuration
		if (config.isTree) {
			// Configure tree-specific properties
			const treeConfig = {
				...gridConfig,
				folderIcon: null,
				loadOnDemand: true,
				dataProperties: {
					openProperty: "_isOpen",
					isFolderProperty: "_isFolder",
					childrenProperty: "_children",
				},
				dataFetchMode: "paged",
			};

			this.grid = isc.TreeGrid.create(treeConfig);
		} else {
			this.grid = isc.ListGrid.create(gridConfig);
		}
	},

	/**
	 * Disables or enables the component and triggers the selection change on the grid.
	 * @param {boolean} disabled - flag indicating whether the component should be disabled (true) or enabled (false).
	 */
	setDisabled: function (disabled) {
		this._disabled = disabled;
		if (this.grid) {
			this.grid.selectionChanged();
		}
	},

	/**
	 * Sets the BizListGrid into pick list mode by updating the lookup, filter parameters, and view.
	 * @param {Object} lookup - the lookup object containing description and relevant details.
	 * @param {Object} filterParams - the unprocessed filter parameters to be used for filtering the data.
	 * @param {Object} view - the view used to process the filter parameters.
	 */
	setLookup: function (lookup, filterParams, view) {
		this._lookup = lookup;
		this._config.params = filterParams;
		this._view = view;
	},

	/**
	 * Refreshes the list view by deselecting all records, invalidating the cache,
	 * applying the appropriate filter criteria, and scrolling to the top row.
	 */
	refresh: function () {
		const topRowNum = this.grid.getVisibleRows()[0];

		this.grid.deselectAllRecords();
		this._eventRowNum = null;
		this._eventColumnNum = null;
		this._eventRecord = null;

		// Invalidate the cache to trigger a refresh of the data
		this.grid.invalidateCache();

		// Apply filter criteria, based on the advanced filter toggle state
		const filterCriteria = this._advancedFilter.toggleButton.selected
			? this._advancedFilter.getCriteria()
			: this.grid.getFilterEditorCriteria(true);
		this.grid.filterData(filterCriteria);

		// Scroll to the top row for the refreshed data
		this.grid.scrollToRow(topRowNum, "top");
	},

	/**
	 * Called when a new record is added to a pick list view.
	 * The pick list view must be refreshed to display the newly added record.
	 * For other list types, this method does not trigger a rerender as it is invoked by form scatter when the grid is not ready.
	 */
	rerender: function () {
		if (this.isPickList) {
			this.refresh();
		}
	},

	/**
	 * Sets the data source for the component and configures the related grid and form settings.
	 *
	 * @param {string} ID - the ID of the data source.
	 * @param {Object} [menuConfig] - optional configuration passed from the menu. It may override certain defaults.
	 * @returns {string} the title of the data source.
	 */
	setDataSource: function (ID, menuConfig) {
		// Switch off any snapshot selected before
		this._clearSnap();

		this._dataSource = isc.DataSource.get(ID);

		this.canCreate = this._dataSource.canCreate;
		this.canUpdate = this._dataSource.canUpdate;
		this.canDelete = this._dataSource.canDelete;
		this.aggregate = this._dataSource.aggregate;

		if (this.aggregate) {
			if (!this._config.isRepeater) {
				this.hideMember(this._toolbar);
				if (!this._config.isTree) {
					this.hideMember(this._summaryGrid);
				}
			}
			this.canCreate = false;
			this.canUpdate = false;
			this.canDelete = false;
		} else {
			if (!this._config.isRepeater) {
				this.showMember(this._toolbar);

				// Disable chart if a tree or a model data source
				this._chartButton.setDisabled(
					this._config.isTree || this._dataSource.ID.contains("__"),
				);

				if (!this._config.isTree) {
					if (
						this.showSummary === undefined ||
						this.showSummary === null ||
						this.showSummary
					) {
						this.showMember(this._summaryGrid);
					} else {
						this.hideMember(this._summaryGrid);
					}
				}
			}
		}

		if (menuConfig) {
			// Set the menu defaults (remember that one instance of BizListGrid is shared for list view)
			this.autoPopulate = true;

			// Override with menu config values if present
			if (menuConfig.autoPopulate !== undefined) {
				this.autoPopulate = menuConfig.autoPopulate;
			}
		}

		this.cellHeight = this._dataSource.cellHeight || null;

		const fields = [];
		if (this.isRepeater || this.aggregate) {
			fields.add({ name: "bizTagged", hidden: true, canHide: false });
		} else if (this.showTag) {
			fields.add({
				name: "bizTagged",
				width: 38,
				align: "center",
				canHide: false,
				canSort: false,
				canToggle: false, // if true, this displays disabled
				canGroupBy: false,
				showHover: false,
				ignoreKeyboardClicks: true,
				recordClick: (
					viewer,
					record,
					recordNum,
					field,
					fieldNum,
					value,
					rawValue,
					editedRecord,
				) => {
					if (record && this.canUpdate && this.canEdit) {
						if (this.tagId) {
							this._eventRecord = record;
							this._eventRowNum = recordNum;
							this._eventColNum = fieldNum;
							record.bizTagged = record.bizTagged ? "UNTAG" : "TAG";
							this.grid.updateData(
								record,
								(dsResponse, data, dsRequest) => {
									if (dsResponse.status >= 0) {
										this._csrf = dsResponse.httpHeaders["x-csrf-token"];
									}
								},
								{
									showPrompt: false,
									params: { _tagId: this.tagId, _csrf: this._csrf },
								},
							);
						} else {
							isc.warn(
								"Select or create a tag first from the tags menu in the list toolbar",
							);
						}
					}
					return false; // do not allow list grid level record click event to fire
				},
			});
		} else {
			fields.add({ name: "bizTagged", hidden: true, canHide: false });
		}

		if (this.isRepeater || this.aggregate) {
			fields.add({ name: "bizFlagComment", hidden: true, canHide: false });
		} else {
			fields.add({
				name: "bizFlagComment",
				width: !this.showTag && this.showSummary ? 80 : 40,
				align: "center",
				canHide: false,
				ignoreKeyboardClicks: true,
				formatCellValue: (value, record) =>
					value ? '<img src="images/flag.gif">' : "",
				recordClick: (
					viewer,
					record,
					recordNum,
					field,
					fieldNum,
					value,
					rawValue,
					editedRecord,
				) => {
					if (this.canUpdate && this.canEdit) {
						this._eventRecord = record;
						this._eventRowNum = recordNum;
						this._eventColNum = fieldNum;
						this._flagForm.editRecord(record);
						this._flagDialog.show();
					}
					return false; // do not allow list grid level record click event to fire
				},
				hoverHTML: function (record, value, rowNum, colNum, grid) {
					return record.bizFlagComment;
				},
			});
		}

		const fieldNames = this._dataSource.getFieldNames(true);
		let hasDetailFields = false;
		let treeFieldNotSet = true;

		fieldNames.forEach((fieldName) => {
			if (fieldName !== "bizTagged" && fieldName !== "bizFlagComment") {
				const dsField = this._dataSource.getField(fieldName);
				if (!dsField.foreignKey) {
					// Not the parent FK tree field
					const gridField = {
						name: fieldName,
						autoFitWidth: false,
						canToggle: false,
					};
					if (treeFieldNotSet) {
						gridField.treeField = true;
						treeFieldNotSet = false;
					}
					if (dsField.canSave === false) {
						gridField.canEdit = false;
					}
					if (!hasDetailFields && dsField.detail) {
						hasDetailFields = true;
					}
					fields.add(gridField);
				}
			}
		});

		this._advancedFilter.setDataSource(this._dataSource);

		this._flagForm.setDataSource(this._dataSource);
		this._flagForm.setFields([
			{
				name: "bizFlagComment",
				type: "richText",
				colSpan: 2,
				height: 175,
				validators: [
					{ type: "lengthRange", min: 0, max: 1024, clientOnly: true },
				],
			},
			{
				type: "button",
				title: "Clear",
				width: 100,
				align: "right",
				startRow: false,
				endRow: false,
				click: () => {
					const commentField = this._flagForm.getField("bizFlagComment");
					if (commentField.getValue() !== "") {
						commentField.setValue("");
						this._flagForm.saveData(
							(dsResponse) => {
								if (dsResponse.status >= 0) {
									this._csrf = dsResponse.httpHeaders["x-csrf-token"];
									this._flagForm.reset();
									this._flagDialog.hide();
								}
							},
							{ params: { _csrf: this._csrf } },
						);
					}
				},
			},
			{
				type: "button",
				title: "Flag",
				width: 100,
				align: "left",
				startRow: false,
				endRow: true,
				click: () => {
					if (this._flagForm.validate(true)) {
						this._flagForm.saveData(
							(dsResponse) => {
								if (dsResponse.status >= 0) {
									this._csrf = dsResponse.httpHeaders["x-csrf-token"];
									this._flagForm.reset();
									this._flagDialog.hide();
								}
							},
							{ params: { _csrf: this._csrf } },
						);
					}
				},
			},
		]);

		if (this.grid) {
			this.removeMember(this.grid);
			this.grid.destroy();
		}
		this._createGrid(this._config, fields, hasDetailFields);

		if (this._config.isTree || this._config.isRepeater) {
			this.addMember(this.grid); // Add to the end - no summary row
		} else {
			this.addMember(this.grid, this.getMembers().length - 1); // Add before the summary row
		}

		if (this.rootIdBinding) {
			this.grid.getDataSource().getField("bizParentId").rootValue =
				"_" + this._view._vm.getValue(this.rootIdBinding);
		} else {
			const bizParentIdField = this.grid
				.getDataSource()
				.getField("bizParentId");
			if (bizParentIdField) {
				bizParentIdField.rootValue = null;
			}
		}

		if (this.autoPopulate) {
			this.grid.filterData(
				this._advancedFilter.toggleButton.selected
					? this._advancedFilter.getCriteria()
					: this.grid.getFilterEditorCriteria(),
			);
		}

		this.grid.selectionChanged(null, false); // Ensure buttons are disabled

		return this._dataSource.getTitle();
	},

	/**
	 * Retrieves and sets the field state of the data grid, ensuring all fields have a width defined.
	 * @returns {Array} the field state, including field names and their corresponding widths.
	 */
	getFieldState: function () {
		// Ensure the widths of all fields are set
		const fieldState = eval(this.grid.getFieldState());

		return fieldState.map((field) => {
			// Handle specific fields 'bizTagged' and 'bizFlagComment'
			if (field === "bizTagged") {
				return { name: "bizTagged", width: 38 };
			}
			if (field === "bizFlagComment") {
				return { name: "bizFlagComment", width: this.showTag ? 40 : 80 };
			}

			// Handle other fields
			if (!field.width) {
				return {
					...field,
					width: this.grid.getFieldWidth(field.name),
				};
			}
			return field;
		});
	},

	/**
	 * Navigates to the edit view, either for a new or existing record, based on the specified parameters.
	 * Handles the conversation continuation logic and ensures the proper context for zooming into the record.
	 *
	 * @param {boolean} zoomToNew - indicates whether to navigate to a new record (true) or an existing one (false).
	 * @param {boolean} contConv - specifies whether to continue the owning view's conversation (true) or start a new one (false).
	 * @param {Object|null|undefined} newParams - a map of parameter names to expressions to evaluate, can be null or undefined.
	 */
	zoom: function (zoomToNew, contConv, newParams) {
		const { grid, _eventRecord, _view, _dataSource } = this;
		let module = null;
		let document = null;
		let bizId = null;

		// Determine module, document, and bizId based on context
		if (!zoomToNew && _eventRecord) {
			module = _eventRecord.bizModule;
			document = _eventRecord.bizDocument;
			bizId = _eventRecord.bizId;
		} else {
			const dotIndex = grid.dataSource.modoc.indexOf(".");
			module = grid.dataSource.modoc.substring(0, dotIndex);
			document = grid.dataSource.modoc.substring(dotIndex + 1);
		}

		const gridRect = grid.body.getPageRect();

		isc.BizUtil.getEditView(module, document, (view) => {
			if (_view) {
				const instance = _view.gather(false); // Don't validate

				// If continuing the conversation
				if (contConv) {
					_view._source = _dataSource.ID.substring(
						_dataSource.ID.lastIndexOf("_") + 1,
					);
					this._zoom(zoomToNew, view, newParams, bizId, instance._c, gridRect);
				} else {
					// If changes need to be applied
					if (instance._apply || _view._vm.valuesHaveChanged()) {
						delete instance._apply;
						_view.saveInstance(true, null, () => {
							_view._source = _dataSource.ID.substring(
								_dataSource.ID.lastIndexOf("_") + 1,
							);
							this._zoom(zoomToNew, view, newParams, bizId, null, gridRect);
						});
					} else {
						// No changes - directly zoom in
						_view._source = _dataSource.ID.substring(
							_dataSource.ID.lastIndexOf("_") + 1,
						);
						this._zoom(zoomToNew, view, newParams, bizId, null, gridRect);
					}
				}
			} else {
				this._zoom(zoomToNew, view, newParams, bizId, null, gridRect);
			}
		});
	},

	/**
	 * Handles the zooming logic for either a new or existing record, and displays the appropriate view in a popup.
	 *
	 * @param {boolean} zoomToNew - indicates whether to navigate to a new record (true) or an existing one (false).
	 * @param {Object} view - the view to display in the popup.
	 * @param {Object|null|undefined} newParams - a map of parameter names to expressions to evaluate, can be null or undefined.
	 * @param {string} bizId - the business ID of the document for editing (if not creating a new record).
	 * @param {Object} _c - additional context or configuration passed to the view methods.
	 * @param {Array} gridRect - the coordinates of the grid that will define the position of the popup.
	 */
	_zoom: function (zoomToNew, view, newParams, bizId, _c, gridRect) {
		if (zoomToNew) {
			isc.WindowStack.popup(gridRect, "New", true, [view]);
			view.newInstance(newParams, null, _c);
		} else {
			const rowRect = [
				gridRect[0],
				this.grid.body.getRowPageTop(this._eventRowNum),
				gridRect[2],
				this.grid.body.getRowSize(this._eventRowNum),
			];
			isc.WindowStack.popup(rowRect, "Edit", true, [view]);
			view.editInstance(bizId, null, _c);
		}
	},

	/**
	 * Handles the selection of a record from a picklist, updating the lookup description and calling necessary functions.
	 *
	 * @param {Object} lookupDescription - the description object for the lookup field being populated.
	 */
	pick: function (lookupDescription) {
		if (this._eventRecord) {
			lookupDescription.setValueMapFromPickList(this._eventRecord);
			lookupDescription.setValue(this._eventRecord.bizId);
			lookupDescription.bizPicked(
				lookupDescription.form,
				lookupDescription,
				this._eventRecord.bizId,
			);
		}

		isc.WindowStack.popoff(false); // Remove the pick popup - no rerender of the parent edit view

		if (this._eventRecord) {
			// Only call changedForServer if it is defined
			if (lookupDescription.changedForServer) {
				lookupDescription.changedForServer(
					lookupDescription.form,
					lookupDescription,
					this._eventRecord.bizId,
				);
			}
			this._eventRecord = null;
		}
	},
});
