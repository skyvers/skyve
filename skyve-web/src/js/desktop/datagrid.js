/**
 * Implements the BizGrid UI component.
 * Extends VLayout from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizGrid", "VLayout");

isc.BizGrid.addProperties({
	grid: null,
	_eventRowNum: null, // Row at which context menu was invoked
	_eventColumnNum: null, // Column at which context menu was invoked
	_eventRecord: null, // Record at which context menu was invoked
	_disabled: false, // Whether this widget is disabled or not
	deleteSelectionItem: null, // Standard delete item for all grids
	deleteSelectionButton: null, // Standard delete button for all grids
	clearSelectionItem: null, // Standard clear selection item for all grids

	// Document privileges granted
	aggregate: false,
	canCreate: false,
	canUpdate: false,
	canDelete: false,

	// Conditions to evaluate from the view widget definition
	canAdd: true,
	canEdit: true,
	canZoom: true,
	canRemove: true,

	// Repeater switches
	isRepeater: false,
	showColumnHeaders: true,
	showGrid: true,
});

isc.BizGrid.addMethods({
	initWidget: function (config) {
		this.Super("initWidget", arguments);

		// Delete selection item
		this.deleteSelectionItem = {
			title: "Delete/Remove Selected",
			name: "deleteRemoveSelected",
			icon: "icons/delete.png",
			enableIf: () =>
				!this._disabled &&
				this.canDelete &&
				this.canRemove &&
				this.grid.anySelected(),
			click: () => {
				isc.ask("Do you want to delete/remove the selected rows?", (value) => {
					if (value) {
						// If a list grid, retrieve CSRF properties
						const requestProperties = {
							params: this._b ? {} : { _csrf: this._csrf },
						};

						if (this._view) {
							const instance = this._view.gather(false);
							requestProperties.params._c = instance._c;
						}

						this.grid.removeSelectedData((dsResponse, data, dsRequest) => {
							// If a list grid, assign the CSRF token from the response header
							if (dsResponse) {
								this._csrf = dsResponse.httpHeaders["x-csrf-token"];
							}

							this.grid.selectionChanged(null, false);
							this._eventRowNum = null;
							this._eventColumnNum = null;
							this._eventRecord = null;

							// If a data grid or embedded list grid...
							if (this._view) {
								// If a data grid...
								if (this._b) {
									this._view._vm.setValue("_changed", true); // Make view dirty
									this._view._vm.setValue("_apply", true); // Post view changes before zoom
								}

								// Run any registered event callbacks
								if (this.bizRemoved) {
									this.bizRemoved();
								}
							}
						}, requestProperties);
					}
				});
			},
		};

		// Delete selection button
		this.deleteSelectionButton = isc.BizUtil.createImageButton(
			this.deleteSelectionItem.name,
			this.deleteSelectionItem.icon,
			true,
			"<b>Delete/Remove</b> selected.",
			this.deleteSelectionItem.click,
		);
		this.deleteSelectionButton.setDisabled(true);

		// Clear selection item
		this.clearSelectionItem = {
			title: "Deselect all",
			icon: "icons/clearSelection.png",
			click: () => {
				this.grid.deselectAllRecords();
				this._eventRowNum = null;
				this._eventColumnNum = null;
				this._eventRecord = null;
			},
		};
	},

	/**
	 * Refreshes the grid.
	 */
	refresh: function () {
		this.grid.refresh();
	},
});

/**
 * Implements the BizDataGrid UI component.
 * Extends BizGrid implemented above.
 */
isc.ClassFactory.defineClass("BizDataGrid", "BizGrid");

isc.BizDataGrid.addProperties({
	// Buttons that are enabled/disabled on grid row selection
	_newButton: null,
	_zoomButton: null,
	_popoutButton: null,
	_editButton: null,

	// Switches to turn off tool buttons / menu items
	showAdd: true,
	showZoom: true,
	showEdit: true,
	showRemove: true,
	showDeselect: true,

	_mod: null, // Module name
	_doc: null, // Document name
	_b: null, // Binding
	_ordinal: null, // Field name to use for user ordering of the records
});

isc.BizDataGrid.addMethods({
	initWidget(config) {
		this.Super("initWidget", arguments);

		// New item
		const newItem = {
			title: "New",
			name: "new",
			icon: "icons/new.png",
			enableIf: () => !this._disabled && this.canCreate && this.canAdd,
			click: () => (config.inline ? this.add() : this.zoom(true)),
		};

		// Zoom item
		this._zoomItem = {
			title: "Zoom",
			name: "zoom",
			icon: "icons/zoom.gif",
			click: () => this.zoom(false),
			enableIf: () => this.canZoom && this.grid.anySelected(),
		};

		// Edit item
		const editItem = {
			title: "Edit",
			name: "edit",
			icon: "icons/edit.png",
			enableIf: () =>
				!this._disabled &&
				this.canUpdate &&
				this.canEdit &&
				this.grid.anySelected(),
			click: () => {
				if (this.grid.anySelected()) {
					this.grid.startEditing(this._eventRowNum, this._eventColNum);
				}
			},
		};

		// Create buttons
		this._newButton = isc.BizUtil.createImageButton(
			newItem.name,
			newItem.icon,
			true,
			"<b>New</b> record.",
			newItem.click,
		);

		this._zoomButton = isc.BizUtil.createImageButton(
			this._zoomItem.name,
			this._zoomItem.icon,
			true,
			"<b>Zoom</b> into record.",
			this._zoomItem.click,
		);
		this._zoomButton.setDisabled(true);

		this._editButton = isc.BizUtil.createImageButton(
			editItem.name,
			editItem.icon,
			true,
			"<b>Edit</b> a record inline.",
			editItem.click,
		);
		this._editButton.setDisabled(true);

		// Context menu
		const contextMenuData = [];
		if (config.editable) {
			if (this.showAdd) contextMenuData.push(newItem);
			if (this.showZoom) contextMenuData.push(this._zoomItem);
			if (this.showEdit) contextMenuData.push(editItem);
			if (this.showRemove) contextMenuData.push(this.deleteSelectionItem);
			if (this.showDeselect) {
				if (contextMenuData.length > 0) {
					contextMenuData.push({ isSeparator: true });
				}
				contextMenuData.push(this.clearSelectionItem);
			}
		}

		const contextMenu = isc.Menu.create({
			showShadow: true,
			shadowDepth: 10,
			data: contextMenuData,
		});

		// Create the grid
		this._createGrid(config, this._fields, contextMenu);

		// Assign the grid to the form grids
		if (!this._view._grids[this._b]) {
			this._view._grids[this._b] = {};
		}
		this._view._grids[this._b][this.getID()] = this;

		// Toolstrip
		if (config.editable && !config.isRepeater) {
			const toolStripMembers = [];
			if (this.showAdd) toolStripMembers.push(this._newButton);
			if (this.showZoom) toolStripMembers.push(this._zoomButton);
			if (this.showEdit) toolStripMembers.push(this._editButton);
			if (this.showRemove) toolStripMembers.push(this.deleteSelectionButton);
			if (this.showDeselect) {
				if (toolStripMembers.length > 0) toolStripMembers.push("separator");
				toolStripMembers.push(
					isc.BizUtil.createImageButton(
						this.clearSelectionItem.icon,
						false,
						"<b>Deselect</b> all.",
						this.clearSelectionItem.click,
					),
				);
			}

			if (toolStripMembers.length > 0) {
				this.addMember(
					isc.ToolStrip.create({
						membersMargin: 2,
						layoutMargin: 2,
						width: "100%",
						members: toolStripMembers,
					}),
				);
			}
		}

		// Set grid minHeight if no toolbar
		if (this.getMembersLength() === 0) {
			this.grid.setMinHeight(this.minHeight);
		}
		this.addMember(this.grid);
	},

	/**
	 * Creates the grid.
	 * @param {Object} config - the configuration object.
	 * @param {Array} fields - the grid fields.
	 * @param {isc.Menu} contextMenu - the context menu.
	 */
	_createGrid: function (config, fields, contextMenu) {
		const me = this; // Required as parent and child scope is required

		const showHeader = !this.isRepeater || this.showColumnHeaders;

		const gridConfig = {
			height: "*",
			minHeight: 100,
			autoFetchData: false,
			showHeader,
			headerHeight: 30,
			showFilterEditor: false,
			defaultFilterOperator: "iContains",
			showSelectedStyle: !this.isRepeater,
			showEmptyMessage: !this.isRepeater,
			baseStyle: this.isRepeater ? "" : null,
			border: this.isRepeater ? (this.showGrid ? null : "none") : null,
			bodyBackgroundColor: this.isRepeater
				? this.showGrid
					? "white"
					: "#F9F9F9"
				: "white",
			fields,
			selectionType: "single",
			alternateRecordStyles: true,
			canEdit: this.canUpdate && this.canEdit,
			editEvent: "none",
			neverValidate: false,
			validateByCell: true,
			saveByCell: false,
			validateOnChange: false,
			recordEnabledProperty: "_enabled",
			canHover: !this.isRepeater,
			wrapCells: !!this.wordWrap,
			fixedRecordHeights: !this.wordWrap,
			canReorderFields: false,
			canSort: !config._ordinal,
			canGroupBy: !config._ordinal,
			canReorderRecords: !!config._ordinal && this.canUpdate && this.canEdit,
			recordsDropped: function (dropRecords, index, destWidget, sourceWidget) {
				this.reorderData();
			},
			reorderData: function () {
				if (config._ordinal) {
					const data = this.getData();
					data.forEach((record, index) => {
						record[config._ordinal] = index + 1;
					});
				}
			},
			dragDataAction: "move",
			autoSaveEdits: true,
			modalEditing: true,
			canFreezeFields: false,
			contextMenu: this.isRepeater ? null : contextMenu,
			showRollOver: true,
			rowClick: function (record, rowNum, colNum) {
				if (record?.bizId) {
					me._eventRecord = record;
					me._eventRowNum = rowNum;
					me._eventColNum = colNum;
				}
				return this.Super("rowClick", arguments);
			},
			rowContextClick: function (record, rowNum, colNum) {
				if (record?.bizId) {
					this.deselectAllRecords();
					me._eventRecord = record;
					me._eventRowNum = rowNum;
					me._eventColNum = colNum;
					this.selectSingleRecord(record);
					return true;
				}
				return false;
			},
			rowDoubleClick: function (record) {
				if (config.editable && record && !record.isFolder) {
					me._eventRecord = record;
					if (config.inline) {
						if (me.canUpdate && me.canEdit && me.showEdit && !me._disabled) {
							this.startEditing(me._eventRowNum, me._eventColNum, false);
						}
					} else if (me.canZoom && me.showZoom) {
						if (me._view._blurry) {
							me._view._blurry = me._zoomItem;
						} else {
							me._zoomItem.click();
						}
					}
				}
				return this.Super("rowDoubleClick", arguments);
			},
			selectionChanged: function () {
				const hasSelection = this.anySelected();
				me._zoomButton.setDisabled(!hasSelection || !me.canZoom);
				me._editButton.setDisabled(
					!hasSelection || me._disabled || !me.canUpdate || !me.canEdit,
				);
				me.deleteSelectionButton.setDisabled(
					!hasSelection || me._disabled || !me.canDelete || !me.canRemove,
				);
				me._newButton.setDisabled(me._disabled || !me.canCreate || !me.canAdd);
			},
			selectionUpdated: function (record) {
				if (me.selectedIdBinding) {
					const changes = me._view._vm.valuesHaveChanged();
					me._view._vm.setValue(me.selectedIdBinding, record?.bizId || null);
					if (!changes) me._view._vm.rememberValues();
				}
				if (me.bizSelected) {
					if (me.showZoom && me.canZoom && config.editable) {
						if (isc.RPCManager.requestsArePending()) {
							me._view._blurry = null;
						} else {
							me._view._blurry = true;
						}
						me.delayCall("bizSelected", [], this.doubleClickDelay);
					} else {
						me.bizSelected();
					}
				}
			},
			canEditCell: function (rowNum, colNum) {
				return !me._disabled && this.Super("canEditCell", arguments);
			},
			editComplete: (
				rowNum,
				colNum,
				newValues,
				oldValues,
				editCompletionEvent,
			) => {
				this._view._vm.setValue("_changed", true);
				this._view._vm.setValue("_apply", true);
				if (this.bizEdited) this.bizEdited();
			},
		};

		if (config.gridConfig) {
			isc.addProperties(gridConfig, config.gridConfig);
		}

		this.grid = isc.ListGrid.create(gridConfig);
	},

	/**
	 * Sets the disabled state of the grid.
	 * @param {boolean} disabled - whether the grid is disabled.
	 */
	setDisabled: function (disabled) {
		this._disabled = disabled;
		if (this.grid) {
			this.grid.selectionChanged();
		}
	},

	/**
	 * Zooms into a record.
	 * @param {boolean} zoomToNew - whether to zoom into a new record.
	 */
	zoom: function (zoomToNew) {
		const mod = this._eventRecord?.bizModule || this._mod;
		const doc = this._eventRecord?.bizDocument || this._doc;

		isc.BizUtil.getEditView(mod, doc, (view) => {
			const viewBinding = this._view._b ? `${this._view._b}.${this._b}` : this._b;
			const zoomToBizId = zoomToNew ? null : this._eventRecord.bizId;

			const instance = this._view.gather(true);
			if (instance) {
				const gridRect = this.grid.body.getPageRect();
				const rowTop = this.grid.body.getRowPageTop(this._eventRowNum);
				const rowHeight = this.grid.body.getRowSize(this._eventRowNum);

				if (instance._apply || this._view._vm.valuesHaveChanged()) {
					delete instance._apply;
					this._view.saveInstance(true, null, (data, success) => {
						if (success) {
							this._zoom(
								zoomToNew,
								zoomToBizId,
								viewBinding,
								view,
								instance._c,
								gridRect,
								rowTop,
								rowHeight,
							);
						}
					});
				} else {
					this._zoom(
						zoomToNew,
						zoomToBizId,
						viewBinding,
						view,
						instance._c,
						gridRect,
						rowTop,
						rowHeight,
					);
				}
			} else {
				isc.warn("You cannot zoom in until you fix the problems found");
			}
		});
	},

	/**
	 * Handles the zoom logic.
	 * @param {boolean} zoomToNew - whether to zoom into a new record.
	 * @param {string} zoomToBizId - the business ID of the record to zoom into.
	 * @param {string} viewBinding - the view binding.
	 * @param {isc.View} view - the view to zoom into.
	 * @param {string} _c - the context identifier.
	 * @param {Array} gridRect - the grid rectangle.
	 * @param {number} rowTop - the top position of the row.
	 * @param {number} rowHeight - the height of the row.
	 */
	_zoom: function (
		zoomToNew,
		zoomToBizId,
		viewBinding,
		view,
		_c,
		gridRect,
		rowTop,
		rowHeight,
	) {
		const rowRect = [gridRect[0], rowTop, gridRect[2], rowHeight];
		isc.WindowStack.popup(
			zoomToNew ? gridRect : rowRect,
			zoomToNew ? "New" : "Edit",
			false,
			[view],
		);

		view.editInstance(zoomToBizId, viewBinding, _c, true);
	},

	/**
	 * Adds a new record to the grid.
	 */
	add: function () {
		this.grid.startEditingNew({ bizId: null });
	},

	/**
	 * Removes a record from the grid.
	 * @param {string} bizId - the ID of the record to remove.
	 */
	remove: function (bizId) {
		const data = this.grid.data;
		data.removeAt(data.findIndex("bizId", bizId));
		this.grid.deselectAllRecords();
		this._eventRowNum = null;
		this._eventColumnNum = null;
		this._eventRecord = null;
	},
});
