isc.ClassFactory.defineClass("BizGrid", "VLayout");
isc.BizGrid.addProperties({
	// the title
	title: null,

	// the main grid
	grid: null,

	// row at which context menu was invoked
	_eventRowNum: null,
	
	// column at which context menu was invoked
	_eventColumnNum: null,

	// record at which context menu was invoked
	_eventRecord: null,
	
	_disabled: false, // whether this widget is disabled or not

	// standard delete item for all grids
	deleteSelectionItem: null,
	
	// standard delete button for all grids
	deleteSelectionButton: null,

	// standard clear selection item for all grids
	clearSelectionItem: null,
	
	// document privileges granted - affects the tools that are enabled
	aggregate: false,
	canCreate: false, 
	canUpdate: false, 
	canDelete: false,
	
	// conditions to evaluate from the view widget defn
	canAdd: true,
	canEdit: true,
	canZoom: true,
	canRemove: true,
	
	// Repeater switches
	isRepeater: false,
	showColumnHeaders: true,
	showGrid: true
});

isc.BizGrid.addMethods({
	initWidget: function(config) {
		if (config.title) {
			this.isGroup = true;
			this.groupTitle = '&nbsp;&nbsp;' + config.title + '&nbsp;&nbsp;';
			this.groupLabelStyleName = 'bizhubBorderLabel';
			this.groupBorderCSS = '1px solid #bfbfbf';
			this.margin = 1;
			this.padding = 10;
			this.groupLabelBackgroundColor = 'transparent';
		}

		this.Super("initWidget", config);
		// NB style isnt applied unless I use the setter here - makes no sense
		if (config.title) {
			this.setStyleName('bizhubRoundedBorder');
		}

		var me = this;

		me.deleteSelectionItem = {
			title: "Delete/Remove Selected", 
			icon: "icons/delete.png",
			enableIf: function(target, menu, item) {
				return ((! me._disabled) && me.canDelete && me.canRemove && me.grid.anySelected());
			},
			click: function() {
				isc.ask(
					"Do you want to delete/remove the selected rows?",
					function(value) {
						if (value) {
							var requestProperties = {};
							if (me._b) { // is a data grid
								requestProperties.params = {};
							} else { // is a list grid
								requestProperties.params = {_csrf: me._csrf}
							}
							
							if (me._view) {
								var instance = me._view.gather(false);
								requestProperties.params._c = instance._c;
							}
							
							me.grid.removeSelectedData(function(dsResponse, data, dsRequest) {
								if (dsResponse) { // is a list grid
									// Assign the CSRF Token from the response header
									me._csrf = dsResponse.httpHeaders['x-csrf-token'];
								}

								me.grid.selectionChanged(null, false);
								me._eventRowNum = null;
								me._eventColumnNum = null;
								me._eventRecord = null;

								if (me._view) { // could be data grid or embedded list grid
									if (me._b) { // is a data grid
										me._view._vm.setValue('_changed', true); // make the view dirty
										me._view._vm.setValue('_apply', true); // post view changes before zooming
									}

									// run any registered event callbacks
									if (me.bizRemoved) {
										me.bizRemoved();
									}
								}
							},
							requestProperties);
						}
					}
				);
			}
		};

		me.deleteSelectionButton = isc.BizUtil.createImageButton(me.deleteSelectionItem.icon, 
																	true,
																	"<b>Delete/Remove</b> selected.",
																	me.deleteSelectionItem.click);
		me.deleteSelectionButton.setDisabled(true);
		
		me.clearSelectionItem = {
			title: "Deselect all", 
			icon: "icons/clearSelection.png",
			click: function() {
				me.grid.deselectAllRecords(); // event data set null by selectionChanged
				me._eventRowNum = null;
				me._eventColumnNum = null;
				me._eventRecord = null;
			}
		};
	},

	refresh: function() {
		this.grid.refresh();
	}
});

isc.ClassFactory.defineClass("BizDataGrid", "BizGrid");
isc.BizDataGrid.addProperties({
	// Buttons that are enabled disabled on grid row selection
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
	
	_mod: null, // module name
	_doc: null, // document name
	_b: null, // binding
	_ordinal: null // the field name to use for user ordering of the records.
});

/*
 * inline: true to add and edit records inline, false to add and edit records in edit view.
 */
isc.BizDataGrid.addMethods({
	initWidget : function(config) {
		this.Super("initWidget", arguments);
		var me = this;

		var newItem = {
			title: "New", 
			icon: "icons/new.png",
			enableIf: function(target, menu, item) {
				return ((! me._disabled) && me.canCreate && me.canAdd);
			},
			click: function() {
				if (config.inline) {
					me.add();
				}
				else {
					me.zoom(true);
				}
			}
		};
		me._zoomItem = {
			title: "Zoom", 
			icon: "icons/zoom.gif",
			click: function() {
				me.zoom(false);
			},
			enableIf: function(target, menu, item) {
				return (me.canZoom && me.grid.anySelected());
			}
		};
		var editItem = {
			title: "Edit", 
			icon: "icons/edit.png", 
			enableIf: function(target, menu, item) {
				return ((! me._disabled) && me.canUpdate && me.canEdit && me.grid.anySelected());
			},
			click: function() {
				if (me.grid.anySelected()) {
					me.grid.startEditing(me._eventRowNum, me._eventColNum);
				}
			}
		};

		me._newButton = isc.BizUtil.createImageButton(newItem.icon, 
														true, 
														"<b>New</b> record.",
														newItem.click);
		me._zoomButton = isc.BizUtil.createImageButton(me._zoomItem.icon, 
														true, 
														"<b>Zoom</b> into record.",
														me._zoomItem.click);
		me._zoomButton.setDisabled(true);
		me._editButton = isc.BizUtil.createImageButton(editItem.icon, 
														true,
														"<b>Edit</b> a record inline.",
														editItem.click);
		me._editButton.setDisabled(true);

		// the context menu of the BizDataGrid
		var contextMenuData = [];
		if (config.editable) {
			if (me.showAdd) {
				contextMenuData.add(newItem);
			}
			if (me.showZoom) {
				contextMenuData.add(me._zoomItem);
			}
			if (me.showEdit) {
				contextMenuData.add(editItem);
			}
			if (me.showRemove) {
				contextMenuData.add(this.deleteSelectionItem);
			}
			if (me.showDeselect) {
				if (contextMenuData.length > 0) {
					contextMenuData.add({isSeparator: true});
				}
				contextMenuData.add(this.clearSelectionItem);
			}
		}
		var contextMenu = isc.Menu.create({showShadow: true, shadowDepth: 10, data: contextMenuData});

		this._createGrid(config, this._fields, contextMenu);

		// assign the grid to the form grids...
		var grids = me._view._grids[me._b];
		if (grids) {} else {
			grids = {};
			me._view._grids[me._b] = grids;
		}
		grids[me.getID()] = me;
		
		if (config.editable) {
			if (config.isRepeater) {} else {
				var toolStripMembers = [];
				if (me.showAdd) {
					toolStripMembers.add(me._newButton);
				}
				if (me.showZoom) {
					toolStripMembers.add(me._zoomButton);
				}
				if (me.showEdit) {
					toolStripMembers.add(me._editButton);
				}
				if (me.showRemove) {
					toolStripMembers.add(me.deleteSelectionButton);
				}
				if (me.showDeselect) {
					if (toolStripMembers.length > 0) {
						toolStripMembers.add("separator");
					}
					toolStripMembers.add(isc.BizUtil.createImageButton(me.clearSelectionItem.icon, 
																		false,
																		"<b>Deselect</b> all.",
																		me.clearSelectionItem.click));
				}

				if (toolStripMembers.length > 0) {
					me.addMember(isc.ToolStrip.create({
						membersMargin: 2,
						layoutMargin: 2,
					    width: '100%',
						members: toolStripMembers
					}));
				}
			}
		}
		// Set grid minHeight the same as the VLayout parent coz there is no toolbar
		if (me.getMembersLength() == 0) {
			me.grid.setMinHeight(me.minHeight);
		}
		me.addMember(me.grid);
	},

	_createGrid: function(config, fields, contextMenu) {
		var me = this;

		var showHeader = true;
		if (me.isRepeater) {
			if (me.showColumnHeaders) {} else {
				showHeader = false;
			}
		}

		var gridConfig = {
			height: "*",
			minHeight: 100,
			autoFetchData: false,
			showHeader: showHeader,
			headerHeight: 30,
			showFilterEditor: false,
			defaultFilterOperator: 'iContains',
			showRollOver: (! me.isRepeater),
			showSelectedStyle: (! me.isRepeater),
			showEmptyMessage: (! me.isRepeater),
			baseStyle: me.isRepeater ? '' : null,
			border: me.isRepeater ? (me.showGrid ? null : 'none') : null,
			bodyBackgroundColor: me.isRepeater ? (me.showGrid ? 'white' : '#F9F9F9') : 'white',
			fields: me._fields,
			selectionType: "single",
			alternateRecordStyles:true,
			canEdit: me.canUpdate && me.canEdit,
			editEvent: 'none',
			neverValidate: false,
			validateByCell: true,
			saveByCell: false,
			validateOnChange: false,
			// change the default of 'enabled' coz it clashes
			recordEnabledProperty: '_enabled',
			canHover: me.isRepeater ? false : true,
			wrapCells: me.wordWrap ? true : false,
		    fixedRecordHeights: me.wordWrap ? false : true,
			canReorderFields: false,
			// can't sort or group by columns if this is reorderable
			canSort: (config._ordinal ? false : true),
			canGroupBy: (config._ordinal ? false : true),
			canReorderRecords: (config._ordinal ? ((config.canUpdate && me.canEdit) ? true : false) : false),
			// reorder the ordinal field
			recordsDropped: function(dropRecords, index, destWidget, sourceWidget) {
				this.reorderData();
			},
			reorderData: function() {
				if (config._ordinal) { // this grid is orderable
					var data = this.getData();
					for (var i = 0, l = data.length; i < l; i++) {
						data[i][config._ordinal] = i + 1;
					}
				}
			},
			dragDataAction: "move",
			autoSaveEdits: true,
			modalEditing: true,
			canFreezeFields: false,
			contextMenu: me.isRepeater ? null : contextMenu,
			showRollOver: true,
			rowClick: function(record, rowNum, colNum) {
				if (record && record.bizId) { // not a group by row
					me._eventRecord = record;
					me._eventRowNum = rowNum;
					me._eventColNum = colNum;
				}

				// ensure that recordClick() on the data source fields get called
				return this.Super("rowClick", arguments);
			},
			rowContextClick: function(record, rowNum, colNum) {
				if (record && record.bizId) { // not a group by row
					this.deselectAllRecords();
					me._eventRecord = record;
					me._eventRowNum = rowNum;
					me._eventColNum = colNum;
					this.selectSingleRecord(record);
					return true;
				}

				return false; // stop normal context menu
			},
			rowDoubleClick: function(record, rowNum, colNum) {
				if (config.editable) { // editable grid
					if ((! record) || record.isFolder) {} else { // group by folder row - so ignore
						me._eventRecord = record;
						me._eventRowNum = rowNum;
						me._eventColNum = colNum;
						if (config.inline) {
							if (me.canUpdate && me.canEdit && me.showEdit && (! me._disabled)) {
								this.startEditing(rowNum, colNum, false);
							}
						}
						else {
							if (me.canZoom && me.showZoom) {
								// blurry is set true in field blur event or grid select event
								// if true, set it to the zoom item
								if (me._view._blurry) {
									me._view._blurry = me._zoomItem;
								}
								else {
									me._zoomItem.click();
								}
							}
						}
					}
				}
				
				// allow normal click processing - ie expand/collapse group row etc
				return this.Super("rowDoubleClick", arguments);
			},
			selectionChanged: function(record, state) { // state is true for selected or false for deselected
				if (this.anySelected()) {
					me._zoomButton.setDisabled(! me.canZoom);
					me._editButton.setDisabled(me._disabled || (! me.canUpdate) || (! me.canEdit));
					me.deleteSelectionButton.setDisabled(me._disabled || (! me.canDelete) || (! me.canRemove));
				}
				else {
					me._zoomButton.setDisabled(true);
					me._editButton.setDisabled(true);
					me.deleteSelectionButton.setDisabled(true);
				}
				me._newButton.setDisabled(me._disabled || (! me.canCreate) || (! me.canAdd));
			},
			selectionUpdated: function(record, recordList) {
				if (me.selectedIdBinding) {
					// NB:- trackChanges switches whether selection should affect the form's dirtiness or not
					if (me.selectedIdTrackChanges) {
						me._view._vm.setValue(me.selectedIdBinding, record ? record.bizId : null);
					}
					else {
						var changes = me._view._vm.valuesHaveChanged();
						me._view._vm.setValue(me.selectedIdBinding, record ? record.bizId : null);
						if (changes) {} else {
							me._view._vm.rememberValues();
						}
					}
				}
				if (me.bizSelected) {
					if (me.showZoom && me.canZoom && config.editable) {
						// if double click is enabled and we have a select event
						// set blurry true and delay the bizSelected call for the double click delay time
						// the bizSelected call and the zoom in call are serialized through the "blurry" method variants generated on the server JS
						if (isc.RPCManager.requestsArePending()) { 
							me._view._blurry = null;
						}
						else {
							me._view._blurry = true;
						}
						me.delayCall('bizSelected', [], this.doubleClickDelay);
					}
					else {
						me.bizSelected();
					}
				}
			},

			canEditCell: function(rowNum, colNum) {
				return (! me._disabled) && this.Super("canEditCell", arguments);
			},

			// set the view dirty on the client-side when an edit is made in the data grid
			editComplete: function (rowNum, colNum, newValues, oldValues, editCompletionEvent) {
				me._view._vm.setValue('_changed', true); // make the view dirty
				me._view._vm.setValue('_apply', true); // post view changes before zooming

				if (me.bizEdited) {
					me.bizEdited();
				}
			}
			
/*
			showRollOverCanvas:true,
    		rollOverCanvasConstructor:isc.HLayout,
    		rollOverCanvasProperties: {
				snapTo:"TL", 
				height:20, 
				width:40,
				members:[
					{_constructor:"Button", 
						icon: "zoom.gif",
	             		click:"isc.say('Zoom record:' + this.echo(this.parentElement.record))", 
	             		height:20, 
	             		width:20
             		},
					{_constructor:"Button",
						icon: "delete.gif",
						click:"isc.say('Delete record:' + this.echo(this.parentElement.record))",
						height:20,
						width:20
					}
				]
			}
*/
		};
		
		if (config.gridConfig) {
			isc.addProperties(gridConfig, config.gridConfig);
		}
		
		me.grid = isc.ListGrid.create(gridConfig);
	},
	
	setDisabled: function(disabled) {
		this._disabled = disabled;
		if (this.grid) {
			this.grid.selectionChanged();
		}
	},
	
	// goes to edit view (on either the context menu or double click)
	zoom: function(zoomToNew) { // boolean - do we want a new record or an existing one
		var me = this;
		var mod = (this._eventRecord ? this._eventRecord.bizModule : this._mod);
		var doc = (this._eventRecord ? this._eventRecord.bizDocument : this._doc);
		isc.BizUtil.getEditView(mod, 
								doc,
								function(view) { // the view
									// determine the view binding
									var viewBinding = ((me._view._b) ? me._view._b + '.' + me._b : me._b);
									var zoomToBizId = (zoomToNew ? null : me._eventRecord.bizId);

									var instance = me._view.gather(true); // validate
									if (instance) { // no form errors
										var gridRect = me.grid.body.getPageRect();
										// these next 2 must be evaluated before a repaint occurs
										var rowTop = me.grid.body.getRowPageTop(me._eventRowNum);
										var rowHeight = me.grid.body.getRowSize(me._eventRowNum);

										if (instance._apply || me._view._vm.valuesHaveChanged()) {
											delete instance._apply;
											// apply changes to current form before zoom in
											me._view.saveInstance(true, null, function() {
												me._zoom(zoomToNew,
															zoomToBizId,
															viewBinding,
															view,
															instance._c,
															gridRect,
															rowTop,
															rowHeight);
											});
										}
										else {
											me._zoom(zoomToNew,
														zoomToBizId,
														viewBinding,
														view,
														instance._c,
														gridRect,
														rowTop,
														rowHeight);
										}
									}
									else {
										isc.warn('You cannot zoom in until you fix the problems found');
									}
								});
	},
	
	_zoom: function(zoomToNew, zoomToBizId, viewBinding, view, _c, gridRect, rowTop, rowHeight) {
		if (zoomToNew) {
			isc.WindowStack.popup(gridRect, "New", false, [view]);
		}
		else {
			var rowRect = [gridRect[0],
	                		rowTop,
	                		gridRect[2],
	                		rowHeight];
			isc.WindowStack.popup(rowRect, "Edit", false, [view]);
		}

		// apply changes on child form
		view.editInstance(zoomToBizId,
							viewBinding, 
							_c,
							true);
	},
	
	add: function() {
//		if (this.bizAdded) {
//			
//		}

//		var me = this;
//		isc.RPCManager.sendRequest({
//			showPrompt: true,
//			evalResult: true,
//			useSimpleHttp: true,
//			httpMethod: 'POST',
//			params: {_mod: moduleName, _doc: documentName},
//			actionURL: SKYVE.Util.CONTEXT_URL + "smartedit",
//			callback: function(rpcResponse, data, rpcRequest) {
//				this.grid.startEditingNew(data);
//			}
//		 });

		// bizId needs to be defined to separate it from grouping expansion rows
		this.grid.startEditingNew({bizId: null});
	},
	
	remove: function(bizId) {
		// remove the array element
		var data = this.grid.data;
		data.removeAt(data.findIndex('bizId', bizId));
		
		// clean up the grid state
		this.grid.deselectAllRecords();
		this._eventRowNum = null;
		this._eventColumnNum = null;
		this._eventRecord = null;
	}
});
