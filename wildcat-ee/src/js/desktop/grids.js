isc.ClassFactory.defineClass("BizGrid", "VLayout");
BizGrid.addProperties({
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
	canCreate: false, 
	canUpdate: false, 
	canDelete: false,
	
	// conditions to evaluate from the view widget defn
	canAdd: true,
	canEdit: true,
	canZoom: true,
	canRemove: true
});

BizGrid.addMethods({
	initWidget : function () {
        this.Super("initWidget", arguments);
		var me = this;

		me.deleteSelectionItem = {
			title: "Delete/Remove Selected", 
			icon: "../images/icons/delete.png",
			enableIf: function(target, menu, item) {
				return ((! me._disabled) && me.canDelete && me.canRemove && me.grid.anySelected());
			},
			click: function() {
				isc.ask(
					"Do you want to delete/remove the selected rows?",
					function(value) {
						if (value) {
							var dsRequest = {};
							
							if (me._view) {
								var instance = me._view.gather(false);
								dsRequest.params = {_c: instance._c};
							}
							
							// There are no DSCallback arguments here even though they are documented.
							me.grid.removeSelectedData(function() {
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
							dsRequest);
						}
					}
				);
			}
		};

		me.deleteSelectionButton = BizUtil.createImageButton(me.deleteSelectionItem.icon, 
																true,
																"<b>Delete/Remove</b> selected records.",
																me.deleteSelectionItem.click);
		me.deleteSelectionButton.setDisabled(true);
		
		me.clearSelectionItem = {
			title: "Clear Selection", 
			icon: "../images/icons/clearSelection.png",
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

isc.ClassFactory.defineClass("BizListGrid", "BizGrid");
BizListGrid.addProperties({
	// the type of aggregate function selected
	summaryType: '',
	
	// the tag currently active
	tagId: null,

	// the currently set data source {with setDataSource()}
	_dataSource: null,

	// the toolbar
	_toolbar:null,
	
	// the filter builder
	_advancedFilter: null,
	
	// the summary grid
	_summaryGrid: null,
	
	_flagForm: null,

	// flag dialog
	_flagDialog: null,
	
	// Buttons that are enabled disabled on grid row selection
	_newButton: null,
	_zoomButton: null,
	_editButton: null,
	_pickButton: null,
	
	// Lookup control - set when this BizListGrid is used as a picklist
	_lookup: null
});

// Config has 4 properties possible
// For ListView functionality
// No parameters
//
// OR
//
// For PickList functionality
// "isPickList" - true
// NB setLookup() sets the _lookup, config.params and _view attributes so that filtering works as per the ListGrid functionality
//
// OR
//
// For ListGrid functionality
// "params" - an array of binding names, operators and string expressions to evaluate with "EditView.toDisplay()" when a view is populated.
//                These parameter evaluations are sent down as filter criteria to the server
// AND
// "_view" - the view that owns this BizListGrid
// AND
// "contConv" - true = use the owning view's conversation for updates, false = start a new conversation when editing
// AND optionally
// bizAdded, bizEdited, bizRemoved event callback functions
BizListGrid.addMethods({
	initWidget: function(config) { // has 4 properties - see above
		this.Super("initWidget", arguments);
		var me = this;
		
		// this is assigned here so that setLookup() can access the config object to set params attribute
		this._config = config;
		
		me._flagDialog = isc.Window.create({
			autoCenter: true,
			isModal: true,
			showModalMask: true,
			canDragReposition: true,
			canDragResize: false,
			showShadow: true,
			shadowSoftness: 10,
			shadowOffset: 0,
			title: 'Flag',
			headerIconDefaults: {src: '../images/flag.gif', width:16, height: 16},
			showMaximizeButton: false,
			showMinimizeButton: false,
			showHeaderIcon: true,
			items: [
				isc.DynamicForm.create({
					padding: 5,
//					margin: 5,
					width:'100%',
					useAllDataSourceFields: false,
					fields: [{name:'bizFlagComment', 
								type:'richText', 
								height:175,
								width: '100%',
								validators: [{type: "lengthRange", min: 0, max: 1024, clientOnly: true}]}]
				}),
				isc.HLayout.create({
					padding: 10,
					membersMargin: 5,
					align: 'right',
					members: [
						isc.IButton.create({
							title: "Clear",
							width: 100,
							click: function() {
								var commentField = me._flagForm.getField('bizFlagComment');
								if (commentField.getValue() != '') {
									commentField.setValue('');
									me._flagForm.saveData(function(dsResponse, data, dsRequest) {
										if (dsResponse.status >= 0) { // success
											me._flagForm.reset(); // ensure form is not dirty before hiding it
											me._flagDialog.hide();
										}
									});
								}
							}
						}),
						isc.IButton.create({
							title: "Flag",
							width: 100,
							click: function() {
								if (me._flagForm.validate(true)) {
									me._flagForm.saveData(function(dsResponse, data, dsRequest) {
										if (dsResponse.status >= 0) { // success
											me._flagForm.reset(); // ensure form is not dirty before hiding it
											me._flagDialog.hide();
										}
									});
								}
							}
						})
					]
				})
	        ],
			width: 580,
			height: 265
		});

		var getAllCriteria = function() {
			// Get the list criteria for advanced filter or for header filter as appropriate
			// If we have defined filter criteria on a listgrid, convert to an advanced criteria
			var result = me._advancedFilter.toggleButton.selected ?
							me._advancedFilter.getCriteria() :
							me.grid.getFilterEditorCriteria();

			// if params are defined, ensure they are added to the filter criteria
			// NB only listgrid's have config.params (and thus me._view is defined)
			if (config && config.params) {
				result = BizUtil.completeFilterCriteria(result, config.params, me._view);
			}
			
			return result;
		};

		// action items
		var newItem = {
			title: "New", 
			icon: "../images/icons/new.png",
			enableIf: function(target, menu, item) {
				return ((! me._disabled) && me.canCreate && me.canAdd);
			},
			click: function() {
				// only list grids (embedded in edit views) have config.contConv & config.params defined
				if (config) {
					var contConv = false;
					if (config.contConv) {
						contConv = config.contConv;
					}

					if (contConv) {
						var changedOnServer = me._view.gather(false)._changed;
						if (changedOnServer || me._view._vm.valuesHaveChanged()) {
							isc.say('There are unsaved changes in the ' + me._view._singular + 
											'.  Save your changes to the ' + me._view._singular + ' first.',
										null,
										{title:'Unsaved Changes!'}
							);
							return;
						}
					}

					var newParams = {};
					if (config.params) {
						BizUtil.addFilterRequestParams(newParams, 
														config.params,
														me._view);
					}
					me.zoom(true, contConv, newParams);
				}
				else {
					me.zoom(true, false);
				}
			}
		};
		me._zoomItem = {
			title: "Zoom", 
			icon: "../images/icons/zoom.gif",
			enableIf: function(target, menu, item) {
				return (me.canZoom && me.grid.anySelected());
			},
			click: function() {
				if (config && config.contConv) {
					var changedOnServer = me._view.gather(false)._changed;
					if (changedOnServer || me._view._vm.valuesHaveChanged()) {
						isc.say('There are unsaved changes in the ' + me._view._singular + 
										'.  Save your changes to the ' + me._view._singular + ' first.',
									null,
									{title:'Unsaved Changes!'}
						);
					}
					else {
						me.zoom(false, true);
					}
				}
				else {
					me.zoom(false, false);
				}
			}
		};
		var editItem = {
			title: "Edit", 
			icon: "../images/icons/edit.png", 
			enableIf: function(target, menu, item) {
				return ((! me._disabled) && me.canUpdate && me.canEdit && me.grid.anySelected());
			},
			click: function() {
				if (me.grid.anySelected()) {
					// Ensure that embedded list grids use their parent view's conversation to edit data
					// in the same way as when zooming in
					if (me._view) { // this is an embedded list grid
						if (config && config.contConv) {
							var instance = me._view.gather(false); // don't validate
							if (instance._changed || me._view._vm.valuesHaveChanged()) {
								isc.say('There are unsaved changes in the ' + me._view._singular + 
												'.  Save your changes to the ' + me._view._singular + ' first.',
											null,
											{title:'Unsaved Changes!'}
								);
							}
							else {
								if (me.grid.saveRequestProperties) {} else {
									me.grid.saveRequestProperties = {};
								}
								if (me.grid.saveRequestProperties.params) {} else {
									me.grid.saveRequestProperties.params = {};
								}
								me.grid.saveRequestProperties.params._c = instance._c;
								me.grid.startEditing(me._eventRowNum, me._eventColNum);
							}
						}
						else {
							me.grid.startEditing(me._eventRowNum, me._eventColNum);
						}
					}
					else {
						if (me.grid.saveRequestProperties && me.grid.saveRequestProperties.params) {
							delete me.grid.saveRequestProperties.params._c;
						}

						me.grid.startEditing(me._eventRowNum, me._eventColNum);
					}
				}
			}
		};
		var pickItem = {
			title: "Pick", 
			icon: "../images/icons/select.png", 
			enableIf: function(target, menu, item) {
				return ! me._disabled;
			},
			click: function() {
				me.pick(me._lookup);
			}
		};

		// toolbar buttons
		me._newButton = BizUtil.createImageButton(newItem.icon, 
													true, 
													"<b>New</b> record.",
													newItem.click);
		me._zoomButton = BizUtil.createImageButton(me._zoomItem.icon, 
													true, 
													"<b>Zoom</b> into record.",
													me._zoomItem.click);
		me._zoomButton.setDisabled(true);
		me._editButton = BizUtil.createImageButton(editItem.icon, 
													true,
													"<b>Edit</b> a record inline.",
													editItem.click);
		me._editButton.setDisabled(true);
		me._pickButton = BizUtil.createImageButton(pickItem.icon, 
													true,
													"<b>Pick</b> this record.",
													pickItem.click);
		me._pickButton.setDisabled(true);

		var clearFilterItem = {
			title: "Clear Filter", 
			icon: "../images/icons/filter_delete.png",
			click: function() {
				me.grid.setFilterEditorCriteria({});
				me._advancedFilter.clearCriteria();
				me.refresh();
			}
		};
		var refreshItem = {title: "Refresh", 
							icon: "../images/icons/refresh.png",
							click: function() {
								me.refresh();
							}};
		var exportItem = {title: "Export Data...", 
							icon: "../images/icons/export.png",
							click: function() {
								// Make the selected and unselected field lists
								var fieldNames = me._dataSource.getFieldNames(true); // no hidden fields
								var unselectedFields = [{name: "bizFlagComment", title: 'Flag', line: 1, width: 100}];
								var selectedFields = [];
								// fieldNames[0] is bizTagged
								// fieldNames[1] is "bizFlagComment"
								var fieldState = me.getFieldState();
								for (var i = 2, l = fieldNames.length; i < l; i++) {
									var field = me._dataSource.getField(fieldNames[i]);
									var dataGridField = me.grid.getField(fieldNames[i]);
									var align = dataGridField ? dataGridField.align : "center";
									if ((fieldState[i].visible === undefined) ||
											(fieldState[i].visible == null) ||
											(fieldState[i].visible)) {
										selectedFields.add({name: fieldNames[i],
																	title: field.title,
																	line: 1,
																	width: fieldState[i].width,
																	align: align});
									}
									else {
										unselectedFields.add({name: fieldNames[i],
																	title: field.title,
																	line: 1,
																	width: 100,
																	align: align});
									}
								}
								
								// Put the filter parameters into this call also
								var allCriteria = getAllCriteria();

								ReportDialog.popupExport(me._dataSource.ID,
															allCriteria,
															unselectedFields, 
															selectedFields);
							}
        };
/*
		var printItem = {title: "Print", 
        	//enabled: false, 
        	icon: "../images/icons/print.png",
        	click: function() {
				isc.Canvas.showPrintPreview(ListView.contents)
        	}
        };
*/

		var contextMenuData = (config && config.isPickList) ? [pickItem] : [];
		contextMenuData.addList([newItem,
		                         	me._zoomItem,
									editItem,
									this.deleteSelectionItem,
									{isSeparator: true},
			        				this.clearSelectionItem,
			        				clearFilterItem,
			        				refreshItem,
			        		        {isSeparator: true},
			        				exportItem]);
		// the context menu of the BizListGrid
		me._contextMenu = isc.Menu.create({
		    showShadow: true,
		    shadowDepth: 10,
		    data: contextMenuData
		});
		
		me._advancedFilter = isc.AdvancedFilter.create({filterableComponent: me, filterableComponentConfig: config});
		me._advancedFilter.toggleButton.click = function() {
			me._advancedFilter.toggleButtonClick();
		};

		// the snap menu in the BizListGrid
		var snapMenu = isc.Menu.create({
		    showShadow: true,
		    shadowDepth: 10,
		    canSelectParentItems: true,
		    data: []
		});

		var snapMenuButton = isc.MenuButton.create({
			autoFit: true,
			padding: 3,
		    title: "No Snapshot",
		    menu: snapMenu,
		    click: function() {
		    	var params = {a: 'L', ID: snapMenuButton.ID, d: me._dataSource.ID};
		    	if (me.snapId) {
		    		params.i = me.snapId;
		    	} 
		    	RPCManager.sendRequest({
					showPrompt: false,
					evalResult: true,
					useSimpleHttp: true,
					httpMethod: 'POST',
					params: params,
					actionURL: BizUtil.URL_PREFIX + 'smartsnap',
					callback: function(rpcResponse, data, rpcRequest) {
						snapMenu.setData(data);
					}
				});

		    	this.Super('click', arguments);
		    }
		});

		snapMenuButton.newSnap = function() {
			isc.askForValue(
				'Enter the new snapshot name', 
				function(value) {
					if (value) {
						RPCManager.sendRequest({
							showPrompt: true,
							evalResult: true,
							useSimpleHttp: true,
							httpMethod: 'POST',
							params: {a: 'N', 
										n: value, 
										d: me._dataSource.ID, 
										s: {criteria: me._advancedFilter.toggleButton.selected ?
														me._advancedFilter.getCriteria() :
														me.grid.getFilterEditorCriteria(),
												advancedCriteriaStyle: me._advancedFilter.getStyle(),
												fieldState: me.grid.getFieldState(),
												sortState: me.grid.getSortState(),
												groupState: me.grid.getGroupState(),
												summaryType: me.summaryType}},
							actionURL: BizUtil.URL_PREFIX + 'smartsnap',
							callback: function(rpcResponse, data, rpcRequest) {
								me.snapId = data.bizId;
								snapMenuButton.setTitle(value);
							}
						});
					}
				},
				{width: 300});
		};

		// called from server
		snapMenuButton.setSnap = function(snapId, title, snapshot) {
			me.snapId = snapId;
			snapMenuButton.setTitle(title);

			if (snapshot) {
				if (snapshot.criteria) {
					if (snapshot.criteria.operator) { // advanced criteria
						me._advancedFilter.toggleButton.select();
						me._advancedFilter.toggleButtonClick();
						me._advancedFilter.setStyle(snapshot.advancedCriteriaStyle);
						
						me.grid.setFilterEditorCriteria({});
						me._advancedFilter.setCriteria(snapshot.criteria);
					}
					else {
						me._advancedFilter.toggleButton.deselect();
						me._advancedFilter.toggleButtonClick();
		
						me.grid.setFilterEditorCriteria(snapshot.criteria);
						me._advancedFilter.clearCriteria();
					}
				}
				if (snapshot.fieldState) {
					me.grid.setFieldState(snapshot.fieldState);
				}
				if (snapshot.sortState) {
					me.grid.setSortState(snapshot.sortState);
				}
				if (snapshot.groupState) {
					me.grid.setGroupState(snapshot.groupState);
				}
				if (snapshot.summaryType) {
					me.summaryType = snapshot.summaryType;
					me._summaryGrid.data[0].bizFlagComment = snapshot.summaryType;
				}
			}
			me.refresh();
		};
		
		// called from server
		snapMenuButton.updateSnap = function(snapId) {
			RPCManager.sendRequest({
				showPrompt: true,
				evalResult: true,
				useSimpleHttp: true,
				httpMethod: 'POST',
				params: {a: 'U', 
							i: snapId,
							s: {criteria: me._advancedFilter.toggleButton.selected ?
											me._advancedFilter.getCriteria() :
											me.grid.getFilterEditorCriteria(),
									advancedCriteriaStyle: me._advancedFilter.getStyle(),
									fieldState: me.grid.getFieldState(),
									sortState: me.grid.getSortState(),
									groupState: me.grid.getGroupState(),
									summaryType: me.summaryType}},
				actionURL: BizUtil.URL_PREFIX + 'smartsnap'
			});
		};

		// called from server
		snapMenuButton.deleteSnap = function(snapId) {
			isc.ask('Do you want to delete this snapshot?',
						function(value) {
							if (value) {
								RPCManager.sendRequest({
									showPrompt: true,
									evalResult: true,
									useSimpleHttp: true,
									httpMethod: 'POST',
									params: {a: 'D', i: snapId},
									actionURL: BizUtil.URL_PREFIX + 'smartsnap',
									callback: function(rpcResponse, data, rpcRequest) {
										snapMenuButton.setSnap(null, 'No Snapshot', null);
									}
								});
							}
						});
		};

		me._clearSnap = function() {
			me.snapId = null;
			snapMenuButton.setTitle('No Snapshot');
		};

		// the tags menu in the BizListGrid
		var tagsMenu = isc.Menu.create({
		    showShadow: true,
		    shadowDepth: 10,
		    canSelectParentItems: true,
		    data: []
		});

		var tagsMenuButton = isc.MenuButton.create({
			autoFit: true,
			padding: 3,
		    title: "No Tag",
		    menu: tagsMenu,
		    click: function() {
		    	var params = {a: 'L', ID: tagsMenuButton.ID};
		    	if (me.tagId) {
		    		params.t = me.tagId;
		    	} 
		    	RPCManager.sendRequest({
					showPrompt: false,
					evalResult: true,
					useSimpleHttp: true,
					httpMethod: 'POST',
					params: params,
					actionURL: BizUtil.URL_PREFIX + 'smarttag',
					callback: function(rpcResponse, data, rpcRequest) {
						tagsMenu.setData(data);
					}
				});
				this.Super('click', arguments);
		    }
		});

		tagsMenuButton.newTag = function() {
			isc.askForValue(
				'Enter the new tag name', 
				function(value) {
					if (value) {
						RPCManager.sendRequest({
							showPrompt: true,
							evalResult: true,
							useSimpleHttp: true,
							httpMethod: 'POST',
							params: {a: 'N', n: value, ID: tagsMenuButton.ID},
							actionURL: BizUtil.URL_PREFIX + 'smarttag',
							callback: function(rpcResponse, data, rpcRequest) {
								me.tagId = data.bizId;
								tagsMenuButton.setTitle(value);
								me.refresh();
							}
						});
					}
				},
				{width: 300});
		};

		tagsMenuButton.setTag = function(tagId, title) {
			me.tagId = tagId;
			tagsMenuButton.setTitle(title);
			me.refresh();
		};
		
		// action can be 
		// L - list all tags, tagId is the select tag id or null and influences the sub-menus
		// T - tag all in the list based on the criteria - tagId is the tag to work with
		// U - untag all in the list based on the criteria - tagId is the tag to work with
		// C - clear all tagged in this tag (not just in the grid) - tagId is the tag to work with
		// N - Create a new tag
		// D - Delete an existing tag
		tagsMenuButton.tagOp = function(tagId, action) {
			if (action == 'C') { // clear
				isc.ask('Do you want to clear all tagged data from this tag?',
							function(value) {
								if (value) {
									privateTagOp(tagId, action);
								}
							});
			}
			else if (action == 'D') {
				isc.ask('Do you want to delete this tag?',
							function(value) {
								if (value) {
									privateTagOp(tagId, action);
								}
							});
			}
			else if ((action == 'T') || (action == 'U')) {
				var count = me.grid.getTotalRows();
				if (count > 10000) {
					isc.ask('There are ' + count + ' rows in this list to ' +
									((action == 'U') ? 'un' : '') + 
									'tag which could take more than 1 minute!  Do you want to continue?',
								function(value) {
									if (value) {
										privateTagOp(tagId, action);
									}
								});
				}
				else if (count > 1000) {
					isc.ask('There are ' + count + ' rows to ' + 
								((action == 'U') ? 'un' : '') + 
								'tag which may take a few seconds.  Do you want to continue?',
								function(value) {
									if (value) {
										privateTagOp(tagId, action);
									}
								});
				}
				else {
					privateTagOp(tagId, action);
				}
			}
			else {
				privateTagOp(tagId, action);
			}
		};

		var privateTagOp = function(tagId, action) {
			var params = {a: action, t: tagId};
			if (action == 'T' || action == 'U') {
				params.d = me._dataSource.ID;
				var criteria = getAllCriteria();
				params.c = criteria;
			}
			RPCManager.sendRequest({
				showPrompt: true,
				evalResult: true,
				useSimpleHttp: true,
				httpMethod: 'POST',
				params: params,
				actionURL: BizUtil.URL_PREFIX + 'smarttag',
				callback: function(rpcResponse, data, rpcRequest) {
					if (action == 'D') {
						tagsMenuButton.setTag(null, 'No Tag');
					}
					me.refresh();
				}
			});
		};
		
		var toolStripMembers = (config && config.isPickList) ? [me._pickButton] : [];
		toolStripMembers.addList([
            me._newButton,
			me._zoomButton,
			me._editButton,
			me.deleteSelectionButton,
			"separator",
			BizUtil.createImageButton(me.clearSelectionItem.icon, 
										false,
										"<b>Clear selected</b> records.",
										me.clearSelectionItem.click),
			BizUtil.createImageButton(clearFilterItem.icon,
										false,
										"<b>Clear filter</b> criteria.",
										clearFilterItem.click),
			BizUtil.createImageButton(refreshItem.icon,
										false,
										"<b>Refresh</b> table data.",
										refreshItem.click),
	        "separator",
			BizUtil.createImageButton(exportItem.icon,
										false,
										"<b>Export</b> table data.",
										exportItem.click),
			"separator",
			me._advancedFilter.toggleButton,
			"separator",
			isc.Label.create({
				width: 60,
			    contents: "Snapshot:"
			}),
			snapMenuButton,
			"separator",
			isc.Label.create({
				width: 30,
			    contents: "Tag:"
			}),
			tagsMenuButton
		]);
		me._toolbar = isc.ToolStrip.create({
			membersMargin: 2,
			layoutMargin: 2,
			width: '100%',
			members: toolStripMembers
		});
		
        me._summaryGrid = isc.ListGrid.create({
			editByCell: true,
			canEditCell: function(rowNum, colNum) {
				return (colNum == 0);
			},
			rowClick: function() {
				this.selectRecord(0, false);
				return false;
			},
			rowDoubleClick: function() {
				this.selectRecord(0, false);
				return false;
			},
			rowContextClick: function() {
				this.selectRecord(0, false);
				return false;
			},
			height: 20,
			leaveScrollbarGap: true,
			autoFetchData: false,
			autoFitData: null,
			showHeader: false,
			showEmptyMessage: false,	
			bodyOverflow: "hidden"
        });

        if (me.title) {
			me.addMember(isc.HTMLFlow.create({contents: '<div class="dataGridTitle">' + me.title + '</div>'}));
		}
		me.addMember(me._toolbar);
		me.addMember(me._advancedFilter);
		me.addMember(me.grid);
		me.addMember(me._summaryGrid);
		
		// _flagForm needs to be assigned after the BizListGrid object has been constructed
		me._flagForm = me._flagDialog.items[0];
		
		if (config && config.name) {
			// assign the grid to the form grids...
			var grids = me._view._grids[config.name];
			if (grids) {} else {
				grids = {};
				me._view._grids[config.name] = grids;
			}
			grids[me.getID()] = me;
		}
	},

	hasDataSource: function() {
		return (this.grid != null);
	},
	
	_createGrid: function(config, fields) {
		var me = this;
		me.grid = isc.ListGrid.create({
			// this is required to enable an edit view (or list view)
			// to set data straight away instead of waiting for it to paint
			autoDraw: true, 
			height: '*',
			leaveScrollbarGap: true,
			// width: '100%', - causes scrollbars under firefox
			autoFetchData: false,
			useAllDataSourceFields: true,
			showHeader: true,
			headerHeight: 30,
			showFilterEditor: (! me._advancedFilter.toggleButton.selected),
			selectionType: "single",
			alternateRecordStyles:true,
			canEdit: true,
			dataSource: me._dataSource,
			fields: fields,
			editEvent: 'none', // no click or double click to enter edit mode
			neverValidate: false,
			validateByCell: true,
			saveByCell: false,
			validateOnChange: false,
			canHover: true,
			canReorderFields: false,
			autoSaveEdits: true,
			modalEditing: true,
			canFreezeFields: false,
			contextMenu: me._contextMenu,
			showRollOver: true,
			canExpandRecords: false,
			expansionMode: 'details',
//			autoFitWidthApproach: 'both', - The summary row doesn't scroll in sync with the list
//gridComponents:[isc.Canvas.create({width: '100%', height:1}), "header", "filterEditor", "body"],
			rowClick: function(record, rowNum, colNum) {
				if (record && record.bizId) { // not a group by row
					me._eventRecord = record;
					me._eventRowNum = rowNum;
					me._eventColNum = colNum;
					this.selectSingleRecord(record);
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
				if (record && record.bizId) { // not a group by row
					me._eventRecord = record;
					me._eventRowNum = rowNum;
					me._eventColNum = colNum;
					if (config && config.isPickList) {
						me.pick(me._lookup);
					}
					else {
						if (me.canZoom) {
							me._zoomItem.click();
						}
					}
				}
	
				return true; // allow normal click processing - ie expand/collapse group row etc
			},
			selectionChanged: function(record, state) { // state is true for selected or false for deselected
				if (this.anySelected()) {
					me._zoomButton.setDisabled(! me.canZoom);
					me._editButton.setDisabled(me._disabled || (! me.canUpdate) || (! me.canEdit));
					me._pickButton.setDisabled(me._disabled);
					me.deleteSelectionButton.setDisabled(me._disabled || (! me.canDelete) || (! me.canRemove));
				}
				else {
					me._zoomButton.setDisabled(true);
					me._editButton.setDisabled(true);
					me._pickButton.setDisabled(true);
					me.deleteSelectionButton.setDisabled(true);
				}
				me._newButton.setDisabled(me._disabled || (! me.canCreate) || (! me.canAdd));
			},
			editComplete: function(rowNum, colNum, newValues, oldValues, editCompletionEvent, dsResponse) {
				if (me.bizEdited) {
					me.bizEdited();
				}
			},
			// override to put summaryRow into the request parameters
			// This ensures that the server sends back an extra summary row
			// grid.dataProperties.transformData() is overridden to expect the summary row
			filterData: function(criteria, callback, requestProperties) {
				var result = criteria;
				
				// ensure summaryType & tagId are sent down in the requestProperties
				if (requestProperties) {
					if (requestProperties.params) {} else {
						requestProperties.params = {};
					}
				}
				else {
					requestProperties = {};
					requestProperties.params = {};
				}
				requestProperties.params._summary = me.summaryType;
				requestProperties.params._tagId = me.tagId;
	
				// if params are defined, ensure they are added to the filter criteria
				// NB config.params is only defined for listgrid's so me._view is defined in this case
				// NB result could be an advanced criteria after this call whereas critiera could be a simple criteria still
				if (config && config.params) {
					result = BizUtil.completeFilterCriteria(result, config.params, me._view);
				}
				
				this.Super("filterData", [result, callback, requestProperties]);

				// The super call above sets the grid's filter editor data, 
				// but 'result' could be an advanced criteria, so reset it if and only if it was a simple criteria to start with
				if (criteria) {
					if (criteria.operator) {
						// Commented this out as a geometry critiera in the grid filter line searches produces an advanced criteria
						// this.setFilterEditorCriteria({});
					}
					else {
						this.setFilterEditorCriteria(criteria);
					}
				}
				else {
					this.setFilterEditorCriteria({});
				}
			},
	
			dataProperties: {
				transformData: function(newData, dsResponse) {
					// only process this if we are doing a fetch/filter operation
					// (by checking for "requestIndex" in the clientContext
					// and we have a success response from the server
					if (dsResponse.context && 
							(dsResponse.context.operationType == 'fetch') && 
							(dsResponse.status == isc.RPCResponse.STATUS_SUCCESS)) {
						// Ensure the summary grid fields match what will be in the data grid
						var fields = [{
							name: "bizFlagComment", 
							type: "enum", 
							valueMap: ["", "Count", "Avg", "Sum", "Min", "Max"],
							width: 70,
//							frozen: false, // Like it to be true but group by descriptions are clipped when group by a grid column
							change: function(form, item, value, oldValue) {
								me.summaryType = value;
								me.grid.invalidateCache();
								me.grid.filterData(me._advancedFilter.toggleButton.selected ?
														me._advancedFilter.getCriteria() :
														me.grid.getFilterEditorCriteria());
							}
						}];

						// Make the count summary fields numeric (if applicable)
						var fieldNames = me._dataSource.getFieldNames(true); // no hidden fields
						fields.setLength(fieldNames.length - 1);
						// fieldNames[0] is "bizTagged"
						// fieldNames[1] is "bizFlagComment"
						for (var i = 2, l = fieldNames.length; i < l; i++) {
							var field = me._dataSource.getField(fieldNames[i]);
							var fieldType = 'float'; // for Count, Sum, Avg
							var editorType = null;
							// Ensure the format stays the same in the summary grid
							if ((me.summaryType == 'Min') || (me.summaryType == 'Max')) {
								fieldType = field.type;
								if ((fieldType != 'comboBox') && 
										(fieldType != 'enum') && 
										(fieldType != 'select') &&
										(fieldType != 'bizLookupDescription') && 
										(fieldType != 'boolean')) {
									editorType = field.editorType;
								}
							}
							fields[i - 1] = {name: fieldNames[i], type: fieldType, editorType: editorType, canEdit: false};
							if (fieldType == 'float') {
								fields[i - 1].formatCellValue = function(value, record, rowNum, colNum, grid) {
									if (isc.isA.Boolean(value)) {
										return null;
									}
									return value;
								};
							}
						}
						me._summaryGrid.setFields(fields);
	
						// pop off the last record in the page from the server as this is the summary row
						var summaryData = newData.pop();
						me._summaryGrid.setData([summaryData]);

						// Ensure that the summary grid fields are in the same state as the data grid
						me.grid.fieldStateChanged();
	
						// Edit the row we've created above, so we get the drop down
						me._summaryGrid.startEditing(0, 0, true);
						me._summaryGrid.selectRecord(0, false);
					}
					return newData;
				}
			},
	
			canEditCell: function(rowNum, colNum) {
				// column zero = tag, column 1 = flag
				return (! me._disabled) && (colNum > 1);
			},
	
			fieldStateChanged: function() {
				// ensure the widths of all fields are set
				var fieldState = me.getFieldState();
	
				// make the first column = the width of the first and second columns together
				// ie the bizTagged and BizFlagComment columns
				fieldState[1].width = fieldState[0].width + fieldState[1].width;
				fieldState.remove(0);
	
				me._summaryGrid.setFieldState(fieldState);
			},
			scrolled: function() {
				if (me._summaryGrid.body) {
					me._summaryGrid.body.scrollTo(me.grid.body.getScrollLeft(), 0);
				}
			},
			getCellCSSText: function (record, rowNum, colNum) {
				if (record.bizTagged) {
		        	return "font-weight:bold;background-color:#B8D1EA;";
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
						icon: "../images/zoom.gif",
	             		click:"isc.say('Zoom record:' + this.echo(this.parentElement.record))", 
	             		height:20, 
	             		width:20
	         		},
					{_constructor:"Button",
						icon: "../images/delete.gif",
						click:"isc.say('Delete record:' + this.echo(this.parentElement.record))",
						height:20,
						width:20
					}
				]
			}
*/
		});
	},

	setDisabled: function(disabled) {
		this._disabled = disabled;
		if (this.grid) {
			this.grid.selectionChanged();
		}
	},

	// use this method to set the BizListGrid into pick list mode
	setLookup: function(lookup, // the lookup (description)
							filterParams, // the un-processed filter params
							view) { // the view used to process the filter params
		this._lookup = lookup;
		this._config.params = filterParams;
		this._view = view;
	},
	
	// Refresh the list view, used by the refresh menu item
	// Used by the server-side, by rerender() below and by editViews to refresh listgrids.
	refresh: function() {
//		var me = this;
//		var selectedBizId = this._eventRecord ? this._eventRecord.bizId : null;
		this.grid.deselectAllRecords();
		this._eventRowNum = null;
		this._eventColumnNum = null;
		this._eventRecord = null;

		this.grid.invalidateCache();
		this.grid.filterData(this._advancedFilter.toggleButton.selected ?
									this._advancedFilter.getCriteria() :
									this.grid.getFilterEditorCriteria());
/*
		this.grid.filterData(this._advancedFilter.toggleButton.selected ?
									this._advancedFilter.getCriteria() :
									this.grid.getFilterEditorCriteria(),
								function(dsResponse, data, dsRequest) {
									if (dsResponse.status >= 0) { // success
										var selectedIndex = data.findIndex('bizId', selectedBizId);
										if (selectedIndex >= 0) {
											me._eventRowNum = selectedIndex;
											me._eventColumnNum = 1; // not the flag column
											me._eventRecord = data.get(selectedIndex);
											me.grid.selectSingleRecord(selectedIndex);
alert('select record ' + selectedIndex + ' ' + me._eventRecord.bizId + " = " + selectedBizId);
											// this method will load data pages but there is no callback method to do the selection after
//											me.grid.scrollRecordIntoView(selectedIndex); // use scrollToRow() in smartclient 8
										}
									}
								});
*/
	},
	
	// Called when a new record is added to a pick list view 
	// The pick view must refresh to display the added record (or rerender(), which is called by WindowStack)
	// Any other type of list does not rerender as this is called by form scatter (when the grid is not ready)
	rerender: function() {
		if (this.isPickList) {
			this.refresh();
		}
	},
	
	// returns the title of the data source
	setDataSource: function(ID) { // the ID of the data source
		var me = this;

		// switch off any snapshot selected before
		me._clearSnap();

		me._dataSource = DataSource.get(ID);

		me.canCreate = me._dataSource.canCreate;
		me.canUpdate = me._dataSource.canUpdate;
		me.canDelete = me._dataSource.canDelete;
				
		var fields = [{name: "bizTagged",
			width: 30,
			align: 'center',
			canHide: false,
			canSort: false,
			canToggle: true,
			canGroupBy: false,
			showHover: false,
//			frozen: false, // Like it to be true but group by descriptions are clipped when group by a grid column
			recordClick: function(viewer, // the parent list grid 
									record, 
									recordNum, 
									field, 
									fieldNum, 
									value, 
									rawValue) {
				if (me.canUpdate && me.canEdit) {
					if (me.tagId) {
						me._eventRecord = record;
						me._eventRowNum = recordNum;
						me._eventColNum = fieldNum;
						if (record.bizTagged) {
							record.bizTagged = 'UNTAG';
						}
						else {
							record.bizTagged = 'TAG';
						}
						me.grid.updateData(record, '', {showPrompt: false, params: {_tagId: me.tagId}});
					}
					else {
						isc.warn('Select or create a tag first from the tags menu in the list toolbar');
					}
				}
				
				return false; // do not allow list grid level record click event to fire
			}
		},
   		{name: "bizFlagComment", 
			width: 40, 
			align: 'center',
			// Cant hide this field as the summary type
			// relies on the real-estate this column uses.
			canHide: false,
//			frozen: false, // Like it to be true but group by descriptions are clipped when group by a grid column
			formatCellValue: function(value) {
				if (value) {
					return '<img src="images/flag.gif">';
				}
				else {
					return '';
				}
			},
			recordClick: function(viewer, // the parent list grid 
									record, 
									recordNum, 
									field, 
									fieldNum, 
									value, 
									rawValue) {
				if (me.canUpdate && me.canEdit) {
					me._eventRecord = record;
					me._eventRowNum = recordNum;
					me._eventColNum = fieldNum;
					me._flagForm.editRecord(record);
					me._flagDialog.show();
				}
				return false; // do not allow list grid level record click event to fire
			},
			hoverHTML: function(record, value, rowNum, colNum, grid) {
				return record.bizFlagComment;
			}
		}];

		var fieldNames = me._dataSource.getFieldNames(true);
		var hasDetailFields = false;
		for (var i = 2; i < fieldNames.length; i++) { // don't include bizTag and bizFlag
			var dsField = me._dataSource.getField(fieldNames[i]);
			var gridField = {name: dsField.name, autoFitWidth: false, canToggle: false}; // don't allow toggling of boolean checkboxes without going into edit mode
			if (dsField.canSave == false) {
				gridField.canEdit = false;
			}
			hasDetailFields = (hasDetailFields || dsField.detail);
			fields.add(gridField);
		}

		me._advancedFilter.setDataSource(me._dataSource);

		me._flagForm.setDataSource(me._dataSource);
		me._flagForm.setFields([{name:'bizFlagComment', 
									type:'richText', 
									height:175,
									validators: [{type: "lengthRange", min: 0, max: 1024, clientOnly: true}]}]);
		
		if (me.grid) {
			me.removeMember(me.grid);
			me.grid.destroy();
		}
		me._createGrid(me._config, fields);
		// Set if the grid can expand based on whether there are detail fields defined
		me.grid.setCanExpandRecords(hasDetailFields);
		me.addMember(me.grid, me.getMembers().length - 1);
		
		me.grid.filterData();
		me.grid.selectionChanged(null, false); // ensure that buttons are disabled

		return me._dataSource.getTitle();
	},

	// get (and set if not already) the data grid's field state
	getFieldState: function() {
		// ensure the widths of all fields are set
		var fieldState = eval(this.grid.getFieldState());
		for (var i = 0, l = fieldState.length; i < l; i++) {
			if (fieldState[i].width) {
				continue;
			}
			else {
				fieldState[i].width = this.grid.getFieldWidth(fieldState[i].name);
			}
		}

		if (this.grid.filterEditor) {
			this.grid.filterEditor.setFieldState(fieldState);
		}

		return fieldState;
	},
	
	// goes to edit view (on either the context menu or double click)
	zoom: function(zoomToNew, // boolean - do we want a new record or an existing one
					contConv, // boolean - continue the owning view's conversation or start a new one
					newParams) { // a map of parameter names to expressions to evaluate - can be null or undefined
		var me = this;
		var module = null;
		var document = null;
		var bizId = null;

		// only get the module and document from the grid row
		// if we are editing it and we have an event record
		if ((! zoomToNew) && me._eventRecord) {
			module = me._eventRecord.bizModule;
			document = me._eventRecord.bizDocument;
			bizId = me._eventRecord.bizId;
// not required			me._eventRecord = null;
		}
		else {
			var dotIndex = this.grid.dataSource.modoc.indexOf('.');
			module = this.grid.dataSource.modoc.substring(0, dotIndex);
			document = this.grid.dataSource.modoc.substring(dotIndex + 1);
		}
		
		var gridRect = me.grid.body.getPageRect();
		BizUtil.getEditView(module, 
								document,
								function(view) { // the view
									if (me._view) { // data grid or embedded list grid
										var instance = me._view.gather(false); // don't validate
										// NB bean must have been saved earlier as checked in the 
										// calling event if using the same conversation
										if (contConv) { // continuing conversation
											me._zoom(zoomToNew, view, newParams, bizId, instance._c, gridRect);
										}
										else { // no conversation propagation
											// if there are any changes in the form, apply them
											if (instance._apply || me._view._vm.valuesHaveChanged()) {
												delete instance._apply;
												// apply changes to current form before zoom in
												me._view.saveInstance(null, function() {
													// now zoom in, after changes applied
													me._zoom(zoomToNew, view, newParams, bizId, null, gridRect);
												});
											}
											else { // no changes - just zoom right in there
												me._zoom(zoomToNew, view, newParams, bizId, null, gridRect);
											}
										}
									}
									else {
										me._zoom(zoomToNew, view, newParams, bizId, null, gridRect);
									}
								});
	},
	
	_zoom: function(zoomToNew, view, newParams, bizId, _c, gridRect) {
		if (zoomToNew) {
			WindowStack.popup(gridRect, "New", true, [view]);
			view.newInstance(newParams, null, _c);
		}
		else {
			var rowRect = [gridRect[0],
	                		this.grid.body.getRowPageTop(this._eventRowNum),
	                		gridRect[2],
	                		this.grid.body.getRowSize(this._eventRowNum)];
			WindowStack.popup(rowRect, "Edit", true, [view]);
			view.editInstance(bizId, null, _c);
		}
	},
	
	// pick the record in a picklist
	pick: function(lookupDescription) {
		if (this._eventRecord) {
			lookupDescription.setValueMapFromPickList(this._eventRecord);
			lookupDescription.setValue(this._eventRecord.bizId);
			lookupDescription.bizPicked(lookupDescription.form, 
											lookupDescription, 
											this._eventRecord.bizId);
		}
		
		WindowStack.popoff(false); // remove the pick popup - no rerender of the parent edit view
		
		if (this._eventRecord) {
			// only call changed for server if it is defined
			if (lookupDescription.changedForServer) {
				lookupDescription.changedForServer(lookupDescription.form, lookupDescription, this._eventRecord.bizId);
			}
			this._eventRecord = null;
		}
	}
});

isc.ClassFactory.defineClass("BizDataGrid", "BizGrid");
BizDataGrid.addProperties({
	// Buttons that are enabled disabled on grid row selection
	_newButton: null,
	_zoomButton: null,
	_editButton: null,
	_mod: null, // module name
	_doc: null, // document name
	_b: null, // binding
	_ordinal: null // the field name to use for user ordering of the records.
});

/*
 * inline: true to add and edit records inline, false to add and edit records in edit view.
 */
BizDataGrid.addMethods({
	initWidget : function (config) {
        this.Super("initWidget", arguments);
		var me = this;

		var newItem = {
			title: "New", 
			icon: "../images/icons/new.png",
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
		var zoomItem = {
			title: "Zoom", 
			icon: "../images/icons/zoom.gif",
			click: function() {
				me.zoom(false);
			},
			enableIf: function(target, menu, item) {
				return (me.canZoom && me.grid.anySelected());
			}
		};
		var editItem = {
			title: "Edit", 
			icon: "../images/icons/edit.png", 
			enableIf: function(target, menu, item) {
				return ((! me._disabled) && me.canUpdate && me.canEdit && me.grid.anySelected());
			},
			click: function() {
				if (me.grid.anySelected()) {
					me.grid.startEditing(me._eventRowNum, me._eventColNum);
				}
			}
		};

		me._newButton = BizUtil.createImageButton(newItem.icon, 
													true, 
													"<b>New</b> record.",
													newItem.click);
		me._zoomButton = BizUtil.createImageButton(zoomItem.icon, 
													true, 
													"<b>Zoom</b> into record.",
													zoomItem.click);
		me._zoomButton.setDisabled(true);
		me._editButton = BizUtil.createImageButton(editItem.icon, 
													true,
													"<b>Edit</b> a record inline.",
													editItem.click);
		me._editButton.setDisabled(true);

		// the context menu of the BizDataGrid
		var contextMenu = (config.editable) ? 
							isc.Menu.create({
							    showShadow: true,
							    shadowDepth: 10,
							    data: [
									newItem,
									zoomItem,
									editItem,
									this.deleteSelectionItem,
							        {isSeparator: true},
									this.clearSelectionItem
							    ]
							}) :
							isc.Menu.create({data: []});
		
		me.grid = isc.ListGrid.create({
			height: "*",
			autoFetchData: false,
			headerHeight: 30,
			showFilterEditor: false,
			fields: me._fields,
			selectionType: "single",
			alternateRecordStyles:true,
			canEdit: me.canUpdate && me.canEdit,
			editEvent: ((me.canUpdate && me.canEdit && config.editable) ? (config.inline ? 'doubleClick' : 'none') : 'none'),
			neverValidate: false,
			validateByCell: true,
			saveByCell: false,
			validateOnChange: false,
			canHover: true,
			wrapCells: me.wordWrap ? true : false,
		    fixedRecordHeights: me.wordWrap ? false : true,
			canReorderFields: false,
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
			contextMenu: contextMenu,
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
				if (record && record.bizId && config.editable) { // not a group by row
					me._eventRecord = record;
					me._eventRowNum = rowNum;
					me._eventColNum = colNum;
					this.selectSingleRecord(record);
					if (config.inline) {
						// do nothing as the double click event is set in the list grid configuration.
					}
					else {
						if (me.canZoom) {
							me.zoom(false);
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
			
			canEditCell: function(rowNum, colNum) {
				return ! me._disabled;
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
						icon: "../images/zoom.gif",
	             		click:"isc.say('Zoom record:' + this.echo(this.parentElement.record))", 
	             		height:20, 
	             		width:20
             		},
					{_constructor:"Button",
						icon: "../images/delete.gif",
						click:"isc.say('Delete record:' + this.echo(this.parentElement.record))",
						height:20,
						width:20
					}
				]
			}
*/
		});

		// assign the grid to the form grids...
		var grids = me._view._grids[me._b];
		if (grids) {} else {
			grids = {};
			me._view._grids[me._b] = grids;
		}
		grids[me.getID()] = me;
		
		if (me.title) {
			me.addMember(isc.HTMLFlow.create({contents: '<div class="dataGridTitle">' + me.title + '</div>'}));
		}
		
		if (config.editable) {
			me.addMember(isc.ToolStrip.create({
				membersMargin: 2,
				layoutMargin: 2,
			    width: '100%',
				members: [
				    me._newButton,
				    me._zoomButton,
					me._editButton,
					me.deleteSelectionButton,
					"separator",
					BizUtil.createImageButton(me.clearSelectionItem.icon, 
												false,
												"<b>Clear selected</b> records.",
												me.clearSelectionItem.click)
				]
			}));
		}
		me.addMember(me.grid);
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
		BizUtil.getEditView(mod, 
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
											me._view.saveInstance(null, function() {
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
			WindowStack.popup(gridRect, "New", false, [view]);
		}
		else {
			var rowRect = [gridRect[0],
	                		rowTop,
	                		gridRect[2],
	                		rowHeight];
			WindowStack.popup(rowRect, "Edit", false, [view]);
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
//		RPCManager.sendRequest({
//			showPrompt: true,
//			evalResult: true,
//			useSimpleHttp: true,
//			httpMethod: 'POST',
//			params: {_mod: moduleName, _doc: documentName},
//			actionURL: BizUtil.URL_PREFIX + "smartedit",
//			callback: function(rpcResponse, data, rpcRequest) {
//				this.grid.startEditingNew(data);
//			}
//		 });

		this.grid.startEditingNew();
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
