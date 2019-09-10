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

	return this.Super("handleError", arguments);
};
Date.setShortDisplayFormat("toEuropeanShortDate");
Date.setNormalDisplayFormat("toEuropeanShortDate");
Date.setInputFormat("DMY");

isc.ListGrid.addProperties({
	getFilterEditorType: function(field) {
	    // Simple case: support explicit filterEditorType on the field
	    if (field.filterEditorType != null) return field.filterEditorType;

	    // TODO: re-implement this once RecordEditor correctly returns AdvancedCriteria
	    var ds = this.getDataSource();
//	    if (isc.SimpleType.inheritsFrom(field.type, "date") &&  ds &&
//	        ds.supportsAdvancedCriteria())
//	    {
//	        return "MiniDateRangeItem";
//	    }

	    var type = field.type;
	    var isFileType = (type == this._$binary || type == this._$file ||
	                        type == this._$imageFile);

	    if (isFileType && field.editorType == null) {
	        if (field.filenameSuppressed || ds && ds.getFilenameField && ds.getFilenameField(field.name) == null) {
	            return "StaticTextItem";
	        } else {
	            return "TextItem";
	        }
	    }

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

// isc.ResultSet._willFetchData() doesn't cater for SearchOperator.requiresServer = true.
// So for spatial operators, I will force a fetch.
isc.ResultSet.addMethods({
	skyveSetCriteria: isc.ResultSet.getPrototype().setCriteria,
	setCriteria: function(newCriteria) {
		var result = this.skyveSetCriteria(newCriteria);
		if (newCriteria && JSON.stringify(newCriteria).match(/"operator"\s*:\s*"geo/)) {
			this.invalidateCache();
		}
		return result;
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
isc.DataSource.addSearchOperator({ID:'geoEquals',
	title: 'Equals',
	fieldTypes:['geometry'],
	valueType: 'fieldType',
	requiresServer: true,
	condition: function(value, record, fieldName, criterion, operator) {
		return true;
	},
	compareCriteria: function(newCriterion, oldCriterion) {
		return -1;
	}
});
isc.DataSource.addSearchOperator({ID:'geoDisjoint',
	title: 'Disjoint',
	fieldTypes:['geometry'],
	valueType: 'fieldType',
	requiresServer: true,
	condition: function(value, record, fieldName, criterion, operator) {
		return true;
	},
	compareCriteria: function(newCriterion, oldCriterion) {
		return -1;
	}
});
isc.DataSource.addSearchOperator({ID:'geoIntersects',
	title: 'Intersects',
	fieldTypes:['geometry'],
	valueType: 'fieldType',
	requiresServer: true,
	condition: function(value, record, fieldName, criterion, operator) {
		return true;
	},
	compareCriteria: function(newCriterion, oldCriterion) {
		return -1;
	}
});
isc.DataSource.addSearchOperator({ID:'geoTouches',
	title: 'Touches',
	fieldTypes:['geometry'],
	valueType: 'fieldType',
	requiresServer: true,
	condition: function(value, record, fieldName, criterion, operator) {
		return true;
	},
	compareCriteria: function(newCriterion, oldCriterion) {
		return -1;
	}});
isc.DataSource.addSearchOperator({ID:'geoCrosses',
	title: 'Crosses',
	fieldTypes:['geometry'],
	valueType: 'fieldType',
	requiresServer: true,
	condition: function(value, record, fieldName, criterion, operator) {
		return true;
	},
	compareCriteria: function(newCriterion, oldCriterion) {
		return -1;
	}
});
isc.DataSource.addSearchOperator({ID:'geoWithin',
	title: 'Within',
	fieldTypes:['geometry'],
	valueType: 'fieldType',
	requiresServer: true,
	condition: function(value, record, fieldName, criterion, operator) {
		return true;
	},
	compareCriteria: function(newCriterion, oldCriterion) {
		return -1;
	}
});
isc.DataSource.addSearchOperator({ID:'geoContains',
	title: 'Contains',
	fieldTypes:['geometry'],
	valueType: 'fieldType',
	requiresServer: true,
	condition: function(value, record, fieldName, criterion, operator) {
		return true;
	},
	compareCriteria: function(newCriterion, oldCriterion) {
		return -1;
	}
});
isc.DataSource.addSearchOperator({ID:'geoOverlaps',
	title: 'Overlaps',
	fieldTypes:['geometry'],
	valueType: 'fieldType',
	requiresServer: true,
	condition: function(value, record, fieldName, criterion, operator) {
		return true;
	},
	compareCriteria: function(newCriterion, oldCriterion) {
		return -1;
	}
});

// TODO Leave as modals for now and see how error tightening on server goes
//isc.RPCManager.promptStyle = "cursor";

// utility class
isc.defineClass("BizUtil");
isc.BizUtil.addClassProperties({
	// Map of modules loaded -> views -> used & unused views
	_modules: {},
	
	// List of picklists to use (for pick views)
	_unusedPickLists: [],
	
	// Data source for the "previous values" mechanism on text fields.
	PREVIOUS_VALUES_DATA_SOURCE: isc.RestDataSource.create({
		dataFormat: 'json',
		jsonPrefix: '',
		jsonSuffix: '',
		dataURL: "smartprev",
		fields: [{name: 'value', type: 'text'}]
	})
});
isc.BizUtil.addClassMethods({
	_currentView: null, // the view currently displayed

	getCurrentView: function() {
		return isc.BizUtil._currentView;
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
	
	// returns an ToolStripButton
	createImageButton: function(icon, // src relative to isomorphic directory - use ../images/ etc
								hasDisabledIcon, // true to look for disabled icon ie icon_Disabled
								tooltip, // the tooltip to add to the button
								click) { // function to call when clicked
		return isc.ToolStripButton.create({
			icon: icon,
			iconAlign: "center",
			showDisabledIcon: hasDisabledIcon,
			showDownIcon: false,
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
			align: 'right',
			height: 1,
			membersMargin: 1,
			members:[
				isc.IButton.create({
					autoFit: true,
					title: buttonTitle,
					icon: buttonIcon,
					showDisabledIcon: buttonHasDisabledIcon,
					canHover: true,
					getHoverHTML: function() {return buttonTooltip;},
					click: buttonClick
				}),
				isc.MenuButton.create({
					title: null,
					width: 26,
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
	
	createUploadButton: function(contentFormItem, // the item this upload button will live in
									image) { // whether this is a contentImage or a contentLink
		return isc.BizUtil.createSplitButton(
			'Upload', 
			null, 
			false, 
			'Upload content', 
			function() {
				var instance = contentFormItem.form._view.gather(false);
				var url = (image ? 'image' : 'content') + 'Upload.xhtml?_n=' + contentFormItem.name.replaceAll('_', '.') + 
							'&_c=' + instance._c;
				if (contentFormItem.form._view._b) {
					url += '&_b=' + contentFormItem.form._view._b.replaceAll('_', '.');
				}
				isc.WindowStack.popup(null,
										image ? 'Upload Image' : 'Upload Content',
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
				actionURL: SKYVE.Util.CONTEXT_URL + "smartgen" + "?_mod=" + moduleName  + "&_doc=" + documentName,
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
	},
	
	onPushMessage: function(pushMessage) {
		var growls = [];
		var messages = [];
		var warn = false;

		for (var i = 0, l = pushMessage.length; i < l; i++) {
			var m = pushMessage[i];
			if (m.type == 'g') {
				growls.add({severity: m.severity, summary: m.message});
			}
			else if (m.type == 'm') {
				if (m.severity != 'info') {
					warn = true;
				}
				messages.add(m.message);
			}
			else if (m.type == 'r') {
				var view = isc.BizUtil.getCurrentView();
				if (view && view.rerender) {
					view.rerender();
				}
			}
			else if (m.type == 'j') {
				window[m.method](m.argument);
			}
		}
		
		if (growls.length > 0) {
			isc.BizUtil.growl(growls);
		}
		if (messages.length > 0) {
			var multiple = (messages.length > 1);
			var markup = multiple ? '<ul>' : '';
			for (var i = 0, l = messages.length; i < l; i++) {
				var message = messages[i];
				if (multiple) {
					markup += '<li>';
				}
				markup += message;
				if (multiple) {
					markup += '</li>';
				}
			}
			if (multiple) {
				markup += '</ul>';
			}

			if (warn) {
				isc.warn(markup);
			}
			else {
				isc.say(markup);
			}
		}
	}
});
