// Reporting parameters
isc.ClassFactory.defineClass("ReportDialog");
isc.ReportDialog.addClassProperties({
	_pageFormats: {'LETTER': {width: 612, height: 792},
					'NOTE':	{width: 540, height: 720},
					'LEGAL': {width: 612, height: 1008},
					'A0': {width: 2380, height: 3368},
					'A1': {width: 1684, height: 2380},
					'A2': {width: 1190, height: 1684},
					'A3': {width: 842, height: 1190},
					'A4': {width: 595, height: 842},
					'A5': {width: 421, height: 595},
					'A6': {width: 297, height: 421},
					'A7': {width: 210, height: 297},
					'A8': {width: 148, height: 210},
					'A9': {width: 105, height: 148},
					'A10': {width: 74, height: 105},
					'B0': {width: 2836, height: 4008},
					'B1': {width: 2004, height: 2836},
					'B2': {width: 1418, height: 2004},
					'B3': {width: 1002, height: 1418},
					'B4': {width: 709, height: 1002},
					'B5': {width: 501, height: 709},
					'ARCH_E': {width: 595, height: 842},
					'ARCH_D': {width: 595, height: 842},
					'ARCH_C': {width: 595, height: 842},
					'ARCH_B': {width: 595, height: 842},
					'ARCH_A': {width: 595, height: 842},
					'FLSA': {width: 612, height: 936},
					'FLSE': {width: 612, height: 936},
					'HALFLETTER': {width: 396, height: 612},
					'11x17': {width: 792, height: 1224},
					'LEDGER': {width: 1224, height: 792}},

	_available: [
		{name: 'title', title: 'Available Columns', required: true},
		{name: 'line', 
			title: 'Line', 
			type: 'integer', 
			editorType: 'spinner', 
			editorProperties: {
				min: 1,
				max: 9,
				step: 1,
				defaultValue: 1,
				validators: [{type: 'integerRange', min: 1, max: 9}]
			},
			width: 50},
		{name: 'width', 
			title: 'Width', 
			type: 'integer', 
			editorType: 'spinner', 
			editorProperties: {
				min: 1,
				max: 9999,
				step: 10,
				defaultValue: 1,
				validators: [{type: 'integerRange', min: 1, max: 9999}]
			},
			width: 50}
	],
	_selected: [
            {name: 'title', title: 'Exported Columns', required: true},
	  		{name: 'line', 
	  			title: 'Line', 
	  			type: 'integer', 
	  			editorType: 'spinner', 
	  			editorProperties: {
	  				min: 1,
	  				max: 9,
	  				step: 1,
	  				defaultValue: 1,
	  				validators: [{type: 'integerRange', min: 1, max: 9}]
	  			},
	  			width: 50},
	  		{name: 'width', 
	  			title: 'Width', 
	  			type: 'integer', 
	  			editorType: 'spinner', 
	  			editorProperties: {
	  				min: 1,
	  				max: 9999,
	  				step: 10,
	  				defaultValue: 1,
	  				validators: [{type: 'integerRange', min: 1, max: 9999}]
	  			},
	  			width: 50}
	],
	_valuesManager: isc.ValuesManager.create()
});
isc.ReportDialog.addClassProperties({
	_columnList: isc.ListGrid.create({
		width: "100%", 
		height: "100%",
		canDragRecordsOut: true,
		canAcceptDroppedRecords: true,
		dragDataAction: "move",
		alternateRecordStyles: true,
		autoFetchData: false,
		leaveScrollbarGap: false,
		showHeaderContextMenu: false,
		fields: isc.ReportDialog._available
	}),

	_selectedColumnList: isc.ListGrid.create({
		width: "100%",
		height: "100%",
		canDragRecordsOut: true,
		canAcceptDroppedRecords: true,
		dragDataAction: "move",
		canReorderRecords: true,
		canRemoveRecords: false,
		canEdit: true,
		editByCell: true,
		editEvent: 'click',
		modalEditing: true,
		alternateRecordStyles: true,
		autoFetchData: false,
		preventDuplicates: true,
		leaveScrollbarGap: false,
		showHeaderContextMenu: false,
		fields: isc.ReportDialog._selected
	})
});
isc.ReportDialog.addClassProperties({
	// create a jasper report format picklist
	_createJasperReportFormatPickList: function(colSpan, onChangeFunction) { // callback for onchange event
		var valueMap = {};
		var valueIcons = {};
		// An array of report format names from org.skyve.report.ReportFormat.
		for (var i = 0, l = SKYVE.Util.allowedReportFormats.length; i < l; i++) {
			var format = SKYVE.Util.allowedReportFormats[i];
			if (format == 'pdf') {
				valueMap[format] = 'PDF (Adobe Portable Document Format)';
				valueIcons[format] = format;
			}
			else if (format == 'docx') {
				valueMap[format] = 'DOCX (Word Document - Office 2007)';
				valueIcons[format] = 'rtf';
			}
			else if (format == 'xlsx') {
				valueMap[format] = 'XLSX (Excel Document - Office 2007)';
				valueIcons[format] = 'xls';
			}
			else if (format == 'pptx') {
				valueMap[format] = 'PPTX (Powerpoint Document - Office 2007)';
				valueIcons[format] = format;
			}
			else if (format == 'xls') {
				valueMap[format] = 'XLS (Excel - 98-2003)';
				valueIcons[format] = format;
			}
			else if (format == 'rtf') {
				valueMap[format] = 'RTF (Rich Text Format)';
				valueIcons[format] = format;
			}
			else if (format == 'ods') {
				valueMap[format] = 'ODS (Open Document Spreadsheet Format)';
				valueIcons[format] = 'oo';
			}
			else if (format == 'odt') {
				valueMap[format] = 'ODT (Open Document Text Format)';
				valueIcons[format] = 'oo';
			}
			else if (format == 'html') {
				valueMap[format] = 'HTML (Hyper Text Markup Language)';
				valueIcons[format] = format;
			}
			else if (format == 'csv') {
				valueMap[format] = 'CSV (Comma Separated Values)';
				valueIcons[format] = format;
			}
			else if (format == 'xml') {
				valueMap[format] = 'XML (JRXML Format)';
				valueIcons[format] = format;
			}
			else if (format == 'txt') {
				valueMap[format] = 'TXT (Text Format)';
				valueIcons[format] = format;
			}
			else {
				valueMap[format] = format;
				valueIcons[format] = format;
			}
		} 

		return {
			name: 'reportFormat', 
			showTitle: false, 
			type: 'select',
			width: 300,
			required: true,
			valueMap: valueMap,
			imageURLPrefix: 'reporting/',
			imageURLSuffix: '.png',
			valueIcons: valueIcons,
			defaultValue: SKYVE.Util.allowedReportFormats[0],
			colSpan: colSpan,
			change: onChangeFunction
		};
	},
	// create a freemarker report format picklist
	_createFreemarkerReportFormatPickList: function() {
		var valueMap = {};
		var valueIcons = {};
		var defaultValue = null;
		// An array of report format names from org.skyve.report.ReportFormat.
		for (var i = 0, l = SKYVE.Util.allowedReportFormats.length; i < l; i++) {
			var format = SKYVE.Util.allowedReportFormats[i];
			if (format == 'pdf') {
				valueMap[format] = 'PDF (Adobe Portable Document Format)';
				valueIcons[format] = format;
				if (defaultValue == null) {
					defaultValue = format;
				}
			}
			else if (format == 'csv') {
				valueMap[format] = 'CSV (Comma Separated Values)';
				valueIcons[format] = format;
				if (defaultValue == null) {
					defaultValue = format;
				}
			}
		} 

		return {
			name: 'reportFormat', 
			showTitle: false, 
			type: 'select',
			width: 300,
			required: true,
			valueMap: valueMap,
			imageURLPrefix: 'reporting/',
			imageURLSuffix: '.png',
			valueIcons: valueIcons,
			defaultValue: defaultValue,
			colSpan: 1
		};
	}
});
isc.ReportDialog.addClassProperties({
	_columnSelectorLayout: isc.HLayout.create({
		membersMargin: 10,
		height: "100%", // need height to centre the arrow
		members: [
			isc.ReportDialog._columnList,
			isc.VLayout.create({
				layoutAlign: "center",
				membersMargin: 10,
				height: 100,
				members: [
					isc.IButton.create({
						title: null,
						icon: "reporting/arrow_right.png",
						iconWidth: 24,
						iconHeight: 24,
						iconAlign: "center",
						width: 36, 
						height: 36, 
						click: function() {isc.ReportDialog._selectedColumnList.transferSelectedData(isc.ReportDialog._columnList);},
						canHover: true,
						getHoverHTML: function() {return "Move the selected columns into the report";}
					}),
					isc.IButton.create({
						title: null,
						icon: "reporting/arrow_left.png",
						iconWidth: 24,
						iconHeight:24,
						iconAlign: "center", 
						width: 36, 
						height: 36, 
						click: function() {isc.ReportDialog._columnList.transferSelectedData(isc.ReportDialog._selectedColumnList);},
						canHover: true,
						getHoverHTML: function() {return "Move the selected columns out of the report";}
					})
				]
			}),
			isc.ReportDialog._selectedColumnList
		]
	}),
	_reportFormatForm: null,
	_pageFormatForm: isc.DynamicForm.create({
		valuesManager: isc.ReportDialog._valuesManager,
		numCols: 10,
		padding: 15,
		items: [
			{name: 'format', 
				title: 'Format', 
				type: 'select',
				required: true,
				width: 80,
				valueMap: ['LETTER', 
							'NOTE', 
							'LEGAL',
							'A0',
							'A1',
							'A2',
							'A3',
							'A4',
							'A5',
							'A6',
							'A7',
							'A8',
							'A9',
							'A10',
							'B0',
							'B1',
							'B2',
							'B3',
							'B4',
							'B5',
							'ARCH_E',
							'ARCH_D',
							'ARCH_C',
							'ARCH_B',
							'ARCH_A',
							'FLSA',
							'FLSE',
							'HALFLETTER',
							'11x17',
							'LEDGER'],
				defaultValue: 'A4',
				change: function(form, item, value, oldValue) {
					if (form.getItem('orientation').getValue() == "portrait") {
						form.getItem('width').setValue(isc.ReportDialog._pageFormats[value].width);
						form.getItem('height').setValue(isc.ReportDialog._pageFormats[value].height);
					}
					else {
						form.getItem('width').setValue(isc.ReportDialog._pageFormats[value].height);
						form.getItem('height').setValue(isc.ReportDialog._pageFormats[value].width);
					}
				}
			},
			{name: 'orientation',
				title: 'Orientation',
				type: 'radioGroup',
				required: true,
				valueMap: {portrait: 'Portrait', landscape: 'Landscape'},
				defaultValue: 'portrait',
				// swap the width and height values
				change: function(form, item, vaue, oldValue) {
					var widthItem = form.getItem('width');
					var heightItem = form.getItem('height');
					var width = widthItem.getValue();
					widthItem.setValue(heightItem.getValue());
					heightItem.setValue(width);
				}
			},
			{name: 'width',
				title: 'Width',
				type: 'spinner',
				required: true,
				width: 80,
				min: 1,
				max: 9999,
				step: 1,
				defaultValue: isc.ReportDialog._pageFormats['A4'].width,
				validators: [{type: 'integerRange', min: 1, max: 9999}]
			},
			{name: 'height',
				title: 'Height',
				type: 'spinner',
				required: true,
				width: 80,
				min: 1,
				max: 9999,
				step: 1,
				defaultValue: isc.ReportDialog._pageFormats['A4'].height,
				validators: [{type: 'integerRange', min: 1, max: 9999}]
			}
		]
	}),
	_marginsForm: isc.DynamicForm.create({
		valuesManager: isc.ReportDialog._valuesManager,
		numCols: 8,
		padding: 15,
		items: [
			{name: 'top',
				title: 'Top',
				type: 'spinner',
				required: true,
				width: 80,
				min: 1,
				max: 9999,
				step: 1,
				defaultValue: 20,
				validators: [{type: 'integerRange', min: 1, max: 9999}]
			},
			{name: 'bottom',
				title: 'Bottom',
				type: 'spinner',
				required: true,
				width: 80,
				min: 1,
				max: 9999,
				step: 1,
				defaultValue: 20,
				validators: [{type: 'integerRange', min: 1, max: 9999}]
			},
			{name: 'left',
				title: 'Left',
				type: 'spinner',
				required: true,
				width: 80,
				min: 1,
				max: 9999,
				step: 1,
				defaultValue: 20,
				validators: [{type: 'integerRange', min: 1, max: 9999}]
			},
			{name: 'right',
				title: 'Right',
				type: 'spinner',
				required: true,
				width: 80,
				min: 1,
				max: 9999,
				step: 1,
				defaultValue: 20,
				validators: [{type: 'integerRange', min: 1, max: 9999}]
			}
		]
	}),
	
	_submitForm: isc.DynamicForm.create({
		autoDraw: true,
		canSubmit: true,
		method: 'POST',
		items: [{name: 'values', type: 'hidden'}, {name: '_c', type: 'hidden'}]
	}),

	// The entire interface
	_layout: null,

	// The extra criteria defined on the query or model
	_criteria: null,
	
	// The bizId of the selected tag (if any) in the listgrid
	_tagId: null,
	
	// The data source representing the server-side query or model
	_dataSourceID: null,
	
	_createReportPanel: function(buttonDefn, // the button to include in the panel
									formDefn) { // the RHS form
		return isc.HLayout.create({
			backgroundImage: 'background.png',
			backgroundRepeat: 'repeat',
			membersMargin: 5,
			margin: 5,
			layoutAlign: "center",
			members: [
				isc.VLayout.create({
					backgroundImage: 'background.png',
					backgroundRepeat: 'repeat',
					width: 134,
					height: 130,
					layoutAlign: "center",
					margin: 0,
					membersMargin: 10,
					members: [
						isc.HLayout.create({width: 134, align: 'center', members: [
							isc.Img.create({
							    imageType: "normal",
							    src: "reporting/reporting.png",
							    height: 96,
							    layoutAlign: "center"
							})
						]}),
						isc.HLayout.create({width: 134, align: 'center', members: [buttonDefn]})
					]
				}),
				formDefn
			]
		});
	},
	
	// construct the interface
	_createExport: function() {
		if (isc.ReportDialog._exportLayout == null) {
			var changeHandler = function(form, item, value, oldValue) {
				if ((value == 'pdf') ||
						(value == 'docx') ||
						(value == 'pptx') ||
						(value == 'rtf') ||
						(value == 'odt')) {
					form.getItem('isPaginated').setValue(true);
					form.getItem('isPretty').setValue(true);
                    form.getItem('showSummary').setValue(true);
				}
				else if (value == 'html') {
					form.getItem('isPaginated').setValue(false);
					form.getItem('isPretty').setValue(true);
                    form.getItem('showSummary').setValue(true);
				}
				else {
					form.getItem('isPaginated').setValue(false);
					form.getItem('isPretty').setValue(false);
					form.getItem('showSummary').setValue(false);
				}
				form.getItem('fileNameSuffix').setValue('.' + value);
			};

			isc.ReportDialog._reportFormatForm = isc.DynamicForm.create({
				valuesManager: isc.ReportDialog._valuesManager,
				numCols: 7,
				colWidths: [60, 200, 40, 30, '*', 60, '*'],
				padding: 15,
				items: [
			        isc.ReportDialog._createJasperReportFormatPickList(3, changeHandler),
					{name: "isPaginated",
						title: "Paginated Report",
						type: "checkbox",
						required: true
					},
					{name: 'style',
						title: 'Style',
						type: 'radioGroup',
						vertical: true,
						required: true,
						rowSpan: 2,
						valueMap: {tabular: 'Tabular', columnar: 'Columnar'},
						defaultValue: 'tabular',
						change: function(form, item, value, oldValue) {
							if (value == "columnar") {
								// hide table columns
								isc.ReportDialog._columnList.hideField("line");
								isc.ReportDialog._columnList.hideField("width");
								isc.ReportDialog._selectedColumnList.hideField("line");
								isc.ReportDialog._selectedColumnList.hideField("width");
							}
							else {
								// show table columns
								isc.ReportDialog._columnList.showField("line");
								isc.ReportDialog._columnList.showField("width");
								isc.ReportDialog._selectedColumnList.showField("line");
								isc.ReportDialog._selectedColumnList.showField("width");
							}
						}
					},
					{name: "fileNameNoSuffix",
						title: "Filename",
						type: "text",
						required: true,
						width: '100%',
						defaultValue: "export"
					},
					{name: "fileNameSuffix",
						showTitle: false,
						type: "staticText",
						startRow: false,
						endRow: false
					},
					{name: "isPretty",
						title: "Pixel Perfect",
						type: "checkbox",
						required: true
					},
		            {type: "spacer"},
		            {type: "spacer"},
		            {type: "spacer"},
		            {name: "showSummary",
		                title: "Show Summary",
		                type: "checkbox",
		                required: true
		            }
				]
			}),
			
			changeHandler(isc.ReportDialog._reportFormatForm, null, SKYVE.Util.allowedReportFormats[0], null);
			
			isc.ReportDialog._exportLayout = isc.VLayout.create({
				backgroundImage: 'background.png',
				backgroundRepeat: 'repeat',
				height: "100%",
				width: "100%",
				autoDraw: true,
				margin: 5,
				membersMargin: 5,
				members: [
			        isc.ReportDialog._createReportPanel(
						isc.IButton.create({
							title: 'Generate',
							click: function() {
								if (isc.ReportDialog._valuesManager.validate()) {
									var values = isc.ReportDialog._valuesManager.getValues();
									values.columns = isc.ReportDialog._selectedColumnList.getData();
									values.ds = isc.ReportDialog._dataSourceID;
									if (isc.ReportDialog._criteria) {
										values.criteria = isc.ReportDialog._criteria;
									}
									if (isc.ReportDialog._tagId) {
										values.tagId = isc.ReportDialog._tagId;
									}

									// Use a standard form POST, HTML targeted to a blank window
									var format = isc.ReportDialog._reportFormatForm.getItem("reportFormat").getValue();
									var fileNameNoSuffix = isc.ReportDialog._reportFormatForm.getItem("fileNameNoSuffix").getValue();
									isc.ReportDialog._submitForm.setValue('values', isc.JSON.encode(values, {prettyPrint:false}));
									if (isc.ReportDialog._c) {
										isc.ReportDialog._submitForm.setValue('_c', isc.ReportDialog._c);
									}
									isc.ReportDialog._submitForm.setAction('export/' + fileNameNoSuffix + '.' + format);
									isc.ReportDialog._submitForm.setTarget((format === 'html') ? '_blank' : '_self');
									isc.ReportDialog._submitForm.submitForm();
								}
							}
						}),
						isc.VLayout.create({
							backgroundImage: 'background.png',
							backgroundRepeat: 'repeat',
							margin: 0,
							membersMargin: 5,
							members: [isc.VLayout.create({isGroup: true,
															groupTitle: 'Report Format',
															styleName: 'bizhubRoundedBorder',
															groupBorderCSS: '1px solid #bfbfbf',
															margin: 1,
															groupLabelBackgroundColor: 'transparent',
															groupLabelStyleName:'bizhubBorderLabel',
															backgroundImage: 'background.png',
															backgroundRepeat: 'repeat',
															members: [isc.ReportDialog._reportFormatForm]}), 
										isc.VLayout.create({isGroup: true,
																groupTitle: 'Page Format',
																styleName: 'bizhubRoundedBorder',
																groupBorderCSS: '1px solid #bfbfbf',
																margin: 1,
																groupLabelBackgroundColor: 'transparent',
																groupLabelStyleName:'bizhubBorderLabel',
																backgroundImage: 'background.png',
																backgroundRepeat: 'repeat',
																members: [isc.ReportDialog._pageFormatForm]}),
										isc.VLayout.create({isGroup: true,
																groupTitle: 'Margins',
																styleName: 'bizhubRoundedBorder',
																groupBorderCSS: '1px solid #bfbfbf',
																margin: 1,
																groupLabelBackgroundColor: 'transparent',
																groupLabelStyleName:'bizhubBorderLabel',
																backgroundImage: 'background.png',
																backgroundRepeat: 'repeat',
																members: [isc.ReportDialog._marginsForm]})]
						})
					),
					isc.ReportDialog._columnSelectorLayout
				]
			});
		}
	},
	
	popupExport: function(dataSourceID, // the ID of the data source - and thus the server-side query or model
							_c, // the web context identifier
							criteria, // the criteria to apply to the server-side query or model
							tagId, // the tagId of the selected tag from the listgrid to apply server-side
							unselectedFields, // data for the unselected fields in field selection list grids
							selectedFields) { // data for the selected fields in field selection list grids
		isc.ReportDialog._createExport();
		
		isc.ReportDialog._dataSourceID = dataSourceID;
		isc.ReportDialog._c = _c;
		isc.ReportDialog._criteria = criteria;
		isc.ReportDialog._tagId = tagId;
		
		isc.ReportDialog._columnList.setData(unselectedFields);
		isc.ReportDialog._selectedColumnList.setData(selectedFields);
		
		isc.WindowStack.popup(null, "Reporting", true, [isc.ReportDialog._exportLayout]);
	},
	
	popupReport: function(view, // view
							params) { // param map (name -> binding expression with {} if required
		if (params && params._f) {
			var format = params._f;
			var paramsCopy = isc.addProperties({}, params);
			delete paramsCopy._f;
			isc.ReportDialog._redirectToReport(view, paramsCopy, format);
		}
		else if (params && params._e && params._e == 'Freemarker') {
			if (isc.ReportDialog._freemarkerReportLayout == null) {
				isc.ReportDialog._freemarkerReportLayout = isc.ReportDialog._createReportPanel(
					isc.IButton.create({
						title: 'View',
						ID: '_freemarkerReportViewButton',
						_view: null,
						_params: null,
						click: function() {
							var format = _freemarkerReportForm.getValue("reportFormat");
							isc.ReportDialog._redirectToReport(this._view, this._params, format);
						}
					}),
					isc.DynamicForm.create({
						ID: '_freemarkerReportForm',
						fields: [
					        {type:'rowSpacer'}, 
					        {type:'rowSpacer'},
						    isc.ReportDialog._createFreemarkerReportFormatPickList()
			         	]
					})
				);
			}
			
			_freemarkerReportViewButton._view = view;
			_freemarkerReportViewButton._params = params;
			isc.WindowStack.popup(null, "Report", true, [isc.ReportDialog._freemarkerReportLayout], 180, 480);
		}
		else {
			if (isc.ReportDialog._jasperReportLayout == null) {
				isc.ReportDialog._jasperReportLayout = isc.ReportDialog._createReportPanel(
					isc.IButton.create({
						title: 'View',
						ID: '_jasperReportViewButton',
						_view: null,
						_params: null,
						click: function() {
							var format = _jasperReportForm.getValue("reportFormat");
							isc.ReportDialog._redirectToReport(this._view, this._params, format);
						}
					}),
					isc.DynamicForm.create({
						ID: '_jasperReportForm',
						fields: [
					        {type:'rowSpacer'}, 
					        {type:'rowSpacer'},
						    isc.ReportDialog._createJasperReportFormatPickList(
						    	1,
					    		function(form, item, value, oldValue) {
					    			// do nothing
					    		}),
			         	]
					})
				);
			}
			
			_jasperReportViewButton._view = view;
			_jasperReportViewButton._params = params;
			isc.WindowStack.popup(null, "Report", true, [isc.ReportDialog._jasperReportLayout], 180, 480);
		}
	},
	
	_redirectToReport: function(view, params, format) {
		var instance = view.gather(false); // don't validate
		if (instance) {
			var c = instance._c;
			var b = view._b;
			var bizId = instance.bizId;

			var src;
            if (params && params['_n']) {
                src = 'report/' + params['_n'] + '.' + format +
                    '?_f=' + format +
                    (c ? '&_c=' + c : '') +
                    (b ? '&_b=' + b : '') +
                    (bizId ? '&_id=' + bizId : '');
			} else {
                src = 'report/' +
                    '?_f=' + format +
                    (c ? '&_c=' + c : '') +
                    (b ? '&_b=' + b : '') +
                    (bizId ? '&_id=' + bizId : '');
			}
			if (params) {
				for (var name in params) {
					var binding = params[name];
					// encodeURI and then encode '?' and '&' chars which are not encoded by encodeURI().
					src += '&' + name + '=' + encodeURI(view.toDisplay(binding, instance)).replaceAll('?', '%3F').replaceAll('&', '%26');
				}
			}
			if (format === 'html') {
				window.open(src, 'report', 'location=0,status=0,scrollbars=1,resizable=1,width=800,height=600');
			}
			else {
				window.location = src;
			}
		}
	}
});
