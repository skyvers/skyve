// Reporting parameters
ClassFactory.defineClass("ReportDialog");
ReportDialog.addClassProperties({
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
		{name: 'title', title: 'Column', required: true},
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
            {name: 'title', title: 'Column', required: true},
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
ReportDialog.addClassProperties({
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
		fields: ReportDialog._available
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
		fields: ReportDialog._selected
	})
});
ReportDialog.addClassProperties({
	// create a report format picklist
	_createReportFormatPickList: function(colSpan, onChangeFunction) { // callback for onchange event
		return {
			name: 'reportFormat', 
			showTitle: false, 
			type: 'select',
			width: 300,
			required: true,
			valueMap: {
				'pdf': 'PDF (Adobe Portable Document Format)',
				'docx': 'DOCX (Word Document - Office 2007)',
				'xlsx': 'XLSX (Excel Document - Office 2007)',
				'pptx': 'PPTX (Powerpoint Document - Office 2007)',
				'xls': 'XLS (Excel - 98-2003)',
				'rtf': 'RTF (Rich Text Format)',
				'ods': 'ODS (Open Document Spreadsheet Format)',
				'odt': 'ODT (Open Document Text Format)',
				'html': 'HTML (Hyper Text Markup Language)',
				'xhtml': 'XHTML (Conformant Hyper Text Markup Language)',
				'csv': 'CSV (Comma Separated Values)',
				'xml': 'XML (JRXML Format)'
			},
			imageURLPrefix: 'reporting/',
			imageURLSuffix: '.png',
			valueIcons: {
				'pdf': 'pdf',
				'docx': 'rtf',
				'xlsx': 'xls',
				'pptx': 'pptx',
				'xls': 'xls',
				'rtf': 'rtf',
				'odt': 'oo',
				'ods': 'oo',
				'html': 'html',
				'xhtml': 'html',
				'txt': 'txt',
				'csv': 'csv',
				'xml': 'xml'
			},
			defaultValue: 'pdf',
			colSpan: colSpan,
			change: onChangeFunction
		};
	}
});
ReportDialog.addClassProperties({
	_columnSelectorLayout: isc.HLayout.create({
		membersMargin: 10,
		height: "100%", // need height to centre the arrow
		members: [
			ReportDialog._columnList,
			isc.VLayout.create({
				layoutAlign: "center",
				membersMargin: 10,
				height: 100,
				members: [
					isc.IButton.create({
						icon: "reporting/arrow_right.png",
						iconWidth: 24,
						iconHeight: 24,
						iconAlign: "center",
						showText: false, 
						width: 32, 
						height: 32, 
						click: function() {ReportDialog._selectedColumnList.transferSelectedData(ReportDialog._columnList);},
						canHover: true,
						getHoverHTML: function() {return "Move the selected columns into the report";}
					}),
					isc.IButton.create({
						icon: "reporting/arrow_left.png",
						iconWidth: 24,
						iconHeight:24,
						iconAlign: "center", 
						showText: false,
						width: 32, 
						height: 32, 
						click: function() {ReportDialog._columnList.transferSelectedData(ReportDialog._selectedColumnList);},
						canHover: true,
						getHoverHTML: function() {return "Move the selected columns out of the report";}
					})
				]
			}),
			ReportDialog._selectedColumnList
		]
	}),

	_reportFormatForm: isc.DynamicForm.create({
		valuesManager: ReportDialog._valuesManager,
		numCols: 7,
		colWidths: [60, 200, 40, 30, '*', 60, '*'],
		padding: 15,
		items: [
	        ReportDialog._createReportFormatPickList(
	        	3,
	        	function(form, item, value, oldValue) {
					if ((value == 'pdf') ||
							(value == 'docx') ||
							(value == 'pptx') ||
							(value == 'rtf') ||
							(value == 'odt')) {
						form.getItem('isPaginated').setValue(true);
						form.getItem('isPretty').setValue(true);
					}
					else if ((value == 'html') || (value == 'xhtml')) {
						form.getItem('isPaginated').setValue(false);
						form.getItem('isPretty').setValue(true);
					}
					else {
						form.getItem('isPaginated').setValue(false);
						form.getItem('isPretty').setValue(false);
					}
					form.getItem('fileNameSuffix').setValue('.' + value);
				}
	        ),
			{name: "isPaginated",
				title: "Paginated Report",
				type: "checkbox",
				required: true,
				defaultValue: true
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
						ReportDialog._columnList.hideField("line");
						ReportDialog._columnList.hideField("width");
						ReportDialog._selectedColumnList.hideField("line");
						ReportDialog._selectedColumnList.hideField("width");
					}
					else {
						// show table columns
						ReportDialog._columnList.showField("line");
						ReportDialog._columnList.showField("width");
						ReportDialog._selectedColumnList.showField("line");
						ReportDialog._selectedColumnList.showField("width");
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
				endRow: false,
				defaultValue: ".pdf"
			},
			{name: "isPretty",
				title: "Pixel Perfect",
				type: "checkbox",
				required: true,
				defaultValue: true
			}
		]
	}),
	
	_pageFormatForm: isc.DynamicForm.create({
		valuesManager: ReportDialog._valuesManager,
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
						form.getItem('width').setValue(ReportDialog._pageFormats[value].width);
						form.getItem('height').setValue(ReportDialog._pageFormats[value].height);
					}
					else {
						form.getItem('width').setValue(ReportDialog._pageFormats[value].height);
						form.getItem('height').setValue(ReportDialog._pageFormats[value].width);
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
				defaultValue: ReportDialog._pageFormats['A4'].width,
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
				defaultValue: ReportDialog._pageFormats['A4'].height,
				validators: [{type: 'integerRange', min: 1, max: 9999}]
			}
		]
	}),
	_marginsForm: isc.DynamicForm.create({
		valuesManager: ReportDialog._valuesManager,
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
		items: [{name: 'values', type: 'hidden'}]
	}),

	// The entire interface
	_layout: null,

	// The extra criteria defined on the query
	_criteria: null,
	
	// The data source representing the server-side query
	_dataSourceID: null,
	
	_createReportPanel: function(buttonDefn, // the button to include in the panel
									formDefn) { // the RHS form
		return isc.HLayout.create({
			membersMargin: 5,
			margin: 5,
			layoutAlign: "center",
			members: [
				isc.VLayout.create({
					width: 120,
					height: 120,
					layoutAlign: "center",
					margin: 0,
					membersMargin: 10,
					members: [
						isc.Img.create({
						    imageType: "normal",
						    src: "reporting/reporting.jpeg",
						    layoutAlign: "center"
						}),
						buttonDefn
					]
				}),
				formDefn
			]
		});
	},
	
	// construct the interface
	_createExport: function() {
		if (ReportDialog._exportLayout == null) {
			ReportDialog._exportLayout = isc.VLayout.create({
				height: "100%",
				width: "100%",
				autoDraw: true,
				margin: 5,
				membersMargin: 5,
				members: [
			        ReportDialog._createReportPanel(
						isc.IButton.create({
							title: 'Generate',
							click: function() {
								if (ReportDialog._valuesManager.validate()) {
									var values = ReportDialog._valuesManager.getValues();
									values.columns = ReportDialog._selectedColumnList.getData();
									values.query = ReportDialog._dataSourceID;
									if (ReportDialog._criteria) {
										values.criteria = ReportDialog._criteria;
									}

									// have to request to this setup URL as document.write() doesn't work in IE6
//									var src = 'setup.rpt' +
//												'?format=' + ReportDialog._reportFormatForm.getItem("reportFormat").getValue() +
//												'&values=' + escape(isc.JSON.encode(values, {prettyPrint:false}));
//									var reportWindow = window.open(src, "report", "location=0,status=0,scrollbars=1,resizable=1,width=800,height=600");

//									var format = ReportDialog._reportFormatForm.getItem("reportFormat").getValue();
//									var src = 'export.' + ReportDialog._reportFormatForm.getItem("reportFormat").getValue() + '?values=' + isc.JSON.encode(values, {prettyPrint:false});
//									if ((format === 'html') || (format === 'xhtml')) {
//										window.open(encodeURI(src), 'report', 'location=0,status=0,scrollbars=1,resizable=1,width=800,height=600');
//									}
//									else {
//										window.location = encodeURI(src);
//									}
//									if ((format === 'html') || (format === 'xhtml')) {
//										var reportWindow = window.open('', 'report', 'location=0,status=0,scrollbars=1,resizable=1,width=800,height=600');
//									    reportWindow.document.write('<form name="auto" method="post" action="export.' + format + 
//			    								'"><input name="values" type="hidden" value="' + isc.JSON.encode(values, {prettyPrint:false}).replaceAll('"', '&quot;')
//			    								 + 
//			    								'"/></form>\x3Cscript type="text/javascript">\document.forms[0].submit();\x3C/script>');
//									}
									// Use a standard form POST, HTML/XHTML targetted to a blank window
									var format = ReportDialog._reportFormatForm.getItem("reportFormat").getValue();
									var fileNameNoSuffix = ReportDialog._reportFormatForm.getItem("fileNameNoSuffix").getValue();
									ReportDialog._submitForm.setValue('values', isc.JSON.encode(values, {prettyPrint:false}));
									ReportDialog._submitForm.setAction('export/' + fileNameNoSuffix + '.' + format);
									ReportDialog._submitForm.setTarget(((format === 'html') || (format === 'xhtml')) ?
																		'_blank' :
																		'_self');
									ReportDialog._submitForm.submitForm();
								}
							}
						}),
						isc.VLayout.create({
							margin: 0,
							membersMargin: 5,
							members: [isc.VLayout.create({isGroup: true,
															groupTitle: 'Report Format',
															members: [ReportDialog._reportFormatForm]}), 
										isc.VLayout.create({isGroup: true,
																groupTitle: 'Page Format',
																members: [ReportDialog._pageFormatForm]}),
										isc.VLayout.create({isGroup: true,
																	groupTitle: 'Margins',
																	members: [ReportDialog._marginsForm]})]
						})
					),
					ReportDialog._columnSelectorLayout
				]
			});
		}
	},
	
	popupExport: function(dataSourceID, // the ID of the data source - and thus the server-side query
					criteria, // the criteria to apply to the server-side query
					unselectedFields, // data for the unselected fields in field selection list grids
					selectedFields) { // data for the selected fields in field selection list grids
		ReportDialog._createExport();
		
		ReportDialog._dataSourceID = dataSourceID;
		ReportDialog._criteria = criteria;
		ReportDialog._columnList.setData(unselectedFields);
		ReportDialog._selectedColumnList.setData(selectedFields);
		
		WindowStack.popup(null, "Reporting", true, [ReportDialog._exportLayout]);
	},
	
	popupReport: function(view, // view
							params) { // param names
		if (ReportDialog._reportLayout == null) {
			ReportDialog._reportLayout = ReportDialog._createReportPanel(
				isc.IButton.create({
					title: 'View',
					ID: '_reportViewButton',
					_view: null,
					_params: null,
					click: function() {
						var instance = this._view.gather(false); // don't validate
						if (instance) {
							var c = instance._c;
							var b = this._view._b;
							var bizId = instance.bizId;

							// have to post to this setup URL as document.write() doesn't work in IE6
							var format = _reportForm.getValue("reportFormat");
							var src = '/report/' + this._params['_n'] + '.' + format +
										'?_format=' + format +
										(c ? '&_c=' + c : '') +
										(b ? '&_b=' + b : '') +
										(bizId ? '&_id=' + bizId : '');
							if (this._params) {
								for (var name in this._params) {
									var binding = this._params[name];
									src += '&' + name + '=' + this._view.toDisplay(binding, instance);
								}
							}
							if ((format === 'html') || (format === 'xhtml')) {
								window.open(encodeURI(src), 'report', 'location=0,status=0,scrollbars=1,resizable=1,width=800,height=600');
							}
							else {
								window.location = encodeURI(src);
							}
						}
					}
				}),
				isc.DynamicForm.create({
					ID: '_reportForm',
					fields: [
				        {type:'rowSpacer'}, 
				        {type:'rowSpacer'},
					    ReportDialog._createReportFormatPickList(
					    	1,
				    		function(form, item, value, oldValue) {
				    			// do nothing
				    		}),
		         	]
				})
			);
		}
		
		_reportViewButton._view = view;
		_reportViewButton._params = params;
		WindowStack.popup(null, "Report", true, [ReportDialog._reportLayout], 175, 450);
	}
});