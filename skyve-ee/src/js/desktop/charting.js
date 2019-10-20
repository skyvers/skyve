// Charting parameters
isc.ClassFactory.defineClass("ChartDialog");
isc.ChartDialog.addClassProperties({
	_valuesManager: isc.ValuesManager.create(),
});
isc.ChartDialog.addClassProperties({
	_chartForm: isc.DynamicForm.create({
		valuesManager: isc.ChartDialog._valuesManager,
		numCols: 6,
		colWidths: [100, 120, 80, '*', 90, '*'],
		items: [
			{name: 'type',
				type: "enum", 
				title: 'Chart Type',
				width: '*',
				valueMap: {line: 'Line',
							lineArea: 'Line Area',
							bar: 'Bar',
							horizontalBar: 'Horizontal Bar',
							radar: 'Radar',
							pie: 'Pie',
							doughnut: 'Doughnut',
							polarArea: 'Polar Area'},
				required: true},
			{name: 'title',
				type: 'text',
				width: '*',
				title: 'Chart Title'},
			{name: 'label',
				type: 'text',
				width: '*',
				title: 'Data Label'}
		]
	}),
	_categoryForm: isc.DynamicForm.create({
		valuesManager: isc.ChartDialog._valuesManager,
		numCols: 7,
		colWidths: [100, '*', 110, 160, 60, 60, 150],
		items: [
			{name: 'categoryBinding',
				type: 'select',
				title: 'Category Field',
				width: '*',
				required: true,
				valueMap: []},
			{name: 'categoryBucket',
				type: "enum", 
				title: 'Category Bucket',
				width: '*',
				allowEmptyValue: true,
				valueMap: {NumericMultipleBucket: 'Numeric Multiple',
							TextLengthBucket: 'Text Length',
							TextStartsWithBucket: 'Text Starts With',
							TemporalBucket: 'Temporal'},
				redrawOnChange: true},
			{name: 'numericMultiple',
				type: 'spinner',
				width: '*',
				title: 'Multiple',
				min: 1,
				step: 1,
				defaultValue: 100,
				validators: [{type: 'requiredIf', expression: 'item.isVisible()'}],
				showIf: "form.getItem('categoryBucket').getValue() == 'NumericMultipleBucket'"},
			{name: 'startsWithLength',
				type: 'spinner',
				width: '*',
				title: 'Length',
				min: 1,
				step: 1,
				defaultValue: 1,
				validators: [{type: 'requiredIf', expression: 'item.isVisible()'}],
				showIf: "form.getItem('categoryBucket').getValue() == 'TextStartsWithBucket'"},
			{name: 'startsWithCaseSensitive',
				type: 'checkbox',
				allowEmptyValue: false,
				title: 'Case Sensitive',
				showTitle: false,
				defaultValue: false,
				validators: [{type: 'requiredIf', expression: 'item.isVisible()'}],
				showIf: "form.getItem('categoryBucket').getValue() == 'TextStartsWithBucket'"},
			{name: 'temporalBucketType',
				type: "enum", 
				width: '*',
				title: 'Type',
				colSpan: 2,
				allowEmptyValue: false,
				valueMap: {
					day: 'Day',
					dayMonthYear: 'Day/Month/Year',
					hour: 'Hour',
					hourDay: 'Hour/Day',
					hourDayMonth: 'Hour/Day/Month',
					minuteHour: 'Minute/Hour',
					month: 'Month',
					monthYear: 'Month/Year',
					secondMinuteHour: 'Second/Minute/Hour',
					year: 'Year'
				},
				validators: [{type: 'requiredIf', expression: 'item.isVisible()'}],
				showIf: "form.getItem('categoryBucket').getValue() == 'TemporalBucket'"}
		]
	}),
	_valueForm: isc.DynamicForm.create({
		valuesManager: isc.ChartDialog._valuesManager,
		numCols: 4,
		colWidths: [100, '*', 105, 100],
		items: [
			{name: 'valueBinding',
				type: 'select',
				title: 'Value Field',
				startRow: true,
				width: '*',
				required: true,
				valueMap: []},
			{name: 'valueFunction',
				type: "enum", 
				width: '*',
				title: 'Value Function',
				valueMap: [null, "Count", "Avg", "Sum", "Min", "Max"]}
		]
	}),
	_topForm: isc.DynamicForm.create({
		valuesManager: isc.ChartDialog._valuesManager,
		numCols: 8,
		colWidths: [30, 40, 60, 40, 130, 40, 100, 130],
		items: [
			{name: 'topOn',
				type: 'checkbox',
				allowEmptyValue: false,
				showTitle: false,
				showLabel: false,
				defaultValue: false,
				redrawOnChange: true,
				change: function(form, item, value, oldValue) {
					if (value) {
						form.getItem('top').enable();
						form.getItem('topBy').enable();
						form.getItem('topSort').enable();
						form.getItem('includeOthers').enable();
					}
					else {
						var topItem = form.getItem('top');
						if (! topItem.getValue()) {
							topItem.setValue('10');
							topItem.clearErrors();
						}
						topItem.disable();
						form.getItem('topBy').disable();
						form.getItem('topSort').disable();
						form.getItem('includeOthers').disable();
					}
				}
			},
			{name: 'top',
				type: 'spinner',
				title: 'Top',
				width: '*',
				required: true,
				disabled: true,
				min: 1,
				max: 9999,
				step: 1,
				defaultValue: 10},
			{name: 'topBy',
				type: 'radioGroup',
				title: 'By',
				vertical: false,
				required: true,
				disabled: true,
				defaultValue: 'category',
				valueMap: {category: 'Category', value: 'Value'}},
			{name: 'topSort',
				type: 'radioGroup',
				title: 'Sort',
				vertical: false,
				required: true,
				disabled: true,
				defaultValue: 'ascending',
				valueMap: {ascending: 'Ascending', descending: 'Descending'}},
			{name: 'includeOthers',
				type: 'checkbox',
				allowEmptyValue: false,
				title: 'Include Others',
				showTitle: false,
				disabled: true,
				defaultValue: false}
		]
	}),
	_orderForm: isc.DynamicForm.create({
		valuesManager: isc.ChartDialog._valuesManager,
		numCols: 5,
		colWidths: [30, 40, 130, 40, '*'],
		items: [
			{name: 'orderOn',
				type: 'checkbox',
				allowEmptyValue: false,
				showTitle: false,
				showLabel: false,
				defaultValue: false,
				change: function(form, item, value, oldValue) {
					if (value) {
						form.getItem('orderBy').enable();
						form.getItem('orderSort').enable();
					}
					else {
						form.getItem('orderBy').disable();
						form.getItem('orderSort').disable();
					}
				}
			},
			{name: 'orderBy',
				type: 'radioGroup',
				title: 'By',
				vertical: false,
				required: true,
				disabled: true,
				defaultValue: 'category',
				valueMap: {category: 'Category', value: 'Value'}},
			{name: 'orderSort',
				type: 'radioGroup',
				title: 'Sort',
				vertical: false,
				disabled: true,
				defaultValue: 'ascending',
				valueMap: {ascending: 'Ascending', descending: 'Descending'}}
		]
	}),
	
	_chart: null,

	// The entire interface
	_layout: null,

	// The extra criteria defined on the query or model
	_criteria: null,
	
	// The bizId of the selected tag (if any) in the listgrid
	_tagId: null,
	
	// The data source representing the server-side query or model
	_dataSource: null,

	_border: function(title, // border title
						members) { // the layout members array
		return isc.VLayout.create({
			isGroup: true,
			groupTitle: title,
			styleName: 'bizhubRoundedBorder',
			groupBorderCSS: '1px solid #bfbfbf',
			padding: 5,
			height: 1,
			groupLabelBackgroundColor: 'transparent',
			groupLabelStyleName:'bizhubBorderLabel',
			backgroundImage: 'background.png',
			backgroundRepeat: 'repeat',
			members: members});
	},

	_createChartPanel: function(buttonDefn, // the button to include in the panel
									formDefn) { // the RHS form
		return isc.HLayout.create({
			backgroundImage: 'background.png',
			backgroundRepeat: 'repeat',
			membersMargin: 5,
			margin: 5,
			height: 1,
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
	_create: function() {
		if (isc.ChartDialog._layout == null) {
			isc.ChartDialog._chart = isc.BizChart.create({});
			var result = isc.ChartDialog._border('Result', [isc.ChartDialog._chart]);
			result.setHeight('100%');
			var chart = isc.ChartDialog._border('Chart', [isc.ChartDialog._chartForm, 
															isc.ChartDialog._categoryForm,
															isc.ChartDialog._valueForm]);
			
			isc.ChartDialog._layout = isc.VLayout.create({
				backgroundImage: 'background.png',
				backgroundRepeat: 'repeat',
				height: "100%",
				width: "100%",
				autoDraw: true,
				margin: 5,
				membersMargin: 5,
				members: [
			        isc.ChartDialog._createChartPanel(
						isc.IButton.create({
							title: 'Generate',
							click: function() {
								if (isc.ChartDialog._valuesManager.validate()) {
									var params = {};
									params.ds = isc.ChartDialog._dataSource.ID;
									if (isc.ChartDialog._criteria) {
										params.criteria = isc.ChartDialog._criteria;
									}
									if (isc.ChartDialog._tagId) {
										params.tagId = isc.ChartDialog._tagId;
									}
									params.t = isc.ChartDialog._valuesManager.getValue('type');
									
									params.b = isc.ChartDialog._valuesManager.getValues();

									// do some type validation on the bucket
									var categoryBinding = params.b.categoryBinding;
									var categoryBucket = params.b.categoryBucket;
									if (categoryBucket) {
										var field = isc.ChartDialog._dataSource.getField(categoryBinding);
										var type = field.type;
										if (isc.BizUtil.isNumeric(type)) {
											if (categoryBucket != 'NumericMultipleBucket') {
												isc.ChartDialog._valuesManager.setErrors({
													categoryBinding: 'Use a numeric bucket only with a number field',
													categoryBucket: 'Use a numeric bucket only with a number field'
												}, true);
												return true;
											}
										}
										else if (isc.BizUtil.isTemporal(type)) {
											if (categoryBucket != 'TemporalBucket') {
												isc.ChartDialog._valuesManager.setErrors({
													categoryBinding: 'Use a temporal bucket only with a date or time field',
													categoryBucket: 'Use a temporal bucket only with a date or time field'
												}, true);
												return true;
											}
										}
										else {
											if (categoryBucket == 'TemporalBucket') {
												isc.ChartDialog._valuesManager.setErrors({
													categoryBinding: 'Use a temporal bucket only with a date or time field',
													categoryBucket: 'Use a temporal bucket only with a date or time field'
												}, true);
												return true;
											}
											else if (categoryBucket == 'NumericMultipleBucket') {
												isc.ChartDialog._valuesManager.setErrors({
													categoryBinding: 'Use a numeric bucket only with a number field',
													categoryBucket: 'Use a numeric bucket only with a number field'
												}, true);
												return true;
											}
										}
									}
									
									// do some type validation on the aggregate function
									var valueBinding = params.b.valueBinding;
									var valueFunction = params.b.valueFunction;
									var field = isc.ChartDialog._dataSource.getField(valueBinding);
									var type = field.type;
									if ((type != 'integer') && (type != 'float') && (valueFunction != 'Count')) {
										isc.ChartDialog._valuesManager.setErrors({
											valueBinding: 'Use the [Count] value function with a non-numeric field',
											valueFunction: 'Use the [Count] value function with a non-numeric field'
										}, true);
										return true;
									}
									
									// Create a data label if there isnt one defined
									if (! params.b.label) {
										params.b.label = (valueFunction ? 
																isc.ChartDialog._valueForm.getItem('valueFunction').getDisplayValue() + ' ' :
																'') +
															isc.ChartDialog._valueForm.getItem('valueBinding').getDisplayValue() + 
															' by ' + 
															isc.ChartDialog._categoryForm.getItem('categoryBinding').getDisplayValue();
									}

									isc.ChartDialog._valuesManager.clearErrors(true);
									
									isc.RPCManager.sendRequest({
										showPrompt: true,
										evalResult: true,
										actionURL: SKYVE.Util.CONTEXT_URL + 'chart',
										httpMethod: 'POST',
										params: params,
										callback: function(rpcResponse, data, rpcRequest) {
											if (data.config) {
												var oldChartType = null;
												if (isc.ChartDialog._chart && isc.ChartDialog._chart.chartConfig) { 
													oldChartType = isc.ChartDialog._chart.chartConfig.type;
												}
												var newChartType = data.config.type;

												if (oldChartType != newChartType) {
													isc.ChartDialog._destroyAndRecreate();
												}
												isc.ChartDialog._chart._update(data);
											}
										}
									});
								}
							}
						}),
						isc.VLayout.create({
							backgroundImage: 'background.png',
							backgroundRepeat: 'repeat',
							margin: 0,
							height: 1,
							membersMargin: 5,
							members: [chart,
										isc.ChartDialog._border('Top', [isc.ChartDialog._topForm]),
										isc.ChartDialog._border('Order', [isc.ChartDialog._orderForm])]
						})
					),
					result
				]
			});
		}
		else {
			isc.ChartDialog._destroyAndRecreate();
		}
	},
	
	_destroyAndRecreate: function() {
		var border = null;
		if (isc.ChartDialog._chart) {
			border = isc.ChartDialog._layout.getMember(1);
			border.removeMember(isc.ChartDialog._chart);
			if (isc.ChartDialog._chart.chart) {
				isc.ChartDialog._chart.chart.destroy();
			}
			isc.ChartDialog._chart.destroy();
		}
		isc.ChartDialog._chart = isc.BizChart.create({});
		if (border) {
			border.addMember(isc.ChartDialog._chart);
		}
		else {
			isc.ChartDialog._layout.addMember(isc.ChartDialog._border('Result', [isc.ChartDialog._chart]));
		}
	},
	
	popupChart: function(dataSource, // the data source - and the ID is the server-side query or model
							_c, // the web context identifier
							criteria, // the criteria to apply to the server-side query or model
							tagId, // the tagId of the selected tag from the listgrid to apply server-side
							fields) { // data for the fields in field selection
		isc.ChartDialog._create();
		
		isc.ChartDialog._dataSource = dataSource;
		isc.ChartDialog._c = _c;
		isc.ChartDialog._criteria = criteria;
		isc.ChartDialog._tagId = tagId;
		
		// Make the field valueMap
		var valueMap = {};
		var fieldNames = dataSource.getFieldNames(true); // no hidden fields
		for (var i = 0, l = fieldNames.length; i < l; i++) {
			if ((fieldNames[i] != 'bizTagged') && (fieldNames[i] != 'bizFlagComment')) {
				var field = dataSource.getField(fieldNames[i]);
				valueMap[field.name] = field.title;
			}
		}

		var field = isc.ChartDialog._categoryForm.getItem('categoryBinding');
		field.setValue('');
		field.setValueMap(valueMap);
		field = isc.ChartDialog._valueForm.getItem('valueBinding');
		field.setValue('');
		field.setValueMap(valueMap);
		
		isc.WindowStack.popup(null, "Charting", true, [isc.ChartDialog._layout]);
	}
});
