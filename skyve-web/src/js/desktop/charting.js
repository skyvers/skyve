/**
 * Implements charting functionality.
 */
isc.ClassFactory.defineClass("ChartDialog");

isc.ChartDialog.addClassProperties({
	_valuesManager: isc.ValuesManager.create(),
});

isc.ChartDialog.addClassProperties({
	_chartForm: isc.DynamicForm.create({
		valuesManager: isc.ChartDialog._valuesManager,
		numCols: 6,
		colWidths: [100, 120, 80, "*", 90, "*"],
		items: [
			{
				name: "type",
				type: "enum",
				title: "Chart Type",
				width: "*",
				valueMap: {
					line: "Line",
					lineArea: "Line Area",
					bar: "Bar",
					horizontalBar: "Horizontal Bar",
					radar: "Radar",
					pie: "Pie",
					doughnut: "Doughnut",
					polarArea: "Polar Area",
				},
				required: true,
			},
			{ name: "title", type: "text", width: "*", title: "Chart Title" },
			{ name: "label", type: "text", width: "*", title: "Data Label" },
		],
	}),

	_categoryForm: isc.DynamicForm.create({
		valuesManager: isc.ChartDialog._valuesManager,
		numCols: 7,
		colWidths: [100, "*", 110, 160, 60, 60, 150],
		items: [
			{
				name: "categoryBinding",
				type: "select",
				title: "Category Field",
				width: "*",
				required: true,
				valueMap: [],
			},
			{
				name: "categoryBucket",
				type: "enum",
				title: "Category Bucket",
				width: "*",
				allowEmptyValue: true,
				valueMap: {
					NumericMultipleBucket: "Numeric Multiple",
					TextLengthBucket: "Text Length",
					TextStartsWithBucket: "Text Starts With",
					TemporalBucket: "Temporal",
				},
				redrawOnChange: true,
			},
			{
				name: "numericMultiple",
				type: "spinner",
				width: "*",
				title: "Multiple",
				min: 1,
				step: 1,
				defaultValue: 100,
				validators: [{ type: "requiredIf", expression: "item.isVisible()" }],
				showIf:
					"form.getItem('categoryBucket').getValue() == 'NumericMultipleBucket'",
			},
			{
				name: "startsWithLength",
				type: "spinner",
				width: "*",
				title: "Length",
				min: 1,
				step: 1,
				defaultValue: 1,
				validators: [{ type: "requiredIf", expression: "item.isVisible()" }],
				showIf:
					"form.getItem('categoryBucket').getValue() == 'TextStartsWithBucket'",
			},
			{
				name: "startsWithCaseSensitive",
				type: "checkbox",
				allowEmptyValue: false,
				title: "Case Sensitive",
				showTitle: false,
				defaultValue: false,
				validators: [{ type: "requiredIf", expression: "item.isVisible()" }],
				showIf:
					"form.getItem('categoryBucket').getValue() == 'TextStartsWithBucket'",
			},
			{
				name: "temporalBucketType",
				type: "enum",
				width: "*",
				title: "Type",
				colSpan: 2,
				allowEmptyValue: false,
				valueMap: {
					day: "Day",
					dayMonthYear: "Day/Month/Year",
					hour: "Hour",
					hourDay: "Hour/Day",
					hourDayMonth: "Hour/Day/Month",
					minuteHour: "Minute/Hour",
					month: "Month",
					monthYear: "Month/Year",
					secondMinuteHour: "Second/Minute/Hour",
					year: "Year",
				},
				validators: [{ type: "requiredIf", expression: "item.isVisible()" }],
				showIf: "form.getItem('categoryBucket').getValue() == 'TemporalBucket'",
			},
		],
	}),

	_valueForm: isc.DynamicForm.create({
		valuesManager: isc.ChartDialog._valuesManager,
		numCols: 4,
		colWidths: [100, "*", 105, 100],
		items: [
			{
				name: "valueBinding",
				type: "select",
				title: "Value Field",
				startRow: true,
				width: "*",
				required: true,
				valueMap: [],
			},
			{
				name: "valueFunction",
				type: "enum",
				width: "*",
				title: "Value Function",
				valueMap: [null, "Count", "Avg", "Sum", "Min", "Max"],
			},
		],
	}),

	_topForm: isc.DynamicForm.create({
		valuesManager: isc.ChartDialog._valuesManager,
		numCols: 8,
		colWidths: [30, 40, 60, 40, 130, 40, 100, 130],
		items: [
			{
				name: "topOn",
				type: "checkbox",
				allowEmptyValue: false,
				showTitle: false,
				showLabel: false,
				defaultValue: false,
				redrawOnChange: true,
				change: function (form, item, value, oldValue) {
					const topItem = form.getItem("top");
					const topByItem = form.getItem("topBy");
					const topSortItem = form.getItem("topSort");
					const includeOthersItem = form.getItem("includeOthers");

					if (value) {
						topItem.enable();
						topByItem.enable();
						topSortItem.enable();
						includeOthersItem.enable();
					} else {
						if (!topItem.getValue()) {
							topItem.setValue("10");
							topItem.clearErrors();
						}
						topItem.disable();
						topByItem.disable();
						topSortItem.disable();
						includeOthersItem.disable();
					}
				},
			},
			{
				name: "top",
				type: "spinner",
				title: "Top",
				width: "*",
				required: true,
				disabled: true,
				min: 1,
				max: 9999,
				step: 1,
				defaultValue: 10,
			},
			{
				name: "topBy",
				type: "radioGroup",
				title: "By",
				vertical: false,
				required: true,
				disabled: true,
				defaultValue: "category",
				valueMap: { category: "Category", value: "Value" },
			},
			{
				name: "topSort",
				type: "radioGroup",
				title: "Sort",
				vertical: false,
				required: true,
				disabled: true,
				defaultValue: "ascending",
				valueMap: { ascending: "Ascending", descending: "Descending" },
			},
			{
				name: "includeOthers",
				type: "checkbox",
				allowEmptyValue: false,
				title: "Include Others",
				showTitle: false,
				disabled: true,
				defaultValue: false,
			},
		],
	}),

	_orderForm: isc.DynamicForm.create({
		valuesManager: isc.ChartDialog._valuesManager,
		numCols: 5,
		colWidths: [30, 40, 130, 40, "*"],
		items: [
			{
				name: "orderOn",
				type: "checkbox",
				allowEmptyValue: false,
				showTitle: false,
				showLabel: false,
				defaultValue: false,
				change: function (form, item, value, oldValue) {
					const orderByItem = form.getItem("orderBy");
					const orderSortItem = form.getItem("orderSort");

					if (value) {
						orderByItem.enable();
						orderSortItem.enable();
					} else {
						orderByItem.disable();
						orderSortItem.disable();
					}
				},
			},
			{
				name: "orderBy",
				type: "radioGroup",
				title: "By",
				vertical: false,
				required: true,
				disabled: true,
				defaultValue: "category",
				valueMap: { category: "Category", value: "Value" },
			},
			{
				name: "orderSort",
				type: "radioGroup",
				title: "Sort",
				vertical: false,
				disabled: true,
				defaultValue: "ascending",
				valueMap: { ascending: "Ascending", descending: "Descending" },
			},
		],
	}),

	_chart: null,
	_layout: null, // The entire interface
	_criteria: null, // The extra criteria defined on the query or model
	_tagId: null, // The bizId of the selected tag (if any) in the listgrid
	_dataSource: null, // The data source representing the server-side query or model

	/**
	 * Creates a bordered layout with a title and members.
	 * @param {string} title - the title of the border.
	 * @param {Array} members - the layout members array.
	 * @returns {isc.VLayout} the created layout.
	 */
	_border: function (title, members) {
		return isc.VLayout.create({
			isGroup: true,
			groupTitle: title,
			styleName: "bizhubRoundedBorder",
			groupBorderCSS: "1px solid #bfbfbf",
			padding: 5,
			height: 1,
			groupLabelBackgroundColor: "transparent",
			groupLabelStyleName: "bizhubBorderLabel",
			backgroundImage: "background.png",
			backgroundRepeat: "repeat",
			members,
		});
	},

	/**
	 * Creates a chart panel with a button and form.
	 * @param {isc.IButton} buttonDefn - the button to include in the panel.
	 * @param {isc.DynamicForm} formDefn - the RHS form.
	 * @returns {isc.HLayout} the created panel.
	 */
	_createChartPanel: function (buttonDefn, formDefn) {
		return isc.HLayout.create({
			backgroundImage: "background.png",
			backgroundRepeat: "repeat",
			membersMargin: 5,
			margin: 5,
			height: 1,
			layoutAlign: "center",
			members: [
				isc.VLayout.create({
					backgroundImage: "background.png",
					backgroundRepeat: "repeat",
					width: 134,
					height: 130,
					layoutAlign: "center",
					margin: 0,
					membersMargin: 10,
					members: [
						isc.HLayout.create({
							width: 134,
							align: "center",
							members: [
								isc.Img.create({
									imageType: "normal",
									src: "reporting/reporting.png",
									height: 96,
									layoutAlign: "center",
								}),
							],
						}),
						isc.HLayout.create({
							width: 134,
							align: "center",
							members: [buttonDefn],
						}),
					],
				}),
				formDefn,
			],
		});
	},

	/**
	 * Constructs the interface.
	 */
	_create: function () {
		if (isc.ChartDialog._layout === null) {
			isc.ChartDialog._chart = isc.BizChart.create({});
			const result = isc.ChartDialog._border("Result", [isc.ChartDialog._chart]);
			result.setHeight("100%");
			const chart = isc.ChartDialog._border("Chart", [
				isc.ChartDialog._chartForm,
				isc.ChartDialog._categoryForm,
				isc.ChartDialog._valueForm,
			]);

			isc.ChartDialog._layout = isc.VLayout.create({
				backgroundImage: "background.png",
				backgroundRepeat: "repeat",
				height: "100%",
				width: "100%",
				autoDraw: true,
				margin: 5,
				membersMargin: 5,
				members: [
					isc.ChartDialog._createChartPanel(
						isc.IButton.create({
							title: "Generate",
							click() {
								if (isc.ChartDialog._valuesManager.validate()) {
									const params = {
										ds: isc.ChartDialog._dataSource.ID,
										t: isc.ChartDialog._valuesManager.getValue("type"),
										b: isc.ChartDialog._valuesManager.getValues(),
									};

									if (isc.ChartDialog._criteria) {
										params.criteria = isc.ChartDialog._criteria;
									}
									if (isc.ChartDialog._tagId) {
										params.tagId = isc.ChartDialog._tagId;
									}

									validateCategoryBucket(params);
									validateValueFunction(params);

									if (!params.b.label) {
										params.b.label = generateDataLabel(params);
									}

									isc.ChartDialog._valuesManager.clearErrors(true);

									isc.RPCManager.sendRequest({
										showPrompt: true,
										evalResult: true,
										actionURL: SKYVE.Util.CONTEXT_URL + "chart",
										httpMethod: "POST",
										params,
										callback(rpcResponse, data) {
											if (data.config) {
												const oldChartType = isc.ChartDialog._chart?.chartConfig?.type;
												const newChartType = data.config.type;

												if (oldChartType !== newChartType) {
													isc.ChartDialog._destroyAndRecreate();
												}
												isc.ChartDialog._chart._update(data);
											}
										},
									});
								}
							},
						}),
						isc.VLayout.create({
							backgroundImage: "background.png",
							backgroundRepeat: "repeat",
							margin: 0,
							height: 1,
							membersMargin: 5,
							members: [
								chart,
								isc.ChartDialog._border("Top", [isc.ChartDialog._topForm]),
								isc.ChartDialog._border("Order", [isc.ChartDialog._orderForm]),
							],
						}),
					),
					result,
				],
			});
		} else {
			isc.ChartDialog._destroyAndRecreate();
		}
	},

	/**
	 * Destroys and recreates the chart.
	 */
	_destroyAndRecreate: function () {
		let border = null;
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
		} else {
			isc.ChartDialog._layout.addMember(
				isc.ChartDialog._border("Result", [isc.ChartDialog._chart]),
			);
		}
	},

	/**
	 * Populates the chart dialog with data and displays it.
	 * @param {Object} dataSource - the data source representing the server-side query or model.
	 * @param {string} _c - the web context identifier.
	 * @param {Object} criteria - the criteria to apply to the server-side query or model.
	 * @param {string} tagId - the tagId of the selected tag from the listgrid to apply server-side.
	 */
	popupChart: function (dataSource, _c, criteria, tagId) {
		isc.ChartDialog._create();

		isc.ChartDialog._dataSource = dataSource;
		isc.ChartDialog._c = _c;
		isc.ChartDialog._criteria = criteria;
		isc.ChartDialog._tagId = tagId;

		const valueMap = createValueMap(dataSource);
		const categoryField =
			isc.ChartDialog._categoryForm.getItem("categoryBinding");
		const valueField = isc.ChartDialog._valueForm.getItem("valueBinding");

		categoryField.setValue("");
		categoryField.setValueMap(valueMap);
		valueField.setValue("");
		valueField.setValueMap(valueMap);

		isc.WindowStack.popup(null, "Charting", true, [isc.ChartDialog._layout]);
	},
});

/**
 * Creates a value map from the data source fields.
 * @param {Object} dataSource - the data source.
 * @returns {Object} - the value map.
 */
createValueMap = function (dataSource) {
	const valueMap = {};
	const fieldNames = dataSource.getFieldNames(true); // no hidden fields
	fieldNames.forEach((fieldName) => {
		if (fieldName !== "bizTagged" && fieldName !== "bizFlagComment") {
			const field = dataSource.getField(fieldName);
			valueMap[field.name] = field.title;
		}
	});
	return valueMap;
};

/**
 * Validates the category bucket based on the field type.
 * @param {Object} params - the parameters object.
 */
validateCategoryBucket = function (params) {
	const { categoryBinding, categoryBucket } = params.b;
	if (categoryBucket) {
		const field = isc.ChartDialog._dataSource.getField(categoryBinding);
		const { type } = field;

		if (isc.BizUtil.isNumeric(type)) {
			if (categoryBucket !== "NumericMultipleBucket") {
				setCategoryBucketErrors("Use a numeric bucket only with a number field");
				return;
			}
		} else if (isc.BizUtil.isTemporal(type)) {
			if (categoryBucket !== "TemporalBucket") {
				setCategoryBucketErrors(
					"Use a temporal bucket only with a date or time field",
				);
				return;
			}
		} else if (
			categoryBucket === "TemporalBucket" ||
			categoryBucket === "NumericMultipleBucket"
		) {
			setCategoryBucketErrors(
				"Use a temporal bucket only with a date or time field",
			);
			return;
		}
	}
};

/**
 * Sets errors for the category bucket fields.
 * @param {string} errorMessage - the error message to set.
 */
setCategoryBucketErrors = function (errorMessage) {
	isc.ChartDialog._valuesManager.setErrors(
		{
			categoryBinding: errorMessage,
			categoryBucket: errorMessage,
		},
		true,
	);
};

/**
 * Validates the value function based on the field type.
 * @param {Object} params - the parameters object.
 */
validateValueFunction = function (params) {
	const { valueBinding, valueFunction } = params.b;
	const field = isc.ChartDialog._dataSource.getField(valueBinding);
	const { type } = field;

	if (type !== "integer" && type !== "float" && valueFunction !== "Count") {
		isc.ChartDialog._valuesManager.setErrors(
			{
				valueBinding: "Use the [Count] value function with a non-numeric field",
				valueFunction: "Use the [Count] value function with a non-numeric field",
			},
			true,
		);
	}
};

/**
 * Generates a data label if none is defined.
 * @param {Object} params - the parameters object.
 * @returns {string} the generated data label.
 */
generateDataLabel = function (params) {
	const { valueFunction } = params.b;
	const valueFunctionDisplay = valueFunction
		? isc.ChartDialog._valueForm.getItem("valueFunction").getDisplayValue()
		: "";
	const valueBindingDisplay = isc.ChartDialog._valueForm
		.getItem("valueBinding")
		.getDisplayValue();
	const categoryBindingDisplay = isc.ChartDialog._categoryForm
		.getItem("categoryBinding")
		.getDisplayValue();

	return `${valueFunctionDisplay} ${valueBindingDisplay} by ${categoryBindingDisplay}`;
};
