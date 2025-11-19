/**
 * Implements ReportDialog in the UI.
 */
isc.ClassFactory.defineClass("ReportDialog");

isc.ReportDialog.addClassProperties({
	/**
	 * Predefined page formats with dimensions in points.
	 * @constant {Object.<string, {width: number, height: number}>}
	 */
	_pageFormats: {
		LETTER: { width: 612, height: 792 },
		NOTE: { width: 540, height: 720 },
		LEGAL: { width: 612, height: 1008 },
		A0: { width: 2380, height: 3368 },
		A1: { width: 1684, height: 2380 },
		A2: { width: 1190, height: 1684 },
		A3: { width: 842, height: 1190 },
		A4: { width: 595, height: 842 },
		A5: { width: 421, height: 595 },
		A6: { width: 297, height: 421 },
		A7: { width: 210, height: 297 },
		A8: { width: 148, height: 210 },
		A9: { width: 105, height: 148 },
		A10: { width: 74, height: 105 },
		B0: { width: 2836, height: 4008 },
		B1: { width: 2004, height: 2836 },
		B2: { width: 1418, height: 2004 },
		B3: { width: 1002, height: 1418 },
		B4: { width: 709, height: 1002 },
		B5: { width: 501, height: 709 },
		ARCH_E: { width: 595, height: 842 },
		ARCH_D: { width: 595, height: 842 },
		ARCH_C: { width: 595, height: 842 },
		ARCH_B: { width: 595, height: 842 },
		ARCH_A: { width: 595, height: 842 },
		FLSA: { width: 612, height: 936 },
		FLSE: { width: 612, height: 936 },
		HALFLETTER: { width: 396, height: 612 },
		"11x17": { width: 792, height: 1224 },
		LEDGER: { width: 1224, height: 792 },
	},

	/**
	 * Defines the available columns for selection.
	 * @constant {Array<Object>}
	 */
	_available: [
		{ name: "title", title: "Available Columns", required: true },
		{
			name: "line",
			title: "Line",
			type: "integer",
			editorType: "spinner",
			editorProperties: {
				min: 1,
				max: 9,
				step: 1,
				defaultValue: 1,
				validators: [{ type: "integerRange", min: 1, max: 9 }],
			},
			width: 50,
		},
		{
			name: "width",
			title: "Width",
			type: "integer",
			editorType: "spinner",
			editorProperties: {
				min: 1,
				max: 9999,
				step: 10,
				defaultValue: 1,
				validators: [{ type: "integerRange", min: 1, max: 9999 }],
			},
			width: 50,
		},
	],

	/**
	 * Defines the selected columns for export.
	 * @constant {Array<Object>}
	 */
	_selected: [
		{ name: "title", title: "Exported Columns", required: true },
		{
			name: "line",
			title: "Line",
			type: "integer",
			editorType: "spinner",
			editorProperties: {
				min: 1,
				max: 9,
				step: 1,
				defaultValue: 1,
				validators: [{ type: "integerRange", min: 1, max: 9 }],
			},
			width: 50,
		},
		{
			name: "width",
			title: "Width",
			type: "integer",
			editorType: "spinner",
			editorProperties: {
				min: 1,
				max: 9999,
				step: 10,
				defaultValue: 1,
				validators: [{ type: "integerRange", min: 1, max: 9999 }],
			},
			width: 50,
		},
	],

	/**
	 * Manages form values for the report dialog.
	 * @constant {Object}
	 */
	_valuesManager: isc.ValuesManager.create(),
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
		fields: isc.ReportDialog._available,
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
		editEvent: "click",
		modalEditing: true,
		alternateRecordStyles: true,
		autoFetchData: false,
		preventDuplicates: true,
		leaveScrollbarGap: false,
		showHeaderContextMenu: false,
		fields: isc.ReportDialog._selected,
	}),
});

isc.ReportDialog.addClassProperties({
	/**
	 * Generates valueMap and valueIcons for report formats.
	 *
	 * @private
	 * @param {string[]} allowedFormats - list of allowed report formats.
	 * @param {string[]} [defaultFormats=[]] - list of preferred default formats.
	 * @returns {{ valueMap: Object, valueIcons: Object, defaultValue: string|null }} - An object containing valueMap, valueIcons, and the default format.
	 */
	_generateReportFormatMappings: function (allowedFormats, defaultFormats = []) {
		const formatMappings = {
			pdf: { label: "PDF (Adobe Portable Document Format)", icon: "pdf" },
			docx: { label: "DOCX (Word Document - Office 2007)", icon: "rtf" },
			xlsx: { label: "XLSX (Excel Document - Office 2007)", icon: "xls" },
			pptx: { label: "PPTX (PowerPoint Document - Office 2007)", icon: "pptx" },
			xls: { label: "XLS (Excel - 98-2003)", icon: "xls" },
			rtf: { label: "RTF (Rich Text Format)", icon: "rtf" },
			ods: { label: "ODS (Open Document Spreadsheet Format)", icon: "oo" },
			odt: { label: "ODT (Open Document Text Format)", icon: "oo" },
			html: { label: "HTML (HyperText Markup Language)", icon: "html" },
			csv: { label: "CSV (Comma Separated Values)", icon: "csv" },
			xml: { label: "XML (JRXML Format)", icon: "xml" },
			txt: { label: "TXT (Text Format)", icon: "txt" },
		};

		const valueMap = {};
		const valueIcons = {};
		let defaultValue = defaultFormats.length ? null : allowedFormats[0];

		allowedFormats.forEach((format) => {
			if (formatMappings[format]) {
				valueMap[format] = formatMappings[format].label;
				valueIcons[format] = formatMappings[format].icon;
			} else {
				valueMap[format] = format;
				valueIcons[format] = format;
			}

			// Set default value if it's one of the preferred defaults
			if (!defaultValue && defaultFormats.includes(format)) {
				defaultValue = format;
			}
		});

		return { valueMap, valueIcons, defaultValue };
	},
	/**
	 * Creates a picklist for selecting Jasper report formats.
	 *
	 * @private
	 * @param {number} colSpan - the column span for the picklist.
	 * @param {Function} onChangeFunction - callback function triggered on change.
	 * @returns {Object} - configuration object for the picklist.
	 */
	_createJasperReportFormatPickList: function (colSpan, onChangeFunction) {
		const { valueMap, valueIcons, defaultValue } =
			this._generateReportFormatMappings(SKYVE.Util.allowedReportFormats);

		return {
			name: "reportFormat",
			showTitle: false,
			type: "select",
			width: 300,
			required: true,
			valueMap,
			imageURLPrefix: "reporting/",
			imageURLSuffix: ".png",
			valueIcons,
			defaultValue,
			colSpan,
			change: onChangeFunction,
		};
	},
});

isc.ReportDialog.addClassProperties({
	/**
	 * Layout for the column selector, including buttons to move columns between lists.
	 * @type {HLayout}
	 */
	_columnSelectorLayout: isc.HLayout.create({
		membersMargin: 10,
		height: "100%", // Height is needed to center the arrow
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
						/**
						 * Handles the click event to move selected columns into the report.
						 */
						click: function () {
							return isc.ReportDialog._selectedColumnList.transferSelectedData(
								isc.ReportDialog._columnList,
							);
						},
						canHover: true,
						/**
						 * Returns the hover tooltip text.
						 * @returns {string} tooltip text.
						 */
						getHoverHTML: function () {
							return "Move the selected columns into the report";
						},
					}),
					isc.IButton.create({
						title: null,
						icon: "reporting/arrow_left.png",
						iconWidth: 24,
						iconHeight: 24,
						iconAlign: "center",
						width: 36,
						height: 36,
						/**
						 * Handles the click event to move selected columns out of the report.
						 */
						click: function () {
							isc.ReportDialog._columnList.transferSelectedData(
								isc.ReportDialog._selectedColumnList,
							);
						},
						canHover: true,
						/**
						 * Returns the hover tooltip text.
						 * @returns {string} tooltip text.
						 */
						getHoverHTML: function () {
							return "Move the selected columns out of the report";
						},
					}),
				],
			}),
			isc.ReportDialog._selectedColumnList,
		],
	}),

	/**
	 * Form for configuring the report format.
	 * @type {DynamicForm|null}
	 */
	_reportFormatForm: null,

	/**
	 * Form for configuring the page format, including size and orientation.
	 * @type {DynamicForm}
	 */
	_pageFormatForm: isc.DynamicForm.create({
		valuesManager: isc.ReportDialog._valuesManager,
		numCols: 10,
		padding: 15,
		items: [
			{
				name: "format",
				title: "Format",
				type: "select",
				required: true,
				width: 80,
				valueMap: [
					"LETTER",
					"NOTE",
					"LEGAL",
					"A0",
					"A1",
					"A2",
					"A3",
					"A4",
					"A5",
					"A6",
					"A7",
					"A8",
					"A9",
					"A10",
					"B0",
					"B1",
					"B2",
					"B3",
					"B4",
					"B5",
					"ARCH_E",
					"ARCH_D",
					"ARCH_C",
					"ARCH_B",
					"ARCH_A",
					"FLSA",
					"FLSE",
					"HALFLETTER",
					"11x17",
					"LEDGER",
				],
				defaultValue: "A4",
				/**
				 * Handles the change event for the format dropdown.
				 * Updates the width and height fields based on the selected format and orientation.
				 * @param {DynamicForm} form - the form instance.
				 * @param {FormItem} item - the form item that triggered the change.
				 * @param {string} value - the new value of the format dropdown.
				 * @param {string} oldValue - the previous value of the format dropdown.
				 */
				change: function (form, item, value, oldValue) {
					const orientation = form.getItem("orientation").getValue();
					const { width, height } = isc.ReportDialog._pageFormats[value];
					form
						.getItem("width")
						.setValue(orientation === "portrait" ? width : height);
					form
						.getItem("height")
						.setValue(orientation === "portrait" ? height : width);
				},
			},
			{
				name: "orientation",
				title: "Orientation",
				type: "radioGroup",
				required: true,
				valueMap: { portrait: "Portrait", landscape: "Landscape" },
				defaultValue: "portrait",
				/**
				 * Handles the change event for the orientation radio group.
				 * Swaps the width and height values when the orientation changes.
				 * @param {DynamicForm} form - the form instance.
				 * 		@param {FormItem} item - the form item that triggered the change.
				 * @param {string} value - the new value of the format dropdown.
				 * @param {string} oldValue - the previous value of the format dropdown.
				 */
				change: function (form, item, value, oldValue) {
					const widthItem = form.getItem("width");
					const heightItem = form.getItem("height");
					const width = widthItem.getValue();
					widthItem.setValue(heightItem.getValue());
					heightItem.setValue(width);
				},
			},
			{
				name: "width",
				title: "Width",
				type: "spinner",
				required: true,
				width: 80,
				min: 1,
				max: 9999,
				step: 1,
				defaultValue: isc.ReportDialog._pageFormats["A4"].width,
				validators: [{ type: "integerRange", min: 1, max: 9999 }],
			},
			{
				name: "height",
				title: "Height",
				type: "spinner",
				required: true,
				width: 80,
				min: 1,
				max: 9999,
				step: 1,
				defaultValue: isc.ReportDialog._pageFormats["A4"].height,
				validators: [{ type: "integerRange", min: 1, max: 9999 }],
			},
		],
	}),

	/**
	 * Form for configuring the page margins.
	 * @type {DynamicForm}
	 */
	_marginsForm: isc.DynamicForm.create({
		valuesManager: isc.ReportDialog._valuesManager,
		numCols: 8,
		padding: 15,
		items: [
			{
				name: "top",
				title: "Top",
				type: "spinner",
				required: true,
				width: 80,
				min: 1,
				max: 9999,
				step: 1,
				defaultValue: 20,
				validators: [{ type: "integerRange", min: 1, max: 9999 }],
			},
			{
				name: "bottom",
				title: "Bottom",
				type: "spinner",
				required: true,
				width: 80,
				min: 1,
				max: 9999,
				step: 1,
				defaultValue: 20,
				validators: [{ type: "integerRange", min: 1, max: 9999 }],
			},
			{
				name: "left",
				title: "Left",
				type: "spinner",
				required: true,
				width: 80,
				min: 1,
				max: 9999,
				step: 1,
				defaultValue: 20,
				validators: [{ type: "integerRange", min: 1, max: 9999 }],
			},
			{
				name: "right",
				title: "Right",
				type: "spinner",
				required: true,
				width: 80,
				min: 1,
				max: 9999,
				step: 1,
				defaultValue: 20,
				validators: [{ type: "integerRange", min: 1, max: 9999 }],
			},
		],
	}),

	/**
	 * Form for submitting the report configuration.
	 * @type {DynamicForm}
	 */
	_submitForm: isc.DynamicForm.create({
		autoDraw: true,
		canSubmit: true,
		method: "POST",
		items: [
			{ name: "values", type: "hidden" },
			{ name: "_c", type: "hidden" },
		],
	}),

	_layout: null, // The entire interface
	_criteria: null, // The extra criteria defined on the query or model
	_tagId: null, // The bizId of the selected tag (if any) in the listgrid
	_dataSourceID: null, // The data source representing the server-side query or model

	/**
	 * Creates a report panel with a button and a form.
	 * @param {IButton} buttonDefn - the button to include in the panel.
	 * @param {DynamicForm} formDefn - the form to display on the right-hand side.
	 * @returns {HLayout} the configured HLayout instance.
	 */
	_createReportPanel: function (buttonDefn, formDefn) {
		return isc.HLayout.create({
			backgroundImage: "background.png",
			backgroundRepeat: "repeat",
			membersMargin: 5,
			margin: 5,
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
	 * Constructs the export interface.
	 */
	_createExport: function () {
		if (isc.ReportDialog._exportLayout == null) {
			/**
			 * Handles the change event for the report format dropdown.
			 * Updates form fields based on the selected format.
			 * @param {DynamicForm} form - the form instance.
			 * @param {FormItem} item - the form item that triggered the change.
			 * @param {string} value - the new value of the format dropdown.
			 * @param {string} oldValue - the previous value of the format dropdown.
			 */
			const changeHandler = function (form, item, value, oldValue) {
				const isDocumentFormat = ["pdf", "docx", "pptx", "rtf", "odt"].includes(
					value,
				);
				const isHtmlFormat = value === "html";

				form.getItem("isPaginated").setValue(isDocumentFormat);
				form.getItem("isPretty").setValue(isDocumentFormat || isHtmlFormat);
				form.getItem("showSummary").setValue(isDocumentFormat || isHtmlFormat);
				form.getItem("fileNameSuffix").setValue(`.${value}`);
			};

			isc.ReportDialog._reportFormatForm = isc.DynamicForm.create({
				valuesManager: isc.ReportDialog._valuesManager,
				numCols: 7,
				colWidths: [60, 200, 40, 30, "*", 60, "*"],
				padding: 15,
				items: [
					isc.ReportDialog._createJasperReportFormatPickList(3, changeHandler),
					{
						name: "isPaginated",
						title: "Paginated Report",
						type: "checkbox",
						required: true,
					},
					{
						name: "style",
						title: "Style",
						type: "radioGroup",
						vertical: true,
						required: true,
						rowSpan: 2,
						valueMap: { tabular: "Tabular", columnar: "Columnar" },
						defaultValue: "tabular",
						/**
						 * Handles the change event for the style radio group.
						 * Shows or hides table columns based on the selected style.
						 * @param {DynamicForm} form - the form instance.
						 * @param {FormItem} item - the form item that triggered the change.
						 * @param {string} value - the new value of the radio group.
						 * @param {string} oldValue- the previous value of the radio group.
						 */
						change: function (form, item, value, oldValue) {
							const isColumnar = value === "columnar";
							isc.ReportDialog._columnList[isColumnar ? "hideField" : "showField"](
								"line",
							);
							isc.ReportDialog._columnList[isColumnar ? "hideField" : "showField"](
								"width",
							);
							isc.ReportDialog._selectedColumnList[
								isColumnar ? "hideField" : "showField"
							]("line");
							isc.ReportDialog._selectedColumnList[
								isColumnar ? "hideField" : "showField"
							]("width");
						},
					},
					{
						name: "fileNameNoSuffix",
						title: "Filename",
						type: "text",
						required: true,
						width: "100%",
						defaultValue: "export",
					},
					{
						name: "fileNameSuffix",
						showTitle: false,
						type: "staticText",
						startRow: false,
						endRow: false,
					},
					{
						name: "isPretty",
						title: "Pixel Perfect",
						type: "checkbox",
						required: true,
					},
					{ type: "spacer" },
					{ type: "spacer" },
					{ type: "spacer" },
					{
						name: "showSummary",
						title: "Show Summary",
						type: "checkbox",
						required: true,
					},
				],
			});

			// Initialize the form with the first allowed report format
			changeHandler(
				isc.ReportDialog._reportFormatForm,
				null,
				SKYVE.Util.allowedReportFormats[0],
				null,
			);

			isc.ReportDialog._exportLayout = isc.VLayout.create({
				backgroundImage: "background.png",
				backgroundRepeat: "repeat",
				height: "100%",
				width: "100%",
				autoDraw: true,
				margin: 5,
				membersMargin: 5,
				members: [
					isc.ReportDialog._createReportPanel(
						isc.IButton.create({
							title: "Generate",
							/**
							 * Handles the click event to generate the report.
							 */
							click: function () {
								if (isc.ReportDialog._valuesManager.validate()) {
									const values = isc.ReportDialog._valuesManager.getValues();
									values.columns = isc.ReportDialog._selectedColumnList.getData();
									values.ds = isc.ReportDialog._dataSourceID;

									if (isc.ReportDialog._criteria) {
										values.criteria = isc.ReportDialog._criteria;
									}
									if (isc.ReportDialog._tagId) {
										values.tagId = isc.ReportDialog._tagId;
									}

									// Use a standard form POST, HTML targeted to a blank window
									const format = isc.ReportDialog._reportFormatForm
										.getItem("reportFormat")
										.getValue();
									const fileNameNoSuffix = isc.ReportDialog._reportFormatForm
										.getItem("fileNameNoSuffix")
										.getValue();
									isc.ReportDialog._submitForm.setValue(
										"values",
										isc.JSON.encode(values, { prettyPrint: false }),
									);
									if (isc.ReportDialog._c) {
										isc.ReportDialog._submitForm.setValue("_c", isc.ReportDialog._c);
									}
									isc.ReportDialog._submitForm.setAction(
										`export/${fileNameNoSuffix}.${format}`,
									);
									isc.ReportDialog._submitForm.setTarget(
										format === "html" ? "_blank" : "_self",
									);
									isc.ReportDialog._submitForm.submitForm();
								}
							},
						}),
						isc.VLayout.create({
							backgroundImage: "background.png",
							backgroundRepeat: "repeat",
							margin: 0,
							membersMargin: 5,
							members: [
								isc.VLayout.create({
									isGroup: true,
									groupTitle: "Report Format",
									styleName: "bizhubRoundedBorder",
									groupBorderCSS: "1px solid #bfbfbf",
									margin: 1,
									groupLabelBackgroundColor: "transparent",
									groupLabelStyleName: "bizhubBorderLabel",
									backgroundImage: "background.png",
									backgroundRepeat: "repeat",
									members: [isc.ReportDialog._reportFormatForm],
								}),
								isc.VLayout.create({
									isGroup: true,
									groupTitle: "Page Format",
									styleName: "bizhubRoundedBorder",
									groupBorderCSS: "1px solid #bfbfbf",
									margin: 1,
									groupLabelBackgroundColor: "transparent",
									groupLabelStyleName: "bizhubBorderLabel",
									backgroundImage: "background.png",
									backgroundRepeat: "repeat",
									members: [isc.ReportDialog._pageFormatForm],
								}),
								isc.VLayout.create({
									isGroup: true,
									groupTitle: "Margins",
									styleName: "bizhubRoundedBorder",
									groupBorderCSS: "1px solid #bfbfbf",
									margin: 1,
									groupLabelBackgroundColor: "transparent",
									groupLabelStyleName: "bizhubBorderLabel",
									backgroundImage: "background.png",
									backgroundRepeat: "repeat",
									members: [isc.ReportDialog._marginsForm],
								}),
							],
						}),
					),
					isc.ReportDialog._columnSelectorLayout,
				],
			});
		}
	},

	/**
	 * Opens the export popup and sets up the necessary data for exporting.
	 *
	 * @param {string} dataSourceID - the ID of the data source (server-side query/model).
	 * @param {string} _c - the web context identifier.
	 * @param {Object} criteria - the criteria to apply to the server-side query or model.
	 * @param {string} tagId - the tag ID of the selected tag from the list grid to apply server-side.
	 * @param {Array} unselectedFields - data for the unselected fields in the field selection list grid.
	 * @param {Array} selectedFields - data for the selected fields in the field selection list grid.
	 */
	popupExport: function (
		dataSourceID,
		_c,
		criteria,
		tagId,
		unselectedFields,
		selectedFields,
	) {
		isc.ReportDialog._createExport();
		isc.ReportDialog._dataSourceID = dataSourceID;
		isc.ReportDialog._c = _c;
		isc.ReportDialog._criteria = criteria;
		isc.ReportDialog._tagId = tagId;
		isc.ReportDialog._columnList.setData(unselectedFields);
		isc.ReportDialog._selectedColumnList.setData(selectedFields);
		isc.WindowStack.popup(null, "Reporting", true, [
			isc.ReportDialog._exportLayout,
		]);
	},

	/**
	 * Opens the report popup based on the view and parameters provided.
	 *
	 * @param {string} view - the view name.
	 * @param {Object} params - the parameter map (name -> binding expression with {} if required).
	 */
	popupReport: function (view, params) {
		if (params && params._f) {
			const { _f: format, ...paramsCopy } = params;
			isc.ReportDialog._redirectToReport(view, paramsCopy, format);
		} else if (params && params._e === "Freemarker") {
			if (!isc.ReportDialog._freemarkerReportLayout) {
				isc.ReportDialog._freemarkerReportLayout =
					isc.ReportDialog._createReportPanel(
						isc.IButton.create({
							title: "View",
							ID: "_freemarkerReportViewButton",
							_view: null,
							_params: null,
							click: function () {
								const format = _freemarkerReportForm.getValue("reportFormat");
								isc.ReportDialog._redirectToReport(this._view, this._params, format);
							},
						}),
						isc.DynamicForm.create({
							ID: "_freemarkerReportForm",
							fields: [
								{ type: "rowSpacer" },
								{ type: "rowSpacer" },
								isc.ReportDialog._createFreemarkerReportFormatPickList(),
							],
						}),
					);
			}

			_freemarkerReportViewButton._view = view;
			_freemarkerReportViewButton._params = params;
			isc.WindowStack.popup(
				null,
				"Report",
				true,
				[isc.ReportDialog._freemarkerReportLayout],
				180,
				480,
			);
		} else {
			if (!isc.ReportDialog._jasperReportLayout) {
				isc.ReportDialog._jasperReportLayout = isc.ReportDialog._createReportPanel(
					isc.IButton.create({
						title: "View",
						ID: "_jasperReportViewButton",
						_view: null,
						_params: null,
						click: function () {
							const format = _jasperReportForm.getValue("reportFormat");
							isc.ReportDialog._redirectToReport(this._view, this._params, format);
						},
					}),
					isc.DynamicForm.create({
						ID: "_jasperReportForm",
						fields: [
							{ type: "rowSpacer" },
							{ type: "rowSpacer" },
							isc.ReportDialog._createJasperReportFormatPickList(
								1,
								function (form, item, value, oldValue) {
									// No operation needed
								},
							),
						],
					}),
				);
			}

			_jasperReportViewButton._view = view;
			_jasperReportViewButton._params = params;
			isc.WindowStack.popup(
				null,
				"Report",
				true,
				[isc.ReportDialog._jasperReportLayout],
				180,
				480,
			);
		}
	},

	/**
	 * Redirects to the appropriate report URL based on the view, parameters, and format.
	 *
	 * @param {string} view - the view name.
	 * @param {Object} params - the parameter map (name -> binding expression with {} if required).
	 * @param {string} format - the format of the report (e.g., 'html', 'pdf').
	 */
	_redirectToReport: function (view, params, format) {
		const instance = view.gather(false); // Don't validate
		if (instance) {
			const { _c: c, _b: b, bizId } = instance;

			let src;
			if (params && params["_n"]) {
				src = `report/${params["_n"]}.${format}?_f=${format}${c ? `&_c=${c}` : ""}${b ? `&_b=${b}` : ""}${bizId ? `&_id=${bizId}` : ""}`;
			} else {
				src = `report/?_f=${format}${c ? `&_c=${c}` : ""}${b ? `&_b=${b}` : ""}${bizId ? `&_id=${bizId}` : ""}`;
			}

			if (params) {
				Object.keys(params).forEach((name) => {
					const binding = params[name];
					// Encode URI and then encode '?' and '&' chars which are not encoded by encodeURI().
					src += `&${name}=${encodeURI(view.toDisplay(binding, instance)).replaceAll("?", "%3F").replaceAll("&", "%26")}`;
				});
			}

			if (format === "html") {
				window.open(
					src,
					"report",
					"location=0,status=0,scrollbars=1,resizable=1,width=800,height=600",
				);
			} else {
				window.location = src;
			}
		}
	},
});
