// Define our extra date and time formats
isc.addMethods(Date.prototype, {
	/**
	 * Formats the date as "HH:MM" in 24-hour format.
	 * @returns {string} - the formatted time string.
	 */
	toHH24_MI: function () {
		return `${this.getHours().stringify()}:${this.getMinutes().stringify()}`;
	},

	/**
	 * Formats the date as "HH:MM AM/PM" in 12-hour format.
	 * @returns {string} - the formatted time string.
	 */
	toHH_MI: function () {
		let hours = this.getHours();
		let ampm = " AM";
		let hoursString = hours.stringify();

		if (hours === 0) {
			hoursString = "12";
		} else if (hours === 12) {
			ampm = " PM";
		} else if (hours > 12) {
			ampm = " PM";
			hours -= 12;
			hoursString = hours.stringify();
		}

		return `${hoursString}:${this.getMinutes().stringify()}${ampm}`;
	},

	/**
	 * Formats the date as "HH:MM:SS" in 24-hour format.
	 * @returns {string} - the formatted time string.
	 */
	toHH24_MI_SS: function () {
		return `${this.toHH24_MI()}:${this.getSeconds().stringify()}`;
	},

	/**
	 * Formats the date as "HH:MM:SS AM/PM" in 12-hour format.
	 * @returns {string} - the formatted time string.
	 */
	toHH_MI_SS: function () {
		const time = this.toHH_MI();
		return `${time.substring(0, time.length - 3)}:${this.getSeconds().stringify()}${time.substring(time.length - 3)}`;
	},

	/**
	 * Formats the date as "DD/MM/YYYY".
	 * @returns {string} - the formatted date string.
	 */
	toDD_MM_YYYY: function () {
		return `${this.getDate().stringify()}/${(this.getMonth() + 1).stringify()}/${this.getFullYear().stringify()}`;
	},

	/**
	 * Formats the date as "DD/MM/YYYY HH:MM" in 24-hour format.
	 * @returns {string} - the formatted date and time string.
	 */
	toDD_MM_YYYY_HH24_MI: function () {
		return `${this.toDD_MM_YYYY()} ${this.toHH24_MI()}`;
	},

	/**
	 * Formats the date as "DD/MM/YYYY HH:MM AM/PM" in 12-hour format.
	 * @returns {string} - the formatted date and time string.
	 */
	toDD_MM_YYYY_HH_MI: function () {
		return `${this.toDD_MM_YYYY()} ${this.toHH_MI()}`;
	},

	/**
	 * Formats the date as "DD/MM/YYYY HH:MM:SS" in 24-hour format.
	 * @returns {string} - the formatted date and time string.
	 */
	toDD_MM_YYYY_HH24_MI_SS: function () {
		return `${this.toDD_MM_YYYY()} ${this.toHH24_MI_SS()}`;
	},

	/**
	 * Formats the date as "DD/MM/YYYY HH:MM:SS AM/PM" in 12-hour format.
	 * @returns {string} - the formatted date and time string.
	 */
	toDD_MM_YYYY_HH_MI_SS: function () {
		return `${this.toDD_MM_YYYY()} ${this.toHH_MI_SS()}`;
	},

	/**
	 * Formats the date as "DD-MMM-YYYY".
	 * @returns {string} - the formatted date string.
	 */
	toDD_MMM_YYYY: function () {
		const shortMonthNames = [
			"Jan",
			"Feb",
			"Mar",
			"Apr",
			"May",
			"Jun",
			"Jul",
			"Aug",
			"Sep",
			"Oct",
			"Nov",
			"Dec",
		];
		return `${this.getDate().stringify()}-${shortMonthNames[this.getMonth()]}-${this.getFullYear().stringify()}`;
	},

	/**
	 * Formats the date as "DD-MMM-YYYY HH:MM" in 24-hour format.
	 * @returns {string} - the formatted date and time string.
	 */
	toDD_MMM_YYYY_HH24_MI: function () {
		return `${this.toDD_MMM_YYYY()} ${this.toHH24_MI()}`;
	},

	/**
	 * Formats the date as "DD-MMM-YYYY HH:MM AM/PM" in 12-hour format.
	 * @returns {string} - the formatted date and time string.
	 */
	toDD_MMM_YYYY_HH_MI: function () {
		return `${this.toDD_MMM_YYYY()} ${this.toHH_MI()}`;
	},

	/**
	 * Formats the date as "DD-MMM-YYYY HH:MM:SS" in 24-hour format.
	 * @returns {string} - the formatted date and time string.
	 */
	toDD_MMM_YYYY_HH24_MI_SS: function () {
		return `${this.toDD_MMM_YYYY()} ${this.toHH24_MI_SS()}`;
	},

	/**
	 * Formats the date as "DD-MMM-YYYY HH:MM:SS AM/PM" in 12-hour format.
	 * @returns {string} - the formatted date and time string.
	 */
	toDD_MMM_YYYY_HH_MI_SS: function () {
		return `${this.toDD_MMM_YYYY()} ${this.toHH_MI_SS()}`;
	},

	/**
	 * Formats the date as "MM/DD/YYYY".
	 * @returns {string} - the formatted date string.
	 */
	toMM_DD_YYYY: function () {
		return `${(this.getMonth() + 1).stringify()}/${this.getDate().stringify()}/${this.getFullYear().stringify()}`;
	},

	/**
	 * Formats the date as "MM/DD/YYYY HH:MM" in 24-hour format.
	 * @returns {string} - the formatted date and time string.
	 */
	toMM_DD_YYYY_HH24_MI: function () {
		return `${this.toMM_DD_YYYY()} ${this.toHH24_MI()}`;
	},

	/**
	 * Formats the date as "MM/DD/YYYY HH:MM AM/PM" in 12-hour format.
	 * @returns {string} - the formatted date and time string.
	 */
	toMM_DD_YYYY_HH_MI: function () {
		return `${this.toMM_DD_YYYY()} ${this.toHH_MI()}`;
	},

	/**
	 * Formats the date as "MM/DD/YYYY HH:MM:SS" in 24-hour format.
	 * @returns {string} - the formatted date and time string.
	 */
	toMM_DD_YYYY_HH24_MI_SS: function () {
		return `${this.toMM_DD_YYYY()} ${this.toHH24_MI_SS()}`;
	},

	/**
	 * Formats the date as "MM/DD/YYYY HH:MM:SS AM/PM" in 12-hour format.
	 * @returns {string} - the formatted date and time string.
	 */
	toMM_DD_YYYY_HH_MI_SS: function () {
		return `${this.toMM_DD_YYYY()} ${this.toHH_MI_SS()}`;
	},

	/**
	 * Formats the date as "MMM-DD-YYYY".
	 * @returns {string} - the formatted date string.
	 */
	toMMM_DD_YYYY: function () {
		const shortMonthNames = [
			"Jan",
			"Feb",
			"Mar",
			"Apr",
			"May",
			"Jun",
			"Jul",
			"Aug",
			"Sep",
			"Oct",
			"Nov",
			"Dec",
		];
		return `${shortMonthNames[this.getMonth()]}-${this.getDate().stringify()}-${this.getFullYear().stringify()}`;
	},

	/**
	 * Formats the date as "MMM-DD-YYYY HH:MM" in 24-hour format.
	 * @returns {string} - the formatted date and time string.
	 */
	toMMM_DD_YYYY_HH24_MI: function () {
		return `${this.toMMM_DD_YYYY()} ${this.toHH24_MI()}`;
	},

	/**
	 * Formats the date as "MMM-DD-YYYY HH:MM AM/PM" in 12-hour format.
	 * @returns {string} - the formatted date and time string.
	 */
	toMMM_DD_YYYY_HH_MI: function () {
		return `${this.toMMM_DD_YYYY()} ${this.toHH_MI()}`;
	},

	/**
	 * Formats the date as "MMM-DD-YYYY HH:MM:SS" in 24-hour format.
	 * @returns {string} - the formatted date and time string.
	 */
	toMMM_DD_YYYY_HH24_MI_SS: function () {
		return `${this.toMMM_DD_YYYY()} ${this.toHH24_MI_SS()}`;
	},

	/**
	 * Formats the date as "MMM-DD-YYYY HH:MM:SS AM/PM" in 12-hour format.
	 * @returns {string} - the formatted date and time string.
	 */
	toMMM_DD_YYYY_HH_MI_SS: function () {
		return `${this.toMMM_DD_YYYY()} ${this.toHH_MI_SS()}`;
	},

	/**
	 * Formats the date as "YYYY/MM/DD".
	 * @returns {string} - the formatted date string.
	 */
	toYYYY_MM_DD: function () {
		return `${this.getFullYear().stringify()}/${(this.getMonth() + 1).stringify()}/${this.getDate().stringify()}`;
	},

	/**
	 * Formats the date as "YYYY/MM/DD HH:MM" in 24-hour format.
	 * @returns {string} - the formatted date and time string.
	 */
	toYYYY_MM_DD_HH24_MI: function () {
		return `${this.toYYYY_MM_DD()} ${this.toHH24_MI()}`;
	},

	/**
	 * Formats the date as "YYYY/MM/DD HH:MM AM/PM" in 12-hour format.
	 * @returns {string} - the formatted date and time string.
	 */
	toYYYY_MM_DD_HH_MI: function () {
		return `${this.toYYYY_MM_DD()} ${this.toHH_MI()}`;
	},

	/**
	 * Formats the date as "YYYY/MM/DD HH:MM:SS" in 24-hour format.
	 * @returns {string} - the formatted date and time string.
	 */
	toYYYY_MM_DD_HH24_MI_SS: function () {
		return `${this.toYYYY_MM_DD()} ${this.toHH24_MI_SS()}`;
	},

	/**
	 * Formats the date as "YYYY/MM/DD HH:MM:SS AM/PM" in 12-hour format.
	 * @returns {string} - the formatted date and time string.
	 */
	toYYYY_MM_DD_HH_MI_SS: function () {
		return `${this.toYYYY_MM_DD()} ${this.toHH_MI_SS()}`;
	},
});

/**
 * Implements the BizDateItem widget.
 * Extends DateItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizDateItem", "DateItem");

isc.BizDateItem.addClassMethods({
	/**
	 * Replaces a month name in a string with its corresponding ordinal number.
	 * @param {string} value - the input string containing the month name.
	 * @param {string} month - the month name to replace.
	 * @returns {string} - the updated string with the month replaced by its ordinal.
	 */
	_replaceMonthWithOrdinal: function (value, month) {
		const lowerMonth = month.toLowerCase();
		const monthMap = {
			jan: "1",
			feb: "2",
			mar: "3",
			apr: "4",
			may: "5",
			jun: "6",
			jul: "7",
			aug: "8",
			sep: "9",
			oct: "10",
			nov: "11",
			dec: "12",
		};

		for (const [key, ordinal] of Object.entries(monthMap)) {
			if (lowerMonth.startsWith(key)) {
				return value.replaceAll(month, ordinal);
			}
		}
		return value;
	},

	/**
	 * Parses an input value into a Date object based on the provided date pattern.
	 * @param {string|Date} value - the input value to parse.
	 * @param {string} datePattern - the date pattern to use for parsing.
	 * @returns {Date|null} - the parsed Date object, or null if parsing fails.
	 */
	parseInput: function (value, datePattern) {
		if (isc.isA.Date(value)) {
			return value;
		}

		if (value) {
			value = value.trim();

			// Handle US date format (MMM-DD-YYYY)
			if (datePattern && datePattern.startsWith("toMMM")) {
				const match = /^([A-Za-z]+)[^A-Za-z0-9]\d?\d.*$/.exec(value); // Month word, 1 or 2 digits, anything
				if (match) {
					value = this._replaceMonthWithOrdinal(value, match[1]);
				}
			} else {
				// Handle other formats (DD-MMM-YYYY)
				const match = /^\d?\d[^A-Za-z0-9]([A-Za-z]+).*$/.exec(value); // 1 or 2 digits, anything, month word
				if (match) {
					value = this._replaceMonthWithOrdinal(value, match[1]);
				}
			}

			// Parse schema date (e.g., 9999-99-99)
			let result = /^\d{4}-\d\d-.*$/.test(value)
				? isc.DateUtil.parseSchemaDate(value)
				: null;

			if (!result) {
				// Parse based on the date pattern
				const format = datePattern?.startsWith("toMM")
					? "MDY"
					: datePattern?.startsWith("toYYYY")
						? "YMD"
						: "DMY";
				result = isc.DateUtil.parseInput(value, format, 50, true);

				// Handle cases where only day and month are provided
				if (!result && /^\d?\d[^A-Za-z0-9]\d?\d$/.test(value)) {
					const currentYear = Date.create().getFullYear().stringify();
					result = isc.DateUtil.parseInput(
						format === "MDY"
							? `${value} ${currentYear}`
							: format === "YMD"
								? `${currentYear} ${value}`
								: `${value} ${currentYear}`,
						format,
						50,
						true,
					);
				}
			}

			return result;
		}
		return null;
	},

	/**
	 * Formats a Date object into a string based on the provided format.
	 * @param {Date|string} value - the value to format (can be a Date or string).
	 * @param {string} format - the format to use for formatting.
	 * @returns {string} - the formatted date string, or the original value if it's not a Date.
	 */
	format: function (value, format) {
		if (!isc.isA.Date(value)) {
			return value;
		}
		return value[format]();
	},
});

isc.BizDateItem.addProperties({
	enforceDate: true, // Whether to validate date input
	changeOnBlur: true, // Whether to perform validation on field blur
	changeOnKeypress: false, // Whether to perform validation on key press
	width: 100,
	useTextField: true, // Whether to use a text field for date input
	showHint: true,
	showHintInField: true,
	hint: "DD MM(M) YY(YY)",
	textFieldProperties: { selectOnFocus: true },
	showPickerTimeItem: false,
	dateFormatter: "toDD_MM_YYYY",

	/**
	 * Parses a date value using the configured date formatter.
	 * @param {string|Date} value - the value to parse.
	 * @returns {Date|null} - the parsed Date object, or null if parsing fails.
	 */
	parseDate: function (value) {
		return isc.BizDateItem.parseInput(value, this.dateFormatter);
	},
});

isc.BizDateItem.addMethods({
	/**
	 * Maps a raw value to a display-friendly string.
	 * @param {string|Date|null} value - the value to map.
	 * @returns {string} - the formatted date string, or an empty string if the value is null.
	 */
	mapValueToDisplay: function (value) {
		if (value === null || value === undefined) {
			return isc.emptyString;
		}
		if (!isc.isA.Date(value)) {
			value = isc.BizDateItem.parseInput(value, this.dateFormatter);
		}
		return isc.BizDateItem.format(value, this.dateFormatter);
	},

	/**
	 * Maps a display-friendly string to a raw value (Date object or null).
	 * @param {string} value - the display value to map.
	 * @returns {Date|null} - the parsed Date object, or null if the input is empty.
	 */
	mapDisplayToValue: function (value) {
		if (isc.isAn.emptyString(value)) {
			value = null;
		} else {
			value = isc.BizDateItem.parseInput(value, this.dateFormatter);
		}
		return value;
	},

	/**
	 * Ensures the value displayed in the element matches the saved value and is formatted correctly.
	 * Overrides the default `updateValue` method.
	 */
	updateValue: function () {
		// Map the value to a valid date
		this.Super("updateValue", arguments);

		// Update the displayed string so it's formatted correctly
		this.setElementValue(this.mapValueToDisplay(this.getValue()));
	},

	/**
	 * Sets the value of the date item. If the input is a string, it is parsed into a Date object.
	 * Overrides the default `setValue` method.
	 * @param {string|Date} newValue - the new value to set.
	 * @returns {Object} - the instance of the date item.
	 */
	setValue: function (newValue) {
		if (typeof newValue === "string") {
			newValue = isc.BizDateItem.parseInput(newValue, this.dateFormatter);
		}
		return this.Super("setValue", [newValue]);
	},
});

isc.SimpleType.create({
	name: "bizDate",
	inheritsFrom: "date",
	editorType: "BizDateItem",

	/**
	 * Formats the internal date value for editing.
	 * @param {Date} internalValue - the internal date value.
	 * @param {Object} field - the field configuration.
	 * @param {Object} form - the form containing the field.
	 * @param {Object} record - the record containing the field value.
	 * @returns {string} - the formatted date string.
	 */
	editFormatter: function (internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},

	/**
	 * Formats the internal date value for normal display.
	 * @param {Date} internalValue - the internal date value.
	 * @param {Object} field - the field configuration.
	 * @param {Object} component - the component containing the field.
	 * @param {Object} record - the record containing the field value.
	 * @returns {string} - the formatted date string.
	 */
	normalDisplayFormatter: function (internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},

	/**
	 * Formats the internal date value for short display.
	 * @param {Date} internalValue - the internal date value.
	 * @param {Object} field - the field configuration.
	 * @param {Object} component - the component containing the field.
	 * @param {Object} record - the record containing the field value.
	 * @returns {string} - the formatted date string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toDD_MM_YYYY");
	},
});

isc.SimpleType.create({
	name: "DD_MM_YYYY",
	inheritsFrom: "bizDate",
});

/**
 * Registeres the DD_MMM_YYYY_Item.
 * Extends BizDateItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("DD_MMM_YYYY_Item", "BizDateItem");

isc.DD_MMM_YYYY_Item.addProperties({
	dateFormatter: "toDD_MMM_YYYY",
});

isc.SimpleType.create({
	name: "DD_MMM_YYYY",
	inheritsFrom: "bizDate",
	editorType: "DD_MMM_YYYY_Item",

	/**
	 * Formats the internal date value for short display in the "DD MMM YYYY" format.
	 * @param {Date} internalValue - the internal date value.
	 * @param {Object} field - the field configuration.
	 * @param {Object} component - the component containing the field.
	 * @param {Object} record - the record containing the field value.
	 * @returns {string} - the formatted date string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toDD_MMM_YYYY");
	},
});

/**
 * Registers MM_DD_YYYY_Item.
 * Extends BizDateItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("MM_DD_YYYY_Item", "BizDateItem");

isc.MM_DD_YYYY_Item.addProperties({
	hint: "MM(M) DD YY(YY)",
	dateFormatter: "toMM_DD_YYYY",
});

isc.SimpleType.create({
	name: "MM_DD_YYYY",
	inheritsFrom: "bizDate",
	editorType: "MM_DD_YYYY_Item",

	/**
	 * Formats the internal date value for short display in the "MM DD YYYY" format.
	 * @param {Date} internalValue - the internal date value.
	 * @param {Object} field - the field configuration.
	 * @param {Object} component - the component containing the field.
	 * @param {Object} record - the record containing the field value.
	 * @returns {string} - the formatted date string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toMM_DD_YYYY");
	},
});

/**
 * Registers MMM_DD_YYYY_Item.
 * Extends BizDateItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("MMM_DD_YYYY_Item", "BizDateItem");

isc.MMM_DD_YYYY_Item.addProperties({
	hint: "MM(M) DD YY(YY)",
	dateFormatter: "toMMM_DD_YYYY",
});

isc.SimpleType.create({
	name: "MMM_DD_YYYY",
	inheritsFrom: "bizDate",
	editorType: "MMM_DD_YYYY_Item",

	/**
	 * Formats the internal date value for short display in the "MMM DD YYYY" format.
	 * @param {Date} internalValue - the internal date value.
	 * @param {Object} field - the field configuration.
	 * @param {Object} component - the component containing the field.
	 * @param {Object} record - the record containing the field value.
	 * @returns {string} - the formatted date string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toMMM_DD_YYYY");
	},
});

/**
 * Registers YYYY_MM_DD_Item.
 * Extends BizDateItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("YYYY_MM_DD_Item", "BizDateItem");

isc.YYYY_MM_DD_Item.addProperties({
	hint: "YY(YY) MM(M) DD",
	dateFormatter: "toYYYY_MM_DD",
});

isc.SimpleType.create({
	name: "YYYY_MM_DD",
	inheritsFrom: "bizDate",
	editorType: "YYYY_MM_DD_Item",

	/**
	 * Formats the internal date value for short display in the "YYYY-MM-DD" format.
	 * @param {Date} internalValue - the internal date value.
	 * @param {Object} field - the field configuration.
	 * @param {Object} component - the component containing the field.
	 * @param {Object} record - the record containing the field value.
	 * @returns {string} - the formatted date string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toYYYY_MM_DD");
	},
});

/**
 * Implements the BizDateTimeItem widget.
 * Extends BizTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizDateTimeItem", "DateTimeItem");

isc.BizDateTimeItem.addProperties({
	enforceDate: true,
	changeOnBlur: true, // Perform validation on field blur
	changeOnKeypress: false, // Don't perform validation on key press
	width: 200,
	useTextField: true,
	showHint: true,
	showHintInField: true,
	hint: "DD MM(M) YY(YY) HH:MI AM/PM",
	textFieldProperties: { selectOnFocus: true },
	showPickerTimeItem: true,
	pickerTimeItemProperties: { showSecondItem: false, use24HourTime: false },
	dateFormatter: "toDD_MM_YYYY_HH_MI",

	/**
	 * Parses a date string input using the specified date formatter.
	 * @param {string} value - the date string to be parsed.
	 * @returns {Date|null} - the parsed date object or null if parsing fails.
	 */
	parseDate: function (value) {
		return isc.BizDateItem.parseInput(value, this.dateFormatter);
	},
});

isc.BizDateTimeItem.addMethods({
	/**
	 * Maps a value to its display representation.
	 * If the value is null, returns an empty string.
	 * If the value is not a Date, it attempts to parse it using the configured date formatter.
	 *
	 * @param {Date|string|null} value - the value to be mapped.
	 * @returns {string} - the formatted display string.
	 */
	mapValueToDisplay: function (value) {
		if (value == null) {
			return isc.emptyString;
		}
		if (!isc.isA.Date(value)) {
			value = isc.BizDateItem.parseInput(value, this.dateFormatter);
		}
		return isc.BizDateItem.format(value, this.dateFormatter);
	},

	/**
	 * Maps a display string to its corresponding value.
	 * If the input is an empty string, it returns null.
	 * Otherwise, it parses the input string into a Date object.
	 *
	 * @param {string} value - the display string to be converted.
	 * @returns {Date|null} - the parsed Date object or null if the input was empty.
	 */
	mapDisplayToValue: function (value) {
		if (isc.isAn.emptyString(value)) {
			return null;
		}
		return isc.BizDateItem.parseInput(value, this.dateFormatter);
	},

	/**
	 * Ensures the displayed value in the element matches the saved value and is correctly formatted.
	 * Overrides the default `updateValue` method.
	 */
	updateValue: function () {
		// Call the superclass implementation to ensure value is mapped to a valid float
		this.Super("updateValue", arguments);
		// Update the displayed string with the correctly formatted value
		this.setElementValue(this.mapValueToDisplay(this.getValue()));
	},

	/**
	 * Sets a new value, mapping strings to the appropriate date before saving.
	 * This ensures that string inputs are parsed properly since they won't go through `mapDisplayToValue`.
	 * Overrides the default `setValue` method.
	 *
	 * @param {string|Date|null} newValue - the new value to be set.
	 * @returns {*} - the result of calling the superclass `setValue` method.
	 */
	setValue: function (newValue) {
		if (isc.isA.String(newValue)) {
			newValue = isc.BizDateItem.parseInput(newValue, this.dateFormatter);
		}
		return this.Super("setValue", [newValue]);
	},
});

isc.SimpleType.create({
	name: "bizDateTime",
	inheritsFrom: "dateTime",
	editorType: "BizDateTimeItem",

	/**
	 * Formats the internal value for editing.
	 * Delegates to `shortDisplayFormatter` to ensure consistent formatting.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} form - the form containing the field.
	 * @param {Object} record - the record being edited.
	 * @returns {string} - the formatted value for editing.
	 */
	editFormatter: function (internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},

	/**
	 * Formats the internal value for normal display.
	 * Delegates to `shortDisplayFormatter` to ensure consistent formatting.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component displaying the value.
	 * @param {Object} record - the record associated with the value.
	 * @returns {string} - the formatted value for display.
	 */
	normalDisplayFormatter: function (internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},

	/**
	 * Formats the internal value using the `toDD_MM_YYYY_HH_MI` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component displaying the value.
	 * @param {Object} record - the record associated with the value.
	 * @returns {string} - the formatted string in `DD_MM_YYYY_HH_MI` format.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toDD_MM_YYYY_HH_MI");
	},
});

isc.SimpleType.create({
	name: "DD_MM_YYYY_HH_MI",
	inheritsFrom: "bizDateTime",
});

/**
 * Registers DD_MM_YYYY_HH24_MI_Item.
 * Extends BizDateTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("DD_MM_YYYY_HH24_MI_Item", "BizDateTimeItem");

isc.DD_MM_YYYY_HH24_MI_Item.addProperties({
	dateFormatter: "toDD_MM_YYYY_HH24_MI",
	hint: "DD MM(M) YY(YY) HH(24):MI",
	pickerTimeItemProperties: { showSecondItem: false, use24HourTime: true },
});

isc.SimpleType.create({
	name: "DD_MM_YYYY_HH24_MI",
	inheritsFrom: "bizDateTime",
	editorType: "DD_MM_YYYY_HH24_MI_Item",

	/**
	 * Formats the internal value using the `toDD_MM_YYYY_HH24_MI` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component displaying the value.
	 * @param {Object} record - the record associated with the value.
	 * @returns {string} - the formatted string in `DD_MM_YYYY_HH24_MI` format.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toDD_MM_YYYY_HH24_MI");
	},
});

/**
 * Registers DD_MM_YYYY_HH_MI_SS_Item.
 * Extends BizDateTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("DD_MM_YYYY_HH_MI_SS_Item", "BizDateTimeItem");
isc.DD_MM_YYYY_HH_MI_SS_Item.addProperties({
	dateFormatter: "toDD_MM_YYYY_HH_MI_SS",
	hint: "DD MM(M) YY(YY) HH:MI(:SS) AM/PM",
	pickerTimeItemProperties: { showSecondItem: true, use24HourTime: false },
});

isc.SimpleType.create({
	name: "DD_MM_YYYY_HH_MI_SS",
	inheritsFrom: "bizDateTime",
	editorType: "DD_MM_YYYY_HH_MI_SS_Item",

	/**
	 * Formats the internal value using the `toDD_MM_YYYY_HH_MI_SS` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component displaying the value.
	 * @param {Object} record - the record associated with the value.
	 * @returns {string} - the formatted string in `DD_MM_YYYY_HH_MI_SS` format.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toDD_MM_YYYY_HH_MI_SS");
	},
});

/**
 * Registers DD_MM_YYYY_HH24_MI_SS_Item.
 * Extends BizDateTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("DD_MM_YYYY_HH24_MI_SS_Item", "BizDateTimeItem");

isc.DD_MM_YYYY_HH24_MI_SS_Item.addProperties({
	dateFormatter: "toDD_MM_YYYY_HH24_MI_SS",
	hint: "DD MM(M) YY(YY) HH(24):MI(:SS)",
	pickerTimeItemProperties: { showSecondItem: true, use24HourTime: true },
});

isc.SimpleType.create({
	name: "DD_MM_YYYY_HH24_MI_SS",
	inheritsFrom: "bizDateTime",
	editorType: "DD_MM_YYYY_HH24_MI_SS_Item",

	/**
	 * Formats the internal date-time value into the `DD_MM_YYYY_HH24_MI_SS` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component instance.
	 * @param {Object} record - the record object containing the value.
	 * @returns {string} The formatted date-time string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toDD_MM_YYYY_HH24_MI_SS");
	},
});

/**
 * Registers DD_MMM_YYYY_HH_MI_Item.
 * Extends BizDateTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("DD_MMM_YYYY_HH_MI_Item", "BizDateTimeItem");

isc.DD_MMM_YYYY_HH_MI_Item.addProperties({
	dateFormatter: "toDD_MMM_YYYY_HH_MI",
	hint: "DD MM(M) YY(YY) HH:MI AM/PM",
	pickerTimeItemProperties: { showSecondItem: false, use24HourTime: false },
});

isc.SimpleType.create({
	name: "DD_MMM_YYYY_HH_MI",
	inheritsFrom: "bizDateTime",
	editorType: "DD_MMM_YYYY_HH_MI_Item",

	/**
	 * Formats the internal date-time value into the `DD_MMM_YYYY_HH_MI` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component instance.
	 * @param {Object} record - the record object containing the value.
	 * @returns {string} The formatted date-time string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toDD_MMM_YYYY_HH_MI");
	},
});

/**
 * Registers DD_MMM_YYYY_HH24_MI_Item.
 * Extends BizDateTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("DD_MMM_YYYY_HH24_MI_Item", "BizDateTimeItem");

isc.DD_MMM_YYYY_HH24_MI_Item.addProperties({
	dateFormatter: "toDD_MMM_YYYY_HH24_MI",
	hint: "DD MM(M) YY(YY) HH(24):MI",
	pickerTimeItemProperties: { showSecondItem: false, use24HourTime: true },
});

isc.SimpleType.create({
	name: "DD_MMM_YYYY_HH24_MI",
	inheritsFrom: "bizDateTime",
	editorType: "DD_MMM_YYYY_HH24_MI_Item",

	/**
	 * Formats the internal date-time value into the `DD_MMM_YYYY_HH24_MI` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component instance.
	 * @param {Object} record - the record object containing the value.
	 * @returns {string} The formatted date-time string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toDD_MMM_YYYY_HH24_MI");
	},
});

/**
 * Registers DD_MMM_YYYY_HH_MI_SS_Item.
 * Extends BizDateTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("DD_MMM_YYYY_HH_MI_SS_Item", "BizDateTimeItem");

isc.DD_MMM_YYYY_HH_MI_SS_Item.addProperties({
	dateFormatter: "toDD_MMM_YYYY_HH_MI_SS",
	hint: "DD MM(M) YY(YY) HH:MI(:SS) AM/PM",
	pickerTimeItemProperties: { showSecondItem: true, use24HourTime: false },
});

isc.SimpleType.create({
	name: "DD_MMM_YYYY_HH_MI_SS",
	inheritsFrom: "bizDateTime",
	editorType: "DD_MMM_YYYY_HH_MI_SS_Item",

	/**
	 * Formats the internal date-time value into the `DD_MMM_YYYY_HH_MI_SS` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component instance.
	 * @param {Object} record - the record object containing the value.
	 * @returns {string} The formatted date-time string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toDD_MMM_YYYY_HH_MI_SS");
	},
});

/**
 * Registers DD_MMM_YYYY_HH24_MI_SS_Item.
 * Extends BizDateTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("DD_MMM_YYYY_HH24_MI_SS_Item", "BizDateTimeItem");

isc.DD_MMM_YYYY_HH24_MI_SS_Item.addProperties({
	dateFormatter: "toDD_MMM_YYYY_HH24_MI_SS",
	hint: "DD MM(M) YY(YY) HH(24):MI(:SS)",
	pickerTimeItemProperties: { showSecondItem: true, use24HourTime: true },
});

isc.SimpleType.create({
	name: "DD_MMM_YYYY_HH24_MI_SS",
	inheritsFrom: "bizDateTime",
	editorType: "DD_MMM_YYYY_HH24_MI_SS_Item",

	/**
	 * Formats the internal date-time value into the `DD_MMM_YYYY_HH24_MI_SS` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component instance.
	 * @param {Object} record - the record object containing the value.
	 * @returns {string} The formatted date-time string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toDD_MMM_YYYY_HH24_MI_SS");
	},
});

/**
 * Registers MM_DD_YYYY_HH_MI_Item.
 * Extends BizDateTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("MM_DD_YYYY_HH_MI_Item", "BizDateTimeItem");

isc.MM_DD_YYYY_HH_MI_Item.addProperties({
	dateFormatter: "toMM_DD_YYYY_HH_MI",
	hint: "MM(M) DD YY(YY) HH:MI AM/PM",
	pickerTimeItemProperties: { showSecondItem: false, use24HourTime: false },
});

isc.SimpleType.create({
	name: "MM_DD_YYYY_HH_MI",
	inheritsFrom: "bizDateTime",
	editorType: "MM_DD_YYYY_HH_MI_Item",

	/**
	 * Formats the internal date-time value into the `MM_DD_YYYY_HH_MI` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component instance.
	 * @param {Object} record - the record object containing the value.
	 * @returns {string} The formatted date-time string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toMM_DD_YYYY_HH_MI");
	},
});

/**
 * Registers MM_DD_YYYY_HH24_MI_Item.
 * Extends BizDateTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("MM_DD_YYYY_HH24_MI_Item", "BizDateTimeItem");

isc.MM_DD_YYYY_HH24_MI_Item.addProperties({
	dateFormatter: "toMM_DD_YYYY_HH24_MI",
	hint: "MM(M) DD YY(YY) HH(24):MI",
	pickerTimeItemProperties: { showSecondItem: false, use24HourTime: true },
});

isc.SimpleType.create({
	name: "MM_DD_YYYY_HH24_MI",
	inheritsFrom: "bizDateTime",
	editorType: "MM_DD_YYYY_HH24_MI_Item",

	/**
	 * Formats the internal date-time value into the `MM_DD_YYYY_HH24_MI` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component instance.
	 * @param {Object} record - the record object containing the value.
	 * @returns {string} The formatted date-time string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toMM_DD_YYYY_HH24_MI");
	},
});

/**
 * Registers MM_DD_YYYY_HH_MI_SS_Item.
 * Extends BizDateTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("MM_DD_YYYY_HH_MI_SS_Item", "BizDateTimeItem");

isc.MM_DD_YYYY_HH_MI_SS_Item.addProperties({
	dateFormatter: "toMM_DD_YYYY_HH_MI_SS",
	hint: "MM(M) DD YY(YY) HH:MI(:SS) AM/PM",
	pickerTimeItemProperties: { showSecondItem: true, use24HourTime: false },
});

isc.SimpleType.create({
	name: "MM_DD_YYYY_HH_MI_SS",
	inheritsFrom: "bizDateTime",
	editorType: "MM_DD_YYYY_HH_MI_SS_Item",

	/**
	 * Formats the internal date-time value into the `MM_DD_YYYY_HH_MI_SS` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component instance.
	 * @param {Object} record - the record object containing the value.
	 * @returns {string} The formatted date-time string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toMM_DD_YYYY_HH_MI_SS");
	},
});

/**
 * Registers MM_DD_YYYY_HH24_MI_SS_Item.
 * Extends BizDateTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("MM_DD_YYYY_HH24_MI_SS_Item", "BizDateTimeItem");

isc.MM_DD_YYYY_HH24_MI_SS_Item.addProperties({
	hint: "MM(M) DD YY(YY) HH(24):MI(:SS)",
	dateFormatter: "toMM_DD_YYYY_HH24_MI_SS",
	pickerTimeItemProperties: { showSecondItem: true, use24HourTime: true },
});

isc.SimpleType.create({
	name: "MM_DD_YYYY_HH24_MI_SS",
	inheritsFrom: "bizDateTime",
	editorType: "MM_DD_YYYY_HH24_MI_SS_Item",

	/**
	 * Formats the internal date-time value into the `MM_DD_YYYY_HH24_MI_SS` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component instance.
	 * @param {Object} record - the record object containing the value.
	 * @returns {string} The formatted date-time string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toMM_DD_YYYY_HH24_MI_SS");
	},
});

/**
 * Registers MMM_DD_YYYY_HH_MI_Item.
 * Extends BizDateTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("MMM_DD_YYYY_HH_MI_Item", "BizDateTimeItem");

isc.MMM_DD_YYYY_HH_MI_Item.addProperties({
	dateFormatter: "toMMM_DD_YYYY_HH_MI",
	hint: "MM(M) DD YY(YY) HH:MI AM/PM",
	pickerTimeItemProperties: { showSecondItem: false, use24HourTime: false },
});

isc.SimpleType.create({
	name: "MMM_DD_YYYY_HH_MI",
	inheritsFrom: "bizDateTime",
	editorType: "MMM_DD_YYYY_HH_MI_Item",

	/**
	 * Formats the internal date-time value into the `MMM_DD_YYYY_HH_MI` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component instance.
	 * @param {Object} record - the record object containing the value.
	 * @returns {string} The formatted date-time string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toMMM_DD_YYYY_HH_MI");
	},
});

/**
 * Registers MMM_DD_YYYY_HH24_MI_Item.
 * Extends BizDateTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("MMM_DD_YYYY_HH24_MI_Item", "BizDateTimeItem");

isc.MMM_DD_YYYY_HH24_MI_Item.addProperties({
	dateFormatter: "toMMM_DD_YYYY_HH24_MI",
	hint: "MM(M) DD YY(YY) HH(24):MI",
	pickerTimeItemProperties: { showSecondItem: false, use24HourTime: true },
});

isc.SimpleType.create({
	name: "MMM_DD_YYYY_HH24_MI",
	inheritsFrom: "bizDateTime",
	editorType: "MMM_DD_YYYY_HH24_MI_Item",

	/**
	 * Formats the internal date-time value into the `MMM_DD_YYYY_HH24_MI` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component instance.
	 * @param {Object} record - the record object containing the value.
	 * @returns {string} The formatted date-time string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toMMM_DD_YYYY_HH24_MI");
	},
});

/**
 * Registers MMM_DD_YYYY_HH_MI_SS_Item.
 * Extends BizDateTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("MMM_DD_YYYY_HH_MI_SS_Item", "BizDateTimeItem");

isc.MMM_DD_YYYY_HH_MI_SS_Item.addProperties({
	dateFormatter: "toMMM_DD_YYYY_HH_MI_SS",
	hint: "MM(M) DD YY(YY) HH(:MI(:SS) AM/PM",
	pickerTimeItemProperties: { showSecondItem: true, use24HourTime: false },
});

isc.SimpleType.create({
	name: "MMM_DD_YYYY_HH_MI_SS",
	inheritsFrom: "bizDateTime",
	editorType: "MMM_DD_YYYY_HH_MI_SS_Item",

	/**
	 * Formats the internal date-time value into the `MMM_DD_YYYY_HH_MI_SS` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component instance.
	 * @param {Object} record - the record object containing the value.
	 * @returns {string} The formatted date-time string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toMMM_DD_YYYY_HH_MI_SS");
	},
});

/**
 * Registers MMM_DD_YYYY_HH24_MI_SS_Item.
 * Extends BizDateTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("MMM_DD_YYYY_HH24_MI_SS_Item", "BizDateTimeItem");

isc.MMM_DD_YYYY_HH24_MI_SS_Item.addProperties({
	hint: "MM(M) DD YY(YY) HH(24):MI(:SS)",
	dateFormatter: "toMMM_DD_YYYY_HH24_MI_SS",
	pickerTimeItemProperties: { showSecondItem: true, use24HourTime: true },
});

isc.SimpleType.create({
	name: "MMM_DD_YYYY_HH24_MI_SS",
	inheritsFrom: "bizDateTime",
	editorType: "MMM_DD_YYYY_HH24_MI_SS_Item",

	/**
	 * Formats the internal date-time value into the `MMM_DD_YYYY_HH24_MI_SS` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component instance.
	 * @param {Object} record - the record object containing the value.
	 * @returns {string} The formatted date-time string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toMMM_DD_YYYY_HH24_MI_SS");
	},
});

/**
 * Registers YYYY_MM_DD_HH_MI_Item.
 * Extends BizDateTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("YYYY_MM_DD_HH_MI_Item", "BizDateTimeItem");

isc.YYYY_MM_DD_HH_MI_Item.addProperties({
	dateFormatter: "toYYYY_MM_DD_HH_MI",
	hint: "YY(YY) MM(M) DD HH:MI AM/PM",
	pickerTimeItemProperties: { showSecondItem: false, use24HourTime: false },
});

isc.SimpleType.create({
	name: "YYYY_MM_DD_HH_MI",
	inheritsFrom: "bizDateTime",
	editorType: "YYYY_MM_DD_HH_MI_Item",

	/**
	 * Formats the internal date-time value into the `YYYY_MM_DD_HH_MI` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component instance.
	 * @param {Object} record - the record object containing the value.
	 * @returns {string} The formatted date-time string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toYYYY_MM_DD_HH_MI");
	},
});

/**
 * Registers YYYY_MM_DD_HH24_MI_Item.
 * Extends BizDateTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("YYYY_MM_DD_HH24_MI_Item", "BizDateTimeItem");

isc.YYYY_MM_DD_HH24_MI_Item.addProperties({
	dateFormatter: "toYYYY_MM_DD_HH24_MI",
	hint: "YY(YY) MM(M) DD HH(24):MI",
	pickerTimeItemProperties: { showSecondItem: false, use24HourTime: true },
});

isc.SimpleType.create({
	name: "YYYY_MM_DD_HH24_MI",
	inheritsFrom: "bizDateTime",
	editorType: "YYYY_MM_DD_HH24_MI_Item",

	/**
	 * Formats the internal date-time value into the `YYYY_MM_DD_HH24_MI` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component instance.
	 * @param {Object} record - the record object containing the value.
	 * @returns {string} The formatted date-time string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toYYYY_MM_DD_HH24_MI");
	},
});

/**
 * Registers YYYY_MM_DD_HH_MI_SS_Item.
 * Extends BizDateTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("YYYY_MM_DD_HH_MI_SS_Item", "BizDateTimeItem");

isc.YYYY_MM_DD_HH_MI_SS_Item.addProperties({
	dateFormatter: "toYYYY_MM_DD_HH_MI_SS",
	hint: "YY(YY) MM(M) DD HH:MI(:SS) AM/PM",
	pickerTimeItemProperties: { showSecondItem: true, use24HourTime: false },
});

isc.SimpleType.create({
	name: "YYYY_MM_DD_HH_MI_SS",
	inheritsFrom: "bizDateTime",
	editorType: "YYYY_MM_DD_HH_MI_SS_Item",

	/**
	 * Formats the internal date-time value into the `YYYY_MM_DD_HH_MI_SS` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component instance.
	 * @param {Object} record - the record object containing the value.
	 * @returns {string} The formatted date-time string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toYYYY_MM_DD_HH_MI_SS");
	},
});

/**
 * Registers YYYY_MM_DD_HH24_MI_SS_Item.
 * Extends BizDateTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("YYYY_MM_DD_HH24_MI_SS_Item", "BizDateTimeItem");

isc.YYYY_MM_DD_HH24_MI_SS_Item.addProperties({
	hint: "YY(YY) MM(M) DD HH(24):MI(:SS)",
	dateFormatter: "toYYYY_MM_DD_HH24_MI_SS",
	pickerTimeItemProperties: { showSecondItem: true, use24HourTime: true },
});

isc.SimpleType.create({
	name: "YYYY_MM_DD_HH24_MI_SS",
	inheritsFrom: "bizDateTime",
	editorType: "YYYY_MM_DD_HH24_MI_SS_Item",

	/** Formats the internal date-time value into the `YYYY_MM_DD_HH24_MI_SS` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component instance.
	 * @param {Object} record - the record object containing the value.
	 * @returns {string} The formatted date-time string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, "toYYYY_MM_DD_HH24_MI_SS");
	},
});

/**
 * Overrides the `init` method of `isc.TimeItem` to prevent setting the time formatter.
 * This ensures that the time formatter is not modified after the component is constructed.
 */
isc.TimeItem.addMethods({
	init: function () {
		this.Super("init", arguments);
	},
});

/**
 * Implements the BizTimeItem widget.
 * Extends TimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizTimeItem", "TimeItem");

isc.BizTimeItem.addProperties({
	useTextField: true,
	use24HourTime: false,
	showHint: true,
	showHintInField: true,
	hint: "HH:MI am/pm",
	textFieldProperties: { selectOnFocus: true },
	displayFormat: "toShortTime",
	timeFormatter: "toShortTime",
});

isc.BizTimeItem.addMethods({
	/**
	 * Sets the value of the `BizTimeItem` component.
	 * If the new value is a string, it attempts to parse it into a valid time.
	 *
	 * @param {string|Date|null} newValue - the value to be set. If a string, it will be parsed into a Date object.
	 * @returns {Date|null} The parsed time value or null if invalid.
	 * @override
	 */
	setValue: function (newValue) {
		if (isc.isA.String(newValue)) {
			newValue = isc.Time.parseInput(newValue);
		}
		return this.Super("setValue", [newValue]);
	},

	/**
	 * Determines whether the component has advanced criteria.
	 * Always returns `false` for `BizTimeItem`, as it does not support advanced filtering.
	 *
	 * @returns {boolean} `false`, indicating no advanced criteria support.
	 * @override
	 */
	hasAdvancedCriteria: function () {
		return false;
	},
});

isc.SimpleType.create({
	name: "bizTime",
	inheritsFrom: "time",
	editorType: "BizTimeItem",

	/**
	 * Formats the internal value for editing.
	 * Delegates to `shortDisplayFormatter` to ensure consistency.
	 *
	 * @param {Date|null} internalValue - the raw time value.
	 * @param {object} field - the field definition.
	 * @param {object} form - the form containing the field.
	 * @param {object} record - the record containing the value.
	 * @returns {string} The formatted time string.
	 */
	editFormatter: function (internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},

	/**
	 * Formats the internal value for normal display.
	 * Delegates to `shortDisplayFormatter` to ensure consistency.
	 *
	 * @param {Date|null} internalValue - the raw time value.
	 * @param {object} field - the field definition.
	 * @param {object} component - the UI component displaying the value.
	 * @param {object} record - the record containing the value.
	 * @returns {string} The formatted time string.
	 */
	normalDisplayFormatter: function (internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},

	/**
	 * Converts a raw time value into a short time format string.
	 *
	 * @param {Date|null} internalValue - the time value to format.
	 * @param {object} field - the field definition.
	 * @param {object} component - the UI component displaying the value.
	 * @param {object} record - the record containing the value.
	 * @returns {string} The formatted time string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.Time.toShortTime(internalValue, "toShortTime", true);
	},
});

isc.SimpleType.create({
	name: "HH_MI",
	inheritsFrom: "bizTime",
});

/**
 * Registers HH_MI_SS_Item.
 * Extends BizTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("HH_MI_SS_Item", "BizTimeItem");

isc.HH_MI_SS_Item.addProperties({
	use24HourTime: false,
	hint: "HH:MI:SS am/pm",
	dateFormatter: "toTime",
	timeFormatter: "toTime",
});

isc.SimpleType.create({
	name: "HH_MI_SS",
	inheritsFrom: "bizTime",
	editorType: "HH_MI_SS_Item",

	/** Formats the internal time value into the `HH_MI_SS` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component instance.
	 * @param {Object} record - the record object containing the value.
	 * @returns {string} The formatted time string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.Time.toTime(internalValue, "toTime", true);
	},
});

/**
 * Registers HH24_MI_Item.
 * Extends BizTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("HH24_MI_Item", "BizTimeItem");

isc.HH24_MI_Item.addProperties({
	use24HourTime: true,
	hint: "HH24:MI",
	dateFormatter: "toShortPadded24HourTime",
	timeFormatter: "toShortPadded24HourTime",
});

isc.SimpleType.create({
	name: "HH24_MI",
	inheritsFrom: "bizTime",
	editorType: "HH24_MI_Item",

	/** Formats the internal time value into the `HH24` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component instance.
	 * @param {Object} record - the record object containing the value.
	 * @returns {string} The formatted time string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.Time.toShortTime(internalValue, "toShortPadded24HourTime", true);
	},
});

/**
 * Registers HH24_MI_SS_Item.
 * Extends BizTimeItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("HH24_MI_SS_Item", "BizTimeItem");

isc.HH24_MI_SS_Item.addProperties({
	use24HourTime: true,
	hint: "HH24:MI:SS",
	dateFormatter: "toPadded24HourTime",
	timeFormatter: "toPadded24HourTime",
});

isc.SimpleType.create({
	name: "HH24_MI_SS",
	inheritsFrom: "bizTime",
	editorType: "HH24_MI_SS_Item",

	/** Formats the internal time value into the `HH24_MI_SS` format.
	 *
	 * @param {Date|string|null} internalValue - the value to be formatted.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the component instance.
	 * @param {Object} record - the record object containing the value.
	 * @returns {string} The formatted time string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.Time.toTime(internalValue, "toPadded24HourTime", true);
	},
});

/**
 * Implements the BizDecimal2Item widget.
 * Extends TextItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizDecimal2Item", "TextItem");

isc.BizDecimal2Item.addClassMethods({
	/**
	 * Parses the input value into a decimal number.
	 *
	 * @param {string|number|null} value - the input value, which can be a number or a string.
	 * @returns {number|string} - the parsed float value, or an empty string if parsing fails.
	 */
	parseInput: function (value) {
		if (isc.isA.Number(value)) {
			return value;
		}

		if (value) {
			value = value.toString().trim().replace(/\$|\,/g, "");

			if (isNaN(value)) {
				return "";
			}

			const result = parseFloat(value);

			// Ensure the parsed number is within a reasonable range
			return result >= 1e18 ? "" : result;
		}

		return "";
	},

	/**
	 * Formats a given number to a localized string with the specified number of decimal places.
	 *
	 * @param {number|string|null} value - the value to format. If not a number, it is returned as is.
	 * @param {number} decimalPlaces - the number of decimal places to format to.
	 * @returns {string} - the formatted number as a localized string, or the original value if it's not a number.
	 */
	format: function (value, decimalPlaces) {
		if (!isc.isA.Number(value)) {
			return value;
		}

		return value.toLocalizedString(decimalPlaces, ".", ",", "-");
	},
});

isc.BizDecimal2Item.addProperties({
	changeOnBlur: true, // Perform validation on field blur
	changeOnKeypress: false, // Don't perform validation on key press
	width: 100,
	showHint: false,
	selectOnFocus: true,
	decimalPlaces: 2,
});

isc.BizDecimal2Item.addMethods({
	/**
	 * Maps a numeric value to a properly formatted display string.
	 *
	 * @param {number|string|null} value - the value to format for display.
	 * @returns {string} - a formatted string representation of the value, or an empty string if null.
	 */
	mapValueToDisplay: function (value) {
		if (value == null) {
			return isc.emptyString;
		}
		if (!isc.isA.Number(value)) {
			value = isc.BizDecimal2Item.parseInput(value);
		}
		return isc.BizDecimal2Item.format(value, this.decimalPlaces);
	},

	/**
	 * Maps a user-inputted display string back to a numeric value.
	 *
	 * @param {string|null} value - the user-inputted value from the display.
	 * @returns {number|null} - the parsed numeric value, or null if the input is empty.
	 */
	mapDisplayToValue: function (value) {
		if (isc.isAn.emptyString(value)) {
			return null;
		}
		return isc.BizDecimal2Item.parseInput(value);
	},

	/**
	 * Ensures the displayed value is correctly formatted after an update.
	 * Overrides the default `updateValue` method.
	 */
	updateValue: function () {
		// Map the current value to a valid float and update
		this.Super("updateValue", arguments);
		// Update the displayed string to reflect the correctly formatted value
		this.setElementValue(this.mapValueToDisplay(this.getValue()));
	},

	/**
	 * Sets the value of the item, ensuring it is correctly parsed as a float.
	 * If passed a string, it maps it to the appropriate float before saving.
	 *
	 * @param {string|number|null} newValue - the new value to set.
	 * @returns {any} - the result of the superclass's `setValue` method.
	 */
	setValue: function (newValue) {
		if (isc.isA.String(newValue)) {
			newValue = isc.BizDecimal2Item.parseInput(newValue);
		}
		return this.Super("setValue", [newValue]);
	},
});

isc.SimpleType.create({
	name: "bizDecimal2",
	inheritsFrom: "float",
	editorType: "BizDecimal2Item",

	/**
	 * Formats the internal value for editing in the UI.
	 *
	 * @param {number|null} internalValue - the raw value stored in the system.
	 * @param {Object} field - the field definition.
	 * @param {Object} form - the form containing this field.
	 * @param {Object} record - the record containing this field.
	 * @returns {string} - the formatted value for display in an editable field.
	 */
	editFormatter: function (internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},

	/**
	 * Formats the internal value for normal (read-only) display in the UI.
	 *
	 * @param {number|null} internalValue - the raw value stored in the system.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the UI component displaying the field.
	 * @param {Object} record - the record containing this field.
	 * @returns {string} - the formatted value for display.
	 */
	normalDisplayFormatter: function (internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},

	/**
	 * Converts the internal value to a formatted string with 2 decimal places.
	 *
	 * @param {number|null} internalValue - the raw value stored in the system.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the UI component displaying the field.
	 * @param {Object} record - the record containing this field.
	 * @returns {string} - a formatted string representation of the value.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDecimal2Item.format(internalValue, 2);
	},

	/**
	 * Validators to ensure the value is either null or a valid number.
	 */
	validators: [
		{
			type: "custom",
			clientOnly: true,
			condition: "(value == null) || isc.isA.Number(value)",
		},
	],
});

/**
 * Implements the BizDecimal5Item widget.
 * Extends BizDecimal2Item implemented above.
 */
isc.ClassFactory.defineClass("BizDecimal5Item", "BizDecimal2Item");

isc.BizDecimal5Item.addProperties({
	decimalPlaces: 5,
});

isc.SimpleType.create({
	name: "bizDecimal5",
	inheritsFrom: "float",
	editorType: "BizDecimal5Item",

	/**
	 * Formats the internal value for editing in the UI.
	 *
	 * @param {number|null} internalValue - the raw value stored in the system.
	 * @param {Object} field - the field definition.
	 * @param {Object} form - the form containing this field.
	 * @param {Object} record - the record containing this field.
	 * @returns {string} - the formatted value for display in an editable field.
	 */
	editFormatter: function (internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},

	/**
	 * Formats the internal value for normal (read-only) display in the UI.
	 *
	 * @param {number|null} internalValue - the raw value stored in the system.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the UI component displaying the field.
	 * @param {Object} record - the record containing this field.
	 * @returns {string} - the formatted value for display.
	 */
	normalDisplayFormatter: function (internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},

	/**
	 * Converts the internal value to a formatted string with 5 decimal places.
	 *
	 * @param {number|null} internalValue - the raw value stored in the system.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the UI component displaying the field.
	 * @param {Object} record - the record containing this field.
	 * @returns {string} - a formatted string representation of the value.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDecimal2Item.format(internalValue, 5);
	},

	/**
	 * Validators to ensure the value is either null or a valid number.
	 */
	validators: [
		{
			type: "custom",
			clientOnly: true,
			condition: "(value == null) || isc.isA.Number(value)",
		},
	],
});

/**
 * Implements the BizDecimal10Item widget.
 * Extends BizDecimal2Item implemented above.
 */
isc.ClassFactory.defineClass("BizDecimal10Item", "BizDecimal2Item");

isc.BizDecimal10Item.addProperties({
	decimalPlaces: 10,
});

isc.SimpleType.create({
	name: "bizDecimal10",
	inheritsFrom: "float",
	editorType: "BizDecimal10Item",

	/**
	 * Formats the internal value for editing in the UI.
	 *
	 * @param {number|null} internalValue - the raw value stored in the system.
	 * @param {Object} field - the field definition.
	 * @param {Object} form - the form containing this field.
	 * @param {Object} record - the record containing this field.
	 * @returns {string} - the formatted value for display in an editable field.
	 */
	editFormatter: function (internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},

	/**
	 * Formats the internal value for normal (read-only) display in the UI.
	 *
	 * @param {number|null} internalValue - the raw value stored in the system.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the UI component displaying the field.
	 * @param {Object} record - the record containing this field.
	 * @returns {string} - the formatted value for display.
	 */
	normalDisplayFormatter: function (internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},

	/**
	 * Converts the internal value to a formatted string with 10 decimal places.
	 *
	 * @param {number|null} internalValue - the raw value stored in the system.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the UI component displaying the field.
	 * @param {Object} record - the record containing this field.
	 * @returns {string} - a formatted string representation of the value.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDecimal2Item.format(internalValue, 10);
	},

	/**
	 * Validators to ensure the value is either null or a valid number.
	 */
	validators: [
		{
			type: "custom",
			clientOnly: true,
			condition: "(value == null) || isc.isA.Number(value)",
		},
	],
});

/**
 * Implements the BizDecimal0Item widget.
 * Extends BizDecimal2Item implemented above.
 */
isc.ClassFactory.defineClass("BizDecimal0Item", "BizDecimal2Item");

isc.BizDecimal0Item.addProperties({
	decimalPlaces: 0,
});

isc.SimpleType.create({
	name: "bizDecimal0",
	inheritsFrom: "float",
	editorType: "BizDecimal0Item",

	/**
	 * Formats the internal value for editing in the UI.
	 *
	 * @param {number|null} internalValue - the raw value stored in the system.
	 * @param {Object} field - the field definition.
	 * @param {Object} form - the form containing this field.
	 * @param {Object} record - the record containing this field.
	 * @returns {string} - the formatted value for display in an editable field.
	 */
	editFormatter: function (internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},

	/**
	 * Formats the internal value for normal (read-only) display in the UI.
	 *
	 * @param {number|null} internalValue - the raw value stored in the system.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the UI component displaying the field.
	 * @param {Object} record - the record containing this field.
	 * @returns {string} - the formatted value for display.
	 */
	normalDisplayFormatter: function (internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},

	/**
	 * Converts the internal value to a formatted string with 0 decimal places (whole number).
	 *
	 * @param {number|null} internalValue - the raw value stored in the system.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the UI component displaying the field.
	 * @param {Object} record - the record containing this field.
	 * @returns {string} - a formatted string representation of the value.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDecimal2Item.format(internalValue, 0);
	},

	/**
	 * Validators to ensure the value is either null or a valid number.
	 */
	validators: [
		{
			type: "custom",
			clientOnly: true,
			condition: "(value == null) || isc.isA.Number(value)",
		},
	],
});

/**
 * Implements the BizDecimal1Item widget.
 * Extends BizDecimal2Item implemented above.
 */
isc.ClassFactory.defineClass("BizDecimal1Item", "BizDecimal2Item");

isc.BizDecimal1Item.addProperties({
	decimalPlaces: 1,
});

isc.SimpleType.create({
	name: "bizDecimal1",
	inheritsFrom: "float",
	editorType: "BizDecimal1Item",

	/**
	 * Formats the internal value for editing in the UI.
	 *
	 * @param {number|null} internalValue - the raw value stored in the system.
	 * @param {Object} field - the field definition.
	 * @param {Object} form - the form containing this field.
	 * @param {Object} record - the record containing this field.
	 * @returns {string} - the formatted value for display in an editable field.
	 */
	editFormatter: function (internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},

	/**
	 * Formats the internal value for normal (read-only) display in the UI.
	 *
	 * @param {number|null} internalValue - the raw value stored in the system.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the UI component displaying the field.
	 * @param {Object} record - the record containing this field.
	 * @returns {string} - the formatted value for display.
	 */
	normalDisplayFormatter: function (internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},

	/**
	 * Converts the internal value to a formatted string with 1 decimal place.
	 *
	 * @param {number|null} internalValue - the raw value stored in the system.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the UI component displaying the field.
	 * @param {Object} record - the record containing this field.
	 * @returns {string} - a formatted string representation of the value.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDecimal2Item.format(internalValue, 1);
	},

	/**
	 * Validators to ensure the value is either null or a valid number.
	 */
	validators: [
		{
			type: "custom",
			clientOnly: true,
			condition: "(value == null) || isc.isA.Number(value)",
		},
	],
});

/**
 * Implements the BizDollarsAndCentsItem widget.
 * Extends BizDecimal2Item implemented above.
 */
isc.ClassFactory.defineClass("BizDollarsAndCentsItem", "BizDecimal2Item");

isc.BizDollarsAndCentsItem.addProperties({
	showHint: true,
	showHintInField: true,
	hint: "(+/-)$$$$$.cc",
});

isc.SimpleType.create({
	name: "bizDollarsAndCents",
	inheritsFrom: "float",
	editorType: "BizDollarsAndCentsItem",

	/**
	 * Formats the internal value for editing in the UI.
	 *
	 * @param {number|null} internalValue - the raw currency value stored in the system.
	 * @param {Object} field - the field definition.
	 * @param {Object} form - the form containing this field.
	 * @param {Object} record - the record containing this field.
	 * @returns {string} - the formatted value for display in an editable field.
	 */
	editFormatter: function (internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},

	/**
	 * Formats the internal value for normal (read-only) display in the UI.
	 *
	 * @param {number|null} internalValue - the raw currency value stored in the system.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the UI component displaying the field.
	 * @param {Object} record - the record containing this field.
	 * @returns {string} - the formatted value for display.
	 */
	normalDisplayFormatter: function (internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},

	/**
	 * Converts the internal value to a formatted string with 2 decimal places,
	 * ensuring correct currency representation.
	 *
	 * @param {number|null} internalValue - the raw currency value stored in the system.
	 * @param {Object} field - the field definition.
	 * @param {Object} component - the UI component displaying the field.
	 * @param {Object} record - the record containing this field.
	 * @returns {string} - a formatted currency string (e.g., "$1,234.56").
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizDollarsAndCentsItem.format(internalValue, 2);
	},

	/**
	 * Validators to ensure the value is either null or a valid number.
	 */
	validators: [
		{
			type: "custom",
			clientOnly: true,
			condition: "(value == null) || isc.isA.Number(value)",
		},
	],
});

/**
 * Implements the BizIntegerPercentageItem widget.
 * Extends TextItem implemented above.
 */
isc.ClassFactory.defineClass("BizIntegerPercentageItem", "TextItem");

isc.BizIntegerPercentageItem.addClassMethods({
	/**
	 * Parses the input value as a percentage.
	 * If the value is a string, it removes any leading/trailing whitespace and the '%' sign.
	 * Then, it converts the value to a float, divides it by 100, and returns the result.
	 * If the value is invalid or exceeds the maximum threshold, `null` is returned.
	 *
	 * @param {string|number} value - the value to parse, which could be a string or a number.
	 * @returns {number|null} - the parsed percentage as a float, or `null` if the value is invalid.
	 */
	parseInput: function (value) {
		if (isc.isA.Number(value)) {
			return value;
		}

		if (value) {
			value = value.toString().trim().trim("%");
			if (isNaN(value)) {
				return null;
			} else {
				let result = Math.round(parseFloat(value)) / 100;
				// Ensure the result is not too large
				if (result >= 1000000000000000000.0) {
					result = null;
				}
				return result;
			}
		} else {
			return null;
		}
	},

	/**
	 * Formats the value as a percentage.
	 * The value is multiplied by 100 and localized for display, with a '%' symbol added at the end.
	 *
	 * @param {number|string} value - the value to format as a percentage.
	 * @returns {string} - the formatted percentage string, e.g., "12%" or "50.5%".
	 */
	format: function (value) {
		if (!isc.isA.Number(value)) {
			return value;
		}

		return (value * 100).toLocalizedString(0, ".", ",", "-") + "%";
	},
});

isc.BizIntegerPercentageItem.addProperties({
	changeOnBlur: true, // Perform validation on field blur
	changeOnKeypress: false, // Don't perform validation on key press
	width: 100,
	showHint: true,
	showHintInField: true,
	hint: "(+/-)99999",
	selectOnFocus: true,
});

isc.BizIntegerPercentageItem.addMethods({
	/**
	 * Maps a value to its display representation as a percentage.
	 * If the value is `null`, it returns an empty string.
	 * If the value is not a number, it attempts to parse the value using `parseInput`.
	 * The value is then formatted as a percentage.
	 *
	 * @param {string|number|null} value - the value to map to display, which could be a string, number, or `null`.
	 * @returns {string} - the formatted percentage as a string (e.g., "12%" or "50.5%").
	 */
	mapValueToDisplay: function (value) {
		if (value == null) {
			return isc.emptyString;
		}
		if (!isc.isA.Number(value)) {
			value = isc.BizIntegerPercentageItem.parseInput(value);
		}
		return isc.BizIntegerPercentageItem.format(value);
	},

	/**
	 * Maps a display value (usually a string) back to the internal value format.
	 * If the display value is an empty string, it returns `null`.
	 * Otherwise, it attempts to parse the value using `parseInput`.
	 *
	 * @param {string|null} value - the value to map from display to internal value format.
	 * @returns {number|null} - the parsed internal value as a number or `null` if the value is empty.
	 */
	mapDisplayToValue: function (value) {
		if (isc.isAn.emptyString(value)) {
			value = null;
		} else {
			value = isc.BizIntegerPercentageItem.parseInput(value);
		}
		return value;
	},

	/**
	 * Override: Ensures that the displayed value matches the saved value and is formatted correctly.
	 * This method first calls the superclass method `updateValue` to set the internal value,
	 * then updates the displayed string so it is formatted correctly.
	 */
	updateValue: function () {
		// This will map the value to a valid float
		this.Super("updateValue", arguments);
		// This will update the displayed string so it's formatted correctly
		this.setElementValue(this.mapValueToDisplay(this.getValue()));
	},

	/**
	 * Override: If a string value is passed, it is mapped to the appropriate float value before saving.
	 * This is necessary as strings will not go through `mapDisplayToValue`.
	 *
	 * @param {string|number} newValue - the new value to set, which could be a string or a number.
	 * @returns {any} - the result of the `setValue` method from the superclass, after mapping the value.
	 */
	setValue: function (newValue) {
		if (isc.isA.String(newValue)) {
			newValue = isc.BizIntegerPercentageItem.parseInput(newValue);
		}
		return this.Super("setValue", [newValue]);
	},
});

isc.SimpleType.create({
	name: "bizIntegerPercentage",
	inheritsFrom: "integer",
	editorType: "BizIntegerPercentageItem",

	/**
	 * Formatter function for displaying the value when in edit mode.
	 * Uses the `shortDisplayFormatter` to format the internal value.
	 *
	 * @param {number|null} internalValue - the internal value to format.
	 * @param {Object} field - the field object that holds the value.
	 * @param {Object} form - the form object containing the field.
	 * @param {Object} record - the record containing the field value.
	 * @returns {string} - the formatted value as a string.
	 */
	editFormatter: function (internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},

	/**
	 * Formatter function for displaying the value in normal display mode.
	 * Uses the `shortDisplayFormatter` to format the internal value.
	 *
	 * @param {number|null} internalValue - the internal value to format.
	 * @param {Object} field - the field object that holds the value.
	 * @param {Object} component - the component object containing the field.
	 * @param {Object} record - the record containing the field value.
	 * @returns {string} - the formatted value as a string.
	 */
	normalDisplayFormatter: function (internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},

	/**
	 * Short display formatter for formatting the internal value as a percentage.
	 * Uses `BizIntegerPercentageItem.format` to format the value.
	 *
	 * @param {number|null} internalValue - the internal value to format.
	 * @param {Object} field - the field object that holds the value.
	 * @param {Object} component - the component object containing the field.
	 * @param {Object} record - the record containing the field value.
	 * @returns {string} - the formatted percentage value as a string (e.g., "10%").
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizIntegerPercentageItem.format(internalValue);
	},

	/**
	 * Validators for the custom type. The value must either be `null` or a number.
	 *
	 * @type {Array}
	 * @default [{type: 'custom', clientOnly: true, condition: '(value == null) || isc.isA.Number(value)'}]
	 */
	validators: [
		{
			type: "custom",
			clientOnly: true,
			condition: "(value == null) || isc.isA.Number(value)",
		},
	],
});

/**
 * Implements the BizSimplePercentageItem widget.
 * Extends BizIntegerPercentageItem implemented above.
 */
isc.ClassFactory.defineClass(
	"BizSimplePercentageItem",
	"BizIntegerPercentageItem",
);

isc.BizSimplePercentageItem.addClassMethods({
	/**
	 * Parses the input value, converting it to a number (float) representation.
	 * This method handles both strings (with or without the '%' sign) and numbers.
	 * If the input is a valid percentage string, it converts it to a float and returns it.
	 *
	 * @param {string|number} value - the value to be parsed. Can be a string or a number.
	 * @returns {number|null} - the parsed float value, or null if parsing fails.
	 */
	parseInput: function (value) {
		if (isc.isA.Number(value)) {
			return value;
		}

		if (value) {
			value = value.toString().trim(" ").trim("%");
			if (isNaN(value)) {
				return null;
			} else {
				let result = Math.round(parseFloat(value));
				if (result >= 1000000000000000000.0) {
					result = null;
				}
				return result;
			}
		} else {
			return null;
		}
	},

	/**
	 * Formats the input value as a percentage string.
	 * If the input is a number, it converts it to a localized string and appends a '%' symbol.
	 * If the input is not a number, it returns the original value.
	 *
	 * @param {string|number} value - the value to be formatted. Can be a number or a string.
	 * @returns {string} - the formatted percentage string (e.g., "10%") or the original value if not a number.
	 */
	format: function (value) {
		if (!isc.isA.Number(value)) {
			return value;
		}

		return value.toLocalizedString(0, ".", ",", "-") + "%";
	},
});

isc.BizSimplePercentageItem.addMethods({
	/**
	 * Maps the value to a displayable format.
	 * If the value is null, it returns an empty string.
	 * If the value is not a number, it attempts to parse the input using `parseInput`.
	 * Finally, it formats the value as a percentage string.
	 *
	 * @param {string|number|null} value - the value to be mapped to a displayable format. Can be a number, string, or null.
	 * @returns {string} - the formatted percentage string or an empty string if value is null.
	 */
	mapValueToDisplay: function (value) {
		if (value == null) {
			return isc.emptyString;
		}
		if (!isc.isA.Number(value)) {
			value = isc.BizSimplePercentageItem.parseInput(value);
		}
		return isc.BizSimplePercentageItem.format(value);
	},

	/**
	 * Maps the display value back to the internal value.
	 * If the value is an empty string, it returns null.
	 * Otherwise, it parses the input using `parseInput` to convert it to a number.
	 *
	 * @param {string|null} value - the display value to be mapped to the internal value. Can be a string or null.
	 * @returns {number|null} - the parsed number or null if the input is empty.
	 */
	mapDisplayToValue: function (value) {
		if (isc.isAn.emptyString(value)) {
			value = null;
		} else {
			value = isc.BizSimplePercentageItem.parseInput(value);
		}

		return value;
	},

	/**
	 * Sets the value of the component.
	 * If the value passed is a string, it is converted to a float using `parseInput` before being set.
	 * This ensures that the string representation of the value is properly converted before saving.
	 *
	 * @param {string|number} newValue - the new value to be set. It can be a string (percentage) or a number.
	 * @returns {*} - the result of the `Super("setValue")` call after setting the value.
	 */
	setValue: function (newValue) {
		if (isc.isA.String(newValue)) {
			newValue = isc.BizSimplePercentageItem.parseInput(newValue);
		}
		return this.Super("setValue", [newValue]);
	},
});

isc.SimpleType.create({
	name: "bizSimplePercentage",
	inheritsFrom: "float",
	editorType: "BizSimplePercentageItem",

	/**
	 * Formats the internal value for editing.
	 * This method is used for formatting the value when it is being displayed in the editor.
	 *
	 * @param {number} internalValue - the value to be formatted.
	 * @param {object} field - the field the value belongs to.
	 * @param {object} form - the form in which the value will be displayed.
	 * @param {object} record - the record that contains the value.
	 * @returns {string} - the formatted value to be displayed in the editor.
	 */
	editFormatter: function (internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},

	/**
	 * Formats the internal value for normal display.
	 * This method is used for formatting the value when it is displayed outside the editor.
	 *
	 * @param {number} internalValue - the value to be formatted.
	 * @param {object} field - the field the value belongs to.
	 * @param {object} component - the component that displays the value.
	 * @param {object} record - the record that contains the value.
	 * @returns {string} - the formatted value to be displayed normally.
	 */
	normalDisplayFormatter: function (internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},

	/**
	 * Formats the internal value as a percentage for display.
	 * This method converts the value into a percentage string (e.g., "45%").
	 *
	 * @param {number} internalValue - the value to be formatted as a percentage.
	 * @param {object} field - the field the value belongs to.
	 * @param {object} component - the component displaying the value.
	 * @param {object} record - the record that contains the value.
	 * @returns {string} - the formatted percentage string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizSimplePercentageItem.format(internalValue);
	},

	/**
	 * Custom validator for the `bizSimplePercentage` type.
	 * Validates that the value is either null or a valid number.
	 *
	 * @type {Array}
	 * @property {object} validators
	 * @property {string} type - the type of validation to apply.
	 * @property {boolean} clientOnly - specifies whether the validation is client-side only.
	 * @property {string} condition - the validation condition to be checked.
	 */
	validators: [
		{
			type: "custom",
			clientOnly: true,
			condition: "(value == null) || isc.isA.Number(value)",
		},
	],
});

/**
 * Implements the BizIntegerSeparatorItem widget.
 * Extends TextItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizIntegerSeparatorItem", "TextItem");

isc.BizIntegerSeparatorItem.addClassMethods({
	/**
	 * Parses the input value, ensuring that it is a valid number.
	 * Removes commas and trims the string before converting it into a valid float.
	 *
	 * @param {string|number} value - the value to be parsed. Can be a string or a number.
	 * @returns {number|null} - the parsed float or `null` if the value is invalid.
	 */
	parseInput: function (value) {
		// If the value is already a number, return it as is.
		if (isc.isA.Number(value)) {
			return value;
		}

		// If the value exists, process it.
		if (value) {
			// Remove commas and trim the string
			value = value.toString().replace(/,/g, "").trim(" ");

			// Check if the value is a valid number after removing commas.
			if (isNaN(value)) {
				return null;
			} else {
				// Round the parsed value and return it
				let result = Math.round(parseFloat(value));

				// If the result is too large, return null
				if (result >= 1000000000000000000.0) {
					result = null;
				}
				return result;
			}
		}
		// Return null if the value is empty
		else {
			return null;
		}
	},

	/**
	 * Formats the value as a localized string with thousands separators.
	 * If the value is a number, it is formatted and returned as a string.
	 *
	 * @param {number|string} value - the value to be formatted. Can be a number or a string.
	 * @returns {string} - the formatted value as a string with thousands separators.
	 */
	format: function (value) {
		// If the value is not a number, return it as is.
		if (!isc.isA.Number(value)) {
			return value;
		}

		// Format the value using a localized string with separators for thousands
		return value.toLocalizedString(0, ".", ",", "-");
	},
});

isc.BizIntegerSeparatorItem.addProperties({
	changeOnBlur: true, // Perform validation on field blur
	changeOnKeypress: false, // Don't perform validation on key press
	width: 100,
	showHint: true,
	showHintInField: true,
	hint: "(+/-)99,999,999",
	selectOnFocus: true,
});

isc.BizIntegerSeparatorItem.addMethods({
	/**
	 * Maps the provided value to a displayable string, formatted with separators for thousands.
	 * If the value is not already a number, it will be parsed.
	 *
	 * @param {string|number|null} value - the value to be mapped for display. Can be a string, number, or `null`.
	 * @returns {string} - the formatted display string or an empty string if the value is null.
	 */
	mapValueToDisplay: function (value) {
		// If the value is null, return an empty string
		if (value == null) {
			return isc.emptyString;
		}

		// If the value is not a number, parse it to a valid number
		if (!isc.isA.Number(value)) {
			value = isc.BizIntegerSeparatorItem.parseInput(value);
		}

		// Format and return the value for display
		return isc.BizIntegerSeparatorItem.format(value);
	},

	/**
	 * Maps the displayed value back to the corresponding internal value.
	 * If the display value is an empty string, it will be converted to `null`.
	 * Otherwise, the string will be parsed into a number.
	 *
	 * @param {string|null} value - the value displayed in the UI. Can be a string or `null`.
	 * @returns {number|null} - the internal value corresponding to the display value, or `null` if the value is empty.
	 */
	mapDisplayToValue: function (value) {
		// If the value is an empty string, set it to null
		if (isc.isAn.emptyString(value)) {
			value = null;
		} else {
			// Otherwise, parse the value into a number
			value = isc.BizIntegerSeparatorItem.parseInput(value);
		}

		return value;
	},

	/**
	 * Updates the internal value and the displayed string in the UI.
	 * Ensures that the displayed value matches the formatted internal value.
	 */
	updateValue: function () {
		// Call the parent method to update the internal value
		this.Super("updateValue", arguments);

		// Set the formatted value in the UI element
		this.setElementValue(this.mapValueToDisplay(this.getValue()));
	},

	/**
	 * Sets the internal value, ensuring it is correctly parsed from a string to a number.
	 * This is necessary because the string passed in doesn't go through `mapDisplayToValue`.
	 *
	 * @param {string|number} newValue - the new value to set. Can be a string or number.
	 * @returns {*} - the result of the `Super` method call for setting the value.
	 */
	setValue: function (newValue) {
		// If the new value is a string, parse it into a number
		if (isc.isA.String(newValue)) {
			newValue = isc.BizIntegerSeparatorItem.parseInput(newValue);
		}

		// Call the parent method to set the value
		return this.Super("setValue", [newValue]);
	},
});

isc.SimpleType.create({
	name: "bizIntegerSeparator",
	inheritsFrom: "integer",
	editorType: "BizIntegerSeparatorItem",

	/**
	 * Formatter for the value when editing. It applies the `shortDisplayFormatter` method to format the internal value
	 * before displaying it in an editing context.
	 *
	 * @param {number} internalValue - the internal value to be formatted for display.
	 * @param {object} field - the field being edited.
	 * @param {object} form - the form containing the field.
	 * @param {object} record - the record containing the field value.
	 * @returns {string} - the formatted string representing the internal value.
	 */
	editFormatter: function (internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},

	/**
	 * Formatter for normal display. It applies the `shortDisplayFormatter` method to format the internal value
	 * before displaying it in a non-editing context.
	 *
	 * @param {number} internalValue - the internal value to be formatted for normal display.
	 * @param {object} field - the field being displayed.
	 * @param {object} component - the component that holds the field.
	 * @param {object} record - the record containing the field value.
	 * @returns {string} - the formatted string representing the internal value.
	 */
	normalDisplayFormatter: function (internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},

	/**
	 * Short display formatter for formatting an internal value, typically for display in forms or UI elements.
	 * It uses the `BizIntegerSeparatorItem.format` method to apply the correct formatting with separators for thousands.
	 *
	 * @param {number} internalValue - the internal value to be formatted.
	 * @param {object} field - the field related to the value.
	 * @param {object} component - the component where the value will be displayed.
	 * @param {object} record - the record containing the value to be formatted.
	 * @returns {string} - the formatted string, representing the value with appropriate separators for thousands.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizIntegerSeparatorItem.format(internalValue);
	},

	/**
	 * Validators for the `bizIntegerSeparator` type. This custom validator ensures that the value is either `null`
	 * or a number. It will be applied when validating values in the form.
	 *
	 * @type {Array}
	 * @example
	 * // Validator ensures that the value is either null or a number
	 * validators: [{type: 'custom', clientOnly: true, condition: '(value == null) || isc.isA.Number(value)'}]
	 */
	validators: [
		{
			type: "custom",
			clientOnly: true,
			condition: "(value == null) || isc.isA.Number(value)",
		},
	],
});

/**
 * Implements the BizTwoDecimalPlacesPercentageItem widget.
 * Extends BizIntegerPercentageItem implemented above.
 */
isc.ClassFactory.defineClass(
	"BizTwoDecimalPlacesPercentageItem",
	"BizIntegerPercentageItem",
);

isc.BizTwoDecimalPlacesPercentageItem.addClassMethods({
	/**
	 * Parses the input value, which can be a string or a number, and returns a float value.
	 * The input string is trimmed, any percentage sign is removed, and the value is parsed as a float.
	 * The result is rounded to two decimal places.
	 * If the parsed value is too large or invalid, `null` is returned.
	 *
	 * @param {string|number} value - the input value to be parsed. It can be a string representing a percentage (e.g., "25%") or a number.
	 * @returns {number|null} - the parsed float value, or `null` if the input is invalid or too large.
	 */
	parseInput: function (value) {
		if (isc.isA.Number(value)) {
			return value;
		}

		if (value) {
			value = value.toString().trim(" ").trim("%");
			if (isNaN(value)) {
				return null;
			} else {
				let result = Math.round(parseFloat(value) * 100) / 10000;
				if (result >= 1000000000000000000.0) {
					result = null;
				}
				return result;
			}
		} else {
			return null;
		}
	},

	/**
	 * Formats the value as a percentage, ensuring it's displayed with two decimal places.
	 * If the value is a valid number, it's multiplied by 100 and formatted using `toLocalizedString` with two decimal places.
	 * The formatted result includes a percentage sign ('%').
	 *
	 * @param {string|number} value - the value to be formatted. It can be a number or a string representing a percentage.
	 * @returns {string} - the formatted percentage string, or the original string if the value is not a valid number.
	 */
	format: function (value) {
		if (!isc.isA.Number(value)) {
			return value;
		}

		return (value * 100).toLocalizedString(2, ".", ",", "-") + "%";
	},
});

isc.BizTwoDecimalPlacesPercentageItem.addProperties({
	hint: "(+/-)99999.99",
});

isc.BizTwoDecimalPlacesPercentageItem.addMethods({
	/**
	 * Maps the given value to a formatted display string. If the value is `null`, an empty string is returned.
	 * If the value is not a valid number, it is parsed using the `parseInput` method before being formatted.
	 * The formatted value is a percentage string with two decimal places.
	 *
	 * @param {string|number|null} value - the value to be mapped to a display string. It can be a string or a number.
	 * @returns {string} - the formatted display string, or an empty string if the value is `null`.
	 */
	mapValueToDisplay: function (value) {
		if (value == null) {
			return isc.emptyString;
		}
		if (!isc.isA.Number(value)) {
			value = isc.BizTwoDecimalPlacesPercentageItem.parseInput(value);
		}
		return isc.BizTwoDecimalPlacesPercentageItem.format(value);
	},

	/**
	 * Maps the given display value back to its raw value. If the display value is an empty string, `null` is returned.
	 * Otherwise, the value is parsed using the `parseInput` method.
	 *
	 * @param {string|null} value - the display value to be mapped back to a raw value. It can be a string or `null`.
	 * @returns {number|null} - the raw value corresponding to the display value, or `null` if the display value is an empty string.
	 */
	mapDisplayToValue: function (value) {
		if (isc.isAn.emptyString(value)) {
			value = null;
		} else {
			value = isc.BizTwoDecimalPlacesPercentageItem.parseInput(value);
		}

		return value;
	},

	/**
	 * Sets the value of the item. If the passed value is a string, it is parsed into a float using the `parseInput` method.
	 * The parsed value is then passed to the `setValue` method of the superclass.
	 *
	 * @param {string|number} newValue - the new value to be set. It can be a string or a number.
	 * @returns {any} - the result of the superclass's `setValue` method call with the parsed value.
	 */
	setValue: function (newValue) {
		if (isc.isA.String(newValue)) {
			newValue = isc.BizTwoDecimalPlacesPercentageItem.parseInput(newValue);
		}
		return this.Super("setValue", [newValue]);
	},
});

isc.SimpleType.create({
	name: "bizTwoDecimalPlacesPercentage",
	inheritsFrom: "float",
	editorType: "BizTwoDecimalPlacesPercentageItem",

	/**
	 * Formats the internal value for display when editing. This uses the `shortDisplayFormatter` method to generate
	 * the formatted value.
	 *
	 * @param {number} internalValue - the internal value to be formatted.
	 * @param {object} field - the field object for the value.
	 * @param {object} form - the form object that contains the field.
	 * @param {object} record - the record containing the data.
	 * @returns {string} - the formatted display string for the value.
	 */
	editFormatter: function (internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},

	/**
	 * Formats the internal value for display in a normal context (outside of editing). This uses the `shortDisplayFormatter`
	 * method to generate the formatted value.
	 *
	 * @param {number} internalValue - the internal value to be formatted.
	 * @param {object} field - the field object for the value.
	 * @param {object} component - the component object that contains the field.
	 * @param {object} record - the record containing the data.
	 * @returns {string} - the formatted display string for the value.
	 */
	normalDisplayFormatter: function (internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},

	/**
	 * Formats the internal value as a percentage with two decimal places. This method is used for both normal and
	 * editing display formatting.
	 *
	 * @param {number} internalValue - the internal value to be formatted.
	 * @param {object} field - the field object for the value.
	 * @param {object} component - the component object that contains the field.
	 * @param {object} record - the record containing the data.
	 * @returns {string} - the formatted value as a percentage string with two decimal places.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizTwoDecimalPlacesPercentageItem.format(internalValue);
	},

	validators: [
		{
			type: "custom",
			clientOnly: true,
			condition: "(value == null) || isc.isA.Number(value)",
		},
	],
});

/**
 * Implements the BizTimeDurationItem widget.
 * Extends TextItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizTimeDurationItem", "TextItem");

isc.BizTimeDurationItem.addClassMethods({
	/**
	 * Parses the input value and converts it to a float representing a time duration. It can handle:
	 * - Decimal format (e.g., "2.5")
	 * - time format with hours and minutes (e.g., "2:30" or "2 30")
	 *
	 * @param {string|number} value - the input value to be parsed, either a string or a number.
	 * @returns {number|null} - the parsed time duration as a float, or `null` if parsing fails.
	 */
	parseInput: function (value) {
		if (isc.isA.Number(value)) {
			return value;
		}

		if (value) {
			// Look for a decimal formatted match (e.g., "2.5")
			let match = /^\s*\s*([-+]?\d+)\s*\.\s*(\d?\d)?\s*$/.exec(value); // sign, 1 or more digits, a decimal point, 1 or 2 digits
			if (match) {
				let result = parseFloat(match[1] + "." + (match[2] | 0));
				if (result >= 1000000000000000000.0) {
					result = null;
				}
				return result;
			}

			// Look for a time formatted match (e.g., "2 30" or "2:30")
			match = /^\s*\s*([-+]?\d+)\s*\D?\s*(\d?\d)?\s*$/.exec(value); // sign, 1 or more digits, any non digit, 1 or 2 digits
			if (match) {
				const hours = parseInt(match[1] | 0, 10);
				const minutes = Math.min(parseInt(match[2] | 0, 10), 59);
				if (hours >= 0) {
					return hours + minutes / 60.0;
				}
				return hours - minutes / 60.0;
			} else {
				return null;
			}
		} else {
			return null;
		}
	},

	/**
	 * Formats the time duration value into a string with hours and minutes. It formats the value as:
	 * - Positive values: "H:MM"
	 * - Negative values: "-H:MM"
	 * It ensures that the minutes part is always two digits (e.g., "02" instead of "2").
	 *
	 * @param {number} value - the time duration to be formatted.
	 * @returns {string} - the formatted time duration string (e.g., "2:30", "-2:30").
	 */
	format: function (value) {
		if (!isc.isA.Number(value)) {
			return value;
		}

		let decimalMinutes;
		let hours;
		if (value >= 0) {
			hours = Math.floor(value);
			decimalMinutes = value - hours;
		} else {
			hours = Math.ceil(value);
			decimalMinutes = Math.abs(value - hours);
		}

		let stringMinutes = Math.round(decimalMinutes * 60).toString();
		if (stringMinutes == "60") {
			hours = value >= 0 ? hours + 1 : hours - 1;
			stringMinutes = "00";
		} else if (stringMinutes.length == 1) {
			stringMinutes = "0" + stringMinutes;
		}

		return hours + ":" + stringMinutes;
	},
});

isc.BizTimeDurationItem.addProperties({
	changeOnBlur: true, // Perform validation on field blur
	changeOnKeypress: false, // Don't perform validation on key press
	width: 100,
	showHint: true,
	showHintInField: true,
	hint: "(+/-)HHH:MM",
	selectOnFocus: true,
});

isc.BizTimeDurationItem.addMethods({
	/**
	 * Maps the provided value to a displayable format. If the value is null, an empty string is returned.
	 * If the value is not a number, it is parsed into a valid time duration before being formatted.
	 *
	 * @param {string|number|null} value - the value to be mapped to a displayable format. It can be a string, number, or null.
	 * @returns {string} - the formatted time duration string.
	 */
	mapValueToDisplay: function (value) {
		if (value == null) {
			return isc.emptyString;
		}
		if (!isc.isA.Number(value)) {
			value = isc.BizTimeDurationItem.parseInput(value);
		}
		return isc.BizTimeDurationItem.format(value);
	},

	/**
	 * Maps the provided display value (usually from the UI) back to a value that can be saved.
	 * If the value is an empty string, it will return null. Otherwise, it will parse the string into a valid time duration.
	 *
	 * @param {string|null} value - the value to be mapped back to a usable format. Can be a string or null.
	 * @returns {number|null} - the parsed time duration as a float, or null if the input was an empty string.
	 */
	mapDisplayToValue: function (value) {
		if (isc.isAn.emptyString(value)) {
			value = null;
		} else {
			value = isc.BizTimeDurationItem.parseInput(value);
		}

		return value;
	},

	/**
	 * Ensures the value displayed in the element matches the saved value and is correctly formatted.
	 * It calls the superclass method to update the value and then updates the displayed value to the properly formatted string.
	 *
	 * @override
	 * @returns {void}
	 */
	updateValue: function () {
		// This will map the value to a valid float
		this.Super("updateValue", arguments);
		// This will update the displayed string so it's formatted correctly
		this.setElementValue(this.mapValueToDisplay(this.getValue()));
	},

	/**
	 * Sets a new value for the item. If the provided value is a string, it will be parsed into a valid float
	 * representing the time duration before being saved. This ensures the value is correctly formatted for saving.
	 *
	 * @override
	 * @param {string|number} newValue - the new value to be set. It can be a string or number.
	 * @returns {void}
	 */
	setValue: function (newValue) {
		if (isc.isA.String(newValue)) {
			newValue = isc.BizTimeDurationItem.parseInput(newValue);
		}
		return this.Super("setValue", [newValue]);
	},
});

isc.SimpleType.create({
	name: "bizTimeDuration",
	inheritsFrom: "float",
	editorType: "BizTimeDurationItem",

	/**
	 * Formatter to convert the internal value to a displayable format for the editor.
	 * Uses the `shortDisplayFormatter` method to return the formatted value.
	 *
	 * @param {number} internalValue - the internal value to be formatted for display in the editor.
	 * @param {object} field - the field containing the value.
	 * @param {object} form - the form containing the field.
	 * @param {object} record - the record containing the field.
	 * @returns {string} - the formatted display value.
	 */
	editFormatter: function (internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},

	/**
	 * Formatter for the normal display value, used for displaying the value in a standard format.
	 * It delegates to the `shortDisplayFormatter` method to return the formatted value.
	 *
	 * @param {number} internalValue - the internal value to be formatted for standard display.
	 * @param {object} field - the field containing the value.
	 * @param {object} component - the component containing the field.
	 * @param {object} record - the record containing the field.
	 * @returns {string} - the formatted display value.
	 */
	normalDisplayFormatter: function (internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},

	/**
	 * A helper function to format the internal value into a string representation.
	 * This method is used by both the `editFormatter` and `normalDisplayFormatter`.
	 *
	 * @param {number} internalValue - the internal value to be formatted for display.
	 * @param {object} field - the field containing the value.
	 * @param {object} component - the component containing the value.
	 * @param {object} record - the record containing the value.
	 * @returns {string} - the formatted time duration string.
	 */
	shortDisplayFormatter: function (internalValue, field, component, record) {
		return isc.BizTimeDurationItem.format(internalValue);
	},

	/**
	 * Custom validator to ensure the value is either null or a valid number.
	 * This is used for validating the value entered by the user.
	 *
	 * @type {Array<object>}
	 * @property {string} type - the type of the validator.
	 * @property {boolean} clientOnly - indicates that this is a client-side validation.
	 * @property {string} condition - the condition for validation, checks that the value is null or a valid number.
	 */
	validators: [
		{
			type: "custom",
			clientOnly: true,
			condition: "(value == null) || isc.isA.Number(value)",
		},
	],
});

/**
 * Implements the BizContentLinkItem widget.
 * Extends CanvasItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizContentLinkItem", isc.CanvasItem);

isc.BizContentLinkItem.addProperties({
	height: 25,
	width: "*",
	rowSpan: "*",
	endRow: false,
	startRow: false,
	canFocus: true,
	shouldSaveValue: true, // This is an editable data item
});

isc.BizContentLinkItem.addMethods({
	/**
	 * Initializes the `BizContentLinkItem` by creating the link and canvas elements.
	 * If the item is editable, it includes an upload button; otherwise, it only displays the link.
	 *
	 * @param {object} config - the configuration object for the component.
	 * @param {boolean} config.editable - whether the content link is editable.
	 */
	init: function (config) {
		// Create an HTMLFlow to display the link content
		this._link = isc.HTMLFlow.create({
			contents: "Empty",
			width: "100%",
		});

		// Create the canvas layout depending on whether the item is editable or not
		if (config.editable) {
			this.canvas = isc.HLayout.create({
				defaultLayoutAlign: "center",
				members: [
					this._link,
					isc.LayoutSpacer.create({ width: 3 }),
					isc.BizUtil.createUploadButton(this, false, false),
				],
			});
		} else {
			this.canvas = isc.HLayout.create({
				defaultLayoutAlign: "center",
				members: [this._link],
			});
		}

		// Call the superclass's init method
		this.Super("init", arguments);
	},

	/**
	 * Updates the content link value and displays it on the link element.
	 * If the value is not provided, it displays "<Empty>".
	 *
	 * @param {string|null} newValue - the new value for the content link, or `null` if the link should be empty.
	 * @returns {object} - the updated `BizContentLinkItem` instance.
	 */
	setValue: function (newValue) {
		// Ensure the link is updated only if the canvas is initialized and the value is not user-set
		if (this.canvas != null && !this.userSetValue) {
			if (newValue) {
				// Construct the URL with required query parameters
				const url =
					SKYVE.Util.CONTEXT_URL +
					"content?_n=" +
					newValue +
					"&_doc=" +
					this.form._view._mod +
					"." +
					this.form._view._doc +
					"&_b=" +
					this.name.replaceAll("_", ".");

				// Update the link's contents with the constructed URL
				this._link.setContents(
					'<div style="line-height:25px;vertical-align:middle;">' +
						this.canvas.linkHTML(
							url,
							this.value ? this.value : "Content",
							"_blank",
						) +
						"</div>",
				);
			} else {
				// If no value, display "<Empty>" in the link
				this._link.setContents(
					'<div style="line-height:25px;vertical-align:middle;">&lt;Empty&gt;</div>',
				);
			}
		}

		// Call the superclass's setValue method
		return this.Super("setValue", [newValue]);
	},
});

isc.SimpleType.create({
	inheritsFrom: "canvas",
	name: "bizContentLink",
	editorType: "BizContentLinkItem",
});

/**
 * Implements the BizContentImageItem widget.
 * Extends CanvasItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizContentImageItem", isc.CanvasItem);

isc.BizContentImageItem.addProperties({
	width: "*",
	rowSpan: "*",
	endRow: false,
	startRow: false,
	canFocus: true,
	shouldSaveValue: true, // This is an editable data item
});

isc.BizContentImageItem.addMethods({
	/**
	 * Initializes the `BizContentImageItem` by creating an image element and a canvas layout.
	 * If the item is editable, it includes an upload button; otherwise, it only displays the image.
	 *
	 * @param {object} config - the configuration object for the component.
	 * @param {string} [config.width='100%'] - the width of the image.
	 * @param {string} [config.height='100%'] - the height of the image.
	 * @param {boolean} config.editable - whether the image is editable.
	 * @param {boolean} [config.showMarkup=false] - whether to show the markup in the upload button.
	 */
	init: function (config) {
		// Create an Img element with specified configurations
		this._img = isc.Img.create({
			width: config.width ? config.width : "100%",
			height: config.height ? config.height : "100%",
			imageType: "stretch",
			overflow: "hidden",
			isGroup: true,
			styleName: "bizhubRoundedBorder bizhubContentImgFit",
			groupBorderCSS: "1px solid #bfbfbf",
			margin: 1,
			groupLabelBackgroundColor: "transparent",
			showDisabled: false,
			showDown: false,
			showFocus: false,
			showFocused: false,
			showFocusedAsOver: false,
			showRollOver: false,
			src: "[SKIN]blank.gif",
			cursor: "pointer",
			click(event) {
				let src = this.src;
				// Remove the width and height parameters to get the full image, not a Thumbnail
				src = src.replace(/&_w=\d*/, "").replace(/&_h=\d*/, "");
				window.open(src, "_blank");
			},
		});

		// Create the canvas layout with an upload button if editable
		if (config.editable) {
			const centredUploadButton = isc.HLayout.create({
				width: 1, // Make minimum width of button
				height: config.height ? config.height : "100%",
				defaultLayoutAlign: "center",
				members: [
					isc.BizUtil.createUploadButton(this, true, config.showMarkup),
				],
			});
			this.canvas = isc.HLayout.create({
				defaultLayoutAlign: "center",
				members: [
					this._img,
					isc.LayoutSpacer.create({ width: 5 }),
					centredUploadButton,
				],
			});
		} else {
			this.canvas = isc.HLayout.create({
				defaultLayoutAlign: "center",
				members: [this._img],
			});
		}

		// Call the superclass's init method
		this.Super("init", arguments);
	},

	/**
	 * Updates the image source when a new value is set.
	 * If the value is not provided, it sets the source to a blank image.
	 *
	 * @param {string|null} newValue - the new content value, or `null` to clear the image.
	 * @returns {object} - the updated `BizContentImageItem` instance.
	 */
	setValue: function (newValue) {
		// Ensure the image is updated only if the canvas is initialized and the value is not user-set
		if (this.canvas != null && !this.userSetValue) {
			if (newValue) {
				// Construct the URL with required query parameters
				const url =
					SKYVE.Util.CONTEXT_URL +
					"content?_n=" +
					newValue +
					"&_doc=" +
					this.form._view._mod +
					"." +
					this.form._view._doc +
					"&_b=" +
					this.name.replaceAll("_", ".") +
					"&_w=" +
					(this._img.getWidth() - 20) + // -20 for the border
					"&_h=" +
					(this._img.getHeight() - 20); // -20 for the border
				// Update the image source
				this._img.setSrc(url);
			} else {
				// If no value, display a blank image
				this._img.setSrc("[SKIN]blank.gif");
			}
		}

		// Call the superclass's setValue method
		return this.Super("setValue", [newValue]);
	},
});

isc.SimpleType.create({
	inheritsFrom: "canvas",
	name: "bizContentImage",
	editorType: "BizContentImageItem",
});

/**
 * Implements the BizLookupDescriptionItem widget.
 * Extends CanvasItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizLookupDescriptionItem", isc.CanvasItem);

isc.BizLookupDescriptionItem.addProperties({
	height: 20,
	width: "*",
	rowSpan: "*",
	endRow: false,
	startRow: false,
	canFocus: true,
	shouldSaveValue: true, // This is an editable data item
	fetchMissingValues: false, // These are defined for the combo but also need to be here on the canvas
	autoFetchData: false, // Don't let the canvas autofetch, the combo on the canvas does this

	// Based on document permissions
	canCreate: false,
	canUpdate: false,

	// Conditions to evaluate from the view widget definition
	canPick: true,
	canEdit: true,
	canAdd: true,
	canClear: true,
});

isc.BizLookupDescriptionItem.addMethods({
	/**
	 * Initializes the combo box with the given configuration.
	 * @param {Object} config - the configuration object for the combo box.
	 * @param {boolean} config.allowEmptyValue - specifies if an empty value is allowed.
	 * @param {string} config.textAlign - the alignment of the text within the combo box.
	 * @param {Object} config.optionDataSource - the data source for the options of the combo box.
	 * @param {string} config.valueField - the field representing the value in the options.
	 * @param {string} config.displayField - the field representing the display value in the options.
	 * @param {Object} config.pickListFields - the fields for the pick list.
	 * @param {Object} config.filterFields - the fields to use for filtering.
	 * @param {Object} config.params - additional parameters for filtering.
	 */
	init: function (config) {
		const me = this; // Required as parent and child scope is required

		const combo = {
			name: "_combo",
			type: "comboBox",
			showTitle: false,
			width: "*",
			textAlign: config.textAlign,
			selectOnFocus: true,
			fetchMissingValues: false,
			autoFetchData: false,
			useClientFiltering: false,
			cachePickListResults: false,
			allowEmptyValue: config.allowEmptyValue,
			optionDataSource: config.optionDataSource,
			valueField: config.valueField,
			displayField: config.displayField,
			pickListFields: config.pickListFields,
			filterFields: config.filterFields,
			pickListProperties: {
				/**
				 * Filters data based on the provided criteria and updates the request properties.
				 * @param {Object} criteria - the filter criteria.
				 * @param {function} callback - the callback function to call after filtering.
				 * @param {Object} requestProperties - the request properties to be updated.
				 */
				filterData: function (criteria, callback, requestProperties) {
					if (requestProperties) {
						if (!requestProperties.params) {
							requestProperties.params = {};
						}
					} else {
						requestProperties = { params: {} };
					}

					requestProperties.params._c = me._view.gather(false)._c;
					if (me._view._b) {
						requestProperties.params._b = me._view._b;
					}
					requestProperties.params._cc = ""; // continue conversation
					this.Super("filterData", arguments);
				},
			},
			completeOnTab: true,
			textMatchStyle: "substring",

			/**
			 * Gets and modifies the filter criteria used for the pick list.
			 * This method extends the default filter criteria by converting it using BizUtil functions
			 * and adding any additional parameters specified in the configuration.
			 *
			 * @returns {Object} the modified filter criteria object containing:
			 *   - the base criteria from the parent class
			 *   - converted criteria using BizUtil.convertFilterCriteria
			 *   - additional parameters from config.params if specified
			 */
			getPickListFilterCriteria: function () {
				let result = this.Super("getPickListFilterCriteria", arguments);
				isc.BizUtil.convertFilterCriteria(result);
				if (config.params) {
					result = isc.BizUtil.completeFilterCriteria(
						result,
						config.params,
						me._view,
					);
				}
				return result;
			},

			/**
			 * Shows the pick list dropdown and marks the data source for dropdown behavior.
			 * If the option data source supports criteria comparison, sets a flag indicating
			 * this is a dropdown operation.
			 *
			 * @override
			 * @returns {void}
			 */
			showPicker: function () {
				const optionDataSource = this.getOptionDataSource();
				if (optionDataSource && optionDataSource.compareCriteria) {
					optionDataSource._drop = true;
				}
				this.Super("showPicker");
			},

			/**
			 * Handles the change event of the combo box.
			 * @param {DynamicForm} form - the form containing the combo box.
			 * @param {Canvas} item - the combo box item.
			 * @param {string} value - the new value selected.
			 */
			changed: function (form, item, value) {
				if (this.getDisplayValue() == value) {
					me.Super("setValue", null);
				} else {
					me.Super("setValue", [value]);
					if (value) {
						me.bizPicked(me.form, me, value);
					}
				}
			},
			/**
			 * Handles the blur event of the combo box.
			 * @param {DynamicForm} form - the form containing the combo box.
			 * @param {Canvas} item - the combo box item.
			 */
			blur: (form, item) => {
				if (this.getValue() == null) {
					const itemValue = item.getValue();
					if (itemValue && itemValue != "") {
						item.setValue(""); // blank the combobox
					}
				}
			},
			valueMap: {},
		};

		// Create the form with editable state
		if (config.editable) {
			this._form = isc.DynamicForm.create({
				writeFormTag: false,
				numCols: 2,
				colWidths: ["*", 65],
				margin: 0,
				cellPadding: 0,
			});

			this._splitButton = isc.BizUtil.createSplitButton(
				"Pick",
				null,
				false,
				"Pick a record",
				() => {
					const pickList = isc.BizUtil.getPickList(
						this,
						config.params,
						this._view,
					);
					pickList.setDataSource(config.optionDataSource);
					isc.WindowStack.popup(this.getPageRect(), "Pick", true, [pickList]);
				},
				"Other Options",
				this._form,
				[
					{
						title: "Edit",
						icon: "icons/zoom.gif",
						click: (event) => {
							this.zoom(false);
						},
						enableIf: (target, menu, item) => {
							return this.canUpdate && this.canEdit && this.getValue() != null;
						},
					},
					{
						title: "New",
						icon: "icons/new.png",
						click: (event) => {
							const newParams = config.params
								? isc.BizUtil.addFilterRequestParams(
										{},
										config.params,
										this._view,
									)
								: {};
							this.zoom(true, newParams);
						},
						enableIf: (target, menu, item) => {
							return this.canCreate && this.canAdd;
						},
					},
					{
						title: "Clear",
						icon: "icons/delete.png",
						click: (event) => {
							this.setValue(null);
							this.bizCleared(this.form, this, null);
						},
						enableIf: (target, menu, item) => {
							return this.canClear;
						},
					},
				],
			);

			this._form.setItems([
				combo,
				{
					name: "_splitButton",
					showTitle: false,
					type: "canvas",
					canvas: this._splitButton,
					align: "right",
				},
			]);
		} else {
			this._form = isc.DynamicForm.create({
				writeFormTag: false,
				numCols: 1,
				colWidths: ["*"],
			});

			this._form.setItems([combo]);
		}

		this.canvas = this._form;
		this.Super("init", arguments);
	},

	/**
	 * Sets the value of the combo box.
	 * @param {string} newValue - the new value to set.
	 */
	setValue: function (newValue) {
		if (this.canvas != null) {
			if (this.userSetValue) {
				this.bizPicked(this.form, this, newValue);
			} else {
				this._form.getItem("_combo").setValue(newValue);
			}
		}

		this.Super("setValue", [newValue]);
	},

	/**
	 * Focuses the combo box item.
	 */
	focusInItem: function () {
		if (this._form) {
			this._form.getItem("_combo").focusInItem();
		}
	},

	/**
	 * A placeholder method for when a business entity is added.
	 * @param {DynamicForm} form - the form containing the item.
	 * @param {Canvas} item - the item added.
	 * @param {string} value - the value of the item added.
	 */
	bizAdded: function (form, item, value) {
		// No operation - overridden in server-side definition if required
	},

	/**
	 * A placeholder method for when a business entity is edited.
	 * @param {DynamicForm} form - the form containing the item.
	 * @param {Canvas} item - the item edited.
	 * @param {string} value - the value of the item edited.
	 */
	bizEdited: function (form, item, value) {
		// No operation - overridden in server-side definition if required
	},

	/**
	 * A placeholder method for when a business entity is picked.
	 * @param {DynamicForm} form - the form containing the item.
	 * @param {Canvas} item - the item picked.
	 * @param {string} value - the value of the item picked.
	 */
	bizPicked: function (form, item, value) {
		// No operation - overridden in server-side definition if required
	},

	/**
	 * A placeholder method for when a business entity is cleared.
	 * @param {DynamicForm} form - the form containing the item.
	 * @param {Canvas} item - the item cleared.
	 * @param {string} value - the value of the item cleared.
	 */
	bizCleared: function (form, item, value) {
		// No operation - overridden in server-side definition if required
	},

	/**
	 * Sets the value map from the fetched server data.
	 * @param {Object} values The values fetched from the server.
	 */
	setValueMapFromEditView: function (values) {
		this._setValueMap(
			values[this.name],
			values[this.name + "_" + this.displayField],
		);
	},

	/**
	 * Sets the value map from a picked record.
	 * @param {Object} values - the picked record values.
	 */
	setValueMapFromPickList: function (values) {
		this._setValueMap(values.bizId, values[this.displayField]);
	},

	/**
	 * Internal method to set the value map for the combo box.
	 * @param {string} bizId - the business ID.
	 * @param {string} display - the display value.
	 */
	_setValueMap: function (bizId, display) {
		if (bizId) {
			const valueMap = {};
			valueMap[bizId] = display || "<unknown>";
			this._form.getItem("_combo").setValueMap(valueMap);
			this._form.getItem("_combo").setValue(bizId);
		}
	},

	/**
	 * Enables or disables the pick functionality depending on the `canPick` property.
	 * Disables the combo box and split button (if available) when `canPick` is false.
	 *
	 * @returns {void}
	 */
	enableDisablePick: function () {
		// Enable or disable the combo box based on the canPick property
		this._form.getItem("_combo").setDisabled(!this.canPick);

		// If there's a split button (editable), disable its first member based on canPick
		if (this._splitButton) {
			this._splitButton.getMember(0).setDisabled(!this.canPick);
		}
	},

	/**
	 * Zooms into a view, either for a new record or an existing one, based on the `zoomToNew` flag.
	 * The view is populated with parameters passed through `newParams`, and a validation check occurs before zooming.
	 *
	 * @param {boolean} zoomToNew - a flag indicating whether to zoom to a new record (`true`) or an existing one (`false`).
	 * @param {Object|null|undefined} [newParams] - a map of parameter names to expressions to evaluate.
	 *        Can be `null` or `undefined` if no new parameters are needed.
	 * @returns {void}
	 */
	zoom: function (zoomToNew, newParams) {
		const mod = isc.DataSource.get(this.optionDataSource).modoc;
		const dotIndex = mod.indexOf(".");
		const doc = mod.substring(dotIndex + 1);
		const modName = mod.substring(0, dotIndex);

		// Fetch the edit view using BizUtil
		isc.BizUtil.getEditView(modName, doc, (view) => {
			// Determine the view binding for proper view context
			const viewBinding = this._view._b
				? `${this._view._b}.${this.name}`
				: this.name;
			const fromRect = this.getPageRect();

			if (zoomToNew) {
				// Handle case for zooming into a new record
				const required = this.required;
				if (required) {
					this.setRequired(false);
				}

				const instance = this._view.gather(true);
				if (instance) {
					// Apply changes before zooming in
					if (instance._apply || this._view._vm.valuesHaveChanged()) {
						delete instance._apply;
						this._view.saveInstance(true, null, () => {
							this.setRequired(required); // Reset required flag
							isc.WindowStack.popup(fromRect, "New", false, [view]);
							view.newInstance(newParams, viewBinding, instance._c, false);
						});
					} else {
						this.setRequired(required); // Reset required flag
						isc.WindowStack.popup(fromRect, "New", false, [view]);
						view.newInstance(newParams, viewBinding, instance._c, false);
					}
				} else {
					isc.warn("You cannot zoom in until you fix the problems found");
				}
			} else {
				// Handle case for zooming into an existing record
				const instance = this._view.gather(true);
				if (instance) {
					// Apply changes before zooming in
					if (instance._apply || this._view._vm.valuesHaveChanged()) {
						delete instance._apply;
						this._view.saveInstance(true, null, () => {
							isc.WindowStack.popup(fromRect, "Edit", false, [view]);
							view.editInstance(
								this.getValue(),
								viewBinding,
								instance._c,
								false,
							);
						});
					} else {
						isc.WindowStack.popup(fromRect, "Edit", false, [view]);
						view.editInstance(this.getValue(), viewBinding, instance._c, false);
					}
				} else {
					isc.warn("You cannot zoom in until you fix the problems found");
				}
			}
		});
	},
});

isc.SimpleType.create({
	inheritsFrom: "comboBox",
	name: "bizLookupDescription",
	editorType: "BizLookupDescriptionItem",
});

/**
 * Implements the BizCompleteItem widget.
 * Extends ComboBoxItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizCompleteItem", isc.ComboBoxItem);

isc.BizCompleteItem.addProperties({
	optionDataSource: isc.BizUtil.COMPLETE_DATA_SOURCE,
	showPickerIcon: true, // NB false wrecks the style sheet for the picker - no borders etc
	fetchMissingValues: false,
	autoFetchData: false,
	useClientFiltering: false,
	cachePickListResults: false,
	textMatchStyle: "substring",
	valueField: "value",
	displayField: "value",
	selectOnFocus: true,
	completeOnTab: true,
});

isc.BizCompleteItem.addMethods({
	/**
	 * Initializes the `BizCompleteItem` with configuration settings and sets up the pick list properties.
	 *
	 * @param {Object} config - the configuration object passed to initialize the component.
	 * @param {string} config._a - a property used to configure how unknown values are handled. If 'constrain', unknown values are not allowed.
	 * @param {string} config._b - a property used for setting attributes in the request.
	 * @returns {void}
	 */
	init: function (config) {
		const me = this; // Required as parent and child scope is required

		// Check if the '_a' configuration property is 'constrain' and adjust addUnknownValues accordingly
		if (config._a === "constrain") {
			this.addUnknownValues = false;
		}

		// Setup the pick list properties
		this.pickListProperties = {
			/**
			 * Custom filterData method for handling request properties and parameters.
			 *
			 * @param {Object} criteria - the filtering criteria used for the request.
			 * @param {function} callback - the callback function to invoke once data is filtered.
			 * @param {Object} requestProperties - the properties for the request, including parameters.
			 * @returns {void}
			 */
			filterData: function (criteria, callback, requestProperties) {
				// Ensure parameters are included in requestProperties
				if (!requestProperties) {
					requestProperties = {};
				}
				if (!requestProperties.params) {
					requestProperties.params = {};
				}

				// Set additional parameters for the request
				requestProperties.params._attr = config._b;
				requestProperties.params._a = config._a;

				// These parameters are required by Bizlet.complete() on the server-side (if defined)
				requestProperties.params._c = me.form._view.gather(false)._c;
				if (me.form._view._b) {
					requestProperties.params._b = me.form._view._b;
				}

				// Call the superclass's filterData method
				this.Super("filterData", arguments);
			},
		};

		// Call the superclass's init method to complete the initialization
		this.Super("init", arguments);
	},
});

isc.SimpleType.create({
	inheritsFrom: "comboBox",
	name: "bizComplete",
	editorType: "BizCompleteItem",
});

// CKEditor is loaded on the fly when required
// NB: This is declared with 'var' to ensure global scope
var CKEDITOR = null;

/**
 * Implements the BizHTMLItem widget.
 * Extends CanvasItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("BizHTMLItem", isc.CanvasItem);

isc.BizHTMLItem.addProperties({
	width: "*",
	height: "200px",
	rowSpan: "*",
	endRow: false,
	startRow: false,
	canFocus: true,
	shouldSaveValue: true, // This is an editable data item
});

isc.BizHTMLItem.addMethods({
	/**
	 * Initializes the BizHTMLItem with a configured HTML pane and an edit button to manage HTML content.
	 * Sets up CKEditor for editing HTML content if available and configures mention markers if defined.
	 *
	 * @param {Object} config - the configuration object for initializing the BizHTMLItem.
	 * @param {number} config.width - the width of the HTML pane.
	 * @param {number} config.height - the height of the HTML pane.
	 * @param {string} [config.mentionMarkers] - a comma-separated list of mention markers to configure CKEditor mentions.
	 * @returns {void}
	 */
	init: function (config) {
		const me = this; // Required as parent and child scope is required

		// Create the HTML pane for displaying content
		this._pane = isc.HTMLPane.create({
			width: config.width,
			height: config.height,
			isGroup: true,
			styleName: "bizhubRoundedBorder",
			groupBorderCSS: "1px solid #bfbfbf",
			margin: 1,
			groupLabelBackgroundColor: "transparent",
			padding: 10,
			contents: "Empty",
		});

		// Create an edit button for triggering the HTML editor (CKEditor)
		this._editButton = isc.IButton.create({
			width: 40,
			title: "Edit",
			canHover: true,
			getHoverHTML() {
				return "Edit the HTML";
			},
			/**
			 * Handles the click event to toggle the CKEditor for editing.
			 * Loads CKEditor script if not already loaded.
			 *
			 * @param {Event} editEvent - the event triggered when the edit button is clicked.
			 * @returns {void}
			 */
			click: function (editEvent) {
				if (CKEDITOR) {
					this._show();
				} else {
					SKYVE.Util.loadJS("ckeditor/ckeditor.js", function () {
						me._editButton._show();
					});
				}
			},

			/**
			 * Displays the CKEditor inside a newly created holder canvas.
			 * Configures CKEditor and initializes mention markers if defined in config.
			 *
			 * @returns {isc.Canvas} - the holder canvas containing the CKEditor instance.
			 */
			_show: () => {
				const owningView = this.form._view;
				const formValues = owningView.gather(false);

				const holder = isc.Canvas.create({
					width: "100%",
					height: "100%",
					margin: 5,
				});
				holder.setContents(
					'<div style="width:100%;height:100%" id="_CKEditor"></div>',
				);
				holder.draw = function () {
					this.Super("draw");

					// Set CKEditor configuration
					CKEDITOR.config.filebrowserImageBrowseUrl =
						"pages/htmlEdit/browseImages.jsp?_doc=" +
						owningView._mod +
						"." +
						owningView._doc +
						"&_id=" +
						formValues["bizId"];
					CKEDITOR.config.filebrowserBrowseUrl =
						"pages/htmlEdit/browseDocuments.jsp?_doc=" +
						owningView._mod +
						"." +
						owningView._doc +
						"&_id=" +
						formValues["bizId"];
					CKEDITOR.config.resize_enabled = false;
					CKEDITOR.config.skin = "moono-lisa";
					CKEDITOR.config.autoUpdateElement = false;
					CKEDITOR.config.baseFloatZIndex = 9000000;
					CKEDITOR.config.toolbar = [
						["Source", "-", "NewPage", "Preview"],
						[
							"Cut",
							"Copy",
							"Paste",
							"PasteText",
							"PasteFromWord",
							"-",
							"Print",
							"SpellChecker",
							"Scayt",
						],
						[
							"Undo",
							"Redo",
							"-",
							"Find",
							"Replace",
							"-",
							"SelectAll",
							"RemoveFormat",
						],
						"/",
						[
							"Bold",
							"Italic",
							"Underline",
							"Strike",
							"-",
							"Subscript",
							"Superscript",
						],
						[
							"NumberedList",
							"BulletedList",
							"-",
							"Outdent",
							"Indent",
							"Blockquote",
							"CreateDiv",
						],
						["JustifyLeft", "JustifyCenter", "JustifyRight", "JustifyBlock"],
						["Link", "Unlink", "Anchor"],
						[
							"Image",
							"Flash",
							"Table",
							"HorizontalRule",
							"Smiley",
							"SpecialChar",
							"PageBreak",
						],
						"/",
						["Format", "Font", "FontSize"],
						["TextColor", "BGColor"],
						["Maximize", "ShowBlocks"],
					];

					// Configure mention markers if provided in config
					if (config.mentionMarkers) {
						const feed = function (options, callback) {
							const values = me.form._view.gather(false);
							const requestProperties = {
								params: { _attr: me.name, _a: "suggest", _c: values._c },
							};
							if (me.form._view._b) {
								requestProperties.params._b = me.form._view._b;
							}

							isc.BizUtil.COMPLETE_DATA_SOURCE.fetchData(
								{ value: options.marker + options.query },
								function (response) {
									if (response && response.data) {
										for (let i = 0, l = response.data.length; i < l; i++) {
											const data = response.data[i];
											data.id = i;
											data.name = data.value.substring(1);
										}
										callback(response.data);
									}
								},
								requestProperties,
							);
						};

						const markers = config.mentionMarkers.split(",");
						const mentions = [];
						for (let i = 0, l = markers.length; i < l; i++) {
							const marker = markers[i];
							if (marker.length > 1) {
								const first = marker.substring(0, 1);
								const second = marker.substring(1, 2);
								mentions.add({
									feed: feed,
									marker: first,
									minChars: 0,
									pattern: new RegExp(
										"\\" + first + "[^\\" + first + "\\" + second + "]*$",
									),
									cache: false,
									throttle: 500,
								});
							} else {
								mentions.add({
									feed: feed,
									marker: marker,
									minChars: 0,
									cache: false,
									throttle: 500,
								});
							}
						}
						CKEDITOR.config.mentions = mentions;
					}

					// Set styles for bold and italic to align with Jasper report styles
					CKEDITOR.config.coreStyles_bold = {
						element: "b",
						overrides: "strong",
					};
					CKEDITOR.config.coreStyles_italic = { element: "i", overrides: "em" };

					me._editor = CKEDITOR.replace("_CKEditor", {
						customConfig: SKYVE.Util.ckEditorConfigFileUrl,
					});
					me._editor.setData(me.getValue());

					return this;
				};

				// Open the CKEditor popup window
				isc.WindowStack.popup(
					null,
					"Edit HTML",
					true,
					[
						holder,
						isc.HLayout.create({
							membersMargin: 5,
							margin: 5,
							align: "right",
							members: [
								isc.IButton.create({
									width: 60,
									title: "Apply",
									click: (applyEvent) => {
										this.setValue(this._editor.getData());
										isc.WindowStack.popoff(false);
									},
								}),
								isc.IButton.create({
									width: 60,
									title: "Cancel",
									click: function (applyEvent) {
										isc.WindowStack.popoff(false);
									},
								}),
							],
						}),
					],
					420,
				);
			},
		});

		// Create the canvas layout containing the HTML pane and edit button
		this.canvas = isc.HLayout.create({
			defaultLayoutAlign: "center",
			members: [
				this._pane,
				isc.LayoutSpacer.create({ width: 3 }),
				this._editButton,
			],
		});

		// Call the superclass's init method
		this.Super("init", arguments);
	},

	/**
	 * Sets the value of the HTML content and updates the displayed HTML in the pane.
	 *
	 * @param {string} newValue - the new value to set for the HTML content.
	 * @returns {string} - the new value after setting it.
	 */
	setValue: function (newValue) {
		if (this.canvas != null && !this.userSetValue) {
			if (newValue) {
				this._pane.setContents(newValue);
			} else {
				this._pane.setContents(" "); // Avoid using an empty string
			}
		}
		return this.Super("setValue", [newValue]);
	},
});

isc.SimpleType.create({
	inheritsFrom: "canvas",
	name: "bizHTML",
	editorType: "BizHTMLItem",
});

/**
 * Implements the GeometryItem widget.
 * Extends CanvasItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("GeometryItem", isc.CanvasItem);

isc.GeometryItem.addClassProperties({
	validOperators: [
		"geoWithin",
		"geoContains",
		"geoOverlaps",
		"geoDisjoint",
		"geoIntersects",
		"geoTouches",
		"geoCrosses",
		"geoEquals",
		"isNull",
		"notNull",
	],
});

isc.GeometryItem.addClassMethods({
	/**
	 * Formats the geometry value to a readable string.
	 *
	 * @param {string} value - the geometry value to format.
	 * @returns {string} the formatted geometry type (e.g., 'Point', 'Line', 'Polygon') or an empty string if the value does not match any known geometry types.
	 */
	format: function (value) {
		if (value) {
			// Check for different geometry types and return corresponding labels
			if (value.startsWith("POINT")) {
				return "Point";
			} else if (value.startsWith("LINESTRING")) {
				return "Line";
			} else if (value.startsWith("POLYGON")) {
				return "Polygon";
			}
		}

		// Return an empty string if no match is found
		return "";
	},
});

isc.GeometryItem.addProperties({
	showHint: false,
	width: 100,
	operator: "geoWithin",
	validOperators: isc.GeometryItem.validOperators,
	shouldSaveValue: true, // This is an editable data item
});

isc.GeometryItem.addMethods({
	/**
	 * Creates the canvas layout for the geometry item.
	 *
	 * This method builds a canvas containing a DynamicForm with a text item, which displays a value
	 * and allows interaction with a map picker to set or view geometry.
	 *
	 * @returns {isc.HLayout} the created layout containing the DynamicForm and map picker functionality.
	 */
	createCanvas: function () {
		// Icons configuration for interaction with the map
		const icons = [
			{
				src: "icons/map.png",
				prompt: "Click to set or see the geometry on a map",
				click: (form, item, icon) => {
					const options = this.drawingTools
						? { field: this, drawingTools: this.drawingTools }
						: { field: this };
					isc.WindowStack.popup(item.getPageRect(), "Map", true, [
						isc.BizMapPicker.create(options),
					]);
				},
			},
		];

		// Return a HLayout to avoid form nesting within filter criteria
		return isc.HLayout.create({
			height: 1,
			width: "100%",
			members: [
				isc.DynamicForm.create({
					writeFormTag: false, // Prevent nested forms
					numCols: 1,
					width: "100%",
					margin: 0,
					cellPadding: 0,
					items: [
						{
							name: "value",
							type: "text",
							showTitle: false,
							width: "*",
							hint: this.hint,
							showHintInField: true,
							selectOnFocus: true,
							changed: (form, item, value) => {
								this.storeValue(value, false);
							},
							icons: icons,
						},
					],
				}),
			],
		});
	},

	/**
	 * Displays a value on the geometry item.
	 *
	 * This method sets the value of the text field in the canvas to the provided display value.
	 *
	 * @param {string} displayValue - the value to be displayed in the text field.
	 * @param {string} dataValue - the raw data value.
	 * @param {isc.DynamicForm} form - the form containing the item.
	 * @param {isc.FormItem} item - the form item to update.
	 */
	showValue: function (displayValue, dataValue, form, item) {
		item.canvas.getMember(0).setValue("value", displayValue);
	},

	/**
	 * Sets the hint for the geometry item.
	 *
	 * This method updates the hint text displayed in the input field.
	 *
	 * @param {string} hint - the hint text to set for the item.
	 */
	setHint: function (hint) {
		this.canvas.getMember(0).setHint(hint);
	},

	/**
	 * Sets the value from the BizMapPicker.
	 *
	 * This method is invoked from the BizMapPicker to store the selected value.
	 * It also triggers the change events.
	 *
	 * @param {string} newValue - the new value to store in the geometry item.
	 */
	setValueFromPicker: function (newValue) {
		// Store the new value and trigger change events
		this.storeValue(newValue, true);
	},
});

isc.SimpleType.create({
	name: "geometry",
	inheritsFrom: "canvas",
	editorType: "GeometryItem",
	defaultOperator: "gWithin",
	validOperators: isc.GeometryItem.validOperators,
});

/**
 * Implements the GeometryMapItem widget.
 * Extends CanvasItem from the SmartClient library.
 */
isc.ClassFactory.defineClass("GeometryMapItem", isc.CanvasItem);

isc.GeometryMapItem.addProperties({
	width: "*",
	rowSpan: "*",
	endRow: false,
	startRow: false,
	canFocus: false,
	shouldSaveValue: true, // This is an editable data item
});

isc.GeometryMapItem.addMethods({
	/**
	 * Creates the canvas layout for the geometry map item.
	 *
	 * This method returns a centered HLayout for the map item canvas.
	 *
	 * @returns {isc.HLayout} The created HLayout for the geometry map item.
	 */
	createCanvas: function () {
		return isc.HLayout.create({ defaultLayoutAlign: "center" });
	},

	/**
	 * Displays a value on the geometry map item.
	 *
	 * This method sets up a map picker widget inside the canvas if it has not been set from the picker already.
	 * It also configures options based on the width, height, and available drawing tools.
	 *
	 * @param {string} displayValue - the display value for the geometry map item (currently not used in this method).
	 * @param {string} dataValue - the raw data value (currently not used in this method).
	 */
	showValue: function (displayValue, dataValue) {
		// If value has not been set from the picker already, initialize the map picker
		if (!this._valueSetFromPicker) {
			const options = {
				field: this,
				width: this.width || "100%", // Use provided width or default to '100%'
				height: this.height || "100%", // Use provided height or default to '100%'
			};

			// Add drawing tools if available
			if (this.drawingTools) {
				options.drawingTools = this.drawingTools;
			}

			// Set the map picker as a member of the canvas
			this.canvas.setMembers([isc.BizMapPicker.create(options)]);
		}
	},

	/**
	 * Disables or enables the geometry map item.
	 *
	 * This method sets the disabled state of the map picker inside the canvas.
	 *
	 * @param {boolean} disabled - whether to disable the item (true) or enable it (false).
	 */
	setDisabled: function (disabled) {
		this.canvas.getMember(0).setDisabled(disabled);
	},

	/**
	 * Sets the value from the BizMapPicker.
	 *
	 * This method is called from the BizMapPicker to store the selected value, trigger the `changed` event,
	 * and ensure that the value is only set once from the picker.
	 *
	 * @param {string} newValue - the new value selected from the BizMapPicker.
	 */
	setValueFromPicker: function (newValue) {
		this._valueSetFromPicker = true;
		this.setValue(newValue); // Store the selected value
		delete this._valueSetFromPicker;

		// Trigger the `changed` event if defined
		if (this.changed) {
			this.changed(this.form, this, newValue);
		}
	},
});

isc.SimpleType.create({
	name: "geometryMap",
	inheritsFrom: "canvas",
	editorType: "GeometryMapItem",
	validOperators: isc.GeometryItem.validOperators,
});
