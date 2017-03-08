//isc.Time.setDefaultDisplayTimezone('+00:00');
//isc.Time.adjustForDST = false;

// Define our extra date and time formats
isc.addMethods(Date.prototype, {
	toDD_MM_YYYY: function() {
		return this.getDate().stringify() + '/' + (this.getMonth() + 1).stringify() + '/' + this.getFullYear();
	},
	toDD_MMM_YYYY: function() {
		var shortMonthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
		return this.getDate().stringify() + '-' + shortMonthNames[this.getMonth()] + '-' + this.getFullYear();
	},
	toDD_MM_YYYY_HH_MI: function() {
		return this.toDD_MM_YYYY() + ' ' + this.getHours().stringify() + ":" + this.getMinutes().stringify();
	},
	toDD_MMM_YYYY_HH_MI: function() {
		return this.toDD_MMM_YYYY() + ' ' + this.getHours().stringify() + ":" + this.getMinutes().stringify();
	},
	toDD_MM_YYYY_HH_MI_SS: function() {
		return this.toDD_MM_YYYY_HH_MI() + ':' + this.getSeconds().stringify();
	},
	toDD_MMM_YYYY_HH_MI_SS: function() {
		return this.toDD_MMM_YYYY_HH_MI() + ':' + this.getSeconds().stringify();
	}
});

// Ensure that DateItem calendar picker works in UTC
/*
isc.DateTimeItem.addProperties({
	pickerDataChanged : function (picker) {
        var date = picker.getData();

        // avoid firing 'updateValue' while setting the values of sub items
        this._suppressUpdates = true;

        if (this.useTextField) {
//            date = new Date(date.getUTCFullYear(), date.getUTCMonth(), date.getUTCDate(), date.getUTCHours(), date.getUTCMinutes(), date.getUTCSeconds());
            this.dateTextField.setValue(date.toShortDate(this.displayFormat));
        } else {
            var year = date.getUTCFullYear(),
	            month = date.getUTCMonth(),
	            day = date.getUTCDate();
            if (this.yearSelector) this.yearSelector.setValue(year);
            if (this.monthSelector) this.monthSelector.setValue(month);
            if (this.daySelector) this.daySelector.setValue(day);
        }
        this._suppressUpdates = false;
        
        // Explicitly call 'updateValue' to save the new date 
        // (handles firing change handlers, etc. too)
        this.updateValue();
        
        // Ensure we have focus
        if (! this.hasFocus) this.focusInItem();
	}
});
*/
// Define our type editors here

isc.ClassFactory.defineClass("BizDateItem", "DateItem");
isc.BizDateItem.addClassMethods({
	parseInput: function(value) { // value is not only a string, returns a float
		if (isc.isA.Date(value)) {
			return value;
		}
		
		if (value) {
			value = value.trim();
			
			// look for a string month match
			var match = /^\d?\d[^A-Za-z0-9]([A-Za-z]+).*$/.exec(value); // 1 or 2 digits, anything, month word
			if (match) {
				var month = match[1];
				var lowerMonth = month.toLowerCase();
				if (lowerMonth.startsWith('jan')) {
					value = value.replaceAll(month, '1');
				}
				else if (lowerMonth.startsWith('feb')) {
					value = value.replaceAll(month, '2');
				}
				else if (lowerMonth.startsWith('mar')) {
					value = value.replaceAll(month, '3');
				}
				else if (lowerMonth.startsWith('apr')) {
					value = value.replaceAll(month, '4');
				}
				else if (lowerMonth.startsWith('may')) {
					value = value.replaceAll(month, '5');
				}
				else if (lowerMonth.startsWith('jun')) {
					value = value.replaceAll(month, '6');
				}
				else if (lowerMonth.startsWith('jul')) {
					value = value.replaceAll(month, '7');
				}
				else if (lowerMonth.startsWith('aug')) {
					value = value.replaceAll(month, '8');
				}
				else if (lowerMonth.startsWith('sep')) {
					value = value.replaceAll(month, '9');
				}
				else if (lowerMonth.startsWith('oct')) {
					value = value.replaceAll(month, '10');
				}
				else if (lowerMonth.startsWith('nov')) {
					value = value.replaceAll(month, '11');
				}
				else if (lowerMonth.startsWith('dec')) {
					value = value.replaceAll(month, '12');
				}
			}

			var result = Date.parseSchemaDate(value);
			if (result) {
			}
			else {
				// any non-matches returns null
				result = Date.parseInput(value, 'DMY', 50, true);
				// check if we have just a day and month, no year or time etc
				if (result) {
				}
				else {
					if (/^\d?\d[^A-Za-z0-9]\d?\d$/.test(value)) { // 1 or 2 digits, anything, month word, whitespace or nothing
						// try adding the current year on the end - any non-matches returns null
						result = Date.parseInput(value + ' ' + Date.create().getFullYear(), 'DMY', 50, true);
					}
				}
			}
			// Adjust time to UTC
//			if (result) {
//				result.setTime(result.getTime() - result.getTimezoneOffset() * 60000);
//				result.set
//			}

			return result;
		}
		else {
			return null;
		}
	},
	format: function(value, format) { // value is not only a date, returns a String (if value is a date)
		if (! isc.isA.Date(value)) {
			return value;
		}

		return eval("value." + format + "()");
	}
});
isc.BizDateItem.addProperties({
	enforceDate: true,
	changeOnBlur: true, // perform validation on field blur
    changeOnKeypress: false, // don't perform validation on key press
    width: 100,
	useTextField: true,
    showHint: true,
    showHintInField: true,
   	hint: 'DD MM(M) YY(YY)',
   	textFieldProperties: {selectOnFocus: true},
   	showPickerTimeItem: false,
	inputFormat: function(value) {
		return isc.BizDateItem.parseInput(value);
	},
	displayFormat: 'toDD_MM_YYYY'
});
isc.BizDateItem.addMethods({
	mapValueToDisplay: function(value) {
		if (value == null) {
			return isc.emptyString;
		}
		if (! isc.isA.Date(value)) {
			value = isc.BizDateItem.parseInput(value);
		}
		return isc.BizDateItem.format(value, this.displayFormat);
	},
	mapDisplayToValue: function(value) {
		if (isc.isAn.emptyString(value)) {
			value = null;
		}
		else {
			value = isc.BizDateItem.parseInput(value);
		}
		
		return value;
	},
	// Override - ensure the value displayed in the element matches the saved value, and is formatted correctly
	updateValue: function() {
		// this will map the value to a valid float
        this.Super("updateValue", arguments);
        // this will update the displayed string so its formatted correctly
        this.setElementValue(this.mapValueToDisplay(this.getValue()));
	},
	// Override - if passed a string, map it to the appropriate date before saving
    // (this is required since the string passed in won't go through 'mapDisplayToValue')
    setValue : function (newValue) {
        if (isc.isA.String(newValue)) {
        	newValue = isc.BizDateItem.parseInput(newValue);
        }
        return this.Super("setValue", [newValue]);
    }
});
isc.SimpleType.create({
	name: "bizDate",
	inheritsFrom: "date",
	editorType: "BizDateItem",
	editFormatter: function(internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},
	normalDisplayFormatter: function(internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, 'toDD_MM_YYYY');
	}//,
//	validators: [{type: 'custom', errorMessage: "Please enter a date", clientOnly: true, condition: '(value == null) || isc.isA.Date(value)'}]
});
isc.SimpleType.create({
	name: "DD_MM_YYYY",
	inheritsFrom: "bizDate"
});

isc.ClassFactory.defineClass("DD_MMM_YYYY_Item", "BizDateItem");
isc.DD_MMM_YYYY_Item.addProperties({
	displayFormat: 'toDD_MMM_YYYY'
});
isc.SimpleType.create({
	name: "DD_MMM_YYYY",
	inheritsFrom: "bizDate",
	editorType: 'DD_MMM_YYYY_Item',
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, 'toDD_MMM_YYYY');
	}
});

isc.ClassFactory.defineClass("BizDateTimeItem", "DateTimeItem");
isc.BizDateTimeItem.addProperties({
	enforceDate: true,
	changeOnBlur: true, // perform validation on field blur
    changeOnKeypress: false, // don't perform validation on key press
    width: 200,
	useTextField: true,
    showHint: true,
    showHintInField: true,
   	hint: 'DD MM(M) YY(YY) HH(24):MI',
   	textFieldProperties: {selectOnFocus: true},
	showPickerTimeItem: true,
	pickerTimeItemProperties: {showSecondItem: false, use24HourTime: false},
	inputFormat: function(value) {
		return isc.BizDateItem.parseInput(value);
	},
	displayFormat: 'toDD_MM_YYYY_HH_MI'
});
isc.BizDateTimeItem.addMethods({
	mapValueToDisplay: function(value) {
		if (value == null) {
			return isc.emptyString;
		}
		if (! isc.isA.Date(value)) {
			value = isc.BizDateItem.parseInput(value);
		}
		return isc.BizDateItem.format(value, this.displayFormat);
	},
	mapDisplayToValue: function(value) {
		if (isc.isAn.emptyString(value)) {
			value = null;
		}
		else {
			value = isc.BizDateItem.parseInput(value);
		}
		
		return value;
	},
	// Override - ensure the value displayed in the element matches the saved value, and is formatted correctly
	updateValue: function() {
		// this will map the value to a valid float
        this.Super("updateValue", arguments);
        // this will update the displayed string so its formatted correctly
        this.setElementValue(this.mapValueToDisplay(this.getValue()));
	},
	// Override - if passed a string, map it to the appropriate date before saving
    // (this is required since the string passed in won't go through 'mapDisplayToValue')
    setValue : function (newValue) {
        if (isc.isA.String(newValue)) {
        	newValue = isc.BizDateItem.parseInput(newValue);
        }
        return this.Super("setValue", [newValue]);
    }
});
isc.SimpleType.create({
	name: "bizDateTime",
	inheritsFrom: "dateTime",
	editorType: "BizDateTimeItem",
	editFormatter: function(internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},
	normalDisplayFormatter: function(internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, 'toDD_MM_YYYY_HH_MI');
	}//,
//	validators: [{type: 'custom', errorMessage: "Please enter a date", clientOnly: true, condition: '(value == null) || isc.isA.Date(value)'}]
});
isc.SimpleType.create({
	name: "DD_MM_YYYY_HH_MI",
	inheritsFrom: "bizDateTime"
});
isc.ClassFactory.defineClass("DD_MM_YYYY_HH24_MI_Item", "BizDateTimeItem");
isc.DD_MM_YYYY_HH24_MI_Item.addProperties({
	displayFormat: 'toDD_MM_YYYY_HH_MI',
	pickerTimeItemProperties: {showSecondItem: false, use24HourTime: true}
});
isc.SimpleType.create({
	name: "DD_MM_YYYY_HH24_MI",
	inheritsFrom: "bizDateTime",
	editorType: 'DD_MM_YYYY_HH24_MI_Item',
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, 'toDD_MM_YYYY_HH_MI');
	}
});

isc.ClassFactory.defineClass("DD_MMM_YYYY_HH_MI_Item", "BizDateTimeItem");
isc.DD_MMM_YYYY_HH_MI_Item.addProperties({
	displayFormat: 'toDD_MMM_YYYY_HH_MI',
	pickerTimeItemProperties: {showSecondItem: false, use24HourTime: false}
});
isc.SimpleType.create({
	name: "DD_MMM_YYYY_HH_MI",
	inheritsFrom: "bizDateTime",
	editorType: 'DD_MMM_YYYY_HH_MI_Item',
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, 'toDD_MMM_YYYY_HH_MI');
	}
});
isc.ClassFactory.defineClass("DD_MMM_YYYY_HH24_MI_Item", "BizDateTimeItem");
isc.DD_MMM_YYYY_HH24_MI_Item.addProperties({
	displayFormat: 'toDD_MMM_YYYY_HH_MI',
	pickerTimeItemProperties: {showSecondItem: false, use24HourTime: true}
});
isc.SimpleType.create({
	name: "DD_MMM_YYYY_HH24_MI",
	inheritsFrom: "bizDateTime",
	editorType: 'DD_MMM_YYYY_HH24_MI_Item',
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, 'toDD_MMM_YYYY_HH_MI');
	}
});

isc.ClassFactory.defineClass("DD_MM_YYYY_HH_MI_SS_Item", "BizDateTimeItem");
isc.DD_MM_YYYY_HH_MI_SS_Item.addProperties({
	displayFormat: 'toDD_MM_YYYY_HH_MI_SS',
   	hint: 'DD MM(M) YY(YY) HH(24):MI(:SS)',
	pickerTimeItemProperties: {showSecondItem: true, use24HourTime: false}
});
isc.SimpleType.create({
	name: "DD_MM_YYYY_HH_MI_SS",
	inheritsFrom: "bizDateTime",
	editorType: 'DD_MM_YYYY_HH_MI_SS_Item',
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, 'toDD_MM_YYYY_HH_MI_SS');
	}
});
isc.ClassFactory.defineClass("DD_MM_YYYY_HH24_MI_SS_Item", "BizDateTimeItem");
isc.DD_MM_YYYY_HH24_MI_SS_Item.addProperties({
	displayFormat: 'toDD_MM_YYYY_HH_MI_SS',
   	hint: 'DD MM(M) YY(YY) HH(24):MI(:SS)',
	pickerTimeItemProperties: {showSecondItem: true, use24HourTime: true}
});
isc.SimpleType.create({
	name: "DD_MM_YYYY_HH24_MI_SS",
	inheritsFrom: "bizDateTime",
	editorType: 'DD_MM_YYYY_HH24_MI_SS_Item',
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, 'toDD_MM_YYYY_HH_MI_SS');
	}
});

isc.ClassFactory.defineClass("DD_MMM_YYYY_HH_MI_SS_Item", "BizDateTimeItem");
isc.DD_MMM_YYYY_HH_MI_SS_Item.addProperties({
	displayFormat: 'toDD_MMM_YYYY_HH_MI_SS',
   	hint: 'DD MM(M) YY(YY) HH(24):MI(:SS)',
	pickerTimeItemProperties: {showSecondItem: true, use24HourTime: false}
});
isc.SimpleType.create({
	name: "DD_MMM_YYYY_HH_MI_SS",
	inheritsFrom: "bizDateTime",
	editorType: 'DD_MMM_YYYY_HH_MI_SS_Item',
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, 'toDD_MMM_YYYY_HH_MI_SS');
	}
});
isc.ClassFactory.defineClass("DD_MMM_YYYY_HH24_MI_SS_Item", "BizDateTimeItem");
isc.DD_MMM_YYYY_HH24_MI_SS_Item.addProperties({
	displayFormat: 'toDD_MMM_YYYY_HH_MI_SS',
   	hint: 'DD MM(M) YY(YY) HH(24):MI(:SS)',
	pickerTimeItemProperties: {showSecondItem: true, use24HourTime: true}
});
isc.SimpleType.create({
	name: "DD_MMM_YYYY_HH24_MI_SS",
	inheritsFrom: "bizDateTime",
	editorType: 'DD_MMM_YYYY_HH24_MI_SS_Item',
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, 'toDD_MMM_YYYY_HH_MI_SS');
	}
});

/* Override the init method so it does NOT set the time formatter.
 * It seems that the time formatter cannot be set to something else after construction of the time item.
 */
isc.TimeItem.addMethods({
	init: function() {
//	    if (!this.use24HourTime) {
//	        this.timeFormatter = "toShortTime"
//	    }
	    this.Super("init", arguments);
	}
});

isc.ClassFactory.defineClass("BizTimeItem", "TimeItem");
isc.BizTimeItem.addProperties({
	useTextField: true,
	use24HourTime: false,
	showHint: true,
    showHintInField: true,
   	hint: 'HH:MI am/pm',
   	textFieldProperties: {selectOnFocus: true},
   	displayFormat: 'toShortTime',
   	timeFormatter: 'toShortTime'
});
isc.BizTimeItem.addMethods({
    setValue : function (newValue) {
        if (isc.isA.String(newValue)) {
        	newValue = isc.Time.parseInput(newValue);
        }
        return this.Super("setValue", [newValue]);
    },
    hasAdvancedCriteria: function() {
		return false;
    }
});

isc.SimpleType.create({
	name: "bizTime",
	inheritsFrom: "time",
	editorType: "BizTimeItem",
	editFormatter: function(internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},
	normalDisplayFormatter: function(internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.Time.toShortTime(internalValue, 'toShortTime', true);
	}
});
isc.SimpleType.create({
	name: "HH_MI",
	inheritsFrom: "bizTime"
});
isc.ClassFactory.defineClass("HH_MI_SS_Item", "BizTimeItem");
isc.HH_MI_SS_Item.addProperties({
	use24HourTime: false,
	hint: 'HH:MI:SS am/pm',
   	displayFormat: 'toTime',
   	timeFormatter: 'toTime'
});
isc.SimpleType.create({
	name: "HH_MI_SS",
	inheritsFrom: "bizTime",
	editorType: 'HH_MI_SS_Item',
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.Time.toTime(internalValue, 'toTime', true);
	}
});
isc.ClassFactory.defineClass("HH24_MI_Item", "BizTimeItem");
isc.HH24_MI_Item.addProperties({
	use24HourTime: true,
	hint: 'HH24:MI',
   	displayFormat: 'toShortPadded24HourTime',
   	timeFormatter: 'toShortPadded24HourTime'
});
isc.SimpleType.create({
	name: "HH24_MI",
	inheritsFrom: "bizTime",
	editorType: 'HH24_MI_Item',
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.Time.toShortTime(internalValue, 'toShortPadded24HourTime', true);
	}
});
isc.ClassFactory.defineClass("HH24_MI_SS_Item", "BizTimeItem");
isc.HH24_MI_SS_Item.addProperties({
	use24HourTime: true,
	hint: 'HH24:MI:SS',
   	displayFormat: 'toPadded24HourTime',
   	timeFormatter: 'toPadded24HourTime'
});
isc.SimpleType.create({
	name: "HH24_MI_SS",
	inheritsFrom: "bizTime",
	editorType: 'HH24_MI_SS_Item',
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.Time.toTime(internalValue, 'toPadded24HourTime', true);
	}
});

isc.ClassFactory.defineClass("BizDecimal2Item", "TextItem");
isc.BizDecimal2Item.addClassMethods({
	parseInput: function(value) { // value is not only a string, returns a float
		if (isc.isA.Number(value)) {
			return value;
		}
		
		if (value) {
			value = value.toString().trim(' ').replace(/\$|\,/g,'');
			if (isNaN(value)) {
				return '';
			}
			else {
				var result = parseFloat(value);
				if (result >= 1000000000000000000.0) {
					result = '';
				}
				return result;
			}
		}
		else {
			return '';
		}
	},
	// value - the value to format
	// decimalPlaces - the number of decimal places to format to
	format: function(value, decimalPlaces) { // value is not only a float, returns a String (if value is a float)
		if (! isc.isA.Number(value)) {
			return value;
		}

		return value.toLocalizedString(decimalPlaces, '.', ',', '-');
	}
});
isc.BizDecimal2Item.addProperties({
	changeOnBlur: true, // perform validation on field blur
    changeOnKeypress: false, // dont perform validation on key press
    width: 100,
    showHint: false,
    selectOnFocus: true,
    decimalPlaces: 2
});
isc.BizDecimal2Item.addMethods({
	mapValueToDisplay: function(value) {
		if (value == null) {
			return isc.emptyString;
		}
		if (! isc.isA.Number(value)) {
			value = isc.BizDecimal2Item.parseInput(value);
		}
		return isc.BizDecimal2Item.format(value, this.decimalPlaces);
	},
	mapDisplayToValue: function(value) {
		if (isc.isAn.emptyString(value)) {
			value = null;
		}
		else {
			value = isc.BizDecimal2Item.parseInput(value);
		}
		
		return value;
	},
	// Override - ensure the value displayed in the element matches the saved value, and is formatted correctly
	updateValue: function() {
		// this will map the value to a valid float
        this.Super("updateValue", arguments);
        // this will update the displayed string so its formatted correctly
        this.setElementValue(this.mapValueToDisplay(this.getValue()));
	},
	// Override - if passed a string, map it to the appropriate float before saving
    // (this is required since the string passed in won't go through 'mapDisplayToValue')
    setValue : function (newValue) {
        if (isc.isA.String(newValue)) {
        	newValue = isc.BizDecimal2Item.parseInput(newValue);
        }
        return this.Super("setValue", [newValue]);
    }
});
isc.SimpleType.create({
	name: "bizDecimal2",
	inheritsFrom: "float",
	editorType: "BizDecimal2Item",
	editFormatter: function(internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},
	normalDisplayFormatter: function(internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDecimal2Item.format(internalValue, 2);
	},
	validators: [{type: 'custom', clientOnly: true, condition: '(value == null) || isc.isA.Number(value)'}]
});

isc.ClassFactory.defineClass("BizDecimal5Item", "BizDecimal2Item");
isc.BizDecimal5Item.addProperties({
    decimalPlaces: 5
});
isc.SimpleType.create({
	name: "bizDecimal5",
	inheritsFrom: "float",
	editorType: "BizDecimal5Item",
	editFormatter: function(internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, value);
	},
	normalDisplayFormatter: function(internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDecimal2Item.format(internalValue, 5);
	},
	validators: [{type: 'custom', clientOnly: true, condition: '(value == null) || isc.isA.Number(value)'}]
});

isc.ClassFactory.defineClass("BizDecimal10Item", "BizDecimal2Item");
isc.BizDecimal10Item.addProperties({
    decimalPlaces: 10
});
isc.SimpleType.create({
	name: "bizDecimal10",
	inheritsFrom: "float",
	editorType: "BizDecimal10Item",
	editFormatter: function(internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},
	normalDisplayFormatter: function(internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDecimal2Item.format(internalValue, 10);
	},
	validators: [{type: 'custom', clientOnly: true, condition: '(value == null) || isc.isA.Number(value)'}]
});

isc.ClassFactory.defineClass("BizDecimal0Item", "BizDecimal2Item");
isc.BizDecimal0Item.addProperties({
    decimalPlaces: 0
});
isc.SimpleType.create({
	name: "bizDecimal0",
	inheritsFrom: "float",
	editorType: "BizDecimal0Item",
	editFormatter: function(internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},
	normalDisplayFormatter: function(internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDecimal2Item.format(internalValue, 0);
	},
	validators: [{type: 'custom', clientOnly: true, condition: '(value == null) || isc.isA.Number(value)'}]
});

isc.ClassFactory.defineClass("BizDecimal1Item", "BizDecimal2Item");
isc.BizDecimal1Item.addProperties({
    decimalPlaces: 1
});
isc.SimpleType.create({
	name: "bizDecimal1",
	inheritsFrom: "float",
	editorType: "BizDecimal1Item",
	editFormatter: function(internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},
	normalDisplayFormatter: function(internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDecimal2Item.format(internalValue, 1);
	},
	validators: [{type: 'custom', clientOnly: true, condition: '(value == null) || isc.isA.Number(value)'}]
});

isc.ClassFactory.defineClass("BizDollarsAndCentsItem", "BizDecimal2Item");
isc.BizDollarsAndCentsItem.addProperties({
    showHint: true,
    showHintInField: true,
   	hint: '(+/-)$$$$$.cc'
});
isc.SimpleType.create({
	name: "bizDollarsAndCents",
	inheritsFrom: "float",
	editorType: "BizDollarsAndCentsItem",
	editFormatter: function(internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},
	normalDisplayFormatter: function(internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDollarsAndCentsItem.format(internalValue, 2);
	},
	validators: [{type: 'custom', clientOnly: true, condition: '(value == null) || isc.isA.Number(value)'}]
});

isc.ClassFactory.defineClass("BizIntegerPercentageItem", "TextItem");
isc.BizIntegerPercentageItem.addClassMethods({
	parseInput: function(value) { // value is not only a string, returns a float
		if (isc.isA.Number(value)) {
			return value;
		}
		
		if (value) {
			value = value.toString().trim(' ').trim('%');
			if (isNaN(value)) {
				return null;
			}
			else {
				var result = Math.round(parseFloat(value)) / 100;
				if (result >= 1000000000000000000.0) {
					result = null;
				}
				return result;
			}
		}
		else {
			return null;
		}
	},
	format: function(value) { // value is not only a float, returns a String (if value is a float)
		if (! isc.isA.Number(value)) {
			return value;
		}

		return (value * 100).toLocalizedString(0, '.', ',', '-') + '%';
	}
});
isc.BizIntegerPercentageItem.addProperties({
	changeOnBlur: true, // perform validation on field blur
    changeOnKeypress: false, // dont perform validation on key press
    width: 100,
    showHint: true,
    showHintInField: true,
   	hint: '(+/-)99999',
   	selectOnFocus: true
});
isc.BizIntegerPercentageItem.addMethods({
	mapValueToDisplay: function(value) {
		if (value == null) {
			return isc.emptyString;
		}
		if (! isc.isA.Number(value)) {
			value = isc.BizIntegerPercentageItem.parseInput(value);
		}
		return isc.BizIntegerPercentageItem.format(value);
	},
	mapDisplayToValue: function(value) {
		if (isc.isAn.emptyString(value)) {
			value = null;
		}
		else {
			value = isc.BizIntegerPercentageItem.parseInput(value);
		}
		
		return value;
	},
	// Override - ensure the value displayed in the element matches the saved value, and is formatted correctly
	updateValue: function() {
		// this will map the value to a valid float
        this.Super("updateValue", arguments);
        // this will update the displayed string so its formatted correctly
        this.setElementValue(this.mapValueToDisplay(this.getValue()));
	},
	// Override - if passed a string, map it to the appropriate float before saving
    // (this is required since the string passed in won't go through 'mapDisplayToValue')
    setValue : function (newValue) {
        if (isc.isA.String(newValue)) {
        	newValue = isc.BizIntegerPercentageItem.parseInput(newValue);
        }
        return this.Super("setValue", [newValue]);
    }
});
isc.SimpleType.create({
	name: "bizIntegerPercentage",
	inheritsFrom: "integer",
	editorType: "BizIntegerPercentageItem",
	editFormatter: function(internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},
	normalDisplayFormatter: function(internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizIntegerPercentageItem.format(internalValue);
	},
	validators: [{type: 'custom', clientOnly: true, condition: '(value == null) || isc.isA.Number(value)'}]
});

isc.ClassFactory.defineClass("BizSimplePercentageItem", "BizIntegerPercentageItem");
isc.BizSimplePercentageItem.addClassMethods({
	parseInput: function(value) { // value is not only a string, returns a float
		if (isc.isA.Number(value)) {
			return value;
		}
		
		if (value) {
			value = value.toString().trim(' ').trim('%');
			if (isNaN(value)) {
				return null;
			}
			else {
				var result = Math.round(parseFloat(value));
				if (result >= 1000000000000000000.0) {
					result = null;
				}
				return result;
			}
		}
		else {
			return null;
		}
	},
	format: function(value) { // value is not only a float, returns a String (if value is a float)
		if (! isc.isA.Number(value)) {
			return value;
		}

		return value.toLocalizedString(0, '.', ',', '-') + '%';
	}
});
isc.BizSimplePercentageItem.addMethods({
	mapValueToDisplay: function(value) {
		if (value == null) {
			return isc.emptyString;
		}
		if (! isc.isA.Number(value)) {
			value = isc.BizSimplePercentageItem.parseInput(value);
		}
		return isc.BizSimplePercentageItem.format(value);
	},
	mapDisplayToValue: function(value) {
		if (isc.isAn.emptyString(value)) {
			value = null;
		}
		else {
			value = isc.BizSimplePercentageItem.parseInput(value);
		}
		
		return value;
	},
	// No need to override update value as we have extended BizIntegerPercentageItem

	// Override - if passed a string, map it to the appropriate float before saving
    // (this is required since the string passed in won't go through 'mapDisplayToValue')
    setValue : function (newValue) {
        if (isc.isA.String(newValue)) {
        	newValue = isc.BizSimplePercentageItem.parseInput(newValue);
        }
        return this.Super("setValue", [newValue]);
    }
});
isc.SimpleType.create({
	name: "bizSimplePercentage",
	inheritsFrom: "float",
	editorType: "BizSimplePercentageItem",
	editFormatter: function(internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},
	normalDisplayFormatter: function(internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizSimplePercentageItem.format(internalValue);
	},
	validators: [{type: 'custom', clientOnly: true, condition: '(value == null) || isc.isA.Number(value)'}]
});


isc.ClassFactory.defineClass("BizIntegerSeparatorItem", "TextItem");
isc.BizIntegerSeparatorItem.addClassMethods({
	parseInput: function(value) { // value is not only a string, returns a float
		if (isc.isA.Number(value)) {
			return value;
		}
		
		if (value) {
			value = value.toString().replace(',','').replace(',','').replace(',','').replace(',','').replace(',','').replace(',','').trim(' ');
			if (isNaN(value)) {
				return null;
			}
			else {
				var result = Math.round(parseFloat(value)) ;
				if (result >= 1000000000000000000.0) {
					result = null;
				}
				return result;
			}
		}
		else {
			return null;
		}
	},
	format: function(value) { // value is not only a float, returns a String (if value is a float)
		if (! isc.isA.Number(value)) {
			return value;
		}

		return (value).toLocalizedString(0, '.', ',', '-');
	}
});
isc.BizIntegerSeparatorItem.addProperties({
	changeOnBlur: true, // perform validation on field blur
    changeOnKeypress: false, // dont perform validation on key press
    width: 100,
    showHint: true,
    showHintInField: true,
   	hint: '(+/-)99,999,999',
   	selectOnFocus: true
});
isc.BizIntegerSeparatorItem.addMethods({
	mapValueToDisplay: function(value) {
		if (value == null) {
			return isc.emptyString;
		}
		if (! isc.isA.Number(value)) {
			value = isc.BizIntegerSeparatorItem.parseInput(value);
		}
		return isc.BizIntegerSeparatorItem.format(value);
	},
	mapDisplayToValue: function(value) {
		if (isc.isAn.emptyString(value)) {
			value = null;
		}
		else {
			value = isc.BizIntegerSeparatorItem.parseInput(value);
		}
		
		return value;
	},
	// Override - ensure the value displayed in the element matches the saved value, and is formatted correctly
	updateValue: function() {
		// this will map the value to a valid float
        this.Super("updateValue", arguments);
        // this will update the displayed string so its formatted correctly
        this.setElementValue(this.mapValueToDisplay(this.getValue()));
	},
	// Override - if passed a string, map it to the appropriate float before saving
    // (this is required since the string passed in won't go through 'mapDisplayToValue')
    setValue : function (newValue) {
        if (isc.isA.String(newValue)) {
        	newValue = isc.BizIntegerSeparatorItem.parseInput(newValue);
        }
        return this.Super("setValue", [newValue]);
    }
});
isc.SimpleType.create({
	name: "bizIntegerSeparator",
	inheritsFrom: "integer",
	editorType: "BizIntegerSeparatorItem",
	editFormatter: function(internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},
	normalDisplayFormatter: function(internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizIntegerSeparatorItem.format(internalValue);
	},
	validators: [{type: 'custom', clientOnly: true, condition: '(value == null) || isc.isA.Number(value)'}]
});


isc.ClassFactory.defineClass("BizTwoDecimalPlacesPercentageItem", "BizIntegerPercentageItem");
isc.BizTwoDecimalPlacesPercentageItem.addClassMethods({
	parseInput: function(value) { // value is not only a string, returns a float
		if (isc.isA.Number(value)) {
			return value;
		}
		
		if (value) {
			value = value.toString().trim(' ').trim('%');
			if (isNaN(value)) {
				return null;
			}
			else {
				var result = Math.round(parseFloat(value) * 100) / 10000;
				if (result >= 1000000000000000000.0) {
					result = null;
				}
				return result;
			}
		}
		else {
			return null;
		}
	},
	format: function(value) { // value is not only a float, returns a String (if value is a float)
		if (! isc.isA.Number(value)) {
			return value;
		}

		return (value * 100).toLocalizedString(2, '.', ',', '-') + '%';
	}
});
isc.BizTwoDecimalPlacesPercentageItem.addProperties({
   	hint: '(+/-)99999.99'
});
isc.BizTwoDecimalPlacesPercentageItem.addMethods({
	mapValueToDisplay: function(value) {
		if (value == null) {
			return isc.emptyString;
		}
		if (! isc.isA.Number(value)) {
			value = isc.BizTwoDecimalPlacesPercentageItem.parseInput(value);
		}
		return isc.BizTwoDecimalPlacesPercentageItem.format(value);
	},
	mapDisplayToValue: function(value) {
		if (isc.isAn.emptyString(value)) {
			value = null;
		}
		else {
			value = isc.BizTwoDecimalPlacesPercentageItem.parseInput(value);
		}
		
		return value;
	},
	// No need to override update value as we have extended BizIntegerPercentageItem

	// Override - if passed a string, map it to the appropriate float before saving
    // (this is required since the string passed in won't go through 'mapDisplayToValue')
    setValue : function (newValue) {
        if (isc.isA.String(newValue)) {
        	newValue = isc.BizTwoDecimalPlacesPercentageItem.parseInput(newValue);
        }
        return this.Super("setValue", [newValue]);
    }
});
isc.SimpleType.create({
	name: "bizTwoDecimalPlacesPercentage",
	inheritsFrom: "float",
	editorType: "BizTwoDecimalPlacesPercentageItem",
	editFormatter: function(internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},
	normalDisplayFormatter: function(internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizTwoDecimalPlacesPercentageItem.format(internalValue);
	},
	validators: [{type: 'custom', clientOnly: true, condition: '(value == null) || isc.isA.Number(value)'}]
});

isc.ClassFactory.defineClass("BizTimeDurationItem", "TextItem");
isc.BizTimeDurationItem.addClassMethods({
	parseInput: function(value) { // value is not only a string, returns a float
		if (isc.isA.Number(value)) {
			return value;
		}
		
		if (value) {
			// look for a decimal formatted match
			var match = /^\s*\s*([-+]?\d+)\s*\.\s*(\d?\d)?\s*$/.exec(value); // sign, 1 or more digits, a decimal point, 1 or 2 digits
			if (match) {
				var result = parseFloat(match[1] + '.' + (match[2] | 0));
				if (result >= 1000000000000000000.0) {
					result = null;
				}
				return result;
			}

			// look for a time formatted match
			match = /^\s*\s*([-+]?\d+)\s*\D?\s*(\d?\d)?\s*$/.exec(value); // sign, 1 or more digits, any non digit, 1 or 2 digits
			if (match) {
				var hours = parseInt(match[1] | 0, 10);
				var minutes = Math.min(parseInt(match[2] | 0, 10), 59);
				if (hours >= 0) {
					return hours + (minutes / 60.0);
				}
				return hours - (minutes / 60.0);
			}
			else {
				return null;
			}
		}
		else {
			return null;
		}
	},
	format: function(value) { // value is not only a float, returns a String (if value is a float)
		if (! isc.isA.Number(value)) {
			return value;
		}

		var decimalMinutes;
		var hours;
		if (value >= 0) {
			hours = Math.floor(value);
			decimalMinutes = value - hours;
		}
		else {
			hours = Math.ceil(value);
			decimalMinutes = Math.abs(value - hours);
		}
		var stringMinutes = Math.round(decimalMinutes * 60).toString();
		if (stringMinutes.length == 1) {
			stringMinutes = '0' + stringMinutes;
		}
		return hours + ':' + stringMinutes;
	}
});
isc.BizTimeDurationItem.addProperties({
	changeOnBlur: true, // perform validation on field blur
    changeOnKeypress: false, // dont perform validation on key press
    width: 100,
    showHint: true,
    showHintInField: true,
   	hint: '(+/-)HHH:MM',
   	selectOnFocus: true
});
isc.BizTimeDurationItem.addMethods({
	mapValueToDisplay: function(value) {
		if (value == null) {
			return isc.emptyString;
		}
		if (! isc.isA.Number(value)) {
			value = isc.BizTimeDurationItem.parseInput(value);
		}
		return isc.BizTimeDurationItem.format(value);
	},
	mapDisplayToValue: function(value) {
		if (isc.isAn.emptyString(value)) {
			value = null;
		}
		else {
			value = isc.BizTimeDurationItem.parseInput(value);
		}
		
		return value;
	},
	// Override - ensure the value displayed in the element matches the saved value, and is formatted correctly
	updateValue: function() {
		// this will map the value to a valid float
        this.Super("updateValue", arguments);
        // this will update the displayed string so its formatted correctly
        this.setElementValue(this.mapValueToDisplay(this.getValue()));
	},
	// Override - if passed a string, map it to the appropriate float before saving
    // (this is required since the string passed in won't go through 'mapDisplayToValue')
    setValue : function (newValue) {
        if (isc.isA.String(newValue)) {
        	newValue = isc.BizTimeDurationItem.parseInput(newValue);
        }
        return this.Super("setValue", [newValue]);
    }
});
isc.SimpleType.create({
	name: "bizTimeDuration",
	inheritsFrom: "float",
	editorType: "BizTimeDurationItem",
	editFormatter: function(internalValue, field, form, record) {
		return this.shortDisplayFormatter(internalValue, field, form, record);
	},
	normalDisplayFormatter: function(internalValue, field, component, record) {
		return this.shortDisplayFormatter(internalValue, field, component, record);
	},
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizTimeDurationItem.format(internalValue);
	},
	validators: [{type: 'custom', clientOnly: true, condition: '(value == null) || isc.isA.Number(value)'}]
});

isc.ClassFactory.defineClass("BizContentLinkItem", isc.CanvasItem);
//instance properties and methods
isc.BizContentLinkItem.addProperties({
	height: 25, 
	width: '*',
	rowSpan: "*", 
	endRow: false, 
	startRow: false,
	canFocus: true,
	// this is going to be an editable data item
	shouldSaveValue: true
});

//Properties are :-
//
//width
//name
//title
//icons
// editable
// value - the content link value (not the href)
isc.BizContentLinkItem.addMethods({
	init: function(config) {
		this._link = isc.HTMLFlow.create({
		    contents: "Empty",
		    	width: '100%'
		});

		if (config.editable) {
			this.canvas = isc.HLayout.create({
				defaultLayoutAlign: 'center',
				members: [this._link, isc.LayoutSpacer.create({width:3}), isc.BizUtil.createUploadButton(this)]
			});
		}
		else {
			this.canvas = isc.HLayout.create({
				defaultLayoutAlign: 'center',
				members: [this._link]
			});
		}
		
		this.Super("init", arguments);
	},
	
	// Override setValue to update the link selection
	setValue: function(newValue) {
		if ((this.canvas != null) && (! this.userSetValue)) {
			if (newValue) {
				var url = isc.BizUtil.URL_PREFIX + 
				"content?_n=" + newValue +
				"&_doc=" + this.form._view._mod + '.' + this.form._view._doc +
				"&_b=" + this.name.replaceAll('_', '.') +
				"&_ctim=" + new Date().getTime();

				this._link.setContents('<div style="line-height:25px;vertical-align:middle;">' + this.canvas.linkHTML(url, (this.value ? this.value : "Content"), "_blank") + '</div>');
			}
			else {
				this._link.setContents('<div style="line-height:25px;vertical-align:middle;">&lt;Empty&gt;</div>');
			}
	   }
	   return this.Super("setValue", [newValue]);
	}
});

//register the editor
isc.SimpleType.create({
	inheritsFrom: "canvas",
	name: "bizContentLink",
	editorType: "BizContentLinkItem"
});

isc.ClassFactory.defineClass("BizContentImageItem", isc.CanvasItem);
//instance properties and methods
isc.BizContentImageItem.addProperties({
//	height: 20, 
	width: '*',
	rowSpan: "*", 
	endRow: false, 
	startRow: false,
	canFocus: true,
	// this is going to be an editable data item
	shouldSaveValue: true
});

//Properties are :-
//
//width
//height
//editable
//name
//title
//icons
isc.BizContentImageItem.addMethods({
	init: function(config) {
		this._img = isc.Img.create({
			width: (config.width ? config.width : '100%'),
			height: (config.height ? config.height : '100%'),
			imageType: 'center',
			showEdges: true,
			showDisabled: false,
		    src: "[SKIN]blank.gif"
		});

		if (config.editable) {
			var centredUploadButton = isc.HLayout.create({
				width: 1, // make minimum width of button
				height: (config.height ? config.height : '100%'),
				defaultLayoutAlign: 'center', 
				members: [isc.BizUtil.createUploadButton(this)]
			});
			this.canvas = isc.HLayout.create({
				defaultLayoutAlign: 'center',
				members: [this._img, isc.LayoutSpacer.create({width:5}), centredUploadButton]
			});
		}
		else {
			this.canvas = isc.HLayout.create({
				defaultLayoutAlign: 'center',
				members: [this._img]
			});
		}

		this.Super("init", arguments);
	},
	
	// Override setValue to update the image selection
	setValue: function(newValue) {
		if ((this.canvas != null) && (! this.userSetValue)) {
			if (newValue) {
				var url = isc.BizUtil.URL_PREFIX + 
							"content?_n=" + newValue +
							"&_doc=" + this.form._view._mod + '.' + this.form._view._doc +
							"&_b=" + this.name.replaceAll('_', '.') +
							"&_w=" + (this._img.getWidth() - 20) + // -20 for the border
							"&_h=" + (this._img.getHeight() - 20) + // -20 for the border
							"&_ctim=" + new Date().getTime();
				this._img.setSrc(url);
			}
			else {
				this._img.setSrc("[SKIN]blank.gif");
			}
	    }
		return this.Super("setValue", [newValue]);
	}
});

//register the editor
isc.SimpleType.create({
	inheritsFrom: "canvas",
	name: "bizContentImage",
	editorType: "BizContentImageItem"
});

isc.ClassFactory.defineClass("BizLookupDescriptionItem", isc.CanvasItem);
//instance properties and methods
isc.BizLookupDescriptionItem.addProperties({
	height: 20, 
	width: '*',
	rowSpan: "*", 
	endRow: false, 
	startRow: false,
	canFocus: true,
	// this is going to be an editable data item
	shouldSaveValue: true,

	// these are defined for the combo but also need to be here on the canvas as it does its own thing also
	// ie - don't let the canvas autofetch, or fetch missing values, the combo on the canvas does this
	fetchMissingValues: false,
	autoFetchData: false,

	// based on document permissions
	canCreate: false,
	canUpdate: false,
	
	// conditions to evaluate from the view widget defn
	canPick: true,
	canEdit: true,
	canAdd: true,
	canClear: true
});

// Properties are :-
//
// width
// name
// title
// allowEmptyValue
// canCreate - whether we can create a new target document bean
// canUpdate - whether we can update an existing target document bean
// icons
// optionDataSource - the data source to use when the dropdown is activated
// valueField
// displayField
// pickListFields - fields defined for drop down
// params - an associative array of binding name to string expressions to evaluate with "isc.EditView.toDisplay()" when a view is populated.
//				These parameter evaluations are sent down as filter criteria to the server.
isc.BizLookupDescriptionItem.addMethods({
	init: function(config) {
		var me = this;
		
		var combo = {
			name: '_combo', 
			type: 'comboBox', 
			showTitle: false,
			width: '*',
			height: 22,
			selectOnFocus: true,
			fetchMissingValues: false,
//			addUnknownValues: false, This triggers a fetch on blur
			autoFetchData: false,
			useClientSideFiltering: false,
			cachePickListResults: false,
			allowEmptyValue: config.allowEmptyValue,
			optionDataSource: config.optionDataSource,
			valueField: config.valueField,
			displayField: config.displayField,
			pickListFields: config.pickListFields,
			filterFields: config.filterFields,
			completeOnTab: true,
			textMatchStyle: 'substring',
//			filterWithValue: true,
//NEVER USE THIS AS IT SCREWS WITH VALUES COMING FROM THE SERVER AT POPULATE TIME
//				addUnknownValues: false,
			getPickListFilterCriteria: function() {
				var result = this.Super("getPickListFilterCriteria", arguments);
				if (config.params) {
					result = isc.BizUtil.completeFilterCriteria(result, config.params, me._view);
				}
				
				return result;
			},
			// This method isn't part of the external API, but is the only place
			// to intercept the dropdown event.
			// pickerIconClick is fired after the dropdown query is requested.
			showPicker: function() {
				var optionDataSource = this.getOptionDataSource();
				if (optionDataSource) {
					if (optionDataSource.compareCriteria) {
						optionDataSource._drop = true;
					}
				}
				
				this.Super('showPicker');
			},
			// ensure that the canvasItem value is changed when the combo box value is
			changed: function(form, item, value) {
				// if the display value = the hidden value then we have no record
				if (this.getDisplayValue() == value) { // set the editors value to null
					me.Super("setValue", null);
				}
				else { // set the editors value to the bizId
					me.Super("setValue", [value]);
					if (value) {
						me.bizPicked(me.form, me, value);
					}
				}
			},
			// when we lose focus, ensure that the combo box description is blanked if we have no bizId stored in the editor
			blur: function(form, item) {
				if (me.getValue() == null) { // value stored in lookup desc canvas item
					var itemValue = item.getValue();
					// only set it when itemValue is not empty as it'll cause a server hit on optionDataSource
					if (itemValue && (itemValue != '')) {
						item.setValue(''); // blank the combobox
					}
				}
			},
			valueMap: {}
        };

		if (config.editable) {
			this._form = isc.DynamicForm.create({
				writeFormTag: false, // ensure there are no nested forms
				numCols: 3,
				colWidths: ['*', 1, 53],
				margin: 0,
				cellPadding: 0
			});

			this._splitButton =	isc.BizUtil.createSplitButton(
				'Pick', 
				null, 
				false, 
				'Pick a record', 
				function() {
					var pickList = isc.BizUtil.getPickList(me, config.params, me._view);
					pickList.setDataSource(config.optionDataSource);
					isc.WindowStack.popup(me.getPageRect(), 'Pick', true, [pickList]);
				},
				'Other Options', 
				this._form,
				[{title: 'Edit', 
					icon: "icons/zoom.gif",
					click: function(event) {
						me.zoom(false);
					},
					enableIf: function(target, menu, item) {
						return (me.canUpdate && me.canEdit && (me.getValue() != null));
					}},
				 {title: 'New', 
					icon: "icons/new.png",
					click: function(event) {
						if (config && config.params) {
							var newParams = {};
							isc.BizUtil.addFilterRequestParams(newParams, 
																config.params,
																me._view);
							me.zoom(true, newParams);
						}
						else {
							me.zoom(true);
						}
					},
					enableIf: function(target, menu, item) {
						return me.canCreate && me.canAdd;
					}},
				 {title: 'Clear', 
					icon: "icons/delete.png",
					click: function(event) {
						me.setValue(null);
						me.bizCleared(me.form, me, null);
					},
					enableIf: function(target, menu, item) {
						return me.canClear;
					}}]
			);
			
			this._form.setItems([
			    combo,
			    {type: 'spacer', width: 1},
		        {name: '_splitButton', showTitle: false, type: 'canvas', canvas: this._splitButton}
		    ]);
		}
		else {
			this._form = isc.DynamicForm.create({
				writeFormTag: false, // ensure there are no nested forms
				numCols: 1,
				colWidths: ['*']
			});

			this._form.setItems([combo]);
		}

		this.canvas = this._form;
		this.Super("init", arguments);
	},
	
	// Override setValue to update the combo selection
	setValue: function(newValue) {
		if (this.canvas != null) {
			if (this.userSetValue) {
				this.bizPicked(this.form, this, newValue);
			}
			else {
				this._form.getItem('_combo').setValue(newValue);
			}
		}

		this.Super("setValue", [newValue]);
	},
	
	// Override focusInItem to focus in the combo selection
	focusInItem: function() {
		if (this._form) {
			this._form.getItem('_combo').focusInItem();
		}
	},
	
	// bizhub events
	bizAdded: function(form, item, value) {
		// do nothing - overridden in serverside generated definition if required
	},
/* existence tested before calling this method in zoomout code in editView.js
	bizAddedForServer: function(form, item, value) { // called by the zoomed (child) view if required
		// do nothing - overridden in serverside generated definition if required
	},
*/
	bizEdited: function(form, item, value) {
		// do nothing - overridden in serverside generated definition if required
	},
/* existence tested before calling this method in zoomout code in editView.js
	bizEditedForServer: function(form, item, value) { // called by the zoomed (child) view if required
		// do nothing - overridden in serverside generated definition if required
	},
*/
	bizPicked: function(form, item, value) {
		// do nothing - overridden in serverside generated definition if required
	},
	bizCleared: function(form, item, value) {
		// do nothing - overridden in serverside generated definition if required
	},

	// set the value map from a fetch from the server
	setValueMapFromEditView: function(values) { // object to get extra values out of
		this._setValueMap(values[this.name], values[this.name + '_' + this.displayField]);
	},
	
	// set the value map from a picked record
	setValueMapFromPickList: function(values) { // object to get extra values out of
		this._setValueMap(values.bizId, values[this.displayField]);
	},
	
	_setValueMap: function(bizId, display) {
		if (bizId) {
			var valueMap = {};
			valueMap[bizId] = display ? display : '<unknown>';
			this._form.getItem('_combo').setValueMap(valueMap);
			this.setValueMap(valueMap);
		}
	},
 	
	enableDisablePick: function() {
		this._form.getItem('_combo').setDisabled(! this.canPick);
		if (this._splitButton) { // is editable
			this._splitButton.getMember(0).setDisabled(! this.canPick);
		}
	},
	
	// goes to edit view
	zoom: function(zoomToNew, // boolean - do we want a new record or an existing one
					newParams) { // a map of parameter names to expressions to evaluate - can be null or undefined
		var me = this;
		var mod = isc.DataSource.get(this.optionDataSource).modoc;
		var dotIndex = mod.indexOf('.');
		var doc = mod.substring(dotIndex + 1);
		mod = mod.substring(0, dotIndex);

		isc.BizUtil.getEditView(mod, 
								doc,
								function(view) { // the view
									// determine the view binding
									var viewBinding = (me._view._b) ? me._view._b + '.' + me.name : me.name;
									var fromRect = me.getPageRect();
									if (zoomToNew) {
										// make this form item not required whilst we post before zoom in
										var required = me.required;
										if (required) {
											me.setRequired(false);
										}
										// Validate here so we can put out the zoom message if required
										var instance = me._view.gather(true);
										if (instance) {} else {
											isc.warn('You cannot zoom in until you fix the problems found');
										}

										if (instance._apply || me._view._vm.valuesHaveChanged()) {
											delete instance._apply;
											// apply changes to current form before zoom in
											me._view.saveInstance(true, null, function() {
												me.setRequired(required); // reset form item's required-ness
												isc.WindowStack.popup(fromRect, "New", false, [view]);
												view.newInstance(newParams, viewBinding, instance._c, false);
											});
										}
										else {
											me.setRequired(required); // reset form item's required-ness
											isc.WindowStack.popup(fromRect, "New", false, [view]);
											view.newInstance(newParams, viewBinding, instance._c, false);
										}
									}
									else {
										// Validate here so we can put out the zoom message if required
										var instance = me._view.gather(true);
										if (instance) {} else {
											isc.warn('You cannot zoom in until you fix the problems found');
										}
										
										if (instance._apply || me._view._vm.valuesHaveChanged()) {
											delete instance._apply;
											// apply changes to current form before zoom in
											me._view.saveInstance(true, null, function() {
												isc.WindowStack.popup(fromRect, "Edit", false, [view]);
												view.editInstance(me.getValue(),
																	viewBinding,
																	instance._c,
																	false);
											});
										}
										else {
											isc.WindowStack.popup(fromRect, "Edit", false, [view]);
											view.editInstance(me.getValue(),
																viewBinding,
																instance._c,
																false);
										}
									}
								});
	}
});

//register the editor
isc.SimpleType.create({
	inheritsFrom: "comboBox",
	name: "bizLookupDescription",
	editorType: "BizLookupDescriptionItem"
});

// CKEditor is loaded by Ajile when required
var CKEDITOR = null;

isc.ClassFactory.defineClass("BizHTMLItem", isc.CanvasItem);
//instance properties and methods
isc.BizHTMLItem.addProperties({
	width: '*',
	height: '200px',
	rowSpan: "*", 
	endRow: false, 
	startRow: false,
	canFocus: true,
	// this is going to be an editable data item
	shouldSaveValue: true
});

//Properties are :-
//
//width
//name
//title
//icons
isc.BizHTMLItem.addMethods({
	init: function(config) {
		this._pane = isc.HTMLPane.create({
			width: config.width,
			height: config.height,
			showEdges: true,
		    contents: 'Empty'
		});
		
		var me = this;
		this._editButton = isc.IButton.create({
			height: 22,
			width: 30,
			title: 'Edit',
			canHover: true,
			getHoverHTML: function() {return 'Edit the HTML';},
			click: function(editEvent) {
				if (CKEDITOR) {
					this._show();
				}
				else {
					isc.BizUtil.loadJS('ckeditor456/ckeditor.js', function() {
						me._editButton._show();
					});
				}
			},
			_show: function() {
				var owningView = me.form._view;
				var formValues = owningView.gather(false);
				
				var holder = isc.Canvas.create({width:'100%', height:'100%'});
				holder.setContents('<div style="width:100%;height:100%" id="_CKEditor"></div>');
				holder.draw = function() {
					this.Super('draw');
					me._editor = CKEDITOR.replace(
						'_CKEditor', 
						{customConfig: '',
							resize_enabled: false,
							skin: 'moono',
							autoUpdateElement: false,
							baseFloatZIndex: 9000000,
							filebrowserImageBrowseUrl: "pages/htmlEdit/browseImages.jsp?_doc=" + owningView._mod + '.' + owningView._doc + '&_id=' + formValues['bizId'],
							filebrowserBrowseUrl: "pages/htmlEdit/browseDocuments.jsp?_doc=" + owningView._mod + '.' + owningView._doc + '&_id=' + formValues['bizId'],
							toolbar: [
								['Source', '-', 'NewPage', 'Preview'], 
								['Cut', 'Copy', 'Paste', 'PasteText', 'PasteFromWord', '-', 'Print', 'SpellChecker', 'Scayt'], 
								['Undo', 'Redo', '-', 'Find', 'Replace', '-', 'SelectAll', 'RemoveFormat'], 
								'/', 
								['Bold', 'Italic', 'Underline', 'Strike', '-', 'Subscript', 'Superscript'], 
								['NumberedList', 'BulletedList', '-', 'Outdent', 'Indent', 'Blockquote', 'CreateDiv'], 
								['JustifyLeft', 'JustifyCenter', 'JustifyRight', 'JustifyBlock'], 
								['Link', 'Unlink', 'Anchor'], ['Image', 'Flash', 'Table', 'HorizontalRule', 'Smiley', 'SpecialChar', 'PageBreak'], 
								'/', 
								['Format', 'Font', 'FontSize'], 
								['TextColor', 'BGColor'], ['Maximize', 'ShowBlocks']
							]
						});
					me._editor.setData(me.getValue());
					return this;
				};

				isc.WindowStack.popup(null,
										'Edit HTML', 
										true,
										[holder,
											isc.HLayout.create({
												membersMargin: 5,
												margin: 10,
												align: "right",
												members: [
													isc.IButton.create({
														height: 22, 
														width: 60, 
														title: 'Apply',
														click: function(applyEvent) {
															me.setValue(me._editor.getData());
															isc.WindowStack.popoff(false);
														}
													}),
													isc.IButton.create({
														height: 22, 
														width: 60, 
														title: 'Cancel',
														click: function(applyEvent) {
															isc.WindowStack.popoff(false);
														}
													})
												]
											})
										],
										405);
			}
		});
		
		this.canvas = isc.HLayout.create({
			defaultLayoutAlign: 'center',
			members: [this._pane, isc.LayoutSpacer.create({width:3}), this._editButton]
		});

		this.Super("init", arguments);
	},
	
	// Override setValue to update the link selection
	setValue: function(newValue) {
		if ((this.canvas != null) && (! this.userSetValue)) {
			if (newValue) {
				this._pane.setContents(newValue);
			}
			else {
				this._pane.setContents('');
			}
	   }
	   return this.Super("setValue", [newValue]);
	}
});

//register the editor
isc.SimpleType.create({
	inheritsFrom: "canvas",
	name: "bizHTML",
	editorType: "BizHTMLItem"
});

isc.ClassFactory.defineClass("BizMapPicker", "HTMLFlow");
isc.BizMapPicker.addClassMethods({
	v: 0,
	initialise: function() {
		eval(isc.BizMapPicker.id + '.build()');
	}
});
isc.BizMapPicker.addMethods({
	init: function(config) {
		this.width = '100%';
		this.height = '100%';
		this.styleName = 'googleMapDivParent';
		this.ID = 'bizMapPicker' + isc.BizMapPicker.v++;
		this.contents = '<div id="' + this.ID + '_map" style="margin:0;padding:0;height:100%">Loading Map...</div>';
		this.Super("init", arguments);
		this._overlays = [];
		this.field = config.field;
		
		if (window.google && window.google.maps) {
			this.build();
		}
		else {
			isc.BizMapPicker.id = this.ID;

			isc.BizUtil.loadJS('wicket/wicket.js?v=' + isc.BizUtil.version, function() {
				isc.BizUtil.loadJS('wicket/wicket-gmap3.js?v=' + isc.BizUtil.version, function() {
					isc.BizUtil.loadJS('https://maps.googleapis.com/maps/api/js?v=3&libraries=drawing&' +
            							'callback=isc.BizMapPicker.initialise');
				});
			});
		}
	},

    mapIt: function() {
    	var value = this.field.getValue();
		if (value) {} else {
			return;
		}

		var wkt = new Wkt.Wkt();
        try { // Catch any malformed WKT strings
        	wkt.read(value);
        }
        catch (e) {
            if (e.name === 'WKTError') {
                alert('The WKT string is invalid.');
                return;
            }
        }

        var obj = wkt.toObject(this._map.defaults);
        
        if (wkt.type === 'polygon' || wkt.type === 'linestring') {
        }
		else {
            if (obj.setEditable) {obj.setEditable(false);}
        }

        if (Wkt.isArray(obj)) { // Distinguish multigeometries (Arrays) from objects
        	for (i in obj) {
                if (obj.hasOwnProperty(i) && ! Wkt.isArray(obj[i])) {
                    obj[i].setMap(this._map);
                    this._overlays.push(obj[i]);
                }
            }
        }
        else {
            obj.setMap(this._map); // Add it to the map
            this._overlays.push(obj);
        }

        // Pan the map to the feature
        if (obj.getBounds !== undefined && typeof obj.getBounds === 'function') {
            // For objects that have defined bounds or a way to get them
            this._map.fitBounds(obj.getBounds());
        }
        else {
            if (obj.getPath !== undefined && typeof obj.getPath === 'function') {
	            // For Polygons and Polylines - fit the bounds to the vertices
				var bounds = new google.maps.LatLngBounds();
				var path = obj.getPath();
				for (var i = 0, l = path.getLength(); i < l; i++) {
					bounds.extend(path.getAt(i));
				}
				this._map.fitBounds(bounds);
            }
            else { // But points (Markers) are different
                if (obj.getPosition !== undefined && typeof obj.getPosition === 'function') {
                    this._map.panTo(obj.getPosition());
                }
                if (this._map.getZoom() < 15) {
                    this._map.setZoom(15);
                }
            }
        }
    },

    clearIt: function () {
        for (var i = 0, l = this._overlays.length; i < l; i++) {
            this._overlays[i].setMap(null);
        }
        this._overlays.length = 0;
    },

	build: function() {
		if (this.isDrawn()) {
			var mapOptions = {
				zoom: 4,
				center: new google.maps.LatLng(-26,133.5),
				mapTypeId: google.maps.MapTypeId.ROADMAP
			};
			var drawingDefaults = {
                    editable: true,
                    strokeColor: '#990000',
                    fillColor: '#EEFFCC',
                    fillOpacity: 0.6
            };
			this._map = new google.maps.Map(document.getElementById(this.ID + '_map'), mapOptions);

            this._map.drawingManager = new google.maps.drawing.DrawingManager({
                drawingControlOptions: {
                    position: google.maps.ControlPosition.TOP_CENTER,
                    defaults: drawingDefaults,
                    drawingModes: [
                        google.maps.drawing.OverlayType.MARKER,
                        google.maps.drawing.OverlayType.POLYLINE,
                        google.maps.drawing.OverlayType.POLYGON,
                        google.maps.drawing.OverlayType.RECTANGLE
                    ]
                },
                markerOptions: drawingDefaults,
                polygonOptions: drawingDefaults,
                polylineOptions: drawingDefaults,
                rectangleOptions: drawingDefaults
            });
            this._map.drawingManager.setMap(this._map);

            var me = this;
            
            google.maps.event.addListener(this._map.drawingManager, 'overlaycomplete', function (event) {
                me.clearIt();

                // Set the drawing mode to "pan" (the hand) so users can immediately edit
                this.setDrawingMode(null);

                me._overlays.push(event.overlay);
                var wkt = new Wkt.Wkt();
                wkt.fromObject(event.overlay);
                var wktValue = wkt.write();
                me.field.setValue(wktValue);
            });

			this.clearIt();
			this.mapIt();
		}
		else {
			this.delayCall('build', null, 100);
		}
	}
});

isc.ClassFactory.defineClass("GeometryItem", "TextItem");
isc.GeometryItem.addClassProperties({
	validOperators: ['gWithin', 'gContains', 'gOverlaps', 'gDisjoint', 'gIntersects', 'gTouches', 'gCrosses', 'gEquals']
});
isc.GeometryItem.addClassMethods({
	format: function(value) {
		if (value) {
			if (value.startsWith('POINT')) {
				return 'Point';
			}
			else if (value.startsWith('LINESTRING')) {
				return 'Line';
			}
			else if (value.startsWith('POLYGON')) {
				return 'Polygon';
			}
		}

		return '';
	}
});
isc.GeometryItem.addProperties({
//    canEdit: false,
//    disableIconsOnReadOnly: false,
    width: 100,
    operator: 'gWithin',
	validOperators: isc.GeometryItem.validOperators,
	selectOnFocus: true
});
isc.GeometryItem.addMethods({
	init: function(config) {
		this.icons = [{
	        src: 'icons/map.png',
	        prompt: 'Click to set or see the geometry on a map',
	        click: function(form, item, icon) {
	    		isc.WindowStack.popup(item.getPageRect(), 'Map', true, [isc.BizMapPicker.create({field:item})]);
	        }
	    }];
		if (config.icons) {
			this.icons.addList(config.icons);
		}
		
		this.Super("init", arguments);
	}
});
isc.SimpleType.create({
    name: "geometry",
    inheritsFrom: "text",
	editorType: "GeometryItem",
	validOperators: isc.GeometryItem.validOperators
});