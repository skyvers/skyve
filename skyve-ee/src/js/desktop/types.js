//isc.Time.setDefaultDisplayTimezone('+00:00');
//isc.Time.adjustForDST = false;

// Define our extra date and time formats
isc.addMethods(Date.prototype, {
	toDD_MM_YYYY: function() {
		return this.getDate().stringify() + '/' + (this.getMonth() + 1).stringify() + '/' + this.getFullYear();
	},
	toDD_MM_YYYY_HH_MI: function() {
		return this.toDD_MM_YYYY() + ' ' + this.getHours().stringify() + ":" + this.getMinutes().stringify();
	},
	toDD_MM_YYYY_HH_MI_SS: function() {
		return this.toDD_MM_YYYY_HH_MI() + ':' + this.getSeconds().stringify();
	},
	
	toDD_MMM_YYYY: function() {
		var shortMonthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
		return this.getDate().stringify() + '-' + shortMonthNames[this.getMonth()] + '-' + this.getFullYear();
	},
	toDD_MMM_YYYY_HH_MI: function() {
		return this.toDD_MMM_YYYY() + ' ' + this.getHours().stringify() + ":" + this.getMinutes().stringify();
	},
	toDD_MMM_YYYY_HH_MI_SS: function() {
		return this.toDD_MMM_YYYY_HH_MI() + ':' + this.getSeconds().stringify();
	},
	
	toMM_DD_YYYY: function() {
		return (this.getMonth() + 1).stringify() + '/' + this.getDate().stringify() + '/' + this.getFullYear();
	},
	toMM_DD_YYYY_HH_MI: function() {
		return this.toMM_DD_YYYY() + ' ' + this.getHours().stringify() + ":" + this.getMinutes().stringify();
	},
	toMM_DD_YYYY_HH_MI_SS: function() {
		return this.toMM_DD_YYYY_HH_MI() + ':' + this.getSeconds().stringify();
	},
	
	toMMM_DD_YYYY: function() {
		var shortMonthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
		return shortMonthNames[this.getMonth()]+ '-' + this.getDate().stringify() + '-' + this.getFullYear();
	},
	toMMM_DD_YYYY_HH_MI: function() {
		return this.toMMM_DD_YYYY() + ' ' + this.getHours().stringify() + ":" + this.getMinutes().stringify();
	},
	toMMM_DD_YYYY_HH_MI_SS: function() {
		return this.toMMM_DD_YYYY_HH_MI() + ':' + this.getSeconds().stringify();
	},
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
	parseInput: function(value, datePattern) { // value is not only a string		
		if (isc.isA.Date(value)) {
			return value;
		}
		
// console.log("datePattern:", datePattern);
		
		if (value) { // must be a string
			value = value.trim();
			
			// US date format
			if(datePattern && datePattern.startsWith("toMMM")) {
				// look for a string month match
				var match = /^([A-Za-z]+)[^A-Za-z0-9]\d?\d.*$/.exec(value); // month word, 1 or 2 digits, anything 
				if (match) {
					var month = match[1];
// console.log("found US month match:", month);
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
			} else {
				// look for a string month match
				var match = /^\d?\d[^A-Za-z0-9]([A-Za-z]+).*$/.exec(value); // 1 or 2 digits, anything, month word
				if (match) {
					var month = match[1];
// console.log("found non-US month match:", month);
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
			}
			
			var result = isc.DateUtil.parseSchemaDate(value);
			if (! result) {
				// any non-matches returns null
				if(datePattern && datePattern.startsWith("toMM")) {
					result = Date.parseInput(value, 'MDY', 50, true);
				} else {
					result = Date.parseInput(value, 'DMY', 50, true);
				}
				// check if we have just a day and month, no year or time etc
				if (! result) {
					if (/^\d?\d[^A-Za-z0-9]\d?\d$/.test(value)) { // 1 or 2 digits, anything, 1 or 2 digits
						// try adding the current year on the end - any non-matches returns null
						if(datePattern && datePattern.startsWith("toMM")) {
							result = Date.parseInput(value + ' ' + Date.create().getFullYear(), 'MDY', 50, true);
						} else {
							result = Date.parseInput(value + ' ' + Date.create().getFullYear(), 'DMY', 50, true);
						}
					}
				}
			}

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
   	dateFormatter: 'toDD_MM_YYYY',
	inputFormat: function(value) {
		return isc.BizDateItem.parseInput(value, this.dateFormatter);
	}
});
isc.BizDateItem.addMethods({
	mapValueToDisplay: function(value) {
		if (value == null) {
			return isc.emptyString;
		}
		if (! isc.isA.Date(value)) {
			value = isc.BizDateItem.parseInput(value, this.dateFormatter);
		}
		return isc.BizDateItem.format(value, this.dateFormatter);
	},
	mapDisplayToValue: function(value) {
		if (isc.isAn.emptyString(value)) {
			value = null;
		}
		else {
			value = isc.BizDateItem.parseInput(value, this.dateFormatter);
		}
		
		return value;
	},
	// Override - ensure the value displayed in the element matches the saved value, and is formatted correctly
	updateValue: function() {
		// this will map the value to a valid date
        this.Super("updateValue", arguments);
        // this will update the displayed string so its formatted correctly
        this.setElementValue(this.mapValueToDisplay(this.getValue()));
	},
	// Override - if passed a string, map it to the appropriate date before saving
    // (this is required since the string passed in won't go through 'mapDisplayToValue')
    setValue : function (newValue) {
        if (isc.isA.String(newValue)) {
        	newValue = isc.BizDateItem.parseInput(newValue, this.dateFormatter);
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

// register dd-mmm-yyyy
isc.ClassFactory.defineClass("DD_MMM_YYYY_Item", "BizDateItem");
isc.DD_MMM_YYYY_Item.addProperties({
	dateFormatter: 'toDD_MMM_YYYY'
});
isc.SimpleType.create({
	name: "DD_MMM_YYYY",
	inheritsFrom: "bizDate",
	editorType: 'DD_MMM_YYYY_Item',
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, 'toDD_MMM_YYYY');
	}
});

// register mm-dd-yyyy
isc.ClassFactory.defineClass("MM_DD_YYYY_Item", "BizDateItem");
isc.MM_DD_YYYY_Item.addProperties({
	dateFormatter: 'toMM_DD_YYYY'
});
isc.SimpleType.create({
	name: "MM_DD_YYYY",
	inheritsFrom: "bizDate",
	editorType: 'MM_DD_YYYY_Item',
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, 'toMM_DD_YYYY');
	}
});

// register mmm-dd-yyyy
isc.ClassFactory.defineClass("MMM_DD_YYYY_Item", "BizDateItem");
isc.MMM_DD_YYYY_Item.addProperties({
	dateFormatter: 'toMMM_DD_YYYY'
});
isc.SimpleType.create({
	name: "MMM_DD_YYYY",
	inheritsFrom: "bizDate",
	editorType: 'MMM_DD_YYYY_Item',
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, 'toMMM_DD_YYYY');
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
		return isc.BizDateItem.parseInput(value, this.dateFormatter);
	},
	dateFormatter: 'toDD_MM_YYYY_HH_MI'
});
isc.BizDateTimeItem.addMethods({
	mapValueToDisplay: function(value) {
		if (value == null) {
			return isc.emptyString;
		}
		if (! isc.isA.Date(value)) {
			value = isc.BizDateItem.parseInput(value, this.dateFormatter);
		}
		return isc.BizDateItem.format(value, this.dateFormatter);
	},
	mapDisplayToValue: function(value) {
		if (isc.isAn.emptyString(value)) {
			value = null;
		}
		else {
			value = isc.BizDateItem.parseInput(value, this.dateFormatter);
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
        	newValue = isc.BizDateItem.parseInput(newValue, this.dateFormatter);
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

// register dd-MM-yyyy HH:mm
isc.ClassFactory.defineClass("DD_MM_YYYY_HH24_MI_Item", "BizDateTimeItem");
isc.DD_MM_YYYY_HH24_MI_Item.addProperties({
	dateFormatter: 'toDD_MM_YYYY_HH_MI',
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

// register dd-MMM-yyyy hh:mm a
isc.ClassFactory.defineClass("DD_MMM_YYYY_HH_MI_Item", "BizDateTimeItem");
isc.DD_MMM_YYYY_HH_MI_Item.addProperties({
	dateFormatter: 'toDD_MMM_YYYY_HH_MI',
	hint: 'DD MM(M) YY(YY) HH:MI AM/PM',
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

// register dd-MMM-yyyy HH:mm
isc.ClassFactory.defineClass("DD_MMM_YYYY_HH24_MI_Item", "BizDateTimeItem");
isc.DD_MMM_YYYY_HH24_MI_Item.addProperties({
	dateFormatter: 'toDD_MMM_YYYY_HH_MI',
	hint: 'DD MM(M) YY(YY) HH(24):MI',
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

// register dd-MM-yyyy hh:mm:ss a
isc.ClassFactory.defineClass("DD_MM_YYYY_HH_MI_SS_Item", "BizDateTimeItem");
isc.DD_MM_YYYY_HH_MI_SS_Item.addProperties({
	dateFormatter: 'toDD_MM_YYYY_HH_MI_SS',
   	hint: 'DD MM(M) YY(YY) HH:MI(:SS) AM/PM',
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

// register dd-MM-yyyy HH:mm:ss
isc.ClassFactory.defineClass("DD_MM_YYYY_HH24_MI_SS_Item", "BizDateTimeItem");
isc.DD_MM_YYYY_HH24_MI_SS_Item.addProperties({
	dateFormatter: 'toDD_MM_YYYY_HH_MI_SS',
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

// register dd-MMM-yyyy hh:mm:ss a
isc.ClassFactory.defineClass("DD_MMM_YYYY_HH_MI_SS_Item", "BizDateTimeItem");
isc.DD_MMM_YYYY_HH_MI_SS_Item.addProperties({
	dateFormatter: 'toDD_MMM_YYYY_HH_MI_SS',
   	hint: 'DD MM(M) YY(YY) HH:MI(:SS) AM/PM',
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

// register dd-MMM-yyyy HH:mm:ss
isc.ClassFactory.defineClass("DD_MMM_YYYY_HH24_MI_SS_Item", "BizDateTimeItem");
isc.DD_MMM_YYYY_HH24_MI_SS_Item.addProperties({
	dateFormatter: 'toDD_MMM_YYYY_HH_MI_SS',
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

// register MM-dd-yyyy HH:mm
isc.ClassFactory.defineClass("MM_DD_YYYY_HH24_MI_Item", "BizDateTimeItem");
isc.MM_DD_YYYY_HH24_MI_Item.addProperties({
	dateFormatter: 'toMM_DD_YYYY_HH_MI',
	hint: 'MM(M) DD YY(YY) HH(24):MI',
	pickerTimeItemProperties: {showSecondItem: false, use24HourTime: true}
});
isc.SimpleType.create({
	name: "MM_DD_YYYY_HH24_MI",
	inheritsFrom: "bizDateTime",
	editorType: 'MM_DD_YYYY_HH24_MI_Item',
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, 'toMM_DD_YYYY_HH_MI');
	}
});

// register MMM-dd-yyyy hh:mm a
isc.ClassFactory.defineClass("MMM_DD_YYYY_HH_MI_Item", "BizDateTimeItem");
isc.MMM_DD_YYYY_HH_MI_Item.addProperties({
	dateFormatter: 'toMMM_DD_YYYY_HH_MI',
	hint: 'MM(M) DD YY(YY) HH:MI AM/PM',
	pickerTimeItemProperties: {showSecondItem: false, use24HourTime: false}
});
isc.SimpleType.create({
	name: "MMM_DD_YYYY_HH_MI",
	inheritsFrom: "bizDateTime",
	editorType: 'MMM_DD_YYYY_HH_MI_Item',
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, 'toMMM_DD_YYYY_HH_MI');
	}
});

// register MMM-dd-yyyy HH:mm
isc.ClassFactory.defineClass("MMM_DD_YYYY_HH24_MI_Item", "BizDateTimeItem");
isc.MMM_DD_YYYY_HH24_MI_Item.addProperties({
	dateFormatter: 'toMMM_DD_YYYY_HH_MI',
	hint: 'MM(M) DD YY(YY) HH(24):MI',
	pickerTimeItemProperties: {showSecondItem: false, use24HourTime: true}
});
isc.SimpleType.create({
	name: "MMM_DD_YYYY_HH24_MI",
	inheritsFrom: "bizDateTime",
	editorType: 'MMM_DD_YYYY_HH24_MI_Item',
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, 'toMMM_DD_YYYY_HH_MI');
	}
});

// register MM-dd-yyyy hh:mm:ss a
isc.ClassFactory.defineClass("MM_DD_YYYY_HH_MI_SS_Item", "BizDateTimeItem");
isc.DD_MM_YYYY_HH_MI_SS_Item.addProperties({
	dateFormatter: 'toMM_DD_YYYY_HH_MI_SS',
   	hint: 'MM(M) DD YY(YY) HH:MI(:SS) AM/PM',
	pickerTimeItemProperties: {showSecondItem: true, use24HourTime: false}
});
isc.SimpleType.create({
	name: "MM_DD_YYYY_HH_MI_SS",
	inheritsFrom: "bizDateTime",
	editorType: 'MM_DD_YYYY_HH_MI_SS_Item',
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, 'toMM_DD_YYYY_HH_MI_SS');
	}
});

// register MM-dd-yyyy HH:mm:ss
isc.ClassFactory.defineClass("MM_DD_YYYY_HH24_MI_SS_Item", "BizDateTimeItem");
isc.MM_DD_YYYY_HH24_MI_SS_Item.addProperties({
	dateFormatter: 'toMM_DD_YYYY_HH_MI_SS',
   	hint: 'MM(M) DD YY(YY) HH(24):MI(:SS)',
	pickerTimeItemProperties: {showSecondItem: true, use24HourTime: true}
});
isc.SimpleType.create({
	name: "MM_DD_YYYY_HH24_MI_SS",
	inheritsFrom: "bizDateTime",
	editorType: 'MM_DD_YYYY_HH24_MI_SS_Item',
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, 'toMM_DD_YYYY_HH_MI_SS');
	}
});

// register MMM-dd-yyyy hh:mm:ss a 
isc.ClassFactory.defineClass("MMM_DD_YYYY_HH_MI_SS_Item", "BizDateTimeItem");
isc.MMM_DD_YYYY_HH_MI_SS_Item.addProperties({
	dateFormatter: 'toMMM_DD_YYYY_HH_MI_SS',
   	hint: 'MM(M) DD YY(YY) HH(:MI(:SS) AM/PM',
	pickerTimeItemProperties: {showSecondItem: true, use24HourTime: false}
});
isc.SimpleType.create({
	name: "MMM_DD_YYYY_HH_MI_SS",
	inheritsFrom: "bizDateTime",
	editorType: 'MMM_DD_YYYY_HH_MI_SS_Item',
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, 'toMMM_DD_YYYY_HH_MI_SS');
	}
});

// register MMM-dd-yyyy HH:mm:ss
isc.ClassFactory.defineClass("MMM_DD_YYYY_HH24_MI_SS_Item", "BizDateTimeItem");
isc.MMM_DD_YYYY_HH24_MI_SS_Item.addProperties({
	dateFormatter: 'toMMM_DD_YYYY_HH_MI_SS',
   	hint: 'MM(M) DD YY(YY) HH(24):MI(:SS)',
	pickerTimeItemProperties: {showSecondItem: true, use24HourTime: true}
});
isc.SimpleType.create({
	name: "MMM_DD_YYYY_HH24_MI_SS",
	inheritsFrom: "bizDateTime",
	editorType: 'MMM_DD_YYYY_HH24_MI_SS_Item',
	shortDisplayFormatter: function(internalValue, field, component, record) {
		return isc.BizDateItem.format(internalValue, 'toMMM_DD_YYYY_HH_MI_SS');
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
	dateFormatter: 'toTime',
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
	dateFormatter: 'toShortPadded24HourTime',
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
	dateFormatter: 'toPadded24HourTime',
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
		if (stringMinutes == '60') {
			hours = (value >= 0) ? (hours + 1) : (hours - 1);
			stringMinutes = '00';
		}
		else if (stringMinutes.length == 1) {
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
				members: [this._link, isc.LayoutSpacer.create({width:3}), isc.BizUtil.createUploadButton(this, false)]
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
				var url = SKYVE.Util.CONTEXT_URL + 
				"content?_n=" + newValue +
				"&_doc=" + this.form._view._mod + '.' + this.form._view._doc +
				"&_b=" + this.name.replaceAll('_', '.');

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
			isGroup: true,
			styleName: 'bizhubRoundedBorder',
			groupBorderCSS: '1px solid #bfbfbf',
			margin: 1,
			groupLabelBackgroundColor:'transparent',
			showDisabled: false,
		    src: "[SKIN]blank.gif"
		});

		if (config.editable) {
			var centredUploadButton = isc.HLayout.create({
				width: 1, // make minimum width of button
				height: (config.height ? config.height : '100%'),
				defaultLayoutAlign: 'center', 
				members: [isc.BizUtil.createUploadButton(this, true)]
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
				var url = SKYVE.Util.CONTEXT_URL + 
							"content?_n=" + newValue +
							"&_doc=" + this.form._view._mod + '.' + this.form._view._doc +
							"&_b=" + this.name.replaceAll('_', '.') +
							"&_w=" + (this._img.getWidth() - 20) + // -20 for the border
							"&_h=" + (this._img.getHeight() - 20); + // -20 for the border
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
			pickListProperties: {
				filterData: function(criteria, callback, requestProperties) {
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

					// both of these are required (if defined) for ListModel.setBean() on server side
					requestProperties.params._c = me._view.gather(false)._c;
					if (me._view._b) {
						requestProperties.params._b = me._view._b;
					}
					requestProperties.params._cc = ''; // continue conversation
					this.Super("filterData", arguments);
				}
			},
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
				numCols: 2,
				colWidths: ['*', 65],
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
		        {name: '_splitButton', showTitle: false, type: 'canvas', canvas: this._splitButton, align: 'right'}
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
										if (instance) {
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
											isc.warn('You cannot zoom in until you fix the problems found');
										}
									}
									else {
										// Validate here so we can put out the zoom message if required
										var instance = me._view.gather(true);
										if (instance) {
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
										else {
											isc.warn('You cannot zoom in until you fix the problems found');
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

// CKEditor is loaded on the fly when required
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
			isGroup: true,
			styleName: 'bizhubRoundedBorder',
			groupBorderCSS: '1px solid #bfbfbf',
			margin: 1,
			groupLabelBackgroundColor:'transparent',
			padding: 10,
		    contents: 'Empty'
		});
		
		var me = this;
		this._editButton = isc.IButton.create({
			width: 40,
			title: 'Edit',
			canHover: true,
			getHoverHTML: function() {return 'Edit the HTML';},
			click: function(editEvent) {
				if (CKEDITOR) {
					this._show();
				}
				else {
					SKYVE.Util.loadJS('ckeditor/ckeditor.js', function() {
						me._editButton._show();
					});
				}
			},
			_show: function() {
				var owningView = me.form._view;
				var formValues = owningView.gather(false);
				
				var holder = isc.Canvas.create({width:'100%', height:'100%', margin: 5});
				holder.setContents('<div style="width:100%;height:100%" id="_CKEditor"></div>');
				holder.draw = function() {
					this.Super('draw');
					CKEDITOR.config.filebrowserImageBrowseUrl = "pages/htmlEdit/browseImages.jsp?_doc=" + owningView._mod + '.' + owningView._doc + '&_id=' + formValues['bizId'],
					CKEDITOR.config.filebrowserBrowseUrl = "pages/htmlEdit/browseDocuments.jsp?_doc=" + owningView._mod + '.' + owningView._doc + '&_id=' + formValues['bizId'],
					CKEDITOR.config.resize_enabled = false;
					CKEDITOR.config.skin = 'moono-lisa';
					CKEDITOR.config.autoUpdateElement = false;
					CKEDITOR.config.baseFloatZIndex = 9000000;
					CKEDITOR.config.toolbar = [
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
					];
/* for now this is commented out (1 for each marker character)
					CKEDITOR.config.mentions = [{
						feed: 'http://fucked.com/?q={encodedQuery}?c=admin.Poo',
						marker: '{',
						minChars: 0,
						template: '<li data-id="{id}">{fullName}</li>',
					}];
*/
					// align with the "markup" styles supported in jasper reports
					// and display more compatibly with varying style sheets
					CKEDITOR.config.coreStyles_bold = {element: 'b', overrides: 'strong'};
					CKEDITOR.config.coreStyles_italic = {element: 'i', overrides: 'em'};
					me._editor = CKEDITOR.replace('_CKEditor', {customConfig: SKYVE.Util.ckEditorConfigFileUrl});
					me._editor.setData(me.getValue());
					return this;
				};

				isc.WindowStack.popup(null,
										'Edit HTML', 
										true,
										[holder,
											isc.HLayout.create({
												membersMargin: 5,
												margin: 5,
												align: "right",
												members: [
													isc.IButton.create({
														width: 60, 
														title: 'Apply',
														click: function(applyEvent) {
															me.setValue(me._editor.getData());
															isc.WindowStack.popoff(false);
														}
													}),
													isc.IButton.create({
														width: 60, 
														title: 'Cancel',
														click: function(applyEvent) {
															isc.WindowStack.popoff(false);
														}
													})
												]
											})
										],
										420);
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

isc.ClassFactory.defineClass("GeometryItem", isc.CanvasItem);
isc.GeometryItem.addClassProperties({
	validOperators: ['geoWithin', 'geoContains', 'geoOverlaps', 'geoDisjoint', 'geoIntersects', 'geoTouches', 'geoCrosses', 'geoEquals', 'isNull', 'notNull']
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
	showHint: false,
	width: 100,
    operator: 'geoWithin',
	validOperators: isc.GeometryItem.validOperators,
	// this is going to be an editable data item
	shouldSaveValue: true
});
isc.GeometryItem.addMethods({
	createCanvas: function() {
		var me = this;
		var icons = [{
	        src: 'icons/map.png',
	        prompt: 'Click to set or see the geometry on a map',
	        click: function(form, item, icon) {
	        	var options = me.drawingTools ? {field: me, drawingTools: me.drawingTools} : {field: me};
	    		isc.WindowStack.popup(item.getPageRect(), 'Map', true, [isc.BizMapPicker.create(options)]);
	        }
		}];
		// Use a HLayout coz if we returned a DynamicForm here, 
		// CanvasItem would add the form items to the filter criteria automatically
		return isc.HLayout.create({
			height: 1,
			width: '100%',
			members: [
				isc.DynamicForm.create({
					writeFormTag: false, // ensure there are no nested forms
					numCols: 1,
					width: '100%',
					margin: 0,
					cellPadding: 0,
					items: [{name: 'value',
								type: 'text',
								showTitle: false,
								width: '*',
								hint: this.hint,
								showHintInField: true,
								selectOnFocus: true,
								changed: function(form, item, value) {
									me.storeValue(value, false);
								},
								icons: icons}]
				})
			]
		});
	},
	
	showValue: function(displayValue, dataValue, form, item) {
		item.canvas.getMember(0).setValue('value', displayValue);
	},

	setHint: function(hint) {
		this.canvas.getMember(0).setHint(hint);
	},
	
	// called from BizMapPicker
	setValueFromPicker: function(newValue) {
		// NB calls change events too.
		this.storeValue(newValue, true);
	}
});
isc.SimpleType.create({
    name: "geometry",
    inheritsFrom: "canvas",
	editorType: "GeometryItem",
	defaultOperator: 'gWithin',
	validOperators: isc.GeometryItem.validOperators
});

isc.ClassFactory.defineClass("GeometryMapItem", isc.CanvasItem);
//instance properties and methods
isc.GeometryMapItem.addProperties({
	width: '*',
	rowSpan: "*", 
	endRow: false, 
	startRow: false,
	canFocus: false,
	// this is going to be an editable data item
	shouldSaveValue: true
});

//Properties are :-
//
//width
//height
//name
//title
//icons
isc.GeometryMapItem.addMethods({
	createCanvas: function() {
		return isc.HLayout.create({defaultLayoutAlign: 'center'});
	},
	
	showValue: function(displayValue, dataValue) {
		if (! this._valueSetFromPicker) {
			var options = {
					field: this, 
					width: (this.width ? this.width : '100%'),
					height: (this.height ? this.height : '100%')};
			if (this.drawingTools) {
				options.drawingTools = this.drawingTools;
			}
			this.canvas.setMembers([
				isc.BizMapPicker.create(options)
			]);
		}
	},
	
	setDisabled: function(disabled) {
		this.canvas.getMember(0).setDisabled(disabled);
	},
	
	setValueFromPicker: function(newValue) {
		this._valueSetFromPicker = true;
		this.setValue(newValue);
		delete this._valueSetFromPicker;
		if (this.changed) {
			this.changed(this.form, this, newValue);
		}
	}
});

//register the editor
isc.SimpleType.create({
	name: "geometryMap",
	inheritsFrom: "canvas",
	editorType: "GeometryMapItem",
	validOperators: isc.GeometryItem.validOperators
});
