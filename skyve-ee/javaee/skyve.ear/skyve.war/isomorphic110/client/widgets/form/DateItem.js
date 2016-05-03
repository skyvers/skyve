/*
 * Isomorphic SmartClient
 * Version v11.0p_2016-03-31 (2016-03-31)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */
// Class will not work without the ListGrid
if (isc.ListGrid) {







isc.defineClass("NativeDateItem", "TextItem");

isc.NativeDateItem.addProperties({
    ariaRole: "range",

    browserInputType: "date",

    textBoxStyle: "nativeDateItem",
    inFieldHintStyle: "nativeDateItemInFieldHint",

    showClippedValueOnHover: false
});

isc.NativeDateItem.addMethods({

    _$wrapper: "wrapper",
    _getWrapperElementId : function () {
        if (this.isInactiveHTML()) return this._getDOMID(this._$wrapper);
        if (this.__wrapperId == null) {
            this.__wrapperId = this._getDOMID(this._$wrapper, true);
        }
        return this.__wrapperId;
    },

    _$hint: "hint",
    _getHintElementId : function () {
        if (this.isInactiveHTML()) return this._getDOMID(this._$hint);
        if (this.__hintId == null) {
            this.__hintId = this._getDOMID(this._$hint, true);
        }
        return this.__hintId;
    },

    _getInFieldHintElemStyle : function () {
        if (this.showDisabled && this.renderAsDisabled()) return this.inFieldHintStyle + "Disabled";
        return this.inFieldHintStyle;
    },

    getElementHTML : function (value, dataValue) {
        if (!this._getShowHintInField()) return this.Super("getElementHTML", arguments);

        var height = this.getHeight();
        return "<div id='" + this._getWrapperElementId() +
               "' class='" + this._getCellStyle(this._$wrapper) +
               "' style='display:inline-block;position:relative;height:" + height +
               "px'>" +
               this.Super("getElementHTML", arguments) +
               "<span id='" + this._getHintElementId() +
               "' class='" + this._getInFieldHintElemStyle() +
               "' style='line-height:" + height +
               "px'></span></div>";
    },

    operaForegroundColor: "black",
    getElementStyleHTML : function () {
        var styleHTML = this.Super("getElementStyleHTML", arguments);
        if (isc.Browser.isOpera) {
            
            styleHTML = styleHTML.substring(0, styleHTML.length - 2) +
                        ";color:" + this.operaForegroundColor +
                        styleHTML.substring(styleHTML.length - 2);
        }

        var startDate = isc.Date.getLogicalDateOnly(this.getStartDate()),
            endDate = isc.Date.getLogicalDateOnly(this.getEndDate());
        if (startDate != null) styleHTML += "min='" + this.mapValueToDisplay(startDate) + "' ";
        if (endDate != null) styleHTML += "max='" + this.mapValueToDisplay(endDate) + "' ";

        return styleHTML;
    },

    getElementValue : function () {
        var element = this.getDataElement(),
            value;
        if (!element || !(value = element.value)) return null;
        return Date.createLogicalDate(parseInt(value, 10),
                                      parseInt(value.substring(5), 10) - 1,
                                      parseInt(value.substring(8), 10));
    },

    setElementValue : function (newValue, dataValue) {
        if (!this.isDrawn()) return;
        var undef;
        if (dataValue === undef) {
            dataValue = this._value;
        }

        var element = this.getDataElement();
        if (element != null) {
            element.value = newValue;
        }
    },

    getStartDate : function () {
        var startDate = this.startDate;
        if (startDate != null && !isc.isA.Date(startDate)) {
            startDate = isc.DateItem.DEFAULT_START_DATE;
        }
        return startDate;
    },

    getEndDate : function () {
        var endDate = this.endDate;
        if (endDate != null && !isc.isA.Date(endDate)) {
            endDate = isc.DateItem.DEFAULT_END_DATE;
        }
        return endDate;
    },

    mapValueToDisplay : function (value) {
        if (isc.isA.Date(value)) {
            value = isc.Date.getLogicalDateOnly(value);
            return value.toSchemaDate();
        }
        return value;
    },

    _showInFieldHint : function () {
        var hintElem = isc.Element.get(this._getHintElementId());
        if (hintElem != null) {
            this.getDataElement().className = this._getInFieldHintStyle();

            hintElem.className = this._getInFieldHintElemStyle();
            var hint = this.getHint();
            hintElem.innerText = String.htmlStringToString(hint);
            this._showingInFieldHint = true;
        }
    },

    _hideInFieldHint : function (clearStyleOnly) {
        var hintElem = isc.Element.get(this._getHintElementId());
        if (hintElem != null) {
            this.getDataElement().className = this.getTextBoxStyle();

            hintElem.className = this._getInFieldHintElemStyle();
            hintElem.innerText = isc.emptyString;
            this._showingInFieldHint = false;
        }
    },

    updateState : function () {
        this.Super("updateState", arguments);
        if (this._showingInFieldHint) {
            var hintElem = isc.Element.get(this._getHintElementId());
            if (hintElem != null) hintElem.className = this._getInFieldHintElemStyle();
        }
    }
});

isc.defineClass("NativeDateTimeItem", "NativeDateItem");

isc.NativeDateTimeItem.addProperties({
    
    browserInputType: "datetime-local",

    textBoxStyle: "nativeDatetimeItem",
    inFieldHintStyle: "nativeDatetimeItemInFieldHint"
});

isc.NativeDateTimeItem.addMethods({
    getElementValue : function () {
        var element = this.getDataElement(),
            value;
        if (!element || !(value = element.value)) return null;
        // http://www.w3.org/TR/html5/infrastructure.html#valid-normalized-local-date-and-time-string
        var year = parseInt(value, 10),
            month = parseInt(value.substring(5), 10) - 1,
            date = parseInt(value.substring(8), 10),
            h = parseInt(value.substring(11), 10),
            m = parseInt(value.substring(14), 10),
            s = (value.length >= 19 ? parseInt(value.substring(17), 10) : 0);
        return new Date(Date.UTC(year, month, date, h, m, s));
    },

    mapDisplayToValue : function (value) {
        if (isc.Time._customTimezone && isc.isA.Date(value)) {
            return Date.createDatetime(value.getUTCFullYear(),
                                       value.getUTCMonth(),
                                       value.getUTCDate(),
                                       value.getUTCHours(),
                                       value.getUTCMinutes(),
                                       value.getUTCSeconds(),
                                       value.getUTCMilliseconds());
        }
        return value;
    },

    mapValueToDisplay : function (value) {
        if (isc.isA.Date(value)) {
            var displayValue = value.toSerializeableDate();
            // Date.toSerializeableDate() separates the date and time by a space. This needs
            // to be changed to a 'T'.
            return displayValue.substring(0, 10) + "T" + displayValue.substring(11);
        }
        return value;
    }
});


//> @class DateItem
// Item for manipulating Dates.
// <p>
// Can be rendered as a text field, or as 3 selects for day, month, year.  Includes optional
// pop-up picker.
// @inheritsFrom FormItem
// @example dateItem
// @visibility external
//<
isc.defineClass("DateItem", "ContainerItem");

isc.DateItem.addClassProperties({

    //> @classAttr DateItem.mapCache (object : {} : IRW)
    // Cache for the map of day, month and year values 
    // -- so we don't have to calculate the values over and over.
    // Items are keyed in the map by "<code>day|month|year</code>.<code>start</code>.<code>end</code>".
    //<
    mapCache:{},
 
    //> @type DateItemSelectorFormat
    // Order of pickers and which pickers are present when using a DateItem with
    // +link{dateItem.useTextField} false.
    DAY_MONTH_YEAR:"DMY", // @value isc.DateItem.DAY_MONTH_YEAR Output fields in day, month, year order.
    MONTH_DAY_YEAR:"MDY", // @value isc.DateItem.MONTH_DAY_YEAR Output fields in month, day, year order.
    YEAR_MONTH_DAY:"YMD", // @value isc.DateItem.YEAR_MONTH_DAY Output fields in year, month, day order.

    DAY_MONTH:"DM",  // @value isc.DateItem.DAY_MONTH Output only day, month fields.
    MONTH_DAY:"MD",  // @value isc.DateItem.MONTH_DAY Output only month, day fields.
    YEAR_MONTH:"YM", // @value isc.DateItem.YEAR_MONTH Output only year, month fields.
    MONTH_YEAR:"MY", // @value isc.DateItem.MONTH_YEAR Output only month, year fields.
    // @visibility external
    //<

    DEFAULT_START_DATE:Date.createLogicalDate(1995, 0, 1),
    DEFAULT_END_DATE:Date.createLogicalDate(2020, 11, 31),
    DEFAULT_CENTURY_THRESHOLD:25,
    
    chooserWidth:150, // @classAttr isc.DateItem.chooserWidth (number) Width of the date chooser -- used to choose a date graphically.
    getChooserWidth : function () {
        return (isc.DateChooser.getPrototype().width || isc.DateItem.chooserWidth);
    },
    chooserHeight:175, // @classAttr isc.DateItem.chooserHeight (number) Height of the date chooser -- used to choose a date graphically.
    getChooserHeight : function () {
        return (isc.DateChooser.getPrototype().height || isc.DateItem.chooserHeight);
    }

});

isc.DateItem.addProperties({

    defaultType: "date",

    //> @attr dateItem.textField (AutoChild TextItem : null : R)
    // Text field to hold the entire date in "type in" format, if +link{DateItem.useTextField,useTextField}
    // is true.
    //
    // @group dateItemAppearance
    // @visibility external
    //<

    // It's documented as an autoChild so Defaults / Properties are implied but
    // explicitly expose the 'properties' block as this is good to have clearly visible
    // for customization of items.
    //> @attr dateItem.textFieldProperties (TextItem properties : null : IRA)
    // Custom properties to apply to this dateItem's generated +link{dateItem.textField}.
    // Only applies if +link{dateItem.useTextField} is true.
    // @group dateItemAppearance
    // @visibility external
    //<

    textFieldDefaults: {
        name:"dateTextField", type:"text", changeOnBlur:true, width: "100%",

        _getShowHintInField : function () {
            return this.parentItem._getShowHintInField();
        },
        _getUsePlaceholderForHint : function () {
            
            if (!this.parentItem.usePlaceholderForHint) return false;
            return this._supportsPlaceholderAttribute();
        },
        getHint : function () {
            if (this._getShowHintInField()) return this.parentItem.hint;
            return null;
        },

        // on keypress run standard 'change' behavior to store the value
        // as this._value - also mark as "dirty"
        // This allows us to preserve partially typed entries across redraws
        // while the item has focus.
        // We clear the dirty flag when we actually update the DateItem's value
        // on blur, or in setValue() if we're changing to a new value.
        changeOnKeypress:true,
        changed : function () {
            this.isDirty = true;
        },

        // Override the blur method to update the DateItem value
        // Using blur rather than saveValue / change allows changeOnKeypress to
        // be set to true without the dateItem clobbering the user's half-typed
        // strings
        blur : function () {
            this.isDirty = false;
            if (this.parentItem) this.parentItem.updateValue();
        },

        shouldSaveValue:false,
        // Pick up textBoxStyle from the DateItem for simplicity of styling the sub-item.
        getTextBoxStyle : function () {
            var parentItem = this.parentItem;
            return parentItem && !parentItem._useNativeInput()
                   ? parentItem.getTextItemTextBoxStyle()
                   : this.Super("getTextBoxStyle", arguments);
        },

        // Determine our size based on our parents specified textBox size
        getTextBoxWidth : function (value) {
            if (this.parentItem) return this.parentItem.getTextBoxWidth(value);
            return this.Super("getTextBoxWidth", arguments);
        },

        _shouldUpdateParentItem: true,

        getDefaultValue : function () {
            if (this.parentItem) return this.parentItem.getDefaultValue();
            return this.Super("getDefaultValue", arguments);
        },
        
        // customize itemHoverHTML
        // - if itemHoverHTML is customized at the DateItem.js, call it
        // - otherwise we'll call the normal 'itemHoverHTML' method on the DynamicForm
        //   which will pick up this.prompt.
        itemHoverHTML : function (item, form) {
            if (this.parentItem) return this.parentItem.subItemHoverHTML(item, form);
        }
    },

    //> @attr dateItem.browserInputType (String : null : IRA)
    // If +link{DateItem.useTextField,useTextField} is true and browserInputType is set to
    // "date", then a native +externalLink{http://www.w3.org/TR/html5/forms.html#date-state-(type=date),HTML5 date input}
    // is used in place of a text input.
    // <p>
    // The use of a native HTML5 date input causes certain features to be disabled. Input masks,
    // the picker icon, and a custom +link{DateItem.dateFormatter,dateFormatter} are not supported.
    // +link{DateItem.showHintInField,In-field hints} are currently supported, but future browser
    // changes might force this support to be removed. Therefore, it is safest to <em>not</em>
    // use in-field hints (set showHintInField to false) in conjunction with a native HTML5 date
    // input.
    // <p>
    // <b>NOTE:</b> This feature requires specific CSS changes. Currently these changes have
    // been made to the Enterprise, EnterpriseBlue, and Graphite skins only.
    //
    // @visibility external
    //<
    

    // TextBoxStyle for the text-field textBox if showing
    
    textBoxStyle:"textItem",
    _sizeTextBoxAsContentBox : function () {
        return isc.Browser.isStrict;
    },
    getTextItemTextBoxStyle : function () {
        
        if (this.textField == null) return this.getTextBoxStyle();
        
        if (this._isPrinting() && this.printTextBoxStyle) {
            return this._getCellStyle(this.printTextBoxStyle, this._$printTextBox);
        }

        // Pick up the specified textBoxStyle from the DateItem, but call
        // _getCellStyle on the textItem directly.
        // This will ensure things like focus are picked up as expected.
        var tbStyle = (this.getCanEdit() == false && this.renderAsStatic() ? 
                this.getReadOnlyTextBoxStyle() : this.textBoxStyle),
            styleName = this.textField._getCellStyle(tbStyle, this._$textBox)
        ;
        return styleName;
    },
        
    //> @attr dateItem.wrapHintText (Boolean : false : IR)
    // @include FormItem.wrapHintText
    //<
    wrapHintText: false,

    //> @attr DateItem.showHintInField
    // If +link{DateItem.useTextField,useTextField} is true and a +link{FormItem.hint,hint} is
    // set, should the hint be shown within the field?
    // <p>
    // Note that when using a native HTML5 date input (see +link{DateItem.browserInputType}),
    // in-field hints are currently supported, but future browser changes might not allow
    // in-field hints to be supported. Therefore, it is safest to <em>not</em> use in-field
    // hints in conjunction with a native HTML5 date input.
    // <p>
    // To change this attribute after being drawn, it is necessary to call +link{FormItem.redraw()}
    // or redraw the form.
    // @include TextItem.showHintInField
    // @visibility external
    //<

    //> @attr DateItem.usePlaceholderForHint (boolean : true : IRA)
    // @include TextItem.usePlaceholderForHint
    // @visibility external
    //<
    
    usePlaceholderForHint: true,


    // default to equals if this.operator is unset, rather than being sensitive to textMatchStyle
    getOperator : function (textMatchStyle) {
        if (!this.operator) return "equals";
        return this.operator;
    },

    //> @attr DateItem.daySelector (AutoChild SelectItem : null : R)
    // Select item to hold the day part of the date.
    // @group dateItemAppearance
    // @visibility external
    //<

    
    
    //> @attr dateItem.daySelectorProperties (SelectItem properties : null : IRA)
    // Custom properties to apply to this dateItem's generated +link{dateItem.daySelector}.
    // @group dateItemAppearance
    // @visibility external
    //<
    
    daySelectorDefaults: {
        name: "daySelector", title:"Day", prompt: "Choose a day", type: "select", 
        valueMap: "this.parentItem.getDayOptions()", shouldSaveValue: false,
        // Override updateValue to update the parent.
        
        updateValue : function () {
            this.Super("updateValue", arguments);
            this.parentItem.updateValue();
        },
        // Don't adjust the selectors for errors (which are shown at the parent-item level)
        getErrorWidth:function () {return 0;},
        width: 45,
        // avoid additional changed events from this sub-item
        suppressItemChanged: true,
        
        // customize itemHoverHTML
        // - if itemHoverHTML is customized at the DateItem.js, call it
        // - otherwise we'll call the normal 'itemHoverHTML' method on the DynamicForm
        //   which will pick up this.prompt.
        itemHoverHTML : function (item, form) {
            if (this.parentItem) return this.parentItem.subItemHoverHTML(item, form);
        }
    },

    //> @attr DateItem.monthSelector (AutoChild SelectItem : null : R)
    // Select item to hold the month part of the date.
    // @group dateItemAppearance
    // @visibility external
    //<

    //> @attr dateItem.monthSelectorProperties (SelectItem properties : null : IRA)
    // Custom properties to apply to this dateItem's generated +link{dateItem.monthSelector}.
    // @group dateItemAppearance
    // @visibility external
    //<

    monthSelectorDefaults: {
        name: "monthSelector", title:"Month", prompt: "Choose a month", type: "select",
        valueMap: "this.parentItem.getMonthOptions()", shouldSaveValue: false,
        updateValue : function () {
            this.Super("updateValue", arguments);
            this.parentItem.updateValue();
        },
        // Don't adjust the selectors for errors (which are shown at the parent-item level)
        getErrorWidth:function () {return 0;},

        width: 55,
        // avoid additional changed events from this sub-item
        suppressItemChanged: true,
        
        
        pickListHeight: 1000,

        // customize itemHoverHTML
        // - if itemHoverHTML is customized at the DateItem.js, call it
        // - otherwise we'll call the normal 'itemHoverHTML' method on the DynamicForm
        //   which will pick up this.prompt.
        itemHoverHTML : function (item, form) {
            if (this.parentItem) return this.parentItem.subItemHoverHTML(item, form);
        }
    },

    //> @attr DateItem.yearSelector (AutoChild SelectItem : null : R)
    // Select item to hold the year part of the date.
    // @group dateItemAppearance
    // @visibility external
    //<

    //> @attr dateItem.yearSelectorProperties (SelectItem properties : null : IRA)
    // Custom properties to apply to this dateItem's generated +link{dateItem.yearSelector}.
    // @group dateItemAppearance
    // @visibility external
    //<
    yearSelectorDefaults: {
        name:"yearSelector", title:"Year", prompt:"Choose a year", type:"select",
        valueMap:"this.parentItem.getYearOptions()", shouldSaveValue:false,
        updateValue:function () {
            this.Super("updateValue", arguments);
            this.parentItem.updateValue();
        },
        // Don't adjust the selectors for errors (which are shown at the parent-item level)
        getErrorWidth:function () {return 0;},
        width:60,
        // avoid additional changed events from this sub-item
        suppressItemChanged: true,
        
        // customize itemHoverHTML
        // - if itemHoverHTML is customized at the DateItem.js, call it
        // - otherwise we'll call the normal 'itemHoverHTML' method on the DynamicForm
        //   which will pick up this.prompt.
        itemHoverHTML : function (item, form) {
            if (this.parentItem) return this.parentItem.subItemHoverHTML(item, form);
        }
    },
    
    // Default to 150 wide
    // This is an appropriate default if we're showing the text field 
    // If we're showing the selectors, this value will be forced to 200 during setItems
    width:150,

    cellPadding:0,

    //> @attr dateItem.useSharedPicker (Boolean : true : [IR])
    // When set to true (the default), use a single shared date-picker across all widgets that
    // use one.  When false, create a new picker using the autoChild system.  See 
    // +link{dateItem.pickerDefaults, picker} and 
    // +link{dateItem.pickerProperties, pickerProperties} for details on setting up an unshared
    // picker.
    // @visibility external
    //<
    useSharedPicker: true,

    //> @attr dateItem.pickerConstructor (string : "DateChooser" : [IR])
    // SmartClient class for the +link{FormItem.picker} autoChild displayed to allow the user
    // to directly select dates.
    // @visibility external
    //<
    pickerConstructor: "DateChooser",

    //> @attr dateItem.pickerDefaults (DateChooser Properties : see below : [IR])
    // Defaults for the +link{DateChooser} created by this form item.
    //<
    pickerDefaults: {
        border:"1px solid black;",
        // show a cancel button that closes the window
        showCancelButton: true,
        autoHide: true,
        closeOnEscapeKeypress: true
    },

    //> @attr dateItem.pickerProperties (DateChooser Properties : see below : [IR])
    // Properties for the +link{DateChooser} created by this form item.
    // @visibility external
    //<

    //> @attr dateItem.useTextField (Boolean : null : IR)
    // Should we show the date in a text field, or as 3 select boxes?
    // @group basics
    // @visibility external
    // @example dateItem
    //<
 
    //> @attr dateItem.textAlign (Alignment : varies : IRW)
    // If +link{dateItem.useTextField} is <code>true</code> this property governs the alignment
    // of text within the text field. Defaults to <code>"right"</code> by default or
    // <code>"left"</code> if the page is in +link{isc.Page.isRTL(),rtl mode}.
    // <p>
    // This attribute does not have an effect if a native HTML5 date input is being used.
    // See +link{DateItem.browserInputType}.
    //
    // @group appearance
    // @visibility external
    //<
    
    textAlign:isc.Page.isRTL() ? isc.Canvas.LEFT : isc.Canvas.RIGHT,

    //> @attr dateItem.useMask (boolean : null : IA)
    // If +link{dateItem.useTextField} is not <code>false</code> this property determines if
    // an input mask should be used. The format of the mask is determined by the 
    // +link{dateItem.inputFormat} or +link{dateItem.dateFormatter} (in that order).
    // <p>
    // This attribute does not have an effect if a native HTML5 date input is being used.
    // See +link{DateItem.browserInputType}.
    // @group basics
    // @see dateItem.maskDateSeparator
    // @visibility external
    //<

    //> @attr   dateItem.maskDateSeparator   (string : null : IA)
    // If +link{dateItem.useTextField} and +link{dateItem.useMask} are both <code>true</code>
    // this value is the separator between date components. If unset +link{Date.getDefaultDateSeparator()}
    // will be used.
    // @group basics
    // @visibility external
    //<
//    maskDateSeparator: "/",

    //> @attr dateItem.enforceDate  (Boolean : false : IRWA)
    // Can this field be set to a non-date value [other than null]?
    // <P>
    // When set to true, +link{formItem.setValue()} will return false without setting the item value
    // and log a warning if passed something other than a valid date value.
    // If the dateItem is showing a +link{dateItem.useTextField,free-form text entry field},
    // and a user enters a text value which cannot be parsed into a valid date, the item will
    // automatically redraw and display the +link{dateItem.invalidDateStringMessage} (though at this
    // point calling +link{formItem.getValue()} will return the string entered by the user).
    // <P>
    // When set to false, a user may enter a value that is not a valid date (for example, "Not
    // applicable") and the value will not immediately be flagged as an error.  However note
    // that for the value to actually pass validation you would need to declare the field as
    // not of "date" type, for example:
    // <pre>
    //     {name:"startDate", type:"dateOrOther", editorType:"DateItem", useTextField:true },
    // </pre>
    // The type "dateOrOther" could be declared as a +link{SimpleType}, with validators that
    // will accept either a valid date or certain special Strings (like "Not Available").
    // <P>
    // Only applies to dateItems where +link{dateItem.useTextField} is true. Non-Date values
    // are never supported in items where useTextField is false.
    // <p>
    // This attribute does not have an effect if a native HTML5 date input is being used.
    // See +link{DateItem.browserInputType}.
    //
    // @visibility external
    //<
    // Note: this is very similar to setting validateOnChange, with the exception of actually
    // rejecting setValue() calls with an invalid date.
    enforceDate:false,
    
    //> @attr dateItem.invalidDateStringMessage (string : "Invalid date" : IRW)
    // Validation error message to display if the user enters an invalid date
    // @visibility external
    // @group i18nMessages
    //<                                            
    invalidDateStringMessage:"Invalid date",
    
    //> @attr dateItem.showPickerIcon (boolean : true : IRW)
    // Should we show the pick button icon?
    // <p>
    // This attribute does not have an effect if a native HTML5 date input is being used.
    // See +link{DateItem.browserInputType}.
    // @visibility pickerIcon
    //<
    
    showPickerIcon:true,
    // Suppress the picker icon if we don't have the pickerConstructor class loaded.
    // This can occur when the Forms module is loaded without the Grid module.
    _shouldShowPickerIcon : function () {
        if (this._useNativeInput()) return false;
        if (isc[this.pickerConstructor] == null) {
            this.logWarn("Date Item pickerConstructor class '" + this.pickerConstructor +
                "' is not loaded. This property may have been modified incorrectly " +
                " or a required module may not be loaded. Suppressing the pickerIcon.");
            return false;
        }
        return this.Super("_shouldShowPickerIcon", arguments);
    },

    
    //> @attr dateItem.pickerIconWidth (number : 20: IRW)
    // Width for the date item's pickerIcon.
    // @visibility pickerIcon
    //<    
    pickerIconWidth:20,

    //> @attr dateItem.pickerIconHeight (number : 20 : IRW)
    // Height for the date item's pickerIcon.
    // @visibility pickerIcon
    //<    
    pickerIconHeight:20,
    
    //> @attr dateItem.pickerIconSrc (SCImgURL : "[SKIN]/DynamicForm/DatePicker_icon.gif" : IRW)
    // Src for the picker icon image
    // @visibility pickerIcon
    //<
    pickerIconSrc:"[SKIN]/DynamicForm/DatePicker_icon.gif", 
    
    // give the picker icon 3px of horizontal space by default
    pickerIconHSpace:3,

    //> @attr dateItem.pickerIconPrompt (HTMLString : "Show Date Chooser" : IR)
    // Prompt to show when the user hovers the mouse over the picker icon for this DateItem. May
    // be overridden for localization of your application.
    // @visibility external
    // @group i18nMessages
    //<
    pickerIconPrompt: "Show Date Chooser",

    
        
    //> @attr dateItem.startDate (Date : 1/1/1995 : IRW)
    // Minimum date the selectors will allow the user to pick.
    // <P>
    // <b>NOTE:</b> by design, setting <code>startDate</code> and <code>endDate</code> will not
    // always prevent the user from picking invalid values.  In particular:
    // <ul>
    // <li> the set of available days will only be restricted if the start and end dates fall
    // within the same month
    // <li> the set of available months will only be restricted if the start and end dates fall
    // within the same year
    // </ul>
    // <P>
    // This is <b>by design</b> as it allows the user to set the day, month and year in
    // whatever order is convenient, rather than forcing them to pick in a specific order.
    // <P>
    // For actual enforcement of a date being in correct range before data is submitted, a
    // +link{Validator} of type "dateRange" should always be declared.
    //
    // @group appearance
    // @visibility external
    //<
    

    //> @attr dateItem.endDate (Date : 12/31/2015 : IRW)
    // Maximum date the selectors will allow the user to pick.
    // <P>
    // See +link{dateItem.startDate} for details on how this restriction works.
    //
    // @group appearance
    // @visibility external
    //<
    

    //> @attr dateItem.centuryThreshold (number : 25 : IRW)
    // Only used if we're showing the date in a text field. When parsing a date, if the year
    // is specified with 1 or 2 digits and is less than the centuryThreshold, then the year will
    // be assumed to be 20xx; otherwise it will be interpreted according to default browser
    // behaviour, which will consider it to be 19xx.
    // <P>
    // If you need to allow 1 and 2 digit years, set this attribute to 
    // <code>null</code> to have the control retain your year-value as entered.
    // @group appearance
    // @visibility external
    //<
    centuryThreshold:isc.DateItem.DEFAULT_CENTURY_THRESHOLD,
    
    //> @attr DateItem.use24HourTime (Boolean : true : IRW)
    // When showing the +link{class:DateChooser} and the field is of type "datetime", whether
    // the +link{dateChooser.showTimeItem, time field} should be set to use 24-hour time.  The 
    // default is true.
    // <P>
    // Has no effect if +link{showPickerTimeItem} is explicitly set to <code>false</code>.
    // @visibility external
    //< 
    use24HourTime: true,

    //> @attr dateItem.showPickerTimeItem (Boolean : true : IRW)
    // If this field is of type <code>"datetime"</code>, when showing the
    // +link{class:DateChooser}, should the +link{dateChooser.showTimeItem,time field} be
    // displayed?
    // <P>
    // Has no effect for fields of type <code>"date"</code>.
    // <p>
    // This attribute does not have an effect if a native HTML5 date input is being used.
    // See +link{DateItem.browserInputType}.
    // @visibility external
    //<
    showPickerTimeItem:true

    //> @attr dateItem.pickerTimeItemProperties (TimeItem Properties : null : IRWA)
    // A set of properties to apply to the +link{class:TimeItem} displayed in the picker when
    // +link{showPickerTimeItem} is true.
    // <P>
    // Has no effect for fields of type <code>"date"</code>.
    // @visibility external
    //<

    //> @attr dateItem.dateFormatter (DateDisplayFormat : null : IRW)
    // If +link{dateItem.useTextField} is <code>true</code> this property can be used to 
    // customize the format in which dates are displayed for this item.<br>
    // Should be set to a standard +link{type:DateDisplayFormat}.
    // <P>
    // As with any formItem rendering out a date value, if no explicit dateFormatter is
    // supplied, dateFormatter will be derived from +link{DynamicForm.dateFormatter} or
    // +link{DynamicForm.datetimeFormatter},  depending on the specified +link{formItem.type} for
    // this field, if set, otherwise from the standard default +link{Date.setShortDisplayFormat()}
    // or +link{Date.setShortDatetimeDisplayFormat()}.
    // <P>
    // NOTE: For entirely custom formats, developers may apply a custom
    // <smartclient>
    // +link{dateItem.formatEditorValue()} method. To ensure the
    // DateItem is able to parse user-entered date strings back into Dates, for most cases
    // developers can specify an explicit +link{dateItem.inputFormat}, or if necessary a
    // custom +link{dateItem.parseEditorValue()}.
    // </smartclient>
    // <smartgwt>
    // <code>editorValueFormatter</code>. To ensure the
    // DateItem is able to parse user-entered date strings back into Dates, for most cases
    // developers can specify an explicit +link{dateItem.inputFormat}, or if necessary a
    // custom <code>editorValueParser</code>
    // </smartgwt>
    // <p>
    // This attribute does not have an effect if a native HTML5 date or datetime input is being used.
    // See +link{DateItem.browserInputType}.
    //
    // @visibility external
    //<
    
    
    
    //> @attr dateItem.displayFormat (DateDisplayFormat : null : IRW)
    // If +link{dateItem.useTextField} is <code>true</code> this property can be used to 
    // customize the format in which dates are displayed.<br>
    // Should be set to a standard +link{type:DateDisplayFormat} or
    // a function which will return a formatted date string.
    // <P>
    // If unset, the standard shortDate format as set up via +link{Date.setShortDisplayFormat()}
    // will be used.
    // <P>
    // <B>NOTE: you may need to update the +link{DateItem.inputFormat, inputFormat} to ensure the
    // DateItem is able to parse user-entered date strings back into Dates</B>
    // <p>
    // This attribute does not have an effect if a native HTML5 date input is being used.
    // See +link{DateItem.browserInputType}.
    // @see dateItem.inputFormat
    // @visibility external
    // @deprecated Use +link{dateItem.dateFormatter} instead.
    //<
    //displayFormat:"toShortDate"
    
    //> @attr  dateItem.inputFormat  (DateInputFormat : null : IRW)
    // If +link{dateItem.useTextField} is <code>true</code> this property can be used to specify
    // the input format for date strings. 
    // If unset, the input format will be determined based on the specified
    // +link{DateItem.dateFormatter} if possible (see +link{DateItem.getInputFormat()}), otherwise
    // picked up from the Date class (see +link{Date.setInputFormat()}).
    // <P>
    // Should be set to a standard +link{type:DateInputFormat}
    // <P>
    // Note that the +link{DateInputFormat} property is sufficient to parse date or datetime
    // strings specified in most standard date formats. However should an entirely custom
    // parsing function be required developers can 
    // <smartclient>implement a custom +link{dateItem.parseEditorValue()} method.</smartclient>
    // <smartgwt>apply a custom <code>editorValueParser</code> function.</smartgwt>
    // <p>
    // This attribute does not have an effect if a native HTML5 date input is being used.
    // See +link{DateItem.browserInputType}.
    //
    // @see dateItem.displayFormat
    // @visibility external
    //<
    //inputFormat:null,

    //> @attr dateItem.selectorFormat (DateItemSelectorFormat : null : IRW)
    // If showing date selectors rather than the date text field (so when 
    // <code>this.useTextField</code> is false), this property allows customization of the 
    // order of the day, month and year selector fields.  If unset these fields will match the
    // specified inputFormat for this item.
    // <P>
    // Note: selectors may be ommitted entirely by setting selectorFormat to (for example) 
    // <code>"MD"</code>. In this case the value for the omitted selector will match the
    // +link{formItem.defaultValue,defaultValue} specified for the item.  For example,
    // if the selector format is "MD" (month and day only), the year comes from the Date
    // specified as the defaultValue.
    //
    // @visibility external
    //<
    
    //selectorFormat:null

});

isc.DateItem.addMethods({

    init : function () {
        // Set the default value of useTextField if not explicitly defined
        if (this.useTextField == null) this.useTextField = this.useMask || false;
        else if (!this.useTextField && this.showTime) {
            this.logWarn("When showing the time component, useTextField must be true.");
            this.useTextField = true;
        }

        if (this.selectorFormat) {
            if (!this.selectorFormat.toUpperCase || this.selectorFormat.length == 0) {
                this.selectorFormat = null;
            } else {
                var format = this.selectorFormat.toUpperCase();
                if (!format.match("^[DMY]*$")) {
                    // invalid selectorFormat - this will cause incorrect rendering of select 
                    // items when useTextField is false - assume the default of null so that 
                    // default getSelectorFormat() bahavior applies
                    this.selectorFormat = null;
                }
            }
        }

        if (this.useTextField) {
            // if showing the textField, saveOnEnter needs to be true (as it is on the textField)
            this.saveOnEnter = true;
            // pass the readOnlyDisplay through to the textField (it may have been set in an
            // initialization properties block, directly on the DateItem)
            this.textFieldDefaults.readOnlyDisplay = this.readOnlyDisplay;
        }

        return this.Super("init", arguments);
    },

    //> @method dateItem.pendingStatusChanged()
    // Notification method called when +link{FormItem.showPending,showPending} is enabled and
    // this date item should either clear or show its pending visual state.
    // <p>
    // The default behavior is that the +link{FormItem.titleStyle,titleStyle} and
    // +link{FormItem.cellStyle,cellStyle} are updated to include/exclude the "Pending" suffix.
    // In addition, when displayed in the pending state:
    // <ul>
    // <li>If +link{attr:useTextField,useTextField} is <code>true</code>, then the "Pending"
    //     suffix will be appended to the +link{FormItem.textBoxStyle,textBoxStyle} applied to the
    //     +link{attr:textField,textField}; otherwise
    // <li>(<code>useTextField</code> is <code>false</code>) the color of the
    //     +link{attr:daySelector,daySelector}, +link{attr:monthSelector,monthSelector}
    //     and/or +link{attr:yearSelector,yearSelector} will change when the day, month, or year
    //     is different, respectively.
    // </ul>
    // Returning <code>false</code> will cancel this default behavior.
    // @include FormItem.pendingStatusChanged()
    //<
    _updatePendingStatuses : function () {
        var pendingStatus = this._getShowPending() && this.pendingStatus;

        if (this.useTextField) {
            var textField = this.textField;
            if (textField != null) textField.setFixedPendingStatus(pendingStatus);
        } else {
            

            var oldValue = this._getOldValue(),
                oldLogicalDate;
            if (!isc.isA.Date(oldValue)) oldLogicalDate = null;
            else if (oldValue.logicalDate) oldLogicalDate = oldValue;
            else oldLogicalDate = isc.Date.getLogicalDateOnly(oldValue);

            var newValue = this._value,
                newLogicalDate;
            if (!isc.isA.Date(newValue)) newLogicalDate = null;
            else {
                newLogicalDate = newValue;
                
            }

            var daySelector = this.daySelector;
            if (daySelector != null) {
                daySelector.setFixedPendingStatus(pendingStatus &&
                                                  (oldLogicalDate == null ||
                                                   newLogicalDate == null ||
                                                   oldLogicalDate.getDate() != newLogicalDate.getDate()));
            }

            var monthSelector = this.monthSelector;
            if (monthSelector != null) {
                monthSelector.setFixedPendingStatus(pendingStatus &&
                                                    (oldLogicalDate == null ||
                                                     newLogicalDate == null ||
                                                     oldLogicalDate.getMonth() != newLogicalDate.getMonth()));
            }

            var yearSelector = this.yearSelector;
            if (yearSelector != null) {
                yearSelector.setFixedPendingStatus(pendingStatus &&
                                                   (oldLogicalDate == null ||
                                                    newLogicalDate == null ||
                                                    oldLogicalDate.getFullYear() != newLogicalDate.getFullYear()));
            }
        }
    },

    _getShowHintInField : function () {
        return !!(this.useTextField && this.showHint && this.hint && this.showHintInField &&
                  
                  !(isc.Browser.isOpera && this._useNativeDatetimeInput()));
    },
    getHint : function () {
        if (!this.showHint || !this.hint || this._getShowHintInField()) return null;
        return this.hint;
    },

    _$date: "date",
    _$datetime: "datetime",
    _useNativeInput : function () {
        var browserInputType = this.getBrowserInputType();
        return this.useTextField && (browserInputType == this._$date ||
                                     browserInputType == this._$datetime);
    },
    _useNativeDatetimeInput : function () {
        return this.useTextField && this.getBrowserInputType() == this._$datetime;
    },

    // if selectorFormt is unset, back off to standard inputFormat.
    getSelectorFormat : function () {
        if (this.selectorFormat) { 
            return this.selectorFormat;
        } else if (this.inputFormat && isc.isA.String(this.inputFormat)) {
            return this.inputFormat;
        } else {
            var inputFormat = Date.getInputFormat();
            if (isc.isA.String(inputFormat)) return inputFormat;
            // Asssume US date format if we can't deduce the desired format from the date input
            // format
            this.logInfo("DateItem selectorFormat unspecified - assuming US format");
            return "MDY"
        }
    },

    getInputFormatMask : function (inputFormat) {
        
        var separator = this.maskDateSeparator || this._getDefaultDateSeparator();
        
        var mask;
        // Could use indexOf etc but quicker just to look at the standard set of options
        if (inputFormat == "YMD") {
            mask = [this._yearMask,separator,this._monthMask,separator,this._dayMask];
        } else if (inputFormat == "DMY") {
            mask = [this._dayMask,separator,this._monthMask,separator,this._yearMask];
        } else {
            // assume MDY as last valid format
            mask = [this._monthMask,separator,this._dayMask,separator,this._yearMask];
        }
        
        // Support DateTimeItem with additional mask
        if (isc.isA.DateTimeItem(this)) {
            mask.addList([" ",this._timeMask]);
        }
        return mask.join("");

    },
    _monthMask:"[01][0-9]",
    _dayMask:"[0-3]#",
    _yearMask:"####",
    _timeMask: "[0-2][0-9]:[0-6][0-9]",

    _maskDisplayFormats:{
        "MDY": "toUSShortDate",
        "DMY": "toEuropeanShortDate",
        "YMD": "toJapanShortDate"
    },

    //> @method dateItem.setItems() (A)
    //
    //  Override the setItems() routine to set the order of the fields according to this.dateFormat
    //<
    _getDefaultDateSeparator:function () {
        return Date.getDefaultDateSeparator();
    },
    _getDefaultDateSeparatorRegex : function () {
        var sep = this._getDefaultDateSeparator();
        return new RegExp(sep, "/g");
    },
    setItems : function (itemList) {
    
        var DI = isc.DateItem,
            format = this.getSelectorFormat()
        ;
        
        if (itemList != null && itemList.length != 0) {
            this.logWarn("setItems() called for dateItem with itemList:" + itemList + 
                            " - ignoring, and making use of default date fields");
        }

        // create a new itemList
        itemList = this.items = [];      

        if (this.useTextField) {
            // Setup properties that are being merged from the date item into the text field
            var mergeProperties = {
                textAlign: this.textAlign,
                emptyDisplayValue: this.emptyDisplayValue,
                operator: this.operator,
                title: this.title
            };
            
            var fieldProps = isc.addProperties({}, this.textFieldDefaults, DI.TEXT_FIELD,
                    this.textFieldProperties);

            var maskProperties = {};
            if (this._useNativeInput()) {
                maskProperties.textAlign = this.isRTL() ? isc.Canvas.RIGHT : isc.Canvas.LEFT;
                maskProperties.editorType = (this._useNativeDatetimeInput()
                                             ? isc.NativeDateTimeItem
                                             : isc.NativeDateItem);
                maskProperties.startDate = this.getStartDate(true);
                maskProperties.endDate = this.getEndDate(true);
            } else if (this.useMask) {
                var inputFormat = this.getInputFormat();
                // Default to US date format
                if (!inputFormat) inputFormat = "MDY";

                var mask = this.getInputFormatMask(inputFormat);

                maskProperties.mask = mask;
                maskProperties.maskSaveLiterals = true;
                
                // support maskOverwriteMode being set via textFieldProperties
                if (fieldProps.maskOverwriteMode == null) fieldProps.maskOverwriteMode = true;

                // Display format must match input so we force it here
                if (this.inputFormat) {
                    
                    this.dateFormatter = this._maskDisplayFormats[inputFormat];
                }
            }

            var textField = isc.addProperties({}, mergeProperties, fieldProps, maskProperties);
            
            textField.name = "dateTextField";
            // If we have a specified height, expand the text box to fill the available space
            
            if (this.textField) {
                
                textField.height = this.textField.height;
            } else if (this.height && (!this.textFieldProperties || !this.textFieldProperties.height)) 
            {
                textField.height = this.getInnerHeight();
            }

            itemList.add(textField);

            //>EditMode for dynamically changing useTextField
            
            var undef;
            this.daySelector = this.yearSelector = this.monthSelector = undef;
            //<EditMode
        
        } else {
            
            
            // iterate through the characters of the format
            for (var i = 0; i < format.length; i++) {
                var field = format.charAt(i);
                // assigning the selector for that format to the itemList
                var dayField, monthField, yearField,
                    item = null;
                if (field == "D") {
                    var dayField;
                    if (this.daySelectorProperties != null) {
                        dayField = isc.addProperties({}, this.daySelectorDefaults, DI.DAY_SELECTOR, this.daySelectorProperties);
                    } else {
                        dayField = isc.addProperties({}, this.daySelectorDefaults, DI.DAY_SELECTOR);
                    }
                    dayField.name = "daySelector";
                    // make the field wide enough to fully contain any of the values
                    if (this._dayChooserWidth == null) {
                        var valueHTML = this.getDayOptions().join("<br>");
                        this._dayChooserWidth = isc.Canvas.measureContent(valueHTML, null, null, { padding: 4 });
                        this._dayChooserWidth += this.getPickerIconWidth() + (this.selectorPadding * 2);
                    }
                    dayField.minWidth = this._dayChooserWidth;
                    dayField.width = this._dayChooserWidth;
                    item = dayField;
                    itemList.add(dayField);
                } else if (field == "M") {
                    var monthField;
                    if (this.monthSelectorProperties != null) {
                        monthField = isc.addProperties({}, this.monthSelectorDefaults, DI.MONTH_SELECTOR, this.monthSelectorProperties);
                    } else {
                        monthField = isc.addProperties({}, this.monthSelectorDefaults, DI.MONTH_SELECTOR);
                    }     
                    monthField.name = "monthSelector";
                    // make the field wide enough to fully contain any of the values
                    if (this._monthChooserWidth == null) {
                        var valueHTML = isc.getValues(this.getMonthOptions()).join("<br>");
                        this._monthChooserWidth = isc.Canvas.measureContent(valueHTML, null, null, { padding: 4 });
                        this._monthChooserWidth += this.getPickerIconWidth() + (this.selectorPadding * 2);
                    }
                    monthField.minWidth = this._monthChooserWidth;
                    monthField.width = this._monthChooserWidth;
                    item = monthField;
                    itemList.add(monthField);
                } else if (field == "Y") {
                    var yearField;
                    if (this.yearSelectorProperties != null) {
                        yearField = isc.addProperties({}, this.yearSelectorDefaults, DI.YEAR_SELECTOR, this.yearSelectorProperties);
                    } else {
                        yearField = isc.addProperties({}, this.yearSelectorDefaults, DI.YEAR_SELECTOR);
                    }
                    yearField.name = "yearSelector";
                    // make the field wide enough to fully contain any of the values
                    if (this._yearChooserWidth == null) {
                        var valueHTML = this.getYearOptions().join("<br>");
                        this._yearChooserWidth = isc.Canvas.measureContent(valueHTML, null, null, { padding: 6 });
                        this._yearChooserWidth += this.getPickerIconWidth() + (this.selectorPadding * 2);
                    }
                    yearField.minWidth = this._yearChooserWidth;
                    yearField.width = this._yearChooserWidth;
                    item = yearField;
                    itemList.add(yearField);
                }
                // Leave a gap between items via left-padding
                
                if (i > 0 && item) {
                    if (item.cssText == null) {
                        item.cssText = "padding-left:3px;";
                    }
                }
            }
        }

        // call the superclass routine to properly set the items
        this.Super("setItems", [itemList], arguments);

        if (this._getShowPending()) this._updatePendingStatuses();

        
        if (this.useTextField) {
            this.textField = this.dateTextField;
        }
    },
    
    // Hover notification from a sub-item
    subItemHoverHTML : function (item, form) {
        if (this.itemHoverHTML) {
            return this.itemHoverHTML(this, form);
        }
        return form.itemHoverHTML(item);
    },
    
    // override getInnerWidth().
    // If we're showing selectors, explicitly fit to them (ignore any specified size)
    
    getInnerWidth : function () {
        
        if (this.useTextField) {
            return this.Super("getInnerWidth", arguments);
        }
        
        var size = 0, 
            selectorCount = 0;
        if (this.daySelector) {
            selectorCount +=1;
            size += this.daySelector.width;
        }
        if (this.monthSelector) {
            selectorCount += 1;
            size += this.monthSelector.width;
        }
        if (this.yearSelector) {
            selectorCount += 1;
            size += this.yearSelector.width;
        }
        if (this.showPickerIcon) size += this.getPickerIconWidth();
        
        if (selectorCount > 0) size += (selectorCount-1) * this.selectorPadding;
        
        return size;
    },
    selectorPadding:2,

    // Override isEditable() to indicate that the user can edit this items value directly
    isEditable : function () {
        return true;
    },
    
    
    //> @method dateItem.getEnteredValue()
    // Returns the raw text value typed into this items text field if +link{dateItem.useTextField} 
    // is true (otherwise returns the result of this.getValue()).
    // @return (String) value the user entered
    // @visibility external
    //<
    getEnteredValue : function () {
        if (this.useTextField && this.textField != null) {
            return this.textField.getEnteredValue();
        }
        return this.getValue();
    },

    //> @method dateItem.setElementReadOnly()
    // Change the read-only state of the form element immediately.
    //<
    setElementReadOnly : function (readOnly) {
        // There is no element to mark read-only but we don't want the date field(s)
        // to be redrawn so we handle this method and do nothing. The text item or 
        // select items will be updated individually.
    },

    //> @method dateItem.setValue() (A)
    // Override setValue to set the values for the sub-items of the date.
    //<
    setValue : function (value) {        
        this._setValueCalled = true;    
        
        // may still be null if we're working with a text field
        var setToDefault = false;
        if (value == null) {
            var defaultValue = this.getDefaultValue();
            var undef;
            if (defaultValue !== undef) {
                value = defaultValue;
                setToDefault = true;
            }
        }

        var setToExisting = (isc.isA.Date(value) && isc.isA.Date(this._value)
                                    ? (this.useLogicalDates() 
                                        ? (Date.compareLogicalDates(value,this._value) == 0) 
                                        : (Date.compareDates(value, this._value) == 0)
                                      )
                                    : value == this._value);
        
        var date, invalidDate;
        // allow null values if useTextField is true and field is blank
        // Note - For consistency it would seem like 'allowEmptyValue' should be supported in
        // some way on DateItems, but we currently don't suport setting null dates on date items
        // showing selectors - 
        // not clear how this mechanism would work 
        // - once a date was null, presumably all 3 selectors would be showing "". 
        // - when the user then chose a value from one selector, would we default the other 2 to
        //   some default?
        // - similarly if the 3 selectors showed a valid date, how would the user set it to an
        //   empty date (one at a time?)
        if (isc.is.emptyString(value)) value = null;
        if (value == null) {
            invalidDate = true;
            
            date = value;
            // remove any value previously picked from the dateChooser (used for the time portion)
            if (this._lastPickedTime) delete this._lastPickedTime;
        } else {
            
            date = this.parseDate(value);
            // parseDate returns null if passed something it doesn't understand
            if (date == null) {
                invalidDate = true;
                date = value;
            }
        }
        if (invalidDate) {
        
            // If setValue() is called with an invalid date:
            // - if we're not showing a text field, essentially no-op, and maintain the current
            //   value - we have no way of displaying a non-date value
            // - if we're showing a text field
            //  - if this.enforceDate is false, just allow the non-date
            //  - if this.enforceDate is true, 2 possibilities:
            //      - the non-date was entered by a user, and setValue() has subsequently been
            //        called due to a redraw - check this._inavlidDate flag for this case, and
            //        silently allow the non-date
            //      - this method was called directly with a new non-date value. In this case 
            //        just log a warning and refuse to set the value.
            var dropDate;
            if (!this.useTextField) {
                dropDate = true;
            // explicitly support 'clearValue()' on a date field with a textItem even if
            // enforceDate is set
            } else if (this.enforceDate && value != null) {
                var textField = this.dateTextField;
                dropDate = !this._invalidDate || !textField || (textField.getValue() != value);
            }
                
            if (dropDate) {
                //>DEBUG
                this.logInfo("dateItem.setValue(): invalid date passed: '" + value + 
                            "'.  Ignoring this value. Non date values are only supported " +
                            " for dateItems where useTextField is true and enforceDate is false.");
                //<DEBUG
                return false;
                
            }
        }
        
        // If enforceDate was true, and we're changing from an invalidDate to a valid date,
        // clear errors.
        if (!invalidDate && this._invalidDate) {
            delete this._invalidDate;
            this.clearErrors();
            this.redraw();
        }
         
        // hang onto the value passed in
        this.saveValue(date, setToDefault);
        
        // Avoid attempting to parse / correct the dates in response to these setValues calls
        this._suppressUpdates = true;
        if (this.useTextField) {
            if (this.dateTextField) {
                // If the dateTextField is dirty this implies it has focus and the user
                // has entered some characters
                // Unless we're actually setting to a *new* date value, don't wipe out what
                // the user has entered.
                // This is required to ensure that if a redraw occurs 
                // (which calls setItemValues(), then falls through to setValue()) 
                // we don't lose a partially typed entry
                // If it's truly a new value, we can change the typed entry of course.
                if (setToExisting && this.dateTextField.isDirty) {
                    this.dateTextField.setValue(this.dateTextField._value);                    
                } else {
                    // re-format the date-string entered by the user if necessary
                    var textValue = invalidDate ? date : this.formatDate(date);
                    this.dateTextField.setValue(textValue);
                    delete this.dateTextField.isDirty;
                }
            }

        // set the day, month and year selectors
        } else {
            if (this.daySelector != null) this.daySelector.setValue(date.getDate());
            if (this.monthSelector != null) this.monthSelector.setValue(date.getMonth());
            if (this.yearSelector != null) this.yearSelector.setValue(date.getFullYear());
        }
        delete this._suppressUpdates;

        if (this._getShowPending()) this._updatePendingStatuses();

        return true;
    },


    // if we're doing a direct submit of the DateItem value, convert it to the 
    // dbDate format so it can be parsed on the server.
    _setHiddenDataElementValue : function (value) {
        var hde = this._getHiddenDataElement();
        if (hde != null) {
            if (isc.isA.Date(value)) hde.value = value.toDBDate();
            
            else hde.value = value;
        }
    },
    
    // Override getCellHeight() to ensure the containing form leaves enough space for this item.
    
    getCellHeight : function () {
        var cellHeight = this.Super("getCellHeight", arguments);
        if (isc.Browser.isIE && this.useTextField && isc.isA.Number(cellHeight)) cellHeight += 2;
        return cellHeight;
    },    
    
    
    elementChanged : function () {
        return;
    },

    // override getCriteriaValue and ensure we return a sensible date value
    getCriteriaValue : function () {
        return this.parseDate(this.getValue());
    },

    // Override updateValue to verify that the contents of the element(s) make a valid date.
    updateValue : function () {
        // _suppressUpdates flag set when we're in the process of setting our sub items' values
        // to represent a known, valid date.
        
        if (this._suppressUpdates) return;

        // We're likely to manipulate the values of the form items as this method runs - avoid
        // re-running updateValue in response to 'saveValue()' on the sub items.
        this._suppressUpdates = true;

        var date;
        if (this.useTextField) {
            // Note: this method is called from "saveValue()" on the sub-items (after saving out 
            // their values) so typically the sub item values will be up to date.
            // However this method may also be called externally while the text item is pending
            // an update (from blur [or keypress]).
            // Call updateValue() to ensure the text field value matches the current element
            // value for that field.
            this.dateTextField.updateValue();
            var value = this.dateTextField.getValue(),
                useNativeInput = this._useNativeInput(),
                invalidDate;

            var dateString = value;
            if (value == isc.emptyString || value == null) date = null;
            else if (useNativeInput) {
                date = value;
                dateString = this.formatDate(value);
            } else {
                // This will return a null value if the date string is invalid.
                // If enforceDate is false we allow a dateItem to be set to a non-date value
                // though typically validation would fail for the field if it's data-type was
                // date
                // If enforce date is true, accept this value, but show a validation error
                
                date = this.parseDate(value);
                if (date == null) {
                    invalidDate = true;
                    
                    // we're going to store the text value even though it's not a valid date
                    date = value;
                } else {
                       
                    // If the date was valid, the format may have slightly changed
                    // (01/01/01 -> 1/1/2001, for example) - if necessary update the text
                    // field here.
                    dateString = this.formatDate(date);
                    if (value != dateString) {
                        // we've set _suppressUpdates, so we won't end up in an infinite loop 
                        // from this call
                        this.dateTextField.setValue(dateString);
                    }
                }
            }
            // Date <--> formatted string logic is slightly ambiguous since even if we show
            // the time-component of a date, it is unlikely we show all the way down to "ms".
            // Therefore compare the formatted string in the field with the stored date value
            // (formatted to a string). If they match, treat the value as unchanged.
            
            if (this._value == date || 
                (isc.isA.Date(this._value) && (this.formatDate(this._value) == dateString)))
            {
                delete this._suppressUpdates;
                return;
            }

            // If enforceDate is true and we're showing an invalid date error, clear it unless
            // we still have an invalid date
            if (!useNativeInput && this.enforceDate) {
                if (this._invalidDate && !invalidDate) {
                    delete this._invalidDate;
                    this.clearErrors();
                    this.redraw();
                } else if (invalidDate) {
                    this.logWarn("Invalid date string entered in date text field :"+ date);
                    if (!this._invalidDate) {
                        this._invalidDate = true;
                        this.setError(this.invalidDateStringMessage);
                        
                        // We need to redraw to show the error. We don't want the user's entry
                        // to vanish, so we store it under a temp var. which the text field will
                        // display
                        
                        this.redraw();
                    }
                }
            }

        } else {
            // If we're not showing a text field, start with the last remembered date, and update
            // that based on the values in the selector items
            // This actually means we won't change the time value (which of course could not
            // be edited by the user)
            date = (this._value || this.getDefaultValue());
            // copy the date object to allow us to reset to _value if change handler fails
            date = date.duplicate();

            var day = date.getDate(),
                daySelector = this.daySelector,
                month = date.getMonth(),
                monthSelector = this.monthSelector,
                year = date.getFullYear(),
                yearSelector = this.yearSelector;

            // Store the specified day first, and apply it after setting month/year
            //
            // Note: Before setting month / year, we set the date to 1 so that setting the month
            // will not lead to an invalid date like Feb 30.
            // This avoids the case where 
            //  - the selectors are set to Feb 30, and the previous date was Jan 30.
            //  - the date object has 'setMonth()' called, setting the month is set to "Feb", 
            //    causing the date to be automatically updated to March 2
            //  - the day is set to 30 (from the date selector), leaving us with a date of
            //    March 30.
            //  At this point the logic to roll the days back to the end of the month would fail
            if (daySelector != null) {
                day = this.daySelector.getValue();
                if (day == null) day = date.getDate();
            }

            date.setDate(1);

            if (yearSelector != null) {
                year = yearSelector.getValue()
                if (year != null) date.setFullYear(year);
                else year = date.getFullYear();
            }

            if (monthSelector != null) {
                month = monthSelector.getValue();
                if (month != null) date.setMonth(month);
                else month = date.getMonth();
            }

            // Now set date to the appropriate "day" value
            // this is the value of the daySelector, or if we're not showing a day selector
            // the previously selected day value
            date.setDate(day);

            // If set to an invalid date, such as Feb 30, or Feb 29th on a non-leap year, the month 
            // will have been rolled forward (making it easy to catch such errors)
            // make sure the date's month is the same as that specified in the list
            // if it's not, we should roll back the day selector, and update the date to the 
            // appropriate day / month
            if (month != date.getMonth()) {
                // This rolls the date back to the end of the previous month
                day = day - date.getDate();
                if (daySelector != null) daySelector.setValue(day);
                date.setMonth(month);
                date.setDate(day);
            }

            if (isc.SimpleType.inheritsFrom(this.type, "datetime")) {
                // if we're showing choosers for day/month/year but the fieldtype is "datetime"
                // then the time portion can only be picked via the dateChooser - when that 
                // happens, we store off the _lastPickedTime
                var realValue = this._lastPickedTime ? this._lastPickedTime :
                        isc.isA.Date(this._value) ? this._value : null;
                if (realValue) {
                    var time = isc.Date.getLogicalTimeOnly(realValue);
                    date = isc.Date.combineLogicalDateAndTime(date, time);
                }
            }
        }
        delete this._suppressUpdates;

        return this.storeValue(date);
        
    },

    // Override saveValue(), fired from setValue and updateValue
    // Mark date as logicalDate:true if type is specified as "date"
    // This ensures that a simple JSON serialization of values from a form will treat dateItem
    // values as logical dates.
    saveValue : function (value) {
        if (isc.isA.Date(value) && value.logicalDate == null && value.logicalTime == null) {
            if (this.useLogicalDates()) value.logicalDate = true;
        }
        // Note that since dates are passed around by reference we can just pass the
        // arguments object to super and the change will have been picked up.
        var returnVal =  this.Super("saveValue", arguments);
        if (this._getShowPending()) this._updatePendingStatuses();
        return returnVal;
    },

    //> @method dateItem.resetValue()
    //      Overridden to get the value from the old value stored in the form, rather than
    //      replacing this item's value with the date object
    // @group elements
    //<
    resetValue : function () {
        var oldValue = this.form._oldValues[this.getFieldName()];
        if (isc.isA.Date(oldValue) && isc.isA.Date(this._value)) 
            oldValue = this._value.setTime(oldValue.getTime());
        this.setValue(oldValue);
    },    


    // getItemValue() - method to get the initial value of items when writing out this 
    // containerItem's innerHTML.
    // For the Date Item we give our sub items (selects / text item) the correct value when they
    // are initially set up.
    getItemValue : function (item, values) {
        
        if (isc.isAn.emptyObject(values)) values = null;
        
        var dateVal = isc.isA.Date(values),
            currDateVal = isc.isA.Date(this._value);
        
        if (values == this._value || 
            (dateVal && currDateVal && (Date.compareDates(values, this._value) == 0)))
        {
            return item.getValue();
        }
        
        // If we're rendering out inactiveItemHTML we may be showing a value that doesn't 
        // match the value stored by the form item. An example of this is showing
        // inactive editor HTML in grids where alwaysShowEditors is true.
        if (item == this.dateTextField) return dateVal ? this.formatDate(values) : values;
        else if (item == this.daySelector) return dateVal ? values.getDate() : null;
        else if (item == this.monthSelector) return dateVal ? values.getMonth() : null;
        else if (item == this.yearSelector) return dateVal ? values.getFullYear() : null;
        
    },
    
    // Override getDisplayValue() to return the short-date formatted value.
    
    getDisplayValue : function () {
        var dataValue = this.getValue();
        if (!isc.isA.Date(dataValue)) return this.Super("getDisplayValue", arguments);
        if (this.useTextField || !this.items) {
            return this.formatDate(dataValue);
        } else {
            // If we're undrawn the sub items won't yet be populated! Do this now.
            if (!this.isDrawn()) {
                
                if (this.yearSelector) this.yearSelector.setValue(dataValue.getFullYear());
                if (this.monthSelector) this.monthSelector.setValue(dataValue.getMonth());
                if (this.daySelector) this.daySelector.setValue(dataValue.getDate());
            }
            // This will give us a the contents of each selector separated by a space,
            // for example "Jun 25 2009" for MDY dates
            return this.items.map("getDisplayValue").join(" ");
        }
    },
    
    //> @method dateItem.getDefaultValue() (A)
    //  Override getDefaultValue to guarantee that it returns a date if 
    //  <code>item.enforceDate</code> is true. If no default date is supplied, defaults to the
    //  current date.
    //<
    // Note: As currently written this method will not consistently return the same date instance
    // unless this.defaultValue is explicitly specifed as a date object. Instead we create a
    // new date instance each time the method is called and return that. 
    // This can be a gotcha - for exmaple when checking for changes to a date item we have to 
    // use compareDates() rather than ==.
    getDefaultValue : function () {
        var value = this.Super("getDefaultValue");
        if (!isc.isA.Date(value)) {
            var dateValue = this.parseDate(value);
            if (isc.isA.Date(dateValue)) value = dateValue;
            else if (!this.useTextField || this.enforceDate) {
                var replaceDefaultValue;
                if (value != null) {
                    this.logWarn("Default DateItem value provided as:" + value + 
                             ". This is not recognized as a valid date - defaulting to a new date");
                    // if this came from a static default value, replace it so we don't see
                    // multiple warnings
                    replaceDefaultValue = this.defaultValue == value;
                }
                
                // if we still don't have a valid date, default to a new Date().
                // NOTE: can't just set the defaultValue to "new Date()" as this object would then
                // be shared amongst all date instances
                // Exception: We DO support null value for dateItems where useTextField is true
                // even if enforceDate is set.
                if (!this.useTextField) value = this._getEmptyDate();
                
                if (replaceDefaultValue) this.defaultValue = value;
            }
        }
        return value;
    },
    
    _getEmptyDate : function () {
        var value = Date.createLogicalDate();
        return value;
    },
    
    // useLogicalDates(): does this item produce "logical date" values, or datetime values?
    // If this item has type explicitly specified as "date", we work with "logical date" objects.
    // These are native dates where the time component is basically opaque and is set such that
    // the date is always correct in browser native local time.
    // We never apply developer-specified custom timezones to logical dates.
    // If this item's type is unspecified, or the item is specified as type "datetime", this item
    // will produce datetime values (values where the time is meaningful and will be formatted
    // according to the timezone offset specified by the developer, if there is one).
    // See +link{DateUtil.createLogicalDate} for more on this.
    
    useLogicalDates : function () {
        var type = this.getType(),
            isDateField = isc.SimpleType.inheritsFrom(type, "date"),
            isDatetimeField = isc.SimpleType.inheritsFrom(type, "datetime");
        return type != null && isDateField && !isDatetimeField
    },

    //> @method dateItem.getStartDate() (A)
    // use this method, rather than referring to this.startDate, to guarantee that it
    //      returns a date
    //      Note - Does not update this.startDate - should it?
    //<
    getStartDate : function (allowNull) {
        var startDate = this.startDate;
        if (isc.isA.String(startDate)) startDate = this.parseDate(this.startDate);
        if ((startDate == null && !allowNull) || !isc.isA.Date(startDate)) {
            //>DEBUG
            if (startDate != null) {
            	this.logWarn("startDate was not in valid date format - using default start date");
            }
            //<DEBUG
            startDate = isc.DateItem.DEFAULT_START_DATE;
        }
        return startDate;
    },

    //> @method dateItem.setStartDate() (A)
    // Setter for +link{DateItem.startDate}.
    // @param startDate (LogicalDate | String) the new startDate.
    // @visibility external
    //<
    setStartDate : function (startDate) {
        this.startDate = startDate;

        var textField = this.dateTextField;
        if (textField && this._useNativeInput()) {
            startDate = this.getStartDate(true);
            textField.startDate = startDate;
            var dataElement = textField.getDataElement();
            if (startDate == null) dataElement.removeAttribute("min");
            else {
                dataElement.min = textField.mapValueToDisplay(startDate);
            }
        }

        this.updateValue();
    },

    //> @method dateItem.getEndDate() (A)
    // use this method, rather than referring to this.endDate, to guarantee that it
    //      returns a date
    //<
    getEndDate : function (allowNull) {
        var endDate = this.endDate;
        if (isc.isA.String(endDate)) endDate = this.parseDate(this.endDate);
        if ((endDate == null && !allowNull) || !isc.isA.Date(endDate)) {
            //>DEBUG
            if (endDate != null) {
            	this.logWarn("endDate was not in valid date format - using default end date");
            }
            //<DEBUG
            endDate = isc.DateItem.DEFAULT_END_DATE;
        }
        return endDate;
    },

    //> @method dateItem.setEndDate() (A)
    // Setter for +link{DateItem.endDate}.
    // @param endDate (LogicalDate | String) the new endDate.
    // @visibility external
    //<
    setEndDate : function (endDate) {
        this.endDate = endDate;

        var textField = this.dateTextField;
        if (textField && this._useNativeInput()) {
            endDate = this.getEndDate(true);
            textField.endDate = endDate;
            var dataElement = textField.getDataElement();
            if (endDate == null) dataElement.removeAttribute("max");
            else {
                dataElement.max = textField.mapValueToDisplay(endDate);
            }
        }

        this.updateValue();
    },

    
    _canFocus : function () {
        if (this.canFocus != null) return this.canFocus;
        return true;
    },
    
    // Override focusInItem to focus in the appropriate sub-item
    focusInItem : function () {
        if (!this.isVisible()) return;

        if (this.useTextField) {
            if (this.dateTextField) this.dateTextField.focusInItem();
        } else {
            var format = this.getSelectorFormat(),

                // Format will be "DMY" / "YMD" / "MDY" / etc.
                // (Parse the string rather than comparing with the DateItem.DAY_MONTH_YEAR class 
                // constants - it's slower but will support the user specifying just "MY" or something)
                firstSelector = format.charAt(0)
            ;
            
            if (firstSelector == "D" && this.daySelector) this.daySelector.focusInItem();
            if (firstSelector == "M" && this.monthSelector) this.monthSelector.focusInItem();
            if (firstSelector == "Y" && this.yearSelector) this.yearSelector.focusInItem();
        }
        // If it couldn't find the appropriate sub-item, this method is a no-op        
    },
    
    // override get/setSelectionRange - if we're showing a text field, call through to the
    // methods on that sub-item
    
    //> @method dateItem.setSelectionRange()
    // If +link{dateItem.useTextField} is true, falls through to standard
    // +link{textItem.setSelectionRange(),setSelectionRange()} implementation on this items freeform text entry field.
    // Otherwise has no effect.
    // @param start (int) character index for start of new selection
    // @param end (int) character index for end of new selection
    // @visibility external
    //<
    setSelectionRange : function (start,end) {
        if (this.dateTextField) return this.dateTextField.setSelectionRange(start,end);
    },

    //> @method dateItem.getSelectionRange()
    // If +link{dateItem.useTextField} is true, falls through to standard
    // +link{textItem.getSelectionRange(),getSelectionRange()} implementation on this items freeform text entry field.
    // Otherwise has no effect.
    // @return (array) 2 element array indicating start/end character index of current selection
    //  within our text entry field. Returns null if this item is undrawn or doesn't have focus.
    // @visibility external
    //<
    getSelectionRange : function () {
        if (this.dateTextField) return this.dateTextField.getSelectionRange();
    },
    
    //> @method dateItem.selectValue()
    // If +link{dateItem.useTextField} is true, falls through to standard
    // +link{textItem.selectValue(),selectValue()} implementation on this items freeform text entry field.
    // Otherwise has no effect.
    // @visibility external
    //<
    selectValue : function () {
        if (this.dateTextField) return this.dateTextField.selectValue();
    },
    
    //> @method dateItem.deselectValue()
    // If +link{dateItem.useTextField} is true, falls through to standard
    // +link{textItem.deselectValue(),deselectValue()} implementation on this items freeform text entry field.
    // Otherwise has no effect.
    // @param [start] (Boolean) If this parameter is passed, new cursor insertion position will be
    //   moved to the start, rather than the end of this item's value.
    // @visibility external
    //<
    deselectValue : function (start) {
        if (this.dateTextField) return this.dateTextField.deselectValue()
    },
    
    //> @method dateItem.getDayOptions() (A)
    // Return the list of options for the day selector.
    //
    // @return (array) Array of day numbers from 1-31;
    //<
    getDayOptions : function () {

        var startDate = this.getStartDate(),
            endDate = this.getEndDate();

        // If the date range spans more than one month, return [1 - 31]
        // Only time we want to have this return a range smaller than 1-31 is if we have a range
        // within a single month (Feb 2 - 20th, 1945), for example.  Otherwise we force the
        // user to pick fields in a specific order.
        var startDay = 1, 
            endDay = 31;
        
        // If it's within a single month in a year, return appropriate subset of days    
        if (startDate.getYear() == endDate.getYear() &&
            startDate.getMonth() == endDate.getMonth()) 
        {
            startDay = startDate.getDate()
            endDay = endDate.getDate()
        }

        // if the list of options is already in the mapCache, just pull it from there
        var key = "day." + startDay + "." + endDay;
        if (isc.DateItem.mapCache[key]) return isc.DateItem.mapCache[key];

        // otherwise build the options and store it in the dayMapCache
        var options = isc.DateItem.mapCache[key] = [];
        for (var i = startDay; i <= endDay; i++) options[i - startDay] = i;

        return options;
    },

    //> @method dateItem.getMonthOptions() (A)
    // Return the list of options for the month selector.
    //
    // @return (array) Object of month number (0-based!) to short month name ["Jan","Feb",...]
    //<
    getMonthOptions : function () {

        var startDate = this.getStartDate(),
            endDate = this.getEndDate();
            
        // If the date range spans more than one year, return ["Jan" - "December"]
        // Only time we want to have this return an incomplete range is if we have a range
        // within a single year (Feb - April, 1945), for example.  Otherwise we force the user
        // to pick fields in a specific order.
        var startMonth = 0, 
            endMonth = 11;

        // If it's within a single month in a year, return appropriate subset of days    
        if (startDate.getYear() == endDate.getYear()) {
            startMonth = startDate.getMonth()
            endMonth = endDate.getMonth()
        }

        // if the list of options is already in the mapCache, just pull it from there
        var key = "month." + startMonth + "." + endMonth;
        if (isc.DateItem.mapCache[key]) return isc.DateItem.mapCache[key];

        // otherwise build the options and store it in the dayMapCache
        var options = isc.DateItem.mapCache[key] = {};

        // get the list of names as an array
        var monthNames = Date.getShortMonthNames();
        // and convert it to an object
        for (; startMonth <= endMonth; startMonth++) {
            options[startMonth] = monthNames[startMonth];
        }

        return options;
    },

    //> @method dateItem.getYearOptions() (A)
    // Return the list of options for the year selector.
    //
    // @return (array) Array of day numbers from this.startYear - this.endYear;
    //<
    getYearOptions : function () {

        var startYear = this.getStartDate().getFullYear(),
            endYear = this.getEndDate().getFullYear();

        // if the list of options is already in the mapCache, just pull it from there
        var key = "year." + startYear + "." + endYear;
        if (isc.DateItem.mapCache[key]) return isc.DateItem.mapCache[key];

        // otherwise build the options and store it in the dayMapCache
        var options = isc.DateItem.mapCache[key] = [];
        for (var i = startYear; i <= endYear; i++) {
            options[i-startYear] = i;
        }
        return options;
    },

    //> @method dateItem.parseDate()
    // Parse a date passed in as a string.
    // @group elements
    //
    // @param dateString (string) date value as a string
    // @param inputFormat (DateInputFormat) format for date strings to be parsed
    //
    // @return (date) date value
    //<
    parseDate : function (dateString, inputFormat) {
        if (isc.isA.Date(dateString)) return dateString;

        // If there is a custom editValue parser applied to this item, bypass standard
        // Date parsing logic entirely
        if (this.parseEditorValue != null) {
            var dateVal = this.parseEditorValue(dateString, this.form, this);
            return dateVal;
        }

        if (inputFormat == null) inputFormat = this.getInputFormat();
        
        var isLogicalDate = this.useLogicalDates();
        
        var date = Date.parseInput(dateString, inputFormat, 
                                this.centuryThreshold, true, !isLogicalDate);
        return date;
    },
    
    // formatDate() - given a live date object, returns the formatted date string to display
    // Only applies if useTextField is true.
    formatDate : function (date) {
        if (this.formatEditorValue != null) {
            var record = this.form ? this.form.values : {};
            return this.formatEditorValue(date, record, this.form, this);
        }
        if (!isc.isA.Date(date)) return date;
        
        if (this.format && !this.getCanEdit()) {
            return isc.DateUtil.format(date, this.format);
        }
        
        var formatter = this._getDateFormatter(),
            type = this.getType(),
            isDateField = isc.SimpleType.inheritsFrom(type, "date"),
            isDatetimeField = isc.SimpleType.inheritsFrom(type, "datetime");
        // this.showTime - undocumented flag to use 'toShortDatetime' rather than 'toShortDate'
        // when formatting the date. Used by the DateTimeItem subclass.
        
        return this.showTime ? date.toShortDatetime(formatter, isDatetimeField || !isDateField)
                             : date.toShortDate(formatter, isDatetimeField || !isDateField);
    },

    //>@method dateItem.parseEditorValue() (A)
    // Convert a text value entered in this item's text field to a final data value 
    // for storage.
    // <P>
    // If +link{dateItem.useTextField} is true, entirely custom date formatting and
    // parsing logic may be applied via overrides to +link{parseEditorValue} and
    // +link{formatEditorValue}. These methods apply to this FormItem only - 
    // system-wide Date and Datetime formatting and parsing may also be customized via
    // the APIs on the +link{Date} class. See +link{group:dateFormatAndStorage} for more
    // on this.
    // <P>
    // Note: custom parsing for this item may also be achieved by modifying the
    // +link{dateItem.inputFormat}. This mechanism provides support many common date formats
    // without the need for an entirely custom parser function.
    //
    // @param value (string) value as entered by the user
    // @param form (DynamicForm) pointer to the dynamicForm containing this item
    // @param item (FormItem) pointer to this item
    // @return (any) Data value to store for this item.
    // @visibility external
    //<
    
    //>@method dateItem.formatEditorValue() (A)
    // Convert this item's data value to a text value for display in this item's
    // text field.
    // <P>
    // If +link{dateItem.useTextField} is true, entirely custom date formatting and
    // parsing logic may be applied via overrides to +link{parseEditorValue} and
    // +link{formatEditorValue}. These methods apply to this FormItem only - 
    // system-wide Date and Datetime formatting and parsing may also be customized via
    // the APIs on the +link{Date} class. See +link{group:dateFormatAndStorage} for more
    // on this.
    // <P>
    // Note: custom formatting for this item may also be achieved via the
    // +link{dateItem.dateFormatter} which allows you to directly specify various standard
    // date display formats.
    // @param value (any) Underlying data value to format. May be null.
    // @param record (ListGridRecord) The record currently being edited by this form.
    //      Essentially the form's current values object.
    // @param form (DynamicForm) pointer to the DynamicForm
    // @param item (FormItem) pointer to the FormItem
    // @return (string) display value to show in the editor.
    // @visibility external
    //<
    
    //>@method dateItem.getInputFormat() (A)
    // If +link{dateItem.useTextField} is <code>true</code> this method returns a
    // standard +link{type:DateInputFormat}, determining how values entered
    // by the user are to be converted to Javascript Date objects.
    // <P>
    // If an explicit +link{DateItem.inputFormat} has been specified it will be returned,
    // otherwise, the input format will be automatically derived from the
    // +link{dateItem.dateFormatter} property.
    // <P>
    // Note that the inputFormat will ignore any separator characters and padding of values.
    // However if necessary entirely custom date formatting and parsing may be achieved via the
    // <smartclient>+link{dateItem.formatEditorValue()} and
    // +link{dateItem.parseEditorValue()} methods.</smartclient>
    // <smartgwt><code>setEditorValueFormatter()</code> and 
    // <code>setEditorValueParser()</code> APIs.</smartgwt>
    // 
    // @return (DateInputFormat) expected format of date strings to parse
    // @visibility external
    //<
    getInputFormat : function () {
        // developer may explicitly specify an inputFormat (this used to be the only way to change
        // input/display format for text-based date items)
        if (this.inputFormat) return this.inputFormat;
        
        // If a display format, but no inputFormat is specified attempt to derive the inputFormat
        // from the displayFormat. This works for the standard shortDate display formatters but
        // you'll still need to specify an explicit input format for anything more exotic
        var dateFormatter = this._getDateFormatter();
        return Date.mapDisplayFormatToInputFormat(dateFormatter);
    },

    // Methods effecting the dateChooser
    
    getPickerIcon : function (a,b,c,d) {
        var icon = this.invokeSuper(isc.DateItem, "getPickerIcon", a,b,c,d);
        if (icon.prompt == null) icon.prompt = this.pickerIconPrompt;
        return icon;
    },
    
    //> @method DateItem.getFiscalCalendar()
    // Returns the +link{FiscalCalendar} object that will be used by this item's DateChooser.
    //
    // @return (FiscalCalendar) the fiscal calendar for this chooser, if set, or the global
    //            one otherwise
    // @visibility external
    //<
    getFiscalCalendar : function () {
        return this.fiscalCalendar || Date.getFiscalCalendar();
    },

    //> @method DateItem.setFiscalCalendar()
    // Sets the +link{FiscalCalendar} object that will be used by this item's DateChooser.  If 
    // unset, the +link{Date.getFiscalCalendar, global fiscal calendar} is used.
    //
    // @param [fiscalCalendar] (FiscalCalendar) the fiscal calendar for this chooser, if set, or the global
    //            one otherwise
    // @visibility external
    //<
    setFiscalCalendar : function (fiscalCalendar) {
        this.fiscalCalendar = fiscalCalendar;
    },

    // override 'showPicker' - instead of creating a picker instance we're reusing a shared
    // one.
    showPicker : function () {
        if (isc[this.pickerConstructor] == null) {
            this.logWarn("Date Item pickerConstructor class '" + this.pickerConstructor +
                "' is not loaded. This property may have been modified incorrectly " +
                " or a required module may not be loaded. Refusing to show picker.");
            return;
        }

        if (!this.form._setValuesPending) this.updateValue();

        var handsetDefaults = {};
        if (isc.Browser.isHandset) {
            handsetDefaults.width = isc.Page.getWidth();
            handsetDefaults.height = isc.Page.getHeight();
            handsetDefaults.left = 0;
            handsetDefaults.top = 0;
        }
        var pickerProps = isc.addProperties({}, this.pickerDefaults, handsetDefaults, this.pickerProperties);

        if (!this.picker) {
            if (this.useSharedPicker) {
                var props = isc.addProperties({}, {
                        fiscalCalendar: this.getFiscalCalendar(),
                        showFiscalYearChooser: this.showChooserFiscalYearPicker,
                        showWeekChooser: this.showChooserWeekPicker
                    }, pickerProps);
                this.picker = isc[this.pickerConstructor].getSharedDateChooser(props);
            } else {
                this.picker = isc[this.pickerConstructor].create(
                    isc.addProperties({}, pickerProps, 
                        {
                            border: "none",
                            _generated:true,
                            // When re-using a DateChooser, we're almost certainly displaying it as a 
                            // floating picker rather than an inline element. Apply the common options for 
                            // a floating picker
                            autoHide:true,
                            showCancelButton:true,
                            closeOnEscapeKeypress: true
                        }
                    )
                );
            }
            
            // in the case of SGWT, this.picker is not drawn after creation, so it needs to be drawn
            // in order to place it properly after it has its final dimensions.
            if (!this.picker.isDrawn()) {
                // place it offscreen before draw to avoid it appears briefly at the wrong location
                this.picker.moveTo(null, -9999);
                this.picker.draw();
            }

        } else {
            if (isc.getKeys(pickerProps).length > 0) {
                // if pickerProperties were applied to the item, apply them to the picker now
                this.picker.setProperties(pickerProps);
            }
        }


        var picker = this.picker;

        var oldItem = picker.callingFormItem;
        if (oldItem != this) {
            if (oldItem) oldItem.ignore(picker, "dataChanged");
            this.observe(picker, "dataChanged", "observer.pickerDataChanged(observed)");
            
            picker.callingFormItem = this;
            picker.callingForm = this.form;
            
            picker.locatorParent = this.form;
        }
        
        picker.startYear = this.getStartDate().getFullYear();
        picker.endYear = this.getEndDate().getFullYear();

        // set the year and week attributes
        picker.fiscalCalendar = this.getFiscalCalendar();
        picker.showFiscalYearChooser = this.showChooserFiscalYearPicker;
        picker.showWeekChooser = this.showChooserWeekPicker;
        
        // show a TimeItem in the picker if type is datetime
        var showTimeItem = this.shouldShowPickerTimeItem();

        picker.showTimeItem = showTimeItem;
        picker.use24HourTime = this.use24HourTime;
        if (this.pickerTimeItemProperties) picker.timeItemProperties = this.pickerTimeItemProperties;

        // We must do a reflow of the layout now, so the FormItem.showPicker method
        // can calculate a correct size for the widget, or it will be misplaced
        if (this.shouldShowPickerTimeItem() && this.picker.timeLayout.visibility == isc.Canvas.HIDDEN) {
            // if hidden, the timeLayout has to be shown for the calculated size after
            // reflowNow to be correct.
            this.picker.timeLayout.show();
            picker.reflowNow();
        } else {
            picker.reflowNow();
            if (!this.shouldShowPickerTimeItem() && this.picker.timeLayout.visibility != isc.Canvas.HIDDEN)
                this.picker.timeLayout.hide();
        }

        // Default showPicker implementation will call setData() with the result
        // of this.getValue() or this.getPickerData().
        var returnVal = this.Super("showPicker", arguments);
        // call updateUI once the picker's date has been set.
        if (picker.updateUI) picker.updateUI();

        return returnVal;
    },

    shouldShowPickerTimeItem : function () {    
        if (this.showPickerTimeItem == false) return false;
        return isc.SimpleType.inheritsFrom(this.type, "datetime");
    },
    
    // custom code to center the picker over the picker icon
    getPickerRect : function () {
        // we want the date chooser to float centered over the picker icon.
        var left = this.getPageLeft(),
            top = this.getPageTop(),
            
            chooserWidth = isc.DateItem.getChooserWidth() + 3,
            chooserHeight = isc.DateItem.getChooserHeight() + 3
        ;

        left += Math.round((this.getVisibleWidth() - (this.getPickerIconWidth() /2)) - 
                (chooserWidth/2));
        
        top += Math.round((this.getPickerIconHeight() / 2) - (chooserHeight/2));

        // NOTE: don't return chooserWidth/Height as part of the rect, which would cause the
        // picker to actually be resized to those dimensions, and they may match the natural
        // size at which the chooser draws given skinning properties.
        return [left, top];
    },
    

    //> @method dateItem.pickerDataChanged()
    // Store the date passed in, and fire the change handler for this item.
    // Called when the user selects a date from the date-chooser window.  
    // @visibility internal
    //<
    pickerDataChanged : function (picker) {
    
        var pickerDate = picker.getData(),
            dateOnly = isc.Date.getLogicalDateOnly(pickerDate),
            year = dateOnly.getFullYear(),
            month = dateOnly.getMonth(),
            day = dateOnly.getDate(),
            timestamp = pickerDate.getTime()
        ;
            
        // The date-picker creates "logical dates" - dates where day/month/year are set
        // correctly in browser native local time and the time component is an arbitrary value.
        // If this field is being used for a "datetime" value we want to set the time portion
        // to zero in the display timezone so that it shows up as eg 06/16/2011 00:00
        // rather than some arbitrary time.
        var isLogicalDate = this.useLogicalDates();
        if (!isLogicalDate) {
            //this.setToZeroTime(date);
        }
            
        // avoid firing 'updateValue' while setting the values of sub items
        this._suppressUpdates = true;

        if (this.useTextField) {
            var formatted = this.formatDate(pickerDate);
            this.dateTextField.setValue(formatted);
        } else {
            var date = this._value || this.getDefaultValue(),
                hiddenSelector;
            if (this.yearSelector) this.yearSelector.setValue(year);
            else {
                date.setFullYear(year);
                hiddenSelector = true;
            }
            if (this.monthSelector) this.monthSelector.setValue(month);
            else {
                date.setMonth(month-1);
                hiddenSelector = true;
            }
            if (this.daySelector) this.daySelector.setValue(day);
            else {
                date.setDate(day);
                hiddenSelector = true;
            }
            
            if (isc.SimpleType.inheritsFrom(this.type, "datetime")) {
                var time = isc.Date.getLogicalTimeOnly(pickerDate);
                this._lastPickedTime = time;
            }

            // if this._value was unset before this method fired, set it now
            // This will be duplicated as part of update value and the selector values overlayed
            if (hiddenSelector) {
                this._value = date;
            }
        }
        this._suppressUpdates = false;

        // Explicitly call 'updateValue' to save the new date (handles firing change
        // handlers, etc. too)
        this.updateValue();

        // Ensure we have focus
        
        if (!this.hasFocus) this.focusInItem();

        // if we were already in a bad state, revalidate now
        var errors = this.getErrors();
        if (errors && errors.length > 0) this.validate();
        
        // if validateOnEditorExit is true we can get confused by the fact that we updated
        // the value and then shifted focus into the item and so assume there was no change and
        // not re-validate on exit.
        // Set the special flag to explicitly force a re-validation on editor exit
        
        if (this.validateOnExit || this.form.validateOnExit) {
            this._forceValidateOnExit = true;
        }
        
    },

    setHint : function (hintText) {
        if (this.useTextField && this._getShowHintInField()) {
            this.dateTextField.setHint(hintText);
        } else {
            this.Super("setHint", arguments); 
        }
    },

    // Override getPickerData() -- add support for providing a default picker date separate
    // from the default date for the item as a whole    
    getPickerData : function () {
        var date = this.getValue();
        if (date != null) {
            // wangle the value into a date if possible
            if (!isc.isA.Date(date)) {
                date = new Date(date);
            }
            // Note that a dateItem can return an arbitrary string - in this case new Date(...)
            // will give us a Date object but with no meaningful time etc data.
            if (isc.isA.Date(date) && !isNaN(date.getTime())) return date;
        }
        return this.getDefaultChooserDate();
    },
    
    //> @attr DateItem.defaultChooserDate (Date : null : IRW)
    // Default date to show in the date chooser. If this items value is currently unset,
    // this property may be specified to set a default date to highlight in the dateChooser 
    // for this item. If unset, the date chooser will highlight the current date by default.
    // Note that this has no effect if the item as a whole currently has a value - in that
    // case the date chooser will always highlight the current value for the item.
    // @visibility external
    //<
    //defaultChooserDate:null,
    
    //> @method DateItem.getDefaultChooserDate()
    // Returns the default date to display in the date chooser if this form items value is
    // currently unset.
    // <P>
    // Default implementation returns +link{dateItem.defaultChooserDate}
    // @return (Date) date to display, or null, indicating the current system date should be
    //   displayed.
    // @visibility external
    //<
    getDefaultChooserDate : function () {
        return this.defaultChooserDate;
    },
    
    _shouldAllowExpressions : function () {
        if (this.useTextField) {
            return this.Super("_shouldAllowExpressions", arguments);
        } else {
            return false;
        }
    },
    
    //> @attr DateItem.showChooserFiscalYearPicker (Boolean : false : IRW)
    // When set to true, show a button that allows the calendar to be navigated by fiscal year.
    // @visibility external
    //< 
    showChooserFiscalYearPicker: false,
    //> @attr DateItem.showChooserWeekPicker (Boolean : false : IRW)
    // When set to true, show a button that allows the calendar to be navigated by week or
    // fiscal week, depending on the value of +link{showChooserFiscalYearPicker}.
    // @visibility external
    //< 
    showChooserWeekPicker: false

    //>EditMode dynamically changing useTextField
    , 
    propertyChanged : function (propertyName) {
        if (propertyName == "useTextField" || propertyName == "useMask") {
            this.setItems();
        }
        this.Super("propertyChanged", arguments);
    }
    //<EditMode
});

}
