/*
 * Isomorphic SmartClient
 * Version SNAPSHOT_v10.1p_2015-12-10 (2015-12-10)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */
// Class will not work without the ListGrid
if (isc.ListGrid) {



    
//> @class RelativeDateItem
// A FormItem for entering a date relative to today or relative to some other date, or a specific
// date.  Typically used for filtering data by date.
// <P>
// The RelativeDateItem consists of a +link{ComboBoxItem} where the user may directly choose 
// one of several +link{relativeDateItem.presetOptions, preset options}, choose to enter a 
// +link{relativeDateItem.quantityField, quantity} and +link{type:TimeUnit, time unit} 
// (eg "4 months ago" or "3 years from now") or directly type in 
// an absolute date value (7/18/2009).
// @visibility external
//<
isc.defineClass("RelativeDateItem", "CanvasItem");

isc.RelativeDateItem.addClassMethods({

    //> @classMethod relativeDateItem.getAbsoluteDate() 
    // @include DateUtil.getAbsoluteDate()
    // @param relativeDate (RelativeDate) the relative date to convert
    // @param [baseDate] (Date) base value for conversion.  Defaults to today
    // @return (Date) resulting absolute date value
    // @visibility external
    //<
    getAbsoluteDate : function (relativeDate, baseDate, type, rangePosition) {

        var isLogicalDate = false;
        if (isc.SimpleType.inheritsFrom(type, "date")
            && !isc.SimpleType.inheritsFrom(type, "datetime")) 
        {
            isLogicalDate = true;
        }
        var absoluteDate = isc.DateUtil.getAbsoluteDate(relativeDate, baseDate, rangePosition,
                                                        isLogicalDate);
        return absoluteDate;
    },

    
    
    getPeriodName : function (periodKey) {
        return isc.DateUtil.getTimeUnitName(periodKey);
    },
    
    getPeriodKey : function (periodName) {
        return isc.DateUtil.getTimeUnitKey(periodName);
    },

    getRelativeDateParts : function (relativeDateString) {
        return isc.DateUtil.getRelativeDateParts(relativeDateString);
    },
    isRelativeDate : function (value) {
        return isc.DateUtil.isRelativeDate(value);
    }
});


isc.RelativeDateItem.addProperties({


//    titleOrientation: "top",
    height: 20,
    cellHeight: 20,
    canFocus: true,
    
    //> @attr relativeDateItem.rangePosition (RelativeDateRangePosition : null : IRWA)
    // Does this items relative date value refer to the start or end of the chosen date? Useful
    // when using this item to generate filter criteria, such as the from or to value for an
    // inclusive range.
    // <P>
    // If unset "start" is assumed.
    //
    // @see relativeDateItem.operator
    // @see relativeDateItem.rangeRoundingGranularity
    // @visibility external
    //<
    // Note that 'getAbsoluteDate()' [falls through to the method on the DateUtil class] actually
    // implements rounding the chosen value to the range position.
    // This has no effect if we're dealing with logical-dates (field is of
    // type "date" rather than "datetime"), since logical dates theoretically have no time component.
    // Of course they're implemented as JS Date objects so do have a time value but it should
    // always be set to the same static fixed value and we won't be modifying this.
    

    //> @type TimeUnit
    //   An enum of time-units available for use with the +link{RelativeDateItem},
    // +link{TimeItem} and +link{Calendar} widgets.
    // 
    // @value "millisecond"    a millisecond time-unit
    // @value "second"    a second time-unit
    // @value "minute"    a minute time-unit
    // @value "hour"    an hour time-unit
    // @value "day"    a day time-unit
    // @value "week"    a week time-unit
    // @value "month"    a month time-unit
    // @value "quarter"    a quarter (3 month) time-unit
    // @value "year"    a year time-unit
    // 
    // @visibility external
    //<
    
    //> @attr relativeDateItem.timeUnitOptions (Array of TimeUnit : ["day", "week", "month"] : IR)
    // List of time units that will be offered for relative dates.
    // <P>
    // Each available time unit option will cause two options to appear in the 
    // +link{valueField}.  For example, if "day" is an available +link{type:TimeUnit,time unit}
    // option, there will be +link{relativeDateItem.daysAgoTitle, "N days ago"} and 
    // +link{relativeDateItem.daysFromNowTitle, "N days from now"}.
    //
    // @see relativeDateItem.showPastOptions
    // @see relativeDateItem.showFutureOptions
    // @see relativeDateitem.rangeRoundingGranularity
    // 
    // @visibility external
    //<
    timeUnitOptions: ["day", "week", "month"],
    
    //> @attr relativeDateItem.showPastOptions (Boolean : true : IR)
    // Should we show time-unit options in the past? If set to false, for each 
    // +link{timeUnitOptions,timeUnitOption} we will show only future options [for example
    // "N weeks from now"].
    // <P>
    // Note: this does not change the +link{relativeDateItem.presetOptions}, which show up in 
    // addition to the time-unit options (<i>"N days from now"</i>, etc). The default
    // preset options include both past and future presets so developers may wish to modify the
    // presets to ensure only past options are available.    
    //
    // @visibility external
    //<
    showPastOptions:true,

    //> @attr relativeDateItem.showFutureOptions (Boolean : true : IR)
    // Should we show time-unit options in the future? If set to false, for each 
    // +link{timeUnitOptions,timeUnitOption} we will show only past options [for example
    // "N weeks ago"].
    // <P>
    // Note: this does not change the +link{relativeDateItem.presetOptions}, which show up in 
    // addition to the time-unit options (<i>"N days from now"</i>, etc). The default
    // preset options include both past and future presets so developers may wish to modify the
    // presets to ensure only future options are available.
    //
    // @visibility external
    //<
    showFutureOptions:true,
    
    //> @attr relativeDateItem.rangeRoundingGranularity (Object : {...} : IRWA)
    // A map from a granularity of time specified by a user to the granularity of time used for 
    // rounding.
    // <P>
    // A relative date such as "n days from now" is normally shifted to the end of the day when 
    // used as a range endpoint, and the beginning of the day when used as the beginning of a range.
    // (The rounding direction on some item can be specified via 
    // +link{relativeDateItem.rangePosition}).
    // This causes the intuitive behavior that "from yesterday to today" is from the beginning of
    // yesterday to the end of today, and that "from today until 5 days from now" includes the 
    // entirety of Friday if today is Monday.
    // <P>
    // This same rule <i>can</i> be applied to any time granularity, such that "from now until 
    // 20 minutes from now" is up to 5:32 if it is now 5:11:34, and 
    // "from now until 2 months from now" means end of June if it is mid-April.
    // <P>
    // User intuitions about where this rounding is expected for any given time period tend to
    // vary based on what kind of event is being discussed and subtle phrasing differences 
    // (consider "up to one year from now", "until next year", "within the next couple of years"). 
    // The defaults behaviors are:
    // <ul>
    // <li> for days, weeks and months round to <b>day</b> end/beginning
    // <li> for hours, round to <b>minute</b> end/beginning
    // <li> for minutes and seconds, round to <b>second</b> end/beginning
    // </ul>
    // To customize this rounding behavior, this attribute may be set to a simple javascript object
    // mapping each timeUnit to the granularity for that timeUnit.<br>
    // For example the following config code would produce an item where the user could select
    // only day or week values, and the selected value would be rounded to the beginning of the day
    // if a day was selected, or the beginning of the week if a week was selected:
    // <pre>
    //  {
    //      name:"fromDate", type:"RelativeDateItem",
    //      timeUnitOptions:["day", "week"],
    //      rangePosition:"start",
    //      rangeRoundingGranularity:{
    //          "day":"day",
    //          "week":"week"
    //      }
    //  }
    // </pre>
    //
    // @visibility external
    //<
    
    
    rangeRoundingGranularity:{
        "year":"day",
        "month":"day",
        "week":"day",
        "day":"day",
        "hour":"minute",
        "minute":"second",
        "second":"second"
    },
    
    // i18n attributes

    //> @attr relativeDateItem.millisecondsAgoTitle (string : "N milliseconds ago" : IR)
    // The title to show for historical periods when the +link{type:TimeUnit} is "millisecond".
    // @visibility external
    // @group i18nMessages
    //<
    millisecondsAgoTitle: "N milliseconds ago",

    //> @attr relativeDateItem.secondsAgoTitle (string : "N seconds ago" : IR)
    // The title to show for historical periods when the +link{type:TimeUnit} is "second".
    // @visibility external
    // @group i18nMessages
    //<
    secondsAgoTitle: "N seconds ago",

    //> @attr relativeDateItem.minutesAgoTitle (string : "N minutes ago" : IR)
    // The title to show for historical periods when the +link{type:TimeUnit} is "minute".
    // @visibility external
    // @group i18nMessages
    //<
    minutesAgoTitle: "N minutes ago",

    //> @attr relativeDateItem.hoursAgoTitle (string : "N hours ago" : IR)
    // The title to show for historical periods when the +link{type:TimeUnit} is "hour".
    // @visibility external
    // @group i18nMessages
    //<
    hoursAgoTitle: "N hours ago",

    //> @attr relativeDateItem.daysAgoTitle (string : "N days ago" : IR)
    // The title to show for historical periods when the +link{type:TimeUnit} is "day".
    // @visibility external
    // @group i18nMessages
    //<
    daysAgoTitle: "N days ago",

    //> @attr relativeDateItem.weeksAgoTitle (string : "N weeks ago" : IR)
    // The title to show for historical periods when the +link{type:TimeUnit} is "week".
    // @visibility external
    // @group i18nMessages
    //<
    weeksAgoTitle: "N weeks ago",

    //> @attr relativeDateItem.monthsAgoTitle (string : "N months ago" : IR)
    // The title to show for historical periods when the +link{type:TimeUnit} is "month".
    // @visibility external
    // @group i18nMessages
    //<
    monthsAgoTitle: "N months ago",

    //> @attr relativeDateItem.quartersAgoTitle (string : "N quarters ago" : IR)
    // The title to show for historical periods when the +link{type:TimeUnit} is "quarter".
    // @visibility external
    // @group i18nMessages
    //<
    quartersAgoTitle: "N quarters ago",

    //> @attr relativeDateItem.yearsAgoTitle (string : "N years ago" : IR)
    // The title to show for historical periods when the +link{type:TimeUnit} is "year".
    // @visibility external
    // @group i18nMessages
    //<
    yearsAgoTitle: "N years ago",

    //> @attr relativeDateItem.millisecondsFromNowTitle (string : "N milliseconds from now" : IR)
    // The title to show for future periods when the +link{type:TimeUnit} is "millisecond".
    // @visibility external
    // @group i18nMessages
    //<
    millisecondsFromNowTitle: "N milliseconds from now",

    //> @attr relativeDateItem.secondsFromNowTitle (string : "N seconds from now" : IR)
    // The title to show for future periods when the +link{type:TimeUnit} is "second".
    // @visibility external
    // @group i18nMessages
    //<
    secondsFromNowTitle: "N seconds from now",

    //> @attr relativeDateItem.minutesFromNowTitle (string : "N minutes from now" : IR)
    // The title to show for future periods when the +link{type:TimeUnit} is "minute".
    // @visibility external
    // @group i18nMessages
    //<
    minutesFromNowTitle: "N minutes from now",

    //> @attr relativeDateItem.hoursFromNowTitle (string : "N hours from now" : IR)
    // The title to show for future periods when the +link{type:TimeUnit} is "hour".
    // @visibility external
    // @group i18nMessages
    //<
    hoursFromNowTitle: "N hours from now",

    //> @attr relativeDateItem.daysFromNowTitle (string : "N days from now" : IR)
    // The title to show for future periods when the +link{type:TimeUnit} is "day".
    // @visibility external
    // @group i18nMessages
    //<
    daysFromNowTitle: "N days from now",

    //> @attr relativeDateItem.weeksFromNowTitle (string : "N weeks from now" : IR)
    // The title to show for future periods when the +link{type:TimeUnit} is "week".
    // @visibility external
    // @group i18nMessages
    //<
    weeksFromNowTitle: "N weeks from now",

    //> @attr relativeDateItem.monthsFromNowTitle (string : "N months from now" : IR)
    // The title to show for future periods when the +link{type:TimeUnit} is "month".
    // @visibility external
    // @group i18nMessages
    //<
    monthsFromNowTitle: "N months from now",

    //> @attr relativeDateItem.quartersFromNowTitle (string : "N quarters from now" : IR)
    // The title to show for future periods when the +link{type:TimeUnit} is "quarter".
    // @visibility external
    // @group i18nMessages
    //<
    quartersFromNowTitle: "N quarters from now",

    //> @attr relativeDateItem.yearsFromNowTitle (string : "N years from now" : IR)
    // The title to show for future periods when the +link{type:TimeUnit} is "year".
    // @visibility external
    // @group i18nMessages
    //<
    yearsFromNowTitle: "N years from now",

    //> @attr relativeDateItem.defaultValue (Date or RelativeDateString or TimeUnit : "$today" : IR)
    // Default value to show.  Can be a concrete Date, a +link{RelativeDateString} that matches 
    // one of the +link{relativeDateItem.presetOptions}, or one of the available 
    // +link{relativeDateItem.timeUnitOptions, time units}.  If setting a +link{type:TimeUnit},
    // use +link{relativeDateItem.defaultQuantity, defaultQuantity} to establish a default 
    // value for the +link{relativeDateItem.quantityField, quantityField}.
    // 
    // @visibility external
    //<
    defaultValue: "$today",

    //> @attr relativeDateItem.operator (OperatorId : "greaterThan" : IR)
    // What operator to use when <smartclient>+link{getCriterion()}</smartclient>
    // <smartgwt>the <code>FormItemCriterionGetter</code>'s <code>getCriterion()</code> method</smartgwt>
    // is called.
    // 
    // @visibility external
    //<
    operator: "greaterThan",

    //> @attr relativeDateItem.presetOptions (Object : see below : IR)
    // Preset relative dates, such as "today" or "tomorrow", that the user can choose directly
    // from the +link{valueField}.
    // <P>
    // Format is an Object mapping user-visible titles to +link{RelativeDateShortcut} or
    // +link{RelativeDateString}s. The default value (expressed in JSON) is:
    // <pre>
    // {
    //     "$today" : "Today",
    //     "$yesterday" : "Yesterday",
    //     "$tomorrow" : "Tomorrow",
    //     "$weekAgo" : "Current day of last week",
    //     "$weekFromNow" : "Current day of next week",
    //     "$monthAgo" : "Current day of last month",
    //     "$monthFromNow" : "Current day of next month"
    // }
    // </pre>
    // In addition to these presets, options are shown for each of the 
    // +link{type:TimeUnit, timeUnit options}.
    // 
    // @visibility external
    //<
    
    presetOptions: {
        "$today" : "Today",
        "$yesterday" : "Yesterday",
        "$tomorrow" : "Tomorrow",
        "$weekAgo" : "Current day of last week",
        "$weekFromNow" : "Current day of next week",
        "$monthAgo" : "Current day of last month",
        "$monthFromNow" : "Current day of next month"
    },

    // default implementation of formItem.shouldSaveOnEnter() returns this
    saveOnEnter: true,

    //> @attr relativeDateItem.valueFieldWidth (int | String : null : IRW)
    // The +link{formItem.width, width} for the +link{relativeDateItem.valueField, valueField}
    // in this item.  Defaults to the current default value for the width attribute on the 
    // +link{class:DateTimeItem} class - this is assumed to be just wide enough to show a full 
    // datetime string, in the current global datetime format.
    // <P>
    // Setting the width globally on the +link{class:DateTimeItem, DateTimeItem} class results
    // in all text-based datetime entry fields assuming the same default width - this caters for
    // custom date-time formatters that need differing amounts of space.
    // 
    // @visibility external
    //<
    //valueFieldWidth: null,
    
    // this is the old default width, which is appropriate for the presetOption titles - used
    // as a minimum value for valueFieldWidth, when taken from DateTimeItem as a default
    _minValueFieldWidth: 130,
    
    //> @attr relativeDateItem.valueField (AutoChild ComboBoxItem : null : IR)
    // +link{ComboBoxItem} field where a user may choose among 
    // +link{relativeDateItem.presetOptions, presets}, 
    // +link{type:TimeUnit, time unit} plus +link{relativeDateItem.quantityField,quantity}, or 
    // direct entry of a date as text.
    // 
    // @visibility external
    //<
    valueFieldDefaults: {
        editorType: "ComboBoxItem",
        showTitle: false,
        shouldSaveValue: false,
        validateOnChange: false,
        width: "*",
        // Override getInnerWidth() - the inner form has cellPadding set to zero to ensure the
        // left edge of this item lines up with the left edge of other select/combo items in the
        // form - however we'll also render *wider* since our inner width won't have cellPadding
        // to take into account. Deal with this by explicitly knocking off the cellPadding from
        // the form the RelativeDateItem is written into!
        getInnerWidth : function (adjustForIcons) {
            var iw = this.Super("getInnerWidth", arguments);
            var rdi = this.form.canvasItem;
            if (rdi == null || this._absPos() || !rdi._writtenIntoCell() || !isc.isA.Number(iw)) {
                return Math.max(0, iw);
            }
            return Math.max(0, iw - rdi._getCellHBorderPadSpacing());
        },
        keyPress : function (item, form, keyName, characterValue) {
            if (keyName == "Enter") {
                var parentItem = form.canvasItem;
                if (parentItem && parentItem.form)
                    parentItem.form.handleKeyPress(isc.EH.lastEvent);
            }
        },
        pickValue : function () {
            this.form.canvasItem.setBaseDate();
            this.Super("pickValue", arguments);
        }
    },

    //> @attr relativeDateItem.defaultQuantity (int : 1 : IR)
    // Default quantity to show in the +link{quantityField}.
    // 
    // @visibility external
    //<
    defaultQuantity: 1,

    //> @attr relativeDateItem.quantityField (AutoChild SpinnerItem : null : IR)
    // Field allowing user to pick units of time, eg, number of days.
    // 
    // @visibility external
    //<
    quantityFieldDefaults: {
        editorType: "SpinnerItem",
        width: 50,
        min: 0,
        step: 1,
        showTitle: false,
        shouldSaveValue: false,
        selectOnFocus: true,
        keyPress : function (item, form, keyName, characterValue) {
            if (keyName == "Enter") {
                var parentItem = form.canvasItem;
                if (parentItem && parentItem.form) {
                    parentItem.updateValue();
                    parentItem.form.handleKeyPress(isc.EH.lastEvent);
                }
            }
        }
    },

    //> @attr relativeDateItem.showChooserIcon (Boolean : true : IR)
    // Should we show the icon that displays a date-chooser?
    // @visibility external
    //<
    showChooserIcon:true,

    //> @attr relativeDateItem.pickerIcon (AutoChild FormItemIcon : null : IR)
    // Icon that launches a +link{DateChooser} for choosing an absolute date.
    // 
    // @visibility external
    //<
    pickerIconDefaults: {
        name: "chooserIcon",
        showOver: false,
        showFocused: false,
        showFocusedWithItem: false,
        neverDisable: true,
        width: 16, height: 16,
        src:"[SKIN]/DynamicForm/DatePicker_icon.gif"
    },
    
    iconVAlign: "center",

    //> @attr relativeDateItem.pickerIconPrompt (HTMLString : "Show Date Chooser" : IR)
    // Prompt to show when the user hovers the mouse over the picker icon for this 
    // RelativeDateItem. May be overridden for localization of your application.
    // @visibility external
    // @group i18nMessages
    //<
    pickerIconPrompt: "Show Date Chooser",

    //> @attr relativeDateItem.pickerConstructor (string : "DateChooser" : [IR])
    // SmartClient class for the +link{DateChooser, dateChooser} autoChild displayed to allow the user
    // to directly select dates.
    // @visibility external
    //<
    pickerConstructor: "DateChooser",

    //> @attr relativeDateItem.baseDate (Date : null : IR)
    // Base date for calculating the relative date entered by the user.
    // <P>
    // The default is to use the current date.
    // 
    // @visibility external
    //<

    //> @attr relativeDateItem.showCalculatedDateField (Boolean : true : IRW)
    //  Should the Calculated-Date be displayed to the right of the +link{pickerIcon}.
    // @visibility external
    //<
    showCalculatedDateField:true,

    //> @attr relativeDateItem.calculatedDateField (AutoChild BlurbItem : null : IR)
    // Field that shows the current calculated date by adding the user-entered relative date to
    // the +link{baseDate}.
    // 
    // @visibility external
    //<
    invalidCalculatedDatePrompt: "",
    calculatedDateFieldDefaults: {
        editorType: "BlurbItem",
        border:"1px solid black;",
        width: "*",
        
        setValue : function (value) {
            if (value == null || value == "") value = this.defaultValue;
            return this.Super("setValue", arguments);
        },
        formatValue : function (value) {
            if (!value || value == "") return "";
            return "(" + value + ")";
        },
        startRow: false,
        showTitle: false,
        shouldSaveValue: false
    },

    
    
    //> @method relativeDateItem.formatEditorValue() [A]
    // RelativeDateItems do not make use of the standard +link{formItem.formatEditorValue()} and
    // +link{formItem.parseEditorValue()} methods. Developers can customize the display values
    // for these items in the following ways:<ul>
    // <li>The +link{presetOptions} map allows standard preset RelativeDateString 
    //     and RelativeDateShortcut values to be mapped to custom display values</li>
    // <li>The text displayed for each of the +link{timeUnitOptions} (e.g:"N days ago") may be
    //     customized via the per-time unit title attributes (+link{daysFromNowTitle}, 
    //     +link{daysAgoTitle}, etc)</li>
    // <li>The +link{dateFormatter} and +link{inputFormat} may be used modify how date values
    //     are displayed (both in the text entry box and in the
    //    +link{showCalculatedDateField,calculatedDateField}</li>
    // </ul>
    // @visibility external
    //<
    
    //> @method relativeDateItem.parseEditorValue() [A]
    // RelativeDateItems do not make use of the standard +link{formItem.formatEditorValue()} and
    // +link{formItem.parseEditorValue()} methods. Developers can customize the display values
    // for these items in the following ways:<ul>
    // <li>The +link{presetOptions} map allows standard preset RelativeDateString 
    //     and RelativeDateShortcut values to be mapped to custom display values</li>
    // <li>The text displayed for each of the +link{timeUnitOptions} (e.g:"N days ago") may be
    //     customized via the per-time unit title attributes (+link{daysFromNowTitle}, 
    //     +link{daysAgoTitle}, etc)</li>
    // <li>The +link{dateFormatter} and +link{inputFormat} may be used modify how date values
    //     are displayed (both in the text entry box and in the
    //    +link{showCalculatedDateField,calculatedDateField}</li>
    // </ul>
    // @visibility external
    //<

    //> @attr relativeDateItem.inputFormat (DateInputFormat : null : IR)
    // Format for direct user input of date values.
    // <P>
    // If unset, the input format will be determined based on the specified
    // +link{displayFormat} if possible, otherwise picked up from the Date class (see
    // +link{Date.setInputFormat()}).
    // <smartclient>
    // <P>
    // Note: if entirely custom date formatting/parsing logic is required for this item, 
    // this attribute may be set to a function which takes a single parameter (the formatted
    // date string) and returns a JavaScript date object.
    // </smartclient>
    // @visibility external
    //<
    
    //> @attr relativeDateItem.displayFormat (DateDisplayFormat : null : IR)
    // Format for displaying dates in the +link{valueField} and +link{calculatedDateField}.  
    // Defaults to the system-wide default established by +link{Date.setShortDisplayFormat()}, or
    // if this item has its type specified as datetime, +link{Date.setShortDatetimeDisplayFormat()}.
    // @deprecated in favor of RelativeDateItem.dateFormatter
    // @visibility external
    //<

    //> @attr relativeDateItem.dateFormatter (DateDisplayFormat : null : IR)
    // Format for displaying dates in the +link{valueField} and +link{calculatedDateField}.
    // If unset a default DateDisplayFormat will be picked up from +link{dynamicForm.dateFormatter}
    // (or +link{dynamicForm.datetimeFormatter} for datetime fields} or otherwise from 
    // the system-wide default established by +link{Date.setShortDisplayFormat()}, or
    // if this item has its type specified as datetime, +link{Date.setShortDatetimeDisplayFormat()}.
    // <smartclient>
    // <P>
    // Note: if entirely custom date formatting/parsing logic is required for this item, this
    // attribute may be set to a custom formatting function. In this case the function will be
    // applied to the Date being formatted (for example <code>this.getFullYear()</code> would
    // give you back the full year), and should return the formatted date string.
    // </smartclient>
    // 
    // @visibility external
    //<

    //> @attr relativeDateItem.startDate (Date : 1/1/1995 : IRW)
    // @include dateItem.startDate
    // @group appearance
    // @visibility external
    //<
    //startDate:isc.DateItem.DEFAULT_START_DATE,    

    //> @attr relativeDateItem.endDate (Date : 12/31/2020 : IRW)
    // @include dateItem.endDate
    // @group appearance
    // @visibility external
    //<
    //endDate:isc.DateItem.DEFAULT_END_DATE,

    //> @attr relativeDateItem.centuryThreshold (number : 25 : IRW)
    // @include dateItem.centuryThreshold
    // @visibility external
    //<
    centuryThreshold:isc.DateItem.DEFAULT_CENTURY_THRESHOLD,

    //> @attr relativeDateItem.shouldSaveValue (Boolean : true : IR)
    // @include FormItem.shouldSaveValue
    //<
    shouldSaveValue: true,

    editCriteriaInInnerForm:false,

    //> @attr relativeDateItem.editor (AutoChild DynamicForm : null : [IRW])
    //
    // The editor that will be rendered inside this item.  Unless overridden, the editor will be
    // an instance of +link{class:DynamicForm}. It will be created using the overrideable 
    // defaults standard to the +link{group:autoChildren,AutoChild} subsystem - editorConstructor 
    // and editorProperties.
    //
    //  @visibility internal
    //<
    editorConstructor: "DynamicForm",
    editorDefaults: {
        numCols: 4,
        // Don't specify an explicit width - we'll expand/contract based on whether we're
        // showing the "n day" picker / whether we're showing the calculated date field, etc.
        // If we render the form at a fixed size, any icons for the item get mispositioned.
        cellPadding:0,
        colWidths: ["*", 50, 22, "*"],
        itemChanged : function (item, newValue) {
            this.creator.updateValue();
        },
        // Pass keyPress events through to this form item
        
        itemKeyPress : function (item,keyName,characterValue) {
        	// Fire item.keyPress and item.form.itemKeyPress on the 
        	// RelativeDateItem instance.
        	var item = this.canvasItem;
        	if (item && item.form) {
	        	return item._fireKeyPressHandlers(item,item.form,keyName,characterValue);
			}
        }
    },

    //> @attr relativeDateItem.useSharedPicker (Boolean : true : [IR])
    // When set to true (the default), use a single shared date-picker across all widgets that
    // use one.  When false, create a new picker using the autoChild system.  See 
    // +link{dateItem.pickerDefaults,picker} and 
    // +link{dateItem.pickerProperties,pickerProperties} for details on setting up an unshared
    // picker.
    // @visibility external
    //<
    useSharedPicker: false,

    //> @attr relativeDateItem.pickerDefaults (DateChooser : see below : [IR])
    // Defaults for the +link{DateChooser} created by this form item.
    //<
    pickerDefaults: {
        closeOnEscapeKeypress:true,
        // show a cancel button that closes the window
        showCancelButton: true,
        autoHide: true
    },

    //> @attr RelativeDateItem.showChooserFiscalYearPicker (Boolean : false : IRW)
    // When set to true, show a button that allows the calendar to be navigated by fiscal year.
    // @visibility external
    //< 
    showChooserFiscalYearPicker: false,
    //> @attr RelativeDateItem.showChooserWeekPicker (Boolean : false : IRW)
    // When set to true, show a button that allows the calendar to be navigated by week or
    // fiscal week, depending on the value of +link{showChooserFiscalYearPicker}.
    // @visibility external
    //< 
    showChooserWeekPicker: false

    //> @attr relativeDateItem.pickerProperties (DateChooser : see below : [IR])
    // Properties for the +link{DateChooser} created by this form item.
    //<

/*
    
    //> @attr relativeDateItem.pickerIconProperties (object : {...} : IRW)
    // Properties for the pickerIcon.
    // @visibility pickerIcon
    //<
    pickerIconProperties:{
    },
*/
    

});

isc.RelativeDateItem.addMethods({
    
    init : function () {
        if (!this.startDate) this.startDate = isc.DateItem.getPrototype().getStartDate();
        if (!this.endDate) this.endDate = isc.DateItem.getPrototype().getEndDate();
        this._createEditor();
        this.Super("init", arguments);
    },

    isEditable : function () {
        return true;
    },
    
    // if we get destroyed, wipe out our editor too
    autoDestroy:true,

    _createEditor: function(){
        var ds;
        var dynProps = { _suppressColWidthWarnings: true };
        this.addAutoChild("editor", dynProps);
        this.canvas = this.editor;        

        var _this = this,
            items = [],
            blurbIndex=2
        ;

        if (this.valueFieldWidth == null) {
            var width = isc.DateTimeItem.getInstanceProperty("width"),
                iconWidth = isc.ComboBoxItem.getInstanceProperty("pickerIconWidth")
            ;
            width = Math.max(this._minValueFieldWidth, width + iconWidth);
            this.valueFieldWidth = width;
        }

        items[0] = isc.addProperties({}, this.valueFieldDefaults, this.valueFieldProperties,
            { 
                valueMap: this.getValueFieldOptions()
            }, { name: "valueField" }
        );
        items[1] = isc.addProperties({}, this.quantityFieldDefaults, 
            this.quantityFieldProperties, 
            { 
                defaultValue: this.defaultQuantity
            }, { name: "quantityField" }
        );
        
        if (this.showChooserIcon) {
            blurbIndex = 3;
            items[2] = { name: "iconPlaceholder", type: "staticText", width: 1, 
                showTitle: false,
                canFocus:true, _canFocusInTextBox:function () { return false },
                // The synthetic tab-order management stuff used when the clickmask is
                // up needs to have focusInItem et al behave (never attempting to
                // focus on the text-box)
                focusInItem:function () {
                    this.focusInIcon(this.icons[0]);
                },
                _moveFocusWithinItem : function (forward) {
                    return false;
                },
                iconVAlign: "center",
                icons: [
                    isc.addProperties({ prompt: this.pickerIconPrompt }, 
                        this.pickerIconDefaults, this.pickerIconProperties,
                        {
                            click : function () {
                                if (!_this.isReadOnly()) {
                                    _this.showPicker();
                                }
                            } 
                        }
                    )
                ]
            };
        }

        // set a default baseDate is one wasn't provided
        var type = this.getType(),
            isLogicalDate = false;
        if (isc.SimpleType.inheritsFrom(type, "date") && 
            !isc.SimpleType.inheritsFrom(type, "datetime"))
        {
            isLogicalDate = this.isLogicalDate = true;
        }
        
        this.setBaseDate();

        if (this.showCalculatedDateField) {
            items[blurbIndex] = isc.addProperties({}, this.calculatedDateFieldDefaults, 
                this.calculatedDateFieldProperties,
                { cellStyle: this.getHintStyle(), defaultValue: this.invalidCalculatedDatePrompt },
                { name: "calculatedDateField" }
            );
        }

        this.canvas.setFields(items);

        this.valueField = this.canvas.getField("valueField");
        this.quantityField = this.canvas.getField("quantityField");
        if (this.showCalculatedDateField) 
            this.calculatedDateField = this.canvas.getField("calculatedDateField");
        if (this.showChooserIcon) {
            this.iconPlaceholder = this.canvas.getField("iconPlaceholder");
            this.pickerIcon = this.iconPlaceholder.icons.find("name", "chooserIcon");
        }

        this.setValue(this.value || this.defaultValue);
    },
    
    getBaseDate : function () {
        return this.baseDate;
    },

    setBaseDate : function (baseDate) {
        this.baseDate = baseDate || 
                    (this.isLogicalDate ? isc.Date.createLogicalDate() : new Date());
    },

    // updateEditor() Fired when the value changes (via updateValue or setValue)
    // Shows or hides the quantity box and updates the hint to reflect the current value.
    updateEditor : function () {
    
        if (!this.valueField || !this.quantityField) return;
        
        var focusItem,
            selectionRange,
            mustRefocus = false;
            
        if (this.valueField.hasFocus) {
            focusItem = this.valueField;
            selectionRange = this.valueField.getSelectionRange();
        } else if (this.quantityField.hasFocus) {
            focusItem = this.quantityField;
            selectionRange = this.quantityField.getSelectionRange();
        }
        
        var value = this.valueField.getValue(),
            quantity = this.quantityField.getValue();

        var showQuantity = (value && isc.isA.String(value) && this.relativePresets[value]);

        if (!showQuantity) {
            mustRefocus = true;
            this.editor.colWidths = [this.valueFieldWidth, 22, "*", "*"];
            this.quantityField.hide();
        } else {
            mustRefocus = true;
            this.editor.colWidths = [this.valueFieldWidth, 50, 22, "*"];
            this.quantityField.show();
        }

        if (this.calculatedDateField) {
            var value = this.getAbsoluteDate();
            var visibleDate = this.calculatedDateField.getValue();
            var newDate = !value ? "" : "" + this.formatDate(value) + "";
            if (visibleDate != newDate) {
                
                mustRefocus = true;
                this.calculatedDateField.setValue(newDate);
            }
        }
        // If we redrew the form to show or hide the qty field, we may need to refocus and
        // reset the selection range
        
        if (mustRefocus && focusItem != null) {
            
            if (!showQuantity && focusItem == this.quantityField) {
                this.valueField.focusInItem();
            }
        }
    },
 
    _valueFieldOptions: null,
    getValueFieldOptions : function (useStored) {
        if (useStored && this._valueFieldOptions) return this._valueFieldOptions;

        var options = isc.addProperties({}, this.presetOptions);

        this.relativePresets = {};

        // add two entries for each available time-unit, one historical, the other futuristic
        for (var i=0; i< this.timeUnitOptions.length; i++) {
            var key = this.timeUnitOptions[i];
            if (this.showPastOptions) {
                options[key+"_ago"] = this[key+"sAgoTitle"];
                this.relativePresets[key+"_ago"] = true;
            }
            if (this.showFutureOptions)  {
                options[key+"_fromNow"] = this[key+"sFromNowTitle"];
                this.relativePresets[key+"_fromNow"] = true;
            }
        }

        this._valueFieldOptions = options;
        return options;
    },

    setValue : function (value, allowNullValue) {
    
        
        if (!this.valueField) return this.Super("setValue", arguments);
    
        // If we have a defaultValue, pick it up now so we can convert from rel to abs date etc. 
        // Check for allowNullvalue matches the superclass implementation so we get the same
        // default as we would calling 'super'
        if (value == null && !allowNullValue) {
            var defaultVal = this.getDefaultValue();
            if (defaultVal != null) {
                value = defaultVal;
            }
        }
        
        // Overridden to:
        // - update the valueField / quantity field to reflect the value passed in
        // - convert to an absolute date (if a relative date was passed in) and store that out so
        //   getValue() always returns an absolute date.

        // Note - if passed a relative date we *also* store out the relative date value, and
        // hang it on the derived absolute date as an attribute.
        // This allows us to display the relative date and return it from getRelativeDate()
        // calls even if a setValue(getValue()) cycle occurs [can happen on initial draw with
        // setItemValues for example].
        
        // If this is a date that was derived from our current relativeDate value, we're basically
        // resetting to our current value -- retain the relative date information.
        if (isc.isA.Date(value) && value._fromRelativeDate != null &&
            this.compareValues(this._relativeDate,value._fromRelativeDate)) 
        {
            // We hung onto a timestamp when we converted from relative-date - if it's
            // changed this implies the date has subsequently been modified, so don't
            // make use of this relative value.
            if (value.getTime() == value._relativeDateTimestamp) {
                value = value._fromRelativeDate;
            }
        }

        var isRelativeDate = false;

        // reset the baseDate
        this.setBaseDate();

        if (value == null) {
            this.valueField.setValue(null);
            
        } else if (isc.isA.Date(value) || this.valueField.valueMap[value] || 
                (value.value && this.valueField.valueMap[value.value])) 
        {
            var absDate = isc.isA.Date(value);
            isRelativeDate = !absDate;
            // the defaultValue is a preset or a date, just set the value
            this.valueField.setValue(absDate ? this.formatDate(value) : 
                    value.value ? value.value : value);
        
        
        } else if (this.timeUnitOptions.contains(value)) {
            isRelativeDate = true;
            // the defaultValue is a timeUnit - select the future version of it
            value += "_fromNow";
            this.valueField.setValue(value);
        } else if (isc.isA.String(value) && isc.isA.Date(this.parseDate(value, this.getInputFormat()))) {
            // convert the text input to a date, since it is one
            value = this.parseDate(value, this.getInputFormat());
            this.valueField.setValue(this.formatDate(value));
        } else {
            // a defaultValue was provided, but it's none of preset, timeUnit or date
            var absoluteDate,
                isRelativeDate = isc.RelativeDateItem.isRelativeDate(value);
            
            // Handle being passed a relative date object or a shortcut string
            if (isRelativeDate || isc.isA.String(value)) {
                absoluteDate = isc.RelativeDateItem.getAbsoluteDate(
                        value, this.getBaseDate(), this.getType(), this.rangePosition);
            }
            // Unable to convert to a date - we were passed something other than a valid 
            // RelativeDate - just ignore it.
            if (!isc.isA.Date(absoluteDate)) {
                isRelativeDate = false;
                this.valueField.setValue(null);
//                this.valueField.setValue("$today");
            } else {
                
                var relativeDateString = isRelativeDate ? value.value : value;
                
                // at this point we know we were passed either a relative date shortcut
                // ($now etc) which isn't present in our presetOptions (we checked for being present
                // in the valueMap earlier in this method), or we were passed a true relative date
                // string like +1w
                // If possible, set both value and quantity to preserve the relative date -- if 
                // we're not showing options that allow us to do this, just store out the
                // absolute date.
                relativeDateString = isc.DateUtil.mapRelativeDateShortcut(relativeDateString,
                                        this.rangePosition);
                var key, quantity,
                    parts = isc.RelativeDateItem.getRelativeDateParts(relativeDateString),
                    period = isc.RelativeDateItem.getPeriodName(parts.period),
                    suffix = (parts.direction == "+" ? "fromNow" : "ago")
                ;
                
                quantity = parts ? parts.countValue : null;
                key = period ? period.toLowerCase()+"_"+suffix : null;
            

                if (key && this.valueField.valueMap[key]) {
                    this.valueField.setValue(key);
                    this.quantityField.setValue(quantity);
                    isRelativeDate = true;
                } else {
                    // the period to which this relativeDate applies is not in the list
                    // just resolve to an absolute date and store that value
                    isRelativeDate = false;
                    this.valueField.setValue(this.formatDate(absoluteDate));
                }
            }
        }
        
        if (isRelativeDate) {
            // we know we were passed a relative date. getDataValue() will normalize to
            // the standard RelativeDateObject format.
            // convertToAbsoluteDate() will convert to an actual date, and store the '_fromRelativeDate'
            // marker on that absolute date.
            this._relativeDate = this.getDataValue();
            value = this._convertToAbsoluteDate(this._relativeDate);
        } else {
            this._relativeDate = null;
        }
        
        this.Super("setValue", [value, allowNullValue], arguments);
        
        this.updateEditor();
    },
    
    _convertToAbsoluteDate : function (relativeDate) {
        var relativeDateValue = relativeDate.value ? relativeDate.value : relativeDate;
        var absDate = isc.RelativeDateItem.getAbsoluteDate(
                        relativeDateValue, this.getBaseDate(), this.getType(), this.rangePosition);

        if (isc.isA.Date(absDate)) {        
            // Hang flags on the converted date so we can tell what relative date it was
            // derived from.
            // We use this to detect the case where setValue(getValue()) is called on an item
            // showing a relative date value so we can hang onto the relative date information.
            absDate._fromRelativeDate = relativeDate;
            absDate._relativeDateTimestamp = absDate.getTime();
        }
        
        return absDate;
    },

    //> @method relativeDateItem.getAbsoluteDate()
    // Returns the current value as a Date. If the user entered a relative date value, this
    // will be normalized to a Date and returned.
    //
    // @return (Date) value
    // @visibility internal
    //<
    // Currently getValue() always normalizes to a real date.
    getAbsoluteDate : function () {
        return this.getDataValue(true);
    },

    //> @method relativeDateItem.getRelativeDate()
    // Returns the current +link{RelativeDate} object for this item. Only applies if the user entered
    // a relative date value (such as "Today") - if an absolute date was entered, this method
    // returns null.<br>
    // Relative date objects have the following format:
    // <pre>
    //     { _constructor: "RelativeDate", value: "$today" }
    // </pre>
    //
    // @return (object) an object containing the relativeDate string for the current value
    // @visibility external
    //<
    getRelativeDate : function () {
        var value = this.valueField.getValue(),
            quantity = this.quantityField.getValue()
        ;
        if (!value || !isc.isA.String(value)) return null;

        var firstChar = value.substring(0,1);

        if (firstChar == "+" || firstChar == "-" || 
                (firstChar == "$" && this.getValueFieldOptions(true)[value]))
        {
            // this is a relativeDate anyway, just return it
            return this.getRelativeDateObject(value);
        }

        // check for one of the other built-in types (in the format [period]_ago, [period]_fromNow
        // This is what gets created when the user picks a quantity and a value from the timePeriod
        // drop downs.
        var underscoreIndex = value.indexOf("_");

        if (underscoreIndex >= 0) {
            var periodName = value.substring(0, underscoreIndex),
                negative = (value.substring(underscoreIndex+1) == "ago"),
                key = isc.RelativeDateItem.getPeriodKey(periodName)
            ;
            if (key) {
                // Return a relative date string representing the period, rounded by the appropriate
                // offset - example for 3 weeks from now when rounding to the beginning of the day,
                // return "+3w[-0D]"
                var string = (negative ? "-" : "+") + quantity + key;

                var offsetDirection = this.rangePosition == "end" ? "+" : "-",
                    offset = this.rangeRoundingGranularity[periodName];
                if (offset != null && offset.toLowerCase() != "millisecond") {
                    offset = isc.RelativeDateItem.getPeriodKey(offset).toUpperCase();
                    string += "[" + offsetDirection + "0" + offset + "]";
                }
                return this.getRelativeDateObject(string);
            }
        }

        return null;
    },

    getRelativeDateObject : function (relativeDate) {
        var result = { _constructor: "RelativeDate", value: relativeDate };
        if (this.rangePosition) result.rangePosition = this.rangePosition;
        return result;
    },

    // Helper method to return the current value as an absolute or relative date object.
    // Returns null if the entered value can't be parsed into either absolute or relative date
    getDataValue : function (returnAbsoluteDate) {
        var enteredVal = this.valueField.getValue(),
            dateValue;
            
        if (enteredVal == null || isc.isAn.emptyString(enteredVal)) {
            dateValue = null;
        } else {
            var relativeDate = this.getRelativeDate();

            if (relativeDate) {
                if (returnAbsoluteDate) {
                    if (!relativeDate.value.startsWith("$") || 
                            this.getValueFieldOptions(true)[relativeDate.value]) {
                        dateValue = this._convertToAbsoluteDate(relativeDate);
                    }
                } else {
                    dateValue = relativeDate;
                }
            } else {
                dateValue = this.parseDate(enteredVal, this.getInputFormat());
            }
            if (isc.isA.Date(dateValue)) {
                var type = this.getType();
                if (type == null || 
                    (!isc.SimpleType.inheritsFrom(type, "datetime") && 
                     !isc.SimpleType.inheritsFrom(type, "time")))
                {
                    dateValue.logicalDate = true;
                }
                if (this.rangePosition) {
                    dateValue.rangePosition = this.rangePosition;
                }
            }
        }
        return dateValue;
    },

    // This formItem returns a Date from getValue(), so we want to convert to an absolute date
    // for storage.
    // If a relative date was entered we also hang onto that (on both the item and the date object)
    // so we can maintain it across item.setValue(item.getValue()) cycles (EG form validation/redraw)
    updateValue : function() {
        if (!this.valueField || !this.quantityField) return;

        var oldValue = this._value,
            oldRelativeDate = this._relativeDate,
            dataValue = this.getDataValue(false)
        ;

        if (!oldValue && dataValue) this.setBaseDate();

        var absDateValue = (dataValue == null || isc.isA.Date(dataValue)) 
                            ? dataValue 
                            : this._convertToAbsoluteDate(dataValue);

        if (dataValue != absDateValue) {
            this._relativeDate = dataValue;
        } else {
            this._relativeDate = null;
        }

        // Note: We compare both the absolute date and the relative date - this means
        // we *will* fire change/changed handler if the user changes from "Yesterday" to
        // the absolute date for yesterday, or "1 day ago". Seems desirable since 
        // getRelativeDate() will return different results in the second case.
        
        if (this.compareValues(oldValue,absDateValue) && 
            this.compareValues(oldRelativeDate,this._relativeDate)) return;

        this._updateValue(absDateValue);
        this.updateEditor();
    },

    // We always return advanced criteria if we have a value.
    hasAdvancedCriteria : function () {
        return (this.valueField && this.valueField.getValue() != null);
    },

    //> @method relativeDateItem.getCriterion()
    // Get the criterion based on the values the user has entered.
    // @param [absolute] (boolean) whether to use an absolute date in the Criterion produced.  
    //                             By default a +link{RelativeDate} will be used if the user 
    //                             entered a relative date value
    // @return (Criterion)
    // @visibility external
    //<
    getCriterion : function (absolute) {
        var date = this.getDataValue(absolute);

        if (date == null) return null;
        var field = this.getCriteriaFieldName();
        return { operator: this.operator, value: date, fieldName:field };
    },

    getCellHeight : function () {
        var cellHeight = this.Super("getCellHeight", arguments);
        if (isc.Browser.isIE && this.useTextField && isc.isA.Number(cellHeight)) cellHeight += 2;
        return cellHeight;
    },

    //> @method RelativeDateItem.getFiscalCalendar()
    // Returns the +link{FiscalCalendar} object that will be used by this item's DateChooser.
    //
    // @return (FiscalCalendar) the fiscal calendar for this chooser, if set, or the global
    //            one otherwise
    // @visibility external
    //<
    getFiscalCalendar : function () {
        return this.fiscalCalendar || Date.getFiscalCalendar();
    },

    //> @method RelativeDateItem.setFiscalCalendar()
    // Sets the +link{FiscalCalendar} object that will be used by this item's DateChooser.  If 
    // unset, the _link{Date.getFiscalCalendar, global fiscal calendar} is used.
    //
    // @param [fiscalCalendar] (FiscalCalendar) the fiscal calendar for this chooser, if set, or the global
    //            one otherwise
    // @visibility external
    //<
    setFiscalCalendar : function (fiscalCalendar) {
        this.fiscalCalendar = fiscalCalendar;
    },
    
    //> @attr RelativeDateItem.showPickerTimeItem (Boolean : true : IRWA)
    // If this item is editing a field of type <code>"datetime"</code>, should the
    // +link{class:DateChooser} display the +link{dateChooser.showTimeItem,time field}, 
    // allowing the user to select a time?
    // <P>
    // One case where developers will wish to suppress this time-field from being displayed
    // is if a custom +link{dateFormatter} has been specified which does not display the
    // time portion of the selected date. In this case any value selected from the 
    // DateChooser's time field will be discarded when the picker is dismissed, making
    // it a confusing UI for the end user.
    // <P>
    // Has no effect if the field type is <code>"date"</code> - in this case the
    // picker will never show the time field.
    //
    // @visibility external
    //<
    showPickerTimeItem:true,
    
    //> @attr RelativeDateItem.pickerTimeItemProperties (TimeItem Properties : null : IRWA)
    // A set of properties to apply to the +link{class:TimeItem} displayed in the picker when
    // +link{showPickerTimeItem} is true.
    // <P>
    // Has no effect for fields of type <code>"date"</code>.
    // @visibility external
    //<

    //> @attr RelativeDateItem.use24HourTime (Boolean : true : IRW)
    // When showing the +link{class:DateChooser}, should the 
    // +link{dateChooser.showTimeItem,time field} be set to use 24-hour time?
    // Has no effect for fields of type <code>"date"</code> rather than 
    // <code>"datetime"</code>, or if +link{showPickerTimeItem} is <code>false</code>.
    // <P>
    // Default is true.
    // @visibility external
    //< 
    use24HourTime: true,

    // override 'showPicker' - instead of creating a picker instance we're reusing a shared
    // one.
    showPicker : function () {

        if (!this.picker) {
            if (this.useSharedPicker) this.picker = isc.DateChooser.getSharedDateChooser();
            else {
                this.picker = isc[this.pickerConstructor].create(
                    isc.addProperties({}, this.pickerDefaults, this.pickerProperties, 
                        {
                            _generated:true,
                            // When re-using a DateChooser, we're almost certainly displaying it as a 
                            // floating picker rather than an inline element. Apply the common options for 
                            // a floating picker
                            autoHide:true,
                            showCancelButton:true
                        }
                    )
                );
            }
        }

        var picker = this.picker;

        var oldItem = picker.callingFormItem;
        if (oldItem != this) {
            if (oldItem) oldItem.ignore(picker, "dataChanged");
            this.observe(picker, "dataChanged", "observer.pickerDataChanged(observed)");
            
            picker.callingFormItem = this;
            picker.callingForm = this.canvas; // this.form;
            
            picker.locatorParent = this.canvas; //this.form;
        }

        if (this.inputFormat) picker.inputFormat = this.inputFormat;

        picker.startYear = this.getStartDate().getFullYear();
        picker.endYear = this.getEndDate().getFullYear();

        // set the year and week attributes
        picker.fiscalCalendar = this.getFiscalCalendar();
        picker.showFiscalYearChooser = this.showChooserFiscalYearPicker;
        picker.showWeekChooser = this.showChooserWeekPicker;
        // show a TimeItem in the picker if type is datetime
        var type = this.type, isLogicalDate = false;
        if (isc.SimpleType.inheritsFrom(type, "date")
            && !isc.SimpleType.inheritsFrom(type, "datetime")) 
        {
            isLogicalDate = true;
        }
        var showTimeItem = isLogicalDate ? false : this.showPickerTimeItem;
        picker.showTimeItem = showTimeItem
        picker.use24HourTime = this.use24HourTime;
        if (this.pickerTimeItemProperties) 
            picker.timeItemProperties = isc.addProperties({}, picker.timeItemProperties, 
                this.pickerTimeItemProperties);

        var absoluteDate = this.getAbsoluteDate();
        if (picker.setData) {
            if (isc.isA.Date(absoluteDate) && !isNaN(absoluteDate.getTime())) {
                // this item has a value, pass it to the DateChooser
                picker.setData(absoluteDate);
            } else {
                var chosenDate = new Date();
                // this item has no value - if it has a rangePosition, set the time 
                // appropriately on the default chosenDate
                if (this.rangePosition == "start") {
                    chosenDate = isc.DateUtil.getStartOf(chosenDate, "D");
                } else if (this.rangePosition == "end") {
                    chosenDate = isc.DateUtil.getEndOf(chosenDate, "D");
                }
                picker.setData(chosenDate);
                // prevent a second call to setData(), from the Super() call, that passes 
                // item.getValue(), which returns null at that point
                picker._ignorePickerSetData = true;
            }
        }
        if (picker.updateUI) picker.updateUI();

        return this.Super("showPicker", arguments);
        
    },

    // custom code to center the picker over the picker icon
    getPickerRect : function () {
        // we want the date chooser to float centered over the picker icon.
        var left = this.getPageLeft(),
            top = this.getPageTop(),
            
            chooserWidth = isc.DateItem.getChooserWidth() + 3,
            chooserHeight = isc.DateItem.getChooserHeight() + 3,
            form = this.canvas,
            item
        ;

        item = form.getItem("iconPlaceholder");

        left += item.getLeft();
        left += Math.round((item.getVisibleWidth() - (this.getPickerIconWidth() /2)) -
                (chooserWidth/2));
        top += Math.round((this.getPickerIconHeight() / 2) - (chooserHeight/2));

        // NOTE: don't return chooserWidth/Height as part of the rect, which would cause the
        // picker to actually be resized to those dimensions, and they may match the natural
        // size at which the chooser draws given skinning properties.
        return [left, top];
    },
    
    updateDisabled : function () {
        this.Super("updateDisabled", arguments);
        if (this.iconPlaceholder && this.iconPlaceholder.isVisible()) {
            this.iconPlaceholder.redraw();
        }
    },

    //> @method relativeDateItem.pickerDataChanged()
    //      Store the date passed in, and fire the change handler for this item.
    //      Called when the user selects a date from the date-chooser window.  
    //  @visibility internal
    //<
    pickerDataChanged : function (picker) {

        var date = picker.getData();
        var type = this.getType(),
            isDate = isc.SimpleType.inheritsFrom(type, "date"),
            isDatetime = isc.SimpleType.inheritsFrom(type, "datetime");
        if (!this.showPickerTimeItem && (!isDate || isDatetime)) {
            this.setToZeroTime(date);
            // Respect rangePosition on the date picked (note that zero time is the default)
            if (this.rangePosition == "end") date = isc.DateUtil.getEndOf(date, "D");
            
        }

        // avoid firing 'updateValue' while setting the values of sub items
        this._suppressUpdates = true;
        
        this.valueField.setValue(this.formatDate(date));
        
        this._suppressUpdates = false;

        // Explicitly call 'updateValue' to save the new date (handles firing change
        // handlers, etc. too)
        this.updateValue();

        // Ensure we have focus
        
        if (!this.hasFocus) this.focusInItem();
    },
    
    //> @method relativeDateItem.getStartDate() (A)
    //    use this method, rather than referring to this.startDate, to guarantee that it
    //    returns a date
    //      Note - Does not update this.startDate - should it?
    //<
    getStartDate : function () {
        var startDate = this.parseDate(this.startDate);
        if(!isc.isA.Date(startDate)) {
            //>DEBUG
            this.logWarn("startDate was not in valid date format - using default start date");
            //<DEBUG
            startDate = isc.DateItem.getPrototype().getStartDate();
        }
        return startDate;
    },
    
    //> @method relativeDateItem.getEndDate() (A)
    //    use this method, rather than referring to this.endDate, to guarantee that it
    //      returns a date
    //<
    getEndDate : function () {
        var endDate = this.parseDate(this.endDate);
        if(!isc.isA.Date(endDate)) {
            //>DEBUG
            this.logWarn("endDate was not in valid date format - using default end date");
            //<DEBUG
            endDate = isc.DateItem.getPrototype().getEndDate();
        }
        return endDate;
    },

    //> @method relativeDateItem.parseDate()
    // Parse a date passed in as a string.
    //
    // @param dateString (string) date value as a string
    // @param inputFormat (DateInputFormat) format for date strings to be parsed
    // @return (date) date value
    // @group elements
    //<
    parseDate : function (dateString, inputFormat) {
        if (dateString == null || isc.isAn.emptyString(dateString)) return null;
        if (inputFormat == null) inputFormat = this.getInputFormat();
        var dataType = this.getType(),
            isDate = isc.SimpleType.inheritsFrom(dataType, "date"),
            isDatetime = isc.SimpleType.inheritsFrom(dataType, "datetime"),
            isLogicalDate =  isDate && !isDatetime;
        
        var date = Date.parseInput(dateString, inputFormat, 
                                this.centuryThreshold, true, !isLogicalDate);
        // If it's a datetime, we may not actually be showing a time portion in the string.
        // In this case we'll want to clamp to the start or end of day!
        if (isc.isA.Date(date) && !isLogicalDate) {
            var enteredVal = this.getEnteredValue();

            if (enteredVal != null && !isc.isA.Function(inputFormat)) {
                var validTime = isc.Date.isDatetimeString(enteredVal, inputFormat);
            
                //var validTime = isc.Time.parseInput(enteredVal, true);

                if (!validTime) {
                    this.setToZeroTime(date);
                    // Respect rangePosition on the date picked (note that zero time is the default)
                    if (this.rangePosition == "end") date = isc.DateUtil.getEndOf(date, "D");                
                }
            }
        }
        return date;
    },


    // formatDate() - given a live date object, returns the formatted date string to display
    formatDate : function (date) {
        if (!isc.isA.Date(date)) return date;
        
        var displayFormat = this._getDateFormatter();

        var type = this.getType(),
            isDatetime = false,
            isDate = isc.SimpleType.inheritsFrom(type, "date");
        if (!isDate || isc.SimpleType.inheritsFrom(type, "datetime")) isDatetime = true;

        
        if (isDatetime) {
            return date.toShortDatetime(displayFormat, true);
        } else {
            return date.toShortDate(displayFormat, !isDate);
        }
    },

    //> @method relativeDateItem.getInputFormat() (A)
    // @include dateItem.getInputFormat
    // @visibility external
    //<
    getInputFormat : function () {
        // developer may explicitly specify an inputFormat (this used to be the only way to change
        // input/display format for text-based date items)
        if (this.inputFormat) return this.inputFormat;
        // If a display format, but no inputFormat is specified attempt to derive the inputFormat
        // from the displayFormat. This works for the standard shortDate display formatters but
        // you'll still need to specify an explicit input format for anything more exotic
        var displayFormat = this._getDateFormatter();
        if (displayFormat) { 
            return Date.mapDisplayFormatToInputFormat(displayFormat);
        }
        // couldn't get an input format - rely on the standard global Date inputFormat
        return null;
    },
    
    
    //> @method relativeDateItem.getEnteredValue()
    // Returns the raw text value typed into this items value text field
    // @visibility external
    //<
    getEnteredValue : function () {
        if (this.valueField) return this.valueField.getValue();
        return this.getValue();
    },
    
    getValue : function () {
        var value = this.Super("getValue", arguments);
        return value || (this.valueField && this.valueField.getValue());
    },
    
    validators: [
        { type: "isDate" }
    ]

});

}
