/*
 * Isomorphic SmartClient
 * Version v10.0p_2014-09-10 (2014-09-10)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */

 

// This file creates a mini-calendar that is used to pick a date, for example, you might have a
// button next to a form date field that brings this file up.




//>	@class	DateChooser
//
// Simple interactive calendar interface used to pick a date.
// Used by the +link{class:dateItem} class.
//
// @treeLocation Client Reference/Forms
// @visibility external
//<

// create a special canvas to show the days in a month
isc.ClassFactory.defineClass("DateChooser", "VLayout");

isc.DateChooser.addProperties({
    width: 10,
    height: 10,
    overflow: "visible",

    // Header
    // ---------------------------------------------------------------------------------------

    //> @attr dateChooser.navigationLayout (AutoChild HLayout : null : IR)
    // An +link{AutoChild} +link{HLayout}, rendered above the +link{class:DateGrid, date grid},
    // and showing a number of widgets for navigating the DateChooser.  These include buttons 
    // for moving to the previous +link{dateChooser.previousYearButton, year} or 
    // +link{dateChooser.previousMonthButton, month}, the next 
    // +link{dateChooser.nextYearButton, year} or +link{dateChooser.nextMonthButton, month}, 
    // and for selecting a specific +link{dateChooser.yearChooserButton, year}, 
    // +link{dateChooser.monthChooserButton, month} or 
    // +link{dateChooser.weekChooserButton, week}.
    // @visibility external
    //<
    showNavigationLayout:true,
    navigationLayoutConstructor: "HLayout",
    navigationLayoutDefaults: {
        width: 1,
        height: 1,
        layoutAlign: "center",
        align: "center"
    },
    
    //> @attr DateChooser.showFiscalYearChooser (Boolean : false : IRW)
    // When set to true, show a button that allows the calendar to be navigated by fiscal year.
    // @visibility external
    //< 
    showFiscalYearChooser: false,

    //> @attr dateChooser.fiscalYearChooserButton (AutoChild IButton : null : IR)
    // A button shown in the +link{dateChooser.navigationLayout, navigation layout} which, 
    // when clicked, shows a picker for selecting a specific fiscal year.
    // @visibility external
    //<
    fiscalYearChooserButtonDefaults: {
        width: 30,
        click : function () {
            this.creator.showFiscalYearMenu();
        },
        autoParent: "navigationLayout",
        align: "center"
    },

    //> @attr DateChooser.showWeekChooser (Boolean : false : IRW)
    // When set to true, show a button that allows the calendar to be navigated by week or
    // fiscal week, depending on the value of +link{showFiscalYearChooser}.
    // 
    // @visibility external
    //< 
    showWeekChooser: false,

    //> @attr dateChooser.weekChooserButton (AutoChild IButton : null : IR)
    // A button shown in the +link{dateChooser.navigationLayout, navigation layout} which shows
    // a picker for selecting a specific week of the year.  When +link{showFiscalYearChooser}
    // is true, the week number represents a fiscal week number, one offset from the start of 
    // the fiscal year.  Otherwise, it represents a week number offset from the start of the
    // calendar year.
    // 
    // @visibility external
    //<
    weekChooserButtonDefaults: {
        width: 25,
        click : function () {
            this.creator.showWeekMenu();
        },
        autoParent: "navigationLayout",
        align: "center"
    },

    //> @attr dateChooser.previousYearButton (AutoChild IButton : null : IR)
    // A button shown in the +link{dateChooser.navigationLayout, navigation layout} that shifts
    // the calendar view backward by a year.
    // 
    // @visibility external
    //<
    previousYearButtonDefaults: {
        width: 20,
        click : function () {
            this.creator.showPrevYear();
        },
        autoParent: "navigationLayout",
        align: "center",
        noDoubleClicks: true
    },

    //> @attr dateChooser.previousMonthButton (AutoChild IButton : null : IR)
    // A button shown in the +link{dateChooser.navigationLayout, navigation layout} that shifts
    // the calendar view backward by a month.
    // 
    // @visibility external
    //<
    previousMonthButtonDefaults: {
        width: 20,
        click : function () {
            this.creator.showPrevMonth();
        },
        autoParent: "navigationLayout",
        align: "center",
        noDoubleClicks: true
    },

    //> @attr dateChooser.monthChooserButton (AutoChild IButton : null : IR)
    // A button shown in the +link{dateChooser.navigationLayout, navigation layout} that shows 
    // a picker for selecting a specific month.
    // 
    // @visibility external
    //<
    monthChooserButtonDefaults: {
        minWidth: 30,
        autoFit: true,
        click : function () {
            this.creator.showMonthMenu();
        },
        autoParent: "navigationLayout",
        align: "center"
    },

    //> @attr dateChooser.yearChooserButton (AutoChild IButton : null : IR)
    // A button shown in the +link{dateChooser.navigationLayout, navigation layout} that shows 
    // a picker for selecting a specific calendar year.
    // 
    // @visibility external
    //<
    yearChooserButtonDefaults: {
        width: 32,
        click : function () {
            this.creator.showYearMenu();
        },
        autoParent: "navigationLayout",
        align: "center"
    },
    
    //> @attr dateChooser.nextMonthButton (AutoChild IButton : null : IR)
    // A button shown in the +link{dateChooser.navigationLayout, navigation layout} that shifts
    // the calendar view forward by a month.
    // 
    // @visibility external
    //<
    nextMonthButtonDefaults: {
        width: 20,
        click : function () {
            this.creator.showNextMonth();
        },
        autoParent: "navigationLayout",
        align: "center",
        noDoubleClicks: true
    },

    //> @attr dateChooser.nextYearButton (AutoChild IButton : null : IR)
    // A button shown in the +link{dateChooser.navigationLayout, navigation layout} that shifts
    // the calendar view forward by a year.
    // 
    // @visibility external
    //<
    nextYearButtonDefaults: {
        width: 20,
        click : function () {
            this.creator.showNextYear();
        },
        autoParent: "navigationLayout",
        align: "center",
        noDoubleClicks: true
    },
    
    //> @attr dateChooser.buttonLayout (AutoChild HLayout : null : IR)
    // An +link{AutoChild} +link{HLayout}, rendered below the +link{class:DateGrid, date grid},
    // and showing the +link{dateChooser.todayButton, Today},
    // +link{dateChooser.cancelButton, Cancel} and, when working with "datetime" values, 
    // +link{dateChooser.applyButton, Apply} buttons. 
    // @visibility external
    //<
    buttonLayoutConstructor: "HLayout",
    buttonLayoutDefaults: {
        width: 1,
        height: 1,
        overflow: "visible",
        layoutAlign: "center",
        extraSpace: 2
    },
    
    
    //> @attr dateChooser.dateGrid (AutoChild DateGrid : null : IR)
    // A +link{ListGrid} subclass, responsible for rendering the calendar view.
    // 
    // @visibility external
    //<
    dateGridDefaults: {
        _constructor: "DateGrid",
        autoDraw: false,
        layoutAlign: "center",
        dateClick : function (year, month, date) {
            this.creator.dateClick(year, month, date);
        },
        getSelectedDate : function () {
            return this.creator.chosenDate;
        },
        selectedWeekChanged : function (weekNum) {
            this.creator.updateWeekChooser(weekNum, true);
        }
    },
    
    bottomButtonConstructor:"IButton",
    
    //> @attr dateChooser.todayButton (AutoChild IButton : null : IR)
    // A button shown below the +link{class:DateGrid, calendar grid} which, when clicked, 
    // navigates the calendar to today.
    // 
    // @visibility external
    //<
    todayButtonDefaults: {
        padding: 2,
        autoFit: true,
        autoParent: "buttonLayout",
        click : function () {
            this.creator.todayClick();
        }
    },

    //> @attr dateChooser.cancelButton (AutoChild IButton : null : IR)
    // A button shown below the +link{class:DateGrid, calendar grid} which, when clicked, 
    // closes the DateChooser without selecting a value.
    // 
    // @visibility external
    //<
    cancelButtonDefaults: {
        padding: 2,
        autoFit: true,
        autoParent: "buttonLayout",
        click : function () {
            this.creator.cancelClick();
        }
    },

    //> @attr dateChooser.applyButton (AutoChild IButton : null : IR)
    // When a DateChooser is configured for +link{dateChooser.timeItem, a "datetime" value},
    // clicking on a date cell in the +link{dateChooser.dateGrid, grid} will not automatically
    // dismiss the DateChooser canvas.  In this case, use the <code>Apply</code> button to
    // accept the selected date and time and dismiss the chooser.
    // 
    // @visibility external
    //<
    applyButtonDefaults: {
        padding: 2,
        autoFit: true,
        autoParent: "buttonLayout",
        click : function () {
            this.creator.applyClick();
        }
    },

    //> @attr DateChooser.headerHeight (Integer : 20 : IR)
    // Height of the header area (containing the navigation buttons) in pixels.
    // @visibility external
    // @deprecated in favor of +link{dateChooser.navigationLayoutHeight}
    //<

    //> @attr DateChooser.navigationLayoutHeight (int : 20 : IR)
    // Height of the +link{dateChooser.navigationLayout, navigation area}, containing the 
    // various buttons for navigating the +link{dateChooser.dateGrid, calendar view}.
    // @visibility external
    // @deprecated in favor of +link{dateChooser.navigationLayoutHeight}
    //<
	navigationLayoutHeight:20,

    
	showYearButtons:true,
	showYearChooser:true,
	showMonthButtons:true,
	showMonthChooser:true,

    //> @attr DateChooser.skinImgDir (string : "images/common/" : IRWA)
    // Overridden directory where images for this widget (such as the month and year button icons)
    // may be found.
    // @visibility external
    //<
	skinImgDir:"images/common/",

    //> @attr DateChooser.prevYearIcon (URL : "[SKIN]doubleArrow_left.gif" : IR)
    // Icon for the previous year button
    // @see attr:DateChooser.showDoubleYearIcon
    // @visibility external
    //<
    prevYearIcon:"[SKIN]doubleArrow_left.gif",

    //> @attr DateChooser.prevYearIconRTL (URL : null : IRW)
    // Icon for the previous year button if +link{isc.Page.isRTL()} is true.
    // If not set, and the page is in RTL mode, the +link{nextYearIcon} will be
    // used in place of the +link{prevYearIcon} and vice versa.
    // @see attr:DateChooser.showDoubleYearIcon
    // @visibility external
    //<

    //> @attr DateChooser.prevYearIconWidth (int : 14 : IR)
    // Width of the icon for the previous year button
    // @visibility external
    //<
    prevYearIconWidth:14,
    //> @attr DateChooser.prevYearIconHeight (int : 7 : IR)
    // Height of the icon for the previous year button
    // @visibility external
    //<
    prevYearIconHeight:7,
    
    //> @attr DateChooser.prevMonthIcon (URL : "[SKIN]arrow_left.gif" : IR)
    // Icon for the previous month button
    // @visibility external
    //<
    prevMonthIcon:"[SKIN]arrow_left.gif",
    
    //> @attr DateChooser.prevMonthIconRTL (URL : null : IR)
    // Icon for the previous month button if +link{isc.Page.isRTL()} is true.
    // If not set, and the page is in RTL mode, the +link{nextMonthIcon} will be
    // used in place of the +link{prevMonthIcon} and vice versa.
    // @visibility external
    //<
    
    //> @attr DateChooser.prevMonthIconWidth (int : 7 : IR)
    // Width of the icon for the previous month button
    // @visibility external
    //<
    prevMonthIconWidth:7,
    
    //> @attr DateChooser.prevMonthIconHeight (int : 7 : IR)
    // Height of the icon for the previous month button
    // @visibility external
    //<
    prevMonthIconHeight:7,
    
    //> @attr DateChooser.nextYearIcon (URL : "[SKIN]doubleArrow_right.gif" : IR)
    // Icon for the next year button
    // @see attr:DateChooser.showDoubleYearIcon
    // @visibility external
    //<
    nextYearIcon:"[SKIN]doubleArrow_right.gif",

    //> @attr DateChooser.nextYearIconRTL (URL : null : IR)
    // Icon for the next year button if +link{isc.Page.isRTL()} is true.
    // If not set, and the page is in RTL mode, the +link{nextYearIcon} will be
    // used in place of the +link{prevYearIcon} and vice versa.
    // @see attr:DateChooser.showDoubleYearIcon
    // @visibility external
    //<
    
    //> @attr DateChooser.nextYearIconWidth (int : 14 : IR)
    // Width of the icon for the next year button
    // @visibility external
    //<
    nextYearIconWidth:14,    
    
    //> @attr DateChooser.nextYearIconHeight (int : 7 : IRW)
    // Height of the icon for the next year button
    // @visibility external
    //<
    nextYearIconHeight:7,
    
    //> @attr DateChooser.nextMonthIcon (URL : "[SKIN]arrow_right.gif" : IRW)
    // Icon for the next month button
    // @visibility external
    //< 
    nextMonthIcon:"[SKIN]arrow_right.gif",
    
    //> @attr DateChooser.nextMonthIconRTL (URL : null : IRW)
    // Icon for the next month button
    // @visibility external
    //< 

    //> @attr DateChooser.nextMonthIconWidth (int : 7 : IRW)
    // Width of the icon for the next month button if +link{isc.Page.isRTL()} is true.
    // If not set, and the page is in RTL mode, the +link{nextMonthIcon} will be
    // used in place of the +link{prevMonthIcon} and vice versa.
    // @visibility external
    //<
    nextMonthIconWidth:7,
    
    //> @attr DateChooser.nextMonthIconHeight (int : 7 : IRW)
    // Height of the icon for the next month button
    // @visibility external
    //<
    nextMonthIconHeight:7,
    
    //> @attr DateChooser.showDoubleYearIcon (boolean : true : IRW)
    // If this property is set to true the previous and next year buttons will render out the 
    // previous and next month button icons twice rather than using the
    // +link{DateChooser.prevYearIcon} and +link{DateChooser.nextYearIcon}.
    // <P>
    // Set to <code>true</code> by default as not all skins contain media for the year icons.
    // @visibility external
    //<
    // This is really for back-compat (pre 6.1).
    // We intend to set this to true and provide year icon media in all skins we provide from this
    // point forward, but we don't want to break existing customized skins
    showDoubleYearIcon:true,

    // Pop-up Year & Month Pickers
    // ---------------------------------------------------------------------------------------
    
    //> @attr DateChooser.yearMenuStyle (CSSStyleName : "dateChooserYearMenu" : IR)
    // Style for the pop-up year menu.
    // @visibility external
    //<
    yearMenuStyle:"dateChooserYearMenu",

    //> @attr DateChooser.startYear (int : 1995 : IR)
    // Earliest year that may be selected.
    // @visibility external
    //<
	startYear:1995,

    //> @attr DateChooser.endYear (int : 2020 : IR)
    // Last year that may be selected.
    // @visibility external
    //<
	endYear:2020,

    //> @attr DateChooser.monthMenuStyle (CSSStyleName : "dateChooserMonthMenu" : IR)
    // Style for the pop-up year menu.
    // @visibility external
    //<
    monthMenuStyle:"dateChooserMonthMenu",
    
    //> @attr DateChooser.weekMenuStyle (CSSStyleName : "dateChooserWeekMenu" : IR)
    // Style for the pop-up week menu.
    // @visibility external
    //<
    weekMenuStyle:"dateChooserWeekMenu",

    // Today / Cancel Buttons
    // ---------------------------------------------------------------------------------------
    
    //> @attr DateChooser.showTodayButton (Boolean : true : IRW)
    // Determines whether the "Today" button will be displayed, allowing the user to select 
    // the current date.
    // @visibility external
    //<
	showTodayButton:true,

    //> @attr DateChooser.showCancelButton (Boolean : false : IRW)
    // Determines whether the "Cancel" button will be displayed.
    // @visibility external
    //<    
	showCancelButton:false,
    
    //> @attr DateChooser.showApplyButton (Boolean : null : IRW)
    // Determines whether the +link{applyButton} will be displayed.
    // @visibility external
    //<    

    //> @attr DateChooser.todayButtonTitle  (string:"Today":IRW)
    // Title for "Today" button.
    // @group i18nMessages
    // @visibility external
    //<
    todayButtonTitle:"Today",
    
    //> @attr DateChooser.cancelButtonTitle  (string:"Cancel":IRW)
    // Title for the cancellation button.
    // @group i18nMessages
    // @visibility external
    //<
    cancelButtonTitle:"Cancel",
 
    //> @attr DateChooser.applyButtonTitle  (string:"Apply":IRW)
    // Title for the +link{dateChooser.applyButton, Apply} button.
    // @group i18nMessages
    // @visibility external
    //<
    applyButtonTitle:"Apply",

    //> @attr DateChooser.todayButtonHeight  (integer:null:IRW)
    // If set specifies a fixed height for the Today and Cancel buttons.
    // @visibility external
    //<
    //todayButtonHeight:null,
    
    // Weekends   
    // ---------------------------------------------------------------------------------------

    //> @attr DateChooser.disableWeekends (Boolean : false : IR)
    // Whether it should be valid to pick a weekend day.  If set to true, weekend days appear
    // in disabled style and cannot be picked. 
    // <P>
    // Which days are considered weekends is controlled by +link{Date.weekendDays}.
    //
    // @visibility external
    //<
    disableWeekends: false,
    
    //> @attr DateChooser.showWeekends (Boolean : true : IR)
    // Whether weekend days should be shown.  Which days are considered weekends is controlled
    // by +link{Date.weekendDays}.
    //
    // @visibility external
    //<
    showWeekends: true,
    
    //> @attr DateChooser.firstDayOfWeek  (int : 0 : IR)
    // Day of the week to show in the first column.  0=Sunday, 1=Monday, ..., 6=Saturday.  The
    // default value for this attribute is picked up from the current locale and can also be 
    // altered system-wide with the +link{Date.setFirstDayOfWeek, global setter}.
    // 
    // @group i18nMessages, appearance
    // @visibility external
    //<
    
    firstDayOfWeek:0,

    // Initial value   
    // ---------------------------------------------------------------------------------------

	year:new Date().getFullYear(),		// full year number
	month:new Date().getMonth(),		// 0-11
	chosenDate:new Date(),	// JS date object -- defaults to today
    
    // Day Buttons styling
    // ---------------------------------------------------------------------------------------

    //> @attr DateChooser.baseButtonStyle (CSSStyleName : "dateChooserButton" : IRW)
    // Base CSS style applied to this picker's buttons. Will have "Over", "Selected" and "Down"
    // suffix appended as the user interacts with buttons.
    // @visibility external
    //< 
	baseButtonStyle:"dateChooserButton",

    //> @attr DateChooser.baseWeekdayStyle (CSSStyleName : "dateChooserWeekday" : IRW)
    // Base CSS style applied to weekdays. Will have "Over", "Selected" and "Down"
    // suffix appended as the user interacts with buttons.  Defaults to +link{baseButtonStyle}.
    // @visibility external
    //<
    baseWeekdayStyle: "dateChooserWeekday",

    //> @attr DateChooser.baseWeekendStyle (CSSStyleName : "dateChooserWeekend" : IRW)
    // Base CSS style applied to weekends. Will have "Over", "Selected" and "Down"
    // suffix appended as the user interacts with buttons.  Defaults to +link{baseWeekdayStyle}.
    // @visibility external
    //<
    baseWeekendStyle: "dateChooserWeekend",

    //> @attr DateChooser.baseFiscalYearStyle (CSSStyleName : "dateChooserFiscalYearCell" : IRW)
    // Base CSS style applied to cells in the +link{showFiscalYearChooser, fiscal year column}.
    // @visibility external
    //<
    baseFiscalYearStyle: "dateChooserFiscalYearCell",

    //> @attr DateChooser.fiscalYearHeaderStyle (CSSStyleName : null : IRW)
    // Base CSS style applied to the header of the 
    // +link{showFiscalYearChooser, fiscal year column} in the 
    // +link{dateChooser.dateGrid, calendar view}.
    // @visibility external
    //<

    //> @attr DateChooser.baseWeekStyle (CSSStyleName : "dateChooserWeekCell" : IRW)
    // Base CSS style applied to cells in the +link{showWeekChooser, fiscal week column}.
    // @visibility external
    //<
    baseWeekStyle: "dateChooserWeekCell",

    //> @attr DateChooser.weekHeaderStyle (CSSStyleName : null : IRW)
    // Base CSS style applied to the header of the 
    // +link{showWeekChooser, fiscal or calendar week column} in the
    // +link{dateChooser.dateGrid, calendar view}.
    // @visibility external
    //<

    //> @attr DateChooser.disabledDates (Array of Date : null : IRW)
    // An array of Date instances that should be disabled if they appear in the calendar view.
    // @visibility external
    //<

    //> @attr DateChooser.disabledWeekdayStyle (CSSStyleName : "dateChooserDisabledWeekday" : IRW)
    // Base CSS style applied to weekday dates which have been +link{disabledDates, disabled}.
    // @visibility external
    //<
    disabledWeekdayStyle: "dateChooserDisabledWeekday",

    //> @attr DateChooser.disabledWeekendStyle (CSSStyleName : "dateChooserDisabledWeekend" : IRW)
    // Base CSS style applied to weekend dates which have been +link{disabledDates, disabled}.
    // @visibility external
    //<
    disabledWeekendStyle: "dateChooserDisabledWeekend",

    //> @attr DateChooser.selectedWeekStyle (CSSStyleName : "dateChooserSelectedWeek" : IRW)
    // CSS style applied to the Fiscal Year and Week columns for the currently selected week 
    // (the one being displayed in the +link{dateChooser.showWeekChooser, week chooser}).
    // @visibility external
    //<
    selectedWeekStyle: "dateChooserSelectedWeek",

    //> @attr DateChooser.alternateWeekStyles (boolean:null:IRW)
    // Whether alternating weeks should be drawn in alternating styles. If enabled, the cell style
    // for alternate rows will have +link{alternateStyleSuffix} appended to it.
    // @visibility external
    //<    

    //> @attr DateChooser.alternateStyleSuffix (string:"Dark":IRW)
    // The text appended to the style name when using +link{alternateWeekStyles}.
    // @visibility external
    //<    
    alternateStyleSuffix:"Dark",
    
    //> @attr DateChooser.headerStyle (CSSStyleName : "dateChooserButtonDisabled" : IRW)
    // CSS style applied to the day-of-week headers. By default this applies to all days of the 
    // week. To apply a separate style to weekend headers, set 
    // +link{DateChooser.weekendHeaderStyle}
    // 
    // @visibility external
    //<    
	headerStyle:"dateChooserButtonDisabled",
    
    //> @attr DateChooser.weekendHeaderStyle (string:null:IRW)
    // Optional CSS style applied to the day-of-week headers for weekend days. If unset 
    // +link{DateChooser.headerStyle} will be applied to both weekdays and weekend days.
    // @visibility external
    //<    
	//weekendHeaderStyle:null,
    
    //> @attr DateChooser.baseNavButtonStyle (CSSStyleName : null : IRW)
    // CSS style to apply to navigation buttons and date display at the top of the
    // component. If null, the CSS style specified in +link{baseButtonStyle} is used.
    // @visibility external
    //< 
    
    //> @attr DateChooser.navButtonConstructor (SCClassName : IButton : IRA)
    // Constructor for navigation buttons at the top of the component.
    // @visibility external
    //<
    navButtonConstructor: "IButton",

    //> @attr DateChooser.baseBottomButtonStyle (CSSStyleName : null : IRW)
    // CSS style to apply to the buttons at the bottom of the DateChooser ("Today" and
    // "Cancel").  If null, the CSS style specified in +link{baseButtonStyle} is used.
    // @visibility external
    //< 


    
    useBackMask:true,
    
    canFocus:true,
    
    //> @attr DateChooser.useFirstDayOfFiscalWeek (Boolean : true : IRW)
    // When showing the +link{showFiscalYearChooser, fiscal year chooser}, should firstDayOfWeek
    // be defaulted to the same day as the fiscal start date?  If true and a fiscal year 
    // starts on a Tuesday, the calendar will display Tuesday to Monday from left to right.
    // @visibility external
    //< 
    useFirstDayOfFiscalWeek: true,

    //> @attr dateChooser.timeLayout (AutoChild HLayout : null : IR)
    // An +link{AutoChild} +link{HLayout}, rendered below the +link{class:DateGrid, date grid},
    // and showing the +link{dateChooser.timeItem, timeItem},
    // @visibility internal
    //<
    timeLayoutConstructor: "HLayout",
    timeLayoutDefaults: {
        width: 1,
        height: 1,
        overflow: "visible",
        layoutAlign: "center",
        extraSpace: 1
    },
    timeFormDefaults: {
        _constructor: "DynamicForm",
        width: 1,
        overflow: "visible",
        layoutAlign: "center"
    },

    //> @attr dateChooser.closeOnEscapeKeypress (boolean : false : IR)
    // Should this dateChooser be dismissed if the user presses the Escape key?
    // @visibility external
    //<
    closeOnEscapeKeypress: false,

    //> @attr dateChooser.timeItem (AutoChild TimeItem : null : R)
    // +link{TimeItem} for editing the time portion of dates.  Visible by default for fields 
    // of type "datetime" and can be controlled by setting +link{dateChooser.showTimeItem}.
    // 
    // @visibility external
    //<
    
    //> @attr dateChooser.timeItemProperties (TimeItem properties : null : IRA)
    // Custom properties to apply to the +link{dateChooser.timeItem,time field} used 
    // for editing the time portion of the date.
    // @visibility external
    //<
    
    //> @attr DateChooser.showTimeItem  (Boolean : null : IRW)
    // Whether to show the +link{dateChooser.timeItem, time field} for editing the time portion
    // of the date.  When unset, the time field is shown automatically if the field type is
    // "datetime".
    // @visibility external
    //<
    timeItemDefaults: {
        name: "time", 
        editorType: "TimeItem", 
        useTextField: false,
        showTitle: false
    },

    //> @attr DateChooser.timeItemTitle  (string : "Time" : IRW)
    // Title for the +link{dateChooser.timeItem,time field}.
    // @group i18nMessages
    // @visibility external
    //<
    timeItemTitle: "Time",

    //> @attr DateChooser.use24HourTime (Boolean : true : IRW)
    // When showing the +link{showTimeItem, time field}, whether the 
    // +link{class:TimeItem, TimeItem} should be set to use 24-hour time.  The default is true.
    // @visibility external
    //< 
    use24HourTime: true,
    
    //> @attr DateChooser.fiscalYearFieldTitle  (string : "Year" : IRW)
    // Title for the +link{dateChooser.showFiscalYearChooser,fiscal year} field in the date grid.
    // @group i18nMessages
    // @visibility external
    //<
    fiscalYearFieldTitle: "Year",

    //> @attr DateChooser.weekFieldTitle  (string : "Wk" : IRW)
    // Title for the +link{dateChooser.showWeekChooser,week} field in the date grid.
    // @group i18nMessages
    // @visibility external
    //<
    weekFieldTitle: "Wk"

    //> @attr DateChooser.showSecondItem  (Boolean : null : IRW)
    // When showing the +link{dateChooser.timeItem, time field}, whether to show the "second" 
    // picker.  When unset, the second field is not shown.
    // @visibility external
    //<

});

//!>Deferred
isc.DateChooser.addMethods({

    initWidget : function () {
        this.Super("initWidget", arguments);
        
        if (this.showFiscalYearChooser && this.useFirstDayOfFiscalWeek) {
            var fDate = Date.getFiscalStartDate(new Date(), this.getFiscalCalendar());
            this.firstDayOfWeek = fDate.getDay();
        }

        if (this.headerHeight != null) this.navigationLayoutHeight = this.headerHeight;
        
        if (this.showNavigationLayout != false) {
            this.addAutoChild("navigationLayout", {}, this.navigationLayoutConstructor);
            this.addMember(this.navigationLayout);

            this.addAutoChild("fiscalYearChooserButton", {
                baseStyle:(this.baseNavButtonStyle || this.baseButtonStyle),
                title: this.chosenDate.getFiscalYear(this.getFiscalCalendar()).fiscalYear,
                autoDraw: false
            },
            this.navButtonConstructor);

            this.addAutoChild("weekChooserButton", {
                baseStyle:(this.baseNavButtonStyle || this.baseButtonStyle),
                title: this.showFiscalYearChooser ? 
                        this.chosenDate.getFiscalWeek(this.getFiscalCalendar()) : 
                        this.chosenDate.getWeek(this.firstDayOfWeek),
                autoDraw: false
            },
            this.navButtonConstructor);

            if (this.showYearButtons) {
                this.addAutoChild("previousYearButton", {
                    baseStyle:(this.baseNavButtonStyle || this.baseButtonStyle),
                    title: this.getPreviousYearIconHTML()
                },
                this.navButtonConstructor);
            }
            if (this.showMonthButtons) {
                this.addAutoChild("previousMonthButton", {
                    baseStyle:(this.baseNavButtonStyle || this.baseButtonStyle),           
                    title: this.getPreviousMonthIconHTML()
                },
                this.navButtonConstructor);
            }
            if (this.showMonthChooser != false) {
                this.addAutoChild("monthChooserButton", {
                    baseStyle:(this.baseNavButtonStyle || this.baseButtonStyle),           
                    title: this.chosenDate.getShortMonthName()
                },
                this.navButtonConstructor);
            }
            if (this.showYearChooser != false) {
                this.addAutoChild("yearChooserButton", {
                    baseStyle:(this.baseNavButtonStyle || this.baseButtonStyle),           
                    title: this.chosenDate.getFullYear()
                },
                this.navButtonConstructor);
            }
            if (this.showMonthButtons) {
                baseStyle:(this.baseNavButtonStyle || this.baseButtonStyle),            
                this.addAutoChild("nextMonthButton", {
                    baseStyle:(this.baseNavButtonStyle || this.baseButtonStyle),           
                    title: this.getNextMonthIconHTML()
                },
                this.navButtonConstructor);
            }
            if (this.showYearButtons) {
                baseStyle:(this.baseNavButtonStyle || this.baseButtonStyle),            
                this.addAutoChild("nextYearButton", {
                    baseStyle:(this.baseNavButtonStyle || this.baseButtonStyle),
                    title: this.getNextYearIconHTML()
                },
                this.navButtonConstructor);
            }
            
        }

        var item = isc.addProperties({}, 
                { title: this.timeItemTitle, use24HourTime: this.use24HourTime },
                this.timeItemDefaults, 
                this.timeItemProperties,
                { name: "time" }
        );
        this.addAutoChild("timeLayout");
        this.addAutoChild("timeForm", { items: [item] });
        this.timeLayout.addMember(this.timeForm);
        this.addMember(this.timeLayout);
        this.timeLayout.hide();

        if (this.showTodayButton || this.showCancelButton) {
            var props = {};
            if (this.todayButtonHeight != null) props.height = this.todayButtonHeight;

            this.addAutoChild("buttonLayout", props, this.buttonLayoutConstructor);
            this.addMember(this.buttonLayout);

            props.baseStyle = this.baseBottomButtonStyle || this.baseButtonStyle;

            props.title = this.todayButtonTitle;
            this.addAutoChild("todayButton", props, this.bottomButtonConstructor);

            props.title = this.cancelButtonTitle;
            this.addAutoChild("cancelButton", props, this.bottomButtonConstructor);

            props.title = this.applyButtonTitle;
            this.addAutoChild("applyButton", props, this.bottomButtonConstructor);
            if (this.applyButton) this.applyButton.hide();
        }
        this.markForRedraw();
        if (this.chosenDate) {
            this.year = this.chosenDate.getFullYear();
            this.month = this.chosenDate.getMonth();
        }
        this.updateUI();
    },
    
    draw : function () {
        this.Super("draw", arguments);
        if (!this.dateGrid) {
            var usedHeight = 0;
            if (this.navigationLayout && this.navigationLayout.isVisible()) {
                usedHeight += this.navigationLayout.getVisibleHeight();
            }
            if (this.timeLayout && this.timeLayout.isVisible()) {
                usedHeight += this.timeLayout.getVisibleHeight();
            }
            if (this.buttonLayout && this.buttonLayout.isVisible()) {
                usedHeight += this.buttonLayout.getVisibleHeight();
            }

            var gridProps = { startDate: this.chosenDate, dayNameLength: this.dayNameLength,
                showFiscalYear: this.showFiscalYearChooser,
                fiscalYearFieldTitle: this.fiscalYearFieldTitle,
                showFiscalWeek: this.showFiscalYearChooser && this.showWeekChooser,
                showCalendarWeek: !this.showFiscalYearChooser && this.showWeekChooser,
                weekFieldTitle: this.weekFieldTitle,
                disabledDates: this.disabledDates,
                firstDayOfWeek: this.firstDayOfWeek,
                headerBaseStyle: this.headerStyle,
                weekendHeaderStyle: this.weekendHeaderStyle || this.headerStyle,
                baseFiscalYearStyle: this.baseFiscalYearStyle,
                fiscalYearHeaderStyle: this.fiscalYearHeaderStyle || this.baseFiscalYearStyle,
                baseWeekStyle: this.baseWeekStyle,
                weekHeaderStyle: this.weekHeaderStyle || this.baseWeekStyle,
                baseWeekdayStyle: this.baseWeekdayStyle || this.baseButtonStyle,
                baseWeekendStyle: this.baseWeekendStyle || this.baseWeekdayStyle || this.baseButtonStyle,
                alternateRecordStyles: this.alternateWeekStyles,
                disabledWeekdayStyle: this.disabledWeekdayStyle,
                disabledWeekendStyle: this.disabledWeekendStyle,
                selectedWeekStyle: this.selectedWeekStyle,
                fiscalCalendar: this.getFiscalCalendar(),
                showWeekends: this.showWeekends,
                disableWeekends: this.disableWeekends,
                locatorParent: this,
                width: "100%", height: "*",
                _availableHeight: this.getHeight() - usedHeight
            };

            this.addAutoChild("dateGrid", gridProps);
            this.addMember(this.dateGrid, this.navigationLayout ? 1 : 0);
        }
    },

    getTimeItem : function () {
        if (this.timeForm) return this.timeForm.getItem("time");
    },
    recreateTimeItem : function (value) {
        var item = isc.addProperties({}, { title: this.timeItemTitle, 
                    use24HourTime: this.use24HourTime, showSecondItem: !!this.showSecondItem },
                this.timeItemDefaults, 
                this.timeItemProperties,
                { name: "time", value: value }
        );
        this.timeForm.setItems([item]);
    },

    resized : function () {
        //if (this.navigationLayout && this.navigationLayout.isDrawn()) this.navigationLayout.redraw();
    },

    handleKeyPress : function () {
        var returnVal = this.Super("handleKeyPress", arguments);
        if (returnVal != false) {
            if ((this.closeOnEscapeKeypress) && ("Escape" == isc.EH.getKey())) {
                this.cancelClick();
            }
        }
    },

    getPreviousYearIconHTML : function () {
        var prevYearIconHTML,
            displayDate = new Date(this.year, this.month, 1),
            disableNextYear = displayDate.getFullYear() == 9999
        ;
        if (this.showDoubleYearIcon) {
            var monthIconHTML = this.getPreviousMonthIconHTML();
            prevYearIconHTML = disableNextYear ? "&nbsp;" :
                   "<NOBR>"+ monthIconHTML + monthIconHTML + "<\/NOBR>";
        } else {
            var icon = this.isRTL() ? 
                    this.prevYearIconRTL || this.nextYearIcon : this.prevYearIcon;
            prevYearIconHTML = disableNextYear ? "&nbsp;" :
                        this.imgHTML(icon, this.prevYearIconWidth,
                                         this.prevYearIconHeight);
        }

        return prevYearIconHTML;
    },
    
    getPreviousMonthIconHTML : function () {
        var icon = this.isRTL() ? 
                this.prevMonthIconRTL || this.nextMonthIcon : this.prevMonthIcon,
            monthIconHTML = this.imgHTML(icon, this.prevMonthIconWidth,
                                                 this.prevMonthIconHeight);
        return monthIconHTML;
    },

    getNextMonthIconHTML : function () {
        var icon = this.isRTL() ? 
                this.nextMonthIconRTL || this.prevMonthIcon : this.nextMonthIcon,
            monthIconHTML = this.imgHTML(icon, this.nextMonthIconWidth,
                                                 this.nextMonthIconHeight);
        return monthIconHTML;
    },

    getNextYearIconHTML : function () {
        var nextYearIconHTML,
            displayDate = new Date(this.year, this.month, 1),
            disableNextYear = displayDate.getFullYear() == 9999
        ;
        if (this.showDoubleYearIcon) {
            var monthIconHTML = this.getNextMonthIconHTML();
            nextYearIconHTML = disableNextYear ? "&nbsp;" :
                               "<NOBR>"+ monthIconHTML + monthIconHTML + "<\/NOBR>";
        } else {
            var icon = this.isRTL() ? 
                    this.nextYearIconRTL || this.prevYearIcon : this.nextYearIcon;
            nextYearIconHTML = disableNextYear ? "&nbsp;" :
                                    this.imgHTML(icon,
                                                 this.nextYearIconWidth,
                                                 this.nextYearIconHeight);
        }
        
        return nextYearIconHTML;
    },

    // Override show() to show the clickMask if autoClose is true
    // Note: If we're showing this date chooser in a separate window, this is unnecessary, as the
    // user will be unable to click on any part of the window that isn't covered by the date-chooser
    // but will do no harm.
    show : function () {
        var returnVal = this.Super("show", arguments);
        
        
        if (this.autoClose) {                
            // pass this dateChooser as an unmasked widget to showClickMask because
            // when the dateChooser is shown from a modal window, the dateChooser
            // ends up being masked by its own clickmask for some unknown reason.
			this.showClickMask(this.getID()+".close();", true, this);        	        	        	
        	this.bringToFront();
        }
    },
    
    // picker interface

    //> @method DateChooser.setData()
    // Set the picker to show the given date.
    // 
    // @param date (Date) new value
    // @visibility external
    //<
    setData : function (data) {
        if (!isc.isA.Date(data)) data = new Date();

        var type = "datetime";
        if (this.callingFormItem) {
            type = this.callingFormItem.type;
        }

        var dateOnly = Date.getLogicalDateOnly(data),
            timeOnly = Date.getLogicalTimeOnly(data)
        ;

        this.year = data.getFullYear();
        this.month = data.getMonth();

        
        var hours = isc.Time.getUTCHoursDisplayOffset(data),
            minutes = isc.Time.getUTCMinutesDisplayOffset(data),
            offset = ((hours * 60) + minutes + data.getTimezoneOffset()) * 60000
        ;
        this.chosenDate = new Date(data.getTime() + offset);

        // set the timeItem's value, if it's there
        var timeItem = this.getTimeItem();
        if (timeItem) timeItem.setValue(timeOnly);

        this.updateUI();
        if (this.dateGrid) this.dateGrid.setStartDate(this.chosenDate);
    },

    updateGridData : function (date) {
        if (!this.dateGrid) return;
        date.setDate(1);

        var fy = Date._getFiscalYearObjectForDate(date),
            fiscalStart = fy.startDate
        ;

        this.dateGrid.showWeekends = this.showWeekends;

        this.dateGrid.showFiscalYear = this.showFiscalYearChooser;
        this.dateGrid.showFiscalWeek = this.showFiscalYearChooser && this.showWeekChooser;
        this.dateGrid.showCalendarWeek = !this.showFiscalYearChooser && this.showWeekChooser;

        if (this.showFiscalYearChooser) {
            if (this.useFirstDayOfFiscalWeek) {
                // if using fiscal startDate.getDay() as firstDayOfWeek, we need to use the
                // fiscalYear in which the startDate exists, not the one in which the start of
                // the month exists
                var nfy = Date.getFiscalYear(fy.fiscalYear + 1);
                if (nfy.year < fy.fiscalYear) nfy = Date.getFiscalYear(nfy.fiscalYear + 1);
                this.dateGrid.firstDayOfWeek = this.firstDayOfWeek = nfy.startDate.getDay();
            }
        }
        this.dateGrid.refreshUI(date);
    },

    //> @method DateChooser.getData()
    // Get the current value of the picker.
    // <P>
    // See +link{dataChanged()} for how to respond to the user picking a date.
    //
    // @return (Date) current date
    // @visibility external
    //<
    
    getData : function () {
        return this.chosenDate;
    },
    
    redraw : function () {
        this.Super("redraw", arguments);
        this.updateUI();
    },
	
	//> @attr DateChooser.dayNameLength (number : 2 : IR)
	// How long (how many characters) should be day names be. May be 1, 2 or 3 characters.
	// @visibility external
	//<
	dayNameLength:2,

    getDayNames : function () {
        if (isc.DateChooser._dayNames == null) {
            // Don't hard-code day-names -- we need them to be localizeable
            // isc.DateChooser._dayNames = ["Su", "Mo","Tu", "We", "Th", "Fr", "Sa"]
            // Support 1, 2 or 3 chars
            isc.DateChooser._dayNames = [Date.getShortDayNames(1),Date.getShortDayNames(2),Date.getShortDayNames(3)];
        }
        return isc.DateChooser._dayNames[this.dayNameLength-1];
    },

	getDayCellButtonHTML : function (date, style, state) {
        // null date == Special case for dates beyond 9999
        // This limit is enforced due to dates greater than 9999 causing a browser crash in IE
        // - also our parsing logic assumes a 4 digit date
        if (date == null) 
            return this.getCellButtonHTML("&nbsp;", null, style, false, false, isc.Canvas.CENTER);
        
        
		var selected = (this.chosenDate && (Date.compareLogicalDates(date,this.chosenDate) == 0)),
            disabled = (date.getMonth() != this.month);

		var partEvent = "dateFromId",
            id = date.getFullYear() + "_" + date.getMonth() + "_" + date.getDate();

        // check for weekends
        if (this.disableWeekends && Date.getWeekendDays().contains(date.getDay())) {
            disabled = true;
            partEvent = null;
        }           
		return this.getCellButtonHTML(date.getDate(), style, selected, disabled, 
                                      isc.Canvas.CENTER, null, partEvent, id);
	},

    dateIsSelected : function (date) {
		return null
	},
    
	showPrevMonth : function () {
		if (--this.month == -1) {
			this.month = 11;
			this.year--;
		}
		this.updateUI();
	},

	showNextMonth : function () {
		if (++this.month == 12) {
			this.month = 0;
			this.year++;
		}
        this.updateUI();
    },
    
    updateHeader : function (weekNum, date) {
        if (!this.showNavigationLayout && this.navigationLayout) {
            this.navigationLayout.hide();
        } else if (this.showNavigationLayout) {
            this.navigationLayout.show();

            var members = this.navigationLayout.members;
            if (this.weekChooserButton) {
                if (this.showWeekChooser && !members.contains(this.weekChooserButton)) {
                    this.navigationLayout.addMember(this.weekChooserButton, 0);
                    this.weekChooserButton.show();
                } else if (!this.showWeekChooser && members.contains(this.weekChooserButton)) {
                    this.navigationLayout.removeMember(this.weekChooserButton);
                    this.weekChooserButton.hide();
                }
                this.updateWeekChooser(weekNum != null ? weekNum : 
                    (this.fiscalYearChooserButton ? date.getFiscalWeek(this.getFiscalCalendar()) : 
                    new Date(date.getTime() + (4*86400000)).getWeek(this.firstDayOfWeek)));
            }
            if (this.fiscalYearChooserButton) {
                if (this.showFiscalYearChooser && !members.contains(this.fiscalYearChooserButton)) {
                    this.navigationLayout.addMember(this.fiscalYearChooserButton, 0);
                    this.fiscalYearChooserButton.show();
                } else if (!this.showFiscalYearChooser && members.contains(this.fiscalYearChooserButton)) {
                    this.navigationLayout.removeMember(this.fiscalYearChooserButton);
                    this.fiscalYearChooserButton.hide();
                }
                this.fiscalYearChooserButton.setTitle("" + date.getFiscalYear(this.getFiscalCalendar()).fiscalYear);
            }
            this.monthChooserButton.setTitle(date.getShortMonthName());
            this.yearChooserButton.setTitle("" + this.year);

            var isFirstYear = this.startYear && this.startYear == date.getFullYear(),
                isLastYear = this.endYear && this.endYear == date.getFullYear()
            ;
            this.previousYearButton.setDisabled(isFirstYear);
            this.previousMonthButton.setDisabled(isFirstYear && date.getMonth() == 0);
            this.nextMonthButton.setDisabled(isLastYear && date.getMonth() == 11);
            this.nextYearButton.setDisabled(isLastYear);
        }
    },
    updateUI : function (weekNum) {
        // update month/year button titles
        var date = new Date(this.year, this.month, this.chosenDate.getDate()); //1);

        if (date.getMonth() > this.month) date = isc.DateUtil.getEndOf(new Date(this.year, this.month, 1), "M", true);

        this.updateHeader(weekNum, date);

        if (!this.showTimeItem && this.timeForm) {
            this.timeLayout.hide();
            if (this.applyButton) this.applyButton.hide();
        } else if (this.showTimeItem) {
            this.recreateTimeItem(isc.Date.getLogicalTimeOnly(this.chosenDate));
            this.timeLayout.show();
            if (this.applyButton) this.applyButton.show();
        }

		this.updateGridData(date);
	},

    updateWeekChooser : function (weekNum, skipGridUpdate) {
        if (this.weekChooserButton) {
            this.weekChooserButton.setTitle("" + weekNum);
            if (!skipGridUpdate && this.dateGrid) this.dateGrid.setSelectedWeek(weekNum);
        }
    },

	showMonth : function (monthNum) {
		this.month = monthNum;
		if (this.monthMenu) this.monthMenu.hide();
        this.bringToFront();
        this.updateUI();
	},


    //> @method DateChooser.getFiscalCalendar()
    // Returns the +link{FiscalCalendar} object that will be used by this DateChooser.
    //
    // @return (FiscalCalendar) the fiscal calendar for this chooser, if set, or the global
    //            one otherwise
    // @visibility external
    //<
    getFiscalCalendar : function () {
        return this.fiscalCalendar || Date.getFiscalCalendar();
    },

    //> @method DateChooser.setFiscalCalendar()
    // Sets the +link{FiscalCalendar} object that will be used by this DateChooser.  If unset,
    // the _link{Date.getFiscalCalendar, global fiscal calendar} is used.
    //
    // @param [fiscalCalendar] (FiscalCalendar) the fiscal calendar for this chooser
    // @visibility external
    //<
    setFiscalCalendar : function (fiscalCalendar) {
        this.fiscalCalendar = fiscalCalendar;
    },
    
	showWeek : function (weekNum) {
        if (this.fiscalYearChooserButton) {
            var displayDate = Date.createLogicalDate(this.year, this.month, this.chosenDate.getDate());
            var cal = this.getFiscalCalendar(),
                fiscalStart = Date.getFiscalStartDate(displayDate),
                date = new Date(fiscalStart.getFullYear(), cal.defaultMonth, cal.defaultDate + (7 * weekNum))
            ;
        } else {
            date = new Date(this.year, 0, 1 + (7 * weekNum));
        }

        this.year = date.getFullYear();
        this.month = date.getMonth();
		if (this.weekMenu) this.weekMenu.hide();
        this.bringToFront();
        this.updateUI(weekNum);
	},

    monthMenuFormat: "MMM",
    getMonthText : function (date) {
        var result = isc.DateUtil.format(date, this.monthMenuFormat);
        return result;
    },
	showMonthMenu : function () {
		if (!this.monthMenu) {
			// create the menu items using the date.getShortMonthName() for internationalization
			var monthItems = [[]],
				date = Date.createLogicalDate(2001,0,1);
			for (var i = 0; i < 12; i++) {
				date.setMonth(i);
				monthItems[monthItems.length-1].add(
									{	contents:this.getMonthText(date),
                                        eventPart: "showMonth",
										eventId: i
									}
					);
				if ((i+1)%3 == 0) monthItems.add([]);
			}
			this.monthMenu = isc.MonthChooser.newInstance({
                styleName:this.monthMenuStyle,
				left:this.monthChooserButton.getPageLeft()+5,
				top:this.getPageTop()+this.navigationLayoutHeight,
				width:Math.min(this.getVisibleWidth(), 120),
				height:Math.min(this.getVisibleHeight()-this.navigationLayoutHeight, 80),
				items:monthItems,
				visibility:isc.Canvas.HIDDEN,
				baseButtonStyle:this.baseButtonStyle,
                dateChooser: this
			});
            // (autoDraw is true, so it is drawn, with visibility hidden at this point)
            var left = this.monthChooserButton.getPageLeft() - 
                        ((this.monthMenu.getWidth() - this.monthChooserButton.getWidth()) /2);
            this.monthMenu.setPageLeft(Math.max(left, 0));
		} else {
            // L, T, W, H
            var top = this.getPageTop()+this.navigationLayoutHeight,
				width = Math.min(this.getVisibleWidth(), 120),
				height = Math.min(this.getVisibleHeight()-this.navigationLayoutHeight, 80),
                buttonWidth = this.monthChooserButton.getWidth(),
                left = this.monthChooserButton.getPageLeft() - ((width - buttonWidth)/2)
            ;
            this.monthMenu.setPageRect(left, top, width, height);
        }
        
        // We show the month menu modally.  This means if the user clicks outside it, we
        // will not allow the click to carry on down, so it will hide the month menu (and then
        // dismiss the monthMenu's click mask), but won't fire the click action on the 
        // DateChooser's click mask and hide the entire date chooser.
        // As with all modal clickMasks, for us to float the month menu above it, we need the
        // month menu to be a top-level element (which is how it's currently implemented)
		this.monthMenu.showModal();
	},

	showWeekMenu : function () {
		if (!this.weekMenu) {
			// create the menu items using the date.getShortMonthName() for internationalization
			var weekItems = [[]],
				date = Date.createLogicalDate(2001,0,1);
			for (var i = 1; i < 53; i++) {
				weekItems[weekItems.length-1].add(
									{	contents:"" + i,
                                        eventPart: "showWeek",
										eventId: i
									}
					);
				if ((i)%7 == 0) weekItems.add([]);
			}
            
			this.weekMenu = isc.WeekChooser.newInstance({
                styleName:this.weekMenuStyle,
				left:this.weekChooserButton.getPageLeft()+5,
				top:this.getPageTop()+this.navigationLayoutHeight,
				width:Math.min(this.getVisibleWidth(), 120),
				height:Math.min(this.getVisibleHeight()-this.navigationLayoutHeight, 80),
				items:weekItems,
				visibility:isc.Canvas.HIDDEN,
				baseButtonStyle:this.baseButtonStyle,
                dateChooser: this
			});
            // (autoDraw is true, so it is drawn, with visibility hidden at this point)
            var left = this.weekChooserButton.getPageLeft() - 
                        ((this.weekMenu.getWidth() - this.weekChooserButton.getWidth()) /2);
            this.weekMenu.setPageLeft(Math.max(left, 0));
		} else {
            // L, T, W, H
            var top = this.getPageTop()+this.navigationLayoutHeight,
				width = Math.min(this.getVisibleWidth(), 120),
				height = Math.min(this.getVisibleHeight()-this.navigationLayoutHeight, 80),
                buttonWidth = this.weekChooserButton.getWidth(),
                left = this.weekChooserButton.getPageLeft() - ((width - buttonWidth)/2)
            ;
            this.weekMenu.setPageRect(Math.max(left, 0), top, width, height);
        }
        
		this.weekMenu.showModal();
	},

	showPrevYear : function () {
		this.year--;
        this.updateUI();
	},

	showNextYear : function () {
        if (this.year < this.endYear) {
		    this.year++;
            this.updateUI();
        }
	},

	showYear : function (yearNum) {
        if (yearNum < this.startYear || yearNum > this.endYear) return;
		this.year = yearNum;
		if (this.yearMenu) this.yearMenu.hide();
        this.updateUI();
	},

	showFiscalYear : function (yearNum) {
        var f = Date.getFiscalYear(yearNum, this.getFiscalCalendar());

		this.year = f.year;
        this.month = f.month;    
		if (this.yearMenu) this.yearMenu.hide();
        this.updateUI();
	},

    showFiscalYearMenu : function () {
        this.showYearMenu(true);
    },
    
	showYearMenu : function (fiscal) {
        var component = !fiscal ? this.yearChooserButton : this.fiscalYearChooserButton;
    
        var yearDiff = (this.endYear-this.startYear),
            colCount = Math.round(yearDiff/10) > 3 ? Math.round(yearDiff/10) : 3;
            
        var yearItems = [[]];
        for (var i = 0; i <= (this.endYear-this.startYear); i++) {
            var year = i+this.startYear;
            yearItems[yearItems.length-1].add({
                contents: year,
                eventPart: "showYear",
                eventId: year
            });
            if ((i+1)%colCount == 0) yearItems.add([]);
        }

        if (!this.yearMenu) {
			this.yearMenu = isc.YearChooser.newInstance({
                styleName:this.yearMenuStyle,
				top:this.getPageTop()+this.navigationLayoutHeight,
				width:Math.min(this.getVisibleWidth(), (40*colCount)),
				height:Math.min(this.getVisibleHeight()-this.navigationLayoutHeight, 80),
				items:yearItems,
				visibility:isc.Canvas.HIDDEN,
				baseButtonStyle:this.baseButtonStyle,
                dateChooser: this
			});
            // (autoDraw is true, so it is drawn, with visibility hidden at this point)
			//this.yearMenu.setPageLeft(this.getPageLeft() + ((this.width - this.yearMenu.width)/2));
            var left = component.getPageLeft() - ((this.yearMenu.getWidth() - component.getWidth()) /2);
            this.yearMenu.setPageLeft(Math.max(left, 0));

		} else {
            // L, T, W, H
            var top = this.getPageTop()+this.navigationLayoutHeight,
				width = Math.min(this.getVisibleWidth(), (40*colCount)),
				height = Math.min(this.getVisibleHeight()-this.navigationLayoutHeight, 80),
                buttonWidth = component.getWidth(),
                left = component.getPageLeft() - ((width - buttonWidth)/2)
            ;

            this.yearMenu.items = yearItems;
            this.yearMenu.setPageRect(Math.max(left,0), top, width, height);
        }

        var _fiscal = fiscal;
        this.yearMenu.showYearClick = function (element, id) {
            if (_fiscal) this.dateChooser.showFiscalYear(parseInt(id));
            else this.dateChooser.showYear(parseInt(id));
        }

		//XXX it'd be nice to hilite the current year somehow...
		this.yearMenu.showModal();
	},

	dateClick : function (year, month, day, selectNow) {
        var date = this.chosenDate = Date.createLogicalDate(year, month, day);
        // set this.month / this.year - this ensures we actually show the selected 
        // date if the user hits the today button while viewing another month
        
        var yearChanged = this.year != year;
        if (yearChanged) this.year = year;
        if (yearChanged || this.month != month) this.showMonth(month);
        
        this.month = month;
        this.year = year;
        
        if (this.showTimeItem) {
            // if we're showing the timeItem, combine it's value with the logicalDate created 
            // from the calendar
            var time = this.getTimeItem().getValue();
            date = this.chosenDate = Date.combineLogicalDateAndTime(date, time);
        }

        if (selectNow) this.dateGrid.selectDateCell(date);
        this.dataChanged();

    	if (window.dateClickCallback) {
			// if it's a string, normalize it to a function
			if (isc.isA.String(window.dateClickCallback)) {
                window.dateClickCallback = isc._makeFunction("date",window.dateClickCallback);
            }
			// and call it, passing the date
			window.dateClickCallback(date)
		}

        if (this.autoHide) this.hide();
		if (this.autoClose) this.close();

		return date;
	},
    
    // Observable dataChanged function (fired from dateClick)

    //> @method DateChooser.dataChanged()
    // Method to override or observe in order to be notified when a user picks a date value.
    // <P>
    // Has no default behavior (so no need to call Super).
    // <P>
    // Use +link{getData()} to get the current date value.
    // 
    // @visibility external
    //<
    dataChanged : function () {
    },
    
    //> @method DateChooser.cancelClick()
    // Fired when the user clicks the cancel button in this date chooser. Default implementation
    // clears the date chooser.
    // @visibility external
    //<
    
    cancelClick : function () {
        this.close();
    },
    
    //> @method DateChooser.todayClick()
    // Fired when the user clicks the Today button. Default implementation will select the current
    // date in the date chooser.
    // @visibility external
    //<
    
    todayClick : function () {
        var date = new Date();
        this.dateClick(date.getFullYear(), date.getMonth(), date.getDate(), true);
    },
    
    //> @method DateChooser.applyClick()
    // Fired when the user clicks the Today button. Default implementation will select the current
    // date in the date chooser.
    //<
    applyClick : function () {
        var date = this.chosenDate.duplicate();
        this.dateClick(date.getFullYear(), date.getMonth(), date.getDate(), true);
    },

    //> @method DateChooser.close()
    // Close the DateChooser.  
    //< 
    close : function () {
        this.hideClickMask();
        if (this.yearMenu && this.yearMenu.isVisible()) this.yearMenu.hide();
        if (this.monthMenu && this.monthMenu.isVisible()) this.monthMenu.hide();
        if (this.isDrawn()) this.clear();
    },

	dateFromIdClick : function (element, id) {
        var parts = id.split("_");
        if (parts.length != 3) return null;
        
        var year  = parseInt(parts[0]),
            month = parseInt(parts[1]),
            day   = parseInt(parts[2]);

        return this.dateClick(year, month, day);
    }

});
//!<Deferred




// For efficiency we want to re-use a single date-chooser widget in most cases.
// Add a class method for this
isc.DateChooser.addClassMethods({
    
    // getSharedDateChooser()   Simple method to return a standard date chooser.
    // Used by the DateItem
    getSharedDateChooser : function (properties) {

        if (!this._globalDC) {
        
            this._globalDC = this.create(properties, {

                _generated:true,
                // When re-using a DateChooser, we're almost certainly displaying it as a 
                // floating picker rather than an inline element. Apply the common options for 
                // a floating picker
                autoHide:true,
                showCancelButton:true,
                closeOnEscapeKeypress: true
                
            });
            
            return this._globalDC;
        }
        
        isc.addProperties(this._globalDC, properties);
        return this._globalDC;
    }
    
});

isc.ClassFactory.defineClass("WeekChooser", "ButtonTable");
isc.WeekChooser.addMethods({

	showWeekClick : function (element, id) {
        this.dateChooser.showWeek(parseInt(id));
    }

});

isc.ClassFactory.defineClass("MonthChooser", "ButtonTable");
isc.MonthChooser.addMethods({

	showMonthClick : function (element, id) {
        this.dateChooser.showMonth(parseInt(id));
    }

});

isc.ClassFactory.defineClass("YearChooser", "ButtonTable");
isc.YearChooser.addMethods({

	showYearClick : function (element, id) {
        this.dateChooser.showYear(parseInt(id));
    }

});
