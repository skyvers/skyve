/*
 * Isomorphic SmartClient
 * Version v9.1p_2014-03-26 (2014-03-26)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */




//> @class Calendar
// The Calendar component provides several different ways for a user to view and
// edit a set of events. Note that the <b>ISC_Calendar.js</b> module must be 
// loaded to make use of the Calendar class.
// <P>
// <b>CalendarEvents</b>
// <P>
// Events are represented as ordinary JavaScript Objects (see +link{CalendarEvent}).  
// The Calendar expects to be able to read and write a basic set of properties 
// on events: name, startDate, endDate, description, etc, which can be stored 
// under configurable property names (see eg +link{calendar.startDateField}).
// <P>
// Much like a +link{ListGrid} manages it's ListGridRecords, the Calendar can 
// either be passed an ordinary Array of CalendarEvents or can fetch data from a 
// DataSource.
// <P>
// If the calendar is bound to a DataSource, event changes by user action or by 
// calling methods will be saved to the DataSource.
// <P>
// <b>Navigation</b>
// <P>
// The calendar supports a number of views by default: +link{calendar.dayView,day},
// +link{calendar.weekView,week}, +link{calendar.monthView,month} and 
// +link{calendar.timelineView, timeline}.  The user can navigate using back and forward buttons or 
// via an attached +link{calendar.dateChooser,DateChooser}.
// <P>
// <b>Event Manipulation</b>
// <P>
// Events can be created by clicking directly onto one of the views, or via the 
// +link{calendar.addEventButton, Add Event} button.  In the day, week and timeline views, the user may 
// click and drag to create an event of a specific duration.
// <P>
// Creating an event via click or click and drag pops up the
// +link{calendar.eventDialog,EventDialog}, which provides a simple form for 
// quick event entry (for normal events, only the description is required by default - for 
// events that are shown in a +link{calendar.lanes, lane}, that field is also required).
// <P>
// A separate editor called the +link{calendar.eventEditor,EventEditor} provides 
// an interface for editing all possible properties of an event, including custom 
// properties.  The EventEditor is used whenever a pre-existing event is being 
// edited, and can also be invoked
// by the user wherever the simpler EventDialog appears.
// <P>
// Events can also be programmatically +link{calendar.addEvent,added},
// +link{calendar.removeEvent,removed}, or +link{calendar.updateEvent,updated}.  
//  
// @implements DataBoundComponent
// @treeLocation  Client Reference/Calendar
// @example simpleCalendar
// @visibility calendar
//<
isc.ClassFactory.defineClass("Calendar", "Canvas", "DataBoundComponent");

isc.Calendar.addProperties({

defaultWidth: "100%",
defaultHeight: "100%",

year:new Date().getFullYear(),  // full year number
month:new Date().getMonth(),    // 0-11

//> @attr calendar.chosenDate (Date : 'Today' : IRW)
// The date for which events are displayed in the day, week, and month tabs of 
// the calendar.  Default is today.
//
// @group date
// @visibility calendar
//<

//> @attr calendar.firstDayOfWeek  (Number : null : IRW)
// The numeric day (0-6) which the calendar should consider as the first day of the week - if
// unset, the default is taken from the current locale.
//
// @group date
// @visibility calendar
//<
//firstDayOfWeek:0,

// Styling
// ---------------------------------------------------------------------------------------

//> @attr calendar.baseStyle  (CSSStyleName : "calendar" : IRW)
// The base name for the CSS class applied to the grid cells of the day and week views
// of the calendar. This style will have "Dark", "Over", "Selected", or "Disabled"
// appended to it according to the state of the cell.
//
// @group appearance
// @visibility calendar
//<
baseStyle: "calendar",

//> @attr calendar.dayHeaderBaseStyle  (CSSStyleName : "calMonthDayHeader" : IRW)
// The base name for the CSS class applied to the day headers of the month view.
// This style will have "Dark", "Over", "Selected", or "Disabled"
// appended to it according to the state of the cell.
//
// @group appearance
// @visibility calendar
//<
dayHeaderBaseStyle: "calMonthDayHeader",

//> @attr calendar.dayBodyBaseStyle  (CSSStyleName : "calMonthDayBody" : IRW)
// The base name for the CSS class applied to the day body of the month view
// of the calendar. This style will have "Dark", "Over", "Selected", or "Disabled"
// appended to it according to the state of the cell.
//
// @group appearance
// @visibility calendar
//<
dayBodyBaseStyle: "calMonthDayBody",

//> @attr calendar.otherDayHeaderBaseStyle  (CSSStyleName : "calMonthDayHeader" : IRW)
// The base name for the CSS class applied to the day headers of the month view.
// This style will have "Dark", "Over", "Selected", or "Disabled"
// appended to it according to the state of the cell.
//
// @group appearance
// @visibility calendar
//<
otherDayHeaderBaseStyle: "calMonthOtherDayHeader",

//> @attr calendar.otherDayBodyBaseStyle  (CSSStyleName : "calMonthDayBody" : IRW)
// The base name for the CSS class applied to the day body of the month view
// of the calendar. This style will have "Dark", "Over", "Selected", or "Disabled"
// appended to it according to the state of the cell.
//
// @group appearance
// @visibility calendar
//<
otherDayBodyBaseStyle: "calMonthOtherDayBody",

//> @attr calendar.otherDayBlankStyle (CSSStyleName : "calMonthOtherDayBlank" : IR)
// The CSS style applied to both the header and body of days from other months in the
// +link{monthView, month view}, when +link{showOtherDays} is false.
//
// @group appearance
// @visibility calendar
//<
otherDayBlankStyle: "calMonthOtherDayBlank",

//> @attr calendar.minimumDayHeight (integer : 80 : IRW)
// In the +link{monthView, month view} when +link{showDayHeaders} is true, this is the minimum
// height applied to a day cell and its header combined.
// <P>
// If <code>showDayHeaders</code> is false, this attribute has no effect - the minimum height 
// of day cells is either an equal share of the available height, or the rendered height of the
// cell's HTML content, whichever is greater.  If the latter, a vertical scrollbar is shown.
//
// @group appearance
// @visibility calendar
//<
minimumDayHeight: 80,

//> @attr calendar.selectedCellStyle  (CSSStyleName : "calendarCellSelected" : IRW)
// The base name for the CSS class applied to a cell that is selected via a mouse drag.
//
// @group appearance
// @visibility calendar
//<
selectedCellStyle: "calendarCellSelected",

//> @attr calendar.eventWindowStyle  (CSSStyleName : "eventWindow" : IRW)
// The base name for the CSS class applied to event windows within calendars.
// This style will have "Header", "HeaderLabel", and "Body" appended to it, according to 
// which part of the event window is being styled. For example, to style the header, define
// a css class called 'eventWindowHeader'.
//  
// @group appearance
// @visibility calendar
//<
eventWindowStyle: "eventWindow",

calMonthEventLinkStyle: "calMonthEventLink",

// Workday properties
//---------------------------------------------------------------------------------------------

//> @attr calendar.workdayBaseStyle (CSSStyleName : "calendarWorkday" : IR)
// If +link{showWorkday} is set, this is the style used for cells that are within the workday, 
// as defined by +link{workdayStart} and +link{workdayEnd}, or by a date-specific range 
// provided in +link{getWorkdayStart} and +link{getWorkdayEnd} implementations.
//
// @group workday, appearance
// @visibility calendar
//<
workdayBaseStyle: "calendarWorkday",

//> @attr calendar.workdayStart (Time : "9:00am" : IR)
// When using +link{showWorkday}:true, <code>workdayStart</code> and <code>workdayEnd</code>
// specify the time of day when the workday starts and ends, specified as a
// String acceptable to +link{Time.parseInput()}.
// <P>
// Both start and end time must fall on a 30 minute increment (eg 9:30, but not 9:45).
// <P>
// The hours of the workday can be customized for particular dates by providing implementations
// of +link{getWorkdayStart} and +link{getWorkdayEnd}.
//
// @group workday, date
// @visibility calendar
//<
workdayStart: "9:00am",

//> @attr calendar.workdayEnd (Time : "5:00pm" : IR)
// @include calendar.workdayStart
//
// @group workday, date
// @visibility calendar
//<
workdayEnd: "5:00pm",

//> @attr calendar.showWorkday (Boolean : false : IR)
// If set, causes the calendar to use +link{workdayBaseStyle}
// for cells falling within the workday as defined by +link{workdayStart} and +link{workdayEnd},
// in both the +link{weekView} and +link{calendar.dayView,dayView}.
// <P>
// The hours of the workday can be customized for particular dates by providing implementations
// of +link{getWorkdayStart} and +link{getWorkdayEnd}.
//
// @group workday
// @visibility calendar
//<
showWorkday: false,

//> @attr calendar.workdays (Array : [1,2,3,4,5] : IR)
// Array of days that are considered workdays when +link{showWorkday} is true.
// <smartclient>Has no effect if +link{dateIsWorkday} is implemented.</smartclient>
//
// @group workday
// @visibility calendar
//<
workdays: [1, 2, 3, 4, 5],

//> @attr calendar.scrollToWorkday (Boolean : false : IR)
// If set, causes the +link{workdayStart,workday hours} to be sized to fill the available space
// in the day view and week view, and automatically scrolls these views to the start of the
// workday when the calendar is first displayed and whenever the user switches to a new day or
// week.
//
// @group workday
// @visibility calendar
//<
scrollToWorkday: false,

// return the event window title for the passed event in the passed view
getEventTitle : function (event, view) {
    var sTime = view.isTimelineView() ? null :
            isc.Time.toTime(event[this.startDateField], this.timeFormatter, true),
        eTitle = (sTime ? sTime + " " : "") + event[this.nameField]
    ;
    return eTitle;
},

// internal minutesPerRow - must divide into 60
minutesPerRow: 30,
getMinutesPerRow : function (view) {
    view = view || this.getSelectedView();
    return this.minutesPerRow;
},

// get the number or rows in an hour
getRowsPerHour : function (view) {
    return Math.floor(60 / this.getMinutesPerRow());
},

// return the number of pixels that the parameter minutes will occupy in the passed view
getMinutePixels : function (minutes, rowSize, view) {
    view = view || this.getSelectedView();
    if (view.isTimelineView()) {
        // for now, this will only be called when timeline granularity is set to 'hour'
        // rowHeight is actually rowWidth in this case.
        var hourWidth = rowSize != null ? rowSize : view.columnWidth;
        // divide hourWidth by 60 to get the width of each minute
        return Math.round((hourWidth / 60) * minutes);
    } else if (view.isDayView() || view.isWeekView()) {
        var hourHeight = (rowSize != null ? rowSize : view.getRowHeight(0)) * 
                this.getRowsPerHour(view);
        return Math.round((hourHeight / 60) * minutes);
    }
},

// return the rowNum that covers the passed date
getRowFromDate : function (view, date) {
    var minsPerRow = this.getMinutesPerRow(view),
        rowsPerHour = this.getRowsPerHour(view),
        minuteRows = Math.floor(date.getMinutes() / minsPerRow),
        extraRows = (date.getMinutes() % minsPerRow == 0 ? 0 : 1),
        // minsPerRow:15 (rowsPerHour:4), 6:48am gives: (6 * 4) + 3 + 1
        sRow = (date.getHours() * rowsPerHour) + minuteRows + extraRows
    ;
    return sRow;
},

//> @method calendar.scrollToTime()
// Scroll the calendar Day or Week views to the specified time.
// @param time (string) any parsable time-string
// @visibility calendar
//<
scrollToTime : function (time) {
    //TODO: should be passing "view" into this method
    var view = this.getSelectedView();
    time = isc.Time.parseInput(time);
    if (isc.isA.Date(time)) {
        var sRow = this.getRowFromDate(view, time);
        var sRowTop = view.getRowHeight(null, 0) * sRow;
        view.body.scrollTo(0, sRowTop);
        view.redraw();
   }
},

// Fields on Event Records
// ---------------------------------------------------------------------------------------

//> @attr calendar.nameField  (String : "name" : IR)
// The name of the name field in a +link{CalendarEvent}.
// 
// @group calendarEvent
// @visibility calendar
// @see CalendarEvent
//<
nameField: "name",

//> @attr calendar.descriptionField  (String : "description" : IR)
// The name of the description field in a +link{CalendarEvent}.
//
// @group calendarEvent
// @visibility calendar
//<
descriptionField: "description",

//> @attr calendar.startDateField  (String : "startDate" : IR)
// The name of the start date field in a +link{CalendarEvent}.
//
// @group calendarEvent
// @visibility calendar
// @see CalendarEvent
//<
startDateField: "startDate",

//> @attr calendar.endDateField  (String : "endDate" : IR)
// The name of the end date field in a +link{CalendarEvent}.
//
// @group calendarEvent
// @visibility calendar
// @see CalendarEvent
//<
endDateField: "endDate",

//> @attr calendar.leadingDateField  (String : "leadingDate" : IR)
// The name of the leading date field for each event.  When this attribute and 
// +link{trailingDateField} are present in the data, a line extends out from the event showing the 
// extent of the leading and trailing dates - useful for visualizing a pipeline of events 
// where some can be moved a certain amount without affecting others.
// 
// @group calendarEvent
// @visibility calendar
// @see CalendarEvent
//<
leadingDateField: "leadingDate",

//> @attr calendar.trailingDateField  (String : "trailingDate" : IR)
// The name of the trailing date field for each event.  When this attribute and 
// +link{leadingDateField} are present in the data, a line extends out from the event showing 
// the extent of the leading and trailing dates - useful for visualizing a pipeline of events 
// where some can be moved a certain amount without affecting others.
// 
// @group calendarEvent
// @visibility calendar
// @see CalendarEvent
//<
trailingDateField: "trailingDate",


labelColumnWidth: 60,

// adds space around events so that they sit within their lane, not at the edges of it
timelineEventPadding: 0,

//> @attr calendar.eventDragGap  (Number : 10 : IRW)
// The number of pixels to leave to the right of events so overlapping events can still be 
// added using the mouse.
//
// @visibility external
//<
eventDragGap: 10,

//> @attr calendar.laneNameField  (String : "lane" : IR)
// The name of the field which will determine the +link{Calendar.lanes, lane} in which this 
// event will be displayed in +link{Timeline}s and in the +link{dayView, day view}, if 
// +link{showDayLanes} is true.
//
// @group calendarEvent
// @visibility external
// @see CalendarEvent
//<
laneNameField: "lane",

//> @attr calendar.eventWindowStyleField (String : "eventWindowStyle" : IR)
// The name of the field used to override +link{calendar.eventWindowStyle} for an individual
// +link{CalendarEvent}.  See +link{calendarEvent.eventWindowStyle}.
//
// @group calendarEvent, appearance
// @visibility calendar
//<
eventWindowStyleField: "eventWindowStyle",

//> @attr calendar.canEditField  (String : "canEdit" : IR)
// Name of the field on each +link{CalendarEvent} that determines editability.
//
// @group calendarEvent
// @visibility calendar
// @see CalendarEvent
//<
canEditField: "canEdit",

//> @attr calendar.canEditLaneField  (String : "canEditLane" : IR)
// Name of the field on each +link{CalendarEvent} that determines whether that event can be 
// moved between lanes.
//
// @group calendarEvent
// @see CalendarEvent
// @visibility calendar
//<
canEditLaneField: "canEditLane",

//> @attr calendar.canRemoveField  (String : "canRemove" : IR)
// Name of the field on each +link{CalendarEvent} that determines whether an event shows a
// remove button.
//
// @group calendarEvent
// @visibility calendar
// @see CalendarEvent
//<
canRemoveField: "canRemove",

//> @attr calendar.canDragEventField  (String : "canEdit" : IR)
// Name of the field on each +link{CalendarEvent} that determines dragability.
//
// @group calendarEvent
// @visibility calendar
// @see CalendarEvent
//<
canDragEventField: "canDrag",

//> @attr calendar.weekEventBorderOverlap (Boolean : false : IR)
// Augments the width of week event windows slightly to avoid duplicate adjacent borders
// between events.
//
// @group appearance
// @visibility calendar
//<
weekEventBorderOverlap: false,

//> @attr calendar.headerLevels (Array of HeaderLevel : null : IRW)
// Configures the levels of +link{HeaderLevel, headers} shown above the event area, and 
// their time units.
// <P>
// Header levels are provided from the top down, so the first header level should be the largest 
// time unit and the last one the smallest.  The smallest is then used for the actual 
// field-headers.
// @setter setHeaderLevels()
// @visibility external
//<

//> @method calendar.setHeaderLevels()
// For +link{Timeline}s, configures the levels of +link{HeaderLevel, headers} shown above the 
// event area, and their time units, after initialization.
// @param headerLevels (Array of HeaderLevel) the array of HeaderLevels to set
// @visibility external
//<
setHeaderLevels : function (headerLevels) {
    this.headerLevels = headerLevels;
    if (this.timelineView) this.timelineView.rebuild(true);
},

// Event Editing
// ---------------------------------------------------------------------------------------

//> @attr calendar.eventSnapGap (Integer : 20 : IR)
// In +link{dayView, day} and +link{weekView, week} views, determines the number of vertical 
// pixels by which an event can be moved or resized when dragging.  The default of 20px means 
// that snapping occurs to each row border, since the default height of each 
// +link{calendar.rowHeight, row} in those views is also 20px.
// <P>
// For timelines, this attribute affects the number of horizontal pixels used for drag-snapping.
// Since the default width for +link{headerLevels} is 60px, the default eventSnapGap of 20px
// means that each column is split into 20 minute sections, assuming that the 
// +link{timelineGranularity} is "hour".
//
// @group editing
// @visibility external
//<
eventSnapGap: 20,

//> @attr calendar.showQuickEventDialog (Boolean : true : IR)
// Determines whether the quick event dialog is displayed when a time is clicked. If this is
// false, the full event editor is displayed.
//
// @group editing
// @visibility calendar
//<
showQuickEventDialog: true,

//> @attr calendar.eventEditorFields (Array of FormItem : see below : IR)
// The set of fields for the +link{calendar.eventEditor, event editor}.
// <p>
// The default set of fields are: 
// <pre>
//    {name: "startHours", title: "From",      editorType: "SelectItem", type: "integer", width: 60},
//    {name: "startMinutes", showTitle: false, editorType: "SelectItem", type: "integer", width: 60},
//    {name: "startAMPM", showTitle: false, type: "select", width: 60},
//    {name: "invalidDate", type: "blurb", colSpan: 4, visible: false}
//    {name: "endHours", title: "To",        editorType: "SelectItem", type: "integer", width: 60},
//    {name: "endMinutes", showTitle: false, editorType: "SelectItem", type: "integer", width: 60},
//    {name: "endAMPM", showTitle: false, type: "select", width: 60},
//    {name: "name", title: "Name", type: "text", colSpan: 4},
//    {name: "description", title: "Description", type: "textArea", colSpan: 4, height: 50}
// </pre>
// See the Customized Binding example below for more information on altering default datasource 
// fields within forms.
// 
// @group editing
// @example customCalendar
// @example validationFieldBinding
// @visibility calendar
//<

//> @attr calendar.eventDialogFields (Array of FormItem : see below : IR)
// The set of fields for the +link{calendar.eventDialog, event dialog}. 
// <p>
// The default set of fields are: 
// <pre>
//    {name: "name", title: "Event Name", type: nameType, width: 250 },
//    {name: "save", title: "Save Event", editorType: "SubmitItem", endRow: false},
//    {name: "details", title: "Edit Details", type: "button", startRow: false}
// </pre>
// See the Customized Binding example below for more information on altering default datasource 
// fields within forms.
//
// @group editing
// @example customCalendar
// @example validationFieldBinding
// @visibility calendar
//<

// Allowed operations
// ---------------------------------------------------------------------------------------

//> @groupDef allowedOperations
// 
// @title Allowed Operations
// @visibility external
//<

//> @attr calendar.canCreateEvents (Boolean : true : IR)
// If true, users can create new events.
//
// @group allowedOperations
// @visibility calendar
//<
canCreateEvents: true,

//> @attr calendar.canEditEvents (Boolean : true : IR)
// If true, users can edit existing events.
//
// @group allowedOperations
// @visibility calendar
//<
canEditEvents: true, 

//> @attr calendar.canDeleteEvents (Boolean : null : IR)
// If true, users can delete existing events. Defaults to +link{calendar.canEditEvents}.
//
// @group allowedOperations
// @visibility calendar
// @deprecated in favor of +link{calendar.canRemoveEvents}
//<
//canDeleteEvents: true,

//> @attr calendar.canRemoveEvents (Boolean : true : IR)
// If true, users can remove existing events. Defaults to +link{calendar.canEditEvents}.
//
// @group allowedOperations
// @visibility calendar
//<
canRemoveEvents: true,

//> @attr calendar.canDragEvents (Boolean : true : IR)
// If true, users can drag-reposition existing events. 
//
// @group allowedOperations
// @visibility calendar
//<
canDragEvents: true,

// Show / Hide parts of the interface
// ---------------------------------------------------------------------------------------

//> @attr calendar.showDateChooser (Boolean : true : IR)
// Determines whether the +link{calendar.dateChooser,dateChooser} is displayed.
//
// @group visibility
// @visibility calendar
//<
showDateChooser: false, 

//> @attr calendar.disableWeekends (Boolean : true : IR)
// If set, weekend days appear in disabled style and events cannot be created on weekends.
// Which days are considered weekends is controlled by +link{Date.weekendDays}.
//
// @group visibility 
// @visibility calendar
//<
disableWeekends: true,

//> @attr calendar.showWeekends (Boolean : true : IR)
// Suppresses the display of weekend days in the week and month views, and disallows the
// creation of events on weekends.  Which days are considered weekends is controlled by
// +link{Date.weekendDays}.
//
// @group visibility
// @visibility calendar
//<
showWeekends: true,

//> @attr calendar.showDayHeaders (Boolean : true : IR)
// If true, the default, show a header cell for each day cell in the 
// +link{monthView, month view}, with both cells having a minimum combined height of 
// +link{minimumDayHeight}.  If false, the header cells will not be shown, and the value 
// of +link{minimumDayHeight} is ignored.  This causes the available vertical space in month
// views to be shared equally between day cells, such that no vertical scrollbar is required
// unless the HTML in the cells renders them taller than will fit.
//
// @group visibility
// @visibility calendar
//<
showDayHeaders: true,

//> @attr calendar.showOtherDays (Boolean : true : IR)
// If set to true, in the +link{monthView, month view}, days that fall in an adjacent month are
// still shown with a header and body area, and are interactive.  Otherwise days from other 
// months are rendered in the +link{otherDayBlankStyle} and are non-interactive.
//
// @group visibility
// @visibility calendar
//<
showOtherDays: true,

// Overlapping event placement
// ---------------------------------------------------------------------------------------

//> @attr calendar.eventAutoArrange (Boolean : true : IR)
// If set to true, enables the auto-arrangement of events that share time in the calendar.  The
// default is true.
//
// @group calendarEvent
// @visibility calendar
//<
eventAutoArrange: true,

//> @attr calendar.eventOverlap (Boolean : true : IR)
// When +link{eventAutoArrange} is true, setting eventOverlap to true causes events that 
// share timeslots to overlap each other by a percentage of their width, specified by 
// +link{eventOverlapPercent}.  The default is true.
//
// @group calendarEvent
// @visibility calendar
//<
eventOverlap: true,

//> @attr calendar.eventOverlapPercent (number : 10 : IR)
// The size of the overlap, presented as a percentage of the width of events sharing timeslots.
//
// @group calendarEvent
// @visibility calendar
//<
eventOverlapPercent: 10,

//> @attr calendar.eventOverlapIdenticalStartTimes (Boolean : false : IR)
// When set to true, events that start at the same time will not overlap each other to prevent 
// events having their close button hidden.
//
// @group calendarEvent
// @visibility calendar
//<

// AutoChildren
// ---------------------------------------------------------------------------------------

//> @attr calendar.mainView (AutoChild TabSet : null : R)
// +link{TabSet} for managing calendar views when multiple views are available (eg,
// +link{dayView, day} and +link{monthView, month}).
//
// @visibility calendar
//<

//> @attr calendar.dayView (AutoChild ListGrid : null : R)
// +link{ListGrid} used to display events that pertain to a given day.
//
// @visibility calendar
//<

//> @attr calendar.weekView (AutoChild ListGrid : null : R)
// +link{ListGrid} used to display events that pertain to a given week.
//
// @visibility calendar
//<

//> @attr calendar.monthView (AutoChild ListGrid : null : R)
// +link{ListGrid} used to display events that pertain to a given month.
//
// @visibility calendar
//<


//> @attr calendar.dateChooser (AutoChild DateChooser : null : R)
// +link{DateChooser} used to select the date for which events will be displayed.
//
// @visibility calendar
//<


// CalendarEvent
// ---------------------------------------------------------------------------------------

//> @object CalendarEvent
// A type of +link{Record} which represents an event to occur at a specific time, displayed
// within the calendar.
//
// @group data
// @treeLocation Client Reference/Calendar
// @visibility calendar
//<

//> @attr calendarEvent.startDate (Date : null : IRW)
// Date object which represents the start date of a +link{CalendarEvent}.
// The name of this field within the CalendarEvent can be changed via
// +link{Calendar.startDateField}
//
// @visibility calendar
//<

//> @attr calendarEvent.endDate (Date : null : IRW)
// Date object which represents the end date of a +link{CalendarEvent}
// The name of this field within the CalendarEvent can be changed via
// +link{Calendar.endDateField}
//
// @visibility calendar
//<

//> @attr calendarEvent.name (String : null : IRW)
// String which represents the name of a +link{CalendarEvent}
// The name of this field within the CalendarEvent can be changed via
// +link{Calendar.nameField}
//
// @visibility calendar
//<

//> @attr calendarEvent.description (String : null : IRW)
// String which represents the description of a +link{CalendarEvent}
// The name of this field within the CalendarEvent can be changed via
// +link{Calendar.descriptionField}
//
// @visibility calendar
//<

//> @attr calendarEvent.canEdit (String : null : IRW)
// Optional boolean value controlling the editability of this particular calendarEvent.
//  The name of this field within the CalendarEvent can be changed via
//  +link{calendar.canEditField}.
//
// @visibility calendar
//<

//> @attr calendarEvent.canEditLane (String : null : IRW)
// Boolean indicating whether this event can be moved between lanes.  Can also be set at the
// +link{calendar.canEditLane, calendar level}.
// <P>
// The name of this field within the CalendarEvent can be changed via 
// +link{calendar.canEditLaneField}.
//
// @visibility calendar
//<

//> @attr calendarEvent.backgroundColor (String : null : IRW)
// An optional background color for this event's window.
//
// @visibility calendar
//<

//> @attr calendarEvent.textColor (String : null : IRW)
// An optional text color for this event's window.
//
// @visibility calendar
//<

//> @attr calendarEvent.headerBackgroundColor (String : null : IRW)
// An optional background color for this event's window-header.
//
// @visibility internal
//<

//> @attr calendarEvent.headerTextColor (String : null : IRW)
// An optional text color for this event's window-header.
//
// @visibility internal
//<

//> @attr calendarEvent.eventWindowStyle (CSSStyleName : null : IR)
// CSS style series to use for the draggable event window that represents this event.  If
// specified, overrides +link{calendar.eventWindowStyle} for this specific event.
// <P>
// The name of this field within the CalendarEvent can be changed via
// +link{Calendar.eventWindowStyleField}
//
// @visibility calendar
//<

//> @attr calendarEvent.lane (String : null : IRW)
// When in Timeline mode, or when +link{calendar.showDayLanes} is true, a string that 
// represents the name of the +link{calendar.lanes, lane} this +link{CalendarEvent} should 
// sit in.  The name of this field within the CalendarEvent can be changed via
// +link{Calendar.laneNameField}.
//
// @visibility calendar
//<

//> @attr calendar.alternateLaneStyles (Boolean : null : IRW)
// When showing a +link{timelineView, Timeline}, or a +link{dayView, day view} when 
// +link{showDayLanes} is true, whether to make lane boundaries more obvious by showing
// alternate lanes in a different color.
//
// @visibility calendar
//<
//alternateLanesStyles: false,

//> @attr calendar.alternateLaneFrequency (number : 1 : IRW)
// When +link{alternateLaneStyles} is true, for +link{Timeline}s and +link{dayView, day view} 
// with +link{showDayLanes} set, the number of consecutive lanes to draw in the same style 
// before alternating.
// @group cellStyling
// @visibility internal
//<
alternateLaneFrequency: 1,

//> @method calendar.getWorkdayStart()
// Returns the start of the working day on the passed date.  By default, this method returns
// the value of +link{calendar.workdayStart, workdayStart}.
// @param date (Date) a Date instance
// @param (laneName) (String) the name of the relevant lane - only passed for dayView with 
//                            showDayLanes: true
// @return (String) any parsable time-string
// @visibility calendar
//<
getWorkdayStart : function (date, lane) {
    return this.workdayStart;
},

//> @method calendar.getWorkdayEnd()
// Returns the end of the working day on the passed date.  By default, this method returns
// the value of +link{calendar.workdayEnd, workdayEnd}.
// @param date (Date) a Date instance
// @param (laneName) (String) the name of the relevant lane - only passed for dayView with 
//                            showDayLanes: true
// @return (String) any parsable time-string
// @visibility calendar
//<
getWorkdayEnd : function (date, laneName) {
    return this.workdayEnd;
},

//> @method calendar.getVisibleStartDate()
// Returns the first visible date in the currently selected calendar view.
// @return (Date) first visible date
// @visibility calendar
//<
getVisibleStartDate : function () {
    var selectedView = this.getSelectedView();
    if (!selectedView || isc.isAn.emptyString(selectedView)) return null;

    if (selectedView.isDayView()) return this.chosenDate;

    if (selectedView.isMonthView()) return this.getCellDate(0,0);
    return this.getCellDate(0,1);
},

//> @method calendar.getVisibleEndDate()
// Returns the last visible date in the currently selected calendar view.
// @return (Date) last visible date
// @visibility calendar
//<
getVisibleEndDate : function () {
    var selectedView = this.getSelectedView();

    if (selectedView.isDayView()) return this.chosenDate;

    return this.getCellDate(selectedView.getData().length-1, selectedView.getFields().length-1);
},

//> @method calendar.getPeriodStartDate()
// Returns the start of the selected week or month depending on the current calendar view.  
// For the month view, and for the week view when not showing weekends, this will often be a 
// different date than that returned by +link{calendar.getVisibleStartDate}.
// @return (Date) period start date
// @visibility calendar
//<
getPeriodStartDate : function () {
    var selectedView = this.getSelectedView(),
        date = this.chosenDate
    ;

    if (selectedView.isDayView()) {
        return date;
    } else if (selectedView.isWeekView()) {
        return isc.DateUtil.getStartOf(date, isc.DateUtil.getTimeUnitKey("week"));
    } else if (selectedView.isMonthView()) {
        return isc.DateUtil.getStartOf(date, isc.DateUtil.getTimeUnitKey("month"));
    }
},

//> @method calendar.getPeriodEndDate()
// Returns the end of the selected week or month depending on the current calendar view.  
// For the month view, and for the week view when not showing weekends, this will often be a 
// different date than that returned by +link{calendar.getVisibleEndDate}.
// @return (Date) period end date
// @visibility calendar
//<
getPeriodEndDate : function () {
    var selectedView = this.getSelectedView(),
        date = this.chosenDate
    ;
    
    if (selectedView.isDayView()) {
        return date;
    } else if (selectedView.isWeekView()) {
        return isc.DateUtil.getEndOf(date, isc.DateUtil.getTimeUnitKey("week"));
    } else if (selectedView.isMonthView()) {
        return isc.DateUtil.getEndOf(date, isc.DateUtil.getTimeUnitKey("month"));
    }
},

// Data & Fetching
// ---------------------------------------------------------------------------------------

//> @attr calendar.data (Array[] of CalendarEvent : null : IRW)
// A List of CalendarEvent objects, specifying the data to be used to populate the
// calendar.  
// <p>
// This property will typically not be explicitly specified for databound Calendars, where
// the data is returned from the server via databound component methods such as
// +link{fetchData()}. In this case the data objects will be set to a 
// +link{class:ResultSet,resultSet} rather than a simple array.
//
// @group data
// @see CalendarEvent
// @setter setData()
// @visibility calendar
//<

//> @method calendar.fetchData()
// @include dataBoundComponent.fetchData()
// @group dataBoundComponentMethods
// @visibility calendar
// @example databoundFetch
//<

//> @attr calendar.autoFetchData (boolean : false : IR)
// @include dataBoundComponent.autoFetchData
// @group databinding
// @visibility calendar
// @example fetchOperation
//<

//> @attr calendar.autoFetchTextMatchStyle (TextMatchStyle : null : IR)
// @include dataBoundComponent.autoFetchTextMatchStyle
// @group databinding
// @visibility external
//<

//> @method calendar.filterData()
// @include dataBoundComponent.filterData()
// @group dataBoundComponentMethods
// @visibility external
//<

//> @attr Calendar.initialCriteria (Criteria : null :IR)
// @include dataBoundComponent.initialCriteria
// @visibility calendar
//<

//> @attr calendar.showDetailFields (Boolean : true : IR)
// @include dataBoundComponent.showDetailFields
// @group databinding
//<

//> @attr calendar.dataFetchMode (FetchMode : "paged" : IRW)
// @include dataBoundComponent.dataFetchMode
//<

//> @type CalendarFetchMode 
// Granularity at which CalendarEvents are fetched from the server.
//
// @value "all" no criteria is sent to the server, so all events will be fetched
// @value "month" events are fetched one month at a time
// @value "week" events are fetch one week at a time.  Month view may not be used
// @value "day" events are fetched one day at a time.  Only day view may be used
// @visibility internal
//<

//> @attr calendar.fetchMode (CalendarFetchMode : "month" : IR)
// The granularity at which events are fetched.
// <P>
// With any setting other than "all", whenever +link{fetchData} is called the calendar will add
// criteria requesting a range of either one month, one week or one day of events depending on
// this setting.  Subsequently, additional fetch requests will be sent automatically as the user
// navigates the calendar.
// <P>
// If +link{calendar.criteriaFormat} is "simple", the criteria will be added as two fields
// "firstVisibleDay" and "lastVisibleDay" with values of type Date.  Note that the these
// fieldNames intentionally differ from +link{calendarEvent.startDate} and
// +link{calendarEvent.endDate} because adding values for <code>startDate</code> and
// <code>endDate</code> to simple criteria would match only events on those exact dates.
// <P>
// If the <code>criteriaFormat</code> is "advanced", the criteria passed to
// <code>fetchData</code> will be converted to +link{AdvancedCriteria} if needed, then criteria
// will be added that would select the appropriate records from any DataSource that supports
// searching with AdvancedCriteria.  That is, the criteria will express:
// <pre>
//   calendarEvent.endDate => firstVisibleDay AND
//   calendarEvent.startDate <= lastVisibleDay
// </pre>
// 
// @visibility internal
//<

//> @type CriteriaFormat
// @value "simple" criteria represents as simple key-value pairs - see +link{Criteria}
// @value "advanced" criteria represents as type-operator-value triplets, potentially nested to
//                   form complex queries.  See +link{AdvancedCriteria}.
// @visibility internal
//<

//> @method calendar.criteriaFormat (CriteriaFormat : "advanced" : IR)
// When adding criteria to select events for the currently visible date range, should we use
// simple +link{Criteria} or +link{AdvancedCriteria}?  See +link{fetchMode}.
// @visibility internal
//<

// TimelineView
// ---------------------------------------------------------------------------------------

//> @attr calendar.showTimelineView (Boolean : false : IRW)
// If set to true, show the +link{timelineView, Timeline view}.
// @visibility external
//<
showTimelineView: false,

//> @attr calendar.timelineView (AutoChild ListGrid : null : R)
// +link{ListGrid} used to display events in lanes in a horizontal +link{Timeline} view.
//
// @visibility calendar
//<

// now works and is the default for all views
renderEventsOnDemand: true,

//> @attr calendar.timelineGranularity (TimeUnit : "day" : IR)
// The granularity with which the timelineView will display events.  Possible values are
// those available in the built-in +link{type:TimeUnit, TimeUnit} type.
// @visibility external
//<
timelineGranularity: "day",

//> @attr calendar.timelineUnitsPerColumn (int : 1 : IR)
// How many units of +link{timelineGranularity} each cell represents.
// @visibility external
//<
timelineUnitsPerColumn: 1,

//> @attr calendar.canResizeTimelineEvents (Boolean : false : IR)
// Can +link{Timeline} events be stretched by their left and right edges?
// @visibility external
//<
canResizeTimelineEvents: false,

//> @attr calendar.canEditLane (boolean : null : IR)
// Can we edit the lane of the event, specified by the +link{laneNameField}?
// If so, the event can be dragged to a different +link{lanes, lane} and, when it's editor is
// shown, an additional drop-down widget is provided allowing the user to select a different
// lane.
// <P>
// In either case, the event's +link{laneNameField} is updated automatically.
// <P>
// This setting can be overridden on each +link{CalendarEvent.canEditLane, event}.
//
// @visibility external
//<

//> @attr calendar.canReorderLanes (Boolean : null : IR)
// If true, lanes can be reordered by dragging them with the mouse.
// @visibility external
//<

//> @attr calendar.startDate (Date : null : IR)
// The start date of the calendar +link{class:Timeline, timeline view}.  Has no effect in 
// other views.  If not specified, defaults to a timeline starting from the beginning 
// of the current +link{Calendar.timelineGranularity, timelineGranularity} and spanning
// +link{Calendar.defaultTimelineColumnSpan, a default of 20} columns of that granularity. 
// <P>
// To set different start and +link{calendar.endDate, end} dates after initial draw,
// see +link{calendar.setTimelineRange, setTimelineRange}.
// <P>
// Note that this attribute may be automatically altered if showing 
// +link{calendar.headerLevels, header-levels}, to fit to header boundaries.
// @visibility external
//<

//> @attr calendar.defaultTimelineColumnSpan (number : 20 : IR)
// The number of columns of the +link{Calendar.timelineGranularity, timelineGranularity} to
// give the timeline by default if no +link{calendar.endDate, endDate} is provided.  The 
// default is 20.
// @visibility external
//<
defaultTimelineColumnSpan: 20,

//> @attr calendar.columnsPerPage (number : null : IR)
// When using the Next and Previous arrows to scroll a Timeline, this is the number of columns 
// of the +link{Calendar.timelineGranularity, timelineGranularity} to scroll by.  With the
// default value of null, the Timeline will scroll by its current length.
// @visibility external
//<

//> @attr calendar.endDate (Date : null : IR)
// The end date of the calendar timeline view.  Has no effect in other views.
// <P>
// To set different +link{calendar.startDate, start} and end dates after initial draw,
// see +link{calendar.setTimelineRange, setTimelineRange}.
// <P>
// Note that this attribute may be automatically altered if showing 
// +link{calendar.headerLevels, header-levels}, to fit to header boundaries.
// @visibility external
//<

//> @object HeaderLevel
// Defines one level of headers shown above the event area in a +link{Timeline}.
// @treeLocation  Client Reference/Calendar
// @visibility external
//<

//> @attr headerLevel.unit (TimeUnit : null : IR)
// Unit of time shown at this level of header.
// @visibility external
//<

//> @attr headerLevel.headerWidth (integer : null : IR)
// If set, the width for each of the spans in this headerLevel.  Note that this setting only 
// has an effect on the innermost headerLevel.
// @visibility external
//<

//> @attr headerLevel.titles (Array of String : null : IR)
// Optional sparse array of titles for the spans on this headerLevel.  If a given span in this
// headerLevel has a corresponding entry in this array, it will be used as the span's title.
// <P> 
// If not specified, default titles are generated (eg "Q1" for unit "quarter") and then passed
// into the +link{headerLevel.titleFormatter, formatter function}, if one has been installed,
// for further customization.
//
// @visibility external
//<

//> @method headerLevel.titleFormatter()
// An optional function for providing formatted HTML for the title of a given span in this 
// HeaderLevel.  If unset, Timelines use the +link{HeaderLevel.titles, titles array}, if one is
// set, or generate default titles based on the unit-type and date-range.
// <P>
// Note that this method will not run for spans in this headerLevel that have a non-null entry 
// in the +link{HeaderLevel.titles, titles} array.
// 
// @param headerLevel (HeaderLevel) a reference to this headerLevel
// @param startDate (Date) the start of the date-range covered by this span in this level
// @param endDate (Date) the end of the date-range covered by this span in this level - may be 
//                       null
// @param defaultValue (String) the default title as generated by the Timeline
// @param viewer (Calendar) a reference to the Calendar or Timeline
// @return (String) The formatted title for the values passed in
// @visibility external
//<

//> @attr calendar.weekPrefix (String : "Week" : IR)
// The text to appear before the week number in the title of +link{TimeUnit, week-based} 
// +link{HeaderLevel}s when this calendar is showing a timeline.
// @group i18nMessages
// @visibility external
//<
weekPrefix: "Week",

//> @type DateEditingStyle
// The type of date/time editing style to use when editing an event.
//
// @value "date" allows editing of the logical start and end dates of the event
// @value "datetime" allows editing of both date and time
// @value "time" allows editing of the time portion of the event only
// @visibility external
//<


//> @attr calendar.dateEditingStyle (DateEditingStyle : null : IR)
// Indicates the type of controls to use in event-windows.  Valid values are those in the 
// +link{type:DateEditingStyle, DateEditingStyle} type.
// <P>
// If unset, the editing style will be set to the field-type on the DataSource, if there is one.
// If there's no DataSource, it will be set to "date" if the 
// +link{calendar.timelineGranularity, granularity} is "day" or larger and "time" if granularity 
// is "minute" or smaller, otherwise "datetime".
// @visibility external
//<


//> @object Lane
// Lane shown in a +link{Timeline} view, or in a +link{calendar.dayView,day view} when 
// +link{calendar.showDayLanes,showDayLanes} is true.  Each lane is a row or column, respectively, that can contain a
// set of +link{CalendarEvent}s.  CalendarEvents are placed in lanes by matching the
// +link{Lane.name} property to the value of the +link{calendar.laneNameField} property on the
// CalendarEvent.
// <P>
// Lanes are typically used to show tasks assigned to different people, broadcasts planned for
// different channels, and similar displays.
// 
// @treeLocation  Client Reference/Calendar
// @visibility external
//<

//> @attr lane.name (String : null : IR)
// To determine whether a CalendarEvent should be placed in this lane, the value of this 
// attribute is compared with the +link{calendar.laneNameField} property on the CalendarEvent.
//
// @visibility external
//<

//> @attr lane.height (Number : null : IR)
// In +link{Timeline}s, the height of this Lane's row.  Has no effect when set on a Lane being
// displayed in a +link{calendar.dayView,dayView} as a result of +link{calendar.showDayLanes,showDayLanes} being set.
//
// @visibility external
//<

//> @attr lane.width (Number : null : IR)
// When set on a Lane being displayed in a +link{calendar.dayView,dayView} as a result of +link{calendar.showDayLanes,showDayLanes} 
// being set, dictates the width of the Lane's column.  Has no effect in +link{Timeline}s.
//
// @visibility external
//<

//> @attr lane.title (HTMLString : null : IR)
// Title to show for this lane.
//
// @visibility external
//<

//> @attr calendar.lanes (Array of Lane : null : IRW)
// An array of +link{Lane} definitions that represent the rows of the +link{timelineView}, or
// the columns of the +link{calendar.dayView,dayView} if +link{calendar.showDayLanes,showDayLanes} is true.
// @visibility external
// @setter setLanes()
//<

//> @method calendar.setLanes()
// Sets the +link{calendar.lanes, lanes} in the current calendar view.  Only has an effect
// in +link{timelineView, timeline views}, and in +link{dayView, day views} when 
// +link{showDayLanes} is true.
//
// @param lanes (Array of Lane) array of lanes to display
//
// @visibility external
//<
setLanes : function (lanes) {
    // bail if nothing passed
    if (!lanes) { return; }
    // store lanes but don't call through if not yet draw()n
    this.lanes = lanes;
    if (this.timelineView) { this.timelineView.setLanes(this.lanes); }
    if (this.showDayLanes && this.dayView) { this.dayView.setLanes(this.lanes); }
},

//> @method calendar.addLane()
// Adds a new +link{object:Lane} to the calendar +link{timelineView}, or to the 
// +link{dayView} if +link{showDayLanes} is true.
//
// @param lane (Lane) a new Lane object to add to the view
//
// @visibility external
//<
addLane : function (lane, index) {
    var view;
    if (this.timelineViewSelected()) { view = this.timelineView; }
    else if (this.dayViewSelected() && this.showDayLanes) { view = this.dayView; }
    if (!view) { return; }

    if (!this.lanes) this.lanes = [];
    if (index == null) index = this.lanes.length;
    this.lanes.add(lane, index);
    view.setLanes(this.lanes);
},

//> @method calendar.removeLane()
// Removes a lane from the calendar in +link{timelineView}, or in +link{dayView} if 
// +link{showDayLanes} is true.
// <P>
// Accepts either a +link{object:Lane, Lane object} or a string that represents the 
// +link{Lane.name, name} of a lane.
//
// @param lane (Lane | String) either the actual Lane object or the name of the lane to remove
//
// @visibility external
//<
removeLane : function (lane) {
    var view;
    if (this.timelineViewSelected()) view = this.timelineView;
    else if (this.dayViewSelected() && this.showDayLanes) view = this.dayView;
    if (!view || !this.lanes) return;

    if (isc.isA.String(lane)) lane = this.lanes.find("name", lane);
    if (lane) {
        this.lanes.remove(lane);
        view.setLanes(this.lanes);
    }
},

//> @attr calendar.laneFields (Array of ListGridField : null : IR)
// Field definitions for the frozen area of the +link{timelineView}, which shows data about the
// timeline +link{lanes}.  Each field shows one attribute of the objects provided as
// +link{calendar.lanes}.
// @visibility external
//<

//> @attr calendar.showDayLanes (Boolean : null : IR)
// If set to true, the +link{dayView, day view} uses +link{calendar.lanes} to render multiple
// vertical "lanes" within the day, very much like a vertical +link{Timeline}.
// <P>
// Day lanes are useful for showing events for various entities on the same day - agendas for 
// various staff members, for example, or delivery schedules for a fleet of trucks.
// <P>
// Each day lane is self-contained, showing in a column with a header and individual events 
// are placed in +link{CalendarEvent.lane, appropriate lanes}, respecting padding and 
// overlapping.  If +link{canEditEvents} is true, events can be drag-moved or drag-resized 
// from their top and bottom edges, within the containing lane.  To allow events to be dragged 
// from one lane into another, see +link{canEditLane}.
// 
// @visibility external
//<

//> @method calendar.setShowDayLanes()
// Changes the +link{showDayLanes, view mode} of the day view at runtime - whether to show a
// normal day column for the +link{chosenDate}, or the specified set of 
// +link{calendar.lanes, vertical lanes}.
// 
// @param showDayLanes (boolean) whether or not to show lanes in the day view
// @visibility external
//<
setShowDayLanes : function (showDayLanes) {
    if (this.showDayLanes == showDayLanes) return;
    this.showDayLanes = showDayLanes;
    if (this.dayView) {
        this.dayView._scrollRowAfterRefresh = this.dayView.body.getScrollTop();
        this.dayView.rebuildFields();
        if (this.dayViewSelected()) {
            this.dayView.refreshEvents();
        } else {
            this.dayView._needsRefresh = true;
        }
    }
},

//> @attr calendar.minLaneWidth (Integer : null : IR)
// When showing +link{showDayLanes, vertical lanes} in the +link{dayView}, this attribute sets 
// the minimum width of each column or field.
// 
// @visibility external
//<

//> @attr calendar.overlapSortSpecifiers (Array of SortSpecifier : null : IRW)
// For +link{Timeline, timelines} that allow overlapping events, an array of 
// +link{SortSpecifier, sort-specifiers} that dictate the vertical rendering order of 
// overlapped events in each +link{Lane, lane}.
// <P>
// By default, events that share space in a Lane are rendered from top to bottom according to 
// their +link{startDateField, start-dates} - the earliest in a given lane appears top-most in 
// that lane.
// <P>
// Providing <code>overlapSortSpecifiers</code> allows for the events to be ordered by one or
// more of the fields stored on the events, or in the underlying +link{DataSource, data-source},
// if the timeline is databound.
//
// @visibility external
//<

//> @attr calendar.todayBackgroundColor (String : null : IR)
// The background color for today in the Month view, or in the Timeline view when 
// +{timelineGranularity} is "day".
// @visibility external
//<

//> @attr calendar.showEventDescriptions (Boolean : true : IR)
// If false, the event header will take up the entire space of the event. This is useful
// when you want to be able to drag reposition by the entire event and not just the header.
// @visibility external
//<
showEventDescriptions: true,

//> @method calendar.eventsRendered()
// A notification method fired when the events in the current view have been refreshed.
// @visibility calendar
//<


// Event Overlap
// ---------------------------------------------------------------------------------------

//> @attr calendar.allowEventOverlap (boolean : true : IR)
// If false, events are not allowed to overlap when they are drag repositioned or resized.
// Events that *would* overlap an existing event will automatically be placed either before or 
// after those events.
//
// @visibility internal
//<
allowEventOverlap: true,

//> @attr calendar.equalDatesOverlap (boolean : null : IR) 
// If true, when events or date ranges share a border on exactly the same date (and time),
// they will be treated as overlapping. By default, the value of this attribute is null, 
// meaning that such events will *not* be treated as overlapping.
//
// @visibility internal
//<

// ---------------------------------------------------------------------------------------

//> @attr calendar.sizeEventsToGrid (Boolean : true : IR)
// If true, events will be sized to the grid, even if they start and/or end at times
// between grid cells.
// @visibility external
//<
sizeEventsToGrid: true,

// i18n
// ---------------------------------------------------------------------------------------

//> @attr calendar.dayViewTitle (string : "Day" : IR)
// The title for the +link{dayView, day view}.
// 
// @group i18nMessages
// @visibility calendar
//<
dayViewTitle: "Day",

//> @attr calendar.weekViewTitle (string : "Week" : IR)
// The title for the +link{weekView, week view}.
// 
// @group i18nMessages
// @visibility calendar
//<
weekViewTitle: "Week",

//> @attr calendar.monthViewTitle (string : "Month" : IR)
// The title for the +link{monthView, month view}.
// 
// @group i18nMessages
// @visibility calendar
//<
monthViewTitle: "Month", 

//> @attr calendar.timelineViewTitle (string : "Timeline" : IR)
// The title for the +link{timelineView, timeline view}.
// 
// @group i18nMessages
// @visibility external
//<
timelineViewTitle: "Timeline",

//> @attr calendar.eventNameFieldTitle (string : "Event Name" : IR)
// The title for the event name field in the quick and advanced event dialogs
// 
// @group i18nMessages
// @visibility calendar
//<
eventNameFieldTitle: "Event Name",

//> @attr calendar.eventStartDateFieldTitle (string : "From" : IR)
// The title for the start date field in the quick and advanced event dialogs
// 
// @group i18nMessages
// @visibility calendar
//<
eventStartDateFieldTitle: "From",

//> @attr calendar.eventEndDateFieldTitle (string : "To" : IR)
// The title for the end date field in the quick and advanced event dialogs
// 
// @group i18nMessages
// @visibility calendar
//<
eventEndDateFieldTitle: "To",

//> @attr calendar.eventDescriptionFieldTitle (string : "Description" : IR)
// The title for the +link{descriptionField} field in the quick and advanced event dialogs
// 
// @group i18nMessages
// @visibility calendar
//<
eventDescriptionFieldTitle: "Description",

//> @attr calendar.eventLaneFieldTitle (string : "Lane" : IR)
// The title for the +link{calendar.laneFields, laneField} field in the quick and advanced event dialogs
// 
// @group i18nMessages
// @visibility calendar
//<
eventLaneFieldTitle: "Lane",

//> @attr calendar.saveButtonTitle (string : "Save Event" : IR)
// The title for the save button in the quick event dialog and the event editor
// 
// @group i18nMessages
// @visibility calendar
//<
saveButtonTitle: "Save Event",

//> @attr calendar.detailsButtonTitle (string : "Edit Details" : IR)
// The title for the edit button in the quick event dialog
// 
// @group i18nMessages
// @visibility calendar
//<
detailsButtonTitle: "Edit Details",

//> @attr calendar.cancelButtonTitle (string : "Cancel" : IR)
// The title for the cancel button in the event editor
// 
// @group i18nMessages
// @visibility calendar
//<
cancelButtonTitle: "Cancel", 

//> @attr calendar.previousButtonHoverText (string : "Previous" : IR)
// The text to be displayed when a user hovers over the +link{calendar.previousButton, previous}
// toolbar button.
// 
// @group i18nMessages
// @visibility calendar
//<
previousButtonHoverText: "Previous",

//> @attr calendar.nextButtonHoverText (string : "Next" : IR)
// The text to be displayed when a user hovers over the +link{calendar.nextButton, next} 
// toolbar button
// 
// @group i18nMessages
// @visibility calendar
//<
nextButtonHoverText: "Next",

//> @attr calendar.addEventButtonHoverText (string : "Add an event" : IR)
// The text to be displayed when a user hovers over the +link{calendar.addEventButton, add event}
// toolbar button
// 
// @group i18nMessages
// @visibility calendar
//<
addEventButtonHoverText: "Add an event",

//> @attr calendar.datePickerHoverText (string : "Choose a date" : IR)
// The text to be displayed when a user hovers over the +link{calendar.datePickerButton, date picker}
// toolbar button
// 
// @group i18nMessages
// @visibility calendar
//<
datePickerHoverText: "Choose a date",

//> @attr calendar.invalidDateMessage (Boolean : "From must be before To" : IR)
// The message to display in the +link{eventEditor} when the 'To' date is greater than
// the 'From' date and a save is attempted.
//
// @group i18nMessages
// @visibility calendar
//<
invalidDateMessage: "From must be before To",


// AutoChild constructors and defaults
// ----------------------------------------------------------------------------------------
dayViewConstructor: "DaySchedule",

weekViewConstructor: "WeekSchedule",

monthViewConstructor: "MonthSchedule",

timelineViewConstructor: "TimelineView", 

mainViewDefaults : {
    _constructor:isc.TabSet,
    defaultWidth: "80%",
    defaultHeight: "100%",
    tabBarAlign: "right",
    selectedTab: 1    
},

dateChooserConstructor: "DateChooser",

//> @attr calendar.eventDialog (AutoChild Window : null : R)
// An +link{AutoChild} of type +link{Window} that displays a quick event entry form in a 
// popup window.
//
// @visibility calendar
//<
eventDialogConstructor: "Window",
eventDialogDefaults : {
    showHeaderIcon: false,
    showMinimizeButton: false,
    showMaximumButton: false,
    canDragReposition: true,
    // so that extra fields are visible without the end user having to tweak bodyProperties
    overflow: "visible",
    bodyProperties: {overflow: "visible"},
    width: 400,
    height: 100
    
},

//> @attr calendar.eventEditorLayout (AutoChild Window : null : R)
// An +link{AutoChild} of type +link{Window} that displays the full 
// +link{calendar.eventEditor, event editor}
//
// @visibility calendar
//<
eventEditorLayoutConstructor: "Window",
eventEditorLayoutDefaults : {
    showHeaderIcon: false,
    showShadow: false,
    showMinimizeButton: false,
    showMaximumButton: false,
    canDragReposition: false    
},


//> @attr calendar.eventEditor (AutoChild DynamicForm : null : R)
// An +link{AutoChild} of type +link{DynamicForm} which displays +link{CalendarEvent, event data}. 
// This form is created within the +link{calendar.eventEditorLayout,event editor layout}
//
// @visibility calendar
//<
eventEditorConstructor: "DynamicForm",
eventEditorDefaults : {
    padding: 4,
    numCols: 4,
    showInlineErrors: false,
    width: 460,
    titleWidth: 60
},

//> @attr calendar.showAddEventButton (Boolean : null : IRW)
// Set to false to hide the +link{addEventButton, Add Event} button.
// @visibility calendar
//<

//> @attr calendar.addEventButton (AutoChild ImgButton : null : IR)
// An +link{ImgButton} that appears in a Calendar's week/day/month views and offers an 
// alternative way to create a new +link{CalendarEvent, event}.
//
// @visibility calendar
//<
addEventButtonConstructor: "ImgButton",
addEventButtonDefaults : {
    title: "",
    src:"[SKINIMG]actions/add.png",    
    showRollOver: false, 
    showDown: false,
    showFocused:false,
    width: 16, 
    height: 16
},

//> @attr calendar.showDatePickerButton (Boolean : null : IRW)
// Set to false to hide the +link{datePickerButton} that allows selecting a new base date for 
// this Calendar.
// @visibility calendar
//<

//> @attr calendar.datePickerButton (AutoChild ImgButton : null : IR)
// An +link{ImgButton, ImgButton} that appears above the various views of the
// calendar and offers alternative access to a +link{DateChooser} to pick the current day.
//
// @visibility calendar
//<
datePickerButtonConstructor: "ImgButton",
datePickerButtonDefaults : {
    title: "",
    src:"[SKIN]/controls/date_control.gif",
    width: 16, 
    height: 16,
    showRollOver: false,
    showFocused: false
},

//> @attr calendar.showControlsBar (Boolean : true : IR)
// If false the controls bar at the top of the calendar will not be displayed - this means 
// that the +link{controlsBar} will be hidden, so the autoChildren (+link{previousButton}, 
// +link{dateLabel}, +link{nextButton}, +link{addEventButton}, and +link{datePickerButton}) 
// will not be created or shown.
// @visibility calendar
//<
showControlsBar: true,

//> @attr calendar.controlsBar (AutoChild HLayout : null : IR)
// An +link{class:HLayout, HLayout} shown above the Calendar views and displaying a set of
// controls for interacting with the current view - namely, the +link{nextButton, next},
// +link{previousButton, previous} and +link{addEventButton, add} buttons, 
// the +link{dateLabel, date label} and the +link{datePickerButton, date-picker} icon.
//
// @visibility calendar
//<
controlsBarConstructor: "HLayout",
controlsBarDefaults : {
    defaultLayoutAlign:"center",
    height: 25,
    membersMargin: 5
},

//> @attr calendar.showPreviousButton (Boolean : null : IRW)
// Set to false to hide the +link{previousButton, Previous} button.
// @visibility calendar
//<

//> @attr calendar.previousButton (AutoChild ImgButton : null : IR)
// An +link{ImgButton} that appears above the week/day/month views of the
// calendar and allows the user to move the calendar backwards in time.
//
// @visibility calendar
//<
previousButtonConstructor: "ImgButton",
previousButtonDefaults : {
    title: "", 
    src:"[SKINIMG]actions/back.png",
    showFocused:false,
    width: 16, 
    height: 16,
    click : function () {
        this.creator.previous();
    },
    showRollOver: false, 
    showDown: false
},


//> @attr calendar.showNextButton (Boolean : null : IRW)
// Set to false to hide the +link{nextButton, Next} button.
// @visibility calendar
//<

//> @attr calendar.nextButton (AutoChild ImgButton : null : IR)
// An +link{ImgButton} that appears above the week/day/month views of the
// calendar and allows the user to move the calendar forwards in time.
//
// @visibility calendar
//<
nextButtonConstructor: "ImgButton",
nextButtonDefaults : {
    title: "", 
    src:"[SKINIMG]actions/forward.png", 
    showFocused:false,
    width: 16, 
    height: 16,
    click : function () {
        this.creator.next();
    },
    showRollOver: false, 
    showDown: false        
}, 

//> @attr calendar.dateLabel (AutoChild Label : null : IR)
// The +link{AutoChild} +link{Label} used to display the current date or range above the 
// selected calendar view.
//
// @visibility calendar
//<
dateLabelConstructor: "Label",
dateLabelDefaults : {
    wrap: false,
    width: 5,
    contents: "-"    
},

// initial setup of the calendar
initWidget : function () {
    if (!this.chosenDate) this.chosenDate = new Date();
    this.year = this.chosenDate.getFullYear();
    this.month = this.chosenDate.getMonth();

    if (this.firstDayOfWeek == null) 
        this.firstDayOfWeek = Number(isc.DateChooser.getInstanceProperty("firstDayOfWeek"));

    //>!BackCompat 2012.03.14 - previously undoc'd attributes, now being replaced
    if (this.timelineSnapGap != null) {
        this.snapGap = this.timelineSnapGap;
        delete this.timelineSnapGap;
    }
    if (this.timelineStartDate != null) {
        this.startDate = this.timelineStartDate.duplicate();
        delete this.timelineStartDate;
    }
    if (this.timelineEndDate != null) {
        this.endDate = this.timelineEndDate.duplicate();
        delete this.timelineEndDate;
    }
    if (this.timelineLabelFields != null) {
        this.laneFields = this.timelineLabelFields;
        this.timelineLabelFields = null;
    }
    if (this.eventTypeData != null) {
        this.lanes = isc.clone(this.eventTypeData);
        this.eventTypeData = null;
    }
    if (this.eventTypeField != null) {
        this.laneNameField = this.eventTypeField;
        delete this.eventTypeField;
    }
    if (this.showDescription != null) {
        this.showEventDescriptions = this.showDescription;
        delete this.showDescription;
    }
    if (this.canEditEventType != null) {
        this.canEditLane = this.canEditEventType;
        delete this.canEditEventType;
    }
    if (this.canDeleteEvents != null) {
        this.canRemoveEvents = this.canDeleteEvents;
        delete this.canDeleteEvents;
    }
    //<!BackCompat

    if (this.overlapSortSpecifiers && !isc.isAn.Array(this.overlapSortSpecifiers)) {
        this.overlapSortSpecifiers = [this.overlapSortSpecifiers];
    }

    if (!this.data) this.data = this.getDefaultData();
    // set hover text strings for toolbar buttons
    // can't set dynamically in defaults block, so have to do it here.
    this.previousButtonDefaults.prompt = this.previousButtonHoverText;
    this.nextButtonDefaults.prompt = this.nextButtonHoverText;
    this.datePickerButtonDefaults.prompt = this.datePickerHoverText;
    this.addEventButtonDefaults.prompt  = this.addEventButtonHoverText;
    
    this._setChosenWeek();
    this.createChildren();
    this._setWeekTitles();

    if (!this.initialCriteria && this.autoFetchData) {
        this.initialCriteria = this.getNewCriteria(null);
    }

    // initialize the data object, setting it to an empty array if it hasn't been defined
    this.setData(null);

    this.invokeSuper(isc.Calendar, "initWidget");

    this.createEditors();
},

autoDetectFieldNames : function () {
    this.dataSource = isc.DS.getDataSource(this.dataSource);

    // pick some likely looking fields if no sensible ones are provided - wants 
    // for some future cleverness, perhaps, pretty basic selection here
    
    var ds = this.dataSource,
        fields = isc.getValues(ds.getFields()),
        maxSize = 1024000,
        bestField,
        field
    ;

    if (this.fieldIsMissing(this.nameField, ds)) {
        // assume the titleField from the DS if the 
        this.nameField = ds.getTitleField();
    }
    if (this.fieldIsMissing(this.descriptionField, ds)) {
        // loop and find a string field > 255 chars and < 100k (otherwise 
        // choose the largest under 100k)
        fields.sortByProperties(["length"], [false]);

        bestField = {length:0};
        for (var i=0; i<fields.length; i++) {
            field = fields.get(i);
            if (!field.type || field.type == "text" || field.type == "string") {
                if (field.length > 255 && field.length < maxSize) {
                    this.descriptionField = field.name;
                    break;
                } else if (field.length && field.length < maxSize && 
                    field.length > bestField.length) {
                    bestField = field;
                } else if (!field.length) {
                    if (!bestField) bestField = field;
                } 
            }
        }
        if (this.fieldIsMissing(this.descriptionField, ds) && bestField)
            this.descriptionField = bestField.name;
    }
    if (this.fieldIsMissing(this.startDateField, ds)) {
        // any date field, preferring one with "start" or "begin" in it's name        
        bestField=null;
        for (var i=0; i<fields.length; i++) {
            field = fields.get(i);
            if ((field.type == "date" || field.type == "datetime")) {
                if (field.name.toLowerCase().indexOf("start") >= 0 ||
                    field.name.toLowerCase().indexOf("begin") >= 0) 
                {
                    this.startDateField = field.name;
                    break;
                } else bestField = field;
            }
        }
        if (this.fieldIsMissing(this.startDateField, ds) && bestField)
            this.startDateField = bestField.name;
    }
    if (this.fieldIsMissing(this.endDateField, ds)) {
        // any date field, preferring one with "end" or "stop" in it's name
        bestField=null;
        for (var i=0; i<fields.length; i++) {
            field = fields.get(i);
            if ((field.type == "date" || field.type == "datetime")) {
                if (field.name.toLowerCase().indexOf("end") >= 0 ||
                    field.name.toLowerCase().indexOf("stop") >= 0) 
                {
                    this.endDateField = field.name;
                    break;
                } else if (field.name != this.startDateField) 
                    bestField = field;
            }
        }
        if (this.fieldIsMissing(this.endDateField, ds) && bestField)
            this.endDateField = bestField.name;
    }
},

fieldIsMissing : function (fieldName, ds) {
    // is a field unset or absent from the ds
    return (!fieldName || fieldName == "" || (ds && !ds.getField(fieldName)));
},

getDefaultData : function () { return []; },

//> @method calendar.setData() ([])
// Initialize the data object with the given array. Observes methods of the data object
// so that when the data changes, the calendar will redraw automatically.
// 
// @param newData (List of CalendarEvent) data to show in the list
//
// @group data
// @visibility calendar
//<
setData : function (newData) {
    // if the current data and the newData are the same, bail
    // (this also handles the case that both are null)
    if (this.data == newData) return;

    // if we are currently pointing to data, stop observing it
    if (this.data) {
        this.ignore(this.data, "dataChanged");
        // if the data was autoCreated, destroy it to clean up RS<->DS links
        if (this.data._autoCreated && isc.isA.Function(this.data.destroy))
            this.data.destroy();
    }

    // if newData was passed in, remember it
    if (newData) this.data = newData;

    // if data is not set, bail
    if (!this.data) return;

    // observe the data so we will update automatically when it changes
    this.observe(this.data, "dataChanged", "observer.dataChanged()");
    if (this.hasData()) {
        // invoke dataChanged so calendar refreshes when passed new data
        this.dataChanged();
    }
},

//> @method calendar.getData()
// Get the data that is being displayed and observed
//
// @return (object) The data that is being displayed and observed
//<
getData : function () {
    return this.data;
},

hasData : function () {
    if (!this.data || 
        (isc.ResultSet && isc.isA.ResultSet(this.data) && !this.data.lengthIsKnown())) 
    {
        return false;
    } else {
        return true;
    }
},


dataChanged : function () {
    if (this.destroying || this.destroyed) return;

    // see addEvent, updateEvent, deleteEvent, and comment above about _ignoreDataChanged
    if (this._ignoreDataChanged) {
        this.logDebug('dataChanged, ignoring','calendar');
        this._ignoreDataChanged = false;    
    } else {
        this.logDebug('dataChanged, refreshing', 'calendar');
        this.refreshSelectedView();
    }
   
},

destroy : function () {
    if (this.data) this.ignore(this.data, "dataChanged");
    this.Super("destroy", arguments);
},

refreshSelectedView : function () {
    if (this.dayViewSelected()) {
        this.dayView.refreshEvents();
        if (this.monthView) this.monthView.refreshEvents();
    } else if (this.weekViewSelected()) {
        this.weekView.refreshEvents();
        if (this.monthView) this.monthView.refreshEvents();
    } else if (this.monthViewSelected()) {
        this.monthView.refreshEvents();
    } else if (this.timelineViewSelected()) {
        this.timelineView.refreshEvents();
    }
},

//> @method calendar.getSelectedView()
// Returns the currently selected +link{ListGrid, grid-view} instance.
// @return (ListGrid) the currently selected grid-view
//<
getSelectedView : function () {
    if (this.dayViewSelected()) {
       return this.dayView;
    } else if (this.weekViewSelected()) {
       return this.weekView;
    } else if (this.monthViewSelected()) {
       return this.monthView;
    } else if (this.timelineViewSelected()) {
       return this.timelineView;    
    }   
},

//> @type ViewName
// The names of the Calendar views.
// @value "day" day view
DAY: "day",
// @value "week" week view
WEEK: "week",
// @value "month" month view
MONTH: "month",
// @value "timeline" timeline view
TIMELINE: "timeline",
// @visibility external
//<

//> @attr calendar.rowHeight (number : 20 : IRW)
// The height of time-slots in the calendar.
// @visibility external
//<
rowHeight: isc.ListGrid.getInstanceProperty("cellHeight"), 

setRowHeight : function (newHeight, skipScroll) {
    if (this.eventSnapGap == this.rowHeight) {
        
        this.eventSnapGap = newHeight;
    }
    this.rowHeight = newHeight;
    if (this.dayView) {
        this.dayView.setCellHeight(this.rowHeight);
        this.dayView.refreshEvents();
        if (this.scrollToWorkday && !skipScroll) this.dayView.scrollToWorkdayStart();
    }
    if (this.weekView) {
        this.weekView.setCellHeight(this.rowHeight);
        this.weekView.refreshEvents();
        if (this.scrollToWorkday && !skipScroll) this.weekView.scrollToWorkdayStart();
    }
},

//> @attr calendar.currentViewName (ViewName : null: IRW)
// The name of the view that should be visible initially by default.
// @visibility external
//<

//> @method calendar.getCurrentViewName()
// Get the name of the visible view.   Returns one of 'day', 'week', 'month' or 'timeline'.
//
// @return (ViewName) The name of the currently visible view.
// @visibility external
//<
getCurrentViewName : function () {
    var view = this.getSelectedView();
    return view != null ? view.viewName : null;
},

//> @method calendar.setCurrentViewName()
// Sets the currently visible view.
//
// @param viewName (ViewName) The name of the view that should be made visible.
// @return (ViewName) The name of the visible view.
// @visibility external
//<
setCurrentViewName : function (viewName) {
    var tabToSelect = this.mainView.tabs.findIndex("viewName", viewName);
    if (tabToSelect != null) this.selectTab(tabToSelect);    

    return viewName;
},

// get/setEventCanvasID ensure that eventCanvas-to-event mapping remains stable when databound. 
// The expando approach doesn't work when databound because the expando gets wiped out
// on update.
getEventCanvasID : function (view, event) {
    if (!event) return null;
    var ds = this.getDataSource();
    if (view._eventCanvasMap && ds && ds.getPrimaryKeyFieldNames().length > 0) {
        var pks = ds.getPrimaryKeyFields();
        var pk = "";
        for (var pkName in pks) {
            pk += event[pkName]; 
        }
        return view._eventCanvasMap[pk];
    } else {
        return event._eventCanvasID;
    }
},

setEventCanvasID : function (view, event, eventCanvasID) {
    if (!view._eventCanvasMap) view._eventCanvasMap = [];
    var ds = this.getDataSource();
    if (ds && ds.getPrimaryKeyFieldNames().length > 0) {
        var pks = ds.getPrimaryKeyFields();
        var pk = "";
        for (var pkName in pks) {
            pk += event[pkName];        
        }
        view._eventCanvasMap[pk] = eventCanvasID;
    } else {
        event._eventCanvasID = eventCanvasID;    
    }
},

//< @method calendar.clearTimeSelection()
// When overriding +link{calendar.backgroundClick} and returning false to suppress default
// behavior, use this method to clear the selection from the day or week views.
// @visibility internal
//<
clearTimeSelection : function () {
    if (this.dayView) this.dayView.clearSelection();
    if (this.weekView) this.weekView.clearSelection();
},

// includes start date but not end date
getDayDiff : function (date1, date2, weekdaysOnly) {
    return Math.abs(isc.Date._getDayDiff(date1, date2, weekdaysOnly, false));
},

getEventStartCol : function (event, eventWin) {
    var view = eventWin ? eventWin._parentView : this.getSelectedView(),
        win = eventWin || this._findEventWindow(event, view),
        startCol = view.getEventColumn(win.getLeft() + 1);
    return startCol;
},

getEventEndCol : function (event, eventWin) {
    var view = eventWin ? eventWin._parentView : this.getSelectedView(),
        win = eventWin || this._findEventWindow(event, view),
        endCol = view.getEventColumn(win.getLeft() + win.getVisibleWidth() + 1);
    return endCol;
},

// helper method for getting the left coordinate of an event
getEventLeft : function (event, grid) {
    grid = grid || this.getSelectedView();
    
    var colSize = grid.body.getColumnWidth(0),
        eLeft = 0
    ;
    if (grid.isWeekView()) {
        var dayDiff = this.getDayDiff(event[this.startDateField], this.chosenWeekStart, 
            (this.showWeekends == false));
        //isc.logWarn('getEventLeft:' + [event.name, event.startDate.toShortDate(), 
        //                   this.chosenWeekStart.toShortDate(),dayDiff ]);
        eLeft = (dayDiff * colSize);
    } else if (this.showDayLanes) {
        var fieldId = grid.completeFields.findIndex("name", event[this.laneNameField]);
        if (fieldId) {
            eLeft = grid.getColumnLeft(fieldId);
        }
    }
    if (this.logIsDebugEnabled("calendar")) {
        this.logDebug('calendar.getEventLeft() = ' + eLeft + ' for:' + isc.Log.echoFull(event), 'calendar');
    }
    return eLeft;
},

//> @method calendar.setShowWeekends()
//  Setter for +link{calendar.showWeekends} to change this property at runtime.
//
// @visibility calendar
//<
setShowWeekends : function (showWeekends) {
    this.showWeekends = showWeekends;
    if (isc.isA.TabSet(this.mainView)) {
        var tabNum = this.mainView.getSelectedTabNumber();
        this.mainView.removeTabs(this.mainView.tabs);
      
        if (this.dayView) this.dayView.destroy();
       
        if (this.weekView) this.weekView.destroy();
        
        if (this.monthView) this.monthView.destroy();
        
        var newTabs = this._getTabs();
        
        this.mainView.addTabs(newTabs);
        this.mainView.selectTab(tabNum);
        
    } else {
        var memLayout = this.children[0].members[1];
        
        var oldMem = memLayout.members[1];
        var newMem = this._getTabs()[0].pane;
        
        memLayout.removeMember(oldMem);
        oldMem.destroy();
        memLayout.addMember(newMem);
        //memLayout.redraw();
        //newMem.show();
    }
    this._setWeekTitles();
    this.setDateLabel();
},

//> @method calendar.canEditEvent()
// Method called whenever the calendar needs to determine whether a particular event should be
// editable.
// <P>
// By default, checks the +link{canEditField} on the provided +link{CalendarEvent}, and if null,
// returns +link{canEditEvents}.
//
// @param event (CalendarEvent) 
// @return (boolean) whether the user should be allowed to edit the provided CalendarEvent
//<
canEditEvent : function (event) {
    if (!event) return false;
    else if (event[this.canEditField] != null) return event[this.canEditField];
    else return this.canEditEvents;
},

//> @method calendar.canRemoveEvent()
// Method called whenever the calendar needs to determine whether a particular event should show
// a remove button to remove it from the dataset.
// <P>
// By default, checks the +link{canRemoveField} on the provided +link{CalendarEvent}, and if null,
// returns true if both +link{canRemoveEvents} and 
// +link{calendar.canEditEvent, canEditEvent(event)} are true.
//
// @param event (CalendarEvent) 
// @return (boolean) whether the user should be allowed to remove the provided CalendarEvent
//<
canRemoveEvent : function (event) {
    if (!event) return false;
    // return the canRemoveField value if its set
    else if (event[this.canRemoveField] != null) return event[this.canRemoveField];
    // return true if canRemoveEvents is true AND the event is editable
    else return this.canRemoveEvents && this.canEditEvent(event);
},

getDateEditingStyle : function () {
    // ensure backward compatibility
    if (!this.timelineView) {
        return "time";
    }
    var result = this.dateEditingStyle;
    if (!result) {
        // auto-detect based on field-type
        if (this.dataSource) result = this.getDataSource().getField(this.startDateField).type;

        // default to datetime
        if (!result) {
            switch (this.timelineGranularity) {
                case "hour": result = "datetime"; break; // > "minute" && < "day"
                case "millisecond":
                case "second":
                case "minute": result = "time"; break; // <= "minute"
                default: result = "date"; break; // >= "day"
            }
        }
    }
    return result;
},

//> @method calendar.addLaneEvent()
// For +link{Timeline}s, and for +link{dayView}s with +link{showDayLanes} set, create a new 
// event and adds it to a particular +link{Lane}.
//
// @param laneName        (Lane) the Lane in which to add this event
// @param startDate       (Date or Object) start date of event, or CalendarEvent Object 
// @param [endDate]       (Date) end date of event
// @param [name]          (String) name of event
// @param [description]   (String) description of event
// @param [otherFields]   (any) new values of additional fields to be updated
//
// @visibility calendar
//<
addLaneEvent : function (laneName, startDate, endDate, name, description, otherFields) {
    this.addEvent(startDate, endDate, name, description, otherFields, laneName);
},

//> @method calendar.addEvent()
// Create a new event in this calendar instance.
//
// @param startDate       (Date or Object) start date of event, or CalendarEvent Object 
// @param [endDate]       (Date) end date of event
// @param [name]          (String) name of event
// @param [description]   (String) description of event
// @param [otherFields]   (Object) new values of additional fields to be updated
//
// @visibility calendar
//<
addEvent : function (startDate, endDate, name, description, otherFields, laneName, ignoreDataChanged) {
    // We explicitly update the UI in this method, so no need to react to 'dataChanged' on the
    // data object
    if (ignoreDataChanged == null) ignoreDataChanged = true;
    if (!isc.isAn.Object(otherFields)) otherFields = {};
    var evt;
    if (isc.isA.Date(startDate)) {
        evt = {};
        evt[this.startDateField] = startDate;
        evt[this.endDateField] = endDate;
        evt[this.nameField] = name;
        evt[this.descriptionField] = description;
        // if laneName isn't passed, but otherFields[calendar.laneNameField] is, use that
        if (laneName || otherFields[this.laneNameField]) {
            evt[this.laneNameField] = laneName || otherFields[this.laneNameField];
        }
        isc.addProperties(evt, otherFields);
    } else if (isc.isAn.Object(startDate)) {
        evt = startDate;
    } else {
        isc.logWarn('addEvent error: startDate parameter must be either a Date or an event record (Object)');
        return;
    }

    var _this = this;
    
    // add event to data
    // see comment above dataChanged about _ignoreDataChanged
    if (ignoreDataChanged) this._ignoreDataChanged = true;
    if (this.dataSource) {
        isc.DataSource.get(this.dataSource).addData(evt, function (dsResponse, data, dsRequest) {
            _this.processSaveResponse(dsResponse, data, dsRequest);
        }, {componentId: this.ID, willHandleError: true});
        return;
    } else {
        // set the one-time flag to ignore data changed since we manually refresh in _finish()
        this._ignoreDataChanged = true;
        this.data.add(evt);
        this.processSaveResponse({status:0}, [evt], {operationType:"add"});
    }
    
},

//> @method calendar.removeEvent()
// Remove an event from this calendar.
//
// @param event (Object) The event object to remove from the calendar
//
// @visibility calendar
//<
removeEvent : function (event, ignoreDataChanged) {
    // We explicitly update the UI in this method, so no need to react to 'dataChanged' on the
    // data object
    if (ignoreDataChanged == null) ignoreDataChanged = true;

    var startDate = event[this.startDateField],
        endDate = event[this.endDateField];
    
     // set up a callback closure for when theres a DS
    var self = this;
    var _finish = function () {
        if (self._shouldRefreshDay(startDate, endDate)) {
            self.dayView.removeEvent(event);
        }
        if (self._shouldRefreshWeek(startDate, endDate)) {
            self.weekView.removeEvent(event);
        }
        if (self._shouldRefreshMonth(startDate, endDate)) {
            self.monthView.refreshEvents();
        }
        if (self._shouldRefreshTimelineView(startDate, endDate)) {
            self.timelineView.refreshEvents();
        }
        // when eventAutoArrange is true, refresh the day and week views to reflow the events
        // so that they fill any space made available by the removed event
        if (self.eventAutoArrange) {
            if (self.dayView) self.dayView.refreshEvents();
            if (self.weekView) self.weekView.refreshEvents();    
        }
        // fire eventRemoved if present
        if (self.eventRemoved) self.eventRemoved(event);
    };
    // remove the data
    // see comment above dataChanged about _ignoreDataChanged
    if (ignoreDataChanged) this._ignoreDataChanged = true;
    if (this.dataSource) {
        isc.DataSource.get(this.dataSource).removeData(event, _finish, {
            componentId: this.ID,
            oldValues : event
        }); 
        return;
    } else {
        this.data.remove(event);
        _finish();
    }
             
},

//> @method calendar.updateEvent()
// Update an event in this calendar.
//
// @param event       (CalendarEvent) The event object to remove from the calendar
// @param startDate   (Date) start date of event
// @param endDate     (Date) end date of event
// @param name        (String) name of event
// @param description (String) description of event
// @param otherFields (Object) new values of additional fields to be updated
//     
// @visibility calendar
//<
updateEvent : function (event, startDate, endDate, name, description, otherFields, ignoreDataChanged, laneName) {
    // We explicitly update the UI in this method, so no need to react to 'dataChanged' on the
    // data object
    if (ignoreDataChanged == null) ignoreDataChanged = true;

    if (!isc.isAn.Object(otherFields)) otherFields = {};
    
    // must call _shouldRefreshDay twice, both with old and new dates. see _shouldRefreshDay.
    var oldStart = event[this.startDateField];
    var oldEnd = event[this.endDateField];

    var _this = this;
    
    // see comment above dataChanged about _ignoreDataChanged
    if (ignoreDataChanged) this._ignoreDataChanged = true;
    if (this.dataSource) {
        var updateRecord, ds = isc.DataSource.get(this.dataSource);

        var changes = {};
        changes[this.startDateField] = startDate.duplicate();
        changes[this.endDateField] = endDate.duplicate();
        changes[this.descriptionField] = description;
        changes[this.nameField] = name;
        if (laneName) changes[this.laneNameField] = laneName;
        var updatedRecord = isc.addProperties({}, event, changes, otherFields);
        ds.updateData(updatedRecord, function (dsResponse, data, dsRequest) {
            _this.processSaveResponse(dsResponse, data, dsRequest, event);
        }, {oldValues: event, componentId: this.ID, willHandleError: true});
        return;
    } else {
        var oldEvent = isc.addProperties({}, event);
        event[this.startDateField] = startDate.duplicate();
        event[this.endDateField] = endDate.duplicate();
        event[this.descriptionField] = description;
        event[this.nameField] = name;
        if (laneName) event[this.laneNameField] = laneName;
        isc.addProperties(event, otherFields);
        this.processSaveResponse({status:0}, [event], {operationType:"update"}, oldEvent);
    }
   
},

processSaveResponse : function (dsResponse, data, dsRequest, oldEvent) {
    var newEvent = isc.isAn.Array(data) ? data[0] : data;
    
    if (!newEvent || isc.isA.String(newEvent)) newEvent = oldEvent;
    
    var opType = dsRequest ? dsRequest.operationType : null,
        isUpdate = opType == "update",
        isAdd = opType == "add",
        fromDialog = this._fromEventDialog,
        fromEditor = this._fromEventEditor,
        oldStart = isUpdate && oldEvent ? oldEvent[this.startDateField] : null,
        oldEnd = isUpdate && oldEvent ? oldEvent[this.endDateField] : null,
        oldLane = isUpdate && oldEvent ? oldEvent[this.laneNameField] : null
    ;

    delete this._fromEventDialog;
    delete this._fromEventEditor;

    if (dsResponse && dsResponse.status < 0) {
        var errors = dsResponse ? dsResponse.errors : null;
        // show any validation errors inline in the appropriate UI
        if (fromDialog) {
            if (errors) this.eventDialog.items[0].setErrors(errors, true);
            this.eventDialog.show();
        } else if (fromEditor) {
            this.eventEditorLayout.show();
            if (errors) this.eventEditor.setErrors(errors, true);
        }
        // have RPCManager handle other errors
        if (!errors) isc.RPCManager._handleError(dsResponse, dsRequest);
        return;
    }

    var startDate = newEvent[this.startDateField],
        endDate = newEvent[this.endDateField],
        newLane = newEvent[this.laneNameField]
    ;
    if (this._shouldRefreshDay(startDate, endDate) || 
            (isUpdate && this._shouldRefreshDay(oldStart, oldEnd))) 
    {
        if (!this.dayViewSelected()) this.dayView._needsRefresh = true;
        else {
            if (isUpdate) {
                // call self.refreshEvents instead of self.updateEventWindow(newEvent),  to handle 
                // all 3 cases described above _shouldRefreshDay
                this.dayView.refreshEvents();
                //this.dayView.refreshVisibleEvents();
            } else if (isAdd) {
                this.dayView.addEvent(newEvent, null, true);
                this.dayView.refreshVisibleEvents();
            }
        }
    }
    if (this._shouldRefreshWeek(startDate, endDate)) {
        if (!this.weekViewSelected()) this.weekView._needsRefresh = true;
        else {
            if (isUpdate) {
                this.weekView.retagDayEvents(oldStart);
                if (isc.Date.compareLogicalDates(oldStart, startDate) != 0)
                    this.weekView.retagDayEvents(startDate);

                //this.weekView.updateEventWindow(newEvent);
            } else if (isAdd) {
                this.weekView.addEvent(newEvent, null, true);
                this.weekView.refreshVisibleEvents();
            }
        }
    }
    if (this._shouldRefreshMonth(startDate, endDate)) {
        if (!this.monthViewSelected()) this.monthView._needsRefresh = true;
        else this.monthView.refreshEvents();
    }
    if (this._shouldRefreshTimelineView(startDate, endDate)) {
        if (!this.timelineViewSelected()) this.timelineView._needsRefresh = true;
        else {
            if (oldLane && oldLane != newLane) this.timelineView.retagLaneEvents(oldLane);
            this.timelineView.retagLaneEvents(newLane);
            //this.timelineView.refreshEvents();
        }
    }

    // fire eventChanged or eventAdded as appropriate
    if (isUpdate && this.eventChanged) this.eventChanged(newEvent);
    if (isAdd && this.eventAdded) this.eventAdded(newEvent);
},

eventsAreSame : function (first, second) {
    if (this.dataSource) {
        var ds = isc.DataSource.get(this.dataSource), 
            pks = ds.getPrimaryKeyFieldNames(), 
            areEqual = true;
        for (var i=0; i < pks.length; i++) {
            var pkName = pks[i];
            if (first[pkName]!= second[pkName]) {
                areEqual = false;
                break;
            }
        }
        return areEqual;
    } else {
        return (first === second);    
    }
},

// Date / time formatting customization / localization


//> @attr calendar.dateFormatter (DateDisplayFormat : null : [IRW])
// Date formatter for displaying events.
// Default is to use the system-wide default short date format, configured via
// +link{Date.setShortDisplayFormat()}.  Specify any valid +link{type:DateDisplayFormat}.
// @visibility external
//<
dateFormatter:null,

//> @attr calendar.timeFormatter (TimeDisplayFormat : "toShortPaddedTime" : [IRW])
// Display format to use for the time portion of events' date information.
// @visibility external
//<
timeFormatter:"toShortPaddedTime",

//> @method calendar.getEventHoverHTML()
// Gets the hover HTML for an event being hovered over. Override here to return custom 
// HTML based upon the parameter event object.
//
// @param event (CalendarEvent) The event being hovered
// @param eventWindow (EventWindow) the event window being hovered
// @return (HTMLString) the HTML to show in the hover
//     
// @visibility calendar
//<
getEventHoverHTML : function (event, eventWindow) {
    var cal = this;
    
     // format date & times
    var startDate = event[cal.startDateField],
        sDate = startDate.toShortDate(this.dateFormatter, false),
        sTime = isc.Time.toTime(startDate, this.timeFormatter, true),
        endDate = event[cal.endDateField],
        eDate = endDate.toShortDate(this.dateFormatter, false),
        eTime = isc.Time.toTime(endDate, this.timeFormatter, true),
        name = event[cal.nameField],
        description = event[cal.descriptionField],
        result = sDate + "&nbsp;" + sTime + "&nbsp;-&nbsp;" + eTime
    ;

    if (eventWindow._parentView.isTimelineView()) {
        if (startDate.getDate() != endDate.getDate()) {
            // Timeline dates can span days
            result = sDate + "&nbsp;" + sTime + "&nbsp;-&nbsp;" + eDate + "&nbsp;" + eTime;
        }
    }
    
    result += (name || description ? "</br></br>" : "")
            + (name ? name + "</br></br>" : "")
            + (description ? description : "")
    ;
    
    return result;
},

// trickiest case. 3 separate cases to handle:
// 1. event changed within chosen day
// 2. event moved into chosen day
// 3. event moved out of chosen day
// to handle all of these:
// - for adding, just pass start and end date
// - for deleting, just pass start and end date
// - for updating, must call this twice, both with old dates and new dates. see updateEvent.
_shouldRefreshDay : function (startDate, endDate) {
    if (!this.dayView) return false;
    var dayStart = new Date(this.year, this.month, this.chosenDate.getDate(),0, 0);
    var dayEnd = new Date(this.year, this.month, this.chosenDate.getDate(),23, 59);
    // subtle change: use only startDate instead of startDate and endDate to determine if
    // parameter range is in range so that events with end date on the next day are included. 
    if (this.dayView.body && dayStart.getTime() <= startDate.getTime() 
        && dayEnd.getTime() >= startDate.getTime()) {
        return true;
    } else return false;
    
},

_shouldRefreshWeek : function (startDate, endDate) {
    if (!this.weekView) return false;
    // advance end of week date by 1 minute so it falls on the first minute of the next day...
    // this allows events to end on 12:00am of the day following the the last day of the week
    // and fixes a bug where events created at that time weren't showing up
    var weekEnd = this.chosenWeekEnd.duplicate();
    weekEnd.setMinutes(weekEnd.getMinutes() + 1);
    //isc.logWarn('_shouldRefreshWeek:' + [weekEnd, endDate]);
    if (this.weekView.body && this.chosenWeekStart.getTime() <= startDate.getTime()
        && weekEnd.getTime() >= endDate.getTime()) {
        return true;
    } else return false;
},

_shouldRefreshMonth : function (startDate, endDate) {
    if (!this.monthView) return false;
    // provide a nice broad range to detect a month refresh should be done
    var mStart = new Date(this.year, this.month, -7);
    var mEnd = new Date(this.year, this.month, 37);
    if (mStart.getTime() <= startDate.getTime() && mEnd.getTime() >= endDate.getTime()) {
        return true;
    } else return false;
},

_shouldRefreshTimelineView : function (startDate, endDate) {
    // for now just return true if we're showing timeline view
    if (this.showTimelineView) return true;
    else return false;    
},

eventCanvasConstructor: "EventWindow",

//> @attr calendar.eventWindow (AutoChild EventWindow : null : A)
// To display events in day and week views, the Calendar creates instance of +link{EventWindow}
// for each event.  Use the +link{AutoChild} system to customize these windows.
// @visibility external
//<

_getNewEventWindow : function (event, eventIndex, view) {
    var canEdit = this.canEditEvent(event),
        canRemove = this.canRemoveEvent(event),
        styleName = event[this.eventWindowStyleField] || this.eventWindowStyle,
        reclaimed = false
    ;
    
    var props = isc.addProperties({
            // flag for quicker re-detection later
            isEventCanvas: true,
            autoDraw: false,
            calendar: this,
            baseStyle: styleName,
            canDragReposition: canEdit,
            canDragResize: canEdit,
            _redrawWithParent:false,
            showCloseButton: canRemove,
            event: event,
            descriptionText: event[this.descriptionField],
            dragTarget: view.eventDragTarget,
            headerProps: isc.addProperties({}, {dragTarget: view.eventDragTarget}),
            footerProperties: {dragTarget: view.eventDragTarget}
        }, this.eventWindowDefaults, this.eventWindowProperties
    );
    
    // see if there's a *current* eventCanvas that already shows this event - will
    // save time on updating titles and styles, if those things haven't changed
    var canvasPool = view._eventCanvasPool,
        canvas // = view.getCurrentEventCanvas(event),
    ;

    // there's a current event window, or we're recycling them and we have one available...
    if (view.poolEventWindows && (canvas || 
            (this.renderEventsOnDemand && canvasPool[eventIndex]))
       )
    {
        if (canvas) {
            var existingWinIndex = canvasPool.indexOf(canvas);
            if (existingWinIndex != eventIndex) {
                var moveThisWin = canvasPool[eventIndex];
                canvasPool[eventIndex] = canvas;
                canvasPool[existingWinIndex] = moveThisWin;
            }
        } else {
            // ...reclaim the event from the event bin
            canvas = canvasPool[eventIndex];
            canvas.event = event;
            canvas.VSnapOrigin = 0;
            reclaimed = true;
            canvas.setStyleName(event[this.eventWindowStyleField] || this.eventWindowStyle);
        }
        canvas.setProperties(props);
    } else {
        //canvas = view.getCurrentEventCanvas(event);
        // create eventWindow as an autoChild so it can be customized.
        if (!canvas) {
            canvas = this.createAutoChild("eventWindow", props, this.eventCanvasConstructor);
            view._eventCanvasPool.add(canvas);
        }
    }

    canvas.setStyleName(styleName);

    canvas._availableForUse = false;
    
    this.setEventCanvasID(view, event, canvas.ID);

    return canvas;
},
 
_getEventsInRange : function (start, end, view, visibleLanesOnly) {
        
        var results = [],
            wends = Date.getWeekendDays(),
            dataLength = this.data.getLength(),
            //laneNames = (this.lanes || []).getProperty("name")
            laneNames = []
        ;
        view = view || this.getSelectedView();

        if (visibleLanesOnly) {
            var visibleCols = view.body.getVisibleColumns();
            if (visibleCols[0] >= 0 && visibleCols[1] >= 0) {
                for (var i=visibleCols[0]; i<=visibleCols[1]; i++) {
                    laneNames.add(view.body.fields[i][this.laneNameField]);
                }
            }
        }

        for (var i = 0; i < dataLength; i++) { 
            var curr = this.data.get(i);
            
            if (visibleLanesOnly && !laneNames.contains(curr[this.laneNameField])) continue;
                
            if (!curr || !curr[this.startDateField]) return [];
            // add the event if we're showing weekends or the date is not a weekend
            // The event won't get added only when !this.showWeekends and it is a weekend
            // subtle change: use only startDate instead of startDate and endDate to determine if
            // parameter range is in range so that events with end date on the next day are included.
            if (curr[this.startDateField].getTime() >= start.getTime() 
                && curr[this.startDateField].getTime() <= end.getTime()
                && (this.showWeekends || !wends.contains(curr[this.startDateField].getDay())))
            {
                if (view && view.isWeekView()) results.add(curr);
                else if (!this.showDayLanes || laneNames.contains(curr[this.laneNameField]))
                    results.add(curr);
            }
        }
        
        return results;
},

_findEventWindow : function (event, view) {
    // return the eventWindow object containing the passed event
    view = view || this.getSelectedView();
    var isWeek = view.isWeekView();
    
    if (!view.body || !view.body.children) return;
    var arr = view.body.children;
    if (this.dataSource) this._pks = isc.DataSource.get(this.dataSource).getLocalPrimaryKeyFields();
    for (var i = 0; i < arr.length ; i++) {
        var canvas = arr[i];
        if (canvas && canvas.isEventCanvas 
            && view.areSame(canvas.event, event)
            && !!canvas._isWeek == isWeek) {
            // return the event-canvas
            return canvas;
        }
    }

    return false;
},


getDayEnd : function (startDate) {
    return new Date(startDate.getFullYear(), startDate.getMonth(), startDate.getDate(),23,59,59);
},

isTimeline : function () {
    var isTimeline = this.getCurrentViewName() == "timeline";
    return isTimeline;
},

eventsOverlapGridLines: true,

_setChosenWeek : function () {
    
    var startDate = 
        this.chosenWeekStart = new Date(this.year, this.month, this.chosenDate.getDate()
        - this.chosenDate.getDay() + this.firstDayOfWeek);

    // make sure the current week surrounds the current date.
    // if chosen date is less than startDate, shift week window back one week.
    if (Date.compareDates(this.chosenDate,startDate) == 1) { 
        this.chosenWeekStart.setDate(this.chosenWeekStart.getDate() - 7);    
    }
    this.chosenWeekEnd = new Date(startDate.getFullYear(), startDate.getMonth(), 
       startDate.getDate() + 6, 23, 59);
   
    // similarly, if chosen date is greater than chosenWeekEnd, shift week window up one week.
    if (Date.compareDates(this.chosenDate, this.chosenWeekEnd) == -1) {
        this.chosenWeekStart.setDate(this.chosenWeekStart.getDate() + 7);
        this.chosenWeekEnd.setDate(this.chosenWeekEnd.getDate() + 7);
    }
},

//> @method calendar.setChosenDate()
// Set the current date for which the calendar will display events.
//
// @param newDate (Date) the new date to set as the current date
//
// @visibility external
//<
setChosenDate : function (newDate, fromTimelineView) {
    var view = this.getSelectedView();

    this.year = newDate.getFullYear();
    this.month = newDate.getMonth();
    this._oldDate = this.chosenDate.duplicate();
    this.chosenDate = newDate;
    this._setChosenWeek();
    
    if (this.dayView) {
        var props = {
                date: isc.Date.createLogicalDate(
                    newDate.getFullYear(), newDate.getMonth(), newDate.getDate()
                ),
                _dayNum: newDate.getDay(),
                _dateNum: newDate.getDate(),
                _monthNum: newDate.getMonth(),
                _yearNum: newDate.getFullYear()
            },
            field
        ;
        
        for (var i=0; i<this.dayView.body.fields.length; i++) {
            field = this.dayView.body.getField(i);
            // update the date-parts on ALL fields in in a dayView (lanes need the date too)
            if (field) isc.addProperties(field, props);
        }
    }
    
    // redraw monthView if need be
    if (this._oldDate.getFullYear() != this.year || this._oldDate.getMonth() != this.month) { 
        if (this.monthView) {
            if (this.monthViewSelected()) this.monthView.refreshEvents();
            else this.monthView._needsRefresh = true;
        }
    }

    // check if the week needs redrawn
    var startDate = new Date(this._oldDate.getFullYear(), this._oldDate.getMonth(), 
        this._oldDate.getDate() - this._oldDate.getDay());
    var endDate = new Date(startDate.getFullYear(), startDate.getMonth(), 
            startDate.getDate() + 6);
    var chosenTime = this.chosenDate.getTime();
    if (chosenTime < startDate.getTime() || chosenTime > endDate.getTime()) {
        if (this.weekView) {
            this._setWeekTitles();
            if (this.weekViewSelected()) this.weekView.refreshEvents();
            else this.weekView._needsRefresh = true;
        }
    }
    // check for day redraw
    if (chosenTime != this._oldDate.getTime()) {
        if (this.dayView) {
            this.dayView.refreshStyle();
            if (this.dayViewSelected()) this.dayView.refreshEvents();
            else this.dayView._needsRefresh = true;
        }
    }

    if (this.timelineView && !fromTimelineView) {
        this.timelineView.setTimelineRange(this.chosenDate, null, null, null, true);
    } else {
        if (this.scrollToWorkday && view.scrollToWorkdayStart) {
            view.scrollToWorkdayStart();
        } else {
            view.redraw();
        }
    }

    // reset date label
    this.setDateLabel();
    // call dateChanged
    this.dateChanged();

},

//> @method calendar.dateIsWorkday()
// Should the parameter date be considered a workday? By default this method tries to find the
// parameter date day in +link{workdays}, and returns true if found. Override this method to
// provide custom logic for determining workday, for example returning false on holidays.
// <P>
// Note that, when showing +link{calendar.showDayLanes,vertical lanes} in the 
// +link{dayView, day view}, this method is also passed the name of the associated lane.
//
// @param date (Date) date to check for being a workday
// @param laneName (String) the name of the lane if +link{showDayLanes} is true, null otherwise
// @return (boolean) true if date is a workday, false otherwise
// @visibility Calendar
//<
dateIsWorkday : function (date, laneName) {
    if (!date || !this.workdays) return false;
    return this.workdays.contains(date.getDay());
},

//> @method calendar.adjustCriteria()
// Gets the criteria to use when the calendar date ranges shift and the +link{calendar.fetchMode}
// is not "all". This would be called, for example, when the next button is clicked and new
// events possibly need to be fetched. Override this function to add any custom criteria to the
// default criteria constructed by the calendar.
//
// @param defaultCriteria (Criterion) default criteria generated by the calendar
// @return (Criterion) modified criteria
//
// @visibility internal
//<
adjustCriteria : function (defaultCriteria) {
    return defaultCriteria;
},

getNewCriteria : function (view) {
    var criteria = {};

    view = view || this.getSelectedView();

    if (view) { // && view.renderEventsOnDemand) {
        if (this.fetchMode == "timeline") {
            var crit = {
                _constructor:"AdvancedCriteria", operator:"and",
                criteria: [
                    { fieldName: this.startDateField, operator: "lessThan", value: view.endDate},
                    { fieldName: this.endDateField, operator: "greaterThan", value: view.startDate}
                ]
            };
            // allow users to manipulate the criteria by overriding getNewCriteria()
            criteria = this.adjustCriteria(crit);
        }
    }
    return criteria;
},

_usDateRegex:/^\d{4}.\d\d?.\d\d?$/,
_jpDateRegex:/^\d\d?.\d\d.\d{4}?$/,
_setWeekTitles : function () {
    if (!this.weekView) return;
    var nDate = this.chosenWeekStart.duplicate();      
    // set day titles
    var sdNames = Date.getShortDayNames();
    var weekends = Date.getWeekendDays();
    
    for (var i = 1; i < 8; i++) {
        // for hidden columns, getFieldNum will return -1. without this check, a logWarn is
        // produced when weekends are hidden
        if (this.weekView.getFieldNum("day" + i) >= 0) {
            // We want a format like "Mon 28/11" or "Mon 11/28" depending on whether the
            // dateFormatter specified is Euro / US / Japanese.
            // We don't currently have anything built into Date for this so get the shortDate
            // and lop off the year + separator.
            var dateStr = nDate.toShortDate(this.dateFormatter, false);
            
            if (dateStr.match(this._usDateRegex) != null) dateStr = dateStr.substring(5);
            else if (dateStr.match(this._jpDateRegex)) dateStr = dateStr.substring(0,dateStr.length-5);

            var ntitle = sdNames[nDate.getDay()] + " " + dateStr;
            //(nDate.getMonth() + 1) + "/" + nDate.getDate();
            // _dayNum is used in colDisabled()
            // _dateNum, monthNum, yearNum are used in headerClick
            var p = {
                title: ntitle, align: "right",
                _dayNum: nDate.getDay(),
                _dateNum: nDate.getDate(),
                _monthNum: nDate.getMonth(),
                _yearNum: nDate.getFullYear()
            };
            p.date = isc.Date.createLogicalDate(p._yearNum, p._monthNum, p._dateNum),
            this.weekView.setFieldProperties("day" + i, p);
            if (this.weekView.header) this.weekView.header.markForRedraw();
            //isc.logWarn('here:' + [nDate.toShortDate(), "day" + i]);
        }
        
        nDate.setDate(nDate.getDate() + 1);
    }
},

//> @method calendar.next()
// Move to the next day, week, or month, depending on which tab is selected.
//
// @visibility calendar
//<
next : function () {
   // var tab = this.mainView.selectedTab;
    var newDate;
    if (this.dayViewSelected()) {
        newDate = new Date(this.year, this.month, this.chosenDate.getDate() + 1);
        // if hiding weekends, find next non-weekend day
        if (!this.showWeekends) {
            var wends = Date.getWeekendDays();
            for (var i = 0; i < wends.length; i++) {
                if (wends.contains(newDate.getDay())) newDate.setDate(newDate.getDate() + 1);
            }
        }
    } else if (this.weekViewSelected()) {
        newDate = new Date(this.year, this.month, this.chosenDate.getDate() + 7);   
    } else if (this.monthViewSelected()) {
        newDate = new Date(this.year, this.month + 1, 1);
    } else if (this.timelineViewSelected()) {
        newDate = this.chosenDate.duplicate();
        this.timelineView.nextOrPrev(true);
        return;
    }
    this.dateChooser.setData(newDate);
    this.setChosenDate(newDate);
},

//> @method calendar.previous()
// Move to the previous day, week, month, or timeline range depending on which tab is selected.
//
// @visibility calendar
//<
previous : function () {
    var newDate;
    //var tab = this.mainView.selectedTab;
    if (this.dayViewSelected()) {
        newDate = new Date(this.year, this.month, this.chosenDate.getDate() - 1);
        // if hiding weekends, find next non-weekend day
        if (!this.showWeekends) {
            var wends = Date.getWeekendDays();
            for (var i = 0; i < wends.length; i++) {
                if (wends.contains(newDate.getDay())) newDate.setDate(newDate.getDate() - 1);
            }
        }
    } else if (this.weekViewSelected()) {
        newDate = new Date(this.year, this.month, this.chosenDate.getDate() - 7);   
    } else if (this.monthViewSelected()) {
        newDate = new Date(this.year, this.month - 1, 1);
    } else if (this.timelineViewSelected()) {
        newDate = this.chosenDate.duplicate();
        this.timelineView.nextOrPrev(false);
        return;
    }
    this.dateChooser.setData(newDate);
    this.setChosenDate(newDate);
},

dataArrived : function () { 
    return true;   
},

// override draw to add the calendar navigation bar floating above the mainView tabbar
draw : function (a, b, c, d) {
    
    this.invokeSuper(isc.Calendar, "draw", a, b, c, d);
    
    if (isc.ResultSet && isc.isA.ResultSet(this.data) && this.dataSource) {
        this.observe(this.data, "dataArrived", "observer.dataArrived(arguments[0], arguments[1])");    
    }
    if (this.mainView.isA("TabSet")) {
        if (this.showControlsBar != false) {
            this.mainView.addChild(this.controlsBar);
            this.controlsBar.moveAbove(this.mainView.tabBar);
        }
    }
    if (!isc.isA.TabSet(this.mainView)) {
        // if there's no tabset then only one view is visible - in that case, call 
        // setChosenDate() to have any SGWT override of getDateLabelText() called correctly
        this.setChosenDate(this.chosenDate);
    } else {
        this.setDateLabel();
    }
},  

/*
getSnapGapPixels : function (snapGap, grid) {
    if (!snapGap) return snapGap;
    // get percentage of snapGap in relation to 30 minutes, the length in minutes of a row, and 
    // multiply by row height to get pixels
    return Math.floor((snapGap / 30) * grid.getRowHeight(null, 0));
},
*/
_getTabs : function () {
    var nTabs = [];
    // viewName used by calendar internals, so don't put into defaults
    if (this.showDayView != false) {
        this.dayView = this.createAutoChild("dayView", 
            { baseStyle: this.baseStyle, viewName: "day", cellHeight: this.rowHeight } );
        nTabs.add({title: this.dayViewTitle, pane: this.dayView, viewName: "day" });
    }
    if (this.showWeekView != false) {
        this.weekView = this.createAutoChild("weekView", 
            {_isWeek: true, baseStyle: this.baseStyle, viewName: "week", cellHeight: this.rowHeight } );
        nTabs.add({title: this.weekViewTitle, pane: this.weekView, viewName: "week" });
    }
    if (this.showMonthView != false) {
        this.monthView = this.createAutoChild("monthView", 
            {baseStyle: this.baseStyle, viewName: "month",
             bodyConstructor:"MonthScheduleBody"} );
        nTabs.add({title: this.monthViewTitle, pane: this.monthView, viewName: "month" });
    }
    if (this.showTimelineView != false) {
        this.timelineView = this.createAutoChild("timelineView",
            {baseStyle: this.baseStyle, viewName: "timeline" } );
        nTabs.add({title: this.timelineViewTitle, pane: this.timelineView, viewName: "timeline" });
    }
    return nTabs;
},

_createTabSet : function (tabsArray) {
    // if there is only one view displayed, don't use tabs
    if (tabsArray.length > 1) {
        this.mainView = this.createAutoChild("mainView", {           
            tabs: tabsArray,
            tabSelected : function (tabNum, tabPane, ID, tab) {
                // store selected view name for later use, in day/week/monthViewSelected functions
                this.creator._selectedViewName = tabPane.viewName;
                this.creator.setDateLabel();
                if (this.creator.getSelectedView()._needsRefresh) {
                    this.creator.refreshSelectedView();
                }
                this.creator.currentViewChanged(tabPane.viewName);
            }
            
        } );
        // set the default tab according to currentViewName if defined
        if (this.currentViewName) {
            var tabToSelect = tabsArray.find("viewName", this.currentViewName);
            if (tabToSelect) this.mainView.selectTab(tabToSelect);
        }
    } else {
        this.mainView = tabsArray[0].pane;     
    }   
},

getLaneMap : function () {
    if (!this.isTimeline() && !this.showDayLanes) return {};

    var data = this.showDayLanes ? this.lanes : this.timelineView.data,
        laneMap = {}
    ;
    for (var i=0; i<data.length; i++) {
        var name = data[i].name || data[i][this.laneNameField],
            title = data[i].title || name
        ;
        laneMap[name] = title;
    }
    return laneMap;
},

// create the content of the calendar
createChildren : function () {
    
    
    // main tabbed view
    var mvTabs = this._getTabs();
 
    this._createTabSet(mvTabs);
    var tbButtonDim = 20;
    if (this.showControlsBar != false) {
        // dateLabel
        this.dateLabel = this.createAutoChild("dateLabel");
        // addEventButton
        this.addEventButton = this.createAutoChild("addEventButton", {
            click: function () {
                var cal = this.creator;
                var currView = cal.getSelectedView();
                
                cal.eventDialog.event = null;
                cal.eventDialog.isNewEvent = true;
                cal.eventDialog.items[0].createFields(); //false);
      
                var sDate = new Date(), 
                    eDate = null,
                    pickedDate = cal.chosenDate.duplicate();
                // if dayView is chosen, set dialog date to chosen date
                if (currView.isDayView()) {
                    sDate = pickedDate;
                // if weekView, set dialog to first day of chosen week unless
                // today is greater
                } else if (currView.isWeekView()) {
                    if (cal.chosenWeekStart.getTime() > sDate.getTime()) {
                        sDate = cal.chosenWeekStart.duplicate();    
                    }
                    // if hiding weekends, find next non-weekend day
                    if (!this.showWeekends) {
                        var wends = Date.getWeekendDays();
                        for (var i = 0; i < wends.length; i++) {
                            if (wends.contains(sDate.getDay())) sDate.setDate(sDate.getDate() + 1);
                        }
                    }
                    sDate.setMinutes(0);
                    // move event to next day if now is end of day
                    if (sDate.getHours() > 22) {
                        sDate.setDate(sDate.getDate() + 1);
                        sDate.setHours(0);
                    } // otherwise move to next hour
                    else sDate.setHours(sDate.getHours() + 1);
                // if monthView, set dialog to first day of chosen month unless
                // today is greater
                } else if (currView.isMonthView()) {
                    pickedDate.setDate(1);
                    if (pickedDate.getTime() > sDate.getTime()) sDate = pickedDate; 
                } else if (cal.isTimeline()) {
                    var tl = cal.timelineView,
                        dates = tl.getVisibleDateRange();
                    sDate = dates[0];

                    eDate = sDate.duplicate();
                    eDate = tl.addUnits(eDate, 1, cal.timelineGranularity);
                 }

                cal.eventDialog.setDate(sDate, eDate);
                // place the dialog at the left edge of the calendar, right below the button itself
                cal.eventDialog.setPageLeft(cal.getPageLeft());
                cal.eventDialog.setPageTop(this.getPageTop() + this.getVisibleHeight());
               
                cal.eventDialog.show();
            }
        } );
        
        // datePickerButton
        this.datePickerButton = this.createAutoChild("datePickerButton", {
            click: function () {
                var cal = this.creator;
                if (this._datePicker) {
                    // redraw the datePicker, positioning is already taken care of   
                    this._datePicker.setData(cal.chosenDate);
                    this._datePicker.draw();
                } else {
                    this._datePicker = isc[cal.dateChooserConstructor].create({
                        calendar: this.creator, autoDraw: false,
                        showCancelButton: true, autoClose: true,
                        disableWeekends: this.creator.disableWeekends,
                        firstDayOfWeek: this.creator.firstDayOfWeek,
                        showWeekends: this.creator.showWeekends,
                        // override dateClick to change the selected day
                        dateClick : function (year, month, day) {
                            var nDate = new Date(year, month, day);
                            this.setData(nDate);
                            // change the chosen date via the dateChooser
                            this.calendar.dateChooser.dateClick(year, month, day);
                            this.close();
                        }
                    });
                    this._datePicker.setData(cal.chosenDate);
                    cal.addChild(this._datePicker);

                    this._datePicker.placeNextTo(this, "bottom", true);         
                }
            }
        } );
        
        this.previousButton = this.createAutoChild("previousButton", {});
        
        this.nextButton = this.createAutoChild("nextButton", {});
    }
    var cbMems = [];
    if (this.showPreviousButton != false) cbMems.add(this.previousButton);
    if (this.showDateLabel != false) cbMems.add(this.dateLabel);
    if (this.showDatePickerButton != false) cbMems.add(this.datePickerButton);
    if (this.canCreateEvents && this.showAddEventButton != false) cbMems.add(this.addEventButton);
    if (this.showNextButton != false) cbMems.add(this.nextButton);
    // set up calendar navigation controls
    if (this.showControlsBar != false) {
        this.controlsBar = this.createAutoChild("controlsBar", { 
            members: cbMems
        });
    }
    //if (mvTabs.length == 1) this.controlsBar.layoutAlign = "center";
    
    var cal = this;
    
    // date chooser
    this.dateChooser = this.createAutoChild("dateChooser", {
            disableWeekends: this.disableWeekends,
            showWeekends: this.showWeekends,
            chosenDate: this.chosenDate, 
            month: this.month,
            year: this.year,
            // override dateClick to change the selected day
            dateClick : function (year, month, day) {
                var nDate = new Date(year, month, day);
                this.setData(nDate);
               
                // recalculate displayed events
                this.creator.setChosenDate(nDate);    
            },
            
            showPrevYear : function () {
                this.year--;
                this.dateClick(this.year, this.month, this.chosenDate.getDate());
            },
        
            showNextYear : function () {
                this.year++;
                this.dateClick(this.year, this.month, this.chosenDate.getDate());
            },
            
            showPrevMonth : function () {
                if (--this.month == -1) {
                    this.month = 11;
                    this.year--;
                }
                this.dateClick(this.year, this.month, 1);
            },
        
            showNextMonth : function () {
                if (++this.month == 12) {
                    this.month = 0;
                    this.year++;
                }
                this.dateClick(this.year, this.month, 1);
            }
    } );
    
    // layout for date chooser and main calendar view
    if (!this.children) this.children = [];
    var mainMembers = [];
    var subMembers = [];
    //if (this.canCreateEvents) subMembers.add(this.addEventButton);
    subMembers.add(this.dateChooser);
    if (this.showDateChooser) {
        mainMembers.add(isc.VLayout.create({
                    autoDraw:false,
                    width: "20%",
                    membersMargin: 10,
                    layoutTopMargin: 10,
                    members: subMembers
                }));
    }
    
    if (this.mainView.isA("TabSet")) {
        mainMembers.add(this.mainView);   
    // center align controlsBar
    } else {
        if (this.showControlsBar != false) {
             
            var controlsBarContainer = isc.HLayout.create({
                    autoDraw: false,
                    height: this.controlsBar.getVisibleHeight(),
                    width: "100%"
            });
            
            controlsBarContainer.addMember(isc.LayoutSpacer.create({autoDraw:false, width:"*"}));
            controlsBarContainer.addMember(this.controlsBar);
            controlsBarContainer.addMember(isc.LayoutSpacer.create({autoDraw:false, width:"*"}));
            mainMembers.add(isc.VLayout.create({
                    autoDraw:false,
                    members: [controlsBarContainer, this.mainView]
                }));
        } else {
             mainMembers.add(this.mainView);
        }
    }
    
    this.children.add(
        isc.HLayout.create({ 
            autoDraw:false,
            width: "100%",
            height: "100%",
            members:mainMembers
           
        })
    );
    
    this.setDateLabel();    
}, // end createChildren

createEditors : function () {
    var cal = this;
    
    // quick event dialog
    this.eventDialog = this.createAutoChild("eventDialog", {
 
        items: [
            isc.DynamicForm.create({
                autoDraw: false,
                padding:4,
                calendar: this,
                saveOnEnter: true,
                useAllDataSourceFields: true,
                numCols: 2,
                colWidths: [80, "*"],
                _internalFields : [cal.nameField, cal.laneNameField],
                getCustomValues : function () {
                    if (!this.calendar.eventDialogFields) return;
                    var internalValues = this._internalFields;
                    var fields = this.calendar.eventDialogFields;
                    var cFields = {};
                    for (var i = 0; i < fields.length; i++) {
                        var fld = fields[i];
                        if (fld.name && !internalValues.contains(fld.name)) {
                            cFields[fld.name] = this.getValue(fld.name);        
                        }
                    }
                    return cFields;    
                },
                setCustomValues : function (values) {
                    if (!this.calendar.eventDialogFields) return;
                    var internalValues = this._internalFields;
                    var fields = this.calendar.eventDialogFields;
                    for (var i = 0; i < fields.length; i++) {
                        var fld = fields[i];
                        if (fld.name && !internalValues.contains(fld.name)) {
                            this.setValue(fld.name, values[fld.name]);        
                        }
                    }
                          
                },
                createFields : function (isEvent) {
                    var cal = this.calendar,
                        isNewEvent = cal.eventDialog.isNewEvent,
                        nameType = !isNewEvent ? "staticText" : "text",
                        laneType = !isNewEvent ? "staticText" : "select",
                        showLane = cal.isTimeline() || (cal.showDayLanes && cal.dayViewSelected())
                    ;

                    // set up default fields
                    var fieldList = [
                        {name: cal.nameField, title: cal.eventNameFieldTitle, type: nameType, 
                            width: 250 
                        },
                        {name: cal.laneNameField, title: cal.eventLaneFieldTitle, type: laneType, width: 150, 
                                valueMap: cal.getLaneMap(),
                                showIf: showLane ? "true" : "false" 
                        },
                        {name: "save", title: cal.saveButtonTitle, editorType: "SubmitItem", endRow: false},
                        {name: "details", title: cal.detailsButtonTitle, type: "button", startRow: false,
                            click : function (form, item) {
                                var cal = form.calendar,
                                    isNew = cal.eventDialog.isNewEvent,
                                    event = cal.eventDialog.event,
                                    name = form.getValue(cal.nameField),
                                    laneName = form.getValue(cal.laneNameField)
                                ;
                                if (isNew) {
                                    event[cal.nameField] = name;
                                    if (laneName) event[cal.laneNameField] = laneName;
                                }
                                form.calendar._showEventEditor(event, isNew);
                            }
                        }
                    ];
                    if (!isNewEvent) fieldList.removeAt(2);
                    // create internal dataSource
                    var dialogDS = isc.DataSource.create({
                        addGlobalId: false,
                        fields: fieldList
                    });
                    // set dataSource then fields...other way around doesn't work
                    this.setDataSource(dialogDS);
                    this.setFields(isc.shallowClone(this.calendar.eventDialogFields));
                },

                submit : function () {
                    var cal = this.calendar,
                        isNewEvent = cal.eventDialog.isNewEvent,
                        evt = isNewEvent ? cal.eventDialog.event : null, 
                        sdate = cal.eventDialog.currentStart,
                        edate = cal.eventDialog.currentEnd,
                        lane
                    ;

                    if (!this.validate()) return;

                    if (cal.isTimeline() || (cal.dayViewSelected() && cal.showDayLanes)) {
                        lane = this.getItem(cal.laneNameField).getValue();
                    }

                    cal._fromEventDialog = true;
                    if (!isNewEvent) { // event window clicked, so update 
                        cal.updateEvent(evt, sdate, edate, 
                            this.getItem(cal.nameField).getValue(), evt[cal.descriptionField],
                            this.getCustomValues(), true, lane);
                    } else { // create new event
                        cal.addEvent(sdate, edate, this.getItem(cal.nameField).getValue(),
                            "", this.getCustomValues(), lane, true);
                    }
                    cal.eventDialog.hide();
                }
            })
        ],

        setDate : function (startDate, endDate) {
            var cal = this.creator;
            if (!endDate) {
                // handle the case where where the startDate is 11:30 pm...in this case only 
                // do a 1/2 hour long event
                if (startDate.getHours() == 23 
                        && startDate.getMinutes() == (60 - cal.getMinutesPerRow())) {
                    endDate = new Date(startDate.getFullYear(), startDate.getMonth(),
                    startDate.getDate() + 1); 
                } else {
                    endDate = new Date(startDate.getFullYear(), startDate.getMonth(),
                        startDate.getDate(), startDate.getHours() + 1, startDate.getMinutes());
                }
            }
            this.setTitle(cal._getEventDialogTitle(startDate, endDate));
            this.currentStart = startDate;
            this.currentEnd = endDate;
            this.items[0].getItem(cal.nameField).setValue("");
        },
        
        setLane : function (lane) {
            var cal = this.creator;
            if (isc.isA.Number(lane)) lane = cal.lanes[lane].name;
            this.items[0].getItem(cal.laneNameField).setValue(lane);
        },

        // eventDialog_setEvent
        setEvent : function (event) {
            this.event = event;
    
            var theForm = this.items[0],
                cal = this.creator
            ;
            
            // if we have custom fields, clear errors and set those custom fields
            if (cal.eventDialogFields) {
                theForm.clearErrors(true);
                theForm.setCustomValues(event);
            }
            this.setDate(event[cal.startDateField], event[cal.endDateField]);
            
            theForm.setValues(event);
        },
        
        closeClick : function () {
            this.Super('closeClick');
            // clear selections on close of dialog
            if (this.creator.dayView) this.creator.dayView.clearSelection();
            if (this.creator.weekView) this.creator.weekView.clearSelection();
        },

        show : function () {
            if (this.creator.showQuickEventDialog) {
                
                if (!this.isDrawn()) this.draw();
                this.Super('show');
                this.items[0].getItem(this.creator.nameField).focusInItem();
            } else {
                this.creator.showEventEditor(this.event);    
            }
        },
        
        hide : function () {
            this.Super('hide');
            this.moveTo(0, 0);
        }
        
    } );
    
    // event editor form
    this.eventEditor = this.createAutoChild("eventEditor", {   
        useAllDataSourceFields: true,
        titleWidth: 80,
        initWidget : function () {
            // invoke initWidget here rather than at the end of the function, or else we multiple
            // log warnings of form fields being clobbered
            this.invokeSuper(isc.DynamicForm, "initWidget", arguments);
   
            this.timeFormat = this.creator.timeFormat;
            var fieldList = [],
                cal = this.creator,
                editStyle = cal.getDateEditingStyle()
            ;

            this._internalFields.addList([cal.nameField, cal.descriptionField, 
                cal.startDateField, cal.endDateField]
            );

            if (cal.timelineView || cal.showDayLanes) {
                // if the calendar allows laneEditing, show the lane picker - if a given event
                // is canEditLane: false, the picker will be disabled
                var laneMap = cal.getLaneMap(),
                    field = { name: cal.laneNameField, title: cal.eventLaneFieldTitle, type: "select", 
                        valueMap: laneMap, endRow: true, colSpan: "*"
                    }
                ;
                fieldList.add(field);
            }

            if (editStyle == "date" || editStyle == "datetime") {
                fieldList.addList([
                    { name: cal.startDateField, title: cal.eventStartDateFieldTitle, type: editStyle, endRow: true },
                    { name: cal.endDateField, title: cal.eventEndDateFieldTitle, type: editStyle, endRow: true },
                    { name: "invalidDate", type: "blurb", colSpan: 4, visible: false,
                        defaultValue: cal.invalidDateMessage, 
                        cellStyle: this.errorStyle || "formCellError", endRow: true
                    }
                ]);
            } else if (editStyle == "time") {
                this.numCols = 4;
                this.setColWidths([this.titleWidth, 60, 60, "*"]);
                fieldList.addList([
                    {name: "startHours", title: cal.eventStartDateFieldTitle, type: "integer", width: 60,
                     editorType: "select", valueMap: this.getTimeValues("hours")},
                    {name: "startMinutes", showTitle: false, type: "integer", width: 60,
                     editorType: "select", valueMap: this.getTimeValues("minutes")},
                    {name: "startAMPM", showTitle: false, type: "select", width: 60,
                     valueMap: this.getTimeValues(), endRow: true},
                    {name: "invalidDate", type: "blurb", colSpan: 4, visible: false,
                     defaultValue: cal.invalidDateMessage, 
                     cellStyle: this.errorStyle || "formCellError", endRow: true},
                    {name: "endHours", title: cal.eventEndDateFieldTitle, type: "integer", width: 60,
                     editorType: "select", valueMap: this.getTimeValues("hours")},
                    {name: "endMinutes", showTitle: false, type: "integer", width: 60,
                     editorType: "select", valueMap: this.getTimeValues("minutes")},
                    {name: "endAMPM", showTitle: false, type: "select", width: 60,
                     valueMap: this.getTimeValues(), endRow: true}
                ]);
            }

            fieldList.addList([
                {name: cal.nameField, title: cal.eventNameFieldTitle, type: "text", colSpan: "*", width: "*"},
                {name: cal.descriptionField, title: cal.eventDescriptionFieldTitle, type: "textArea", colSpan: "*", 
                    width: "*", height: 50}
            ]);

            // create an internal ds and bind to it so that the default fields can be 
            // overridden. See forms->validation->customized binding in the feature explorer
            var editorDS = isc.DataSource.create({
                addGlobalId: false,
                fields: fieldList
            });
            // only dataSource then fields seems to work
            this.setDataSource(editorDS);
            this.setFields(isc.shallowClone(this.creator.eventEditorFields));
        },
        getTimeValues : function (type, startTime) {
            if (!startTime) startTime = 0;
            var obj = {};
            if (type == "hours") {
                for (var i = startTime; i < 12; i++) {
                    obj[(i + 1) + ""] = (i + 1);                     
                }
            } else if (type == "minutes") {
                for (var i = 0; i < 60; i++) {
                    // stringify the minutes
                    var stringMin = i < 10 ? "0" + i : "" + i;
                    obj[i + ""] = stringMin;
                }
            } else {
                obj["am"] = "am";
                obj["pm"] = "pm";
            }
            
            return obj;
        },
        _internalFields : ["startHours", "startMinutes", "startAMPM", "endHours", 
                "endMinutes", "endAMPM" ],
        getCustomValues : function () {
            if (!this.creator.eventEditorFields) return;
            var cal = this.creator,
                internalValues = this._internalFields;
            var fields = this.creator.eventEditorFields;
            var cFields = {};
            for (var i = 0; i < fields.length; i++) {
                var fld = fields[i];
                if (fld.name && !internalValues.contains(fld.name)) {
                    cFields[fld.name] = this.getValue(fld.name);        
                }
            }
            return cFields;    
        },
        setCustomValues : function (values) {
            if (!this.creator.eventEditorFields) return;
            var internalValues = this._internalFields;
            var fields = this.creator.eventEditorFields;
            for (var i = 0; i < fields.length; i++) {
                var fld = fields[i];
                if (fld.name && !internalValues.contains(fld.name)) {
                    this.setValue(fld.name, values[fld.name]);        
                }
            }
                  
        }
    } );
    
    // event editor layout
    this.eventEditorLayout = this.createAutoChild("eventEditorLayout", { 
        items: [
            this.eventEditor,
            isc.HLayout.create({
                membersMargin: 10, 
                layoutMargin: 10,
                autoDraw:false, 
                members: [
                    isc.IButton.create({autoDraw: false, title: this.saveButtonTitle, calendar: this,
                        click : function () {
                            this.calendar.addEventOrUpdateEventFields();
                        }
                    }),
                    isc.IButton.create({autoDraw: false, title: this.cancelButtonTitle, calendar:this,
                        click: function () {
                            this.calendar.eventEditorLayout.hide();    
                        }
                    })
                ]
            })
        ],

        // eventEditorLayout_setDate
        setDate : function (startDate, endDate, eventName, lane) {
            if (!eventName) eventName = "";
            if (!endDate) {
                endDate = new Date(startDate.getFullYear(), startDate.getMonth(),
                    startDate.getDate(), startDate.getHours() + 1, startDate.getMinutes());
            }
            var cal = this.creator;
            this.setTitle(cal._getEventDialogTitle(startDate, endDate));
            this.currentStart = startDate;
            this.currentEnd = endDate;

            // cater for dateEditingStyle
            var editStyle = cal.getDateEditingStyle(),
                form = this.items[0]
            ;
            if (editStyle == "date" || editStyle == "datetime") {
                form.getItem(cal.startDateField).setValue(startDate.duplicate()); 
                form.getItem(cal.endDateField).setValue(endDate.duplicate());
            } else if (editStyle == "time") {
                form.getItem("startHours").setValue(this.getHours(startDate.getHours())); 
                form.getItem("endHours").setValue(this.getHours(endDate.getHours()));
                form.getItem("startMinutes").setValue(startDate.getMinutes());
                form.getItem("endMinutes").setValue(endDate.getMinutes());
                if (!cal.twentyFourHourTime) {
                    form.getItem("startAMPM").setValue(this.getAMPM(startDate.getHours()));
                    form.getItem("endAMPM").setValue(this.getAMPM(endDate.getHours()));
                }
            }
            form.setValue(cal.nameField, eventName);
            form.setValue(cal.descriptionField, "");
            form.setValue(cal.laneNameField, lane);
        },
        
        getHours : function (hour) {
            if (this.creator.twentyFourHourTime) return hour;
            else return this.creator._to12HrNotation(hour);
        },
        
        getAMPM : function (hour) {
            if (hour < 12) return "am";
            else return "pm";
        },
        
        // eventEditorLayout_setEvent
        setEvent : function (event) {
            var form = this.items[0],
                cal = this.creator,
                view = this.view,
                laneSwitcher = form.getItem(cal.laneNameField)
            ;

            this.event = event;
            // if we have custom fields, clear errors and set those custom fields
            if (cal.eventEditorFields) {
                form.clearErrors(true);
                form.setCustomValues(event);
            }
            this.setDate(event[cal.startDateField], event[cal.endDateField]);
            if (laneSwitcher) {
                laneSwitcher.setValueMap(cal.getLaneMap());
                laneSwitcher.setValue(event[cal.laneNameField]);
                laneSwitcher.setDisabled(event[cal.canEditLaneField] == false);
                var showSwitcher = view.isTimelineView() || (view.isDayView() && cal.showDayLanes);
                if (showSwitcher) laneSwitcher.show();
                else laneSwitcher.hide();
            }
            form.setValue(cal.nameField, event[cal.nameField]);
            form.setValue(cal.descriptionField, event[cal.descriptionField]);
            this.originalStart = isc.clone(this.currentStart);
            this.originalEnd = isc.clone(this.currentEnd);
        },

        hide : function () {
            this.Super('hide');
            // clear any selection that's been made
            if (this.creator.dayView) this.creator.dayView.clearSelection();
            if (this.creator.weekView) this.creator.weekView.clearSelection();
            // clear any errors
            this.creator.eventEditor.hideItem("invalidDate");
        },
       
        sizeMe : function () {
            this.setWidth(this.creator.mainView.getVisibleWidth());
            this.setHeight(this.creator.mainView.getVisibleHeight()); 
            this.setLeft(this.creator.mainView.getLeft());
        }
    });
    
    this.eventEditorLayout.hide();
},

addEventOrUpdateEventFields : function () {
    var cal = this,
        isNewEvent = cal.eventEditorLayout.isNewEvent,
        evt = cal.eventEditorLayout.event,
        form = cal.eventEditor,
        editStyle = cal.getDateEditingStyle()
    ;

    if (editStyle == "date" || editStyle == "datetime") {
        var start = form.getValue(cal.startDateField),
            end = form.getValue(cal.endDateField),
            laneName
        ;

        if (end < start) {
            form.showItem("invalidDate");
            return false;
        }

        // run validation so rules for custom fields added by the developer are enforced
        if (!form.validate()) return false;

        cal.eventEditorLayout.currentStart = start;
        cal.eventEditorLayout.currentEnd = end;

        cal.eventEditorLayout.hide();

        // lanes now apply to timelines (rows) and to dayView with showDayLanes: true (columns)
        if (cal.isTimeline() || (cal.dayViewSelected() && cal.showDayLanes) && cal.canEditLane) {
            laneName = form.getValue(cal.laneNameField);
        }

        cal._fromEventEditor = true;
        if (!isNewEvent) {
            cal.updateEvent(evt, start, end,
                            form.getValue(cal.nameField), form.getValue(cal.descriptionField), 
                            form.getCustomValues(), true, laneName
                           );            
        } else {
            cal.addEvent(start, end,
                         form.getValue(cal.nameField), form.getValue(cal.descriptionField),
                         form.getCustomValues(), laneName, true);
        }

    } else if (editStyle == "time") {
        var sHrs = form.getValue("startHours"),
            eHrs = form.getValue("endHours"),
            sMins = form.getValue("startMinutes"), 
            eMins = form.getValue("endMinutes"),
            sAMPM, eAMPM
        ;

        if (!cal.twentyFourHourTime) {
            sAMPM = form.getValue("startAMPM");
            eAMPM = form.getValue("endAMPM");
            sHrs = cal._to24HourNotation(sHrs, sAMPM);
            eHrs = cal._to24HourNotation(eHrs, eAMPM);
            // handle the case where end date is 12am, which is valid, as this
            // is considered the end of the current day
            if (eHrs == 0) eHrs = 24;
        }
        // check for invalid times
        if (!(sHrs < eHrs || (sHrs == eHrs && sMins < eMins))) {
            form.showItem("invalidDate");
            return false;
        }

        // run validation so rules for custom fields added by the
        // developer are enforced
        if (!form.validate()) return false;

        cal.eventEditorLayout.hide();

        var sdate = cal.eventEditorLayout.currentStart,
            edate = cal.eventEditorLayout.currentEnd;

        // Differing calendar dates:
        // For an end date of midnight we end up with the start date
        // and the end date being on different days.
        // Cases we need to handle:
        // - stored start/end date are the same day, and user has
        //   moved end time forward to midnight.
        //   * call 'setHour(24)' - will auto increment date value
        // - stored start/end date are different days (so end is midnight)
        //   and user has moved end date back to a time before midnight.
        //   * call 'setDate()' to decrease the end date to the same day,
        //     then apply the new time via setHour()
        // - stored start end date are different (end date is midnight)
        //   and user has left it selected
        //   * no need to actually setHours on end date but if we do,
        //     convert the '24' set up above to zero so we don't
        //     increment the date an additional day.
        if (edate.getDate() > sdate.getDate()) {
            if (eHrs == 24) eHrs = 0;
            else {
                edate.setDate(sdate.getDate());
            }
        }
        sdate.setHours(sHrs);
        sdate.setMinutes(sMins);
        edate.setHours(eHrs);
        edate.setMinutes(eMins);
        
        cal._fromEventEditor = true;
        
        if (!isNewEvent) { // event window clicked, so update 
            var sStartDate=cal.eventEditorLayout.originalStart,
                sEndDate=cal.eventEditorLayout.originalEnd;

            cal.updateEvent(evt, sdate, edate,
                         form.getValue(cal.nameField), form.getValue(cal.descriptionField),
                         form.getCustomValues(), true, form.getValue(cal.laneNameField));

        } else {
            cal.addEvent(sdate, edate,
                         form.getValue(cal.nameField), form.getValue(cal.descriptionField),
                         form.getCustomValues(), form.getValue(cal.laneNameField), true);
        }
    }
    return true;
},

// sets the date label of the calendar. Called whenever the chosenDate or selected tab
// changes
setDateLabel : function () {
    if (!this.dateLabel) return;

    var content="",
        startDate = this.chosenDate,
        endDate = null,
        viewName = this.getCurrentViewName()
    ;

    if (viewName == "day") { // day tab
    } else if (viewName == "week") { // week tab
        var dateRange = this._getWeekRange();
        startDate = dateRange[0];
        endDate = dateRange[1];
    } else if (viewName == "month") { // month tab
        startDate = isc.DateUtil.getStartOf(startDate, "M");
        endDate = isc.DateUtil.getEndOf(startDate, "M");
    } else if (viewName == "timeline") {
        var ebtView = this.timelineView;
        startDate = ebtView.startDate;
        endDate = ebtView.endDate;
    }
    content = this.getDateLabelText(viewName, startDate, endDate);
    this.dateLabel.setContents(content);
},

//> @method calendar.getDateLabelText()
// Returns the text to display between the navigation buttons above the Calendar - indicates 
// the visible date range.
// @param viewName (String) one of "day", "week", "month" or "timeline"
// @param startDate (Date) the start of the visible date range
// @param [endDate] (Date) the optional end of the visible date range
// @return (String) a formatted date or date-range string appropriate to the passed view
// @visibility calendar
//<
getDateLabelText : function (viewName, startDate, endDate) {
    var result = "";
    if (viewName == "day") { // day tab
        result = "<b>" + Date.getFormattedDateRangeString(startDate) + "</b>";
    } else if (viewName == "week") { // week tab
        result = "<b>" + Date.getFormattedDateRangeString(startDate, endDate) + "</b>";
    } else if (viewName == "month") { // month tab
        result = "<b>" + startDate.getShortMonthName() + " " + startDate.getFullYear() + "</b>";
    } else if (viewName == "timeline") {
        var ebtView = this.timelineView;
        result = "<b>" + ebtView.formatDateForDisplay(startDate) + "</b> through <b>" +
                ebtView.formatDateForDisplay(endDate) + "</b>";
    }
    return result;
},

_getWeekRange : function () {
    var start = this.chosenWeekStart.duplicate();
    var end = this.chosenWeekEnd.duplicate();
    if (!this.showWeekends) {
        var wEnds = Date.getWeekendDays();
        var numDays = 7 - wEnds.length;
        // first augment start so its not sitting on a weekend
        while (wEnds.contains(start.getDay())) {
            start.setDate(start.getDate() + 1);    
        }
        // number of days to add to numDays when calculating end day
        // The idea is to add weekdays length to start date to arrive at end date. If there are 
        // weekends in between, however, we need to add those days to the end date as well
        var addDays = 0, cursorDate = start.duplicate(); 
        for (var i = 0; i < numDays; i++) {
            if (wEnds.contains(cursorDate.getDay())) addDays++;
            cursorDate.setDate(cursorDate.getDate() + 1);
        }
        end = start.duplicate();
        //isc.logWarn('here:' + [numDays, addDays]);
        end.setDate(end.getDate() + (numDays - 1) + addDays);
    }
    return [start, end];
},

dayViewSelected : function () {
    if (this.mainView && !this.mainView.isA("TabSet")) return this.mainView.viewName == "day";
    else return this._selectedViewName == "day";  
},

weekViewSelected : function () {
    if (this.mainView && !this.mainView.isA("TabSet")) return this.mainView.viewName == "week";
    else return this._selectedViewName == "week";
},

monthViewSelected : function () {
    if (this.mainView && !this.mainView.isA("TabSet")) return this.mainView.viewName == "month";
    else return this._selectedViewName == "month";
},

timelineViewSelected : function () {
    if (this.mainView && !this.mainView.isA("TabSet")) return this.mainView.viewName == "timeline";
    else return this._selectedViewName == "timeline";    
},

//> @method calendar.showEventDialog()
// Open the Quick Event dialog showing minimal information about an existing 
// +link{CalendarEvent, event}.
// <P>
// The +link{calendar.startDateField, startDate} field on the event is used to calculate the 
// display location for the dialog.
// <P>
// If this method is called when the Event Dialog is already showing another event, and if 
// changes have been made, a confirmation dialog is displayed and editing of the new event 
// is cancelled unless confirmed.
// 
// @param event (CalendarEvent) the event to show in the Editor
// @visibility calendar
//<
showEventDialog : function (event) {
    if (!event) {
        this.logWarn("showEventDialog called with no event - returning.");
        return;
    }
    this._showEventDialog(event, false);
},

//> @method calendar.showNewEventDialog()
// Open the Quick Event dialog to begin editing a new +link{CalendarEvent, event}.
// <P>
// If passed, the event parameter is used as defaults for the new event - in addition, the 
// event's +link{calendar.startDateField, startDate}, and its 
// +link{calendar.laneNameField, lane}, for timeline events, are used to calculate the 
// display location for the dialog.
// <P>
// If this method is called when the Event Dialog is already showing another event, and if 
// changes have been made, a confirmation dialog is displayed and editing of the new event 
// is cancelled unless confirmed.
// 
// @param [event] (CalendarEvent) defaults for the new event 
// @visibility calendar
//<
showNewEventDialog : function (event) {
    event = event || {};
    this._showEventDialog(event, true);
},

// Displays the event entry/edit dialog at row/col position calculated from the start/endDates
// set on the passed event object
_showEventDialog : function (event, isNewEvent) {

    //TODO: if there's an existing dialog with changes, intercept it with a confirmation dialog
    
    var startDate = event[this.startDateField] || new Date();
    var endDate = event[this.endDateField];

    var currentView = this.getSelectedView(),
        eventWindow = currentView.isMonthView() ? null : this._findEventWindow(event, currentView),
        rowNum, colNum, coords
    ;

    // no event window means that an empty slot was clicked, so show dialog for creating a 
    // new event
    if (!eventWindow) {
        if (this.eventEditorLayout) {
            this.eventEditorLayout.event = event;
            this.eventEditorLayout.isNewEvent = isNewEvent;
        }

        // clear out the stored eventWindow and store the passed event - determine whether 
        // it's new via eventDialog.isNewEvent
        this.eventDialog.eventWindow = null;
        this.eventDialog.event = event;
        this.eventDialog.isNewEvent = isNewEvent;
        this.eventDialog.items[0].createFields();

        var sDate = startDate, 
            eDate = endDate;

        event[this.startDateField] = sDate;

        if (currentView.isMonthView()) { // get date for clicked month day cell
            var sHrs = new Date();
            sHrs = sHrs.getHours();
            // take an hour off so the event stays within the day
            if (sHrs > 22) sHrs -= 1;
            sDate.setHours(sHrs);
            event[this.startDateField] = sDate;
        }
        if (currentView.isTimelineView()) {
            var tl = this.timelineView;

            rowNum = tl.getEventLaneIndex(event);
            colNum = tl.body.getEventColumn(tl.getDateLeftOffset(sDate));
            // assume a default length of one unit of the timelineGranularity for new events
            eDate = endDate || this.getDateFromPoint(tl.getDateLeftOffset(sDate) + tl.getColumnWidth(colNum));
            // set the lane
            this.eventDialog.setLane(rowNum);
        } else {
            if (currentView.isMonthView()) {
                rowNum = currentView.getEventRow();
                colNum = currentView.getEventColumn();
                // assume a default length of one hour (two rows) for new Calendar events
                eDate = endDate || this.getCellDate(rowNum, colNum, currentView);
            } else {
                rowNum = startDate.getHours() * this.getRowsPerHour(currentView);
                rowNum += Math.floor(startDate.getMinutes() / this.getMinutesPerRow());
                if (this.showDayLanes && currentView.isDayView()) {
                    colNum = currentView.getEventLaneIndex(event);
                } else {
                    colNum = currentView.getColFromDate(startDate);
                }
                // assume a default length of one hour (two rows) for new Calendar events
                eDate = endDate || this.getCellDate(rowNum, colNum, currentView);
            }
        }

        event[this.endDateField] = eDate;

        this.eventDialog.setEvent(event);
    } else { // otherwise show dialog for clicked event
        if (currentView.isTimelineView()) {
            rowNum = currentView.getEventLaneIndex(event);
            colNum = currentView.body.getEventColumn(currentView.getDateLeftOffset(startDate));
        } else if (currentView.isDayView() || currentView.isWeekView()) {
            rowNum = startDate.getHours() * this.getRowsPerHour(currentView);
            rowNum += Math.floor(startDate.getMinutes() / this.getMinutesPerRow());
            colNum = currentView.getColFromDate(startDate);
        }
        this.eventDialog.eventWindow = eventWindow;
        this.eventDialog.isNewEvent = false;
        this.eventDialog.items[0].createFields();
        this.eventDialog.setEvent(eventWindow.event);    
        
        coords = [eventWindow.getPageLeft(), eventWindow.getPageTop()];
    }

    // ensure the dialog is drawn before placing it
    
    //this.eventDialog.moveTo(0, -10000);
    //this.eventDialog.show();

    if (!coords) coords = currentView.body.getCellPageRect(rowNum, colNum);

    //TODO: don't let the window show outside of the body
    
    this.eventDialog.placeNear(coords[0], coords[1]);
    this.eventDialog.show();
    // bringToFront() needs to be put on a timer, else it fails to actually bring the
    // eventDialog to the front
    isc.Timer.setTimeout(this.ID + ".eventDialog.bringToFront()");
},

//> @method calendar.showEventEditor()
// Show an Event Editor for the passed event.  Event Editor's fill the Calendar and allow 
// for editing of the built-in Event fields, like +link{nameField, name} and 
// +link{descriptionField, description}, as well as any 
// custom fields supplied via +link{calendar.eventDialogFields}.
// <P>
// If no event is passed, a new Event with no default values is created via 
// +link{showNewEventEditor}.
// 
// @param [event] (CalendarEvent) an existing event to show in the Editor
// @visibility calendar
//<
showEventEditor : function (event) {
    if (event) this._showEventEditor(event);
    else this.showNewEventEditor(null);
},

//> @method calendar.showNewEventEditor()
// Show an Event Editor for a new event.  If an +link{CalendarEvent, event} is passed as the 
// parameter, it is used as defaults for the new event.
// 
// @param [event] (CalendarEvent) defaults for the new event to show in the Editor
// @visibility calendar
//<
showNewEventEditor : function (event) {
    this._showEventEditor(event, true);
},

newEventEditorWindowTitle: "New Event",
_showEventEditor : function (event, isNewEvent) {
    
    if (!this.eventEditorLayout.isDrawn()) this.eventEditorLayout.draw();
    this.eventEditorLayout.setWidth(this.mainView.getVisibleWidth());
    this.eventEditorLayout.setHeight(this.mainView.getVisibleHeight());
    // move the eventEditor to cover the mainView only
    
    this.eventEditorLayout.setPageLeft(this.mainView.getPageLeft());
    this.eventEditorLayout.setPageTop(this.getPageTop());

    this.eventEditorLayout.isNewEvent = isNewEvent;
    
    this.eventEditorLayout.view = this.getSelectedView();
    
    //if (this.eventEditorFields) this.eventEditor.reset();
    if (event) {
        this.eventEditorLayout.setEvent(event);
    } else {
        this.eventEditor.clearValues();
        this.eventEditorLayout.setTitle(this.newEventEditorWindowTitle);
        if (this.eventDialog && this.eventDialog.isVisible()) {
            // pass any custom field values through to the event editor
            if (this.eventEditorFields) {
                this.eventEditorLayout.items[0].setCustomValues(this.eventDialog.items[0].getCustomValues());
            }
            var eventName = this.eventDialog.items[0].getValue(this.nameField);
            var laneItem = this.eventDialog.items[0].getItem(this.laneNameField);
            var lane = laneItem ? laneItem.getValue() : null;
            
            var startDate = new Date();
            
            this.eventEditorLayout.setDate(
                startDate,
                this.eventDialog.currentEnd, 
                eventName, lane
            );
        }
    }

    this.eventDialog.hide();

    this.eventEditorLayout.show();
},

_getEventDialogTitle : function (startDate, endDate) {
    var days = Date.getShortDayNames(),
        months = Date.getShortMonthNames(),
        sTime = isc.Time.toTime(startDate, this.timeFormatter, true),
        eTime = isc.Time.toTime(endDate, this.timeFormatter, true),
        result
    ;
    if (this.isTimeline()) {
        var differentDays = startDate.getDay() != endDate.getDay();

        if (differentDays) { // Saturday, Feb 28, 10:00 - Sunday, March 1, 10:00
            result = days[startDate.getDay()] + ", " + months[startDate.getMonth()] + " " +
                        startDate.getDate() + ", " + sTime + " - " + 
                     days[endDate.getDay()] + ", " + months[endDate.getMonth()] + " " + 
                        endDate.getDate() + ", " + eTime
            ;
            return result;
        }
    }

    var timeStr = sTime + " - " + eTime;
    
    return days[startDate.getDay()] + ", " + months[startDate.getMonth()]
        + " " + startDate.getDate() + ", " + timeStr ;
}, 

_to12HrNotation : function (hour) {
    if (hour == 0) return 12;
    else if (hour < 13) return hour;
    else return hour - 12;
},

_to24HourNotation : function (hour, ampmString) {
    // make sure we're dealing with an int
    hour = parseInt(hour);
    if (ampmString.toLowerCase() == "am" && hour == 12) { 
        return 0;
    } else if (ampmString.toLowerCase() == "pm" && hour < 12) {
        return hour + 12;    
    } else {
        return hour;    
    }
},

_getCellCSSText : function (grid, record, rowNum, colNum) {
    var cal = this,
        currDate = cal.getCellDate(rowNum, colNum, grid),
        result = cal.getDateCSSText(currDate, rowNum, colNum, grid)
    ;

    // an override of getDateCSSText() returned something - return that
    if (result) return result;

    // if the date is the same as the calendar's chosenDate and todayBackgroundColor is set,
    // return CSS for that
    var dateComp = isc.Date.compareLogicalDates(currDate, new Date());
    if ((dateComp !== false && dateComp == 0) && cal.todayBackgroundColor) {
        return "background-color:" + cal.todayBackgroundColor + ";";
    }
    
    return null;
},

//> @method calendar.getDateCSSText()
// Return CSS text for styling the cell associated with the passed date and/or rowNum & colNum,
// which will be applied in addition to the CSS class for the cell, as overrides.
// <p>
// "CSS text" means semicolon-separated style settings, suitable for inclusion in a CSS
// stylesheet or in a STYLE attribute of an HTML element.
//
// @see getDateStyle()
//
// @param date (Date) the date to return CSS text for
// @param rowNum (Integer) the row number to get the CSS for
// @param colNum (Integer) the column number to get the date for
// @param viewer (ListGrid) the ListGrid used by the current Calendar view
// @return (String) CSS text for the associated cell
//
// @visibility calendar
//<
getDateCSSText : function (date, rowNum, colNum, viewer) {
    return null;
},

//> @method calendar.getDateStyle()
// Return the CSS styleName for the cell associated with the passed date and/or rowNum & colNum.
//
// @see getDateCSSText()
//
// @param date (Date) the date to return CSS text for
// @param rowNum (Integer) the row number to get the CSS for
// @param colNum (Integer) the column number to get the date for
// @param viewer (ListGrid) the ListGrid used by the current Calendar view
// @return (CSSStyleName) CSS style for the cell associated with the passed date
//
// @visibility calendar
//<
getDateStyle : function (date, rowNum, colNum, viewer) {
    return null;
},

//> @method calendar.getCellDate()
// Return the Date instance associated with the passed co-ordinates in the current view.  If
// the cell at the passed co-ordinates is not a date-cell, returns null.
// <P>
// To determine the date at a more specific point within a cell, see +link{getDateFromPoint}.
//
// @param rowNum (Integer) the row number to get the date for
// @param colNum (Integer) the column number to get the date for
// @return (Date) the date, if any, associated with the passed co-ords in the current view
//
// @visibility calendar
//<
getCellDate : function (rowNum, colNum, view) {
    var retDate;
    view = view || this.getSelectedView();
    
    var frozenFieldCount = view.frozenFields ? view.frozenFields.length : 0;
    
    if (view.isDayView() || view.isWeekView() || view.isTimelineView()) {
        var col = colNum - frozenFieldCount;
        retDate = col >= 0 ? view.getCellDate(rowNum, col) : null;
    } else if (view.isMonthView()) {
        if (colNum >= view.getFields().length)
            colNum = view.getFields().length-1;
        var rec = view.data.get(rowNum);
        // get the index into the record from the field at colNum.
        var dIndex = view.getField(colNum)._dayIndex;
        if (rec && rec["date" + dIndex] != null) {
            retDate = rec["date" + dIndex].duplicate();
            // return midnight of the given day
            retDate.setHours(0); retDate.setMinutes(0); retDate.setSeconds(0);
        }
    } else {
        return;
    }
    return retDate;
},

//> @method calendar.getDateFromPoint()
// Returns a Date instance representing the point at the passed offsets into the body of the 
// current view.
// <P>
// If snapOffsets is passed as false, returns the date representing the 
// exact position of the passed offsets.  If unset or passed as true, returns the date at the 
// nearest eventSnapGap to the left, for +link{Timeline}s, or above for +link{dayView, day} 
// and +link{weekView, week} views.
// <P>
// If neither x nor y offsets are passed, assumes them from the last mouse event.
// <P>
// If the cell at the eventual offsets is not a date-cell, returns null.
// <P>
// Note that, for the +link{monthView, month view}, this method is functionally equivalent to 
// +link{getCellDate}, which determines the date associated with a cell, without the additional 
// offset precision offered here.
//
// @param [x] (Integer) the x offset into the body of the selected view - non-functional for 
//                      the +link{dayView, day view}.  If this param and "y" are both unset, 
//                      assumes both offsets from the last mouse event.
// @param [y] (Integer) the y offset into the body of the selected view - non-functional for the
//                            +link{timelineView, timeline view}.  If this param and "x" are 
//                            both unset, assumes both offsets from the last mouse event.
// @param [snapOffsets] (Boolean) whether to snap the offsets to the nearest eventSnapGap - if
//                                 unset, the default is true
// @return (Date) the date, if any, associated with the passed co-ords in the current view
//
// @visibility calendar
//<
getDateFromPoint : function (x, y, snapOffsets, view) {

    view = view || this.getSelectedView();

    // snapOffsets unset, assume true
    if (snapOffsets == null) snapOffsets = true;

    if (view.getDateFromPoint) return view.getDateFromPoint(x, y, null, snapOffsets);

    if (x == null && y == null) {
        // no offsets passed, return the date at the last mouse event position
        x = view.body.getOffsetX();
        y = view.body.getOffsetY();
    }

    var colNum = view.body.getEventColumn(x), 
        rowNum = view.body.getEventRow(y),
        retDate
    ;

    if (view.isMonthView()) {
        retDate = this.getCellDate(rowNum, colNum, view);
    } else {
        return;
    }

    return retDate;
},

getDateLeftOffset : function (date, view) {
    if (view && view.getDateLeftOffset) return view.getDateLeftOffset(date);
},

getView : function (viewName) {
    if (viewName == "day") return this.dayView;
    if (viewName == "week") return this.weekView;
    if (viewName == "month") return this.monthView;
    if (viewName == "timeline") return this.timelineView;
},

monthViewEventClick : function (rowNum, colNum, eventIndex) {
    var events = this.monthView.getEvents(rowNum, colNum);
    var evt = events[eventIndex];
    if (this.eventClick(evt, "month")) this.showEventEditor(evt);   
},

//> @method calendar.currentViewChanged()
// Notification that fires whenever the current view changes via the 
// +link{mainView, mainView tabset}.
//
// @param viewName (String) the name of the current view after the change
// @return (HTML) HTML to display
// 
// @visibility calendar
//<
currentViewChanged : function (viewName) {
},

//> @method calendar.getDayBodyHTML()
// Return the HTML to be shown in the body of a day in the month view.
// <P>
// Default is to render a series of links that call +link{eventClick} to provide details
// and/or an editing interface for the events.
// <P>
// <code>getDayBodyHTML()</code> is not called for days outside of the current month if
// +link{showOtherDays} is false.
//
// @param date (Date) JavaScript Date object representing this day
// @param events (Array of CalendarEvent) events that fall on this day
// @param calendar (Calendar) the calendar itself
// @param rowNum (int) the row number to which the parameter date belongs
// @param colNum (int) the column number to which the parameter date belongs
// @return (HTML) HTML to display
// 
// @group monthViewFormatting
// @visibility calendar
//<
getDayBodyHTML : function (date, events, calendar, rowNum, colNum) {
    
    var day = date.getDay();
    
    var evtArr = events, lineHeight = 15,
        record = this.monthView.data ? this.monthView.data[1] : null,
        rHeight = this.monthView.getRowHeight(record, 1);
    var retVal = "";
    for (var i = 0; i < evtArr.length; i++) {
        var eTime = isc.Time.toTime(evtArr[i][this.startDateField], this.timeFormatter, true) + " ";
        if (this.canEditEvent(evtArr[i])) {
            // when clicked, call the the editEvent method of this calendar, passing the
            // row, column, and position of the event in this cell's event array
            var template  = "<a href='javascript:" + this.ID + ".monthViewEventClick(" + 
                rowNum + "," + colNum + "," + i + ");' class='" 
                + this.calMonthEventLinkStyle + "'>";
                
            retVal += template + eTime + evtArr[i][this.nameField] + "</a><br/>";
        } else {
            retVal += eTime + evtArr[i][this.nameField] + "<br/>";      
        }
        if ((i + 3) * lineHeight > rHeight) break; 
    }
    if (i < evtArr.length - 1) {
        retVal += "+ " + (evtArr.length - 1 - i) + " more...";
    }
    return retVal;
},

//> @method calendar.getMonthViewHoverHTML()
// This method returns the hover HTML to be displayed when the user hovers over a cell
// displayed in the calendar month view tab.
// <P>
// Default implementation will display a list of the events occurring on the date the user is
// hovering over. Override for custom behavior. Note that returning null will suppress the
// hover altogether.
//
// @param date (Date) Date the user is hovering over
// @param events (Array of CalendarEvent) array of events occurring on the current date. May be empty.
// @return (HTML) HTML string to display
//
// @visibility calendar
//<
getMonthViewHoverHTML : function(currDate, evtArr) {
    if(evtArr!=null) {
        var retVal = "";
        for (var i = 0; i < evtArr.length; i++) {
            var target = this.creator || this;
            var eTime = isc.Time.toTime(evtArr[i][target.startDateField], target.timeFormatter, true);
            retVal += eTime + " " + evtArr[i][target.nameField] + "<br/>";
        }
        return retVal;
    }
},

// @method calendar.getDayHeaderHTML()
// Return the HTML to be shown in the header of a day in the month view.
// <P>
// Default is to render just the day of the month, as a number.
//
// @param date (Date) JavaScript Date object representing this day
// @param events (Array of CalendarEvent) events that fall on this day
// @param calendar (Calendar) the calendar itself
// @return (HTML) HTML to show in the header of a day in the month view
// 
// @group monthViewFormatting
// @visibility calendar
//<
getDayHeaderHTML : function (date, events, calendar, rowNum, colNum) {
    //isc.logWarn('here:' + [date.getDate(), rowNum, colNum]);
    return date.getDate();
},

//> @method calendar.dayBodyClick()
// Called when the body area of a day in the month view is clicked on, outside of any links
// to a particular event.
// <P>
// By default, if the user can add events, shows a dialog for adding a new event for that
// day.  Return false to cancel this action.
// <P>
// Not called if the day falls outside the current month and +link{showOtherDays} is false.
//
// @param date (Date) JavaScript Date object representing this day
// @param events (Array of CalendarEvent) events that fall on this day
// @param calendar (Calendar) the calendar itself
// @param rowNum (Integer) the row number to which the parameter date belongs
// @param colNum (Integer) the column number to which the parameter date belongs
// @return (boolean) false to cancel the default action
//
// @group monthViewEvents
// @visibility calendar
//<
dayBodyClick : function (date, events, calendar, rowNum, colNum) {
   return true;
},

//> @method calendar.dayHeaderClick()
// Called when the header area of a day in the month view is clicked on.
// <P>
// By default, moves to the day tab and shows the clicked days events.
// Return false to cancel this action.
// <P>
// Not called if the day falls outside the current month and +link{showOtherDays} is false.
//
// @param date (Date) JavaScript Date object representing this day
// @param events (Array of CalendarEvent) events that fall on this day
// @param calendar (Calendar) the calendar itself
// @param rowNum (int) the row number to which the parameter date belongs
// @param colNum (int) the column number to which the parameter date belongs
// @return (boolean) return false to cancel the action
//
// @group monthViewEvents
// @visibility calendar
//<
dayHeaderClick : function (date, events, calendar, rowNum, colNum) {
    return true;    
},

//> @method calendar.eventChanged()
// Notification fired whenever a user changes an event, whether by dragging the event or by
// editing it in a dialog.
// <P>
// In a calendar with a DataSource, eventChanged() fires <b>after</b> the updated event has
// been successfully saved to the server
//
// @param event (CalendarEvent) the event that changed
// @group monthViewEvents
// @visibility calendar
//<

//> @method calendar.eventRemoved()
// Notification fired whenever a user removes an event
// <P>
// In a calendar with a DataSource, eventRemoved() fires <b>after</b> the event has
// been successfully removed from the server
//
// @param event (CalendarEvent) the event that was removed
// @group monthViewEvents
// @visibility calendar
//<

//> @method calendar.eventAdded()
// Notification fired whenever a user adds an event.
// <P>
// In a calendar with a DataSource, eventAdded() fires <b>after</b> the event has
// been successfully added to the server
//
// @param event (CalendarEvent) the event that was added
// @group monthViewEvents
// @visibility calendar
//<

//> @method calendar.eventClick()
// Called whenever an event is clicked on in the day, week or month views.
// <P>
// By default a dialog appears showing details for the event, and offering the ability to
// edit events which are editable.  Return false to cancel the default action. This is a good
// place to, for example, show a completely customized event dialog instead of the default one.
//
// @param event (CalendarEvent) event that was clicked on
// @param viewName (String) view where the event was clicked on: "day", "week", "month", "timeline"
// @return (boolean) false to cancel the default action
//
// @group monthViewEvents
// @visibility calendar
//<
eventClick : function (event, viewName) {
    return true;
},

//> @method calendar.eventRemoveClick()
// Called whenever the close icon of an event is clicked within the day or week view. Return
// false to cancel the removal, or true to allow it.
// <P>
// Implement this method to do something like, for example, showing a confirmation dialog 
// before an event is removed.
//
// @param event (CalendarEvent) event that was clicked on
// @param viewName (String) view where the event was clicked on: "day", "week" or "month"
// @return (boolean) false to cancel the removal
//
// @group monthViewEvents
// @visibility calendar
//<
eventRemoveClick : function (event, viewName) {
    return true;    
},

//> @method calendar.eventMoved()
// Called when an event is moved via dragging by a user.  Return false to disallow the move.
// @param newDate (Date) new start date and time that the event is being moved to
// @param event (CalendarEvent) the event as it will be after this movement
// @param newLane (String) the lane into which the event was moved
// @return (boolean) return false to disallow the move.
//
// @group monthViewEvents
// @visibility calendar
//<
eventMoved : function (newDate, event, newLane) {
    return true;    
},

//> @method calendar.eventResized()
// Called when an event is resized via dragging by a user.  The passed date value is the new
// *end* date for the event, since resizing can only be performed on the bottom edge of an event
// in normal calendar views.
// @param newDate (Date) new end date and time that event is being resized to
// @param event (CalendarEvent) the event as it will be after this resize
// @return (boolean) return false to disallow the resize
//
// @group monthViewEvents
// @visibility calendar
//<
eventResized : function (newDate, event) {
    return true;    
},

//> @method calendar.timelineEventMoved()
// Called when a Timeline event is moved via dragging by a user.  Return false to disallow the 
// move.
// @param event (CalendarEvent) the event that was moved
// @param startDate (Date) new start date of the passed event 
// @param endDate (Date) new end date of the passed event 
// @param lane (Lane) the Lane in which this event has been dropped
// @return (boolean) return false to disallow the move.
//
// @visibility calendar
//<
timelineEventMoved : function (event, startDate, endDate, lane) {
    return true;    
},

//> @method calendar.timelineEventResized()
// Called when a Timeline event is resized via dragging by a user.  Return false to disallow 
// the resize.
// @param event (CalendarEvent) the event that was resized
// @param startDate (Date) new start date of the passed event 
// @param endDate (Date) new end date of the passed event 
// @return (boolean) return false to disallow the resize
//
// @visibility calendar
//<
timelineEventResized : function (event, startDate, endDate) {
    return true;    
},

// helper method, gets a valid date with respect to the eventSnapGap and starting point of 
// referenceDate. Used in eventWindow dragRepositionStop and dragResizeStop to ensure a valid
// date every time.
getValidSnapDate : function (referenceDate, snapDate) {
    if (this.isTimeline()) {
        
    } else {
        // the formula for getting the snapDate is: 
        // round((snapDate as minutes - offset) / snapGap) * snapGap + offset
        // where offset = reference date as minutes mod snapGap
        var snapGap = this.eventSnapGap;
    
        var offset = ((referenceDate.getHours() * 60) + referenceDate.getMinutes()) % snapGap;
    
        var dateMinutes = (snapDate.getHours() * 60) + snapDate.getMinutes();
        var gapsInDate = Math.round((dateMinutes - offset) / snapGap);
    
        var totMins = (gapsInDate * snapGap) + offset;
   
        var hrs = Math.floor(totMins / 60), mins = totMins % 60;
        snapDate.setHours(hrs);
        snapDate.setMinutes(mins);
    }

    return snapDate;    
},

//> @method calendar.selectTab()
// Selects the calendar view in the passed tab number.
//
// @param tabnum (number) the index of the tab to select
// @visibility calendar
//<
selectTab : function (tabnum) {
    if (this.mainView && this.mainView.isA("TabSet") && this.mainView.tabs.getLength() > tabnum) {
        this.mainView.selectTab(tabnum);
        this.refreshSelectedView();
        return true;        
    } else {
        return false;    
    }
},

// override parentResized to resize the eventEditorLayout as well
parentResized : function () {
    //isc.logWarn('calendar parentResized');
     this.Super('parentResized', arguments);
     // only resize the eventEditorLayout if its shown
     if (this.eventEditorLayout.isVisible()) this.eventEditorLayout.sizeMe();
},

//> @method calendar.dateChanged()
// Fires whenever the user changes the current date, including picking a specific date or
// navigating to a new week or month.
// @visibility external
//<
dateChanged : function () {
    return true;
},

//> @method calendar.getActiveDay()
// Gets the day of the week (0-6) that the mouse is currently over.
//
// @return (integer) the day that the mouse is currently over
// @see calendar.getActiveTime()
// @visibility external
//<
getActiveDay : function () {
    var activeTime = this.getActiveTime();
    if (activeTime) return activeTime.getDay();
},

//> @method calendar.getActiveTime()
// Gets a date object representing the date over which the mouse is hovering for the current
// selected view. For month view, the time will be set to midnight of the active day. For day
// and week views, the time will be the rounded to the closest half hour relative to the mouse
// position.
// @return (Date) the date that the mouse is over
// @visibility external
//<
getActiveTime : function () {
    var EH = this.ns.EH,
    currView = this.getSelectedView();
    var rowNum = currView.getEventRow();
    var colNum = currView.getEventColumn();
    return this.getCellDate(rowNum, colNum, currView);
},

//> @method calendar.setTimelineRange()
// Sets the range over which the timeline will display events.
// <P>
// If the <code>end</code> parameter is not passed, the end date of the range will default to 
// +link{Calendar.defaultTimelineColumnSpan, 20} columns of the current 
// +link{Calendar.timelineGranularity, granularity} following the start date.
// 
// @param start (Date) start of range
// @param [end] (date) end of range
// @visibility external
//<
setTimelineRange : function (start, end, gran, units, callback) {
    if (this.timelineView) this.timelineView.setTimelineRange(start, end, gran, units);        
    if (callback) this.fireCallback(callback);
},

// get event length in minutes
getEventLength : function (startDate, endDate) {
    var minDiff = (endDate.getTime() - startDate.getTime()) / (1000 * 60);
    return minDiff;
},

canEditEventLane : function (event, view) {
    var canEdit = event[this.canEditLaneField] != null ? 
            event[this.canEditLaneField] : this.canEditLane != false;
    return canEdit;
},


checkForOverlap : function (view, eventWin, event, startDate, endDate, lane) {
    var overlapTest = {}, 
        startField = this.startDateField, 
        endField = this.endDateField
    ;

    overlapTest[startField] = startDate.duplicate();
    overlapTest[endField] = endDate.duplicate();
    overlapTest[this.laneNameField] = lane;

    var events = lane ? this.data.findAll(this.laneNameField, lane) : this.data;

    var overlappingEvents = view.findOverlappingEvents(event, overlapTest, false, (lane != null), events);
    
    if (overlappingEvents.length == 0) {
        // return false, meaning no overlap detected
        return false;
    // for now just return if overlapping more than one event
    } else if (overlappingEvents.length > 1) {
        //isc.logWarn("overlap detected:" + overlappingEvents.length);
        return true;
    } else {
        var overlapped = overlappingEvents[0];
       
        // case 1: drop event partially overlaps existing event to the left, so try to
        // drop event to the left
        if ((this.equalDatesOverlap == false ? 
                endDate > overlapped[startField] : endDate >= overlapped[startField])
                && startDate < overlapped[startField]
                )
        {
            // set end date to be overlapped event start date, less one minute
            endDate = overlapped[startField].duplicate();
            
            //endDate.setMinutes(endDate.getMinutes() - 1);
            // put the start date back by however many minutes the event is long
            startDate = endDate.duplicate();
            startDate.setMinutes(startDate.getMinutes() - eventWin.getEventLength());
            //isc.logWarn('left overlap:' + [startDate]);
            return [startDate, endDate];
        // case 2: drop event partially overlaps existing event to the right, so try to
        // drop event to the right
        } else if ((this.equalDatesOverlap == false ? 
                startDate < overlapped[endField] : startDate <= overlapped[endField])
                && endDate > overlapped[endField]
                )
        {
            // set start date to be overlapped event end date, plus one minute
            startDate = overlapped[endField].duplicate();
            //startDate.setMinutes(startDate.getMinutes() + 1);
            // put the start date back by however many minutes the event is long
            endDate = startDate.duplicate();
            endDate.setMinutes(endDate.getMinutes() + eventWin.getEventLength());
            //isc.logWarn('right overlap:' + [overlapped.id, overlapped.end, startDate, endDate]);
            return [startDate, endDate];
        // other cases: for now don't allow drops where drop event completely encompasses 
        // or is encompassed by another event
        } else {
            return true;
        }
       
    }
},

viewProps: {
    getCalendar : function () {
        return this.creator;
    },
    isSelectedView : function () {
        return this.calendar.getSelectedViewName() == this.viewName;
    },
    isTimelineView : function () {
        return this.viewName == "timeline";
    },
    isDayView : function () {
        return this.viewName == "day";
    },
    isWeekView : function () {
        return this.viewName == "week";
    },
    isMonthView : function () {
        return this.viewName == "month";
    }
},

gridProps: {
    getPrintHTML : function (printProperties, callback) {
        printProperties = isc.addProperties({}, printProperties);
        
        this.body.printChildrenAbsolutelyPositioned = true;
        
        var cal = this.getCalendar(),
            view = this.viewName,
            isTimeline = this.isTimelineView(),
            isWeek = this._isWeek,
            isDay = view.isDayView(),
            isMonth = view.isMonthView()
        ;

        if (isMonth) return;
        
        var fields = this.getFields(),
            data = this.getData(),
            output = isc.StringBuffer.create(),
            totalWidth = 0,
            fieldWidths = null,
            _this = this
        ;
        
        if (isTimeline) {
            fieldWidths = fields.map(function (item) {
                return _this.getFieldWidth(item);
            });
            //isc.logWarn("field.width returns: " + fields.getProperty("width") + "\n" +
            //    getFieldWidth() returns: " + fieldWidths);

            //totalWidth = fieldWidths.sum();
            if (this.frozenFields) totalWidth += this.frozenBody._fieldWidths.sum();
        } else {
            totalWidth = this.body._fieldWidths.sum();
            if (this.frozenBody) totalWidth += this.frozenBody._fieldWidths.sum();
        }
        
        //totalWidth -= ((this.getFields().length-1) * 4);

        var rowStart = "<TR",
            rowEnd = "</TR>",
            gt = ">",
            heightAttr = " HEIGHT=",
            valignAttr = " VALIGN="
        ;


        var bodyVOffset = 40;

        output.append("<TABLE WIDTH=", totalWidth, " style='position: absolute; top:", bodyVOffset, ";'>");

        if (this.showHeader) {
            // don't generate column-headers for dayView
            output.append(this.getPrintHeaders(0, this.fields.length));
        }

        // absolutely position the body and events after the header
        bodyVOffset += this.getHeaderHeight();

        output.append("<TABLE role='presentation' border='' class:'", this.baseStyle, "' ",
            "style='borderSpacing:0; position: absolute; top:", bodyVOffset, 
            "; z-index: -1' cellpadding='0' cellspacing='0'>"
        );

        for (var i=0; i<data.length; i++) {
            output.append(rowStart, heightAttr, this.getRowHeight(i), gt);
            for (var j=0; j<fields.length; j++) {
                var value = this.getCellValue(data[i], i, j);
                output.append("<TD padding=0 class='", this.getCellStyle(data[i], i, j), "' ",
                    "width='", this.getFieldWidth(j) + (j == 0 ? 2 : 4), "px' ",
                    "style='margin: 0px; padding: 0px; ",
                    "border-width: 0px 1px 1px 0px; ",
                    "border-bottom: 1px solid #ABABAB; border-right: 1px solid #ABABAB; ",
                    "border-top: none; border-left: none;'>"
                );
                output.append(this.getCellValue(data[i], i, j) || "&nbsp;");
                output.append("</TD>");
            }
            output.append(rowEnd);
        }

        var events = [];
        if (cal.isTimeline()) {
            events = this.getVisibleEvents();
            for (var i=0; i<events.length; i++) {
                var event = events.get(i), 
                    winId = cal.getEventCanvasID(this, event), 
                    eWin = window[winId],
                    props = isc.addProperties({}, printProperties, {i: i})
                ;
                if (eWin) {
                    output.append(eWin.getPrintHTML(printProperties, callback));
                }
            }
        } else {
            events = this.body.children;
            for (var i=0; i<events.length; i++) {
                if (!events[i].isEventCanvas) continue;
                output.append(events[i].getPrintHTML(printProperties, callback));
            }
        }

        output.append("</TR>");
        output.append("</TABLE>");
        output.append("</TABLE>");
        
        var result = output.toString();

        return result;
        },

        getPrintHeaders : function (startCol, endCol) {
        
        var defaultAlign = (this.isRTL() ? isc.Canvas.LEFT : isc.Canvas.RIGHT),
            printHeaderStyle = this.printHeaderStyle || this.headerBaseStyle,
            HTML;

        
        // We support arbitrarily nested, asymmetrical header-spans - these require
        // some slightly tricky logic so use a conditional to avoid this if not required.
        if (this.headerSpans) {

            // Step 1: We'll build an array of "logical columns" in this format:
            // [field1], [innerHeader1], [topHeader]
            // [field2], [innerHeader2], [topHeader]
            // [field3], [topHeader2]
            // Each array contains an entry for each row we'll write out (each header
            // span the field is embedded in, plus the field).
            // Note that the top row of HTML will be the last entry in each sub-array and
            // the bottom row will be the first entry (the field should appear below 
            // all its headers).
            // Also note we have repeats in here - we'll handle this by applying colSpans
            // to the generated HTML - and that the column arrays will be different lengths
            // due to different depth of nesting of header spans - we'll handle this by 
            // applying rowSpans.
            var logicalColumns = [],
                numRows = 1;
            
            for (var i = startCol; i < endCol; i++) {
                var field = this.getField(i);
                logicalColumns[i] = [field];
                
                var span = this.spanMap[field.name];
                
                // build a logical column from the fieldName up to the top span
                // (Note that we will have the same span in multiple cols, which is ok)
                while (span != null) {
                    logicalColumns[i].add(span);
                    span = span.parentSpan;
                }
                // Remember how deep the deepest nested column is - this is required to
                // allow us to apply numRows.
                numRows = Math.max(logicalColumns[i].length, numRows);
            }

            // Step 2: Iterate through the column arrays starting at the last entry
            // (outermost header)
            HTML = [];

            for (var i = numRows-1; i >= 0; i--) {
                HTML[HTML.length] = "<TR>";
                
                var lastEntry = null,
                    colSpanSlot = null;
                for (var ii = startCol; ii < endCol; ii++) {
                    var rowSpan = 1, colSpan = 1;
                    // When we reach the first entry in the array we'll be looking at a field
                    var isField = (i == 0);

                    var entry = logicalColumns[ii][i];
                    
                    
                    if (entry == "spanned") {
                        continue;
                    }
                    var minDepth,
                        spanningColNum = ii,
                        spannedColOffsets = [];
                        
                    // set colSpan to zero. We'll increment in the loop below
                    colSpan = 0;
                    
                    while (spanningColNum < endCol) {
                        var entryToTest = null,
                            foundMismatch = false;
                        for (var offset = 0; (i-offset) >= 0; offset++) {
                            entryToTest = logicalColumns[spanningColNum][i-offset];

                            if (entryToTest != null) {
                                // If we originally hit a null entry, pick up the first
                                // non null entry so we have something to actually write out.
                                if (entry == null) {
                                    entry = entryToTest;
                                    minDepth = offset;
                                    if (i-offset == 0) {
                                        isField = true;
                                    }
                                }
                                if (entry == entryToTest) {
                                    spannedColOffsets[colSpan] = offset;
                                    minDepth = Math.min(offset, minDepth);
                                } else {
                                    foundMismatch = true;
                                }
                                break;                                
                            } 
                        }
                        if (foundMismatch) {
                            break;
                        }
                        spanningColNum ++;
                        
                        colSpan++;
                    }
                    
                    // set rowSpan for the cell based on how deep we had to
                    // go to find a real entry (shift from zero to 1-based)
                    if (minDepth != null) {
                        rowSpan = minDepth+1;
                    }
                    
                    
                       
                    // For each column this entry spans, add markers indicating that
                    // we're handling this via TD with rowSpan and colSpan set (and
                    // clear out duplicate entries).
                    for (var spannedCols = 0; spannedCols < spannedColOffsets.length; 
                        spannedCols++) 
                    {
                    
                        var logicalColArray = logicalColumns[spannedCols + ii],
                            offset = spannedColOffsets[spannedCols];
                            
                        for (var spannedRows = 0; spannedRows <= offset; spannedRows++) {
                            
                            if (spannedCols == 0 && spannedRows == 0) {
                                logicalColArray[i-spannedRows] = entry;
                            } else if (spannedRows <= minDepth) {
                                logicalColArray[i - spannedRows] = "spanned";
                            } else {
                                logicalColArray[i - spannedRows] = null;
                            }
                        }
                    }
                    
                    // We don't expect to ever end up with a null entry - not sure
                    // how this could happen but log a warning
                    if (entry == null) {
                        this.logWarn("Error in getPrintHeaders() - unable to generate " +
                            "print header HTML from this component's specified headerSpans");
                    }
                
                    var align = "center",
                        cellValue;
                    
                    if (isField) {
                        align = entry.align || defaultAlign;
                        cellValue = this.getHeaderButtonTitle(entry.masterIndex);
                    } else {
                        cellValue = entry.title;
                    }

                    var cellStart = HTML.length;
                    
                    HTML[HTML.length] = "<TD class='";
                    HTML[HTML.length] = printHeaderStyle;
                    HTML[HTML.length] = "' align='";
                    HTML[HTML.length] = "center";
                    HTML[HTML.length] = "' rowSpan='";
                    HTML[HTML.length] = rowSpan;
                    HTML[HTML.length] = "' colSpan='";
                    HTML[HTML.length] = colSpan;
                    HTML[HTML.length] = "' width=";
                    HTML[HTML.length] = this.getFieldWidth(entry);
                    HTML[HTML.length] = ">";
                    HTML[HTML.length] = cellValue;
                    HTML[HTML.length] = "</TD>";
                    
                }
                HTML[HTML.length] = "</TR>";
            }
        //         this.logWarn("\n\nGenerated print header HTML (including spans):" + HTML.join(""));
            
        } else {
        
            HTML = ["<TR>"];
                
            var cellStartHTML = ["<TD CLASS=", printHeaderStyle,
                                 " ALIGN="].join("");

            // Just iterate through the fields once, then assemble the HTML and return it.
            if (this.frozenBody) {
                for (var colNum = 0; colNum < this.frozenFields.length; colNum++) {
                    var field = this.frozenBody.fields[colNum];
                    if (!field) continue;
                    var align = field.align || defaultAlign;
                    //var width = field.width || this.getFieldWidth(colNum);
                    var width = this.getFieldWidth(colNum);
                    HTML.addList([cellStartHTML, align, " width=" + width + ">",
                                        this.getHeaderButtonTitle(field.masterIndex), "</TD>"]);
                }
            }

            // Just iterate through the fields once, then assemble the HTML and return it.
            for (var colNum = startCol; colNum < endCol; colNum++) {
                var field = this.body.fields[colNum];
                if (!field) continue;
                var align = field.align || defaultAlign;
                //var width = field.width || this.getFieldWidth(colNum);
                var width = this.getFieldWidth(colNum);
                HTML.addList([cellStartHTML, align, " width=" + width + ">",
                                    this.getHeaderButtonTitle(field.masterIndex), "</TD>"]);
            }
            
            // Output the standard header row
            HTML[HTML.length] = "</TR>";
        }
        return HTML.join(isc.emptyString);
    },

    eventDragTargetDefaults: {
        _constructor: "Canvas",
        border: "1px dashed red",
        width:1, height: 1,
        dragAppearance: "target",
        dragTarget: this,
        visibility: "hidden",
        keepInParentRect: true,
        setView : function (view) {
            this.view = view;
        },
        getEventPadding : function () {
            var cal = this.eventWin.calendar;
            if (this.view.isTimelineView()) return cal.timelineEventPadding;
            return 0;
        },
        getDragProps : function () {
            var eventWin = this.eventWin,
                cal = eventWin.calendar,
                view = this.view,
                isTimeline = view.isTimelineView(),
                gr = view.body,
                eventRow = gr.getEventRow(),
                rowTop = gr.getRowTop(eventRow),
                rowHeight = gr.getRowHeight(eventRow),
                eventLeft = view.getEventLeft(eventWin.event) + 1,
                eventCol = gr.getEventColumn(eventLeft),
                columnLeft = gr.getColumnLeft(eventCol),
                columnWidth = gr.getColumnWidth(eventCol),
                offsetX = gr.getOffsetX() - eventWin.getLeft(),
                offsetY = gr.getOffsetY() - eventWin.getTop()
            ;

            return {
                top: isTimeline ? rowTop : eventWin.getTop(),
                bottom: isTimeline ? rowTop + rowHeight : eventWin.getTop() + eventWin.getVisibleHeight(),
                left: isTimeline ? eventWin.getLeft() : columnLeft,
                right: isTimeline ? eventWin.getLeft() + eventWin.getVisibleWidth() : columnLeft + columnWidth,
                startOffsetX: offsetX,
                startOffsetY: offsetY,
                getWidth : function () { return this.right - this.left; },
                getHeight : function () { return this.bottom - this.top; }
            };
        },
        fillOverlapSlots: true,
        positionToEventWin : function (show) {
            var eventWin = this.eventWin,
                cal = eventWin.calendar,
                view = this.view,
                left = view.getEventLeft(eventWin.event) + this.getEventPadding(),
                top = eventWin.getTop(),
                width = eventWin.getVisibleWidth(),
                height = eventWin.getVisibleHeight()
            ;

            if (this.fillOverlapSlots) {
                // cause the drag rect to fill the column's width, or the row's height
                if (view.isTimelineView()) {
                    var row = view.getEventRow(top);
                    top = view.getRowTop(row);
                    height = view.getRowHeight(row);
                } else {
                    // use body colNum (labelCols are frozen)
                    var col = view.body.getEventColumn(left);
                    left = view.body.getColumnLeft(col);
                    width = view.body.getColumnWidth(col);
                }
            }
            
            if (this._resizing) {
                if (view.isTimelineView()) {
                    top = view.body.getRowTop(eventWin._dragProps._startRow);
                } else {
                    left = view.body.getColumnLeft(eventWin._dragProps._startCol);
                }
            }
            
            if (left<0) left = 0;
            
            this.moveTo(left, top);
            this.resizeTo(width, height);
            
            if (show) {
                if (!this.isDrawn()) this.draw();
                this.show();
                this.bringToFront();
            }
        },
        dragRepositionStart : function () {
            var eventWin = this.eventWin,
                cal = eventWin.calendar,
                view = this.view,
                gr = view.body
            ;

            if (!cal.canEditEvent(eventWin.event)) return false;

            this._repositioning = true;

            var eventRow = gr.getEventRow(),
                rowTop = gr.getRowTop(eventRow),
                rowHeight = gr.getRowHeight(eventRow),
                eventLeft = view.getEventLeft(eventWin.event) + 1,
                eventCol = gr.getEventColumn(eventLeft),
                columnLeft = gr.getColumnLeft(eventCol),
                columnWidth = gr.getColumnWidth(eventCol),
                offsetX = gr.getOffsetX() - eventWin.getLeft(),
                offsetY = gr.getOffsetY() - eventWin.getTop()
            ;

            var isTimeline = view.isTimelineView();
            
            var dp = eventWin._dragProps = {};

            dp._isVertical = !isTimeline;
                
            dp._fixedHeight = true;
            dp._fixedWidth = true;
            dp._startRow = eventRow;
            dp._startCol = eventCol;
            dp._rowHeight = rowHeight;
            dp._colWidth = columnWidth;

            dp._startWidth = isTimeline ? eventWin.getVisibleWidth() : dp._colWidth;
            dp._startHeight = isTimeline ? dp._rowHeight : eventWin.getVisibleHeight();
            dp._eventRowDelta = eventWin.getTop() - rowTop ;
            dp._currentRow = eventRow;
            dp._currentCol = eventCol;
            dp._startOffsetX = offsetX;
            dp._startOffsetY = offsetY;
            
            dp._rowCount = Math.round(dp._startHeight / dp._rowHeight);
            dp._maxRow = view.data.length - dp._rowCount;
            dp._maxTop = view.getRowTop(dp._maxRow);
            dp._maxLeft = isTimeline ? gr.getScrollWidth() - dp._startWidth :
                    gr.getColumnLeft(gr.fields.length-1);
            dp._maxCol = isTimeline ? gr.getEventColumn(dp._maxLeft) :
                    gr.fields.length - 1;

            this.positionToEventWin(true);

            return isc.EH.STOP_BUBBLING;
        },
        dragRepositionMove : function () {
            var eventWin = this.eventWin,
                props = eventWin._dragProps,
                event = eventWin.event,
                cal = eventWin.calendar,
                view = this.view,
                isTL = view.isTimelineView(),
                gr = view.body,
                overRow = gr.getEventRow(),
                eventRow = Math.min(props._maxRow, 
                    overRow < 0 ? (overRow == -1 ? 0 : props._maxRow) : overRow),
                rowTop = gr.getRowTop(eventRow),
                eventCol = Math.min(props._maxCol, gr.getEventColumn()),
                columnLeft = gr.getColumnLeft(eventCol),
                mouseY = gr.getOffsetY(),
                snapY = (Math.floor((mouseY - rowTop) / cal.eventSnapGap) * cal.eventSnapGap)
            ;

            var offsetX = (gr.getOffsetX() - props._startOffsetX),
                tempLeft = offsetX - ((offsetX - columnLeft) % cal.eventSnapGap) + 1,
                snapTop = isTL ? rowTop : Math.min(props._maxTop, rowTop + snapY),
                date = view.getDateFromPoint(tempLeft, snapTop, null, true),
                eventLeft = Math.min(props._maxLeft, 
                    (isTL ? cal.getDateLeftOffset(date, view) + cal.timelineEventPadding :
                                columnLeft)),
                eventRight = eventLeft + eventWin.getVisibleWidth()
            ;

            var rightColNum = gr.getEventColumn(eventRight);

            if (rightColNum < 0) {
                this.moveTo(props._previousLeft, snapTop);
                return isc.EH.STOP_BUBBLING;
            }

            if (eventRow != props._currentRow) {
                // rowNum has changed
                if (view.isTimelineView()) {
                    if (!cal.canEditEventLane(event, view)) {
                        eventRow = props._currentRow;
                        rowTop=gr.getRowTop(eventRow);
                        snapTop=(gr.getTop() + rowTop + props._eventRowDelta);
                    }
                } else {
                    if (eventRow < 0) {
                        // don't let day/week events be dragged off the top of the view
                        eventRow = 0;
                        snapTop = 0;
                    } else {
                        var tempBottom = rowTop + props._startHeight;
                        
                        var bottomRow = gr.getEventRow(rowTop + props._startHeight - props._rowHeight);
                        if (bottomRow < 0) {
                        //if (tempBottom > view.getScrollHeight()) {
                            // don't let day/week events be dragged off the bottom of the view
                            eventRow = props._currentRow;
                            snapTop = gr.getRowTop(eventRow);
                        } else {
                            props._currentRow = eventRow;
                        }
                    }
                }
            }
            props._currentRow = eventRow;
            if (eventCol != props._currentCol) {
                if (view.isDayView() || view.isWeekView()) {
                    if (view.isDayView() && cal.showDayLanes && !cal.canEditEventLane(event, view)) {
                        // lanes in dayView
                        eventCol = props._currentCol;
                        eventLeft = props._previousLeft;
                    } else {
                        // dayView without lanes
                        if (eventCol == -1) props._currentCol = 0;
                        else if (eventCol == -2) props._currentCol = props._currentCol;
                        else props._currentCol = eventCol;
                        eventLeft = gr.getColumnLeft(props._currentCol);
                    }
                } else {
                    props._currentCol = Math.max(1, eventCol);
                }
            }

            //if (view.isDayView() || view.isWeekView())
            if (snapTop < 0) snapTop = 0;
            props._previousTop = snapTop;
            if (eventLeft < 0) eventLeft = 0;
            props._previousLeft = eventLeft;
            this.moveTo(eventLeft, snapTop);
            return isc.EH.STOP_BUBBLING;
        },
        dragRepositionStop : function () {
            // need to do this here, else previous setting of HSnapOrigin destroys dragResize
            // correct functionality. (see dragRepositionStart())
            var eventWin = this.eventWin,
                props = eventWin._dragProps,
                cal = eventWin.calendar,
                view = this.view,
                gr = view.body,
                event = eventWin.event
            ;

            eventWin.parentElement.HSnapOrigin = 0;

            var canEditLane = cal.canEditEventLane(event),
                currRow = props._currentRow,
                rowTop = props._previousTop,
                currCol = (view.isDayView() && canEditLane ? view.body.getEventColumn() : 
                    gr.getEventColumn(props._previousLeft+1)), //props._currentCol),
                //mouseY = gr.getOffsetY(),
                //snapY = (Math.floor((mouseY - rowTop) / cal.eventSnapGap) * cal.eventSnapGap),
                newLane
            ;
            
            // hide the manual dragTarget before calling the cancellable timelineEventMoved()
            this.hide();

            if (view.isTimelineView()) {
                if (currRow != props._startRow && currRow >= 0) {
                    if (canEditLane) {
                        var lane = cal.lanes ? cal.lanes.get(currRow) : null;
                        var currLaneRec = lane ? lane.name || lane[cal.laneNameField] : null;
                        newLane = currLaneRec;
                    } else {
                        eventWin.showLines();
                        return false;
                    }
                }
            } else if (view.isDayView() && cal.showDayLanes) {
                if (currCol != props._startCol && currCol >= 0) {
                    if (canEditLane) {
                        var lane = cal.lanes ? cal.lanes.get(currCol) : null;
                        var currLaneRec = lane ? lane.name || lane[cal.laneNameField] : null;
                        newLane = currLaneRec;
                    } else {
                        return false;
                    }
                }
            }

            var dates = [ event[cal.startDateField].duplicate(), event[cal.endDateField].duplicate()];
            if (view.isTimelineView()) {
                if (event[cal.leadingDateField] && event[cal.trailingDateField]) {
                    dates.add(event[cal.leadingDateField].duplicate());
                    dates.add(event[cal.trailingDateField].duplicate());
                }
            }
            
            // step 1 find initial drop dates
            // augment (or decrement if diff is negative) each date by the difference between 
            // the drag start date and the drag end date
            var EH = eventWin.ns.EH;
            
            var dragLeft = props._previousLeft; // - this.getEventPadding() + 1;
            //var dragTop = EH.dragMoveTarget.getTop() + this.getEventPadding() + (view.isTimelineView() ? 0 : 4);
            var dragTop = rowTop; //eventWin._dragProps._previousTop;
            // convert dragTracker left to a date
            var sDate = cal.getDateFromPoint(dragLeft+1, dragTop+1, true, view);
            var eDate = sDate.duplicate();
            // get endDate
            eDate.setMinutes(eDate.getMinutes() + eventWin.getEventLength());
            dates[0] = sDate;
            dates[1] = eDate;
            // minsDiff = difference in minutes between new start date and old start date
            var minsDiff = Math.floor((sDate.getTime() - event[cal.startDateField].getTime()) / (1000 * 60));
            // adjust leading and trailing dates by minsDiff amount of minutes. 
            // if event dragged behind itself, minsDiff will be negative.
            if (event[cal.leadingDateField]) dates[2].setMinutes(dates[2].getMinutes() + minsDiff);
            if (event[cal.trailingDateField]) dates[3].setMinutes(dates[3].getMinutes() + minsDiff);
            
            var otherFields = {};
            if (newLane) otherFields[cal.laneNameField] = newLane;
            if (event[cal.leadingDateField] && event[cal.trailingDateField]) {
                otherFields[cal.leadingDateField] = dates[2];
                otherFields[cal.trailingDateField] = dates[3];
            }
            if (newLane == null) newLane = event[cal.laneNameField];
            // step 2 adjust initial drop dates, via overridden method 
            if (cal.adjustEventTimes) {
                var adjustedTimes = cal.adjustEventTimes(event, eventWin, dates[0], dates[1], newLane);
                if (adjustedTimes) {
                    dates[0] = adjustedTimes[0].duplicate();
                    dates[1] = adjustedTimes[1].duplicate();
                }
            }

            var repositionedDates = cal.checkForOverlap(view, eventWin, event, dates[0], dates[1], newLane); 

            // if this event was previously overlapped, and it isn't now, reset it's slotNum
            if (event._overlapProps && event._overlapProps.slotNum > 0 && repositionedDates != true)  {
                event._overlapProps.slotNum = event._slotNum = 1;
                event._overlapProps.totalSlots = 1;
            }
            
            // step 3 adjust modified drop dates so no overlapping occurs
            if (cal.allowEventOverlap == false) {
                //TODO: this code is still timeline specific
                if (repositionedDates == true) {
                    // event overlaps in such a way that dropping anywhere near this location would
                    // be impossible
                    if (cal.timelineEventOverlap) {
                        cal.timelineEventOverlap(false, event, eventWin, dates[0], dates[1], newLane);
                    }
                    return false;   
                } else if (isc.isAn.Array(repositionedDates)){
                   dates[0] = repositionedDates[0].duplicate();
                   dates[1] = repositionedDates[1].duplicate();
                   if (cal.timelineEventOverlap) { 
                       cal.timelineEventOverlap(true, event, eventWin, dates[0], dates[1], newLane);
                   }
                   
                }
                // otherwise don't do anything, as no overlap occurred
            }

            if (view.isTimelineView()) {
                // step 4 fire timelineEventMoved notification to allow drop cancellation
                if (cal.timelineEventMoved(event, dates[0], dates[1], newLane) == false) return false;
            } else {
                // step 4 fire eventMoved notification to allow drop cancellation
                if (cal.eventMoved(dates[0], event, newLane) == false) return false;
            }
            
            // finally update event
            //isc.logWarn('updating event:' + [dates[0], dates[1]]);
            cal.updateEvent(event, dates[0], dates[1], event[cal.nameField], 
                event[cal.descriptionField], otherFields, true, newLane);

            this._repositioning = false;
            delete eventWin._dragProps;
            
            //return false;
            return isc.EH.STOP_BUBBLING;
        },
        
        // dragTarget_dragResizeStart
        dragResizeStart : function () {
            var eventWin = this.eventWin,
                cal = eventWin.calendar,
                view = this.view,
                gr = view.body
            ;

            if (!cal.canEditEvent(eventWin.event)) return false; //.canDragReposition) return false;

            this._resizing = true;

            var eventRow = gr.getEventRow(),
                rowTop = gr.getRowTop(eventRow),
                rowHeight = gr.getRowHeight(eventRow),
                eventCol = gr.getEventColumn(),
                eventLeft = view.getEventLeft(eventWin.event) + 1,
                //eventCol = gr.getEventColumn(eventLeft),
                colWidth = gr.getColumnWidth(eventCol),
                //offsetX = gr.getOffsetX() - eventWin.getLeft(),
                offsetX = gr.getOffsetX() - eventWin.getLeft(), // - this.getEventPadding(),
                offsetY = gr.getOffsetY() - eventWin.getTop(),
                eventWidth = eventWin.getVisibleWidth()
            ;

            // if the offset 
            var isLeft = (offsetX < eventWidth / 2);
            
            eventWin._dragProps = {
                _startRow: eventRow,
                _eventRowDelta: view._repositioning ? eventWin.getTop() - rowTop : 0,
                _startCol: eventCol,
                _currentCol: eventCol,
                _currentRow: eventRow,
                _startOffsetX: offsetX,
                _startOffsetY: offsetY,
                _startWidth: eventWidth,

                _endOffsetX: offsetX + eventWidth,

                _colWidth: colWidth,
                _rowHeight: rowHeight,
                
                _previousLeft: eventWin.getLeft(),
                _previousRight: eventWin.getLeft() + eventWidth,
                _previousTop: view.isTimelineView() ? rowTop : eventWin.getTop(),
                _previousHeight: (view.isTimelineView() ? rowHeight : eventWin.getVisibleHeight()),
                _leftDrag: isLeft
            };

            this.positionToEventWin(true);
            
            //var snapOrigin = (eventWin.getTop() + eventWin.getVisibleHeight()) % eventWin.parentElement.snapVGap;
            //this.parentElement.VSnapOrigin = snapOrigin; 
            return isc.EH.STOP_BUBBLING;
        },

        dragResizeMove : function () {
            var eventWin = this.eventWin,
                props = eventWin._dragProps,
                event = eventWin.event,
                cal = eventWin.calendar,
                view = this.view,
                    gr = this.view.body,
                
                eventRow = gr.getEventRow(),
                rowTop = gr.getRowTop(eventRow),
                snapTop = this.fillOverlapSlots ? rowTop : 
                    (gr.getTop() + rowTop + props._eventRowDelta),

                eventCol = Math.max(0, gr.getEventColumn(gr.getOffsetX() - props._startOffsetX)),
                columnLeft = gr.getColumnLeft(eventCol),
                offsetX = (gr.getOffsetX() - props._startOffsetX),
                tempLeft = offsetX - ((offsetX - columnLeft) % cal.eventSnapGap) + 1,
                date = view.getDateFromPoint(tempLeft, snapTop, null, true),
                eventLeft = (view.isTimelineView() ? cal.getDateLeftOffset(date, view) + cal.timelineEventPadding :
                                columnLeft),
                //eventRight = eventLeft + this.getVisibleWidth(),
                isTimeline = view.isTimelineView(),
                mouseY = gr.getOffsetY(),
                snapY = (Math.floor((mouseY - rowTop) / cal.eventSnapGap) * cal.eventSnapGap)
            ;

            var top, left, bottom, right, width, height;

            if (isTimeline) {
                top = props._previousTop;
                height = props._previousHeight;
                if (props._leftDrag) {
                    right = props._previousRight;
                    left = gr.getOffsetX() - (gr.getOffsetX() % cal.eventSnapGap) + cal.eventSnapGap; //eventLeft; //gr.getOffsetX() - (gr.getOffsetX() - props._startOffsetX);
                    //left = Math.max(left, left * -1);
                    if (left < 0){
                        this.setLeft(props._previousLeft);
                        this.setWidth(props._previousRight - props._previousLeft);
                        return isc.EH.STOP_BUBBLING;
                    } else if (left > right - cal.eventSnapGap) {
                        //this.setLeft(props._previousLeft);
                        return isc.EH.STOP_BUBBLING;
                    }
                } else {
                    left = props._previousLeft;
                    right = gr.getOffsetX() - (gr.getOffsetX() % cal.eventSnapGap) + cal.eventSnapGap; // props._startOffsetX);
                    //right = Math.max(right, right * -1);
                    if (right < left + cal.eventSnapGap) right = left + cal.eventSnapGap;
                    var rightColNum = gr.getEventColumn(right);
                    if (rightColNum < 0) {
                        this.resizeTo(props._previousRight - props._previousLeft);
                        return isc.EH.STOP_BUBBLING;
                    }
                }
                width = right - left;
                this.setLeft(left);
                this.setWidth(width);
            } else {
                top = props._previousTop;
                left = props._previousLeft;
                right = props._previousRight;
                //bottom = gr.getOffsetY() - (gr.getOffsetY() % cal.eventSnapGap) + cal.eventSnapGap; 
                mouseY = gr.getOffsetY();
                snapY = (Math.floor((mouseY - rowTop) / cal.eventSnapGap) * cal.eventSnapGap);

                bottom = gr.getOffsetY() - (gr.getOffsetY() % props._rowHeight) + snapY;
                height = bottom - top;
                width = right - left;
                //this.moveTo(top, null);
                //this.resizeTo(null, height);
                this.setHeight(height);
                this.setTop(top);
            }

            //eventWin._dragProps._previousTop = top;
            if (view.isTimelineView()) {
                if (props._leftDrag) props._previousLeft = left;
                else props._previousRight = right;
            } else {
                props._previousHeight = height;
            }
            
            isc.logWarn("moving drag window to " + [top, left, width, height]);
            return isc.EH.STOP_BUBBLING;
        },

        // eventWindow_dragResizeStop
        dragResizeStop : function () {
            var eventWin = this.eventWin,
                props = eventWin._dragProps,
                cal = eventWin.calendar,
                view = this.view,
                event = eventWin.event,
                // store these so we can auto-arrange both source and target locations after the move
                startDate,
                endDate, 
                EH = this.ns.EH,
                colNum,
                // convert dragLeft into local coords by calculating an offset based on eventWindow
                // pageLeft - left.
                //leftOffset = this.getPageLeft() - this.getLeft(),
                //dragLeft = EH.dragMoveTarget.getLeft() - leftOffset
                target = EH.dragMoveTarget
            ;

            if (view.isTimelineView()) {
                var dragLeft = target.getLeft();
                var ewLeft = this.eventWin.getLeft();
                if ([ewLeft, ewLeft+1].contains(dragLeft)) { // right side dragged
                    startDate = event[cal.startDateField].duplicate();
                    //var dragRight = (dragLeft + this.getVisibleWidth() + (cal.eventSnapGap*2)); //EH.dragMoveTarget.getVisibleWidth());
                    var dragRight = props._previousRight;
                    endDate = view.getDateFromPoint(dragRight, null, null, true);
                    // special case: when sizing to grid on a right drag, take a columns length off the 
                    // end date, as getDateFromPoint() handles the border case as being on the next 
                    // day, which is fine for the start date but not for the end date
                    //if (cal.sizeEventsToGrid) {
                    //    var minsInACol = cal.timelineView._getMinsInACell();
                    //    endDate.setMinutes(endDate.getMinutes() - minsInACol);
                    //}
                } else { // left side dragged
                    startDate = view.getDateFromPoint(dragLeft, null, null, true);
                    endDate = event[cal.endDateField].duplicate();
                }
            } else {
                var dragBottom = props._previousTop + props._previousHeight;

                // only the bottom can be dragged for normal eventWindows
                startDate = event[cal.startDateField].duplicate();
                endDate = view.getDateFromPoint(eventWin._dragProps._previousLeft+2, dragBottom, null, true);
            }

            // hide the manual dragTarget before calling the cancellable timelineEventResized()
            this.hide();

            this._resizing = false;
            delete eventWin._dragProps;

            // Added undoc'd endDate param - is necessary for Timeline items because they can be 
            // stretched or shrunk from either end
            if (view.isTimelineView()) {
                // step 4 fire timelineEventMoved notification to allow drop cancellation
                if (cal.timelineEventResized(event, startDate, endDate) == false) return false;
            } else {
                // step 4 fire eventMoved notification to allow drop cancellation
                if (cal.eventResized(endDate, event) == false) return false;
            }

            //this._skipResize = true;
            //isc.logWarn('dragResizeStop:' + [startDate, endDate]);
            cal.updateEvent(event, startDate, endDate,
                    event[cal.nameField], event[cal.descriptionField], null, true, event[cal.laneNameField]);

            return isc.EH.STOP_BUBBLING;
        }
    },

    scrolled : function () {
        if (this.renderEventsOnDemand && this.refreshVisibleEvents) {
            var _this = this,
                events = this.data
            ;
            if (this._layoutEventId) isc.Timer.clear(this._layoutEventId);
            this._layoutEventId = isc.Timer.setTimeout(function () {
                _this.refreshVisibleEvents();
            });
        }       
    },

    resized : function () {
        this.Super('resized', arguments);
        //isc.logWarn(this.viewName + " resized:" + [this.isDrawn(), this.getCalendar().hasData()]);
        if (this.renderEventsOnDemand && this.isDrawn() && this.getCalendar().hasData()) {
            this.refreshVisibleEvents();
        }
    },

    forceDataSort : function (data) {
        var cal = this.getCalendar(),
            specifiers = []
        ;
        
        if (this.isTimelineView() || (this.isDayView() && cal.showDayLanes)) {
            specifiers.add({ property: cal.laneNameField, direction: "ascending" });
        }

        if (this.isTimelineView() && cal.overlapSortSpecifiers) {
            specifiers.addList(cal.overlapSortSpecifiers);
        } else {
            specifiers.add({ property: cal.startDateField, direction: "ascending" });
        }

        if (!data) {
            data = cal.data;
            cal._ignoreDataChanged = true;
        }

        data.setSort(specifiers);
    },

    findEventsInRange : function (startDate, endDate, lane, data) {
        var cal = this.getCalendar(),
            range = {},
            useLane = lane != null && (this.isTimelineView() || (this.isDayView() && cal.showDayLanes))
        ;
        range[cal.startDateField] = startDate;
        range[cal.endDateField] = endDate;
        if (useLane) range[cal.laneNameField] = lane;
        var events = this.findOverlappingEvents(range, range, false, useLane, data);
        return events;
    },

    // realEvent is the actual event object, passed in so that we can exclude
    // it from the overlap tests. paramEvent is an object with date fields  - the third param
    // allows the function to return the realEvent as well
    findOverlappingEvents : function (realEvent, paramEvent, includeRealEvent, useLanes, data) {
        var cal = this.getCalendar(),
            dataPassed = data != null
        ;

        var events = dataPassed ? data : cal.data;
        
        if (!dataPassed) this.forceDataSort(events);
    
        var results = [],
            length = events.getLength(),
            dayEnd = isc.DateUtil.getEndOf(paramEvent[cal.startDateField], "d"),
            dayStart = isc.DateUtil.getStartOf(paramEvent[cal.startDateField], "d")
        ;

        var rangeObj = {};
        
        var lane = useLanes ? realEvent[cal.laneNameField] : null,
            startIndex = 0;
        
        if (lane) startIndex = events.findIndex(cal.laneNameField, lane);

        for (var i = startIndex; i < length; i++) {
            var event = events.get(i);
            if (!event) {
                isc.logWarn('findOverlappingEvents: potentially invalid index: ' + i);  
                break;
            }

            if (useLanes && event[cal.laneNameField] != lane) break;

            if (!includeRealEvent && cal.eventsAreSame(event, realEvent)) {
                continue;
            }
            if (this.isTimelineView()) {
                // if we're not showing lead-trail lines use start-endDate fields instead to 
                // determine overlap
                if (event[cal.leadingDateField] && event[cal.trailingDateField]) {
                    rangeObj[cal.leadingDateField] = paramEvent[cal.leadingDateField];
                    rangeObj[cal.trailingDateField] = paramEvent[cal.trailingDateField];
                    if (rangeObj[cal.trailingDateField].getTime() > this.endDate.getTime()) {
                        rangeObj[cal.trailingDateField].setTime(this.endDate.getTime()-1)
                    }
                } else {
                    rangeObj[cal.startDateField] = paramEvent[cal.startDateField];
                    rangeObj[cal.endDateField] = paramEvent[cal.endDateField];
                    if (rangeObj[cal.endDateField].getTime() > this.endDate.getTime()) {
                        rangeObj[cal.endDateField].setTime(this.endDate.getTime()-1)
                    }
                }
            } else {
                if (event[cal.startDateField].getTime() > dayEnd.getTime()) continue;
                if (event[cal.endDateField].getTime() < dayStart.getTime()) continue;
                rangeObj[cal.startDateField] = paramEvent[cal.startDateField];
                rangeObj[cal.endDateField] = paramEvent[cal.endDateField];    
                if (rangeObj[cal.endDateField].getTime() > paramEvent[cal.endDateField].getTime()) {
                    rangeObj[cal.endDateField].setTime(paramEvent[cal.endDateField].getTime())
                }
            }

            rangeObj[cal.laneNameField] = event[cal.laneNameField];

            if (this.eventsOverlap(rangeObj, event, useLanes)) { 
                //isc.logWarn('findOverlappingEvents:' + event.id); 
                results.add(event);
            }
        }
        
        return results;
    },

    eventsOverlap : function (rangeObject, event, sameLaneOnly) {
        var a = rangeObject, 
            b = event,
            cal = this.getCalendar(),
            startField = cal.startDateField,
            endField = cal.endDateField
        ;
        
        if (sameLaneOnly && a[cal.laneNameField] != b[cal.laneNameField]) return false;
        
        if (this.isTimelineView()) {
            if (a[cal.leadingDateField] && b[cal.leadingDateField]) startField = cal.leadingDateField;
            if (a[cal.trailingDateField] && b[cal.trailingDateField]) endField = cal.trailingDateField;
        }
        
        // simple overlap detection logic: there can only be an overlap if 
        // neither region A end <= region B start nor region A start >= region b end.
        // No need to check other boundary conditions, this should satisfy all
        // cases: 1. A doesn't overlap B, A partially overlaps B, A is completely
        // contained by B, A completely contains B.
        // NOTE: using the equals operator makes the case where 
        // two dates are exactly equal be treated as not overlapping.
        var aStart = a[startField].getTime(), aEnd = a[endField].getTime(),
            bStart = b[startField].getTime(), bEnd = b[endField].getTime()
        ;
        if (cal.equalDatesOverlap && cal.allowEventOverlap) {
            if ((aStart < bStart && aEnd >= bStart && aEnd <= bEnd) // overlaps to the left
                || (aStart <= bEnd && aEnd > bEnd) // overlaps to the right
                || (aStart <= bStart && aEnd >= bEnd) // overlaps entirely
                || (aStart >= bStart && aEnd <= bEnd) // is overlapped entirely
            ) {
                return true;
            } else {
                return false;
            }
        } else {
            // b is event, a is range
            if (bStart == aEnd || bEnd == aStart) {
                // one event starts exactly on the end of the other event - only overlap if
                // cal.equalDatesOverlap if true
                return !!cal.equalDatesOverlap;
            }
            if (bStart <= aEnd && bEnd >= aStart) return true;
            return false;
            /*
            if ((aStart < bStart && aEnd > bStart && aEnd < bEnd) // overlaps to the left
                || (aStart < bEnd && aEnd > bEnd) // overlaps to the right
                || (aStart <= bStart && aEnd >= bEnd) // overlaps entirely
                || (aStart >= bStart && aEnd <= bEnd) // is overlapped entirely
            ) {
                return true;
            } else {
                return false;
            }
            */
        }
   
    },


    updateEventRange : function (event, range) {
        if (!isc.isAn.Object(range)) range = this.overlapRanges.ranges[range];
        
        var events = range.events;
        events.remove(event);
        this.updateOverlapRanges(events);
    },

    
    updateOverlapRanges : function (passedData) {
        var cal = this.getCalendar(),
            data = passedData || cal.data,
            ranges = this.overlapRanges || [],
            //ranges = [],
            dataLen = data.getLength(),
            // should we only detect overlaps by date if the events are in the same lane?
            useLanes = this.isTimelineView() || (this.isDayView() && cal.showDayLanes),
            // events on different days can currently only overlap if on the same date
            splitDates = !this.isTimelineView(),
            // the list of overlap ranges that were actually affected by the process, so the
            // ranges that need to be re-tagged
            touchedRanges = [],
            minDate = this.startDate,
            maxDate = this.endDate
        ;

        if (isc.isA.ResultSet(data)) {
            data = data.allRows;
        }

        data.setProperty("_tagged", false);
        data.setProperty("_overlapProps", null);
        data.setProperty("_slotNum", null);
        
        var laneNames = useLanes && cal.lanes ? cal.lanes.getProperty(cal.laneNameField) : [];

        //this.forceDataSort(d)

        for (var i=0; i<dataLen; i++) {
            var event = data.get(i);
            if (event._tagged) continue;

            if (useLanes && !laneNames.contains(event[cal.laneNameField])) {
                //event._tagged = true;
                continue;
            }

            event._tagged = true;
            event._overlapProps = {};

            var addRange = false,
                range = {};
            range[cal.startDateField] = event[cal.startDateField];
            range[cal.endDateField] = event[cal.endDateField];
            if (useLanes) range[cal.laneNameField] = range.lane = event[cal.laneNameField];
            range.events = [];

            var overlappers = this.findOverlappingEvents(event, event, true, useLanes, data);
            if (overlappers && overlappers.length > 0) {
                range.totalSlots = overlappers.length;
                var totalSlots = range.totalSlots;
                var localSlots = 1;
                for (var j=0; j<overlappers.length; j++) {
                    var ol = overlappers[j];

                    if (ol[cal.startDateField] < range[cal.startDateField])
                        range[cal.startDateField] = ol[cal.startDateField];
                    if (ol[cal.endDateField] > range[cal.endDateField])
                        range[cal.endDateField] = ol[cal.endDateField];

                    var subOL = ol != event ? this.findOverlappingEvents(ol, ol, true, useLanes, data) : [];
                    
                    if (subOL && subOL.length > 0) {
                        var totals = [];
                        subOL.map(function(item) {
                            if (item._overlapProps) totals.add(item._overlapProps.totalSlots);
                        });

                        if (totals.max() != totalSlots) {
                            totalSlots = Math.min(totals.max(), totalSlots);
                            localSlots++;
                        }
                    }
                    
                    var slotNum = localSlots;
                    
                    if (!ol._overlapProps) {
                        ol._tagged = true;
                        ol._overlapProps = { slotNum: slotNum, totalSlots: localSlots };
                    } else {
                        ol._overlapProps.totalSlots = Math.max(localSlots, ol._overlapProps.totalSlots);
                        ol._ignore = true;
                    }
                }
                range.totalSlots = localSlots;
                overlappers.map(function (item) {
                    if (item._ignore) delete item._ignore;
                    else item._overlapProps.totalSlots = localSlots;
                });
                
                event._overlapProps.totalSlots = range.totalSlots;

                range.events = overlappers;

                addRange = true;
                
                for (var k=0; k<ranges.length; k++) {
                    if (range[cal.laneNameField] != ranges[k][cal.laneNameField]) continue;
                    var overlaps = this.eventsOverlap(range, ranges[k], true);
                    if (overlaps) {
                        // this range overlaps another range
                        if (range.totalSlots > ranges[k].totalSlots) {
                            event._overlapProps.totalSlots = range.totalSlots;
                            event._overlapProps.slotCount = range.totalSlots - event._overlapProps.slotNum;
                        }
                        // merge the two ranges - the dates of the existing range are altered to 
                        // fully incorporate both ranges and events are copied over
                        this.mergeOverlapRanges(range, ranges[k]);
                        if (!touchedRanges.contains(ranges[k])) touchedRanges.add(ranges[k]);
                        addRange = false;
                    }
                    if (!addRange) break;
                }
                
            }
            if (addRange) {
                ranges.add(range);
                if (!touchedRanges.contains(range)) touchedRanges.add(range);
            }
        }

        for (i=0; i<ranges.length; i++) {
            var range = ranges[i];
            // set an overlapRangeId on the events in each range
            range.events.setProperty("overlapRangeId", ranges.length + i);
            // set a colNum on each range (used in dayView the absence of a lane)
            if (!this.isTimelineView()) range.colNum = this.getColFromDate(range[cal.startDateField]);
        }

        this.overlapRanges = ranges;

        return touchedRanges;
    },
    
    getTouchedOverlapRanges : function (startDate, endDate, lane) {
        if (!this.overlapRanges) this.overlapRanges = [];
        // return a list of all overlapRanges that touch the passed date range and lane
        // - existing ranges will never overlap each other, but multiple existing ranges 
        // might overlap the passed one (if, say, you drop a long event into a new day or 
        // lane that already has various separate overlapRanges)
        var addRange = true,
            cal = this.getCalendar(),
            tR = this.overlapRanges,
            r = {},
            ranges = []
        ;
        
        r[cal.startDateField] = startDate;
        r[cal.endDateField] = endDate;
        r[cal.laneNameField] = lane;
        
        for (var k=0; k<tR.length; k++) {
            var range = tR[k];
            if (lane != null && range[cal.laneNameField] != lane) continue;
            var overlaps = this.eventsOverlap(r, range, true);
            if (overlaps) {
                ranges.add(range);
            }
        }
        return ranges;
    },
    
    mergeOverlapRanges : function (fromRanges, toRange) {
        // merge the passed fromRanges in the passed toRange - the toRange ends up spanning
        // the date extents and all events from each of the merged ranges
        if (!isc.isAn.Array(fromRanges)) fromRanges = [fromRanges];
        
        var cal = this.getCalendar(), start = cal.startDateField, end = cal.endDateField,
            b = toRange
        ;

        for (var i=0; i<fromRanges.length; i++) {
            var a = fromRanges[i];
            // extend the toRange to fully incorporate the fromRange
            if (a[start] < b[start]) b[start] = a[start];
            if (a[end] > b[end]) b[end] = a[end];
            // increase toRange.totalSlots to fromRange.totalSlots, if thats greater
            if (a.totalSlots > b.totalSlots) b.totalSlots = a.totalSlots;
            // add the events in the fromRange to the toRange
            b.events.addList(a.events);
            b.events = b.events.getUniqueItems();
        }
    },
    getEventLaneIndex : function (event) {
        return this.getLaneIndex(event[this.getCalendar().laneNameField]);
    },
    getEventLane : function (event) {
        return this.getLane(event[this.getCalendar().laneNameField]);
    },
    hasOverlapRanges : function () { 
        // are there any overlap ranges?  should always be if there are any visible events in the range
        return this.overlapRanges != null && this.overlapRanges.length > 0;
    },
    getLaneOverlapRanges : function (laneName) {
        // return a list of the overlapRanges that exist for the passed lane
        if (!this.hasOverlapRanges()) return;
        var cal = this.getCalendar(),
            ranges = [];
        this.overlapRanges.map(function (range) {
            if (range[cal.laneNameField] == laneName) ranges.add(range);
        });
        return ranges;
    },
    getDayOverlapRanges : function (date) {
        // return a list of the overlapRanges that exist for the passed date (column)
        if (!this.hasOverlapRanges()) return;
        var colNum = this.getColFromDate(date);
        if (colNum >= 0) return this.getColOverlapRanges(colNum);
    },
    getColOverlapRanges : function (colNum) {
        // return a list of the overlapRanges that exist for the passed column (lane or date)
        if (!this.hasOverlapRanges()) return;
        var ranges = this.overlapRanges.findAll("colNum", colNum);
        return ranges;
    },
    removeOverlapRanges : function (ranges) {
        // remove the passed list of overlapRanges in preparation for re-tagging
        if (!this.hasOverlapRanges() || !ranges) return;
        ranges.map(function (range) {
            // disassociate the events from the range
            range.events.setProperty("overlapRangeId", null);
        });
        this.overlapRanges.removeList(ranges);
    },
    getEventOverlapRange : function (event) {
        // get the single overlap range that this event appears in
        if (!this.hasOverlapRanges()) return;
        return this.overlapRanges[event.overlapRangeId];
    },
    getDateOverlapRange : function (date, lane) {
        // get the single overlap range, if any, that contains the passed date
        if (!this.hasOverlapRanges()) return;
        var cal = this.getCalendar(),
            timeStamp = date.getTime()
        ;
        var ranges = this.overlapRanges.map(function (range) {
            if (timeStamp >= range[cal.startDateField].getTime() &&
                    timeStamp <= range[cal.endDateField].getTime() &&
                    (!lane || lane == range[cal.laneNameField]))
            {
                // this range starts before and ends after the passed date (and is in the 
                // correct lane, if one was passed in)
                return range;
            }
        });
        if (ranges) ranges.removeEmpty();
        return ranges && ranges.length && ranges[0] ? ranges[0] : null;
    },

    // recalculate the overlap ranges in a given lane (either vertical or horizontal) and 
    // re-render events appropriately
    retagLaneEvents : function (laneName) {
        var isTimeline = this.isTimelineView(),
            lane;

        if (!(isTimeline || (this.isDayView() && this.getCalendar().showDayLanes))) return;

        if (isTimeline) {
            lane = this.getLane(laneName);
            this.retagRowEvents(lane, true);
        } else {
            lane = this.getLane(laneName);
            this.retagColumnEvents(lane, true);
        }
    },

    // recalculate the overlap ranges in a given day (one vertical column, or multiple 
    // vertical lanes, if in dayView and showDayLanes is true
    retagDayEvents : function (date) {
        if (this.isTimelineView()) return;

        var field = this.getColFromDate(date);
        this.retagColumnEvents(field, false);
    },

    // recalculate the overlap ranges in a given column - might be a "day" or a vertical lane
    retagColumnEvents : function (colNum, isLane) {
        if (this.isTimelineView()) return;

        var field;
        if (isc.isA.Number(colNum)) {
            field = this.body.getField(colNum);
        } else {
            field = colNum;
            colNum = this.body.getFieldNum(field);
        }
        
        // 1) remove the ranges that appear in this column
        this.removeOverlapRanges(this.getColOverlapRanges(colNum));
        
        // 2) get a list of events that will be in this column
        var date = this.getDateFromCol(colNum);
        if (!date) return;
        var startDate = date,
            endDate = isc.DateUtil.getEndOf(date, "d")
        ;
        var events = this.findEventsInRange(startDate, endDate, (isLane ? field.name : null));

        // 3) re-tag and render those events
        this.renderEvents(events, isLane);
    },

    // recalculate the overlap ranges in a given row - only applicable to timelines
    retagRowEvents : function (rowNum) {
        if (!this.isTimelineView()) return;

        var cal = this.getCalendar(),
            row;
        if (isc.isA.Number(rowNum)) {
            row = this.getRecord(rowNum);
        } else {
            row = rowNum;
            rowNum = this.getRecordIndex(row);
        }
        
        var laneName = row[cal.laneNameField];
        
        // 1) remove the ranges that appear in this column
        this.removeOverlapRanges(this.getLaneOverlapRanges(laneName));
        
        // 2) get a list of events that will be in this lane (only runs for timelines, rows are lanes)
        var startDate = this.startDate,
            endDate = this.endDate
        ;
        var events = this.findEventsInRange(startDate, endDate, laneName);

        // 3) re-tag and render those events
        this.renderEvents(events, true);
    },

    retagOverlapRange : function (startDate, endDate, lane) {
        // 1) get any existing ranges that touch the passed one, merge them together and
        // then use the extents of the resulting range to retag events
        var cal = this.getCalendar(),
            touchedRanges = this.getTouchedOverlapRanges(startDate, endDate, lane),
            range = touchedRanges ? touchedRanges[0] : null,
            start = startDate.duplicate(),
            end = endDate.duplicate()
        ;

        if (range) {
            touchedRanges.removeAt(0);
            this.mergeOverlapRanges(touchedRanges, range);
            start = range[cal.startDateField];
            end = range[cal.endDateField];
            this.removeOverlapRanges(touchedRanges);
            this.removeOverlapRanges([range]);

            // 2) get the list of events that are in the (merged range's) date range and lane
            var events = this.findEventsInRange(start, end, lane, range.events);
        
            // 3) re-tag and render those events
            this.renderEvents(range.events, (lane != null));
        } else {
            // 2) get the list of events that are in the (merged range's) date range and lane
            var events = this.findEventsInRange(start, end, lane, cal.data);

            // 3) re-tag and render those events
            this.renderEvents(events, (lane != null));
        }
    },

    sortForRender : function (events) {
        
        var cal = this.getCalendar(),
            specifiers = [];
        if (this.isTimelineView() || (this.isDayView() && cal.showDayLanes)) {
            specifiers.add({ property: cal.laneNameField, direction: "ascending" });
        }
        if (this.isTimelineView() && cal.overlapSortSpecifiers) {
            specifiers.addList(cal.overlapSortSpecifiers);
        } else {
            specifiers.addList([
                { property: "_slotNum", direction: "ascending" },
                { property: cal.startDateField, direction: "ascending" }
            ]);
        }
        events.setSort(specifiers);
    },
    renderEvents : function (events, isLane) {
        
        // tag the data - this causes sorting and building of overlapRanges for all of the 
        // passed events
        this.tagDataForOverlap(events, isLane);
        // sort the affected events to make zOrdering happen from left to right
        this.sortForRender(events);
        var cal = this.getCalendar(),
            visibleCols = this.body.getVisibleColumns(),
            _this = this;
        for (var i=0; i<events.length; i++) {
            var event = events.get(i),
                props = event._overlapProps,
                laneIndex = isLane ? _this.getLaneIndex(event[cal.laneNameField]) : null
            ;
            if (!isLane || (laneIndex >= visibleCols[0] && laneIndex <= visibleCols[1])) {
                // size the eventWindow for each passed event
                var win = cal._findEventWindow(event, _this);
                if (win) {
                    win.event = event;
                    _this.sizeEventWindow(win, _this);
                }
            }
        };
    },

    //------------------------------------------------------    
    // range building and rendering stuff
    //------------------------------------------------------    

    sizeEventWindow : function (eventWin, forceRedraw) {
        //var doDebug = (eventWin.event.assayEventId == 29);
        var cal = this.getCalendar(), 
            event = eventWin.event,
            isTimeline = this.isTimelineView(),
            useLanes = isTimeline || (this.isDayView() && cal.showDayLanes)
        ;

        if (Array.isLoading(event)) return;        

        var eTop, eLeft, eWidth, eHeight;
            laneIndex = this.getLaneIndex(event[this.laneNameField])
        ;

        if (isTimeline) {
            if (laneIndex < 0) laneIndex = this.data.findIndex(this.laneNameField, event[this.laneNameField]);
            eHeight = this.getLaneHeight(laneIndex);

            // calculate event width
            eWidth = this._getEventBreadth(eventWin);
            

            // calculate event left
            eLeft = this.getEventLeft(event);
            //isc.logWarn("sizeEventWindow:" + [eWidth, eLeft]);
        } else {
            var colNum;
            if (this.isDayView() && cal.showDayLanes) {
                var laneIndex = cal.lanes.findIndex("name", event[cal.laneNameField]);
                if (laneIndex < 0) laneIndex = cal.lanes.findIndex(cal.laneNameField, event[cal.laneNameField]);
                colNum = laneIndex;
            } else {
                if (this.isDayView()) colNum = 0;
                else colNum = this.getColFromDate(event[cal.startDateField]);
            }
            eLeft = this.body.getColumnLeft(colNum);
            eWidth = this.body.getColumnWidth(colNum);
            
            var rowSize = this.body.getRowHeight(1),
                // catch the case where the end of the event is on 12am, which happens when an
                // event is dragged or resized to the bottom of the screen
                eStartDate = event[cal.startDateField],
                eEndDate = event[cal.endDateField],
                eHrs = eEndDate.getHours() == 0 
                        && eEndDate.getDate() != eStartDate.getDate() 
                        ? 24 : eEndDate.getHours(),
                // if the event ends on the next day, render it as ending on the last hour of the 
                // current day
                spansDays = false,
                minsPerRow = cal.getMinutesPerRow(this),
                rowsPerHour = cal.getRowsPerHour(this)
            ;
            
            if (event[cal.endDateField].getDate() > event[cal.startDateField].getDate()) {
                spansDays = true;
                eHrs = 24;
            }

            eTop = event[cal.startDateField].getHours() * (rowSize * rowsPerHour);

            // each (rowSize * 2) represents one hour, so we're doing (hour diff) * (1 hour height)
            eHeight = (eHrs - event[cal.startDateField].getHours()) * (rowSize * rowsPerHour);
            
            eHeight -= 1;

            // for border overlap
            if (cal.weekEventBorderOverlap && this.isWeekView()) eWidth += 1; 
            if (event[cal.startDateField].getMinutes() > 0) {
                eHeight -= cal.getMinutePixels(event[cal.startDateField].getMinutes(), rowSize, this);
                eTop += cal.getMinutePixels(event[cal.startDateField].getMinutes(), rowSize, this);
            }
            if (event[cal.endDateField].getMinutes() > 0 && !spansDays) {
                eHeight += cal.getMinutePixels(event[cal.endDateField].getMinutes(), rowSize, this);
            }

            // for border overlap
            if (cal.weekEventBorderOverlap && this.isWeekView()) eWidth += 1;

            if (cal.eventsOverlapGridLines) {
                eLeft -= 1;
                eWidth += 1;
                eTop -= 1;
                eHeight += 1;
            }
            
        }

        var eTitle = cal.getEventTitle(event, this),
            style = ""
        ;
        if (event.headerBackgroundColor) style += "backgroundColor: " + event.headerBackgroundColor + ";";
        if (event.headerTextColor) style += "backgroundColor: " + event.headerTextColor + ";";
        if (style != "") eTitle = "<span style='" + style + "'>" + eTitle + "<span>";
        eventWin.setTitle(eTitle);

        // hide and show to avoid having the title be stale when the event win is moved...
        // otherwise you have the old title flashing briefly when the calendar is scrolled
        // see calendar.addEvent
        // Note: this used to be a call to refresh(), but that still caused flickering and in
        // some cases caused scrolling to break by seemingly moving focus
        // only run updateColors() now if we aren't going to do so from eventWin.show() later
        if (forceRedraw) eventWin.hide();
        //else eventWin.updateColors();
        
        //if (eventWin.headerLabel) eventWin.headerLabel.setContents(cal.getEventTitle(event));
        //else eventWin.setTitle(cal.getEventTitle(event));

        if (isTimeline) {
            eTop = this.getRowTop(laneIndex);

            if (this.timelineEventPadding > 0) {
                eTop += this.timelineEventPadding;
                eLeft += this.timelineEventPadding;
                eWidth -= (this.timelineEventPadding * 2);
                eHeight -= (this.timelineEventPadding * 2);
            }

            if (cal.eventsOverlapGridLines) {
                if (eLeft > 0) eLeft -= 1;
                eWidth += 1;
                eTop -= 1;
                eHeight += 1;
            }

            if (this.eventDragGap > 0) {
                eWidth -= this.eventDragGap;
            }
        }

        //if (doDebug) isc.logWarn('sizeEventWindow:' + [daysFromStart, cal.startDate]);
        this.adjustDimensionsForOverlap(eventWin, eLeft, eTop, eWidth, eHeight);

        // set description after resize so percentage widths can be respected in html that may
        // be in the description
        if (cal.showEventDescriptions != false) {
            eventWin.setDescriptionText(event[cal.descriptionField]);    
        } else {
            eventWin.setDescriptionText(event[cal.nameField]);
        }

        if (isTimeline) {
            // draw leading and trailing lines
            if (event[cal.leadingDateField] && event[cal.trailingDateField]) {
                if (eventWin._lines) this.addLeadingAndTrailingLines(eventWin);
                // split this onto another thread so that ie doesn't pop the 
                // slow script warning. Applies to first draw only.
                else this.delayCall("addLeadingAndTrailingLines", [eventWin]);
            }
        }

    },
    
    adjustDimensionsForOverlap : function (eventWin, left, top, width, height) {
        var cal = this.getCalendar(),
            overlapProps = eventWin.event._overlapProps,
            isTimeline = this.isTimelineView()
        ;
        //isc.logWarn('adjustDimForOverlap:' + eventWin.event.EVENT_ID + this.echoFull(overlapProps));
        //overlapProps = false;
        if (overlapProps && overlapProps.totalSlots > 0) {
            var slotSize = isTimeline ? Math.floor(height / overlapProps.totalSlots) :
                    Math.floor(width / overlapProps.totalSlots)
            ;
            if (isTimeline) {
                height = slotSize;
                if (overlapProps.slotCount) height *= overlapProps.slotCount;
                if (overlapProps.totalSlots > 1) {
                    height -= Math.floor(this.timelineEventPadding / (overlapProps.totalSlots));
                }
                top = top + Math.floor((slotSize * (overlapProps.slotNum - 1)));
                if (overlapProps.slotNum > 1) top += (this.timelineEventPadding * (overlapProps.slotNum-1));
            } else {
                width = slotSize;
                if (overlapProps.slotCount) Math.floor(width *= overlapProps.slotCount);
                if (overlapProps.totalSlots > 1) {
                    //width -= Math.floor(this.timelineEventPadding / (overlapProps.totalSlots));
                }
                left = left + Math.floor((slotSize * (overlapProps.slotNum - 1)));
                if (cal.eventOverlap && overlapProps._drawOverlap != false) {
                    if (overlapProps.slotNum > 1) {
                        left -= Math.floor(slotSize * (cal.eventOverlapPercent / 100));
                        width += Math.floor(slotSize * (cal.eventOverlapPercent / 100));
                    }
                }
                // remove some width for the eventDragGap - do this after all the other 
                // manipulation to avoid percentage calculations returning different values
                var lastSlot = !overlapProps ? true :
                    (overlapProps.slotNum == overlapProps.totalSlots || 
                    (overlapProps.slotNum + overlapProps.slotCount) - 1 
                        == overlapProps.totalSlots)
                ;
                if (lastSlot) {
                    // leave an eventDragGap to the right of right-aligned events to allow 
                    // drag-creation of overlapping events
                    width -= cal.eventDragGap;
                }
            }
            // add a pixel of height to all overlapped events so that their borders are flush 
            if (overlapProps.totalSlots > 1 && cal.eventsOverlapGridLines) height += 1;
        }

        eventWin.renderEvent(top, left, width, height);

        /*
        eventWin.resizeTo(width, height);
         // continuation of ugly hack from getNewEventWindow:
        // for some reason the header label doesn't respect the sizing of its
        // parent, so make sure we resize it here.
        if (eventWin._customHeader) eventWin.header.resizeTo(width, height);
        
        var moved = eventWin.moveTo(left, top);
        if (!isTimeline) eventWin.bringToFront();
        */
    },


    tagDataForOverlap : function (data, lane) {
        if (data.getLength() == 0) return;
        var cal = this.getCalendar(),    
            priorOverlaps = [], // moving window of overlapping events
            overlapMembers = 0, // number of events in the current overlap group 
            currentOverlapTot = 0, // max number of events that overlap each other in the current overlap group
            maxTotalOverlaps = 0, // max number of events that overlap each other in current lane
            isTimeline = this.isTimelineView()
        ;
        
        if (cal.eventAutoArrange == false) return;
        
        this.forceDataSort(data);

        var firstEvent = data.get(0), // the first event in the passed data
            currLane =  firstEvent[cal.laneNameField] // current lane we're dealing with
        ;

        var processedEvents = [];
        
        data.setProperty("_overlapProps", null);
        data.setProperty("_slotNum", null);

        var useLanes = this.isTimelineView() || (this.isDayView() && cal.showDayLanes);

        var olRanges = this.updateOverlapRanges(data);

        var rangeSort = [];
        if (isTimeline || (this.isDayView() && cal.showDayLanes)) {
            rangeSort.add({ property: cal.laneNameField, direction: "ascending" });
        }
        if (isTimeline && cal.overlapSortSpecifiers) {
            rangeSort.addList(cal.overlapSortSpecifiers);
        } else {
            rangeSort.add({ property: cal.startDateField, direction: "ascending" });
            rangeSort.add({ property: "eventLength", direction: "descending" });
        }

        for (var j = 0; j<olRanges.length; j++) {
        
            var range = olRanges[j];
            
            var innerData = range.events;

            innerData.map(function (mEvent) {
                mEvent.eventLength = mEvent[cal.endDateField].getTime() -
                    mEvent[cal.startDateField].getTime();
            });
            
            innerData.setSort(rangeSort);

            var usedEvents = [];
            
            var maxSlotNum = 1;
            
            for (var i = 0; i < innerData.getLength(); i++) {
                var event = innerData.get(i);
                
                lane = event[cal.laneNameField];
                
                event._overlapProps = {};
                
                var sameDateSlot = null;

                var tempSlotNum = 1;

                if (usedEvents.length > 0) {
                    var tempOverlaps = [],
                        foundSlot = false,
                        tempStartSlot = 1,
                        minFoundSlot=1, 
                        maxFoundSlot=1
                    ;
                    for (var k=0; k<usedEvents.length; k++) {
                        var uEvent = usedEvents[k],
                            r = isc.addProperties({}, event),
                            ueProps = uEvent._overlapProps
                        ;
                        //r[cal.laneNameField] = lane;
                        if (this.eventsOverlap(r, uEvent, useLanes)) {
                            // if this previously used event overlaps the current event, we 
                            // need to work out the slotNum and endSlotNum for the new
                            // event...
                            
                            if (cal.eventOverlap) {
                                if (!cal.eventOverlapIdenticalStartTimes) {
                                    var sameDates = (r[cal.startDateField].getTime() ==
                                            uEvent[cal.startDateField].getTime());

                                    if (sameDates) {
                                        sameDateSlot = ueProps.slotNum;
                                    }
                                } else {
                                    event._overlapProps._drawOverlap = true;
                                }
                            }

                            
                            // the current event overlaps this previous event
                            if (!foundSlot) {
                                // found first overlapper
                                foundSlot = true;
                                if (ueProps.slotNum >= tempSlotNum) {
                                    //if (ueProps.slotNum > minFoundSlot) minFoundSlot = ueProps.slotNum;
                                    //if (ueProps.slotNum > maxFoundSlot) maxFoundSlot = ueProps.slotNum;
                                    //tempStartSlot = uEvent._overlapProps.slotNum - tempSlotNum;
                                    if (ueProps.slotNum == tempSlotNum+1) {
                                        //endSlotNum = ueProps.slotNum;
                                        event._overlapProps.endSlotNum = ueProps.slotNum;
                                        event._overlapProps.slotCount = ueProps.slotNum - tempSlotNum;
                                        break;
                                    } else if (ueProps.slotNum == tempSlotNum) {
                                        tempSlotNum++;
                                        ueProps.endSlotNum = tempSlotNum;
                                        ueProps.slotCount = tempSlotNum - ueProps.slotNum;
                                    }
                                    continue;
                                    //break
                                }
                            }
                            
                            if (ueProps.slotNum == event._overlapProps.endSlotNum) {
                                event._overlapProps.slotCount = event._overlapProps.endSlotNum - tempSlotNum;
                                break;
                            }
                            if (ueProps.slotNum == tempSlotNum) {
                                var delta = ueProps.slotCount;
                                if (delta == null) delta = 1;
                                // the previous event is already using this slot or a later one
                                tempSlotNum = ueProps.slotNum + 1;
                            } else if (ueProps.slotNum > tempSlotNum) {
                                event._overlapProps.slotCount = ueProps.slotNum - tempSlotNum;
                                break;
                            }
                            if (ueProps.slotCount == null) {
                                tempOverlaps.add(uEvent);
                            }
                        }
                    }
                    if (tempOverlaps.length) {
                        for (var k=0; k<tempOverlaps.length; k++) {
                            var tOL = tempOverlaps[k];
                            tOL._overlapProps.slotCount = tempSlotNum - tOL._overlapProps.slotNum;
                        }
                    }
                }
                event._overlapProps.slotNum = event._slotNum = tempSlotNum;
                
                if (sameDateSlot != null && sameDateSlot < tempSlotNum) 
                    event._overlapProps._drawOverlap = false;

                if (tempSlotNum > maxSlotNum) {
                    maxSlotNum = tempSlotNum;
                }
                
                usedEvents.add(event);
            }

            // update the total slots for all events (they're all in the same range)
            innerData.map(function (item) {
                if (!item._overlapProps.slotCount) {
                    item._overlapProps.slotCount = (maxSlotNum - item._overlapProps.slotNum) + 1;
                }
                item._overlapProps.totalSlots = maxSlotNum;
            });
        
        }
        
        return processedEvents;
    },
    
    //-------------------------rendering events on demand-----------------------------

    getVisibleDateRange : function () {
        var cal = this.getCalendar();
        if (!this.renderEventsOnDemand) {
            if (this.isTimelineView()) {
                return [this.startDate.duplicate(), this.endDate.duplicate()];    
            } else if (this.isWeekView()) {
                return [cal.chosenWeekStart, cal.chosenWeekEnd];
            } else if (this.isDayView()) {
                return [isc.DateUtil.getStartOf(cal.chosenDate, "D"),
                        isc.DateUtil.getEndOf(cal.chosenDate, "D")];
            } else if (this.isMonthView()) {
                return [isc.DateUtil.getStartOf(cal.chosenDate, "M"),
                        isc.DateUtil.getEndOf(cal.chosenDate, "M")];
            }
        }
        
        var startX = this.body.getScrollLeft(),
            endX = startX + this.body.getVisibleWidth(),
            startCol = this.body.getEventColumn(startX + 1),
            endCol = this.body.getEventColumn(endX),
            startY = this.body.getScrollTop(),
            endY = startY + this.body.getVisibleHeight(),
            startRow = this.body.getEventRow(startY + 1),
            endRow = this.body.getEventRow(endY)
        ;
        
        if (endRow < 0) endRow = this.data.getLength()-1;
        if (endCol < 0) endCol = this.body.fields.length - 1;
        
        var startDate = this.getCellDate(startRow, startCol),
            endDate = this.getCellDate(endRow, endCol)
        ;
        
        //if (endDate.getTime() < startDate.getTime()) endDate = isc.DateUtil.getEndOf(endDate, "D");
        
        return [ startDate, endDate ];


        // round rangeStart to the nearest column start, otherwise events that are on the left
        // edge may not get rendered when sizeEventsToGrid is true
        //var rangeStart = this.getDateFromPoint(startPos, null, null, true);
        //var rangeEnd = this.getDateFromPoint(endPos);
        
        //return [rangeStart, rangeEnd];
    },
    
    getVisibleRowRange : function () {
        if (!this.renderEventsOnDemand) {
            return [0, this.data.getLength()];    
        }
        return this.getVisibleRows();
    },

    getVisibleColumnRange : function () {
        if (!this.renderEventsOnDemand) {
            return [0, this.data.getLength()];    
        }
        
        return this.body.getVisibleColumns();
    },

    // refreshEvents is only called when data changes, etc. 
    // refreshVisibleEvents is called whenever the view is scrolled and only draws visible events.
    // see scrolled()
    refreshVisibleEvents : function (events) {
        if (!this.body || !this.body.isDrawn()) return;
        //this._ignoreDataChanged = true;
        //this.forceDataSort();

        // get visible events and add them. addEvent takes care of reclaiming and positioning
        events = events || this.getVisibleEvents();
        
        var eventsLen = events.getLength();
        this.logDebug('refreshing visible events','calendar');  
        for (var i = 0; i < eventsLen; i++) {
            //if (i > 20) break;
            var event = events.get(i),
                shouldAdd = true
            ;
            //isc.logWarn('refreshing event:' + event.id);
            if (!this.poolEventWindows) {
                // not pooling canvases - check if this event was already added
                shouldAdd = !this._drawnEvents.contains(event); //(this.getCurrentEventCanvas(event) == null);
                if (shouldAdd) this._drawnEvents.add(event);
            }
            if (shouldAdd) {
                this.addEvent(event, i, false);
            }
        }
        // hide events after repositioning visible events, starting right after the number of the 
        // last positioned events. This prevents stale (not in view) events from hanging around.
        if (this.poolEventWindows) this.clearEvents(eventsLen);
        var cal = this.getCalendar();
        if (cal.eventsRendered && isc.isA.Function(cal.eventsRendered)) 
            cal.eventsRendered();
    },
    
    getVisibleEvents : function () {               
        var cal = this.getCalendar(),
            isTimeline = this.isTimelineView(),
            hasDayLanes = cal.showDayLanes && this.isDayView(),
            dateRange = this.getVisibleDateRange(),
            useLanes = (isTimeline || hasDayLanes),
            laneRange = useLanes ?
                (isTimeline ? this.getVisibleRowRange() : this.getVisibleColumnRange()) : null
        ;

        if (!this.renderEventsOnDemand) return cal.data;

        var events = cal.data,
            startMillis = dateRange[0].getTime(),
            endMillis = dateRange[1].getTime(),
            eventsLen = events.getLength(),
            results = []
        ;

        for (var i = 0; i < eventsLen; i++) {
            var event = events.get(i);
            
            if (isc.isA.String(event)) return [];
            
            if (!event) {
                isc.logWarn('getVisibleEvents: potentially invalid index: ' + i);  
                break;
            }

            // build a range object to compare against
            var rangeObj = {};

            if (useLanes) {
                var laneIndex = this.getEventLaneIndex(event);

                // optimization - if the lane isn't in the viewport, continue
                if (laneIndex == null || laneIndex < laneRange[0] || laneIndex > laneRange[1]) 
                    continue;

                rangeObj[cal.laneNameField] = event[cal.laneNameField];
            }

            if (isTimeline) {
                // if we're not showing lead-trail lines use start/endDate fields instead to 
                // determine overlap
                if (event[cal.leadingDateField] && event[cal.trailingDateField]) {
                    rangeObj[cal.leadingDateField] = dateRange[0];
                    rangeObj[cal.trailingDateField] = dateRange[1];
                } else {
                    rangeObj[cal.startDateField] = dateRange[0];
                    rangeObj[cal.endDateField] = dateRange[1];    
                }
            } else {
                rangeObj[cal.startDateField] = dateRange[0];
                rangeObj[cal.endDateField] = dateRange[1];
            }

            // event ends before the range start
            //if (event[cal.endDateField].getTime() <= startMillis) continue;
            // event starts after the range end
            //if (event[cal.startDateField].getTime() >= endMillis) continue;

            //sameLaneOnly = useLanes ? !cal.canEditEventLane(event) : false;
            //if (this.eventsOverlap(rangeObj, event, sameLaneOnly)) {
            if (this.eventsOverlap(rangeObj, event, useLanes)) {
                results.add(event);
            }
        }

        return results;
    },

    getCurrentEventCanvas : function (event) {
        var eventCanvasID = this.getCalendar().getEventCanvasID(this, event);
        return window[eventCanvasID];
    },

    clearEventCanvas : function (eventCanvas, destroy) {
        if (eventCanvas) {
            if (eventCanvas.hide) eventCanvas.hide();
            if (destroy) {
                eventCanvas.destroy();
                eventCanvas = null;
            }
        }
    },

    clearEvents : function (start, destroy) {
        var pool = this._eventCanvasPool;
        // hide all the canvases in the _eventCanvasPool
        if (!this.body || !this.body.children || !pool) return;
        if (!start) start = 0;
        //isc.logWarn('clearing events');

        for (var i = start; i < pool.length; i++) {
            //isc.logWarn('hiding event:' + i);
            if (pool[i]) {
                this.clearEventCanvas(pool[i], destroy);
            }
        }
        
        pool.removeEmpty();
    }

}

// END gridProps 
});


// DaySchedule
// --------------------------------------------------------------------------------------------
isc.ClassFactory.defineClass("DaySchedule", "ListGrid");


isc.DaySchedule.changeDefaults("bodyProperties", {
    //childrenSnapToGrid: true,
    
    snapToCells: false,
    suppressVSnapOffset: true
//    redrawOnResize:true
});

isc.DaySchedule.addProperties({
    //defaultWidth: 300,
    //defaultHeight: 300,
    autoDraw: false,
    canSort: false,
    canResizeFields: false,
    canReorderFields: false,
    showHeader: false,
    showHeaderContextMenu: false,
    showAllRecords: true,
    fixedRecordHeights: true,
    labelColumnWidth: 60,
    labelColumnAlign: "right",
    showLabelColumn: true,
    labelColumnPosition: "left",
    labelColumnBaseStyle: "labelColumn",
    
    // show cell-level rollover
    showRollOver:true,
    useCellRollOvers:true,

    // needed to avoid the grid scrolling to the 0,0 when clicking body children (events)
    hiliteRowOnFocus: false,
    
    // disable autoFitting content on header double clicking
    canAutoFitFields : false,
    
    canSelectCells:true,

    poolEventWindows: true,
    initWidget : function () {
        this.fields = [];

        this.addProperties(this.creator.viewProps);
        this.addProperties(this.creator.gridProps);
        this.addProperties(this.creator.dayViewProperties);

        var cal = this.getCalendar();

        if (cal.showDayLanes && this.isDayView() && cal.alternateLaneStyles) {
            this.alternateFieldStyles = true;
            this.alternateFieldFrequency = cal.alternateFieldFrequency;
        }

        if (cal.labelColumnWidth && cal.labelColumnWidth != this.labelColumnWidth) {
            this.labelColumnWidth = cal.labelColumnWidth;
        }
        
        this.renderEventsOnDemand = cal.renderEventsOnDemand;
        this.eventDragGap = cal.eventDragGap;
        this.fields = [];

        this.Super("initWidget");

        this.rebuildFields();

        this.addAutoChild("eventDragTarget");
        this.body.addChild(this.eventDragTarget);
        
        if (isc.isAn.Array(cal.data)) {
            this._refreshEventsOnDraw = true;
            //this.refreshEvents();
        }
    },

    reorderFields : function (start, end, moveDelta) {
        this.Super("reorderFields", arguments);
        this.refreshEvents();
    },
    
    rebuildFields : function () {
        var cal = this.getCalendar(),
            fields = [],
            labelCol = {
                width: this.labelColumnWidth,
                name: "label", 
                title: " ",
                cellAlign: "right",
                calendar: cal,
                formatCellValue : function (value, record, rowNum, colNum, grid) {
                    
                    var rowsPerHour = grid.creator.getRowsPerHour(grid);
                    if (rowNum % rowsPerHour == 0) {
                        var hour = (rowNum / rowsPerHour);
                        var date = isc.Time.parseInput(hour);
                        return isc.Time.toTime(date, grid.creator.timeFormatter, true);
                    } 
                    else {
                        return "";
                    }
                }
            }
        ;
        if (this.showLabelColumn && this.labelColumnPosition == "left") {
            fields.add(labelCol);
        }

        if (cal.showDayLanes && cal.lanes && !this.isWeekView()) {
            fields[0].frozen = true;
            var d = cal.chosenDate.duplicate(),
                scaffolding = isc.DaySchedule._getEventScaffolding(cal.getMinutesPerRow(this), d),
                nDate = isc.Date.createLogicalDate(d.getFullYear(), d.getMonth(), d.getDate()),
                props = { date: nDate, align: "center", canReorder: cal.canReorderLanes }
            ;
            for (var i=0; i<cal.lanes.length; i++) {
                var lane = cal.lanes[i],
                    laneName = lane[cal.laneNameField] || lane.name,
                    p = isc.addProperties({}, props, { name: laneName })
                ;
                p[cal.laneNameField] = laneName;
                fields.add(isc.addProperties(p, lane));
                scaffolding.setProperty(laneName, "");
            }
            this.setShowHeader(true);
            if (cal.canReorderLanes) this.canReorderFields = cal.canReorderLanes;
            if (cal.minLaneWidth != null) this.minFieldWidth = cal.minLaneWidth;
            this.data = scaffolding;
        } else {
            fields[0].frozen = true;
            fields.add({name: "day1", align: "center", date: cal.chosenDate});
            if (this.isWeekView()) {
                var numDays = 8; 
                for (var i = 2; i < numDays; i++) {
                    fields.add({name: "day" + i, align: "center" } );   
                }
                this.setShowHeader(true);
            
                // hide weekends 
                if (!this.creator.showWeekends) {
                    var start = this.showLabelColumn && this.labelColumnPosition == "left" ? 1 : 0;
                
                    var weekendDays = Date.getWeekendDays();
                    for (var i = start; i < fields.length; i++) {
                    
                        var adjDay = ((i - start) + this.creator.firstDayOfWeek) % 7;
                        //isc.logWarn('here:' + [i, adjDay]);
                        if (weekendDays.contains(adjDay)) {
                            fields[i].showIf = "return false;";
                        }
                    }
                }
            } else {
                this.setShowHeader(false);
            }
            this.data = isc.DaySchedule._getEventScaffolding(cal.getMinutesPerRow(this));
        }
        if (this.showLabelColumn && this.labelColumnPosition == "right") {
            fields.add(labelCol);
        }
        
        this.setFields(fields);
    },
    
    getDateFromPoint : function (x, y, round, useSnapGap) {
        var cal = this.getCalendar();

        if (useSnapGap) {
            // when click/drag creating, we want to snap to the eventSnapGap
            //y -= y % cal.eventSnapGap;
        }
        if (x == null && y == null) {
            // if no co-ords passed, assume mouse offsets into the body
            y = this.body.getOffsetY();
            x = this.body.getOffsetX();
        }
        
        var rowNum = this.body.getEventRow(y),
            rowHeight = this.body.getRowHeight(rowNum),
            rowTop = this.body.getRowTop(rowNum),
            colNum = this.body.getEventColumn(x),
            badCol = (colNum < 0)
        ;

        if (colNum == -1) colNum = 0;
        else if (colNum == -2) colNum = this.body.fields.length-1;


        // get the date for the top of the cell
        var colDate = this.getCellDate(rowNum, colNum),
            minsPerRow = cal.getMinutesPerRow(this),
            rowsPerHour = cal.getRowsPerHour(this),
            offsetY = y == null ? 0 - rowTop : y - rowTop,
            pixels = offsetY - (offsetY % cal.eventSnapGap),
            snapGapMins = minsPerRow / (rowHeight / cal.eventSnapGap),
            snapGaps = pixels / cal.eventSnapGap,
            minsToAdd = snapGapMins * snapGaps
        ;

        colDate.setMinutes(colDate.getMinutes() + minsToAdd);

        return colDate;
    },

    getCellDate : function (rowNum, colNum) {
        if (!(this.body && this.body.fields)) return null;
        var cal = this.getCalendar(),
            result;

        if (rowNum == -1) rowNum = 0;
        if (rowNum == -2) rowNum = this.data.getLength() - 1;

        var fieldDate = this.body.fields[colNum].date,
            record = this.getRecord(rowNum),
            rowTime = record ? isc.Time.parseInput(record.time) : null
        ;
        if (fieldDate && rowTime) {
            result = isc.Date.combineLogicalDateAndTime(fieldDate, rowTime);
        }
        
        return result;
    },

    getEventLeft : function (event) {
        return this.creator.getEventLeft(event, this);
    },
    
    getDateLeftOffset : function (date) {
        for (var i=0; i<this.fields.length; i++) {
            var f = this.fields[i];
            if (f._yearNum != null && f._monthNum != null && f._dateNum != null) {
                var colDate = Date.createLogicalDate(f._yearNum, f._monthNum, f._dateNum);
                if (Date.compareLogicalDates(date, colDate) == 0) {
                    return this.getColumnLeft(this.getFieldNum(f));
                }
            }
        }

        return 0;
    },

    setLanes : function (lanes) {
        this.lanes = lanes;
        this.rebuildFields();
        this.refreshEvents();
    },
    getLane : function (laneName) {
        var index = this.getLaneIndex(laneName);
        if (index >= 0) return this.body.fields[index];
    },
    getLaneIndex : function (laneName) {
        if (!this.isDayView() || !this.creator.showDayLanes) return;
        var laneIndex = this.body.fields.findIndex("name", laneName);
        if (laneIndex < 0) 
            laneIndex = this.body.fields.findIndex(this.creator.laneNameField, laneName);
        return laneIndex;
    },
    
    draw : function (a, b, c, d) {
        this.invokeSuper(isc.DaySchedule, "draw", a, b, c, d);

        this.logDebug('draw', 'calendar');
        // call refreshEvents() whenever we're drawn
        // see comment above dataChanged for the logic behind this
        
        this.body.addChild(this.eventDragTarget);
        this.eventDragTarget.setView(this);

        /*
        if (this.isDayView() && this.getCalendar().scrollToWorkday) {
            var newRowHeight = this.calcRowHeight();
            if (newRowHeight != this.getCalendar().rowHeight) {
                this.getCalendar().setRowHeight(newRowHeight);
            } else this.refreshEvents();
        } else {
            this.refreshEvents();
        }
        */

        if (this._refreshEventsOnDraw) {
            delete this._refreshEventsOnDraw;
            this.refreshEvents();
        }

        // set the snapGap after were drawn, so that we can pick up a dynamic row height.
        // this is mostly so that scrollToWorkday code works properly.
        this.setSnapGap();
        // if scrollToWorkday is set, do that here
        if (this.creator.scrollToWorkday) this.scrollToWorkdayStart();
    },
    
    setSnapGap : function () {
        // get percentage of snapGap in relation to 30 minutes, the length in minutes of a row, and 
        // multiply by row height to get pixels
        var snapGap = this.creator.eventSnapGap;
        this.body.snapVGap = Math.round((snapGap / this.creator.getMinutesPerRow(this)) 
                * this.body.getRowSize(0));
        this.body.snapHGap = null;
    },
    
    // To be used with calendar.scrollToWorkday 
    scrollToWorkdayStart : function () {
        var cal = this.getCalendar(),
            sDate;

        if (cal.scrollToWorkday) {
            var newRowHeight = this.calcRowHeight();
            if (newRowHeight != cal.rowHeight) {
                cal.setRowHeight(newRowHeight, true);
            }
        }

        var range = this.getWorkdayRange();
        sDate = range.start;

        var minsPerRow = cal.getMinutesPerRow(this),
            rowsPerHour = cal.getRowsPerHour(this),
            sRow = sDate.getHours() * rowsPerHour,
            dateMins = sDate.getMinutes(),
            remainder = dateMins % minsPerRow,
            rowDelta = Math.floor((dateMins - remainder) / minsPerRow)
        ;
        sRow += rowDelta;
        if (remainder > 0) sRow++;
        var sRowTop = cal.rowHeight * sRow;
        //this.scrollRecordIntoView(sRow, false);
        this.body.delayCall("scrollTo", [0,sRowTop]);
        //this.redraw();
    },
    
    getWorkdayRange : function () {
        var fields = this.body.fields,
            result = { start: isc.Time.parseInput("23:59"), end: isc.Time.parseInput("00:01") },
            cal = this.getCalendar(),
            date = cal.chosenDate,
            time
        ;
        
        if (this.isWeekView()) {
            // get the largest range across the week
            for (var i=0; i < fields.length; i++) {
                date = this.getDateFromCol(i);
                if (isc.isA.Date(date)) {
                    time = isc.Time.parseInput(cal.getWorkdayStart(date));
                    if (isc.Date.compareDates(result.start, time) < 0) {
                        result.start = time;
                    }
                    time = isc.Time.parseInput(cal.getWorkdayEnd(date));
                    if (isc.Date.compareDates(result.end, time) > 0) {
                        result.end = time;
                    }
                }
            }
        } else if (cal.showDayLanes) {
            // get the largest range across the lanes in the day
            for (var i=0; i < fields.length; i++) {
                var field = fields[i],
                    lane = field[cal.laneNameField]
                ;
                if (isc.isA.Date(date)) {
                    time = isc.Time.parseInput(cal.getWorkdayStart(date, lane));
                    if (isc.Date.compareDates(result.start, time) < 0) {
                        result.start = time;
                    }
                    time = isc.Time.parseInput(cal.getWorkdayEnd(date, lane));
                    if (isc.Date.compareDates(result.end, time) > 0) {
                        result.end = time;
                    }
                }
            }
        } else {
            result.start = isc.Time.parseInput(cal.getWorkdayStart(cal.chosenDate));
            result.end = isc.Time.parseInput(cal.getWorkdayEnd(cal.chosenDate));
        }
        return result;
    },
    
    calcRowHeight : function () {
        var range = this.getWorkdayRange(),
            workdayLen = range.end.getHours() - range.start.getHours(),
            cellHeight = this.getCalendar().rowHeight
        ;
        // if workdayStart > workdayEnd, just return default cellHeight
        if (workdayLen <= 0) return cellHeight;
        var rHeight = Math.floor(this.body.getViewportHeight() / 
                (workdayLen * this.creator.getRowsPerHour()));
        return rHeight < cellHeight ? cellHeight : rHeight;
    },
    getRowHeight : function (record, rowNum) {
		// when scrollToWorkday is true, the rowHeight/cellHeight has already been re-calculated, 
        // so just return it - causes issues with the frozen body if this method returns a different
        // number than the current cellHeight
        return this.getCalendar().rowHeight;
    },
    
    getDayFromCol : function (colNum) {
        if (colNum < 0) return null;
        var dayNum = this.body.fields.get(colNum)._dayNum;
        return dayNum;
    },

    getDateFromCol : function (colNum) {
        if (colNum < 0) return null;
        var fld = this.body.fields.get(colNum);
        if (fld._yearNum == null || fld._monthNum == null || fld._dateNum == null) return null;
        var newDate = new Date(fld._yearNum, fld._monthNum, fld._dateNum);
        return newDate;
    },

    getColFromDate : function (date) {
        for (var i=0; i<this.body.fields.length; i++) {
            var fld = this.body.fields.get(i);
            if (fld._yearNum == null || fld._monthNum == null || fld._dateNum == null) continue;
            var newDate = new Date(fld._yearNum, fld._monthNum, fld._dateNum);
            if (isc.Date.compareLogicalDates(date, newDate) == 0) return i;
        }
        return null;
    },

    isLabelCol : function (colNum) {
        var field = this.getField(colNum);
        return field && field.frozen;
        /*
        if (colNum == 0 && this.showLabelColumn && this.labelColumnPosition == "left") {
            return true;
        } else if (colNum == this.fields.length - 1 && this.showLabelColumn && 
            this.labelColumnPosition == "right") {
             return true;
        } else {
            return false;   
        }
        */
    },
    
    // helper function for detecting when a weekend is clicked, and weekends are disabled
    colDisabled : function (colNum) {
        var body = this.getFieldBody(colNum);
        if (!body || body == this.frozenBody) return false;
        var col = this.getLocalFieldNum(colNum);
        var dayNum = this.isWeekView() ? this.getDayFromCol(col) : this.creator.chosenDate.getDay();
        //isc.logWarn('colDisabled:' + [colNum, dayNum]);
        if (this.creator.disableWeekends 
            && Date.getWeekendDays().contains(dayNum)) {
            return true;        
        } else {
            return false;   
        }
    },
    
    // helper function to refresh dayView cell styles for weekend disabling
    refreshStyle : function () {
        if (!this.body) return;
        if (this.isWeekView() || this.getCalendar().showDayLanes) {
            // need to refresh all cells to cater for weekView (for workday handling)
            this.markForRedraw();
            return;
        }
        for (var i = 0; i < this.data.length; i++) {
            this.body.refreshCellStyle(i, 1);    
        }
    },
    
    // use the chosen week start to figure out the base date, then add the headerFieldNum
    // to that to get the appropriate date. Use dateChooser.dateClick() to simplify code.
    headerClick : function (headerFieldNum, header) {
        var cal = this.getCalendar();

        if (this.isLabelCol(headerFieldNum)) return true;
        if (cal.showDayLanes && !this._isWeek) return true;

        var fld = this.getField(headerFieldNum);
        cal.dateChooser.dateClick(fld._yearNum, fld._monthNum, fld._dateNum);
        cal.selectTab(0);
        return true;
    },
    
    
    getCellAlign : function (record, rowNum, colNum) {
       return this.labelColumnAlign;
    },
    
    cellMouseDown : function (record, rowNum, colNum) {       
        if (this.isLabelCol(colNum) || this.colDisabled(colNum)) return true; 
        
        // if backgroundMouseDown is implemented, run it and return if it returns false
        var startDate = this.getCellDate(this.body.getEventRow(), this.body.getEventColumn());
        if (this.creator.backgroundMouseDown && this.creator.backgroundMouseDown(startDate) == false) return;

        // don't set up selection tracking if canCreateEvents is disabled
        if (!this.creator.canCreateEvents) return true;
        // first clear any previous selection   
        this.clearSelection();
        this._selectionTracker = {};
        this._selectionTracker.colNum = colNum;
        this._selectionTracker.startRowNum = rowNum;
        this._selectionTracker.endRowNum = rowNum;
        this._mouseDown = true;
        this.refreshCellStyle(rowNum, colNum);
    },
    
    cellOver : function (record, rowNum, colNum) {
        if (this._mouseDown && this._selectionTracker) {
            var refreshRowNum;
            // selecting southbound
            if (this._selectionTracker.startRowNum < this._selectionTracker.endRowNum) {
                // should select this cell
                if (rowNum > this._selectionTracker.endRowNum) {
                    refreshRowNum = rowNum;             
                } else { // should deselect the previous end row number
                    refreshRowNum = this._selectionTracker.endRowNum;
                }
                // trigger cell style update from getCellStyle
                this._selectionTracker.endRowNum = rowNum;
            // selecting northbound
            } else {
                // should select this cell
                if (rowNum < this._selectionTracker.endRowNum) {
                    refreshRowNum = rowNum;    
                } else { // should deselect the previous end row number
                    refreshRowNum = this._selectionTracker.endRowNum;
                }
                this._selectionTracker.endRowNum = rowNum;
            }
            var refreshGap = 6,
                col = this._selectionTracker.colNum;
            for (var i = refreshRowNum - refreshGap; i < refreshRowNum + refreshGap; i++) {
                // 48 1/2 hours in a day, don't refresh non-existent cells
                if (i >= 0 && i <= 47) this.refreshCellStyle(i, col);        
            }                 
        }
    },
    
    cellMouseUp : function (record, rowNum, colNum) {
        if (!this._selectionTracker) return true;

        this._mouseDown = false;
        var sRow, eRow, diff;
        // cells selected upwards
        if (this._selectionTracker.startRowNum > this._selectionTracker.endRowNum) {
            sRow = this._selectionTracker.endRowNum;
            eRow = this._selectionTracker.startRowNum;
        // cells selected downwards
        } else {
            eRow = this._selectionTracker.endRowNum;
            sRow = this._selectionTracker.startRowNum;
        }
        diff = eRow - sRow + 1;

        var startDate = this.creator.getCellDate(sRow, colNum, this);
        var endDate = this.creator.getCellDate(sRow+diff, colNum, this);

        // if backgroundClick is implemented, and there's no selection (a click, not just mouseUp), 
        // run it and bail if it returns false
        if (diff == 1 && this.creator.backgroundClick) {
            if (this.creator.backgroundClick(startDate, endDate) == false) {
                this.clearSelection();
                return;
            }
        }
        // if backgroundMouseUp is implemented, run it and bail if it returns false
        if (this.creator.backgroundMouseUp) {
            if (this.creator.backgroundMouseUp(startDate, endDate) == false) {
                this.clearSelection();
                return;
            }
        }

        //this.creator._showEventDialog(null, sRow, this._selectionTracker.colNum, diff);
        var newEvent = {};
        newEvent[this.creator.startDateField] = startDate;
        newEvent[this.creator.endDateField] = endDate;
        if (this.creator.showDayLanes && this.creator.dayViewSelected()) 
            newEvent[this.creator.laneNameField] = this.getField(colNum).name;
        this.creator.showNewEventDialog(newEvent);
    },

    getCellStyle : function (record, rowNum, colNum) {
        var cal = this.getCalendar(),
            bStyle = this.getBaseStyle(record, rowNum, colNum)
        ;

        if (this.isLabelCol(colNum)) return bStyle;
        if (this.colDisabled(colNum)) return bStyle + "Disabled";

        if (this._selectionTracker && this._selectionTracker.colNum == colNum) {
            var sRow = this._selectionTracker.startRowNum,
                eRow = this._selectionTracker.endRowNum;
            // if rowNum is within start and end of selection, return selected style
            if (rowNum >= sRow && rowNum <= eRow || rowNum >= eRow && rowNum <= sRow) {
                if (bStyle == cal.workdayBaseStyle) return bStyle + "Selected";
                return cal.selectedCellStyle;    
            }
        } 
        
        // odd row in dayView, with alternateRecordStyles
        if (!this._isWeek && this.alternateRecordStyles && rowNum % 2 != 0) {
            if (bStyle == cal.workdayBaseStyle) return bStyle;
            return bStyle + "Dark";
        }
        
        // odd column in dayView with showDayLanes and alternateFieldStyles
        if (cal.dayViewSelected() && cal.showDayLanes && this.alternateFieldStyles && colNum % 2 != 0) {
            if (bStyle == cal.workdayBaseStyle) return bStyle;
            return bStyle + "Dark";
        }

        return bStyle;
    },
    
    // day/weekView
    getBaseStyle : function (record, rowNum, colNum) {
        var cal = this.getCalendar(),
            date = cal.getCellDate(rowNum, colNum, this),
            style = date ? cal.getDateStyle(date, rowNum, colNum, this) : null,
            isWeek = this.isWeekView()
        ;

        if (style) {
            // getDateStyle() returned a style - just return that
            return style;
        }

        if (this.isLabelCol(colNum)) return this.labelColumnBaseStyle;

        if (!cal.showWorkday) return this.baseStyle;

        var body = this.getFieldBody(colNum),
            bodyCol = colNum
        ;
        if (body == this.body) bodyCol = this.getLocalFieldNum(colNum);

        var dayNum = isWeek ? this.getDayFromCol(bodyCol) : cal.chosenDate.getDay();

        // workdayStart/end need to be based on current date and not just parsed workdayStart.
        // this fixes an issue where parsed date could have the wrong day.
        var wStart = isWeek ? this.getDateFromCol(bodyCol) : cal.chosenDate.duplicate(),
            wEnd = wStart.duplicate(),
            currRowTime = date ? date.duplicate() : null,
            lane = cal.showDayLanes ? this.body.getField(bodyCol)[cal.laneNameField] : null
        ;

        if (currRowTime) {
            var parsedStart = isc.Time.parseInput(cal.getWorkdayStart(currRowTime, lane)),
                parsedEnd = isc.Time.parseInput(cal.getWorkdayEnd(currRowTime, lane))
            ;

            // need to set hours and minutes of start and end to the same as workdayStart and
            // workdayEnd
            wStart.setHours(parsedStart.getHours());
            wStart.setMinutes(parsedStart.getMinutes());
            wEnd.setHours(parsedEnd.getHours());
            wEnd.setMinutes(parsedEnd.getMinutes());

            var dayIsWorkday = cal.dateIsWorkday(currRowTime, lane);
            currRowTime = currRowTime.getTime();
            if (dayIsWorkday && wStart.getTime() <= currRowTime && currRowTime < wEnd.getTime()) {
                return cal.workdayBaseStyle;
            } else {
                return this.baseStyle;
            }
        } else {
            return this.baseStyle;
        }
    },

    clearSelection : function () {
        if (this._selectionTracker) {
            var sRow, eRow, colNum = this._selectionTracker.colNum;
            // establish order of cell refresh
            if (this._selectionTracker.startRowNum < this._selectionTracker.endRowNum) {
                sRow = this._selectionTracker.startRowNum;
                eRow = this._selectionTracker.endRowNum;
            } else {
                sRow = this._selectionTracker.endRowNum;
                eRow = this._selectionTracker.startRowNum;
            }
            // remove selection tracker so cells get reset to baseStyle
            this._selectionTracker = null;
            for (var i = sRow; i < eRow + 1; i++) {
                this.refreshCellStyle(i, colNum);    
            }
        }
    },

    refreshEvents : function () {
        var cal = this.getCalendar();
        // bail if the grid hasn't been drawn yet, or hasn't any data yet
        if (!this.body || !cal.hasData()) return;

        if (!this.poolEventWindows) {
            // not pooling event canvases, destroy the ones created by the last refresh
            this.clearEvents(0, true);
            this._drawnEvents = [];
        }

        var startDate, endDate;
        if (this.isWeekView()) {
            startDate = cal.chosenWeekStart;
            endDate = cal.chosenWeekEnd;
        } else {
            startDate = new Date(cal.year, cal.month, cal.chosenDate.getDate(),0, 0);
            endDate = new Date(cal.year, cal.month, cal.chosenDate.getDate(),23, 59);
        }
        var startMillis = startDate.getTime(),
            endMillis = endDate.getTime()
        ;

        this.removeOverlapRanges(this.overlapRanges);

        //var events = cal._getEventsInRange(startDate, endDate);
        var allEvents = isc.isA.Array(cal.data) ? cal.data : cal.data.getRange(0, cal.data.getLength());
        var events = [];

        for (var i=0; i< allEvents.getLength(); i++) {
            var event = allEvents.get(i);
            if (!isc.isA.String(event)) {
                var sDate = (event[cal.leadingDateField] || event[cal.startDateField]).getTime(),
                    eDate = (event[cal.trailingDateField] || event[cal.endDateField]).getTime()
                ;
                if ((sDate >= startMillis && sDate < endMillis) ||
                    (eDate > startMillis && eDate <= endMillis))
                {
                    events.add(event);
                }
            }
        };

        this.logDebug("refreshing " + this.viewName + " - " + events.length + ' events','calendar');
        
        //var laneNames = isc.getKeys(cal.getLaneMap());

        var _this = this;
        
        //laneNames.map(function (laneName) {
        //    _this.retagLaneEvents(laneName);
        //});
        
        this.tagDataForOverlap(events);

        //this.refreshVisibleEvents(events);
        this.refreshVisibleEvents();
        
        if (this._scrollRowAfterRefresh) {
            this.body.scrollTo(null, this._scrollRowAfterRefresh);
        }
        delete this._needsRefresh;
        delete this._scrollRowAfterRefresh;

    },

    poolEventWindow : function (canvas) {
        if (this.body) {
            canvas.event = null;
            canvas._availableForUse = true;
            if (!this._eventCanvasPool) this._eventCanvasPool = [];
            this._eventCanvasPool.add(canvas);
            return true;
        } else return false;
    },
    getPooledEventWindow : function (event, eventIndex) {
        if (!this.body) return;
        if (!this._eventCanvasPool) this._eventCanvasPool = [];
        var pool = this._eventCanvasPool,
            cal = this.getCalendar(),
            canvas
        ;
        if (pool.length > 0) {
            //if (event) canvas = this.getCurrentEventCanvas(event);
            if (canvas && canvas._availableForUse == false) {
                var existingCanvasIndex = pool.indexOf(canvas);
                var moveThisWin = pool[eventIndex];
                pool[eventIndex] = canvas;
                pool[existingCanvasIndex] = moveThisWin;
            } else {
                if (eventIndex == null) {
                    eventIndex = pool.findIndex("_availableForUse", true);
                }
                // ...reclaim an event from the event bin
                canvas = pool[eventIndex];
                if (!canvas) return null;
                cal.setEventCanvasID(this, event, canvas.ID);
            }
            canvas._availableForUse = false;
        }
        return canvas;
    },
    destroy : function () {
        this.destroyEvents(true);
        if (this.body) this._eventCanvasPool = null;
        this.Super("destroy", arguments);
    },
    destroyEvents : function () {
        if (!this.body || !this.body.children) return;

        for (var i = this.body.children.length-1; i >= 0 ; i--) {
            var child = this.body.children[i];
            if (child && child.isEventCanvas) {
                this.body.removeChild(child);
                child.destroy();
                child = null;
            }
        }

    },
    
    addEvent : function (event, eventIndex, retag) {
        if (!this._eventCanvasPool) this._eventCanvasPool = [];

       // clear any cell selection that has been made
        this.clearSelection();

        var cal = this.getCalendar(),
            win = cal._getNewEventWindow(event, eventIndex, this),
            hideWindow = false
        ;

        if (win.isDrawn()) win.hide();

        win._parentView = this;

        win._isWeek = this._isWeek;
        if (this.body) this.body.addChild(win);
        
        if (!this.isWeekView() && cal.showDayLanes) {
            // don't show the eventWindow if it's lane isn't visible
            var laneName = event[cal.laneNameField],
                lane = cal.lanes.find("name", laneName)
            ;
            if (!lane) hideWindow = true;
        }

        win.setCanDragReposition(cal.canDragEvents, this.eventDragTarget);

        if (!hideWindow && this.body && this.body.isDrawn()) {
            // if the "retag" param was passed, this is an event that hasn't been rendered 
            // before (it comes from processSaveResponse() after an "add" op) - rather than 
            // just resizing the window, get a list of overlapRanges that intersect the new
            // event, combine the event-list from each of them and add the new event,
            // remove the existing ranges and then retag the event-list
            if (retag) {
                this.retagOverlapRange(event[cal.startDateField], event[cal.endDateField], event[cal.laneNameField]);
            } else {
                this.sizeEventWindow(win);
            }
        }
    },

    removeEvent : function (event) {
        var arr = this.body.children || [];
        for (var i = 0; i < arr.length ; i++) {
            if (arr[i] && arr[i].isEventCanvas && arr[i].event === event) {
                var win = arr[i];
                this.clearEventCanvas(win, !this.poolEventWindows);
                return true;
            }
        }
        return false;
    },
    
    // DaySchedule updateEventWindow
    updateEventWindow : function (event) {
        if (!this.body || !this.body.children) return;
        var arr = this.body.children, cal = this.getCalendar();
        if (cal.dataSource) cal._pks = cal.getDataSource().getLocalPrimaryKeyFields();
        for (var i = 0; i < arr.length ; i++) {
            if (arr[i] && arr[i].isEventCanvas && this.areSame(arr[i].event, event)) {
                // reassign event for databound update, because databound update creates
                // a new object
                arr[i].event = event;
                this.sizeEventWindow(arr[i]);
                //arr[i].renderEvent(arr[i].getTop(), arr[i].getLeft(), arr[i].getVisibleWidth(), arr[i].getVisibleHeight());
                //arr[i].sizeToEvent();
                arr[i].setDescriptionText(event[cal.descriptionField]);
                //arr[i].updateTitle();
                return true;
            }
        }
        return false;
    },

    areSame : function (first, second) {
        var cal = this.getCalendar();
        if (cal.dataSource) {
            var pks = cal._pks, areEqual = true;
            for (var pkName in pks) {
                if (first[pkName]!= second[pkName]) {
                    areEqual = false;
                    break;
                }
            }
            return areEqual;
        } else {
            return (first === second);    
        }
    }
   
});

// WeekSchedule
// --------------------------------------------------------------------------------------------
isc.ClassFactory.defineClass("WeekSchedule", "DaySchedule");


// MonthSchedule
// --------------------------------------------------------------------------------------------
isc.ClassFactory.defineClass("MonthSchedule", "ListGrid");

// Create a separate subclass for month schedule body

isc.ClassFactory.defineClass("MonthScheduleBody", "GridBody");

isc.MonthSchedule.changeDefaults("headerButtonProperties", {
    showRollOver: false, 
    showDown: false, 
    cursor: "default"  
});

isc.MonthSchedule.changeDefaults("bodyProperties", {
    redrawOnResize:true
});

isc.MonthSchedule.addProperties({
    autoDraw: false,
    leaveScrollbarGap: false,

    showAllRecords: true,
    fixedRecordHeights: true,

    // show header but disable all header interactivity
    showHeader: true,
    showHeaderContextMenu: false,
    canSort: false,
    canResizeFields: false,
    canReorderFields: false,

    // disable header resizing by doubleclick
    canAutoFitFields:false,

    canHover: true,
    showHover: true,
    hoverWrap: false,
    // show cell-level rollover
    showRollOver:true,
    useCellRollOvers:true,

    // set up cell-level drag selection
    //canDrag:true,
    // dragAppearance:"none",
    //canDragSelect:true,
    canSelectCells:true,
    
    //firstDayOfWeek: 0,
    dayHeaderHeight: 20,
    // set alternateRecordStyle to false: for many skins, not having this set to
    // false leads to undefined styles being generated like 'calMonthOtherDayBodyDisabledDark'.
    // See GridRenderer.getCellStyleIndex() where it checks for this.alternateRowStyles.
    // We manually set row styles for the month view, so it should be safe to disable
    // alternate row styles.
    alternateRecordStyles: false,
    
    initWidget : function () {
        this.addProperties(this.creator.viewProps);

        var cal = this.getCalendar();
        // create month UI scaffolding
        if (cal.data) this.data = this.getDayArray();
        this.fields = [
            {name: "day1", align: "center"},
            {name: "day2", align: "center"},
            {name: "day3", align: "center"},
            {name: "day4", align: "center"},
            {name: "day5", align: "center"},
            {name: "day6", align: "center"},
            {name: "day7", align: "center"}
        ];

        // set day titles
        this.firstDayOfWeek = cal.firstDayOfWeek;
        var sdNames = Date.getShortDayNames();
        var weekendDays = Date.getWeekendDays();
        for (var i = 0; i < 7; i++) {
            var dayNum = (i + this.firstDayOfWeek) % 7;
            this.fields[i].title = sdNames[dayNum];
            this.fields[i]._dayNum = dayNum;
            // store day index to easily get to the right day properties stored on the month
            // records from methods like formatCellValue
            this.fields[i]._dayIndex = i + 1;
            // hide weekends
            if (!cal.showWeekends && weekendDays.contains(dayNum)) {
                this.fields[i].showIf = "return false;";
            }

        }

        this.minimumDayHeight = cal.minimumDayHeight;
        
        this.Super("initWidget");
    },

    getCalendar : function () {
        return this.creator;
    },
    
    getCellCSSText : function (record, rowNum, colNum) {
        var result = this.creator._getCellCSSText(this, record, rowNum, colNum);

        if (result) return result;
        return this.Super("getCellCSSText", arguments);
    },

    getDayArray : function () {
        var dayArr = [], eventArr, endDate,
            displayDate = new Date(this.creator.year, this.creator.month, 1),
            cal = this.getCalendar()
        ;
        
        // go back to the first day of the week
        while (displayDate.getDay() != cal.firstDayOfWeek) {
            this.incrementDate(displayDate, -1);
        }
        
        // special case when hiding weekends, can have the first row be entirely from the previous
        // month. In this case, hide the first row by adding 7 days back to the displayDate
         if (!cal.showWeekends) {
            var wEnds = Date.getWeekendDays();
            var checkDate = displayDate.duplicate();
            var hideFirstRow = true;
            for (var i = 0; i <= 7 - wEnds.length; i++) {
                if (checkDate.getMonth() == cal.month) {
                    hideFirstRow = false;
                    break;
                }
                this.incrementDate(checkDate,1);
            }
            if (hideFirstRow) this.incrementDate(displayDate, 7);
           
        }
        
        // 40 days from start date seems like a nice round number for getting 
        // all the relevant events in a month, with extra days for adjacent months
        endDate = new Date(cal.year, cal.month, 
            displayDate.getDate() + 40);
        eventArr = cal._getEventsInRange(displayDate, endDate, this);
        // sort events by date
        eventArr.sortByProperty("name", true, 
            function (item, propertyName, context) {
                return item[context.startDateField].getTime();
            }, cal
        );
        this._eventIndex = 0;
        for (var i=0; i<6; i++) { // the most we need to iterate is 6, sometimes less
            // add rows of data to designate days and day headers. Each row is either a header
            // or a day body.
            if (cal.showDayHeaders) dayArr.add(this.getHeaderRowObject(displayDate));
            dayArr.add(this.getEventRowObject(displayDate, eventArr));
            this.incrementDate(displayDate, 7);
            // if we hit the next month, don't keep adding rows, we're done.
            if (displayDate.getMonth() != cal.month) break;
        }
        return dayArr;
    },
    
    getHeaderRowObject : function (theDate) {
        var obj = {};
        var nDate = theDate.duplicate();
        for (var i=0; i<7; i++) {
            obj["day" + (i + 1)] = nDate.getDate();
            // store the complete date
            obj["date" + (i + 1)] = nDate.duplicate();
            this.incrementDate(nDate, 1);
        }
        return obj;
    },
    
    incrementDate : function (date, offset) {
        var curDate = date.getDate();
        date.setDate(curDate + offset);
        // In some timezones, DST can cause certain date/times to be invalid so if you attempt
        // to set a java date to (say) 00:00 on Oct 16, 2011, with native timezone set to 
        // Brasilia, Brazil, the actual date gets set to 23:00 on Oct 15th, leading to 
        // bad display.
        // Workaround this by tweaking the time to avoid such an issue
        
        if (date.getDate() == (curDate+offset) -1) {
            date.setHours(date.getHours() + 1);
            date.setDate(curDate + offset);
        }
        return date;
    },
    
    getEventRowObject : function (theDate, events) {
        var obj = {};
        var nDate = theDate.duplicate();
        for (var i=0; i<7; i++) {
            var evArr = [];
            while (this._eventIndex < events.length) {
                var evnt = events[this._eventIndex];
                if (evnt[this.creator.startDateField].getMonth() != nDate.getMonth() 
                    || evnt[this.creator.startDateField].getDate() != nDate.getDate()) {
                    break;    
                } else {
                    evArr.add(evnt);
                    this._eventIndex += 1;
                }
                
            }
            // store the day number here too
            obj["day" + (i + 1)] = nDate.getDate();
            // store the complete date
            obj["date" + (i + 1)] = nDate.duplicate();
            // store the events
            obj["event" + (i + 1)] = evArr;
            this.incrementDate(nDate, 1);
        }
        return obj;
    },
    
    // utility method used for retrieving events from a given row and column number.
    // used by calendar.monthViewEventCick
    getEvents : function (rowNum, colNum) {
        var body = this.getFieldBody(colNum);
        if (!body || body == this.frozenBody) return false;
        var col = this.getLocalFieldNum(colNum);
        var day = this.getDayFromCol(col);

        var dayIndex = this.fields.get(col)._dayIndex;
        var events = this.data[rowNum]["event" + dayIndex];
        return events;
    },
    
    getEventCell : function (event) {
        var data = this.data;
        for (var colNum = 0; colNum < this.fields.length; colNum++) {
            var dayIndex = this.fields[colNum]._dayIndex,
                eventTitle = "event" + dayIndex;
            for (var rowNum = 0; rowNum < data.length; rowNum++) {
                var events = data.get(rowNum)[eventTitle];
                if (events != null && events.contains(event)) {
                    return [rowNum,colNum];
                }
            }
        }
    },
    
    getDayFromCol : function (colNum) {
        var dayNum = this.body.fields.get(colNum)._dayNum;
        return dayNum;
        
    },
    
    // helper function for detecting when a weekend is clicked, and weekends are disabled
    colDisabled : function (colNum) {
        var body = this.getFieldBody(colNum);
        if (!body || body == this.frozenBody) return false;
        var col = this.getLocalFieldNum(colNum);
        if (this.creator.disableWeekends 
            && Date.getWeekendDays().contains(this.getDayFromCol(col))) {
            return true;
        } else {
            return false;
        }
    },

    refreshEvents : function () {
        var cal = this.getCalendar();
        // bail if no data yet
        if (!cal.hasData()) return;
        this.logDebug('refreshEvents: month', 'calendar');
        this.setData(this.getDayArray());    
        if (cal.eventsRendered && isc.isA.Function(cal.eventsRendered)) 
            cal.eventsRendered();
   },
    
    rowIsHeader : function (rowNum) {
        var cal = this.getCalendar();
        if (!cal.showDayHeaders || (cal.showDayHeaders && rowNum % 2 == 1)) return false;
        else return true;
    },
    
    formatCellValue : function (value, record, rowNum, colNum) {
        var cal = this.getCalendar(),
            fieldIndex = this.fields.get(colNum)._dayIndex,
            evtArr = record["event" + fieldIndex],
            currDate = record["date" + fieldIndex],
            isOtherDay = currDate.getMonth() != cal.chosenDate.getMonth();
       
        if (this.rowIsHeader(rowNum)) {
            if (!cal.showOtherDays && isOtherDay) {
                return "";  
            } else {
                //isc.logWarn('here:' + [value, currDate.getDate(), rowNum, colNum]);
  
                return cal.getDayHeaderHTML(currDate, evtArr, cal, rowNum, colNum); 
            }
        } else {
            if (!cal.showOtherDays && isOtherDay) {
                return "";  
            } else {
                return cal.getDayBodyHTML(currDate, evtArr, cal, rowNum, colNum); 
            }
        }
    },
    
    cellHeight: 1,
    enforceVClipping: true,
    getRowHeight : function (record, rowNum) {
        var cal = this.getCalendar(),
            dayHeaders = cal.showDayHeaders
        ;
        if (this.rowIsHeader(rowNum)) { // header part
            return this.dayHeaderHeight;
        } else { // event part, should use fixedRecordHeights:false
            var minsPerRow = cal.getMinutesPerRow(this),
                rowsPerHour = cal.getRowsPerHour(this),
                rows = dayHeaders ? this.data.length / rowsPerHour : this.data.length,
                viewHeight = dayHeaders ? this.body.getViewportHeight() 
                    - (this.dayHeaderHeight * rows) : this.body.getViewportHeight(),
                minHeight = dayHeaders ? this.minimumDayHeight - this.dayHeaderHeight : null
            ;
            
            if (viewHeight / rows <= minHeight) { 
                return minHeight;
            } else {   
                // calculate the remainder and add 1 to the current row height if need be.
                // this eliminates a gap at the bottom of the month view
                var remainder = viewHeight % rows,
                    offset = 0,
                    currRow = dayHeaders ? (rowNum - 1) / rowsPerHour : rowNum
                ;
                if (currRow < remainder) offset = 1; 
                return (Math.floor(viewHeight / rows) + offset);   
            } 
        }
    },
    
    getCellAlign : function (record, rowNum, colNum) {
        if (this.rowIsHeader(rowNum)) return "right";
        else return "left";
    },
    
    getCellVAlign : function (record, rowNum, colNum) {
        if (!this.rowIsHeader(rowNum)) return "top";
        else return "center";
        
    },

    cellHoverHTML : function (record, rowNum, colNum) {
        var fieldIndex = this.fields.get(colNum)._dayIndex;
        var currDate   = record["date" + fieldIndex];
        var evtArr     = record["event" + fieldIndex];

        if (!this.rowIsHeader(rowNum) && evtArr != null) {
            var cal = this.getCalendar();
            return cal.getMonthViewHoverHTML(currDate,evtArr);
        }
    },
    
    // monthView
    getBaseStyle : function (record, rowNum, colNum) {
        var cal = this.getCalendar(), fieldIndex = this.fields.get(colNum)._dayIndex;
        var bStyle;
        if (this.rowIsHeader(rowNum)) { // header
            if ((rowNum == 0 && record["day" + fieldIndex] > 7)
                || (rowNum == this.data.length - 2 && record["day" + fieldIndex] < 7)) {
                if (!cal.showOtherDays) return cal.otherDayBlankStyle;
                else bStyle = cal.otherDayHeaderBaseStyle;
            } else bStyle = cal.dayHeaderBaseStyle;
        } else { // body
            var dis = this.colDisabled(colNum), 
                startRow = cal.showDayHeaders ? 1 : 0, endRow = this.data.length - 1;
                
            if ((rowNum == startRow && this.data[startRow]["day" + fieldIndex] > 7)
                || (rowNum == endRow && this.data[endRow]["day" + fieldIndex] < 7)) {
                if (!cal.showOtherDays) return cal.otherDayBlankStyle;
                else bStyle = dis ? cal.otherDayBodyBaseStyle + "Disabled" : cal.otherDayBodyBaseStyle;
            } else bStyle = dis ? cal.dayBodyBaseStyle + "Disabled" : cal.dayBodyBaseStyle;      
        }      
        return bStyle;
    },
    
    // monthView cellClick
    // if a header is clicked, go to that day. Otherwise, open the event dialog for that day.    
    cellClick : function (record, rowNum, colNum) {
        var cal = this.getCalendar(), year, month, fieldIndex = this.fields.get(colNum)._dayIndex,
            currDate = record["date" + fieldIndex],
            evtArr = record["event" + fieldIndex],
            isOtherDay = cal.chosenDate.getMonth() != currDate.getMonth(),
            doDefault = false;
        if (this.rowIsHeader(rowNum)) { // header clicked
            if (!(!this.creator.showOtherDays && isOtherDay)) {
                doDefault = cal.dayHeaderClick(currDate, evtArr, cal, rowNum, colNum);        
            }
            if (doDefault) {
                // previous month day clicked
                if (rowNum == 0 && record["day" + fieldIndex] > 7) {
                    // check for previous year boundaries
                    if (cal.month == 0) {
                        year = cal.year - 1;
                        month = 11;
                    } else {
                        year = cal.year;
                        month = cal.month - 1;
                    }
                } else if (rowNum == this.data.length - 2 && record["day" + fieldIndex] < 7) {
                    // check for next year boundaries
                    if (cal.month == 11) {
                        year = cal.year + 1;
                        month = 0;
                    } else {
                        year = cal.year;
                        month = cal.month + 1;
                    }
                } else {
                    year = cal.year;
                    month = cal.month;
                }

                cal.dateChooser.dateClick(year, month, record["day" + fieldIndex]);
                cal.selectTab(0);
            }
        } else { // day body clicked
            if (!this.colDisabled(colNum) && !(!cal.showOtherDays && isOtherDay)) {
                doDefault = cal.dayBodyClick(currDate, evtArr, cal, rowNum, colNum);
                if (doDefault && cal.canCreateEvents) {
                    var startDate = cal.getCellDate(rowNum, colNum, this),
                        endDate = cal.getCellDate(rowNum, colNum+1, this)
                    ;
                    //this.creator._showEventDialog(null, sRow, this._selectionTracker.colNum, diff);
                    var newEvent = {};
                    newEvent[cal.startDateField] = startDate;
                    newEvent[cal.endDateField] = endDate;
                    cal.showNewEventDialog(newEvent);
                }
            }

        }
    }




});

// EventWindow
//---------------------------------------------------------------------------------------------
//> @class EventWindow
// Subclass of Window used to display events within a +link{Calendar}.  Customize via
// +link{calendar.eventWindow}.
//
// @treeLocation  Client Reference/Calendar
// @visibility external
//<
isc.ClassFactory.defineClass("EventWindow", "Window");

isc.EventWindow.changeDefaults("resizerDefaults", {
    overflow:"hidden", height: 6,
    snapTo: "B", 
    canDragResize:true//, getEventEdge:function () {return "B"} 
});

isc.EventWindow.changeDefaults("headerDefaults", {
    layoutMargin:0, layoutLeftMargin:3, layoutRightMargin:3
});

isc.EventWindow.addProperties({
    autoDraw: false,
    minHeight: 5,
    // for timelineEvents, so they can be resized to be very small
    minWidth: 5, 
    showHover: true, 
    canHover: true,
    hoverWidth: 200,

    canDragResize: true,
    canDragReposition: true,
    resizeFrom: ["B"],
    showShadow: false,
    showEdges: false,
    showHeaderBackground: false,
    useBackMask: false,
    keepInParentRect: true,
    headerProperties: {height:14},
    
    closeButtonProperties: {height: 10, width: 10},
    bodyColor: null,
    
    showHeaderIcon: false,
    showMinimizeButton: false,
    showMaximimumButton: false,

    showFooter: true,
    
    baseStyle: "eventWindow",
    
    dragAppearance: "none",

    _footerProperties: {overflow:"hidden", defaultLayoutAlign:"center", height: 7},

    initWidget : function () {
        this.footerProperties = isc.addProperties({}, this.footerProperties, this._footerProperties);
        
        if (this.bodyConstructor == null) this.bodyConstructor = isc.HTMLFlow;

        if (this.calendar.showEventDescriptions != false) {
            this.bodyProperties = isc.addProperties({}, this.bodyProperties, 
                {contents: this.descriptionText, valign:"top", overflow: "hidden"}
            );
        }
        if (this.calendar.showEventBody == false) {
            this.showBody = false;
            // need to set showTitle to false so that drag reposition works
            this.showTitle = false;
        }

        this.Super("initWidget", arguments);
        
        // ugly hack, required for this original EventWindow when showEventDescriptions is 
        // false - we completely eliminate the header and
        // body of the window, and simply make our own header. We add this to
        // the event window as a child (if added as a member it won't be drawn).
        // The regular header won't be drawn if showBody:false, probably having
        // to do with _redrawWithParent on the window.
        if (this.calendar.showEventDescriptions == false) {
            var lbl = isc.Label.create({
                    autoDraw: false,
                    border: "0px",
                    padding: 3,
                    height: "100%",
                    width: "100%",
                    backgroundColor: this.event.backgroundColor,
                    textColor: this.event.textColor,
                    setContents : function (contents) {
                        this._origContents = contents;
                        this.Super("setContents", arguments);
                    },
                    canHover: true,
                    showHover: true,
                    eventWin: this,
                    getHoverHTML : function () {
                        return this.eventWin.getHoverHTML();
                    }
            });
            this.addChild(lbl);
            this.header = lbl;
            this._customHeaderLabel = lbl;
            this._customHeader = true;
            //eventWin.updateColors();
        }
        
        
        this.setStyleName(this.baseStyle);
    },
    
    setStyleName : function (styleName) {
        this.bodyStyle = styleName + "Body";
        this.headerStyle = styleName + "Header";
        this.Super("setStyleName", arguments);
        if (this.header) this.header.setStyleName(this.headerStyle);
        if (this.headerLabel) {
            this.headerLabel.setStyleName(this.headerStyle);
        } else {
            this.headerLabelProperties = isc.addProperties({}, this.headerLabelProperties, 
                    { styleName: this.headerStyle });
        }
        if (this.body) this.body.setStyleName(this.bodyStyle);
        if (this._customHeaderLabel) this._customHeaderLabel.setStyleName(this.bodyStyle);
    },

    mouseUp : function () {
        return isc.EH.STOP_BUBBLING;
    },
    
    makeFooter : function () {
        // if not showing a footer, bail
        if (!this.showFooter || this.canDragResize == false) return;
        
        this.resizer = this.createAutoChild("resizer", {
            dragTarget:this.dragTarget,
            styleName: this.baseStyle + "Resizer"
        });
        this.addChild(this.resizer);
        
        // needs to be above the statusBar
        if (this.resizer) this.resizer.bringToFront();
    },
    
    setDescriptionText : function (descriptionText) {
        if (this.calendar.getDescriptionText) {
            descriptionText = this.calendar.getDescriptionText(this.event);
        } 
        if (this.body) {
            this.descriptionText = descriptionText;
            this.body.setContents(descriptionText);
        } else {
            this.descriptionText = descriptionText;
            if (this._eventLabel) {
                
                this._eventLabel.moveTo(0, 0);
                this._eventLabel.setWidth("100%");
                this._eventLabel.setContents(descriptionText);    
            } else if (this.calendar.showEventDescriptions == false) {
                this._customHeaderLabel.setContents(descriptionText);
                this._customHeaderLabel.redraw();
            }
        }
    },
    
    click : function () {
        if (this._closed) return;
        if (this._hitCloseButton) {
            // one-time flag set when the close button is clicked but eventRemoveClick() has
            // been implemented and cancels the removal.
            this._hitCloseButton = null;
            return;
        }
        var cal = this.calendar;
        var doDefault = cal.eventClick(this.event, this._isWeek ? "week" : "day");
        if (doDefault != false) {
            if (!cal.canEditEvent(this.event)) return;
            // handle the case when a selection is made, then an event is clicked
            if (this._isWeek) cal.weekView.clearSelection();
            else if (cal.dayView) cal.dayView.clearSelection();
            var offset = (this._isWeek && cal.weekView.isLabelCol(0) ? 1 : 0);
            var col = this._isWeek ? this.event[cal.startDateField].getDay() - 
                cal.firstDayOfWeek + offset : offset;
            // account for no weekends shown
            if (this._isWeek && cal.showWeekends == false) col--;
            var row =  this.event[cal.startDateField].getHours() * cal.getRowsPerHour();
            cal.showEventDialog(this.event);
        }
    },
   
    mouseDown : function () {
        if (this.dragTarget) this.dragTarget.eventWin = this;
        this.calendar.eventDialog.hide();
        return isc.EH.STOP_BUBBLING;
    },
    
    updateTitle : function () {
        var cal = this.calendar, event = this.event,
            sTime = isc.Time.toTime(event[cal.startDateField], this.calendar.timeFormatter, true),
            eTitle = sTime + " " + event[cal.nameField],
            style = ""
        ;

        if (event.headerTextColor) style += "color:" + event.headerTextColor + ";";
        if (event.headerBackgroundColor) {
            style += "background-color:" + event.headerBackgroundColor + ";";
            var headerLevelParent = this.header.getMember(0);
            if (headerLevelParent) {
                headerLevelParent.setBackgroundColor(event.headerBackgroundColor);
            }
        }
        if (style != "") eTitle = "<span style='" + style + "'>" + eTitle + "</span>";

        this.setTitle(eTitle);
    },
    
    renderEvent : function (eTop, eLeft, eWidth, eHeight) {
        var cal = this.calendar, event = this.event;

        if (isc.isA.Number(eWidth) && isc.isA.Number(eHeight)) {
            this.resizeTo(Math.round(eWidth), Math.round(eHeight));
        }
        if (isc.isA.Number(eTop) && isc.isA.Number(eLeft)) {
            this.moveTo(Math.round(eLeft), Math.round(eTop));
        }

        this.updateColors();

        if (this._customHeader) 
            this.header.resizeTo(Math.round(eWidth), Math.round(eHeight));

        //this.updateTitle();
        this.bringToFront();
        if (!this.isDrawn()) this.draw();
        this.show();
    },
    
    updateColors : function () {
        var cal = this.calendar,
            event = this.event,
            header = this.header,
            labelParent = header ? header.getMember ? header.getMember(0) : header : null,
            label = labelParent,
            eTitle = cal.getEventTitle(event, this._parentView)
        ;

        if (!event) return;

        if (labelParent && labelParent.children && labelParent.children[0]) {
            var members = labelParent.children[0].members;
            if (members && members.length > 0) label = members[0];
        }

        if (event.backgroundColor) {
            this.setBackgroundColor(event.backgroundColor);
            if (this.body) this.body.setBackgroundColor(event.backgroundColor);
        } else {
            this.backgroundColor = null;
            if (this.isDrawn() && this.getStyleHandle()) {
                this.getStyleHandle().backgroundColor = null;
            }
            if (this.body) {
                this.body.backgroundColor = null;
                if (this.body.isDrawn() && this.body.getStyleHandle()) {
                    this.body.getStyleHandle().backgroundColor = null;
                }
            }
            if (label) {
                label.backgroundColor = null;
                if (label.isDrawn() && label.getStyleHandle()) {
                    label.getStyleHandle().backgroundColor = null;
                }
            }
        }
        
        if (event.textColor) {
            this.setTextColor(event.textColor);
            if (this.body) {
                var style = "color:" + event.textColor + ";"
                this.body.setTextColor(event.textColor);
                this.body.setContents("<span style='" + style + "'>" + 
                        event[cal.descriptionField] || "" + "</span>");
            }
        } else {
            if (this.textColor) {
                this.setTextColor(null);
                if (this.isDrawn() && this.getStyleHandle()) {
                    this.getStyleHandle().color = null;
                }
                if (this.body) {
                    this.body.setTextColor(null);
                    this.body.setContents(event[cal.descriptionField]);
                }
                if (label) {
                    label.setTextColor(null);
                    label.setContents(eTitle);
                }
                if (this._customHeaderLabel) {
                    this._customHeaderLabel.setTextColor(null);
                    this._customHeaderLabel.setContents(eTitle);
                }
            }
        }
        
        if (this.header) {
            var backColor, textColor;
            if (cal.showEventDescriptions == false) {
                backColor = event.backgroundColor;
                textColor = event.textColor;
            } else {
                backColor = event.headerBackgroundColor;
                textColor = event.headerTextColor;
            }
            if (backColor) {
                this.header.setBackgroundColor(backColor);
                if (label) label.setBackgroundColor(backColor);
            } else {
                this.header.backgroundColor = null;
                if (this.isDrawn() && this.header.getStyleHandle()) {
                    this.header.getStyleHandle().backgroundColor = null;
                }
                if (label) {
                    label.backgroundColor = null;
                    if (label.getStyleHandle()) {
                        label.getStyleHandle().backgroundColor = null;
                    }
                }
            }
            if (textColor) {
                this.header.setTextColor(textColor);
                var style = "color:" + textColor + ";",
                    val = cal.showEventDescriptions == false ? 
                                    this.header._origContents : eTitle,
                    html = "<span style='" + style + "'>" + val + "</span>"
                ;
                if (!label) {
                    if (this.header.setContents) this.header.setContents(html);
                } else {
                    label.setTextColor(textColor);
                    label.setContents(html);
                }
            } else {
                if (this.header.textColor) {
                    this.header.setTextColor(null);
                    if (this.isDrawn() && this.header.getStyleHandle()) {
                        this.header.getStyleHandle().color = null;
                    }
                    if (label) {
                        label.setTextColor(null);
                        if (label.isDrawn() && label.getStyleHandle()) {
                            label.getStyleHandle().color = null;
                        }
                    }
                }
            }
            this.markForRedraw();
        }
    },

    getPrintHTML : function (printProperties, callback) {
        var output = isc.StringBuffer.create(),
            cal = this.calendar,
            isTimeline = cal.isTimeline(),
            gridBody = this.parentElement,
            grid = gridBody.grid,
            bodyVOffset = 40 + grid.getHeaderHeight(),
            winTop = this.getTop(),
            bodyTop =  gridBody.getPageTop(),
            top = (winTop) + bodyVOffset + 1,
            widths = gridBody._fieldWidths,
            left = grid.getLeft() + gridBody.getLeft() + 
                        (grid.getEventLeft ? grid.getEventLeft(this.event) : 
                            cal.getEventLeft(this.event, grid)),
            width = this.getVisibleWidth(),
            height = this.getVisibleHeight() - 2,
            i = (printProperties && printProperties.i ? printProperties.i : 1)
        ;

        var startCol = cal.getEventStartCol(this.event, this),
            endCol = cal.getEventEndCol(this.event, this)
        ;

        if (isTimeline) {
            left += (14 + ((startCol-1)*2));
            width += endCol-startCol;
        } else {
            left += grid._isWeek ? 6 : 8;
        }

        var baseStyle = isTimeline ? this.baseStyle : this.body.styleName;

        output.append("<div class='", baseStyle, "' ",
            "style='border: 1px solid grey; vertical-align: ",
            (cal.showEventDescriptions ? "top" : "middle"), "; ",
            (isTimeline ? "overflow:hidden; " : ""), 
            "position: absolute; ",
            "left:", left, "; top:", top, "; width: ", width, "; height: ", height, "; ",
            "z-index:", i+2, ";'>"
        );
        if (cal.showEventDescriptions) {
            output.append(this.title, "<br>", this.event[cal.descriptionField]);
        } else {
            output.append(this.title);
        }
        output.append("</div>");

        //var result = this.Super("getPrintHTML", arguments);
        var result = output.toString();
        
        return result;
    },

    getHoverHTML : function () {
        return this.calendar.getEventHoverHTML(this.event, this);
    },
    
    closeClick : function () {
        var cal = this.calendar;
        if (cal.eventRemoveClick(this.event) == false) {
            // one-time flag to avoid general click() handler firing and triggering event
            // editing
            this._hitCloseButton = true; 
            return;
        }
        this.Super("closeClick", arguments);
        this.calendar.removeEvent(this.event, true);
        this._closed = true;         
    },
 
    parentResized : function () {
        this.Super('parentResized', arguments);
        // need to resize the event window here (columns are usually auto-fitting, so the 
        // available space probably changed if the calendar as a whole changed size)
        this._parentView.sizeEventWindow(this);
    },

    // get event length in minutes
    getEventLength : function (startDate, endDate) {
        return this.calendar.getEventLength(
            startDate || this.event[this.calendar.startDateField], 
            endDate || this.event[this.calendar.endDateField]
        );
    },
    
    show : function () {
        this.Super("show", arguments);
    }
    
}); // end eventWindow

// TimelineWindow
isc.ClassFactory.defineClass("TimelineWindow", "EventWindow");

isc.TimelineWindow.addProperties({
        
    showFooter: false,
    // not sure why minimized:true was set, but it was preventing L,R resize handles from 
    // working (as expected), so get rid of it.
    //minimized: true,
    resizeFrom: ["L", "R"],
    
    dragAppearance: "none",
    
    initWidget : function () {
        if (this.calendar.showEventWindowHeader == false) {
            this.showBody = false;
            // need to set showTitle to false so that drag reposition works
            this.showTitle = false;
        }

        this.Super("initWidget", arguments);    
    },
    
    draw : function (a, b, c, d) {
        this.invokeSuper(isc.TimelineWindow, "draw", a, b, c, d);
        if (this.calendar.showEventWindowHeader == false) {
             var lbl = isc.Canvas.create({
                    // border: "1px solid red",
                    autoDraw:false,
                    width: "100%",
                    height: 0,
                    top:0,
                    contents: (this.descriptionText ? this.descriptionText : " "),
                    backgroundColor: this.event.backgroundColor,
                    textColor: this.event.textColor
            });
            if (this.body) this.body.addMember(lbl);
            else this.addMember(lbl);
            lbl.setHeight("100%");
            this._eventLabel = lbl;
        }
    },

    click : function () {
        var cal = this.calendar,
            tl = cal.timelineView,
            doDefault = cal.eventClick(this.event, "timeline")
        ;
        if (doDefault != false) {
            if (!cal.canEditEvent(this.event)) return;
            cal.showEventDialog(this.event);
        } else return isc.EH.STOP_BUBBLING;
    },

    destroyLines : function () {
        if (this._lines) {
            if (this._lines[0]) this._lines[0].destroy();
            if (this._lines[1]) this._lines[1].destroy();
            if (this._lines[2]) this._lines[2].destroy();
            if (this._lines[3]) this._lines[3].destroy();
        }
    },
    
    hideLines : function () {
        if (this._lines) {
            if (this._lines[0]) this._lines[0].hide();
            if (this._lines[1]) this._lines[1].hide();
            if (this._lines[2]) this._lines[2].hide();
            if (this._lines[3]) this._lines[3].hide();
        }
    },
    
    showLines : function () {
        if (this._lines) {
            if (this._lines[0]) this._lines[0].show();
            if (this._lines[1]) this._lines[1].show();
            if (this._lines[2]) this._lines[2].show();
            if (this._lines[3]) this._lines[3].show();
        }
    },
    
    hide : function () {
        this.invokeSuper(isc.TimelineWindow, "hide");
        this.hideLines();
    },
    
    show : function () {
        this.invokeSuper(isc.TimelineWindow, "show");
        // this seems like overkill
        //this.updateColors();
        this.showLines();
    },
    
    parentResized : function () {
        // skip EventWindow implementation of parentResized. We shouldn't need to resize
        // all eventWindows for this view.
        this.invokeSuper(isc.EventWindow, "parentResized");
        //this.Super('parentResized', arguments);
        //this._parentView.sizeEventWindow(this);
        
    }    
        
}); // end TimelineWindow

// TimelineView
//---------------------------------------------------------------------------------------------
isc.ClassFactory.defineClass("TimelineView", "ListGrid");

isc.TimelineView.changeDefaults("bodyProperties", {
    //childrenSnapToGrid: true,
    
    snapToCells: false,
    suppressVSnapOffset: true
});

isc.TimelineView.addProperties({
    canSort: false,
    canResizeFields: false,
    canAutoFitFields: false,
    canReorderFields: false,
    showHeaderContextMenu: false,
    showAllRecords: true,
    alternateRecordStyles: false,
    showRollOver:true,
    useCellRollOvers:true,
    canSelectCells:true,

    laneNameField: "lane",
    columnWidth: 60,
    laneHeight: 60,

    labelColumnWidth: 75,
    labelColumnBaseStyle: "labelColumn",

    eventPageSize: 30,
    trailIconSize: 16,
    leadIconSize: 16,
    scrollToToday: false,//5,
    
    lineImage: "[SKINIMG]Stretchbar/hsplit_over_stretch.gif",
    trailingEndPointImage: "[SKINIMG]actions/prev.png",
    leadingEndPointImage: "[SKINIMG]actions/next.png",

    headerSpanHeight: 24,
    
    
    headerProperties: {
        inherentWidth:false
    },

    eventCanvasConstructor: "TimelineWindow",

    poolEventWindows: true,
    initWidget : function () {
        this.fields = [];
        
        this.addProperties(this.creator.viewProps);
        this.addProperties(this.creator.gridProps);
        
        this.addProperties(this.creator.timelineViewProperties);

        var c = this.getCalendar();

        if (c.alternateLaneStyles) {
            this.alternateRecordStyles = c.alternateLaneStyles;
        }
        
        if (c.canReorderLanes) {
            this.canReorderRecords = c.canReorderLanes;
        }

        this.firstDayOfWeek = this.creator.firstDayOfWeek;

        if (c.laneNameField) this.laneNameField = c.laneNameField;
        if (c.renderEventsOnDemand) this.renderEventsOnDemand = c.renderEventsOnDemand;
        if (c.startDate) this.startDate = c.startDate.duplicate();
        if (c.endDate) this.endDate = c.endDate.duplicate();
        
        // the default widths of laneFields in this timeline
        if (c.labelColumnWidth && c.labelColumnWidth != this.labelColumnWidth) {
            this.labelColumnWidth = c.labelColumnWidth;
        }
        // adds some space around and between events
        if (c.timelineEventPadding != null) this.timelineEventPadding = c.timelineEventPadding;
        if (c.eventDragGap != null) this.eventDragGap = c.eventDragGap;

        if (c.headerLevels) this.headerLevels = isc.shallowClone(c.headerLevels);
        
        this._headerHeight = this.headerHeight;
        this.cellHeight = this.laneHeight;

        var gran = c.timelineGranularity,
            granString = isc.DateUtil.getTimeUnitKey(gran)
        ;

        if (!this.startDate) {
            this.startDate = c.startDate = isc.DateUtil.getAbsoluteDate("-0" + granString, c.chosenDate);
        }

        if (!this.endDate) {
            // no endDate - default to defaultTimelineColumnSpan columns of timelineGranularity
            this.endDate = c.endDate = isc.DateUtil.getAbsoluteDate("+" + 
                    c.defaultTimelineColumnSpan + granString, this.startDate);
        } else if (isc.Date.compareDates(this.startDate, this.endDate) == -1) {
            // startDate is larger than endDate - log a warning and switch the dates
            var s = this.startDate;
            this.startDate = c.startDate = this.endDate;
            this.endDate = c.endDate = s;
            this.logWarn("Timeline startDate is later than endDate - switching the values.");
        }
        
        this.Super("initWidget");

        this.rebuild(true);
        
        this.addAutoChild("eventDragTarget");
        //this.body.addChild(this.eventDragTarget);
    },

    cellMouseDown : function (record, rowNum, colNum) {       
        if (this.isLabelCol(colNum)) {
            return true; 
        }
        
        var offsetX = this.body.getOffsetX(),
            startDate = this.getDateFromPoint(offsetX, null, null, true),
            cal = this.getCalendar()
        ;

        // if backgroundMouseDown is implemented, run it and return if it returns false
        if (cal.backgroundMouseDown && cal.backgroundMouseDown(startDate) == false) return;

        // don't set up selection tracking if canCreateEvents is disabled
        if (!cal.canCreateEvents) return true;
        // first clear any previous selection   
        this.clearSelection();

        var laneName = this.getRecord(rowNum)[cal.laneNameField];

        // don't allow selection if the day is a weekend and weekends are disabled
        if (cal.disableWeekends && !cal.dateIsWorkday(startDate, laneName)) {
            return true;
        }
        
        this._selectionTracker = {};
        this._selectionTracker.rowNum = rowNum;
        this._selectionTracker.startColNum = colNum-1;
        this._selectionTracker.endColNum = colNum-1;
        this._selectionTracker.startDate = startDate;
        this._selectionTracker.startOffsetX = offsetX;
        this._mouseDown = true;
        this.refreshCellStyle(rowNum, colNum-1);
    },

    cellOver : function (record, rowNum, colNum) {
        colNum -=1;
        if (this._mouseDown && this._selectionTracker) {
            var refreshColNum;
            // selecting southbound
            if (this._selectionTracker.startColNum < this._selectionTracker.endColNum) {
                // should select this cell
                if (colNum > this._selectionTracker.endColNum) {
                    refreshColNum = colNum;             
                } else { // should deselect the previous end Col number
                    refreshColNum = this._selectionTracker.endColNum;
                }
                // trigger cell style update from getCellStyle
                this._selectionTracker.endColNum = colNum;
            // selecting northbound
            } else {
                // should select this cell
                if (colNum < this._selectionTracker.endColNum) {
                    refreshColNum = colNum;
                } else { // should deselect the previous end Col number
                    refreshColNum = this._selectionTracker.endColNum;
                }
                this._selectionTracker.endColNum = colNum;
            }
            var refreshGap = 1;
            var row = this._selectionTracker.rowNum;
            //var colNum = this._selectionTracker.colNum;
            for (var i = refreshColNum - refreshGap; i < refreshColNum + refreshGap; i++) {
                this.refreshCellStyle(row, i);        
            }                 
        }
    },

    cellMouseUp : function (record, rowNum, colNum) {
        if (!this._selectionTracker) return true;

        var cal = this.getCalendar();

        var offsetX = this.body.getOffsetX();
        if (offsetX - this._selectionTracker.startOffsetX < cal.eventSnapGap) {
            offsetX = this._selectionTracker.startOffsetX + this.columnWidth;
        }
        this._selectionTracker.endDate = this.getDateFromPoint(offsetX, null, null, true);

        this._mouseDown = false;
        var sCol, eCol, diff;
        // cells selected upwards
        if (this._selectionTracker.startColNum > this._selectionTracker.endColNum) {
            sCol = this._selectionTracker.endColNum;
            eCol = this._selectionTracker.startColNum;
        // cells selected downwards
        } else {
            eCol = this._selectionTracker.endColNum;
            sCol = this._selectionTracker.startColNum;
        }
        diff = eCol - sCol + 1;

        var startDate = this._selectionTracker.startDate || cal.getCellDate(rowNum, sCol, this);
        var endDate = this._selectionTracker.endDate || cal.getCellDate(rowNum, sCol+diff, this);

        // if backgroundClick is implemented, run it and return if it returns false
        if (diff == 1 && cal.backgroundClick) {
            if (cal.backgroundClick(startDate, endDate) == false) {
                this.clearSelection();
                return;
            }
        }

        // if backgroundMouseUp is implemented, run it and bail if it returns false
        if (cal.backgroundMouseUp) {
            if (cal.backgroundMouseUp(startDate, endDate) == false) {
                this.clearSelection();
                return;
            }
        }

        var newEvent = {};
        newEvent[cal.startDateField] = startDate;
        newEvent[cal.endDateField] = endDate;
        newEvent[cal.laneNameField] = this.data.get(rowNum)["name"];
        cal.showNewEventDialog(newEvent);
    },

    clearSelection : function () {
        if (this._selectionTracker) {
            var sCol, eCol, rowNum = this._selectionTracker.rowNum;
            // establish order of cell refresh
            if (this._selectionTracker.startColNum < this._selectionTracker.endColNum) {
                sCol = this._selectionTracker.startColNum;
                eCol = this._selectionTracker.endColNum;
            } else {
                sCol = this._selectionTracker.endColNum;
                eCol = this._selectionTracker.startColNum;
            }
            // remove selection tracker so cells get reset to baseStyle
            this._selectionTracker = null;
            for (var i = sCol; i < eCol + 1; i++) {
                this.refreshCellStyle(rowNum, i);    
            }
        }
    },

    getCellDate : function (rowNum, colNum) {
        if (!this.body) return null;
        var field = this.body.getField(colNum);
        if (!field || !field.date) return null;
        return field.date;
    },

    recordDrop : function (dropRecords, targetRecord, index, sourceWidget) {
        this.Super("recordDrop", arguments);
        this._refreshData();
        this.markForRedraw();
    },
    
    rebuild : function (refreshData) {
        this.clearEvents();
        var fields = this.calcFields();
        
        if (this.isDrawn()) this.setFields(fields);
        else this.fields = fields;
        
        var lanes = this.lanes || this.creator.lanes || [];
        this.setLanes(lanes.duplicate(), true);
        this._scrubDateRange();

        if (refreshData) {
            this._refreshData();
        } else {
            this.refreshEvents();
        }
    },

    _refreshData : function () {
        var cal = this.getCalendar();
        //isc.logWarn("nextOrPrev:" + cal.data.willFetchData(cal.getNewCriteria()));
        if (cal.dataSource && isc.ResultSet && isc.isA.ResultSet(cal.data)) {
            cal.data.invalidateCache();
            cal.fetchData(cal.getNewCriteria(this));
        } else {
            // force dataChanged hooks to fire so event positions are correctly updated
            cal.dataChanged();
        }
    },
    
    setLanes : function (lanes, skipRefreshData) {
        var laneNameField = this.creator.laneNameField;
        this.lanes = lanes;
        lanes.map(function (lane) {
            if (!lane[laneNameField]) lane[laneNameField] = lane.name;
        });
        this.setData(lanes);
        // refetch or just redraw applicable events (setLanes() may have been called after setData)
        if (!skipRefreshData) this._refreshData();
    },
    getLaneIndex : function (laneName) {
        var laneIndex = this.data.findIndex("name", laneName);
        if (laneIndex < 0) 
            laneIndex = this.data.findIndex(this.creator.laneNameField, laneName);
        return laneIndex;
    },
    getLane : function (laneName) {
        var index = this.getLaneIndex(laneName);
        if (index >= 0) return this.data[index];
    },
    
    _scrubDateRange : function () {
        var gran = this.creator.timelineGranularity;
        if (gran == "month") {
            this.startDate.setDate(1);
        } else if (gran == "week") {
            this.startDate = isc.DateUtil.getStartOf(this.startDate, "w", true);
        } else if (gran == "day") {
            this.startDate.setHours(0);
            this.startDate.setMinutes(0);
            this.startDate.setSeconds(0);
            this.startDate.setMilliseconds(0);
        } else if (gran == "hour") {
            this.startDate.setMinutes(0);
            this.startDate.setSeconds(0);
            this.startDate.setMilliseconds(0);
        } else if (gran == "minute") {
            this.startDate.setSeconds(0);
            this.startDate.setMilliseconds(0);
        }
    },
    
    // make sure link between lanes and this.data is maintained
    //setData : function (newData) {
    //     this.creator.lanes = newData;
    //     this.invokeSuper(isc.TimelineView, "setData", newData);
    //},
    scrollTimelineTo : function (pos) {
        this.bodies[1].scrollTo(pos);
    },
        
    setLaneHeight : function (newHeight) {
        this.laneHeight = newHeight;
        this.setCellHeight(newHeight);
        this.refreshEvents();
    },
    
    getRowHeight : function (record, rowNum) {
        return record.height || this.Super("getRowHeight", arguments);
    },
 
    setInnerColumnWidth : function (newWidth) {
        this.columnWidth = newWidth;
        this.setFields(this.calcFields());
        this.refreshEvents();
    },
    
    setTimelineRange : function (start, end, timelineGranularity, timelineUnitsPerColumn, fromSetChosenDate) {
        var cal = this.getCalendar(),
            colSpan = this._dateFieldCount || cal.defaultTimelineColumnSpan;

        this.startDate = start.duplicate();
        cal.startDate = start.duplicate();

        if (end) {
            this.endDate = end.duplicate();
        } else {
            var gran = (timelineGranularity || cal.timelineGranularity).toLowerCase(),
                granString = isc.DateUtil.getTimeUnitKey(gran)
            ;
            this.endDate = isc.DateUtil.getAbsoluteDate("+" + 
                    colSpan + granString, this.startDate);
        }
        cal.endDate = this.endDate.duplicate();
        
        if (timelineGranularity) cal.timelineGranularity = timelineGranularity;
        if (timelineUnitsPerColumn) cal.timelineUnitsPerColumn = timelineUnitsPerColumn;
        
        //isc.logWarn('setTimelineRange:' + [timelineGranularity, timelineUnitsPerColumn, 
        //        cal.timelineGranularity, cal.timelineUnitsPerColumn]);
        cal.dateChooser.setData(this.startDate);
        if (!fromSetChosenDate) cal.setChosenDate(this.startDate, true);
        this.rebuild(true);
    },
    
    addUnits : function (date, units, granularity) {
        granularity = granularity || this.getCalendar().timelineGranularity;
        if (granularity == "century") {
            date.setFullYear(date.getFullYear() + (units * 100));
        } else if (granularity == "decade") {
            date.setFullYear(date.getFullYear() + (units * 10));
        } else if (granularity == "year") {
            date.setFullYear(date.getFullYear() + units);
        } else if (granularity == "quarter") {
            date.setMonth(date.getMonth() + (units * 3));
        } else if (granularity == "month") {
            date.setMonth(date.getMonth() + units);
        } else if (granularity == "week") {
            date.setDate(date.getDate() + (units * 7));
        } else if (granularity == "day") {
            date.setDate(date.getDate() + units);
        } else if (granularity == "hour") {
            date.setHours(date.getHours() + units);    
        } else if (granularity == "minute") {
            date.setMinutes(date.getMinutes() + units);    
        } else if (granularity == "second") {
            date.setSeconds(date.getSeconds() + units);    
        } else if (granularity == "millisecond") {
            date.setMilliseconds(date.getMilliseconds() + units);    
        }
        return date;
    },
    getTimelineEventLeft : function (event) {
        var colNum = this.getTimelineEventColNum(event);
        return this.getColumnLeft(colNum);
    },
    getTimelineEventColNum : function (event) {
        var fields = this.getFields(),
            startDate = event[this.creator.startDateField]
        ;
        
        for (var i=0; i<fields.length; i++) {
            var field = fields[i];
            if (field.date && field.date > startDate) {
                return i-1;
            }
        }
        return null;
    },
    getTimelineEventTop : function (event) {
        var rowNum = this.getTimelineEventRowNum(event);
        return this.getRowTop(rowNum);
    },
    getTimelineEventRowNum : function (event) {
        var row = this.data.find("name", event[this.creator.laneNameField]);
        if (!row) row = this.data.find(this.creator.laneNameField, event[this.creator.laneNameField]);
        return this.getRecordIndex(row);
    },

    calcFields : function () {
        var newFields = [],
            c = this.creator
        ;

        if (this.creator.laneFields) {
            var laneFields = this.creator.laneFields;
            laneFields.setProperty("frozen", true);
            laneFields.setProperty("isLaneField", true);
            for (var i = 0; i < laneFields.length; i++) {
                if (laneFields[i].width == null) laneFields[i].width = this.labelColumnWidth;
                newFields.add(laneFields[i]);
            }
        } else {
            var labelCol = {
                 width: this.labelColumnWidth,
                 name: "title",
                 title: " ",
                 showTitle: false,
                 frozen: true,
                 isLaneField: true
             };
             newFields.add(labelCol);    
        }

        if (!c.headerLevels && !this.headerLevels) {
            c.headerLevels = [ { unit: c.timelineGranularity } ];
        }
        
        if (c.headerLevels) {
            this.headerLevels = isc.shallowClone(c.headerLevels);
        } 
        
        if (this.headerLevels) {
            // we have some header-levels - the innermost level is going to be stripped and its
            // "unit" and "titles" array used for field-headers (unit becomes 
            // calendar.timelineGranularity - they should already be the same)
            this.fieldHeaderLevel = this.headerLevels[this.headerLevels.length-1];
            this.headerLevels.remove(this.fieldHeaderLevel);
            c.timelineGranularity = this.fieldHeaderLevel.unit;
        }

        
        this.adjustTimelineForHeaders();

        // add date columns to fields
        var sDate = this.startDate.duplicate(),
            eDate = this.endDate.duplicate(),
            units = c.timelineUnitsPerColumn,
            spanIndex = 0,
            headerLevel = this.fieldHeaderLevel,
            titles = headerLevel && headerLevel.titles ? headerLevel.titles : []
        ;

        if (headerLevel.headerWidth) this.columnWidth = headerLevel.headerWidth;

        while (sDate.getTime() <= eDate.getTime()) {
            var newField = {},
                title = this.getInnerFieldTitle(headerLevel, spanIndex, sDate)
            ;
            
            newField = isc.addProperties(newField, {
                name: "f" + spanIndex,
                title: title,
                width: headerLevel.headerWidth || this.columnWidth,
                date: sDate.duplicate()
            }, this.getFieldProperties(sDate));
            newFields.add(newField);

            sDate = this.addUnits(sDate, units);
            spanIndex++;
        }

        this.buildHeaderSpans(newFields, this.headerLevels, this.startDate, this.endDate);

        this._dateFieldCount = spanIndex-1;

        return newFields;
    },

    adjustTimelineForHeaders : function () {
        // if we weren't 
        var cal = this.getCalendar(),
            unit = this.fieldHeaderLevel ? this.fieldHeaderLevel.unit : cal.timelineGranularity,
            start = cal.startDate,
            end = cal.endDate
        ;

        // we have at least one header - make sure we start and end the timeline 
        // at the beginning and end of the innerLevel's unit-type (the actual field-headers, 
        // that is)
        var key = isc.DateUtil.getTimeUnitKey(unit);

        cal.startDate = this.startDate = isc.DateUtil.getStartOf(start, key);
        cal.endDate = this.endDate = isc.DateUtil.getEndOf(end, key);
    },

    buildHeaderSpans : function (fields, levels, startDate, endDate) {
        var date = startDate.duplicate(),
            c = this.creator,
            result = [],
            spans = []
        ;

        if (levels && levels.length > 0) {
            spans = this.getHeaderSpans(startDate, endDate, levels, 0, fields);
            this.headerHeight = this._headerHeight + this.headerSpanHeight;
        }

        if (spans && spans.length > 0) this.headerSpans = spans;
    },

    getHeaderSpans : function (startDate, endDate, headerLevels, levelIndex, fields) {
        var date = startDate.duplicate(),
            c = this.creator,
            headerLevel = headerLevels[levelIndex],
            unit = headerLevel.unit,
            lastUnit = levelIndex > 0 ? headerLevels[levelIndex-1].unit : unit,
            unitsPerColumn = c.timelineUnitsPerColumn,
            titles = headerLevel.titles || [],
            result = [],
            spanIndex = 0
        ;

        if (levelIndex > 0) {
            if (isc.DateUtil.compareTimeUnits(unit, lastUnit) > 0) {
                // the unit on this level is larger than on it's parent-level - warn
                isc.logWarn("The order of the specified HeaderLevels is incorrect - '" + unit +
                    "' is of a larger granularity than '" + lastUnit + "'");
            }
        }
        
        var DU = isc.DateUtil;
        
        while (date <= endDate) {
            DU.dateAdd(date, "mn", 1, 1);
            var newDate = this.addUnits(date.duplicate(), unitsPerColumn, unit);

            var span = { unit: unit, startDate: date, endDate: newDate, fields: [] };

            this.setSpanDates(span, date);
            
            newDate = span.endDate;

            var title = this.getHeaderLevelTitle(headerLevel, spanIndex, date, newDate);

            span.title = title;
            
            // this condition should be re-introduced once LG supports multiple-headers where
            // only the inner-most spans require a fields array
            //if (levelIndex == headerLevels.length-1) {
                for (var i=0; i<fields.length; i++) {
                    var field = fields[i];
                    if (field.isLaneField || field.date < span.startDate) continue;
                    if (field.date >= span.endDate) break;
                    span.fields.add(field.name);
                }
            //}

            if (levelIndex < headerLevels.length-1) {
                span.spans = this.getHeaderSpans(span.startDate, span.endDate, headerLevels, levelIndex + 1, fields);
                if (headerLevel.titles && headerLevel.titles.length != span.spans.length) {
                    // fewer titles were supplied than we have spans - log a warning about it
                    // but don't bail because we'll auto-generate titles for any spans that
                    // don't have one in the supplied title-array
                    isc.logWarn("The titles array provided for the " + headerLevel.unit + 
                        " levelHeader has a length mismatch: expected " + span.spans.length + 
                        " but " + headerLevel.titles.length + " are present.  Some titles " +
                        " may be auto-generated according to TimeUnit."
                    );
                }
            }

            result.add(isc.clone(span));
            date = newDate.duplicate();
            spanIndex++;
        }

        return result;
    },

    getHeaderLevelTitle : function (headerLevel, spanIndex, startDate, endDate) {
        var unit = headerLevel.unit,
            title = headerLevel.titles ? headerLevel.titles[spanIndex] : null
        ;
        if (!title) {
            // only generate a default value and call the titleFormatter if there was no 
            // entry for this particular span in headerLevels.titles
            if (unit == "century" || unit == "decade") {
                title = startDate.getFullYear() + " - " + endDate.getFullYear();
            } else if (unit == "year") {
                title = startDate.getFullYear();
            } else if (unit == "quarter") {
                title = startDate.getShortMonthName() + " - " + endDate.getShortMonthName();
            } else if (unit == "month") {
                title = startDate.getShortMonthName();
            } else if (unit == "week") {
                title = this.creator.weekPrefix + " " + endDate.getWeek(this.firstDayOfWeek);
            } else if (unit == "day") {
                title = startDate.getShortDayName();
            } else {
                if (unit == "hour") title = startDate.getHours();
                if (unit == "minute") title = startDate.getMinutes();
                if (unit == "second") title = startDate.getSeconds();
                if (unit == "millisecond") title = startDate.getMilliseconds();
                if (unit == "hour") title = startDate.getHours();
            }
            if (isc.isA.Function(headerLevel.titleFormatter)) {
                title = headerLevel.titleFormatter(headerLevel, startDate, endDate, title, this.creator);
            }
        }
        return title;

    },

    setSpanDates : function (span, date) {
        var key = isc.DateUtil.getTimeUnitKey(span.unit);

        span.startDate = isc.DateUtil.getStartOf(date, key, null, this.firstDayOfWeek);
        span.endDate = isc.DateUtil.getEndOf(span.startDate, key, null, this.firstDayOfWeek);
    },

    getFieldProperties : function (date) {
        return null;
    },
    getInnerFieldTitle : function (headerLevel, spanIndex, startDate, endDate) {
        var granularity = headerLevel.unit,
            result = headerLevel.titles ? headerLevel.titles[spanIndex] : null
        ;
        if (!result) {
            // only generate a default value and call the titleFormatter if there was no 
            // entry for this particular span in headerLevels.titles
            if (granularity == "year") {
                result = startDate.getFullYear();
            } else if (granularity == "month") {
                result = startDate.getShortMonthName();
            } else if (granularity == "week") {
                result = this.creator.weekPrefix + startDate.getWeek(this.firstDayOfWeek);
            } else if (granularity == "day") {
                result = (startDate.getMonth() + 1) + "/" + startDate.getDate();
            } else {
                var mins = startDate.getMinutes().toString();
                if (mins.length == 1) mins = "0" + mins;
                result = startDate.getHours() + ":" + mins;    
            }
            if (isc.isA.Function(headerLevel.titleFormatter)) {
                result = headerLevel.titleFormatter(headerLevel, startDate, endDate, result, this.creator);
            }
        }

        return result;
    },

    draw : function (a, b, c, d) {
        this.invokeSuper(isc.TimelineView, "draw", a, b, c, d);
        var snapGap = this.creator.eventSnapGap;
        if (snapGap) {
            this.body.snapHGap = Math.round((snapGap / 60) * this.columnWidth);
            //this.body.snapHGap = 5;
        } else {
            this.body.snapHGap = this.columnWidth;    
        }
        
        this.body.snapVGap = this.laneHeight;
        // scroll to today if defined
        if (this.scrollToToday != false) {
            var today = new Date();
            today.setDate(today.getDate() - this.scrollToToday);
            var diff = this.creator.getDayDiff(this.startDate, today);            
            var sLeft = diff * this.columnWidth;
            this.bodies[1].scrollTo(sLeft, 0);
        }
        this.logDebug('draw', 'calendar');
        // call refreshEvents() whenever we're drawn
        // see comment above dataChanged for the logic behind this
        
        this.body.addChild(this.eventDragTarget);
        this.eventDragTarget.setView(this);

        this.refreshEvents();

    },

    getCellCSSText : function (record, rowNum, colNum) {
        var result = this.creator._getCellCSSText(this, record, rowNum, colNum);

        if (result) return result;
        return this.Super("getCellCSSText", arguments);
    },

    formatDateForDisplay : function (date) {
        return  date.getShortMonthName() + " " + date.getDate() + ", " + date.getFullYear();
    },

    getLabelColCount : function () {
        if (this.creator.laneFields) {
            return this.creator.laneFields.length;
        } else {
            return 1;
        }
    },

    isLabelCol : function (colNum) {
        return this.getField(colNum).frozen == true;
        //if (colNum < this.getLabelColCount()) return true;
        //else return false;
    },

    getCellStyle : function (record, rowNum, colNum) {
        var bStyle = this.getBaseStyle(record, rowNum, colNum);

        if (this.isLabelCol(colNum)) return bStyle;
        
        if (this.alternateRecordStyles && rowNum % 2 != 0) return bStyle + "Dark";
        
        //if (this.alternateFieldStyles && colNum % 2 != 0) return bStyle + "Dark";

        return bStyle;
    },
    
    // timelineView - doesn't work properly - not clear that getDateStyle() is applicable
    /*
    getBaseStyle : function () {
        var result;
        if (this.creator.getDateStyle) result = this.creator.getDateStyle();
        if (!result) result = this.Super("getBaseStyle", arguments);
        return result;
    },
    */


    getBaseStyle : function (record, rowNum, colNum) {
        if (this.isLabelCol(colNum)) return this.labelColumnBaseStyle;        
        else {
            return this.baseStyle;    
        }
    },

    slideRange : function (slideRight) {
        var c = this.creator,
            gran = c.timelineGranularity.toLowerCase(),
            granString = isc.DateUtil.getTimeUnitKey(gran),
            units = c.timelineUnitsPerColumn,
            startDate = this.startDate.duplicate(),
            endDate = this.endDate.duplicate(),
            multiplier = slideRight ? 1 : -1,
            scrollCount = c.columnsPerPage || (this.getFields().length - this.getLabelColCount())
        ;

        
        startDate = isc.DateUtil.dateAdd(startDate, granString, scrollCount, multiplier, false);
        startDate = isc.DateUtil.getStartOf(startDate, granString, false);
        endDate = isc.DateUtil.dateAdd(endDate, granString, scrollCount, multiplier, false);
        endDate = isc.DateUtil.getEndOf(endDate, granString, false);

        this.setTimelineRange(startDate, endDate, gran, units, false);
    },

    nextOrPrev : function (next) {
        this.slideRange(next);
    },
    
    refreshEvents : function () {
        var cal = this.getCalendar();
        // bail if the grid hasn't been drawn yet, or hasn't gotten data yet
        if (!this.body || !cal.hasData()) return;

        var startDate = this.startDate, 
            startMillis = startDate.getTime(),
            endDate = this.endDate, 
            endMillis = endDate.getTime()
        ;

        this.removeOverlapRanges(this.overlapRanges);

        //var events = cal._getEventsInRange(startDate, endDate);
        var allEvents = cal.data.getRange(0, cal.data.getLength());
        var events = [];

        allEvents.map(function (event) {
            if (!isc.isA.String(event)) {
                var sDate = event[cal.startDateField].getTime(),
                    eDate = event[cal.endDateField].getTime()
                ;
                if ((sDate >= startMillis && sDate < endMillis) ||
                    (eDate > startMillis && eDate <= endMillis))
                {
                    events.add(event);
                }
            }
        });
        
        this.logDebug('refreshing ' + events.length + ' events','calendar');
        
        var laneNames = isc.getKeys(cal.getLaneMap());

        var _this = this;
        
        //laneNames.map(function (laneName) {
        //    _this.retagLaneEvents(laneName);
        //});
        
        this.tagDataForOverlap(events);

        this.refreshVisibleEvents();
    },

    compareDates : function (date1, date2, d) {
        // year
        if (date1.getFullYear() < date2.getFullYear()) {
            return 1;       
        } else if (date1.getFullYear() > date2.getFullYear()) {
            return -1;
        }
        // month
        if (date1.getMonth() < date2.getMonth()) {
            return 1;       
        } else if (date1.getMonth() > date2.getMonth()) {
            return -1;
        }
        // day
        if (date1.getDate() < date2.getDate()) {
            return 1;       
        } else if (date1.getDate() > date2.getDate()) {
            return -1;
        }
        // equal
        return 0;
        
    },
    
    getDateFromPoint : function (x, y, round, useSnapGap) {
        var retDate = this.startDate.duplicate();
        var colWidth = this.columnWidth;
        var minsInACol = this._getMinsInACell();
        var minsToAdd = 0;

        if (useSnapGap) {
            // when click/drag creating, we want to snap to the eventSnapGap
            x -= x % this.creator.eventSnapGap;
        }

        // convert x to minutes via how many column lengths are in the point
        minsToAdd += Math.floor(x / colWidth) * minsInACol;
        // account for the remainder, only if not rounding (see getVisibleDateRange)
        if (!round) minsToAdd += ((x % colWidth) / colWidth) * minsInACol;
        
        retDate.setMinutes(retDate.getMinutes() + minsToAdd);
        
        return retDate;
    },
    
    _getMinsInACell : function () {
        var colUnits = this.creator.timelineUnitsPerColumn;
        var granularity = this.creator.timelineGranularity;
        var minsInADay = 24*60;
        var minsInACol;
        var breadth = 0;
        if (granularity == "month") {
            minsInACol = colUnits * (minsInADay * 30);
        } else if (granularity == "week") {
            minsInACol = colUnits * (minsInADay * 7);
        } else if (granularity == "day") {
            minsInACol = colUnits * minsInADay;   
        } else if (granularity == "hour") {
            minsInACol = colUnits * 60;    
        } else if (granularity == "minute") {
            minsInACol = colUnits;    
        } 
        return minsInACol;
    },
    
    // gets the width that the event should be sized to in pixels
    _getEventBreadth : function (eventWin) {
        var minsInACol = this._getMinsInACell(),
            event = eventWin.event,
            cal = this.getCalendar(),
            start, end
        ;
        if (Array.isLoading(event)) return null;        

        // account for events that overlap the end range of the timeline. Event breadth will
        // be truncated to end where the timeline ends.
        if (event[cal.startDateField].getTime() < this.startDate.getTime()) {
            start = this.startDate.duplicate();
        } else {
            start = event[cal.startDateField];    
        }
        if (event[cal.endDateField].getTime() > this.endDate.getTime()) {
            end = this.endDate.duplicate();
            // timeline actually renders one column past the specified end date, so take that
            // into account
            end.setMinutes(end.getMinutes() + minsInACol);
        } else {
            end = event[cal.endDateField].duplicate();
        }
        var minsLen = eventWin.getEventLength(start, end);
        var colWidth = this.columnWidth;
        var breadth = 0;
        var grossCols = Math.floor(minsLen / minsInACol);
        //isc.logWarn('getEventBreadth:' + [minsLen, minsLen / 60, minsInACol, minsInACol/60, grossCols, eventWin.event.start, eventWin.event.end]);
        // when sizing events to grid, always augment the column count by 1.
        if (cal.sizeEventsToGrid) {
            var col = this.getTimelineEventColNum(event);
            
            if (col) {
                var field = this.getField(col),
                    isStartOfCol = field.date ? 
                        field.date.getTime() == event[cal.startDateField].getTime() : false,
                    r = minsLen % minsInACol
                ;
                if (r != 0 || !isStartOfCol) grossCols += 1;
            }
        }

        // first get how many full columns the event spans
        var eStartCol = Math.max(0, this.getEventStartCol(event) + 1);
        if (eStartCol + grossCols + 1 > this.fields.length) grossCols--;
        breadth += Math.max(0, grossCols) * colWidth;
        if (cal.sizeEventsToGrid == false) {
            // then add the remainder
            var add = Math.floor(((minsLen % minsInACol) / minsInACol) * colWidth);
            breadth += add;
        }

        return breadth;
    },

    // getEventStartCol timelineView
    getEventStartCol : function (event) {
        // minDiff = difference between range start and event start in minutes
        var minDiff = (event[this.creator.startDateField].getTime() - this.startDate.getTime())
                    / (1000 * 60);
        // Don't work with fractional min-diff as this could potentially introduce
        // precision errors
        minDiff = Math.round(minDiff);
        var minsInACol = this._getMinsInACell();

        return Math.floor(minDiff / minsInACol);
    },

     // getEventLeft timelineView
    getDateLeftOffset : function (date) {
        // minDiff = difference between range start and event start in minutes
        var minDiff = (date.getTime() - this.startDate.getTime()) 
                    / (1000 * 60);
        // Don't work with fractional min-diff as this could potentially introduce
        // precision errors
        minDiff = Math.round(minDiff);
        var colWidth = this.columnWidth;
        var minsInACol = this._getMinsInACell();
        var eLeft = 0;
        
        // first get how many columns from range start the event is
        eLeft += Math.floor(minDiff / minsInACol) * colWidth;     
        if (this.creator.sizeEventsToGrid == false) {
            // then add the remainder: percentage of leftover mins to minsInACol * colWidth
            eLeft += Math.round(((minDiff % minsInACol) / minsInACol) * colWidth);      
        }
        // don't let event left be < 0. Breadth will compensate for the overflow as well in 
        // getEventBreadth
        if (eLeft < 0) eLeft = 0;

        return eLeft;
        
    },
    
    // getEventLeft timelineView
    getEventLeft : function (event) {
        return this.getDateLeftOffset(event[this.creator.startDateField]);
    },

    getLaneHeight : function (lane) {
        if (isc.isA.Number(lane)) lane = this.getRecord(lane);
        return lane && lane.height || this.cellHeight;
    },

    
    addLeadingAndTrailingLines : function (eventWin) {
        // destroy previous lines and icons before creating new ones
        //eventWin.destroyLines();
        var leadLine, leadIcon, trailLine, trailIcon;
        if (eventWin._lines) {
            leadLine = eventWin._lines[0];
            leadIcon = eventWin._lines[1];
            trailLine = eventWin._lines[2];
            trailIcon = eventWin._lines[3];
        } else {
            leadLine = this._makeLine();
            leadIcon = this._makeIcon(eventWin, "lead");
            trailLine = this._makeLine();
            trailIcon = this._makeIcon(eventWin, "trail");     
        }
       
        
        var showLead = this._positionIcon(leadIcon, leadLine);
        var showTrail = this._positionIcon(trailIcon, trailLine);
        
       
        if (!eventWin._lines) {
            this.body.addChild(leadLine);
            this.body.addChild(leadIcon);
            
            this.body.addChild(trailLine);
            this.body.addChild(trailIcon);
            eventWin._lines = [
               leadLine, leadIcon, trailLine, trailIcon 
            ];
        }
        
       
    },
    
    _positionIcon : function (icon, line) {
        var cal = this.getCalendar(), eventWin = icon._eventWin, event = eventWin.event, 
            type = icon.type, eWidth = this.columnWidth, 
            eHeight = eventWin.getVisibleHeight(), eTop = eventWin.getTop(), 
            eLeft = eventWin.getLeft();
            
        // size/reposition line first
        var dayDiff, lineWidth, drawIcon = true;    
        if (type == "trail") {
            // if trailing date is past our date range, draw the line up to the end of the grid
            // and don't draw the trailing icon
            if (this.compareDates(event[cal.trailingDateField],this.endDate) < 0) {
                dayDiff = cal.getDayDiff(this.endDate, event[cal.startDateField]);
                // don't allow invalid lead day. Set to 1 if invalid.
                if (dayDiff < 1) dayDiff = 1;
                lineWidth = dayDiff * eWidth;
                drawIcon = false;
                //icon.hide();
            } else {
                dayDiff = cal.getDayDiff(event[cal.trailingDateField], event[cal.startDateField]);
                lineWidth = (dayDiff * eWidth) - (Math.round(eWidth / 2));
                //if (icon.isDrawn()) icon.show();
            }                
        } else {
            // if leading date is past our date range, draw the line up to the end of the grid
            // and don't draw the leading icon
            if (this.compareDates(this.startDate, event[cal.leadingDateField]) < 0) {
                dayDiff = cal.getDayDiff(this.startDate, event[cal.startDateField]);
                // don't allow invalid lead day. Set to 1 if invalid.
                if (dayDiff < 1) dayDiff = 1;
                lineWidth = dayDiff * eWidth;
                drawIcon = false;
                //icon.hide();
            } else {
                dayDiff = cal.getDayDiff(event[cal.leadingDateField], event[cal.startDateField]);    
                lineWidth = ( dayDiff * eWidth) - (Math.round(eWidth / 2));
                //if (icon.isDrawn()) icon.show();
            }
        }
      
        //isc.logWarn(event[cal.trailingDateField].toShortDate());
        var lLeft = (type == "trail" ? eLeft + eWidth : eLeft - lineWidth);
        line.moveTo(lLeft, eTop + (Math.round(eHeight / cal.getRowsPerHour(this))));
        line.setWidth(lineWidth);
        
        // position icon
        // calculate a vertical offset to add to the event arrows so that if they are overlapping,
        // drag moving will keep them in the same vertical axis. Just try commenting out the code
        // below and setting vOffset to 0, and drag moving arrows to see the issue.
        var  vOffset = 0;
        if (event._overlapProps && event._overlapProps.slotNum > 0)  {
            vOffset = (event._overlapProps.slotNum - 1) * eHeight;
        } 
        var iconSize = (type == "trail" ? this.trailIconSize : this.leadIconSize);
        var iLeft;
        if (drawIcon == false) iLeft = -50;
        else if (type == "trail") iLeft = eLeft + eWidth + lineWidth - Math.round(iconSize / 2);
        else iLeft = eLeft - lineWidth - Math.round(iconSize / 2);
        icon.moveTo(iLeft, eTop + Math.round(eHeight / 2) - Math.round(iconSize / 2));
        icon._vSnapOrigin = Math.round(eHeight / 2) - Math.round(iconSize / 2) + vOffset;
        icon._hSnapOrigin = Math.round(eWidth / 2) - Math.round(iconSize / 2);
        icon._eventStartCol = cal.getDayDiff(event[cal.startDateField], this.startDate);
        
        return drawIcon;
    },
    
    _makeIcon : function (eventWin, type) {
        var iconSize = (type == "trail" ? this.trailIconSize : this.leadIconSize);
        var icon = isc.Img.create({
            _eventWin: eventWin,
            type: type,
           
            //prompt:eventWin.event.EVENT_ID,
            autoDraw:false,
            _redrawWithParent: false,
            src: (type == "trail" ? this.trailingEndPointImage : this.leadingEndPointImage),
            width: iconSize,
            height: iconSize,
            canDragReposition: (this.creator.canEditEvents == true),
            dragRepositionStart : function () {
                this._dragProps._startRow = this.parentElement.getEventRow();
                this._dragProps._startCol = this.parentElement.getEventColumn();
                //isc.logWarn('icon drag start:');
                this.parentElement.VSnapOrigin = this._vSnapOrigin;
                this.parentElement.HSnapOrigin = this._hSnapOrigin;
            },
            dragRepositionStop : function () {
               var eventStartCol = this._eventStartCol, startCol = this._dragProps._startCol, 
                    endCol = this.parentElement.getEventColumn(), delta = endCol - startCol,   
                    event = this._eventWin.event, cal = this._eventWin.calendar,
                    eventDelta = this.type == "trail" ? endCol - eventStartCol : eventStartCol - endCol;
               //isc.logWarn('icon drag stop:' + eventDelta);
               if (eventDelta < 1) return false;
               var otherFields = {};
               var dateField = this.type == "trail" ? cal.trailingDateField : cal.leadingDateField;
               var newDate = event[dateField].duplicate();
               newDate.setDate(newDate.getDate() + delta);
               otherFields[dateField] = newDate;
               cal.updateEvent(event, event[cal.startDateField], event[cal.endDateField], 
                   event[cal.nameField], event[cal.descriptionField], otherFields, true);
               return true;
              
            }       
        });
        return icon;
    },
    
    _makeLine : function () {
        //var line = isc.Img.create({
        var line = isc.Canvas.create({
            autoDraw:false,
            _redrawWithParent: false,
            //src: this.lineImage,
            height: 2,

            overflow: "hidden",
            styleName: "eventLine"
        });
        
        return line;
    },

    // addEvent timelineView
    addEvent : function (event, eventIndex) {
        if (!this._eventCanvasPool) this._eventCanvasPool = [];

        var reclaimed = false,
            cal = this.getCalendar(),
            canEditEvent = cal.canEditEvent(event),
            canDrag = (cal.canDragEvents == true && event[cal.canDragEventField] != false),
            canvasPool = this._eventCanvasPool,
            canvas
        ;

        // see if there's a *current* eventCanvas that already shows this event - will
        // save time on updating titles and styles, if those things haven't changed
        //canvas = this.getCurrentEventCanvas(event);

        // there's a current event window, or we're recycling them and we have one available...
        if (canvas || (this.renderEventsOnDemand && canvasPool[eventIndex])) {
            if (canvas) {
                var existingWinIndex = canvasPool.indexOf(canvas);
                if (existingWinIndex != eventIndex) {
                    var moveThisWin = canvasPool[eventIndex];
                    canvasPool[eventIndex] = canvas;
                    canvasPool[existingWinIndex] = moveThisWin;
                }
            } else {
                // ...reclaim the event from the event bin
                canvas = this._eventCanvasPool[eventIndex];
                cal.setEventCanvasID(this, event, canvas.ID);
                canvas.event = event;
                canvas.VSnapOrigin = 0;
                reclaimed = true;
                canvas.setStyleName(event[cal.eventWindowStyleField] || cal.eventWindowStyle);
            }
        } else {
            // otherwise make a new window and put it in the bin for future reclamation
            canvas = this.getNewEventWindow(event);
            canvas._parentView = this;
            this._eventCanvasPool.add(canvas);
        }

        canvas._availableForUse = false;

        // Adding a check on parentElement here to ensure that we can't end up with a window
        // in the event bin that is not a child of this.body.  This is related to the change 
        // made a few lines further down, to ensure that an undrawn window is drawn.  I suspect
        // the root of this is event windows being created before the Timeline itself has 
        // been drawn
        if (this.body && (!reclaimed || canvas.parentElement != this.body)) {         
            this.body.addChild(canvas);  
        }

        //TODO: add a helper to set these various drag properties
        canvas.canDragResize = cal.canResizeTimelineEvents && canEditEvent;
        canvas.setCanDragReposition(cal.canDragEvents, this.eventDragTarget);
        //canvas.canDragReposition = cal.canDragEvents;
        
        this.sizeEventWindow(canvas, reclaimed);
        
        //if (!canvas.isDrawn()) canvas.draw();
        //if (canvas.body) canvas.body.show();
        canvas.show();
    },

    removeEvent : function (event) {
        var arr = this.body.children;
        for (var i = 0; i < arr.length ; i++) {
            if (isc.isAn.EventWindow(arr[i]) && arr[i].event === event) {
                var win = arr[i];
                win.parentElement.removeChild(win);
                win.destroy();
                return true;
            }
        }
        return false;
    },
    
    getNewEventWindow : function (event) {
        var cal = this.getCalendar(),
            styleName = event[cal.eventWindowStyleField] || cal.eventWindowStyle,
            bodyProps=isc.addProperties({}, this.bodyProperties,
                {backgroundColor: event.backgroundColor, textColor: event.textColor,
                styleName: styleName + "Body"}
            ),
            headerProps=isc.addProperties({dragTarget: this.eventDragTarget}, this.headerProperties, 
                {backgroundColor: event.headerBackgroundColor, textColor: event.headerTextColor,
                styleName: styleName + "Header"}
            ),
            showMembers = true
        ;
        if (cal.showEventDescriptions == false) {
            //bodyProps = {height: 0, overflow:"hidden" };
            //headerProps = {height: "*"};
            showMembers = false;
        }
        var canDrag = (cal.canDragEvents == true && event[cal.canDragEventField] != false),
            canEditEvent = cal.canEditEvent(event)
        ;

        if (showMembers) headerProps.headerHeight = 14;

        var eventWinProps = isc.addProperties({
                isEventCanvas: true,
                calendar: cal,
                _redrawWithParent: false,
                styleName: styleName,
                baseStyle: styleName,
                canDragReposition: canDrag && canEditEvent,
                canDragResize: cal.canResizeTimelineEvents && canEditEvent,
                edgeMarginSize:10,
                // leave this at false, because that's what its always been - but overlay with
                // eventWindowProperties below, to allow close buttons to be displayed
                showCloseButton: false,
                event: event,
                descriptionText: event[cal.descriptionField] || "",
                showHeader: showMembers,
                showBody: showMembers,

                dragTarget: this.eventDragTarget,

                backgroundColor: event.backgroundColor,
                textColor: event.textColor,

                headerProperties: headerProps,
                bodyProperties: bodyProps,
                headerStyle: styleName + "Header",
                bodyStyle: styleName + "Body"
            }, cal.eventWindowDefaults, cal.eventWindowProperties
        );

        if (event.backgroundColor) {
            eventWinProps.backgroundColor = event.backgroundColor;
        }

        //var eventWin = this.createAutoChild("eventCanvas", eventWinProps, this.eventCanvasConstructor);
        var canvasClass = isc.ClassFactory.getClass(this.eventCanvasConstructor ||
                cal.eventCanvasConstructor);
        var eventWin =  (canvasClass || isc.TimelineWindow).create(eventWinProps);

        eventWin._availableForUse = false;
        
        cal.setEventCanvasID(this, event, eventWin.ID);
        //isc.logWarn('getNewEventWindow:' + [eventWin.ID, eventWin.canDragResize]);
        return eventWin;
    },

    // timeliveView
    updateEventWindow : function (event) {
        if (!this.body || !this.body.children) return;

        var cal = this.getCalendar(),
            laneName = event[cal.laneNameField]
        ;

        // if one event is updated, all events in the same row may need to be updated as
        // well due to overlapping. By passing a type into tagDataForOverlap, only
        // events in the same row as event will be processed
        var events = this.tagDataForOverlap(cal.data.getRange(0, cal.data.getLength()), 
                laneName);

        if (this.renderEventsOnDemand) {
            // just refresh events
            this.refreshVisibleEvents();
        } else {
            for (var i = 0; i < events.length; i++) {
                var thisEvent = events.get(i), 
                    eWin = this.getCurrentEventCanvas(this, thisEvent)
                ;
                // make sure to re-initialize the object that the eventWindow is pointing to, which
                // gets out of sync on update
                eWin.event = thisEvent;
                this.sizeEventWindow(eWin);
            }    
        }
        
        
    },
    
    areSame : function (first, second) {
        var cal = this.getCalendar();
        if (cal.dataSource) {
            var pks = cal._pks, areEqual = true;
            for (var pkName in pks) {
                if (first[pkName]!= second[pkName]) {
                    areEqual = false;
                    break;
                }
            }
            return areEqual;
        } else {
            return (first === second);    
        }
    }

}); // end timelineView addProperties()

isc.Calendar.registerStringMethods({
    getDayBodyHTML : "date,events,calendar,rowNum,colNum",
    getDayHeaderHTML : "date,events,calendar,rowNum,colNum",
    dayBodyClick : "date,events,calendar,rowNum,colNum",
    dayHeaderClick : "date,events,calendar,rowNum,colNum",
    eventClick : "event,viewName",
    eventChanged : "event",
    eventMoved : "newDate,event",
    eventResized : "newDate,event",
    //> @method calendar.backgroundClick
    // Callback fired when the mouse is clicked in a background-cell, ie, one without an 
    // event.
    //
    // @param startDate (Date) start datetime of the selected slot
    // @param endDate (Date) end datetime of the selected slot
    // @return (boolean) return false to cancel the default behavior of creating a new 
    //                      event at the selected location and showing its editor.
    // @visibility external
    //<
    backgroundClick : "startDate,endDate",
    //> @method calendar.backgroundMouseDown
    // Callback fired when the mouse button is depressed over a background-cell, ie, one 
    // without an event.  Return false to cancel the default behavior of allowing sweep
    // selection via dragging.
    //
    // @param startDate (Date) start datetime of the selected slot
    // @return (boolean) return false to suppress default behavior of allowing sweep
    //                      selection via dragging.
    // @visibility external
    //<
    backgroundMouseDown : "startDate",
    //> @method calendar.backgroundMouseUp
    // Notification method fired when the mouse button is released over a background-cell, ie, 
    // one without an event.  Return false to cancel the default behavior of showing a dialog
    // to add a new event with the passed dates.
    //
    // @param startDate (Date) the datetime of the slot where the mouse button was depressed
    // @param endDate (Date) the datetime of the slot where the mouse button was released
    // @return (boolean) return false to suppress default behavior of showing a dialog
    //                      to add a new event with the passed dates.
    // @visibility external
    //<
    backgroundMouseUp : "startDate,endDate"
});

isc.DaySchedule.addClassProperties({

    
    _getEventScaffolding : function (minsPerRow, startDate) {
        var rowCount = (60 / minsPerRow) * 24,
            data = [],
            row = {label:"", day1:"", day2:"", day3:"", day4:"", day5:"", day6:"", day7:""},
            today = startDate || new Date(),
            date = new Date(today.getFullYear(), today.getMonth(), today.getDate(),0, 0, 0, 0)
        ;

        for (var i=0; i<rowCount; i++) {
            var time = isc.Time.toTime(date);
            data.add(isc.addProperties({}, row, { time: time}));
            date = isc.DateUtil.dateAdd(date, "mn", minsPerRow, 1);
        }
        return data;
    }

});


// Call the AutoTest method to apply Calendar-specific methods now we've loaded
isc.AutoTest.customizeCalendar();



