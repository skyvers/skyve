/*

  SmartClient Ajax RIA system
  Version v13.0p_2023-11-09/LGPL Deployment (2023-11-09)

  Copyright 2000 and beyond Isomorphic Software, Inc. All rights reserved.
  "SmartClient" is a trademark of Isomorphic Software, Inc.

  LICENSE NOTICE
     INSTALLATION OR USE OF THIS SOFTWARE INDICATES YOUR ACCEPTANCE OF
     ISOMORPHIC SOFTWARE LICENSE TERMS. If you have received this file
     without an accompanying Isomorphic Software license file, please
     contact licensing@isomorphic.com for details. Unauthorized copying and
     use of this software is a violation of international copyright law.

  DEVELOPMENT ONLY - DO NOT DEPLOY
     This software is provided for evaluation, training, and development
     purposes only. It may include supplementary components that are not
     licensed for deployment. The separate DEPLOY package for this release
     contains SmartClient components that are licensed for deployment.

  PROPRIETARY & PROTECTED MATERIAL
     This software contains proprietary materials that are protected by
     contract and intellectual property law. You are expressly prohibited
     from attempting to reverse engineer this software or modify this
     software for human readability.

  CONTACT ISOMORPHIC
     For more information regarding license rights and restrictions, or to
     report possible license violations, please contact Isomorphic Software
     by email (licensing@isomorphic.com) or web (www.isomorphic.com).

*/

if(window.isc&&window.isc.module_Core&&!window.isc.module_Calendar){isc.module_Calendar=1;isc._moduleStart=isc._Calendar_start=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc._moduleEnd&&(!isc.Log||(isc.Log && isc.Log.logIsDebugEnabled('loadTime')))){isc._pTM={ message:'Calendar load/parse time: ' + (isc._moduleStart-isc._moduleEnd) + 'ms', category:'loadTime'};
if(isc.Log && isc.Log.logDebug)isc.Log.logDebug(isc._pTM.message,'loadTime');
else if(isc._preLog)isc._preLog[isc._preLog.length]=isc._pTM;
else isc._preLog=[isc._pTM]}isc.definingFramework=true;


if (window.isc && isc.version != "v13.0p_2023-11-09/LGPL Deployment" && !isc.DevUtil) {
    isc.logWarn("SmartClient module version mismatch detected: This application is loading the core module from "
        + "SmartClient version '" + isc.version + "' and additional modules from 'v13.0p_2023-11-09/LGPL Deployment'. Mixing resources from different "
        + "SmartClient packages is not supported and may lead to unpredictable behavior. If you are deploying resources "
        + "from a single package you may need to clear your browser cache, or restart your browser."
        + (isc.Browser.isSGWT ? " SmartGWT developers may also need to clear the gwt-unitCache and run a GWT Compile." : ""));
}




//> @class CalendarView
// CalendarView is a base class, extended by the various views available in a
// +link{class:Calendar, Calendar}.
//
// @inheritsFrom ListGrid
// @treeLocation Client Reference/Calendar
// @visibility calendar
//<
isc.ClassFactory.defineClass("CalendarView", "ListGrid");

isc.CalendarView.addProperties({

    isCalendarView: true,

    showHeaderSpanContextMenu: false,

    autoDraw: false,

    verticalEvents: true,

    // Only TimelineView supports multiDayEvents
    allowMultiDayEvents: false,

    // this attribute should be false for all Calendar types except Timelines, which might
    // well need a timeItem
    showDateChooserTimeItem: false,

    // needed to avoid the grid scrolling to 0,0 when clicking body children (eventCanvases)
    hiliteRowOnFocus: false,

    hoverDelay: 0,
    showHover: true,
    canHover: true,
    hoverByCell: null,
    hoverAutoFitWidth: true,
    hoverAutoFitMaxWidth: 200,

    // prevent mouseUp/mouseDown from bubbling beyond the CalendarView
    mouseUp : function () {
        return isc.EH.STOP_BUBBLING;
    },
    mouseDown : function () {
        return isc.EH.STOP_BUBBLING;
    },


    canFreezeFields: false,

    // switch off alternateFieldStyles
    alternateFieldStyles: false,
    alternateRecordStyles: false,

    // default dateEditingStyle (vertical views) is "time", no day-selection
    dateEditingStyle: "time",

    // Avoid ever attempting to participate in the saved searches subsystem.
    canSaveSearches:false,

    init : function () {
        this.calendar = this.creator;
        // initialize a bunch of objects used to manage events and canvases
        this.resetEventCaches(true);
        this.resetCanvasCaches(true);
        this.resetLaneCaches(true);
        return this.Super("init", arguments);
    },

    resetEventCaches : function (initialize) {
        // initialize or clear a bunch of objects used to manage events and canvases
        this._allEventMap = initialize ? {} : null;         // map of event.__key to event-record
        this._usedEventMap = initialize ? {} : null;        // event-records attached to canvases - __key to canvasID
        this._lastUsedEventMap = initialize ? {} : null;    // last canvas used by a given event - __key to canvasID
    },
    resetCanvasCaches : function (initialize) {
        // initialize or clear a bunch of objects used for managing canvases
        this._allCanvasMap = initialize ? {} : null;        // all event-canvases - canvasID to canvas
        this._pooledCanvasMap = initialize ? {} : null;     // available canvases (hidden, no event attached) - canvasID to canvas
        this._usedCanvasMap = initialize ? {} : null;       // canvases attached to events - canvasID to event.__key
        this._lastUsedCanvasMap = initialize ? {} : null;   // last event used by a given canvas - canvasID to event.__key
    },
    resetLaneCaches : function (initialize) {
        // initialize or clear a bunch of objects used for caching lane details
        this.laneMap = initialize ? {} : null;          // lane-name to lane-object
        this.laneIndexCache = initialize ? {} : null;   // index cache
        this.laneHeightCache = initialize ? {} : null;  // height cache
    },

    initWidget : function () {
        // initialize a cache to store some frequently used props that only change with a rebuild
        this._cache = {};
        // initialize the local _eventData array - returned from getEventData()
        this._eventData = [];
        this._snapGapList = [];

        var cal = this.calendar;
        this.firstDayOfWeek = cal.firstDayOfWeek;
        var showHover = this.showHover;
        if (showHover == null) showHover = cal.showViewHovers;
        this.setShowHover(showHover);
        this.Super("initWidget", arguments);

        // only installed on TimelineView for now
        if (this.installLocalHandlers) this.installLocalHandlers()

        // always run setChosenDate() to get things ready
        this.setChosenDate(this.chosenDate);
    },

    setEventData : function (events) {
        this._eventData = events;
    },

    getEventData : function () {
        // get the local array of events
        if (!this._eventData) this._eventData = [];
        return this._eventData;
    },

    addEventData : function (event) {
        var data = this.getEventData();
        if (!data.contains(event)) {
            data.add(event);
            // return true, event was added
            return true;
        }
        // return false, event was already present
        return false;
    },

    removeEventData : function (event) {
        // remove the event from the local array
        this.getEventData().remove(event);
    },



    viewMouseMove : function (a, b, c, d, e) {
    },

    viewDragMove : function (event) {
        this.logInfo("In viewDragMove");
        this.viewMouseMove();
    },

    getMouseData : function () {
        return this._mouseData;
    },

    // if this returns true, events are sized to content and stacked in a VLayout
    shouldShowColumnLayouts : function () {
        if (this.isMonthView()) return false;
        if (this.isTimelineView()) return false;
        return !!this.calendar.showColumnLayouts;
    },


    canTabToHeader: false,


    //includeRangeCriteria: null,
    //fetchMode: "view",
    rangeCriteriaMode: null,

    getVisibleStartDate : function () {
        return this.startDate;
    },
    getVisibleEndDate : function () {
        return this.endDate;
    },

    _usDateRegex:/^\d{4}.\d\d?.\d\d?$/,
    _jpDateRegex:/^\d\d?.\d\d.\d{4}?$/,
    rangeUnit: "D",
    setChosenDate : function (newDate) {
        var cal = this.calendar;
        // newDate should be a logicalDate, as passed internally from Calendar.setChosenDate(),
        // so should already be in the defaultDisplayTimezone - but run it through
        // getLogicalDateOnly() anyway
        var chosenDate = newDate.logicalDate ? newDate.duplicate() :
                isc.DateUtil.getLogicalDateOnly(newDate);

        var chosenWeek = chosenDate.getWeek();

        // get logical date and week
        this.logicalDate = chosenDate.duplicate();
        //isc.logWarn("logicalDate is " + this.logicalDate.toString());
        this.chosenDate = chosenDate;
        this.chosenWeek = chosenWeek;

        // always update the firstDayOfWeek from the parent caledar
        this.firstDayOfWeek = this.calendar.firstDayOfWeek;
        var fDOW = this.firstDayOfWeek;

        // set the range dates
        if (this.rangeUnit) {
            this.periodStartDate = this.startDate = isc.DateUtil.getStartOf(this.chosenDate, this.rangeUnit, false, fDOW);
            this.periodEndDate = this.endDate = isc.DateUtil.getEndOf(this.chosenDate, this.rangeUnit, false, fDOW);

            if (this.isWeekView()) {
                // in weekView, set a couple of special values for use purely in the dateLabel
                // above the calendar
                this.labelStartDate = this.startDate.duplicate();
                this.labelEndDate = this.endDate.duplicate();

                if (cal.showWeekends == false) {
                    // in weekView, when showWeekends is false, modify the start/end dates so they
                    // match the first and last visible dates - the periodStart/End are still the
                    // full week extents
                    if (isc.DateUtil.isWeekend(this.startDate)) {
                        this.labelStartDate = isc.DateUtil.getNextWeekday(this.startDate, cal.getWeekendDays());
                    }
                    if (isc.DateUtil.isWeekend(this.endDate)) {
                        this.labelEndDate = isc.DateUtil.getPreviousWeekday(this.endDate, cal.getWeekendDays());
                    }
                }
            }

            // get logical date and week
            this.logicalDate = isc.DateUtil.createLogicalDate(this.startDate);
        }

        this._refreshingEvents = false;

        if (this.isSelectedView()) {
            // if scrollToWorkday is set, set it up here
            if (this.verticalEvents && cal.showWorkday && cal.scrollToWorkday) {
                this._needsScrollToWorkdayStart = true;
            }
            this.updateRange();
            this._refreshEvents();
        } else {
            // if scrollToWorkday is set, set it up here
            if (this.verticalEvents && cal.showWorkday && cal.scrollToWorkday) {
                this._needsScrollToWorkdayStart = true;
            }
            this._needsUpdateRange = true;
            this._needsRefresh = true;
        }
    },

    getPeriodStartDate : function () {
        return this.periodStartDate;
    },
    getPeriodEndDate : function () {
        return this.periodEndDate;
    },

    updateRange : function (newDate) {
        delete this._needsUpdateRange;
        if (this.isWeekView() || this.isDayView()) {
            this.updateFieldDates();
            this.updateRowDates();
            // if _needsScrollToWorkdayStart is set, do that here
            if (this._needsScrollToWorkdayStart) {
                delete this._needsScrollToWorkdayStart;
                this.delayCall("scrollToWorkdayStart");
            }
            this.markForRedraw();
        } else if (this.isMonthView()) {
        }
    },


    getFieldTitle : function (fieldId) {
        var cal = this.calendar,
            title = this.Super("getFieldTitle", arguments),
            field = this.getUnderlyingField(fieldId)
        ;
        if (field) {
            var date = field.date,
                // monthView doesn't have field.date, because a field represents multiple dates
                dayOfWeek = date ? date.getDay() : field._dayNum
            ;
            if (dayOfWeek != null) {
                if (cal.getDateHeaderTitle) {
                    title = cal.getDateHeaderTitle(date, dayOfWeek, title, this) || title;
                }
            }
        }
        return title;
    },

    getMinimumSnapGapTime : function (unit) {
        // get the min sensible snapGap time that can be represented in the current resolution
        // - if a specified or calculated eventSnapGap is smaller than this value, it will be
        // defaulted to this value, to prevent rendering and sizing issues
        var props = this._cache,
            millis = props.minimumSnapGapMillis
        ;
        if (!millis) {
            var opts = [ 1, 5, 10, 15, 20, 30, 60, 120, 180, 240, 360, 480, 720, 1440];
            var mins = isc.DateUtil.convertPeriodUnit(props.millisPerPixel, "ms", "mn");
            for (var i=0; i<opts.length; i++) {
                if (mins <= opts[i]) {
                    mins = opts[i];
                    break;
                }
            }
            millis = isc.DateUtil.convertPeriodUnit(mins, "mn", "ms");
        }
        if (!unit) unit = "mn";
        return Math.floor(isc.DateUtil.convertPeriodUnit(millis, "ms", unit));
    },

    getTimePerCell : function (unit) {
       var cal = this.calendar,
            props = this._cache,
            millis = props.millisPerCell
        ;
        if (!millis) {
            millis = isc.DateUtil.convertPeriodUnit(cal.minutesPerRow, "mn", "ms");
        }
        if (!unit) unit = "mn";
        return Math.floor(isc.DateUtil.convertPeriodUnit(millis, "ms", unit));
    },
    getTimePerSnapGap : function (unit) {
         var cal = this.calendar,
            props = this._cache,
            millis = props.millisPerSnapGap
        ;
        if (!millis) {
            if (props.calendarEventSnapGap == null) {
                // eventSnapGap is null, use timePerCell (minutesPerRow)
                millis = this.getTimePerCell("ms");
            } else if (props.calendarEventSnapGap == 0) {
                // eventSnapGap is zero, use the smallest snaps that can be represented
                millis = this.getMinimumSnapGapTime("ms");
            } else {
                millis = isc.DateUtil.convertPeriodUnit(props.calendarEventSnapGap, "mn", "ms");
            }
            props.millisPerSnapGap = millis;
        }
        if (!unit) unit = "mn";
        return isc.DateUtil.convertPeriodUnit(millis, "ms", unit);
    },
    getTimePerPixel : function (unit) {
        var cal = this.calendar,
            props = this._cache
        ;

        var msPerPixel = Math.floor(this.getTimePerCell("ms") / this.cellHeight);
        if (!unit) unit = "mn";
        return isc.DateUtil.convertPeriodUnit(msPerPixel, "ms", unit);
    },

    getSnapGapPixels : function (rowNum, colNum) {
        var snapCount = this.getTimePerCell() / this.getTimePerSnapGap();
        return Math.round(this.cellHeight/snapCount);
    },

    getDateLabelText : function (startDate, endDate) {
        return null;
    },

    setShowHover : function (showHover) {
        if (this.showViewHovers == false) return;
        this.showHover = showHover;
        this.canHover = showHover;
    },

    shouldShowEventHovers : function () {
        if (this.showHover == false || this.calendar.showViewHovers == false) return false;
        if (this.showEventHovers != null) return this.showEventHovers;
        return this.calendar.showEventHovers;
    },
    shouldShowHeaderHovers : function () {
        if (this.showHover == false || this.calendar.showViewHovers == false) return false;
        if (this.showHeaderHovers != null) return this.showHeaderHovers;
        return this.calendar.showHeaderHovers;
    },
    shouldShowLaneFieldHovers : function () {
        if (this.showHover == false) return false;
        if (this.showLaneFieldHovers != null) return this.showLaneFieldHovers;
        return this.calendar.showLaneFieldHovers;
    },
    shouldShowCellHovers : function () {
        if (this.showHover == false) return false;
        if (this.showCellHovers != null) return this.showCellHovers;
        return this.calendar.showCellHovers;
    },
    shouldShowDragHovers : function () {
        if (this.showHover == false) return false;
        if (this.showDragHovers != null) return this.showDragHovers;
        return this.calendar.showDragHovers;
    },
    shouldShowZoneHovers : function () {
        if (this.shouldShowCellHovers()) return false;
        if (this.showZoneHovers != null) return this.showZoneHovers;
        return this.calendar.showZoneHovers;
    },


    // standard helpers, applicable to all views

    //> @attr calendarView.calendar (Calendar : null : R)
    // The +link{Calendar, calendar} this view is in.
    // @visibility external
    //<

    //> @attr calendarView.viewName (String : null : R)
    // The name of this view, used to identify it in the +link{calendarView.calendar, calendar}.
    // @visibility external
    //<

    //> @method calendarView.isSelectedView()
    // Returns true if this view is the currently selected view in the parent calendar.
    // @return (Boolean) true if the view is selected in the parent calendar, false otherwise
    // @visibility external
    //<
    isSelectedView : function () {
        return this.creator.getCurrentViewName() == this.viewName;
    },
    //> @method calendarView.isTimelineView()
    // Returns true if this is the +link{calendar.timelineView, timeline view}, false otherwise.
    // @return (boolean) true if this is a Timeline view
    // @visibility external
    //<
    isTimelineView : function () {
        return this.viewName == "timeline";
    },
    //> @method calendarView.isDayView()
    // Returns true if this is the +link{calendar.dayView, day view}, false otherwise.
    // @return (boolean) true if this is a Day view
    // @visibility external
    //<
    isDayView : function () {
        return this.viewName == "day";
    },
    //> @method calendarView.isWeekView()
    // Returns true if this is the +link{calendar.weekView, week view}, false otherwise.
    // @return (boolean) true if this is a Week view
    // @visibility external
    //<
    isWeekView : function () {
        return this.viewName == "week";
    },
    //> @method calendarView.isMonthView()
    // Returns true if this is the +link{calendar.monthView, month view}, false otherwise.
    // @return (boolean) true if this is a Month view
    // @visibility external
    //<
    isMonthView : function () {
        return this.viewName == "month";
    },

    updateLaneRollover : function (newRow) {
        if (!this.isTimelineView()) return;
        this.clearLastHilite();
        if (newRow == null) return;
        this.body.lastOverRow = newRow;
        this.body.updateRollOver(newRow);
    },

    //> @method calendarView.rebuild()
    // Rebuild this CalendarView, including re-fetching its data as necessary.  To avoid
    // re-fetching the data, pass 'false' to this method, or call
    // +link{calendarView.refreshEvents, refreshEvents()} instead.
    // @param [refreshData] (Boolean) If false, prevents data from bing refreshed.
    // @visibility external
    //<
    rebuild : function (refreshData) {
        if (refreshData == null) refreshData = true;
        if (this._rebuild) this._rebuild(refreshData);
        else if (this.rebuildFields) {
            // this is a day/weekView - rebuild the fields and call setChosenDate() to refresh
            // events
            this.rebuildFields();
            this.setChosenDate(this.chosenDate);
        } else {
            // this is monthView - refresh the fields, to pick up showIf, and refresh the events
            if (this.isMonthView()) this.refreshFields();
            this.refreshEvents("rebuild");
        }
    },
    initCacheValues : function () {
        var cal = this.calendar;
        this._cache = {
            firstDayOfWeek: this.firstDayOfWeek,
            rangeStartDate: cal.getPeriodStartDate(this),
            rangeEndDate: cal.getPeriodEndDate(this),
            calendarEventSnapGap: cal.eventSnapGap
        };
        this._cache.rangeStartMillis = this._cache.rangeStartDate.getTime();
        this._cache.rangeEndMillis = this._cache.rangeEndDate.getTime();
        this.updateSnapProperties();
        return this._cache;
    },

    updateSnapProperties : function () {
        delete this._cache.millisPerCell;
        delete this._cache.millisPerSnapGap;
        delete this._cache.millisPerPixel;
        delete this._cache.snapGapPixels;
        this._cache.millisPerCell = this.getTimePerCell("ms");
        this._cache.millisPerPixel = this.getTimePerPixel("ms");
        this._cache.minimumSnapGapMillis = this.getMinimumSnapGapTime("ms");
        this._cache.millisPerSnapGap = this.getTimePerSnapGap("ms");
    },
    // temp attribute showLaneFields - allows lane fields to be hidden in timelines
    showLaneFields: null,



    //>    @attr calendarView.useEventCanvasPool (Boolean : true : IRW)
    // Should +link{EventCanvas, event canvas} instances be reused when visible events change?
    // @visibility external
    //<
    useEventCanvasPool: true,
    // incomplete poolingMode implementation, just so we can switch to a better default right
    // away - "data" mode only pools the event canvases when dataChanged (and refreshEvents)
    // happens - the other mode of "viewport" pools windows as soon as they leave the viewport
    eventCanvasPoolingMode: "data",

    //> @attr calendarView.eventStyleName  (CSSStyleName : null : IRW)
    // If specified, overrides +link{calendar.eventStyleName} and dictates the CSS style to
    // use for events rendered in this view.  Has no effect on events that already have a
    // +link{calendarEvent.styleName, style specified}.
    //
    // @group appearance
    // @visibility external
    //<

    // -------------------------
    // Lanes and Sublanes
    // --------------------------

    getLaneIndex : function (laneName) { return null; },
    getLane : function (lane) { return null; },
    getLaneFromPoint : function (x, y) { return null; },

    getSublane : function (laneName, sublaneName) {
        if (!this.hasSublanes()) return null;
        var lane = this.getLane(laneName),
            sublane = lane && lane.sublanes ?
                        isc.isAn.Object(sublaneName) ? sublaneName :
                        lane.sublanes.find(this.calendar.laneNameField, sublaneName)
                      : null
        ;
        return sublane;
    },
    getSublaneFromPoint : function (x, y) { return null; },

    setLaneTitle : function (lane, title) {
        // get the live lane object, set it's title, and do an update in-place
        var l = this.getLane(lane);
        if (!l) return false;
        l.title = title;
        this.updateLanes(this.lanes);
        return true;
    },

    updateLanes : function (lanes) {
        // call setLanes without refreshing events - called as part of refreshEvents()
        this.setLanes(lanes, true);
        // refresh the body here so that getRowTop() and similar work as expected immediately
        // following calls to this method
        this.body.redraw();
    },

    // get the internal laneName -> lane-object map
    getLaneMap : function () {
        return this.laneMap;
    },

    usesLanes : function () {
        return this.isTimelineView() || (this.isDayView() && this.calendar.showDayLanes);
    },
    hasLanes : function () {
        return this.usesLanes() && this.lanes && this.lanes.length > 0;
    },
    hasSublanes : function () {
        return this.calendar.useSublanes && this.usesLanes();
    },
    useLanePadding : function () {
        if (this.isTimelineView()) return true;
        if (this.usesLanes()) {
            // don't introduce horizontal padding between events in a vertical lane if the
            // calendar is set up to overlap the event canvases
            return this.calendar.eventOverlap ? false : true;
        }
        return false;
    },

    canScrollToEvent : function (event) {
        var rangeStart = this.startDate.getTime(),
            rangeEnd = this.endDate.getTime(),
            sDate = this.calendar.getEventStartDate(event),
            eDate = this.calendar.getEventEndDate(event)
        ;
        if ((sDate.getTime() >= rangeStart && sDate.getTime() <= rangeEnd) ||
            (eDate.getTime() >= rangeStart && eDate.getTime() <= rangeEnd))
        {
            return true;
        }
        return false;
    },
    scrollToEvent : function (event) {
        var cal = this.calendar,
            range = { start: this.startDate.getTime(), end: this.endDate.getTime() },
            sDate = cal.getEventStartDate(event),
            eDate = cal.getEventEndDate(event)
        ;
        if (this.canScrollToEvent(event)) {
            var x = null,
                y = null
            ;
            var canvas = this.getCurrentEventCanvas(event);
            if (canvas) {
                x = canvas.getLeft();
                y = canvas.getTop();
            } else {
                x = this.getDateLeftOffset(sDate);
                if (this.isTimelineView()) {
                    y = this.getRowTop(this.getLaneIndex(event[cal.laneNameField]));
                } else {
                    y = this.getDateTopOffset(sDate, event[cal.laneNameField]);
                }
            }
            // event is in the current scrollable range - scroll to it
            this.body.scrollTo(x, y);
        }
    },

    getCellCSSText : function (record, rowNum, colNum) {
        var result = this.calendar._getCellCSSText(this, record, rowNum, colNum);

        return result;
        //if (result) return result;
        //return this.Super("getCellCSSText", arguments);
    },

    getEventCanvasStyle : function (event) {
        if (this.usesLanes()) {
            var cal = this.calendar,
                lnField = cal.laneNameField,
                slnField = cal.sublaneNameField,
                styleField = cal.eventStyleNameField,
                lane = this.getLane(event[lnField]),
                sublane = lane && cal.useSublanes ? this.getSublane(lane[lnField], event[slnField]) : null
            ;
            // get the eventStyleName from the sublane, then the lane, then this view
            return (sublane && sublane.eventStyleName) || (lane && lane.eventStyleName)
                        || this.eventStyleName;
        }
        return this.eventStyleName
    },

    getDateFromPoint : function () {
        return this.getCellDate();
    },

// cell hovers
    cellHoverHTML : function (record, rowNum, colNum) {
        var html = this.calendar._getCellHoverHTML(this, record, rowNum, colNum);
        return html;
    },
    headerHoverHTML : function (fieldNum) {
        var field = this.getField(fieldNum);
        return this.calendar._getHeaderHoverHTML(this, this.fieldHeaderLevel,
            field, field.date, field.endDate);
    },
    // all views
    getPrintHTML : function (printProperties, callback) {
        if (this.isMonthView()) return this.Super("getPrintHTML", arguments);
        if (callback) {
            this.delayCall("asyncGetPrintHTML", [printProperties, callback]);
            return null;
        } else {
            return this.asyncGetPrintHTML(printProperties, callback);
        }
    },

    // all views
    asyncGetPrintHTML : function (printProperties, callback) {

        this.__printing = true;

        // force a refresh of ALL events - this will create and draw canvases for any events
        // that haven't yet been scrolled into view
        this.refreshVisibleEvents(true, "asyncGetPrintHTML");

        printProperties = isc.addProperties({}, printProperties);

        this.body.printChildrenAbsolutelyPositioned = true;

        var cal = this.calendar,
            isTimeline = this.isTimelineView(),
            isWeek = this.isWeekView(),
            isDay = this.isDayView(),
            isMonth = this.isMonthView()
        ;

        if (isMonth) return;

        var fields = this.getVisibleFields(this.completeFields),
            data = this.getData(),
            output = isc.StringBuffer.create(),
            totalWidth = 0,
            fieldWidths = []
        ;

        // if the (Timeline) view is grouped, use the _openListCache
        if (isc.isA.Tree(data)) data = data._openListCache;

        for (var i=0; i<fields.length; i++) {
            var field = fields[i];
            var button = this.getFieldHeaderButton(field.masterIndex);
            var result = button ? button.width || button.getVisibleWidth() : null;
            if (result == null) result = this.getFieldWidth(field);
            // round the field-width
            result = isTimeline ? Math.ceil(result) : Math.floor(result);
            fieldWidths.add(result);
        }

        totalWidth = fieldWidths.sum();

        var rowStart = "<TR",
            rowEnd = "</TR>",
            gt = ">",
            heightAttr = " HEIGHT=",
            valignAttr = " VALIGN="
        ;


        var bodyVOffset = 40;

        output.append("<div style='position:relative;'>");

        output.append("<TABLE cellpadding='0' cellspacing='0' WIDTH='", totalWidth,
            "' style='",
            "border: 1px solid grey;'>"
        );

        output.append("<THEAD>");

        if (this.showHeader) {
            var headerHTML = this.getPrintHeaders(0, this.fields.length, fieldWidths);
            //headerHTML = "<span style='whitespace:nowrap;'>" + headerHTML + "</span>";
            this._headerPrintHeight = isc.Canvas.measureContent(headerHTML, "printHeader", true, true, { wrap: false});
            //isc.logWarn( "header print height: " + this._headerPrintHeight);
            // don't generate column-headers for dayView
            output.append(headerHTML);
        }

        output.append("</THEAD>");

        // absolutely position the body and events after the header
        bodyVOffset += this.getHeaderHeight();

        output.append("<TBODY>");

        for (var i=0; i<data.length; i++) {
            var rowHeight = Math.floor(this.getRowHeight(data[i], i));
            // deal with differing row-heights in zoomed pages
            if (i < data.length - 1) {
                rowHeight = this.getRowTop(i+1) - this.getRowTop(i);
            }
            output.append(rowStart, heightAttr, rowHeight, gt);
            for (var j=0; j<fields.length; j++) {
                var value = this.getCellValue(data[i], i, j);
                output.append("<TD class='", this.getCellStyle(data[i], i, j), "' ",
                    "style='width:", fieldWidths[j]-1,  "px; ",
                    "box-sizing: border-box; ",
                    //(isTimeline ? "max" : "min"), "-width:",  fieldWidths[j]-1 + "px;",
                    "max-width:",  fieldWidths[j]-1 + "px;",
                    "border-width: 0px 1px 1px 0px; ",
                    "height: ", rowHeight, "px;",
                    "border-bottom: 1px solid #ABABAB; border-right: 1px solid #ABABAB; ",
                    "border-top: none; border-left: none;",
                    this.getCellCSSText(data[i], i, j),
                    "'>"
                );
                output.append(this.getCellValue(data[i], i, j) || "&nbsp;");
                output.append("</TD>");
            }
            output.append(rowEnd);
        }

        output.append("</TBODY>");
        output.append("</TABLE>");

        var events = this.body.children;
        for (var i=0, len=events.length; i<len; i++) {
            var event = events[i],
                isValid = event.isEventCanvas || event.isZoneCanvas || event.isIndicatorCanvas;
            if (!isValid) {
                if (this.shouldShowColumnLayouts() && isc.isA.Layout(event)) {
                    // events are children of per-column VLayouts - script the layout's
                    // outerHTML (includes placement)
                    output.append(event.getHandle().outerHTML);
                }
                continue;
            }
            // events are children of the grid-body
            if (!event.isDrawn() || !event.isVisible()) continue;
            if (event.isZoneCanvas) printProperties.i = 0;
            else if (event.isIndicatorCanvas) printProperties.i = len;
            else printProperties.i = i;
            var nextHTML = event.getPrintHTML(printProperties);
            output.append(nextHTML);
        }

        output.append("</div>");

        var result = output.release(false);

        if (callback) {
            this.fireCallback(callback, "HTML", [result]);
        }

        delete this.__printing;

        return result;
    },

    headerRowPrintHeight: 30,
    getPrintHeaders : function (startCol, endCol, fieldWidths) {

        var defaultAlign = (this.isRTL() ? isc.Canvas.LEFT : isc.Canvas.RIGHT),
            // consider printing headers with a headerButton style, which looks much better
            //printHeaderStyle = this.printHeaderStyle || this.headerBaseStyle,
            printHeaderStyle = this.headerBaseStyle,
            HTML
        ;

        var printHeight = this.headerRowPrintHeight;

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
                HTML[HTML.length] = "<TR HEIGHT=" + printHeight + ">";

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
                    HTML[HTML.length] = "' ";
                    var thisWidth = fieldWidths[(isField ? entry.masterIndex : ii)];
                    HTML[HTML.length] = "style='margin: 0px; padding: 0px; " +
                        "width:" + thisWidth + "px; height:" + printHeight + "px; " +
                        "border-width: 0px 1px 1px 0px;' "
                    ;
                    HTML[HTML.length] = ">";
                    HTML[HTML.length] = cellValue;
                    HTML[HTML.length] = "</TD>";

                }

                HTML[HTML.length] = "</TR>";
            }
        //         this.logWarn("\n\nGenerated print header HTML (including spans):" + HTML.join(""));

        } else {

            HTML = ["<TR HEIGHT=" + printHeight + ">"];

            var cellStartHTML = ["<TD CLASS='", printHeaderStyle, "' HEIGHT=" + printHeight + " ALIGN="].join(""),
                frozenCount = this.frozenBody ? this.frozenBody.fields.length : 0
            ;

            // Just iterate through the fields once, then assemble the HTML and return it.
            if (this.frozenBody) {
                for (var colNum = 0; colNum < frozenCount; colNum++) {
                    var field = this.frozenBody.fields[colNum];
                    if (!field) continue;
                    var align = field.align || defaultAlign;
                    //var width = field.width || this.getFieldWidth(colNum);
                    var width = fieldWidths[colNum];
                    var title = this.getHeaderButtonTitle(field.masterIndex);
                    HTML.addList([cellStartHTML, align, " style='margin: 0px; padding: 0px; " +
                        "width:" + width + "px; height:" + printHeight + "px; " +
                        "border-width: 0px 1px 1px 0px;'>", title, "</TD>"]);
                }
            }

            // Just iterate through the fields once, then assemble the HTML and return it.
            for (var colNum = 0; colNum < (endCol-frozenCount); colNum++) {
                var field = this.body.fields[colNum];
                if (!field) continue;
                var align = field.align || defaultAlign;
                //var width = field.width || this.getFieldWidth(colNum);
                var width = fieldWidths[colNum + frozenCount];
                HTML.addList([cellStartHTML, align, " style='width:" + width + "px;'>",
                                    this.getHeaderButtonTitle(field.masterIndex), "</TD>"]);
            }

            // Output the standard header row
            HTML[HTML.length] = "</TR>";
        }
        return HTML.join(isc.emptyString);
    },

    //> @attr calendarView.eventDragCanvasStyleName (CSSStyleName : "eventDragCanvas" : IR)
    // CSS class applied to the +link{calendarView.eventDragCanvas, eventDragCanvas}.
    //
    // @visibility calendar
    //<
    eventDragCanvasStyleName: "eventDragCanvas",

    //> @attr calendarView.eventDragCanvas (AutoChild Canvas : null : IR)
    // +link{Canvas} displayed while dragging or resizing an event in this view and styled
    // according to +link{calendarView.eventDragCanvasStyleName, eventDragCanvasStyleName}.
    //
    // @visibility calendar
    //<
    eventDragCanvasDefaults: {
        _constructor: "Canvas",
        width:1, height: 1,
        snapToGrid: false,
        autoDraw: false,
        moveWithMouse: false,
        dragAppearance: "target",
        visibility: "hidden",
        hideUsingDisplayNone: true,
        keepInParentRect: true,
        hoverMoveWithMouse: true,
        showHover: true,
        hoverDelay: 0,
        hoverProps: {
            overflow: "visible",
            hoverMoveWithMouse: this.hoverMoveWithMouse
        },
        getHoverHTML : function () {
            var canvas = this.eventCanvas,
                event = canvas.event,
                props = canvas._dragProps
            ;
            if (!props) return;

            var startDate = props._lastStartDate,
                endDate = props._lastEndDate,
                newEvent = this.view.calendar.createEventObject(event, startDate, endDate,
                    props._lastLane, props._lastSublane)
            ;
            return this.view.calendar._getDragHoverHTML(this.view, newEvent);
        },
        setView : function (view) {
            this.view = view;
        },
        getEventPadding : function () {
            var cal = this.eventCanvas.calendar;
            return cal.useDragPadding ? cal.getLanePadding(this.view) : 0;
        },
        show : function () {
            if (this._supressShow) return;
            //this.logWarn("in show() - stacktrace: \n" + isc.EH.getStackTrace());
            return this.Super("show", arguments);
        },
        fillOverlapSlots: true,
        positionToEventCanvas : function (show) {
            if (this._suppressShow || !this.eventCanvas) {
                this.hide();
                return isc.EH.STOP_BUBBLING;
            }

            var canvas = this.eventCanvas,
                cal = canvas.calendar,
                view = this.view,
                left = view.getEventLeft(canvas.event) + this.getEventPadding(),
                top = canvas.getTop(),
                width = (view._getEventBreadth ? view._getEventBreadth(canvas.event) : canvas.getVisibleWidth()),
                height = canvas.getVisibleHeight(),
                props = canvas._dragProps
            ;

            if (this.fillOverlapSlots) {
                // cause the drag rect to fill the column's width, or the row's height - if
                // there are sublanes, have the rect fill the sublane height or width
                if (view.isTimelineView()) {

                    var row = canvas._dragProps._startRow;
                    top = view.body.getRowTop(row);
                    if (canvas.isIndicatorCanvas) {
                        // for indicators, show the drag rect at actual height (over all lanes)
                        height = canvas.getVisibleHeight();
                        props._fixedTop = true;
                    } else if (!props._useSublanes) {
                        height = view.getLaneHeight(row);
                    } else {
                        top += props._lastSublane.top;
                        height = props._lastSublane.height;
                    }
                } else {

                    var col = canvas._dragProps._startCol;
                    left = view.body.getColumnLeft(col);
                    if (props._useLanes) {
                        if (!props._useSublanes) {
                            width = view.getLaneWidth(col);
                        } else {
                            left += props._lastSublane.left;
                            width = props._lastSublane.width;
                        }
                    } else {
                        width = view.body.getColumnWidth(col);
                    }
                }
            }

            if (this._resizing) {
                if (view.isTimelineView()) {
                    top = view.body.getRowTop(canvas._dragProps._startRow);
                } else {
                    left = view.body.getColumnLeft(canvas._dragProps._startCol);
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

            //if (view.shouldShowDragHovers()) isc.Hover.show(this.getHoverHTML(), this.hoverProps);
            if (view.shouldShowDragHovers()) this.handleHover();
        },
        moveToEvent : function () {
            // no-op here to avoid automatic snapping to the wrong place
        },
        dragRepositionStart : function () {

            if (!this.eventCanvas) return isc.EH.STOP_BUBBLING;

            var canvas = this.eventCanvas,
                event = canvas.event,
                cal = canvas.calendar,
                view = this.view,
                gr = view.body
            ;

            // canDragEvent() also calls canEditEvent(), which checks both event and calendar
            if (!cal.canDragEvent(event, view)) return false;

            this._repositioning = true;

            var eventRow = gr.getEventRow(),
                rowTop = gr.getRowTop(eventRow),
                rowHeight = gr.getRowHeight(view.getRecord(eventRow), eventRow),
                eventLeft = view.getEventLeft(event) + 1,
                eventCol = gr.getEventColumn(eventLeft),
                columnLeft = gr.getColumnLeft(eventCol),
                columnWidth = gr.getColumnWidth(eventCol),
                offsetX = gr.getOffsetX() - canvas.getLeft(),
                offsetY = gr.getOffsetY() - canvas.getTop()
            ;

            var isTimeline = view.isTimelineView();

            var dp = canvas._dragProps = {};

            dp._isVertical = !isTimeline;

            dp._startRow = eventRow;
            dp._startCol = eventCol;
            dp._rowHeight = rowHeight;
            dp._colWidth = columnWidth;

            dp._startWidth = isTimeline ? view._getEventBreadth(event) : dp._colWidth;
            dp._startHeight = isTimeline ? dp._rowHeight : canvas.getVisibleHeight();
            dp._currentRow = eventRow;
            dp._currentCol = eventCol;
            dp._startOffsetX = offsetX;
            // make the startOffsetY the snap-positionm, not just the mosue position
            dp._startOffsetY = view.getRowTop(view.getEventRow(offsetY));

            dp._rowCount = Math.round(dp._startHeight / dp._rowHeight);
            dp._maxRow = view.data.getLength() - dp._rowCount;
            dp._maxTop = view.getRowTop(dp._maxRow);
            dp._maxLeft = isTimeline ? gr.getScrollWidth() - dp._startWidth :
                    gr.getColumnLeft(gr.fields.length-1);
            dp._maxCol = isTimeline ? gr.getEventColumn(dp._maxLeft) :
                    gr.fields.length - 1;

            dp._lastStartDate = cal.getEventStartDate(event);
            dp._lastEndDate = cal.getEventEndDate(event);

            dp._lastValidStartDate = dp._lastStartDate.duplicate();
            dp._lastValidEndDate = dp._lastEndDate.duplicate();

            dp._startMouseDate = view.getDateFromPoint() || dp._lastStartDate.duplicate();
            dp._lastMouseDate = dp._startMouseDate.duplicate();

            dp._useLanes = view.usesLanes() && !canvas.isIndicatorCanvas && !canvas.isZoneCanvas;
            if (dp._useLanes) {
                var lane = view.getLane(event[cal.laneNameField]),
                    sublane = !lane || !lane.sublanes ? null :
                        lane.sublanes.find(cal.laneNameField, event[cal.sublaneNameField])
                ;
                dp._startLane = lane;
                dp._lastLane = lane;
                dp._useSublanes = cal.useSublanes && lane && lane.sublanes && lane.sublanes.length > 0;
                dp._startSublane = sublane;
                dp._lastSublane = sublane;
                dp._lockLane = !cal.canEditEventLane(event, view);
                dp._lockSublane = !cal.canEditEventSublane(event, view);
            }

            this.positionToEventCanvas(true);

            return isc.EH.STOP_BUBBLING;
        },
        dragRepositionMove : function () {

            if (!this.eventCanvas) return isc.EH.STOP_BUBBLING;

            var canvas = this.eventCanvas,
                props = canvas._dragProps,
                event = canvas.event,
                cal = canvas.calendar,
                view = this.view,
                eventSnapPixels = cal.getSnapGapPixels(view),
                isTL = view.isTimelineView(),
                gr = view.body,
                lanePadding = this.getEventPadding(),
                // IndicatorCanvas sets this value in positionToEventCanvas
                fixedTop = props._fixedTop != null ? props._fixedTop : -1,
                fixedLeft = -1,
                fixedWidth = -1,
                fixedHeight = -1
            ;

            // if the mouse has been dragged out of the body-rows, just bail - this prevents an
            // issue where dragging an event off the bottom of a vertical calendar (past midnight)
            // could shift it to 00:00 in the same day
            if (gr.getEventRow() < 0) return;

            // do the same horizontally
            if (gr.getEventColumn() < 0) return;

            var newMouseDate = view.getDateFromPoint();
            if (!newMouseDate) return;

            var newMouseLane = view.getLaneFromPoint();
            if (props._useLanes && !newMouseLane) return;

            if (props._lastMouseDate && props._lastMouseDate.getTime() == newMouseDate.getTime()
                && props._lastLane && props._lastLane == newMouseLane) return;

            if (props._useLanes) {
                //if (isTL) {
                    // handle top/height snapping for lanes and sublanes in timelines
                    var mouseLane = newMouseLane,
                        mouseSublane = props._useSublanes ? view.getSublaneFromPoint() : null
                    ;

                    if (!mouseLane || view.isGroupNode(mouseLane)) {
                        mouseLane = props._lastLane;
                        mouseSublane = props._lastSublane;
                    } else {
                        if (props._lockLane) {
                            mouseLane = props._startLane;
                            if (props._useSublanes &&
                                    (props._lockSublane || !mouseLane.sublanes.contains(mouseSublane)))
                            {
                                // sublane locked, or mouseSublane isn't in the mouseLane
                                // (because we changed it above)
                                mouseSublane = props._startSublane;
                            }
                        } else {
                            if (props._useSublanes) {
                                if (props._lockSublane) {
                                    // sublane locked - if there's a matching sublane in the new
                                    // lane, use that - otherwise, revert to last lane and sublane
                                    var localSublane = mouseLane.sublanes ?
                                          mouseLane.sublanes.find(cal.laneNameField, props._startSublane.name)
                                          : null
                                    ;
                                    if (localSublane) {
                                        // there's an appropriate sublane in the mouseLane - use it
                                        mouseSublane = localSublane;
                                    } else {
                                        // no appropriate sublane - use the last lane/sublane
                                        mouseLane = props._lastLane;
                                        mouseSublane = props._lastSublane;
                                    }
                                } else {
                                    // sublane isn't locked, but the current lane may not HAVE
                                    // any sublanes - revert to last lane and sublane if not
                                    if (mouseLane != props._lastLane) {
                                        if (!mouseLane.sublanes) {
                                            mouseLane = props._lastLane;
                                            mouseSublane = props._lastSublane;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    if (isTL) {
                        var laneRecordIndex = view.getRecordIndex(mouseLane);
                        fixedTop = view.getRowTop(laneRecordIndex);
                        if (mouseSublane) fixedTop += mouseSublane.top;
                        fixedHeight = (mouseSublane ? mouseSublane.height : mouseLane.height);

                        props._currentRow = laneRecordIndex;
                    } else {
                        var laneRecordIndex = view.getLaneIndex(mouseLane.name);
                        fixedLeft = view.body.getColumnLeft(laneRecordIndex);
                        fixedWidth = view.getLaneWidth(mouseLane.name);
                        if (mouseSublane) {
                            fixedLeft += mouseSublane.left;
                            fixedWidth = mouseSublane.width;
                        }

                        props._currentCol = laneRecordIndex;
                    }

                //}
            }

            // top/height -related
            var overRow = gr.getEventRow(),
                eventRow = Math.min(props._maxRow,
                    (overRow < 0 ? 0 : overRow)),
                rowTop = gr.getRowTop(eventRow),
                mouseY = gr.getOffsetY(),
                snapY = Math.floor((Math.floor((mouseY - rowTop) / eventSnapPixels)) * eventSnapPixels),
                // the snapTop needs to be calculated by the event's position relative to the
                // mouse, not just by the mouse-position itself - this fixes an issue where
                // starting a drag from anywhere in an eventCanvas would position the top of
                // the event to the mouse - now the event will move relative to where you started dragging
                startDateRow = gr.getEventRow(mouseY - props._startOffsetY),
                snapTop = isTL ? rowTop : Math.min(props._maxTop, gr.getRowTop(startDateRow)),
                oldHeight = this.getVisibleHeight(),
                newHeight = oldHeight
            ;


            var delta = newMouseDate.getTime() - props._lastMouseDate.getTime(),
                multiplier = delta < 0 ? -1 : delta == 0 ? 0 : 1,
                snapGapDelta = Math.floor(Math.abs(delta) / view.getTimePerSnapGap("ms"))
            ;

            var dropStart = cal.addSnapGapsToDate(props._lastStartDate, view, snapGapDelta * multiplier);
            var dropEnd = cal.addSnapGapsToDate(props._lastEndDate, view, snapGapDelta * multiplier);

            var drawStartDate = dropStart.duplicate();
            var drawEndDate = dropEnd.duplicate();

            if (isTL) {
                if (dropStart.getTime() < view.startDate.getTime()) {
                    // actual start date is before the start of the visible timeline - when
                    // drawing the drag target, extend it to the far left of the view
                    drawStartDate = view.startDate.duplicate();
                }
                if (dropEnd.getTime() > view.endDate.getTime()) {
                    // actual end date is after the end date of the visible timeline - when
                    // drawing the drag target, extend it to the far right of the view
                    drawEndDate = view.endDate.duplicate();
                }
            }

            // left/width -related
            var eventCol = Math.min(props._maxCol, gr.getEventColumn()),
                columnLeft = gr.getColumnLeft(eventCol),
                offsetX = (gr.getOffsetX() - props._startOffsetX),
                tempLeft = Math.max(0, offsetX - ((offsetX - columnLeft) % eventSnapPixels) + 1),
                date = view.getDateFromPoint(tempLeft, snapTop, null, true),
                eventLeft = Math.min(props._maxLeft,
                    (isTL ? cal.getDateLeftOffset(date, view) :
                                columnLeft)),
                eventRight = eventLeft + (isTL ? (props._startWidth)
                        : canvas.getVisibleWidth())
            ;

            if (!isTL) {
                if (eventRow != props._currentRow) {
                    // rowNum has changed
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

            var sizeToLane = view.isTimelineView() ? (fixedTop >= 0 && fixedHeight >= 0) :
                    (props._useLanes ? (fixedLeft >= 0 && fixedWidth >= 0) : false)
            if (!sizeToLane) {
                props._currentRow = eventRow;
            }

            if (eventCol != props._currentCol) {
                if (view.isDayView() || view.isWeekView()) {
                    if (view.usesLanes() && !cal.canEditEventLane(event, view)) {
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

            var tempTop = Math.max(0, (fixedTop >= 0 ? fixedTop : snapTop)),
                tempBottom = Math.min(view.body.getScrollHeight(), tempTop + props._startHeight)
            ;

            // make sure the temp position is within the grid-body
            if (tempTop < 0) {
                tempTop = 0;
                tempBottom = props._startHeight;
            } else if (tempBottom >= view.body.getScrollHeight()) {
                tempBottom = view.body.getScrollHeight();
                tempTop = tempBottom - props._startHeight;
            }

            if (!view.isTimelineView()) {

                dropStart = view.getDateFromPoint(eventLeft+1, tempTop+1);

                dropEnd = view.getDateFromPoint(eventRight - 1, tempBottom);
                drawStartDate = dropStart.duplicate();
                drawEndDate = dropEnd.duplicate();
            }

            if (view.isDayView() || view.isWeekView()) {
                // for vertical views, check if the dropEnd date is different - this indicates
                // a drop at the very bottom of the calendar - use the end of the dropStart
                // day instead
                if (dropStart.getDate() != dropEnd.getDate()) {
                    //dropEnd = isc.DateUtil.getEndOf(dropStart, "d");
                }
            }



            var testEndDate = dropEnd.duplicate();
            testEndDate.setTime(dropEnd.getTime()-1);

            var allowDrop = true;

            // fire the cancellable notification method before actually moving the dragTarget
            var newEvent = cal.createEventObject(event, dropStart, testEndDate,
                    mouseLane && mouseLane.name,
                    mouseSublane && mouseSublane.name)
            ;
            // the default implementation of this method checks disabled dates
            allowDrop = cal.eventRepositionMove(event, newEvent, this);



            if (sizeToLane) {
                if (isTL) {
                    tempTop = fixedTop;
                    props._previousHeight = fixedHeight;

                    // recalc eventLeft and width from the calculated drawStart/EndDate
                    eventLeft = view.getDateLeftOffset(drawStartDate);
                    // TODO: changed to RightOffset - revisit for an event which has just been created - shows 1 snap too wide, but dates are correct
                    eventRight = view.getDateRightOffset(drawEndDate);
                    props._startWidth = eventRight - eventLeft;

                    this.resizeTo(props._startWidth, fixedHeight);
                } else {
                    eventLeft = fixedLeft;
                    props._previousWidth = fixedWidth;
                    this.resizeTo(fixedWidth, null);
                }
                props._lastSublane = mouseSublane;
                props._lastLane = mouseLane;
            } else{
                if (tempTop + newHeight > view.body.getScrollHeight()-1) {
                    newHeight = view.body.getScrollHeight() - 1 - tempTop;
                }
                props._previousHeight = newHeight;
                this.resizeTo(null, newHeight);
            }

            props._previousTop = tempTop;
            props._previousLeft = eventLeft;

            //isc.logWarn("last start/end dates:\n" +
            //    props._lastStartDate + " / " + props._lastEndDate + "\n" +
            //    "new start/end dates:\n" +
            //    dropStart + " / " + dropEnd + "\n"
            //);

            props._lastStartDate = dropStart.duplicate();
            props._lastEndDate = dropEnd.duplicate();

            // store the mouse date - this is used when dragRepositioning to calculate the new
            // start and end dates in order to deal with events that extend beyond the
            // accessible timeline
            props._lastMouseDate = newMouseDate.duplicate();

            if (allowDrop) {
                props._lastValidStartDate = dropStart.duplicate();
                props._lastValidEndDate = dropEnd.duplicate();
                this.setDragCursor("default");
            } else {
                this.setDragCursor("not-allowed");
            }

            this.moveTo(props._previousLeft, props._previousTop);

            if (view.shouldShowDragHovers()) isc.Hover.show(this.getHoverHTML(), this.hoverProps);

            return isc.EH.STOP_BUBBLING;
        },
        dragRepositionStop : function () {

            if (!this.eventCanvas) return isc.EH.STOP_BUBBLING;

            var canvas = this.eventCanvas,
                props = canvas._dragProps,
                cal = canvas.calendar,
                view = this.view,
                gr = view.body,
                event = canvas.event
            ;

            // hide the Hover and the manual dragTarget before calling the cancellable
            // eventRepositionStop() event below
            if (view.shouldShowDragHovers()) isc.Hover.hide();
            this.hide();

            // cancel the drop if the cursor is "not-allowed" and there's no previous
            // valid drop location
            var cancelDrop = (this.cursor == "not-allowed" && cal.eventUseLastValidDropDates != true);

            // reset the cursor in case we changed it during a drag
            this.setDragCursor("default");

            // bail if cancelDrop is true
            if (cancelDrop) return;

            var isIndicator = canvas.isIndicatorCanvas;

            var canEditLane = props._useLanes && cal.canEditEventLane(event, view),
                canEditSublane = props._useLanes && cal.canEditEventSublane(event, view),
                newLane,
                newSublane
            ;

            if (!isIndicator) {
                if (view.isTimelineView()) {
                    if (canEditLane || canEditSublane) {
                        if (canEditLane) newLane = props._lastLane.name;
                        if (canEditSublane && cal.useSublanes && props._lastSublane) {
                            newSublane = props._lastSublane.name;
                        }
                    }
                } else if (view.usesLanes()) {
                    if (canEditLane || canEditSublane) {
                        if (canEditLane) newLane = props._lastLane.name;
                        if (canEditSublane && cal.useSublanes && props._lastSublane) {
                            newSublane = props._lastSublane.name;
                        }
                    } else return false;
                }

                // the props object has _lastValidStartDate - which, for a drag-move, will be on
                // a snapGap - and _lastValidEndDate, which represents the _lastValidStartDate
                // plus the original length of the event, and might not be on a snapGap
                var actualEndDate = new Date(props._lastValidEndDate.getTime());
                var dates = [ props._lastValidStartDate.duplicate(), actualEndDate ];

                // minsDiff = difference in minutes between new start date and old start date
                var deltaMillis = dates[0].getTime() - cal.getEventStartDate(event).getTime(),
                    minsDiff = Math.floor(deltaMillis / (1000 * 60)),
                    otherFields = {}
                ;
                if (view.isTimelineView()) {
                    // adjust leading and trailing dates by minsDiff amount of minutes.
                    if (event[cal.leadingDateField] && event[cal.trailingDateField]) {
                        dates.add(event[cal.leadingDateField].duplicate());
                        dates[2].setMinutes(dates[2].getMinutes() + minsDiff);
                        dates.add(event[cal.trailingDateField].duplicate());
                        dates[3].setMinutes(dates[3].getMinutes() + minsDiff);
                        otherFields[cal.leadingDateField] = dates[2];
                        otherFields[cal.trailingDateField] = dates[3];
                    }
                }

                if (newLane == null) newLane = event[cal.laneNameField];

                // step 2 adjust initial drop dates, via overridden method
                if (cal.adjustEventTimes) {
                    var adjustedTimes = cal.adjustEventTimes(event, canvas, dates[0], dates[1], newLane);
                    if (adjustedTimes) {
                        dates[0] = adjustedTimes[0].duplicate();
                        dates[1] = adjustedTimes[1].duplicate();
                    }
                }

                // step 3 adjust modified drop dates so no overlapping occurs
                if (cal.allowEventOverlap == false) {
                    var repositionedDates = cal.checkForOverlap(view, canvas, event, dates[0], dates[1], newLane);

                    //TODO: this code is still timeline specific
                    if (repositionedDates == true) {
                        // event overlaps in such a way that dropping anywhere near this location would
                        // be impossible
                        if (cal.timelineEventOverlap) {
                            cal.timelineEventOverlap(false, event, canvas, dates[0], dates[1], newLane);
                        }
                        return false;
                    } else if (isc.isAn.Array(repositionedDates)){
                       dates[0] = repositionedDates[0].duplicate();
                       dates[1] = repositionedDates[1].duplicate();
                       if (cal.timelineEventOverlap) {
                           cal.timelineEventOverlap(true, event, canvas, dates[0], dates[1], newLane);
                       }

                    }
                    // otherwise don't do anything, as no overlap occurred
                }

                // don't update the end date on duration events
                if (cal.isZeroLengthEvent(event)) dates[1] = null;

                // if an overlap-resulting drop was disallowed, the dates may have changed - update
                // the stored drag props as necessary
                if (dates[0] != props._lastValidStartDate) props._lastValidStartDate = dates[0];
                if (dates[1] != props._lastValidEndDate) props._lastValidEndDate = dates[1];
            } else {
                props._lastValidEndDate = props._lastValidStartDate;
            }

            var tempEvent = isc.addProperties({}, event, otherFields);

            // build the new event as it would be after the drop
            var newEvent = cal.createEventObject(tempEvent, props._lastValidStartDate, props._lastValidEndDate,
                    props._lastLane && props._lastLane.name,
                    props._lastSublane && props._lastSublane.name);

            if (cal.getEventStartDate(tempEvent).getTime() == cal.getEventStartDate(newEvent).getTime()) {
                if (!view.usesLanes() || tempEvent[cal.laneNameField] == newEvent[cal.laneNameField]) {
                    // if the event was dropped exactly where it started, just ignore the drag
                    return;
                }
            }


            var continueUpdate = cal.eventRepositionStop(event, newEvent, otherFields, this);

            this._repositioning = false;

            if (continueUpdate != false) {
                if (!isIndicator) {
                    // fire the separate moved variants, which are deprecated
                    if (view.isTimelineView()) {
                        // step 4 fire timelineEventMoved notification to allow drop cancellation
                        if (cal.timelineEventMoved(event, props._lastValidStartDate, props._lastValidEndDate,
                                newLane) == false) return false;
                    } else {
                        // step 4 fire eventMoved notification to allow drop cancellation
                        if (cal.eventMoved(props._lastValidStartDate, event, newLane) == false) return false;
                    }

                    // finally update event
                    //isc.logWarn('updating event:' + [event[cal.startDateField], event[cal.endDateField]]);
                    cal.updateCalendarEvent(event, newEvent, otherFields);
                } else {
                    var indicator = cal.indicators.find(cal.nameField, event[cal.nameField]);
                    indicator[cal.startDateField] = props._lastValidStartDate;
                    // no need to refresh all events - if the indicator is dragged out of the
                    // viewport, the grid will scroll and that already refreshes events - here,
                    // just the indicators and zones need redrawing, which is much quicker -
                    // order is important - see calendar.showIndicatorsInFront
                    canvas.calendarView.drawIndicators();
                    canvas.calendarView.drawZones();
                }
            }

            delete canvas._dragProps;

            //return false;
            return isc.EH.STOP_BUBBLING;
        },


        handleDragResizeStart : function () {
            var canvas = this.eventCanvas,
                event = canvas.event,
                cal = canvas.calendar,
                view = this.view,
                gr = view.body
            ;

            if (!cal.canResizeEvent(canvas.event, view)) return false;

            this._resizing = true;

            var eventRow = gr.getEventRow(),
                rowTop = gr.getRowTop(eventRow),
                rowHeight = gr.getRowHeight(view.getRecord(eventRow), eventRow),
                eventCol = gr.getEventColumn(),
                colLeft = gr.getColumnLeft(eventCol),
                colWidth = gr.getColumnWidth(eventCol),
                offsetX = gr.getOffsetX() - canvas.getLeft(), // - this.getEventPadding(),
                offsetY = gr.getOffsetY() - canvas.getTop(),
                eventWidth = canvas.getVisibleWidth(),
                usesLanes = view.usesLanes(),
                isTimeline = view.isTimelineView(),
                // leftDrag if its a timeline and offsetX is nearer left than right
                isLeftDrag = isTimeline && (offsetX < eventWidth / 2),
                lane = usesLanes ? view.getLaneFromPoint() : null,
                sublane = lane && cal.useSublanes ? cal.getSublaneFromPoint() : null
            ;

            var props;
            if (isTimeline) {
                props = {
                    _startRow: eventRow,
                    _startCol: eventCol,
                    _useLanes: view.usesLanes(),
                    _useSublanes: cal.useSublanes,
                    _previousLeft: view.getDateLeftOffset(cal.getEventStartDate(event)),
                    _previousRight: canvas.getLeft() + eventWidth,
                    _previousTop: rowTop + (sublane ? sublane.top : 0),
                    _previousHeight: sublane ? sublane.height : lane.height,
                    _previousWidth: canvas.getVisibleWidth(),
                    _leftDrag: isLeftDrag,
                    _rightDrag: !isLeftDrag,
                    _lastStartDate: cal.getEventStartDate(canvas.event),
                    _lastEndDate: cal.getEventEndDate(canvas.event),
                    _lastLane: lane,
                    _lastSublane: sublane
                };
            } else {
                // vertical views
                props = {
                    _startRow: eventRow,
                    _startCol: eventCol,
                    _useLanes: view.usesLanes(),
                    _useSublanes: cal.useSublanes,
                    _previousLeft: colLeft + (usesLanes && sublane ? sublane.left : 0),
                    _previousRight: canvas.getLeft() + eventWidth,
                    _previousTop: canvas.getTop(),
                    _previousHeight: canvas.getVisibleHeight(),
                    _previousWidth: (sublane ? sublane.width :
                        (lane && view.getLaneWidth ? view.getLaneWidth(event[cal.laneNameField])
                        : colWidth)
                    ),
                    _bottomDrag: true,
                _lastStartDate: cal.getEventStartDate(canvas.event),
                _lastEndDate: cal.getEventEndDate(canvas.event),
                _lastLane: lane,
                _lastSublane: sublane
            };
            }

            if (props._previousTop == -1) {
                //TODO: fix this - event partly off the top of the viewport shows at top:0
                // this is to do with keepInParentRect, of course
                props._previousTop = 0;
                props._previousHeight -= gr.getScrollTop();
            }

            canvas._dragProps = props;
            this.positionToEventCanvas(true);

            props._invalidDrop = false;

            return isc.EH.STOP_BUBBLING;
        },

        dragResizeMove : function () {
            var canvas = this.eventCanvas,
                props = canvas._dragProps,
                event = canvas.event,
                cal = canvas.calendar,
                view = this.view,
                top = props._previousTop,
                left = props._previousLeft,
                height = props._previousHeight,
                width = props._previousWidth,
                startDate = props._lastStartDate,
                endDate = props._lastEndDate,
                utils = isc.DateUtil
            ;
            var mouseSnap = view.getSnapData(null, null, null, true, null, props._bottomDrag);
            var snapDate;
            if (props._bottomDrag) {
                snapDate = view.getDateFromPoint();
                // day/week view bottom drag - snapDate is new endDate, only height changes -
                // its more natural to use the snapDate AFTER (below) the mouse offset when
                // bottom-dragging, so the drag rect includes the snapDate that's actually
                // under the mouse
                endDate = cal.addSnapGapsToDate(snapDate, view, 1);
                var lStart = isc.DateUtil.getLogicalDateOnly(startDate);
                var lEnd = isc.DateUtil.getLogicalDateOnly(endDate);
                // start and end are in different logical days
                if (lStart.getDate() != lEnd.getDate()) {
                    // endDate is the end of the logical start-date (in the current locale)
                    endDate = isc.DateUtil.getEndOf(lStart, "D", false);
                }
                var bottom = view.getDateTopOffset(endDate);
                height = bottom - top;
            } else if (props._leftDrag) {
                // timeline left drag - snapDate is new startDate, only left and width change
                snapDate = mouseSnap && mouseSnap.startDate.duplicate();
                if (!snapDate) snapDate = view.startDate.duplicate();
                startDate = snapDate;
                var right = left + width;
                if (event[cal.durationField] != null) {
                    var millis = endDate.getTime() - startDate.getTime(),
                        timeUnit = event[cal.durationUnitField],
                        unitMillis = utils.getTimeUnitMilliseconds(timeUnit)
                    ;
                    if (millis % unitMillis != 0) {
                        var units = Math.round(utils.convertPeriodUnit(millis, "ms", timeUnit)),
                        startDate = utils.dateAdd(endDate, timeUnit, units * -1,
                            null, null, null, this.firstDayOfWeek);
                    }
                }
                var vStartSnap = view.getSnapData(null, null, startDate);
                var left = vStartSnap.startLeftOffset;
                width = (right - left);
            } else {
                // timeline right drag - endDate is the endDate of the snapGap under the mouse
                snapDate = mouseSnap && mouseSnap.endDate.duplicate();
                if (!snapDate) snapDate = view.endDate.duplicate();

                endDate = snapDate.duplicate();
                var visibleEnd = cal.getVisibleEndDate(view);
                if (endDate.getTime() > visibleEnd.getTime()) {
                    endDate.setTime(visibleEnd.getTime());
                }
                if (event[cal.durationField] != null) {
                    var millis = endDate.getTime() - startDate.getTime(),
                        timeUnit = event[cal.durationUnitField],
                        unitMillis = utils.getTimeUnitMilliseconds(timeUnit)
                    ;
                    if (millis % unitMillis != 0) {
                        var units = Math.round(utils.convertPeriodUnit(millis, "ms", timeUnit)),
                        endDate = utils.dateAdd(startDate, timeUnit, units,
                            null, null, null, this.firstDayOfWeek);
                    }
                }
                // size to the start of the startSnap and end of the endSnap
                var vStartSnap = view.getSnapData(null, null, startDate);
                var vEndSnap = view.getSnapData(null, null, endDate);
                // if the event starts before the timeline range, clamp left at zero
                var left = vStartSnap ? vStartSnap.startLeftOffset : 0,
                    right = vEndSnap.endLeftOffset
                ;
                width = right-left;
            }

            if (endDate.getTime() <= startDate.getTime() || width <= 0 || height <= 0) {
                // invalid endDate, earlier than start date - just disallow - should leave the
                // default minimum size (the eventSnapPixels)
                return isc.EH.STOP_BUBBLING;
            }


            // call eventResizeMove
            var newEvent = cal.createEventObject(event, startDate, endDate);
            var allowResize = cal.eventResizeMove(event, newEvent, view, props);


            props._lastStartDate = startDate;
            props._lastEndDate = endDate;
            props._previousTop = top;
            props._previousLeft = left;
            props._previousWidth = width;
            props._previousHeight = height;

            this.setRect(props._previousLeft, props._previousTop,
                props._previousWidth, props._previousHeight);

            if (allowResize != false) {
                props._invalidDrop = false;
                props._lastValidStartDate = startDate.duplicate();
                props._lastValidEndDate = endDate.duplicate();
                this.setDragCursor("default");
            } else {
                props._invalidDrop = true;
                this.setDragCursor("not-allowed");
            }

            if (view.shouldShowDragHovers()) isc.Hover.show(this.getHoverHTML(), this.hoverProps);

            return isc.EH.STOP_BUBBLING;
        },

        setDragCursor : function (newCursor) {
            var cursor = this.getCurrentCursor();
            if (cursor == newCursor) return;
            this.setCursor(newCursor);
            this.eventCanvas.setCursor(newCursor);
            this.view.setCursor(newCursor);
            if (this.view.body) this.view.body.setCursor(newCursor);
            if (this.view.frozenBody) this.view.frozenBody.setCursor(newCursor);
            if (isc.EH.lastEvent && isc.EH.lastEvent.target && isc.EH.lastEvent.target.setCursor) {
                isc.EH.lastEvent.target.setCursor(newCursor);
            }
        },

        // eventWindow_dragResizeStop
        dragResizeStop : function () {
            var canvas = this.eventCanvas,
                props = canvas._dragProps,
                cal = canvas.calendar,
                view = this.view,
                event = canvas.event,
                startDate = props._lastValidStartDate,
                endDate = props._lastValidEndDate
            ;
            if (props._invalidDrop && !cal.eventUseLastValidDropDates) {

                startDate = null;
                endDate = null;
            }

            //this.logWarn("LastValid start/end: " + startDate + " -- " + endDate);

            // reset the cursor in case we changed it during a drag
            this.setDragCursor("default");

            // hide the dragHover, if there was one, and the manual dragTarget
            if (view.shouldShowDragHovers()) isc.Hover.hide();
            this.hide();

            if ((props._leftDrag && !startDate) || (props._rightDrag && !endDate)) {
                // if left-dragging and no valid startDate, or right-dragging and no
                // valid endDate, bail
                this._resizing = false;
                return isc.EH.STOP_BUBBLING;
            }

            // build the new event as it would be after the drop
            var newEvent = cal.createEventObject(event, startDate, endDate);
            if (event[cal.durationField] != null) {
                // the event is a duration - force the new length of the event to the nearest
                // durationUnit, so there aren't fractional durations
                var millis = endDate.getTime() - startDate.getTime();
                var roundedDuration = Math.round(
                        isc.DateUtil.convertPeriodUnit(millis, "ms", event[cal.durationUnitField])
                );
                // update the duration
                newEvent[cal.durationField] = roundedDuration;
                // recalc the end date, based on the new duration
                endDate = props._lastValidEndDate = cal.getEventEndDate(newEvent);
                newEvent[cal.endDateField] = endDate;
            }

            if (cal.getEventStartDate(event).getTime() == cal.getEventStartDate(newEvent).getTime()
                && cal.getEventEndDate(event).getTime() == cal.getEventEndDate(newEvent).getTime())
            {
                // if newEvent has the same dates as the current event, just ignore the drag
                return;
            }

            if (view.isTimelineView()) {
                // update leading and trailing dates after a drag-resize
                if (props._leftDrag && event[cal.leadingDateField]) {
                    var leadingDate = event[cal.leadingDateField].duplicate();
                    // get difference in minutes
                    var diff = isc.DateUtil.getPeriodLength(
                            event[cal.startDateField], newEvent[cal.startDateField], "mn");
                    leadingDate.setMinutes(leadingDate.getMinutes()+diff);
                    newEvent[cal.leadingDateField] = leadingDate;
                }
                if (!props._leftDrag && event[cal.trailingDateField]) {
                    var trailingDate = event[cal.trailingDateField].duplicate();
                    // get difference in minutes
                    var diff = isc.DateUtil.getPeriodLength(
                            event[cal.endDateField], newEvent[cal.endDateField], "mn");
                    trailingDate.setMinutes(trailingDate.getMinutes()+diff);
                    newEvent[cal.trailingDateField] = trailingDate;
                }
            }


            var continueUpdate = cal.eventResizeStop(event, newEvent, null, this);

            if (continueUpdate != false) {
                // Added undoc'd endDate param - is necessary for Timeline items because they can be
                // stretched or shrunk from either end
                if (view.isTimelineView()) {
                    // step 4 fire timelineEventMoved notification to allow drop cancellation
                    if (cal.timelineEventResized(event, startDate, endDate) == false) return false;
                } else {
                    // step 4 fire eventMoved notification to allow drop cancellation
                    if (cal.eventResized(endDate, event) == false) return false;
                }

                //isc.logWarn('dragResizeStop:' + [startDate, endDate]);
                cal.updateCalendarEvent(event, newEvent);
            }

            this._resizing = false;
            //delete canvas._dragProps;
            return isc.EH.STOP_BUBBLING;
        }
    },

    scrolled : function () {
        if (this.shouldShowColumnLayouts()) return;
        if (this.renderEventsOnDemand && this.refreshVisibleEvents) {
            delete this._cache.viewportStartMillis;
            delete this._cache.viewportEndMillis;
            this.delayedRefreshVisibleEvents(null, "scrolled");
        }
    },

    resized : function (deltaX, deltaY, reason ) {
        // update the cellHeight to fill the current viewport height
        if (this.calendar.showWorkday  && !this.isMonthView()) {
            this.cacheWorkdayRowHeight();
            this.markForRedraw();
        }

        var result;

        //var result = this.Super('resized', arguments);

        if (this.renderEventsOnDemand) {
            if (!this._updatingRecordComponents && this.isDrawn()) {
                //for (var key in this._usedCanvasMap) {
                //    this._allCanvasMap[key].needsResize = true;
                //}
                if (this.shouldShowColumnLayouts()) {
                    this.updateColumnLayouts();
                } else {
                    //this.delayedRefreshVisibleEvents(null, "resized - " + reason);
                    this.refreshVisibleEvents(null, "resized - " + reason);
                }
                //this.logInfo("Calendar " + this.viewName + " view resized - reason is '" +
                //    (reason || "no reason") + " -- w/h is " + this.body.getVisibleWidth() + "/" +
                //    this.body.getVisibleHeight(), "calendar"
                //);
            }
        } else if (this.shouldShowColumnLayouts()) {
            this.updateColumnLayouts();
        }
        return result;
    },

    forceDataSort : function (data, ignoreDataChanged) {
        var cal = this.calendar,
            specifiers = []
        ;

        if (this.usesLanes()) {
            specifiers.add({ property: cal.laneNameField, direction: "ascending" });
        }

        if (cal.overlapSortSpecifiers) {
            specifiers.addList(cal.overlapSortSpecifiers);
        } else {
            specifiers.add({ property: cal.startDateField, direction: "ascending" });
        }

        if (ignoreDataChanged || !data) {
            if (!data) data = this.getEventData();
            //cal._ignoreDataChanged = true;
        }

        data.setSort(specifiers);
    },

    findEventsInRange : function (startDate, endDate, lane, data) {
        var cal = this.calendar,
            range = {},
            useLane = lane != null && this.usesLanes()
        ;
        range[cal.startDateField] = startDate;
        range[cal.endDateField] = endDate;
        if (useLane) range[cal.laneNameField] = lane;

        var events = this.findOverlappingEvents(range, range, [range], useLane, data, true);
        return events;
    },

    // realEvent is the actual event object, passed in so that we can exclude
    // it from the overlap tests. paramEvent is an object with date fields  - the third param
    // is an array of events to ignore
    findOverlappingEvents : function (realEvent, paramEvent, excludeThese, useLanes, data, ignoreDataChanged) {
        var cal = this.calendar,
            dataPassed = data != null
        ;

        var events = dataPassed ? data : this.getEventData();

        if (!dataPassed) this.forceDataSort(events, ignoreDataChanged);

        var results = [],
            length = events.getLength(),
            paramStart = cal.getEventStartDate(paramEvent),
            paramEnd = cal.getEventEndDate(paramEvent)
        ;

        var rangeObj = {};

        var lane = useLanes ? realEvent[cal.laneNameField] : null,
            startIndex = 0;

        if (lane) startIndex = events.findIndex(cal.laneNameField, lane);
        if (startIndex < 0) return results;


        var isTimeline = this.isTimelineView();

        for (var i = startIndex; i < length; i++) {
            var event = events.get(i);
            if (!event) {
                isc.logWarn('findOverlappingEvents: potentially invalid index: ' + i);
                break;
            }

            if (useLanes && event[cal.laneNameField] != lane) break;

            var excluded = false;
            if (excludeThese && excludeThese.length > 0) {
                for (var j=0; j<excludeThese.length; j++) {
                    if (cal.eventsAreSame(event, excludeThese[j])) {
                        excluded = true;
                        break;
                    }
            }
                if (excluded) continue;
            }

            if (isTimeline) {
                // if we're not showing lead-trail lines use start-endDate fields instead to
                // determine overlap
                if (event[cal.leadingDateField] && event[cal.trailingDateField]) {
                    rangeObj[cal.leadingDateField] = paramEvent[cal.leadingDateField];
                    rangeObj[cal.trailingDateField] = paramEvent[cal.trailingDateField];

                    if (rangeObj[cal.trailingDateField] && this.endDate) {
                        if (rangeObj[cal.trailingDateField].getTime() > this.endDate.getTime()) {
                            rangeObj[cal.trailingDateField].setTime(this.endDate.getTime()-1)
                        }
                    }
                } else {
                    rangeObj[cal.startDateField] = paramStart;
                    rangeObj[cal.endDateField] = paramEnd;
                    if (rangeObj[cal.endDateField].getTime() > this.endDate.getTime()) {
                        rangeObj[cal.endDateField].setTime(this.endDate.getTime()-1)
                    }
                }
            } else {
                var dayStart = isc.DateUtil.getStartOf(paramEnd, "d", false).getTime(),
                    dayEnd = isc.DateUtil.getEndOf(paramStart, "d", false).getTime(),
                    eStartDate = cal.getEventStartDate(event),
                    eStart = eStartDate.getTime(),
                    eEndDate = cal.getEventEndDate(event),
                    eEnd = eEndDate.getTime()
                ;
                // if the event ends before or starts after the range, continue
                if (eStart > dayEnd || eEnd < dayStart) continue;
                // for the weekView, if an event starts before AND ends after the range, there's
                // no sensible column to draw it in - so ignore it for now...
                if (eStart < dayStart && eEnd > dayEnd) {
                    if (this.isWeekView()) continue;
                    eStart = dayStart;

                }
                rangeObj[cal.startDateField] = paramStart;
                if (rangeObj[cal.startDateField].getTime() < dayStart) {
                    // the event starts on another day, and we don't support multi-day events -
                    // clamp the start of the range to the start of the day
                    rangeObj[cal.startDateField].setTime(dayStart)
                }
                rangeObj[cal.endDateField] = paramEnd;
                if (rangeObj[cal.endDateField].getTime() > dayEnd) {
                    // the event ends on another day, and we don't support multi-day events -
                    // clamp the end of the range to the end of the day
                    rangeObj[cal.endDateField].setTime(dayEnd)
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
            aCache = a["_" + this.viewName] || {},
            b = event,
            bCache = b["_" + this.viewName] || {},
            cal = this.calendar,
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
        var aStart =  a[startField], aEnd = a[endField] || cal.getEventEndDate(a);

        if (!aStart || !aEnd) return false;

        var aLeft = aStart.duplicate(), aRight = aEnd.duplicate(),
            bStart = b[startField], bEnd = b[endField] || cal.getEventEndDate(b),
            bLeft = bStart.duplicate(), bRight = bEnd.duplicate()
        ;

        if (this.isTimelineView()) {
            // if the event isn't accessible in the view, return false
            if (bStart.getTime() > this.endDate.getTime()) return false;
            if (bEnd.getTime() < this.startDate.getTime()) return false;

            // first test the dates themselves
            if (bLeft.getTime() < aRight.getTime() && bRight.getTime() > aLeft.getTime()) return true;


            if (bLeft.getTime() != aRight.getTime() && bRight.getTime() != aLeft.getTime()) {

                var minWidth = Math.round(cal.getSnapGapPixels(this));
                aLeft = aCache.snapStartLeftOffset || this.getDateLeftOffset(aStart);
                aRight = Math.max((aCache.snapEndLeftOffset || this.getDateRightOffset(aEnd)),
                            aLeft);
                if (aRight-aLeft < minWidth) aRight = aLeft + minWidth;
                bLeft = bCache.snapStartLeftOffset || this.getDateLeftOffset(bStart);
                bRight = Math.max((bCache.snapEndLeftOffset || this.getDateRightOffset(bEnd)),
                            bLeft);
                if (bRight-bLeft < minWidth) bRight = bLeft + minWidth;
            }
        }
        if (cal.equalDatesOverlap && cal.allowEventOverlap) {
            if ((aLeft < bLeft && aRight >= bRight && aLeft <= bRight) // overlaps to the left
                || (aLeft <= bRight && aRight > bRight) // overlaps to the right
                || (aLeft <= bLeft && aRight >= bRight) // overlaps entirely
                || (aLeft >= bLeft && aRight <= bRight) // is overlapped entirely
            ) {
                return true;
            } else {
                return false;
            }
        } else {
            // b is event, a is range
            if (bLeft < aRight && bRight > aLeft) return true;
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
        var cal = this.calendar,
            rawData = passedData || this.getEventData(),
            ranges = this.overlapRanges || [],
            //ranges = [],
            dataLen = rawData.getLength(),
            isTimeline = this.isTimelineView(),
            // should we only detect overlaps by date if the events are in the same lane?
            useLanes = this.usesLanes(),
            // events on different days can currently only overlap if on the same date
            splitDates = !isTimeline,
            // the list of overlap ranges that were actually affected by the process, so the
            // ranges that need to be re-tagged
            touchedRanges = [],
            minDate = this.startDate,
            maxDate = this.endDate
        ;

        var data = (isc.isA.ResultSet(rawData) ? rawData.allRows : rawData);

        data.setProperty("tagged", false);
        data.setProperty("overlapProps", null);
        data.setProperty("slotNum", null);

        data.setSort([
            { property: cal.laneNameField, direction: "ascending" },
            { property: cal.startDateField, direction: "ascending" },
            { property: cal.endDateField, direction: "descending" }
        ]);

        for (var i=0; i<dataLen; i++) {
            var event = data.get(i);
            var eRange = { events: [event] };
            eRange[cal.startDateField] = cal.getEventStartDate(event);
            // clamp range-start
            if (eRange[cal.startDateField] < minDate) eRange[cal.startDateField] = minDate;

            eRange[cal.endDateField] = cal.getEventEndDate(event);
            // clamp range-end
            if (eRange[cal.endDateField] > maxDate) eRange[cal.endDateField] = isc.DateUtil.adjustDate(maxDate, "-1ms");
            eRange[cal.laneNameField] = eRange.lane = useLanes ? event[cal.laneNameField] : null;

            var addRange = true;

            for (var j=0; j<ranges.length; j++) {
                // event not for this range's lane
                if (eRange[cal.laneNameField] != ranges[j][cal.laneNameField]) continue;
                else {
                    // ends before or starts after the range
                    if (eRange[cal.endDateField].getDate() < ranges[j][cal.startDateField].getDate() ||
                        eRange[cal.startDateField].getDate() > ranges[j][cal.endDateField].getDate()) {
                        continue;
                    }
                }
                if (this.eventsOverlap(eRange, ranges[j], useLanes)) {
                    // merge the two ranges - the dates of the existing range are altered to
                    // fully incorporate both ranges and events are copied over
                    this.mergeOverlapRanges(eRange, ranges[j]);
                        addRange = false;
                    }
                    if (!addRange) break;
                }
            if (addRange) {
                ranges.add(eRange);
                if (!touchedRanges.contains(eRange)) touchedRanges.add(eRange);
            }
        }

        for (i=0; i<ranges.length; i++) {
            var range = ranges[i];
            // set an overlapRangeId on the range - include the lane if set
            range.id = "range_" + i + "_" + this.viewName;
            if (range.lane) range.id += "_lane_" + range.lane;

            range.events.setProperty("overlapRangeId", range.id);

            // get the field for the range's logical date
            var field = this.getFieldFromDate(range[cal.startDateField], range[cal.laneNameField]);

            if (field) {
                // set the fieldName and colNum on each range - used in various methods like tagDayEvents(date)
                range.fieldName = field.name;
                range.colNum = field.masterIndex;
                if (field.endDate && field.endDate <= range.endDate) {
                    range.endDate = field.endDate.duplicate();
                }
            }
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
            cal = this.calendar,
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

        var cal = this.calendar,
            b = toRange,
            start, end
        ;

        for (var i=0; i<fromRanges.length; i++) {
            var a = fromRanges[i];
            if (a[cal.leadingDateField] && b[cal.leadingDateField]) {
                start = cal.leadingDateField;
                end = cal.trailingDateField;
            } else {
                start = cal.startDateField;
                end = cal.endDateField;
            }
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
        return this.getLaneIndex(event[this.calendar.laneNameField]);
    },
    getEventLane : function (event) {
        return this.getLane(event[this.calendar.laneNameField]);
    },
    hasOverlapRanges : function () {
        // are there any overlap ranges?  should always be if there are any visible events in the range
        return this.overlapRanges != null && this.overlapRanges.length > 0;
    },
    getLaneOverlapRanges : function (laneName) {
        // return a list of the overlapRanges that exist for the passed lane
        if (!this.hasOverlapRanges()) return;
        var cal = this.calendar,
            ranges = [];
        this.overlapRanges.map(function (range) {
            if (range[cal.laneNameField] == laneName) ranges.add(range);
        });
        return ranges;
    },
    getDayOverlapRanges : function (date) {
        // return a list of the overlapRanges that exist for the passed date (column)
        if (!this.hasOverlapRanges()) return;
        var field = this.getFieldNameFromDate(date);
        if (field) return this.getColOverlapRanges(field);
    },
    getColOverlapRanges : function (colNum) {
        // return a list of the overlapRanges that exist for the passed column (lane or date)
        if (!this.hasOverlapRanges()) return;

        var ranges = [];
        if (isc.isAn.String(colNum)) {
            // colNum is a field-name
            ranges = this.overlapRanges.findAll("fieldName", colNum);
        } else {
            ranges = this.overlapRanges.findAll("colNum", colNum);
        }
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
        return this.overlapRanges.find("id", event.overlapRangeId);
    },
    getDateOverlapRange : function (date, lane) {
        // get the single overlap range, if any, that contains the passed date
        if (!this.hasOverlapRanges()) return;
        var cal = this.calendar,
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
        if (!this.usesLanes()) return;

        var laneIndex = this.getLaneIndex(laneName);
        if (this.isTimelineView()) {
            this.retagRowEvents(laneIndex, true);
        } else {
            this.retagColumnEvents(laneIndex, true);
        }
    },

    // recalculate the overlap ranges in a given day (one vertical column, or multiple
    // vertical lanes, if in dayView and showDayLanes is true
    retagDayEvents : function (date) {
        if (this.isTimelineView()) return;

        var field = this.getFieldFromDate(date);
        if (field) this.retagColumnEvents(field, false);
    },

    // recalculate the overlap ranges in a given column - might be a "day" or a vertical lane
    retagColumnEvents : function (colNum, isLane) {
        if (this.isTimelineView()) return;

        //this.logWarn("*** retagColumnEvents(" + colNum + ")");

        var field;
        if (isc.isA.Number(colNum)) {
            field = this.body.getField(colNum);
        } else {
            field = colNum;
            colNum = this.body.getFieldNum(field);
        }

        // 1) remove the ranges that appear in this column
        this.removeOverlapRanges(this.getColOverlapRanges(field.name));

        // 2) get a list of events that will be in this column
        var date = field.startDate;
        if (!date) return;
        var startDate = date,
            endDate = this.getCellEndDate(this.data.length-1, colNum)
        ;
        var events = this.findEventsInRange(startDate, endDate, (isLane ? field.name : null));

        // 3) re-tag and render those events
        this.renderEvents(events, isLane);
    },

    // recalculate the overlap ranges in a given row - only applicable to timelines
    retagRowEvents : function (rowNum) {
        if (!this.isTimelineView()) return;

        //this.logWarn("*** retagRowEvents(" + rowNum + ")");

        var cal = this.calendar,
            row;
        if (isc.isA.Number(rowNum)) {
            row = this.getRecord(rowNum);
        } else {
            row = rowNum;
            rowNum = this.isGrouped ? this.getGroupedRecordIndex() : this.getRecordIndex(row);
        }

        var laneName = row.name;

        // 1) remove the ranges that appear in this lane
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
        var cal = this.calendar,
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
            //this.renderEvents(range.events, (lane != null));
            this.renderEvents(events, (lane != null));
        } else {
            // 2) get the list of events that are in the (merged range's) date range and lane
            var events = this.findEventsInRange(start, end, lane, this.getEventData()); //cal.data);

            // 3) re-tag and render those events
            this.renderEvents(events, (lane != null));
        }
    },

    sortForRender : function (events) {

        var cal = this.calendar,
            specifiers = [];
        if (this.usesLanes()) {
            specifiers.add({ property: cal.laneNameField, direction: "ascending" });
        }
        if (cal.overlapSortSpecifiers) {
            specifiers.addList(cal.overlapSortSpecifiers);
        } else {
            if (!cal.showColumnLayouts) {
                // only sort by slotNum if overlapping is in use (not showing columnLayouts)
                specifiers.add({ property: "slotNum", direction: "ascending" });
            }
            specifiers.add({ property: cal.startDateField, direction: "ascending" });
        }
        events.setSort(specifiers);
    },
    renderEvents : function (events, isLane) {
        if (!events || events.length == 0) return;

        // tag the data - this causes sorting and building of overlapRanges for all of the
        // passed events
        this.tagDataForOverlap(events, isLane);
        // sort the affected events to make zOrdering happen from left to right
        this.sortForRender(events);
        var cal = this.calendar,
            isTimeline = this.isTimelineView(),
            visibleLanes = isLane ? (isTimeline ? this.body.getVisibleRows() : this.body.getVisibleColumns()) : [],
            _this = this;
        for (var i=0; i<events.length; i++) {
            var event = events.get(i),
                props = event.overlapProps,
                laneIndex = isLane ? _this.getLaneIndex(event[cal.laneNameField]) : null
            ;
            if (!isLane || (laneIndex >= visibleLanes[0] && laneIndex <= visibleLanes[1])) {
                // size the eventCanvas for each passed event
                var canvas = this.getCurrentEventCanvas(event);
                if (canvas) {
                    if (canvas.setEvent) canvas.setEvent(event);
                    else canvas.event = event;
                    _this.sizeEventCanvas(canvas, false, "renderEvents");
                } else {
                    canvas = this.addEvent(event);
                }
            }
        };
    },

    //------------------------------------------------------
    // range building and rendering stuff
    //------------------------------------------------------

    sizeEventCanvas : function (canvas, forceRedraw, calledFrom) {
        if (!canvas || !canvas.event || Array.isLoading(canvas.event)) return;

        if (this.shouldShowColumnLayouts()) {
            this.logInfo("sizeEventCanvas() called from " + calledFrom + " while " +
                "showColumnLayouts is true...");
            return;
        }

        var cal = this.calendar;
        if (cal == null) return;

        var event = canvas.event,
            isTimeline = this.isTimelineView(),
            isWeekView = this.isWeekView(),
            useLanes = this.usesLanes(),
            startDate = cal.getEventStartDate(event),
            endDate = cal.getEventEndDate(event)
        ;

        //this.logWarn("in sizeEventCanvas - " + isc.echoFull(canvas.getRect()));


        var hasRedrawFocus = canvas.hasFocus;
        // we have to reset the focus but WHERE DOES IT GET DRAWN???
        //if (forceRedraw) {
            //canvas.hide();
        //}

        var eTop, eLeft, eWidth, eHeight,
            laneIndex = useLanes ? this.getLaneIndex(event[cal.laneNameField]) : null,
            lane = useLanes ? this.getLane(event[cal.laneNameField]) : null,
            padding = cal.getLanePadding(this);
        ;

        if (isTimeline) {
            if (!lane) return;
            eHeight = this.getLaneHeight(lane);

            // calculate event width by the offsets of the start and end dates
            eWidth = Math.round(this._getEventBreadth(event));

            // minWidth is one snapGap, or zeroLengthEventWidth for zero-length duration events
            var minWidth = Math.round(cal.getSnapGapPixels(this));
            if (cal.isDurationEvent(event) && cal.getEventDuration(event) == 0) {
                minWidth = cal.zeroLengthEventSize + (padding * 2);
            }
            eWidth = Math.max(eWidth, minWidth);

            // calculate event left
            eLeft = this.getDateLeftOffset(startDate);

            eTop = this.getRowTop(laneIndex);

            if (padding > 0) {
                eTop += padding;
                eLeft += padding;
                eWidth -= (padding * 2);
                eHeight -= (padding * 2);
                // if the minWidth > sum of padding, reduce the minWidth
                if (minWidth > (padding * 2)) minWidth -= (padding * 2);
            }

            if (cal.eventsOverlapGridLines) {

                //if (eLeft > 0) {
                    eLeft -= 1;
                    eWidth += 1;
                //}
                    eTop -= 1;
                    eHeight += 1;
                }

            if (this.eventDragGap > 0) {
                eWidth = Math.max(this.eventDragGap, eWidth - this.eventDragGap);
            }

            eWidth = Math.max(eWidth, minWidth);
        } else {

            if (canvas._cacheValues) {
                delete canvas._cacheValues._innerHTML;
                delete canvas._cacheValues._headerHeight;
            }

            // create a new logical date, without converting anything
            var newStart = isc.DateUtil.createLogicalDate(
                    startDate.getFullYear(), startDate.getMonth(), startDate.getDate()
            );

            var colNum;
            if (this.isDayView()) {
                if (this.usesLanes()) colNum = laneIndex;
                else colNum = 0;
            } else {
                colNum = this.getColFromDate(newStart);
            }

            var colDate = this.getCellDate(0, colNum);
            eLeft = this.body.getColumnLeft(colNum);
            eWidth = this.body.getColumnWidth(colNum);

            var rowSize = this.body.getRowHeight(this.getRecord(1), 1),
                // if the event ends on the next day, render it as ending on the last hour of the
                // current day
                spansDays = false,
                minsPerRow = this.getTimePerCell(),
                rowsPerHour = cal.getRowsPerHour(this),
                eHrs = null
            ;
            if (colDate && startDate.getTime() < colDate.getTime()) {
                newStart = colDate.duplicate();
                spansDays = true;
            }

            // detect different days with logical-dates
            var newEnd = isc.DateUtil.getLogicalDateOnly(endDate);
            var sameDay = isc.DateUtil.compareLogicalDates(newStart, newEnd) == 0;
            if (!sameDay) {
                newEnd = isc.DateUtil.getEndOf(newStart, "d", false);
                spansDays = true;
                eHrs = 24;
            }

            // use logicalTimes to cater for positioning with a custom display timezone
            var startTime = isc.DateUtil.getLogicalTimeOnly(startDate);
            var endTime = isc.DateUtil.getLogicalTimeOnly(endDate);

            if (eHrs == null) {
                // if endHours < startHours in the current displayTimezone, clamp to midnight
                // so we don't render off the bottom of vertical calendar-views
                if (endTime.getHours() < startTime.getHours()) eHrs = 24;
                // catch the case where the end of the event is on 12am, which happens when an
                // event is dragged or resized to the bottom of the screen
                else eHrs = endTime.getHours() == 0 && endTime.getMinutes() == 0 && sameDay ? 24 : endTime.getHours();
            }

            // use getRowTop() to get the top of the rows containing the start and end times
            eTop = this.getRowTop(startTime.getHours() * rowsPerHour);
            var eBottom = this.getRowTop(eHrs * rowsPerHour);

            eHeight = eBottom - eTop;

            eHeight -= 1;

            if (cal.showDayLanes) {
                if (padding > 0) {
                    eTop += padding;
                    eLeft += padding;
                    eWidth -= (padding * 2);
                    eHeight -= (padding * 2);
                }
            }

            var startMins = startTime.getMinutes();
            if (startMins > 0) {
                var startMinPixels = cal.getMinutePixels(startMins, rowSize, this);
                eHeight -= startMinPixels;
                eTop += startMinPixels;
            }

            if (endTime.getMinutes() > 0 && !spansDays) {
                eHeight += cal.getMinutePixels(endTime.getMinutes(), rowSize, this);
            }

            if (cal.eventsOverlapGridLines) {
                eLeft -= 1;
                eWidth += 1;
                eTop -= 1;
                eHeight += 1;
            }

            var scrollH = this.body.getScrollHeight();
            if (eTop+eHeight > scrollH) {
                eHeight = scrollH - eTop;
            }

        }

        var rectChanged = true;
        if (cal.useSublanes && lane && lane.sublanes) {
            this.sizeEventCanvasToSublane(canvas, lane, eLeft, eTop, eWidth, eHeight);
        } else {
            //if (doDebug) isc.logWarn('sizeEventCanvas:' + [daysFromStart, cal.startDate]);
            rectChanged = this.adjustDimensionsForOverlap(canvas, eLeft, eTop, eWidth, eHeight);
        }

        // update the canvas
        if (!canvas.isDrawn()) {
            canvas.draw();
        } else canvas.redraw();
        canvas.show();

        if (isTimeline && event != null) {
            // draw leading and trailing lines
            if (event[cal.leadingDateField] && event[cal.trailingDateField]) {
                if (canvas.leadingIcon) canvas.updateLeadingTrailingDates();
                // split this onto another thread so that ie doesn't pop the
                // slow script warning. Applies to first draw only.
                else canvas.delayCall("updateLeadingTrailingDates");
            }
        }

        if (hasRedrawFocus && !canvas.hasFocus) {
            canvas.focus("restore calendar event focus after redraw");
        }

        delete canvas.needsResize;
    },

    /*
    getRowTop : function () {
        var result = this.Super("getRowTop", arguments);
        return result;
    },
    */

    adjustDimensionsForOverlap : function (canvas, left, top, width, height) {
        var cal = this.calendar,
            props = canvas.event.overlapProps
        ;
        if (!props) {
            //this.logWarn(canvas.event.name + " - no overlapProps");
            return;
       }
        var isTimeline = this.isTimelineView(),
            usePadding = this.useLanePadding(),
            padding = usePadding ? cal.getLanePadding(this) : 0,
            halfPadding = usePadding ? Math.floor(padding / 2) : 0,
            totalPadding = usePadding && props ? (props.totalSlots-1) * padding : 0
        ;

        if (props.slotNum == null) {
            props.slotNum = 1;
        }

        //isc.logWarn('adjustDimForOverlap:' + canvas.event.EVENT_ID + this.echoFull(props));
        //props = false;
        if (props && props.totalSlots > 0) {
            var slotSize;
            if (isTimeline) {

                slotSize = Math.floor((height-totalPadding) / props.totalSlots);
                height = slotSize;
                if (props.slotCount) {
                    height *= props.slotCount;
                    height += (props.slotCount-1) * padding;
                }
                if (props.totalSlots != 1) {
                    if (props.slotNum == props.totalSlots) height -= halfPadding;
                }
                top = top + Math.floor((slotSize * (props.slotNum - 1)));
                if (props.slotNum > 1) top += (padding * (props.slotNum-1));
                if (cal.eventOverlap && props._drawOverlap != false) {
                    if (props.slotNum > 1) {
                        top -= Math.floor(slotSize * (cal.eventOverlapPercent / 100));
                        height += Math.floor(slotSize * (cal.eventOverlapPercent / 100));
                    }
                }
            } else {
                slotSize = Math.floor((width-totalPadding) / props.totalSlots);
                width = slotSize;
                if (props.slotCount) {
                    width *= props.slotCount;
                    width += (props.slotCount-1) * padding;
                }
                if (props.totalSlots != 1) {
                    if (props.slotNum == props.totalSlots) width -= halfPadding;
                }
                left = left + Math.floor((slotSize * (props.slotNum - 1)));
                if (!cal.eventOverlap && props.slotNum > 1) left += (padding * (props.slotNum-1));
                if (cal.eventOverlap && props._drawOverlap != false) {
                    if (props.slotNum > 1) {
                        left -= Math.floor(slotSize * (cal.eventOverlapPercent / 100));
                        width += Math.floor(slotSize * (cal.eventOverlapPercent / 100));
                    }
                }
                // remove some width for the eventDragGap - do this after all the other
                // manipulation to avoid percentage calculations returning different values
                var lastSlot = !props ? true :
                    (props.slotNum == props.totalSlots ||
                    (props.slotNum + props.slotCount) - 1
                        == props.totalSlots)
                ;
                if (lastSlot) {
                    // leave an eventDragGap to the right of right-aligned events to allow
                    // drag-creation of overlapping events
                    width -= cal.eventDragGap || 1;
                }
            }
        } else {
            if (isTimeline) {
            } else {
                // leave an eventDragGap to the right of right-aligned events to allow
                // drag-creation of overlapping events
                width -= cal.eventDragGap || 1;
            }
        }
        // add a pixel of height to all overlapped events so that their borders are flush
        if (cal.eventsOverlapGridLines) {
            if (isTimeline) {
                if (props && props.totalSlots > 1) height += 1
            } else {
                height += 1;
                if (props && props.slotNum > 0 && !cal.eventOverlap) {
                    width += 1;
                }
            }
        }

        var rect = canvas.getRect();
        if (rect[0] == left && rect[1] == top && rect[2] == width && rect[3] == height) {
            // if position and size haven't changed, bail before an unnecessary renderEvent(),
            // returning false to prevent an unnecessary markForRedraw()
            this.logDebug("Not resizing event '" + canvas.event.name + "'.");
            return false;
        }
        canvas.renderEvent(top, left, width, height);
        return true;
    },

    sizeEventCanvasToSublane : function (canvas, lane, left, top, width, height) {
        var cal = this.calendar,
            event = canvas.event,
            sublanes = lane.sublanes,
            sublaneIndex = sublanes.findIndex("name", event[this.calendar.sublaneNameField]),
            isTimeline = this.isTimelineView(),
            len = sublanes.length,
            padding = cal.getLanePadding(this),
            offset = 0
        ;

        // bail if no sublane (shouldn't happen)
        if (sublaneIndex < 0) return;

        for (var i=0; i<=sublaneIndex; i++) {
            if (i == sublaneIndex) {
                if (isTimeline) {
                    top += offset;
                    height = sublanes[i].height - padding;
                } else {
                    left += offset;
                    width = sublanes[i].width - padding;
                    if (left + width + 1 < this.body.getScrollWidth()) width += 1;
                    if (top + height + 1 < this.body.getScrollHeight()) height += 1;
                }
                break;
            }
            if (isTimeline) offset += sublanes[i].height;
            else offset += sublanes[i].width;
        }
        //canvas.padding = padding;
        if (sublaneIndex > 0 && padding > 0) {
            if (isTimeline) height -= Math.floor(padding / sublanes.length);
            else width -= Math.floor(padding / sublanes.length);
        }

        //if (cal.eventsOverlapGridLines) {
        //    if (overlapProps.totalSlots > 1) height += 1
        //}

        canvas.renderEvent(top, left, width, height);
    },

    getOverlapSlot : function (index, snapCount) {
        var slot = { slotNum: index, events: [], snapGaps: [] };
        for (var i=0; i<snapCount; i++) slot.snapGaps[i] = 0;
        return slot;
    },
    getSnapData : function (x, y, date, returnExtents, lane, isEndSnap) {
        var cal = this.calendar,
            snapMins = this.getTimePerSnapGap("mn"),
            snapPixels = this.getSnapGapPixels(0),
            snap = {}
        ;

        if (date == null) {
            // use the date at the passed co-ords (or the mouse)
            date = this.getDateFromPoint(x, y);
        }


        if (date != null) {
            var d = (isc.isA.Date(date) ? date.duplicate() : new Date(date));
            if (returnExtents) {
                // clamp the date if it's outside of the current range
                if (d.getTime() < this.startDate.getTime()) {
                    d = this.startDate.duplicate();
                } else if (d.getTime() > this.endDate.getTime()) {
                    d = this.endDate.duplicate();
                }
            }
            // start at the snap before (date + 1ms is between snaps) - but don't do this if
            // d == view.endDate, because that puts the date outside of the range by a ms,
            // immediately after the code above deals with that situation
            if (!isEndSnap && d.getTime() < this.endDate.getTime()) d.setTime(d.getTime()+1);
            snap.startTopOffset = this.getDateTopOffset(d, lane);

            // end at the snap after (start + snapMins)
            var end = isc.DateUtil.dateAdd(d, "mn", snapMins);
            if (d.getDate() == end.getDate()) {
                snap.endTopOffset = this.getDateTopOffset(end, lane);
            } else {
                snap.endTopOffset = this.body.getScrollHeight();
            }
            // index is (startTopOffset / snapMins)
            snap.index = Math.floor(snap.startTopOffset / snapPixels);
            // startDate is getDateFromPoint(null, startTopOffset)
            snap[cal.startDateField] = this.getDateFromPoint(null, snap.startTopOffset);
            // startDate is getDateFromPoint(null, startTopOffset)
            snap[cal.endDateField] = this.getDateFromPoint(null, snap.endTopOffset);
        }

        return snap;
    },

    tagDataForOverlap : function (data, lane) {
        data = data || this.getEventData();
        if (data.getLength() == 0) return;
        var cal = this.calendar,
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

        data.setProperty("overlapProps", null);
        data.setProperty("slotNum", null);

        var useLanes = this.usesLanes();

        var olRanges = this.updateOverlapRanges(data);

        var rangeSort = [];
        if (useLanes) {
            rangeSort.add({ property: cal.laneNameField, direction: "ascending" });
        }
        if (cal.overlapSortSpecifiers) {
            rangeSort.addList(cal.overlapSortSpecifiers);
        } else {

            rangeSort.add({ property: "eventLength", direction: "descending" });
            rangeSort.add({ property: cal.startDateField, direction: "ascending" });
            rangeSort.add({ property: cal.endDateField, direction: "ascending" });
        }

        var addLogs = false;

        if (addLogs) {
            this.logWarn("tagDataForOverlap: about to loop over " + olRanges.length + " overlap ranges");
        }

        var lastSnapIndex = this.getLastSnapIndex();

        for (var j = 0; j<olRanges.length; j++) {
            var range = olRanges[j];

            if (addLogs) {
                this.logWarn("range: " + isc.echoFull(range) + "");
            }

            var field = (range.fieldName ? this.getFieldByName(range.fieldName) : null);

            var rangeStartSnapObj = this.getSnapData(null, null, range[cal.startDateField], true, range[cal.laneNameField]),
                rangeStartSnap = rangeStartSnapObj ? rangeStartSnapObj.index : 0,
                rangeEndSnapObj = this.getSnapData(null, null, range[cal.endDateField], true, range[cal.laneNameField], true),
                rangeEndSnap = rangeEndSnapObj ? rangeEndSnapObj.index : this._snapGapList.length-1,
                // range start and end snaps are inclusive
                rangeSnapCount = (rangeEndSnap-rangeStartSnap) + 1,
                slotList = [],
                slotCount = 1
            ;

            // add an initial slot
            slotList[0] = this.getOverlapSlot(0, rangeSnapCount);

            var events = range.events;

            //events.setSort(rangeSort);

            for (var eventIndex=0; eventIndex<events.length; eventIndex++) {

                var event = events[eventIndex];

                event.overlapProps = {};

                var oProps = event.overlapProps;

                if (addLogs) {
                    this.logWarn("Processing event " + event.name);
                }
                // get the event's snapGapList - last param will return the first/last snaps
                // if the dates are out of range
                var eStart = cal.getEventStartDate(event),
                    eEnd = cal.getEventEndDate(event)
                ;

                var eStartDate = isc.DateUtil.getLogicalDateOnly(eStart);
                var eStartTime = isc.DateUtil.getLogicalTimeOnly(eStart);
                eStart = isc.DateUtil.combineLogicalDateAndTime(eStartDate, eStartTime);

                var eEndDate = isc.DateUtil.getLogicalDateOnly(eEnd);
                var eEndTime = isc.DateUtil.getLogicalTimeOnly(eEnd);
                eEnd = isc.DateUtil.combineLogicalDateAndTime(eEndDate, eEndTime);

                // take 1ms from the end time to deal with the common case of people specifying
                // a rounded end date, like 10pm, which is typically the start of the next snapGap
                eEnd = isc.DateUtil.adjustDate(eEnd, "-1ms");

                if (field && field.endDate && eEnd > field.endDate) {
                    // clamp the event's endDate to the column's endDate
                    eEnd = field.endDate.duplicate();
                }

                if (addLogs) {
                    this.logWarn("    startDate: " + eStart + "\n      endDate: " + eEnd);
                }

                // tweak the dates by 1ms, to prevent exact matches on a snap-boundary from
                // causing incorrect overlaps
                oProps.eventStartSnap = this.getSnapData(null, null, eStart.getTime()+1, true, event[cal.laneNameField]);
                oProps.eventEndSnap = this.getSnapData(null, null, eEnd.getTime()-1, true, event[cal.laneNameField]);

                // deal with hidden snaps - if eventStart/EndSnap aren't set, use last/nextValidSnap
                var eStartSnap = (oProps.eventStartSnap ? oProps.eventStartSnap.index : oProps.nextValidSnap.index) -rangeStartSnap;
                var eEndSnap = (oProps.eventEndSnap ? oProps.eventEndSnap.index : oProps.lastValidSnap.index) -rangeStartSnap;

                // increment the endSnap to ensure appropriate overlap-detection - the +1 is no longer applied
                // to the array.fill() calls later
                eEndSnap++;

                eEndSnap = Math.min(eEndSnap, lastSnapIndex);

                var found = false;
                var slot = null;

                for (var slotIndex=0; slotIndex<slotCount; slotIndex++) {

                    var gaps = slotList[slotIndex].snapGaps.slice(eStartSnap, eEndSnap);
                    var used = gaps.sum() > 0;
                    if (addLogs) {
                        this.logWarn("    checking slots: " + eStartSnap + " to " + eEndSnap);
                        this.logWarn("    used is: " + used);
                        this.logWarn("    gaps are: " + isc.echoFull(gaps));
                    }
                    if (!used) {
                        found = true;
                        slotList[slotIndex].snapGaps.fill(1, eStartSnap, eEndSnap);
                        slotList[slotIndex].events.add(event);
                        event.overlapProps.slotNum = slotIndex;
                        if (addLogs) {
                            this.logWarn("event " + event.name + " now occupying slot " + slotIndex);
                        }
                        break;
                    }
                }
                if (!found) {
                    // add a new slot
                    slotList[slotCount] = this.getOverlapSlot(slotCount, rangeSnapCount);
                    slotList[slotCount].snapGaps.fill(1, eStartSnap, eEndSnap);
                    slotList[slotCount].events.add(event);
                    event.overlapProps.slotNum = slotCount
                    if (addLogs) {
                        this.logWarn("event " + event.name + " added to new slot index " + slotCount);
                    }
                    slotCount++;
                }

            }

            for (var i=0; i<slotList.length; i++) {
                var slot = slotList[i];
                // for each event in this slot, check all later slots - if one has an event
                // that overlaps this event directly, this event ends in the slot before -
                // decides this event's slotCount
                for (var eIndex=0; eIndex < slot.events.length; eIndex++) {
                    var event = slot.events[eIndex];
                    var oProps = event.overlapProps;

                    // update the totalSlots
                    oProps.totalSlots = slotCount;

                    // get the event snapGaps
                    var eStartSnap = (oProps.eventStartSnap ? oProps.eventStartSnap.index : rangeStartSnap) -rangeStartSnap;
                    var eEndSnap = (oProps.eventEndSnap ? oProps.eventEndSnap.index : rangeStartSnap) -rangeStartSnap;

                    eEndSnap++;

                    var found = false;

                    for (var innerIndex=i+1; innerIndex<slotList.length; innerIndex++) {
                        //var gaps = slotList[innerIndex].snapGaps.slice(eStartSnap, eEndSnap+1);
                        var gaps = slotList[innerIndex].snapGaps.slice(eStartSnap, eEndSnap);
                        var used = gaps.sum() > 0;
                        if (used) {
                            oProps.slotCount = innerIndex - oProps.slotNum;
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        // should span all following slots
                        oProps.slotCount = slotCount - oProps.slotNum;
                    }
                    // we want slotNum to start from 1, for legacy downstream code
                    oProps.slotNum++;
                    event.slotNum = oProps.slotNum;
                }
            }

            range.slotList = slotList;

            if (addLogs) {
                this.logWarn("***** slotList *****\n" + isc.echoFull(slotList));
            }
        }

    },

    getLastSnapIndex : function () {
        var snap = this.getSnapData(null, null, isc.DateUtil.adjustDate(this.endDate, "-1ms"), true);
        if (snap) return snap.index;
    },

    //-------------------------rendering events on demand-----------------------------

    getVisibleDateRange : function (refreshAll) {

        var cal = this.calendar;

        if (refreshAll) {
            return [cal.getVisibleStartDate(this), cal.getVisibleEndDate(this)];

        //} else if (this._cache.viewportStartMillis) {
        //    return [ new Date(this._cache.viewportStartMillis), new Date(this._cache.viewportEndMillis) ]
        }

        if (!this.renderEventsOnDemand) {
                return [this.startDate.duplicate(), this.endDate.duplicate()];
        }

        if (this.isTimelineView()) {
            // TODO: add this code to have the timeline use viewport range, rather than scrollable range
            //var cols = this.getVisibleColumnRange(),
            //    startDate = this.getCellDate(0, cols[0]),
            //    endDate = this.getCellEndDate(0, cols[1])
            //;
            //return [startDate, endDate];
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

        if (endRow < 0 || isNaN(endRow)) endRow = this.data.getLength()-1;
        if (endCol < 0 || isNaN(endCol)) {
            if (this.isTimelineView()) {
                endCol = this._dateFieldCount;
            } else {
                endCol = this.body.fields.length - 1;
            }
        }

        endCol = Math.min(endCol, this.body.fields.length-1);
        endRow = Math.min(endRow, this.data.length-1);

        var startDate = this.getCellDate(startRow, startCol) || this.startDate,
            endDate = (this.getCellEndDate ? this.getCellEndDate(endRow, endCol) :
                this.getCellDate(endRow, endCol)) || this.endDate
        ;

        this._cache.viewportStartMillis = startDate.getTime();
        this._cache.viewportEndMillis = endDate.getTime();

        return [ startDate, endDate ];

    },

    getVisibleRowRange : function () {
        if (!this.renderEventsOnDemand) {
            return [0, this.data.getLength()];
        }
        return this.getVisibleRows();
    },

    getVisibleColumnRange : function () {
        if (!this.renderEventsOnDemand) {
            return [0, this.fields.getLength()];
        }

        return this.body.getVisibleColumns();
    },

    delayedRefreshVisibleEvents : function (refreshAll, caller) {
        if (this.refreshVisibleEventsTimer) isc.Timer.clear(this.refreshVisibleEventsTimer);
        var _this = this;
        this.refreshVisibleEventsTimer = isc.Timer.setTimeout(function () {
            //isc.logWarn("delayedRefreshVisibleEvents('" + caller + "')");
            isc.Timer.clear(_this.refreshVisibleEventsTimer);
            delete _this.refreshVisibleEventsTimer;
            _this.refreshVisibleEvents(refreshAll, caller);
        }, 100);
    },

    // refreshEvents is only called when data changes, etc.
    // refreshVisibleEvents is called whenever the view is scrolled and only draws visible events.
    // see scrolled()
    refreshVisibleEvents : function (refreshAll, caller) {
        //this.logWarn("refreshVisisbleEvents() called by '" + caller + "' - " +
        //    "isDrawn=" + this.isDrawn() + ", body.isDrawn=" + (this.body && this.body.isDrawn()));
        // bail unless both the view and its body are drawn
        if (!this.isDrawn() || !this.body || !this.body.isDrawn()) return;
        // bail if this is a lane-based view but there aren't any lanes (can't render anything)

        if (this._pendingGroupComplete || this._groupOnDraw) {
            // if this flag is set, grouping hasn't finished yet, and we'll run again when it does
            return;
        }

        //isc.logWarn("refreshVisibleEvents - called from " + caller);

        // get visible events (those in the viewport)
        var events = this.getVisibleEvents(refreshAll);

        // need to do this to ensure consistent zordering
        this.sortForRender(events);

        var eventsLen = events.getLength();

        var clearThese = this.useEventCanvasPool ? isc.addProperties({}, this._usedEventMap) : {},
            addThese = {}
        ;

        this.logDebug('refreshing visible events','calendar');

        // build a map of event.__key to event-object
        var eventMap = events.makeIndex("__key");

        // we don't need to resize/redraw *already drawn* canvases just because of a scroll
        var firedFromScroll = (caller == "scrolled");
        for (var key in eventMap) {
            //var event = events.get(i),
            var event = eventMap[key],
                alreadyVisible = this._usedEventMap[key] != null
            ;

            if (alreadyVisible) {
                // if an event is already in the _usedEventMap, remove it from the
                // clearThese object
                delete clearThese[key];
                if (!firedFromScroll && (this.isGrouped || this.useEventCanvasPool)) {
                    // if the view is grouped or using the eventCanvasPool, reposition and
                    // redraw the canvas
                    var canvas = this.getCurrentEventCanvas(event);
                    //if (canvas && canvas.needsResize) this.sizeEventCanvas(canvas);
                    this.sizeEventCanvas(canvas, null, "refreshVisibleEvents");
                }
                continue;
            }

            addThese[event.__key] = event;
        }

        if (this.isGrouped ||
                (this.useEventCanvasPool && this.eventCanvasPoolingMode == "viewport"))
        {
            // we want to clear eventCanvases that are no longer in the viewport if we're using
            // viewport pooling mode, or if the grid is grouped (a group might have closed)
            for (var key in clearThese) {
                var canvas = this._allCanvasMap[clearThese[key]];
                if (canvas) this.clearEventCanvas(canvas);
            }
        }
        clearThese = null;

        if (isc.getKeys(addThese).length > 0) {
            for (var key in addThese) {
                var event = addThese[key];
                this.addEvent(event, false, true);
            }
        }
        addThese = null;

        // if there are columnLayouts, resize and resort them
        if (this.shouldShowColumnLayouts()) this.updateColumnLayouts();

        if (!this.__printing) {
            // redraw any zones and indicators in the background
            this.drawZones();
            this.drawIndicators();
        }

        // update the tab positions of all the visible eventCanvases
        if (this.calendar.canSelectEvents) this.setEventCanvasTabPositions();

        this._eventsRendered();
    },

    _eventsRendered : function () {
        var cal = this.calendar;
        if (this._scrollToEvent) {
            this.scrollToEvent(this._scrollToEvent);
            this._scrollToEvent = null;
        }
        if (cal.eventsRendered && isc.isA.Function(cal.eventsRendered)) {
            cal.eventsRendered();
        }
    },

    // update the tab-indexes of all eventCanvases
    setEventCanvasTabPositions : function () {
        if (!this.calendar.canSelectEvents) return;

        // sort the events as they'll be rendered
        var events = this.getEventData();
        this.forceDataSort(events);

        // loop over the events, get the associated canvas and set it's tabIndex
        for (var j=0; j<events.length; j++) {
            var canvas = this.getCurrentEventCanvas(events[j]);
            if (canvas && canvas.isEventCanvas && canvas.event) {
                // ensure the tabIndex is always horizontal and then vertical within each lane,
                // and then vertical into the next visible lane (not next alphabetically)
                var tabIndex = 0;
                if (this.usesLanes()) {
                    tabIndex = this.getLaneIndex(canvas.event[this.calendar.laneNameField]) * 1000;
                }
                tabIndex += j;
                canvas.setRelativeTabPosition(tabIndex);
            }
        }
    },

    getVisibleEvents : function (refreshAll) {
        var cal = this.calendar;
        if (!this.renderEventsOnDemand) return this.getEventData();

        var isTimeline = this.isTimelineView(),
            dateRange = this.getVisibleDateRange(refreshAll),
            useLanes = this.usesLanes(),
            laneRange = useLanes ?
                (isTimeline ? this.getVisibleRowRange() : this.getVisibleColumnRange()) : null
        ;

        var events = this.getEventData(),
            startMillis = dateRange[0].getTime(),
            endMillis = dateRange[1].getTime(),
            eventsLen = events.getLength(),
            results = [],
            isWeekView = this.isWeekView(),
            openList = this.isGrouped ? this.data.getOpenList() : null
        ;

        for (var i = 0; i < eventsLen; i++) {
            var event = events.get(i);

            if (!event) {
                isc.logWarn('getVisibleEvents: potentially invalid index: ' + i);
                break;
            }

            if (isc.isA.String(event)) return [];

            // if shouldShowEvent() is implemented and returns false, skip the event
            if (cal.shouldShowEvent(event, this) == false) continue;
            if (useLanes && cal.shouldShowLane(this.getLane(event[cal.laneNameField]), this) == false) continue;

            var eventStart = cal.getEventLeadingDate(event) || cal.getEventStartDate(event);
            // bail if there's no startDate
            if (!eventStart) {
                if (event.loadingMarker) {
                    this.logWarn(this.viewName + ".getVisibleEvents() encountered a " +
                        "place-holder for a loading record, rather than a valid record. " +
                        "Can't continue:  " + isc.echoFull(this.getStackTrace()));
                    break;
                }
                this.logWarn(this.viewName + ".getVisibleEvents() - event has no start-date: " + isc.echoFull(event));
                continue;
            }

            var eventEnd = cal.getEventTrailingDate(event) || cal.getEventEndDate(event),
                eventEndMillis = eventEnd.getTime()
            ;

            // event ends before the range start
            if (eventEndMillis <= startMillis) continue;
            // event starts after the range end
            if (eventStart.getTime() >= endMillis) continue;

            if (isWeekView) {
                // if the event's end date is on a different day, assume the end of the start
                // day, so that the range-comparison below works properly
                var lStart = isc.DateUtil.getLogicalDateOnly(eventStart),
                    lEnd =  isc.DateUtil.getLogicalDateOnly(eventEnd)
                ;
                if (lStart.getDate() != lEnd.getDate()) {
                    eventEnd = isc.DateUtil.getEndOf(eventStart, "d", false);
                }
                // the range is from hour-start on the start date, to hour-end on the end date
                // but we don't want events that are vertically not in view, so discard events
                // that end before the viewport start time or start after the viewport end-time
                //if (isc.DateUtil.getLogicalTimeOnly(eventEnd).getHours() <
                //        isc.DateUtil.getLogicalTimeOnly(dateRange[0]).getHours()) continue;
                //if (isc.DateUtil.getLogicalTimeOnly(eventStart).getHours() <
                //        isc.DateUtil.getLogicalTimeOnly(dateRange[1]).getHours()) continue;
            }

            // build a range object to compare against
            var rangeObj = {};

            if (useLanes) {
                if (this.isGrouped) {
                    // if grouped, check that the lane is in the openList
                    var index = openList.findIndex("name", event[cal.laneNameField]);
                    if (index < 0) continue;
                } else {
                    if (refreshAll != true) {
                        var laneIndex = this.getEventLaneIndex(event);
                        // optimization - if the lane isn't in the viewport, continue
                        if (laneIndex == null || laneIndex < laneRange[0] || laneIndex > laneRange[1])
                            continue;
                    }
                }

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

            //sameLaneOnly = useLanes ? !cal.canEditEventLane(event, view) : false;
            //if (this.eventsOverlap(rangeObj, event, sameLaneOnly)) {
            if (this.eventsOverlap(rangeObj, event, useLanes)) {
                results.add(event);
            }
        }

        return results;
    },

    clearEventCanvas : function (eventCanvas, destroy) {
        // clears (and pools or destroys) the passed eventCanvas - also accepts an array of
        // eventCanvas instances
        if (eventCanvas) {
            if (!isc.isAn.Array(eventCanvas)) eventCanvas = [eventCanvas];
            var len = eventCanvas.length;
            while (--len >= 0) {
                var canvas = eventCanvas[len];
                if (canvas.hide) canvas.hide();
                // also clear the canvas so it can no longer affect the size of the body
                if (canvas.clear) canvas.clear();
                delete this._usedCanvasMap[canvas.ID];
                if (canvas.event) {
                    delete this._usedEventMap[canvas.event.__key];
                }
                if (this.useEventCanvasPool && !destroy) {
                    this.poolEventCanvas(canvas);
                } else {
                    canvas.event = null;
                    canvas.destroy();
                    canvas = null;
                }
            }
        }
    },

    clearEvents : function (start, destroy) {
        // hide all the canvases in the _eventCanvasPool
        //if (!this.body || !this.body.children) return;
        if (!start) start = 0;
        //isc.logWarn('clearing events');

        if (destroy == null) destroy = !this.useEventCanvasPool;

        for (var key in this._usedCanvasMap) {
            this.clearEventCanvas(this._allCanvasMap[key], destroy);
        }
    },

    areSame : function (first, second) {
        if (!first || !second) return false;
        var cal = this.calendar;
        if (cal.dataSource) {
            var pks = cal.getEventPKs(), areEqual = true;
            for (var i=0, len=pks.length; i<len; i++) {
                if (first[pks[i]] != second[pks[i]]) {
                    areEqual = false;
                    break;
                }
            }
            return areEqual;
        } else {
            return (first === second);
        }
    },


    getEventCanvasConstructor : function (event) {
        return this.eventCanvasConstructor;
    },

    getCurrentEventCanvas : function (event) {
        var canvasID = this._usedEventMap[event.__key];
        return canvasID != null ? this._allCanvasMap[canvasID] : null;
    },

    poolEventCanvas : function (canvas) {
        if (this.body) {
            var eventKey;
            delete this._usedCanvasMap[canvas.ID];
            if (canvas.event) {
                eventKey = canvas.event.__key;
                delete this._usedEventMap[eventKey];
                this.logInfo("pooling canvas " + canvas.ID + " for event + " + eventKey, "calendar");
                //this.calendar.setEventCanvasID(this, canvas.event, null);
                canvas.event = null;
            }
            canvas.availableForUse = true;
            this._pooledCanvasMap[canvas.ID] = canvas;
            return true;
        } else return false;
    },
    getPooledEventCanvas : function (event) {
        if (!this.body) return;

        var canvasID = null;
        var canvas = null;

        var lastUsedCanvasID = this._lastUsedEventMap[event.__key];
        if (lastUsedCanvasID && this._pooledCanvasMap[lastUsedCanvasID]) {
            // get the canvas last used to render this event, if it's still pooled
            canvasID = lastUsedCanvasID;
            canvas = this._allCanvasMap[canvasID];
            this.logInfo("re-using canvas " + canvasID + " for event + " + event.__key, "calendar");
        } else {
            var pooledKeys = isc.getKeys(this._pooledCanvasMap);
            if (pooledKeys.length > 0) {
                canvas = this._pooledCanvasMap[pooledKeys[0]];
                canvasID = canvas.ID;
                this.logInfo("un-pooling canvas " + canvasID + " for event + " + event.__key, "calendar");
            }
        }

        if (!canvas) return;

        // update the various maps
        this._usedEventMap[event.__key] = canvasID;
        this._lastUsedEventMap[event.__key] = canvasID;
        this._usedCanvasMap[canvasID] = event.__key;
        this._lastUsedCanvasMap[canvasID] = event.__key;
        delete this._pooledCanvasMap[canvasID];

        canvas.availableForUse = false;
        //this.calendar.setEventCanvasID(this, event, canvasID);

        return canvas;
    },

    addEvent : function (event, retag, skipReflow) {
        // if this is a new event, store it's __key for faster access later
        if (!event.__key) this.calendar.getEventKey(event, this);

        // clear any cell selection that has been made
        this.clearSelection();

        this.addEventData(event);

        var cal = this.calendar,
            hideWindow = false
        ;

        // get a canvas for the event - either the current one...
        var canvas = this.getCurrentEventCanvas(event)
        // or a pooled one (which will return the one that last showed this event, if it's
        // available, or another one from the pool otherwise)
        if (!canvas) canvas = this.getPooledEventCanvas(event);
        // or a new one if there's nothing available in the pool
        if (!canvas) canvas = cal._getEventCanvas(event, this);

        //if (canvas.isDrawn()) canvas.hide();
        if (canvas.isDrawn()) canvas.clear();

        // assign the dragTarget, if not stacking event in columns - different drag mechanism
        if (!this.shouldShowColumnLayouts()) {
            canvas.dragTarget = this.eventDragCanvas;
        }
        canvas.setEvent(event);

        if (!this.shouldShowColumnLayouts() && canvas.parentElement != this.body) {
            // add canvases the body, if they're not being added to columnLayouts
            this.body.addChild(canvas, null, false);
        }

        canvas.needsResize = true;

        // add canvas->event and event->canvas map entries
        this._usedCanvasMap[canvas.ID] = event.__key;
        this._lastUsedCanvasMap[canvas.ID] = event.__key;
        this._usedEventMap[event.__key] = canvas.ID;
        this._lastUsedEventMap[event.__key] = canvas.ID;

        canvas._isWeek = this.isWeekView();

        if (this.isDayView() && this.usesLanes()) {
            // don't show the eventCanvas if it's lane isn't visible
            var laneName = event[cal.laneNameField];
            if (!this.getLane(laneName)) hideWindow = true;
        }

        if (!this.shouldShowColumnLayouts()) {
            if (!hideWindow && this.body && this.body.isDrawn()) {
                // if the "retag" param was passed, this is an event that hasn't been rendered
                // before (it comes from processSaveResponse() after an "add" op) - rather than
                // just resizing the window, get a list of overlapRanges that intersect the new
                // event, combine the event-list from each of them and add the new event,
                // remove the existing ranges and then retag the event-list
                if (retag) {
                    if (this.verticalEvents) {
                        this.retagDayEvents(cal.getEventStartDate(event));
                    } else {
                        this.retagOverlapRange(cal.getEventStartDate(event),
                                cal.getEventEndDate(event), event[cal.laneNameField]);
                    }
                } else {
                    // add the eventCanvas to the body before rendering it, so that bringToFront()
                    // behaves as expected
                    this.sizeEventCanvas(canvas, null, "addEvent");
                    canvas.bringToFront();
                }
            }
        } else {
            // when showColumnLayouts is true, we just add eventCanvases into this.getDayLayout(date)
            this.addEventCanvasToColumnLayout(canvas, skipReflow);
        }
    },

    getColumnLayout : function (date) {
        var col = this.getColFromDate(date);
        return this.columnLayouts[col];
    },
    addEventCanvasToColumnLayout : function (eventCanvas, skipReflow) {
        var col = this.getColFromDate(this.calendar.getEventStartDate(eventCanvas.event));
        var layout = this.columnLayouts[col];
        if (layout) {
            layout.addMember(eventCanvas);
            eventCanvas.clear();
            eventCanvas.setHeight(1);
            //eventCanvas.setWidth("100%");
            eventCanvas.setOverflow("visible");
            if (!eventCanvas.isDrawn()) eventCanvas.draw();
            else eventCanvas.redraw();
            eventCanvas.show();
            //eventCanvas.bringToFront();
            if (!skipReflow) layout.resort();
            return true;
        }
        return false;
    },

    removeEvent : function (event) {
        var canvas = this.getCurrentEventCanvas(event);
        if (canvas) {
            this.clearEventCanvas(canvas, !this.useEventCanvasPool);
            this.removeEventData(event);
            return true;
        } else {
            return false;
        }
    },

    clearZones : function () {
        var zones = (this._zoneCanvasList || []);
        zones.callMethod("clear");
        zones.callMethod("deparent");
        zones.callMethod("destroy");
        zones = null;
        this._zoneCanvasList = [];
    },
    drawZones : function () {
        if (this._zoneCanvasList) this.clearZones();
        if (!this.calendar.showZones) return;

        var cal = this.calendar,
            zones = cal.zones || [],
            canvasList = this._zoneCanvasList = []
        ;

        if (this.isGrouped) {
            this.logInfo("Zones are not currently supported in grouped Calendar views.");
            return;
        }
        if (!zones || zones.length <= 0) return;

        //zones.setSort([{property: cal.startDateField, direction: "ascending"}]);
        var rangeZones = [],
            dateRange = this.getVisibleDateRange(),
            startMillis = dateRange[0].getTime(),
            endMillis = dateRange[1].getTime()
        ;

        for (var i=0; i<zones.length; i++) {
            var zone = zones[i];
            if (zone[cal.startDateField].getTime() < endMillis &&
                zone[cal.endDateField].getTime() > startMillis)
            {
                rangeZones.add(zone)
            }
            zone.styleName = cal.getZoneCanvasStyle(zone, this);
        }

        for (var i=0; i<rangeZones.length; i++) {
            var zone = rangeZones[i],
                canvas = cal.getZoneCanvas(zone, this),
                left = this.getDateLeftOffset(zone[cal.startDateField]),
                right = this.getDateLeftOffset(zone[cal.endDateField]),
                // use the sum of the lane-heights, even if that's less than the body height
                height = this.data.getProperty("height").sum()
            ;
            this.body.addChild(canvas, null, false)
            canvas.renderEvent(0, left, right-left, height, true);
            canvasList.add(canvas);
        }
    },

    clearIndicators : function () {
        var indicators = (this._indicatorCanvasList || []);
        indicators.callMethod("clear");
        indicators.callMethod("deparent");
        indicators.callMethod("destroy");
        indicators = null;
        this._indicatorCanvasList = [];
    },
    drawIndicators : function () {
        if (this._indicatorCanvasList) this.clearIndicators();
        if (!this.calendar.showIndicators) return;

        var cal = this.calendar,
            indicators = cal.indicators || [],
            canvasList = this._indicatorCanvasList = []
        ;

        if (this.isGrouped) {
            this.logInfo("Indicators are not currently supported in grouped Calendar views.");
            return;
        }
        if (!indicators || indicators.length <= 0) return;

        //indicators.setSort([{property: cal.startDateField, direction: "ascending"}]);
        var rangeIndicators = [],
            dateRange = this.getVisibleDateRange(),
            startMillis = dateRange[0].getTime(),
            endMillis = dateRange[1].getTime()
        ;

        for (var i=0; i<indicators.length; i++) {
            var indicator = indicators[i];
            // indicators are zero-length duration events - ensure that here
            delete indicator[cal.endDateField];
            indicator.duration = 0;
            indicator.durationUnit = "minute";
            var iMillis = cal.getEventStartDate(indicator).getTime();
            if (iMillis >= startMillis && iMillis < endMillis) {
                // indicator's startDate is in the visible range
                rangeIndicators.add(indicator);
            }
        };

        for (var i=0; i<rangeIndicators.length; i++) {
            var indicator = rangeIndicators[i],
                canvas = cal.getIndicatorCanvas(indicator, this),
                left = this.getDateLeftOffset(indicator[cal.startDateField]),
                // use the sum of the lane-heights, even if that's less than the body height
                height = this.data.getProperty("height").sum()
            ;
            this.body.addChild(canvas, null, false)

            canvas.renderEvent(0, left, cal.zeroLengthEventSize, height, !cal.showIndicatorsInFront);
            canvasList.add(canvas);
        }
    },

    _refreshEvents : function () {
        //this.logWarn("_refreshEvents");
        if (!this.isDrawn()) {
            // if the view isn't drawn yet, mark it for refresh on draw
            this._refreshEventsOnDraw = true;
            return;
        }
        if (this.calendar.shouldIncludeRangeCriteria(this)) {
            this._refreshData();
            return;
        }
        this.refreshEvents("_refreshEvents");
    },

    delayedRefreshEvents : function (caller) {
        if (this.refreshEventsTimer) isc.Timer.clear(this.refreshEventsTimer);
        var _this = this;
        this.refreshEventsTimer = isc.Timer.setTimeout(function () {
            isc.Timer.clear(_this.refreshEventsTimer);
            delete _this.refreshEventsTimer;
            _this.refreshEvents(caller);
        }, 100);
    },

    //> @method calendarView.refreshEvents()
    // Clear, recalculate and redraw the events for the current range, without causing a fetch.
    // @visibility external
    //<
    refreshEvents : function (caller) {
        //this.logWarn("refreshEvents() called by " + caller);
        //this.logWarn("refreshEvents() called by '" + caller + "' - " +
        //    "isDrawn=" + this.isDrawn() + ", body.isDrawn=" + (this.body && this.body.isDrawn()));

        if (this._refreshingEvents) return;

        var cal = this.calendar;
        // bail if the grid hasn't been drawn yet, or hasn't any data yet
        if (!this.body || !this.body.isDrawn() || !cal.hasData()) return;

        this._refreshEventsCalled = true;

        // flag to prevent setLanes() from calling back through this method
        this._refreshingEvents = true;

        // clear any zones and indicators (so they don't prevent the body from shrinking)
        this.clearZones();
        this.clearIndicators();

        // clear event canvases
        this.clearEvents(0, !this.useEventCanvasPool);

        // clear event records
        this.setEventData([]);

        // reset the lists of drawn events and canvases - they're either destroyed or pooled now
        this._usedEventMap = {};

        // update various dates and snap-properties
        if (!this.isTimelineView()) {
            this.initCacheValues();
        }
        var startDate = cal.getVisibleStartDate(this) || this.startDate,
            startMillis = startDate.getTime(),
            endDate = cal.getVisibleEndDate(this) || this.endDate,
            endMillis = endDate.getTime()
        ;

        this.overlapRanges = [];


        var eventsLen = cal.data.getLength();
        var allEvents = cal.data.getRange(0, eventsLen);

        // make a map of all the lanes available on the calendar - events that have some other
        // lane will be ignored in the loop below
        var laneMap;
        if (this.usesLanes()) laneMap = cal.lanes ? cal.lanes.makeIndex("name") : null;
        var events = [];

        // reset _allEventMap
        this._allEventMap = {}

        while (--eventsLen >= 0) {
            var event = allEvents.get(eventsLen);
            if (!event) continue;
            if (!isc.isA.String(event)) {
                // if shouldShowEvent() is implemented and returns false, skip the event
                if (cal.shouldShowEvent(event, this) == false) continue;
                if (laneMap && event[cal.laneNameField] && !laneMap[event[cal.laneNameField]]) {
                    // if the view is lane-based but the event's lane doesn't exist, continue -
                    // otherwise, these events will get tagged for overlapping unnecessarily
                    //this.logWarn(this.viewName + ".refreshEvents() - event has no lane: " + isc.echoFull(event[cal.laneNameField]));
                    continue;
                }
                var eventStartDate = cal.getEventStartDate(event);
                //eventStartDate = isc.Calendar._getAsDisplayDate(eventStartDate);
                if (!eventStartDate) {
                    if (event.loadingMarker) {
                        this.logWarn(this.viewName + ".refreshEvents() encountered a " +
                            "place-holder for a loading record, rather than a valid record. " +
                            "Can't continue:  " + isc.echoFull(this.getStackTrace()));
                        break;
                    }
                    this.logWarn(this.viewName + ".refreshEvents() - event has no start-date: " + isc.echoFull(event));
                    continue;
                }

                var dates = cal.getEventDates(event),
                    eLead = dates[cal.leadingDateField],
                    eStart = dates[cal.startDateField],
                    eEnd = dates[cal.endDateField],
                    eTrail = dates[cal.trailingDateField]
                ;

                // if the event itself (not including any leading/trailing dates) covers any
                // part of the scrollable range, it's available to render
                if ((eStart && eStart.getTime() >= startMillis && eStart.getTime() < endMillis) ||
                    (eEnd && eEnd.getTime() > startMillis && eEnd.getTime() <= endMillis) ||
                    // starts before and ends after the range not valid in vertical views - no multi-day events
                    (!this.verticalEvents && eStart.getTime() <= startMillis && eEnd.getTime() >= endMillis)
                   )
                {

                    // get the event's render-snaps
                    var startSnap = this.getSnapData(null, null, eStart),
                        endSnap = this.getSnapData(null, null, eEnd)
                    ;

                    // if the event starts and ends on hidden dates...
                    if (startSnap && startSnap.startHidden && endSnap && endSnap.endHidden) {
                        // only render it if there are valid (visible) snaps between them
                        if (startSnap.lastValidSnap == endSnap.lastValidSnap &&
                            startSnap.nextValidSnap == endSnap.nextValidSnap)
                        {
                            // start/end snaps are in the same hidden block of snaps - no
                            // sensible way to render this event - skip it
                            continue;
                        }
                    }

                    // store the length of the event, not including lead/trail dates, in ms
                    event.eventLength = (eEnd.getTime() - eStart.getTime());
                    if (event[cal.durationField] != null) {
                        event.isDuration = true;
                        event.isZeroDuration = event[cal.durationField] == 0;
                    }

                    // store the event's key and put it in the _allEventMap (all events)
                    if (!event.__key) {
                        event.__key = cal.getEventKey(event);
                    }
                    this._allEventMap[event.__key] = event;

                    //event[propsName] = props;
                    events.add(event);
                } else {
                    //this.logWarn("Evene '" + event.name + "' skipped - startDate: " + event.startDate + ", endDate: " + event.endDate);
                }
            }
        };

        if (this.usesLanes() && cal.lanes) {
            // update the lanes, according to the events just processed
            var lanes = [];
            var usedLaneNames = events.getProperty(cal.laneNameField).getUniqueItems();
            for (var i=0; i<cal.lanes.length; i++) {
                if (cal.hideUnusedLanes) {
                    // remove any lanes that have no *visible* events after processing
                    // list of lanes used by current event-data
                    if (usedLaneNames.contains(cal.lanes[i].name)) {
                        lanes.add(cal.lanes[i]);
                    }
                } else lanes.add(cal.lanes[i]);
            }

            // updateLanes is true if the two arrays have different lengths or entries
            var updateLanes = lanes.length != this.lanes.length;
            for (var i=0; i<this.lanes.length; i++) {
                if (!lanes[i] || lanes[i].name != this.lanes[i].name) {
                    updateLanes = true;
                    break;
                }
            }
            if (updateLanes) this.updateLanes(lanes, true);
        }

        this.setEventData(events);
        this.tagDataForOverlap();

        // redraw the events
        this.refreshVisibleEvents(null, "refreshEvents");

        // scroll as necessary and clear the flag
        if (this._scrollRowAfterRefresh) {
            this.body.scrollTo(null, this._scrollRowAfterRefresh);
            delete this._scrollRowAfterRefresh;
        }

        // clear the internal refresh flags
        delete this._needsRefresh;
        delete this._refreshingEvents;
    },

    _refreshData : function () {
        var cal = this.calendar;
        //isc.logWarn("nextOrPrev:" + cal.data.willFetchData(cal.getNewCriteria()));
        if (cal.dataSource && isc.ResultSet && isc.isA.ResultSet(cal.data)) {
            cal._ignoreDataChanged = true;
            cal.invalidateCache();
            cal.fetchData(cal.getCriteria());
        } else {
            // force dataChanged hooks to fire so event positions are correctly updated
            cal.dataChanged();
        }
    },

    getFrozenLength : function () {
        if (this.frozenBody && this.frozenBody.fields) return this.frozenBody.fields.length;
        return 0;
    },

    getCellAlign : function (record, rowNum, colNum) {
        if (this.isMonthView()) return;
        var cal = this.calendar,
            field = this.fields[colNum],
            // support Lane.cellAlign - secondary default to field (laneField, headerLevel)
            recordAlign = record ? record.cellAlign : null,
            dateAlign = null
        ;
        if (field) {
            if (field.frozen) {
                // laneField or labelColumn - support field, lane, view
                return field.cellAlign || recordAlign || this.labelColumnAlign;
            }
            var frozenLength = this.getFrozenLength();
            if (cal.getDateCellAlign) {
                var date = this.getCellDate(rowNum, colNum-frozenLength);
                if (date) dateAlign = cal.getDateCellAlign(date, rowNum, colNum-frozenLength, this);
            }
            field = this.body.fields[colNum - frozenLength];
            if (dateAlign || field.cellAlign || recordAlign) {
                return dateAlign || field.cellAlign || recordAlign;
            }
        }
        return this.Super("getCellAlign", arguments);
    },

    _defaultCellVAlign: "center",
    getCellVAlign : function (record, rowNum, colNum) {
        if (this.isMonthView()) return;
        var cal = this.calendar,
            field = this.fields[colNum],
            // support Lane.cellVAlign - secondary default to field (laneField, headerLevel)
            recordVAlign = record ? record.cellVAlign : null,
            dateVAlign = null
        ;
        if (field) {
            if (field.frozen) {
                // laneField or labelColumn - support field, lane, view, default ("center")
                return field.cellVAlign || recordVAlign ||
                        this.labelColumnVAlign || this._defaultCellVAlign;
            }
            var frozenLength = this.getFrozenLength();
            if (cal.getDateCellVAlign) {
                var date = this.getCellDate(rowNum, colNum-frozenLength);
                if (date) dateVAlign = cal.getDateCellVAlign(date, rowNum, colNum-frozenLength, this);
            }
            field = this.body.fields[colNum - frozenLength];
            if (dateVAlign || recordVAlign || field.cellVAlign) {
                return dateVAlign || recordVAlign || field.cellVAlign;
            }
        }
        return recordVAlign || this._defaultCellVAlign;
    },

    getCellValue : function (record, rowNum, colNum) {
        if (!this.calendar.getDateHTML) return this.Super("getCellValue", arguments);
        if (this.isMonthView()) return this.Super("getCellValue", arguments);
        var cal = this.calendar,
            frozenLength = this.getFrozenLength()
        ;
        if (colNum-frozenLength >= 0) {
            var date = this.getCellDate(rowNum, colNum-frozenLength);
            if (date) {
                var dateHTML = cal.getDateHTML(date, rowNum, colNum-frozenLength, this);
                if (dateHTML) return dateHTML;
            }
        }
        return this.Super("getCellValue", arguments);
    },

    destroyEvents : function () {
        if (!this.body || !this.body.children) return;

        // clear zone and indicator canvases
        if (this.clearZones) this.clearZones();
        if (this.clearIndicators) this.clearIndicators();

        var len = this.body.children.length;
        while (--len >= 0) {
            var child = this.body.children[len];
            if (child && !child.destroyed) {
                child.clear();
                child.deparent();
                child.destroy();
            }
            child = null;
        }

        // clear event and canvas -related caches
        this.resetEventCaches();
    },

    destroy : function () {
        if (this.isObserving(this, "_updateFieldWidths")) {
            this.ignore(this, "_updateFieldWidths");
        }
        if (this.removeLocalHandlers) this.removeLocalHandlers();
        this.destroyEvents(true);
        if (this.clearZones) this.clearZones();
        if (this.clearIndicators) this.clearIndicators();
        this.calendar = null;
        this.overlapRanges = null;
        this._snapGapList = null;
        this._cache = null;

        // lane-related caches
        this.lanes = null;
        this.resetLaneCaches();
        // event-related caches
        this.resetEventCaches();
        // EventCanvas-related caches
        this.resetCanvasCaches();

        if (this.cellSelection) {
            if (this.cellSelection.destroy) this.cellSelection.destroy();
            this.cellSelection = null;
        }
        if (this.data) {
            if (this.data.destroy) this.data.destroy();
            this.data = null;
        }
        this.Super("destroy", arguments);
    },

    draw : function () {
        var isMonth = this.isMonthView();
        // see if this.cellHeight needs updating
        if (this.calendar.showWorkday && !isMonth) this.cacheWorkdayRowHeight();
        var result = this.Super("draw", arguments);
        // run the cellHeight check again - it will no-op as necessary
        if (this.calendar.showWorkday && !isMonth) this.cacheWorkdayRowHeight();
        if (this._pendingScrollTo) {
            //isc.logWarn("_pendingScrollTo: " + this._pendingScrollTo);
            // scroll to the extents of the range
            this["scrollTo" + this._pendingScrollTo]();
            delete this._pendingScrollTo;
        }
    },

    //> @method CalendarView.scrollToStart()
    // Move the viewport of this CalendarView to the start of its scrollable range.
    // @visibility calendar
    //<
    scrollToStart : function () {
        if (!this.isDrawn()) {
            // store the requested scroll-type - scroll to it at the end of draw()
            this._pendingScrollTo = "Start";
            return;
        }
        if (this.verticalEvents) {
            this.body.scrollToTop();
        } else {
            this.body.scrollToLeft();
        }
    },

    //> @method CalendarView.scrollToEnd()
    // Move the viewport of this CalendarView to the end of its scrollable range.
    // @visibility calendar
    //<
    scrollToEnd : function () {
        if (!this.isDrawn()) {
            // store the requested scroll-type - scroll to it at the end of draw()
            this._pendingScrollTo = "End";
            return;
        }
        if (this.verticalEvents) {
            this.body.scrollToBottom();
        } else {
            this.body.scrollToRight();
        }
    },

    getNewEventDefaults : function (event) {
        // ensure the passed event has start and end dates on it
        var evt = event || {};
        var cal = this.calendar;
        var sDate = evt[cal.startDateField];
        if (!sDate) {
            // start from the mouse-date, or the current datetime (addEvent button-click)
            sDate = this.getDateFromPoint() || isc.DateUtil.createDatetime();
            if (sDate.getHours() > 22) sDate.setHours(22);
            evt[cal.startDateField] = sDate;
        }
        if (!evt[cal.endDateField]) {
            var eDate = sDate.duplicate();
            eDate.setHours(sDate.getHours() + 1);
            evt[cal.endDateField] = eDate;
        }
        return evt;
    },

    getEditDialogPosition : function (event) {
        var cal = this.calendar,
            startDate = cal.getEventStartDate(event),
            colNum = this.getColFromDate(startDate, event[cal.laneNameField]),
            rowNum
        ;
        if (cal.showColumnLayouts) rowNum = this.body.getEventRow();
        else rowNum = this.body.getEventRow(this.getDateTopOffset(startDate));
        var rect = this.body.getCellPageRect(rowNum, colNum);
        return {
            left: Math.max(rect[0], this.body.getPageLeft()),
            top: Math.max(rect[1], this.body.getPageTop())
        };
    },

    // helper function for detecting when a weekend is clicked, and weekends are disabled
    cellDisabled : function (rowNum, colNum) {
        var body = this.getFieldBody(colNum);
        if (!body || body == this.frozenBody) return false;
        var col = this.getLocalFieldNum(colNum),
            date = this.getCellDate(rowNum, col)
        ;
        return this.calendar.shouldDisableDate(date, this);
    }

});

isc.CalendarView.changeDefaults("bodyDefaults", {
    canFocus:false,
    drawChildWithParent : function (child) {
        // we never want to auto-draw event-canvases with the parent - see code in
        // refreshEvents()/refreshVisibleEvents() for that
        if (child.isEventCanvas) return false;
        return this.Super("drawChildWithParent", arguments);
    }
});

isc.CalendarView.changeDefaults("frozenBodyDefaults", {
    canFocus:false
});

// DaySchedule
// --------------------------------------------------------------------------------------------
isc.ClassFactory.defineClass("DaySchedule", "CalendarView");

isc.DaySchedule.changeDefaults("bodyProperties", {
    //childrenSnapToGrid: true,

    //snapToCells: false,
    //suppressVSnapOffset: true
//  //  redrawOnResize:true
    snapToCells: false,
    suppressVSnapOffset: true,
    suppressHSnapOffset: true,
    childrenSnapToGrid: false,
    scrollTo : function (left, top) {
        var grid = this.creator;
        if (grid.calendar.showWorkday && grid.calendar.limitToWorkday) {
            // prevent scrolling beyond the workday start and end
            if (top < grid._workdayMinTop) top = grid._workdayMinTop;
            else if (top > grid._workdayMaxTop) top = grid._workdayMaxTop;
        }

        return this.Super("scrollTo", arguments);
    }
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
    //labelColumnVAlign: "center",
    labelColumnPosition: "left",
    labelColumnBaseStyle: "labelColumn",

    // leave the scrollbar gap to prevent ongoing resizes
    //leaveScrollbarGap: true,

    // show cell-level rollover
    showRollOver: true,
    useCellRollOvers: true,

    // disable autoFitting content on header double clicking
    canAutoFitFields : false,

    canSelectCells: true,
    navigateOnTab:false,

    // return the string to show in the Calendar controlsBar
    dateLabelFormat: "dddd, MMM dd, YYYY",
    getDateLabelText : function (startDate, endDate) {
        if (this.isWeekView()) {
            return"<b>" + isc.DateUtil.getFormattedDateRangeString(startDate, endDate) + "</b>";
        }
        return "<b>" + isc.DateUtil.format(startDate, this.dateLabelFormat) + "</b>";
    },

    initWidget : function () {
        var cal = this.calendar;

        if (this.isDayView() && this.usesLanes() && cal.alternateLaneStyles) {
            this.alternateFieldStyles = true;
            this.alternateFieldFrequency = cal.alternateFieldFrequency;
        }

        if (cal.labelColumnWidth && cal.labelColumnWidth != this.labelColumnWidth) {
            this.labelColumnWidth = cal.labelColumnWidth;
        }

        this.renderEventsOnDemand = cal.renderEventsOnDemand;
        this.eventDragGap = cal.eventDragGap;

        var fDOW = this.calendar.firstDayOfWeek;
        // set the range dates
        if (this.rangeUnit) {
            this.periodStartDate = this.startDate = isc.DateUtil.getStartOf(this.chosenDate, this.rangeUnit, false, fDOW);
            this.periodEndDate = this.endDate = isc.DateUtil.getEndOf(this.chosenDate, this.rangeUnit, false, fDOW);
        }

        if (!this.logicalDate) {
            // needed for the initial call to _getCellDates(), pre-draw
            this.logicalDate = isc.DateUtil.getLogicalDateOnly(this.chosenDate);
        }

        if (this.usesLanes()) {
            // initialize the local lanes array and a name->object map
            this.lanes = cal.lanes ? cal.lanes.duplicate() : [];
            this.laneMap = this.lanes.makeIndex("name");
        }

        // initialize this.fields (and this.data)
        this.rebuildFields();

        this.Super("initWidget", arguments);

        if (isc.isAn.Array(cal.data)) {
            this._refreshEventsOnDraw = true;
            this._ignoreDataChanged = true;
        }

        this.addAutoChild("eventDragCanvas", { styleName: this.eventDragCanvasStyleName });

    },

    getFirstDateColumn : function () {
        return this.frozenBody ? this.frozenBody.fields.length : 0;
    },

    reorderFields : function (start, end, moveDelta) {
        this.Super("reorderFields", arguments);
        this.refreshEvents("reorderFields");
    },

    _getLabelFieldProperties : function () {
        return {
                autoFitWidth: true,
                minWidth: this.labelColumnWidth,
                width: this.labelColumnWidth,
                name: "label",
                isLabelField: true,
                title: " ",
                cellAlign: "right",
                calendar: this.calendar,
                frozen: this.labelColumnPosition == "left",
                formatCellValue : function (value, record, rowNum, colNum, grid) {
                    var cal = grid.calendar;
                    var time = record.logicalTime;
                    var mins = (time.getHours()*60) + time.getMinutes();
                    if (mins % cal.rowTitleFrequency == 0) {
                        return isc.Time.toShortTime(time, grid.creator.timeFormatter, true);
                    } else {
                        return "";
                    }
                }
        };
    },
    rebuildFields : function () {
        this.initCacheValues();
        var cal = this.calendar,
            fields = [],
            labelColumnOnLeft = this.labelColumnPosition == "left"
        ;

        this.showLabelColumn = cal.showLabelColumn;

        // add the label column at the start, according to settings
        if (this.showLabelColumn && labelColumnOnLeft) {
            fields.add(this._getLabelFieldProperties());
        }

        var lanes = [];
        if (this.usesLanes()) {
            // if the view is using lanes but no lanes were provided, turn the feature off
            //lanes = this.lanes = this.lanes || (cal.lanes && cal.lanes.duplicate()) || [];
            lanes = this.lanes || [];
            if (lanes.length == 0) {
                isc.logWarn("Calendar.showDayLanes has been turned off because no lanes have " +
                    "been provided.");
                this.calendar.showDayLanes = false;
            }
        }
        if (lanes.length > 0) {
            var d = this.startDate.duplicate(),
                scaffolding = isc.DaySchedule._getEventScaffolding(this),
                nDate = isc.DateUtil.createLogicalDate(d.getFullYear(), d.getMonth(), d.getDate()),
                props = { align: "center", canReorder: cal.canReorderLanes, date: nDate,
                    startDate: isc.DateUtil.getStartOf(isc.DateUtil.createDatetime(nDate), "D", false),
                    endDate: isc.DateUtil.getEndOf(isc.DateUtil.createDatetime(nDate), "D", false)
                }
            ;
            for (var i=0; i<lanes.length; i++) {
                var lane = lanes[i],
                    laneName = lane.name,
                    p = isc.addProperties({}, props, { name: laneName })
                ;
                p[cal.laneNameField] = laneName;
                if (lane.sublanes) {
                    // if there are sublanes, work out the left offsets and widths for them
                    // now - if a sublane has a specified width, uses that value - otherwise,
                    // applies a width of (laneWidth / subLane count).
                    var laneWidth = this.getLaneWidth(lane),
                        len = lane.sublanes.length,
                        sublaneWidth = Math.floor(laneWidth / len),
                        offset = 0
                    ;
                    for (var j=0; j<len; j++) {
                        var sublane = lane.sublanes[j];
                        sublane[cal.laneNameField] = sublane.name;
                        sublane.left = offset;
                        if (sublane.width == null) sublane.width = sublaneWidth;
                        offset += sublane.width;
                    }
                    lane.width = lane.sublanes.getProperty("width").sum();
                }
                fields.add(isc.addProperties(p, lane));
            }
            scaffolding.setProperty(laneName, "");
            this.setShowHeader(true);
            if (cal.canReorderLanes) this.canReorderFields = cal.canReorderLanes;
            if (cal.minLaneWidth != null) this.minFieldWidth = cal.minLaneWidth;
            this.data = scaffolding;
        } else {
            var nDate = isc.DateUtil.getLogicalDateOnly(this.startDate);
            var weekendDays = cal.getWeekendDays();
            var dayNames = isc.DateUtil.getShortDayNames();
            var numDays = this.isWeekView() ? 7 : 1;
            if (this.shouldShowColumnLayouts() && !this.columnLayouts) {
                this.createColumnLayouts(numDays);
            }
            for (var i=0; i<numDays; i++) {
                var dateStr = nDate.toShortDate(this.dateFormatter, false);
                if (dateStr.match(this._usDateRegex) != null) dateStr = dateStr.substring(5);
                else if (dateStr.match(this._jpDateRegex)) dateStr = dateStr.substring(0,dateStr.length-5);

                var fieldName = "day" + (i+1);
                var field = {
                    name: fieldName, width: "*", autoFitWidth: false, align: "right",
                    // only potentially hide fields in WeekView, never in dayView which will
                    // skip past weekend days anyway, if showWeekends is false, and it
                    // might also have multiple fields if dayLanes are showing
                    showIf: "return !list.isWeekView() || list.calendar.showWeekends || !field.isWeekend;",
                    isWeekend: weekendDays.contains(nDate.getDay()),
                    title: dayNames[nDate.getDay()] + " " + dateStr,
                    date: isc.DateUtil.getLogicalDateOnly(nDate),
                    startDate: isc.DateUtil.getStartOf(isc.DateUtil.createDatetime(nDate), "D", false),
                    endDate: isc.DateUtil.getEndOf(isc.DateUtil.createDatetime(nDate), "D", false),
                    _dayNum: nDate.getDay(),
                    _dateNum: nDate.getDate(),
                    _monthNum: nDate.getMonth(),
                    _yearNum: nDate.getFullYear()
                };
                if (this.shouldShowColumnLayouts()) {
                    field.eventLayoutID = this.columnLayouts[i].ID;
                }
                fields.add(field);
                nDate.setDate(nDate.getDate() + 1);
            }
            this.setShowHeader(this.isWeekView());
            this.data = isc.DaySchedule._getEventScaffolding(this);
        }

        // add the label column at the end, according to settings
        if (this.showLabelColumn && !labelColumnOnLeft) {
            fields.add(this._getLabelFieldProperties());
        }


        if (this.isDrawn()) this.setFields(fields);
        else this.fields = fields;
    },

    fieldStateChanged : function () {
        if (!this.shouldShowColumnLayouts()) return;
        this.updateColumnLayouts();
    },
    createColumnLayouts : function (numDays) {
        if (!this.shouldShowColumnLayouts()) return;
        this.columnLayouts = [];
        for (var i=0; i<numDays; i++) {
            this.columnLayouts.add(this.calendar.createColumnLayout(this));
        }
        this.columnLayoutMap = this.columnLayouts.makeIndex("ID");
    },
    destroyColumnLayouts : function () {
        if (!this.shouldShowColumnLayouts()) return;
        if (this.columnLayoutMap) delete this.columnLayoutMap;
        if (this.columnLayouts) {
            for (var i=0; i<this.columnLayouts.length; i++) {
                this.columnLayouts[i].markForDestroy();
                this.columnLayouts[i] = null;
            }
            this.columnLayouts = null
        }
    },
    updateColumnLayouts : function () {
        if (!this.shouldShowColumnLayouts()) return;
        if (!this.body || !this.body.isDrawn()) {
            this._updateColumnLayoutsOnDraw = true;
            return;
        }
        if (this.fields) {
            var widths = this.getFieldWidths();
            // resize each field's columnLayout - events are layout-members, so will auto-size
            for (var i=this.fields.length-1; i>=0; i--) {
                var f = this.fields[i];
                if (f.eventLayoutID) {
                    var eventLayout = this.columnLayoutMap[f.eventLayoutID];
                    eventLayout.fieldName = f.name;
                    if (!this.body.contains(eventLayout)) {
                        this.body.addChild(eventLayout);
                    }
                    var cLeft = this.getColumnLeft(f.masterIndex);
                    if (eventLayout.getLeft() != cLeft)
                        eventLayout.setLeft(cLeft);
                    if (eventLayout.getWidth() != widths[i])
                        eventLayout.setWidth(widths[i]);
                    var bHeight = this.body.getScrollHeight();
                    if (eventLayout.getHeight() != bHeight)
                        eventLayout.setHeight(bHeight);
                    if (eventLayout.isDrawn()) eventLayout.redraw();
                    else eventLayout.draw();
                    // make sure sorting is up to date
                    eventLayout.resort();
                    eventLayout.show();
                    eventLayout.bringToFront();
                }
            }
        }
    },

    setFields : function () {
        var result = this.Super("setFields", arguments);
        if (this.shouldShowColumnLayouts()) this.updateColumnLayouts();
        return result;
    },

    updateFieldDates : function () {
        // update the titles and dates on the existing fields, to reflect the current
        // startDate and endDate
        var dayNames = isc.DateUtil.getShortDayNames();
        var weekendDays = this.calendar.getWeekendDays();
        var numDays = this.isWeekView() ? 7 : 1;
        var nDate = this.logicalDate.duplicate()
        for (var i=0; i<numDays; i++) {
            var dateStr = nDate.toShortDate(this.dateFormatter, false);
            if (dateStr.match(this._usDateRegex) != null) dateStr = dateStr.substring(5);
            else if (dateStr.match(this._jpDateRegex)) dateStr = dateStr.substring(0,dateStr.length-5);

            var fieldName = "day" + (i+1);
            var props = {
                title: dayNames[nDate.getDay()] + " " + dateStr,
                date: nDate.duplicate(),
                startDate: isc.DateUtil.adjustDate(nDate, "-0D"),
                endDate: isc.DateUtil.adjustDate(nDate, "+0D"),
                _dayNum: nDate.getDay(),
                _dateNum: nDate.getDate(),
                _monthNum: nDate.getMonth(),
                _yearNum: nDate.getFullYear()
            };
            // apply the new date, date-parts and title to the existing fields
            this.setFieldProperties(fieldName, props);
            nDate.setDate(nDate.getDate() + 1);
        }
        if (this.shouldShowColumnLayouts()) this.updateColumnLayouts();
        if (this.header) this.header.markForRedraw();
    },
    updateRowDates : function () {
        var minsPerRow = this.getTimePerCell(),
            rowCount = (60 / minsPerRow) * 24,
            startDate = this.startDate,
            date = isc.DateUtil.getLogicalDateOnly(startDate)
        ;

        // update the cellDate cache
        isc.DaySchedule._getCellDates(this);

        // recreate the date at midnight in the display timezone
        date = isc.DateUtil.createDatetime(date.getFullYear(), date.getMonth(), date.getDate(),0,0,0,0);

        var logicalDate = this.logicalDate.duplicate(),
            logicalTime = isc.DateUtil.createLogicalTime(0, 0, 0)
        ;

        for (var i=0; i<rowCount; i++) {
            var time = isc.DateUtil.combineLogicalDateAndTime(logicalDate, logicalTime);
            var rec = this.data[i];
            rec.time = time.duplicate();
            rec.logicalDate = logicalDate.duplicate();
            rec.logicalTime = logicalTime.duplicate();
            logicalTime.setMinutes(logicalTime.getMinutes() + minsPerRow);
            //this.logWarn("ROW " + i + " UPDATE: " + isc.echoFull(rec));
        }
    },

    getDateFromPoint : function (x, y, round, useSnapGap) {
        var cal = this.calendar;

        //if (useSnapGap) {
            // when click/drag creating, we want to snap to the eventSnapGap
            //y -= y % cal.getSnapGapPixels();
        //}

        if (x == null && y == null) {
            // if no co-ords passed, assume mouse offsets into the body
            y = this.body.getOffsetY();

            x = this.body.getOffsetX();
        }

        if (y > this.body.getScrollHeight())
            return null;
            //y = this.body.getScrollHeight();

        var rowNum = this.body.getEventRow(y);
        if (rowNum == -1)  rowNum = 0;
        else if (rowNum == -2) rowNum = this.getTotalRows() - 1;

        var rowHeight = this.cellHeight,
            rowTop = this.body.getRowTop(rowNum),
            colNum = this.body.getEventColumn(x),
            badCol = (colNum < 0)
        ;

        if (colNum == -1) colNum = 0;
        else if (colNum == -2) colNum = this.body.fields.length-1;

        // get the date for the top of the cell
        var colDate = this.getCellDate(rowNum, colNum);

        // if getCellDate() returns null, bail
        if (!colDate) return null;

        var minsPerRow = this.getTimePerCell(),
            rowsPerHour = cal.getRowsPerHour(this),
            offsetY = y - rowTop,
            eventSnapPixels = cal.getSnapGapPixels(this),
            pixels = offsetY - (offsetY % eventSnapPixels),
            snapGapMins = Math.round(minsPerRow / (rowHeight / eventSnapPixels)),
            snapGaps = pixels / eventSnapPixels,
            minsToAdd = snapGapMins * snapGaps
        ;

        colDate.setMinutes(colDate.getMinutes() + minsToAdd);

        return colDate;
    },

    getCellDate : function (rowNum, colNum) {
        if (!this.body || !this.body.fields || !this._cellDates || !this.body.fields[colNum]) {
            return null;
        }

        // use the last row if invalid rowNum passed
        if (rowNum < 0) rowNum = this.data.getLength() - 1;

        // return the cell date from the array built by _getCellDates()
        var fieldName = this.isDayView() ? "day1" : this.body.fields[colNum][this.fieldIdProperty];
        if (!fieldName.startsWith("day")) return;
        var obj = this._cellDates[rowNum];

        // if obj[fieldName] isn't set, date cells weren't calculated yet - return null
        if (!obj || !obj[fieldName]) return null;

        var date = isc.DateUtil.combineLogicalDateAndTime(obj[fieldName + "_logicalDate"], obj.logicalTime);

        return date;
    },

    getCellEndDate : function (rowNum, colNum) {
        if (!this.body || !this.body.fields || !this._cellDates || !this.body.fields[colNum]) {
            return null;
        }

        // use the last row if invalid rowNum passed
        if (rowNum < 0) rowNum = this.data.getLength() - 1;

        // return the cell date from the array built by _getCellDates()
        var fieldName = this.isDayView() ? "day1" : this.body.fields[colNum][this.fieldIdProperty];
        if (!fieldName.startsWith("day")) return;
        var obj = this._cellDates[rowNum];

        if (!obj || !obj[fieldName]) return null;

        var date = isc.DateUtil.combineLogicalDateAndTime(obj[fieldName + "_logicalDate"], obj.logicalEndTime);

        return date;
    },

    getEventLeft : function (event) {
        var col = this.getColFromDate(this.calendar.getEventStartDate(event), event[this.calendar.laneNameField]);
        return this.body.getColumnLeft(col);
    },
    getEventRight : function (event) {
        var col = this.getColFromDate(this.calendar.getEventEndDate(event), event[this.calendar.laneNameField]);
        return this.body.getColumnLeft(col) + this.body.getColumnWidth(col);
    },

    // get the left offset of a date in this view - will either be zero (dayView) or the
    // getColumnLeft() of the day column containing the date
    getDateLeftOffset : function (date) {
        var col = this.getColFromDate(date);
        if (col != null) return this.body.getColumnLeft(col);
        return 0;
    },

    // get the top offset of a date in this view - will be the top of the row that contains
    // the date, plus any snapGap heights within the row
    getDateTopOffset : function (date, lane) {
        var cal = this.calendar;
        // get the time in the display timezone
        var time = isc.DateUtil.getLogicalTimeOnly(date);
        var hours = time.getHours()
        var snapCount = Math.round(time.getMinutes() / this.getTimePerSnapGap("mn"));
        var snapTop = this.getRowTop(time.getHours() * cal.getRowsPerHour());
        snapTop += (snapCount * this.getSnapGapPixels());

        return snapTop;
    },

    setLanes : function (lanes, skipRefreshEvents) {
        var cal = this.creator;
        // make sure there's a lane.name on each lane - see comment in the method
        cal.checkLaneNames(lanes);
        this.lanes = lanes;
        this.laneMap = this.lanes.makeIndex("name");
        this.rebuildFields();
        if (!skipRefreshEvents) this.refreshEvents("setLanes");
    },
    getLane : function (laneName) {
        return this.getLaneMap()[laneName];
    },
    getLaneIndex : function (lane) {
        if (!this.usesLanes()) return;
        var fields = this.body.fields,
            index = -1;
        if (isc.isAn.Object(lane)) index = fields.indexOf(lane)
        else if (isc.isA.String(lane)) {
            index = fields.findIndex("name", lane);
            if (index < 0) index = fields.findIndex(this.calendar.laneNameField, lane);
        }
        return index;
    },
    getLaneWidth : function (lane) {
        var width = null;
        if (isc.isA.String(lane)) lane = this.getLane(lane);
        if (lane) {
            if (lane.width) width = lane.width;
            else {
                var fieldName = this.calendar.laneNameField,
                    index = this.body.fields.findIndex(fieldName, lane[fieldName])
                ;
                width = index >= 0 ? this.body.getColumnWidth(index) : null;
            }
        }
        return width;
    },
    getLaneFromPoint : function (x, y) {
        if (!this.usesLanes()) return null;
        if (x == null) x = this.body.getOffsetX();

        var colNum = this.body.getEventColumn(x),
            lane = this.body.fields[colNum]
        ;

        return !this.isGroupNode(lane) ? lane : null;
    },
    getSublaneFromPoint : function (x, y) {
        if (!this.hasSublanes()) return null;
        if (x == null) x = this.body.getOffsetX();

        var colNum = this.body.getEventColumn(x),
            lane = this.body.fields[colNum],
            sublanes = lane ? lane.sublanes : null
        ;

        if (!sublanes) return null;

        var colLeft = this.body.getColumnLeft(colNum),
            laneOffset = x - colLeft,
            laneWidth = this.getLaneWidth(lane),
            len = sublanes.length,
            offset = 0
        ;
        for (var i=0; i<len; i++) {
            if (offset + sublanes[i].width > laneOffset) {
                return sublanes[i];
            }
            offset += sublanes[i].width;
        }

        return null;
    },

    draw : function (a, b, c, d) {
        this.invokeSuper(isc.DaySchedule, "draw", a, b, c, d);

        this.logDebug('draw', 'calendar');
        // call refreshEvents() whenever we're drawn
        // see comment above dataChanged for the logic behind this

        this.body.addChild(this.eventDragCanvas, null, false);
        this.eventDragCanvas.setView(this);

        if (this._updateColumnLayoutsOnDraw) {
            delete this._updateColumnLayoutsOnDraw;
            this.updateColumnLayouts();
        }

        if (this._refreshEventsOnDraw) {
            delete this._refreshEventsOnDraw;
            this.refreshEvents("draw");
        }

        // if scrollToWorkday is set, do that here
        if (this.calendar.scrollToWorkday) this.delayCall("scrollToWorkdayStart");

        if (this._fireViewChangedOnDraw) {
            delete this._fireViewChangedOnDraw;
            this.calendar.currentViewChanged(this.viewName);
        }

    },

    // To be used with calendar.scrollToWorkday
    scrollToWorkdayStart : function () {
        this.calendar.scrollToTime(this.getWorkdayRange(false).start);
    },

    getWorkdayRange : function (dayRanges) {
        var fields = this.fields,
            result = { start: isc.Time.parseInput("23:59"), end: isc.Time.parseInput("00:01") },
            cal = this.calendar,
            date = cal.chosenDate,
            time
        ;

        if (this.isWeekView() && dayRanges != false) {
            // get the largest range across the week
            for (var i=0; i < fields.length; i++) {
                date = fields[i].date; //this.getDateFromCol(i);
                if (isc.isA.Date(date)) {
                    time = isc.Time.parseInput(cal.getWorkdayStart(date));
                    if (isc.DateUtil.compareDates(result.start, time) < 0) {
                        result.start = time;
                    }
                    time = isc.Time.parseInput(cal.getWorkdayEnd(date));
                    if (isc.DateUtil.compareDates(result.end, time) > 0) {
                        result.end = time;
                    }
                }
            }
        } else if (cal.showDayLanes && dayRanges != false) {
            // get the largest range across the lanes in the day
            for (var i=0; i < fields.length; i++) {
                var field = fields[i],
                    lane = field[cal.laneNameField]
                ;
                if (isc.isA.Date(date)) {
                    time = isc.Time.parseInput(cal.getWorkdayStart(date, lane));
                    if (isc.DateUtil.compareDates(result.start, time) < 0) {
                        result.start = time;
                    }
                    time = isc.Time.parseInput(cal.getWorkdayEnd(date, lane));
                    if (isc.DateUtil.compareDates(result.end, time) > 0) {
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

    getRowHeight : function (record, rowNum) {
        return this.cellHeight;
    },

    cacheWorkdayRowHeight : function () {
        // if the viewportHeight has changed, update this.cellHeight
        var viewportHeight = this.getViewportHeight() - (this.showHeader ? this.headerHeight : 0);
        if (this.calendar.showWorkday) {
            // need to resize rows so the workday fits in the viewport
            var cal = this.calendar,
                // rowCount to divide the space by is work-day hours * rowsPerHour
                range = cal.getWorkdayRange(),
                hours = range.end.getHours() - range.start.getHours(),
                rowCount = hours * cal.getRowsPerHour(this),
                // rowHeight is the viewportHeight divided by the rowCount
                rowHeight = Math.floor(viewportHeight / rowCount)
            ;
            if (this.calendar.sizeToWorkday) {
                this.cellHeight = Math.max(this.calendar.minRowHeight, rowHeight);
                if (this.body) this.body.cellHeight = Math.max(this.calendar.minRowHeight, rowHeight);
            }

            //this.logWarn(this.viewName + " - in cacheWorkdayRowHeight" +
            //    " -- viewportHeight: " + viewportHeight +
            //    " -- body: " + (this.body != null) +
            //    " -- drawn: + " + this.isDrawn() +
            //    " -- setting height " + this.cellHeight
            //);

            // when Calendar.limitToWorkday is true, we need to suppress scrolling beyond the
            // workday rows - to this end, cache the first and last workday-rowNums, the min/max
            // topOffsets for the workday, and the height of the workday at the calculated cellHeight
            this._workdayRowHeight = Math.max(this.calendar.minRowHeight, rowHeight);
            this._workdayFirstRow = range.start.getHours() * cal.getRowsPerHour(this);
            this._workdayLastRow = range.end.getHours() * cal.getRowsPerHour(this);
            this._workdayMinTop = this.cellHeight * this._workdayFirstRow;
            this._workdayMaxTop = this._workdayMinTop;
            this._workdayHeight = this.cellHeight * rowCount;
            if (this._workdayHeight > viewportHeight) {
                this._workdayMaxTop += (this._workdayHeight - viewportHeight);
            }

            //this.logWarn(this.viewName + " - in cacheWorkdayRowHeight" +
            //    " -- viewportHeight: " + viewportHeight +
            //    " -- cellHHeight: " + this.cellHeight +
            //    " -- workdayMinTop: " + this._workdayMinTop +
            //    " -- workdayMaxTop: " + this._workdayMaxTop +
            //    " -- workdayHeight: " + this._workdayHeight
            //);

            this._lastViewportHeight = viewportHeight;
        }
        return this.cellHeight;
    },

    getDayFromCol : function (colNum) {
        if (colNum < 0) return null;
        var dayNum = this.body.fields.get(colNum)._dayNum;
        return dayNum;
    },

    getDateFromCol : function (colNum) {
        if (colNum < 0) return null;
        var cellDate = this.getCellDate(0, colNum);
        return cellDate;
    },

    getColFromDate : function (date, lane) {
        var lDate = isc.DateUtil.getLogicalDateOnly(date);
        for (var i=0; i<this.body.fields.length; i++) {
            var fld = this.body.fields.get(i);
            if (!fld.date) continue;
            if (isc.DateUtil.compareLogicalDates(lDate, fld.date) == 0) {
                if (this.usesLanes() && lane) {
                    // showDayLanes has multiple columns with the same date, but different lane
                    // names -- only return true if date and lane name match
                    if (fld.lane == lane) return i;
                } else return i;
            }
        }
        return null;
    },

    // returne the fieldName for the field with the passed date - better than using colNum
    // which confuses where you can call a method from, in the presence of frozen fields
    // - overrides on the body have different colNum than overrides on the grid, and some
    // Calendar methods can be called from either place
    getFieldNameFromDate : function (date, lane) {
        var field = this.getFieldFromDate(date, lane);
        if (field) return field.name;
        return "";
    },

    // return the field with the passed date
    getFieldFromDate : function (date, lane) {
        var lDate = isc.DateUtil.getLogicalDateOnly(date);
        for (var i=0; i<this.body.fields.length; i++) {
            var fld = this.body.fields.get(i);
            if (!fld.date) continue;
            if (isc.DateUtil.compareLogicalDates(lDate, fld.date) == 0) {
                if (this.usesLanes() && lane) {
                    // showDayLanes has multiple columns with the same date, but different lane
                    // names -- only return the fieldName if date and lane name match
                    if (fld.lane == lane) return fld;
                } else return fld;
            }
        }
        return null;
    },

    isLabelCol : function (colNum) {
        var frozenLength = this.frozenFields ? this.frozenFields.length : 0;
        if (colNum < frozenLength) return true;
        var date = this.getCellDate(1, colNum-frozenLength);
        return date == null;
    },

    // day/weekView - helper function for detecting when a weekend is clicked, and weekends are disabled
    cellDisabled : function (rowNum, colNum) {
        var body = this.getFieldBody(colNum);
        if (!body || body == this.frozenBody) return false;
        var col = this.getLocalFieldNum(colNum),
            date = this.getCellDate(rowNum, col)
        ;
        if (this._dstCells) {
            var cells = this._dstCells;
            // disable any cells that we know cover DST crossover hours - these are
            // detected by _getCellDates(), which runs when the range changes
            for (var i=0; i<cells.length; i++) {
                if (cells[i].rowNum == rowNum && cells[i].colNum == col) {
                    return true;
                }
            }
        }
        return this.calendar.shouldDisableDate(date, this);
    },

    // helper function to refresh dayView cell styles for weekend disabling
    refreshStyle : function () {
        if (!this.body) return;
        if (this.isWeekView() || this.calendar.showDayLanes) {
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
        var cal = this.calendar;

        if (this.isLabelCol(headerFieldNum)) return true;
        if (cal.showDayLanes && !this.isWeekView()) return true;

        var fld = this.getField(headerFieldNum);
        cal.selectTab(0);
        cal.setChosenDate(fld.date);
        return true;
    },

    cellMouseDown : function (record, rowNum, colNum) {
        if (this.isLabelCol(colNum) || this.cellDisabled(rowNum, colNum)) return true;

        var cal = this.calendar;

        // if backgroundMouseDown is implemented, run it and return if it returns false
        var startDate = this.getCellDate(this.body.getEventRow(), this.body.getEventColumn());
        if (cal.backgroundMouseDown && cal.backgroundMouseDown(startDate) == false) return;

        // don't set up selection tracking if canCreateEvents is disabled
        if (!cal.canCreateEvents) return true;
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
        // if Browser.isTouch, don't allow long events to be created by dragging
        if (this.calendar.canDragCreateEvents == false) return;
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
                col = this._selectionTracker.colNum,
                rowCount = this.getTotalRows()
            ;
            for (var i = refreshRowNum - refreshGap; i < refreshRowNum + refreshGap; i++) {
                // don't assume 48 1/2 hour slots in a day - that's already not true, because
                // rowsPerHour/minutesPerRow might be set - also represents a step toward
                // facilities to show any arbitrary period of time in a vertical calendar
                // column, including more than 24 hours
                if (i >= 0 && i < rowCount) this.refreshCellStyle(i, col);
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

        var cal = this.calendar,
            startDate = cal.getCellDate(sRow, colNum, this),
            endDate = cal.getCellDate(sRow+diff, colNum, this)
        ;
        if (!endDate) {
            // if there's no end date (click in last row), assume the end of the day
            endDate = isc.DateUtil.getEndOf(startDate, "D", false, this.firstDayOfWeek);
        }

        // if backgroundClick is implemented, and there's no selection (a click, not just mouseUp),
        // run it and bail if it returns false
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

        var lane, sublane;
        if (cal.showDayLanes && cal.dayViewSelected()) {
            lane = this.getLaneFromPoint();
            sublane = lane ? this.getSublaneFromPoint() : null;

        }
        var newEvent = cal.createEventObject(null,
                startDate,
                endDate,
            lane && lane.name, sublane && sublane.name
        );
        cal.showEventDialog(newEvent, true);
        return isc.EH.STOP_BUBBLING;
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

    destroyEvents : function () {
        if (!this.body || !this.body.children) return;

        var len = this.body.children.length;
        while (--len >= 0) {
            var child = this.body.children[len];
            if (child && !child.destroyed) {
                child.clear();
                child.deparent();
                child.destroy();
            }
            child = null;
        }
        this._usedEventMap = null;
        this._usedCanvasMap = null;
        this._eventCanvasPool = null;
    },

    // day/weekView
    getBaseStyle : function (record, rowNum, colNum) {
        var cal = this.calendar,
            date = cal.getCellDate(rowNum, colNum, this),
            field = this.getField(colNum),
            // the lane is a field (column) in vertical views
            style = (field && field.styleName) ||
                (date && cal.getDateStyle ? cal.getDateStyle(date, rowNum, colNum, this) : null),
            isWeek = this.isWeekView()
        ;

        if (style) {
            // getDateStyle() returned a style - just return that
            return style;
        }

        if (this.isLabelCol(colNum)) return this.labelColumnBaseStyle;

        if (!cal.showWorkday || !cal.styleWorkday) return this.baseStyle;

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
            lane = this.usesLanes() ? this.body.getField(bodyCol)[cal.laneNameField] : null
        ;

        if (currRowTime) {
            var parsedStart = isc.Time.parseInput(cal.getWorkdayStart(currRowTime, lane)),
                parsedEnd = isc.Time.parseInput(cal.getWorkdayEnd(currRowTime, lane))
            ;

            // need to set hours and minutes of start and end to the same as workdayStart and
            // workdayEnd
            wStart.setHours(parsedStart.getHours(), parsedStart.getMinutes(), 0, 0);
            wEnd.setHours(parsedEnd.getHours(), parsedEnd.getMinutes(), 0, 0);

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
    getCellStyle : function (record, rowNum, colNum) {
        var cal = this.calendar,
            field = this.getField(colNum),
            isSelected = this.isSelected(record),
            gridDisabled = this.isDisabled(),
            cellDate = field.date ?
                isc.DateUtil.combineLogicalDateAndTime(field.date, record.logicalTime) : null,
            cellDisabled = cellDate ? cal.shouldDisableDate(cellDate) : false
        ;

        var bStyle = this.getBaseStyle(record, rowNum, colNum);

        if (gridDisabled || (cellDisabled && !isSelected)) {
            bStyle += "Disabled";
        }

        if (this.isLabelCol(colNum)) {
            return bStyle;
        }

        if (this._selectionTracker && this._selectionTracker.colNum == colNum) {
            var sRow = this._selectionTracker.startRowNum,
                eRow = this._selectionTracker.endRowNum;
            // if rowNum is within start and end of selection, return selected style
            if (rowNum >= sRow && rowNum <= eRow || rowNum >= eRow && rowNum <= sRow) {
                if (bStyle == cal.workdayBaseStyle) bStyle += "Selected";
                else bStyle = cal.selectedCellStyle;
                return bStyle;
            }
        }

        // odd rows in vertical views
        if (this.alternateRecordStyles && rowNum % 2 != 0) {
            // odd row in dayView, with alternateRecordStyles
            if (bStyle != cal.workdayBaseStyle) bStyle += "Dark";
        } else if (this.alternateFieldStyles && colNum % 2 != 0) {
            if (cal.dayViewSelected() && cal.showDayLanes) {
                // odd column in dayView with showDayLanes and alternateFieldStyles
                if (bStyle != cal.workdayBaseStyle) bStyle += "Dark";
            }
        }

        return bStyle;
    },

    getWorkdayRowRange : function () {
        var start = this._workdayMinTop / this.cellHeight,
            end = start + (this._workdayHeight / this.cellHeight)
        ;
        return { start: start, end: end };
    }

// end DaySchedule overrides


});

// WeekSchedule
// --------------------------------------------------------------------------------------------
isc.ClassFactory.defineClass("WeekSchedule", "DaySchedule");
isc.WeekSchedule.addProperties({
    rangeUnit: "W"
});

// MonthSchedule
// --------------------------------------------------------------------------------------------
isc.ClassFactory.defineClass("MonthSchedule", "CalendarView");

// Create a separate subclass for month schedule body

isc.ClassFactory.defineClass("MonthScheduleBody", "GridBody");

isc.MonthSchedule.changeDefaults("headerButtonProperties", {
    showRollOver: false,
    showDown: false,
    cursor: "default"
});

isc.MonthSchedule.changeDefaults("bodyProperties", {
    redrawOnResize:true,
    // don't set overflow - means we show a vertical scrollbar if the cell heights would
    // otherwise be less than calendar.minimumDayHeight
    //overflow: "visible",
    // this is necessary because monthView shows rows of two distinct heights (dayHeader/Body)
    fixedRowHeights: false
});

isc.MonthSchedule.addProperties({
    rangeUnit: "M",
    autoDraw: false,
    leaveScrollbarGap: false,

    showAllRecords: true,

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
    // show cell-level rollover
    showRollOver:true,
    useCellRollOvers:true,
    hoverByCell: true,

    showViewHovers: false,

    // default dateEditingStyle for monthView is "datetime", because you can't drag events
    // around to change days, in this view - should probably support that as well
    //dateEditingStyle: "datetime",

    // set up cell-level drag selection
    //canDrag:true,
    // dragAppearance:"none",
    //canDragSelect:true,
    canSelectCells:true,
    navigateOnTab:false,

    dayHeaderHeight: 20,
    // set alternateRecordStyle to false: for many skins, not having this set to
    // false leads to undefined styles being generated like 'calMonthOtherDayBodyDisabledDark'.
    // See GridRenderer.getCellStyleIndex() where it checks for this.alternateRowStyles.
    // We manually set row styles for the month view, so it should be safe to disable
    // alternate row styles.
    alternateRecordStyles: false,

    // return the string to show in the Calendar controlsBar
    getDateLabelText : function (startDate, endDate) {
        // ignore the params passed in - just show the month and year from the chosenDate
        return "<b>" + this.chosenDate.getShortMonthName() + " " + this.chosenDate.getFullYear() + "</b>";
    },

    initWidget : function () {
        var cal = this.calendar;
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
        var dayNames = isc.DateUtil.getShortDayNames();
        var weekendDays = cal.getWeekendDays();
        for (var i = 0; i < 7; i++) {
            var dayNum = (i + this.firstDayOfWeek) % 7;
            this.fields[i].title = dayNames[dayNum];
            this.fields[i]._dayNum = dayNum;
            // store day index to easily get to the right day properties stored on the month
            // records from methods like formatCellValue
            this.fields[i]._dayIndex = i + 1;
            // hide weekends
            if (weekendDays.contains(dayNum)) {
                this.fields[i].showIf = "return list.creator.showWeekends!=false;";
            }

        }

        this.minimumDayHeight = cal.minimumDayHeight;

        // set the range dates
        if (this.rangeUnit) {
            this.periodStartDate = this.startDate = isc.DateUtil.getStartOf(this.chosenDate,
                this.rangeUnit, false, this.firstDayOfWeek);
            this.periodEndDate = this.endDate = isc.DateUtil.getEndOf(this.chosenDate,
                this.rangeUnit, false, this.firstDayOfWeek);
        }

        this.Super("initWidget", arguments);

        // delay refreshing events (which also rebuilds rows in monthView) until draw
        this._refreshEventsOnDraw = true;
    },

    canSelectCell : function (rowNum, colNum) {
        // disallow grid-selection of disabled dates
        return !this.calendar.shouldDisableDate(this.calendar.getCellDate(rowNum, colNum, this));
    },

    getCalendar : function () {
        return this.calendar;
    },

    getTimePerCell : function (unit) {
        // cells are always a day in monthView
        return isc.DateUtil.convertPeriodUnit(1, "d", "mn");
    },
    getTimePerSnapGap : function (unit) {
        // snaps are always a day in monthView
        return isc.DateUtil.convertPeriodUnit(1, "d", "mn");
    },

    getDayArray : function () {
        var dayArr = [], eventArr, endDate,
            cal = this.calendar,
            displayDate = isc.DateUtil.createLogicalDate(cal.year, cal.month, 1)
        ;

        // go back to the first day of the week
        while (displayDate.getDay() != this.firstDayOfWeek) {
            this.incrementDate(displayDate, -1);
        }

        // special case when hiding weekends, can have the first row be entirely from the previous
        // month. In this case, hide the first row by adding 7 days back to the displayDate
         if (!cal.showWeekends) {
            var wEnds = cal.getWeekendDays();
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

        // store the visible-startDate for the view as a datetime
        this.startDate = isc.DateUtil.adjustDate(displayDate, "-0D");

        // 40 days from start date seems like a nice round number for getting
        // all the relevant events in a month, with extra days for adjacent months
        this.endDate = isc.DateUtil.adjustDate(displayDate, "+40D");

        // use the view's periodStart/EndDates, not logical dates, when getting events
        eventArr = cal._getEventsInRange(this.startDate, this.endDate, this);

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

        // cache rowHeights on the records to avoid recalculating them repeatedly, and
        // potentially at the wrong time, from getRowHeight()
        this.cacheRowHeights(dayArr);

        // cache the array of records
        this._dayArray = dayArr;

        return dayArr;
    },

    resized : function () {
        if (!this._dayArray) return;
        // update the rowHeights cached on the current data, so all space is used
        this.cacheRowHeights();
        this.markForRedraw();
    },

    cacheRowHeights : function (rows) {
        rows = rows || this._dayArray;
        var cal = this.calendar,
            dayHeaders = cal.showDayHeaders,
            dayHeaderHeight = dayHeaders ? this.dayHeaderHeight : 0,
            dayCount = dayHeaders ? rows.length / 2 : rows.length,
            bodyHeight = this.body.getVisibleHeight(),
            usedHeight = dayHeaders ? dayHeaderHeight * dayCount : 0,
            rowHeight = Math.floor(bodyHeight / rows.length)
        ;

        if (dayHeaders) {
            // sum the header and body cell heights
            rowHeight = Math.floor((bodyHeight - usedHeight) / dayCount);
            // enforce minimum total day height (header + body cell)
            var minHeight = this.minimumDayHeight - dayHeaderHeight;
            if (rowHeight < minHeight) rowHeight = minHeight;
            usedHeight += (rowHeight * dayCount);
        } else {
            usedHeight = rowHeight * rows.length;
        }

        var extra = bodyHeight - usedHeight;

        for (var i=0; i<rows.length; i++) {
            if (rows[i].isHeaderRow) rows[i].rowHeight = dayHeaderHeight;
            else {
                rows[i].rowHeight = rowHeight;
                if (extra > 0 && i == rows.length - 1) {
                    // add the extra pixels to the last row to use available space
                    rows[i].rowHeight += extra;
                }
            }
        }
        //this.logWarn(isc.echoFull(rows.getProperty("rowHeight")));

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
        obj.isHeaderRow = true;
        return obj;
    },

    _$cellDateKey: "date",
    getCellDate : function (rowNum, colNum) {
        if (rowNum == null && colNum == null) {
            rowNum = this.getEventRow();
            colNum = this.getEventColumn();
        }
        if (rowNum < 0 || colNum < 0) return null;
        var fieldIndex = this.body.fields.get(colNum)._dayIndex,
            record = this.getRecord(rowNum),
            key = [this._$cellDateKey, fieldIndex].join(""),
            cellDate = record[key]
        ;

        return cellDate;
    },

    getCellEndDate : function (rowNum, colNum) {
        // monthView - cellEndDate is the end of the cell's day
        var cellDate = this.getCellDate(rowNum, colNum);
        if (!cellDate) return null;
        return isc.DateUtil.getEndOf(cellDate, "d", false);
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
                if (evnt[this.calendar.startDateField].getMonth() != nDate.getMonth()
                    || evnt[this.calendar.startDateField].getDate() != nDate.getDate()) {
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

    getDateCells : function (date) {
        for (var i=0; i<this.data.length; i++) {
            var row = this.data[i];
            for (var key in row) {
                if (key.startsWith("date") &&
                    isc.DateUtil.compareLogicalDates(date, row[key]) == 0)
                {
                    var cells = [];
                    if (this.calendar.showDayHeaders)
                        cells.add([i+1, new Number(key.substring(4,5))-1]);
                    cells.add([i, new Number(key.substring(4,5))-1])
                    return cells;
                }
            }
        }
        return null;
    },


    refreshEvents : function () {
        var cal = this.calendar;
        // bail if no data yet
        if (!cal.hasData()) return;
        this.logDebug('refreshEvents: month', 'calendar');

        // for monthView, always run setData() from refreshEvents(), because events are in the
        // cellHTML, which needs regenerating in case there are new events in the data
        this.year = cal.year;
        this.month = cal.month;
        var data = this.getDayArray();
        if (this.data) this.setData([]);
        this.setData(data);

        // now the data is present, tweak the stored endDate to match the visible one
        this.endDate = this.getCellEndDate(this.getData().length-1, this.body.fields.length-1);

        this.selectChosenDateCells();
        if (cal.eventsRendered && isc.isA.Function(cal.eventsRendered))
            cal.eventsRendered();
   },

    rowIsHeader : function (rowNum) {
        var cal = this.calendar;
        if (!cal.showDayHeaders || (cal.showDayHeaders && rowNum % 2 == 1)) return false;
        else return true;
    },

    formatCellValue : function (value, record, rowNum, colNum) {
        if (!record) return;
        var cal = this.calendar,
            fieldIndex = this.fields.get(colNum)._dayIndex,
            evtArr = record["event" + fieldIndex],
            currDate = record["date" + fieldIndex],
            isOtherDay = currDate.getMonth() != cal.month;

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
        // getDayArray() caches rowHeights on the data
        if (record && record.rowHeight != null) return record.rowHeight;
        return this.cellHeight;
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
            var cal = this.calendar;
            return cal.getMonthViewHoverHTML(currDate,evtArr);
        }
    },

    // monthView
    getBaseStyle : function (record, rowNum, colNum) {
        var cal = this.calendar, fieldIndex = this.fields.get(colNum)._dayIndex;
        var bStyle;
        if (this.rowIsHeader(rowNum)) { // header
            if ((rowNum == 0 && record["day" + fieldIndex] > 7)
                || (rowNum == this.data.length - 2 && record["day" + fieldIndex] < 7)) {
                if (!cal.showOtherDays) return cal.otherDayBlankStyle;
                else bStyle = cal.otherDayHeaderBaseStyle;
            } else bStyle = cal.dayHeaderBaseStyle;
        } else { // body
            var startRow = cal.showDayHeaders ? 1 : 0, endRow = this.data.length - 1;
            if ((rowNum == startRow && this.data[startRow]["day" + fieldIndex] > 7)
                || (rowNum == endRow && this.data[endRow]["day" + fieldIndex] < 7)) {
                if (!cal.showOtherDays) return cal.otherDayBlankStyle;
                else bStyle = cal.otherDayBodyBaseStyle;
            } else bStyle = cal.dayBodyBaseStyle;
        }
        return bStyle;
    },
    getCellStyle : function (record, rowNum, colNum) {
        var cal = this.calendar,
            field = this.getField(colNum),
            gridDisabled = this.isDisabled(),
            cellDate = this.data[rowNum]["date" + field._dayIndex],
            cellDisabled = cellDate ? cal.shouldDisableDate(cellDate) : false
        ;

        var bStyle = this.getBaseStyle(record, rowNum, colNum);

        if (gridDisabled || cellDisabled) {
            bStyle += "Disabled";
        }

        return bStyle;
    },

    selectChosenDateCells : function () {
        var cal = this.calendar;
        if (cal.selectChosenDate) {
            this.getCellSelection().deselectAll();
            var displayDate = isc.Calendar._getAsDisplayDate(cal.chosenDate),
                cellRange = this.getDateCells(displayDate);
            this.getCellSelection().selectCellList(cellRange);
        }
    },

    // monthView cellClick
    // if a header is clicked, go to that day. Otherwise, open the event dialog for that day.
    cellClick : function (record, rowNum, colNum) {
        var cal = this.calendar,
            year, month,
            fieldIndex = this.fields.get(colNum)._dayIndex,
            currDate = record["date" + fieldIndex],
            evtArr = record["event" + fieldIndex],
            isOtherDay = cal.month != currDate.getMonth(),
            doDefault = false
        ;

        // if showOtherDays is false and the clicked date is in another month, bail without
        // calling setChosenDate()
        if (!cal.showOtherDays && isOtherDay) return;

        var clickDate = isc.DateUtil.createLogicalDate(currDate.getFullYear(),
                                 currDate.getMonth(), currDate.getDate())

        // clicked in a cell, so set the global selected chosenDate - this will update
        // selection in this (month) view and mark day/week views for a refresh, as required
        cal.setChosenDate(clickDate);

        if (this.rowIsHeader(rowNum)) { // header clicked
            if (!(!cal.showOtherDays && isOtherDay)) {
                doDefault = cal.dayHeaderClick(clickDate, evtArr, cal, rowNum, colNum);
            }
            if (doDefault) {
                // just change tabs - the calendar-level chosenDate has already been set
                cal.selectTab(0);
            }
        } else { // day body clicked
            // if the click was in the day-body of a different month, do nothing - view will
            // navigate to that month only
            if (isOtherDay) return;

            if (!this.cellDisabled(rowNum, colNum)) {
                doDefault = cal.dayBodyClick(clickDate, evtArr, cal, rowNum, colNum);
                if (doDefault && cal.canCreateEvents) {
                    var startDate = cal.getCellDate(rowNum, colNum, this),
                        //endDate = this.getCellEndDate(rowNum, colNum, this)
                        // use a 1-hour event-length for new events
                        endDate = isc.DateUtil.adjustDate(startDate, "+1h")
                    ;
                    var newEvent = cal.createEventObject(null, startDate, endDate);
                    cal.showEventDialog(newEvent, true);
                }
            }

        }
    },

    draw : function ( ) {
        this.Super("draw", arguments);

        if (this._refreshEventsOnDraw) {
            delete this._refreshEventsOnDraw;
            this.setChosenDate(this.calendar.chosenDate);
        }
        if (this._fireViewChangedOnDraw) {
            delete this._fireViewChangedOnDraw;
            this.calendar.currentViewChanged(this.viewName);
        }
    },

    // MonthView
    getEditDialogPosition : function (event) {
        var rect = this.body.getCellPageRect(this.getEventRow(), this.getEventColumn());
        return {
            left: Math.max(rect[0], this.body.getPageLeft()),
            top: Math.max(rect[1], this.body.getPageTop())
        };
    }

});

// TimelineView
//---------------------------------------------------------------------------------------------
isc.ClassFactory.defineClass("TimelineView", "CalendarView");

isc.TimelineView.changeDefaults("bodyProperties", {

    snapToCells: false,
    suppressVSnapOffset: true,
    suppressHSnapOffset: true,
    childrenSnapToGrid: false
});

isc.TimelineView.addProperties({
    canSort: false,
    canResizeFields: false,
    canAutoFitFields: false,
    canReorderFields: false,
    showHeaderContextMenu: false,
    showAllRecords: true,
    alternateRecordStyles: false,
    // rollover is dictated by Calendar.showLaneRollover
    showRollOver: false,
    useCellRollOvers: false,
    canSelectCells: false,
    selectionType: "multiple",

    laneNameField: "lane",
    columnWidth: 60,
    laneHeight: 60,

    //cellPadding: 0,
    //fixedRecordHeights: false,

    labelColumnWidth: 75,
    labelColumnBaseStyle: "labelColumn",
    labelColumnAlign: "left",

    eventPageSize: 30,
    trailIconSize: 16,
    leadIconSize: 16,
    scrollToToday: false,//5,

    lineImage: "[SKINIMG]Stretchbar/hsplit_over_stretch.gif",
    trailingEndPointImage: "[SKINIMG]Calendar/trailingDateGripper.png",
    leadingEndPointImage: "[SKINIMG]Calendar/leadingDateGripper.png",

    headerSpanHeight: 28,


    headerProperties: {
        inherentWidth: false
    },

    // timelines always use time editing
    dateEditingStyle: null,

    // clear this value so the dateChooser will show the timeItem according to the data value
    showDateChooserTimeItem: null,

    // events in timelines resize horizontally
    verticalEvents: false,

    // events can span multiple days visually in timelines
    allowMultiDayEvents: true,


    animateFolders: false,


    includeRangeCriteria: true,

    unitSnapGapsPerCell: { minute: 1, hour: 15, day: 60, week: 1440, month: 1440, year: 1440*30 },

    getTimePerCell : function (unit) {
        var cal = this.calendar,
            props = this._cache,
            millis = props.millisPerCell
        ;
        if (!millis) {
            millis = isc.DateUtil.convertPeriodUnit(1 * props.unitsPerColumn, props.granularity, "ms");
        }
        if (!unit) unit = "mn";
        return Math.floor(isc.DateUtil.convertPeriodUnit(millis, "ms", unit));
    },

    getTimePerSnapGap : function (unit) {
        var cal = this.calendar,
            props = this._cache,
            millis = props.millisPerSnapGap
        ;
        if (!millis) {
            if (props.calendarEventSnapGap == null) {
                // eventSnapGap is null - snaps are to cell-boundaries
                millis = this.getTimePerCell("ms");
            } else if (props.calendarEventSnapGap == 0) {
                // eventSnapGap is zero - calculate a snapGap - see doc on Calendar
                if (props.unitsPerColumn > 1) {
                    millis = isc.DateUtil.convertPeriodUnit(1, props.innerHeaderUnit || props.granularity, "ms");
                } else {
                    millis = isc.DateUtil.convertPeriodUnit(this.unitSnapGapsPerCell[props.granularity], "mn", "ms");
                    millis = Math.max(millis, props.minimumSnapGapMillis);
                }
            } else {
                millis = isc.DateUtil.convertPeriodUnit(props.calendarEventSnapGap, "mn", "ms");
                var minMillis = props.minimumSnapGapMillis;
                if (millis < minMillis) {
                    // the eventSnapGap on the calendar is too small to represent in the
                    // available columnWidth - choose the lowest sensible snapGap
                    this.logWarn("Invalid eventSnapGap - " + ((millis / 1000) / 60) +
                        " minutes - altered to the lowest sensible time that can be " +
                        "represented by the column-widths in the current view: " +
                        ((minMillis / 1000) / 60) + " minutes."
                    );
                    millis = minMillis;
                }
            }
            props.calendarEventSnapGap = isc.DateUtil.convertPeriodUnit(millis, "ms", "mn");
        }
        if (!unit) unit = "mn";
        return isc.DateUtil.convertPeriodUnit(millis, "ms", unit);
    },

    getHeaderButtonWidth : function (colNum) {

        var result = this.columnWidth;

        var cal = this.calendar;
        if (cal.headerLevels) {
            // if there are headerLevels, use the _innerHeaderWidth calculated in calcFields()
            result = this._innerHeaderWidth;
        }

        return result;
    },

    getTimePerPixel : function (unit) {
        var cal = this.calendar,
            props = this._cache,
            millis = props.millisPerPixel
        ;
        if (!millis) {
            millis = this.getTimePerCell("ms") / this.getHeaderButtonWidth();
        }
        if (!unit) unit = "mn";
        return isc.DateUtil.convertPeriodUnit(millis, "ms", unit);
    },

    getSnapGapPixels : function (rowNum, colNum) {
        var snapCount = this.getTimePerCell() / this.getTimePerSnapGap();
        return this.getHeaderButtonWidth() / snapCount;
    },

    // first/last *visible* snaps - takes precedence over start/endDate
    getVisibleStartDate : function () {
        var firstSnap = this.getFirstVisibleSnap();
        if (firstSnap) return firstSnap.startDate;
        return this.startDate;
    },
    getVisibleEndDate : function () {
        var lastSnap = this.getLastVisibleSnap();
        if (lastSnap) return lastSnap.endDate;
        return this.endDate;
    },
    getFirstVisibleSnap : function () {
        // return the first *visible* snap - arbitrary date-columns can be hidden
        if (this._snapGapList) {
            return this._snapGapList[this._snapGapList.firstVisibleIndex];
        }
        return null;
    },
    getLastVisibleSnap : function () {
        // return the last *visible* snap - arbitrary date-columns can be hidden
        if (this._snapGapList) {
            return this._snapGapList[this._snapGapList.lastVisibleIndex];
        }
        return null;
    },

    setChosenDate : function (newDate) {
        // newDate is the actual date from the dateChooser (usually), so may or may not be
        // logical - we want view.chosenDate to be whatever gets passed, for round-tripping
        // back to the dateChooser - logical date is used for the date-label
        this.chosenDate = newDate.duplicate();
        this.logicalDate = isc.DateUtil.createLogicalDate(this.chosenDate);

        // pick up values from Calendar
        this.remapCalendarSettings();

        var key = isc.DateUtil.getTimeUnitKey(this.timelineGranularity);

        // startDate is the start of the granularity (day/week, eg) containing the chosenDate
        this.startDate = isc.DateUtil.getStartOf(this.chosenDate, key, false, this.firstDayOfWeek);

        // add (timelineColumnSpan * timelineUnitsPerColumn) - 1 units of the timelineGranularity to
        // the startDate - the subtraction is after the multiplication, because we just want to
        // take one unit from the total, rather than taking one column's-worth of units
        this.endDate = isc.DateUtil.dateAdd(this.startDate, key,
            (this.calendar.timelineColumnSpan * this.timelineUnitsPerColumn)-1, 1,
            // isLogicalDate, rangePosition, firstDayOfWeek
            null, null, this.firstDayOfWeek)

        // and then find the end of that granularity - eg, for a four week view, we figure out
        // the start of the first week, add 3 full weeks (so, the start of the fourth week),
        // and then get the end of that week
        this.endDate = isc.DateUtil.getEndOf(this.endDate, key, null, this.firstDayOfWeek);

        this.setTimelineRange(this.startDate, this.endDate);
    },

    getDateLabelText : function (startDate, endDate) {
        if (!startDate || !endDate) {
            this.logInfo("missing dates in getDateLabelText()");
            return "";
        }
        var sDate = new Date(startDate.getTime() + 1),
            eDate = new Date(endDate.getTime() - 1)
        ;
        var sYear = sDate.getFullYear(),
            sMonth = sDate.getMonth(),
            sDay = sDate.getDate(),
            eYear = eDate.getFullYear(),
            eMonth = eDate.getMonth(),
            eDay = eDate.getDate(),
            s = ""
        ;

        if (sYear == eYear) {
            // same year
            if (sMonth == eMonth) {
                // same month
                if (sDay == eDay) {
                    // same day
                    s = sDate.getShortMonthName() + " " + sDay + ", " + sYear;
                } else {
                    // different day
                    s = sDate.getShortMonthName() + " " + sDay + " - " + eDay + ", " + eYear;
                }
            } else {
                // different month
                s = sDate.getShortMonthName() + " " + sDay + " - " + eDate.getShortMonthName() + " " + eDay + ", " + eYear;
            }
        } else {
            // different year
            s = sDate.getShortMonthName() + " " + sDay + ", " + sYear + " - " + eDate.getShortMonthName() + " " + eDay + ", " + eYear;
        }

        return "<b>" + s + "</b>";
    },


    groupNodeStyle: null,
    groupNodeBaseStyle: "groupNode",
    // timeline
    initWidget : function () {
        this.fields = [];

        // remember the initial headerHeight, to recalculate as headerLevels change
        this._headerHeight = this.headerHeight;

        this.remapCalendarSettings();

        var c = this.calendar;

        var granString = isc.DateUtil.getTimeUnitKey(this.timelineGranularity);

        if (c.startDate) {
            c.startDate = isc.DateUtil.getStartOf(c.startDate, granString, false,
                this.firstDayOfWeek);
            this.startDate = c.startDate.duplicate();
        }
        if (c.endDate) {
            // round to the end of the granularity
            c.endDate = isc.DateUtil.getEndOf(c.endDate, granString, false,
                this.firstDayOfWeek);
            this.endDate = c.endDate.duplicate();
        }

        if (!this.startDate) {
            this.startDate = c.startDate = isc.DateUtil.getAbsoluteDate("-0" + granString, c.chosenDate);
        }

        if (!this.endDate) {
            // no endDate - default to defaultTimelineColumnSpan columns of timelineGranularity
            this.endDate = c.endDate = isc.DateUtil.getAbsoluteDate("+" +
                    c.defaultTimelineColumnSpan + granString, this.startDate);
        } else if (isc.DateUtil.compareDates(this.startDate, this.endDate) == -1) {
            // startDate is larger than endDate - log a warning and switch the dates
            var s = this.startDate;
            this.startDate = c.startDate = this.endDate.duplicate();
            this.endDate = c.endDate = s;
            this.logWarn("Timeline startDate is later than endDate - switching the values.");
        }



        this.Super("initWidget", arguments);


        // only refreshData at this time if the calendar is not autoFetchData: true
        this._refreshEventsOnDraw = true;

        this.addAutoChild("eventDragCanvas", { styleName: this.eventDragCanvasStyleName });
        //this.body.addChild(this.eventDragCanvas);



        // set up grouping based on the laneGroupBy settings on Calendar
        if (c.canGroupLanes == true) this._groupOnDraw = true;
    },

    remapCalendarSettings : function () {
        var c = this.calendar;

        this.firstDayOfWeek = c.firstDayOfWeek;
        if (c.renderEventsOnDemand != null) this.renderEventsOnDemand = c.renderEventsOnDemand;

        // lanes - name field on lanes
        if (c.laneNameField) this.laneNameField = c.laneNameField;
        // default width of laneFields (frozen)
        if (c.labelColumnWidth != null) this.labelColumnWidth = c.labelColumnWidth;
        // alternate row styling
        if (c.alternateLaneStyles != null) this.alternateRecordStyles = c.alternateLaneStyles;
        // lane rollovers
        if (c.showLaneRollOver != null) {
            this.showRollOver = c.showLaneRollOver;
            this.useCellRollOvers = false;
        }
        // lane drag-reorder
        if (c.canReorderLanes != null) this.canReorderRecords = c.canReorderLanes;

        // set up grouping based on the laneGroupBy settings on Calendar
        // this is now done in initWidget, so it doesn't get re-applied later
        //if (c.canGroupLanes == true) this._groupOnDraw = true;

        if (c.eventDragGap != null) this.eventDragGap = c.eventDragGap;

        this.cellHeight = this.laneHeight;

        if (c.headerLevels) {
            this.headerLevels = isc.shallowClone(c.headerLevels);
        }
        var innerHeader = this.headerLevels && this.headerLevels.length > 0 ?
                this.headerLevels[this.headerLevels.length-1] : null;

        if (innerHeader) {
            // if there's an inner headerLevel, use it's unit as the timelineGranularity
            // - do this now, before we calculate range dates below
            this.timelineGranularity = innerHeader.unit;
            c.timelineGranularity = innerHeader.unit;
        } else {
            this.timelineGranularity = c.timelineGranularity;
        }
        this.timelineUnitsPerColumn = c.timelineUnitsPerColumn;
    },

    // install/removeLocalHandlers are automatically called if they exist - timelines only for now
    installLocalHandlers : function () {
        if (this.calendar.showLaneRollOver) {
            // hook a few mouse events to pre-calculate a few values like mouse-date, and support
            // lane rollovers on both internal eventCanvas drags and externally initiated drags
            this.viewMouseMoveEventId = isc.Page.setEvent("mouseMove", this.getID() + ".viewMouseMove()");
            this.viewDragMoveEventId = isc.Page.setEvent("dragMove", this.getID() + ".viewDragMove()");
            this.viewDragRepositionMoveEventId = isc.Page.setEvent("dragRepositionMove", this.getID() + ".viewDragMove()");
            this._mouseEventsInstalled = true;
        }
    },
    removeLocalHandlers : function () {
        if (this._mouseEventsInstalled) {
            isc.Page.clearEvent("mouseMove", this.viewMouseMoveEventId);
            isc.Page.clearEvent("dragMove", this.viewDragMoveEventId);
            isc.Page.clearEvent("dragRepositionMove", this.viewDragRepositionMoveEventId);
            delete this._mouseEventsInstalled;
        }
    },

    initCacheValues : function () {
        var cal = this.calendar;
        this._cache = {
            alternateLaneStyles: this.alternateRecordStyles,
            firstDayOfWeek: this.firstDayOfWeek,
            granularity: this.timelineGranularity,
            unitsPerColumn: this.timelineUnitsPerColumn || 1,
            rangeStartDate: this.startDate,
            rangeEndDate: this.endDate,
            calendarEventSnapGap: cal.eventSnapGap
        };
        this._cache.rangeStartMillis = this._cache.rangeStartDate.getTime();
        this._cache.rangeEndMillis = this._cache.rangeEndDate.getTime();
        this.updateSnapProperties();
        return this._cache;
    },

    updateSnapProperties : function () {
        if (this.fieldHeaderLevel) this._cache.innerHeaderUnit = this.fieldHeaderLevel.unit;
        this.Super("updateSnapProperties", arguments);
    },

    dragSelectCanvasDefaults: {
        _constructor: "Canvas",
        styleName: "calendarCellSelected",
        opacity: 60,
        width: 1,
        height: 1,
        disabled: true,
        visibility: "hidden",
        autoDraw: false,
        resizeNow : function (props) {
            var view = this.creator,
                cal = view.calendar,
                p = isc.addProperties({}, this.props, props)
            ;

            var lane = p.lane ? view.getLane(p.lane) : null;
            var sublane = p.lane && p.sublane ? view.getSublane(p.sublane) : null;
            if (p.top == null) {
                p.top = view.getRowTop(view.getLaneIndex(p.lane));
                if (sublane) p.top += sublane.top;
            }
            if (p.height == null) {
                p.height = sublane ? sublane.height : view.getLaneHeight(p.lane);
            }
            var left = p.startSnap.startLeftOffset,
                right = p.endSnap.endLeftOffset,
                width = Math.abs(right - left)
            ;

            this.props = p;

            this.moveTo(left, p.top);
            this.resizeTo(width, p.height);
            if (!this.isDrawn()) this.draw();
            if (!this.isVisible()) {
                this.show();
            }
            if (view.shouldShowDragHovers()) isc.Hover.show(this.getHoverHTML());
        },
        hoverMoveWithMouse: true,
        showHover: true,
        hoverDelay: 0,
        hoverProps: {
            overflow: "visible",
            hoverMoveWithMouse: this.hoverMoveWithMouse
        },
        getHoverHTML : function () {
            var view = this.creator,
                props = this.props,
                startDate = props.startSnap[view.calendar.startDateField],
                endDate = props.endSnap[view.calendar.endDateField]
            ;
            var newEvent = view.calendar.createEventObject({}, startDate, endDate,
                    props.lane, props.sublane);
            return view.calendar._getDragHoverHTML(view, newEvent);
        }
    },
    getDragSelectCanvas : function (props) {
        if (!this.body) return null;
        if (!this.dragSelectCanvas) {
            this.dragSelectCanvas = this.createAutoChild("dragSelectCanvas", { eventProxy: this.body });
            this.body.addChild(this.dragSelectCanvas);
        }
        return this.dragSelectCanvas;
    },
    cellMouseDown : function (record, rowNum, colNum) {
        // if an event has leading/trailingDate icons in-drag, ignore mouseDown
        if (isc.EH.dragTarget || isc.EH.lastTarget != this.body) return;
        if ((record && record._isGroup) || this.isLabelCol(colNum)) {
            return true;
        }

        var cal = this.calendar;

        if (cal.canDragCreateEvents == false && this.canDragScroll) {
            // set up to drag-scroll the grid-body
            this._dragScrolling = true;
            this._dragBodyScrollLeft = this.body.getScrollLeft();
            this._dragMouseStartX = this._dragMouseLastX = this.getOffsetX();
            this._mouseDown = true;
            return false;
        }

        var mouseData = this.getMouseData() || { x: this.body.getOffsetX(), y: this.body.getOffsetY() },
            startSnap = this.getSnapData(mouseData.x, mouseData.y),
            startDate = startSnap && startSnap[cal.startDateField]
        ;

        // don't allow selection if the date is disabled (eg, a its weekend and weekends are
        // disabled)
        if (cal.shouldDisableDate(startDate, this)) {
            return false;
        }

        // if backgroundMouseDown is implemented, run it and return if it returns false
        if (cal.backgroundMouseDown && cal.backgroundMouseDown(startDate) == false) return;

        // don't set up selection tracking if canCreateEvents is disabled
        if (!cal.canCreateEvents || cal.canDragCreateEvents == false) return true;
        // first clear any previous selection
        this.clearSelection();

        var canvas = this.getDragSelectCanvas(),
            endDate = startSnap[cal.endDateField],
            lane = this.getLaneFromPoint(),
            sublane = this.getSublaneFromPoint()
        ;

        var p = { top: null, height: null };
        p.lane = lane && lane.name;
        p.sublane = sublane && sublane.name;
        p.draggingLeftEdge = false;
        p.startSnap = startSnap;
        p.endSnap = startSnap;
        canvas.resizeNow(p);

        this._mouseDown = true;
        return false;
    },

    cellOver : function (record, rowNum, colNum) {
        colNum -=1;
        this._lastOverLaneIndex = rowNum;

        if (this._dragScrolling) {
            // drag-scroll the grid-body
            var scrollLeft = this.body.getScrollLeft(),
                newMouseX = this.getOffsetX(),
                delta = this._dragMouseLastX - newMouseX
            ;

            this._dragMouseLastX = newMouseX;

            var scrollToX = Math.max(0, this._dragBodyScrollLeft + delta);

            //isc.logWarn("_dragMouseStartX = " + this._dragMouseStartX + "\n" +
            //    "bodyScrollLeft = " + scrollLeft + "\n" +
            //    "newMouseX = " + newMouseX + "\n" +
            //    "delta = " + delta + "\n" +
            //    "scrollToX = " + scrollToX);

            //this.body.scrollTo(scrollToX);
            this.body.scrollBy(delta);
        } else if (this._mouseDown) {
            var canvas = this.getDragSelectCanvas(),
                props = canvas.props,
                mouseData = this.getMouseData() || { x: this.body.getOffsetX(), y: this.body.getOffsetY()},
                snapData = this.getSnapData(mouseData.x, mouseData.y)
            ;

            if (snapData) {
                if (snapData.index < props.startSnap.index) {
                    // mouse-snap is earlier than previous startSnap - left drag
                    if (props.draggingLeftEdge) props.startSnap = snapData;
                    else {
                        // swap the start and end snaps and mark as a left-edge drag
                        props.endSnap = props.startSnap;
                        props.startSnap = snapData;
                        props.draggingLeftEdge = true;
                    }
                } else if (snapData.index > props.endSnap.index) {
                    // mouse-snap is after endSnap - right drag
                    if (!props.draggingLeftEdge) props.endSnap = snapData;
                    else {
                        // swap the start and end snaps and mark as a right-edge drag
                        props.startSnap = props.endSnap;
                        props.endSnap = snapData;
                        props.draggingLeftEdge = false;
                    }
                } else {
                    if (props.draggingLeftEdge) props.startSnap = snapData;
                    else props.endSnap = snapData;
                }

                canvas.resizeNow(props);
            }
        }

        return this.Super("cellOver", arguments);
    },

    cellMouseUp : function (record, rowNum, colNum) {
        if (!this._mouseDown) return;

        this._mouseDown = false;

        if (this.shouldShowDragHovers()) isc.Hover.hide();


        if (this._dragScrolling) {
            // end drag-scrolling of the grid-body
            this._dragMouseStartX = null;
            this._dragBodyScrollLeft = null;
            this._dragScrolling = false;
            return isc.EH.STOP_BUBBLING;
        }

        var cal = this.calendar,
            canvas = this.getDragSelectCanvas(),
            props = canvas.props,
            startDate = props.startSnap[cal.startDateField],
            endDate = props.endSnap[cal.endDateField]
        ;

        // if backgroundClick is implemented, run it and return if it returns false
        if (cal.backgroundClick) {
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

        // don't show an event editor if the date is disabled (eg, a its weekend and weekends are
        // disabled) - take a millisecond off the end date in case it ends exactly at the start
        // of a disabled date - for example, at midnight on a friday night when weekends are
        // disabled
        if (cal.shouldDisableDate(isc.DateUtil.dateAdd(endDate, "ms", -1), this)) {
            this.clearSelection();
            return false;
        }

        var newEvent = cal.createEventObject(null, startDate, endDate,
            props.lane, props.sublane
        );
        cal.showEventDialog(newEvent, true);
        return isc.EH.STOP_BUBBLING;
    },

    clearSelection : function () {
        var canvas = this.getDragSelectCanvas();
        if (canvas) canvas.hide();
    },

    getCellDate : function (rowNum, colNum) {
        if (!this.body) return null;
        var field = this.body.getField(colNum);
        if (!field || !field.date) return null;
        return field.date;
    },

    getCellEndDate : function (rowNum, colNum) {
        if (!this.body) return null;
        var field = this.body.getField(colNum);
        if (!field || !field.endDate) return null;
        return field.endDate;
    },

    recordDrop : function (dropRecords, targetRecord, index, sourceWidget) {
        this.Super("recordDrop", arguments);
        this._refreshData();
        this.markForRedraw();
    },

    getFirstDateColumn : function () {
        return this.frozenBody ? this.frozenBody.fields.length : 0;
    },


    updateOverlapRanges : function (passedData) {
        var cal = this.calendar,
            rawData = passedData || this.getEventData(),
            dataLen = rawData.getLength(),
            ranges = this.overlapRanges || [],
            // the list of overlap ranges that were actually affected by the process, so the
            // ranges that need to be re-tagged
            touchedRanges = [],
            minDate = this.startDate,
            maxDate = this.endDate
        ;

        var data = (isc.isA.ResultSet(rawData) ? rawData.allRows : rawData);

        data.setProperty("tagged", false);
        data.setProperty("overlapProps", null);
        data.setProperty("slotNum", null);

        data.setSort([
            { property: cal.laneNameField, direction: "ascending" },
            { property: cal.startDateField, direction: "ascending" },
            { property: cal.endDateField, direction: "descending" }
        ]);

        for (var i=0; i<dataLen; i++) {
            var event = data.get(i);
            var eRange = { events: [event] };
            eRange[cal.leadingDateField] = event[cal.leadingDateField];
            eRange[cal.startDateField] = cal.getEventStartDate(event);
            // clamp range-start
            if (eRange[cal.leadingDateField] > maxDate) eRange[cal.leadingDateField] = maxDate;
            if (eRange[cal.startDateField] < minDate) eRange[cal.startDateField] = minDate;

            eRange[cal.trailingDateField] = event[cal.trailingDateField];
            eRange[cal.endDateField] = cal.getEventEndDate(event);
            // clamp range-end
            if (eRange[cal.trailingDateField] > maxDate) eRange[cal.trailingDateField] = maxDate;
            if (eRange[cal.endDateField] > maxDate) eRange[cal.endDateField] = maxDate;
            eRange[cal.laneNameField] = eRange.lane = event[cal.laneNameField];

            var addRange = true;

            for (var j=0; j<ranges.length; j++) {
                if (eRange[cal.laneNameField] != ranges[j][cal.laneNameField]) continue;
                if (this.eventsOverlap(eRange, ranges[j], true)) {
                    // merge the two ranges - the dates of the existing range are altered to
                    // fully incorporate both ranges and events are copied over
                    this.mergeOverlapRanges(eRange, ranges[j]);
                    addRange = false;
                }
                if (!addRange) break;
            }
            if (addRange) {
                ranges.add(eRange);
                if (!touchedRanges.contains(eRange)) touchedRanges.add(eRange);
            }
        }

        for (i=0; i<ranges.length; i++) {
            var range = ranges[i];
            // set an overlapRangeId on the range and it's events
            range.id = "range_" + i + "_lane_" + range.lane;
            range.events.setProperty("overlapRangeId", range.id);
        }

        this.overlapRanges = ranges;

        return touchedRanges;
    },

    getOverlapSlot : function (index, snapCount) {
        var slot = { slotNum: index, events: [], snapGaps: [] };
        for (var i=0; i<snapCount; i++) slot.snapGaps[i] = 0;
        return slot;
    },
    tagDataForOverlap : function (data, lane) {
        data = data || this.getEventData();
        if (data.getLength() == 0) return;

        var addLogs = false;

        var cal = this.calendar;

        if (cal.eventAutoArrange == false) return;

        this.forceDataSort(data);

        var useLanes = this.usesLanes();

        var olRanges = this.updateOverlapRanges(data);

        var rangeSort = [];
        if (useLanes) {
            rangeSort.add({ property: cal.laneNameField, direction: "ascending" });
        }
        if (cal.overlapSortSpecifiers) {
            rangeSort.addList(cal.overlapSortSpecifiers);
        } else {

            rangeSort.add({ property: "eventLength", direction: "descending" });
            rangeSort.add({ property: cal.startDateField, direction: "ascending" });
            rangeSort.add({ property: cal.endDateField, direction: "ascending" });
        }


        if (addLogs) {
            this.logWarn("tagDataForOverlap: about to loop over " + olRanges.length + " overlap ranges");
        }

        for (var j = 0; j<olRanges.length; j++) {
            var range = olRanges[j];

            if (addLogs) {
                this.logWarn("range: " + isc.echoFull(range) + "");
            }

            var sDate = range[cal.leadingDateField] || range[cal.startDateField]
            var eDate = range[cal.trailingDateField] || range[cal.endDateField]
            var rangeStartSnapObj = this.getSnapData(null, null, sDate),
                rangeStartSnap = rangeStartSnapObj ? rangeStartSnapObj.index : 0,
                rangeEndSnapObj = this.getSnapData(null, null, eDate),
                rangeEndSnap = rangeEndSnapObj ? rangeEndSnapObj.index : this._snapGapList.length-1,
                // range start and end snaps are inclusive
                rangeSnapCount = (rangeEndSnap-rangeStartSnap) + 1,
                slotList = [],
                slotCount = 1
            ;

            // add an initial slot
            slotList[0] = this.getOverlapSlot(0, rangeSnapCount);

            var events = range.events;

            events.setSort(rangeSort);

            for (var eventIndex=0; eventIndex<events.length; eventIndex++) {

                var event = events[eventIndex];

                event.overlapProps = {};

                var oProps = event.overlapProps;

                // get the event's snapGapList - last param will return the first/last snaps
                // if the dates are out of range
                var eStart = event[cal.leadingDateField] || cal.getEventStartDate(event),
                    eEnd = event[cal.trailingDateField] || cal.getEventEndDate(event)
                ;
                // tweak the dates by 1ms, to prevent exact matches on a snap-boundary from
                // causing incorrect overlaps
                oProps.eventStartSnap = this.getSnapData(null, null, eStart.getTime()+1, true);
                oProps.eventEndSnap = this.getSnapData(null, null, eEnd.getTime()-1, true);

                // deal with hidden snaps - if eventStart/EndSnap aren't set, use last/nextValidSnap
                var eStartSnap = (oProps.eventStartSnap ? oProps.eventStartSnap.index : oProps.nextValidSnap.index) -rangeStartSnap;
                var eEndSnap = (oProps.eventEndSnap ? oProps.eventEndSnap.index : oProps.lastValidSnap.index) -rangeStartSnap;

                var found = false;
                var slot = null;

                for (var slotIndex=0; slotIndex<slotCount; slotIndex++) {
                    var gaps = slotList[slotIndex].snapGaps.slice(eStartSnap, eEndSnap+1);
                    var used = gaps.sum() > 0;
                    if (!used) {
                        found = true;
                        slotList[slotIndex].snapGaps.fill(1, eStartSnap, eEndSnap+1);
                        slotList[slotIndex].events.add(event);
                        event.overlapProps.slotNum = slotIndex
                        if (addLogs) {
                            this.logWarn("event " + event.name + " occupying slot " + slotIndex);
                        }
                        break;
                    }
                }
                if (!found) {
                    // add a new slot
                    slotList[slotCount] = this.getOverlapSlot(slotCount, rangeSnapCount);
                    slotList[slotCount].snapGaps.fill(1, eStartSnap, eEndSnap+1);
                    slotList[slotCount].events.add(event);
                    event.overlapProps.slotNum = slotCount
                    if (addLogs) {
                        this.logWarn("event " + event.name + " added to new slot index " + slotCount);
                    }
                    slotCount++;
                }

            }

            for (var i=0; i<slotList.length; i++) {
                var slot = slotList[i];
                // for each event in this slot, check all later slots - if one has an event
                // that overlaps this event directly, this event ends in the slot before -
                // decides this event's slotCount
                for (var eIndex=0; eIndex < slot.events.length; eIndex++) {
                    var event = slot.events[eIndex];
                    var oProps = event.overlapProps;

                    // update the totalSlots
                    oProps.totalSlots = slotCount;

                    // get the event snapGaps
                    var eStartSnap = (oProps.eventStartSnap ? oProps.eventStartSnap.index : rangeStartSnap) -rangeStartSnap;
                    var eEndSnap = (oProps.eventEndSnap ? oProps.eventEndSnap.index : rangeStartSnap) -rangeStartSnap;

                    var found = false;

                    for (var innerIndex=i+1; innerIndex<slotList.length; innerIndex++) {
                        var gaps = slotList[innerIndex].snapGaps.slice(eStartSnap, eEndSnap+1);
                        var used = gaps.sum() > 0;
                        if (used) {
                            oProps.slotCount = innerIndex - oProps.slotNum;
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        // should span all following slots
                        oProps.slotCount = slotCount - oProps.slotNum;
                    }
                    // we want slotNum to start from 1, for legacy downstream code
                    oProps.slotNum++;
                    event.slotNum = oProps.slotNum;
                }
            }

            range.slotList = slotList;

            if (addLogs) {
                this.logWarn("***** slotList *****\n" + isc.echoFull(slotList));
            }
        }
    },

    getSnapData : function (x, y, date, returnExtents) {
        var snaps = this._snapGapList,
            snapCount = snaps.length,
            findByDate = (date != null),
            millis = null
        ;

        if (findByDate) {
            if (isc.isA.Number(date)) millis = date;
            else if (date.getTime) millis = date.getTime();
        } else {
            if (x == null) x = this.body.getOffsetX();
        }

        if (millis != null) {
            // if the date is out of range, return the first or last snap
            if (millis < snaps[snaps.firstVisibleIndex].startMillis) {
                if (returnExtents) return snaps[snaps.firstVisibleIndex];
                return null;
            }
            if (millis > snaps[snaps.lastVisibleIndex].endMillis) {
                if (returnExtents) return snaps[snaps.lastVisibleIndex];
                return null;
            }

        }

        for (var i=0; i<snapCount; i++) {
            var snap = snaps[i];
            if (findByDate) {
                if (snap.startMillis <= millis && snap.endMillis >= millis) {
                    // always return the appropriate snap - hidden snaps link to the next/last good snaps
                    return snap;
                }
            } else {
                // if the start and end dates of the snap are hidden, it's a hidden field - ignore
                if (snap.startHidden && snap.endHidden) continue;
                if (x >= snap.startLeftOffset && x <= snap.endLeftOffset) return snap;
            }
        }

        return null;
    },

    buildSnapGapList : function (reason) {
        if (!this._cache) return;
        //isc.logWarn("buildSnapGapList - reason '" + reason + "'");
        if (!this.body) return;
        var cal = this.creator,
            fields = this.frozenBody ? this.body.fields : this.getFields(),
            snapPixels = cal.getSnapGapPixels(this),
            pixelTime = this.getTimePerPixel("ms"),
            snapTime = this.getTimePerSnapGap("ms"),
            snapMins = this.getTimePerSnapGap("mn"),
            rangeStartDate = this._cache.rangeStartDate,
            startTime = this._cache.rangeStartMillis,
            endTime = this._cache.rangeEndMillis,
            rangeEndDate = this._cache.rangeEndDate,
            loopDate = this._cache.rangeStartDate.duplicate(),
            currTime = startTime,
            i = 0,
            snapList = [],
            shouldBreak = false,
            lastStartCol = null,
            fieldSnapIndex = 0,
            // remember the first/last visible snaps - tag these onto the _snapGapList array
            firstVisibleIndex = null,
            lastVisibleIndex = null,
            nextTime
        ;
        while (currTime < endTime) {
            var newNextTime = currTime + snapTime;
            if (newNextTime == nextTime) {
                this.logWarn("snapGaps " + i + " and " + (i+1) + " have identical times");
            } else if (newNextTime >= endTime) {
                newNextTime = endTime;
                shouldBreak = true;
            }
            if (snapMins == 1440) {

                loopDate.setDate(loopDate.getDate() + 1);
                newNextTime = loopDate.getTime();
            } else {
                loopDate.setTime(newNextTime - 1);
            }

            nextTime = newNextTime;
            nextTime--;

            var snap = { index: i++, startMillis: currTime, endMillis: nextTime,
                    startDate: new Date(currTime),
                    endDate: new Date(nextTime)
                    //startDate: loopDate.duplicate()
            };

            snap.startField = this.getFieldContainingDate(currTime+1, true);
            if (snap.startField) {
                snap.startCol = fields.indexOf(snap.startField);
                // modify the start/end leftOffsets for the snap's startField, according to the current size
                snap.startField.startLeftOffset = this.body.getColumnLeft(snap.startCol);
                snap.startField.endLeftOffset = snap.startField.startLeftOffset + this._innerHeaderWidth;
            } else {
                snap.startHidden = true;
            }

            if (snap.startField) {
                // if the colNum just changed, zero out the pixel-offset counter
                if (snap.startCol == lastStartCol) {
                    fieldSnapIndex++;
                } else {
                    lastStartCol = snap.startCol;
                    fieldSnapIndex = 0;
                }
                snap.fieldSnapIndex = fieldSnapIndex;

                var startXOffset = cal.getMinutePixels(Math.floor((currTime - snap.startField.date.getTime()) / 1000 / 60), null, this);
                snap.startLeftOffset = snap.startField.startLeftOffset + startXOffset;
            }

            snap.endField = this.getFieldContainingDate(nextTime, true);
            if (!snap.endField) {

                snap.endField = this.getFieldContainingDate(nextTime, false);
            }
            if (snap.endField) {
                snap.endCol = fields.indexOf(snap.endField);
                // modify the start/end leftOffsets for the snap's startField, according to the current size
                snap.endField.startLeftOffset = this.body.getColumnLeft(snap.endCol);
                snap.endField.endLeftOffset = snap.endField.startLeftOffset + this._innerHeaderWidth;
            } else {
                snap.endHidden = true;
            }

            if (snap.endField) {
                var endXOffset = cal.getMinutePixels(Math.floor((snap.endField.endDate.getTime() - nextTime) / 1000 / 60), null, this);
                snap.endLeftOffset = snap.endField.endLeftOffset - endXOffset;
            }

            snap[cal.startDateField] = new Date(currTime);
            snap[cal.endDateField] = new Date(nextTime);

            snapList.add(snap);

            if (!snap.startHidden) {
                // cache the first and last visible snaps for getVisibleStart/EndDate()
                if (firstVisibleIndex == null) firstVisibleIndex = snap.index;
                if (lastVisibleIndex == null || snap.index > lastVisibleIndex) lastVisibleIndex = snap.index;
            }

            if (shouldBreak) break;
            currTime = nextTime + 1;
        }

        var lastSnap;
        var startHideIndex;
        var lastGoodSnapIndex;
        for (var i=0; i<snapList.length; i++) {
            var snap = snapList[i];
            if (snap.startHidden) {
                if (startHideIndex == null) startHideIndex = i;
            } else {
                if (startHideIndex != null) {
                    for (var j=startHideIndex; j<i; j++) {
                        snapList[j].nextValidSnap = snap;
                    }
                    startHideIndex = null;
                }
            }
            if (snap.endHidden) {
                snap.lastValidSnap = snapList[lastGoodSnapIndex];
            } else {
                lastGoodSnapIndex = i;
            }
            lastSnap = snap;
        }

        // shift the final snapGap so it ends at the end of the accessible body
        if (lastSnap.endField) lastSnap.endField.endLeftOffset = this.header.getWidth()-1;


        if (lastSnap.startField && lastSnap.endField) {
            if (lastSnap.startField.masterIndex == lastSnap.endField.masterIndex)
                lastSnap.startField.endLeftOffset = lastSnap.endField.endLeftOffset;
        }

        if (firstVisibleIndex != null) snapList.firstVisibleIndex = firstVisibleIndex;
        if (lastVisibleIndex != null) snapList.lastVisibleIndex = lastVisibleIndex;

        this._snapGapList = snapList;

        //this.logWarn("buildSnapGapList:  " + reason + "\n\n" + isc.echoAll(snapList));
    },

    _rebuildFields : function () {

        this._needsSnapGapUpdate = true;

        var fields = this.calcFields();
        if (this.isDrawn()) {
            this.body.removeChild(this.eventDragCanvas);
            this.setFields(fields);
            this.body.addChild(this.eventDragCanvas, null, false);
        } else this.fields = fields;


        this.buildSnapGapList("_rebuildFields");


        if (this.isDrawn() && this.scrollToToday !== false) {
            var today = isc.DateUtil.getLogicalDateOnly(new Date());
            today.setDate(today.getDate() - this.scrollToToday);
            var colNum = this.getColFromDate(today, true);
            if (colNum != null) {
                this.bodies[1] && this.bodies[1].scrollTo(this.bodies[1].getColumnLeft(colNum), 0);
            }
        }
    },
    _rebuild : function (refreshData) {
        this.clearEvents();

        // availability of hovers may have changed
        this.setShowHover(this.calendar.showViewHovers);

        this._rebuildFields();

        if (!this._skipSetLanes) {
            var lanes = this.lanes || this.calendar.lanes || [];
            this.setLanes(lanes.duplicate(), true);
        }
        this._scrubDateRange();

        if (refreshData) {
            this._refreshData();
        } else {
            // TODO: this should really be doing a refreshVisibleEvents(), since refreshData is
            // false - needs to be looked into as part of streamlining for large datasets
            this.refreshEvents("_rebuild");
            //this.refreshVisibleEvents();
        }
    },

    refreshEvents : function () {

        this.buildSnapGapList("refreshEvents");
        return this.Super("refreshEvents", arguments);
    },

    setLanes : function (lanes, skipRefreshEvents) {
        // clear the cached lane info
        this.resetLaneCaches(true);

        //this.logWarn("*** running setLanes) -- " + isc.echoFull(lanes));
        var cal = this.calendar,
            laneNameField = cal.laneNameField;

        // make sure there's a lane.name on each lane - see comment in the method
        cal.checkLaneNames(lanes);

        // set up the local lanes - ones which are shouldShowLane() true
        this.lanes = [];
        for (var i=0; i<lanes.length; i++) {
            if (this.calendar.shouldShowLane(lanes[i])) this.lanes.add(lanes[i]);
        }

        // size the visible lanes
        var laneCount = this.lanes.length;
        for (var i=0; i<laneCount; i++) {
            var lane = this.lanes[i];
            if (lane.sublanes) {
                var laneHeight = this.getLaneHeight(lane),
                    len = lane.sublanes.length,
                    sublaneHeight = Math.floor(laneHeight / len),
                    offset = 0
                ;
                for (var j=0; j<len; j++) {
                    var sublane = lane.sublanes[j];
                    sublane[laneNameField] = sublane.name;
                    sublane.top = offset;
                    if (sublane.height == null) sublane.height = sublaneHeight;
                    offset += sublane.height;
                }
                lane.height = lane.sublanes.getProperty("height").sum();
            } else {
                lane.height = this.getLaneHeight(lane);
            }
        }

        // apply the lanes as rows in the grid
        this.setData(this.lanes);

        // cache a map of lanes to avoid later array-searches - if the grid is grouped,
        // originalData will be set - if it is, use that, because data will be a Tree
        this.laneMap = (this.originalData || this.data).makeIndex("name");


        //if (this.isDrawn()) this.redraw();

        // refresh current events according to the new set of lanes
        if (!skipRefreshEvents) this.refreshEvents("setLanes");
        return;
    },

    // cache the result of getLaneIndex(), which is called frequently - cleared in setLanes()
    laneIndexCache: {},
    getLaneIndex : function (laneName) {
        // ignore laneIndex caching for the moment - failing in grouped timelines
        //var laneIndex = this.laneIndexCache[laneName];
        var laneIndex = null;
        if (laneIndex == null) {
            //this.logWarn("*** caching getLaneIndex('" + laneName + "')");
            var lane;
            if (isc.isAn.Object(laneName)) lane = laneName;
            else if (this.data) {
                lane = this.laneMap[laneName] ||
                       this.data.find(this.calendar.laneNameField, laneName);
            } else laneIndex = -1;

            if (laneIndex == null) laneIndex = this.getRecordIndex(lane);

            // cache the index because this method runs a lot
            this.laneIndexCache[laneName] = laneIndex;
        }
        return laneIndex;
    },
    getLane : function (laneName) {
        return this.getLaneMap()[laneName];
    },
    getLaneFromPoint : function (x, y) {
        if (y == null) y = this.body.getOffsetY();

        var rowNum = this.getEventRow(y),
            lane = this.getRecord(rowNum)
        ;

        return !this.isGroupNode(lane) ? lane : null;
    },
    getSublaneFromPoint : function (x, y) {
        if (y == null) y = this.body.getOffsetY();

        var rowNum = this.getEventRow(y),
            lane = this.getRecord(rowNum),
            sublanes = lane ? lane.sublanes : null
        ;

        if (!sublanes) return null;

        var rowTop = this.getRowTop(rowNum),
            laneOffset = y - rowTop,
            laneHeight = this.getLaneHeight(lane),
            len = sublanes.length,
            offset = 0
        ;
        for (var i=0; i<len; i++) {
            // needs >= to cater for the pixel at the lane boundary
            if (offset + sublanes[i].height >= laneOffset) {
                return sublanes[i];
            }
            offset += sublanes[i].height;
        }

        return null;
    },

    _scrubDateRange : function () {
        var gran = this.calendar.timelineGranularity;
        if (gran == "month") {
            this.startDate.setDate(1);
        } else if (gran == "week") {
            this.startDate = isc.DateUtil.getStartOf(this.startDate, "w", true, this.calendar.firstDayOfWeek);
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
    //     this.calendar.lanes = newData;
    //     this.invokeSuper(isc.TimelineView, "setData", newData);
    //},
    scrollTimelineTo : function (pos) {
        this.bodies[1] && this.bodies[1].scrollTo(pos);
    },

    setLaneHeight : function (newHeight) {
        this.laneHeight = newHeight;
        this.setCellHeight(newHeight);
        this.refreshEvents("setLaneHeight");
    },

    groupRowHeight: 30,
    getRowHeight : function (record, rowNum) {
        var height = null;
        if (record) {
            if (this.isGroupNode(record)) height = this.groupRowHeight;
            else height = record.height;
        }
        return Math.round(height || this.Super("getRowHeight", arguments));
    },

    setInnerColumnWidth : function (newWidth) {
        this.columnWidth = newWidth;
        this._rebuild(true);
    },

    rangeCriteriaMode: "view",
    setTimelineRange : function (start, end)
    {
        //this.logWarn("*** in view.setTimelineRange() -- " + start.toString() + " -- " + (end ? end.toString() : "no end"));
        var cal = this.calendar;

        this.timelineGranularity = cal.timelineGranularity;
        this.timelineUnitsPerColumn = cal.timelineUnitsPerColumn;
        this.timelineColumnSpan = cal.timelineColumnSpan;

        var colSpan = this.timelineColumnSpan || this._totalGranularityCount || cal.defaultTimelineColumnSpan,
            refreshData = false,
            gran = this.timelineGranularity.toLowerCase(),
            granString = isc.DateUtil.getTimeUnitKey(gran);
        ;

        start = start || this.startDate;
        // move the start date to it's closest previous granularity boundary ("day" by default)
        start = isc.DateUtil.getStartOf(start, granString, false, this.firstDayOfWeek);

        if (!end) {
            // end wasn't passed - if this.endDate is set, use that - otherwise, calculate it
            if (start.getTime() == this.startDate.getTime() && this.endDate) end = this.endDate;
            else end = isc.DateUtil.getAbsoluteDate("+" + (colSpan*this.timelineUnitsPerColumn) + granString, start);
        }

        var criteriaMode = this.rangeCriteriaMode || cal.rangeCriteriaMode;
        if (criteriaMode && criteriaMode != "none") refreshData = true;

        if (start.logicalDate) {
            start = isc.DateUtil.getStartOf(start, granString, false, this.firstDayOfWeek);
        }
        if (end.logicalDate) {
            end = isc.DateUtil.getEndOf(end, granString, false, this.firstDayOfWeek);
        }

        if (isc.DateUtil.compareLogicalDates(start, end) == 0) {
            if (cal.showWeekends == false && cal.dateIsWeekend(start)) {
                cal.showWeekends = true;
                // log that showWeekends was reset to true because the range-dates span less
                // than one day, and it happens to be a weekend day
                this.logWarn("showWeekends was automatically switched on because the dates " +
                    "provided for the timeline spanned less than one day and the day is a " +
                    "weekend."
                );
            }
        }

        // in timelines, visible and period dates are the same
        this.startDate = this.periodStartDate = start.duplicate();
        this.endDate = this.periodEndDate = end.duplicate();

        cal.startDate = start.duplicate();
        cal.endDate = end.duplicate();



        //isc.logWarn('setTimelineRange:' + [timelineGranularity, timelineUnitsPerColumn,
        //        cal.timelineGranularity, cal.timelineUnitsPerColumn]);

        // only update the dateChooser now if it's part of the UI, rather than a popup
        // the second param here causes the dateChooser to set showTimeItem false if the date
        // is a logicalDate, and true otherwise
        //if (cal.showDateChooser) cal.dateChooser.setData(this.startDate, true);

        cal.setDateLabel();

        // if the calendar is autoFetchData and is in mid-draw, don't refreshData here, or we'll
        // get two fetches
        if (cal.autoFetchData && cal._calendarDrawing) refreshData = false;

        // if not yet drawn, set a flag which will cause a field-rebuild at draw time
        if (!this.isDrawn()) {
             this._needsRebuildOnDraw = true;
        } else {
            this._rebuild(refreshData);
        }
    },

    addUnits : function (date, units, granularity) {
        granularity = granularity || this.calendar.timelineGranularity;
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

    getColFromDate : function (date, forEndDate) {
        var fields = this.frozenBody ? this.body.fields : this.getFields(),
            startMillis = (date && date.getTime) ? date.getTime() : date
        ;

        if (date) {
            for (var i=0; i<fields.length; i++) {
                var field = fields[i],
                    fieldTime = field && field.date ? field.date.getTime() : null,
                    fieldEndTime = field && field.endDate ? field.endDate.getTime() : null
                ;
                if (!fieldTime || !fieldEndTime) continue;
                if (forEndDate) {
                    if (startMillis >= fieldTime && startMillis < fieldEndTime) {
                        return i;
                    }
                } else {
                    if (fieldTime >= startMillis) {
                        return i;
                        //return i-1;
                    }
                }
            }
        }
        return null;
    },

    getFieldContainingDate : function (date, nullIfOutOfRange) {
        var fields = this.frozenBody ? this.body.fields : this.getFields(),
            startMillis = (date && date.getTime) ? date.getTime() : date
        ;

        if (startMillis) {
            if (startMillis < this.startDate.getTime()) return fields[0];
            if (startMillis >= this.endDate.getTime()) {
                return nullIfOutOfRange ? null : fields[fields.length - 1];
            }
            for (var i=0; i<fields.length; i++) {
                var field = fields[i],
                    fieldTime = field && field.date ? field.date.getTime() : null,
                    fieldEndTime = field && field.endDate ? field.endDate.getTime() : null
                ;
                if (startMillis >= fieldTime && startMillis <= fieldEndTime) {
                    return field;
                }
            }
        }
        return null;
    },

    // internal attribute that limits the date-loop in calcFields below - just a safeguard in
    // case someone sets a huge startDate/endDate range and a high resolution

    maximumTimelineColumns: 400,
    calcFields : function () {
        var newFields = [],
            cal = this.calendar
        ;


        if (this.showLaneFields != false) {
            var showLaneHovers = this.shouldShowLaneFieldHovers();
            if (cal.laneFields) {
                var laneFields = cal.laneFields;
                for (var i = 0; i < laneFields.length; i++) {
                    var lf = isc.addProperties({}, {
                        hoverDelay: this.hoverDelay+1,
                        hoverMoveWithMouse: true,
                        frozen: true,
                        isLaneField: true,
                        baseStyle: this.getLaneFieldStyleName(laneFields[i], {}),
                        minWidth: this.labelColumnWidth,
                        width: this.labelColumnWidth,
                        canHover: showLaneHovers,
                        showHover: showLaneHovers
                    }, laneFields[i]);
                    newFields.add(lf);
                }
            } else {
                var labelCol = isc.addProperties({
                    autoFitWidth: true,
                    width: this.labelColumnWidth,
                    minWidth: this.labelColumnWidth,
                    baseStyle: this.labelColumnBaseStyle,
                    name: "title",
                    title: " ",
                    showTitle: false,
                    frozen: true,
                    isLaneField: true,
                    hoverDelay: this.hoverDelay+1,
                    hoverMoveWithMouse: true,
                    canHover: showLaneHovers,
                    showHover: showLaneHovers
                });
                newFields.add(labelCol);
            }
        }

        if (!cal.headerLevels && !this.headerLevels) {
            cal.headerLevels = [ { unit: cal.timelineGranularity } ];
        }

        if (cal.headerLevels) {
            this.headerLevels = isc.shallowClone(cal.headerLevels);
        }

        if (this.headerLevels) {
            // we have some header-levels - the innermost level is going to be stripped and its
            // "unit" and "titles" array used for field-headers (unit becomes
            // calendar.timelineGranularity - they should already be the same)
            this.fieldHeaderLevel = this.headerLevels[this.headerLevels.length-1];
            if (this.fieldHeaderLevel.headerWidth != null) {
                // update view.columnWidth here, right before updating the snapGaps
                //this.columnWidth = this.fieldHeaderLevel.headerWidth;
            }
            this.headerLevels.remove(this.fieldHeaderLevel);
            cal.timelineGranularity = this.fieldHeaderLevel.unit;
            // cache a couple of values
            this._cache.innerHeaderLevel = this.fieldHeaderLevel;
            this._cache.granularity = cal.timelineGranularity;
            // this will get called by initCacheValues() below
            //this.updateSnapProperties();
        }


        this.adjustTimelineForHeaders();

        // add date columns to fields
        var sDate = this.startDate.duplicate(),
            eDate = this.endDate.duplicate(),
            units = cal.timelineUnitsPerColumn,
            spanIndex = 0,
            headerLevel = this.fieldHeaderLevel,
            titles = headerLevel && headerLevel.titles ? headerLevel.titles : []
        ;

        //if (headerLevel.headerWidth) this.columnWidth = headerLevel.headerWidth;

        var eDateMillis = eDate.getTime(),
            //colWidth = this.getHeaderButtonWidth(),
            colWidth = this.columnWidth,
            startLeftOffset = 0,
            endLeftOffset = startLeftOffset + colWidth-1,
            daysPerCell = this.getTimePerCell("d"),
            ignoreHiddenDates =  daysPerCell > 1,
            fieldEndDate
        ;


        this._totalGranularityCount = 0;

        var showCellHovers = this.shouldShowCellHovers();
        //var headerButtonWidth = this.getHeaderButtonWidth()
        var headerButtonWidth = colWidth; //this.getHeaderButtonWidth()

        while (sDate.getTime() <= eDateMillis) {
            var thisDate = sDate.duplicate(),
                showDate = ignoreHiddenDates || cal.shouldShowDate(sDate, this)
            ;

            thisDate = isc.DateUtil.getStartOf(thisDate, cal.timelineGranularity,
                false, this.firstDayOfWeek);

            if (thisDate.getTime() >= eDateMillis) break;

            this._totalGranularityCount++;

            fieldEndDate = isc.DateUtil.getEndOf(this.addUnits(sDate.duplicate(), units),
                cal.timelineGranularity, false, this.firstDayOfWeek);

            var newField = null;

            if (fieldEndDate.getTime() > eDateMillis) {
                fieldEndDate.setTime(eDateMillis);
            }

            if (showDate) {
                var title = this.getInnerFieldTitle(headerLevel, spanIndex, sDate);

                newField = isc.addProperties({}, {
                    name: "f" + spanIndex,
                    headerLevel: headerLevel,
                    title: title,
                    width: headerLevel.headerWidth || headerButtonWidth,
                    cellAlign: headerLevel.cellAlign,
                    cellVAlign: headerLevel.cellVAlign,
                    date: thisDate.duplicate(),
                    logicalDate: isc.DateUtil.getLogicalDateOnly(thisDate),
                    logicalTime: isc.DateUtil.getLogicalTimeOnly(thisDate),
                    canGroup: false,
                    canSort: false,
                    canFreeze: false,
                    canFocus: false,
                    startLeftOffset: startLeftOffset,
                    endLeftOffset: endLeftOffset,
                    hoverDelay: this.hoverDelay+1,
                    hoverMoveWithMouse: true,
                    canHover: showCellHovers,
                    showHover: showCellHovers
                }, this.getFieldProperties(thisDate));

                if (cal.shouldDisableDate(thisDate)) {
                    newField.dateDisabled = true;
                }

                //isc.logWarn("field " + spanIndex + ":\n" +
                //    "    " + thisDate + "\n" +
                //    "        " + isc.DateUtil.getLogicalDateOnly(thisDate) + "\n" +
                //    "        " + isc.DateUtil.getLogicalTimeOnly(thisDate) + "\n" +
                //    "    " + fieldEndDate + "\n" +
                //    "        " + isc.DateUtil.getLogicalDateOnly(fieldEndDate) + "\n" +
                //    "        " + isc.DateUtil.getLogicalTimeOnly(fieldEndDate) + "\n"
                //);
            }

            sDate = fieldEndDate.duplicate();

            if (showDate) {
                // store the end date, as the next start date
                newField.endDate = sDate.duplicate();
                newField.endDate.setTime(newField.endDate.getTime()-1);
                newField.logicalEndDate = isc.DateUtil.getLogicalDateOnly(sDate);
                newField.logicalEndTime = isc.DateUtil.getLogicalTimeOnly(sDate),
                newFields.add(newField);
                spanIndex++;
                startLeftOffset += colWidth;
                endLeftOffset += colWidth;
            }

            if (newFields.length >= this.maximumTimelineColumns) {
                this.endDate = sDate.duplicate();
                this.logWarn("Date-range too large - limiting to " +
                        this.maximumTimelineColumns + " columns.");
                break;
            }
        }


        //this._totalGranularityCount--;

        var hoverProps = {
            hoverDelay: this.hoverDelay+1,
            hoverMoveWithMouse: true,
            // canHover is always true, showHover is calculated in getHoverHTML()
            canHover: true,
            getHoverHTML : function () {
                var view = this.grid;
                // only show header-hovers if the view/calendar is configured to do so
                if (!view.shouldShowHeaderHovers()) return;
                return view.calendar._getHeaderHoverHTML(view, view.fieldHeaderLevel,
                    this, this.date, this.endDate);
            }
        };
        for (var i=0, fieldCount=newFields.length; i<fieldCount; i++) {
            var field = newFields[i];
            isc.addProperties(field, hoverProps);
            field.headerLevel = this.fieldHeaderLevel;
        }

        // handle headerLevel.headerWidth being "*" - auto fit date-fields to available space
        if (headerLevel && headerLevel.headerWidth != null) {
            if (headerLevel.headerWidth == "*") {
                if (this.isDrawn() && this.isVisible()) {
                    this._innerHeaderWidth = this.body.getColumnWidth(0);
                } else {
                    this._innerHeaderWidth = Math.floor(this.body.getWidth() / this._totalGranularityCount)
                }
            } else if (isc.isA.Number(headerLevel.headerWidth)) {
                this._innerHeaderWidth = headerLevel.headerWidth;
            }
        } else {
            this._innerHeaderWidth = this.columnWidth;
        }

        this.buildHeaderSpans(newFields, this.headerLevels, this.startDate, this.endDate);

        this.initCacheValues();

        this._dateFieldCount = spanIndex-1;

        // do this in the calling function, after this.fields has been updated
        //this.buildSnapGapList("calcFields");

        return newFields;
    },

    redraw : function () {
        this.Super("redraw", arguments);
        if (!this.animateFolders && this._redrawForGrouping) {
            // _redrawForGrouping is set in groupByComplete() and toggleFolder()
            delete this._redrawForGrouping;
            this.laneHeightCache = {};
            this.laneIndexCache = {};
            this.delayedRefreshVisibleEvents();
        }
    },

    toggleFolder : function (record) {
        this.Super("toggleFolder", arguments);
        // this flag causes redraw() to call refreshVisibleEvents()
        this._redrawForGrouping = true;
        this.markForRedraw();

        // if not animating folders, refresh events now - otherwise, do it when the row
        // animation completes
        //if (!this.animateFolders) {
        //    this._redrawForGrouping = true;
        //    this.markForRedraw();
        //}
    },

    rowAnimationComplete : function (body, hasFrozenBody) {
        this.Super("rowAnimationComplete", arguments);
        // animating folders, refresh events now, if the rowAnimationComplete callback is gone,
        // indicating that both bodies are fully redrawn
        if (!this._rowAnimationCompleteCallback) {

            delete this.body._rowHeights;
            this.refreshVisibleEvents();
        }
    },

    adjustTimelineForHeaders : function () {
        // if we weren't
        var cal = this.calendar,
            unit = this.fieldHeaderLevel ? this.fieldHeaderLevel.unit : cal.timelineGranularity,
            start = this.startDate,
            end = new Date(this.endDate.getTime()-1)
        ;

        // we have at least one header - make sure we start and end the timeline
        // at the beginning and end of the innerLevel's unit-type (the actual field-headers,
        // that is)
        var key = isc.DateUtil.getTimeUnitKey(unit);

        this.startDate = isc.DateUtil.getStartOf(start, key, false, this.firstDayOfWeek);
        this.endDate = isc.DateUtil.getEndOf(end, key, false, this.firstDayOfWeek);
    },

    buildHeaderSpans : function (fields, levels, startDate, endDate) {
        var date = startDate.duplicate(),
            c = this.calendar,
            result = [],
            spans = []
        ;

        if (levels && levels.length > 0) {
            spans = this.getHeaderSpans(startDate, endDate, levels, 0, fields);
            this.headerHeight = this._headerHeight + (levels.length * this.headerSpanHeight);
        }

        if (spans && spans.length > 0) {
            this.setHeaderSpans(spans, true);
        }
    },

    getHeaderSpans : function (startDate, endDate, headerLevels, levelIndex, fields) {
        var date = startDate.duplicate(),
            c = this.calendar,
            headerLevel = headerLevels[levelIndex],
            unit = headerLevel.unit,
            unitKey = isc.DateUtil.getTimeUnitKey(unit),
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

        var firstLoop = true;
        while (date <= endDate) {
            date = DU.dateAdd(date, "mn", 1, 1);
            if (firstLoop) {
                firstLoop = false;
                var newDate = isc.DateUtil.getEndOf(date, unitKey, false, c.firstDayOfWeek);
            } else {
                var newDate = this.addUnits(date.duplicate(), unitsPerColumn, unit);
            }

            var span = { unit: unit,
                hoverDelay: this.hoverDelay+1,
                hoverMoveWithMouse: true,
                // canHover is always true, showHover is calculated in getHoverHTML()
                canHover: true,
                canFocus: false,
                headerLevel: headerLevel,
                getHoverHTML : function () {
                    var view = this.creator;
                    // only show header-hovers if the view/calendar is configured to do so
                    if (!view.shouldShowHeaderHovers()) return;
                    return view.calendar._getHeaderHoverHTML(view, this.headerLevel, this,
                        this.startDate, this.endDate
                    );
                }
            };
            span.startDate = date.duplicate();
            span.endDate = newDate.duplicate();

            this.setSpanDates(span, date.duplicate());

            newDate = span.endDate;

            var title = this.getHeaderLevelTitle(headerLevel, spanIndex, date, newDate);

            span.title = title;

            // this condition should be re-introduced once LG supports multiple-headers where
            // only the inner-most spans require a fields array
            //if (levelIndex == headerLevels.length-1) {
                span.fields = [];
                for (var i=0; i<fields.length; i++) {
                    var field = fields[i];
                    if (field.isLaneField || field.date < span.startDate) continue;
                    if (field.date >= span.endDate) break;
                    field.headerLevel = headerLevels[levelIndex];
                    span.fields.add(field.name);
                }
            //}

            if (levelIndex < headerLevels.length-1) {
                span.spans = this.getHeaderSpans(span.startDate, span.endDate,
                    headerLevels, levelIndex + 1, fields);
                if (span.spans && span.spans.length > 0) span.fields = null;
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
                title = startDate.getFullYear() + " - " + startDate.getFullYear();
            } else if (unit == "year") {
                title = startDate.getFullYear();
            } else if (unit == "quarter") {
                title = startDate.getShortMonthName() + " - " + endDate.getShortMonthName();
            } else if (unit == "month") {
                title = startDate.getShortMonthName();
            } else if (unit == "week") {
                // use the week number for the Date.firstWeekIncludesDay'th day of the week - thursday
                var midWeek = isc.DateUtil.getStartOf(startDate, "W", false, this.calendar.firstDayOfWeek);
                midWeek.setDate(midWeek.getDate() + (midWeek.firstWeekIncludesDay - this.calendar.firstDayOfWeek));
                title = this.calendar.weekPrefix + " " + midWeek.getWeek(this.calendar.firstDayOfWeek);
            } else if (unit == "day") {
                title = startDate.getShortDayName();
            } else {
                if (unit == "hour") title = startDate.getHours();
                if (unit == "minute") title = startDate.getMinutes();
                if (unit == "second") title = startDate.getSeconds();
                if (unit == "millisecond") title = startDate.getMilliseconds();
                if (unit == "hour") title = startDate.getHours();
            }
            title = "" + title;
            if (isc.isA.Function(headerLevel.titleFormatter)) {
                title = headerLevel.titleFormatter(headerLevel, startDate, endDate, title, this.calendar);
            }
        }
        return title;

    },

    setSpanDates : function (span, date) {
        var key = isc.DateUtil.getTimeUnitKey(span.unit);
        var cal = this.calendar;

        span.startDate = isc.DateUtil.getStartOf(date, key, false, this.firstDayOfWeek);
        span.endDate = isc.DateUtil.getEndOf(span.startDate, key, false, this.firstDayOfWeek);
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
                // use the week number for the Date.firstWeekIncludesDay'th day of the week - thursday
                var midWeek = isc.DateUtil.getStartOf(startDate, "W", null, this.calendar.firstDayOfWeek);
                midWeek.setDate(midWeek.getDate() + (midWeek.firstWeekIncludesDay - this.calendar.firstDayOfWeek));
                result = this.calendar.weekPrefix + " " + midWeek.getWeek(this.calendar.firstDayOfWeek);
            } else if (granularity == "day") {
                // get the order of date parts from the inputFormat and use the defaultDateSeparator
                var format = isc.DateUtil.getInputFormat();
                if (isc.isA.String(format)) {
                    // remove year and transform day
                    format = format.replace("Y", "");
                    format = format.replace("D", "d");
                    format = format[0] + isc.DateUtil.getDefaultDateSeparator() + format[1];
                    result = isc.DateUtil.format(startDate, format);
                } else {
                    result = (startDate.getMonth() + 1) + "/" + startDate.getDate();
                }
            } else {
                var date = startDate.duplicate();
//                date.setMinutes(date.getMinutes() + offsetMin);
                var mins = date.getMinutes().toString();
                if (mins.length == 1) mins = "0" + mins;
                result = date.getHours() + ":" + mins;
            }
            if (isc.isA.Function(headerLevel.titleFormatter)) {
                result = headerLevel.titleFormatter(headerLevel, startDate, endDate, result, this.calendar);
            }
        }

        return result;
    },

    // this method is fired from an observation of grid._updateFieldWidths() - if the known
    // width of the first field has changed since it was set up in calcFields(), clear the
    // events visually, reinit the cache values (column-widths, etc), rebuild the snapGapList
    // and then fire refreshVisibleEvents on a delay to resize/reposition/redraw visible events
    refreshEventsForBodyResize : function () {
        if (this._innerHeaderWidth != this.body._fieldWidths[0] && this.isDrawn() && this.isVisible()) {
            this._innerHeaderWidth = this.body._fieldWidths[0];
            this.clearEvents();
            this.initCacheValues();
            this.buildSnapGapList();
            //this.delayedRefreshEvents();
            this.delayedRefreshVisibleEvents();
        }
    },

    draw : function (a, b, c, d) {
        this.invokeSuper(isc.TimelineView, "draw", a, b, c, d);

        // observe _updateFieldWidths() in order to resize/reposition visible events when the
        // field-sizes change
        if (!this.isObserving(this, "_updateFieldWidths")) {
            this.observe(this, "_updateFieldWidths", "observer.refreshEventsForBodyResize();");
        }

        var cal = this.calendar;

        this.logDebug('draw', 'calendar');
        // call refreshEvents() whenever we're drawn
        // see comment above dataChanged for the logic behind this

        this.body.addChild(this.eventDragCanvas, null, false);
        this.eventDragCanvas.setView(this);

        if (this._needsRebuildOnDraw) {
            // flag set at the end of setTimelineRange() if it runs before draw
            delete this._needsRebuildOnDraw;
            this._rebuild();
        }

        if (this._refreshEventsOnDraw) {
            delete this._refreshEventsOnDraw;
            //this._refreshEvents();
            //this.setChosenDate(this.calendar.chosenDate);
        }

        if (this._groupOnDraw) {
            // set up grouping based on the laneGroupBy settings on Calendar
            this.canGroupBy = true;
            this.groupByField = cal.laneGroupByField;
            if (cal.laneGroupStartOpen != null) this.groupStartOpen = cal.laneGroupStartOpen;

            this._pendingGroupComplete = true;
            this.delayCall("groupBy", [this.groupByField], 0);
        }

        //this.refreshEvents();
        if (this._fireViewChangedOnDraw) {
            delete this._fireViewChangedOnDraw;
            this.calendar.currentViewChanged(this.viewName);
        }

    },

    groupByComplete : function () {
        // clear the groupOnDraw flag
        delete this._groupOnDraw;
        // these flags are set in draw() and cleared here - refreshVisibleEvents() will bail if
        // it runs while this flag is set (while the grouping is running)
        delete this._pendingGroupComplete;

        this.delayedRefreshVisibleEvents();
        return;

        //// this flag is checked in timelineView.redraw() - if it's set, redraw() will call
        //// refreshVisibleEvents() to reposition events in case their lanes have moved
        //this._redrawForGrouping = true;
        //// cause a redraw() to update the events
        //this.markForRedraw();
    },

    formatDateForDisplay : function (date) {
        return  date.getShortMonthName() + " " + date.getDate() + ", " + date.getFullYear();
    },

    getLabelColCount : function () {
        var cal = this.calendar,
            count = 1
        ;
        if (cal.laneFields) {
            count = 0;
            for (var i=0; i<cal.laneFields.length; i++) {
                if (cal.laneFields[i].hidden != true) count++;
            }
        }
        return count;
    },

    isLabelCol : function (colNum) {
        var field = this.getField(colNum);
        return field && field.frozen;
    },

    showField : function () {
        this.Super("showField", arguments);
        this.refreshEvents("showField");
    },
    hideField : function () {
        this.Super("hideField", arguments);
        this.refreshEvents("hideField");
    },

    getLaneStyleName : function (lane) {
        if (lane && lane.styleName) return lane.styleName;
    },
    getLaneFieldStyleName : function (field, lane) {
        if (field && field.styleName) return field.styleName;
        if (lane && lane.fieldStyleName) return lane.fieldStyleName;
        return this.labelColumnBaseStyle;
    },

    // timelineView
    getBaseStyle : function (record, rowNum, colNum) {
        var cal = this.calendar;
        // for group rows, return the baseStyle
        if (record._isGroup) return this.groupNodeBaseStyle;
        // for frozen-fields in lanes with a fieldStyleName, return fieldStyleName
        if (record.fieldStyleName && this.frozenFields) {
            if (this.fieldIsFrozen(colNum)) return record.fieldStyleName;
        }
        return this.Super("getBaseStyle", arguments);
    },
    getCellStyle : function (record, rowNum, colNum) {
        var cal = this.calendar,
            field = this.getField(colNum),
            body = this.getFieldBody(colNum) || this.getBody(),
            isOver = rowNum == body.lastOverRow,
            isSelected = this.isSelected(record),
            gridDisabled = this.isDisabled(),
            cellDisabled = (field && field.date && cal.shouldDisableDate(field.date))
        ;

        // get the baseStyle
        var bStyle = this.getBaseStyle(record, rowNum, colNum);

        // append Disabled suffix
        if (gridDisabled || (cellDisabled && !isSelected)) {
            bStyle += "Disabled";
        }

        // append Selected suffix
        if (isSelected) bStyle += "Selected";

        // append Over suffix
        if (cal.showLaneRollOver && isOver) bStyle += "Over";

        // odd rows in vertical views
        if (this.alternateRecordStyles && rowNum % 2 != 0) {
            // odd row in TimelineView, with alternateRecordStyles
            bStyle += "Dark";
        }

        return bStyle;
    },

    slideRange : function (slideRight) {
        var c = this.calendar,
            gran = this.timelineGranularity.toLowerCase(),
            granString = isc.DateUtil.getTimeUnitKey(gran),
            units = this.timelineUnitsPerColumn || 1,
            startDate = this.startDate.duplicate(),
            endDate = this.endDate.duplicate(),
            multiplier = slideRight ? 1 : -1,
            scrollCount = c.columnsPerPage || (this.getFields().length - this.getLabelColCount())
        ;

        startDate = isc.DateUtil.dateAdd(startDate, granString, scrollCount * units, multiplier,
            false, null, this.firstDayOfWeek);
        startDate = isc.DateUtil.getStartOf(startDate, granString, false, this.firstDayOfWeek);

        // this flag prevents _rebuild() from updating the lanes - only dates have changed
        this._skipSetLanes = true;

        // call the usual setChosenDate(), which deals with everything
        // call on the parent calendar, so it's chosenDate is properly updated
        this.calendar.setChosenDate(startDate);
        delete this._skipSetLanes;
    },

    nextOrPrev : function (next) {
        this.slideRange(next);
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
        var cal = this.calendar;

        if (x == null && y == null) {
            // if no co-ords passed, assume mouse offsets into the body
            x = this.body.getOffsetX();
            //y = this.body.getOffsetY();
        }

        var snapData = this.getSnapData(x, null, null, true);
        if (snapData) {
            if (snapData.nextValidSnap) {
                // it's a left offset, so if the snap is hidden, use the start offset of
                // the next good snap
                return snapData.nextValidSnap[cal.startDateField].duplicate();
            } else if (snapData.lastValidSnap) {
                // there's no valid next snap, use the previous one if it's there
                return snapData.lastValidSnap[cal.endDateField].duplicate();
            }
            return snapData[cal.endDateField].duplicate();
        }

        if (x < 0 || y < 0) return null;

        // get the colNum *before* catering for useSnapGap
        var colNum = this.body.getEventColumn(x);
        if (colNum == -2) colNum = this.body.fields.length-1;
        if (colNum == -1) return null;

        if (useSnapGap == null) useSnapGap = true;

        var snapGapPixels = Math.max(cal.getSnapGapPixels(this), 1);
        if (useSnapGap) {
            // when click/drag creating, we want to snap to the eventSnapGap
            var r = x % snapGapPixels;
            if (r) x -= r;
        }

        var date = this.body.fields[colNum].date,
            colLeft = this.body.getColumnLeft(colNum),
            delta = x - colLeft,
            snapGaps = Math.floor(delta / snapGapPixels)
        ;
        if (snapGaps) date = cal.addSnapGapsToDate(date.duplicate(), this, snapGaps);
        return date;
    },

    // gets the width that the event should be sized to in pixels
    _getEventBreadth : function (event, exactBreadth) {
        var props = event && event["_" + this.viewName];
        if (props) {
            if (exactBreadth && props.exactBreadth) return props.exactBreadth;
            if (!exactBreadth && props.snapBreadth) return props.snapBreadth;
        }

        // this method should now use two calls to getDateLeftOffset() to get start and end
        // X offset, and the breadth is the pixel delta - this allows events to span arbitrary
        // hidden columns, while still rendering events that span the gap between the two dates
        var cal = this.calendar,
            eventStart = cal.getEventStartDate(event),
            eventEnd = cal.getEventEndDate(event),
            visibleStart = cal.getVisibleStartDate(this),
            visibleEnd = isc.DateUtil.adjustDate(cal.getVisibleEndDate(this), "-1ms");
        ;
        if (eventStart.getTime() < visibleStart.getTime()) eventStart = visibleStart;
        if (eventEnd.getTime() > visibleEnd.getTime()) eventEnd = visibleEnd;

        var eventLeft = this.getDateLeftOffset(eventStart, null, exactBreadth),
            eventRight = this.getDateRightOffset(eventEnd, exactBreadth),
            newBreadth = eventRight - eventLeft
        ;

        if (props) {
            if (exactBreadth) props.exactBreadth = newBreadth;
            else props.snapBreadth = newBreadth;
        }
        return newBreadth;
    },

    getDateRightOffset : function (date, exactOffset) {
        if (!date) return 0;

        var snapData = this.getSnapData(null, null, date, true);
        var leftOffset = 0;
        if (snapData) {
            if (snapData.lastValidSnap) {
                // it's a right offset, so if the snap is hidden, use the end offset of
                // the last good snap
                leftOffset = snapData.lastValidSnap.endLeftOffset;
            } else leftOffset = snapData.endLeftOffset;
        }
        return leftOffset;
    },
    // getDateLeftOffset timelineView
    getDateLeftOffset : function (date, useNextSnapGap, exactOffset) {

        if (!date) return 0;

        var snapData = this.getSnapData(null, null, date, true);
        if (snapData) {
            if (snapData.nextValidSnap) {
                // it's a left offset, so if the snap is hidden, use the start offset of
                // the next good snap
                return snapData.nextValidSnap.startLeftOffset;
            } else if (snapData.lastValidSnap) {
                // there's no valid next snap, use the previous one if it's there
                return snapData.lastValidSnap.endLeftOffset;
            }
            return snapData.startLeftOffset;
        }

        var visibleStartMillis = this.calendar.getVisibleStartDate(this).getTime();
        var visibleEndMillis = this.calendar.getVisibleEndDate(this).getTime();

        var millis = isc.isA.Number(date) ? date : date.getTime();
        if (millis <= visibleStartMillis) millis = visibleStartMillis + 1;
        if (millis >= visibleEndMillis) millis = visibleEndMillis;

        var cal = this.calendar,
            snapGapPixels = cal.getSnapGapPixels(this),
            snapMins = cal.getSnapGapMinutes(this)
        ;


        var fields = this.body.fields,
            len = fields.getLength(),
            mins = Math.floor(millis / 60000),
            colWidth = this.body.getColumnWidth(0),
            cellMins = this.getTimePerCell("mn")
        ;


        for (var i=0; i<len; i++) {
            var field = fields[i];
            //if (!this.fieldIsVisible(field)) continue;

            var startMillis = field.date.getTime(),
                endMillis = field.endDate.getTime(),
                startMins = Math.floor(field.date.getTime() / 60000),
                endMins = Math.floor(field.endDate.getTime() / 60000)
            ;
            if (mins == endMins) {
                return this.body.getColumnLeft(i) + colWidth;
            } else if (mins < endMins) {
                if (mins == startMins) {
                    return this.body.getColumnLeft(i);
                } else if (mins > startMins) {
                    // passed date is within this field - now get the snap point
                    var columnLeft = (colWidth * i),
                        deltaMins = mins - startMins,
                        snapsToAdd = Math.floor(deltaMins / snapMins),
                        extraMins = deltaMins % snapMins
                    ;
                    if (useNextSnapGap) {
                        // useNextSnapGap is passed in by getDateRightOffset() - if it's set
                        // and passed date is after the last snapGap, use the next one
                        if (extraMins > 0 || deltaMins < snapMins) snapsToAdd++;
                    }
                    var left = columnLeft + Math.round((snapsToAdd * snapGapPixels));
                    if (exactOffset) left += Math.round(cal.getMinutePixels(extraMins, null, this));
                    return left;
                } else {
                    // passed date should have been in the previous field, but that field is
                    // clearly hidden - just return the left offset of this field
                    return (colWidth * i);
                }
            }
        }

        return -1;
    },

    // getEventLeft timelineView
    getEventLeft : function (event) {
        return this.getDateLeftOffset(this.calendar.getEventStartDate(event));
    },
    getEventRight : function (event) {
        return this.getDateRightOffset(this.calendar.getEventEndDate(event));
    },

    // cache the result of getLaneHeight(), which is called frequently - cleared in setLanes()
    laneHeightCache: {},
    getLaneHeight : function (lane) {
        var laneName = isc.isA.String(lane) ? lane : (isc.isAn.Object(lane) ? lane.name : null);
        // lane-height is from lane.height or grid.cellHeight
        var laneHeight = (lane && lane.height) || this.cellHeight;

        if (!laneName) {
            if (isc.isA.Number(lane)) lane = this.getRecord(lane);
            if (lane) {
                laneName = lane.name;
                if (lane.height != null) laneHeight = lane.height;
                lane = null;
            }
        }

        var cacheHeight = this.laneHeightCache[laneName];
        if (cacheHeight == null) {
            //this.logWarn("*** caching getLaneHeight('" + laneName + "')");
            this.laneHeightCache[laneName] = laneHeight;
        }
        return this.laneHeightCache[laneName];
    },
    getSublaneHeight : function (sublane, lane) {
        if (!isc.isAn.Object(sublane)) {
            if (!lane || !lane.sublanes) return null;
            if (isc.isA.Number(sublane)) sublane = lane.sublanes[sublane];
            else if (isc.isA.String(sublane)) {
                sublane = lane.sublanes.find(this.calendar.laneNameField, sublane);
            }
        }
        return sublane ? sublane.height : null;
    },

    getEditDialogPosition : function (event) {
        var cal = this.calendar,
            colNum = this.getColFromDate(cal.getEventStartDate(event), event[cal.laneNameField]),
            rowNum = this.getEventLaneIndex(event),
            rect = this.body.getCellPageRect(rowNum, colNum)
        ;
        return {
            left: Math.max(rect[0], this.body.getPageLeft()),
            top: Math.max(rect[1], this.body.getPageTop())
        };
    }

}); // end timelineView addProperties()

isc.DaySchedule.addClassProperties({


    _getEventScaffolding : function (view) {
        var minsPerRow = view.getTimePerCell(),
            rowCount = (60 / minsPerRow) * 24,
            data = [],
            row = {label:"", day1:"", day2:"", day3:"", day4:"", day5:"", day6:"", day7:""}
        ;

        isc.DaySchedule._getCellDates(view);

        var logicalDate = view.logicalDate.duplicate(),
            logicalTime = isc.DateUtil.createLogicalTime(0, 0, 0)
        ;

        for (var i=0; i<rowCount; i++) {
            var time = isc.DateUtil.combineLogicalDateAndTime(logicalDate, logicalTime);
            var rec = data.add(isc.addProperties({}, row));
            rec.time = time.duplicate();
            rec.logicalTime = logicalTime.duplicate();
            rec.logicalDate = logicalDate.duplicate();
            logicalTime.setMinutes(logicalTime.getMinutes() + minsPerRow);
            //this.logWarn("ROW " + i + " UPDATE: " + isc.echoFull(rec));
        }

        return data;
    },




    _getCellDates : function (view) {
        var startDate = view.startDate || new Date();
        var minsPerRow = view.getTimePerCell(),
            rowCount = (60 / minsPerRow) * 24,
            counter = view.isDayView() ? 1 : 7,
            cellDates = []
        ;

        var logicalDate = view.logicalDate.duplicate();

        var date = isc.DateUtil.createDatetime(logicalDate.getFullYear(), logicalDate.getMonth(),
                    logicalDate.getDate(), 0, 0, 0, 0);

        view._dstCells = null;

        for (var j=0; j < counter; j++) {
            var cellDate = date.duplicate();
            for (var i=0; i<rowCount; i++) {
                if (j == counter-1 && i == rowCount) break;
                if (!cellDates[i]) cellDates[i] = {};
                // store the dates in object properties rather than an array - makes life easier
                // in the week view when weekends aren't visible
                cellDates[i]["day" + (j+1)] = cellDate.duplicate();
                cellDates[i].logicalTime = isc.DateUtil.getLogicalTimeOnly(cellDate);

                // when calculating the times for cells, add rows*mins to the column's base
                // datetime - then create a logicalTime with the same offset
                var minsToAdd = minsPerRow * (i + 1);
                var newCellDate = isc.DateUtil.dateAdd(date, "mn", minsToAdd, 1);
                var newTime = isc.DateUtil.getLogicalTimeOnly(newCellDate, true);

                // use the logicalTime to cater for custom display timezone
                newTime.setTime(newTime.getTime() + (minsToAdd*60000));
                cellDates[i]["day" + (j+1) + "_end"] = isc.DateUtil.dateAdd(newCellDate, "ms", -1);
                cellDates[i].logicalEndTime = isc.DateUtil.getLogicalTimeOnly(cellDates[i]["day" + (j+1) + "_end"]);
                // compare the newTime (which is a logical time and not subject to DST) with the
                // time portion of the next calculated cellDate - if they're different, the cell's
                // datetime falls during the DST crossover
                var newDate_temp = cellDate.getDate(),
                    newCellDate_temp = newCellDate.getDate(),
                    newHours = newTime.getHours(),
                    newMinutes = newTime.getMinutes(),
                    // use the logicalTime to cater for custom display timezone
                    cellTime = isc.Date.getLogicalTimeOnly(newCellDate, true),
                    cellHours = cellTime.getHours(),
                    cellMinutes = cellTime.getMinutes()
                ;


                view.calendar.ignoreDST = true;
                if (view.calendar.ignoreDST) {
                    cellDate = newCellDate.duplicate();
                } else {
                    if (newHours != cellHours || newMinutes != cellMinutes) {
                        // the time portion of the parsed date doesn't match the logical time -
                        // this time must be involved in the DST crossover - use whatever was the
                        // time when they were last the same and store off the cell in question
                        // so it can be disabled in the UI
                        if (!view._dstCells) view._dstCells = [];
                        view._dstCells.add({ rowNum: i+1, colNum: j });
                    } else {
                        cellDate = newCellDate.duplicate();
                    }
                }

                cellDates[i]["day" + (j+1) + "_logicalDate"] = logicalDate.duplicate();
            }
            date = isc.DateUtil.dateAdd(date, "d", 1);
            logicalDate = isc.DateUtil.dateAdd(logicalDate, "d", 1);
        }

        view._cellDates = cellDates;
        return cellDates;
    }

});






//> @class Calendar
// The Calendar component provides several different ways for a user to view and
// edit a set of events. Note that the standard Calendar module must be loaded to make use of
// the Calendar class.
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
// DataSource.  When this is the case, if the DataSource
// does not contain fields with the configured property names, an attempt is made to
// auto-detect likely-looking fields from those that are present.  To see logs indicating that
// this has happened, switch default logging preferences to INFO level in the Developer Console.
// <P>
// If the calendar is bound to a DataSource, event changes by user action or by
// calling methods will be saved to the DataSource.
// <P>
// <b>Navigation</b>
// <P>
// The calendar supports a number of views by default: +link{calendar.dayView,day},
// +link{calendar.weekView,week}, +link{calendar.monthView,month} and
// +link{calendar.timelineView, timeline}.  The user can navigate using back and forward
// buttons or via an attached +link{calendar.dateChooser,DateChooser}.
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
// Events can also be programmatically +link{calendar.addCalendarEvent,added},
// +link{calendar.removeEvent,removed}, or +link{calendar.updateCalendarEvent,updated}.
//
// @inheritsFrom Canvas
// @implements DataBoundComponent
// @treeLocation  Client Reference/Calendar
// @example simpleCalendar
// @visibility calendar
//<
isc.ClassFactory.defineClass("Calendar", "Canvas", "DataBoundComponent");

isc.Calendar.addProperties({

defaultWidth: "100%",
defaultHeight: "100%",

// we don't want the Calendar as a whole to overflow
overflow: "hidden",

year:new Date().getFullYear(),  // full year number
month:new Date().getMonth(),    // 0-11

//> @attr calendar.chosenDate (Date : 'Today' : IRW)
// The date for which events are displayed in the day, week, and month tabs of
// the calendar.  Default is today.
//
// @group date
// @visibility calendar
//<

//> @attr calendar.firstDayOfWeek (Number : null : IRW)
// The numeric day (0-6, Sunday-Saturday) which the calendar should consider as the first
// day of the week in multi-day views, and in the popup +link{calendar.dateChooser, DateChooser}.
// <P>
// If unset, the default is taken from the current locale.
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
// <P>
// See +link{group:cellStyleSuffixes} for details on how stateful suffixes are combined with the
// base style to generate stateful cell styles.
//
// @group appearance
// @visibility calendar
//<
baseStyle: "calendar",

//> @attr calendar.dayHeaderBaseStyle  (CSSStyleName : "calMonthDayHeader" : IRW)
// The base name for the CSS class applied to the day headers of the month view.
// This style will have "Dark", "Over", "Selected", or "Disabled"
// appended to it according to the state of the cell.
// <P>
// See +link{group:cellStyleSuffixes} for details on how stateful suffixes are combined with the
// base style to generate stateful cell styles.
//
// @group appearance
// @visibility calendar
//<
dayHeaderBaseStyle: "calMonthDayHeader",

//> @attr calendar.dayBodyBaseStyle  (CSSStyleName : "calMonthDayBody" : IRW)
// The base name for the CSS class applied to the day body of the month view
// of the calendar. This style will have "Dark", "Over", "Selected", or "Disabled"
// appended to it according to the state of the cell.
// <P>
// See +link{group:cellStyleSuffixes} for details on how stateful suffixes are combined with the
// base style to generate stateful cell styles.
//
// @group appearance
// @visibility calendar
//<
dayBodyBaseStyle: "calMonthDayBody",

//> @attr calendar.otherDayHeaderBaseStyle  (CSSStyleName : "calMonthDayHeader" : IRW)
// The base name for the CSS class applied to the day headers of the month view.
// This style will have "Dark", "Over", "Selected", or "Disabled"
// appended to it according to the state of the cell.
// <P>
// See +link{group:cellStyleSuffixes} for details on how stateful suffixes are combined with the
// base style to generate stateful cell styles.
//
// @group appearance
// @visibility calendar
//<
otherDayHeaderBaseStyle: "calMonthOtherDayHeader",

//> @attr calendar.otherDayBodyBaseStyle  (CSSStyleName : "calMonthOtherDayBody" : IRW)
// The base name for the CSS class applied to the day body of the month view
// of the calendar. This style will have "Dark", "Over", "Selected", or "Disabled"
// appended to it according to the state of the cell.
// <P>
// See +link{group:cellStyleSuffixes} for details on how stateful suffixes are combined with the
// base style to generate stateful cell styles.
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

//> @attr calendar.minimumDayHeight (Integer : 80 : IRW)
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

//> @attr calendar.showDayView (Boolean : true : IR)
// Whether to show the Day view.
//
// @group appearance
// @visibility calendar
//<
showDayView: true,

//> @attr calendar.showWeekView (Boolean : true : IR)
// Whether to show the Week view.
//
// @group appearance
// @visibility calendar
//<
showWeekView: true,

//> @attr calendar.showMonthView (Boolean : true : IR)
// Whether to show the Month view.
//
// @group appearance
// @visibility calendar
//<
showMonthView: true,

//> @attr calendar.selectedCellStyle  (CSSStyleName : "calendarCellSelected" : IRW)
// The base name for the CSS class applied to a cell that is selected via a mouse drag.
//
// @group appearance
// @visibility calendar
//<
selectedCellStyle: "calendarCellSelected",

//> @attr calendar.eventWindowStyle  (CSSStyleName : null : IRW)
// The base name for the CSS class applied to event windows within calendars.
// This style will have "Header", "HeaderLabel", and "Body" appended to it, according to
// which part of the event window is being styled. For example, to style the header, define
// a CSS class called 'eventWindowHeader'.
//
// @group appearance
// @visibility calendar
// @deprecated in favor of +link{calendar.eventStyleName}
//<

//> @attr calendar.eventStyleName  (CSSStyleName : "eventWindow" : IRW)
// The base name for the CSS class applied to +link{calendar.eventCanvas, events} when they're
// rendered in calendar views.
// This style will have "Header" and "Body" appended to it, according to
// which part of the event window is being styled. For example, to style the header, define
// a CSS class called 'eventWindowHeader'.
//
// @group appearance
// @visibility calendar
//<
eventStyleName: "eventWindow",


//> @attr calendar.calMonthEventLinkStyle  (CSSStyleName : "calMonthEventLink" : IRW)
// The base name for the CSS class applied to the links rendered by +link{calendar.getDayBodyHTML}.
// <P>
// These links are rendered as plain HTML links using A elements, and the CSS style in the
// provided skins references the pseudo-classes :link, :visited, :active, :hover.
// <BR>
// Even though it goes against the general policy of not exposing the HTML structures SC writes out
// and not relying on them for styling, applying style to these particular selectors is acceptable,
// as we're unlikely to use any other kind of HTML structure than a link.
//
// @group appearance
// @visibility calendar
//<
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

//> @attr calendar.showWorkday (Boolean : false : IRW)
// When set to true, this setting enables various features related to cells that fall within
// the workday (as defined by +link{workdayStart} and +link{workdayEnd}) in vertical calendar
// views (+link{dayView, day} and +link{weekView, week}).  Workday cells can be
// +link{workdayBaseStyle, styled separately} and +link{sizeToWorkday, sized automatically},
// and users can be prevented from scrolling the calendar beyond the
// +link{limitToWorkday, workday hours}.
// <P>
// The hours of the workday can be customized for particular dates by providing implementations
// of +link{getWorkdayStart} and +link{getWorkdayEnd}.
// @see calendar.styleWorkday
// @see calendar.scrollToWorkday
// @see calendar.sizeToWorkday
// @see calendar.limitToWorkday
// @group workday
// @visibility calendar
//<
showWorkday: false,

//> @attr calendar.styleWorkday (Boolean : true : IRW)
// When +link{calendar.showWorkday, showWorkday} is true, applies the +link{workdayBaseStyle}
// to cells that fall within the workday (as defined by +link{workdayStart} and +link{workdayEnd}),
// in both the +link{dayView} and +link{weekView}.
// <P>
// The hours of the workday can be customized for particular dates by providing implementations
// of +link{getWorkdayStart} and +link{getWorkdayEnd}.
// @see calendar.scrollToWorkday
// @see calendar.sizeToWorkday
// @see calendar.limitToWorkday
// @group workday
// @visibility calendar
//<
styleWorkday: true,

//> @attr calendar.workdays (Array : [1,2,3,4,5] : IR)
// Array of days that are considered workdays when +link{showWorkday} is true.
// <smartclient>Has no effect if +link{dateIsWorkday} is implemented.</smartclient>
// @group workday
// @visibility calendar
//<
workdays: [1, 2, 3, 4, 5],

//> @attr calendar.scrollToWorkday (Boolean : false : IRW)
// If set, and +link{calendar.showWorkday, showWorkday} is true, automatically scrolls the
// +link{calendar.dayView, day} and +link{calendar.weekView, week} views to the start of the
// +link{workdayStart,workday} when the calendar is first displayed and whenever the user
// changes to a different day or week.
// @see calendar.styleWorkday
// @see calendar.sizeToWorkday
// @see calendar.limitToWorkday
// @group workday
// @visibility calendar
//<
scrollToWorkday: false,

//> @attr calendar.sizeToWorkday (Boolean : false : IRW)
// When +link{calendar.showWorkday, showWorkday} is true, attempt to resize rows in the
// day and week views so that the +link{workdayStart,workday hours} fill the visible
// viewport-height, and the whole workday is visible without scrolling.  If the Calendar is
// resized, the row-size is recalculated to keep the workday hours visible.
// <P>
// Note that row-heights will not shrink below the +link{minRowHeight}, so the entire workday
// may not be visible without scrolling if the workday is long or the viewport-height is
// insufficient.
// @see calendar.styleWorkday
// @see calendar.scrollToWorkday
// @see calendar.limitToWorkday
// @see calendar.minRowHeight
// @group workday
// @visibility calendar
//<
sizeToWorkday: false,

//> @attr calendar.limitToWorkday (Boolean : false : IR)
// When +link{calendar.showWorkday, showWorkday} is true, this attribute prevents the user from
// scrolling vertical views beyond the specified workday +link{workdayStart,start} and
// +link{workdayEnd, end} hours.
// @see calendar.styleWorkday
// @see calendar.sizeToWorkday
// @see calendar.scrollToWorkday
// @see calendar.minRowHeight
// @group workday
// @visibility calendar
//<
limitToWorkday: false,

//> @attr calendar.minutesPerRow (Integer : 30 : IR)
// The number of minutes per row in +link{calendar.dayView, day} and
// +link{calendar.weekView, week} views.  The default of 30 minutes shows two rows per hour.
// Note that this value must divide into 60.
//
// @visibility calendar
//<
minutesPerRow: 30,
getMinutesPerRow : function (view) {
    view = view || this.getSelectedView();
    if (view && view.verticalEvents) return view.getTimePerCell("mn");
    return null;
},

//> @attr calendar.rowTitleFrequency (Integer : 60 : IR)
// A minute value that indicates which rows should show times in vertical views, like
// +link{calendar.dayView, day} and +link{calendar.weekView, week}.  The default of 60 minutes
// shows titles on the first row of each hour.  The value provided must be a multiple of
// +link{calendar.minutesPerRow, minutesPerRow} and be no larger than 60.
//
// @visibility calendar
//<
rowTitleFrequency: 60,

getMinutesPerCol : function (view) {
    view = view || this.getSelectedView();
    if (view && !view.verticalEvents) return view.getTimePerCell("mn");
    return null;
},

getSnapGapMinutes : function (view, rowNum, colNum) {
    view = view || this.getSelectedView();
    if (view) return view.getTimePerSnapGap("mn");
},

getSnapGapPixels : function (view, rowNum, colNum) {
    view = view || this.getSelectedView();
    if (view._needsSnapGapUpdate || view._cache.snapGapPixels == null || isNaN(view._cache.snapGapPixels)) {
        if (rowNum == null) rowNum = 0;
        if (colNum == null) colNum = 0;
        var useCol = view && view.verticalEvents == false,
            eventSnapMins = this.getSnapGapMinutes(view, rowNum, colNum),
            minsPerSize = view.getTimePerCell(),
            totalSize
        ;
        if (view && view.body) {
            totalSize = useCol ? view.body.getColumnWidth(colNum) :
                view.getRowHeight(view.getRecord(rowNum), rowNum);
        } else {
            if (useCol) {
                var headerLevel = view && view.fieldHeaderLevel;
                var fieldWidth = this.getFieldWidth(colNum);
                totalSize = fieldWidth || (view && view.columnWidth);
            } else totalSize = this.rowHeight;
        }
        var snapGapPixels = totalSize / ((minsPerSize / eventSnapMins));
        delete view._needsSnapGapUpdate;
        view._cache.snapGapPixels = Math.max(snapGapPixels, 1);
    }
    // never return a value less than 1 from here - that makes no sense and can result in
    // downstream calculations using zero
    return view._cache.snapGapPixels;
},

addSnapGapsToDate : function (date, view, gapsToAdd) {
    if (!date) return null;
    if (gapsToAdd == 0) return date.duplicate();
    view = view || this.getSelectedView();
    if (gapsToAdd == null) gapsToAdd = 1;
    var snapMinutes = this.getSnapGapMinutes(view),
        millis = (snapMinutes * gapsToAdd) * 60000,
        newDate = date.duplicate()
    ;

    if (snapMinutes == 1440) newDate.setDate(newDate.getDate()+gapsToAdd);
    else newDate.setTime(newDate.getTime() + millis);
    return newDate;
},

// get the number or rows in an hour
getRowsPerHour : function (view) {
    view = view || this.getSelectedView();
    return Math.floor(60 / view.getTimePerCell("mn"));
},

// return the rowNum that covers the passed date
getRowFromDate : function (view, date) {
    var minsPerRow = view.getTimePerCell("mn"),
        rowsPerHour = this.getRowsPerHour(view),
        minuteRows = Math.floor(date.getMinutes() / minsPerRow),
        extraRows = (date.getMinutes() % minsPerRow == 0 ? 0 : 1),
        // minsPerRow:15 (rowsPerHour:4), 6:48am gives: (6 * 4) + 3 + 1
        sRow = (date.getHours() * rowsPerHour) + minuteRows + extraRows
    ;
    return sRow;
},

// return the number of pixels that the parameter minutes will occupy in the passed view
getMinutePixels : function (minutes, rowSize, view) {
    view = view || this.getSelectedView();
    if (view.isTimelineView()) {
        // divide the timePerCell by the cell-width to get the minutes per pixel
        var minsPerPixel = view.getTimePerCell() / view.columnWidth;
        // divide the parameter minutes by the minutesPerPixel - that gives the final result,
        // the pixel-count for the parameter minutes
        return Math.round(minutes / minsPerPixel);
    } else if (view.isDayView() || view.isWeekView()) {
        var hourHeight = (rowSize != null ? rowSize : view.getRowHeight(view.getRecord(0), 0)) *
                this.getRowsPerHour(view);
        return Math.round((hourHeight / 60) * minutes);
    }
},

//> @method calendar.scrollToTime()
// Scrolls Calendar +link{calendar.dayView, day} or +link{calendar.weekView, week} views to the
// time represented by the time parameter.  This string parameter is expected to be an
// arbitrary logical time value in any parsable time format - no date portion is expected, but
// time formats like "13:31" or "1:20am" are supported.
// <P>
// Has no effect in +link{calendar.timelineView, timelines}, where an arbitrary time-value is
// inapplicable to any range or resolution greater than a day.
// @param time (String) any parsable time-string
// @visibility calendar
//<
scrollToTime : function (time, view) {
    if (!this.isDrawn()) {
        // store the requested scroll-time - scroll to it at the end of draw()
        this._pendingScrollToTime = time;
        return;
    }
    view = view || this.getSelectedView();
    time = isc.Time.parseInput(time);
    if (isc.isA.Date(time)) {
        var sRow = this.getRowFromDate(view, time);
        var sRowTop = view.getRowHeight(view.getRecord(0), 0) * sRow;
        view.body.scrollTo(0, sRowTop);
        view.redraw();
   }
},

//> @method calendar.scrollToStart()
// Move the viewport of a CalendarView to the start of it's scrollable range.
// @param [view] (CalendarView) the view to affect, the current view if not specified
// @visibility calendar
//<
scrollToStart : function (view) {
    view = view || this.getSelectedView();
    if (view) view.scrollToStart();
},

//> @method calendar.scrollToEnd()
// Move the viewport of a CalendarView to the end of its scrollable range.
// @param [view] (CalendarView) the view to affect, the current view if not specified
// @visibility calendar
//<
scrollToEnd : function (view) {
    view = view || this.getSelectedView();
    if (view) view.scrollToEnd();
},

//> @method calendar.moveToEvent()
// Resets the current visible range of a calendar view so that it shows the date on which the
// passed event occurs.
// @param event (CalendarEvent) the event to move the calendar view to
// @deprecated in favor of +link{calendar.scrollToEvent}.
// @visibility external
//<
moveToEvent : function (event, view) {
    view = view || this.getSelectedView();
    this.setChosenDate(this.getEventStartDate(event));
},

//> @method calendar.scrollToEvent()
// Scrolls the +link{calendar.getCurrentViewName, current view} so the passed event is
// visible.  If the event is outside of the view's current date-range, the default behavior is
// to automatically reload the view with a date-range starting at the event's
// +link{calendar.startDateField, startDate} and then scroll to the event vertically as
// necessary.  Pass false as the <code>canReload</code> param to prevent that default behavior.
// @param event (CalendarEvent) the event to move the calendar view to
// @param [canReload] (boolean) set to false to prevent a view from automatically reloading
//                              with a new range if the passed event is not in its current
//                              scrollable range
// @visibility external
//<
scrollToEvent : function (event, canReload, view) {
    view = view || this.getSelectedView();
    if (canReload != false && !view.canScrollToEvent(event)) {
        view._scrollToEvent = event;
        this.setChosenDate(this.getEventStartDate(event));
        return;
    }
    view.scrollToEvent(event);
},


// Fields on Event Records
// ---------------------------------------------------------------------------------------

//> @attr calendar.nameField  (String : "name" : IR)
// The name of the name field on a +link{CalendarEvent}.
//
// @group calendarEvent
// @visibility calendar
// @see CalendarEvent
//<
nameField: "name",

//> @attr calendar.descriptionField  (String : "description" : IR)
// The name of the description field on a +link{CalendarEvent}.
//
// @group calendarEvent
// @visibility calendar
//<
descriptionField: "description",

//> @attr calendar.startDateField  (String : "startDate" : IR)
// The name of the start date field on a +link{CalendarEvent}.
//
// @group calendarEvent
// @visibility calendar
// @see CalendarEvent
//<
startDateField: "startDate",

//> @attr calendar.endDateField  (String : "endDate" : IR)
// The name of the end date field on a +link{CalendarEvent}.
//
// @group calendarEvent
// @visibility calendar
// @see CalendarEvent
//<
endDateField: "endDate",

//> @attr calendar.durationField  (String : "duration" : IR)
// The name of the +link{calendarEvent.duration, duration} field on a +link{CalendarEvent}.
//
// @group calendarEvent
// @see CalendarEvent
// @visibility external
//<
durationField: "duration",

//> @attr calendar.durationUnitField  (String : "durationUnit" : IR)
// The name of the +link{calendarEvent.durationUnit, durationUnit} field on a
// +link{CalendarEvent}.
//
// @group calendarEvent
// @see CalendarEvent
// @visibility external
//<
durationUnitField: "durationUnit",

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

//> @attr calendar.hideUnusedLanes (Boolean : null : IRW)
// When set to true, hides any +link{calendar.lanes, lane} that doesn't have any active events
// in the current dataset.
// @visibility external
//<

//> @method calendar.setHideUnusedLanes()
// Setter for updating +link{calendar.hideUnusedLanes} after creation.
// @param hideUnusedEvents (boolean) whether to hide unused lanes
// @visibility external
//<
setHideUnusedLanes : function (hideUnusedLanes) {
    this.hideUnusedLanes = hideUnusedLanes;
    if (this.dayView && this.showDayLanes) this.dayView.refreshEvents("setHideUnusedLanes");
    if (this.timelineView) this.timelineView.refreshEvents("setHideUnusedLanes")
},


//> @attr calendar.useSublanes (Boolean : null : IR)
// When set to true, causes +link{calendar.lanes, lanes} to be sub-divided according to their
// set of +link{Lane.sublanes, sublanes}.
//
// @visibility external
//<

//> @attr calendar.sublaneNameField  (String : "sublane" : IR)
// The name of the field which will determine the +link{Lane.sublanes, sublane} in which this
// event will be displayed, within its parent Lane, in +link{Timeline}s and in the
// +link{dayView, day view}, if +link{showDayLanes} is true.
//
// @group calendarEvent
// @visibility external
//<
sublaneNameField: "sublane",

//> @attr calendar.leadingDateField  (String : "leadingDate" : IR)
// The name of the leading date field for each event.  When this attribute and
// +link{trailingDateField} are present in the data, a line extends out from the event showing
// the extent of the leading and trailing dates - useful for visualizing a pipeline of events
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

//> @attr calendar.showLabelColumn (boolean : true : IR)
// When set to false, hides the frozen Label-Column in vertical +link{class:CalendarView}s.
// <P>
// Always false when +link{calendar.showColumnLayouts, showColumnLayouts} is true.
// @visibility external
//<
showLabelColumn: true,

//> @attr calendar.eventWindowStyleField (String : "eventWindowStyle" : IR)
// The name of the field used to override +link{calendar.eventWindowStyle} for an individual
// +link{CalendarEvent}.  See +link{calendarEvent.eventWindowStyle}.
//
// @group calendarEvent, appearance
// @visibility calendar
// @deprecated in favor of +link{calendar.eventStyleNameField}
//<
eventWindowStyleField: "eventWindowStyle",

//> @attr calendar.eventStyleNameField (String : "styleName" : IR)
// The name of the field used to override +link{calendar.eventStyleName} for an individual
// +link{calendarEvent.styleName}.
//
// @group calendarEvent, appearance
// @visibility calendar
//<
eventStyleNameField: "styleName",

//> @attr calendar.canEditField  (String : "canEdit" : IR)
// Name of the field on each +link{CalendarEvent} that determines whether it can be edited in
// the +link{calendar.eventEditor, event editor}.  Note that an event with <code>canEdit</code>
// set to true can also have +link{calendar.canDragEventField, canDrag} or
// +link{calendar.canResizeEventField, canResize} set to false,
// which would still allow editing, but not via drag operations.
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

//> @attr calendar.canEditSublaneField (String : "canEditSublane" : IR)
// Name of the field on each +link{CalendarEvent} that determines whether that event can be
// moved between individual +link{Lane.sublanes, sublanes} in a +link{class:Lane}.
//
// @group calendarEvent
// @see CalendarEvent
// @visibility external
//<
canEditSublaneField: "canEditSublane",

//> @attr calendar.canRemoveField  (String : "canRemove" : IR)
// Name of the field on each +link{CalendarEvent} that determines whether an event shows a
// remove button.
//
// @group calendarEvent
// @visibility calendar
// @see CalendarEvent
//<
canRemoveField: "canRemove",

//> @attr calendar.canDragEventField  (String : "canDrag" : IR)
// Name of the field on each +link{CalendarEvent} that determines whether an +link{EventCanvas}
// can be moved or resized by dragging with the mouse.  Note that
// +link{calendar.canEditEvents, canEditEvents} must be true for dragging to be allowed.
//
// @group calendarEvent
// @visibility calendar
// @see CalendarEvent
//<
canDragEventField: "canDrag",

//> @attr calendar.canResizeEventField  (String : "canResize" : IR)
// Name of the field on each +link{CalendarEvent} that determines whether an event can be
// resized by dragging.
//
// @group calendarEvent
// @visibility calendar
// @see CalendarEvent
//<
canResizeEventField: "canResize",



//> @attr calendar.allowDurationEvents  (Boolean : null : IRW)
// When set to true, allows events to be managed by duration, as well as by end date.  Values
// can be set for +link{calendarEvent.duration, duration} and
// +link{calendarEvent.durationUnit, duration unit} on each event, and are then maintained,
// instead of the end date, when alterations are made to the event via editors or dragging
// with the mouse.
//
// @group calendarEvent
// @see CalendarEvent
// @visibility external
//<

durationUnitOptions: [ "minute", "hour", "day", "week" ],
getDurationUnitMap : function () {
    var options = this.durationUnitOptions,
        util = isc.DateUtil,
        result = {}
    ;
    for (var i=0; i<options.length; i++) {
        result[util.getTimeUnitKey(options[i])] = util.getTimeUnitTitle(options[i]) + "s";
    }
    return result;
},


//> @attr calendar.laneEventPadding  (Integer : 0 : IRW)
// The pixel space to leave between events and the edges of the +link{calendar.lanes, lane} or
// +link{Lane.sublanes, sublane} they appear in.  Only applicable to
// +link{calendar.timelineView, timelines} and to +link{calendar.dayView, dayViews} showing
// +link{calendar.showDayLanes, day lanes}.
//
// @visibility external
//<
laneEventPadding: 0,

//> @attr calendar.eventDragGap  (Integer : 10 : IRW)
// The number of pixels to leave to the right of events so overlapping events can still be
// added using the mouse.
//
// @visibility external
//<
eventDragGap: 10,

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

//> @attr calendar.eventSnapGap (Integer : null : IRW)
// The number of minutes that determines the positions to which events will snap when rendered,
// and when moved or resized with the mouse.
// <P>
// If unset (the default), all views will snap to each cell boundary; 30 minutes in a default
// vertical view, or one +link{calendar.timelineGranularity, column} in a default Timeline.
// <P>
// If set to zero, views will snap to one of a set of known "sensible" defaults: for a default
// vertical, this will be 5 minutes.  For timelines, the eventSnapGap is automatic depending on
// the current +link{calendar.timelineGranularity}.  If +link{calendar.timelineUnitsPerColumn}
// is greater than 1, the snapGap is set to one unit of the current granularity.  So, a
// cell-resolution of 15 minutes would snap to every minute, assuming there are at least 15
// pixels per column. Otherwise, the snapGap is either 15 minutes, 1 hour, one day or one
// month, depending on granularity.
// <P>
// If any other value is specified, it is used where possible.
// <P>
// If the specified or calculated value is less than the time covered by a single pixel in the
// current view, then it can't be represented.  In this case, it is rounded up to the lowest of
// a set of "sensible" time-spans that <i>can</i> be represented: one of
// [1, 5, 10, 15, 20, 30, 60, 120, 240, 360, 480, 720, 1440].
// <P>
// For example - a Timeline showing "day" columns cannot support an eventSnapGap of 1 minute,
// unless each column is at least 1440 pixels wide - if the columns were only 150px wide, then
// each pixel would represent around 9.6 minutes, which would result in unpleasant and unexpected
// time-offsets when dragging events.  So, the calculated eventSnapGap will be rounded
// up to the nearest "sensible" time-span - in this case, 10 minutes.  If the columns were only
// 60px wide, it would be 30 minutes.
//
// @group editing
// @visibility external
//<
//eventSnapGap: null,

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

//> @attr calendar.eventEditorDateFieldTitle (HTMLString : "Date" : IR)
// The title for the Date-field in the
// +link{Calendar.showEventEditor, eventEditor} that allows for changing the logical start-date
// of an event, along with its start and end times, when editing events in the
// +link{calendar.dayView, day} and +link{calendar.weekView, week} views.
// @group editing, i18nMessages
// @visibility calendar
//<
eventEditorDateFieldTitle: "Date",

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

//> @attr calendar.canDragEvents (Boolean : null : IR)
// A boolean value controlling whether users can drag-reposition events.  By default, this is
// false for Touch devices, where drag gestures scroll the view, and true otherwise.
// <P>
// Only has an effect when +link{calendar.canEditEvents, canEditEvents} is true.
//
// @group allowedOperations
// @visibility calendar
//<
canDragEvents: null,

//> @attr calendar.canResizeEvents (Boolean : true : IR)
// Can +link{CalendarEvent, events} be resized by dragging appropriate edges of the
// +link{eventCanvas.verticalResize, canvas}?  Only has an effect when both
// +link{calendar.canEditEvents, canEditEvents} and +link{calendar.canDragEvents, canDragEvents}
// are true.  Set this attribute to false to disallow drag-resizing.
// <P>
// Always false when +link{calendar.showColumnLayouts, showColumnLayouts} is true.
// @visibility external
//<
canResizeEvents: true,

// Show / Hide parts of the interface
// ---------------------------------------------------------------------------------------

//> @attr calendar.showDateChooser (Boolean : true : IR)
// Determines whether the +link{calendar.dateChooser,dateChooser} is displayed.
//
// @group visibility
// @visibility calendar
//<
showDateChooser: false,

//> @attr calendar.disableWeekends (Boolean : true : IRW)
// If true, weekend days appear in a disabled style and events cannot be created on weekends.
// Which days are considered weekends is controlled by +link{calendar.weekendDays}.
//
// @group visibility
// @visibility calendar
//<
disableWeekends: true,

dateIsWeekend : function (date) {
    // compare a logicalDate (which is in the display-timezone)
    var lDate = isc.DateUtil.getLogicalDateOnly(date);
    return this.getWeekendDays().contains(lDate.getDay());
},

//> @attr calendar.weekendDays (Array of int : null : IRW)
// An array of integer day-numbers that should be considered to be weekend days by this
// Calendar instance.  If unset, defaults to the set of days indicated
// +link{DateUtil.weekendDays, globally}.
//
// @group visibility
// @visibility calendar
//<
getWeekendDays : function () {
    return this.weekendDays;
},


//ignoreDST: null,

//> @method calendar.shouldDisableDate()
// Returns true if the passed date should be considered disabled.  Disabled dates don't allow
// events to be created by clicking on them, and drag operations that would start or end on
// such dates are also disallowed.
// <P>
// The default implementation returns false only for dates that fall on a
// +link{DateUtil.getWeekendDays(), weekend}.
// @param date (Date) a Date instance
// @param [view] (CalendarView) the view the date appears in
// @return (boolean) true if this date should be considered disabled
// @visibility external
//<
shouldDisableDate : function (date, view, isEndDate) {
    if (!date) return false;
    if (!this.disableWeekends || isEndDate) return false;
    view = view || this.getSelectedView();
    // is the passed date disabled?  by default, just returns false if the date falls on a
    // weekend and disableWeekends is true
    // compare with a logicalDate (which is in the display-timezone)
    var displayDate = isc.DateUtil.getLogicalDateOnly(date);
    if (this.dateIsWeekend(displayDate)) return true;
    return false;
},

//> @method calendar.shouldShowDate()
// Indicates whether the passed date should be visible in the passed +link{class:CalendarView}.
// <P>
// The default implementation returns true, unless the date falls on a
// +link{DateUtil.getWeekendDays(), weekend} and +link{calendar.showWeekends, showWeekends} is
// false.
// @param date (Date) a Date instance
// @param [view] (CalendarView) the view the date appears in
// @return (boolean) true if this date should be considered disabled
// @visibility external
//<
shouldShowDate : function (date, view) {
    view = view || this.getSelectedView();
    if (view.isTimelineView()) {
        if (!this.showWeekends && this.dateIsWeekend(date)) return false;
    }
    return true;
},

//> @method calendar.shouldShowLane()
// Indicates whether the passed +link{calendar.lanes, lane} should be visible in the passed
// +link{class:CalendarView}.
// <P>
// The default implementation returns true, unless the lane has no events and
// +link{calendar.hideUnusedLanes} is true.
// @param lane (Lane | String) the lane object or name
// @param [view] (CalendarView) the view the lane appears in
// @return (boolean) true if this lane should be displayed in the passed view
// @visibility external
//<
shouldShowLane : function (lane, view) {
    view = view || this.getSelectedView();

    if (this.hideUnusedLanes && !this.laneHasVisibleEvents(lane)) {
        // hide lanes with no events
        return false;
    }
    return true;
},

//> @method calendar.shouldShowEvent()
// Indicates whether the passed +link{class:CalendarEvent, event} should be visible in the
// passed +link{class:CalendarView}.
// <P>
// The default implementation returns true - note that this method only runs for events that are
// known to be in the accessible range and is a mechanism for extended custom filtering.
// @param event (CalendarEvent) the event to check
// @param [view] (CalendarView) the view the event will be rendered in
// @return (boolean) true if this event should be displayed in the passed view
// @visibility external
//<
shouldShowEvent : function (event, view) {
    return true;
},

//> @attr calendar.showWeekends (Boolean : true : IRW)
// Suppresses the display of weekend days in the +link{calendar.weekView, week},
// +link{calendar.monthView, month} and +link{calendar.timelineView, timeline} views, and
// disallows the creation of events on weekends.  Which days are considered weekends is
// controlled by +link{calendar.weekendDays}.
//
// @setter calendar.setShowWeekends()
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

//> @attr calendar.selectChosenDate (Boolean : true : IRW)
// When true, shows the current +link{calendar.chosenDate, chosenDate} in a selected style
// in the +link{monthView, month view}  Has no effect in other views.
//
// @group visibility
// @visibility calendar
//<
selectChosenDate: true,

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

//> @attr calendar.bringEventsToFront (Boolean : null : IR)
// If set to true, clicking an event will bring it to the front of the zorder.
//
// @group calendarEvent
// @visibility calendar
//<
//bringEventsToFront: null,

//> @attr calendar.eventOverlap (Boolean : true : IR)
// When +link{eventAutoArrange} is true, setting eventOverlap to true causes events that
// share timeslots to overlap each other by a percentage of their width, specified by
// +link{eventOverlapPercent}.  The default is true for Calendars and false for
// +link{class:Timeline, Timelines}.
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

//> @attr calendar.minimalUI (boolean : false : IRW)
// A boolean value controlling whether the Calendar shows tabs for available calendar views.
// By default, this is true for handsets and false otherwise.
//
// @visibility external
//<
minimalUI: null,

//> @attr calendar.canDragCreateEvents (Boolean : null : IRW)
// A boolean value controlling whether new events of varying length can be created by dragging
// the cursor.  By default, this is false for Touch devices and true otherwise.
//
// @visibility external
//<
canDragCreateEvents: null,

// AutoChildren
// ---------------------------------------------------------------------------------------

//> @attr calendar.mainView (AutoChild TabSet : null : R)
// +link{TabSet} for managing calendar views when multiple views are available (eg,
// +link{dayView, day} and +link{monthView, month}).
//
// @visibility calendar
//<

//> @attr calendar.dayView (AutoChild CalendarView : null : R)
// +link{CalendarView} used to display events that pertain to a given day.
//
// @visibility calendar
//<

//> @attr calendar.weekView (AutoChild CalendarView : null : R)
// +link{CalendarView} used to display events that pertain to a given week.
//
// @visibility calendar
//<

//> @attr calendar.monthView (AutoChild CalendarView : null : R)
// +link{CalendarView} used to display events that pertain to a given month.
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

//> @attr calendarEvent.duration (Integer : null : IRW)
// The duration of this event.  May be specified instead of an
// +link{calendarEvent.endDate, end date} and implies that this is a "Period" type event.  If
// set to zero, implies an "Instant" type event - an event with a start date but no length.
//
// @visibility external
//<

//> @attr calendarEvent.durationUnit (TimeUnit : "minute" : IRW)
// When a +link{calendarEvent.duration, duration} is set for this event, this is the unit of
// that duration.  The default is minutes.
//
// @visibility external
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

//> @attr calendarEvent.canEdit (Boolean : null : IRW)
// Optional boolean value controlling the editability of this particular calendarEvent.
// The name of this field within the CalendarEvent can be changed via
// +link{calendar.canEditField}.
//
// @visibility calendar
//<

//> @attr calendarEvent.canDrag (Boolean : null : IRW)
// Optional boolean value controlling whether this event can be dragged with the mouse.
// The name of this field within the CalendarEvent can be changed via
// +link{calendar.canDragEventField}.  Only has an effect when
// +link{calendar.canEditEvents, editing} is enabled.
// <P>
// You can separately disallow drag-resize via +link{calendarEvent.canResize, canResize}.
//
// @visibility calendar
//<

//> @attr calendarEvent.canResize (Boolean : null : IRW)
// Optional boolean value controlling whether this event can be drag-resized with the mouse.
// The name of this field within the CalendarEvent can be changed via
// +link{calendar.canResizeEventField}.
// <P>
// Only has an effect if +link{calendar.canEditEvents, editing} and
// +link{calendar.canDragEvents, dragging} are also enabled.
//
// @visibility calendar
//<

//> @attr calendarEvent.canEditLane (Boolean : null : IRW)
// Boolean indicating whether this event can be moved between lanes.  Can also be set at the
// +link{calendar.canEditLane, calendar level}.
// <P>
// The name of this field within the CalendarEvent can be changed via
// +link{calendar.canEditLaneField}.
//
// @visibility calendar
//<

//> @attr calendarEvent.canEditSublane (Boolean : null : IRW)
// Boolean indicating whether this event can be moved between lanes.  Can also be set at the
// +link{calendar.canEditSublane, calendar level}.
// <P>
// The name of this field within the CalendarEvent can be changed via
// +link{calendar.canEditSublaneField}.
//
// @visibility external
//<

//> @attr calendarEvent.backgroundColor (String : null : IRW)
// An optional background color for the body portion of +link{class:EventCanvas, canvases}
// representing this event in the various +link{class:CalendarView, calendar views}.
// <P>
// Note that the recommended approach for styling events is to set a
// +link{calendarEvent.styleName, custom CSS style}, which allows more complete customization
// of both header and body portions.
//
// @visibility calendar
//<

//> @attr calendarEvent.textColor (String : null : IRW)
// An optional text color for the body portion of +link{class:EventCanvas, canvases}
// representing this event in the various +link{class:CalendarView, calendar views}.
// <P>
// Note that the recommended approach for styling events is to set a
// +link{calendarEvent.styleName, custom CSS style}, which allows more complete customization
// of both header and body portions.
//
// @visibility calendar
//<

//> @attr calendarEvent.borderColor (String : null : IRW)
// An optional border color for the body portion of +link{class:EventCanvas, canvases}
// representing this event in the various +link{class:CalendarView, calendar views}.
// <P>
// Note that the recommended approach for styling events is to set a
// +link{calendarEvent.styleName, custom CSS style}, which allows more complete customization
// of both header and body portions.
//
// @visibility calendar
//<

//> @attr calendarEvent.headerBackgroundColor (String : null : IRW)
// An optional background color for the header portion of +link{class:EventCanvas, canvases}
// representing this event in the various +link{class:CalendarView, calendar views}.
// <P>
// Note that the recommended approach for styling events is to set a
// +link{calendarEvent.styleName, custom CSS style}, which allows more complete customization
// of both header and body portions.
//
// @visibility calendar
//<

//> @attr calendarEvent.headerTextColor (String : null : IRW)
// An optional text color for the header portion of +link{class:EventCanvas, canvases}
// representing this event in the various +link{class:CalendarView, calendar views}.
// <P>
// Note that the recommended approach for styling events is to set a
// +link{calendarEvent.styleName, custom CSS style}, which allows more complete customization
// of both header and body portions.
//
// @visibility calendar
//<

//> @attr calendarEvent.headerBorderColor (String : null : IRW)
// An optional border color for the header portion of +link{class:EventCanvas, canvases}
// representing this event in the various +link{class:CalendarView, calendar views}.
// <P>
// Note that the recommended approach for styling events is to set a
// +link{calendarEvent.styleName, custom CSS style}, which allows more complete customization
// of both header and body portions.
//
// @visibility calendar
//<

//> @attr calendarEvent.eventWindowStyle (CSSStyleName : null : IR)
// CSS style series to use for the draggable event window that represents this event.  If
// specified, overrides +link{calendar.eventWindowStyle} for this specific event.
// <P>
// The name of this field within the CalendarEvent can be changed via
// +link{Calendar.eventWindowStyleField}
//
// @visibility calendar
// @deprecated in favor of +link{calendarEvent.styleName}
//<

//> @attr calendarEvent.styleName (CSSStyleName : null : IR)
// CSS style series to use for +link{calendar.eventCanvas, canvas instances} that
// represent this event in the various +link{class:CalendarView, calendar views}.  The basic
// series should include three classes - the base style and others suffixed "Header" and "Body".
// <P>
// If not specified on the event, the style can be specified on the
// +link{calendar.eventStyleName, calendar}, the +link{calendarView.eventStyleName, view} or
// individually on each +link{lane.eventStyleName, lane} or +link{lane.sublanes, sublane}.
// <P>
// The name of this field within the CalendarEvent can be changed via
// +link{Calendar.eventStyleNameField}
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

//> @attr calendarEvent.sublane (String : null : IRW)
// When in Timeline mode, or when +link{calendar.showDayLanes} is true, a string that
// represents the name of the +link{Lane.sublanes, sublane} this +link{CalendarEvent} should
// sit in.  The name of this field within the CalendarEvent can be changed via
// +link{Calendar.sublaneNameField}.
//
// @visibility external
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

//> @attr calendar.showLaneRollOver (Boolean : null : IRW)
// When set to true, causes +link{timelineView, Timelines} to highlight the Lane under the
// mouse with the "Over" style.
//
// @visibility calendar
//<
//showLaneRollOver: null,

//> @method calendar.getWorkdayStart()
// Returns the start of the working day on the passed date.  By default, this method returns
// the value of +link{calendar.workdayStart, workdayStart}.
// @param date (Date) a Date instance
// @param [laneName] (String) the name of the relevant lane - only passed for dayView with
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
// @param [laneName] (String) the name of the relevant lane - only passed for dayView with
//                            showDayLanes: true
// @return (String) any parsable time-string
// @visibility calendar
//<
getWorkdayEnd : function (date, laneName) {
    return this.workdayEnd;
},

//> @method calendar.getWorkdayRange()
// Returns an object with with start and end attributes, which are logical-times representing
// the +link{calendar.getWorkdayStart(), start} and
// +link{calendar.getWorkdayEnd(), end} of the +link{calendar.showWorkday, working day}
// for the passed date.
// @param date (Date) a Date instance
// @param [laneName] (String) the name of the relevant lane - only passed for dayView with
//                            showDayLanes: true
// @return (Object) array or startTime, endTime for the passed date
// @visibility internal
//<
getWorkdayRange : function (date, laneName) {
    var result = {
        start: isc.Time.parseInput(this.getWorkdayStart(date || this.chosenDate)),
        end: isc.Time.parseInput(this.getWorkdayEnd(date || this.chosenDate))
    };
    return result;
},

//> @method calendar.getVisibleStartDate()
// Returns the first visible date in the passed, or currently selected, calendar view.
// @param [view] (CalendarView) the view to get the startDate for, or current view if
// @return (Date) first visible date
// @visibility calendar
//<
getVisibleStartDate : function (view) {
    view = view || this.getSelectedView();
    if (!view || isc.isAn.emptyString(view)) return null;
    if (view.isMonthView() || (view.body && view.bodies.length > 1)) return view.getCellDate(0, 0);
    return view.startDate;
},

//> @method calendar.getVisibleEndDate()
// Returns the last visible date in the passed, or currently selected, calendar view.
// @param [view] (CalendarView) the view to get the endDate for, or current view if null
// @return (Date) last visible date
// @visibility calendar
//<
getVisibleEndDate : function (view) {
    view = view || this.getSelectedView();
    if (!view || isc.isAn.emptyString(view)) return null;
    // monthView is expected to only have 1 body
    if (!view.body || (view.bodies.length == 1 && !view.isMonthView())) return view.endDate;

    var rowNum = view.getData().length-1,
        colNum = view.body.fields.length-1
    ;
    if (view.getCellEndDate) return view.getCellEndDate(rowNum, colNum);
    return view.getCellDate(rowNum, colNum);
},

//> @method calendar.getPeriodStartDate()
// Returns the start of the selected week or month depending on the current calendar view.
// For the month view, and for the week view when not showing weekends, this will often be a
// different date than that returned by +link{calendar.getVisibleStartDate}.
// @param [view] (CalendarView) the view to get the periodStartDate for, or current view if null
// @return (Date) period start date
// @visibility calendar
//<
getPeriodStartDate : function (view) {
    view = view || this.getSelectedView();
    if (view) return view.getPeriodStartDate();
},

//> @method calendar.getPeriodEndDate()
// Returns the end of the period selected in the passed, or current, calendar view.
// For the +link{calendar.monthView, month view}, and for the
// +link{calendar.weekView, week view} when not showing weekends, this will often be a
// different date than that returned by +link{calendar.getVisibleEndDate}.
// @param [view] (CalendarView) the view to get the periodEndDate for, or current view if null
// @return (Date) period end date
// @visibility calendar
//<
getPeriodEndDate : function (view) {
    view = view || this.getSelectedView();
    if (view) return view.getPeriodEndDate();
},

// Data & Fetching
// ---------------------------------------------------------------------------------------

//> @attr calendar.data (Array of CalendarEvent : null : IRW)
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

//> @attr calendar.dataSource (DataSource | ID : null : IRW)
// @include dataBoundComponent.dataSource
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
// @group searchCriteria
// @visibility calendar
//<

//> @attr Calendar.implicitCriteria (Criteria : null : IRW)
// @include dataBoundComponent.implicitCriteria
// @visibility external
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
// "firstVisibleDay" and "lastVisibleDay" with values of type Date.  Note that these
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

//> @attr calendar.timelineView (AutoChild CalendarView : null : R)
// +link{CalendarView} used to display events in lanes in a horizontal +link{Timeline} view.
//
// @visibility calendar
//<

//> @attr calendar.renderEventsOnDemand (boolean : true : IR)
// When set to true, the default, each +link{class:EventCanvas, event} is rendered as it
// appears in the viewport.  If set to false, all events are rendered up-front, whenever the
// current range changes.
// <P>
// Has no effect when +link{calendar.showColumnLayouts, showColumnLayouts} is true.
// @visibility external
//<
renderEventsOnDemand: true,

//> @attr calendar.timelineGranularity (TimeUnit : "day" : IR)
// The granularity in which the +link{calendar.timelineView, timelineView} will display events.
// Possible values are those available in the built-in +link{type:TimeUnit, TimeUnit} type.
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
// @deprecated in favor of +link{calendar.canResizeEvents, canResizeEvents};
//<
canResizeTimelineEvents: false,

//> @attr calendar.canEditLane (boolean : null : IR)
// Can events be moved between lanes?  If so, the event can be dragged to a different
// +link{calendar.lanes, lane}, and the event +link{calendar.eventDialog, quick dialog} and
// +link{calendar.eventEditor, editor} allow a lane to be selected with a drop-down chooser.
// <P>
// In either case, the event's +link{calendar.laneNameField,laneNameField} is updated automatically.
// <P>
// If set to false, cross-lane dragging is disallowed and drop-down Lane-choosers are disabled
// when editing existng events.  When creating +link{calendar.canCreateEvents, new events},
// the Lane-chooser remains enabled so an initial Lane can be selected.
// <P>
// This setting can be overridden on each +link{CalendarEvent.canEditLane, event}.
//
// @visibility external
//<

//> @attr calendar.canEditSublane (boolean : null : IR)
// Can events be moved between sublanes?
// <P>
// If so, the event can be dragged to a different +link{Lane.sublanes, sublane} within the same
// parent Lane and, when it's editor is shown, an additional drop-down widget is provided
// allowing the sublane to be altered.
// <P>
// If the sublane is locked, but the +link{calendar.canEditLane, parent lane} isn't, an update
// to the event's +link{calendar.laneNameField, lane name} will be allowed, assuming that the
// new Lane has an existing sublane with the same name.
// <P>
// In either case, the event's +link{Calendar.sublaneNameField, sublane} is updated
// automatically.
// <P>
// This setting can be overridden on each +link{CalendarEvent.canEditSublane, event}.
//
// @visibility external
//<

//> @attr calendar.canReorderLanes (Boolean : null : IR)
// If true, lanes can be reordered by dragging their +link{calendar.laneFields, laneFields}
// with the mouse.
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
// Note that the value you provide may be automatically altered if showing
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
// Note that the value you provide may be automatically altered if showing
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

//> @attr headerLevel.headerWidth (Integer : null : IR)
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
// @return (HTMLString) The formatted title for the values passed in
// @visibility external
//<

//> @method headerLevel.hoverHTML()
// An optional function for providing formatted HTML for the hover shown
// +link{calendar.showHeaderHovers, showHeaderHovers} is true and the mouse hovers over this
// headerLevel.
//
// @param headerLevel (HeaderLevel) a reference to this headerLevel
// @param startDate (Date) the start of the date-range covered by this span in this level
// @param endDate (Date) the end of the date-range covered by this span in this level - may be
//                       null
// @param defaultValue (String) the default hover HTML as generated by the Timeline
// @param view (CalendarView) a reference to the CalendarView
// @return (HTMLString) The HTML to show in a hover for the values passed in
// @visibility internal
//<

//> @attr calendar.weekPrefix (HTMLString : "Week" : IR)
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

// default to having hovers show immediately
hoverDelay: 0,

//> @object Lane
// Lane shown in a +link{class:Timeline} view, or in a +link{calendar.dayView, day view} when
// +link{calendar.showDayLanes, showDayLanes} is true.  Each lane is a row or column,
// respectively, that can contain a set of +link{CalendarEvent}s.  CalendarEvents are placed in
// lanes by matching the +link{Lane.name} property to the value of the
// +link{calendar.laneNameField} property on the CalendarEvent.
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
// In +link{class:Timeline}s, the height of this Lane's row.  Has no effect when set on a Lane
// being displayed in a +link{calendar.dayView, day view} as a result of
// +link{calendar.showDayLanes} being true.
// <P>
// If set directly on a +link{lane.sublanes, sublane}, overrides the default behavior of
// dividing the height equally among the lane's sublanes.  Each sublane is still initially
// assigned an equal slice of the parent height, and the value for this sublane is
// then updated.  So the overall height of the parent lane will change by the delta between the
// initial slice and the specified one.
//
// @visibility external
//<

//> @attr lane.width (Number : null : IR)
// When set on a Lane being displayed in a +link{calendar.dayView, day view} as a result of
// +link{calendar.showDayLanes} being set, dictates the width of the Lane's column.  Has no
// effect in +link{class:Timeline}s.
// <P>
// If set directly on a +link{lane.sublanes, sublane}, overrides the default behavior of
// dividing the width equally among the lane's sublanes.  Each sublane is still initially
// assigned an equal slice of the original parent width, and the value for this sublane is then
// updated.  So the overall width of the parent lane will change by the delta between the
// initial slice and the specified one.
//
// @visibility external
//<

//> @attr lane.title (HTMLString : null : IR)
// Title to show for this lane.  Has no effect if set directly on +link{lane.sublanes, sublanes}.
//
// @visibility external
//<

//> @attr lane.sublanes (Array of Lane : null : IR)
// Array of +link{class:Lane} objects that will share the available space in the parent Lane,
// vertically in +link{calendar.timelineView, timelines} and horizontally in
// +link{calendar.dayView, day views}.
// <P>
// Only one level of sublanes is supported, so this attribute only has an effect on
// +link{calendar.lanes, top-level lanes}.
// <P>
// Note that this feature is mutually exclusive with the
// +link{calendar.eventAutoArrange, auto arrangement} of events that share time.
//
// @visibility external
//<

//> @attr lane.eventStyleName  (CSSStyleName : null : IRW)
// The base name for the CSS class applied to +link{calendar.eventCanvas, events} when they're
// rendered in this lane.  See +link{calendar.eventStyleName}.
// <P>
// If set directly on a +link{lane.sublanes, sublane}, overrides the corresponding value on
// the parent +link{calendar.lanes, lane}.  See
// +link{calendar.getEventCanvasStyle, getEventCanvasStyle()} for more information.
//
// @group appearance
// @visibility calendar
//<

//> @attr lane.styleName  (CSSStyleName : null : IRW)
// The base style-name for normal cells in this Lane.
//
// @group appearance
// @visibility calendar
//<

//> @attr lane.fieldStyleName  (CSSStyleName : null : IRW)
// The base style-name for +link{Calendar.laneFields, lane-fields} displayed in this Lane.
//
// @group appearance
// @visibility calendar
//<


//> @attr calendar.canGroupLanes (Boolean : null : IRW)
// If true, allows the lanes in a Timeline to be grouped by providing a value for
// +link{calendar.laneGroupByField, laneGroupByField}.  The fields available for grouping on
// are those defined as +link{calendar.laneFields, lane fields}.  Since these are definitions
// for +link{ListGridField, normal fields}, you can choose to +link{listGridField.showIf, hide}
// the field in the timeline, but still have it available for grouping.
// @visibility external
//<

//> @method calendar.groupLanesBy()
// When +link{calendar.canGroupLanes, canGroupLanes} is true, this method allows the grouping
// in +link{calendar.timelineView, timeline}s to be altered at runtime.
// @param groupFieldName (String | Array of String) one or more field names to group by
// @visibility external
// @group grouping
//<
groupLanesBy : function (groupFieldName) {
    if (this.timelineView) {
        this.timelineView.groupBy(groupFieldName);
    }
},

//> @attr calendar.laneGroupByField (String | Array of String : null : IRW)
// For timelines with +link{calendar.canGroupLanes, canGroupLanes} set to true, this is a
// field name or array of field names on which to group the lanes in a timeline.
// @visibility external
//<

//> @attr calendar.laneGroupStartOpen (GroupStartOpen | Array : "first" : IRW)
// Describes the default state of lane groups in timelines when
// +link{calendar.groupLanesBy, groupLanesBy} is called.
//
// Possible values are:
// <ul>
// <li>"all": open all groups
// <li>"first": open the first group
// <li>"none": start with all groups closed
// <li>Array of values that should be opened
// </ul>
//
// @group grouping
// @see calendar.groupLanesBy()
// @visibility external
//<
laneGroupStartOpen: "first",

//> @attr calendar.lanes (Array of Lane : null : IRW)
// An array of +link{Lane} definitions that represent the rows of the +link{timelineView}, or
// the columns of the +link{dayView} if +link{calendar.showDayLanes, showDayLanes} is true.
// @visibility external
// @setter setLanes()
//<

//> @method calendar.setLanes()
// Sets the +link{calendar.lanes, lanes} in the current calendar view.  Only has an effect
// in +link{timelineView, timeline views}, and in +link{dayView, day views} when
// +link{showDayLanes} is true.
//
// @param lanes (Array of Lane) array of lanes to display
// @param [skipRefreshEvents] (boolean) set to false to prevent events from being refreshed
//
// @visibility external
//<
setLanes : function (lanes, skipRefreshEvents) {
    // bail if nothing passed
    if (!lanes) { return; }
    // make sure there's a lane.name on each lane - see comment in the method
    this.checkLaneNames(lanes);
    // store lanes but don't call through if not yet draw()n
    this.lanes = lanes;
    if (this.timelineView) { this.timelineView.setLanes(this.lanes, skipRefreshEvents); }
    if (this.showDayLanes && this.dayView) { this.dayView.setLanes(this.lanes, skipRefreshEvents); }
},

checkLaneNames : function (lanes) {
    if (!lanes || lanes.length == 0) return;
    // attempt to set a "name" property on each lane if there isn't one
    var cal = this;
    if (cal.laneNameField && cal.laneNameField != "name") {

        lanes.map(function (lane) {
            if (!lane.name && lane[cal.laneNameField]) {
                cal.logInfo("No lane.name provided - mapping to laneNameField (" +
                    cal.laneNameField + ") - lane.name is now '" +
                    lane[cal.laneNameField] + "'");
                lane.name = lane[cal.laneNameField];
            }
        });
    }
},

//> @method calendar.addLane()
// Adds a new +link{object:Lane} to the calendar, for display in the
// +link{timelineView, timeline view}, and in the
// +link{calendar.dayView, day view} if +link{calendar.showDayLanes, showDayLanes} is true.
//
// @param lane (Lane) a new Lane object to add to the calendar
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
    else if (isc.isAn.Object(lane)) lane = this.lanes.find("name", lane.name);
    if (lane) {
        this.lanes.remove(lane);
        view.setLanes(this.lanes);
    }
},

//> @attr calendar.laneFields (Array of ListGridField : null : IR)
// Field definitions for the frozen area of the +link{timelineView}, which shows data about the
// timeline +link{lanes}.  Each field shows one attribute of the objects provided as
// +link{calendar.lanes}.
// <P>
// When +link{calendar.canGroupLanes, lane grouping} is enabled, only fields that are specified
// as lane fields can be used as group fields.
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
            this.dayView.refreshEvents("setShowDayLanes");
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
// A set of +link{SortSpecifier, sort-specifiers} for customizing the render order of events
// that overlap.
// <P>
// In +link{Timeline, timelines}, this dictates the vertical rendering order of
// overlapped events in each +link{Lane, lane}.
// <P>
// In +link{calendar.dayView, day} and +link{calendar.weekView, week} views, it dictates the
// horizontal rendering order of overlapped events in each column or Lane.
// <P>
// By default, events that share space in a Lane or column are rendered from top to bottom,
// or left to right according to their +link{startDateField, start-dates} - the earliest in a
// given lane appears top-most in that lane, or left-most in its column.
// <P>
// Providing <code>overlapSortSpecifiers</code> allows for the events to be ordered by one or
// more of the fields stored on the events, or in the underlying +link{DataSource, data-source},
// if the Calendar is databound.
//
// @visibility external
//<

//> @attr calendar.todayBackgroundColor (String : null : IR)
// The background color for cells that represent today in all +link{class:CalendarView}s.
// @visibility external
//<

//> @attr calendar.showEventDescriptions (boolean : true : IR)
// When rendering the +link{calendar.eventCanvas, canvas} for an event, whether to show the
// +link{eventCanvas.showBody, body area}, typically containing brief details of the event -
// +link{calendar.getEventBodyHTML, by default},
// +link{calendar.descriptionField, its description}.
// <P>
// The default is true - if set to false, the event's +link{eventCanvas.showHeader, header}
// will fill the canvas.
// @visibility external
//<
showEventDescriptions: true,

//> @attr calendar.showEventHeaders (boolean : true : IR)
// When rendering the +link{calendar.eventCanvas, canvas} for an event, whether to show the
// +link{eventCanvas.showHeader, header area}, typically containing suitable title text -
// +link{calendar.getEventHeaderHTML, by default}, the event's +link{calendar.nameField, name}.
// <P>
// The default is true - if set to false, the event's +link{eventCanvas.showBody, body area}
// will fill the canvas.
// @visibility external
//<
showEventHeaders: true,

//> @attr calendar.eventHeaderWrap (boolean : true : IR)
// When rendering the +link{calendar.eventCanvas, canvas} for an event, whether to allow the
// content of the +link{eventCanvas.showHeader, header area} to wrap to multiple lines.
// <P>
// The default is true - if set to false, the header area is
// +link{calendar.eventHeaderHeight, fixed}, unless +link{calendar.showEventDescriptions} is
// false, in which case the header area fills the canvas.
// @visibility external
//<
eventHeaderWrap: true,

//> @attr calendar.eventHeaderHeight (int : 14 : IR)
// When +link{calendar.eventHeaderWrap, eventHeaderWrap} is false and
// +link{calendar.showEventDescriptions, showEventDescriptions} is true, this is the fixed
// height for the +link{eventCanvas.showHeader, header area} in event canvases.
// @visibility external
//<
eventHeaderHeight: 14,

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

//> @attr calendar.dayViewTitle (String : "Day" : IR)
// The title for the +link{dayView, day view}.
//
// @group i18nMessages
// @visibility calendar
//<
dayViewTitle: "Day",

//> @attr calendar.weekViewTitle (String : "Week" : IR)
// The title for the +link{weekView, week view}.
//
// @group i18nMessages
// @visibility calendar
//<
weekViewTitle: "Week",

//> @attr calendar.monthViewTitle (String : "Month" : IR)
// The title for the +link{monthView, month view}.
//
// @group i18nMessages
// @visibility calendar
//<
monthViewTitle: "Month",

//> @attr calendar.timelineViewTitle (String : "Timeline" : IR)
// The title for the +link{timelineView, timeline view}.
//
// @group i18nMessages
// @visibility external
//<
timelineViewTitle: "Timeline",

//> @attr calendar.eventNameFieldTitle (HTMLString : "Event Name" : IR)
// The title for the +link{nameField} in the quick
// +link{calendar.eventDialog, event dialog} and the detailed
// +link{calendar.eventEditor, editor}.
//
// @group i18nMessages
// @visibility external
//<
eventNameFieldTitle: "Event Name",

//> @attr calendar.eventStartDateFieldTitle (HTMLString : "From" : IR)
// The title for the +link{startDateField} in the quick
// +link{calendar.eventDialog, event dialog} and the detailed
// +link{calendar.eventEditor, editor}.
//
// @group i18nMessages
// @visibility external
//<
eventStartDateFieldTitle: "From",

//> @attr calendar.eventEndDateFieldTitle (HTMLString : "To" : IR)
// The title for the +link{endDateField} in the quick
// +link{calendar.eventDialog, event dialog} and the detailed
// +link{calendar.eventEditor, editor}.
//
// @group i18nMessages
// @visibility external
//<
eventEndDateFieldTitle: "To",

//> @attr calendar.eventDescriptionFieldTitle (HTMLString : "Description" : IR)
// The title for the +link{descriptionField} field in the quick
// +link{calendar.eventDialog, event dialog} and the detailed
// +link{calendar.eventEditor, editor}.
//
// @group i18nMessages
// @visibility external
//<
eventDescriptionFieldTitle: "Description",

//> @attr calendar.eventLaneFieldTitle (HTMLString : "Lane" : IR)
// The title for the +link{calendar.laneNameField, laneNameField} in the quick
// +link{calendar.eventDialog, event dialog} and the detailed
// +link{calendar.eventEditor, editor}.
//
// @group i18nMessages
// @visibility external
//<
eventLaneFieldTitle: "Lane",

//> @attr calendar.eventSublaneFieldTitle (HTMLString : "Sublane" : IR)
// The title for the +link{calendar.sublaneNameField, sublaneNameField} in the quick
// +link{calendar.eventDialog, event dialog} and the detailed
// +link{calendar.eventEditor, event editor}.
// @group i18nMessages
// @visibility external
//<
eventSublaneFieldTitle: "Sublane",

//> @attr calendar.eventDurationFieldTitle (HTMLString : "Duration" : IR)
// The title for the +link{calendar.durationField, duration field} in the quick
// +link{calendar.eventDialog, event dialog} and the detailed
// +link{calendar.eventEditor, editor}.
//
// @group i18nMessages
// @visibility external
//<
eventDurationFieldTitle: "Duration",

//> @attr calendar.eventDurationUnitFieldTitle (HTMLString : "&nbsp;" : IR)
// The title for the +link{calendar.durationUnitField, duration unit field} in the quick
// +link{calendar.eventDialog, event dialog} and the detailed
// +link{calendar.eventEditor, editor}.
//
// @group i18nMessages
// @visibility external
//<
eventDurationUnitFieldTitle: "&nbsp;",

//> @attr calendar.saveButtonTitle (HTMLString : "Save Event" : IR)
// The title for the +link{calendar.saveButton, Save button} in the
// +link{calendar.eventDialog, quick event dialog} and the
// +link{calendar.eventEditor, event editor}.
//
// @group i18nMessages
// @visibility external
//<
saveButtonTitle: "Save Event",

//> @attr calendar.detailsButtonTitle (HTMLString : "Edit Details" : IR)
// The title for the edit button in the quick +link{calendar.eventDialog, quick event dialog}.

//
// @group i18nMessages
// @visibility external
//<
detailsButtonTitle: "Edit Details",

//> @attr calendar.removeButtonTitle (HTMLString : "Remove Event" : IR)
// The title for the +link{calendar.removeButton, Remove button} in the
// +link{calendar.eventEditor, event editor}.
//
// @group i18nMessages
// @visibility external
//<
removeButtonTitle: "Remove Event",

//> @attr calendar.cancelButtonTitle (HTMLString : "Cancel" : IR)
// The title for the +link{calendar.cancelButton, Cancel button} in the
// +link{calendar.eventEditor, event editor}.
//
// @group i18nMessages
// @visibility external
//<
cancelButtonTitle: "Cancel",

//> @attr calendar.monthButtonTitle (HTMLString : "&lt; ${monthName}" : IR)
// The title of the +link{calendar.monthButton, month button}, used for showing and hiding the
// +link{calendar.monthView, month view} on Handsets.
// <P>
// This is a dynamic string - text within <code>&#36;{...}</code> are dynamic variables and will
// be evaluated as JS code when the message is displayed.
// <P>
// Only one dynamic variable, monthName, is available and represents the name of the month
// containing the currently selected date.
// <P>
// The default value is a left-facing arrow followed by the Month-name of the selected date.
// <P>
// When the month view is already visible, the title for the month button is set according to
// the value of +link{calendar.backButtonTitle}.
//
// @group i18nMessages
// @visibility external
//<
monthButtonTitle: "&lt; ${monthName}",

//> @attr calendar.monthMoreEventsLinkTitle (HTMLString : "+ ${eventCount} more..." : IR)
// The title of the link shown in a cell of a +link{calendar.monthView, month view} when there
// are too many events to be displayed at once.
// <P>
// This is a dynamic string - text within <code>&#36;{...}</code> are dynamic variables and will
// be evaluated as JS code when the message is displayed.
// <P>
// Only one dynamic variable, eventCount, is available and represents the number of events that
// are not currently displayed and that will appear in the menu displayed when the More Events
// link is clicked.
// <P>
// The default value is a string like "+ 3 more...".
//
// @group i18nMessages
// @visibility external
//<
monthMoreEventsLinkTitle: "+ ${eventCount} more...",


//> @attr calendar.backButtonTitle (HTMLString : "Back" : IR)
// The title of the +link{calendar.monthButton, month} on Handsets when the
// +link{calendar.monthView, month view} is the current visible view.
// <P>
// When the month view is not the current visible view, the title for the month button is set
// according to the value of +link{calendar.monthButtonTitle}.
//
// @group i18nMessages
// @visibility external
//<
backButtonTitle: "Back",

//> @attr calendar.previousButtonHoverText (String : "Previous" : IR)
// The text to be displayed when a user hovers over the +link{calendar.previousButton, previous}
// toolbar button.
//
// @group i18nMessages
// @visibility external
//<
previousButtonHoverText: "Previous",

//> @attr calendar.nextButtonHoverText (String : "Next" : IR)
// The text to be displayed when a user hovers over the +link{calendar.nextButton, next}
// toolbar button
//
// @group i18nMessages
// @visibility external
//<
nextButtonHoverText: "Next",

//> @attr calendar.addEventButtonHoverText (String : "Add an event" : IR)
// The text to be displayed when a user hovers over the +link{calendar.addEventButton, add event}
// toolbar button
//
// @group i18nMessages
// @visibility external
//<
addEventButtonHoverText: "Add an event",

//> @attr calendar.datePickerHoverText (String : "Choose a date" : IR)
// The text to be displayed when a user hovers over the +link{calendar.datePickerButton, date picker}
// toolbar button
//
// @group i18nMessages
// @visibility external
//<
datePickerHoverText: "Choose a date",

//> @attr calendar.invalidDateMessage (String : "From must be before To" : IR)
// The message to display in the +link{eventEditor} when the 'To' date is greater than
// the 'From' date and a save is attempted.
//
// @group i18nMessages
// @visibility external
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
dateChooserDefaults: {
    left: 0,
    top: -9999,
    autoDraw: false,
    visibility: "hidden",
    autoHide: true,
    showCancelButton: true,
    closeOnEscapeKeypress: true
},

//> @attr calendar.eventDialog (AutoChild Window : null : R)
// An +link{AutoChild} of type +link{Window} that displays a quick event entry form in a
// popup window.
//
// @visibility calendar
//<
eventDialogConstructor: "Window",
eventDialogDefaults: {
    showHeaderIcon: false,
    showMinimizeButton: false,
    showMaximumButton: false,
    canDragReposition: true,
    // so that extra fields are visible without the end user having to tweak bodyProperties
    overflow: "visible",
    bodyProperties: {overflow: "visible"},
    keepInParentRect: true,
    maxWidth: 400,
    height: 100,
    visibility: "hidden",
    autoDraw: false
},

//> @attr calendar.eventEditorLayout (AutoChild Window : null : R)
// An +link{AutoChild} of type +link{Window} that displays the full
// +link{calendar.eventEditor, event editor}
//
// @visibility calendar
//<
eventEditorLayoutConstructor: "Window",
eventEditorLayoutDefaults: {
    showHeaderIcon: false,
    showShadow: false,
    showMinimizeButton: false,
    showMaximumButton: false,
    canDragReposition: false,
    visibility: "hidden",
    autoDraw: false
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
    numCols: 5,
    colWidths: [ 80, 40, 40, "*", "*" ],
    showInlineErrors: false,
    width: 460,
    titleWidth: 80,
    wrapItemTitles: false,
    autoDraw: false
},

//> @attr calendar.eventEditorButtonLayout (AutoChild HLayout : null : R)
// An +link{AutoChild} of type +link{HLayout} which houses the
// +link{calendar.saveButton, Save}, +link{calendar.removeButton, Remove}
// and +link{calendar.cancelButton, Cancel} buttons in the
// +link{calendar.eventEditor, eventEditor}.
//
// @visibility calendar
//<
eventEditorButtonLayoutConstructor: "HLayout",
eventEditorButtonLayoutDefaults: {
    width: "100%", height: "100%",
    membersMargin: 5,
    layoutMargin: 10
},
//> @attr calendar.saveButton (AutoChild IButton : null : R)
// An +link{AutoChild} of type +link{IButton}, used to save an event from the
// +link{calendar.eventEditor, eventEditor}.
//
// @visibility calendar
//<
saveButtonConstructor: "IButton",
saveButtonDefaults: {
    autoFit: true,
    click : function () {
        this.calendar.addEventOrUpdateEventFields();
    }
},
//> @attr calendar.removeButton (AutoChild IButton : null : R)
// An +link{AutoChild} of type +link{IButton}, used to permanently remove an event from the
// +link{calendar.eventEditor, eventEditor}.
//
// @visibility calendar
//<
removeButtonConstructor: "IButton",
removeButtonDefaults: {
    autoFit: true,
    click : function () {
        var cal = this.calendar;
        if (cal.eventRemoveClick(cal.eventEditorLayout.event, cal.getCurrentViewName()) != false) {
            cal.removeEvent(cal.eventEditorLayout.event);
        }
        cal.eventEditorLayout.hide();
    }
},
//> @attr calendar.cancelButton (AutoChild IButton : null : R)
// An +link{AutoChild} of type +link{IButton}, used to cancel editing of an event and close the
// +link{calendar.eventEditor, eventEditor}.
//
// @visibility calendar
//<
cancelButtonConstructor: "IButton",
cancelButtonDefaults: {
    autoFit: true,
    click : function () {
        this.calendar.eventEditorLayout.hide();
    }
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
    autoDraw: false,
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
    showFocused: false,
    autoDraw: false
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
    layoutAlign: "center",
    width: 1,
    height: 1,
    overflow: "visible",
    autoDraw: false,
    layoutMargin: 5,
    membersMargin: 5
},


//> @attr calendar.showMonthButton (Boolean : null : IRW)
// Set to false to prevent the +link{monthButton, Month} button from displaying on Handset
// devices.
// @visibility calendar
//<

//> @attr calendar.monthButton (AutoChild NavigationButton : null : IR)
// A +link{NavigationButton} that appears to the left of other navigation controls in the
// +link{Calendar.controlsBar, controls bar} on Handset devices.
// <P>
// Used to show and hide the +link{calendar.monthView, month view} on devices with limited space.
//
// @visibility calendar
//<
monthButtonConstructor: "NavigationButton",
monthButtonDefaults : {
    click : function () {
        var cal = this.creator,
            currentView = cal.getCurrentViewName()
        ;

        if (currentView != "month") {
            this.previousViewName = currentView;
            this.creator.setCurrentViewName("month");
            cal.updateMonthButton();
        } else {
            this.creator.setCurrentViewName(this.previousViewName);
            delete this.previousViewName;
            cal.updateMonthButton();
        }
    }
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
    if (this.startDate && this.endDate && this.endDate.getTime() < this.startDate.getTime()) {
        // inverted range-dates - flip them and log a warning
        this.logWarn("endDate (" + this.endDate + ") is before startDate (" +
            this.startDate + ") - switching values");
        var sDate = this.startDate.duplicate();
        this.startDate = this.endDate.duplicate();
        this.endDate = sDate;
    }
    if (!this.chosenDate) {

        if (this.startDate) this.chosenDate = this.startDate.duplicate();
        else this.chosenDate = new Date();
    }

    // if showWeekends is false, don't allow the chosenDate to be a weekend
    if (!this.showWeekends && isc.DateUtil.isWeekend(this.chosenDate, this.getWeekendDays())) {
        this.chosenDate = isc.DateUtil.getNextWeekday(this.chosenDate, this.getWeekendDays());
    }

    // set year and month consistently with the appropriate display date
    var displayDate = isc.Calendar._getAsDisplayDate(this.chosenDate);
    this.year = displayDate.getFullYear();
    this.month = displayDate.getMonth();
    this.displayDate = displayDate.getDate();

    if (this.twentyFourHourTime == null) {
        // unset - assume the global default so that editors are inline with formatters
        this.twentyFourHourTime = isc.Time.use24HourTime;
    } else {
        if (!this.timeFormatter) {
            // no timeFormatter - default it based on twentyFourHourTime
            this.timeFormatter = this.twentyFourHourTime ? "toShortPadded24HourTime" :
                "toShortPaddedTime";
        }
    }

    if (this.firstDayOfWeek == null)
        this.firstDayOfWeek = Number(isc.DateChooser.getInstanceProperty("firstDayOfWeek"));

    // use the system-wide set of weekendDays if none were supplied
    if (!this.weekendDays) this.weekendDays = isc.DateUtil.getWeekendDays();

    if (this.laneGroupByField && !isc.isAn.Array(this.laneGroupByField)) {
        this.laneGroupByField = [this.laneGroupByField];
    }

    if (this.showTimelineView) {
        // prepare the initial timeline column-span
        if (this.endDate) {
            // calculate columns per unit in the range
            this.timelineColumnSpan = Math.round(isc.DateUtil.getPeriodLength(this.startDate,
                this.endDate, this.timelineGranularity) / this.timelineUnitsPerColumn);
        } else {
            //// otherwise, apply the default columnSpan
            if (!this.timelineColumnSpan) this.timelineColumnSpan = this.defaultTimelineColumnSpan;
        }
    }

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

    // on touch devices, drag gestures are expected to scroll the view by default, rather than
    // creating or repositioning events - if the attributes for these features are unset,
    // default them now - false for touch browsers, true otherwise
    var isTouch = isc.Browser.isTouch ? true : false;
    if (this.canDragCreateEvents == null) this.canDragCreateEvents = !isTouch;
    if (this.canDragEvents == null) this.canDragEvents = !isTouch;

    if (this.minimalUI == null) this.minimalUI = isc.Browser.isHandset;
    if (this.minimalUI) {
        // if Browser.isHandset, don't show the tabBar for switching views.  Instead,
        // register a handler for the Page orientationChange event and switch views according
        // to orientation - landscape == weekView, portrait == dayView
        this.mainViewDefaults.showTabBar = false;
        var _this = this;
        this.orientationEventId = isc.Page.setEvent("orientationChange", function () {
            _this.pageOrientationChanged();
        });
    }
    //<!BackCompat

    if (this.overlapSortSpecifiers && !isc.isAn.Array(this.overlapSortSpecifiers)) {
        this.overlapSortSpecifiers = [this.overlapSortSpecifiers];
    }

    if (!this.data) this.data = this.getDefaultData();

    // create the selection model
    if (this.canSelectEvents) this.createSelectionModel();

    // set hover text strings for toolbar buttons
    // can't set dynamically in defaults block, so have to do it here.
    this.previousButtonDefaults.prompt = this.previousButtonHoverText;
    this.nextButtonDefaults.prompt = this.nextButtonHoverText;
    this.datePickerButtonDefaults.prompt = this.datePickerHoverText;
    this.addEventButtonDefaults.prompt  = this.addEventButtonHoverText;

    if (this.dataSource) this.autoDetectFieldNames();

    // initialize the data object, setting it to an empty array if it hasn't been defined
    //this.setData(null);

    if (this.showColumnLayouts) {
        // when showColumnLayouts is true, the views are essentially not showing times at all
        // - some features need to switched off or set up
        this.showLabelColumn = false;
        this.canResizeEvents = false;
        this.renderEventsOnDemand = false;
    }

    this.invokeSuper(isc.Calendar, "initWidget");

    this.createChildren();
},

updateMonthButton : function () {
    if (this.getCurrentViewName() == "month") {
        this.monthButton.setTitle(this.backButtonTitle);
    } else {
        var month = this.chosenDate.getMonthName();
        this.monthButton.setTitle(
            this.monthButtonTitle.evalDynamicString(this, { monthName: month })
        );
    }
},

pageOrientationChanged : function (orientation) {
    orientation = orientation || isc.Page.getOrientation();
    if (orientation == "landscape" && this.weekView) this.setCurrentViewName("week");
    if (orientation == "portrait" && this.dayView) this.setCurrentViewName("day");
},

autoDetectFieldNames : function () {
    this.dataSource = isc.DS.getDataSource(this.dataSource);

    // pick some likely looking fields if no sensible ones are provided - wants
    // for some future cleverness, perhaps, pretty basic selection here

    var ds = this.dataSource,
        fields = isc.getValues(ds.getFields()),
        maxSize = 1024000,
        bestField = null,
        field
    ;

    if (this.fieldIsMissing(this.nameField, ds)) {
        // assume the titleField from the DS if the
        this.nameField = ds.getTitleField();
        if (this.fieldIsMissing(this.nameField, ds)) {
            this.logWarn("Specified field '" + this.nameField + "' is not present in " +
                "the DataSource and no suitable alternative was auto-detected.");
        } else {
            // log that the expected field was not in the DS, but an alternative was auto-detected
            this.logInfo("Specified event name field is not present in the DataSource - " +
                "using DataSource.getTitleField() instead: '" + this.nameField + "'");
        }
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
        if (bestField != null && this.fieldIsMissing(this.descriptionField, ds))
            this.descriptionField = bestField.name;

        if (this.fieldIsMissing(this.descriptionField, ds)) {
            this.logWarn("Specified field '" + this.descriptionField + "' is not present in " +
                "the DataSource and no suitable alternative was auto-detected.");
        } else {
            // log that the expected field was not in the DS, but an alternative was auto-detected
            this.logInfo("Specified event description field is not present in the DataSource - " +
                "using auto-detected field '" + this.descriptionField + "' instead.");
        }
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
        if (bestField != null && this.fieldIsMissing(this.startDateField, ds))
            this.startDateField = bestField.name;

        if (this.fieldIsMissing(this.startDateField, ds)) {
            this.logWarn("Specified field '" + this.startDateField + "' is not present in " +
                "the DataSource and no suitable alternative was auto-detected.");
        } else {
            // log that the expected field was not in the DS, but an alternative was auto-detected
            this.logInfo("Specified event startDate field is not present in the DataSource - " +
                "using auto-detected field '" + this.startDateField + "' instead.");
        }
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
        if (bestField != null && this.fieldIsMissing(this.endDateField, ds))
            this.endDateField = bestField.name;

        if (this.fieldIsMissing(this.endDateField, ds)) {
            this.logWarn("Specified field '" + this.endDateField + "' is not present in " +
                "the DataSource and no suitable alternative was auto-detected.");
        } else {
            // log that the expected field was not in the DS, but an alternative was auto-detected
            this.logInfo("Specified event endDate field is not present in the DataSource - " +
                "using auto-detected field '" + this.endDateField + "' instead.");
        }
    }
    if (this.showTimelineView != false || (this.showDayView != false && this.showDayLanes)) {
        // the DS must have lane and possibly sublane fields in it
        if (this.useSublanes && this.fieldIsMissing(this.sublaneNameField, ds)) {
            // loop and find a string field containing the word "sublane"
            bestField = null;
            for (var i=0; i<fields.length; i++) {
                field = fields.get(i);
                if (!field.type || field.type == "text" || field.type == "string") {
                    var fName = field.name.toLowerCase();
                    if (fName.contains("sublane")) {
                        this.sublaneNameField = field.name;
                        break;
                    }
                }
            }
            if (this.fieldIsMissing(this.sublaneNameField, ds)) {
                this.logWarn("Specified field '" + this.sublaneNameField + "' is not present in " +
                    "the DataSource and no suitable alternative was auto-detected.");
            } else {
                // log that the expected field was not in the DS, but an alternative was auto-detected
                this.logInfo("Specified event sublane field is not present in the DataSource - " +
                    "using auto-detected field '" + this.sublaneNameField + "' instead.");
            }
        }

        if (this.fieldIsMissing(this.laneNameField, ds)) {
            // loop and find a string field containing the word "lane", but not "sublane"
            bestField = null;
            for (var i=0; i<fields.length; i++) {
                field = fields.get(i);
                if (!field.type || field.type == "text" || field.type == "string") {
                    var fName = field.name.toLowerCase();
                    if (fName.contains("lane") && fName != this.sublaneNameField) {
                        this.laneNameField = field.name;
                        break;
                    }
                }
            }
            if (this.fieldIsMissing(this.laneNameField, ds)) {
                this.logWarn("Specified field '" + this.laneNameField + "' is not present in " +
                    "the DataSource and no suitable alternative was auto-detected.");
            } else {
                // log that the expected field was not in the DS, but an alternative was auto-detected
                this.logInfo("Specified event lane field is not present in the DataSource - " +
                    "using auto-detected field '" + this.laneNameField + "' instead.");
            }
        }

        // if the type of the specified laneNameField is not text-based, log a warning
        var laneNameField = ds && ds.getField(this.laneNameField);
        if (laneNameField) {
            if (!isc.SimpleType.inheritsFrom(laneNameField.type, "text")) {
                this.logWarn("Calendar.laneNameField must be set to a text-based field since " +
                    "its values may be used as identifiers.  The specified field " +
                    "'" + this.laneNameField + "' is of type '" + laneNameField.type + "'.");
            }
        }
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
// @param newData (Array of CalendarEvent[]) data to show in the list
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
    this.observe(this.data, "dataChanged", "observer.dataChanged(arguments)");
    if (this.hasData()) {
        // invoke dataChanged so calendar refreshes when passed new data
        this.dataChanged("fetch");
    }
},

//> @method calendar.getData()
// Get the data that is being displayed and observed
//
// @return (Object) The data that is being displayed and observed
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

    //this.logWarn("*** dataChanged(" + isc.echoFull(arguments) + ")");

    if (!this.dataIsAvailable()) {
        this._ignoreDataChanged = true;
        this._observeDataArrived = true;
    } else {
        delete this._observeDataArrived;
    }
    // see addEvent, updateEvent, deleteEvent, and comment above about _ignoreDataChanged
    if (this._ignoreDataChanged) {
        this.logDebug('dataChanged, ignoring','calendar');
        this._ignoreDataChanged = false;
    } else {
        this.logDebug('dataChanged, refreshing', 'calendar');
        delete this._observeDataArrived;
        this.refreshSelectedView();
    }

},

dataIsAvailable : function () {
    if (isc.isAn.Array(this.data)) return true;
    if (this.data.allMatchingRowsCached()) return true;
    return false;
},

destroy : function () {
    if (this.orientationEventId) isc.Page.clearEvent("orientationChange", this.orientationEventId);
    if (this.data) {
        // ignore observations on the data and clear it
        if (this.isObserving(this.data, "dataChanged")) this.ignore(this.data, "dataChanged");
        if (this.isObserving(this.data, "dataArrived")) this.ignore(this.data, "dataArrived");
        if (this.data.destroy) this.data.destroy();
        this.data = null;
    }
    if (this.controlsBar) this.controlsBar.destroy();
    if (this.controlsBarContainer) this.controlsBarContainer.destroy();
    if (this.dateChooser) this.dateChooser.destroy();
    if (this.eventCanvasButtonLayout) this.eventCanvasButtonLayout.destroy();
    if (this.mainLayout) this.mainLayout.destroy();
    this.Super("destroy", arguments);
},

refreshSelectedView : function () {
    var view = this.getSelectedView();
    // bail if no selected view, or it isn't drawn yet
    if (!view) return;
    if (!view.isDrawn()) {
        view._refreshEventsOnDraw = true
        return;
    }
    if (this.dayViewSelected()) {
        this.dayView.delayedRefreshEvents("refreshSelectedView");
        if (this.weekView) this.weekView._needsRefresh = true;
        if (this.monthView) this.monthView._needsRefresh = true;
    } else if (this.weekViewSelected()) {
        this.weekView.delayedRefreshEvents("refreshSelectedView");
        if (this.dayView) this.dayView._needsRefresh = true;
        if (this.monthView) this.monthView._needsRefresh = true;
    } else if (this.monthViewSelected()) {
        this.monthView.refreshEvents("refreshSelectedView");
        if (this.dayView) this.dayView._needsRefresh = true;
        if (this.weekView) this.weekView._needsRefresh = true;
    } else if (this.timelineViewSelected()) {
        this.timelineView.delayedRefreshEvents("refreshSelectedView");
    }
},

//> @method calendar.getSelectedView()
// Returns the currently selected +link{CalendarView, view}.
// @return (CalendarView) the currently selected view
// @visibility external
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

//> @method calendar.getView()
// Returns the +link{CalendarView, view} with the passed +link{ViewName, name}.
// @param viewName (ViewName) the name of the CalendarView to return
// @return (CalendarView) the currently selected view
// @visibility external
//<
getView : function (viewName) {
    if (!viewName) return this.getSelectedView();
    if (viewName == "day") return this.dayView;
    if (viewName == "week") return this.weekView;
    if (viewName == "month") return this.monthView;
    if (viewName == "timeline") return this.timelineView;
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

//> @attr calendar.minRowHeight (number : 20 : IRW)
// The minimum height of time-rows in vertical calendar views.  Rows will not shrink below
// this height when +link{sizeToWorkday} is true, meaning that a Calendar with a long workday
// may not be able to fit all workday rows in the viewport at once, and scrolling may be
// necessary.
// <P>
// To prevent users from scrolling beyond the workday hours, see +link{limitToWorkday}.
// @visibility external
//<
minRowHeight: isc.ListGrid.getInstanceProperty("cellHeight"),

setRowHeight : function (newHeight, skipScroll) {
    this.rowHeight = newHeight;
    if (this.dayView) {
        this.dayView.setCellHeight(this.rowHeight);
        this.dayView.refreshEvents("setRowHeight");
        if (this.scrollToWorkday && !skipScroll) this.dayView.scrollToWorkdayStart();
    }
    if (this.weekView) {
        this.weekView.setCellHeight(this.rowHeight);
        this.weekView.refreshEvents("setRowHeight");
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
    // set _selectedViewName because getSelectedView(), which is called pervasively, returns
    // the view with that name
    this._selectedViewName = viewName;
    if (this.mainView && this.mainView.tabs) {
        var tabToSelect = this.mainView.tabs.findIndex("viewName", viewName);
        if (tabToSelect != null) {
            this.selectTab(tabToSelect);
        }
    }
    return viewName;
},

// get/setEventCanvasID ensure that eventCanvas-to-event mapping remains stable when databound.
// The expando approach doesn't work when databound because the expando gets wiped out
// on update.
getEventPKs : function (ds) {
    if (!this._eventPKs) {
        ds = ds || this.getDataSource();
        if (ds) {
            this._eventPKs = ds.getPrimaryKeyFieldNames();
        }
    }
    return this._eventPKs || [];
},
getRandomEventKey : function () {
    return "_event_" + (Math.random()*100) + "_" + ((Math.random()*Math.random()) * 100);
},
_eventKeySB:null,
getEventKey : function (event) {
    if (!event.__key) {
        if (!this.dataSource) {
            event.__key = this.getRandomEventKey();
            return event.__key;
        } else {
            var pks = this.getEventPKs().duplicate(),
                eventKey = this._eventKeySB
            ;
            if (!eventKey) eventKey = isc.StringBuffer.create();
            if (pks.length > 0) {
                eventKey.append(this.getID(), "_event_");
                for (var i=0; i<pks.length; i++) {
                    eventKey.append(event[pks[i]]);
                    if (i==pks.length) break;
                }
            }
            var result = eventKey.release(false);
            event.__key = result;
        }
    }
    return event.__key;
},

//< @method calendar.clearViewSelection()
// When overriding +link{calendar.backgroundClick} and returning false to suppress default
// behavior, use this method to clear the selection from the day, week and timeline views.
// @param [view] (CalendarView) The view to clear the selection in - if not passed, clears
//                            all views
// @visibility internal
//<
clearViewSelection : function (view) {
    if (view) {
        if (view.clearSelection) view.clearSelection();
    } else {
        // clear the selection on appropriate views
        if (this.dayView) this.dayView.clearSelection();
        if (this.weekView) this.weekView.clearSelection();
        if (this.timelineView) this.timelineView.clearSelection();
    }
},

// includes start date but not end date
getDayDiff : function (date1, date2, weekdaysOnly) {
    return Math.abs(isc.DateUtil._getDayDiff(date1, date2, weekdaysOnly, false,
                                             this.getWeekendDays()));
},

getEventStartCol : function (event, eventCanvas, calendarView) {
    var view = calendarView || (eventCanvas ? eventCanvas.calendarView : this.getSelectedView()),
        canvas = eventCanvas || view.getCurrentEventCanvas(event),
        startCol = view.getEventColumn(canvas.getLeft() + 1);
    return startCol;
},

getEventEndCol : function (event, eventCanvas, calendarView) {
    var view = view || (eventCanvas ? eventCanvas.calendarView : this.getSelectedView()),
        canvas = eventCanvas || view.getCurrentEventCanvas(event),
        endCol = view.getEventColumn(canvas.getLeft() + canvas.getVisibleWidth() + 1);
    return endCol;
},

// helper method for getting the left coordinate of an event
getEventLeft : function (event, view) {
    view = view || this.getSelectedView();

    if (view.getEventLeft) return view.getEventLeft(event);

    var colSize = view.body.getColumnWidth(0),
        eLeft = 0
    ;
    if (view.isWeekView()) {
        var eLeft = view.getColumnLeft(view.getColFromDate(this.getEventStartDate(event)));
    } else if (this.showDayLanes) {
        var fieldId = view.completeFields.findIndex("name", event[this.laneNameField]);
        if (fieldId) {
            eLeft = view.body.getColumnLeft(fieldId);
        }
    } else {
        var fieldId = view.getColFromDate(this.getEventStartDate(event));
        if (fieldId) {
            eLeft = view.body.getColumnLeft(fieldId);
        }
    }
    if (this.logIsDebugEnabled("calendar")) {
        this.logDebug('calendar.getEventLeft() = ' + eLeft + ' for:' + isc.Log.echoFull(event), 'calendar');
    }
    return eLeft;
},

//> @method calendar.getEventHeaderHTML()
// Returns the title text for the passed event, which is displayed in the header area of an
// eventCanvas rendered in a vertical or horizontal view, or as a clickable link in a cell in a
// +link{calendar.showMonthView, Month view}.
// <P>
// The default implementation returns the event's
// +link{calendar.nameField, name field} for timelines, and that same value pre-pended with
// the event's +link{calendar.startDateField, start} for day, week and month views.
//
// @param event (CalendarEvent) the event to get the description text for
// @param [view] (CalendarView) the view in which the event is being rendered
// @return (HTMLString) the HTML to display in the header of an event canvas
// @visibility external
//<
getEventHeaderHTML : function (event, view) {
    if (!event) return null;
    var sDate = this.getEventStartDate(event),
        fTime = isc.Time.toShortTime(sDate, this.timeFormatter, false),
        sTime = (view.isTimelineView() ? null : fTime),
        eTitle = (sTime ? sTime + " " : ""),
        name = event[this.nameField]
    ;
    // support eventCanvas.escapeHTML for event-name, but don't escape the bolded datetimes
    var canvas = view.getCurrentEventCanvas(event);
    if (name && canvas && canvas.escapeHTML) {
        name = name.asHTML();
    }
    return eTitle + (name || "");
},

//> @method calendar.getEventBodyHTML()
// Returns the description text for the passed event, for display in the body area of an event
// canvas.  The default implementation returns the event's
// +link{calendar.descriptionField, description field}.
//
// @param event (CalendarEvent) the event to get the description text for
// @param [view] (CalendarView) the view in which the event is being rendered
// @return (HTMLString) the HTML to display in the body of the passed event's EventCanvas
// @visibility external
//<
getEventBodyHTML : function (event, view) {
    if (!event) return null;
    var defaultValue = event[this.descriptionField];
    // support eventCanvas.escapeHTML for event-body HTML
    var canvas = view.getCurrentEventCanvas(event);
    if (defaultValue && canvas && canvas.escapeHTML) {
        defaultValue = defaultValue.asHTML();
    }
    return defaultValue;
},

getEventLeadingDate : function (event, view) {
// return a copy of the leadingDate for the passed event, if it has one
    if (!event) return null;
    var date = event[this.leadingDateField];
    return date ? date.duplicate() : null;
},

getEventTrailingDate : function (event, view) {
// return a copy of the trailingDate for the passed event, if it has one
    if (!event) return null;
    var date = event[this.trailingDateField];
    return date ? date.duplicate() : null;
},

//> @method calendar.getEventStartDate()
// Returns the +link{calendarEvent.startDate, start date} of the passed event.
//
// @param event (CalendarEvent) the event to get the start date of
// @return (Date) the start date of the passed event
// @visibility external
//<
getEventStartDate : function (event, view) {
    if (!event || !event[this.startDateField]) return null;
// return a copy of the startDate for the passed event
    return event[this.startDateField].duplicate();
},

// helper to return all four dates associated with an event (leading, start, end, trailing)
getEventDates : function (event, view) {
    // return
    var result = {};
    result[this.leadingDateField] = this.getEventLeadingDate(event);
    result[this.startDateField] = this.getEventStartDate(event);
    result[this.endDateField] = this.getEventEndDate(event);
    result[this.trailingDateField] = this.getEventTrailingDate(event);
    return result;
},

//> @method calendar.getEventEndDate()
// Returns the +link{calendar.endDateField, end date} of the passed event.  If the event is
// +link{calendar.allowDurationEvents, duration-based}, the result is calculated from the
// +link{calendarEvent.startDate, start date} and the specified
// +link{calendarEvent.duration, duration} and +link{calendarEvent.durationUnit, unit}.
//
// @param event (CalendarEvent) the event to get the start date of
// @return (Date) the end date of the passed event
// @visibility external
//<
getEventEndDate : function (event, view) {
    if (!event) return null;
    var duration = this.getEventDuration(event),
        date = event[this.endDateField]
    ;
    if (duration != null) {
        // there's a duration specified - calculate an end date
        var unit = this.getEventDurationUnit(event) || "mn"
        date = this.getEventStartDate(event);
        if (unit) {
            date = isc.DateUtil.dateAdd(date, unit, duration, null, null, null,
                this.firstDayOfWeek);
        }
    }
    return date ? date.duplicate() : null;
},

// this is the default width at which to draw zero-length events in timelines - the general
// event padding is added to this so that the event is visible
zeroLengthEventSize: 2,
isDurationEvent : function (event) {
    return (!event[this.endDateField] && event[this.durationField] != null);
},

// return the duration of the passed event
getEventDuration : function (event, view) {
    return event[this.durationField];
},

// return the durationUnit of the passed event, of the default of "mn"
_$defaultEventDurationUnit: "mn",
getEventDurationUnit : function (event, view) {
    return event[this.durationUnitField] || this._$defaultEventDurationUnit;
},

isZeroLengthEvent : function (event) {
    var isDuration = this.isDurationEvent(event),
        isZeroLength = isDuration && this.getEventDuration(event) == 0
    ;
    return isZeroLength;
},

//> @method calendar.setShowWeekends()
// Setter for updating +link{calendar.showWeekends} at runtime.
//
// @param showWeekends (boolean) whether or not to show weekends
// @visibility calendar
//<
setShowWeekends : function (showWeekends) {
    if (showWeekends == this.showWeekends) return;
    this.showWeekends = showWeekends;
    // rebuild the views, they might all show weekends
    this.dayView && this.dayView.rebuild();
    this.weekView && this.weekView.rebuild();
    this.monthView && this.monthView.rebuild();
    this.timelineView && this.timelineView.rebuild();
    this.setDateLabel();
},

//> @method calendar.canEditEvent()
// Method called whenever the calendar needs to determine whether a particular event should be
// editable.
// <P>
// By default, returns the +link{canEditField} on the provided +link{CalendarEvent} if its set,
// and +link{canEditEvents} otherwise.
// <P>
// Note that vertical views do not support editing of multi-day events.
// @param event (CalendarEvent)
// @return (boolean) whether the user should be allowed to edit the provided CalendarEvent
//<
canEditEvent : function (event, view) {
    if (!event) return false;
    view = view || this.getSelectedView();
    // event has a canEdit setting
    if (event[this.canEditField] != null) return event[this.canEditField];
    // indicator-events are zero-duration and can be drag-moved if editing is enabled
    if (event.duration == 0) return this.canEditEvents;

    // only Timelines can edit multi-day events at the moment
    if (!view.allowMultiDayEvents) {

        var start = isc.DateUtil.getLogicalDateOnly(this.getEventStartDate(event)),
            // the end date needs 1ms taking off it, in case it ends at midnight
            end = isc.DateUtil.getLogicalDateOnly(isc.DateUtil.adjustDate(this.getEventEndDate(event), "-1ms"))
        ;
        if (isc.DateUtil.compareLogicalDates(start, end) != 0) {
            // multi-day event - editing only supported if allowMultiDayEvents is true (timelines)
            return false;
        }
    }
    return this.canEditEvents;
},

//> @method calendar.canDragEvent()
// Method called whenever the calendar needs to determine whether a particular event should be
// draggable.
// <P>
// By default, returns false if +link{calendar.canEditEvent, canEditEvent} returns false.
// Otherwise, checks the +link{canDragEventField} on the provided +link{CalendarEvent}, and
// if null, returns +link{calendar.canDragEvents}.
// <P>
// See +link{calendar.canResizeEvent, canResizeEvent} for finer control of drag operations.
//
// @param event (CalendarEvent)
// @return (boolean) whether the user should be allowed to drag the provided CalendarEvent
//<
canDragEvent : function (event, view) {
    view = view || this.getSelectedView();
    if (!event || !this.canEditEvent(event, view)) return false;
    if (event[this.canDragEventField] != null) return event[this.canDragEventField];
    else return this.canDragEvents;
},

//> @method calendar.canResizeEvent()
// Method called whenever the calendar needs to determine whether a particular event can be
// resized by dragging.
// <P>
// By default, drag-resizing requires that +link{calendar.canEditEventField, editing} and
// +link{calendar.canDragEventField, dragging} be switched on.  If they aren't, this method
// returns false.  Otherwise, returns +link{calendar.canResizeEventField, canResize} on the
// provided +link{CalendarEvent} if its set, and +link{calendar.canEditEvents, canEditEvents}
// if not.
//
// @param event (CalendarEvent)
// @return (boolean) whether the user should be allowed to edit the provided CalendarEvent
//<
canResizeEvent : function (event, view) {
    view = view || this.getSelectedView();
    if (!event || !this.canEditEvent(event, view) || !this.canDragEvent(event, view)) return false;
    // showColumnLayouts:true causes canvases to auto-size - no manual resizing
    if (view.shouldShowColumnLayouts()) return false;
    if (event[this.canResizeEventField] != null) return event[this.canResizeEventField];
    // if the passed event is a "duration" and the duration is zero, this is considered an
    // "instant", a moment in time.  For such events, disallow drag-resize - it doesn't really
    // make sense - drag-move is ok
    else if (this.isZeroLengthEvent(event)) return false;
    else return this.canResizeEvents;
},

//> @method calendar.canRemoveEvent()
// Method called whenever the calendar needs to determine whether a particular event should show
// a remove button to remove it from the dataset.
// <P>
// By default, checks the +link{canRemoveField} on the provided +link{CalendarEvent}, and if
// null, returns true if +link{calendar.canRemoveEvents, canRemoveEvents} is true and
// +link{calendar.canEditEvent, canEditEvent} also returns true.
//
// @param event (CalendarEvent)
// @return (boolean) whether the user should be allowed to remove the provided CalendarEvent
//<
canRemoveEvent : function (event, view) {
    if (!event) return false;
    // return the canRemoveField value if its set
    if (event[this.canRemoveField] != null) return event[this.canRemoveField];
    // return true if canRemoveEvents is true AND the event is editable
    return this.canRemoveEvents && this.canEditEvent(event, view || this.getSelectedView());
},

getDateEditingStyle : function () {
    // ensure backward compatibility
    var view = this.getSelectedView();
    if (view.dateEditingStyle) {
        return view.dateEditingStyle;
    }
    var result = this.dateEditingStyle;
    if (!result) {
        // auto-detect based on field-type
        if (this.dataSource) result = this.getDataSource().getField(this.startDateField).type;

        // default to datetime
        if (!result) {
            switch (this.timelineGranularity) {
                case "day":
                    if (!this.timelineView) result = "date";
                    else if (this.getSnapGapPixels(this.timelineView) < this.timelineView.columnWidth) {
                        // if each cell is a day, return "datetime" if there are snapGaps within
                        // cells (meaning times must be applicable), or "date" otherwise
                        result = "datetime";
                    } else {
                        result = "date";
                    }
                    break;
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
// For +link{Timeline}s, and for +link{calendar.dayView, dayView} with
// +link{calendar.showDayLanes, showDayLanes} set, creates a new event and adds it to a
// particular +link{Lane}.
//
// @param laneName        (Lane) the Lane in which to add this event
// @param startDate       (Date | Object) start date of event, or CalendarEvent Object
// @param [endDate]       (Date) end date of event
// @param [name]          (String) name of event
// @param [description]   (String) description of event
// @param [otherFields]   (Any) new values of additional fields to be updated
//
// @visibility calendar
// @deprecated in favor of +link{calendar.addCalendarEvent}
//<
addLaneEvent : function (laneName, startDate, endDate, name, description, otherFields) {
    otherFields = otherFields || {};
    var newEvent = this.createEventObject(null, startDate, endDate,
            laneName, otherFields[this.sublaneNameField], name, description);
    this.addCalendarEvent(newEvent, otherFields);
},

getCleanEventRecord : function (event) {
    if (isc.propertyDefined(event, "overlapProps")) delete event.overlapProps;
    if (isc.propertyDefined(event, "overlapRangeId")) delete event.overlapRangeId;
    if (isc.propertyDefined(event, "slotNum")) delete event.slotNum;
    if (isc.propertyDefined(event, "tagged")) delete event.tagged;
    //if (isc.propertyDefined(event, "__key")) delete event["__key"];
    return event;
},

createEventObject : function (sourceEvent, start, end, lane, sublane, name, description) {
    var newEvent = isc.addProperties({}, sourceEvent);
    if (start) newEvent[this.startDateField] = start;
    if (end) newEvent[this.endDateField] = end;
    if (lane) newEvent[this.laneNameField] = lane;
    if (sublane) newEvent[this.sublaneNameField] = sublane;
    if (name) newEvent[this.nameField] = name;
    if (description) newEvent[this.descriptionField] = description;
    // scrap the eventLength - it will be recalculated later
    delete newEvent.eventLength;

    delete newEvent.__ref;
    return newEvent;
},

//> @method calendar.addEvent()
// Create a new event in this calendar instance.
//
// @param startDate       (Date | CalendarEvent) start date of event, or CalendarEvent Object
// @param [endDate]       (Date) end date of event
// @param [name]          (String) name of event
// @param [description]   (String) description of event
// @param [otherFields]   (Object) new values of additional fields to be updated
//
// @visibility calendar
// @deprecated in favor of +link{calendar.addCalendarEvent}
//<
addEvent : function (startDate, endDate, name, description, otherFields, laneName, ignoreDataChanged) {
    otherFields = otherFields || {};
    var newEvent;
    if (isc.isA.Date(startDate)) {
        newEvent = this.createEventObject(null, startDate, endDate,
            laneName || otherFields[this.laneNameField],
            otherFields[this.sublaneNameField], name, description);
    } else if (isc.isAn.Object(startDate)) {
        newEvent = startDate;
    } else {
        isc.logWarn('addEvent error: startDate parameter must be either a Date or a CalendarEvent');
        return;
    }
    this.addCalendarEvent(newEvent, otherFields, ignoreDataChanged);
},

//> @method calendar.addCalendarEvent()
// Create a new event in this calendar.
// <P>
// In all cases, the +link{CalendarEvent, event} passed as the first parameter must have at
// least a +link{calendar.startDateField, start date} set.  If the calendar is showing
// +link{calendar.lanes, lanes}, the name of the +link{calendarEvent.lane, lane} and, if
// applicable, the +link{calendarEvent.sublane, sublane}, must also be set.
//
// @param event (CalendarEvent) the new calendar event to add
// @param [customValues] (Object) additional, custom values to be saved with the event
//
// @visibility calendar
//<
addCalendarEvent : function (event, customValues, ignoreDataChanged) {
    if (!event) return;
    // We explicitly update the UI in this method, so no need to react to 'dataChanged' on the
    // data object
    if (ignoreDataChanged == null) ignoreDataChanged = true;

    var start = this.getEventStartDate(event);
    if (!isc.isA.Date(start)) {
        isc.logWarn('addCalendarEvent: passed event has no start date');
        return;
    }

    // combine the customValues onto the event
    event = this.getCleanEventRecord(isc.addProperties(event, customValues));

    // add event to data
    // see comment above dataChanged about _ignoreDataChanged
    if (ignoreDataChanged) this._ignoreDataChanged = true;
    if (this.dataSource) {
        var _this = this;
        var requestProps = {componentId: this.ID, willHandleError: true};
        if (this.addOperation) requestProps.operationId = this.addOperation;
        isc.DataSource.get(this.dataSource).addData(event, function (dsResponse, data, dsRequest) {
            _this.processSaveResponse(dsResponse, data, dsRequest);
        }, requestProps);
        return;
    } else {
        // set the one-time flag to ignore data changed since we manually refresh in _finish()
        this._ignoreDataChanged = true;
        this.data.add(event);
        this.processSaveResponse({status:0}, [event], {operationType: "add"});
    }

},

//> @method calendar.removeEvent()
// Remove an event from this calendar.
//
// @param event (CalendarEvent) The event object to remove from the calendar
//
// @visibility calendar
//<
removeEvent : function (event, ignoreDataChanged) {
    // We explicitly update the UI in this method, so no need to react to 'dataChanged' on the
    // data object
    if (ignoreDataChanged == null) ignoreDataChanged = true;

    var startDate = this.getEventStartDate(event),
        endDate = this.getEventEndDate(event);

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
            self.monthView.refreshEvents("removeEvent");
        }
        if (self._shouldRefreshTimeline(startDate, endDate)) {
            self.timelineView.removeEvent(event);
            // if not databound, recalculate overlaps for other events in the associated lane
            if (!self.dataSource) self.timelineView.retagLaneEvents(event[self.laneNameField]);
        }
        // when eventAutoArrange is true, refresh the day and week views to reflow the events
        // so that they fill any space made available by the removed event
        if (self.eventAutoArrange) {
            if (self.dayView) {
                if (self.dayView.isSelectedView()) self.dayView.refreshEvents("removeEvent");
                else self.dayView._needsRefresh = true;
            }
            if (self.weekView) {
                if (self.weekView.isSelectedView()) self.weekView.retagDayEvents(startDate);
                else self.weekView._needsRefresh = true;
            }
        }
        // fire eventRemoved if present
        if (self.eventRemoved) self.eventRemoved(event);
    };
    // remove the data
    // see comment above dataChanged about _ignoreDataChanged
    if (ignoreDataChanged) this._ignoreDataChanged = true;
    event = this.getCleanEventRecord(event);
    if (this.dataSource) {
        var requestProps = { componentId: this.ID, oldValues : event, willHandleError: true };
        if (this.removeOperation) requestProps.operationId = this.removeOperation;
        isc.DataSource.get(this.dataSource).removeData(event, _finish, requestProps);
        return;
    } else {
        this.data.remove(event);
        _finish();
    }

},

//> @method calendar.updateEvent()
// Update an event in this calendar.
//
// @param event       (CalendarEvent) The event object to update
// @param startDate   (Date) start date of event
// @param endDate     (Date) end date of event
// @param name        (String) name of event
// @param description (String) description of event
// @param otherFields (Object) new values of additional fields to be updated
//
// @visibility calendar
// @deprecated in favor of +link{calendar.updateCalendarEvent}
//<
updateEvent : function (event, startDate, endDate, name, description, otherFields, ignoreDataChanged, laneName, sublaneName) {
    // We explicitly update the UI in this method, so no need to react to 'dataChanged' on the
    // data object
    if (ignoreDataChanged == null) ignoreDataChanged = true;

    if (!isc.isAn.Object(otherFields)) otherFields = {};

    var newEvent = this.createEventObject(event, startDate, endDate,
            laneName || otherFields[this.laneNameField],
            sublaneName || otherFields[this.sublaneNameField], name, description
    );

    this.updateCalendarEvent(event, newEvent, otherFields, ignoreDataChanged);
},

//> @method calendar.updateCalendarEvent()
// Update an event in this calendar.
//
// @param event (CalendarEvent) The event object that will be updated
// @param newEvent (CalendarEvent) The new attributes for the event
// @param otherFields (Object) new values of additional fields to be updated
//
// @visibility calendar
//<
updateCalendarEvent : function (event, newEvent, otherFields, ignoreDataChanged) {
    // see comment above dataChanged about _ignoreDataChanged
    if (ignoreDataChanged) this._ignoreDataChanged = true;
    otherFields = otherFields || {};

    var view = this.getSelectedView();
    var canvas = view.getCurrentEventCanvas(event);
    if (canvas) {
        if (this.dataSource) view.clearEventCanvas(canvas);
    }

    if (this.dataSource) {
        var ds = isc.DataSource.get(this.dataSource);
        var updatedRecord = this.getCleanEventRecord(isc.addProperties({}, newEvent, otherFields));
        var oldEventRecord = this.getCleanEventRecord(isc.addProperties({}, event));

        var requestProps = {oldValues: oldEventRecord, componentId: this.ID, willHandleError: true };
        if (this.updateOperation) requestProps.operationId = this.updateOperation;

        var _this = this;
        ds.updateData(updatedRecord, function (dsResponse, data, dsRequest) {
            _this.processSaveResponse(dsResponse, data, dsRequest, oldEventRecord);
        }, requestProps);
        return;
    } else {
        var oldEvent = isc.addProperties({}, event);
        isc.addProperties(event, newEvent, otherFields);
        this.processSaveResponse({status:0}, [event], {operationType:"update"}, oldEvent);
    }
},

processSaveResponse : function (dsResponse, data, dsRequest, oldEvent) {
    var newEvent = isc.isAn.Array(data) ? data[0] : data,
        opType = dsRequest ? dsRequest.operationType : null,
        isUpdate = opType == "update",
        isAdd = opType == "add",
        fromDialog = this._fromEventDialog,
        fromEditor = this._fromEventEditor
    ;

    delete this._fromEventDialog;
    delete this._fromEventEditor;

    if (dsResponse && dsResponse.status < 0) {
        var errors = dsResponse ? dsResponse.errors : null;
        if (dsResponse.status == isc.RPCResponse.STATUS_VALIDATION_ERROR) {
            // show any validation errors inline in the appropriate UI and don't fire central
            // error handling
            if (fromDialog) {
                if (errors) this.eventDialog.items[0].setErrors(errors, true);
                this.displayEventDialog(true);
                return;
            } else if (fromEditor) {
                this.eventEditorLayout.show();
                if (errors) this.eventEditor.setErrors(errors, true);
                return;
            }
        }
        if (isUpdate && oldEvent) {
            // if the save was an update, re-add the old event back to the view's eventData array
            var view = this.getSelectedView();
            if (view) {
                view.addEvent(oldEvent);
                // Re-Focus in the canvas if the attempt to update came from a user edit
                if (this.canSelectEvents && (fromDialog || fromEditor)) {
                    var eventCanvas = view.getCurrentEventCanvas(oldEvent);
                    if (eventCanvas) eventCanvas.focus();
                }
            }
        }


        // fire central RPCManager/DataSource error-handlers
        isc.RPCManager._handleError(dsResponse, dsRequest);
        return;
    }

    if (!newEvent || isc.isA.String(newEvent)) {
        if (isAdd) {
            this.logWarn("Calendar Add operation did not return a record. " +
                "The operation succeeded but no CalendarViews will be refreshed.")
            return;
        } else newEvent = oldEvent;
    }

    var oldStart = isUpdate && oldEvent ? this.getEventStartDate(oldEvent) : null,
        oldEnd = isUpdate && oldEvent ? this.getEventEndDate(oldEvent) : null,
        oldLane = isUpdate && oldEvent ? oldEvent[this.laneNameField] : null,
        startDate = this.getEventStartDate(newEvent),
        endDate = this.getEventEndDate(newEvent),
        newLane = newEvent[this.laneNameField]
    ;

    // set the eventLength and a couple of duration-related attributes
    newEvent.eventLength = (endDate.getTime() - startDate.getTime());
    if (newEvent[this.durationField] != null) {
        //event[this.endDateField] = eDate;
        newEvent.isDuration = true;
        newEvent.isZeroDuration = newEvent[this.durationField] == 0;
    }

    var view = this.getSelectedView();

    //view.initEventCache(newEvent);

    if (this._shouldRefreshDay(startDate, endDate) ||
            (isUpdate && this._shouldRefreshDay(oldStart, oldEnd)))
    {
        if (!this.dayViewSelected()) this.dayView._needsRefresh = true;
        else {
            if (isUpdate) {
                var view = this.dayView;
                if (this.showDayLanes) {
                    view.retagLaneEvents(oldLane);
                    if (newLane != oldLane) view.retagLaneEvents(newLane)
                } else {
                    view.refreshEvents("processSaveResponse");
                }
            } else if (isAdd) {
                view.refreshEvents("processSaveResponse");
            }
        }
    }
    if (this._shouldRefreshWeek(startDate, endDate)) {
        if (!this.weekViewSelected()) this.weekView._needsRefresh = true;
        else {
            var view = this.weekView;
            if (isUpdate) {
                // with showColumnLayouts, only need to update stuff if update was via one of
                // the dialogs (ie, not following a drag-drop)
                if (!this.showColumnLayouts || (fromDialog || fromEditor)) {
                    var lOldDate = isc.DateUtil.getLogicalDateOnly(oldStart);
                    var lNewDate = isc.DateUtil.getLogicalDateOnly(startDate);

                    if (!this.showColumnLayouts) {
                        view.retagDayEvents(lOldDate);

                    } else {
                        // setEvent() to update title/body text and sortValue
                        var eventCanvas = view.getCurrentEventCanvas(oldEvent);
                        if (eventCanvas) eventCanvas.setEvent(newEvent);
                        // if need be, resort the columnLayout in this mode
                        var oldField = view.getField(view.getColFromDate(lOldDate));
                        if (oldField.eventLayoutID) {
                            view.columnLayoutMap[oldField.eventLayoutID].resort();
                        }
                    }
                    if (isc.DateUtil.compareLogicalDates(lOldDate, lNewDate) != 0) {
                        view.retagDayEvents(lNewDate);
                        if (this.showColumnLayouts) {
                            // if need be, resort the columnLayout in this mode
                            var newField = view.getField(view.getColFromDate(lOldDate));
                            if (newField.eventLayoutID) {
                                view.columnLayoutMap[newField.eventLayoutID].resort();
                            }
                        }
                    }
                }
            } else if (isAdd) {
                view.addEvent(newEvent, true);
                view.retagDayEvents(startDate);
            }
        }
    }
    if (this._shouldRefreshMonth(startDate, endDate)) {
        if (!this.monthViewSelected()) this.monthView._needsRefresh = true;
        else this.monthView.refreshEvents("processSaveResponse");
    }
    if (this._shouldRefreshTimeline(startDate, endDate)) {
        if (!this.timelineViewSelected()) this.timelineView._needsRefresh = true;
        else {
            var view = this.timelineView;
            if (isUpdate) {
                if (oldLane && oldLane != newLane) view.retagLaneEvents(oldLane);
                view.retagLaneEvents(newLane);
            } else if (isAdd) {
                //view.addEvent(newEvent, true);
                view.refreshEvents("processSaveResponse");
            }
        }
    }

    // the updating has all been done - remove the flag preventing dataChanged from firing, in
    // case a dev hooks eventChanged or eventAdded and does something like adding more events
    if (this._ignoreDataChanged) delete this._ignoreDataChanged;

    // Re-Focus in the canvas if the change came from a user edit
    if (this.canSelectEvents && (fromDialog || fromEditor)) {
        var eventCanvas = view.getCurrentEventCanvas(oldEvent || newEvent);
        if (eventCanvas) eventCanvas.focus();
    }

    // fire eventChanged or eventAdded as appropriate
    if (isUpdate && this.eventChanged) this.eventChanged(newEvent);
    if (isAdd && this.eventAdded) this.eventAdded(newEvent);


},

//> @method calendar.refreshEvent()
// Refreshes the passed event in the current view.
//
// @param event       (CalendarEvent) The event object to refresh in the current view
// @visibility calendar
//<
refreshEvent : function (event) {
    var view = this.getSelectedView();
    var win = view.getCurrentEventCanvas(event);
    if (win) {
        win.setEvent(event)
        win.markForRedraw();
    }
},

//> @method calendar.setEventStyle()
// Update the styleName for the passed event.  Refreshes the event's canvas in the current view.
//
// @param event       (CalendarEvent) The event object to refresh in the current view
// @param styleName   (CSSStyleName) The new CSS style to apply to the canvases showing this event
// @visibility calendar
//<
setEventStyle : function (event, styleName) {
    event.eventWindowStyle = styleName;
    var win = this.getSelectedView().getCurrentEventCanvas(event);
    if (win) {
        win.setEventStyle(styleName);
        win.markForRedraw();
    }
},

eventsAreSame : function (first, second) {
    if (!first || !second) return false;
    if (this.dataSource) {
        var ds = isc.DataSource.get(this.dataSource),
            pks = this.getEventPKs(),
            //pks = ds.getPrimaryKeyFieldNames(),
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


//> @attr calendar.twentyFourHourTime  (Boolean : null : [IR])
// Dictates whether times throughout the widget are formatted and edited as 24-hour values.  If
// unset, defaults to the +link{Time.use24HourTime, global 24-hour setting}.  If set, and no
// +link{Calendar.timeFormatter, local formatter} is installed, causes the
// Calendar to choose an appropriate builtin formatter.
//
// @visibility external
//<

//> @attr calendar.dateFormatter (DateDisplayFormat : null : [IRW])
// Date formatter for displaying events.
// Default is to use the system-wide default short date format, configured via
// +link{DateUtil.setShortDisplayFormat()}.  Specify any valid +link{type:DateDisplayFormat}.
// @visibility external
//<
dateFormatter:null,

//> @attr calendar.timeFormatter (TimeDisplayFormat : null : [IRW])
// Display format to use for the time portion of events' date information.  By default, times
// are displayed in the global format, including the influence of the global
// +link{Time.use24HourTime, 24-hour} option, which is true by default.
// P>
// Note that this display setting does not affect the way in which time values are edited in the
// +link{calendar.eventEditor, eventEditor} - see +link{calendar.twentyFourHourTime} for more
// information.
// @visibility external
//<
timeFormatter:null,

//> @attr calendar.alwaysShowEventHovers (Boolean : true : IRW)
// By default, EventCanvases show their content in hovers.  If you set this attribute to false,
// hovers will only be shown if the content of the event-canvas is visually clipped.
// <p>
// Note - if you have custom hover-content/handling, you should leave this property set to
// true.
//
// @visibility external
//<
alwaysShowEventHovers: true,

//> @method calendar.getEventHoverHTML()
// Gets the hover HTML for an event being hovered over. Override here to return custom
// HTML based upon the parameter event object.
//
// @param event (CalendarEvent) The event being hovered
// @param eventCanvas (EventCanvas) the event canvas being hovered over
// @param view (CalendarView) the CalendarView in which the eventCanvas lives
// @param [defaultValue] (String) the default HTML to show when hovering over the passed event
// @return (HTMLString) the HTML to show in the hover
//
// @visibility calendar
//<
_getEventHoverHTML : function (event, eventCanvas, view) {
     // format date & times
    var cal = this,
        startDate = cal.getEventStartDate(event),
        sDate = startDate.toShortDate(this.dateFormatter, true),
        sTime = isc.Time.toShortTime(startDate, this.timeFormatter, false),
        endDate = this.getEventEndDate(event),
        eDate = endDate.toShortDate(this.dateFormatter, false),
        eTime = isc.Time.toShortTime(endDate, this.timeFormatter, false),
        name = event[cal.nameField],
        description = event[cal.descriptionField],
        sb = isc.StringBuffer.create()
    ;

    if (view.isTimelineView()) {
        if (isc.DateUtil.compareLogicalDates(startDate, endDate) != 0) {
            // Timeline dates can span days
            sb.append(sDate, "&nbsp;", sTime, "&nbsp;-&nbsp;", eDate, "&nbsp;", eTime);
        } else {
            sb.append(sDate, "&nbsp;", sTime, "&nbsp;-&nbsp;", eTime);
        }
    } else {
        sb.append(sDate, "&nbsp;", sTime, "&nbsp;-&nbsp;", eTime);
    }



    // support EventCanvas.escapeHTML for just the name and description fields in default HTML
    // - ie, only escape dev-provided HTML and not, for example, bolded datetimes in builtin
    // HTML.  If the dev modifies the default HTML, we will escape the entire thing below
    if (name) {
        // support eventCanvas.escapeHTML for event-names
        if (eventCanvas && eventCanvas.escapeHTML) name = name.asHTML();
        sb.append("<br><br>", name);
    }
    if (description) {
        // support eventCanvas.escapeHTML for event-descriptions
        if (eventCanvas && eventCanvas.escapeHTML) description = description.asHTML();
        sb.append("<br>", description);
    }

    var defaultValue = sb.release(false);
    var result = this.getEventHoverHTML(event, eventCanvas, view, defaultValue);

    // support eventCanvas.escapeHTML for dev-provided hoverHTML
    if (result != defaultValue && eventCanvas && eventCanvas.escapeHTML) result = result.asHTML();
    return result;
},
getEventHoverHTML : function (event, eventCanvas, view, defaultValue) {
    return defaultValue;
},

//> @method calendar.getZoneHoverHTML()
// Gets the hover HTML for a +link{calendar.zones, zone} being hovered over. Override here to
// return custom HTML based upon the parameter zone object.
//
// @param zone (CalendarEvent) The zone being hovered over
// @param zoneCanvas (ZoneCanvas) the zone canvas being hovered over
// @param view (CalendarView) the CalendarView in which the zoneCanvas is displayed
// @param defaultValue (String) the default HTML to show when hovering over the passed Zone
// @return (HTMLString) the HTML to show in the hover
//
// @visibility external
//<
_getZoneHoverHTML : function (zone, zoneCanvas, view) {
    var defaultValue = this._getEventHoverHTML(zone, zoneCanvas, view);
    return this.getZoneHoverHTML(zone, zoneCanvas, view, defaultValue);
},
getZoneHoverHTML : function (zone, zoneCanvas, view, defaultValue) {
    return defaultValue;
},

//> @method calendar.indicatorClick()
// Called whenever an +link{class:IndicatorCanvas} is clicked in the
// +link{calendar.timelineView, timelineView}.  There is no default implementation.
//
// @param indicatorEvent (CalendarEvent) indicator that was clicked on
// @param viewName (ViewName) view where the event's canvas was clicked
// @return (Boolean) false to cancel the default action
//
// @visibility calendar
//<
indicatorClick : function (indicatorEvent, viewName) {
    return true;
},

//> @attr calendar.showIndicatorsInFront (boolean : true : IR)
// In +link{calendar.indicators, indicator lines} are showing, this attribute affects where in
// the z-order their canvases will be rendered:  either in front of, or behind normal calendar
// events.
// @visibility external
//<
showIndicatorsInFront: true,

//> @method calendar.getIndicatorHoverHTML()
// Gets the hover HTML for an +link{calendar.indicators, indicator} being hovered over.
// Override here to return custom HTML based upon the parameter indicator object.
//
// @param indicator (CalendarEvent) The indicator being hovered over
// @param indicatorCanvas (IndicatorCanvas) the indicator canvas being hovered over
// @param view (CalendarView) the CalendarView in which the indicatorCanvas is displayed
// @param defaultValue (String) the default HTML to show when hovering over the passed Indicator
// @return (HTMLString) the HTML to show in the hover
//
// @visibility external
//<
_getIndicatorHoverHTML : function (indicator, indicatorCanvas, view) {
    var defaultValue = this._getEventHoverHTML(indicator, indicatorCanvas, view);
    return this.getIndicatorHoverHTML(indicator, indicatorCanvas, view, defaultValue);
},
getIndicatorHoverHTML : function (indicator, indicatorCanvas, view, defaultValue) {
    return defaultValue;
},

//> @attr calendar.showCellHovers (Boolean : false : IR)
// When +link{calendar.showViewHovers, showViewHovers} is true, dictates whether to display
// hover prompts when the mouse rolls over the normal cells in the body of CalendarViews.
// <P>
// The content of the hover is determined by a call to
// +link{calendar.getCellHoverHTML}, which can be overridden to return custom results; by
// default, it returns the cell's date as a string.
//
// @visibility external
//<
showCellHovers: false,

//> @method calendar.getCellHoverHTML()
// Returns the hover HTML for the cell at the passed co-ordinates in the passed view.  By
// default, the hover text is  the snap date closest to the mouse, if the cell being hovered is
// a normal date cell - otherwise, it is the title of the +link{calendar.laneFields, laneField}
// being hovered over.
// <P>
// Override here to return custom HTML for the passed cell.
//
// @param view (CalendarView) the CalendarView the mouse is hovered over
// @param record (Record) The record containing the cell being hovered
// @param rowNum (Integer) The rowNum of the cell being hovered
// @param colNum (Integer) the colNum of the cell being hovered
// @param date (Date) the snap-date at the mouse, which may be different from the result of a
//                    call to +link{calendar.getCellDate, getCellDate}
// @param defaultValue (String) the default hover text for the passed values
// @return (HTMLString) the HTML to show in the hover
//
// @visibility external
//<
//dateCellHoverStyle: "testStyle2",
_getCellHoverHTML : function (view, record, rowNum, colNum) {
    var field = view.getField(colNum),
        date = null,
        defaultValue = null
    ;
    if (!field) return;
    if (field.isLaneField) {
        if (!view.shouldShowLaneFieldHovers()) return;
        defaultValue = record && record[field[view.fieldIdProperty]];
        // support field.escapeHTML for cell hovers
        if (defaultValue && field.escapeHTML) defaultValue = defaultValue.asHTML();
        if (field.hoverHTML) {
            // support field.escapeHTML for custom hoverHTML()
            var defaultValue2 = field.hoverHTML(record, defaultValue, rowNum, colNum, view);
            if (defaultValue2 != defaultValue && field.escapeHTML) {
                defaultValue2 = defaultValue2.asHTML();
            }
            defaultValue = defaultValue2;
        }
    } else {
        if (!view.shouldShowCellHovers()) return;
        var date = view.getDateFromPoint();
        if (date) {
            // don't include the time in monthView
            defaultValue = "<nobr>" + this.__getLocalDatetimeString(date, view.isMonthView()) + "</nobr>";
        }
    }

    // For MonthView and non-Date cells in other views, respect showClippedValuesOnHover -
    // if it's set, only show hovers for clipped values - date-cells never have values, so
    // they're never clipped
    if (!field.date || view.isMonthView()){
        if (view.showClippedValuesOnHover && !view.cellValueIsClipped(rowNum, colNum)) return;
    }

    var result = this.getCellHoverHTML(view, record, rowNum, colNum, date, defaultValue)

    // support field.escapeHTML for dev-provided hoverHTML
    if (result != defaultValue && field && field.escapeHTML) result = result.asHTML();
    return result;
},
getCellHoverHTML : function (view, record, rowNum, colNum, date, defaultValue) {
    return defaultValue;
},



//> @attr calendar.showHeaderHovers (Boolean : false : IR)
// When +link{calendar.showViewHovers, showViewHovers} is true, dictates whether to display
// hover prompts when the mouse rolls over the +link{calendar.headerLevels, header levels} in
// a +link{class:CalendarView}.
// <P>
// The content of the hover is determined by a call to
// +link{calendar.getHeaderHoverHTML}, which can be overridden to return custom results;
//
// @visibility external
//<
showHeaderHovers: false,

//> @method calendar.getHeaderHoverHTML()
// Returns the hover HTML to show in a hover when the mouse moves over the header area.
//
// @param view (CalendarView) the CalendarView the mouse is hovered over
// @param headerLevel (HeaderLevel) the header level hovered over
// @param startDate (Date) the start date of the span being hovered over
// @param endDate (Date) the end date of the span being hovered over
// @param defaultValue (String) the default text for the passed header level and date range
// @return (HTMLString) the HTML to show in the hover
//
// @visibility external
//<
_getHeaderHoverHTML : function (view, headerLevel, button, startDate, endDate) {
    // internal method - builds a defaultValue and then fires override points:
    // - headerLevel.hoverHTML() if it exists
    // - getHeaderHoverHTML()
    if (!view.shouldShowHeaderHovers()) return;
    var defaultValue = button && (button.title || button.name);
    if (defaultValue && button && button.escapeHTML) defaultValue = defaultValue.asHTML();
    if (headerLevel.hoverHTML) {
        var defaultValue2 = headerLevel.hoverHTML(view, startDate, endDate, defaultValue);
        if (defaultValue2 != defaultValue && button && button.escapeHTML) {
            defaultValue2 = defaultValue2.asHTML();
        }
        defaultValue = defaultValue2;
    }

    var result = this.getHeaderHoverHTML(view, headerLevel, startDate, endDate, defaultValue);

    // support field.escapeHTML for dev-provided hoverHTML
    if (result != defaultValue && button && button.escapeHTML) result = result.asHTML();
    return result;
},
getHeaderHoverHTML : function (view, headerLevel, startDate, endDate, defaultValue) {
    return defaultValue;
},

//> @attr calendar.showViewHovers (Boolean : true : IRW)
// When set to true, the default value, causes the Calendar to show customizable hovers when
// the mouse moves over various areas of a CalendarView.
// <P>
// See +link{calendar.showEventHovers, showEventHovers},
// +link{calendar.showZoneHovers, showZoneHovers},
// +link{calendar.showHeaderHovers, showHeaderHovers},
// +link{calendar.showCellHovers, showCellHovers},
// +link{calendar.showLaneFieldHovers, showLaneFieldHovers},
// +link{calendar.showDragHovers, showDragHovers} for further configuration options.
//
// @setter calendar.setShowViewHovers()
// @visibility external
//<
showViewHovers: true,

//> @method calendar.setShowViewHovers()
// Switches the various levels of +link{calendar.showViewHovers, hovers} on or off at runtime.
//
// @param showViewHovers (boolean) whether to allow CalendarViews to show hovers
// @visibility external
//<
setShowViewHovers : function (showViewHovers, view) {
    this.showViewHovers = showViewHovers;
    if (view) {
        view.setShowHover(showViewHovers);
    } else {
        if (this.dayView) this.dayView.setShowHover(showViewHovers);
        if (this.weekView) this.weekView.setShowHover(showViewHovers);
        if (this.monthView) this.monthView.setShowHover(showViewHovers);
        if (this.timelineView) this.timelineView.setShowHover(showViewHovers);
    }
},

//> @attr calendar.showEventHovers (Boolean : true : IRW)
// When +link{calendar.showViewHovers, showViewHovers} is true, dictates whether to display
// hover prompts when the mouse moves over an +link{class:EventCanvas, event canvas} in a
// calendarView.
// <P>
// The content of the hover is determined by a call to
// +link{calendar.getCellHoverHTML}, which can be overridden to return custom results.
//
// @visibility external
//<
showEventHovers: true,

//> @attr calendar.showZoneHovers (Boolean : true : IRW)
// When +link{calendar.showViewHovers, showViewHovers} is true, dictates whether to display
// hover prompts when the mouse moves over a +link{calendar.zones, zone} in a calendarView.
// <P>
// When +link{calendar.showCellHovers, showCellHovers} is true, this attribute is ignored and
// zone hovers are not displayed.
// <P>
// The content of the hover is determined by a call to
// +link{calendar.getZoneHoverHTML}, which can be overridden to return custom results.
//
// @visibility external
//<
showZoneHovers: true,

//> @attr calendar.showLaneFieldHovers (Boolean : false : IRW)
// When +link{calendar.showViewHovers, showViewHovers} is true, dictates whether to display
// hover prompts when the mouse moves over the cells in a
// +link{calendar.laneFields, laneField}.
// <P>
// The content of the hover is determined by a call to
// +link{calendar.getCellHoverHTML}, which can be overridden to return custom results.  Note
// that getCellHoverHTML() is also called when the mouse moves over cells if
// +link{calendar.showCellHovers, showCellHovers} is true - when called for a laneField, no
// "date" parameter is passed to that method.
//
// @visibility external
//<
showLaneFieldHovers: false,

//> @attr calendar.showDragHovers (Boolean : false : IRW)
// When +link{calendar.showViewHovers, showViewHovers} is true, dictates whether to display
// hover prompts when an event is being dragged with the mouse.
// <P>
// The content of the hover is determined by a call to
// +link{calendar.getDragHoverHTML}, which can be overridden to return custom results; by
// default, it returns the date range of the drag canvas as a string.
//
// @visibility external
//<
showDragHovers: false,

//> @method calendar.getDragHoverHTML()
// Returns the HTML to show in a hover when an existing event is dragged, or when a new event
// is being created by dragging with the mouse.
//
// @param view (CalendarView) the CalendarView the mouse is hovered over
// @param event (CalendarEvent) the CalendarEvent attached to the EventCanvas being dragged
// @param defaultValue (String) the default text for the passed values
// @return (HTMLString) the HTML to show in the hover
//
// @visibility external
//<
_getDragHoverHTML : function (view, event) {
    event = event || {};
    var style = event.hoverStyleName || this.hoverStyleName || "";
    var startDate = event[this.startDateField],
        endDate = event[this.endDateField],
        defaultValue =
            "<div style='" + style + "'><nobr>" +
                this.__getLocalDatetimeString(startDate) + "</nobr></div>" +
            "<div style='" + style + "'><nobr>" +
                this.__getLocalDatetimeString(endDate) + "</nobr></div>"
    ;
    return this.getDragHoverHTML(view, event, defaultValue);
},

__getLocalDatetimeString : function (date, dateOnly) {
    var lDate = isc.DateUtil.getLogicalDateOnly(date);
    var lTime = isc.DateUtil.getLogicalTimeOnly(date);
    var result = lDate.toShortDate(this.dateFormatter);
    if (!dateOnly) {
        // monthView doesn't show times on rollover
        result += " " + isc.Time.toShortTime(lTime, this.timeFormatter, true);
    }
    return result;
},

getDragHoverHTML : function (view, event, defaultValue) {
    return defaultValue;
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
    if (!this.dayView || !this.dayView.body) return false;
    var lViewDate = isc.DateUtil.getLogicalDateOnly(this.dayView.startDate);
    var lStartDate = isc.DateUtil.getLogicalDateOnly(startDate);
    var lEndDate = isc.DateUtil.getLogicalDateOnly(endDate);
    var validStart = isc.DateUtil.compareLogicalDates(lStartDate, lViewDate) == 0;
    var validEnd = isc.DateUtil.compareLogicalDates(lEndDate, lViewDate) == 0;
    // if start is less than rangeEnd and end is greater than rangeStart, its in range
    return (validStart || validEnd);
},

_shouldRefreshWeek : function (startDate, endDate) {
    if (!this.weekView || !this.weekView.body) return false;
    var validStart = startDate.getTime() < this.weekView.endDate.getTime(),
        validEnd = endDate.getTime() > this.weekView.startDate.getTime()
    ;
    // if start is less than rangeEnd and end is greater than rangeStart, its in range
    return (validStart || validEnd);
},

_shouldRefreshMonth : function (startDate, endDate) {
    if (!this.monthView || !this.monthView.body) return false;
    // provide a nice broad range to detect whether a month refresh should be done
    var startMillis = new Date(this.year, this.month, -7, 0, 0, 0).getTime(),
        endMillis = new Date(this.year, this.month, 37, 23, 59, 59).getTime();
    return (startDate.getTime() < endMillis && endDate.getTime() > startMillis);
},

_shouldRefreshTimeline : function (startDate, endDate) {
    if (!this.timelineView || !this.timelineView.body) return false;
    var validStart = startDate.getTime() < this.timelineView.endDate.getTime(),
        validEnd = endDate.getTime() > this.timelineView.startDate.getTime()
    ;
    // if start is less than rangeEnd and end is greater than rangeStart, its in range
    return (validStart && validEnd);
},






//> @attr calendar.eventCanvasConstructor (String : "EventCanvas" : IR)
// The constructor with which to create the canvases used to represent events.  The default
// value is "EventCanvas".  You can customize these canvases by creating a subclass of
// +link{class:EventCanvas} and setting this attribute to the name of that class.
// @getter Calendar.getEventCanvasConstructor()
// @visibility internal
//<
eventCanvasConstructor: "EventCanvas",
eventCanvasDefaults: {
    autoDraw: false,
    visibility: "hidden",
    _redrawWithParent:false
},

//> @method calendar.getEventCanvasConstructor()
// Returns the +link{Class, constructor} to use when creating a canvas to render the passed
// +link{CalendarEvent, event}, in the passed +link{CalendarView, view}.  By default, returns
// the value on the +link{calendarView.eventCanvasConstructor, view}, if there is one, or the
// value on the +link{Calendar.eventCanvasConstructor, calendar} otherwise.
// @param event (CalendarEvent) the event to get constructor for
// @param view (CalendarView) the CalendarView containing the canvas in question
// @return (Class) the constructor class or class name
// @visibility internal
//<
getEventCanvasConstructor : function (event, view) {
    view = view || this.getSelectedView();
    // each view can specify a canvas constructor
    return view.getEventCanvasConstructor(event) || this.eventCanvasConstructor;
},


//> @attr calendar.showEventCanvasComponents (boolean : false : IR)
// Whether +link{calendar.eventCanvas, event-canvases} should show a custom widget as content,
// rather than the default +link{eventCanvas.showHeader, header} and
// +link{eventCanvas.showBody, body} HTML.
// @see calendar.createEventCanvasComponent()
// @see calendar.updateEventCanvasComponent()
// @visibility external
//<
showEventCanvasComponents: false,

//> @attr calendar.eventCanvasComponent (MultiAutoChild Canvas : null : IR)
// Multi-AutoChild component, created as a space-filling member in individual
// +link{class:EventCanvas, event-canvases}, when +link{calendar.showEventCanvasComponents} is
// true.
// <P>
// @see calendar.showEventCanvasComponents
// @see calendar.createEventCanvasComponent()
// @see calendar.updateEventCanvasComponent()
// @visibility external
//<
eventCanvasComponentConstructor: "DetailViewer",
eventCanvasComponentDefaults: {
    colWidths: [100, "*"],
    getHoverHTML : function () {
        return this.creator.getHoverHTML();
    },
    //FIXME
    handleClick : function () {
        return isc.EH.STOP_BUBBLING;
    }
},

//> @method calendar.showEventCanvasComponent()
// Should a component be applied to the passed +link{class:EventCanvas, canvas} in the
// +link{eventCanvas.calendarView, view} in which it appears?  Return false from this method to
// override the global value of
// +link{calendar.showEventCanvasComponents, showEventCanvasComponents} for this canvas.
// @param canvas (EventCanvas) should this eventCanvas get a component component?
// @return (boolean) boolean
// @see calendar.createEventCanvasComponent()
// @see calendar.updateEventCanvasComponent()
// @visibility external
//<
showEventCanvasComponent : function (canvas) {
    return canvas._shouldShowComponent();
},

eventScreenPlaceholderDefaults: {
    _constructor: isc.Label,
    autoDraw: false,
    border: "1px solid red",
    contents: "eventScreen<br>Placeholder",
    align: "center"
},

//> @method calendar.createEventCanvasComponent()
// Called from +link{eventCanvas.setEvent} when +link{calendar.showEventCanvasComponents} is
// true and an eventCanvas needs a component.   The method is expected to create and return a
// single component to apply as a space-filling member of the passed +link{class:EventCanvas}
// in the +link{eventCanvas.calendarView, view} in which it appears.
// <P>
// By default, this method returns a +link{class:DetailViewer} showing values from the
// associated event, according to the fields in the Calendar's
// +link{calendar.dataSource, dataSource}, or the default event-fields if no dataSource is
// present.
// <P>
// However, the component can be any +link{class:Canvas, canvas-based} widget, including a Layout
// containing an arrangement of other widgets.  When applied as a member of an
// eventCanvas, the component fills its container and limits its own content to that size,
// showing scrollbars as appropriate.
// <P>
// See +link{calendar.updateEventCanvasComponent} for details on updating components that have
// already been created and applied.
// @param canvas (EventCanvas) the eventCanvas to get the component for
// @return (Canvas) any Canvas
// @see calendar.updateEventCanvasComponent()
// @see eventScreen
// @visibility external
//<
createEventCanvasComponent : function (canvas) {
    if (!this.showEventCanvasComponent(canvas)) return;
    var component;
    if (this.eventScreen) {
        component = isc.RPCManager.createScreen(this.eventScreen);
        if (!component) {
            component = this.createAutoChild("eventScreenPlaceholder");
        }
    } else {
        component = this.createAutoChild("eventCanvasComponent", {
            // fill the canvas and set overflow: auto
            width: "100%",
            // showColumnLayouts fits events to content, needs height:1px and overflow:visible
            height: this.showColumnLayouts ? 1 : "100%",
            overflow: this.showColumnLayouts ? "visible" : "auto",
            autoDraw: false,
            //
            dragTarget: canvas,
            // without this, the hover only moves as the mouse changes background view-cell
            hoverMoveWithMouse: canvas.hoverMoveWithMouse,
            getHoverHTML : function () {
                return this.dragTarget.getHoverHTML();
            },
            dataSource: this.getClientOnlyEventDS(canvas.calendarView)
        });
    }
    return component;
},

//> @method calendar.updateEventCanvasComponent()
// Called from +link{eventCanvas.setEvent} when +link{calendar.showEventCanvasComponents} is
// true and the eventCanvas already has a
// +link{calendar.createEventCanvasComponent, component}.  This method is expected to update
// the passed <code>component</code> as necessary, based on the
// +link{eventCanvas.event, current event}.
// <P>
// By default, if the passed <code>component</code> has methods called
// <code>setEvent</code> or <code>setData</code>, those methods are called automatically.
// @param canvas (EventCanvas) the eventCanvas to update the component for
// @param component (Canvas) the component to be updated the canvas in question
// @see calendar.createEventCanvasComponent()
// @visibility external
//<
updateEventCanvasComponent : function (canvas, component) {
    if (!this.showEventCanvasComponent(canvas)) return;
    if (component) {
        if (this.eventScreen) {
            var ds = this.getDataSource();
            if (ds) {
                var dataContext = {};
                dataContext[ds.ID] = canvas.event;
                component.setDataContext(dataContext);
            }
        } else {
            // call setData() and/or setEvent() on the component, if those methods exist
            if (component.setData) component.setData(canvas.event);
            if (component.setEvent) component.setEvent(canvas.event);
        }
    }
},

//> @attr calendar.eventScreen (String : null : IR)
// Screen to create (via +link{RPCManager.createScreen,createScreen()}) in lieu of calling
// +link{createEventCanvasComponent()}.
// <P>
// If this calendar has a +link{dataBoundComponent.dataSource,dataSource}, the created screen is
// provided with a +link{canvas.dataContext} that includes the event being shown.
// Be sure the event screen meets these +link{canvas.autoPopulateData,requirements}
// to utilize the <code>dataContext</code>.
// @visibility external
//<

// helper to get a clientOnly DS configured with the CalendarEvent fields, from the DS or
// just a default set - used for field-lists in default eventCanvasComponents
getClientOnlyEventDS : function (view, dsID) {
    var dsProps = { ID: dsID, clientOnly: true };
    var eccProps = this.eventCanvasComponentProperties,
        ds = (eccProps && eccProps.dataSource) || this.dataSource;
    if (ds) {
        // get the fields from the true event-DS
        dsProps.inheritsFrom = isc.DataSource.getDataSource(ds).getID();
    } else {
        // get a list of standard fields
        var fields = [
            { name: this.nameField, width: "*" },
            { name: this.descriptionField, width: "*" },
            { name: this.startDateField, type: "datetime", width: "*" },
            { name: this.endDateField, type: "datetime", width: "*" }
        ];
        if (view.usesLanes()) {
            fields.add({ name: this.laneNameField, width: "*" });
        }
        dsProps.fields = fields;
    }
    var newDS = isc.DataSource.create(dsProps);
    return newDS;
},


//> @method calendar.getEventCanvasStyle()
// Returns the +link{CSSStyleName, styleName} to use for the passed
// +link{CalendarEvent, event}, in the passed +link{CalendarView, view}.  By default, returns
// the style +link{calendar.eventStyleNameField, on the event}, if one is specified - otherwise,
// in +link{calendar.lanes, lane-based} views, it returns the style specified on the
// +link{Lane.eventStyleName, lane or sublane}, or the style specified on the
// +link{calendar.eventStyleName, calendar}.
// @param event (CalendarEvent) the event to get the CSS style for
// @param [view] (CalendarView) the CalendarView that contains the canvas being styled
// @return (CSSStyleName) the CSS style to apply to the passed event in the passed view
// @visibility external
//<
getEventCanvasStyle : function (event, view) {
    view = view || this.getSelectedView();
    var styleName = this._getEventStyleName(event) ||
            view.getEventCanvasStyle(event) ||
            this.eventWindowStyle || this.eventStyleName;
    return styleName;
},

//> @attr calendar.eventCanvasContextMenu (AutoChild Menu : null : R)
// Context menu displayed when an +link{class:EventCanvas, event canvas} is right-clicked, or
// when the rollover +link{calendar.eventCanvasContextButton, context button} is clicked.  The
// context button, and the menu itself, will only be displayed if
// +link{calendar.getEventCanvasMenuItems, getEventCanvasMenuItems} returns
// an array of appropriate items for the event.
// @visibility external
//<
eventCanvasContextMenuConstructor: "Menu",
//> @attr calendar.eventCanvasContextMenuStyle (CSSStyleName : "eventWindowContextMenu" : IR)
// The CSS style to apply to the +link{calendar.eventCanvasContextMenu, menu} displayed when
// the rollover +link{calendar.eventCanvasContextButton, context button} is clicked.
// @visibility internal
//<
eventCanvasContextMenuStyle: "eventWindowContextMenu",
eventCanvasContextMenuDefaults: {
},
// internalize this method name, and use the normal autoChild attribute (without the _ prefix) to
// allow a dev to prevent ANY context menu from being shown (including the built-in browser one)
_showEventCanvasContextMenu : function (canvas) {
    if (this.showEventCanvasContextMenu == false) return false;
    var menuItems = this.getEventCanvasMenuItems(canvas);
    if (menuItems && menuItems.length > 0) {
        if (!this.eventCanvasContextMenu) this.addAutoChild("eventCanvasContextMenu");
        this.eventCanvasContextMenu.setData(menuItems);
        canvas.contextMenu = this.eventCanvasContextMenu;
        canvas.showContextMenu();
        // return false to cancel the default context menu
        return false;
    }
    return true;
},
_eventCanvasContextClick : function (canvas) {
    return this._showEventCanvasContextMenu(canvas);
},

//> @method calendar.getEventCanvasMenuItems()
// If this method returns a value, it is expected to return an array of
// +link{class:MenuItem, items} applicable to the passed canvas and its event.  If an array
// with valid entries is returned, the rollover
// +link{calendar.eventCanvasContextButton, context button} is shown for the passed canvas.
// @param canvas (EventCanvas) the canvas to get menu items for
// @return (Array of MenuItem)
// @visibility external
//<
// don't expose the view param for now - needs to be a CalendarView
// - param view (CalendarView) the canvas to get menu items for
getEventCanvasMenuItems : function (canvas, view) {
    //view = view || this.getSelectedView();
/*
    var items = [
        { title: "Item 1", click:"isc.say('item 1');" },
        { title: "Item 2", isSeparator:true },
        { title: "Item 3", click:"isc.say('item 3');" }
    ];
    return items;
*/
    return;
},

//> @attr calendar.useEventCanvasRolloverControls (boolean : true : IR)
// By default, the +link{calendar.eventCanvasCloseButton, close buttons} and the
// +link{calendar.eventCanvasHResizer, horizontal} and
// +link{calendar.eventCanvasVResizer, vertical} resizer widgets
// for event canvases are shown only when the mouse is over a given event.  Set this attribute
// to false to have event canvases show these widgets permanently.
// @visibility external
//<
useEventCanvasRolloverControls: true,

hideEventCanvasControls : function (canvas, propertyName) {
    var obj = canvas[propertyName];
    if (!obj) return;

    var skipThese = [ "closeButton", "contextButton" ];
    for (var key in obj) {
        var comp = obj[key];
        // hide the control
        comp.hide();
        // remove it's ref to the eventCanvas that last used it
        delete comp.eventCanvas;
        // and re-add it as a child of the Calendar, which removes it from the eventCanvas
        if (!skipThese.contains(key)) this.addChild(comp, null, false);
    }
    delete canvas[propertyName];
},

_createEventCanvasControls : function (canvas, focusControl) {
    // called to create a set of rolloverControls for an eventCanvas - if a canvas is passed,
    // we know the orientation, and so which resizers to add - otherwise, add them all
    var controls = {};

    var layout = this.createAutoChild("eventCanvasButtonLayout");
    controls.contextButton = this.createAutoChild("eventCanvasContextButton");
    layout.addMember(controls.contextButton);
    if (this.canRemoveEvents != false) {
        // add the close button if event removal is not disallowed entirely
        controls.closeButton = this.createAutoChild("eventCanvasCloseButton");
        layout.addMember(controls.closeButton);
    }
    controls.buttonLayout = layout;
    if (this.canResizeEvents != false) {
        var view = this.getSelectedView();
        var vertical = (canvas && canvas.verticalResize) || view.verticalEvents;
        // if passed a view, add correct resizers for it's orientation - otherwise, add all 3
        if (!canvas || vertical) {
            // add the bottom resizer
            controls.endResizerB = this.getEventCanvasResizer(null, "B", focusControl);
        }
        if (!canvas || !vertical) {
            // add the left and right resizers
            controls.startResizerL = this.getEventCanvasResizer(null, "L", focusControl);
            controls.endResizerR = this.getEventCanvasResizer(null, "R", focusControl);
        }
    }
    return controls;
},

_getRolloverControls : function () {
    // returns the single set of rolloverControls applied to an eventCanvas on mouseOver
    if (!this._rolloverControls) {
        this._rolloverControls = this._createEventCanvasControls();
    }
    return this._rolloverControls;
},


// Selection - use a Selection object to maintain event-selection


//> @attr calendar.canSelectEvents (Boolean : null : IR)
// When set to true, makes individual +link{class:EventCanvas, event canvases} selectable.
// Events may be selected via a single click, as well as being
// included in the page's tab order. The current selected event is shown in a special
// style and pressing TAB or Shift-TAB will move the selection first among the events
// in the same lane, and then among those in the next or previous lane.
// <P>
// Pressing Enter while an editable event is selected
// will show either the event- +link{calendar.eventDialog, dialog} or
// +link{calendar.eventEditor, editor}.  Pressing Delete will remove the event.
// <P>
// Note that when this property is false, single clicking the event canvas for an
// editable event will bring up an editing interface for that event.
// When true this is no longer the case - a user can double click to bring up the editing
// interface instead (a single click will simply select the event canvas).
//
// @visibility external
//<
//canSelectEvents: null,

//> @attr calendar.selectionManager (Selection : null : [RA])
// The +link{group:selection,Selection object} associated with the <code>Calendar</code>.
// @group selection
// @visibility external
//<

//> @method calendar.selectSingleEvent()
// Selects a single event in the current view, showing it in a selected style and deselecting
// any other selected events.
// @param event (CalendarEvent) the event to select
// @return (Boolean) true if the selection was changed, false otherwise
// @visibility external
//<
selectSingleEvent : function (event, canvas) {
    return this.selectEvent(event, canvas, true)
},
//> @method calendar.selectEvent()
// Adds an event to the list of selected events in the current view, showing it in a selected
// style.
// @param event (CalendarEvent) the event to add to the selection
// @return (Boolean) true if the selection was changed, false otherwise
// @visibility external
//<
selectEvent : function (event, canvas, clearSelection) {
    var selection = this.getSelection();
    if (clearSelection) {
        this.deselectAllEvents();
    }
    var result = selection.select(event);
    if (result) {
        if (!canvas) {
            var view = this.getSelectedView();
            canvas = view.getCurrentEventCanvas(event);
        }
        this.selectEventCanvas(canvas);
        this.selectionChanged(event, selection.getSelection());
    }
    return result;
},
//> @method calendar.selectEvents()
// Adds one or more events to the list of selected events in the current view, showing them in
// a selected style.
// @param events (Array of CalendarEvent) the events to add to the selection
// @return (Boolean) true if the selection was changed, false otherwise
// @visibility external
//<
selectEvents : function (events) {
    var result = false;
    if (events && events.length > 0) {
        for (var i=0; i<events.length; i++) {
            var iResult = this.selectEvent(events[i]);
            if (iResult) result = true;
        }
    }
    return result;
},
selectEventCanvas : function (canvas) {
    canvas.bringToFront();
    canvas.isSelected = true;
    canvas.event.isSelected = true;
    canvas.setEventStyle(canvas._cacheValues.eventStyleName);
},

//> @method calendar.deselectEvent()
// Removes an event from the list of selected events in the current view, clearing its selected
// style.
// @param event (CalendarEvent) the event to deselect
// @return (Boolean) true if the selection was changed, false otherwise
// @visibility external
//<
deselectEvent : function (event, canvas) {
    var selection = this.getSelection();
    var result = selection.deselect(event);
    if (result) {
        if (!canvas) {
            var view = this.getSelectedView();
            canvas = view.getCurrentEventCanvas(event);
        }
        this.deselectEventCanvas(canvas);
        this.selectionChanged(event, selection.getSelection());
    }
    return result;
},
//> @method calendar.deselectEvents()
// Removes one or more events from the list of selected events in the current view, clearing
// their selected styles.
// @param events (Array of CalendarEvent) the events to deselect
// @return (Boolean) true if the selection was changed, false otherwise
// @visibility external
//<
deselectEvents : function (events) {
    var result = false;
    if (events && events.length > 0) {
        for (var i=0; i<events.length; i++) {
            var iResult = this.deselectEvent(events[i]);
            if (iResult) result = true;
        }
    }
    return result;
},
deselectAllEvents : function () {
    var result = false;
    var selected = this.getSelectedEvents();
    for (var i=0; i<selected.length; i++) {
        var iResult = this.deselectEvent(selected[i]);
        if (iResult) result = true;
    }
    return result;
},
deselectEventCanvas : function (canvas) {
    canvas.isSelected = false;
    canvas.event.isSelected = false;
    canvas.setEventStyle(canvas._cacheValues.eventStyleName);
},


selectionChanged : function (event, selectedList) {
},

selectionUpdated : function (event, selectedList) {
},

getSelection : function () {
    return this.selectionManager;
},
clearSelection : function () {
    this.deselectAllEvents();
},


//> @method calendar.getSelectedEvent()
// Returns the currently selected +link{class:CalendarEvent, event}, or the first one if more
// than one is selected.
// @return (CalendarEvent) the selected event
// @visibility external
//<
getSelectedEvent : function () {
    // return the first selected event
    var result = this.getSelectedEvents();
    if (result) result = result[0];
    return result;
},

//> @method calendar.getSelectedEvents()
// Returns the currently selected list of +link{class:CalendarEvent, events}.
// @return (Array of CalendarEvent) the list of selected events
// @visibility external
//<
getSelectedEvents : function () {
    // return all selected events
    var selected = this.getSelection();
    if (selected) selected = selected.getSelection();
    return selected;
},

// ----------------


_getFocusControls : function () {
    // returns the single set of rolloverControls applied to an eventCanvas on focus
    if (!this._focusControls) {
        this._focusControls = this._createEventCanvasControls(null, true);
    }
    return this._focusControls;
},

_focusEventCanvas : function (canvas) {
    // fired when an eventCanvas receives focus
    if (!canvas) return;

    if (canvas._staticControls) {
        // no canvas or calendar.useEventCanvasRolloverControls is false - every canvas has its
        // own set of components, so nothing to do here
        return;
    }
    if (canvas._rolloverControls) {
        // the canvas is already showing the rolloverControls (mouseOver) - hide them now
        this.hideEventCanvasControls(canvas, "_rolloverControls");
    }
    canvas.updateRolloverControls();

    // Ensure the target is visible in the viewport
    canvas.parentElement.scrollIntoView(
        canvas.getLeft(), canvas.getTop(), canvas.getWidth(), canvas.getHeight()
    );

    if (this._focusShouldSelect) {
        // if Tab triggered this focus, select the new focus item
        delete this._focusShouldSelect;
        this.selectSingleEvent(canvas.event, canvas);
    }
},

_blurEventCanvas : function (canvas) {
    // fired when an eventCanvas loses focus
    if (!canvas || canvas._staticControls) {
        // no canvas or calendar.useEventCanvasRolloverControls is false - every canvas has its
        // own set of components, so nothing to do here
        return;
    }
    // remove the focus controls
    this.hideEventCanvasControls(canvas, "_focusControls");
    canvas.updateRolloverControls();

    if (this._tabPressed) {
        // if blur() was triggered by the Tab key, set a flag that causes _focusEventCanvas()
        // to single-select the new focus-item
        delete this._tabPressed;
        this._focusShouldSelect = true;
    }
},

//> @attr calendar.eventCanvasGripper (MultiAutoChild Img : null : A)
// The "gripper" widget that snaps to the top of an event canvas and allows an
// event to be dragged with the mouse.
// @visibility external
//<
eventCanvasGripperConstructor:"Img",
eventCanvasGripperDefaults:{
    width: 11,
    height: 10,
    padding: 0,
    margin: 0,
    overflow: "visible",
    imageType: "center",
    autoDraw: false,
    visibility: "hidden",
    showDown:false,
    showOver: false,
    showRollOver:false,
    canDrag: true,
    layoutAlign:"center",
    cursor: isc.Canvas.MOVE
},
getEventCanvasGripper : function (props, canvas, view) {
    props = props || {};
    props.src = this.getEventCanvasGripperIcon(canvas, view);
    var gripper = this.createAutoChild("eventCanvasGripper", props);
    view.addChild(gripper, null, false);
    return gripper;
},

//> @attr calendar.eventCanvasGripperIcon (SCImgURL : "[SKIN]/Calendar/gripper.png" : A)
// Icon used as the default eveng gripper icon.
// @visibility external
//<
eventCanvasGripperIcon: "[SKIN]/Calendar/gripper.png",

//> @method calendar.getEventCanvasGripperIcon()
// Returns the +link{calendar.eventCanvasGripperIcon, source image} to use as the gripper for
// the passed event canvas.
// @param canvas (EventCanvas) the canvas that will show the gripper
// @return (SCImgURL) the URL for the image to load
// @visibility external
//<
getEventCanvasGripperIcon : function (canvas, view) {
    return canvas.gripperIcon || this.eventCanvasGripperIcon;
},

//> @attr calendar.eventCanvasLabel (MultiAutoChild Label : null : A)
// @visibility external
//<
eventCanvasLabelConstructor:"Label",
eventCanvasLabelDefaults:{
    height:1,
    width:1,
    autoSize: true,
    wrap: false,
    overflow: "visible",
    autoDraw: false,
    visibility: "hidden",
    padding: 2,
    minWidth: 40,
    maxWidth: 150,
    showOver: false,
    showDown: false,
    showRollOver: true,
    layoutAlign:"center",
    click : function () {
    },
    isEventCanvasLabel: true
},
getEventCanvasLabel : function (props, view) {
    var label = this.createAutoChild("eventCanvasLabel", props);
    view.addChild(label, null, false);
    return label;
},

//> @attr calendar.eventCanvasButtonLayout (AutoChild HLayout : null : A)
// HLayout that snaps to the top-right of an event canvas on rollover and contains the
// +link{calendar.eventCanvasCloseButton, close} and/or
// +link{calendar.eventCanvasContextButton, context} buttons.
// @visibility external
//<
eventCanvasButtonLayoutConstructor:"HLayout",
eventCanvasButtonLayoutDefaults:{
    width: 1, height: 1, overflow: "visible",
    autoDraw: false,
    snapTo:"TR",
    membersMargin: 1,
    layoutTopMargin: 3,
    layoutRightMargin: 3,
    mouseOver: function () { return isc.EH.STOP_BUBBLING; }
},

//> @attr calendar.eventCanvasCloseButton (AutoChild ImgButton : null : A)
// The close button that snaps to the top-right of an event canvas on rollover and allows an
// event to be removed from a +link{class:CalendarView}.
// @visibility external
//<
eventCanvasCloseButtonConstructor:"ImgButton",
eventCanvasCloseButtonDefaults:{
    width:11,
    height:10,
    autoDraw: false,
    showDown:false,
    showRollOver:true,
    layoutAlign:"center",
    src:"[SKIN]/headerIcons/close.png",
    baseStyle: "eventCanvasCloseButton",
    click : function () {
        var canvas = this.eventCanvas;
        this.creator._eventCanvasCloseClick(canvas);
        return false;
    }
},
_eventCanvasCloseClick : function (canvas) {
    if (this.eventRemoveClick(canvas.event, canvas.calendarView.viewName) != false) {
        this.removeEvent(canvas.event, false);
    }
},

getEventCanvasCloseButton : function (canvas) {
    if (this.useEventCanvasRolloverControls) {
        if (!this.eventCanvasCloseButton) {
            this.eventCanvasCloseButton = this.addAutoChild("eventCanvasCloseButton");
        }
        return this.eventCanvasCloseButton;
    } else {
        return this.createAutoChild("eventCanvasCloseButton");
    }
},

//> @attr calendar.eventCanvasContextButton (AutoChild ImgButton : null : A)
// The context button that snaps to the top-right of an event canvas on rollover and shows a
// custom +link{calendar.getEventCanvasMenuItems, context menu} when clicked.
// @visibility external
//<
eventCanvasContextButtonConstructor:"ImgButton",
eventCanvasContextButtonDefaults:{
    width:11,
    height:10,
    autoDraw: false,
    showDown:false,
    showRollOver:true,
    layoutAlign:"left",
    src:"[SKIN]/headerIcons/arrow_down.png",
    click : function () {
        this.creator._showEventCanvasContextMenu(this.eventCanvas);
        return false;
    }
},
getEventCanvasContextButton : function (canvas) {
    if (this.useEventCanvasRolloverControls) {
        if (!this.eventCanvasContextButton) {
            this.eventCanvasContextButton = this.addAutoChild("eventCanvasContextButton");
        }
        return this.eventCanvasContextButton;
    } else {
        return this.createAutoChild("eventCanvasContextButton");
    }
},


// single-instance resizers, shown for a single eventCanvas on mouseOver

//> @attr calendar.eventCanvasVResizer (MultiAutoChild Img : null : A)
// The resizer image that snaps to the bottom of event canvases in +link{calendar.dayView, day}
// and +link{calendar.weekView, week} views, allowing them to be resized vertically by dragging
// with the mouse.
// @visibility external
//<
eventCanvasVResizerConstructor:"Img",
eventCanvasVResizerDefaults: {
    width:12, height:6, overflow:"hidden", src:"[SKIN]/Window/v_resizer.png",
    autoDraw: false,
    cursor: isc.Canvas.ROW_RESIZE,
    canDragResize: true
},
//> @attr calendar.eventCanvasHResizer (MultiAutoChild Img : null : A)
// The resizer image that snaps to the left and right edges of an editable event canvas in a
// +link{class:Timeline}, allowing it to be resized horizontally by dragging with the mouse.
// @visibility external
//<
eventCanvasHResizerConstructor:"Img",
eventCanvasHResizerDefaults: {
    width:6, height:12, overflow:"hidden", src:"[SKIN]/Window/h_resizer.png",
    autoDraw: false,
    cursor: isc.Canvas.COL_RESIZE,
    canDragResize: true
},
getEventCanvasResizer : function (canvas, snapTo, focusControl) {
    var widgetName = "eventCanvasResizer" + snapTo,
        widget = focusControl ? null : this[widgetName]
    ;
    if (!this.useEventCanvasRolloverControls || !widget) {
        var className = "eventCanvas" + (["T", "B"].contains(snapTo) ? "V" : "H") + "Resizer",
            props = {
                snapTo: snapTo,
                getEventEdge : function () {
                    return this.snapTo;
                }
            }
        ;
        widget = this.createAutoChild(className, props);
        if (!focusControl && this.useEventCanvasRolloverControls) this[widgetName] = widget;
    }
    return widget;
},

//> @attr calendar.showZones (Boolean : null : IRW)
// Set to true to render any defined +link{calendar.zones, zones} into
// +link{calendar.timelineView, timeline views}.
// @visibility external
//<
setShowZones : function (showZones) {
    this.showZones = showZones;
    var view = this.timelineView;
    if (view && view.isSelectedView()) view.refreshEvents("setShowZones");
    else if (view) view._needsRefresh = true;
},

//> @attr calendar.zones (Array of CalendarEvent : null : IRW)
// An array of CalendarEvent instances representing pre-defined periods of time to be
// highlighted in +link{calendar.timelineView, timeline views}.  Each zone renders out a
// +link{class:ZoneCanvas, zone canvas}, a special, non-interactive subclass of
// +link{class:EventCanvas}, which spans all lanes and draws behind any normal, interactive
// events in the zorder.
// <P>
// The default +link{calendar.zoneStyleName, style} for these components renders them
// semi-transparent and with a bottom-aligned title label.
// @visibility external
//<

//> @method calendar.setZones()
// Sets the +link{calendar.zones, zones} used to highlight areas of this calendar.
//
// @param zones (Array of CalendarEvent) array of zones to display
//
// @visibility external
//<
setZones : function (zones) {
    // bail if nothing passed
    if (!zones) { return; }
    // store zones but don't call through if not yet draw()n
    this.zones = zones;
    if (this.timelineView) { this.timelineView.drawZones(); }
},

//> @method calendar.addZone()
// Adds a new +link{calendar.zones, zone} to the calendar.
//
// @param zone (CalendarEvent) a new zone to add to the calendar
//
// @visibility external
//<
addZone : function (zone) {
    if (!zone) return;
    this.zones = this.zones || [];
    this.zones.add(zone);
    this.setZones(this.zones);
},

//> @method calendar.removeZone()
// Removes a +link{calendar.zones, zone} from the calendar.
// <P>
// Accepts either a +link{CalendarEvent, zone object} or a string that represents the
// +link{calendarEvent.name, name} of a zone.
//
// @param zone (CalendarEvent | String) either the actual CalendarEvent representing the zone,
//                 or the name of the zone to remove
//
// @visibility external
//<
removeZone : function (zone) {
    if (!zone || !this.zones) return;

    if (isc.isA.String(zone)) zone = this.zones.find(this.nameField, zone);
    if (zone) {
        this.zones.remove(zone);
        this.setZones(this.zones);
    }
},

//> @method calendar.zoneClick()
// Called whenever a +link{class:ZoneCanvas} is clicked in the
// +link{calendar.timelineView, timelineView}.  There is no default implementation.
//
// @param zoneEvent (CalendarEvent) zone that was clicked on
// @param viewName (ViewName) view where the event's canvas was clicked
// @return (Boolean) false to cancel the default action
//
// @visibility calendar
//<
zoneClick : function (zoneEvent, viewName) {
    return true;
},

//> @attr calendar.zoneStyleName (CSSStyleName : "zoneCanvas" : IRW)
// CSS style to apply to the +link{calendar.zoneCanvas, canvases} created for each
// specified +link{calendar.zones, zone}.
// @visibility external
//<
zoneStyleName: "zoneCanvas",

//> @attr calendar.zoneCanvas (MultiAutoChild ZoneCanvas : null : A)
// AutoChild component created for each +link{calendar.zones, zone} entry.
// @visibility external
//<
zoneCanvasConstructor: "ZoneCanvas",

getZoneCanvas : function (zone, view) {
    var props = { calendar: this, calendarView: view, event: zone, isZoneCanvas: true,
            styleName: this.getZoneCanvasStyle(zone, view) };
    var canvas = this.createAutoChild("zoneCanvas", props, this.zoneCanvasConstructor);
    if (this.customizeCanvas) this.customizeCanvas(canvas, view);
    return canvas;
},

_getEventStyleName : function (event) {
    // support the deprecated eventWindowStyle attribute
    return event[this.eventWindowStyleField] || event[this.eventStyleNameField];
},

//> @attr calendar.zoneTitleOrientation (VerticalAlignment : "bottom" : IR)
// The vertical alignment of the header-text in each +link{calendar.zones, zone}.
// @visibility external
//<
zoneTitleOrientation: "bottom",

//> @method calendar.getZoneCanvasStyle()
// Returns the +link{CSSStyleName, styleName} to use for the passed
// +link{calendar.zones, zone}, in the passed +link{CalendarView, view}.  By default,
// returns the style +link{calendar.eventStyleNameField, on the zone}, if one is specified,
// or the style specified on the +link{calendar.zoneStyleName, calendar} otherwise.
// @param zone (CalendarEvent) the zone to get the CSS style for
// @param [view] (CalendarView) the CalendarView that contains the canvas being styled
// @return (CSSStyleName)
// @visibility external
//<
getZoneCanvasStyle : function (zone, view) {
    view = view || this.getSelectedView();
    var style = this._getEventStyleName(zone) || (view && view.zoneStyleName) || this.zoneStyleName;
    return style;
},


//> @attr calendar.showIndicators (Boolean : null : IRW)
// Set to true to render any defined +link{calendar.indicators, indicators} into
// +link{calendar.timelineView, timeline views}.
// @visibility external
//<
setShowIndicators : function (showIndicators) {
    this.showIndicators = showIndicators;
    var view = this.timelineView;
    if (view && view.isSelectedView()) view.refreshEvents("setShowIndicators");
    else if (view) view._needsRefresh = true;
},

//> @attr calendar.indicators (Array of CalendarEvent : null : IRW)
// An array of CalendarEvent instances representing instants in time, to be
// highlighted in +link{calendar.timelineView, timeline views}.  Each indicator renders out as
// an +link{class:IndicatorCanvas, indicator canvas}, a special, non-interactive subclass of
// +link{class:EventCanvas}, which spans all lanes and draws behind any normal, interactive
// events in the zorder, but in front of any +link{calendar.zones, zones}.  The default
// +link{calendar.indicatorStyleName, style} for these components renders them as thin vertical
// lines that span all lanes and have a hover but no title.
// @visibility external
//<

//> @attr calendar.indicatorStyleName (CSSStyleName : "indicatorCanvas" : IRW)
// CSS style to apply to the +link{calendar.indicatorCanvas, canvases} created for each
// specified +link{calendar.indicators, indicator}.
// @visibility external
//<
indicatorStyleName: "indicatorCanvas",

//> @attr calendar.indicatorCanvas (MultiAutoChild IndicatorCanvas : null : A)
// AutoChild component created for each +link{calendar.indicators, indicator} entry.
// @visibility external
//<
indicatorCanvasConstructor: "IndicatorCanvas",

getIndicatorCanvas : function (indicator, view) {
    view = view || this.getSelectedView();
    var props = { calendar: this, calendarView: view, event: indicator, isIndicatorCanvas: true,
            styleName: this.getIndicatorCanvasStyle(indicator, view),
            dragTarget: view.eventDragCanvas
        },
        canvas = this.createAutoChild("indicatorCanvas", props, this.indicatorCanvasConstructor)
    ;
    if (this.customizeCanvas) this.customizeCanvas(canvas, view);
    return canvas;
},

//> @method calendar.getIndicatorCanvasStyle()
// Returns the +link{CSSStyleName, styleName} to use for the passed
// +link{calendar.indicators, indicator}, in the passed +link{CalendarView, view}.  By default,
// returns the style +link{calendar.eventStyleNameField, on the indicator}, if one is specified,
// or the style specified on the +link{calendar.indicatorStyleName, calendar} otherwise.
// @param indicator (CalendarEvent) the indicator to get the CSS style for
// @param [view] (CalendarView) the CalendarView that contains the canvas being styled
// @return (CSSStyleName)
// @visibility external
//<
getIndicatorCanvasStyle : function (indicator, view) {
    view = view || this.getSelectedView();
    return this._getEventStyleName(indicator) || (view && view.indicatorStyleName)
                || this.indicatorStyleName;
},

// ---
//> @method calendar.setIndicators()
// Sets the +link{calendar.indicators, indicators} used to highlight instants in time.
// @param indicators (Array of CalendarEvent) array of indicators to display
// @visibility external
//<
setIndicators : function (indicators) {
    // bail if nothing passed
    if (!indicators) { return; }
    // store indicators but don't call through if not yet draw()n
    this.indicators = indicators;
    if (this.timelineView) { this.timelineView.drawIndicators(); }
},

//> @method calendar.addIndicator()
// Adds a new +link{calendar.indicators, indicator} to the calendar.
// @param indicator (CalendarEvent) a new indicator to add to the calendar
// @visibility external
//<
addIndicator : function (indicator) {
    if (!indicator) return;
    this.indicators = this.indicators || [];
    this.indicators.add(indicator);
    this.setIndicators(this.indicators);
},

//> @method calendar.removeIndicator()
// Removes a +link{calendar.indicators, indicator} from the calendar.
// <P>
// Accepts either a +link{CalendarEvent, indicator object} or a string that represents the
// +link{calendarEvent.name, name} of anindicator.
// @param indicator (CalendarEvent | String) either the actual CalendarEvent representing the
//                 indicator, or the name of the indicator to remove
// @visibility external
//<
removeIndicator : function (indicator) {
    if (!indicator || !this.indicators) return;
    if (isc.isA.String(indicator)) indicator = this.indicators.find(this.nameField, indicator);
    if (indicator) {
        this.indicators.remove(indicator);
        this.setIndicators(this.indicators);
    }
},

//> @attr calendar.eventCanvas (MultiAutoChild EventCanvas : null : A)
// To display events in +link{calendar.dayView, day}, +link{calendar.weekView, week} and
// +link{calendar.timelineView, timeline} views, the Calendar creates instances of
// +link{class:EventCanvas} for each event.  Use the +link{AutoChild} system to customize
// these canvases.
// @visibility external
//<




//> @attr calendar.showColumnLayouts (Boolean : false : IR)
// When true, causes +link{calendar.columnLayout, layouts} to be added to each column in
// vertical views.  In this mode, eventCanvases are stacked in these layouts, filling width
// and auto-sizing vertically to content, rather than being placed, sized and overlapped
// according to their times.
// <P>
// Because times are ignored in this mode, various behaviors are switched off automatically;
// for example, the +link{calendar.showLabelColumn, time-column} is hidden and event-canvases
// cannot be +link{calendar.canResizeEvents, resized} or rendered
// +link{calendar.renderEventsOnDemand, on-demand}.
//
// @group appearance
// @visibility calendar
//<
showColumnLayouts: false,

//> @attr calendar.columnLayout (MultiAutoChild VLayout : null : A)
// When +link{calendar.showColumnLayouts} is true, the layouts added to each column to stack
// events.
// @visibility calendar
//<
columnLayoutDefaults: {
    _constructor: "VLayout",
    height: "100%",
    overflow: "auto",
    // this style should be opaque by default, to avoid grid-lines showing through
    styleName: "calendarColumnLayout",
    border: null,
    canDrag: false,
    canDragReposition: false,
    canDragResize: false,
    mouseDown : function () {
        var cal = this.creator;
        if (cal.canCreateEvents) {
            var field = this.calendarView.getFieldByName(this.fieldName);
            var newEvent = cal.createEventObject({}, field.startDate, field.endDate);
            this.creator.showNewEventDialog(newEvent);
            return isc.EH.STOP_BUBBLING;
        }
    },
    canDragReorder: false,
    canAcceptDrop: true,
    canDropComponents: false,
    showDragPlaceHolder: false,
    dropTypes: ["EventCanvas"],
    drop : function () {
        var cal = this.calendar;
        var canvas = isc.EH.dragTarget;
        if (!canvas) return;

        var field = this.calendarView.getFieldByName(this.fieldName),
            sTime = isc.DateUtil.getLogicalTimeOnly(cal.getEventStartDate(canvas.event)),
            eTime = isc.DateUtil.getLogicalTimeOnly(cal.getEventEndDate(canvas.event)),
            // fix the dates to the current day
            // ToDo: although start and end date values on the field are not really used in
            // vertical views, look into why they're both wrong at this point (+ 1 day)
            newStart = isc.DateUtil.combineLogicalDateAndTime(field.date, sTime),
            newEnd = isc.DateUtil.combineLogicalDateAndTime(field.date, eTime)
        ;
        var newEvent = cal.createEventObject(canvas.event, newStart, newEnd)

        // update the sortValue on the canvas and save the event
        canvas.sortValue = newStart.duplicate();
        this.calendar.updateCalendarEvent(canvas.event, newEvent); //, null, true);

        // add the eventCanvas to this columnLayout
        this.addMember(canvas);

        // hide the dropLine
        this.hideDropLine();

        return isc.EH.STOP_BUBBLING;
    },
    getDropPosition : function () {
        // return the sorted insert-position for the eventCanvas being dragged
        var cal = this.calendar;
        var canvas = isc.EH.getDragTarget();
        var evt = canvas.event;
        var time = evt && isc.DateUtil.getLogicalTimeOnly(evt[cal.startDateField]);
        if (time) {
            for (var i=0; i<this.members.length; i++) {
                var iEvt = this.members[i].event;
                var eTime = iEvt && isc.DateUtil.getLogicalTimeOnly(iEvt[cal.startDateField]);
                if (isc.DateUtil.compareDates(time, eTime) > 0) {
                    break;
                }
            }
            return Math.max(i, 0);
        }
        return this.Super("getDropPosition", arguments);
    },
    addMember : function (member) {
        // store the event startDate as a sortValue, for use by resort() below
        if (member.event) member.sortValue = member.event[this.calendar.startDateField].duplicate();
        else member.sortValue = null;
        return this.Super("addMember", arguments);
    },
    resort : function () {
        var cal = this.calendar;
        // resort the members and reflow
        this.members.setSort([{ property: "sortValue", direction: "ascending" }]);
        this.reflowNow();
    },

    getPrintHTML : function (printProperties, callback) {
        if (callback) {
            this.delayCall("asyncGetPrintHTML", [printProperties, callback]);
            return null;
        } else {
            return this.asyncGetPrintHTML(printProperties, callback);
        }
    },

    asyncGetPrintHTML : function (printProperties, callback) {
        return this.getHandle().innerHTML;
    }
},

createColumnLayout : function (view, fieldName, props) {
    if (!this.showColumnLayouts) return;
    props = isc.addProperties({}, this.columnLayoutProperties, props);
    props.calendar = this;
    props.calendarView = view;
    props.fieldName = fieldName;
    return this.createAutoChild("columnLayout", props);
},

_getEventCanvas : function (event, view) {
    var props = isc.addProperties({
            calendar: this,
            calendarView: view,
            overflow: this.showColumnLayouts ? "visible" :
                (this.showEventCanvasComponents ? "auto" : "hidden")
        }, this.eventCanvasDefaults, this.eventCanvasProperties
    );

    // create canvas as an autoChild so it can be customized.
    var canvasClass = this.getEventCanvasConstructor(event, view),
        canvas = this.createAutoChild("eventCanvas", props, canvasClass);

    // add the canvas to the canvasMap and links between it and the event
    view._allCanvasMap[canvas.ID] = canvas;
    view._usedCanvasMap[canvas.ID] = event.__key;
    view._usedEventMap[event.__key] = canvas.ID;

    //canvas.setEvent(event, styleName);

    //if (this.customizeCanvas) this.customizeCanvas(canvas, view);

    return canvas;
},

_getEventsInRange : function (start, end, view, visibleLanesOnly) {

        var results = [],
            wends = this.getWeekendDays(),
            dataLength = this.data.getLength(),
            //laneNames = (this.lanes || []).getProperty("name")
            laneNames = [],
            startMillis = start.getTime(),
            endMillis = end.getTime()
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
            var curr = this.data.get(i),
                eventStart = this.getEventStartDate(curr)
            ;

            if (visibleLanesOnly && !laneNames.contains(curr[this.laneNameField])) continue;

            if (!curr || !eventStart) return [];
            // add the event if we're showing weekends or the date is not a weekend
            // The event won't get added only when !this.showWeekends and it is a weekend
            // subtle change: use only startDate instead of startDate and endDate to determine if
            // parameter range is in range so that events with end date on the next day are included.
            if (eventStart.getTime() >= start.getTime()
                && eventStart.getTime() <= end.getTime()
                && (this.showWeekends || !wends.contains(eventStart.getDay())))
            {
                if (view && (view.isWeekView() || view.isMonthView())) results.add(curr);
                else if (!this.showDayLanes || laneNames.contains(curr[this.laneNameField]))
                    results.add(curr);
            }
        }

        return results;
},

getDayEnd : function (startDate) {
    return isc.DateUtil.getEndOf(startDate, "d", false, this.firstDayOfWeek);
},

isTimeline : function () {
    var isTimeline = this.getCurrentViewName() == "timeline";
    return isTimeline;
},

eventsOverlapGridLines: true,

//> @method calendar.setChosenDate()
// Set the current date for which the calendar will display events.
//
// @param newDate (Date) the new date to set as the current date
//
// @visibility external
//<
setChosenDate : function (newDate, fromTimelineView) {
    var view = this.getSelectedView();

    // if passed a date on a weekend, and showWeekends is false, shift to the next weekday
    if (!this.showWeekends && isc.DateUtil.isWeekend(newDate)) {
        newDate = isc.DateUtil.getNextWeekday(newDate, this.getWeekendDays());
    }

    // store the logicalDate (which is in the displayTimezone), set year and month consistently
    this.chosenDate = isc.DateUtil.getLogicalDateOnly(newDate);
    this.year = this.chosenDate.getFullYear();
    this.month = this.chosenDate.getMonth();
    this.displayDate = this.chosenDate.getDate();

    // have the views prepare for the new chosen date
    if (this.dayView) this.dayView.setChosenDate(this.chosenDate);
    if (this.weekView) this.weekView.setChosenDate(this.chosenDate);
    if (this.monthView) this.monthView.setChosenDate(this.chosenDate);
    // for the timeline, pass the newDate, which may not be logical
    if (this.timelineView) this.timelineView.setChosenDate(newDate);

    if (this.monthButton) this.updateMonthButton();
    // reset date label
    this.setDateLabel();
    // call dateChanged
    this.dateChanged();

},

//> @method calendar.dateIsWorkday()
// Should the parameter date be considered a workday? By default this method tries to find the
// parameter date day in +link{calendar.workdays}, and returns true if found. Override this
// method to provide custom logic for determining workday, perhaps returning false on holidays.
// <P>
// Note that, when showing +link{calendar.showDayLanes, vertical lanes} in the
// +link{dayView, day view}, this method is also passed the name of the associated lane.
//
// @param date (Date) date to check for being a workday
// @param laneName (String) the name of the lane if +link{showDayLanes} is true, null otherwise
// @return (boolean) true if date is a workday, false otherwise
// @visibility Calendar
//<
dateIsWorkday : function (date, laneName) {
    if (!date || !this.workdays) return false;
    var lDate = isc.DateUtil.getLogicalDateOnly(date);
    return this.workdays.contains(lDate.getDay());
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

includeRangeCriteria: false,
shouldIncludeRangeCriteria : function (view) {

    view = view || this.getSelectedView();
    if (view && view.includeRangeCriteria != null) return view.includeRangeCriteria;
    return this.includeRangeCriteria;
},
_filter : function (type, criteria, callback, requestProperties, doneSaving) {
    // override _filter to remove any specified range-criteria, and then add the
    // range-criteria for the current view, as required
    var view = this.getSelectedView();
    if (this.shouldIncludeRangeCriteria(view)) {
        // forcibly limit the filter to dates that are accessible in the current view - if the
        // fetch-criteria already includes date-range entries, replace them with those
        // provided by the view
        if (criteria) {
            criteria = isc.DS.removeCriteriaForField(criteria, this.startDateField);
            criteria = isc.DS.removeCriteriaForField(criteria, this.endDateField);
        }
        var rangeCrit = this.getRangeCriteria(view);
        if (rangeCrit) {
            if (!criteria || isc.isA.emptyObject(criteria)) criteria = rangeCrit;
            else {
                criteria = isc.DS.combineCriteria(criteria, rangeCrit);
            }
        }
        criteria = isc.DS.compressNestedCriteria(criteria);
    }
    return this.Super("_filter", arguments);
},

getRangeCriteria : function (view) {
    // if no view passed, use the selected one - if one isn't selected, bail
    view = view || this.getSelectedView();
    if (!view) return null;
    if (!this.shouldIncludeRangeCriteria(view)) return null;

    var start = null,
        end = null,
        criteria = {},
        criteriaMode = view.rangeCriteriaMode || this.rangeCriteriaMode || "none"
    ;

    //if (this.loadEventsOnDemand) {
        if (criteriaMode == "auto") {
            // use the largest scrollable range from the visible views - fetches all events that
            // any of the views can reach
            var range = this.getLargestScrollableRange();
            start = range[0];
            end = range[1];
        } else if (criteriaMode != "none") {
            start = this.getVisibleStartDate(view);
            end = this.getVisibleEndDate(view);
        }
    //}



    if (start && end) {
        // fetchMode was something other than "all" - start and end have been set to
        // dates that span an appropriate range - use these to build a range criteria
        criteria = {
            _constructor: "AdvancedCriteria", operator: "and",
            criteria: [
                { fieldName: this.startDateField, operator: "lessThan", value: end},
                { fieldName: this.endDateField, operator: "greaterThan", value: start}
            ]
        };
    }

    // allow users to manipulate the criteria by overriding adjustCriteria()
    criteria = this.adjustCriteria(criteria);

    return criteria;
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
        newDate = isc.DateUtil.createLogicalDate(this.year, this.month, this.displayDate + 1);
        // if hiding weekends, find next non-weekend day
        if (!this.showWeekends && isc.DateUtil.isWeekend(newDate)) {
            newDate = isc.DateUtil.getNextWeekday(newDate, this.getWeekendDays());
        }
    } else if (this.weekViewSelected()) {
        newDate = isc.DateUtil.createLogicalDate(this.year, this.month, this.chosenDate.getDate() + 7);
    } else if (this.monthViewSelected()) {
        newDate = isc.DateUtil.createLogicalDate(this.year, this.month + 1, 1);
    } else if (this.timelineViewSelected()) {
        this.timelineView.nextOrPrev(true);
        return;
    }
    // only update the dateChooser now if it's part of the UI, rather than a popup
    if (this.showDateChooser && this.dateChooser) this.dateChooser.setData(newDate);
    this.setChosenDate(newDate);
},

//> @method calendar.previous()
// Move to the previous day, week, month, or timeline range depending on which tab is selected.
//
// @visibility calendar
//<
previous : function () {
    var newDate;
    if (this.dayViewSelected()) {
        newDate = isc.DateUtil.createLogicalDate(this.year, this.month, this.displayDate - 1);
        // if hiding weekends, find previous non-weekend day
        if (!this.showWeekends && isc.DateUtil.isWeekend(newDate)) {
            newDate = isc.DateUtil.getPreviousWeekday(newDate, this.getWeekendDays());
        }
    } else if (this.weekViewSelected()) {
        newDate = isc.DateUtil.createLogicalDate(this.year, this.month, this.displayDate - 7)
    } else if (this.monthViewSelected()) {
        newDate = isc.DateUtil.createLogicalDate(this.year, this.month - 1, 1);
    } else if (this.timelineViewSelected()) {
        this.timelineView.nextOrPrev(false);
        return;
    }
    // only update the dateChooser now if it's part of the UI, rather than a popup
    if (this.showDateChooser) this.dateChooser.setData(newDate);
    this.setChosenDate(newDate);
},

dataArrived : function () {
    // if _observeDataArrived
    if (this._observeDataArrived) this.dataChanged();
    return true;
},

// override draw to add the calendar navigation bar floating above the mainView tabbar
draw : function (a, b, c, d) {

    this._calendarDrawing = true;

    this.invokeSuper(isc.Calendar, "draw", a, b, c, d);

    if (isc.ResultSet && isc.isA.ResultSet(this.data) && this.dataSource) {
        if (!this.isObserving(this.data, "dataArrived")) {
            this.observe(this.data, "dataArrived", "observer.dataArrived(arguments[0], arguments[1])");
        }
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
        // if startDate is set, pass that because it might be a Datetime, where chosenDate
        // may be a logicalDate
        this.setChosenDate(this.startDate || this.chosenDate);
    } else {
        this.setDateLabel();
    }

    delete this._calendarDrawing;

    if (this._pendingScrollToTime) {
        // flag set up when scrollToTime() is called before draw()
        var time = this._pendingScrollToTime;
        delete this._pendingScrollToTime;
        this.scrollToTime(time);
    }
},

_getTabPane : function (viewName) {
    var props = { viewName: viewName, calendar: this, baseStyle: this.baseStyle,
            startDate: this.startDate, chosenDate: this.chosenDate, autoDraw: false
    };
    if (viewName == "day") {
        this.dayView = this.createAutoChild("dayView",
            isc.addProperties({}, props,
                { cellHeight: this.rowHeight, enforceVClipping: true }
            )
        );
        return this.dayView;
    } else if (viewName == "week") {
        this.weekView = this.createAutoChild("weekView",
            isc.addProperties({_refreshEventsOnDraw: true}, props,
                { cellHeight: this.rowHeight, enforceVClipping: true }
            )
        );
        return this.weekView;
    } else if (viewName == "month") {
        this.monthView = this.createAutoChild("monthView",
            isc.addProperties({}, props,
                { bodyConstructor:"MonthScheduleBody"}
            )
        );
        return this.monthView;
    } else if (viewName == "timeline") {
        this.timelineView = this.createAutoChild("timelineView",
            isc.addProperties({}, props));
        return this.timelineView;
    }
},

_getTabs : function () {
    var nTabs = [];
    if (this.showDayView != false) {
        nTabs.add({title: this.dayViewTitle, pane: null, viewName: "day" });
    }
    if (this.showWeekView != false) {
        nTabs.add({title: this.weekViewTitle, pane: null, viewName: "week" });
    }
    if (this.showMonthView != false) {
        nTabs.add({title: this.monthViewTitle, pane: null, viewName: "month" });
    }
    if (this.showTimelineView != false) {
        nTabs.add({title: this.timelineViewTitle, pane: null, viewName: "timeline" });
    }
    return nTabs;
},

_createTabSet : function (tabsArray) {
    // if there is only one view displayed, don't use tabs
    if (tabsArray.length > 1) {
        this.mainView = this.createAutoChild("mainView", {
            tabs: tabsArray,
            _tabSelected : function (tab) {
                this.Super("_tabSelected", arguments);
                // store selected view name for later use, in day/week/monthViewSelected functions
                var tabPane = this.getTabPane(tab);
                if (!tabPane) {
                    // lazily create calendar views on tab selection
                    this.updateTab(tab, this.creator._getTabPane(tab.viewName));
                    tabPane = this.getTabPane(tab);
                }
                this.creator._selectedViewName = tabPane.viewName;
                this.creator.setDateLabel();
                var view = this.creator.getSelectedView();
                // update the column dates and titles, and the row datetimes
                if (view._needsUpdateRange) view.updateRange();
                // if the view is already drawn, redraw it now to ensure that cellStyles update
                if (view.isDrawn()) view.redraw();
                else {
                    // if the view isn't drawn yet, delay the currentViewChanged() notification until draw
                    view._fireViewChangedOnDraw = true;
                }
                if (view._needsRefresh) {
                    view._refreshEvents();
                    //this.creator.refreshSelectedView();
                }
                if (!view._fireViewChangedOnDraw) {
                    this.creator.currentViewChanged(tabPane.viewName);
                }
            }
        } );
        var tabToSelect;
        // set the default tab according to currentViewName if defined
        if (this.currentViewName) {
            tabToSelect = tabsArray.find("viewName", this.currentViewName);
            if (tabToSelect) this.mainView.selectTab(tabToSelect);
        } else if (this.minimalUI) {
            // for some devices, set the default view according to device orientation
            this.pageOrientationChanged();
        } else {
            // ensure that a tab is know to be selected at this time - the current tab
            var viewName = this.weekView ? "week" :
                    (this.dayView ? "day" :
                        (this.monthView ? "month" :
                            (this.timelineView ? "timeline" : null)));
            if (viewName) {
                tabToSelect = tabsArray.find("viewName", viewName);
                if (tabToSelect) {
                    this.mainView.selectTab(tabToSelect);
                    this.mainView.viewName = viewName;
                    this._selectedViewName = viewName;
                }
            }
        }
    } else {
        this.mainView = this._getTabPane(tabsArray[0].viewName);
    }
},

getLaneMap : function () {
    if (!this.isTimeline() && !this.showDayLanes) return {};
    // if cached, return it
    if (this.laneMap) return this.laneMap;
    // otherwise, cache it
    var data = this.showDayLanes ? this.lanes :
            this.canGroupLanes ? this.timelineView.getOriginalData() : this.timelineView.data,
        laneMap = {}
    ;

    for (var i=0; i<data.length; i++) {
        var name = data[i].name || data[i][this.laneNameField],
            title = data[i].title || name
        ;
        laneMap[name] = title;
    }
    this.laneMap = isc.addProperties({}, laneMap);
    return laneMap;
},

getSublaneMap : function (lane, view) {
    view = view || this.getSelectedView();
    var sublaneMap = {};
    if (isc.isA.String(lane)) lane = view.getLane(lane);
    if (lane && lane.sublanes) {
        for (var i=0; i<lane.sublanes.length; i++) {
            var sublane = lane.sublanes[i],
                name = sublane.name || sublane[this.laneNameField],
                title = sublane.title || name
            ;
            sublaneMap[name] = title;
        }
    }
    return sublaneMap;
},

//> @method calendar.getLanePadding()
// For views that support +link{calendar.lanes, lanes}, returns the padding to apply to events
// rendered in lanes in the passed or current view.  By default, returns
// +link{calendar.laneEventPadding, laneEventPadding}.
//
// @param [view] (CalendarView) the view to get the lane padding for
// @return (Integer) the padding to apply to events in lanes in the passed or current view
//
// @visibility external
//<
getLanePadding : function (view) {
    view = view || this.getSelectedView();
    if (view && view.useLanePadding()) return this.laneEventPadding;
    return 0;
},

//> @method calendar.setLaneTitle()
// For views that support +link{calendar.lanes, lanes}, updates the title for the passed lane.
//
// @param lane (String) the name of the lane to change the title for
// @param title (String) the new title to apply
// @return (boolean) true if the title was updated, false otherwise
//
// @visibility external
//<
setLaneTitle : function (lane, title) {
    var view = this.getSelectedView();
    if (view && view.setLaneTitle) {
        return view.setLaneTitle(lane, title);
    }
    return false;
},

//> @method calendar.getLaneEvents()
// For views that support +link{calendar.lanes, lanes}, returns the array of events in the
// current dataset that apply to the passed lane in the passed or current view.
//
// @param lane (Lane | String) lane object or name to get the events for
// @param [view] (CalendarView) the view in which the passed lane lives - uses the selected
//                              view if unset
// @return (Array of CalendarEvent) the list of events that apply to the passed lane and view
//
// @visibility external
//<
getLaneEvents : function (lane, view) {
    // deal with being passed a lane object - bail if there's no appropriate lane-name
    var laneName = isc.isAn.Object(lane) ? lane.name : lane;
    if (!laneName || !isc.isA.String(laneName)) return [];
    // default to the selected view
    view = view || this.getSelectedView();
    var allEvents = this.data.findAll(this.laneNameField, laneName) || [],
        visibleEvents = []
    ;
    for (var i=0; i<allEvents.length; i++) {
        var event = allEvents[i];
        if (!event) continue;
        if (this.shouldShowEvent(event, view)) {
            visibleEvents.add(event);
        }
    }
    return visibleEvents;
},

// helper to return whether any *visible* events are available for the passed lane - works like
// getLaneEvents() but returns a boolean on the first match, rather than returning all matches
laneHasVisibleEvents : function (lane, view) {
    // deal with being passed a lane object - bail if there's no appropriate lane-name
    var laneName = isc.isAn.Object(lane) ? lane.name : lane;
    if (!laneName || !isc.isA.String(laneName)) return [];
    // default to the selected view
    view = view || this.getSelectedView();
    var allEvents = this.data.findAll(this.laneNameField, laneName) || [];
    for (var i=0; i<allEvents.length; i++) {
        if (!allEvents[i]) continue;
        if (this.shouldShowEvent(allEvents[i], view)) return true;
    }
    return false;
},

//> @method calendar.getSublaneEvents()
// For views that support +link{calendar.lanes, lanes} and allow
// +link{calendar.useSublanes, sublanes}, returns the array of events in the
// current dataset that apply to the passed lane and sublane in the passed or current view.
//
// @param lane (Lane | String) lane object or name to get the events for
// @param sublane (Lane | String) sublane object or name to get the events for
// @param [view] (CalendarView) the view in which the passed sublane lives - uses the selected
//                              view if unset
// @return (Array of CalendarEvent) the list of events that apply to the passed sublane and view
//
// @visibility external
//<
getSublaneEvents : function (lane, sublane, view) {
    // deal with being passed lane/sublane objects - bail if either is missing
    var lName = isc.isAn.Object(lane) ? lane.name : lane,
        slName = isc.isAn.Object(sublane) ? sublane.name : sublane
    ;
    if ((!lName || !isc.isA.String(lName)) || (!slName || !isc.isA.String(slName))) {
        return [];
    }
    // use the selected view if not passed
    view = view || this.getSelectedView();
    var laneEvents = this.getLaneEvents(lName, view),
        sublaneEvents = laneEvents.findAll(this.sublaneNameField, slName)
    ;
    return sublaneEvents;
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
            click : function () {
                var cal = this.creator;
                var currView = cal.getSelectedView();

                var sDate = currView.chosenDate.duplicate(),
                    eDate = null,
                    pickedDate = sDate.duplicate()
                ;

                if (currView.isDayView()) {
                    // start-date to chosen date
                    sDate = pickedDate;
                    // endDate is startDate + 1 hour
                    eDate = sDate.duplicate();
                    eDate.setHours(eDate.getHours()+1);
                } else if (currView.isWeekView()) {
                    // if weekView, set dialog to first day of chosen week unless today is greater
                    if (currView.startDate.getTime() > sDate.getTime()) {
                        sDate = currView.startDate.duplicate();
                    }
                    // if hiding weekends, find next non-weekend day
                    if (!this.showWeekends) {
                        var wends = cal.getWeekendDays();
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
                    // endDate is startDate + 1 hour
                    eDate = sDate.duplicate();
                    eDate.setHours(eDate.getHours()+1);
                } else if (currView.isMonthView()) {
                    // if monthView, set dialog to first day of chosen month unless
                    // today is greater
                    pickedDate.setDate(1);
                    sDate = pickedDate.duplicate();
                    // endDate is startDate + 1 hour
                    eDate = sDate.duplicate();
                    eDate.setHours(eDate.getHours()+1);
                } else if (cal.isTimeline()) {
                    var tl = cal.timelineView,
                        dates = tl.getVisibleDateRange();
                    sDate = dates[0];
                    eDate = sDate.duplicate();
                    eDate = tl.addUnits(eDate, 1, cal.timelineGranularity);
                }

                var newEvent = {};
                newEvent[cal.startDateField] = sDate;
                newEvent[cal.endDateField] = eDate;
                cal.showEventEditor(newEvent, true);
            }
        } );

        // datePickerButton
        this.datePickerButton = this.createAutoChild("datePickerButton", {
            click: function () {

                var cal = this.creator;
                var dateChooser = cal.getDateChooser();
                // dateChooser shows the same firstDayOfWeek as the parent Calendar
                dateChooser.firstDayOfWeek = cal.firstDayOfWeek;
                // only the TimelineView supports data-type inclusion of the TimeItem - other
                // views never show it
                var view = cal.getSelectedView();
                var showTime = cal.getSelectedView().showDateChooserTimeItem;
                if (showTime == null) {
                    var editingStyle = cal.getDateEditingStyle();
                    showTime = !(editingStyle == "date");
                }
                if (showTime != null) {
                    dateChooser.showTimeItem = showTime;
                    // modify showApplyButton as well
                    dateChooser.showApplyButton = showTime;
                }
                dateChooser.setData(view.chosenDate, showTime == null);

                if (!dateChooser.isDrawn()) dateChooser.draw();
                else dateChooser.redraw();
                dateChooser.placeNear(this.getPageLeft(), this.getPageTop() + this.getHeight());
                dateChooser.show();
            }
        } );

        if (this.minimalUI && this.showMonthButton != false && this.showMonthView != false) {
            this.monthButton = this.createAutoChild("monthButton");
            this.updateMonthButton();
        }

        this.previousButton = this.createAutoChild("previousButton", {});

        this.nextButton = this.createAutoChild("nextButton", {});
    }
    var cbMems = [];
    if (this.monthButton) cbMems.add(this.monthButton);
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

    // layout for date chooser and main calendar view
    if (!this.children) this.children = [];
    var mainMembers = [];
    // if showDateChooser is true, add the dateChooser as a child of the Calendar, because it
    // will be visible - otherwise, deparent the dateChooser so it's size isn't clipped when
    // displayed by the picker button later, if the calendar itself is smaller than the
    // dateChooser will be
    if (this.showDateChooser) {
        this.addChild(this.getDateChooser());
        var subMembers = [];
        //if (this.canCreateEvents) subMembers.add(this.addEventButton);
        subMembers.add(this.dateChooser);
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

            this.controlsBarContainer = this.createAutoChild("controlsBarContainer", {
                    autoDraw: false,
                    height: this.controlsBar.getVisibleHeight(),
                    width: "100%"
            }, isc.HLayout);

            this.controlsBarContainer.addMember(isc.LayoutSpacer.create({autoDraw:false, width:"*"}));
            this.controlsBarContainer.addMember(this.controlsBar);
            this.controlsBarContainer.addMember(isc.LayoutSpacer.create({autoDraw:false, width:"*"}));
            this.mainLayout = this.createAutoChild("mainLayout", { autoDraw:false,
                    members: [this.controlsBarContainer, this.mainView]
            }, isc.VLayout);

            mainMembers.add(this.mainLayout);
        } else {
            mainMembers.add(this.mainView);
        }
    }

    this.children.add(
        isc.HLayout.create({
            autoDraw:false,
            overflow: "hidden",
            width: "100%",
            height: "100%",
            members:mainMembers
        })
    );

}, // end createChildren

getDateChooser : function () {
    if (this.dateChooser) return this.dateChooser;
    this.dateChooser = this.createAutoChild("dateChooser", {

        disableWeekends: this.disableChooserWeekends != null ? this.disableChooserWeekends : false,
        showWeekends: this.showChooserWeekends != null ? this.showChooserWeekends : true,
        weekendDays: this.getWeekendDays(),
        chosenDate: this.chosenDate,
        month: this.month,
        year: this.year,
        closeOnEscapeKeypress: true,
        autoHide: true,
        autoClose: true,
        visibility: "hidden",
        // dateChooser shows the same firstDayOfWeek as the parent Calendar
        firstDayOfweek: this.firstDayOfWeek,
        // override dataChanged to change the selected day
        dataChanged : function () {
            var nDate = this.getData();  // this call combines data and time appropriately
            // no need to rebuild lanes when just changing date - TimelineView only
            this.creator.getSelectedView()._skipSetLanes = true;
            if (nDate) this.creator.setChosenDate(nDate);
            // clear _skipSetLanes - TimelineView only
            delete this.creator.getSelectedView()._skipSetLanes;
            return nDate;
        },
        show : function () {
            this.Super("show", arguments);
            this.bringToFront();
            this.focus();
        }
    } );
    this.dateChooser.deparent();
    return this.dateChooser;
},

getEventDialog : function () {
    // create the eventDialog lazily, as required
    if (this.eventDialog) return this.eventDialog;

    var cal = this;

    // quick event dialog
    this.eventDialog = this.createAutoChild("eventDialog", {
        items: [
            isc.DynamicForm.create({
                autoDraw: false,
                padding:4,
                calendar: this,
                saveOnEnter: true,
                implicitSave: false,
                implicitSaveOnBlur: false,
                useAllDataSourceFields: true,
                numCols: 2,
                colWidths: [80, "*"],
                _internalFields : [cal.nameField, cal.laneNameField, cal.sublaneNameField],

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
                        showLane = cal.isTimeline() || (cal.showDayLanes && cal.dayViewSelected()),
                        showSublane = showLane && cal.useSublanes
                    ;

                    // set up default fields
                    var fieldList = [
                        {name: cal.nameField, title: cal.eventNameFieldTitle, type: "text",
                            width: 250, wrapTitle: false
                        },
                        {name: cal.laneNameField, title: cal.eventLaneFieldTitle,
                                type: "select", width: 150,
                                valueMap: cal.getLaneMap(),
                                showIf: showLane ? "true" : "false",
                                changed : function (form, item, value) {
                                    var lane = cal.lanes.find("name", value);
                                    if (value && lane) {
                                        var slItem = form.getItem(cal.sublaneNameField);
                                        if (slItem) slItem.setValueMap(cal.getSublaneMap(lane));
                                    }
                                }
                        },
                        {name: cal.sublaneNameField, title: cal.eventSublaneFieldTitle,
                            type: "select", width: 150,
                            valueMap: [], //cal.getLaneMap(),
                            showIf: showSublane ? "true" : "false"
                        },
                        {name: "save", title: cal.saveButtonTitle, editorType: "SubmitItem", endRow: false },
                        {name: "details", title: cal.detailsButtonTitle, type: "button", startRow: false,
                            click : function (form, item) {
                                var cal = form.calendar,
                                isNew = cal.eventDialog.isNewEvent,
                                event = cal.eventDialog.event || {},
                                name = form.getValue(cal.nameField),
                                laneName = form.getValue(cal.laneNameField),
                                sublaneName = form.getValue(cal.sublaneNameField)
                                ;
                                if (isNew) {
                                    event[cal.nameField] = name;
                                    if (laneName) event[cal.laneNameField] = laneName;
                                    if (sublaneName) event[cal.sublaneNameField] = laneName;
                                }
                                cal.showEventEditor(event, isNew);
                            }
                        }
                    ];

                    // remove internal DS if it exists
                    if (this.ds) {
                        if (this.ds.destroy) this.ds.destroy();
                        this.ds = null;
                    }
                    // create an internal ds and bind to it so that the default fields can be
                    // overridden. See forms->validation->customized binding in the feature explorer
                    this.ds = isc.DataSource.create({
                        fields: fieldList,
                        clientOnly: true
                    });
                    // only dataSource then fields seems to work
                    this.setDataSource(this.ds);
                    this.setFields(isc.shallowClone(this.calendar.eventDialogFields));
                },
                submit : function () {
                    var cal = this.calendar,
                        isNewEvent = cal.eventDialog.isNewEvent,
                        evt = cal.eventDialog.event || {},
                        sdate = cal.eventDialog.currentStart,
                        edate = cal.eventDialog.currentEnd,
                        dataForm = this,
                        lane = null,
                        sublane = null
                    ;

                    if (!dataForm.validate()) return;

                    if (cal.isTimeline() || (cal.dayViewSelected() && cal.showDayLanes)) {
                        lane = dataForm.getValue(cal.laneNameField);
                        sublane = dataForm.getValue(cal.sublaneNameField);
                    }

                    var customValues = isc.addProperties({}, dataForm.getCustomValues());

                    cal._fromEventDialog = true;
                    var newEvent = cal.createEventObject(evt,
                            evt[cal.startDateField], evt[cal.endDateField],
                            lane, sublane, dataForm.getValue(cal.nameField)
                    );
                    if (!isNewEvent) { // event window clicked, so update
                        cal.updateCalendarEvent(evt, newEvent, customValues);
                    } else { // create new event
                        cal.addCalendarEvent(newEvent, customValues);
                    }
                    cal.hideEventDialog();
                },
                getCalendarView : function () {
                    return this.calendar.getSelectedView();
                },
                destroy : function () {
                    if (this.ds) {
                        // destroy the ds used for the custom form
                        if (this.ds.destroy) this.ds.destroy();
                        this.ds = null;
                    }
                    this.calendar = null;
                    return this.Super("destroy", arguments);
                }
            })
        ],

        setDate : function (startDate, endDate) {
            var cal = this.creator;
            if (!endDate) {
                // handle the case where where the startDate is 11:30 pm...in this case only
                // do a 1/2 hour long event
                if (startDate.getHours() == 23
                        && startDate.getMinutes() == (60 - cal.getSelectedView().getTimePerCell())) {
                    endDate = isc.DateUtil.createDatetime(startDate.getFullYear(), startDate.getMonth(),
                    startDate.getDate() + 1);
                } else {
                    endDate = isc.DateUtil.createDatetime(startDate.getFullYear(), startDate.getMonth(),
                        startDate.getDate(), startDate.getHours() + 1, startDate.getMinutes());
                }
            }

            this.logicalStartDate = isc.DateUtil.getLogicalDateOnly(startDate);
            this.logicalStartTime = isc.DateUtil.getLogicalTimeOnly(startDate);

            this.logicalEndDate = isc.DateUtil.getLogicalDateOnly(endDate);
            this.logicalEndTime = isc.DateUtil.getLogicalTimeOnly(endDate);

            this.setTitle(cal._getEventDialogTitle(startDate, endDate));
            if (this.isNewEvent) {
                this.event[cal.startDateField] = startDate.duplicate();
                this.event[cal.endDateField] = endDate.duplicate();
            }
            this.currentStart = startDate;
            this.currentEnd = endDate;
            this.items[0].setValue(cal.nameField, "");
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
                buttonForm = this.items[1],
                cal = this.creator,
                view = cal.getSelectedView(),
                isNew = !!this.isNewEvent,
                // allow lane-editing if it's a new event, even if canEditLane is false
                canEditLane = isNew || cal.canEditEventLane(event, view)
            ;

            if (view.usesLanes()) theForm.getItem(cal.laneNameField).setDisabled(!canEditLane);

            // if we have custom fields, clear errors and set those custom fields
            if (cal.eventDialogFields) {
                theForm.clearErrors(true);
                theForm.setCustomValues(event);
            }
            this.setDate(cal.getEventStartDate(event), cal.getEventEndDate(event));

            if (view.hasSublanes() && event[cal.laneNameField]) {
                var lane = view.getLane(event[cal.laneNameField]);
                if (lane) {
                    var slItem = theForm.getItem(cal.sublaneNameField);
                    slItem.setValueMap(cal.getSublaneMap(lane));
                }
            }
            theForm.setValues(event);
        },

        closeClick : function () {
            this.Super('closeClick', arguments);
            // When the dialog is dismissed, select the event
            // (So a user can tab to an event, hit enter, hit escape and focus goes
            // back to the event as expected)
            if (this.creator.canSelectEvents) {
                var view = this.creator.getSelectedView(),
                    eventCanvas = view && view.getCurrentEventCanvas(this.event);
                if (eventCanvas && eventCanvas._canFocus()) {
                    eventCanvas.focus();
                }
            }
            this.creator.clearViewSelection();
        },

        show : function () {
            if (this.creator.showQuickEventDialog) {

                if (!this.isDrawn()) this.draw();
                this.Super('show', arguments);
                this.items[0].getItem(this.creator.nameField).focusInItem();
            } else {
                this.creator.showEventEditor(this.event, this.isNewEvent);
            }
        },

        hide : function () {
            this.Super('hide', arguments);
            this.moveTo(0, 0);
        },

        destroy : function () {
            this.calendar = null;
            this.Super("destroy", arguments);
        }

    } );
    this.eventDialog.form = this.eventDialog.items[0];
    return this.eventDialog;
},

getEventEditorLayout : function () {
    // create the eventEditor and layout lazily, as required
    if (this.eventEditorLayout) return this.eventEditorLayout;

    var cal = this;

    // event editor form
    this.eventEditor = this.createAutoChild("eventEditor", {
        useAllDataSourceFields: true,
        titleWidth: 80,
        calendar: this,
        initWidget : function () {
            // invoke initWidget here rather than at the end of the function, or we get multiple
            // log warnings about form fields being clobbered
            this.invokeSuper(isc.DynamicForm, "initWidget", arguments);

            var cal = this.creator;
            this.timeFormat = cal.timeFormat;
            this.rebuildFieldList();
        },
        rebuildFieldList : function () {
            var cal = this.creator,
                view = cal.getSelectedView(),
                fieldList = [],
                editStyle = cal.getDateEditingStyle(),
                durationFields = [
                    { name: "endType", type: "text", showTitle: false, width: "*",
                        editorType: "SelectItem", textAlign: "right",
                        valueMap: [ cal.eventDurationFieldTitle, cal.eventEndDateFieldTitle ],
                        endRow: false,
                        changed : function (form, item, value) {
                            var cal = form.calendar;
                            editStyle = cal.getDateEditingStyle();
                            if (value == cal.eventDurationFieldTitle) {
                                form.getItem(cal.durationField).show();
                                form.getItem(cal.durationUnitField).show();
                                if (editStyle == "time") {
                                    form.getItem("endHours").hide();
                                    form.getItem("endMinutes").hide();
                                    form.getItem("endAMPM").hide();
                                } else {
                                    form.getItem(cal.endDateField).hide();
                                }
                            } else {
                                form.getItem(cal.durationField).hide();
                                form.getItem(cal.durationUnitField).hide();
                                if (editStyle == "time") {
                                    form.getItem("endHours").show();
                                    form.getItem("endMinutes").show();
                                    form.getItem("endAMPM").show();
                                } else {
                                    form.getItem(cal.endDateField).show();
                                }
                            }
                        }
                    },
                    { name: cal.durationField, type: "integer", editorType: "SpinnerItem",
                        title: cal.eventDurationFieldTitle, endRow: false, showTitle: false,
                        width: "*", colSpan: 1, defaultValue: 1
                    },
                    { name: cal.durationUnitField, type: "text", showTitle: false, endRow: true,
                        title: cal.eventDurationUnitFieldTitle, width: "*", colSpan: 1,
                        valueMap: cal.getDurationUnitMap(), defaultValue: "minute"
                    }
                ]
            ;

            // when the "durationCheckbox" is checked, show the duration/UnitField items
            this._internalFields.addList([cal.nameField, cal.descriptionField,
                cal.startDateField, "endType",
                cal.durationField, cal.durationUnitField,
                cal.endDateField
            ]);

            if (view.usesLanes()) {
                // if the calendar allows laneEditing, show the lane picker - if a given event
                // is canEditLane: false, the picker will be disabled
                var laneMap = cal.getLaneMap(),
                    field = { name: cal.laneNameField, title: cal.eventLaneFieldTitle, type: "select",
                        valueMap: laneMap, endRow: true,
                        width: "*", colSpan: 3,
                        changed : function (form, item, value) {
                            // when the lane changes, refetch the list of sublanes
                            var cal = form.calendar;
                            var lane = cal.lanes.find("name", value);
                            if (value && lane) {
                                var slItem = form.getItem(cal.sublaneNameField);
                                if (slItem) slItem.setValueMap(cal.getSublaneMap(lane));
                            }
                        }
                    }
                ;
                fieldList.add(field);
                if (cal.useSublanes) {
                    // if the calendar allows laneEditing, show the lane picker - if a given event
                    // is canEditLane: false, the picker will be disabled
                    var sublaneMap = {},
                        slField = { name: cal.sublaneNameField, title: cal.eventSublaneFieldTitle,
                            type: "select", valueMap: sublaneMap, endRow: true,
                            width: "*", colSpan: 3
                        }
                    ;
                    fieldList.add(slField);
                }
            }

            // duration fields - a selectItem for allowing the change between using an end date
            // or a duration, a spinner for the duration value and a selectItem for the unit
            var allowDurations = cal.allowDurationEvents;
            if (editStyle == "date" || editStyle == "datetime") {
                fieldList.add({ name: cal.startDateField, title: cal.eventStartDateFieldTitle,
                        type: editStyle, colSpan: "*", endRow: true
                });
                if (allowDurations) fieldList.addList(durationFields);
                fieldList.addList([
                    { name: cal.endDateField, title: cal.eventEndDateFieldTitle,
                        showTitle: !allowDurations, type: editStyle, colSpan: "*", endRow: true
                    },
                    { name: "invalidDate", type: "blurb", width: "*", colSpan: "*",
                        visible: false,
                        defaultValue: cal.invalidDateMessage,
                        cellStyle: this.errorStyle || "formCellError", endRow: true
                    }
                ]);
            } else if (editStyle == "time") {
                fieldList.add({ name: "logicalStartDate", title: cal.eventEditorDateFieldTitle,
                    type: "date", colSpan: "*", endRow: true,
                    changed : function (form, item) {
                        form.calendar.eventEditorLayout.logicalStartDate = item.getValue();
                    }
                });
                var baseStyleName = isc.SelectItem.getInstanceProperty("textBoxStyle"),
                    ctrlStyleName = isc.SelectItem.getInstanceProperty("controlStyle"),
                    iconStyleName = isc.SelectItem.getInstanceProperty("pickerIconStyle"),
                    // get the extra width to add to the render-width of the widest valueMap entry
                    extraWidth = isc.SelectItem.getInstanceProperty("pickerIconWidth") +
                        isc.Element.getHMarginSize(baseStyleName) +
                        isc.Element._getHBorderPad(baseStyleName) +
                        4 // this last is necessary because there is no right-padding
                ;

                if (ctrlStyleName) {
                    // include padding/margin/border from the controlStyle
                    extraWidth += isc.Element.getHMarginSize(ctrlStyleName);
                    extraWidth += isc.Element._getHBorderPad(ctrlStyleName);
                }

                if (iconStyleName) {
                    // include padding/margin/border from the pickerIcon's style
                    extraWidth += isc.Element.getHMarginSize(iconStyleName);
                    extraWidth += isc.Element._getHBorderPad(iconStyleName);
                }

                // calculate widths for the hour, minute and AM/PM pickers, wide enough for
                // whatever the contents will be
                valueHTML = isc.getValues(this.getTimeValues("hours")).join("<br>");
                var hoursWidth = isc.Canvas.measureContent(valueHTML, baseStyleName) + extraWidth;

                valueHTML = isc.getValues(this.getTimeValues("minutes")).join("<br>");
                var minutesWidth = isc.Canvas.measureContent(valueHTML, baseStyleName) + extraWidth;

                var valueHTML = isc.getValues(this.getTimeValues()).join("<br>");
                var ampmWidth = isc.Canvas.measureContent(valueHTML, baseStyleName) + extraWidth;

                // set up the form columns
                this.numCols = 5;
                this.setColWidths([this.titleWidth, hoursWidth, minutesWidth, ampmWidth, "*"]);
                // and add the items
                fieldList.addList([
                    {name: "startHours", title: cal.eventStartDateFieldTitle, type: "integer",
                        width: hoursWidth, editorType: "select",
                        valueMap: this.getTimeValues("hours")
                    },
                    {name: "startMinutes", showTitle: false, type: "integer", width: minutesWidth,
                        editorType: "select", valueMap: this.getTimeValues("minutes")},
                    {name: "startAMPM", showTitle: false, type: "select", width: ampmWidth,
                        valueMap: this.getTimeValues(), endRow: true,
                        showIf: function (item) {
                            if (!item.form) return "false";
                            return item.form.calendar.twentyFourHourTime ? "false" : "true";
                        }
                    },
                    {name: "invalidDate", type: "blurb", colSpan: 4, visible: false,
                     defaultValue: cal.invalidDateMessage,
                     cellStyle: this.errorStyle || "formCellError", endRow: true}
                ]);
                if (allowDurations) fieldList.addList(durationFields);
                fieldList.addList([
                    {name: "endHours", type: "integer", width: "*", startRow: true,
                        title: cal.eventEndDateFieldTitle, showTitle: !allowDurations,
                        editorType: "select", valueMap: this.getTimeValues("hours")},
                    {name: "endMinutes", showTitle: false, type: "integer", width: "*",
                        editorType: "select", valueMap: this.getTimeValues("minutes")},
                    {name: "endAMPM", showTitle: false, type: "select", width: ampmWidth,
                        valueMap: this.getTimeValues(), endRow: true,
                        showIf: function (item) {
                            if (!item.form) return "false";
                            return item.form.calendar.twentyFourHourTime ? "false" : "true";
                        }
                    }
                ]);
            }

            fieldList.addList([
                {name: cal.nameField, title: cal.eventNameFieldTitle, type: "text",
                    colSpan: "*", width: "*", startRow: true},
                {name: cal.descriptionField, title: cal.eventDescriptionFieldTitle,
                    type: "textArea", colSpan: "*", width: "*", height: 50, startRow: true}
            ]);

            // remove internal DS if it exists
            if (this.ds) {
                if (this.ds.destroy) this.ds.destroy();
                this.ds = null;
            }
            // create an internal ds and bind to it so that the default fields can be
            // overridden. See forms->validation->customized binding in the feature explorer
            this.ds = isc.DataSource.create({
                fields: fieldList,
                clientOnly: true
            });
            // only dataSource then fields seems to work
            this.setDataSource(this.ds);
            var fieldsToUse = isc.shallowClone(cal.eventEditorFields);
            this.setFields(fieldsToUse);
            this._fieldListSet = true;
        },
        getTimeValues : function (type) {
            var obj = {},
                cal = this.calendar
            ;
            if (type == "hours") {
                // use 0-23 for 24-hour time and 1-12 for 12-hour time
                var use24Hrs = cal.twentyFourHourTime,
                    count = use24Hrs ? 24 : 12,
                    delta = use24Hrs ? 0 : 1
                ;
                for (var i = 0; i < count; i++) {
                    // stringify the hours
                    var stringHour = (i + delta < 10 ? "0" : "") + (i + delta);
                    obj["" + (i + delta)] = stringHour;
                }
            } else if (type == "minutes") {
                for (var i = 0; i < 60; i++) {
                    // stringify the minutes
                    var stringMin = i < 10 ? "0" + i : "" + i;
                    obj[i + ""] = stringMin;
                }
            } else {
                obj["am"] = isc.Time.AMIndicator;
                obj["pm"] = isc.Time.PMIndicator;
            }

            return obj;
        },
        _internalFields : ["startHours", "startMinutes", "startAMPM", "endHours",
                "endMinutes", "endAMPM" ],
        getCustomValues : function () {
            if (!this.creator.eventEditorFields) return;
            var cal = this.calendar,
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
            if (!this.calendar.eventEditorFields) return;
            var internalValues = this._internalFields;
            var fields = this.calendar.eventEditorFields;
            for (var i = 0; i < fields.length; i++) {
                var fld = fields[i];
                if (fld.name && !internalValues.contains(fld.name)) {
                    this.setValue(fld.name, values[fld.name]);
                }
            }

        },
        destroy : function () {
            if (this.ds) {
                // destroy the ds used for the custom form
                if (this.ds.destroy) this.ds.destroy();
                this.ds = null;
            }
            this.calendar = null;
            return this.Super("destroy", arguments);
        }
    } );

    // event editor layout
    this.eventEditorLayout = this.createAutoChild("eventEditorLayout", isc.addProperties({
        calendar: this,
        // eventEditorLayout_setDate
        setDate : function (startDate, endDate, eventName, lane, sublane) {
            if (!eventName) eventName = "";
            if (!endDate) {
                endDate = isc.DateUtil.dateAdd(startDate, "h");
            }
            var cal = this.calendar;

            this.logicalStartDate = isc.DateUtil.getLogicalDateOnly(startDate);
            this.logicalStartTime = isc.DateUtil.getLogicalTimeOnly(startDate);

            this.logicalEndDate = isc.DateUtil.getLogicalDateOnly(endDate);
            this.logicalEndTime = isc.DateUtil.getLogicalTimeOnly(endDate);

            this.setTitle(cal._getEventDialogTitle(startDate, endDate));
            this.currentStart = startDate;
            this.currentEnd = endDate;

            // cater for dateEditingStyle
            var editStyle = cal.getDateEditingStyle(),
                form = this.items[0]
            ;
            if (editStyle == "date" || editStyle == "datetime") {
                form.setValue(cal.startDateField, startDate.duplicate());
                form.setValue(cal.endDateField, endDate.duplicate());
            } else if (editStyle == "time") {
                form.setValue("logicalStartDate", this.logicalStartDate);
                var formatter = cal.twentyFourHourTime ? "toShortPadded24HourTime" : cal.timeFormatter,
                    // use the logicalTime to cater for positioning with a custom display timezone
                    sTime = this.logicalStartTime,
                    eTime = this.logicalEndTime
                ;
                form.setValue("startHours", sTime.getHours());
                form.setValue("endHours", eTime.getHours());
                form.setValue("startMinutes", sTime.getMinutes());
                form.setValue("endMinutes", eTime.getMinutes());
                if (!cal.twentyFourHourTime) {
                    form.setValue("startAMPM", this.getAMPM(sTime.getHours()));
                    form.setValue("endAMPM", this.getAMPM(eTime.getHours()));
                }
            }
        },

        getHours : function (hour) {
            if (this.creator.twentyFourHourTime) return hour;
            else return this.creator._to12HrNotation(hour);
        },

        getAMPM : function (hour) {
            if (hour < 12) return "am";
            else return "pm";
        },

        createButtonLayout : function () {
            // this layout and it's buttons are documented autoChildren of the Calendar
            this.buttonLayout = this.calendar.createAutoChild("eventEditorButtonLayout");
            this.saveButton = this.calendar.createAutoChild("saveButton",
                { title: this.calendar.saveButtonTitle, calendar: this.calendar });
            this.removeButton = this.calendar.createAutoChild("removeButton",
                { title: this.calendar.removeButtonTitle, calendar: this.calendar });
            this.cancelButton = this.calendar.createAutoChild("cancelButton",
                { title: this.calendar.cancelButtonTitle, calendar: this.calendar });
            this.buttonLayout.addMembers([this.saveButton, this.removeButton, this.cancelButton]);
            this.addItem(this.calendar.eventEditor);
            this.addItem(this.buttonLayout);
        },
        // eventEditorLayout_setEvent
        setEvent : function (event) {
            if (!this.buttonLayout) {
                // create the various buttons on first access
                this.createButtonLayout();
            }

            var form = this.items[0],
                cal = this.calendar,
                view = this.view,
                laneSwitcher = form.getItem(cal.laneNameField),
                sublaneSwitcher = form.getItem(cal.sublaneNameField),
                allowDurations = cal.allowDurationEvents,
                fDurationCB = form.getItem("endType"),
                fDuration = form.getItem(cal.durationField),
                fDurationUnit = form.getItem(cal.durationUnitField)
            ;

            if (!cal.twentyFourHourTime) {
                if (form.getItem("startAMPM")) form.showItem("startAMPM");
                if (form.getItem("endAMPM")) form.showItem("endAMPM");
            } else {
                if (form.getItem("startAMPM")) form.hideItem("startAMPM");
                if (form.getItem("endAMPM")) form.hideItem("endAMPM");
            }

            this.event = event;
            // if we have custom fields, clear errors and set those custom fields
            if (cal.eventEditorFields) {
                form.clearErrors(true);
                form.setCustomValues(event);
            }
            if (laneSwitcher) {
                laneSwitcher.setValueMap(cal.getLaneMap());
                laneSwitcher.setValue(event[cal.laneNameField]);
                laneSwitcher.setDisabled(this.isNewEvent ? false : !cal.canEditEventLane(event, view));
                var showSwitcher = view.isTimelineView() || (view.isDayView() && cal.showDayLanes);
                if (showSwitcher) laneSwitcher.show();
                else laneSwitcher.hide();
            }
            if (sublaneSwitcher) {
                sublaneSwitcher.setValueMap(cal.getSublaneMap(event[cal.laneNameField]));
                sublaneSwitcher.setValue(event[cal.sublaneNameField]);
                sublaneSwitcher.setDisabled(this.isNewEvent ? false : !cal.canEditEventSublane(event));
                var showSwitcher = cal.useSublanes &&
                        (view.isTimelineView() || (view.isDayView() && cal.showDayLanes));
                if (showSwitcher) sublaneSwitcher.show();
                else sublaneSwitcher.hide();
            }
            if (allowDurations) {
                var eventDuration = event[cal.durationField],
                    unit = event[cal.durationUnitField] || "minute"
                ;
                if (eventDuration != null) {
                    fDurationCB.setValue(cal.eventDurationFieldTitle);
                    fDuration.setValue(eventDuration);
                    fDuration.show();
                    fDurationUnit.setValue(unit);
                    fDurationUnit.show();
                    if (cal.getDateEditingStyle() == "time") {
                        if (form.getField("endHours")) form.hideField("endHours");
                        if (form.getField("endMinutes")) form.hideField("endMinutes");
                        if (form.getField("endAMPM")) form.hideField("endAMPM");
                    } else {
                        form.hideField(cal.endDateField);
                    }
                } else {
                    fDurationCB.setValue(cal.eventEndDateFieldTitle);
                    fDuration.hide();
                    fDurationUnit.hide();
                    var endDate = event[cal.endDateField];
                    if (cal.getDateEditingStyle() == "time") {
                        form.showField("endHours");
                        form.setValue("endHours", endDate.getHours());
                        form.showField("endMinutes");
                        form.setValue("endMinutes", endDate.getMinutes());
                    } else {
                        form.showField(cal.endDateField);
                        form.setValue(cal.endDateField, endDate);
                    }
                }
            }
            this.setDate(cal.getEventStartDate(event), cal.getEventEndDate(event));
            if (!event[cal.nameField]) {
                event[cal.nameField] = this.getDefaultItemValue(cal.nameField);
            }
            form.setValue(cal.nameField, event[cal.nameField]);
            if (!event[cal.descriptionField]) {
                event[cal.descriptionField] = this.getDefaultItemValue(cal.descriptionField);
            }
            form.setValue(cal.descriptionField, event[cal.descriptionField]);
            this.originalStart = this.currentStart.duplicate();
            this.originalEnd = this.currentEnd.duplicate();

            // show/hide the "Remove Event" button according to canRemoveEvent(event)
            if (!this.isNewEvent && cal.canRemoveEvent(event, view)) this.removeButton.show();
            else this.removeButton.hide();
        },

        getDefaultItemValue : function (itemName) {
            var form = this.items[0],
                item = form.getItem(itemName);
            return item && item.defaultValue;
        },

        hide : function () {
            this.Super('hide', arguments);
            this.calendar.clearViewSelection();
            // clear any errors
            this.calendar.eventEditor && this.calendar.eventEditor.hideItem("invalidDate");
        },

        sizeMe : function () {
            this.setWidth(this.calendar.mainView.getVisibleWidth());
            this.setHeight(this.calendar.mainView.getVisibleHeight());
            this.setLeft(this.calendar.mainView.getLeft());
        },

        draw : function () {
            var form = this.items && this.items[0];
            if (form && !form._fieldListSet) form.rebuildFieldList();
            this.Super("draw", arguments);
        },

        items: [this.eventEditor]
    }, this.eventEditorLayoutProperties));
    //this.eventEditorLayout.addItem(this.eventEditor);
    //this.eventEditorLayout.hide();

    return this.eventEditorLayout;
},

hideEventDialog : function () {
    if (this.eventDialog && this.eventDialog.isVisible()) this.eventDialog.hide();

},
displayEventDialog : function (reposition) {
    var dialog = this.eventDialog;
    if (reposition && dialog._lastRect) {
        dialog.placeNear(dialog._lastRect[0], dialog._lastRect[1]);
    }
    dialog.show();
},

addEventOrUpdateEventFields : function () {
    var cal = this,
        view = this.getSelectedView(),
        isNewEvent = cal.eventEditorLayout.isNewEvent,
        evt = cal.eventEditorLayout.event,
        form = cal.eventEditor,
        editStyle = cal.getDateEditingStyle(),
        values = form.getValues(),
        // lanes now apply to timelines (rows) and to dayView with showDayLanes: true (columns)
        useLanes = cal.isTimeline() || (cal.dayViewSelected() && cal.showDayLanes) && cal.canEditLane,
        laneName = useLanes ? values[cal.laneNameField] : null,
        sublaneName = useLanes && cal.useSublanes ? values[cal.sublaneNameField] : null,
        useDuration = values["endType"] == this.eventDurationFieldTitle,
        duration = useDuration ? values[this.durationField] || 1 : null,
        durationUnit = useDuration ? values[this.durationUnitField] ||
            (editStyle == "time" ? "minute" : "hour") : null
    ;

    var newEvent = isc.addProperties({}, evt, {eventLength: null});
    newEvent[this.nameField] = values[this.nameField];
    newEvent[this.descriptionField] = values[this.descriptionField];
    if (laneName) newEvent[this.laneNameField] = laneName;
    if (sublaneName) newEvent[this.sublaneNameField] = sublaneName;

    if (editStyle == "date" || editStyle == "datetime") {
        var start = values[this.startDateField],
            end = !useDuration ? values[this.endDateField] : null
        ;

        if (!useDuration && end < start) {
            form.showItem("invalidDate");
            return false;
        }

        // run validation so rules for custom fields added by the developer are enforced
        if (!form.validate()) return false;

        newEvent[cal.startDateField] = start;
        newEvent.isDuration = useDuration;
        if (useDuration) {
            newEvent[cal.durationField] = duration;
            newEvent[cal.durationUnitField] = durationUnit;
            delete newEvent[cal.endDateField];
        } else {
            newEvent[cal.endDateField] = end;
            delete newEvent[cal.durationField];
            delete newEvent[cal.durationUnitField];
        }

        cal.eventEditorLayout.currentStart = start;
        cal.eventEditorLayout.currentEnd = cal.getEventEndDate(newEvent);

        cal.eventEditorLayout.hide();

        cal._fromEventEditor = true;

    } else if (editStyle == "time") {
        var sAMPM = values["startAMPM"],
            sHrs = cal._to24HourNotation(values["startHours"], sAMPM),
            sMins = values["startMinutes"]
        ;

        var startDate = isc.DateUtil.createDatetime(cal.eventEditorLayout.logicalStartDate, null, null, sHrs, sMins, 0, 0);
        var startMillis = startDate.getTime(),
            maxEndDate = isc.DateUtil.getEndOf(startDate, "D", false)
        ;

        newEvent[cal.startDateField] = startDate;

        if (useDuration) {
            var maxEndMillis = maxEndDate.getTime(),
                millis = isc.DateUtil.convertPeriodUnit(duration, durationUnit, "ms"),
                endMillis = Math.min(startMillis + millis, maxEndMillis)
            ;
            if (endMillis != startMillis + millis) {
                // the specified duration exceeds the end of the day, so clamp it at the last
                // duration boundary
                duration = isc.DateUtil.convertPeriodUnit(endMillis - startMillis, "ms", durationUnit);
                duration = Math.round(duration);
            }
            newEvent[this.durationField] = duration;
            newEvent[this.durationUnitField] = durationUnit;
        } else {
            var eHrs = values["endHours"],
                eMins = values["endMinutes"],
                eAMPM
            ;

            if (!cal.twentyFourHourTime) {
                eAMPM = values["endAMPM"];
                eHrs = cal._to24HourNotation(eHrs, eAMPM);
            }
            // check for invalid times - bail if
            // - end hour < start hour and end time != 00:00
            // - end hours == start hours and end mins is <= start mins
            if ((eHrs < sHrs && eHrs+eMins != 0) || (eHrs == sHrs && eMins <= sMins)) {
                form.showItem("invalidDate");
                return false;
            }

            // if the end time is 00;00, make it 24:00 - it will get rounded back to 11:59
            if (eHrs == 0 && eMins == 0) eHrs = 24;

            // run validation so rules for custom fields added by the
            // developer are enforced
            if (!form.validate()) return false;

            var elDate = isc.DateUtil.getLogicalDateOnly(startDate);
            var endDate = isc.DateUtil.createDatetime(elDate, null, null, eHrs, eMins, 0, 0);
            if (endDate.getTime() > maxEndDate.getTime()) {
                //endDate = maxEndDate.duplicate();
            }

            // check for equal start and end times (specifically, midnight to midnight)
            if (isc.DateUtil.compareDates(startDate, endDate) == 0) {
                form.showItem("invalidDate");
                return false;
            }

            newEvent[cal.endDateField] = endDate;

            cal._fromEventEditor = true;

        }
    }

    if (view.isTimelineView()) {
        // update leading and trailing dates after an edit - there's no editing UI for these
        // dates
        if (evt[cal.leadingDateField] && evt[cal.startDateField] != newEvent[cal.startDateField]) {
            var leadingDate = evt[cal.leadingDateField].duplicate();
            // get difference in minutes
            var diff = isc.DateUtil.getPeriodLength(
                    evt[cal.startDateField], newEvent[cal.startDateField], "mn");
            leadingDate.setMinutes(leadingDate.getMinutes()+diff);
            newEvent[cal.leadingDateField] = leadingDate;
        }
        if (evt[cal.trailingDateField] && evt[cal.endDateField] != newEvent[cal.endDateField]) {
            var trailingDate = evt[cal.trailingDateField].duplicate();
            // get difference in minutes
            var diff = isc.DateUtil.getPeriodLength(
                    evt[cal.endDateField], newEvent[cal.endDateField], "mn");
            trailingDate.setMinutes(trailingDate.getMinutes()+diff);
            newEvent[cal.trailingDateField] = trailingDate;
        }
    }

    // get the custom values
    var customValues = isc.addProperties({}, form.getCustomValues());

    cal.eventEditorLayout.hide();

    if (!isNewEvent) {
        cal.updateCalendarEvent(evt, newEvent, customValues);
    } else {
        cal.addCalendarEvent(newEvent, customValues, false);
    }
    return true;
},

// sets the date label of the calendar. Called whenever the chosenDate or selected tab
// changes
setDateLabel : function () {
    var view = this.getSelectedView();
    if (!view || !this.dateLabel) return;
    var content = this.getDateLabelText(view.viewName,
            // pass the startDate before logical-date into Calendar.getDateLabelText() - the
            // default implementation of that method gets the logical dates anyway, and a dev
            // may want to override that behavior
            view.labelStartDate || view.startDate || view.logicalDate,
            view.isDayView() ? null : view.labelEndDate || view.logicalEndDate || view.endDate
    );
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
    var view = (viewName ? this.getView(viewName) : null) || this.getSelectedView();

    var result = view && view.getDateLabelText(
            startDate ? isc.DateUtil.getLogicalDateOnly(startDate) : null,
            endDate ? isc.DateUtil.getLogicalDateOnly(endDate) : null
    );
    return result || "";
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

//> @method calendar.cancelEditing()
// Cancels the current edit-session, closing the builtin event
// +link{calendar.eventDialog, dialog} or +link{calendar.eventEditor, editor} and clearing any
// visible edit-selection from the +link{calendar.getSelectedView, current CalendarView}.
//
// @visibility calendar
//<
cancelEditing : function () {
    var view = this.getSelectedView();
    if (view && view.clearSelection) view.clearSelection();
    if (this.eventDialog && this.eventDialog.isVisible()) {
        this.hideEventDialog();
    }
    if (this.eventEditor && this.eventEditor.isVisible()) {
        this.hideEventDialog();
    }
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
// <P>
// You can override this method to prevent the default action, perhaps instead showing a custom
// interface that performs validations or gathers custom data before making a call to
// +link{calendar.addCalendarEvent, addCalendarEvent} or
// +link{calendar.updateCalendarEvent, updateCalendarEvent} when the new data is available.
//
// @param [event] (CalendarEvent) the event to show in the Editor
// @param [isNewEvent] (Boolean) optional boolean indicating that this is a new event, event if
//                               an event is passed - used to pass defaults for a new event
// @visibility calendar
//<
showEventDialog : function (event, isNewEvent) {
    if (isNewEvent == null) isNewEvent = (event == null);
    this._showEventDialog(event, isNewEvent);
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
// <P>
// You can override this method to prevent the default action, perhaps instead showing a custom
// interface that performs validations or gathers custom data before making a call to
// +link{calendar.addCalendarEvent, addCalendarEvent} or
// +link{calendar.updateCalendarEvent, updateCalendarEvent} when the new data is available.
//
// @param [event] (CalendarEvent) defaults for the new event
// @visibility calendar
//<
showNewEventDialog : function (event) {
    event = event || {};
    this.showEventDialog(event, true);
},

// Displays the event entry/edit dialog at row/col position calculated from the start/endDates
// set on the passed event object
_showEventDialog : function (event, isNewEvent) {
    // clear out the stored eventWindow and event - set isNewEvent and create the fields
    var dialog = this.getEventDialog();
    dialog.eventWindow = null;
    dialog.event = null;
    dialog.isNewEvent = isNewEvent;
    dialog.items[0].createFields();
    // draw the dialog off-screen - measure it later
    dialog.moveTo(0, -9999);
    if (!dialog.isDrawn()) dialog.draw();
    else dialog.redraw();

    var view = this.getSelectedView();

    // get view-specific start/end dates, if they aren't specified on the event
    if (isNewEvent) event = view.getNewEventDefaults(event);

    var eventCanvas = view.getCurrentEventCanvas(event);

    var offset = {};
    if (eventCanvas) {
        // bring the canvas to the front
        if (this.bringEventsToFront) eventCanvas.bringToFront();
        // use canvas left and top, keeping inside body page-rect
        var rect = eventCanvas.getPageRect();
        offset.left = Math.max(rect[0], view.body.getPageLeft());
        offset.top = Math.max(rect[1], view.body.getPageTop());
    } else {
        offset = view.getEditDialogPosition(event);
    }

    dialog.setEvent(eventCanvas ? eventCanvas.event : event);

    if (this.eventEditorLayout) {
        this.eventEditorLayout.event = event;
        this.eventEditorLayout.isNewEvent = isNewEvent;
    }

    // ensure the dialog won't render off the bottom of the Calendar
    var thisHeight = this.getPageTop() + this.getHeight();
    // use the offset returned by the view
    var dHeight = dialog.getVisibleHeight();
    var dTop = offset.top;
    if (dTop + dHeight >= thisHeight) {
        dTop = thisHeight - (dHeight + 2);
    }

    dialog._lastRect = [offset.left, dTop];
    dialog.placeNear(dialog._lastRect[0], dialog._lastRect[1]);
    dialog.show();
    dialog.bringToFront();

    // show the UI disabled if the event can't be edited
    if (isNewEvent) {
        dialog.form.setDisabled(false);
    } else {
        dialog.form.setDisabled(!this.canEditEvent(event, view));
    }

    return;
},

visibilityChanged : function (isVisible) {
    if (!isVisible) this.hideEventDialog();
},

//> @method calendar.showEventEditor()
// Show an Event Editor for the passed event.  Event Editor's fill the Calendar and allow
// for editing of the built-in Event fields, like +link{nameField, name} and
// +link{descriptionField, description}, as well as any
// custom fields supplied via +link{calendar.eventEditorFields}.
// <P>
// If isNewEvent is true, a new event is created - in this case, if an event is passed, it
// represents default values to apply to the new event.
// <P>
// You can override this method to prevent the default action, perhaps instead showing a custom
// interface that performs validations or gathers custom data before making a call to
// +link{calendar.addCalendarEvent, addCalendarEvent} or
// +link{calendar.updateCalendarEvent, updateCalendarEvent} when the new data is available.
//
// @param [event] (CalendarEvent) an existing event to show in the Editor
// @param [isNewEvent] (Boolean) optional boolean indicating that this is a new event, even if
//                               an event is passed - used to pass defaults for a new event
// @visibility calendar
//<
showEventEditor : function (event, isNewEvent) {
    if (isNewEvent == null) isNewEvent = (event == null);
    this._showEventEditor(event, isNewEvent);
},

//> @method calendar.showNewEventEditor()
// Show an Event Editor for a new event.  If an +link{CalendarEvent, event} is passed as the
// parameter, it is used as defaults for the new event.
//
// @param [event] (CalendarEvent) defaults for the new event to show in the Editor
// @visibility calendar
//<
showNewEventEditor : function (event) {
    this.showEventEditor(event, true);
},

//> @attr calendar.newEventWindowTitle (HTMLString : "New Event" : IR)
// The title-text displayed in the popup event dialog/editor for new events.
// @group i18nMessages
// @visibility external
//<
newEventWindowTitle: "New Event",
_showEventEditor : function (event, isNewEvent) {
    var editorLayout = this.getEventEditorLayout();

    if (!editorLayout.isDrawn()) {

        editorLayout.setVisibility(isc.Canvas.INHERIT);
        editorLayout.draw();
    }
    editorLayout.setWidth(this.mainView.getVisibleWidth());
    editorLayout.setHeight(this.mainView.getVisibleHeight());
    // move the eventEditor to cover the mainView only

    editorLayout.setPageLeft(this.mainView.getPageLeft());
    editorLayout.setPageTop(this.getPageTop());

    editorLayout.isNewEvent = isNewEvent;

    editorLayout.view = this.getSelectedView();

    var editingStyle = this.getDateEditingStyle();
    if (this.eventEditor.dateEditingStyle != editingStyle) {
        // if dateEditingStyle is different, re-initialize the eventEditor
        // with appropriate edit items
        this.eventEditor.dateEditingStyle = editingStyle;
        this.eventEditor.rebuildFieldList();
    }

    //if (this.eventEditorFields) this.eventEditor.reset();
    if (event) {
        editorLayout.setEvent(event);
    } else {
        this.eventEditor.clearValues();
        editorLayout.setTitle(this.newEventWindowTitle);
        if (this.eventDialog && this.eventDialog.isVisible()) {
            // pass any custom field values through to the event editor
            if (this.eventEditorFields) {
                editorLayout.items[0].setCustomValues(this.eventDialog.items[0].getCustomValues());
            }
            var eventName = this.eventDialog.items[0].getValue(this.nameField);
            var laneItem = this.eventDialog.items[0].getItem(this.laneNameField);
            var lane = laneItem ? laneItem.getValue() : null;

            var startDate = new Date();

            editorLayout.setDate(
                startDate,
                this.eventDialog.currentEnd,
                eventName, lane
            );
        }
    }

    this.hideEventDialog();

    var canEdit = this.canEditEvent(event, this.getSelectedView());
    // show the UI disabled if the event can't be edited
    this.eventEditor.setDisabled(!canEdit);
    editorLayout.saveButton.setDisabled(!canEdit);

    editorLayout.show();
},

_getEventDialogTitle : function (startDate, endDate) {
//    var result = isc.DateUtil.getFormattedDateRangeString(startDate, endDate);
//    var oldResult = this._old_getEventDialogTitle(startDate, endDate);
//    this.logWarn("NEW:  " + result + "\nOLD:  " + oldResult);
//    return result;
//},
//
//_old_getEventDialogTitle : function (startDate, endDate) {
    var days   = isc.DateUtil.getShortDayNames(),
        months = isc.DateUtil.getShortMonthNames(),
        sTime = isc.Time.toShortTime(startDate, this.timeFormatter),
        eTime = isc.Time.toShortTime(endDate, this.timeFormatter),
        result
    ;


    if (this.isTimeline()) {
        var differentDays = (isc.DateUtil.compareLogicalDates(startDate, endDate) != 0);

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
    if (ampmString == null) return hour;
    else if (ampmString.toLowerCase() == "am" && hour == 12) {
        return 0;
    } else if (ampmString.toLowerCase() == "pm" && hour < 12) {
        return hour + 12;
    } else {
        return hour;
    }
},

_getCellCSSText : function (grid, record, rowNum, colNum) {
    var currDate = this.getCellDate(rowNum, colNum, grid);
    // not a date cell
    if (!currDate) return null;

    var result = this.getDateCSSText ? this.getDateCSSText(currDate, rowNum, colNum, grid) : null;
    // an override of getDateCSSText() returned something - return that
    if (result) return result;

    if (this.todayBackgroundColor) {
        // if todayBackgroundColor is set and the passed logical date is today,
        // return CSS for that...
        var today = isc.DateUtil._getDisplayOffsetDate(new Date());
        //this.logWarn(new Date() + "\n" + today);
        var dateComp = isc.DateUtil.compareLogicalDates(currDate, today);
        if ((dateComp !== false && dateComp == 0)) {
            return "background-color:" + this.todayBackgroundColor + ";";
        }
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
// @see getDateHTML()
// @see getDateStyle()
//
// @param date (Date) the date to return CSS text for
// @param rowNum (Integer) the row number containing the date to get the CSS for
// @param colNum (Integer) the column number containing the date to get the CSS for
// @param view (CalendarView) the relevant CalendarView
// @return (String) CSS text for the cell with the passed date and rowNum/colNum
//
// @visibility calendar
//<
//getDateCSSText : function (date, rowNum, colNum, view) {
//    return null;
//},

//> @method calendar.getDateStyle()
// Return the CSS styleName for the associated date-cell in the passed view.
//
// @see getDateHTML()
// @see getDateCSSText()
//
// @param date (Date) the date to return the CSS styleName for
// @param rowNum (Integer) the row number containing the date to get the CSS styleName for
// @param colNum (Integer) the column number containing the date to get the CSS styleName for
// @param view (CalendarView) the relevant CalendarView
// @return (CSSStyleName) CSS style for the cell with the passed date and rowNum/colNum
//
// @visibility calendar
//<
//getDateStyle : function (date, rowNum, colNum, view) {
//    return null;
//},

//> @method calendar.getDateHTML()
// Return the HTML to be displayed in the associated date-cell in the passed view.
//
// Note that the +link{calendar.monthView, month view} has default cell HTML, controlled via
// +link{calendar.getDayBodyHTML, getDayBodyHTML()}.
//
// @see getDateCellAlign()
// @see getDateCellVAlign()
// @see getDateStyle()
// @see getDateCSSText()
// @see getDayBodyHTML()
//
// @param date (Date) the date to get the HTML for
// @param rowNum (Integer) the row number containing the date to get the HTML for
// @param colNum (Integer) the column number containing the date to get the HTML for
// @param view (CalendarView) the relevant CalendarView
// @return (HTMLString) HTML to display in the cell with the passed date and rowNum/colNum
//
// @visibility calendar
//<

//> @method calendar.getDateCellAlign()
// When +link{calendar.getDateHTML, getDateHTML} returns a value, this method returns the
// horizontal alignment for that value in its cell, in the passed view.
//
// @see getDateHTML()
// @see getDateCellVAlign()
// @see getDateStyle()
// @see getDateCSSText()
//
// @param date (Date) the date to get the cell-alignment for
// @param rowNum (Integer) the row number containing the date to get the cell-alignment for
// @param colNum (Integer) the column number containing the date to get the cell-alignment for
// @param view (CalendarView) the relevant CalendarView
// @return (HTMLString) cell-alignment for content in the cell with the passed date and rowNum/colNum
//
// @visibility calendar
//<

//> @method calendar.getDateCellVAlign()
// When +link{calendar.getDateHTML, getDateHTML} returns a value, this method returns the
// vertical alignment for that value in its cell, in the passed view.
//
// @see getDateHTML()
// @see getDateCellAlign()
// @see getDateStyle()
// @see getDateCSSText()
//
// @param date (Date) the date to get the cell-alignment for
// @param rowNum (Integer) the row number containing the date to get the cell-alignment for
// @param colNum (Integer) the column number containing the date to get the cell-alignment for
// @param view (CalendarView) the relevant CalendarView
// @return (HTMLString) vertical-alignment for content in the cell with the passed date and rowNum/colNum
//
// @visibility calendar
//<
//> @method calendar.getDateHeaderTitle()
// Return the title text to display in the header-button of the ListGridField showing the
// passed date, in the passed view.
//
// @param date (Date) the date to return the header-title for - note that the
//                    +link{calendar.monthView, month view} does not pass this parameter
//                    because a single column represents multiple dates
// @param dayOfWeek (int) the week-day number of the passed date, except for the
//                         +link{calendar.monthView, month view}, where no date is passed,
//                         because the week-day number represents multiple dates.
// @param defaultValue (String) the default header-title for the passed date and view
// @param view (CalendarView) the relevant CalendarView
// @return (String) the text to show in the header-button for the passed date/field
//
// @visibility calendar
//<
getDateHeaderTitle : function (date, dayOfWeek, defaultValue, view) {
    return null;
},

//> @method calendar.getCellDate()
// Return the Date instance associated with the passed co-ordinates in the passed or selected
// view.  If the cell at the passed co-ordinates is not a date-cell, returns null.  If rowNum
// and colNum are both unset, returns the date from the cell under the mouse.
// <P>
// To determine the date at a more specific point within a cell, see +link{getDateFromPoint}.
//
// @param [rowNum] (Integer) the row number to get the date for
// @param [colNum] (Integer) the column number to get the date for
// @param [view] (CalendarView) the view to use - uses the selected view if not passed
// @return (Date) the date, if any, associated with the passed co-ords in the appropriate view
//
// @visibility calendar
//<
getCellDate : function (rowNum, colNum, view) {
    view = view || this.getSelectedView();

    var retDate;

    if (rowNum == null && colNum == null) {
        // no co-ords, use the cell under the mouse
        rowNum = view.getEventRow();
        colNum = view.getEventCol();
    }

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
// @param [view] (CalendarView) the view to use - or the selected view if not passed
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

//> @method calendar.getLane()
// Returns the +link{Lane, lane} with the passed name, in the passed view
// @param lane (String) the name of the lane to return
// @param [view] (CalendarView) the view to get the lane object from
// @return (Lane) the lane with the passed name, or null if not found
// @visibility external
//<
getLane : function (lane, view) {
    if (!lane) return null;
    view = view || this.getSelectedView();
    if (view.getLane) return view.getLane(lane);
    return null;
},

//> @method calendar.getEventLane()
// Returns the +link{Lane, lane} associated with the passed event, in the passed view
// @param event (CalendarEvent) the event to get the lane for
// @param [view] (CalendarView) the view to get the lane object from
// @return (Lane) the lane associated with the passed event
// @visibility external
//<
getEventLane : function (event, view) {
    if (!event) return null;
    return this.getLane(event[this.laneNameField], view);
},

//> @method calendar.getSublane()
// Returns the +link{Lane.sublanes, sublane} with the passed name, from the +link{Lane, lane}
// with the passed name, in the passed view.
// @param lane (String) the name of the lane containing the sublane to return
// @param sublane (String) the name of the sublane to return
// @param [view] (CalendarView) the view to get the sublane object from
// @return (Lane) the sublane with the passed name, or null if not found
// @visibility external
//<
getSublane : function (lane, sublane, view) {
    if (!lane) return null;
    view = view || this.getSelectedView();
    if (view.getSublane) return view.getSublane(lane, sublane);
    return null;
},

//> @method calendar.getEventSublane()
// Returns the +link{lane.sublanes, sublane} associated with the passed event, in the passed view
// @param event (CalendarEvent) the event to get the sublane for
// @param [view] (CalendarView) the view to get the sublane object from
// @return (Lane) the sublane associated with the passed event
// @visibility external
//<
getEventSublane : function (event, view) {
    if (!event) return null;
    return this.getSublane(event[this.laneNameField], event[this.sublaneNameField], view);
},

//> @method calendar.getLaneFromPoint()
// Returns the +link{Lane} at the passed co-ordinates.  To get the lane under the mouse, pass
// null for both x and y.
// @param [x] (Integer) the x offset into the body of the selected view
// @param [y] (Integer) the y offset into the body of the selected view. If this param and "x" are
//                            both unset, assumes both offsets from the last mouse event.
// @param [view] (CalendarView) the view to get the lane from - selected view if not passed
// @return (Lane) the Lane at the passed co-ords in the passed or selected view
//
// @visibility external
//<
getLaneFromPoint : function (x, y, view) {
    view = view || this.getSelectedView();
    if (!view.usesLanes()) return null;
    if (view.getLaneFromPoint) return view.getLaneFromPoint(x, y);
    return null;
},

//> @method calendar.getSublaneFromPoint()
// Returns the +link{Lane.sublanes, sublane} at the passed co-ordinates.  To get the sublane under
// the mouse, pass null for both x and y.
// @param [x] (Integer) optional x offset into the body of the selected view
// @param [y] (Integer) optional y offset into the body of the selected view. If this param and "x" are
//                            both unset, assumes both offsets from the last mouse event.
// @param [view] (CalendarView) the view to get the sublane from - selected view if not passed
// @return (Lane) the sublane at the passed co-ords in the selected view
//
// @visibility external
//<
getSublaneFromPoint : function (x, y, view) {
    view = view || this.getSelectedView();
    if (view.getSublaneFromPoint) return view.getSublaneFromPoint(x, y);
    return null;
},

getDateLeftOffset : function (date, view) {
    if (view && view.getDateLeftOffset) return view.getDateLeftOffset(date);
},


//> @method calendar.currentViewChanged()
// Notification that fires whenever the current view changes via the
// +link{mainView, mainView tabset}.
//
// @param viewName (ViewName) the name of the current view after the change
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
// @return (HTMLString) HTML to display
//
// @group monthViewFormatting
// @visibility calendar
//<
getDayBodyHTML : function (date, events, calendar, rowNum, colNum) {

    // bail if there's no date or events to display
    if (!date || !events || events.length == 0) return "";

    var view = calendar.monthView,
        day = date.getDay(),
        record = view.getRecord(rowNum),
        // available cell-height is row-height - cell-padding
        rHeight = view.getRowHeight(record, rowNum) - (view.cellPadding*2),
        content = "",
        // the index at which the "+ N more..." link is added - remaining events appear in the
        // monthMoreEventsMenu
        moreItemIndex = null,
        html = ""
    ;

    // figure out how many events can be displayed in the record before the "+ N more..." link
    // needs adding
    for (var i = 0; i < events.length; i++) {
        if (i > 0) content += "<br>";
        content += "<nobr>" + events[i].name + "</nobr>";
        var height = isc.Canvas.measureContent(content, this.dayBodyBaseStyle, true, true);
        if (height >= rHeight) {
            moreItemIndex = i - 1;
            break;
        }
    }

    if (moreItemIndex == null) moreItemIndex = events.length;

    for (var i = 0; i < moreItemIndex; i++) {
        var title = this.getEventHeaderHTML(events[i], view);
        if (!this.isPrinting) {
            // clicking these (active) links fires the Canvas.eventClick() notification - note
            // that links for disabled events are still active, but clicking them shows a
            // disabled eventEditor
            var template  = "<a href='javascript:" + this.ID + ".monthViewEventClick(" +
                    rowNum + "," + colNum + "," + i + ");' class='"
                    + this.calMonthEventLinkStyle + "'>"
            ;

            html += template + title + "</a><br/>";
        } else {
            html += title + "<br/>";
        }
    }
    if (moreItemIndex != events.length && !this.isPrinting) {
        // show a link that opens the monthMoreEventsMenu
        var moreLink = "<a href='javascript:" + this.ID + ".monthMoreEventsLinkClick(" +
                rowNum + "," + colNum + "," + moreItemIndex + ");' class='"
                + this.calMonthEventLinkStyle + "'>",
            str = this.monthMoreEventsLinkTitle,
            title = str.evalDynamicString(this, { eventCount: (events.length - i) })
        ;
        html += moreLink + title + "</a><br/>";
    }
    return html;
},

monthViewEventClick : function (rowNum, colNum, eventIndex) {
    if (this.monthView.isDisabled()) return;
    var events = this.monthView.getEvents(rowNum, colNum);
    var evt = events[eventIndex];
    if (this.eventClick(evt, "month")) this.showEventEditor(evt);
},

//> @attr calendar.monthMoreEventsMenu (AutoChild Menu : null : R)
// AutoChild Menu, shown when a user clicks the
// +link{calendar.monthMoreEventsLinkTitle, more events} link in a cell of the
// +link{calendar.monthView, monthView}.  Items in this menu represent additional events,
// not already displayed in the cell, and clicking them fires the
// +link{calendar.eventClick, eventClick} notification.
// @visibility external
//<
monthMoreEventsMenuConstructor: "Menu",
monthMoreEventsMenuDefaults: {
    autoDraw: false,
    visibility: "hidden",
    keepInParentRect: true
},

_getMonthMoreEventsMenu : function () {
    if (!this.monthMoreEventsMenu) {
        this.monthMoreEventsMenu = this.createAutoChild("monthMoreEventsMenu");
    }
    return this.monthMoreEventsMenu;
},

monthMoreEventsLinkClick : function (rowNum, colNum, startIndex) {
    var cal = this,
        view = this.monthView,
        events = view && view.getEvents(rowNum, colNum) || [],
        items = []
    ;
    for (var i=startIndex; i<events.length; i++) {
        var event = events[i];
        items.add({
            title: event[this.nameField],
            // all menuItems should be enabled - clicks will show the event in a disabled editor
            //enabled: this.canEditEvent(event, view),
            event: event,
            calendar: cal,
            click : function () {
                if (this.calendar.eventClick(this.event, "month")) {
                    this.calendar.showEventEditor(this.event);
                }
            }
        });
    }
    var menu = this._getMonthMoreEventsMenu();
    menu.setItems(items);
    menu.positionContextMenu();
    menu.show();
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
// @return (HTMLString) HTML string to display
//
// @visibility calendar
//<
getMonthViewHoverHTML : function(currDate, events) {
    if(events!=null) {
        var retVal = "";
        var target = this.creator || this;
        for (var i = 0; i < events.length; i++) {
            var eTime = isc.Time.toShortTime(target.getEventStartDate(events[i]), target.timeFormatter, true);
            retVal += "<nobr>" + eTime + " " + events[i][target.nameField] + "<nobr/><br/>";
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
// @return (HTMLString) HTML to show in the header of a day in the month view
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
// Notification fired whenever a user removes an event.
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
// been successfully added at the server
//
// @param event (CalendarEvent) the event that was added
// @visibility calendar
//<

//> @method calendar.eventClick()
// Called whenever an event is clicked on in the day, week or month views.
// <P>
// By default, a dialog appears showing details for the event, and offering the ability to
// edit events that can be edited.  Return false to cancel the default action. This is a good
// place to, for example, show a completely customized event dialog instead of the default one.
//
// @param event (CalendarEvent) event that was clicked on
// @param viewName (ViewName) view where the event's canvas was clicked
// @return (Boolean) false to cancel the default action
//
// @visibility calendar
//<
eventClick : function (event, viewName) {
    return true;
},

_eventCanvasClick : function (canvas, defaultOnly) {
    var event = canvas.event,
        view = canvas.calendarView,
        isWeekView = view.isWeekView(),
        doDefault = (defaultOnly ? true : this.eventClick(event, view.viewName)) != false
    ;

    // defaultOnly is passed by _eventCanvasDoubleClick
    if (!defaultOnly) {
        if (this.canSelectEvents) {
            if (isc.EH.modifierKeyDown()) {
                if (!this.getSelection().isSelected(event)) {
                    // CTRL down and event not already selected - select it
                    this.selectEvent(event, canvas);
                } else {
                    // CTRL down and event already selected - deselect it
                    this.deselectEvent(event, canvas);
                }
            } else {
                // CTRL not down, performs a selectSingle, clearing other selected events
                this.selectSingleEvent(event, canvas);
            }
            return;
        } else if (this.bringEventsToFront) {
            // bring the event to the front of the zorder
            canvas.bringToFront();
        }
    }

    if (doDefault) {
        // handle the case when a selection is made, then an event is clicked
        this.clearViewSelection();
        if (!view.isTimelineView()) {
            var eventStart = this.getEventStartDate(event);
            var offset = (view.frozenFields ? view.frozenFields.length : 0);
            var col = isWeekView ? eventStart.getDay() - this.firstDayOfWeek + offset : offset;
            // account for no weekends shown
            if (isWeekView && this.showWeekends == false) col--;
            var row = eventStart.getHours() * this.getRowsPerHour(view);
        }

        if (!this.canEditEvent(event, view)) {
            // show the editor in a disabled mode
            this.showEventEditor(event);
        } else {
            // show the dialog in editable mode
            this.showEventDialog(event);
        }
        return isc.EH.STOP_BUBBLING;
    }
},


_eventCanvasKeyPress : function (canvas, key) {
    var view = canvas.calendarView,
        cache = canvas._cacheValues
    ;
    if (key) {
        if (key == "Enter") {
            // Enter opens the eventDialog (or eventEditor) for this event
            if (cache.canEdit) this._eventCanvasClick(canvas, true);
        } else if (key == "Backspace" || key == "Delete") {
            // Delete removes this event from the calendar (close button)
            if (cache.showCloseButton) {
                var selected = this.getSelectedEvents();
                for (var i=0; i<selected.length; i++) {
                    var eCanvas = view.getCurrentEventCanvas(selected[i])
                    this._eventCanvasCloseClick(eCanvas);
                }
            }
        } else if (key == "Tab") {
            this._tabPressed = true;
        }
    }
},

eventDoubleClick : function (event, viewName) {
    return true;
},
_eventCanvasDoubleClick : function (canvas) {
    var event = canvas.event,
        view = canvas.calendarView,
        isWeekView = view.isWeekView(),
        doDefault = this.eventDoubleClick(event, view.viewName) != false
    ;

    // bring the event to the front of the zorder
    if (this.canSelectEvents) {
        // call the calendar-level handler, passing the second param, defaultOnly:true - that
        // will skip calling bringToFront() again and firing the eventClick() notification, but
        // still perform it's default behavior of showing the editor or dialog for the event
        this._eventCanvasClick(canvas, true)
    } else if (this.bringEventsToFront) {
        canvas.bringToFront();
    }
},

//> @method calendar.eventRemoveClick()
// Called whenever the close icon of an +link{EventCanvas, event canvas} is clicked in the
// +link{dayView, day}, +link{weekView, week} and +link{timelineView, timeline} views, or when
// the +link{removeButton, remove button} is pressed in the +link{eventEditor, event editor}.
// <P>
// Implement this method to intercept the automatic removal of data.  You can return false to
// prevent the default action (calling +link{calendar.removeEvent, removeEvent()}) and instead
// take action of your own.  For example, returning false from this method and then showing a
// custom confirmation dialog - if the user cancels, do nothing, otherwise
// make a call to +link{calendar.removeEvent, removeEvent(event)}, passing the event.
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
// @param newLane (String) the name of the lane into which the event was moved
// @return (boolean) return false to disallow the move.
//
// @group monthViewEvents
// @visibility calendar
// @deprecated in favor of +link{calendar.eventRepositionStop}
//<
eventMoved : function (newDate, event, newLane) {
    return true;
},

//> @method calendar.eventResized()
// Called when an event is resized with the mouse.  The passed date value is the new
// *end* date for the event, since resizing can only be performed on the bottom edge of an event
// in normal calendar views.
// @param newDate (Date) new end date and time that event is being resized to
// @param event (CalendarEvent) the event as it will be after this resize
// @return (boolean) return false to disallow the resize
//
// @group monthViewEvents
// @visibility calendar
// @deprecated in favor of +link{calendar.eventResizeStop}
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
// @return (Boolean) return false to disallow the move.
//
// @visibility calendar
// @deprecated in favor of +link{calendar.eventRepositionStop}
//<
timelineEventMoved : function (event, startDate, endDate, lane) {
    return true;
},

//> @method calendar.timelineEventResized()
// Called when a Timeline event is resized via dragging by a user.  Return false to disallow
// the resize.
// @param event (CalendarEvent) the event that was resized
// @param startDate (Date) new start date of the passed event, after the resize
// @param endDate (Date) new end date of the passed event, after the resize
// @return (Boolean) return false to disallow the resize
//
// @visibility calendar
// @deprecated in favor of +link{calendar.eventResizeStop}
//<
timelineEventResized : function (event, startDate, endDate) {
    return true;
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
        // this should not be necessary, and is quite costly
        //this.refreshSelectedView();
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
     if (this.eventEditorLayout && this.eventEditorLayout.isVisible()) this.eventEditorLayout.sizeMe();
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
// @return (Integer) the day that the mouse is currently over
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
// @param [end] (Date) end of range
// @visibility external
//<
setTimelineRange : function (start, end, callback) {
    var view = this.timelineView;

    // if there's more than one headerLevel, headerSpans need updating - needs a full rebuild
    var needsRebuild = this.headerLevels && this.headerLevels.length > 1;
    // if hidden dates are allowed, needs a refreshFields()
    var needsRefreshFields = !this.showWeekends || this.shouldShowDate != null;

    start = isc.DateUtil.getStartOf(start, this.timelineGranularity, false, this.firstDayOfWeek);
    if (!end) {
        // if no endDate passed, calculate one based on the current visiblt view range
        var length = Math.round(isc.DateUtil.getPeriodLength(view.startDate, view.endDate,
                this.timelineGranularity));
        end = isc.DateUtil.dateAdd(start, this.timelineGranularity, length, 1,
            null, null, this.firstDayOfWeek);
    }

    // cols range from the rounded start of the view to the passed or calculated end
    var colsRequired = Math.round(isc.DateUtil.getPeriodLength(start, end,
            this.timelineGranularity) / this.timelineUnitsPerColumn);

    if (colsRequired != this.timelineColumnSpan) {
        this.timelineColumnSpan = colsRequired;
        if (this.timelineView) this.timelineView.setTimelineRange(start, end);
    } else {
        view.setChosenDate(start);
    }
    if (callback) this.fireCallback(callback);
},

//> @method calendar.setResolution()
// Reset the resolution, the header levels and scrollable range, of the timeline view.
// <P>
// <code>headerLevels</code> specifies the array of +link{HeaderLevel, headers} to show above
// the timeline, and the <code>unit</code> and <code>unitCount</code> parameters dictate the
// scrollable range (eg, passing "week" and 6 will create a timeline with a scrollable range of
// six weeks, irrespective of the number of columns that requires, according to the
// +link{timelineGranularity, granularity}).
// <P>
// If the optional <code>granularityPerColumn</code> parameter is passed, each column will span
// that number of units of the granularity, which is determined from the unit of the innermost
// of the passed headerLevels.  For example, to show a span of 12 hours with inner columns that
// each span 15 minutes, you could pass "hour" and "minute" -based headerLevels, unit and
// unitCount values of "hour" and 12 respectively, and granularityPerColumn of 15.
//
// @param headerLevels (Array of HeaderLevel) the header levels to show in the timeline
// @param unit (TimeUnit) the time unit to use when calculating the range of the timeline
// @param unitCount (Integer) the count of the passed unit that the timeline should span
// @param [granularityPerColumn] (Integer) how many units of the granularity (the unit of the
//           innermost headerLevel) should each column span?  The default is 1.
// @visibility external
//<
setResolution : function (headerLevels, unit, unitCount, granularityPerColumn, callback) {
    // revert to one granularity per column if not passed
    this.timelineUnitsPerColumn = granularityPerColumn || 1;
    if (unit) this.timelineGranularity = unit;
    if (headerLevels) {
        // deal with being passed a single headerLevel object, not in an array
        if (!isc.isAn.Array(headerLevels)) headerLevels = [headerLevels];
        this.headerLevels = headerLevels;
    }
    if (headerLevels.length > 0) {
        // update the timelineGranularity to the unit of the innermost headerLevel, so
        // that calculating column-dates works correctly
        this.timelineGranularity = headerLevels[headerLevels.length-1].unit;
    }

    // calculate the endDate by adding (unit*unitCount) to the startDate
    var view = this.timelineView;
    var unitKey = unit;
    var endDate = isc.DateUtil.dateAdd(this.startDate, unitKey, unitCount, 1,
            null, null, this.firstDayOfWeek);
    var colsRequired = Math.round(isc.DateUtil.getPeriodLength(this.startDate, endDate,
            this.timelineGranularity) / this.timelineUnitsPerColumn);
    if (colsRequired != this.timelineColumnSpan) {
        this.timelineColumnSpan = colsRequired;
    }
    // rebuild the timeline
    if (view) view.setChosenDate(this.startDate);
    if (callback) this.fireCallback(callback);
},

//> @method calendar.getEventLength()
// Returns the length of the passed +link{CalendarEvent, event} in the passed
// +link{TimeUnit, unit}.  If <code>unit</code> isn't passed, returns the length of the event
// in milliseconds.
//
// @param event (CalendarEvent) the event to get the length of
// @param [unit] (TimeUnit) the time unit to return the length in, milliseconds if not passed
// @visibility external
//<
// get event length in milliseconds - pass in a timeUnit (like "m" or "d") for other resolutions
getEventLength : function (event, unit) {
    // get the length stored on the event during refreshEvents()
    var length = event.eventLength,
        util = isc.DateUtil
    ;
    if (length == null) {
        // eventLength isn't present - calculate it and store it
        length = util.getPeriodLength(this.getEventStartDate(event), this.getEventEndDate(event));
        event.eventLength = length;
    }
    if (unit) {
        return util.convertPeriodUnit(event.eventLength, "ms", unit);
    }
    return event.eventLength;
},

canEditEventLane : function (event, view) {
    view = view || this.getSelectedView();
    if (!view.usesLanes()) return false;
    var canEdit = event[this.canEditLaneField] != null ?
            event[this.canEditLaneField] : this.canEditLane != false;
    return canEdit;
},

canEditEventSublane : function (event, view) {
    if (!this.useSublanes) return false;
    var canEdit = event[this.canEditSublaneField];
    if (canEdit == null) canEdit = (this.canEditSublane != false);
    return canEdit;
},


//eventUseLastValidDropDates: null,

//> @method calendar.eventRepositionMove()
// Notification called whenever the drop position of an event being drag-moved changes.
// <P>
// The <code>newEvent</code> parameter represents the event as it will be after the move,
// including +link{calendarEvent.startDate, start} and +link{calendarEvent.endDate, end} dates
// and +link{calendarEvent.lane, lane} and +link{calendarEvent.sublane, sublane} where
// applicable.
// <P>
// Return false to prevent the default action, of positioning the drag canvas to the newEvent.
//
// @param event (CalendarEvent) the event that's being moved
// @param newEvent (CalendarEvent) the event as it would be if dropped now
// @return (Boolean) return false to cancel the default drag move behavior
// @visibility external
//<
eventRepositionMove : function (event, newEvent, view) {
    var startDate = this.getEventStartDate(newEvent),
        endDate = this.getEventEndDate(newEvent)
    ;
    if (this.shouldDisableDate(startDate, view) || this.shouldDisableDate(endDate, view)) {
        // shouldDisableDate deals with disableWeekends, and might have been overridden
        // to add custom support
        return false;
    }
    return true;
},

//> @method calendar.eventRepositionStop()
// Notification called when an event being drag-moved is dropped.
// <P>
// The <code>newEvent</code> parameter represents the event as it will be after the move,
// including +link{calendarEvent.startDate, start} and +link{calendarEvent.endDate, end} dates
// and +link{calendarEvent.lane, lane} and +link{calendarEvent.sublane, sublane} where
// applicable.
// <P>
// Return false to prevent the default action, of actually
// +link{calendar.updateCalendarEvent, updating} the event.
//
// @param event (CalendarEvent) the event that's about to be moved
// @param newEvent (CalendarEvent) the event as it will be, unless this method returns false
// @param [customValues] (Object) additional custom values associated with the event
// @return (Boolean) return false to cancel the default drop behavior
// @visibility external
//<
eventRepositionStop : function (event, newEvent, customValues, view) {
    return true;
},

//> @method calendar.eventResizeMove()
// Notification called on each resize during an event drag-resize operation.
// <P>
// The <code>newEvent</code> parameter represents the event as it will be after the resize.
// <P>
// Return false to prevent the default action, of resizing the drag canvas to the newEvent.
//
// @param event (CalendarEvent) the event that's being drag-resized
// @param newEvent (CalendarEvent) the event as it would be if dropped now
// @return (Boolean) return false to cancel the default drag resize behavior
// @visibility external
//<
eventResizeMove : function (event, newEvent, view, props) {
    var startDate = this.getEventStartDate(newEvent),
        endDate = this.getEventEndDate(newEvent)
    ;

    if (startDate.getTime() == endDate.getTime()) return false;


    endDate.setTime(endDate.getTime()-1);

    // only disallow resize if the date at the edge being dragged is disabled, not
    // if either of them is disabled
    if (((props._leftDrag || props._topDrag) && this.shouldDisableDate(startDate, view)) ||
        ((props._rightDrag || props._bottomDrag) && this.shouldDisableDate(endDate, view))) {
        // the new dragDate (start/end) is disabled (eg, its a weekend) - just disallow
        return false;
    }
    return true;
},

//> @method calendar.eventResizeStop()
// Notification called when an event drag-resize operation completes.
// <P>
// The <code>newEvent</code> parameter represents the event as it will be after the move.
// <P>
// Return false to prevent the default action, of actually
// +link{calendar.updateCalendarEvent, updating} the event.
//
// @param event (CalendarEvent) the event that's about to be resized
// @param newEvent (CalendarEvent) the event as it will be, unless this method returns false
// @param [customValues] (Object) additional custom values associated with the event
// @return (Boolean) return false to cancel the default drag-resize stop behavior
// @visibility external
//<
eventResizeStop : function (event, newEvent, customValues, view) {
    return true;
},


checkForOverlap : function (view, eventCanvas, event, startDate, endDate, lane) {
    var overlapTest = {},
        startField = this.startDateField,
        endField = this.endDateField
    ;

    overlapTest[startField] = startDate.duplicate();
    overlapTest[endField] = endDate.duplicate();
    overlapTest[this.laneNameField] = lane;

    var events = this.data;
    if (lane) {
        events = this.getLaneEvents(lane, view);
    }

    var overlappingEvents = view.findOverlappingEvents(event, overlapTest, null, (lane != null), events);
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
            startDate.setMinutes(startDate.getMinutes() - this.getEventLength(event, "minute"));
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
            endDate.setMinutes(endDate.getMinutes() + this.getEventLength(event, "minute"));
            //isc.logWarn('right overlap:' + [overlapped.id, overlapped.end, startDate, endDate]);
            return [startDate, endDate];
        // other cases: for now don't allow drops where drop event completely encompasses
        // or is encompassed by another event
        } else {
            return true;
        }

    }
}

,

// customize printHTML - we want to write out the controlsBar and the current view, not the
// tabSet and it's container, since the tab-buttons will be removed, but the styling still includes
// offsets which complicate eventCanvas-placement
getPrintHTML : function (printProperties, callback) {
    // PrintCanvas.getPrintFrameURL() will use this to pass a URL param to the printFrame.
    isc._printWithDensityThisTime = true;
    if (callback) {
        this.delayCall("asyncGetPrintHTML", [printProperties, callback]);
        return null;
    } else {
        return this.asyncGetPrintHTML(printProperties, callback);
    }
},
asyncGetPrintHTML : function (printProperties, callback) {
    var html = "";
    // include the controlsBar if it's visible
    var handle = this.controlsBar && this.controlsBar.getHandle();
    if (handle) html += handle.innerHTML; //getPrintHTML();

    var view = this.getSelectedView();
    html += view.getPrintHTML();
    //html += view.body.getHandle().innerHTML;
    if (isc.isA.Function(callback)) callback(html);
    if (callback && isc.isA.Function(callback.callback)) callback.callback(html);
    return html;
}

});

isc.Calendar.addClassMethods({


_getAsDisplayDate : function (date) {
    if (!isc.Time._customTimezone) {
        return new Date(date.getFullYear(), date.getMonth(), date.getDate());
    }
    var offsetDate = date._getTimezoneOffsetDate(
        isc.Time.getUTCHoursDisplayOffset(date),
        isc.Time.getUTCMinutesDisplayOffset(date)
    );
    return new Date(offsetDate.getUTCFullYear(), offsetDate.getUTCMonth(),
                    offsetDate.getUTCDate());
}

});

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






//> @class EventCanvas
// The EventCanvas component is a lightweight +link{class:VLayout, layout} subclass for
// displaying a +link{CalendarEvent} in a +link{CalendarView}.
// <P>
// Each instance can be +link{calendarEvent.styleName, styled}, and can render a single area,
// or separate +link{eventCanvas.showHeader, header} and +link{eventCanvas.showBody, body}
// areas, for the look of a Window.
// <P>
// The component's close and context buttons, and any necessary resizers, are
// shown on +link{eventCanvas.showRolloverControls, rollover}.
//
// @inheritsFrom VLayout
// @treeLocation  Client Reference/Calendar
// @visibility external
//<
isc.defineClass("EventCanvas", "VLayout");



isc.EventCanvas.addClassProperties({

    headerSizer: null,
    getHeaderHeight : function (text, width, height, wrap, canvas) {
        var ec = isc.EventCanvas;
        if (!ec.headerSizer) {
            ec.headerSizer = isc.Canvas.create({
                ID: "_headerSizer",
                autoDraw: false,
                visibility: "hidden",
                //backgroundColor: "red",
                left: 0,
                top: -1000
            });
        }
        ec.headerSizer.setProperties({
            height: wrap ? 1 : height,
            maxHeight: wrap ? null : height,
            width: width,
            maxWidth: width,
            overflow: wrap ? "visible" : "hidden",
            contents: text,
            styleName: canvas.getHeaderStyle()
        });

        if (!ec.headerSizer.isDrawn()) ec.headerSizer.draw();
        else ec.headerSizer.redraw();

        var newHeight = ec.headerSizer.getVisibleHeight();

        // clear the resizer canvas to avoid overflow warnings next time
        ec.headerSizer.hide();
        ec.headerSizer.clear();
        return newHeight;
    }

});

isc.EventCanvas.addProperties({
    visibility: "hidden",
    autoDraw: false,
    overflow: "hidden",
    minHeight: 1,
    minWidth: 1,

    hideUsingDisplayNone: true,

    // hover properties - see also getHoverHTML()
    showHover: true,
    canHover: true,
    hoverMoveWithMouse: true,
    hoverAutoFitWidth: true,
    hoverAutoFitMaxWidth: 200,

    // drag properties
    dragType: "EventCanvas",
    snapToGrid: false,
    //keepInParentRect: true,
    dragAppearance: "none",
    canDragResize: true,
    canDragReposition: true,

    isEventCanvas: true,
    //> @attr eventCanvas.isZoneCanvas (boolean : false : R)
    // Readonly property indicating whether this is a special +link{class:ZoneCanvas} subclass.
    // @visibility calendar
    //<
    isZoneCanvas: false,

    //> @attr eventCanvas.isIndicatorCanvas (boolean : false : R)
    // Readonly property dictating whether this is a special +link{class:IndicatorCanvas}
    // subclass.
    // @visibility calendar
    //<
    isIndicatorCanvas: false,

    //> @attr eventCanvas.escapeHTML (Boolean : null : IRW)
    // When set to true, escapes the HTML displayed in this EventCanvas and it's hover.
    //
    // @visibility external
    //<
    // The default is taken from +link{calendar.eventEscapeHTML}.
    //escapeHTML: null,

    //> @attr eventCanvas.showHeader (Boolean : null : IRW)
    // Renders a header DIV above the main body of the event, an area of limited
    // height, styled to stand out from the main +link{eventCanvas.showBody, body} of the
    // event, and typically showing a +link{calendarEvent.name, name} or title - like a Window.
    // This header area can be styled via +link{eventCanvas.headerStyle} and the HTML it shows
    // is retrieved from a call to +link{eventCanvas.getHeaderHTML, getHeaderHTML()}.
    // The default is taken from +link{calendar.showEventHeaders}.
    //
    // @visibility external
    //<
    //showHeader: true,
    getShowHeader : function () {
        if (this.showHeader != null) return this.showHeader;
        return this.calendar.showEventHeaders;
    },

    //> @attr eventCanvas.showBody (Boolean : null : IRW)
    // Renders a body DIV that fills the main area of the canvas, or all of it if no
    // +link{eventCanvas.showHeader, header} is shown.  This area typically displays an
    // +link{calendarEvent.description, event description}.  This area can be styled via
    // +link{eventCanvas.bodyStyle} and the HTML it shows is retrieved
    // from a call to +link{eventCanvas.getBodyHTML, getBodyHTML()}.  The default is taken
    // from +link{calendar.showEventDescriptions}.
    //
    // @visibility external
    //<
    //showBody: true,
    getShowBody : function () {
        if (this.showBody != null) return this.showBody;
        return this.calendar.showEventDescriptions;
    },

    //> @attr eventCanvas.verticalResize (Boolean : null : IRW)
    // Indicates the orientation of the event in its containing view.  Affects drag and resize
    // orientation and which edges of the canvas are available for resizing.
    //
    // @visibility external
    //<
    //verticalResize: true,

    //> @attr eventCanvas.styleName (CSSStyleName : null : IRW)
    // The CSS class for this EventCanvas.  Defaults to the style on
    // +link{calendarEvent.styleName, eventCanvas.event}, if specified, or on the
    // +link{calendar.eventStyleName, calendar} otherwise.
    // <P>
    // Also see +link{eventCanvas.headerStyle} and +link{eventCanvas.bodyStyle}.
    // @group appearance
    // @visibility external
    //<

    //> @attr eventCanvas.event (CalendarEvent : null : IR)
    // The +link{CalendarEvent, event} associated with this EventCanvas.
    // @visibility external
    //<

    //> @attr eventCanvas.calendar (Calendar : null : IR)
    // The +link{Calendar} in which this EventCanvas is being rendered.
    // @visibility external
    //<

    //> @attr eventCanvas.calendarView (CalendarView : null : IR)
    // The +link{CalendarView} in which this EventCanvas is being rendered.
    // @visibility external
    //<

    _mouseTransparent: null,

    //> @attr eventCanvas.showLabel (Boolean : null : IRW)
    // When set to true, the +link{eventCanvas.getHeaderHTML, header text} for the
    // associated event is not rendered inside the eventCanvas itself.
    // <P>
    // Instead, it is rendered in it's own +link{eventCanvas.label, label} and shown
    // as a peer of this eventCanvas, immediately outside of it.
    // @visibility external
    //<
    //showLabel: false,

    //> @attr eventCanvas.label (AutoChild Label : null : IRW)
    // When +link{eventCanvas.showLabel, showLabel} is true, this autoChild is
    // used to display the +link{eventCanvas.getHeaderHTML, header text}, adjacent to
    // this eventCanvas.
    // @visibility external
    //<

    //> @attr eventCanvas.labelSnapTo (String : null : IRW)
    // The side to snap the +link{eventCanvas.label} to when
    // +link{eventCanvas.showLabel, showLabel} is true.
    // <P>
    // Possible values: BR, BL, TR, TL, R, L, B, T, C - where B=Bottom, T=Top, L=Left, R=right
    // and C=center.
    // <P>
    // This general position can then be fine-tuned via
    // +link{eventCanvas.labelOffsetX, labelOffsetX} and
    // +link{eventCanvas.labelOffsetY, labelOffsetY}.
    // @visibility internal
    //<
    //labelSnapTo: "B",

    //> @attr eventCanvas.labelOffsetX (Integer : 0 : IRW)
    // When +link{eventCanvas.showLabel, showLabel} is true, this is the number of
    // horizontal pixels to offset the snapTo position of this component's
    // +link{eventCanvas.label} by.
    // @visibility internal
    //<
    labelOffsetX: 0,

    //> @attr eventCanvas.labelOffsetY (Integer : 0 : IRW)
    // When +link{eventCanvas.showLabel, showLabel} is true, this is the number of
    // vertical pixels to offset the snapTo position of this component's
    // +link{eventCanvas.label} by.
    // @visibility internal
    //<
    labelOffsetY: 0,

    //> @attr eventCanvas.titleOrientation (VerticalAlignment : "top" : IRW)
    // When +link{eventCanvas.showLabel, showLabel} is not set to true, this attribute controls
    // the vertical alignment of the +link{eventCanvas.getHeaderHTML, headerHTML} in this
    // canvas.
    //
    // @visibility internal
    //<
    titleOrientation: "top",

    //> @attr eventCanvas.showGripper (Boolean : null : IRW)
    // When set to true, shows the +link{eventCanvas.gripper, gripper} component, which snaps,
    // centered, to the top edge of the eventCanvas and can be used to move it with the mouse.
    //
    // @visibility external
    //<
    //showGripper: false,

    //> @attr eventCanvas.gripperIcon (SCImgURL : null : IRW)
    // The source for the icon displayed as the "gripper" that snaps to the top of an event
    // canvas and allows an event to be dragged with the mouse.
    // @visibility external
    //<

    //> @attr eventCanvas.gripper (AutoChild Img : null : IRW)
    // When +link{eventCanvas.showGripper, showGripper} is true, this is the component that will
    // be rendered adjacent to the canvas and allow the canvas to be moved with the mouse.
    //
    // @visibility external
    //<

    maxLabelWidth: 150,


    //opacity: 90,

    // component implementation - see calendar.create/updateEventCanvasComponent() APIs/docs
    showComponent: null,
    _shouldShowComponent : function () {
        if (this.showComponent == false) return false;
        return this.calendar.showEventCanvasComponents;
    },
    getComponent : function (event) {
        if (this.component) {
            return this.component;
        }
        this.component = this.calendar.createEventCanvasComponent(this, this.calendarView);
        this.addMember(this.component);
        return this.component;
    },

    allowBodyScroll: false,
    headerLabelDefaults: {
        _constructor: "Canvas",
        width: "100%", height: 1,
        autoDraw: false,
        overflow: "visible"
    },
    bodyLabelDefaults: {
        _constructor: "Canvas",
        width: "100%",
        height: "*",
        overflow: "auto",
        autoDraw: false
    },

    init : function () {
        // autoFit means size to the header and/or body content - used with showColumnLayouts
        this.autoFit = this.calendar.showColumnLayouts;
        if (this.autoFit) {
            this.height = 1;
            this.overflow = "visible";
        } else {
            this.overflow = "hidden";
        }
        return this.Super("init", arguments);
    },
    initWidget : function () {
        // verticalResize is null here, use calendarView.verticalEvents
        if (this.verticalResize == null) this.verticalResize = this.calendarView.verticalEvents;


        this.resizeFrom = [];

        this.hoverDelay = this.calendar.hoverDelay + 1;

        if (this.useStaticControls == null) {
            this.useStaticControls = (this.calendar.useEventCanvasRolloverControls == false);
        }
        if (this.canFocus == null) {
            this.setCanFocus(this.calendar.canSelectEvents);
        }

        if (this.calendar.showColumnLayouts) {
            //this.dragType = "EventCanvas";
            this.dragTarget = this;
            this.dragAppearance = "target";
            this.canDragReposition = true;
            this.canDrop = true;
        }

        //if (!this.calendar.showEventDescriptions) this.showBody = false;
        this.Super("initWidget", arguments);

        if (this.shouldShowGripper()) this.createGripper();
        if (this.shouldShowLabel()) this.createLabel();

        //if (this.event) this.setEvent(this.event, this.styleName);

        if (this._mouseTransparent == null && !this.calendarView.shouldShowEventHovers()) {
            //this._mouseTransparent = true;
        }

        this.createHeaderAndBody();

        this.updateShowHovers();
    },

    createHeaderAndBody : function () {
        if (this.getShowHeader()) {
            // header customizations
            var hProps = { dragTarget: this };
            if (this.headerHeight != null) {
                hProps.height = this.headerHeight;
                hProps.overflow = "hidden";
            }
            if (!this.getShowBody()) hProps.height = "*";
            this.addAutoChild("headerLabel", hProps);
            this.addMember(this.headerLabel);
        }

        if (this.getShowBody()) {
            // body customizations
            var bProps = { dragTarget: this };
            if (this.autoFit) bProps.overflow = "visible";
            else if (!this.allowBodyScroll) bProps.overflow = "hidden";
            this.addAutoChild("bodyLabel", bProps);
        }

        if (this.bodyLabel) {
            this.addMember(this.bodyLabel, (this.titleOrientation == "bottom" ? 0 : null));
        }
    },

    updateShowHovers : function () {
        if (this._mouseTransparent) this.eventProxy = this.calendarView;
    },

    shouldShowGripper : function () {
        var cal = this.calendar;
        // don't show the gripper if there's no event, or the event can't be edited or dragged
        if (!this.event || !cal.canEditEvent(this.event, this.calendarView) ||
                !cal.canDragEvent(this.event, this.calendarView)) {
            return false;
        }
        if (this.showGripper != null) return this.showGripper;
        if (this.calendar.isZeroLengthEvent(this.event)) return true;
        return false;
    },
    createGripper : function () {
        if (this.gripper) return;
        // create the gripper if one is required - floats adjacent to this canvas - allows
        // this canvas to be moved by dragging
        var props = {
            canDrag: true,
            dragTarget: this.dragTarget,
            eventProxy: this,
            eventCanvas: this,
            canDragResize: false,
            styleName: this.gripperStyle||this.styleName+"Gripper"
        };
        this.gripper = this.calendar.getEventCanvasGripper(props, this, this.calendarView);
    },

    shouldShowLabel : function () {
        if (!this.event) return false;
        if (this.showLabel != null) return this.showLabel;
        if (this.calendar.isZeroLengthEvent(this.event)) return true;
        return false;
    },
    createLabel : function () {
        if (this.label) return;
        // create the label if one is required - this will float adjacent to the
        // eventCanvas, rather than taking up any of it's inner area
        var props = {
            canDrag: false,
            //eventProxy: this,
            eventCanvas: this,
            canDragResize: false,
            canHover: true,
            showHover: true,
            showOver: false,
            showRollOver: false,
            margin: 3,
            styleName: this.labelStyle||this.styleName+"Gripper",
            contents: this.getHeaderHTML(),
            getHoverHTML : function () {
                return this.eventCanvas.getHoverHTML();
            }
        };
        this.label = this.calendar.getEventCanvasLabel(props, this.calendarView);
    },

    parentScrolled : function () {
        if (this.gripper || this.label) this.repositionPeers();
    },

    redraw : function () {
        var result = this.Super("redraw", arguments);
        if (this.gripper || this.label) this.repositionPeers();
        return result;
    },

    draw : function () {
        var result = this.Super("draw", arguments);
        //if (this._updateCanvasColorsOnDraw) {
            delete this._updateCanvasColorsOnDraw;
            this.updateCanvasColors();
        //}
        return result;
    },

    hide : function () {
        this.Super("hide", arguments);
        if (this.gripper) this.gripper.hide();
        if (this.label) this.label.hide();
        this.hideLeadingTrailingDates();
    },

    leadingTrailingIconDefaults: {
        _constructor: "Img",
        autoDraw:false,
        _redrawWithParent: false,
        getCalendar : function () { return this.creator.calendar; },
        getView : function () { return this.creator.calendarView; },
        getEventCanvas : function () { return this.creator; },
        getEventInstance : function () { return this.creator.event; },
        mouseDown: function () {
            this.Super("mouseDown", arguments);
            return isc.EH.STOP_BUBBLING;
        },
        dragAppearance: "target",
        dragRepositionStart : function () {
            this.startTop = this.getTop();
            this.lastleft = this.getLeft();
        },
        dragRepositionMove : function () {
            //isc.logWarn('icon drag start:');
            this.setTop(this.startTop);
            var newDate = this.getView().getDateFromPoint();
            // left-align the icon to the snap-date - the image itself is also left-aligned
            var newLeft = this.getView().getDateLeftOffset(newDate);
            var line = window[this.lineID];
            if (this.type == "trail") {
                newLeft += this.getView().getSnapGapPixels();
                // right-align the icon to the snap-date - the image itself is also right-aligned
                newLeft -= this.getWidth();
                if (newLeft+1 < this.getEventCanvas().getLeft() + this.getEventCanvas().getWidth()) {
                    // over the canvas - don't move the icon
                    this.setLeft(this.lastleft);
                    return;
                }
                line.setWidth(Math.round((newLeft - line.getLeft()) + this.getWidth()));
            } else {
                if (newLeft-1 > this.getEventCanvas().getLeft()) {
                    // over the canvas - don't move the icon
                    this.setLeft(this.lastleft);
                    return;
                }
                line.setLeft(newLeft);
                line.setWidth(Math.round(this.getEventCanvas().getLeft() - newLeft));
            }
            this.lastLeft = newLeft;
            this.setLeft(newLeft);
        },
        dragRepositionStop : function () {
            this.setTop(this.startTop);
            var newLeft = this.getLeft();
            // if it's a trailing icon, add a snapGap
            if (this.type == "trail") newLeft -= 1;

            // new leading/trailingDate is the one at the new left offset
            var newDate = this.getView().getDateFromPoint(newLeft + 1);
            //isc.say("dropping at " + newDate.toString());

            var cal = this.getCalendar();
            var eventObj = this.getEventInstance();
            var dateField = this.type == "trail" ? cal.trailingDateField : cal.leadingDateField;
            var otherFields = {};
            otherFields[dateField] = newDate;
            // build the new event as it would be after the drop
            var newEvent = isc.addProperties({}, eventObj, otherFields);
            cal.updateCalendarEvent(eventObj, newEvent); //, otherFields, true);

            return true;
        }
    },
    leadingTrailingLineDefaults: {
        _constructor: "Canvas",
        autoDraw:false,
        _redrawWithParent: false,
        height: 2,
        overflow: "hidden",
        styleName: "eventLine"
    },

    createLeadingTrailingDates : function () {
        var view = this.calendarView;
        if (!this.leadingIcon) {
            // create the lead/trailing icons and lines
            this.leadingLine = this.createAutoChild("leadingTrailingLine", {
                type: "lead"
            });
            this.leadingIcon = this.createAutoChild("leadingTrailingIcon", {
                width: view.leadIconSize, height: view.leadIconSize,
                lineID: this.leadingLine.getID(),
                type: "lead",
                src: view.leadingEndPointImage,
                canDragReposition: this.canDragReposition,
                canDrag: this.canDrag
            });
            // trailing line and icon
            this.trailingLine = this.createAutoChild("leadingTrailingLine", {
                type: "trail"
            });
            this.trailingIcon = this.createAutoChild("leadingTrailingIcon", {
                width: view.trailIconSize, height: view.trailIconSize,
                lineID: this.trailingLine.getID(),
                type: "trail",
                src: view.trailingEndPointImage,
                canDragReposition: this.canDragReposition,
                canDrag: this.canDrag
            });
        }
        if (this.leadingLine.parentElement != view.body) view.body.addChild(this.leadingLine, null, false);
        if (this.leadingIcon.parentElement != view.body) view.body.addChild(this.leadingIcon, null, false);
        if (this.trailingLine.parentElement != view.body) view.body.addChild(this.trailingLine, null, false);
        if (this.trailingIcon.parentElement != view.body) view.body.addChild(this.trailingIcon, null, false);
    },
    hideLeadingTrailingDates : function () {
        this.leadingIcon && this.leadingIcon.hide();
        this.leadingLine && this.leadingLine.hide();
        this.trailingIcon && this.trailingIcon.hide();
        this.trailingLine && this.trailingLine.hide();
    },
    updateLeadingTrailingDates : function (canvas) {
        // destroy previous lines and icons before creating new ones
        //canvas.destroyLines();
        if (!this.leadingIcon) {
            this.createLeadingTrailingDates();
        }

        var showLeadingIcon = this._positionIcon(this.leadingIcon, this.leadingLine);
        var showTrailingIcon = this._positionIcon(this.trailingIcon, this.trailingLine);

        // and show/hide stuff as appropriate
        if (this.leadingLine.getWidth() > 0) {
            if (!this.leadingLine.isDrawn()) this.leadingLine.draw();
            this.leadingLine.show()
        } else this.leadingLine.hide();
        if (showLeadingIcon) {
            if (!this.leadingIcon.isDrawn()) this.leadingIcon.draw();
            this.leadingIcon.show();
        } else this.leadingIcon.hide();

        if (this.trailingLine.getWidth() > 0) {
            if (!this.trailingLine.isDrawn()) this.trailingLine.draw();
            this.trailingLine.show()
        } else this.trailingLine.hide();
        if (showTrailingIcon) {
            if (!this.trailingIcon.isDrawn()) this.trailingIcon.draw();
            this.trailingIcon.show();
        } else this.trailingIcon.hide();
    },

    _positionIcon : function (icon, line) {
        var cal = this.calendar,
            view = this.calendarView,
            canvas = this,
            event = canvas.event,
            type = icon.type,
            canvasTop = canvas.getTop(),
            canvasLeft = canvas.getLeft(),
            canvasWidth = canvas.getVisibleWidth(),
            canvasHeight = canvas.getVisibleHeight()
        ;

        var iconVisible = true;
        var iconLeft = 0;
        var iconTop = Math.round(canvasTop + ((canvasHeight - icon.height) / 2));

        if (!line) line = window[icon.lineID];
        var lineLeft = canvasLeft;
        var lineTop = Math.round(canvasTop + ((canvasHeight - line.getHeight()) / 2));
        var lineWidth = 0;

        if (type == "trail") {
            // icon is visible if trailingDate < view.endDate - line to grid-edge, as required
            iconVisible = view.compareDates(event[cal.trailingDateField],view.endDate) >= 0;
            iconLeft = view.getDateLeftOffset(event[cal.trailingDateField]);
            iconLeft += view.getSnapGapPixels() - 1;
            iconLeft -= icon.width;

            lineLeft += canvasWidth;
            if (iconVisible) lineWidth = (iconLeft + icon.width) - lineLeft;
            else lineWidth = view.body._fieldWidths.sum() - lineLeft;
        } else {
            // icon is visible if leadingDate > view.startDate - line to grid-edge, as required
            iconVisible = view.compareDates(event[cal.leadingDateField],view.startDate) < 0;
            iconLeft = view.getDateLeftOffset(event[cal.leadingDateField]);

            lineLeft = Math.max(0, iconLeft);
            lineWidth = canvasLeft - lineLeft;
        }

        //isc.logWarn(event[cal.trailingDateField].toShortDate());
        line.setRect(Math.round(lineLeft), Math.round(lineTop),
            (Math.round(lineWidth) > 0 ? Math.round(lineWidth) : null));

        if (iconVisible) {
            icon.setRect(Math.round(iconLeft), Math.round(iconTop));
            //icon.show();
        } else {
            icon.hide();
        }

        return iconVisible;
    },

    repositionPeers : function (skipDraw) {
        if (!this.gripper && !this.label) return;



        var view = this.calendarView,
            body = view.body,
            showLabel = this.shouldShowLabel(),
            showGripper = this.shouldShowGripper()
        ;

        // hide both peers and bail if the event is horizontally outside of the viewport
        var bodyLeft = body.getLeft(),
            bodyScrollLeft = body.getScrollLeft(),
            bodyWidth = body.getVisibleWidth(),
            thisWidth = this.isDrawn() ? this.getWidth() : view._getEventBreadth(this.event),
            thisLeft = this.getLeft() + Math.floor(thisWidth / 2)
        ;
        if (thisLeft < bodyScrollLeft || thisLeft > bodyScrollLeft + bodyWidth) {
            // h-center of the event is outside of the viewport - hide peers and bail
            if (this.gripper && this.gripper.isVisible()) this.gripper.hide();
            if (this.label && this.label.isVisible()) this.label.hide();
            return;
        }

        var bodyTop = body.getTop(),
            bodyScrollTop = body.getScrollTop(),
            bodyHeight = body.getViewportHeight(),
            thisTop = this.getTop(),
            thisHeight = this.getHeight(),
            thisBottom = thisTop + thisHeight,
            hideGripper = false,
            hideLabel = false
        ;

        // mark gripper/label to be hidden if top/bottom of the event are outside of the viewport
        if (thisTop < bodyScrollTop || thisTop > bodyScrollTop + bodyHeight) hideGripper = true;
        if (thisBottom < bodyScrollTop || thisBottom > bodyScrollTop + bodyHeight + 1) hideLabel = true;

        if (this.gripper) {
            if (hideGripper || !showGripper) this.gripper.hide();
            else {
                var left = thisLeft + bodyLeft - bodyScrollLeft,
                    top = view.header.getHeight() + thisTop - bodyScrollTop;

                if (!skipDraw && this.isDrawn() && !this.gripper.isDrawn()) this.gripper.draw();

                left = Math.floor(left - Math.floor(this.gripper.getVisibleWidth() / 2));
                top = Math.floor(top - (this.gripper.getVisibleHeight() / 2));

                this.gripper.moveTo(left, top);
                if (!skipDraw && this.isDrawn() && !this.gripper.isVisible()) {
                    this.gripper.show();
                    this.gripper.bringToFront();
                }
            }
        }
        if (this.label) {
            if (hideLabel || !showLabel) {
                this.label.hide();
            } else {
                var left = thisLeft + bodyLeft - bodyScrollLeft,
                    top = view.header.getHeight() + thisBottom - bodyScrollTop,
                    headerHTML = this.getHeaderHTML(),
                    textHeight = isc.EventCanvas.getHeaderHeight(headerHTML,
                        (this.maxLabelWidth || thisWidth), this.headerHeight,
                        this.getHeaderWrap(), this
                    )
                ;

                this.label.setContents(headerHTML);
                if (!skipDraw && this.isDrawn() && !this.label.isDrawn()) this.label.draw();

                left = Math.floor(left - Math.floor(this.label.getVisibleWidth() / 2));
                top = Math.floor(top - (textHeight / 2));

                this.label.moveTo(left, top);
                if (!skipDraw && this.isDrawn() && !this.label.isVisible()) {
                    this.label.show();
                    this.label.bringToFront();
                }
            }
        }
    },

    //> @method eventCanvas.setEvent()
    // Assigns a new +link{CalendarEvent, event} to this EventCanvas, including updates to
    // drag, style and +link{eventCanvas.showRolloverControls, rollover} properties.
    //
    // @param event (CalendarEvent) the new event to apply to this EventCanvas
    // @param [styleName] (CSSStyleName) optional CSS class to apply to this EventCanvas
    // @param [headerStyle] (CSSStyleName) optional separate CSS class to apply to the
    //                                     +link{eventCanvas.showHeader, header}.
    // @param [bodyStyle] (CSSStyleName) optional separate CSS class to apply to the
    //                                     +link{eventCanvas.showBody, body}.
    // @group appearance
    // @visibility external
    //<
    setEvent : function (event, styleName, headerStyle, bodyStyle) {
        var cal = this.calendar,
            view = this.calendarView
        ;

        // pre-calculate a bunch of date/sizing/edit values for the new event to save time later
        var cache = this._updateValueCache(event, styleName, headerStyle, bodyStyle);
        this.sortValue = event[cal.startDateField].duplicate();

        // apply some drag-related properties
        //this.showCloseButton = cache.canRemove;
        this.canDragReposition = cache.canDragMove;
        this.canDragResize = cache.canDragResize;
        //this.setProperty("canDrag", cache.canDrag);
        if (this.canDragReposition == false) {
            this.setCursor(isc.Canvas.DEFAULT);
        } else {
            this.setCursor(isc.Canvas.MOVE);
        }

        this.resizeFrom = [];
        if (cache.canDragResize) {
            if (cache.canDragStartDate) {
                if (!this.verticalResize) this.resizeFrom.add("L");
            }
            if (cache.canDragEndDate) {
                if (!this.verticalResize) this.resizeFrom.add("R");
                else this.resizeFrom.add("B");
            }
        }

        if (this.shouldShowGripper()) this.createGripper();
        else if (this.gripper) this.gripper.hide();
        if (this.shouldShowLabel()) this.createLabel();
        else if (this.label) this.label.hide();

        // only call setEventStyle() if it's changed
        this.checkStyle();
        //this.setEventStyle(cache.eventStyleName, headerStyle, bodyStyle);

        // these two attributes are used by willShowHover(), to only show event-hovers
        // when calendar.alwaysShowEventHovers is true, or the event's content is clipped
        this._willShowHover = null;
        this._contentHeight = 0;

        // set content in the header and body widgets
        if (this.getShowHeader()) {
            this.headerLabel.setContents(this.getHeaderHTML() || "");
        }
        if (this.getShowBody()) {
            // by default, getBodyHTML() will return null if the descriptionField is unset for
            // the event, and that causes setContents() to no-op and not clear the previous
            // event's description - if null, default to empty string
            this.bodyLabel.setContents(this.getBodyHTML() || "");
        }

        // get the child component that fills this canvas
        if (this.calendar.showEventCanvasComponent(this)) {
            var component = this.getComponent(event);
            if (component) {
                this.calendar.updateEventCanvasComponent(this, component);
            }
        }
    },

    //getCursor : function () {
    //    if (this.calendarView.__printing) return "arrow";
    //    return this.Super("getCursor", arguments);
    //},

    setCursor : function (cursor) {
        this.Super("setCursor", arguments);
        // potentially also setCursor() on the header/body widgets, and the custom component
        if (this.getShowHeader()) this.headerLabel.setCursor(this.cursor)
        if (this.getShowBody()) this.bodyLabel.setCursor(this.cursor)
        if (this.component && this.component.setCursor) this.component.setCursor(this.cursor);
    },

    _getCacheValue : function (valueName) {
        return this._cacheValues && this._cacheValues[valueName];
    },

    _updateValueCache : function (event, styleName, headerStyle, bodyStyle) {
        this._cacheValues = {};
        this.event = event;
        if (this.event) {
            var cal = this.calendar,
                view = this.calendarView,
                cache = this._cacheValues
            ;
            // store various drag-related properties
            cache.canEdit = cal.canEditEvent(event, view);
            cache.canDrag = cache.canDragMove = cal.canDragEvent(event, view);
            cache.canDragResize = cal.canResizeEvent(event, view);
            cache.canRemove = cal.canRemoveEvent(event, view);

            if (this._inPrintFlow) {
                // if called when printing, disable cursor-changes
                cache.canDrag = false;
                cache.canDragMove = false;
                cache.canDragResize = false;
                cache.canRemove = false;
            }

            // pre-calculate some event-related values
            cache.eventStartDate = cal.getEventStartDate(event).getTime();
            cache.eventEndDate = cal.getEventEndDate(event).getTime();
            cache.eventStyleName = cal.getEventCanvasStyle(event, view);
            cache.eventLane = event[cal.laneNameField];

            // get a few attributes for the view that only change when the visible range changes
            var view = this.calendarView;
            cache.viewStartDate = cal.getPeriodStartDate(view).getTime();
            cache.viewEndDate = cal.getPeriodEndDate(view).getTime();

            // drag properties
            cache.canDragStartDate = cache.canDragResize && cache.eventStartDate >= cache.viewStartDate;
            if (view.verticalEvents) cache.canDragStartDate = false;
            cache.canDragEndDate = cache.canDragResize && cache.eventEndDate && cache.eventEndDate <= cache.viewEndDate;
            cache.showCloseButton = cache.canRemove;

        }

        return this._cacheValues;
    },

    createRolloverControls : function () {
        // create all the _staticControls for this canvas, which will manage their
        // visibility whenever setEvent() is called
        return this.calendar._createEventCanvasControls(this);
    },
    updateRolloverControls : function (mouseOver) {
        var cal = this.calendar,
            c = this._staticControls || this._focusControls || this._rolloverControls
        ;

        if (!c) {
            if (this.useStaticControls) this._staticControls = this.createRolloverControls();
            else if (this.isFocused()) this._focusControls = cal._getFocusControls();
            else if (mouseOver) this._rolloverControls = cal._getRolloverControls();
            c = this._staticControls || this._focusControls || this._rolloverControls;
        }
        if (!c) return;

        for (var key in c) {
            c[key].eventCanvas = this;
            this[key] = c[key];
        }

        if (this.closeButton) this.closeButton.canFocus = false;
        if (this.buttonLayout) {
            this.buttonLayout.canFocus = false;
            this.addChild(this.buttonLayout, null, false);
        }
        if (this.verticalResize) {
            this.startResizer = null;
            this.endResizer = this.endResizerB;
        } else {
            this.startResizer = this.startResizerL;
            this.endResizer = this.endResizerR;
        }
        if (this.startResizer) {
            this.startResizer.dragTarget = this.dragTarget;
            this.addChild(this.startResizer, null, false);
        }
        if (this.endResizer) {
            this.endResizer.dragTarget = this.dragTarget;
            this.addChild(this.endResizer, null, false);
        }

        var cache = this._cacheValues || {};

        if (!this.buttonLayout.isDrawn() && this.isDrawn()) this.buttonLayout.draw();
        this.buttonLayout.show();
        this.buttonLayout.bringToFront();
        if (this.closeButton) {
            if (!cache.showCloseButton) this.closeButton.hide();
            else {
                if (!this.closeButton.isDrawn() && this.isDrawn()) this.closeButton.draw();
                this.closeButton.show();
            }
        }
        if (this.contextButton) {
            if (!this.shouldShowContextButton()) this.contextButton.hide()
            else {
                // only shown if not switched off and getEventCanvasMenuItems() returns something
                var menuItems = this.calendar.getEventCanvasMenuItems(this, this.calendarView);
                if (menuItems) {
                    if (!this.contextButton.isDrawn() && this.isDrawn()) this.contextButton.draw();
                    this.contextButton.show();
                } else this.contextButton.hide();
            }
        }

        if (this.startResizer) {
            if (!cache.canDragStartDate) this.startResizer.hide();
            else {
                if (!this.startResizer.isDrawn() && this.isDrawn()) this.startResizer.draw();
                this.startResizer.show();
                this.startResizer.bringToFront();
            }
        }
        if (this.endResizer) {
            if (!cache.canDragEndDate) this.endResizer.hide();
            else {
                if (!this.endResizer.isDrawn() && this.isDrawn()) this.endResizer.draw();
                this.endResizer.show();
                this.endResizer.bringToFront();
            }
        }
    },

    markForColorUpdate : function (data) {
        // flags that colors were changed on style-handles - causes checkStyle()
        // to always run setStyleName(), even if the styleName is the same -
        // needed to clear the style handles
        this.needsColorUpdateOnSetEvent = true;
    },

    setEventStyle : function (styleName, headerStyle, bodyStyle) {
        headerStyle = headerStyle || this.headerStyle || (styleName + "Header");
        bodyStyle = bodyStyle || this.bodyStyle || (styleName + "Body");
        this.baseStyle = styleName;
        if (this.event.isSelected) {
            this.baseStyle += "Selected";
            headerStyle += "Selected";
            bodyStyle += "Selected";
            styleName += "Selected";
        }
        this._bodyStyle = bodyStyle;
        this._headerStyle = headerStyle;
        if (this.gripper) {
            this.gripper.setStyleName(this.gripperStyle || styleName + "Gripper");
        }
        if (this.label) this.label.setStyleName(this.labelStyle || styleName + "Label");

        // if the innerHTML has been cached, delete to rebuild with the new styles
        if (this._cacheValues) delete this._cacheValues._innerHTML;

        if (this.needsColorUpdateOnSetEvent || this.styleName != styleName) {
            delete this.needsColorUpdateOnSetEvent;
            this.setStyleName(styleName);
            if (this.getShowHeader()) this.headerLabel.setStyleName(this._headerStyle);
            if (this.getShowBody()) this.bodyLabel.setStyleName(this._bodyStyle);
            this.markForRedraw();
        }

        this.updateCanvasColors();
    },

    updateCanvasColors : function () {
        // called from draw(), may not have an event yet in SGWT
        if (!this.event) return;
        var e = this.event;
        var domAccessed = false;
        if (e.textColor || e.backgroundColor || e.borderColor) {
            // if the event has a specified backgroundColor, set it directly on the styleHandle
            // so that an eventCanvas as a whole gets the same backgroudColor as the body
            var handle = this.getStyleHandle();
            if (handle) {
                handle["background-color"] = e.backgroundColor;
                if (e.borderColor) {
                    handle["border-style"] = "solid";
                    handle["border-color"] = e.borderColor;
                } else {
                    handle["border-color"] = null;
                }
            }
            if (e.textColor || e.backgroundColor) {
                // apply text and background, but not border, to the body widget
                var bodyHandle = this.bodyLabel ? this.bodyLabel.getStyleHandle() : null;
                if (bodyHandle) {
                    bodyHandle["background-color"] = e.backgroundColor;
                    if (e.textColor) bodyHandle["color"] = e.textColor;
                }
            }
            domAccessed = true;
        }
        if (e.headerTextColor || e.headerBackgroundColor || e.headerBorderColor) {
            var headerHandle = this.headerLabel ? this.headerLabel.getStyleHandle() : null;
            if (headerHandle) {
                // apply/clear text, background and border colors to the header widget
                if (e.headerBackgroundColor) headerHandle["background-color"] = e.headerBackgroundColor;
                if (e.headerTextColor) headerHandle["color"] = e.headerTextColor;
                if (e.headerBorderColor) headerHandle["border-color"] = e.headerBorderColor;
            }
            domAccessed = true;
        }
        // this flags that the colors are customized - if this canvas is reused while
        // this flag is set, setStyleName() needs to be called, even if the styleName
        // hasn't changed, in order to overwrite those customized settings on the handles
        if (domAccessed) this.markForColorUpdate();
    },


    getStartDate : function () {
        return this._getCacheValue("eventStartDate") || this.calendar.getEventStartDate(this.event);
    },
    getEndDate : function () {
        return this._getCacheValue("eventEndDate") || this.calendar.getEventEndDate(this.event);
    },
    getDuration : function () {
        return this.event[this.calendar.durationField];
    },

    // get event length in the passed unit, default minutes
    getEventLength : function (unit) {
        if (this.event.eventLength) return this.event.eventLength;
        return this.calendar.getEventLength(this.event, unit || "minute");
    },

    isZeroLengthEvent : function () {
        // returns true if the event has a "duration", and it's zero - includes IndicatorLines
        return this.calendar.isZeroLengthEvent(this.event);
    },



// ----------
// rendering

    //> @attr eventCanvas.headerWrap (Boolean : null : IRW)
    // Whether the +link{eventCanvas.showHeader, header area} should autosize vertically to
    // display all contents.  If true, the header will wrap to multiple lines.  If false, the
    // header will be sized according to the specified +link{eventCanvas.headerHeight, height},
    // or to the full height of the canvas is +link{eventCanvas.showBody, showBody} is false.
    // @group appearance
    // @visibility external
    //<
    //headerWrap: true,
    getHeaderWrap : function () {
        if (this.headerWrap != null) return this.headerWrap;
        return this.calendar.eventHeaderWrap;
    },
    //> @attr eventCanvas.headerHeight (Integer : null : IRW)
    // The height for the +link{eventCanvas.showHeader, header area}, when
    // +link{eventCanvas.headerWrap, headerWrap} is false and
    // +link{eventCanvas.showBody, showBody} is true.  If <code>showBody</code> is false, the
    // header area fills the canvas.
    // @group appearance
    // @visibility external
    //<
    //headerHeight: 12,
    getHeaderHeight : function (textHeight) {
        var result;
        if (textHeight || this.getShowBody()) {
            var definedHeight = this._getDefinedHeaderHeight(),
                thisWidth = this.isDrawn() || !this.calendarView.isTimelineView() ?
                    this.getWidth() : this.calendarView._getEventBreadth(this.event),
                width = thisWidth - (this.calendar.getLanePadding() * 2)
            ;
            var height = isc.EventCanvas.getHeaderHeight(this.getHeaderHTML(), width,
                    definedHeight, this.getHeaderWrap(), this
            );
            result = height;
        } else {
            result = this.getInnerHeight();
        }
        return result;
    },
    _getDefinedHeaderHeight : function () {
        return this.headerHeight != null ? this.headerHeight : this.calendar.eventHeaderHeight;
    },
    //> @attr eventCanvas.headerStyle (CSSStyleName : null : IRW)
    // CSS class for the +link{eventCanvas.showHeader, header area} of the EventCanvas.
    // If unset, defaults to the +link{eventCanvas.styleName, base styleName} with the suffix
    // "Header".
    // @group appearance
    // @visibility external
    //<
    getHeaderStyle : function () {
        // this internal variable is set up in setEventStyle() - the value might be passed into
        // that method, specified on the instance or auto-generated
        return this._headerStyle;
    },
    //> @method eventCanvas.getHeaderHTML()
    // Returns the HTML to show in the header of this EventCanvas.  The default implementation
    // returns the +link{calendar.nameField, name} of the current
    // +link{eventCanvas.event, event}.
    //
    // @return (HTMLString) HTML to display in the header of the canvas
    // @group appearance
    // @visibility external
    //<
    getHeaderHTML : function () {
        if (!this.event) {
            return "No event";
        }
        return this.calendar.getEventHeaderHTML(this.event, this.calendarView);
    },
    padding: null,

    //> @attr eventCanvas.bodyStyle (CSSStyleName : null : IRW)
    // CSS class for the +link{eventCanvas.showBody, body area} of the EventCanvas.
    // If unset, defaults to the +link{eventCanvas.styleName, base styleName} with the suffix
    // "Body".
    // @group appearance
    // @visibility external
    //<
    getBodyStyle : function () {
        // this internal variable is set up in setEventStyle() - the value might be passed into
        // that method, specified on the instance or auto-generated
        return this._bodyStyle;
    },

    //> @method eventCanvas.getBodyHTML()
    // Return the HTML to show in the body of this EventCanvas.  The default implementation
    // calls +link{calendar.getEventBodyHTML}, which returns the value of the
    // +link{calendar.descriptionField, description field} for the current
    // +link{CalendarEvent, event}.
    //
    // @return (HTMLString) HTML to display in the body of the canvas
    // @group appearance
    // @visibility external
    //<
    getBodyHTML : function () {
        if (!this.event) {
            return "";
        }
        return this.calendar.getEventBodyHTML(this.event, this.calendarView);
    },

// generating HTML

    // returns true if this particular event should show its content in a hover
    willShowHover : function ( ) {
        // if this flag is set to true, always show hovers
        if (this.calendar.alwaysShowEventHovers == true) return true;

        // if autoFit is true, overflow is "visible", so nothing is clipped - no hover
        if (this.autoFit) return false;

        // otherwise, we only want to show the content in a hover if the content is clipped
        if (this._willShowHover == null) {
            var width = this.getInnerWidth();
            if (this.getShowHeader()) {
                // measure the height of the headerHTML, wrapping it at the full innerWidth of
                // this canvas
                var headerHTML = this.getHeaderHTML() || "";
                this._contentHeight += isc.Canvas.measureContent(headerHTML, this.headerLabel.styleName,
                    true, true, { width: width, maxWidth: width }
                );
                // measure the contentWidth - we want hovers for h-clipping as well
                this._contentWidth = isc.Canvas.measureContent(headerHTML, this.headerLabel.styleName,
                    false, true, { width: width }
                );
            }
            if (this.getShowBody()) {
                var bodyHTML = this.getBodyHTML() || "";
                // measure the height of the bodyHTML, wrapping it at the full innerWidth of
                // this canvas
                this._contentHeight += isc.Canvas.measureContent(bodyHTML, this.bodyLabel.styleName,
                    true, true, { width: width, maxWidth: width }
                );
                // measure the contentWidth - we want hovers for h-clipping as well
                var bodyContentWidth = isc.Canvas.measureContent(bodyHTML, this.bodyLabel.styleName,
                    false, true, { width: width }
                );
                this._contentWidth = Math.max((this._contentWidth || 0), bodyContentWidth);
            }

            // if the combined content-height is greater than the canvas-height, or there's
            // unhandled h-clipping, show a hover
            this._willShowHover = this.getVisibleHeight() < this._contentHeight
                || this.getVisibleWidth() < this._contentWidth;
        }
        return this._willShowHover;
    },
    getHoverHTML : function () {
        if (this.calendarView.shouldShowEventHovers()) {
            if (this.willShowHover()) {
                return this.calendar._getEventHoverHTML(this.event, this, this.calendarView);
            }
        }
    },

    // more helpers
    shouldShowCloseButton : function () {
        if (this.showCloseButton != null) return this.showCloseButton != false;
        return this._getCacheValue("showCloseButton");
    },

    //> @attr eventCanvas.showContextButton (Boolean : false : IRW)
    // When set to true, shows a +link{calendar.eventCanvasContextButton, small icon} in the
    // top corner of an EventCanvas, beside the
    // +link{calendar.eventCanvasContextButton, close-icon}.  When clicked, shows a
    // +link{calendar.getEventCanvasMenuItems, context menu} containing items applicable to
    // this canvas.
    //
    // @visibility external
    //<
    showContextButton: false,
    shouldShowContextButton : function () {
        if (this.showContextButton != null) return this.showContextButton != false;
        return this._getCacheValue("showContextButton");
    },

    //> @attr eventCanvas.showRolloverControls (Boolean : true : IRW)
    // When set to the default value of true, this attribute causes a set of components to be
    // shown when the mouse rolls over this EventCanvas.  These components include the
    // +link{calendar.eventCanvasCloseButton, close} and
    // +link{calendar.eventCanvasContextButton, context} buttons, the latter's
    // +link{calendar.eventCanvasContextMenu, context menu} and the images used for
    // drag-resizing.
    // <P>
    // Using rollover controls is more efficient that showing static buttons in each
    // eventCanvas, so this is the default behavior.  See
    // +link{calendar.useEventCanvasRolloverControls} for the alternative.
    //
    // @visibility external
    //<
    showRolloverControls: true,
    getRolloverControls : function () { return null; },

    //> @method eventCanvas.renderEvent()
    // Sizes and draws this EventCanvas.
    //
    // @visibility internal
    //<
    renderEvent : function (eTop, eLeft, eWidth, eHeight, sendToBack) {
        //this.logWarn("renderEvent: " + this.event.eventId + " - " + this.event.name);
        this.setRect(eLeft, eTop, eWidth, eHeight);

        // get the styleName at render time - may have been dropped into a lane or sublane that
        // specifies a style for all of its events
        this.checkStyle();

        if (!this.parentElement || !this.parentElement.isDrawn()) return;

        if (!this.isDrawn()) this.draw();
        this.show();
        if (sendToBack) this.sendToBack();
        else this.bringToFront();

        if (this.shouldShowGripper() || this.shouldShowLabel()) {
            this.repositionPeers();
        }

        if (this.useStaticControls && !this.isZoneCanvas && !this.isIndicatorCanvas) {
            this.updateRolloverControls();
        }
    },
    checkStyle : function () {
        var styleName = this.calendar.getEventCanvasStyle(this.event, this.calendarView);
        if (this.needsColorUpdateOnSetEvent || this.calendar.canSelectEvents || styleName != this.styleName) {
            // if needsColorUpdateOnSetEvent is set, or canSelectEvents is true, always update
            // the style, even if it's the same
            this.setEventStyle(styleName);
        }
        this.updateCanvasColors();
    },

// internal stuff - mouse handler
    click : function () {
        // call the calendar-level handler, which will call the public eventClick()
        // notification as required
        this.calendar._eventCanvasClick(this)
    },
    doubleClick : function () {
        this.calendar._eventCanvasDoubleClick(this)
    },

    handleShowContextMenu : function () {
        // call the calendar-level handler, which will show the same menu as would be displayed
        // from a click on the contextMenu rollover control
        return this.calendar._eventCanvasContextClick(this);
    },

    mouseUp : function () {
        return isc.EH.STOP_BUBBLING;
    },

    rightMouseDown : function () {
        if (this.dragTarget && this.dragTarget != this) {
            this.dragTarget.eventCanvas = null;

            this.dragTarget._suppressShow = true;
            this.dragTarget.hide();
            return false;
        }
    },

    mouseDown : function () {
        if (!isc.EH.rightButtonDown()) {
            if (this.dragTarget) {
                this.dragTarget.eventCanvas = this;
                delete this.dragTarget._suppressShow;
            }
        }
        this.calendar.hideEventDialog();
        return isc.EH.STOP_BUBBLING;
    },

    focusChanged : function (hasFocus) {
        var target = isc.EH.getTarget();

        if (target != this && this.contains(target)) return;
        // if hasFocus is true, apply the secondary set of rolloverControls that show only in
        // the focused eventCanvas, for keyboard interaction
        if (hasFocus && !this._focusControls) {
            this.calendar._focusEventCanvas(this);
        } else if (this._focusControls) {
            this.calendar._blurEventCanvas(this);
        }
    },

    keyPress : function () {
        var cal = this.calendar,
            key = isc.EventHandler.getKey()
        ;

        var result = cal._eventCanvasKeyPress(this, key);
        if (result == false) return;
        return this.Super("keyPress", arguments);
    },

    dragRepositionStart : function () {
        if (this.calendarView.shouldShowColumnLayouts()) {
            this.deparent();
            this.bringToFront();
        }
    },
    handleMouseMove : function () {
        //var view = this.calendarView;
        //if (view.showRollOver) view.refreshRow(view.getEventRow());
        // return false so this canvas's hover isn't clobbered by the background cell-hover
        return false;
    },
    handleMouseOver : function () {
        // if the canvas has _staticControls, or is currently focused (has _focusControls),
        // don't show _rolloverControls - ignore mouseOver
        if (this._staticControls || this._focusControls) return;
        // if the canvas shouldn't show controls on rollover, bail
        if (this.showRolloverControls == false) return;
        if (this._rolloverControls) {
            var lastCanvas = isc.EH.lastEvent.target;
            if (lastCanvas == this || lastCanvas.eventCanvas == this) return;
        }
        // show rolloverControls
        this.updateRolloverControls(true);
    },

    mouseOut : function () {
        // if the canvas has _staticControls, or is currently focused (has _focusControls),
        // there aren't any _rolloverControls - ignore mouseOut
        if (this._staticControls || this._focusControls) return;
        // if the canvas shouldn't show controls on rollover, or isn't doing, bail
        if (this.showRolloverControls == false || !this._rolloverControls) return;
        var target = isc.EH.lastEvent.target;
        if (target && (target.eventCanvas == this || target == isc.Hover.hoverCanvas)) return;
        // hide rollover controls
        this.calendar.hideEventCanvasControls(this, "_rolloverControls");
    },


    parentResized : function () {
        this.Super('parentResized', arguments);
        // need to resize the event window here (columns are usually auto-fitting, so the
        // available space probably changed if the calendar as a whole changed size)
        //if (this.event) this.calendarView.sizeEventCanvas(this);

        if (this.shouldShowGripper() || this.shouldShowLabel()) {
            // if showing peers, reposition them now
            this.repositionPeers();
        }
    },

    _skipTheseControls: [ "closeButton", "contextButton" ],
    destroy : function () {
        if (this._staticControls) {
            // static controls should be destroyed automatically as children of this canvas
            var skipThese = [ "closeButton", "contextButton" ];
            for (var key in this._staticControls) {
                var comp = this._staticControls[key];
                // hide the control
                comp.hide();
                // remove it as a childand re-add it as a child of the Calendar, which removes it from the eventCanvas
                if (!skipThese.contains(key)) this.removeChild(comp);
                comp.destroy();
                delete this[key];
                comp = null;
            }
            delete this._staticControls;
        } else {
            if (this._focusControls) {
                this.calendar.hideEventCanvasControls(this, "_focusControls");
            } else if (this._rolloverControls) {
                this.calendar.hideEventCanvasControls(this, "_rolloverControls");
            }
        }
        if (this.gripper) {
            this.calendarView.removeChild(this.gripper);
            this.gripper.destroy();
            this.gripper = null;
        }
        if (this.label) {
            this.calendarView.removeChild(this.label);
            this.label.destroy();
            this.label = null;
        }
        if (this.dragTarget) this.dragTarget = null;
        if (this.component) {
            this.removeMember(this.component);
            this.component.destroy();
            this.component = null;
        }

        // leading/trailing UI
        this.leadingIcon && this.leadingIcon.destroy();
        this.leadingLine && this.leadingLine.destroy();
        this.trailingIcon && this.trailingIcon.destroy();
        this.trailingLine && this.trailingLine.destroy();
        this.leadingIcon = this.leadingLine = this.trailingIcon = this.trailingLine = null;

        this.calendarView = null;
        this.calendar = null;
        this.event = null;
        this._cacheValues = null;
        this.Super("destroy", arguments);
    },

    // eventCanvas
    getPrintHTML : function (printProperties, callback) {
        if (callback) {
            this.delayCall("asyncGetPrintHTML", [printProperties, callback]);
            return null;
        } else {
            return this.asyncGetPrintHTML(printProperties, callback);
        }
    },

    asyncGetPrintHTML : function (printProperties, callback) {
        var thisHandle = this.getHandle(),
            output = isc.StringBuffer.create(),
            cal = this.calendar,
            view = this.calendarView,
            isTimeline = view.isTimelineView(),
            // round the frozenWidth
            frozenWidth = Math.round(view.frozenBody ? view.frozenBody.getVisibleWidth() : 0),
            // event and render rect
            evt = this.event,
            top = 0,
            left = 0,
            // get sizes from the handle - printFrame.html now supports density, so matches view
            width = parseInt(thisHandle.style.width),
            height = parseInt(thisHandle.style.height),
            i = (printProperties && printProperties.i ? printProperties.i : 1)
        ;

        var bodyTop = view.body.getTop() + (view.header ? view.header.getVisibleHeight() : 0);

        var headerRows = cal.headerLevels ? cal.headerLevels.length : 1;
        var headerHeight = view.headerRowPrintHeight * headerRows;
        if (isTimeline) {
            top = bodyTop + this.getTop();
            //this.logWarn("canvas rect: " + this.getRect() + "\nhandle: " +
            //    [thisHandle.style.top, thisHandle.style.left, thisHandle.style.width, thisHandle.style.height])

            top = headerHeight + this.getTop();
            top = top + 1;

            left = frozenWidth + this.getLeft();
        } else {
            left = frozenWidth + this.getLeft();

            top = headerHeight + this.getTop();
            top += 1;

            width = parseInt(thisHandle.style.width);
            height = parseInt(thisHandle.style.height);
        }

        if (isc.Browser.isFirefox) {
            // not clear why FF mis-sizes here - might be something to do with box-sizing..
            width -= 4;
            height -= 4;
            // seems to sit events exactly right
            if (isTimeline) left += 1;
        }

        var baseStyle = this.styleName;

        output.append("<div class='", baseStyle, "' ",
            "style='vertical-align: ",
            (cal.showEventDescriptions ? "top" : "middle"), "; ",
            // colors from settings in the canvas style, rather than its CSS class
            (evt.backgroundColor ? "background-color: " + evt.backgroundColor + ";" : ""),
            (evt.textColor ? "color: " + evt.textColor + ";" : ""),
            (evt.borderColor ? "border-color: " + evt.borderColor + ";" : ""),
            // positioning
            "overflow: hidden; ",
            "position: absolute; ",
            "left: ", left, "px; top: ", top, "px; width: ", width, "px; height: ", height, "px; ",
            "z-index:", i+2, ";",
            "'>"
        );

        // reset the event with no cursor changes
        this._inPrintFlow = true;
        this.setEvent(this.event);
        this.redraw();

        var canvasHTML = this.getHandle().innerHTML;
        output.append(canvasHTML);

        output.append("</div>");

        // and reset the event back to whatever cursor changes it had before
        delete this._inPrintFlow;
        this.setEvent(this.event);
        this.redraw();

        if (this.label) {
            top = top + height - 5;
            width = this.label.getVisibleWidth();
            height = this.label.getInnerHeight();
            left -= Math.floor(width/2);
            output.append("<div class='", baseStyle + "Header", "' ",
                "style='overflow:hidden; ",
                "position: absolute; ",
                "padding:2px; ",
                "z-index:", i+2, ";",
                "left:", left, "px; top:", top, "px; width: ", width, "px; height: ", height, "px; ",
                "'>"
            );
            output.append(this.getHeaderHTML());
            output.append("</div>");
        }

        return output.release(false);
    }

});

//> @class ZoneCanvas
// A subclass of +link{Class:EventCanvas, EventCanvas}, used to render
// +link{calendar.zones, styled areas} in +link{class:CalendarView, calendar views}.
// <P>
// A ZoneCanvas is a semi-transparent canvas that highlights a portion of a
// calendar view, by rendering across all lanes and behind normal +link{calendar.data, events}.
// <P>
// By default, the canvas shows a bottom-aligned label containing the
// +link{calendarEvent.name, zone name}.
// Default styling is specified at the +link{calendar.zoneStyleName, calendar level}
// and can be overridden for +link{calendarEvent.styleName, individual zones}.
//
// @inheritsFrom EventCanvas
// @treeLocation  Client Reference/Calendar
// @visibility external
//<
isc.defineClass("ZoneCanvas", "EventCanvas");
isc.ZoneCanvas.addProperties({
    titleOrientation: null,
    showHeader: true,
    showBody: true,
    canEdit: false,
    canDrag: false,
    canDragReposition: false,
    canDragResize: false,
    canRemove: false,
    showRolloverControls: false,

    isEventCanvas: false,
    isZoneCanvas: true,

    maxLabelWidth: null,

    // allow the view to show it's hover text (for cells) when this canvas gets mouse moves
    //_mouseTransparent: true,
    initWidget : function () {
        this.showCloseButton = false;
        this.canDragReposition = false;
        this.canDragResize = false;
        if (this.titleOrientation == null) this.titleOrientation = this.calendar.zoneTitleOrientation;
        // _mouseTransparent sets the eventProxy for this canvas to the containing calendarView
        // - causes cellHovers to be shown instead of zoneHovers
        this._mouseTransparent = !this.calendarView.shouldShowZoneHovers();
        this.Super("initWidget", arguments);
    },
    setEvent : function (event, styleName, headerStyle, bodyStyle) {
        this.event = event;
        // make the canvas non-interactive, apart from hover prompt
        this.showCloseButton = false;
        this.canDragReposition = false;
        this.canDragResize = false;
        var cal = this.calendar;
        styleName = styleName || cal.getZoneCanvasStyle(event, this.calendarView);
        this.setEventStyle(styleName, headerStyle, bodyStyle);
    },
    click : function () {
        // fire calendar.zoneClick() if it's there
        if (this.calendar.zoneClick) this.calendar.zoneClick(this.event, this.calendarView.viewName)
    },
    getHoverHTML : function () {
        if (this.calendarView.shouldShowZoneHovers()) {
            var result = this.calendar._getZoneHoverHTML(this.event, this, this.calendarView);
            return result;
        }
    },
    checkStyle : function () {
        // no-op
    },
    updateRolloverControls : function () {
    }
});

//> @class IndicatorCanvas
// A subclass of +link{Class:EventCanvas, EventCanvas}, used to render
// +link{calendar.indicators, indicator lines} at important points in
// +link{class:CalendarView, calendar views}.
// <P>
// An IndicatorCanvas is a non-interactive, semi-transparent canvas that highlights a portion of a
// calendar view, by rendering across all lanes and behind normal +link{calendar.data, events}.
// <P>
// By default, the canvas shows no label but does show a hover.
// <P>
// Default styling is specified at the +link{calendar.indicatorStyleName, calendar level}
// and can be overridden for +link{calendarEvent.styleName, individual indicators}.
//
// @inheritsFrom EventCanvas
// @treeLocation  Client Reference/Calendar
// @visibility external
//<
isc.defineClass("IndicatorCanvas", "EventCanvas");
isc.IndicatorCanvas.addProperties({
    showHeader: false,
    showBody: false,
    headerSnapTo: "B",
    // show a "gripper" peer, top-aligned, that moves the IndicatorCanvas when dragged
    showGripper: true,
    showLabel: true,

    isEventCanvas: false,
    isIndicatorCanvas: true,

    canDrag: true,
    canDragReposition: true,
    dragRepositionCursor: isc.Canvas.MOVE,
    canDragResize: false,
    canRemove: false,
    showRolloverControls: false,
    initWidget : function () {
        this.showCloseButton = false;
        this.canDragResize = false;
        this.Super("initWidget", arguments);
        if (this.event) this.setEvent(this.event);
    },

    setEvent : function (event, styleName, headerStyle, bodyStyle) {
        this.event = event;

        // pre-calculate a bunch of date/sizing/edit values for the new event to save time later
        var cache = this._updateValueCache(event, styleName, headerStyle, bodyStyle);

        // apply some drag-related properties
        this.canEdit = cache.canEdit;
        this.canDrag = cache.canDrag;
        this.canDragResize = false;
        this.canDragReposition = cache.canDragMove;
        if (this.canDragReposition == false) {
            this.setCursor(isc.Canvas.DEFAULT);
        } else {
            this.setCursor(isc.Canvas.MOVE);
        }
        this.showCloseButton = false;

        var cal = this.calendar;
        styleName = styleName || cal.getIndicatorCanvasStyle(event, this.calendarView);
        this.setEventStyle(styleName, headerStyle, bodyStyle);
    },
    click : function () {
        // fire calendar.indicatorClick() if it's there
        if (this.calendar.indicatorClick) this.calendar.indicatorClick(this.event, this.calendarView.viewName)
    },
    getHoverHTML : function () {
        return this.calendar._getIndicatorHoverHTML(this.event, this, this.calendarView);
    },
    checkStyle : function () {
        // no-op
    },
    updateRolloverControls : function () {
    }
});




//>    @class Timeline
// Timeline is a trivial subclass of +link{Calendar} that configures the Calendar with settings
// typical for a standalone timeline view: hides the +link{calendar.dayView, day},
// +link{calendar.weekView, week} and +link{calendar.monthView, month} tabs and the
// +link{calendar.controlsBar, controls bar} by default.
// <P>
// Note that the +link{group:loadingOptionalModules, Calendar module} must be loaded to make
// use of the Timeline class.
//
// @inheritsFrom Calendar
// @treeLocation  Client Reference/Calendar
// @visibility external
//<
isc.ClassFactory.defineClass("Timeline", "Calendar");

isc.Timeline.addProperties({

showTimelineView: true,
showDayView: false,
showWeekView: false,
showMonthView: false,
showControlBar: false,

labelColumnWidth: 75,

sizeEventsToGrid: false,
eventDragGap: 0,

// by default, don't overlap events in Timelines by a percentage
eventOverlap: false

});
isc._debugModules = (isc._debugModules != null ? isc._debugModules : []);isc._debugModules.push('Calendar');isc.checkForDebugAndNonDebugModules();isc._moduleEnd=isc._Calendar_end=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc.Log&&isc.Log.logIsInfoEnabled('loadTime'))isc.Log.logInfo('Calendar module init time: ' + (isc._moduleEnd-isc._moduleStart) + 'ms','loadTime');delete isc.definingFramework;if (isc.Page) isc.Page.handleEvent(null, "moduleLoaded", { moduleName: 'Calendar', loadTime: (isc._moduleEnd-isc._moduleStart)});}else{if(window.isc && isc.Log && isc.Log.logWarn)isc.Log.logWarn("Duplicate load of module 'Calendar'.");}
/*

  SmartClient Ajax RIA system
  Version v13.0p_2023-11-09/LGPL Deployment (2023-11-09)

  Copyright 2000 and beyond Isomorphic Software, Inc. All rights reserved.
  "SmartClient" is a trademark of Isomorphic Software, Inc.

  LICENSE NOTICE
     INSTALLATION OR USE OF THIS SOFTWARE INDICATES YOUR ACCEPTANCE OF
     ISOMORPHIC SOFTWARE LICENSE TERMS. If you have received this file
     without an accompanying Isomorphic Software license file, please
     contact licensing@isomorphic.com for details. Unauthorized copying and
     use of this software is a violation of international copyright law.

  DEVELOPMENT ONLY - DO NOT DEPLOY
     This software is provided for evaluation, training, and development
     purposes only. It may include supplementary components that are not
     licensed for deployment. The separate DEPLOY package for this release
     contains SmartClient components that are licensed for deployment.

  PROPRIETARY & PROTECTED MATERIAL
     This software contains proprietary materials that are protected by
     contract and intellectual property law. You are expressly prohibited
     from attempting to reverse engineer this software or modify this
     software for human readability.

  CONTACT ISOMORPHIC
     For more information regarding license rights and restrictions, or to
     report possible license violations, please contact Isomorphic Software
     by email (licensing@isomorphic.com) or web (www.isomorphic.com).

*/

