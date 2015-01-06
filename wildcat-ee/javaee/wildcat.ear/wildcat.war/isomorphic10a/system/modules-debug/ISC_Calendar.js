
/*

  SmartClient Ajax RIA system
  Version v10.0p_2015-01-04/LGPL Deployment (2015-01-04)

  Copyright 2000 and beyond Isomorphic Software, Inc. All rights reserved.
  "SmartClient" is a trademark of Isomorphic Software, Inc.

  LICENSE NOTICE
     INSTALLATION OR USE OF THIS SOFTWARE INDICATES YOUR ACCEPTANCE OF THE
     SOFTWARE LICENSE AGREEMENT. If you have received this file without an 
     Isomorphic Software license file, please see:

         http://www.isomorphic.com/licenses/license-sisv.html

     You are not required to accept this agreement, however, nothing else
     grants you the right to copy or use this software. Unauthorized copying
     and use of this software is a violation of international copyright law.

  PROPRIETARY & PROTECTED MATERIAL
     This software contains proprietary materials that are protected by
     contract and intellectual property law. YOU ARE EXPRESSLY PROHIBITED
     FROM ATTEMPTING TO REVERSE ENGINEER THIS SOFTWARE OR MODIFY THIS
     SOFTWARE FOR HUMAN READABILITY.

  CONTACT ISOMORPHIC
     For more information regarding license rights and restrictions, or to
     report possible license violations, please contact Isomorphic Software
     by email (licensing@isomorphic.com) or web (www.isomorphic.com).

*/

if(window.isc&&window.isc.module_Core&&!window.isc.module_Calendar){isc.module_Calendar=1;isc._moduleStart=isc._Calendar_start=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc._moduleEnd&&(!isc.Log||(isc.Log && isc.Log.logIsDebugEnabled('loadTime')))){isc._pTM={ message:'Calendar load/parse time: ' + (isc._moduleStart-isc._moduleEnd) + 'ms', category:'loadTime'};
if(isc.Log && isc.Log.logDebug)isc.Log.logDebug(isc._pTM.message,'loadTime');
else if(isc._preLog)isc._preLog[isc._preLog.length]=isc._pTM;
else isc._preLog=[isc._pTM]}isc.definingFramework=true;

if (window.isc && isc.version != "v10.0p_2015-01-04/LGPL Deployment") {
    isc.logWarn("SmartClient module version mismatch detected: This application is loading the core module from "
        + "SmartClient version '" + isc.version + "' and additional modules from 'v10.0p_2015-01-04/LGPL Deployment'. Mixing resources from different "
        + "SmartClient packages is not supported and may lead to unpredictable behavior. If you are deploying resources "
        + "from a single package you may need to clear your browser cache, or restart your browser."
        + (isc.Browser.isSGWT ? " SmartGWT developers may also need to clear the gwt-unitCache and run a GWT Compile." : ""));
}




//> @class CalendarView
// CalendarView is a base class, extended by the various views available in a
// +link{class:Calendar, Calendar}.
//
// @treeLocation Client Reference/Calendar
// @visibility calendar
//<
isc.ClassFactory.defineClass("CalendarView", "ListGrid");

isc.CalendarView.addProperties({
    verticalEvents: true,

    // needed to avoid the grid scrolling to 0,0 when clicking body children (eventCanvases)
    hiliteRowOnFocus: false,

    canHover: true,
    showHover: null,

    // prevent mouseUp/mouseDown from bubbling beyond the CalendarView
    mouseUp : function () {
        return isc.EH.STOP_BUBBLING;
    },
    mouseDown : function () {
        return isc.EH.STOP_BUBBLING;
    },


    canFreezeFields: false,

    initWidget : function () {
        var cal = this.calendar;
        var showHover = this.showHover;
        if (showHover == null) showHover = cal.showViewHovers;
        this.setShowHover(showHover);
        this.Super("initWidget", arguments);
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
        return this.calendar.getCurrentViewName() == this.viewName;
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

    //> @method calendarView.rebuild()
    // Rebuild this CalendarView, including a data refresh.
    // @visibility external
    //<
    rebuild : function (refreshData) {
        if (refreshData == null) refreshData = true;
        if (this._rebuild) this._rebuild(refreshData);
        else if (this.rebuildFields) this.rebuildFields();
        else this.refreshEvents();
    },



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

    hasLanes : function () {
        return this.isTimelineView() || (this.isDayView() && this.calendar.showDayLanes);
    },
    hasSublanes : function () {
        return this.calendar.useSublanes && this.hasLanes();
    },

    getEventCanvasStyle : function (event) {
        if (this.hasLanes()) {
            var cal = this.calendar,
                lnField = cal.laneNameField,
                slnField = cal.sublaneNameField,
                styleField = cal.eventStyleNameField,
                lane = this.getLane(event[lnField]),
                sublane = lane ? this.getSublane(lane[lnField], event[slnField]) : null
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

    // override mouseMove to fire a notification when the snapDate under the mouse changes
    mouseMove : function () {
        var cal = this.calendar,
            lastDate = this._lastMouseDate,
            mouseTarget = isc.EH.lastEvent.target,
            rowNum = (mouseTarget != this.body ? this.frozenBody && this.frozenBody.lastMouseOverRow :
                this.body && this.body.lastMouseOverRow),
            colNum = (mouseTarget != this.body ? this.frozenBody && this.frozenBody.lastMouseOverCol :
                (this.body && this.body.lastMouseOverCol) + (this.frozenFields ? this.frozenFields.length : 0)),
            mouseDate = this.getDateFromPoint()
        ;
        this._lastMouseTarget = mouseTarget;
        cal._mouseMoved(this, mouseTarget, mouseDate ? mouseDate.duplicate() : null, lastDate, rowNum, colNum);
        this._lastMouseDate = mouseDate;
        if (this._mouseDown) {
            // cellOver doesn't fire on mouseMove, so call it now to update
            // the drag-selection canvas, if a drag is in progress
            if (this.isTimelineView()) this.cellOver();
        }
        return true;
    },

// cell hovers
    hoverDelay: 0,
    //hoverStyle: "testStyle",
    getHoverHTML : function () {
        var rowNum = this.getEventRow(),
            colNum = this.getEventColumn(),
            record = this.getRecord(rowNum)
        ;
        var html = this.calendar._getCellHoverHTML(this, record, rowNum, colNum);
        return html;
    },

    getPrintHTML : function (printProperties, callback) {
        if (this.isMonthView()) return this.Super("getPrintHTML", arguments);
        if (callback) {
            this.delayCall("asyncGetPrintHTML", [printProperties, callback]);
            return null;
        } else {
            return this.asyncGetPrintHTML(printProperties, callback);
        }
    },

    asyncGetPrintHTML : function (printProperties, callback) {

        this.__printing = true;

        // force a refresh of ALL events - this will create and draw canvases for any events
        // that haven't yet been scrolled into view
        this.refreshVisibleEvents(null, true, "asyncGetPrintHTML");

        printProperties = isc.addProperties({}, printProperties);

        this.body.printChildrenAbsolutelyPositioned = true;

        var cal = this.calendar,
            isTimeline = this.isTimelineView(),
            isWeek = this.isWeekView(),
            isDay = this.isDayView(),
            isMonth = this.isMonthView()
        ;

        if (isMonth) return;

        var fields = this.getFields(),
            data = this.getData(),
            output = isc.StringBuffer.create(),
            totalWidth = 0,
            fieldWidths = []
        ;

        for (var i=0; i<fields.length; i++) {
            var field = fields[i];
            var button = this.getFieldHeaderButton(field.masterIndex);
            var result = button ? button.width || button.getVisibleWidth() : null;
            if (result == null) result = this.getFieldWidth(field);
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

        output.append("<TABLE cellpadding='0' cellspacing='0' WIDTH=", totalWidth,
            " style='",
            "border: 1px solid grey;'>"
        );

        output.append("<THEAD>");

        if (this.showHeader) {
            // don't generate column-headers for dayView
            output.append(this.getPrintHeaders(0, this.fields.length, fieldWidths));
        }

        output.append("</THEAD>");

        // absolutely position the body and events after the header
        bodyVOffset += this.getHeaderHeight();

        output.append("<TBODY>");

        for (var i=0; i<data.length; i++) {
            var rowHeight = this.getRowHeight(data[i]);
            output.append(rowStart, heightAttr, rowHeight, gt);
            for (var j=0; j<fields.length; j++) {
                var value = this.getCellValue(data[i], i, j);
                output.append("<TD padding=0 class='", this.getCellStyle(data[i], i, j), "' ",
                    "style='margin: 0px; padding: 0px; ",
                    "width:", fieldWidths[j] + "px; ",
                    "border-width: 0px 1px 1px 0px; ",
                    "border-bottom: 1px solid #ABABAB; border-right: 1px solid #ABABAB; ",
                    "border-top: none; border-left: none;'>"
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
            if (!isValid) continue;
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

    getPrintHeaders : function (startCol, endCol, fieldWidths) {

        var defaultAlign = (this.isRTL() ? isc.Canvas.LEFT : isc.Canvas.RIGHT),
            //printHeaderStyle = this.printHeaderStyle || this.headerBaseStyle,
            // printing header-levels and fields with a headerButton style looks much better
            printHeaderStyle = this.headerBaseStyle,
            rowHeight = this.getHeaderHeight(),
            HTML
        ;

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
                HTML[HTML.length] = "<TR HEIGHT=23>";

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
                    HTML[HTML.length] = "style='margin: 0px; padding: 0px; " +
                        "width:" + fieldWidths[entry.masterIndex] + "px; height:23px; " +
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

            HTML = ["<TR HEIGHT=23>"];

            var cellStartHTML = ["<TD CLASS='", printHeaderStyle, "' ALIGN="].join(""),
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
                    HTML.addList([cellStartHTML, align, " style='width:" + width + "px; padding:0px; margin:0px;'>",
                        this.getHeaderButtonTitle(field.masterIndex), "</TD>"]);
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

    eventDragTargetDefaults: {
        _constructor: "Canvas",
        border: "1px dashed red",
        width:1, height: 1,
        snapToGrid: false,
        autoDraw: false,
        moveWithMouse: false,
        dragAppearance: "target",
        dragTarget: this,
        visibility: "hidden",
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
        fillOverlapSlots: true,
        positionToEventCanvas : function (show) {
            var canvas = this.eventCanvas,
                cal = canvas.calendar,
                view = this.view,
                left = view.getEventLeft(canvas.event) + this.getEventPadding(),
                top = canvas.getTop(),
                width = canvas.getVisibleWidth(),
                height = canvas.getVisibleHeight(),
                props = canvas._dragProps
            ;

            if (this.fillOverlapSlots) {
                // cause the drag rect to fill the column's width, or the row's height - if
                // there are sublanes, have the rect fill the sublane height or width
                if (view.isTimelineView()) {
                    var row = view.getEventRow(top);
                    top = view.getRowTop(row);
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
                    var col = view.body.getEventColumn(left);
                    left = view.body.getColumnLeft(col);
                    if (props.useLanes) {
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

            if (view.shouldShowDragHovers()) isc.Hover.show(this.getHoverHTML(), this.hoverProps);
        },
        moveToEvent : function () {
            // no-op here to avoid automatic snapping to the wrong place
        },
        dragRepositionStart : function () {
            var canvas = this.eventCanvas,
                event = canvas.event,
                cal = canvas.calendar,
                view = this.view,
                gr = view.body
            ;

            // canDragEvent() also calls canEditEvent(), which checks both event and calendar
            if (!cal.canDragEvent(event)) return false;

            this._repositioning = true;

            var eventRow = gr.getEventRow(),
                rowTop = gr.getRowTop(eventRow),
                rowHeight = gr.getRowHeight(eventRow),
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
            dp._startOffsetY = offsetY;

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

            dp._useLanes = view.hasLanes() && !canvas.isIndicatorCanvas && !canvas.isZoneCanvas;
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

            if (props._useLanes) {
                //if (isTL) {
                    // handle top/height snapping for lanes and sublanes in timelines
                    var mouseLane = view.getLaneFromPoint(),
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
                        var laneRecordIndex = view.getLaneIndex(mouseLane[cal.laneNameField]);
                        fixedLeft = view.body.getColumnLeft(laneRecordIndex);
                        fixedWidth = view.getLaneWidth(mouseLane[cal.laneNameField]);
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
                snapTop = isTL ? rowTop : Math.min(props._maxTop, rowTop + snapY),
                oldHeight = this.getVisibleHeight(),
                newHeight = oldHeight
            ;

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

            var rightColNum = gr.getEventColumn(eventRight - 1);

            if (rightColNum < 0) {
                this.moveTo(props._previousLeft, snapTop);
                return isc.EH.STOP_BUBBLING;
            }


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

            var tempTop = Math.max(0, (fixedTop >= 0 ? fixedTop : snapTop)),
                tempBottom = Math.min(view.body.getScrollHeight(), tempTop + props._startHeight),
                dropStart = view.getDateFromPoint(eventLeft+1, tempTop),

                dropEnd = view.getDateFromPoint(eventRight - (!view.isTimelineView() ? 1 : 0), tempBottom)
            ;

            if (view.isDayView() || view.isWeekView()) {
                // for vertical views, check if the dropEnd date is different - this indicates
                // a drop at the very bottom of the calendar - use the end of the dropStart
                // day instead
                if (dropStart.getDate() != dropEnd.getDate()) {
                    dropEnd = isc.DateUtil.getEndOf(dropStart, "d");
                }
            }


            var testEndDate = dropEnd.duplicate();
            testEndDate.setTime(dropEnd.getTime()-1);

            var allowDrop = true;
            // fire the cancellable notification method before actually moving the dragTarget
            var newEvent = cal.createEventObject(event, dropStart, testEndDate,
                    mouseLane && mouseLane[cal.laneNameField],
                    mouseSublane && mouseSublane[cal.laneNameField])
            ;
            // the default implementation of this method checks disabled dates
            allowDrop = cal.eventRepositionMove(event, newEvent, this);



            if (sizeToLane) {
                if (isTL) {
                    tempTop = fixedTop;
                    props._previousHeight = fixedHeight;
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

            props._lastStartDate = dropStart.duplicate();
            props._lastEndDate = dropEnd.duplicate();

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
            var canvas = this.eventCanvas,
                props = canvas._dragProps,
                cal = canvas.calendar,
                view = this.view,
                gr = view.body,
                event = canvas.event
            ;

            // hide the manual dragTarget before calling the cancellable timelineEventMoved()
            if (view.shouldShowDragHovers()) isc.Hover.hide();
            this.hide();

            // reset the cursor in case we changed it during a drag
            var cancelDrop = (this.cursor != "default" && cal.eventUseLastValidDropDates != true);
            this.setDragCursor("default");
            if (cancelDrop) return;

            if (canvas.isIndicatorCanvas) {
                var indicator = cal.indicators.find(cal.nameField, event[cal.nameField]);
                indicator[cal.startDateField] = props._lastValidStartDate;
                // no need to refresh all events - if the indicator is dragged out of the
                // viewport, the grid will scroll and that already refreshes events - here,
                // just the indicators and zones need redrawing, which is much quicker - order
                // is important - see calendar.showIndicatorsInFront
                canvas.calendarView.drawIndicators();
                canvas.calendarView.drawZones();
                return isc.EH.STOP_BUBBLING;
            }

            var canEditLane = props._useLanes && cal.canEditEventLane(event, view),
                canEditSublane = props._useLanes && cal.canEditEventSublane(event, view),
                newLane,
                newSublane
            ;

            if (view.isTimelineView()) {
                if (canEditLane || canEditSublane) {
                    if (canEditLane) newLane = props._lastLane[cal.laneNameField];
                    if (canEditSublane && cal.useSublanes && props._lastSublane) {
                        newSublane = props._lastSublane[cal.laneNameField];
                    }
                }
            } else if (view.isDayView() && cal.showDayLanes) {
                if (canEditLane || canEditSublane) {
                    if (canEditLane) newLane = props._lastLane[cal.laneNameField];
                    if (canEditSublane && cal.useSublanes && props._lastSublane) {
                        newSublane = props._lastSublane[cal.laneNameField];
                    }
                } else return false;
            }

            var dates = [ props._lastValidStartDate.duplicate(), props._lastValidEndDate.duplicate() ];

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

            // build the new event as it would be after the drop
            var newEvent = cal.createEventObject(event, props._lastValidStartDate, props._lastValidEndDate,
                    props._lastLane && props._lastLane[cal.laneNameField],
                    props._lastSublane && props._lastSublane[cal.laneNameField]);


            var continueUpdate = cal.eventRepositionStop(event, newEvent, otherFields, this);

            this._repositioning = false;

            if (continueUpdate != false) {
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
                //isc.logWarn('updating event:' + [dates[0], dates[1]]);
                cal.updateCalendarEvent(event, newEvent);
            }

            delete canvas._dragProps;

            //return false;
            return isc.EH.STOP_BUBBLING;
        },

        // dragTarget_dragResizeStart
        dragResizeStart : function () {
            var canvas = this.eventCanvas,
                event = canvas.event,
                cal = canvas.calendar,
                view = this.view,
                gr = view.body
            ;

            if (!cal.canResizeEvent(canvas.event)) return false;

            this._resizing = true;

            var eventRow = gr.getEventRow(),
                rowTop = gr.getRowTop(eventRow),
                rowHeight = gr.getRowHeight(eventRow),
                eventCol = gr.getEventColumn(),
                colLeft = gr.getColumnLeft(eventCol),
                colWidth = gr.getColumnWidth(eventCol),
                offsetX = gr.getOffsetX() - canvas.getLeft(), // - this.getEventPadding(),
                offsetY = gr.getOffsetY() - canvas.getTop(),
                eventWidth = canvas.getVisibleWidth(),
                hasLanes = view.hasLanes(),
                isTimeline = view.isTimelineView(),
                // leftDrag if its a timeline and offsetX is nearer left than right
                isLeftDrag = isTimeline && (offsetX < eventWidth / 2),
                lane = hasLanes ? view.getLaneFromPoint() : null,
                sublane = lane && cal.useSublanes ? cal.getSublaneFromPoint() : null
            ;

            var props = {
                _useLanes: view.hasLanes(),
                _useSublanes: cal.useSublanes,
                _previousLeft: isTimeline ? view.getDateLeftOffset(cal.getEventStartDate(event))
                    : colLeft + (hasLanes && sublane ? sublane.left : 0),
                _previousRight: canvas.getLeft() + eventWidth,
                _previousTop: isTimeline ? rowTop + (sublane ? sublane.top : 0) : canvas.getTop(),
                _previousHeight: (isTimeline ? (sublane ? sublane.height : lane.height)
                    : canvas.getVisibleHeight()),
                _previousWidth: isTimeline ? canvas.getVisibleWidth()
                    : (sublane ? sublane.width :
                        (lane && view.getLaneWidth ? view.getLaneWidth(event[cal.laneNameField])
                        : colWidth)
                    ),
                _leftDrag: isLeftDrag,
                _rightDrag: isTimeline && !isLeftDrag,
                _bottomDrag: !isTimeline,
                _lastStartDate: cal.getEventStartDate(canvas.event),
                _lastEndDate: cal.getEventEndDate(canvas.event),
                _lastLane: lane,
                _lastSublane: sublane
            };

            if (props._previousTop == -1) {
                //TODO: fix this - event partly off the top of the viewport shows at top:0
                // this is to do with keepInParentRect, of course
                props._previousTop = 0;
                props._previousHeight -= gr.getScrollTop();
            }

            canvas._dragProps = props;
            this.positionToEventCanvas(true);

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
            var snapDate = view.getDateFromPoint();
            if (props._bottomDrag) {
                // day/week view bottom drag - snapDate is new endDate, only height changes -
                // its more natural to use the snapDate AFTER (below) the mouse offset when
                // bottom-dragging, so the drag rect includes the snapDate that's actually
                // under the mouse
                endDate = cal.addSnapGapsToDate(snapDate, view, 1);
                if (endDate.getDate() != startDate.getDate()) {
                    endDate = isc.DateUtil.getEndOf(startDate, "d");
                }
                var bottom = view.getDateTopOffset(endDate);
                height = bottom - top;
            } else if (props._leftDrag) {
                if (!snapDate) snapDate = view.startDate.duplicate();
                // timeline left drag - snapDate is new startDate, only left and width change
                startDate = snapDate;
                var right = left + width;
                if (event[cal.durationField] != null) {
                    var millis = endDate.getTime() - startDate.getTime(),
                        timeUnit = event[cal.durationUnitField],
                        unitMillis = utils.getTimeUnitMilliseconds(timeUnit)
                    ;
                    if (millis % unitMillis != 0) {
                        var units = Math.round(utils.convertPeriodUnit(millis, "ms", timeUnit)),
                        startDate = utils.dateAdd(endDate.duplicate(), timeUnit, units * -1);
                    }
                }
                left = view.getDateLeftOffset(startDate);
                width = (right - left);
            } else {
                // timeline right drag - snapDate is new endDate, only width changes - its more
                // natural to use the snapDate AFTER the mouse offset when right-dragging, so
                // the drag rect includes the snapDate that's actually under the mouse
                if (!snapDate) snapDate = view.endDate.duplicate();
                else snapDate = cal.addSnapGapsToDate(snapDate.duplicate(), view, 1);
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
                        endDate = utils.dateAdd(startDate.duplicate(), timeUnit, units);
                    }
                }
                var tempEvent = {};
                tempEvent[cal.startDateField] = startDate;
                tempEvent[cal.endDateField] = endDate;
                width = view._getEventBreadth(tempEvent);
            }

            /*
            var allowResize = true;
            if (endDate.getTime() <= startDate.getTime()) {
                // invalid endDate, earlier than start date - just disallow - should leave the
                // default minimum size (the eventSnapPixels)
                allowResize = false;
            }
            */

            // call eventResizeMove
            var newEvent = cal.createEventObject(event, startDate, endDate)
            var allowResize = cal.eventResizeMove(event, newEvent, view, props);

            props._lastStartDate = startDate;
            props._lastEndDate = endDate;
            props._previousTop = top;
            props._previousLeft = left;
            props._previousWidth = width;
            props._previousHeight = height;

            this.resizeTo(props._previousWidth, props._previousHeight);
            this.moveTo(props._previousLeft, props._previousTop);

            if (allowResize != false) {
                props._lastValidStartDate = startDate.duplicate();
                props._lastValidEndDate = endDate.duplicate();
                this.setDragCursor("default");
            } else {
                this.setDragCursor("not-allowed");
            }

            if (view.shouldShowDragHovers()) isc.Hover.show(this.getHoverHTML(), this.hoverProps);

            return isc.EH.STOP_BUBBLING;
        },

        setDragCursor : function (newCursor) {
            var cursor = this.getCurrentCursor();
            if (cursor == newCursor) return;
            this.setCursor(newCursor);
            this.view.setCursor(newCursor);
            if (this.view.body) this.view.body.setCursor(newCursor);
            if (this.view.frozenBody) this.view.frozenBody.setCursor(newCursor);
            isc.EH.lastEvent.target.setCursor(newCursor);
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

            // reset the cursor in case we changed it during a drag
            this.setDragCursor("default");

            // hide the dragHover, if there was one, and the manual dragTarget
            if (view.shouldShowDragHovers()) isc.Hover.hide();
            this.hide();

            // build the new event as it would be after the drop
            var newEvent = cal.createEventObject(event, startDate);
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
            }
            newEvent[cal.endDateField] = endDate;

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
        if (this.renderEventsOnDemand && this.refreshVisibleEvents) {
            var _this = this,
                events = this.data
            ;
            if (this._layoutEventId) isc.Timer.clear(this._layoutEventId);
            this._layoutEventId = isc.Timer.setTimeout(function () {
                this._layoutEventId = null;
                //if (!_this._refreshEventsCalled) _this.refreshEvents();
                //else
                _this.refreshVisibleEvents(null, null, "scrolled");
            });
        }
    },

    resized : function () {
        this.Super('resized', arguments);
        //isc.logWarn(this.viewName + " resized:" + [this.isDrawn(), this.calendar.hasData()]);
        if (this.renderEventsOnDemand && this.isDrawn() && this.calendar.hasData()) {
            this.refreshVisibleEvents(null, null, "resized");
        }
    },

    forceDataSort : function (data, ignoreDataChanged) {
        var cal = this.calendar,
            specifiers = []
        ;

        if (this.isTimelineView() || (this.isDayView() && cal.showDayLanes)) {
            specifiers.add({ property: cal.laneNameField, direction: "ascending" });
        }

        if (cal.overlapSortSpecifiers) {
            specifiers.addList(cal.overlapSortSpecifiers);
        } else {
            specifiers.add({ property: cal.startDateField, direction: "ascending" });
        }

        if (ignoreDataChanged || !data) {
            if (!data) data = cal.data;
            cal._ignoreDataChanged = true;
        }

        data.setSort(specifiers);
    },

    findEventsInRange : function (startDate, endDate, lane, data) {
        var cal = this.calendar,
            range = {},
            useLane = lane != null && (this.isTimelineView() || (this.isDayView() && cal.showDayLanes))
        ;
        range[cal.startDateField] = startDate;
        range[cal.endDateField] = endDate;
        if (useLane) range[cal.laneNameField] = lane;

        var events = this.findOverlappingEvents(range, range, false, useLane, data, true);
        return events;
    },

    // realEvent is the actual event object, passed in so that we can exclude
    // it from the overlap tests. paramEvent is an object with date fields  - the third param
    // allows the function to return the realEvent as well
    findOverlappingEvents : function (realEvent, paramEvent, includeRealEvent, useLanes, data, ignoreDataChanged) {
        var cal = this.calendar,
            dataPassed = data != null
        ;

        var events = dataPassed ? data : cal.data;

        if (!dataPassed) this.forceDataSort(events, ignoreDataChanged);

        var results = [],
            length = events.getLength(),
            paramStart = cal.getEventStartDate(paramEvent),
            paramEnd = cal.getEventEndDate(paramEvent),
            dayStart = isc.DateUtil.getStartOf(paramEnd, "d"),
            dayEnd = isc.DateUtil.getEndOf(paramStart, "d")
        ;

        var rangeObj = {};

        var lane = useLanes ? realEvent[cal.laneNameField] : null,
            startIndex = 0;

        if (lane) startIndex = events.findIndex(cal.laneNameField, lane);
        if (startIndex < 0) return results;

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
                    rangeObj[cal.startDateField] = paramStart;
                    rangeObj[cal.endDateField] = paramEnd;
                    if (rangeObj[cal.endDateField].getTime() > this.endDate.getTime()) {
                        rangeObj[cal.endDateField].setTime(this.endDate.getTime()-1)
                    }
                }
            } else {
                if (cal.getEventStartDate(event).getTime() > dayEnd.getTime()) continue;
                if (cal.getEventEndDate(event).getTime() < dayStart.getTime()) continue;
                rangeObj[cal.startDateField] = paramStart;
                rangeObj[cal.endDateField] = paramEnd;
                if (rangeObj[cal.endDateField].getTime() > paramEnd.getTime()) {
                    rangeObj[cal.endDateField].setTime(paramEnd.getTime())
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
        var aStart = a[startField], aEnd = a[endField] || cal.getEventEndDate(a),
            bStart = b[startField], bEnd = b[endField] || cal.getEventEndDate(b)
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
            if (bStart < aEnd && bEnd > aStart) return true;
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

        // use the existing getLaneMap() helper to get visible lanes
        var laneNames = useLanes && cal.lanes ? isc.getKeys(cal.getLaneMap()) : [];

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
            range[cal.startDateField] = cal.getEventStartDate(event)
            range[cal.endDateField] = cal.getEventEndDate(event);
            if (useLanes) range[cal.laneNameField] = range.lane = event[cal.laneNameField];
            range.events = [];

            var overlappers = this.findOverlappingEvents(event, event, true, useLanes, data);
            if (overlappers && overlappers.length > 0) {
                range.totalSlots = overlappers.length;
                var totalSlots = range.totalSlots;
                var localSlots = 1;
                for (var j=0; j<overlappers.length; j++) {
                    var ol = overlappers[j],
                        olStart = cal.getEventStartDate(ol),
                        olEnd = cal.getEventStartDate(ol)
                    ;

                    if (olStart < range[cal.startDateField])
                        range[cal.startDateField] = olStart;
                    if (olEnd > range[cal.endDateField])
                        range[cal.endDateField] = olEnd;

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

        var cal = this.calendar, start = cal.startDateField, end = cal.endDateField,
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
        var isTimeline = this.isTimelineView();

        if (!(isTimeline || (this.isDayView() && this.calendar.showDayLanes))) return;

        var lane = this.getLane(laneName);
        if (isTimeline) {
            this.retagRowEvents(lane, true);
        } else {
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

        var cal = this.calendar,
            row;
        if (isc.isA.Number(rowNum)) {
            row = this.getRecord(rowNum);
        } else {
            row = rowNum;
            rowNum = this.isGrouped ? this.getGroupedRecordIndex() : this.getRecordIndex(row);
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
            var events = this.findEventsInRange(start, end, lane, cal.data);

            // 3) re-tag and render those events
            this.renderEvents(events, (lane != null));
        }
    },

    sortForRender : function (events) {

        var cal = this.calendar,
            specifiers = [];
        if (this.isTimelineView() || (this.isDayView() && cal.showDayLanes)) {
            specifiers.add({ property: cal.laneNameField, direction: "ascending" });
        }
        if (cal.overlapSortSpecifiers) {
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
        var cal = this.calendar,
            isTimeline = this.isTimelineView(),
            visibleLanes = isLane ? (isTimeline ? this.body.getVisibleRows() : this.body.getVisibleColumns()) : [],
            _this = this;
        for (var i=0; i<events.length; i++) {
            var event = events.get(i),
                props = event._overlapProps,
                laneIndex = isLane ? _this.getLaneIndex(event[cal.laneNameField]) : null
            ;
            if (!isLane || (laneIndex >= visibleLanes[0] && laneIndex <= visibleLanes[1])) {
                // size the eventCanvas for each passed event
                var canvas = this.getCurrentEventCanvas(event);
                if (canvas) {
                    canvas.event = event;
                    _this.sizeEventCanvas(canvas, _this);
                }
            }
        };
    },

    //------------------------------------------------------
    // range building and rendering stuff
    //------------------------------------------------------

    sizeEventCanvas : function (canvas, forceRedraw) {
        if (Array.isLoading(canvas.event)) return;

        var cal = this.calendar;
        if (cal == null) return;

        var event = canvas.event,
            isTimeline = this.isTimelineView(),
            isWeekView = this.isWeekView(),
            useLanes = this.hasLanes(),
            startDate = cal.getEventStartDate(event),
            endDate = cal.getEventEndDate(event)
        ;


        if (forceRedraw) canvas.hide();

        var eTop, eLeft, eWidth, eHeight,
            laneIndex = useLanes ? this.getLaneIndex(event[cal.laneNameField]) : null,
            lane = useLanes ? this.getLane(event[cal.laneNameField]) : null
        ;

        if (isTimeline) {
            if (!lane) return;
            eHeight = this.getLaneHeight(lane.name);

            // calculate event width by the offsets of the start and end dates
            eWidth = this._getEventBreadth(event);

            // minWidth is one snapGap, or zeroLengthEventWidth for zero-length duration events
            var minWidth = cal.getSnapGapPixels();
            if (cal.isDurationEvent(event) && cal.getEventDuration(event) == 0) {
                minWidth = cal.zeroLengthEventSize + (cal.getLanePadding(this) * 2);
            }
            eWidth = Math.max(eWidth, minWidth);

            // calculate event left
            eLeft = this.getEventLeft(event);

            eTop = this.getRowTop(laneIndex);

            var padding = cal.getLanePadding(this);
            if (padding > 0) {
                eTop += padding;
                eLeft += padding;
                eWidth -= (padding * 2);
                eHeight -= (padding * 2);
            }

            if (cal.eventsOverlapGridLines) {
                // need to do this even when left is zero, to deal with a border issue
                //if (eLeft > 0) eLeft -= 1;
                eLeft -= 1;
                eWidth += 1;
                eTop -= 1;
                eHeight += 1;
            }

            if (this.eventDragGap > 0) {
                eWidth = Math.max(this.eventDragGap, eWidth - this.eventDragGap);
            }
        } else {
            var colNum;
            if (this.isDayView()) {
                if (cal.showDayLanes) colNum = laneIndex;
                else colNum = 0;
            } else {
                colNum = this.getColFromDate(startDate);
            }
            eLeft = this.body.getColumnLeft(colNum);
            eWidth = this.body.getColumnWidth(colNum);

            var rowSize = this.body.getRowHeight(1),
                // catch the case where the end of the event is on 12am, which happens when an
                // event is dragged or resized to the bottom of the screen
                eHrs = endDate.getHours() == 0
                        && endDate.getDate() != startDate.getDate()
                        ? 24 : endDate.getHours(),

                // if the event ends on the next day, render it as ending on the last hour of the
                // current day
                spansDays = false,
                minsPerRow = cal.getMinutesPerRow(this),
                rowsPerHour = cal.getRowsPerHour(this)
            ;

            if (endDate.getDate() > startDate.getDate()) {
                spansDays = true;
                eHrs = 24;
            }

            eTop = startDate.getHours() * (rowSize * rowsPerHour);

            // each (rowSize * 2) represents one hour, so we're doing (hour diff) * (1 hour height)
            eHeight = (eHrs - startDate.getHours()) * (rowSize * rowsPerHour);

            eHeight -= 1;

            var startMins = startDate.getMinutes();
            if (startMins > 0) {
                var startMinPixels = cal.getMinutePixels(startMins, rowSize, this);
                eHeight -= startMinPixels;
                eTop += startMinPixels;
            }
            if (endDate.getMinutes() > 0 && !spansDays) {
                eHeight += cal.getMinutePixels(endDate.getMinutes(), rowSize, this);
            }

            if (cal.eventsOverlapGridLines) {
                eLeft -= 1;
                eWidth += 1;
                eTop -= 1;
                eHeight += 1;
            }

        }

        if (cal.useSublanes && lane && lane.sublanes) {
            this.sizeEventCanvasToSublane(canvas, lane, eLeft, eTop, eWidth, eHeight);
        } else {
            //if (doDebug) isc.logWarn('sizeEventCanvas:' + [daysFromStart, cal.startDate]);
            this.adjustDimensionsForOverlap(canvas, eLeft, eTop, eWidth, eHeight);
        }

        // set description after resize so percentage widths can be respected in html that may
        // be in the description
        if (canvas.setDescriptionText) {
            //TODO: this is specific to the old eventWindow - get rid of it
            if (cal.showEventDescriptions != false) {
                canvas.setDescriptionText(event[cal.descriptionField]);
            } else {
                canvas.setDescriptionText(event[cal.nameField]);
            }
        } else {
            canvas.markForRedraw();
        }

        if (isTimeline && event != null) {
            // draw leading and trailing lines
            if (event[cal.leadingDateField] && event[cal.trailingDateField]) {
                if (canvas._lines) this.addLeadingAndTrailingLines(canvas);
                // split this onto another thread so that ie doesn't pop the
                // slow script warning. Applies to first draw only.
                else this.delayCall("addLeadingAndTrailingLines", [canvas]);
            }
        }

    },

    /*
    getRowTop : function () {
        var result = this.Super("getRowTop", arguments);
        return result;
    },
    */

    adjustDimensionsForOverlap : function (canvas, left, top, width, height) {
        var cal = this.calendar,
            overlapProps = canvas.event._overlapProps,
            isTimeline = this.isTimelineView(),
            padding = cal.getLanePadding(this)
        ;
        //isc.logWarn('adjustDimForOverlap:' + canvas.event.EVENT_ID + this.echoFull(overlapProps));
        //overlapProps = false;
        if (overlapProps && overlapProps.totalSlots > 0) {
            var slotSize = isTimeline ? Math.floor(height / overlapProps.totalSlots) :
                    Math.floor(width / overlapProps.totalSlots)
            ;
            if (isTimeline) {
                height = slotSize;
                if (overlapProps.slotCount) height *= overlapProps.slotCount;
                if (overlapProps.totalSlots > 1) {
                    height -= Math.floor(padding / (overlapProps.totalSlots));
                }
                top = top + Math.floor((slotSize * (overlapProps.slotNum - 1)));
                if (overlapProps.slotNum > 1) top += (padding * (overlapProps.slotNum-1));
            } else {
                width = slotSize;
                if (overlapProps.slotCount) Math.floor(width *= overlapProps.slotCount);
                if (overlapProps.totalSlots > 1) {
                    //width -= Math.floor(padding / (overlapProps.totalSlots));
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
                    width -= cal.eventDragGap || 1;
                }
            }
            // add a pixel of height to all overlapped events so that their borders are flush
            if (cal.eventsOverlapGridLines) {
                if (isTimeline) {
                    if (overlapProps.totalSlots > 1) height += 1
                } else {
                    height += 1;
                    if (overlapProps.slotNum > 0 && !cal.eventOverlap) {
                        width += 1;
                    }
                }
            }
        }

        canvas.renderEvent(top, left, width, height);
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

    tagDataForOverlap : function (data, lane) {
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

        data.setProperty("_overlapProps", null);
        data.setProperty("_slotNum", null);

        var useLanes = this.isTimelineView() || (this.isDayView() && cal.showDayLanes);

        var olRanges = this.updateOverlapRanges(data);

        var rangeSort = [];
        if (isTimeline || (this.isDayView() && cal.showDayLanes)) {
            rangeSort.add({ property: cal.laneNameField, direction: "ascending" });
        }
        if (cal.overlapSortSpecifiers) {
            rangeSort.addList(cal.overlapSortSpecifiers);
        } else {
            rangeSort.add({ property: cal.startDateField, direction: "ascending" });
            rangeSort.add({ property: "eventLength", direction: "descending" });
        }

        for (var j = 0; j<olRanges.length; j++) {

            var range = olRanges[j];

            var innerData = range.events;

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

    getVisibleDateRange : function (refreshAll) {
        var cal = this.calendar;

        if (refreshAll) {
            return [cal.getVisibleStartDate(this), cal.getVisibleEndDate(this)];
        }

        if (!this.renderEventsOnDemand) {
            if (this.isTimelineView()) {
                return [this.startDate.duplicate(), this.endDate.duplicate()];
            } else if (this.isWeekView()) {
                return [cal.chosenWeekStart, cal.chosenWeekEnd];
            } else if (this.isDayView()) {
                return [cal.chosenDateStart, cal.chosenDateEnd];
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

        if (endRow < 0 || isNaN(endRow)) endRow = this.data.getLength()-1;
        if (endCol < 0 || isNaN(endCol)) {
            if (this.isTimelineView()) {
                endCol = this._dateFieldCount
            } else {
                endCol = this.body.fields.length - 1;
            }
        }

        endCol = Math.min(endCol, this.body.fields.length-1);
        endRow = Math.min(endRow, this.data.length-1);

        var startDate = this.getCellDate(startRow, startCol),
            endDate = this.getCellEndDate ? this.getCellEndDate(endRow, endCol) :
                this.getCellDate(endRow, endCol)
        ;

        //if (endDate.getTime() < startDate.getTime()) endDate = isc.DateUtil.getEndOf(endDate, "D");

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

    // refreshEvents is only called when data changes, etc.
    // refreshVisibleEvents is called whenever the view is scrolled and only draws visible events.
    // see scrolled()
    refreshVisibleEvents : function (events, refreshAll, caller) {
        // bail unless both the view and its body are visible
        if (!this.isDrawn() || !this.isVisible() || !this.body || !this.body.isDrawn()) return;
        // bail if this is a lane-based view but there aren't any lanes (can't render anything)
        if (this.hasLanes() && (!this.lanes || this.lanes.length == 0)) return;
        // if there are no drawnEvents, refreshEvents hasn't been called yet - do that and bail
        if (!this._drawnEvents) {
            this.refreshEvents();
            return;
        }

        //isc.logWarn("refreshVisibleEvents - called from " + caller);

        // get visible events (those in the viewport)
        events = events || this.getVisibleEvents(refreshAll);

        // need to do this to ensure consistent zordering
        this.sortForRender(events);

        var eventsLen = events.getLength();

        var clearThese = this.useEventCanvasPool ? this._drawnEvents.duplicate() : [],
            addThese = []
        ;

        this.logDebug('refreshing visible events','calendar');

        for (var i = 0; i < eventsLen; i++) {
            var event = events.get(i),
                alreadyVisible = this._drawnEvents.contains(event)
            ;

            if (alreadyVisible) {
                // if an event is already in the _drawnEvents array, remove it from the
                // clearThese array
                clearThese.remove(event);
                if (this.isGrouped || this.useEventCanvasPool) {
                    // if the view is grouped or using the eventCanvasPool, reposition and
                    // redraw the canvas
                    var canvas = this.getCurrentEventCanvas(event);
                    this.sizeEventCanvas(canvas, true);
                }
                continue;
            }

            addThese.add(event);
        }

        if (this.isGrouped ||
                (this.useEventCanvasPool && this.eventCanvasPoolingMode == "viewport"))
        {
            // we want to clear eventCanvases that are no longer in the viewport if we're using
            // viewport pooling mode, or if the grid is grouped (a group might have closed)
            for (var i=0; i<clearThese.length; i++) {
                var canvas = this.getCurrentEventCanvas(clearThese[i]);
                if (canvas) this.clearEventCanvas(canvas);
            }
        }

        if (addThese.length > 0) {
            var len = addThese.length;
            for (var i=0; i<len; i++) {
                var event = addThese[i];
                if (!this._drawnEvents.contains(event)) this._drawnEvents.add(event);
                this.addEvent(event, false);
            }
        }

        if (!this.__printing) {
            // redraw any zones and indicators in the background
            this.drawZones();
            this.drawIndicators();
        }

        var cal = this.calendar;

        if (cal.eventsRendered && isc.isA.Function(cal.eventsRendered))
            cal.eventsRendered();
    },

    getVisibleEvents : function (refreshAll) {
        if (!this.renderEventsOnDemand) return cal.data;

        var cal = this.calendar,
            isTimeline = this.isTimelineView(),
            hasDayLanes = cal.showDayLanes && this.isDayView(),
            dateRange = this.getVisibleDateRange(refreshAll),
            useLanes = (isTimeline || hasDayLanes),
            laneRange = useLanes ?
                (isTimeline ? this.getVisibleRowRange() : this.getVisibleColumnRange()) : null
        ;

        var events = cal.data,
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
            if (cal.shouldShowLane(this.getLane(event.lane), this) == false) continue;

            var eventStart = cal.getEventLeadingDate(event) || cal.getEventStartDate(event);
            // bail if there's no startDate
            if (!eventStart) {
                this.logWarn("getVisibleEvents - event has no start-date: " + isc.echoFull(event));
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
                // the range is from hour-start on the start date, to hour-end on the end date
                // but we don't want events that are vertically not in view, so discard events
                // that end before the viewport start time or start after the viewport end-time
                if (eventEnd.getHours() < dateRange[0].getHours()) continue;
                if (eventStart.getHours() > dateRange[1].getHours()) continue;
            }

            // build a range object to compare against
            var rangeObj = {};

            if (useLanes) {
                if (this.isGrouped) {
                    // if grouped, check that the lane is in the openList
                    var index = openList.findIndex(cal.laneNameField, event[cal.laneNameField]);
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

            //sameLaneOnly = useLanes ? !cal.canEditEventLane(event) : false;
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
                if (this._drawnCanvasList) this._drawnCanvasList.remove(canvas);
                if (this._drawnEvents) this._drawnEvents.remove(canvas.event);
                if (this.useEventCanvasPool && !destroy) {
                        this.poolEventCanvas(canvas);
                } else {
                    canvas.destroy();
                    canvas = null;
                }
            }
        }
    },

    clearEvents : function (start, destroy) {
        var pool = this._eventCanvasPool;
        // hide all the canvases in the _eventCanvasPool
        if (!this.body || !this.body.children || !pool) return;
        if (!start) start = 0;
        //isc.logWarn('clearing events');

        if (destroy == null) destroy = !this.useEventCanvasPool;

        var list = this._drawnCanvasList,
            len = list.length
        ;

        while (--len >= 0) {
            //isc.logWarn('hiding event:' + i);
            if (list[len]) {
                if (list[len]._availableForUse) {
                    this.clearEventCanvas(list[len], destroy);
                }
            }
        }

        list.removeEmpty();
    },

    areSame : function (first, second) {
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
        var eventCanvasID = this.calendar.getEventCanvasID(this, event);
        var canvas = window[eventCanvasID];
        return canvas;
    },

    poolEventCanvas : function (canvas) {
        if (!this._eventCanvasPool) this._eventCanvasPool = [];
        if (this.body) {
            if (canvas.event) {
                this.calendar.setEventCanvasID(this, canvas.event, null);
                canvas.event = null;
            }
            canvas._availableForUse = true;
            if (this._drawnCanvasList) this._drawnCanvasList.remove(canvas);
            if (!this._eventCanvasPool.contains(canvas)) this._eventCanvasPool.add(canvas);
            return true;
        } else return false;
    },
    getPooledEventCanvas : function (event) {
        if (!this._eventCanvasPool) this._eventCanvasPool = [];
        if (!this.body) return;
        var pool = this._eventCanvasPool,
            cal = this.calendar,
            canvas
        ;
        if (pool.length > 0) {
            // reclaim an event from the eventCanvas pool
            var index = pool.findIndex("_availableForUse", true);
            if (index < 0) return null;
            canvas = pool[index];
            canvas._availableForUse = false;
            cal.setEventCanvasID(this, event, canvas.ID);
            pool.remove(canvas);
        }
        return canvas;
    },

    addEvent : function (event, retag) {
        if (!this._drawnCanvasList) this._drawnCanvasList = [];
        if (!this._eventCanvasPool) this._eventCanvasPool = [];

        // clear any cell selection that has been made
        this.clearSelection();

        //if (!this._localEvents.contains(event)) this._localEvents.add(event);

        var cal = this.calendar,
            canvas = cal._getEventCanvas(event, this),
            hideWindow = false
        ;

        if (canvas.isDrawn()) canvas.hide();

        if (!this._drawnCanvasList.contains(canvas)) this._drawnCanvasList.add(canvas);

        canvas._isWeek = this.isWeekView();

        if (this.isDayView() && cal.showDayLanes) {
            // don't show the eventWindow if it's lane isn't visible
            var laneName = event[cal.laneNameField],
                lane = this.lanes.find("name", laneName)
            ;
            if (!lane) hideWindow = true;
        }

        var canEdit = cal.canEditEvent(event);
        canvas.setDragProperties(canEdit, canEdit, this.eventDragTarget);

        if (!hideWindow && this.body && this.body.isDrawn()) {
            // if the "retag" param was passed, this is an event that hasn't been rendered
            // before (it comes from processSaveResponse() after an "add" op) - rather than
            // just resizing the window, get a list of overlapRanges that intersect the new
            // event, combine the event-list from each of them and add the new event,
            // remove the existing ranges and then retag the event-list
            if (retag) {
                if (this.body) this.body.addChild(canvas);
                this.retagOverlapRange(cal.getEventStartDate(event),
                        cal.getEventEndDate(event), event[cal.laneNameField]);
            } else {
                // add the eventCanvas to the body before rendering it, so that bringToFront()
                // behaves as expected
                if (this.body) this.body.addChild(canvas);
                this.sizeEventCanvas(canvas);
                canvas.bringToFront();
            }
        }
    },

    removeEvent : function (event) {
        var canvas = this.getCurrentEventCanvas(event);
        if (canvas) {
            this.clearEventCanvas(canvas, !this.useEventCanvasPool);
            return true;
        } else {
            return false;
        }
    },

    clearZones : function () {
        var zones = this._zoneCanvasList || [];
        for (var i=0; i<zones.length; i++) {
            if (zones[i]) {
                this.body.removeChild(zones[i]);
                if (zones[i].destroy()) zones[i].destroy();
                zones[i] = null;
            }
        }
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
            this.body.addChild(canvas)
            canvas.renderEvent(0, left, right-left, height, true);
            canvasList.add(canvas);
        }
    },

    clearIndicators : function () {
        var indicators = this._indicatorCanvasList || [];
        for (var i=0; i<indicators.length; i++) {
            if (indicators[i]) {
                this.body.removeChild(indicators[i]);
                if (indicators[i].destroy()) indicators[i].destroy();
                indicators[i] = null;
            }
        }
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
            delete indicator.endDate;
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
            this.body.addChild(canvas)

            canvas.renderEvent(0, left, cal.zeroLengthEventSize, height, !cal.showIndicatorsInFront);
            canvasList.add(canvas);
        }
    },

    refreshEvents : function () {
        if (this._refreshingEvents) return;

        this._refreshEventsCalled = true;
        if (!this._drawnCanvasList) this._drawnCanvasList = [];
        if (!this._drawnEvents) this._drawnEvents = [];

        var cal = this.calendar;
        // bail if the grid hasn't been drawn yet, or hasn't any data yet
        if (!this.body || !cal.hasData()) return;

        // flag to prevent setLanes() from calling back through this method
        this._refreshingEvents = true;

        // clear any zones and indicators (so they don't prevent the body from shrinking)
        this.clearZones();
        this.clearIndicators();

        // set all the canvases as availableForUse:true so that clearEvents pools them
        var arr = this._drawnCanvasList;
        if (arr.length > 0) {
            arr.setProperty("_availableForUse", true);
            // pool or destroy eventCanvases created since the last refreshEvents()
            this.clearEvents(0, !this.useEventCanvasPool);
        }
        // reset the lists of drawn events and canvases - they're either destroyed or pooled now
        this._drawnEvents = [];
        this._drawnCanvasList = [];

        var startDate = cal.getVisibleStartDate(this),
            startMillis = startDate.getTime(),
            endDate = cal.getVisibleEndDate(this),
            endMillis = endDate.getTime()
        ;

        this.overlapRanges = [];


        var eventsLen = cal.data.getLength();
        var allEvents = cal.data.getRange(0, eventsLen);

        var events = [];

        var propsName = this.viewName + "Props";

        while (--eventsLen >= 0) {
            var event = allEvents.get(eventsLen);
            if (!event) continue;
            if (!isc.isA.String(event)) {
                // if shouldShowEvent() is implemented and returns false, skip the event
                if (cal.shouldShowEvent(event, this) == false) continue;
                if (!cal.getEventStartDate(event)) {
                    this.logWarn("refreshEvents - event has no start-date: " + isc.echoFull(event));
                    continue;
                }
                var sDate = cal.getEventLeadingDate(event) || cal.getEventStartDate(event),
                    sTime = sDate.getTime(),
                    eDate = cal.getEventTrailingDate(event) || cal.getEventEndDate(event),
                    eTime = eDate.getTime()
                ;
                if ((sTime >= startMillis && sTime < endMillis) ||
                    (eTime > startMillis && eTime <= endMillis))
                {
                    // this event can be reached using the scrollbar (as opposed to the next
                    // and previous buttons), so we'll include it in _localEvents - store its
                    // row/col - we'll use this to avoid some calculations later (specifically,
                    // calls to getScrollHeight/Top which aren't especially fast)
                    //var props = event[propsName] || {};
                    //props.colNum = this.getColFromDate(sDate);
                    //props.endColNum = this.getColFromDate(eDate);
                    //event[propsName] = props;
                    // add this later to save time on rebuilding the events PKs every time
                    //if (!event._internalKey) event._internalKey = cal.getEventKey(event);
                    event.eventLength = (eDate - sDate);
                    if (event[cal.durationField] != null) {
                        //event[cal.endDateField] = eDate;
                        event.isDuration = true;
                        event.isZeroDuration = event[cal.durationField] == 0;
                    }
                    //event[propsName] = props;
                    events.add(event);
                }
            }
        };

        this.tagDataForOverlap(events);

        if (this.hasLanes() && cal.lanes) {
            var len = cal.lanes.length,
                visibleLanes = [],
                shouldRedraw = false
            ;
            // hide any lanes that shouldn't be shown (default impl. of shouldShowLane() just tests
            // for calendar.hideUnusedLanes being true and returns false for lanes with no events
            for (var i=0; i<len; i++) {
                var record = cal.lanes[i];
                if (this.isGroupNode(record)) continue;
                if (cal.shouldShowLane(record)) {
                    visibleLanes.add(record);
                    shouldRedraw = true;
                }
            }
            if (shouldRedraw && (!this.lanes || this.lanes.length != visibleLanes.length)) {
                this.setLanes(visibleLanes, true);
                this.redraw();
            }
        }

        // redraw the events
        this.refreshVisibleEvents(null, null, "refreshEvents");

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
            cal.data.invalidateCache();
            cal.fetchData(cal.getNewCriteria(this));
        } else {
            // force dataChanged hooks to fire so event positions are correctly updated
            cal.dataChanged();
        }
    }

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
    childrenSnapToGrid: false
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

    // disable autoFitting content on header double clicking
    canAutoFitFields : false,

    canSelectCells:true,

    initWidget : function () {
        this.fields = [];

        var cal = this.calendar;

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

        if (isc.isAn.Array(cal.data)) {
            this._refreshEventsOnDraw = true;
            this._ignoreDataChanged = true;
            //this.refreshEvents();
        }

        this.rebuildFields();

        this.addAutoChild("eventDragTarget");
        this.body.addChild(this.eventDragTarget);
        this.dragTarget = this.eventDragTarget;
    },

    getFirstDateColumn : function () {
        return this.frozenBody ? this.frozenBody.fields.length : 0;
    },
    getCellValue : function (record, recordNum, fieldNum) {
        var firstDateCol = this.getFirstDateColumn();
        if (fieldNum >= firstDateCol) return null;
        return this.Super("getCellValue", arguments);
    },

    reorderFields : function (start, end, moveDelta) {
        this.Super("reorderFields", arguments);
        this.refreshEvents();
    },

    rebuildFields : function () {
        var cal = this.calendar,
            fields = [],
            labelCol = {
                width: this.labelColumnWidth,
                name: "label",
                isLabelField: true,
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

        if (this.hasLanes()) {
            var lanes = this.lanes = this.lanes || cal.lanes.duplicate() || [];
            fields[0].frozen = true;
            var d = cal.chosenDate.duplicate(),
                scaffolding = isc.DaySchedule._getEventScaffolding(cal, this, d),
                nDate = isc.Date.createLogicalDate(d.getFullYear(), d.getMonth(), d.getDate()),
                props = { date: nDate, align: "center", canReorder: cal.canReorderLanes }
            ;
            for (var i=0; i<lanes.length; i++) {
                var lane = lanes[i],
                    laneName = lane.name || lane[cal.laneNameField],
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
            var scaffoldingStartDate = cal.chosenDate;
            fields[0].frozen = true;
            fields.add({name: "day1", align: "center", date: cal.chosenDate});
            if (this.isWeekView()) {
                var numDays = 8;
                for (var i = 2; i < numDays; i++) {
                    fields.add({name: "day" + i, align: "center" } );
                }
                this.setShowHeader(true);

                // hide weekends
                if (!cal.showWeekends) {
                    var start = this.showLabelColumn && this.labelColumnPosition == "left" ? 1 : 0;

                    var weekendDays = Date.getWeekendDays();
                    for (var i = start; i < fields.length; i++) {

                        var adjDay = ((i - start) + cal.firstDayOfWeek) % 7;
                        //isc.logWarn('here:' + [i, adjDay]);
                        if (weekendDays.contains(adjDay)) {
                            fields[i].showIf = "return false;";
                        }
                    }
                }
                scaffoldingStartDate = this.chosenWeekStart;
            } else {
                this.setShowHeader(false);
            }
            this.data = isc.DaySchedule._getEventScaffolding(cal, this, this.scaffoldingStartDate);
        }
        if (this.showLabelColumn && this.labelColumnPosition == "right") {
            fields.add(labelCol);
        }

        this.setFields(fields);
    },

    getDateFromPoint : function (x, y, round, useSnapGap) {
        var cal = this.calendar;

        if (useSnapGap) {
            // when click/drag creating, we want to snap to the eventSnapGap
            //y -= y % cal.getSnapGapPixels();
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
            offsetY = y - rowTop,
            eventSnapPixels = cal.getSnapGapPixels(),
            pixels = offsetY - (offsetY % eventSnapPixels),
            snapGapMins = minsPerRow / (rowHeight / eventSnapPixels),
            snapGaps = pixels / eventSnapPixels,
            minsToAdd = snapGapMins * snapGaps
        ;

        colDate.setMinutes(colDate.getMinutes() + minsToAdd);

        return colDate;
    },

    getCellDate : function (rowNum, colNum) {
        if (!(this.body && this.body.fields) || !this._cellDates) return null;

        // use the last row if invalid rowNum passed
        if (rowNum < 0) rowNum = this.data.getLength() - 1;

        // return the cell date from the array built by _getCellDates()
        var fieldName = this.isDayView() ? "day1" : this.body.fields[colNum][this.fieldIdProperty];
        if (!fieldName.startsWith("day")) return;
        return this._cellDates[rowNum][fieldName].duplicate();
    },

    getEventLeft : function (event) {
        var col = this.getColFromDate(this.calendar.getEventStartDate(event));
        return this.body.getColumnLeft(col);
    },
    getEventRight : function (event) {
        var col = this.getColFromDate(this.calendar.getEventEndDate(event));
        return this.body.getColumnLeft(col) + this.body.getColumnWidth(col);
    },

    // get the left offset of a date in this view - will either be zero (dayView) or the
    // getColumnLeft() of the day column containing the date
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

    // get the top offset of a date in this view - will be the top of the row that contains
    // the date, plus any snapGap heights within the row
    getDateTopOffset : function (date) {
        if (!date) return null;
        var cal = this.calendar,
            eventSnapPixels = cal.getSnapGapPixels(),
            millis = date.getTime(),
            col = this.getColFromDate(date),
            len = this.data.length
        ;
        for (var i=0; i<=len; i++) {
            var rDate = this.getCellDate(i, col),
                rMillis = rDate.getTime()
            ;
            if (rMillis >= millis) {
                // found the first later row - use the previous one, get its top and add extra
                // minutes for the snapGap
                var rowNum = i - (i == 0 ? 0 : 1),
                    top = this.getRowTop(rowNum),
                    rowHeight = this.getRowHeight(rowNum)
                ;
                if (rowHeight / eventSnapPixels != 1) {
                    var millisDelta = rMillis - millis,
                        mins = cal.getMinutesPerRow() - Math.floor(millisDelta / 1000 / 60),
                        snapGapMins = cal.getSnapGapMinutes(this),
                        extraPixels = Math.floor((mins / snapGapMins) * eventSnapPixels)
                    ;
                    top += extraPixels;
                } else {
                    top += rowHeight;
                }
                return top;
            }
        }

        // the passed time must be in the last row
        return this.body.getScrollHeight() - 1;
    },

    setLanes : function (lanes) {
        this.lanes = lanes.duplicate();
        this.rebuildFields();
        this.refreshEvents();
    },
    getLane : function (lane) {
        var index = isc.isA.Number(lane) ? lane : -1;
        if (index == -1) {
            if (isc.isAn.Object(lane)) index = this.body.fields.indexOf(lane);
            else if (isc.isA.String(lane)) index = this.getLaneIndex(lane);
        }
        if (index >= 0) return this.body.fields[index];
    },
    getLaneIndex : function (lane) {
        if (!this.isDayView() || !this.creator.showDayLanes) return;
        var fields = this.body.fields,
            index = -1;
        if (isc.isAn.Object(lane)) index = fields.indexOf(lane)
        else if (isc.isA.String(lane)) {
            index = fields.findIndex("name", lane);
            if (index < 0) index = fields.findIndex(this.creator.laneNameField, lane);
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
        if (!this.hasLanes()) return null;
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

        this.body.addChild(this.eventDragTarget);
        this.eventDragTarget.setView(this);

        /*
        if (this.isDayView() && this.calendar.scrollToWorkday) {
            var newRowHeight = this.calcRowHeight();
            if (newRowHeight != this.calendar.rowHeight) {
                this.calendar.setRowHeight(newRowHeight);
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
        var snapGap = this.creator.getSnapGapPixels();
        this.body.snapVGap = Math.round((snapGap / this.creator.getMinutesPerRow(this))
                * this.body.getRowSize(0));
        this.body.snapHGap = null;
    },

    // To be used with calendar.scrollToWorkday
    scrollToWorkdayStart : function () {
        var cal = this.calendar;

        if (this._scrollingToWorkday) return;

        if (cal.scrollToWorkday && !this.hasLanes()) {
            var newRowHeight = this.calcRowHeight();
            if (newRowHeight != cal.rowHeight) {
                this._scrollingToWorkday = true;
                cal.setRowHeight(newRowHeight, true);
                delete this._scrollingToWorkday;
            }
        }

        var range = this.getWorkdayRange(),
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
            cal = this.calendar,
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
            cellHeight = this.calendar.rowHeight
        ;
        // if workdayStart > workdayEnd, just return default cellHeight
        if (workdayLen <= 0) return cellHeight;
        var rHeight = Math.ceil(this.body.getViewportHeight() /
                (workdayLen * this.calendar.getRowsPerHour())) - 1;
        return rHeight < cellHeight ? cellHeight : rHeight;
    },
    getRowHeight : function (record, rowNum) {
        // when scrollToWorkday is true, the rowHeight/cellHeight has already been re-calculated,
        // so just return it - causes issues with the frozen body if this method returns a different
        // number than the current cellHeight
        return this.calendar.rowHeight;
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

    getColFromDate : function (date) {
        for (var i=0; i<this.body.fields.length; i++) {
            var fld = this.body.fields.get(i);
            if (!fld.date) continue;
            //if (fld._yearNum == null || fld._monthNum == null || fld._dateNum == null) continue;
            //var newDate = new Date(fld._yearNum, fld._monthNum, fld._dateNum);
            if (isc.Date.compareLogicalDates(date, fld.date) == 0) return i;
        }
        return null;
    },

    isLabelCol : function (colNum) {
        return this.completeFields[colNum] && this.completeFields[colNum].date == null;
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
        cal.dateChooser.dateClick(fld._yearNum, fld._monthNum, fld._dateNum);
        cal.selectTab(0);
        return true;
    },


    getCellAlign : function (record, rowNum, colNum) {
       return this.labelColumnAlign;
    },

    cellMouseDown : function (record, rowNum, colNum) {
        if (this.isLabelCol(colNum) || this.cellDisabled(rowNum, colNum)) return true;

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
        // if Browser.isTouch, don't allow long events to be created by dragging
        if (!this.calendar.canDragCreateEvents) return;
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
        var newEvent = cal.createEventObject(null, startDate, endDate,
            lane && lane[cal.laneNameField], sublane && sublane[cal.laneNameField]
        );
        cal.showEventDialog(newEvent, true);
        return isc.EH.STOP_BUBBLING;
    },

    getCellStyle : function (record, rowNum, colNum) {
        var cal = this.calendar,
            bStyle = this.getBaseStyle(record, rowNum, colNum)
        ;

        if (this.isLabelCol(colNum)) return bStyle;
        if (this.cellDisabled(rowNum, colNum)) return bStyle + "Disabled";

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
        if (!this.isWeekView() && this.alternateRecordStyles && rowNum % 2 != 0) {
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
        var cal = this.calendar,
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
            if (child) {
                this.body.removeChild(child);
                child.destroy();
                child = null;
            }
        }
        this._drawnEvents = null;
        this._drawnCanvasList = null;
        this._eventCanvasPool = null;
    },
    destroy : function () {
        this.calendar = null;
        this.destroyEvents(true);
        if (this.clearZones) this.clearZones();
        if (this.clearIndicators) this.clearIndicators();
        this.Super("destroy", arguments);
    },

    // DaySchedule updateEventWindow
    updateEventWindow : function (event) {
        if (!this.body || !this.body.children) return;
        var arr = this.body.children, cal = this.calendar;
        //if (cal.dataSource) cal._pks = cal.getDataSource().getLocalPrimaryKeyFields();
        for (var i = 0; i < arr.length ; i++) {
            if (arr[i] && arr[i].isEventCanvas && this.areSame(arr[i].event, event)) {
                // reassign event for databound update, because databound update creates
                // a new object
                arr[i].event = event;
                this.sizeEventCanvas(arr[i]);
                //arr[i].renderEvent(arr[i].getTop(), arr[i].getLeft(), arr[i].getVisibleWidth(), arr[i].getVisibleHeight());
                //arr[i].sizeToEvent();
                if (arr[i].setDescriptionText)
                    arr[i].setDescriptionText(event[cal.descriptionField]);
                return true;
            }
        }
        return false;
    }

// base-class overrides


});

// WeekSchedule
// --------------------------------------------------------------------------------------------
isc.ClassFactory.defineClass("WeekSchedule", "DaySchedule");


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

    showViewHovers: false,

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
        var cal = this.calendar;
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
        return this.calendar;
    },

    getCellCSSText : function (record, rowNum, colNum) {
        var result = this.creator._getCellCSSText(this, record, rowNum, colNum);

        if (result) return result;
        return this.Super("getCellCSSText", arguments);
    },

    getDayArray : function () {
        var dayArr = [], eventArr, endDate,
            displayDate = new Date(this.creator.year, this.creator.month, 1),
            cal = this.calendar
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

    _$cellDateKey: "date",
    getCellDate : function (rowNum, colNum) {
        if (rowNum == null && colNum == null) {
            rowNum = this.getEventRow();
            colNum = this.getEventColumn();
        }
        if (rowNum < 0 || colNum < 0) return null;
        var fieldIndex = this.body.fields.get(colNum)._dayIndex,
            record = this.getRecord(rowNum),
            cellDate = record[[this._$cellDateKey, fieldIndex].join()]
        ;

        return cellDate;
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
    cellDisabled : function (rowNum, colNum) {
        var body = this.getFieldBody(colNum);
        if (!body || body == this.frozenBody) return false;
        var col = this.getLocalFieldNum(colNum),
            date = this.getCellDate(rowNum, col)
        ;
        return this.calendar.shouldDisableDate(date, this);
    },

    refreshEvents : function () {
        var cal = this.calendar;
        // bail if no data yet
        if (!cal.hasData()) return;
        this.logDebug('refreshEvents: month', 'calendar');
        this.setData(this.getDayArray());
        if (cal.eventsRendered && isc.isA.Function(cal.eventsRendered))
            cal.eventsRendered();
   },

    rowIsHeader : function (rowNum) {
        var cal = this.calendar;
        if (!cal.showDayHeaders || (cal.showDayHeaders && rowNum % 2 == 1)) return false;
        else return true;
    },

    formatCellValue : function (value, record, rowNum, colNum) {
        var cal = this.calendar,
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
        var cal = this.calendar,
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
            var dis = this.cellDisabled(rowNum, colNum),
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
        var cal = this.calendar, year, month, fieldIndex = this.fields.get(colNum)._dayIndex,
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
            if (!this.cellDisabled(rowNum, colNum) && !(!cal.showOtherDays && isOtherDay)) {
                doDefault = cal.dayBodyClick(currDate, evtArr, cal, rowNum, colNum);
                if (doDefault && cal.canCreateEvents) {
                    var startDate = cal.getCellDate(rowNum, colNum, this),
                        endDate = cal.getCellDate(rowNum, colNum+1, this)
                    ;
                    var newEvent = cal.createEventObject(null, startDate, endDate);
                    cal.showEventDialog(newEvent, true);
                }
            }

        }
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
    showRollOver:false,
    useCellRollOvers:false,
    canSelectCells:false,

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
        inherentWidth: false
    },

    // events in timelines resize horizontally
    verticalEvents: false,


    animateFolders: false,

    initWidget : function () {
        this.fields = [];

        var c = this.calendar;

        if (c.alternateLaneStyles) {
            this.alternateRecordStyles = c.alternateLaneStyles;
        }

        if (c.showLaneRollOver != null) {
            this.showRollOver = c.showLaneRollOver;
            this.useCellRollOvers = false;
        }

        if (c.canGroupLanes != null) {
            // set up grouping based on the laneGroupBy settings on Calendar
            this.canGroupBy = c.canGroupLanes;
            if (this.canGroupBy) this.groupByField = c.laneGroupByField;
            if (c.laneGroupStartOpen != null) this.groupStartOpen = c.laneGroupStartOpen;
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

        // only refreshData at this time if the calendar is autoFetchData: true
        this._rebuild(c.autoFetchData);

        this.addAutoChild("eventDragTarget");
        //this.body.addChild(this.eventDragTarget);
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

            if (p.top == null) {
                p.top = view.getRowTop(view.getLaneIndex(p.lane));
                if (p.sublane) p.top += p.sublane.top;
            }
            if (p.height == null) {
                p.height = p.sublane ? p.sublane.height :
                            view.getLaneHeight(p.lane[cal.laneNameField]);
            }
            var left = view.getDateLeftOffset(p.startDate),
                width = view.getDateLeftOffset(p.endDate) - left
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
                startDate = props.startDate,
                endDate = props.endDate
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
        if (this.isLabelCol(colNum) || (record && record._isGroup)) {
            return true;
        }

        var startDate = this._lastMouseDate,
            cal = this.calendar
        ;

        // don't allow selection if the date is disabled (eg, a its weekend and weekends are
        // disabled)
        if (cal.shouldDisableDate(startDate, this)) {
            return true;
        }

        var offsetX = this.body.getOffsetX(),
            leftOffset = this.getDateLeftOffset(startDate)
        ;

        // if backgroundMouseDown is implemented, run it and return if it returns false
        if (cal.backgroundMouseDown && cal.backgroundMouseDown(startDate) == false) return;

        // don't set up selection tracking if canCreateEvents is disabled
        if (!cal.canCreateEvents) return true;
        // first clear any previous selection
        this.clearSelection();

        var canvas = this.getDragSelectCanvas(),
            endDate = cal.addSnapGapsToDate(startDate, this, 1),
            lane = this.getLaneFromPoint(),
            sublane = this.getSublaneFromPoint()
        ;

        var p = { top: null, height: null };
        p.lane = lane;
        p.sublane = sublane;
        p[cal.startDateField] = startDate,
        p[cal.endDateField] = endDate
        canvas.resizeNow(p);

        this._mouseDown = true;
        return false;
    },

    cellOver : function (record, rowNum, colNum) {
        colNum -=1;

        if (this._mouseDown) {
            var canvas = this.getDragSelectCanvas(),
                props = canvas.props,
                mouseDate = this.getDateFromPoint(),
                endDate = this.calendar.addSnapGapsToDate(mouseDate, this, 1)
            ;

            props.endDate = endDate;
            canvas.resizeNow(props);
        }

        this._lastOverLaneIndex = rowNum;

        this.Super("cellOver", arguments);

    },

    cellMouseUp : function (record, rowNum, colNum) {
        if (!this._mouseDown) return true;

        this._mouseDown = false;

        var cal = this.calendar,
            canvas = this.getDragSelectCanvas(),
            props = canvas.props
        ;

        if (this.shouldShowDragHovers()) isc.Hover.hide();

        var startDate = props.startDate,
            endDate = props.endDate
        ;

        // if backgroundClick is implemented, run it and return if it returns false
        if (cal.backgroundClick) {
            if (cal.backgroundClick(props.startDate, props.endDate) == false) {
                this.clearSelection();
                return;
            }
        }

        // if backgroundMouseUp is implemented, run it and bail if it returns false
        if (cal.backgroundMouseUp) {
            if (cal.backgroundMouseUp(props.startDate, props.endDate) == false) {
                this.clearSelection();
                return;
            }
        }

        // don't show an event editor if the date is disabled (eg, a its weekend and weekends are
        // disabled)
        if (cal.shouldDisableDate(endDate, this)) {
            this.clearSelection();
            return true;
        }

        var lane = props.lane,
            sublane = props.sublane
        ;
        var newEvent = cal.createEventObject(null, startDate, endDate,
            lane && lane[cal.laneNameField], sublane && sublane[cal.laneNameField]
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

    getCellValue : function (record, recordNum, fieldNum) {
        var firstDateCol = this.getFirstDateColumn();
        if (fieldNum >= firstDateCol) return null;
        return this.Super("getCellValue", arguments);
    },

    _rebuild : function (refreshData) {
        if (this._drawnCanvasList && this._drawnCanvasList.length > 0) {
            this._drawnCanvasList.setProperty("_availableForUse", true);
            this.clearEvents();
        }

        // availability of hovers may have changed
        this.setShowHover(this.calendar.showViewHovers);

        var fields = this.calcFields();
        if (this.isDrawn()) {
            this.setFields(fields);
        } else this.fields = fields;

        var lanes = this.lanes || this.creator.lanes || [];
        this.setLanes(lanes.duplicate(), true);
        this._scrubDateRange();

        if (refreshData) {
            this._refreshData();
        } else {
            this.refreshEvents();
        }
    },

    setLanes : function (lanes, skipDataUpdate) {
        var cal = this.calendar,
            laneNameField = cal.laneNameField;
        this.lanes = lanes.duplicate();
        var laneCount = lanes.length;
        for (var i=0; i<laneCount; i++) {
            var lane = lanes[i];
            if (!lane[laneNameField]) lane[laneNameField] = lane.name;
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

        this.setData(lanes);
        if (this.isDrawn()) this.redraw();

        // refetch or just redraw applicable events (setLanes() may have been called after setData)
        if (!skipDataUpdate) this._refreshData();
    },
    getLaneIndex : function (laneName) {
        var lane;
        if (isc.isAn.Object(laneName)) lane = laneName;
        else if (this.data) {
            lane = this.data.find("name",                     laneName) ||
                   this.data.find(this.creator.laneNameField, laneName);
        } else return -1;

        //var laneIndex = this.isGrouped ? this.getGroupedRecordIndex(lane) : this.getRecordIndex(lane);
        var laneIndex = this.getRecordIndex(lane);
        return laneIndex;
    },
    getLane : function (laneName) {
        var index = this.getLaneIndex(laneName);
        if (index >= 0) return this.getRecord(index);
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
        var gran = this.creator.timelineGranularity;
        if (gran == "month") {
            this.startDate.setDate(1);
        } else if (gran == "week") {
            this.startDate = isc.DateUtil.getStartOf(this.startDate, "w", true, this.creator.firstDayOfWeek);
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

    groupRowHeight: 30,
    getRowHeight : function (record, rowNum) {
        var height = null;
        if (record) {
            if (this.isGroupNode(record)) height = this.groupRowHeight;
            else height = record.height;
        }
        return height || this.Super("getRowHeight", arguments);
    },

    setInnerColumnWidth : function (newWidth) {
        this.columnWidth = newWidth;
        this.setFields(this.calcFields());
        this.refreshEvents();
    },

    setTimelineRange : function (start, end, timelineGranularity, columnCount, timelineUnitsPerColumn, headerLevels, fromSetChosenDate) {
        var cal = this.calendar,
            colSpan = columnCount || this._dateFieldCount || cal.defaultTimelineColumnSpan,
            refreshData = false
        ;

        start = start || this.startDate;

        this.startDate = start.duplicate();
        cal.startDate = start.duplicate();

        if (end) {
            this.endDate = end.duplicate();
        } else {
            var gran = (timelineGranularity || cal.timelineGranularity).toLowerCase(),
                granString = isc.DateUtil.getTimeUnitKey(gran),
                headerLevel = headerLevels  && headerLevels.length ?
                    headerLevels[headerLevels.length-1] : null
            ;
            this.endDate = isc.DateUtil.getAbsoluteDate("+" +
                    colSpan + granString, this.startDate);
        }
        cal.endDate = this.endDate.duplicate();

        if (timelineGranularity) cal.timelineGranularity = timelineGranularity;
        if (timelineUnitsPerColumn) cal.timelineUnitsPerColumn = timelineUnitsPerColumn;
        if (headerLevels) {
            // if headerLevels have been passed, refresh the data
            cal.headerLevels = headerLevels;
            refreshData = true;
        }

        if (cal.fetchMode && cal.fetchMode != "all") refreshData = true;

        //isc.logWarn('setTimelineRange:' + [timelineGranularity, timelineUnitsPerColumn,
        //        cal.timelineGranularity, cal.timelineUnitsPerColumn]);
        cal.dateChooser.setData(this.startDate);
        if (!fromSetChosenDate) cal.setChosenDate(this.startDate, true);
        //cal.setDateLabel();
        this._rebuild(refreshData);
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

    getColFromDate : function (date) {
        var fields = this.frozenBody ? this.body.fields : this.getFields(),
            startMillis = date.getTime()
        ;

        for (var i=0; i<fields.length; i++) {
            var field = fields[i];
            if (field.date && field.date.getTime() > startMillis) {
                return i-1;
            }
        }
        return null;
    },

    // internal attribute that limits the date-loop in calcFields below - just a safeguard in
    // case someone sets a huge startDate/endDate range and a high resolution
    maximumTimelineColumns: 100,
    calcFields : function () {
        var newFields = [],
            cal = this.creator
        ;

        var hoverProps = {
            hoverDelay: this.hoverDelay+1,
            hoverMoveWithMouse: true,
            canHover: this.shouldShowHeaderHovers(),
            showHover: this.shouldShowHeaderHovers(),
            mouseMove : function () {
                var view = this.grid,
                    rowNum = view.getEventRow(),
                    isHeader = rowNum < 0
                ;
                if (view.shouldShowHeaderHovers()) {
                    isc.Hover.show(this.getHoverHTML());
                    return isc.EH.STOP_BUBBLING;
                }
            },
            getHoverHTML : function () {
                var view = this.grid;
                return view.calendar._getHeaderHoverHTML(view, view.fieldHeaderLevel,
                    this, this.date, this.endDate);
            }
        };

        if (cal.laneFields) {
            var laneFields = cal.laneFields;
            laneFields.setProperty("frozen", true);
            laneFields.setProperty("isLaneField", true);
            for (var i = 0; i < laneFields.length; i++) {
                if (laneFields[i].width == null) laneFields[i].width = this.labelColumnWidth;
                newFields.add(laneFields[i]);
            }
        } else {
            var labelCol = isc.addProperties({
                 width: this.labelColumnWidth,
                 name: "title",
                 title: " ",
                 showTitle: false,
                 frozen: true,
                 isLaneField: true
             }, hoverProps);
             newFields.add(labelCol);
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
            this.headerLevels.remove(this.fieldHeaderLevel);
            cal.timelineGranularity = this.fieldHeaderLevel.unit;
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

        if (headerLevel.headerWidth) this.columnWidth = headerLevel.headerWidth;

        var eDateMillis = eDate.getTime();
        while (sDate.getTime() <= eDateMillis) {
            var thisDate = sDate.duplicate(),
                showDate = cal.shouldShowDate(sDate, this),
                newField = null
            ;

            if (showDate) {
                var title = this.getInnerFieldTitle(headerLevel, spanIndex, sDate);

                newField = isc.addProperties({}, {
                    name: "f" + spanIndex,
                    headerLevel: headerLevel,
                    title: title,
                    width: headerLevel.headerWidth || this.columnWidth,
                    date: thisDate.duplicate(),
                    canGroup: false,
                    canSort: false,
                    canFreeze: false
                }, hoverProps, this.getFieldProperties(thisDate));
            }

            sDate = this.addUnits(sDate, units);

            if (showDate) {
                // store the end date, as the next start date
                newField.endDate = sDate.duplicate();
                newFields.add(newField);
                spanIndex++;
            }

            if (newFields.length >= this.maximumTimelineColumns) {
                this.endDate = sDate.duplicate();
                this.logWarn("Date-range too large - limiting to " +
                        this.maximumTimelineColumns + " columns.");
                break;
            }
        }

        for (var i=0, fieldCount=newFields.length; i<fieldCount; i++) {
            var field = newFields[i];
            isc.addProperties(field, hoverProps);
            field.headerLevel = this.fieldHeaderLevel;
        }

        this.buildHeaderSpans(newFields, this.headerLevels, this.startDate, this.endDate);

        this._dateFieldCount = spanIndex-1;

        return newFields;
    },

    redraw : function () {
        this.Super("redraw", arguments);
        if (!this.animateFolders && this._fromToggleFolder) {
            delete this._fromToggleFolder;
            this.refreshVisibleEvents(null, null, "redraw");
        }
    },

    toggleFolder : function (record) {
        this.Super("toggleFolder", arguments);
        // if not animating folders, refresh events now - otherwise, do it when the row
        // animation completes
        if (!this.animateFolders) {
            this._fromToggleFolder = true;
            this.markForRedraw();
        }
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
            start = cal.startDate,
            end = cal.endDate
        ;

        // we have at least one header - make sure we start and end the timeline
        // at the beginning and end of the innerLevel's unit-type (the actual field-headers,
        // that is)
        var key = isc.DateUtil.getTimeUnitKey(unit);

        cal.startDate = this.startDate = isc.DateUtil.getStartOf(start, key, null, cal.firstDayOfWeek);
        cal.endDate = this.endDate = isc.DateUtil.getEndOf(end, key, null, cal.firstDayOfWeek);
    },

    buildHeaderSpans : function (fields, levels, startDate, endDate) {
        var date = startDate.duplicate(),
            c = this.creator,
            result = [],
            spans = []
        ;

        if (levels && levels.length > 0) {
            spans = this.getHeaderSpans(startDate, endDate, levels, 0, fields);
            this.headerHeight = this._headerHeight + (levels.length * this.headerSpanHeight);
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

        var firstLoop = true;
        while (date <= endDate) {
            DU.dateAdd(date, "mn", 1, 1);
            if (firstLoop) {
                firstLoop = false;
                var newDate = isc.DateUtil.getEndOf(date.duplicate(), isc.DateUtil.getTimeUnitKey(unit), false, c.firstDayOfWeek);
            } else {
                var newDate = this.addUnits(date.duplicate(), unitsPerColumn, unit);
            }

            var span = { unit: unit,
                hoverDelay: this.hoverDelay+1,
                hoverMoveWithMouse: true,
                canHover: this.shouldShowHeaderHovers(),
                showHover: this.shouldShowHeaderHovers(),
                headerLevel: headerLevel,
                mouseMove : function () {
                    var view = this.creator;
                    if (view.shouldShowHeaderHovers()) {
                        if (isc.Hover.lastHoverTarget != view) view.startHover();
                        else view.updateHover();
                        return isc.EH.STOP_BUBBLING;
                    }
                },
                getHoverHTML : function () {
                    var view = this.creator;
                    return view.calendar._getHeaderHoverHTML(view, this.headerLevel, this,
                        this.startDate, this.endDate
                    );
                }
            };
            span[c.startDateField] = date;
            span[c.endDateField] = newDate;

            this.setSpanDates(span, date);

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
                span.spans = this.getHeaderSpans(span.startDate, span.endDate, headerLevels, levelIndex + 1, fields);
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
            title = "" + title;
            if (isc.isA.Function(headerLevel.titleFormatter)) {
                title = headerLevel.titleFormatter(headerLevel, startDate, endDate, title, this.creator);
            }
        }
        return title;

    },

    setSpanDates : function (span, date) {
        var key = isc.DateUtil.getTimeUnitKey(span.unit);

        span.startDate = isc.DateUtil.getStartOf(date, key, null, this.creator.firstDayOfWeek);
        span.endDate = isc.DateUtil.getEndOf(span.startDate, key, null, this.creator.firstDayOfWeek);
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
        var snapGap = this.creator.getSnapGapPixels();
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

    showField : function () {
        this.Super("showField", arguments);
        this.refreshEvents();
    },
    hideField : function () {
        this.Super("hideField", arguments);
        this.refreshEvents();
    },

    getCellStyle : function (record, rowNum, colNum) {
        var body = this.getFieldBody(colNum);
        var bStyle = this.getBaseStyle(record, rowNum, colNum);

        if (colNum == null) return bStyle;

        var isDate = !this.isLabelCol(colNum);
        if (isDate) {
            var col = colNum - (this.frozenBody ? this.frozenBody.fields.length : 0);
            var date = this.getCellDate(rowNum, col);
            if (date && this.calendar.shouldDisableDate(date, this)) {
                bStyle += "Disabled";
            }
        }

        if (!this.__printing) {
            // over styling
            var overRow = body.lastOverRow;
            if (rowNum != null && overRow != null && rowNum == overRow) bStyle += "Over";
        }

        // alternate record styling
        if (this.alternateRecordStyles && rowNum % 2 != 0) bStyle += "Dark";

        return bStyle;
    },

    // timelineView
    getBaseStyle : function (record, rowNum, colNum) {
        var cal = this.calendar;
        if (this.isLabelCol(colNum)) return this.labelColumnBaseStyle;
        else {
            var date = cal.getCellDate(rowNum, colNum, this),
                style = date ? cal.getDateStyle(date, rowNum, colNum, this) : null
            ;

            return style || this.baseStyle;
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
        startDate = isc.DateUtil.getStartOf(startDate, granString, false, c.firstDayOfWeek);
        endDate = isc.DateUtil.dateAdd(endDate, granString, scrollCount, multiplier, false);
        endDate = isc.DateUtil.getEndOf(endDate, granString, false, c.firstDayOfWeek);

        this.setTimelineRange(startDate, endDate, gran, null, units, null, false);
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
            y = this.body.getOffsetY();
        }

        if (x < 0 || y < 0) return null;

        // get the colNum *before* catering for useSnapGap
        var colNum = this.body.getEventColumn(x);
        if (colNum == -2) colNum = this.body.fields.length-1;
        if (colNum == -1) return null;

        if (useSnapGap == null) useSnapGap = true;

        var snapGapPixels = cal.getSnapGapPixels();
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
    _getEventBreadth : function (event) {
        // this method should now use two calls to getDateLeftOffset() to get start and end
        // X offset, and the breadth is the pixel delta - this allows events to span arbitrary
        // hidden columns, while still rendering events that span the gap between the two dates
        var cal = this.calendar,
            eventStart = cal.getEventStartDate(event),
            eventEnd = cal.getEventEndDate(event),
            visibleStart = cal.getVisibleStartDate(this).getTime(),
            visibleEnd = cal.getVisibleEndDate(this).getTime()
        ;

        if (eventStart.getTime() < visibleStart) eventStart.setTime(visibleStart);

        if (eventEnd.getTime() >= visibleEnd) {
            // set the eventEnd to 1ms before the "visible" end because getDateRightOffset()
            // rounds the date up to the next snapGap
            eventEnd.setTime(visibleEnd - 1);
        }

        var eventLeft = this.getDateLeftOffset(eventStart),
            eventRight = this.getDateRightOffset(eventEnd),
            newBreadth = eventRight - eventLeft
        ;

        return newBreadth;
    },

    getDateRightOffset : function (date) {
        return this.getDateLeftOffset(date, true);
    },
    // getDateLeftOffset timelineView
    getDateLeftOffset : function (date, useNextSnapGap) {
        if (!date) return 0;
        var localDate = date.duplicate(),
            visibleStartDate = this.calendar.getVisibleStartDate(this),
            visibleEndDate = this.calendar.getVisibleEndDate(this)
        ;
        if (localDate.getTime() <= visibleStartDate.getTime()) {
            localDate.setTime(visibleStartDate.getTime() + 1);
        }
        if (localDate.getTime() >= visibleEndDate.getTime()) {
            localDate.setTime(visibleEndDate.getTime() - 1);
        }

        var cal = this.calendar,
            snapGapPixels = cal.getSnapGapPixels(),
            fields = this.body.fields,
            len = fields.getLength(),
            millis = localDate.getTime(),
            mins = Math.floor(millis / 60000)
        ;


        for (var i=0; i<len; i++) {
            var field = fields[i];
            if (!this.fieldIsVisible(field)) continue;

            var startMillis = field.date.getTime(),
                endMillis = field.endDate.getTime(),
                startMins = Math.floor(field.date.getTime() / 60000),
                endMins = Math.floor(field.endDate.getTime() / 60000)
            ;
            if (mins < endMins) {
                if (mins >= startMins) {
                    // passed date is within this field - now get the snap point
                    var columnLeft = this.body.getColumnLeft(i),
                        deltaMillis = (millis - startMillis),
                        deltaMins = mins - startMins,
                        snapMins = cal.getSnapGapMinutes(this),
                        snapsToAdd = !snapMins ? 1 :
                                useNextSnapGap ? Math.round(deltaMins / snapMins) :
                                Math.floor(deltaMins / snapMins),
                        left = columnLeft +
                            (mins == startMins ? 0 : (snapsToAdd * snapGapPixels))
                    ;
                    return left;
                } else {
                    // passed date should have been in the previous field, but that field is
                    // clearly hidden - just return the left offset of this field
                    return this.body.getColumnLeft(i);
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

    getLaneHeight : function (lane) {
        if (lane == null || lane === undefined) return;
        if (isc.isA.Number(lane)) lane = this.getRecord(lane);
        else if (isc.isA.String(lane)) lane = this.getLane(lane);
        return lane && lane.height || this.cellHeight;
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

    addLeadingAndTrailingLines : function (canvas) {
        // destroy previous lines and icons before creating new ones
        //canvas.destroyLines();
        var leadLine, leadIcon, trailLine, trailIcon;
        if (canvas._lines) {
            leadLine = canvas._lines[0];
            leadIcon = canvas._lines[1];
            trailLine = canvas._lines[2];
            trailIcon = canvas._lines[3];
        } else {
            leadLine = this._makeLine();
            leadIcon = this._makeIcon(canvas, "lead");
            trailLine = this._makeLine();
            trailIcon = this._makeIcon(canvas, "trail");
        }


        var showLead = this._positionIcon(leadIcon, leadLine);
        var showTrail = this._positionIcon(trailIcon, trailLine);


        if (!canvas._lines) {
            this.body.addChild(leadLine);
            this.body.addChild(leadIcon);

            this.body.addChild(trailLine);
            this.body.addChild(trailIcon);
            canvas._lines = [
               leadLine, leadIcon, trailLine, trailIcon
            ];
        }


    },

    _positionIcon : function (icon, line) {
        var cal = this.calendar, canvas = icon.eventCanvas, event = canvas.event,
            type = icon.type, eWidth = this.columnWidth,
            eHeight = canvas.getVisibleHeight(), eTop = canvas.getTop(),
            eLeft = canvas.getLeft();

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
                dayDiff = cal.getDayDiff(this.startDate, cal.getEventStartDate(event));
                // don't allow invalid lead day. Set to 1 if invalid.
                if (dayDiff < 1) dayDiff = 1;
                lineWidth = dayDiff * eWidth;
                drawIcon = false;
                //icon.hide();
            } else {
                dayDiff = cal.getDayDiff(event[cal.leadingDateField], cal.getEventStartDate(event));
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
        icon._eventStartCol = cal.getDayDiff(cal.getEventStartDate(event), this.startDate);

        return drawIcon;
    },

    _makeIcon : function (canvas, type) {
        var iconSize = (type == "trail" ? this.trailIconSize : this.leadIconSize);
        var icon = isc.Img.create({
            eventCanvas: canvas,
            type: type,

            //prompt:canvas.event.EVENT_ID,
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
                    event = this.eventCanvas.event, cal = this.eventCanvas.calendar,
                    eventDelta = this.type == "trail" ? endCol - eventStartCol : eventStartCol - endCol;
               //isc.logWarn('icon drag stop:' + eventDelta);
               if (eventDelta < 1) return false;
               var otherFields = {};
               var dateField = this.type == "trail" ? cal.trailingDateField : cal.leadingDateField;
               var newDate = event[dateField].duplicate();
               newDate.setDate(newDate.getDate() + delta);
               otherFields[dateField] = newDate;
               cal.updateEvent(event, cal.getEventStartDate(event), cal.getEventEndDate(event),
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

    // timeliveView
    updateEventWindow : function (event) {
        if (!this.body || !this.body.children) return;

        var cal = this.calendar,
            laneName = event[cal.laneNameField]
        ;

        // if one event is updated, all events in the same row may need to be updated as
        // well due to overlapping. By passing a type into tagDataForOverlap, only
        // events in the same row as event will be processed
        //var events = this.tagDataForOverlap(this._localEvents, laneName);
        var events = this.tagDataForOverlap(cal.data.getRange(0, cal.data.getLength()),
                laneName);

        if (this.renderEventsOnDemand) {
            // just refresh events
            this.refreshVisibleEvents(null, null, "updateEventWindow");
        } else {
            for (var i = 0; i < events.length; i++) {
                var thisEvent = events.get(i),
                    canvas = this.getCurrentEventCanvas(this, thisEvent)
                ;
                // make sure to re-initialize the object that the eventWindow is pointing to, which
                // gets out of sync on update
                canvas.event = thisEvent;
                this.sizeEventCanvas(canvas);
            }
        }
    },

    getEventCanvasConstructor : function (event) {
        if (this.eventCanvasConstructor) return this.eventCanvasConstructor;
        if (this.calendar.eventCanvasConstructor == "EventWindow") return "TimelineWindow";
        return null;
    }

}); // end timelineView addProperties()

isc.DaySchedule.addClassProperties({


    _getEventScaffolding : function (calendar, view, startDate) {
        var minsPerRow = calendar.getMinutesPerRow(view),
            rowCount = (60 / minsPerRow) * 24,
            data = [],
            row = {label:"", day1:"", day2:"", day3:"", day4:"", day5:"", day6:"", day7:""},
            today = startDate || new Date(),
            date = new Date(today.getFullYear(), today.getMonth(), today.getDate(),0, 0, 0, 0),
            cellDates = [],
            isDayView = view.isDayView()
        ;

        if (isDayView) isc.DaySchedule._getCellDates(calendar, view, date.duplicate());

        for (var i=0; i<rowCount; i++) {
            var time = date.duplicate();
            data.add(isc.addProperties({}, row, { time: time }));
            date = isc.DateUtil.dateAdd(date, "mn", minsPerRow, 1);
        }

        return data;
    },




    _getCellDates : function (calendar, view, startDate) {
        startDate = startDate || new Date();
        var minsPerRow = calendar.getMinutesPerRow(view),
            today = startDate.duplicate(),
            date = new Date(today.getFullYear(), today.getMonth(), today.getDate(), 0, 0, 0, 0),
            rowCount = (60 / minsPerRow) * 24,
            counter = view.isDayView() ? 1 : 7,
            cellDates = []
        ;

        view._dstCells = null;

        for (var j=0; j < counter; j++) {
            var colDate = date.duplicate(),
                cellDate = colDate.duplicate()
            ;
            for (var i=0; i<=rowCount; i++) {
                if (!cellDates[i]) cellDates[i] = {};
                // store the dates in object properties rather than an array - makes life easier
                // in the week view when weekends aren't visible
                cellDates[i]["day" + (j+1)] = cellDate;

                // when calculating the times for cells, add rows*mins to the column's base
                // datetime - then create a logicalTime with the same offset
                var minsToAdd = minsPerRow * (i + 1);
                var newCellDate = isc.DateUtil.dateAdd(colDate.duplicate(), "mn", minsToAdd, 1);
                var newTime = isc.Date.getLogicalTimeOnly(newCellDate);

                var newTime = isc.Date.getLogicalTimeOnly(colDate);
                newTime.setTime(newTime.getTime() + (minsToAdd*60000));
                // compare the newTime (which is a logical time and not subject to DST) with the
                // time portion of the next calculated cellDate - if they're different, the cell's
                // datetime falls during the DTS crossover
                if ((newTime.getHours() != newCellDate.getHours()) ||
                    (newTime.getMinutes() != newCellDate.getMinutes()))
                {
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
            date = isc.DateUtil.dateAdd(date.duplicate(), "d", 1);
        }

        view._cellDates = cellDates;
        return cellDates;
    }

});






//> @class Calendar
// The Calendar component provides several different ways for a user to view and
// edit a set of events. Note that the +link{group:loadingOptionalModules, Calendar module}
// must be loaded to make use of the Calendar class.
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
// <P>
// See +link{cellStyleSuffixes} for details on how stateful suffixes are combined with the
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
// See +link{cellStyleSuffixes} for details on how stateful suffixes are combined with the
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
// See +link{cellStyleSuffixes} for details on how stateful suffixes are combined with the
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
// See +link{cellStyleSuffixes} for details on how stateful suffixes are combined with the
// base style to generate stateful cell styles.
//
// @group appearance
// @visibility calendar
//<
otherDayHeaderBaseStyle: "calMonthOtherDayHeader",

//> @attr calendar.otherDayBodyBaseStyle  (CSSStyleName : "calMonthDayBody" : IRW)
// The base name for the CSS class applied to the day body of the month view
// of the calendar. This style will have "Dark", "Over", "Selected", or "Disabled"
// appended to it according to the state of the cell.
// <P>
// See +link{cellStyleSuffixes} for details on how stateful suffixes are combined with the
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
// in both the +link{weekView} and +link{dayView}.
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

// internal minutesPerRow - must divide into 60
minutesPerRow: 30,
getMinutesPerRow : function (view) {
    view = view || this.getSelectedView();
    return this.minutesPerRow;
},

getMinutesPerCol : function (view) {
    return isc.DateUtil.convertPeriodUnit(1 * this.timelineUnitsPerColumn, this.timelineGranularity, "mn");
},

getSnapGapMinutes : function (view, rowNum, colNum) {
    view = view || this.getSelectedView();
    if (this.eventSnapGap != null) return this.eventSnapGap;
    if (view && view.isTimelineView()) return this.getMinutesPerCol();
    else return this.getMinutesPerRow();
},

getSnapGapPixels : function (view, rowNum, colNum) {
    view = view || this.getSelectedView();
    if (rowNum == null) rowNum = 0;
    if (colNum == null) colNum = 0;
    var useCol = view && view.isTimelineView(),
        eventSnapMins = this.getSnapGapMinutes(),
        minsPerSize = useCol ? this.getMinutesPerCol(view) : this.getMinutesPerRow(view),
        totalSize
    ;
    if (view && view.body) {
        totalSize = useCol ? view.body.getColumnWidth(colNum) : view.getRowHeight(rowNum);
    } else {
        if (useCol) {
            var headerLevel = view && view.fieldHeaderLevel;
            totalSize = (headerLevel && headerLevel.headerWidth) || (view && view.columnWidth);
        } else totalSize = this.rowHeight;
    }
    var snapGapPixels = Math.floor(totalSize / (Math.floor(minsPerSize / eventSnapMins)));
    return snapGapPixels;
},

addSnapGapsToDate : function (date, view, gapsToAdd) {
    if (!date) return null;
    view = view || this.getSelectedView();
    if (!gapsToAdd) gapsToAdd = 1;
    var snapMinutes = this.getSnapGapMinutes(view);
    var newDate = date.duplicate();
    newDate.setMinutes(newDate.getMinutes() + (snapMinutes * gapsToAdd));
    return newDate;
},

// get the number or rows in an hour
getRowsPerHour : function (view) {
    return Math.floor(60 / this.getMinutesPerRow(view));
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

//> @method calendar.scrollToTime()
// Scroll the calendar Day or Week views to the specified time.
// @param time (string) any parsable time-string
// @visibility calendar
//<
scrollToTime : function (time, view) {
    view = view || this.getSelectedView();
    time = isc.Time.parseInput(time);
    if (isc.isA.Date(time)) {
        var sRow = this.getRowFromDate(view, time);
        var sRowTop = view.getRowHeight(null, 0) * sRow;
        view.body.scrollTo(0, sRowTop);
        view.redraw();
   }
},

//> @method calendar.moveToEvent()
// Rests the current visible range of a calendar view so that it shows the date on which the
// passed event occurs.
// @param event (CalendarEvent) the event to move the calendar view to
// @visibility external
//<
moveToEvent : function (event, view) {
    view = view || this.getSelectedView();
    this.setChosenDate(this.getEventStartDate(event));
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
//
// @visibility external
//<

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
// +link{calendar.timelineView, timelines}, has no effect in other views.
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

//> @attr calendar.eventSnapGap (Integer : null : IR)
// Determines the number of minutes by which an event can be moved or resized when dragging.
// <P>
// The default value of null means that snapping occurs only to the boundaries of Calendar rows
// or Timeline columns.
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
// If true, users can drag-reposition existing events.  Only has an effect when
// +link{calendar.canEditEvents, canEditEvents} is true.
//
// @group allowedOperations
// @visibility calendar
//<
canDragEvents: true,

//> @attr calendar.canResizeEvents (Boolean : true : IR)
// Can +link{CalendarEvent, events} be resized by dragging appropriate edges of the
// +link{eventCanvas.vertical, canvas}?  Only has an effect when both
// +link{calendar.canEditEvents, canEditEvents} and +link{calendar.canDragEvents, canDragEvents}
// are true.  Set this attribute to false to disallow drag-resizing.
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
// If set, weekend days appear in a disabled style and events cannot be created on weekends.
// Which days are considered weekends is controlled by +link{Date.weekendDays}.
//
// @group visibility
// @visibility calendar
//<
disableWeekends: true,

dateIsWeekend : function (date) {
    return Date.getWeekendDays().contains(date.getDay());
},

//> @method calendar.shouldDisableDate()
// Returns true if the passed date should be considered disabled.  Disabled dates don't allow
// events to be created by clicking on them, and drag operations that would start or end on
// such dates are also disallowed.
// <P>
// The default implementation returns false only for dates that fall on a
// +link{Date.getWeekendDays(), weekend}.
// @param date (Date) a Date instance
// @param [view] (CalendarView) the view the date appears in
// @return (boolean) true if this date should be considered disabled
// @visibility external
//<
shouldDisableDate : function (date, view) {
    if (!date) return false;
    view = view || this.getSelectedView();
    // is the passed date disabled?  by default, just returns false if the date falls on a
    // weekend and disableWeekends is true
    if (this.disableWeekends && this.dateIsWeekend(date)) {
        return true;
    }
    return false;
},

//> @method calendar.shouldShowDate()
// Indicates whether the passed date should be visible in the passed +link{class:CalendarView}.
// <P>
// The default implementation returns true, unless the date falls on a
// +link{Date.getWeekendDays(), weekend} and +link{calendar.showWeekends, showWeekends} is
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
    if (this.hideUnusedLanes && this.getLaneEvents(lane).length == 0) {
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
// Suppresses the display of weekend days in the week and month views, and disallows the
// creation of events on weekends.  Which days are considered weekends is controlled by
// +link{Date.weekendDays}.
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

//> @attr calendar.minimalUI (boolean : false : IRW)
// A boolean value controlling whether the Calendar shows tabs for available calendar views.
// By default, this is true for handsets and false otherwise.
//
// @visibility internal
//<
minimalUI: null,

//> @attr calendar.canDragCreateEvents (boolean : false : IRW)
// A boolean value controlling whether the events of varying length can be created by dragging
// the cursor.  By default, this is false for Touch devices and true otherwise.
//
// @visibility internal
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
// When set to true, causes +link{timelineView, Timelines}, and +link{dayView, day views} with
// +link{showDayLanes} set, to highlight the Lane under the mouse with the "Over" style.
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

//> @method calendar.getVisibleStartDate()
// Returns the first visible date in the passed, or currently selected, calendar view.
// @param [view] (CalendarView) the view to get the startDate for, or current view if
// @return (Date) first visible date
// @visibility calendar
//<
getVisibleStartDate : function (view) {
    view = view || this.getSelectedView();
    if (!view || isc.isAn.emptyString(view)) return null;
    return !view.body ? view.startDate : view.getCellDate(0,0);
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
    if (!view.body) return view.endDate;

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

    if (view.isDayView()) {
        return this.chosenDateStart.duplicate();
    } else if (view.isWeekView()) {
        return this.chosenWeekStart.duplicate();
    } else if (view.isMonthView()) {
        return isc.DateUtil.getStartOf(this.chosenDate, isc.DateUtil.getTimeUnitKey("month"));
    } else if (view.isTimelineView()) {
        return this.getVisibleStartDate(view);
    }
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

    if (view.isDayView()) {
        return this.chosenDateEnd.duplicate();
    } else if (view.isWeekView()) {
        return this.chosenWeekEnd.duplicate();
    } else if (view.isMonthView()) {
        return isc.DateUtil.getEndOf(this.chosenDate, isc.DateUtil.getTimeUnitKey("month"));
    } else if (view.isTimelineView()) {
        return this.getVisibleEndDate(view);
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

//> @attr calendar.timelineView (AutoChild CalendarView : null : R)
// +link{CalendarView} used to display events in lanes in a horizontal +link{Timeline} view.
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
// @deprecated in favor of +link{calendar.canResizeEvents, canResizeEvents};
//<
canResizeTimelineEvents: false,

//> @attr calendar.canEditLane (boolean : null : IR)
// Can events be moved between lanes?  If so, the event can be dragged to a different
// +link{lanes, lane} and, when it's editor is shown, an additional drop-down widget is
// provided allowing the user to select a different lane.
// <P>
// In either case, the event's +link{laneNameField} is updated automatically.
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
// the the parent +link{calendar.lanes, lane}.  See
// +link{calendar.getEventCanvasStyle, getEventCanvasStyle()} for more information.
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
// The background color for today in the Month view, or in the Timeline view when
// +{timelineGranularity} is "day".
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
// +link{calendar.showEventDescriptions, showEventDescriptions} is true, this is the the fixed
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
// The title for the +link{nameField} in the quick
// +link{calendar.eventDialog, event dialog} and the detailed
// +link{calendar.eventEditor, editor}.
//
// @group i18nMessages
// @visibility external
//<
eventNameFieldTitle: "Event Name",

//> @attr calendar.eventStartDateFieldTitle (string : "From" : IR)
// The title for the +link{startDateField} in the quick
// +link{calendar.eventDialog, event dialog} and the detailed
// +link{calendar.eventEditor, editor}.
//
// @group i18nMessages
// @visibility external
//<
eventStartDateFieldTitle: "From",

//> @attr calendar.eventEndDateFieldTitle (string : "To" : IR)
// The title for the +link{endDateField} in the quick
// +link{calendar.eventDialog, event dialog} and the detailed
// +link{calendar.eventEditor, editor}.
//
// @group i18nMessages
// @visibility external
//<
eventEndDateFieldTitle: "To",

//> @attr calendar.eventDescriptionFieldTitle (string : "Description" : IR)
// The title for the +link{descriptionField} field in the quick
// +link{calendar.eventDialog, event dialog} and the detailed
// +link{calendar.eventEditor, editor}.
//
// @group i18nMessages
// @visibility external
//<
eventDescriptionFieldTitle: "Description",

//> @attr calendar.eventLaneFieldTitle (string : "Lane" : IR)
// The title for the +link{calendar.laneNameField, laneNameField} in the quick
// +link{calendar.eventDialog, event dialog} and the detailed
// +link{calendar.eventEditor, editor}.
//
// @group i18nMessages
// @visibility external
//<
eventLaneFieldTitle: "Lane",

//> @attr calendar.eventSublaneFieldTitle (String : "Sublane" : IR)
// The title for the +link{calendar.sublaneNameField, sublaneNameField} in the quick
// +link{calendar.eventDialog, event dialog} and the detailed
// +link{calendar.eventEditor, event editor}.
// @group i18nMessages
// @visibility external
//<
eventSublaneFieldTitle: "Sublane",

//> @attr calendar.eventDurationFieldTitle (string : "Duration" : IR)
// The title for the +link{calendar.durationField, duration field} in the quick
// +link{calendar.eventDialog, event dialog} and the detailed
// +link{calendar.eventEditor, editor}.
//
// @group i18nMessages
// @visibility external
//<
eventDurationFieldTitle: "Duration",

//> @attr calendar.eventDurationUnitFieldTitle (string : "&nbsp" : IR)
// The title for the +link{calendar.durationUnitField, duration unit field} in the quick
// +link{calendar.eventDialog, event dialog} and the detailed
// +link{calendar.eventEditor, editor}.
//
// @group i18nMessages
// @visibility external
//<
eventDurationUnitFieldTitle: "&nbsp",

//> @attr calendar.saveButtonTitle (string : "Save Event" : IR)
// The title for the save button in the quick event dialog and the event editor
//
// @group i18nMessages
// @visibility external
//<
saveButtonTitle: "Save Event",

//> @attr calendar.detailsButtonTitle (string : "Edit Details" : IR)
// The title for the edit button in the quick event dialog
//
// @group i18nMessages
// @visibility external
//<
detailsButtonTitle: "Edit Details",

//> @attr calendar.cancelButtonTitle (string : "Cancel" : IR)
// The title for the cancel button in the event editor
//
// @group i18nMessages
// @visibility external
//<
cancelButtonTitle: "Cancel",

//> @attr calendar.monthButtonTitle (string : "< \${monthName}" : IR)
// The title of the +link{calendar.monthButton, month button}, used for showing and hiding the
// +link{calendar.monthView, month view} on Handsets.
// <P>
// This is a dynamic string - text within <code>\${...}</code> are dynamic variables and will
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
monthButtonTitle: "< \${monthName}",

//> @attr calendar.backButtonTitle (string : "Back" : IR)
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

//> @attr calendar.previousButtonHoverText (string : "Previous" : IR)
// The text to be displayed when a user hovers over the +link{calendar.previousButton, previous}
// toolbar button.
//
// @group i18nMessages
// @visibility external
//<
previousButtonHoverText: "Previous",

//> @attr calendar.nextButtonHoverText (string : "Next" : IR)
// The text to be displayed when a user hovers over the +link{calendar.nextButton, next}
// toolbar button
//
// @group i18nMessages
// @visibility external
//<
nextButtonHoverText: "Next",

//> @attr calendar.addEventButtonHoverText (string : "Add an event" : IR)
// The text to be displayed when a user hovers over the +link{calendar.addEventButton, add event}
// toolbar button
//
// @group i18nMessages
// @visibility external
//<
addEventButtonHoverText: "Add an event",

//> @attr calendar.datePickerHoverText (string : "Choose a date" : IR)
// The text to be displayed when a user hovers over the +link{calendar.datePickerButton, date picker}
// toolbar button
//
// @group i18nMessages
// @visibility external
//<
datePickerHoverText: "Choose a date",

//> @attr calendar.invalidDateMessage (Boolean : "From must be before To" : IR)
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
    visibility: "hidden"
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
    visibility: "hidden"
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
    visibility: "hidden"
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
    colWidths: [ 80, 60, 90, "*", "*" ],
    showInlineErrors: false,
    width: 460,
    titleWidth: 80,
    wrapItemTitles: false
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
    if (!this.chosenDate) {

        if (this.startDate) this.chosenDate = this.startDate.duplicate();
        else this.chosenDate = new Date();
    }

    this.year = this.chosenDate.getFullYear();
    this.month = this.chosenDate.getMonth();

    if (this.firstDayOfWeek == null)
        this.firstDayOfWeek = Number(isc.DateChooser.getInstanceProperty("firstDayOfWeek"));

    if (this.laneGroupByField && !isc.isAn.Array(this.laneGroupByField)) {
        this.laneGroupByField = [this.laneGroupByField];
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

    // on touch devices, don't allow events of varying length to be created by dragging
    if (this.canDragCreateEvents == null) this.canDragCreateEvents = !isc.Browser.isTouch;

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

    // switch over to EventCanvas
    if (this.eventWindowDefaults != null) {
        // if there are defaults for eventWindow, underlay them on eventCanvas
        this.eventCanvasDefaults = isc.addProperties({},
                this.eventWindowDefaults, this.eventCanvasDefaults);
        delete this.eventWindowDefaults;
    }
    if (this.eventWindowProperties != null) {
        // if there are properties for eventWindow, underlay them on eventCanvas
        this.eventCanvasProperties = isc.addProperties({},
                this.eventWindowProperties, this.eventCanvasProperties);
        delete this.eventWindowProperties;
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

    if (this.dataSource) this.autoDetectFieldNames();

    this._storeChosenDateRange(this.chosenDate);

    this.createChildren();
    this._setWeekTitles();

    // initialize the data object, setting it to an empty array if it hasn't been defined
    this.setData(null);

    if (!this.initialCriteria && this.autoFetchData) {
        this.initialCriteria = this.getNewCriteria(null);
    }

    this.invokeSuper(isc.Calendar, "initWidget");

    this.createEditors();
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
    if (this.orientationEventId) isc.Page.clearEvent("orientationChange", this.orientationEventId);
    if (this.data) this.ignore(this.data, "dataChanged");
    if (this.controlsBar) this.controlsBar.destroy();
    if (this.controlsBarContainer) this.controlsBarContainer.destroy();
    if (this.dateChooser) this.dateChooser.destroy();
    if (this.eventCanvasButtonLayout) this.eventCanvasButtonLayout.destroy();
    if (this.mainLayout) this.mainLayout.destroy();
    this.Super("destroy", arguments);
},

refreshSelectedView : function () {
    if (this.dayViewSelected()) {
        this.dayView.refreshEvents();
        if (this.monthView) this.monthView._needsRefresh = true; //refreshEvents();
    } else if (this.weekViewSelected()) {
        this.weekView.refreshEvents();
        if (this.monthView) this.monthView._needsRefresh = true; //refreshEvents();
    } else if (this.monthViewSelected()) {
        this.monthView.refreshEvents();
    } else if (this.timelineViewSelected()) {
        this.timelineView.refreshEvents();
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

setRowHeight : function (newHeight, skipScroll) {
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
getEventPKs : function (ds) {
    if (!this._eventPKs) {
        ds = ds || this.getDataSource();
        if (ds) {
            this._eventPKs = ds.getPrimaryKeyFieldNames();
        }
    }
    return this._eventPKs || [];
},
getEventCanvasID : function (view, event) {
    if (!event || !view || !view._eventCanvasMap) return null;
    var pks = this.getEventPKs();
    if (pks.length > 0) {
        var eventKey = this.getID() + "_event_";
        for (var i=0; i<pks.length; i++) {
            eventKey += event[pks[i]];
            if (i==pks.length) break;
        }
        return view._eventCanvasMap[eventKey];
    } else {
        return event._eventCanvasMap ? event._eventCanvasMap[this.getID() + "_" + view.viewName] : null;
    }
},

setEventCanvasID : function (view, event, eventCanvasID) {
    if (!view._eventCanvasMap) view._eventCanvasMap = {};
    var pks = this.getEventPKs().duplicate();
    if (pks.length > 0) {
        var eventKey = this.getID() + "_event_";
        for (var i=0; i<pks.length; i++) {
            eventKey += event[pks[i]];
            if (i==pks.length) break;
        }
        view._eventCanvasMap[eventKey] = eventCanvasID;
    } else {
        if (!event._eventCanvasMap) event._eventCanvasMap = {};
        // _eventCanvasMap stores multiple canvases IDs, one per applicable view, per calendar
        event._eventCanvasMap[this.getID() + "_" + view.viewName] = eventCanvasID;
    }
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
    return Math.abs(isc.Date._getDayDiff(date1, date2, weekdaysOnly, false));
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
        var dayDiff = this.getDayDiff(this.getEventStartDate(event), this.chosenWeekStart,
            (this.showWeekends == false));
        //isc.logWarn('getEventLeft:' + [event.name, event.startDate.toShortDate(),
        //                   this.chosenWeekStart.toShortDate(),dayDiff ]);
        eLeft = (dayDiff * colSize);
    } else if (this.showDayLanes) {
        var fieldId = view.completeFields.findIndex("name", event[this.laneNameField]);
        if (fieldId) {
            eLeft = view.getColumnLeft(fieldId);
        }
    } else {
        var fieldId = view.getColFromDate(this.getEventStartDate(event));
        if (fieldId) {
            eLeft = view.getColumnLeft(fieldId);
        }
    }
    if (this.logIsDebugEnabled("calendar")) {
        this.logDebug('calendar.getEventLeft() = ' + eLeft + ' for:' + isc.Log.echoFull(event), 'calendar');
    }
    return eLeft;
},

//> @method calendar.getEventHeaderHTML()
// Returns the title text for the passed event, for display in the header area of an event
// canvas.  The default implementation returns the event's
// +link{calendar.nameField, name field} for timelines, and that same value pre-pended with
// the event's +link{calendar.startDateField, start} and +link{calendar.endDateField, end}
// dates for day and week views.
//
// @param event (CalendarEvent) the event to get the description text for
// @param [view] (CalendarView) the view in which the event is being rendered
// @return (HTMLString) the HTML to display in the header of an event canvas
// @visibility external
//<
getEventHeaderHTML : function (event, view) {
    var sTime = view.isTimelineView() ? null :
            isc.Time.toTime(this.getEventStartDate(event), this.timeFormatter, true),
        eTitle = (sTime ? sTime + " " : "") + event[this.nameField]
    ;
    return eTitle;
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
    return event[this.descriptionField];
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
        if (unit) date = isc.DateUtil.dateAdd(date, unit, duration);
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
    this.showWeekends = showWeekends;
    if (isc.isA.TabSet(this.mainView)) {
        var tabNum = this.mainView.getSelectedTabNumber();
        this.mainView.removeTabs(this.mainView.tabs);

        if (this.dayView) this.dayView.destroy();

        if (this.weekView) this.weekView.destroy();

        if (this.monthView) this.monthView.destroy();

        var newTabs = this._getTabs();
        this._setWeekTitles();

        this.mainView.addTabs(newTabs);
        this.mainView.selectTab(tabNum);

    } else {
        var memLayout = this.children[0].members[1];
        if (!memLayout) return;
        var oldMem = memLayout.members[1];
        var newMem = this._getTabs()[0].pane;

        memLayout.removeMember(oldMem);
        oldMem.destroy();
        memLayout.addMember(newMem);
        //memLayout.redraw();
        //newMem.show();

        this._setWeekTitles();
    }
    this.setDateLabel();
},

//> @method calendar.canEditEvent()
// Method called whenever the calendar needs to determine whether a particular event should be
// editable.
// <P>
// By default, returns the +link{canEditField} on the provided +link{CalendarEvent} if its set,
// and +link{canEditEvents} otherwise.
//
// @param event (CalendarEvent)
// @return (boolean) whether the user should be allowed to edit the provided CalendarEvent
//<
canEditEvent : function (event) {
    if (!event) return false;
    else if (event[this.canEditField] != null) return event[this.canEditField];
    else return this.canEditEvents;
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
canDragEvent : function (event) {
    if (!event || !this.canEditEvent(event)) return false;
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
canResizeEvent : function (event) {
    if (!event || !this.canEditEvent(event) || !this.canDragEvent(event)) return false;
    else if (event[this.canResizeEventField] != null) return event[this.canResizeEventField];
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
// @param startDate       (Date or Object) start date of event, or CalendarEvent Object
// @param [endDate]       (Date) end date of event
// @param [name]          (String) name of event
// @param [description]   (String) description of event
// @param [otherFields]   (any) new values of additional fields to be updated
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
// @param startDate       (Date or CalendarEvent) start date of event, or CalendarEvent Object
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
    isc.addProperties(event, customValues);

    // add event to data
    // see comment above dataChanged about _ignoreDataChanged
    if (ignoreDataChanged) this._ignoreDataChanged = true;
    if (this.dataSource) {
        var _this = this;
        isc.DataSource.get(this.dataSource).addData(event, function (dsResponse, data, dsRequest) {
            _this.processSaveResponse(dsResponse, data, dsRequest);
        }, {componentId: this.ID, willHandleError: true});
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
            self.monthView.refreshEvents();
        }
        if (self._shouldRefreshTimeline(startDate, endDate)) {
            self.timelineView.removeEvent(event);
        }
        // when eventAutoArrange is true, refresh the day and week views to reflow the events
        // so that they fill any space made available by the removed event
        if (self.eventAutoArrange) {
            if (self.dayView) {
                if (self.dayView.isSelectedView()) self.dayView.refreshEvents();
                else self.dayView._needsRefresh = true;
            }
            if (self.weekView) {
                if (self.weekView.isSelectedView()) self.weekView.refreshEvents();
                else self.weekView._needsRefresh = true;
            }
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
    if (this.dataSource) {
        var ds = isc.DataSource.get(this.dataSource);
        var updatedRecord = isc.addProperties({}, newEvent, otherFields);
        var _this = this;
        ds.updateData(updatedRecord, function (dsResponse, data, dsRequest) {
            _this.processSaveResponse(dsResponse, data, dsRequest, event);
        }, {oldValues: event, componentId: this.ID, willHandleError: true});
        return;
    } else {
        var oldEvent = isc.addProperties({}, event);
        isc.addProperties(event, newEvent, otherFields);
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
        oldStart = isUpdate && oldEvent ? this.getEventStartDate(oldEvent) : null,
        oldEnd = isUpdate && oldEvent ? this.getEventEndDate(oldEvent) : null,
        oldLane = isUpdate && oldEvent ? oldEvent[this.laneNameField] : null
    ;

    delete this._fromEventDialog;
    delete this._fromEventEditor;

    if (dsResponse && dsResponse.status < 0) {
        var errors = dsResponse ? dsResponse.errors : null;
        // show any validation errors inline in the appropriate UI
        if (fromDialog) {
            if (errors) this.eventDialog.items[0].setErrors(errors, true);
            this.displayEventDialog();
        } else if (fromEditor) {
            this.eventEditorLayout.show();
            if (errors) this.eventEditor.setErrors(errors, true);
        }
        // have RPCManager handle other errors
        if (!errors) isc.RPCManager._handleError(dsResponse, dsRequest);
        return;
    }

    var startDate = this.getEventStartDate(newEvent),
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
                    view.retagColumnEvents(0);
                }
            } else if (isAdd) {
                this.dayView.refreshEvents();
            }
        }
    }
    if (this._shouldRefreshWeek(startDate, endDate)) {
        if (!this.weekViewSelected()) this.weekView._needsRefresh = true;
        else {
            var view = this.weekView;
            if (isUpdate) {
                view.retagDayEvents(oldStart);
                if (isc.Date.compareLogicalDates(oldStart, startDate) != 0) {
                    view.retagDayEvents(startDate);
                }
            } else if (isAdd) {
                view.addEvent(newEvent, true);
                view.retagDayEvents(startDate);
            }
        }
    }
    if (this._shouldRefreshMonth(startDate, endDate)) {
        if (!this.monthViewSelected()) this.monthView._needsRefresh = true;
        else this.monthView.refreshEvents();
    }
    if (this._shouldRefreshTimeline(startDate, endDate)) {
        if (!this.timelineViewSelected()) this.timelineView._needsRefresh = true;
        else {
            var view = this.timelineView;
            if (oldLane && oldLane != newLane) view.retagLaneEvents(oldLane);
            view.retagLaneEvents(newLane);
            view.refreshVisibleEvents();
        }
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
        sDate = startDate.toShortDate(this.dateFormatter, false),
        sTime = isc.Time.toTime(startDate, this.timeFormatter, true),
        endDate = this.getEventEndDate(event),
        eDate = endDate.toShortDate(this.dateFormatter, false),
        eTime = isc.Time.toTime(endDate, this.timeFormatter, true),
        name = event[cal.nameField],
        description = event[cal.descriptionField],
        sb = isc.StringBuffer.create()
    ;

    if (view.isTimelineView()) {
        if (startDate.getDate() != endDate.getDate()) {
            // Timeline dates can span days
            sb.append(sDate, "&nbsp;", sTime, "&nbsp;-&nbsp;", eDate, "&nbsp;", eTime);
        } else {
            sb.append(sDate, "&nbsp;", sTime, "&nbsp;-&nbsp;", eTime);
        }
    } else {
        sb.append(sDate, "&nbsp;", sTime, "&nbsp;-&nbsp;", eTime);
    }


    sb.append((name || description ? "</br></br>" : ""),(name ? name + "</br></br>" : ""),
        (description ? description : "")
    );

    var defaultValue = sb.release(false);
    return this.getEventHoverHTML(event, eventCanvas, view, defaultValue);
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
        if (field.hoverHTML) {
            defaultValue = field.hoverHTML(record, defaultValue, rowNum, colNum, view);
        }
    } else {
        if (!view.shouldShowCellHovers()) return;
        var date = view.getDateFromPoint();
        defaultValue = date && date.toShortDateTime();
    }
    return this.getCellHoverHTML(view, record, rowNum, colNum, date, defaultValue)
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
    if (headerLevel.hoverHTML) {
        defaultValue = headerLevel.hoverHTML(view, startDate, endDate, defaultValue);
    }
    // may need to support this too
    //if (button && button.hoverHTML) return button.hoverHTML(view, startDate, endDate
    return this.getHeaderHoverHTML(view, headerLevel, startDate, endDate, defaultValue);
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
    var defaultValue =
        "<div style='" + style + "'>" + event[this.startDateField].toShortDatetime() + "</div>" +
        "<div style='" + style + "'>" + event[this.endDateField].toShortDatetime() + "</div>";
    return this.getDragHoverHTML(view, event, defaultValue);
},
getDragHoverHTML : function (view, event, defaultValue) {
    return defaultValue;
},

_mouseMoved : function (view, mouseTarget, mouseDate, oldMouseDate, rowNum, colNum) {
    if (!view) return;
    if (Date.compareDates(mouseDate, oldMouseDate) != 0) {
        // useful (undocumented) override point
        if (this.mouseDateChanged) this.mouseDateChanged(view, mouseDate, oldMouseDate);
    }
    var field = view.getField(colNum),
        laneFieldHover = field && (field.isLaneField || field.isLabelField) && view.shouldShowLaneFieldHovers(),
        dateCellHover = field && field.date && view.shouldShowCellHovers(),
        headerHover = mouseTarget && mouseTarget.ID.contains("header")
    ;
    if (!headerHover && (laneFieldHover || dateCellHover)) {
        if (mouseTarget == view || mouseTarget == view.body || mouseTarget == view.frozenBody)
            isc.Hover.show(view.getHoverHTML());
        else
            isc.Hover.show(mouseTarget.getHoverHTML());
    } else if (mouseTarget && mouseTarget.getHoverHTML) {

        if (!view.isMonthView()) isc.Hover.show(mouseTarget.getHoverHTML());
    }
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
    var validStart = startDate.getTime() < this.chosenDateEnd.getTime(),
        validEnd = endDate.getTime() > this.chosenDateStart.getTime()
    ;
    // if start is less than rangeEnd and end is greater than rangeStart, its in range
    return (validStart && validEnd);
},

_shouldRefreshWeek : function (startDate, endDate) {
    if (!this.weekView || !this.weekView.body) return false;
    var validStart = startDate.getTime() < this.chosenWeekEnd.getTime(),
        validEnd = endDate.getTime() > this.chosenWeekStart.getTime()
    ;
    // if start is less than rangeEnd and end is greater than rangeStart, its in range
    return (validStart && validEnd);
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




eventCanvasConstructor: "EventCanvas",

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
// Context menu displayed when the rollover
// +link{calendar.eventCanvasContextButton, context button} is clicked.  The context button is
// only displayed if +link{calendar.getEventCanvasMenuItems, getEventCanvasMenuItems} returns
// an array of items to display in the context menu.
// @visibility external
//<
eventCanvasContextMenuConstructor: "Menu",
//> @attr calendar.eventCanvasContextMenuStyle (CSSStyleName : "eventWindowContextMenu" : R)
// The CSS style to apply to the +link{calendar.eventCanvasContextMenu, menu} displayed when
// the rollover +link{calendar.eventCanvasContextButton, context button} is clicked.
// @visibility internal
//<
eventCanvasContextMenuStyle: "eventWindowContextMenu",
eventCanvasContextMenuDefaults: {
},
showEventCanvasContextMenu : function (canvas) {
    if (!canvas.shouldShowContextButton()) return false;
    var menuItems = this.getEventCanvasMenuItems(canvas);
    if (!this.eventCanvasContextMenu) this.addAutoChild("eventCanvasContextMenu");
    this.eventCanvasContextMenu.setData(menuItems);
    canvas.contextMenu = this.eventCanvasContextMenu;
    canvas.showContextMenu();
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
    view = view || this.getSelectedView();
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

hideEventCanvasRolloverControls : function (canvas) {
    if (!canvas._rolloverControls) return;
    for (var i=0; i<canvas._rolloverControls.length; i++) {
        // hide the control
        canvas._rolloverControls[i].hide();
        // remove it's ref to the eventCanvas that last used it
        delete canvas._rolloverControls[i].eventCanvas;
        // and re-add it as a child of the Calendar, which removes it from the eventCanvas
        this.addChild(canvas._rolloverControls[i]);
    }
    canvas._rolloverControls = [];
},

//> @attr calendar.useEventCanvasRolloverControls (boolean : true : R)
// By default, the +link{calendar.eventCanvasCloseButton, close buttons} and resizer widgets
// for event canvases are shown only when the mouse is over a given event.  Set this attribute
// to false to have event canvases show these widgets permanently.
// @visibility internal
//<
useEventCanvasRolloverControls: true,

showEventCanvasRolloverControls : function (canvas) {
    if (canvas.showRolloverControls == false) return false;

    var view = canvas.calendarView,
        showClose = canvas.shouldShowCloseButton(),
        showContext = canvas.shouldShowContextButton(),
        controls = [],
        control
    ;

    if (showClose || showContext) {
        var layout;
        if (this.useEventCanvasRolloverControls) {
            // create single-instance rollover controls
            if (!this.eventCanvasButtonLayout) this.addAutoChild("eventCanvasButtonLayout")
            layout = this.eventCanvasButtonLayout;
            layout.members.removeAll();
        } else {
            // create new components for each canvas
            layout = this.createAutoChild("eventCanvasButtonLayout");
        }
        if (showContext) {
            // add the context control
            // only shown if not switched off and getEventCanvasMenuItems() returns something
            var menuItems = this.getEventCanvasMenuItems(canvas);
            if (menuItems) {
                control = this.getEventCanvasContextButton();
                if (control) {
                    control.eventCanvas = canvas;
                    layout.addMember(control);
                    control.show();
                }
            }
        } else if (this.useEventCanvasRolloverControls) {
            if (this.eventCanvasContextButton) this.eventCanvasContextButton.hide();
        }
        if (showClose) {
            // add the close control
            control = this.getEventCanvasCloseButton();
            if (control) {
                control.eventCanvas = canvas;
                layout.addMember(control);
                control.show();
            }
        } else if (this.useEventCanvasRolloverControls) {
            if (this.eventCanvasCloseButton) this.eventCanvasCloseButton.hide();
        }
        if (layout.members.length > 0) {
            layout.eventCanvas = canvas;
            controls.add(layout);
        } else {
        }
    }

    // add required resizers
    if (this.canResizeEvent(canvas.event)) {
        var sides = canvas.resizeFrom || [],
            startDate = this.getEventStartDate(canvas.event),
            endDate = this.getEventEndDate(canvas.event)
        ;
        for (var i=0; i<sides.length; i++) {
            var side = sides[i];
            if ((["T", "L"].contains(side) && !this.shouldDisableDate(startDate, view)) ||
                (["B", "R"].contains(side) && !this.shouldDisableDate(endDate, view))) {
                // only show top or left resizer if the startDate is not disabled
                // only show bottom or right resizer if the endDate is not disabled
                control = this.getEventCanvasResizer(sides[i]);
                if (control) {
                    control.eventCanvas = canvas;
                    control.dragTarget = canvas.dragTarget;
                    controls.add(control);
                }
            }
        }
    }

    canvas._rolloverControls = [];
    for (var i=0; i<controls.length; i++) {
        canvas.addChild(controls[i]);
        canvas._rolloverControls.add(controls[i]);
        controls[i].show();
    }

    return true;
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
    cursor: "move"
},
getEventCanvasGripper : function (props, canvas, view) {
    props = props || {};
    props.src = this.getEventCanvasGripperIcon(canvas, view);
    var gripper = this.createAutoChild("eventCanvasGripper", props);
    view.addChild(gripper);
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
    view.addChild(label);
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
    layoutTopMargin: 1,
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
    styleName: "eventCanvasCloseButton",
    click : function () {
        var canvas = this.eventCanvas;
        if (this.creator.eventRemoveClick(canvas.event, canvas.calendarView.viewName) != false) {
            this.creator.removeEvent(canvas.event);
        }
        return false;
    }
},
getEventCanvasCloseButton : function () {
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
        this.creator.showEventCanvasContextMenu(this.eventCanvas);
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
eventCanvasVResizerConstructor:"Img",
eventCanvasVResizerDefaults: {
    width:12, height:6, overflow:"hidden", src:"[SKIN]/Window/v_resizer.png",
    autoDraw: false,
    canDragResize: true
},
eventCanvasHResizerConstructor:"Img",
eventCanvasHResizerDefaults: {
    width:6, height:10, overflow:"hidden", src:"[SKIN]/Window/h_resizer.png",
    autoDraw: false,
    canDragResize: true
},
getEventCanvasResizer : function (snapTo) {
    var widgetName = "eventCanvasResizer" + snapTo,
        widget = this.useEventCanvasRolloverControls ? this[widgetName] : null
    ;
    if (!widget) {
        var className = "eventCanvas" + (["T", "B"].contains(snapTo) ? "V" : "H") + "Resizer",
            props = { snapTo: snapTo, getEventEdge : function () { return this.snapTo; } }
        ;
        widget = this.createAutoChild(className, props);
        this[widgetName] = widget;
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
    if (view && view.isSelectedView()) view.refreshEvents();
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
    if (view && view.isSelectedView()) view.refreshEvents();
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
            dragTarget: view.eventDragTarget
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

//> @attr calendar.eventWindow (MultiAutoChild EventWindow : null : A)
// To display events in day and week views, the Calendar creates instance of +link{EventWindow}
// for each event.  Use the +link{AutoChild} system to customize these windows.
// @visibility external
// @deprecated in favor of +link{calendar.eventCanvas}
//<

//> @attr calendar.eventCanvas (MultiAutoChild EventCanvas : null : A)
// To display events in +link{calendar.dayView, day}, +link{calendar.weekView, week} and
// +link{calendar.timelineView, timeline} views, the Calendar creates instances of
// +link{class:EventCanvas} for each event.  Use the +link{AutoChild} system to customize
// these canvases.
// @visibility external
//<

_getEventCanvas : function (event, view) {
    var canDrag = this.canDragEvents,
        canEdit = this.canEditEvent(event),
        canRemove = this.canRemoveEvent(event),
        styleName = this.getEventCanvasStyle(event, view),
        reclaimed = false
    ;

    var props = isc.addProperties({
            // flag for quicker re-detection later
            isEventCanvas: true,
            vertical: view.verticalEvents,
            autoDraw: false,
            calendar: this,
            calendarView: view,
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
        // the event may already be visible, in which case get its current canvas
        canvas = view.getCurrentEventCanvas(event),
        canvasFound = (canvas != null)
    ;

    if (canvasFound) {
        view._eventCanvasPool.remove(canvas);
    } else if (view.useEventCanvasPool) {
        // no canvas currently showing this event - get one from the pool
        canvas = view.getPooledEventCanvas(event);
        if (canvas) {
            reclaimed = true;
            //canvas.VSnapOrigin = 0;
        }
    }
    if (canvas) {
        canvas.event = event;
        if (!canvasFound) canvas.setProperties(props);
        if (canvas.setEvent) canvas.setEvent(event, styleName);
        else {
            canvas.event = event;
            canvas.setEventStyle(styleName);
        }
        //canvas.setDragProperties(
    } else {
        props = isc.addProperties(props, {
            event: event,
            baseStyle: styleName,
            styleName: styleName
        });
        // create eventWindow as an autoChild so it can be customized.
        var canvasClass = this.getEventCanvasConstructor(event, view);
        canvas = this.createAutoChild("eventCanvas", props, canvasClass);
    }
    // add the canvas to the drawnCanvasList
    if (view._drawnCanvasList && !view._drawnCanvasList.contains(canvas))
        view._drawnCanvasList.add(canvas);
    if (view._drawnEvents && !view._drawnEvents.contains(event))
        view._drawnEvents.add(event);

    canvas._availableForUse = false;

    this.setEventCanvasID(view, event, canvas.ID);

    if (this.customizeCanvas) this.customizeCanvas(canvas, view);

    return canvas;
},

_getEventsInRange : function (start, end, view, visibleLanesOnly) {

        var results = [],
            wends = Date.getWeekendDays(),
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
                if (view && view.isWeekView()) results.add(curr);
                else if (!this.showDayLanes || laneNames.contains(curr[this.laneNameField]))
                    results.add(curr);
            }
        }

        return results;
},

getDayEnd : function (startDate) {
    return new Date(startDate.getFullYear(), startDate.getMonth(), startDate.getDate(),23,59,59);
},

isTimeline : function () {
    var isTimeline = this.getCurrentViewName() == "timeline";
    return isTimeline;
},

eventsOverlapGridLines: true,

_storeChosenDateRange : function (date) {
    // store off the start and end of the chosenDate, for clarity later
    this.chosenDateStart = isc.DateUtil.getStartOf(date, "d");
    this.chosenDateEnd = isc.DateUtil.getEndOf(date, "d");

    var startDate =
        this.chosenWeekStart = isc.DateUtil.getStartOf(date, "w", null, this.firstDayOfWeek)
    ;

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
    // set the start and end dates for the chosenDate in day and week views
    this._storeChosenDateRange(newDate.duplicate());

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

        isc.DaySchedule._getCellDates(this, this.dayView, this.chosenDate);
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
            //this.dayView.refreshStyle();
            if (this.dayViewSelected()) this.dayView.refreshEvents();
            else this.dayView._needsRefresh = true;
        }
    }

    if (this.timelineView && !fromTimelineView) {
        this.timelineView.setTimelineRange(this.chosenDate, null, null, null, null, null, true);
    } else {
        if (this.scrollToWorkday && view.scrollToWorkdayStart) {
            view.scrollToWorkdayStart();
        } else {
            view.redraw();
        }
    }

    if (this.monthButton) this.updateMonthButton();
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
    // if no view passed, use the selected one - if one isn't selected, bail
    view = view || this.getSelectedView();
    if (!view) return {};

    var start = null,
        end = null,
        criteria = {},
        fetchMode = this.fetchMode || "all"
    ;

    //if (this.loadEventsOnDemand) {
        if (fetchMode == "auto") {
            // use the largest scrollable range from the visible views - fetches all events that
            // any of the views can reach
            var range = this.getLargestScrollableRange();
            start = range[0];
            end = range[1];
        } else if (fetchMode != "all") {
            var theView = this.getView(fetchMode);
            start = this.getVisibleStartDate(theView);
            end = this.getVisibleEndDate(theView);
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

_usDateRegex:/^\d{4}.\d\d?.\d\d?$/,
_jpDateRegex:/^\d\d?.\d\d.\d{4}?$/,
_setWeekTitles : function () {
    if (!this.weekView) return;
    var nDate = this.chosenWeekStart.duplicate();
    // set day titles
    var sdNames = Date.getShortDayNames();
    var weekends = Date.getWeekendDays();

    isc.DaySchedule._getCellDates(this, this.weekView, this.chosenWeekStart);

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

    this.weekView.startDate = this.chosenWeekStart;
    this.weekView.endDate = this.chosenWeekEnd;
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

_getTabs : function () {
    var nTabs = [],
        props = { calendar: this, baseStyle: this.baseStyle },
        lanes = this.lanes ? this.lanes.duplicate() : null
    ;
    // viewName used by calendar internals, so don't put into defaults
    if (this.showDayView != false) {
        this.dayView = this.createAutoChild("dayView", isc.addProperties({viewName: "day",
            startDate: this.chosenDateStart, endDate: this.chosenDateEnd},
            props,
            { cellHeight: this.rowHeight, enforceVClipping: true } )
        );
        nTabs.add({title: this.dayViewTitle, pane: this.dayView, viewName: "day" });
    }
    if (this.showWeekView != false) {
        this.weekView = this.createAutoChild("weekView", isc.addProperties({viewName: "week"},
            props,
            { cellHeight: this.rowHeight, enforceVClipping: true } )
        );
        nTabs.add({title: this.weekViewTitle, pane: this.weekView, viewName: "week" });
    }
    if (this.showMonthView != false) {
        this.monthView = this.createAutoChild("monthView", isc.addProperties({viewName: "month"},
            props,
            { bodyConstructor:"MonthScheduleBody"} ));
        nTabs.add({title: this.monthViewTitle, pane: this.monthView, viewName: "month" });
    }
    if (this.showTimelineView != false) {
        this.timelineView = this.createAutoChild("timelineView",
            isc.addProperties({viewName: "timeline"}, props));
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
        } else if (this.minimalUI) {
            // for some devices, set the default view according to device orientation
            this.pageOrientationChanged();
        }
    } else {
        this.mainView = tabsArray[0].pane;
    }
},

getLaneMap : function () {
    if (!this.isTimeline() && !this.showDayLanes) return {};

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
    if (view && view.hasLanes()) return this.laneEventPadding;
    return 0;
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

                var newEvent = {};
                newEvent[cal.startDateField] = sDate;
                newEvent[cal.endDateField] = eDate;
                cal.eventDialog.event = newEvent;
                cal.eventDialog.setDate(sDate, eDate);
                // place the dialog at the left edge of the calendar, right below the button itself
                cal.eventDialog.setPageLeft(cal.getPageLeft());
                cal.eventDialog.setPageTop(this.getPageTop() + this.getVisibleHeight());

                cal.displayEventDialog();
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
                        nameType = !isNewEvent ? "staticText" : "text",
                        laneType = !isNewEvent ? "staticText" : "select",
                        sublaneType = !isNewEvent ? "staticText" : "select",
                        showLane = cal.isTimeline() || (cal.showDayLanes && cal.dayViewSelected()),
                        showSublane = showLane && cal.useSublanes
                    ;

                    // set up default fields
                    var fieldList = [
                        {name: cal.nameField, title: cal.eventNameFieldTitle, type: nameType,
                            width: 250
                        },
                        {name: cal.laneNameField, title: cal.eventLaneFieldTitle,
                                type: laneType, width: 150,
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
                                type: sublaneType, width: 150,
                                valueMap: [], //cal.getLaneMap(),
                                showIf: showSublane ? "true" : "false"
                        },
                        {name: "save", title: cal.saveButtonTitle, editorType: "SubmitItem", endRow: false},
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
                                form.calendar.showEventEditor(event, isNew);
                            }
                        }
                    ];
                    if (!isNewEvent) fieldList.removeAt(3);
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
                        lane = null,
                        sublane = null
                    ;

                    if (!this.validate()) return;

                    if (cal.isTimeline() || (cal.dayViewSelected() && cal.showDayLanes)) {
                        lane = this.getItem(cal.laneNameField).getValue();
                        sublane = this.getItem(cal.sublaneNameField).getValue();
                    }

                    var customValues = isc.addProperties({}, this.getCustomValues());

                    cal._fromEventDialog = true;
                    var newEvent = cal.createEventObject(evt, sdate, edate,
                            lane, sublane, this.getValue(cal.nameField)
                    );

                    if (!isNewEvent) { // event window clicked, so update
                        cal.updateCalendarEvent(evt, newEvent, customValues);
                    } else { // create new event
                        cal.addCalendarEvent(newEvent, customValues);
                    }
                    cal.hideEventDialog();
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
                cal = this.creator,
                view = cal.getSelectedView()
            ;

            // if we have custom fields, clear errors and set those custom fields
            if (cal.eventDialogFields) {
                theForm.clearErrors(true);
                theForm.setCustomValues(event);
            }
            this.setDate(cal.getEventStartDate(event), cal.getEventEndDate(event));

            if (cal.useSublanes && event[cal.laneNameField]) {
                var lane = view.getLane(event[cal.laneNameField]);
                if (lane) {
                    var slItem = theForm.getItem(cal.sublaneNameField);
                    slItem.setValueMap(cal.getSublaneMap(lane));
                }
            }
            theForm.setValues(event);
        },

        closeClick : function () {
            this.Super('closeClick');
            this.creator.clearViewSelection();
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
                editStyle = cal.getDateEditingStyle(),
                durationFields = [
                    { name: "endType", type: "text", showTitle: false, width: "*",
                        editorType: "SelectItem", textAlign: "right",
                        valueMap: [ cal.eventDurationFieldTitle, cal.eventEndDateFieldTitle ],
                        endRow: false,
                        changed : function (form, item, value) {
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

            if (cal.timelineView || (cal.dayViewSelected() && cal.showDayLanes)) {
                // if the calendar allows laneEditing, show the lane picker - if a given event
                // is canEditLane: false, the picker will be disabled
                var laneMap = cal.getLaneMap(),
                    field = { name: cal.laneNameField, title: cal.eventLaneFieldTitle, type: "select",
                        valueMap: laneMap, endRow: true,
                        width: "*", colSpan: 3,
                        changed : function (form, item, value) {
                            // when the lane changes, refetch the list of sublanes
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
                     cellStyle: this.errorStyle || "formCellError", endRow: true}
                ]);
                if (allowDurations) fieldList.addList(durationFields);
                fieldList.addList([
                    {name: "endHours", type: "integer", width: 60,
                     title: cal.eventEndDateFieldTitle, showTitle: !allowDurations,
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
            var fieldsToUse = isc.shallowClone(cal.eventEditorFields);
            this.setFields(fieldsToUse);
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
        setDate : function (startDate, endDate, eventName, lane, sublane) {
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
                laneSwitcher = form.getItem(cal.laneNameField),
                sublaneSwitcher = form.getItem(cal.sublaneNameField),
                allowDurations = cal.allowDurationEvents,
                fDurationCB = form.getItem("endType"),
                fDuration = form.getItem(cal.durationField),
                fDurationUnit = form.getItem(cal.durationUnitField)
            ;

            this.event = event;
            // if we have custom fields, clear errors and set those custom fields
            if (cal.eventEditorFields) {
                form.clearErrors(true);
                form.setCustomValues(event);
            }
            if (laneSwitcher) {
                laneSwitcher.setValueMap(cal.getLaneMap());
                laneSwitcher.setValue(event[cal.laneNameField]);
                laneSwitcher.setDisabled(!cal.canEditEventLane(event));
                var showSwitcher = view.isTimelineView() || (view.isDayView() && cal.showDayLanes);
                if (showSwitcher) laneSwitcher.show();
                else laneSwitcher.hide();
            }
            if (sublaneSwitcher) {
                sublaneSwitcher.setValueMap(cal.getSublaneMap(event[cal.laneNameField]));
                sublaneSwitcher.setValue(event[cal.sublaneNameField]);
                sublaneSwitcher.setDisabled(!cal.canEditEventSublane(event));
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
                        form.getField("endHours").hide();
                        form.getField("endMinutes").hide();
                        form.getField("endAMPM").hide();
                    } else {
                        form.getField(cal.endDateField).hide();
                    }
                } else {
                    fDurationCB.setValue(cal.eventEndDateFieldTitle);
                    fDuration.hide();
                    fDurationUnit.hide();
                    var endDate = event[cal.endDateField];
                    if (cal.getDateEditingStyle() == "time") {
                        form.getField("endHours").show();
                        form.getField("endHours").setValue(endDate.getHours());
                        form.getField("endMinutes").show();
                        form.getField("endMinutes").setValue(endDate.getMinutes());
                        form.getField("endAMPM").show();
                    } else {
                        form.getField(cal.endDateField).show();
                        form.getField(cal.endDateField).setValue(endDate);
                    }
                }
            }
            this.setDate(cal.getEventStartDate(event), cal.getEventEndDate(event));
            form.setValue(cal.nameField, event[cal.nameField]);
            form.setValue(cal.descriptionField, event[cal.descriptionField]);
            this.originalStart = isc.clone(this.currentStart);
            this.originalEnd = isc.clone(this.currentEnd);
        },

        hide : function () {
            this.Super('hide');
            this.creator.clearViewSelection();
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

hideEventDialog : function () {
    this.eventDialog.hide();
},
displayEventDialog : function () {
    this.eventDialog.show();
},

addEventOrUpdateEventFields : function () {
    var cal = this,
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

        var startDate = cal.eventEditorLayout.currentStart.duplicate();
        startDate.setHours(sHrs);
        startDate.setMinutes(sMins);
        var startMillis = startDate.getTime(),
            maxEndDate = isc.DateUtil.getEndOf(startDate.duplicate(), "d")
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

            var endDate = startDate.duplicate();
            endDate.setHours(eHrs);
            endDate.setMinutes(eMins);
            if (endDate.getTime() > maxEndDate.getTime()) {
                endDate = maxEndDate.duplicate();
            }

            newEvent[cal.endDateField] = endDate;

            cal._fromEventEditor = true;

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
    event = event || {};
    var startDate = this.getEventStartDate(event) || new Date(),
        endDate = this.getEventEndDate(event),
        currentView = this.getSelectedView(),
        eventWindow = currentView.isMonthView() ? null : currentView.getCurrentEventCanvas(event),
        rowNum, colNum, coords,
        bodyLeft = currentView.body.getLeft(),
        bodyTop = currentView.body.getTop(),
        dialog = this.eventDialog
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
        dialog.eventWindow = null;
        dialog.event = event;
        dialog.isNewEvent = isNewEvent;
        dialog.items[0].createFields();

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
            rowNum = currentView.getEventRow();
            colNum = currentView.getEventColumn();
        } else if (currentView.isTimelineView()) {
            var tl = this.timelineView;

            rowNum = tl.getEventLaneIndex(event);
            colNum = tl.body.getEventColumn(tl.getDateLeftOffset(sDate));
            // assume a default length of one unit of the timelineGranularity for new events
            eDate = endDate || this.getDateFromPoint(tl.getDateLeftOffset(sDate) + tl.getColumnWidth(colNum));
            // set the lane
            dialog.setLane(event[this.laneNameField]);
        } else {
            if (currentView.isMonthView()) {
                rowNum = currentView.getEventRow();
                colNum = currentView.getEventColumn();
                // assume a default length of one hour (two rows) for new Calendar events
                eDate = endDate || this.getCellDate(rowNum, colNum, currentView);
            } else {
                rowNum = startDate.getHours() * this.getRowsPerHour(currentView);
                rowNum += Math.floor(startDate.getMinutes() / this.getMinutesPerRow(currentView));
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

        dialog.setEvent(event);

        coords = [ currentView.body.getColumnLeft(colNum), currentView.body.getRowTop(rowNum) ];
    } else { // otherwise show dialog for clicked event
        if (currentView.isTimelineView()) {
            rowNum = currentView.getEventLaneIndex(event);
            colNum = currentView.body.getEventColumn(currentView.getDateLeftOffset(startDate));
        } else if (currentView.isDayView() || currentView.isWeekView()) {
            rowNum = startDate.getHours() * this.getRowsPerHour(currentView);
            rowNum += Math.floor(startDate.getMinutes() / this.getMinutesPerRow(currentView));
            colNum = currentView.getColFromDate(startDate);
        }
        dialog.eventWindow = eventWindow;
        dialog.isNewEvent = false;
        dialog.items[0].createFields();

        dialog.setEvent(eventWindow.event);
        if (this.bringEventsToFront) eventWindow.bringToFront();

        coords = [eventWindow.getLeft(), eventWindow.getTop()];
    }


    var viewRect = currentView.getPageRect(),
        bodyRect = currentView.body.getPageRect(),
        body = currentView.body
    ;

    coords[0] += body.getLeft() - body.getScrollLeft();
    coords[1] += currentView.getHeaderHeight() - body.getScrollTop();

    dialog.keepInParentRect = true;
    if (dialog.parentWidget) dialog.deparent();
    currentView.addChild(dialog);

    dialog.moveTo(coords[0], coords[1]);
    dialog.show();
    // bringToFront() needs to be put on a timer, else it fails to actually bring the
    // eventDialog to the front
    isc.Timer.setTimeout(this.ID + ".eventDialog.bringToFront()");
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

newEventEditorWindowTitle: "New Event",
_showEventEditor : function (event, isNewEvent) {

    if (!this.eventEditorLayout.isDrawn()) {

        this.eventEditorLayout.setVisibility(isc.Canvas.INHERIT);
        this.eventEditorLayout.draw();
    }
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

    this.hideEventDialog();

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
        var differentDays = (isc.Date.compareLogicalDates(startDate, endDate) != 0);

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
    var currDate = this.getCellDate(rowNum, colNum, grid);
    // not a date cell
    if (!currDate) return null;

    var result = this.getDateCSSText(currDate, rowNum, colNum, grid);
    // an override of getDateCSSText() returned something - return that
    if (result) return result;

    if (this.todayBackgroundColor) {
        // if todayBackgroundColor is set and the passed logical date is today,
        // return CSS for that...
        var dateComp = isc.Date.compareLogicalDates(currDate, new Date());
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
// @see getDateStyle()
//
// @param date (Date) the date to return CSS text for
// @param rowNum (Integer) the row number to get the CSS for
// @param colNum (Integer) the column number to get the date for
// @param view (CalendarView) the current CalendarView
// @return (String) CSS text for the associated cell
//
// @visibility calendar
//<
getDateCSSText : function (date, rowNum, colNum, view) {
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
// @param view (CalendarView) the current CalendarView
// @return (CSSStyleName) CSS style for the cell associated with the passed date
//
// @visibility calendar
//<
getDateStyle : function (date, rowNum, colNum, view) {
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
// @return (Lane) the Lane at the the passed co-ords in the passed or selected view
//
// @visibility external
//<
getLaneFromPoint : function (x, y, view) {
    view = view || this.getSelectedView();
    if (!view.hasLanes()) return null;
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

monthViewEventClick : function (rowNum, colNum, eventIndex) {
    var events = this.monthView.getEvents(rowNum, colNum);
    var evt = events[eventIndex];
    if (this.eventClick(evt, "month")) this.showEventEditor(evt);
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
        var eTime = isc.Time.toTime(this.getEventStartDate(evtArr[i]), this.timeFormatter, true) + " ";
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
        var target = this.creator || this;
        for (var i = 0; i < evtArr.length; i++) {
            var eTime = isc.Time.toTime(target.getEventStartDate(evtArr[i]), target.timeFormatter, true);
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
// By default a dialog appears showing details for the event, and offering the ability to
// edit events which are editable.  Return false to cancel the default action. This is a good
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

_eventCanvasClick : function (canvas) {
    var event = canvas.event,
        view = canvas.calendarView,
        isWeekView = view.isWeekView(),
        doDefault = this.eventClick(event, view.viewName)
    ;

    // bring the event to the front of the zorder
    if (this.bringEventsToFront) canvas.bringToFront();

    if (doDefault) {
        if (!this.canEditEvent(event)) return;
        // handle the case when a selection is made, then an event is clicked
        this.clearViewSelection();
        if (!view.isTimelineView()) {
            var eventStart = this.getEventStartDate(event);
            var offset = (view.frozenFields ? view.frozenFields.length : 0);
            var col = isWeekView ? eventStart.getDay() - this.firstDayOfWeek + offset : offset;
            // account for no weekends shown
            if (isWeekView && this.showWeekends == false) col--;
            var row = eventStart.getHours() * this.getRowsPerHour();
        }

        this.showEventDialog(event);
    }
},

//> @method calendar.eventRemoveClick()
// Called whenever the close icon of an +link{EventCanvas, event canvas} is clicked in the
// +link{dayView, day}, +link{weekView, week} and +link{timelineView, timeline} views.
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

// helper method, gets a valid date with respect to the eventSnapGap and starting point of
// referenceDate. Used in eventWindow dragRepositionStop and dragResizeStop to ensure a valid
// date every time.
getValidSnapDate : function (referenceDate, snapDate) {
    if (this.isTimeline()) {

    } else {
        // the formula for getting the snapDate is:
        // round((snapDate as minutes - offset) / snapGap) * snapGap + offset
        // where offset = reference date as minutes mod snapGap
        var snapGap = this.getSnapGapPixels();

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
// @param [end] (Date) end of range
// @visibility external
//<
setTimelineRange : function (start, end, gran, unitCount, units, headerLevels, callback) {
    if (this.timelineView) this.timelineView.setTimelineRange(start, end, gran, unitCount, units, headerLevels);
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
    if (this.timelineView) {
        granularityPerColumn = granularityPerColumn || 1;
        this.timelineView.setTimelineRange(this.startDate, null, unit, unitCount,
            granularityPerColumn, headerLevels
        );
    }
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

});


// EventWindow
//---------------------------------------------------------------------------------------------
//> @class EventWindow
// Subclass of Window used to display events within a +link{Calendar}.  Customize via
// +link{calendar.eventWindow}.
//
// @treeLocation  Client Reference/Calendar
// @visibility external
// @deprecated in favor of +link{class:EventCanvas}
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
    headerProperties: {padding:0, margin: 0, height:14},

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
        //headerProps: isc.addProperties({}, {dragTarget: view.eventDragTarget}),

        this.addProperties(this.creator.viewProps);
        this.addProperties(this.creator.gridProps);

        this.addProperties(this.creator.timelineViewProperties);

        this.descriptionText = this.event[this.calendar.descriptionField];

        this.showHeader = this.calendar.showEventHeaders;
        this.showBody = this.calendar.showEventDescriptions;

        this.footerProperties = isc.addProperties({dragTarget: this.eventDragTarget},
                this.footerProperties, this._footerProperties);

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
                    autoDraw: true,
                    border: "0px",
                    padding: 3,
                    height: 1,
                    width: 1,
                    backgroundColor: this.event.backgroundColor,
                    textColor: this.event.textColor,
                    setContents : function (contents) {
                        this._origContents = contents;
                        this.Super("setContents", arguments);
                    },
                    canHover: true,
                    showHover: true,
                    eventCanvas: this,
                    getHoverHTML : function () {
                        return this.eventCanvas.getHoverHTML();
                    },
                    redrawWithParent: true
            });
            lbl.addMember = function (item) { this.addChild(item); };
            lbl.addChild(this.resizer);
            this.addChild(lbl);
            this.header = lbl;
            this._customHeaderLabel = lbl;
            this._customHeader = true;
        }


        this.setEventStyle(this.baseStyle);
    },

    getEvent : function () {
        return this.event;
    },

    getCalendar : function () {
        return this.calendar;
    },

    getCalendarView : function () {
        return this.calendarView;
    },

    // helper method to set the various drag properties
    setDragProperties : function (canDragReposition, canDragResize, dragTarget) {
        this.canDragResize = canDragResize == null ? true : canDragResize;
        if (canDragReposition == null) canDragReposition = true;

        this.dragTarget = dragTarget;

        this.setCanDragReposition(canDragReposition, dragTarget);

        if (this.canDragResize) {
            if (!this.resizer) this.makeFooter();
            else if (!this.resizer.isVisible()) this.resizer.show();
        } else {
            if (this.resizer && this.resizer.isVisible()) this.resizer.hide();
        }

    },

    setEventStyle : function (styleName, headerStyle, bodyStyle) {
        headerStyle = headerStyle || this.headerStyle || styleName + "Header";
        bodyStyle = bodyStyle || this.bodyStyle || styleName + "Body";
        this.baseStyle = styleName;
        this.styleName = styleName;
        this.bodyStyle = bodyStyle;
        this.headerStyle = headerStyle;
        this.setStyleName(styleName);
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

        var props = { dragTarget:this.dragTarget, styleName: this.baseStyle + "Resizer" };

        if (this._customHeader) props.snapTo = "B";
        this.resizer = this.createAutoChild("resizer", props);

        if (this._customHeader) {
            this.header.addChild(this.resizer);
        } else {
            this.addChild(this.resizer);
        }

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

        // bring the event to the front of the zorder
        if (cal.bringEventsToFront) this.bringToFront();

        var doDefault = cal.eventClick(this.event, this._isWeek ? "week" : "day");
        if (doDefault) {
            if (!cal.canEditEvent(this.event)) return;
            // handle the case when a selection is made, then an event is clicked
            cal.clearViewSelection();
            var offset = (this._isWeek && cal.weekView.isLabelCol(0) ? 1 : 0);
            //var row =  cal.getEventStartDate(this.event).getHours() * cal.getRowsPerHour();
            var col = this._isWeek ? cal.getEventStartDate(this.event).getDay() -
                        cal.firstDayOfWeek + offset : offset;
            // account for no weekends shown
            if (this._isWeek && cal.showWeekends == false) col--;
            cal.showEventDialog(this.event);
        }
    },

    mouseDown : function () {
        if (this.dragTarget) this.dragTarget.eventCanvas = this;
        this.calendar.eventDialog.hide();
        return isc.EH.STOP_BUBBLING;
    },

    renderEvent : function (eTop, eLeft, eWidth, eHeight) {
        var cal = this.calendar, event = this.event;

        if (isc.isA.Number(eWidth) && isc.isA.Number(eHeight)) {
            this.resizeTo(Math.round(eWidth), Math.round(eHeight));
        }
        if (isc.isA.Number(eTop) && isc.isA.Number(eLeft)) {
            this.moveTo(Math.round(eLeft), Math.round(eTop));
        }

        var title = cal.getEventHeaderHTML(event, this.calendarView),
            eTitle = title,
            style = ""
        ;
        if (event.headerBackgroundColor) style += "backgroundColor: " + event.headerBackgroundColor + ";";
        if (event.headerTextColor) style += "backgroundColor: " + event.headerTextColor + ";";
        if (style != "") eTitle = "<span style='" + style + "'>" + eTitle + "<span>";
        this.setTitle(eTitle);

        this.updateColors(title);

        if (this._customHeader) {
            this.header.resizeTo(Math.round(eWidth), Math.round(eHeight));
            this.header.setContents(eTitle);
        }

        if (!this.isDrawn()) this.draw();
        this.show();
        this.bringToFront();
    },

    updateColors : function (title) {
        var cal = this.calendar,
            event = this.event,
            header = this.header,
            labelParent = header ? header.getMember ? header.getMember(0) : header : null,
            label = labelParent,
            eTitle = title || cal.getEventHeaderHTML(event, this.calendarView)
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

        var startCol = cal.getEventStartCol(this.event, this, this.calendarView),
            endCol = cal.getEventEndCol(this.event, this, this.calendarView)
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
        var result = output.release(false);

        return result;
    },

    getHoverHTML : function () {
        return this.calendar._getEventHoverHTML(this.event, this, this.calendarView);
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
        if (this.event) this.calendarView.sizeEventCanvas(this);
    },

    // get event length in minutes
    getEventLength : function () {
        return this.event.eventLength;
    },

    show : function () {
        this.Super("show", arguments);
    },

    resized : function () {
        if (this._customHeader) {
            this.header.resizeTo(this.getVisibleWidth(), this.getVisibleHeight());
        }
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
        if (doDefault) {
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
    }



}); // end TimelineWindow

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
// @treeLocation  Client Reference/Calendar
// @visibility external
//<
isc.defineClass("EventCanvas", "VLayout");



isc.EventCanvas.addClassProperties({

    headerSizer: null,
    getHeaderHeight : function (text, width, height, wrap, canvas) {
        if (!this.headerSizer) {
            this.headerSizer = isc.Canvas.create({
                ID: "_headerSizer",
                visibility: "hidden",
                //backgroundColor: "red",
                top: -1000
            });
        }
        this.headerSizer.setProperties({
            height: wrap ? 1 : height,
            maxHeight: wrap ? null : height,
            width: width,
            maxWidth: width,
            overflow: wrap ? "visible" : "hidden",
            contents: text,
            styleName: canvas.getHeaderStyle()
        });
        if (!this.headerSizer.isDrawn()) this.headerSizer.draw();
        else this.headerSizer.redraw();
        var newHeight = this.headerSizer.getVisibleHeight();
        return newHeight;
    }

});

isc.EventCanvas.addProperties({
    autoDraw: false,
    overflow: "hidden",
    minHeight: 1,
    minWidth: 1,

    // hover properties - see also getHoverHTML()
    showHover: true,
    canHover: true,
    hoverMoveWithMouse: true,
    hoverWidth: 200,

    // drag properties
    snapToGrid: false,
    keepInParentRect: true,
    dragAppearance: "none",
    canDragResize: true,
    canDragReposition: true,

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

    //> @attr eventCanvas.vertical (Boolean : true : IRW)
    // Indicates the orientation of the event in its containing view.  Affects drag and resize
    // orientation and which edges of the canvas are available for resizing.
    //
    // @visibility external
    //<
    vertical: true,

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
    // as a peer of this eventCanvas, immediately outside of it, on the side indicated by
    // +link{eventCanvas.labelSnapTo}.
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

    //> @type HeaderPosition
    // @value "header" Show the headerHTML in a styled DIV above the body, like a +link{Window, Window}
    // @value "body" Show the label in the +link{body} - do not show an event's description
    // @value "footer" Show the headerHTML in a styled DIV below the body
    // @value "adjacent" Show the headerHTML adjacent to the container - see +link{labelSnapTo}
    // @value "none" Don't show the label at all
    // @visibility internal
    //<

    //> @attr eventCanvas.headerPosition (LabelPosition : "header" : [IRW])
    // @visibility internal
    //<
    headerPosition: "header",

    //> @attr eventCanvas.showGripper (Boolean : null : IRW)
    // When set to true, shows the +link{eventCanvas.gripper, gripper} component, which snaps,
    // centered, to the top edge of the eventCanvas and can be used to move it with the mouse.
    //
    // @visibility external
    //<
    //showGripper: false,

    //> @attr eventCanvas.gripperIcon (SCImgURL : null : IRW)
    // The source for the icon displayed as the "gripper" that snaps to the top of an event canvas
    // and allows an event to be dragged with the mouse.
    // @visibility external
    //<

    //> @attr eventCanvas.gripper (AutoChild Img : null : IRW)
    // When +link{eventCanvas.showGripper, showGripper} is true, this is the component that will
    // be rendered adjacent to the canvas and allow the canvas to be moved with the mouse.
    //
    // @visibility external
    //<


    //opacity: 90,

    initWidget : function () {

        if (this.vertical) this.resizeFrom = ["B"];
        else this.resizeFrom = ["L","R"];

        this.hoverDelay = this.calendar.hoverDelay + 1;

        //if (!this.calendar.showEventDescriptions) this.showBody = false;

        this.Super("initWidget", arguments);

        if (this.shouldShowGripper()) this.createGripper();
        if (this.shouldShowLabel()) this.createLabel();

        if (this.event) this.setEvent(this.event, this.styleName);

        if (!this.calendar.useEventCanvasRolloverControls) {
            this.calendar.showEventCanvasRolloverControls(this);
        }

        if (this._mouseTransparent == null && !this.calendarView.shouldShowEventHovers()) {
            //this._mouseTransparent = true;
        }
        this.updateShowHovers();
    },

    updateShowHovers : function () {
        if (this._mouseTransparent) this.eventProxy = this.calendarView;
    },

    shouldShowGripper : function () {
        var cal = this.calendar;
        // don't show the gripper if there's no event, or the event can't be edited or dragged
        if (!this.event || !cal.canEditEvent(this.event) || !cal.canDragEvent(this.event)) {
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
            canDragResize: false
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

    resized : function () {
        if (this.gripper || this.label) this.repositionPeers();
    },

    moved : function () {
        if (this.gripper || this.label) this.repositionPeers();
    },

    redraw : function () {
        this.Super("redraw");
        if (this.gripper || this.label) this.repositionPeers();
    },

    hide : function () {
        this.Super("hide", arguments);
        if (this.gripper) this.gripper.hide();
        if (this.label) this.label.hide();
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
            thisWidth = this.getWidth(),
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
                    textHeight = isc.EventCanvas.getHeaderHeight(headerHTML, 40,
                        this.headerHeight, this.getHeaderWrap(), this)
                ;

                this.label.setContents(this.getHeaderHTML());
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
        this.event = event;
        var cal = this.calendar,
            canEdit = cal.canEditEvent(event),
            canDrag = cal.canDragEvent(event),
            canResize = cal.canResizeEvent(event),
            canRemove = cal.canRemoveEvent(event)
        ;
        this.showCloseButton = canRemove;
        this.canDragReposition = canDrag;
        this.canDragResize = canResize;
        //this.dragTarget = this.calendarView.dragTarget;

        if (this.shouldShowGripper()) this.createGripper();
        else if (this.gripper) this.gripper.hide();
        if (this.shouldShowLabel()) this.createLabel();
        else if (this.label) this.label.hide();

        styleName = styleName || cal.getEventCanvasStyle(event, this.calendarView);
        this.setEventStyle(styleName, headerStyle, bodyStyle);
    },


    setDragProperties : function (canDragReposition, canDragResize, dragTarget) {
        this.canDragReposition = canDragReposition == null ? true : canDragReposition;
        this.canDragResize = canDragResize == null ? true : canDragResize;
        this.dragTarget = dragTarget;
    },

    setEventStyle : function (styleName, headerStyle, bodyStyle) {
        headerStyle = headerStyle || this.headerStyle || (styleName + "Header");
        bodyStyle = bodyStyle || this.bodyStyle || (styleName + "Body");
        this.baseStyle = styleName;
        this.styleName = styleName;
        this._bodyStyle = bodyStyle;
        this._headerStyle = headerStyle;
        if (this.gripper) {
            this.gripper.setStyleName(this.gripperStyle || styleName + "Gripper");
        }
        if (this.label) this.label.setStyleName(this.labelStyle || styleName + "Label");
        this.setStyleName(styleName);
    },


    getStartDate : function () {
        return this.calendar.getEventStartDate(this.event)
    },
    getEndDate : function () {
        return this.calendar.getEventEndDate(this.event)
    },
    getDuration : function () {
        return this.event[this.calendar.durationField];
    },

    // get event length in minutes
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
        if (textHeight || this.getShowBody()) {
            var definedHeight = this._getDefinedHeaderHeight(),
                width = this.getWidth() - (this.calendar.getLanePadding() * 2)
            ;
            var height = isc.EventCanvas.getHeaderHeight(this.getHeaderHTML(), width,
                    definedHeight, this.getHeaderWrap(), this
            );
            return height;
        } else {
            return this.getInnerHeight();
        }
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
    getHeaderCSSText : function (headerHeight) {
        var event = this.event,
            sb = isc.StringBuffer.create()
        ;



        var headerHeight = headerHeight || this.getHeaderHeight(),
            headerWrap = this.getHeaderWrap(),
            padding = this.padding != null ? this.padding : 0,
            paddingTop=0, paddingLeft=0, paddingBottom=0, paddingRight=0
        ;

        sb.append("position:absolute; top:", padding, "px; -moz-box-sizing:border-box; left:", paddingLeft, "px;");
        sb.append("width:100%; ");
        sb.append("height:", headerHeight, "px; ");

        sb.append("vertical-align:" + (this.headerPosition == "footer" ? "bottom; " : "middle; "));
        if (!this.vertical) sb.append("text-align:" + (this.getShowBody() ? "left; " : "center; "));
        if (!headerWrap) sb.append("text-wrap:none; ");

        if (event.headerTextColor) sb.append("color:", event.headerTextColor, ";");
        if (event.headerBackgroundColor) {
            sb.append("background-color:", event.headerBackgroundColor, ";");
        }
        return sb.release(false);
    },

    bodyHeight: "auto",
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
    getBodyCSSText : function (headerHeight) {
        var event = this.event,
            sb = isc.StringBuffer.create(),
            padding = this.padding != null ? this.padding : 0,
            paddingTop=0, paddingLeft=0, paddingBottom=0, paddingRight=0
        ;

        // if the header isn't showing, ignore it's height when calculating the bodyHeight
        if (!this.getShowHeader()) headerHeight = 0;

        var bodyHeight = this.getInnerHeight() - headerHeight - (padding*2) - (paddingTop+paddingBottom) ;
        sb.append("position:absolute; -moz-box-sizing:border-box; top:", headerHeight + paddingTop, "px; left:",
            paddingLeft, "px;");
        sb.append("width:100%; ");
        sb.append("height:", bodyHeight, "px; ");
        sb.append("vertical-align:top; ");
        if (event.textColor) sb.append("color:", event.textColor, ";");
        if (event.backgroundColor) {
            sb.append("background-color:", event.backgroundColor, ";");
        }
        return sb.release(false);
    },


// generating HTML

    divTemplate: [
        "<div class='",
        , // this.header/bodyStyle
        "' style='",
        , // header/body CSS - width/height/text/background color/margins, etc
        "'>",
        ,// getHeader/BodyHTML();
        "</div>"
    ],

    //> @method eventCanvas.getInnerHTML()
    // Returns the HTML to show in the EventCanvas as a whole.  By default, this method
    // generates one or two styled DIVs, depending on the values of
    // +link{eventCanvas.showHeader, showHeader} and +link{eventCanvas.showBody, showBody}.
    //
    // @return (HTMLString) the innerHTML for this canvas
    // @visibility external
    //<
    getInnerHTML : function () {
        var html = "",
            headerHTML = "",
            bodyHTML = "",
            showHeader = this.getShowHeader(),
            showBody = this.getShowBody(),
            showLabel = this.shouldShowLabel()
        ;
        if (this.event) {
            // if the event has a specified backgroundColor, set it directly on the styleHandle
            // so that an eventCanvas as a whole gets the same backgroudColor as the body
            var color = this.event.backgroundColor;
            if (color) {
                var handle = this.getStyleHandle();
                if (handle)
                    handle.backgroundColor = color;
            }

            var headerHeight = this.getHeaderHeight();
            var tempHeight = this.getHeight() - 2;
            var tableHTML = "<TABLE width='100%' height=" + tempHeight + " cellspacing='1' cellpadding='1' style='width:100%; height:" + tempHeight + "px; padding:0px; margin: 0px;'>";

            if (showHeader || showLabel) {
                var hT = this.divTemplate.duplicate();
                hT[1] = this.getHeaderStyle();
                hT[3] = this.getHeaderCSSText(headerHeight);

                hT[4] = "' eventPart='headerLabel'>"
                hT[5] = this.getHeaderHTML();
                headerHTML = hT.join("");
            }
            if (showBody) {
                var bT = this.divTemplate.duplicate();
                bT[1] = this.getBodyStyle();
                bT[3] = this.getBodyCSSText(headerHeight);
                bT[4] = "' eventPart='body'>";
                bT[5] = this.getBodyHTML();
                bodyHTML += bT.join("");
            }

            if (showLabel) {
                // show the title text in the separate label
                if (this.label) {
                    this.label.setContents(headerHTML);
                }
            }

            if (showHeader || showBody) {
                html = "";
                if (showHeader) {
                    html += headerHTML;
                }
                if (showBody) {
                    html += bodyHTML;
                }

                //if (this.headerPosition == "header") html = headerHTML
                //else if (this.headerPosition == "body") html = headerHTML;
                //else if (this.headerPosition == "footer") html = headerHTML;
            }


            if (!showLabel && !showHeader && !showBody) {
                // just write out the result of getHeaderHTML() into the main div of the Canvas
                html = this.getHeaderHTML();
            }

        }

        return html;
    },

    getHoverHTML : function () {
        if (this.calendarView.shouldShowEventHovers()) {
            return this.calendar._getEventHoverHTML(this.event, this, this.calendarView);
        }
    },

    // more helpers
    shouldShowCloseButton : function () {
        return this.showCloseButton != false;
    },
    shouldShowContextButton : function () {
        return this.showContextButton != false;
    },

    //> @attr eventCanvas.showRolloverControls (Boolean : true : IRW)
    // When set to the default value of true, this attribute causes a set of components to be
    // shown when the mouse rolls over this EventCanvas.  These components include the
    // +link{calendar.eventCanvasCloseButton, close} and
    // +link{calendar.eventCanvasContextButton, context} buttons, the latter's
    // +link{calendar.eventCanvasContextMenu, context menu} and the images used for
    // drag-resizing.
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
        if (isc.isA.Number(eWidth) && isc.isA.Number(eHeight)) {
            this.resizeTo(Math.round(eWidth), Math.round(eHeight));
        }
        if (isc.isA.Number(eTop) && isc.isA.Number(eLeft)) {
            this.moveTo(Math.round(eLeft), Math.round(eTop));
        }

        // get the styleName at render time - may have been dropped into a lane or sublane that
        // specifies a style for all of its events
        this.checkStyle();

        if (!this.parentElement || !this.parentElement.isDrawn()) return;

        if (!this.isDrawn()) this.draw();
        this.show();
        if (sendToBack) this.sendToBack();
        else this.bringToFront();

        if (this.shouldShowGripper() || this.shouldShowLabel()) {
            this.repositionPeers(true);
        }
    },
    checkStyle : function () {
        var styleName = this.calendar.getEventCanvasStyle(this.event, this.calendarView);
        if (styleName != this.styleName) this.setEventStyle(styleName);
    },

// internal stuff - mouse handler
    click : function () {
        // call the calendar-level handler, which will call the public eventClick()
        // notification as required
        this.calendar._eventCanvasClick(this)
    },

    mouseUp : function () {
        return isc.EH.STOP_BUBBLING;
    },

    mouseDown : function () {
        if (this.dragTarget) this.dragTarget.eventCanvas = this;
        this.calendar.eventDialog.hide();
        return isc.EH.STOP_BUBBLING;
    },

    mouseOver : function () {
        // see showRolloverControls - if set, call the Calendar API to show the controls - if
        // useEventCanvasRolloverControls is false, show the controls permanently
        if (!this.showRolloverControls || !this.calendar.useEventCanvasRolloverControls) return;
        if (this._rolloverControls && this._rolloverControls.length > 0) {
            var lastCanvas = isc.EH.lastEvent.target;
            if (lastCanvas == this || lastCanvas.eventCanvas == this) return;
        }
        this.calendar.showEventCanvasRolloverControls(this);
    },

    mouseOut : function () {
        // see showRolloverControls - if set, call the Calendar API to hide the controls
        if (!this.showRolloverControls || !this.calendar.useEventCanvasRolloverControls) return;
        var target = isc.EH.lastEvent.target;
        if (target && (target.eventCanvas == this || target == isc.Hover.hoverCanvas)) return;
        // hide rollover controls
        this.calendar.hideEventCanvasRolloverControls(this);
    },


    //parentResized : function () {
    //    this.Super('parentResized', arguments);
    //    // need to resize the event window here (columns are usually auto-fitting, so the
    //    // available space probably changed if the calendar as a whole changed size)
    //    if (this.event) this.calendarView.sizeEventCanvas(this);
    //},

    destroy : function () {
        if (!this.calendar.useEventCanvasRolloverControls && this._rolloverControls) {
            for (var i=this._rolloverControls.length-1; i>=0; i--) {
                var widget = this._rolloverControls[i];
                this._rolloverControls.removeAt(i);
                this.removeChild(widget);
                widget.destroy();
                widget = null;
            }
        }
        if (this.gripper) {
            this.gripper.destroy();
            this.gripper = null;
        }
        if (this.label) {
            this.label.destroy();
            this.label = null;
        }
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
        var output = isc.StringBuffer.create(),
            cal = this.calendar,
            view = this.calendarView,
            isTimeline = view.isTimelineView(),
            viewBody = view.body,
            bodyVOffset = 0,
            winTop = this.getTop(),
            bodyTop =  viewBody.getTop(),
            top = (winTop) + bodyVOffset + 1,
            widths = viewBody._fieldWidths,
            left = view.getLeft() + viewBody.getLeft() +
                        (view.getEventLeft ? view.getEventLeft(this.event) :
                            cal.getEventLeft(this.event, view)),
            width = this.getInnerWidth(),
            height = this.getInnerHeight() - 1,
            i = (printProperties && printProperties.i ? printProperties.i : 1)
        ;

        var startCol = cal.getEventStartCol(this.event, this, this.calendarView),
            endCol = cal.getEventEndCol(this.event, this, this.calendarView)
        ;

        var cal = this.calendar,
            calTop = cal.getTop(),
            calPageTop = cal.getPageTop(),
            viewTop = view.getTop(),
            viewPageTop = view.getPageTop(),
            bodyTop = view.body.getTop() + view.header.getHeight()
        ;
        if (isTimeline) {
            top = this.getTop() + bodyTop + 2;

            left = this.getLeft() + (view.frozenBody ? view.frozenBody.getVisibleWidth() : 0);
        } else {
            left = this.getLeft() + (view.frozenBody ? view.frozenBody.getVisibleWidth() : 0) + cal.getLeft() + view.getLeft();
            top = this.getTop() + bodyTop + 1;
        }

        var baseStyle = this.styleName;

        output.append("<div class='", baseStyle, "' ",
            "style='vertical-align: ",
            (cal.showEventDescriptions ? "top" : "middle"), "; ",
            "overflow:hidden; ",
            "position: absolute; ",
            "left:", left, "px; top:", top, "px; width: ", width, "px; height: ", height, "px; ",
            "z-index:", i+2, ";",
            "'>"
        );
        output.append(this.getInnerHTML());
        output.append("</div>");

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
// @treeLocation  Client Reference/Calendar
// @visibility external
//<
isc.defineClass("ZoneCanvas", "EventCanvas");
isc.ZoneCanvas.addProperties({
    headerPosition: "footer",
    showHeader: false,
    showBody: false,
    canEdit: false,
    canDrag: false,
    canDragReposition: false,
    canDragResize: false,
    canRemove: false,
    showRolloverControls: false,
    // allow the view to show it's hover text (for cells) when this canvas gets mouse moves
    //_mouseTransparent: true,
    initWidget : function () {
        this.showCloseButton = false;
        this.canDragReposition = false;
        this.canDragResize = false;
        // _mouseTransparent sets the eventProxy for this canvas to the containing calendarView
        // - causes cellHovers to be shown instead of zoneHovers
        this._mouseTransparent = !this.calendarView.shouldShowZoneHovers();
        this.Super("initWidget", arguments);
    },
    getInnerHTML : function () {
        var sb = isc.StringBuffer.create();
        sb.append("<div class='", this.getHeaderStyle(),
            "' style='position:absolute;bottom:0;width:100%;'>", this.event.name, "</div>"
        );
        return sb.release(false);
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
// @treeLocation  Client Reference/Calendar
// @visibility external
//<
isc.defineClass("IndicatorCanvas", "EventCanvas");
isc.IndicatorCanvas.addProperties({
    // by default, don't show the standard header or body DIVs - instead, use
    // headerPosition: "adjacent" and headerSnapTo: "B"
    showHeader: false,
    showBody: false,
    headerPosition: "adjacent",
    headerSnapTo: "B",
    // show a "gripper" peer, top-aligned, that moves the IndicatorCanvas when dragged
    showGripper: true,
    showLabel: true,

    canEdit: false,
    canDrag: true,
    canDragReposition: true,
    canDragResize: false,
    canRemove: false,
    showRolloverControls: false,
    initWidget : function () {
        this.showCloseButton = false;
        this.canDragReposition = true;
        this.canDragResize = false;
        this.Super("initWidget", arguments);
    },

    setEvent : function (event, styleName, headerStyle, bodyStyle) {
        this.event = event;
        // make the canvas non-interactive, apart from hover prompt
        this.showCloseButton = false;
        this.canDragReposition = true;
        this.canDragResize = false;
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
    }
});

// Call the AutoTest method to apply Calendar-specific methods now we've loaded
isc.AutoTest.customizeCalendar();







//>    @class Timeline
// Timeline is a trivial subclass of +link{Calendar} that configures the Calendar with settings
// typical for a standalone timeline view: hides the +link{calendar.dayView, day},
// +link{calendar.weekView, week} and +link{calendar.monthView, month} tabs and the
// +link{calendar.controlsBar, controls bar} by default.
// <P>
// Note that the +link{group:loadingOptionalModules, Calendar module} must be loaded to make
// use of the Timeline class.
//
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
eventDragGap: 0

});
isc._debugModules = (isc._debugModules != null ? isc._debugModules : []);isc._debugModules.push('Calendar');isc.checkForDebugAndNonDebugModules();isc._moduleEnd=isc._Calendar_end=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc.Log&&isc.Log.logIsInfoEnabled('loadTime'))isc.Log.logInfo('Calendar module init time: ' + (isc._moduleEnd-isc._moduleStart) + 'ms','loadTime');delete isc.definingFramework;if (isc.Page) isc.Page.handleEvent(null, "moduleLoaded", { moduleName: 'Calendar', loadTime: (isc._moduleEnd-isc._moduleStart)});}else{if(window.isc && isc.Log && isc.Log.logWarn)isc.Log.logWarn("Duplicate load of module 'Calendar'.");}

/*

  SmartClient Ajax RIA system
  Version v10.0p_2015-01-04/LGPL Deployment (2015-01-04)

  Copyright 2000 and beyond Isomorphic Software, Inc. All rights reserved.
  "SmartClient" is a trademark of Isomorphic Software, Inc.

  LICENSE NOTICE
     INSTALLATION OR USE OF THIS SOFTWARE INDICATES YOUR ACCEPTANCE OF THE
     SOFTWARE LICENSE AGREEMENT. If you have received this file without an 
     Isomorphic Software license file, please see:

         http://www.isomorphic.com/licenses/license-sisv.html

     You are not required to accept this agreement, however, nothing else
     grants you the right to copy or use this software. Unauthorized copying
     and use of this software is a violation of international copyright law.

  PROPRIETARY & PROTECTED MATERIAL
     This software contains proprietary materials that are protected by
     contract and intellectual property law. YOU ARE EXPRESSLY PROHIBITED
     FROM ATTEMPTING TO REVERSE ENGINEER THIS SOFTWARE OR MODIFY THIS
     SOFTWARE FOR HUMAN READABILITY.

  CONTACT ISOMORPHIC
     For more information regarding license rights and restrictions, or to
     report possible license violations, please contact Isomorphic Software
     by email (licensing@isomorphic.com) or web (www.isomorphic.com).

*/

