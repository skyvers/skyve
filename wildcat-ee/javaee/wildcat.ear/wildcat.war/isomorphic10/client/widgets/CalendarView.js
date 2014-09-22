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

    
    
	//>	@attr calendarView.useEventCanvasPool (Boolean : true : IRW)
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
            rowNum = this.getEventRow(),
            colNum = this.getEventColumn(),
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
        for (var i=0; i<events.length; i++) {
            var event = events[i],
                isValid = event.isEventCanvas || event.isZoneCanvas || event.isIndicatorCanvas;
            if (!isValid) continue;
            if (!event.isDrawn() || !event.isVisible()) continue;
            var nextHTML = event.getPrintHTML(printProperties);
            output.append(nextHTML);
        }

        output.append("</div>");
        
        var result = output.release(false);

        if (callback) {
            this.fireCallback(callback, "HTML", [result]);
        }

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
                snapY = (Math.floor((mouseY - rowTop) / cal.eventSnapGap) * cal.eventSnapGap),
                snapTop = isTL ? rowTop : Math.min(props._maxTop, rowTop + snapY),
                oldHeight = this.getVisibleHeight(),
                newHeight = oldHeight
            ;

            // left/width -related
            var eventCol = Math.min(props._maxCol, gr.getEventColumn()),
                columnLeft = gr.getColumnLeft(eventCol),
                offsetX = (gr.getOffsetX() - props._startOffsetX),
                tempLeft = Math.max(0, offsetX - ((offsetX - columnLeft) % cal.eventSnapGap) + 1),
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
                width = view._getEventBreadth({ startDate: startDate, endDate: endDate });
            }

            /*
            var allowResize = true;
            if (endDate.getTime() <= startDate.getTime()) {
                // invalid endDate, earlier than start date - just disallow - should leave the
                // default minimum size (the eventSnapGap)
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
            var minWidth = cal.eventSnapGap;
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

        var addThese = [];
        
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
                // if an event is already in the _drawnEvents array, ignore it and remove it
                // from the clearThese array
                clearThese.remove(event);
                if (this.isGrouped) {
                    // if we're grouped and the canvas was already visible, we need to 
                    // reposition it, in case an earlier group node was expanded or collapsed
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
        
        // redraw any zones and indicators in the background
        this.drawZones();
        this.drawIndicators();

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
            
            var eventStart = cal.getEventLeadingDate(event) || cal.getEventStartDate(event),
                eventEnd = cal.getEventTrailingDate(event) || cal.getEventEndDate(event),
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
                this.sizeEventCanvas(canvas);
                if (this.body) this.body.addChild(canvas);
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
            if (!isc.isA.String(event)) {
                // if shouldShowEvent() is implemented and returns false, skip the event
                if (cal.shouldShowEvent(event, this) == false) continue;
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
            offsetY = y - rowTop,
            pixels = offsetY - (offsetY % cal.eventSnapGap),
            snapGapMins = minsPerRow / (rowHeight / cal.eventSnapGap),
            snapGaps = pixels / cal.eventSnapGap,
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
        var millis = date.getTime(),
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
                if (rowHeight / this.calendar.eventSnapGap != 1) {
                    var mins = Math.floor(rMillis / 1000 / 60),
                        snapGapMins = this.getRowHeight(i) / this.calendar.eventSnapGap,
                        extraPixels = Math.floor((mins / snapGapMins) * this.calendar.eventSnapGap)
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
        var snapGap = this.creator.eventSnapGap;
        this.body.snapVGap = Math.round((snapGap / this.creator.getMinutesPerRow(this)) 
                * this.body.getRowSize(0));
        this.body.snapHGap = null;
    },
    
    // To be used with calendar.scrollToWorkday 
    scrollToWorkdayStart : function () {
        var cal = this.calendar;

        if (cal.scrollToWorkday && !this.hasLanes()) {
            var newRowHeight = this.calcRowHeight();
            if (newRowHeight != cal.rowHeight) {
                cal.setRowHeight(newRowHeight);
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
        var rHeight = Math.floor(this.body.getViewportHeight() / 
                (workdayLen * this.calendar.getRowsPerHour()));
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
    
    getCellDate : function (rowNum, colNum) {
        if (rowNum == null && colNum == null) {
            rowNum = this.getEventRow();
            colNum = this.getEventColumn();
        }
        if (rowNum < 0 || colNum < 0) return null;
        var fieldIndex = this.body.fields.get(colNum)._dayIndex,
            record = this.getRecord(rowNum),
            cellDate = record["date" + fieldIndex]
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

        this._rebuild(true);
        
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
        
        var p = { lane: lane, sublane: sublane, startDate: startDate, endDate: endDate, 
                    top: null, height: null };
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

        if (this._lastOverLaneIndex != null && this._lastOverLaneIndex != rowNum) {
            this.refreshRow(this._lastOverLaneIndex);
        }
        this._lastOverLaneIndex = rowNum;

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

            var span = { unit: unit, startDate: date, endDate: newDate,
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

    showField : function () {
        this.Super("showField", arguments);
        this.refreshEvents();
    },
    hideField : function () {
        this.Super("hideField", arguments);
        this.refreshEvents();
    },

    getCellStyle : function (record, rowNum, colNum) {
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

        // over styling
        var overRow = this.getEventRow();
        if (rowNum != null && overRow != null && rowNum == overRow) bStyle += "Over";

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
        
        if (useSnapGap) {
            // when click/drag creating, we want to snap to the eventSnapGap
            var r = x % this.creator.eventSnapGap;
            if (r) x -= r;
        }

        var date = this.body.fields[colNum].date,
            colLeft = this.body.getColumnLeft(colNum),
            delta = x - colLeft,
            snapGaps = Math.floor(delta / cal.eventSnapGap)
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
                            (mins == startMins ? 0 : (snapsToAdd * cal.eventSnapGap))
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


