/*
 * Isomorphic SmartClient
 * Version v10.0p_2015-01-04 (2015-01-04)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */

 


//>	@class	GridRenderer
//
// A flexible, high-speed table that offers consistent cross-platform sizing, clipping, and events.
//
//  @treeLocation Client Reference/Foundation
//  @visibility external
//<

isc.ClassFactory.defineClass("GridRenderer", "Canvas");

isc.GridRenderer.addClassProperties({

	//>	@type	CellState	
	// Appearance of the cell -- used to set to different visual states.
	// Also used as a suffix to gridRenderer.baseStyle to set CSS styles.
	// @group appearance
	// @see	gridRenderer.getCellStyle()
	SELECTED:"Selected",		//	@value	isc.gridRenderer.SELECTED		Cell is selected.
	DISABLED:"Disabled",		//	@value	isc.gridRenderer.DISABLED		Cell is disabled.
	OVER:"Over",				//	@value	isc.gridRenderer.OVER			Mouse is over the cell.
	//<

    //>	@attr	gridRenderer.standardStyleSuffixes    (array : array of strings : IR)
	//		Array of the 12 standard cell style suffix strings ("Over", "SelectedOver", etc.)
    //      to allow quicker calculation of cell styles.
	//		@see	gridRenderer.getCellStyle()
    //<
    standardStyleSuffixes:[
                 "",
                 "Over",
                 "Selected",
                 "SelectedOver",
                 "Disabled",
                 "DisabledOver",
                 "DisabledSelected",
                 "DisabledSelectedOver",
                 "Dark",
                 "OverDark",
                 "SelectedDark",
                 "SelectedOverDark",
                 "DisabledDark" 
     ]
});

isc.GridRenderer.addProperties({
showFocusOutline: false,

//>	@attr gridRenderer.totalRows (number : 0 : [IRW])
// Total number of rows in the grid.<br><br>
//
// NOTE: in order to create a valid grid, you must either provide a totalRows value or implement
// getTotalRows()
//
// @see method:getTotalRows
// @visibility external
//<
totalRows : 0,
 
//>	@attr gridRenderer.showAllRows (boolean : false : [IRA])
// Whether all rows should be drawn all at once, or only rows visible in the viewport.
// <P>
// Drawing all rows causes longer initial rendering time, but allows smoother vertical scrolling.
// With a very large number of rows, showAllRows will become too slow.
// <P>
// See also +link{drawAheadRatio} and +link{drawAllMaxCells}.
//
// @group performance
// @visibility external
//<
//showAllRows : false,

//>	@attr gridRenderer.virtualScrolling (boolean : null : [IRA])
// When incremental rendering is switched on and there are variable record heights, the virtual
// scrolling mechanism manages the differences in scroll height calculations due to the
// unknown sizes of unrendered rows to make the scrollbar and viewport appear correctly.
// <P>
// virtualScrolling is switched on automatically when fixedRowHeights is false but you should 
// switch it on any time there are variable record heights.
// @visibility external
//<
//virtualScrolling:true,

//>	@attr gridRenderer.showAllColumns (boolean : false : [IRA])
// Whether all columns should be drawn all at once, or only columns visible in the viewport.
// <P>
// Drawing all columns causes longer initial rendering time, but allows smoother horizontal
// scrolling.  With a very large number of columns, showAllColumns will become too slow.
//
// @group performance
// @visibility external
//<
//showAllColumns : false, 

//>@attr gridRenderer.drawAllMaxCells (integer : 250 : IRWA)
// If drawing all rows would cause less than <code>drawAllMaxCells</code> cells to be rendered,
// the full dataset will instead be drawn even if +link{listGrid.showAllRecords,showAllRecords}
// is false and the viewport size and +link{drawAheadRatio} setting would normally have caused
// incremental rendering to be used.
// <P>
// The <code>drawAllMaxCells</code> setting prevents incremental rendering from being used in
// situations where it's really unnecessary, such as a 40 row, 5 column dataset (only 200
// cells) which happens to be in a grid with a viewport showing only 20 or so rows.
// Incremental rendering causes a brief "flash" during scrolling as the visible portion of the
// dataset is redrawn, and a better scrolling experience can be obtained in this situation by
// drawing the entire dataset up front, which in this example would have negligible effect on
// initial draw time.
// <P>
// <code>drawAllMaxCells:0</code> disables this features.  You may want to disable this feature
// if performance is an issue and:
// <ul>
// <li> you are very frequently redraw a grid
// <li> you do a lot of computation when rendering each cell (eg formulas)
// <li> you are showing many grids on one screen and the user won't scroll most of them
// </ul>
//
// @group performance
// @visibility external
//<
drawAllMaxCells:250,


//> @attr gridRenderer.recordCanSelectProperty (string : "canSelect" : [IRA])
// If set to false on a record, selection of that record is disallowed.
//<
recordCanSelectProperty: "canSelect",

//> @attr gridRenderer.isSeparatorProperty (string : "isSeparator" : [IRA])
// If this property is defined on some record, render the record as a separator row.
//<
// Documented at the ListGrid level only. ListGrids will pass the isSeparatorProperty through
// to their body.
isSeparatorProperty:"isSeparator",

//> @attr gridRenderer.singleCellValueProperty (string : "singleCellValue" : [IRA])
// If this property is defined on some record, render the record as a single cell (spanning all
// columns).
//<
// Documented at the ListGrid level only. ListGrids also implement getCellValue() which
// will display record.singleCellValue as the value of the cell.
// ListGrids will pass the singleCellValueProperty through to their body.
singleCellValueProperty:"singleCellValue",

//> @attr gridRenderer.instantScrollTrackRedraw (boolean : true : IRW)
// If true, if the user clicks on the scroll buttons at the end of the track or clicks once on
// the scroll track, there will be an instant redraw of the grid content so that the user
// doesn't see any blank space.  For drag scrolling or other types of scrolling, the
// +link{scrollRedrawDelay} applies.
//
// @group performance
// @visibility external
//<
instantScrollTrackRedraw:true,

//> @attr gridRenderer.scrollRedrawDelay (integer : 75 : IRW)
// While drag scrolling in an incrementally rendered grid, time in milliseconds to wait
// before redrawing, after the last mouse movement by the user. This delay may be 
// separately customized for mouse-wheel scrolling via +link{scrollWheelRedrawDelay}.
// <P>
// See also
// +link{gridRenderer.instantScrollTrackRedraw} for cases where this delay is skipped.
//
// @group performance
// @visibility external
//<
scrollRedrawDelay:75,

//> @attr gridRenderer.scrollWheelRedrawDelay (Integer : null : IRW)
// While scrolling an incrementally rendered grid, using the mouseWheel, time in 
// milliseconds to wait before redrawing, after the last mouseWheel movement by the user.  
// If not specified +link{scrollRedrawDelay} will be used as a default for both
// drag scrolling and mouseWheel scrolling.
// <P>
// See also
// +link{gridRenderer.instantScrollTrackRedraw} for cases where this delay is skipped.
//
// @group performance
// @visibility external
//<
//scrollWheelRedrawDelay:null,

//>	@attr gridRenderer.drawAheadRatio (float : 1.3 : [IRWA])
// How far should we render rows ahead of the currently visible area?  This is expressed as a
// ratio from viewport size to rendered area size.
// <P>
// Tweaking drawAheadRatio allows you to make tradeoffs between continuous scrolling speed vs
// initial render time and render time when scrolling by large amounts.
// <P>
// NOTE: Only applies when showAllRows is false.
//
// @group performance
// @visibility external
//<
drawAheadRatio : 1.3,

//>	@attr gridRenderer.quickDrawAheadRatio (float : 1.0 : [IRWA])
// Alternative to +link{drawAheadRatio}, to be used when the user
// is rapidly changing the grids viewport (for example drag scrolling through the grid).
// If unspecified +link{drawAheadRatio} will be used in all cases
// @group performance
// @visibility external
//<
quickDrawAheadRatio:1.0,
   
//>	@attr gridRenderer.cellHeight		(number : 20 : [IRW])
// The default height of each row in pixels.
//
// @see gridRenderer.getRowHeight()
// @visibility external
// @group cellStyling
//<
cellHeight:20,							

//>	@attr gridRenderer.fixedRowHeights (boolean : true : IRWA)
// Should we vertically clip cell contents, or allow rows to expand vertically to show all
// contents?
// <P>
// If we allow rows to expand, the row height as derived from
// +link{gridRenderer.getRowHeight(),getRowHeight()} or the
// default +link{cellHeight} is treated as a minimum.
// 
// @group cellStyling
// @visibility external
//<

// Note: We can have variable height rows even with fixedRowHeights set to true, if we
// have embedded components (which cause rows to expand) for example.

fixedRowHeights:true,
//enforceVClipping:null,
shouldEnforceVClipping : function () {
    return this.fixedRowHeights && (this.enforceVClipping || this.wrapCells);
},



//>	@attr	gridRenderer.fixedColumnWidths		(boolean : true : IRWA)
// Should we horizontally clip cell contents, or allow columns to expand horizontally to
// show all contents?<br><br>
//
// If we allow columns to expand, the column width is treated as a minimum.
//
// @group	sizing
// @visibility external
//<

fixedColumnWidths:true,

//>	@attr	gridRenderer.autoFit              (boolean : false : IRWA)
// If true, make columns only wide enough to fit content, ignoring any widths specified.
// Overrides fixedFieldWidths.
// 
// @group	sizing
// @visibility external
//<
//autoFit:false,

//>	@attr	gridRenderer.wrapCells		(boolean : false : IRWA)
// Should content within cells be allowed to wrap?
// @group cellStyling
// @visibility external
//<
//wrapCells:false,

//>	@attr	gridRenderer.cellSpacing		(number : 0 : [IRW])
// The amount of empty space, in pixels, between each cell.
// @group cellStyling
// @visibility internal
//<
cellSpacing:0,

//>	@attr	gridRenderer.cellPadding		(number : 2 : [IRW])
// The amount of empty space, in pixels, surrounding each value in its cell.
// @group cellStyling
// @visibility external
//<
cellPadding:2,

//>	@attr	gridRenderer.canSelectOnRightMouse  (boolean : true : [RW])
//  If true, rightMouseDown events will fire 'selectOnRightMouseDown()' for the appropriate cells.
// @group	events
// @visibility external
//<
canSelectOnRightMouse:true,

// Hover
// ---------------------------------------------------------------------------------------

//>	@attr gridRenderer.canHover (boolean : null : [RW])
// If true, cellHover and rowHover events will fire and then a hover will be shown (if not
// canceled) when the user leaves the mouse over a row / cell unless the corresponding field has
// +link{ListGridField.showHover,showHover} set to false. If unset or null, the hover will be
// shown if the corresponding field has showHover:true. If false, then hovers are disabled.
// <p>
// Note that standard hovers override +link{showClippedValuesOnHover,clipped value hovers}. Thus,
// to enable clipped value hovers, canHover must be unset or null and the corresponding field
// must have showHover unset or null as well.
// @group	events
// @visibility external
// @see cellHover()
// @see rowHover()
// @see showHover
// @see showClippedValuesOnHover
//<

//>	@attr gridRenderer.showHover (boolean : null : [RW])
// If true, and canHover is also true, when the user hovers over a cell, hover text will pop up
// next to the mouse.  The contents of the hover is determined by +link{cellHoverHTML()}.
// @group	events
// @visibility external
// @see canHover
// @see cellHoverHTML()
//<

//> @attr gridRenderer.showClippedValuesOnHover (Boolean : null : IRWA)
// If true and a cell's value is clipped, then a hover containing the full cell value is
// enabled.
// <p>
// Note that standard cell hovers override clipped value hovers. Thus, to enable clipped value
// hovers, +link{canHover,canHover} must be unset or null and the corresponding field must have
// +link{ListGridField.showHover,showHover} unset or null as well.
// @group events
// @see canHover
// @see cellValueHoverHTML()
// @visibility external
//<

// can be set to false to cause Hover to be per-row instead of per-cell
hoverByCell:true,

// CSS styles
// --------------------------------------------------------------------------------------------


backgroundColor:"white",

// style applied to the table element.  NOTE: don't expose: styling of a grid should be
// accomplish by styling the surrounding DIV, where we can use the standard methodology to
// detect borders, margins, etc (eg getInnerHeight())
tableStyle:"listTable",	

//>	@attr	gridRenderer.baseStyle		(CSSStyleName : "cell" : [IR])
// The base name for the CSS class applied to cells. This style will have "Dark", 
// "Over", "Selected", or "Disabled" appended to it according to the state of the cell.
// @visibility external
// @group cellStyling
// @see method:getCellStyle()
// @see method:getBaseStyle()
//<
baseStyle:"cell",

//> @attr gridRenderer.alternateRowStyles (boolean : false : [IRW])
// Whether alternating rows should be drawn in alternating styles, in order to create a "ledger"
// effect for easier reading.  If enabled, the cell style for alternate rows will have "Dark"
// appended to it.
// @visibility external
// @group cellStyling
//<
//alternateRowStyles:false,

//> @attr gridRenderer.alternateRowFrequency (number : 1 : [IRW])
// The number of consecutive rows to draw in the same style before alternating, when
// +link{gridRenderer.alternateRowStyles, alternateRowStyles} is true.
// @visibility external
// @group cellStyling
//<
alternateRowFrequency:1,

//> @attr gridRenderer.alternateColumnStyles (boolean : false : [IRW])
// Whether alternating columns should be drawn in alternating styles, in order to create a 
// vertical "ledger" effect for easier reading.  If enabled, the cell style for alternate 
// columns will have "Dark" appended to it.
// @visibility external
// @group cellStyling
//<
//alternateColumnStyles:false,

//> @attr gridRenderer.alternateColumnFrequency (number : 1 : [IRW])
// The number of consecutive columns to draw in the same style before alternating, when
// +link{gridRenderer.alternateColumnStyles, alternateColumnStyles} is true.
// @visibility external
// @group cellStyling
//<
alternateColumnFrequency:1,

//> @attr gridRenderer.emptyCellValue (string : "&nbsp;" : IRW)
// Value to show in empty cells (when getCellValue returns null).
// @group cellStyling
// @visibility external
//<
emptyCellValue:"&nbsp;",


// Empty messages (what to do if no data is present)
// --------------------------------------------------------------------------------------------
//>	@attr	gridRenderer.showEmptyMessage		(boolean : true : [IRW])
// Indicates whether the text of the emptyMessage property should be displayed if no data is
// available.
//      @visibility external
//      @group  emptyMessage
//      @see	emptyMessage
//<

//>	@attr	gridRenderer.emptyMessage		(string : "No data to display" : IRW)
// The string to display in the body of a listGrid with an empty data array, if
// showEmptyMessage is true.
// @group emptyMessage, i18nMessages
// @visibility external
//      @see	gridRenderer.showEmptyMessage
//      @see    gridRenderer.emptyMessageStyle
//<
emptyMessage: "No data to display",

//>	@attr gridRenderer.emptyMessageTableStyle (CSSStyleName : null : IRW)
// CSS styleName for the table as a whole if we're showing the empty message
// @group emptyMessage
// @visibility external
//<

//>	@attr gridRenderer.emptyMessageStyle (CSSStyleName : null : IRW)
// The CSS style name applied to the emptyMessage string if displayed.
// @group emptyMessage
// @visibility external
//<

//>	@attr gridRenderer.showOfflineMessage (boolean : true : [IRW])
// Indicates whether the text of the offlineMessage property should be displayed if no data
// is available because we are offline and there is no suitable cached response
// @visibility external
// @group emptyMessage, offlineGroup
// @see offlineMessage
//<

//>	@attr gridRenderer.offlineMessageStyle (CSSStyleName : null : IRW)
// The CSS style name applied to the offlineMessage string if displayed.
// @group emptyMessage, offlineGroup
// @visibility external
//<

//>	@attr gridRenderer.offlineMessage (string : "No data available while offline" : IRW)
// The string to display in the body of a listGrid with an empty data array, if
// showOfflineMessage is true and the data array is empty because we are offline and there
// is no suitable cached response
// @group offlineGroup, emptyMessage, i18nMessages
// @visibility external
// @see showOfflineMessage
// @see offlineMessageStyle
//<
offlineMessage: "No data available while offline",

//> @attr gridRenderer.fastCellUpdates (boolean: true : IRWA)
//
// <b>Note: This property only has an effect in Internet Explorer</b>
// <P>
// Advanced property to improve performance for dynamic styling of gridRenderer cells in
// Internet Explorer, at the expense of slightly slower initial drawing, and some 
// limitations on supported styling options.
// <P>
// <code>fastCellUpdates</code> speeds up the dynamic styling system used by rollovers,
// selections, and custom styling that calls +link{gridRenderer.refreshCellStyle()}, at
// the cost of slightly slower draw() and redraw() times.
// <P>
// Notes:
// <ul>
// <li>When this property is set, ListGrid cells may be styled using the 
//     +link{listGrid.tallBaseStyle}. See +link{listGrid.getBaseStyle()} for 
//     more information.</li>
// <li>If any cell styles specify a a background image URL, the URL will be resolved relative
//     to the page location rather than the location of the CSS stylesheet. This means cell
//     styles with a background URL should either supply a fully qualified path, or the
//     background image media should be made available at a second location for IE.</li>
// <li>fastCellUpdates will not work if the styles involved are in an external stylesheet loaded
//     from a remote host. Either the stylesheet containing cell styles needs to be loaded
//     from the same host as the main page, or the cell styles need to be inlined in the html 
//     of the bootstrap page.</li>
// <li>fastCellUpdates will not work if the css styles for cells are defined in
//     a <code>.css</code> file loaded via <code>@import</code>. Instead the <code>.css</code>
//     file should be loaded via a <code>&lt;link ...&gt;</code> tag.</li>
// </ul>
// @visibility external
//<

// This default is overridden in ListGrid
fastCellUpdates:true,

//> @method gridRenderer.setFastCellUpdates()
// Setter for +link{gridRenderer.fastCellUpdates}. Has no effect in browsers other than
// Internet Explorer.
// @param fastCellUpdates (boolean) whether to enable fastCellUpdates.
// @visibility external
//<
setFastCellUpdates : function (fcu) {
    if (fcu && !isc.Browser.isIE) {
        this.fastCellUpdates = false;
        //this.logInfo("fastCellUpdates was enabled - this has no effect " +
        //    "in browsers other than Internet Explorer. Disabling.");
        return;
    }
    if (fcu == this.fastCellUpdates) return;
    this.fastCellUpdates = fcu;
    this.markForRedraw();
},




// Standard Canvas settings
// --------------------------------------------------------------------------------------------
overflow:"auto",


_avoidRedrawFlash:true,

canFocus:true

//>Animation
// If a px / second speed is specified for a row animation, cap it at a maximum
// (Inherited from LG / TG if specified there)
,animateRowsMaxTime:1000,
//<Animation


//>	@attr gridRenderer.snapToCells (boolean : false : IRW)
// Should drag-and-drop operations snap the dragged object into line with the nearest cell?
//
// @group dragdrop
// @visibility external
//<
snapToCells: false,
	
//> @attr gridRenderer.snapInsideBorder (boolean : false : IRW)
// If true, snap-to-cell drops will snap the dropped object inside the selected cell's border.
// If false, snap-to-cell drops will snap the dropped object to the edge of the selected cell, 
// regardless of borders
//
// @group dragdrop
// @see	GridRenderer.snapToCells
// @visibility external
//<
snapInsideBorder: false,


snapHDirection: isc.Canvas.BEFORE,
snapVDirection: isc.Canvas.BEFORE

});

isc.GridRenderer.addMethods({

initWidget : function () {
    // Make sure we have columnWidths set up - we rely on this for some methods
    if (!this._fieldWidths) this.setColumnWidths([]);

    if (this.selection) this.setSelection(this.selection);

    // If we're overflow visible, we have to write out our entire body content.
    if (this.overflow == isc.Canvas.VISIBLE) {
        this.showAllRows = true;
    }

    // turn virtualScrolling on if it's unset and we have variable-height rows
    if (!this.fixedRowHeights && this.virtualScrolling == null) this.virtualScrolling = true;
    // default for virtualScrolling is now null, so if it's been turned on, also turn 
    // fixedRowHeights off so that virtualScrolling has an effect
    //if (this.fixedRowHeights && this.virtualScrolling) this.fixedRowHeights = false;
    if (!this.fixedRowHeights && !this.showAllRows) {
        
        //this._avoidRedrawFlash = true;

        if (this.showCustomScrollbars == false) {
            this.logInfo("Variable height records cannot be used with native scrollbars;" + 
                         " setting showCustomScrollbars:true on this GridRenderer and using" +
                         " the special 'NativeScrollbar' class as a scrollbarConstructor.");
            this.showCustomScrollbars = true;
            this.useNativeTouchScrolling = false;
            this.scrollbarConstructor = "NativeScrollbar";
        }

    }

    // initialize fastCellUpdates via the setter.
    // Disables this attribute where not supported
    this.setFastCellUpdates(this.fastCellUpdates);
},


removeColumnSizerDelay: 100,
removeColumnSizer : function(removeCompletely) {
    if (this._columnSizer) {
        if (removeCompletely) {
            this._columnSizer.clear();
            delete this._columnSizer;
        } else {
            this._columnSizer.setContents("&nbsp;");
        }
    }
},

destroy : function () {
    this.clearSelection();
    this.removeColumnSizer(true);
    this.Super("destroy", arguments);
},

shouldShowAllColumns : function () {

    if (this.showAllColumns) return true;
    
    // have to force rendering all columns, otherwise, row heights would vary with drawn
    // columns.
    
    if (!this.fixedRowHeights && !this.showAllRows) return true;
    if (this.overflow == isc.Canvas.VISIBLE) {
        return true;
    }
    return false;
},

// Empty Message handling
// --------------------------------------------------------------------------------------------

isEmpty : function () { return false; },
 
_showEmptyMessage : function (startCol,endCol) {
    return this.getEmptyMessageHTML(startCol,endCol, this.grid.isOffline());
},

//>	@method	gridRenderer.getEmptyMessageHTML()	([A])
//			Return the HTML to show if there's nothing in the list
//		@group	drawing
//		@return	(HTMLString)	HTML for the empty message layer
//<
getEmptyMessageHTML : function (startCol,endCol,offline) {
    if (!offline) {
        if (!this.showEmptyMessage)	return "&nbsp;";
    } else {
        if (!this.showOfflineMessage) return "&nbsp;";
    }
    if (this.isPrinting) { 
        if (startCol == null) startCol = 0;
        if (endCol == null) endCol = this.fields ? this.fields.getLength() -1 : 0;
        
        
        var showHeader = !(this._printingChunk && this.printChunkOnly);
        
        return "<TABLE role='presentation' cellspacing=0 style='width:100%'" +
                (this.emptyMessageTableStyle?(" class='" + this.emptyMessageTableStyle + "'"):"") +
                ">" +
                (showHeader ? this.getPrintHeaders(startCol, endCol+1) : "") + 
                "<TR><TD  ALIGN=CENTER class='" + 
                (offline ? this.offlineMessageStyle : this.emptyMessageStyle) +
                "' colspan='" + ((endCol-startCol)) + "'>" +
                (offline ? this.getOfflineMessage() : this.getEmptyMessage())
                + "</TD></TR>" +
                (showHeader ? this.getPrintFooters(startCol, endCol+1) : "") + 
                "</TABLE>";
    }
    
    // Always ensure the empty message fills the viewport.
    // Respect flag to match the empty message size to the specified field widths - 
    // if the specified field sizes exceed the viewport size, expand the empty message
    // to accommodate it.
    var width = this.getInnerWidth(),
        extraWidth = 0;
    if (this.expandEmptyMessageToMatchFields && this._fieldWidths) {
        extraWidth = this._fieldWidths.sum() - width;
        if (extraWidth < 0) extraWidth = 0;
    }
    if (this.applyHSpaceToEmptyMessage) {
        if (this.leftSpace != null) extraWidth += this.leftSpace;
        if (this.rightSpace != null) extraWidth += this.rightSpace;
    }

    // Note that if the GR is scrollable, we want the empty message to be visible / 
    // centered when scrolled to 0/0 so table into 2 cells, centering the empty message
    // within the first cell which spans the viewport
    var splitTable = extraWidth && this.overflow != isc.Canvas.VISIBLE;

    var vPad = this._vPad;
    // this._vPad may be undefined (for example, when drawing the "[Empty menu]" submenu).
    if (vPad == null) {
        vPad = 0;
        
        if (isc.Browser.isStrict &&
            ((isc.Browser.isMoz && isc.Browser.version >= 17) ||
             (isc.Browser.isSafari || isc.Browser.isIE)))
        {
            vPad = (this.fixedRowHeights ? 0 : this.cellPadding * 2);
            if (this.emptyMessageTableStyle) vPad += isc.Element._getVBorderSize(this.emptyMessageTableStyle);
        }
    }

    var sb = isc.StringBuffer.create();
    sb.append(
			"<TABLE role='presentation' BORDER=0 MARGIN=0 CELLSPACING='", this.cellSpacing,
                "' CELLPADDING='", this.cellPadding,
                (this.emptyMessageTableStyle?("' CLASS='" + this.emptyMessageTableStyle):null),
                "' style='width:",(width+extraWidth),"px;",
                
                (isc.Browser.isSafari ? "'"
                                      : "' HEIGHT=100%"),
            "><TR><TD ALIGN=CENTER CLASS='",
            (offline ? this.offlineMessageStyle : this.emptyMessageStyle),
            "' style='padding-left:0px;padding-right:0px;height:" + (this.cellHeight - vPad) + "px'>",
            // NOTE: empty message can't be too tall, or it will introduce vscrolling in
            // shorter grids
			(offline ? this.getOfflineMessage() : this.getEmptyMessage()),
            (extraWidth && splitTable ? "<br>" + isc.Canvas.spacerHTML(width,1) : null),
			"</TD>"
    );
    if (extraWidth && splitTable) {
        sb.append("<TD style='padding-left:0px;padding-right:0px;'>",
                  isc.Canvas.spacerHTML(extraWidth, 1), "</TD>");
    }
    sb.append("</TR></TABLE>");
    return sb.release(false);
},


//>	@method	gridRenderer.getEmptyMessage()	([A])
//		@group	drawing
//			return the text for the empty message
//			you can ovveride this function if your data has additional semantics
//				(eg: initial conditions, loading, filtering, etc)
//		@return	(string)	empty message
//<
getEmptyMessage : function () {
	return this.emptyMessage;
},

//>	@method	gridRenderer.getOfflineMessage()	([A])
//		@group	drawing
//			return the text for the offline message.  The default implementation returns the
//          value of the containing Grid's offlineMessage property;
//			you can ovveride this function if your data has additional semantics
//				(eg: initial conditions, loading, filtering, etc)
//		@return	(string)	offline message
//<
getOfflineMessage : function () {
	return this.grid.offlineMessage;
},

// Drawing
// --------------------------------------------------------------------------------------------



// use a rel-pos div to apply a z-index to the content.
// required for the ability to float embedded components above or below the table
_$zIndexDivTemplate:["<DIV id='",
                     ,  // 1 [_getZIndexDivID()]
                     "' style='" +
                        
                        (isc.Browser.isMoz && isc.Browser.version == 18 ? "display:inline;" : "") +
                        
                        "position:relative;z-index:",
                     ,  // 3 [getTableZIndex()]
                     ";width:",
                     ,  // 5 width
                     "px'>",
                     ,  // 7 [table html]
                     ,  // 8 [optional filler div]
                     "</DIV>"],
_$fillerDiv:"<table style='position:absolute;top:0px;font-size:1px;height:100%;width:100%;z-index:1;overflow:hidden;visibility:hidden;'><tr><td>&nbsp;</td></tr></table>",
_getZIndexDivID : function () {
    return this._getDOMID("zIndexDiv");
},
_getZIndexDiv : function () {
    return this.getDocument().getElementById(this._getZIndexDivID());
},
getInnerHTML : function () {
    var tableHTML = this.getTableHTML(),
        template = this._$zIndexDivTemplate;
    template[1] = this._getZIndexDivID();
    template[3] = this.getTableZIndex();
    var width = this._fieldWidths.sum() - this.getHBorderSize();
    // When the grid is (or will) show the sorter, but the grid body is not showing a
    // vertical scrollbar, we want to increase the scrollWidth of the grid body by the width
    // of the sorter button so that if the last field is wider than the viewport width, its
    // header button won't be partially occluded by the sorter.
    //
    // This requires innerSizeChanged() to be overridden to update the CSS width whenever the
    // grid body shows or hides a vertical scrollbar.
    var grid = this.grid;
    if (!this.frozen && grid != null && grid._showSortButton() && !this.vscrollOn) {
        width += grid._getSorterWidth();
    }
    template[5] = width;
    template[7] = tableHTML;
    
    if (isc.Browser.isMoz) template[8] = this._$fillerDiv;
    return template.join(isc.emptyString);
},

innerSizeChanged : function (reason, b,c,d) {
    var returnVal = this.invokeSuper(isc.GridRenderer, "innerSizeChanged", reason, b,c,d);

    var zIndexDiv = this._getZIndexDiv();
    if (zIndexDiv != null) {
        var width = this._fieldWidths.sum() - this.getHBorderSize();
        if (!this.frozen && this.grid != null && this.grid._showSortButton() && !this.vscrollOn) {
            width += this.grid._getSorterWidth();
        }
        zIndexDiv.style.width = width + isc.px;
    }

    return returnVal;
},

isFastScrolling : function () {
    return this.isDragScrolling() || this.isRepeatTrackScrolling() || this.isMouseWheelScrolling();
},

shouldUseQuickDrawAheadRatio : function () {
    // NOTE: useQuickDrawAheadRatio is a flag you can flip on temporarily when there's some
    // reason other than fast scrolling that you want quick redraws (eg column drag resize)
    return this.useQuickDrawAheadRatio || this.isFastScrolling();
},


doneFastScrolling : function () {
    // if our last redraw was caused by fast scrolling we will have applied the quick 
    // draw ahead ratio when determining which records to draw. In this case we now want
    // to redraw with the standard drawAhead ratio so short distance scrolls around this area
    // will requrie fewer redraws
    var redrawRequired = this._appliedQuickDrawAhead;
    if (redrawRequired) {
        // set the flag to suppress drawAhead direction calculation. This ensures that we 
        // add draw-ahead rows in all directions on the theory that the user is done scrolling 
        // large increments in  one direction.
        // We clear this flag when the delayed redraw() actually fires
        this._suppressDrawAheadDirection = true;
        this.markForRedraw("Done Fast scrolling.");
    }
},

// given a range elements (rows or cols) currently visible in the viewport, apply the
// drawAheadRatio to determine the range to draw.  The "drawAheadRatio" is a fraction (>1) of
// the viewport. "scrollForward" is the scrolling direction: true (forward), false (backward),
// or null (unknown)
addDrawAhead : function (startIndex, endIndex, maxIndex, scrollForward, vertical) {
    
 
    // figure out how many elements we intend to draw
    var useQuickDrawAhead = this.shouldUseQuickDrawAheadRatio(),
        ratio = useQuickDrawAhead && this.quickDrawAheadRatio != null ? 
                                             this.quickDrawAheadRatio : this.drawAheadRatio,
        numToDraw = Math.ceil((endIndex - startIndex) * ratio);
    // respect the flag to suppress the drawAhead scrolling direction logic
    if (this._suppressDrawAheadDirection) scrollForward = null;

    if (scrollForward != null) {
        // we know the scroll direction; render extra elements in the current direction of
        // scrolling
        if (scrollForward) endIndex = startIndex + numToDraw;
        else startIndex = endIndex - numToDraw;
    } else {
        // we haven't been scrolled yet; if we're flush at the beginning (very common), render
        // ahead forward
        if (startIndex == 0) endIndex = numToDraw;
        else {
            // otherwise, render extra rows on either side
            var extraElements = Math.ceil((numToDraw - (endIndex - startIndex))/2);
            startIndex -= extraElements;
            endIndex += extraElements;
        }
    }

    // clamp ends of the range to 0 / maxIndex
    if (startIndex < 0) {   // shift both ends of the range forward so startIndex = 0
        endIndex -= startIndex;
        startIndex = 0;
    }        
    
    if (endIndex >= maxIndex) { // shift both ends of the range back so endIndex < maxIndex
        var offset = endIndex - (maxIndex -1);
        startIndex = Math.max(0, (startIndex - offset));
        endIndex = Math.max(0, maxIndex - 1);
    }        
    
    
    
    // store a flag indicating whether this redraw used the special 'quick draw ahead' code
    // this is checked in doneFastScrolling to determine whether a redraw is required.
    if (useQuickDrawAhead) this._appliedQuickDrawAhead = true;
    else delete this._appliedQuickDrawAhead;
    
    return [startIndex, endIndex];
},

// Helper - returns 1 if this is the frozen body of a ListGrid, zero otherwise.

_isFrozenBody:function() {
    if (this.grid && this.grid.frozenBody == this) return 1;
    return 0;
},

getExtraRowHeight : function (startRow, endRow) {
    var total = 0;
    for (var rowNum = startRow; rowNum < endRow; rowNum++) {
        var height = this.getRowHeight(this.getCellRecord(rowNum, 0), rowNum, this._isFrozenBody()),
            extraHeight = (height - this.cellHeight);
        if (extraHeight > 0) {
            //this.logWarn("rowNum: " + rowNum + 
            //             " in range: " + [this._firstDrawnRow, this._lastDrawnRow] + 
            //             " with extraHeight: " + extraHeight);
            total += extraHeight;
        }
    }
    return total;
},

getDrawArea : function (colNum) {
	// Figure out what rows should be drawn
	// --------------------------------------------------------------------------------------------
    var totalRows = this.getTotalRows(), startRow, endRow, vScrollForward;

    // figure out if we should show all cells in case the total displayable cells are less than
    // drawAllMaxCells
    var totalCells = totalRows * this.fields.length, 
        showAllCells = totalCells <= this.drawAllMaxCells &&
                       !isc.EH.dragging && !this.isAnimating() && 
                       !(this.parentElement && this.parentElement.isAnimating());

    if (this.showAllRows || showAllCells) {
        // draw all rows
		startRow = 0;
		endRow = Math.max(totalRows - 1, 0);
    } else {
        // ordinary incremental rendering
        var rowArr = this._getDrawRows();
        startRow = rowArr[0];
        endRow = rowArr[1];
        // just for logging
        vScrollForward = rowArr[2]
	}

	// Figure out which columns to draw
	// --------------------------------------------------------------------------------------------
    var startCol, endCol, totalCols = this.fields.length, hScrollForward;
	if (colNum != null) {
		// a column number was specified, draw that column only (needed for legacy Nav4 support, and
        // for column auto-sizing)
		startCol = colNum;
        endCol = colNum + 1;
	} else if (showAllCells || this.shouldShowAllColumns()) {
        // draw all columns
        startCol = 0;
        endCol = totalCols - 1;
    } else {
        // incremental rendering
        var visibleColumns = this.getVisibleColumns();
        // detect scrolling direction: true (forward), false (backward), or null (unknown)
        hScrollForward = (this.lastScrollLeft == null ? null :
                          this.lastScrollLeft < this.getScrollLeft());

        var drawAheadRange = this.addDrawAhead(visibleColumns[0], visibleColumns[1], 
                                               totalCols, hScrollForward);
        
        startCol = drawAheadRange[0];
        endCol = drawAheadRange[1];
	}

    // figure out the appropriate chunk size on first draw ever
    if (this.cacheDOM && !this._rowChunkSize) {
        this._rowChunkSize = endRow - startRow;
        this._colChunkSize = endCol - startCol;
    }

    

    return [startRow, endRow, startCol, endCol];
},

_getDrawRows : function () {

        // figure out which rows we need to draw to minimally fill the viewport
        var visibleRows = this._getViewportFillRows();

        // detect scrolling direction: true (forward), false (backward), or null (unknown)
        var vScrollForward = (this.lastScrollTop == null ? null :
                             this.lastScrollTop < this.getScrollTop());

        var totalRows = this.getTotalRows();
        
        // Note: addDrawAhead will add the draw-ahead rows (rows drawn offscreen for
        // scrolling), and clamp the ends of the drawn range to the ends of the data (ensuring
        // we don't end up with startRow < 0, or endRow > (totalRows-1)
        var drawAheadRange = this.addDrawAhead(visibleRows[0], visibleRows[1], 
                                               totalRows, vScrollForward, true);
        
        //this.logWarn("draw range: " + this._getViewportFillRows() + " fwd:" + vScrollForward +
        //             ", after adding drawAhead:" + drawAheadRange);
        
        // just for logging - return whether we added the fwd scroll
        drawAheadRange[2] = vScrollForward;
        
        return drawAheadRange;
        
},
        

 

// get the row at the coordinate, as a floating point number representing a partial distance
// through the row
getRowCoordinate : function (coord) {
    var rowNum = this.getEventRow(coord),
        // get our offset into it
        rowTop = this.getRowTop(rowNum),
        offsetIntoRow = coord - rowTop,
        rowHeight = this.getRowSize(rowNum),
        percentIntoRow = offsetIntoRow / rowHeight;

    // detect inconsistency between getEventRow and getRowTop()
    //if (offsetIntoRow < 0 || offsetIntoRow > rowHeight) {
    //    this.logWarn("*******************************\n" + 
    //                 ", coord: " + coord + 
    //                 ", eventRow: " + rowNum + 
    //                 ", rowTop: " + rowTop + 
    //                 ", offsetIntoRow: " + offsetIntoRow +
    //                 ", rowSize: " + rowHeight +
    //                 ", firstDrawn: " + this._firstDrawnRow +
    //                 ", lastDrawn: " + this._lastDrawnRow +
    //                 ", heights: " + this._getDrawnRowHeights());
    //}

    return rowNum + percentIntoRow;
},


// override to interpret ratio in terms of rowNum instead of scrollTop vs scrollHeight
scrollToRatio : function (vertical, ratio, reason,a,b) {
    if (!vertical || !this._isVirtualScrolling) {
        return this.invokeSuper(isc.GridRenderer, "scrollToRatio", vertical,ratio,reason,a,b);
    }
 
    var maxRow = this.getTotalRows() - 1,
        exactRowNum = ratio * maxRow,
        rowNum = Math.floor(exactRowNum),
        rowOffset = Math.round((exactRowNum - rowNum) * this.getRowSize(rowNum));
    
    
    this._targetRow = rowNum;
    this._rowOffset = rowOffset;
    this._scrollToTargetRow(reason || "scrollToRatio");

    // if scrolling to that position makes us dirty, setup to scroll to the indicated target
    // row during redraw
    if (this.isDirty()) {
        this._scrollRatio = ratio;
        this._targetRow = rowNum;
        this._rowOffset = rowOffset;
    }
},

// override to return ratio in terms of rowNum instead of scrollTop vs scrollHeight
getScrollRatio : function (vertical,b,c,d) {
   
    if (!vertical || !this._isVirtualScrolling) {
        return this.invokeSuper(isc.GridRenderer, "getScrollRatio", vertical,b,c,d);
    }
    if (this.isDirty() && this._scrollRatio != null) return this._scrollRatio;

    // if there are 0 or 1 rows, we're at the top 
    var maxRow = this.getTotalRows() - 1;

    if (maxRow <= 0) return 0;

    var scrollTop = this.getScrollTop(), 
        topCoord = this.getRowCoordinate(scrollTop),
        ratio = topCoord / maxRow;

    //this.logWarn("getScrollRatio: " + ratio + 
    //             ", maxRow: " + maxRow + 
    //             ", topCoord: " + topCoord);

    return Math.min(1,ratio);
},

// show a fixed-size thumb in virtualScrolling mode.  Otherwise thumb size fluctuates
// meaninglessly.
getViewportRatio : function (vertical,b,c,d) {
    if (!vertical || !this._isVirtualScrolling) {
        return this.invokeSuper(isc.GridRenderer, "getViewportRatio", vertical,b,c,d);
    }
    var avgRowHeight = this._viewRatioRowHeight || this.getAvgRowHeight();

    return Math.min(1, (this.getViewportHeight() / avgRowHeight) / this.getTotalRows());
},

// take some drawn row that is likely to remain drawn, and store the position it should be in
// relative to the viewport, so that if we have to redraw, we can match user expectation by
// placing rows where the user expects.
_storeTargetRow : function (scrollTop, delta) {
    
    // don't pick up a target row during the special scroll that places us on the target row
    if (this._literalScroll) return;
    
    // if we're empty just bail
    if (this.isEmpty()) return;

    // according to scrolling direction, pick the row at the top or bottom of the viewport as
    // the row most likely to remain onscreen
    var viewportEdge,
        targetRow,
        maxRow = this.getTotalRows()-1;
    if (delta > 0) {
        // scrolling down
        viewportEdge = scrollTop + this.getViewportHeight();
        targetRow = this.getEventRow(viewportEdge);
        // If there is no row at the end of the viewport (possible with the spacer we write out for
        // virtual scrolling to work), clamp to the last actual row in the data set.
        if (targetRow == -2 && maxRow >= 0) {
            targetRow = maxRow;
        }
    } else {
        viewportEdge = scrollTop;
        targetRow = this.getEventRow(viewportEdge);
    }
    var newScrollTop = scrollTop;
   
        
    if (targetRow < 0 || targetRow > maxRow) {
        this._targetRow = maxRow;
        this._rowOffset = 0;
        newScrollTop = this.getRowTop(maxRow); 
    } else {
        this._targetRow = targetRow;
        // how far into the target row the top of the viewport should be (positive means more
        // of row is scrolled offscreen)
        this._rowOffset = scrollTop - this.getRowTop(this._targetRow) + delta;
        //var drawArea = this.getDrawArea();
        //if (targetRow < drawArea[0] || targetRow > drawArea[1]) {
        if (Math.abs(this._rowOffset) > this.getViewportHeight()) {
            // happens only if we are programmatically scrolled by a large amount to a totally
            // new viewport, in which case anchoring to a row from the old viewport is useless
            // and could lead to surprises (eg if scrolled near max, scrolling to 10px should
            // place us on 10px into the first row but might do something different if we
            // calculate coordinates based on drawn row sizes in a viewport near the end).
            this.logInfo("storeTargetRow: targetRow: " + targetRow +
                         " with offset: " + this._rowOffset + 
                         //" wouldn't fall within draw range: " + [drawArea[0],drawArea[1]] +
                         ", clearing", "virtualScrolling");
            this._rowOffset = this._targetRow = null;
        }
    }

    

    return newScrollTop;
},

// scroll the previously stored target row into the stored position
_scrollToTargetRow : function (reason) {
    var targetRow = this._targetRow,
        offset = this._rowOffset;
    
    var scrollTop = this.getRowTop(targetRow) + offset;
    var scrollLeft = this._cellOffset;
    

    this._literalScroll = true;
    
    this._scrollHeight = null;
    this.scrollTo(scrollLeft, scrollTop, reason || "targetRow");
    
    this._literalScroll = false;

    // stop reporting last requested scroll ratio since we've now scrolled to match the
    // requested ratio
    this._scrollRatio = null;
},

scrollIntoView : function (x,y, width, height, xPosition, yPosition, animated, callback, alwaysCenter, source) {
    if ((source != null) && (source != this)) {
        // This method call from a child widget
        return;
    }

    this.invokeSuper(isc.GridRenderer, "scrollIntoView", x,y, width, height, xPosition, yPosition, animated, callback, alwaysCenter, source);
},

// if we're rendering rows/cols incrementally, we may need to redraw on scroll
scrollTo : function (left, top, reason, animating) {
    if (isc._traceMarkers) arguments.__this = this;

    
    if (this._isVirtualScrolling && top != null && reason != "nativeScroll") {
        var oldScrollTop = this.getScrollTop(),
            delta = top - oldScrollTop;

        if (delta != 0) {
            this._storeTargetRow(oldScrollTop, delta);
            top = Math.min(top, this.getRowTop(this.getTotalRows()-1));
        }
    }

    
    this.invokeSuper(isc.GridRenderer, "scrollTo", left,top, reason, animating);

    // don't check for the need to redraw if we're already dirty.  Optimization: for
    // scroll-and-scroll-back situations, we could avoid a redraw by undirtying ourselves
    if (this.isDirty() || this._scrollFromRedraw) return;

	// if we're only drawing rows near the viewport..
    if (!this._delayedRedraw && this._needAxisRedraw()) {
        
        this.redrawOnScroll(!this.isFastScrolling() && this.instantScrollTrackRedraw);
    }
},

_getRedrawOnScrollCallback:function () {
    if (this._redrawOnScrollCallback == null) {
        var _this = this;
        this._redrawOnScrollCallback = function () { 
            delete this._pendingScrollRedrawFromWheel;

            // fire a synthetic mouseMove - this will update the
            // mouseOver row, etc
            _this._fireSyntheticMouseMove();
            
            _this.redraw("scrolled") 
        };
    }
    return this._redrawOnScrollCallback;
},

redrawOnScroll : function (immediate) {

    // Suppress redrawOnScroll during rowHeight animation.
    // If we're shrinking a row, and we're scrolled to the end we expect to see scrolls
    // occur natively during the animation - we don't want to cut it short and redraw
    
    if (this._rowHeightAnimation) {
        return;
    }
    if (immediate) {
        this.redraw("scrolled");
    } else {
        var isMouseWheelScrolling = this.isMouseWheelScrolling(),
            delay = this.getScrollRedrawDelay(isMouseWheelScrolling);
            
        if (delay == 0) {
        this.markForRedraw("scrolled");
    } else {
        this.fireOnPause("scrollRedraw", 
            this._getRedrawOnScrollCallback(), 
                delay);
            this._pendingScrollRedrawFromWheel = isMouseWheelScrolling;
        }
    }
    
    this._scrollRedraw = true;
},

getScrollRedrawDelay : function (isMouseWheelScrolling) {
    var delay = this.scrollRedrawDelay;
    if (this.scrollWheelRedrawDelay == null) return delay;
    
    if (isMouseWheelScrolling == null) {
        isMouseWheelScrolling = this.isMouseWheelScrolling();
    }
    if (isMouseWheelScrolling) {
        delay = this.scrollWheelRedrawDelay;
    }
    return delay;
},

_needRowRedraw : function () {
	if (this.showAllRows) return false;

    // we have a range of records that have been drawn, from grid._firstDrawnRow to
    // grid._lastDrawnRow (updated in getTableHTML).  See if the new viewport falls
    // completely into the drawn range.
    // NOTE: we use visible rows rather than viewport fill rows because by using
    // actual rendered row height we can avoid some redraws when we have several viewports
    // worth of drawn data due to tall rows.
    // Note also that visible rows is only an approximation if asked about an undrawn area,
    // which is fine, because all we care about is whether the new viewport falls
    // completely within the drawn range.
    var visibleRows = this.getVisibleRows(),
        firstVisible = visibleRows[0],
        lastVisible = visibleRows[1];
    
    // check that the last visible row doesn't exceed the total number of rows we will
    // draw.  NOTE: -1 because totalRows is a count and lastVisible is an index.
    var totalRows = this.getTotalRows();
    if (lastVisible > totalRows-1) lastVisible = totalRows-1;

    var needRedraw = (firstVisible < this._firstDrawnRow || lastVisible > this._lastDrawnRow);

    

    
    return needRedraw;
},

_needColumnRedraw : function () {
	// if we're only drawing columns near the viewport..
    if (this.shouldShowAllColumns()) return false;

    var visibleCols = this.getVisibleColumns(),
        firstVisible = visibleCols[0],
        lastVisible = visibleCols[1],
        needRedraw = (firstVisible < this._firstDrawnCol || lastVisible > this._lastDrawnCol);

    
    return needRedraw;
},

_needAxisRedraw : function () {
    return this._needRowRedraw() || this._needColumnRedraw();
},

// disable incremental rendering when overflow:visible is set on the fly
setOverflow : function (overflow) {
    if (overflow == isc.Canvas.VISIBLE) {
        this.showAllRows = true;
    }
    
    return this.Super("setOverflow", arguments);
},


// Cache DOM mode
// ---------------------------------------------------------------------------------------
// Mode that caches rendered chunks of the grid area to avoid redrawing as a user revisits the
// same area of the grid without having changed anything.  Currently incomplete.

// === cacheDOM mode limitations
// - can't have fixedRecordHeights:false
// - does not support row animation
// - doesn't work with rowSpans
// - shouldn't use with full-row inline edit and large number of columns
// - doesn't support startSpace / endSpace



getRowChunkNum : function (logicalRowNum) {
    return Math.round(logicalRowNum / this._rowChunkSize);
},

getColChunkNum : function (logicalColNum) {
    return Math.round(logicalColNum / this._colChunkSize);
},

getTableChunk : function (rowChunkNum, colChunkNum) {
    var tableCache = this._tableCache;
    if (!tableCache) return;

    // if row and col are unpassed, return the chunk at 0,0
    rowChunkNum = rowChunkNum || 0;
    colChunkNum = colChunkNum || 0;

    var colCache = tableCache[rowChunkNum];
    return colCache ? colCache[colChunkNum] : null;
},

getTableChunkAt : function (logicalRowNum, logicalColNum) {
    var rowChunkNum = this.getRowChunkNum(logicalRowNum),
        colChunkNum = this.getColChunkNum(logicalColNum),
        tableElem = this.getTableChunk(rowChunkNum, colChunkNum);

    if (tableElem != null) {
        // semi-hack: set the offsets used in getTableElement() to find physical cells from
        // logical cells
        this._firstDrawRow = rowChunkNum * this._rowChunkSize;
        this._firstDrawnCol = colChunkNum * this._colChunkSize;
        return tableElem;
    }
},

// We need to ensure the table cache is clear after we've reset our inner HTML
// - otherwise we'll be pointing to stale elements.
// Clear the cache before we update inner HTML and 
// also set a flag so any inadvertant calls to code that would re-cache coming
// from getInnerHTML will not re-cache stale elements.
// note: _updateInnerHTML is called if we have no children, otherwise
// _updateParentHTML updates the innerHTML

_updateInnerHTML : function (a,b,c,d) {

    if (this.cacheDOM) {
        this.drawVisibleChunks();
    } else {
      this._clearTableCache();
      this._suppressTableCaching = true;

       this.invokeSuper(isc.GridRenderer, "_updateInnerHTML", a,b,c,d);
       
       delete this._suppressTableCaching;
    }
},

_updateParentHTML : function (a,b,c,d) {
    this._clearTableCache();
    this._suppressTableCaching = true;
    this.invokeSuper(isc.GridRenderer, "_updateParentHTML", a,b,c,d);
    delete this._suppressTableCaching;
},

// in cacheDOM mode, this is called in lieu of normal redraw
drawVisibleChunks : function () {
    // figure out what undrawn chunks are visible and draw them
    var visibleRows = this.getVisibleRows(),
        visibleCols = this.getVisibleColumns(),
        startRowChunk = this.getRowChunkNum(visibleRows[0]),
        startColChunk = this.getColChunkNum(visibleCols[0]),
        endRowChunk = this.getRowChunkNum(visibleRows[1]),
        endColChunk = this.getColChunkNum(visibleCols[1]);

    for (var rowChunk = startRowChunk; rowChunk < endRowChunk; rowChunk++) {
        for (var colChunk = startColChunk; colChunk < endColChunk; colChunk++) {
            if (this.getTableChunk(rowChunk, colChunk) == null) {
                this.logWarn("drawing chunk: " + [rowChunk, colChunk]);
                this.renderTableChunk(rowChunk, colChunk);
            }
        }
    }
    
    var newHTML = this.getTableHTML();
},

renderTableChunk : function (rowChunkNum, colChunkNum) {
    // figure out geometry of table to draw
    var startRow = rowChunkNum * this._rowChunkSize,
        endRow = startRow + this._rowChunkSize,
        startCol = colChunkNum * this._colChunkSize,
        endCol = startCol + this._colChunkSize;
    
    // draw new table chunk
    var html = this.getTableHTML([startCol, endCol], startRow, endRow),
        tableElem = isc.Element.insertAdjacentHTML(this.getHandle(), "beforeEnd", html, true);

    //this.logWarn("html form chunk: " + [rowChunkNum, colChunkNum] +
    //             "\n" + html +
    //             "\nelement: " + this.echo(tableElem));

    // cache the table element
    var tableCache = this._tableCache = this._tableCache || [],
        colCache = tableCache[rowChunkNum] = tableCache[rowChunkNum] || [];
    colCache[colChunkNum] = tableElem;
},

//>Animation Row Animation support
// ---------------------------------------------------------------------------------------
// Methods to animate a show / hide of multiple rows

//> @method gridRenderer.startRowAnimation()
// Animates a show / hide of rows by growing the rows into view.
// Note: the rows to be shown/hidden should already be in the data, and the calling function
// is responsible for any manipulation to the data / redraw at the end of this method.
// @param show (boolean) are we showing or hiding rows?
// @param startRow (number) first row in range to be shown/hidden
// @param endRow (number) last row in range to be shown/hidden
// @param [callback] (callback) callback to fire when animation completes
// @param [speed] (number) speed for the animation in pixels / second
// @param [duration] (number) if speed is not set, number of milliseconds for the animation to take
// @param [effect] (string) optional acceleration effect for the animation
// @param [slideIn] (boolean) if specified, the rows will appear to slide into view rather than 
//                            being revealed
//<
// additional param indicates this was called from the listGrid - we use this to ensure
// we fire the callback in the listGrid's scope
startRowAnimation : function (show, startRow, endRow, callback, speed, duration, effect, slideIn,
                              fromListGrid, isDelayed) {
    // Always call finishRowAnimation - this will no op if there is no current/pending
    // row animation in progress
    this.finishRowAnimation();
    if (!this.isDrawn() || !this.isVisible()) {
        if (callback != null) {
            var target = fromListGrid ? this.parentElement : this;
            target.fireCallback(callback);
        } 
        return;
    }
    if (show == null) show = true;
    if (startRow == null) startRow = 0;
    if (endRow == null) endRow = this.getTotalRows() - 1;
 
    if (startRow == endRow) {
        this.logWarn("startRowAnimation passed empty row range, aborting: " + 
                     [startRow, endRow]);
        return;
    }
   
    
    var canRedraw = this.readyToRedraw("animating show / hide of rows", false);
    if (!canRedraw) {
        this._delayedRowAnimationArgs = [show, startRow, endRow, callback, speed, 
                                         duration, effect, slideIn, fromListGrid];
        this._delayedRowAnimation = isc.Timer.setTimeout(
                                        {target:this, methodName:"_delayedStartRowAnimation"},
                                        0
                                    );
        return;
    }
    

    // redraw, placing an entire subtable with the rows to be animated inside a single row, and
    // measure the size of the rows we're going to reveal or hide
    
    
    // When doing a 'slide-in' animation, we have to write out every row to be revealed since
    // they'll all scroll past the user's nose.
    // Have a check for this being a huge number of rows so we don't hit performance issues
    // generating this initial HTML!
    if ((endRow-startRow) > this.maxAnimateSlideInRows) slideIn = false;
    // set up the this._slideInAnimationRows attribute - this allows us to determine
    // whether we need to write out every row in the fragment
    this._slideInAnimationRows = slideIn;
    var fragmentHeight = this._initializeShowHideRow(show, startRow, endRow, callback, fromListGrid);
    
    // Use animateRowHeight to grow or shrink the height
    this.animateRowHeight(this._animatedShowStartRow, 
                         // NOTE: animate all the way down to zero, so there is no lurch
                         // between the final frame of the animation and the subsequent redraw
                         (show ? fragmentHeight : 0), 
                         {target:this, methodName:"_rowShowComplete"}, 
                         speed, duration, effect, slideIn);
},

// maximum number of rows to be animated into view using a 'slide' animation
// this kind of animation requires every row be rendered so we limit this to ensure
// we don't hit a performance roadblock writing out thousands of records' HTML!
maxAnimateSlideInRows:100,


// Helper to start delayed row animation
_delayedStartRowAnimation : function () {

    if (this._delayedRowAnimationArgs == null) {
        this.logWarn("Unable to perform delayed row animation - bailing");
        return;
    }
    
    var argsArr = this._delayedRowAnimationArgs,
        show = argsArr[0],
        startRow=argsArr[1],
        endRow = argsArr[2], 
        callback = argsArr[3],
        speed = argsArr[4],
        duration = argsArr[5],
        effect = argsArr[6], 
        slideIn = argsArr[7],
        fromListGrid = argsArr[8];
        
    this._delayedRowAnimationArgs = null;
    this._delayedRowAnimation = null;
    // The additional param indicates that the row animation is delayed
    
    this.startRowAnimation(show, startRow, endRow, callback, speed, duration, effect, slideIn, 
                           fromListGrid, true);
},


// helper method to redraw the GR in its state at the beginning of the show/hide row animation
// Returns the height of the table fragment to be written into the animation row.
_initializeShowHideRow : function (show, startRow, endRow, callback, fromListGrid) {

    // If we've already been called and performed a redraw to set up the animated row table,
    // just return the height of the rows to animate
    // This allows us to separate the initial redraw (rendering the animation rows in a clippable
    // div) from the row animation.
    
    var fragmentHeight = 0;
    if (this._animatedShowStartRow == startRow && this._animatedShowEndRow == endRow) {
        
        // check the anmiation cell only
        var animationCell = this.getTableElement(this._animatedShowStartRow, 0),
            clipDiv = this._getCellClipDiv(animationCell);
            
        if (!clipDiv) {
            fragmentHeight = (endRow - startRow) * this.cellHeight;
        } else fragmentHeight = clipDiv.scrollHeight;
    
    } else {
        
        // hang a flag onto this table so we know where the fragment gets written into the normal
        // table.
        this._animatedShowStartRow = startRow;
        this._animatedShowEndRow = endRow;
        
        // if we're hiding visible rows, we can look at their drawn heights now
        if (!show) {
            var heights = this._getDrawnRowHeights();
            for (var i = startRow; i < endRow; i++) {
                fragmentHeight += heights[i];
            }
        
            // used when writing out the row containing the fragment     
            this._animatedShowRowHeight = fragmentHeight;
            // This redraw writes out the single animation row with an entire table inside it
            this.redraw("initializing animated hide row");
            
        // In this case we're going to show rows that are currently undrawn.
        } else {
            this._animatedShowRowHeight = 1;
            this.redraw("initializing animated show row");
            // At this point we have written out the fragment and it's clipped by the containing
            // cell / div
            var animationCell = this.getTableElement(this._animatedShowStartRow, 0),
                clipDiv = this._getCellClipDiv(animationCell);
                
            if (!clipDiv) {
                fragmentHeight = (endRow - startRow) * this.cellHeight;
            } else fragmentHeight = clipDiv.scrollHeight;
    
        }
        
        if (this.isDirty()) this.redraw("Initializing row animation requires second redraw");
    }
    
    this._animatedShowCallback = {callback:callback, 
                                  target:(fromListGrid ? this.parentElement : this)};    
    
    return fragmentHeight;
},

// finishRowAnimation - synchronously short-cut to the end of the current row show/hide 
// animation, and fire the callback.
finishRowAnimation : function () {
    // a currently running rowAnimation (show/hide rows) implies a running rowHeightAnimation - 
    // finishing that will jump to the approprate size and fire the callback to finish the
    // show/hide animation
    if (this._animatedShowStartRow != null) {
        this.finishAnimateRowHeight();
        
    } else {
        // In this case we're not running a show/hide row animation - but we may have set up
        // a delayed one
        
        if (this._delayedRowAnimation != null) {
        
            // don't fire the delayed animation
            isc.Timer.clearTimeout(this._delayedRowAnimation);

            var args = this._delayedRowAnimationArgs,
                show = args[0], startRow = args[1], endRow = args[2],
                callback = args[3], duration = args[4], fromListGrid = args[5];

            delete this._delayedRowAnimationArgs;
            delete this._delayedRowAnimation;
            
            if (!this.readyToRedraw()) {
                this.logWarn("Finish row animation called while Grid is not ready to redraw. " + 
                             "GridRenderer HTML will not be updated when callback fires.", "animation");
                var target = fromListGrid ? this.parentElement : this;
                if (callback) target.fireCallback(callback);
                
            } else {
                // redraw the GR with the single animation row containing the table fragment
                // at the start of the animation height
                var fragmentHeight = this._initializeShowHideRow(show, startRow, endRow, callback, fromListGrid);
                // set the height to the final height of that row and fire the 'complete' method
                // to fire callbacks / clean up vars (rather than ever performing an animation
                this.setRowHeight(startRow, (show ? fragmentHeight : 1));
                this._rowShowComplete();
            }
        } 
    }
},

// Fired when animated show / hide of rows completes.
_rowShowComplete : function () {
    var callback = this._animatedShowCallback;
    delete this._animatedShowCallback;
    delete this._animatedShowStartRow;
    delete this._animatedShowEndRow;
    delete this._animatedShowRowHeight;
    // We stored the callback as an object {target:... callback:...}
    // This allows us to fire the callback on the ListGrid if that's where the method was
    // originally called from
    if (callback && callback.callback) callback.target.fireCallback(callback.callback);
},

//> @method gridRenderer.animateRowHeight()
// Will animate a resize of a row to the specified height, firing a callback when the resize
// is complete
// @param rowNum (number) Row to resize
// @param toHeight (number) new height for the row
// @param [callback] (callback) Callback to fire when animation completes
// @param [speed] (number) Speed for the animation (pixels / second)
// @param [duration] (number) duration of the resize in ms
// @param [effect] (string) Optionally an acceleration effect can be specified - if not specified
//                          default is to do a smooth animation (no acceleration)
// @param [slideIn] (boolean) if specified, rows will appear to slide into view rather than 
//                            being revealed
//<
// Additional param 'fromListGrid' indicates this was fired from the ListGrid, so we should 
// fire the callback in that scope
_$none:"none",
animateRowHeight : function (rowNum, toHeight, callback, speed, duration, effect, 
                             slideIn, fromListGrid) {
    // If we're not drawn, no need to try to animate since this is a visual update only
    if (!this.isDrawn()) {
        if (callback) {
            var target = (fromListGrid ? this.parentElement : this);
            target.fireCallback(callback);
        }
        return;
    }
    
    // simultaneous row height animations not currently supported    
    if (this._rowHeightAnimation != null) {
        this.logInfo("early finish of row animation, because new animation started",
                     "animation")
        this.finishAnimateRowHeight();
    }
    
    var fromHeight = this.getRowSize(rowNum);
    
    // If speed (pixels / second) is specified, it takes precedence over duration
    if (speed != null) {
        var change = (toHeight - fromHeight);
        if (change < 0) change = 0 - change;
        
        duration = Math.round((change / speed) * 1000);
        // Don't let the animation exceed a maximum
        if (duration > this.animateRowsMaxTime) duration = this.animateRowsMaxTime;
    }

    this._rowAnimationInfo = {
        _rowNum:rowNum,
        _fromHeight:fromHeight,
        _toHeight:toHeight,
        _callback:callback,
        _slideIn:slideIn,
        _fromList:fromListGrid
    }

    effect = (effect || this._$none);
    if (this.logIsInfoEnabled("animation")) {
        this.logInfo("starting row animation, row:" + rowNum  + 
                    ", duration: " + duration + ", effect: " + effect, 
                     "animation")
    }
    this._rowHeightAnimation = this.registerAnimation(
                                    {target:this, method:this._fireRowAnimation}, 
                                    duration, effect
                               );
    // suppress adjustOverflow until the row animation completes. This will avoid unnecessary
    // scrollbars from showing up
    if (this.overflow == isc.Canvas.AUTO || this.overflow == isc.Canvas.SCROLL)
        this._suppressAdjustOverflow = true;
},

_fireRowAnimation : function (ratio) {

    var info = this._rowAnimationInfo;
    
    if (info == null) return;

    var rowNum = info._rowNum,
        rowHeight = this._getRatioTargetValue(info._fromHeight, info._toHeight, ratio);
    
    if (isc.Browser.isSafari && info._fromHeight > info._toHeight) 
        this._forceRowRefreshForAnimation = true;
    // pass in explict "" as className so we don't adjust sizing for the standard row styling
    // (which won't be applied to this row during animation)
    this.setRowHeight(rowNum, rowHeight, null, isc.emptyString, true, true, true);
    if (isc.Browser.isSafari) delete this._forceRowRefreshForAnimation;

    if (info._slideIn) {    
        var clipDiv = this._getCellClipDiv(this.getTableElement(rowNum,0));
        if (clipDiv) {
            var scrollHeight = clipDiv.scrollHeight,
                offsetHeight = clipDiv.offsetHeight;
            if (scrollHeight > offsetHeight) clipDiv.scrollTop = scrollHeight - offsetHeight;
            else clipDiv.scrollTop = 0;
        }
    }
    
    // Fire the completion callback in a separate thread - this means if it does a lot of
    // processing we shouldn't see a visual pause before the native repaint at the full-size
    if (ratio == 1) {
        isc.Timer.setTimeout({target:this, methodName:"_rowAnimationComplete"}, 0);
    }
},

// Fired when we're done with a row resize animation
_rowAnimationComplete : function () {
    // allow standard adjustOverflow to resume
    delete this._suppressAdjustOverflow;
    this.adjustOverflow("row animation complete");


    var info = this._rowAnimationInfo;

    delete this._rowHeightAnimation;
    delete this._rowAnimationInfo;
    
    if (info && info._callback) {
        var target = info._fromList ? this.parentElement : this;
        target.fireCallback(info._callback);
    }
},


//> @method gridRenderer.finishAnimatingRowHeight()
// Completes any row height animation currently in progress and fires the callback from that
// animation.<br>
// May be fired automatically to avoid (unsupported) overlapping animations, etc.
//<
// Leave this as unexposed for now
finishAnimateRowHeight : function () {

    if (!this._rowHeightAnimation) return;
    
    // cancel upcoming animation cycles
    this.cancelAnimation(this._rowHeightAnimation);
    
    // Simply firing the "last step" of the rowHeight animation will jump to the appropriate 
    // height and fire the callback
    
    this._fireRowAnimation(1);
},

//<Animation

// When printing we need to write out embedded components' printHTML directly in our table HTML
_getPrintChildren : function () {
    return this._embeddedComponents;
},


// returns the tableHTML for printing.
// Used direclty by ListGrid.getPrintHTML()

getTablePrintHTML : function (colNum, startRow, endRow, discreteCols, asyncCallback) {
    return this.getTableHTML(colNum, startRow, endRow, discreteCols, asyncCallback);
},



// Should we suppress focus in the widget handle entirely in favor of focussing in rows when
// in screenReader mode?
screenReader_suppressHandleFocus:true,

draw : function () {
    // don't write tabIndex/ focus/blur handlers onto the handle if we're
    // going to write them onto a row element.
    if (isc.screenReader && this.screenReader_suppressHandleFocus) {
        this.clipHandleIsFocusHandle = this.isEmpty();
    }
    return this.Super("draw", arguments);
},


handleKeyDown : function (event, eventInfo) {
    var rv = this.Super("handleKeyDown", arguments);
    if (rv != false && isc.screenReader && this.screenReader_suppressHandleFocus) {
        var mask = isc.EH.clickMaskUp(),
            hardMaskUp = false;
        if (mask) {
            var masks = isc.EH.clickMaskRegistry;
            for (var i = 0; i < masks.length; i++) {
                if (isc.EH.isHardMask(masks[i])) {
                    hardMaskUp = true;
                    break;
                }
            }
        }
        if (!hardMaskUp) {
            var keyName = event.keyName;
            if (keyName == "Tab") {
                this._focusInNextTabElement(!isc.EH.shiftKeyDown())
                return false;
            }
        }
    }
    return rv;
},

// allowRowSpanning: If set to false we never call getRowSpan even if the method is present.
// This is a grid passthrough attribute - defaulted to false at the ListGrid level.

allowRowSpanning:true,


closeNOBRs:false,

_getScreenReaderCellSeparatorID : function () {
    return this._getDOMID("screenReaderCellSeparator");
},
_getScreenReaderRowSeparatorID : function () {
    return this._getDOMID("screenReaderRowSeparator");
},

// returns the innerHTML for the table
// If passed a startRow / endRow, it will return just the HTML for that fragment of the table.
// asyncCallback / isAsync is required for printing only. This allows us to handle the
// embedded components generating their printHTML asynchronously
getTableHTML : function (colNum, startRow, endRow, discreteCols, asyncCallback, isAsync) {
    if (isc._traceMarkers) arguments.__this = this;
	//>DEBUG
	// timing
    var t0 = isc.timeStamp();
	//<DEBUG

    // show empty message
    if (this.isEmpty()) {
        // clear drawn area
        this._firstDrawnRow = this._lastDrawnRow = 
                this._firstDrawnCol = this._lastDrawnCol = null;
        
        // note that if we're printing, showEmptyMessage handles embedding the 
        // printHeaders / printFooters directly in the generated message text.
        return this._showEmptyMessage();
    }
    
    // If we're printing and we have embedded components we need to get their printHTML
    // and plug it into the cells directly
    if (this.isPrinting && (!this._printingChunk || startRow == 0)) {
        
        var printComponents = this._getPrintChildren();
        
        if (printComponents != null && printComponents.length > 0) {
            
            for (var i = 0; i < printComponents.length; i++) {
                var component = printComponents[i];
                if (component._gridBodyPrintHTML != null) continue;
                var printComponentContext = {
                        component:component,
                        colNum:colNum, startRow:startRow, endRow:endRow, descreteCols:discreteCols,
                        asyncCallback:asyncCallback
                    };
                // this.logWarn("calling getPrintHTML on embedded component:" + printComponents[i]);
                var printCHTML = printComponents[i].getPrintHTML(
                                    this.printProperties,
                                    asyncCallback == null ? null 
                                        : {target:this, methodName:"gotComponentPrintHTML", 
                                            context:printComponentContext}
                                 );
                if (printCHTML != null) {
                    component._gridBodyPrintHTML = printCHTML;
                } else {
                    // If the printComponent generates its printHTML asynchronously, 
                    // we will be notified and fire the gotComponentPrintHTML() method
                    // when that returns, at which point we'll re-run this method, skipping
                    // the component(s) for which we already have HTML and ultimately firing
                    // the asyncCallback.
//                    this.logWarn("GR.getTableHTML() - getPrintHTML for component:" +
//                        component + " went asynchronous", "printing");
                    return null;
                }
            }
        }
        
        // at this point we've generated HTML for all our print components and stored it
        // We'll write it directly into the cells / rows below.
    }

    var fragment = (startRow != null && endRow != null),
        rangeStart = startRow != null ? startRow : 0,
        rangeEnd = endRow != null ? endRow : this.getTotalRows();
    
	// Figure out rows and columns to actually draw
	// ----------------------------------------------------------------------------------------

    var drawRect = this._getTableHTMLDrawArea(startRow, endRow, true),
        grid = this.grid;
    startRow = drawRect[0];
    endRow = drawRect[1];

    if (!fragment) {
        // If virtualScrolling is enabled, turn it on unless we're showing all rows.
        var showingAllRows = (startRow == 0 && endRow == this.getTotalRows());
        if (this.virtualScrolling) {
           this._isVirtualScrolling = !showingAllRows;
        }
        // if we've stopped virtual scrolling, clean up those pointers
        if (!this._isVirtualScrolling) {
            // _targetRow / _rowRatio is used when virtual scrolling to ensure redraws maintain
            // apparant scroll position.
            delete this._targetRow;
            delete this._rowRatio;
            // _scrollRatio is similarly used when doing a scrollToRatio
            delete this._scrollRatio;
            // _viewRatioHeight is used to determine scrollbar thumb size when virtual scrolling.
            // (We don't technically need to clear this - it's ignored if _isVirtualScrolling is
            // false and would be reset when virtual scrolling was reintroduced)
            delete this._viewRatioHeight
        }
    }

    // always refresh _firstDrawnCol / _lastDrawnCol
    // This may be required even if we're rendering a fragment as  in some cases we
    // asynchronously
    // fetch fragments (EG printing HTML) and the rendered area may have changed (EG
    // shouldPrint:false fields) 
    if (!this._gettingAutoSizeHTML) {
    this._firstDrawnCol = drawRect[2];
    this._lastDrawnCol = drawRect[3];
    }

    // colNum can be passed to render one column only - used for auto-sizing
    // or if passed an array can specify a specific set of columns - used for 
    // rendering an entire row (without spacers), for (EG) showing row HTML as a drag-tracker
    // discreteCols parameter implies the colNum array passed in is a set of specific cols
    // to render (used when we're determining auto-size of a set of discontiguous columns) - 
    // in this case startCol / endCol aren't actually going to be used
    var colsArray = colNum != null && isc.isAn.Array(colNum),
        startCol, endCol;
    if (!colsArray) discreteCols = false;
    
    if (colNum != null) {
        if (colsArray) {
            startCol = colNum[0];
            endCol = colNum[1] + 1;
         } else {
            startCol = colNum;
            endCol = colNum +1;
        }        
    } else {
        startCol = this._firstDrawnCol;
        endCol = this._lastDrawnCol + 1;        
    }
    
    var colNums;
    if (discreteCols) colNums = colNum;
    else {
        colNums = [];
        for (var i = startCol; i < endCol; i++) {
            colNums[colNums.length] = i;
        }
    }

    // total columns we'll be drawing, for colSpans
    var numCols = colNums.length;

    // if "colNum" has been passed such that we are returning the HTML for just one column, we
    // are essentially in showAllColumns mode in the sense that we don't want to adding
    // padding/margins to compensate for unrendered columns
    var showAllColumns = (this.shouldShowAllColumns() || colNum != null);

	// Draw
	// ---------------------------------------------------------------------------------------

	var output = isc.StringBuffer.create(),
        fields = this.fields, 
		sizes = this._fieldWidths;

    // remember the specified width of the first column when we draw.  This helps us prevent
    // unnecessary redraw on resize; see setColumnWidths()
    this._colWidthAtDraw = colNums[0] != 0 ? null : this._fieldWidths[0];

    var leftColPad, rightColPad, totalHorizontalWidth, padType;
    if (!showAllColumns || this.leftSpace != null || this.rightSpace != null) {
        leftColPad = (this.leftSpace != null) ? this.leftSpace : 0;
        rightColPad = (this.rightSpace != null) ? this.rightSpace : 0;
        
        // figure out size of columns to left and right of visible area
        if (!showAllColumns) {
            leftColPad += this._fieldWidths.slice(0, startCol).sum();
            rightColPad += this._fieldWidths.slice(endCol, this._fieldWidths.length).sum();
        }
        totalHorizontalWidth = this._fieldWidths.sum()
        
        //this.logWarn("column pads: " + [leftColPad, rightColPad] + " type:" + padType);
        padType = (this.cacheDOM || (isc.Browser.isIE && !(isc.Browser.isIE8 || isc.Browser.isIE9)) 
            ? "margin" : "padding");
        //padType = "padding";
    }
    var autoFit = this.autoFit;

    var widthHTML = "";
    if (colNum != null) {
        if (!autoFit && this.fixedColumnWidths) {
            // if rendering just one column, size it to 100% of it's containing Canvas, since
            // the Canvas will be sized to the column width
            widthHTML = " WIDTH=100%";
        }
    } else if (this.isPrinting && this.autoFit) {
        // when printing, autoFit should mean full screen
        widthHTML = " WIDTH=100%";
    
    } else if ((isc.Browser.isIE8Strict || isc.Browser.isMoz || isc.Browser.isSafari) 
                && !autoFit) 
    {
        // total size of the table we're drawing (NOTE: may be larger or smaller than the body
        // Canvas, since the body Canvas is a viewport on to this table)
        var tableWidth = this._fieldWidths.slice(startCol, endCol).sum();
        
        widthHTML = " WIDTH=" + tableWidth;
    }
    
    // output a blank spacer in a DIV that is as tall as all the records before the table.
    // This causes the scrollable area to be as large as if we were drawing all records,
    // so the thumb is the correct size and scrolling works as expected.
    
    // In some cases we explicitly specify additional space to show above / below the
    // rows in a GridRenderer (this.startSpace / endSpace)
    // If this.startSpace is non null, add it to the calculated height of the undrawn 
    // start rows.
    // Note that in this case the range of rows is shifted down - already handled by
    // _getViewportFillRows
    var startSpacerHeight = this.startSpace || 0;
    if (fragment) startSpacerHeight = 0;
    if (startRow != rangeStart) {
        var undrawnRowHeight = ((startRow - rangeStart) * this.getAvgRowHeight());
        this._startRowSpacerHeight = undrawnRowHeight;
        startSpacerHeight += undrawnRowHeight;
    } else {
        this._startRowSpacerHeight = 0;
    }
    
    
    
    var canResizeSpacerDivs = true;
    var totalHeight = (rangeEnd - rangeStart) * this.getAvgRowHeight();
    if (isc.Browser.isIE) {
        if (totalHeight > 1300000) canResizeSpacerDivs = false;
    }
    if (!fragment) this._canResizeSpacerDivs = canResizeSpacerDivs;
    
    
    if (totalHeight > 10000000) {
        this.logWarn("This grid is showing " + (rangeEnd - rangeStart).toLocalizedString()
            + " rows. Due to native rendering limitations, grids with this many rows" 
            + " may not appear correctly on all browsers. Consider filtering the data"
            + " displayed to the user to reduce the total number of rows displayed at a time."
            + " This will improve usability as well as avoiding unpredictable behavior.");
    }
    
    if (!this.cacheDOM && !this.isPrinting) {
        // If the space is zero sized, we still want to write out the spacer div so we can handle
        // setStartSpace() etc without a redraw
        // In IE specifying the height as zero px won't work, so set display none instead to ensure
        // the spacer takes up no space
        // Give the spacer DIV an ID so we can look at it's height, etc. later.
        // When we resize this on the fly (in setStartSpace()) we'll set display back to the default
        // (inline) if necessary.
            
         output.append("<DIV style='width:1px;");
         if (canResizeSpacerDivs) {
             output.append("height:", startSpacerHeight, "px;overflow:hidden;");
         }
         if (startSpacerHeight == 0) output.append("display:none;");
         output.append("' ");
         
         if (fragment || this.isPrinting) {
             output.append(">");
         } else {
             output.append(" ID="+ this.getID()+ "_topSpacer>");
         }
         output.append(isc.Canvas.spacerHTML(1, startSpacerHeight), "</DIV>");
    } 
    
	//
	//	output the start table tag
	//
	// XXX: If height of the list is screwy in IE5 until the cursor passes over it, 
	//			we should set the height of the table explicitly
    // Note: We divide large tables into chunks so we can assemble the HTML in separate threads
    // (avoids script is running slowly message). Avoid writing out the outer table tags for every
    // chunk so the HTML ends up in a single table.
    if (!this._printingChunk || (startRow == 0 && !this.printChunkOnly)) {
        if (isc.screenReader) {
            output.append("<div id='", this._getScreenReaderCellSeparatorID(), "' style='display:none'>",
                          this.screenReaderCellSeparator, "</div>",
                          "<div id='", this._getScreenReaderRowSeparatorID(), "' style='display:none'>",
                          this.screenReaderRowSeparator, "</div>");
        }
        output.append(
            
            "<TABLE role='presentation' BORDER=0", widthHTML, 
            ((!fragment && !this.isPrinting) ? " ID=" + this.getTableElementId() : null),
            (this.tableStyle && isc.Browser.isDOM ? 
             " CLASS='" + this.tableStyle + this._$singleQuote : isc._emptyString),
            " CELLSPACING=" , this.cellSpacing,	
            " CELLPADDING=" , this.cellPadding,
            " STYLE='",
            
            (isc.Browser.isDOM && !autoFit && this.fixedColumnWidths ? 
             "table-layout:fixed;overflow:hidden;" : ""),
            
            (!showAllColumns ? 
                padType + (this.isRTL() ? "-right: " : "-left:") + leftColPad + "px;" + 
                padType + (this.isRTL() ? "-left:" : "-right:") + rightColPad + "px;"
            : ""),
    
            
            (this.cacheDOM && this._startRowSpacerHeight > 0
                ? "margin-top:" + this._startRowSpacerHeight + "px;" : ""),
            
            // if we plan to scroll immediately after draw, draw the table as hidden, so we don't
            // momentarily see it in the wrong scroll position
            (this._targetRow != null && !(isc.Browser.isIE && this._avoidRedrawFlash) 
                && !this.isPrinting ? "visibility:hidden;" : ""),
            "'>",
            
            (isc.Browser.isMoz ? "<TBODY></TBODY>" : "")
        );

        
        var vPad = 0, hPad = 0,
            // get style we'll use on the first record, used for sizing calculations
            firstRecordStyle = this._getFirstRecordStyle();

        // In strict mode / HTML5 mode we have to knock off specified vertical padding/border
        // to get the height to write into table cells
        
        if (isc.Browser.isStrict &&
            ((isc.Browser.isMoz && isc.Browser.version >= 17) ||
             (isc.Browser.isSafari || isc.Browser.isIE)))
        {
            
            if ((isc.Browser.isIE && isc.Browser.version < 8) ||
                (isc.Browser.isSafari && isc.Browser.safariVersion < 530)) 
            {
                hPad = this._getCellHBorderPad();
            }
            
            vPad = (this.fixedRowHeights ? 0 : this.cellPadding * 2);
            vPad += isc.Element._getVBorderSize(firstRecordStyle);
        }
        // store pad amounts since they are needed on cell refresh
        this._vPad = vPad;
        this._hPad = hPad;

        output.append("<colgroup>");

        
        if (!autoFit && isc.Browser.isDOM) {
            
            for (var i = 0; i < colNums.length; i++) {
                output.append("<COL WIDTH=" , (sizes[colNums[i]] - hPad), ">");
            }
        }

        output.append("</colgroup>");

        
        output.append("<TBODY>");
    }

    
	var cellHeight = this.cellHeight,
        // Do we need to write a DIV into the cell (See comments in _writeDiv())
        writeDiv = this._writeDiv(cellHeight),
        nowrap = !this.wrapCells && !isc.Browser.isIE8Strict,
        skipNOBR = !nowrap || writeDiv,
        cellWrapHTML = (skipNOBR ? "" : "<NOBR>"),
        cellWrapHTMLClose = (!this.closeNOBRs || skipNOBR ? "" : "</NOBR>")
	;

    var singleCells = 0;

	// Draw rows
	// --------------------------------------------------------------------------------------------
	if (isc.Browser.isDOM) {

        
        
        

        // template of cell HTML
        var cellHTML = [],
            ariaSlot = 1, heightAttrSlot = 2, heightSlot = 3, alignSlot = 5,
            valignAttrSlot = 6, valignSlot = 7, widthSlot = 8,
            minHeightCSSSlot = 10, cssStartSlot = 11, styleSlot = 18,
            cellIDSlot, cellIDs, divStartSlot = 21, cellValueSlot = 24;
        cellHTML[0] = "<TD";
        // [1] ARIA attributes if enabled
        // [2] height attribute, if set (per row)
        // [3] height value, if set (per row)
        cellHTML[4] = " ALIGN=";
        // [5] align (per col) and rowSpan (per cell) if necessary
        // [6] valign attribbute, if set (per cell)
        // [7] valign value, if set (per cell)
        // [8] width (per col) OR colspan when drawing a one-cell row (per row).
        //     Ends with an open STYLE='
        //cellHTML[9] = ";filter:Alpha(opacity=100);";
        // [gap]
        // [10] min-height css text - used for rows with shouldFixRowHeight() == false where
        //     this.fixedRowHeights as a whole is true
        // [11] cssText range start (per cell)
        // [12] used if this.fastCellUpdates is true!
        // [gap]
        cellHTML[16] = skipNOBR && !this.wrapCells ? ";white-space: nowrap;" : null;
        // [17] fastCellUpdates: close STYLE attribute; no CLASS attribute will be written
        // [17] normal: close STYLE attribute, start CLASS attribute
        cellHTML[17] = this.fastCellUpdates ? "' " : "' class=";
        // [18] cell style (when not using fastCellUpdates)
        // don't write out cell element id's for fragments - it's possible that we'd end up with
        // duplicate IDs that way.
        if (!fragment && !this.isPrinting && (isc.screenReader || this.getCellElementId)) {
            cellHTML[19] = " id=";
            cellIDSlot = 20;
        }
        // [20] cell ID (per cell, optional)
        // [21] DIV start to force correct cellHeight, if necessary (per table)
        // [22] rest of DIV to force column width if writeDiv (per column)
        cellHTML[23] = ">" + cellWrapHTML; // ">" + wrap (per table)
        // [24] value range start (per cell)
        // [gap]
        cellHTML[30] = cellWrapHTMLClose + (writeDiv ? "</DIV></TD>" : "</TD>");

        var rowStart = "<TR",
            rowEnd = "</TR>",
            gt = ">",
            heightAttr = " HEIGHT=",
            valignAttr = " VALIGN=";

        if (grid && grid.canDragRecordsOut && grid.useNativeDrag) {
            rowStart += " draggable='true'";
        }

        
        if (isc.Browser.isMobileWebkit) rowStart += " onmousedown='return true;' style='-webkit-tap-highlight-color: rgba(0,0,0,0)'";

        // make row elements programmatically focuseable
        
        var tabIndexString;
        if (isc.screenReader) {
            tabIndexString = " tabIndex=-1";
        }
        // whether to write ARIA attributes
        var ariaEnabled = isc.Canvas.ariaEnabled();

        // these are used only when cells have rowSpans (only possible if getRowSpan() has been
        // defined)
    
        // colNum -> number of remaining cells to skip (for columns where a cell spans into the
        // current row)
        var cellSkips = [],
            // number of cells that will be skipped in this row.  Increased when spans start,
            // decreased when they end
            skipCount = 0, 
            // colNum -> start row of rowSpanning cell (for columns where a cell spans into the
            // current row)
            cellSkipSourceRows = [];

        // rowSpanning: outerSpanCount is a map of logical rowNum -> count of number of DOM cells that
        // have actually been written into the first column - the "visual" row number if
        // spanning cells in the leftmost column are taken to define "rows".
        var outerSpanCount = this._outerSpanCount = {};

        this._cacheColumnHTML(colNums, autoFit, hPad, writeDiv);

        if (this.isPrinting && (!this._printingChunk || (startRow == 0 && !this.printChunkOnly))) {
            
            output.append(this.getPrintHeaders(startCol, endCol));
        }

        var isFrozenBody = this._isFrozenBody();
        if (cellIDSlot != null) {
            cellIDs = new Array(colNums.length);
        }
		// output each record in turn
		for (var rowNum = startRow; rowNum < endRow; rowNum++) {
            //>Animation
            
            var isAnimationRow = (!fragment && this._animatedShowStartRow == rowNum);
            //<Animation

            if (cellIDSlot != null) {
                var physicalRowNum = rowNum - startRow;
                for (var i = 0; i < colNums.length; ++i) {
                    var colNum = colNums[i],
                        physicalColNum = colNum - startCol;
                    if (this.getCellElementId) {
                        cellIDs[physicalColNum] = this.getCellElementId(rowNum, physicalRowNum, colNum, physicalColNum);
                    } else {
                        
                        cellIDs[physicalColNum] = this.ID + "_"+"cell" + rowNum + "_" + colNum;
                    }
                }
            }

			// get a pointer to the record for this row.
            // NOTE: record can be null.  The various routines below (eg getCellValue) are
            // expected to handle this.
			var record = this.getCellRecord(rowNum);
	
			// If this row is a separator or is not loaded yet, we draw a single cell with
            // COLSPAN set to extend across the entire table.
			var drawRecordAsSingleCell = //>Animation
                                         isAnimationRow ||   //<Animation
                                         this._drawRecordAsSingleCell(rowNum, record);
			// start the table row
            output.append(rowStart);
            if (tabIndexString != null) {
                // Assign the correct tabIndex plus focus/blur handlers to the single row
                // that should get native focus if we're not allowing native focus to go to the handle
                if (this.screenReader_suppressHandleFocus) {
                    if (this.getNativeFocusRow() == rowNum) {
                        output.append(" tabIndex=", this.getTabIndex(), 
                                    isc.Canvas._onFocus, this._getNativeFocusHandlerString(),
                                    isc.Canvas._onBlur, this._getNativeBlurHandlerString());
                    } else {
                        output.append(tabIndexString);
                    }
                } else {
                    output.append(tabIndexString);
                }
            }
            if (!fragment && !this.isPrinting  && this.getRowElementId) {
                output.append(" ID=", this.getRowElementId(rowNum, rowNum-startRow));
            }
            if (ariaEnabled) {
                var rowRole = this.getRowRole && this.getRowRole(rowNum, record);
                if (rowRole) output.append(" role='", rowRole, "'");

                // output attributes such as "selected"
                var rowState;
                if (this.getRowAriaState != null) {
                    rowState = this.getRowAriaState(rowNum, record);
                } else if (cellIDSlot != null && isc.screenReader) {
                    rowState = { labelledby: null };
                }
                if (rowState != null) {
                    if (cellIDSlot != null && isc.screenReader && rowState.labelledby == null) {
                        rowState.labelledby = (cellIDs.join(" " + this._getScreenReaderCellSeparatorID() + " ") + " " +
                                               this._getScreenReaderRowSeparatorID());
                    }
                    output.append(isc.Canvas.getAriaStateAttributes(rowState));
                }
            }
            output.append(gt);

            // set per-row pieces of cell HTML

            // establish row height to clip content (fixedRecordHeights:true) or as a minimum
            // (fixedRowHeights:false)

            // use the getRowHeight function if it's defined, otherwise use the cellHeight
            // property
            var rowHeight = //>Animation
                            isAnimationRow ? this._animatedShowRowHeight :
                            //<Animation                         
                            (this.getRowHeight != null ? 
                             this.getRowHeight(record, rowNum, isFrozenBody) : 
                             cellHeight);

            // If this widget has a 'shouldFixRowHeight()' method, check whether that returns
            // false (enables override of 'fixedRowHeights' on a per-row basis - currently
            // only used internally, for row-level editing of ListGrids)
            var fixedRowHeight;
            if (isAnimationRow) {
                fixedRowHeight = true;
            } else {
                fixedRowHeight = this.fixedRowHeights;
                
                
                if (fixedRowHeight && this.shouldFixRowHeight != null) {
                    fixedRowHeight = (this.shouldFixRowHeight(record, rowNum) != false);
                }
            }
                                                 
            //this.logWarn("rowNum: " + rowNum + 
            //             ", rowHeight: " + rowHeight + 
            //             ", this.fixedRowHeights: " + this.fixedRowHeights +
            //             ", this row isFixed: " + fixedRowHeight);

            // If this row is of fixed height, write the height out into the TD
            if (fixedRowHeight) {
                // write a height attribute to enforce height
                cellHTML[heightAttrSlot] = heightAttr;
                cellHTML[heightSlot] = rowHeight - vPad;
                cellHTML[minHeightCSSSlot] = null;

            // If the row can expand with content, avoid writing a height into the TD -
            // use the min-height CSSText instead
            } else {
                // don't write a height attribute at all
                cellHTML[heightAttrSlot] = null;
                cellHTML[heightSlot] = null;

                // Apply min css height to per-cell css... 
                if (!drawRecordAsSingleCell && 
                    rowHeight == this.cellHeight && !this.fixedRowHeights) 
                {
                    // null it out, already handled by CSS that establishes cell height as a
                    // minimum (in the "widthHTML" slot)
                    // Note that if we're drawing as single cell, the widthHTML slot contains
                    // colspan instead, so in this case we need min height CSSText
                    cellHTML[minHeightCSSSlot] = null;
                } else {
                    cellHTML[minHeightCSSSlot] = this._getMinHeightCSSText(record,rowNum);
                }
            }

            if (writeDiv) {
                // this method returns css text to set the height for the DIV
                
                cellHTML[divStartSlot] = ">" + this._$cellClipDivStart +
                       this._getCellDivCSSHeight(rowHeight, record, rowNum, isAnimationRow);
                if (nowrap) cellHTML[divStartSlot] += "white-space:nowrap;";
            }

            // If we're drawing the record as a single cell, figure out which cells it's spanning
            
            var singleCellSpan = drawRecordAsSingleCell ?      
                                    this._getSingleCellSpan(record,rowNum,startCol,endCol) : null;
            //if (skipCount > 0) {
            //    this.logWarn("rowSpan start rows for row: " + rowNum + 
            //                 ": " + cellSkipSourceRows);
            //}

			// output each cell
			for (var i = 0; i < colNums.length; i++) {
			    colNum = colNums[i];

                if (this.useCellRecords) record = this.getCellRecord(rowNum, colNum);
                
                var field = fields[colNum],
                    cellRecord = record;
                if (cellRecord == null) cellRecord = this.getCellRecord(rowNum, colNum);

                if (cellSkips[colNum] > 0) {
                    // this cell will be skipped due to a rowSpanning cell in a previous row

                    // record the start row of the rowSpanning cell
                    field._rowSpans[rowNum] = cellSkipSourceRows[colNum];
                    //this.logWarn("recording start row: " + field._rowSpans[rowNum] +
                    //             " at " + rowNum);

                    // copy down the rowNum as determine by spanning
                    if (colNum == 0) {
                        outerSpanCount[rowNum] = rowNum > 0 ? outerSpanCount[rowNum-1] : 0
                    }

                    // reduce the count of cells remaining to be skipped due to this rowSpanning
                    // cell
                    cellSkips[colNum]--;

                    if (cellSkips[colNum] == 0) {
                        // we don't have to skip any more cells due to this rowSpanning cell
                        skipCount--;
                        // so clear the colNum -> rowNum with rowSpan cell mapping
                        cellSkipSourceRows[colNum] = null;
                    }
                    continue;
                } else if (colNum == 0) {
                    // no spanning in left column, increment row-span based count
                    outerSpanCount[rowNum] = rowNum > 0 ? outerSpanCount[rowNum-1]+1 : 0
                }

                // per column HTML (align)
                cellHTML[alignSlot] = this.getCellAlign(record, field, rowNum, colNum);
                
                // per column valign
                var vAlign = this.getCellVAlign(record, field, rowNum, colNum);
                if (vAlign != null) {
                    cellHTML[valignAttrSlot] = valignAttr
                    cellHTML[valignSlot] = vAlign;
                }

                if (singleCellSpan != null && (colNum == singleCellSpan[0])) {
                    // note singleCells variable used for logging only
                    singleCells++;
                    
                    // HTML to cause the cell to span several cells for
                    // drawRecordAsSingleCell case
                    cellHTML[widthSlot] = this._getTDSpanHTML(singleCellSpan[1]-singleCellSpan[0]);

                    // If we're writing out a DIV, we need to close the "'" around the style
                    if (writeDiv) {
                        cellHTML[divStartSlot + 1] = this._$singleQuote;
                    }

                    // We'll write out the rest of the HTML, then increment i to jump to the
                    // end of the span

                } else {
                    // per column HTML (width)
                    // XXX Actually a misnomer - this includes some height information too
                    cellHTML[widthSlot] = field._widthHTML;

                    // we have a row span function, write rowSpan into the table cell (after
                    // the specified width)
                    if (this.allowRowSpanning && this.getRowSpan) {
                        var rowSpan = this._getRowSpan(record, rowNum, colNum);
                        if (rowSpan > 1) {
                            var rowSpanText = " ROWSPAN=" + rowSpan;
                            // piggyback the rowSpan on the alignment slot
                            if (cellHTML[alignSlot] != null) 
                                cellHTML[alignSlot] += rowSpanText;
                            else 
                                cellHTML[alignSlot] = rowSpanText;
                            // set up to skip outputting cells in this column
                            cellSkips[colNum] = rowSpan - 1;
                            skipCount++;
 
                            // field._rowSpans:
                            // - is a map of rowNum -> starting rowNum of cell that spans into
                            //   that cell.  
                            // - only contains rowSpanning cells; other slots have the value
                            //   undefined.
                            // - exists only on field objects where there are rowSpanning
                            //   cells
                            if (field._rowSpans == null) field._rowSpans = {};
                            field._rowSpans[rowNum] = rowNum;

                            // remember the start row of the rowSpanning cell
                            cellSkipSourceRows[colNum] = rowNum;

                            if (colNum == 0) {
                                outerSpanCount[rowNum] = rowNum > 0 ?
                                                outerSpanCount[rowNum-1]+1 : 0
                            }

                        }
                    }

                    cellHTML[ariaSlot] = null;
                    if (ariaEnabled) {
                        var cellRole = this.getCellRole && this.getCellRole(rowNum, colNum, record);
                        if (cellRole) {
                            cellHTML[ariaSlot] = " role='" + cellRole + "'";
                        }
                        var cellState = this.getCellAriaState && this.getCellAriaState(rowNum, colNum, record);
                        if (cellState != null) {
                            var cellStateAttributes = isc.Canvas.getAriaStateAttributes(cellState);
                            var currentAriaSlotValue = cellHTML[ariaSlot];
                            cellHTML[ariaSlot] = (currentAriaSlotValue == null
                                                  ? cellStateAttributes
                                                  : currentAriaSlotValue + cellStateAttributes);
                        }
                    }

                    if (writeDiv) {
                        // also closes the style= attribute
                        cellHTML[divStartSlot + 1] = field._divWidthHTML;
                    } else {
                        // Note - if we're not writing a DIV, we're not in the middle of a style
                        // attribute, so no need for "'"
                        cellHTML[divStartSlot + 1] = null;
                    }

                }

                // set per-cell pieces of cell HTML
	            // -------------------------------------------------------------------------
                
                // cell style (CSS classname)
                var cellStyle = this.getCellStyle(record, rowNum, colNum),
                    // cell CSS text (direct value for STYLE attribute)
                    customCSSText = (this.getCellCSSText ? 
                                     this.getCellCSSText(record, rowNum, colNum) :
                                     null);
                //>Animation                       
                // always have the animation row cell have no padding / border, since the 
                // table written into it already has padding / border for each cell.
                if (isAnimationRow) {
                    var nopad = "padding:0px;border:0px;";
                    if (customCSSText) customCSSText += ";" + nopad
                    else customCSSText = nopad;
                }                                     
                //<Animation

                

                if (!this.fastCellUpdates) {
                    // in normal mode, write CLASS=[CSS className] and write custom CSS text into a
                    // STYLE attribute
                    cellHTML[cssStartSlot] = customCSSText == null ? null : String.asAttValue(customCSSText);
                    cellHTML[styleSlot] = cellStyle;
                } else {
                    
                    var styleText = this._getEscapedStyleText(cellStyle);
                    cellHTML[cssStartSlot] = styleText;
                    cellHTML[cssStartSlot + 1] = customCSSText == null ? null : String.asAttValue(customCSSText);

                    
                }

                // cell value (HTML contents)
                //>Animation
                if (isAnimationRow) {
                    // Set a flag so getTableHTML() is aware that the fragment its returning
                    // is to be used in the animated show/hide row.
                    this._writingAnimatedShowRows = true;
                    var tableHTML = this.getTableHTML(null, this._animatedShowStartRow, 
                                                                this._animatedShowEndRow);
                    delete this._writingAnimatedShowRows;
                                                                                    
                    
                    if (!writeDiv) {
                        cellHTML[cellValueSlot] = isc.SB.concat(this._$cellClipDivStart,
                                                                this._getCellDivCSSHeight(rowHeight, record, rowNum, isAnimationRow),
                                                                this._$singleQuote, this._$rightAngle, tableHTML,
                                                                "</DIV>");
                    } else {
                        cellHTML[cellValueSlot] = tableHTML;
                    }

                } else    //<Animation

                
                cellHTML[cellValueSlot] = this._getCellValue(record, rowNum, colNum);

                if (cellIDSlot != null) {
                    cellHTML[cellIDSlot] = cellIDs[colNum - startCol];
                }

                output.append(cellHTML);

                // if the record has an embedded component update its row/colNum now
                if (!fragment && cellRecord != null && this.grid._hasEmbeddedComponents(cellRecord)) {

                    // avoid calling this method multiple times if one record spans several
                    // cells (IE one record / row)
                    var ecs = this.grid._getEmbeddedComponents(cellRecord);
                    if (ecs[0] && ecs[0].rowNum == null) {
                        this.updateEmbeddedComponentCoords(ecs, cellRecord, rowNum, colNum);
                    }
                }

                // see the first cell's HTML, as a sample
                //if (!this._gotSample) {
                //    alert("cellHTML: " + cellHTML.join(""));
                //    this._gotSample = true;
                //}

				if (drawRecordAsSingleCell && (colNum == singleCellSpan[0])) {
				    // increase the counter - well skip the rest of the colNums in the 
				    // array
				    
                    i += singleCellSpan[1] - singleCellSpan[0];
                }
			}
			// end the table row
			output.append(rowEnd);

			// If we're printing and there are embedded components that span the entire row,
			// write them into a separate cell
			
			if (this.isPrinting && record && this.grid._hasEmbeddedComponents(record)) {
			    var ecs = this.grid._getEmbeddedComponents(record);
			    for (var ecIndex = 0; ecIndex < ecs.length; ecIndex++) {
			        var ec = ecs[ecIndex];
			        if (ec._currentColNum == null && ec._gridBodyPrintHTML != null) {
			            output.append(rowStart, gt, '<td colspan="',numCols,'">', 
			                ec._gridBodyPrintHTML, "</td>", rowEnd);
			            delete ec._gridBodyPrintHTML;
			        }
			    }
			}

            //>Animation
            // Skip the rows between animationStartRow and animationEndRow, since they'll be
            // written into a single row
            if (isAnimationRow) {
                rowNum = this._animatedShowEndRow -1;
            }
            //<Animation
        }
	}

	// output the end table tag
	if (!this._printingChunk || (endRow == this.getTotalRows() && !this.printChunkOnly)) {
	    if (this.grid && this.isPrinting) {
	        output.append(this.getPrintFooters(startCol, endCol));
        }
        output.append("</TBODY></TABLE>");
    }


    
    var tailRecords = rangeEnd - endRow,
        virtualScrolling = (!fragment && this._isVirtualScrolling);

    // ignore endSpace if this.cacheDOM is true - not currently supported in that mode.
    var endSpacerHeight = this.cacheDOM ? 0 : (this.endSpace || 0);
    // Ignore endSpace if a fragment was requested.
    
    if (fragment) {
        endSpacerHeight = 0;
    }
    // reset this._endRowSpacerHeight
    if (!this._gettingAutoSizeHTML) this._endRowSpacerHeight = 0;
    if (!this.showAllRows && (tailRecords != 0 || virtualScrolling)) {
        var endRowSpacerHeight = tailRecords * this.getAvgRowHeight();
        
        if (virtualScrolling && tailRecords == 0 && !fragment) {
            var minHeight = this.getViewportHeight();
            if (endRowSpacerHeight < minHeight) {
                endRowSpacerHeight = minHeight;
            }
        }

        if (!this._gettingAutoSizeHTML) this._endRowSpacerHeight = endRowSpacerHeight;
        endSpacerHeight += endRowSpacerHeight;
    }
    // NOTE: setting overflow:hidden allows later code to shrink the spacer
    // without rewriting the spacer content
    if (!this.cacheDOM && !this.isPrinting) {
 
        output.append("<DIV style='width:1px;");
        if (canResizeSpacerDivs) {
            output.append("height:", endSpacerHeight, "px;overflow:hidden;");
        }
        if (endSpacerHeight == 0) output.append("display:none;");
        output.append("' ");
         
        if (fragment || this.isPrinting) {
            output.append(">");
        } else {
            output.append(" ID="+ this.getID()+ "_endSpacer>");
        }
        output.append(isc.Canvas.spacerHTML(1, endSpacerHeight), "</DIV>");
       
    }

	//>DEBUG timing
    if (this.logIsDebugEnabled("gridHTML")) {
        var totalTime = (isc.timeStamp() - t0), 
            numCells = (numCols * (endRow - startRow)),
            perCell = (totalTime / numCells),
            perSecond = (1000 / perCell);

        // toFixed appears not to be supported in Safari
        if (perCell.toFixed != null) perCell = perCell.toFixed(2);
        if (perSecond.toFixed != null) perSecond = perSecond.toFixed(2);
        
        this.logDebug("getTableHTML: columns " + (discreteCols ? colNums 
                                                    : startCol + "->" + (endCol-1)) + 
                      ", rows " + startRow + "->" + (endRow-1) + 
                      ", time: " + totalTime + "ms (" +
                      numCells + " cells at " + 
                      perCell + "ms per cell, " + 
                      perSecond + " cells per second), " + 
                      "spacerHeights: [" + [startSpacerHeight, endSpacerHeight] + "], " +
                      "left/right pad: [" + [leftColPad, rightColPad] + "], " +
                      singleCells + " single cell rows",
                      "gridHTML"); 
    }
	//<DEBUG

	var result = output.release(false);

	if (isAsync) {
	    if (asyncCallback != null) {
	        this.fireCallback(asyncCallback, "HTML,callback", [result,asyncCallback]);
	    }
	    return null;
	}

	// now return the output
	return result;
},

_getTableHTMLDrawArea : function (startRow, endRow, setFirstAndLastDrawnRow) {
    var fragment = (startRow != null && endRow != null);

    // Figure out rows and columns to actually draw
    // ----------------------------------------------------------------------------------------

    var drawRect = this.getDrawArea(),
        grid = this.grid,
        scrollRowNum;

    if (grid) {
        if (grid._scrollCell) {
            scrollRowNum = (grid._scrollCell == null ? 0 :
                isc.isAn.Array(grid._scrollCell) ? grid._scrollCell[0] : grid._scrollCell);
        } else if (grid.data && grid.data.getFirstUsedIndex && drawRect[0] == 0) {
            scrollRowNum = grid.data.getFirstUsedIndex();
        }
        if (scrollRowNum) {
            var diff = drawRect[1] - drawRect[0],
                lastRow = scrollRowNum + diff,
                totalRows = this.getTotalRows();

            if (lastRow >= totalRows) {
                scrollRowNum -= (lastRow - (totalRows - 1))
                lastRow = totalRows - 1;
            }
            if (scrollRowNum < 0) scrollRowNum = 0;
            drawRect[0] = scrollRowNum;
            drawRect[1] = lastRow;
        }
    }

    if (!fragment) {
        var firstDrawnRow = drawRect[0],
            lastDrawnRow = drawRect[1];
        //>Animation
        // If we're doing an animated show/hide of some rows, we need to write out enough rows
        // to fill the viewport when the rows to be animated are sized at zero height (will
        // happen either initially or at the end of the draw).
        if (this._animatedShowStartRow != null) {
            lastDrawnRow += (this._animatedShowEndRow - this._animatedShowStartRow);
            var totalRows = this.getTotalRows();
            if (lastDrawnRow >= totalRows) lastDrawnRow = totalRows - 1;
        }
        //<Animation

        // NOTE: _lastDrawnRow/Col are the last row/col to
        // be drawn, logic below renders up to but not including endCol/endRow
        startRow = firstDrawnRow;
        endRow = lastDrawnRow + 1;

        if (setFirstAndLastDrawnRow) {
            this._firstDrawnRow = firstDrawnRow;
            this._lastDrawnRow = lastDrawnRow;
        }

    
    } else {
        var viewportTop = drawRect[0],
            viewportEnd = drawRect[1] + 1;

        //>Animation
        // A common use of table fragments is animating folder open/close, where we write out
        // a bunch of child rows and animate them into view.
        // In this case if we're scrolling the rows into view and we're inside the viewport,
        // each of the rows will be seen by the user so we can't skip writing any content
        // as we don't want to show blank spacers to the user.
        if (this._writingAnimatedShowRows) {
            // All off the bottom or the top - just draw a big spacer so the scrollbar adjusts
            if (viewportTop > endRow || viewportEnd < startRow) {
                startRow = endRow;
            } else {
                if (!this._slideInAnimationRows) {
                    startRow = Math.max(startRow,viewportTop);
                    endRow = Math.min(endRow,viewportEnd);
                }

            }
        }
        //<Animation
    }
    drawRect[0] = startRow;
    drawRect[1] = endRow;
    return drawRect;
},

// Arbitrary HTML to write out *inside the table* before the first row and after the
// last row, when printing.
// Used to render headers (and footers) by ListGrids - overridden in GridBody class.
getPrintHeaders : function (startCol, endCol) {
    return "";
},
getPrintFooters : function (startCol, endCol) {
    return "";
},

setFocus : function (focus, reason) {
    // in screenreader mode, we put focus into the current focus row element of the grid when
    // focus is called.
    // on mousedown, we explicitly call widget.focus() -- ensure that in this case we put focus
    // into the mouse-down row, not into whatever the current focus row happens to be.
    if (isc.screenReader && reason == "focus on mousedown") {
        var eventRow = this.getEventRow();
        if (eventRow != null && !this.isEmpty()) {
            // Handle the user clicking on the end spacer in a scrolled grid (can come up if
            // virtual scrolling is enabled). In this case we want native focus to go to the
            // last real row, not to a row that's scrolled out of view.
            if (eventRow == -2) eventRow = this.getTotalRows()-1;
            if (eventRow >= 0) {
                this._putNativeFocusInRow(eventRow, true);
            }
        }
    }
    return this.Super("setFocus", arguments);
},

// Helper to put native focus into a row if we're in screenReader mode.

_putNativeFocusInRow : function (rowNum, suppressElementFocus) {
    
    var updateElement = this.screenReader_suppressHandleFocus;
    // if necessary, clear the tabIndex/focus and blur handlers from the current focus row
    if (updateElement) this._updateHandleForFocus(false);
    this._nativeFocusRow = rowNum;
    // assign the tabIndex / focus/blur handlers to new focus row
    if (updateElement) this._updateHandleForFocus(true);

    // update this.clipHandleIsFocusHandle
    
    if (this.screenReader_suppressHandleFocus) {
        this.clipHandleIsFocusHandle = this.isEmpty();
    }

    // Treat "suppressElementFocus:false" as saying explicitly force focus into
    // the row - otherwise only actually focus if we as a widget have logical focus. 
    // That allows us to set up the row as a valid tabstop (without forcing focus into it)
    if (suppressElementFocus == null) suppressElementFocus = !this.hasFocus;
    if (suppressElementFocus) {
        return;
    }
    var element = this.getFocusHandle();
    if (element) {
        this.logDebug("_putNativeFocusInRow() about to call native focus()", "nativeFocus");
        element.focus();

        
        if (!updateElement) isc.EH._focusCanvas = null;
        this.logDebug("_putNativeFocusInRow() about to call native focus() again", "nativeFocus");
        element.focus();
        if (!updateElement) isc.EH._focusCanvas = this;
    }
},


getFocusHandle : function () {
    if (!isc.screenReader) return this.Super("getFocusHandle", arguments);
    if (isc.screenReader) {
        var rowNum = this.getNativeFocusRow();
        var row = this.getTableElement(rowNum);
        if (row != null) return row;
        return this.Super("getFocusHandle", arguments);
    }
},

// When printing, we call 'getPrintHTML()' on each embedded component.
// This method may run asynchronously.
// This callback is fired when this happens. It stores the print HTML on the component
// temporarily and calls 'getTableHTML()' passing in the callback we were passed.
gotComponentPrintHTML : function (HTML, callback) {
    
    var context = callback.context,
        component = context.component;
        
    if (context.asyncCallback == null) {
        return;
    }
        
    component._gridBodyPrintHTML = HTML;
    
    
    return this.getTableHTML(context.colNum, context.startRow, context.endRow,
        context.discreteCols, context.asyncCallback, true);
},


// When we write out the per cell HTML using templating, in fastCellUpdates:true mode,
// we write out style='<style text from css class definition>'
// This helper method will convert any single quotes to HTML entities so that they don't
// terminate the style attribute in the written-out HTML.

_escapedStyleText:{},
_getEscapedStyleText : function (styleName) {
    if (this._escapedStyleText[styleName] != null) return this._escapedStyleText[styleName];
    //this.logWarn("escaping:" + styleName);
    var styleText = isc.Element.getStyleText(styleName, true);

    this._escapedStyleText[styleName] = String.asAttValue(styleText);
    return this._escapedStyleText[styleName];
},


// Methods to return cell alignment
// Overridden on the gridbody class
getCellVAlign : function (record, field, rowNum, colNum) {
    return null;
},

getCellAlign : function (record, field, rowNum, colNum) {
    return field.cellAlign || field.align;
},


// draw record as single cell does not always span the entire row - in the ListGrid, if we have
// a checkbox field we want to show it on records even where singleCellValue is true
// startCol / endCol passed in are the start/end cols we're currently rendering
_getSingleCellSpan : function (record, rowNum, startCol, endCol) {
    return [startCol, endCol];
},

// This is some innerHTML written into the <TD for single cell values, to govern it's COLSPAN
// This is extremely time critical so only create a new string once for each 'span' we have
// requested
_getTDSpanHTML : function (span) {
    if (!isc.GridRenderer._tdSpanHTML) {
        isc.GridRenderer._tdSpanHTML = {_fixedRowHeights:{},
                                      _varRowHeights:{}};
    }
    var cache = this.fixedRowHeights ? isc.GridRenderer._tdSpanHTML._fixedRowHeights 
                                      : isc.GridRenderer._tdSpanHTML._varRowHeights;
    if (cache[span]) return cache[span];
    else {
        return cache[span] = " COLSPAN=" + span  + " STYLE='" + 
                           (this.fixedRowHeights ? "padding-top:0px;padding-bottom:0px;" : "");
    }
},
                           
                

// Returns the base style of the first record. Used to calculate sizing for cells based on 
// the style's padding etc. [making the assumption that the padding etc is constant across potential
// cell styles]
_getFirstRecordStyle : function () {
    var grid = this.grid,
        rowNum = 0;

    if (grid) {
        if (grid._scrollCell) {
            rowNum = grid._scrollCell == null ? 0 :
                isc.isAn.Array(grid._scrollCell) ? grid._scrollCell[0] : grid._scrollCell;
        } else if (grid.data && grid.data.getFirstUsedIndex) {
            rowNum = grid.data.getFirstUsedIndex();
        }
    }

    return (this.getBaseStyle != null ? 
            this.getBaseStyle(this.getCellRecord(rowNum,0), 0, 0) :
            this.baseStyle);
},

_cacheColumnHTML : function (colNums, autoFit, hPad, writeDiv) {
    var fields = this.fields, 
		sizes = this._fieldWidths;
    
  
	// compute per-column HTML
	for (var i = 0; i < colNums.length; i++) {
	    var colNum = colNums[i],
	        field = fields[colNum];

        field._rowSpans = null; // clear old rowSpan info

        // NOTE: this slot must end in "STYLE='" so that the next slot can be arbitrary CSS text
        if (autoFit) {
            // don't write widths
            field._widthHTML = (isc.Browser.isIE && !isc.Browser.isIEStrict) ? " STYLE='" : " STYLE='OVERFLOW:hidden;";
            // have to reset this HTML in case settings change
            field._divWidthHTML = this._$singleQuote;
        } else {
            
            var styleStart = isc.Browser.isIE8Strict ? " STYLE='overflow:hidden;" : 
                                                       " STYLE='";
            // NOTE: Moz and Win IE5 require that we set the widths of columns in their
            // cells as well as in the COL tags or things won't always display correctly.  
            field._widthHTML = (isc.Browser.isIE ? 
                                " WIDTH=" + (sizes[colNum] - hPad) + styleStart :
                                " STYLE='" + this._getCSSTextForColWidth(colNum));

            if (writeDiv) {
                field._divWidthHTML = (this._getFieldDivWidthCSSText(colNum) +
                                       this._$singleQuote);
            }

            
        }

        // when fixedRowHeights is false, we want the cellHeight to act as a minimum height.  
        
        if (!this.fixedRowHeights) {                  
            field._widthHTML += this._getMinHeightCSSText();
        }
            
        
        if (this.fixedRowHeights) field._widthHTML += "padding-top:0px;padding-bottom:0px;";
    }
},

//> @method gridRenderer.cellValueIsClipped() ([A])
// Is the value in a given cell clipped?
// @param rowNum (number) row number of the cell
// @param colNum (number) column number of the cell
// @return (Boolean) null if there is no cell at the given row, column; otherwise, whether the
// value in the specified cell is clipped.
// @see cellValueHover()
// @visibility external
//<
cellValueIsClipped : function (rowNum, colNum) {
    var cellElem = this.getTableElement(rowNum, colNum);
    var clipDiv;
    if (!this._writeDiv(this.cellHeight)) clipDiv = cellElem;
    else clipDiv = this._getCellClipDiv(cellElem);
    return this._cellValueIsClipped(clipDiv);
},
_cellValueIsClipped : function (clipDiv) {
    if (clipDiv == null) return null;
    return isc.Element.getClientWidth(clipDiv) < clipDiv.scrollWidth;
},

// _writeDiv() : Do we need to write a DIV into the grid cells?

// this is re-used for every clipDiv
_$cellClipDivStart:"<DIV role='presentation' cellClipDiv=true style='overflow:hidden;" + isc.Browser._textOverflowPropertyName + ":ellipsis;",

// In some cases we need to write a DIV to clip cells either vertically or horizontally; writing
// overflow hidden and specifying a height or width simply doesn't cause clipping.
_writeDiv : function (cellHeight) {

    

    
    return (isc.Browser.isSafari ||
            
            (isc.Browser.isOpera && !this.autoFit && 
             (this.fixedColumnWidths || this.fixedRowHeights)) ||
            
           (isc.Browser.isMoz && isc.Browser.geckoVersion >= 20040113 && 
            this.fixedColumnWidths && !this.autoFit) ||

            
                (this.shouldEnforceVClipping() && 
                    // Moz or IE Strict (Safari covered above - always on)
                    (isc.Browser.isMoz || 
                        (isc.Browser.isIE && (isc.Browser.isStrict || isc.Browser.version >= 10))
                    )
                )
            );            
},

// Should the record be drawn as a single cell, spanning all the cols in the table?
_drawRecordAsSingleCell : function (rowNum, record) {
    //!DONTCOMBINE
    // If this row is a separator or is not loaded yet, we draw a single cell with COLSPAN
    // set to extend across the entire table.
    return (record && 
            (record[this.singleCellValueProperty] != null || record[this.isSeparatorProperty] || 
             
             (Array.isLoading(record) &&
                !(isc.Browser.isSafari && (rowNum == 0 || rowNum == this._firstDrawnRow)) )
            )
           );
},

// Method to return the CSS text to specify the height of the div written into a cell

_useMaxHeightForCellDivCSSHeight : function () {
    return (isc.Browser.isMoz || isc.Browser.isSafari || (isc.Browser.isIE &&
                                                          ((isc.Browser.version >= 7 && isc.Browser.isStrict) ||
                                                           isc.Browser.version >= 10)));
},
_getCellDivCSSHeight : function (rowHeight, record, rowNum, isAnimationRow) {
    // vertically clip if..
    var shouldWriteHeight =
                            //>Animation it's the animation row on a row reveal animation,
                            // where if we don't clip, we'll just reveal the new rows
                            // immediately
                            isAnimationRow ||  //<Animation
                            // we're in a situation where we normally enforce clipping on every
                            // row (see comments in _writeDiv)
                            (this.shouldEnforceVClipping() && 
                            // we don't have an implementation of shouldFixRowHeight() that is
                            // telling us not to clip this individual row.  Note this is
                            // currently only used internally in order to allow the edit row to
                            // expand.
                             (this.shouldFixRowHeight == null ||
                              this.shouldFixRowHeight(record, rowNum) != false));

    


    if (shouldWriteHeight) {

        // Note: since we override padding-top and padding-bottom, we should not need
        // to adjust for it in the height of the DIV if fixedRowHeights is true.
        var divHeight = rowHeight - 2*this.cellSpacing - 
                
                (isAnimationRow ? 0 : 2);
        if (rowNum == this._editRowNum) {
            for (var i = 0; i < this._editRowForm.getItems().length; i ++) {
                divHeight = Math.max(this._editRowForm.getItems()[i].getHeight(), this._editRowForm.getItems()[i].iconHeight);
            }
        }

        

        // avoid writing out a negative height
        
        if (divHeight < 1) divHeight = 1;
        return (this._useMaxHeightForCellDivCSSHeight() ? "MAX-HEIGHT:" : "HEIGHT:") + divHeight + "px;";
    }

    return isc._emptyString;
}, 

// Method to return the CSS text for the width of the div written into a cell.
// NOTE: Assumed to be written in as part of a STYLE=... attribute.  
_getFieldDivWidthCSSText : function (colNum) {

    // No need to set a width if we don't have fixed column widths - we already have a minimum
    // from the size written into the TD, and it breaks calculation of scroll width in Safari.
    if (!this.fixedColumnWidths || this.autoFit) return isc.emptyString;

    return "WIDTH:" + this.getInnerColumnWidth(colNum) + "px;";
},                    

_getMinHeightCSSText : function (record, rowNum) {
    
    
    var height = (rowNum != null ? this.getRowHeight(record, rowNum, this._isFrozenBody()) : this.cellHeight),
        IE = isc.Browser.isIE, strict = isc.Browser.isStrict;
    
    if (strict) height -= this._vPad;

    if (IE && !strict && !(this.autoFit || !this.fixedColumnWidths)) {
        return "MIN-HEIGHT:" + height + "px;"
    }
    return "HEIGHT:" + height + "px;"
},

_getCSSTextForColWidth : function (colNum) {
    if (isc.Browser.isIE || this.autoFit) return isc._emptyString;
    
    
    if (this._colWidthCSSText == null) {
        this._colWidthCSSText = [];
        for (var i = 0; i < this._fieldWidths.length; i++) {
            var width = this._fieldWidths[i];

            this._colWidthCSSText[i] = "WIDTH:" + width + 
                (this.fixedColumnWidths ? "px;OVERFLOW:hidden;" : "px;");
        }
    }
    return this._colWidthCSSText[colNum];
},

// No default data model for GRs - this is implemented at the ListGrid level
getCellRecord : function (rowNum, colNum) {
    return null;
},

findRowNum : function (record) {
    return -1;
},

findColNum : function (record) {
    return -1;
},


_$divStart:"<div>", _$divEnd:"</div>",
_$spacerDivTemplate:[
    ,                                         // [0] value
    "<div style='height:",                    // [1]
    ,,,,,                                     // [2-6] requiredHeight
    "px;line-height:0px'>&nbsp;</div>"        // [7]
],
_getCellValue : function (record, rowNum, colNum) { 
    //!DONTCOMBINE
    var isPrinting = this.isPrinting,
        value = this.getCellValue(record, rowNum, colNum, this);
    // If a record has an associated component to display, add a spacer underneath the record
    // to force the contents to draw above the component.

    if (!isPrinting && this._writeEmbeddedComponentSpacer(record)) {
        var details = this.getMaxEmbeddedComponentHeight(record),
            spacerDivTemplate = this._$spacerDivTemplate;
        if (details.allWithin) {
            if (details.requiredHeight && (details.requiredHeight > this.cellHeight)) {
                spacerDivTemplate[0] = value;
                isc._fillNumber(spacerDivTemplate, (details.requiredHeight - Math.max(isc.ListGrid.getInstanceProperty("cellHeight"), this.cellHeight)),
                                2, 5, false);
                value = spacerDivTemplate.join(isc.emptyString);

                //isc.logWarn("In _getCellValue:  details are "+isc.echoAll(details));
            }
        } else if (details.requiredHeight > 0) {
            spacerDivTemplate[0] = value;
            isc._fillNumber(spacerDivTemplate, details.requiredHeight, 2, 5, false);
            value = spacerDivTemplate.join(isc.emptyString);

            //isc.logWarn("In _getCellValue:  details are "+isc.echoAll(details));
        }

    // write embedded components right into cells if printing...
    } else if (record && this.grid._hasEmbeddedComponents(record)) {
        var colNumOffset = 0,
            frozenColNumOffset = 0;
        if (isPrinting) {
            colNumOffset = this.grid._embeddedComponentColNumOffset;
            frozenColNumOffset = this.grid._frozenEmbeddedComponentColNumOffset;
        }
        var components = this.grid._getEmbeddedComponents(record);
        for (var i = 0, len = (components ? components.length : 0); i < len; ++i) {
            var component = components[i];
            if (component == null) continue;
            var currentColNum = component._currentColNum;
            if (isPrinting && currentColNum != null) {
                currentColNum += (component._wasFrozen ? frozenColNumOffset : colNumOffset);
            }
            if (currentColNum != colNum) {
                continue;
            }

            var isWithin = (component.embeddedPosition == this._$within);

            var cPrintHTML = component._gridBodyPrintHTML;
            if (cPrintHTML != null) {
                var nonemptyCPrintHTML = (cPrintHTML != isc.emptyString);
                if (isWithin && nonemptyCPrintHTML) {
                    cPrintHTML = this._$divStart + cPrintHTML + this._$divEnd;
                }
                if (value == this.emptyCellValue && nonemptyCPrintHTML) {
                    value = cPrintHTML;
                } else {
                    value += cPrintHTML;
                }
                // clean that property up as we go
                delete component._gridBodyPrintHTML;
            }
        }
    }

    return value;
},
_writeEmbeddedComponentSpacer : function (record) {
    return (record && this.grid._hasEmbeddedComponents(record));
},

getCellValue : function (record, rowNum, colNum) { 
    return this.emptyCellValue;
},

// Specifying Table Geometry
// --------------------------------------------------------------------------------------------


//>	@method gridRenderer.getTotalRows()
// Return the total number of rows in the grid.<br><br>
//
// NOTE: in order to create a valid grid, you must either provide a totalRows value or implement
// getTotalRows()
//
// @return (number)
// @see attr:totalRows
// @visibility external
//<
getTotalRows : function () {
    return this.totalRows;
},

// NOTE: this.fields and the GridRenderer
// fields are currently used for the following config:
// - column width
// - column header alignment
// - column cell alignment
// It may be more appropriate to handle the above via a getColumnWidth()/getAlign interface.

//> @method gridRenderer.setColumnWidth()
// Sets the width of a single column.
//
// @param colNum (number) the number of the column to resize
// @param newWidth (number) the new width
//
// @visibility external
//<
setColumnWidth : function (colNum, newWidth) {
    this.fields[colNum].width = this._fieldWidths[colNum] = newWidth;
    this._colWidthCSSText = null;
    this.markForRedraw("setColumnWidth");
},

//> @method gridRenderer.setColumnWidths()
//
// Sets the width of all columns in the grid.
//
// @param newWidths (Array) array of new widths - one for each column.
//
// @visibility external
//<
setColumnWidths : function (columnWidths) {
    var oldWidths = this._fieldWidths;

    // copy the widths so we aren't affected if the caller subsequently changes the array
    this._fieldWidths = columnWidths.duplicate();
    this._colWidthCSSText = null;

    if (oldWidths != null && columnWidths != null && oldWidths.length == columnWidths.length) {
        // same number of columns
        
        // widths unchanged means no need to redraw
        if (oldWidths == columnWidths) return;
        var changed = false;
        for (var i = 0; i < oldWidths.length; i++) {
            if (oldWidths[i] != columnWidths[i]) changed = true; 
        }
        if (!changed) return;

        
        if (!this.fixedColumnWidths && !this.wrapCells && this.isDrawn() && 
            columnWidths.length == 1) {
                
                // NOTE: for the oldMinimum, we want the specified size as of the last time we
                // drew; in the meantime, resizes that did not cause a redraw may have changed
                // the logical column width
                var oldMinimum = this._colWidthAtDraw || oldWidths[0],
                    newMinimum = columnWidths[0],
                    renderedSize = this.getColumnSize(0);
                
                // If the drawn size was the old minimum, a change in minimum requires a
                // redraw: a lower minimum means the content might draw smaller, and a higher
                // minimum means it must draw larger.
                // If the drawn size is less than the new minimum, the column will have to
                // expand to that new minimum.
                // Therefore the drawn size will change unless the rendered size exceeded the 
                // old minimum and exceeds or is equal to the new minimum.
                //this.logWarn("oldMinimum: " + oldMinimum + ", newMinimum: " + newMinimum +
                //             ", renderedSize: " + renderedSize);
                if ((oldMinimum == newMinimum) || 
                    (renderedSize > oldMinimum && renderedSize >= newMinimum)) {
                    return;
                }
        }
    }

    this.markForRedraw("setColumnWidths");
},

shouldRedrawOnResize : function (deltaX, deltaY, animating) {
    if (this.redrawOnResize != null) return this.redrawOnResize;
 
    // quick hack: if we are being resized in a Layout because some other member is animating
    // don't redraw until the animation completes, otherwise our redraw will make the animation
    // lurch.
    if (isc.isA.ListGrid(this.parentElement) &&
        isc.isA.Layout(this.parentElement.parentElement)) 
    {
        var siblings = this.parentElement.parentElement.getMembers();
        if (siblings && siblings.map("isAnimating").or()) return false;
    }

    // redraw if our new size reveals more rows or columns
    if (this._needRowRedraw() || this._needColumnRedraw()) return true;
    
    // if we're showing the empty message we need to redraw since we write a static width / height
    // into the empty message
    if (this.isEmpty()) return true;

     
    return false;
},


// string methods: getRowSpan

getRowHeight : function (record, rowNum) {
    var height = this.updateHeightForEmbeddedComponents(record, rowNum, this.cellHeight);
    return height;
},

updateHeightForEmbeddedComponents : function (record, rowNum, height) {
    
    if (record && this.grid._hasEmbeddedComponents(record)) {
        var details = this.getMaxEmbeddedComponentHeight(record, rowNum);
        if (details.allWithin) {
            // all components are "within", use the max of the row height and the max comp height
            height = Math.max(height, details.requiredHeight);
            //this.logWarn("in updateHeightForEmbeddedComponents ("+this.grid+"): details are "+isc.echoAll(details)+"\nheight is "+height);
        } else {
            // some components are NOT "within" - add the max comp height to the row height
            height += details.requiredHeight;
            //this.logWarn("in updateHeightForEmbeddedComponents ("+this.grid+"): details are "+isc.echoAll(details)+"\nheight is "+height);
        }
    }
    return height;
},

getMaxEmbeddedComponentHeight : function (record, rowNum) {
    var components = this.grid._getEmbeddedComponents(record) || [],
        maxComponentHeight = 0,
        allWithin = true,
        isPrinting = this.isPrinting
    ;
    
    
    
    for (var i = 0; i < components.length; i++) {
        var component = components[i];
        
        if (!component) continue;
        // when printing, we write colspanning components into a separate row
        
        if (isPrinting) continue;

        // mark the component with the row it currently appears in
        if (rowNum != null) component._currentRowNum = rowNum;
        
        // see if the component has an embeddedPosition of "within" - if not all components have
        // this position, an extra spacer is created to show components below the normal values
        var isWithin = (component.embeddedPosition == this._$within);
        if (!isWithin) allWithin = false;

        // get the component's current drawn height
        var tempHeight = component.getVisibleHeight(),
            componentHeight = 0
        ;
        // if the component does not have "within" position, use its drawn height for comparison
        // otherwise, use the drawn height only if its larger than the natural rowHeight
        if (!isWithin) componentHeight = tempHeight;
        else if (tempHeight > this.cellHeight) componentHeight = tempHeight;
        
        // expand the row so that the component appears under the normal cells
        if (component._percent_height != null) {
            // the component has a percent height - this is a percentage of the cellHeight
            component.height = component._percent_height;
            componentHeight = this.cellHeight;
        }
        var origHeight = component.specifiedHeight;
        if (isWithin && origHeight && isc.isA.String(origHeight) && origHeight.contains("%")) {
            // if there was a specified height, and it was a percentage, assume cellHeight (by
            // comparing against zero)
            componentHeight = 0;
        }
        if (componentHeight > maxComponentHeight) {
            maxComponentHeight = componentHeight;
        }
    } 

    return { allWithin: allWithin, requiredHeight: maxComponentHeight };
},

_getRowSpan : function (record, rowNum, colNum) {
    if (!this.allowRowSpanning || !this.getRowSpan) return 1;
    var span = this.getRowSpan(record, rowNum, colNum);
    return Math.min(span, this.getTotalRows() - rowNum);
},

//> @method gridRenderer.getCellStartRow()
// When using +link{getRowSpan,row spanning}, returns the row number where a row-spanning cell
// starts.
// <P>
// For example, if row 2 col 0 spans 3 cells, <code>getCellStartRow()</code> for row 2 col 0,
// row 3 col 0, row 4 col 0 will all return 2, because that's the row when spanning starts.
//
// @param rowNum (int) row number of cell for which the start row should be returned
// @param colNum (int) column number of cell for which the start row should be returned
// @return (int) row number where spanning starts
// @visibility external
//<
getCellStartRow : function (rowNum, colNum) {
    var field = this.fields[colNum];
    if (field == null) return null;

    var spans = field._rowSpans;
 
    // no spanning at/above this cell
    if (spans == null || spans[rowNum] == null) return rowNum;

    //this.logWarn("span at row/col: " + [rowNum, colNum] + " start row: " + spans[rowNum]);

    return spans[rowNum];
},

//> @method gridRenderer.getCellRowSpan()
// When using +link{getRowSpan,row spanning}, returns the number of cells spanned by the cell
// at the given coordinates.
// <P>
// If the passed coordinates are in the middle of a series of spanned cells, the row span of
// the spanning cell is returned.  For example, if row 2 col 0 spans 3 cells, calls to
// <code>getCellRowSpan()</code> for row 2 col 0, row 3 col 0, row 4 col 0 will all return 3.
// <P>
// This method returns row span information for the current rendered cells.  In contrast, if
// the grid is about to be redrawn, a call to <code>getRowSpan()</code> may return row span
// values for how the grid is about to be drawn.  Also, user-provided getRowSpan() functions
// are not required to operate properly when called outside of the grid rendering loop.
// <P>
// <b>Note:</b> This method is a utility method for developers - it is not called
// directly by the grid rendering path and therefore is not intended for override. To
// set up custom row-spanning behavior, override +link{getRowSpan()} instead.
//
// @param rowNum (int) row number of cell to return the row span for
// @param colNum (int) column number of cell to return the row span for
// @return (int) number of cells spanned by the cell that spans through these coordinates
// @visibility external
//<
getCellRowSpan : function (rowNum, colNum) {
    var spans = this.fields[colNum]._rowSpans;
    if (spans == null) return 1; // no spanning on this field

    var startRow = this.getCellStartRow(rowNum, colNum);

    // iterate down rows until we hit a different span start row 
    var currentRow = rowNum + 1,
        // span extends at least from startRow to rowNum
        spannedCells = rowNum - startRow + 1;
    // see how much further this span extends
    while (currentRow <= this._lastDrawnRow &&
           spans[currentRow] == startRow)
    {
        currentRow++;
        spannedCells++;
    }
    return spannedCells;
},

// Embedded Components
// --------------------------------------------------------------------------------------------

// You can call addEmbeddedComponent to associate a component with a given record and row or cell
// The "position" attribute specifies how the component should appear
// - "expand" (the default): After being added, the component will appear "embedded" in the 
//   row, beneath the row's data.
// - "within": The component will appear aligned with the top left edge of the row or cell (may use
//   snapTo to specify different edge to attach to). If percentage sizing is specified, component
//   will size to percentage of record.
//
// NOTE: embedded components are currently only supported for uses of the gridRenderer that
// return some record for each row.  We could associate components with row numbers, but in
// most uses an embedded component changes row number (eg ListGrid sort, TreeGrid
// expand/collapse), so we'd need the caller to tell us about rowNum changes.
// 


// Method to actually attach a component to a record
_$within:"within",
_$expand:"expand",
_$cell:"cell",
addEmbeddedComponent : function (component, record, rowNum, colNum, position) {
    if (position == null) position = this._$expand;
    // if position is "expand", or fixedRowHeights is false (and the 
    // embedded component height > specified row height) we may expand records.
    var mayChangeRowHeight = ((position == this._$expand) || !this.fixedRowHeights);

    // instantiate the component if it's passed as just properties
    if (!isc.isA.Canvas(component)) {
        component.autoDraw = false;
        
        var cons = isc.ClassFactory.getClass(component._constructor);
        if (cons == null) cons = isc.Canvas;
        
        component = cons.create(component);
    }

    var moveOnly = false;
    // if addEmbeddedComponent is called twice on the same comp, remove before embedding!
    if (this._embeddedComponents && this._embeddedComponents.contains(component)) {
        // already embedded at the right spot = a no op
        // Note: 
        if (this.grid._hasEmbeddedComponents(record) && this.grid._getEmbeddedComponents(record).contains(component) &&
            component.embeddedPosition == position && 
            component._currentRowNum == rowNum && component._currentColNum == colNum) 
        {
            return;
        }
        // we can avoid a redraw if
        // position is within, this.fixedRowHeights is true,
        // and position is unchanged
        // (and we're not dirty already)
        if (position == component.embeddedPosition && !mayChangeRowHeight) {            
            moveOnly = !this.isDirty();
        }
        // third param to suppress clear / redraw - we'll take care of that
        this.removeEmbeddedComponent(component.embeddedRecord, component, true);
        
    } else if (!mayChangeRowHeight) {
        moveOnly = !this.isDirty();
    }

    // Make the record hang onto the component
    this.grid._addEmbeddedComponent(record, component);

    // add the component to the list of embedded components.
    if (this._embeddedComponents == null) this._embeddedComponents = [];
    this._embeddedComponents.add(component);

    component.embeddedPosition = position;
    component.embeddedRecord = record;

    // set up the current row / colNum passed in
    // note that if we redraw to render the embedded component these will be recalculated
    // anyway, but by setting up an initial currentColNum we ensure the component is
    // embedded in a cell rather than a row!
    component._currentRowNum = rowNum;
    component._currentColNum = colNum;
    // for frozen columns, mark the component with the id of the GridBody it's being stored in
    component._embedBody = this.getID();

    // if position == "within" we'll handle percentage sizing and snapTo ourselves
    // unexposed flag to disable standard snapTo / percent sizing logic
    component.percentBox = "custom";
    
    // add it as a child (which will force a draw, and give us a size) - hide it first so it
    // doesn't appear and then get moved into place
    // temporarily suppress adjustOverflow while we do this so we don't show huge
    // scrollbars if the thing is sized to 100% wide or tall
    if (component.parentElement != this) {
        var wasSuppressingAO = this._suppressAdjustOverflow; 
        this._suppressAdjustOverflow = true;
        component.hide();
        this.addChild(component);
        if (wasSuppressingAO == null) delete this._suppressAdjustOverflow;
    }

    this.observe(component, "resized", function (deltaX, deltaY) {
        this._handleEmbeddedComponentResize(component, deltaX, deltaY);
    });

    // don't redraw the component when the grid redraws, otherwise we'll be redrawing embedded
    // components continually during scrolling.  NOTE: it may be that this should be the
    // default for parents that have a mixture of content and children.
    component.__oldRedrawWithParent = component._redrawWithParent;
    component._redrawWithParent = false;   

    // prevent bubbling of most mouse events while the component is embedded.  We still want 
    // mouseWheel events to bubble or it feels like scrolling is broken when embeddedComponents
    // scroll under the mouse.  We prevent other events because otherwise,
    // cellClick, recordClick et al will fire while the component is embedded, which is usually
    // wrong.
    component._origBubbleMouseEvents = component.bubbleMouseEvents;
    if (!component.bubbleMouseEvents) {
        // prevent most mouse events from bubbling
        component.bubbleMouseEvents = ["mouseDown", "mouseUp", "click", "doubleClick", "contextClick"];
    }

    // If the component is going to expand the row we'll need a redraw
    // Also, if we don't know the rowNum / colNum for the record, this will get picked up at redraw
    // time
    // Otherwise we just move the canvas into place.
    if (moveOnly && (rowNum == -1 || colNum == -1)) {     
        moveOnly = false;
    }
    if (moveOnly) {
        this.placeEmbeddedComponent(component);
    } else {
        // redraw, which will draw the row at the new height and place the component
        this.markForRedraw("added embedded component");
    }
    return component;
},

_handleEmbeddedComponentResize : function (component, deltaX, deltaY) {
    var position = component.embeddedPosition;

    
    
    // if the embedded component resizes vertically, redraw so the row becomes the right size
    if (position != this._$within) {
        if (deltaY!=null && deltaY!=0) this.markForRedraw('embedded component resized');
        
    // If positioned within the cell, respond to resized (EG adjustOverlow) by
    // repositioning so snapTo continues to work...
    } else {
        this.placeEmbeddedComponent(component);
    }

},

// updateEmbeddedComponentCoords() called when we render out a record with an embedded component
// rowNum / colNum indicate the rowNum/colNum on which the record has been rendered
// (colNum is ambiguous on 1 record/row model) 
// Default behavior leaves colNum untouched. Note that if we have one record per cell we might
// want to update colNum here, but we don't know that about our data model - rely on overrides
// to achieve this if required.
updateEmbeddedComponentCoords : function (components, record, rowNum, colNum) {
    components.setProperty("_currentRowNum", rowNum);
},

// place an embedded component over the correct row.
// Ideally this would only be called on sort, dataChanged, etc -- currently being called
// on every body redraw (may impact performance when incremental scrolling, for example)
placeEmbeddedComponent : function (component) {
    var rowNum = component._currentRowNum;
    if (rowNum == null || (this._firstDrawnRow == null || this._lastDrawnRow == null)
                    || (rowNum < this._firstDrawnRow || rowNum > this._lastDrawnRow)) 
    {
        // row is no longer drawn - clear component
        
        if (component.isDrawn()) component.clear();
        return;
    }

    if (!component.removeOnHideField && 
            component._currentFieldName && !this.grid.getField(component._currentFieldName)) 
    {
        // if we're trying to place a component that's attached to a field which is not visible,
        // eg. following a call to grid.hideField(), clear the component
        if (component.isDrawn()) component.clear();
        return;
    }

    // Note: If we're not fitting to a specific col, and we're showing
    // horizontal scrollbars,  we could fit to the viewport or
    // fit to the content (span the entire grid).
    // We'll fit to the viewport - standard usage in this case is a kind of
    // floating sub-component that we want to be visible for interactions.
    
    var record = component.embeddedRecord,
        position = component.embeddedPosition,
        colNum = component._currentColNum,
        topOrigin = this.getRowTop(rowNum),
        leftOrigin = (colNum != null && colNum >= 0) ? this.getColumnLeft(colNum) : null,
        // to make component snap to right of visible area, use getInnerWidth() to demarcate
        // the snap area, or the sum of all column widths, whichever is smaller
        width = (colNum != null && colNum >= 0) ? this.getColumnWidth(colNum) : 
                            Math.min(this.getInnerWidth(), this._fieldWidths.sum()) ;
    if (leftOrigin == null) {
        if (!this.isRTL()) {
            leftOrigin = this.getScrollLeft();
        } else {
            // _shiftScrollLeftOrigin will give us back the scroll position adjusted
            // to the child abs-coord space - so hard left becomes a negative value and
            // hard right becomes zero.
            leftOrigin = this._shiftScrollLeftOrigin(this.getScrollLeft(), true);
        }
    }
        
//        this.logWarn("Placing embedded component " + component + ", row/col:" + [rowNum,colNum]
//                + ", top/left cell origin:" + [topOrigin,leftOrigin] + ", position:" + position,
//                "embeddedComponents");    
    if (position == this._$within) {
        // Respect "snapTo" if specified
        // *Note: we are suppressing standard canvas percent sizing and snap-to behavior
        // so we can explicitly size / position based on cell coordinates
        var snapTo = this.getEmbeddedComponentSnapTo(component, record, rowNum, colNum),
            snapEdge = component.snapEdge || snapTo;
           
        // figure out sizes before placing!
        var height;
        if (this.allowRowSpanning && this.getRowSpan) {
            // Assume this method only runs for actual rendered rows (so if there's 
            // a span, we're the first spanned cell.
            // If colNum is null we're looking at a per-row component - just size
            // to fit the left-most column
            var span = this.getRowSpan(record, rowNum, colNum==null ? 0 : colNum);
            if (span == null) span = 1;
            height = 0;
            for (var i = 0; i < span; i++) {
                height += this.getRowSize(rowNum + i);
            }
        } else {
            height = this.getRowSize(rowNum);
        }
        var cpw = component._percent_width, 
            cph = component._percent_height,
            cw, ch;
        // for "within" components we want the "bottom" to be the bottom of the row
        // content only -- IE we don't want to center over the row as expanded to 
        // accomodate "expand" type components.
        // Adjust the size to account for this
        var expandedComponentDelta = 0,
            components = this.grid._getEmbeddedComponents(record);
        for (var i = 0; i< components.length; i++) {
            var expComponent = components[i];
            
            if (expComponent == null) continue;
            var isWithin = (expComponent.embeddedPosition == this._$within);
            if (isWithin) continue;
            var componentHeight = expComponent.getVisibleHeight();
            if (componentHeight > expandedComponentDelta) {
                expandedComponentDelta = componentHeight;
            }
        }
        height -= expandedComponentDelta;

        // If positioned offset from the left, shrink the target space
        if (component.snapOffsetLeft) width -= component.snapOffsetLeft;

        if (isc.isA.String(cpw) && cpw.endsWith("%")) {
            cw = Math.round((parseInt(cpw) * width) / 100);   
        }
        if (isc.isA.String(cph) && cph.endsWith("%")) {
            ch = Math.round((parseInt(cph) * height) / 100);
        }
            
        var compHeight = ch != null ? ch : component.getHeight(),
            compWidth = cw != null ? cw : component.getWidth();

        if (ch || cw) {
            component.resizeTo(cw, ch);
             // retain percentages so we reflow correctly!
            component._percent_width = cpw;
            component._percent_height = cph;
        }
        // pass row/column dimensions to snapToEdge in lieu of a canvas
        isc.Canvas.snapToEdge([leftOrigin, topOrigin, width, height], snapTo, component, snapEdge);
            
    } else {
        // NOTE: if you need multiple "expand" components to expand a single row, generally
        // you're expected to use a Stack or Layout to manage them.

        // float at the bottom of the row, rather than the top 
        
        //topOrigin += this.cellHeight;
        component.moveTo(leftOrigin, topOrigin);
        
        // Percent specified height on an "expand" component is unusual.
        // If encountered, treat as a percentage of standard cell-height
        var cw, ch,
            cph = component._percent_height;
        if (isc.isA.String(cph) && cph.endsWith("%")) {
            ch = Math.round((parseInt(cph) * this.cellHeight) / 100);
        }
        

        
        // Note that resizing horizontally (only) may still
        // adjust the visibleHeight of the component due to overflow
        cw = width;
        
        component.resizeTo(cw, ch);
    }

    var showing = this.isDrawn();
    if (showing && !component.isDrawn()) component.draw();
    
    // at this point we can measure the component to see if it forces vertical expansion of the
    // row
    // Note that the row's height as written into the dom is set via getRowHeight() which already
    // checks the 'visibleHeight' of all embedded components, so the cases we're catching here are
    // if the visibleHeight was misreported before this method ran.
    // We've seen this happen when the component overflows its specified size, specifically:
    //  - if the setWidth() call above caused an already drawn component to reflow and overflow
    //    vertically in a different manner
    //  - if the component was scrolled out of view and clear()'d [cleared components return
    //    specified height from adjustOverflow], and scrolling back into view is draw()ing it again.
    //
    // Note: We observe the 'resized' method of the embedded component, so when resizing due
    // to the component size changing (such as adjustOverflow from the setWidth call above)
    // we get a markForRedrawCall() which in turn runs '_placeEmbeddedComponents()' and resizes/
    // repositions all embedded components.
    
    
    // If we're currently in the process of redrawing however, we'll be marked as dirty so this
    // if resized trips here in response to the above setWidth or draw(), markForRedraw will have
    // no effect - catch this case (via an isDirty() check) and explicitly size the
    // row to the new desired height.
    // NOTE: when we redraw we run through '_placeEmbeddedComponents' which positions/sizes
    // all embedded components. We do this in rowNum order, so if there are subsequent embedded
    // components rendered into the grid these should get shifted down automatically if a row
    // above them is expanded by this method.
    
    
//    if (position != this._$within) {
        var redrawing = this.isDirty(),
            expectedRowHeight = this.getRowHeight(record,rowNum,this._isFrozenBody()),
            // we need to size the row if
            // - the resized observation didn't trip and mark us as dirty
            // - we are already mid-redraw so being marked as dirty had no effect
            
            needsResize = !this.isDirty() || redrawing;

        if (needsResize && (expectedRowHeight != this.getRowSize(rowNum))) {
            this.setRowHeight(rowNum, expectedRowHeight, record);
            // refreshing the content ensures we re-write the spacer, which causes the
            // content to top-align properly
            this.refreshRow(rowNum);
        }
//    }

    if (showing) {
        if (position != this._$within) {
            var offset = this.getDrawnRowHeight(rowNum) - component.getVisibleHeight() - 1;
            component.moveTo(null, this.getRowTop(rowNum) + offset);
        }
        if (!component.isVisible()) {            
            if (this.shouldAnimateEmbeddedComponent(component)) {
                component.animateShow();
            } else {
                component.show();
            }
        }
    }
    
    this.updateEmbeddedComponentZIndex(component);
},


alignSnapToMap:{
    left:{
        top:"TL",
        center:"L",
        bottom:"BL"
    },
    right:{
        top:"TR",
        center:"R",
        bottom:"BR"
    },
    center:{
        top:"T",
        center:"C",
        bottom:"B"
    }
},


getEmbeddedComponentSnapTo : function (component, record, rowNum, colNum) {
    if (component.snapTo != null) return component.snapTo;
    if (colNum == null) {
        return "TL"
    }
    
    var align = this.getCellAlign(record, this.fields[colNum], rowNum, colNum) || "center",
        valign = this.getCellVAlign(record, this.fields[colNum], rowNum, colNum) || "center";

    var result = this.alignSnapToMap[align][valign];
    //this.logWarn("result:"+ result);
    return result;
},

// should this embeddedComponent animate show? 
shouldAnimateEmbeddedComponent : function (component) {
    return false;
},

// update the zindex of embedded components. Overridden at the LG level by default
updateEmbeddedComponentZIndex : function (component) {
},
 
getEmbeddedComponent : function (record, colNum) {
    // support specifying rowNum instead
    
    if (isc.isA.Number(record)) record = this.getCellRecord(record, 0);

    var components = this.grid._getEmbeddedComponents(record);
    if (components == null) return;

    var component = null;

    if (isc.isA.Number(colNum)) 
        component = components.find({_currentColNum: colNum, _embedBody: this.getID()});
    else
        component = components.find({_embedBody: this.getID()});
    
    return component;
},

removeEmbeddedComponent : function (record, component, suppressRedraw) {
    // support specifying rowNum instead
    
    if (isc.isA.Number(record)) record = this.getCellRecord(record, 0);

    var components = this.grid._getEmbeddedComponents(record);
    if (components == null) return;

    // support specifying component by colNum
    if (isc.isA.Number(component)) 
        component = components.find({_currentColNum: component, _embedBody: this.getID()});
        
    if (!component) // a single expansion component
        component = components.find({ _embedBody: this.getID() });

    if (!components.contains(component)) return;

    if (this.isObserving(component, "resized")) {
        this.ignore(component, "resized"); // stop watching for resizes
    }
    this.grid._removeEmbeddedComponent(record, component);
    //if (this.grid._getEmbeddedComponents(record).length == 0) this.grid._setEmbeddedComponents(record, null);
    if (this._embeddedComponents) this._embeddedComponents.remove(component);

    // reset redraw w/parent flag to original setting
    component._redrawWithParent = component.__oldRedrawWithParent;
    component.__oldRedrawWithParent = null;

    // reset bubbleMouseEvents setting
    component.bubbleMouseEvents = component._origBubbleMouseEvents;
    
    var expand = component.embeddedPosition == this._$expand;
    component.embeddedPosition = null;
    component._currentRowNum = null;
    component._currentColNum = null;
    component._embedBody = null;
    
    // suppress redraw - used when an embedded component is just being shifted to another record
    if (suppressRedraw) {
        // hide even if we don't clear/draw -- this ensures we re-animate if appropriate
        component.hide();
        return;
    }

    if (component.isExpansionComponent && this.grid.expansionComponentPoolingMode != "none") {
        // this is an expansionLayout - the first (only) member is an expansionComponent
        var exComp = component.members[0];
        if (this.grid.expansionComponentPoolingMode == "destroy") {
            // "destroy" pooling mode
            if (!exComp.isStockComponent) {
                // custom component - remove the custom component from the wrapper layout prior
                // to destroying the layout - don't destroy the custom component,
                // unless grid.destroyCustomExpansionComponents or comp.destroyOnUnembed
                // have been set to true
                if (!this.grid.destroyCustomExpansionComponents && !exComp.destroyOnUnembed) {
                    component.removeMember(exComp);
                    exComp.deparent();
                }
            }
            component.destroy();
        }
    } else {
        if (component.dontAutoDestroy) {
            this.removeChild(component);
        } else if (component.destroyOnUnEmbed) {
            component.destroy();
        } else {
            // clear it and clear up references to the record
            this.removeChild(component);        
        }
    }

    // no need to redraw if the component didn't effect the size of any content    
    if (expand) { 
        this.markForRedraw("removed embedded component");
    }

},

// before each redraw, clear the property holding the rowNum where the component was found.  Hence
// we ensure that if a component isn't found during rendering it gets hidden.
// Leave the colNum intact - 
_resetEmbeddedComponents : function () {
    var components = this._embeddedComponents;
    if (components == null) return;
    components.setProperty("_currentRowNum", null);
},

// ensure all embedded components are in the right place.  Called after every redraw.
_placeEmbeddedComponents : function () {
    var components = this._embeddedComponents;
    if (components == null) return;
    // sort by current row num. This means we place the components in the order in which they're
    // drawn within the table. If their heights change and they expand their containing rows, they
    // will therefore also change the top coords of subsequent rows
    components.sortByProperty("_currentRowNum", true);
    for (var i = 0; i < components.length; i++) {
        this.placeEmbeddedComponent(components[i]);
    }
},

// Apply a known z-index to the table so we can float embedded components below it if necessary
getTableZIndex : function () {
    // default Canvas range starts around 200000
    // Give the table a zindex of 1000 - widgets will still float above it by default (even when
    // sendToBack() is called), but we can explicitly force them to appear below it if necessary
    return 1000;
},

// Cell Styling
// --------------------------------------------------------------------------------------------



//> @attr   gridRenderer.recordCustomStyleProperty  ( "customStyle" : string : IRW)
// Denotes the name of a property that can be set on records to display a custom style.
// For example if this property is set to <code>"customStyle"</code>, setting 
// <code>record.customStyle</code> to a css styleName will cause the record in question to
// render out with that styling applied to it.  Note that this will be a static 
// style - it will not be modified as the state of the record (selected / over etc) changes.
// @see gridRenderer.getCellStyle()
// @visibility external
//<
recordCustomStyleProperty:"customStyle",

//> @attr gridRenderer.showSelectedStyle ( boolean : true : IRW )
// Should the "Selected" style be applied to selected records?
// @see gridRenderer.getCellStyle()
// @visibility external
//<
showSelectedStyle:true,

//> @groupDef cellStyleSuffixes
// As with +link{StatefulCanvas.getStateSuffix,stateful canvases}, grid cells support being 
// styled to reflect the current state of the cell by generating a css styleName from the
// specified +link{listGrid.baseStyle,baseStyle}, plus stateful suffixes.
// <P>
//    
// There are four independent boolean states, which are combined in the order given:
// <ol>
// <li>"Disabled" : whether the cell is disabled; enable by setting the "enabled" flag on record
//     returned by getCellRecord
// <li>"Selected" : whether cell is selected; enable by passing a Selection object as "selection"
// <li>"Over" : mouse is over this cell; enable with showRollovers
// <li>"Dark" : alternating color bands; enable with alternateRowStyles
// </ol>
// This leads to the following set of standard style names:
// <table border=1>
// <tr><td><b>CSS Class Applied</b></td><td><b>Description</b></td><td><b>Example</b></td></tr>
// <tr><td><code><i>baseStyle</i></code></td><td>Default css style for the cell</td>
//     <td><code>cell</code></td></tr>
// <tr><td><code><i>baseStyle</i>+Dark</code></td>
//      <td>Suffix for alternating color bands when +link{listGrid.alternateRowStyles} is true</td>
//     <td><code>cellDark</code></td></tr>
// <tr><td><code><i>baseStyle</i>+Disabled</code></td>
//      <td>Whether the cell is disabled; enable by setting the "enabled" flag on record
//     returned by getCellRecord.</td>
//     <td><code>cellDisabled</code></td></tr>    
// <tr><td><code><i>baseStyle</i>+Selected</code></td>
//      <td>Whether the cell is +link{listGrid.getSelectedRecord(),selected}. 
//      Only applies if +link{listGrid.showSelectedStyle} is true</td>
//     <td><code>cellSelected</code></td></tr>
// <tr><td><code><i>baseStyle</i>+Over</code></td>
//      <td>Mouse is over this record. Only applies if +link{listGrid.showRollOver} is true</td>
//     <td><code>cellOver</code></td></tr>
// <tr><td colspan=2><i>Combined styles</i></td></tr>
// <tr><td><code><i>baseStyle</i>+Disabled+Dark</code></td>
//      <td>Disabled style applied to cells in alternate color bands.</td>
//     <td><code>cellDisabledDark</code></td></tr>
// <tr><td><code><i>baseStyle</i>+Selected+Over</code></td>
//      <td>Style applied to selected cells as the mouse rolls over them.</td>
//     <td><code>cellSelectedOver</code></td></tr>
// <tr><td><code><i>baseStyle</i>+Selected+Dark</code></td>
//      <td>Selected style applied to cells in alternate color bands.</td>
//     <td><code>cellSelectedDark</code></td></tr>
// <tr><td><code><i>baseStyle</i>+Over+Dark</code></td>
//      <td>Style applied to alternate color band cells as the mouse rolls over them.</td>
//     <td><code>cellOverDark</code></td></tr>
// <tr><td><code><i>baseStyle</i>+Selected+Over+Dark</code></td>
//      <td>Style applied to selected, alternate color band cells as the mouse rolls over them.</td>
//     <td><code>cellSelectedOverDark</code></td></tr>
// </table>
//
// @visibility external
//<

    
//>	@method	gridRenderer.getCellStyle()
// Return the CSS class for a cell. By default this method has the following implementation:<br>
// - return any custom style for the record (e +link{GridRenderer.recordCustomStyleProperty}) 
//   if defined.<br>
// - create a style name based on the result of +link{gridRenderer.getBaseStyle()} and the 
//   state of the record using the rules described in +link{cellStyleSuffixes}.
// <p>
// Cell Styles are customizable by:
// <ul>
// <li>attaching a custom style to a record by setting 
//    <code>record[this.recordCustomStyleProperty]</code> to some valid CSS style name.  
// <li>modifying the base style returned by getBaseStyle() [see that method for further 
//     documentation on this]
// <li>overriding this function
// </ul>
// <p>
// In addition to this +link{getCellCSSText()} may be overriden to provide custom cssText to
// apply on top of the styling attributes derived from the named style.
//
//		@param	record		(ListGridRecord)	record object for this row and column 
//		@param	rowNum      (number)	number of the row
//		@param	colNum      (number)	number of the column
//
//		@return	(CSSStyleName)	CSS style for this cell
// @group	appearance
// @visibility external
//<
getCellStyle : function (record, rowNum, colNum) {
    // Allow a record to apply it's own style - ignoring our styling code
	if (record && record[this.recordCustomStyleProperty] != null) {
        return record[this.recordCustomStyleProperty];
    }

    // If not using an entirely custom style, determine the cell state and
    // get the appropriate suffix.
    var styleIndex = this.getCellStyleIndex(record, rowNum, colNum);

    return this.getCellStyleName(styleIndex, record, rowNum, colNum);
},


getCellStyleName : function (styleIndex, record, rowNum, colNum) {

    var standardSuffixes = isc.GridRenderer.standardStyleSuffixes;

    // Are we dynamically determining the baseStyle from this.getBaseStyle() ?
    // If so, concat baseStyle with the appropriate suffix
    if (this.getBaseStyle) {
            
        var baseStyle = this.getBaseStyle(record, rowNum, colNum);
        // check if the baseStyle returned is exactly the same String instance as
        // this.baseStyle, in which case we can use the precomputed style combinations.  This
        // would happen if someone defined a custom getBaseStyle that usually returns
        // this.baseStyle, infrequently returning special values.
        if (baseStyle !== this.baseStyle) {

            // append the appropriate suffix to the baseStyle
            if (styleIndex == 0) return baseStyle; // styleIndex 0 is the empty suffix
            return baseStyle + standardSuffixes[styleIndex];
        }
    }
    
    // In this case we're using the default baseStyle

    // Cache the entire set of cellStyles 
    if (!this._cellStyles) {
        this._cellStyles = [];
        for (var i = 0; i < standardSuffixes.length; i++) {
            this._cellStyles[i] = this.baseStyle + standardSuffixes[i];
        }
    }                                           
	// return the style	
	return this._cellStyles[styleIndex];
},

// return the index of the current state.  The index is a bitfield containing flags for each of
// the mutually exclusive style states: Over, Selected, Disabled, and Dark (Ledger).  The
// purpose of computing an index rather than computing the string directly is for speed.
getCellStyleIndex : function (record, rowNum, colNum) {

    // Note - we have an array of the 12 applicable suffixes in gridRenderer.standardSuffixes
    //    
    // 0 = baseStyle
    // 1 = Over(1)
    // 2 = Selected(2)
    // 3 = Selected(2) + Over(1)
    // 4 = Disabled(4)
    // 5 = Disabled(4) + Over(1) 
    // 6 = Disabled(4) + Selected(2)
    // 7 = Disabled(4) + Selected(2) + Over(1)
    // 8 = Dark(8)
    // 9 = Dark(8) + Over(1)
    // 10 = Dark(8) + Selected(2)
    // 11 = Dark(8) + Selected(2) + Over(1)
    // 12 = Dark(8) + Disabled(4)
    //
    // NOTE: By default, disabled is actually mutually exclusive with Selected and Over states,
    // so states 5-7 never happen and no style declaration is required.
    
	var styleIndex = 0;     // base style

    var useAlternateStyle = true;
    if (this.grid != null) {
        var field = this.grid.getField(this.grid.getFieldNumFromLocal(colNum, this));
        useAlternateStyle = !field ? true : field.showAlternateStyle != false;
    }

    // if alternating record or column styles, see if the cell is in a dark band
    if (useAlternateStyle) {
        var isOdd = false;
        if (this.alternateRowStyles) {
            if (this.useRowSpanStyling) {
                // rowSpan-sensitive styling: style based on "rows" determined by spans of
                // left-most cell.  So if first two left-most DOM cells span 4 and 6 rows, first 4
                // rows will be normal, next 6 dark.
                var spanRowNum = this.getSpanningRowNum(rowNum);
                isOdd = (Math.floor(spanRowNum / this.alternateRowFrequency) % 2 == 1);
                if (isOdd) styleIndex += 8;
            } else {
                isOdd = (Math.floor(rowNum / this.alternateRowFrequency) % 2 == 1);
                if (isOdd) styleIndex += 8;
            }
        }
        // if alternating column styles, see if the cell is in a dark band
        if (this.alternateColumnStyles && !isOdd) {
            isOdd = (Math.floor(colNum / this.alternateColumnFrequency) % 2 == 1);
            if (isOdd) styleIndex += 8;
        }
    }

    // Disabled?
    if (!this.cellIsEnabled(rowNum, colNum, record)) {
        styleIndex += 4;

    // Not disabled - check for selected and/or over
    } else {
    
        // if we're over the row or cell - add 1 to get the Over style
        
        if (this.shouldShowRollOver(rowNum, colNum) && !this.isPrinting && 
            rowNum == this.lastOverRow &&
            (!this.useCellRollOvers || colNum == this.lastOverCol)) 
        {
            styleIndex += 1;
        }
    
        // if selection is enabled, see if the cell is selected
        if (this.showSelectedStyle && this.selectionEnabled()) {
            var isSelected;
            if (this.canSelectCells) {
                var startRow = rowNum;
                if (this.useRowSpanStyling) startRow = this.getCellStartRow(rowNum, colNum);
                isSelected = this.selection.cellIsSelected(startRow, colNum);
            } else {
                isSelected = this.selection.isSelected(record, true);
            }
            // if the cell is selected, add 2 to get the Selected style
            if (isSelected) styleIndex += 2;
        }
	}
    return styleIndex;
},

// when rowSpanning is being used, returns the visual rowNum: the rowNum if the left-most
// column's cells are treated as row boundaries
getSpanningRowNum : function (rowNum) {
    var outerSpanCount = this._outerSpanCount;
    // if we are part of a ListGrid with frozen columns, the first column is in the frozen
    // body
    if (this.grid && this.grid.frozenBody) {
        var outerSpanCount = this.grid.frozenBody._outerSpanCount;
    }
    // TODO: support incremental rendering: go back to last non-empty entry before the
    // requested rowNum, call getRowSpan() for all intervening rows and record results.
    
    return outerSpanCount != null ? outerSpanCount[rowNum] : rowNum;
},

//>	@method	gridRenderer.cellIsEnabled()	([A])
// Whether this cell should be considered enabled.  Affects whether events will fire for the
// cell, and the default styling behavior in getCellStyle. 
//
// @group	selection, appearance
//
// @param	rowNum	(number)	row number for the cell
// @param	colNum	(number)	column number of the cell
// @return	(boolean)	whether this record is enabled or not
// @visibility external
//<

cellIsEnabled : function (rowNum, colNum, record) { return true; },

// Element IDs
// --------------------------------------------------------------------------------------------

//> @method gridRenderer.getTableElementId()  ([A])
// Get the DOM ID that should be used for the table element.  For integration with legacy test
// scripts.
// @visibility testAutomation
//<
getTableElementId : function () {
    return this.getCanvasName() + "table";
},

//> @method gridRenderer.getRowElementId()  ([A])
// Get the DOM ID that should be used for a row element.  For integration with legacy test
// scripts.
//
// When using incremental rendering, the <code>rowNum</code> param represents the
// rowNum in virtual coordinates, and the <code>physicalRowNum</code> param represents the
// index that the row will ultimately have in table.rows.
//
// @param    rowNum  (number)          <b>virtual</b> row number
// @param    physicalRowNum (number)   <b>physical</b> row number
// @visibility testAutomation
//<


//> @method gridRenderer.getCellElementId()  ([A])
// Get the DOM ID that should be used for a cell element.  For integration with legacy test
// scripts.
//
// When using incremental rendering, the <code>rowNum</code> and <code>colNum</code> params
// represents virtual coordinates, and the <code>physicalRowNum</code> param represents the
// index that the row/cell will ultimately have in table.rows or row.cells.
//
// @param    rowNum         (number)   <b>virtual</b> row number
// @param    physicalRowNum (number)   <b>physical</b> row number
// @param    colNum         (number)   <b>virtual</b> col number
// @param    physicalColNum (number)   <b>physical</b> col number
// @visibility testAutomation
//<

// Table Manipulation
// --------------------------------------------------------------------------------------------

getDOMTable : function (logicalRowNum, logicalColNum) {
    if (this.cacheDOM) return this.getTableChunkAt(logicalRowNum, logicalColNum);
    
    // bail out fast if asked for a cell that isn't currently rendered
    // this method is called from 'getTableElement()', so rowNum / colNum may be null
    if ((logicalRowNum != null && 
         (logicalRowNum - this._firstDrawnRow < 0 ||
          logicalRowNum > this._lastDrawnRow)) 
        ||
        (logicalColNum != null && 
         (logicalColNum - this._firstDrawnCol < 0 ||
          logicalColNum > this._lastDrawnCol))
       ) 
         return null;

    var table = this._tableElement;
    if (table == null) {
        var tableName = this.getTableElementId();
        var table = isc.Element.get(tableName);

        if (table == null) return null;
        // If we're mid-redraw, don't re-cache the current table element
        if (this._suppressTableCaching) {
            
            this.logInfo("getTableElement() called while updating table HTML. " +
                "This call may be invalid as the table is being rewritten in the DOM. " +
                "Suppressing caching of the current element.", "redrawing");
            return table;
        }
    }
    // cache table element
    return this._tableElement = table;
},

//>	@method	gridRenderer.getTableElement()	([A])
//		Get the element for the TD that holds a particular cell, specified as row/column indices.
//
//		If called with no parameters, returns the table itself.
//		If called with rowNum only, returns the row element
//		If called with colNum, returns a particular cell.
//
//		In all cases, returns null if the table, row, or cell cannot be found.
//
//		NOTE: calling this repeatedly is expensive as it makes multiple DOM lookups.
//
//		@param	[rowNum]	(number)	Record number to get cell for.  
//										You DO NOT need subtract the startRow from this.
//		@param	[colNum]	(number)	Field number to get cell for.
//		@return	(DOMElement)			Table, row or cell of the list body table.
//										Returns null if the element can't be obtained.
//		@group	drawing
//<

getTableElement : function (logicalRowNum, logicalColNum) {
    var table = this.getDOMTable(logicalRowNum, logicalColNum);

	if (logicalRowNum == null) return table;
    
    if (!table) return null;
		
    // if we're using incremental rendering, the HTML we draw only contains _firstDrawnRow ->
    // _lastDrawnRow, so when asked for row X we subtract _firstDrawnRow to find the
    // corresponding DOM element.  "rowNum" is now the rendered row number in the DOM table
    var rowNum = logicalRowNum - (this._firstDrawnRow > 0 ? this._firstDrawnRow : 0);

    
    if (rowNum < 0) {
        //this.logWarn("bailing on negative rowNum");
        return null;
    }
     
    var row;
    // use cached row lookup
    if (this._rowElements != null) row = this._rowElements[rowNum];
    if (row == null) row = table.rows[rowNum];
    

	if (row == null) return null;

    // cache row lookup (invalidated on body redraw)
    // If we're mid-redraw, don't re-cache the current table element
    if (!this._suppressTableCaching) {
        if (this._rowElements == null) this._rowElements = [];
        this._rowElements[rowNum] = row;
    }
	
	if (logicalColNum == null) return row;
	
    // for incremental column rendering: if we're not drawing all columns, the DOM will contain
    // cells only for the columns we actually draw.
    var colNum = logicalColNum - this._firstDrawnCol;

    
    if (colNum < 0) {
        //this.logWarn("bailing on negative colNum");
        return null;
    }

    
    if (this.allowRowSpanning && this.getRowSpan) {
        
        var startRow = this.getCellStartRow(logicalRowNum, colNum);
        if (startRow != rowNum) {
            //this.logWarn("detected spanning cell extending from " + [startRow, colNum] +
            //             " to " + [logicalRowNum, colNum]);

            // cell starts in a previous row - switch to the row that contains this cell
            rowNum = startRow;
            row = this.getTableElement(startRow);
        }
        
        if (row.childNodes.length < (this._lastDrawnCol - this._firstDrawnCol + 1)) {
            // this row has less cells than the number of columns we drew, indicating cells from
            // previous rows spanned into this row (but not at this column, which we already
            // checked).
                
            // figure out many cells are missing up to the column we're interested in
            var skips = 0;
            for (var i = 0; i < colNum; i++) {
                if (this.fields[i]._rowSpans != null && 
                    this.fields[i]._rowSpans[rowNum] != null &&
                    this.fields[i]._rowSpans[rowNum] != rowNum) skips++;
            }
            //this.logWarn("in row: " + rowNum + " skipping " + skips + " cells");

            // and adjust the column number appropriately
            colNum -= skips;
        }        
    }
 
	// actually got all the way to the cell level -- return the appropriate cell
    return row.childNodes[colNum];    
},

// Cell Style and HTML Updates
// --------------------------------------------------------------------------------------------

//>	@method	gridRenderer._updateCellStyle()
// Update the CSS styling for a cell.  Will also update the row's height, and the cell's 
// innerHTML if appropriate.
//  
//		@group	appearance
//		@param	[record]		(object)	reference to the record object who's style is being set
//		@param	rowNum      	(number)    row number of the cell
//      @param  colNum          (number)    col number of the cell
//		@param	cell	        (DHTML object)	reference to the HTML table cell element
//      @param  [className]     (string)    name of the CSS class of the style to be used
//
//<
_updateCellStyle : function (record, rowNum, colNum, cell, className) {
    
    // get the DOM cell object if not provided
    if (cell == null) cell = this.getTableElement(rowNum, colNum);
    if (cell == null) return; // cell not currently drawn

    if (record == null) record = this.getCellRecord(rowNum, colNum);
    // determine the CSS style className if not provided
    if (className == null) className = this.getCellStyle(record, rowNum, colNum);

//this.logWarn("setting: " + [rowNum, colNum] + " to className:" + className);

    if (this.fastCellUpdates) {
        
        cell.style.cssText = this._getCompleteCellCSSText(record, rowNum, colNum, className);
    } else {
        // update the classname on the DOM cell object
        if (cell.className != className) cell.className = className;
        // If this.getCellCSSText has been defined, set cell.style.cssText
        
        if (this.getCellCSSText) {
            // Use this._getCompleteCellCSSText
            // This handles the overflow settings for Moz, converting the 
            // getCellCSSText stringMethod to a method, etc.
            cell.style.cssText = this._getCompleteCellCSSText(record, rowNum, colNum, className)
        }
    }
    // if aspects of styling are incorporated into the cell's innerHTML, refresh the cell
    if (this.shouldRefreshCellHTML(record, rowNum, colNum)) {
        this.refreshCellValue(rowNum, colNum);
    }
    
    
    if (!this.isDrawn()) return;
    
    if (!this.suppressRowHeightUpdate) this._updateRowHeight(record, rowNum, className);
},

_updateRowHeight : function (record, rowNum, className) {
    
    var shouldClip = this.fixedRowHeights && 
                          (this.shouldFixRowHeight == null || 
                           this.shouldFixRowHeight(record, rowNum) != false),
        newHeight = (this.getRowHeight != null ? this.getRowHeight(record, rowNum, this._isFrozenBody()) 
                                                  : this.cellHeight);
    
    this.setRowHeight(rowNum, newHeight, record, className, shouldClip);
    
},

_$nobr:"NOBR",
_$cellClipDiv:"cellClipDiv",

_getCellClipDiv : function (cellElement) {
    if (cellElement == null) return null;
    
    var div = cellElement.firstChild;
    if (!div) return null;
    // In IE the first child of a cell is actually a NOBR element - we need to look inside that
    // to get the cell clip div
    if (div.tagName == this._$nobr) div = div.firstChild;
    if (div && 
        (div.cellClipDiv || 
         (div.getAttribute && div.getAttribute(this._$cellClipDiv)) ) ) 
    {
        return div;
    }
    return null;
},

//> @method gridRenderer.setRowHeight() 
// Sets the height of some row to the height passed in.
// This is a styling effect only - a redraw will revert to the height as derived from 
// this.cellHeight / this.getRowHeight()
// @param rowNum (number) rowNum to set height on
// @param newHeight (number) height for the row
//<
// Additional params are not required, but make the method more efficient
// Also used by showInlineEditor to make a row overflow:visible for tall editors
_$height:"height",
_$minHeight:"minHeight",
setRowHeight : function (rowNum, newHeight, record, className, shouldClip, instantOverflow) {
    var firstDrawnCol = this._firstDrawnCol,
        lastDrawnCol = this._lastDrawnCol;
    
    if (shouldClip == null) {
        if (record == null) record = this.getCellRecord(rowNum, firstDrawnCol);
        shouldClip = this.fixedRowHeights && 
                                          (this.shouldFixRowHeight == null || 
                                           this.shouldFixRowHeight(record, rowNum) != false);
    }
    
    
     var firstCell = this.getTableElement(rowNum, firstDrawnCol),
         currentSpecifiedHeight = firstCell ? parseInt(firstCell.height) : null,
         heightChanged
    ;

    if (!isc.isA.Number(currentSpecifiedHeight)) currentSpecifiedHeight = null;
    
    if (isc.Browser.isStrict &&
        ((isc.Browser.isMoz && isc.Browser.version >= 17) ||
         (isc.Browser.isSafari || isc.Browser.isIE))
    ) {                    
        if (record == null) record = this.getCellRecord(rowNum, firstDrawnCol);
        var cellStyle = className;
        if (cellStyle == null) cellStyle = this.getCellStyle(record, rowNum, firstDrawnCol)
        
        newHeight -= isc.Element._getVBorderSize(cellStyle);
        if (!this.fixedRowHeights) newHeight -= (this.cellPadding *2)
        
    }
    
    // if we were previously clipping and will not any longer
    if ((!shouldClip && currentSpecifiedHeight != null) ||
        // or we're changing the specified height (clipped or not)
        (currentSpecifiedHeight != newHeight &&
        !(currentSpecifiedHeight == null && newHeight == isc.emptyString))) 
    {
        // the height of the cell (therefore the row) has changed
        heightChanged = true;
    }
    // If the height of this row has changed, we need to update (or clear) the specified 
    // heights of each cell in the row.
    
    if (!heightChanged) return;

    
    var numericHeight = isc.isA.Number(newHeight);
    if (numericHeight && newHeight <=0) newHeight = shouldClip ? 0 : 1;
        
//    this.logWarn("height changed for cell in row: " + rowNum +
//                 ", currentSpecifiedHeight: " + currentSpecifiedHeight +
//                 ", shouldClip?:" + shouldClip + 
//                 " (derived from firstCell.height: " + firstCell.height + ")" +
//                 ", newHeight: " + newHeight);

    var currentRow = this.getTableElement(rowNum);
    // Don't crash if passed a bad row num
    
    if (currentRow == null) return;

    if (newHeight == 0 && shouldClip) {
        currentRow.style.display = "none";    
        //var firstCell = this.getTableElement(rowNum, firstDrawnCol);
        //this.logWarn("first row height is: " + firstCell.offsetHeight);
    } else {
//TODO: when printing sub-grids, we can get crashes here because currentRow is null - needs a looksee
//   } else if (currentRow != null) {
        // should theoretically be "table-row", but IE doesn't currently support that value,
        // and they all seem to accept ""
        currentRow.style.display = isc.emptyString; 

        var cssProp = (!isc.Browser.isIE || isc.Browser.isStrict) ? this._$height
                                                                  : this._$minHeight,
            cellHeight = numericHeight ? newHeight + isc.px : newHeight,
            clipDivHeight = shouldClip ? cellHeight : isc.emptyString;

        for (var i = firstDrawnCol; i <= lastDrawnCol; i++) {
            var currentCell = this.getTableElement(rowNum, i);
            if (currentCell) {
                if (shouldClip) {
                    currentCell.height = cellHeight;
                    currentCell.style[cssProp] = isc.emptyString;
                } else {
                    currentCell.height = isc.emptyString;
                    currentCell.style[cssProp] = cellHeight;
                }

                var clipDiv = this._getCellClipDiv(currentCell);
                if (clipDiv) {
                    if (this._useMaxHeightForCellDivCSSHeight()) {
                        clipDiv.style.maxHeight = clipDivHeight;
                    } else {
                        clipDiv.style.height = clipDivHeight ;
                    }
                }
            }
        }

    }

    
    if (isc.Browser.isSafari && this._forceRowRefreshForAnimation) {
        var row = this.getTableElement(rowNum);
        if (row != null) {
            row.innerHTML = row.innerHTML;
        }
    }

    // clear the cache of rowHeights since at least this one has changed, and mark for
    // adjustOverflow as the overall height of the body will have changed too.
    this._clearTableCache();
        
    if (instantOverflow) {  
        this.adjustOverflow("cell height changed");
    } else {
        this._markForAdjustOverflow("cell height changed");        
    }

},

//> @method gridRenderer._getCompleteCellCSSText() (I)
//
//  Returns complete CSS text for a cell.
//
//  If this.fastCellUpdates is true, this method will return both the raw CSS text associated
//  with the style, and any custom CSS text set up by the public getCellCSSText() method
//  If false, this method returns no style CSS text, and just falls through to getCellCSSText()
//
//		@param	record		(ListGridRecord)	record for this row or cell
//		@param	rowNum      (number)	row number
//		@param	colNum      (number)	column number
//      @param  [style]     (string)    CSS class style name to apply
//
//		@return	(CSSText)	CSS text to style this cell
//  @group	appearance
//<
_$semi:";",
_$zeroVPadding:"padding-top:0px;padding-bottom:0px;",
_$overflowHidden:"overflow:hidden;",
_getCompleteCellCSSText : function (record, rowNum, colNum, className) {
    var cssText = null;
    // Make sure top and bottom padding are set to zero if fixedRowHeights is true
    
    if (this.fixedRowHeights) cssText = this._$zeroVPadding;
    else {        
        cssText = this._getMinHeightCSSText(record,rowNum);
    }

    if (isc.Browser.isIE8Strict) {
        if (cssText == null) cssText = this._$overflowHidden;
        else cssText += this._$overflowHidden;
    }

    // For Moz, pre-pend the width and overflow cssText
    
    if (isc.Browser.isMoz || isc.Browser.isSafari) {
        if (cssText == null) cssText = this._getCSSTextForColWidth(colNum);
        else cssText += this._getCSSTextForColWidth(colNum);
    }

    if (this.fastCellUpdates) {
        // figure out the style for this cell if not provided 
        if (className == null) className = this.getCellStyle(record, rowNum, colNum);
        //this.logWarn("_getCompleteCellCSSText style: " + className);

        // get CSS text for this style
        var styleText = isc.Element.getStyleText(className, true);
        
        if (styleText == null && isc.Page._remoteStyling) {
            this.logInfo("fastCellUpdates set to true but this page loads styles from a " +
                "remote stylesheet. This is unsupported - disabling fastCellUpdates.");
            this.fastCellUpdates = false;
            this.redraw();
        }
        if (cssText != null) cssText += styleText;
        else cssText = styleText;
    }

    // Get any custom CSSText derived from this.getCellCSSText
    if (this.getCellCSSText) {
        var customCSSText = this.getCellCSSText(record, rowNum, colNum)

        if (customCSSText != null) {
            // Ensure the custom css text ends with a semi
            
            if (!customCSSText.endsWith(this._$semi)) {
                customCSSText += this._$semi;
            }
            if (cssText != null) cssText += customCSSText
            else cssText = customCSSText
        }
    }

    // If we skipped NOBR tags, write out the css equivalent.
    if (isc.Browser.isIE8Strict && !this.wrapCells) {
        cssText += ";white-space: nowrap;"
    }
    return cssText;
},

// does this cell need to update its HTML in order to show hiliting/styling
shouldRefreshCellHTML : function (record, rowNum, colNum) {
    return this.showHiliteInCells;
},


// Helper method to check that we can safely refresh a cell (or row) without delaying
_readyToRefreshCell : function (rowNum, colNum) {
    
    if ((isc.EH._handlingMouseUp || isc.EH._handlingMouseDown) && isc.EH.lastEvent.target == this) {
        
        var eventRow = this.getEventRow();
        if (eventRow != rowNum) return true;
        
        if (colNum != null) {
            var eventCol = this.getEventColumn();
            if (colNum != eventCol) return true;
        }
        // If the event occurred on the same row (and col for a cell), we can't redraw in
        // the same thread
        return false;
    }

    
    var EH = this.ns.EH;
    if (EH._handlingTouchEventSequence()) {
        var cellElem = this.getTableElement(rowNum, colNum);
        if (cellElem != null) {
            if (isc.Browser.isMobileWebkit || isc.Browser.isChrome) {
                var mouseDownEvent = EH.mouseDownEvent;
                if (mouseDownEvent != null &&
                    mouseDownEvent.DOMevent.target != null &&
                    cellElem.contains(mouseDownEvent.DOMevent.target))
                {
                    return false;
                }
            }
        }
    }

    return true;
},

//> @method gridRenderer.refreshCellValue() ([A])
// Update just cell value without updating cell styling.
// @param rowNum (number) Row number of the cell to refresh
// @param colNum (number) Column number of the cell to refresh
//<
refreshCellValue : function (rowNum, colNum) {
	// get a pointer to the cell, if possible
	var cell = this.getTableElement(rowNum, colNum);
	if (!cell) return; // cell not currently drawn

    // If we need to delay the refresh, fire again after a delay
    if (!this._readyToRefreshCell(rowNum, colNum)) {
        this.delayCall("refreshCellValue", [rowNum, colNum]);
        return;
    }

	var record = this.getCellRecord(rowNum, colNum),
		field = this.fields[colNum]
	;

    // Allow refreshing of null records - this may occur with separator rows, loading rows,
    // etc.
	if (!field) {
		//>DEBUG
		this.logDebug("refreshCell called for invalid field " + colNum); //<DEBUG	
        return;
	}

	// update the cell's innerHTML: Use the helper methods we made use of in getTableHTML() to
    // write in any additional innerHTML such as DIV tags in the cell, etc.

    // use a StringBuffer rather than normal concatenation
    var sb = isc.StringBuffer.create();

    // determine whether we need to write a DIV for this cell                   
    var writeDiv = this._writeDiv(rowHeight),
        nowrap = !this.wrapCells && !isc.Browser.isIE8Strict;
    if (writeDiv) {
        // cellclipdivstart includes an open style=' attr
        sb.append(this._$cellClipDivStart);

        // height enforcement
        var rowHeight = (this.getRowHeight != null ? this.getRowHeight(record, rowNum, this._isFrozenBody()) 
                                                   : this.cellHeight);
        sb.append(this._getCellDivCSSHeight(rowHeight, record, rowNum));

        // width enforcement (unless colspan *)
        var drawRecordAsSingleCell = this._drawRecordAsSingleCell(rowNum, record);
        if (!drawRecordAsSingleCell) {
            sb.append(this._getFieldDivWidthCSSText(colNum));
        }

        if (nowrap) sb.append("white-space:nowrap;");

        sb.append("'>");
    }

    // NOBR tags if we're not wrapping cells and we're not writing out `white-space:nowrap' on
    // a cell div.
    var writeNOBR = nowrap && !writeDiv;
    if (writeNOBR) {
        sb.append("<NOBR>");
    }
    // Get the actual value for the cell
    sb.append(this._getCellValue(record, rowNum, colNum));
    // close the NOBR tag if necessary
    if (writeNOBR && this.closeNOBRs) {
        sb.append("</NOBR>");
    }
    // close the DIV if necessary
    if (writeDiv) {
        sb.append("</DIV>");
    }
        
    // Actually apply the innerHTML to the innerHTML of the cell.    
	cell.innerHTML = sb.release(false);
},

//>	@method	gridRenderer.setCellStyle()
// Set the CSS class of a record
//		@group	appearance
//
//		@param	rowNum (number)	row number to set class of
//		@param	colNum	(number) column number to set class of
//		@param	[className]	(CSSStyleName)	name of the CSS class to set to; if not specified, 
//                                          will use getCellStyle()
//<
setCellStyle : function (rowNum, colNum, className) {
    // Just fall through to setRowStyle
    return this.setRowStyle(rowNum, className, colNum);
},


//>	@method	gridRenderer.setRowStyle()
// Set the CSS class of a record
//		@group	appearance
//
//		@param	rowNum (number)	record number to set class of.  This takes this._firstDrawnRow into account
//		@param	[className]	(CSSStyleName)	name of the CSS class to set to; if not specified, will 
//                                          use getCellStyle()
//		@param	[colNum]	(number)		column number to set class of.  If not specified, will 
//                                          set all columns in that row.
//<
setRowStyle : function (rowNum, className, colNum) {
    if (isc._traceMarkers) arguments.__this = this;
    // navigate into the DOM and change the contents of the native table cells

    // if the rowNum is null, use this.selection.lastSelectionItem
    if (rowNum == null || rowNum < 0) {
        this.logWarn("setRowStyle: bad rowNum: " + rowNum);
        return false;
    }
    
    // verify that we've drawn the table
    var cell = this.getTableElement(rowNum, colNum);

    if (cell == null) {
        // when incremental rendering is on, this is a normal condition indicating that we are
        // trying to update some row/cell that has been scrolled out of view, hence no longer
        // exists.  NOTE: don't log, we might be calling this for thousands of unrendered cells.
        //this.logDebug("setRowStyle(): cell (" + rowNum + "," + colNum + ") not present");
        return false;
    }
    
    var record = this.getCellRecord(rowNum, colNum);

    // for eg, rows that are about to be completely refreshed anyway
    if (record && record._ignoreStyleUpdates) {
        return;
    }
    
    
    // if a colNum was specified, update just the individual cell (we got a pointer to it
    // above)
    if (colNum != null) {
        this._updateCellStyle(record, rowNum, colNum, cell, className);
    } else {
                
        var row = this.getTableElement(rowNum);
        if (row != null) {
            var td = "TD", 
                firstCol = (!this.shouldShowAllColumns() ? this._firstDrawnCol : 0), 
                lastCol = (!this.shouldShowAllColumns() ? this._lastDrawnCol : this.fields.length-1),
                // If incremental rendering is enabled, the indices of the cells in the DOM 
                // will not match the colNum of the cell being updated.
                renderedCellNum = 0;

            var recursive = this.suppressRowHeightUpdate;
            this.suppressRowHeightUpdate = true;

            for (var fieldNum = firstCol; fieldNum <= lastCol; fieldNum++, renderedCellNum++) {

                var cell;
                // If we're showing columns separately, we'll style the whole table for the
                // column (Nav case)
                if (this.showColumnsSeparately || this.cacheDOM) {
                    cell = this.getTableElement(rowNum, fieldNum);


                // Otherwise we'll style the individual cells in the row.
                } else {
                    cell = row.childNodes[renderedCellNum];

                }
                if (cell == null) continue;

                // Pass in the optional record object, className and cell objects to avoid them
                // being re-calculated.
                this._updateCellStyle(record, rowNum, fieldNum, cell, className);
            }
            
            if (!recursive) {
                delete this.suppressRowHeightUpdate;
                this._updateRowHeight(record, rowNum, className);

            }
        }
    }

    // return true to indicate that we were able to update the cell(s)
    return true;
},

//>	@method	gridRenderer.refreshCellStyle()
// Refresh the styling of an individual cell without redrawing the grid.
// <P>
// The cell's CSS class and CSS text will be refreshed, to the current values returned by
// getCellStyle() and getCellCSSText() respectively.
// <P>
// The cell's contents (as returned by getCellValue()) will <b>not</b> be refreshed.  To
// refresh both styling and contents, call refreshCell() instead.
//
//		@group	appearance
//		@param	rowNum (number)	row number of cell to refresh 
//		@param	colNum	(number) column number of cell to refresh
//
// @see refreshCell() to update cell contents too
// @visibility external
//<
// NOTE:
// - className param not public because we don't persist the change
//		@param	[className]	(CSSStyleName)	name of the CSS class to set to; if not specified, 
//                                          will use getCellStyle()
refreshCellStyle : function (row, col, className) {
    // this is a synonym for setCellStyle();
    // We could also fall through to refreshCellStyles() but this would force us to create an
    // array object to pass in.
    return this.setCellStyle(row, col, className);
},

//>	@method		gridRenderer.refreshCell()	([A])
// Refresh an individual cell without redrawing the grid.
// <P>
// The cell's value, CSS class, and CSS text will be refreshed, to the current values returned
// by getCellValue(), getCellStyle() and getCellCSSText() respectively.
//
//		@group	appearance
//		@param	rowNum (number)	row number of cell to refresh 
//		@param	colNum	(number) column number of cell to refresh
//
// @see refreshCellStyle() to update just styling
// @visibility external
//<
refreshCell : function (rowNum, colNum) {
    this.refreshCellStyle(rowNum, colNum);
    // refresh the value too unless it's already been refreshed as part of styling
    if (!this.shouldRefreshCellHTML()) this.refreshCellValue(rowNum, colNum);
},

//>	@method		gridRenderer.refreshRow()	([A])
// Refresh an entire row of cells without redrawing the grid.
// <P>
// The cells' values, CSS classes, and CSS text will be refreshed, to the current values
// returned by getCellValue(), getCellStyle() and getCellCSSText() respectively.
//
//		@group	appearance
//		@param	rowNum (number)	row number of cell to refresh 
//
// @see refreshCellStyle() to update just styling
// @see refreshCell()
// @visibility external
//<
refreshRow : function (rowNum) {
    if (!this._readyToRefreshCell(rowNum)) {
        this.delayCall("refreshRow", [rowNum]);
    } 
    for (var i = 0; i < this.fields.length; i++) {
        this.refreshCell(rowNum, i);
    }
},

//>	@method	gridRenderer.refreshCellStyles()	([A])
//  @group	selection, appearance
//  
//  Update the style of a list of cells. (Used to show selection changes when cell selection is 
//  enabled)
//
//		@param	cellList    (Array) 
//              Array of [rowNum, colNum] array pairs.
//		@param	[className] (CSSStyleName)
//              Name of the CSS class to set to; if not specified, will use getCellStyle()
//
//		@return	(boolean)	true == actually updated now, false == will update later
//
//<
refreshCellStyles : function (cellList, className) {
    //>DEBUG
    this.logDebug("refreshing cell styles: " + cellList.length + " cells");
    //<DEBUG

    //this.logWarn("refresh cells: " + this.echoAll(cellList));

    // NOTE: this is a very time critical method, as it is called every mouseMove during drag
    // selection.
    
    for (var i = 0; i < cellList.length; i++) {
        var rowNum = cellList[i][0],
            colNum = cellList[i][1];

        

        var cell = this.getTableElement(rowNum, colNum);
        if (cell == null) {
            // when incremental rendering is on, this is a normal condition indicating that we
            // are trying to update some row/cell that has been scrolled out of view, hence no
            // longer exists.  NOTE: don't log, we might be calling this for thousands of
            // unrendered cells.
            //this.logDebug("setRowStyle(): cell (" + rowNum + "," + colNum + ") not present");
            continue;
        } else {
            // no need to pass in the record object, this will be calculated in _updateCellStyle()
            this._updateCellStyle(null, rowNum, colNum, cell, className);
        }
    }
    // return true to indicate that we were able to update the cell(s)
    return true;
},

// Size Detection
// --------------------------------------------------------------------------------------------

//> @method gridRenderer.getCellPageRect() ([A])
// Returns the page offsets and size of the cell at the passed row and column.  If auto-sizing 
// is enabled, sizes are not definitive until the grid has finished drawing, so calling this 
// method before drawing completes will return the configured column sizes.
// @param rowNum (number) row index of the cell
// @param colNum (number) column index of the cell
// @return (Array of Integer) the page rect of the passed cell
// @group sizing, positioning
// @visibility external
//<
getCellPageRect : function (rowNum, colNum) {
    return [
        this.getColumnPageLeft(colNum),
        this.getRowPageTop(rowNum),
        this.getColumnSize(colNum),
        this.getRowSize(rowNum)];
},

//> @method gridRenderer.getColumnLeft() ([A])
// Return the left coordinate (in local coordinate space) of a particular column.
// @param colNum (Integer) number of the column
// @return (Integer) left coordinate of the passed colNum
// @group sizing, positioning
// @visibility external
//<
getColumnLeft : function (colNum) {
    // Note: we don't have to worry about undrawn columns because this._fieldWidths has all
    // column widths, not just the drawn ones.

	// textDirection: we calculate field sizes from right to left in RTL mode
	
	if (this.isRTL()) {
		return this.getViewportWidth() - this._fieldWidths.sum(0, colNum+1);
	} else {
		// otherwise return the width of fields 0-colNum
		return this._fieldWidths.sum(0, colNum);
	}
},


//> @method gridRenderer.getColumnPageLeft() ([A])
// Return the left coordinate for a given column number as a GLOBAL coordinate
// @param colNum (Integer) number of the column
// @return (Integer) page left offset of the passed colNum, or null if undrawn or no such column
// @group sizing, positioning
// @visibility external
//<
getColumnPageLeft : function (colNum) {
    var columnLeft = this.getColumnLeft(colNum);
    if (columnLeft == null) return null;
    var left = this.getPageLeft() - this.getScrollLeft() + columnLeft;
    if (this.isRTL()) {
        if (this.vscrollOn) left += this.getScrollbarSize();
        left += this.getScrollWidth() - this.getViewportWidth();
    }
    return left;
},


//> @method gridRenderer.getColumnWidth() ([A])
// Return the width of a particular column.
// @param colNum (number) number of the column.
// @return (number) width
// @group sizing, positioning
// @visibility external
//<
getColumnWidth : function (colNum) {
	// return the width of the column from the _fieldWidths property
	return this._fieldWidths[colNum];
},

//>	@method	gridRenderer.getInnerColumnWidth()	([A])
//		Return the width of a particular column adjusted for this.cellPadding / cellSpacing.
//		@group	sizing, positioning
//		@param	colNum    (number)	number of the column.
//		@return	(number)	inner width
//<
getInnerColumnWidth : function (colNum) {
    var width = this.getColumnWidth(colNum);
    if (width == null) return null;
    
    // Note: cell spacing still breaks alignment with ListGrid headers in both firefox and IE.
    // However this is a non-exposed feature for now
    return (width - (2* this.cellSpacing + this._getCellHBorderPad()));
},

// method to get, and cache horizontal cell padding size (based on this.cellPadding and styling)
// Used for sizing the cell-level clipping div, etc.

_getCellHBorderPad : function (recalc) {
    if (!recalc && this._cellHBorderPad != null) return this._cellHBorderPad;
    
    var firstStyle = this._getFirstRecordStyle(),
        padLeft = isc.Element._getLeftPadding(firstStyle, true),
        padRight = isc.Element._getRightPadding(firstStyle, true),
        border = isc.Element._getHBorderSize(firstStyle);

    if (padLeft == null) padLeft = this.cellPadding;
    if (padRight == null) padRight = this.cellPadding;

    this._cellHBorderPad = (padLeft + padRight + border);
    return this._cellHBorderPad;
},

//>	@method	gridRenderer.getRowTop()	([A])
// Returns the top coordinate for a given row number, relative to the top of body content.  Use
// +link{getRowPageTop()} for a page-relative coordinate.
// <P>
// This method is reliable only for rows that are currently drawn, which is generally only rows
// that are visible in the viewport.  If row heights vary (see <code>fixedRowHeights</code>),
// coordinates for rows that are not currently shown are rough approximations.
//
// @param rowNum (int)
// @return (int) Y-coordinate
// @group positioning
// @visibility external
//<
getRowTop : function (rowNum) {
    // undrawn rows before or after the drawn area are treated as having fixed height
    if (rowNum < this._firstDrawnRow) return this.getAvgRowHeight() * rowNum;
    
    var undrawnHeight = this._getUndrawnHeight(),
        drawnHeights = this._getDrawnRowHeights();

    if (rowNum > this._lastDrawnRow) {
        // undrawn rows after the drawn area are treated as having fixed height
        return undrawnHeight + drawnHeights.sum() + 
                    (((rowNum-1) - this._lastDrawnRow) * this.getAvgRowHeight());
    }
    // otherwise return the sum of heights of records 0-rowNum
    return undrawnHeight + drawnHeights.sum(0, rowNum - this._firstDrawnRow);
},

//>	@method	gridRenderer.getRowPageTop()	([A])
// Returns the Y-coordinate for a given row number as a page-relative coordinate.  See
// +link{getRowTop()}.
//
// @param rowNum (int)
// @return (int) Y-coordinate
// @group positioning
// @visibility external
//<
getRowPageTop : function (rowNum) {
    return this.getPageTop() + this.getTopBorderSize() + 
                (this.getRowTop(rowNum)- this.getScrollTop());
},

//>	@method	gridRenderer.getRowSize()	([A])
// Get the drawn height of a row.
//
// @param rowNum (number)
//
// @return (number) height
// @group sizing, positioning
// @deprecated As of SmartClient 8.0, use +link{gridRenderer.getDrawnRowHeight}.
//<
getRowSize : function (rowNum) {
    return this.getDrawnRowHeight(rowNum);
},

//>	@method	gridRenderer.getDrawnRowHeight() ([A])
// Get the drawn height of a row.
//
// @param rowNum (number)
//
// @return (number) height
// @group sizing, positioning
//<
getDrawnRowHeight : function (rowNum) {

    // treat all undrawn rows as though they were cellHeight tall 
    if (this._firstDrawnRow == null || this._lastDrawnRow == null 
        || rowNum < this._firstDrawnRow || rowNum > this._lastDrawnRow) 
    {
        return this.getAvgRowHeight();
    }

    var visibleRowNum = rowNum - this._firstDrawnRow,
	    heights = this._getDrawnRowHeights();
	return heights[visibleRowNum];
},

//>	@method	gridRenderer.getColumnSize()	([A])
// Get the drawn width of a column.
//
// @param colNum (number)
//
// @return (number) width in pixels
// @group sizing, positioning
//<
// NOTE: this function must be named getColumnSize because getColumnWidth refers to specified
// width.
getColumnSize : function (colNum) {
    if ((this.fixedFieldWidths && !this.autoSize) ||
        (colNum < this._firstDrawnCol || colNum > this._lastDrawnCol)) 
    {
        // fixed sizes, or not rendered; return specified size
        return this.getColumnWidth(colNum);
    }
    var visibleColNum = colNum - this._firstDrawnCol,
        widths = this.getColumnSizes();
    return widths[visibleColNum];
},


// get the total height of all rows that are not currently drawn because they are above the
// viewport (and out of drawAhead range)
_getUndrawnHeight : function () {
    return this._firstDrawnRow * this.getAvgRowHeight();
},

// get the heights of all drawn rows
_getDrawnRowHeights : function () {
    //!DONTCOMBINE
    if (this._rowHeights != null) return this._rowHeights;

	// create a new array and store it in the _recordHeights slot
	var heights = this._rowHeights = [];

	// make sure that the table is defined by checking to make sure it exists
	//	-- if it isn't defined, return an empty list
	var table = this.getTableElement();
	if (!table || !table.rows) {
		// otherwise delete the recordHeights so we'll calculate them again
		// since this is being called prematurely (???)
		delete this._rowHeights;
		return heights;
	}

    

    
    var oldSafari = isc.Browser.isSafari && isc.Browser.safariVersion < 500;

    var rowRange = this.getDrawnRows(), //this._lastDrawnRow - this._firstDrawnRow + 1,
        drawnRows = rowRange[1] - rowRange[0] + 1,
        nonZeroHeight = false,
        firstRowBCR = null,
        prevHeightsSum = 0,
        isRTL = this.isRTL();
	for (var rowNum = 0; rowNum <= drawnRows; rowNum++) {
        var row = this.cacheDOM ? this.getTableElement(rowNum + this._firstDrawnRow) : table.rows[rowNum];

        if (row == null) {
            /*empty*/

        
        } else if (isc.Browser._hasGetBCR) {
            var rowBCR = isc.Element.getBoundingClientRect(row);
            if (firstRowBCR == null) firstRowBCR = rowBCR;
            heights[rowNum] = Math.round((rowBCR.bottom - firstRowBCR.top) - prevHeightsSum);

        
        } else if (this.allowRowSpanning && this.getRowSpan) {
            heights[rowNum] = row.offsetHeight;

        } else {
            
            var checkAllCellHeights = 
                (oldSafari &&
                    (this.fixedRowHeights == false || 
                        (this.shouldFixRowHeight != null && 
                         this.shouldFixRowHeight(this.getCellRecord(rowNum), rowNum) == false ) 
                     )
                 ),
			    cell, safariCellArray = [];

            if (!oldSafari || !checkAllCellHeights) {
                
                cell = row.lastChild;
            } else {
                for (var k = 0; k < row.childNodes.length; k++) {
                    safariCellArray[k] = row.childNodes[k]
                }
            }

            
            if (checkAllCellHeights) {
                heights[rowNum] = 0;
                for (var cellNum = 0; cellNum < safariCellArray.length; cellNum ++) {
                    var currentCell = safariCellArray[cellNum],
                        height = currentCell.offsetHeight;
                    
                     
                    var specifiedHeight = parseInt(currentCell.style ? currentCell.style.height 
                                                                     : null);
                    if (isc.Browser.isStrict) {
                        
                        if (this.cellPadding) specifiedHeight += this.cellPadding;
                        specifiedHeight += isc.Element._getVBorderPad(currentCell.className);
                    }
                    
                    if (isc.isA.Number(specifiedHeight) && specifiedHeight > height) 
                        height = specifiedHeight;
                        
                    if (height > heights[rowNum]) heights[rowNum] = height;                
                }                
                heights[rowNum] += this.cellSpacing;

            } else if (cell) { 

                if (!oldSafari) {
                    heights[rowNum] = cell.offsetHeight;
                } else {
                    // In Safari the offsetHeight is often misreported, and can't exceed
                    // the specified height for the cell, so use the specified height
                    // directly
                    
                    var cellHeight = parseInt(cell.height);
                    if (cellHeight != null && isc.isA.Number(cellHeight)) {
                        if (isc.Browser.isStrict) {
                                                        
                            cellHeight += isc.Element._getVBorderSize(cell.className);                            
                        }                         
                    } else {
                        cellHeight = cell.offsetHeight || 0;
                    }
                     
                    heights[rowNum] = cellHeight;
                }

                heights[rowNum] += this.cellSpacing;
			}
		}

        var height = heights[rowNum];
        if (height > 0) {
            nonZeroHeight = true;
            prevHeightsSum += height;
        }
	}

	// add the cellSpacing to the first record.  This makes it so when the cursor is in the
	// spacing region, it actually goes to the lower record, which looks better than it
    // going to the upper record
	heights[0] += this.cellSpacing;

    
    if (!nonZeroHeight) {
        this.logWarn("row heights not yet available; returning all zeroes");
        this._rowHeights = null;
    }

    
    if (isc.Browser.isSafari && !isc.Page.isLoaded()) this._rowHeights = null;

	return heights;
},


//>	@method	gridRenderer.getColumnSizes()	([A])
//	Get rendered column widths
//		@group	sizing, positioning
//		@return	(boolean)	null | false
//<
// NOTE: sets sets gridRenderer._renderedColumnWidths
getColumnSizes : function () {
     if (this._renderedColumnWidths != null) return this._renderedColumnWidths;

    // If undrawn, don't cache potentially incorrect values.
    if (!this.isDrawn()) return this._fieldWidths.duplicate() || [];
    
	if (this.fixedColumnWidths && isc.Browser.version >= 5) {
		 return (this._renderedColumnWidths = this._fieldWidths.duplicate());
	} else {
        // inspect the DOM to determine rendered widths
        

		// create a new array and store it in the _recordHeights slot
		var widths = this._renderedColumnWidths = [];
		
		// get the first row in the table to test to see if it's drawn
		var row = this.getTableElement(this._firstDrawnRow);
		// if the row isn't defined, 
		if (row == null) {
			// use the fieldWidths as specified in the settings
			this._renderedColumnWidths = widths.concat(this._fieldWidths);
			return this._renderedColumnWidths;
		}
			
		
		var	sizeDelta = (isc.Browser.isMac ? this.cellSpacing : 0);

		// iterate for all of the fields of the table that have been drawn, getting the sizes
		for (var colNum = 0; colNum < this.fields.length; colNum++) {
			var cell;
            if (this.showColumnsSeparately) {
                cell = this.getTableElement(this._firstDrawnRow,colNum);
            // Note leading not - This code will fire for safari 1.2, and other browsers
            } else if (!(isc.Browser.isSafari && isc.Browser.safariVersion < 125)) {
                cell = row.childNodes[colNum];
            }

			if (cell) {
				widths[colNum] = cell.offsetWidth + sizeDelta;
			} else {
				widths[colNum] = this._fieldWidths[colNum];
			}
		}
		// NOTE: we do this only in the case where we're not setting fixed widths
		this.innerWidth = this.getTableElement().offsetWidth;

        return widths;
	}
},

// Event Row/Col
// --------------------------------------------------------------------------------------------

_differentEventCharacteristics : function (eventA, eventB) {
    var rowA = this.getEventRow(this.getOffsetY(eventA)),
        rowB = this.getEventRow(this.getOffsetY(eventB));
    if (rowA != rowB) return true;
    var colA = this.getEventColumn(this.getOffsetX(eventA)),
        colB = this.getEventColumn(this.getOffsetX(eventB));
    if (colA != colB) return true;
    return false;
},

//>	@method	gridRenderer.getEventRow()
// Returns the row number of the most recent mouse event.
//		@group	events, selection
//
//		@param [y] (Integer) optional y-coordinate to obtain row number, in lieu of the y
//                        coordinate of the last mouse event
//
//		@return	(int)	row number, or -2 if beyond last drawn row
//      @visibility external
//<
getEventRow : function (y) {
    // if we're empty always return rowNum -2 (beyond the end of any valid data)
    
    if (this.isEmpty()) return -2;
    
	// If a y-coordinate was not passed, get it from the offset of the last event
	if (y == null) y = this.getOffsetY(); 
    
    // if we're showing a start spacer, knock that off from the event coordinate so we can figure 
    // out which row we hit
    if (this.startSpace) y -= this.startSpace;

    var undrawnHeight = this._getUndrawnHeight();
    // if it's a coordinate before the drawn area, treat all offscreen rows as fixed height
    if (y <= undrawnHeight) return Math.floor(y / this.getAvgRowHeight());

    var remainder = y - undrawnHeight,
	    heights = this._getDrawnRowHeights();

	// check visible rows.  Note that if it's past the end of the visible rows, inWhichPosition
    // returns -2, and so do we
	var drawnRowNum = this.inWhichPosition(heights, remainder),
        pos;
    if (drawnRowNum >= 0) {
        pos = this._firstDrawnRow + drawnRowNum;        
    } else {
        // assume the rest of the rows are fixed height
        var pastDrawnRows = remainder - heights.sum();
        pos = this._lastDrawnRow + 1 + Math.floor(pastDrawnRows / this.getAvgRowHeight());
        // Avoid returning a number higher than our total number of rows
        if (pos >= this.getTotalRows()) pos = -2;
    }

    //this.logWarn("getEventRow(" + (y == null ? this.getOffsetY() : y) + "): " +
    //             " rowHeights:" + heights +
    //             " drawn range: " + [this._firstDrawnRow, this._lastDrawnRow] +
    //             ", undrawnHeight: " + undrawnHeight +
    //             ", eventRow:" + pos);

    //this.logWarn("getEventRow(" + (y == null ? this.getOffsetY() : y) + "): " + pos);

	return pos;
},

//>	@method	gridRenderer.getEventColumn()
// Returns the column number of the most recent mouse event.
//		@group	events, selection
//
//		@param [x] (Integer) optional x-coordinate to obtain column number for, in lieu of the x
//                        coordinate of the last mouse event
//
//		@return	(int)	column number, or -2 if beyond last drawn column
//      @visibility external
//<
getEventColumn : function (x) {

	var widths = this.getColumnSizes();

	// If a x-coordinate was not passed, get it from the offset of the last event
	if (x == null) x = this.getOffsetX();	

    // In RTL mode, if the grid is leaving a scrollbar gap but the vertical scroll bar is not
    // showing on the body, we need to make an adjustment by the scrollbar size because the
    // left coordinate of the body is not offset by the scrollbar size.
    var textDirection = this.getTextDirection(),
        grid;
    
    if (!this.frozen &&
        textDirection == isc.Page.RTL &&
        !this.vscrollOn &&
        (grid = this.grid) != null &&
        grid._shouldLeaveScrollbarGap())
    {
        
        x -= this.getScrollbarSize();
    }

    return this.inWhichPosition(widths, x, textDirection);
},

// getFocusRow / col for keypress events. Overridden at the ListGrid gridBody level.

getFocusRow : function () {
    return 0;
},
getFocusCol : function () {
    return 0;
},

// Helper to return the row that should get native focus in screenReader mode
// will basically match the focusRow- but could differ from it if (EG) the focus row isn't actually
// drawn.
getNativeFocusRow : function () {

    var rowNum = this._nativeFocusRow;
    if (rowNum == null) rowNum = this.getFocusRow();
    
    // default to top of viewport if we don't have a row already, or it is undrawn.
    
    var rows = this.getDrawnRows();
    if (rows != null && rows.length > 0 && 
        (rowNum == null || rowNum < rows[0] || rowNum > rows[1])) {
        rowNum = this._getViewportFillRows()[0];
        // if first row is partially offscreen jump to next row to avoid auto-scroll
        if (this.getRowTop(rowNum) < this.getScrollTop()) rowNum += 1;
    }
    return rowNum;

},

//>	@method	gridRenderer.getNearestRowToEvent()
//			Returns the nearest row to the event coordinates
//		@group	events, selection
//      @visibility external
//<
getNearestRowToEvent : function () {
    var rowNum = this.getEventRow();
    if (rowNum < 0) {
        var visibleRows = this.getVisibleRows();
        if (rowNum == -1) return visibleRows[0];
        if (rowNum == -2) return visibleRows[1];
    }
    return rowNum;
},

//>	@method	gridRenderer.getNearestColToEvent()
//			Returns the nearest column to the event coordinates
//		@group	events, selection
//      @visibility external
//<
getNearestColToEvent : function () {
    var colNum = this.getEventColumn();
    if (colNum < 0) {
        var visibleColumns = this.getVisibleColumns();
        if (colNum == -1) return visibleColumns[0];
        if (colNum == -2) return visibleColumns[1];
    }
    return colNum;
},

// Note: viewport rows / visible rows / drawn rows
// =================
// There is a distinction here between:
// - "rows we need to draw to fill the viewport" (getViewportFillRows) 
// - "drawn rows that are currently visible in the viewport" (getVisibleRows)
// - "rows we've actually drawn" (this._firstDrawnRow -> this._lastDrawnRow)
//
// With drawAheadRatio > 1, rows we've drawn clearly differ from the other two.  With variable
// height cells, viewportFillRows differ from visible rows since we don't know how tall the
// cells will be before we draw them; the last viewportFillRow may actually be rendered below
// the viewport.  With fixedRowHeights:false and a drawAheadRatio > 1, the first viewport fill
// row may be below the top of the viewport.
// 
// Generally:
// - we use the viewportFillRows only to determine how many rows to draw / whether to redraw
// - we use _firstDrawnRow/_lastDrawnRow to do DOM manipulation
// - event handling code that cares about the viewport (particularly D&D) uses visible rows

_getViewportFillRows : function () {

    var viewportHeight = this.getViewportHeight(),
        avgRowHeight = this.getAvgRowHeight()
    ;
    
    // scrollTop/cellHeight is the number of rows that fit into the space above the viewport, so
    // round down for the index of the first row to stick into the viewport
    var firstVisible = Math.floor(this.getScrollTop() / avgRowHeight);

    

    var nRecords;
    if (this.autoFitData == "both" || this.autoFitData == "vertical") {
        nRecords = this.getTotalRows() - firstVisible;

        var maxRecords = this.autoFitMaxRecords;
        if (maxRecords != null && maxRecords < nRecords) nRecords = maxRecords;

        var totalHeight = this.getAutoFitMaxHeight();
        if (totalHeight != null) {
            var maxRecordsByHeight = Math.ceil(totalHeight / this.cellHeight);
            if (maxRecordsByHeight < nRecords) nRecords = maxRecordsByHeight;
        }
    } else {
        nRecords = Math.ceil(viewportHeight / this.cellHeight);
    }
    // [firstVisible, lastVisible] is an *inclusive* range, so subtract one here
    var lastVisible = firstVisible + nRecords - 1; 

    // if we're showing an explicit spacer at the top, it'll shift the rows down
    // take that into account now
    if (this.startSpace) {
        var spaceRows = Math.floor(this.startSpace / avgRowHeight);
        firstVisible = Math.max(0, firstVisible - spaceRows);
        lastVisible = Math.max(0, lastVisible - spaceRows);
    }
    
    // Are we virtual scrolling? Don't rely on this._isVirtualScrolling in case data or
    // fixedRowHeights, etc has changed
    var vscrolling = this.virtualScrolling && this._targetRow != null;
    if (vscrolling) {
        if (firstVisible == 0 && lastVisible >= (this.getTotalRows()-1)) vscrolling = false;
    }
    
    if (!vscrolling) return [firstVisible, lastVisible];

    
    // when using virtual scrolling, calculate the rows that need to be drawn to fill the
    // viewport based on the target row that needs to be scrolled into view (scrollTop is
    // irrelevant)
    
    //this.logWarn("calculating viewport based on targetRow: " + this._targetRow +
    //             ", row offset: " + this._rowOffset);

    var startCoord = this._targetRow;
    // if we have a large negative offset (targetRow will be well below viewport), ensure
    // enough rows are rendered above the targetRow
    if (this._rowOffset < 0) startCoord += Math.floor(this._rowOffset / this.cellHeight);
    if (startCoord < 0) startCoord = 0;
    var endCoord = startCoord + Math.ceil(viewportHeight / this.cellHeight);
    
    return [startCoord, endCoord];
    
},

// Arbitrary average row height for incremental rendering and variable row heights
// default to a typical row height given a few lines of wrapping text
// Used whenever we need to know / estimate how tall undrawn rows will render


avgRowHeight:60,
getAvgRowHeight : function () {
    return this.fixedRowHeights ? this.cellHeight : Math.max(this.cellHeight,this.avgRowHeight);
},

//>	@method	gridRenderer.getVisibleRows()
// Get the rows that are currently visible in the viewport, as an array of 
// [firstRowNum, lastRowNum]. 
// <p>
// If the grid contains no records, will return [-1,-1].  Will also return [-1,-1] if called at
// an invalid time (for example, data is in the process of being fetched - see
// +link{ResultSet.lengthIsKnown()}).
// 
// @return (Array of int)
// @visibility external
//<
// NOTE: the viewport can extend beyond the last row or column, in which case the last row or
// column is reported as the last visible.
getVisibleRows : function () {

    if (isc.ResultSet && isc.isA.ResultSet(this.data) && !this.data.lengthIsKnown())
    {
        return [-1,-1];
    }

    var scrollTop = this.getScrollTop();
    var rows = [
        this.getEventRow(scrollTop),
        this.getEventRow(scrollTop + this.getInnerHeight())
    ];
    // viewport extends beyond last row
    if (rows[1] == -2) {
        var totalRows = this.getTotalRows();
        if (totalRows == 0 || rows[0] < 0) {
            // empty data, or some other condition that caused getEventRow() to return
            // something invalid
            rows[0] = -1;
            rows[1] = -1;
        } else {
            // return the *index of* the last row
            rows[1] = this.getTotalRows() - 1;
        }
    }
    return rows;
},

//>	@method	gridRenderer.getVisibleColumns()
// Get the currently visible columns, as an array of [leftColumnNum, rightColumnNum]
//<
getVisibleColumns : function () {
    
    var widths = this._fieldWidths;
    if (this.overflow == isc.Canvas.VISIBLE) return [0, widths.length-1];
    
    var scrollPos = this.getScrollLeft();

    if (this.isRTL()) {
        
        var maxScroll = this.getScrollWidth() - this.getInnerWidth(),
            scrollPos = maxScroll - scrollPos;
    }
 
    var firstCol = this.inWhichPosition(widths, scrollPos),
        lastCol = this.inWhichPosition(widths, scrollPos + this.getInnerWidth());

    //this.logWarn("scrollLeft: " + scrollPos + 
    //             ", firstCol: " + firstCol +
    //             ", lastCol: " + lastCol);

    if (lastCol == -2) lastCol = this._fieldWidths.length - 1;
    return [firstCol, lastCol];

    // maxScroll - scrollLeft
    // 0 - 0: 0, works with non-reversed traversal
    // max - max: 0, works
    // max - 0: 

    
},

//>	@method	gridRenderer.getDrawnRows()
// Get the rows that are currently drawn (exist in the DOM), as an array of [firstRowNum,
// lastRowNum].  
// <P>
// The drawn rows differ from the +link{getVisibleRows,visibleRows} because of
// +link{drawAheadRatio,drawAhead}.  The drawn rows are the appropriate range to consider if
// you need to, eg, using +link{refreshCell()} to update all the cells in a column.
// <P>
// If the grid is undrawn or the +link{emptyMessage} is currently shown, returns
// [null,null];
//
// @return (Array)
// @visibility external
//<
getDrawnRows : function () { 
    if (this.cacheDOM) return this.getVisibleRows();
    return [this._firstDrawnRow, this._lastDrawnRow] 
},

// Synthetic Row/Cell Events (over/out/hover/contextClick)
// --------------------------------------------------------------------------------------------

// shouldShowRollOver
// Should we show the "over" styling for this row when the mouse goes over it?
// By default this is always true if this.showRollOver is true.
shouldShowRollOver : function (rowNum, colNum) {
    // NOTE: colNum may be null.
    
    return (this.showRollOver && !this._rowAnimationInfo);
},

// called whenever the current row needs to be updated to reflect a change in the rollOver
// state.  This includes both the rollover *leaving* a row (rollover appearance needs to be
// cleared) and entering the row.
updateRollOver : function (rowNum, colNum) {
    this.setRowStyle(rowNum, null, (this.useCellRollOvers ? colNum : null));
},

// We fire synthetic events such as 'cellMouseOver' or 'rowMouseOver' on mouseOver of a cell.
// The handling for this is very similar for each event type - 
//  on X event, if cellX is defined fire it.  If rowX is defined fire that.
// (If both are defined, we fire both).

// NOTE: checking for valid (>=0) row and col coordinates is necessary because the table can be
// drawn smaller than the area of the containing GridRenderer Canvas (eg in the LV, with a small
// number of records), so the mouse can be within the GridRenderer Canvas without being over the
// table as such.

// NOTE: most of the pseudo events do not have default handlers - meaning they can be defined 
// without the developer having to call "Super".  The only case where we currently would
// require an override to call Super is if the developer overrides
// 'selectOnMouseDown(record,rowNum,colNum)' or 'selectOnMouseUp(record,rowNum,colNum)'.



// Override the (unexposed) startHover() method to be a no-op
// This is called by the EH directly to show hover on mouseOver - we're handling our hovers on
// mouseOver of specific cells etc.
startHover : function () { },

//>	@method	gridRenderer.mouseMove()	([A])
//		@group	events
//			Generate cell/row over/out events
//		@return	(boolean)	false if same cell/row as before
//<
mouseMove : function (arg1, arg2) {

    if (this._suppressEventHandling()) return;
    
    // check for keyboard-navigation-induced scrolls
    if (this._suppressNextMouseMove) {
        this._suppressNextMouseMove = false;
        return;
    }

	var rowNum = this.getEventRow(),
        colNum = this.getEventColumn();

    // If we're pending a redraw from scroll, and the mouse is currently over an
    // undrawn part of the table, suppress standard mouseMove handling.
    // We don't want to fetch data which is just being scrolled through, or attempt to
    // redraw to refresh styling, etc.
    // (This is most relevant to mouseWheel scrolling where the mouse is over the body
    // of the grid).
    
    var pendingScrollRedraw = this.pendingActionOnPause("scrollRedraw"),
        drawnRows = this.getDrawnRows();
    if (pendingScrollRedraw && drawnRows && 
        (rowNum < drawnRows[0] || rowNum > drawnRows[1])) 
    {
        return;
    }
        

    // On rollOver of cells we do a couple of things:
    // - highlight the cell by applying "Over" styling
    // - fire cell level events ("rowOver", "rowOut", and also hover events if appropriate).
    // The "Over" styling subsystem is also coopted at the ListGrid level for visual feedback with
    // keyboard navigation.
    // Track the current "Over" styled cell separately from the last "cellOver" event cell - this
    // is required to avoid an obscure bug where if a cell is highlighted with the "Over" style 
    // by keyboard navigation or focus, and then the user rolls over it, the appropriate
    // rowOver event never fires.
    
    var lastStyleRow = this.lastOverRow,
        lastMouseRow = this.lastMouseOverRow,
        lastStyleCol = this.lastOverCol,
        lastMouseCol = this.lastMouseOverCol;

    //this.logWarn("row: " + rowNum + ", col: " + colNum);
    var validRowCol = (rowNum >= 0 && colNum >= 0),
        notSameStyleRowCol = !(rowNum == lastStyleRow && colNum == lastStyleCol),
        notSameMouseRowCol = !(rowNum == lastMouseRow && colNum == lastMouseCol),

        
        requireRecord = validRowCol && (
            notSameStyleRowCol || notSameMouseRowCol || this.cellMove || this.rowMove),
        record = requireRecord && this.getCellRecord(rowNum, colNum),

        hasNewCell = (validRowCol && requireRecord && this.cellIsEnabled(rowNum, colNum, record));

    // Styling:
    if (notSameStyleRowCol) {

        // If we were showing "over" styling for another row/cell, clear it.
        
        if (lastStyleRow != null && lastStyleCol != null) {
            this.lastOverRow = null;
            this.lastOverCol = null;

            // If we're not over a valid column (we're too far to the right of the listGrid)
            // consider this a row change, for the purposes of restyling correctly
            if (rowNum != lastStyleRow || colNum < 0 || this.useCellRollOvers) {
            
                
                this.updateRollOver(lastStyleRow, lastStyleCol, hasNewCell);
            }
        }
        
        // And show the over style for the new cell:
        if (hasNewCell) {
        
            this.lastOverRow = rowNum;
            this.lastOverCol = colNum;
            
            if (lastStyleRow != rowNum || this.useCellRollOvers) {
                // show rollover hiliting
                if (this.shouldShowRollOver(rowNum, colNum)) {
                    this.updateRollOver(rowNum, colNum);
                }
            }
        }
    }

    // handle situations when we return cursor from inner components to a cell of current grid
    var returnedBackToCurrentGrid = this._lastTarget != arg1.target && (arg1.target == this);
    this._lastTarget = arg1.target;

    // Events (including hovers):
    if (notSameMouseRowCol || returnedBackToCurrentGrid) {

        if (lastMouseRow != null && lastMouseCol != null) {
        
            this.lastMouseOverRow = null;
            this.lastMouseOverCol = null;

            // Once again - if new colNum < 0, we are to the right of the rightmost column, so
            // call stopHover even if we're hovering by row.
            if ((rowNum != lastMouseRow || colNum < 0 || this.hoverByCell) &&
                this.getCanHover() && !this.keepHoverActive)
            {
                this.stopHover();
            }

            var lastMouseRecord = this.getCellRecord(lastMouseRow, lastMouseCol);

            // support field.cellOut, cell.cellOut?
            if (this.cellOut) {
                this.cellOut(lastMouseRecord, lastMouseRow, lastMouseCol);
            }
            if (rowNum != lastMouseRow && this.rowOut) {
                this.rowOut(lastMouseRecord, lastMouseRow, lastMouseCol);
            }
        }

        if (hasNewCell) {

            this.lastMouseOverRow = rowNum;
            this.lastMouseOverCol = colNum;

            if (rowNum != lastMouseRow || this.hoverByCell) {
                // set hover action
                if (validRowCol && this.getCanHover()) {
                    var hoverDelay = this.getCellHoverDelay(rowNum, colNum);
                    isc.Hover.setAction(this, this._cellHover, [rowNum, colNum], hoverDelay);

                }
            }

            // support field.cellOver, cell.cellOver?
            if (this.cellOver) {
                this.cellOver(record, rowNum, colNum);
            }

            if (rowNum != lastMouseRow && this.rowOver) {
                this.rowOver(record, rowNum, colNum);
            }
        }
    }

    if (validRowCol) {
        // cellMove / rowMove 
        // Not currently exposed - used internally to update hovers for validation error icons
        // in ListGrid.
        if (this.cellMove) {
            this.cellMove(record, rowNum, colNum);
        }
        if (this.rowMove) {
            this.rowMove(record, rowNum, colNum);
        }
    }
    
    // If this mouseMove was tripped by the user scrolling (see Canvas.scrolled),
    // and we're pending a scroll-redraw, reset the scroll redraw timer now
    
    if (pendingScrollRedraw && this._firingSyntheticMouseMove) {
        var delay = this.getScrollRedrawDelay(this._pendingScrollRedrawFromWheel);
        this.fireOnPause("scrollRedraw", 
            this._getRedrawOnScrollCallback(), 
            delay);
    }
},

// Support suppressing mouse/keyboard event handling at certain times.
_suppressEventHandling : function () {
    //>Animation
    // If we're in the process of animate-resizing a row just suppress all event handling!
    if (this._rowAnimationInfo != null) return true;
    //<Animation
    return false;
},

//>	@method	gridRenderer.mouseOut()	([A])
//		@group	events
//			call _cellOut or _rowOut if appropriate
//		@return	(boolean)	false if no hiliting
//<
mouseOut : function () {
    // Don't suppress mouseOut handling even if this._suppressEventHandling returns true
    // - we don't want the list stuck in an "over" state
    
    // if the mouseOut occurred by the mouse going over a child of an embedded component, don't
    // hide rollover / fire mouseOut methods.
    var target = isc.EH.getTarget();
    if (this._embeddedComponents) {
        var components = this._embeddedComponents;
        for (var i = 0; i < components.length; i++) {
            if (!components[i]) continue;
            if (components[i].contains(target, true)) {
                return;
            }
        }
    }
    // Note that we'll still get a bubbled mouseout when the user rolls out of the embedded
    // component so we won't get stuck in an 'over' state. 
    
    // If the target == this, we're still over this widget.
    // This can happen if we're starting to drag - in this case continue as with any other 
    // mouseOut (killing the hover is technically unnecessary as in this case as
    // EH.handleDragStart() always calls Hover.clear(), but we also want to clear up over-styling
    // etc.)
    // It can also happen if we're in the process of being masked by the clickMask
    // which explicitly calls 'mouseOut' on the target, allowing us to clean up state.
    //
    // Otherwise this event was bubbled from the user rolling off an embedded component back
    // into the body and we can ignore it.    
    if (target == this && !isc.EH.getDragTarget() && !isc.EH._showingClickMask) {
        return;
    }
    
	// clear any hover timer/window
	if (this.getCanHover()) this.stopHover();
    // if we were previously over a valid cell, reset the style for that cell and fire
    // cellOut / rowOut
    
    if (this.lastOverRow != null && this.lastOverCol != null) {
    
        var lastOverRow = this.lastOverRow,
            lastOverCol = this.lastOverCol;
            
        this.lastOverRow = null;
        this.lastOverCol = null;

    	// clear rollover hiliting
	    if (this.shouldShowRollOver(lastOverRow, lastOverCol)) {
		    this.updateRollOver(lastOverRow, lastOverCol);
        }
    }
    
    if (this.lastMouseOverRow != null && this.lastMouseOverCol != null) {

        var lastOverRow = this.lastMouseOverRow,
            lastOverCol = this.lastMouseOverCol,
            lastOverRecord = this.getCellRecord(lastOverRow, lastOverCol);
        
        this.lastMouseOverRow = null;
        this.lastMouseOverCol = null;
        
    	// support field.cellOut, cell.cellOut?
	    if (this.cellOut) {
    		this.cellOut(lastOverRecord, lastOverRow, lastOverCol);
	    }
        if (this.rowOut) {
            this.rowOut(lastOverRecord, lastOverRow, lastOverCol);
        }
    }
        
},


_getShowClippedValuesOnHover : function () {
    return this.showClippedValuesOnHover;
},

getCellHoverDelay : function (rowNum, colNum) {
    return this.hoverDelay;
},

// support field.cellHover, cell.cellHover, field.showHover, cell.showHover?
_cellHover : function (rowNum, colNum) {
    //!DONTCOMBINE
	var record = this.getCellRecord(rowNum, colNum);
	
	if (!this.shouldFireCellHover(record, rowNum, colNum)) return;

	// call user-defined handler and bail (don't show hover window) if it returns false
    var returnVal,
        cellValueIsClipped = this.cellValueIsClipped(rowNum, colNum);
    if (this._getShowClippedValuesOnHover() && cellValueIsClipped &&
        this.cellValueHover && this.cellValueHover(record, rowNum, colNum) == false)
    {
        returnVal = false;
    }
	if (this.cellHover && this.cellHover(record, rowNum, colNum) == false) returnVal = false;
    if (this.rowHover && this.rowHover(record, rowNum, colNum) == false) returnVal = false;
    
    if (returnVal == false) return;

	// show hover window if enabled
	if (this.showHover) this._showHover(record, rowNum, colNum, cellValueIsClipped);
},

// If we get a cellHover timer event over what appears to be an invalid cell (no record),
// don't fire the notification methods.

shouldFireCellHover : function (record, rowNum, colNum) {
    return record != null;
},

defaultCellValueHoverHTML : function (record, rowNum, colNum) {
    return this._getCellValue(record, rowNum, colNum);
},

_showHover : function (record, rowNum, colNum, cellValueIsClipped) {
    var properties = this._getHoverProperties();
    var content = this._getCellHoverComponent(record, rowNum, colNum);
    if (!content) {
        // Prefer the standard cell hover if customized.
        var useStandardCellHover = false,
            isCellHoverSuppressed = false;
        if (this.cellHoverHTML._isPassthroughMethod && this.grid) {
            var grid = this.grid,
                gridCol = grid.getFieldNumFromLocal(colNum, this),
                field = grid.getField(gridCol);
            useStandardCellHover = (grid.canHover ||
                                    (field.showHover && grid.canHover != false) ||
                                    grid.cellHoverHTML != grid.getClass().getInstanceProperty("cellHoverHTML") ||
                                    (grid.canHover == null && field.showHover)) &&
                                   field.showHover != false;
            isCellHoverSuppressed = grid._isCellHoverSuppressed(rowNum, gridCol);
        }

        if (!isCellHoverSuppressed) {
            if (!useStandardCellHover) {
                if (this._getShowClippedValuesOnHover() && cellValueIsClipped) {
                    content = this.cellValueHoverHTML(record, rowNum, colNum, this.defaultCellValueHoverHTML(record, rowNum, colNum));
                }
            } else {
                content = this.cellHoverHTML(record, rowNum, colNum);
            }
        }
    }
    isc.Hover.show(content,
				   properties,
                   this.cellHoverBoundary(rowNum, colNum),
                   this.getHoverTarget());
},

_getCellHoverComponent : function (record, rowNum, colNum) {
},

// getHoverTarget() - returns the 'targetCanvas' passed to Hover.show() in _showHover()
// This allows the developer to call 'updateHover()' on that canvas to change the hover content HTML
// override in LG bodies to point to the ListGrid
getHoverTarget : function () {
    return this;
},

cellHoverHTML : function (record, rowNum, colNum) {
	return null;
},

cellValueHoverHTML : function (record, rowNum, colNum, defaultHTML) {
    return null;
},

getCellHoverComponent : function (record, rowNum, colNum) {
	return null;
},


cellHoverBoundary : function (rowNum, colNum) {
	
	return null;
    
},

// generate cell/row contextClick events
showContextMenu : function () {
    if (this._suppressEventHandling()) return false;
    
	var rowNum = this.getEventRow(),
		colNum = this.getEventColumn();
		
	// If this came from a keyboard event, use the keyboard focus row / col
	var keyboardEvent = isc.EH.isKeyEvent();
	if (keyboardEvent) {
	    rowNum = this.getFocusRow(),
	    colNum = this.getFocusCol();
	}

    var validRowCol = (rowNum >= 0 && colNum >= 0),
        record = validRowCol && this.getCellRecord(rowNum, colNum);
    if (validRowCol && this.cellIsEnabled(rowNum, colNum, record)) {
        var returnVal;
        if (this.cellContextClick) 
            if (this.cellContextClick(record, rowNum, colNum) == false) returnVal = false;

        if (this.rowContextClick) 
            if (this.rowContextClick(record, rowNum, colNum) == false) returnVal = false;

        // Legacy:
        if (this.recordContextClick)
            if (this.recordContextClick(record, rowNum, colNum) == false) returnVal = false;            
        
    	if (returnVal == false) return false;
    }

    return this.Super("showContextMenu");
},




// Selection
// --------------------------------------------------------------------------------------------

setSelection : function (selection) { 
    this.selection = selection;

    // update cell/row styling on selection change
    if (this.selection.isA("CellSelection")) {
        this.observe(this.selection, "selectionChanged", function () {
            this._cellSelectionChanged(this.selection.changedCells);
        });
    } else {
        this.observe(this.selection, "setSelected", function () {
            this._setSelectedObservation(this.selection);
        });
    }
},

clearSelection : function () {
    if (this.selection) {
        if (this.isObserving(this.selection, "selectionChanged")) 
            this.ignore(this.selection, "selectionChanged");
        if (this.isObserving(this.selection, "setSelected")) 
            this.ignore(this.selection, "setSelected");
        
        delete this.selection;
    }
},

_cellSelectionChanged : function (cellList) {
    //this.logWarn("cellSelectionChanged with list: " + this.echoFull(cellList));
	// call user-defined handler and bail (don't hilite cells) if it returns false
	if (this.cellSelectionChanged) {
		if (this.cellSelectionChanged(cellList) == false) return false;
	}
	// refresh the affected cells to visually indicate selection
	this.refreshCellStyles(cellList);
},

// setSelected was fired on the selection object.
// If the selection actually changed call _rowSelectionChanged to fire notification methods and
// refresh the row in question.
// setSelected may be called without changing selection. This is actually done internally
// to manage cascading selection sync.
_setSelectedObservation : function (selection) {
    var changed = false;
    

    // If the state changed, we need to refresh
    if (!!selection.lastSelectionPreviousState != !!selection.lastSelectionState) {
        changed = true;
    // We show partial selection in some cases in tree-grids.
    // If the partial selected state changed, we also need to refresh
    } else if (selection.lastSelectionState && 
                (!!selection.lastSelectionPartialValue != !!selection.lastSelectionPreviousPartialValue))
    {
        changed = true;
    }

    
    if (changed) {
        this._rowSelectionChanged(
            selection.lastSelectionItem,
            !!selection.lastSelectionState,
            selection.cascadeSyncOnly
        );
    }
},

_rowSelectionChanged : function (record, state, cascadeSyncOnly) {
    if (!cascadeSyncOnly) {
    	// call user-defined handler and bail (don't hilite rows) if it returns false.
    	
	    if (this.handleSelectionChanged(record, state) == false) {
	        return false;
        }
                
    }

    if (this.selection._selectingList) {
        this.markForRowSelectionRefresh();
        return;
    }

	// refresh the affected records to visually indicate selection
    
    var selection = this.selection,
        lastItem = selection.lastSelectionItem,
        rowNum = selection.data.indexOf(lastItem,
                                        this._firstDrawnRow,
                                        this._lastDrawnRow);
    if (rowNum == -1) rowNum = selection.data.indexOf(lastItem);
    
    if (rowNum == -1) return;
    this.updateRowSelection(rowNum);
},


markForRowSelectionRefresh : function () {
    this.markForRedraw("Selection changed");
},

handleSelectionChanged : function (record,state) {
    if (this.selectionChanged) return this.selectionChanged(record,state);
},

updateRowSelection : function (rowNum) {
    this.setRowStyle(rowNum);    
},

// Catch-all redraw to indicate selection has updated

redrawForSelectionChanged : function () {
    this.markForRedraw("selection changed");
},

selectionEnabled : function () {
    return this.selection != null;
},

canSelectRecord : function (record) {
    return (record != null && record[this.recordCanSelectProperty] !== false);
},

//>	@method	gridRenderer.mouseDown()	([A])
//		@group	events, selection
//			handle a mouseDown event
//		@return	(boolean)	false if record is disabled
//<
mouseDown : function () {
    if (this._suppressEventHandling()) return;

    var rowNum = this.getEventRow(),
        colNum = this.getEventColumn();

    // not over a cell - just bail
    if (!(rowNum >= 0 && colNum >= 0)) return;

    var record = this.getCellRecord(rowNum, colNum);

    // if the record is explicitly disabled, kill the event
    if (!this.cellIsEnabled(rowNum, colNum, record)) return false;

    // hang onto the rowNum / colNum to see if we get a click over the same cell
    this._mouseDownRow = rowNum;
    this._mouseDownCol = colNum;

    this._scrolledSinceMouseDown = false;

    // remember the location of the click too.
    
    this._mouseDownX = isc.EH.getX();
    this._mouseDownY = isc.EH.getY();

	// call user-defined synthetic mouseDown event handler
    if (!isc.EH.rightButtonDown()) { 
        return this._cellMouseDown(record, rowNum, colNum);
	} else {
        return this._cellRightMouseDown(record, rowNum, colNum);
    }

},


rightMouseDown : function () { 
    // required to handle remembering which record the mouse went down over. Also fires 
    // _cellRightMouseDown()
    return this.mouseDown(); 
},

_cellMouseDown : function (record, rowNum, colNum) {

    var returnVal;

    if (this.cellMouseDown && (this.cellMouseDown(record, rowNum, colNum) == false)) returnVal = false;
    if (this.rowMouseDown && (this.rowMouseDown(record, rowNum, colNum) == false)) returnVal = false;
    // legacy
    if (this.recordMouseDown && this.recordMouseDown(rowNum, colNum) == false) returnVal = false;

    if (returnVal == false) return false;

    // perform selection
    this.selectOnMouseDown(record, rowNum, colNum);
}, 


selectOnMouseDown : function (record, rowNum, colNum, keyboardGenerated) {

    if (!this.selectionEnabled()) return true;

    //this.logWarn("mouseDown at: " + [rowNum, colNum]);
    
    if (rowNum >= 0 && colNum >= 0 && this.canSelectRecord(record) &&
        !this._shouldSelectOnMouseUp())
    {
        this._updateSelectionOnMouseUp = true;
        var selectionChanged = this.selection.selectOnMouseDown(this, rowNum, colNum);
        if (selectionChanged && this.fireSelectionUpdated && isc.isA.Function(this.fireSelectionUpdated)) {
            this.fireSelectionUpdated();
        }
    }

},

_shouldSelectOnMouseUp : function () {
    var EH = this.ns.EH;
    if (EH.dragTarget != null && EH.dragOperation == EH.DRAG_SCROLL ||
        this._usingNativeTouchScrolling())
    {
        return true;
    }
    return false;
},

_cellRightMouseDown : function (record, rowNum, colNum) {
    // currently there are no cell / row level rightMouseDown handlers, but this is where we would
    // call them if there were.

    // perform right-mouse style selection
    if (this.canSelectOnRightMouse) this.selectOnRightMouseDown(record, rowNum, colNum);
},

// We override Canvas._scrolled() here to prevent keyboard-navigation-triggered scrolling from
// firing a synthetic mouse event which can corrupt the tracking of the navigation location.
// If navigation is not in progress, we simply call the parent (Canvas) method.
_scrolled : function (deltaX, deltaY) {
    this._scrolledSinceMouseDown = true;
    if (this.grid && this.grid._handlingKeyboardNavigation) {
        // some browsers trigger extra native mouse move events; suppress them
        if (isc.Browser.nativeMouseMoveOnCanvasScroll) {
            this._suppressNextMouseMove = true;
            isc.Page.setEvent(isc.EH.MOUSE_MOVE, this, "once", "_suppressMouseMove");
        }
        this._fireParentScrolled(this, deltaX, deltaY);    
        if (this.scrolled) this.scrolled(deltaX, deltaY);
    } else {
        this.Super("_scrolled", arguments);        
    }
},

// Do nothing unless the mouse move event is not the one that we expect to suppress;
// in that case, clear the flag so it gets handled normally like any other event
_suppressMouseMove : function () {
    var target = isc.EH.lastEvent.target;
    if (target != this) this._suppressNextMouseMove = false;
},

// Default implementation is just to do 'selectOnMouseDown' - override if you want something else.
selectOnRightMouseDown : function (record, rowNum, colNum) {
    this.selectOnMouseDown(record, rowNum, colNum);
},

//>	@method	gridRenderer.mouseUp()	([A])
//		@group	events,	selection
//			handle a mouseUp event
//		@return	(boolean)	false if no hiliting; true otherwise
//<
mouseUp : function () {

    if (this._suppressEventHandling()) return;

    var rowNum = this.getEventRow(),
        colNum = this.getEventColumn();

    // not over a cell - just bail
    if (!(rowNum >=0 && colNum >=0)) return;        

    var record = this.getCellRecord(rowNum, colNum);

    // if the record is explicitly disabled, just return
    if (!this.cellIsEnabled(rowNum, colNum, record)) return;

	// call user-defined cell / row level mouseUp handler
    var returnVal;

    if (this.cellMouseUp && (this.cellMouseUp(record, rowNum, colNum) == false)) returnVal = false;
    if (this.rowMouseUp && (this.rowMouseUp(record, rowNum, colNum) == false)) returnVal = false;
    // legacy
    if (this.recordMouseUp && this.recordMouseUp(rowNum, colNum) == false) returnVal = false;

    if (returnVal == false) return returnVal;

    this.selectOnMouseUp(record, rowNum, colNum);
},

selectOnMouseUp : function (record, rowNum, colNum, keyboardGenerated) {
    if (!this.selectionEnabled()) return true;

    if (rowNum >= 0 && colNum >= 0 && this.canSelectRecord(record) &&
        (keyboardGenerated ||
         (this._mouseDownRow == rowNum && this._mouseDownCol == colNum && !this._scrolledSinceMouseDown)))
    {
        var selectionChanged = false;
        // If we didn't select on mouseDown, fire both 'selectOnMouseDown' and 'selectOnMouseUp'
        // to update the selection
        
        if (this._shouldSelectOnMouseUp()) {
            this._updateSelectionOnMouseUp = true;
            selectionChanged = this.selection.selectOnMouseDown(this, rowNum, colNum);
        }

        if (this.grid) this.grid._dontRefreshSelection = true;
        if (this.selection.selectOnMouseUp(this, rowNum, colNum)) selectionChanged = true;
        if (this.grid) this.grid._dontRefreshSelection = null;

        // only refresh if the selection actually changed - otherwise we'll end up redrawing
        // on every click on a selected row, etc.
        if (selectionChanged) {
            this.redrawForSelectionChanged();
            if (this._updateSelectionOnMouseUp) {
                if (this.fireSelectionUpdated && isc.isA.Function(this.fireSelectionUpdated)) {
                    this.fireSelectionUpdated();
                }
                if (this.grid.getCurrentCheckboxField() != null) {
                    this.grid.updateCheckboxHeaderState();
                }
                this._updateSelectionOnMouseUp = null;
            }
        }
    }

},

//>	@method	gridRenderer.click()  ([A])
//		@group	events
//			handle a click event
//          fires cell or row level click handler
//
//		@return	(boolean)	false if the event was cancelled by some handler
//<
click : function () {
    if (this._suppressEventHandling()) return;
    
    var rowNum = this.getEventRow(),
        colNum = this.getEventColumn();
    return this._rowClick(rowNum, colNum);
},

// _rowClick - fire rowClick and cellClick handlers
_rowClick : function (rowNum, colNum) {
    // Clear out old _clickRow/ _clickCol, which are now out of date.
    // [These will be set to meaningful values if the event occurred over a valid cell].
    this._clickRow = this._clickCol = null;

    var mdR = this._mouseDownRow;

    // if the click occurred over a different record from the previous mousedown, just bail
    
    if (mdR != null && rowNum != mdR) {
        if (isc.EH.getX() == this._mouseDownX) {
            rowNum = this._mouseDownRow;
        } else {
            // Don't return false - we don't want to suppress click from bubbling.
            return;
        }
    }
    if (isc.EH.getY() == this._mouseDownY && this._mouseDownCol != null) {
        colNum = this._mouseDownCol;
    }
    
    // no record - just bail
	if (!(rowNum >=0 && colNum >=0)) return;

    var record = this.getCellRecord(rowNum, colNum);

    // if the record is explicitly disabled, return false to kill doubleClick etc
    if (!this.cellIsEnabled(rowNum, colNum, record)) return false;

    // record the click cell details for double-click events to check
    this._clickRow = rowNum;

    var returnVal;
    // only fire cellClick if it was on the same column as well as the same row
    if (!this._cellClick(record, rowNum, colNum)) returnVal = false;
    if (this.rowClick && (this.rowClick(record, rowNum, colNum) == false)) 
        returnVal = false;

    // clear out the old mouseDown row
    // Note - this method is fired _after_ mouseUp, so we can't clear these values out there.
    this._mouseDownRow = null;

    return returnVal;

},

// _cellClick - fire cellClick handlers on the specified row/col
_cellClick : function (record, rowNum, colNum) {

    
    // Assertion - this method is only called when we have already verified that the click 
    // occurred over the same row as the last mousedown
    if (this._mouseDownCol != colNum) {
        // Clearing out this._clickCol avoids the possibility of the doubleClick handler firing
        // over this cell which never received a first click.
        this._clickCol = null;
        return;
    }
    // update _clickCol so double clicks can determine whether they occurred over the 
    // same column as the previous click.
    this._clickCol = colNum;
    
    this._mouseDowncol = null;
    return !(this.cellClick && (this.cellClick(record, rowNum, colNum) == false));
},

//>	@method	gridRenderer.doubleClick()  ([A])
//		@group	events
//			handle a doubleClick event
//          fires cell or row level doubleClick handler
//
//		@return	(boolean)	false if the event was cancelled by some handler
//<
doubleClick : function () {
    if (this._suppressEventHandling()) return;

    var rowNum = this.getEventRow(),
        colNum = this.getEventColumn();

    // no record - just bail
	if (!(rowNum >= 0 && colNum >= 0)) return;

    var record = this.getCellRecord(rowNum, colNum);

    // if the record is explicitly disabled, kill the event
    if (!this.cellIsEnabled(rowNum, colNum, record)) return false;

    // If the double click occurred over a different row from the previous click, fire 
    // rowClick / cellClick over the new row.
    if (rowNum != this._clickRow) {
        return this._rowClick(rowNum, colNum);
    }
    
	// call user-defined cell / row level click and mouseUp handler
    var handlerReturn;
    
    // the click occurred over a different col from the last click, fire a single click on
    // that cell (but not that row)
    // NOTE: this means if the user double clicks within a row, but the clicks land on different
    // columns we'll get a single click on each cell, and a double click on the row.
    if (colNum != this._clickCol) {
        handlerReturn = this._cellClick(record, rowNum, colNum);

    // otherwise fire a double click handler on the cell
    } else if (this.cellDoubleClick && (this.cellDoubleClick(record, rowNum, colNum) == false)) 
    {
       handlerReturn = false;
    }

    if (this.rowDoubleClick  && (this.rowDoubleClick(record, rowNum, colNum) == false)) 
        handlerReturn = false;
    
    // clear out the temp vars -- we don't want to fire a click on mouseUp after this doubleclick
    // (this is fired before mouseUp) and no need to hang onto the clickRow / col.
    this._mouseDownRow = this._mouseDownCol = null;    
    this._clickRow = this._clickCol = null;
    
    if (handlerReturn == false) return false;
            
},

dragStart : function () {
    var EH = isc.EH,
        event = EH.lastEvent;
    // Try to normalize the appearance of the default native drag tracker image. In browsers
    // that support the EH.setDragTrackerImage() API, this will set the native drag tracker
    // image to the table row wherever the user starts the drag (e.g. if the user starts
    // the drag on an icon, then the browser will use the icon as the drag tracker image).
    
    if (event.target == this &&
        event.eventType == "dragStart")
    {
        var dt;
        try {
            dt = event.DOMevent.dataTransfer;
        } catch (e) {
            
            dt = null;
        }

        if (dt != null && dt.setDragImage != null) {
            var rowNum = this.getNearestRowToEvent(),
                rowElement = this.getTableElement(rowNum);
            if (rowElement != null) {
                var offsets = isc.Element.getOffsets(rowElement),
                    x = EH.getX(),
                    y = EH.getY();
                dt.setDragImage(rowElement, x - offsets.left, y - offsets.top);
            }
        }
    }
},

//>	@method	gridRenderer.dragMove()	([A])
//		@group	events, dragging
//			drag move event
//		@return	(boolean)
//<
// XXX
// We may want to add handling for row and cell level rowDragMove() and cellDragMove() handlers.
// If we do this we would also add row and cell level dragStart / dragStop / dropMove / drop, etc.
// - Default (internal) implementation would handle dragSelection if this.canDragSelect
//   Not worrying about this for now.
dragMove : function () {
    
    if (this._suppressEventHandling() || !this.selectionEnabled() || !this.canDragSelect) 
        return true;
    
    var rowNum = this.getNearestRowToEvent(),
        colNum = this.getNearestColToEvent();

    //this.logWarn("selectOnDragMove: " + [rowNum, colNum]);
    this.selection.selectOnDragMove(this, rowNum, colNum);
},

dragStop : function () {
    this.fireSelectionUpdated();
},

// Override Drag/drop snap-to-grid functionality from Canvas

// suppress drag offset when snap dragging to cells.
noSnapDragOffset : function (dragTarget) {
    return this.snapToCells;
},
getHSnapPosition : function (localCoordinate, dir) {
    if ( ! this.snapToCells) {
        return this.Super("getHSnapPosition", arguments);
    }
    var EH = this.ns.EH,
        direction = dir || this.snapHDirection,
        col = this.snapHGap ? Math.floor(localCoordinate / this.snapHGap) : this.getEventColumn(localCoordinate),
        beforeLeft = this.snapHGap ? (col * this.snapHGap) : this.getColumnLeft(col),
        beforeRight = this.snapHGap ? beforeLeft + this.snapHGap : this.getColumnLeft(col) + this.getColumnSize(col),
        afterCol = this.snapHGap ? col + 1 : this.getEventColumn(beforeRight + 1),
        afterLeft;
        
    if (afterCol >= 0 ) {
        afterLeft = this.snapHGap ? afterCol * this.snapHGap : this.getColumnLeft(afterCol);
    } else {
        afterLeft = beforeLeft;
    }
    var halfway = beforeLeft + (this.snapHGap ? this.snapHGap : this.getColumnSize(col)) / 2;

    // Fix up for cell borders if necessary
    if (this.snapInsideBorder) {
        var lb = isc.Element._getLeftBorderSize(this.baseStyle)
        var rb = isc.Element._getRightBorderSize(this.baseStyle)
        beforeLeft += lb;
        beforeRight -= rb;
        afterLeft += lb;
    }
    
    // For resize, always extend the drag-target to cover the current "over" cell
    if (EH.dragOperation == EH.DRAG_RESIZE) {
        var goingLeft = isc.EH.resizeEdge.contains("L");
        return goingLeft ? beforeLeft : beforeRight;
    } else {
        if (direction == isc.Canvas.BEFORE) {
            return beforeLeft;
        } else if (direction == isc.Canvas.AFTER) {
            return afterLeft;
        } else {
            // If we're exactly inbetween, go left
            if (localCoordinate <= halfway) {
                return beforeLeft;
            } else {
                return afterLeft;
            }
        }
    }

},

getVSnapPosition : function (localCoordinate, dir) {

    if ( ! this.snapToCells) {
        return this.Super("getVSnapPosition", arguments);
    }

    // this almost works...repositioning gets thrown off when moving up. May be worth exploring
    // at some point
    //if (this.snapVGap) {
    //    return this.Super("getVSnapPosition", localCoordinate, dir) + gridInnerPageTop;     
    //}
    var EH = this.ns.EH,
        direction = dir || this.snapVDirection,
        // for snapVGap, row is just a snapVGap sized chunk of space
        row = this.snapVGap ? Math.floor(localCoordinate / this.snapVGap) : this.getEventRow(localCoordinate),
        // top coordinate of row
        beforeTop = this.snapVGap ? (row * this.snapVGap) : this.getRowTop(row),
        // bottom coordinate of row
        beforeBot = this.snapVGap ? beforeTop + this.snapVGap : this.getRowTop(row) + this.getRowSize(row),
        afterRow = this.snapVGap ? row + 1 : this.getEventRow(beforeBot + 1),
        afterTop;
    if (afterRow >= 0 ) {
        afterTop = this.snapVGap ? afterRow * this.snapVGap : this.getRowTop(afterRow);
    } else {
        afterTop = beforeTop;
    }
    var halfway = beforeTop + (this.snapVGap ? this.snapVGap : this.getRowSize(row)) / 2;
    
    
    // Fix up for borders if necessary
    if (this.snapInsideBorder) {
        var tb = isc.Element._getTopBorderSize(this.baseStyle)
        var bb = isc.Element._getBottomBorderSize(this.baseStyle)
        //this.logWarn("tb: " + tb + ", bb: " + bb);
        beforeTop += tb;
        beforeBot -= bb;
        afterTop += tb;
    }
    
    
    if (EH.dragOperation == EH.DRAG_RESIZE) {
       var goingUp = isc.EH.resizeEdge.contains("T");
       return goingUp ? beforeTop : beforeBot;
    } else {
        if (direction == isc.Canvas.BEFORE) {
            return beforeTop;
        } else if (direction == isc.Canvas.AFTER) {
            return afterTop;
        } else {
            // If we're exactly inbetween, go up
            if (localCoordinate <= halfway) return beforeTop;
            else return afterTop;
        }
    }
},

// AutoSizing
// --------------------------------------------------------------------------------------------

//>	@method	gridRenderer.getColumnAutoSize()	([A])
//	    @group	sizing, positioning
//      Get the size this column needs to be in order to accommodate it's contents.
//
//      Can only be called after draw()
//
//      NOTE: if using partial table rendering (showAllRows:false), this is the size for the
//      currently visible contents of the column
//<
getColumnAutoSize : function (columnNum, startRow, endRow) {
    if (this.isEmpty()) {
        return null;
    }
    // create an offscreen Canvas to do sizing in
    var columnSizer = this._columnSizer = this._columnSizer || isc.Canvas.create({
                               // flag to simplify debugging!
                               columnSizer:true,
                               top:-1000,
                               width:1, height:1,
                               overflow:"hidden",
                               autoDraw:false,
                               _generated:true
                           });
    
    // get HTML for a table containing only this column, written without column widths and 
    // with no text wrapping
    var autoFit = this.autoFit,
        wrapCells = this.wrapCells;
    
    this.autoFit = true;
    this.wrapCells = false;
    
    // pass in startRow / endRow
    // If not explicitly specified, just use the current draw area
    // Passing this parameter in avoids us writing a (unnecessary in this case) spacer
    // above / below the cell values and will turn on the "fragment" logic in getTableHTML()
    // which avoids writing out DOM IDs on the various parts.
    if (startRow == null || endRow == null) {
        var drawRect = this.getDrawArea();
        startRow = drawRect[0];
        // remember drawRect is inclusive, we want exclusive
        endRow = drawRect[1]+1;
    }
    
//    this.logWarn("Logic to get col autoSize running:" + [columnNum, startRow, endRow]);
    // set a flag so we can write out different HTML for the sizer if necessary - such
    // as using inactive HTML for edit items in a listGrid
    this._gettingAutoSizeHTML = true;
    columnSizer.setContents(this.getTableHTML(columnNum,startRow,endRow, true));

    delete this._gettingAutoSizeHTML;

    this.autoFit = autoFit;
    this.wrapCells = wrapCells;

    // draw the table and figure out how large it is
    if (!columnSizer.isDrawn()) {
        columnSizer.draw();
    } else {
        if (columnSizer.isDirty()) columnSizer.redraw();
        if (columnSizer._delayedAdjustOverflow) columnSizer.adjustOverflow("Check autoFit column sizing");
    }

    var returnVal;
    if (isc.isA.Array(columnNum)) {
        // We're going to have to reach into the table.
        var table,
            nodes = columnSizer.getHandle().childNodes;
        for (var i = 0; i < nodes.length; i++) {
            if (nodes[i].tagName.toLowerCase() == "table") {
                table = nodes[i];
                break;
            }
        }
        var rowNum = this._getValidAutoFitRowNum();
        if (table && table.rows[rowNum]) {
            var firstRow = table.rows[rowNum],
                cells = firstRow.childNodes,
                numCells = cells.length;
            returnVal = [];

            
            if (isc.Browser.isMoz && isc.Browser.hasTextOverflowEllipsis) {
                for (var i = 0; i < numCells; ++i) {
                    returnVal[i] = Math.ceil(cells[i].getBoundingClientRect().width);
                }

            } else if (isc.Browser.isIE10) {
                var doc = firstRow.ownerDocument;
                
                var origMsCSSOMElementFloatMetrics = doc.msCSSOMElementFloatMetrics;
                doc.msCSSOMElementFloatMetrics = true;
                for (var i = 0; i < numCells; ++i) {
                    returnVal[i] = Math.ceil(cells[i].offsetWidth);
                }
                doc.msCSSOMElementFloatMetrics = origMsCSSOMElementFloatMetrics;

            } else {
                for (var i = 0; i < numCells; ++i) {
                    returnVal[i] = cells[i].offsetWidth;

                    if (isc.Browser.hasTextOverflowEllipsis) {
                        var c = Math.ceil(returnVal[i]);
                        
                        returnVal[i] = (returnVal[i] == c ? c + 1 : c);
                    }
                }
            }
        } 

    } else {
        returnVal = columnSizer.getScrollWidth();
    }

    // No need to immediately clear the columnSizer: It's drawn offscreen already and if we clear we'll have
    // to re-draw next time we size a column which can end up with a huge number of expensive
    // clear-draw's for a grid with lots of columns. Set up a timer to remove it after a period of time to
    // allow for other columns to make use of it in the meantime.

    var _this = this;
    this.fireOnPause("removeColumnSizer", 
            function () { _this.removeColumnSizer(); }, 
            this.removeColumnSizerDelay);

    return returnVal;
},

// Overridden in ListGrid to handle the case where we're grouped and so have some
// col-spanning cells in group header rows (which need to be skipped)
_getValidAutoFitRowNum : function () {
    return 0;
},


// Table Cache Clearing
// --------------------------------------------------------------------------------------------

// clear anything we've cached about the HTML table we draw
redraw : function (a,b,c,d) {
    this._resetEmbeddedComponents();
    this.invokeSuper(isc.GridRenderer, "redraw", a,b,c,d);
    // if we're redrawing in response to the end of 'fast scrolling', the suppresDrawAhead flag
    // will have been set in markForRedraw()
    // clear this now
    delete this._suppressDrawAheadDirection;
    
},

modifyContent : function () {
    
    // resize / place embedded components before 
    // - restoring virtual scrolling
    // - adjusting overflow    
    // Both of these need to know the drawn heights of rows (including E.C's)
    // If we're performing an animated show/hide of row, don't attempt to place embedded
    // components until it completes
    if (!this._animatedShowStartRow) this._placeEmbeddedComponents();

    if (this._targetRow != null) {
        
        this._scrollFromRedraw = true;   
        this._scrollToTargetRow("scrollToRow in modifyContent");
        this._scrollFromRedraw = null;

        // show the table element, which is drawn as hidden so we can scroll before we make it
        // visible, to prevent it showing the wrong scroll position briefly
		var tableElement = this.getTableElement();
		if (tableElement) tableElement.style.visibility = "inherit";

        
    }

    if (this._isVirtualScrolling) {
        // shrink the endSpacer, if any, to avoid scrolling when unnecessary.  
        var totalRowHeight = this._getDrawnRowHeights().sum();
        if (totalRowHeight < this.getViewportHeight()) {
            this._endRowSpacerHeight = 0;
            var spacer = isc.Element.get(this.getID() + "_endSpacer"),
                height = this._endRowSpacerHeight + (this.endSpace || 0);
            if (spacer) {
                
                if (height == 0) spacer.style.display = "none"
                else spacer.style.display = "";
                spacer.style.height = height + "px";
            }
            // overflow:hidden, so no need to rewrite div content
            //this.logWarn("shrank spacer: " + totalRowHeight);
        }

        
        var visibleRows = this.getVisibleRows(),
            numVisibleRows = Math.max(1, visibleRows[1] - visibleRows[0]),
            trueRatio = numVisibleRows/this.getTotalRows(),
            approxRatio = this.getViewportRatio(true);
        //this.logWarn("viewportHeight: " + this.getViewportHeight() + 
        //             ", visibleRows: " + visibleRows +
        //             ", approxRatio: " + approxRatio +
        //             ", trueRatio: " + trueRatio);
        if (isc.isA.Number(trueRatio) && 
            ((approxRatio == 1 && trueRatio < 1) ||
             approxRatio/trueRatio > 1.25) 
           ) 
        {   
            this._viewRatioRowHeight = Math.max(this.cellHeight,
                                         Math.round(this.getViewportHeight() / numVisibleRows));
            //this.logWarn("set average row height to: " + this._viewRatioRowHeight);
        }
    }

    /*
    // resize the bottom spacer to keep the scrollHeight constant
    var totalRows = this.getTotalRows(),
        oldScrollHeight = this._oldScrollHeight;
    // if we've got the same size dataset as before, and we're not drawing all the way to the
    // end
    if (this._lastTotalRows == totalRows && this._lastDrawnRow < totalRows-1) {
        var newScrollHeight = this.getScrollHeight(true),
            spacer = isc.Element.get(this.getID() + "_endSpacer"),
            spacerHeight = parseInt(spacer.offsetHeight),
            newSpacerHeight = spacerHeight + (oldScrollHeight-newScrollHeight); 
    
        this.logWarn("adding extraHeight.  oldScrollHeight: " + oldScrollHeight + 
                     ", lastDrawnRow: " + this._lastDrawnRow +
                     ", newScrollHeight: " + newScrollHeight +
                     ", spacerHeight: " + spacerHeight +
                     ", newSpacerHeight: " + newSpacerHeight);

        if (newSpacerHeight < 0) {
            this.logWarn("************** NOT ENOUGH ROOM to adjust spacer");
            newSpacerHeight = this.cellHeight;
        }
        spacer.style.height = newSpacerHeight + "px";
    }
    this._lastTotalRows = totalRows;
    */

 
},

 
// Helper to update the explicit space above / below the rows in the grid
setStartSpace : function (value) {
    if (!isc.isA.Number(value) || value == this.startSpace) return;
    var reduction = this.startSpace && this.startSpace > value;
    this.startSpace = value;
    if (!this.isDrawn()) return;
    var height = value + this._startRowSpacerHeight,
        spacer = isc.Element.get(this.getID() + "_topSpacer");
    if (spacer) {
        if (height == 0) spacer.style.display = "none";
        else spacer.style.display = ""; // default (== "inline")
        
         
        if (this._canResizeSpacerDivs) {
            spacer.style.height = height + "px";
        }
        // overflow:hidden so don't have to rewrite contents if we're shrinking
        if (!reduction || !this._canResizeSpacerDivs) {
            spacer.innerHTML = isc.Canvas.spacerHTML(1,height);
        }
        this._markForAdjustOverflow();
    } 
    // If there was no spacer we must be in cacheDOM mode where we don't currently
    // support startSpace / endSpace
    
},

setEndSpace : function (value) {
    if (!isc.isA.Number(value) || value == this.endSpace) return;
    var reduction = this.endSpace && this.endSpace > value;
    this.endSpace = value;
    if (!this.isDrawn()) return;
    var height = value + this._endRowSpacerHeight,
        spacer = isc.Element.get(this.getID() + "_endSpacer");
    if (spacer) {
        if (height == 0) spacer.style.display = "none";
        else spacer.style.display = ""; // default (== "inline")
        if (this._canResizeSpacerDivs) spacer.style.height = height + "px";
        if (!reduction || !this._canResizeSpacerDivs) {
            spacer.innerHTML = isc.Canvas.spacerHTML(1,height);
        }
        this._markForAdjustOverflow();
    }
    // If there was no spacer we must be in cacheDOM mode where we don't currently
    // support startSpace / endSpace
},

setLeftSpace : function (value) {
    if (this.leftSpace == value) return;
    this.leftSpace = value;
    this.redraw();    
},

setRightSpace : function (value) {
    if (this.rightSpace == value) return;
    this.rightSpace = value;
    this.redraw();
},

clear : function () {
    this.Super("clear", arguments);
    this.removeColumnSizer(true);
    this._clearTableCache();
    // if we're cleared before the delayed redraw from the end of fast scrolling fires,
    // clear the suppressDrawAheadDirection flag
    delete this._suppressDrawAheadDirection;    
},

// clear anything we've cached about the HTML table we draw
_clearTableCache : function () {
    // drop our cache of HTML row elements 
    this._rowElements = null;
    this._tableElement = null;

	// clear the cached table geometry information so it'll be recalculated the next time it's
    // asked for
	delete this._renderedColumnWidths;
	delete this._undrawnHeight;
	delete this._rowHeights;

    this._scrollRedraw = false;
} 

});
isc.GridRenderer._gridAPIs = {
    // customizing cell values
	// --------------------------------------------------------------------------------------------
    //>	@method	gridRenderer.getCellRecord()
    // Return the record that holds the value for this cell.
    // <P>
    // Implementing <code>getCellRecord</code> is optional: the actual HTML placed into each
    // grid cell comes from <code>getCellValue</code>, and a valid grid can be created without any
    // notion of "records" at all.
    // <p>
    // If you do implement <code>getCellRecord</code>, the value you return is passed to you as the
    // "record" parameter in other methods.
    //
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(object)	record for this cell
    // @visibility external
    //<
    getCellRecord : "rowNum,colNum",

    //>	@method	gridRenderer.getCellValue()
    // Return the HTML to display in this cell.  Implementing this is required to get a non-empty
    // grid.
    //
    // @param	record  (ListGridRecord)	cell record as returned by getCellRecord
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(string)	HTML to display in this cell
    // @visibility external
    //<
    getCellValue : "record,rowNum,colNum,gridBody",
    
    //>	@method	gridRenderer.findRowNum()
    // Given a record displayed in this grid, find and return the rowNum in which the record
    // appears.
    // <P>
    // As with +link{gridRenderer.getCellRecord()} implementing this method is optional as a valid
    // grid may be created without any notion of records.
    //
    // @param	record  (ListGridRecord)	cell record as returned by getCellRecord
    // @return	(number) index of the row containing the record or -1 if not found
    // @visibility external
    //<
    findRowNum : "record",
    //>	@method	gridRenderer.findColNum()
    // Given a record displayed in this grid, find and return the colNum in which the record
    // appears.
    // <P>
    // As with +link{gridRenderer.getCellRecord()} implementing this method is optional as a valid
    // grid may be created without any notion of records, or records may not be displayed in a
    // single column (as with the +link{class:ListGrid,ListGrid} class where each record is 
    // displayed in an entire row.  
    //
    // @param	record  (ListGridRecord)	cell record as returned by getCellRecord
    // @return	(number) index of the column containing the record or -1 if not found
    // @visibility external
    //<
    findColNum : "record",
    
    // customizing cell styling
	// --------------------------------------------------------------------------------------------
    //>	@method	gridRenderer.getBaseStyle() ([A])
    // Return the base stylename for this cell.  Default implementation just returns this.baseStyle.
    // See +link{listGrid.getCellStyle,getCellStyle()} for a general discussion of how to style cells.
    //
    // @see getCellStyle()
    //
    // @param	record  (ListGridRecord)	cell record as returned by getCellRecord
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(CSSStyleName)	CSS class for this cell
    // @visibility external
    //<
    getBaseStyle : "record,rowNum,colNum",

    // getCellStyle doc'd above
    getCellStyle : "record,rowNum,colNum",
    
    //>	@method	gridRenderer.getCellCSSText() ([A])
    // Return CSS text for styling this cell, which will be applied in addition to the CSS class
    // for the cell, as overrides.
    // <p>
    // "CSS text" means semicolon-separated style settings, suitable for inclusion in a CSS
    // stylesheet or in a STYLE attribute of an HTML element.
    //
    // @see getCellStyle()
    //
    // @param	record  (ListGridRecord)	cell record as returned by getCellRecord
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(string)	CSS text for this cell
    // @visibility external
    //<
    getCellCSSText : "record,rowNum,colNum",

    // doc'd above
    cellIsEnabled : "rowNum,colNum",

    // customizing table geometry
	// --------------------------------------------------------------------------------------------
    //>	@method	gridRenderer.getRowHeight()
    // Return the height this row should be.  Default is this.cellHeight. If
    // +link{GridRenderer.fixedRowHeights} is false, the row may be rendered taller than this
    // specified size.
    // <P>
    // If records will be variable height,
    // you should switch on +link{gridRenderer.virtualScrolling, virtualScrolling}.
    // @param	record  (ListGridRecord)	cell record as returned by getCellRecord
    // @param	rowNum	(number)	row number
    // @return	(number)	height in pixels
    // @visibility external
    //<
    // Undocumented 'isFrozenBody' param used by the ListGrid
    getRowHeight : "record,rowNum,isFrozenBody",
    
    //>	@method	gridRenderer.getRowSpan() ([A])
    // Return how many rows this cell should span.  Default is 1. 
    // <P>
    // NOTE: if using horizontal incremental rendering, <code>getRowSpan()</code> may be called for
    // a rowNum <b>in the middle of a spanning cell</b>, and should return the remaining span from
    // that rowNum onward.
    // <P>
    // NOTE: if a cell spans multiple rows, getCellRecord/Style/etc will be called with the topmost
    // row coordinates only.
    //
    // @param	record  (ListGridRecord)	cell record as returned by getCellRecord
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(number)	number of cells to span
    // @visibility external
    //<
    getRowSpan : "record,rowNum,colNum",

    // synthetic row/cell events
	// --------------------------------------------------------------------------------------------

    //>	@method	gridRenderer.cellOut() ([A])
    // Called when the mouse pointer leaves a cell
    //
    // @group   events
    // @param	record  (ListGridRecord)	cell record as returned by getCellRecord
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(boolean)	whether to cancel the event
    // @visibility external
    //<
    cellOut : "record,rowNum,colNum",

    //>	@method	gridRenderer.cellOver() ([A])
    // Called when the mouse pointer enters a cell
    //
    // @group   events    
    // @param	record  (ListGridRecord)	cell record as returned by getCellRecord
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(boolean)	whether to cancel the event
    // @visibility external
    //<
    cellOver : "record,rowNum,colNum",

    //>	@method	gridRenderer.rowOut() ([A])
    // Called when the mouse pointer leaves a row
    //
    // @group   events    
    // @param	record  (ListGridRecord)	cell record as returned by getCellRecord
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(boolean)	whether to cancel the event
    // @visibility external
    //<
    rowOut : "record,rowNum,colNum",

    //>	@method	gridRenderer.rowOver() ([A])
    // Called when the mouse pointer enters a row
    //
    // @group   events    
    // @param	record  (ListGridRecord)	cell record as returned by getCellRecord
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(boolean)	whether to cancel the event
    // @visibility external
    //<
    rowOver : "record,rowNum,colNum",
    
    //>	@method	gridRenderer.cellMove() ([A])
    // Called when the mouse pointer moves within a cell
    //
    // @group   events    
    // @param	record  (ListGridRecord)	cell record as returned by getCellRecord
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(boolean)	whether to cancel the event
    // @visibility internal
    //<
    cellMove : "record,rowNum,colNum",

    //>	@method	gridRenderer.rowMove() ([A])
    // Called when the mouse pointer moves within a row
    //
    // @group   events    
    // @param	record  (ListGridRecord)	cell record as returned by getCellRecord
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(boolean)	whether to cancel the event
    // @visibility internal
    //<
    rowMove : "record,rowNum,colNum",
    

    //>	@method	gridRenderer.cellContextClick() ([A])
    // Called when a cell receives a contextclick event.
    //
    // @group   events    
    // @param	record  (ListGridRecord)	cell record as returned by getCellRecord
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(boolean)	whether to cancel the event
    // @visibility external
    //<
    cellContextClick : "record,rowNum,colNum",

    //>	@method	gridRenderer.rowContextClick() ([A])
    // Called when a row receives a contextclick event.
    // @group   events
    // @param	record  (ListGridRecord)	cell record as returned by getCellRecord()
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(boolean)	whether to cancel the event
    // @visibility external
    //<    
    rowContextClick : "record,rowNum,colNum",
    // legacy support
    recordContextClick : "record,recordNum,fieldNum",

    //>	@method	gridRenderer.cellMouseDown() ([A])
    // Called when a cell receives a mousedown event.
    // 
    // @group   events
    // @param   record  (ListGridRecord)    cell record as returned by getCellRecord()
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(boolean)	whether to cancel the event
    // @visibility external
    //<    
    cellMouseDown : "record,rowNum,colNum",

    //>	@method	gridRenderer.rowMouseDown() ([A])
    // Called when a row receives a mousedown event.
    // 
    // @group   events
    // @param  record   (ListGridRecord)    record object returned from 'getCellRecord()'
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(boolean)	whether to cancel the event
    // @visibility external
    //<    
    rowMouseDown : "record,rowNum,colNum",
    
    // legacy
    recordMouseDown : "recordNum,fieldNum",
    
    //>	@method	gridRenderer.cellMouseUp() ([A])
    // Called when a cell receives a mouseup event.
    //
    // @group   events    
    // @param   record  (ListGridRecord)    Record object (retrieved from getCellRecord(rowNum, colNum))
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(boolean)	whether to cancel the event
    // @visibility external
    //<    
    cellMouseUp : "record,rowNum,colNum",
    
    //>	@method	gridRenderer.rowMouseUp() ([A])
    // Called when a row receives a mouseup event.
    //
    // @group   events    
    // @param   record  (ListGridRecord)    Record object returned from getCellRecord()
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(boolean)	whether to cancel the event
    // @visibility external
    //<    
    rowMouseUp : "record,rowNum,colNum",
    recordMouseUp : "recordNum,fieldNum",

    //>	@method	gridRenderer.selectOnMouseDown() ([A])
    // Called when a cell / record receives a mouseDown event, if no cell / row level mouseDown 
    // handlers return false.
    // Default implementation handles selection by calling this.selection.selectOnMouseDown()
    //
    // @group   selection    
    // @param   record  (ListGridRecord)    Record object returned from getCellRecord()
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    //<        
    selectOnMouseDown : "record,rowNum,colNum",

    //>	@method	gridRenderer.selectOnRightMouseDown() ([A])
    // Called when a cell / record receives a right mouseDown event, if this.canSelectOnRightMouse
    // is true.
    // Default implementation handles selection by calling this.selectOnMouseDown()
    //
    // @group   selection
    // @param   record  (ListGridRecord)    Record object returned from getCellRecord()
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @see selectOnMouseDown()
    //<        
    selectOnRightMouseDown : "record,rowNum,colNum",
    
    //>	@method	gridRenderer.selectOnMouseUp() ([A])
    // Called when a cell / record receives a mouseUp event, if no cell / row level mouseUp 
    // handlers return false.
    // Default implementation handles selection by calling this.selection.selectOnMouseUp()
    //
    // @group   selection    
    // @param   record  (ListGridRecord)    Record object returned from getCellRecord()
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    //<        
    selectOnMouseUp : "record,rowNum,colNum",

    //>	@method	gridRenderer.cellClick() ([A])
    // Called when a cell receives a click event.
    //
    // @group   events    
    // @param   record  (ListGridRecord)    Record object returned from getCellRecord()    
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(boolean)	whether to cancel the event
    // @visibility external
    //<    
    cellClick : "record,rowNum,colNum",

    //>	@method	gridRenderer.cellDoubleClick() ([A])
    // Called when a cell receives a double click event.
    //
    // @group   events    
    // @param   record  (ListGridRecord)    Record object returned from getCellRecord()    
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(boolean)	whether to cancel the event
    // @visibility external
    //<        
    cellDoubleClick : "record,rowNum,colNum",

    //>	@method	gridRenderer.rowClick() ([A])
    // Called when a row receives a click event.
    //
    // @group   events    
    // @param   record  (ListGridRecord)    Record object returned from getCellRecord()
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(boolean)	whether to cancel the event
    // @visibility external
    //<    
    rowClick : "record,rowNum,colNum",

    //>	@method	gridRenderer.rowDoubleClick() ([A])
    // Called when a row receives a double click event.
    //
    // @group   events    
    // @param   record  (ListGridRecord)    Record object returned from getCellRecord()
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(boolean)	whether to cancel the event
    // @visibility external
    //<        
    rowDoubleClick : "record,rowNum,colNum",

    // Hover events
	// --------------------------------------------------------------------------------------------
    //>	@method	gridRenderer.cellHover() ([A])
    // Called when the mouse hovers over a cell if this.canHover is true. 
    //  Returning false will suppress the hover text from being shown if this.showHover is true.
    //
    // @group   events    
    // @see     canHover
    // @param	record  (ListGridRecord)	cell record as returned by getCellRecord
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(boolean)	whether to cancel the event
    // @visibility external
    //<
    cellHover : "record,rowNum,colNum",

    //> @method gridRenderer.cellValueHover() ([A])
    // Optional stringMethod to fire when the user hovers over a cell and the value is clipped.
    // If this.showClippedValuesOnHover is true, the default behavior is to show a hover canvas
    // containing the HTML returned by cellValueHoverHTML(). Return false to suppress this default
    // behavior.
    //
    // @group events
    // @param record (ListGridRecord) cell record as returned by getCellRecord()
    // @param rowNum (number) row number for the cell
    // @param colNum (number) column number of the cell
    // @return (boolean) false to suppress the standard hover
    // @see showClippedValuesOnHover
    // @see cellValueIsClipped()
    // @see cellValueHoverHTML()
    // @visibility external
    //<
    cellValueHover : "record,rowNum,colNum",

    //>	@method	gridRenderer.rowHover() ([A])
    // Called when the mouse hovers over a row if this.canHover is true. 
    //  Returning false will suppress the hover text from being shown if this.showHover is true.
    //
    // @group   events    
    // @see     canHover    
    // @param	record  (ListGridRecord)	cell record as returned by getCellRecord
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(boolean)	whether to cancel the event (default behavior of showing the hover)
    // @visibility external
    //<    
    rowHover : "record,rowNum,colNum",
    
    //>	@method	gridRenderer.cellHoverHTML() ([A])
    // StringMethod to dynamically assemble an HTML string to show in a hover window over the
    // appropriate cell/record when this.canHover and this.showHover are both true.
    // Called when the mouse hovers over a cell.
    //
    // @group events
    // @param record (ListGridRecord) cell record as returned by getCellRecord
    // @param rowNum (number) row number for the cell
    // @param colNum (number) column number of the cell
    // @return (HTMLString) the html to be shown inside the hover for this cell
    // @see canHover
    // @see showHover
    // @visibility external
    //<    
    cellHoverHTML : "record,rowNum,colNum",

    //> @method gridRenderer.cellValueHoverHTML() ([A])
    // Returns the HTML that is displayed by the default cellValueHover handler. Return null or
    // an empty string to cancel the hover.
    // <smartgwt><p>Use <code>setCellValueHoverFormatter()</code> to provide a custom
    // implementation.</smartgwt>
    //
    // @group events
    // @param record (ListGridRecord) cell record as returned by getCellRecord()
    // @param rowNum (number) row number for the cell
    // @param colNum (number) column number of the cell
    // @param defaultHTML (HTMLString) the HTML that would have been displayed by default
    // @return (HTMLString) HTML to be displayed in the hover. If null or an empty string, then the hover
    // is canceled.
    // @see cellValueHover()
    // @visibility external
    //<
    cellValueHoverHTML : "record,rowNum,colNum,defaultHTML",

    //>	@method	gridRenderer.getCellHoverComponent() ([A])
    // StringMethod to dynamically create a Canvas-based component to show as a hover window 
    // over the appropriate cell/record when this.canHover and this.showHover are both true and
    // when an override of getCellHoverComponent() is present.
    // Called when the mouse hovers over a cell.
    //
    // @group   events
    // @param	record  (ListGridRecord)	cell record as returned by getCellRecord
    // @param	rowNum	(number)	row number for the cell
    // @param	colNum	(number)	column number of the cell
    // @return	(Canvas)	a Canvas to be shown as the hover for this cell
    // @see canHover
    // @see showHover
    // @visibility external
    //<    
    getCellHoverComponent : "record,rowNum,colNum",

    // selection notification
    // --------------------------------------------------------------------------------------------

    //>	@method	gridRenderer.selectionChanged() ([A])
    // Called when (row-based) selection changes within this grid. Note this method fires for
    // each record for which selection is modified - so when a user clicks inside a grid this
    // method will typically fire twice (once for the old record being deselected, and once for
    // the new record being selected).
    // <P>
    // NOTE: For updating other components based on selections or triggering selection-oriented
    // events within an application, see the
    // +link{dataBoundComponent.selectionUpdated(),selectionUpdated()} event
    // which is likely more suitable.  Calls to +link{selection.getSelection(),getSelection()}
    // from within this event may not return a valid set of selected records if the event has
    // been triggered by a call to +link{selection.selectAll(),selectAll()} or 
    // +link{selection.deselectAll(),deselectAll()} - in this case use the 
    // +link{dataBoundComponent.selectionUpdated(),selectionUpdated()} event instead.
    //
    // @param	record  (ListGridRecord)	record for which selection changed
    // @param	state   (boolean)	New selection state (true for selected, false for unselected)
    // @group selection
    // @visibility external
    //<    
    selectionChanged : "record,state",

    // Doc'd as DataBoundComponent.selectionUpdated
    selectionUpdated : "record,recordList",
    
    //>	@method	gridRenderer.cellSelectionChanged() ([A])
    // Called when (cell-based) selection changes within this grid.
    //
    // @param	cellList    (array) Array of cells whos selected state was modified.
    // @return	(boolean)   Returning false will prevent the GridRenderer styling from being updated
    //                      to reflect the selection change.
    // @group selection
    // @visibility external
    //<
    cellSelectionChanged : "cellList",

    // IDs for legacy test tools; JSDoc above
    getRowElementId : "rowNum,physicalRowNum",
    getCellElementId : "rowNum,physicalRowNum,colNum,physicalColNum",
    
    // Row Heights and Embedded Components
    // ---------------------------------------------------------------------------------------
    shouldFixRowHeight : "record,rowNum",
    
    updateEmbeddedComponentZIndex : "component",
    updateEmbeddedComponentCoords : "component,record,rowNum,colNum",

    // WAI ARIA
    // ---------------------------------------------------------------------------------------
    getRowRole : "rowNum,record",
    getRowAriaState : "rowNum,record",
    getCellRole : "rowNum,colNum,record",
    getCellAriaState : "rowNum,colNum,record"
};
isc.GridRenderer.registerStringMethods(isc.GridRenderer._gridAPIs);

