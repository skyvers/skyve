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





//>	@class	TileLayout
// Lays out a series of components, calls "tiles", in a grid with multiple tiles per row.
// 
// @treeLocation Client Reference/Grids
// @visibility external
//<
isc.ClassFactory.defineClass("TileLayout", "Canvas");

isc.TileLayout.addProperties({

//> @attr tileLayout.tiles (Array of Canvas : null : IR)
// List of tiles to lay out.
//
// @visibility external
//<

//> @attr tileLayout.layoutPolicy (TileLayoutPolicy : "fit" : IR)
// Policy for laying out tiles.  See +link{type:TileLayoutPolicy} for options.
//
// @group layoutPolicy
// @visibility external
//<
layoutPolicy: "fit",

//> @attr tileLayout.tileSize (int : 50 : IR)
// Size of each tile in pixels.  Depending on the +link{layoutPolicy}, <code>tileSize</code>
// may be taken as a maximum, minimum or exact size of tiles, or may be irrelevant.
// <P>
// Width and height may be separately set via +link{tileHeight} and +link{tileWidth}.
//
// @group sizing
// @visibility external
//<
tileSize: 50,

//> @attr tileLayout.tileWidth (int : null : IR)
// Width of each tile in pixels.  See +link{tileSize}.
// If +link{layoutPolicy} is "fit", +link{expandMargins} is false, +link{tilesPerLine} is set,
// +link{orientation} is "horizontal",
// and tileWidth is not set, tileWidth will be computed automatically based on +link{tilesPerLine}.
// 
// @group sizing
// @visibility external
//<

//> @attr tileLayout.tileHeight (int : null : IR)
// Height of each tile in pixels.  See +link{tileSize}.
// If +link{layoutPolicy} is "fit", +link{expandMargins} is false, +link{tilesPerLine} is set, 
// +link{orientation} is "vertical",
// and tileHeight is not set, tileHeight will be computed automatically based on +link{tilesPerLine}.
//
// @group sizing
// @visibility external
//<

//> @attr tileLayout.tileMargin (int : 10 : IR)
// Margin in between tiles.  Can be set on a per-axis basis with +link{tileHMargin} and
// +link{tileVMargin}.
//
// @group layoutMargin
// @visibility external
//<
tileMargin: 10,

//> @attr tileLayout.tileHMargin (int : null : IR)
// Horizontal margin in between tiles.  See +link{tileMargin}.
//
// @group layoutMargin
// @visibility external
//<

//> @attr tileLayout.tileVMargin (int : null : IR)
// Vertical margin in between tiles.  See +link{tileMargin}.
//
// @group layoutMargin
// @visibility external
//<

//> @attr tileLayout.layoutMargin (integer : 5 : IR)
// A margin left around the outside of all tiles.
//
// @group layoutMargin
// @visibility external
//<
layoutMargin:5,

//> @attr tileLayout.paddingAsLayoutMargin (Boolean : true : IRWA) 
// @include layout.paddingAsLayoutMargin
//
// @group layoutMargin
// @visibility external
//<
paddingAsLayoutMargin:true,

//> @attr tileLayout.animateTileChange (Boolean : true : IRWA) 
// If set, tiles animate to their new positions when a tile is added, removed, or reordered via
// drag and drop.
//
// @group appearance
// @visibility external
//<
animateTileChange: true,

//> @attr tileLayout.orientation (Orientation : "horizontal" : IR)
// Direction of tiling.  See also +link{type:TileLayoutPolicy}.
//
// @group layoutPolicy
// @visibility external
//<
orientation : "horizontal",

//> @attr tileLayout.tilesPerLine (int : null : IRW)
// Number of tiles to show in each line.  Auto-derived from +link{tileSize} for some layout
// modes.  See +link{type:TileLayoutPolicy}.
// This can also affect +link{tileWidth} or +link{tileHeight}. See those properties for details.
//
// @group layoutPolicy
// @visibility external
//<

//> @attr tileLayout.overflow   (Overflow : "auto" : IR)
// Normal +link{type:Overflow} settings can be used on TileLayouts, for example, an
// overflow:auto TileLayout will scroll if members exceed its specified size, whereas an
// overflow:visible TileLayout will grow to accommodate members.
//
// @group sizing
// @visibility external
//<
overflow: "auto",

// canFocus must be true or else a tileLayout with no scrollbars will be unable to 
// respond to keyboard events
canFocus: true,

//> @attr tileLayout.expandMargins (Boolean : true : IR)
// With +link{layoutPolicy}:"fit", should margins be expanded so that tiles fill the
// available space in the TileLayout on the breadth axis?
// This can also affect +link{tileWidth} or +link{tileHeight}. See those properties for details.
//
// @group layoutMargin
// @visibility external
//<
expandMargins: true,

//> @attr tileLayout.autoWrapLines (Boolean : true : IR)
// When +link{layoutPolicy} is "flow", should we automatically start a new line when there's
// not enough room to fit the next tile on the same line?
// <P>
// If set to false, a new line will only be started if a tile specifies tile.startLine or
// tile.endLine.
//
// @group layoutPolicy
// @visibility external
//<
autoWrapLines: true,

//> @type TileLayoutPolicy
// Policy for laying out tiles. 
// <P>
// Because a TileLayout can be either horizontally or vertically oriented, the general term
// "line" is used to mean either a row or column of tiles. 
// <P>
// <b>NOTE</b>: for typical form layouts (a variety of input fields and buttons in a
// tabular layout with col-spanning and row-spanning support), use a +link{DynamicForm} and
// see the +link{group:formLayout} topic.
//
// @value "fit"
// Each line has the same number of tiles, based on +link{tileLayout.tilesPerLine} if set, otherwise,
// based on fitting as many tiles per line as possible consistent with
// +link{tileLayout.tileSize,tileSize} and +link{tileLayout.tileMargin}. 
// <P>
// Tiles that do not specify a size will be sized to fill available space.
//
// @value "flow"
// Tiles are laid out with varying numbers of tiles per line according to each tile's size,
// the +link{tileLayout.tileMargin,tileMargin}, and the available space.
// <P>
// Tiles are never resized by the TileLayout and +link{tileLayout.tileSize} is ignored.
//
// @visibility external
//<

//> @attr tileLayout.dragLine (AutoChild Canvas : null : R)
// Canvas used to display a drop indicator when a another canvas is dragged over this widget.
//
// @visibility external
//<
dragLineDefaults: {
    overflow: "hidden",
    styleName: "layoutDropLine"
    
},

initWidget : function () {
    this._enforceLegalLayoutPolicy();
    this.invokeSuper(isc.TileLayout, "initWidget");
    if (!this.tiles) this.tiles = [];
    // this makes d & d code work with tileLayout and tileGrid equally well
    this.data = this.tiles; 
    
    
},

draw : function (a, b, c, d) {
    this.invokeSuper(isc.TileLayout, "draw", a, b, c, d);
    // set tile width or height if it unset based on tilesPerLine
    this._setTileSize();
    this.logDebug('calling layoutTiles from draw', "TileLayout");
    this.layoutTiles();
    
},

layoutChildren : function () {
    this.Super("layoutChildren", arguments);
    this.logDebug('calling layoutTiles from layoutChildren', "TileLayout");

    
    this._setTileSize();
    this.layoutTiles();
},

childResized : function (child, deltaX, deltaY, reason) {
    if (this._layoutInProgress) return;

    this.invokeSuper(isc.TileLayout, "childResized", child, deltaX, deltaY, reason);
    // react to one of our tiles resizing (but ignore eg the emptyMessageLabel or other
    // non-tile resizes)
    if (this.tiles != null && this.tiles.contains(child)) {
        // need to use a timeout here otherwise the child doesn't get resized
        this.logDebug('calling layoutTiles from childResized', "TileLayout");
        this.delayCall("layoutTiles", null, 100);
    }
},

// make sure that a legal layoutPolicy is set or code breakage will ensue
_enforceLegalLayoutPolicy : function () {
    if (this.layoutPolicy != "fit" && this.layoutPolicy != "flow") {
        this.layoutPolicy = "fit";    
    }
},


willScroll : function () {
    var isHoriz = this.orientation == "horizontal";
    var topMargin = this._topMargin;
    var bottomMargin = this._bottomMargin;
    var leftMargin = this._leftMargin;
    var rightMargin = this._rightMargin;
    // make sure that only user-visible tiles are iterated
    var numTiles, userVisibleTiles;
    if (this._enableUserHiding) {
        userVisibleTiles = this.getUserVisibleTiles();
        numTiles = userVisibleTiles.getLength();
    } else {
        numTiles = this.getLength();
    }
    if (this.layoutPolicy == "flow") {
        if (this.overflow != "auto" && this.overflow != "scroll") return false;
       
        var availBreadth =  isHoriz ? this.getInnerWidth() - rightMargin : this.getInnerHeight() - bottomMargin;
        var availLength = isHoriz ? this.getInnerHeight() - bottomMargin : this.getInnerWidth() - leftMargin;
        
        var currBreadthPos = isHoriz ? leftMargin : topMargin;
        var currLengthPos = isHoriz ? topMargin : leftMargin;
        var currMaxLength = 0;
        // keep track of rows for debugging purposes mainly
        var numRows = 0;
        var tileBreadthMargin = isHoriz ? this.getTileHMargin() : this.getTileVMargin();
        var tileLengthMargin = isHoriz ? this.getTileVMargin() : this.getTileHMargin();
        for (var i = 0; i < numTiles; i++) {
            var newTile = userVisibleTiles ? userVisibleTiles[i] : this.getTile(i);
             // check for startLine flag 
            if (!this.autoWrapLines && newTile.startLine) currBreadthPos = 0;
            var tileBreadth = isHoriz ? newTile.getVisibleWidth() : newTile.getVisibleHeight();
            
            // check for end of line
            if ((this.autoWrapLines && currBreadthPos + tileBreadth > availBreadth) 
                    || (!this.autoWrapLines && newTile.endLine)) {
                // reset breadth position
                currBreadthPos = isHoriz ? leftMargin : topMargin;;
                // currMaxLength + lengthMargin gives us length position of next line
                currLengthPos += currMaxLength + tileLengthMargin;
                currMaxLength = 0;
                numRows++;
            }    
            var tileLength = isHoriz ? newTile.getVisibleHeight() : newTile.getVisibleWidth();
            // if the children are already passed our length, we will scroll, so return true
            if (currLengthPos + tileLength > availLength) return true;
            // keep track of max length
            if (currMaxLength < tileLength) {
                currMaxLength = tileLength;    
            }
            // move to the next breadth position
            currBreadthPos += tileBreadth + tileBreadthMargin;
            
        }
        // no scrolling, so return false
        return false;
    } else {
        // scrolling will happen if... 
        // this.length / (tileLength + tileMargin) * tiles per line > data length 
        var availLength = isHoriz ? this.getVisibleHeight() - (topMargin + bottomMargin) 
                                    : this.getVisibleWidth() - (rightMargin + leftMargin);
        var tileLength = isHoriz ? this.getTileHeight() : this.getTileWidth();
        var tileLengthMargin = isHoriz ? this.getTileVMargin() : this.getTileHMargin();
        var tpl = this.getTilesPerLine();
        var maxLinesTillScroll = Math.floor(availLength / (tileLength + tileLengthMargin));
        var maxTilesTillScroll = tpl * maxLinesTillScroll;
        return (numTiles > maxTilesTillScroll);
    }
},

layoutTiles : function (mode) {
    // set a flag that we are doing layout stuff, so that we can ignore when we're notified that a
    // member has been resized
    var layoutAlreadyInProgress = this._layoutInProgress;
    this._layoutInProgress = true;

    
    this._layoutTiles(mode);

    this._layoutInProgress = layoutAlreadyInProgress;
    if (layoutAlreadyInProgress) this._abortLayoutInProgress = true;
},

_layoutTiles : function (mode) {
    
    if (!this.isDrawn()) return;

    // keep track of visible tiles for animation purposes.
    // Tiles are added here in processTile(). Used in _animateChange() 
    this._visibleTiles = [];
    // keep track of # of tiles processed so extra ones can be hidden
    this._numTilesProcessed = 0;

    // if recycleTiles is false, processed tiles may not be contiguous
    if (this.tiles) this.tiles.map(function(tile) {delete tile._processed;});

    // cache our dimensions to determine if a tile is visible in processTile()
    this._viewportRect = [this.getScrollLeft(), this.getScrollTop(),
                          this.getInnerWidth(), this.getInnerHeight()];
    // do margin calculations first so they can be used for both flow and fit layouts
    var tPad, bPad, lPad, rPad;
    if (this.paddingAsLayoutMargin) {
        var padding = this._calculatePadding();
        lPad = padding.left; rPad = padding.right;
        tPad = padding.top; bPad = padding.bottom;    
    }
    // cache margin calculations
    var topMargin = this._topMargin = this._firstNonNull(this.layoutMargin, tPad, 0);
    var bottomMargin = this._bottomMargin = this._firstNonNull(this.layoutMargin, bPad, 0);
    var leftMargin = this._leftMargin = this._firstNonNull(this.layoutMargin, lPad, 0);
    var rightMargin = this._rightMargin = this._firstNonNull(this.layoutMargin, rPad, 0);
    // make sure that only user-visible tiles are iterated
    // NOTE could have explored skipping hidden tiles in the layout loops, but culling visible
    // tiles beforehand seemed cleaner/easier.
    var numTiles, userVisibleTiles;
    if (this._enableUserHiding) {
        userVisibleTiles = this.getUserVisibleTiles();
        numTiles = userVisibleTiles.getLength();
    } else {
        numTiles = this.getLength();
    }
    // flow layout
    if (this.layoutPolicy == "flow") {
        this.logDebug("starting flow layout", "TileLayout");
        var isHoriz = this.orientation == "horizontal";
        // only subtract right or bottom margin from available breadth, as top/left margin is already
        // accounted for when setting current breadth/length position
        var availBreadth =  isHoriz ? this.getInnerWidth() - rightMargin : 
                                    this.getInnerHeight() - bottomMargin;
        // if scrolling will occur, take the scrollbar into account
        if (this.willScroll()) availBreadth -= this.getScrollbarSize();
  
        var currBreadthPos = isHoriz ? leftMargin : topMargin;
        var currLengthPos = isHoriz ? topMargin : leftMargin;
        var currMaxLength = 0;
        // keep track of rows for debugging purposes mainly
        var numRows = 0;
        var tileBreadthMargin = isHoriz ? this.getTileHMargin() : this.getTileVMargin();
        var tileLengthMargin = isHoriz ? this.getTileVMargin() : this.getTileHMargin();
       
        for (var i = 0; i < numTiles; i++) {     
            var newTile = userVisibleTiles ? userVisibleTiles[i] : this.getTile(i);
             // check for startLine flag 
            if (!this.autoWrapLines && newTile.startLine)  currBreadthPos = 0;
            var tileBreadth = isHoriz ? newTile.getVisibleWidth() : newTile.getVisibleHeight();
            // check for end of line
            if ((this.autoWrapLines && currBreadthPos + tileBreadth > availBreadth) 
                || (!this.autoWrapLines && newTile.endLine)) {
                // reset breadth position
                currBreadthPos = isHoriz ? leftMargin : topMargin;;
                // currMaxLength + lengthMargin gives us length position of next line
                // When the first tile is larger than available breadth, don't add a margin
                // to currLengthPos; we just want it to remain the same in this case, which is 0. 
                currLengthPos += currMaxLength + (i == 0 ? 0 : tileLengthMargin);
                currMaxLength = 0;
                numRows++;
            }
            // horizontal : top = length position, left = breadth position
            // vertical : top = breadth position, left = length position
            var nTop = isHoriz ? currLengthPos : currBreadthPos;
            var nLeft = isHoriz ? currBreadthPos : currLengthPos;
            var tileToUse = userVisibleTiles ? userVisibleTiles[i] : i;
            this.processTile(tileToUse, nTop, nLeft);
            // keep track of max length
            var tileLength = isHoriz ? newTile.getVisibleHeight() : newTile.getVisibleWidth();
            if (currMaxLength < tileLength) {
                currMaxLength = tileLength;    
            }
            // move to the next breadth position
            currBreadthPos += tileBreadth + tileBreadthMargin;
            
        }
    // fit layout  
    } else {
        this.logDebug("starting fit layout:" + this._animating, "TileLayout");
        var tPerLine = this.getTilesPerLine();
        var tHeight = this.getTileHeight();
        var tWidth = this.getTileWidth();
        var isHoriz = this.orientation == "horizontal";
        var tHMargin, tVMargin, extraPixels;
       
        var totalTiles, tileNum, numLines, spacerCanvas, startLine, endLine;
        if (this.shouldUseIncrRendering()) {
            this.logDebug("fit layout, using incremental rendering", "TileLayout");
            var tileRange = this.getVisibleTiles();
            // store visible tile range for subclasses that may want to use it (TileGrid, etc.)
            // these values will be returned by getDrawnStartIndex() getDrawnEndIndex(), which subclasses
            // should use to access these values
            this._lastVisibleTiles = tileRange;
            tileNum = tileRange[0];
            totalTiles = numTiles;
            var lineRange = this.getVisibleLines();
            startLine = lineRange[0];
            endLine = lineRange[0] + lineRange[1];
            // make sure all visible tiles are present. If not, return. 
            // Relevent for databound subclasses (TileGrid etc)
            if (!this.hasAllVisibleTiles(tileRange, true)) {
                
                if (this.loadingMessage == null || !this.data.lengthIsKnown()) return;
            }
        } else {
            this.logDebug("fit layout, rendering all tiles", "TileLayout");
            delete this._lastVisibleTiles;
            totalTiles = numTiles;
            tileNum = 0;
            startLine = 0;
            endLine = Math.ceil(totalTiles / tPerLine);
            tHMargin = this.getTileHMargin();
            tVMargin = this.getTileVMargin();       
        }
        
        if (this._lastVisibleTilesChanged) this._lastVisibleTilesChanged();
        
        // set up the spacer canvas for all rendering options so that the layoutMargin can 
        // always be created and show up on the bottom.
        var totNumLines = Math.ceil(numTiles / tPerLine);
        var tVMargin = this.getTileVMargin();
        var tHMargin = this.getTileHMargin();
        var tileLength = isHoriz ? tHeight : tWidth;
        var lengthMargin = isHoriz ? tVMargin : tHMargin;
        // total lines length is tile length + margin length times # of lines
        // subtract 1 extra tilemargin length from the end, and add either top + bottom margin 
        // (for horizontal orientation) or left + right margin (for vertical)
        var totLen = ((tileLength + lengthMargin) * totNumLines) - lengthMargin 
            + (isHoriz ? topMargin + bottomMargin : leftMargin + rightMargin);
        spacerCanvas = this._getSpacerCanvas();
        isHoriz ? spacerCanvas.setHeight(totLen) : spacerCanvas.setWidth(totLen);
        spacerCanvas.sendToBack();
        
        // get the extra pixels before begining actual tile layout
        extraPixels = this.getExtraMarginPixels(tPerLine, tHeight, tWidth, tHMargin, tVMargin);

        // allow nested calls to layoutTiles() to be detected
        this._abortLayoutInProgress = false;
       
        for (var i = startLine; i < endLine; i++) {
            // keep track of extra pixels to divide among tiles 
            var exPixels = extraPixels;
            for (var j = 0; j < tPerLine; j++) {  
                // for horizontal, top will remain constant and left will vary.
                // for vertical, top will vary and left will remain constant.
                var topPos = isHoriz ? i : j;
                var leftPos = isHoriz ? j : i;

                // top = (current top position * tile height) + (current top position * tile vertical margin) 
                //      + top layout margin
                var nTop = (topPos * tHeight) + (topPos * tVMargin ) + topMargin;
                // if we're vertical and the current vertical position is less than the number of
                // extra pixels, use our vertical position as the number of extra pixels to add to 
                // the current top coordinate to get the proper placement
                if (!isHoriz && j + 1 <= extraPixels) nTop += j + 1;
                // left = same logic as top, but substitute left
                var nLeft = (leftPos * tWidth) + (leftPos * tHMargin) + leftMargin;
                // if we're horizontal and the current horizontal position is less than the number of
                // extra pixels, use our horizontal position as the number of extra pixels to add to 
                // the current left coordinate to get the proper placement
                if (isHoriz && j + 1 <= extraPixels) nLeft += j + 1;
                var tileToUse = userVisibleTiles ? userVisibleTiles[tileNum] : tileNum
                
                var newTile = this.processTile(tileToUse, nTop, nLeft, 
                        this.getTileHeight(), this.getTileWidth());
                if (this._abortLayoutInProgress) return;
              
                tileNum++;
                // check for the case where more images are returned than will fit
                if (tileNum >= totalTiles) break; 
            }  
            if (tileNum >= totalTiles) break;
        }
    }
   
},

// This function is more for the sake of databound superclasses (TileGrid). 
// For tilelayout just return true.
hasAllVisibleTiles : function () { return true; },

// flag so that superclasses (i.e. TileGrid) can turn manual tile hiding off
_enableUserHiding: true,
// helper function used in layoutTiles to get only tiles not hidden by an external call to hide()
getUserVisibleTiles : function () {
    var visTiles = [];
    for (var i = 0; i < this.getLength(); i++) {
        var curr = this.getTile(i);
        if (!curr._userHidden) visTiles.add(curr);
    }
    return visTiles;
},

// tileNum can be the index of a given tile (TileGrid), or the tile itself (TileLayout)
processTile : function (tileNum, top, left, height, width) {
    var tile;
    //isc.logWarn("processing tile:" + [top, left, height, width]);
    if (this._animating) {
        
        if (isc.isA.Canvas(tileNum)) tile = tileNum;
        else tile = this.getRecordTile(tileNum);
        if (!tile) return;
        // verify that tile was visible before
        if (this._oldVisibleTiles != null && this._oldVisibleTiles.findIndex("ID", tile.ID) == -1) return;
        //isc.logWarn("tile:" + tile);
        if (!tile) return;
        
        if (height) tile.setHeight(height);
        if (width) tile.setWidth(width);
        // only track the tile for animation if it will actually change position. This helps
        // prevent excess show/hide cycles.
        if (tile.getTop() != top || tile.getLeft() != left) {
            // store the top and left to which this tile should be animated (see _animateChange())
            tile._newTop = top;
            tile._newLeft = left;
            this._tilesToAnimate.add(tile);
        }
        
        var tileRect = [left, top, tile.getVisibleWidth(), tile.getVisibleHeight()];
     
        // make sure tile is actually visible before adding to visibleTiles
        if (isc.Canvas.rectsIntersect(this._viewportRect, tileRect)) {
             if (!this._visibleTiles.contains(tile)) this._visibleTiles.add(tile);
        }
    } else {
       
        if (isc.isA.Canvas(tileNum)) tile = tileNum;
        else tile = this.getTile(tileNum);
        //isc.logWarn('processing tile: ' + this.echoFull(tile));
        if (!tile) return;
        // redraw dirty tiles
        if (tile.isDirty()) tile.redraw("tile dirty during layout");
        // set height and width here
        if (height) tile.setHeight(height);
        if (width) tile.setWidth(width);
        tile.setTop(top);
        tile.setLeft(left);
        // add tile to children
        this.addChild(tile);
        // must do this because tiles may be hidden from a call to cleanupExtraTiles() from 
        // within dataChanged(). see dataChanged() for more info.
        
        if (isc.TileGrid && isc.isA.TileGrid(this)) {
            tile._suppressVisibilityChange = true;
            tile.show();
            tile._suppressVisibilityChange = null;
        } else {
            if (tile.visibility == "hidden") tile._userHidden = true;
            else                             tile._userHidden = null;
        }
        // add tile to _visibleTiles for animation purposes. See _animateChange.
        // we keep track of this in TileGrid and not TileLayout, because this pertains more to 
        // TileGrid functionality than to tileLayout, thus keeping the code cleaner.
        // Not true anymore, tileLayout now has animations 11/3/08
        var tileRect = [tile.getLeft(), tile.getTop(), tile.getVisibleWidth(), tile.getVisibleHeight()];
     
        // make sure tile is actually visible before adding to visibleTiles
        if (isc.Canvas.rectsIntersect(this._viewportRect, tileRect)) {
             if (!this._visibleTiles.contains(tile)) this._visibleTiles.add(tile);
        }
        // keep track of which tiles are processed so that extra tiles can be hidden if needed
        // in layoutTiles()
        this._numTilesProcessed++;
        tile._processed = true;
        return tile;
    }
      
},

_getSpacerCanvas : function () {
    if (!this._spacerCanvas) {
        this._spacerCanvas = isc.Canvas.create({autoDraw:false, width:1, height:1}); 
        // spacer canvas to make the scroll behave appropriately
        this.addChild(this._spacerCanvas);
    }
    return this._spacerCanvas;
},

getDrawnStartIndex : function (nullIfNoRange) {
    // this check should be adequate. We aren't supporting changing incremental rendering on the 
    // fly, so this will always return null if its off..._lastVisibleTiles is only created when
    // incremental rendering is on. (see shouldUseIncrRendering())
    if (this._lastVisibleTiles) return this._lastVisibleTiles[0];
    // default to first record since that's the most common use case
    return nullIfNoRange ? null : 0;
},

getDrawnEndIndex : function (nullIfNoRange) {
    if (this._lastVisibleTiles) return this._lastVisibleTiles[1];
    // default to the number of records since that's most common use case
    return nullIfNoRange ? null : (this.data ? this.data.getLength() : 0);
},

shouldUseIncrRendering : function () {
    if (this._animating) {
        return true;
    } else if (!this.showAllRecords && this.layoutPolicy == "fit" && 
               (this.overflow == "auto" || this.overflow == "hidden")) 
    {
        var data = this.data;
        // drawAllMaxTiles comparison can't be made without a valid length for the data
        if (data == null || isc.ResultSet && isc.isA.ResultSet(data) && !data.lengthIsKnown()) {
            return true
        }
        var maxTiles = this.drawAllMaxTiles;
        // allow drawAllMaxTiles to disable incremental rendering based on data length
        return maxTiles == null || maxTiles == 0 || data.getLength() > maxTiles;
    } else {
        return false;
    }
},

getLength : function () {
    if (!this.tiles) return 0;
    else return this.tiles.getLength();
},

getTilesPerLine : function () {
    if (this.tilesPerLine) return this.tilesPerLine;
    else {
        // variable names are in terms of horizontal orientation
        var tSize = this.orientation == "horizontal" ? this.getTileWidth() : this.getTileHeight();
        var tMargin = this.orientation == "horizontal" ? (this.tileHMargin || this.tileMargin) : 
                            (this.tileVMargin || this.tileMargin);
        var thisSize = this.orientation == "horizontal" ? this.getInnerWidth() : this.getInnerHeight();
        // first just get the number of tiles, without margin, that will fit
        // NOTE margin between the peripheral tiles and widget boundaries are not counted
        var numTiles = Math.floor(thisSize / tSize);
        
        // now factor in the margins; if the total width is greater than our width, subtract
        // 1 from the the tile count.
        var marginOffset = this.orientation == "horizontal" ? this._leftMargin + this._rightMargin :
                                                              this._topMargin + this._bottomMargin;
        var totLineWidth = tMargin * (numTiles - 1) + (numTiles * tSize) + marginOffset;  
        // total line size may be over the limit by more than 1 tile size
        if (totLineWidth > thisSize) {
            var numTilesOver = Math.ceil((totLineWidth - thisSize) / tSize);
            numTiles -= numTilesOver;
        }
        // at least one tile per line
        if (numTiles < 1) numTiles = 1;
        
        return numTiles;
    }
},

_setTileSize : function () {
    var isHoriz = this.orientation == "horizontal";
    // only set tile width/height if layoutPolicy = fit, expandMargins = false,
    // tilesPerLine is set, and tileWidth/height is not set
    if (this.layoutPolicy != "fit" || this.expandMargins || !this.tilesPerLine
        || (isHoriz && this.tileWidth) || (!isHoriz && this.tileHeight)) return;
    var tMargin = isHoriz ? (this.tileHMargin || this.tileMargin) : 
                            (this.tileVMargin || this.tileMargin);
    var thisSize = isHoriz ? this.getInnerWidth() : this.getInnerHeight();
    // getInnerWidth does not take margin into account 
    // for some reason, this._leftMargin etc. aren't set yet here so use layoutMargin directly
    var marginOffset = this.layoutMargin * 2;
    var totLineWidth = thisSize - marginOffset;
    if (!this.tilesPerLine) {
        // Ensure tileSize has a default
        
        if (this.tileSize == null) this.tileSize = 50;    
    } else {
        var tSize = Math.floor(totLineWidth / this.tilesPerLine);
        tSize -= tMargin;
        //isc.logWarn('setTileSize:' + [tSize, thisSize, this.layoutMargin]);
        if (this.orientation == "horizontal") this.defaultTileWidth = tSize;
        else this.defaultTileHeight = tSize;
    }
},

getVisibleLines : function () {
    var horizontal = (this.orientation == "horizontal");
    var scrollPos = horizontal ? this.getScrollTop() : this.getScrollLeft();
    var lineHeight = horizontal ? this.getTileHeight() + this.getTileVMargin() :
                                  this.getTileWidth() + this.getTileHMargin();
    var windowSize = horizontal ? this.getInnerHeight() : this.getInnerWidth();
    
    var numLinesOffScreen = Math.floor(scrollPos / lineHeight);
    // overestimate start line by adding a line to the front
    if (numLinesOffScreen > 0) numLinesOffScreen--;
    
    var numLinesOnScreen = Math.ceil(windowSize / lineHeight) + 2;
    
    return [numLinesOffScreen, numLinesOnScreen];
},

getVisibleTiles : function () {
    var visibleLines = this.getVisibleLines();
    var tilesPerLine = this.getTilesPerLine();
    var start = visibleLines[0] * tilesPerLine;
    var end = (visibleLines[0] + visibleLines[1]) * tilesPerLine;
    
    return [start, end];
},

scrolled : function () {
    if (this.shouldUseIncrRendering()) {
        if (this._layoutEventId) isc.Timer.clear(this._layoutEventId);
        this._layoutEventId = this.delayCall("layoutAfterScroll");
    }
},

layoutAfterScroll : function () {
    this.logDebug('layoutAfterScroll', "TileLayout");
    if (this.shouldLayoutTiles()) {
        this.logDebug('calling layoutTiles from layoutAfterScroll', "TileLayout");
        this.layoutTiles();
    } 
    this._layoutEventId = null;
},

shouldLayoutTiles : function () {
    var currTiles = this.getVisibleTiles();
    return currTiles[0] != this.getDrawnStartIndex(true) ||
           currTiles[1] != this.getDrawnEndIndex  (true)
},

getTileWidth : function () {
    if (this.tileWidth) {
        if (isc.isA.String(this.tileWidth)) {
            this.tileWidth = parseInt(this.tileWidth);
            if (!isc.isA.Number(this.tileWidth)) this.tileWidth = this.tileSize;
        }
        return this.tileWidth;
    } else if (this.defaultTileWidth) {
        return this.defaultTileWidth;
    } else {
        return this.tileSize;    
    }
},

getTileHeight : function () {
    if (this.tileHeight) {
        if (isc.isA.String(this.tileHeight)) {
            this.tileHeight = parseInt(this.tileHeight);
            if (!isc.isA.Number(this.tileHeight)) this.tileHeight = this.tileSize;
        }
        return this.tileHeight;
    } else if (this.defaultTileHeight != null) {
        return this.defaultTileHeight;
    } else {
        return this.tileSize;
    }
},

// helper function to consolidate code used in various places
getInnerBreadth : function () {
    /*
    
    
    var breadth = this.orientation == "horizontal" ? this.getVisibleWidth() - this.getHBorderPad()
        : this.getVisibleHeight() - this.getVBorderPad();
    if (this.willScroll()) breadth -= this.getScrollbarSize();
    // account for the tileLayout having edges
    if (this.showEdges && this._edgedCanvas) {
        var edgesSize, ec = this._edgedCanvas;
        if (this.orientation == "horizontal") edgesSize = ec._leftEdge + ec._rightEdge;
        else edgesSize = ec._topEdge + ec._bottomEdge;
        breadth -= edgesSize;
    }
    */
    // getInnerWidth/Height takes care of all borders and edges
    var breadth = this.orientation == "horizontal" ? this.getInnerWidth() 
        : this.getInnerHeight();
    //if (this.willScroll()) breadth -= this.getScrollbarSize();
    return breadth;
},

getTileHMargin : function () {    
    
    var hMargin;        
    if (this.tileHMargin) {
        hMargin = this.tileHMargin;
    } else {
        hMargin = this.tileMargin;
    }
    if (this.layoutPolicy == "fit" && this.expandMargins && this.orientation == "horizontal") {
        var tpl = this.getTilesPerLine();
        
        // expanded margin = (viewable width - all tiles in a line width - layoutMargins) / number of margins in a line
        // remainder is calculated later, in getExtraMarginPixels
        var lMargins = this._leftMargin + this._rightMargin;
        var marginsInALine = tpl - 1;
        // can't have zero margins in a line
        if (marginsInALine == 0) marginsInALine = 1;
        var exMargin = Math.floor((this.getInnerBreadth() - (tpl * this.getTileWidth()) - lMargins) / marginsInALine);
        // don't return less than hMargin
        if (exMargin < hMargin) return hMargin;
        else return exMargin;
    } else {
        return hMargin;
    }
    
   
},

getTileVMargin : function () {
   // if (this.tileVMargin) return this.tileVMargin;
   // else return this.tileMargin;
    
    var vMargin;        
    if (this.tileHMargin) {
        vMargin = this.tileVMargin;
    } else {
        vMargin = this.tileMargin;
    }
    if (this.layoutPolicy == "fit" && this.expandMargins && this.orientation == "vertical") {
        var tpl = this.getTilesPerLine();
        // expanded margin = (viewable height - tilesPerLine height) / marginsPerLine
        // remainder is calculated later, in getExtraMarginPixels
        var lMargins = this._topMargin + this._bottomMargin;
        var marginsInALine = tpl - 1;
        // can't have zero margins in a line
        if (marginsInALine == 0) marginsInALine = 1;
        var exMargin = Math.floor((this.getInnerBreadth() - (tpl * this.getTileHeight()) - lMargins) / marginsInALine);
        // don't return less than hMargin
        if (exMargin < vMargin) return vMargin;
        else return exMargin;
    } else {
        return vMargin;
    }
},

// this calculates the remainder of pixels after expanded margins are calculated in getTileHMargin 
// and getTileVMargin. These pixels will then be spread out between tiles in layoutTiles() for fit layout mode
getExtraMarginPixels : function (tpl, tHeight, tWidth, tHMargin, tVMargin) {
    if (this.expandMargins && this.orientation == "horizontal") {
        // viewable width - (tilesPerLine width + marginsPerLine width)
        var usedPixels = (tpl * tWidth) + ((tpl - 1) * tHMargin) + (this._leftMargin + this._rightMargin);
        return this.getInnerBreadth() - usedPixels;
    } else if (this.expandMargins && this.orientation == "vertical") {
        // viewable height - (tilesPerLine height + marginsPerLine height)
        var usedPixels = (tpl * tHeight) + ((tpl - 1) * tVMargin) + (this._topMargin + this._bottomMargin);
        return this.getInnerBreadth() - usedPixels;
                                        
    } else {
        return 0;
    }
},

//> @method tileLayout.getTile()
// Retrieve a tile by index.  
// <P>
// The TileLayout consistently uses this method to access tiles, in order to allow subclasses
// to create tiles on demand.
//
// @param index (int) index of the tile
// @return (Canvas) the tile
//
// @visibility external
//<
getTile : function (index) {
    
    return isc.Class.getArrayItem(index, this.tiles);
},

//> @method tileLayout.addTile()
// Add a tile to the layout, dynamically.
//
// @param tile (Canvas) new tile to add 
// @param [index] (integer) position where the tile should be added.  Defaults to adding the tile
// at the end.
// @visibility external
//<
addTile : function (tile, index) {
     if (!this.tiles) return;
     if (index !== 0 && !index) index = this.tiles.getLength();
     this.tiles.addAt(tile, index);
     this.reLayout();
},

//> @method tileLayout.removeTile()
// Remove a tile from the layout.
//
// @param tileID (Canvas or int or ID) index or String ID of the tile
// @return (boolean) whether a tile was found and removed
// @visibility external
//<
removeTile : function (tile) {
    if (!this.tiles) return;
   
    if (!isc.isA.Canvas(tile)) {
        // first assume tile is a canvas ID
        var index = this.tiles.findIndex("ID", tile);
        // if not, assume tile is an index into tiles
        if (index == -1) tile = this.tiles.get(tile);
        else tile = this.tiles.get(index);
    }
    // removeChild handles internal cleanup as well
    this.removeChild(tile);
},

// override removeChild to do internal cleanup and layout tiles again after the child is removed
removeChild : function (child, name) {
    this.tiles.removeWhere("ID", child.ID);
    if (this._visibleTiles) {
        this._visibleTiles.removeWhere("ID", child.ID);
    }
    if (this._oldVisibleTiles && this._oldVisibleTiles != this._visibleTiles) {
        this._oldVisibleTiles.removeWhere("ID", child.ID);
    }
    if (this._tilesToAnimate) {
        this._tilesToAnimate.removeWhere("ID", child.ID);
    }
    this.invokeSuper(isc.TileLayout, "removeChild", child, name);
    this.reLayout();
},

getRecordTile : function (recordIndex) {
    if (recordIndex == null) return null;
    return this.tiles.get(recordIndex);    
},

// override here to track external calls to tile.hide(), so that we won't forcibly show
// those tiles in layoutTiles
childVisibilityChanged : function (child, newVisibility) {
    if (!this._animating && !child._suppressVisibilityChange) {
        // _userHidden is used in getUserVisibleTiles()
        if (newVisibility == "hidden") child._userHidden = true;
        else child._userHidden = null;
        this.reLayout();
    } 
},

relayoutProperties:["tilesPerLine", "orientation", "tileWidth", "tileHeight", "expandMargins"],
propertyChanged : function (propertyName, value) {
    this.invokeSuper(isc.TileLayout, "propertyChanged", propertyName, value);
    // tileMargin et al
    if (isc.endsWith(propertyName, "Margin") || 
        this.relayoutProperties.contains(propertyName)) 
    {
        this.layoutTiles();
    }
},

// --------------------------Drag and Drop-----------------------------------------------------
// for dragging records out, use the drag tracker
//dragAppearance:isc.EH.TRACKER,
//dragTrackerMode: "title",
canDrop: true,
_dragTrackerThickness: 2,

//>	@method	tileGrid.showDragLineForRecord()	(A)
// Show the drag line relative to a particular record.
// <p>
// If no record number is passed, assumes the one under the mouse.
// <p>
// This is used to show feedback in reordering rows or to insert dragged records at a particular
// row.
//		@group	dragging, drawing
//<
showDragLineForRecord : function () {
  // isc.logWarn('tilelayout.showDragLineForRecord: ' + this.data);
     if (isc.isAn.Array(this.data) || (isc.isA.ResultSet(this.data))) {
        var x = this.getOffsetX(), y = this.getOffsetY(), xBase = this.getPageLeft(), yBase = this.getPageTop();
        if (this.data.getLength() == 0) {
            return;    
        }
        
        // get index of tile that mouse is over
        var currIndex = this.findIndexForCoord(x, y);
        // keep track of the index where dragging started to detect a self-drop in this.drop()
        // would have used dragStart, but it doesn't fire for tileLayout (does for tileGrid)
        if (this._dragStartIndex == null) this._dragStartIndex = currIndex;
        var tLeft, tTop, tile = this.getRecordTile(currIndex);
        //isc.logWarn('TL.showDragLine:' + [currIndex, this._dragStartIndex]);
      
        // make the cursor move to the next tile if the mouse is more than half way 
        // across the current tile
        if (tile != null) {
           if ((this.orientation == "horizontal" && x > tile.getLeft() + (tile.getVisibleWidth() / 2)) ||
               (this.orientation == "vertical" && y > tile.getTop() + (tile.getVisibleHeight() / 2))) {
                  currIndex++;
                  tile = this.getRecordTile(currIndex);
               }
        }
        // if no tile is moused over, make the index point to the end of the array and place
        // the tracker at the end of the last tile
        if (currIndex == null || tile == null) {
            currIndex = this.data.getLength();
            tile = this.getRecordTile(currIndex - 1);
            if (this.orientation == "horizontal") {
                tLeft = tile.getLeft() + tile.getVisibleWidth() - this._dragTrackerThickness;
                tTop = tile.getTop();
            } else {
                tLeft = tile.getLeft();
                tTop = tile.getTop() + tile.getVisibleHeight() - this._dragTrackerThickness;
            }     
        } else {
            tLeft = tile.getLeft();
            tTop = tile.getTop();     
        }
       
        // store this for drop
        this._lastDropIndex = currIndex;
       
        var lineWidth, lineHeight;
        if (this.orientation == "horizontal") {
            lineHeight = tile.getVisibleHeight();
            lineWidth = this._dragTrackerThickness;
        }
        else {
            lineWidth = tile.getVisibleWidth();
            lineHeight = this._dragTrackerThickness;
        }
                         
        this.showDragLine(xBase + (tLeft - this.getScrollLeft()), 
                    yBase + (tTop - this.getScrollTop()), lineWidth, lineHeight);
                
    }   
},

showDragLine : function (left, top, width, height) {
    
    // make sure the drag line is set up
	this.makeDragLine();
	// make sure the drag line isn't hanging over the top or bottom edges of 
	// the tilegrid. If it is, adjust the top and height params accordingly.
	
	// cache tilegrid visible height
	var visHeight = this.getVisibleHeight();
	// calculate the border around this tilegrid
	var border = Math.round((visHeight - this.getInnerHeight()) / 2); 	
	// cache the pageTop
	var pTop = this.getPageTop();
	// get the top and bottom y coords of the inner part of this tilegrid
	var gridTop = pTop + border;
	var gridBottom = pTop + visHeight - border;
	// if the dragline is sticking out above the grid, make the height = the 
	// amount its sticking out, and move it down so its aligned with the top edge 
	// of the tilegrid	
	if (top < gridTop) {	    	    
	    height = height - (gridTop - top);
	    top = gridTop;
	// otherwise if the dragline is sticking out below the tilegrid, truncate
	// the dragline so its within the grid borders
	} else if (top + height > gridBottom ) {
	    // border condition: if a record is dragged over this tilegrid and 
	    // we can reorder tiles, there are certain cases where the dragLine
	    // will placed completely below and outside of the grid. In this case,
	    // just make the tracker height 0 so it doesn't appear at all.
        if (top >= gridBottom) {           
            height = 0;
        } else {
            height = height - ((top + height) - gridBottom);
	    }
	}
	
    this._dragLine.moveTo(left, top);
    this._dragLine.resizeTo(width, height);
    this._dragLine.show();    
},

dropOut : function () {
    
    this.hideDragLine();
},

dropMove : function () {   
    this.showDragLineForRecord();
    
},

// returns the index of the first record whose tile left and top is greater than the passed in 
// left and top coords
findIndexForCoord : function (left, top) {
    // first see if we can use drawnStart/endIndex. This is the case for showAllRecords:false
    var start = this.getDrawnStartIndex(), end = this.getDrawnEndIndex();
    // obtain the ID of the drag target if it exists. It will be one of the 
    // tiles if dragAppearance = 'target' on the tile
    var eh = this.ns.EH;
    var dragTarg = eh.dragTarget, dragID;
    if (dragTarg) dragID = dragTarg.ID;
    for (var i = start; i < end; i++) {
        var tile = this.getRecordTile(i);
        if (!tile) continue;
        // skip the drag target, otherwise the only valid index will ever be
        // the start index of the target.
        if (tile.ID == dragID) continue;       
        if (tile.getLeft() + tile.getVisibleWidth() > left 
            && tile.getTop() + tile.getVisibleHeight() > top) return i;
    }
    
    return null;
},

//>	@method	tileLayout.drop()	(A)
//			handle a drop event
//		@return	(boolean)	true if the list can't reorder or dragging did not begin from the list body;
//							false if disabled, no selection, or otherwise
//		@group	events, dragging
//<
drop : function () {
    var index = this._lastDropIndex || 0;
    var dropRecords = this.ns.EH.dragTarget;
    var dragStartIndex = this._dragStartIndex;
    //isc.logWarn('dropped:' + index);
    // reset _dragStartIndex so the next drag will start over
    // NOTE can probably remove this code from here, as only TileGrid drop uses dragStartIndex
    // currrently. But leave it for now.
    this._dragStartIndex = null;
   
    if (!isc.isAn.Array(dropRecords)) dropRecords = [dropRecords];
    var fromCanvas = dropRecords[0].parentElement;
    
    var targetRecord = this.data.get(index);
    this.transferRecords(dropRecords, targetRecord, index, fromCanvas);
    this.reLayout();
},

//>	@method	tileLayout.transferDragData()
// @include dataBoundComponent.transferDragData()
//<

//>	@method	tileLayout.getDragData()
// @include dataBoundComponent.getDragData()
//<

//>	@attr	tileLayout.dragDataAction		
// @include dataBoundComponent.dragDataAction
//<

// --------------------------Animation-----------------------------------------------------

reLayout : function () {
    // prevent tileLayout in the process of destroying from setting timers and causing errors
    if (this.destroying) return;

    if (this.animateTileChange) {
        this.delayCall("_animateChange", null, 200);
    } else {
        this.logDebug('calling layoutTiles from reLayout', "TileLayout");
        this.layoutTiles();    
    }
},

_animateChange : function () {
    this.logDebug("starting _animateChange()", "TileLayout");
    // tell this.processTile() that we're animating, which will be called from tileGrid.layoutTiles() 
    this._animating = true;
    // keep track of animation ID's so they can be cancelled later if another dataChange event 
    // happens while an animation is going on.
    this._animationIDs = [];
    // hold on to the array of visibleTiles, as this._visibleTiles will get reset when 
    // we run layoutTiles() from here
    var visTiles = this._oldVisibleTiles = this._visibleTiles;
    // 1. calling layoutTiles will build an array of tiles that need to be animated (i.e. that were
    // visible before and are still visible now) by calling processTile(), and 
    // storing these tiles in this._tilesToAnimate
    this._tilesToAnimate = [];
    this.layoutTiles();    
    // 2. hide tiles pointing to data that will no longer be visible
    // check that visTiles exists, based on an alleged issue with tileGrid in a sectionStack
    if (visTiles != null) {
        for (var i = 0; i < visTiles.length; i++) {
            // if the currently visible tiles contain the old visible tiles, hide those tiles.
            // if we just hide them all without the check, there is a visible flash for tiles
            // that stay in the same place.
            if (!this._visibleTiles.contains(visTiles[i]))  {
                visTiles[i].hide();
            }
        }
    }
    // 3. animate the tiles that need to be moved
    var numTiles = this._tilesToAnimate.length;
    if (numTiles == 0) {
        this._finishAnimating();    
        return;
    }
    for (var i = 0; i < numTiles; i++) {
        var tile = this._tilesToAnimate[i];
        // see the comment below step 2, we need to show here because the tile may have been
        // hidden from a call to cleanupExtraTiles() within dataChanged().
        tile.show();
        // if last tile, pass a callback
        var animID;
        if (i == numTiles - 1) {
            animID = tile.animateMove(tile._newLeft, tile._newTop, this.getID() + "._finishAnimating()");     
        } else {
            animID = tile.animateMove(tile._newLeft, tile._newTop);    
        }
        // keep track of animations for later cancellation. see _layoutAfterDataChange()
        this._animationIDs.add({ID:animID, tile:tile});
    }
    
},

_finishAnimating : function () {
    // no longer animating
    this._animating = false;
    // clean up internal arrays
    this._oldVisibleTiles = null;
    delete this._oldVisibleTiles;
    this._tilesToAnimate = null;
    delete this._tilesToAnimate;
    // layout tiles 
    this.logDebug('calling layoutTiles from _finishAnimating', "TileLayout");
    this.layoutTiles();    
},

//> @method TileLayout.isAnimatingTileLayout()
// 
// Returns whether the TileLayout is currently performing animation as defined
// by its _animating flag.  Dinstinct from +link{Canvas.isAnimating()}.
// Useful for writing Selenium scripts that depend on operations completing.
//<
isAnimatingTileLayout : function () {
    return this._animating === true;
},

// -----------------------tileLayout-----------------------------------------------------------
//>	@method	tileLayout.setTileSize()	
// Sets the height and width of tiles.
// @param (Integer) size	
//
// @group tileLayout
// @visibility external
//<
setTileSize : function (size) {
    this.tileSize = size;
    this.layoutTiles();    
},

//>	@method	tileLayout.setTileWidth()	
// Sets the width of tiles.
// @param (Integer) width	
//
// @group tileLayout
// @visibility external
//<
setTileWidth : function (width) {
    this.tileWidth = width;
    this.layoutTiles();
},

//>	@method	tileLayout.setTileHeight()	
// Sets the height of tiles.
// @param (Integer) height	
//
// @group tileLayout
// @visibility external
//<
setTileHeight : function (height) {
    this.tileHeight = height;
    this.layoutTiles();
},

//> @method tileLayout.setTilesPerLine()
// Sets the number of tiles per line.
// @param tilesPerLine (Integer) New +link{tileLayout.tilesPerLine} value
// @group tileLayout
// @visibility external
//<
setTilesPerLine : function (tilesPerLine) {
    this.tilesPerLine = tilesPerLine;
    // re-run _setTileSize to recalculate default width/height if necessary
    this._setTileSize();
    this.layoutTiles();
    
},

//>	@method	tileLayout.setTileMargin()	
// Sets the vertical and horizontal margin of tiles.
// @param (Integer) margin	
//
// @group tileLayout
// @visibility external
//<
setTileMargin : function (margin) {
    this.tileMargin = margin;
    this.layoutTiles();
},

//>	@method	tileLayout.setTileHMargin()	
// Sets the horizontal margin of tiles.
// @param (Integer) width	
//
// @group tileLayout
// @visibility external
//<
setTileHMargin : function (margin) {
    this.tileHMargin = margin;
    this.layoutTiles();
},

//>	@method	tileLayout.setTileVMargin()	
// Sets the vertical margin of tiles.
// @param (Integer) width	
//
// @group tileLayout
// @visibility external
//<
setTileVMargin : function (margin) {
    this.tileVMargin = margin;
    this.layoutTiles();
}
});

//>	@class FlowLayout
// Arranges a set of Canvas components into rows, flowing into available space so that
// different numbers of components may appear in each row.
// <p>
// <code>FlowLayout</code> is essentially just a subclass of +link{TileLayout} where the
// default +link{tileLayout.layoutPolicy} is "flow" instead of "fit".
// 
// @see tileLayout.layoutPolicy
// @treeLocation Client Reference/Grids
// @visibility external
//<
isc.ClassFactory.defineClass("FlowLayout", "TileLayout");

isc.FlowLayout.addProperties({
        layoutPolicy: "flow"            
});        
