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

 






//>	@class	EdgedCanvas
// EdgedCanvas acts as a decorative, image-based frame around another single Canvas.
//
// @treeLocation Client Reference/Foundation
// @group imageEdges
// @visibility roundCorners
//<
isc.defineClass("EdgedCanvas", "Canvas").addProperties({

    // we don't want to redraw for any automatic reasons
    redrawOnResize:false,
    _redrawWithParent:false,
    _redrawWithMaster:false,

    // we do our own sizing, based on the master's margins
    _resizeWithMaster:false,

    // we don't need the extra structure, and it causes problems in Safari
    useClipDiv:false,

    
    overflow:isc.Browser.isMoz ? isc.Canvas.VISIBLE : isc.Canvas.HIDDEN,
    _useMozScrollbarsNone:false,

    // calculated margins based on visible corners and settings
    _leftMargin : 0,
    _topMargin : 0,
    _rightMargin : 0,
    _bottomMargin : 0,
    // all edges in cell order
    _allEdges : ["TL", "T", "TR", "L", "center", "R", "BL", "B", "BR"],
    _leftEdges : {L:true, TL:true, BL:true},
    _rightEdges : {R:true, TR:true, BR:true},
    // In RTL browsers tables are reversed so we need to flip our media too
    _allEdgesRTL : ["TR", "T", "TL", "R", "center", "L", "BR", "B", "BL"],

    // Edge Media
    // ---------------------------------------------------------------------------------------

    //> @attr edgedCanvas.skinImgDir   (URL : "images/edges/" : IR)
    // Standard skin directory for edge images (sides and corners).
    //
    // @group imageEdges
    // @visibility roundCorners
    //<
    skinImgDir:"images/edges/",

    //> @attr edgedCanvas.edgeImage   (SCImgURL : "[SKIN]/rounded/frame/FFFFFF/6.png" : IR)
    // Base name of images for edges.  Extensions for each corner or edge piece will be added
    // to this image URL, before the extension.  For example, with the default base name of
    // "edge.gif", the top-left corner image will be "edge_TL.gif".
    // <P>
    // The full list of extensions is: "_TL", "_TR", "_BL", "_BR", "_T", "_L", "_B", "_R",
    // "_center".
    //
    // @group imageEdges
    // @visibility roundCorners
    //<
    edgeImage:"[SKIN]/rounded/frame/FFFFFF/6.png",

    //> @attr edgedCanvas.edgeColor (CSSColor : null : IR)
    // CSS color (WITHOUT "#") for the edges.  If specified, will be used as part of image
    // names.  Example: "edge_88FF88_TL.gif".
    // @group imageEdges
    // @visibility roundCorners
    //<

    // Shown Edges
    // ---------------------------------------------------------------------------------------
    //> @attr edgedCanvas.customEdges (Array of String : null : IR)
    // Array of side names ("T", "B", "L", "R") specifying which sides of the decorated
    // component should show edges.  For example: <pre>
    //      customEdges : ["T", "B"]
    // </pre>
    // .. would show edges only on the top and bottom of a component.
    // <P>
    // The default of <code>null</code> means edges will be shown on all sides.
    //
    // @group imageEdges
    // @visibility roundCorners
    //<

    //> @attr edgedCanvas.showCenter (Boolean : false : IR)
    // Whether to show media in the center section, that is, behind the decorated Canvas.
    //
    // @group imageEdges
    // @visibility roundCorners
    //<

    //> @attr edgedCanvas.shownEdges   (Object : [all edges] : IR)
    // The corners and edges which should appear outside the contained Canvas.
    // <P>
    // Any combination is permitted so long as every visible edge piece has two visible
    // adjacent corners.
    // <P>
    // Some examples:<ul>
    // <li>cap on top: TL, T, TR
    // <li>cap on top and bottom: TL, T, TR, BL, B, BR
    // <li>3 rounded corners: TR, R, BR, B, BL
    // <li>4 rounded corners, contained Canvas flush with bottom: TL, T, TR, L, R, BL, BR
    // </ul>
    //<
    // NOTE: not documented for now in favor of simpler customEdges interface

    
    shownEdges : {
            TL:true,
            T:true,
            TR:true,
            L:true,
            R:true,
            BL:true,
            B:true,
            BR:true
    },

    // Edge Sizing
    // ---------------------------------------------------------------------------------------

    //> @attr edgedCanvas.edgeSize (integer : 6 : IR)
    // Size in pixels for corners and edges
    // @group imageEdges
    // @visibility roundCorners
    //<
    edgeSize:6

    //> @attr edgedCanvas.edgeLeft     (integer : null : IR)
    // Height in pixels for left corners and edges.  Defaults to edgeSize when unset.
    // @group imageEdges
    // @visibility roundCorners
    //<
    
    //> @attr edgedCanvas.edgeRight    (integer : null : IR)
    // Height in pixels for right corners and edges.  Defaults to edgeSize when unset.
    // @group imageEdges
    // @visibility roundCorners
    //<
    
    //> @attr edgedCanvas.edgeTop      (integer : null : IR)
    // Height in pixels for top corners and edges.  Defaults to edgeSize when unset.
    // @group imageEdges
    // @visibility roundCorners
    //<
    
    //> @attr edgedCanvas.edgeBottom   (integer : null : IR)
    // Height in pixels for bottom corners and edges.  Defaults to edgeSize when unset.
    // @group imageEdges
    // @visibility roundCorners
    //<

    // Overlapping the Edges
    // ---------------------------------------------------------------------------------------

    //> @attr edgedCanvas.edgeOffset       (integer : null : [IRA])
    // Amount the contained Canvas should be offset.  Defaults to edgeSize; set to less than
    // edgeSize to allow the contained Canvas to overlap the edge and corner media. 
    // @group imageEdges
    // @visibility roundCorners
    // @example edges
    //<

    //> @attr edgedCanvas.edgeOffsetTop    (integer : null : [IRA])
    // Amount the contained Canvas should be offset from the top.  Defaults to the size for 
    // the top edge.  Set smaller to allow the contained Canvas to overlap the edge and 
    // corner media. 
    // @group imageEdges
    // @visibility roundCorners
    //<
    
    //> @attr edgedCanvas.edgeOffsetBottom (integer : null : [IRA])
    // Amount the contained Canvas should be offset from the bottom.  Defaults to the size for
    // the bottom edge.  Set smaller to allow the contained Canvas to overlap the edge and
    // corner media. 
    // @group imageEdges
    // @visibility roundCorners
    //<
    
    //> @attr edgedCanvas.edgeOffsetLeft   (integer : null : [IRA])
    // Amount the contained Canvas should be offset from the left.  Defaults to the size for
    // the left edge.  Set smaller to allow the contained Canvas to overlap the edge and
    // corner media. 
    // @group imageEdges
    // @visibility roundCorners
    //<
    
    //> @attr edgedCanvas.edgeOffsetRight  (integer : null : [IRA])
    // Amount the contained Canvas should be offset from the right.  Defaults to the size for
    // the right edge.  Set smaller to allow the contained Canvas to overlap the edge and
    // corner media. 
    // @group imageEdges
    // @visibility roundCorners
    //<
    
    // ---------------------------------------------------------------------------------------

    //> @attr edgedCanvas.centerBackgroundColor (CSSColor : null : IR)
    // Background color for the center section only.  Can be used as a surrogate background
    // color for the decorated Canvas, if the Canvas is set to partially overlap the edges and
    // hence can't show a background color itself without occluding media.
    // @group imageEdges
    // @visibility roundCorners
    //<

    
});

isc.EdgedCanvas.addMethods({



initWidget : function () {
    this.invokeSuper(isc.EdgedCanvas, this._$initWidget);

    // NOTE: for simplicity, we always generate the same DOM structure.  The "shownEdges"
    // structure indicates which cells will have media, hence where margins have to be in order
    // to reveal the structure.

	// HACK 040913: friendlier public interface
	//	specify an array of sides to get the edge treatment
	//	reasons: shorter code, consistent with clippedCorners, avoids naming problem (ie that
	//	'edges' implies sides, not sides and corners), and enforces the current limitation that
	//	corners MUST be displayed for each side
    var customEdges = this.customEdges;
	if (customEdges) {
		var edges = this.shownEdges = {};
		if (customEdges.contains("T")) {
			edges.T = edges.TL = edges.TR = true;
		}
		if (customEdges.contains("B")) {
			edges.B = edges.BL = edges.BR = true;
		}
		if (customEdges.contains("L")) {
			edges.L = edges.TL = edges.BL = true;
		}
		if (customEdges.contains("R")) {
			edges.R = edges.TR = edges.BR = true;
		}
	}

    this.updateEdgeSizes();
},

updateEdgeSizes : function () {

    var edgeSize = this.edgeSize;
    
    this._leftEdge = this._firstNonNull(this.edgeLeft, edgeSize);
    this._rightEdge = this._firstNonNull(this.edgeRight, edgeSize);
    this._topEdge = this._firstNonNull(this.edgeTop, edgeSize);
    this._bottomEdge = this._firstNonNull(this.edgeBottom, edgeSize);

    // whether to show margin on side: rail with media on side always indicates a margin -
    // otherwise that rail could not possibly be shown.  Corner on side does not
    // necessarily indicate a margin, eg, in endcap case (tl t tr only), no right or left
    // margin.
    // Margins can be set independently of edges to allow the Canvas to overlap the edges.
    // NOTE: these margin settings are automatically picked up by the Canvas that owns us, and
    // added to its own margin settings to produce the native margin settings.
    var edges = this.shownEdges,
        marginSize = this.edgeOffset;
    if (edges.L) this._leftMargin = 
        this._firstNonNull(this.edgeOffsetLeft, marginSize, this._leftEdge);
    if (edges.R) this._rightMargin =
        this._firstNonNull(this.edgeOffsetRight, marginSize, this._rightEdge);
    if (edges.T) this._topMargin = 
        this._firstNonNull(this.edgeOffsetTop, marginSize, this._topEdge);
    if (edges.B) this._bottomMargin = 
        this._firstNonNull(this.edgeOffsetBottom, marginSize, this._bottomEdge);

    this.markForRedraw(); // in case we're drawn
},

getInnerWidth : function (a,b,c) {
    var width = this.invokeSuper(isc.EdgedCanvas, "getInnerWidth", a,b,c);
    return width - this._leftMargin - this._rightMargin;
},

getInnerHeight : function (a,b,c) {
    var height = this.invokeSuper(isc.EdgedCanvas, "getInnerHeight", a,b,c);
    return height - this._topMargin - this._bottomMargin;
},

_emptyCellStart : "<TD class='",
_emptyCellEnd : "' ></TD>",

getInnerHTML : function () {

    // For minimum performance impact, we want to write HTML for the corners that
    // automatically reflows on resize.
    //
    // Most published methods of doing this rely on embedding content inside a containing
    // element with the borders arrayed around the edges, where the containing element
    // vertically stretches to accommodate content.  We can't feasibly use that approach
    // because abs pos content doesn't stretch the containing element, so we'd have to
    // force all Canvas children to be relatively positioned, so layout code would have to
    // take into account that each Canvas starts out offset by all previous Canvii - a
    // mess.  
    //
    // So we need to write reflowable HTML that will simply fill its container.  This is
    // easy with a table; with CSS the only difficult aspect is writing the center pieces
    // so that they stretch.  Approaches:
    // - classic CSS 3 column layout: float or abs pos left and right pieces, put margins
    //   on center piece.
    //   - relies on the fact that a block-level element expands to fill horizontal space
    //     minus margins.  There is no analogous vertical technique; 100% height produces a
    //     DIV that's as tall as the container including margins
    // - use both a right and left coordinate, or top and bottom coordinate, to imply size
    //   - works in Moz.  IE6 respects right coordinate but not right with left (likewise
    //     bottom and top)
    //
    // A TABLE is the only way we know of to do this in IE6, with various workarounds the table
    // can be made to work on all platforms, and the HTML is much shorter.
    var output = isc.SB.create(),
        // image names
        baseURL = this.edgeImage,
        period = baseURL.lastIndexOf(isc.dot),
        name = baseURL.substring(0, period),
        extension = baseURL.substring(period),
        urlStart = this.getImgURL(name),
        cellStart = "<TD HEIGHT=",
        nonHeightStart = "<TD",
        mediaStart, mediaEnd,
        cellEnd;

    // setup cell HTML
    if (!isc.Browser.isIE10 && 
        !((isc.Browser.isStrict && isc.Browser.isIE && isc.Browser.version >= 8) 
          || (isc.Browser.isMoz && isc.Browser.isUnix)
          )
        ) 
    {
        
    //if (isc.Browser.isSafari || 
    //    (isc.Canvas._fixPNG() && extension.toLowerCase() == ".png") || isc.Browser.isMoz) {
        // NOTE: on IE5.5+, calling imgHTML kicks in the PNG alpha loading workaround

        var imgProps = isc.EdgedCanvas._imgProps;
        if (!imgProps) {
            imgProps = isc.EdgedCanvas._imgProps = {
                width:"100%",
                height:"100%"
            };
            // Safari requires align != "texttop", otherwise each cell sizes to a minimum of a
            // text line height
            if (isc.Browser.isSafari) imgProps.align = "middle";
            // In Strict mode write images out as explicit display:block
            // This avoids a well documented issue where images inside table cells leave gaps
            // under them in strict mode 
            if (isc.Browser.isStrict && !isc.Browser.isTransitional) {
                imgProps.extraCSSText = "display:block";
            }
            
            if (isc.Browser.isTouch) {
                imgProps.extraCSSText = ((imgProps.extraCSSText == null ? "" : imgProps.extraCSSText + ";") +
                                         "-webkit-touch-callout:none");
                imgProps.extraStuff = " oncontextmenu='javascript:return false;'";
            }
        }
        imgProps.src = baseURL;
        var imgHTML = this.imgHTML(imgProps);
        // In Safari (3.1.2 on Windows), 100% sized images will stretch correctly but will not
        // shrink below their native size -- enclose in a clip-div to handle this
        if (isc.Browser.isSafari) {
            imgHTML = "<DIV style='overflow:hidden;width:100%;height:100%'>" + imgHTML + "</DIV>";
        }
        var dotIndex = imgHTML.lastIndexOf(isc.dot);
        mediaStart = this._$rightAngle + imgHTML.substring(0, dotIndex);
        mediaEnd = imgHTML.substring(dotIndex); 
        cellEnd = "</TD>";

        // NOTE: IE native drawing problems using PNG alpha filter directly on table cell: 
        // HTML is correct, reported and visible sizes for cells are correct, but some cells,
        // at random, stretch PNG only partway across cell.  Meaningless native changes, like
        // setting a cell border to "" when it is already "", will correct some cells but not
        // others.
        //cellStart = "<TD STYLE='filter:progid:DXImageTransform.Microsoft.AlphaImageLoader(src=\""
        //    + urlStart + "_";
        //cellEnd = extension + "\",sizingMethod=\"scale\");'></TD>";
    } else {
    
         
        mediaStart = " STYLE='background:url(" + urlStart;
        mediaEnd = extension + ");"
        cellEnd = "'></TD>";
    }
    
    // incorporate edgeColor into image name if specified
    if (this.edgeColor) mediaStart += isc._underscore + this.edgeColor;

    var edges = this.shownEdges;

    
    var isRTL = this.isRTL(),
        leftEdge = isRTL ? this._rightEdge : this._leftEdge,
        rightEdge = isRTL ? this._leftEdge : this._rightEdge;
    output.append(this._$edgeTableStart,
                  leftEdge, this._$edgeColMid, rightEdge, this._$rightAngle,
                  "<TR HEIGHT=", this._topEdge, this._$rightAngle);
                  
    this._writeEdgeCells(cellStart, this._topEdge, isc.px, mediaStart, mediaEnd, cellEnd, 0, 2, edges, output);

    var middleRowHeight = this.getHeight() - this._topEdge - this._bottomEdge;
    if (isc.Browser.isStrict && (isc.Browser.isIE || isc.Browser.isOpera)) {
        // write explicit size for IE strict, which will be updated on resize
        output.append("</TR><TR HEIGHT=", middleRowHeight, ">");
    } else {
        output.append("</TR><TR>");
    }

    if (isc.Browser.isMoz || isc.Browser.isWebKit) {
        this._writeEdgeCells(cellStart, middleRowHeight, isc.px, mediaStart, mediaEnd, cellEnd, 3, 5, edges, output);
    } else {
        this._writeEdgeCells(cellStart, null, isc.px, mediaStart, mediaEnd, cellEnd, 3, 5, edges, output);
    }

    output.append("</TR><TR HEIGHT=", this._bottomEdge, ">");

    this._writeEdgeCells(cellStart, this._bottomEdge, isc.px, mediaStart, mediaEnd, cellEnd, 6, 8, edges, output);

    output.append("</TR></TABLE>");

    //this.logWarn("output: " + output.toString());

    return output.release(false);
},

_$edgeTableStart : "<TABLE role='presentation' CELLPADDING='0' CELLSPACING='0' "
                        + "STYLE='height:100%;width:100%;table-layout:fixed'>"
                        + "<COL WIDTH=",
_$edgeColMid : "><COL><COL WIDTH=", 
_$bgSizeArr:["background-size:", , ," ", , ,";"],

// params:
// cellStart/cellEnd: HTML for non-empty cells; edgeName will be inserted between
// start/end: range of edge names to output, inclusive
// edges: map of edges that will actually be output (otherwise, empty cell used)
// output: StringBuffer to append to
_$classEquals:" class=",
_writeEdgeCells : function (cellStart, height, heightUnit, mediaStart, mediaEnd, cellEnd, start, end, edges, output) {

    var allEdges = this.isRTL() ? this._allEdgesRTL : this._allEdges;
    
        
    var sizeBackgroundImage = isc.Browser.isIE9;
    
    for (var i = start; i <= end; i++) {
        var edgeName = allEdges[i],
            backgroundSizingCSS  = null;
        if (sizeBackgroundImage && height != null) {
            if (this._leftEdges[edgeName]) {
                this._$bgSizeArr[1] = this._leftEdge;
                this._$bgSizeArr[2] = isc.px;
                this._$bgSizeArr[4] = height;
                this._$bgSizeArr[5] = heightUnit;
                backgroundSizingCSS = this._$bgSizeArr.join(isc.emptyString);
            } else if (this._rightEdges[edgeName]) {
                this._$bgSizeArr[1] = this._rightEdge;
                this._$bgSizeArr[2] = isc.px;
                this._$bgSizeArr[4] = height;
                this._$bgSizeArr[5] = heightUnit;
                backgroundSizingCSS = this._$bgSizeArr.join(isc.emptyString);
            } else if (edgeName != isc.Canvas.CENTER) {
                this._$bgSizeArr[1] = 100;
                this._$bgSizeArr[2] = this._$percent;
                this._$bgSizeArr[4] = height;
                this._$bgSizeArr[5] = heightUnit;
                backgroundSizingCSS = this._$bgSizeArr.join(isc.emptyString);
            }
        }

        var styleName = this.getEdgeStyleName(edgeName),
            classEquals = styleName ? this._$classEquals : null;

        if (edges[edgeName] || (this.showCenter && edgeName == isc.Canvas.CENTER)) {
            if (height != null) {
                output.append(cellStart, height, heightUnit);
            } else {
                output.append(cellStart, "''");
            }
            if (isc.Browser.isOpera) {
                
                if (edgeName == "TL" || edgeName == "L" || edgeName == "BL") {
                    output.append(" width=", this._leftEdge);
                } else if (edgeName == "TR" || edgeName == "R" || edgeName == "BR") {
                    output.append(" width=", this._rightEdge);
                }
            }
            output.append(classEquals, styleName,
                          mediaStart, this.getEdgePrefix(edgeName), 
                          isc._underscore, edgeName,
                          // mediaEnd / backgroundSizingCSS will be null if not
                          // required.
                          mediaEnd, backgroundSizingCSS,
                          cellEnd);
        } else {
            if (this.centerBackgroundColor && edgeName == isc.Canvas.CENTER) {
                output.append("<TD ", classEquals, styleName, " style='background-color:",
                             this.centerBackgroundColor, "'></TD>");
            } else {
                output.append(this._emptyCellStart, classEquals, styleName,
                              this._emptyCellEnd);
            }
        }
    }
},


//> @attr EdgedCanvas.edgeStyleName (CSSStyleName : null : IRW)
// Optional property specifying the CSS ClassName to apply to the various parts of this edged canvas
// (top, bottom, corners, sides and center). To
// apply separate styles for each part, use +link{EdgedCanvas.addEdgeStyleSuffix}.
// @group imageEdgeStyles
// @group imageEdges
// @visibility external
//<

//> @attr EdgedCanvas.addEdgeStyleSuffix (Boolean : false : IRW)
// If specified, the +link{EdgedCanvas.edgeStyleName} will be treated as a base style name and
// appended with following suffixes to support separate styling per cell:
// <P>
// <code>_TL</code> (top left cell)<br>
// <code>_T</code> (top center cell)<br>
// <code>_TR</code> (top right cell)<br>
// <code>_L</code> (middle left cell)<br>
// <code>_C</code> (center cell)<br>
// <code>_R</code> (middle right cell)<br>
// <code>_BL</code> (bottom left cell)<br>
// <code>_B</code> (bottom center cell)<br>
// <code>_BR</code> (bottom right cell)
// @group imageEdgeStyles
// @group imageEdges
// @visibility external
//<
addEdgeStyleSuffix:false,


getEdgeStyleName : function (edge) {
    if (this.edgeStyleName == null) return;
    if (!this.addEdgeStyleSuffix) return this.edgeStyleName;
    
    if (!this._$edgeCellStyleMap || this._$edgeCellStyleMap.base != this.edgeStyleName) {
        var baseStyle = this.edgeStyleName;
        this._$edgeCellStyleMap = {
            base:baseStyle,
            TL:baseStyle + "_TL",
            T :baseStyle + "_T",
            TR:baseStyle + "_TR",
            L: baseStyle + "_L",
            C: baseStyle + "_C",
            R: baseStyle + "_R",
            BL:baseStyle + "_BL",
            B: baseStyle + "_B",
            BR:baseStyle + "_BR"
        }
    }
    return this._$edgeCellStyleMap[edge];
},

// this is essentially here to allow "depth" to be inserted by dropShadows, so not documented
// for now
getEdgePrefix : function (edgeName) { },

_handleResized : function () {
        
    if (!this.isDrawn() || this._suppressReactToResize) return;

    // HACK: very odd - without this, the bottom edge gets chopped off in Opera.  Using the IE
    // strict path below fixes initial rendering of overflowing edged canvases, but if the
    // canvas is subsequently resized, the bottom edge gets chopped off.
    if (isc.Browser.isOpera) {
        this.masterElement.bringToFront();
        return;
    }

    // update middle row for IE strict
    
    if (isc.Browser.isIE && isc.Browser.isStrict) {
        var middleRow = this.getHandle().firstChild.rows[1];
        this._assignSize(middleRow.style, this._$height, 
                         this.getHeight() - this._topEdge - this._bottomEdge);
        return;
    }

    
    if (isc.Browser.isWebKit) {
        var middleRow = this.getHandle().firstChild.rows[1];
        var rowHeight = Math.max(0,this.getHeight() - this._topEdge - this._bottomEdge);
        for (var i = 0; i < middleRow.cells.length; i++) {
            //this.logWarn("assigning size to cell: " + 
            //    (this.getHeight() - this._topEdge - this._bottomEdge) +
            //    " to: " + this.echoDOM(middleRow.cells[i]));
            this._assignSize(middleRow.cells[i].style, this._$height, 
                             rowHeight);
        }
        return;
    }

    if (!isc.Browser.isMoz) return;

    // NOTE: Moz issues: even after being coddled with the unnecessary overflow:visible setting it
    // likes, and some redundant size specifications that avoid it borking itself on move, Moz
    // still needs one final smack to the head to get the size right: if there's media in the
    // central cell, Moz won't allow the table to get shorter than it is wide (wider than it is
    // tall is OK).  Setting a height on the central cell fixes this.  This needs to be done
    // after draw, redraw or resize.
    var middleCell = this.getHandle().firstChild.rows[1].cells[1],
        height = this.getHeight() - this._topEdge - this._bottomEdge;
    // don't assign to negative size - moz just ignores this        
    if (height < 0) height = 0;
    //this.logWarn("assigning: " + height +
    //             " to " + this.echoLeaf(middleCell));
    this._assignSize(middleCell.style, this._$height, height);
    
    if (this.forceMozRowHeight) {
        var cells = middleCell.parentNode.cells;
        this._assignSize(cells[0].style, this._$height, height);
        this._assignSize(cells[2].style, this._$height, height);
    }
    
},
forceMozRowHeight:true,

layoutChildren : function (a,b,c) {
    var children = this.children;
    if (!children) return;

    
    isc.Canvas._instancePrototype.layoutChildren.call(this, a,b,c);    
    //this.Super("layoutChildren", arguments);

    if (children.length == 0) return;

    var child = children[0];
    child.setRect(this._leftMargin, this._topMargin,
                  this.getInnerWidth(),
                  this.getInnerHeight());
},

addChild : function (a,b,c) {
    
    isc.Canvas._instancePrototype.addChild.call(this, a,b,c);    
    //this.Super("addChild", arguments);
    this.layoutChildren("addChild");
},

draw : function (a,b,c,d) {

    if (!this.readyToDraw()) return this;
    // if we're acting as a peer, size now, since our master has drawn and sizes are available
    if (this.masterElement) {
        // if we, or one of our ancestors is relatively positioned, and the page
        // is not loaded, we likely have a native DOM ancestor that is not yet complete
        // (EG an open table cell tag).
        // This makes our reported sizing be incorrect. Catch this case and explicitly 
        // call fitToMaster() on page load
        var me = this.masterElement,
            hasRelativeParent = false;
        while (me) {
            if (me.position == this._$relative) {
                hasRelativeParent = true;
                break;
            }
            me = me.parentElement;
        }
        if (!hasRelativeParent || isc.Page.isLoaded()) this.fitToMaster();
        else isc.Page.setEvent("load", this, isc.Page.FIRE_ONCE, "fitToMaster");
    }
    
    this.invokeSuper(isc.EdgedCanvas, "draw", a,b,c,d);
    this._handleResized(); // for Moz
    return this;
},


fitToMaster : function () {

    if (this.destroyed) return;
    
    var master = this.masterElement;

    // We want to ensure the master element's size is fully up to date.
    // If we're pending the initial adjustOverflow because the handle isn't fully rendered,
    // wait for it to complete and re-run this method.
    if (master._delayedAdjustOverflow) {
        isc.Timer.setTimeout({target:this, methodName:"fitToMaster"}, 200);
        return;
    // Otherwise if markForAdjustOverflow() was called, just adjustOverflow now.        
    } else if (master._overflowQueued) {
        master.adjustOverflow();
    }
    
    var margins = master._getSpecifiedMargins();
            
    this.setRect( master.getOffsetLeft() + margins.left,
                  master.getOffsetTop() + margins.top,
                  Math.max(1, (master.getVisibleWidth() - margins.left - margins.right)),
                  Math.max(1, (master.getVisibleHeight() - margins.top - margins.bottom))
    );

},

redraw : function () {
    this.Super("redraw", arguments);
    this._handleResized(); // for Moz
    return this;
},

// when the master resizes, resize the edgedCanvas to fit the interior of this widgets *specified*
// margins
masterResized : function () {
    var master = this.masterElement;
    //>Animation If the master is animated show-ing / hide-ing, bail. We handle this case 
    // specially in animated show/hide.
    if (this.masterElement.isAnimating([this._$show, this._$hide])) return;
    //<Animation
    
    
    var delay = isc.Browser.isSafari;
    
    if (delay) {
        this.delayCall("_sizeToMaster", [master]);
    } else {
        this._sizeToMaster(master);
    }
},

_sizeToMaster : function (master) {
    if (this.destroyed || master != this.masterElement) return;
    
    var margins = master._getSpecifiedMargins();

    this.resizeTo(Math.max(1, master.getVisibleWidth() - margins.left - margins.right),
                  Math.max(1, master.getVisibleHeight() - margins.top - margins.bottom));
},

// ---------------------------------------------------------------------------------------
setEdgeImage : function (edgeImage) {
    if (this.edgeImage == edgeImage) return;
    this.edgeImage = edgeImage;
    this.markForRedraw("setEdgeImage");
}

});

isc.defineClass("DropShadow", "EdgedCanvas").addProperties({
    _cosmetic: true,

    skinImgDir:"images/shared/shadows/",
    edgeImage:"[SKIN]ds.png",

    // never occlude anything in terms of events
    isMouseTransparent:true,

    // this is a bit redundant, but we can't simply flip on edges.center without affecting the
    // superclass structure
    shownEdges : {
            center:true,
            TL:true,
            T:true,
            TR:true,
            L:true,
            R:true,
            BL:true,
            B:true,
            BR:true
    },

    depth:4,

    initWidget : function () {
        this.setDepth(this.depth);
        this.Super(this._$initWidget);
    },
    
    setDepth : function (depth) {
        if (depth != null) this.depth = depth;
    
        var newSoftness = (this.softness || this.depth),
            softnessChange = this._softness != newSoftness;

        // auto-derive softness and offset from depth if not specified.  Use separate
        // properties so depth can be changed later and we won't think the properties were
        // developer-set
        this._softness = newSoftness;
        this.edgeSize = 2*this._softness;

        var defaultOffset = this._firstNonNull(this.offset, Math.round(this.depth/2));
        this._offsetX = this._firstNonNull(this.offsetX, defaultOffset);
        this._offsetY = this._firstNonNull(this.offsetY, defaultOffset);

        // XXX we need to call this to take into account new margin settings, but currently
        // this *always* marks for redraw
        this.updateEdgeSizes();

        // depth change implies a move, resize, and redraw
        if (this.isDrawn()) {
            this.masterMoved();
            // softness change means new media, so redraw
            
            if (softnessChange || this.isDirty()) this.redraw();
            this.masterResized();
        }
    },

    // add a center segment, which doesn't use the "depth" as a prefix
    getEdgePrefix : function (edgeName) {   
        if (edgeName != isc.Canvas.CENTER) return this._softness;
    },

    // NOTE: fires on draw, and whenever the master moves
    masterMoved : function () {
        var hidden = this.visibility == isc.Canvas.HIDDEN,
            master = this.masterElement,
            left = master.getOffsetLeft(),
            top = master.getOffsetTop();
        if (!hidden) {
            left += this._offsetX - this._softness;
            top += this._offsetY - this._softness;
        }
        this.moveTo(left, top);
    },

    // NOTE: fires on draw, and whenever the master resizes
    masterResized : function () {
        
        // when we're hidden, we're always rendered at size 1x1 so we don't take up any space
        if (this.visibility == isc.Canvas.HIDDEN) return;
        
        var master = this.masterElement;
        //this.logWarn("masterResized called, master size: " + 
        //             [master.getVisibleWidth(), master.getVisibleHeight()]
                     // + this.getStackTrace()
        //             );
        this.resizeTo(master.getVisibleWidth() + 2*this._softness,
                      master.getVisibleHeight() + 2*this._softness);        
    },
    
    // Make 'fitToMaster' fall through to masterMoved(); masterResized()
    
    fitToMaster : function () {
        this.masterMoved();
        this.masterResized();
    },
    
    // Ensure that when hidden the shadow doesn't take up any space
    // Use setVisibility rather than show()/hide() as this method may be called directly
    setVisibility : function (vis, a,b,c,d) {
        var changed = (vis != this.visibility);
        this.invokeSuper(isc.DropShadow, "setVisibility", vis,a,b,c,d);
        
        if (changed) {
            if (vis == isc.Canvas.HIDDEN) {
                this.resizeTo(1,1);
                var master = this.masterElement;
                this.moveTo(master.getOffsetLeft(), master.getOffsetTop());
            } else {
                this.fitToMaster();
            }
        }
    },

    // dragResizeFromShadow behavior: dynamically set canDragResize based on the master's
    // canDragResize setting.
    getCurrentCursor : function (a,b,c,d) {
        var master = this.masterElement;
        if (master && master.dragResizeFromShadow) this.canDragResize = master.canDragResize;
        return this.invokeSuper(isc.DropShadow, "getCurrentCursor", a,b,c,d);
    },
    prepareForDragging : function (a,b,c,d) {
        var master = this.masterElement;
        if (master && master.dragResizeFromShadow) this.canDragResize = master.canDragResize;
        return this.invokeSuper(isc.DropShadow, "prepareForDragging", a,b,c,d);
    },
    
    // Avoid recursive shadows 
    showShadow:false

});



