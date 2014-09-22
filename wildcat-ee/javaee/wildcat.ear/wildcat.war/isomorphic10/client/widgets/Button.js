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

 





//>	@class	Button
//
// The Button widget class implements interactive, style-based button widgets.
//
// @treeLocation Client Reference/Control
// @visibility external
//<

//> @groupDef buttonIcon
// Control over optional icons shown in Buttons, Labels and other contexts
// @title Button Icon
// @visibility external
//<

isc.ClassFactory.defineClass("Button", "StatefulCanvas");

isc.defer("if (isc.Button._instancePrototype.showFocused == null) isc.Button.addProperties({ showFocused: !isc.Browser.isTouch });");

isc.Button.addProperties({


    // Various properties documented on StatefulCanvas that affect all buttons
    // NOTE: This block is included in Button, ImgButton, and StrechlImgButton.
    //       If you make changes here, make sure you duplicate it to the other
    //       classes.
    // 
    // End of this block is marked with: END StatefulCanvas @include block
    // ========================================================================

    // Title
    //------
    //> @attr button.title (HTMLString : "Untitled Button" : IRW)
    // @include statefulCanvas.title
    // @visibility external
    // @group basics
    // @group i18nMessages
    // @example buttonStates
    //<
	title:"Untitled Button",

    //> @attr button.clipTitle (Boolean : true : IR)
    // If this.overflow is "hidden" and the title for this button is too large for the available
    // space, should the title be clipped by an ellipsis?
    // <p>
    // This feature is supported only in browsers that support the CSS UI text-overflow
    // property (IE6+, Firefox 7+, Safari, Chrome, Opera 9+).
    //<
    clipTitle: true,

    //> @attr button.hiliteAccessKey    (boolean : null : IRW)
    // @include statefulCanvas.hiliteAccessKey
    // @visibility external
    //<    

    //>	@method	button.getTitle()	(A)
    // @include statefulCanvas.getTitle
    // @visibility external
    //<
    //>	@method	button.setTitle()
    // @include statefulCanvas.setTitle
    // @visibility external
    //<

    //> @attr button.showClippedTitleOnHover (Boolean : false : IRW)
    // If true and the title is clipped, then a hover containing the full title of this button
    // is enabled.
    // @group hovers
    // @visibility external
    //<
    showClippedTitleOnHover:false,

    _canHover:true,

    // don't set className on the widget's handle, because we apply styling to another element
    suppressClassName:true,

    // Icon
    //------

    // set useEventParts to true so we can handle an icon click separately from a
    // normal button click if we want
    useEventParts:true,

    //> @attr button.icon
    // @include statefulCanvas.icon
    // @visibility external
    // @example buttonIcons
    //<
    //> @attr button.iconSize
    // @include statefulCanvas.iconSize
    // @visibility external
    //<
    //> @attr button.iconWidth
    // @include statefulCanvas.iconWidth
    // @visibility external
    //<
    //> @attr button.iconHeight
    // @include statefulCanvas.iconHeight
    // @visibility external
    //<
    //> @attr button.iconStyle
    // @include StatefulCanvas.iconStyle
    // @visibility external
    //<
    //> @attr button.iconOrientation
    // @include statefulCanvas.iconOrientation
    // @visibility external
    // @example buttonIcons
    //<
    //> @attr button.iconAlign
    // @include statefulCanvas.iconAlign
    // @visibility external
    //<
    //> @attr button.iconSpacing
    // @include statefulCanvas.iconSpacing
    // @visibility external
    //<
    //> @attr button.showDisabledIcon
    // @include statefulCanvas.showDisabledIcon
    // @visibility external
    //<
    //> @attr button.showRollOverIcon
    // @include statefulCanvas.showRollOverIcon
    // @visibility external
    //<
    //> @attr button.iconCursor
    // @include StatefulCanvas.iconCursor
    // @visibility external
    //<
    //> @attr button.disabledIconCursor
    // @include StatefulCanvas.disabledIconCursor
    // @visibility external
    //<

    //> @attr button.showFocusedIcon
    // @include statefulCanvas.showFocusedIcon
    // @visibility external
    //<
    //> @attr button.showDownIcon
    // @include statefulCanvas.showDownIcon
    // @visibility external
    // @example buttonIcons
    //<
    //> @attr button.showSelectedIcon
    // @include statefulCanvas.showSelectedIcon
    // @visibility external
    //<
    //> @method button.setIconOrientation()
    // @include statefulCanvas.setIconOrientation
    // @visibility external
    //<
    //> @method button.setIcon()
    // @include statefulCanvas.setIcon
    // @visibility external
    //<

    // AutoFit
    //--------
    //> @attr button.autoFit
    // @include statefulCanvas.autoFit
    // @group sizing
    // @visibility external
    // @example buttonAutoFit
    //<    
    //> @method button.setAutoFit()
    // @include statefulCanvas.setAutoFit
    // @visibility external
    //<
    
    // only autoFit horizontally by default
    autoFitDirection:"horizontal",

    // baseStyle
    //----------
    //> @attr button.baseStyle (CSSStyleName : "button" : IRW)
    // @include statefulCanvas.baseStyle
    // @visibility external
    //<    
	baseStyle:"button",
    //> @method button.setBaseStyle()
    // @include statefulCanvas.setBaseStyle
    // @visibility external
    //<

    // selection
    //----------
    //> @attr button.selected
    // @include statefulCanvas.selected
    // @visibility external
    //<   
    //> @method button.select()
    // @include statefulCanvas.select
    // @visibility external
    //<
    //> @method button.deselect()
    // @include statefulCanvas.select
    // @visibility external
    //<
    //> @method button.isSelected()
    // @include statefulCanvas.isSelected
    // @visibility external
    //<
    //> @method button.setSelected()
    // @include statefulCanvas.select
    // @visibility external
    //<

    // radioGroup
    //-----------
    //> @attr button.radioGroup
    // @include statefulCanvas.radioGroup
    // @visibility external
    // @example buttonRadioToggle
    //<     
    //> @method button.addToRadioGroup()
    // @include statefulCanvas.addToRadioGroup
    // @visibility external
    //<
    //> @method button.removeFromRadioGroup()
    // @include statefulCanvas.removeFromRadioGroup
    // @visibility external
    //<
    //> @attr button.actionType
    // @include statefulCanvas.actionType
    // @visibility external
    // @example buttonRadioToggle
    //<     
    //> @method button.setActionType()
    // @include statefulCanvas.setActionType
    // @visibility external
    //<
    //> @method button.getActionType()
    // @include statefulCanvas.getActionType
    // @visibility external
    //<

    // state
    //------
    //> @attr button.state
    // @include statefulCanvas.state
    // @visibility external
    //<  
    //> @method button.setState()
    // @include statefulCanvas.setState
    // @visibility external
    //<
    //> @method button.setDisabled()
    // @include statefulCanvas.setDisabled
    // @visibility external
    //<
    //> @method button.getState()
    // @include statefulCanvas.getState
    // @visibility external
    //<
    //> @attr button.showDisabled
    // @include statefulCanvas.showDisabled
    // @visibility external
    // @example buttonStates
    //<  
    //> @attr button.showDown
    // @include statefulCanvas.showDown
    // @visibility external
    // @example buttonStates
    //<  
    //> @attr button.showFocused
    // @include statefulCanvas.showFocused
    // @visibility external
    //<  
	showDown:true,
    
    showFocused:null, // !isc.Browser.isTouch
    //> @attr button.showRollOver
    // @include statefulCanvas.showRollOver
    // @visibility external
    // @example buttonStates
    //<  
	showRollOver:true,

    
    mozOutlineOffset: "0px",

    // alignment
    //----------
    //> @attr button.align
    // @include statefulCanvas.align
    // @visibility external
    //<          
    //> @attr button.valign
    // @include statefulCanvas.valign
    // @visibility external
    //<      


    // Button.action
    //> @method button.action()
    // @include statefulCanvas.action
    // @visibility external
    //<
    
    // ================= END StatefulCanvas @include block =============== //

    
    //>	@attr	button.wrap		(Boolean : false : [IRW])
    // A boolean indicating whether the button's title should word-wrap, if necessary.
    // @group basics
    //      @visibility external
    //<
	wrap:false,

    // NOTE: by setting "height" rather than "defaultHeight", we make this into an explicit
    // setting which will be respected by a Layout
	height:20,					        
    width:100,
    
	//>	@attr	button.overflow		(attrtype : isc.Canvas.HIDDEN : IRWA)
	// Clip the contents of the button if necessary
	//<
    overflow:isc.Canvas.HIDDEN,				
    
    //>	@attr	button.redrawOnDisable		(boolean : true : IRWA)
	// true == redraw the button when it's enabled/disabled
	//<
	redrawOnDisable:false,				

    //>	@attr	button.redrawOnStateChange		(boolean : true : IRWA)
	// true == redraw the button when it's state changes
	//		@group	state
	//<										
	redrawOnStateChange:false,			

	//>	@attr	button.cursor		(Cursor : isc.Canvas.HAND : IRW)
	// Hand cursor since these items are clickable
	//<
	cursor:isc.Canvas.HAND,

	// Style of the button is set via baseStyle, etc. above
    // NOTE: the button applies its CSS style to a contained cell, not the Canvas itself.
    className:null,						

    // If true, add a space to left or right-aligned titles so that they are not flush with
    // the edge of the widget.
    // NOTE: FIXME: this should really be done via CSS padding, hence not external doc'd
    //padTitle:false,

    //> @attr statefulCanvas.titleStyle        (CSSStyleName : "normal" : [IR])
    // For buttons with icons only, optional style to be applied to title text only.  This
    // style should contain text-related properties only (such as font size); border, padding
    // and background color should be specified in the style used as the "baseStyle".
    //
    // This property applied only to icon buttons rendered with a subtable, and currently only
    // works if redrawOnStateChange is true.  Internal for now.
    //<
    
    //titleStyle:"buttonTitle",

    canFocus:true
});

// add instance methods
isc.Button.addMethods({

//>	@method	button.initWidget()	(A)
//			Extended initWidget() to allow initialization of the enabled property
//
//		@param	[all arguments]	(object)	objects with properties to override from default
//<
initWidget : function () {
    
    

    if (this.border != null && !isc.StatefulCanvas.pushTableBorderStyleToDiv) {
        this._buttonBorder = this.border;
        this.border = null;
    }
    if (this.padding != null) {
        this._buttonPadding = this.padding;
        this.padding = null;
    }
    if (this.backgroundColor != null) {
        this._buttonBGColor = this.backgroundColor;
        this.backgroundColor = null;
    }
    
    
    this.forceHandleOverflowHidden = isc.StatefulCanvas.pushTableBorderStyleToDiv;

    // Call super implementation directly rather than using Super() to avoid a string 
    // allocation.
    return isc.StatefulCanvas._instancePrototype.initWidget.call(this);
},

// setHandleRect() handles resizing the widget handle.
// After adjusting size, it falls through to _assignRectToHandle (with sizes adjusted for
// custom scrollbars, etc).
// If we're writing out an explicitly sized inner table override this method to also
// resize the inner table's handle.
_assignRectToHandle : function (left,top,width,height,styleHandle) {
    // Resize the handle
    this.invokeSuper(isc.Button, "_assignRectToHandle", left,top,width,height,styleHandle);

    
    if (this.redrawOnResize && !this.isPrinting && this._explicitlySizeTable()) return;
    var tableElem = this._getTableElement();
    if (tableElem != null) {
        // If provided a width, and this button's markup requires a width attribute to be set
        // on the <table>, then update the attribute.
        if (width != null && isc.isA.Number(width) &&
            (this.overflow !== isc.Canvas.VISIBLE ||
             (!this._explicitlySizeTable() && this.redrawOnResize != false)))
        {
            var tableWidth = width;
            if (this.isBorderBox) tableWidth -= this.getHBorderPad();
            this._assignSize(tableElem, this._$width, tableWidth);
        }

        if (height != null && isc.isA.Number(height)) {
            var tableHeight = height;
            if (this.isBorderBox) tableHeight -= this.getVBorderPad();
            this._assignSize(tableElem, this._$height, tableHeight);
        }
    }
},


shouldRedrawOnResize : (isc.Browser.isIPhone ?
    function (deltaX, deltaY) {
        return (this !== this.ns.EH.dragTarget);
    }
: // !isc.Browser.isIPhone
    isc.Button.getSuperClass()._instancePrototype.shouldRedrawOnResize
),

getCanHover : function (a, b, c) {
    return this._canHover || this.invokeSuper(isc.Button, "getCanHover", a, b, c);
},

shouldClipTitle : function () {
    return this.getOverflow() == isc.Canvas.HIDDEN && !!this.clipTitle;
},

_$titleClipper:"titleClipper",
_getTitleClipperID : function () {
    return this._getDOMID(this._$titleClipper);
},

//> @method button.titleClipped() (A)
// Is the title of this button clipped?
// @return (boolean) whether the title is clipped.
// @visibility external
//<
titleClipped : function () {
    var titleClipperHandle = this.getDocument().getElementById(this._getTitleClipperID());
    if (titleClipperHandle == null) return false;

    
    if (isc.Browser.isMoz && isc.Browser.isMac && isc.Browser.version >= 7) {
        var range = this.getDocument().createRange();
        range.selectNodeContents(titleClipperHandle);
        var contentsBCR = range.getBoundingClientRect();
        var bcr = titleClipperHandle.getBoundingClientRect();
        return (bcr.width < contentsBCR.width);

    } else {
        return (isc.Element.getClientWidth(titleClipperHandle) < titleClipperHandle.scrollWidth);
    }
},

defaultTitleHoverHTML : function () {
    return this.getTitleHTML();
},

//> @method button.titleHoverHTML()
// Returns the HTML that is displayed by the default +link{Button.titleHover(),titleHover}
// handler. Return null or an empty string to cancel the hover.
// <smartgwt><p>Use <code>setTitleHoverFormatter()</code> to provide a custom
// implementation.</smartgwt>
// @param defaultHTML (HTMLString) the HTML that would have been displayed by default
// @return (HTMLString) HTML to be displayed in the hover. If null or an empty string, then the hover
// is canceled.
// @visibility external
//<
titleHoverHTML : function (defaultHTML) {
    return defaultHTML;
},

handleHover : function (a, b, c) {
    // If there is a prompt, prefer the standard hover handling.
    if (this.canHover == null && this.prompt) return this.invokeSuper(isc.Button, "handleHover", a, b, c);

    if (!this.showClippedTitleOnHover || !this.titleClipped()) {
        if (this.canHover) return this.invokeSuper(isc.Button, "handleHover", a, b, c);
        else return;
    }

    if (this.titleHover && this.titleHover() == false) return;

    var HTML = this.titleHoverHTML(this.defaultTitleHoverHTML());
    if (HTML != null && !isc.isAn.emptyString(HTML)) {
        var hoverProperties = this._getHoverProperties();
        isc.Hover.show(HTML, hoverProperties, null, this);
    }
},


_explicitlySizeTable : function (iconAtEdge, clipTitle) {
    if (iconAtEdge == null) iconAtEdge = this._iconAtEdge();
    if (clipTitle == null) clipTitle = this.shouldClipTitle();

    return !(
        // This expression is negated, so this is the case where we want to write
        // a table with size natively set to "100%" in both directions.
        iconAtEdge || !clipTitle ||
         (isc.Browser.isIE && ((!isc.Browser.isStrict && isc.Browser.version < 10) ||
                              isc.Browser.version <= 7))
    );
},
//> @method button.getInnerHTML() (A)
// Return the HTML for this button
// @return (HTMLString) HTML output for the button
// @group drawing
//<
getInnerHTML : function () {
    var iconAtEdge = this._iconAtEdge(),
        clipTitle = this.shouldClipTitle(),
        isRTL = this.isRTL();
    if (this.isPrinting || !this._explicitlySizeTable(iconAtEdge, clipTitle)) {
        

        var button = isc.Button;
        if (!button._buttonHTML) {
            
            button._100Size = " width='100%' height='100%";
            button._100Width = " width='100%";
            button._widthEquals = "width='";
            button._heightEquals = "' height='";            
            button._hiddenOverflow = "' style='table-layout:fixed;overflow:hidden;";

            var cellStartHTML = button._cellStartHTML = [];
            button._gt = ">";
            button._nowrapTrue = " nowrap='true'";
            button._classEquals = " class='";
            button._colWidthEquals = "<col width='";
            button._pxEndCol = "px'/>";
            button._emptyCol = "<col/>";
            cellStartHTML[0] = "'><colgroup>";
            // [1] _emptyCol or _colWidthEquals
            // [2] null or afterPadding or _colWidthEquals
            // [3] null or afterPadding or _pxEndCol
            // [4] null or _pxEndCol or _emptyCol
            cellStartHTML[5] = "</colgroup><tbody><tr><td";
            // [6] null or _nowrapTrue
            // [7] _classEquals

            button._cellStartWrap = "'><tbody><tr><td class='";
            button._cellStartNoWrap = "'><tbody><tr><td nowrap='true' class='";

            var buttonHTML = button._buttonHTML = [];
            // NOTE: for DOM platforms, padding should be achieved by CSS padding and spacing
            // by CSS margins
            buttonHTML[0] = "<table role='presentation' cellspacing='0' cellpadding='0' ";
            // [1] 100% width and height, or width=
            // [2] null or this.getWidth()
            // [3] null or height=
            // [4] null or this.getHeight();
            
            // [5] overflow setting
            // [6] cell start (wrap/nowrap variants)
            // [7] CSS class

            // [8] optional cssText

            buttonHTML[9] = "' align='";
            // [10] align
            // [11] valign
            button._valignMiddle = "' valign='middle";
            button._valignTop = "' valign='top";
            button._valignBottom = "' valign='bottom";

            // [12-13] titleClipper ID
            button._id = "' id='";

            // [14-16] tabIndex and focus
            
            button._tabIndexStart = "' " + (isc.Browser.isChrome ? "" : "tabindex='-1'") 
                                    + " onfocus='";
            button._callFocus = "._cellFocus()'>";
            button._closeQuoteRightAngle = "'>";

            // IE 
            // [17] title

            // Moz
            // [17] Moz start DIV
            // [18] title
            // [19] Moz end DIV

            // end table (see _endTemplate)
        }

        var buttonHTML = button._buttonHTML;
        // if we're printing the button, make it fit its parent element
        // If we're not redrawing on resize, use 100% sizing - will reflow on resize of parent
        // element
        if (this.isPrinting || this.redrawOnResize == false) {
            // if we're not going to redraw on resize, write HTML that reflows automatically.  Not
            // yet possible in every browser.

            buttonHTML[1] = (this.isPrinting ? button._100Width : button._100Size);
            buttonHTML[2] = null; buttonHTML[3] = null; buttonHTML[4] = null;
        } else {
            buttonHTML[1] = button._widthEquals;

            // If we're going to draw a clip div and pushTableBorderStyleToDiv is true, then
            // the border styling is going to be pushed onto the inner content DIV, so getInnerWidth()/
            // getInnerHeight() are off by the horizontal/vertical border size.
            var willDrawClipDiv = this._shouldWriteClipDiv();
            buttonHTML[2] = this.getInnerWidth() - (willDrawClipDiv && isc.StatefulCanvas.pushTableBorderStyleToDiv
                                                    ? this.getHBorderSize() : 0);
            buttonHTML[3] = button._heightEquals;
            buttonHTML[4] = this.getInnerHeight() - (willDrawClipDiv && isc.StatefulCanvas.pushTableBorderStyleToDiv
                                                     ? this.getVBorderSize() : 0);
        }


        if (this.overflow == isc.Canvas.VISIBLE) {
            buttonHTML[5] = null;
        } else {
            buttonHTML[5] = button._hiddenOverflow;
        }

        // Inside the cell:

        
        var afterPadding;
        if (isc.Browser.isIE && !isc.Browser.isStrict && this._isStatefulCanvasLabel && isc.Browser.version < 10 &&
            (afterPadding = this._getAfterPadding == null ? null : this._getAfterPadding()) > 0)
        {
            var cellStartHTML = button._cellStartHTML;
            cellStartHTML[1] = button._emptyCol;
            cellStartHTML[2] = button._colWidthEquals;
            cellStartHTML[3] = afterPadding;
            cellStartHTML[4] = button._pxEndCol;

            cellStartHTML[6] = (this.wrap ? null : button._nowrapTrue);
            cellStartHTML[7] = button._classEquals;

            buttonHTML[6] = cellStartHTML.join(isc.emptyString);
        } else {
            buttonHTML[6] = (this.wrap ? button._cellStartWrap : button._cellStartNoWrap);
        }

        buttonHTML[7] = this.isPrinting ? this.getPrintStyleName() : this.getStateName();


        var isTitleClipper = !iconAtEdge && clipTitle;

        
        var writeStyle = isTitleClipper || this.cssText || this._buttonBorder || this._buttonPadding || 
                         this._buttonBGColor || this.margin || this._writeZeroVPadding() || 
                         isc.StatefulCanvas.pushTableBorderStyleToDiv ||
                         this._getAfterPadding != null;
        if (writeStyle) buttonHTML[8] = this._getCellStyleHTML(null, isTitleClipper);
        else buttonHTML[8] = null;

        // If the iconOrientation and iconAlign are set such that the icon is pinned to the
        // edge of the table rather than showing up next to the title, ensure we center the
        // inner table - alignment of the title will be written directly into its cell.
        if (iconAtEdge || this.align == null) {
            buttonHTML[10] = isc.Canvas.CENTER;
        } else if (!isRTL || this.ignoreRTL) {
            buttonHTML[10] = this.align;
        } else {
            
            buttonHTML[10] = isc.StatefulCanvas._mirroredAlign[this.align];
        }
        buttonHTML[11] = (this.valign == isc.Canvas.TOP ? button._valignTop : 
                            (this.valign == isc.Canvas.BOTTOM ? button._valignBottom
                                                              : button._valignMiddle) );

        if (isTitleClipper) {
            buttonHTML[12] = button._id;
            buttonHTML[13] = this._getTitleClipperID();
        } else {
            buttonHTML[13] = buttonHTML[12] = null;
        }

        
        if (this._canFocus() && this._useNativeTabIndex) {
            buttonHTML[14] = button._tabIndexStart;
            buttonHTML[15] = this.getID();
            buttonHTML[16] = button._callFocus;
        } else {
            buttonHTML[14] = button._closeQuoteRightAngle;
            buttonHTML[15] = buttonHTML[16] = null;
        }
        this.fillInCell(buttonHTML, 17, isTitleClipper);
        return buttonHTML.join(isc.emptyString);
    } else {
        var sb = isc.SB.create(),
            valign = (this.valign == isc.Canvas.TOP || this.valign == isc.Canvas.BOTTOM
                      ? this.valign
                      : "middle");
        var textAlign;
        if (this.align == null) {
            textAlign = isc.Canvas.CENTER;
        } else if (!isRTL || this.ignoreRTL) {
            textAlign = this.align;
        } else {
            
            textAlign = isc.StatefulCanvas._mirroredAlign[this.align];
        }
        sb.append("<table role='presentation' cellspacing='0' cellpadding='0'",
                  (this.overflow !== isc.Canvas.VISIBLE ? " width='" + this.getInnerWidth() + "' style='table-layout:fixed'" : null),
                  " height='", this.getInnerHeight(), "'><tbody><tr><td class='",
                  this.getStateName(), "' style='", this._getCellStyleHTML([]), "text-align:", textAlign,
                  ";vertical-align:", valign, (!this.wrap ? ";white-space:nowrap" : ""), "'>");

        var titleClipperID = this._getTitleClipperID(),
            iconSpacing = this.getIconSpacing(),
            iconWidth = (this.iconWidth || this.iconSize),
            extraWidth = iconSpacing + iconWidth,
            opposite = ((!isRTL && this.iconOrientation == isc.Canvas.RIGHT) ||
                        (isRTL && ((this.ignoreRTL && this.iconOrientation == isc.Canvas.LEFT) ||
                                   (!this.ignoreRTL && this.iconOrientation == isc.Canvas.RIGHT)))),
            b = (isRTL || opposite) && !(isRTL && opposite);

        var beforePadding = 0,
            afterPadding = 0,
            iconHTML = null;
        if (this.icon != null) {
            beforePadding = extraWidth;
            

            iconHTML = this._generateIconImgHTML({
                align: "absmiddle",
                extraCSSText: (b ? "margin-left:" : "margin-right:") +
                              iconSpacing + "px;vertical-align:middle",
                extraStuff: this._$defaultImgExtraStuff
            });
        }
        sb.append((!opposite ? iconHTML : null),
                  "<div id='", titleClipperID, "' style='display:inline-block;",
                  (this.icon ? (b ? "margin-right:" : "margin-left:") + (-extraWidth) + "px;" : null),
                  isc.Element._boxSizingCSSName, ":border-box;max-width:100%;",
                  (beforePadding ? ((b ? "padding-right:" : "padding-left:") + beforePadding + "px;") : null),
                  (afterPadding ? ((b ? "padding-left:" : "padding-right:") + afterPadding + "px;") : null),
                  "vertical-align:middle;overflow:hidden;",
                  isc.Browser._textOverflowPropertyName, ":ellipsis'>", this.getTitleHTML(), "</div>",
                  (opposite ? iconHTML : null));

        sb.append("</td></tr></tbody></table>");
        return sb.release(false);
    }
},

_getTableElement : function () {
    var handle = this.getHandle();
    return handle && handle.firstChild;
},

_getCellElement : function () {
    var tableElem = this._getTableElement();
    if (tableElem == null) return null;

    
    return tableElem.rows[0].childNodes[0];
},

// force a redraw on setOverflow()
// This is required since we write out clipping HTML for our title table if our overflow
// is hidden (otherwise we don't), so we need to regenerate this.
setOverflow : function () {
    var wasDirty = this.isDirty(),
        oldOverflow = this.overflow;
    this.Super("setOverflow", arguments);
    
    if (!wasDirty && (oldOverflow != this.overflow ||
                      (isc.StatefulCanvas.pushTableShadowStyleToDiv && this._getHandleOverflow() === isc.Canvas.HIDDEN)))
    {
        this.redraw();
    }
},

__adjustOverflow : function (reason) {
    this.Super("__adjustOverflow", arguments);

    
    if (isc.Browser.isSafari && !isc.Browser.isChrome && this.icon != null &&
        !(this.isPrinting || !this._explicitlySizeTable()))
    {
        var isRTL = this.isRTL(),
            opposite = ((!isRTL && this.iconOrientation == isc.Canvas.RIGHT) ||
                        (isRTL && ((this.ignoreRTL && this.iconOrientation == isc.Canvas.LEFT) ||
                                   (!this.ignoreRTL && this.iconOrientation == isc.Canvas.RIGHT))));

        if (!opposite) {
            var textAlign;
            if (this.align == null) {
                textAlign = isc.Canvas.CENTER;
            } else if (!isRTL || this.ignoreRTL) {
                textAlign = this.align;
            } else {
                
                textAlign = isc.StatefulCanvas._mirroredAlign[this.align];
            }

            var titleClipperHandle = this.getDocument().getElementById(this._getTitleClipperID()),
                titleClipperStyle = titleClipperHandle.style,
                iconSpacing = this.getIconSpacing(),
                iconWidth = (this.iconWidth || this.iconSize),
                extraWidth = iconSpacing + iconWidth;

            var beforePadding = extraWidth,
                afterPadding = 0;

            // Reset the title clipper's left and right padding to "normal" before checking whether
            // the title is clipped.
            titleClipperStyle[isRTL ? "paddingRight" : "paddingLeft"] = beforePadding + "px";
            titleClipperStyle[isRTL ? "paddingLeft" : "paddingRight"] = "";
            if (this.titleClipped()) {
                
                if (textAlign === isc.Canvas.CENTER) {
                    beforePadding = ((beforePadding + iconSpacing) / 2) << 0;
                    afterPadding = (iconSpacing / 2) << 0;
                } else if ((!isRTL && textAlign === isc.Canvas.RIGHT) ||
                           (isRTL && textAlign === isc.Canvas.LEFT))
                {
                    beforePadding -= (beforePadding / 2) << 0;
                    afterPadding = iconWidth;
                }

                titleClipperStyle[isRTL ? "paddingRight" : "paddingLeft"] = beforePadding + "px";
                titleClipperStyle[isRTL ? "paddingLeft" : "paddingRight"] = afterPadding + "px";
            }
        }
    }
},

// override getPrintTagStart to avoid writing out the printClassName on the outer div
getPrintTagStart : function (absPos) {
    var props = this.currentPrintProperties,
        topLevel = props.topLevelCanvas == this, 
        inline = !absPos && !topLevel && props.inline;

    return [((this.wrap == false) ? "<div style='white-space:nowrap' " : inline ? "<span " : "<div "),
            // could add borders etc here
            this.getPrintTagStartAttributes(absPos),
            ">"].join(isc.emptyString);
},


_$pxSemi:"px;", _$semi:";",
_$borderColon:"border:",
_$zeroVPad:"padding-top:0px;padding-bottom:0px;",
_$paddingColon:"padding:",
_$paddingRightColon:"padding-right:",
_$paddingLeftColon:"padding-left:",
_$bgColorColon:"background-color:",
_$zeroMargin:"margin:0px;",
_$filterNone:"filter:none;",
_$textOverflowEllipsis:isc.Browser._textOverflowPropertyName + ":ellipsis;overflow:hidden;",
_$cellStyleTemplate:[
    "' style='", // [0]
    ,           // [1] explicit css text applied to the button

    ,           // [2] null or "border:" (button border)
    ,           // [3] null or this._buttonBorder (button border)
    ,           // [4] null or ";" (button border)

    ,           // [5] null or "padding:" (button padding)
    ,           // [6] null or this._buttonPadding (button padding)
    ,           // [7] null or ";"  (button padding)

    ,           // [8] null or backgroundColor (button bg color)
    ,           // [9] null or this._buttonBGColor (button bg color)
    ,           // [10] null or ";" (button bg color)

    ,           // [11] null or "margin:0px" (avoid margin doubling)
    ,           // [12] null or "filter:none" (avoid IE8 filter issues)

    ,           // [13] null or "text-overflow:ellipsis;overflow:hidden;"

    ,           // [14] null or "padding-right:"/"padding-left:" (after padding)
    ,           // [15] null or this._getAfterPadding() (after padding)
    null        // [16] null or "px;" (after padding)

    // No need to close the quote - the button HTML template handles this.
],


_getCellStyleHTML : function (template, isTitleClipper) {
    template = template || this._$cellStyleTemplate;

    template[1] = (this.cssText ? this.cssText : null);

    var border = isc.StatefulCanvas.pushTableBorderStyleToDiv ? "none" : this._buttonBorder;
    if (border != null) {
        template[2] = this._$borderColon;
        template[3] = border;
        template[4] = this._$semi;
    } else {
        template[2] = null;
        template[3] = null;
        template[4] = null;
    }
    
    var padding = this._buttonPadding;
    if (padding != null) {
        template[5] = this._$paddingColon;
        template[6] = padding;
        template[7] = this._$pxSemi;
    } else {
        template[5] = null;
        template[6] = null;
        template[7] = null;
    }
    if (this._writeZeroVPadding()) {
        template[7] = (template[7] || isc.emptyString) + this._$zeroVPad;
    }
    
    if (this._buttonBGColor != null) {
        template[8] = this._$bgColorColon;
        template[9] = this._buttonBGColor;
        template[10] = this._$semi;
    } else {
        template[8] = null;
        template[9] = null;
        template[10] = null;
    }
    
    if (this.margin != null) template[11] = this._$zeroMargin;
    else template[11] = null;

    if (isc.Browser.useCSSFilters) template[12] = null;
    else template[12] = this._$filterNone;

    if (isTitleClipper) template[13] = this._$textOverflowEllipsis;
    else template[13] = null;

    var afterPadding;
    if (this.overflow != isc.Canvas.VISIBLE &&
        (!isc.Browser.isIE || isc.Browser.isStrict || !this._isStatefulCanvasLabel || isc.Browser.version >= 10) &&
        (afterPadding = (this._getAfterPadding == null ? null : this._getAfterPadding())) > 0)
    {
        template[14] = (this.isRTL() ? this._$paddingLeftColon : this._$paddingRightColon);
        template[15] = afterPadding;
        template[16] = this._$pxSemi;
    } else {
        template[16] = template[15] = template[14] = null;
    }

    return template.join(isc.emptyString);
},


_writeZeroVPadding : function () {
    return this.overflow == isc.Canvas.HIDDEN && 
           // don't remove padding during animations or text may reflow
           !this.isAnimating() && 
            (isc.Browser.isMoz || isc.Browser.isSafari || isc.Browser.isIE);
},     

 
setBorder : function (border) {
    var pushStyle = isc.StatefulCanvas.pushTableBorderStyleToDiv;
    if (pushStyle) this.border = border;
    else    this._buttonBorder = border;
    this.markForRedraw();
},
setPadding : function (padding) {
    this._buttonPadding = padding;
    this.markForRedraw();
},

setBackgroundColor : function (color) {
    this._buttonBGColor = color;
    var cellElem = this._getCellElement();
    if (cellElem != null) cellElem.style.backgroundColor = (color == null ? "" : color);
},

_$endTable :"</td></tr></tbody></table>",
_endTemplate : function (template, slot) {
    template[slot] = this._$endTable;
    template.length = slot+1;
    return template;
},

_$innerTableStart : "<table role='presentation' cellspacing='0' cellpadding='0'><tbody><tr><td ",
_$fillInnerTableStart : "<table role='presentation' width='100%' cellspacing='0' cellpadding='0'><tbody><tr><td ",
_$fillInnerFixedTableStart : "<table role='presentation' width='100%' cellspacing='0' cellpadding='0' style='table-layout:fixed'><tbody><tr><td ",


_$leftIconCellStyleStart : "font-size:" + 
                            (isc.Browser.isFirefox && isc.Browser.isStrict ? 0 : 1) + 
                            "px;padding-right:",
_$rightIconCellStyleStart : "font-size:" + 
                            (isc.Browser.isFirefox && isc.Browser.isStrict ? 0 : 1) + 
                            "px;padding-left:",
_$pxClose : "px'>",
_$newInnerCell : "</td><td ", 

_$classEquals : "class='",

//_$tableNoStyleDoubling : defined in Canvas.js

_$closeInnerTag : "'>",
_$closeInnerTagNoWrap : "' nowrap='true'>",    
 
_$innerTableEnd : "</td></tr></tbody></table>",

// used to check alignment for the icon
_$right:"right",

// Helper - is the icon pinned to the left / right edge, rather than floated next to the title?
_iconAtEdge : function () {
    return this.icon != null && this.iconAlign != null && 
                (this.iconAlign == this.iconOrientation) && 
                (this.iconAlign != this.align);
},

getIconSpacing : function () {
    if (this.icon == null || this.title == null) return 0;
    return this.iconSpacing;
},

fillInCell : function (template, slot, cellIsTitleClipper) {

    var isRTL = this.isRTL();

    var title = this.getTitleHTML();

    if (!this.icon) {
        if (isc.Browser.isMoz) {
            var minHeight = this.reliableMinHeight;
            template[slot] = (minHeight ? "<div>" : null);
            template[slot+1] = title;
            template[slot+2] = (minHeight ? "</div>" : null);
            this._endTemplate(template, slot+3)
        } else {
            template[slot] = title;
            this._endTemplate(template, slot+1)
        }
        return;
    }

    var iconLeft = (!isRTL && this.iconOrientation != isc.Canvas.RIGHT) ||
                    (isRTL && ((this.ignoreRTL && this.iconOrientation != isc.Canvas.LEFT) ||
                               (!this.ignoreRTL && this.iconOrientation != isc.Canvas.RIGHT))),
        iconImg = this._generateIconImgHTML();


    

    // draw icon and text with spacing w/o a table.
    if (cellIsTitleClipper || this.noIconSubtable) {
        var spacer = isc.Canvas.spacerHTML(this.getIconSpacing(),1);
        template[slot] = (iconLeft ? isc.SB.concat(iconImg, spacer, title)
                                   : isc.SB.concat(title, spacer, iconImg));
        this._endTemplate(template, slot+1)
        return;
    }

    // Should we have the icon show up at the edge of the button, rather than being
    // adjacent to the title text?
    

    var iconAtEdge = this._iconAtEdge(),
        iconCellSpace;
    if (iconAtEdge) {
        iconCellSpace = (this.iconWidth ? this.iconWidth : this.iconSize) +
            
            (isc.Browser.isBorderBox ? this.getIconSpacing() : 0)
    }

    var clipTitle = this.shouldClipTitle();

    // if the icon is showing at one edge (and the text is separated from it), draw the
    // table 100% wide
    template[slot] = (iconAtEdge
                      ? (clipTitle
                         ? this._$fillInnerFixedTableStart
                         : this._$fillInnerTableStart)
                      : this._$innerTableStart);

    var styleName = this.isPrinting ? this.getPrintStyleName() :
                    (this.titleStyle
                      ? this.getTitleStateName()
                      : this.getStateName()
                    );

    var tableNoStyleDoubling = this._$tableNoStyleDoubling;
    if (!isc.Browser.useCSSFilters) tableNoStyleDoubling += this._$filterNone;

    var align;
    if (this.align == null) {
        align = isc.Canvas.CENTER;
    } else if (!isRTL || this.ignoreRTL) {
        align = this.align;
    } else {
        
        align = isc.StatefulCanvas._mirroredAlign[this.align];
    }

    if (iconLeft) {

        // icon cell
        template[++slot] = this._$classEquals;
        template[++slot] = styleName;
        template[++slot] = tableNoStyleDoubling;

        template[++slot] = !isRTL ? this._$leftIconCellStyleStart :
                                    this._$rightIconCellStyleStart;

        template[++slot] = this.getIconSpacing();
        if (iconAtEdge) {
            template[++slot] = "px;width:";
            template[++slot] = iconCellSpace;
        }
        template[++slot] = this._$pxClose;
        template[++slot] = iconImg;
        // title cell
        template[++slot] = this._$newInnerCell;
        template[++slot] = this._$classEquals;

        template[++slot] = styleName;
        template[++slot] = tableNoStyleDoubling;
        if (clipTitle) template[++slot] = this._$textOverflowEllipsis;

        if (iconAtEdge) {
            template[++slot] = "' align='";
            template[++slot] = align;
        }
        if (clipTitle) {
            template[++slot] = isc.Button._id;
            template[++slot] = this._getTitleClipperID();
        }
        template[++slot] = (this.wrap ? this._$closeInnerTag : this._$closeInnerTagNoWrap)
        template[++slot] = title;

    } else {
        // title cell:
        template[++slot] = this._$classEquals;
        template[++slot] = styleName;
        template[++slot] = tableNoStyleDoubling;
        if (clipTitle) template[++slot] = this._$textOverflowEllipsis;

        if (iconAtEdge) {
            template[++slot] = "' align='";
            template[++slot] = align;
        }
        if (clipTitle) {
            template[++slot] = isc.Button._id;
            template[++slot] = this._getTitleClipperID();
        }
        template[++slot] = (this.wrap ? this._$closeInnerTag : this._$closeInnerTagNoWrap)
        template[++slot] = title;

        // icon cell
        template[++slot] = this._$newInnerCell;

        template[++slot] = this._$classEquals;
        template[++slot] = styleName;
        template[++slot] = tableNoStyleDoubling;

        template[++slot] = !isRTL ? this._$rightIconCellStyleStart :
                                    this._$leftIconCellStyleStart;
        template[++slot] = this.getIconSpacing();
        if (iconAtEdge) {
            template[++slot] = "px;width:";
            template[++slot] = iconCellSpace;
        }
        template[++slot] = this._$pxClose;
        template[++slot] = iconImg;

    }
    template[++slot] = this._$innerTableEnd; 

    this._endTemplate(template, slot+1)
},





_imgParams : {
    align: "absmiddle" // just prevents default "texttop" from kicking in
},
_$icon:"icon",
_$defaultImgExtraCSSText: "vertical-align:middle",
_$defaultImgExtraStuff: " eventpart='icon'",
_generateIconImgHTML : function (imgParams) {
    // NOTE: we reuse a single global imgParams structure, so we must set every field we ever
    // use every time.
    if (imgParams == null) {
        imgParams = this._imgParams;
        imgParams.extraCSSText = this._$defaultImgExtraCSSText;
        imgParams.extraStuff = this._$defaultImgExtraStuff;
    }
    if (this.iconStyle != null) {
        var classText = " class='" + this.iconStyle + this._getIconStyleSuffix() + this._$singleQuote;
        if (imgParams.extraStuff == null) imgParams.extraStuff = classText;
        else imgParams.extraStuff += classText;
    }

    imgParams.name = this._$icon;
    imgParams.width = this.iconWidth || this.iconSize;
    imgParams.height = this.iconHeight || this.iconSize;
    imgParams.src = this._getIconURL();

    if (this.iconCursor != null) {
        var cursor = this._getIconCursor();
        var cursorCSSText = "cursor:" + cursor;
        if (imgParams.extraCSSText == null) {
            imgParams.extraCSSText = cursorCSSText;
        } else {
            imgParams.extraCSSText += ";" + cursorCSSText;
        }
    }

	return this.imgHTML(imgParams);
},
_getIconURL : function () {
    
    var state = this.state,
        selected = this.selected,
        customState = this.getCustomState(),
        sc = isc.StatefulCanvas;

    // ignore states we don't care about
    if (state == sc.STATE_DISABLED && !this.showDisabledIcon) state = null;
    else if (state == sc.STATE_DOWN && !this.showDownIcon) state = null;
    else if (state == sc.STATE_OVER && !this.showRollOverIcon) state = null;

    if (!this.showIconState) {
        state = null;
        customState = null;
    }

    if (selected && !this.showSelectedIcon) selected = false;
    // Note that getFocusedState() will return false if showFocusedAsOver is true, which is
    // appropriate
    var focused = this.showFocusedIcon ? this.getFocusedState() : null;
    var icon = this.icon;
    if (isc.isAn.Object(icon)) icon = icon.src;
    return isc.Img.urlForState(icon, selected, focused, state, (this.showRTLIcon && this.isRTL() ? "rtl" : null), customState);
},

// Get the suffix to append to the iconStyle.
// This is similar to StatefulCanvas.getStateSuffix(), but instead of being configured by
// show showRollOver, showDown, showDisabled, etc., this is configured by showRollOverIcon,
// showDownIcon, showDisabledIcon, etc.
_$RTL: "RTL",
_getIconStyleSuffix : function () {
    var state = this.state,
        selected = this.selected ? isc.StatefulCanvas.SELECTED : null,
        customState = this.getCustomState(),
        sc = isc.StatefulCanvas;

    // Ignore states we don't care about
    if (state == sc.STATE_DISABLED && !this.showDisabledIcon) state = isc.emptyString;
    else if (state == sc.STATE_DOWN && !this.showDownIcon) state = isc.emptyString;
    else if (state == sc.STATE_OVER && !this.showRollOverIcon) state = isc.emptyString;

    if (!this.showIconState) {
        state = isc.emptyString;
        customState = null;
    }

    if (selected != null && !this.showSelectedIcon) selected = null;
    // Note that getFocusedState() will return false if showFocusedAsOver is true, which is
    // appropriate.
    var focused = this.showFocusedIcon ? (this.getFocusedState() ? isc.StatefulCanvas.FOCUSED : null) : null;

    var suffix = this._getStateSuffix(state, selected, focused, customState);
    if (this.showRTLIcon && this.isRTL()) suffix += this._$RTL;
    return suffix;
},

getTitleHTML : function (a,b,c,d) {
    // This will call getTitle() so return contents if appropriate, and will hilite accessKeys
    var title = this.invokeSuper(isc.Button, "getTitleHTML", a,b,c,d);

    // FIXME: title padding should be accomplished with CSS
    if (!this.padTitle || this.align == isc.Canvas.CENTER) return title;

    if (this.align == isc.Canvas.RIGHT) return title + isc.nbsp;
    else if (this.align == isc.Canvas.LEFT) return isc.nbsp + title;
},


//> @method Button.setWrap()
// Set whether the title of this button should be allowed to wrap if too long for the button's
// specified width.
//
// @param newWrap (boolean) whether to wrap the title
// @visibility external
//<
setWrap : function (newWrap) {
    if (this.wrap != newWrap) {
        // NOTE: wrap can almost certainly be changed on the fly w/o redraw, at least on modern
        // browsers
        this.wrap = newWrap;
        this.markForRedraw("wrapChanged");
    }
},

// get the cell holding the title text.  DOM only.
getTitleCell : function () {
    if (!this.getHandle()) return null;
    var table = this.getHandle().firstChild,
        row = table && table.rows != null ? table.rows[0] : null,
        cell = row && row.cells != null ? row.cells[0] : null;
    return cell;
},

// get the minimum height of this button which would not clip the title text as it is currently
// wrapped.  Only available after drawing.  For Moz, must set "reliableMinHeight" for
// this to be reliable.
getButtonMinHeight : function () {
    

    var titleCell = this.getTitleCell();
    // In IE, and probably other DOM browsers, the cell's scrollHeight is reliable
    if (!isc.Browser.isMoz) {
        return titleCell.scrollHeight + isc.Element._getVBorderSize(this.getStateName());
    }

    
    return titleCell.firstChild.offsetHeight + 
        isc.Element._getVBorderSize(this.getStateName());
},

// get the width this button would need to be in order to show all text without wrapping
// XXX move deeper, to Canvas?
getPreferredWidth : function () {

    

    var oldWrap = this.wrap,
        oldOverflow = this.overflow,
        oldWidth = this.width;

    // set overflow visible with no minimum width in order to get the minimum width that won't
    // wrap or clip the title text
    // XXX because wrapping is controlled by a <NOBR> tag in the generated HTML, we can't detect
    // preferred width without a redraw, even if we could resize without a redraw
    this.setWrap(false);
    this.overflow = isc.Canvas.VISIBLE;
    this.setWidth(1);
    this.redrawIfDirty("getPreferredWidth");

    var width = this.getScrollWidth();

    // reset text wrapping and overflow setting
    this.setWrap(oldWrap);
    this.overflow = oldOverflow;
    // NOTE: if this button needs to redraw on resize, this will queue up a redraw, but if you
    // are trying to set the button to it's preferred size you will avoid a redraw if you set
    // the new size right away.
    this.setWidth(oldWidth); 

    return width;
},

getTitle : function () {
    if (this.useContents) return this.getContents();
    return this.title;
},

//>	@method	button.stateChanged()	(A)
//		@group	appearance
//			overrides the StatefulCanvas implememntation to update the contents TD className
//<
stateChanged : function () {

    

    if (this.redrawOnStateChange || !this.isDrawn()) {
        return this.Super("stateChanged");
    } else {
        var stateName = this.isPrinting ? this.getPrintStyleName() : this.getStateName();

        // if the border properties are on the DIV, apply them to the element's handle now
        if (isc.StatefulCanvas.pushTableBorderStyleToDiv) this._applyBorderStyle(stateName);
        if (isc.StatefulCanvas.pushTableShadowStyleToDiv && this._getHandleOverflow() === isc.Canvas.HIDDEN) {
            this._applyShadowStyle(stateName);
        }

        
        if (!this.suppressClassName) this.setClassName(stateName);
        else this.setTableClassName(stateName);

        if (this.icon) {
            // NOTE: the icon may or may not actually change to reflect states or selectedness,
            // but either state or selectedness or both may have just changed, and we may be
            // transitioning from a state we do show to a state we don't, so no-oping is
            // tricky; we don't both for now.
            this.setImage(this._$icon, this._getIconURL());

            if (this.iconStyle != null) this.getImage(this._$icon).className = this.iconStyle + this._getIconStyleSuffix();
        }

        // If we have a titleStyle and we are using a subtable, then update the styles of the
        // subtable's cells.
        var TD;
        if (this.titleStyle && (TD = this.getTitleCell()) != null) {
            var firstChild = TD.firstChild;
            if (firstChild != null && firstChild.tagName == this._$TABLE) {
                var titleStyleName = this.isPrinting ? this.getPrintStyleName() : this.getTitleStateName();

                
                var cells = firstChild.rows[0].childNodes;
                for (var i = 0; i < cells.length; i++) {
                    cells[i].className = titleStyleName;
                }
            }
        }
    }
},

// Set the css className of the table cell
_$TABLE: "TABLE",
setTableClassName : function (newClass){
    if (isc.StatefulCanvas.pushTableBorderStyleToDiv) {
        this._cachedBorderSize = null;
    }

    var TD = this.getTitleCell();
    if (!TD) return;
    if (TD.className != newClass) TD.className = newClass;

    if (this.icon && !this.noIconSubtable && !this.titleStyle) { 
        // if we're using a subtable, update the style on the title cell too (it won't
        // cascade).
        
        var firstChild = TD.firstChild;
        if (firstChild != null && firstChild.tagName == this._$TABLE) {
            
            var cells = firstChild.rows[0].children;
            if (cells != null) {
                for (var i = 0; i < cells.length; i++) {
                    if (cells[i] && cells[i].className != newClass) cells[i].className = newClass;
                }
            }
        }
    }

    
    if (this.overflow == isc.Canvas.VISIBLE) {
        
        this._resetHandleOnAdjustOverflow = true;
        this.adjustOverflow("table style changed");
    }
},


getScrollWidth : function (recalculate,a,b,c) {
    if (!recalculate || !this.isDrawn() || !(isc.Browser.isMoz && isc.Browser.isMac && isc.Browser.version >= 4)) {
        return this.invokeSuper(isc.Button, "getScrollWidth", recalculate,a,b,c);
    } else {
        var tableElem = this._getTableElement();
        
        var range = this.getDocument().createRange();
        range.selectNode(tableElem);
        var contentsBCR = range.getBoundingClientRect();
        return Math.ceil(contentsBCR.width);
    }
},

setIcon : function (icon) {
    var hadIcon = this.icon != null;
    this.icon = icon;

    // Make sure that we're drawn before trying to set the image src or redraw().
    if (this.isDrawn()) {
        if (hadIcon && (icon != null)) this.setImage(this._$icon, this._getIconURL());
        else this.redraw();
    }
},

setIconStyle : function (iconStyle) {
    this.iconStyle = iconStyle;

    var hadIcon = this.icon != null;
    if (this.isDrawn() && hadIcon) {
        var image = this.getImage(this._$icon);
        if (image != null) {
            image.className = (iconStyle == null ? isc.emptyString
                                                 : iconStyle + this._getIconStyleSuffix());
        }
    }
},

_cellFocus : function () {
    isc.EH._setThread("cFCS");
    this.focus();
    isc.EH._clearThread();
},

// override _updateCanFocus() to redraw the button.  If the focusability of the button is changed
// and we're making use of native HTML focus / tabIndex behavior, we'll need to regenerate the 
// inner HTML.
_updateCanFocus : function () {
    this.Super("_updateCanFocus", arguments);
    if (this._useNativeTabIndex) this.markForRedraw();
},

// return the border HTML used by getTagStart
_getBorderHTML : function () {

    if (isc.StatefulCanvas.pushTableBorderStyleToDiv) {
        var stateName = this.isPrinting ? this.getPrintStyleName() : this.getStateName();

        var borderHTML = this.border != null ? ";BORDER:" + this.border : "";
        borderHTML += isc.StatefulCanvas._getBorderCSSHTML(this.border != null, stateName);
        // Also apply box-shadow CSS text. Not technically part of the border but
        // this also needs to be shifted from the Table element to the 
        // widget handle
        if (isc.StatefulCanvas.pushTableShadowStyleToDiv && this._getHandleOverflow() === isc.Canvas.HIDDEN) {
            borderHTML += ";" + isc.StatefulCanvas._getShadowCSSHTML(stateName);
        }
        return borderHTML;
    }

    var borderHTML = this.Super("_getBorderHTML", arguments);
    if (isc.StatefulCanvas.pushTableShadowStyleToDiv && this._getHandleOverflow() === isc.Canvas.HIDDEN) {
        var stateName = this.isPrinting ? this.getPrintStyleName() : this.getStateName(),
            shadowCSS = isc.StatefulCanvas._getShadowCSSHTML(stateName);
        if (shadowCSS != isc.emptyString) {
            shadowCSS = ";" + shadowCSS;
            borderHTML = borderHTML == null ? shadowCSS : borderHTML + shadowCSS;
        }
    }

    return borderHTML;
},

_applyBorderStyle : function (className) {
    var styleHandle = this.getHandle().style,
        properties = isc.StatefulCanvas._buildBorderStyle(this.border != null, className);

    // Reset all border styling.
    styleHandle.border = styleHandle.borderRadius = isc.emptyString;

    isc.addProperties(styleHandle, properties);
},

_applyShadowStyle : function (className) {
    var styleHandle = this.getHandle().style,
        properties = isc.StatefulCanvas._buildShadowStyle(className);

    // Reset all shadow styling
    styleHandle.boxShadow = isc.emptyString;

    isc.addProperties(styleHandle, properties);
},

// CSS class that actually governs what borders appear on the handle.
// This is overridden in Button.js where we apply the baseStyle + modifier to the
// handle directly.
_getBorderClassName : function () {
    if (isc.StatefulCanvas.pushTableBorderStyleToDiv) {
        return this.getStateName();
    }
    return this.Super("_getBorderClassName", arguments);
}

//>	@method	button.setAlign()
// Sets the (horizontal) alignment of this buttons content.
//  @group positioning
//  @visibility external
//<
// defined in StatefulCanvas

//>	@method	button.setVAlign()
// Sets the vertical alignment of this buttons content.
//  @group positioning
//  @visibility external
//<
// defined in StatefulCanvas
    
});	// END	isc.Button.addMethods()

isc.Button.registerStringMethods({
    getTitle:null
});


// AutoFitButton
// --------------------------------------------------------------------------------------------
// Button that automatically sizes to the title text.

//> @class AutoFitButton
//
// A button that automatically sizes to the length of its title.  Implemented via the 
// +link{StatefulCanvas.autoFit} property.
//
// @deprecated As of Isomorphic SmartClient version 5.5, autoFit behavior can be achieved using
// the Button class instead by setting the property +link{Button.autoFit} to true.
//
// @see Button
// @treeLocation Client Reference/Control/Button
// @visibility external
//<

isc.ClassFactory.defineClass("AutoFitButton", "Button");

isc.AutoFitButton.addProperties({
    autoFit:true
});




isc.Button.registerStringMethods({
    //>@method Button.iconClick()
    // If this button is showing an +link{Button.icon, icon}, a separate click handler for the
    // icon may be defined as <code>this.iconClick</code>.
    // Returning false will suppress the standard button click handling code.
    // @return (boolean) false to suppress the standard button click event
    // @group buttonIcon    
    // @visibility external
    //<
    // don't expose the parameters - they're not really useful to the developer
    iconClick:"element,ID,event",

    //> @method button.titleHover()
    // Optional stringMethod to fire when the user hovers over this button and the title is
    // clipped. If +link{Button.showClippedTitleOnHover} is true, the default behavior is to
    // show a hover canvas containing the HTML returned by +link{Button.titleHoverHTML()}.
    // Return false to suppress this default behavior.
    // @return (boolean) false to suppress the standard hover
    // @see Button.titleClipped()
    // @group hovers
    // @visibility external
    //<
    titleHover:""
});


// Make "IButton" a synonym of Button by default.

//>	@class	IButton
//
// The IButton widget class is a class that implements the same APIs as the 
// +link{class:Button} class.  Depending on the current skin, <code>IButton</code>s may be
// on the +link{StretchImgButton} component, which renders via images, or may be based on the
// +link{Button} component, which renders via CSS styles.
//
// @treeLocation Client Reference/Control
// @visibility external
//<

isc.addGlobal("IButton", isc.Button);

