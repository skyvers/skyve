/*
 * Isomorphic SmartClient
 * Version v11.0p_2016-03-31 (2016-03-31)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */
//>	@class	AutoFitTextAreaItem
// Class for editable multi-line text areas (uses HTML <code>&lt;TEXTAREA&gt;</code> object)
// automatically expands to accomodate its content
// @visibility external
// @example textAreaItem
//<
isc.ClassFactory.defineClass("AutoFitTextAreaItem", "TextAreaItem");


isc.AutoFitTextAreaItem.addProperties({

    // getScrollHeight / getScrollWidth
    // Neither IE nor moz seems to reliably support the scroll height of a text area if it
    // is less than the inner size of the text box. This means that if we've grown to accommodate
    // content we cant reliably detect when the content is shrunk by looking at our text-box.
    // We therefore create an offscreen 'tester' textarea which remains at the specified size
    // and measure the scrollHeight / scrollWidth of that element
    getTestBox : function (forceResize) {
        var dataValue = this.getValue();
        var value = this.mapValueToDisplay(dataValue);
        var AFTAI = isc.AutoFitTextAreaItem;
        if (!AFTAI._testBoxCanvas) {
            AFTAI._testBoxCanvas = isc.Canvas.create({
                autoDraw:true, 
                overflow:"hidden",
                left:0, top:-100,
                ariaState: {
                    hidden: true
                },
                contents:
                    ["<textarea ID='isc_autoFitTextArea_sizeTester' role='presentation' ",
                     "style='overflow:hidden;",
                     (isc.Browser.isIE ?
                        "margin-top:-1px;margin-bottom:-1px;margin-left:0px;margin-right:0px;" :
                        "margin:0px;"),
                     "'></textarea>"].join("")
            });
        }
        var box = isc.Element.get("isc_autoFitTextArea_sizeTester");   
        // Match the text box's className and CSS Text to ensure our measurement is 
        // accurate
        if (AFTAI.currentItem != this || forceResize) {
            box.className = this.getTextBoxStyle();

            // would be nice to apply this.getElementCSSText directly but doesn't seem to be
            // an obvious way to do this.
            if (isc.Browser.isMoz) {
                if (isc.isA.String(this.wrap) && this.wrap.toLowerCase() != "off") {
                    box.rows = 5; box.cols = 10;
                } else {
                    box.rows = ""; box.cols = "";
                }
            }
            
            box.setAttribute("wrap", this.wrap);

            // use assignSize - this will call setAttribute if necessary.
            var textBoxWidth = this.getTextBoxWidth(dataValue),
                textBoxHeight = this.getTextBoxHeight(dataValue);
            AFTAI._testBoxCanvas._assignSize(box.style, "width", textBoxWidth);
            AFTAI._testBoxCanvas._assignSize(box.style, "height", textBoxHeight);

            box.style.textAlign = this.textAlign || "";

            box.cssText = this.getElementCSSText(textBoxWidth, textBoxHeight);
            AFTAI.currentItem = this;
        }

        box.value = value;
        
        var touch = box.scrollHeight;

        return box;
    },
    
    getScrollHeight : function (resized) {
        var testBox = this.getTestBox(resized);
        return testBox.scrollHeight;
    },
    
    getScrollWidth : function (resized) {
        var testBox = this.getTestBox(resized);
        return testBox.scrollWidth;
    },
    
    // force overflow to be hidden
    // Note: we're writing out the specified size rather than the overflowed size - this is 
    // appropriate to force wrapping in the right places - we'll check the rendered size after
    // drawing and resize if necessary
    getElementCSSText : function (width, height) {
        var txt = this.Super("getElementCSSText", arguments);
        txt += "overflow:hidden;"
        return txt;
    },
    
    // These methods are required to determine the delta between the specified size of the
    // TextArea and the available space for content
    _getTextBoxHPadding : function () {
        if (this._tbhpadding != null) return this._tbhpadding;
        var textBox = this.getDataElement();
        if (!textBox) return 0;
        var leftPadding = 
                parseInt(isc.Element.getComputedStyleAttribute(textBox, "paddingLeft")),
            rightPadding =
                parseInt(isc.Element.getComputedStyleAttribute(textBox, "paddingRight")),
            hPadding = (isc.isA.Number(leftPadding) ? leftPadding : 0) +
                       (isc.isA.Number(rightPadding) ? rightPadding : 0);
                               
        this._tbhpadding = hPadding;
        return hPadding;
        
    },
    _getTextBoxVPadding : function () {
        if (this._tbvpadding != null) return this._tbvpadding;
        var textBox = this.getDataElement();
        if (!textBox) return 0;
        // In IE we've seen textBox.currentStyle be reported as null in some cases
        // if this happens, don't cache that value
        if (isc.Browser.isIE && textBox.currentStyle == null) return 0;
        var topPadding = 
                parseInt(isc.Element.getComputedStyleAttribute(textBox, "paddingTop")),
            bottomPadding = 
                parseInt(isc.Element.getComputedStyleAttribute(textBox, "paddingBottom")),
            vPadding = (isc.isA.Number(topPadding) ? topPadding : 0) + 
                       (isc.isA.Number(bottomPadding) ? bottomPadding : 0);
        this._tbvpadding = vPadding;
        return vPadding;
    },
    
    //> @attr autoFitTextAreaItem.maxHeight (integer : null : IRW)
    // If specified, the autoFitTextArea will not grow taller than this height
    // @visibility external
    //<    
    getMaxHeight : function () {
        return this.maxHeight;
    },
    
    //> @attr autoFitTextAreaItem.maxWidth (integer : null : IRW)
    // If specified, the autoFitTextArea will not grow wider than this width
    // @visibility internal
    //<
    
    getMaxWidth : function () {
        return this.maxWidth;
    },


    updateSize : function (resized) {
        var textBox = this.getDataElement();
        if (!textBox) return;

        var resetHandle, sizeChanged;

        var specifiedHeight = this.getTextBoxHeight(),
            vPadding = this._getTextBoxVPadding(),
            scrollHeight = this.getScrollHeight(resized),
            boxHeight = textBox.offsetHeight,
            calcHeight = scrollHeight + vPadding,
            maxHeight = this.getMaxHeight();
        if (maxHeight != null && maxHeight < calcHeight) {
            calcHeight = maxHeight;
        }
        if (calcHeight > boxHeight) {
            // stringifying the height makes no difference to non-css3 mode but is required 
            // for css3 mode
            textBox.style.height = "" + calcHeight + "px";
            sizeChanged = true;

            // Catch the case where the box is shrinking
        } else if (calcHeight < boxHeight && boxHeight > specifiedHeight) {
            if (calcHeight < boxHeight) {
                // If we're shrinking, the dynamicForm will need to _resetHandleOnAdjustOverflow
                // to detect the shrinking of contents
                resetHandle = true;
                // stringifying the height makes no difference to non-css3 mode but is required
                // for css3 mode 
                textBox.style.height = "" + Math.max(calcHeight, specifiedHeight) + "px";
            }
            sizeChanged = true
        }

        // width is trickier - we can expand easily to fit a non-wrapping line of text but
        // it will be hard to shrink since content will not rewrap smaller.
        var specifiedWidth = this.getTextBoxWidth(this.getValue()),
            hPadding = isc.Browser.isIE ? 0 : this._getTextBoxHPadding(),
            scrollWidth = this.getScrollWidth(resized),
            boxWidth = textBox.offsetWidth,
            calcWidth = hPadding + scrollWidth,
            maxWidth = this.getMaxWidth();
        if (maxWidth != null && calcWidth > maxWidth) calcWidth = maxWidth;

        if (calcWidth > boxWidth) {
            textBox.style.width = calcWidth;
            sizeChanged = true;

        } else if (calcWidth < boxWidth && boxWidth > specifiedWidth) {
            textBox.style.width = Math.max(specifiedWidth, calcWidth);
            resetHandle = true
            sizeChanged = true
        }

        if (resetHandle) this.containerWidget._resetHandleOnAdjustOverflow = true;
        if (sizeChanged) this.adjustOverflow("Updated size to fit content");
    },

    handleChanged : function () {
        this.updateSize();
        return this.Super("handleChanged", arguments);
    },
    drawn : function () {
        this.Super("drawn", arguments);
        delete this._tbhpadding;
        delete this._tbvpadding;
        this.updateSize(true);
    },
    redrawn : function () {        
        this.Super("redrawn", arguments);
        delete this._tbhpadding;
        delete this._tbvpadding;
        this.updateSize(true);
    },
    _resetWidths : function () {
        this.Super("_resetWidths", arguments);
        this.updateSize(true);
    },

    // supportsSelectionRange - does getSelectionRange() return null on this item? (IE only)
    // See FormItem._getIESelectionRange() for background on this
    // Enable this in AutoFitTextAreas where modifying the value of the item is likely to change
    // the form's geometry and cause redraws and you really don't want to lose cursor positioning 
    supportsSelectionRange:true
});
