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

 






//>	@class	TextAreaItem
//
//	Class for editable multi-line text areas (uses HTML <code>&lt;TEXTAREA&gt;</code> object)
// @visibility external
// @example textAreaItem
//<
isc.ClassFactory.defineClass("TextAreaItem", "FormItem");

isc.TextAreaItem.addClassProperties({
    

	//>	@type	TEXTAREA_WRAP
    //	@value	isc.TextAreaItem.OFF  don't allow wrapping at all
	OFF : "OFF",                

    //	@value	isc.TextAreaItem.SOFT   when the entered text reaches the edge of the text area,
    //                                  wrap visibly but don't include line breaks in the textarea
    //                                  value
	SOFT : "SOFT",
	VIRTUAL : "SOFT",			

    //	@value	isc.TextAreaItem.HARD   when the entered text reaches the edge of the text area, 
    //                                  insert a line break
	ON : "HARD",				
	HARD : "HARD",			
	PHYSICAL : "HARD"	
    //
    // @visibility external
	//<
});

isc.TextAreaItem.addProperties({
    //>	@attr	textAreaItem.wrap		(TEXTAREA_WRAP : isc.TextAreaItem.VIRTUAL : IRW)
	// Text wrapping style.
	//		@group	appearance
    // @visibility external
	//<
	wrap:isc.TextAreaItem.VIRTUAL,
	
    //>	@attr	textAreaItem.width		(number : 150 : IRW)
	//			default width of this item
	//		@group	appearance
    // @visibility external
	//<
	width:150,

    //>	@attr	textAreaItem.height		(number : 100 : IRW)
    // Default height of this item
    // <p>
    // Note that when item is rendered as read-only with <code>readOnlyDisplay</code> as "static"
    // the property +link{formItem.staticHeight} is used instead.
    //
    // @group	appearance
    // @visibility external
    //<
    height:100,

    //> @attr textAreaItem.staticHeight   (Integer : 1 : IR)
    // @include formItem.staticHeight
    //<
    staticHeight:1,

    //>	@attr	textAreaItem.textBoxStyle (FormItemBaseStyle : "textItem" : IRW)
	//  Base CSS class to apply to this item's input element.
    // NOTE: See the +link{group:CompoundFormItem_skinning} discussion for special skinning considerations.    
    // 
	// @group   appearance
    // @visibility external
	//<
	textBoxStyle:"textItem",		

    //> @attr textAreaItem.browserAutoCapitalize
    // @include FormItem.browserAutoCapitalize
    // @visibility external
    //<

    //> @attr textAreaItem.browserAutoCorrect
    // @include FormItem.browserAutoCorrect
    // @visibility external
    //<

    //>	@attr	textAreaItem.length		(number : null : IRW)
	// If set, maximum number of characters for this field. If +link{enforceLength} is
	// set to true, user input will be limited to this value, and values exceeding this
	// length passed to +link{formItem.setValue(),setValue()} will be trimmed. Otherwise values exceeding the
	// specified length will raise an error on validation.
	// <P>
	// See also +link{dataSourceField.length}.
	// @group	validation
    // @visibility external
	//<
	length:null,
	
	//> @attr textAreaItem.enforceLength (boolean : false : IRW)
	// If a +link{textAreaItem.length} is specified for this item, should user input be limited
	// to the specified length? If set to true, user input and values passed to 
	// +link{formItem.setValue(),setValue()} will be trimmed to the specified length. Otherwise values
	// exceeding the specified length will raise an error on validation.
	// <P>
	// Note that having this value set to true limits user interactivity in some ways.
	// For example users would be unable to paste a longer string into the field for
	// editing without seeing it be truncated. Given how text areas are typically
	// used to edit longer values than non-wrapping +link{textItem}s, this value is
	// false by default for textAreaItems.
	//
	// @visibility external
	//<
	enforceLength:false,
	
    // Override redrawOnShowFormIcon - we can handle dynamically updating the item's HTML to
    // show / hide textArea item icons
    redrawOnShowIcon:false,
    // setting clipValue to true ensures we resize the text box when showing/hiding icons
    clipValue:true,


    //> @attr   textAreaItem._hasDataElement    (boolean : true : IRW)
    //      Text areas have a data element.
    // @group formValues
    // @visibility   internal
    // @see     method:FormItem.hasDataElement
    // @see     method:FormItem.getDataElement
    //<
    _hasDataElement:true,
    
    // This flag means updateState will apply the result of this.getTextBoxStyle() to this item's
    // data element - appropriate for native text boxes, text areas and selects.
    _dataElementIsTextBox:true,

    //> @attr   textAreaItem.emptyStringValue   (any : null : IRW)
    // @include textItem.emptyStringValue
    //<
    
    emptyStringValue:null,

    //> @attr   textAreaItem.lineBreakValue  (string : "\n" : IRW)
    //  What character string should be used to represent line breaks?<br>
    //  Multi-line values edited in TextAreaItems will use this string
    //  as a line separator.
    // @group formValues
    // @visibility   psft
    //<
    
    lineBreakValue:"\n",

    //> @attr   textAreaItem.iconVAlign  (VerticalAlignment : isc.Canvas.TOP : IR)
    //  Align icons with the top edge of text area icons by default.
    //  @group  formIcons
    // @visibility   external
    //<
    iconVAlign:isc.Canvas.TOP,
    
    // _nativeEventHandlers is a place to specify native event handlers to be applied to the
    // form item element once it has been written into the DOM (without having to override 
    // '_applyHandlersToElement()'
    _nativeEventHandlers : {
        
        onmousedown : (
            isc.Browser.isIE ? function () {
                var element = this,
                    itemInfo = isc.DynamicForm._getItemInfoFromElement(element),
                    item = itemInfo.item;
                if (item) item._setupFocusCheck();

            } : null
        )
    },

    //> @method textAreaItem.getEnteredValue()
    // @include textItem.getEnteredValue()
    // @visibility external
    //<
    getEnteredValue : function () {
        return this.getElementValue();
    },

    //>@attr TextAreaItem.browserSpellCheck (boolean : null : IRWA)
    // @include FormItem.browserSpellCheck
    // @visibility internal
    //<
    
    //>@attr TextAreaItem.selectOnFocus (boolean : null : IRW)
    // @include FormItem.selectOnFocus
    // @visibility external
    //<

    //>@attr TextAreaItem.selectOnClick (boolean : null : IRW)
    // @include FormItem.selectOnClick
    // @visibility external
    //<
    
    //>@attr TextAreaItem.changeOnKeypress (Boolean : true : IRW)
    // @include FormItem.changeOnKeypress
    // @visibility external
    //<
    
    //>@method TextAreaItem.getSelectionRange()
    // @include FormItem.getSelectionRange()
    // @visibility external
    //<
    
    //>@method TextAreaItem.setSelectionRange()
    // @include FormItem.setSelectionRange()
    // @visibility external
    //<
    
    //>@method TextAreaItem.selectValue()
    // @include FormItem.selectValue()
    // @visibility external
    //<
    
    //>@method TextAreaItem.deselectValue()
    // @include FormItem.deselectValue()
    // @visibility external
    //<
    
    // supportsSelectionRange - does getSelectionRange() return null on this item? (IE only)
    // See FormItem._getIESelectionRange() for background on this
    // May cause poor performance determining selection range (for example on redraw) in 
    // items with a lot of content
    supportsSelectionRange:true,

    //>@attr TextAreaItem.showHintInField (boolean : null : IRWA)
    // If showing hint for this form item, should it be shown within the field?
    // <P>CSS style for the hint is +link{textAreaItem.textBoxStyle} with the suffix
    // "Hint" appended to it. 
    // @group appearance
    // @see FormItem.hint
    // @visibility external
    //<

    //>@attr TextAreaItem.printFullText (Boolean : true : IRW)
    // When generating a print-view of the component containing this TextArea, should
    // the form item expand to accommodate its value? If set to false the text box not expand
    // to fit its content in the print view, instead showing exactly as it does in the
    // live form, possibly with scrollbars.
    // @visibility external
    // @group printing
    //<
    printFullText:true,

    showClippedValueOnHover:false
});

isc.TextAreaItem.addMethods({

    _getShowHintInField : function () {
        return !!(this.showHint && this.showHintInField);
    },

    // Don't allow any valueIcon to appear on a different line from the text area
    getTextBoxCellCSS : function () {
        return this._$nowrapCSS;
    },

    
    _sizeTextBoxAsContentBox : function () {
        return isc.Browser.isStrict;
    },


    // NOTE: this is here for doc generation
    //>	@method textAreaItem.keyPress		(A)
    //		@group	event handling
    //			event handler for keys pressed in this item
    //<


    _needHideUsingDisplayNone : function () {
        return isc.Browser.isTouch;
    },


    // _willHandleInput()
    // Can we use the "input" event in this browser / form item?
    // True for Moz and Safari, but not IE. See comments near FormItem._handleInput()
    _willHandleInput : function () {
        return !isc.Browser.isIE;
    },

    //> @method textAreaItem.setElementReadOnly()
    // Change the read-only state of the form element immediately.
    //<
    setElementReadOnly : function (readOnly) {
        // TextArea HTML element has readonly property
        this._setElementReadOnly(readOnly);
    },

    //> @method textAreaItem.getElementHTML() (A)
    // Output the HTML for a text field element
    //
    // @param value (string)  Value of the element
    //                        [Unused because it is more reliably set by setValue].
    // @return (HTML)  HTML output for this element
    // @group drawing
    //<
    getElementHTML : function (value, dataValue) {
        // remember which element number we wrote this out as
        var form = this.form,
            formID = form.getID(),
            itemID = this.getItemID(),
            output = isc.StringBuffer.create(),
            valueIconHTML = this._getValueIconHTML(dataValue);
        if (valueIconHTML != null) output.append(valueIconHTML);
        if (!this.showValueIconOnly) {
            if ((!this.printFullText || !this._isPrinting()) && !this.renderAsStatic()) {
                output.append(
                    "<TEXTAREA NAME=" , this.getElementName(),
                    " ID=", this.getDataElementId(),

                    // hang a flag on the element marking it as the data element for the
                    // appropriate form item.
                    this._getItemElementAttributeHTML(),

                    this.getElementStyleHTML(),
                    (this.renderAsDisabled() ? " DISABLED " : ""),

                    // disable native autoComplete 
                    (this._getAutoCompleteSetting() != "native" ? " AUTOCOMPLETE=OFF " : ""),

                    // enable / disable native spellcheck in Moz
                    // Same setting in Safari - see comments in TextItem.js
                    ((isc.Browser.isMoz || isc.Browser.isSafari) ? 
                        (this.getBrowserSpellCheck() ? " spellcheck=true" : " spellcheck=false") :
                        null),
                    (isc.Browser.isSafari && this.browserAutoCapitalize == false
                        ? " autocapitalize='off'"
                        : null),
                    (isc.Browser.isSafari && this.browserAutoCorrect != null
                        ? (this.browserAutoCorrect ? " autocorrect='on'" : " autocorrect='off'")
                        : null),

                    " WRAP=", this.wrap,

                    " TABINDEX=", this._getElementTabIndex(),
                    (this.showTitle == false && this.accessKey != null ? 
                        " ACCESSKEY=" + this.accessKey : ""),

                    // If this browser supports the "input" event write out a handler for it.
                    (this._willHandleInput() ? " ONINPUT='" + this.getID() + "._handleInput()'"
                                             : null),

                    // If the readonly property is set, set it on the handle too
                    (this.isReadOnly() || this.isInactiveHTML() ? 
                        (isc.screenReader ? "aria-readonly=true READONLY=TRUE" :
                            " READONLY=TRUE") : null),

                    

                    // Ensure we pass events through the ISC event handling system.
                    " handleNativeEvents=false>",
                    (this.isInactiveHTML() ? value : null),
                    "</TEXTAREA>"
                );
            } else {
                if (value == null) value ="";
                // use a div with no sizing info specified. This'll fill the available
                // space in the form cell (and expand vertically as required)
                // note asHTML() to convert \n to <br> etc.
                output.append(
                    "<DIV style='",
                    // if width is specified as a number write it out
                    // if it's specified as "*" etc don't (we can't size based on the
                    // DyanmicForm's rendered width since printHTML is not rendered into
                    // the form!)
                    (isc.isA.Number(this.width) ? "width:" + this.width + "px;" : null)
                    ,"' class='", this.getTextBoxStyle(), "'>", value.asHTML(), "</DIV>"
                );
            }
        }
            
        //this.logWarn("textArea HTML:"+ output);
        return output.release(false);
    },
    
    //> @attr TextAreaItem.allowNativeResize (boolean : false : IRA)
    // Modern browsers allow drag-resizing of TextArea items. This flag may be set to enable
    // or suppress this behavior where supported.
    // @visibility internal
    //<
    
    allowNativeResize : false,
    handleMouseMove : function () {
        var returnVal = this.Super("handleMouseMove", arguments);
        if (returnVal == false || !this.allowNativeResize) return false;
        
        if (isc.EH.mouseIsDown() && this._resizeCheck == null) {
            this._resizeCheck = isc.Page.setEvent("idle", this.getID() + "._checkForElementResize()");
        }
    },
    _checkForElementResize : function () {
        var resized = false;
        var element = this.getDataElement();
        if (element) {
            if (element.offsetWidth != this.getTextBoxWidth()) resized = true;
            if (element.offsetHeight != this.getTextBoxHeight()) resized = true;
        }
        if (resized) this._nativeElementResize();
        
        if (!isc.EH.mouseIsDown()) {
            isc.Page.clearEvent("idle", this._resizeCheck);
            this._resizeCheck = null;
        }
    },
    
    _nativeElementResize : function () {
        var widget = this.containerWidget;
        if (widget) widget._markForAdjustOverflow("Native textarea resize");
    },
    
    // When focus is received, the hint should be hidden if TextAreaItem.showHintInField is true.
    _nativeElementFocus : function (element, itemID) {
        var returnVal = this.Super("_nativeElementFocus", arguments);

        // Hide in-field hint if being shown
        this._hideInFieldHint();
        
                
        // There may be custom parser / formatter logic applied to any text item and this
        // may not be a 1:1 mapping [EG a forgiving date format parser allowing variants on
        // a display format].
        // As with TextItem, store element value at focus so we can compare at blur time and
        // if necessary re-run the formatter on the stored item value
        // - note that by comparing element values rather than checking for changed data value we
        // catch the case where the user modified the display value to something that ultimately
        // maps back to the same data value [in which case a simple dataVal changed check might fail]
        this._elementValueAtFocus = this.getEnteredValue();
        
        return returnVal;
    },

    // Override _nativeElementBlur to fire 'change' explicitly in response to blur rather than
    // relying on the native 'ONCHANGE' handler method
    // (as with textItem)
    _nativeElementBlur : function (element, itemID) {
        var returnVal = this.Super("_nativeElementBlur", arguments);
        
        // Always fire elementChanged(). This will fall through to updateValue which will
        // no-op if the value is actually unchanged.
        this.form.elementChanged(this);
        
          
        // As with TextItem, call mapValueToDisplay() so we format the stored value to 
        // the appropriate display value.
        // Required if a developer has custom formatters/parsers that are not 1:1
        // [EG: A forgiving data parser allowing variants on a display format].
        // Use the "_elementValueAtFocus" to avoid firing this unnecessarily.
        if (this._elementValueAtFocus == null ||
            this._elementValueAtFocus != this.getEnteredValue())
        {
            var value = this.getValue();
            if (this.mapValueToDisplay) {
                value = this.mapValueToDisplay(value);
            }
            this.setElementValue (value);
        }

        // If showing hint within data field, see if it should be shown now.
        if (this._getShowHintInField()) {
            var undef;
            var value = this.getElementValue();
            if (value === undef || value == null || isc.is.emptyString(value)) {
                this._showInFieldHint();
            }
        }

        return returnVal;
    },

	//>	@method	textAreaItem.getElementStyleHTML()	(I)
    //      	Get the HTML string used to set the visual characteristics for a textArea item.
    //          This includes the STYLE=... & CLASS=... properties to be written into this
    //          form item's element.
	//			This varies by platform, as we attempt to make Netscape think in pixels rather than 
    //          characters and rows
	//
	//		@group	appearance
	//		@return	(string)    String of HTML containing STYLE=... & CLASS=... properties for 
    //                          this items element.
	//
	//<
	getElementStyleHTML : function () {
        
        var width = this.getTextBoxWidth(),
            height = this.getTextBoxHeight();

        return isc.StringBuffer.concat(
            " CLASS='" + this.getTextBoxStyle(),
            
            (isc.Browser.isMoz && isc.isA.String(this.wrap) && this.wrap.toLowerCase() != "off" ? 
                      "' ROWS=10 COLS=10" : "'"),
            " STYLE='",
            this.getElementCSSText(width,height),
            "' ");
    },

    //> @attr TextAreaItem.minHeight (int : 16 : IRW)
    // Minimum valid height for this TextAreaItem in px. If the specified +link{TextAreaItem.height}
    // is less than this value, the text area will still render at this height.
    // @visibility external
    //<
    minHeight:16,

    // helper to return the content of the "style" tag in the text box / data element
    getElementCSSText : function (width, height) {
        if (isc.isA.Number(width) && width <= 0) width = 1;
        if (isc.isA.Number(height) && height < this.minHeight) height = this.minHeight;

        return isc.StringBuffer.concat(
            
            this.allowNativeResize ? null : "resize:none;",
            // Ensure there's no margin(helps with sizing and v-alignment with icons)
            
            (isc.TextItem._needNegativeMargins
             ? "margin-top:-1px;margin-bottom:-1px;margin-left:0px;margin-right:0px;"
             : "margin:0px;"),
            (isc.isA.Number(width) 	? "WIDTH:" + width + "px;" : ""),
            (isc.isA.Number(height)	? "HEIGHT:" + height + "px;" : ""),
            // text align property, known to be supported in IE6 and Moz/Firefox on
            // Windows, not supported on Safari 1.2
            (this.textAlign ? "text-align:" + this.textAlign + ";" : ""),

            
            (isc.Browser.isChrome ? "display:block;" : ""),
             // In Mozilla we must use the 'moz-user-focus' css property to govern
             // whether this element can recieve focus or not.
             (isc.Browser.isMoz ? 
                    "-moz-user-focus:" + (this._getElementTabIndex() > 0 ? "normal;" 
                                                                        : "ignore;") :
                    "")
        );
    },


	//>	@method	textAreaItem.mapValueToDisplay()	(A)
	//	Map from the internal value for this item to the display value.
	//		@group	drawing
	//		@param	internalValue		(string)	Internal value for this item.
	//		@return	(string)	Displayed value corresponding to internal value.
	//<
	mapValueToDisplay : function (internalValue, a,b,c,d) {
        var value = this.invokeSuper(isc.TextAreaItem, "mapValueToDisplay", internalValue, a,b,c,d);
        // always display the empty string for null values, rather than "null" or "undefined"
        if (value == null) value = isc.emptyString;
        return value;
        
	},
	
	//>	@method	textAreaItem.mapDisplayToValue()	(A)
	//	Map from a the display value for this item to the internal value.
	//		@group	drawing
	//
	//		@param	displayValue	(string)	Value displayed to the user.
	//		@return	(string)	Internal value corresponding to that display value.
	//<
	mapDisplayToValue : function (displayValue) {
        var value = this._unmapKey(displayValue); 
        value = this._parseDisplayValue(value);
        // if the value to be saved is an empty string, map it to 'null' if necessary
        if (isc.is.emptyString(value)) value = this.emptyStringValue;
        return value;
	},
	
    //> @attr textAreaItem.formatOnBlur (Boolean : false : IRW)
    // With <code>formatOnBlur</code> enabled, this textAreaItem will format its value
    // according to any specified static +link{formItem.formatValue(),static formatter}
    // as long as the item does not have focus. Once the user puts focus into the item
    // the formatter will be removed. This provides a simply way for developers to
    // show a nicely formatted display value in a freeform text field, without the need
    // for an explicit +link{formItem.formatEditorValue()} 
    // and +link{formItem.parseEditorValue()} pair.
    // @visibility external
    //<
    // Implemented at the FormItem level.

    
    // Don't apply arbitrary formatters specified via SimpleType definitions to this item's
    // display value - we have no way to parse it back to a real data value
    applyStaticTypeFormat:false,
    
    // override 'setValue' to update the data value to store when the element value is set to 
    // the empty string.
    // See Text item setValue override for full description
    setValue : function (value) {

        // Make sure in-field hint is hidden
        this._hideInFieldHint();

        
        var undef;
        if (value !== undef && (value == null || isc.is.emptyString(value))) 
            this.emptyStringValue = value;

        // Also clear out the '_hasEditedValue' flag, used to handle line break conversions
        // (See comments by the 'lineBreakValue' property)
        delete this._hasEditedValue;
            
        // Let parent take care of saving the value
        value = this.Super("setValue", arguments);

        // See if the in-field hint needs to be shown
        if (!this.hasFocus && this._getShowHintInField() && this.getHint()) {
            if (value === undef || value == null || isc.is.emptyString(value)) {
                this._showInFieldHint();
            }
        }

        return value;
    },

    // Override 'updateValue()' to set a flag on this item marking it as having been edited.
    // This is used by 'getValue()' to determine whether we should convert line breaks to
    // the lineBreakValue for this item.
    updateValue : function () {
        this._hasEditedValue = true;
        return this.Super("updateValue", arguments);
    },

    compareValues : function (value1, value2) {
        if (value1 == null && value2 == null) return true;
        else if (value1 == null || value2 == null) return false;

        // Because a TextAreaItem is whitespace-sensitive, we don't want an actual number to be
        // treated as equal to a string of the number prefixed or suffixed with whitespace. For
        // example, we want the value 0 to be considered distinct from "0\n".
        // Also, 0 needs to be distinct from "-0" and "+0", and numbers have to be distinct from
        // a the string of the number with a unary plus (e.g. 8 distinct from "+8"), or prefixed
        // with zeroes (e.g. 7 distinct from "007"), or with a ".0" suffix.
        //
        // We don't have to worry about this problem for booleans because `true == "true"' and
        // `false == "false"' are both false.
        if (isc.isA.String(value2)) {
            if (isc.isA.Number(value1) || isc.isA.SpecialNumber(value1)) {
                return String(value1) == value2;
            }
        } else if (isc.isA.String(value1)) {
            if (isc.isA.Number(value2) || isc.isA.SpecialNumber(value2)) {
                return value1 == String(value2);
            }
        }

        return this.Super("compareValues", arguments);
    },

    // Override getValue() to convert any line break characters to this.lineBreakValue.
    // See comments by this.lineBreakValue definition for why we do this.
    getValue : function () {
        var value = this.Super("getValue", arguments);
        if (this._hasEditedValue && isc.isA.String(value)) {
            // replace every line break with our line break char string
            if (!this._lineBreakValueRegex) 
                this._lineBreakValueRegex = new RegExp("(\\r\\n|[\\r\\n])", "g");
            value = "" + value;    
            value = value.replace(this._lineBreakValueRegex, this.lineBreakValue);
        }
        
        return value;
    },
    
    // Disallow bubbling of edit / navigation keys
    stopNavKeyPressBubbling:true,
    stopCharacterKeyPressBubbling:true,
    
    // Disallow bubbling of page up / page down keys (used for scrolling)
    // unless we're already at the top or bottom of the text-box
    _$Page_Up:"Page_Up",
    _$Page_Down:"Page_Down",
    _$Enter:"Enter",
    shouldStopKeyPressBubbling : function (keyName, characterValue) {
        if (keyName == this._$Enter) return true;
        if (keyName == this._$Page_Up) return this.getScrollTop() != 0;
        if (keyName == this._$Page_Down) {
            var element = this._getTextBoxElement();
            if (element) {
                return (element.scrollHeight - element.clientHeight) > this.getScrollTop();
            }
        }

        return this.Super("shouldStopKeyPressBubbling", arguments);                
    },



    // SCROLLING
    // Add support for programmatic scrolling of TextAreas
    
    
    
    getScrollHeight : function () {
        var element = this._getTextBoxElement();
        if (element == null) return this.getHeight();
        
        return element.scrollHeight;
    },
    
    getScrollWidth : function () {
        var element = this._getTextBoxElement();
        if (element == null) return this.getWidth();
        
        return element.scrollWidth;    
    },
    
    _hscrollOn : function () {
        var element = this._getTextBoxElement();
        return element && element.scrollWidth > element.clientWidth;
    },

    _vscrollOn : function () {
        var element = this._getTextBoxElement();
        return element && element.scrollHeight > element.clientHeight;
    },    
    
    getScrollTop : function () {
        var element = this._getTextBoxElement();
        if (element == null) return 0;
        return element.scrollTop;
    },
    
    getScrollLeft : function () {
        var element = this._getTextBoxElement();
        if (element == null) return 0;
        return element.scrollLeft;
    },
    
    scrollTo : function (left, top) {
        var element = this._getTextBoxElement();
        if (element == null) return;
        if (left != null) element.scrollLeft = left;
        if (top != null) element.scrollTop = top;        
    },
    
    scrollToTop : function () {
        this.scrollTo(null, 0);
    },

    scrollToBottom : function () {
        var maxScroll = this.getScrollHeight() - this.getInnerHeight();
        if (maxScroll >= 0) {
            if (this._hscrollOn()) maxScroll += this.form.getScrollbarSize();
            this.scrollTo(null, maxScroll);
        }
    }

});
