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

 





//>	@class	TextItem
//
// FormItem for managing a text field.
//
// @visibility external
// @example textItem
//<
isc.ClassFactory.defineClass("TextItem", "FormItem");

//	Add class-level properties
//		You can access these properties on the static class object.
//		e.g.,	Canvas.myStaticProperty

isc.TextItem.addClassProperties({

    //>	@type CharacterCasing
    // @visibility external
    // @group validation
    // @value isc.TextItem.DEFAULT No character translation
    DEFAULT:"default",
    // @value  isc.TextItem.UPPER  Map characters to uppercase
    UPPER:"upper",
    // @value  isc.TextItem.LOWER  Map characters to lowercase
    LOWER:"lower",
    //<

    // Filter definitions for mask characters
    _filterDefinitions: {
        '0': { charFilter: "[0-9+\\-]" },
        '#': { charFilter: "[0-9]" },
        '9': { charFilter: "[0-9 ]" },
        'L': { charFilter: "[A-Za-z]" },
        '?': { charFilter: "[A-Za-z ]" },
        'a': { charFilter: "[0-9A-Za-z]" },
        'A': { charFilter: "[0-9A-Za-z]" },
        'C': { charFilter: "." }
    },

    _needNegativeMargins: isc.Browser.isIE &&
                          isc.Browser.version <= 9 &&
                          (isc.Browser.version <= 7 || !isc.Browser.isStrict)
});

isc.TextItem.addProperties({
    //>	@attr	textItem.width		(number : 150 : IRW)
	//			Default width for fields.
	//		@group	appearance
    // @visibility external
	//<
	width:150,		
    
    //>	@attr	textItem.height		(number : 19 : IRW)
	//			Default height for text items.
	//		@group	appearance
    // @visibility external
	//<                                       
    
    height:isc.Browser.isSafari ? 22 : 19,

    //>	@attr	textItem.textBoxStyle     (FormItemBaseStyle : "textItem" : IRW)
	//  Base CSS class name for this item's input element.
    // NOTE: See the +link{group:CompoundFormItem_skinning} discussion for special skinning considerations.    
	// @group	appearance
    // @visibility external
	//<
	textBoxStyle:"textItem",		

    //>	@attr	textItem.length		(number : null : IRW)
	// If set, maximum number of characters for this field. If +link{textItem.enforceLength,enforceLength} is
	// set to true, user input will be limited to this value, and values exceeding this
	// length passed to +link{formItem.setValue(),setValue()} will be trimmed. Otherwise values exceeding the
	// specified length will raise an error on validation.
	// <P>
	// See also +link{dataSourceField.length}.
	// @group	validation
    // @visibility external
	//<
	length:null,
	
	//> @attr textItem.enforceLength (boolean : true : IRW)
	// If a +link{textItem.length} is specified for this item, should user input be limited
	// to the specified length? If set to true, user input and values passed to 
	// +link{formItem.setValue(),setValue()} will be trimmed to the specified length. Otherwise values
	// exceeding the specified length will raise an error on validation.
	// <P>
	// Note that having this value set to true limits user interactivity in some ways.
	// For example users would be unable to paste a longer string into the field for
	// editing without seeing it be truncated.
	// @visibility external
	//<
	enforceLength:true,

    // whether its possible for this type of FormItem to do autoCompletion
    canAutoComplete:true,

	//>	@attr	textItem._elementType			(string : "TEXT" : IRW)
	//			type of field (eg: "PASSWORD", "UPLOAD", etc)
	//<		
	_elementType:"TEXT",

    //> @attr   textItem._hasDataElement    (boolean : true : IRW)
    //      Text items have a data element.
    // @group formValues
    // @visibility   internal
    // @see     method:FormItem.hasDataElement
    // @see     method:FormItem.getDataElement
    //<
    _hasDataElement:true,
    
    // Set flag to indicate that our data element is used as the textBox for this item.
    // This flag means updateState will apply the result of this.getTextBoxStyle() to this item's
    // data element - appropriate for native text boxes, text areas and selects.
    _dataElementIsTextBox:true,

    //> @attr   textItem.emptyStringValue   (any : null : IRW)
    // How should an empty string entered by the user be stored?
    // This value is typically set to <code>null</code> or <code>""</code>.
    // <P>
    // Note that a call to +link{formItem.setValue(),setValue(null)} or +link{formItem.setValue(),setValue("")}
    // automatically updates this property to ensure that "empty" values are stored in a 
    // consistent format.
    // @group formValues
    // @visibility   external
    //<    
    
    emptyStringValue:null,
    
    // Override redrawOnShowFormIcon - we can handle dynamically updating the item's HTML to
    // show / hide text item icons
    redrawOnShowIcon:false,
    // setting clipValue to true ensures we resize the text box when showing/hiding icons
    clipValue:true,

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

            } :
            // iOS <= 4.3.2 doesn't give us normal touch start / touch end events when the user
            // touches a text item to focus in it. iOS 5.0+ does fire touch start / touch end events,
            // however, so only use this native handler for affected versions or else we'll process
            // two "mouse down" events.
            // Therefore use explicit handlers for this.
            
            isc.Browser.isIPhone && isc.Browser.iOSVersion < 5 ? function (e) {
                var EH = isc.EventHandler;
                EH.DOMevent = e;
                var	event = EH.getMouseEventProperties(e);
                return EH.handleMouseDown(e, event);
            } : null
        ),

        onmouseup : (
            isc.Browser.isIPhone && isc.Browser.iOSVersion < 5 ? function (e) {
                var EH = isc.EventHandler;
                EH.DOMevent = e;
                var	event = EH.getMouseEventProperties(e);
                return EH.handleMouseUp(e, event);
            } : null
        )
    },

    //> @attr textItem.browserSpellCheck (boolean : null : IRWA)
    // @include FormItem.browserSpellCheck
    // @visibility internal
    //<
    

    //> @attr textItem.browserAutoCapitalize
    // @include FormItem.browserAutoCapitalize
    // @visibility external
    //<

    //> @attr textItem.browserAutoCorrect
    // @include FormItem.browserAutoCorrect
    // @visibility external
    //<

    //> @attr textItem.browserInputType (String : null : IRA)
    // This property corresponds to the HTML5 "inputType" attribute applied to the &lt;input&gt;
    // element for this TextItem.
    // <p>
    // The only currently supported use of this attribute is hinting to touch-enabled mobile
    // devices that a particular keyboard layout should be used.  Even here, be careful; to
    // take a random example, using type "number" on Android up to at least 3.2 leads to a
    // keyboard with no "-" key, so negative numbers cannot be entered.
    // <p>
    // <b>Valid values:</b>
    // <table class="normal" cellpadding="2">
    //   <tbody>
    //   <tr>
    //     <td valign="top"><em>"text"</em></td>
    //     <td>Normal text keyboard</td>
    //   </tr>
    //   <tr>
    //     <td valign="top"><em>"digits"</em></td>
    //     <td>Makes the text field more suitable for entering a string of digits 0 - 9. On iOS,
    //         this causes the virtual keyboard to show a numeric keypad with only "0", "1",
    //         "2", ..., "9", and delete keys.</td>
    //   </tr>
    //   <tr>
    //     <td valign="top"><em>"email"</em></td>
    //     <td>Makes the text field more suitable for entering an e-mail address. On iOS, this
    //         causes the virtual keyboard to show special "@" and "." keys on the alphabetic
    //         keys screen.</td>
    //   </tr>
    //   <tr>
    //     <td valign="top"><em>"tel"</em></td>
    //     <td>Makes the text field more suitable for entering a telephone number. On iOS, this
    //         causes the virtual keyboard to show a numeric keypad with a "+*#" key for
    //         displaying punctuation keys.</td>
    //   </tr>
    //   <tr>
    //     <td valign="top"><em>"number"</em></td>
    //     <td>Makes the text field more suitable for entering a floating-point value. On iOS,
    //         this causes the virtual keyboard to start on the number and punctuation keys screen.
    //         <p>
    //         <b>NOTE:</b> This is not an appropriate text input type for credit card numbers,
    //         postal codes, ISBNs, and other formats that are not strictly parsable as floating-point
    //         numbers. This is because the browser is required to perform floating-point value
    //         sanitization to ensure that the value is a <a href="http://www.w3.org/TR/html5/infrastructure.html#valid-floating-point-number">valid floating-point number</a>.</td>
    //   </tr>
    //   <tr>
    //     <td valign="top"><em>"url"</em></td>
    //     <td>Makes the text field more suitable for entering a URL. On iOS, this causes the
    //         virtual keyboard to show a special ".com" key.</td>
    //   </tr>
    //   <tr>
    //     <td valign="top">Any&nbsp;vendor-<br>specific value</td>
    //     <td>If a browser supports another input type.</td>
    //   </tr>
    //   </tbody>
    // </table>
    // @visibility external
    //<
    

    //> @attr textItem.selectOnFocus (boolean : null : IRW)
    // @include FormItem.selectOnFocus
    // @visibility external
    //<

    //> @attr textItem.selectOnClick (boolean : null : IRW)
    // @include FormItem.selectOnClick
    // @visibility external
    //<
    
    //> @attr textItem.changeOnKeypress (Boolean : true : IRW)
    // @include FormItem.changeOnKeypress
    // @visibility external
    //<
    
    //> @method textItem.getSelectionRange()
    // @include FormItem.getSelectionRange()
    // @visibility external
    //<
    
    //> @method textItem.setSelectionRange()
    // @include FormItem.setSelectionRange()
    // @visibility external
    //<
    
    //> @method textItem.selectValue()
    // @include FormItem.selectValue()
    // @visibility external
    //<
    
    //> @method textItem.deselectValue()
    // @include FormItem.deselectValue()
    // @visibility external
    //<
    
    //> @attr textItem.readOnly  (boolean : null : IRWA)
    // Setter for the standard HTML readonly property of the input element.
    // If set to true, text will be non editable (though it can still be selected and copied etc)
    // @visibility internal
    //<

    //> @attr textItem.showHintInField (boolean : null : IRWA)
    // If showing hint for this form item, should it be shown within the field?
    // <P>CSS style for the hint is +link{textItem.textBoxStyle} with the suffix
    // "Hint" appended to it. If the item is disabled the suffix "DisabledHint" will be
    // used.
    // <P>
    // To change this attribute after being drawn, it is necessary to call +link{FormItem.redraw()}
    // or redraw the form.
    // @group appearance
    // @see FormItem.hint
    // @visibility external
    //<

    //> @attr textItem.printFullText (Boolean : false : IRW)
    // When generating a print-view of the component containing this TextItem, should
    // the form item expand to accommodate its value? If set to false the text box will not expand
    // to fit its content in the print view, instead showing exactly as it does in the
    // live form.
    // @visibility external
    // @group printing
    //<
    printFullText:false,

    //> @attr textItem.saveOnEnter (Boolean : true : IRW)
    // Text items will submit their containing form on enter keypress 
    // if +link{DynamicForm.saveOnEnter,saveOnEnter} is true. Setting this property to
    // <code>false</code> will disable this behavior.
    // @visibility external
    //<
    // default implementation of formItem.shouldSaveOnEnter() returns this
    saveOnEnter: true
});

isc.TextItem.addMethods({
    _getShowHintInField : function () {
        return !!(this.showHint && this.showHintInField);
    },

    _manageCharacterInput : function () {
        return (!!this.mask || !!this._keyPressRegExp || this.characterCasing == isc.TextItem.UPPER ||
                this.characterCasing == isc.TextItem.LOWER);
    },

    // _willHandleInput()
    // Can we use the "input" event in this browser / form item?
    // True for Moz and Safari, but not IE. See comments near FormItem._handleInput()
    _willHandleInput : function () {
        return !isc.Browser.isIE;
    },

    // If the user cuts or pastes into a text field, then, depending on the browser,
    // we will either get an input event or a cut or paste event.  In either case, we
    // need to update the mask with the text currently stored in the text box.
    _$fixMaskAfterCutPaste: "_fixMaskAfterCutPaste",
    _pendingFixMask: null,
    _nativeCutPaste : function () {
        
        if (this._pendingFixMask != null) {
            isc.Timer.clearTimeout(this._pendingFixMask);
        }
        this._pendingFixMask = isc.Timer.setTimeout({target: this, methodName: this._$fixMaskAfterCutPaste}, 0);
        return this.Super("_nativeCutPaste", arguments);
    },
    __handleInput : function () {
        // In IE10+, we only listen for ONINPUT to catch the case where the user clicks on the
        // -ms-clear pseudo-element: http://msdn.microsoft.com/en-us/library/windows/apps/hh465740.aspx
        if (isc.Browser.isIE && isc.Browser.version >= 10) {
            // If the data element value is empty and the _pendingFixMask timer is not set
            // (indicating the user just cut or pasted text), then clear the value.
            if (this._pendingFixMask == null && !this.getElementValue()) {
                this.updateValue();
            }

        } else {
            
            this._fixMaskAfterCutPaste();
            return this.Super("__handleInput", arguments);
        }
    },
    __handleSelect : function () {
        if (this._manageCharacterInput()) {
            this._lastSelectRange = this.getSelectionRange();
        }
        return this.Super("__handleSelect", arguments);
    },
    _fixMaskAfterCutPaste : function () {
        delete this._pendingFixMask;
        if (this._manageCharacterInput()) {
            var value = this.getValue();

            // Ensure that `value' is a string.
            
            if (!value) value = "";
            else value = String(value);

            var expectedElementValue = (this.mask ? this._maskValue(value) : value),
                currentElementValue = this.getElementValue();

            // The user might have just cut the entire value. If the current element value is
            // empty, then call clearValue() and return early.
            if (!currentElementValue) {
                this.clearValue();
                return;
            }

            

            var selection = this.getSelectionRange(),
                pasteLen = currentElementValue.length - expectedElementValue.length,
                pasteEnd;

            if (isc.Browser.iOSVersion < 7 && isc.Browser.isMobileWebkit && !isc.Browser.isChrome) {
                
                pasteEnd = selection[0] + pasteLen;

            } else if (expectedElementValue != currentElementValue &&
                       this._lastSelectRange && this._lastSelectRange[0] < this._lastSelectRange[1])
            {
                pasteLen += (this._lastSelectRange[1] - this._lastSelectRange[0]);
                
                if (isc.Browser.isSafari && !isc.Browser.isChrome && isc.Browser.minorVersion < 6.1 &&
                    this._lastSelectRange[1] == expectedElementValue.length)
                {
                    pasteEnd = selection[0] + pasteLen;
                } else {
                    pasteEnd = selection[0];
                }

            } else {
                pasteEnd = selection[0];
                
            }

            // If the user cut characters and we don't have a mask, then just return if
            // _willHandleInput() (because _fixMaskAfterCutPaste() was called from __handleInput(),
            // which will invoke super.__handleInput()); otherwise (on IE after a cut), call
            // super.__handleInput() explicitly to store the new value.
            if (pasteLen < 0 && !this.mask) {
                if (!this._willHandleInput()) this.invokeSuper(isc.TextItem, "__handleInput");
                return;
            }

            var pasteStart = pasteEnd - pasteLen;

            value = currentElementValue.substring(0, pasteStart);

            // Ensure that `value' is a prefix of `expectedElementValue'
            if (!expectedElementValue.startsWith(value)) {
                for (pasteStart = 0; pasteStart < value.length && pasteStart < expectedElementValue.length; ++pasteStart) {
                    if (value.charAt(pasteStart) != expectedElementValue.charAt(pasteStart)) {
                        break;
                    }
                }
                pasteLen = pasteEnd - pasteStart;
                value = currentElementValue.substring(0, pasteStart);
            }

            this.setElementValue(value);
            this._setSelection(pasteStart);

            var replayString = currentElementValue.substring(pasteStart);
            if (this.mask) {
                var next = pasteStart,
                    foundInvalid = false;
                for (var i = 0; i < replayString.length; ++i) {
                    var c = replayString.charAt(i),
                        characterValue = replayString.charCodeAt(i);
                    if (i == 0) this._setUpInsertCharacterValue(characterValue);
                    var newNext = this._insertCharacterValue(characterValue, true);
                    if (newNext === false) {
                        foundInvalid = true;
                    } else if (!foundInvalid) {
                        next = newNext;
                    }
                }
                this._setSelection(next);
            } else {
                
                var numRejected = 0;
                for (var i = 0; i < replayString.length; ++i) {
                    var c = replayString[i];

                    // Perform character case changes
                    var nc = c;
                    if (!this.mask) nc = this._mapCharacterCase(c, this.characterCasing);

                    // Check keyPress filter to determine if entered character is valid
                    if (this._keyPressRegExp && !this._keyPressRegExp.test(nc)) {
                        ++numRejected;
                        continue;
                    }

                    value += nc;
                }

                // Push new value to field
                if (this.changeOnKeypress) {
                    this.setElementValue(value);
                    this.updateValue();
                } else {
                    this.setValue(value);
                }

                // Set the caret position to pasteEnd less the number of characters that were
                // filtered out of the paste.
                this._setSelection(pasteEnd - numRejected);
            }
        }
    },

    // by putting 'nowrap' on the text box cell we avoid the value icon / text box appearing 
    // on different lines
    getTextBoxCellCSS : function () {
        return this._$nowrapCSS
    },

    //> @method textItem.setElementReadOnly()
    // Change the read-only state of the form element immediately.
    //<
    setElementReadOnly : function (readOnly) {
        // Text HTML element has readonly property
        this._setElementReadOnly(readOnly);
    },

    // NOTE: this is here for doc generation
    //>	@method textItem.keyPress		(A)
	//		@group	event handling
	//			event handler for keys pressed in this item
	//<

	//>	@method	textItem.getElementHTML()	(A)
	//			output the HTML for a text field element
	//		@group	drawing
	//		@param	value	(string)	Value of the element [Unused because it is more reliably set by setValue].
	//		@return	(HTML)	HTML output for this element
	//<
    _$elementStartTemplate:[
        ,                   // [0] possible value icon stuff
        "<INPUT TYPE=",         // [1]
        ,                       // [2] this._elementType,
        " NAME='",               // [3]
        ,                       // [4] this.getElementName(),
        "' ID='",                 // [5]
        ,                       // [6] this.getDataElementId(),
            // We want the EH system to handle events rather than writing native
            // handlers into the form item.
        "' handleNativeEvents=false" // [7]
    ],
    _$tabIndexEquals:" TABINDEX=",
    _$rightAngle:">",
            
    _$disabled:" DISABLED ",
    _$native:"native",
    _$autoCompleteOff:" AUTOCOMPLETE=OFF ",
    _$accessKeyEquals:" ACCESSKEY=",
    
    
    _writeOuterTable : function () {
        var writeOT = this.Super("_writeOuterTable", arguments);
        if (writeOT) return true;
        var iconHTML = this._getValueIconHTML(this.getValue());
        if (iconHTML != null && iconHTML != "") {
            return true;
        }
        return false;
    },
    
    getElementHTML : function (value, dataValue) {
        var valueIconHTML = this._getValueIconHTML(dataValue);
        if (this.showValueIconOnly) return valueIconHTML;
        
        var result;
        
        if (this._isPrinting() || this.renderAsStatic()) {
            if (this.printFullText) {
                result = isc.StringBuffer.concat(
                    "<SPAN ",this.getElementStyleHTML(), ">",
                    dataValue == null ? "&nbsp;" : dataValue.asHTML(), "</SPAN>"
                );
            } else {
                result = isc.StaticTextItem._instancePrototype.getElementHTML.apply(this, arguments);
            }
        } else {
            
            var template = this._$elementStartTemplate,
                form = this.form,
                formID = form.getID(),
                itemID = this.getItemID()
            ;

            // May be null
            template[0] = valueIconHTML;

            var inputType = this._elementType;
            if (this.inputDataType != null && this.browserInputType == null) {
                this.browserInputType = this.inputDataType;
            }
            if (this.browserInputType != null) {
                inputType = this.getBrowserInputType();
            }

            template[2] = inputType;
            template[4] = this.getElementName();
            template[6] = this.getDataElementId();
            
            // hang a flag on the element marking it as the data element for the
            // appropriate form item.
            template[8] = this._getItemElementAttributeHTML();
            
            // At this point we're appending to the end of the template Disable spellchecker in
            // Moz if appropriate so we don't get the red wavy line under email addresses etc.
             
            
            if (isc.Browser.isMoz || isc.Browser.isSafari) {
                if (this.getBrowserSpellCheck()) template[template.length] = " spellcheck=true";
                else template[template.length] = " spellcheck=false"
            }

            // iPhone / Safari specific native features
            if (isc.Browser.isSafari) {
                if (this.browserAutoCapitalize == false) {
                    template[template.length] = " autocapitalize=off";
                }
                if (this.browserAutoCorrect != null) {
                    template[template.length] = this.browserAutoCorrect ? " autocorrect='on'" : " autocorrect='off'";
                }
                if (this.browserInputType == "digits") {
                    template[template.length] = " pattern='\\d*'";
                }
            }

            // If we get an oninput event for this browser, write it out into our element's HTML
            
            
            if (this._willHandleInput() || (isc.Browser.isIE && isc.Browser.version >= 10)) {
                template[template.length] = " ONINPUT='" 
                template[template.length] = this.getID() 
                template[template.length] = "._handleInput()'"
            }

            template[template.length] = " ONSELECT='if (window.";
            template[template.length] = this.getID() 
            template[template.length] = " == null) return;";
            template[template.length] = this.getID();
            template[template.length] = "._handleSelect()'";

            
            if (this.renderAsDisabled() || (this._elementType == "FILE" && this.isReadOnly())) {
                template[template.length] = this._$disabled;
            }

            // Write out 'readOnly' setting if present
            if (this._elementIsReadOnly()) {
                template[template.length] = " READONLY=TRUE";
                if (isc.screenReader) template[template.length] = " aria-readonly=true";
            }
            
            if (this.isInactiveHTML() && value != null && value != isc.emptyString) {
                // run the value through makeXMLSafe - this'll escape single quotes
                // / avoid terminating the tag if a '>' char is present etc
                template[template.length] = " value='" + isc.makeXMLSafe(value) + "'";
            }
            
            // disable native autoComplete 
                  
                  
            if (this._getAutoCompleteSetting() != this._$native) {
                template[template.length] = this._$autoCompleteOff;
            }
            
            template[template.length] = this.getElementStyleHTML();
            
            
            var tabIndex = this._getElementTabIndex();
            if (tabIndex != null) {
                var end = template.length;
                template[end] = this._$tabIndexEquals;  
                isc._fillNumber(template, tabIndex, end+1, 5);
            }
            
            // Note: if we're showing a title for the element, we don't need to set
            // up an accessKey here, since the label tag takes care of that
            if (this.showTitle == false && this.accessKey != null) {
                template[template.length] = this._$accessKeyEquals;
                template[template.length] = this.accessKey;
            }
            
            template[template.length] = this._$rightAngle;
    
            result = template.join(isc.emptyString);
        
            // Trim the entries off the end of the template so we can reuse it.
            template.length = 8;
        }
        //this.logWarn("generated textItem HTML:"+ result);
        
        return result;
	},  
	
	_elementIsReadOnly:function () {
	    return this.isInactiveHTML() || this.isReadOnly();
    },      

    
    _sizeTextBoxAsContentBox : function () {
        if (this._isPrinting()) {
            return this.Super("_sizeTextBoxAsContentBox", arguments);
        }
        return isc.Browser.isStrict;
    },
    
    // override _nativeElementBlur() to fire blur and change handlers in response to a native 
    // blur
    //
    // Natively onblur is fired when focus is taken from the text item, but onchange will
    // only fire if the value on leaving the text item is different from what it was when
    // the user put focus into the text item.
    //
    // Since we do internal values handling, having the same element value when focus is 
    // taken from a form item as when focus first went to a form item is not a guarantee
    // that our stored value for the form item has not changed, and vice versa - 
    // typically we are saving values in response to key events due to 'changeOnKeypress'.
    // 
    // Therefore instead of relying on the native change handler, on blur we will always fire
    // our change handler if changeOnBlur is true, and otherwise compare our stored value to
    // the current element value, and fire the change handler if they do not match.
    
    
    _nativeElementBlur : function (element, itemID) {
    
        // On blur always call elementChanged. This falls through to updateValue() which
        // saves out the new value and fires change handlers [if the value has been modified].
        // If the value is unchanged, elementChanged() essentially no ops so we can always call
        // it here.
        
       if (this.form && !this.form._setValuesPending) {
           this.form.elementChanged(this);
       }

        var returnVal = this.Super("_nativeElementBlur", arguments);
        
        if (this.formatOnFocusChange || this.mask != null || this._elementValueAtFocus == null ||
            this._elementValueAtFocus != this.getEnteredValue())
        {
            this.refreshDisplayValue();
        }

        // If showing hint within data field, see if it should be shown now.
        if (this._getShowHintInField()) {
            var undef;
            var value = this.getElementValue();
            if (value === undef || value == null || isc.is.emptyString(value)) {
                this._showInFieldHint();
            }
        }

        if (this._delayedSelect != null) {
            isc.Timer.clear(this._delayedSelect);
            this._delayedSelect = null;
        }
        return returnVal;
    },

    // Helper to refresh display value:
    // Call mapValueToDisplay() so we format the stored value to the appropriate display value.
    // Required if we have a mask
    // Also required if a developer has custom formatters/parsers that are not 1:1
    // [EG: A forgiving data parser allowing variants on a display format].
    // In this second case - compare the current element value to the element value when
    // the user put focus into the item and skip the call if they're unchanged.
    // This means we'll catch cases where the user has modified the display value (even if it
    // ultimately mapped back to the same data value) but shouldn't run the formatter when
    // the user simply tabbed through the field.
    
    refreshDisplayValue : function () {
        var value = this.mapValueToDisplay(this.getValue());
        if (!this.hasFocus && this._getShowHintInField() && (value == null || value == "")) {
            this._showInFieldHint();
        } else {
            this.setElementValue(value);
        }
    },

	//>	@method	textItem.getElementStyleHTML()	(I)
    //      	Get the HTML string used to set the visual characteristics for a text item.
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
    _$styleTemplate:[
        " CLASS='",          // [0]
        ,                   // [1] this.getTextBoxStyle(),
        "' STYLE='",         // [2]
        ,                   // [3] null or 'width:'
        ,,,,                // [4-7] null or width
        ,                   // [8] null or 'px;'

             
        ,                   // [9] null or 'height:'
        ,,,,                // [10-13] null or height
        ,                   // [14] null or 'px;'

            // text align property, known to be supported in IE6 and Moz/Firefox on
            // Windows, not supported on Safari 1.2
        ,                   // [15] null or 'text-align'
        ,                   // [16] null or this.textAlign
        ,                   // [17] null or ";"
        
            // In Mozilla we must use the '-moz-user-focus' css property to govern
            // whether this element can recieve focus or not.
            // (slots 18 and 19)
        (isc.Browser.isMoz ? "-moz-user-focus:" 
          
            :  null),    // [18]
        ,                   // [19] Moz: 'normal' or 'ignore' - otherwise null
        "' "                // [20]
    ],
    _$normal:"normal;", _$ignore:"ignore;",
    
    _$negativeMargins:"margin-top:-1px;margin-bottom:-1px;",
	getElementStyleHTML : function () {

	    // in 'printFullText' / printing mode we write out a span rather than
	    // an input.
	    // Most of the css will be the same but we can skip a few steps
	    var isStaticElement = this._isPrinting() && this.printFullText;

        var template = this._$styleTemplate,

        
            width = this.getTextBoxWidth(),
            height = this.getTextBoxHeight(),
            style = this.getTextBoxStyle();

		// Render in-field hint and missing value message in "hint" style
        if ((this._getShowHintInField() && this._value == null && this.getHint() != null)
            || 
            (this._fetchMissingValueInProgress() && this.loadingDisplayValue != null)) 
        {
        	style = this._getInFieldHintStyle();
        }

        template[1] = style;

        
        if (isc.isA.Number(width)) {
            template[3] = this._$widthColon;
            isc._fillNumber(template, width, 4, 4);            
            template[8] = this._$pxSemi;
        } else {
            template[3] = template[4] = template[5] = template[6] = 
                template[7] = template[8] = null;
        }

        if (isc.isA.Number(height)) {
            template[9] = this._$heightColon;
            isc._fillNumber(template, height, 10, 4);
            template[14] = this._$pxSemi;
        } else {
            template[9] = template[10] = template[11] = template[12] = 
                template[13] = template[14] = null;            
        }

        if (this.textAlign) {
            template[15] = this._$textAlignColon;
            template[16] = this.textAlign;
            template[17] = this._$semi;
        } else {
            template[15] = template[16] = template[17] = null;
        }
        if (isc.TextItem._needNegativeMargins) {
            template[18] = isStaticElement ? null : this._$negativeMargins;
        }
        if (isc.Browser.isMoz && !isStaticElement) {
            template[19] = (this._getElementTabIndex() > 0 ? this._$normal
                                                           : this._$ignore);
        }
        return template.join(isc.emptyString);
    },

    _getMeasureCanvas : function () {
        return isc.TextItem._measureCanvas ||
               (isc.TextItem._measureCanvas = isc.Canvas.create({
                    _generated: true,
                    top: -1000,
                    overflow: "visible",
                    autoDraw: true,
                    height: 1,
                    width: 1,

                    markForRedraw : function () {}
                }));
    },

    
    _$INPUT: "INPUT",
    _$TEXTAREA: "TEXTAREA",
    _getTextBoxScrollWidth : function (textBoxHandle, b, c, d) {
        var tagName = textBoxHandle.tagName;
        if ((tagName === this._$INPUT || tagName === this._$TEXTAREA) &&
            (isc.Browser.isIE10 || isc.Browser.isMoz))
        {
            var textBoxStyle = this.getTextBoxStyle(),
                elementValue = this.getElementValue();
            if (elementValue == null) elementValue = "";
            else elementValue = String(elementValue);
            if (this._cachedValueScrollWidthInfo == null ||
                this._cachedValueScrollWidthInfo.textBoxStyle != textBoxStyle ||
                this._cachedValueScrollWidthInfo.elementValue != elementValue)
            {
                var measureCanvas = this._getMeasureCanvas();
                if (measureCanvas.styleName != textBoxStyle) measureCanvas.setStyleName(textBoxStyle);
                measureCanvas.setContents("<span style='white-space:nowrap'>" + isc.makeXMLSafe(elementValue) + "</span>");
                measureCanvas.redraw("value scrollWidth measurement: " + elementValue);
                var width = measureCanvas.getScrollWidth(true);
                this._cachedValueScrollWidthInfo = {
                    width: width,
                    textBoxStyle: textBoxStyle,
                    elementValue: elementValue
                };
            }
            return this._cachedValueScrollWidthInfo.width;
        } else {
            return this.invokeSuper(isc.TextItem, "_getTextBoxScrollWidth", textBoxHandle, b, c, d);
        }
    },

    //> @method textItem.getEnteredValue()
    // Returns the raw text value that currently appears in the text field, which can differ from 
    // +link{formItem.getValue()} in various cases - for example:
    // <ul>
    // <li>for items that constrain the value range, such as a +link{DateItem} with
    // +link{DateItem.enforceDate,enforceDate}:true, or a +link{ComboBoxItem} with
    // +link{ComboBoxItem.addUnknownValues,addUnknownValues}:false</li>
    // <li>for items with a defined valueMap or edit value formatter and parser functions
    // which converts display value to data value</li>
    // <li>while the item has focus if +link{TextItem.changeOnKeypress, changeOnKeypress} is false
    // </li></ul>
    // @return (string) current entered value
    // @visibility external
    //<
    getEnteredValue : function () {
        return this.getElementValue();
    },


    //>@method textItem.mapValueToDisplay()  (A)
    // Map from the internal value for this item to the display value.
    // @param   internalValue   (string)   Internal value for this item.
    // @return  (string)   Displayed value corresponding to internal value.
    // @group   drawing
    //<
    mapValueToDisplay : function (internalValue) {
        var value;
        if (this.mask && this.hasFocus) {
            value = this._getMaskBuffer();
        } else {
            
            
            value = isc.FormItem._instancePrototype.mapValueToDisplay.call(this, internalValue);

            // always display the empty string for null values, rather than "null" or "undefined"
            if (value == null) return isc.emptyString;
            if (this.mask) {
                value = this._maskValue(value);
            }
        }
        
        return value;
    },

    // Don't apply arbitrary formatters specified via SimpleType definitions to this item's
    // display value - we have no way to parse it back to a real data value
    applyStaticTypeFormat:false,

    //>@method textItem.mapDisplayToValue()  (A)
    // @group	drawing
    // Map from a the display value for this item to the internal value.
    // @param   displayValue   (string)   Value displayed to the user.
    // @return  (string)   Internal value corresponding to that display value.
    //<
    mapDisplayToValue : function (displayValue) {
        var value;
        if (this.mask) {
            value = this._unmaskValue(displayValue);
        } else {
            // See comments in FormItem.js and ComboBoxItem.js about mapEmptyDisplayToValue
            if (this.mapEmptyDisplayValue || (displayValue != this.emptyDisplayValue)) {
                value = this._unmapKey(displayValue);
            }
        }
        value = this._parseDisplayValue(value);
        // if the value to be saved is an empty string, map it to 'null' if necessary
        if (isc.is.emptyString(value)) value = this.emptyStringValue;
        return value;
    },
    
    // override 'saveValue' so new value can be mapped into mask if used.
    saveValue : function (value, isDefault) {

        // Save the new value into our mask buffer
        if (this.mask) this._maskValue (value);

        this.Super("saveValue", arguments);
    },

    // override 'setValue'.
    // If passed null or the empty string, we store this as the 'empty string value' - this will
    // then be returned whenever the user clears out the text item element.
    setValue : function (value,b,c,d) {
        
        

        // Make sure in-field hint is hidden
        this._hideInFieldHint();

        var undef;
        if (value !== undef && (value == null || isc.is.emptyString(value)))
            this.emptyStringValue = value;

        // Translate incoming value based on characterCasing if needed
        if (value !== undef && value != null && this.characterCasing != isc.TextItem.DEFAULT) {
            if (this.characterCasing == isc.TextItem.UPPER) {
            	if(isc.isA.Array(value))
                    this.arrayToUpperCase(value);
            	else
	                value = value.toUpperCase();
            } else if (this.characterCasing == isc.TextItem.LOWER) {
            	if(isc.isA.Array(value))
                    this.arrayToLowerCase(value);
            	else
	                value = value.toLowerCase();
            }
        }

        // Let parent take care of saving the value
        value = this.invokeSuper(isc.TextItem, "setValue", value,b,c,d);

        // See if the in-field hint needs to be shown
        if (!this.hasFocus && this._getShowHintInField() && this.getHint()) {
            var elementValue = this.getElementValue();
            if (elementValue == null || isc.isAn.emptyString(elementValue)) {
                this._showInFieldHint();
            }

        // If there is a mask and the new value is empty, then update the editor caret position
        // if focused.
        } else if (this.hasFocus && this.mask && (value == null || isc.isAn.emptyString(value))) {
            this._setSelection(this._firstNonMaskPos == null ? 0 : this._firstNonMaskPos);
        }

        return value;
    },
    
    arrayToUpperCase : function(value) {
        for (var i = 0; i < value.length; i++) {
            value[i] = value[i].toUpperCase();
        }
    },
    arrayToLowerCase : function(value) {
        for (var i = 0; i < value.length; i++) {
            value[i] = value[i].toLowerCase();
        }
    },

    // Override getCriteriaFieldName - if we have a displayField, return it rather than the
    // item name
    getCriteriaFieldName : function () {
        if (this.criteriaField) return this.criteriaField;
        if (this.displayField) return this.displayField;
        return this.Super("getCriteriaFieldName", arguments);
    },


    // When focus is received, the hint should be hidden if TextItem.showHintInField is true.
    _nativeElementFocus : function (element, itemID) {
        // if this focus came from a redraw, don't select- we want to retain the pre-redraw selection
        var refocusAfterRedraw = this._refocussingAfterRedraw;
        
        var returnVal = this.Super("_nativeElementFocus", arguments);

        // Hide in-field hint if being shown
        this._hideInFieldHint();
        
        // In IE there's a native bug whereby if you change an element value on focus,
        // you see the caret at the end of the new string, then jump to the beginning of
        // the new string.
        // Work around this by forcing the caret back to the end so the user doesn't get
        // surprised by this result
        
        var forceCaretToEnd;
        
        // If the special flag is set to refresh the display value on focus / blur, refresh the
        // display value
        // (Allows the user to specify a different value when the item has focus)
        if (this.formatOnFocusChange) {
            var elementValue = this.getElementValue();
            this.refreshDisplayValue()
            if (elementValue != this.getElementValue() && isc.Browser.isIE) {
                forceCaretToEnd = true;
            }
        }

        if (this.mask) {
            // Force buffer back into control so unfilled mask spaces
            // will be shown with the maskPromptChar
            this._saveMaskBuffer(false);

            // Determine caret position. By default the caret is placed on the next
            // unfilled mask position or at the end of the field. If selectOnFocus
            // is true, the entire field is selected.
            var begin = 0;
            var end = this._length;
            var selectOnFocus = !refocusAfterRedraw && this._shouldSelectOnFocus();
            if (!selectOnFocus) {
                begin = this._getEndPosition();
                var value = this.getValue();
                if (value != null && begin == 0 && value.length > 0) {
                    // there are no unfilled mask-positions, a value is present and the user 
                    // clicked somewhere mid-value - stick the caret there with no selection
                    begin = this.getSelectionRange()[0];
                }
                end = begin;
            }
            if (!this._delayedSelect) {
                this._delayedSelect = this.delayCall("_delayed_setSelection", [begin, end], 50);
            }

        // There may be custom parser / formatter logic applied to any text item and this
        // may not be a 1:1 mapping [EG a forgiving date format parser allowing variants on
        // a display format].
        // In this case the developer would typically specify changeOnKeypress:false [so as
        // not to break on partial values] and on blur expect values to be updated and 
        // if necessary reformatted to the appropriate display value.
        // We don't want to run potentially expensive formatters if the user hasn't changed
        // the display value, so record the element value on focus and don't reformat
        // if its unchanged on blur implying the user just tabbed through the field or
        // edited, then reverted their edits.
        } else {
        
            var selectOnFocus = !refocusAfterRedraw && this._shouldSelectOnFocus();
            var value = this.getEnteredValue();
            if (selectOnFocus) {
                if (value != null && !this._delayedSelect) {
                    this._delayedSelect = this.delayCall("_delayed_selectValue");
                }
            } else if (forceCaretToEnd) {
                var elementValue = this.getElementValue();
                if (elementValue.length > 0) {
                    this.setSelectionRange(elementValue.length, elementValue.length);
                }
            }
            this._elementValueAtFocus = value;
        }

        return returnVal;
    },
    _delayed_setSelection : function (begin, end) {
        this._delayedSelect = null;
        // manipulating the selection changes focus, so don't do it if focus has moved on
        if (!this._hasNativeFocus()) return;  
        this._setSelection(begin, end);
    },
    _delayed_selectValue : function () {
        this._delayedSelect = null;
        // manipulating the selection changes focus, so don't do it if focus has moved on
        if (!this._hasNativeFocus()) return;  
        this.selectValue();
    },

    // Case conversion and keyPressFilter handling

    //> @attr   textItem.characterCasing   (CharacterCasing : isc.TextItem.DEFAULT : IRWA)
    // Should entered characters be converted to upper or lowercase?
    // Also applies to values applied with +link{formItem.setValue}.
    // <P>
    // Note: character casing cannot be used at the same time as a +link{textItem.mask}.
    // @example formFilters
    // @visibility  external
    //<    
    characterCasing: isc.TextItem.DEFAULT,

    //> @attr   textItem.keyPressFilter   (string : null : IRWA)
    // Sets a keypress filter regular expression to limit valid characters
    // that can be entered by the user. If defined, keys that match the
    // regular expression are allowed; all others are suppressed. The
    // filter is applied after character casing, if defined.
    // <P>
    // Note: keypress filtering cannot be used at the same time as a +link{textItem.mask}.
    // @see textItem.characterCasing
    // @setter setKeyPressFilter
    // @example formFilters
    // @visibility  external
    //<    

    //>@method textItem.setKeyPressFilter()
    // Set the +link{keyPressFilter,keyPressFilter} for this item
    // @param filter (string) new keyPress filter for the item
    // @visibility external
    //<
    setKeyPressFilter : function (filter) {
        if (this.mask) {
            this.logWarn("setKeyPressFilter() ignored because mask is enabled");
            return;
        }
        this.keyPressFilter = filter;
        this._keyPressRegExp = null;
        if (this.keyPressFilter) {
            this._keyPressRegExp = new RegExp (this.keyPressFilter);
        }
    },
    
    init : function() {
        this.Super("init", arguments);

        // Setup mask or keyPress filter
        
        if (this.mask) {
            if ((isc.ComboBoxItem && isc.isA.ComboBoxItem(this)) ||
                (isc.SpinnerItem && isc.isA.SpinnerItem(this)))
            {
                this.logWarn("item.mask is unsupported for this FormItem type. " +
                    "This item has mask specified as '" + this.mask + "' - ignoring.");
                this.mask = null;
            } else {
                this._parseMask ();
                if (this.keyPressFilter) {
                    this.logWarn("init: keyPressFilter ignored because mask is enabled");
                }
            }
        } else if (this.keyPressFilter) {
            this._keyPressRegExp = new RegExp (this.keyPressFilter);
        }
    },

    // Disallow bubbling of edit / navigation keys
    stopNavKeyPressBubbling:true,
    stopCharacterKeyPressBubbling:true,


    // Override handleKeyPress to implement character casing, keypress filter, and
    // masking.
    handleKeyPress : function (event, eventInfo) {
        // default implementaiton will pick up "STOP_BUBBLING" for character keys
        // and Arrow Keys / home/end if necessary.
        var returnVal = this.Super("handleKeyPress", arguments);
        if (returnVal == false) {
            return false;
        }
        // If we're not explicitly returning false, we'll return this returnVal
        // This allows superclass logic to return stop-bubbling and thus prevent
        // things like scrolling of the form/parents on arrow keypresses which have
        // meaning to this item.
        
        // If field is read-only, nothing more to do
        if (this.isReadOnly()) return returnVal;

        var keyName = event.keyName;

        // Let standard key handling process this keyPress if
        // - Ctrl or Alt or Meta key is also pressed
        // - not performing case conversion, key press filtering or masked entry
        
        if ((isc.EventHandler.ctrlKeyDown() || isc.EventHandler.altKeyDown() || isc.EH.metaKeyDown()) &&
	    (eventInfo.characterValue === null || eventInfo.characterValue < 128)) {
            if (this.mask &&
                (keyName.startsWith("Arrow_") || keyName == "Home" || keyName == "End") &&
                !isc.EH.shiftKeyDown())
            {
                delete this._lastSelectRange;
            }

            return returnVal;
        }
        if ((!this.characterCasing || this.characterCasing == isc.TextItem.DEFAULT) &&
            !this._keyPressRegExp &&
            !this.mask)
        {
            return returnVal;
        }

        var characterValue = event.characterValue
        ;

        // Perform in-field navigation and deletion
        if (this.mask) {
            var selection = this._getSelection();
            var isSafari = isc.Browser.isSafari;

            var pos = selection.begin;

            // Handle backspace and delete keys
            if (keyName == "Backspace" || keyName == "Delete") {
                // If there is a selection, these keys the result is identical
                if ((selection.begin - selection.end) != 0 || 
                    (isSafari && this._lastSelection))
                {
                    if (isc.Browser.isSafari && this._lastSelection) {
                        selection = this._lastSelection;
                        this._lastSelection = null;
                    }
                    if (this.maskOverwriteMode) {
                        this._clearMaskBuffer(selection.begin, selection.end);
                    } else {
                        //var len = selection.end - selection.begin + (isSafari ? 1 : 0);
                        var len = selection.end - selection.begin;
                        this._shiftMaskBufferLeft(selection.begin, len);
                    }
                    this._saveMaskBuffer(true);
                    this._positionCaret(selection.begin, 0);
                } else {
                    // No selection
                    if (keyName == "Backspace") {
                        
                        var shiftPos = pos - 1;
                        if (shiftPos >= 0) {
                            if (this.maskOverwriteMode) {
                                while (!this._maskFilters[shiftPos] && shiftPos >= 0) shiftPos--;
                                this._maskBuffer[shiftPos] = this.maskPromptChar;
                            } else {
                                this._shiftMaskBufferLeft(shiftPos);
                            }
                            this._saveMaskBuffer(true);
                            this._positionCaret(shiftPos, -1);
                        }
                    } else {
                        if (this.maskOverwriteMode) {
                            // Don't clear a non-entry position
                            if (pos < this._length && pos == this._getNextEntryPosition (pos - 1)) {
                                this._maskBuffer[pos] = this.maskPromptChar;
                            }
                        } else {
                            this._shiftMaskBufferLeft (pos);
                        }
                        this._saveMaskBuffer(true);
                        this._positionCaret(pos, 0);
                    }
                }
                return false;

            } else if ((keyName.startsWith("Arrow_") || keyName == "Home" || keyName == "End") &&
                       !isc.EH.shiftKeyDown())
            {
                delete this._lastSelectRange;
            }

            this._setUpInsertCharacterValue(characterValue);

            // Handle ESC key
            if (keyName == "Escape") {
                this._clearMaskBuffer(0, this._length)
                this._saveMaskBuffer(true);
                this._setSelection(this._firstNonMaskPos == null ? 0 : this._firstNonMaskPos);
                return false;
            }
        }

        if (this._insertCharacterValue(characterValue) == false) {
            return false;
        } else {
            if (returnVal != false) delete this._lastSelectRange;
            return returnVal;
        }
    },

    _insertCharacterValue : function (characterValue, replayingCharacters) {
        var selection = this._getSelection(),
            pos = selection.begin;

        // Completely unhandled characters can be filtered
        if ((this.mask && !this._isTypableCharacter (characterValue)) ||
            (!this.mask && ((!this._keyPressRegExp && !this._isAlphaCharacter (characterValue)) ||
                            (this._keyPressRegExp && !this._isTypableCharacter (characterValue)))))
        {
            return true;
        }

        var c = String.fromCharCode(characterValue);

        if (this.mask) {
            // Get next typable position
            var p = this._getNextEntryPosition (pos - 1);
            if (p < this._length) {
                var filter = this._maskFilters[p];
                if (filter != null) {
                    if (c === this.maskPromptChar) {
                        if (!this.maskOverwriteMode) this._shiftMaskBufferRight(p);
                        this._maskBuffer[p] = c;
                        this._saveMaskBuffer(false);
                    } else {
                        // Perform character case changes
                        if (c !== this.maskPromptChar && filter.casing != null) {
                            // The German eszett 'ß' is a special case that maps to 'SS' when
                            // uppercased. Use charAt(0) to get a single character.
                            // See: http://unicode.org/Public/UCD/latest/ucd/SpecialCasing.txt
                            c = this._mapCharacterCase(c, filter.casing).charAt(0);
                        }

                        // Validate against the mask filter
                        
                        if (filter.filter.test(c)) {
                            if (!this.maskOverwriteMode) this._shiftMaskBufferRight(p);
                            this._maskBuffer[p] = c;
                            var next = p;
                            if (this._saveMaskBuffer(true)) {
                                next = this._getNextEntryPosition(p);
                            }
                            this._setSelection(next);
                            if (replayingCharacters) return next;
                        }
                    }
                }
            }
            return false;
        }

        // Perform character case changes
        var nc = c;
        if (!this.mask) nc = this._mapCharacterCase(c, this.characterCasing);

        // If no conversion was performed and a key press filter is not registered,
        // revert to standard keyPress handling
        if (c == nc && !this._keyPressRegExp) return true;

        // Check keyPress filter to determine if entered character is valid
        if (this._keyPressRegExp) {
            if (this._isTypableCharacter(characterValue) && !this._keyPressRegExp.test(nc)) {
                // Keypress is not valid. Suppress it by telling keyPress
                // handler that we handled the character but do nothing with it.
                return false;
            }
        }

        // If we get this far, the character entered is valid.
        // However, if case conversion was not performed we are done.
        if (c == nc) return true;

        // Case-converted character needs to be added to the current value.
        // Using the current selection (or insertion point) write the new character.
        var value = this.getValue() || "";
        selection = this.getSelectionRange();

        if ((selection[0] - selection[1]) != 0) {
            value = value.substring(0, selection[0]) + nc + value.substring(selection[1] + 1);
        } else {
            value = value.substring(0, selection[0]) + nc + value.substring(selection[1]);
        }
   
        // Push new value to field and update caret position
        if (this.changeOnKeypress) {
            this.setElementValue(value);
            this.updateValue();
        } else {
            this.setValue(value);
        }
        this.setSelectionRange(selection[0] + 1, selection[0] + 1);


        // Don't process this keyPress event further
        return false;
    },

    _setUpInsertCharacterValue : function (characterValue) {
        if (this.mask) {
            var selection = this._getSelection();
            var isSafari = isc.Browser.isSafari;

            // If there is a selection, see if it should be cleared first
            if (this._isTypableCharacter (characterValue) &&
                ((selection.begin - selection.end) != 0 || (isSafari && this._lastSelection)))
            {
                if (isc.Browser.isSafari && this._lastSelection) {
                    selection = this._lastSelection;
                    this._lastSelection = null;
                }
                if (this.maskOverwriteMode) {
                    this._clearMaskBuffer(selection.begin, selection.end);
                } else {
                    var len = selection.end - selection.begin;
                    this._shiftMaskBufferLeft(selection.begin, len);
                }
            }

            // For Safari, save selection
            if (isSafari && (selection.begin - selection.end) != 0 &&
                !this._isTypableCharacter(characterValue))
            { 
                this._lastSelection = selection;
            } else {
                this._lastSelection = null;
            }
        }
    },

    // Helper methods to determine valid typed characters
    _isTypableCharacter : function (characterValue) {
        return ((characterValue >= 32 && characterValue <= 126) || characterValue > 127);
    },
    _isAlphaCharacter : function (characterValue) {
        return (characterValue >= 65 && characterValue <= 90) ||
            (characterValue >= 97 && characterValue <= 122);
     },
    _mapCharacterCase : function (c, casing) {
        if (casing == isc.TextItem.UPPER) {
            c = c.toUpperCase();
        } else if (casing == isc.TextItem.LOWER) {
            c = c.toLowerCase();
        }
        return c; 
    },
    
    //> @attr textItem.formatOnBlur (Boolean : false : IRW)
    // With <code>formatOnBlur</code> enabled, this textItem will format its value
    // according to any specified static +link{formItem.formatValue(),static formatter}
    // as long as the item does not have focus. Once the user puts focus into the item
    // the formatter will be removed. This provides a simply way for developers to
    // show a nicely formatted display value in a freeform text field, without the need
    // for an explicit +link{formItem.formatEditorValue()} 
    // and +link{formItem.parseEditorValue()} pair.
    // @visibility external
    //<
    // Implemented at the FormItem level.
    
    //> @attr textItem.formatOnFocusChange (Boolean : false : IRW)
    // Should +link{formItem.formatEditorValue} re-run whenever this item recieves or loses focus?
    // Setting this property allows developers to conditionally format the display value based on
    // item.hasFocus, typically to display a longer, more informative string while the item does
    // not have focus, and simplifying it down to an easier-to-edit string when the user puts
    // focus into the item.
    // @visibility external
    //<
    formatOnFocusChange:false,

    //> @attr   textItem.mask   (string : null : IRWA)
    // Input mask used to filter text entry.
    // <P>
    // Sample masks:
    // <UL>
    // <LI>Phone number: (###) ###-####</LI>
    // <LI>Social Security number: ###-##-####
    // <LI>First name: &gt;?&lt;??????????</LI>
    // <LI>Date: ##/##/####</LI>
    // <LI>State: &gt;LL</LI>
    // </UL>
    // Overview of available mask characters
    // <P>
    // <table class="normal">
    // <tr><th>Character</th><th>Description</th></tr>
    // <tr><td>0</td><td>Digit (0 through 9) or plus [+] or minus [-] signs</td></tr>
    // <tr><td>9</td><td>Digit or space</td></tr>
    // <tr><td>#</td><td>Digit</td></tr>
    // <tr><td>L</td><td>Letter (A through Z)</td></tr>
    // <tr><td>?</td><td>Letter (A through Z) or space</td></tr>
    // <tr><td>A</td><td>Letter or digit</td></tr>
    // <tr><td>a</td><td>Letter or digit</td></tr>
    // <tr><td>C</td><td>Any character or space</td></tr>
    // <tr><td>&nbsp;</td></tr>
    // <tr><td>&lt;</td><td>Causes all characters that follow to be converted to lowercase</td></tr>
    // <tr><td>&gt;</td><td>Causes all characters that follow to be converted to uppercase</td></tr>
    // </table>
    // <P>
    // Any character not matching one of the above mask characters or that is escaped
    // with a backslash (\) is considered to be a literal.
    // <P>
    // Custom mask characters can be defined by standard regular expression character set
    // or range. For example, a hexadecimal color code mask could be:
    // <UL>
    // <LI>Color: \#>[0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F]</LI>
    // </UL>
    // <P>
    // Note: input mask cannot be used at the same time as a +link{textItem.keyPressFilter}.
    // Also note that this property is not supported for
    // +link{ComboBoxItem} or +link{SpinnerItem}.
    // @setter setMask()
    // @see textItem.keyPressFilter
    // @example maskedTextItem
    // @visibility  external
    //<

    //> @attr   textItem.maskOverwriteMode   (boolean : null : IRWA)
    // During entry into masked field, should keystrokes overwrite current position value?
    // By default new keystrokes are inserted into the field.
    // @visibility  external
    //<    

    //> @attr   textItem.maskSaveLiterals   (boolean : null : IRWA)
    // Should entered mask value be saved with embedded literals?
    // @visibility  external
    //<    

    //> @attr   textItem.maskPadChar   (string : " " : IRWA)
    // Character that is used to fill required empty mask positions
    // to display text while control has no focus.
    // @visibility  external
    //<    
    maskPadChar: " ",

    //> @attr   textItem.maskPromptChar   (string : "_" : IRWA)
    // Character that is used to fill required empty mask positions
    // to display text while control has focus.
    // @visibility  external
    //<    
    maskPromptChar: "_",

    //> @method textItem.setMask ()
    // Set the mask for this item.  Pass null to clear an existing mask.
    // <P>
    // Note that the current value of the field is cleared when changing the mask.
    // @param mask (string) mask to apply to text item
    // @see textItem.mask
    // @visibility external
    //<
    setMask : function (mask) {
        if (isc.isA.ComboBoxItem(this) || isc.isA.SpinnerItem(this)) {
            return;    
        }
        
        if (!this.mask != !mask && this._delayedSelect != null) {
            isc.Timer.clear(this._delayedSelect);
            this._delayedSelect = null;
        }
        // Setup mask
        this.mask = mask;
        this._parseMask ();
        if (this.keyPressFilter) {
            this._keyPressRegExp = null;
            this.logWarn("setMask: keyPressFilter ignored because mask is enabled");
        }

        // Clear the field value
        this.setValue ("");
    },

    _parseMask : function () {
        var mask = this.mask;
        if (!mask) return;

        // Create an array of mask position checks for keyPress filtering.
        // Each entry will be an object holding the regular expression of
        // valid characters, whether the character should be converted to
        // upper of lower case, and if the character is required (for
        // validation).
        this._maskFilters = [];
        this._firstNonMaskPos = null;

        // This buffer holds the prompt characters and fixed characters from
        // the mask along with the characters entered by the user. It is
        // updated and then rewritten to the field when ready.
        this._maskBuffer = [];

        this._length = 0;

        // Current casing state
        var casing = null;     // no casing change
        // Current escape sequence state
        var escaping = false;
        // Are we processing a custom regex?
        var inRegex = false;
        // Current custom regex value
        var customRegex = "";

        // Build the mask filters and buffer
        for (var i = 0, numMaskChars = mask.length; i < numMaskChars; ++i) {
            
            var c = mask.charAt(i);

            if (!inRegex) {
                if (!escaping && c === "\\") {
                    escaping = true;
                } else if (escaping) {
                    this._addLiteralToMask(c, casing);
                    escaping = false;
                } else if (c === "<") {
                    // lowercase
                    casing = (casing == isc.TextItem.LOWER ? null : isc.TextItem.LOWER);
                } else if (c === ">") {
                    // uppercase
                    casing = (casing == isc.TextItem.UPPER ? null : isc.TextItem.UPPER);
                } else if (c === "[") {
                    // Start of custom regex
                    inRegex = true;
                    customRegex += c;
                } else {
                    this._addUnknownToMask(c, casing);
                }

            } else {
                
                if (c === "]") {
                    // End of custom regex
                    inRegex = false;
                    customRegex += c;
                    this._addCustomRegexToMask(customRegex, casing);
                    customRegex = "";

                } else {
                    // Building custom regex
                    customRegex += c;
                }
            }
        }

        // If we've reached the end of the mask, but are still escaping or within a regex...
        if (!inRegex) {
            if (escaping) {
                this.logWarn("Invalid mask syntax: Trailing backslash");
                this._addLiteralToMask("\\", casing);
                escaping = false;
            }
        } else {
            this.logWarn("Invalid mask syntax: Character class regular expression was not terminated");
            inRegex = false;
            customRegex += "]";
            this._addCustomRegexToMask(customRegex, casing);
            customRegex = "";
        }
    },

    _addLiteralToMask : function(c, casing) {
        this._maskFilters.push (null);
        this._maskBuffer.push (c);
        this._length++;
    },

    _addCustomRegexToMask : function (customRegex, casing) {
        this._maskFilters.add({
            filter: new RegExp(customRegex),
            casing: casing
        });
        if (this._firstNonMaskPos == null) {
            this._firstNonMaskPos = this._maskFilters.length - 1;
        }
        this._maskBuffer.push(this.maskPromptChar);
        this._length++;
    },

    _addUnknownToMask : function(c, casing) {
        // Define standard keypress filters
        var def = isc.TextItem._filterDefinitions[c];
        if (def) {
            this._maskFilters.push(
                { filter: new RegExp (def.charFilter),
                  casing: casing }
            );
            if (this._firstNonMaskPos == null) {
                this._firstNonMaskPos = this._maskFilters.length - 1;
            }
            this._maskBuffer.push (this.maskPromptChar);
        } else {
            // No filter defined for character. Assumed to be a literal.
            this._maskFilters.push (null);
            this._maskBuffer.push (c);
        }
        // Add to our length
        this._length++;
    },

    // Mask handling private helper methods
    // Selection handling wrapper methods
    _getSelection : function () {
        var range = this.getSelectionRange();
        if (range == null) range = [0,0];
        return { begin: range[0], end: range[1] };
    },
    _setSelection : function (begin, end) {
        // end parameter is optional. If not passed, it is matched to begin
        // to set the caret position.
        if (this.hasFocus) {
            end = (isc.isA.Number(end) ? end : begin);
            this.setSelectionRange(begin, end);
        }
    },
    // Get position of next user-entered character (i.e. non-literal)
    _getNextEntryPosition : function (pos) {
        while (++pos < this._length) {
            if (this._maskFilters[pos]) return pos;
        }
        return this._length;
    },
    // Get last unentered character position
    _getEndPosition : function () {
        var lastMatch = 0;
        for (var i = this._length-1; i >= 0; i--) {
            if (this._maskFilters[i]) {
                if (this._maskBuffer[i] == this.maskPromptChar) 
                    lastMatch = i;
                else
                    break;
            }
        }
        return lastMatch;
    },
    // Map the stored value to the display (edit) format.
    // There are two ways a value can be stored: with literals and without.
    // If stored with literals, all entered characters and literals are mapped
    // directly into the mask.
    // If stored without literals the characters have to be placed into the
    // mask from left to right as if typed by the user. 
    // When this control has focus, maskPromptChars are used to fill in unentered
    // characters in the mask. When focus is lost, these same characters are
    // replaced by the maskPadChar.
    // 
    _maskValue : function (value) {
        if (value == null) value = "";
        if (!isc.isA.String(value)) value = value.toString();

        // Clear buffer contents of entered characters. All that is left are
        // the literals and maskPromptChars.
        this._clearMaskBuffer (0, this._length);

        // Keep up with the last character matched into the mask.
        var lastMatch = -1;

        // Merge value into buffer
        if (this.maskSaveLiterals) {
            // value should be a one-to-one match for mask
            for (var i = 0, pos = 0; i < value.length; i++) {
                if (this._maskFilters[i]) {
                    // Position expects user entry
                    var c = value.charAt(i);

                    // Map a space to maskPromptChar when focused.
                    // Or place entered character into buffer.
                    if (c == " " ) {
                        if (!this.hasFocus)
                            this._maskBuffer[i] = c;
                    } else if (this._maskFilters[i].filter.test(c)) {
                        this._maskBuffer[i] = c;
                        lastMatch = i;
                    }
                }
            }
        } else {
            // try to place characters into mask as if type manually.
            for (var i = 0, pos = 0; i < this._length; i++) {
                if (this._maskFilters[i]) {
                    while (pos < value.length) {
                        var c = value.charAt (pos++);

                        // If there is a space in this position, let it be
                        // replaced with the maskPromptChar because it can
                        // be entered.
                        var maskFilter = this._maskFilters[i];
                        if (c == " ") {
                            if (!this.hasFocus) {
                                this._maskBuffer[i] = (maskFilter.casing ? this._mapCharacterCase(c, maskFilter.casing) : c);
                            }
                            break;
                        } else if (maskFilter.filter.test(c)) {
                            this._maskBuffer[i] = (maskFilter.casing ? this._mapCharacterCase(c, maskFilter.casing) : c);
                            lastMatch = i;
                            break;
                        }
                    }
                    if (pos > value.length) break;
                }
            }
        }

        value = this._getMaskBuffer();
        if (!this.hasFocus) {
            // If there are literals after the last matched entry, include
            // those in display.
            if (lastMatch >= 0) {
                for (var i = lastMatch + 1; i < this._length; i++) {
                    if (this._maskFilters[i]) break;
                    lastMatch++;
                }
            }
            // Chop display value to remove trailing spaces
            value = value.substring (0, lastMatch + 1);
        }

        return value;
    },
    // Map the edit value to the stored format.
    _unmaskValue : function (value) {
        // Display should be in masked format. Convert it to desired output format.
        if (value == null) value = "";

        // We need to know if there is anything in the display value other
        // than literals. This way an empty value is produced when done.
        // The resulting value should also be chopped after the last entered
        // or literal character.
        var hasNonLiterals = false;
        var lastValidChar = -1;

        var newValue = "";
        for (var i = 0, pos = 0; i < value.length; i++) {
            var c = value.charAt (i);

            if (this._maskFilters[i]) {
                if (c != this.maskPromptChar && this._maskFilters[i].filter.test (c)) {
                    // Valid character at this position
                    newValue += c;
                    hasNonLiterals = true;
                    lastValidChar = pos++;
                } else {
                    // Invalid character
                    newValue += this.maskPadChar;
                    pos++;
                }
            } else if (this.maskSaveLiterals) {
                // Literal character
                newValue += c;
                lastValidChar = pos++;
            }
        }

        // Truncate result
        if (!hasNonLiterals) {
            newValue = "";
        } else {
            newValue = newValue.substring (0, lastValidChar + 1);
        }

        return newValue;
    },

    // Mask buffer helper methods
    _getMaskBuffer : function () {
        if (this._maskBuffer == null) return "";
        return this._maskBuffer.join('');
    },
    _clearMaskBuffer : function (start, end) {
        for (var i = start; i < end && i < this._length; i++) {
            if (this._maskFilters[i]) this._maskBuffer[i] = this.maskPromptChar;
        }
    },
    _saveMaskBuffer : function (changed) {
        // Update our saved value so a call to getValue() will return our
        // current edit value. Don't call setValue() because it requires
        // the unformatted value and then formats it. We already have a
        // formatted (display) value.
        var buffer = this._getMaskBuffer();

        // Show current display value
        this.setElementValue (buffer);

        if (changed && this.changeOnKeypress) {
            var value = this._unmaskValue(buffer);
            
            // fire the change handler, (handles validation etc)
            var returnVal = this.handleChange(value, this._value);
            // The change handler may call 'setItems' on the form (particularly likely in LG
            // editing) in which case we'll be destroyed
             
            if (this.destroyed) return;
            // Ensure we have the latest value (stored as this._changeValue)
            value = this._changeValue;
            // We may need to perform some visual updates based on the new value - do this here
            this.updateAppearance(value);
            // save the value
            this.saveValue (value);
            // fire any specifed 'changed' handler for this item.
            this.handleChanged(value);

            return returnVal;
        } 
        return true;
    },
    // Position caret at offset in field
    _positionCaret : function (pos, offset) {
        if (offset < 0) {
            while (!this._maskFilters[pos] && pos >= 0) pos--;
        } else {
            while (!this._maskFilters[pos] && pos < this._length) pos++;
        }
        this._setSelection (pos);
    },
    // Shift contents of buffer to left starting at <pos>
    _shiftMaskBufferLeft : function (pos, len) {
        // Skip any user-entered positions to find left-most position to
        // receive shifted contents.
        if (!len) len = 1;
        while (!this._maskFilters[pos] && pos >= 0) pos--;

        // Move each character <len> positions to the left where the character
        // matches the new position's filter.
        for (var i = pos, pos2 = i+len-1; i < this._length; i++, pos2 = j) {
            if (this._maskFilters[i]) {
                this._maskBuffer[i] = this.maskPromptChar;
                var j = this._getNextEntryPosition (pos2++);
                var filter = this._maskFilters[i];
                var c = this._maskBuffer[j];
                if (j < this._length && filter.filter.test (c)) {
                    // Perform character case changes
                    if (filter.casing) {
                        c = this._mapCharacterCase (c, filter.casing);
                    }
                    this._maskBuffer[i] = c;
                } else {
                    while (i < j) {
                        if (this._maskFilters[i]) this._maskBuffer[i] = this.maskPromptChar;
                        i++;
                    }
                    break;
                }
            }
        }
    },

    // Shift contents of buffer to right starting at <pos>
    _shiftMaskBufferRight : function (pos) {
        for (var i = pos, c = this.maskPromptChar; i < this._length; i++) {
            var filter = this._maskFilters[i];
            if (filter) {
                // Perform character case changes
                if (filter.casing) {
                    c = this._mapCharacterCase (c, filter.casing);
                }
                var j = this._getNextEntryPosition (i);
                var t = this._maskBuffer[i];
                this._maskBuffer[i] = c;
                if (j < this._length && this._maskFilters[j].filter.test (t)) {
                    c = t;
                } else {
                    break;
                }
            }
        }
    }

});

