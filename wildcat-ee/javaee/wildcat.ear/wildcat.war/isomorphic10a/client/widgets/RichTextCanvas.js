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





  
 

 
//>	@class RichTextCanvas
//  
//  Canvas to be used for Rich Text Editing
//
//<
isc.ClassFactory.defineClass("RichTextCanvas","Canvas");

isc.RichTextCanvas.addClassProperties({
    // enumerated Justification types
    //CENTER:"center",
    //LEFT:"left",
    //RIGHT:"right",
    FULL:"full",

    //>@classAttr   RichTextCanvas.unsupportedErrorMessage  (string : "Rich text editing not supported in this browser" : [IRW])
    // Message to display to the user if they attempt to access the page in a browser which
    // does not support rich-text-editing
    //<
    
    unsupportedErrorMessage : "Rich text editing not supported in this browser"
});

isc.RichTextCanvas.addProperties({

	editable:true,

    // Override 'canSelectText': to allow for most editing actions the user must be able to 
    // select the content from this widget.
    canSelectText:true,

    // RTC's are focusable
    canFocus: true,

    // Don't write out a focusProxy for RichTextCanvases - we don't want native keyboard 
    // focus to go to a hidden element
    // Instead - in design mode we apply tabIndex directly to the content frame
    // otherwise we rely on native tabIndex to allow focus in the widget handle at
    // the correct times.
    _useFocusProxy:false,

    //> @attr RichTextCanvas.moveFocusOnTab (boolean : true : IRW)
    // If the user presses the "Tab" key, should focus be taken from this editor?
    // If set to <code>false</code> a "Tab" keypress will cause a Tab character
    // to be inserted into the text, and focus will be left in the edit area.
    // @visibility external
    //<
    moveFocusOnTab: true,

    
    overflow:isc.Canvas.AUTO,
    showCustomScrollbars:false,

    // If a syntax rehilite of the entire contents is required, do it this number of
    // milliseconds after the last keystroke (resets to this number every time the user hits a key)
    fullSyntaxHiliteDelay: 3000,
    
    // Don't show a non-breaking space by default
    contents : ""

    // even when hidden the rich text area picks up the bitmap of the area behind it and drags it
    // along as it is scrolled.
    // 
    // XXX not a viable workaround - when hidden in this manner, it fails to show()
//    hideUsingDisplayNone: isc.Browser.isMoz
});

isc.RichTextCanvas.addClassMethods({

    
    //> @classMethod RichTextCanvas.supportsRichTextEditing()    
    //  Does this browser support rich text editing, using the isc.RichTextCanvas class?
    //
    //  @return (boolean)   true if supported by this browser.
    //<
    supportsRichTextEditing : function () {
        var supported = ((isc.Browser.isSafari && isc.Browser.safariVersion >= 312) ||
                         (isc.Browser.isIE) || 
                         // Tested Moz (>=1.4 on Linux / Mac / Windows)
                         //        Firefox (>=1.0 on Linux / Mac / Windows)
                         // Doesn't work on latest camino as of May 17 05 (Version 0.8.4)
                         (isc.Browser.isMoz && !isc.Browser.isCamino) ||
                         isc.Browser.isOpera
                        );
        return supported;
    },

    _fixTabSpan : function (tabSpan) {
        tabSpan.innerHTML = "&#9;";
    }
});

//!>Deferred
isc.RichTextCanvas.addMethods({

    // On init, verify that we're in a supported browser, and that the overflow is "auto"
    initWidget : function () {
                         
        if (!isc.RichTextCanvas.supportsRichTextEditing()) {
            var errorMessage = isc.RichTextCanvas.unsupportedErrorMessage;
            
            this.logError(errorMessage);
        }
    
        if (this.overflow != isc.Canvas.AUTO) {
            this.logWarn('RichTextCanvas class currently only supports an overflow property of "auto"');
            this.overflow = isc.Canvas.AUTO;
        }
        
        // In "design mode" - where we write out an iframe with editable body content, 
        // turn off native tab index on the handle. We'll instead apply the tabIndex directly
        // to the iframe
        if (this._useDesignMode()) {
            this._useNativeTabIndex = false;
        }
        
        this.Super("initWidget", arguments);
    },
    
    // Override getHandleOverflow - we always have an overflow of "auto" specified on the widget
    // but if we're writing out an editable IFRAME, any scrollbars will show up on the inner 
    // content frame rather than the handle, so we never want to show scrollbars on the handle
    
    _getHandleOverflow : function () {
        if (this._useDesignMode()) {
            var overflow;
            if (this._useMozScrollbarsNone) {
                overflow = "-moz-scrollbars-none";
                this._useMozScrollSize = true;
            } else {
                
                overflow = this._$hidden;
            }
            return overflow;
        } else return this.Super("_getHandleOverflow", arguments);
    },
    
    // getInnerHTML() overridden to write out an editable area.
    getInnerHTML : function () {
    
        // If we're writing out an IFrame with designMode:"On", return the appropriate HTML    
        if (this._useDesignMode() && !this.isPrinting) {
            return this.getIFrameHTML();
        }

        // Otherwise we'll just be setting contentEditable on the standard widget handle.
        //  
        // Note: we used to call Super here, but the Canvas implementation calls getContents()
        // with no args which in this case returns the un-marked-up source, resulting in
        // hilighting breaking on redraw.  In this particular case, return the marked up
        // contents since we'll be assigning to innerHTML
        return this.getContents(true);
    },
    
    // _useDesignMode: Should we achieve our rich text canvas via an IFrame with DesignMode "On",
    // or via a contentEdtiable DIV.
    
    _useDesignMode : function () {
        return ((isc.Browser.isChrome ||
                 isc.Browser.isSafari ||
                 isc.Browser.isOpera ||
                 isc.Browser.isMoz) ||
                isc.screenReader);
    },

    // ---------- Design Mode / IFRAME handling ------------------
    _$iframeTemplate: [
         

        

        "<iframe id='",
        , // [1] this.getIFrameID()
        "' style='margin:0px;padding:0px;border:0px;width:",
        
        , // [3] this.getContentFrameWidth()
        "px;height:",
        , // [5] this.getContentFrameHeight()
        "px'",

        
        (isc.Browser.isWebKit || isc.Browser.isIE
         ? " src='" + isc.Page.getURL("[HELPERS]empty.html") + "'"
         : null),

        " onload='",
        , // [9] this.getID()
        "._frameLoaded();' tabindex='",
        , // [11] this.getTabIndex()
        "'></iframe>"
    ],
    getIFrameHTML : function () {
        var srcArray = this._$iframeTemplate;
        srcArray[1] = this.getIFrameID();
        srcArray[3] = this.getContentFrameWidth();
        srcArray[5] = this.getContentFrameHeight();
        srcArray[9] = this.getID();
        srcArray[11] = this.getTabIndex();
        return srcArray.join(isc.emptyString);
    },

    _setHandleTabIndex : function (index) { 
        if (this._useDesignMode()) {
            var frame = this.getContentFrame();
            if (frame != null) frame.tabIndex = index;
        } else {
            return this.Super("_setHandleTabIndex", arguments);
        }
    },


    // getBrowserSpellCheck - function to determine if we want to use native browser spellcheck
    // functionality where present.
    
    getBrowserSpellCheck : function () {
        return true;
    },

    // _frameLoaded - helper method to notify us that the IFRAME has loaded, so we can
    // set up its contents / editability.
    _frameLoaded : function () {
        if (!this._drawingFrame) return;
        delete this._drawingFrame;
        if (!this.isDrawn()) return;
        this._setupEditArea();
    },

    // Get the ID for the frame in the DOM
    getIFrameID : function () {
        return this.getID() + "_iframe";
    },

    // Get a pointer to the IFRAME content document
    getContentDocument : function () {

        if (!this._useDesignMode()) return this.getDocument();

        
        var win = this.getContentWindow(),
            doc = win ? win.document : null;

        if (doc == null) {
            // This can happen validly as the document is not always available immediately 
            // after drawing.
            
            this.logDebug("Unable to get pointer to content document. Content may not be written out");
        }
        return doc;
    },

    // Get a pointer to the document body
    getContentBody : function () {
        var doc = this.getContentDocument();
        if (doc) return doc.body;
        return null;
    },

    // Get a pointer to the IFRAME window object.
    getContentWindow : function () {
        if (!this._useDesignMode()) return this.getWindow();

        var element = this.getContentFrame();
        return element ? element.contentWindow : null;
    },

    // get a pointer to the IFRAME element in the DOM
    getContentFrame : function () {
        if (!this._useDesignMode() || !this.isDrawn()) return null;

        return isc.Element.get(this.getIFrameID());
    },


    // Scrolling / Overflow:
    

    // Override setOverflow() to be a no-op. We've already guaranteed the overflow will be
    // 'auto' when the RichTextCanvas is initialized in initWidget().
    setOverflow : function () {
    
    },
    
    // getScrollHandle()    Returns a pointer to the element that gets natively scrolled by
    // calls to scrollTo().
    // - Overridden to point to the content body if _useDesignMode() is true.
    getScrollHandle : function () {
        if (this._useDesignMode()) return this.getContentBody();

        return this.Super("getScrollHandle", arguments);
    },
    
    // Override the internal adjustOverflow method.
    // If we're showing an IFrame, the default implementation will not reliably calculate 
    // whether scrollbars are visible.
    __adjustOverflow : function () {
        // always call the standard 'adjustOverflow' method to ensure we setHandleRect etc as
        // appropriate.
        this.Super("__adjustOverflow", arguments);    
        
        // If we're not writing out an IFrame we can just do normal overflow adjustment
        // Overflows other than "auto" are not really supported - in this case just return too.
        if (!this._useDesignMode() || this.overflow != isc.Canvas.AUTO) return;

        // Update hscrollOn/ vscrollOn - not reliably set by the standard adjustOverflow logic.
        var scrollHeight = this.getScrollHeight(),
            scrollWidth = this.getScrollWidth(),
            height = this.getHeight(), width = this.getWidth(),
            scrollbarSize = this.getScrollbarSize(),
            hscrollOn = false, vscrollOn = false;

        if (scrollHeight > height) vscrollOn = true;
        if (hscrollOn) width -= scrollbarSize;
        if (scrollWidth > width) hscrollOn = true;
        if (hscrollOn && !vscrollOn && (scrollHeight > height - scrollbarSize)) vscrollOn = true;
 
        this.hscrollOn = hscrollOn;
        this.vscrollOn = vscrollOn;
        
    },

    // methods to return the size for the content frame if we're using design mode.
    
    getContentFrameWidth : function () {
       return this.getWidth() - this.getHMarginBorderPad();
    },
    
    getContentFrameHeight : function () {
       return this.getHeight() - this.getHMarginBorderPad();
    },

    // Override _setHandleRect() to always size the IFRAME to match the size of the 
    // handle.
    _setHandleRect : function (left, top, width, height) {
        this.Super("_setHandleRect", arguments);
        
        
        if (this._useDesignMode()) {
            var cf = this.getContentFrame();
            if (cf != null) {
                var innerWidth = this.getContentFrameWidth(), innerHeight = this.getContentFrameHeight();
                cf.style.width = innerWidth -1 + "px";
                cf.style.height = innerHeight -1 + "px";
            }
        } else {
            var handle = this.getHandle();
            if (handle != null) {
                var innerWidth = this.getContentFrameWidth(), innerHeight = this.getContentFrameHeight();
                handle.style.width = innerWidth -1 + "px";
                handle.style.height = innerHeight -1 + "px";
            }
        }
    },
    
    // Override getScrollHeight() / width to look at the IFRAME body scroll height, since the 
    // IFRAME will always be sized to 100%, making the scroll size of the widget handle always 
    // equal to the specified size.
    getScrollWidth : function (calculateNewValue) {
        if ((this._scrollWidth && !calculateNewValue) || !this._useDesignMode()) 
            return this.Super("getScrollWidth", arguments);
        
        var cb = this.getContentBody();
        if (!cb) return this.Super("getScrollWidth", arguments);

        // cache the scrollWidth for next time this method is called.
        this._scrollWidth = isc.Element.getScrollWidth(cb);
        return this._scrollWidth;
    },
    
    getScrollHeight: function (calculateNewValue) {
        if ((this._scrollHeight && !calculateNewValue) || !this._useDesignMode()) 
            return this.Super("getScrollHeight", arguments);
        
        var cb = this.getContentBody();
        if (!cb) return this.Super("getScrollHeight", arguments);
        
        this._scrollHeight = isc.Element.getScrollHeight(cb);
        return this._scrollHeight;
    },
    
    // --------------------------------------
    
    
    


    // _rememberSelection() - saves out the current selection position, so we can re-set it
    // when this element regains focus. Only used in IE.
    
    _rememberSelection : function () {
        if (!isc.Browser.isIE) return;

        // Check whether we currently have selection before proceeding - otherwise we could
        // remember some text range outside our handle.
        if (!this._hasSelection()) return;

        if (isc.Browser._hasDOMRanges) {
            var sel = this.getContentDocument().getSelection();
            if (sel.rangeCount <= 0) {
                this._savedSelection = null;
                this._savedSelectionAnchorNode = null;
                this._savedSelectionFocusNode = null;
                this._oldSelectionText = null;
            } else {
                var range = this._savedSelection = sel.getRangeAt(0);
                this._savedSelectionAnchorNode = sel.anchorNode;
                this._savedSelectionFocusNode = sel.anchorNode;
                this._oldSelectionText = String(range);
            }
        } else {
            this._savedSelection = this.getContentDocument().selection.createRange();
            // Also remember the content of the selection. If the content changes, we don't
            // want a call to '_resetSelection' to select the new text. 
            this._oldSelectionText = this._savedSelection.text;
            // this.logWarn("just saved selection :"+ Log.echo(this._savedSelection));
        }
    },

    // _hasSelection() - Is the current document selection within the RichTextCanvas?
    // Used by _rememberSelection() [IE only]
    _hasSelection : function () {
    
        if (!this.isDrawn()) return false

        if (!isc.Browser.isIE) return;

        
        if (this._useDesignMode()) {
            return (this.getActiveElement() == this.getContentFrame());
        }
        
            
        
        var handle = this.getHandle();
        if (!handle) return false;
        
        var selElement = isc.Element._getElementFromSelection();
        if (!selElement) return false;
        
        return handle.contains(selElement);
    },
    
    // Remember the selection every time it changes, so we can reset the selection on focus 
    // or execCommand)
    
    selectionChange : function () {
        if (!this._focussing) {
            this._rememberSelection();

            
            if (isc.Browser.isIE &&
                (isc.Browser.version <= 8 ||
                 (isc.Browser.version == 9 && !isc.Browser.isIE9)))
            {
                this._queueContentsChanged();
            }
        }
    },

    // _resetSelection: resets selection to whatever it was last time this RTC had focus.
    
    _resetSelection : function () {
        if (!this.editable || !this.isDrawn() || !this.isVisible()) return;

        if (isc.Browser.isIE) {
            // If no  previous selection, just bail 
            if (!this._savedSelection) return;

            var newSelectionText = isc.Browser._hasDOMRanges ? String(this._savedSelection) : this._savedSelection.text;

            // If the content of the range has changed since it was selected, avoid selecting
            // the modified text
            
            if (this._oldSelectionText != newSelectionText) {
                this._savedSelection.collapse(false);
            }
            
            isc.EH._allowTextSelection = true;
            if (isc.Browser._hasDOMRanges) {
                var doc = this.getContentDocument(),
                    sel = doc.getSelection();
                sel.removeAllRanges();
                sel.addRange(this._savedSelection);
            } else {
                this._savedSelection.select();
            }
            delete isc.EH._allowTextSelection;

        //} else {    //Currently only supported on IE    
        }
    },

    
    
    // Override setFocus() - when focussing in this widget we want the selection to be
    // whatever it was before the widget was blurred, and for the editable text to have
    // keyboard focus.
    setFocus : function (hasFocus) {
        // Call the Superclass implementation.
        
        this._focussing = true;
        this.Super("setFocus", arguments);
        this._focussing = false;
        
        // If we're using an IFRAME ensure it has focus natively
         
        if (this._useDesignMode()) {
            var win = this.getContentWindow();
            if (!win) return;
            
            if (hasFocus) win.focus()
            
            else window.focus();
        
        // Making this widget's handle contentEditable.
        
        } else {
        
            if (hasFocus) {
            	this._resetSelection();

            }
            
            //else this._rememberSelection();
        }
        
    },
    
    // ------------------- Editor init ------------------
    
    // Override draw to ensure we make the HTML editable when it's done drawing.
    draw : function () {
        this.Super("draw", arguments);
        
        // If we're writing out an IFRAME we need to show an event mask for this canvas.
        if (this._useDesignMode())
            isc.EventHandler.registerMaskableItem(this, true);
        
        // Initialize the contents via _setupEditArea();
        
        if (this._useDesignMode()) {
            this._drawingFrame = true;

        } else {
            this._setupEditArea();
        }

    },
    

    
    redraw : function () {
        var reinitRequired = this._useDesignMode();
        if (reinitRequired) this._rememberContents();
        
        this.Super("redraw", arguments);
        if (reinitRequired) this._drawingFrame = true;
    },

    // _setupEditArea:  Fired when the RichTextCanvas is written into the DOM.
    // This will ensure the appropriate contents and edit state are applied to this widget.
    
    _setupEditArea : function () {
        // Update the HTML to ensure that this is actually editable.

        

        var designMode = this._useDesignMode();

        // When using an IFRAME written out in design mode we need to add some custom event
        // handlers.
        if (designMode) {
            // Capture keyboard events so we can fire our keypress handler.
            // Also capture scrolling on the IFRAME directly to update our scroll position,
            // since we're showing native scrollbars on that element.
            
            
            var addKeyboardListenersToContentDoc = isc.Browser.isChrome || isc.Browser.isSafari,
                thisAccessPath = (addKeyboardListenersToContentDoc
                                  ? "this.defaultView.frameElement.ownerDocument.defaultView."
                                  : "");

            
            if (!this._editInputHandler) {
                this._editInputHandler = isc._makeFunction(
                                              "", 
                                              thisAccessPath + this.getID() + "._iFrameInput()"
                                             );
            }

            if (!this._editKeyPressHandler) {
                this._editKeyPressHandler = isc._makeFunction(
                                                 "event",
                                                 "event=event||" + this.getID() + ".getContentWindow().event;" +
                                                 "var returnValue=" + thisAccessPath + this.getID() + "._iFrameKeyPress(event);" +
                                                 "if(returnValue==false && event.preventDefault)event.preventDefault();" +
                                                 "else event.returnValue=(returnValue!=false)"
                                                );
            }
            if (!this._editKeyDownHandler) {
                this._editKeyDownHandler = isc._makeFunction(
                                                 "event",
                                                 "event=event||" + this.getID() + ".getContentWindow().event;" +
                                                 "var returnValue=" + thisAccessPath + this.getID() + "._iFrameKeyDown(event);" +
                                                 "if(returnValue==false && event.preventDefault)event.preventDefault();" +
                                                 "else event.returnValue=(returnValue!=false)"
                                                );
            }
            if (!this._editKeyUpHandler) {
                this._editKeyUpHandler = isc._makeFunction(
                                                 "event",
                                                 "event=event||" + this.getID() + ".getContentWindow().event;" +
                                                 "var returnValue=" + thisAccessPath + this.getID() + "._iFrameKeyUp(event);" +
                                                 "if(returnValue==false && event.preventDefault)event.preventDefault();" +
                                                 "else event.returnValue=(returnValue!=false)"
                                             );
            }
            if (!this._editScrollHandler) {
                this._editScrollHandler = isc._makeFunction(
                                                 "event",
                                                 "event=event||" + this.getID() + ".getContentWindow().event;" +
                                                 "var returnValue=" + this.getID() + "._iFrameScroll(event);" +
                                                 "if(returnValue==false && event.preventDefault)event.preventDefault();" +
                                                 "else event.returnValue=(returnValue!=false);" +
                                                 "return (returnValue!=false)"
                                                );
            }

            if (!this._editFocusHandler) {
                this._editFocusHandler = isc._makeFunction(
                                                "",
                                                this.getID() + "._iFrameOnFocus();"
                                               );
            }
            if (!this._editBlurHandler) {
                this._editBlurHandler = isc._makeFunction(
                                                "",
                                                this.getID() + "._iFrameOnBlur();"
                                              );
            }
            var win = this.getContentWindow(),
                contentDoc = win.document;
            

            
            var keyboardListenersReceiver = (addKeyboardListenersToContentDoc
                                             ? contentDoc
                                             : win);
            if (isc.Browser.isIE && !isc.Browser.isIE9) {
                keyboardListenersReceiver.attachEvent("keypress", this._editKeyPressHandler);
                keyboardListenersReceiver.attachEvent("keydown", this._editKeyDownHandler);
                keyboardListenersReceiver.attachEvent("keyup", this._editKeyUpHandler);

                win.onscroll = this._editScrollHandler;
                win.onfocus = this._editFocusHandler;
                win.onblur = this._editBlurHandler;
            } else {
                keyboardListenersReceiver.addEventListener("input", this._editInputHandler, false);
                keyboardListenersReceiver.addEventListener("keypress", this._editKeyPressHandler, false);
                keyboardListenersReceiver.addEventListener("keydown", this._editKeyDownHandler, false);
                keyboardListenersReceiver.addEventListener("keyup", this._editKeyUpHandler, false);

                win.addEventListener("scroll", this._editScrollHandler, false);
                win.addEventListener("focus", this._editFocusHandler, false);
                win.addEventListener("blur", this._editBlurHandler, false);
            }
            if (addKeyboardListenersToContentDoc) {
                contentDoc.body.handleNativeEvents = "false";
                
                contentDoc.documentElement.handleNativeEvents = "false";
            }

            var bodyStyle = this.getContentBody().style;
            // Suppress the default margin
            bodyStyle.margin = "0px";

            // Apply text-properties from our specified CSS class to the content of the 
            // IFRAME.
            
            var classStyle = isc.Element.getStyleDeclaration(this.className);
            if (classStyle != null) {
                var textStyleAttrs = isc.Canvas.textStyleAttributes;

                for (var i = 0; i < textStyleAttrs.length; i++) {
                    var attr = textStyleAttrs[i];
                    bodyStyle[attr] = classStyle[attr];
                }
            }
        }

        // In moz, if we want native spell-check behavior enable it here (otherwise
        // explicitly disable it).
        if (isc.Browser.isMoz) {
            var contentBody = this.getContentBody();
            if (contentBody) contentBody.spellcheck = (!!this.getBrowserSpellCheck())
        }

        var editable = (this.editable && !this.isDisabled());

        // Actually make the handle editable
        if (!designMode) this._setHandleEditable(editable);
        else {
            
            this.delayCall("_setHandleEditable", [editable,true], 0);
        }

        // set up our initial contents
        //
        // we're calling _setContents() which means we're bypassing hiliting - this is what we
        // want most of the time because this method is called on redraws when the user may
        // have just resized the browser, so there's no reason to recolorize.  But if this is
        // the first time we're drawing, we need to hilite if we have a syntaxHiliter because 
        // contents may have been provided as an init parameter and if we don't do this it
        // won't get syntax hilited.
        //
        // Note: important to do this after all of the above - to make sure the correct
        // styling is applied the first time - otherwise we get a partially styled rendering
        // which snaps to the fully styled rendering after about a second - even on fast systems.
        if (this.syntaxHiliter && !this.formattedOnce) {
            this.formattedOnce = true;
            this.contents = this.hiliteAndCount(this.contents);
        }
        this._setContents(this.contents);
    },

    // ----------------- Event handling ----------------------

    _nativeCutPaste : function () {
        if (!this._settingContents) {
            this._rememberSelection();
            this._queueContentsChanged();
        }
    },

    // _iFrameInput() is used to handle an 'input' event on the <iframe> when using designMode
    _iFrameInput : function () {
        this._queueContentsChanged();
    },

    // If using designMode, we need a handler for the native keypress event on our IFRAME
    _iFrameKeyPress : function (event) {
        
        // apply the properties (keyName, etc.) to EH.lastEvent     
        isc.EH.getKeyEventProperties(event); 
        // Fall through to standard handling, making sure this widget is logged as the 
        // keyTarget
        return isc.EH.handleKeyPress(event, {keyTarget:this});        
    },
    _iFrameKeyDown : function (event) {
        // apply the properties (keyName, etc.) to EH.lastEvent     
        isc.EH.getKeyEventProperties(event); 
        return isc.EH.handleKeyDown(event, {keyTarget:this});        
    },    
    _iFrameKeyUp : function (event) {
        
        // apply the properties (keyName, etc.) to EH.lastEvent     
        isc.EH.getKeyEventProperties(event); 
        return isc.EH.handleKeyUp(event, {keyTarget:this});        
    },    
    
    // If using designMode, we need a handler for the native scroll event on our IFRAME
    // to update the stored scroll position of the handle on scroll.
    // The standard handleCSSScroll method will handle scrolling (as it will check the
    // scroll position of this.getScrollHandle() - which points at the IFRAME).
    _iFrameScroll : function (event) {
        return this._handleCSSScroll(event);
    },
    
    _iFrameOnFocus : function () {
        if (this.destroyed) return;
        isc.EH.focusInCanvas(this, true);
        return true;
    },
    
    _iFrameOnBlur : function () {
        if (this.destroyed) return;
        isc.EH.blurFocusCanvas(this, true);
        return true;
    },

    _$Tab: "Tab",
    _$tabHTML: "<span class='_isc_tab' style='white-space:pre'>\t</span>",
    handleKeyDown : function (event, eventInfo, c, d, e) {
        var key = isc.EH.getKey();
        if (key == this._$Tab) {
            // Move focus
            if (this.moveFocusOnTab) {
                this._focusInNextTabElement(!isc.EH.shiftKeyDown());

            // Otherwise, insert a tab character
            } else {
                this.insertHTML(this._$tabHTML);
            }
            return false;
        }
        return this.invokeSuper(isc.RichTextCanvas, "handleKeyDown", event, eventInfo, c, d, e);
    },

    // Adjust overflow on keypress - updates recorded scroll width/height
    _$br:"<br>",
    _$Enter:"Enter",
    // set of keys that are ignored by handleKeyPress because they can't modify the contents of
    // the editable area.  This isn't exhaustive - the main reason to have these is to
    // eliminate gratuitous syntax hilighting while e.g. the user is using arrow keys to
    // navigate around the document.
    ignoreKeys : ["Arrow_Up", "Arrow_Down", "Arrow_Left", "Arrow_Right", "Ctrl", "Alt", "Tab",
        "Space"
    ],
    handleKeyPress : function (event, eventInfo) {
        var key = isc.EH.getKey();
        
        if (this.ignoreKeys.contains(key)) return isc.EH.STOP_BUBBLING;

        // figure out the start line number of the current selection before the key stroke so
        // we can extract the modified line(s) later.
        if (this.countLines) this.rememberSelectionStartLine();

        
        this._queueContentsChanged();
        var returnVal = this.Super("handleKeyPress", arguments);        

        // in IE, we set a timer onpaste to do syntax hiliting - this enalbes us to respond to
        // a paste command initiated via the context menu or "Edit" menu, but when a user hits
        // a keyboard shortcut to paste, we also get a key press.  So if we're responding to a
        // keypress and there's a paste timer, we delete it so we don't process the paste twice.
        if (isc.Browser.isIE && this._pasteTimer) {
            isc.Timer.clearTimeout(this._pasteTimer);
            delete this._pasteTimer;
        }

        
        

        return returnVal;
    },    

    _queueContentsChanged : function () {
        if (!this._dirtyContent) {
            this._dirtyContent = true;
            if (!this._changedHandlerName) this._changedHandlerName = "_contentsChanged";
            isc.Page.setEvent(isc.EH.IDLE, this, isc.Page.FIRE_ONCE, this._changedHandlerName);
        }
    },

    //_contentsChanged - fired when the contents is edited.
    // Not fired in response to explicit 'setContents' call.
    
    _contentsChanged : function () {
        delete this._dirtyContent;
        
        var oldVal = this.contents,
            newVal = this.getContents();
        if (oldVal == newVal) return;

        // if we're counting lines, then call doLinesChanged().  We're also checking for
        // selectionIsCollapsed() here because Ctrl-A, which causes all contents to be selected
        // should not fire doLinesChanged() - and that's the only known way to get a multichar
        // selection after a keystroke/paste event
        if (this.countLines && this.selectionIsCollapsed()) this.doLinesChanged(oldVal, newVal);

        // AdjustOverflow - our scroll-size is likely to have changed
        this.adjustOverflow("edited");
        
        // Fire this.changed, if present
        if (this.changed != null) this.changed(oldVal, newVal);

        this.contents = newVal;
    },
    

    // ------------------------------------------------------------------------------------
    // lineChanged / Synax Hiliting support
    // ------------------------------------------------------------------------------------
    //
    // We want to detect any change made to the editable area so we can re-format the view.
    // Change can be effected in several ways:
    //   - keypress
    //   - paste action using the browser "Edit" menu
    //   - programmatic update (currently only via setContents())
    //
    // At the time of the change, the user may have an insertion cursor or a block of selected
    // text.
    //
    // We want to format the newly added data, and potentially some data around the new data.
    // To that end we want to:
    //   - mark the location of the current insertion point, so we can restore it after making
    //   changes
    //   - determine the start and end index of the changed text
    //   - expand the above indexes to fully envelop the start and end line of the changed
    //   text.
    //     - we want this because recolorization will happen on a line-by-line basis (for
    //     performance reasons) except for some cases where we'll want to recolorize the
    //     whole document
    //
    // In IE and FF we can insert arbitrary HTML at the insertion cursor.  This allows us to
    // get the current location of the cursor.
    //
    // General issues
    //---------------
    // - In FF, we can't detect what was pasted unless we compare the original contents
    // and the new contents - which is expensive.  In fact, if the user uses the Edit
    // menu paste command, then we can't tell the the editable area even changed.  There's a
    // DOM event called  "onsubtreemodified" that's part of the w3c spec that should at least
    // tell us that the contents changed - but it doesn't work at all in current versions of FF
    // (and this has been corroborated by postings on the web).
    // - In FF, the contents of the editable area change asynchronously with the key event.  In
    // other words you have to set a timeout to get the after-changed state of the editable area.
    // - Detecting current selection/insertion point.  In IE and FF we can detect the current
    // selection/insertion point by wrapping the selection contents in a DOM node (e.g. a span)
    // and then scanning the contents for it.
    // - In FF we lose the insertion cursor if there's no adjoining text.  Also, the cursor
    // marker that we use to extract the cursor position blocks the user from using the right
    // arrow on the keyboard to move to the next character beyond it.
    //
    // Approaches:
    // -----------
    // 1.  Wait for the user to stop typing for a bit and then reformat the entire editable
    // area.  Performance is gated by the formatting algorithm, but this is very easy to
    // implement and may be acceptable for some use cases.
    // - benefits
    //   - easy to implement
    // - problems
    //   - doesn't look as nice, because formatting is not realtime
    //
    // 2.  Determine the edited line by scanning backwards and forwards from the current
    // insertion point looking for <BR>s.
    // - benefits
    //   - easier than maintaining line information in the editable area
    // - problems
    //   - scanning backwards for <BR>s potentially not cheap
    //   - need to both scan backwards for <BR> (so we can select out the HTML to pass to the
    //   formatter) and walk the DOM to the last <BR> so we can efficiently insert the results.
    //
    // 3.  Maintain line information in the editable area by inserting line spans into the
    // contents provided to setContents().  Insert a span with a unique ID into the document at
    // the current insertion point after a change and walk the
    // DOM up from that node to find the current line for fast extraction.
    // - benefits:
    //   - very fast line extraction
    //      - given a selection, can quickly determine what lines it spans.
    //      - after formatting the line, we can replace it effieciently via innerHTML assignment
    //      or equivalent.
    //   - As long as line-based formatting is sufficient for most keystrokes, this approach
    //   gives us an O(1) implementation.
    //   - can get the contents of any line very quickly.  Given a line, can get its immediate
    //   surrounding lines quickly.  Can get the line number quickly.
    // - problems
    //   - These spans must be maintained as the user hits Backspace, Enter and pastes
    //   arbitrary content.
    //     - this means fragmenting multiline pastes and those created by user hitting the
    //     Enter key into separate lines and combining lines created by partial line pastes and
    //     Backspace at beginning of line.
    //       - In IE, copying a whole line out of the editable area also picks up its line span
    //       delimiters, which means we can end up with a line inside a line situation.  
    //         - I think this can be fixed by defining a custom onbeforepaste handler on the
    //         line spans and filtering the line spans out of the data retrieved from the
    //         clipboard.
    //       - In Moz, the <BR> inside the line span isn't picked up by the copy operation and
    //       is replaced by the paste operation if the paste is at the end of the line.
    //       Further, unlikes IE, the line span isn't placed inside the line that gets pasted
    //       into - instead the pasted-into line is fragmented into two line spans by the
    //       browser and the pasted line is added a peer in between those.
    // 
    // 
    // line behaviors
    // --------------
    // - type character at beginning of line
    //   - IE, FF: currentLine contains original chars + new char + selection marker + <br>
    // - type character in the middle of a line 
    //   - IE, FF: as above
    // - type character at end of a line
    //   - IE, FF: as above
    // - no special processing required for the above
    //
    // - hit enter at beginning of line
    //   - IE, FF: current line contains <BR><cursor>origLineText<BR>
    // - hit enter in the middle of a line
    //   - IE, FF: current line contains origLineTextBeforeEnter<BR><cursor>origLineTextAfterEnter<BR>
    // - hit enter at the end of a line
    //   - IE, FF: current line contains origLineText<BR><cursor><BR>
    // - split current line by <BR> count using regexes to extract new lines
    //   
    // - hit backspace at beginning of line
    //   - IE: <cursor> jumps to previous line, deleting the <BR> there.
    //   - FF: previous line missing <BR>, currentLine has <cursor> at start
    // - hit delete at end of line
    //   - IE: current line now missing <BR> (basically as IE above)
    //   - FF: as IE
    // - hit backspace in middle of line
    //   - IE, FF: as typing char in middle of line, except old char deleted
    // - hit backspace at end of line
    //   - IE, FF: as above
    // - if current line is missing <BR>, combine with next, otherwise combine previous with current
    // 
    // - paste external chars (not from a line span) with no BR in line:
    //   - IE: as typing char, but selection goes to front of pasted text
    //   - FF: as typing char
    // - paste external chars (not from a line span) with BR in line:
    //   - IE, FF: as above, plus a <BR> where there's a linebreak in pasted content
    // - handled by above cases
    //
    // - paste chars from a line span with no BR in line:
    //   - IE: if the copied selection touched the start of the line span, then behaves as FF,
    //   except that the pasted line span appears as a child of the pasted-to line span, not a peer.
    //   Otherwise behaves as paste of external chars
    //     - filter out line spans from pasted content with onbeforepaste, onpaste on line spans,
    //     then handled by above cases as external paste
    //
    //   - FF: pasted-to line is fragmented into two line spans by the insertion point.  pasted
    //   text appears as its own line in between the two.  If pasted at end of a line, that
    //   line's BR is moved out of that line's span as the nextSibling the newly created
    //   line span.
    //     - whenever pasting into a line, that line's <BR> is destroyed and either
    //     moves outside the line span (if paste was at end of line) or into a new line
    //     fragment (if there was text to the right of the insertion cursor before paste)
    //     - a paste can be multiline, introducing potentially multiple whole lines, so can't
    //     just look back for a missing <BR> until we hit a normal line (since some intervening
    //     lines may actually be normal, because pasted wholesale from this edit area)
    //     - SOLUTION:
    //        - before paste:
    //          - query total number of lines
    //        - after paste:
    //          - if <BR> is nextSibling outside currentLine (selection always at end of paste),
    //          pull it into currentLine.
    //        - now query total number of lines (after paste).  This is the number of lines we
    //        have to walk back looking for missing <BR>s and combining.
    // 
    // Actual approach used:
    //----------------------
    // #3, except don't combine lines in realtime.  Instead, provide a special change()
    // notification that gives the user the pasted contents and the  pasted contents out to line
    // breaks, and the line number.  The user can then format them and call a method to replace
    // them and insert them at a given location.
    //

    // apply a SyntaxHiliter to the contents
    setSyntaxHiliter : function (syntaxHiliter) {
        if (syntaxHiliter == null) {
            this.removeSyntaxHiliter();
            return;
        }
        this.syntaxHiliter = syntaxHiliter;
        this.countLines = true;

        // apply syntax hiliting to the contents
        var contents = this.getContents() || isc.emptyString;
        this.setContents(contents);
    },

    removeSyntaxHiliter : function () {
        // get the contents (do this before we delete this.syntaxHiliter, otherwise the
        // contents will come with markup.
        var contents = this.getContents() || isc.emptyString;

        delete this.syntaxHiliter;
        delete this.countLines;
        
        // apply the contents, now without the markup
        this.setContents(contents);
    },

    doLinesChanged : function (oldVal, newVal) {
//        this.logWarn("doLinesChanged - newVal: " + newVal);
        var startLineNum = this.getLastSelectionStartLine();
        
        // initial setContents() only
        if (startLineNum == null) return; 

        var startLine = this.getLine(startLineNum);

//        this.logWarn("startLineNum: " + startLineNum);
//        if (!startLine) this.logWarn("startLine is null");
//        else this.logWarn("startLine: " + startLine.innerHTML);
        var html = isc.emptyString;
        var selectionId = this.markCurrentSelection();
        if (isc.Browser.isIE && isc.Browser.version < 11) {
            // startLine contains everything we need - in the event that lines from the editor
            // got pasted back in, those lines appear as children of the current line and the
            // line markers will just get stripped out by unescapeHTML()
            if (!startLine) {
                this.getLineContainer().innerHTML = isc.emptyString;
                var line = this.createLine();
                this.getLineContainer().appendChild(line);
                var range = document.selection.createRange();
                range.moveToElementText(line);
                range.collapse();
                range.select();
                selectionId = this.markCurrentSelection();

                startLineNum = 0;
                startLine = this.getLine(0);
            }
            html = startLine.innerHTML;
        } else {
            var endLine = this.getSelectionStartLine();
            var endLineNum = this.getLineNumber(endLine);
            if (endLineNum < startLineNum) {
                startLine = endLine;
                startLineNum = endLineNum;
            }
//            this.logWarn("endLine: " + endLine.innerHTML);
//            this.logWarn("nextSibling: " + isc.Log.echoAll(endLine.nextSibling));
            var currentLine = startLine;
            var numLines = 0;
            while (currentLine && currentLine != endLine) {
                if (currentLine.innerHTML) {
                    html += currentLine.innerHTML;                                    
//                    this.logWarn("html is now: " + html);
                }
                numLines++;
                currentLine = currentLine.nextSibling;
            }

            // repair bonus BR that gets shunted out of the original line span by FF            
            var nextNode = endLine.nextSibling;
            if (nextNode && nextNode.tagName.toLowerCase() == "br") {
                nextNode.parentNode.removeChild(nextNode);
                endLine.appendChild(nextNode);
//                this.logWarn("repaired br, endline is now: " + endLine.innerHTML);
            }

            html += endLine.innerHTML;

            // if we pasted a line span into the middle of another line span, then it will be
            // fragmented - pick up the trailing fragment
            if (!html.replace(/\n|\r/g, isc.emptyString).match(/<br>$/i)) {
                if (endLine.nextSibling) {
                    html += endLine.nextSibling.innerHTML;
                    numLines++;
                }
            }
        }  

//        this.logWarn("linesChanged: " + html);
        if (!oldVal) {
            oldVal = this.contents;
            newVal = this.getContents();
        }

        // fire linesChanged if it's defined
        if (this.linesChanged) {
            this.linesChanged(oldVal, newVal, startLineNum, numLines, html, selectionId);
        } else if (this.syntaxHiliter) {
            // currently syntaxHiliter is not compatible with linesChanged - use one or the other.
            this.doSyntaxHilite(oldVal, newVal, startLineNum, numLines, html, selectionId);
        }
    },

    doSyntaxHilite : function (oldVal, newVal, startLineNum, numLines, changedHTML, selectionId) {
        // keeping a marker in the code sent to the colorizer, breaks some colorization cases -
        // specifically, this happens if the marker is in the middle of something that would be
        // matched by a regex.  For example in XML colorization of the marker is next to the
        // equal sign in this expression: foo="bar", then that expression won't colorize until
        // something else is edited such that the selection marker moves.

//        this.logWarn("source before html removal: " + changedHTML);
        // remove markup, but keep the selectionSpan so we can extract its index for
        // repositioning the selection correctly after syntax hiliting
        var source = this.removeMarkup(changedHTML, true);        

//        this.logWarn("source after html removal: " + source);
    
        // save off the index of the locationMarker
        var selectionMarkerIndex = this.getSelectionMarkerIndex(source);
        if (selectionMarkerIndex == -1) {
            // marker has been wiped out by a select-all, re-hilite everything
            this.doFullSyntaxHilite();
            return;
        }

        // remove the selectionMarker from the source
        source = this.removeMarkup(changedHTML);

        // if the modified source contains a token that requires us to reformat queue a full
        // hilite, but still do the partial update immediately
//        if (this.syntaxHiliter.containsMultilineToken(source)) this.queueFullHilite();

        // apply the syntax hiliting to just the lines passed in
        var newLines = this.syntaxHiliter.hilite(source, true, selectionMarkerIndex, 
                                                 this._getSelectionSpanHTML(selectionId));
        this.overwriteLines(startLineNum, numLines, newLines);
        this.moveSelectionToMarker(selectionId);
    },

    doFullSyntaxHilite : function () {

//        this.logWarn("full syntax hilite running");
        var selectionId = this.markCurrentSelection();

        var contents = this._getContents();
        var source = this.removeMarkup(contents, true);        

        var selectionMarkerIndex = this.getSelectionMarkerIndex(source);
        if (selectionMarkerIndex == -1) {
            selectionMarkerIndex = contents.length;
        }

        // remove the selectionMarker from the source
        source = this.removeMarkup(contents);
        this.setContents(source, true, selectionMarkerIndex, this._getSelectionSpanHTML(selectionId));
    
        this.moveSelectionToMarker(selectionId);

        delete this.fullHiliteTimer;
    },

    queueFullHilite : function () {
//        this.logWarn("(re)queueing full hilite");
        if (this.fullHiliteTimer) isc.Timer.clearTimeout(this.fullHiliteTimer);
        this.fullHiliteTimer = this.delayCall("doFullSyntaxHilite", [], this.fullSyntaxHiliteDelay);
    },

    selectionIsCollapsed : function () {
        if (isc.Browser._hasDOMRanges) {
            var selection = this.getContentWindow().getSelection();
            return selection.isCollapsed;
        } else if (isc.Browser.isIE) {
            var range = document.selection.createRange();
            return range.text.length == 0;
        }
    },

    rememberSelectionStartLine : function () {
        this.startLineNum = this.getLineNumber(this.getSelectionStartLine());
    },

    getLastSelectionStartLine : function () {
        return this.startLineNum;
    },

    _setPasteTimer : function () {
        this._pasteTimer = this.delayCall("doLinesChanged", [], 0);
    },

    // onpaste/onbeforepaste for IE - caching theset strings
    _getOnBeforePaste : function () {
        if (!this._onBeforePaste) 
            this._onBeforePaste = this.getID()+".rememberSelectionStartLine();event.returnValue=true";
        return this._onBeforePaste;
    },
    _getOnPaste : function () {
        if (!this._onPaste) this._onPaste = this.getID()+"._setPasteTimer();event.returnValue=true"
        return this._onPaste;
    },
    
    // line span HTML caching
    _getLineSpanHTML : function () {
        if (!this._lineSpanHTML) {
            this._lineSpanHTML = "<span isLine='true'";
            // prevent the "bouncing line" effect where adding a space on a line that's clipped
            // causes it to be broken into two lines by the browser (since that space is the first
            // available place to wrap it).  The rendering then snaps back into a single line when
            // the syntaxHiliter converts the space into an &nbsp;
            if (this.syntaxHiliter && !this.syntaxHiliter.autoWrap) 
                this._lineSpanHTML +=" style='white-space:nowrap'";
            if (isc.Browser.isIE) {
                this._lineSpanHTML += " onbeforepaste='"+this._getOnBeforePaste()
                    +"' onpaste='"+this._getOnPaste()+"'"
            }
            this._lineSpanHTML += ">$1</span>";
        }
        return this._lineSpanHTML;
    },

    createLine : function (contents) {
        var doc = this.getContentDocument();
        var line = doc.createElement("span");
        line.setAttribute("isLine", "true");
        // prevent the "bouncing line" effect where adding a space on a line that's clipped
        // causes it to be broken into two lines by the browser (since that space is the first
        // available place to wrap it).  The rendering then snaps back into a single line when
        // the syntaxHiliter converts the space into an &nbsp;
        if (this.syntaxHiliter && !this.syntaxHiliter.autoWrap) 
            line.setAttribute("style", "white-space:nowrap");
        if (isc.Browser.isIE) {
            line.setAttribute("onbeforepaste", this._getOnBeforePaste());
            line.setAttribute("onpaste", this._getOnPaste());
        }
        line.innerHTML = contents ? contents : this._$br;
        return line;
    },

    // returns a string containing an incrementing counter usable unique identifier for the
    // selection span.
    _getNextSelectionId : function () {
        if (!this.selectionIdSequence) this.selectionIdSequence = 0;
        return this.getID()+"_selection_"+this.selectionIdSequence++;
    },

    // returns the DOM node that is the line span containing the start of the current selection.
    getSelectionStartLine : function () {
        var doc = this.getContentDocument();
        var line;
        if (isc.Browser._hasDOMRanges) {
            var selection = this.getContentWindow().getSelection();
            line = selection.anchorNode;
        } else if (isc.Browser.isIE) {
            var selectionId = this._getNextSelectionId();
            var range = doc.selection.createRange();
            // collapse the selection range to the start of the current selection
            range.collapse();
            range.pasteHTML("<span id='"+selectionId+"'></span>");
            var selNode = doc.getElementById(selectionId);
            line = selNode.parentNode;
//            this.logWarn("startLine: " + line.outerHTML);
            line.removeChild(selNode);
        }
//        this.logWarn("anchorNode: " + Log.echo(line));
        // IE will paste the isLine spans into the current line, so we need to find the
        // top-most element that is actually a line
        var lastLine = line;
        while(line.parentNode != null) {
            if (line.getAttribute && line.getAttribute("isLine") != null) lastLine = line;
            line = line.parentNode;
        }
        return lastLine;
    },

    _getSelectionSpanHTML : function (selectionId) {
        return "<span isSelectionSpan='true' id='"+selectionId+"'></span>";
    },
    // inserts an HTML marker at the start of the current selection.  In IE, the marker will be
    // inserted immediately before the current selection.  In FF, immediately after.  FF can be
    // made to work as IE for collapsed selections, but for multichar selections it may be
    // impossible.  See the notes below for enabling IE-style behavior in FF for collapsed
    // selections.
    markCurrentSelection : function () {
        var selectionId = this._getNextSelectionId();
        var doc = this.getContentDocument();
        if (isc.Browser._hasDOMRanges) {
            // create the selection span
            var selectionNode = doc.createElement("span");
            selectionNode.setAttribute('isSelectionSpan', "true");
            selectionNode.setAttribute('id', selectionId);

            // grab the current selection range
            var selection = this.getContentWindow().getSelection();
            var range = selection.getRangeAt(0);


            if (selection.isCollapsed) {
                range.insertNode(selectionNode);
            } else {
                // create a new range whose start and end match the selection range start boundary            
                var collapsedRange = range.cloneRange();
        
                // collapse the new range to the end of the selection
                collapsedRange.collapse(false);

                // insert the selection node at the collapsed range
                collapsedRange.insertNode(selectionNode);
                collapsedRange.detach();

                /*
                // The code below will insert the selection marker immediately before the start
                // of the current selection, but it only works for a collapsed selection.  This
                // is because range.insertNode() puts the new node right after the start of the
                // selection which means that we need to jump the start of the selection over
                // the newly inserted node.  This works fine for a collapsed selection, but a
                // multicharacter selection gets destroyed by insertNode() - at least when the
                // start and end of the multichar selection are in one text node that is
                // fragmented by insertNode().  After insertNode(), the selection reflects
                // the character immediately before the previous start of the selection!

                // Weird FF bug - we need to clear the selection range and then re-add it after
                // we're done manipulating our clone selection - because if we don't it expands
                // to the parentNode for no apparent reason.
                selection.removeAllRanges();

                // create a new range whose start and end match the selection range start boundary
                var collapsedRange = doc.createRange();
                collapsedRange.setStart(range.startContainer, range.startOffset);
                collapsedRange.setEnd(range.startContainer, range.startOffset);

                // insert the selection node at the collapsed range
                collapsedRange.insertNode(selectionNode);
                collapsedRange.detach();
                

                // the above should have been all, but unfortunately the above insertion happened
                // AFTER the start of the current selection range, so now we need to move the
                // current selection range start to after the node we inserted
                    if(range.startContainer.nodeType == 3) {
                        // if it's a text node, then the above insertion fragmented the text node into
                        // a two, so just jump over the selectionNode we just inserted.
                        range.setEnd(range.startContainer.nextSibling.nextSibling, 0);
                        range.setStart(range.startContainer.nextSibling.nextSibling, 0);
                    } else {
                        range.setEndAfter(selectionNode);
                            range.setStartAfter(selectionNode);
                    }

                // add the modified range back so the cursor shows up.
                selection.addRange(range);
                */
            }
        } else if (isc.Browser.isIE) {
            var range = doc.selection.createRange();
            // collapse the selection range to the start of the current selection
            range.collapse();
            // insert our marker span
            range.pasteHTML(this._getSelectionSpanHTML(selectionId));
        }
        return selectionId;
    },

    overwriteLines : function (lineNum, numLines, newLines) {
        if (!isc.isAn.Array(newLines)) newLines = [newLines];
        var line = this.getLine(lineNum);
        while (lineNum >= 0 && (!line || !line.getAttribute || !line.getAttribute("isLine"))) {
            line = this.getLine(lineNum);
            lineNum--
        }

        if (lineNum < 0) {
//            this.logWarn("wiping lineContainer");
            this.getLineContainer().innerHTML = isc.emptyString;
            line = this.createLine();
            this.getLineContainer().appendChild(line);
            if (isc.Browser.isMoz) lineNum++;
        }        

        var container = line.parentNode;
//        this.logWarn("looking for lineNum: " + lineNum + " - got: " + isc.Log.echoAll(line));
        line.innerHTML = newLines[0];
//        this.logWarn("replaced line: " + lineNum + " with: " + newLines[0]);

        // remove the numLines to replace
//        this.logWarn("removing: " + numLines + " lines");
        while (numLines != null && numLines-- > 0) {
            var removeLine = this.getLine(lineNum+1);
            if (removeLine) {
//                this.logWarn("removing line: " + removeLine.innerHTML);
//                this.logWarn("next line is: " + (removeLine.nextSibling ? removeLine.nextSibling.innerHTML:"null"));
                container.removeChild(removeLine);
            }
        }

        // add new lines
        for (var i = 1; i < newLines.length; i++) {
            if (newLines[i] != -1) this.addLineAfter(lineNum+i-1, newLines[i]);
        }
    },

    addLineAfter : function (lineNum, line) {
//        this.logWarn("addAfter: " + lineNum + " line: " + line);
        var afterLine = this.getLine(lineNum);
        var nextLine = this.getNextLine(afterLine);
        line = this.createLine(line);
        if (nextLine) {
            nextLine.parentNode.insertBefore(line, nextLine);
        } else {
            afterLine.parentNode.appendChild(line);
        }
    },

    escapeSelection : function (str, escapeValue) {
        if (escapeValue == null) escapeValue = isc.emptyString;
        return str.replace(/<span [^>]*isSelectionSpan[^>]*><\/span>/gi, escapeValue);
//        var r = new RegExp("<span [^>]*id=\"?"+selectionId+"[^>]*><\/span>", "gi");
//        return str.replace(r, escapeValue);
    },

    getSelectionMarkerIndex : function (s) {
        var regex = new RegExp("<span [^>]*isSelectionSpan[^>]*>", "i");
        var result = regex.exec(s);
        if (result) return result.index;
        return -1;
    },

    getLineNumber : function (line) {
        var peers = line.parentNode.childNodes;
        for (var i = 0; i < peers.length; i++) 
            if (peers[i] == line) return i;
    },

    getPreviousLine : function (line) {
        return line.previousSibling;
    },

    getNextLine : function (line) {
        return line.nextSibling;
    },

    getLineContainer : function () {
        return this._useDesignMode() ? this.getContentBody() : this.getHandle();
    },

    getLine : function (lineNum) {
        return this.getLineContainer().childNodes[lineNum];
    },

    getLineHTML : function (line) {
        return line.innerHTML;
    },

    getLineContents : function (line) {
        return this.removeMarkup(this.getLineHTML(line));
    },

    removeMarkup : function (str, preserveSelectionSpan) {
        // FF actually inserts \n or \r in addition to <BR> on the ENTER key, so first remove
        // any literal newlines.   
        //
        // IE actually inserts <FONT color="foo"></FONT> tags into the contents in some cases -
        // it appears to be tracking the currently applied style, so need to remove that HTML
        // markup here.
        if (preserveSelectionSpan) {
            // remove all tag except <br> and the selectionSpan
            str = str.replace(/\n|\r|(<\/?(?!br|BR|([^>]*isSelectionSpan)).*?>)/gi, isc.emptyString);
        } else {
            // remove all tag except <br>
            str = str.replace(/\n|\r|(<\/?(?!br|BR).*?>)/gi, isc.emptyString);
        }
        
        str = str.unescapeHTML();
        
        
        if (isc.Browser.isOpera) {
            var nbsp = new RegExp(String.fromCharCode(160), "g");
            str = str.replace(nbsp, " ");
        }
        return str;
    },

    // given the selectionId of a selection span in the contents, move the selection cursor to
    // that span.  After selection is moved to the span, the selectionMarker is destroyed.
    moveSelectionToMarker : function (selectionId) {
        var doc = this.getContentDocument();
        var selectionNode = doc.getElementById(selectionId);
        if (isc.Browser._hasDOMRanges) {
            var selection = this.getContentWindow().getSelection();
            selection.removeAllRanges();
            var range = doc.createRange();

            range.setStartBefore(selectionNode);
            range.setEndBefore(selectionNode);
            selection.addRange(range);
        } else if (isc.Browser.isIE) {
            var range = doc.selection.createRange();
            range.moveToElementText(selectionNode);
            range.collapse();
            range.select();
        }
        this.destroySelectionMarker(selectionId);
    },

    // destroyes the selectionMarker in the contents
    destroySelectionMarker : function (selectionId) {
        var doc = this.getContentDocument();
        var selectionNode = doc.getElementById(selectionId);            
        if (selectionNode) selectionNode.parentNode.removeChild(selectionNode);
    },



    // ------------ Public Runtime API --------------

    //>@method  setEditable ()
    // Enable / disable editing of the rich text canvas.
    // @param   editable    (boolean)   True if we are enabling editing
    //<
    setEditable : function (editable) {
        //this.logWarn("setEditable" + editable);

        if (editable == this.editable) return;
        this.editable = editable;
        this._setHandleEditable(editable);
    },

    // Actually set the handle to be editable or not.
    _setHandleEditable : function (editable, initialPass) {

        if (this._useDesignMode()) {
            var cDoc = this.getContentDocument();
            if (cDoc != null) {
                
                if (editable || initialPass) {
                    if (isc.Browser.isIE && !isc.Browser.isIE11) cDoc.body.contentEditable = true;
                    else cDoc.designMode = "on";
                }
                // Call execCommand directly rather than using our _execCommand method as
                // we may have 'this.editable' set to false already.
                if (isc.Browser.isMoz) {
                    try {
                        cDoc.execCommand("contentReadOnly", false, !editable);
                    } catch (e) {
                        // In Firefox, attempting to set contentReadOnly to true while already
                        // read-only raises an NS_ERROR_FAILURE. Seen in Firefox 3.5.19 and 23.0.1.
                    }
                }
                if (!editable) {
                    if (isc.Browser.isIE && !isc.Browser.isIE11) cDoc.body.contentEditable = "inherit";
                    else cDoc.designMode = "off";
                }
            }
        } else {
            var handle = this.getHandle();
            if (handle != null) {
                handle.contentEditable = (editable ? true : "inherit");

                
                if (isc.Browser.isIE) {
                    if (!this.isVisible() && this._hasSelection()) 
                        this._emptySelectionForHide();
                    else if (isc.Browser.version < 6) 
                        this._rememberSelection();

                    
                    if (this._editCutPasteHandler == null) {
                        this._editCutPasteHandler = isc._makeFunction("", this.getID() + "._nativeCutPaste()");
                    }
                    if (editable) {
                        handle.oncut = this._editCutPasteHandler;
                        handle.onpaste = this._editCutPasteHandler;
                        handle.onfocusout = this._editCutPasteHandler;
                    } else {
                        handle.oncut = handle.onpaste = handle.onfocusout = null;
                    }
                    if (isc.Browser.isIE && (isc.Browser.isIE9 || isc.Browser.version >= 10)) {
                        if (editable) {
                            handle.removeEventListener("DOMNodeInserted", this._editCutPasteHandler, false);
                            handle.addEventListener("DOMNodeInserted", this._editCutPasteHandler, false);
                            handle.removeEventListener("DOMNodeRemoved", this._editCutPasteHandler, false);
                            handle.addEventListener("DOMNodeRemoved", this._editCutPasteHandler, false);
                            if (isc.Browser.isIE11) {
                                handle.removeEventListener("DOMCharacterDataModified", this._editCutPasteHandler, false);
                                handle.addEventListener("DOMCharacterDataModified", this._editCutPasteHandler, false);
                            }
                        } else {
                            handle.removeEventListener("DOMNodeRemoved", this._editCutPasteHandler, false);
                            handle.removeEventListener("DOMNodeInserted", this._editCutPasteHandler, false);
                            if (isc.Browser.isIE11) {
                                handle.removeEventListener("DOMCharacterDataModified", this._editCutPasteHandler, false);
                            }
                        }
                    }
                }
            }
        }
    },

    
    parentVisibilityChanged : function (vis) {
        if (!this._useDesignMode() && isc.Browser.isIE && (vis == isc.Canvas.HIDDEN) && 
            this._hasSelection()) 
        {
            this._emptySelectionForHide();
        }
        return this.Super("parentVisibilityChanged", arguments);
    },
    
    // Helper method for IE to ensure that our selection is empty.
    _emptySelectionForHide : function () {
        
        document.body.focus();
        var focusCanvas = isc.EH.getFocusCanvas();
        if (focusCanvas != this && focusCanvas != null) {
        
            focusCanvas.focus();
        }
    },

    // Override disableKeyboardEvents - when disabled we always want to be non-editable
    disableKeyboardEvents : function (disabled) {
        this.Super("disableKeyboardEvents", arguments);
        // If we're editable (when enabled) update the handle to be non editable when 
        // disabled (or make it editable again when enabled)
        if (this.editable) this._setHandleEditable(disabled ? false : true);
    },    

    // ---------- Contents management ---------------
    // We need APIs for the developer to both set and retrieve the HTML contained in the
    // editable area.
    
    // _rememberContents - stores the contents of the editable area under this.contents.
    // Note: getContents() should be used rather than checking this.contents directly.
    _rememberContents : function () {
        if (!this.isDrawn() || this._drawingFrame) return;
        var contents = this._getContents();
        if (contents != null) this.contents = contents;
    },

    _getContents : function () {
        var contents;
        if (this._useDesignMode()) {
            var c_body = this.getContentBody();
            if (!c_body) return;
            contents = c_body.innerHTML;
        
        } else {
            var handle = this.getHandle();
            if (handle) contents = handle.innerHTML;
        }
        return contents;
    },

    //>	@method	RichTextCanvas.getContents()    ([])
    // Returns the current HTML contents of the RichTextCanvas.
    // @return (string) (possibly edited) contents
    // @see RichTextCanvas.setContents()
    //<
    getContents : function (dontRemoveMarkup) {
        this._rememberContents();

        // if a syntaxHiliter or line counting is applied, remove line and hiliting information
        // before returning the contents.
        if ((this.syntaxHiliter || this.countLines) && !dontRemoveMarkup) {
            return this.removeMarkup(this.contents);
        } else {
            return this.contents;
        }
    },

    //>	@method	RichTextCanvas.setContents()    ([])
    //      Changes the contents of a widget to newContents, an HTML string.
    //  @param	newContents	(string)    an HTML string to be set as the contents of this widget
    //  @see RichTextCanvas.getContents()
    //<
    setContents : function (contents, force, selectionMarkerIndex, selectionMarkerHTML) {
        // setContents in effect gets called twice in FF because the iframe takes a while
        // to load, so an end user calling setContents() directly effectively ends up
        // setting this.contents which is then picked up via _setupEditArea()
        if (contents == this.contents && !force) return;

        // don't hilite if we're not drawn since in that case _setContents() would just return
        this.contents = contents;
        if (!this.isDrawn() || this._drawingFrame) return;

        this._setContents(this.hiliteAndCount(contents, selectionMarkerIndex, selectionMarkerHTML));
    },

    _setContents : function (contents) {
        this.contents = contents;

        if (!this.isDrawn()) return;

        this._settingContents = true;
        if (this._useDesignMode()) {
            var c_body = this.getContentBody();
            if (c_body != null) c_body.innerHTML = contents;

        } else {
            var handle = this.getHandle();
            if (handle != null) handle.innerHTML = contents;
        }
        this._settingContents = false;

        // contents have changed, so get updated scrollHeight, check for scrollbars, etc
        this.adjustOverflow();
    },

    hiliteAndCount : function (contents, selectionMarkerIndex, selectionMarkerHTML) {
        if (this.syntaxHiliter) {
            // if a syntaxHiliter is applied, pass the new contents through it
            contents = this.syntaxHiliter.hilite(contents, false, selectionMarkerIndex, selectionMarkerHTML);
        }
        if (this.countLines) {
            // if we're initializing an empty richTextEditor and we're going to be doing
            // realtime hiliting, we must have one of our special lines in there - otherwise
            // all related logic will break.  If the contents is blank, insert a <BR> because
            // the regex immediately below relies on it and can't be efficiently changed to
            // capture this case.
            if (contents == isc.emptyString) contents = this._$br;
            contents = contents.replace(/((?:.*?<br>)|(?:.+$))/gi, this._getLineSpanHTML());
        }
        return contents;
    },

    // adds the specified contents to the innerHTML
    appendContents : function (contents, selectionMarkerIndex, selectionMarkerHTML) {
        contents = this.hiliteAndCount(contents, selectionMarkerIndex, selectionMarkerHTML);    

        var handle = this._useDesignMode() ? this.getContentBody() : this.getHandle();
        handle.innerHTML += contents;  
         
        // contents have changed, so get updated scrollHeight, check for scrollbars, etc 
        this.adjustOverflow();
    },

    // --------------- Rich Text Editing Commands -------------------
    
    // _execCommand()   Fires the standard 'execCommand()' method to modify the editable 
    // content.
    // We currently use the native 'document.execCommand()' method to perform most of our 
    // actions on the text of the RTC, so most of our Rich Text APIs fall through to this
    // wrapper method.
    // Will return explicitly return 'false' if the command is not supported.
    
    _execCommand : function (command, valueString) {
        if (!this.isDrawn() || !this.editable) return;
        
        // We could use 'queryCommandEnabled()' here to determine whether the command is valid
        // given the current selection, etc.
        
        if (!isc.Page.isLoaded()) {
            this.logWarn("Unsupported attempt to manipulate RichTextCanvas content style " +
                         "before page load: postponed until the page has done loading.");
            isc.Page.setEvent(
                "Load", 
                this.getID() + "._execCommand('" 
                                    + command + "','" + valueString + "');"
            );
            return;
        }

        // Ensure we have focus and are selected.
        this.focus();
        
        var designMode = this._useDesignMode(),
            doc =  designMode ? this.getContentDocument() : document;
        
        if (!doc) return;
        
        // If the command is unsupported, return false to notify callers.
        if (!this._commandEnabled(command)) return false;
        
    
        
        
        try {
            doc.execCommand(command, false, valueString);
        } catch (e) {
            return false;
        }
        
        
        if (designMode) {
            // put focus into the window so the user can continue typing and have
            // an effect.
            var cw = this.getContentWindow();
            cw.focus();

        } else {
            
            // text manipulation is likely to have changed the text insertion point.
            this._rememberSelection();
            
        }
        
        // Fire _contentsChanged() - it is likely that the execCommand changed the content
        // of the RTC.
        this._contentsChanged();
    },
    
    // Helper method to test for command being enabled
    _commandEnabled : function (command)  {
		try {
            var doc = this._useDesignMode() ? this.getContentDocument() : document;
            if (!doc) return false;
            if (!doc.queryCommandEnabled(command)) return false;
		} catch (e) {
            return false;
		}
        
        return true;
    },    

    //>@method  RichTextCanvas.boldSelection
    //  Toggle whether the current text selection is bold or not bold
    //<
    boldSelection : function () {
        this._execCommand("bold")
    },
    
    //>@method  RichTextCanvas.italicSelection
    //  Toggle whether the current text selection is italic or not
    //<
    italicSelection : function () {
        this._execCommand("italic");
    },
    
    //>@method  RichTextCanvas.underlineSelection
    //  Toggle whether the current text selection is underlined or not
    //<
    underlineSelection : function () {
        this._execCommand("underline");
    },
    
    //>@method  RichTextCanvas.strikethroughSelection
    //  Toggle whether the current text selection is strikethrough or not
    //<
    strikethroughSelection : function () {
        this._execCommand("strikethrough");
    },
    
    //>@method  RichTextCanvas.showClipboardDisabledError
    //  For some browsers the clipboard functions (cut/copy/paste) are disabled by default.
    //  We catch these cases from cutSelection() / copySelection() / pasteOverSelection() and
    //  call this method to warn the user. Default behavior shows a warn dialog with the text
    // <code>"Your browser does not allow web pages to access the clipboard programmatically."</code>
    // May be overridden to change this behavior.
    // @visibility editor_clipboard
    //<
    // Cut/Copy/Paste all disabled by default in Moz.
    // In Safari Cut/Copy work, but Paste always fails.
    // In IE all three methods are enabled and work by default.
    showClipboardDisabledError : function () {

        var errorMessage = "Your browser does not allow web pages to access the clipboard programmatically.";

        // Mozilla allows you to turn on clipboard access for scripts but it's somewhat complex
        // and you can only turn it on for specific URLs
        // There's a note on this at mozilla.org: http://www.mozilla.org/editor/midasdemo/securityprefs.html
        // NOTE: the freeware HTMLArea application available at:
        //  http://www.dynarch.com/projects/htmlarea/   or
        //  http://www.postnukepro.com/modules/pagesetter/guppy/HTMLArea30beta/
        // points the user directly to this page. We currently don't because:
        //  - the steps to enable the script access are complex
        //  - the explanation specifically refers to the Moz "midas" demo app
        //  - we don't know of any way to turn on "Paste" access in Safari.
        isc.warn(errorMessage);
    },
        
    //>@method  RichTextCanvas.copySelection
    //  Copy the current text selection to the clipboard.
    // @visibility editing_clipboard
    //<
    
    copySelection : function () {
        if (this._execCommand("copy") == false) this.showClipboardDisabledError();
    },
    
    //>@method  RichTextCanvas.cutSelection
    //  Copy the current text selection to the clipboard, and remove it from the edit area.
    // @visibility editing_clipboard
    //<
    cutSelection : function () {
        if (this._execCommand("cut") == false) this.showClipboardDisabledError();;
    },

    //>@method  RichTextCanvas.pasteOverSelection
    //  Paste the current clipboard text over the selection
    // @visibility editing_clipboard
    //<
    
    pasteOverSelection : function () {
        if (this._execCommand("paste") == false) this.showClipboardDisabledError();
    },

    //>@method  RichTextCanvas.deleteSelection
    //  Delete the currently selected text
    //<
    deleteSelection : function () {
        this._execCommand("delete");
    },

    //>@method  RichTextCanvas.indentSelection
    //  Increases the indent for the currently selected paragraph.  Within a list, increases the
    //  list level.
    // @visibility external
    //<
    
    indentSelection : function () {
        this._execCommand("indent");
    },

    //>@method  RichTextCanvas.outdentSelection
    //  Decreases the indent for the currently selected paragraph.  Within a list, decreases the
    //  list level or breaks out of the list.
    // @visibility external
    //<
    outdentSelection : function () {
        this._execCommand("outdent");
    },

    //> @method RichTextCanvas.insertHTML()
    // Replaces the current selection with the given HTML.
    // @param html (HTMLString) the HTML to insert. This must be a valid HTML string.
    //<
    insertHTML : function (html) {
        // IE does not support the insertHTML command.
        if (isc.Browser.isIE) {
            var contentDoc = this.getContentDocument();

            
            if (isc.Browser.version < 11) {
                var range = contentDoc.selection.createRange(),
                    parentElem = parentElem = range.parentElement();
                range.pasteHTML(html);

                
                if (parentElem != null) {
                    isc.Element.forEachDescendantHavingClass(parentElem, "_isc_tab", isc.RichTextCanvas._fixTabSpan);
                }

            // IE 11
            
            } else {
                var sel = contentDoc.getSelection(),
                    range;
                if (sel.rangeCount <= 0) {
                    range = contentDoc.createRange();
                    var lineContainer = this.getLineContainer();
                    if (!lineContainer.lastChild) {
                        lineContainer.appendChild(contentDoc.createTextNode(""));
                    }
                    range.selectNode(lineContainer.lastChild);
                    range.collapse(false);
                } else {
                    range = sel.getRangeAt(0);
                }
                range.deleteContents();
                var docFragment = range.createContextualFragment(html);
                range.insertNode(docFragment);
                range.collapse(false);
                sel.removeAllRanges();
                sel.addRange(range);
            }

        // Use the 'insertHTML' command
        } else {
            this._execCommand("insertHTML", html);
        }
    },

    //>@method  RichTextCanvas.justifySelection
    //  Applies the alignment / justification passed in to the selected paragraph.
    //  Options are "right", "left", "center" (for alignment), and "full" for fully justified
    //  text.
    // @param justification (string)    What justification should be applied to the text.
    //<
    justifySelection : function (justification) {
        if (justification == isc.RichTextCanvas.CENTER) {
            this._execCommand("justifycenter");
            
        } else if (justification == isc.RichTextCanvas.FULL) {
            this._execCommand("justifyfull");
                    
        } else if (justification == isc.RichTextCanvas.RIGHT) {
            this._execCommand("justifyright");
                    
        } else if (justification == isc.RichTextCanvas.LEFT) {
            this._execCommand("justifyleft");
          
        
        /*
        } else {
            this._execCommand("justifynone");
        */         
        
        }
    },
    
    //>@method  RichTextCanvas.setSelectionColor
    //  Set the font color for the selected text.   Takes the desired color as a parameter.
    // @param color (string)    Color to apply to the text.
    //<
    setSelectionColor : function (color) {
        this._execCommand("forecolor", color);
    },
    
    //>@method  RichTextCanvas.setSelectionBackgroundColor
    //  Set the background color for the selected text.   Takes the desired color as a parameter.
    // @param color (string)    Color to apply to the text background.
    //<
    setSelectionBackgroundColor : function (color) {
        // In Moz "backcolor" will style the entire containing IFRAME - while 'hilitecolor'
        // will set the background color for just the selected text.
        var command = isc.Browser.isMoz ? "hilitecolor" : "backcolor";
        this._execCommand(command, color);
    },
    
    //>@method  RichTextCanvas.setSelectionFont
    //  Set the font for the selected text.   Takes the name of a font as a parameter.
    // @param font (string)    Font to apply to the selection
    //<
    setSelectionFont : function (font) {
        this._execCommand("fontname", font);
    },
    
    //>@method  RichTextCanvas.setSelectionFontSize
    //  Set the size of the font for the selected text. Takes a number between 1 and 7.
    // @param size (number)    Desired font size - a value between 1 and 7.
    //<
    setSelectionFontSize : function (size) {
        this._execCommand("fontsize", size);
    },
    
    createLink : function (url) {
        this._execCommand("CreateLink", url);
    },

    _getListElementFromSelection : function () {
        var doc = this.getContentDocument();

        if (!isc.Browser._hasDOMRanges) {
            var selElement = null;
            if (this._savedSelection) {
                selElement = this._savedSelection.parentElement();
            } else {
                selElement = isc.Element._getElementFromSelection(doc);
            }
            for (var elem = selElement; elem != null; elem = elem.parentNode) {
                if (elem.nodeType != 1) continue;
                if (elem.tagName === "OL" || elem.tagName === "UL") {
                    return elem;
                }
            }

        } else {
            // Find the lowest common ancestor <ol> or <ul> element. First traverse the ancestors
            // of the current selection's anchorNode to build a list of ancestor <ol> or <ul> elements.
            // Then traverse the ancestors of the current selection's focusNode looking for one
            // of these <ol> or <ul> elements.
            var selectionAnchorNode,
                selectionFocusNode;

            if (this._useDesignMode() || this._savedSelection == null) {
                var selection = doc.defaultView.getSelection();
                selectionAnchorNode = selection.anchorNode;
                selectionFocusNode = selection.focusNode;

            // When using contentEditable, we need to use the last-saved selection's anchorNode
            // and focusNode because when this method is called in response to clicking on the
            // List Properties editor control button, the selection has already moved to the
            // now-focused button.
            } else {
                selectionAnchorNode = this._savedSelectionAnchorNode;
                selectionFocusNode = this._savedSelectionFocusNode;
            }

            var anchorNodeListAncestors = [];

            var elem = selectionAnchorNode;
            for (; elem != null; elem = elem.parentNode) {
                if (elem.nodeType != 1) continue;
                if (elem.tagName === "OL" || elem.tagName === "UL") {
                    anchorNodeListAncestors.add(elem);
                }
            }

            elem = selectionFocusNode;
            for (; elem != null; elem = elem.parentNode) {
                if (elem.nodeType != 1) continue;
                if (elem.tagName === "OL" || elem.tagName === "UL") {
                    if (anchorNodeListAncestors.contains(elem)) {
                        return elem;
                    }
                }
            }
        }

        return null;
    },

    //> @method richTextCanvas.convertSelectionToOrderedList()
    // Converts the selection to an ordered list. If the selection is within an unordered list,
    // the unordered list is converted to an ordered list.
    // @visibility external
    //<
    convertSelectionToOrderedList : function () {
        var listElem = this._getListElementFromSelection();
        if (listElem != null) {
            
            var listProperties = this.getListProperties(listElem),
                isUnordered = isc.ListPropertiesPane.getListType(listProperties) == "unordered";
            if (isUnordered) {
                this.applyListProperties({
                    style: "decimal"
                }, listElem);

            // Already an ordered list. Ignore.
            } else {
                /*empty*/
            }
        } else {
            this._execCommand("insertOrderedList");
        }
    },

    //> @method richTextCanvas.convertSelectionToUnorderedList()
    // Converts the selection to an unordered list. If the selection is within an ordered list,
    // the ordered list is converted to an unordered list.
    // @visibility external
    //<
    convertSelectionToUnorderedList : function () {
        var listElem = this._getListElementFromSelection();
        if (listElem != null) {
            var listProperties = this.getListProperties(listElem),
                isUnordered = isc.ListPropertiesPane.getListType(listProperties) == "unordered";
            if (!isUnordered) {
                this.applyListProperties({
                    style: "disc"
                }, listElem);

            // Already an unordered list. Ignore.
            } else {
                /*empty*/
            }
        } else {
            this._execCommand("insertUnorderedList");
        }
    },

    //> @method richTextCanvas.getListProperties()
    // @return (ListProperties) the list configuration properties of the currently selected list,
    // or null if no list is selected.
    //<
    getListProperties : function (listElem) {
        listElem = listElem || this._getListElementFromSelection();
        var returnVal = null;
        if (listElem != null) {
            var cs = isc.Element.getComputedStyle(listElem, {
                listStyleType: "list-style-type",
                listStyleImage: "list-style-image"
            });
            var style = cs.listStyleType,
                image = cs.listStyleImage;
            if (image && image != "none") {
                style = "custom-image";
                // Trim off the 'url(' prefix and ')' suffix.
                image = image.substring(4, image.length - 1);
                // In IE and Firefox, need to also trim double quotes around the URL.
                
                if (image.length >= 2 && image[0] == '"') {
                    image = image.substring(1, image.length - 1);
                }
            }
            returnVal = {
                style: style,
                image: image
            };
            if (listElem.start != "" &&
                listElem.start != null)
            {
                returnVal.startNumber = listElem.start << 0;
            }
        }

        return returnVal;
    },

    //> @method richTextCanvas.applyListProperties() [A]
    // Applies the given +link{ListProperties} to the currently selected HTML list, if any.
    // If there is no list selected, then this method does nothing.
    // @param listProperties (ListProperties) the list configuration to apply
    // @visibility external
    //<
    applyListProperties : function (listProperties, listElem) {
        listElem = listElem || this._getListElementFromSelection();
        if (listElem != null) {
            listProperties = isc.ListPropertiesPane.getCanonicalListProperties(listProperties);

            var isUnordered = isc.ListPropertiesPane.getListType(listProperties) == "unordered";

            // Do we need to change a <ul> to <ol> or vice versa?
            
            if ((isUnordered && listElem.tagName != "UL") ||
                (!isUnordered && listElem.tagName != "OL"))
            {
                var doc = listElem.ownerDocument,
                    newListElem = doc.createElement(isUnordered ? "UL" : "OL");
                // Transfer all children of the current list element to the new list element.
                var child;
                while ((child = listElem.firstChild) != null) {
                    newListElem.appendChild(child);
                }

                // Copy attributes (except possibly `start').
                if (listElem.attributes) {
                    for (var i = 0, len = listElem.attributes.length; i < len; ++i) {
                        var attr = listElem.attributes[i];
                        if (!isUnordered || attr.name != "start") {
                            newListElem.setAttributeNode(attr.cloneNode());
                        }
                    }
                }

                listElem.parentNode.replaceChild(newListElem, listElem);
                listElem = newListElem;
            }

            listElem.removeAttribute("start");

            if (isUnordered) {
                if (listProperties.style == "custom-image") {
                    listElem.style.listStyle = "url(\"" + this.getImgURL(listProperties.image) + "\")";
                } else {
                    listElem.style.listStyle = listProperties.style;
                }
            } else {
                listElem.style.listStyle = listProperties.style;
                if (listProperties.startNumber != null) {
                    listElem.start = listProperties.startNumber << 0;
                }
            }
        }
    }

    
});

isc.RichTextCanvas.registerStringMethods({
    changed : "oldValue,newValue"
});
//!<Deferred
