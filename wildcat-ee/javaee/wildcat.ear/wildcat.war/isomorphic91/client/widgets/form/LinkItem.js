/*
 * Isomorphic SmartClient
 * Version v9.1p_2014-03-26 (2014-03-26)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */

 






//> @class LinkItem
// A FormItem that displays an HTML URL. In read-only mode (canEdit:false) the URL is shown
// as a link; in editable mode the URL is shown in a textbox.
// <P>
// The link to open is specified as the item value with +link{formItem.setValue} or 
// +link{formItem.defaultValue}. The link title defaults to the URL unless
// +link{linkItem.linkTitle} is specified.
// <P>
// Additionally, a custom action can be triggered when the link is clicked: see
// +link{linkItem.target} for details.
//
// @visibility external
//<
isc.ClassFactory.defineClass("LinkItem", "TextItem");
isc.LinkItem.addProperties({

    //> @attr linkItem.disableIconsOnReadOnly (Boolean : false : IRW)
    // @include formItem.disableIconsOnReadOnly
    // @group formIcons
    // @visibility external
    //<
    disableIconsOnReadOnly:false,

    wrap: false,
    
    // Don't set a height on LinkItems by default
    
    height:null,
    
    // default to canEdit: false - an editable link is a possibility, but it doesn't seem 
    // that most users would want/expect this to be the default state
    canEdit: false,
    
    // apply "static type format" to LinkItems in read-only mode
    shouldApplyStaticTypeFormat : function () {
        return !this.canEdit;
    },
    
    //> @attr linkItem.readOnlyDisplay (ReadOnlyDisplayAppearance : "readOnly" : IRW)
    // If +link{formItem.canEdit} is set to <code>false</code>, how should this item
    // be displayed to the user?
    // <P>
    // LinkItems are, by default, canEdit: false and only <code>disabled</code> appearance has
    // an effect - for other appearances, the item remains active.
    //
    // @visibility external
    //<
    
    // In read-only mode, we use the static textboxstyle
    readOnlyTextBoxStyle:"staticTextItem"

    //> @attr linkItem.target (string : "_blank" : IRW)
    // By default, clicking a link rendered by this item opens it in a new browser window.  You 
    // can alter this behavior by setting this property.  The value of this property will be 
    // passed as the value to the <code>target</code> attribute of the anchor tag used to render 
    // the link.
    // <P>
    // If you set linkItem.target to "javascript", the default behaviour is to catch and consume
    // mouse-clicks that would result in the link being followed.  Instead, the
    // +link{formItem.click()} event is fired.
    // 
    // @visibility external
    //<

    //> @attr linkItem.linkTitle (HTMLString : null : IRW)
    // Optional title HTML to display for this item's link. If unspecified the value of the item
    // will be the title text as well as the target of the link.
    // @setter setLinkTitle()
    // @visibility external
    //<

});

isc.LinkItem.addMethods({

    // Even though we don't have a data element, we don't need a focus proxy - <a..> will
    // recieve focus in all browsers
    _writeOutFocusProxy : function () {
        return (this.isReadOnly() ? false
                                  : this.Super("_writeOutFocusProxy", arguments));
    },
    
    _getLinkElement : function () {
        if (!this.isReadOnly()) return this.Super("_getLinkElement", arguments);
        if (!this.isDrawn()) return null;
        return (isc.Element.get(this.getID() + "_link"));
    },
    
    // Apply focus/blur handlers to the link itself
    getFocusElement : function () {
        return (this.isReadOnly() ? this._getLinkElement()
                                  : this.Super("getFocusElement", arguments));
    },

    hasDataElement : function () {
        return !this.isReadOnly();
    },
    
    _canFocus : function () {
        // In read-only mode we still want to be focusable
        return (this.isReadOnly() ? true : this.Super("_canFocus", arguments));
    },
    
    getTextBoxStyle : function () {
        if (!this.isReadOnly()) return this.Super("getTextBoxStyle", arguments);

        if (this._isPrinting() && this.printTextBoxStyle) {
            return this._getCellStyle(this.printTextBoxStyle);
        }
        return (this.readOnlyTextBoxStyle ? this._getCellStyle(this.readOnlyTextBoxStyle)
                                          : null);
    },

    // modify the text box template slightly - we're writing out a text box but it doesn't
    // need to be focusable
    _$textBoxTemplate:[ "<DIV ID='", // 0
                        ,            // 1: ID for text box
                        "' " + isc.DynamicForm._containsItem + "='", // 2
                        ,            // 3 [formItem ID]
                        , // 4
                        
                        "' CLASS='", // 5
                        ,            // 6: this.getTextBoxStyle(),
                        "' STYLE='", // 7
                        ,            // 8: this.getTextBoxCSS(), 
                        "'>",        // 9
                        ,            // 10: actual value
                        "</DIV>"
    ],

    // If a linkItem was written out as 'inactive' (EG part of print view) - how should we
    // handle this.
    // Default behavior is to suppress the navigation to href
    // Can be toggled to allow the navigation via undocumented flag "inactiveEditorLinkDisabled"
    inactiveEditorLinkDisabled:true,
    _inactiveLinkClicked : function (event) {
        if (!this.inactiveEditorLinkDisabled) {
            return this._linkClicked(event);
        }
        // Standard 'suppress native behavior' logic.
        if (!isc.Browser.isIE) {
            event.preventDefault();
        }
        return false;
    },

    _linkClicked : function (event) {
        // don't allow the click if the cell should not be interactive.
        var mustCancel = (this.destroyed || !this.isDrawn() || !this.isVisible() ||
                          this.isDisabled());
        // If a clickMask is up and the item is masked, cancel the event.
        // Check both the containerWidget and the form. If they differ and  either is unmasked
        // the item is not considered masked.
        if (!mustCancel) {
            mustCancel = isc.EH.targetIsMasked(this.containerWidget);
            if (mustCancel && (this.form != this.containerWidget)) {
                mustCancel = isc.EH.targetIsMasked(this.form);
            }
        }
        if (!mustCancel && this.target == "javascript") {
            mustCancel=true;
            this.handleClick();
        }

        if (mustCancel) {            
            
            if (!isc.Browser.isIE) {
                event.preventDefault();
            }
            return false;
        }

        return true;
    },
    getReadOnlyHTML : function (value) {
        var linkHTML = this.getLinkHTML(value);
        
        var template = this._$textBoxTemplate;
        template[1] = this._getTextBoxID();
        template[3] = this.getID();
        
        template[6] = this.getTextBoxStyle();
        template[8] = this.getTextBoxCSS();
        
        template[10] = linkHTML;
        
        return template.join(isc.emptyString);
    },
    
    getLinkHTML : function (text) {
        var valueIconHTML = this._getValueIconHTML(this._value);
        if (this.showValueIconOnly) return valueIconHTML;

        // convert to String
        if (text != null) text = isc.iscToLocaleString(text);
        if (text == null) text = isc.emptyString;

        // Convert to actual link
        var target = this.target;
        if (target == "javascript") {
            text = "javascript:void";
        }

        var onclickString = [
            " onclick='if(window.",
            this.getID(),
            ") return ",this.getID()
        ];
        if (this.isInactiveHTML()) {
            onclickString.add(
                "._inactiveLinkClicked(event); " +
                "else {if (event.preventDefault != null) event.preventDefault(); return false;}' "
            );
        } else onclickString.add( "._linkClicked(event);' " );
        onclickString = onclickString.join("");

        text = isc.Canvas.linkHTML(text, this.linkTitle, target,
                                    (this.getID() + "_link"), this.getGlobalTabIndex(), 
                                    this.accessKey,
                                    onclickString,
                                    isc.DynamicForm._itemPart + "='" 
                                        + isc.DynamicForm._textBoxString + "'"
                );
        if (valueIconHTML != null) text = valueIconHTML + text;

        return text;
    },
    
    // Override setElementValue to update the text box with the correct value
    setElementValue : function (value) {
        if (!this.isDrawn()) return;
        if (!this.isReadOnly()) return this.Super("setElementValue", arguments);
        
        
        if (this.hasFocus) this.blurItem();

        var textBox = this._getTextBoxElement();
        if (textBox) {
            textBox.innerHTML = this.getLinkHTML(value);
            // Re apply the event handlers
            this._applyHandlersToElement();
        }
    },
    
    //> @method linkItem.setLinkTitle()
    // Method to set the linkTitle for this item
    // @param title (string) new linkTitle for this item
    // @visibility external
    //<
    setLinkTitle : function (title) {
        this.linkTitle = title;
        this.redraw();
    },

    //> @method linkItem.setElementReadOnly()
    // Change the read-only state of the form element immediately.
    //<
    setElementReadOnly : function (readOnly) {
        // TextItem already handles this by marking item as readonly or not.
        // LinkItem needs to render two different items completely so we override
        // here and force a redraw on ourselves.
        this.redraw();
    }

});
