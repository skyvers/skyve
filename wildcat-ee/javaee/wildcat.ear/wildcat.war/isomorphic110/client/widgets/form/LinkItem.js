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
//> @class LinkItem
// A form item that displays a URL. In the default read-only mode (+link{FormItem.canEdit,canEdit}
// is <code>false</code>) the URL is shown as a link; in editable mode the URL is shown in a textbox.
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
    clipValue: false,

    // Default the iconVAlign to "center" because the height of the link is probably less than
    // the form item icons' height, and the link should be vertically centered with the icons
    // by default.
    //> @attr linkItem.iconVAlign (VerticalAlignment : "center" : IRWA)
    // @include FormItem.iconVAlign
    //<
    iconVAlign: "center",

    // Don't set a height on LinkItems by default
    
    height:null,

    // default to canEdit: false - an editable link is a possibility, but it doesn't seem 
    // that most users would want/expect this to be the default state
    canEdit: false,

    // apply "static type format" to LinkItems in read-only mode
    shouldApplyStaticTypeFormat : function () {
        return !this.canEdit;
    }
    //,

    //> @attr linkItem.readOnlyDisplay
    // If +link{FormItem.canEdit,canEdit} is set to <code>false</code>, how should this
    // <code>LinkItem</code> be displayed to the user?
    // <p>
    // Link items are, by default, canEdit:false. Note that the link remains active regardless
    // of the <code>readOnlyDisplay</code> setting.
    // @include FormItem.readOnlyDisplay
    //<

    // For LinkItem, "disabled" is the only readOnly mode
    //readOnlyTextBoxStyle:"staticTextItem"

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
    // Optional title HTML to display for this item's link. If unset, the <code>LinkItem</code>'s
    // value (the URL) will be used for the link's title.
    // @setter setLinkTitle()
    // @visibility external
    //<

});

isc.LinkItem.addMethods({

    // Even though we don't have a data element, we don't need a focus proxy - <a..> will
    // receive focus in all browsers
    _writeOutFocusProxy : function () {
        return (this.isReadOnly() ? false
                                  : this.Super("_writeOutFocusProxy", arguments));
    },

    _getLinkElement : function () {
        if (!this.isReadOnly()) return this.Super("_getLinkElement", arguments);
        if (!this.isDrawn()) return null;
        return (isc.Element.get(this._getDOMID(this._$link)));
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
                          this.renderAsDisabled()
        );
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
        this._retrievingInactiveHTML = true;
        var linkHTML = this.getLinkHTML(value);

        var template = this._$textBoxTemplate;
        template[1] = this._getTextBoxID();
        template[3] = this.getID();

        template[6] = this.getReadOnlyTextBoxStyle();
        template[8] = this.getTextBoxCSS(value);

        template[10] = linkHTML;

        delete this._retrievingInactiveHTML;
        return template.join(isc.emptyString);
    },
    updateDisabled : function () {
        this.Super("updateDisabled", arguments);
        this.redraw();
    },

    _$linkHTMLExtraStuffTemplate: [
        " onclick='if(window.",            // [0]
        null,                              // [1] this.getID()
        ") return ",                       // [2]
        null,                              // [3] this.getID()
        null,                              // [4] _$invokeInactiveLinkClickedCode / _$invokeLinkClickedCode
        null,                              // [5] _$inactiveElseCode / null
        isc.DynamicForm._itemPart,         // [6]
        "='",                              // [7]
        isc.DynamicForm._textBoxString,    // [8]
        "'",                               // [9]
        null                               // [10] '-webkit-touch-callout' if Browser.isTouch
    ],
    _$javascript: "javascript",
    _$javascriptVoid: "javascript:void",
    _$invokeInactiveLinkClickedCode: "._inactiveLinkClicked(event); ",
    _$inactiveElseCode: "else {if (event.preventDefault != null) event.preventDefault(); return false;}' ",
    _$invokeLinkClickedCode: "._linkClicked(event);' ",
    _$link: "link",
    getLinkHTML : function (text) {
        var valueIconHTML = this._getValueIconHTML(this._value);
        if (this.showValueIconOnly) return valueIconHTML;

        // convert to String
        if (text != null) text = isc.iscToLocaleString(text);
        if (text == null) text = isc.emptyString;

        var title = this.linkTitle;
        if (title == null) title = text;

        // Convert to actual link
        var target = this.target,
            targetIsJavaScript = (target === this._$javascript);
        if (targetIsJavaScript) {
            text = this._$javascriptVoid;
        }
        
        var extraStuffTemplate = this._$linkHTMLExtraStuffTemplate;
        extraStuffTemplate[3] = extraStuffTemplate[1] = this.getID()
        if (this.isInactiveHTML()) {
            extraStuffTemplate[4] = this._$invokeInactiveLinkClickedCode;
            extraStuffTemplate[5] = this._$inactiveElseCode;
        } else {
            extraStuffTemplate[4] = this._$invokeLinkClickedCode;
            extraStuffTemplate[5] = null;
        }

        if (isc.Browser.isMobileWebkit) {
            // Don't allow the link to be bookmarked or opened in a new tab if target is "javascript".
            if (targetIsJavaScript) {
                extraStuffTemplate[10] = " style='-webkit-touch-callout:none'";

            // Otherwise, use the default '-webkit-touch-callout' (overrides any inherited value).
            } else {
                extraStuffTemplate[10] = " style='-webkit-touch-callout:default'";
            }
        } else {
            extraStuffTemplate[10] = null;
        }

        var extraStuff = extraStuffTemplate.join(isc.emptyString);

        if (!this.renderAsDisabled()) {
            text = isc.Canvas.linkHTML(text, title, target,
                                       this._getDOMID(this._$link),
                                       this.getGlobalTabIndex(),
                                       this.accessKey,
                                       extraStuff);
        } else {
            text = "<span style='text-decoration: underline;'>" + title + "</span>";
        }
        if (valueIconHTML != null) {
            text = valueIconHTML + text;
        }

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
    // Setter for +link{attr:linkTitle}.
    // @param title (HTMLString) new <code>linkTitle</code> HTML.
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
    },

    _canFocusInTextBox : function () {
        // when rendering as disabled, no actual link is rendered - instead, some styled 
        // text is written out, and there's no focusable element - return false in this
        // case to prevent some warnings from _applyHandlersToElement() later.
        if (this.renderAsDisabled()) return false;
        return this.Super("_canFocusInTextBox", arguments);
     }
 
});
