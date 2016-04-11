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
//>	@class	RichTextItem
// FormItem for rich text (HTML) editing. Makes use of a +link{RichTextEditor} as the 
// editing interface.
// @visibility external
//<

isc.ClassFactory.defineClass("RichTextItem", isc.CanvasItem);

isc.RichTextItem.addProperties({

    // Override canFocus to allow focus to go to the RichTextEditor
    canFocus:true,

    //> @attr RichTextItem.moveFocusOnTab (boolean : true : IRW)
    // @include RichTextCanvas.moveFocusOnTab
    // @setter setMoveFocusOnTab()
    // @visibility external
    //<
    moveFocusOnTab: true,

    //> @attr richTextItem.shouldSaveValue (Boolean : true : IR)
    // @include FormItem.shouldSaveValue
    //<
    shouldSaveValue:true,

    //> @attr RichTextItem.showTitle (Boolean : false : IR)
    // Don't show the title for rich text items by default
    // @visibility external
    //<
    showTitle:false,
    
    //>@attr RichTextItem.startRow   (Boolean : true : IRW)
    // By default RichTextItems take up an entire row
    // @visibility external
    //<
    startRow:true,
    
    //>@attr RichTextItem.endRow (Boolean : true : IRW)
    // By default RichTextItems take up an entire row
    // @visibility external
    //<
    endRow:true,
    
    //>@attr RichTextItem.colSpan (number | string : "*": IRW)
    // By default RichTextItems take up an entire row
    // @visibility external
    //<
    colSpan:"*",

    // Realistically rich text editors take up a lot of space because of their toolbars.
    width:550,
    
    
    //> @attr RichTextItem.controlGroups (Array of String : null : IA)
    // +link{RichTextEditor.controlGroups} to display for this editor.
    // Each controlGroup should be a property set either on this item or on the RichTextEditor
    // prototype and should be set to an array of +link{type:ControlName}s.
    // @visibility external
    //<
    // For each named control specified, you can override [controlName]Properties to apply
    // specific properties, [controlName]Constructor to supply a class for the control, and
    // [controlName]_autoMaker to supply a function taht actually creates (and returns) the 
    // control.
    //controlGroups : null

    //>@attr RichTextItem.defaultControlConstructor (Array : null : IA)
    // If set, this property will override +link{RichTextEditor.defaultControlConstructor} for
    // this item's RichTextEditor
    // @visibility internal
    //<
    //defaultControlConstructor : null

    canvasConstructor: "RichTextEditor",
    canvasDefaults: {
        getBrowserSpellCheck : function() {
            return this.canvasItem.getBrowserSpellCheck();
        },
        valueChanged : function (oldValue, newValue) {
            this.canvasItem.storeValue(newValue);
        },
        editAreaFocusChanged : function () {
            this.canvasItem.editAreaFocusChanged();
        }
    }

    //>@attr RichTextItem.browserSpellCheck (boolean : null : IRWA)
    // @include FormItem.browserSpellCheck
    // @visibility internal
    //<

});

isc.RichTextItem.addMethods({
    init : function () {
        if (this.value && isc.isA.String(this.value)) {
            this.value = this.value.replaceAll("<BR>", "<br>").replaceAll("<P>", "<p>");
        }

        this.Super("init", arguments);
    },

    // Override _createCanvas to set up a RichTextEditor as this item's canvas
    _createCanvas : function () {
        this._creatingCanvas = true;

        var value = this.getValue();
        // Map "undefined" (etc.) to an empty string
        value = this.mapValueToDisplay(value);

        var properties = {
            ID: this.getID() + "_editor",
            value: value,
            moveFocusOnTab: this.moveFocusOnTab
        };

        var cgs = this.controlGroups;
        if (cgs != null) {
            var propsSuffix = "Properties",
                makerSuffix = "_autoMaker",
                constructorSuffix = "Constructor";

            properties.controlGroups = cgs;
            for (var i = 0; i < cgs.length; i++) {
                if (this[cgs[i]]) {
                    var groupName = cgs[i],
                        group = this[groupName];
                    
                    properties[groupName] = group;

                    // To allow full customization we need to be able to apply properties / 
                    // custom maker functions to each control.
                    for (var ii = 0; ii < group.length; ii++) {
                        var propName = group[ii] + propsSuffix,
                            makerName = group[ii] + makerSuffix,
                            constructorName = group[ii] + constructorSuffix;

                        if (this[propName]) properties[propName] = this[propName];
                        if (this[makerName]) properties[makerName] = this[makerName];
                        if (this[constructorName]) 
                            properties[constructorName] = this[constructorName];
                    }
                }
            }
        }

        if (this.defaultControlConstructor != null) {
            properties.defaultControlConstructor = this.defaultControlConstructor;
        }

        this.canvas = properties;
        this.Super("_createCanvas", arguments);
        delete this._creatingCanvas;
    },

    //> @method RichTextItem.setMoveFocusOnTab()
    // Setter for +link{moveFocusOnTab}.
    // @param moveFocusOnTab (boolean) new value for moveFocusOnTab
    // @visibility external
    //<
    setMoveFocusOnTab : function (moveFocusOnTab) {
        this.moveFocusOnTab = moveFocusOnTab;
        if (this.canvas) this.canvas.setMoveFocusOnTab(moveFocusOnTab);
    },

    // Fire focus/blur on focusChanged on our edit area rather than on the RTE as a whole.
    editAreaFocusChanged : function () {
        this.hasFocus = this.canvas.editArea.hasFocus;
        if (this.hasFocus) {
            this.elementFocus();
            this.form.setFocusItem(this);
        } else {
            this.elementBlur();
        }
    },

    // Override mapValueToDisplay to show null/undefined as ""
    
	mapValueToDisplay : function (internalValue) {
        var value = isc.FormItem._instancePrototype.mapValueToDisplay.call(this, internalValue);
        // always display the empty string for null values, rather than "null" or "undefined"
        if (value == null) return isc.emptyString;
        return value;
	},

    showValue : function (displayValue, dataValue, form, item) {
        if (!this.canvas) return;
        this.canvas.setValue(displayValue);
    }
});
