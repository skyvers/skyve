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
//> @class FloatItem
// <p>
// A TextItem for managing a text field that displays a floating point value.  FloatItem is the
// default FormItem if the +link{formItem.type} is "float".
// <p>
// FloatItem displays its value according to the +link{formItem.decimalPrecision} and
// +link{formItem.decimalPad} properties of the FormItem.  While the value is being edited,
// the item will display the value with its original precision and without extra zero-padding.
// </p>
//
// @group gwtFloatVsDouble
// @visibility external
//<
isc.ClassFactory.defineClass("FloatItem", "TextItem");

isc.FloatItem.addProperties({
// A boolean flag to store whether the item is currently displaying an editor.  This is
// used in the override of mapValueToDisplay() to display the full floating-point value while
// in "editor mode" and a formatted version of that value (where the format is specified by
// decimalPrecision and decimalPad) outside of "editor mode".
_inEditorMode: false,

defaultType: "float",


_forceValidateOnExit: true

});

isc.FloatItem.addMethods({

    // Note: similar code appears in StaticTextItem
    mapValueToDisplay : function (value) {
        if (!this._inEditorMode) {
            var floatValue = null;
            if (isc.isA.String(value) && (this.type == null || !this.type.startsWith("locale"))) {
                var parsedValue = window.parseFloat(value);
                if (!window.isNaN(parsedValue) && parsedValue == value) {
                    floatValue = parsedValue;
                }
            } else if (isc.isA.Number(value)) {
                floatValue = value;
            }
            if (floatValue != null) {
                if (this.format) {
                    return isc.NumberUtil.format(floatValue, this.format);
                
                } else if (this._simpleType != null && this._simpleType.editFormatter != null) {
            
                    var form = this.form,
                        record = this.form ? this.form.values : {};
                    return this._simpleType.editFormatter(value, this, form, record);

                } else if (this.decimalPrecision != null || this.decimalPad != null) {
                    return isc.Canvas.getFloatValueAsString(floatValue,
                        this.decimalPrecision, this.decimalPad);
                } else if (this.precision != null) {
                    return isc.Canvas.getNumberValueAsString(floatValue, 
                        this.precision, "float");
                }
            }
        }
        return this.Super("mapValueToDisplay", arguments);
    },

    handleEditorEnter : function () {
        this._inEditorMode = true;
        var value = this.getValue(),
            displayValue = this.mapValueToDisplay(value);
        var currentValue = this.getEnteredValue();
        if (currentValue != displayValue) {
            var currentSelection = this.getSelectionRange(), newSelection;
            
            // Attempt to reset selection, if it makes sense
            // (current selection is at start, end or spanning the value)
            
            if (currentValue.length == displayValue.length ||
                (currentSelection[0] == 0 && currentSelection[1] == 0)) 
            {
                newSelection = currentSelection;
            } else if (currentSelection[1] == currentValue.length) {
                if (currentSelection[0] == 0) {
                    newSelection = [0,displayValue.length];
                } else if (currentSelection[0] == currentValue.length) {
                    newSelection = [displayValue.length,displayValue.length];
                }
            }
        this.setElementValue(displayValue, value);
            if (newSelection != null) {
                this.setSelectionRange(newSelection[0],newSelection[1]);
            }
        }
        this.Super("handleEditorEnter", arguments);

    },

    handleEditorExit : function () {
        this.Super("handleEditorExit", arguments);

        this._inEditorMode = false;
        var value = this.getValue(),
            displayValue = this.mapValueToDisplay(value);
        this.setElementValue(displayValue, value);
    },

    //> @method floatItem.getValueAsFloat()
    // Return the value tracked by this form item as a Float.  If the value cannot
    // be parsed to a valid float, null will be returned.
    //
    // @return (Float) value of this element
    //
    // @see method:FormItem.getValue
    // @visibility external
    //<
    
    getValueAsFloat : function () {
        var origValue   = this.getValue(),
            parsedValue = parseFloat(origValue);
        return isNaN(parsedValue) ? null : parsedValue;
    }
});

