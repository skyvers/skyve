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

type: "float"
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
                } else if (this._editFormatter != null) {
                    return this._editFormatter(floatValue, this);
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
        this.Super("handleEditorEnter", arguments);

        this._inEditorMode = true;
        var value = this.getValue(),
            displayValue = this.mapValueToDisplay(value);
        this.setElementValue(displayValue, value);
    },

    handleEditorExit : function () {
        this.Super("handleEditorExit", arguments);

        this._inEditorMode = false;
        var value = this.getValue(),
            displayValue = this.mapValueToDisplay(value);
        this.setElementValue(displayValue, value);
    }
});

