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
//>	@class	CycleItem

//
// Form item that iterates through a set of options in response to the user clicking.
//<
// Leave internal for now - we will expose synthetic checkbox and radioItem subclasses instead
isc.defineClass("CycleItem", "FormItem");

isc.CycleItem.addProperties({
    
    canSelectText:false,
    
    canFocus:true,
    
    // V-Align the cycleItem (checkboxes, etc) in the center - this looks best for the (default)
    // case where the height of the checkboxItem exceeds the height of the media
    iconVAlign:"middle"
});

isc.CycleItem.addMethods({
    // On Space keypress, advance the value
    handleKeyPress : function () {
        var key = isc.EH.getKey();
        var readOnly = this.isReadOnly();
        if (!readOnly && key == "Space") {
            this.advanceValue();
            // returning false kills native behavior (such as scrolling on space)
            return false;
        }
        return this.Super("handleKeyPress", arguments);
    },
    
    // on click, advance the value
    handleClick : function () {
        if (this.isDisabled() || this.isReadOnly()) return;
        
        if (!this.hasFocus) this.focusInItem();
        this.advanceValue();
    },
    
    // treat doubleClick like click so 2 close together clicks will flip the value twice
    handleDoubleClick : function () {
        if (this.isDisabled() || this.isReadOnly()) return;
        this.advanceValue();
    },

    // This method will advance the value - selects the next option from this item's valueMap
    advanceValue : function () {
        var valueMap = this.getValueMap(),
            valueMapIsArray = isc.isAn.Array(valueMap),
            values;
        
        if (valueMap == null ||
            (values = (valueMapIsArray ? valueMap : isc.getKeys(valueMap))).length < 2)
        {
            this.logInfo("CycleItem is non interactive as there are no options for this item.");
            return;
        }

        var value = this.getValue(),
            index = values.indexOf(value);

        // It may be that the value is not strictly mapped by the valueMap. For example, if the
        // valueMap is { "true": true, "false": false } and the value is a boolean `false', we
        // will get index = -1 because `["true", "false"].indexOf(false)' is -1 as `"false" == false'
        // is false in JavaScript.
        //
        // If index < 0, check to see which value gets mapped to the same display value by the
        // valueMap.
        if (index < 0) {
            var undef;
            if (value === undef) value = false;

            // For exactly two values, check which value gets mapped to the same value-as-boolean,
            // as long as the two values in the valueMap *as booleans* are distinct (one is true
            // and the other is false).
            if (values.length == 2 &&
                !!values[0] != !!values[1])
            {
                if (valueMapIsArray) {
                    index = (!!value == !!valueMap[0] ? 0 : 1);
                } else {
                    index = (!!valueMap[value] == !!valueMap[values[0]] ? 0 : 1);
                }
            } else {
                if (valueMapIsArray) {
                    for (index = 0; index < values.length; ++index) {
                        if (value == valueMap[index]) {
                            break;
                        }
                    }
                } else {
                    for (index = 0; index < values.length; ++index) {
                        if (valueMap[value] == valueMap[values[index]]) {
                            break;
                        }
                    }
                }
                index = Math.min(index, values.length - 1);
            }

            value = values[index];
        }
        var newIndex = (index + 1) % values.length;

        // Call saveValue rather than setValue() so our change etc handler fires
        var newValue = values[newIndex];
        if (!this.compareValues(newValue, this._value)) {
            var displayValue = this.mapValueToDisplay(newValue);
            this.setElementValue(displayValue, newValue);
            if (isc.Canvas.ariaEnabled()) this.setAriaState("checked", !!(valueMapIsArray ? newValue : valueMap[newValue]));
            // use "updateValue" to actually save the value out. This should handle
            // firing change() / changed() handlers, validation, etc.
            this._updateValue(newValue);
        }
    },

    //> @method cycleItem.setElementReadOnly()
    // Change the read-only state of the form element immediately.
    //<
    setElementReadOnly : function (readOnly) {
        // A cycle button should be rendered as disabled to simulate read-only.
        this._setElementEnabled(!readOnly && !this.isDisabled());
    }
});
