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
    // On Space or Enter keypress, advance the value
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
    
        var valueMap = this.getValueMap();
        if (isc.isA.Object(valueMap) && !isc.isA.Array(valueMap)) {
            valueMap = isc.getKeys(valueMap);
        }
        if (valueMap == null || valueMap.length < 2) {
            this.logInfo("CycleItem is non interactive as there are no options for this item.");
            return;
        }
        
        var value = this.getValue(),
            index = valueMap.indexOf(value);
        if (index == valueMap.length -1) index = -1;
        
        // Call saveValue rather than setValue() so our change etc handler fires
        var newValue = valueMap[index+1];
        
        if (!this.compareValues(newValue, this._value)) {
            var displayValue = this.mapValueToDisplay(newValue);
            this.setElementValue(displayValue, newValue);
            if (isc.Canvas.ariaEnabled()) this.setAriaState("checked", !!newValue);
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
