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

 





//>	@class	RadioItem
// Form item representing a member of a radio group, subclassed from +link{NativeCheckboxItem}.
// RadioItems items are created and managed automatically by +link{RadioGroupItem} instances
// and should not be instantiated directly.
//
//  @treeLocation   Client Reference/Forms/Form Items/RadioGroupItem
// @visibility external
//<
isc.ClassFactory.defineClass("RadioItem", "NativeCheckboxItem");
isc.RadioItem.addProperties({
    //> @attr radioItem.prompt (HTMLString : null : IRW)
	// Mouse-over prompt for the label of this item
	//		@group	appearance
	//<
	//prompt:null

    //>	@attr	radioItem._elementType				(string : "RADIO" : IRW)
	//			type of item ("CHECKBOX" or "RADIO")
	//		@group	appearance
	//<
	_elementType:"RADIO", 
    
    //>	@attr	radioItem.value             (any : true : IRW)
	//          "value" for this radio item, to be returned when the item is selected.
	//  @group formValues
    //  @visibility internal
	//<
    value : true,
    
    //>	@attr	radioItem.unselectedValue   (any : null : IRW)
	//          Value to be returned from this radio item when it is unselected.
	//   @group formValues
    //   @visibility internal
	//<
    
    //>	@attr	radioItem.defaultValue      (any : null : IRW)
	//          Override defaultValue to be null - note that the value returned from an unselected
    //          radioItem will always be null. Set to radioItem.value to have the radioItem be drawn
    //          in a selected state initially.
	//   @group formValues
	//<
    defaultValue:null
});
isc.RadioItem.addMethods({


	//>	@method	radioItem.setElementValue()
	//		@group	elements
	//			update the visible value displayed in the form element to the reflect value passed in
	//
	//		@param	newValue 	(any)				value to set the element to
	//<
	setElementValue : function (newValue) {
		// get a pointer to the element for this item
		var element = this.getDataElement();
		
		// if no element was found, bail
		if (!element) return null;
        
        
        if (isc.isA.String(this.value)) newValue = (newValue + "");
        
		// set the value of the item
		return element.checked = (this.value == newValue);
	},
    
	//>	@method	radioItem.getElementValue()
	//		@group	elements
	//			return the value stored in the form element(s) for this item
	//
	//		@return	(any)		value of this element
	//<
	getElementValue : function () {
		// get a pointer to the element for this item
		var element = this.getDataElement(),
            selectedValue = this.value,
            unselectedValue = this.unselectedValue;
		
		// if no element was found, bail
		if (!element) return unselectedValue;
		
		// get the value of the item
		return (element.checked ? selectedValue : unselectedValue);
	},
	
	//>	@method	radioItem.boxTitleClick()
	//		@group	event handling
	//			handle a click on the label of a checkbox or radio button
	//			this toggles the state of the item
	//
	//<
	boxTitleClick : function () {
		// get a pointer to the element
		var element = this.getDataElement();
		
		// toggle the checked property of the element
		if (element && !element.checked) {
			element.checked = true;
			// call the elementChanged method of the form
			this.form.elementChanged(this.getItemID())
		}
	},
	
	//>	@method	radioItem.mapValueToDisplay()	(A)
	//		@group	drawing
	//			Map from the internal value for this item to the display value.
	//		@param	internalValue		(string)	Internal value for this item.
	//		@return	(string)	Displayed value corresponding to internal value.
	//<
	mapValueToDisplay : function (internalValue) {
		return internalValue;
	},
	
	//>	@method	radioItem.mapDisplayToValue()	(A)
	//		@group	drawing
	//			Map from a the display value for this item to the internal value.
	//
	//		@param	displayValue	(string)	Value displayed to the user.
	//		@return	(string)	Internal value corresponding to that display value.
	//<
	mapDisplayToValue : function (displayValue) {
		return displayValue;
	},

    //> @method radioItem.setElementReadOnly()
    // Change the read-only state of the form element immediately.
    //<
    setElementReadOnly : function (readOnly) {
        // A radio button should be rendered as disabled to simulate read-only.
        this._setElementEnabled(!readOnly && !this.isDisabled());
    }

});


