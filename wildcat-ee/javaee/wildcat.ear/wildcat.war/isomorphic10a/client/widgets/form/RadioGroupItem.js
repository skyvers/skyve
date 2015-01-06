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

 
 






//>	@class	RadioGroupItem
// FormItem that shows a set of mutually exclusive options as a group of radio buttons.
// @visibility external
//<
isc.ClassFactory.defineClass("RadioGroupItem", "ContainerItem");
isc.RadioGroupItem.addProperties({

    //>	@attr	radioGroupItem.itemHeight		(number : 20 : IRW)
	//			default height of EACH item
	//		@group	appearance
	//<
	itemHeight:20,

    //>	@attr	radioGroupItem.vertical			(Boolean : true : IRW)
	//			True == display options vertically, false == display in a single row
	//		@group	appearance
    // @visibility external
	//<
	vertical:true,

    //> @attr radioGroupItem.prompt (HTMLString : null : IRW)
	// Don't have a prompt for the entire item -- use itemPrompt to set the prompt of each
    // individual button.
	//		@group	appearance
	//<
	prompt:null,

    //> @attr radioGroupItem.itemPrompt (HTMLString : null : IRW)
	// Mouse-over prompt for the label of this item.  Use character <code>"*"</code>
    // to substitute in the item value. For example <code>"Select value *"</code> would 
    // show the prompt <b>"Select value a"</b> when a user hovered over a radio button for
    // the value <code>"a"</code>.
	//		@group	appearance
	//<
	//itemPrompt:null

    //>	@attr	radioGroupItem.textBoxStyle (FormItemBaseStyle : "labelAnchor" : IRW)
	// Base CSS class applied to the text for items within this radiogroup.
	// @group appearance
    // @visibility external
	//<
    textBoxStyle:"labelAnchor",
    
    //>	@attr	radioGroupItem.wrap (boolean : null : IRW)
	// Should the text for items within this radio group wrap?
	// @group appearance
    // @visibility external
	//<

	// Don't write out the <label for...> tag around the title of a radioGroup as a whole
	// - we'll write it out around the individual radio-items' titles. Writing it out for
	// the group-title as a whole gives the odd effect of making this title the tab-stop
	// for the item in IE.
	writeOutLabelTag:false
});

isc.RadioGroupItem.addMethods({

    init : function () {
        // handle init-time only 'disabledValues' flag
        if (this.disabledValues != null) {
            this._disabledValues = {};
            for (var i = 0; i < this.disabledValues.length; i++) {
                var value = this.disabledValues[i];
                this._disabledValues[value] = true;
            }
        }
        return this.Super("init", arguments);
    },

    // Override _useHiddenDataElement to return false
    // we're not marked as having a data element (hasDataElement() == false) but our sub elements
    // do have live HTML data elements, so on form submission we don't need to write out a
    // hidden data element.
    _useHiddenDataElement : function () {
        return false;
    },

    //> @method radioGroupItem.valueHoverHTML() (A)
    // If defined, this method should return the HTML to display in a hover canvas when the 
    // user holds the mousepointer over one of the radio-items in this RadioGroupItem.  Return 
    // null to suppress the hover canvas altogether.
    //
    // @param value (Any) The sub-value (radio-item) to get the hoverHTML for
    // @param item (RadioGroupItem) Pointer to this item
    // @param form (DynamicForm)  This item's form
    // @return (HTMLString) HTML to be displayed in the hover
    // @group Hovers
    // @visibility external    
    //<

    

	//>	@method	radioGroupItem.setItems()	(A)
	// Override setItems() to create radio buttons for each item in our valueMap.
	// We store items we've already created in a cache so we don't create them over and
    // over redundantly.
	//
	//		@group	drawing
	//
	//		@param	value	(string)	Value of the element [Unused because it is more reliably set by setValue].
	//		@return	(HTML)	HTML output for this element
	//<
    // Note: If the developer were to specify more than one item in the valueMap with the same
    // title/value, we would get in trouble here, as we'd end up with more than one entry in
    // our items array pointing to the same form item - This is an invalid usage by the
    // developer, but we don't currently catch this case explicitly.
	setItems : function () {
		var valueMap = this.getValueMap();
		if (!this.itemCache) this.itemCache = {};
		
		// create an array to hold the new set of items
		var items = [];
        
		if (isc.isAn.Array(valueMap)) {
			for (var i = 0; i < valueMap.length; i++) {
				var value = valueMap[i];
                
				items.add(this._getRadioItem(value, value));
			}
		} else {
			for (var value in valueMap) {
				var title = valueMap[value];
                
				items.add(this._getRadioItem(title, value));
			}
		}
		// call the superclass method to actually create the items
		return this.Super("setItems", [items]);
	},
    
	//> @method radioGroupItem._getRadioItem() (A)
	// Internal method to create radio item instances based on the title/value passed in.
	//
	// @param title (string) Title to display next to the radio item
    // @param value (any) value the radio item represents.
	// @return (RadioItem) RadioItem instance to plug into the items array.
	// @group drawing
	//<
    _getRadioItem : function (title, value) {
    
        var item = this.itemCache[value+"|"+title];
        // If we're reusing an item, clean off the old properties from when it was last used
        // such as _value / hasFocus
        
        if (item) {
            if (value != item._value) delete item._value;
            delete item.hasFocus;
            delete item.disabled;

        } else {
        
            var prompt;
            if (this.itemPrompt) {
                var starRE = new RegExp("\\*","g");
                prompt = this.itemPrompt.replace(starRE, title);
            }
            var itemObj = {
                type: "radio", 
                
                // ContainerItem stores sub-items under subItem.name.  Give the RadioItems names
                // that won't collide with other properties, but that we can look up by value.
                name: "_radioItem" + value,
                _autoAssignedName: true,

                value: value,
                // override getElementName - the 'name' written into the HTML element
                // must be the same for all these elements to create a radiogroup.
                getElementName : function () {
                    return this.parentItem.getElementName();
                },
                
                title: title, 
                prompt: prompt, 
                height: this.itemHeight, 
                _startRow: this.vertical, 
                
                // apply styling down to the items
                textBoxStyle: this.textBoxStyle,
                wrap: this.wrap,
                
                
                updateValue : function () {
                    // pass in this.value (not this.getValue() which could be null)
                    this.parentItem.updatePreviousSelection(this.value);
                    return this.Super("updateValue", arguments);
                },
                
                setDisabled : function (disabled) {
                    this.parentItem._itemDisabled(this.value, disabled);
                    return this.Super("setDisabled", arguments);
                },
                
                shouldSaveValue: false,
                
                // don't allow changed events from sub-items to fire on the form
                suppressItemChanged: true
            };

            // support arbitrary modification of the item via an auto-child like properties block
            isc.addProperties(itemObj, this.itemProperties);
            
            var _this = this;

            if (!itemObj.itemHoverHTML && this.valueHoverHTML) {
                itemObj.itemHoverHTML = function () {
                    return _this.valueHoverHTML(value, _this, _this.form);
                }
            }

            item = this.itemCache[value+"|"+title] = isc.FormItemFactory.makeItem(itemObj);
        }
        // The RadioGroup manages the disabled status of individual sub-items based on their
        // value.
        // Apply the appropriate disabled status to the item in question.
        
        if (this._disabledValues != null && this._disabledValues[value] != null) {
            item.disabled = this._disabledValues[value];
        }
        return item;
    },
    
    //>@attr RadioGroupItem.itemProperties (RadioItem Properties : null : IR)
    // Map of properties to apply to generated items within this RadioGroup. This allows you to 
    // customize the generated radio items for this item.
    // @visibility external
    //<
    // No need for a defaults block - the developer can just update the RadioItem class
    
    
    // getItemValue is used when generating member item HTML
    
    getItemValue : function (item) {
        var itemVal = item.value;
        if (itemVal == this.getValue()) return itemVal;
        return item.unselectedValue;
    },

    itemForValue : function (value) {
        return this["_radioItem" + value];
    },
    
    //> @attr radioGroupItem.disabledValues (Array of String : null : I)
    // This property allows you to specify an initial set of disabled options within
    // this radioGroup. Once the RadioGroupItem has been created +link{setValueDisabled()}
    // should be used to enable and disable options.
    // @visibility external
    //<
    
    //> @method radioGroupItem.setValueDisabled()
    // Disable or Enable a specific option within this radioGroup
    // @param value (any) value of option to disable
    // @param disabled (boolean) true to disable the option, false to enable it
    // @visibility external
    //<
    setValueDisabled : function (value, disabled) {
        if (this._disabledValues != null && this._disabledValues[value] == disabled) return;
        var item = this.itemForValue(value);
        if (item && this.items.contains(item)) {
            // call 'setDisabled' on the item directly.
            // This is overridden to update our "disabled values" object
            item.setDisabled(disabled);
        // Support changing the disabled status for a value even if it
        // doesn't have an associated item. This actually means you can disable a value
        // that isn't necessarily in the valueMap - and if the valueMap is updated to
        // include it the item in question will show up disabled.
        } else {
            this._disabledValues[value] = disabled;
        }
    },
    
    _itemDisabled : function (value, disabled) {
        if (this._disabledValues == null) this._disabledValues = {};
        this._disabledValues[value] = disabled;
    },

    // Override ContainerItem.getTitleHTML().
    getTitleHTML : function () {
        // We need something with an ID to set the 'aria-labelledby' attribute to.
        
        var title = this.getTitle();
        return isc.SB.concat("<label id=", this._getLabelElementID(), ">", title, "</label>");
    },

	//>	@method	radioGroupItem.getInnerHTML()	(A)
	//		@group	drawing
	//			output the HTML for this element
	//
	//		@param	value	(string)	Value of the element [Unused because it is more reliably set by setValue].
	//		@return	(HTML)	HTML output for this element
	//<
	getInnerHTML : function (value) {        
		// always call setItems() since the valueMap may have changed!
        this.setItems();
		return this.Super("getInnerHTML", arguments);
	},
    
    // Override isEditable to indicate that the user can directly update the radioGroup's value
    isEditable : function () {
        return true;
    },

    //> @method radioGroupItem.setElementReadOnly()
    // Change the read-only state of the form element immediately.
    //<
    setElementReadOnly : function (readOnly) {
        // There is no element to mark read-only but we don't want the entire radio group
        // to be redrawn so we handle this method and do nothing. The radio items will be
        // updated individually.
    },

    //> @method radioGroupItem.setValue()
    //      Set the value of this radiogroup item.
	//
    //      @group formValues
    //      @param  newValue    (any)           value to set the item to
	//<
    setValue : function (newValue) {
        this._setValueCalled = true;

        var invalidValue = (this.valueMap == null) ||
            (isc.isAn.Array(this.valueMap) ? !this.valueMap.contains(newValue)
                                            : !isc.propertyDefined(this.valueMap, newValue));
            
            
        // use the default value if passed null, or a value for which we don't have a sub-item
        if (newValue == null || invalidValue) {
            newValue = this.getDefaultValue();
        }
        // "check" the appropriate sub item. 
        // If there is none, uncheck whatever is currently selected.  
        // Notes:
        // - We performed this check above to determine whether we need to apply a default
        //   value.  We still need to check again as we may not have a default value (or our
        //   default value could conceivably not be included in our valueMap).
        // - Clearing out our value if we don't find the value passed in means the user setting 
        //   the value of a radio-group to anything not in the valueMap is an equivalent to
        //   setting the value to null.
        if (this.items != null) {
            var itemForValue = this.itemForValue(newValue);
            if (itemForValue != null) {
                this.itemForValue(newValue).setValue(newValue);
            }
            
            // if the previous value is not null, clear out the prev item's value
            // This serves 2 purposes:
            // - if we're clearing the value for this item, its required to update the
            //   currently selected item on the page
            // - otherwise the currently selected item will already have been cleared, but
            //   we still want to clear out its _value property.
             if (this._value != null && this._value != newValue) {
                 var prevItem = this.itemForValue(this._value);
                 if (prevItem) prevItem.setValue(null);
             } else if (this._changingValue) {
                var prevItem = this.itemForValue(this._changeValue);
                if (prevItem) prevItem.setValue(null);
             }
        }

        // save the new value        
        this.saveValue(newValue);
    },

    // this function is fired as a result of a selection change in the radioGroup
    // Its purpose is to ensure that the item that was previously selected has its internal
    // _value updated to match the fact that it has been deselected.
    // We have to do this because when a radio item gets selected, it fires a change handler
    // for itself, but no change handler is fired for the previous selection which gets 
    // automatically deselected due to native mutex behavior
    // This function updates the deselected item to ensure that a getValue() call for it returns
    // the appropriate value (null)
    updatePreviousSelection : function (newValue) {
        // Bail if 'updateValue' is fired on the currently selected item.
        // This avoids an infinite loop whereby 
        //  "A" is initially selected
        //  User selects "B"
        //  'updateValue' on "B" fires 'updatePreviousSelection' on the group item
        //  updatePreviousSelection on the group item fires 'updateValue' on "A"
        //  'updateValue' on "A" fires 'updatePreviousSelection' on the group item
        // -- we stop the process there
        
        // Note: We support valueMaps as objects or arrays.
        // If the valueMap is an object, the keys are always strings - so this:
        //   valueMap:{true:"Yes", false:"No"}
        // Will map to "true" and "false" rather than true and false.
        // In this case we are likely to see the value of the item being set programmatically
        // to a non string value (such as boolean true), but the "newValue", picked up from
        // updateValue() on the selected item being a string value.
        // Ensure in this case we convert the old value to a string before comparing.
        // (if we skip this we can end up with failure to update or infinite loops in some cases)
        var currentValue = this.getValue();           
        if (isc.isA.String(newValue)) currentValue = currentValue + "";        
        if (currentValue == null || newValue == currentValue || 
            this.itemForValue(currentValue) == null) 
        {
            return;
        }
        this.itemForValue(currentValue).updateValue();
        
        
    },

    // override updateValue to get the value from the items
    // NOTE: We're currently relying on the fact that 'elementChanged()' will be bubbled and the
    // default implementation of this internal handler will fire updateValue() on a form item.
    // We may want to prevent elementChanged from bubbling, since Form Items should be managing
    // their own elements completely opaquely.  If so we would want to write change handlers
    // onto the sub-items to fire this.parentItem.updateValue().
    updateValue : function () {
        var value;
        // Any non-selected radioItem will return "null" from getValue();
        for (var i = 0; i < this.items.length; i++) {
            value = this.items[i].getValue();
            if (value != null) {
                break;
            }
        }

        // bail if the value is unchanged
        if (value == this._value) return;

        // fire the change handler, and bail if the change failed validation, etc.
        // Note: this method will call 'setValue()' to reset to the old value, or any value
        // suggested by the validators
        if (this.handleChange(value, this._value) == false) {
            return;
        }
        // check for updates performed by the change handler.
        value = this._changeValue;
      
        // save the value
        this.saveValue(value);
        
        // fire any specifed 'changed' handler for this item.
        this.handleChanged(value);
        

    },

	//>	@method	radioGroupItem.setValueMap
	//  Override setValueMap to redraw the form with the new set of radio items
	//<
    // A better way to do this would be to replace just the inner HTML for the relevant cell in
    // the dynamicForm, but currently we have no way to do that.
	setValueMap : function (valueMap) {
		this.Super("setValueMap", arguments);
		// unfortunately, there is no way to just update these values
		//  so we have to tell the item to redraw (will redraw the form by default)
		this.redraw();	
	},
	
	//>	@method	radioGroupItem.getHeight()	(A)
	//		@group	sizing
	//			output the height for this element
	//
	//		@return	(number)	height of the form element
	//<
    getHeight : function () {
        var cellPadding = isc.isA.Number(this.cellPadding) ? this.cellPadding : 0;
        if(!this.vertical) return this.itemHeight + cellPadding * 2;
        var valueMap = this.getValueMap(),
            numItems = 0;

        // if valueMap is an array, number of items is just the length of the array
        if (isc.isAn.Array(valueMap)) {
            numItems = valueMap.length;
        } else {
            for (var key in valueMap) {
                numItems++;
            }
        }
        return numItems * (this.itemHeight + cellPadding * 2);
    },
    
    // Override shouldStopKeypressBubbling to always stop bubbling on arrow keys
    
    _arrowKeys:{
        Arrow_Left:true, Arrow_Right:true,
        Arrow_Up:true, Arrow_Down:true
    },
    shouldStopKeyPressBubbling : function (keyName, characterValue) {
        if (this._arrowKeys[keyName]) return true;
        return this.Super("shouldStopKeyPressBubbling", arguments);
    }

});


