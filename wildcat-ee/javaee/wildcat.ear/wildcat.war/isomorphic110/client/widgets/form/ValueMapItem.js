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
// Class will not work without the ListGrid
if (isc.ListGrid) {

// ValueMapEditor class
// Simple subclass of HLayout containing a button and a listGrid
// used by the valueMapItem class (currently not functional as a standalone widget)
isc.ClassFactory.defineClass("ValueMapEditor", "VLayout");

isc.ValueMapEditor.addProperties({
    
    // Defaults for the button to toggle state
    mapTypeConstructor : isc.Button,
    
    mapTypeDefaults : {
        autoDraw:false,
        
        width:"100%",
        
        click : function () {
            this.creator.canvasItem.toggleObjectArray();
        },
        
        showIf : function () {
            return this.creator.canvasItem.showMapTypeButton;
        },
        
        getTitle : function () {
            var canvasItem = this.creator.canvasItem,
                isObject = canvasItem.saveAsObject;
            if (isObject) return canvasItem.saveAsObjectTitle;
            else return canvasItem.saveAsArrayTitle;
        }
    },
    
    selectorConstructor : isc.ListGrid,
    
    selectorDefaults : {
        // allow it to auto-expand as required
        height:1,
        overflow:"visible",
        bodyOverflow:"visible",
        inherentHeight:true,
        
        // Make it editable, and support adding new value map options easily
        showNewRecordRow:true,
        listEndEditAction:"next",
        canEdit:true,
        editEvent:isc.EH.CLICK,

        // NOTE: tall enough to avoid row slightly enlarging when starting edit, which
        // will normally cause the containing form to redraw
        cellHeight:22,

        // Disable selection of rows.
        selectionType:isc.Selection.NONE,
        selectOnEdit:false,
        
        leaveScrollbarGap:false,
        showSortArrow:isc.ListGrid.NONE,
        canSort:false,
        canResizeFields:false,

        // Override dataChanged to save the new valueMap as the user edits the ListGrid 
        // rows
        dataChanged : function () {

            this.Super("dataChanged", arguments);
            if (this.creator && this.creator.canvasItem) 
                this.creator.canvasItem.updateValue();
        },

        // validation: Ensure the value map ends up being a valid structure.
        stopOnErrors:true,
        
        validateCellValue : function (rowNum, colNum, newValue, oldValue) {
        
            var fieldName = this.getFieldName(colNum),
                canvasItem = this.creator.canvasItem,
                valueMapErrors = 
                    canvasItem._validateSelectorCellValue(fieldName, rowNum, newValue);
                
            if (valueMapErrors) return valueMapErrors;
            return this.Super("validateCellValue", arguments);
        },

        // ContextMenu:
        // We want to show a context menu allowing the user to 
        // - add a new option
        // - delete the clicked option (if there is one)
        // - toggle between Array and JS Object Literal type valueMaps
        
        cellContextClick : function (record, rowNum, colNum) {
            var cm = this.creator.canvasItem.getSelectorContextMenu(record);
            cm.showContextMenu();
            // cancel the native cm
            return false;
        },
        
        getHeaderContextMenu : function () {
            return this.creator.canvasItem.getSelectorContextMenu();
        }        
    }
});

isc.ValueMapEditor.addMethods({
    initWidget : function () {
        if (!this.canvasItem) {
            this.logWarn("ValueMapEditors are not supported as standalone widgets at this time.");
            return;
        }
  
        this.Super("initWidget", arguments);

        this.addAutoChild("mapType", {});
        
        this.addAutoChild("selector", {
        
            newRecordRowMessage : this.canvasItem.newOptionRowMessage,
 
            showHeader:this.showHeader,
           
            // this._getSelectorFields() will return either a single field for an array type
            // valueMap, or 2 fields (for value / display value) for object type valueMaps.
            fields:this.canvasItem._getSelectorFields(),

            // this._getSelectorData() will turn this item's valueMap into a valid data array
            // for the ListGrid
            data:this.canvasItem._getSelectorData()
            
        });
    }
});


//>	@class	ValueMapItem
//
// Form item with an interface especially designed for editing valueMaps.
// The valueMaps will show up in an editable ListGrid, with context menu items for adding / 
// removing values.
// Supports editing JS object literal format value maps or arrays. Context menu includes an
// option to change between these formats.
//<
isc.ClassFactory.defineClass("ValueMapItem", "CanvasItem");


isc.ValueMapItem.addProperties({

    // Override height to be small - will auto expand to accomodate content
    height:1,

    autoDestroy:true,

    //> @attr valueMapItem.shouldSaveValue (Boolean : true : IR)
    // @include FormItem.shouldSaveValue
    //<
    shouldSaveValue:true,

    //>@attr ValueMapItem.saveAsObject (boolean : false : IR)
    // Whether to create an Array or Object valueMap.  Switchable by the user
    // if +link{showMapTypeButton} is true.
    //<

    //>@attr    ValueMapItem.showMapTypeButton  (boolean : true : IR)
    // Should we show the button to toggle between object / array valueMaps?
    // We also support this functionality via a context menu.
    //<    
    showMapTypeButton: true,

    //> @attr ValueMapItem.allowDuplicates (boolean : false : IRW)
    // Whether to allow duplicates when editing Arrays (ie, when +link{saveAsObject}
    // is false).  Duplicate property names in Objects are never allowed.
    //<

    //> @attr ValueMapItem.showHeader (boolean : true : IR)
    // Whether headers should be show in the grid of values.  Typically not desired for
    // editing simple arrays of items.
    //<
    showHeader:true,

    //>@attr    ValueMapItem.newOptionRowMessage    (string : "Click to add a new option" : IR)
    //  Message to display in the 'new option row' - the row which can be clicked by the
    // user to add a new option to the valueMap.
    //<
    newOptionRowMessage: "Click to add a new option",
    
    //>@attr    ValueMapItem.displayTitle   (string : "Display" : IR)
    //  Title for the display value field in our selector listGrid. Only shown if this item's
    //  value is an object (mapping a display value to an internal value).
    //<
    displayTitle: "Display",
    
    //>@attr    ValueMapItem.valueTitle (string : "Value" : IR)
    //  Title for the value field in our selector listGrid.
    //<
    valueTitle: "Value",
    
    
    //>@attr    ValueMapItem.undefinedKeyErrorMessage   (string : "Each valueMap option must have a defined value" : IRW)
    //  Error message to display if the user attempts to save an undefined key in a
    //  JS Literal object valueMap  
    //<    
    undefinedKeyErrorMessage : "Each valueMap option must have a defined value",
    
    //>@attr    ValueMapItem.duplicateValueErrorMessage (string : "Please enter a unique value for this option" : IRW)
    //  Error message to display if the user attempts to save duplicate values in this item's
    //  valueMap.
    //<    
    duplicateValueErrorMessage : "Please enter a unique value for this option",
    
    //>@attr    ValueMapItem.saveAsArrayTitle   (string : "Stored == Displayed" : IRW)
    //  Title for the button / menu item to save our valueMap in array (rather than JS 
    //  Object literal) format
    //<    
    saveAsArrayTitle : "Stored == Displayed",
    
    //>@attr    ValueMapItem.saveAsObjectTitle  (string : "Stored != Displayed" : IRW)
    //  Title for the button / menu item to save our valueMap in JS Object literal (rather 
    //  than Array) format.
    //<    
    saveAsObjectTitle : "Stored != Displayed"

});

isc.ValueMapItem.addMethods({
    
    // _createCanvas()
    // Create the ListGrid used to actually edit the valueMap and set up APIs between the 
    // ListGrid and the ValueMapItem
    _createCanvas : function () {

        if (this.canvas) return;
    
        // Ensure we are working with a valid value (note: should already have been set to
        // this.defaultValue if appropriate, so this handles the case where this.defaultValue 
        // is null)
        if (this._value == null) this._value = [];
 
        this.canvas = isc.ValueMapEditor.create({
            autoDraw:false, _generated:true,
            ID:this.getID() + "_editor",
            canvasItem:this,
 
            showMapType:this.showMapTypeButton,
            showHeader:this.showHeader,
           
            // Allow it to expand to accomodate content.
            height:this.height,
            overflow:isc.Canvas.VISIBLE
        });

        this.Super("_createCanvas", arguments);        
    },
    
    _getMapTypeButton : function () {
        return this.canvas.mapType;
    },

    // _getSelectorGrid() - returns a pointer to the ListGrid in the valueMap editor we created
    // on init.
    _getSelectorGrid : function () {
        return this.canvas.selector;
    },
    
    // _getSelectorData()
    // Converts our (valueMap) value to an array of records that can be displayed by the 
    // selector ListGrid
    _getSelectorData : function (value) {
        var value = value || this.getValue();
        if (!value) value = this.saveAsObject ? {} : [];
        
        var data = [];
        if (isc.isAn.Array(value)) {
            for (var i = 0; i < value.length; i++) {
                data[i] = {value:value[i]}
            }
        } else {
            var i = 0;
            for (var fieldValue in value) {
                data[i] = {value:fieldValue, display:value[fieldValue]}
                i++;
            }
        }
        return data;
    },

    // _getSelectorFields()
    // Fields for the selector listGrid. If we're saving the valueMap as an object, returns
    // 2 fields for display and internal value, otherwise just returns a value field.
    _getSelectorFields : function () {
        if (!this._displayField || !this._valueField) {
            this._displayField = {name:"display", title:this.displayTitle};
            this._valueField = {name:"value", title:this.valueTitle};
        }
        var fields = [this._valueField];
        if (this.saveAsObject) fields.add(this._displayField);
        
        return fields;
    },

    
    // _validateSelectorCellValue()   Method to validate the edited value to ensure it will
    // produce a valid valueMap.
    
    _validateSelectorCellValue : function (fieldName, index, newValue) {
        // No restrictions on the display value for valueMaps.
        if (fieldName != "value") return;
        if (!this.saveAsObject && this.allowDuplicates) return;

        var grid = this._getSelectorGrid(),
            newRow = index > grid.data.getLength(),
            currentValue = this._value,
            currentKeys;

        if (this.saveAsObject) {

            
            if (newValue == null || newValue == "") {
                return [this.undefinedKeyErrorMessage]
            }
            // NOTE: getKeys() returns [] for null
            currentKeys = isc.getKeys(currentValue);
        } else {
            currentKeys = currentValue || [];
        }

        var collidingValues;
        if (newRow) {
            if (currentKeys.contains(newValue)) collidingValues = true;
        } else {
            for (var i = 0; i < currentKeys.length; i++) {
                if (index == i) continue;
                if (newValue == currentKeys[i]) {
                    collidingValues = true;
                    break;
                }
            }
        }

        // If we have colliding values, notify the user
        if (collidingValues) return [this.duplicateValueErrorMessage];
        return null;
    },

    // getSelectorContextMenu()     Method to return a contextMenu for our selector listGrid.
    // Called from a click on some cell or on the header.
    // Will be passed a non-null record parameter if the event occurred over a saved row.
    
    getSelectorContextMenu : function (record) {

        if (!this._selectorCM) {
        
            var items = [
                {title:"Add new option", click:"menu.canvasItem.addOption()"},
                {title:"Delete option", enableIf:"menu.record != null",
                    click:"menu.canvasItem.removeOption(menu.record)"
                }
            ];
            if (this.showMapTypeButton) {
                items.add(
                {   dynamicTitle:"this.canvasItem._getToggleObjectArrayTitle()",
                    click:"menu.canvasItem.toggleObjectArray()"
                }
                );
            }
        
            this._selectorCM = this.ns.Menu.create({ 
                                    canvasItem : this, ID:this.getID() + "_selectorMenu",
                                    data:items
                               });
        }
        
        this._selectorCM.record = record;
        return this._selectorCM;
    },
    
    
    // Helper methods for the menu options
    
    //>@method  ValueMapItem.addOption()
    // Start editing a new option added to the end of the valueMap
    //<
    addOption : function () {
        this._getSelectorGrid().startEditingNew();
    },
    
    //>@method  ValueMapItem.removeOption()
    // Remove an option from the valueMap.
    //<
    
    removeOption : function (record) {
        var grid = this._getSelectorGrid();
        grid.data.remove(record);
        // datachanged will call 'upateValue' on this item
        grid.data.dataChanged();
    },

    // _getToggleObjectArrayTitle()     returns the dynamic title for the menu item used
    // to toggle between saving valueMap as an object and as an array.    
    _getToggleObjectArrayTitle : function () {
        var saveAsObject = this.saveAsObject;
        return saveAsObject ? this.saveAsArrayTitle
                            : this.saveAsObjectTitle;
    },
    
    // Helper method to toggle between saving valueMap as an object and as an array.    
    toggleObjectArray : function () {
        this.setSaveAsObject(!this.saveAsObject);
    },
    

    // setSaveAsObject()
    // method to modify whether the valueMap produced is an object or an array.
    // Will munge data into approprate format
    setSaveAsObject : function (saveAsObject) {
        if (this.saveAsObject == saveAsObject) return;
        
        // convert the current value to the new value
        // We could warn if we're moving from an object to an array as we'll lose some data.
        var newValue,   value = this.getValue();
        if (saveAsObject) {
            newValue = {};
            if (value != null) {
                for (var i = 0; i < value.length; i++) {
                    newValue[value[i]] = value[i];
                }
            }
            
        } else {  
            newValue = [];
            for (var internalValue in value) {
                newValue.add(internalValue);
            }
        }
        
        this._value = newValue;
        this.saveAsObject = saveAsObject;
        
        var grid = this._getSelectorGrid(),
            data = this._getSelectorData();

        // If we're not initialized for some reason, bail here - when we get initialized we'll
        // set up our UI with the appropriate properties.
        if (!grid) return;
        
        grid.setData(data);
        
        grid.setFields(this._getSelectorFields());
        
        // redraw the button (if we're showing it) to refresh the title
        if (this.showMapTypeButton) this._getMapTypeButton().markForRedraw();

    },
    
    
    // Override isEditable
    isEditable : function () {
        return true;
    },
    
    // Override updateValue() to retrieve the value from our selector ListGrid
    updateValue : function () {
    
        var grid = this._getSelectorGrid(),
            data = grid.data,
            saveAsObject = this.saveAsObject,
            value = saveAsObject ? {} : [],
            changed;
            
        if (saveAsObject) {
            // Make a copy of the values currently stored as this._value. We use this for
            // change detection
            var dupVals = {};
            if (isc.isAn.Array(this._value)) {
                changed = true;
             } else {
                isc.addProperties(dupVals, this._value);
            }
              
            var dupKeys = isc.getKeys(dupVals);
            
            for (var i = 0; i < data.length; i++) {
                var record = data[i],
                    newVal = record.display;
                
                value[record.value] = record.display;
                // If the key was previously undefined or mapped to a different display value, 
                // we've changed
                if (!dupKeys.contains(newVal) || (dupVals[record.value] != newVal)) 
                    changed = true;
                delete dupVals[record.value];
            }

            // If the old values contained any value not contained in the new values, we
            // need to fire our change handler.
            if (isc.getKeys(dupVals).length != 0) changed = true;

        } else {

            // If our saved data is of the wrong type, or of a different length,
            // we know the value has changed - ensure we fire the change handler.
            if (!isc.isAn.Array(this._value) || (this._value.length != data.length)) {
                changed = true;
            }
            
            for (var i = 0; i < data.length; i++) {
                var record = data[i],
                    newVal = record.value;
                value[i] = newVal;
                
                // Avoid trying to examine this._value if we know the value has changed.
                // This avoids us trying to access values by index on an object.
                if (!changed && (this._value[i] != newVal)) changed = true;
            }
            
        }

        
        if (!changed) return;
        
        // fire the change handler, and bail if the change failed validation, etc.
        // Note: this method will call 'setValue()' to reset to the old value, or any value
        // suggested by the validators
        if (this.handleChange(value, this._value) == false) return;
        
        value = this._changeValue;
        delete this._changeValue;
        
        // save the value
        this.saveValue(value);        
    },
    
    // Override setValue to update the data in the ListGrid
    setValue : function (value) {
        this._setValueCalled = true;
        
        if (value != null && !isc.isAn.Object(value)) {
            this.logWarn("setValue() passed an invalid object. Must be a valueMap (either " + 
                         "specified as an array or a raw JS object.");
            return
        }
    
        var valueIsArray = isc.isAn.Array(value),
            saveAsObject = (!!this.saveAsObject);
        
        // If we're passed an array when we should be saving as an object (or vice versa)
        // update this.saveAsObject
        if (value != null && valueIsArray == saveAsObject) {
            this.logInfo("setValue() passed a valueMap of type "  + 
                            (valueIsArray ? "Array" : "Object") + 
                         ".  Updating this.saveAsObject to match this data type.");
            this.setSaveAsObject(!valueIsArray);
            saveAsObject = this.saveAsObject;
            
        } 

        this._value = value;
        this._getSelectorGrid().setData(this._getSelectorData());
    }
    
})

}

// Editor for Arrays of simple types (eg DynamicForm.colWidths)
isc.defineClass("ArrayItem", isc.ValueMapItem).addProperties({
    showMapTypeButton:false,
    showHeader:false,
    saveAsObject:false,
    allowDuplicates:true,
    newOptionRowMessage:"Click to add values"
});

// Editor for mappings expressed as objects, where Array format is not allowed
isc.defineClass("MappingItem", isc.ValueMapItem).addProperties({
    showMapTypeButton:false,
    saveAsObject:true
});
