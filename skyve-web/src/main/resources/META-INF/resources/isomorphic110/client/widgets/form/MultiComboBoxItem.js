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
// Define this package and its core dependencies.
// Note:  MultiComboBoxItem uses "widgets/TileLayout", part of the ISC_Grids module, for its
// "flow" layout style.  It is not listed here as a dependency to allow this class to be used
// without pulling in the ISC_Grids module.


//> @class MultiComboBoxItem
// A MultiComboBoxItem is a combo box that allows the user to select multiple options.  Each
// selected option is represented as a button that can be clicked to deselect the option.
//
// The relative layout of the buttons to the combo box is specified with the
// +link{MultiComboBoxItem.layoutStyle} attribute.  The buttons will be kept in the order that
// they were added, with the most recently added button being adjacent to the combo box.
//
// <code>MultiComboBoxItem</code> uses the +link{AutoChild} pattern to construct the
// +link{multiComboBoxItem.comboBox,comboBox} and the +link{multiComboBoxItem.button,buttons}
// so that they can be easily customized.  For example, you can customize the criteria used to
// fetch by
// <smartclient>overriding +link{ComboBoxItem.getPickListFilterCriteria()} via
// +link{MultiComboBoxItem.comboBox,comboBoxProperties}.</smartclient>
// <smartgwt>using +sgwtLink{ComboBoxItem.setPickListFilterCriteriaFunction()} with
// +sgwtLink{MultiComboBoxItem.setComboBoxProperties,setComboBoxProperties()}.</smartgwt>
//
// @see ComboBoxItem
// @inheritsFrom CanvasItem
// @example multiComboBoxItem
// @visibility comboBox
//<
isc.ClassFactory.defineClass("MultiComboBoxItem", "CanvasItem");

isc.MultiComboBoxItem.addClassProperties({
//> @type MultiComboBoxLayoutStyle
// Specifies the layout of the combo box and buttons in a MultiComboBoxItem.
// @value "flow" use a +link{FlowLayout}, showing values first, then the text entry area
FLOW: "flow",
// @value "flowReverse" use a FlowLayout, with the text entry first and values shown afterwards
FLOW_REVERSE: "flowReverse",
// @value "horizontal" Use a horizontal layout with the combo box on the right
HORIZONTAL: "horizontal",
// @value "horizontalReverse" Use a horizontal layout with the combo box on the left
HORIZONTAL_REVERSE: "horizontalReverse",
// @value "vertical" Use a vertical layout
VERTICAL: "vertical",
// @value "verticalReverse" Use a vertical layout with the combo box at the bottom
VERTICAL_REVERSE: "verticalReverse",
// @see MultiComboBoxItem.layoutStyle
// @visibility external
//<

// The name of the attribute of the buttons used to store the value represented by the buttons.
_$buttonDataAttribute: "_mcb_correspondingValue",

//> @classAttr MultiComboBoxItem.defaultHint (string : "Enter values" : R)
// The default hint string.
// @group i18nMessages
// @visibility external
//<
defaultHint: "Enter values"
});

isc.MultiComboBoxItem.addClassMethods({

_binarySearch : function (value, values, compareFn, strict) {
    if (!compareFn) {
        compareFn = isc.MultiComboBoxItem._defaultCompareFn;
    }
    var undef;
    if (strict === undef) {
        strict = true;
    }
    return isc.Array._binarySearch(values, value, compareFn, strict);
},

_defaultCompareFn : isc.Array._defaultCompareFn,

// Moves the member of the layout `oldLayout` at `memberIndex` to index `newIndex` in another
// layout `newLayout`.
_transferMember : function (oldLayout, memberIndex, newLayout, newIndex, dontAnimate) {
    var member = oldLayout.getMember(memberIndex);
    if (oldLayout.Class == "FlowLayout" || newLayout.Class == "FlowLayout") {
        oldLayout.removeMember(member, dontAnimate);
    }
    newLayout.addMember(member, newIndex, dontAnimate);
},

// Moves members of `oldLayout` occupying a range of indices to `newLayout`.
_transferMembers : function (numMembers, oldLayout, memberIndex, newLayout, newIndex, dontAnimate) {
    var _transferMember = isc.MultiComboBoxItem._transferMember;
    for (var i = 0; i < numMembers; ++i) {
        _transferMember(oldLayout, memberIndex, newLayout, newIndex + i, dontAnimate);
    }
},

// Similar to `_transferMembers`, but the members that are moved are placed in reverse order.
_transferMembersReverse : function (numMembers, oldLayout, memberIndex, newLayout, newIndex, dontAnimate) {
    var _transferMember = isc.MultiComboBoxItem._transferMember;
    for (var i = 0; i < numMembers; ++i) {
        _transferMember(oldLayout, memberIndex + (numMembers-i-1), newLayout, newIndex + i, dontAnimate);
    }
},

isOrderReversedForStyle : function (layoutStyle) {
    return layoutStyle == isc.MultiComboBoxItem.HORIZONTAL ||
           layoutStyle == isc.MultiComboBoxItem.FLOW_REVERSE ||
           layoutStyle == isc.MultiComboBoxItem.VERTICAL_REVERSE;
}
});

isc.MultiComboBoxItem.addProperties({

//> @attr multiComboBoxItem.shouldSaveValue (Boolean : true : IR)
// @include FormItem.shouldSaveValue
//<
shouldSaveValue: true,

autoDestroy: true,

//> @attr MultiComboBoxItem.optionDataSource (DataSource | String : null : IR)
// The <code>optionDataSource</code> of the combo box.
// @see ComboBoxItem.optionDataSource
// @visibility external
//<

//> @attr multiComboBoxItem.addUnknownValues (Boolean : false : IRW)
// Similar to +link{ComboBoxItem.addUnknownValues}, controls whether additional values can be
// added to the ComboBox or whether the user must choose from the available values in the picklist
// only.
// <p>
// If this setting is changed after the MultiComboBoxItem has been created, the current value of
// the item is reset to null and all buttons for non-default values (values not in the +link{FormItem.defaultValue}
// array) are removed.
// @visibility external
//<
addUnknownValues: false,

//> @attr multiComboBoxItem.alwaysExitOnTab (Boolean : true : IR)
// If true, hitting tab always exits the field, and will also add a value to the list of
// selected values if there is match (and depending on the setting for
// +link{addUnknownValues,addUnknownValues}).
// <p>
// If false, if the user has typed in a value and hits tab, focus remains in the field.
// If there is a match or if +link{addUnknownValues} is true, a value will be added.
// Otherwise, the input cursor remains at the end of the entered value.
// @visibility external
//<
alwaysExitOnTab: true,

//> @attr MultiComboBoxItem.autoFetchData (Boolean : false : IR)
// Should the MultiComboBoxItem fetch data from the +link{optionDataSource,data source}
// immediately or wait until the user first opens the pickList.
// @see ComboBoxItem.autoFetchData
// @visibility external
//<
autoFetchData: false,

//> @attr MultiComboBoxItem.valueMap (Array or Object : null : IRW)
// The <code>valueMap</code> of the combo box.
// @see FormItem.valueMap
// @visibility external
//<

//> @attr multiComboBoxItem.comboBox (AutoChild ComboBoxItem : null : RA)
// An +link{AutoChild} attribute to create the combo box in a MultiComboBoxItem.
// @visibility external
//<
comboBoxDefaults: {
    name: "cb",
    _autoAssignedName: true,
    showTitle: false,
    width: "*",
    hint: isc.MultiComboBoxItem.defaultHint,
    showHintInField: true,
    completeOnTab: false,
    completeOnEnter: false,
    fetchValueOnTab: true,
    fetchValueOnEnter: true,
    loadingDisplayValue: null,
    clearEnteredValuePendingReply: true,
    shouldSaveValue: false,
    editorType: "ComboBoxItem",

    getTitle : function () {
        var creator = this.creator;
        return creator.getTitle.apply(creator, arguments);
    },

    handleKeyDown : function (a, b, c) {
        var keyName = isc.EH.lastEvent.keyName,
            isTab = keyName == this._$Tab;
        if (!this.hasFocus || this.isReadOnly() ||
            !isTab || this.creator.alwaysExitOnTab)
        {
            return this.Super("handleKeyDown", arguments);
        }

        var pickList = this.pickList,
            pickListVisible = this._isPickListVisible(),
            elementValue = this.getEnteredValue(),
            displayField = this.getDisplayFieldName(),
            shouldFetchMissingValue = this.shouldFetchMissingDisplayValue(elementValue),
            pendingOrFetchingData = this._pendingFetchOnPause() || this._loadingData();

        if (displayField != null && !pendingOrFetchingData && pickList &&
            (isc.isA.ResultSet && isc.isA.ResultSet(pickList.data)) &&
            pickList.data.localData && pickList.data.allMatchingRowsCached())
        {
            shouldFetchMissingValue = false;

            var rs = pickList.data,
                record = rs.localData.find(displayField, elementValue);
            if (record != null) this._addRecordToDisplayFieldCache(record);
        }

        if (!this.creator.addUnknownValues) {
            // If we don't need to fetch a value, it's either because there is no match and we
            // know it or there is a match.
            //
            // If there is a matching value, then update the value like normal. Otherwise leave
            // everything as-is.
            var matchingValue;
            if (!shouldFetchMissingValue &&
                (matchingValue = this.mapDisplayToValue(elementValue)) != null)
            {
                this._updateValue(elementValue, true);
                if (pickList) pickList.hide();
            }
        } else {
            if (shouldFetchMissingValue) {
                this._checkForValueFieldValue(elementValue);
                if (this.clearEnteredValuePendingReply) {
                    this.setElementValue("");
                }
            } else {
                this._updateValue(elementValue, true);
            }

            if (pickList) pickList.hide();
        }

        // Invoke ComboBoxItem's super handleKeyDown() rather than ComboBoxItem.handleKeyDown().
        this.invokeSuper(isc.ComboBoxItem, "handleKeyDown", a, b, c);

        // Prevent the default browser behavior of moving the focus.
        return false;
    },

    changed : function (comboForm, comboBoxItem) {
        this.creator._comboBoxItemChanged(comboForm, comboBoxItem);
    },

    blur : function (comboForm, comboBoxItem) {
        this.creator._comboBoxItemFocusChanged(comboForm, comboBoxItem, false);
    },
    focus : function (comboForm, comboBoxItem) {
        this.creator._comboBoxItemFocusChanged(comboForm, comboBoxItem, true);
    }
},

//> @attr multiComboBoxItem.comboForm (AutoChild DynamicForm : null : RA)
// The +link{DynamicForm} holding the +link{comboBox,comboBox}.
// @visibility external
//<
comboFormDefaults: {
    _constructor: "DynamicForm",
    autoParent: "none",
    numCols: 1,
    // suppress cellPadding so there's no extra space around the item
    
    cellPadding: 0,
    writeFormTag: false
},

//> @attr MultiComboBoxItem.displayField (string : null : IRA)
// The <code>displayField</code> of the combo box.
// @see ComboBoxItem.displayField
// @visibility external
//<

//> @attr MultiComboBoxItem.valueField (string : null : IR)
// The <code>valueField</code> of the combo box.
// @see ComboBoxItem.valueField
// @visibility external
//<

//> @attr multiComboBoxItem.valueLayout (MultiAutoChild Layout : null : RA)
// The layout used to arrange the +link{MultiComboBoxItem.comboForm,comboForm} and the buttons
// representing the values of the MultiComboBoxItem. Note that the constructor cannot be changed
// (setting a valueLayoutConstructor has no effect) because the exact layout class used depends
// on the current +link{MultiComboBoxItem.layoutStyle,layout style}.
// @visibility external
//<

//> @attr multiComboBoxItem.button (MultiAutoChild IButton : null : RA)
// An +link{AutoChild} attribute used to create the buttons in the MultiComboBoxItem.
// @visibility external
//<
buttonConstructor: "IButton",
buttonDefaults: {
    canHover: true,
    align: "left",
    icon: "[SKIN]actions/remove.png",
    iconAlign: "right",
    iconOrientation: "right",

    click : function () {
        
        this.creator._buttonClick(this);
    },
    hover : function () {
        this.creator._handleHover();
        return false;
    },
    focusChanged : function (hasFocus) {
        this.creator._buttonFocusChanged(this, hasFocus);
    }
},

//> @attr multiComboBoxItem.pendingButtonStyle (CSSStyleName : "buttonPending" : IR)
// When +link{FormItem.showPending,showPending} is <code>true</code>, the +link{Button.baseStyle}
// used on +link{attr:button,buttons} that are in the "Pending" visual state.
// <p>
// If unset, then the <code>baseStyle</code> of pending buttons is not changed.
// @see attr:deselectedButtonStyle
// @visibility external
//<
pendingButtonStyle: "buttonPending",

//> @attr multiComboBoxItem.deselectedButtonStyle (CSSStyleName : "buttonDeselected" : IR)
// When +link{FormItem.showDeletions,showDeletions} is <code>true</code>, the +link{Button.baseStyle}
// used on +link{attr:button,buttons} for values that have been deleted (also called "deselected
// buttons").
// <p>
// If unset, then the <code>baseStyle</code> of deselected buttons is not changed.
// <p>
// <strong>NOTE:</strong> Deselected buttons are also disabled, so styling should be provided
// for the <code>deselectedButtonStyle</code>&nbsp;+&nbsp;"Disabled" style name.
// @see attr:pendingButtonStyle
// @visibility external
//<
deselectedButtonStyle: "buttonDeselected",

//> @attr MultiComboBoxItem.layoutStyle (MultiComboBoxLayoutStyle : "flow" : IRW)
// Specifies the layout style of the combo box and the buttons in the MultiComboBoxItem.
// Available values are "flow" (the default), "horizontal", "horizontalReverse",
// "vertical", and "verticalReverse".
//
// <ul>
// <li><b>"flow"</b>:&nbsp; The buttons appear to the left of the combo box.  When there is no
// more room, the combo box and/or buttons flow onto a new line.  The buttons autoFit
// by default.</li>
// <li><b>"horizontal"</b>:&nbsp; The combo box appears on right and buttons are horizontally
// stacked directly left of it.  The buttons must autofit.</li>
// <li><b>"horizontalReverse"</b>:&nbsp; Like "horizontal" but the combo box appears on the left.
// The buttons must autofit.</li>
// <li><b>"vertical"</b>:&nbsp; The combo box appears on top and buttons are stacked beneath it.
// Buttons do not autofit by default.</li>
// <li><b>"verticalReverse"</b>:&nbsp; Like "vertical" but the combo box appears at bottom.
// The buttons do not autofit by default.</li>
// </ul>
// @visibility external
//<
layoutStyle: isc.MultiComboBoxItem.FLOW,

//> @attr MultiComboBoxItem.autoFitButtons (boolean : null : IR)
// Specifies whether to autofit the buttons in the MultiComboBoxItem.  The
// default value is true if +link{layoutStyle} is "flow", but false for a
// layoutStyle of "vertical" or "verticalReverse".  If the <code>layoutStyle</code>
// is "horizontal" or "horizontalReverse" then the buttons will autofit regardless
// of the setting of this property.
// @visibility external
//<

//> @attr MultiComboBoxItem.comboBoxWidth (number : 130 : IRW)
// Specifies the size of the combo box field.
// <P>
// Note that this attribute only has an effect in "flow", "horizontal", and
// "horizontalReverse" +link{layoutStyle,modes}.  In the other modes, the combo box
// is as wide as the overall MultiComboBoxItem.
// @visibility external
//<
comboBoxWidth: 130,

//> @attr MultiComboBoxItem.useInsertionOrder (Boolean : true : IR)
// Specifies whether to arrange the buttons of the MultiComboBoxItem in the order that they were
// selected (the default), or to sort the buttons by +link{displayField}.
// @visibility external
//<
useInsertionOrder: true,

// A function taking two arguments, both records, that returns a number less than zero,
// greater than zero, or exactly zero if the first record is less than, greater than, or equal
// to the second record, respectively.
sortOrder: function (recordA, recordB) {
    var displayField = this.getDisplayFieldName(),
        a = recordA[displayField],
        b = recordB[displayField];

    return isc.MultiComboBoxItem._defaultCompareFn(a, b);
}

// A list of the values of the buttons in the MultiComboBoxItem, in order of insertion, with
// the most recently inserted value at index 0.
//_buttonValues: [],

// A duplicate of _buttonValues in a sorted order as determined by sortOrder.
//_sortedButtonValues: [],

// A map from the button value to the button
//_valueToButton: {},

// A list of selected values where corresponding buttons have not yet been created because the
// full data record for these values has not yet been loaded from the server.
//_unavailableButtonValues: []

// Stores the buttons and the combo box
//_buttonsLayout: null,

// Stores the <code>dataValue</code> argument of <code>showValue()</code> until the
// MultiComboBoxItem is drawn.
//_dataValue: null,
});

isc.MultiComboBoxItem.addMethods({

init : function () {
    this.multiple = true;
    this.autoTrimMultipleValues = false;
    this._valuesSet = isc.Set.create();
    this.fetchMissingValues = true;
    if (this.value == null && this.defaultValue != null) {
        this.value = this.defaultValue;
    }
    this._buttonValues = [];
    this._sortedButtonValues = [];
    this._valueToButton = {};
    this._unavailableButtonValues = [];
    return this.Super("init", arguments);
},

//> @method multiComboBoxItem.setAddUnknownValues()
// Setter for +link{addUnknownValues,addUnknownValues}.
// <p>
// Note that when addUnknownValues is changed after the MultiComboBoxItem has been created, the
// current value of the item is reset to null and all buttons for non-default values are removed.
// @param newAddUnknownValues (boolean) the new value for addUnknownValues.
//<
setAddUnknownValues : function (newAddUnknownValues) {
    var oldAddUnknownValues = this.addUnknownValues;
    this.addUnknownValues = newAddUnknownValues;
    if (this.comboBox != null && oldAddUnknownValues != newAddUnknownValues) {
        this.comboBox.setProperties({
            addUnknownValues: newAddUnknownValues,
            changeOnKeypress: !newAddUnknownValues
        });

        this.setValue(null);

        // If changing from addUnknownValues:true to false, we need to invalidate the displayField
        // cache because we may have inserted dummy records for unknown values.
        if (newAddUnknownValues) this.invalidateDisplayValueCache();
    }
},

_getCanvasTabDescendents : function (canvas, targetArray) {
    // The comboForm is either placed first or last in the tab order.
    var placeComboFormLast = (this.alwaysExitOnTab ||
                              !this._isComboFormFirst());

    if (!placeComboFormLast) {
        this.Super("_getCanvasTabDescendents", [this.comboForm, targetArray], arguments);
    }

    if (this.useInsertionOrder) {
        var k = 0,
            endK = this._buttonValues.length;
        for (; k < endK; ++k) {
            var index = this._transformIndex(k, false);
            targetArray.add(this._buttonsLayout.getMember(index));
        }
    } else {
        var j = 0,
            endJ = this._sortedButtonValues.length;
        for (; j < endJ; ++j) {
            var index = this._transformIndex(false, j);
            targetArray.add(this._buttonsLayout.getMember(index));
        }
    }

    if (placeComboFormLast) {
        this.Super("_getCanvasTabDescendents", [this.comboForm, targetArray], arguments);
    }
},

// Invokes CanvasItem.storeValue() with a duplicate of _buttonValues/_sortedButtonValues.
_isStoredValueReverseOfButtonValues : function () {
    var layoutStyle = this.layoutStyle,
        isRTL = this.isRTL();
    return (this.useInsertionOrder &&
            ((layoutStyle === isc.MultiComboBoxItem.FLOW && !isRTL) ||
             (layoutStyle === isc.MultiComboBoxItem.FLOW_REVERSE && isRTL) ||
             (layoutStyle === isc.MultiComboBoxItem.HORIZONTAL && !isRTL) ||
             (layoutStyle === isc.MultiComboBoxItem.HORIZONTAL_REVERSE && isRTL) ||
             layoutStyle === isc.MultiComboBoxItem.VERTICAL_REVERSE));
},
_storeValue : function () {
    var oldPendingStatus = !!this.pendingStatus;
    // We want the order of the values in the stored value to match the order on screen in normal
    // reading order. This is so that when showPending is true and FormItem.compareValues() is
    // called to determine if the current MCBI value is different than the last-saved value, the
    // order of values in the value array will be consistent.
    var buttonValues;
    if (this.useInsertionOrder) {
        buttonValues = this._buttonValues.duplicate();
        if (this._isStoredValueReverseOfButtonValues()) {
            buttonValues.reverse();
        }
    } else {
        buttonValues = this._sortedButtonValues.duplicate();
    }

    if (this.deselectedButtonStyle != null && this._getShowDeletions()) {
        var valueToButtonMap = this._valueToButton;
        for (var ri = buttonValues.length; ri > 0; --ri) {
            var buttonValue = buttonValues[ri - 1],
                button = valueToButtonMap[buttonValue];
            if (button._deselected) {
                buttonValues.removeAt(ri - 1);
            }
        }
    }

    this.storeValue(buttonValues);

    if (oldPendingStatus && !!this.pendingStatus) this._updatePendingStyles();
},

// Disable loading display value

loadingDisplayValue:null,

// Create a button to represent a selected option having valueField <code>value</code>.
// @param value (any)
// @param displayValue (HTMLString)
_$button: "button",
_createButtonForValue : function (value, displayValue) {
    
    var dynamicProperties = {
        title: displayValue,
        width: 1,
        overflow: "hidden"
    };
    if (!this._getAutoFitButtons()) {
        dynamicProperties.width = this.canvas.getWidth();
    } else {
        dynamicProperties.overflow = "visible";
    }
    var button = this.createAutoChild(this._$button, dynamicProperties, isc.IButton);
    
    this._valueToButton[value] = button;
    button[isc.MultiComboBoxItem._$buttonDataAttribute] = value;
    return button;
},

_comboBoxItemChanged : function (comboForm, comboBoxItem) {
    var value = comboBoxItem.getValue();
    if (value == null || isc.isAn.emptyString(value)) return;

    if (!comboBoxItem._getOptionsFromDataSource()) {
        if (this._insertButtonForValue(value)) this._storeValue();
    } else {
        // A value has been selected using the combo box's pick list.  The pick list should
        // have the full record for the selected value, because it was able to display
        // the option to the user.  Copy that record to the displayField cache (maintained
        // by FormItem) so that it can provide the display value for the button title, and
        // offer data for use by the sort function.  FormItem's displayField cache is reused
        // so that we don't have to maintain a separate cache and we can use FormItem's
        // implementation of mapValueToDisplay() and getDisplayValue().
        var otherDFC = this.comboBox._displayFieldCache,
            valueField = this.getValueFieldName(),
            record = (otherDFC == null ? null : otherDFC.find(valueField, value));

        // Create a dummy record if addUnknownValues:true.
        //
        // If `record' is null and addUnknownValues is false, then `record' will be left as null,
        // and no button will be inserted for the value. Effectively, the value is rejected.
        if (record == null && this.addUnknownValues) {
            record = {};
            record[valueField] = value;
            var displayField = this.getDisplayFieldName();
            if (displayField) record[displayField] = value;
        }

        if (record != null) {
            var needsRefresh = this._addRecordToDisplayFieldCache(record);
            
            this.updateDisplayValueMap(false);

            if (this._insertButtonForValue(value)) this._storeValue();
        }
    }

    // Clear the combo box item's text field so that more entries can be added.
    comboBoxItem.clearValue();
},

_comboBoxItemFocusChanged : function (comboForm, comboBoxItem, hasFocus) {
    var form = this.form,
        focusCanvas = this.ns.EH.getFocusCanvas();
    
    if (focusCanvas != null && this.canvas.contains(focusCanvas, true)) {
        if (form.getFocusSubItem() !== this) {
            form.setFocusItem(this);
            this.elementFocus();
        }
    } else {
        this.elementBlur();
        if (form.getFocusSubItem() === this) form.setFocusItem(null);
    }
},

_isComboFormFirst : function () {
    var useInsertionOrder = this.useInsertionOrder,
        layoutStyle = this.layoutStyle;
    return (useInsertionOrder && layoutStyle == isc.MultiComboBoxItem.HORIZONTAL) ||
           layoutStyle == isc.MultiComboBoxItem.FLOW_REVERSE ||
           layoutStyle == isc.MultiComboBoxItem.HORIZONTAL_REVERSE ||
           layoutStyle == isc.MultiComboBoxItem.VERTICAL ||
           (useInsertionOrder && layoutStyle == isc.MultiComboBoxItem.VERTICAL_REVERSE);
},

// Convert an index of a button in `_buttonValues` or `_sortedButtonValues` to an index into
// `_buttonsLayout`.  The conversion depends on the layout style and the sort type (insertion
// order or by `sortOrder`).
_transformIndex : function (k, j, isInsert) {
    var useInsertionOrder = this.useInsertionOrder,
        layoutStyle = this.layoutStyle,
        numButtons = this._buttonValues.length,
        index;

    if (useInsertionOrder) {
        
        // The most recently added buttons appear next to the combo box.
        // In "flow" mode, new buttons appear at the highest index.
        // In the other modes, new buttons appear at index 0.
        // Note that although the order is reversed in "verticalReverse" (or in
        // "horizontalReverse"), the layout handles the reversal and so the
        // combo box is still said to be at index 0 (or numButtons).
        if (layoutStyle == isc.MultiComboBoxItem.FLOW || layoutStyle == isc.MultiComboBoxItem.FLOW_REVERSE) {
            index = numButtons - k - 1;
            if (isInsert) {
                ++index;
            }
        } else {
            
            index = k;
        }
    } else {
        
        index = j;
    }

    if (this._isComboFormFirst()) {
        // The combo box is at index zero and the buttons start at index 1.
        ++index;
    }
    return index;
},

// Converts `sortOrder` to a function of arguments that are values of `valueField`.
_getSortOrder : function () {
    var valueMap = this.getValueMap();
    if (valueMap == null && this.getOptionDataSource() != null) {
        // If the MultiComboBoxItem is data source driven then return a compare
        // function that will compare by displayField value.

        var self = this,
            valueField = this.getValueFieldName();

        return function (a, b) {
            var cache = self._displayFieldCache,
                recordA = (cache == null ? null : cache.find(valueField, a)),
                recordB = (cache == null ? null : cache.find(valueField, b));

            if (recordA == null || recordB == null) {
                var ods = self.getOptionDataSource(),
                    odsCacheData = (ods == null ? null : ods.getCacheData());
                if (odsCacheData != null) {
                    if (recordA == null) recordA = odsCacheData.find(valueField, a);
                    if (recordB == null) recordB = odsCacheData.find(valueField, b);
                }
            }

            return self.sortOrder.call(self, recordA, recordB);
        }
    } else {
        // If the MultiComboBoxItem is based on a static valueMap then use
        // a simple compare function.
        return isc.MultiComboBoxItem._defaultCompareFn;
    }
},

// Ensure that a button corresponding to the option with a valueField of <code>value</code> is
// in the _buttonsLayout.  This method will create such a button if necessary and insert it
// into the layout while maintaining a sorted order.
// @param value (any)
_insertButtonForValue : function (value) {
    var sortOrder = this._getSortOrder();

    // Determine the sorted index of `value` in `_sortedButtonValues`
    var j = isc.MultiComboBoxItem._binarySearch(
                value, this._sortedButtonValues, sortOrder),
        exists = (j >= 0),
        wasDeselected = false;

    // If the selected value is not the same as one already present,
    // then create a new button and add it.
    if (!exists) {
        j = -(j + 1); // the insertion index

        var displayValue = this.getDisplayValue(value),
            button = this._createButtonForValue(value, displayValue),
            index = this._transformIndex(0, j, true);

        this._buttonValues.unshift(value);
        this._sortedButtonValues.addAt(value, j);
        this._buttonsLayout.addMember(button, index);

    } else {
        var index = this._transformIndex(
                // Compute an index into _buttonValues only if necessary:
                !this.useInsertionOrder || this._buttonValues.indexOf(value), j, false);

        var existingButton = this._buttonsLayout.getMember(index);

        // "Flash" the existing button.
        if (!existingButton._deselected) {
            existingButton.animateFade(50, function () {
                existingButton.animateFade(100, null, 100);
            }, 100);

        } else {
            wasDeselected = true;

            existingButton._deselected = false;
            existingButton.enable();
            existingButton.setBaseStyle(this._getNormalButtonStyle());
            if (isc.Canvas.ariaEnabled()) existingButton.setAriaState("hidden", !existingButton.isVisible());
        }
    }

    return !exists || wasDeselected;
},

_buttonClick : function (button) {
    // ignore button clicks if the item itself is readOnly
    if (this.isReadOnly()) {
        return;
    }

    var buttonHadFocus = button.hasFocus,
        value = button[isc.MultiComboBoxItem._$buttonDataAttribute],
        sortOrder = this._getSortOrder(),
        j = isc.MultiComboBoxItem._binarySearch(value, this._sortedButtonValues, sortOrder),
        k = this._buttonValues.indexOf(value);
    

    var deselectedButtonStyle = this.deselectedButtonStyle;
    if (deselectedButtonStyle != null && this._getShowDeletions() && this._valuesSet.has(value)) {
        button.disable();
        button.setBaseStyle(deselectedButtonStyle);
        button._deselected = true;

    } else {
        // Remove the button from the layout
        delete this._valueToButton[value];
        var buttonsLayout = this._buttonsLayout;
        buttonsLayout.hideMember(button, {
            target: this,
            methodName: this._$finishedHidingButton,
            args: [buttonsLayout, button]
        });

        // Remove the record corresponding to the value from the FormItem's displayField cache.
        this._removeValueFromDisplayFieldCache(value);
        // We have to update the displayValueMap so a call to getValueMap() will no
        // longer return this value, and we'll be confused about whether or not we need
        // a fetch when attempting to redisplay this (now "unmapped" value)
        this.updateDisplayValueMap(false);
        
        this._buttonValues.splice(k, 1);
        this._sortedButtonValues.splice(j, 1);
    }
    // hide member can destroy the button, ensure we don't call this method on it
    // if it's already destroyed.
    if (isc.Canvas.ariaEnabled() && !button.destroyed) button.setAriaState("hidden", true);

    // Move the focus to a different button if the removed button had focus.
    if (buttonHadFocus) {
        var numButtons = this._buttonValues.length;
        if (numButtons > 0) {
            var nextButton;
            if (this.useInsertionOrder) {
                var nextButtonValueIndex = Math.max(0, k - 1),
                nextButton = this._valueToButton[this._buttonValues[nextButtonValueIndex]];
            } else {
                var nextSortedButtonValueIndex = Math.min(j, numButtons - 1),
                nextButton = this._valueToButton[this._sortedButtonValues[nextSortedButtonValueIndex]];
            }
            nextButton.focus();
        } else {
            this.comboBox.focusInItem();
        }
    }

    this._storeValue();
},

_buttonFocusChanged : function (button, hasFocus) {
    var form = this.form,
        focusCanvas = (hasFocus ? button : this.ns.EH.getFocusCanvas());
    if (focusCanvas != null && this.canvas.contains(focusCanvas, true)) {
        if (form.getFocusSubItem() !== this) {
            form.setFocusItem(this);
            this.elementFocus();
        }
    } else {
        this.elementBlur();
        if (form.getFocusSubItem() === this) form.setFocusItem(null);
    }
},

_$finishedHidingButton: "_finishedHidingButton",
_finishedHidingButton : function (buttonsLayout, button) {
    buttonsLayout.removeMember(button, true);
    button.destroy();
},

_$finishedHidingButtons: "_finishedHidingButtons",
_finishedHidingButtons : function (buttonsLayout, buttons) {
    buttonsLayout.removeMembers(buttons, true);
    buttons.map("destroy");
},

// Clears the _buttonsLayout and sets the value of the MultiComboBoxItem to an empty array.
_removeAllButtons : function () {
    var layout = this._buttonsLayout;

    if (this.layoutStyle === isc.MultiComboBoxItem.FLOW || this.layoutStyle == isc.MultiComboBoxItem.FLOW_REVERSE) {
        var tiles;
        if (this._isComboFormFirst()) {
            // The combo box item is at index zero.
            
            tiles = layout.tiles.slice(1);
        } else {
            // The combo box item is last.
            
            tiles = layout.tiles.slice(0, -1);
        }

        for (var ri = tiles.length; ri > 0; --ri) {
            var button = tiles[ri - 1];
            
            layout.removeTile(button);
            button.destroy();
        }
    } else {
        var buttons;
        if (this._isComboFormFirst()) {
            // The combo box item is at index zero.
            buttons = layout.members.slice(1);
        } else {
            // The combo box item is last.
            buttons = layout.members.slice(0, -1);
        }
        
        layout.removeMembers(buttons, true);
        buttons.map("destroy");
    }

    // Clear out the FormItem's displayField cache.
    if (!this.isDrawn()) {
        this.invalidateDisplayValueCache(false);
    }

    this._valueToButton = {};
    this._buttonValues = [];
    this._sortedButtonValues = [];
    this._unavailableButtonValues = [];
},

// Override drawn() here to re-invoke showValue() with arguments that were passed to that
// function before the MultiComboBoxItem was drawn.
drawn : function () {
    var ret = this.Super("drawn", arguments),
        undef;
    if (this._dataValue !== undef) {
        var displayValue = this.getDisplayValue(this._dataValue);
        this.showValue(displayValue, this._dataValue, this.form, this);
        delete this._dataValue;
    }
    return ret;
},

// When this item is redrawn, re-apply ARIA state attributes on the embedded comboBox item.
// If the title of the MultiComboBoxItem is changed, we will want to update the 'aria-label'
// attribute on the comboBox.
redrawn : function () {
    var ret = this.Super("redrawn", arguments);
    var comboBox = this.comboBox;
    if (comboBox != null && isc.screenReader) comboBox.addContentRoles();
    return ret;
},

//> @method MultiComboBoxItem.showValue()
// This method will be called whenever this FormItem's value is being set via a programmatic
// call to e.g: +link{dynamicForm.setValues()} or +link{formItem.setValue()} and may be
// overridden by CanvasItems intended to support displaying data values to update the
// embedded Canvas to reflect the value passed in.
// <p>
// The value of a MultiComboBoxItem to the form is an array of valueField values corresponding
// to the selected combo box options.
// @include CanvasItem.showValue()
// @visibility external
//<
showValue : function (displayValue, dataValue, form, item) {

    if (!this.isDrawn()) {
        this._dataValue = dataValue;
        
    } else if (!dataValue) {
        this._removeAllButtons();
    } else if (isc.isAn.Array(dataValue)) {
        
        this._removeAllButtons();
        for (var i = 0, dataValueLength = dataValue.length; i < dataValueLength; ++i) {
            var value = dataValue[i];
          
            if (this._isValueInCache(value)) {
                this._insertButtonForValue(value, displayValue[i]);
            } else {
                this._unavailableButtonValues.push(value);
            }
        }
    }

    
},

// Checks whether the display value corresponding to the field value <code>value</code> has
// been fetched from the data source and is available.
_isValueInCache : function (value) {
    // _mapKey() is an internal method of FormItem.  The second argument set to true forces
    // _mapKey() to return null if the value is not in the valueMap.
    return (!this.optionDataSource || (this._mapKey(value, true) !== null));
},

_refreshForDisplayValueChange : function () {
    return this.comboBox._refreshForDisplayValueChange.apply(this.comboBox, arguments);
},

// Override FormItem.setValueMap() to set the valueMap on both this form item and the combo box.
setValueMap : function (valueMap) {
    this.Super("setValueMap", arguments);
    this.comboBox.setValueMap.apply(this.comboBox, arguments);
},

updateValueMap : function (refreshDisplay) {
    // Call super, but suppress any call to 'setElementValue' which falls through to
    // showValue - we'll refresh our display info if appropriate below
    this.Super("updateValueMap", false);

    if (refreshDisplay) {
        // Because the valueMap changed, we need to ensure that the button titles reflect the current
        // display values, reconstruct `_sortedButtonValues` according to the new data, and, if we
        // are not in insertion order mode, rearrange the buttons according to the new sorted order.
        var sortOrder = this._getSortOrder(),
            autoFitButtons = this._getAutoFitButtons(),
            useInsertionOrder = this.useInsertionOrder,
            numButtons = this._buttonValues.length;

        var width100 = this.canvas.getWidth();

        var sortedButtonValues = [];
        for (var i = 0; i < numButtons; ++i) {
            var value = this._buttonValues[i],
                displayValue = this.getDisplayValue(value),
                j = isc.MultiComboBoxItem._binarySearch(
                        value, sortedButtonValues, sortOrder),
                button = this._valueToButton[value];

            button.setTitle(displayValue);

            // Refresh the autoFit of the button
            if (!autoFitButtons) {
                button.setWidth(width100);
            }
            button.setAutoFit(autoFitButtons);

            sortedButtonValues.addAt(value, -(j + 1));
        }
        var oldSortedButtonValues = this._sortedButtonValues;
        this._sortedButtonValues = sortedButtonValues;

        // If the sorted order of the values changes, then rearrange the corresponding buttons
        // into sorted order.
        if (!useInsertionOrder) {
            var buttonsLayout = this._buttonsLayout,
                undef;
            for (var i = 0; i < numButtons; ++i) {
                var value = oldSortedButtonValues[i],
                    j = sortedButtonValues.indexOf(value);

                if (i != j) {
                    buttonsLayout.reorderMember(
                            this._transformIndex(undef, i, false),
                            this._transformIndex(undef, j, false));
                }
            }
        }

        // The fetchMissingValues feature ultimately calls this method after it receives
        // display values fetched from the server.  Those display values may be available
        // here so use them to create buttons corresponding to selected values.
        if (!this._unavailableButtonValues.isEmpty()) {
            var len = this._unavailableButtonValues.length,
                offset = 0;

            for (var i = 0; i < len; ++i) {
                var j = i - offset, value = this._unavailableButtonValues[j];

                if (this._isValueInCache(value)) {
                    var displayValue = this.getDisplayValue(value);
                    this._insertButtonForValue(value, displayValue);
                    this._unavailableButtonValues.splice(j, 1);
                    ++offset;
                }
            }
        }
    }
},

//> @method MultiComboBoxItem.setLayoutStyle()
// @param layoutStyle (MultiComboBoxLayoutStyle) the new layout style
// @see layoutStyle
// @visibility external
//<
setLayoutStyle : function (layoutStyle) {
    var oldLayoutStyle = this.layoutStyle;

    if (!layoutStyle || (oldLayoutStyle == layoutStyle)) {
        return;
    }

    // Some information that is used throughout this method:
    // When useInsertionOrder is true the HORIZONTAL, HORIZONTAL_REVERSE, VERTICAL, and
    // VERTICAL_REVERSE layout styles all have the combo box item at index 0.  HORIZONTAL
    // and VERTICAL_REVERSE display the combo box at the last index (at numButtons), so
    // these both use Layout.reverseOrder to flip the buttons and combo box.
    // In FLOW mode or if useInsertionOrder is false then the buttons are at the index
    // at which they are displayed so reversing the order of the buttons and combo box
    // is achieved by actually moving the buttons around in the layout (or moving them
    // in reverse order to another layout).

    var FLOW = isc.MultiComboBoxItem.FLOW,
        FLOW_REVERSE = isc.MultiComboBoxItem.FLOW_REVERSE,
        HORIZONTAL = isc.MultiComboBoxItem.HORIZONTAL,
        HORIZONTAL_REVERSE = isc.MultiComboBoxItem.HORIZONTAL_REVERSE,
        VERTICAL = isc.MultiComboBoxItem.VERTICAL,
        VERTICAL_REVERSE = isc.MultiComboBoxItem.VERTICAL_REVERSE,
        transferMember = isc.MultiComboBoxItem._transferMember,
        _transferMembers = isc.MultiComboBoxItem._transferMembers,
        _transferMembersReverse = isc.MultiComboBoxItem._transferMembersReverse,

        useInsertionOrder = this.useInsertionOrder,
        comboBoxWidth = this.comboBoxWidth,
        dontAnimate = true,
        numButtons = this._buttonValues.length,
        oldReverseOrder = oldLayoutStyle == HORIZONTAL ||
                          oldLayoutStyle == FLOW_REVERSE ||
                          oldLayoutStyle == VERTICAL_REVERSE,
        reverseOrder = isc.MultiComboBoxItem.isOrderReversedForStyle(layoutStyle),

        // Used only if oldLayoutStyle or layoutStyle is FLOW
        transferMembers = (useInsertionOrder ? _transferMembersReverse : _transferMembers);

    if (layoutStyle == FLOW || layoutStyle == FLOW_REVERSE) {

        if (!isc.FlowLayout) {
            // If the FlowLayout class is unavailable then log a message and keep the old
            // layout style.

            isc.Log.logWarn(
                "The ISC_Grids module must be loaded to use layoutStyle:\"flow\".  " +
                "The MultiComboBoxItem will continue to use the \"" + oldLayoutStyle + "\"" +
                " layout style.");
            return;
        }

        // "flow" uses FlowLayout instead of HStack or VStack for _buttonsLayout, so a
        // FlowLayout will need to be created and the buttons and combo box transferred
        // to it.  In insertion order mode the buttons are reversed as they are transfered
        // to the FlowLayout.

        var buttonsLayout = this._createFlowLayout(),
            animateTileChange = buttonsLayout.animateTileChange;

        buttonsLayout.animateTileChange = false;

        transferMember(this._buttonsLayout,
            (!useInsertionOrder && oldReverseOrder ? numButtons : 0),
            buttonsLayout, 0, dontAnimate);
        transferMembers(
                numButtons, this._buttonsLayout, 0, buttonsLayout, 0, dontAnimate);

        this.canvas.removeMember(0, dontAnimate);
        this._buttonsLayout.destroy();
        this._buttonsLayout = buttonsLayout;
        this.canvas.addMember(buttonsLayout, 0, dontAnimate);

    } else if (layoutStyle == HORIZONTAL || layoutStyle == HORIZONTAL_REVERSE ||
               layoutStyle == VERTICAL || layoutStyle == VERTICAL_REVERSE)
    {
        // The "horizontal" and "horizontalReverse" layout styles use an HStack for
        // _buttonsLayout, while "vertical" and "verticalReverse" use a VStack.
        // If the type of layout is different between the old layout style and the new
        // layout style, then a new HStack or VStack will need to be created and all of
        // the buttons will need to be moved into it.

        var isHorizontal = (layoutStyle == HORIZONTAL || layoutStyle == HORIZONTAL_REVERSE),
            oldIsHorizontal = (oldLayoutStyle == HORIZONTAL ||
                               oldLayoutStyle == HORIZONTAL_REVERSE),
            createLayout = (isHorizontal ? this._createHStack : this._createVStack);

        if (oldLayoutStyle == FLOW || oldLayoutStyle == FLOW_REVERSE || (isHorizontal != oldIsHorizontal))
        {
            var buttonsLayout = createLayout.call(this);
            if (oldLayoutStyle == FLOW || oldLayoutStyle == FLOW_REVERSE) {
                this._buttonsLayout.animateTileChange = false;
                transferMember(this._buttonsLayout, numButtons, buttonsLayout, 0, dontAnimate);
                transferMembers(
                        numButtons, this._buttonsLayout, 0, buttonsLayout,
                        (!useInsertionOrder && reverseOrder ? 0 : 1),
                        dontAnimate);
            } else if (useInsertionOrder || (oldReverseOrder == reverseOrder)) {
                // Move everything in the same order.
                _transferMembers(
                        numButtons+1, this._buttonsLayout, 0, buttonsLayout, 0, dontAnimate);
            } else {
                // Move the combo box from back to front, or front to back, and move the
                // buttons in order.
                transferMember(this._buttonsLayout, (oldReverseOrder ? numButtons : 0),
                        buttonsLayout, 0, dontAnimate);
                _transferMembers(numButtons, this._buttonsLayout, 0,
                        buttonsLayout, (oldReverseOrder ? 1 : 0), dontAnimate);
            }
            this.canvas.removeMember(this._buttonsLayout, dontAnimate);
            this._buttonsLayout.destroy();
            this._buttonsLayout = buttonsLayout;
            this.canvas.addMember(buttonsLayout, 0, dontAnimate);
            if (useInsertionOrder && reverseOrder) {
                this._buttonsLayout.reverseOrder = true;
                this._buttonsLayout.reflow();
            }
        } else if (useInsertionOrder) {
            // Reverse the order of the members if necessary
            if (reverseOrder != oldReverseOrder) {
                this._buttonsLayout.reverseOrder = reverseOrder;
                this._buttonsLayout.reflow();
            }
        } else {
            

            // Under sorted order, just the combo box needs to be moved.
            if (!reverseOrder) {
                transferMember(
                        this._buttonsLayout, numButtons, this._buttonsLayout, 0, dontAnimate);
            } else {
                transferMember(
                        this._buttonsLayout, 0, this._buttonsLayout, numButtons, dontAnimate);
            }
        }
    } else {
        return;
    }

    this.layoutStyle = layoutStyle;

    // Set the width of the combo box to the full field width in VERTICAL and VERTICAL_REVERSE
    // modes, or to comboBoxWidth in FLOW, HORIZONTAL, and HORIZONTAL_REVERSE modes.
    this.comboForm.setWidth(layoutStyle == VERTICAL || layoutStyle == VERTICAL_REVERSE
                            ? "100%" : comboBoxWidth);

    // Set the autoFit on the buttons.
    // The autoFitButtons property can change with the layout
    // style if the user did not explicitly provide a value for the
    // autoFitButtons attribute and so it assumes a default value according to
    // the current layout style.
    // `autoFit` is set in any case to make sure that the buttons have the correct width.
    this._setAutoFitButtons(this._getAutoFitButtons(layoutStyle));

    var undef;
    if (animateTileChange !== undef) {
        buttonsLayout.animateTileChange = animateTileChange;
    }
},

destroy : function () {
    if (this.comboForm) this.comboForm.destroy();
    if (this._buttonsLayout) this._buttonsLayout.destroy();
    this.Super("destroy", arguments);
},

handleEditorExit : function() {
    
    var originalSuppressValidateOnEditorExit = this._suppressValidateOnEditorExit;
    this._suppressValidateOnEditorExit = true;
    var returnVal = this.Super("handleEditorExit", arguments);
    this._suppressValidateOnEditorExit = originalSuppressValidateOnEditorExit;
    return returnVal;
},

_setAutoFitButtons : function (autoFitButtons) {
    var i = (this.layoutStyle == isc.MultiComboBoxItem.FLOW ||
             (!this.useInsertionOrder &&
              (this.layoutStyle == isc.MultiComboBoxItem.HORIZONTAL ||
               this.layoutStyle == isc.MultiComboBoxItem.VERTICAL_REVERSE)) ? 0 : 1),
        numButtons = this._buttonValues.length,
        imax = i + numButtons,
        width100 = this.canvas.getWidth();

    for (; i < imax; ++i) {
        var button = this._buttonsLayout.getMember(i);

        if (!autoFitButtons) {
            button.setWidth(width100);
        }
        button.setAutoFit(autoFitButtons);
    }
},

//> @method MultiComboBoxItem.setAutoFitButtons
// Sets the +link{autoFitButtons} property.
// @param autoFitButtons (boolean) whether to autofit the buttons
// @visibility external
//<
setAutoFitButtons : function (autoFitButtons) {
    var oldAutoFitButtons = this._getAutoFitButtons(),
        newAutoFitButtons = this._getAutoFitButtons(null, autoFitButtons);

    this.autoFitButtons = autoFitButtons;

    if (oldAutoFitButtons != newAutoFitButtons) {
        this._setAutoFitButtons(newAutoFitButtons);
    }
},

_createCanvas : function () {
    if ( (this.layoutStyle == isc.MultiComboBoxItem.FLOW ||
          this.layoutStyle == isc.MultiComboBoxItem.FLOW_REVERSE) && !isc.FlowLayout) {
        // If isc.FlowLayout is not available then the MultiComboBoxItem cannot have the "flow"
        // layout style.  In this case the layout style will default to "verticalReverse".
        // This is possible because "widgets/TileLayout" is not listed as a dependency of this
        // package, so the FlowLayout class may not have been loaded.

        isc.Log.logWarn(
                "The ISC_Grids module must be loaded to use layoutStyle:\"flow\".  " +
                "The MultiComboBoxItem will default to using the " +
                "\"" + isc.MultiComboBoxItem.VERTICAL_REVERSE + "\" layout style.");
        this.layoutStyle = isc.MultiComboBoxItem.VERTICAL_REVERSE;
    }

    var comboBoxWidth;
    if (this.layoutStyle == isc.MultiComboBoxItem.FLOW ||
        this.layoutStyle == isc.MultiComboBoxItem.FLOW_REVERSE ||
        this.layoutStyle == isc.MultiComboBoxItem.HORIZONTAL ||
        this.layoutStyle == isc.MultiComboBoxItem.HORIZONTAL_REVERSE)
    {
        comboBoxWidth = this.comboBoxWidth;
    } else {
        comboBoxWidth = "100%";
    }

    
    var comboBoxProperties = isc.addProperties({ ID: this.ID + isc._underscore + "comboBox" }, this.comboBoxDefaults, this.comboBoxProperties, {
        creator: this,
        optionDataSource: this.optionDataSource,
        autoFetchData: this.autoFetchData,
        valueMap: this.valueMap,
        displayField: this.displayField,
        valueField: this.valueField,
        addUnknownValues: this.addUnknownValues,
        changeOnKeypress: !this.addUnknownValues
    });
    var comboForm = this.addAutoChild("comboForm", {
        width: comboBoxWidth,
        items: [comboBoxProperties],
        visibility: this.renderAsReadOnly() || this.renderAsStatic() ? isc.Canvas.HIDDEN : isc.Canvas.INHERIT,
        readOnlyDisplay: this.getReadOnlyDisplay(),
        canEdit: this.getCanEdit()
    });
    var comboBox = this.comboBox = comboForm.getItem(0);

    // Set _buttonsLayout according the the value of layoutStyle.
    // The layout has initial height:1 so that the MultiComboBoxItem's height will be the minimum
    // height required to fit the buttonsLayout.
    var canvasProperties = isc.addProperties({}, this.canvasDefaults, this.canvasProperties, {
        autoDraw: false,
        height: 1
    });

    var reverseOrder = isc.MultiComboBoxItem.isOrderReversedForStyle(this.layoutStyle);
    if (this.layoutStyle == isc.MultiComboBoxItem.HORIZONTAL ||
        this.layoutStyle == isc.MultiComboBoxItem.HORIZONTAL_REVERSE) {

        this._buttonsLayout = this._createHStack();
        this._buttonsLayout.reverseOrder = reverseOrder;

    } else if (this.layoutStyle == isc.MultiComboBoxItem.VERTICAL ||
               this.layoutStyle == isc.MultiComboBoxItem.VERTICAL_REVERSE)
    {
        this._buttonsLayout = this._createVStack();
        this._buttonsLayout.reverseOrder = reverseOrder;

    } else if (this.layoutStyle == isc.MultiComboBoxItem.FLOW || this.layoutStyle == isc.MultiComboBoxItem.FLOW_REVERSE) {
        this._buttonsLayout = this._createFlowLayout();
        this._buttonsLayout.reverseOrder = reverseOrder;
    }
    this._buttonsLayout.addMember(comboForm);
    canvasProperties.members = [this._buttonsLayout];

    this.canvas = isc.HLayout.create(canvasProperties);
    this.Super("_createCanvas", arguments);
},

_getNormalButtonStyle : function () {
    var buttonProperties,
        baseStyle;
    if ((buttonProperties = this.buttonProperties) != null &&
        (baseStyle = buttonProperties.baseStyle) != null)
    {
        return baseStyle;
    } else if ((buttonProperties = this.buttonDefaults) != null &&
               (baseStyle = buttonProperties.baseStyle) != null)
    {
        return baseStyle;
    } else {
        var buttonClass = isc.ClassFactory.getClass(this.getAutoChildClass(this._$button, isc.IButton));
        
        return buttonClass._instancePrototype.baseStyle;
    }
},

_updatePendingStyles : function () {
    if (!this._getShowPending()) return;

    var pendingButtonStyle = this.pendingButtonStyle;
    if (pendingButtonStyle == null) return;

    var buttonStyle = this._getNormalButtonStyle();
    if (pendingButtonStyle === buttonStyle) return;

    var buttonsLayout = this._buttonsLayout;
    if (!this.pendingStatus) {
        if (this.useInsertionOrder) {
            var buttonValues = this._buttonValues,
                numButtonValues = buttonValues.length;
            for (var k = 0; k < numButtonValues; ++k) {
                buttonsLayout.getMember(this._transformIndex(k, false, false)).setBaseStyle(buttonStyle);
            }
        } else {
            var sortedButtonValues = this._sortedButtonValues,
                numSortedButtonValues = sortedButtonValues.length;
            for (var j = 0; j < numSortedButtonValues; ++j) {
                buttonsLayout.getMember(this._transformIndex(false, j, false)).setBaseStyle(buttonStyle);
            }
        }
    } else {
        var oldValue = this._getOldValue();

        var numOldValues;
        if (oldValue == null) numOldValues = 0;
        else if (!isc.isAn.Array(oldValue)) {
            oldValue = [oldValue];
            numOldValues = 1;
        } else {
            numOldValues = oldValue.length;
        }

        if (this.useInsertionOrder) {
            var buttonValues = this._buttonValues,
                numButtonValues = buttonValues.length,
                isStoredValueReverse = this._isStoredValueReverseOfButtonValues();
            var i = 0,
                k;
            for (; i < numOldValues && i < numButtonValues; ++i) {
                k = isStoredValueReverse ? numButtonValues - 1 - i : i;
                if (!isc.DynamicForm.compareValues(oldValue[i], buttonValues[k])) break;
                var existingButton = buttonsLayout.getMember(this._transformIndex(k, false, false));
                if (!existingButton._deselected) existingButton.setBaseStyle(buttonStyle);
            }
            for (; i < numButtonValues; ++i) {
                k = isStoredValueReverse ? numButtonValues - 1 - i : i;
                buttonsLayout.getMember(this._transformIndex(k, false, false)).setBaseStyle(pendingButtonStyle);
            }

        // The old values and the sorted button values should both be sorted using the sort function
        // returned by this._getSortOrder().
        } else {
            var sortedButtonValues = this._sortedButtonValues,
                numSortedButtonValues = sortedButtonValues.length,
                compareFn = this._getSortOrder();
            var i = 0,
                j = 0;
            while (i < numOldValues && j < numSortedButtonValues) {
                var comparison = compareFn(oldValue[i], sortedButtonValues[j]);
                if (comparison < 0) {
                    // The button for oldValue[i] was removed.
                    ++i;
                } else if (comparison == 0) {
                    // The button for oldValue[i] is still present. It may be a deselected button, though.
                    // If so, leave it alone.
                    var existingButton = buttonsLayout.getMember(this._transformIndex(false, j, false));
                    if (!existingButton._deselected) existingButton.setBaseStyle(buttonStyle);
                    ++i;
                    ++j;
                } else {
                    // The button for sortedButtonValues[j] is new.
                    buttonsLayout.getMember(this._transformIndex(false, j, false)).setBaseStyle(pendingButtonStyle);
                    ++j;
                }
            }
            // Any remaining buttons are new.
            for (; j < numSortedButtonValues; ++j) {
                buttonsLayout.getMember(this._transformIndex(false, j, false)).setBaseStyle(pendingButtonStyle);
            }
        }
    }
},

//> @method multiComboBoxItem.pendingStatusChanged()
// Notification method called when +link{FormItem.showPending,showPending} is enabled and this
// <code>MultiComboBoxItem</code> should either clear or show its pending visual state.
// <p>
// The default behavior is that the +link{FormItem.titleStyle,titleStyle} and
// +link{FormItem.cellStyle,cellStyle} are updated to include/exclude the "Pending" suffix.
// In addition, when displayed in the pending state and a
// +link{attr:pendingButtonStyle,pendingButtonStyle} is set, then:
// <ul>
// <li>If +link{attr:useInsertionOrder,useInsertionOrder} is <code>false</code>, buttons for
//     any new values will have their +link{Button.baseStyle,baseStyle} set to
//     <code>pendingButtonStyle</code>; otherwise
// <li>(<code>useInsertionOrder</code> is <code>true</code>) buttons for values will have their
//     +link{Button.baseStyle,baseStyle} set to <code>pendingButtonStyle</code> if either the
//     value is new or it is in a different place within the value array.
// </ul>
// Returning <code>false</code> will cancel this default behavior.
// @include FormItem.pendingStatusChanged()
//<
_pendingStatusChanged : function (pendingStatus) {
    // When no longer pending, set the valuesSet to the now-saved values. This allows us to
    // determine which button values are deleted.
    if (!pendingStatus) {
        var valuesSet = this._valuesSet;
        valuesSet.clear();

        var value = this._value;
        if (isc.isAn.Array(value)) {
            var numValues = value.length;
            for (var i = 0; i < numValues; ++i) {
                valuesSet.add(value[i]);
            }
        } else {
            valuesSet.add(value);
        }

        // Purge any deselected buttons.
        if (this.deselectedButtonStyle != null && this._getShowDeletions()) {
            var buttonsToRemove = [],
                buttonsLayout = this._buttonsLayout;
            if (this.layoutStyle === isc.MultiComboBoxItem.FLOW || this.layoutStyle === isc.MultiComboBoxItem.FLOW_REVERSE) {
                var tiles = buttonsLayout.tiles,
                    numTiles = tiles.length;
                
                for (var ri = numTiles - 1; ri > 0; --ri) {
                    var button = tiles[ri - 1];
                    
                    if (button._deselected) buttonsToRemove.add(button);
                }
            } else {
                var buttons;
                if (this._isComboFormFirst()) {
                    // The combo box item is at index zero.
                    buttons = buttonsLayout.members.slice(1);
                } else {
                    // The combo box item is last.
                    buttons = buttonsLayout.members.slice(0, -1);
                }
                
                for (var i = 0, numButtons = buttons.length; i < numButtons; ++i) {
                    var button = buttons[i];
                    if (button._deselected) buttonsToRemove.add(button);
                }
            }

            var numButtonsToRemove = buttonsToRemove.length;
            if (numButtonsToRemove > 0) {
                buttonsLayout.hideMembers(buttonsToRemove, {
                    target: this,
                    methodName: this._$finishedHidingButtons,
                    args: [buttonsLayout, buttonsToRemove]
                });

                var sortOrder = this._getSortOrder(),
                    valueToButton = this._valueToButton;
                for (var i = 0; i < numButtonsToRemove; ++i) {
                    var button = buttonsToRemove[i],
                        value = button[isc.MultiComboBoxItem._$buttonDataAttribute],
                        j = isc.MultiComboBoxItem._binarySearch(value, this._sortedButtonValues, sortOrder),
                        k = this._buttonValues.indexOf(value);

                    delete valueToButton[value];

                    // Remove the record corresponding to the value from the FormItem's displayField cache.
                    this._removeValueFromDisplayFieldCache(value);
                    this.updateDisplayValueMap(false);
                    
                    this._buttonValues.splice(k, 1);
                    this._sortedButtonValues.splice(j, 1);
                }
            }
        }
    }
    this.Super("_pendingStatusChanged", arguments);
},
_defaultPendingStatusChangedBehavior : function (pendingStatus) {
    this.Super("_defaultPendingStatusChangedBehavior", arguments);
    this._updatePendingStyles();
},

_canEditChanged : function (canEdit) {
    if ((this.canEditChanged == null || this.canEditChanged(canEdit) != false) &&
        this.canvas != null)
    {
        
        this.comboForm.setCanEdit(canEdit);
        this.comboForm.setVisibility(this.renderAsReadOnly() || this.renderAsStatic() ? isc.Canvas.HIDDEN : isc.Canvas.INHERIT);

        // Disable all of the buttons
        this.canvas.setDisabled(this.shouldDisableCanvas());
    }
},

_readOnlyDisplayChanged : function (appearance) {
    if ((this.readOnlyDisplayChanged == null || this.readOnlyDisplayChanged(appearance) != false) &&
        this.canvas != null)
    {
        
        this.comboForm.setReadOnlyDisplay(appearance);
        this.comboForm.setVisibility(this.renderAsReadOnly() || this.renderAsStatic() ? isc.Canvas.HIDDEN : isc.Canvas.INHERIT);
    }
},

// Returns the effective setting of <code>autoFitButtons</code> considering the current
// layout style
_getAutoFitButtons : function (layoutStyle, autoFitButtons) {
    var autoFitButtons = autoFitButtons || this.autoFitButtons,
        layoutStyle = layoutStyle || this.layoutStyle;

    if (layoutStyle == isc.MultiComboBoxItem.HORIZONTAL ||
        layoutStyle == isc.MultiComboBoxItem.HORIZONTAL_REVERSE)
    {
        return true;

    } else if (this.autoFitButtons != null) {
        return this.autoFitButtons;

    } else if (layoutStyle == isc.MultiComboBoxItem.FLOW || this.layoutStyle === isc.MultiComboBoxItem.FLOW_REVERSE) {
        return true;
    } else {
        
        return false;
    }
},

_createHStack : function () {
    return isc.HStack.create(this.valueLayoutDefaults, this.valueLayoutProperties, {
        creator: this,
        autoDraw: false,
        canFocus: false,
        canHover: true,
        height: 1,
        members: [],
        animateMembers: true,
        animateMemberTime: 100,

        hover : function () {
            this.creator._handleHover();
            return false;
        }
    });
},

_createVStack : function () {
    return isc.VStack.create(this.valueLayoutDefaults, this.valueLayoutProperties, {
        creator: this,
        autoDraw: false,
        canFocus: false,
        canHover: true,
        height: 1,
        members: [],
        animateMembers: true,
        animateMemberTime: 100,

        hover : function () {
            this.creator._handleHover();
            return false;
        }
    });
},

_createFlowLayout : function () {
    return isc.FlowLayout.create(this.valueLayoutDefaults, this.valueLayoutProperties, {
        creator: this,
        autoDraw: false,
        canFocus: false,
        canHover: true,
        tiles: [],
        tileMargin: 0,
        layoutMargin: 0,
        height: 1,

        animateTileChange: false,

        // A "visible" overflow expands the layout size instead of introducing scrollbars.
        overflow: "visible",

        // isc.FlowLayout is missing member functions that are available in other layouts:
        // getMember(), getMemberNumber(), getMembers(), addMember(), removeMember(), and
        // reorderMember(), so implement them here.

        getMember : function (position) {
            return this.getTile(position);
        },

        getMemberNumber : function (member) {
            if (isc.isA.Number(member)) {
                return member;
            }

            var numTiles = this.getLength();
            for (var i = 0; i < numTiles; ++i) {
                if (member === this.getTile(i)) {
                    return i;
                }
            }
            return -1;
        },

        getMembers : function () {
            var tiles = [], length = this.getLength();
            for (var i = 0; i < length; ++i) {
                var tile = this.getTile(i);
                tiles.add(tile);
            }
            return tiles;
        },

        addMember : function (newMember, position) {
            this.addTile(newMember, position);

            // The `autoFit: true` setting on a new button can cause the flow layout to
            // render the new button on top of a button in the same row.  Laying out
            // the tiles again fixes the issue.
            this.layoutTiles();
        },

        removeMember : function (member) {
            this.removeTile(member);
        },
        removeMembers : function (members) {
            var numMembers = members.length;
            for (var i = 0; i < numMembers; ++i) {
                this.removeTile(members[i]);
            }
        },

        hideMember : function (member, callback) {
            member.hide();
            this.fireCallback(callback);
        },
        hideMembers : function (members, callback) {
            members.map("hide");
            this.fireCallback(callback);
        },

        reorderMember : function (memberNum, newPosition) {
            if (memberNum != newPosition) {
                var tile = this.getTile(memberNum);
                this.removeMember(tile);
                this.addMember(tile, newPosition);
            }
        },

        hover : function () {
            this.creator._handleHover();
            return false;
        }
    });
}
});
