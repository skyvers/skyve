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
// MultiComboBoxItem uses the +link{AutoChild} pattern to construct the combo box and the buttons
// so that they can be easily customized.
// @see ComboBoxItem
// @inheritsFrom CanvasItem
// @example multiComboBoxItem
// @visibility comboBox
//<
isc.ClassFactory.defineClass("MultiComboBoxItem", "CanvasItem");

isc.MultiComboBoxItem.addClassProperties({
//> @type MultiComboBoxLayoutStyle
// Specifies the layout of the combo box and buttons in a MultiComboBoxItem.
// @value "flow" Use a flow layout
FLOW: "flow",
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

// _binarySearch() returns the lowest index in <code>values</code> at which <code>value</code>
// may be inserted without breaking the sort order of <code>values</code>, as induced by
// <code>compareFn</code>.
//
// This function assumes that <code>values</code> are already sorted by <code>compareFn</code>.
// @param value (any) the value to search for.
// @param values (Array of any) an array of values in which to search.
// @param [compareFn] (Function) a comparator function used to compare values in
// <code>values</code> with <code>value</code>.  <code>compareFn</code> is called with two
// arguments. The first is a value from <code>values</code> and the second is always
// <code>value</code>.  <code>compareFn</code> defaults to
// MultiComboBoxItem._defaultCompareFn() if it is not specified.
// @param [strict] (boolean) Should this function search for identical values to <code>value</code>
// (via ===), or is a zero of <code>compareFn</code> sufficient for determining equality?  The
// default value is true.  Note that compareFn must return zero when passed identically equal
// values for this function to work correctly.
// @return (integer) If <code>value</code> is in <code>values</code> then this function returns
// the index of <code>value</code> in the array.  Otherwise, the return value is
// <code>-(insertion index)-1</code>, where the insertion index is the lowest index at which
// <code>value</code> could be inserted into <code>values</code> while maintaining the sort order.
_binarySearch : function (value, values, compareFn, strict) {
    if (!compareFn) {
        compareFn = isc.MultiComboBoxItem._defaultCompareFn;
    }
    if (strict === undefined) {
        strict = true;
    }

    var low = 0, len = values.length, high = len - 1;
    var i = 0, comparison;
    while (low <= high) {
        i = Math.floor((low + high) / 2);
        comparison = compareFn(values[i], value);
        if (comparison < 0) {
            low = i + 1;
        } else if (comparison > 0) {
            high = i - 1;
        } else {
            // `comparison` == 0

            // `values[i]` equals `value` according to the compare function. However,
            // it may be that `i` is in the middle of a range of equal values. Keep
            // decrementing `i` until it is the lowest index of that range.
            // If `strict` is true then we are actually looking to return the index of an
            // identically equal value in the `values` array.

            if (strict) {
                var j = i;
                while (j < len && compareFn(values[j], value) == 0) {
                    if (values[j] === value) {
                        return j;
                    }
                    ++j;
                }
            }

            while (i > 0 && compareFn(values[i - 1], value) == 0) {
                if (strict && values[i - 1] === value) {
                    return i - 1;
                }
                --i;
            }

            // i is the insertion index, so return -(insertion index)-1
            return -i-1;
        }
    }

    // Return the lowest index such that `values` at that index is greater than `value`.  That
    // is the index at which `value` could be inserted while maintaining sort order.
    // The actual return value is -(insertion index)-1, so that callers can know whether
    // the value was in the `values` array.
    if (comparison !== undefined && comparison < 0) {
        // values[i] < value, so i + 1 is the correct index.
        return -(i + 1)-1;
    } else {
        // values[i] > value, so i is the correct index.
        return -i-1;
    }
},

// Default comparator function used by _binarySearch() if <code>compareFn</code> is not provided.
// @param lhs (any)
// @param rhs (any)
// @return (number) -1 if <code>lhs</code> is less than <code>rhs</code>, 0 if <code>lhs</code>
// and <code>rhs</code> are equal, or 1 if
// <code>lhs</code> is greater than <code>rhs</code>
_defaultCompareFn : function (lhs, rhs) {
    if (lhs < rhs) {
        return -1;
    } else if (lhs > rhs) {
        return 1;
    } else {
        return 0;
    }
},

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
           layoutStyle == isc.MultiComboBoxItem.VERTICAL_REVERSE;
}
});

isc.MultiComboBoxItem.addProperties({

//> @attr MultiComboBoxItem.optionDataSource (DataSource | String : null : IR)
// The <code>optionDataSource</code> of the combo box.
// @see ComboBoxItem.optionDataSource
// @visibility external
//<

//> @attr MultiComboBoxItem.autoFetchData (Boolean : false : IR)
// Should the MultiComboBoxItem fetch data from the +link{optionDataSource,data source}
// immediately or wait until the user first opens the pickList.
// @see ComboBoxItem.autoFetchData
// @visibility external
//<

//> @attr MultiComboBoxItem.valueMap (Array or Object : null : IRW)
// The <code>valueMap</code> of the combo box.
// @see FormItem.valueMap
// @visibility external
//<

//> @attr MultiComboBoxItem.comboBox (AutoChild ComboBoxItem : null : RA)
// An +link{AutoChild} attribute to create the combo box in a MultiComboBoxItem.
// @visibility external
//<

//> @attr MultiComboBoxItem.comboBoxProperties (ComboBoxItem Properties : null : IRA)
// Properties to be used in creating a +link{ComboBoxItem}.
// @visibility external
//<

//> @attr MultiComboBoxItem.comboBoxDefaults (ComboBoxItem Properties : null : RA)
// Default properties for the +link{ComboBoxItem} in a MultiComboBoxItem.
// @visibility external
//<
comboBoxDefaults: {
    showTitle: false,
    width: "*",
    hint: isc.MultiComboBoxItem.defaultHint,
    showHintInField: true,
    completeOnTab: true,
    shouldSaveValue: false,
    editorType: "ComboBoxItem",

    changed : function (comboForm, comboBoxItem) {
        this.creator._comboBoxItemChanged.apply(this.creator, arguments);
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

//> @attr MultiComboBoxItem.buttonProperties (IButton Properties : null : IRA)
// Properties to be used in creating each option button.
// @visibility external
//<

//> @attr MultiComboBoxItem.buttonDefaults (IButton Properties : null : RA)
// Default properties to be used in creating each +link{IButton}.
// @visibility external
//<
buttonDefaults: {
    _constructor: "IButton",
    align: "left",
    icon: "[SKIN]actions/remove.png",
    iconAlign: "right",
    iconOrientation: "right",

    click : function () {
        this.creator._buttonClick(this);
    }
},

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

// Invokes `CanvasItem.storeValue()` with a duplicate of `_buttonValues`.
_storeValue : function () {
    this.storeValue(this._buttonValues.duplicate());
},

// Create a button to represent a selected option having valueField <code>value</code>.
// @param value (any)
// @param displayValue (HTMLString)
_createButtonForValue : function (value, displayValue) {
    var button = this.createAutoChild("button", {
        title: displayValue
    });
    this._valueToButton[value] = button;

    var autoFitButtons = this._getAutoFitButtons();
    if (!autoFitButtons) {
        button.setWidth(this.canvas.getWidth());
    }
    button.setAutoFit(autoFitButtons);

    button[isc.MultiComboBoxItem._$buttonDataAttribute] = value;
    return button;
},

_comboBoxItemChanged : function (comboForm, comboBoxItem) {
    var value = comboBoxItem.getValue();

    // A value has been selected using the combo box's pick list.  The pick list should
    // have the full record for the selected value, because it was able to display
    // the option to the user.  Copy that record to the displayField cache (maintained
    // by FormItem) so that it can provide the display value for the button title, and
    // offer data for use by the sort function.  FormItem's displayField cache is reused
    // so that we don't have to maintain a separate cache and we can use FormItem's
    // implementation of mapValueToDisplay() and getDisplayValue().
    var resultSet = comboBoxItem.getPickListResultSet();
    if (resultSet != null) {
        var valueField = this.getValueFieldName(),
            record = resultSet.find(valueField, value);

        if (record != null) {
            var needsRefresh = this._addRecordToDisplayFieldCache(record);
            // assert !needsRefresh
            this.updateDisplayValueMap(false);
        }
    }

    this._insertButtonForValue(value);
    this._storeValue();

    // Clear the combo box item's text field so that more entries can be added.
    comboBoxItem.clearValue();
},

_isComboFormFirst : function () {
    var useInsertionOrder = this.useInsertionOrder,
        layoutStyle = this.layoutStyle;
    return (useInsertionOrder && layoutStyle == isc.MultiComboBoxItem.HORIZONTAL) ||
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
        if (layoutStyle == isc.MultiComboBoxItem.FLOW) {
            index = numButtons - k - 1;
            if (isInsert) {
                ++index;
            }
        } else {
            // assert layoutStyle == isc.MultiComboBoxItem.HORIZONTAL ||
            //        layoutStyle == isc.MultiComboBoxItem.HORIZONTAL_REVERSE ||
            //        layoutStyle == isc.MultiComboBoxItem.VERTICAL ||
            //        layoutStyle == isc.MultiComboBoxItem.VERTICAL_REVERSE
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
    var dataSource = this.getOptionDataSource();
    if (dataSource != null) {
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
        exists = (j >= 0);

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

        // "Flash" the existing button.
        var existingButton = this._buttonsLayout.getMember(index);
        existingButton.animateFade(50, function () {
            existingButton.animateFade(100, null, 100);
        }, 100);
    }
},

_$finishedHidingButton: "_finishedHidingButton",
_buttonClick : function (button) {
    // ignore button clicks if the item itself is readOnly
    if (this.isReadOnly()) {
        return;
    }

    var value = button[isc.MultiComboBoxItem._$buttonDataAttribute],
        sortOrder = this._getSortOrder(),
        j = isc.MultiComboBoxItem._binarySearch(value, this._sortedButtonValues, sortOrder),
        k = this._buttonValues.indexOf(value);

    

    // Remove the button from the layout
    delete this._valueToButton[value];
    var buttonsLayout = this._buttonsLayout;
    buttonsLayout.hideMember(button, {target: this, methodName: this._$finishedHidingButton, args: [buttonsLayout, button]});

    // Remove the record corresponding to the value from the FormItem's displayField cache.
    this._removeValueFromDisplayFieldCache(value);

    this._buttonValues.splice(k, 1);
    this._sortedButtonValues.splice(j, 1);
    this._storeValue();
},

_finishedHidingButton : function (buttonsLayout, button) {
    buttonsLayout.removeMember(button, true);
    button.destroy();
},

// Clears the _buttonsLayout and sets the value of the MultiComboBoxItem to an empty array.
_removeAllButtons : function () {
    var layout = this._buttonsLayout,
        layoutStyle = this.layoutStyle,
        useInsertionOrder = this.useInsertionOrder;

    if (layoutStyle == isc.MultiComboBoxItem.FLOW) {
        // The combo box item (tile) is last.
        for (var ri = layout.tiles.length - 1; ri > 0; --ri) {
            var button = layout.tiles[ri - 1];
            
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
    var ret = this.Super("drawn", arguments);
    if (this._dataValue !== undefined) {
        var displayValue = this.getDisplayValue(this._dataValue);
        this.showValue(displayValue, this._dataValue, this.form, this);
        delete this._dataValue;
    }
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

// Override FormItem.setValueMap() to set the valueMap on both this form item and the combo box.
setValueMap : function (valueMap) {
    this.Super("setValueMap", arguments);
    this.comboBox.setValueMap.apply(this.comboBox, arguments);
},

updateValueMap : function (refreshDisplay) {
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
        var buttonsLayout = this._buttonsLayout;
        for (var i = 0; i < numButtons; ++i) {
            var value = oldSortedButtonValues[i],
                j = sortedButtonValues.indexOf(value);

            if (i != j) {
                buttonsLayout.reorderMember(
                        this._transformIndex(undefined, i, false),
                        this._transformIndex(undefined, j, false));
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
                          oldLayoutStyle == VERTICAL_REVERSE,
        reverseOrder = isc.MultiComboBoxItem.isOrderReversedForStyle(layoutStyle),

        // Used only if oldLayoutStyle or layoutStyle is FLOW
        transferMembers = (useInsertionOrder ? _transferMembersReverse : _transferMembers);

    if (layoutStyle == FLOW) {

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

        if (oldLayoutStyle == FLOW || (isHorizontal != oldIsHorizontal))
        {
            var buttonsLayout = createLayout.call(this);
            if (oldLayoutStyle == FLOW) {
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
            // assert !useInsertionOrder

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

    if (animateTileChange !== undefined) {
        buttonsLayout.animateTileChange = animateTileChange;
    }
},

destroy : function () {
    if (this.comboForm) this.comboForm.destroy();
    if (this._buttonsLayout) this._buttonsLayout.destroy();
    this.Super("destroy", arguments);
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
    var _this = this;

    if (this.layoutStyle == isc.MultiComboBoxItem.FLOW && !isc.FlowLayout) {
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
        valueMap: this.valueMap,
        displayField: this.displayField,
        valueField: this.valueField,
        addUnknownValues: false // Prevent a "changed" event when the user types in something
                                // that is not a known option.
    });
    var autoFetchData = (this.autoFetchData !== undefined ?
                         this.autoFetchData : comboBoxProperties.autoFetchData);
    comboBoxProperties.autoFetchData = autoFetchData;
    var comboForm = this.addAutoChild("comboForm", {
        width: comboBoxWidth,
        fields: [comboBoxProperties]
    });
    var comboBox = this.comboBox = comboForm.getItem(0);

    // Set _buttonsLayout according the the value of layoutStyle.
    // The layout has initial height:1 so that the MultiComboBoxItem's height will be the minimum
    // height required to fit the buttonsLayout.
    var canvasProperties = isc.addProperties({}, this.canvasProperties, {
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

    } else if (this.layoutStyle == isc.MultiComboBoxItem.FLOW) {
        this._buttonsLayout = this._createFlowLayout();
    }
    this._buttonsLayout.addMember(comboForm);
    canvasProperties.members = [this._buttonsLayout];

    this.canvas = isc.HLayout.create(canvasProperties);
    this.Super("_createCanvas", arguments);
},

readOnlyDisplayChanged : function (appearance) {
    if (this.comboForm) {
        this.comboForm.readOnlyTextBoxStyle = this.getReadOnlyTextBoxStyle();
        this.comboForm.setReadOnlyDisplay(appearance);
    }
},

canEditChanged : function (canEdit) {
    if (this.comboForm) this.comboForm.setCanEdit(canEdit);
    var buttons = isc.getValues(this._valueToButton);
    if (buttons && buttons.length > 0) {
        buttons.map("setCanEdit", canEdit);
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

    } else if (layoutStyle == isc.MultiComboBoxItem.FLOW) {
        return true;
    } else {
        // assert layoutStyle == isc.MultiComboBoxItem.VERTICAL ||
        //        layoutStyle == isc.MultiComboBoxItem.VERTICAL_REVERSE
        return false;
    }
},

_createHStack : function () {
    return isc.HStack.create(this.valueLayoutDefaults, this.valueLayoutProperties, {
        autoDraw: false,
        height: 1,
        members: [],
        animateMembers: true,
        animateMemberTime: 100
    });
},

_createVStack : function () {
    return isc.VStack.create(this.valueLayoutDefaults, this.valueLayoutProperties, {
        autoDraw: false,
        height: 1,
        members: [],
        animateMembers: true,
        animateMemberTime: 100
    });
},

_createFlowLayout : function () {
    return isc.FlowLayout.create(this.valueLayoutDefaults, this.valueLayoutProperties, {
        autoDraw: false,
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

        hideMember : function (member, callback) {
            member.hide();
            this.fireCallback(callback);
        },

        reorderMember : function (memberNum, newPosition) {
            if (memberNum != newPosition) {
                var tile = this.getTile(memberNum);
                this.removeMember(tile);
                this.addMember(tile, newPosition);
            }
        }
    });
}
});
