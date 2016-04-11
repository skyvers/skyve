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
// A container that stores unique values.
// Note: This Set type does not have the same semantics as the proposed Set type of Harmony
// (ECMAScript 6). In particular, two Date objects are treated as the same if they represent
// the same time.
isc.defineClass("Set").addClassProperties({
    _nextUniqueSetNumber: 1,

    _dateCompareFn : function (t1, d2) {
        return (t1 - d2.getTime());
    }
});
isc.Set.addProperties({

//> @attr set.size (int : 0 : R)
// The current size of the set.
//<


    init : function () {
        this.Super("init", arguments);

        // The _withinSetProperty is a special property set on objects that are added to this
        // set.
        var uniqueSetNumber = this._uniqueSetNumber = isc.Set._nextUniqueSetNumber++;
        this._withinSetProperty = "_withinSetNumber" + uniqueSetNumber;

        // The times of any Date objects added to this set. The times are sorted for efficient
        // lookup via Array._binarySearch().
        this._dateValues = [];
        // An array of the objects contained in this set. This array is not accessed except in
        // remove() and clear() because if an object is contained in this set, then it will have
        // the _withinSetProperty set to true.
        // We keep a list of the objects so that if clear() is called with some objects still
        // contained in this set, we can delete the _withinSetProperty on those objects.
        this._objectValues = [];
        // A JavaScriptObject map from primitive values (stringified) to the actual primitive
        // value(s) that are contained in this set.
        this._primitiveValues = {};

        this.size = 0;
    },

    //> @method set.has()
    // Returns <code>true</code> if this set contains the given value.
    // @param value (any) the value to look for.
    // @return (boolean) <code>true</code> if this set contains <code>value</code>; <code>false</code>
    // otherwise.
    //<
    has : function (value) {
        if (isc.isA.Date(value)) {
            var i = isc.Array._binarySearch(this._dateValues, value, isc.Set._dateCompareFn);
            return i >= 0;

        } else if (isc.isAn.Object(value)) {
            return value[this._withinSetProperty];

        } else {
            var primitiveValues = this._primitiveValues[value],
                primitiveValue;
            if (isc.isAn.Array(primitiveValues)) {
                for (var i = 0, len = primitiveValues.length; i < len; ++i) {
                    primitiveValue = primitiveValues[i];
                    // Note: The check of the `typeof' is needed because `isNaN(" NaN")',
                    // `isNaN("NaN ")', `isNaN("NaNgarbage")', and `isNaN("+NaN")' are all
                    // true!
                    if (primitiveValue === value ||
                        (typeof primitiveValue === "number" && isNaN(primitiveValue) &&
                         typeof value === "number" && isNaN(value)))
                    {
                        return true;
                    }
                }
            } else {
                primitiveValue = primitiveValues;
                return (value in this._primitiveValues &&
                        (primitiveValue === value ||
                         (typeof primitiveValue === "number" && isNaN(primitiveValue) &&
                          typeof value === "number" && isNaN(value))));
            }
        }
        return false;
    },

    //> @method set.add()
    // Adds a value to this set.
    // @param value (any) the value to add.
    // @return (Set) this set.
    //<
    add : function (value) {
        if (isc.isA.Date(value)) {
            var i = isc.Array._binarySearch(this._dateValues, value, isc.Set._dateCompareFn);
            if (i >= 0) return this;

            // Store the time instead of the actual Date object because JS Dates are not immutable.
            this._dateValues.addAt(value.getTime(), -i - 1);

        } else if (isc.isAn.Object(value)) {
            // If the object is already within this set, return early.
            if (value[this._withinSetProperty]) return this;

            value[this._withinSetProperty] = true;
            this._objectValues.add(value);

        } else {
            var primitiveValues = this._primitiveValues[value],
                primitiveValue;
            if (isc.isAn.Array(primitiveValues)) {
                // Check for the primitive value already being in this set. If so, return early.
                for (var i = 0, len = primitiveValues.length; i < len; ++i) {
                    primitiveValue = primitiveValues[i];
                    if (primitiveValue === value ||
                        ((typeof primitiveValue === "number" && isNaN(primitiveValue)) &&
                         (typeof value === "number" && isNaN(value))))
                    {
                        return this;
                    }
                }

                // The primitive value is not already in this set.
                primitiveValues.add(value);
            } else if (value in this._primitiveValues) {
                // Check whether the primitive value is the same as the primitive value already
                // in this set. If so, return early.
                var primitiveValue = primitiveValues;
                if (value in this._primitiveValues &&
                    (primitiveValue === value ||
                     ((typeof primitiveValue === "number" && isNaN(primitiveValue)) &&
                      (typeof value === "number" && isNaN(value)))))
                {
                    return this;
                }
                this._primitiveValues[value] = [primitiveValue, value];
            } else {
                this._primitiveValues[value] = value;
            }
        }
        ++this.size;
        return this;
    },

    //> @method set.remove()
    // Removes a value from this set.
    // @param value (any) the value to remove.
    // @return (boolean) <code>true</code> if this set previously +link{Set.has(),contained}
    // <code>value</code>; <code>false</code> otherwise.
    //<
    remove : function (value) {
        if (isc.isA.Date(value)) {
            var i = isc.Array._binarySearch(this._dateValues, value, isc.Set._dateCompareFn);
            if (i >= 0) {
                this._dateValues.removeAt(i);
                --this.size;
                return true;
            }

        } else if (isc.isAn.Object(value)) {
            if (value[this._withinSetProperty]) {
                delete value[this._withinSetProperty];
                this._objectValues.remove(value);
                --this.size;
                return true;
            }

        } else if (value in this._primitiveValues) {
            var primitiveValues = this._primitiveValues[value],
                primitiveValue;
            if (isc.isAn.Array(primitiveValues)) {
                for (var i = 0, len = primitiveValues.length; i < len; ++i) {
                    primitiveValue = primitiveValues[i];
                    if (primitiveValue === value ||
                        ((typeof primitiveValue === "number" && isNaN(primitiveValue)) &&
                         (typeof value === "number" && isNaN(value))))
                    {
                        primitiveValues.removeAt(i);
                        --this.size;
                        return true;
                    }
                }
            } else {
                primitiveValue = primitiveValues;
                if (primitiveValue === value ||
                    ((typeof primitiveValue === "number" && isNaN(primitiveValue)) &&
                     (typeof value === "number" && isNaN(value))))
                {
                    delete this._primitiveValues[value];
                    --this.size;
                    return true;
                }
            }
        }
        return false;
    },

    //> @method set.clear()
    // Removes all values from this set.
    //<
    clear : function () {
        this._dateValues.setLength(0);

        var objectValues = this._objectValues,
            withinSetProperty = this._withinSetProperty;
        for (var i = 0, len = objectValues.length; i < len; ++i) {
            delete objectValues[i][withinSetProperty];
        }
        objectValues.setLength(0);

        this._primitiveValues = {};

        this.size = 0;
    }
});
