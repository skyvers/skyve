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

 






/////////////////////
//
//	Methods for sorting an array easily
//
/////////////////////


isc.addProperties(Array, {
    // note: documented as string values, internal constant is a boolean, code handles either
	//>	@type SortDirection
	ASCENDING:true,			//	@value	"ascending"		Sort in ascending order (eg: A-Z, larger items later in the list)
	DESCENDING:false		//	@value	"descending"	Sort in descending order (eg: Z-A, larger items earlier in the list)
	// @group sorting
	// @visibility external
	//<

});

isc.addMethods(Array, {

//>	@method		Array.shouldSortAscending()
//			Returns the passed in sortDirection (string / boolean) as the appropriate boolean
//		@group	sorting
//
//<
shouldSortAscending : function (sortDirection) {

    if (sortDirection == Array.ASCENDING) return true;
    if (sortDirection == Array.DESCENDING) return false;

    if (isc.isA.String(sortDirection)) {
        if (sortDirection.toLowerCase() == "ascending") return true;
        if (sortDirection.toLowerCase() == "descending") return false;        
    }
    
    // Anything else is invalid  - just return null
    return null;
}

});


// add a bunch of methods to the Array prototype so all arrays can use them
isc.addMethods(Array.prototype, {




//>	@method		array.sortByProperty()
// @include list.sortByProperty()
//<
sortByProperty : function (property, direction, normalizer, context) {
    return this.sortByProperties({property:property, direction:direction, 
                                  normalizer:normalizer, context:context});

}, 

//> @method array.setSort()
// Sort this Array by a list of +link{SortSpecifier}s. 
// @param sortSpecifiers (Array of SortSpecifier) the list of +link{SortSpecifier}s to sort by
// @return (array) the array itself
// @visibility external
//<
setSort : function (sortSpecifiers) {
    var numSortSpecifiers = sortSpecifiers.length,
        properties = new Array(numSortSpecifiers),
        directions = new Array(numSortSpecifiers),
        normalizers = new Array(numSortSpecifiers),
        contexts = new Array(numSortSpecifiers),
        comparators = new Array(numSortSpecifiers);
    for (var i = 0; i < numSortSpecifiers; ++i) {
        var item = sortSpecifiers[i];
        properties[i] = item.property;
        directions[i] = Array.shouldSortAscending(item.direction);
        normalizers[i] = item.normalizer;
        contexts[i] = item.context;
        comparators[i] = item._comparator;
    }
    return this.sortByProperties(properties, directions, normalizers, contexts, comparators);
},

//> @method array.sortByProperties()
// Given an array of objects, sort them by the properties of the member objects.
// Note that the sort will occur by the first property passed in, then for objects
// with matching values for the first property, the second property will be used (and so on).
// Can pass in an arbitary number of parameters.
// @param sortData (object) Each parameter is an object of the following format:<br>
// <code>{property:'propertyName', direction:direction, normalizer:normalizer}</code><br>
// Only the "property" attribute is required.  Pass in multiple arguments to sort by multiple
// properties.
// @return (array) the array itself
//<
// This method also supports being passed a 'context' parameter. If present, this is passed
// into the sort normalizer method as a parameter
// Example use case: ListGrids pass themselves into the 'sortByProperty' method as the context
// and are then available to the sort normalizer for the field.
// The context, if present, should be passed in as the 'context' attribute of each parameter
// object (so we can support 1 context per field name)
// In addition to the documented parameter format, sortByProperties will take 4 arrays - an
// array of property names, an array of sort directions, an array of normalizers and an array
// of 'context' objects.
// The normalizer / sortDirection / context for each property is then determined by 
// the position in the array (so the last 3 arrays are optional and may be sparse)


sortByProperties : function () {

    var normalizedArray = isc._normalizedValues,
        wrongTypeArray = isc._unexpectedTypeValues;
    
    // Support being called with either the signature 
    //  (["prop1", "prop2", ...], [dir1, dir2, ...], [norm1, norm2, ...])    
    // or
    //  ({property:"prop1", direction:dir1, normalizer:norm1}, {property:"prop2", ...},...)
    
    var returnSortIndex = false,
        disallowSortingOnLoadingMarker = false;
    if (isc.isAn.Array(arguments[0])) {
        this.sortProps = arguments[0];
        this.sortDirections = arguments[1] || [];
        this.normalizers = arguments[2] || [];
        this.contexts = arguments[3] || [];
        this._comparators = arguments[4] || [];
        returnSortIndex = arguments[5] || false;
        disallowSortingOnLoadingMarker = arguments[6] || false;
    } else {
    
        // clear out any sortProps so we don't get additional (old) properties
        
        if (!this.sortProps) {
            this.sortProps = [];
            this.normalizers = [];
            this.sortDirections = [];
            this.contexts = [];
            this._comparators = [];
        } else {    
            this.sortProps.clear();
            this.sortDirections.clear();
            this.normalizers.clear();
            this.contexts.clear();
            this._comparators.clear();
        }

        
        for (var i = 0, numArgs = arguments.length, extra = 0; i < numArgs; ++i) {
            var arg = arguments[i];
            if (!isc.isAn.Object(arg)) {
                if (extra == 0) {
                    returnSortIndex = arg || false;
                } else if (extra == 1) {
                    disallowSortingOnLoadingMarker = arg || false;
                }
                ++extra;
            } else {
                this.sortProps[i] = arg.property;
                
                this.sortDirections[i] = arg.direction;
                this.normalizers[i] = arg.normalizer;
                this.contexts[i] = arg.context;
                this._comparators[i] = arg._comparator;
                returnSortIndex = disallowSortingOnLoadingMarker = false;
                extra = 0;
            }
        }
    }

    // Bail out if we have empty sortProps
    
    if (this.sortProps == null || this.sortProps.length == 0) {
        return (returnSortIndex ? null : this);
    }
    
    // local refs
    var props = this.sortProps,
        norms = this.normalizers,
        contexts = this.contexts;

    // Refuse to sort the array if the ResultSet's loading marker if found in it.
    var loadingMarker = (disallowSortingOnLoadingMarker && isc.ResultSet != null ?
            isc.ResultSet.getLoadingMarker() : undefined);

    var compareAscending = Array.compareAscending,
        compareDescending = Array.compareDescending;

    var start = isc.timestamp();

    for (var i = 0; i < props.length; i++) {

        // remember the sort directions on the Array object -- retrieved in _compareNormalized
        var direction = isc._sortDirections[i] = this.sortDirections[i];

        if (!isc.isA.Function(this._comparators[i])) {
            this._comparators[i] = (direction ? compareAscending : compareDescending);
        }

        var property = props[i],
            normalizer = norms[i],
            context = contexts[i],
            l = this.length;
        // Set up the array to store the normalized values for this prop in
        normalizedArray[i] = new Array(l);
        wrongTypeArray[i] = new Array(l);

        if (isc.isA.Function(normalizer)) {

            for (var ii = 0; ii < l; ++ii) {
                var item = this[ii];
                if (item == null) {
                    // If any nulls were detected during the sort notify the developer
                    isc._containsNulls = true;
                    continue;
                } else if (item === loadingMarker) {
                    return (returnSortIndex ? null : this);
                }
                
                item._tempSortIndex = ii;
                var normalizedValue = normalizer(item, this.sortProps[i], context);                
                normalizedArray[i][ii] = normalizedValue;

                // If we're sorting the field according to an explicit data type, store values
                // not of that type for separate comparison
                
                if (dataType != null && !Array._matchesType(item[this.sortProps[i]], dataType)) {
                    wrongTypeArray[i][ii] = item[this.sortProps[i]];
                }

                // a custom normalizer might produce NaN, which is a dangerous because, unlike
                // any other value, both "1 > NaN" and "1 < NaN" are false, which fools the
                // comparator into thinking everything is equal to NaN, so the sort order is
                // scrambled and changes each time, and the reason why isn't obvious to the
                // developer.  Hence normalize NaN to the maximum negative value, like our
                // built-in numeric normalizer does.
                if (isc.isA.SpecialNumber(normalizedValue) && isNaN(normalizedValue)) {
                    normalizedArray[i][ii] = 0-Number.MAX_VALUE;
                }
            }
            //isc.Log.logWarn("function normalizer: normalized values: " + normalizedArray[i] + 
            //                ", unexpected types: " + wrongTypeArray[i]);
        } else {
            // if not passed an explicit normalizer, choose the appropriate function to normalize data 
            // (see above)
            var dataType = null;
            var isValueMap = false;
            var isDataPath = false;
            // catch the case where we were passed a data type rather than a normalizer function
            if (isc.isA.String(normalizer)) {
                dataType = normalizer;
            } else if (normalizer != null) {
                isValueMap = true;
            }
            
            if (context && context.getField) {
                var field = context.getField(property);
                
                if (field) {
                    if (field.dataPath) {
                        // Trim dataPath - required as field.dataPath may be absolute (so may include
                        // component.dataPath.
                        property = isc.Canvas._trimDataPath(field.dataPath, context);
                        isDataPath = true;
                    } else {
                        property = field.name;
                        isDataPath = false;
                    }
                    if (field.type && dataType == null) {
                        dataType = field.type;
                    }
                } else {
                    isDataPath = (property.indexOf("/") >= 0); 
                }
            }

            if (dataType == null) {
                dataType = this._getSortDataType(props[i]);
            }
            
            var type = isc.SimpleType.getType(dataType);
            var baseType = isc.SimpleType.getBaseType(type);
            if (baseType == null) {
                baseType = dataType;
            }
            
            if (!isValueMap) {
                normalizer = Array._getNormalizerFromType(baseType);
            }
            
            // In the case where we were unable to determine a custom data-type normalizer for the field
            // fall back on the default object normalizer.
            if (normalizer == null) normalizer = Array._normalizeObj;
            
            // a non-null, non-dataType, non-function normalizer was passed, assume it's a 
            // propertyValue -> normalizedValue map
            var normalizerMap = this.normalizers[i];
            for (var ii = 0; ii < l; ++ii) {
                var item = this[ii];

                if (item == null) {
                    isc._containsNulls = true;
                    continue;
                } else if (item === loadingMarker) {
                    return (returnSortIndex ? null : this);
                }
                item._tempSortIndex = ii;
                var atomicValue = Array._getAtomicValue(item, property, isDataPath, type);
                
                var normalizedValue = null;
                if (!isValueMap) {
                    normalizedValue = normalizer(atomicValue);
                } else {
                    var mappedVal = normalizer[atomicValue];
                    if (mappedVal == null) mappedVal = atomicValue;
                    normalizedValue = Array._normalizeStr(mappedVal);
                }
                normalizedArray[i][ii] = normalizedValue;
                
                // If we're sorting the field according to an explicit data type, store values
                // not of that type for separate comparison
                
                if (dataType != null && !Array._matchesType(atomicValue, baseType)) {
                    wrongTypeArray[i][ii] = item[this.sortProps[i]];
                }
            }
        }
    }   // END of the for loop


    
    if (isc.Browser.compensateForUnstableSort == null) {
        isc.Browser.compensateForUnstableSort =
                // Webkit covers Chrome, Safari, Android
                isc.Browser.isWebKit || isc.Browser.isOpera ||
                (isc.Browser.isIE && isc.Browser.version>=9);
                
    }
    if (isc.Browser.compensateForUnstableSort) {
        var numProps = normalizedArray.length;
        normalizedArray[numProps] = new Array(this.length);
        for (var i = 0; i < this.length; i++) {
            normalizedArray[numProps][i] = i;
        }
        
        var wrongTypeArrayNumPos = wrongTypeArray.length;
        if (wrongTypeArrayNumPos != 0) {
            wrongTypeArray[wrongTypeArrayNumPos] = new Array(this.length);
            for (var i = 0; i < this.length; i++) {
                wrongTypeArray[wrongTypeArrayNumPos][i] = i;
            }            
        }
        // sort ascending
        isc._sortDirections[numProps] = true;
        this._comparators[numProps] = compareAscending;
    }

    //isc.logWarn("normalizing took: " + (isc.timestamp() - start) + "ms");

    
    
    // worth pre-computing for the common case that there are no values of unexpected type
    var hasUnexpectedTypeValues = false;
    for (var i = 0; i < isc._unexpectedTypeValues.length; i++) {
        if (isc._unexpectedTypeValues[i].length > 0) {
            hasUnexpectedTypeValues = true;
            break;
        }
    }
    isc._hasUnexpectedTypeValues = hasUnexpectedTypeValues;

    //isc.logWarn("about to sort, hasUnexpectedTypeValues: " + isc._hasUnexpectedTypeValues + 
    //            ", normalizedValues: " + isc.echoFull(isc._normalizedValues) +
    //            ", unexpectedTypes: " + isc.echoFull(isc._unexpectedTypeValues) +
    //            " directions: " + isc._sortDirections);

    
    var normalizedValues = isc._normalizedValues,
        directions = isc._sortDirections,
        hasUnexpectedTypeValues = isc._hasUnexpectedTypeValues,
        comparators = this._comparators;

    // define comparator function for sorting by property - uses already stored out normalized
    // values and sort-directions
    var compareNormalized = 
function (a,b) {

    // For null values we'll always compare 'null' regardless of the field property
    var aIndex = (a != null ? a._tempSortIndex : null),
        bIndex = (b != null ? b._tempSortIndex : null);

    for (var i = 0; i < normalizedValues.length; i++) {
         
        var aFieldValue = normalizedValues[i][aIndex],
            bFieldValue = normalizedValues[i][bIndex];

        // if both values were not the expected type, compare them directly in un-normalized
        // form.  Note if only one value was unexpected type, by comparing normalized values we
        // will sort values of unexpected type to one end, since the standard normalizers all
        // normalize unexpected type values to the lowest values in the type's range.
        if (hasUnexpectedTypeValues && aFieldValue != null && bFieldValue != null) {
            var unexpectedTypes = isc._unexpectedTypeValues,
                aWrongType = unexpectedTypes[i][aIndex],
                bWrongType = unexpectedTypes[i][bIndex];
            if (aWrongType !== undefined && bWrongType !== undefined) {
                aFieldValue = aWrongType;
                bFieldValue = bWrongType;
            }
        }

        var returnVal = comparators[i](aFieldValue, bFieldValue);

        //isc.Log.logWarn("compared: " + isc.Log.echo(aFieldValue) + " to " +
        //             isc.Log.echo(bFieldValue) + ", returning: " + returnVal);

        // If we have a non-equal result, return it, otherwise we'll check the next property
        // in array.sortProps
        if (returnVal != 0) return returnVal;
        else if (hasUnexpectedTypeValues) {
            
            if ((aWrongType !== undefined) != (bWrongType !== undefined)) {
                return (aWrongType !== undefined) == !!directions[i] ? -1 : 1;
            }
        }
    }

    // at this point we've gone through every field in the sort, and these 2 items match in
    // each case -- just return 0 to indicate no order pref.
    
    return 0;                    
};

    var start = isc.timeStamp();

    // perform the actual sort
    this.sort(compareNormalized);

    //isc.logWarn("sorted in: " + (isc.timeStamp() - start) + "ms");

    // if we hit any nulls, warn the developer
    if (isc._containsNulls) {
        isc.Log.logWarn("Attempt to sort array by property hit null entry where a record should be. Array:" + 
                        isc.Log.echo(this));
        isc._containsNulls = null;
    }

    // Clear out the index temporarily stored on each item, and empty the temp arrays of
    // sort values / directions

    var sortIndex = null;
    if (returnSortIndex) {
        sortIndex = this._extractProperty("_tempSortIndex");
    } else {
        this.clearProperty("_tempSortIndex");
    }
    normalizedArray.clear();
    wrongTypeArray.clear();
    isc._sortDirections.clear();
    
    // call dataChanged in case anyone is observing it
    this.dataChanged();

    return (returnSortIndex ? sortIndex : this);
}, 


//>	@method		array.unsort()	(A)
//		Turn sorting off for this array, indicating that the current sort
//		order should be preserved.  Return true if this is supported in this List.
//
//		Some implementations may not support this -- they should return false
//		to indicate to the caller that sort order must be maintained (eg: in
//		the case where sort order is derived from the server, etc).
//
//		@group	sorting
//
//		@return	(boolean)	true == list supports unsorting, false == not supported.
// @visibility external
//<
unsort : function () {
    if (this.sortProps) this.sortProps.clear();
    return true;
},


// _getSortDataType() - given a field to sort this Array's member objects by, this method
// returns the data type to treat these field values as. Used to determine the appropriate
// normalizer function for the values.
// Note that a "normalizer" function renders a variable in a standardized format
// so we can sort by it easily.  For example, dates are converted into msec, etc.
_getSortDataType : function (sortProp, value) {
    var list = (value != null ? (isc.isAn.Array(value) ? value : [value]) : this);
	// determine the type WRT sorting based on the type of the first non null value
	for (var i = 0; i < list.length; i++) {

        if (!isc.isAn.Object(list[i])) continue;
        
		value = list[i][sortProp];

		// skip null entries
		if (value == null) continue;

        var type = Array._getType(value);
        if (type != null) return type;
    }
    return null;
},

// _getNormalizer() - method to give us a normalizer based on the data for the
// appropriate field within this Array's member objects.
_getNormalizer : function (sortProp, value) {

	var type = this._getSortDataType(sortProp, value);
    var normalizer = Array._getNormalizerFromType(type);
    return normalizer || Array._normalizeObj;
}, 


//>	@method		array.normalize()	(A)
//		@group	sorting
// 			Normalize a property of an object based on the normalizer for this array 
//			or the type of the property if that's this.normalizer is not set
//
//		@return	(any)	normalized value	
//<
normalize : function (obj, property) {
    var isDataPath = (property.indexOf("/") >= 0);
	var type = null;
	var normalizer;
	if (isc.isA.String(this.normalizer)) {
	    var dataType = this._getSortDataType(property);
	    type = isc.SimpleType.getType(dataType);
	    var baseType = isc.SimpleType.getBaseType(type);
	    normalizer = this._getNormalizerFromType(baseType);
	} else {
	    normalizer = this.normalizer;
	}
	var atomicValue = Array._getAtomicValue(obj, property, isDataPath, type);
    return normalizer[atomicValue];
} 

}); // END isc.addMethods(Array.prototype)

// add static sort routines and variables to the Array object
isc.addProperties(Array,{
	_SORT_TEMP : "__sort_temp",		// name of the temporary variable to use as sort criteria
	_UNEXPECTED_TYPE : "__unexpected_type" // Used by sortByProperty when a value doesn't match
	                                       // the field's expected data type
});

isc.addMethods(Array, {
//>	@method		array._normalize()
//		@group	sorting
//			Normalize one field into another for sorting
//	obj = object to normalize
//	property = property to normalize
//
//		@param	obj			(object)	object to normalize
//		@param	property	(string)	name of the property of object to normalize
//<
_normalizeObj : function (val) {
	return val;
},
_getAtomicValue : function (record, property, isDataPath, simpleType) {
    var value = null;
    if (isDataPath) {
        value = isc.Canvas._getFieldValue(property, null, record, null, true);
    } else {
        value = record[property];
    }
    if (simpleType &&  simpleType.getAtomicValue) {
        isc.Func.replaceWithMethod(simpleType, "getAtomicValue", "value");
        value = simpleType.getAtomicValue(value);
    }
    return value;
},
_normalizeStr : function (val) {
	
	return (isc.isA.String(val) ? val.toLowerCase() : isc.emptyString); 
},
_normalizeNum : function (val) {
    // put non-numbers at the beginning of the sort
	return isc.isA.Number(val) ? val : (0 - Number.MAX_VALUE);
},

_normalizeBool : function (val) {
    if (val == true) return 1;
    if (val == false) return 0;
    if (val == null) return -1;
    return -2;
},

_normalizeDate : function (val) {

    var time = (val && isc.isA.Date(val) ? val.getTime() : new Date(val).getTime()) 
	// NOTE: "new Date([bad date])" creates a special invalidate date object for which 
    // getTime() returns NaN in both Moz and IE.
    // Replace with zero to reliably sort at the top of ascending sort (or end of descending 
    // sort). 
    
    // NOTE: return the earliest valid date, not 0, which would be epoch time start (Jan 1
    // 1970), which would sort into the middle of some sets of dates.
    
    if (isNaN(time) || val == null) return -8640000000000000;
    return time;                
},

_normalizeTime : function (val) {
    if (!isc.isA.Date(val) && val != null) val = isc.Time.parseInput(val);
    
    if (isc.isA.Date(val)) return val.getTime();
    return 0;
},

// Normalizer for sorting data of type string numerically
textToNumericNormalizer : function (val) {
	var value = parseInt(val, 10);
	if (isc.isA.Number(value)) return value;
	else return 0;
},

// Given a known data type - what is the appropriate sort-normalizer?


_$string:"string", _$text:"text", _$number:"number", _$integer:"integer", _$float:"float", 

_$int:"int", _$boolean:"boolean", _$Date_ : "Date",  _$Time:"Time",
_$datetime : "datetime", _$Datetime:"Datetime",
_$date : "date", _$time:"time",

_getNormalizerFromType : function (type) {
    if (!type || !isc.isA.String(type)) return null;
    switch (type) {
        case this._$string:     
        case this._$text:
                                return Array._normalizeStr;
        case this._$boolean:    return Array._normalizeBool;
        case this._$Date_:
        case this._$date:
        case this._$Datetime:
        case this._$datetime:
                                return Array._normalizeDate;
        case this._$Time:
        case this._$time:
                                return Array._normalizeTime;
                
        case this._$number:
        case this._$integer:
        case this._$int:
        case this._$float:
                                return Array._normalizeNum;
    }
    return Array._normalizeObj;
},

// _getType() - returns the "type" name of an object for sorting normalization purposes
_$object:"object",
_getType : function (object) {
    var type = typeof object;
    if (type == this._$object) {
        if (isc.isA.Date(object)) type = this._$date;
    }
    return type;
},

// _matchesType() - helper method used by sortByProperty to catch unexpected type values
// Note the 'type' specified for a field (like "float") may not match the value returned by
// this._getType(object) - so we have to detect equivalent types, (like float and number)
_standardTypeMap:{
    "float":"number",
    "int:":"number",
    "integer":"number",
    "text":"string",
    "Date":"date",
    "Time":"date",
    "time":"date"
},
_matchesType : function (object, type) {
    var objectType = this._getType(object);
    if (objectType == type) return true;
    
    return (this._standardTypeMap[type] == objectType);
},




//>	@classMethod		Array.compareAscending()	(A)
// Compare two values for an ascending order sort, using locale-sensitive comparison.
//		@group	sorting
//
// @param	a	(any)	first value to compare
// @param	b	(any)	second value to compare
//
// @return	(number)	negative == second is larger, 0 == same value, positive == first is larger	
// @visibility external
//<
compareAscending : function (first, second) {
    if (first != null && first.localeCompare != null) {
        return first.localeCompare(second);
    }
    if (second != null && second.localeCompare != null) {
        return -second.localeCompare(first);
    }
	return (second > first ? -1 : second < first ? 1 : 0);
},

//>	@classMethod		Array.compareDescending()	(A)
// Compare two values for a descending order sort, using locale-sensitive comparison.
//		@group	sorting
//
// @param	first	(any)	first value to compare
// @param	second	(any)	second value to compare
//
// @return	(number)	negative == first is larger, 0 == same value, positive == second is larger	
// @visibility external
//<
compareDescending : function (first, second) {
    if (first != null && first.localeCompare != null) {
        return -first.localeCompare(second);
    }
    if (second != null && second.localeCompare != null) {
        return second.localeCompare(first);
    }
	return (second < first ? -1 : second > first ? 1 : 0);
}

//>Safari3 Safari comparators for broken localeCompare
,
safariCompareAscending : function (first, second) {
    if (first != null && first.localeCompare != null) {
        return (first.localeCompare(second) - 2);
    }
    if (second != null && second.localeCompare != null) {
        return -(second.localeCompare(first) - 2);
    }
	return (second > first ? -1 : second < first ? 1 : 0);
},
safariCompareDescending : function (first, second) {
    if (first != null && first.localeCompare != null) {
        return -(first.localeCompare(second) - 2);
    }
    if (second != null && second.localeCompare != null) {
        return (second.localeCompare(first) - 2);
    }
	return (second < first ? -1 : second > first ? 1 : 0);
}
//<Safari3

});

// Central array for temp storage of normalized values for sorting

isc._normalizedValues = [];
isc._unexpectedTypeValues = [];
isc._sortDirections = [];



//>Safari3
(function () {
    if (isc.Browser.isSafari) {
        var b = "b";
        if (b.localeCompare("a") == 3) {
            Array.compareAscending = Array.safariCompareAscending;
            Array.compareDescending = Array.safariCompareDescending;
        }
    }
})();
//<Safari3

