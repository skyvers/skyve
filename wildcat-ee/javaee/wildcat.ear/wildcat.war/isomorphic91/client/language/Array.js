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




//>	@object	Array
// Generic extensions to JavaScript Arrays.  You can call these on any Array.
// <P>
// JavaScript's native Array is retrofitted to support the <code>List</code> API.
//
// @implements List
// @see List
// @treeLocation Client Reference/System
// @visibility external
//<

// Internal notes: Array vs the List interface
// - List is an interface which the native JavaScript Array object is retrofitted to implement
// - When a given method can be implemented solely in terms of other parts of the List interface,
//   there is the possibility that Array and the List interface can share the actual JavaScript
//   function object.  When this is done, the method is first defined on Array (for load order
//   reasons).
// - on Array, several methods can be faster if they use various native functions (like splice()),
//   and so a unique implementation appears here
// - on List, in order to allow a valid List implementation with a minimum of effort, all methods
//   boil down to very simple primitives like addAt

// - public documentation for the List interface is in List.js

//> @groupDef dataChanged
// Operations that change the Array
// @title Data Changes
//<

//> @groupDef iteration
// Operations on entire Arrays at once
// @title Iteration
//<

//> @groupDef arrayMath
// Math operations on entire Arrays at once
// @title Array Math
//<

// add a "Class" property to the array prototype
//	so we can recognize Array instances
Array.prototype.Class = "Array";

//>	@classMethod		Array.newInstance()
//		Create a new array, adding any arguments passed in as properties.
//		Here so we can make standard newInstance() calls on arrays.
//
//		@param	[all arguments]	(object)	objects with properties to override from default
//		@return	(array)	new array.
//<
Array.newInstance = function () {
	var instance = [];
	isc.addPropertyList(instance, arguments);
	return instance;
}
Array.create = Array.newInstance;

//> @classAttr Array.LOADING (String : "loading" : IRA)
// Marker value returned by Lists that manage remote datasets, indicating the requested data is
// being loaded. Note that the recommended test for loading data is to call +link{Array.isLoading()}
// rather than compare to this value directly.
// @visibility external
//<

Array.LOADING = "loading";

//> @classMethod Array.isLoading() (A)
// Is the object passed in a loading marker value? For use with Lists that manage remote
// datasets, to indicate that a record has not yet been retrieved from the server. A typical
// use case might be to check if a row has been loaded in a ListGrid - for example:
// <P>
// <code>
// if (Array.isLoading(myList.getRecord(0))) isc.warn("Please wait for the data to load.");
// </code>
// @param value (any) data to test. 
// @visibility external
//<
Array.isLoading = function (row) {
    
    return row != null &&
            
            !isc.isAn.XMLNode(row) &&
            
            (row === Array.LOADING);
}

//> @classAttr Array.CASE_INSENSITIVE (Function : See below : )
// This is a built-in comparator for the +link{array.find,find} and +link{array.findIndex,findIndex}
// methods of Array.  Passing this comparator to those methods will find case-insensitively,
// so, eg, <code>find("foo", "bar")</code> would find objects with a "foo" property set to 
// "Bar", "BAR" or "bar"
// @visibility external
//<
Array.CASE_INSENSITIVE = function(arrayMemberProperty, comparisonProperty, propertyName) {
    return (
        arrayMemberProperty == comparisonProperty ||
        (isc.isA.String(arrayMemberProperty) &&
         isc.isA.String(comparisonProperty) &&
         arrayMemberProperty.toLowerCase() == comparisonProperty.toLowerCase()));
};

//> @classAttr Array.DATE_VALUES (Function : See below : )
// This is a built-in comparator for the +link{array.find,find} and +link{array.findIndex,findIndex}
// methods of Array.  Passing this comparator to those methods will find instances where Dates  
// in the search criteria match Dates in the array members (ordinarily, Javascript only regards
// Dates as equal if they refer to the exact same object).  This comparator compares <i>logical</i>
// dates; the time elements of the values being compared are ignored, so two Dates representing
// different times on the same day will be considered equal.
// @see Array.DATETIME_VALUES
// @visibility external
//<
Array.DATE_VALUES = function(arrayMemberProperty, comparisonProperty, propertyName) {
    return (
        arrayMemberProperty == comparisonProperty ||
        (isc.isA.Date(arrayMemberProperty) &&
         isc.isA.Date(comparisonProperty) &&
         Date.compareLogicalDates(arrayMemberProperty, comparisonProperty) == 0));
};

//> @classAttr Array.DATETIME_VALUES (Function : See below : )
// This is a built-in comparator for the +link{array.find,find} and +link{array.findIndex,findIndex}
// methods of Array.  Passing this comparator to those methods will find instances where Dates  
// in the search criteria match Dates in the array members (ordinarily, Javascript only regards
// Dates as equal if they refer to the exact same object).  This comparator compares entire
// date values, including the time elements of the values being compared, so two Dates
// representing different times on the same day (even if they are only a millisecond apart)
// will not be considered equal.
// @see Array.DATE_VALUES
// @visibility external
//<
Array.DATETIME_VALUES = function (arrayMemberProperty, comparisonProperty, propertyName) {
    
    return (
        arrayMemberProperty == comparisonProperty ||
        (isc.isA.Date(arrayMemberProperty) &&
         isc.isA.Date(comparisonProperty) &&
         Date.compareDates(arrayMemberProperty, comparisonProperty) == 0));
};


if (!Array.prototype.localeStringFormatter) 
    Array.prototype.localeStringFormatter = "toString";

// add a bunch of methods to the Array prototype so all arrays can use them
isc.addMethods(Array.prototype, {

iscToLocaleString : function () {
	return this[this.localeStringFormatter]();
},

//>	@method		array.getPrototype()
//		Return the Array.prototype -- for conformity with the Class.getPrototype() method
//		Used in exporting arrays.
//<
getPrototype : function () {
	return Array.prototype;
},


//>	@method		array.newInstance()
//		Create a new array, adding any arguments passed in as properties.
//		Here so we can make standard newInstance() calls on arrays.
//
//		@param	[all arguments]	(object)	objects with properties to override from default
//		@return	(array)	new array.
//<
newInstance : Array.newInstance,
create : Array.newInstance,

// List Interface
// --------------------------------------------------------------------------------------------

//>	@method		array.get()
// @include list.get()
//<
get : function (pos) {
	return this[pos]
},

//>	@method		array.getLength()
// @include list.getLength()
//<
getLength : function () {
	return this.length
},

//>	@method		array.isEmpty()
// @include list.isEmpty()
//<
// NOTE: implementation stolen by List interface.  Must use only List API for internal access.
isEmpty : function () {
	return this.getLength() == 0;
},

//>	@method		array.first()
// @include list.first()
//<
first : function () {
	return this[0]
},

//>	@method		array.last()
// @include list.last()
//<
last : function () {
	return this[this.length-1]
},

nativeIndexOf : Array.prototype.indexOf,

//>	@method		array.indexOf()
// @include list.indexOf()
//<

indexOf : function (obj, pos, endPos, comparator) {
	// normalize position to the start of the list
	if (pos == null) pos = 0;
    if (endPos == null) endPos = this.length - 1;

    var hasComparator = (comparator != null);
    for (var i = pos; i <= endPos; i++) {
        if (hasComparator ? comparator(this[i], obj) : this[i] == obj) {
            return i;
        }
    }

	// not found -- return the not found flag
	return -1;
}, 

nativeLastIndexOf : Array.prototype.lastIndexOf,

//>	@method		array.lastIndexOf()
// @include list.lastIndexOf()
//<
lastIndexOf : function (obj, pos, endPos, comparator) {
	// normalize position to the end of the list
	if (pos == null) pos = this.length-1;
    if (endPos == null) endPos = 0;

    var hasComparator = (comparator != null);
    for (var i = pos; i >= endPos; i--) {
        if (hasComparator ? comparator(this[i], obj) : this[i] == obj) {
            return i;
        }
    }

	// not found -- return the not found flag
    return -1;
},

//>	@method		array.contains()
// @include list.contains()
//<
// NOTE: implementation stolen by List interface.  Must use only List API for internal access.
contains : function (obj, pos, comparator) {
	return (this.indexOf(obj, pos, null, comparator) != -1);
},

// helper method for doing a substring search

containsSubstring : function (obj, startPos, endPos, ignoreCase, matchStyle) {
    if (obj == null) return true;
    if (matchStyle == null) matchStyle = "substring";
    var result = this.indexOf(obj, startPos, endPos, function (a, b) {
        var filter = b == null ? null : (isc.isA.String(b) ? b : b.toString()),
            value = a == null ? null : (isc.isA.String(a) ? a : a.toString())
        ;
        if (ignoreCase) {
            if (filter != null) filter = filter.toLowerCase();
            if (value != null) value = value.toLowerCase();
        }
        var r = false;
        if (value != null && filter != null) {
            if (value == filter) {
                r = true;
            } else if (matchStyle == "substring" && 
                                        value && value.contains && value.contains(filter))
            {
                r = true;
            } else if (matchStyle == "startsWith" && 
                                        value && value.startsWith && value.startsWith(filter))
            {
                r = true;
            }
        }
        return r;
    });

	return result >= 0;
},

//> @method     array.containsAll()
// @include list.containsAll()
//<
// NOTE: implementation stolen by List interface.  Must use only List API for internal access.
containsAll : function (list) {
    if (list == null) return true;
    var length = list.getLength();
    for (var i = 0; i < length; i++) {
        if (!this.contains(list.get(i))) return false;
    }
    return true;
},

// string-based method - substring search - returns true if all of the values from the passed 
// list appear somewhere in the contents of the values in this list 
containsAllSubstring : function (list, ignoreCase) {
    if (list == null) return true;
    var length = list.getLength();
    for (var i = 0; i < length; i++) {
        if (!this.containsSubstring(list.get(i), null, null, ignoreCase)) return false;
    }
    return true;
},

//>	@method		array.intersect()
// @include list.intersect()
//<
// NOTE: implementation stolen by List interface.  Must use only List API for internal access.
intersect : function () {
	var results = [];

	// for each element in this array
	for (var i = 0; i < this.length; i++) {
		// if the element is in each argument, add it to the results
		var item = this.get(i),
			isPresent = true;

		// skip null elements
		if (item == null) continue;

		// for each array passed in
		for (var a = 0; a < arguments.length; a++) {
			// if the item is not in that array
			if (!arguments[a].contains(item)) {
				// it hasn't been found
				isPresent = false;
				break;
			}
		}
		if (isPresent) results.add(item);
	}

	// return true
	return results;
},

// variant of intersect that specifically deals with arrays of dates, which need to be compared
// with compareDates() and compareLogicalDates()
intersectDates : function () {
	var results = [];

	// for each element in this array
	for (var i = 0; i < this.length; i++) {
		// if the element is in each argument, add it to the results
		var item = this.get(i),
			isPresent = true
        ;

		// skip null elements
		if (item == null) continue;

        var logicalDate = item.logicalDate;

            // for each array passed in
		for (var a = 0; a < arguments.length; a++) {
            var otherArray = arguments[a];
            var inOtherArray = false;
            if (!otherArray) continue;
            for (var b = 0; b < otherArray.length; b++) {
                var otherItem = otherArray[b];
                if (!otherItem) continue;
                if (logicalDate) {
                    if (Date.compareLogicalDates(item, otherItem) == 0) {
                        inOtherArray = true;
                        break;
                    }
                } else {
                    if (Date.compareDates(item, otherItem) == 0) {
                        inOtherArray = true;
                        break;
                    }
                }
			}
            if (!inOtherArray) {
                isPresent = false;
                break;
            }
		}
		if (isPresent) results.add(item);
	}

	return results;
},

// variant of intersect that compares arrays of values as strings - returns entries from this
// array that appear as a substring of at least one entry in each of the passed arrays

_intersectSubstringIgnoreCase: true,
intersectSubstring : function (lists, ignoreCase, matchStyle) {
    // If the "lists" param is not a list of lists, make it one
    if (!isc.isAn.Array(lists)) lists = [lists];
    if (!isc.isAn.Array(lists[0])) lists =[lists];
	var results = [];
    if (ignoreCase == null) ignoreCase = this._intersectSubstringIgnoreCase;
    ;

	// for each element in this array
	for (var i = 0; i < this.length; i++) {
		// if the element is in each argument, add it to the results
		var item = this.get(i),
			isPresent = true;

		// skip null elements
		if (!item) continue;

		// for each array passed in
		for (var a = 0; a < lists.length; a++) {
            var otherArray = lists[a];
            if (!otherArray) continue;

            // match if any of the elements in the passed array contains "item" as a substring
            if (!otherArray.containsSubstring(item, null, null, ignoreCase, matchStyle)) {
                isPresent = false;
                break;
            }
		}
		if (isPresent) results.add(item);
	}

	// return true
	return results;
},

//>	@method		array.equals()
// @include list.equals()
//<
// NOTE: implementation stolen by List interface.  Must use only List API for internal access.
equals : function (list) {
    if (list == null || !isc.isA.List(list)) return false;
    
    var length = list.getLength();

    // arrays of differing lengths cannot be equals
    if (length != this.getLength()) return false;

    for (var i = 0; i < length; i++) {
        if (list.get(i) != this.get(i)) return false;
    }
    return true;
},

//>	@method		array.getItems()
// @include list.getItems()
//<
// NOTE: implementation stolen by List interface.  Must use only List API for internal access.
getItems : function (itemList) {
	var outputs = [], length = itemList.getLength();
	for (var i = 0; i < length; i++) {
		outputs[i] = this.get(itemList.get(i));
	}
	return outputs;
},

//>	@method		array.getRange()
// @include list.getRange()
//<
getRange : function (start, end) {
    if (end == null) end = this.length - 1;
	return this.slice(start, end);
},

//>	@method		array.duplicate()	(A)
// @include list.duplicate()
//<

duplicate : function () {
	return isc._emptyArray.concat(this); // NOTE: concat creates a copy
},

// getData() from list - no analogous method

//>	@method		array.set()	
// @include list.set()
//<
set : function (pos, item) {
    this[pos] = item;
    this.dataChanged();
},

//>	@method		array.addAt()
// @include list.addAt()
//<
addAt : function (obj, pos) {
    if (pos == null) pos = 0;

    this.splice(pos, 0, obj);

	// call dataChanged in case anyone is observing it
	this.dataChanged();

    // return the object that was added
    return obj;
},

//>	@method		array.removeAt()
// @include list.removeAt()
//<
removeAt : function (pos) {
	// make sure the pos passed in is valid
	var length = this.length;
	if (pos >= length || pos < 0) return null;

    var removedList = this.splice(pos, 1);

	// call dataChanged in case anyone is observing it
	this.dataChanged();

    return removedList[0];
},

//>	@method		array.add()
// @include list.add()
//<
add : function (object, secondArg, disallowSortingOnLoadingMarker) {
    var undefined;
    if (secondArg !== undefined) {
        // support calling as add(index, object)
        return this.addAt(object, secondArg);
    }
	var pos;
	// if the list.sortUnique is true, we're only supposed to have each item once
	if (this.sortUnique) {
		// find the current position of the item in the list
		pos = this.indexOf(object);
		// if it wasn't found, put it at the end
		if (pos == -1) pos = this.length;
	} else {
		// otherwise we always put the item at the end
		pos = this.length;
	}
	// actually stick the object in the list
	this[pos] = object;

    // if we are currently sorted, maintain current sort
    if (this.sortProps && this.sortProps.length > 0) {
        
        this.sortByProperties(
            this.sortProps, this.sortDirections, this.sortNormalizers, undefined, undefined,
            false, disallowSortingOnLoadingMarker);
    }

	// call dataChanged in case anyone is observing it
	this.dataChanged();
	
	// return the object that was added
	return object;
},

//>	@method		array.addList()
// @include list.addList()
//<
// NOTE: implementation stolen by List interface.  Must use only List API for internal access.
addList : function (list, listStartRow, listEndRow) {
    if (list == null) return null;

	this._startChangingData();
	
	if (listStartRow == null) listStartRow = 0;
	if (listEndRow == null) listEndRow = list.getLength();

	for (var pos = listStartRow; pos < listEndRow; pos++) {
		this.add(list.get(pos));
	}

	this._doneChangingData();

	// return the objects that were added
	return list;
},

//>	@method		array.setLength()
// @include list.setLength()
//<
setLength : function (length) {
	this.length = length;
},

//>	@method		array.addListAt()
// @include list.addListAt()
//<
addListAt : function (list, pos) {
    if (list == null) return null;

    // extract the tail of this array, from pos through the end
    var tail = this.splice(pos, this.length - pos);

    // add the new items in list
    this.push.apply(this, list);

    // add back the tail
    this.push.apply(this, tail);

	// call dataChanged in case anyone is observing it
	this.dataChanged();

    // return the list that was added
    return list;
}, 


//>	@method		array.remove()
// @include list.remove()
//<
remove : function (obj) {
    

    var index = this.indexOf(obj);
    if (index == -1) return false;

    this.removeAt(index);
    // removeAt() calls dataChanged().

    return true; // indicating object was removed, per java.util.Collection
},

//>	@method		array.removeList()
// @include list.removeList()
//<
removeList : function (list) {
    if (list == null) return null;

	// run through all the items, putting things we want to retain into new list output
	for (var output = [], i = 0, l = this.length;i < l;i++) {
		if (!list.contains(this[i])) output.add(this[i]);
    }
	// now set the items in this list to the items in output
	this.setArray(output);

	// return the list that was removed
	return list;
}, 

// useful in chaining expressions eg someList.removeEvery(null).getProperty(...)
// .. removeList/removeAll don't work in this circumstance
removeEvery : function (value) {
    this.removeList([value]);
    return this;
},

// methods to ensure dataChanged() fired only once when a series of changes are made: see List.js
_startChangingData : function () {
    var undef;
	if (this._dataChangeFlag === undef) this._dataChangeFlag = 0;
	this._dataChangeFlag++;
},

_doneChangingData : function () {
	if (--this._dataChangeFlag == 0) this.dataChanged();
},

//>	@method		array.dataChanged()	(A)
// @include list.dataChanged()
//<
dataChanged : function () {
    
    if (this.onDataChanged) this.onDataChanged()
},

// In some cases we want to perform a one-liner - call dataChanged unless we're inside a data
// changing loop
_isChangingData : function () {
    return (this._dataChangeFlag != null && this._dataChangeFlag > 0); 
},

// End of List API
// --------------------------------------------------------------------------------------------

//>	@method		array.setArray()
// Completely change the contents of one array to the contents of another array.
// <P>
// This is useful if you have an external pointer to an array, but you want to change its
// contents, such as when you remove some items from the array.
//
//		@group	dataChanged
//
//		@param	(array)		array to set this array to
//<
setArray : function (list) {
    this.setLength(0);

	// fill slots
    this.push.apply(this, list);

	// call dataChanged in case someone is observing data in the list
	this.dataChanged();
},

//>	@method		array.addAsList()
// Add either a single object or a list of items to this array.
//
//		@group	dataChanged
//
//		@param	list	(array or object)		a single object or a list of items to add
//
//		@return	(list)				list of items that were added
//<
addAsList : function (list) {
	if (!isc.isAn.Array(list)) list = [list];
	// return the objects that were added
	return this.addList(list);
},

//>	@method		array.removeRange()
// Remove and return a range of elements from an array - same return value as array.slice(),
// but removes the slice from the array
//
//		@group	dataChanged
//
//		@param	startPos	(number)	start position of range to remove
//      @param  endPos      (number)    end position of range to remove
//
//      @return (array) array of items that were removed
//<
removeRange : function (startPos, endPos) {
    // fall through to splice
    var undefined;
    if (startPos === undefined) return this;    // no arguments
    if (!isc.isA.Number(startPos)) startPos = 0;
    if (!isc.isA.Number(endPos)) endPos = this.length;
    return this.splice(startPos, endPos - startPos);
}, 

//>	@method		array.removeWhere()
//			Remove all instances of object from this array
//		@group	dataChanged
//
//		@param	property	(string)	property to look for
//		@param	value		(string)	value to look for
//<
removeWhere : function (property, value) {
	for (var i = 0, newList = []; i < this.length; i++) {
		if (!this[i] || this[i][property] != value) {
			newList.add(this[i]);
		}
	}
	this.setArray(newList);
}, 

// Corollary to removeWhere - remove every item where some property is not set to some
// specified value.
removeUnless : function (property, value) {
	for (var i = 0, newList = []; i < this.length; i++) {
		if (this[i] && this[i][property] == value) {
			newList.add(this[i]);
		}
	}
	this.setArray(newList);
},

//>	@method		array.removeEmpty()
//			Remove all empty slots in this array (where array[n] == null)
//		@group	dataChanged
//<
removeEmpty : function (property, value) {
	for (var i = 0, newList = []; i < this.length; i++) {
		if (this[i] != null) {
			newList.add(this[i]);
		}
	}
	this.setArray(newList);
}, 

//> @method array.getProperty()
// @include list.getProperty
// @visibility external
//<
getProperty : function (property) {
	for(var output = [], i = 0, l = this.length;i < l;i++)
		output[output.length] = (this[i] ? this[i][property] : null);
	return output;
}, 

//>@method array.getValueMap()
// @include list.getValueMap()
// @visibility external
//<
getValueMap : function (idField, displayField) {
    var valueMap = {};
    for (var i = 0, l = this.getLength(); i < l; i++) {
        var item = this.get(i);
        // Don't attempt to pull properties from empty values / basic data types in the list.
        if (!isc.isAn.Object(item)) continue;
        if (item && item[idField] != null) {
            valueMap[item[idField]] = item[displayField];
        }
    }
    return valueMap;
},

//>	@method		array.map()
// Return an array where the value of item <code>i</code> is the result of calling the provided
// function on item <code>i</code> in this array.
// <P>
// The function to call can either be provided directly as a function object, in which case it
// is invoked with the item as the first argument, or can be provided as the String name of a
// method present on each item, which will be invoked.  In the latter case, if any item is null
// or lacks the named method, null will be returned for that item.
// <P>
// Examples:<PRE>
//    // line up widgets at 20 pixels from page edge
//    [widget1, widget2].map("setPageLeft", 20);
//
//    // find furthest right widget
//    [widget1, widget2].map("getPageRight").max();
// </PRE>
// 
//		@group	iteration
//
//		@param	method  (string or function) function object, or name of method
//	    @param	[(arguments 1-N)]	(any)	 arguments to pass to the function or method
//                                           invoked on each item
//		@return	(array)		array of returned values
// @visibility external
//<
map : function (method, arg1, arg2, arg3, arg4, arg5) {
                         

    var isFunc = isc.isA.Function(method),
        output = [],
        length = this.getLength();
        
	var undef,
        mimicNativeImp = isFunc && 
                        (arg1 === undef || isc.isAn.Object(arg1)) &&
                         arg2 === undef && arg3=== undef && arg4 === undef && arg5 === undef;

    for (var i = 0; i < length; i++) {
        var item = this.get(i);
        
        if (mimicNativeImp) {
            if (arg1 == null) output[i] = method(item, i, this);
            else {
                arg1._tempSlot = method;
                output[i] = arg1._tempSlot(item, i, this);
                delete arg1._tempSlot;
            }            
        } else if (isFunc) {
            output[i] = method(item, arg1, arg2, arg3, arg4, arg5);
        } else {
            output[i] = (item && item[method] != null ?
                         item[method](arg1, arg2, arg3, arg4, arg5) : null);
        }
    }
    return output;
},

//>	@method		array.setProperty()
//	Set item[property] = value for each item in this array.
//		@group	iteration
//
//		@param	property	(string)	name of the property to set
//		@param	value		(any)		value to set to
// @visibility external
//<
setProperty : function (property, value) {
	for(var i = 0, l = this.length;i < l;i++)
		if (this[i]) this[i][property] = value;
}, 

//>	@method		array.clearProperty()
// Delete property in each item in this array.
//		@group	iteration
//
//		@param	property 	(string)	name of the property to clear
// @return (boolean) returns true if any of the properties in the array had a value for the
//     specified property.
// @visibility external
//<
clearProperty : function (property) {
    var hadValue = false, undef;
	for(var i = 0, l = this.length;i < l;i++) {
        hadValue = hadValue || this[i] !== undef;
		if (this[i]) delete this[i][property];
    }
    return hadValue;
},


_extractProperty : function (property) {
    var hadValue = false,
        output = [];
    for (var i = 0, l = this.length; i < l; ++i) {
        var record = this[i];
        hadValue = hadValue || (record !== undefined);
        if (record) {
            output[output.length] = record[property];
            delete record[property];
        } else {
            output[output.length] = null;
        }
    }
    return (hadValue ? output : null);
},

//>	@method		array.getProperties()
// Return a copy of the array where each object has only the list of properties provided.
//		@group	iteration
//
//		@param	properties	(string[])	names of the properties you want to export
//		                    (object)	object with the properties you want to export
//
//		@return	(Array)		new Array with each item limited to the specified properties
//<
getProperties : function (properties) {
    return isc.applyMask(this, properties);
},

//>	@method		array.getUniqueItems()
// Return a list of each unique item in this list exactly once.
// <P>
// Returns in the same order they were found in the list.
// <P>
// Usage example:<br>
// &nbsp;&nbsp;&nbsp;&nbsp;uniqueList = myArray.getProperty("foo").getUniqueItems();
//
//		@group	subset
//
//		@return	(array)	list of each unique item in the list
// @visibility external
//<
getUniqueItems : function () {
	for (var output = [], i = 0, l = this.length; i < l; i++) {
		if (!output.contains(this[i])) output[output.length] = this[i];
	}
	return output;
},

//>	@method		array.slice()
// Return a contiguous range of rows of the array.  
// DOES NOT include element at position <code>end</code> (similar to .substring())
// <P>
// NOTE: uses browser's native implementation if one is provided
//
// @param	start	(number)	start index
// @param	[end]	(number)	end index, if not specified will be list.length
//
// @return	(array)	new array with items from start -> end-1 in it	
// @group	subset
//<
slice :
	(Array.prototype.slice 
		? Array.prototype.slice
		: function (start, end) {
			if (end == null) end = this.length;
			for(var output = [], l = this.length; start < end && start < l;start++)
				output[output.length] = this[start];
			return output;
		}
	), 

//>	@method array.findIndex()
// @include list.findIndex
//<
findIndex : function (property, value, comparator) {
    return this.findNextIndex(0, property, value, null, comparator);
},

//>	@method array.findNextIndex()
// @include list.findNextIndex
//<
findNextIndex : function (start, property, value, endPos, comparator) {
    if (start == null) start = 0;
    else if (start >= this.length) return -1;
    if (endPos == null) endPos = this.length - 1;
    
    if (property == null) return -1;

    var up = endPos >= start;

	if (isc.isA.String(property)) {
        // single property to match
        if (comparator) {
            for (var i = start; (up ? i <= endPos : i >= endPos) ; (up ? i++ : i--)) {
                if (this[i] && comparator(this[i][property], value, property)) return i;
            }
        } else {
            for (var i = start; (up ? i <= endPos : i >= endPos) ; (up ? i++ : i--)) {
                if (this[i] && this[i][property] == value) return i;
            }
        } 
        return -1;
	
    
	} else if (isc.isA.Function(property)) {
        for (var i = start; (up ? i <= endPos : i >= endPos) ; (up ? i++ : i--)) {
            if (property(this[i])) return i;
        }
        return -1;
    } else {
        // "property" is an object specifying a set of properties to match
        return this.findNextMatch(property, start, endPos, comparator);
	}
},

findAllIndices : function (property, value, comparitor) {
    var matches = [];
    var start = 0;
    var match;
    do {
         
        match = this.findNextIndex(start, property, value, null, comparitor);
        if (match != -1) {
            matches.add(match);
            start = match+1;
        }
        
    } while (match != -1);
    return matches;
},

// internal: assumes multiple properties
findNextMatch : function (properties, start, end, comparator) {
    if (properties._constructor == "AdvancedCriteria") {
        if (isc.DataSource == null) {
            isc.warn("DataBinding module not loaded, AdvancedCriteria not supported for find()/findAll()");
            return -1;
        }
        var dataSource = this.dataSource || isc.DataSource;
        var result = dataSource.applyFilter(this.getRange(start, end + 1), properties);
        if (result.size() != 0) return this.findIndex(result.get(0));
        else return -1;
    }

    var propertyNames = isc.getKeys(properties),
        up = end >= start;

    // This processing is largely duplicated, to avoid a check on comparator in the inner loop
    if (comparator) {
        for (var i = start; (up ? i <= end : i >= end); (up ? i++ : i--)) {
            var item = this.get(i);
            if (!item) continue;
            var found = true;
            for (var j = 0; j < propertyNames.length; j++) {
                var propertyName = propertyNames[j];
                if (!comparator(item[propertyName], properties[propertyName], propertyName)) {
                    found = false;
                    break;
                }
            }
            if (found) return i;
        }
    } else {
        for (var i = start; (up ? i <= end : i >= end); (up ? i++ : i--)) {
            var item = this.get(i);
            if (!item) continue;
            var found = true;
            for (var j = 0; j < propertyNames.length; j++) {
                var propertyName = propertyNames[j];
                if (item[propertyName] != properties[propertyName]) {
                    found = false;
                    break;
                }
            }
            if (found) return i;
        }
    }
    return -1;
},

//>	@method array.find()
// @include list.find
//<
find : function (property, value, comparator) {
    var index = this.findIndex(property, value, comparator);
    return (index != -1) ? this.get(index) : null;
},

// given values for the primary key fields ("record"), find the _index of_ the unique 
// matching record.
// Will automatically trim extra, non-key fields from "record"
findByKeys : function (record, dataSource, pos, endPos) {
    if (record == null) {
        //>DEBUG
        isc.Log.logWarn("findByKeys: passed null record");
        //<DEBUG
        return -1;
    }

    // get the values for all the primary key fields from the passed record
    var findKeys = {}, 
        keyFields = dataSource.getPrimaryKeyFields(),
        hasKeys = false;
    
    for (var keyField in keyFields) {
        hasKeys = true;
        if (record[keyField] == null) {
            //>DEBUG
            isc.Log.logWarn("findByKeys: passed record does not have a value for key field '"
            			 + keyField + "'");
            //<DEBUG
            return -1;
        }
        findKeys[keyField] = record[keyField];
    }

    if (!hasKeys) {
        //>DEBUG
        isc.Log.logWarn("findByKeys: dataSource '" + dataSource.ID + "' does not have primary " +
                     "keys declared, can't find record"); 
        //<DEBUG
        return -1;
    }

    // go through the recordSet looking for a record with the same values for the primary keys
    return this.findNextIndex(pos, findKeys, null, endPos);
},

//>	@method		array.containsProperty()
//  Determine whether this array contains any members where the property passed in matches the value
//  passed in.
//
//		@group	find
//		@param	property	(string)	property to look for
//							(object)	key:value pairs to look for
//		@param	[value]		(any)		value to compare against (if property is a string)
//
//		@return	(boolean)   true if this array contains an object with the appropriate property value
// @visibility external
//<
containsProperty : function (property, value) {
    var index = this.findIndex(property, value);
    return (index != -1);
},

//>	@method array.findAll()
// @include list.findAll
//<
findAll : function (property, value, comparator) {

    if (property == null) return null;

	if (isc.isA.String(property)) {
        var matches = null,
            l = this.length;

        // single property to match
        var multiVal = isc.isAn.Array(value),
            hasComparator = (comparator != null);
        for (var i = 0; i < l; i++) {
            var item = this[i];
            if (item && (multiVal ?
                    value.contains(item[property], null, comparator) :
                    (hasComparator ?
                        comparator(item[property], value) :
                        item[property] == value)))
            {
                if (matches == null) matches = [];
                matches.add(item);
            }
		}
        return matches;

    
	} else if (isc.isA.Function(property)) {
        var matches = null,
            l = this.length,
            iterator = property,
            context = value;

        for (var i = 0; i < l; i++) {
            var item = this[i];
            if (iterator(item, context)) {
                if (matches == null) matches = [];
                matches.add(item);
            }
		}
        return matches;
	} else {
        // "property" is an object specifying a set of properties to match
        return this.findAllMatches(property, comparator);
	}
},

// internal: assumes multiple properties
findAllMatches : function (properties, comparators) {
    var l = this.getLength(),
        propertyNames = isc.getKeys(properties),
        matches = null,
        hasComparators = (comparators != null),
        singleComparator = (hasComparators && !isc.isAn.Object(comparators) && comparators);
    
    if (properties._constructor == "AdvancedCriteria") {
        if (isc.DataSource == null) {
            isc.warn("DataBinding module not loaded, AdvancedCriteria not supported for find()/findAll()");
            return -1;
        }
        var dataSource = this.dataSource || isc.DataSource;
        return dataSource.applyFilter(this.getRange(0, this.getLength() + 1), properties);
    }
    for (var i = 0; i < l; i++) {
        var item = this.get(i);
        if (!item) continue;
        var found = true;
        for (var j = 0; j < propertyNames.length; j++) {
            var propertyName = propertyNames[j],
                comparator = (hasComparators && (singleComparator || comparators[propertyName])),
                itemValue = item[propertyName],
                propertiesValue = properties[propertyName];
            if (comparator ?
                !comparator(itemValue, propertiesValue) :
                (itemValue != propertiesValue))
            {
                found = false;
                break;
            }
        }
        if (found) {
            if (matches == null) matches = [];
            matches.add(item);
        }
    }
    return matches;
},

//>	@method		array.slide()	(A)
// Slide element at position start to position destination, moving all the other elements to cover
// the gap.
//
//		@param	start		(number)	start position
//		@param	destination	(number)	destination position for the value at start
// @visibility external
//<
slide : function (start, destination) {
    this.slideRange(start, start+1, destination);
},

//>	@method		array.slideRange()	(A)
// Slide a range of elements from start to end to position destination, moving all the other
// elements to cover the gap.
//
//		@param	start		(number)	start position
//		@param	end         (number)	end position (exclusive, like substring() and slice())
//		@param	destination	(number)	destination position for the range
// @visibility external
//<
slideRange : function (rangeStart, rangeEnd, destination) {
    if (rangeStart == destination) return;
    // remove the range to be moved
    var removed = this.splice(rangeStart, rangeEnd - rangeStart);
    // and add it at the destination
    this.addListAt(removed, destination);
},

//>	@method		array.slideList()	(A)
// Slide the array of rows list to position destination.
//
//		@param	start		(number)	start position
//		@param	destination	(number)	destination position for this[start]
//<
slideList : function (list, destination) {
	var output = [], 
		i
	;

//XXX if destination is negative, set to 0 (same effect, cleaner code below)
if (destination < 0) destination = 0;

	// take all the things from this table before destination that aren't in the list to be moved
	for(i = 0;i < destination;i++)
		if (!list.contains(this[i]))
			output.add(this[i]);

	// now put in all the things to be moved
	for(i = 0;i < list.length;i++)
		output.add(list[i]);

	// now put in all the things after destination that aren't in the list to be moved
	for(i = destination;i < this.length;i++)
		if (!list.contains(this[i]))
			output.add(this[i]);

	// now copy the reordered list back into this array
	this.setArray(output);
},

//>	@method		array.makeIndex()	(A)
// Make an index for the items in this Array by a particular property of each item.
// <P>
// Returns an Object with keys for each distinct listItem[property] value.  Each key will point
// to an array of items that share that property value.  The sub-array will be in the same order
// that they are in this list.
//
//		@param	property		(strings)			names of the property to index by
//		@param	alwaysMakeArray	(boolean : false)	
//              if true, we always make an array for every index.  if false, we make an Array only
//              when more than one item has the same value for the index property
//		@return	(object)					index object
// @visibility external
//<
// NOTE: we don't document the awkard -1 param to allow collisions
makeIndex : function (property, alwaysMakeArray, useIndexAsKey) {
	var index = {};
    var allowCollisions = (alwaysMakeArray == -1);
    alwaysMakeArray = (alwaysMakeArray != null && alwaysMakeArray != 0);
	for (var i = 0; i < this.length; i++) {
		var item = this[i],
			key = item[property]
		;

        // if the item has no value for the key property
        if (key == null) {
            // either skip it..
            if (!useIndexAsKey) continue;
            // or place it in the index under its position in the array
            key = i;
        }

        if (allowCollisions) {
            index[key] = item;
            continue;
        }
 
        var existingValue = index[key];
        if (existingValue == null) {
            if (alwaysMakeArray) {
                // every entry should be an array
                index[key] = [item];
            } else {
                index[key] = item;
            }
        } else {
            if (alwaysMakeArray) {
                // any non-null value is an array we created the first time we found an item
                // with this key value
                index[key].add(item);
            } else {
                // if the existing value is an array, add to it, otherwise put the new and old
                // value together in a new array
                if (isc.isAn.Array(existingValue)) {
                    index[key].add(item);
                } else {
                    index[key] = [existingValue, item];
                }
            }
        }
	}
	
	return index;	
},


//>	@method		array.arraysToObjects()	(A)
// Map an array of arrays to an array of objects.
// <P>
// Each array becomes one object, which will have as many properties as the number of property
// names passed as the "propertyList".  The values of the properties will be the values found
// in the Array, in order.
// <P>
// For example:
// <pre>
//    var arrays = [
//       ["Mickey", "Mouse"],
//       ["Donald", "Duck"],
//       ["Yosemite", "Sam"]
//    ];
//    var objects = arrays.arraysToObjects(["firstName", "lastName"]);
// </pre>
// <code>objects</code> is now:
// <pre>
//    [
//       { firstName:"Mickey", lastName:"Mouse" },
//       { firstName:"Donald", lastName:"Duck" },
//       { firstName:"Yosemite", lastName:"Sam" }
//    ]
// </pre>
//
//		@param	propertyList	(Array of String)		names of the properties to assign to
//
//		@return	(Array of Object)		corresponding array of objects
//<
arraysToObjects : function (propertyList) {
	// get the number of properties we're dealing with
	var propLength = propertyList.length;
	// for each item in this array
	for (var output = [], i = 0, l = this.length; i < l; i++) {
		// make a new object to hold the output
		var it = output[i] = {};
		// for each property in the propertyList list
		for (var p = 0; p < propLength; p++) {
			var property = propertyList[p];
			// assign that item in the array to the proper name of the new object
			it[property] = this[i][p];
		}		
	}
	// return the list that was generated
	return output;
},

//>	@method		array.objectsToArrays()	(A)
// Map an array of objects into an array of arrays.
// <P>
// Each object becomes one array, which contains the values of a list of properties from
// the source object.
// <P>
// For example:
// <pre>
//    var objects = [
//       { firstName:"Mickey", lastName:"Mouse" },
//       { firstName:"Donald", lastName:"Duck" },
//       { firstName:"Yosemite", lastName:"Sam" }
//    ]
//    var arrays = objects.objectsToArrays(["firstName", "lastName"]);
// </pre>
// <code>arrays</code> is now:
// <pre>
// [
//    ["Mickey", "Mouse"],
//    ["Donald", "Duck"],
//    ["Yosemite", "Sam"]
// ]
// </pre>
//
//		@param	propertyList	(Array of String)		names of the properties to output
//
//		@return	(Array of Object)		corresponding array of arrays
//<
objectsToArrays : function (propertyList) {
	// get the number of properties we're dealing with
	var propLength = propertyList.length;
	// for each item in this array
	for (var output = [], i = 0, l = this.length; i < l; i++) {
		// make a new object to hold the output
		var it = output[i] = [];
		// for each property in the propertyList list
		for (var p = 0; p < propLength; p++) {
			var property = propertyList[p];
			// assign that item in the array to the proper name of the new object
			it[p] = this[i][property];
		}
	}
	// return the list that was generated
	return output;	
},

//>	@method		array.spliceArray()	
// 			Like array.splice() but takes an array (to concat) as a third parameter,
//          rather than a number of additional parameters.
//
//		@param	startPos	(number)		starting position for the splice
//      @param  deleteCount (number)        Number of elements to delete from affected array
//      @param  newArray    (any[])         Array of elements to splice into existing array
//
//		@return	(any[])		array of removed elements
//<
spliceArray : function (startPos, deleteCount, newArray) {

    var undefined;

    if (startPos === undefined) return this.splice();
    if (deleteCount === undefined) return this.splice(startPos);
    if (newArray === undefined) return this.splice(startPos, deleteCount);
    if (!isc.isAn.Array(newArray)) {
        isc.Log.logWarn("spliceArray() method passed a non-array third parameter. Ignoring...", "Array");
        return this.splice(startPos, deleteCount); 
    }

    // use 'apply' - allows you to pass in the arguments as an array!
    // xxx - 
    // Note 1: Another syntax for this would be of this form
    // if(newArray.length <= 10) return this.splice(startPos, deleteCount, newArray[0], ...)
    // else return this.splice.apply(...)
    // but seems no better performance-wise, and since (at least in our overridden implementation of
    // splice for IE 5.0) we use arguments.length, is unreliable unless we have a plethora of 
    // if ... then/s to pass in exactly the right number of arguments.
    //
    // Note 2: you have to use concat, rather than splice to put startPos / deleteCount  at the
    // beginning of newArray, as newArray points to an array object that may be being reused 
    // elsewhere, so we can't modify it.
    //
    return this.splice.apply(this, [startPos, deleteCount].concat(newArray))
  
},

// stack peek method - returns the top item on the stack without removing it.
peek : function () {
    var item = this.pop();
    this.push(item);
    return item;
},

// see ResultSet.getCachedRow()
getCachedRow : function (rowNum) {
    return this[rowNum];
},

//
// ----------------------------------------------------------------------------------
// add the observation methods to the Array.prototype as well so we can use 'em there
//

observe: isc.Class.getPrototype().observe, 
ignore : isc.Class.getPrototype().ignore,

// Synonyms and backcompat
// --------------------------------------------------------------------------------------------

    //>!BackCompat 2004.6.15 for old ISC names
    removeItem : function (pos) { return this.removeAt(pos) },
    getItem : function (pos) { return this.get(pos) },
    setItem : function (pos) { return this.set(pos) },
    // NOTE: instead of calling clearAll(), setLength(0) should be called (which is much more
    // efficient), however clearAll() still exists to support the old behavior of returning the
    // removed items.
    clearAll : function (list) { return this.removeList(this) },
    //<!BackCompat

    // Support for java.util.List API
    size : function () { return this.getLength() },
    subList : function (start, end) { return this.getRange(start, end) },
    addAll : function (list) { return this.addList(list); },
    removeAll : function (list) { 
        var origLength = this.getLength();
        this.removeList(list); 
        return this.getLength() != origLength; // return whether list was changed
    },
    clear : function () { this.setLength(0); },
    toArray : function () { return this.duplicate(); }
    // NOTE: incomplete compatibility:
    // - no iterators.  This exists in Java largely for concurrent modification reasons.
    // - remove(int): in Java, the collision between remove(int) and remove(object) is
    //   implemented by method overloading.  In JS, we assume if you pass a number you want
    //   removal by index, but this means remove(5) cannot be used to remove the first instance
    //   of the number 5 from our List.
    // - retainAll: not yet implemented.  Similar to intersect, except the Java version
    //   requires the List to change in place instead of returning the intersection, in order
    //   to preserve the target List's class.
    // - toArray(): in Java, this means go to a native, non-modifiable Array

});


if (!isc.Browser.isIE || isc.Browser.isIE8Strict) {
    Array.prototype.duplicate = Array.prototype.slice;
}

if (Array.prototype.nativeIndexOf != null) {
    Array.prototype.indexOf = function (obj, pos, endPos, comparator) {
        if (pos == null) pos = 0;
        if (endPos == null) endPos = this.length - 1;

        var i;
        if (comparator != null) {
            for (i = pos; i <= endPos; ++i) {
                if (comparator(this[i], obj)) return i;
            }
        } else {
            if (isc.isAn.Instance(obj)) {
                i = this.nativeIndexOf(obj, pos);
                if (i > endPos) i = -1;
                return i;
            }

            for (i = pos; i <= endPos; ++i) {
                if (this[i] == obj) return i;
            }
        }

        return -1;
    };
}

// Fixes to splice() in older browsers.  




//>IE8
// filter() doesn't exist in IE <= 8
if (Array.prototype.filter == null) {

    isc.addMethods(Array.prototype, {

        filter : function (callback, thisObject) {
            var result = [],
                initialLength = this.length; // scan original elements only
            for (var i = 0; i < initialLength; i++) {
                // skip positions for which no elements have been defined
                if (i in this && callback.call(thisObject, this[i])) {
                    result.add(this[i]);
                }
            }
            return result;
        }
    });
    
}
//<IE8
