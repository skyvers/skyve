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

 



//> @interface List
// An interface for an ordered collection of items.
// <P>
// This is the interface that is expected by list-oriented display components such as the
// ListGrid.  The JavaScript native Array object is retrofitted to support the List interface.
// Also, a valid List can be created by mixing the List interface into any class that supports:
// <ul>
// <li> for read-only support: get(position), getLength()
// <li> for modifiable support: set(position), addAt(object, position), removeAt(position)
// </ul>
// <P>
// NOTE: this interface is compatible with the java.util.List interface, except that:
// <ul>
// <li> for removal by index, removeAt() must be called instead of remove().  In Java, remove()
// is an overloaded method that takes either an int or Object, whereas in JavaScript, a
// Number is an Object.
// <li> Iterators don't exist.
// </ul> 
// Some methods from the Java List interface have been omitted from the documentation to avoid
// redundancy.
//
// @treeLocation Client Reference/System
// @visibility external
//<
isc.ClassFactory.defineInterface("List");

// Read-only
// --------------------------------------------------------------------------------------------
// - basics that must be implemented
//   "get", "getLength",
//
// - routines that can be implemented in terms of basics
//   - trivial routines
//     "isEmpty", "itemIsPresent", "rangeIsPresent", "first", "last", 
//   - routines to consider for a custom implementation for performance
//     "indexOf", "lastIndexOf", "contains", "containsAll", "intersect", 
//     "getItems", "getRange", "duplicate", 

// Modification
// --------------------------------------------------------------------------------------------
// - basics that must be implemented
//   "set", "addAt", "removeAt",
//   - NOTE: technically, set() and setLength() are sufficient as a minimal interface, however,
//     this means that addAt() and removeAt() would need to be implemented by
//     lengthening/shortening the List and then setting every slot beyond the modified point,
//     which seems like a very silly default implementation.

// - routines that can be implemented in terms of basics
//   - trivial routines
//     "add", "addList",
//   - routines to consider for a custom implementation for performance
//      "setLength", "addListAt", "remove", "removeList", "sort", "sortByProperty"

//   - notifications
//     "dataChanged", "_startChangingData", "_doneChangingData", 




isc.List.addInterfaceMethods({

init : function () {
	if (!this.data) this.data = [];
},

// --------------------------------------------------------------------------------------------
// Read-only Interface
// --------------------------------------------------------------------------------------------

// Basics that must be implemented
// --------------------------------------------------------------------------------------------

//>	@method		list.get()
// Return the item at a particular position
// @group access
//
//		@param	pos	(Number)	position of the element to get
//
//		@return	(object)		whatever's at that position, undef if not found	
// @visibility external
//<
// expected to be implemented by target

//>	@method		list.getLength()
// Return the number of items in this list
//
// @group access
//
//		@return	(Number)	number of items in the list
// @visibility external
//<
// expected to be implemented by target

// Implementable in terms of basics
// --------------------------------------------------------------------------------------------

//>	@method		list.isEmpty()
// Return whether or not this array is empty
//
// @group access
//		@return	(boolean)	true == this array is empty, false == some items in the array	
// @visibility external
//<
// [stolen from Array]

//>	@method		list.first()
// Return the first item in this list
//
// @group access
//		@return	(any)	first item in the list
// @visibility external
//<
first : function () {
	return this.get(0);
},

//>	@method		list.last()
// Return the last item in this list
//
// @group access
//		@return	(any)	last item in the list
// @visibility external
//<
last : function () {
	return this.get(this.getLength()-1);
},

// Below might need custom implementations for performance
// --------------------------------------------------------------------------------------------

//>	@method		list.indexOf()
// Return the position in the list of the first instance of the specified object.
// <p>
// If pos is specified, starts looking after that position.
// <p>
// Returns -1 if not found.
//
// @group access
//		@param	obj		 (any)		object to look for
//		@param	[pos]	 (number)	earliest index to consider
//		@param	[endPos] (number)	last index to consider
//
//		@return	(number)			position of the item, if found, -1 if not found	
// @visibility external
//<
indexOf : function (obj, pos, endPos) {
	// normalize position to the start of the list
	if (pos == null) pos = 0;
    if (endPos == null) endPos = this.getLength() - 1;
	
	for (var i = pos; i <= endPos; i++) {
		if (this.get(i) == obj) return i;
	}

	// not found -- return the not found flag
	return -1;
}, 

//>	@method		list.lastIndexOf()
// Return the position in the list of the last instance of the specified object.
// <p>
// If pos is specified, starts looking before that position.
// <p>
// Returns -1 if not found.
//
// @param	obj		(any)		object to look for
// @param	[pos]	 (number)	last index to consider
// @param	[endPos] (number)	earliest index to consider
//
// @return	(number)			position of the item, if found, -1 if not found	
//
// @group access
// @visibility external
//<
lastIndexOf : function (obj, pos, endPos) {
	// normalize position to the end of the list
	if (pos == null) pos = this.getLength() - 1;
    if (endPos == null) endPos = 0;
	
	for (var i = pos; i >= endPos; i--)
		if (this.get(i) == obj) return i;

	// not found -- return the not found flag
    return -1;
},

//>	@method		list.findIndex()
// Find the index of the first Object where property == value in the object.
// <P>
// Pass an Object instead to match multiple properties.
// <P>
// Note: for string values, matches are case sensitive.
//
// @param propertyName (String or Object or AdvancedCriteria) property to match, or if an Object is passed, set of
//                                        properties and values to match
// @param [value] (any) value to compare against (if propertyName is a string)
// @return (int) index of the first matching Object or -1 if not found
//
// @group access, find
// @visibility external
//<
// [stolen from Array]

//>	@method		list.findNextIndex()
// Like +link{findIndex()}, but inspects a range from startIndex to endIndex.
//
// @param startIndex (int) first index to consider
// @param propertyName (String or Object or AdvancedCriteria) property to match, or if an Object is passed, set of
//                                        properties and values to match
// @param [value] (any) value to compare against (if propertyName is a string)
// @param [endIndex] (int) last index to consider
// @return (int) index of the first matching Object or -1 if not found
//
// @group access, find
// @visibility external
//<

findNextIndex : function (start, property, value, endPos) {
    var length = this.getLength();    
    if (start == null) start = 0;
    else if (start >= length) return -1;
    if (endPos == null) endPos = length - 1;
    if (property == null) return -1;

	if (isc.isA.String(property)) {
        // single property to match
		for (var i = start; i <= endPos; i++) {
            var item = this.get(i);    
			if (item && item[property] == value) return i;
        }
        // return -1 if we didn't find the object
        return -1;
	} else {
        // "property" is an object specifying a set of properties to match
        return this.findNextMatch(property, start, endPos);
	}
},

//>	@method list.find()
// Like +link{findIndex()}, but returns the object itself instead of its index.
//
// @param propertyName (String or Object or AdvancedCriteria) property to match, or if an Object is passed, set of
//                                        properties and values to match
// @param [value] (any) value to compare against (if propertyName is a string)
// @return (Object) first matching object or null if not found	
//
// @group access, find
// @visibility external
//<
// [stolen from Array]

//>	@method list.findAll()
// Find all objects where property == value in the object.
// <P>
// Pass an Object as the <code>propertyName</code> argument to match multiple properties.
//
// @param propertyName (String or Object or AdvancedCriteria) property to match, or if an Object is passed, set of
//                                        properties and values to match
// @param [value] (any) value to compare against (if propertyName is a string)
// @return (Array) all matching Objects or null if none found	
//
// @group access, find
// @visibility external
//<
findAll : function (property, value) {

    if (property == null) return null;

	if (isc.isA.String(property)) {
        var matches = null,
            l = this.getLength()
        ;
        // single property to match
        for (var i = 0; i < l; i++) {
            var item = this.get(i);
			if (item && item[property] == value) {
                if (matches == null) matches = [];
                matches.add(item);
            }
		}
        return matches;
	} else {
        // "property" is an object specifying a set of properties to match
        return this.findAllMatches(property);
	}
},

//>	@method		list.contains()
// Return if this list contains the specified object.
// <P>
// If pos is specified, starts looking after that position.
//
// @group access
//		@param	obj		(any)		item to look for
//		@param	[pos]	(number)	optional position in the list to look after
//	
//		@return	(boolean)	true == item was found, false == not found
// @visibility external
//<
// [stolen from Array]

//> @method     list.containsAll()
// Return whether this list contains all the item in the specified list.
//
// @group access
//      @param list     (List)      items to look for
//      @return (boolean)   whether all items were found
// @visibility external
//<
// [stolen from Array]

//>	@method		list.intersect()
// Return the list of items that are in both this list and the passed-in list(s).
//
//		@group	arrayMath
//
//		@param	lists    (all List arguments)		lists to intersect with
//		@return	(List)	intersection
// @visibility external
//<
// [stolen from Array]

//> @method     list.equals()
// Return whether this list is equal to another list.
// <P>
// Two lists are equal only if they have the same length and all contained items are in the same
// order and are also equal.
//
// @group access
//      @param list     (List)      list to check for equality
//      @return (boolean)   whether the specified list is equal to this list
// @visibility external
//<
// [stolen from Array]

//>	@method		list.getItems()
// Return the items at a list of specified positions.
//
// @group access
//		@param	itemList	(List of Number)		array of positions
//
//		@return	(array)		subset of the array, in the same order as itemList
// @visibility external
//<
// [stolen from Array]

//>	@method		list.getRange()
// Return the items between position start and end, non-inclusive at the end.
//
// @group access
//		@param	start	(number)	start position
//		@param	end		(number)	end position
//
//		@return	(Array)		subset of the array from start -&gt; end-1
// @visibility external
//<
getRange : function (start, end) {
    if (end == null) end = this.getLength() - 1;
    var output = [];
    for (var i = start; i < end; i++) {
        output[output.length] = this.get(i);
    }
	return output;
},

// see ResultSet.getCachedRow()
getCachedRow : function (rowNum) { return this.get(rowNum); },

//>	@method		list.duplicate()	(A)
// Return an Array that is a shallow copy of the list, that is, containing the same items.
//
// @group access
//		@return	(Array)		new array, pointing to the same items	
// @visibility external
//<
duplicate : function () {
    return this.getClass().create().addList(this);
},

// --------------------------------------------------------------------------------------------
// Modification Interface
// --------------------------------------------------------------------------------------------

// Basics that must be implemented
// --------------------------------------------------------------------------------------------

//>	@method		list.set()
// Change the array element at a particular position.
// <P>
// set() can be used to expand the length of the list.
//
// @group modification
//		@param	pos	(number)	position in the list to change
//		@param	obj	(object)    new value for that position
//
//		@return	(object)		whatever's at that position, null if not found	
// @visibility external
//<
// expected to be implemented by target

//>	@method		list.addAt()
// Add a single item to this array at a specific position in the list, sliding other items over
// to fit.
//
// @group modification
//		@param		obj	(object)	object to add
//		@param		pos	(number)	position in the list to add at
//
//		@return			(object)	object that was added	
// @visibility external
//<
// expected to be implemented by target

//>	@method		list.removeAt()
// Remove the item at the specified position, rearranging all subsequent items to fill the gap
//
// @group modification
//		@param	pos	(number)	position to remove
//
//		@return	(any)	item that was removed
// @visibility external
//<
// expected to be implemented by target

// Implementable in terms of basics
// --------------------------------------------------------------------------------------------

//>	@method		list.add()
// Add an object to this list, at the end
//
// @group modification
//		@param	object	(any)	object to add
//
//		@return	(any)			pointer to the object passed in	
// @visibility external
//<
// As implemented below, if no position is specified, the object will be added to the end of
// the list.
// Note that array.add will attempt to preserve sort order if it is currently sorted when
// add() is called with no explicit position param.

add : function (object, secondArg) {
    var undefined;
    if (secondArg !== undefined) {
        // support calling as add(object, index)
        return this.addAt(object, secondArg);
    }
    this.addAt(object, this.getLength());	
	
	// return the object that was added
	return object;
},

//>	@method		list.addList()
// Add a list of items to this array.
// <P>
// Note: you can specify that a subset range be added by passing start and end indices
//
// @group modification
//		@param	list	(array)		list of items to add
//		@param	[listStartRow]	(number)	optional start index in list
//		@param	[listEndRow]	(number)	optional end index in list (non-inclusive)
//
//		@return	(list)				list of items that were added
// @visibility external
//<
// [stolen from Array]

// Below might need custom implementations for performance
// --------------------------------------------------------------------------------------------

//>	@method		list.setLength()
// Set the length of this list.
// <P>
// If the length of the list is shortened, any elements past the new length of the list are removed.
// If the length is increased, all positions past the old length have the value
// <code>undefined</code>.
//
// @group modification
//		@param	length	(number)	new length
// @visibility external
//<
setLength : function (length) {
	this._startChangingData();
    if (length > this.getLength()) {
        // pad the list with empty slots
        var undefined;
        while (length > this.getLength()) this.add(undefined);
    } else {
        // remove everything beyond the specified length
        while (length < this.getLength()) this.removeAt(this.getLength()-1);
    }
	this._doneChangingData();
},

// Below methods need to shift indices of all existing items
// --------------------------------------------------------------------------------------------

//>	@method		list.addListAt()
// Add list of items list to this array at item pos.  All items after array[pos] will slide down to
// fit new items.
//
// @group modification
//		@param	list	(array)		new array of items
//		@param	pos		(number)	position in this list to put the new items
//
//		@return	(array)		the list of items that was added
// @visibility external
//<
addListAt : function (list, pos) {
	this._startChangingData();
	
    var length = list.getLength();
    for (var i = 0; i < length; i++) {
        this.addAt(list.get(i), pos+i);
    }
	
	this._doneChangingData();
	
	// return the list that was added
	return list;
}, 

//>	@method		list.remove()
// Remove first instance of the passed object from this array, sliding other items around to
// fill gaps.
//
// @group modification
// @param obj (any) item to remove
//
// @return (boolean) true if a matching object was found and removed, false if no matching
// object was found and the list remains unchanged.
// @visibility external
//<
remove : function (obj) {
    
    // return removed item, per java.util.List
    // if (isc.isA.Number(obj)) return this.removeAt(obj); 

    var index = this.indexOf(obj);
    if (index == -1) return false;

	this._startChangingData();

    var length = this.getLength();
	for (var i = index; i < length; i++) this.set(i, this.get(i+1));
    this.setLength(length-1);

	this._doneChangingData();

    return true; // indicating object was removed, per java.util.Collection
}, 

//>	@method		list.removeList()
// Remove all instances of objects in the specified list from this list, sliding the remaining
// objects around to fill gaps.
//
// @group modification
//		@param	list	(array)		list of items to remove
//
//		@return	(list)	list of items passed in
// @visibility external
//<
removeList : function (removeList) {
    if (removeList == null) return null;

	// get ready to change data...
	this._startChangingData();

    var changed = false;
	for (var i = 0; i < this.getLength(); i++) {
        var item = this.get(i);

        // remove the current item and stay at the same position in the list
        if (removeList.contains(item)) {
            changed = true;
            this.removeAt(i);
            i--;
        }
	}
	this._doneChangingData();
	
	// return whether the list was changed
	return removeList;
}, 



//> @method    list.sort()
// Sorts the elements of the List in place.
// <P>
// The optional comparator function should take two parameters "a" and "b" which are the two list
// items to compare, and should return:
// <ul>
// <li> a value less than zero, if "a" is less than "b" such that "a" should appear earlier in the
//      list
// <li> zero, if "a" and "b" are equal
// <li> a value greater than zero, if "a" is greater than "b" such that "b" should appear earlier in
//      the list
// </ul>
// 
//     @param    [comparator]  (function) comparator function to use
//     @return   (List)                   the list itself
// @visibility external
//<
sort : function (comparator) {
    // dump all the items to a native Array and sort them
    var items = this.getRange(0, this.getLength());
    items.sort(comparator);

    // then set every slot in the current List
    for (var i = 0; i < items.length; i++) this.set(i, items[i]);

    return this;
},

//>	@method list.getProperty()
// Return a new Array where the value of item i is the value of "property" of item i in this
// array.  If an item doesn't have that property or is null, return item will be null.
//
// @param property (string)	name of the property to look for
//
// @return (Array) array of the values of property in each item of this list	
// @group iteration
// @visibility external
//<
getProperty : function (property) {
    var values = [];
    // then set every slot in the current List
    for (var i = 0; i < this.getLength(); i++) {
        var item = this.get(i);
        values[i] = item != null ? item[property] : null;
    }
    return values;
},


//>	@method		list.sortByProperty()
// Sort a list of objects by a given property of each item.
// <P>
// The optional normalizer, if passed as a function, is called for each item in the List, and
// should return whatever value should be used for sorting, which does not have to agree with
// the property value. By passing a normalizer function you can achieve any kind of sorting
// you'd like, including sorting by multiple properties.
// <P>
// NOTE: string sort is case INsensitive by default
//
//		@group	sorting
//
//		@param	property 	 (string)	name of the property to sort by
//		@param	up 			 (boolean)	true == sort ascending, false == sort descending
//		@param	[normalizer] (function or ValueMap) 	
//              May be specified as a function, with signature 
//              <code>normalize(item, propertyName, context)</code>, where <code>item</code> is
//              a pointer to the item in the array, <code>propertyName</code> is the 
//              property by which the array is being sorted, and <code>context</code> is the
//              arbitrary context passed into this method. Normalizer function should return
//              the value normalized for sorting.<br>
//              May also be specified as a ValueMap which maps property values to sortable values.
//      @param [context] (any) Callers may pass an arbitrary context into the sort method, which
//                          will then be made available to the normalizer function
//      @return (List) the list itself
//
// @visibility external
//<
sortByProperty : function (property, direction, normalizer, context) {
    // dump all the items to a native Array and sort them
    var items = this.getRange(0, this.getLength());
    items.sortByProperty(property, direction, normalizer, context);

    // then set every slot in the current List
    for (var i = 0; i < items.length; i++) this.set(i, items[i]);

    return this;
},


//> @method list.getValueMap()
// Get a map of the form <code>{ item[idField] -&gt; item[displayField] }</code>, for all 
// items in the list.  Note that if more than one item has the same <code>idProperty</code>, 
// the value for the later item in the list will clobber the value for the earlier item.
//
// @param idField (string)  Property to use as ID (data value) in the valueMap
// @param displayField (string) Property to use a display value in the valueMap
// @return (object) valueMap object
// @visibility external
//<
// imported as part of isc._stealArrayMethods 


// DataChanged notification
// --------------------------------------------------------------------------------------------

//>	@method		list.dataChanged()	(A)
// Method called when this array changes in some way.  Observe the method to react to changes in
// this list.
// <P>
// Note: dataChanged() will only fire when items are added, removed or rearranged.  If a list
// contains objects, dataChanged() will not fire if changes are made to objects within the list
// without changing their position within the list.  If an observer of dataChanged() needs to react
// to such a change, you can manually fire dataChanged() by simply calling it.
// <P>
// Note: may be called multiple times as the result of a multi-item add or remove, etc.
//
// @group modification
// @visibility external
//<
dataChanged : function () {
    
    if (this.onDataChanged) this.onDataChanged()
}

//>	@method		list._startChangingData()	(A)
//			Internal method to indicate that data will be changed within the context of a function.
//			Each occurance of a call to this method should be matched with an occurance of
//			_doneChangingData() -- when they balance, the public dataChanged() method will be called
//			exactly once.  This lets observers of the dataChanged() method only get called once for
//			a set of changes.
//	
//			For example, clearRange() calls clearItem() repeatedly; we don't want each of 
//			 these "nested" calls to clearItem to generate a dataChanged message,
//			 (we only want one when clearRange is done).  However, clearItem() when called by
//			 itself (outside of any higher-level operation) *should* call dataChanged when it
//			 is done.
//
//<
// [stolen from Array]

//>	@method		list._doneChangingData()	(A)
//			Internal method to indicate that we're done changing data in the current scope.
//			See list._startChangingData()
//<
// [stolen from Array]

});

// steal methods from Array 
isc._stealArrayMethods = function () {
    var methodList = [
                      // these are internal helpers only
                      "containsSubstring", "containsAllSubstring", "intersectDates", "intersectSubstring", 
                      // methods that are implemented using only the List API on Array (because
                      // performance difference doesn't matter)
                      "isEmpty", "contains", "containsAll", "intersect", "equals", 
                      "getItems", "addList", "getValueMap", "removeEvery",
                      "_startChangingData", "_doneChangingData", "_isChangingData",
                      // old ISC backcompat
                      "getItem", "setItem", "removeItem", "clearAll",
                      // find
                      "find", "findIndex", "findAllIndices", "findNextMatch", "findAllMatches", "findByKeys",
                      // Java.util.List compat
                      "size", "subList", "addAll", "removeAll", "clear"];
    // NOTE: applyMask won't work here, since the input is an Array instance, which is
    // ambiguous with passing an Array full of objects to mask.
    var methods = {};
    for (var i = 0; i < methodList.length; i++) {
        var methodName = methodList[i];
        methods[methodName] = Array.prototype[methodName];
    }
        
    isc.List.addInterfaceMethods(methods);
}
isc._stealArrayMethods();


// Override isA.List to return true for arrays as well as lists
// we have to do this here after the List interface has been defined...
isc.addMethods(isc.isA, {
//> @classMethod isA.List()
// Does <code>object</code> implement the  <code>List</code> interface?
// @param   object  (object)    object to test
// @return (boolean) <code>true</code> if the object is an Array or belongs to another class that
//                   implements the <code>List</code> API.
// @visibility external
//<
_$List:"List",
List : function (object) {
    if (object == null) return false;
    if (isc.isA.Array(object)) return true;
    // standard implementation for objects inheriting from interfaces/classes
    return object.isA && object.isA(this._$List);
}

});

    
