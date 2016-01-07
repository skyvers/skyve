/*
 * Isomorphic SmartClient
 * Version SNAPSHOT_v10.1p_2015-12-10 (2015-12-10)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */
//	
//
//	Array object utilities -- not commonly used but sometimes useful stuff
//


isc.addMethods(Array.prototype, {

//>	@method		array.max() ([])
//
// 			Returns the largest number in the array, skipping non-numeric values.
//
//			If the start and/or end are given, searches the specified subset of the list.
//
//      @visibility external
//		@group	arrayMath
//		@param	[start]		(number)	optional start index (default is 0)
//		@param	[end]		(number)	optional end index (default is list.length)
//
//		@return	(number)	maximum of all items in the list, or null if all values are non-numeric	
//<
max : function (start, end) {
	if (start == null) start = 0;
	if (end == null) end = this.length;

	var max = null;
	
	for (var i = start; i < end; i++) {
		var value = this[i];
		if (isc.isA.Number(value)) {
			if (max == null) max = value;
			else max = Math.max(max, value);
		}
	}

	return max;
},

//>	@method		array.min() ([])
//          
// 			Returns the smallest number in the array, skipping non-numeric values.
//
//			If the start and/or end are given, searches the specified subset of the list.
//
//      @visibility external
//		@group	arrayMath
//		@param	[start]		(number)	optional start index (default is 0)
//		@param	[end]		(number)	optional end index (default is list.length)
//
//		@return	(number)	minimum of all items in the list, or null if all values are non-numeric	
//<
min : function (start, end) {
	if (start == null) start = 0;
	if (end == null) end = this.length;

	var min = null;
		
	for (var i = start; i < end; i++) {
		var value = this[i];
		if (isc.isA.Number(value)) {
			if (min == null) min = value;
			else min = Math.min(min, value);
		}
	}

	return min;
},

//>	@method		array.sum() ([])
// 			Returns the sum of the numbers in the array, skipping non-numeric values.
//
//			If the start and/or end are given, uses only the specified subset of the list.
//
//      @visibility external
//		@group	arrayMath
//		@param	[start]		(number)	optional start index (default is 0)
//		@param	[end]		(number)	optional end index (default is list.length)
//
//		@return	(number)	sum of all items in the list	
//<
sum : function (start, end) {
	if (start == null) start = 0;
	if (end == null) end = this.length;

	var total = 0;
		
	for(var i = start; i < end; i++)
		if(isc.isA.Number(this[i])) total += this[i];
	return total;
},

//>	@method		array.and() ([])
// Returns true if all values between the start and end indices are true.
//
//      @visibility external
//		@group	arrayMath
//		@param	[start]		(number)	optional start index (default is 0)
//		@param	[end]		(number)	optional end index (default is list.length)
//
//		@return	(boolean)		all of the items in the array are true	
//<
and : function (start, end) {
	if (start == null) start = 0;
	if (end == null) end = this.length;

	for(var i = start; i < end; i++)
		if (!this[i]) return false;
	return true;
},

//>	@method		array.or()  ([])
// Returns true if at least one value between the start and end indices is true.
//
//      @visibility external
//		@group	arrayMath
//		@param	[start]		(number)	optional start index (default is 0)
//		@param	[end]		(number)	optional end index (default is list.length)
//
//		@return	(boolean)		at least one of the items is true	
//<
or : function (start, end) {
	if (start == null) start = 0;
	if (end == null) end = this.length;

	var total = 0;
		
	for(var i = start; i < end; i++)
		if (this[i]) return true;
	return false;
}

})	//END isc.addMethods(Array.prototype)
