/*
 * Isomorphic SmartClient
 * Version v10.0p_2014-09-10 (2014-09-10)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */

 





//>	@classMethod isc.getValueForKey()
// Given a key and an object of <code>key:value</code> pairs, return the value that corresponds to
// that key.
// <P>
// If the key is not found, <code>defaultValue</code> will be returned if provided, otherwise the
// key will be returned.
//
//	@param	key				(string or number)	key to look for
//	@param	valueMap		(object)			object of key:value pairs
//	@param	[defaultValue]	(any)				default value to return if key not found
//
//	@return					(any)				returns value in valueMap under name key, or
//                                              defaultValue if key not found
// @visibility external
//<
isc.getValueForKey = function (key, valueMap, defaultValue) {
    
	if (valueMap && valueMap[key] != null && !isc.isAn.Array(valueMap)) return valueMap[key];
	return (arguments.length < 3 ? key : defaultValue);
}

//>	@classMethod isc.getKeyForValue()
// Given a value and an object of <code>key:value</code> pairs, return a key that corresponds
// to that value.
// <P>
// If the key is not found, <code>defaultKey</code> will be returned if provided, otherwise the
// value will be returned.
//
//	@param	value			(string or number)	value to look for
//	@param	valueMap		(object)			object of key:value pairs
//	@param	[defaultKey]	(any)				default key to return if value not found
//
//	@return					(any)				returns first key in valueMap with value, or
//                                              defaultKey if value not found
// @visibility external
//<
isc.getKeyForValue = function (value, valueMap, defaultKey) {
// JMD: handle null value here?
	if (valueMap) {
		for (var key in valueMap) {
			if (valueMap[key] == value) return key;
		}
	}
	return (arguments.length < 3 ? value : defaultKey);
}


//>	@classMethod  isc.makeReverseMap()
// Given a key:value map, return a new map as value:key.
// <P>
// If the same value appears more than once, the key will correspond to the last instance of that
// value.
//
//	@param	valueMap		(object)			object of key:value pairs
//	@return					(object)			reversed value map
// @visibility external
//<
isc.makeReverseMap = function (valueMap) {
	var newMap = {}, value;
	for (var key in valueMap) {
		value = valueMap[key];
		newMap[value] = key;
	}
	return newMap;
}

// returns a new value map, sorted by the key
// technically, maps can't be sorted, but in JS, objects "remember" the order in which key/value
// pairs were added
// XXX add support for normalizers
isc.sortByKey = function (valueMap) {
    var newMap = {},
        keys = isc.getKeys(valueMap).sort()
    ;
    for (var i = 0; i < keys.length; i++) {
        newMap[keys[i]] = valueMap[keys[i]];
    }
    return newMap;
}

// returns a new value map, sorted by the value
// technically, maps can't be sorted, but in JS, objects "remember" the order in which key/value
// pairs were added
// XXX add support for normalizers
isc.sortByValue = function (valueMap) {
    // make a reverse map of the input map; map is now: value -> key
    // call sortByKey on this reversed map
    // reverse the map again (so map is key -> value) and return it
    // XXX horribly inefficient
    return isc.makeReverseMap(isc.sortByKey(isc.makeReverseMap(valueMap)));
}
