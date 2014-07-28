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

 





//
//
//	clone() methods for the comm package
//

isc.addGlobal("clone", function (object, objPath) { return isc.Comm._clone(object); });
isc.addGlobal("shallowClone", function (object) { return isc.Comm._shallowClone(object); });

isc.Comm.addClassMethods({

//>	@classMethod isc.clone()
// Create a deep clone of an object that can be edited without affecting the original
// <P>
// All mutable types, including Objects, Arrays and Dates, are copied.  All immutable types
// (Number, String, etc) are just preserved by reference.
// <P>
// Only JavaScript built-in types may be cloned.  SmartClient UI widgets do not support
// cloning, instead, use +link{Class.create()} to make a new component with similar
// configuration.
// <P>
// Does not handle looping references (will infinite loop).
//
// @visibility external
//
//		@group	serialization
//		@param	object		(object)	object to clone
//		@return				(object)	cloned object	
//<
clone : isc.clone,

_clone : function (object) {
    

    // preserve undef vs null (eg slot values)
    var undef;
    if (object === undef) return undef;
	if (object == null) return null;

    // just return immutable types
	if (isc.isA.String(object) || isc.isA.Boolean(object) ||
	    isc.isA.Number(object) || isc.isA.Function(object)) return object;

    // copy mutable types
	if (isc.isA.Date(object)) return object.duplicate();
	
	if (isc.isAn.Array(object)) return isc.Comm._cloneArray(object);
    // allow a clone() function to be implemented
	if (isc.isA.Function(object.clone)) {
        
        if (isc.isA.Class(object)) return isc.echoLeaf(object);
        return object.clone();
    }
	return isc.Comm._cloneObject(object);
},

_cloneArray : function (object) {
	var output = [];
	for (var i = 0, len = object.length; i < len; i++) {
		output[i] = isc.Comm._clone(object[i]);
	}
	return output;
},

_cloneObject : function (object) {
	var output = {};
	for (var key in object) {
		var value = object[key];
        
        if (key == isc.gwtRef || key == isc.gwtModule) continue;
		output[key] = isc.Comm._clone(value);
	}
	return output;
},

//> @classMethod isc.shallowClone()
// Creates a shallow copy of the passed-in Object or Array of Objects, that is, copies all
// properties of an Object to a new Object, so that the clone now has exactly the same property
// values as the original Object.
// <P>
// If <code>shallowClone()</code> is passed an immutable type such as String and Number, it is returned
// unchanged.  Dates are copied via <code>new Date(originalDate.getTime())</code>.
// <P>
// Note that if an Array is passed, all members of the Array will be cloned.  For a copy of an
// Array that contains exactly the same members (not copies), use Array.duplicate().
// <P>
// Only an Array directly passed to <code>shallowClone()</code> is copied.  Arrays contained
// within Arrays will not be copied.
//
// @param object (Object or Array of Object) object to be cloned
// @return (Object or Array of Object) a shallow copy of the passed-in data
// @visibility external
//<
shallowClone : isc.shallowClone,

_shallowClone : function (object) {

    // preserve undef vs null (eg slot values)
    var undef;
    if (object === undef) return undef;
	if (object == null) return null;

    // Avoid attempting to manipulate SGWT Java objects
	if (isc.Browser.isSGWT && window.SmartGWT.isNativeJavaObject(object)) return object;
	
    // just return immutable types
	if (isc.isA.String(object) || isc.isA.Boolean(object) ||
	    isc.isA.Number(object) || isc.isA.Function(object)) return object;

    // copy mutable types
	if (isc.isA.Date(object)) return object.duplicate();
	
	if (isc.isAn.Array(object)) return isc.Comm._shallowCloneArray(object);
	
    // make a shallow clone of the object
	return isc.addProperties({}, object);
},

_shallowCloneArray : function (object) {
    var output = [];
	for (var i = 0, len = object.length; i < len; i++) {
        // don't copy arrays, just return them directly
        if (isc.isAn.Array(object[i])) output[i] = object[i];
		else output[i] = isc.Comm._shallowClone(object[i]);
	}
	return output;
}

});	// END isc.addMethods(isc.Comm, {})
