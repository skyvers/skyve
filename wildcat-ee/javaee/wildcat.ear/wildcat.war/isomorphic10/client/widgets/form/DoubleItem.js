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




//> @groupDef gwtFloatVsDouble
// In GWT code, you should generally use Java Doubles rather than Java Float values.
// <p>
// In the current implementation of GWT, Float and Double types are both represented as
// JavaScript Number, so there is no storage or performance advantage to using Float over
// Double, and double provides higher precision.
// <p>
// In addition, because GWT uses true Java Floats <i>in development mode</i> but uses higher
// precision JavaScript Number values in compiled mode, math operations on Float can differ
// between development mode vs compiled mode.
// <p>
// The SmartGWT field type "float" is represented as a JavaScript Number, the same storage GWT
// uses for Doubles, so in any code that accesses or manipulates values stored in a field of
// type "float", use Record.getAttributeAsDouble(), DoubleItem.getValueAsDouble(), and similar
// APIs to avoid being tripped up by GWT's different behavior in development mode.
//
// @title Float vs Double
// @visibility sgwt 
//<

//> @class DoubleItem
//TextForm item for managing a text field that displays a decimal value.
//@visibility external
//<
isc.ClassFactory.defineClass("DoubleItem", "FloatItem");

isc.DoubleItem.addMethods({

    //> @method DoubleItem.getValueAsDouble()
    // Return the value tracked by this form item as a Double.  If the value cannot
    // be parsed to a valid double, null will be returned.
    //
    // @return (Double) the value of this element
    //
    // @see method:FormItem.getValue
    // @visibility external
    //<
    
    getValueAsDouble : function () {
        var origValue   = this.getValue(),
            parsedValue = parseFloat(origValue);
        return isNaN(parsedValue) ? null : parsedValue;
    }

});

