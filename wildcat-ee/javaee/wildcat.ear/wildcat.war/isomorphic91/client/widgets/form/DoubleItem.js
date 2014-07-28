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

//> @method DoubleItem.getValueAsDouble()
// Return the value tracked by this form item.
// @return (Double) the value of the form, converted to a double.
// @visibility external
//<

//> @class DoubleItem
//TextForm item for managing a text field that displays a decimal value.
//@visibility external
//<
isc.ClassFactory.defineClass("DoubleItem", "FloatItem");

