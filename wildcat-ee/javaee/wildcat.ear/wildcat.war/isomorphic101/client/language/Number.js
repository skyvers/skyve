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
//>DEBUG
// This lets us label methods with a name within addMethods
Number.prototype.Class = "Number";
  //<DEBUG 


//> @object Number
//
// Extra methods added to the Number object, available on all number variables.
//
//  @visibility external
//  @treeLocation Client Reference/System
//<

isc.addMethods(Number, {
setStandardFormatter : function (functionName) {
    isc.NumberUtil.setStandardFormatter(functionName);
},
setStandardLocaleStringFormatter : function (functionName) {
    isc.NumberUtil.setStandardLocaleStringFormatter(functionName);
}
});

//
// add methods to all Numbers
//
isc.addMethods(Number.prototype, {
//> @method number.stringify()
//
// Return this number as a string padded out to digits length.
//
// @param [digits] (number : 2) Number of digits to pad to.  (Default is 2)
// @return (string) Padded string version of the number
//
// @example var str = myNumberVar.stringify(2);
// @group stringProcessing
// @visibility external
// @deprecated Moved to a static method on NumberUtil to avoid the possibility of collision 
//              with other libraries on the native Number object
//<

stringify : isc.NumberUtil._stringify,

//> @method number.isBetween()
// Returns true if the number parameter falls between the 'first' and 'second' paramters.
//
// @param number (number) Number object to be evaluated
// @param [first] (number) Number at the lower boundary
// @param [second] (number) Number at the upper boundary
// @param [inclusive] (number) Whether or not the numbers at either end of the boundary should be included in the comparison
// @return (Boolean) True if the given <code>number</code> falls inside the given range, false otherwise
// @example n = 3; bool = n.isBetween(3, 3, 6, true); // true
// @example n = 3; bool = n.isBetween(3, 3, 6);       // false
// @visibility external
//<
isBetween : isc.NumberUtil._isBetween,

//> @method number.toCurrencyString()
// Return this number as a currency-formatted string.
//
// @param [currencyChar] (string) Currency symbol, can be set to an empty string. 
//                                If unset <code>"$"</code> will be used.
// @param [decimalChar] (string) Decimal separator symbol. If unset <code>"."</code> will be used.
// @param [padDecimal] (boolean) Should decimal portion be padded out to two digits? True
//                               by default.
// @param [currencyCharLast] (boolean) Should currency symbol come at the end of the string?
//                                      If unspecified, currency symbol will be shown at the 
//                                      beginning of the string.
// @return (string) Currency-formatted string version of the number
// @group stringProcessing
// @visibility external
// @deprecated Moved to a static method on NumberUtil to avoid the possibility of collision 
//              with other libraries on the native Number object
//<

toCurrencyString : isc.NumberUtil._toCurrencyString

// NOTE:
// We don't provide 'setFormatter' or 'setStandardFormatter' instance methods for Numbers.
// This is because 
// a) we don't want to confuse the issue of where formatters are stored (we have a pattern here
//    and on Dates of having standard formatters for all instances only)
// b) (at least in IE), numbers are not allocated as "true instances", so having a 
//     number instance (var theVar = 2;) does not mean that you can set up properties on it, 
//     such as theVar.formatter -- when you next refer to 'theVar', you are really given 
//     another '2' instance, so your properties have been wiped out.

});

//
// add class-methods to the Number object
//  Moved to NumberUtil.js

isc.addProperties(Number.prototype, {
    
// doc and implementation moved to NumberUtil
iscToLocaleString : function () {
    var result = isc.NumberUtil.iscToLocaleString(this);
    return result;
},

// doc and implementation moved to NumberUtil
toFormattedString : function (formatter) {
    var result = isc.NumberUtil.toFormattedString(this, formatter)
    return result;
},

// doc and implementation moved to NumberUtil
toLocalizedString : function (decimalPrecision, decimalSymbol, groupingSymbol, negativeSymbol) {
    var result = isc.NumberUtil.toLocalizedString(this, decimalPrecision, decimalSymbol, 
                groupingSymbol, negativeSymbol);
    return result;
},


toUSString : function(decimalPrecision) {
    var result = isc.NumberUtil.toUSString(this, decimalPrecision);
    return result;
},
toUSDollarString : function (decimalPrecision) {
    return isc.NumberUtil.toUSCurrencyString(this, decimalPrecision);
},
toUSCurrencyString : function(decimalPrecision) {
    var result = isc.NumberUtil.toUSCurrencyString(this, decimalPrecision);
    return result;
}

}) // end addProperties(Number.prototype) for localizable number formatter



isc.defineClass("Format");

isc.Format.addClassMethods({
    toUSString : function (theNum, decimalPrecision) { 
        if (!isc.isA.Number(theNum)) return theNum;
        return isc.NumberUtil.toUSString(theNum, decimalPrecision) 
    },
    toUSCurrencyString : function (theNum, decimalPrecision) { 
        if (!isc.isA.Number(theNum)) return theNum;
        return isc.NumberUtil.toUSCurrencyString(theNum, decimalPrecision) 
    },
    toUSDollarString : function (theNum, decimalPrecision) { 
        if (!isc.isA.Number(theNum)) return theNum;
        return isc.NumberUtil.toUSCurrencyString(theNum, decimalPrecision) 
    },
    toCurrencyString : function (theNum, currencyChar, decimalChar, 
                                 padDecimal, currencyCharLast) {
        if (!isc.isA.Number(theNum)) return theNum;
        return isc.NumberUtil._toCurrencyString(currencyChar, decimalChar, 
                                       padDecimal, currencyCharLast, theNum);
    }
})
