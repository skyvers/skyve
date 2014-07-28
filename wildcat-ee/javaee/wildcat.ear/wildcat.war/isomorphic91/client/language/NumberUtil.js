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




//> @class NumberUtil
// Static singleton class containing APIs for interacting with Numbers.
// @treeLocation Client Reference/System
// @visibility external
//<
isc.defineClass("NumberUtil");

isc.NumberUtil.addClassProperties({
// decimal symbol used in Number.toString() - doesn't vary by locale (whereas toLocaleString() does)
_jsDecimalSymbol : ".",



//> @classAttr NumberUtil.decimalSymbol (String : "." : IR)
// The decimal symbol to use when formatting numbers
// @group i18nMessages
// @visibility external
//<
decimalSymbol : ".",

//> @classAttr NumberUtil.groupingSymbol (String : "," : IR)
// The grouping symbol, or thousands separator, to use when formatting numbers
// @group i18nMessages
// @visibility external
//<
groupingSymbol : ",",

//> @classAttr NumberUtil.negativeSymbol (String : "-" : IR)
// The negative symbol to use when formatting numbers
// @group i18nMessages
// @visibility external
//<
negativeSymbol : "-",

//> @classAttr NumberUtil.currencySymbol (String : "$" : IR)
// The currency symbol to use when formatting numbers
// @group i18nMessages
// @visibility external
//<
currencySymbol : "$",

//> @classAttr NumberUtil.negativeFormat (Number : 1 : IR)
// The format to use when formatting nagative numbers.  Supported values are: 1 = before, 
// 2 = after, 3 = beforeSpace, 4 = afterSpace, 5 = parens
// @group i18nMessages
// @visibility external
//<
negativeFormat : 1, 

//> @classAttr NumberUtil.groupingFormat (Number : 1 : IR)
// The grouping-format for numbers
// @group i18nMessages
// @visibility external
//<
groupingFormat : 1, // 0 = none; 1 = 123,456,789; 2 = 12,34,56,789

    
//> @classMethod NumberUtil.setStandardFormatter()
// Set the standard "toString()" formatter for Number objects.
// After this call, all <code>numberUtil.toString()</code>  calls will yield a number
// in this format.
//
// @param functionName (string) name of a formatting function on the number object prototype
// @group stringProcessing
//<
setStandardFormatter : function (functionName) {
	if (isc.isA.Function(isc.NumberUtil[functionName]))
        isc.NumberUtil.formatter = functionName;
},

//> @classMethod NumberUtil.setStandardLocaleStringFormatter()
// Set the standard locale formatter for all Number objects.
// After this call, all  <code>isc.iscToLocaleString(number)</code> for number instances 
// calls will yield the string returned by the formatter specified.
//
// @param functionName (string) name of a formatting function (on number instances)
// @group stringProcessing
//<
setStandardLocaleStringFormatter : function (functionName) {
	if (isc.isA.Function(isc.NumberUtil[functionName]))
        isc.NumberUtil.localeStringFormatter = functionName;
},

_1zero : "0",
_2zero : "00",
_3zero : "000",
_4zero : "0000",

_getZeroString : function (length) {
    if (length <= 0) return;

	var nu = isc.NumberUtil,
        pad
    ;
    // with > 4 zeros (very rare), build up a leading pad 4 0's at a time
    while (length > 4) {
        if (pad == null) pad = nu._4zero;
        else pad += nu._4zero;
        length -= 4;
    }

    var finalPad;
    switch (length) {
        case 4: finalPad = nu._4zero; break; 
        case 3: finalPad = nu._3zero; break; 
        case 2: finalPad = nu._2zero; break; 
        case 1: finalPad = nu._1zero; break; 
    } 

    // no leading pad (less than 4 zeros total)
    if (pad == null) return finalPad;
    return pad + finalPad;
},

// Remove any exponent from a the formatted number, adding zeros where 
// necessary while preserving the precision represented in the string.
_expandExponent : function (formattedNumber) {

    return formattedNumber.replace(/^([+-])?(\d+).?(\d*)[eE]([-+]?\d+)$/, 

        // Search for an exponential in the formatted number, matching four groups:
        //     sign, natural, fraction, coeffcient
        //                                   
        //     sign        = sign of the number                               
        //     natural     = integer part of significand (a natural number since no sign)
        //     fraction    = fractional part of significand
        //     coefficient = coefficient of number, including sign

        function(matchedString, sign, natural, fraction, coefficient){

            // We define the following variables
            //     lessThanOne           - whether number's absolute value is less than one
            //     normalizedCoefficient - coefficient normalized for the number of digits in
            //                             natural, integer part of the significand (off by one);
            //                             this abstractly represents the total number of digits
            //                             (including any added zeros) to the left of the
            //                             decimal point in the final formatted number 
            //     digitsToCross         - when moving the decimal point left or right from its
            //                             place in the significand to remove the exponential,
            //                             the number of digits from the signficand that will
            //                             be crossed (excluding zeros added by our own logic)

            var lessThanOne = +coefficient < 0, 
                normalizedCoefficient = natural.length + (+coefficient), 
                digitsToCross = (lessThanOne ? natural : fraction).length;

            // Now, build a string of zeros whose length is determined by the absolute value
            // of the coefficient, less the number of digits to cross; this is the number of 
            // zeros needed to separate the number from the decimal point.

            coefficient = Math.abs(coefficient);

            var nZeros = coefficient >= digitsToCross ? 
                         coefficient - digitsToCross + lessThanOne : 0,
                zeros = nZeros > 0 ? isc.NumberUtil._getZeroString(nZeros) : "";

            // Form the significand (joining both parts together), and attach zeros
            var significand = natural + fraction;
            if (lessThanOne) significand  = zeros + significand;
            else             significand += zeros;

            // If absolute value of number is less than one, offset the
            // normalized coefficient by the number of zeros.
            if (lessThanOne) normalizedCoefficient += zeros.length;

            // Output the digits to the left of the decimal point; we may be done
            var result = (sign || "") + significand.substr(0, normalizedCoefficient);

            // If not, add the remaining fractional digits to the right of the decimal
            if (normalizedCoefficient < significand.length) {
                result += "." + significand.substr(normalizedCoefficient);
            }
            return result;
        });
},

//> @classMethod NumberUtil.stringify()
// Return the passed number as a string padded out to digits length.
//
// @param number (number) Number object to stringify
// @param [digits] (number) Number of digits to pad to.  (Default is 2)
// @return (string) Padded string version of the number
//
// @example var str = isc.NumberUtil.stringify(myNumberVar, 2);
// @group stringProcessing
// @visibility external
//<

stringify : function (number, totalDigits, predecimal) {
    if (!isc.isA.Number(number)) return "";

    
    return isc.NumberUtil._stringify(totalDigits, predecimal, number);
},

_stringify : function (totalDigits, predecimal, number) {
    if (number == null) number = this;
    // default to 2 digits
    if (!totalDigits) totalDigits = 2;

    var numberString = number.toString(),
        zeroes = totalDigits - numberString.length
    ;

    // predecimal: ignore any decimal digits, such that two numbers with differing decimal
    // precision get the same total number of characters before the decimal.
    if (predecimal) {
        var dotIndex = numberString.indexOf(isc.dot);
        if (dotIndex != -1) {
            zeroes += (numberString.length - dotIndex);    
        }
    }
    var pad = isc.NumberUtil._getZeroString(zeroes);

    if (pad == null) return numberString;
    return pad + numberString;
},

//> @classMethod NumberUtil.toCurrencyString()
// Return the passed number as a currency-formatted string, or an empty string if not passed a 
// number.
//
// @param number (Number) the number to convert
// @param [currencyChar] (string) Currency symbol, default taken from the locale and can be 
//                                set to an empty string. If not passed and missing from the
//                                locale, defaults to <code>"$"</code>.
// @param [decimalChar] (string) Decimal separator symbol, default taken from the locale. If 
//                                if not passed and missing from the locale, defaults to 
//                                <code>"."</code>.
// @param [padDecimal] (boolean) Should decimal portion be padded out to two digits? True
//                               by default.
// @param [currencyCharLast] (boolean) Should the currency symbol be shown at the end of the 
//                                      string?  If unspecified, it will prefix the number.
// @return (string) Currency-formatted string version of the number
// @group stringProcessing
// @visibility external
//<
toCurrencyString : function (number, currencyChar, decimalChar, padDecimal, currencyCharLast) {
    if (!isc.isA.Number(number)) return "";

    
    return isc.NumberUtil._toCurrencyString(currencyChar, decimalChar, padDecimal, currencyCharLast, number)
},

_toCurrencyString : function (currencyChar, decimalChar, padDecimal, currencyCharLast, number) {
    if (number == null) number = this;

    var negative = number < 0,
        wholeNumber = number < 0 ? Math.ceil(number) : Math.floor(number),
        decimalNumber = Math.abs(Math.round((number - wholeNumber)*100)),
        output = isc.StringBuffer.create()
    ;

    wholeNumber = Math.abs(wholeNumber);

    // default currency/decimal symbols and decimal padding on
    // allow empty string for no currency character
    currencyChar = currencyChar || isc.NumberUtil.currencySymbol || "$";
    decimalChar = decimalChar || isc.NumberUtil.decimalSymbol || ".";
    if (padDecimal == null) padDecimal = true;

    // output sign
    
    if (negative) output.append(isc.NumberUtil.negativeSymbol || "-");

    // output currency symbol first by default
    if (currencyCharLast != true) output.append(currencyChar);

    // output whole number
    output.append(wholeNumber.stringify(1));

    // output decimal symbol and decimal number
    // (unless padding is off and decimal portion is 0)
    if (padDecimal) {
        output.append(decimalChar);
        output.append(decimalNumber.stringify(2));
    } else if (decimalNumber != 0) {
        output.append(decimalChar);
        if (decimalNumber % 10 == 0) output.append(decimalNumber/10);
        else output.append(decimalNumber.stringify(2));
    }

    // output currency symbol last if specified
    if (currencyCharLast == true) output.append(currencyChar);

    return output.toString();
},

//> @classMethod NumberUtil.toLocalizedString()
//  Format the passed number for readability, with:
//  <ul>
//      <li>separators between three-digit groups</li>
//      <li>optional fixed decimal precision (so decimal points align on right-aligned numbers)</li>
//      <li>localized decimal, grouping, and negative symbols</li>
//  </ul>
//  +link{NumberUtil.decimalSymbol, Decimal symbol}, 
//  +link{NumberUtil.groupingSymbol, grouping symbol}, and 
//  +link{NumberUtil.negativeSymbol, negative symbol} will normally come from
//  SmartClient locale settings (which may come from either client OS or application locale
//  settings), but they are also supported as arguments for mixed-format applications
//  (eg normalize all currency to +link{NumberUtil.toUSCurrencyString, US format}, but use the 
// current locale format for other numbers).
//
//  @param number (Number) the number object to convert
//  @param [decimalPrecision] (number) decimal-precision for the formatted value
//  @param [decimalSymbol] (string) the symbol that appears before the decimal part of the number
//  @param [groupingSymbol] (string) the symbol shown between groups of 3 non-decimal digits
//  @param [negativeSymbol] (string) the symbol that indicate a negative number
//  @return (string) formatted number or empty string if not passed a number.
//  @visibility external
//<

toLocalizedString : function (number, decimalPrecision, decimalSymbol, groupingSymbol, negativeSymbol) {
    if (!isc.isA.Number(number)) return "";

    var roundedValue = !decimalPrecision ? number : 
            Math.round(number * Math.pow(10, decimalPrecision)) / Math.pow(10, decimalPrecision);
    var absNum = Math.abs(roundedValue), // remove sign for now; deal with it at the very end of this method
        wholeNum = Math.floor(absNum), // whole part of the number (no decimal)
        wholeString, // string representation of whole part, before formatting
        decimalString, // string representation of decimal part, after formatting (padding)
        wholeChunks = []; // chunks of the whole number, based on 3-digit groupings
    
    // decimal part - doing this first because this code may round the whole part
    if (decimalPrecision) {
        // decimalPrecision specified and > 0, so
        // round/pad the decimal part to the specified number of digits
        var decimalNum = Math.round( (absNum-wholeNum) * Math.pow(10,decimalPrecision) );
        decimalString = isc.NumberUtil._stringify(decimalPrecision, null, decimalNum); // NOTE: stringify() could use a better name
    } else if (decimalPrecision == 0) {
        // decimalPrecision of 0 explicitly specified, so
        // round the whole number and drop the decimal part entirely
        wholeNum = Math.round(absNum);
    } else {
        // decimalPrecision not specified, so show the decimal part if there is one
        if (absNum-wholeNum > 0) {
            //  PRECISION ERROR - the next line of code introduces noise that makes a very long decimal part,
            //  e.g. 1.1 becomes 1.10000000000000009 - what causes this? some int to float conversion?
            //            decimalString = (absNum-wholeNum).toString().substring(2); // drops the leading "0."
            //  So using this alternate approach - just split the toString() on the decimal point
            //  and take the decimal part
            var absString = absNum.toString();
            decimalString = absString.substring(absString.indexOf(isc.NumberUtil._jsDecimalSymbol)+1);
        }
    }

    // whole part - slice it into chunks to be joined with grouping symbols
    wholeString = wholeNum.toString();
    var wholeLength = wholeString.length;
    var tripletCount = Math.floor(wholeLength/3); // number of complete chunks of 3 digits
    if (wholeLength%3) {
        // start with the incomplete chunk (first 1 or 2 digits) if any
        wholeChunks[0] = wholeString.substr(0, wholeLength%3);
    }
    for (var i=0; i<tripletCount; i++) {
        // then slice out each chunk of 3 digits
        wholeChunks[wholeChunks.length] = wholeString.substr(wholeLength%3 + i*3, 3);
    }
    
    // assembly - join the chunks of the whole part with grouping symbols, and glue together
    // the whole part, decimal symbol, decimal part, and negative sign as appropriate
    var outputString = wholeChunks.join(groupingSymbol || isc.NumberUtil.groupingSymbol);
    if (decimalString) outputString = outputString + (decimalSymbol || isc.NumberUtil.decimalSymbol) + decimalString;
    if (roundedValue < 0) outputString = (negativeSymbol || isc.NumberUtil.negativeSymbol) + outputString;
    return outputString;
},

// same as toLocalizedString but handles extra zeroes using decimalPrecision and decimalPad values 
floatValueToLocalizedString : function (number, decimalPrecision, decimalPad) {
    if (!isc.isA.Number(number)) return "";
    if (!decimalPad) decimalPad = 0;
    var res = isc.NumberUtil.toLocalizedString(number, decimalPrecision);
    var decIndx = res.indexOf(isc.NumberUtil.decimalSymbol);
    var zeroesToAdd = 0;
    if (decIndx < 0) {
        if (decimalPad == 0) return res;
        zeroesToAdd = decimalPad;
        // no decimalSymbol were found, so we adding one
        res += isc.NumberUtil.decimalSymbol;
    } else {
        zeroesToAdd = decimalPad - (res.length - decIndx - 1);    
    }
    if (zeroesToAdd > 0) {
        // add zeroes to the end according decimalPad value
        res += new Array(zeroesToAdd + 1).join('0');
    } else if (zeroesToAdd < 0) {
        // all extra zeroes should be removed
        for (var i = (res.length - 1); i>(decIndx + decimalPad); i--) {
            if (res[i] != '0' && res[i] != isc.NumberUtil.decimalSymbol) break;
        }
        // remove decimalSymbol if is the last one
        if (res[i] == isc.NumberUtil.decimalSymbol) i--;
        res = res.substr(0, i + 1);        
    }
    return res;
},

//> @classMethod NumberUtil.toUSString()
//  Format the passed number as a US string.  Returns empty string if not passed a number.
//
//  @param number (Number) the number object to format
//  @param [decimalPrecision] (number)
//  @return (string) formatted number or empty string if not passed a number
//  @visibility external
//<
toUSString : function(number, decimalPrecision) {
    if (!isc.isA.Number(number)) return "";
    return isc.NumberUtil.toLocalizedString(number, decimalPrecision, ".", ",", "-");
},

//> @classMethod NumberUtil.toUSCurrencyString()
//  Format the passed number as a US Dollar currency string. Returns empty string if not passed 
// a number.
//
//  @param number (Number) the number object to format
//  @param [decimalPrecision] (number)
//  @return (string) formatted number
//  @visibility external
//<
toUSCurrencyString : function(number, decimalPrecision) {
    if (!isc.isA.Number(number)) return "";
    var util = isc.NumberUtil;
    return "$" + util.toLocalizedString(number, decimalPrecision, ".", ",", "-");
},

//> @method NumberUtil.iscToLocaleString()
// Customizeable version of the <code>toLocaleString()</code> method for numbers.
// Called by <code>isc.iscToLocaleString()</code>.
// Uses the formatter set by NumberUtil.setStandardLocaleStringFormatter(), or at the instance 
// level by NumberUtil.setLocaleStringFormatter()
//
// @param number (Number) the number to format
// @return (string) formatted number as a string
//
// @group stringProcessing
//<
iscToLocaleString : function (number) {
    var f = isc.NumberUtil.localeStringFormatter;
    //var method = Number[f] || isc.NumberUtil[f];
    var method = isc.isA.Function(f) ? f : isc.NumberUtil[f];
    return method ? method(number) : number.toString();
},

//> @method NumberUtil.toFormattedString()
// Allow use of a custom number formatter - can be passed in as a parameter, or set by
// NumberUtil.setStandardFormatter()
//
// @param number (Number) the number to format
// @param [formatter] (string) name of a Number function to use
// @return (string) formatted number as a string
//
// @group stringProcessing
//<

toFormattedString : function (number, formatter) {
    var f = formatter || isc.NumberUtil.formatter;
    var method = isc.isA.Function(f) ? f : isc.NumberUtil[f];
    return method ? method(number) : number.toString();
},

toString : function (number) {
    if (number == null) return "";
    if (isc.isA.Class(number)) return number.valueOf().toString();
    return number.toString();
},

//> @classMethod NumberUtil.parseInt()
// Parse string that contains integer number. This method correctly handles locale based
// separators and currency symbol.
//
// @param string (string) the string to parse
// @return (Number) parsed number as a Number
// @visibility external
//
//<

parseInt : function (string) {
  string = string.replace(new RegExp("[" + this.groupingSymbol + "|"  + this.currencySymbol
      + "]", "g"),"");
  return parseInt(string);
},

//> @classMethod NumberUtil.parseFloat()
// Parse string that contains float number. This method correctly handles locale based
// separators, decimal points and currency symbol.
//
// @param string (string) the string to parse
// @return (float) parsed number as a Number
// @visibility external
//
//<
parseFloat : function (string) {
    string = string.replace(new RegExp("[" + this.groupingSymbol + "|"  + this.currencySymbol
        + "]", "g"), "");
    if (this.decimalSymbol != ".") {
        string = string.replace(new RegExp("[" + this.decimalSymbol + "]", "g"), ".");
    }
    return parseFloat(string);
},

parseLocaleFloat : function (string, decimalSymbol, groupingSymbol) {
    if (string == null) return Number.NaN;
    if (!decimalSymbol) decimalSymbol = isc.NumberUtil.decimalSymbol;
    if (!groupingSymbol) groupingSymbol = isc.NumberUtil.groupingSymbol;
    var numberString = "";
    var lastGroupingSymbolIndex = -1;
    var decimalSymbolFound = false;
    var isPositiveNumber = true;
    // user could use grouping symbol in number or not, if he used we should check that every
    // three symbols are followed by one grouping symbol
    var groupingSymbolUsed = string.contains(groupingSymbol);
    for (var i = 0; i < string.length; i++) {
        if (i == 0) {
            if (string[i] == "-") {
                isPositiveNumber = false;
                continue;
            } else if (string[i] == "+") {
                continue;
            }
        }
        // no grouping symbols should be found after decimal symbol found
        var mustBeGroupingSymbol = !decimalSymbolFound && groupingSymbolUsed;
        if (mustBeGroupingSymbol) {
            if (lastGroupingSymbolIndex != -1) {
                // grouping symbol should be after every three letters in the line
                mustBeGroupingSymbol = (i - lastGroupingSymbolIndex) == 4;
            } else {
                // first should be on third position or less
                mustBeGroupingSymbol = (i == 3) || (string[i] == groupingSymbol);
            }
        }
        if (string[i] == groupingSymbol) {
            if (!mustBeGroupingSymbol) {
                return Number.NaN;
            }
            lastGroupingSymbolIndex = i;
            continue;
        } else if (mustBeGroupingSymbol && string[i] != decimalSymbol) {
            return Number.NaN;
        } else if (string[i] == decimalSymbol) {
            if (decimalSymbolFound) return Number.NaN;
            if (groupingSymbolUsed && (i - lastGroupingSymbolIndex) != 4) return Number.NaN;
            decimalSymbolFound = true;
            numberString += ".";
            continue;
        }
        // Number should not contain any other non-digit symbols
        if (string[i] < "0" || string[i] > "9") return Number.NaN;
        numberString += string[i];
    }
    if (!decimalSymbolFound && groupingSymbolUsed && ((i - lastGroupingSymbolIndex) != 4)) {
        return Number.NaN;
    }
    return parseFloat(numberString);
},

parseLocaleInt : function (string, groupingSymbol) {
    if (string == null) return Number.NaN;
    if (!groupingSymbol) groupingSymbol = isc.NumberUtil.groupingSymbol;
    var numberString = "";
    var lastGroupingSymbolIndex = -1;
    var isPositiveNumber = true;
    var groupingSymbolUsed = string.contains(groupingSymbol);
    for (var i = 0; i < string.length; i++) {
        if (i == 0) {
            if (string[i] == "-") {
                isPositiveNumber = false;
                continue;
            } else if (string[i] == "+") {
                continue;
            }
        }
        // no grouping symbols should be found after decimal symbol found
        var mustBeGroupingSymbol = groupingSymbolUsed;
        if (mustBeGroupingSymbol) {
            if (lastGroupingSymbolIndex != -1) {
                // grouping symbol should be after every three letters in the line
                mustBeGroupingSymbol = (i - lastGroupingSymbolIndex) == 4;
            } else {
                // first should be on third position or less
                mustBeGroupingSymbol = (i == 3) || (string[i] == groupingSymbol);
            }
        }
        if (string[i] == groupingSymbol) {
            if (!mustBeGroupingSymbol) {
                return Number.NaN;
            }
            lastGroupingSymbolIndex = i;
            continue;
        } else if (mustBeGroupingSymbol) {
            return Number.NaN;
        }
        // Number should not contain any other non-digit symbols
        if (string[i] < "0" || string[i] > "9") return Number.NaN;
        numberString += string[i];
    }
    if (groupingSymbolUsed && ((i - lastGroupingSymbolIndex) != 4)) {
        return Number.NaN;
    }

    return parseInt(numberString);
},

parseLocaleCurrency : function (string, currencySymbol, decimalSymbol, groupingSymbol) {
    if (string == null) return Number.NaN;
    if (!currencySymbol) currencySymbol = isc.NumberUtil.currencySymbol;
    // correct input could be CHF1.227,33 and 1.227,33CHF (symbol could contain several letters)
    if (string.startsWith(currencySymbol)) {
        string = string.substring(currencySymbol.length);
    } else if (string.endsWith(currencySymbol)) {
        string = string.substring(0, string.length - currencySymbol.length);
    }
    return this.parseLocaleFloat(string);
},

//> @classMethod NumberUtil.parseIfNumeric()
//
// If given a numeric string (that is, a non-empty string which converts to a
// number), will return the equivalent integer. Otherwise, returns the
// parameter unchanged. Useful for dealing with values that can be numbers or
// strings, but which you want to coerce to a numeric type if possible.
//
// @param numberOrString (any) the string or number to parse
// @return (any) an integer, if possible, otherwise the input unchanged 
// @visibility external
//<
// Used for dealing with heights and widths. They can be numbers or strings
// (e.g. "50%" or "*"), and thus are deserialized as strings. But we
// sometimes want to know whether it's "really" a string, or instead a
// "numeric string" like "100". 
parseIfNumeric : function (numberOrString) {
    if (isc.isA.Number(numberOrString)) {
        return numberOrString;
    } else if (isc.isA.nonemptyString(numberOrString)) {
        // Note that we want to return strings with trailing characters (like
        // "100%") unchanged, even though parseInt would produce an integer
        // from them. To check for that, isNaN is probably faster than a
        // regexp.
        if (isNaN(numberOrString)) {
            return numberOrString;
        } else {
            return parseInt(numberOrString, 10);
        }
    } else {
        // If it's neither Number nor String, or an empty String, just return
        // it. An empty string could be parsed to 0, but that's not necessarily
        // what was meant.
        return numberOrString;
    }
},

//>	@classMethod	NumberUtil.format()
// Return the parameter number formatted according to the parameter +link{type:FormatString}.
// This method is used to implement the +link{DataSourceField.format} functionality, but it can
// also be used to format arbitrary numbers programmatically.
// @param  number  (Integer | Float | Double | int | float | double) The number to format
// @param  format  (FormatString) The format to apply
// @return (String) formatted number string
// @visibility external
//<
format : function(number, pformat) {

    if (!isc.isA.Number(number)) {
        this.logWarn("Cannot format '" + number + "' - not a Number");
        return number;
    }

    if (!isc.isA.String(pformat)) {
        this.logWarn("Cannot use format '" + pformat + "' - not a String");
        return number;
    }
    
    if (pformat == "") {
        return number.toString();
    }

    var format = pformat + "",
        n = number + 0,
        neg = n < 0,
        abs = Math.abs(number),
        negFormat,
        parts = abs.toString().split('.'),
        intPart = parts[0],
        decPart = parts[1];

    if (neg) {
        if (format.indexOf(";") != -1) {
            format = negFormat = format.substring(format.indexOf(";")+1);
        } else { 
            format = format.indexOf(";") == -1 ? format : format.substring(0, format.indexOf(";"));
        }
    } else {
        format = format.indexOf(";") == -1 ? format : format.substring(0, format.indexOf(";"));
    }
    
    var quote = format.indexOf("'"),
        literals = [],
        positions = [];
    while (quote != -1) {
        var start = quote,
            end = format.indexOf("'", start+1);
        if (end == -1) {
            var error = "Invalid format string \"" + pformat + "\" - contains " +
                            "mismatched quotes"
            this.logWarn(error);
            return error;
        }
        var literal = format.substring(start+1, end);
        if (literal === "") literal = "'";
        literals.push(literal);
        positions.push(start);
        format = format.substring(0, start) + format.substring(end+1);
        quote = format.indexOf("'");
    }

    var grouping = format.indexOf(",");
    if (grouping > -1) {
        format = format.replace(/,/g, '');
    }
    
    var zeroesStart = format.indexOf("0"),
        poundsStart = format.indexOf("#"),
        decimalPos = format.indexOf("."),
        numberStarts = Math.min(zeroesStart == -1 ? 999 : zeroesStart, 
                                poundsStart == -1 ? 999 : poundsStart, 
                                decimalPos == -1 ? 999 : decimalPos),
        numberEnds = format.length-1,
        percent = false,
        permil = false,
        zeroes = 0;

    for (var i = format.length-1; i > numberStarts; i--) {
        var ch = format.charAt(i);
        if (ch == '0' || ch == '#' || ch == '.') break;
        if (ch == '%') percent = true;
        if (ch == '\u2030') permil = true;
        numberEnds--;
    }
    
    if (numberStarts == 999 || numberEnds < 0 || numberStarts > numberEnds) {
        // This matches the (somewhat arbitrary-seeming) Java DecimalFormat behavior 
        parts = Math.abs(n).toString().split('.');
        intPart = parts[0];
        decPart = "";
        var prefix = format;
        if (positions) {
            for (var i = positions.length-1; i >= 0; i--) {
                prefix = prefix.substring(0, positions[i]) + literals[i] + prefix.substring(positions[i]);
            }
        }
        if (neg && !negFormat) prefix = "-" + prefix;
        var suffix = "",
            dPoint = "";
    } else {
        
        if (zeroesStart != -1 && poundsStart != -1 && decimalPos != -1) {
            if (zeroesStart < poundsStart && poundsStart < decimalPos) {
                var error = "Invalid format string \"" + pformat + "\" - cannot specify '0' to the " +
                                "left of '#' in the integer part"
                this.logWarn(error);
                return error;
            }
        }
        
        var curr = format.indexOf("\u00a4");
        while (curr != -1) {
            format = format.substring(0, curr) + isc.NumberUtil.currencySymbol + format.substring(curr+1);
            curr = format.indexOf("\u00a4");
        }
        
        var prefix = format.substring(0, numberStarts);
        var suffix = format.substring(numberEnds+1);
        for (var i = positions.length-1; i >= 0; i--) {
            if (positions[i] > numberStarts && positions[i] < numberEnds) {
                this.logWarn("Format string \"" + pformat + "\" contains quoted characters within " +
                                "the actual number area - these will be ignored");
            } else if (positions[i] <= numberStarts) {
                prefix = prefix.substring(0, positions[i]) + literals[i] + prefix.substring(positions[i]);
            } else {
                positions[i] -= (numberEnds+1);
                suffix = suffix.substring(0, positions[i]) + literals[i] + suffix.substring(positions[i]);
            }
        }
        
        if (zeroesStart != -1 && (decimalPos == -1 || zeroesStart < decimalPos)) {
            zeroes = (decimalPos == -1 ? numberEnds+1 : decimalPos) - zeroesStart;
        }
        
        if (percent) n = n * 100;
        else if (permil) n = n * 1000;
        
        var precision = "";
        if (decimalPos != -1) precision = format.substring(decimalPos+1, numberEnds+1);

        n = this._roundDecimalForFormatting(n, precision.length, decPart ? decPart.length : 0);
        parts = Math.abs(n).toString().split('.');
        intPart = parts[0];
        decPart = parts[1] || "";
        if (decPart.length < precision.length) {
            var c = decPart.length;
            while (precision[c] === "0" && c++ < precision.length) decPart += "0";
        }
        
        if (intPart == "0") {
            intPart = "0000000000000000000000000000000000000000".substring(0, zeroes);
        } else if (zeroes > intPart.length) {
            intPart = "0000000000000000000000000000000000000000".substring(intPart.length, zeroes) + intPart;
        }
        
        if (grouping > -1) {
            intPart = intPart.replace(/(\d)(?=(\d{3})+(?!\d))/g, '$1' + isc.NumberUtil.groupingSymbol);
        }
        
        if (neg && !negFormat) {
            // No explicit format provided for negative numbers, so just prepend a minus sign to
            // the positive-formatted number
            prefix = prefix ? "-" + prefix : "-";
        }
        
        var dPoint = decPart && decPart.length > 0 ? isc.NumberUtil.decimalSymbol : "";
    }

    return prefix + intPart + dPoint + decPart + suffix;
},

useAccurateRounding: false,

_roundDecimalForFormatting : function(number, targetPrecision, numberPrecision) {
    // NOTE: native toFixed() rounds unpredictably on exact 0.5, 0.05, etc, boundaries, because
    // of inaccuracies introduced by the floating-point format.  So we don't use it...
    if (numberPrecision <= targetPrecision) return number;
    
    // Use the absolute value of negative numbers, to force it to round away from zero like
    // both DecimalFormat and Excel do
    var neg = number < 0;
    if (neg) number = 0 - number;
    
        
    var ori = number;
    var m = Math.pow(10, targetPrecision);
    number = number * m;
    if (isc.NumberUtil.useAccurateRounding) {
        var e = Math.pow(10, numberPrecision * -1);
        number += e;
    }
    number = Math.round(number)
    number = number / m;
    
    
    var otherNum = (Math.round(number * m) / m).toFixed(targetPrecision);
    if (number != otherNum) {
        alert("Round " + ori + " to " + targetPrecision + "dp.  My way gives " + number + ", numeral.js gives " + otherNum);
    }

    
    return neg ? 0 - number : number;
}

});

// set the standard formatter for the date prototype to the native browser string
// so 'toFormattedString()' defaults to returning the standard number format string
if (!isc.NumberUtil.formatter) isc.NumberUtil.formatter = "toString";


if (!isc.NumberUtil.localeStringFormatter) 
    isc.NumberUtil.localeStringFormatter = "toString";

