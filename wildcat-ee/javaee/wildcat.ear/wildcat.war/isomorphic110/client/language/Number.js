/*
 * Isomorphic SmartClient
 * Version v11.0p_2016-03-31 (2016-03-31)
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

}); // end addProperties(Number.prototype) for localizable number formatter



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
});

// Instance of this class can hold numeric value of any size and precision.
isc.defineClass("BigDecimal");

isc.BigDecimal.addProperties({
    // RegExp to parse number. exec() result array values:
    // 0 - parsed value
    // 1 - sign
    // 2 - whole number (if entered witout fraction)
    // 3 - whole number (if entered with fraction)
    // 4 - fraction part (if entered with fraction)
    // 5 - exponent sing
    // 6 - exponent value
    // 7 - Infinity
    r : /^(?:(?:NaN|([+|-]?)(?:(?:(\d+)\.*|(\d*)\.(\d+))(?:[E|e]([+|-]?)(\d+))?|(Infinity))))$/,
    // Holds true if value is NaN
    // new BigDecimal starts as NaN
    nanValue: true,
    // Holds true if value is positive or negative Infinity
    infinityValue: false,
    // 1: "+"
    //-1: "-"
    sign: 1,
    // Holds significant digits of number:
    // for value 12.345: num="12345";
    // for value 607000: num="607";
    // for value 0.00809: num="809";
    num: "",
    // Holds exponent
    // realValue=num*10^exp
    exp: 0
});

isc.BigDecimal.addMethods({
    // Returns true if instance holds value which does not represent valid number.
    isNaN : function() {
        return this.nanValue;
    },
    // Returns true if instance holds Infinity (positive or negative).
    isInfinity : function() {
        return !this.nanValue && this.infinityValue;
    },
    // Returns 1 if holds positive value or -1 if holds negative value.
    getSign : function() {
        return (this.nanValue)?1:this.sign;
    },
    // Returns significant digits of number.
    getNum : function() {
        return (this.nanValue)?"0":this.num;
    },
    // Returns exponent
    getExp : function() {
        return (this.nanValue)?0:this.exp;
    },
    // Normalizes state of number: trims leading/trailing zeroes and adjusts exponent accordingly:
    // 00012300e2 becomes 123e4
    // Value is not changed - method changes only internal representation of the seam value.
    normalize : function() {
        if (this.nanValue) {
            // Reset all internals for NaN
            this.infinityValue = false;
            this.sign = 1;
            this.num = "";
            this.exp = 0;
        } else {
            if (this.infinityValue) {
                // Infinity does not have number representation - reset it
                this.num = "";
                this.exp = 0;
            } else {
                // Trim leading zeroes
                this.num = this.num.replace(/^0*/, "");
                // Trim trailing zeroes
                var trail = /0*$/.exec(this.num);
                if (trail) {
                    this.num = this.num.replace(/0*$/, "");
                    // Adjust exponent
                    this.exp += trail[0].length;
                }
                if (this.num === "") {
                	this.num = "0";
                	this.exp = 0;
                }
            }
        }
        return this;
    },
    // Returns value as a string.
    // If exponent parameter is true - return value in exponent representation
    getStringValue : function(exponent) {
        if (this.nanValue) {
            return "NaN";
        }
        if (this.infinityValue) {
            return (this.sign === 1?"":"-") + "Infinity";
        }
        if (exponent) {
            if (this.num.length === 1) {
                return (this.sign === 1?"":"-") + this.num + "e" + this.exp;
            }
            var res = this.num.substr(0, 1) + "." + this.num.substr(1);
            return (this.sign === 1?"":"-") + res + "e" + (this.exp + (this.num.length - 1));
        } else {
            var res = this.num;
            if (this.exp >= 0) {
                res += "0".repeat(this.exp);
            } else {
                if (res.length < Math.abs(this.exp) + 1) {
                    res = "0".repeat(Math.abs(this.exp) - res.length + 1) + res;
                }
                res = res.substr(0, res.length + this.exp) + "." + res.substr(res.length + this.exp);
            }
            return (this.sign === 1?"":"-") + res;
        }
    },
    // Returns value as Number
    // Loss of precision can occur.
    getNumberValue : function() {
        if (this.nanValue) {
            return NaN;
        } else if (this.infinityValue) {
            return Infinity * this.sign;
        } else {
            return new Number(this.getStringValue());
        }
    },
    // If THIS number is greater than parameter - return 1;
    // If THIS number equals to parameter - return 0;
    // If THIS number is less than parameter - return -1;
    // Special cases:
    //      if parameter can not be converted to number it is treated as zero;
    //      NaN treated as zero
    compareTo : function(number) {
        if (!(isc.isA.BigDecimal(number))) {
            number = isc.BigDecimal.create(number);
        }
        var thisNanValue = this.isNaN();
        var thisInfinityValue = this.isInfinity();
        var thisSign = this.getSign();
        var thisNum = this.getNum();
        var thisExp = this.getExp();
        var otherNanValue = number.isNaN();
        var otherInfinityValue = number.isInfinity();
        var otherSign = number.getSign();
        var otherNum = number.getNum();
        var otherExp = number.getExp();
        if (thisInfinityValue) {
            if (otherInfinityValue) {
                if (thisSign > otherSign) {
                    return 1;
                } else if (thisSign === otherSign) {
                    return 0;
                } else {
                    return -1;
                }
            } else {
                if (thisSign > 0) {
                    return 1;
                } else {
                    return -1;
                }
            }
        }
        if (otherInfinityValue) {
            if (otherSign < 0) {
                return 1;
            } else {
                return -1;
            }
        }
        if (thisExp > otherExp) {
            thisNum += "0".repeat(thisExp - otherExp);
        } else {
            otherNum += "0".repeat(otherExp - thisExp);
        }
        if (thisNum.length > otherNum.length) {
            otherNum = "0".repeat(thisNum.length - otherNum.length) + otherNum;
        } else {
            thisNum = "0".repeat(otherNum.length - thisNum.length) + thisNum;
        }
        if (thisNum > otherNum) {
            if (thisSign > otherSign) {
                return 1;
            } else if (thisSign === otherSign) {
                return 1;
            } else {
                return -1;
            }
        } else if (thisNum === otherNum) {
            if (thisSign > otherSign) {
                return 1;
            } else if (thisSign === otherSign) {
                return 0;
            } else {
                return -1;
            }
        } else {
            if (thisSign > otherSign) {
                return 1;
            } else if (thisSign === otherSign) {
                return -1;
            } else {
                return -1;
            }
        }
    },
    // Negates number
    // Returns new instance
    negate : function() {
        var ret = isc.BigDecimal.create(this);
        if (!ret.isNaN()) {
            if (ret.sign === 1) {
                ret.sign = -1;
            } else {
                ret.sign = 1;
            }
        }
        return ret;
    },
    // Add specified number to THIS number
    // Returns new instance
    add : function(number) {
        if (!(isc.isA.BigDecimal(number))) {
            number = isc.BigDecimal.create(number);
        }
        var thisNanValue = this.isNaN();
        var thisInfinityValue = this.isInfinity();
        var thisSign = this.getSign();
        var thisNum = this.getNum();
        var thisExp = this.getExp();
        var otherNanValue = number.isNaN();
        var otherInfinityValue = number.isInfinity();
        var otherSign = number.getSign();
        var otherNum = number.getNum();
        var otherExp = number.getExp();
        // If both values are NaN - return NaN.
        if (thisNanValue && otherNanValue) {
            return isc.BigDecimal.create();
        }
        // +Infinity-Infinity or -Infinity+Infinity results in NaN.
        if (thisInfinityValue && otherInfinityValue && thisSign != otherSign) {
            return isc.BigDecimal.create();
        }
        var ret = isc.BigDecimal.create("0");
        // If this value is (+/-)Infinity - adding anything to (+/-)Infinity equals (+/-)Infinity
        if (thisInfinityValue) {
            ret.sign = this.sign;
            ret.infinityValue = true;
            return ret.normalize();
        }
        // If other value is (+/-)Infinity - adding (+/-)Infinity to anything equals (+/-)Infinity
        if (otherInfinityValue) {
            ret.sign = number.sign;
            ret.infinityValue = true;
            return ret.normalize();
        }
        if (thisExp > otherExp) {
            thisNum += "0".repeat(thisExp - otherExp);
            thisExp = otherExp;
        } else {
            otherNum += "0".repeat(otherExp - thisExp);
            otherExp = thisExp;
        }
        if (thisNum.length > otherNum.length) {
            otherNum = "0".repeat(thisNum.length - otherNum.length) + otherNum;
        } else {
            thisNum = "0".repeat(otherNum.length - thisNum.length) + thisNum;
        }
        ret.sign = thisSign;
        ret.exp = thisExp;
        if (thisSign === otherSign) {
            var res = "";
            var carry = 0;
            for (var i = thisNum.length - 1; i >= 0; i--) {
                var s = parseInt(thisNum[i]) + parseInt(otherNum[i]) + carry;
                res = "" + (s % 10) + res;
                carry = Math.floor(s / 10);
            }
            if (carry > 0) {
                res = "" + carry + res;
            }
            ret.num = res;
        } else {
            if (thisNum < otherNum) {
                var tmp = thisNum;
                thisNum = otherNum;
                otherNum = tmp;
                ret.sign = otherSign;
            }
            var res = "";
            var carry = 0;
            for (var i = thisNum.length - 1; i >= 0; i--) {
                var s = 10 + parseInt(thisNum[i]) - parseInt(otherNum[i]) - carry;
                res = "" + (s % 10) + res;
                carry = (Math.floor(s / 10) >= 1)?0:1;
            }
            ret.num = res;
        }
        return ret.normalize();
    },
    // Subtracts specified number from THIS number.
    // Returns new instance
    subtract : function(number) {
        number = isc.BigDecimal.create(number);
        number = number.negate();
        return this.add(number);
    },
    multiply : function(number) {
        if (!(isc.isA.BigDecimal(number))) {
            number = isc.BigDecimal.create(number);
        }
        // If any is NaN - return NaN
        if (this.isNaN() || number.isNaN()) {
            return isc.BigDecimal.create();
        }
        // Multiplying any Infinity by 0 gives NaN
        if ((this.compareTo(0) === 0 && number.isInfinity())
            || (this.isInfinity() && number.compareTo(0) === 0)) {
            return isc.BigDecimal.create();
        }
        // Multiplying any Infinity by any number gives Infinity
        if (this.isInfinity() || number.isInfinity()) {
            var ret = isc.BigDecimal.create("Infinity");
            if (this.sign !== number.sign) {
                ret.sign = -1;
            }
            return ret;
        }
        // Multiplying any number by 0 gives 0
        if (this.compareTo(0) === 0 || number.compareTo(0) === 0) {
            return isc.BigDecimal.create(0);
        }
        // Inflate num values (from both sides) that they would represent
        // numbers of same magnitude.
        var tNum = this.num;
        var oNum = number.num;
        tNum += "0".repeat(Math.max(this.exp - number.exp, 0));
        oNum += "0".repeat(Math.max(number.exp - this.exp, 0));
        tNum = "0".repeat(Math.max(oNum.length - tNum.length, 0)) + tNum;
        oNum = "0".repeat(Math.max(tNum.length - oNum.length, 0)) + oNum;
        // Multiply 7-digit chunks: result would be maximum 14 digits long -
        // JS can handle this.
        var a = [];
        while (tNum.length > 0) {
            var tm = parseInt(tNum.substring(Math.max(tNum.length - 7, 0)));
            var tmpONum = oNum;
            var r1 = [];
            var r2 = [];
            while (tmpONum.length > 0) {
                var om = parseInt(tmpONum.substring(Math.max(tmpONum.length - 7, 0)));
                var r = tm * om;
                r1.push(r % 10000000);
                r1.push(0);
                r2.push(0);
                r2.push(Math.floor(r / 10000000));
                tmpONum = tmpONum.substring(0, Math.max(tmpONum.length - 7, 0));
            }
            a.push(r1);
            a.push(r2);
            tNum = tNum.substring(0, Math.max(tNum.length - 7, 0));
        }
        // If resulting array has more than 10 rows we have to use BigDecimal
        // to sum. Summing more than 10 we can get result exceeding
        // 15 digits thus loose precision.
        var useBD = true;
        if (a.length < 10) {
            useBD = false;
        }
        var y = 0;
        var x = 0;
        // Carry
        var c = (useBD)?isc.BigDecimal.create("0"):0;
        var resA = [];
        while (y < a.length) {
            var s = (useBD)?isc.BigDecimal.create("0"):0;
            var xx = x;
            var yy = y;
            while ((a[yy] !== undefined) && (a[yy][xx] !== undefined)) {
                if (useBD) {
                    s = s.add(a[yy++][xx--]);
                } else {
                    s += a[yy++][xx--];
                }
            }
            // Add previous carry and save chunk 7 digits long.
            // Higher digits saved to next carry.
            if (useBD) {
                s = s.add(c);
                var sNum = "00000000" + s.num + "0".repeat(s.exp);
                c = isc.BigDecimal.create(sNum.substring(0, sNum.length - 7));
                s = isc.BigDecimal.create(sNum.substring(sNum.length - 7));
            } else {
                s += c;
                c = Math.floor(s / 10000000);
                s = s % 10000000;
            }
            resA.push(s);
            if (x < a[0].length - 2) {
                x = x + 2;
            } else {
                if (x < a[0].length - 1) {
                    y++
                } else {
                    y = y + 2;
                }
                x = a[0].length - 1;
            }
        }
        resA.push(c);
        var resNum = "";
        for (var i = resA.length - 1; i >= 0; i--) {
            var sNum;
            if (useBD) {
                sNum = "00000000" + resA[i].num + "0".repeat(resA[i].exp);
            } else {
                sNum = "00000000" + resA[i]
            }
            // Each chunk is 7 digits long
            resNum += sNum.substring(sNum.length - 7);
        }
        var rSign = "";
        if (this.sign !== number.sign) {
            rSign = "-";
        }
        // Resulting exponent
        var rExp = Math.min(this.exp, number.exp) * 2;
        return isc.BigDecimal.create(rSign + resNum + "e" + rExp);
    },
    round : function(precision, mode) {
        // NaN, Infinity, Zero - there is nothing to round
        if (this.isNaN() || this.isInfinity() || this.num === "0") {
            return isc.BigDecimal.create(this);
        }
        if (!precision) {
            precision = 0;
        }
        // Number is already at required precision
        if ((-1 * precision) <= this.exp) {
            return isc.BigDecimal.create(this);
        }
        var leftPart;
        var rightPart;
        if ((-1 * precision) >= (this.exp + this.num.length)) {
            leftPart = "0";
            rightPart = "0".repeat((-1 * precision) - (this.exp + this.num.length)) + this.num;
        } else {
            leftPart = this.num.substring(0, this.exp + this.num.length + precision);
            rightPart = this.num.substring(this.exp + this.num.length + precision);
        }
        if (!mode) {
            mode = "round";
        }
        var left = isc.BigDecimal.create(leftPart);
        var right = isc.BigDecimal.create("0." + rightPart);
        if (mode === "round") {
            var c = right.compareTo("0.5");
            if (this.sign >= 0) {
                if (c >= 0) {
                    left = left.add(1);
                }
            } else {
                if (c > 0) {
                    left = left.add(1);
                }
                left = left.negate();
            }
            left.exp += (-1 * precision);
            return left.normalize();
        } else if (mode === "ceil" || mode == "java_ceil") {
            if (this.sign >= 0) {
                left = left.add(1);
            } else {
                left = left.negate();
            }
            left.exp += (-1 * precision);
            return left.normalize();
        } else if (mode === "floor" || mode == "java_floor") {
            if (this.sign < 0) {
                left = left.add(1).negate();
            }
            left.exp += (-1 * precision);
            return left.normalize();
        } else if (mode == "java_up") {
            left = left.add(1);
            if (this.sign < 0) {
                left = left.negate();
            }
            left.exp += (-1 * precision);
            return left.normalize();
        } else if (mode == "java_down") {
            if (this.sign < 0) {
                left = left.negate();
            }
            left.exp += (-1 * precision);
            return left.normalize();
        } else if (mode == "java_halfUp") {
            var c = right.compareTo("0.5");
            if (c >= 0) {
                left = left.add(1);
            }
            if (this.sign < 0) {
                left = left.negate();
            }
            left.exp += (-1 * precision);
            return left.normalize();
        } else if (mode == "java_halfDown") {
            var c = right.compareTo("0.5");
            if (c > 0) {
                left = left.add(1);
            }
            if (this.sign < 0) {
                left = left.negate();
            }
            left.exp += (-1 * precision);
            return left.normalize();
        } else if (mode == "java_halfEven") {
            var c = right.compareTo("0.5");
            if (c > 0) {
                left = left.add(1);
            } else if (c === 0) {
                var lastDigit = leftPart.substring(leftPart.length - 1);
                if (lastDigit === "1" || lastDigit === "3" || lastDigit === "5" || lastDigit === "7" || lastDigit === "9") {
                    left = left.add(1);
                }
            }
            if (this.sign < 0) {
                left = left.negate();
            }
            left.exp += (-1 * precision);
            return left.normalize();
        }
        return isc.BigDecimal.create(this);
    },
    ceil : function(precision) {
        return this.round(precision, "ceil");
    },
    floor : function(precision) {
        return this.round(precision, "floor");
    },
    // Parses provided parameter
    init : function () {
        this.Super("init", arguments);
        if (arguments && arguments[0]) {
            var value = arguments[0];
            if (isc.isA.Number(value)) {
                value = value.toExponential(20);
            }
            if (isc.isA.String(value)) {
                var parts = this.r.exec(value);
                if (parts) {
                    if (parts[0] !== "NaN") {
                        this.nanValue = false;
                        if (parts[1] !== undefined) {
                            this.sign = (parts[1] === "-"?-1:1);
                        }
                        if (parts[7] !== undefined) {
                            this.infinityValue = true;
                        } else {
                            if (parts[6] !== undefined) {
                                this.exp = new Number(parts[6]);
                                if (parts[5] === '-') {
                                    this.exp *= -1;
                                }
                            }
                            if (parts[2] !== undefined) {
                                this.num = parts[2];
                                this.num = this.num.replace(/^0*/, "");
                            } else {
                                this.num = parts[3];
                                this.num = this.num.replace(/^0*/, "");
                                if (parts[4] !== undefined) {
                                    parts[4] = parts[4].replace(/0*$/, "");
                                    this.exp -= parts[4].length;
                                    this.num += parts[4];
                                    this.num = this.num.replace(/^0*/, "");
                                }
                            }
                            this.normalize();
                        }
                    }
                }
            } else if (value === Infinity) {
                this.nanValue = false;
                this.sign = 1;
                this.infinityValue = true;
            } else if (value === -Infinity) {
                this.nanValue = false;
                this.sign = -1;
                this.infinityValue = true;
            } else if (isc.isA.BigDecimal(value)) {
                this.nanValue = value.isNaN();
                this.infinityValue = value.isInfinity();
                this.sign = value.getSign();
                this.num = value.getNum();
                this.exp = value.getExp();
                this.normalize();
            }
        }
    }
});
