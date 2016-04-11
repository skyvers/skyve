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
//> @type RelativeDateShortcut
// A RelativeDateShortcut is a special string that represents a shortcut to a date phrase that can 
// be automatically mapped to a +link{type:RelativeDateString} for use in widgets that 
// leverage relative-dates, such as the +link{class:RelativeDateItem}.
// <P>
// Note that some shortcuts indicate a time period but do not directly indicate whether the value
// refers to the start or end of the time period in question. This ambiguity
// can be resolved by specifying an explicit +link{RelativeDateRangePosition} when calling APIs that 
// convert from RelativeDates to absolute date values. This is the case for <i>$today</i>, 
// <i>$tomorrow</i>, <i>$yesterday</i>, <i>$weekAgo</i>, <i>$weekFromNow</i>, <i>$monthAgo</i>
// and <i>$monthFromNow</i>. If a range position is not explicitly passed, these will all default
// to the start of the day in question.
// <P>
// Builtin options include
// <ul>
// <li> $now - this moment </li>
// <li> $today - the current day. By default this resolves to the start of the current day though
//   an explicit +link{RelativeDateRangePosition} may be used to specify the end of the current day.</li>
// <li> $startOfToday - the start of today</li>
// <li> $endOfToday - the end of today (one millisecond before the $startOfTomorrow) </li>
// <li> $yesterday - the previous day.</li>
// <li> $startOfYesterday - the start of yesterday</li>
// <li> $endOfYesterday - the end of yesterday (one millisecond before the $startOfToday) </li>
// <li> $tomorrow - the following day</li>
// <li> $startOfTomorrow - the start of tomorrow </li>
// <li> $endOfTomorrow - the end of tomorrow </li>
// <li> $weekAgo - the current day of the previous week </li>
// <li> $weekFromNow - the current day of the next week </li>
// <li> $startOfWeek - the start of the current week </li>
// <li> $endOfWeek - the end of the current week </li>
// <li> $monthAgo - the current day of the previous month </li>
// <li> $monthFromNow - the current day of the following month </li>
// <li> $startOfMonth - the start of the current month </li>
// <li> $endOfMonth - the end of the current month </li>
// <li> $startOfYear - the start of the current year </li>
// <li> $endOfYear - the end of the current year </li>
// </ul>
// 
// <P>
// 
// @see RelativeDateString
// @visibility external
//<

//> @type RelativeDateString
// A string of known format used to specify a datetime offset.  For example, a 
// RelativeDateString that represents "one year from today" is written as <code>"+1y"</code>.
// <P>
// RelativeDateStrings are comprised of the following parts:
// <ul>
// <li>direction: the direction in which the quantity applies - one of + or - </li>
// <li>quantity: the number of units of time to apply - a number </li>
// <li>timeUnit: an abbreviated timeUnit to use - one of ms/MS (millisecond), s/S (second), 
//      mn/MN (minute), h/H (hour), d/D (day), w/W (week), m/M (month), q/Q (quarter, 3-months), 
//      y/Y (year), dc/DC (decade) or c/C (century). <br>
//      The timeUnit is case sensitive. A lowercase timeUnit implies an exact offset, so <code>+1d</code>
//      refers to the current date / time increased by exactly 24 hours. If the timeUnit is 
//      uppercase, it refers to the start or end boundary of the period of time in question, so
//      <code>+1D</code> would refer to the end of the day (23:39:59:999) tomorrow, and
//      <code>-1D</code> would refer to the start of the day (00:00:00:000) yesterday.</li>
// <li>[qualifier]: an optional timeUnit encapsulated in square-brackets and used to offset 
//      the calculation - eg. if +1d is "plus one day", +1d[W] is "plus one day from the 
//      end of the current week".  You may also specify another complete RelativeDateString as the
//      [qualifier], which offers more control - eg, +1d[+1W] indicates "plus one day from 
//      the end of NEXT week".</li>
// </ul>
// <P>
// This format is very flexible. Here are a few example relative date strings:<br>
// <code>+0D</code>: End of today. There are often multiple ways to represent the same time
//  using this system - for example this could also be written as <code>-1ms[+1D]</code><br>
// <code>-0D</code>: Beginning of today.<br>
// <code>+1W</code>: End of next week.<br>
// <code>+1w[-0W]</code>: Beginning of next week.<br>
// <code>+1w[-0D]</code>: Beginning of the current day of next week.
//
// @see RelativeDateShortcut
// @visibility external
//<

//> @object RelativeDate
// An object representing a relative date, useful for representing date ranges etc in criteria.
// RelativeDate objects may be created directly by SmartClient components such as the
// +link{RelativeDateItem}.
// <P>
// RelativeDate objects will have <code>"_constructor"</code> set to <code>"RelativeDate"</code>
// and must have a specified +link{RelativeDate.value}. Any other attributes are optional.
//
// @treeLocation Client Reference/System
// @visibility external
//<
// This type of object is returned by RelativeDateItem.getValue() and is understood directly by
// DataSources when assembling criteria.


//> @attr relativeDate.value (RelativeDateString or RelativeDateShortcut : null : IR)
// The value of this relative date, specified as a +link{RelativeDateString} 
// or +link{RelativeDateShortcut}.
// @visibility external
//<

//> @type RelativeDateRangePosition
// When  relative dates are specified in a date range, typically in a RelativeDateItem or
// DateRangeItem, in order to make the range inclusive or exclusive, it is useful to be able 
// to specify whether we're referring to the start or end of the date in question.
//
// @value "start" Indicates this relative date should be treated as the start of the specified
//    logical date.
// @value "end" Indicates this relative date should be treated as the end of the specified logical
//    date.
// @visibility external
//<

//> @attr relativeDate.rangePosition (RelativeDateRangePosition : null : IR)
// If this relative date has its value specified as a +link{RelativeDateShortcut} which doesn't
// specify an exact time-period boundary - for example <code>"$yesterday"</code>, this attribute
// may be set to specify whether the date should be interpreted as the start or end boundary of
// the time period.
// @visibility external
//<

// Add static methods to the DateUtil class (defined in Date.js)
isc.DateUtil.addClassMethods({
    
    //> @classMethod DateUtil.mapRelativeDateShortcut() [A]
    // Converts a +link{RelativeDateShortcut} to a +link{RelativeDateString}.
    // @param relativeDate (RelativeDateShortcut) shortcut string to convert
    // @param [rangePosition] (RelativeDateRangePosition) Are we interested in the start or end of the
    //  specified relative date? This applies to shortcuts which do not specify a specific
    //  moment (such as <code>$today</code>) - it does not apply to shortcuts which 
    //  already specify a specific moment such as <code>$startOfToday</code>. If unspecified 
    //  rangePosition is always assumed to be "start"
    // @return (RelativeDateString) converted relative date string.
    // @visibility external
    //<
    mapRelativeDateShortcut : function (relativeDate, rangePosition) {
        switch (relativeDate) {
            case "$now": return "+0MS";

            case "$today":
                if (rangePosition == "end") {
                    return "+0D";
                } else {
                    return "-0D";
                }
            case "$startOfToday": 
                return "-0D";
            case "$endOfToday": return "+0D";

            case "$yesterday": 
                if (rangePosition == "end") {
                    
                    return "-1d[+0D]";
                } else {
                    return "-1D";
                }
            case "$startOfYesterday": 
                return "-1D";
            case "$endOfYesterday": return "-1d[+0D]";

            case "$tomorrow": 
                if (rangePosition == "end") {
                    return "+1D";
                } else {
                    return "+1d[-0D]";
                }
            case "$startOfTomorrow": 
                return "+1d[-0D]";
            case "$endOfTomorrow": return "+1D";

            case "$startOfWeek": return "-0W";
            case "$endOfWeek": return "+0W";

            case "$startOfMonth": return "-0M";
            case "$endOfMonth": return "+0M";

            case "$startOfYear": return "-0Y";
            case "$endOfYear": return "+0Y";
            
            case "$weekFromNow" :
                if (rangePosition == "end") {
                    return "+1w[+0D]";
                } else {
                    return "+1w[-0D]";
                }
                
            case "$weekAgo" :
                if (rangePosition == "end") {
                    return "-1w[+0D]";
                } else {
                    return "-1w[-0D]";
                }
            
            case "$monthFromNow" :
                if (rangePosition == "end") {
                    return "+1m[+0D]";
                } else {
                    return "+1m[-0D]";
                }
                
            case "$monthAgo" :
                if (rangePosition == "end") {
                    return "-1m[+0D]";
                } else {
                    return "-1m[-0D]";
                }
        }
        return relativeDate;
    },

    //> @classMethod DateUtil.getAbsoluteDate() 
    //  Converts a +link{RelativeDate}, +link{type:RelativeDateShortcut} or +link{RelativeDateString} 
    // to a concrete Date.
    // @param relativeDate (RelativeDate or RelativeDateShortcut or RelativeDateString) the relative
    //   date to convert
    // @param [baseDate] (Date) base value for conversion.  Defaults to the current date/time.
    // @param [rangePosition] (RelativeDateRangePosition) optional date-range position. Only has an effect
    //   if the date passed in is a +link{type:RelativeDateShortcut} where the range position 
    //   is not implicit, such as "$yesterday"
    // @param [isLogicalDate] (boolean) should the generated date be marked as a "logical" date? A
    //   logical date object is a Date value where the time component is ignored for formatting and
    //   serialization purposes - such as the date displayed within a component field of
    //   specified type "date". See +link{group:dateFormatAndStorage} for more on logical dates vs
    //   datetime type values.
    // @return (Date) resulting absolute date value
    // @visibility external
    //<
    getAbsoluteDate : function (relativeDate, baseDate, rangePosition, isLogicalDate) {
        // passed a date object, just return a duplicate of it
        if (isc.isA.Date(relativeDate)) return relativeDate.duplicate();
        
        var _this = isc.DateUtil;
        var value = relativeDate;

        if (_this.isRelativeDate(value)) {
            // the caller passed an actual RelativeDate object - get the relativeDateString and
            // potentially the rangePosition from the object
            if (!rangePosition) rangePosition = value.rangePosition;
            value = relativeDate.value;
        }
    
        // if the param isn't now a string, it's not a relativeDate - return null
        if (!isc.isA.String(value)) return null;
    
        // convert relativeDateShortcut to relativeDateString, if necessary.
        // This will resolve the 'rangePosition'
        if (value.startsWith("$")) {
            var mappedString = _this.mapRelativeDateShortcut(value, rangePosition);
            // if the mapped string is unchanged, it's not a supported shortcut
            if (mappedString == value) return null;
            value = mappedString;
        }

        var localBaseDate = isLogicalDate ? Date.createLogicalDate() : new Date();
        
        if (baseDate != null) localBaseDate.setTime(baseDate.getTime());
        var parts = _this.parseRelativeDateString(value, true);

        // if the string couldn't be parsed, return null
        if (!parts) return null;

        if (parts.qualifier) {
            // Qualifier is always going to be in "boundary" type increments -- support it being
            // specified as upper or lowercase.
            // get rid of the brackets and upper-case it because we're
            // just going to run the baseDate through addDate(), which already understands
            // about capitals
            parts.qualifier = parts.qualifier.toUpperCase();
            
            var qParts = _this.parseRelativeDateString(parts.qualifier);

            if (qParts) {
                if (isc.DateUtil._relativePeriods.contains(qParts.period)) {
                    localBaseDate = _this.dateAdd(localBaseDate, 
                        qParts.period, qParts.countValue, (qParts.direction == "+" ? 1 : -1),
                        isLogicalDate);
                } else {
                    // invalid qualifier - log a warning and skip
                    isc.logWarn("Invalid date-offset qualifier provided: "+qParts.period+".  Valid "+
                        "options are: S, MN (or N), H, D, W, M, Q and Y (or YY, YYYY).");
                }
            }
        }

        // perform the date calculation
        var absoluteDate = _this.dateAdd(localBaseDate, parts.period, 
                                        parts.countValue, (parts.direction == "+" ? 1 : -1),
                                        isLogicalDate);

        if (isLogicalDate) absoluteDate.isLogicalDate = true;

        return absoluteDate;
    },

    _relativePeriods: ["MS", "S", "MN", "N", "H", "D", "W", "M", "Q", "Y", "YY", "YYYY", "DC", "C"],
    isRelativeDate : function (value, includeStrings) {
        // return true if the parameter is a RelativeDate object (or string representation)
        if (!isc.isA.Date(value) && isc.isAn.Object(value) && value._constructor == "RelativeDate") return true;
        var _this = isc.DateUtil;
        if (includeStrings && isc.isA.String(value)) {
            return _this.isRelativeDateShortcut(value) || _this.isRelativeDateString(value, true);
        }
        return false;
    },
    isRelativeDateShortcut : function (value) {
        // return true if the parameter is a string representing a RelativeDateShortcut and the
        // result of mapping it is different than the value itself - that means it's supported
        
        return isc.isA.String(value) && value.startsWith("$") && 
                 isc.DateUtil.mapRelativeDateShortcut(value) != value;
    },

    isRelativeDateString : function (value) {
        // return true if the parameter is a string that can be parsed as a relative date
        return isc.DateUtil.parseRelativeDateString(value, true) != null;
    },
    
    mapsToDate : function (value) {
        // returns true if the param is a Date, a RelativeDate object or a parse-able
        // relativeDateShortcut/String
        if (isc.isA.Date(value) || isc.DateUtil.isRelativeDate(value, true)) return true;
    },

    parseRelativeDateString : function (relativeDateString, suppressDefaults) {
        // if it's not a string, or shorter than 3 chars, it's not a relativeDateString
        if (!isc.isA.String(relativeDateString) || relativeDateString.length < 3) return null;

        var result = {};
        
        // string is in the format +1D[-0D]
        
        var parts = relativeDateString.split("[");
        if (parts[1]) {
            // qualifier is the bit in the square brackets (another relative date string)
            result.qualifier = parts[1].replace("]", "");
        }

        var value = parts[0];
        result.direction = value[0];
        result.count = "";
        result.period = "";
        
        for (var i=1; i<value.length; i++) {
            if (!isNaN(parseInt(value[i]))) result.count += value[i];
            else result.period += value[i];
        }
        result.count = parseInt(result.count);
        
        if (suppressDefaults) {
            // the value must start with a + or - character, there must be a valid count 
            // and a period that exists in the _relativePeriods array (eg, D or M)
            if (!["+", "-"].contains(result.direction)) return null;
            if (isNaN(result.count)) return null;
            if (result.period.length == 0) return null;
            if (!isc.DateUtil._relativePeriods.contains(result.period.toUpperCase())) return null;
        }

        return { 
            direction: (result.direction == "+" || result.direction == "-" ? result.direction : "+"), 
            qualifier: result.qualifier,
            countValue: isc.isA.Number(result.count) ? result.count : 0, 
            period: result.period ? result.period : "D"
        };
    },

    // helper method for adding positive and negative amounts of any time-unit from 
    // milliseconds to centuries to a given date
    // date: base date to modify
    // unit: one of "ms" / "MS", "H" / "h", "D" / "d" etc - if unset, default is "d" (day)
    // amount: the number of "unit"s to offset by
    // multiplier: +1 or -1 - direction in which we're shifting the date
    // Returns the modified date.
    dateAdd : function (date, unit, amount, multiplier, isLogicalDate) {
        
        // boundary: If the specified time-unit is upperCase, we want to calculate the
        // date offset to the end of the time-unit in question. For example:
        // +1d ==> offset to the same time on the next day
        // +1D ==> offset to the end of the next day
        // -1D ==> offset to the beginning of the previous day.
        var boundary = false;

        // set some defaults for missing params - if code calls dateAdd(date), with no other 
        // params, the defaults will add 1 day to the passed date
        if (unit == null) unit = "d";
        if (amount == null) amount = 1;
        if (multiplier == null) multiplier = 1;
        if (isLogicalDate == null) isLogicalDate = date.logicalDate;

        // just in case we were passed a timeUnitName ("minute", rather than "mn", eg)
        if (unit.length > 2) unit = isc.DateUtil.getTimeUnitKey(unit);

        switch (unit) {
            case "MS":
                // no need to set boundary for ms - we don't have a finer gradation than this.
            case "ms":
                date.setMilliseconds(date.getMilliseconds()+(amount*multiplier));
                break;
                
            case "S":
                boundary = true;
            case "s":
                date.setSeconds(date.getSeconds()+(amount*multiplier));
                break;
                
            case "MN":
            case "N":
                boundary = true;
            case "mn":
            case "n":
                date.setMinutes(date.getMinutes()+(amount*multiplier));
                break;
                
            case "H":
                boundary = true;
            case "h":
                date.setHours(date.getHours()+(amount*multiplier));
                break;

            case "D":
                boundary = true;
            case "d":
                if (amount*multiplier != 0) date.setDate(date.getDate()+(amount*multiplier));
                break;
                
            case "W":
                boundary = true;
            case "w":
                date.setDate(date.getDate()+((amount*7)*multiplier));
                break;
                
            case "M":
                boundary = true;
            case "m":
                var tempDate = isc.Date.createLogicalDate(date.getFullYear(), date.getMonth(), 1);

                tempDate.setMonth(tempDate.getMonth()+(amount*multiplier));
                tempDate = isc.DateUtil.getEndOf(tempDate, unit, true);
                
                if (tempDate.getDate() < date.getDate()) date.setDate(tempDate.getDate());
                date.setMonth(tempDate.getMonth());
                date.setFullYear(tempDate.getFullYear());
                break;
            
            case "Q":
                boundary = true;
            case "q":
                date.setMonth(date.getMonth()+((amount*3)*multiplier));
                break;
            
            case "Y":
            case "YY":
            case "YYYY":
                boundary = true;
            case "y":
            case "yy":
            case "yyyy":
                date.setFullYear(date.getFullYear()+(amount*multiplier));
                break;
            
            case "DC":
                boundary = true;
            case "dc":
                date.setFullYear(date.getFullYear()+((amount*10)*multiplier));
                break;

            case "C":
                boundary = true;
            case "c":
                date.setFullYear(date.getFullYear()+((amount*100)*multiplier));
                break;
            default:
        }
        
        if (boundary) {
            if (multiplier > 0) {
                date = this.getEndOf(date, unit, isLogicalDate);
            } else {
                date = this.getStartOf(date, unit, isLogicalDate);
            }
        }
        return date;
    },

    // getStartOf / getEndOf - methods to round a date to start or end of a period (week, day, etc)
    
    
    _datetimeOnlyPeriods:{
        ms:true, MS:true,
        s:true, S:true,
        mn:true, MN:true,
        n:true, N:true,
        h:true, H:true,
        d:true, D:true
    },
    
    //> @classMethod DateUtil.getStartOf() [A]
    // Returns the start of some period, like day, week or month, relative to a passed Date 
    // instance.
    // @param date (Date) the base date to find the period start from
    // @param period (String) the period to return the start of, one of mn/h/d/w/m/y
    // @param [logicalDate] (Boolean) process and return a logicalDate with no time element
    // @param [firstDayOfWeek] (Integer) which day should be considered the firstDayOfWeek - 
    //                overrides the default provided by the locale
    // @return (Date) a Date instance representing the start of the period relative to the
    //                passed date
    // @visibility external
    //<
    getStartOf : function (date, period, logicalDate, firstDayOfWeek) {
        var year, month, dateVal, hours, minutes, seconds, dayOfWeek;
        if (logicalDate == null) logicalDate = date.logicalDate;
        
        if (firstDayOfWeek == null && isc.DateChooser) 
            firstDayOfWeek = isc.DateChooser.getInstanceProperty("firstDayOfWeek");

        // If we're passed a period <= "day", and we're working in logical dates, just return
        // the date - there's no way to round the time within a "logical date"
        if (logicalDate && this._datetimeOnlyPeriods[period] == true) {
            this.logInfo("DateUtil.getStartOf() passed period:" 
                + period + " for logical date. Ignoring");
            var newDate = new Date(date.getTime());
            newDate.logicalDate = true;
            return newDate;
        }

        if (!isc.Time._customTimezone || logicalDate) {
            month = date.getMonth();
            dateVal = date.getDate();
            year = date.getFullYear();
            hours = date.getHours();
            minutes = date.getMinutes();
            seconds = date.getSeconds();
            
            dayOfWeek = date.getDay();
            
        // Developer specified custom timezone
        } else {
            // Use the "offsetDate" trick we use for formatting datetimes - easier to shift the
            // date and call native date APIs than to actually modify potentially 
            // minute, hour, date, month, year directly.
            var offsetDate = date._getTimezoneOffsetDate(
                                isc.Time.getUTCHoursDisplayOffset(date), 
                                isc.Time.getUTCMinutesDisplayOffset(date)
                             );
    
            month = offsetDate.getUTCMonth();
            dateVal = offsetDate.getUTCDate();
            year = offsetDate.getUTCFullYear();
            
            hours = offsetDate.getUTCHours();
            minutes = offsetDate.getUTCMinutes();
            seconds = offsetDate.getUTCSeconds();
            
            dayOfWeek = offsetDate.getDay();
        }

        switch (period.toLowerCase()) {
            case "s":
                // start of second - bit dramatic, but may as well be there
                return Date.createDatetime(year, month, dateVal, hours, minutes, seconds, 0);
            case "mn":
            case "n":
                // start of minute
                return Date.createDatetime(year, month, dateVal, hours, minutes, 0, 0);

            case "h":
                // start of hour
                return Date.createDatetime(year, month, dateVal, hours, 0, 0, 0);

            case "d":
                // start of day
                if (dateVal != date.getDate()) dateVal = date.getDate();
                if (month != date.getMonth()) month = date.getMonth();
                if (year != date.getFullYear()) year = date.getFullYear();
                if (logicalDate) {
                    return Date.createLogicalDate(year, month, dateVal);
                } else {
                    return Date.createDatetime(year, month, dateVal, 0, 0, 0, 0);
                }

            case "w":
                // start of week
                var newDate;
                if (logicalDate) {
                    newDate = Date.createLogicalDate(year, month, dateVal);
                } else {
                    newDate = Date.createDatetime(year, month, dateVal, 0, 0, 0, 0);
                }
                var delta = dayOfWeek - firstDayOfWeek;
                if (delta < 0) delta += 7;
                else if (delta > 0) delta += firstDayOfWeek;
                newDate.setDate(newDate.getDate()-delta);
                return newDate;
                
            case "m":
                // start of month
                if (logicalDate) {
                    return Date.createLogicalDate(year, month, 1);
                } else {
                    return Date.createDatetime(year, month, 1, 0, 0, 0, 0);
                }
            case "q":
                // start of quarter
                var quarterStart = month - (month % 3);
                if (logicalDate) {
                    return Date.createLogicalDate(year, quarterStart, 1);
                } else {
                    return Date.createDatetime(year, quarterStart, 1, 0, 0, 0, 0);
                }
            case "y":
            case "yy":
            case "yyyy":
                // start of year
                if (logicalDate) {
                    return Date.createLogicalDate(year, 0, 1);
                } else {
                    return Date.createDatetime(year, 0, 1, 0, 0, 0, 0);
                }

            case "dc":
                // start of decade
                var decade = year - (year % 10);
                if (logicalDate) {
                    return Date.createLogicalDate(decade, 0, 1);
                } else {
                    return Date.createDatetime(decade, 0, 1, 0, 0 ,0, 0);
                }

            case "c":
                // start of century
                var century = year - (year % 100);
                if (logicalDate) {
                    return Date.createLogicalDate(century, 0, 1);
                } else {
                    return Date.createDatetime(century, 0, 1, 0, 0, 0, 0);
                }
        }

        return date.duplicate();
    },

    //> @classMethod DateUtil.getEndOf() [A]
    // Returns the end of some period, like day, week or month, relative to a passed Date 
    // instance.
    // @param date (Date) the base date to find the period end from
    // @param period (String) the period to return the end of, one of mn/h/d/w/m/y
    // @param [logicalDate] (Boolean) process and return a logicalDate with no time element
    // @param [firstDayOfWeek] (Integer) which day should be considered the firstDayOfWeek - 
    //                overrides the default provided by the locale
    // @return (Date) a Date instance representing the end of the period relative to the
    //                passed date
    // @visibility external
    //<
    getEndOf : function (date, period, logicalDate, firstDayOfWeek) {
        
        var year, month, dateVal, hours, minutes, seconds, dayOfWeek;
        if (logicalDate == null) logicalDate = date.logicalDate;

        if (firstDayOfWeek == null && isc.DateChooser) 
            firstDayOfWeek = isc.DateChooser.getInstanceProperty("firstDayOfWeek");

        // If we're passed a period <= "day", and we're working in logical dates, just return
        // the date - there's no way to round the time within a "logical date"
        if (logicalDate && this._datetimeOnlyPeriods[period] == true) {
            this.logInfo("DateUtil.getEndOf() passed period:" 
                + period + " for logical date. Ignoring");
            var newDate = new Date(date.getTime());
            newDate.logicalDate = true;
            return newDate;
        }

        if (!isc.Time._customTimezone || logicalDate) {
            month = date.getMonth();
            dateVal = date.getDate();
            year = date.getFullYear();
            hours = date.getHours();
            minutes = date.getMinutes();
            seconds = date.getSeconds();
            
            dayOfWeek = date.getDay();
            
        // Developer specified custom timezone
        } else {
            // Use the "offsetDate" trick we use for formatting datetimes - easier to shift the
            // date and call native date APIs than to actually modify potentially 
            // minute, hour, date, month, year directly.
            var offsetDate = date._getTimezoneOffsetDate(
                                isc.Time.getUTCHoursDisplayOffset(date), 
                                isc.Time.getUTCMinutesDisplayOffset(date)
                             );
    
            month = offsetDate.getUTCMonth();
            dateVal = offsetDate.getUTCDate();
            year = offsetDate.getUTCFullYear();
            
            hours = offsetDate.getUTCHours();
            minutes = offsetDate.getUTCMinutes();
            seconds = offsetDate.getUTCSeconds();
            
            dayOfWeek = offsetDate.getDay();
        }

        switch (period.toLowerCase()) {
            case "s":
                // end of second 
                return Date.createDatetime(year, month, dateVal, hours, minutes, seconds, 999);
            case "mn":
            case "n":
                // end of minute
                return Date.createDatetime(year, month, dateVal, hours, minutes, 59, 999);

            case "h":
                // end of hour
                return Date.createDatetime(year, month, dateVal, hours, 59, 59, 999);

            case "d":
                // end of day
                if (dateVal != date.getDate()) dateVal = date.getDate();
                if (month != date.getMonth()) month = date.getMonth();
                if (year != date.getFullYear()) year = date.getFullYear();
                if (logicalDate) {
                    return Date.createLogicalDate(year, month, dateVal);
                } else {
                    return Date.createDatetime(year, month, dateVal, 23, 59, 59, 999);
                }

            case "w":
                // end of week
                var delta = (6-(dayOfWeek-firstDayOfWeek));
                if (delta >= 7) delta -= 7;
                var endDate = dateVal + delta;
                if (logicalDate) {
                    return Date.createLogicalDate(year, month, endDate);
                } else {
                    return Date.createDatetime(year, month, endDate, 23, 59, 59, 999);
                }
                
            case "m":
                // end of month
                // Get start of *next* month, then knock back to prev day.
                var newDate;
                if (logicalDate) {
                    newDate = Date.createLogicalDate(year, month+1, 1);
                    newDate.setDate(newDate.getDate() - 1);
                } else {
                    newDate = Date.createDatetime(year, month+1, 1, 0, 0, 0, 0);
                    newDate.setTime(newDate.getTime()-1);
                }
                return newDate;
                
            case "q":
                // end of quarter
                var nextQ = month + 3 - (month%3),
                    newDate;
                if (logicalDate) {
                    newDate = Date.createLogicalDate(year, nextQ, 1);
                    newDate.setDate(newDate.getDate()-1);
                } else {
                    newDate = Date.createDatetime(year, nextQ, 1, 0, 0, 0, 0);
                    newDate.setTime(newDate.getTime()-1);
                }
                return newDate;
                
            case "y":
            case "yy":
            case "yyyy":
                // end of year
                if (logicalDate) {
                    return Date.createLogicalDate(year, 11, 31);
                } else {
                    return Date.createDatetime(year, 11, 31, 23, 59, 59, 999);
                }

            case "dc":
                // end of decade
                var decade = year + 10 - (year % 10);
                if (logicalDate) {
                    return Date.createLogicalDate(decade, 11, 31);
                } else {
                    return Date.createDatetime(decade, 11, 31, 23, 59, 59, 999);
                }

            case "c":
                // start of century
                var century = year +100 - (year % 100);
                if (logicalDate) {
                    return Date.createLogicalDate(century, 11, 31);
                } else {
                    return Date.createDatetime(century,  11, 31, 23, 59, 59, 999);
                }
        }
        return date.duplicate();
    },
    
    // mappings between "TimeUnit" strings and the equivalent period markers used in 
    // RelativeDateStrings and Calendars/Timelines
    _timeUnitMapping:{
        ms:"millisecond",
        s:"second",
        // support mn and n for minutes (Excel uses n)
        mn:"minute",
        n:"minute",
        h:"hour",
        d:"day",
        w:"week",
        m:"month",
        q:"quarter",
        // support y, yy and yyyy for year (Excel uses yyyy)
        y:"year",
        yy:"year",
        yyyy:"year",
        dc:"decade",
        c:"century"
    },
    getTimeUnitName : function (timeUnitKey) {
        var value = timeUnitKey.toLowerCase();
        return this._timeUnitMapping[value] || value;
    },
    getTimeUnitKey : function (timeUnitName) {
        if (this._timeUnitReverseMapping == null) {
            this._timeUnitReverseMapping = isc.makeReverseMap(this._timeUnitMapping);
        }
        var value = timeUnitName.toLowerCase();
        return this._timeUnitReverseMapping[value] || value;
    },
    compareTimeUnits : function (unitName, otherUnitName) {
        var unitMS = this.getTimeUnitMilliseconds(unitName),
            otherUnitMS = this.getTimeUnitMilliseconds(otherUnitName)
        ;
        if (unitMS <= otherUnitMS) return -1;
        if (unitMS == otherUnitMS) return 0;
        return 1;
    },
    getTimeUnitMilliseconds : function (timeUnitName) {
        var name = this.getTimeUnitName(timeUnitName),
            l = { millisecond: 1, second: 1000 }
        ;

        l.minute = l.second * 60;
        l.hour = l.minute * 60;
        l.day = l.hour * 24;
        l.week = l.day * 7;
        l.month = l.day * 30; // this is accurate enough for the purposes of this method
        l.quarter = l.month * 3;
        l.year = l.day * 365;
        l.decade = l.year * 10;
        l.century = l.decade * 10;

        return l[name];
    },
    
    convertPeriodUnit : function (period, fromUnit, toUnit) {
        if (fromUnit == toUnit) return period;
        var fromKey = this.getTimeUnitKey(fromUnit),
            toKey = this.getTimeUnitKey(toUnit),
            millis = (fromKey == "ms" ? period : period * this.getTimeUnitMilliseconds(fromKey)),
            result = millis / this.getTimeUnitMilliseconds(toKey)
        ;
        return result;
    },

    getTimeUnitTitle : function (unit) {
        return this.getTimeUnitName(unit);
    },
    
    getPeriodLength : function (startDate, endDate, unit, roundUnit) {
        var periodLength = (endDate.getTime() - startDate.getTime());
        if (unit) periodLength = isc.DateUtil.convertPeriodUnit(periodLength, "ms", unit);
        return periodLength;
    }
});
