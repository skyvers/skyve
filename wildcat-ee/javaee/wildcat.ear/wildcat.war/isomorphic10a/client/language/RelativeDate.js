/*
 * Isomorphic SmartClient
 * Version v10.0p_2015-01-04 (2015-01-04)
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
        if (this.isRelativeDate(relativeDate)) {
            // the caller passed an actual RelativeDate object - get the relativeDateString and
            // potentially the rangePosition from the object
            if (!rangePosition) rangePosition = relativeDate.rangePosition;
            relativeDate = relativeDate.value;
        }
    
        // convert relativeDate to relativeDateString, if necessary.
        // This will resolve the 'rangePosition'
        if (relativeDate.startsWith("$")) {
            relativeDate = this.mapRelativeDateShortcut(relativeDate, rangePosition);
        }
        var value = relativeDate,
            localBaseDate = isLogicalDate ? Date.createLogicalDate() : new Date()
        ;
        
        if (baseDate != null) localBaseDate.setTime(baseDate.getTime());
        var parts = this.getRelativeDateParts(value);

        if (parts.qualifier) {
            // Qualifier is always going to be in "boundary" type increments -- support it being
            // specified as upper or lowercase.
            // get rid of the brackets and upper-case it because we're
            // just going to run the baseDate through addDate(), which already understands
            // about capitals
            parts.qualifier = parts.qualifier.toUpperCase();
            
            var qParts = this.getRelativeDateParts(parts.qualifier);

            var options = ["S", "MN", "H", "D", "W", "M", "Q", "Y"];
            if (options.contains(qParts.period)) {
                localBaseDate = this.dateAdd(localBaseDate, 
                    qParts.period, qParts.countValue, (qParts.direction == "+" ? 1 : -1),
                    isLogicalDate);
            } else {
                // invalid qualifier - log a warning and skip
                isc.logWarn("Invalid date-offset qualifier provided: "+qParts.period+".  Valid "+
                    "options are: S, MN, H, D, W, M, Q and Y.");
            }
        }

        // perform the date calculation
        var absoluteDate = this.dateAdd(localBaseDate, parts.period, 
                                        parts.countValue, (parts.direction == "+" ? 1 : -1),
                                        isLogicalDate);

        if (isLogicalDate) absoluteDate.isLogicalDate = true;

        return absoluteDate;
    },
    
    isRelativeDate : function (value) {
        if (isc.isA.Date(value)) return false;
        if (isc.isAn.Object(value) && value._constructor == "RelativeDate") return true;

        return false;
    },

    getRelativeDateParts : function (relativeDateString) {
        var value = relativeDateString,
            direction = value.substring(0,1),
            bracketIndex = value.indexOf("["),
            qualifier = (bracketIndex > 0 ? value.substring(bracketIndex) : null),
            withoutQualifier = (qualifier != null ? value.substring(1, bracketIndex) : value.substring(1)),
            countValue = parseInt(withoutQualifier),
            period = withoutQualifier.replace(countValue, "")
        ;

        return { 
            direction: (direction == "+" || direction == "-" ? direction : "+"), 
            qualifier: qualifier ? qualifier.replace("[", "").replace("]", "").replace(",", "") : null, 
            countValue: isc.isA.Number(countValue) ? countValue : 0, 
            period: period ? period : direction 
        };
    },

    // helper method for adding positive and negative amounts of any time-unit from 
    // milliseconds to centuries to a given date
    // date: base date to modify
    // unit: one of "ms" / "MS", "H" / "h", "D" / "d" etc - if unset, default is "d" (day)
    // amount: the number of "unit"s to offset by
    // multiplier: +1 - add (amount* or -1 - direction in which we're shifting the date
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
                boundary = true;
            case "mn":
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
                date.setDate(date.getDate()+(amount*multiplier));
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
                boundary = true;
            case "y":
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
        s:true, S:true,
        mn:true, MN:true,
        h:true, H:true,
        d:true, D:true
    },
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

        switch (period) {
            case "s":
            case "S":
                // start of second - bit dramatic, but may as well be there
                return Date.createDatetime(year, month, dateVal, hours, minutes, seconds, 0);
            case "mn":
            case "MN":
                // start of minute
                return Date.createDatetime(year, month, dateVal, hours, minutes, 0, 0);

            case "h":
            case "H":
                // start of hour
                return Date.createDatetime(year, month, dateVal, hours, 0, 0, 0);

            case "d":
            case "D":
                // start of day
                if (logicalDate) {
                    return Date.createLogicalDate(year, month, dateVal);
                } else {
                    return Date.createDatetime(year, month, dateVal, 0, 0, 0, 0);
                }

            case "w":
            case "W":
                // start of week
                var newDate;
                if (logicalDate) {
                    newDate = Date.createLogicalDate(year, month, dateVal);
                } else {
                    newDate = Date.createDatetime(year, month, dateVal, 0, 0, 0, 0);
                }
                var delta = firstDayOfWeek - dayOfWeek;
                newDate.setTime(newDate.getTime()+(delta*(24*60*60*1000)));
                return newDate;
            case "m":
            case "M":
                // start of month
                if (logicalDate) {
                    return Date.createLogicalDate(year, month, 1);
                } else {
                    return Date.createDatetime(year, month, 1, 0, 0, 0, 0);
                }
            case "q":
            case "Q":
                // start of quarter
                var quarterStart = month - (month % 3);
                if (logicalDate) {
                    return Date.createLogicalDate(year, quarterStart, 1);
                } else {
                    return Date.createDatetime(year, quarterStart, 1, 0, 0, 0, 0);
                }
            case "y":
            case "Y":
                // start of year
                if (logicalDate) {
                    return Date.createLogicalDate(year, 0, 1);
                } else {
                    return Date.createDatetime(year, 0, 1, 0, 0, 0, 0);
                }

            case "dc":
            case "DC":
                // start of decade
                var decade = year - (year % 10);
                if (logicalDate) {
                    return Date.createLogicalDate(decade, 0, 1);
                } else {
                    return Date.createDatetime(decade, 0, 1, 0, 0 ,0, 0);
                }

            case "c":
            case "C":
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

        switch (period) {
            case "s":
            case "S":
                // end of second 
                return Date.createDatetime(year, month, dateVal, hours, minutes, seconds, 999);
            case "mn":
            case "MN":
                // end of minute
                return Date.createDatetime(year, month, dateVal, hours, minutes, 59, 999);

            case "h":
            case "H":
                // end of hour
                return Date.createDatetime(year, month, dateVal, hours, 59, 59, 999);

            case "d":
            case "D":
                // end of day
                if (logicalDate) {
                    return Date.createLogicalDate(year, month, dateVal);
                } else {
                    return Date.createDatetime(year, month, dateVal, 23, 59, 59, 999);
                }

            case "w":
            case "W":
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
            case "M":
                // end of month
                
                // Get start of *next* month, then knock back to prev day.
                var newDate;
                if (logicalDate) {
                    newDate = Date.createLogicalDate(year, month+1, 1);
                    newDate.setTime(newDate.getTime() - (24*60*60*1000));
                } else {
                    newDate = Date.createDatetime(year, month+1, 1, 0, 0, 0, 0);
                    newDate.setTime(newDate.getTime()-1);
                }
                return newDate;
                
            case "q":
            case "Q":
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
            case "Y":
                // end of year
                if (logicalDate) {
                    return Date.createLogicalDate(year, 11, 31);
                } else {
                    return Date.createDatetime(year, 11, 31, 23, 59, 59, 999);
                }

            case "dc":
            case "DC":
                // end of decade
                var decade = year + 10 - (year % 10);
                if (logicalDate) {
                    return Date.createLogicalDate(decade, 11, 31);
                } else {
                    return Date.createDatetime(decade, 11, 31, 23, 59, 59, 999);
                }

            case "c":
            case "C":
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
        mn:"minute",
        h:"hour",
        d:"day",
        w:"week",
        m:"month",
        q:"quarter",
        y:"year",
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
