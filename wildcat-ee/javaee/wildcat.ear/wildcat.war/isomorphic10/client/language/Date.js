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







//> @class DateUtil
// Static singleton class containing APIs for interacting with Dates.
// @treeLocation Client Reference/System
// @visibility external
//<

isc.defineClass("DateUtil");

isc.DateUtil.addClassMethods({

    _min : function (date1, date2) {
        
        return (date1.getTime() < date2.getTime() ? date1 : date2);
    },
    
    _max : function (date1, date2) {
        
        return (date1.getTime() < date2.getTime() ? date2 : date1);
    },

    //>	@classMethod		DateUtil.format()
    // Return the parameter date formatted according to the parameter +link{type:FormatString}.  
    // This method is used to implement the +link{DataSourceField.format,DataSourceField.format}
    // functionality, but it can also be used to format arbitrary dates programmatically.
    // @group dateFormatting
    // @param  date    (Date) The date to format
    // @param  format  (FormatString) The format to apply to this date
    // @return (String) formatted date string
    // @visibility external
    //<
    // This method is ultimately based on code from the DateJS library (www.datejs.com), refactored
    // to remove dependencies on that library's support structures and i18n approaches and integrate
    // the SmartClient i18n system.  It is also enhanced to support more of the functionality of 
    // Java's SimpleDateFormat class, and to support fiscal days, weeks and years.
    format : function (date, format) {

        if (!isc.isA.String(format)) {
            isc.logWarn("Cannot use Date format '" + format + "' - not a String");
            return date.toString();
        }

        var p = function p(s) {
            return (s.toString().length == 1) ? "0" + s : s;
        };

        return format ? format.replace(/dd?d?d?|EE?E?E?|MM?M?M?|yy?y?y?|YY?Y?Y?|LL?L?L?|ww?|CC?|cc?|DD?|hh?|HH?|mm?|ss?|a|u/g, 
        function (format) {
            switch (format) {
            case "hh":
                return p(date.getHours() < 13 ? date.getHours() : (date.getHours() - 12));
            case "h":
                return date.getHours() < 13 ? date.getHours() : (date.getHours() - 12);
            case "HH":
                return p(date.getHours());
            case "H":
                return date.getHours();
            case "mm":
                return p(date.getMinutes());
            case "m":
                return date.getMinutes();
            case "ss":
                return p(date.getSeconds());
            case "s":
                return date.getSeconds();
            case "yyyy":
                return date.getFullYear();
            case "yy":
                return date.getFullYear().toString().substring(2, 4);
            case "YYYY":
                return isc.DateUtil.getWeekNumber(date)[0];
            case "YY":
                return isc.DateUtil.getWeekNumber(date)[0].toString().substring(2, 4);
            case "dddd":
            case "EEEE":
                return date.getDayName();
            case "ddd":
            case "E":
            case "EE":
            case "EEE":
                return date.getShortDayName();
            case "dd":
                return p(date.getDate());
            case "d":
                return date.getDate().toString();
            case "DD":
                return p(isc.DateUtil.getDayNumber(date));
            case "D":
                return isc.DateUtil.getDayNumber(date).toString();
            case "u":
                return (date.getDay() || 7) + "";
            case "MMMM":
                return date.getMonthName();
            case "MMM":
                return date.getShortMonthName();
            case "MM":
                return p((date.getMonth() + 1));
            case "M":
                return date.getMonth() + 1;
            case "w":
                return isc.DateUtil.getWeekNumber(date)[1];
            case "ww":
                return p(isc.DateUtil.getWeekNumber(date)[1]);
            case "a":
                return date.getHours() < 13 ? isc.Time.AMIndicator : isc.Time.PMIndicator;
            default:
                return "";
            }
        }
        ) : "";
    },

    getWeekNumber : function(date) {
        var d = new Date(date);
        d.setHours(0,0,0);
        // The ISO standard is: the first week of a year is the one that contains the year's 
        // first Thursday.  http://en.wikipedia.org/wiki/ISO-8601#Week_dates
        d.setDate(d.getDate() + 4 - (d.getDay()||7));
        // Get first day of year
        var yearStart = new Date(d.getFullYear(),0,1);
        // Calculate full weeks to nearest Thursday
        var weekNo = Math.ceil(( ( (d - yearStart) / 86400000) + 1)/7);
        // Return array of year and week number
        return [d.getFullYear(), weekNo];
    },
    
    getDayNumber : function(date) {
        var d = new Date(date);
        d.setHours(0,0,0);
        var yearStart = new Date(d.getFullYear(),0,1);
        return Math.ceil(((d - yearStart) / 86400000) + 1);
    },
    
    //> @classMethod DateUtil.createLogicalDate()
    // Create a new Date to represent a logical date value (rather than a specific datetime value),
    // typically for display in a +link{DataSourceField.type,date type field}. The generated
    // Date value will have year, month and date set to the specified values
    // (in browser native local time).
    // @param year (int) full year
    // @param month (int) month (zero based, so 0 is January)
    // @param date (int) date within the month
    // @return (Date) new javascript Date object representing the Date in question
    // @visibility external
    //<
    // For logical dates, the only requirement for the "time" component value is that the
    // date shows up correctly in local time.
    
    createLogicalDate : function (year, month, date, suppressConversion) {
        var d = new Date();
        d.setHours(12, 0, 0, 0);
        year = (year != null ? year : d.getFullYear());
        month = (month != null ? month : d.getMonth());
        date = (date != null ? date : d.getDate());
        d.setFullYear(year, month, date);

        if (suppressConversion) {
            // If the 'suppressConversion' flag was passed, we will want to return null to indicate
            // we were passed an invalid date if the values passed in had to be converted
            // (For example a month of 13 effecting the year, etc)
            var isValid = (d.getFullYear() == year &&
                           d.getMonth() == month &&
                           d.getDate() == date );
            if (!isValid) return null;
        }

        d.logicalDate = true;
        return d;
    }

});

//>	@class Date
//
//	Extensions to the Date class, including added static methods on the Date object, and
//  additional instance methods available on all date instances.
//
//  @treeLocation Client Reference/System
//  @visibility external
//<

//>	@classMethod    isc.timeStamp()
//  Shorthand for <code>new Date().getTime();</code>, this returns a timeStamp - a large number
//  which is incremented by 1 every millisecond.  Can be used to generate unique identifiers,
//  or perform timing tasks.
//
//  @visibility external
//	@return	(int)	a large integer (actually the number of milliseconds since 1/1/1970)
//<

isc.addGlobal("timeStamp", function () {
    
    return new Date().getTime()
});


// synonym
isc.addGlobal("timestamp", isc.timeStamp);


  //>DEBUG
// This lets us label methods with a name within addMethods
Date.prototype.Class = "Date";
Date.Class = "Date";
  //<DEBUG


isc.Date = Date;


isc.addProperties(Date, {
    // add a constant for an error message when attempting to convert an invalid string to a
    // date
    INVALID_DATE_STRING:"Invalid date format"
});


//
// add methods to the Date object itself for parsing additional formats
//
isc.addMethods(Date, {

//>	@classMethod	Date.newInstance()
//			Cover function for creating a date in the 'Isomorphic-style',
//				eg:   Date.newInstance(args)
//			rather than new Date(args)
//		@return				(Date)		Date object
//      @deprecated As of SmartClient 5.5, use +link{Date.create}.
//<
newInstance : function (arg1, arg2, arg3, arg4, arg5, arg6, arg7) {
	return new Date(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
},


//>	@classMethod	Date.create()
//  Create a new <code>Date</code> object - synonym for <code>new Date(arguments)</code>
//	@return (Date) Date object
//  @visibility external
//<
create : function (arg1, arg2, arg3, arg4, arg5, arg6, arg7) {
    // handle being passed a subset of parameters
    // Note that passing undefined into new Date() results in an invalid date where
    // getTime() returns NaN
    var undef;
    if (arg1 === undef) return new Date();
    if (arg2 === undef) return new Date(arg1);
    if (arg3 === undef) arg3 = 0;
    if (arg4 === undef) arg4 = 0;
    if (arg5 === undef) arg5 = 0;
    if (arg6 === undef) arg6 = 0;
    if (arg7 === undef) arg7 = 0;
	return new Date(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
},

//> @classMethod Date.createLogicalDate()
// Create a new Date to represent a logical date value (rather than a specific datetime value),
// typically for display in a +link{DataSourceField.type,date type field}. The generated
// Date value will have year, month and date set to the specified values
// (in browser native local time).
// @param year (int) full year
// @param month (int) month (zero based, so 0 is January)
// @param date (int) date within the month
// @return (Date) new javascript Date object representing the Date in question
// @visibility external
// @deprecated in favor of +link{DateUtil.createLogicalDate}
//<

createLogicalDate : function (year, month, date, suppressConversion) {
    return isc.DateUtil.createLogicalDate(year, month, date, suppressConversion);
},

//> @classMethod Date.createLogicalTime()
// Create a new Date object to represent a logical time value (rather than a specific datetime
// value), typically for display in a +link{DataSourceField.type,time type field}. The generated
// Date value will have year, month and date set to the epoch date (Jan 1 1970), and time
// elements set to the supplied hour, minute and second (in browser native local time).
// @param hour (int) hour (0-23)
// @param minute (int) minute (0-59)
// @param second (int) second (0-59)
// @return (Date) new Javascript Date object representing the time in question
// @visibility external
//<
// This is a synonym for Time.createLogicalTime();
createLogicalTime : function (hour, minute, second, millisecond) {
    return isc.Time.createLogicalTime(hour,minute,second,millisecond);
},

createDatetime : function (year, month, date, hours, minutes, seconds, milliseconds, suppressConversion) {
    var hasHours = hours != null,
        hasMinutes = minutes != null,
        hasSeconds = seconds != null;

    // Handle being passed strings
    if (isc.isA.String(hours)) hours = parseInt(hours || 12, 10);
    if (isc.isA.String(minutes)) minutes = parseInt(minutes || 0, 10);
    if (isc.isA.String(seconds)) seconds = parseInt(seconds || 0, 10);

    var newDate;
    if (!isc.Time._customTimezone) {
        newDate = new Date(year, month, date);
        if (hasHours) {
            if (milliseconds != null) newDate.setHours(hours, minutes, seconds, milliseconds);
            else if (hasSeconds) newDate.setHours(hours, minutes, seconds);
            else if (hasMinutes) newDate.setHours(hours, minutes);
            else newDate.setHours(hours);
        }

        if (!suppressConversion) return newDate;

        // If the 'suppressConversion' flag was passed, we will want to return null to indicate
        // we were passed an invalid date if the values passed in had to be converted
        // (For example a month of 13 effecting the year, etc)
        var isValid = (newDate.getFullYear() == year &&
                       newDate.getMonth() == month &&
                       newDate.getDate() == date &&
                       (!hasHours || newDate.getHours() == hours) &&
                       (!hasMinutes || newDate.getMinutes() == minutes) &&
                       (!hasSeconds || newDate.getSeconds() == seconds)
                       );
        return (isValid ? newDate : null);
    } else {

        // We need a date where the UTCTime is set such that when we apply our
        // custom timezone offset we get back the local time.
        // Do this by creating a new date with UTC time matching this custom display time
        // and then shifting that date by the inverse of our display timezone offset.
        if (hours == null) hours = 0;
        if (minutes == null) minutes = 0;
        if (seconds == null) seconds = 0;
        if (milliseconds == null) milliseconds = 0;

        newDate = new Date(Date.UTC(year, month, date, hours, minutes, seconds, milliseconds));
        // If the 'suppressConversion' flag was passed, we will want to return null to indicate
        // we were passed an invalid date if the values passed in had to be converted
        // (For example a month of 13 effecting the year, etc)
        // Easiest to check this against the date before we apply the offset to correct for
        // our timezone
        if (suppressConversion) {
            var isValid = (newDate.getUTCFullYear() == year &&
                           newDate.getUTCMonth() == month &&
                           newDate.getUTCDate() == date &&
                           (!hasHours || newDate.getUTCHours() ==hours) &&
                           (!hasMinutes || newDate.getUTCMinutes() == minutes) &&
                           (!hasSeconds || newDate.getUTCSeconds() == seconds)
                           );
            if (!isValid) newDate = null;
        }
        if (newDate != null) {
            // Subtract the UTCHoursDisplayOffset and UTCMinutesDisplayOffset, then adjust
            // for DST if required.
            
            newDate._applyTimezoneOffset(
                -isc.Time.UTCHoursDisplayOffset,
                -isc.Time.UTCMinutesDisplayOffset
            );

            newDate._applyTimezoneOffset(-isc.Time.getUTCHoursDisplayOffset(newDate, 0),
                                         -isc.Time.getUTCMinutesDisplayOffset(newDate, 0));
        }
        return newDate;
    }
},

//> @classMethod Date.getLogicalDateOnly()
// Get a logical date - a value appropriate for a DataSourceField of type "date" - from a
// datetime value (a value from a DataSourceField of type "datetime").
// <P>
// This method correctly takes into account the current
// +link{Time.setDefaultDisplayTimezone,display timezone}, specifically, the returned Date
// will reflect the day, month and year that appears when the datetime is rendered
// by a SmartClient component rather than the date values that would be returned by
// Date.getDay() et al (which can differ, since getDay() uses the browser's local timezone).
// <P>
// For further background on date, time and datetime types, storage and transmission, see
// +link{group:dateFormatAndStorage,this overview}.
//
// @param date (Date) a Date instance representing a datetime value
// @return (Date) a Date instance representing just the date portion of the datetime value, as
//                a logical date
// @visibility external
//<
getLogicalDateOnly : function (datetime) {
    if (!isc.isA.Date(datetime)) {
        isc.logWarn("getLogicalDateOnly() passed invalid value:" + datetime
            + ". Returning null.");
        return null;
    }
    var year,month,day;
    // handle being passed something that's already a logical date
    if (datetime.logicalDate) {
        year = datetime.getFullYear();
        month = datetime.getMonth();
        day = datetime.getDate();
    } else {
        var offsetDate = datetime._getTimezoneOffsetDate(
                            isc.Time.getUTCHoursDisplayOffset(datetime),
                            isc.Time.getUTCMinutesDisplayOffset(datetime)
                         );
        offsetDate._applyTimezoneOffset(0, offsetDate.getTimezoneOffset());

        month = offsetDate.getMonth();
        day = offsetDate.getDate();
        year = offsetDate.getFullYear();
    }

    return this.createLogicalDate(year, month, day);
},

//> @classMethod Date.getLogicalTimeOnly()
// Get a logical time - a value appropriate for a DataSourceField of type "time" - from a
// datetime value (a value from a DataSourceField of type "datetime").
// <P>
// This method correctly takes into account the current
// +link{Time.setDefaultDisplayTimezone,display timezone}, specifically, the returned Date will
// reflect the hour, minute and second that appears when the datetime is rendered by a SmartClient
// component rather than the time values that would be returned by Date.getHours() et al (which
// can differ, since getHours() uses the browser's local timezone).
// <P>
// For further background on date, time and datetime types, storage and transmission, see
// +link{group:dateFormatAndStorage,this overview}.
//
// @param date (Date) a Date instance representing a datetime value
// @return (Date) a Date instance representing just the time portion of the datetime value, as
//                a logical time
// @visibility external
//<
getLogicalTimeOnly : function (datetime) {
    if (!isc.isA.Date(datetime)) {
        isc.logWarn("getLogicalTimeOnly() passed invalid value:" + datetime
            + ". Returning null.");
        return null;
    }

    var offsetHours = 0, offsetMinutes = 0;
    if (!datetime.logicalTime) {
        offsetHours = isc.Time.getUTCHoursDisplayOffset(datetime);
        offsetMinutes = isc.Time.getUTCMinutesDisplayOffset(datetime) +
                        datetime.getTimezoneOffset();
    }

    return this.createLogicalTime(datetime.getHours() + offsetHours, datetime.getMinutes() + offsetMinutes,
                                  datetime.getSeconds(), datetime.getMilliseconds());
},


//> @classMethod Date.combineLogicalDateAndTime()
// Combine a logical date (a value appropriate for a DataSourceField of type "date") with a
// logical time (a value appropriate for a DataSourceField of type "time") into a datetime
// value (a value appropriate for a DataSourceField of type "datetime")
// <P>
// This method correctly takes into account the current
// +link{Time.setDefaultDisplayTimezone,display timezone}, specifically, the returned datetime
// value will show the same date and time as the passed date and time objects when rendered by
// a SmartClient component that has been configured with a field of type "datetime".
// <P>
// For further background on date, time and datetime types, storage and transmission, see
// +link{group:dateFormatAndStorage,this overview}.
//
// @param date (Date) a Date instance representing logical date value
// @param time (Date) a Date instance representing logical time value
// @return (Date) a Date instance representing a datetime value combining the logical date and
//                time passed
// @visibility external
//<
combineLogicalDateAndTime : function (date, time) {
    var hasDate = isc.isA.Date(date),
        hasTime = isc.isA.Date(time);
    if (!hasDate || !hasTime) {
        // date only, convert from logical date to datetime.
        if (hasDate) {
            // pass in the result of 'getFullYear()' etc. These numbers are the correct
            // abs values - createDatetime will handle shifting them to account for
            // timezones.
            return this.createDatetime(date.getFullYear(), date.getMonth(), date.getDate(), 0,0,0);
        } else if (hasTime) {
            // We could log a warning and bail in this case. However may as well just
            // give back a datetime with the same time value as the 'time' passed in.
            return time.duplicate();
        } else {
            isc.logWarn("combineLogicalDateAndTime passed invalid parameters:"
                 + date + " and " + time + ". Returning null.");
            return null;
        }
    }

    // Get hours / minutes in display timezone.
    var hour = time.getHours(),
        minutes = time.getMinutes();
    return this.createDatetime(
                date.getFullYear(), date.getMonth(), date.getDate(),
                hour, minutes, time.getSeconds(), time.getMilliseconds()
           );
},


//>	@classMethod	Date.compareDates()
// Compare two dates; returns 0 if equal, -1 if the first date is greater (later), or 1 if
// the second date is greater.  If either value is not a Date object, it is treated as the
// epoch (midnight on Jan 1 1970) for comparison purposes.
//  @param  date1   (date)  first date to compare
//  @param  date2   (date)  second date to compare
//  @return (int)    0 if equal, -1 if first date &gt; second date, 1 if second date &gt; first date
// @visibility external
//<
compareDates : function (a, b) {
    if (a == b) return 0; // same date instance
	var aval = (isc.isA.Date(a) ? a.getTime() : 0),
        bval = (isc.isA.Date(b) ? b.getTime() : 0);
	return aval > bval ? -1 : (bval > aval ? 1 : 0);
},

//>	@classMethod	Date.compareLogicalDates()
// Compare two dates, normalizing out the time elements so that only the date elements are
// considered; returns 0 if equal, -1 if the first date is greater (later), or 1 if
// the second date is greater.
//  @param  date1   (date)  first date to compare
//  @param  date2   (date)  second date to compare
//  @return (int)    0 if equal, -1 if first date &gt; second date, 1 if second date &gt;
//                      first date.  Returns false if either argument is not a date
// @visibility external
//<
compareLogicalDates : function (a, b) {
    if (a == b) return 0; // same date instance
    if (!isc.isA.Date(a) || !isc.isA.Date(b)) return false; // bad arguments, so return false
	var aYear = a.getFullYear(),
	    aMonth = a.getMonth(),
	    aDay = a.getDate(),
	    bYear = b.getFullYear(),
	    bMonth = b.getMonth(),
	    bDay = b.getDate();

    var aval = aYear * 10000 + aMonth * 100 + aDay,
        bval = bYear * 10000 + bMonth * 100 + bDay;

	return aval > bval ? -1 : (bval > aval ? 1 : 0);
},

// `month' begins at 0.
getJulianDayNumber : function (year, month, date) {
    // http://quasar.as.utexas.edu/BillInfo/JulianDatesG.html
    var y = year,
        m = month + 1,
        d = date;

    if (m <= 2) {
        --y;
        m += 12;
    }
    var a = parseInt(y / 100),
        b = parseInt(a / 4),
        c = 2 - a + b,
        e = parseInt(365.25 * (y + 4716)),
        f = parseInt(30.6001 * (m + 1))
    return c + d + e + f - 1524;
},

_getWeekdayCounts : function () {
    var weekendDays = isc.Date.getWeekendDays();
    var weekdayCounts = weekendDays._weekdayCounts;
    if (!weekdayCounts) {
        var isWeekend = {}, numWeekends = 0;
        for (var i = 0; i < weekendDays.length; ++i) {
            if (!isWeekend[weekendDays[i]]) {
                ++numWeekends;
                isWeekend[weekendDays[i]] = true;
            }
        }

        weekdayCounts = [];
        for (var d = 0; d <= 6; ++d) {
            var weekdayCount = 0;
            var counts = [ 0 ];
            for (var dd = 1; dd < 7; ++dd) {
                if (!isWeekend[(d + dd - 1) % 7]) ++weekdayCount;
                counts.push(weekdayCount);
            }
            weekdayCounts[d] = counts;
        }
        weekdayCounts._numWeekends = numWeekends;
        weekendDays._weekdayCounts = weekdayCounts;
    }
    return weekdayCounts;
},

_getDayDiff : function (date1, date2, weekdaysOnly, useCustomTimezone) {
    var logicalDate1, logicalDate2;
    var compareRes = isc.Date.compareDates(date1, date2);
    var sign = (compareRes > 0 ? 1 : -1);
    if (compareRes >= 0) { // `date1' is before `date2'.
        if (useCustomTimezone !== false) {
            logicalDate1 = isc.Date.getLogicalDateOnly(date1);
            logicalDate2 = isc.Date.getLogicalDateOnly(date2);
        } else {
            logicalDate1 = date1;
            logicalDate2 = date2;
        }
    } else {
        if (useCustomTimezone !== false) {
            logicalDate1 = isc.Date.getLogicalDateOnly(date2);
            logicalDate2 = isc.Date.getLogicalDateOnly(date1);
        } else {
            logicalDate1 = date2;
            logicalDate2 = date1;
        }
    }

    var jd1 = isc.Date.getJulianDayNumber(logicalDate1.getFullYear(), logicalDate1.getMonth(), logicalDate1.getDate()),
        jd2 = isc.Date.getJulianDayNumber(logicalDate2.getFullYear(), logicalDate2.getMonth(), logicalDate2.getDate());

    if (weekdaysOnly) {
        var dd = jd2 - jd1;
        var weekdayCounts = isc.Date._getWeekdayCounts();
        return sign * (parseInt(dd / 7) * (7 - weekdayCounts._numWeekends) + weekdayCounts[logicalDate1.getDay()][dd % 7]);
    } else {
        return sign * (jd2 - jd1);
    }
},

//>	@type DateInputFormat
//  3 character string containing the <code>"M"</code>, <code>"D"</code> and <code>"Y"</code>
//  characters to indicate the format of strings being parsed into Date instances via
//  <code>Date.parseInput()</code>.
//  <P>
//  As an example - an input format of "MDY" would parse "01/02/1999" to Jan 2nd 1999
// <smartclient>
//  <P>
//  Note: In addition to these standard formats, a custom date string parser function may be
//  passed directly to +link{Date.setInputFormat()} or passed into +link{Date.parseInput()} as
//  the inputFormat parameter.
// </smartclient>
//  @visibility external
//<

//> @classMethod Date.setInputFormat()
// Sets up the default system-wide input format for strings being parsed into dates via
// <code>Date.parseInput()</code>. This will effect how SmartClient components showing editable
// date or datetime fields parse user-entered values into live Date objects.
// <P>
// The input format can be specified as a +link{type:DateInputFormat} - a 3 character string like
// <code>"MDY"</code> indicating the order of the Month, Day and Year components of date strings.
// <P>
// As an example - an input format of "MDY" would parse "01/02/1999" to Jan 2nd 1999<br>
// This standard parsing logic will also handle date-time strings such as "01/02/1999 08:45", or
// "01/02/1999 16:21:05".
// <P>
// Notes:
// <ul>
// <li>If the inputFormat is not explicitly set,the system automatically determines
//     the standard input format will be based on the specified +link{Date.setShortDisplayFormat,Date.shortDisplayFormat}
//     wherever possible.
//     For example if the short display format has been set to "toEuropeanShortDate" the input
//     format will default to "DMY".</li>
// <li>The default date parsing functionality built into SmartClient will handle dates presented
//     with any separator string, and can handle 1 or 2 digit day and month values and 2 or 4
//     digit year values. This means that in many cases custom date display formats can be parsed
//     back to Date values without the need for a custom parser function. However if more
//     sophisticated parsing logic is required, a function may be passed into this method. In
//     this case the parser function should be able to handle parsing date and datetime values
//     formatted via +link{Date.toShortDate()} and +link{Date.toShortDateTime()}.</li>
// <li>Date parsing and formatting logic may be overridden at the component level by setting
//     properties directly on the component or field in question.</li>
// </ul>
// @param format (DateInputFormat | function) Default format for strings to be parsed into Dates.
// <smartclient>
// If this method is passed a function, it is expected to take a single parameter
// (the formatted date string), and return the appropriate Javascript Date object (or null if
// appropriate).
// </smartclient>
// @see Date.parseInput()
// @example dateFormat
// @example customDateFormat
// @visibility external
//<
setInputFormat : function (format) {
    
    this._inputFormat = format;
},

//> @classMethod Date.getInputFormat()
// Retrieves the default format for strings being parsed into dates via
// <code>Date.parseInput()</code>
// @see Date.setInputFormat()
// @return (string) the current inputFormat for dates
// @visibility external
//<
getInputFormat : function () {
    if (this._inputFormat != null) return this._inputFormat;
    return this.mapDisplayFormatToInputFormat("toShortDate");
},

// Given a display format return the associated input format
_inputFormatMap:{
    toUSShortDate:"MDY",
    toUSShortDateTime:"MDY",
    toUSShortDatetime:"MDY",
    toEuropeanShortDate:"DMY",
    toEuropeanShortDateTime:"DMY",
    toEuropeanShortDatetime:"DMY",
    toJapanShortDate:"YMD",
    toJapanShortDateTime:"YMD",
    toJapanShortDatetime:"YMD"
},
mapDisplayFormatToInputFormat : function (displayFormat) {
    if (displayFormat == null || displayFormat == "toShortDate") {
        displayFormat = Date.prototype._shortFormat;
    } else if (displayFormat == "toNormalDate") {
        displayFormat = Date.prototype.formatter;
    }
    if (isc.isA.Function(displayFormat)) {
        isc.Log.logInfo("Unable to determine input format associated with display format " +
                        "function - returning default input format", "Date");
        return this._inputFormat || "MDY";
    }
    var inputFormat = this._inputFormatMap[displayFormat];
    // Note: isA.String check is necessary - all objects have toString / toLocaleString
    // present on them and we definitely don't want to return those native object formatters
    // as what will become a dateString parsing function!
    if (inputFormat != null && isc.isA.String(inputFormat)) return inputFormat;

    // a couple of special cases where we actually return functions.
    if (displayFormat == "toSerializeableDate") return this.parseSchemaDate;

    // Otherwise you're on your own - assume you've set up input foramt, or overridden this method
    isc.Log.logInfo("Unable to determine input format associated with display format " +
                     displayFormat + " - returning default input format", "Date");

    return this._inputFormat || "MDY";
},

//>	@classMethod	Date.parseInput()
// Parse a date passed in as a string, returning the appropriate date object.
// @param dateString (string) date value as a string
// @param [format] (DateInputFormat) Format of the date string being passed.
//                                      If not passed, the default date input format as set up
//                                      via setInputFormat() will be used.
// @param [centuryThreshold] (integer) For date formats that support a 2 digit
//                                  year, if parsed year is 2 digits and less than this
//                                  number, assume year to be 20xx rather than 19xx
// @param [suppressConversion] (Boolean)
//          If the string passed in was not a valid date, in some cases we can convert to a
//          valid date (for example incrementing the year if the month is greater than 12).
//          This optional parameter will suppress such conversions - anything that doesn't
//          parse directly to a valid date will simply return null.
// @return (Date) date value, or null if the string could not be parsed to a valid date.
// @group dateFormatting
// @visibility external
//<

// Note: undocumented isDatetime parameter. Are we creating a logical "date" value or a standard
// datetime type value where the time component is important? If ommitted assume datetime.
// Implementation-wise, if isDatetime is explicitly false, we will use the system local timezone
// rather than any timezone specified via Time.setDisplayTimezone().

parseInput : function (dateString, format, centuryThreshold, suppressConversion,
                        isDatetime)
{
    var logicalDate = (isDatetime == false);

    if (isc.isA.Date(dateString)) return dateString;

    if (!isc.isA.String(dateString) || isc.isAn.emptyString(dateString)) {
        return null;
    }

    // Strip the '$$DATE$$:' prefix if present.
    var origDateString = dateString;
    dateString = dateString.trim();
    
    if (dateString.startsWith("$$DATESTAMP$$:")) {
        return new Date(parseInt(dateString.substring(14)));
    }
    
    if (dateString.startsWith("$$DATE$$:")) {
        dateString = dateString.substring(9).trimLeft();
    }

    // Default to the standard input format
    if (format == null) format = this.getInputFormat();

    // If the format passed in is the name of a function on the Date class, or an
    // explicit function, assume its a parser and call it directly
    
    if (isc.isA.Function(Date[format])) format = Date[format];
    if (isc.isA.Function(format)) {
        return format(origDateString, centuryThreshold, suppressConversion);
    }

    // use the helper method _splitDateString() to get an array of values back
    // (representing year / month / day, etc.)
    // If null is returned, this was not a valid date - just return null.
    // Otherwise make the month zero-based, by reducing by one, and pass construct a new date
    // from the values returned.
    var array = this._splitDateString(dateString, format);

    if (array != null) {
        var year = array[0],
            bce = year && year.contains("-");

        if (year && bce) year = year.replaceAll("-", "");

        if (year) {
            if (year.length <= 2) {
                year = parseInt(year, 10);
                if (centuryThreshold != null) {
                    if (year < centuryThreshold) year += 2000;
                    else year += 1900;
                }
                array[0] = year;
            } else if (year.length == 3) {
                array[0] = "0" + year.toString();
            } else {
                array[0] = year;
            }
            if (bce) array[0] = "-" + array[0];
        }

        if (logicalDate) {
            return Date.createLogicalDate(array[0], array[1], array[2], suppressConversion);
        } else {
            return Date.createDatetime(array[0], array[1], array[2],
                        array[3], array[4], array[5], null, suppressConversion);
        }
    } else {
        return null;
    }
},

// Helper used by the Relative date item -- returns true if the date-string passed
// in includes a time portion.
// False if it does not (or if it's not a recognized date-string at all)
isDatetimeString : function (dateString, format) {
    format = format || isc.Date.getInputFormat();
    if (!isc.isA.Function(format)) {
        var array = this._splitDateString(dateString, format, false);
        if (array == null) return false;

        return (array[3] != null && !isc.isA.emptyString(array[3])) && 
               (array[4] != null && !isc.isA.emptyString(array[4]));
    }

    
    if (!dateString.contains(" ")) return false;
    // get the offset of the last colon and assume there's a valid time portion if the characters
    // either side of it are numbers
    var colonOffset = dateString.lastIndexOf(":");
    if (colonOffset < 1) return false;
    if (isNaN(dateString.substring(colonOffset-1, colonOffset)) ||
        isNaN(dateString.substring(colonOffset+1, colonOffset+2)))
    {
        return false;
    }
    return true;
},

// Parse a date or datetime value from a dataset or specified in code.
// NB: unlike parseInput, this method should not change behavior in different locales, or dates
// coming over the wire or specified in code will suddenly break!
//
// For Datetime, XML Schema uses "2005-08-01T21:35:48.350Z", see
//    http://www.w3.org/TR/xmlschema-2/#dateTime
// SmartClient Server parses "yyyy-mm-dd" format
parseSchemaDate : function (value) {
    if (isc.isA.Date(value)) return value;

    if (!isc.isA.String(value)) value = (value.toString ? value.toString() : value + "");

    // Notes on regex:
    // - result[4] is the optional timestamp including the T and colon separators
    // - result[8] would be the optional milliseconds including the ".", whereas
    //   result[9] is just the numeric part
    //   results[10] is the timezone - either "Z" (zulu time or GMT) or +/- HH:MM
    var result = value.match(/(\d{4})[\/-](\d{2})[\/-](\d{2})([T ](\d{2}):(\d{2})(:(\d{2}))?)?(\.(\d+))?([+-]\d\d?:\d{2}|Z)?/);
//    isc.Log.logWarn("isDate: '" + value + "', regex match: " + result);

    if (result == null) return null;

    value = value.trim();

    var secondsIndex = 8;
    var msIndex = 10;
    var tzIndex = 11;

    
    var dateValue;
    // NOTE: pass only the relevant arguments as Moz does not like being passed nulls
    
    if (!result[4]) { // no VALID time
        // before creating a logical date, check if the value has additional characters that
        // make it look like it has something in place of a time-value, even if it isn't 
        // valid for schema-format - return null in this case, rather than a valid logicalDate
        if (value.length > 10 && value.contains(" ")) return null;
        dateValue = Date.createLogicalDate(result[1], result[2] - 1, result[3]);
    } else if (!result[msIndex]) { // no ms
        dateValue = new Date(Date.UTC(result[1], result[2] - 1, result[3],
                                      result[5], result[6], result[secondsIndex] || 0));
    } else {
        var ms = result[msIndex];

        // XML Schema says any number of fractional digits can be specified.  new Date() is
        // expecting a whole number of milliseconds (and further precision would be ignored).
        // Multiply by a power of ten based on the number of digits provided, such that ".9"
        // becomes 900 and ".98367" becomes 984.
        if (ms.length != 3) {
            var multiplier = Math.pow(10,3-ms.length);
            ms = Math.round(parseInt(ms,10) * multiplier);
        }
        //isc.Log.logWarn("ms is: " + ms);

        dateValue = new Date(Date.UTC(result[1], result[2] - 1, result[3],
                                      result[5], result[6], result[secondsIndex] || 0, ms));
    }
    // Handle timezone offset from GMT

    if (result[tzIndex] && result[tzIndex].toLowerCase() != "z") {
        var HM = result[tzIndex].split(":"),
            H = HM[0],
            negative = H && H.startsWith("-"),
            M = HM[1];
        H = parseInt(H, 10);
        M = parseInt(M, 10);
        var dateTime = dateValue.getTime();

        
        // Note no need to account for negative on hours since the "+" or "-" prefix was picked up
        // in parseInt
        if (isc.isA.Number(H)) dateTime -= (3600000 * H);
        if (isc.isA.Number(M)) dateTime -= (60000 * M * (negative ? -1 : 1));
        dateValue.setTime(dateTime);
    }

    return dateValue
},

//>!BackCompat 2005.11.3
// parseDate() was old name for parseInput
parseDate : function (dateString, format, centuryThreshold, suppressConversion) {
    return this.parseInput(dateString, format, centuryThreshold, suppressConversion);
},

// For completeness also support parseDatetime()
parseDateTime : function (dateString, format, centuryThreshold, suppressConversion) {
    return this.parseDatetime(dateString,format,centuryThreshold,suppressConversion);
},
parseDatetime : function (dateString, format, centuryThreshold, suppressConversion) {
    return this.parseInput(dateString, format, centuryThreshold, suppressConversion);
},
//<!BackCompat

// ISC DSResponses that use our SQLTransform logic (basically our backend DB implementation)
// will call this method by default - giving the user an opportunity to override.  This can be
// disabled by setting jsTranslater.writeNativeDate: true in server.properties.
//
// Note: month is zero-based, just like the native Date constructor.
parseServerDate : function (year, month, day) {
    return Date.createLogicalDate(year, month, day);
},

// ISC DSResponses will call this method by default for fields of type "time"
parseServerTime : function (hour, minute, second) {
    return Date.createLogicalTime(hour, minute, second);
},


_splitDateString : function (string, format, zeroEmptyTime) {
    var isFunc = isc.isA.Function(format);

    if (zeroEmptyTime == null) zeroEmptyTime = true;

    var month, day, year, hour, minute, second;

    var monthIndex = format && !isFunc ? format.indexOf("M") : 0,
        dayIndex = format && !isFunc ? format.indexOf("D") : 1,
        yearIndex = format && !isFunc ? format.indexOf("Y") : 2; 
    // shortDate implies it's of the format MM/DD/YYYY
    
    //>Safari12
    if (isc.Browser.isSafari && isc.Browser.safariVersion <= 312) {
        var splitDate = this._splitDateViaSubstring(string, monthIndex, dayIndex, yearIndex,
                                                    zeroEmptyTime);
        year = splitDate[0];
        month = splitDate[1];
        day = splitDate[2];
        hour = splitDate[3];
        minute = splitDate[4];
        second = splitDate[5];

    // For browsers that support RegExp properly, use regexp pattern matching to get the result
    // (This has the advantage that we can deal with dates of the form 1/1/1999, and attempt to
    //  convert MM/YY/DD -- though we're relying on the native browser handling for the
    //  Date constructor being passed a 2 digit year)
    } else {
    //<Safari12

        // Each of the first three slots is either YYYY / YY or MM / M (or DD/D) (depends on the
        // format passed in)
        // Note: We don't support years greater than 9999. Attempting to set a year greater than
        // 9999 on a JS date causes a native browser crash on IE6
        var regex =
        //          YYYY || YY/[M]M  /  YYYY || YY/[M]M  /  YYYY || YY/[M]M [(space) [H]H  :    MM    [:     SS]]
        new RegExp(/^\s*(-?\d{1,4})[^\d](-?\d{1,4})[^\d](-?\d{1,4})([^\d](\d{1,2})[^\d](\d\d)[^\d]?(\d\d)?)?\s*$/),
            results = string.match(regex);

        if (results == null) return null;
        // Notes - we need to match the order of day / month / year to the format passed in
        // Also - the month value in the string is 1-based rather than zero based

        // Note: this was parseInt(results[index]) -1, but both IE and Mozilla will do the
        // wrong thing here - if the substring was "09", the parseInt would return 0 rather
        // than 9.
        // In any case, the parseInt is rendered unnecessary by the 'isA.Number' check below.
        month = results[monthIndex +1] -1;
        day = results[dayIndex+1];
        year = results[yearIndex +1];

        // Note - results[4] is the whole time string (if present)
        // Zero out any time fields that are not present - this may happen if
        // - time has invalid format (could check by examining results[4] too)
        // - time not included in dateString (could check by examining results[4] too)
        // - time has no seconds (legal - just zero out the seconds)
        hour = results[5];
        if (zeroEmptyTime && hour == null) hour = 0;
        minute = results[6];
        if (zeroEmptyTime && results[6] == null) minute = 0;
        second = results[7];
        if (zeroEmptyTime && results[7] == null) second = 0;
    //>Safari12
    }
    //<Safari12
    // If they all are numbers, this was a valid date string
    // NOTE: If year - month - day gives a number then they
    // are all numbers, or strings that implicitly convert to numbers.
    // We could also use this syntax:
    // if(parseInt(year) == year && parseInt(month) == month ...)
    // but this is slower in both Moz and IE
    var isValid = zeroEmptyTime ? 
                    isc.isA.Number(year - month - day - hour - minute - second) :
                    isc.isA.Number(year - month - day);
    if (isValid) {
        // Return the hours modulo 24 in case the hours were formatted by a Java `SimpleDateFormat'
        // using the 'k' pattern char. This takes care of both 'H' and 'k'.
        // http://ideone.com/E5HC4E
        
        return ([year,month,day,hour != null ? hour % 24 : null ,minute,second]);
    }
    else return null
},

//>	@type DateDisplayFormat
// Valid display formats for dates.  These strings are the names of formatters which can be
// passed to <code>Date.setNormalDisplayFormat()</code> or <code>Date.setShortDisplayFormat()</code>
// and will be subsequently used as default long or short formatters for date objects by
// SmartClient components.<br>
// Default set of valid display formats is as follows:<br><br>
//
// @value toString
// Default native browser 'toString()' implementation. May vary by browser.<br>
// <i>Example</i>: <code>Fri Nov 04 2005 11:03:00 GMT-0800 (Pacific Standard Time)</code>
// @value toLocaleString
// Default native browser 'toLocaleString()' implementation. May vary by browser.
// <i>Example</i>: <code>Friday, November 04, 2005 11:03:00 AM</code>
// @value toUSShortDate Short date in format MM/DD/YYYY.<br>
// <i>Example</i>: <code>11/4/2005</code>
// @value toUSShortDatetime Short date with time in format MM/DD/YYYY HH:MM<br>
// <i>Example</i>: <code>11/4/2005 11:03</code>
// @value toEuropeanShortDate Short date in format DD/MM/YYYY.<br>
// <i>Example</i>: <code>4/11/2005</code>
// @value toEuropeanShortDatetime Short date with time in format DD/MM/YYYY HH:MM<br>
// <i>Example</i>: <code>4/11/2005 11:03</code>
// @value toJapanShortDate Short date in format YYYY/MM/DD.<br>
// <i>Example</i>: <code>2005/11/4</code>
// @value toJapanShortDatetime Short date with time in format YYYY/MM/DD HH:MM<br>
// <i>Example</i>: <code>2005/11/4 11:03</code>
// @value toSerializeableDate Date in the format YYYY-MM-DD HH:MM:SS<br>
// <i>Example</i>: <code>2005-11-04 11:09:15</code>
// @value toDateStamp   Date in the format &lt;YYYYMMDD&gt;T&lt;HHMMSS&gt;Z
// <i>Example</i>: <code>20051104T111001Z</code>
// <br>
// <br>
// Note: In addition to these standard formats, custom formatting can be set by passing
// a function directly to +link{Date.setNormalDisplayFormat()} et al. This
// function will then be executed whenever the appropriate formatter method is called [eg
// +link{date.toNormalDate()}], in the scope of the date instance in question.
// <p>
// Custom formatting can also be applied by passing a +link{FormatString} instead of a 
// <code>DateDisplayFormat</code> string to +link{Date.setNormalDisplayFormat()} et al. See
// the <code>FormatString</code> docs for details.
//
//  @visibility external
//<

//> @classMethod Date.setNormalDisplayFormat()
// Set the default formatter for date objects to the method name passed in.  After calling this
// method, subsequent calls to +link{Date.toNormalDate()} will return a string formatted
// according to this format specification. Note: this will be the standard long date format used
// by SmartClient components.
// <p>
// The <code>format</code> parameter may be a +link{FormatString}, a +link{DateDisplayFormat} 
// string, or a function. If passed a function, this function will be executed in the scope of
// the Date and should return the formatted string.<br>
// <p>
// Initial default normalDisplayFormat is <code>"toLocaleString"</code>
// @group	dateFormatting
// @param	format	(FormatString | DateDisplayFormat | function)	new formatter
//      @visibility external
//<
setNormalDisplayFormat : function (format) {
    // if a valid formatter was passed in, set our .formatter property
	if (isc.isA.Function(Date.prototype[format]) || 
        isc.isA.Function(format) || 
        isc.isA.String(format)) 
    {
	    Date.prototype.formatter = format;
    }
},

setNormalDateDisplayFormat : function (format) {
    this.setNormalDisplayFormat(format);
},

//> @classMethod Date.setNormalDatetimeDisplayFormat()
//  Set the default normal format for datetime values. After calling this method, subsequent calls to
// +link{Date.toNormalDatetime()} will return a string formatted according to this format
// specification. Note that this will be the standard datetime format used by
// SmartClient components.
// <P>
// The <code>format</code> parameter may be a +link{FormatString}, a +link{DateDisplayFormat} 
// string, or a function. If passed a function, this function will be executed in the scope of
// the Date and should return the formatted string.<br>
//
// @group	dateFormatting
// @param	format	(FormatString | DateDisplayFormat | function)	new formatter
// @example dateFormat
// @example customDateFormat
// @visibility external
//<
setNormalDatetimeDisplayFormat : function (format) {
    // if a valid formatter was passed in, set our .formatter property
	if (isc.isA.Function(Date.prototype[format]) || 
        isc.isA.Function(format) || 
        isc.isA.String(format)) 
    {
	    Date.prototype.datetimeFormatter = format;
    }
},

//>	@classMethod	Date.setShortDisplayFormat()
// Set the default short format for dates. After calling this method, subsequent calls to
// +link{Date.toShortDate()} will return a string formatted according to this format
// specification. Note that this will be the standard short date format used by
// SmartClient components.
// <P>
// The <code>format</code> parameter may be a +link{FormatString}, a +link{DateDisplayFormat} 
// string, or a function. If passed a function, this function will be executed in the scope of
// the Date and should return the formatted string.<br>
// <P>
// Initial default shortDateFormat is <code>"toUSShortDate"</code>. This property
// is commonly modified for localization of applications. See
// +externalLink{http://en.wikipedia.org/wiki/Date_format_by_country}
// for a useful overview of standard date formats per country.
//
// @group	dateFormatting
// @param	format	(FormatString | DateDisplayFormat | function)	new formatter
// @example dateFormat
// @example customDateFormat
// @visibility external
//<
setShortDisplayFormat : function (format) {
	if (isc.isA.Function(Date.prototype[format]) || 
        isc.isA.Function(format) || 
        isc.isA.String(format)) 
    {
        Date.prototype._shortFormat = format;
    }
},

//>	@classMethod Date.setDefaultDateSeparator
// Sets a new default separator that will be used when formatting dates. By default, this
// is a forward slash character: "/"
// @group   dateFormatting
// @param separator (string) separator to use in dates
// @visibility external
//<
setDefaultDateSeparator : function (separator) {
    Date.prototype._shortDateTemplate = [,,,,separator,,,,,separator,,,,null];
    Date.prototype._separator = separator;
},

//>	@classMethod Date.getDefaultDateSeparator
// gets the default date separator string
// @group   dateFormatting
// @return (string) the default date separator
// @visibility external
//<
getDefaultDateSeparator : function (separator) {
    if (Date.prototype._separator) return Date.prototype._separator;
    else return "/";
},

//> @classMethod Date.setShortDatetimeDisplayFormat()
//  Set the default short format for datetime values. After calling this method, subsequent calls to
// +link{Date.toShortDateTime()} will return a string formatted according to this format
// specification. Note that this will be the standard datetime format used by
// SmartClient components.
// <P>
// The <code>format</code> parameter may be a +link{FormatString}, a +link{DateDisplayFormat} 
// string, or a function. If passed a function, this function will be executed in the scope of
// the Date and should return the formatted string.<br>
// <P>
// Initial default format is <code>"toUSShortDatetime"</code>.  See
// +externalLink{http://en.wikipedia.org/wiki/Date_format_by_country}
// for a useful overview of standard date formats per country.
//
// @group	dateFormatting
// @param	format	(FormatString | DateDisplayFormat | function)	new formatter
// @example dateFormat
// @example customDateFormat
// @visibility external
//<
setShortDatetimeDisplayFormat : function (format) {
	if (isc.isA.Function(Date.prototype[format]) || 
        isc.isA.Function(format) || 
        isc.isA.String(format)) 
    {
        Date.prototype._shortDatetimeFormat = format;
    }
},


//> @object FiscalYear
//
// An object representing the start of a given Fiscal Year in the current locale.
// <P>
// See +link{FiscalCalendar} for more information on how FiscalYears are set up and used.
//
// @treeLocation Client Reference/System/Date
// @visibility external
//<

//> @attr fiscalYear.fiscalYear (integer : null : IRW)
//
// The actual fiscal year that this date relates to.
// <P>
// A fiscal year ends when the next one begins. A fiscal year may span the boundary
// between two calendar years in which case the +link{fiscalYear.fiscalYear} value may
// not match the +link{fiscalYear.year} value.
// <P>
// For example fiscalYear 2020 may start in July of 2019 and end in July of 2020. In this
// case the <code>fiscalYear</code> would be set to <code>2020</code> and the
// +link{fiscalYear.year} would be set to <code>2019</code>
//
// @visibility external
//<

//> @attr fiscalYear.year (integer : null : IRW)
//
// The 4-digit calendar year when this fiscal year starts.
//
// @visibility external
//<

//> @attr fiscalYear.month (integer : null : IRW)
//
// The zero-based month-number when this fiscal year starts.
//
// @visibility external
//<

//> @attr fiscalYear.date (integer : null : IRW)
//
// The one-based day-number in the +link{fiscalYear.month, specified month} when this fiscal
// year starts.
//
// @visibility external
//<

//> @object FiscalCalendar
//
// An object representing the start date for fiscal years in the current locale.
// <P>
// A fiscal year spans a configurable date range - it may not exactly
// match a calendar year in length and it can start on any date within the calendar year
// and potentially end in the next calendar year.
// <P>
// Developers may specify explicit fiscal year start dates by adding +link{FiscalYear}
// objects to the +link{FiscalCalendar.fiscalYears, fiscal years array}.
// If none are provided, or if there is no entry for the given year, one is
// manufactured based on the default +link{FiscalCalendar.defaultMonth, month}
// and +link{FiscalCalendar.defaultDate, date}.
//
// @treeLocation Client Reference/System/Date
// @visibility external
//<

//> @attr fiscalCalendar.defaultMonth (integer : null : IRW)
//
// The default zero-based month-number to use for calculating fiscal dates when no
// +link{FiscalCalendar.fiscalYears, fiscal years} are provided. This value together
// with +link{fiscalCalendar.defaultDate} will be used as the start date for the
// fiscal years where no explicitly specified fiscalYear configuration is present.
// <br>
// See also +link{fiscalCalendar.defaultYearMode}.
//
// @visibility external
//<

//> @attr fiscalCalendar.defaultDate (integer : null : IRW)
//
// The default one-based day-number in the +link{fiscalCalendar.defaultMonth, specified month}
// to use for calculating fiscal dates when no +link{FiscalCalendar.fiscalYears, fiscal years}
// are provided. This value together
// with +link{fiscalCalendar.defaultMonth} will be used as the start date for the
// fiscal years where no explicitly specified fiscalYear configuration is present.
// <br>
// See also +link{fiscalCalendar.defaultYearMode}.
//
// @visibility external
//<

//> @type FiscalYearMode
//
// Strategies for calculating the FiscalYear within a +link{fiscalCalendar} from the
// specified +link{fiscalCalendar.defaultDate} and +link{fiscalCalendar.defaultMonth}
// If the specified fiscal year date starts in one calendar year and ends in the next.
//
// @value "end" The fiscalYear value for the date range will match the calendar year
//  in which the period ends. For example if the defaultDate and defaultMonth were set
//  to represent April 1st, the fiscal year starting on April 1st 2020 would end on
//  April 1st 2021. Setting the fiscalYearMode to <code>end</code> would mean the
//  fiscalYear value for this block would be 2021.
//
// @value "start" The fiscalYear value for the date range will match the calendar year
//  in which the period starts. For example if the defaultDate and defaultMonth were set
//  to represent April 1st, the fiscal year starting on April 1st 2020 would end on
//  April 1st 2021. Setting the fiscalYearMode to <code>start</code> would mean the
//  fiscalYear value for this block would be 2020.
// @visibility external
//<

//> @attr fiscalCalendar.defaultYearMode (FiscalYearMode : "end" : IRW)
//
// This attribute controls how the displayed fiscalYear value should be calculated for
// dates falling within a period not explicitly listed in the
// +lik{fiscalCalendar.fiscalYears,fiscal years array}.
// <P>
// The +link{fiscalCalendar.defaultMonth} and +link{fiscalCalendar.defaultDate} will be
// used to calculate the start of the fiscal year period. The defaultYearMode
// determines whether the reported fiscalYear for this period matches the year in which
// the period starts or the year in which it ends (so whether a fiscal year spanning
// dates within both 2020 and 2021 is reported as fiscalYear 2020 or 2021).
// @visibility external
//<

//> @attr fiscalCalendar.fiscalYears (Array of FiscalYear : null : IRW)
//
// An array of +link{FiscalYear, FiscalYear objects} which each represent the start date of a
// single fiscal year.
//
// @visibility external
//<

//>	@classMethod date.setFiscalCalendar()
// Sets the global fiscal calendar, which is used for all calls to
// getFiscalYear() / getFiscalWeek() if those methods aren't passed a fiscalCalander.
//
// @param fiscalCalendar (FiscalCalendar) the object representing the start month and date of
//           the fiscal year in the current locale
// @visibility external
//<
setFiscalCalendar : function (fiscalCalendar) {
    if (!fiscalCalendar.fiscalYears) fiscalCalendar.fiscalYears = [];
    Date.prototype.fiscalCalendar = fiscalCalendar;
    // init the start/endDate values on any specified FiscalYear objects
    Date._getFiscalYearObjectForDate(new Date());
},

//>	@classMethod date.getFiscalCalendar()
// Returns the global +link{FiscalCalendar, FiscalCalendar object} representing the start month and
// date of the fiscal year in the current locale.
// @return (FiscalCalendar)	the FiscalCalendar object
// @visibility external
//<
getFiscalCalendar : function () {
    if (!Date.prototype.fiscalCalendar.fiscalYears) {
        Date.prototype.fiscalCalendar.fiscalYears = [];
    }
    return Date.prototype.fiscalCalendar;
},

//>	@classMethod date.getFiscalStartDate()
// Returns the start date of the fiscal year for the passed date.
//
// @param date (Date | number) the date, or the year-number, to get the fiscal year for
// @param [fiscalCalendar] (FiscalCalendar) the object representing the starts of one or more
//                              fiscal years
// @return (Date)	the start of the fiscal year for the passed date and fiscalCalendar
// @visibility external
//<
getFiscalStartDate : function (date, fiscalCalendar) {
    var fiscalYear = Date._getFiscalYearObjectForDate(date, fiscalCalendar);
    return new Date(fiscalYear.year, fiscalYear.month, fiscalYear.date);
},


getFiscalEndDate : function (date, fiscalCalendar) {
    var fy = Date._getFiscalYearObjectForDate(date, fiscalCalendar),
        nfy = Date.getFiscalYear(fy.fiscalYear + 1);
    if (nfy.year < fy.fiscalYear) nfy = Date.getFiscalYear(nfy.fiscalYear + 1);
    var endDate = new Date(nfy.startDate.getTime()-1);
    return endDate;
},


_getFiscalYearObjectForDate : function (date, fiscalCalendar) {

    fiscalCalendar = fiscalCalendar || Date.getFiscalCalendar();
    if (!fiscalCalendar.fiscalYears) fiscalCalendar.fiscalYears = [];

    var fiscalYears = fiscalCalendar.fiscalYears;

    var defaultStartDate = fiscalCalendar.defaultDate,
        defaultStartMonth = fiscalCalendar.defaultMonth;
    // If unspecified default to calendar years.
    if (defaultStartDate == null) defaultStartDate = 1;
    if (defaultStartMonth == null) defaultStartMonth = 0;

    // In order to rapidly find the fiscalYearObject associated with some date,
    // do a one-time calculation of the start and endDates of each specified fiscal year
    // and store them on the objects.
    
    var initialized = true;
    for (var i = 0; i < fiscalYears.length; i++) {
        if (fiscalYears[i].startDate == null || fiscalYears[i].endDate == null) {
            initialized = false;

            fiscalYears[i].startDate = Date.createDatetime(
                                        fiscalYears[i].year,
                                        fiscalYears[i].month,
                                        fiscalYears[i].date
                                       );
       }
    }
    fiscalYears.setSort({property: "startDate", direction: "ascending" });
    if (!initialized) {
        for (var i = 0; i < fiscalYears.length; i++) {
            var endDate;

            var fy = fiscalYears[i],
                nextFY = fiscalYears[i+1];
            // If the next entry in the fiscalYears array starts in the following year
            // (or later in the same year), consider that the end date for this FY.
            // Otherwise use the defaultDate/defaultMonth of the next year.
            // This allows the specified fiscalYears array to be sparse
            // (For example custom behavior could be specified for 2000 and 2010 only, and
            // every year in between will use the default month/date start)
            if (nextFY && (nextFY.year == fy.year || (nextFY.year == fy.year+1))) {
                fy.endDate = new Date(nextFY.startDate.getTime()-1);
            } else {

                fy.endDate = Date.createDatetime(
                                fy.year+1, defaultStartMonth, defaultStartDate);
                // reduce by 1ms so it's the end of the prev day
                // This will avoid confusion with whether it rolls over a year
                // if the date is actually jan 1st
                fy.endDate.setTime(fy.endDate.getTime()-1);
            }
        }
    }


    // If we're passed just a year value, return the fiscalYear definition where
    // 'fiscalYear' is set to the specified date (may have to be created)
    if (!isc.isA.Date(date)) {

        var fiscalYearObj = fiscalYears.find("fiscalYear", date);
        if (fiscalYearObj != null) {
             return fiscalYearObj;
        }

        // We know we need to create a new fiscalYear object who's fiscalYear
        // property will be set to the specified date value.
        var calendarYear = date;
        if (fiscalCalendar.defaultYearMode != "start" &&
            (defaultStartMonth != 0 || defaultStartDate != 1))
        {
            calendarYear -= 1;
        }
        // Build a default object and return it.
        
        var result = {
            year:calendarYear,
            fiscalYear:date,
            month:defaultStartMonth,
            date:defaultStartDate,
            startDate: isc.DateUtil.getStartOf(new Date(calendarYear, defaultStartMonth, defaultStartDate))
        };
        return result;

    } else {
        var date_timestamp = date.getTime();
        // Array should already be sorted - re-sort just in case it was missed.
        fiscalYears.sortByProperty("startDate", Array.ASCENDING);
        for (var i = 0; i < fiscalYears.length; i++) {
            if (date_timestamp < fiscalYears[i].startDate.getTime()) break;
            if (date_timestamp <= fiscalYears[i].endDate.getTime()) {
                // date falls between start and end of the specified fiscal year so use it.
                return fiscalYears[i];
            }
        }

        // At this point we know we didn't have an entry in the fiscal years array
        // for this date, so create one based on the default start date
        var dateYear = date.getFullYear(),
            startDate = Date.createDatetime(dateYear,
                                          defaultStartMonth,
                                          defaultStartDate);
        // Date falls before default start date, shift back a year.
        if (startDate.getTime() > date_timestamp) {
            dateYear -= 1;
            startDate = Date.createDatetime(dateYear,
                                          defaultStartMonth,
                                          defaultStartDate);
        }

        // Calculate the endDate - the year it falls in will determine the reported
        // 'fiscalYear'.
        var endDate = Date.createDatetime(dateYear+1,
                                          defaultStartMonth,
                                          defaultStartDate);
        // Shunt back to the end of the prev day.
        endDate.setTime(endDate.getTime()-1);

        // If there's a specified fiscalYear in our array that starts before the
        // calculated endDate, truncate this year a little earlier to account for it.
        var endDate_timestamp = endDate.getTime();
        for (var i = 0; i < fiscalYears.length; i++) {

            if (endDate_timestamp < fiscalYears[i].endDate.getTime()) {
                continue;
            } else {
                if (endDate_timestamp > fiscalYears[i].startDate.getTime()) {
                    endDate = new Date(fiscalYears[i].startDate.getTime()-1);
                } else break;
            }
        }

        var fiscalYear = dateYear;
        // If we span 2 calendar years and the year mode is set to "end",
        // (or unset - since this is default behavior), increment the fiscal year to
        // match the end date.
        if (endDate.getFullYear() != startDate.getFullYear()
             && fiscalCalendar.defaultYearMode != "start")
        {
            if (endDate.getFullYear() < date.getFullYear()) {
                fiscalYear = date.getFullYear();

                var tempStart = new Date(fiscalYear, defaultStartMonth, defaultStartDate);
                if (date.getTime() > tempStart.getTime()) {
                    fiscalYear++;
                }
            } else {
                fiscalYear = endDate.getFullYear();
            }
        }

        return {
            year:dateYear,
            fiscalYear:fiscalYear,
            date:defaultStartDate,
            month:defaultStartMonth
        };

    }
},

//>	@classMethod date.setShowChooserFiscalYearPickers()
// Sets the global attribute that dictates whether the +link{DateChooser, choosers} shelled
// from +link{DateItem, DateItems} show a UI for working with Fiscal Years.
//
// @param showChooserFiscalYearPickers (boolean) whether to show Fiscal Year pickers in DateChoosers by default
// @visibility external
//<
setShowChooserFiscalYearPickers : function (showChooserFiscalYearPickers) {
    isc.DateItem.addProperties({
        showChooserFiscalYearPicker: showChooserFiscalYearPickers
    });
    isc.DateChooser.addProperties({
        showFiscalYearChooser: showChooserFiscalYearPickers
    });
},

//>	@classMethod date.setShowChooserWeekPickers()
// Sets the global attribute that dictates whether the +link{DateChooser, choosers} shelled
// from +link{DateItem, DateItems} show a UI for working with Weeks.
//
// @param showChooserWeekPickers (boolean) whether to show Fiscal Week pickers in DateChoosers by default
// @visibility external
//<
setShowChooserWeekPickers : function (showChooserWeekPickers) {
    isc.DateItem.addProperties({
        showChooserWeekPicker: showChooserWeekPickers
    });
    isc.DateChooser.addProperties({
        showWeekChooser: showChooserWeekPickers
    });
},


//>	@classMethod date.setFirstDayOfWeek()
// Sets the global attribute that dictates which day should be treated as the first day of the
// week in calendars and date calculations.  The parameter is expected to be an integer value 
// between 0 (Sunday) and 6 (Saturday).
// <P>
// The default value is picked up from the current locale.
//
// @param firstDayOfWeek (int) the number of the day to use as the first day of the week
// @visibility external
//<
setFirstDayOfWeek : function (firstDayOfWeek) {
    if (isc.DateChooser) {
        if (firstDayOfWeek == null || firstDayOfWeek < 0 || firstDayOfWeek > 6) 
            firstDayOfWeek = 0;
        isc.DateChooser.addProperties({firstDayOfWeek: firstDayOfWeek});
    }
},

//>	@classMethod date.getFirstDayOfWeek()
// Returns the global attribute that dictates which day should be treated as the first day of 
// the week in calendars and date calculations.  The parameter is expected to be an integer 
// value between 0 (Sunday) and 6 (Saturday).
// <P>
// The default value is picked up from the current locale.
//
// @return (int) the number of the day being used as the first day of the week
// @visibility external
//<
getFirstDayOfWeek : function () {
    if (isc.DateChooser) {
        return isc.DateChooser.getInstanceProperty("firstDayOfWeek");
    }
    return 0;
},



//>	@classMethod date.getFiscalYear()
// Returns the +link{FiscalYear} object for the fiscal year in which the passed date exists.
//
// @param date (Date | int) the date to get the fiscal year for
// @param [fiscalCalendar] (FiscalCalendar) the object representing the start of the fiscal period
// @return (FiscalYear) the +link{FiscalYear} object for the passed date
// @visibility external
//<
getFiscalYear : function (date, fiscalCalendar) {
    return Date._getFiscalYearObjectForDate(date, fiscalCalendar);
},

//>	@classMethod date.getFiscalWeek()
// Returns a date's week-number, according to the fiscal calendar
//
// @param date (Date) the date to get the fiscal year for
// @param [fiscalCalendar] (FiscalCalendar) the object representing the starts of fiscal years
// @return (int) the fiscal week for the passed date
// @visibility external
//<
_millisInADay: (1000 * 60 * 60 * 24),
getFiscalWeek : function (date, fiscalCalendar, firstDayOfWeek) {
    fiscalCalendar = fiscalCalendar || Date.getFiscalCalendar();

    var yearStart = Date.getFiscalStartDate(date, fiscalCalendar),
        logicalYearStart = Date.getLogicalDateOnly(yearStart),
        logicalDate = date.logicalDate ? date : Date.getLogicalDateOnly(date);
    return this._getWeekOffset(logicalDate, logicalYearStart, firstDayOfWeek);
},

// Used by getWeek() / getFiscalWeek()
_stackCount:0,
_getWeekOffset : function (date, startDate, firstDayOfWeek) {
    
    var dayDiff = Math.round((date - startDate)/86400000);

    // firstDayOfWeek - used for calendar type views.
    // If weeks explicitly start on (say) a monday but the first of the month
    // falls on a tuesday, adjust the week# to account for this offset (return the
    // #weeks from the monday of the week containing the start day).
    var extraDays = 0;
    if (firstDayOfWeek == null) {
        firstDayOfWeek = isc.DateChooser.getInstanceProperty("firstDayOfWeek");
    }
    if (firstDayOfWeek != null) {
        extraDays = startDate.getDay() - firstDayOfWeek;
        if (extraDays < 0) extraDays += 7;
    }
    // We want to use 1-based weeks (not zero based) - adding 1 and rounding causes issues, so
    // use Math.ceil() instead
    return Math.ceil((dayDiff + extraDays) / 7);
},

//>!BackCompat 2005.11.3
// -- Older depracated synonym of setNormalDisplayFormat
//>	@classMethod		Date.setFormatter()
//  Set the formatter for all date objects to the method name passed in.  After this call
//  all <code>theDate.toNormalDate()</code> calls will fall through to this formatter function to
//  return the date as a string.
//		@group	dateFormatting
//		@param	functionName	(string)	name of a date formatter method on this Date
//      @visibility internal
//<

setFormatter : function (formatter) {
    Date.setNormalDisplayFormat(formatter);
},
//<!BackCompat

//>	@classMethod Date.setLocaleStringFormatter() (A)
// Set default the +link{Date.iscToLocaleString()} formatter for all date instances.
//
//		@param	format (DateDisplayFormat | function) new formatter for iscToLocaleString()
//		@group	dateFormatting
//      @visibility internal
//<

setLocaleStringFormatter : function (functionName) {
	if (isc.isA.Function(Date.prototype[functionName]) || isc.isA.Function(functionName))
        Date.prototype.localeStringFormatter = functionName;
},

// Localizing dayName / monthNames
//> @classAttr  Date.shortDayNames  (Array : null : IRWA)
// This property may be set to an array of names of days of the week. <br>
// For example:
// <pre>
// ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
// </pre>
// The appropriate day name will then be returned from +link{date.getShortDayName()}, and may
// be used whenever SmartClient components display day-names (for example in the
// +link{class:DateItem, DateItem class}).<br>
// Note: For US based applications the first item in the array should be the name for Sunday,
// then Monday, Tuesday, etc. For browsers with different locales this may vary.
// To determine the first day for some locale, you can run the following code:
// <pre>
//    alert(new Date(2000, 0, 2).getDay());
// </pre>
// You should see an alert with a number between zero and 6. This represents the numerical
// 'day' value for Sunday for your browser's locale, since Jan 2nd 2000 was a Sunday.
// Therefore if this code alerted the number 6, Sunday should appear last in your list
// of day-names, and Monday first.
// @group i18nMessages
// @visibility external
//<



//> @classAttr  Date.dayNames  (Array : null : IRWA)
// This property may be set to an array of names of days of the week. <br>
// For example:
// <pre>
// ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]
// </pre>
// The appropriate day name will then be returned from +link{date.getDayName()}, and may
// be used whenever SmartClient components display day-names (for example in the
// +link{class:DateItem, DateItem class}).<br>
// @group i18nMessages
// @visibility external
//<
dayNames: ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"],

//> @classAttr  Date.shortMonthNames  (Array : null : IRWA)
// This property may be set to an array of shortened month-names.<br>
// For example:
// <pre>
// ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
// </pre>
// The appropriate month name will then be returned from +link{date.getShortMonthName()},
// and may be used whenever SmartClient components display month-names (for example in the
// +link{class:DateItem, DateItem class}).
// @group i18nMessages
// @visibility external
//<


derivedShortMonthNameLength: 3,

//> @classAttr  Date.monthNames  (Array : null : IRWA)
// This property may be set to an array of names of months.<br>
// For example:
// <pre>
// ["January", "February", "March", "April", "May", "June", "July", 
//  "August", "September", "October", "November", "December"]
// </pre>
// The appropriate month name will then be returned from +link{date.getMonthName()},
// and may be used whenever SmartClient components display month-names (for example in the
// +link{class:DateItem, DateItem class}).
// @group i18nMessages
// @visibility external
//<
monthNames: ["January","February","March","April","May","June","July","August","September",
             "October","November","December"],

//>	@method		date.getShortMonthNames()	(A)
// Return an array of the short names of each month, suitable for us in a selection list, etc.
// If +link{Date.shortMonthNames} is specified, this list will be used. Otherwise the value
// will be derived from the native browser date formatters.
//		@group	dateFormatting
//
//      @param  length  (int)    Number of characters for each day (Defaults to 3, can't be
//                                  longer than 3)
//		@return		(string[])	array of short month names
//<
getShortMonthNames : function (length) {
    var rawNames = Date.shortMonthNames;

    
    
    // NOOP starts - this block of code will never run, because shortMonthNames will alway be set
    if (rawNames == null) rawNames = Date._derivedShortMonthNames;
    if (rawNames == null) {
        var list = Date._derivedShortMonthNames = [];
        for (var i = 0; i < 12; i++) {
            // Changed the day in this synthetic date to 2 in order to derive the
            // correct month in timezones that are ahead of GMT (if you convert
            // midnight on the first of a month to UTC in such timezones, you
            // get the previous month...)
            var date = Date.createLogicalDate(2000,i,2);
            // have deriveShortMonthNames() return the shortened strings according to the
            // internal default (3 chars)
            list[i] = date.deriveShortMonthName(Date.derivedShortMonthNameLength);
        }
        rawNames = Date._derivedShortMonthNames;
    }
    // NOOP ends
    
    var names = [];
    for (var i =0; i< 12; i++) {
        if (!length) {
            // zero or null length - return the full rawName - we used to default to 3 chars,
            // but that's a bit pointless, since the rawNames are already *short*MonthNames, 
            // but they may not be 3 chars long, or of *any* fixed length
            names[i] = rawNames[i];
        } else {
            names[i] = rawNames[i].substring(0,length);
        }
    }
    return names;
},

//>	@method		date.getMonthNames()	(A)
// Return an array of the full names of each month, suitable for us in a selection list, etc.
// If +link{Date.monthNames} is specified, this list will be used. Otherwise the value
// will be derived from the native browser date formatters.  Note, if we have to derive names 
// from the native browser date string, the day names may be in an abbreviated form, like the 
// result of calling +link{getShortMonthNames()} - we have no control over this, we have to work
// with whatever the browser returns, which may vary by browser as well as locale.  If a 
// consistent and correct set of day names is important in your application, ensure that 
// <code>Date.monthNames</code> is set.
// @group	dateFormatting
// @return		(string[])	array of month names
//<
getMonthNames : function () {
    var rawNames = Date.monthNames;
    if (rawNames == null) rawNames = Date._derivedMonthNames;
    if (rawNames == null) {
        var list = Date._derivedMonthNames = [];
        for (var i = 0; i < 12; i++) {
            // Changed the day in this synthetic date to 2 in order to derive the
            // correct month in timezones that are ahead of GMT (if you convert
            // midnight on the first of a month to UTC in such timezones, you
            // get the previous month...)
            var date = Date.createLogicalDate(2000,i,2);
            list[i] = date.deriveMonthName();
        }
        rawNames = Date._derivedMonthNames;
    }
    return rawNames;
},

//>	@method		date.getShortDayNames()	(A)
// Return an array of the short names of each day, suitable for us in a selection list, etc.
// Day names will be picked up from +link{Date.shortDayNames} if specified - otherwise derived
// from the native browser date string.
//		@group	dateFormatting
//
//      @param  length  (int)    Number of characters for each day (Defaults to 3, can't be
//                                  longer than 3)
//		@return		(string[])	array of short day names
//<
getShortDayNames : function (length) {
    length = length || 3;
        var rawNames = Date.shortDayNames;
    if (rawNames == null) rawNames = Date._derivedShortDayNames;
    if (rawNames == null) {
        Date._derivedShortDayNames = [];
        var dateObj = new Date();
        dateObj.setDate(1);
        if (dateObj.getDay() > 0) dateObj.setDate(dateObj.getDate() + (7-dateObj.getDay()));
        var startDate = dateObj.getDate();
        for (var i = 0; i < 7; i++) {
            dateObj.setDate(startDate + i);
            Date._derivedShortDayNames[i] = dateObj.deriveShortDayName();
        }
        rawNames = Date._derivedShortDayNames;
    }
    var names = [];
    for (var i = 0; i < 7; i++) {
        names[i] = rawNames[i].substring(0,length);
    }
    return names;
},

//>	@method		date.getDayNames()	(A)
// Return an array of the full names of each day, suitable for us in a selection list, etc.
// Day names will be picked up from +link{Date.dayNames} if specified - otherwise derived
// from the native browser date string.  Note, if we have to derive names from the native 
// browser date string, the day names may be in an abbreviated form, like the result of 
// calling +link{getShortDayNames()} - we have no control over this, we have to work with
// whatever the browser returns, which may vary by browser as well as locale.  If a consistent
// and correct set of day names is important in your application, ensure that 
// <code>Date.dayNames</code> is set.
// @group	dateFormatting
// @return	(string[])	array of day names
//<
getDayNames : function () {
    var rawNames = Date.dayNames;
    if (rawNames == null) rawNames = Date._derivedDayNames;
    if (rawNames == null) {
        Date._derivedDayNames = [];
        var dateObj = new Date();
        dateObj.setDate(1);
        if (dateObj.getDay() > 0) dateObj.setDate(dateObj.getDate() + (7-dateObj.getDay()));
        var startDate = dateObj.getDate();
        for (var i = 0; i < 7; i++) {
            dateObj.setDate(startDate + i);
            Date._derivedShortDayNames[i] = dateObj.deriveDayName();
        }
        rawNames = Date._derivedDayNames;
    }
    var names = [];
    for (var i = 0; i < 7; i++) {
        names[i] = rawNames[i];
    }
    return names;
},

//> @classAttr Date.weekendDays (Array of int : [0, 6] : IR)
// Days that are considered "weekend" days.   Values should be the integers returned by the
// JavaScript built-in Date.getDay(), eg, 0 is Sunday and 6 is Saturday.  Override to
// accommodate different workweeks such as Saudi Arabia (Saturday -> Wednesday) or Israel
// (Sunday -> Thursday).
//
// @visibility external
//<

//> @classMethod Date.setWeekendDays()
// Sets the days that are considered +link{Date.weekendDays, weekend days}.  The parameter 
// should be array of the integers returned by the JavaScript built-in Date.getDay(), eg, 0 is 
// Sunday and 6 is Saturday.  Override to accommodate different workweeks such as Saudi Arabia 
// (Saturday -> Wednesday) or Israel (Sunday -> Thursday).
//
// @param weekendDays (Array of Integer) the array of day-numbers to assign as weekend days
// @visibility external
//<
setWeekendDays : function (weekendDays) {
    Date.weekendDays = weekendDays;
},

//> @classMethod Date.getWeekendDays()
// Return an array of days that are considered "weekend" days. Values will be the integers
// returned by the JavaScript built-in Date.getDay(), eg, 0 is Sunday and 6 is Saturday.
// Override +link{date.weekendDays} to accommodate different workweeks such as Saudi Arabia
// (Saturday -> Wednesday) or  Israel (Sunday -> Thursday).
// @group dateFormatting
//
// @return (Array of integer) array of weekend days
// @visibility external
//<
getWeekendDays : function () {
    var daysArr = Date.weekendDays;
    if (daysArr == null) daysArr = Date._derivedWeekendDays;
    if (daysArr == null) {
        daysArr = Date._derivedWeekendDays = [0, 6];
    }
    return daysArr;
},

getFormattedDateRangeString : function (fromDate, toDate) {
    if (fromDate != null && !isc.isA.Date(fromDate)) {
        fromDate = null;
    }
    if (toDate != null && !isc.isA.Date(toDate)) {
        toDate = null;
    }
    var fromMonth = fromDate ? fromDate.getMonth() : null,
        fromMonthName = fromDate ? fromDate.getShortMonthName() : null,
        fromYear = fromDate ? fromDate.getFullYear() : null,
        fromDay = fromDate ? fromDate.getDate() : null,
        toMonth = toDate ? toDate.getMonth() : null,
        toMonthName = toDate ? toDate.getShortMonthName() : null,
        toYear = toDate ? toDate.getFullYear() : null,
        toDay = toDate ? toDate.getDate() : null,
        result = ""
    ;

    if (fromDate && toDate) {
        if (fromYear == toYear) {
            // dates are in the same year - check the months
            if (fromMonth == toMonth) {
                // dates are in the same month - check the day-numbers
                if (fromDay == toDay) {
                    // dates are the same - use just the one date
                    result = fromMonthName + " " + fromDate.getDate() + ", " + fromYear;
                } else {
                    // day-numbers are different, use "month start - end, year"
                    result = fromMonthName + " " + fromDate.getDate() + " - " +
                        toDate.getDate() + ", " + fromYear;
                }
            } else {
                // dates are in different months, use "month start - month end, year"
                result = fromMonthName + " " + fromDate.getDate() + " - " +
                    toMonthName + " " + toDate.getDate() + ", " + fromYear;
            }
        } else {
            // different years - use "month start year - month end year"
                result = fromMonthName + " " + fromDate.getDate() + ", " + fromYear + " - " +
                    toMonthName + " " + toDate.getDate() + ", " + toYear;
        }
    } else if (fromDate) {
        // only a fromDate provided use "month start - end, year"
        result = fromMonthName + " " + fromDate.getDate() + ", " + fromYear;
    } else if (toDate) {
        // only a toDate provided use "month start - end, year"
        result = toMonthName + " " + toDate.getDate() + ", " + toYear;
    }

    return result;
}

});

//
//	add methods to the Date.prototype for additional formatting options
//
isc.addMethods(Date.prototype, {

//>	@method		date.duplicate()	(A)
//      Copy the value of this date into a new Date() object for independent manipulation
//  @visibility external
//<
duplicate : function () {
    var newDate = new Date();
    newDate.setTime(this.getTime());
	newDate.logicalDate = this.logicalDate;
    newDate.logicalTime = this.logicalTime;
    newDate._fromRelativeDate = this._fromRelativeDate;
    newDate._relativeDateTimestamp = this._relativeDateTimestamp
    return newDate;
},

//>	@method		date.clearTimeFields()	(A)
//			Zero-out the time fields for a date.
//		@group	dateFormatting
//<
clearTimeFields : function () {
	this.setHours(0);
	this.setMinutes(0);
	this.setSeconds(0);
	this.setMilliseconds(0);
	return this;
},




// Determine the day name from this.toString()
deriveShortDayName : function (length) {
	var string = this.toString();
    if (length == null || length <=0 || length > 3) length = 3;
	return string.substring(0,length);
},

// Determine the day name from this.toString(), without a target length
deriveDayName : function () {
	var string = this.toString();
    var length = string.indexOf(" ");
	return string.substring(0,length);
},

//>	@method		date.getShortDayName()
// Return the abbreviated (up to 3 chars) day of week name for this date (Mon, Tue, etc).
// To modify the value returned by this method, set +link{Date.shortDayNames}
//
//		@group	dateFormatting
//      @param  length  (int)    Number of characters to return (Defaults to 3, can't be
//                                  longer than 3)
//		@return		(string)	Abbreviated day name
//      @visibility external
//<
getShortDayName : function () {
    return Date.getShortDayNames()[this.getDay()];
},

//>	@method		date.getDayName()
// Return the full day of week name for this date (Monday, Tuesday, etc).
// To modify the value returned by this method, set +link{Date.dayNames}
//
// @group	dateFormatting
// @return		(string)	Day name
// @visibility external
//<
getDayName : function () {
    return Date.getDayNames()[this.getDay()];
},

// deriveShortMonthNames() - figure out the names of months from the native browser
// date formatting methods.
deriveShortMonthName : function (length) {
    // Use this.toUTCString - to work around Opera's different toString format
	var string = this.toUTCString();
	var start = 8;  // The correct start point if we have a 2-digit day portion in the date

    var defaultLength = Date.derivedShortMonthNameLength;
    if (length == null || length < 0 || length > defaultLength) length = defaultLength;

    if (string.substring(6, 7) == ' ') {  // we have a single-digit day number - only IE
                                          // does this, the others put a leading 0 in
        start = 7;
    }
	return string.substring(start, (start+length));
},

deriveMonthName : function () {
    // Use this.toUTCString - to work around Opera's different toString format
	var string = this.toUTCString();
	var start = 8;  // The correct start point if we have a 2-digit day portion in the date
    if (string.substring(6, 7) == ' ') {  // we have a single-digit day number - only IE
                                          // does this, the others put a leading 0 in
        start = 7;
    }
    var length = string.indexOf(" ", start);
	return string.substring(start, (start+length));
},


//>	@method date.getShortMonthName()
// Return the abbreviated name of the month for this date (Jan, Feb, etc)
// To modify the value returned by this method,
// <smartclient>set +link{Date.shortMonthNames}</smartclient>
// <smartgwt>use {@link com.smartgwt.client.util.DateUtil#setShortMonthNames()}</smartgwt>.
// @param length (int) Number of characters to return (Defaults to 3, can't be longer than 3)
// @return (string) Abbreviated month name (3 character string)
// @group dateFormatting
// @visibility external
//<
getShortMonthName : function (length) {
    return Date.getShortMonthNames(length)[this.getMonth()];
},

//>	@method		date.getMonthName()
// Return the full name of the month for this date (January, February, etc)
// To modify the value returned by this method,
// <smartclient>set +link{Date.shortMonthNames}</smartclient>
// <smartgwt>use {@link com.smartgwt.client.util.DateUtil#setMonthNames()}</smartgwt>.
// @group	dateFormatting
// @return		(string)	Month name
// @visibility external
//<
getMonthName : function () {
    return Date.getMonthNames()[this.getMonth()];
},

//>	@method		date.getShortYear()
//      Return a 2 digit year for this date.
//	@group	dateFormatting
//	@return		(string)	year number, padded to 2 characters
//  @visibility external
//<
getShortYear : function () {
	var year = this.getFullYear();
	return (year % 100).stringify(2);
},

//>	@method date.getWeek()
// Returns an integer containing the week number
// @group dateFormatting
// @return (int) week number, starting with 1
// @visibility external
//<
getWeek : function (firstDayOfWeek) {
    var logicalDate = this;

    // Normalize to a logical date, and compare with the logical
    // first day of the year - this will get rid of any oddities around time of day
    // and custom timezones etc (any datetime within the logical day will round to the
    // same logicalDate object)
    if (!this.logicalDate) {
        logicalDate = Date.getLogicalDateOnly(this);
    }
    var yearStart = Date.createLogicalDate(this.getFullYear(),0,1);

    return Date._getWeekOffset(logicalDate, yearStart, firstDayOfWeek);
},

getFiscalCalendar : function () {
    return Date.getFiscalCalendar();
},

//>	@method date.getFiscalYear()
// Returns the +link{FiscalYear} object appropriate for the the current date, according to the
// +link{FiscalCalendar, FiscalCalendar}.
// @return (FiscalYear) the fiscal year object
// @visibility external
//<
getFiscalYear : function (fiscalCalendar) {
    return Date.getFiscalYear(this, fiscalCalendar);
},

//>	@method date.getFiscalWeek()
// Returns the fiscal week number of the current date, according to the global
// +link{Date.setFiscalCalendar, FiscalCalendar}.
// @param [fiscalCalendar] (FiscalCalendar) the object representing the starts of fiscal years
// @return (int) the week number, offset from the start of the fiscal period
// @visibility external
//<
getFiscalWeek : function (fiscalCalendar, firstDayOfWeek) {
    return Date.getFiscalWeek(this, fiscalCalendar, firstDayOfWeek);
},

//
// Date Formatters (toNormalDate(), toShortDate(), etc.)
//
// Date formatters are applied to date objects to convert them into strings for display.
// Dates are intended to be localizable.
// For localization, a developer would typically set either the shortDateFormatter or
// normalDateFormatter, as well as the inputDateFormat, and then call
// "toNormalDate()" / "toShortDate()" and "parseInput()" as normal.

//>	@method		date.toDateStamp()
//			Return this date in the format (UTC timezone):
//				<code><i>YYYYMMDD</i>T<i>HHMMSS</i>[Z]</code>
//		@group	dateFormatting
//		@return					(string)	formatted date string
//  @visibility external
//<
toDateStamp : function () {
	return	this.getUTCFullYear()
		  + (this.getUTCMonth()+1).stringify()
		  + this.getUTCDate().stringify()
		  + "T"
		  +	this.getUTCHours().stringify()
		  + this.getUTCMinutes().stringify()
		  + this.getUTCSeconds().stringify()
		  + "Z";
},

//>	@method date.toNormalDate()
// Returns the date as a formatted string using the format set up via the
// <code>setNormalDisplayFormat()</code> method. Note that the default formatter for this
// method is <code>"toLocaleString"</code>.
// @group   dateFormatting
// @param format (DateDisplayFormat) Optional Format for the date returned
// @return  (string) formatted date string
// @visibility external
//<
// This method is used by our data components such as ListGrid to display long format dates.
// @param useCustomTimezone (boolean) If true, format the date using the timezone
//  setDefaultDisplayTimezone() rather than the native browser locale.
//  Defaults to true.
//  Has no effect if no custom timezone applied
//  * Note that the native browser formatters including toLocaleString won't respect the
//    developer specified timezone of course. We could workaround this (create a new date, shift
//    by offset between specified timezone and native timezone, and call the native formatter on that)
//    but we currently don't.
toNormalDate : function (formatter, useCustomTimezone) {

    
    if (!formatter) formatter = this.formatter;
    // fire the formatter in the scope of this date, so date is available as 'this'

    if (isc.isA.Function(formatter)) {
        return formatter.apply(this, [useCustomTimezone])
    } else if (this[formatter]) {
        return this[formatter](useCustomTimezone);
    } else if (isc.isA.String(formatter)) {
        return isc.DateUtil.format(this, formatter);  // WWW - what about useCustomTimezone??
    }
},

toNormalDateTime : function (formatter, useCustomTimezone) {
    return this.toNormalDatetime(formatter, useCustomTimezone);
},

//>	@method date.toNormalDatetime()
// Returns the datetime as a formatted string using the format set up via the
// <code>setNormalDatetimeDisplayFormat()</code> method.
// @group   dateFormatting
// @param format (DateDisplayFormat) Optional Format for the date returned
// @param [useCustomTimezone] (Boolean) If a custom timezone has been set via
//   Time.setDefaultDisplayTimezone(), by default date formatters will respect this timezone.
//   To suppress this behavior, this parameter should be set to false.
// @return  (string) formatted date string
// @visibility external
//<
toNormalDatetime : function (formatter, useCustomTimezone) {
    if (!formatter) formatter = this.datetimeFormatter;
    return this.toNormalDate(formatter, useCustomTimezone);
},

//>	@method date.toShortDate()
// Returns the date as a formatted string using the format set up via the
// <code>setShortDisplayFormat()</code> method.
// @group   dateFormatting
// @param format (DateDisplayFormat) Optional Format for the date returned
// @param [useCustomTimezone] (Boolean) If a custom timezone has been set via
//   Time.setDefaultDisplayTimezone(), by default date formatters will respect this timezone.
//   to suppress this behavior, this parameter should be set to false.
// @return  (string) formatted date string
// @visibility external
//<

toShortDate : function (formatter, useCustomTimezone) {
    if (!formatter) formatter = this._shortFormat;
    if (isc.isA.Function(formatter)) return formatter.apply(this, [useCustomTimezone]);
    else if (isc.isA.Function(this[formatter])) {
        if (formatter == "toSerializeableDate") return this[formatter]();
        return this[formatter](useCustomTimezone);
    } else if (isc.isA.String(formatter)) {
        return isc.DateUtil.format(this, formatter);  // WWW - what about useCustomTimezone??
    }

    isc.logWarn("Date.toShortDate() specified formatter not understood:" + formatter);
    return this.toUSShortDate();

},


//>	@method date.toShortDateTime()
// Returns the datetime as a formatted string using the format set up via the
// <code>setShortDatetimeDisplayFormat()</code> method.
// @group   dateFormatting
// @param format (DateDisplayFormat) Optional Format for the date returned
// @param [useCustomTimezone] (Boolean) If a custom timezone has been set via
//   Time.setDefaultDisplayTimezone(), by default date formatters will respect this timezone.
//   to suppress this behavior, this parameter should be set to false.
// @return  (string) formatted date string
// @visibility external
//<



toShortDateTime : function (formatter, useCustomTimezone) {
    return this.toShortDatetime(formatter,useCustomTimezone);
},

toShortDatetime : function (formatter, useCustomTimezone) {
    if (!formatter) formatter = this._shortDatetimeFormat;
    return this.toShortDate(formatter, useCustomTimezone);
},


//>	@method date.setDefaultDateSeparator
// Sets a new default separator that will be used when formatting dates. By default, this
// is a forward slash character: "/"
// @group dateFormatting
// @param separator (string) separator to use in dates
// @visibility external
//<
setDefaultDateSeparator : function (separator) {
    this._shortDateTemplate = [,,,,separator,,,,,separator,,,,null];
    this._separator = separator;
},

//>	@method date.getDefaultDateSeparator
// gets the default date separator string
// @group dateFormatting
// @return(string) the default date separator
// @visibility external
//<
getDefaultDateSeperator : function (separator) {
    if (this._separator) return this._separator;
    else return "/";
},


_shortDateTemplate:[,,,,"/",,,,,"/",,,,null],
_$MDY:"MDY",
_$DMY:"DMY",
_$YMD:"YMD",
_$MDY:"MDY",

// _applyTimezoneOffset()
// shift a date by some arbitrary number of hours/minutes
// third parameter allows you to specify the starting date time [result of date.getTime()]
// to offset from
_applyTimezoneOffset : function (hourOffset, minuteOffset, dateTime) {
    if (dateTime == null) dateTime = this.getTime();
    if (isc.isA.Number(hourOffset)) dateTime += (3600000 * hourOffset);
    if (isc.isA.Number(minuteOffset)) dateTime += (60000 * minuteOffset);
    this.setTime(dateTime);
},

// _getTimezoneOffsetDate()
// This is a helper method - given a date with a certain UTC time, apply an explicit timezone
// offset to return a date where the UTC time is offset by the specified hours/minutes.
// We'll use this when formatting dates for display in arbitrary local times [so we can't just
// use the native browser local timezone methods like getHours()]

_getTimezoneOffsetDate : function (hourOffset, minuteOffset) {
    var offsetDate = Date._timezoneOffsetDate;
    if (offsetDate == null) offsetDate = Date._timezoneOffsetDate = new Date();

    offsetDate._applyTimezoneOffset(hourOffset, minuteOffset, this.getTime());
    return offsetDate;

},


// _toShortDate()
// Internal method to give us a shortDate - either DD/MM/YYYY, MM/DD/YYYY or YYYY/MM/DD.
// this will be passed "MDY" / "DYM" / etc. as a format parameter.
// useCustomTimezone parameter: use the hour and minute offset specified by
// Time.setDefaultDisplayTimezone() rather than the native browser local timezone
_$zero:"0",
_toShortDate : function (format, useCustomTimezone) {

    // if this is a "logical date", don't use the developer-specified custom timezone when
    // formatting. Typically handled by DBC's passing in the useCustomTimezone parameter, but
    // we can also check for the logical date marker
    
    if (useCustomTimezone == null) {
        useCustomTimezone = !this.logicalDate;
    }
    var template = this._shortDateTemplate,
        month,day,year;

    // Browser native locale timezone
    if (!useCustomTimezone || !isc.Time._customTimezone) {
        month = this.getMonth()+1;
        day = this.getDate();
        year = this.getFullYear();

    // Developer specified custom timezone
    } else {
        var offsetDate = this._getTimezoneOffsetDate(
                            isc.Time.getUTCHoursDisplayOffset(this),
                            isc.Time.getUTCMinutesDisplayOffset(this)
                         );

        month = offsetDate.getUTCMonth() + 1;
        day = offsetDate.getUTCDate();
        year = offsetDate.getUTCFullYear();
    }

    var monthIndex, dayIndex, yearIndex;

    if (format == this._$MDY) {
        monthIndex = 0;
        dayIndex = 5;
        yearIndex = 10;
    } else if (format == this._$DMY) {
        dayIndex = 0;
        monthIndex = 5;
        yearIndex = 10;
    } else if (format == this._$YMD) {
        yearIndex = 0;
        monthIndex = 5;
        dayIndex = 10
    // Unlikely - don't bother avoiding string alloc's for every one of these options
    } else {
        dayIndex = format.indexOf("D")*5;
        yearIndex = format.indexOf("Y")*5;
        monthIndex = format.indexOf("M")*5;
    }

    // Note: each number has 4 slots so it can accommodate a full year
    // For month/day - if we need a leading zero, fill the first slot with it
    // Use fillNumber to fill 3 slots even though we have a max of 2 digits to ensure
    // the last slot gets cleared out if it was populated by a year already.
    template[dayIndex] = day < 10 ? this._$zero : null
    isc._fillNumber(template, day, dayIndex+1, 3);

    template[monthIndex] = month < 10 ? this._$zero : null
    isc._fillNumber(template, month, monthIndex+1, 3);

    template[yearIndex + 1] = null;
    isc._fillNumber(template, year, yearIndex, 4);
    return template.join(isc.emptyString);
},

//>	@method		date.toUSShortDate()
//			Return this date in the format: <code>MM/DD/YYYY</code>
//		@group	dateFormatting
//		@return					(string)	formatted date string
//  @visibility external
//<
toUSShortDate : function (useCustomTimezone) {
    return this._toShortDate(this._$MDY, useCustomTimezone);
},

// _toShortTime - returns the time portion of the date in HH:MM
_timeTemplate:[null,null],
_toShortTime : function (useCustomTimezone) {
    
    return isc.Time.toShortTime(this, "toShortPadded24HourTime");
},

//>	@method		date.toUSShortDateTime()
//  Return this date in the format: <code>MM/DD/YYYY HH:MM</code>
//
//		@group	dateFormatting
//		@return					(string)	formatted date string
//  @visibility external
//<
toUSShortDateTime : function (useCustomTimezone) {
    return this.toUSShortDatetime(useCustomTimezone);
},


toUSShortDatetime : function (useCustomTimezone) {
    return this.toUSShortDate(useCustomTimezone) + " " + this._toShortTime(useCustomTimezone);
},


//>	@method		date.toEuropeanShortDate()
//			Return this date in the format: <code>DD/MM/YYYY</code>
//		@group	dateFormatting
//		@return					(string)	formatted date string
//      @visibility external
//<
toEuropeanShortDate : function (useCustomTimezone) {
    return this._toShortDate(this._$DMY, useCustomTimezone);
},

//>	@method		date.toEuropeanShortDateTime()
// Return this date in the format: <code>DD/MM/YYYY HH:MM</code>.
//		@group	dateFormatting
//		@return					(string)	formatted date string
//      @visibility external
//<
toEuropeanShortDateTime : function (useCustomTimezone) {
    return this.toEuropeanShortDatetime();
},


toEuropeanShortDatetime : function (useCustomTimezone) {
    return this.toEuropeanShortDate(useCustomTimezone) + " " +
            this._toShortTime(useCustomTimezone);
},

//> @method date.toJapanShortDate()
// Return the date in this format: <code>YYYY/MM/DD</code>
// @group dateFormatting
// @return (string) formatted date string
// @visibility external
//<
toJapanShortDate : function (useCustomTimezone) {
    return this._toShortDate(this._$YMD, useCustomTimezone);
},

//>	@method		date.toJapanShortDateTime()
//			Return this date in the format: <code>YYYY/MM/DD HH:MM:SS</code>
//		@group	dateFormatting
//		@return					(string)	formatted date string
//      @visibility external
//<
toJapanShortDateTime : function (useCustomTimezone) {
    return this.toJapanShortDatetime(useCustomTimezone);
},


toJapanShortDatetime : function (useCustomTimezone) {
    return this.toJapanShortDate(useCustomTimezone) + " " + this._toShortTime(useCustomTimezone);
},

//>	@method		date._serialize()	(A)
//			Serialize this date to a string in a format that can be reinstantiated back into a date.
//				<code>$$DATE$$:<i>YYYY</i>-<i>MM</i>-<i>DD</i></code>
//		@group	dateFormatting
//		@return					(string)	formatted date string
//      @visibility internal
//<
_serialize : function () {
    if (isc.Comm._legacyJSMode) {
        // legacy mode: add $$DATE$$ that only our server-side JS parser understands
        return isc.SB.concat('"' + this.toDBDate(), '"');
    } else {
        // any other caller: return code that would reconstruct the same Date in a JS
        // interpreter
        
        return isc.SB.concat("new Date(", this.getTime(), ")");
    }
},



//> @groupDef dateFormatAndStorage
// The SmartClient system has the following features for handling Date and Time type values
// within DataSources and databound components.
// <P>
// DataSources and databound components may define fields of type <code>date</code>,
// <code>time</code>, or <code>datetime</code>.
// <P>
// <h3>"date" handling</h3>
// <P>
// Fields of type +link{type:FieldType,date} are considered to be logical Dates with no time
// value, such as a holiday or birthday.  In the browser, values for "date" fields are stored
// as Date objects, but when formatted for display to the user, they are typically displayed
// without any time information.
// <P>
// When using the SmartClient server framework, "date" values are automatically transmitted
// with year, month and day preserved and time value ignored.
// <P>
// When sent or received in XML or JSON, date field values should be serialized in the
// +externalLink{http://www.w3.org/TR/xmlschema-2/#dateTime,XML Schema date format} - 
// <code>YYYY-MM-DD</code> - are expected to be received in the same format.  Any time value
// present for a "date" field is ignored.
// <smartclient>
// <P>
// The +link{DateUtil.createLogicalDate()} method may be used to create a new Date object to 
// represent a logical date value on the browser.
// </smartclient>
// <smartgwt>
// <P>
// The DateUtil.createLogicalDate() method may be used to create a new Date object to represent
// a logical date value on the browser.
// </smartgwt>
// <P>
// System wide formatting for dates may be controlled via the
// +link{Date.setNormalDisplayFormat()} and +link{Date.setShortDisplayFormat()} methods.
// <P>
// <h3>"datetime" handling</h3>
// <P>
// Fields of type +link{type:FieldType,datetime} are dates with full time information.
// In the browser, values for datetime fields are stored as Date objects.
// <P>
// When using the SmartClient server framework, "datetime" values are automatically transmitted
// such that the resulting Date object has the same GMT/UTC timestamp (milliseconds since
// epoch).
// <P>
// When sent or received in XML or JSON, datetime field values should be serialized out as full
// datetimes using the standard
// +externalLink{http://www.w3.org/TR/xmlschema-2/#dateTime,XML Schema datetime format}
// (EG:<code>2006-01-10T12:22:04-04:00</code>).  If no timezone offset is supplied, the value
// is assumed to be GMT/UTC.
// <P>
// System wide formatting for datetimes may be controlled via the
// +link{Date.setShortDatetimeDisplayFormat()} method.  Datetimes will be displayed to the user
// in browser local time by default (see also timezone notes below).
// <P>
// <h3>"time" handling</h3>
// <P>
// Fields of type +link{type:FieldType,time} are time values in the absence of a day, such as
// the beginning of the workday (9:00).  In the browser, values for "time" fields are stored as
// Date objects with the time in browser local time.  The date information has no meaning and
// only the time information is displayed to the user.
// <P>
// Time formatting is handled by the +link{Time} class APIs.
// <br>
// When using the SmartClient server framework, "time" values are automatically transmitted
// such that the resulting Date object has the same hour, minute and second values in local
// time, and year/month/day is ignored.
// <P>
// When sent or received in XML or JSON, date field values should be serialized as hours,
// minutes and seconds using the standard
// +externalLink{http://www.w3.org/TR/xmlschema-2/#dateTime,XML Schema time format} -
// <code>"22:01:45"</code>.  Timezone is not relevant and should be omitted.
// <smartclient>
// <P>
// The +link{Date.createLogicalTime()} method may be used to create a new Date object to represent
// a logical time value on the browser.
// </smartclient>
// <smartgwt>
// <P>
// The DateUtil.createLogicalTime() method may be used to create a new Date object to represent
// a logical time value on the browser.
// </smartgwt>
// <P>
// <h3>Timezone settings and Daylight Savings Time</h3>
// <P>
// By default, "datetime" values will be shown to the user in browser local time, as derived
// from the native browser locale.  Developers may modify this behavior by specifying an
// explicit display timezone via +link{Time.setDefaultDisplayTimezone()}.
// <P>
// Note that depending on the specific date being displayed, a Daylight Savings Time offset may
// also be applied based on the browser locale.  To disable this behavior set
// +link{isc.Time.adjustForDST} to false.
// <P>
// If a custom timezone is specified, it will be respected by all +link{TimeDisplayFormat}s, and
// by the standard short +link{DateDisplayFormat}s when formatting dates representing datetime
// type values. However native JavaScript Date formatters,
// including <code>toLocaleString()</code> will not respect the specified timezone. Developers
// specifying a custom timezone may therefore wish to modify the +link{Date.setNormalDisplayFormat()}
// to avoid using a native JS Date formatter function.
// <P>
// Note that in addition to the system-wide date, datetime and time-formatting settings described
// above, databound components also support applying custom display formats for date values.
// Typically this can be achieved via a custom <code>dateFormatter</code> or
// <code>timeFormatter</code> at the field level (see +link{dataSourceField.dateFormatter},
// +link{dataSourceField.timeFormatter} and for example +link{listGridField.dateFormatter}).
// Date formatting may also be configured at the component level by setting the
// <code>dateFormatter</code>, <code>datetimeFormatter</code> and <code>timeFormatter</code>
// attributes (See for example +link{listGrid.dateFormatter}, +link{listGrid.timeFormatter},
// and +link{listGrid.datetimeFormatter}).
// <P>
// <h3>Troubleshooting Date and Time values</h3>
// <P>
// Date and time storage and timezones can be confusing, and Isomorphic receives a steady
// stream of false bug reports from users that are incorrectly analyzing logs and diagnostics.
// Please consider the following points when troubleshooting issues such as date values
// changing to a different day, or datetime value shifting when saved and reloaded:
// <P>
// <h4>1. compare values for "datetime" fields via date.getTime()</h4>
// <P>
// Whenever you use Date.toString() (client or server-side) the value you get is based on the
// server or browser timezone.
// <P>
// Perhaps you are troubleshooting an issue with datetimes and you try to log the value of a
// Date like this:
// <pre>
//    Date someDate = &lt;<i>some expression</i>&gt;;
//    log("date value is: " + someDate);
// </pre>
// Code like this will show the datetime value in the server's timezone if executed
// server-side, and in the client's timezone if executed client-side.  If they are in different
// timezones, the hour or day will be different, <b>whereas the actual datetime value -
// milliseconds since epoch as retrieved by Date.getTime() - is the same</b>.  To correctly
// compare two datetime values, compare the result of getTime().
// <P>
// <h4>2. "date" and "time" field values <b>cannot</b> be compared via getTime()</h4>
// <P>
// This is the inverse situation as for "datetime" values.  As explained above, "date" values
// have no meaningful values for time fields (hours/minutes/seconds) and "time" values have no
// meaningful values for date fields (month/day/year).  Here, the result of Date.getTime() is
// not meaningful, and values should be compared via getHours(), getMonth() et al.
// <P>
// <h4>3. the display timezone does not affect Date.getHours(), Date.getDay() et al</h4>
// <P>
// If you've called setDefaultDisplayTimezone() to cause all datetime values to be rendered in
// a particular timezone, this does not affect the return values of Date.getHours(), which will
// still return values for the browser's current timezone.  Hence it is not a bug if you have a
// "datetime" value which is displaying as 4am, but getHours() returns 10 or some other
// number.  This just reflects the timezone offset between the timezone passed to
// setDefaultDisplayTimezone() and the browser's local timezone.
// <P>
// <h4>4. use correct DataSourceField types and use the matching FormItem type</h4>
// <P>
// If you declare a field as type "date" but values you provide actually contain specific
// hours, minutes and seconds, these will not be preserved.  The system will discard or reset
// the hours, minutes and seconds in the course of serialization or editing.  Likewise
// if you declare a field as type "time" but actually provide values where year, month and day
// have meaning, these values will be dropped.
// <P>
// Similarly, DateItem expects values for "date" fields, TimeItem expects values for "time"
// fields, and DateTimeItem expects values for "datetime" fields.  Providing the wrong type of
// value to a control, such as providing a value from a "datetime" field to a DateItem, will
// have unspecified results.
// <P>
// <smartclient>
// If you want to take the date and time aspects of a "datetime" value and edit them in separate
// FormItems, use +link{Date.getLogicalDateOnly()} and +link{Date.getLogicalTimeOnly()} to
// split a datetime value into date and time values, and use
// +link{Date.combineLogicalDateAndTime()} to re-combine such values. Otherwise it is very
// easy to make mistakes related to timezone offsets.
// </smartclient>
// <smartgwt>
// If you want to take the date and time aspects of a "datetime" value and edit them in separate
// FormItems, use
// <code>getLogicalDateOnly()</code> and <code>DateUtil.getLogicalTimeOnly()</code> to
// split a datetime value into date and time values, and use
// <code>DateUtil.combineLogicalDateAndTime()</code> to re-combine
// such values. Otherwise it is very
// easy to make mistakes related to timezone offsets.
// </smartgwt>
// <P>
// <h4>5. check data at every phase when troubleshooting</h4>
// <P>
// If you're having a problem with round-tripping "datetime" values or "date" values shifting
// to another day, you need to isolate the problem to a specific layer.  Bearing in mind the
// techniques above for comparing values, you potentially need to look at any/all of the
// following:
// <ol>
// <li> what value do I have on the server-side before it's sent to the client?
// <li> what value is being transmitted to the client? (use the RPC Tab of the Developer
// Console to see the actual data sent)
// <ul>
// <li> was the value shifted to a different time/date by my serialization approach?
// <li> does it have the right format? (see above for correct JSON/XML formats)
// </ul>
// <li> what value do I have on the client before it gets to any widgets (eg, do a direct call
// to +link{DataSource.fetchData()} and inspect the data in the callback)
// <li> what value does the FormItem or other editing widget report before saving is attempted?
// <li> what value is reported right before the value is serialized for transmission to the
// server (+link{DataSource.transformRequest()} is a good place to check)
// <li> what value is being transmitted to the server? (use the RPC tab - same concerns as for
// server-to-client transmission above)
// <li> what value does the server have after de-serialization, before saving to the database
// or other permanent storage?
// <li> what value is sent to the database or permanent storage?  If generating SQL or another
// similar query language, does the value in the SQL statement include an explicit timezone?
// If not, how will the database interpret it?
// </ol>
//
// @title Date and Time Format and Storage
// @treeLocation Concepts
// @visibility external
//<


_xmlSerialize : function (name, type, namespace, prefix) {
	return isc.Comm._xmlValue(name, this.toSchemaDate(null, isc.Comm._trimMillis),
                              type || (this.logicalDate ? "date" :
                                        (this.logicalTime &&
                                        !isc.DataSource.serializeTimeAsDatetime ? "time" : "datetime")),
                              namespace, prefix);
},

// logicalType parameter - option to specify "date" vs "datetime" vs "time" which impacts
// how this date instance should be serialized out.
// Alternatively logicalDate / logicalTime attributes may be hung onto the date objet
// directly.
// Used by DataSources when serializing dates out
toSchemaDate : function (logicalType, trimMillis) {
    // logical date values have no meaningful time
    // Note that they also have "no meaningful timezone" - we display native browser locale time
    // to the user and when we serialize to send to the server we serialize in that same
    // local timezone.
    if ((logicalType == "date") || this.logicalDate) {
        return isc.SB.concat(
			this.getFullYear().stringify(4),
			"-",
			(this.getMonth() + 1).stringify(2), 	// getMonth() is zero-based
			"-",
			this.getDate().stringify(2)
        );
    };

    // logical times are serialized as truncated schema strings (HH:MM:SS) by default
    if ((!isc.DataSource || !isc.DataSource.serializeTimeAsDatetime) &&
        (logicalType == "time" || this.logicalTime))
    {
        var value = isc.SB.concat(
            this.getHours().stringify(2), ":",
            this.getMinutes().stringify(2), ":",
            this.getSeconds().stringify(2));
        if (trimMillis !== true) {
            value += "." + this.getUTCMilliseconds().stringify(3);
        };
        return value;
    }

    // represent date time values in UTC
    var value = isc.SB.concat(
        this.getUTCFullYear().stringify(4),
        "-",
        (this.getUTCMonth() + 1).stringify(2), 	// getMonth() is zero-based
        "-",
        this.getUTCDate().stringify(2),
        "T",
        this.getUTCHours().stringify(2),
        ":",
        this.getUTCMinutes().stringify(2),
        ":",
        this.getUTCSeconds().stringify(2));
    if (trimMillis !== true) {
        value += "." + this.getUTCMilliseconds().stringify(3);
    };
    return value;
},

//>	@method		date.toSerializeableDate()	(A)
// Return this date in 'serialized' format <code>YYYY-MM-DD HH:MM:SS</code>
// @group dateFormatting
// @return (String) formatted date string
// @visibility external
//<

toSerializeableDate : function (useCustomTimezone) {
    var output = isc.SB.create();
    output.append(
			this.getFullYear().stringify(4),
			"-",
			(this.getMonth() + 1).stringify(2), 	// getMonth() is zero-based
			"-",
			this.getDate().stringify(2)
    );
    
    output.append(isc.Comm.xmlSchemaMode ? "T" : " ",
                  isc.Time.toShortTime(this, "toPadded24HourTime"));
    return output.release(false);
},

//>	@method		date.toDBDate()	(A)
//			Return this date in the format the database can parse as a datetime:
//				<code>$$DATE$$:<i>YYYY-MM-DD HH:MM:SS</i></code>
//		@group	dateFormatting
//
//		@return					(string)	formatted date string
//  @visibility internal
//<
// Leave this internal for now
toDBDate : function () {
	return isc.StringBuffer.concat(
			"$$DATE$$:",
			this.toSerializeableDate()
			);
},


//>	@method		date.toDBDateTime()	(A)
//			Return this date in the format the database can parse as a dateTime:
//				<code>$$DATE$$:<i>YYYY-MM-DD HH:MM:SS</i></code>
//		@group	dateFormatting
//
//		@return					(string)	formatted date string
//      @visibility internal
//<

toDBDateTime : function () {    return this.toDBDate();       },

//>	@method		date.setFormatter()
//  Set the formatter for this date object to the method name passed in.  After this call
//  wherever appropriate SmartClient components will use this formatter function to return
//  the date as a string.
//		@group	dateFormatting
//		@param	functionName	(string)	name of a date formatter method on this Date
//      @visibility external
//      @deprecated As of SmartClient 5.5 use the static methods
//              +link{classMethod:Date.setNormalDisplayFormat} and
//              +link{classMethod:Date.setShortDisplayFormat} to set default formatters for all dates
//<
setFormatter : function (formatter) {
    this.setNormalDisplayFormat(formatter);
},

//>	@method	date.setLocaleStringFormatter() (A)
//			Set the <code>iscToLocaleString()</code> formatter for a specific date object.
//			After this call, all  <code>theDate.toLocaleString()</code>  calls will yield a string
//			 in this format.
//
//		@param	functionName	(string)	name of a dateFormatting function
//		@group	dateFormatting
//      @visibility internal
//      @deprecated As of SmartClient 5.5 use the static method
//                  +link{classMethod:Date.setLocaleStringFormatter} instead
//<

setLocaleStringFormatter : function (functionName) {
	if (isc.isA.Function(this[functionName]) || isc.isA.Function(functionName))
        this.localeStringFormatter = functionName;
},

// ------------------------Advanced Date Comparison -------------------------------------------
// (currently undocd)
isBeforeToday : function (dateObj) {
    var today = new Date(this.getFullYear(), this.getMonth(), this.getDate(), 0).getTime();
    if (dateObj.getTime() < today) return true;
    else return false;
},

isToday : function (dateObj) {
    if (this.getFullYear() == dateObj.getFullYear() && this.getMonth() == dateObj.getMonth()
        && this.getDate() == dateObj.getDate())
        return true;
    else return false;
},

isTomorrow : function (dateObj) {
    var tomorrowStart = new Date(this.getFullYear(), this.getMonth(), this.getDate() + 1, 0);
    var tomorrowEnd = new Date(this.getFullYear(), this.getMonth(), this.getDate() + 1, 23);
    var dateTime = dateObj.getTime();
    if (dateTime >= tomorrowStart.getTime() && dateTime <= tomorrowEnd.getTime()) {
        return true;
    } else {
        return false;
    }
},

isThisWeek : function (dateObj) {
    var weekStart = new Date(this.getFullYear(), this.getMonth(), this.getDate() - this.getDay(), 0);
    var weekEnd = new Date(this.getFullYear(), this.getMonth(), this.getDate() + (7 - this.getDay()), 23);
    var dateTime = dateObj.getTime();
     if (dateTime >= weekStart.getTime() && dateTime <= weekEnd.getTime()) {
        return true;
    } else {
        return false;
    }
},

isNextWeek : function (dateObj) {
    var weekStart = new Date(this.getFullYear(), this.getMonth(), (this.getDate() - this.getDay()) + 7, 0);
    var weekEnd = new Date(this.getFullYear(), this.getMonth(), (this.getDate() - this.getDay()) + 14, 23);
    var dateTime = dateObj.getTime();
     if (dateTime >= weekStart.getTime() && dateTime <= weekEnd.getTime()) {
        return true;
    } else {
        return false;
    }
},

isNextMonth : function (dateObj) {
    var monthStart = new Date(this.getFullYear(), this.getMonth());
    monthStart.setMonth(monthStart.getMonth() + 1);
    if (monthStart.getFullYear() == dateObj.getFullYear() && monthStart.getMonth() == dateObj.getMonth()) {
        return true;
    } else {
        return false;
    }
}

});


//>	@method		date.toBrowserString()
//  Native <code>date.toString()</code> provided by the browser for Date objects
//		@group	dateFormatting
//      @visibility internal
//      @deprecated As of SmartClient 5.5
//<
// Note that the default formatter varies by browser/platform so it's not that useful.
// This was exposed in 5.2 so we're keeping it around for back-compat only
Date.prototype.toBrowserString = Date.prototype.toString;

//>	@method		date.toBrowserLocaleString()    (A)
//  Synonym for <code>date.toLocaleString()</code> provided by the browser for Date objects
//		@group	dateFormatting
//      @visibility internal
//      @deprecated As of SmartClient 5.5
//<

Date.prototype.toBrowserLocaleString = Date.prototype.toLocaleString;

// default the global fiscal year to the start of the calendar year
Date.prototype.fiscalCalendar = { defaultMonth:0, defaultDate:1, fiscalYears: [] };

// set the standard formatter for the date prototype to the native browser string
//	so everything works as normal until it is overridden.
if (!Date.prototype.formatter) Date.setNormalDateDisplayFormat("toLocaleString");
if (!Date.prototype.datetimeFormatter) Date.setNormalDatetimeDisplayFormat("toLocaleString");

// set the standard toShortDate() formatter to US Short Date
if (!Date.prototype._shortFormat) Date.setShortDisplayFormat("toUSShortDate");
if (!Date.prototype._shortDatetimeFormat) Date.setShortDatetimeDisplayFormat("toUSShortDatetime");

//>	@method		date.iscToLocaleString()   (A)
// Customizeable toLocaleString() type method.
// This method is called when isc.iscToLocaleString(date) is called.
//
//		@group	dateFormatting
//		@return				(string)	formatted date string
//      @visibility internal
//<
// Leave this internal - we don't really expect this to be called directly or overridden by
// the developer

Date.prototype.iscToLocaleString = function () {
    var formatter = this.localeStringFormatter;
    if (isc.isA.Function(formatter)) return formatter.apply(this);
    else if (this[formatter]) return this[formatter]();
}

// By default have iscToLocaleString() call date.toLocaleString()
if (!Date.prototype.localeStringFormatter)
    Date.prototype.localeStringFormatter = "toLocaleString";


//>Safari12
isc.addMethods(Date, {
    // Simple substring matching for splitting up a date string to avoid using unsupported
    // string.match() method in early Safari
    // Note - somewhat flawed: we're assuming well never be handed a single digit month or day
    _splitDateViaSubstring : function (string, monthIndex, dayIndex, yearIndex) {

        // We know that year may be after month and/or day - allow 3 chars ("DD/") for each
        var yearCharIndex = yearIndex * 3,
            year = string.substring(yearCharIndex, yearCharIndex +4)
        ;

        // If we have a 2 or 3 char year, this affects the position of the day/month in the
        // string
        var yearLength = year.length;

        var monthCharIndex = 0,
            dayCharIndex = 0;
        if (monthIndex > dayIndex) monthCharIndex += 3;
        else dayCharIndex += 3;

        if (monthIndex > yearIndex) monthCharIndex += yearLength + 1;
        if (dayIndex > yearIndex) dayCharIndex += yearLength + 1;

        // Note: Month is zero based rather than 1 based.
        var month = string.substring(monthCharIndex, monthCharIndex + 2) -1;
        var day = string.substring(dayCharIndex, dayCharIndex +2);

        // Hour minute second are not expected to change orders
        var hourCharIndex = 7 + yearLength,
            hour = (string.substring(hourCharIndex,hourCharIndex + 2) || 0),
            minute = (string.substring(hourCharIndex + 3, hourCharIndex + 5) || 0),
            second = (string.substring(hourCharIndex + 6, hourCharIndex + 8) || 0);

        return[year,month,day,hour,minute,second];
    }
});
//<Safari12

//>!BackCompat 2005.11.3

isc.addMethods(Date.prototype, {

//>	@method		date.toPrettyString()
//			Return this date in the format: <code>MM/DD/YY HH:MM</code>
//	@group  dateFormatting
//	@return (string)	formatted date string
//  @visibility external
//  @deprecated As of SmartClient 5.5 use +link{date.toShortDate()} instead
//<
toPrettyString : function () {
    return this.toUSShortDatetime();
}

});

isc.addMethods(Date, {


// --- Parsing functions --- :
// In 5.2 the paradigm was to provide formatters and complimentary parsers, like
// 'toEuropeanShortDate' and 'parseEuropeanShortDate'.
// We've moved away from this to instead use a single 'parseInput' function which takes a
// 'format' parameter specifying "MDY" / "DMY", etc.
// This is appropriate since we do not plan to provide parsing functions for every date formatter
// format.
// Leaving the older explicit parsing functions in place for back-compat only.

//>	@classMethod	Date.parseStandardDate()
//      Parse a date passed in as a string of format:
//      <code>YYYY-MM-DD HH:MM:SS</code> or <code>YYYY-MM-DD</code>
//      Returning a new <code>Date</code> object with the appropriate value.
//
//      @group  dateFormatting
//
//      @param  dateString  (string)	date value as a string
//
//      @return	(date)      date value
//      @visibility internal
//  @deprecated As of SmartClient 5.5 use +link{date.parseInput} instead
//<
parseStandardDate : function (dateString) {
    if (!isc.isA.String(dateString)) return null;

    // Note: we could be using a regexp here rather than substring matches
    var year = dateString.substring(0,4),
        month = dateString.substring(5,7)-1,
        day = dateString.substring(8,10),
        hour = dateString.substring(11, 13),
        minute = dateString.substring(14, 16),
        second = dateString.substring(17, 19);

    // If they all are numbers, construct a new date
    // NOTE: If year - month - day gives a number then they
    // are all numbers, or strings that implicitly convert to numbers.
    // We could also use this syntax:
    // if(parseInt(year) == year && parseInt(month) == month ...)
    // but this is slower in both Moz and IE
    if (dateString.length < 19) {
        if (!isc.isA.Number(year - month - day)) return null;
    } else {
        if (!isc.isA.Number(year - month - day - hour - minute - second)) return null;
    }

    return new Date(year, month, day, hour, minute, second);

},

//>	@classMethod	Date.parseSerializeableDate()
//      Parse a date passed in as a string of format:
//      <code>YYYY-MM-DD HH:MM:SS</code> or <code>YYYY-MM-DD</code>
//      Returning a new <code>Date</code> object with the appropriate value.
//      <i>This is a synonym for </i><code>Date.parseStandardDate()</code>
//
//      @group  dateFormatting
//      @param  dateString  (string)	date value as a string
//      @return	(Date)      date value
//      @visibility internal
//  @deprecated As of SmartClient 5.5 use +link{date.parseInput} instead
//<
parseSerializeableDate : function (dateString) {
    // synonym for parseStandardDate
    return this.parseStandardDate(dateString);
},


//>	@classMethod	Date.parseDBDate()
// Parse a date passed in as a string of format:
//  <code>$$DATE$$:<i>YYYY-MM-DD HH:MM:SS</i></code>
//      Returning a new <code>Date</code> object with the appropriate value.
//
//      @group  dateFormatting
//		@param	dateString  (string)	date value as a string
//		@return	(date)		date value
//      @visibility internal
//  @deprecated As of SmartClient 5.5 use +link{date.parseInput} instead
//<
parseDBDate : function (dateString) {

    // remove the leading "$$DATE$$:"
    if (isc.isA.String(dateString) && dateString.startsWith("$$DATE$$:")) {
        dateString = dateString.substring(9)
        return this.parseStandardDate(dateString);
    }

    return null;

},

//>	@classMethod	Date.parseDateStamp()
//
// Parse a dateStamp of the format: <code><i>YYYYMMDD</i>T<i>HHMMSS</i>[Z]</code><br><br>
//
// @group  dateFormatting
// @param	dateString	(string)	String to parse
// @return				(Date)		Date object, or null if not parsed correctly.
//
// @visibility internal
//  @deprecated As of SmartClient 5.5 use +link{date.parseInput} instead
//<
parseDateStamp : function (string) {
	if (string == null || isc.isA.Date(string)) return string;

    var date = new Date( Date.UTC(
                string.substring(0,4),                // year
                parseInt(string.substring(4,6), 10)-1,    // mon
                string.substring(6,8),              // day
                // omit this character (T)
                string.substring(9,11),             // hour
                string.substring(11,13),            // min
                string.substring(13,15)
                // Technically we should look at the last character - if its something other
                // than "z" the timezone would be something other than UTC.
               ));

	if (isc.isA.Date(date)) return date;
	else				return null;

},

//>	@classMethod	Date.parseShortDate()
// Parse a date passed in as a string of format:   <code>MM/DD/YYYY</code>
//
//      @group  dateFormatting
//		@param	dateString  (string)	date value as a string
//      @param  [centuryThreshold]  (int)    if parsed year is 2 digits and less than this
//                                              number, assume year to be 20xx
//
//		@return	(date)		date value
//  @visibility internal
//  @deprecated As of SmartClient 5.5 use +link{date.parseInput} instead
//<
parseShortDate : function (string, centuryThreshold) {
    return this.parseInput(string, "MDY", centuryThreshold);
},

//>	@classMethod	Date.parseShortDateTime()
// Parse a date passed in as a string of format:   <code>MM/DD/YYYY HH:MM:SS</code>
//
//      @group  dateFormatting
//		@param	dateString  (string)	date value as a string
//      @param  [centuryThreshold]    (int)    if parsed year is 2 digits and less than this
//                                              number, assume year to be 20xx
//
//		@return	(date)		date value
//  @visibility internal
//  @deprecated As of SmartClient 5.5 use +link{date.parseInput} instead
//<

parseShortDateTime : function (string, centuryThreshold) {
    // synonym for parseShortDate - included for completeness and to provide the appropriate
    // compliment to date.toShortDateTime()
    return this.parseShortDate(string, centuryThreshold);
},

//>	@classMethod	Date.parsePrettyString()
// Parse a date passed in as a string of format:   <code>MM/DD/YY HH:MM:SS</code>
//
//      @group  dateFormatting
//		@param	dateString  (string)	date value as a string
//      @param  [centuryThreshold]    (int)    if parsed year is less than this
//                                              number, assume year to be 20xx rather than 19xx
//
//		@return	(date)		date value
//  @visibility internal
//  @deprecated As of SmartClient 5.5 use +link{date.parseInput} instead
//<
parsePrettyString : function (string, centuryThreshold) {
    // this is just the same as a short date with a 2 digit year.
    return this.parseShortDate(string, centuryThreshold);
},

//>	@classMethod	Date.parseEuropeanShortDate()
//			parse a date passed in as a string of format:   <code>DD/MM/YYYY</code>
//		@group	dateFormatting
//		@param	dateString  (string)	date value as a string
//      @param  [centuryThreshold]    (int)    if parsed year is 2 digits and less than this
//                                              number, assume year to be 20xx
//
//		@return	(date)		date value
//      @visibility internal
//  @deprecated As of SmartClient 5.5 use +link{date.parseInput} instead
//<
parseEuropeanShortDate : function (string, centuryThreshold) {
    return this.parseInput(string, "DMY", centuryThreshold);
},

//>	@classMethod	Date.parseEuropeanShortDateTime()
//			parse a date passed in as a string of format:   <code>DD/MM/YYYY HH:MM:SS</code>
//		@group	dateFormatting
//		@param	dateString  (string)	date value as a string
//      @param  [centuryThreshold]    (int)    if parsed year is 2 digits and less than this
//                                              number, assume year to be 20xx
//
//		@return	(date)		date value
//  @visibility internal
//  @deprecated As of SmartClient 5.5 use +link{date.parseInput} instead
//<

parseEuropeanShortDateTime : function (string, centuryThreshold) {
    return this.parseInput(string, "DMY", centuryThreshold);
},

// Helper to set the time to zero for a datetime

setToZeroTime : function (date) {
    if (date == null || !isc.isA.Date(date)) return date;

    // Clear the "logicalDate" flag so when we run through formatters we respect
    // developer specified timezone rather than displaying time in the browser native timezone
    var wasLogicalDate = date.logicalDate;
    date.logicalDate = false;

    var timestamp = date.getTime();

    // Apply the timezone offset such that if the default system-wide formatter is used
    // and applies the display timezone offset, 00:00 will be seen.
    var hourOffset = isc.Time.getUTCHoursDisplayOffset(date),
        minuteOffset = isc.Time.getUTCMinutesDisplayOffset(date)
    ;

    if (wasLogicalDate) {
        var previousDay = new Date(date);
        previousDay.setHours(0);
        previousDay.setMinutes(0);

        var previousDayHourOffset = isc.Time.getUTCHoursDisplayOffset(previousDay);
        if (hourOffset != previousDayHourOffset) {
            // logical dates have a time of 12-noon - if the date in question happens to be
            // the one that DST changes on, the final date (with a time of 00:00) will have
            // a different hourOffset - use that one instead.
            hourOffset = previousDayHourOffset;
        }
    }

    var utcHours = hourOffset > 0 ? 24-hourOffset : 0-hourOffset,
        utcMins = 60-minuteOffset;

    if (utcMins >= 60) {
        utcMins -= 60;

    // If the minute offset was non-zero and the offset as a whole is positive
    // we need to knock an additional hour off (as the hours/minutes are cumulative so
    // we otherwise will roll forward to 01:00 local time)
    
    } else if (utcMins != 0) {
        utcHours -= 1;
    }

    
    var oldDisplayDate;
    if (wasLogicalDate) {
        oldDisplayDate = date.getDate();
    } else {
        var offsetDate = date._getTimezoneOffsetDate(hourOffset, minuteOffset);
        oldDisplayDate = offsetDate.getUTCDate();
    }
    
    date.setUTCHours(utcHours);
    date.setUTCMinutes(utcMins);

    var displayOffsetDate = date._getTimezoneOffsetDate(hourOffset, minuteOffset),
        displayDate = displayOffsetDate.getUTCDate(),
        adjustedUTCHours = utcHours;

    if (displayDate != oldDisplayDate) {
        // Cant just check for displayDate > oldDisplayDate since it might be the first or
        // last of a month...
        var moveForward = date.getTime() < timestamp;

        adjustedUTCHours += moveForward ? 24 : -24;
        date.setUTCHours(adjustedUTCHours);
    }

    
    if (date.getUTCHours() != utcHours) {
        date.setTime(timestamp);
        date.setUTCHours(adjustedUTCHours+1);
        if (date.getUTCHours() != utcHours+1) {
            date.setTime(timestamp);
            date.setUTCHours(adjustedUTCHours+2);
        }
    }

    // No need to return the date - we updated it directly.

}

});
//<!BackCompat


