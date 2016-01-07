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
//> @class Time
// Helper methods and system-wide defaults for dealing with time values and time display formats.
// <P>
// This class includes utility methods for the creation and display of logical time values, as well
// as modifying the default display timezone for datetime type values. See
// +link{group:dateFormatAndStorage} for more information on working with dates, times and datetimes
// in SmartClient.
//
// @treeLocation Client Reference/System
// @visibility external
//<
isc.ClassFactory.defineClass("Time");


isc.Time.addClassProperties({

    //> @classAttr  Time.UTCHoursOffset (number : null : IRA)
    // Hour offset from UTC to use when formatting +link{fieldType,"datetime"} type fields for 
    // display to the user.
    // <P>
    // Has no effect on fields specified as logical date (<code>field.type = "date";</code>) and
    // logical time (<code>field.type = "time"</code>) fields.
    //
    // @visibility external
    // @deprecated As of 7.0 this attribute has been deprecated in favor of
    // +link{Time.setDefaultDisplayTimezone()}
    //<
    //UTCHoursOffset:0,
    // ** On page load we check for this property being set and use it to call
    //    setDefaultDisplayTimezone() with a deprecated warning
    
    
    //> @classMethod  Time.setDefaultDisplayTimezone()
    // Sets the offset from UTC to use when formatting values of type +link{FieldType,datetime} 
    // with standard display formatters.
    // <p>
    // This property effects how dates are displayed and also the
    // assumed timezone for user-input. For a concrete example - assume this method has been called 
    // and passed a value of "+01:00", and an application has a +link{DateTimeItem} visible in
    // a DynamicForm. If the value of this field is set to the current date, with UTC time set to
    // "10:00", the time portion of the value displayed in the form item will be "11:00".
    // Similarly if a user modifies the time value in the text box to be "16:00", a call to 
    // +link{FormItem.getValue()} for the item will return a date object with UTC time set to 15:00.
    // <P>
    // Interaction with daylight savings time: The specified "defaultDisplayTimezone" should
    // reflect the correct UTC offset for the current date, for which it will always be exactly respected;
    // adjustment will only be made for dates that fall outside the current daylight savings time mode.
    // <P>
    // In other words if DST is currently not in effect (IE: the current date is a Winter date),
    // any other dates where DST is not in effect will be formatted to exactly respect the specified
    // defaultDisplayTimezone (so for defaultDisplayTimezone of "+01:00", the display
    // string will be 1 hour ahead of the UTC time on the date in question), and any
    // dates where DST is in effect would be further adjusted to account for DST
    // (so the display string would be 2 hours ahead for dates that fall in the Summer).<br>
    // Alternatively if DST currently is in effect (EG: Current date is a Summer date)
    // the situation is reversed. Any date value for which DST should be applied
    // will be be formatted for display with an offset of 1 hour from UTC - and any date value
    // for which DST should not be applied would be formatted with an offset of 0 hours from UTC.
    // <br>
    // Note that the +link{Time.adjustForDST} property may be set to <code>false</code> to
    // disable this logic - in this case the time portion of dates will always be offset from
    // UTC by exactly the specified defaultDisplayOffset, regardless of whether they fall in the
    // range where Daylight Savings Time would usually be applied or not.
    // <p>
    // Note that if a custom timezone is specified, it will not effect native javascript 
    // date formatting functions such as <code>toLocaleString()</code>.
    // See +link{group:dateFormatAndStorage} for more on how SmartClient handles date and time
    // formatting and storage.
    // <P>
    // If this method is never called, the default display timezone for times and datetimes will
    // be derived from the native browser local timezone.
    // <P>
    // Note that the displayTimezone effects datetime fields only and has no effect on fields
    // specified as logical date (<code>field.type = "date";</code>) or
    // logical time (<code>field.type = "time"</code>).
    //
    // @param offset (string) offset from UTC. This should be a string in the format
    //    <code>+/-HH:MM</code> for example <code>"-08:00"</code>
    // @see group:dateFormatAndStorage
    // @visibility external
    //<
    setDefaultDisplayTimezone : function (offset, isBrowserDefault) {
        
        this._customTimezone = !isBrowserDefault;
        
        if (offset == null) return;
        // Handle being passed an offset in minutes - this matches the format returned by
        // native Date.getTimezoneOffset()
        var hours, minutes;
        if (isc.isA.Number(offset)) {
            
            offset = -offset;
            hours = Math.floor(offset/60);
            minutes = offset - (hours*60);
        } else if (isc.isA.String(offset)) {
            var HM = offset.split(":");
            hours = HM[0];
            // If the string starts with "-", hours and minutes will be negative
            var negative = hours && hours.startsWith("-");
            if (negative) hours = hours.substring(1);
            minutes = HM[1];
            
            hours = (negative ? -1 : 1) * parseInt(hours,10);
            minutes = (negative ? -1 : 1) * parseInt(minutes,10);
        }
        
        if (isc.isA.Number(hours) && isc.isA.Number(minutes)) {
            this.UTCHoursDisplayOffset = hours;
            this.UTCMinutesDisplayOffset = minutes;
        }
     
    },
    
    //> @classMethod Time.getDefaultDisplayTimezone()
    // Returns the default display timezone set up by +link{Time.setDefaultDisplayTimezone}.
    // If no explicit timezone has been set this will return the browser locale timezone offset.
    // @return (string) String of the format <code>+/-HH:MM</code>
    // @visibility external
    //<
    // we don't call this internally since it's easier to to work with the stored hours/minutes
    // directly
    getDefaultDisplayTimezone : function () {
        var H = this.UTCHoursDisplayOffset,
            M = this.UTCMinutesDisplayOffset,
            negative = H < 0;
        return (!negative ? "+" : "-") +
		    ((negative ? -1 : 1) * H).stringify(2) + ":" + ((negative ? -1 : 1) * M).stringify(2);    
    },
        
    //>	@classAttr	isc.Time._timeExpressions (Array : [..] : IRA)
	// List of regular expressions to parse a time string
	//		@group	parsing
	//<
	_timeExpressions : [				
			/^\s*(\d?\d)\s*[: ]\s*(\d?\d)\s*[: ]\s*(\d?\d)?\s*([AaPp][Mm]?)?\s*([+-]\d{2}:\d{2}|Z)?\s*$/,
			/^\s*(\d?\d)\s*[: ]\s*(\d?\d)(\s*)([AaPp][Mm]?)?\s*([+-]\d{2}:\d{2}|Z)?\s*$/,
			/^\s*(\d\d)(\d\d)(\d\d)?\s*([AaPp][Mm]?)?\s*([+-]\d{2}:\d{2}|Z)?\s*$/,
			/^\s*(\d)(\d\d)(\d\d)?\s*([AaPp][Mm]?)?\s*([+-]\d{2}:\d{2}|Z)?\s*$/,
			/^\s*(\d\d?)(\s)?(\s*)([AaPp][Mm]?)?\s*([+-]\d{2}:\d{2}|Z)?\s*$/
		],

    // This is a combination of the time patterns matched by regular expressions in `_timeExpressions'.
    // If this is changed, be sure to update Time._prepForParseValueExpressions() as well.
    _combinedTimeExpression: /(?:(\d?\d)\s*[: ]\s*(\d?\d)\s*[: ]\s*(\d?\d)?|(\d?\d)\s*[: ]\s*(\d?\d)(\s*)|(\d\d)(\d\d)(\d\d)|(\d)(\d\d)(\d\d)?|(\d\d?)(\s)?(\s*))\s*([AaPp][Mm])?/g,

    //> @type   TimeDisplayFormat
    // String designating a standard time format for displaying the times associated with 
    // dates strings.
    // @value   toTime
    //  String will display with seconds and am/pm indicator:<code>[H]H:MM:SS am|pm</code>. <br>
    //  Example: <code>3:25:15 pm</code>
    // @value  to24HourTime
    //  String will display with seconds in 24 hour time: <code>[H]H:MM:SS</code>. <br>
    //  Example: <code>15:25:15</code>
    // @value  toPaddedTime
    //  String will display with seconds, with a 2 digit hour and am/pm indicator: 
    //  <code>HH:MM:SS am|pm</code> <br>
    //  Example: <code>03:25:15 pm</code>
    // @value  toPadded24HourTime
    //  String will display with seconds, with a 2 digit hour in 24 hour format: 
    //  <code>HH:MM:SS</code> <br>
    //  Examples: <code>15:25:15</code>, <code>03:16:45</code>
    // @value toShortTime
    //  String will have no seconds and be in 12 hour format:<code>[H]H:MM am|pm</code><br>
    //  Example: <code>3:25 pm</code>
    // @value toShort24HourTime
    //  String will have no seconds and be in 24 hour format: <code>[H]H:MM</code><br>
    //  Example:<code>15:25</code>
    // @value toShortPaddedTime
    //  String will have no seconds and will display a 2 digit hour, in 12 hour clock format:
    //  <code>HH:MM am|pm</code><br>
    //  Example: <code>03:25 pm</code>
    // @value toShortPadded24HourTime
    //  String will have no seconds and will display with a 2 digit hour in 24 hour clock format:
    // <code>HH:MM</code><br>
    // Examples: <code>15:25</code>, <code>03:16</code>
    //
    // @visibility external
    //<

    // To simplify parsing / formatting, map valid formatter names to the details of the format
    formatterMap:{
        toTime:{showSeconds:true, padded:false, show24:false},
        to24HourTime:{showSeconds:true, padded:false, show24:true},

        toPaddedTime:{showSeconds:true, padded:true, show24:false},
        toPadded24HourTime:{showSeconds:true, padded:true, show24:true},
        
        toShortTime:{showSeconds:false, padded:false, show24:false},
        toShort24HourTime:{showSeconds:false, padded:false, show24:true},
        toShortPaddedTime:{showSeconds:false, padded:true, show24:false},

        toShortPadded24HourTime:{showSeconds:false, padded:true, show24:true},
        toTimestamp:{showSeconds:true, padded:true, show24:true, showMillis:true}
    },
    
    
    //> @classAttr Time.displayFormat  (TimeDisplayFormat | function : "toTime" : RWA)
    // Standard formatter to be used when converting a date to a time-string via +link{Time.toTime()}
    // @setter Time.setNormalDisplayFormat()
    // @visibility external
    //<
    displayFormat:"toTime",

    //> @classAttr Time.shortDisplayFormat  (TimeDisplayFormat | function : "toShortTime" : RWA)
    // Standard formatter to be used when converting a date to a time-string via +link{Time.toShortTime()}
    // @setter Time.setShortDisplayFormat()
    // @visibility external
    //<
    shortDisplayFormat:"toShortTime",
    
    //> @classAttr Time.AMIndicator (string : " am" : RWA)
    // String appended to times to indicate am (when not using 24 hour format).
    // @visibility external
    // @group i18nMessages
    //<
    AMIndicator:" am",
    //> @classAttr Time.PMIndicator (string : " pm" : RWA)
    // String appended to times to indicate am (when not using 24 hour format).
    // @visibility external
    // @group i18nMessages
    //<
    PMIndicator:" pm"

    //> @classAttr Time.adjustForDST (boolean : true (see description) : RWA)
    // Determines whether datetime formatters should consider the effect of Daylight Saving
    // Time when computing offsets from UTC.  By default, this flag is set during framework
    // initialization if SmartClient detects that it is running in a locale that is observing 
    // DST this year.  If you do not want DST adjustments to be applied, set this flag to 
    // false.<p>
    // Note that setting this flag to true will have no effect unless you are in a locale 
    // that is observing Daylight Saving Time for the date in question; this is because
    // we rely on the browser for offset information, and browsers are only capable of 
    // returning local date and time information for the computer's current locale.
    // <P>
    // This setting will not have any impact on the display of fields specified as type "time" or
    // "date" (logical dates and logical times) - only on datetime type values. See
    // +link{group:dateFormatAndStorage} for information on working with dates, times and datetimes
    // in SmartClient.
    // @visibility external
    //<
    
});

isc.Time.addClassMethods({

    //> @classMethod Time.toTime()
    // Given a date object, return the time associated with the date as a formatted string.
    // If no formatter is passed, use the standard formatter set up via 
    // +link{Time.setNormalDisplayFormat()}.
    // 
    // @param date (Date) Date to convert to a time string.
    // @param [formatter] (TimeDisplayFormat | function) Optional custom formatter to use. Will accept
    //  a function (which will be passed a pointer to the date to perform the conversion), or
    //  a string designating a standard formatter
    // @param [logicalTime] Is the date passed in a representation of a logical time value such as
    //  a value from a <code>"time"</code> type field on a dataSource or a datetime value? 
    //  For datetime values the formatted string will respect any custom 
    // +link{Time.setDefaultDisplayTimezone,display timezone}.
    // If not explicitly specified, the date passed in will be assumed to be a datetime unless
    // it was created explicitly as a time via +link{Time.createLogicalTime()} or similar APIs.
    // @visibility external
    //<
    toTime : function (date, formatter, logicalTime) {
        return this.format(date, formatter, false, logicalTime);
    },
    
    //> @classMethod Time.toShortTime()
    // Given a date object, return the time associated with the date as a short string.
    // If no formatter is passed, use the standard formatter set up via +link{Time.setShortDisplayFormat()}
    // @param date (Date) Date to convert to a time string.
    // @param [formatter] (TimeDisplayFormat | function) Optional custom formatter to use. Will accept
    //  a function (which will be passed a pointer to the Date to format), or
    //  a string designating a standard formatter
    // @param [logicalTime] Is the date passed in a representation of a logical time value such as
    //  a value from a <code>"time"</code> type field on a dataSource or a datetime value? 
    //  For datetime values the formatted string will respect any custom 
    // +link{Time.setDefaultDisplayTimezone,display timezone}.
    // If not explicitly specified, the date passed in will be assumed to be a datetime unless
    // it was created explicitly as a time via +link{Time.createLogicalTime()} or similar APIs.

    // @visibility external
    //<    
    toShortTime : function (date, formatter, logicalTime) {
        return this.format(date, formatter, true, logicalTime);
    },

    // Given a date return a formatted time string
    _$timeTemplate:[null, ":", null, ":"],
    _$shortTimeTemplate:[null, ":"],
    
    format : function (date, formatter, shortFormat, logicalTime) {
        // If we're passed a random object (most likely null or a string), just return it
        if (!isc.isA.Date(date)) return date;

        var originalFormatter = formatter;

        // Sanity check - don't allow unexpected things passed in as a formatter to give us
        // odd results
        if (!formatter && !isc.isA.String(formatter) && !isc.isA.Function(formatter)) {
            formatter = shortFormat ? this.shortDisplayFormat : this.displayFormat;
        }

        // Support passing in a completely arbitrary formatter function
        if (isc.isA.Function(formatter)) return formatter(date, logicalTime);
        
        if (isc.isA.String(formatter)) formatter = this.formatterMap[formatter];
        
        if (!isc.isAn.Object(formatter)) {
            this.logWarn("Invalid time formatter:" + originalFormatter + " - using 'toTime'");
            formatter = this.formatterMap.toTime;
        }

        var showSeconds = formatter.showSeconds,
            padded = formatter.padded,
            show24 = formatter.show24,
            showMillis = formatter.showMillis;
        
        var useCustomTimezone;
        
        if (logicalTime != null) useCustomTimezone = !logicalTime;
        else useCustomTimezone = !date.logicalTime && !date.logicalDate;
        
        var hour,minutes;
        if (!useCustomTimezone) {
            hour = date.getHours();
            minutes = date.getMinutes();
        } else {
        
            var hour = date.getUTCHours(),
                minutes = date.getUTCMinutes();
    
            // Add the display timezone offset to the hours / minutes so we display the
            // time in the appropriate timezone
            var hm = this._applyTimezoneOffset(hour, minutes,
                                                this.getUTCHoursDisplayOffset(date),
                                                this.getUTCMinutesDisplayOffset(date));
            hour = hm[0];
            minutes = hm[1];
        }
   
        
        var seconds = showSeconds ? date.getUTCSeconds() : null,
            pm = show24 ? null : (hour >=12);
        
        // Hour will be in 24 hour format by default
        if (!show24) {
            if (hour > 12) hour = hour - 12;
            if (hour == 0) hour = 12;
        }
        if (padded) hour = hour.stringify(2);
        
        var template = showSeconds ? this._$timeTemplate : this._$shortTimeTemplate;
        template[0] = hour;
        template[2] = minutes.stringify();
        if (showSeconds) template[4] = seconds.stringify();
        
        if (!show24) template[5] = (pm ? this.PMIndicator : this.AMIndicator);
        else template[5] = null;

        var formatted = template.join(isc.emptyString);
        
        if (showMillis) {
            var millis = date.getMilliseconds().stringify(3);
            formatted += "." + millis;
        }
        
        return formatted;
    },
    
    //> @classMethod Time.parseInput()
    // Converts a time-string such as <code>1:00pm</code> to a new Date object 
    // representing a logical time value (rather than a specific datetime
    // value), typically for display in a +link{DataSourceField.type,time type field}. 
    // Accepts most formats of time string. The generated
    // Date value will have year, month and date set to the epoch date (Jan 1 1970), and time 
    // elements set to the supplied hour, minute and second (in browser native local time).
    // <P>
    // See +link{group:dateFormatAndStorage} for more information on date, time and datetime 
    // values in SmartClient.
    //
    // @param timeString (string) time string to convert to a date
    // @param validTime (boolean) If this method is passed a timeString in an unrecognized format,
    //  return null rather than a date object with time set to 00:00:00
    // @visibility external
    //< 
    // UTCTime param deprecated - leaving supported (though now undocumented) for backCompat only.
    //
    // Additional 'isDatetime' and 'baseDatetime' parameters: These are used for the case where we
    // need to set the time portion of a datetime based on a user-entered time string.
    // In this case we need to respect the local timezone specified by 
    // +link{time.setDefaultDisplayTimezone}, and we'll also support respecting an explicit
    // timezone offset from UTC being present in the string (EG: <code>"00:00:00+02:00"</code>).
    //
    // Assuming we're not passed a 'baseDatetime', the returned date is always set to 1/1/1970. 
    // This is deliberate: It'll make DST never
    // an issue and it matches the format for Time values returned by the server for JSON format
    // DataSources.
    //
    // EXTREMELY forgiving of formatting, can accept the following:
	//		11:34:45 AM	=> 11:34:45
    //      11:34:45    => 11:34:45
	//		1:3:5 AM	=> 01:30:50
	//		1:3p		=> 13:30:00
	//		11 34 am	=> 11:34:00
	//		11-34		=> 11:34:00
	//		113445		=> 11:34:45
	//		13445		=> 01:34:45
	//		1134		=> 11:34:00
	//		134			=> 01:34:00
	//		11			=> 11:00:00
	//		1p			=> 13:00:00
	//		9			=> 09:00:00
    // Also supports explicitly specified timezone offset specified by "+/-HH:MM" at the end, though
    // we only care about this for the datetime case. logical times are literally a way for us
    // to work with numbers for H, M and S.
    
    // Note: technically being passed "1:00" is ambiguous - could be AM or PM.
    // We always interpret as 24 hour clock (so <12 = AM) unless am/pm is  passed in.
    parseInput : function (string, validTime, UTCTime, isDatetime, baseDatetime) {
        var hours = null,
            minutes = null,
            seconds = null,
            // We don't currently extract milliseconds from a time-string. Instead we zero them
            // out for consistency across times created by this method.
            milliseconds = null,
            ampm;

        var hoursOffset, minutesOffset;
        
        // if we're passed a date we'll return a new date with the same time (h/m/s/ms, not the same
        // date).
        if (isc.isA.Date(string)) {
            // We'll match the specified time exactly - no need to manipulate timezone offsets
            // here since the underlying UTC time will match and any offsetting for display
            // will occur in formatters.
            UTCTime = true;
            hours = string.getUTCHours();
            minutes = string.getUTCMinutes();
            seconds = string.getUTCSeconds();
            milliseconds = string.getUTCMilliseconds();
            
        } else if (string != null) {
    		// iterate through the time expressions, trying to find a match
    		for (var i = 0; i < isc.Time._timeExpressions.length; i++) {
                
    			var match = isc.Time._timeExpressions[i].exec(string);
    			if (match) break;
    		}
            if (match) {
        		// get the hours, minutes and seconds from the match
        		// NOTE: this results in 24:00 going to 23:00 rather than 23:59...
                var defaultHours,
                    defaultMinutes,
                    defaultSeconds;
                if (baseDatetime != null) {
                    defaultSeconds = defaultMinutes = defaultHours = null;
                } else {
                    defaultSeconds = defaultMinutes = defaultHours = 0;
                }
                hours = match[1] ? Math.min(parseInt(match[1], 10), 23) : defaultHours;
                minutes = match[2] ? Math.min(parseInt(match[2], 10), 59) : defaultMinutes;
                seconds = match[3] ? Math.min(parseInt(match[3], 10), 59) : defaultSeconds;
                ampm = match[4];

                if (ampm) {
                    if (!this._pmStrings) this._pmStrings = {p:true, P:true, pm:true, PM:true, Pm:true, pM:true};
                    if (this._pmStrings[ampm] == true) {
                        if (hours == null) hours = 12;
                        else if (hours < 12) hours += 12;
                    } else if (hours == 12) hours = 0;
                }
                
                // For dateTimes only, if a timezone was explicitly specified on the value passed in,
                // respect it.
                // So we'll handle 18:00:01 -01:00 as 6pm one hour offset from UTC on the generated
                // date value.
                // NOTE: the offset specifies the timezone the date is already in, so 
                // to get to UTC we have to subtract the offset
                
                if (isDatetime && match[5] != null && match[5] != "" && match[5].toLowerCase() != "z") {
                    var HM = match[5].split(":"),
                        H = HM[0],
                        negative = H && H.startsWith("-"),
                        M = HM[1];
                    hoursOffset = parseInt(H,10);
                    minutesOffset = (negative ? -1 : 1) * parseInt(M,10);
                }
            } else if (validTime) return null;
        } else if (validTime) return null;

        var date;
        if (baseDatetime != null) {
            date = baseDatetime.duplicate();
        } else {
            date = new Date(null);
            // Zero out the milliseconds for consistency.
            date.setMilliseconds(0);
        }
        if (isDatetime || UTCTime) {
            
            if (hoursOffset == null) {
                hoursOffset = UTCTime ? 0 : this.getUTCHoursDisplayOffset(date);
            }
            if (minutesOffset == null) {
                minutesOffset = UTCTime ? 0 : this.getUTCMinutesDisplayOffset(date);
            }
    
            // NOTE: we're creating UTC time -- any offset indicates the offset for the timezone
            // the inputted time is currently in [either browser local time or explicit offset
            // passed in as part of the time string], so we need to subtract this offset to get to
            // UTC time (not add it)
            var hm = this._applyTimezoneOffset(hours, minutes, (0-hoursOffset), (0-minutesOffset));
            
            hours = hm[0];
            minutes = hm[1];

            date.setUTCHours(hours == null ? date.getUTCHours() : hours,
                             minutes == null ? date.getUTCMinutes() : minutes,
                             seconds == null ? date.getUTCSeconds() : seconds,
                             milliseconds == null ? date.getUTCMilliseconds() : milliseconds);
        } else {
            date.setHours(hours == null ? date.getHours() : hours,
                          minutes == null ? date.getMinutes() : minutes,
                          seconds == null ? date.getSeconds() : seconds,
                          milliseconds == null ? date.getMilliseconds() : milliseconds);
        }
        
        // Mark as logical time so we format / serialize correctly without requiring 
        // explicit "logicalTime" param to formatter functions
        if (!isDatetime) date.logicalTime = true;
        
        return date;
    },

    // Preps a string value for parsing by FormItem.parseValueExpressions().
    // It is assumed that value contains a time value expression. The result of calling this
    // function is a new time value expression that can be better parsed by FormItem.parseValueExpressions().
    // For example, without the use of this function, FormItem.parseValueExpressions() will fail
    // on "< 6 am" because of the space between "6" and "am".
    _prepForParseValueExpressions : function (value) {
        if (value == null) return null;
        value = String(value);

        value = value.replace(this._combinedTimeExpression, function (match, p1, p2, p3,
                                                                             p4, p5, p6,
                                                                             p7, p8, p9,
                                                                             p10, p11, p12,
                                                                             p13, p14, p15,
                                                                             p16) {
            p1 = parseInt(p1 || p4 || p7|| p10 || p13) || 0;
            p2 = parseInt(p2 || p5 || p8|| p11 || p14) || 0;
            p3 = (p3 || p6 || p9|| p12 || p15);
            if (p3) {
                p3 = ":" + (parseInt(p3) || 0).stringify(2);
            } else p3 = "";
            p16 = (p16 || "").trim();
            var value = p1 + ":" + p2.stringify(2) + p3;
            if (p16) {
                value += p16;
            }
            return value + " ";
        });
        return value;
    },

    // Helper method to apply an arbitrary timezone offset to hours / minutes
    // Returns array: [newHours,newMinutes,dayOffset]
    // dayOffset ignored for time fields, but can be used to update datetimes
    _applyTimezoneOffset : function (hours, minutes, hOffset, mOffset) {
        if (minutes == null || hours == null) {
            this.logWarn("applyTimezoneOffset passed null hours/minutes");
            return [hours,minutes];
        }
        if (hOffset == null) hOffset = 0;
        if (mOffset == null) hOffset = 0;
        if (hOffset == 0 && mOffset == 0) return [hours,minutes,0];
        
        hours += hOffset;
        minutes += mOffset;
        
        // Catch the case where the display offset from UTC pushes the hours / minutes
        // past 60 [or 24] or below zero
        // (Don't worry about the date - we're only interested in the time!)
        while (minutes >= 60) {
            minutes -= 60;
            hours += 1;
        }
        
        while (minutes < 0) {
            minutes += 60;
            hours -= 1;
        }

        var dayOffset = 0;
        
        while (hours >= 24) {
            hours -= 24;
            dayOffset += 1;
        }
        while (hours < 0) {
            hours += 24;
            dayOffset -= 1;
        }
        
        return [hours,minutes, dayOffset];
    },
    
     
    //> @classMethod Time.createDate()
    // Creates a date object with the time set to the hours, minutes and seconds passed in.
    // Unless the <code>UTCTime</code> parameter is passed in, parameters are assumed
    // to specify the time in native local display time.
    // @param [hours] (number) Hours for the date (defaults to zero)
    // @param [minutes] (number) Minutes for the date (defaults to zero)
    // @param [seconds] (number) Seconds for the date (defaults to zero)
    // @param [milliseconds] (number) Milliseconds for the date (defaults to zero)
    // @param [UTCTime] (boolean) If true, treat the time passed in as UTC time rather than local time
    // @visibility external
    // @deprecated use +link{Time.createLogicalTime()} instead.
    //<
    createDate : function (hours, minutes, seconds, milliseconds, UTCTime) {
        return this.createLogicalTime(hours, minutes, seconds, milliseconds, UTCTime);
    },

    //> @classMethod Time.createLogicalTime()
    // Create a new Date object to represent a logical time value (rather than a specific datetime
    // value), typically for display in a +link{DataSourceField.type,time type field}. The generated
    // Date value will have year, month and date set to the epoch date (Jan 1 1970), and time 
    // elements set to the supplied hour, minute and second (in browser native local time).
    // <P>
    // See +link{group:dateFormatAndStorage} for more information on date, time and datetime 
    // values in SmartClient.
    //
    // @param hour (integer) hour (0-23)
    // @param minute (integer) minute (0-59)
    // @param second (integer) second (0-59)
    // @return (Date) new Javascript Date object representing the time in question
    // @visibility external
    //<
    // This is also available as Date.createLogicalTime [and the deprecated Time.createDate]
    // The returned date is always set to 1/1/1970. This is deliberate: It'll make DST never
    // an issue and it matches the format for Time values returned by the server for JSON format
    // DataSources.
    createLogicalTime : function (hours, minutes, seconds, milliseconds, UTCTime) {

        var date = new Date(null);

        if (hours == null) hours = 0;
        if (minutes == null) minutes = 0;
        if (seconds == null) seconds = 0;
        if (milliseconds == null) milliseconds = 0;
        
        if (UTCTime) {
            date.setUTCHours(hours, minutes, seconds, milliseconds);
        } else {
            date.setHours(hours, minutes, seconds, milliseconds);
        }
        date.logicalTime = true;
        return date;
    },
    
    //> @classMethod Time.setShortDisplayFormat()
    // Sets the default format for strings returned by +link{Time.toShortTime()}.
    // @param formatter (TimeDisplayFormat | function) Optional custom formatter to use. Will accept
    //  a function (which will be passed a pointer to the date to perform the conversion), or
    //  a string designating a standard formatter
    // @visibility external
    //<    
    setShortDisplayFormat : function (format) {
        this.shortDisplayFormat = format;
    },
    
    //> @classMethod Time.setNormalDisplayFormat()
    // Sets the default format for strings returned by +link{Time.toTime()}.
    // @param formatter (TimeDisplayFormat | function) Optional custom formatter to use. Will accept
    //  a function (which will be passed a pointer to the date to perform the conversion), or
    //  a string designating a standard formatter
    // @visibility external
    //<    
    setNormalDisplayFormat : function (format) {
        this.displayFormat = format;
    },
    
    //> @classMethod Time.compareTimes()
    // Compares the times of 2 dates, or strings. If a string is passed as one of the 
    // parameters it should be in a format that converts to a valid time such as <code>"1:30pm"</code>, 
    // <code>"13:30"</code>, or <code>"1:30:45pm"</code>
    // @param time1 (Date|string) First time to compare
    // @param time2 (Date|string) Second time to compare
    // @return (boolean) True if the times match, false if not
    // @visibility external
    //<    
    compareTimes : function (time1, time2) {
        // If this method becomes time-critical we could speed this up by avoiding the
        // date conversion and having parseInput return just an array of H,M,S
        if (isc.isA.String(time1)) time1 = isc.Time.parseInput(time1);
        if (isc.isA.String(time2)) time2 = isc.Time.parseInput(time2);
        
        if (time1 == null && time2 == null) return true;
        
        // If we get non-dates at this point just return false - we don't want to be
        // comparing other types
        if (!isc.isA.Date(time1) || !isc.isA.Date(time2)) return false;
        
        
        return ((time1.getUTCHours() == time2.getUTCHours()) && 
                (time1.getUTCMinutes() == time2.getUTCMinutes()) && 
                (time1.getUTCSeconds() == time2.getUTCSeconds()));
        
    },

    //> @classMethod Time.compareLogicalTimes()
    // Compare two times, normalizing out the date elements so that only the time elements are 
    // considered; returns 0 if equal, -1 if the first time is greater (later), or 1 if
    // the second time is greater.
    //  @param  time1   (Date)  first time to compare
    //  @param  time2   (Date)  second time to compare
    //  @return (number)    0 if equal, -1 if first time &gt; second time, 1 if second time &gt;
    //                      first time.  Returns false if either argument is not a date
    //<
    compareLogicalTimes : function (time1, time2) {
        if (!isc.isA.Date(time1) || !isc.isA.Date(time2)) return false;

        time1 = isc.Date.getLogicalTimeOnly(time1);
        time2 = isc.Date.getLogicalTimeOnly(time2);

        var aHours = time1.getHours(),
            aMinutes = time1.getMinutes(),
            aSeconds = time1.getSeconds(),
            aMillis = time1.getMilliseconds();
        var bHours = time2.getHours(),
            bMinutes = time2.getMinutes(),
            bSeconds = time2.getSeconds(),
            bMillis = time2.getMilliseconds();
        var aval = aMillis + 1000 * (aSeconds + 60 * (aMinutes + 60 * aHours));
        var bval = bMillis + 1000 * (bSeconds + 60 * (bMinutes + 60 * bHours));
        return aval > bval ? -1 : (bval > aval ? 1 : 0);
    },

    _performDstInit : function () {
        var now = new Date(),
            january = new Date(0),
            july = new Date(0);

        // Daylight Saving Time involves moving the clock forward in order to shift some of 
        // the daylight from very early morning (when most people are asleep) to mid-evening
        // (when people benefit from more hours of daylight, and energy can be saved that 
        // would otherwise be needed for lighting).  Not every country observes DST, and those
        // countries that do observe it set their own start and end dates, though there are 
        // common approaches - for example, many European countries start DST during the last
        // weekend of March and end it during the last weekend of October.
        //
        // Daylight Saving Time, if it is applicable at all, always starts sometime in spring 
        // and ends ends sometime in autumn, but there is no more accurate rule than that.
        // Currently, every country that observes DST does so by moving their local time 
        // forward by one hour; however, other values have been used, so this cannot be relied
        // upon either.
        //
        // It is common to transition to and from DST ar 02:00 local time - when
        // DST starts, the local time jumps instantly to 03:00, when DST ends it jumps 
        // instantly back to 01:00.  However, this is again a common approach rather than a
        // rule.
        // 
        // Note that it is important to think in terms of seasons rather than months, because 
        // the northern and southern hemispheres have opposite seasons.  Hence DST (if it 
        // applies at all) starts in March/April and ends in October/November in the northern 
        // hemisphere, and does the exact opposite in the southern hemisphere.
        // 
        // Because of all of this, and because the only timezone information you can retrieve 
        // from a Javascript Date object is the number of minutes that particular date/time 
        // is offset from UTC, we have quite limited information and must resort to roundabout
        // techniques.  We can discover if we are in a locale that observes DST by checking
        // the UTC offsets in January and July; if they are different, the current locale 
        // observes DST.  
        // 
        // Going a step further than this, we can tell whether we are observing DST or normal 
        // time on an arbitrary date: by looking to see whether the clock goes  forward or 
        // backward in the early part of the year (spring in the northern hemisphere), we can 
        // infer which hemisphere the current locale is in, and from that we can decide if 
        // the offset in January is the DST or non-DST offset.  Then, we can check the offset
        // of the given date against the offset in January; if it matches then it is in DST
        // if we're in the southern hemisphere, and in normal time if we're in the northern 
        // hemisphere.
        //
        // For more interesting information on this subject, see 
        // http://www.timeanddate.com/time/aboutdst.html
        
        january.setUTCFullYear(now.getUTCFullYear());
        january.setUTCMonth(0);
        january.setUTCDate(1);
        july.setUTCFullYear(now.getUTCFullYear());
        july.setUTCMonth(6);
        july.setUTCDate(1);
            
        var nowOffset = now.getTimezoneOffset();
        this.januaryDstOffset = january.getTimezoneOffset();
        var julyOffset = july.getTimezoneOffset();
        
        this.dstDeltaMinutes = this.januaryDstOffset - julyOffset;
        if (this.dstDeltaMinutes > 0) {
            // Time is offset further forward from UTC in July; this locale observes DST
            // and is in the northern hemisphere (this logic is curiously backwards, because
            // getTimezoneOffset() returns negative numbers for positive offsets)
            this.southernHemisphere = false;
            this.adjustForDST = true;
            if (nowOffset == julyOffset) this.currentlyInDST = true;
        } else if (this.dstDeltaMinutes < 0) {
            // Time is offset further forward from UTC in January; this locale observes DST
            // and is in the southern hemisphere
            this.southernHemisphere = true;
            this.adjustForDST = true;
            if (nowOffset == this.januaryDstOffset) this.currentlyInDST = true;
        } else {
            // the delta is 0 and DST is not a factor in this locale
            this.adjustForDST = false;
        }
            
        // As noted above, all current observations of Daylight Saving Time involve moving 
        // local time one hour forward, so right now these variables will always end up as
        // 1 and 0 
        this.dstDeltaMinutes = Math.abs(this.dstDeltaMinutes);
        this.dstDeltaHours = Math.floor(this.dstDeltaMinutes / 60);
        this.dstDeltaMinutes -= (this.dstDeltaHours * 60);
    },

    getUTCHoursDisplayOffset : function (date, utcHoursDisplayOffset) {
        // If we're currently inside DST and wanting to calculate an offset for a datetime 
        // that is outside DST, we need to move the offset backwards because the offset we
        // stored on the Time class during startup already includes the DST offset
        var dstDelta = this.currentlyInDST ? -(this.dstDeltaHours) : 0;
        if (this.adjustForDST) {
            if (date.getTimezoneOffset() == this.januaryDstOffset) {
                if (this.southernHemisphere) {
                    dstDelta += this.dstDeltaHours;
                }
            } else {
                if (!this.southernHemisphere) {
                    dstDelta += this.dstDeltaHours;
                }
            }
        }
        return (utcHoursDisplayOffset != null
                ? utcHoursDisplayOffset
                : this.UTCHoursDisplayOffset) + (this.adjustForDST ? dstDelta : 0);
    },

    getUTCMinutesDisplayOffset : function (date, utcMinutesDisplayOffset) {
        var dstDelta = this.currentlyInDST ? -(this.dstDeltaMinutes) : 0;
        if (this.adjustForDST) {
            if (date.getTimezoneOffset() == this.januaryDstOffset) {
                if (this.southernHemisphere) {
                    dstDelta += this.dstDeltaMinutes;
                }
            } else {
                if (!this.southernHemisphere) {
                    dstDelta += this.dstDeltaMinutes;
                }
            }
        }
        return (utcMinutesDisplayOffset != null
                ? utcMinutesDisplayOffset
                : this.UTCMinutesDisplayOffset) + (this.adjustForDST ? dstDelta : 0);
    }
});

// Work out whether we're currently inside Daylight Saving Time, and compute the offset to 
// apply on the transition.
isc.Time._performDstInit();

// set up the default timezone offset based on the browser locale here.
isc.Time.setDefaultDisplayTimezone(new Date().getTimezoneOffset(), true);



