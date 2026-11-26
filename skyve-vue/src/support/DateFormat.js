/**
 * Formats the date and time values the server sends as the Java converters show them, without a date library.
 *
 * The server serialises a date as yyyy-MM-dd, a time as HH:mm:ss, and a date time or timestamp as
 * yyyy-MM-dd'T'HH:mm:ss.SSS with a UTC offset (see TimeUtil.formatISODate). A value with an offset is shown in the
 * browser's time zone; one without is taken as local, and a time on its own as that time today.
 *
 * Patterns use the SimpleDateFormat letters the converters need: dd, MM, MMM, yyyy, HH, hh, mm, ss and a,
 * in English whatever the browser's language (the Java converters format in Locale.ENGLISH).
 * An unparseable value comes out as "Invalid DateTime", as it did with Luxon.
 */

const MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];

const INVALID = 'Invalid DateTime';

// yyyy-MM-dd, optionally followed by Thh:mm[:ss[.fraction]] and an offset (Z, +hh, +hhmm or +hh:mm)
const DATE_RE = /^(\d{4})-(\d{2})-(\d{2})(?:T(\d{2}):(\d{2})(?::(\d{2})(?:[.,](\d{1,9}))?)?(Z|[+-]\d{2}(?::?\d{2})?)?)?$/;

// [T]hh:mm[:ss[.fraction]], optionally with an offset
const TIME_RE = /^T?(\d{2}):(\d{2})(?::(\d{2})(?:[.,](\d{1,9}))?)?(Z|[+-]\d{2}(?::?\d{2})?)?$/;

// The pattern letters, longest first so MMM is matched before MM; anything else is copied as is
const TOKEN_RE = /yyyy|MMM|MM|dd|HH|hh|mm|ss|a/g;

function pad2(n) {
    return (n < 10) ? `0${n}` : String(n);
}

/**
 * The offset in minutes east of UTC, or undefined if none was given.
 */
function offsetMinutes(offset) {
    if (! offset) {
        return undefined;
    }
    if (offset === 'Z') {
        return 0;
    }
    const sign = (offset[0] === '-') ? -1 : 1;
    const digits = offset.slice(1).replace(':', '');
    const hours = Number(digits.slice(0, 2));
    const minutes = (digits.length > 2) ? Number(digits.slice(2)) : 0;
    return sign * ((hours * 60) + minutes);
}

function milliseconds(fraction) {
    return fraction ? Math.floor(Number(`0.${fraction}`) * 1000) : 0;
}

/**
 * Builds the instant from the parts, or returns null if they are not a real date and time.
 * The result is { date, local }: local is true when no offset was given.
 */
function toDate(year, month, day, hour, minute, second, millis, offset) {
    if ((month < 1) || (month > 12) || (day < 1) || (hour > 24) || (minute > 59) || (second > 59)) {
        return null;
    }
    // 24:00 is allowed only on the hour, and means the start of the next day
    if ((hour === 24) && ((minute !== 0) || (second !== 0) || (millis !== 0))) {
        return null;
    }
    // The calendar date must exist (Date rolls 31 April over to 1 May)
    const check = new Date(Date.UTC(2000, 0, 1));
    check.setUTCFullYear(year, month - 1, day);
    if ((check.getUTCMonth() !== (month - 1)) || (check.getUTCDate() !== day)) {
        return null;
    }

    const minutesEast = offsetMinutes(offset);
    let result;
    if (minutesEast === undefined) {
        // Local time; a time that does not exist (a daylight saving gap) moves forward
        result = new Date(2000, 0, 1);
        result.setFullYear(year, month - 1, day);
        result.setHours(hour, minute, second, millis);
    }
    else {
        const utc = new Date(Date.UTC(2000, 0, 1));
        utc.setUTCFullYear(year, month - 1, day);
        utc.setUTCHours(hour, minute, second, millis);
        result = new Date(utc.getTime() - (minutesEast * 60000));
    }
    return Number.isNaN(result.getTime()) ? null : { date: result, local: (minutesEast === undefined) };
}

/**
 * Parses a value from the server into { date, local } (see toDate), or returns null if it cannot be parsed.
 */
export function parseServerDate(value) {
    let m = DATE_RE.exec(value);
    if (m) {
        return toDate(Number(m[1]), Number(m[2]), Number(m[3]),
                        Number(m[4] ?? 0), Number(m[5] ?? 0), Number(m[6] ?? 0),
                        milliseconds(m[7]),
                        m[8]);
    }

    m = TIME_RE.exec(value);
    if (m) {
        const today = new Date();
        return toDate(today.getFullYear(), today.getMonth() + 1, today.getDate(),
                        Number(m[1]), Number(m[2]), Number(m[3] ?? 0),
                        milliseconds(m[4]),
                        m[5]);
    }

    return null;
}

/**
 * Formats the date and time fields with a pattern.
 */
function format(year, month, day, hour, minute, second, pattern) {
    return pattern.replace(TOKEN_RE, (token) => {
        switch (token) {
            case 'yyyy': return String(year).padStart(4, '0');
            case 'MMM': return MONTHS[month];
            case 'MM': return pad2(month + 1);
            case 'dd': return pad2(day);
            case 'HH': return pad2(hour);
            case 'hh': return pad2((hour % 12) || 12);
            case 'mm': return pad2(minute);
            case 'ss': return pad2(second);
            case 'a': return (hour < 12) ? 'AM' : 'PM';
            default: return token;
        }
    });
}

/**
 * Formats a parsed value with a pattern, in the browser's time zone.
 * A local value shows the fields it was given. A value with an offset is moved into the browser's time zone by the
 * browser's offset in whole minutes, as Luxon does; so a date before standard time zones (local mean time, whose
 * offset has seconds) shows the same minute and second as before.
 */
export function formatDate(parsed, pattern) {
    const date = parsed.date;
    if (parsed.local) {
        return format(date.getFullYear(), date.getMonth(), date.getDate(),
                        date.getHours(), date.getMinutes(), date.getSeconds(),
                        pattern);
    }
    const shifted = new Date(date.getTime() - (date.getTimezoneOffset() * 60000));
    return format(shifted.getUTCFullYear(), shifted.getUTCMonth(), shifted.getUTCDate(),
                    shifted.getUTCHours(), shifted.getUTCMinutes(), shifted.getUTCSeconds(),
                    pattern);
}

/**
 * Parses a value from the server and formats it with the pattern.
 */
export function formatServerDate(value, pattern) {
    const parsed = parseServerDate(value);
    return parsed ? formatDate(parsed, pattern) : INVALID;
}
