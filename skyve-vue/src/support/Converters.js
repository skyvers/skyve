import { DateTime } from 'luxon';


const COMMON = {
    zeroDecimalPlaces: _createNumberFormatter({ scale: 0 }),
    oneDecimalPlaces: _createNumberFormatter({ scale: 1 }),
    twoDecimalPlaces: _createNumberFormatter({ scale: 2 }),
};

/**
 * Creates a function that formats a date or time value from the server (ISO 8601) as the Java converter of
 * the same name does: the format is the converter's pattern (Luxon and Java's SimpleDateFormat read the
 * letters used here the same way), in English, as the Java converters format in Locale.ENGLISH
 * (so 'Sep' and 'PM' whatever the browser's language).
 */
function _createDateFormatter(format) {

    return (stringValue) => {

        if (stringValue.trim() == ":") {
            return "";
        }

        const dt = DateTime.fromISO(stringValue, { locale: 'en-US' });
        return dt.toFormat(format);
    };
}

/**
 * Creates a function which can be used to format display values in
 * a row.
 * 
 * Returns a lambda with signature: (number) => string
 */
function _createNumberFormatter({ scale, style = 'decimal' }) {

    const numberFormat = new Intl.NumberFormat(undefined, {
        style,
        minimumFractionDigits: scale,
        maximumFractionDigits: scale
    });

    return (numValue) => {

        if (!Number.isFinite(numValue)) {
            console.warn('Bad value supplied to number formatter', numValue, { scale, style });
            return numValue;
        }

        return numberFormat.format(numValue);
    };
}

/**
 * Decimal5TimeDuration formatter function
 * 
 * Modifies the given field value in the supplied row
 */
function Decimal5TimeDuration(numValue) {

    let newVal = COMMON.twoDecimalPlaces(numValue);

    // Decimal seperator is locale dependent
    const decSep = newVal.at(-3);
    return newVal.replace(decSep, ':');
}

function Decimal2DollarsAndCentsAbsolute(numValue) {

    if (numValue < 0) {
        numValue = -1 * numValue;
    }

    return COMMON.twoDecimalPlaces(numValue);
}

/**
 * This one works a bit differently to the other percentage
 * converters, no multiplication here (ie: 45 => '45%')
 */
function SimplePercentage(numValue) {

    return COMMON.zeroDecimalPlaces(numValue) + '%';
}


const Converters = {

    // date type
    DD_MM_YYYY: _createDateFormatter('dd/MM/yyyy'),  // 20/05/2024
    DD_MMM_YYYY: _createDateFormatter('dd-MMM-yyyy'),  // 20-May-2024
    MM_DD_YYYY: _createDateFormatter('MM/dd/yyyy'),  // 05/20/2024
    MMM_DD_YYYY: _createDateFormatter('MMM-dd-yyyy'),  // May-20-2024
    YYYY_MM_DD: _createDateFormatter('yyyy/MM/dd'),  // 2024/05/20

    // dateTime
    DD_MM_YYYY_HH_MI: _createDateFormatter('dd/MM/yyyy hh:mm a'),  // 20/05/2024 02:47 PM
    DD_MM_YYYY_HH24_MI: _createDateFormatter('dd/MM/yyyy HH:mm'),  // 20/05/2024 14:47
    DD_MM_YYYY_DateTime: _createDateFormatter('dd/MM/yyyy'),  // 20/05/2024
    DD_MMM_YYYY_HH_MI: _createDateFormatter('dd-MMM-yyyy hh:mm a'),  // 20-May-2024 02:47 PM
    DD_MMM_YYYY_HH24_MI: _createDateFormatter('dd-MMM-yyyy HH:mm'),  // 20-May-2024 14:47
    DD_MMM_YYYY_DateTime: _createDateFormatter('dd-MMM-yyyy'),  // 20-May-2024
    MM_DD_YYYY_HH_MI: _createDateFormatter('MM/dd/yyyy hh:mm a'),  // 05/20/2024 02:47 PM
    MM_DD_YYYY_HH24_MI: _createDateFormatter('MM/dd/yyyy HH:mm'),  // 05/20/2024 14:47
    MM_DD_YYYY_DateTime: _createDateFormatter('MM/dd/yyyy'),  // 05/20/2024
    MMM_DD_YYYY_HH_MI: _createDateFormatter('MMM-dd-yyyy hh:mm a'),  // May-20-2024 02:47 PM
    MMM_DD_YYYY_HH24_MI: _createDateFormatter('MMM-dd-yyyy HH:mm'),  // May-20-2024 14:47
    MMM_DD_YYYY_DateTime: _createDateFormatter('MMM-dd-yyyy'),  // May-20-2024
    YYYY_MM_DD_HH_MI: _createDateFormatter('yyyy/MM/dd hh:mm a'),  // 2024/05/20 02:47 PM
    YYYY_MM_DD_HH24_MI: _createDateFormatter('yyyy/MM/dd HH:mm'),  // 2024/05/20 14:47
    YYYY_MM_DD_DateTime: _createDateFormatter('yyyy/MM/dd'),  // 2024/05/20

    // timestamp
    DD_MM_YYYY_HH_MI_SS: _createDateFormatter('dd/MM/yyyy hh:mm:ss a'),  // 20/05/2024 04:07:25 PM
    DD_MM_YYYY_HH24_MI_SS: _createDateFormatter('dd/MM/yyyy HH:mm:ss'),  // 20/05/2024 16:07:25
    DD_MM_YYYY_Timestamp: _createDateFormatter('dd/MM/yyyy'),  // 20/05/2024
    DD_MMM_YYYY_HH_MI_SS: _createDateFormatter('dd-MMM-yyyy hh:mm:ss a'),  // 20-May-2024 04:07:25 PM
    DD_MMM_YYYY_HH24_MI_SS: _createDateFormatter('dd-MMM-yyyy HH:mm:ss'),  // 20-May-2024 16:07:25
    DD_MMM_YYYY_Timestamp: _createDateFormatter('dd-MMM-yyyy'),  // 20-May-2024
    MM_DD_YYYY_HH_MI_SS: _createDateFormatter('MM/dd/yyyy hh:mm:ss a'),  // 05/20/2024 04:07:25 PM
    MM_DD_YYYY_HH24_MI_SS: _createDateFormatter('MM/dd/yyyy HH:mm:ss'),  // 05/20/2024 16:07:25
    MM_DD_YYYY_Timestamp: _createDateFormatter('MM/dd/yyyy'),  // 05/20/2024
    MMM_DD_YYYY_HH_MI_SS: _createDateFormatter('MMM-dd-yyyy hh:mm:ss a'),  // May-20-2024 04:07:25 PM
    MMM_DD_YYYY_HH24_MI_SS: _createDateFormatter('MMM-dd-yyyy HH:mm:ss'),  // May-20-2024 16:07:25
    MMM_DD_YYYY_Timestamp: _createDateFormatter('MMM-dd-yyyy'),  // May-20-2024
    YYYY_MM_DD_HH_MI_SS: _createDateFormatter('yyyy/MM/dd hh:mm:ss a'),  // 2024/05/20 04:07:25 PM
    YYYY_MM_DD_HH24_MI_SS: _createDateFormatter('yyyy/MM/dd HH:mm:ss'),  // 2024/05/20 16:07:25
    YYYY_MM_DD_Timestamp: _createDateFormatter('yyyy/MM/dd'),  // 2024/05/20

    // time
    HH_MI: _createDateFormatter('hh:mm a'),  // 11:59 PM
    HH24_MI: _createDateFormatter('HH:mm'),  // 23:59
    HH_MI_SS: _createDateFormatter('hh:mm:ss a'),  // 11:59:12 PM
    HH24_MI_SS: _createDateFormatter('HH:mm:ss'),  // 23:59:12

    // decimal10
    Decimal10Integer: COMMON.zeroDecimalPlaces,  // 9,510
    Decimal10IntegerPercentage: _createNumberFormatter({ scale: 0, style: 'percent' }),  // 951,001%
    Decimal10DollarsAndCents: COMMON.twoDecimalPlaces,  // 9,510.01
    Decimal10OneDecimalPlace: COMMON.oneDecimalPlaces,  // 9,510.0
    Decimal10TwoDecimalPlaces: COMMON.twoDecimalPlaces,  // 10.01
    Decimal10TwoDecimalPlacesPercentage: _createNumberFormatter({ scale: 2, style: 'percent' }),  // 951,001.23%

    // decimal5
    Decimal5Integer: COMMON.zeroDecimalPlaces,  // 9,510
    Decimal5IntegerPercentage: _createNumberFormatter({ scale: 0, style: 'percent' }),  // 951,001%
    Decimal5DollarsAndCents: COMMON.twoDecimalPlaces,  // 9,510.01
    Decimal5TimeDuration,  // 9510:00
    Decimal5OneDecimalPlace: COMMON.oneDecimalPlaces,  // 9,510.0
    Decimal5TwoDecimalPlaces: COMMON.twoDecimalPlaces,  // 9,510.01
    Decimal5TwoDecimalPlacesPercentage: _createNumberFormatter({ scale: 2, style: 'percent' }),  // 951,001.23%

    // decimal2
    Decimal2DollarsAndCents: COMMON.twoDecimalPlaces, // 9,510.01
    Decimal2DollarsAndCentsAbsolute, // 9,510.01
    Decimal2Integer: COMMON.zeroDecimalPlaces, // 9,510
    Decimal2IntegerPercentage: _createNumberFormatter({ scale: 0, style: 'percent' }), // 951,001%
    Decimal2OneDecimalPlace: COMMON.oneDecimalPlaces, // 9,510.0

    // integer
    SimplePercentage, // 14999%
    IntegerSeparator: COMMON.zeroDecimalPlaces, // 14,999

    // longInteger
    LongIntegerSeparator: COMMON.zeroDecimalPlaces, // 14,999
};


/**
 * The filter calendar of each date and time converter: the PrimeVue Calendar formats that make a column's
 * filter take and show its values as the column's cells show them (the Java converter's format), so that a
 * user can type what a cell shows.
 * - dateFormat: the date, in PrimeVue's letters (dd day, mm month, M short month name, yy 4 digit year);
 *   none for a time.
 * - hourFormat: '24' (HH) or '12' (hh, then a space and AM or PM - see the locale in main.js); none for a
 *   date without a time. PrimeVue writes a time after the date and a space.
 * - showSeconds: the time has seconds.
 * ConverterRegistrationTest (skyve-core) checks each one against its Java converter.
 */
const CALENDARS = {

    // date
    DD_MM_YYYY: { dateFormat: 'dd/mm/yy' },
    DD_MMM_YYYY: { dateFormat: 'dd-M-yy' },
    MM_DD_YYYY: { dateFormat: 'mm/dd/yy' },
    MMM_DD_YYYY: { dateFormat: 'M-dd-yy' },
    YYYY_MM_DD: { dateFormat: 'yy/mm/dd' },

    // dateTime
    DD_MM_YYYY_HH_MI: { dateFormat: 'dd/mm/yy', hourFormat: '12' },
    DD_MM_YYYY_HH24_MI: { dateFormat: 'dd/mm/yy', hourFormat: '24' },
    DD_MM_YYYY_DateTime: { dateFormat: 'dd/mm/yy' },
    DD_MMM_YYYY_HH_MI: { dateFormat: 'dd-M-yy', hourFormat: '12' },
    DD_MMM_YYYY_HH24_MI: { dateFormat: 'dd-M-yy', hourFormat: '24' },
    DD_MMM_YYYY_DateTime: { dateFormat: 'dd-M-yy' },
    MM_DD_YYYY_HH_MI: { dateFormat: 'mm/dd/yy', hourFormat: '12' },
    MM_DD_YYYY_HH24_MI: { dateFormat: 'mm/dd/yy', hourFormat: '24' },
    MM_DD_YYYY_DateTime: { dateFormat: 'mm/dd/yy' },
    MMM_DD_YYYY_HH_MI: { dateFormat: 'M-dd-yy', hourFormat: '12' },
    MMM_DD_YYYY_HH24_MI: { dateFormat: 'M-dd-yy', hourFormat: '24' },
    MMM_DD_YYYY_DateTime: { dateFormat: 'M-dd-yy' },
    YYYY_MM_DD_HH_MI: { dateFormat: 'yy/mm/dd', hourFormat: '12' },
    YYYY_MM_DD_HH24_MI: { dateFormat: 'yy/mm/dd', hourFormat: '24' },
    YYYY_MM_DD_DateTime: { dateFormat: 'yy/mm/dd' },

    // timestamp
    DD_MM_YYYY_HH_MI_SS: { dateFormat: 'dd/mm/yy', hourFormat: '12', showSeconds: true },
    DD_MM_YYYY_HH24_MI_SS: { dateFormat: 'dd/mm/yy', hourFormat: '24', showSeconds: true },
    DD_MM_YYYY_Timestamp: { dateFormat: 'dd/mm/yy' },
    DD_MMM_YYYY_HH_MI_SS: { dateFormat: 'dd-M-yy', hourFormat: '12', showSeconds: true },
    DD_MMM_YYYY_HH24_MI_SS: { dateFormat: 'dd-M-yy', hourFormat: '24', showSeconds: true },
    DD_MMM_YYYY_Timestamp: { dateFormat: 'dd-M-yy' },
    MM_DD_YYYY_HH_MI_SS: { dateFormat: 'mm/dd/yy', hourFormat: '12', showSeconds: true },
    MM_DD_YYYY_HH24_MI_SS: { dateFormat: 'mm/dd/yy', hourFormat: '24', showSeconds: true },
    MM_DD_YYYY_Timestamp: { dateFormat: 'mm/dd/yy' },
    MMM_DD_YYYY_HH_MI_SS: { dateFormat: 'M-dd-yy', hourFormat: '12', showSeconds: true },
    MMM_DD_YYYY_HH24_MI_SS: { dateFormat: 'M-dd-yy', hourFormat: '24', showSeconds: true },
    MMM_DD_YYYY_Timestamp: { dateFormat: 'M-dd-yy' },
    YYYY_MM_DD_HH_MI_SS: { dateFormat: 'yy/mm/dd', hourFormat: '12', showSeconds: true },
    YYYY_MM_DD_HH24_MI_SS: { dateFormat: 'yy/mm/dd', hourFormat: '24', showSeconds: true },
    YYYY_MM_DD_Timestamp: { dateFormat: 'yy/mm/dd' },

    // time
    HH_MI: { hourFormat: '12' },
    HH24_MI: { hourFormat: '24' },
    HH_MI_SS: { hourFormat: '12', showSeconds: true },
    HH24_MI_SS: { hourFormat: '24', showSeconds: true },
};

/**
 * Returns the props of the filter calendar for a column with the converter (see CALENDARS):
 * { dateFormat, showTime, hourFormat, showSeconds }; or null if the converter is not a date or time converter.
 */
export function calendarProps(converterName) {

    const calendar = CALENDARS[converterName];
    if (!calendar) {
        return null;
    }

    return {
        dateFormat: calendar.dateFormat,
        showTime: !!calendar.dateFormat && !!calendar.hourFormat,
        hourFormat: calendar.hourFormat ?? '24',
        showSeconds: !!calendar.showSeconds
    };
}

export function applyConverters(row, columnDefinitionMap) {

    for (let field in row) {

        const columnDef = columnDefinitionMap.get(field);
        if (!columnDef) continue;

        // If a display value has been given, use that in preference to any converter defined
        const displayKey = `_display_${field}`;
        if (row[displayKey]) {
            row[field] = row[displayKey];
            continue;
        }

        // If the value is null, skip it
        const oldValue = row[field];
        if (oldValue == null || oldValue == undefined) {
            continue;
        }

        // Enumerations and constant domains display the description of the code
        if (columnDef.enumValues) {
            const code = String(oldValue);
            const enumValue = columnDef.enumValues.find(ev => ev.value === code);
            if (enumValue) {
                row[field] = enumValue.label;
            }
            continue;
        }

        // Otherwise try to find the right converter
        if (columnDef.converter) {
            const fieldConverter = Converters[columnDef.converter];

            if (!fieldConverter) {
                console.error(`${field} needs converter ${columnDef.converter} which is not available`, columnDef);
                continue;
            }

            // Modify the row using the converter/mutator
            const newValue = fieldConverter(oldValue);
            row[field] = newValue;
        }
    }
}

