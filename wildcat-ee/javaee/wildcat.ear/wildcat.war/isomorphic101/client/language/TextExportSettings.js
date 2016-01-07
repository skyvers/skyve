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
//> @class TextExportSettings
// Settings for use with +link{DataSource.recordsAsText()}.
// @treeLocation Client Reference/System
// @visibility external
//<
isc.ClassFactory.defineClass("TextExportSettings", "TextSettings");

isc.TextExportSettings.addClassProperties({
    //> @type ForceTextApproach
    // Approach to force a text value to be interpreted as text rather than parsed as a date, time
    // or other structured types, as can happen with Microsoft Excel.  For background information,
    // see +link{group:excelPasting}.
    //
    LEADING_SPACE: "leadingSpace",
    // @value "leadingSpace" a leading space character is added
    FORMULA: "formula"
    // @value "formula" text value is turned into a trivial Excel formula (eg "car" becomes ="car").
    // In Excel, this renders just the value "car" but editing the cell reveals the formula.
    // @visibility external
    //<
});

isc.TextExportSettings.addProperties({
    //> @attr textExportSettings.lineSeparator (String : "\n" : IR)
    // Separator between Records.  Default is a newline character ("\n").
    // @visibility external
    //<
    lineSeparator: "\n",

    //> @attr textExportSettings.quoteValues (Boolean : true : IR)
    // Whether to surround each value with quotes ("").
    // @visibility external
    //<
    quoteValues: true,

    //> @attr textExportSettings.nullValueText (String : "": IR)
    // Text to export for a field with a null value.  If this property is null, then
    // null fields will be assumed to have the default value for their field type.
    // @visibility external
    //<
    nullValueText: "",

    //> @attr textExportSettings.useDisplayValue (Boolean : false : IR)
    // Whether to convert each field's value to the corresponding display value
    // for export.  Default of false will directly export the field's value.
    // @visibility external
    //<
    useDisplayValue: false,

    //> @attr textExportSettings.forceText (ForceTextApproach : null : IR)
    // If set, all text fields will use the indicated +link{ForceTextApproach} unless they have
    // a specific setting for +link{dataSourceField.exportForceText}.
    // @visibility external
    //<
    forceText: null,

    //> @attr textExportSettings.dateFormat (DateDisplayFormat : null : IR)
    // Format to use when outputting date values.  Default is to use the format expected by
    // Microsoft Excel (eg 1-2-2011), which Excel will turn into a real date value (see
    // +link{group:excelPasting}).  The current month-day-year order as set by
    // +link{Date.setInputFormat()} will be used.
    // @visibility external
    //<
    dateFormat: null,

    //> @attr textExportSettings.dateTimeFormat (DateDisplayFormat : null : IR)
    // Format to use when outputting datetime values.  Default is to combine the configured date
    // and time formats with a space (" ").
    // @visibility external
    //<
    dateTimeFormat: null,

    //> @attr textExportSettings.timeFormat (TimeDisplayFormat : null : IR)
    // Format to use when outputting time values.  Default is 24 hour time.
    // @visibility external
    //<
    timeFormat: null
});
