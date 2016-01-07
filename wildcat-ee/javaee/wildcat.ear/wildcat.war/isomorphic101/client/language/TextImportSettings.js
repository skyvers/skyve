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
//> @class TextImportSettings
// Settings for use with +link{DataSource.recordsFromText()}.
// @treeLocation Client Reference/System
// @visibility external
//<
isc.ClassFactory.defineClass("TextImportSettings", "TextSettings");

isc.TextImportSettings.addClassProperties({

    // This internal property configures the parser to accept JSON object records.
    // The header items are used directly as field names, without checking or 
    // matching against DataSource titles or fields, and the records are returned
    // without converting them to objects via +link{DataSource.validateJSONRecord()}.
    _importAsJsonObjects : false
});

isc.TextImportSettings.addProperties({

    //> @attr textImportSettings.hasHeaderLine (Boolean : false : IR)
    // If set to true, the data is assumed to have a header line that lists titles for each field,
    // which should be parsed.  
    // <P>
    // <code>recordsFromText</code> will then try to find a same-named
    // DataSourceField by checking parsed titles against both +link{DataSourceField.title} and
    // +link{DataSourceField.name} (titles first), doing a case-insensitive comparison with any
    // leading or trailing whitespace removed from the title.  If no field matches, data will
    // appear in the returned Records under the exact title parsed from the header line.
    // <P>
    // If this approach will not find appropriate DataSourceFields, parse the header line before
    // calling <code>recordsFromText()</code>, and provide the list of field names to use when
    // parsing data as +link{TextSettings.fieldList}.
    // @visibility external
    //<
    hasHeaderLine: false,

    //> @attr textImportSettings.trim (boolean : false : IR)
    // If set to true, calls +link{String.trim} to remove whitespace before and after
    // the value before removing any quotes.
    //<
    trim: false
});

isc.TextImportSettings.addMethods({

    getSpecialCharactersRegExp : function (flags) {
        var expression = this.lineSeparator ? "\\" + this.lineSeparator : "(\r)?\n";
        expression += "|\\" + this.getEscapingModeEscapeChar() + "\"";
        expression += "|\\" + this.fieldSeparator;
        expression += "|\\\"";
        return new RegExp(expression, flags);
    },

    removeUnescapedQuotes : function (value) {
        var escapeChar = this.getEscapingModeEscapeChar();

        var reEscapedQuotes = new RegExp("\\" + escapeChar + "\"", "g");
        
        var reUnEscapedQuotes = escapeChar == "\"" ? 
            new RegExp("([^\"]|^)\"(?!\")", "g") :
            new RegExp("\"\"|([^\\" + escapeChar + "]|^)\"", "g");

        return value.replace(reUnEscapedQuotes, "$1").replace(reEscapedQuotes, "\"");
    },

    addFinalLineSeparatorIfNotPresent : function (text) {
        var lineSeparator = this.lineSeparator || "\n";
        var regExp = new RegExp(lineSeparator + "$");
        if (!text.match(regExp, text)) text += lineSeparator;
        return text;
    },

    parseTextAndApplyFunctions : function (text, fieldFunction, lineFunction) {

        var fieldSeparator = this.fieldSeparator;
        var escapedQuote = this.getEscapingModeEscapeChar() + "\"";
        var boundaryRegExp = this.getSpecialCharactersRegExp();

        var startPos = 0, quoted = false;;

        for (var hit, target = text, offset = 0; null != (hit = target.match(boundaryRegExp)); 
             target = target.substring(increment), offset += increment) {

            var increment = hit[0].length + hit.index;
            switch(hit[0]) {
            case escapedQuote:
                break;
            case "\"":
                quoted = !quoted;
                break;
            default:
                if (quoted) continue;

                var value = text.substring(startPos, offset + hit.index);
                if (this.trim) value = value.trim();
                // disambiguate single set of double quotes as empty value rather than
                // one escaped quote when TextExportSettings escaping mode is DOUBLE
                if (value == "\"\"" && this.escapingMode == isc.TextSettings.DOUBLE) {
                    value = "";
                }
                value = this.removeUnescapedQuotes(value);
                startPos = offset + increment;

                fieldFunction(value);

                if (hit[0] != fieldSeparator) {
                    if (lineFunction()) target = "";
                }
                break;
            }
        }
        return text.substring(startPos);
    }
});
