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





//> @type ListStyleType
// The style of list item marker for a list.
// @value "disc" A filled, black dot (&bull;)
// @value "circle" An unfilled circle (&#9702;)
// @value "square" A filled, black square (&#9632;)
// @value "decimal" Numbers (1., 2., 3., etc.)
// @value "upper-roman" Uppercase Roman numerals (I., II., III., IV., etc.)
// @value "lower-roman" Lowercase Roman numerals (i., ii., iii., iv., etc.)
// @value "upper-alpha" Uppercase letters (A., B., C., etc.)
// @value "lower-alpha" Lowercase letters (a., b., c., etc.)
// @value "custom-image" An image used in place of a marker.
// @visibility external
//<

//> @object ListProperties
// Configuration of an HTML list in a +link{RichTextEditor}.
// @treeLocation Client Reference/Foundation/RichTextEditor
// @visibility external
//<

//> @attr listProperties.style (ListStyleType : null : IR)
// The style of list item marker. If "custom-image", the +link{ListProperties.image,image}
// should be specified.
// @visibility external
//<

//> @attr listProperties.image (SCImgURL : "[SKIN]/RichTextEditor/bullet_blue.png" : IR)
// When the list item marker +link{ListProperties.style,style} is "custom-image", the image
// to use for the markers.
// @visibility external
//<

//> @attr listProperties.startNumber (int : 1 : IR)
// For ordered lists, the number to start the first item with. Must be non-negative.
// @visibility external
//<

isc.defineClass("ListPropertiesSampleTile", "StatefulCanvas").addProperties({
    baseStyle: "simpleTile",
    overflow: "hidden",
    showRollOver: true,
    redrawOnStateChange: true,
    _redrawWithParent: true

    //> @attr listPropertiesSampleTile.listProperties (ListProperties : null : IR)
    // The HTML list configuration to depict by this +link{ListPropertiesPane.sampleTile,sampleTile}.
    // This object must be treated as read-only and not modified.
    //<
});

isc.ListPropertiesSampleTile.addMethods({
    initWidget : function () {
        this.Super("initWidget", arguments);
        this._canonicalProperties = isc.ListPropertiesPane.getCanonicalListProperties(this.listProperties);
        this._itemTextPlaceholder = this.imgHTML(isc.Canvas._blankImgURL, 40, 3, "' style='background-color:#999;vertical-align:middle");
    },

    getInnerHTML : function () {
        var listProperties = this._canonicalProperties,
            style = listProperties.style,
            isUnordered = isc.ListPropertiesPane.getListType(listProperties) == "unordered",
            tagName = isUnordered ? "ul" : "ol",
            startNumber = this.creator.listProperties.startNumber;
        if (startNumber == null) startNumber = this.creator.startNumberField.getValue();

        var html = "<table role='presentation' aria-hidden='true' border='0' cellpadding='0' cellspacing='0' style='width:100%;height:100%;table-layout:fixed'>" +
        "<colgroup><col width='100%'/></colgroup>" +
        "<tr>" +
        "<td class='normal' style='height:100%' valign='middle' align='center'>" +
        "<" + tagName;

        if (!isUnordered) {
            var minStartNumber = style == "decimal" ? 0 : 1;
            html += " start='" + Math.max(minStartNumber, startNumber) + "'";
        }

        html += " style='margin:0;padding:0 0 0 30px";

        if (style == "custom-image") {
            var src = listProperties.image;
            html += ";list-style-image:url(" + this.getImgURL(src) + ")";
        } else {
            html += ";list-style-type:" + style;
        }

        html += ";font-size:14px;line-height:22px;text-align:left'>" +
        "<li>" + this._itemTextPlaceholder + "</li>" +
        "<li>" + this._itemTextPlaceholder + "</li>" +
        "<li>" + this._itemTextPlaceholder + "</li>" +
        "</" + tagName + ">" +
        "</td>" +
        "</tr>" +
        "</table>";
        return html;
    },

    click : function () {
        this.creator.setSelectedStyle(this.listProperties.style, this.listProperties.image, true);
    }
});

//> @class ListPropertiesPane
// Pane containing controls for editing the style of HTML lists in a +link{RichTextEditor}.
// <p>
// Cannot be directly used; shown in documentation only for skinning purposes.
// @treeLocation Client Reference/Foundation/RichTextEditor
// @visibility external
//<
isc.defineClass("ListPropertiesPane", "Layout").addClassProperties({
    defaultSamplesList: [
    // unordered
    {
        style: "disc"
    }, {
        style: "circle"
    }, {
        style: "square"
    },
    // ordered
    {
        style: "decimal"
    }, {
        style: "upper-roman"
    }, {
        style: "lower-roman"
    }, {
        style: "upper-alpha"
    }, {
        style: "lower-alpha"
    },
    // custom image (unordered)
    {
        style: "custom-image",
        image: "[SKIN]/RichTextEditor/bullet_blue.png"
    }, {
        style: "custom-image",
        image: "[SKIN]/RichTextEditor/bullet_green.png"
    }, {
        style: "custom-image",
        image: "[SKIN]/RichTextEditor/bullet_red.png"
    }, {
        style: "custom-image",
        image: "[SKIN]/RichTextEditor/bullet_purple.png"
    }],

    // For mapping CSS `list-style-type' aliases
    listStyleTypeMap: {
        "lower-latin": "lower-alpha",
        "upper-latin": "upper-alpha"
    },

    getCanonicalImageURL : function (image) {
        if (!image) return "[SKIN]/RichTextEditor/bullet_blue.png";
        var pos = image.indexOf("/images/RichTextEditor/");
        if (pos >= 0) {
            return "[SKIN]/" + image.substring(pos + 8);
        }
        return image;
    },

    getCanonicalListProperties : function (listProperties) {
        if (listProperties == null) return { _canonical: true, style: "disc" };

        // If the given `listProperties' object is already in canonical form, return it.
        if (listProperties._canonical) return listProperties;

        var returnVal = {
            _canonical: true,
            style: this.getCanonicalListStyleType(listProperties.style)
        };
        if (this.getListType(listProperties) == "ordered") {
            if ("startNumber" in listProperties) returnVal.startNumber = listProperties.startNumber << 0;
        } else {
            if (returnVal.style == "custom-image") {
                returnVal.image = this.getCanonicalImageURL(listProperties.image);
            }
        }
        return returnVal;
    },

    getCanonicalListStyleType : function (style) {
        if (!style) return "disc";
        if (this.listStyleTypeMap.hasOwnProperty(style)) return this.listStyleTypeMap[style];
        return style;
    },

    getListType : function (listProperties) {
        if (listProperties == null) return "unordered"; // The default list configuration is a
                                                        // bulleted list.

        var style = this.getCanonicalListStyleType(listProperties.style);
        return (style == "none" ||
                style == "disc" ||
                style == "circle" ||
                style == "square" ||
                style == "custom-image" ||

                // CSS3 List predefined counter styles
                // http://www.w3.org/TR/css3-lists/#ua-stylesheet
                style == "box" ||
                style == "check" ||
                style == "diamond" ||
                style == "dash"
                ? "unordered"
                : "ordered");
    },

    convertFromExcelLetters : function (letters) {
        if (!letters) return null;

        var charCode = letters.charCodeAt(letters.length - 1),
            returnVal = charCode - (charCode >= 97 ? 96 : 64),
            pow = 26;
        for (var ri = letters.length - 1; ri > 0; --ri, pow *= 26) {
            charCode = letters.charCodeAt(ri - 1);
            var r = charCode - (charCode >= 97 ? 96 : 64);
            returnVal += r * pow;
        }
        return returnVal;
    },

    _romanNumeralsData: [
        1000, "M",
        900, "CM",
        500, "D",
        400, "CD",
        100, "C",
        90, "XC",
        50, "L",
        40, "XL",
        10, "X",
        9, "IX",
        5, "V",
        4, "IV",
        1, "I"
    ],
    convertFromRomanNumerals : function (numerals) {
        var romanNumeralsData = this._romanNumeralsData;

        numerals = numerals.toUpperCase();

        var returnVal = 0,
            parsePos = 0;
        for (var p = 0; p < romanNumeralsData.length; p += 2) {
            var s = romanNumeralsData[p + 1];
            while (numerals.indexOf(s, parsePos) == parsePos) {
                returnVal += romanNumeralsData[p];
                parsePos += s.length;
            }
        }
        return returnVal;
    },

    // http://stackoverflow.com/questions/181596/how-to-convert-a-column-number-eg-127-into-an-excel-column-eg-aa
    convertToExcelLetters : function (num, lowercase) {
        if (!isc.isA.Number(num)) return null;

        var baseCode = lowercase ? 97 : 65;
        if (num <= 26) {
            return String.fromCharCode(baseCode + num - 1);
        }

        var charCodes = [];
        while (num > 26) {
            var r = (num - 1) % 26
            charCodes[charCodes.length] = baseCode + r;
            num = ((num - r) / 26) << 0;
        }
        charCodes[charCodes.length] = baseCode + num - 1;
        charCodes.reverse();
        return String.fromCharCode.apply(String, charCodes);
    },

    // http://stackoverflow.com/questions/7040289/converting-integers-to-roman-numerals
    convertToRomanNumerals : function (num, lowercase) {
        if (!isc.isA.Number(num)) return null;
        if (num > 3999) return String(num);

        var romanNumeralsData = this._romanNumeralsData;

        var numerals = [];
        for (var p = 0; p < romanNumeralsData.length; p += 2) {
            var b = romanNumeralsData[p];
            while (num >= b) {
                numerals[numerals.length] = romanNumeralsData[p + 1];
                num -= b;
            }
        }

        var str = numerals.join(isc.emptyString);
        if (lowercase) str = str.toLowerCase();
        return str;
    }
});

isc.ListPropertiesPane.addProperties({
    vertical: true,
    width: 400,
    overflow: "visible",

    //> @attr listPropertiesPane.samplesList (Array of ListProperties : null : IR)
    // A list of +link{ListProperties} to display a +link{ListPropertiesPane.sampleTile,sampleTile} for.
    //<

    //> @attr listPropertiesPane.listProperties (ListProperties : null : IRW)
    // The properties corresponding to the currently-selected list configuration.
    // @visibility external
    //<

    //> @attr listPropertiesPane.sampleTileLayout (AutoChild TileLayout : null : R)
    // Shows available bullet options as a series of tiles.
    // @visibility external
    //<
    sampleTileLayoutDefaults: {
        _constructor: "TileLayout",
        width: 400,
        height: 260,
        tileWidth: 80,
        tileHeight: 80,
        tileMargin: 5
    },

    //> @attr listPropertiesPane.sampleTile (MultiAutoChild Canvas : null : R)
    // Tile used to demonstrate each bullet style.
    // @visibility external
    //<
    sampleTileDefaults: {
        _constructor: "ListPropertiesSampleTile"
    },

    //> @attr listPropertiesPane.startNumberForm (AutoChild DynamicForm : null : R)
    // Form used to show the +link{ListPropertiesPane.startNumberField,startNumberField} for
    // configuring the starting value of a list.
    // @visibility external
    //<
    startNumberFormDefaults: {
        _constructor: "DynamicForm",
        width: "100%",
        colWidths: [ 90, "*" ],
        numCols: 2
    },

    //> @attr listPropertiesPane.startNumberFieldTitle (String : "Start at" : IR)
    // The +link{FormItem.title,title} of the +link{ListPropertiesPane.startNumberField,startNumberField}.
    // @visibility external
    // @group i18nMessages
    //<
    startNumberFieldTitle: "Start at",

    //> @attr listPropertiesPane.startNumberField (AutoChild SpinnerItem : null : R)
    // +link{SpinnerItem} used to modify the starting value of the list.
    // <p>
    // +link{ListPropertiesPane.startNumberFieldTitle,startNumberFieldTitle} is a
    // +link{group:autoChildUsage,passthrough} for the field's +link{FormItem.title,title}.
    // @visibility external
    //<
    startNumberFieldDefaults: {
        editorType: "SpinnerItem",
        defaultValue: 1,
        step: 1,
        width: 75,
        change : function (form, self, value, oldValue) {
            this.form.creator.setStartNumber(value, true);
        },
        getPreviousValue : function (currentValue, step) {
            var listProperties = isc.ListPropertiesPane.getCanonicalListProperties(this.form.creator.listProperties),
                isUnordered = isc.ListPropertiesPane.getListType(listProperties) == "unordered",
                style = listProperties.style,
                min = (isUnordered || style == "decimal" ? 0 : 1);
            return Math.max(min, currentValue + step);
        },
        getNextValue : function (currentValue, step) {
            var listProperties = isc.ListPropertiesPane.getCanonicalListProperties(this.form.creator.listProperties),
                style = listProperties.style;
            if (style == "upper-roman" || style == "lower-roman") {
                return Math.min(currentValue + step, 3999 - 3);
            }
            return currentValue + step;
        },
        _parseDisplayValue : function (displayValue) {
            if (!isc.isA.String(displayValue) ||
                (displayValue = displayValue.trim()) == "")
            {
                return this.Super("_parseDisplayValue", arguments);
            }

            var periodPos = displayValue.indexOf('.');
            if (periodPos >= 0) {
                displayValue = displayValue.substring(0, periodPos).trim();
            }

            var listProperties = isc.ListPropertiesPane.getCanonicalListProperties(this.form.creator.listProperties),
                style = listProperties.style;
            if (style == "upper-roman" || style == "lower-roman") {
                return isc.ListPropertiesPane.convertFromRomanNumerals(displayValue);
            } else if (style == "upper-alpha" || style == "lower-alpha") {
                return isc.ListPropertiesPane.convertFromExcelLetters(displayValue);
            }

            return this.Super("_parseDisplayValue", arguments);
        },
        formatEditorValue : function (value, record, form, self) {
            var listProperties = isc.ListPropertiesPane.getCanonicalListProperties(this.form.creator.listProperties),
                style = listProperties.style;
            if (style == "upper-roman") {
                return isc.ListPropertiesPane.convertToRomanNumerals(value, false) + ".";
            } else if (style == "lower-roman") {
                return isc.ListPropertiesPane.convertToRomanNumerals(value, true) + ".";
            } else if (style == "upper-alpha") {
                return isc.ListPropertiesPane.convertToExcelLetters(value, false) + ".";
            } else if (style == "lower-alpha") {
                return isc.ListPropertiesPane.convertToExcelLetters(value, true) + ".";
            }
            return String(value) + ".";
        }
    }

});

isc.ListPropertiesPane.addMethods({

initWidget : function () {
    this.Super("initWidget", arguments);
    var listProperties = this.listProperties = this.listProperties || {};

    var samplesList = this.samplesList;
    if (samplesList == null) {
        this.samplesList = samplesList = isc.ListPropertiesPane.defaultSamplesList.duplicate();
    }

    var sampleTiles = [];
    for (var i = 0, len = samplesList.length; i < len; ++i) {
        sampleTiles[i] = this.createAutoChild("sampleTile", {
            listProperties: samplesList[i]
        });
    }

    this.addAutoChild("sampleTileLayout", {
        tiles: sampleTiles
    });
    // Try to find the sample tile corresponding to the initial list properties and select it.
    var selectedTile = this._findCorrespondingSampleTile(listProperties);
    if (selectedTile != null) {
        selectedTile.setSelected(true);
        this._selectedTile = selectedTile;
    }

    var startNumberField = isc.addProperties({}, this.startNumberFieldDefaults, this.startNumberFieldProperties, {
        name: "startNumber",
        title: this.startNumberFieldTitle,
        value: listProperties.startNumber,
        disabled: isc.ListPropertiesPane.getListType(listProperties) == "unordered"
    });
    this.addAutoChild("startNumberForm", {
        items: [ startNumberField ]
    });
    startNumberField = this.startNumberField = this.startNumberForm.getField("startNumber");
    listProperties.startNumber = startNumberField.getValue();
},

setListProperties : function (listProperties) {
    listProperties = this.listProperties = isc.ListPropertiesPane.getCanonicalListProperties(listProperties);

    this.setStartNumber(listProperties.startNumber);
    this.setSelectedStyle(listProperties.style, listProperties.image);
},

//> @method listPropertiesPane.setSelectedStyle()
// @param style (ListStyleType) the new marker style
// @param [image] (SCImgURL) when style is "custom-image", the marker +link{ListProperties.image,image}
//<
setSelectedStyle : function (style, image, fireChangeEvent) {
    if (this._selectedTile != null) {
        this._selectedTile.setSelected(false);
        this._selectedTile = null;
    }

    var listProperties = this.listProperties;
    listProperties._canonical = false;
    listProperties.style = style;
    listProperties.image = image;
    // In case we're switching from an unordered to an ordered style, the listProperties'
    // startNumber needs to be initialized with the current startNumberField value.
    if (listProperties.startNumber == null) listProperties.startNumber = this.startNumberField.getValue();
    listProperties = this.listProperties = isc.ListPropertiesPane.getCanonicalListProperties(listProperties);

    var oldStartNumber = this.startNumberField.getValue();
    this.startNumberField.setValue(oldStartNumber);
    var newStartValue = this.startNumberField.getValue();
    if (oldStartNumber != newStartValue) {
        this.setStartNumber(newStartValue);
    }

    // Try to find the corresponding sample tile and select it.
    var selectedTile = this._findCorrespondingSampleTile(listProperties);
    if (selectedTile != null) {
        selectedTile.setSelected(true);
        this._selectedTile = selectedTile;
    }

    // If the new style is for an unordered list, disable the startNumberField. Otherwise
    // enable it.
    var isUnordered = isc.ListPropertiesPane.getListType(listProperties) == "unordered";
    this.startNumberField.setDisabled(isUnordered);

    if (fireChangeEvent && this.listPropertiesChanged) this.listPropertiesChanged(listProperties);
},

setStartNumber : function (startNumber, fireChangeEvent) {
    var listProperties = this.listProperties;

    startNumber = (startNumber != null ? startNumber << 0 : this.startNumberField.getValue());
    this.startNumberField.setValue(startNumber);
    listProperties.startNumber = this.startNumberField.getValue();

    // Redraw all sample tiles because the startNumber has changed and the tiles may need to
    // update their display accordingly.
    this.sampleTileLayout.markForRedraw();

    if (fireChangeEvent && this.listPropertiesChanged) this.listPropertiesChanged(listProperties);
},

_findCorrespondingSampleTile : function (listProperties) {
    listProperties = isc.ListPropertiesPane.getCanonicalListProperties(listProperties);

    var style = listProperties.style,
        image = listProperties.image,
        tiles = this.sampleTileLayout.tiles,
        isCustomImage = style == "custom-image"
        ;

    for (var i = 0, numTiles = tiles.length; i < numTiles; ++i) {
        var tile = tiles[i];
        listProperties = tile._canonicalProperties;

        if (listProperties == null) {
            listProperties = tile._canonicalProperties
                           = isc.ListPropertiesPane.getCanonicalListProperties(tile.listProperties);
        }

        if (listProperties.style == style && (!isCustomImage || listProperties.image == image)) {
            return tile;
        }
    }

    return null;
}

});

isc.ListPropertiesPane.registerStringMethods({

    //> @method listPropertiesPane.listPropertiesChanged()
    // Notification method fired when the pane's +link{ListPropertiesPane.listProperties} changes.
    // @param listProperties (ListProperties) the new list configuration properties
    // @visibility external
    //<
    listPropertiesChanged : "listProperties"

});


//> @class ListPropertiesDialog
// Dialog shown for editing properties of HTML lists in a +link{RichTextEditor}.  Contains a
// +link{ListPropertiesPane}.
// <p>
// Cannot be directly used; shown in documentation only for skinning purposes.
// @treeLocation Client Reference/Foundation/RichTextEditor
// @visibility external
//<

isc.defineClass("ListPropertiesDialog", "Window");

isc.ListPropertiesDialog.addProperties({

    //> @attr listPropertiesDialog.title (String : "List Properties" : IR)
    // The title of this ListPropertiesDialog.
    // @visibility external
    // @group i18nMessages
    //<
    title: "List Properties",

    autoSize: true,

    //> @attr listPropertiesDialog.listPropertiesPane (AutoChild ListPropertiesPane : null : R)
    // The +link{ListPropertiesPane} contained by this ListPropertiesDialog.
    // @visibility external
    //<
    listPropertiesPaneDefaults: {
        _constructor: "ListPropertiesPane",
        autoParent: "none"
    },

    //> @attr listPropertiesDialog.bottomLayout (AutoChild HLayout : null : R)
    //<
    bottomLayoutDefaults: {
        _constructor: "HLayout",
        autoParent: "none",
        rightPadding: 5,
        bottomPadding: 5,
        leftPadding: 5,
        width: 400,
        height: 22,
        align: "right",
        membersMargin: 5
    },

    //> @attr listPropertiesDialog.applyButtonTitle (String : "Apply" : IR)
    // The title of the +link{ListPropertiesDialog.applyButton,Apply button}.
    // @visibility external
    // @group i18nMessages
    //<
    applyButtonTitle: "Apply",

    //> @attr listPropertiesDialog.applyButton (AutoChild IButton : null : R)
    // The Apply button. When clicked, the +link{ListPropertiesDialog.applyClick(),applyClick}
    // event is fired.
    // <p>
    // +link{ListPropertiesDialog.applyButtonTitle,applyButtonTitle} is a +link{group:autoChildUsage,passthrough}
    // for the button's +link{Button.title,title}.
    // @visibility external
    //<
    applyButtonDefaults: {
        _constructor: "IButton",
        autoParent: "bottomLayout",
        autoFit: true,
        click : function () {
            this.creator.applyClick(this.creator.listPropertiesPane.listProperties);
        }
    },

    //> @attr listPropertiesDialog.cancelButtonTitle (String : "Cancel" : IR)
    // The title of the +link{ListPropertiesDialog.cancelButton,Cancel button}.
    // @visibility external
    // @group i18nMessages
    //<
    cancelButtonTitle: "Cancel",

    //> @attr listPropertiesDialog.cancelButton (AutoChild IButton : null : R)
    // The Cancel button. When clicked, the +link{ListPropertiesDialog.cancelClick(),cancelClick}
    // event is fired.
    // <p>
    // +link{ListPropertiesDialog.cancelButtonTitle,cancelButtonTitle} is a +link{group:autoChildUsage,passthrough}
    // for the button's +link{Button.title,title}.
    // @visibility external
    //<
    cancelButtonDefaults: {
        _constructor: "IButton",
        autoParent: "bottomLayout",
        autoFit: true,
        click : function () {
            this.creator.cancelClick();
        }
    }
});

isc.ListPropertiesDialog.addMethods({

initWidget : function () {
    var listPropertiesPane = this.addAutoChild("listPropertiesPane"),
        bottomLayout = this.addAutoChild("bottomLayout");

    this.addAutoChild("applyButton", {
        title: this.applyButtonTitle
    });
    this.addAutoChild("cancelButton", {
        title: this.cancelButtonTitle
    });

    this.items = [ listPropertiesPane, bottomLayout ];
    this.Super("initWidget", arguments);
},

applyClick : isc.Class.NO_OP,

cancelClick : isc.Class.NO_OP

});

isc.ListPropertiesDialog.registerStringMethods({

    //> @method listPropertiesDialog.applyClick()
    // Notification method fired when the +link{ListPropertiesDialog.applyButton,Apply button}
    // is clicked.
    // @param listProperties (ListProperties) the list properties to apply
    // @visibility external
    //<
    applyClick : "listProperties",

    //> @method listPropertiesDialog.cancelClick()
    // Notification method fired when the +link{ListPropertiesDialog.cancelButton,Cancel button}
    // is clicked.
    // @visibility external
    //<
    cancelClick : ""

});
