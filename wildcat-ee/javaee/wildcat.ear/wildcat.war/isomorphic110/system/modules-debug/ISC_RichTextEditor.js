
/*

  SmartClient Ajax RIA system
  Version v11.0p_2016-03-31/LGPL Deployment (2016-03-31)

  Copyright 2000 and beyond Isomorphic Software, Inc. All rights reserved.
  "SmartClient" is a trademark of Isomorphic Software, Inc.

  LICENSE NOTICE
     INSTALLATION OR USE OF THIS SOFTWARE INDICATES YOUR ACCEPTANCE OF
     ISOMORPHIC SOFTWARE LICENSE TERMS. If you have received this file
     without an accompanying Isomorphic Software license file, please
     contact licensing@isomorphic.com for details. Unauthorized copying and
     use of this software is a violation of international copyright law.

  DEVELOPMENT ONLY - DO NOT DEPLOY
     This software is provided for evaluation, training, and development
     purposes only. It may include supplementary components that are not
     licensed for deployment. The separate DEPLOY package for this release
     contains SmartClient components that are licensed for deployment.

  PROPRIETARY & PROTECTED MATERIAL
     This software contains proprietary materials that are protected by
     contract and intellectual property law. You are expressly prohibited
     from attempting to reverse engineer this software or modify this
     software for human readability.

  CONTACT ISOMORPHIC
     For more information regarding license rights and restrictions, or to
     report possible license violations, please contact Isomorphic Software
     by email (licensing@isomorphic.com) or web (www.isomorphic.com).

*/

if(window.isc&&window.isc.module_Core&&!window.isc.module_RichTextEditor){isc.module_RichTextEditor=1;isc._moduleStart=isc._RichTextEditor_start=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc._moduleEnd&&(!isc.Log||(isc.Log && isc.Log.logIsDebugEnabled('loadTime')))){isc._pTM={ message:'RichTextEditor load/parse time: ' + (isc._moduleStart-isc._moduleEnd) + 'ms', category:'loadTime'};
if(isc.Log && isc.Log.logDebug)isc.Log.logDebug(isc._pTM.message,'loadTime');
else if(isc._preLog)isc._preLog[isc._preLog.length]=isc._pTM;
else isc._preLog=[isc._pTM]}isc.definingFramework=true;


if (window.isc && isc.version != "v11.0p_2016-03-31/LGPL Deployment" && !isc.DevUtil) {
    isc.logWarn("SmartClient module version mismatch detected: This application is loading the core module from "
        + "SmartClient version '" + isc.version + "' and additional modules from 'v11.0p_2016-03-31/LGPL Deployment'. Mixing resources from different "
        + "SmartClient packages is not supported and may lead to unpredictable behavior. If you are deploying resources "
        + "from a single package you may need to clear your browser cache, or restart your browser."
        + (isc.Browser.isSGWT ? " SmartGWT developers may also need to clear the gwt-unitCache and run a GWT Compile." : ""));
}





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









//>    @class RichTextCanvas
//
// Canvas to be used for Rich Text Editing
//
// @treeLocation Client Reference/Foundation/RichTextEditor
// @visibility external
//<
isc.ClassFactory.defineClass("RichTextCanvas","Canvas");

isc.RichTextCanvas.addClassProperties({
    // enumerated Justification types
    //CENTER:"center",
    //LEFT:"left",
    //RIGHT:"right",
    FULL:"full",

    //>@classAttr   RichTextCanvas.unsupportedErrorMessage  (string : "Rich text editing not supported in this browser" : [IRW])
    // Message to display to the user if they attempt to access the page in a browser which
    // does not support rich-text-editing
    //<

    unsupportedErrorMessage : "Rich text editing not supported in this browser"
});

isc.RichTextCanvas.addProperties({

    editable:true,

    // Override 'canSelectText': to allow for most editing actions the user must be able to
    // select the content from this widget.
    canSelectText:true,

    // RTC's are focusable
    canFocus: true,

    // Don't write out a focusProxy for RichTextCanvases - we don't want native keyboard
    // focus to go to a hidden element
    // Instead - in design mode we apply tabIndex directly to the content frame
    // otherwise we rely on native tabIndex to allow focus in the widget handle at
    // the correct times.
    _useFocusProxy:false,

    //> @attr richTextCanvas.moveFocusOnTab (boolean : true : IRW)
    // If the user presses the "Tab" key, should focus be taken from this editor?
    // If set to <code>false</code> a "Tab" keypress will cause a Tab character
    // to be inserted into the text, and focus will be left in the edit area.
    // @visibility external
    //<
    moveFocusOnTab: true,


    overflow:isc.Canvas.AUTO,
    showCustomScrollbars:false,

    // If a syntax rehilite of the entire contents is required, do it this number of
    // milliseconds after the last keystroke (resets to this number every time the user hits a key)
    fullSyntaxHiliteDelay: 3000,

    // Don't show a non-breaking space by default
    contents : ""

    // even when hidden the rich text area picks up the bitmap of the area behind it and drags it
    // along as it is scrolled.
    //
    // XXX not a viable workaround - when hidden in this manner, it fails to show()
//    hideUsingDisplayNone: isc.Browser.isMoz
});

isc.RichTextCanvas.addClassMethods({


    //> @classMethod RichTextCanvas.supportsRichTextEditing()
    //  Does this browser support rich text editing, using the isc.RichTextCanvas class?
    //
    //  @return (boolean)   true if supported by this browser.
    //<
    supportsRichTextEditing : function () {
        var supported = ((isc.Browser.isSafari && isc.Browser.safariVersion >= 312) ||
                         (isc.Browser.isIE) ||
                         // Tested Moz (>=1.4 on Linux / Mac / Windows)
                         //        Firefox (>=1.0 on Linux / Mac / Windows)
                         // Doesn't work on latest camino as of May 17 05 (Version 0.8.4)
                         (isc.Browser.isMoz && !isc.Browser.isCamino) ||
                         isc.Browser.isOpera
                        );
        return supported;
    },

    _fixTabSpan : function (tabSpan) {
        tabSpan.innerHTML = "&#9;";
    }
});

//!>Deferred
isc.RichTextCanvas.addMethods({

    // On init, verify that we're in a supported browser, and that the overflow is "auto"
    initWidget : function () {

        if (!isc.RichTextCanvas.supportsRichTextEditing()) {
            var errorMessage = isc.RichTextCanvas.unsupportedErrorMessage;

            this.logError(errorMessage);
        }

        if (this.overflow != isc.Canvas.AUTO) {
            this.logWarn('RichTextCanvas class currently only supports an overflow property of "auto"');
            this.overflow = isc.Canvas.AUTO;
        }

        // In "design mode" - where we write out an iframe with editable body content,
        // turn off native tab index on the handle. We'll instead apply the tabIndex directly
        // to the iframe
        if (this._useDesignMode()) {
            this._useNativeTabIndex = false;
        }

        this.Super("initWidget", arguments);
    },

    // Override getHandleOverflow - we always have an overflow of "auto" specified on the widget
    // but if we're writing out an editable IFRAME, any scrollbars will show up on the inner
    // content frame rather than the handle, so we never want to show scrollbars on the handle

    _getHandleOverflow : function () {
        if (this._useDesignMode()) {
            var overflow;
            if (this._useMozScrollbarsNone) {
                overflow = "-moz-scrollbars-none";
                this._useMozScrollSize = true;
            } else {

                overflow = this._$hidden;
            }
            return overflow;
        } else return this.Super("_getHandleOverflow", arguments);
    },

    // getInnerHTML() overridden to write out an editable area.
    getInnerHTML : function () {

        // If we're writing out an IFrame with designMode:"On", return the appropriate HTML
        if (this._useDesignMode() && !this.isPrinting) {
            return this.getIFrameHTML();
        }

        // Otherwise we'll just be setting contentEditable on the standard widget handle.
        //
        // Note: we used to call Super here, but the Canvas implementation calls getContents()
        // with no args which in this case returns the un-marked-up source, resulting in
        // hilighting breaking on redraw.  In this particular case, return the marked up
        // contents since we'll be assigning to innerHTML
        return this.getContents(true);
    },

    // _useDesignMode: Should we achieve our rich text canvas via an IFrame with DesignMode "On",
    // or via a contentEdtiable DIV.

    _useDesignMode : function () {
        return ((isc.Browser.isChrome ||
                 isc.Browser.isSafari ||
                 isc.Browser.isOpera ||
                 isc.Browser.isMoz) ||
                isc.screenReader);
    },

    // ---------- Design Mode / IFRAME handling ------------------
    _$iframeTemplate: [




        "<iframe id='",
        , // [1] this.getIFrameID()
        "' style='margin:0px;padding:0px;border:0px;width:",

        , // [3] this.getContentFrameWidth()
        "px;height:",
        , // [5] this.getContentFrameHeight()
        "px'",


        (isc.Browser.isWebKit || isc.Browser.isIE
         ? " src='" + isc.Page.getURL("[HELPERS]empty.html") + "'"
         : null),

        " onload='",
        , // [9] this.getID()
        "._frameLoaded();' tabindex='",
        , // [11] this.getTabIndex()
        "'></iframe>"
    ],
    getIFrameHTML : function () {
        var srcArray = this._$iframeTemplate;
        srcArray[1] = this.getIFrameID();
        srcArray[3] = this.getContentFrameWidth();
        srcArray[5] = this.getContentFrameHeight();
        srcArray[9] = this.getID();
        srcArray[11] = this.getTabIndex();
        return srcArray.join(isc.emptyString);
    },

    _setHandleTabIndex : function (index) {
        if (this._useDesignMode()) {
            var frame = this.getContentFrame();
            if (frame != null) frame.tabIndex = index;
        } else {
            return this.Super("_setHandleTabIndex", arguments);
        }
    },


    // getBrowserSpellCheck - function to determine if we want to use native browser spellcheck
    // functionality where present.

    getBrowserSpellCheck : function () {
        return true;
    },

    // _frameLoaded - helper method to notify us that the IFRAME has loaded, so we can
    // set up its contents / editability.
    _frameLoaded : function () {
        if (!this._drawingFrame) return;
        delete this._drawingFrame;
        if (!this.isDrawn()) return;
        this._setupEditArea();
    },

    // Get the ID for the frame in the DOM
    getIFrameID : function () {
        return this.getID() + "_iframe";
    },

    // Get a pointer to the IFRAME content document
    getContentDocument : function () {

        if (!this._useDesignMode()) return this.getDocument();


        var win = this.getContentWindow(),
            doc = win ? win.document : null;

        if (doc == null) {
            // This can happen validly as the document is not always available immediately
            // after drawing.

            this.logDebug("Unable to get pointer to content document. Content may not be written out");
        }
        return doc;
    },

    // Get a pointer to the document body
    getContentBody : function () {
        var doc = this.getContentDocument();
        if (doc) return doc.body;
        return null;
    },

    // Get a pointer to the IFRAME window object.
    getContentWindow : function () {
        if (!this._useDesignMode()) return this.getWindow();

        var element = this.getContentFrame();
        return element ? element.contentWindow : null;
    },

    // get a pointer to the IFRAME element in the DOM
    getContentFrame : function () {
        if (!this._useDesignMode() || !this.isDrawn()) return null;

        return isc.Element.get(this.getIFrameID());
    },


    // Scrolling / Overflow:


    // Override setOverflow() to be a no-op. We've already guaranteed the overflow will be
    // 'auto' when the RichTextCanvas is initialized in initWidget().
    setOverflow : function () {

    },

    // getScrollHandle()    Returns a pointer to the element that gets natively scrolled by
    // calls to scrollTo().
    // - Overridden to point to the content body if _useDesignMode() is true.
    getScrollHandle : function () {
        if (this._useDesignMode()) return this.getContentBody();

        return this.Super("getScrollHandle", arguments);
    },

    // Override the internal adjustOverflow method.
    // If we're showing an IFrame, the default implementation will not reliably calculate
    // whether scrollbars are visible.
    __adjustOverflow : function () {
        // always call the standard 'adjustOverflow' method to ensure we setHandleRect etc as
        // appropriate.
        this.Super("__adjustOverflow", arguments);

        // If we're not writing out an IFrame we can just do normal overflow adjustment
        // Overflows other than "auto" are not really supported - in this case just return too.
        if (!this._useDesignMode() || this.overflow != isc.Canvas.AUTO) return;

        // Update hscrollOn/ vscrollOn - not reliably set by the standard adjustOverflow logic.
        var scrollHeight = this.getScrollHeight(),
            scrollWidth = this.getScrollWidth(),
            height = this.getHeight(), width = this.getWidth(),
            scrollbarSize = this.getScrollbarSize(),
            hscrollOn = false, vscrollOn = false;

        if (scrollHeight > height) vscrollOn = true;
        if (hscrollOn) width -= scrollbarSize;
        if (scrollWidth > width) hscrollOn = true;
        if (hscrollOn && !vscrollOn && (scrollHeight > height - scrollbarSize)) vscrollOn = true;

        this.hscrollOn = hscrollOn;
        this.vscrollOn = vscrollOn;

    },

    // methods to return the size for the content frame if we're using design mode.

    getContentFrameWidth : function () {
       return this.getWidth() - this.getHMarginBorderPad();
    },

    getContentFrameHeight : function () {
       return this.getHeight() - this.getHMarginBorderPad();
    },

    // Override _setHandleRect() to always size the IFRAME to match the size of the
    // handle.
    _setHandleRect : function (left, top, width, height) {
        this.Super("_setHandleRect", arguments);


        if (this._useDesignMode()) {
            var cf = this.getContentFrame();
            if (cf != null) {
                var innerWidth = this.getContentFrameWidth(), innerHeight = this.getContentFrameHeight();
                cf.style.width = innerWidth -1 + "px";
                cf.style.height = innerHeight -1 + "px";
            }
        } else {
            var handle = this.getHandle();
            if (handle != null) {
                var innerWidth = this.getContentFrameWidth(), innerHeight = this.getContentFrameHeight();
                handle.style.width = innerWidth -1 + "px";
                handle.style.height = innerHeight -1 + "px";
            }
        }
    },

    // Override getScrollHeight() / width to look at the IFRAME body scroll height, since the
    // IFRAME will always be sized to 100%, making the scroll size of the widget handle always
    // equal to the specified size.
    getScrollWidth : function (calculateNewValue) {
        if ((this._scrollWidth && !calculateNewValue) || !this._useDesignMode())
            return this.Super("getScrollWidth", arguments);

        var cb = this.getContentBody();
        if (!cb) return this.Super("getScrollWidth", arguments);

        // cache the scrollWidth for next time this method is called.
        this._scrollWidth = isc.Element.getScrollWidth(cb);
        return this._scrollWidth;
    },

    getScrollHeight: function (calculateNewValue) {
        if ((this._scrollHeight && !calculateNewValue) || !this._useDesignMode())
            return this.Super("getScrollHeight", arguments);

        var cb = this.getContentBody();
        if (!cb) return this.Super("getScrollHeight", arguments);

        this._scrollHeight = isc.Element.getScrollHeight(cb);
        return this._scrollHeight;
    },

    // --------------------------------------





    // _rememberSelection() - saves out the current selection position, so we can re-set it
    // when this element regains focus. Only used in IE.

    _rememberSelection : function () {
        if (!isc.Browser.isIE) return;

        // Check whether we currently have selection before proceeding - otherwise we could
        // remember some text range outside our handle.
        if (!this._hasSelection()) return;

        if (isc.Browser._hasDOMRanges) {
            var sel = this.getContentDocument().getSelection();
            if (sel.rangeCount <= 0) {
                this._savedSelection = null;
                this._savedSelectionAnchorNode = null;
                this._savedSelectionFocusNode = null;
                this._oldSelectionText = null;
            } else {
                var range = this._savedSelection = sel.getRangeAt(0);
                this._savedSelectionAnchorNode = sel.anchorNode;
                this._savedSelectionFocusNode = sel.anchorNode;
                this._oldSelectionText = String(range);
            }
        } else {
            this._savedSelection = this.getContentDocument().selection.createRange();
            // Also remember the content of the selection. If the content changes, we don't
            // want a call to '_resetSelection' to select the new text.
            this._oldSelectionText = this._savedSelection.text;
            // this.logWarn("just saved selection :"+ Log.echo(this._savedSelection));
        }
    },

    // _hasSelection() - Is the current document selection within the RichTextCanvas?
    // Used by _rememberSelection() [IE only]
    _hasSelection : function () {

        if (!this.isDrawn()) return false

        if (!isc.Browser.isIE) return;


        if (this._useDesignMode()) {
            return (this.getActiveElement() == this.getContentFrame());
        }



        var handle = this.getHandle();
        if (!handle) return false;

        var selElement = isc.Element._getElementFromSelection();
        if (!selElement) return false;

        return handle.contains(selElement);
    },

    // Remember the selection every time it changes, so we can reset the selection on focus
    // or execCommand)

    selectionChange : function () {
        if (!this._focussing) {
            this._rememberSelection();


            if (isc.Browser.isIE &&
                (isc.Browser.version <= 8 ||
                 (isc.Browser.version == 9 && !isc.Browser.isIE9)))
            {
                this._queueContentsChanged();
            }
        }
    },

    // _resetSelection: resets selection to whatever it was last time this RTC had focus.

    _resetSelection : function () {
        if (!this.editable || !this.isDrawn() || !this.isVisible()) return;

        if (isc.Browser.isIE) {
            // If no  previous selection, just bail
            if (!this._savedSelection) return;

            var newSelectionText = isc.Browser._hasDOMRanges ? String(this._savedSelection) : this._savedSelection.text;

            // If the content of the range has changed since it was selected, avoid selecting
            // the modified text

            if (this._oldSelectionText != newSelectionText) {
                this._savedSelection.collapse(false);
            }

            isc.EH._allowTextSelection = true;
            if (isc.Browser._hasDOMRanges) {
                var doc = this.getContentDocument(),
                    sel = doc.getSelection();
                sel.removeAllRanges();
                sel.addRange(this._savedSelection);
            } else {
                this._savedSelection.select();
            }
            delete isc.EH._allowTextSelection;

        //} else {    //Currently only supported on IE
        }
    },



    // Override setFocus() - when focussing in this widget we want the selection to be
    // whatever it was before the widget was blurred, and for the editable text to have
    // keyboard focus.
    setFocus : function (hasFocus) {
        // Call the Superclass implementation.

        this._focussing = true;
        this.Super("setFocus", arguments);
        this._focussing = false;

        // If we're using an IFRAME ensure it has focus natively

        if (this._useDesignMode()) {
            var win = this.getContentWindow();
            if (!win) return;

            if (hasFocus) win.focus()

            else window.focus();

        // Making this widget's handle contentEditable.

        } else {

            if (hasFocus) {
                this._resetSelection();

            }

            //else this._rememberSelection();
        }

    },

    // ------------------- Editor init ------------------

    // Override draw to ensure we make the HTML editable when it's done drawing.
    draw : function () {
        this.Super("draw", arguments);

        // If we're writing out an IFRAME we need to show an event mask for this canvas.
        if (this._useDesignMode())
            isc.EventHandler.registerMaskableItem(this, true);

        // Initialize the contents via _setupEditArea();

        if (this._useDesignMode()) {
            this._drawingFrame = true;

        } else {
            this._setupEditArea();
        }

    },





    redraw : function () {
        var reinitRequired = this._useDesignMode();
        if (reinitRequired) this._rememberContents();

        this.Super("redraw", arguments);
        if (reinitRequired) this._drawingFrame = true;
    },

    _domRefreshedByParent : function () {
        this.Super("_domRefreshedByParent", arguments);
        var reinitRequired = this._useDesignMode();
        if (reinitRequired) this._drawingFrame = true;
    },

    // _setupEditArea:  Fired when the RichTextCanvas is written into the DOM.
    // This will ensure the appropriate contents and edit state are applied to this widget.

    _setupEditArea : function () {
        // Update the HTML to ensure that this is actually editable.

        

        var designMode = this._useDesignMode();

        // When using an IFRAME written out in design mode we need to add some custom event
        // handlers.
        if (designMode) {
            // Capture keyboard events so we can fire our keypress handler.
            // Also capture scrolling on the IFRAME directly to update our scroll position,
            // since we're showing native scrollbars on that element.


            var addKeyboardListenersToContentDoc = isc.Browser.isChrome || isc.Browser.isSafari,
                thisAccessPath = (addKeyboardListenersToContentDoc
                                  ? "this.defaultView.frameElement.ownerDocument.defaultView."
                                  : "");


            if (!this._editInputHandler) {
                this._editInputHandler = isc._makeFunction(
                                              "",
                                              thisAccessPath + this.getID() + "._iFrameInput()"
                                             );
            }

            if (!this._editKeyPressHandler) {
                this._editKeyPressHandler = isc._makeFunction(
                                                 "event",
                                                 "event=event||" + this.getID() + ".getContentWindow().event;" +
                                                 "var returnValue=" + thisAccessPath + this.getID() + "._iFrameKeyPress(event);" +
                                                 "if(returnValue==false && event.preventDefault)event.preventDefault();" +
                                                 "else event.returnValue=(returnValue!=false)"
                                                );
            }
            if (!this._editKeyDownHandler) {
                this._editKeyDownHandler = isc._makeFunction(
                                                 "event",
                                                 "event=event||" + this.getID() + ".getContentWindow().event;" +
                                                 "var returnValue=" + thisAccessPath + this.getID() + "._iFrameKeyDown(event);" +
                                                 "if(returnValue==false && event.preventDefault)event.preventDefault();" +
                                                 "else event.returnValue=(returnValue!=false)"
                                                );
            }
            if (!this._editKeyUpHandler) {
                this._editKeyUpHandler = isc._makeFunction(
                                                 "event",
                                                 "event=event||" + this.getID() + ".getContentWindow().event;" +
                                                 "var returnValue=" + thisAccessPath + this.getID() + "._iFrameKeyUp(event);" +
                                                 "if(returnValue==false && event.preventDefault)event.preventDefault();" +
                                                 "else event.returnValue=(returnValue!=false)"
                                             );
            }
            if (!this._editScrollHandler) {
                this._editScrollHandler = isc._makeFunction(
                                                 "event",
                                                 "event=event||" + this.getID() + ".getContentWindow().event;" +
                                                 "var returnValue=" + this.getID() + "._iFrameScroll(event);" +
                                                 "if(returnValue==false && event.preventDefault)event.preventDefault();" +
                                                 "else event.returnValue=(returnValue!=false);" +
                                                 "return (returnValue!=false)"
                                                );
            }

            if (!this._editFocusHandler) {
                this._editFocusHandler = isc._makeFunction(
                                                "",
                                                this.getID() + "._iFrameOnFocus();"
                                               );
            }
            if (!this._editBlurHandler) {
                this._editBlurHandler = isc._makeFunction(
                                                "",
                                                this.getID() + "._iFrameOnBlur();"
                                              );
            }
            var win = this.getContentWindow(),
                contentDoc = win.document;



            var keyboardListenersReceiver = (addKeyboardListenersToContentDoc
                                             ? contentDoc
                                             : win);
            if (isc.Browser.isIE && !isc.Browser.isIE9) {
                keyboardListenersReceiver.attachEvent("keypress", this._editKeyPressHandler);
                keyboardListenersReceiver.attachEvent("keydown", this._editKeyDownHandler);
                keyboardListenersReceiver.attachEvent("keyup", this._editKeyUpHandler);

                win.onscroll = this._editScrollHandler;
                win.onfocus = this._editFocusHandler;
                win.onblur = this._editBlurHandler;
            } else {

                keyboardListenersReceiver.addEventListener("input", this._editInputHandler, false);
                keyboardListenersReceiver.addEventListener("keypress", this._editKeyPressHandler, false);
                keyboardListenersReceiver.addEventListener("keydown", this._editKeyDownHandler, false);
                keyboardListenersReceiver.addEventListener("keyup", this._editKeyUpHandler, false);

                win.addEventListener("scroll", this._editScrollHandler, false);
                win.addEventListener("focus", this._editFocusHandler, false);
                win.addEventListener("blur", this._editBlurHandler, false);
            }
            if (addKeyboardListenersToContentDoc) {
                contentDoc.body.handleNativeEvents = "false";

                contentDoc.documentElement.handleNativeEvents = "false";
            }

            var bodyStyle = this.getContentBody().style;
            // Suppress the default margin
            bodyStyle.margin = "0px";

            // Apply text-properties from our specified CSS class to the content of the
            // IFRAME.

            var classStyle = isc.Element.getStyleDeclaration(this.className);
            if (classStyle != null) {
                var textStyleAttrs = isc.Canvas.textStyleAttributes;

                for (var i = 0; i < textStyleAttrs.length; i++) {
                    var attr = textStyleAttrs[i];
                    bodyStyle[attr] = classStyle[attr];
                }
            }
        }

        // In moz, if we want native spell-check behavior enable it here (otherwise
        // explicitly disable it).
        if (isc.Browser.isMoz) {
            var contentBody = this.getContentBody();
            if (contentBody) contentBody.spellcheck = (!!this.getBrowserSpellCheck())
        }

        var editable = (this.editable && !this.isDisabled());

        // Actually make the handle editable
        if (!designMode) this._setHandleEditable(editable);
        else {

            this.delayCall("_setHandleEditable", [editable,true], 0);
        }

        // set up our initial contents
        //
        // we're calling _setContents() which means we're bypassing hiliting - this is what we
        // want most of the time because this method is called on redraws when the user may
        // have just resized the browser, so there's no reason to recolorize.  But if this is
        // the first time we're drawing, we need to hilite if we have a syntaxHiliter because
        // contents may have been provided as an init parameter and if we don't do this it
        // won't get syntax hilited.
        //
        // Note: important to do this after all of the above - to make sure the correct
        // styling is applied the first time - otherwise we get a partially styled rendering
        // which snaps to the fully styled rendering after about a second - even on fast systems.
        if (this.syntaxHiliter && !this.formattedOnce) {
            this.formattedOnce = true;
            this.contents = this.hiliteAndCount(this.contents);
        }
        this._setContents(this.contents);
    },

    // ----------------- Event handling ----------------------

    _nativeCutPaste : function () {
        if (!this._settingContents) {
            this._rememberSelection();
            this._queueContentsChanged();
        }
    },

    // _iFrameInput() is used to handle an 'input' event on the <iframe> when using designMode
    _iFrameInput : function () {
        this._queueContentsChanged();
    },

    // If using designMode, we need a handler for the native keypress event on our IFRAME
    _iFrameKeyPress : function (event) {

        // apply the properties (keyName, etc.) to EH.lastEvent
        isc.EH.getKeyEventProperties(event);
        // Fall through to standard handling, making sure this widget is logged as the
        // keyTarget
       return isc.EH.handleKeyPress(event, {keyTarget:this});

    },
    _iFrameKeyDown : function (event) {
        // apply the properties (keyName, etc.) to EH.lastEvent
        isc.EH.getKeyEventProperties(event);
        return isc.EH.handleKeyDown(event, {keyTarget:this});
    },
    _iFrameKeyUp : function (event) {

        // apply the properties (keyName, etc.) to EH.lastEvent
        isc.EH.getKeyEventProperties(event);
        return isc.EH.handleKeyUp(event, {keyTarget:this});
    },

    // If using designMode, we need a handler for the native scroll event on our IFRAME
    // to update the stored scroll position of the handle on scroll.
    // The standard handleCSSScroll method will handle scrolling (as it will check the
    // scroll position of this.getScrollHandle() - which points at the IFRAME).
    _iFrameScroll : function (event) {
        return this._handleCSSScroll(event);
    },

    _iFrameOnFocus : function () {
        if (this.destroyed) return;
        isc.EH.focusInCanvas(this, true);
        return true;
    },

    _iFrameOnBlur : function () {
        if (this.destroyed) return;
        isc.EH.blurFocusCanvas(this, true);
        return true;
    },

    _$Tab: "Tab",
    _$tabHTML: "<span class='_isc_tab' style='white-space:pre'>\t</span>",
    handleKeyDown : function (event, eventInfo, c, d, e) {
        var key = isc.EH.getKey();
        if (key == this._$Tab) {
            // Move focus
            if (this.moveFocusOnTab) {
                this._focusInNextTabElement(!isc.EH.shiftKeyDown());

            // Otherwise, insert a tab character
            } else {
                this.insertHTML(this._$tabHTML);
            }
            return false;
        }
        return this.invokeSuper(isc.RichTextCanvas, "handleKeyDown", event, eventInfo, c, d, e);
    },

    // Adjust overflow on keypress - updates recorded scroll width/height
    _$br:"<br>",
    _$Enter:"Enter",
    // set of keys that are ignored by handleKeyPress because they can't modify the contents of
    // the editable area.  This isn't exhaustive - the main reason to have these is to
    // eliminate gratuitous syntax hilighting while e.g. the user is using arrow keys to
    // navigate around the document.

    ignoreKeys : ["Arrow_Up", "Arrow_Down", "Arrow_Left", "Arrow_Right", "Ctrl", "Alt", "Tab",
        "Space", "Home", "End"
    ],
    handleKeyPress : function (event, eventInfo) {
        var key = isc.EH.getKey();

        if (this.ignoreKeys.contains(key)) return isc.EH.STOP_BUBBLING;

        // figure out the start line number of the current selection before the key stroke so
        // we can extract the modified line(s) later.
        if (this.countLines) this.rememberSelectionStartLine();


        this._queueContentsChanged();
        var returnVal = this.Super("handleKeyPress", arguments);

        // in IE, we set a timer onpaste to do syntax hiliting - this enalbes us to respond to
        // a paste command initiated via the context menu or "Edit" menu, but when a user hits
        // a keyboard shortcut to paste, we also get a key press.  So if we're responding to a
        // keypress and there's a paste timer, we delete it so we don't process the paste twice.
        if (isc.Browser.isIE && this._pasteTimer) {
            isc.Timer.clearTimeout(this._pasteTimer);
            delete this._pasteTimer;
        }




        return returnVal;
    },

    _queueContentsChanged : function () {
        if (!this._dirtyContent) {
            this._dirtyContent = true;
            if (!this._changedHandlerName) this._changedHandlerName = "_contentsChanged";
            isc.Page.setEvent(isc.EH.IDLE, this, isc.Page.FIRE_ONCE, this._changedHandlerName);
        }
    },

    //_contentsChanged - fired when the contents is edited.
    // Not fired in response to explicit 'setContents' call.

    _contentsChanged : function () {
        delete this._dirtyContent;

        var oldVal = this.contents,
            newVal = this.getContents();
        if (oldVal == newVal) return;

        // if we're counting lines, then call doLinesChanged().  We're also checking for
        // selectionIsCollapsed() here because Ctrl-A, which causes all contents to be selected
        // should not fire doLinesChanged() - and that's the only known way to get a multichar
        // selection after a keystroke/paste event
        if (this.countLines && this.selectionIsCollapsed()) this.doLinesChanged(oldVal, newVal);

        // AdjustOverflow - our scroll-size is likely to have changed
        this.adjustOverflow("edited");

        // Fire this.changed, if present
        if (this.changed != null) this.changed(oldVal, newVal);

        this.contents = newVal;
    },


    // ------------------------------------------------------------------------------------
    // lineChanged / Synax Hiliting support
    // ------------------------------------------------------------------------------------
    //
    // We want to detect any change made to the editable area so we can re-format the view.
    // Change can be effected in several ways:
    //   - keypress
    //   - paste action using the browser "Edit" menu
    //   - programmatic update (currently only via setContents())
    //
    // At the time of the change, the user may have an insertion cursor or a block of selected
    // text.
    //
    // We want to format the newly added data, and potentially some data around the new data.
    // To that end we want to:
    //   - mark the location of the current insertion point, so we can restore it after making
    //   changes
    //   - determine the start and end index of the changed text
    //   - expand the above indexes to fully envelop the start and end line of the changed
    //   text.
    //     - we want this because recolorization will happen on a line-by-line basis (for
    //     performance reasons) except for some cases where we'll want to recolorize the
    //     whole document
    //
    // In IE and FF we can insert arbitrary HTML at the insertion cursor.  This allows us to
    // get the current location of the cursor.
    //
    // General issues
    //---------------
    // - In FF, we can't detect what was pasted unless we compare the original contents
    // and the new contents - which is expensive.  In fact, if the user uses the Edit
    // menu paste command, then we can't tell the the editable area even changed.  There's a
    // DOM event called  "onsubtreemodified" that's part of the w3c spec that should at least
    // tell us that the contents changed - but it doesn't work at all in current versions of FF
    // (and this has been corroborated by postings on the web).
    // - In FF, the contents of the editable area change asynchronously with the key event.  In
    // other words you have to set a timeout to get the after-changed state of the editable area.
    // - Detecting current selection/insertion point.  In IE and FF we can detect the current
    // selection/insertion point by wrapping the selection contents in a DOM node (e.g. a span)
    // and then scanning the contents for it.
    // - In FF we lose the insertion cursor if there's no adjoining text.  Also, the cursor
    // marker that we use to extract the cursor position blocks the user from using the right
    // arrow on the keyboard to move to the next character beyond it.
    //
    // Approaches:
    // -----------
    // 1.  Wait for the user to stop typing for a bit and then reformat the entire editable
    // area.  Performance is gated by the formatting algorithm, but this is very easy to
    // implement and may be acceptable for some use cases.
    // - benefits
    //   - easy to implement
    // - problems
    //   - doesn't look as nice, because formatting is not realtime
    //
    // 2.  Determine the edited line by scanning backwards and forwards from the current
    // insertion point looking for <BR>s.
    // - benefits
    //   - easier than maintaining line information in the editable area
    // - problems
    //   - scanning backwards for <BR>s potentially not cheap
    //   - need to both scan backwards for <BR> (so we can select out the HTML to pass to the
    //   formatter) and walk the DOM to the last <BR> so we can efficiently insert the results.
    //
    // 3.  Maintain line information in the editable area by inserting line spans into the
    // contents provided to setContents().  Insert a span with a unique ID into the document at
    // the current insertion point after a change and walk the
    // DOM up from that node to find the current line for fast extraction.
    // - benefits:
    //   - very fast line extraction
    //      - given a selection, can quickly determine what lines it spans.
    //      - after formatting the line, we can replace it effieciently via innerHTML assignment
    //      or equivalent.
    //   - As long as line-based formatting is sufficient for most keystrokes, this approach
    //   gives us an O(1) implementation.
    //   - can get the contents of any line very quickly.  Given a line, can get its immediate
    //   surrounding lines quickly.  Can get the line number quickly.
    // - problems
    //   - These spans must be maintained as the user hits Backspace, Enter and pastes
    //   arbitrary content.
    //     - this means fragmenting multiline pastes and those created by user hitting the
    //     Enter key into separate lines and combining lines created by partial line pastes and
    //     Backspace at beginning of line.
    //       - In IE, copying a whole line out of the editable area also picks up its line span
    //       delimiters, which means we can end up with a line inside a line situation.
    //         - I think this can be fixed by defining a custom onbeforepaste handler on the
    //         line spans and filtering the line spans out of the data retrieved from the
    //         clipboard.
    //       - In Moz, the <BR> inside the line span isn't picked up by the copy operation and
    //       is replaced by the paste operation if the paste is at the end of the line.
    //       Further, unlikes IE, the line span isn't placed inside the line that gets pasted
    //       into - instead the pasted-into line is fragmented into two line spans by the
    //       browser and the pasted line is added a peer in between those.
    //
    //
    // line behaviors
    // --------------
    // - type character at beginning of line
    //   - IE, FF: currentLine contains original chars + new char + selection marker + <br>
    // - type character in the middle of a line
    //   - IE, FF: as above
    // - type character at end of a line
    //   - IE, FF: as above
    // - no special processing required for the above
    //
    // - hit enter at beginning of line
    //   - IE, FF: current line contains <BR><cursor>origLineText<BR>
    // - hit enter in the middle of a line
    //   - IE, FF: current line contains origLineTextBeforeEnter<BR><cursor>origLineTextAfterEnter<BR>
    // - hit enter at the end of a line
    //   - IE, FF: current line contains origLineText<BR><cursor><BR>
    // - split current line by <BR> count using regexes to extract new lines
    //
    // - hit backspace at beginning of line
    //   - IE: <cursor> jumps to previous line, deleting the <BR> there.
    //   - FF: previous line missing <BR>, currentLine has <cursor> at start
    // - hit delete at end of line
    //   - IE: current line now missing <BR> (basically as IE above)
    //   - FF: as IE
    // - hit backspace in middle of line
    //   - IE, FF: as typing char in middle of line, except old char deleted
    // - hit backspace at end of line
    //   - IE, FF: as above
    // - if current line is missing <BR>, combine with next, otherwise combine previous with current
    //
    // - paste external chars (not from a line span) with no BR in line:
    //   - IE: as typing char, but selection goes to front of pasted text
    //   - FF: as typing char
    // - paste external chars (not from a line span) with BR in line:
    //   - IE, FF: as above, plus a <BR> where there's a linebreak in pasted content
    // - handled by above cases
    //
    // - paste chars from a line span with no BR in line:
    //   - IE: if the copied selection touched the start of the line span, then behaves as FF,
    //   except that the pasted line span appears as a child of the pasted-to line span, not a peer.
    //   Otherwise behaves as paste of external chars
    //     - filter out line spans from pasted content with onbeforepaste, onpaste on line spans,
    //     then handled by above cases as external paste
    //
    //   - FF: pasted-to line is fragmented into two line spans by the insertion point.  pasted
    //   text appears as its own line in between the two.  If pasted at end of a line, that
    //   line's BR is moved out of that line's span as the nextSibling the newly created
    //   line span.
    //     - whenever pasting into a line, that line's <BR> is destroyed and either
    //     moves outside the line span (if paste was at end of line) or into a new line
    //     fragment (if there was text to the right of the insertion cursor before paste)
    //     - a paste can be multiline, introducing potentially multiple whole lines, so can't
    //     just look back for a missing <BR> until we hit a normal line (since some intervening
    //     lines may actually be normal, because pasted wholesale from this edit area)
    //     - SOLUTION:
    //        - before paste:
    //          - query total number of lines
    //        - after paste:
    //          - if <BR> is nextSibling outside currentLine (selection always at end of paste),
    //          pull it into currentLine.
    //        - now query total number of lines (after paste).  This is the number of lines we
    //        have to walk back looking for missing <BR>s and combining.
    //
    // Actual approach used:
    //----------------------
    // #3, except don't combine lines in realtime.  Instead, provide a special change()
    // notification that gives the user the pasted contents and the  pasted contents out to line
    // breaks, and the line number.  The user can then format them and call a method to replace
    // them and insert them at a given location.
    //

    // apply a SyntaxHiliter to the contents
    setSyntaxHiliter : function (syntaxHiliter) {
        if (syntaxHiliter == null) {
            this.removeSyntaxHiliter();
            return;
        }
        this.syntaxHiliter = syntaxHiliter;
        this.countLines = true;

        // apply syntax hiliting to the contents
        var contents = this.getContents() || isc.emptyString;
        this.setContents(contents);
    },

    removeSyntaxHiliter : function () {
        // get the contents (do this before we delete this.syntaxHiliter, otherwise the
        // contents will come with markup.
        var contents = this.getContents() || isc.emptyString;

        delete this.syntaxHiliter;
        delete this.countLines;

        // apply the contents, now without the markup
        this.setContents(contents);
    },

    doLinesChanged : function (oldVal, newVal) {
//        this.logWarn("doLinesChanged - newVal: " + newVal);
        var startLineNum = this.getLastSelectionStartLine();

        // initial setContents() only
        if (startLineNum == null) return;

        var startLine = this.getLine(startLineNum);

//        this.logWarn("startLineNum: " + startLineNum);
//        if (!startLine) this.logWarn("startLine is null");
//        else this.logWarn("startLine: " + startLine.innerHTML);
        var html = isc.emptyString;
        var selectionId = this.markCurrentSelection();
        if (isc.Browser.isIE && isc.Browser.version < 11) {
            // startLine contains everything we need - in the event that lines from the editor
            // got pasted back in, those lines appear as children of the current line and the
            // line markers will just get stripped out by unescapeHTML()
            if (!startLine) {
                this.getLineContainer().innerHTML = isc.emptyString;
                var line = this.createLine();
                this.getLineContainer().appendChild(line);
                var range = document.selection.createRange();
                range.moveToElementText(line);
                range.collapse();
                range.select();
                selectionId = this.markCurrentSelection();

                startLineNum = 0;
                startLine = this.getLine(0);
            }
            html = startLine.innerHTML;
        } else {
            var endLine = this.getSelectionStartLine();
            var endLineNum = this.getLineNumber(endLine);
            if (endLineNum < startLineNum) {
                startLine = endLine;
                startLineNum = endLineNum;
            }
//            this.logWarn("endLine: " + endLine.innerHTML);
//            this.logWarn("nextSibling: " + isc.Log.echoAll(endLine.nextSibling));
            var currentLine = startLine;
            var numLines = 0;
            while (currentLine && currentLine != endLine) {
                if (currentLine.innerHTML) {
                    html += currentLine.innerHTML;
//                    this.logWarn("html is now: " + html);
                }
                numLines++;
                currentLine = currentLine.nextSibling;
            }

            // repair bonus BR that gets shunted out of the original line span by FF
            var nextNode = endLine.nextSibling;
            if (nextNode && nextNode.tagName.toLowerCase() == "br") {
                nextNode.parentNode.removeChild(nextNode);
                endLine.appendChild(nextNode);
//                this.logWarn("repaired br, endline is now: " + endLine.innerHTML);
            }

            html += endLine.innerHTML;

            // if we pasted a line span into the middle of another line span, then it will be
            // fragmented - pick up the trailing fragment
            if (!html.replace(/\n|\r/g, isc.emptyString).match(/<br>$/i)) {
                if (endLine.nextSibling) {
                    html += endLine.nextSibling.innerHTML;
                    numLines++;
                }
            }
        }

//        this.logWarn("linesChanged: " + html);
        if (!oldVal) {
            oldVal = this.contents;
            newVal = this.getContents();
        }

        // fire linesChanged if it's defined
        if (this.linesChanged) {
            this.linesChanged(oldVal, newVal, startLineNum, numLines, html, selectionId);
        } else if (this.syntaxHiliter) {
            // currently syntaxHiliter is not compatible with linesChanged - use one or the other.
            this.doSyntaxHilite(oldVal, newVal, startLineNum, numLines, html, selectionId);
        }
    },

    doSyntaxHilite : function (oldVal, newVal, startLineNum, numLines, changedHTML, selectionId) {
        // keeping a marker in the code sent to the colorizer, breaks some colorization cases -
        // specifically, this happens if the marker is in the middle of something that would be
        // matched by a regex.  For example in XML colorization of the marker is next to the
        // equal sign in this expression: foo="bar", then that expression won't colorize until
        // something else is edited such that the selection marker moves.

//        this.logWarn("source before html removal: " + changedHTML);
        // remove markup, but keep the selectionSpan so we can extract its index for
        // repositioning the selection correctly after syntax hiliting
        var source = this.removeMarkup(changedHTML, true);

//        this.logWarn("source after html removal: " + source);

        // save off the index of the locationMarker
        var selectionMarkerIndex = this.getSelectionMarkerIndex(source);
        if (selectionMarkerIndex == -1) {
            // marker has been wiped out by a select-all, re-hilite everything
            this.doFullSyntaxHilite();
            return;
        }

        // remove the selectionMarker from the source
        source = this.removeMarkup(changedHTML);

        // if the modified source contains a token that requires us to reformat queue a full
        // hilite, but still do the partial update immediately
//        if (this.syntaxHiliter.containsMultilineToken(source)) this.queueFullHilite();

        // apply the syntax hiliting to just the lines passed in
        var newLines = this.syntaxHiliter.hilite(source, true, selectionMarkerIndex,
                                                 this._getSelectionSpanHTML(selectionId));
        this.overwriteLines(startLineNum, numLines, newLines);
        this.moveSelectionToMarker(selectionId);
    },

    doFullSyntaxHilite : function () {

//        this.logWarn("full syntax hilite running");
        var selectionId = this.markCurrentSelection();

        var contents = this._getContents();
        var source = this.removeMarkup(contents, true);

        var selectionMarkerIndex = this.getSelectionMarkerIndex(source);
        if (selectionMarkerIndex == -1) {
            selectionMarkerIndex = contents.length;
        }

        // remove the selectionMarker from the source
        source = this.removeMarkup(contents);
        this.setContents(source, true, selectionMarkerIndex, this._getSelectionSpanHTML(selectionId));

        this.moveSelectionToMarker(selectionId);

        delete this.fullHiliteTimer;
    },

    queueFullHilite : function () {
//        this.logWarn("(re)queueing full hilite");
        if (this.fullHiliteTimer) isc.Timer.clearTimeout(this.fullHiliteTimer);
        this.fullHiliteTimer = this.delayCall("doFullSyntaxHilite", [], this.fullSyntaxHiliteDelay);
    },

    selectionIsCollapsed : function () {
        if (isc.Browser._hasDOMRanges) {
            var selection = this.getContentWindow().getSelection();
            return selection.isCollapsed;
        } else if (isc.Browser.isIE) {
            var range = document.selection.createRange();
            return range.text.length == 0;
        }
    },

    rememberSelectionStartLine : function () {
        this.startLineNum = this.getLineNumber(this.getSelectionStartLine());
    },

    getLastSelectionStartLine : function () {
        return this.startLineNum;
    },

    _setPasteTimer : function () {
        this._pasteTimer = this.delayCall("doLinesChanged", [], 0);
    },

    // onpaste/onbeforepaste for IE - caching theset strings
    _getOnBeforePaste : function () {
        if (!this._onBeforePaste)
            this._onBeforePaste = this.getID()+".rememberSelectionStartLine();event.returnValue=true";
        return this._onBeforePaste;
    },
    _getOnPaste : function () {
        if (!this._onPaste) this._onPaste = this.getID()+"._setPasteTimer();event.returnValue=true"
        return this._onPaste;
    },

    // line span HTML caching
    _getLineSpanHTML : function () {
        if (!this._lineSpanHTML) {
            this._lineSpanHTML = "<span isLine='true'";
            // prevent the "bouncing line" effect where adding a space on a line that's clipped
            // causes it to be broken into two lines by the browser (since that space is the first
            // available place to wrap it).  The rendering then snaps back into a single line when
            // the syntaxHiliter converts the space into an &nbsp;
            if (this.syntaxHiliter && !this.syntaxHiliter.autoWrap)
                this._lineSpanHTML +=" style='white-space:nowrap'";
            if (isc.Browser.isIE) {
                this._lineSpanHTML += " onbeforepaste='"+this._getOnBeforePaste()
                    +"' onpaste='"+this._getOnPaste()+"'"
            }
            this._lineSpanHTML += ">$1</span>";
        }
        return this._lineSpanHTML;
    },

    createLine : function (contents) {
        var doc = this.getContentDocument();
        var line = doc.createElement("span");
        line.setAttribute("isLine", "true");
        // prevent the "bouncing line" effect where adding a space on a line that's clipped
        // causes it to be broken into two lines by the browser (since that space is the first
        // available place to wrap it).  The rendering then snaps back into a single line when
        // the syntaxHiliter converts the space into an &nbsp;
        if (this.syntaxHiliter && !this.syntaxHiliter.autoWrap)
            line.setAttribute("style", "white-space:nowrap");
        if (isc.Browser.isIE) {
            line.setAttribute("onbeforepaste", this._getOnBeforePaste());
            line.setAttribute("onpaste", this._getOnPaste());
        }
        line.innerHTML = contents ? contents : this._$br;
        return line;
    },

    // returns a string containing an incrementing counter usable unique identifier for the
    // selection span.
    _getNextSelectionId : function () {
        if (!this.selectionIdSequence) this.selectionIdSequence = 0;
        return this.getID()+"_selection_"+this.selectionIdSequence++;
    },

    // returns the DOM node that is the line span containing the start of the current selection.
    getSelectionStartLine : function () {
        var doc = this.getContentDocument();
        var line;
        if (isc.Browser._hasDOMRanges) {
            var selection = this.getContentWindow().getSelection();
            line = selection.anchorNode;
        } else if (isc.Browser.isIE) {
            var selectionId = this._getNextSelectionId();
            var range = doc.selection.createRange();
            // collapse the selection range to the start of the current selection
            range.collapse();
            range.pasteHTML("<span id='"+selectionId+"'></span>");
            var selNode = doc.getElementById(selectionId);
            line = selNode.parentNode;
//            this.logWarn("startLine: " + line.outerHTML);
            line.removeChild(selNode);
        }
//        this.logWarn("anchorNode: " + Log.echo(line));
        // IE will paste the isLine spans into the current line, so we need to find the
        // top-most element that is actually a line
        var lastLine = line;
        while(line.parentNode != null) {
            if (line.getAttribute && line.getAttribute("isLine") != null) lastLine = line;
            line = line.parentNode;
        }
        return lastLine;
    },

    _getSelectionSpanHTML : function (selectionId) {
        return "<span isSelectionSpan='true' id='"+selectionId+"'></span>";
    },
    // inserts an HTML marker at the start of the current selection.  In IE, the marker will be
    // inserted immediately before the current selection.  In FF, immediately after.  FF can be
    // made to work as IE for collapsed selections, but for multichar selections it may be
    // impossible.  See the notes below for enabling IE-style behavior in FF for collapsed
    // selections.
    markCurrentSelection : function () {
        var selectionId = this._getNextSelectionId();
        var doc = this.getContentDocument();
        if (isc.Browser._hasDOMRanges) {
            // create the selection span
            var selectionNode = doc.createElement("span");
            selectionNode.setAttribute('isSelectionSpan', "true");
            selectionNode.setAttribute('id', selectionId);

            // grab the current selection range
            var selection = this.getContentWindow().getSelection();
            var range = selection.getRangeAt(0);


            if (selection.isCollapsed) {
                range.insertNode(selectionNode);
            } else {
                // create a new range whose start and end match the selection range start boundary
                var collapsedRange = range.cloneRange();

                // collapse the new range to the end of the selection
                collapsedRange.collapse(false);

                // insert the selection node at the collapsed range
                collapsedRange.insertNode(selectionNode);
                collapsedRange.detach();

                /*
                // The code below will insert the selection marker immediately before the start
                // of the current selection, but it only works for a collapsed selection.  This
                // is because range.insertNode() puts the new node right after the start of the
                // selection which means that we need to jump the start of the selection over
                // the newly inserted node.  This works fine for a collapsed selection, but a
                // multicharacter selection gets destroyed by insertNode() - at least when the
                // start and end of the multichar selection are in one text node that is
                // fragmented by insertNode().  After insertNode(), the selection reflects
                // the character immediately before the previous start of the selection!

                // Weird FF bug - we need to clear the selection range and then re-add it after
                // we're done manipulating our clone selection - because if we don't it expands
                // to the parentNode for no apparent reason.
                selection.removeAllRanges();

                // create a new range whose start and end match the selection range start boundary
                var collapsedRange = doc.createRange();
                collapsedRange.setStart(range.startContainer, range.startOffset);
                collapsedRange.setEnd(range.startContainer, range.startOffset);

                // insert the selection node at the collapsed range
                collapsedRange.insertNode(selectionNode);
                collapsedRange.detach();


                // the above should have been all, but unfortunately the above insertion happened
                // AFTER the start of the current selection range, so now we need to move the
                // current selection range start to after the node we inserted
                    if(range.startContainer.nodeType == 3) {
                        // if it's a text node, then the above insertion fragmented the text node into
                        // a two, so just jump over the selectionNode we just inserted.
                        range.setEnd(range.startContainer.nextSibling.nextSibling, 0);
                        range.setStart(range.startContainer.nextSibling.nextSibling, 0);
                    } else {
                        range.setEndAfter(selectionNode);
                            range.setStartAfter(selectionNode);
                    }

                // add the modified range back so the cursor shows up.
                selection.addRange(range);
                */
            }
        } else if (isc.Browser.isIE) {
            var range = doc.selection.createRange();
            // collapse the selection range to the start of the current selection
            range.collapse();
            // insert our marker span
            range.pasteHTML(this._getSelectionSpanHTML(selectionId));
        }
        return selectionId;
    },

    overwriteLines : function (lineNum, numLines, newLines) {
        if (!isc.isAn.Array(newLines)) newLines = [newLines];
        var line = this.getLine(lineNum);
        while (lineNum >= 0 && (!line || !line.getAttribute || !line.getAttribute("isLine"))) {
            line = this.getLine(lineNum);
            lineNum--
        }

        if (lineNum < 0) {
//            this.logWarn("wiping lineContainer");
            this.getLineContainer().innerHTML = isc.emptyString;
            line = this.createLine();
            this.getLineContainer().appendChild(line);
            if (isc.Browser.isMoz) lineNum++;
        }

        var container = line.parentNode;
//        this.logWarn("looking for lineNum: " + lineNum + " - got: " + isc.Log.echoAll(line));
        line.innerHTML = newLines[0];
//        this.logWarn("replaced line: " + lineNum + " with: " + newLines[0]);

        // remove the numLines to replace
//        this.logWarn("removing: " + numLines + " lines");
        while (numLines != null && numLines-- > 0) {
            var removeLine = this.getLine(lineNum+1);
            if (removeLine) {
//                this.logWarn("removing line: " + removeLine.innerHTML);
//                this.logWarn("next line is: " + (removeLine.nextSibling ? removeLine.nextSibling.innerHTML:"null"));
                container.removeChild(removeLine);
            }
        }

        // add new lines
        for (var i = 1; i < newLines.length; i++) {
            if (newLines[i] != -1) this.addLineAfter(lineNum+i-1, newLines[i]);
        }
    },

    addLineAfter : function (lineNum, line) {
//        this.logWarn("addAfter: " + lineNum + " line: " + line);
        var afterLine = this.getLine(lineNum);
        var nextLine = this.getNextLine(afterLine);
        line = this.createLine(line);
        if (nextLine) {
            nextLine.parentNode.insertBefore(line, nextLine);
        } else {
            afterLine.parentNode.appendChild(line);
        }
    },

    escapeSelection : function (str, escapeValue) {
        if (escapeValue == null) escapeValue = isc.emptyString;
        return str.replace(/<span [^>]*isSelectionSpan[^>]*><\/span>/gi, escapeValue);
//        var r = new RegExp("<span [^>]*id=\"?"+selectionId+"[^>]*><\/span>", "gi");
//        return str.replace(r, escapeValue);
    },

    getSelectionMarkerIndex : function (s) {
        var regex = new RegExp("<span [^>]*isSelectionSpan[^>]*>", "i");
        var result = regex.exec(s);
        if (result) return result.index;
        return -1;
    },

    getLineNumber : function (line) {
        var peers = line.parentNode.childNodes;
        for (var i = 0; i < peers.length; i++)
            if (peers[i] == line) return i;
    },

    getPreviousLine : function (line) {
        return line.previousSibling;
    },

    getNextLine : function (line) {
        return line.nextSibling;
    },

    getLineContainer : function () {
        return this._useDesignMode() ? this.getContentBody() : this.getHandle();
    },

    getLine : function (lineNum) {
        return this.getLineContainer().childNodes[lineNum];
    },

    getLineHTML : function (line) {
        return line.innerHTML;
    },

    getLineContents : function (line) {
        return this.removeMarkup(this.getLineHTML(line));
    },

    removeMarkup : function (str, preserveSelectionSpan) {
        // FF actually inserts \n or \r in addition to <BR> on the ENTER key, so first remove
        // any literal newlines.
        //
        // IE actually inserts <FONT color="foo"></FONT> tags into the contents in some cases -
        // it appears to be tracking the currently applied style, so need to remove that HTML
        // markup here.
        if (preserveSelectionSpan) {
            // remove all tag except <br> and the selectionSpan
            str = str.replace(/\n|\r|(<\/?(?!br|BR|([^>]*isSelectionSpan)).*?>)/gi, isc.emptyString);
        } else {
            // remove all tag except <br>
            str = str.replace(/\n|\r|(<\/?(?!br|BR).*?>)/gi, isc.emptyString);
        }

        str = str.unescapeHTML();


        if (isc.Browser.isOpera) {
            var nbsp = new RegExp(String.fromCharCode(160), "g");
            str = str.replace(nbsp, " ");
        }
        return str;
    },

    // given the selectionId of a selection span in the contents, move the selection cursor to
    // that span.  After selection is moved to the span, the selectionMarker is destroyed.
    moveSelectionToMarker : function (selectionId) {
        var doc = this.getContentDocument();
        var selectionNode = doc.getElementById(selectionId);
        if (isc.Browser._hasDOMRanges) {
            var selection = this.getContentWindow().getSelection();
            selection.removeAllRanges();
            var range = doc.createRange();

            range.setStartBefore(selectionNode);
            range.setEndBefore(selectionNode);
            selection.addRange(range);
        } else if (isc.Browser.isIE) {
            var range = doc.selection.createRange();
            range.moveToElementText(selectionNode);
            range.collapse();
            range.select();
        }
        this.destroySelectionMarker(selectionId);
    },

    // destroyes the selectionMarker in the contents
    destroySelectionMarker : function (selectionId) {
        var doc = this.getContentDocument();
        var selectionNode = doc.getElementById(selectionId);
        if (selectionNode) selectionNode.parentNode.removeChild(selectionNode);
    },



    // ------------ Public Runtime API --------------

    //>@method  setEditable ()
    // Enable / disable editing of the rich text canvas.
    // @param   editable    (boolean)   True if we are enabling editing
    //<
    setEditable : function (editable) {
        //this.logWarn("setEditable" + editable);

        if (editable == this.editable) return;
        this.editable = editable;
        this._setHandleEditable(editable);
    },

    // Actually set the handle to be editable or not.
    _setHandleEditable : function (editable, initialPass) {

        if (this._useDesignMode()) {
            var cDoc = this.getContentDocument();
            if (cDoc != null) {

                if (editable || initialPass) {
                    if (isc.Browser.isIE && !isc.Browser.isIE11) cDoc.body.contentEditable = true;
                    else cDoc.designMode = "on";
                }
                // Call execCommand directly rather than using our _execCommand method as
                // we may have 'this.editable' set to false already.
                if (isc.Browser.isMoz) {
                    try {
                        cDoc.execCommand("contentReadOnly", false, !editable);
                    } catch (e) {
                        // In Firefox, attempting to set contentReadOnly to true while already
                        // read-only raises an NS_ERROR_FAILURE. Seen in Firefox 3.5.19 and 23.0.1.
                    }
                }
                if (!editable) {
                    if (isc.Browser.isIE && !isc.Browser.isIE11) cDoc.body.contentEditable = "inherit";
                    else cDoc.designMode = "off";
                }
            }
        } else {
            var handle = this.getHandle();
            if (handle != null) {

                if (isc.Browser.isIE) {
                    handle.className = "richTextEditor";
                }

                handle.contentEditable = (editable ? true : "inherit");


                if (isc.Browser.isIE) {
                    if (!this.isVisible() && this._hasSelection())
                        this._emptySelectionForHide();
                    else if (isc.Browser.version < 6)
                        this._rememberSelection();


                    if (this._editCutPasteHandler == null) {
                        this._editCutPasteHandler = isc._makeFunction("", this.getID() + "._nativeCutPaste()");
                    }
                    if (editable) {
                        handle.oncut = this._editCutPasteHandler;
                        handle.onpaste = this._editCutPasteHandler;
                        handle.onfocusout = this._editCutPasteHandler;
                    } else {
                        handle.oncut = handle.onpaste = handle.onfocusout = null;
                    }
                    if (isc.Browser.isIE && (isc.Browser.isIE9 || isc.Browser.version >= 10)) {
                        if (editable) {
                            handle.removeEventListener("DOMNodeInserted", this._editCutPasteHandler, false);
                            handle.addEventListener("DOMNodeInserted", this._editCutPasteHandler, false);
                            handle.removeEventListener("DOMNodeRemoved", this._editCutPasteHandler, false);
                            handle.addEventListener("DOMNodeRemoved", this._editCutPasteHandler, false);
                            if (isc.Browser.isIE11) {
                                handle.removeEventListener("DOMCharacterDataModified", this._editCutPasteHandler, false);
                                handle.addEventListener("DOMCharacterDataModified", this._editCutPasteHandler, false);
                            }
                        } else {
                            handle.removeEventListener("DOMNodeRemoved", this._editCutPasteHandler, false);
                            handle.removeEventListener("DOMNodeInserted", this._editCutPasteHandler, false);
                            if (isc.Browser.isIE11) {
                                handle.removeEventListener("DOMCharacterDataModified", this._editCutPasteHandler, false);
                            }
                        }
                    }
                }
            }
        }
    },


    parentVisibilityChanged : function (vis) {
        if (!this._useDesignMode() && isc.Browser.isIE && (vis == isc.Canvas.HIDDEN) &&
            this._hasSelection())
        {
            this._emptySelectionForHide();
        }
        return this.Super("parentVisibilityChanged", arguments);
    },

    // Helper method for IE to ensure that our selection is empty.
    _emptySelectionForHide : function () {

        document.body.focus();
        var focusCanvas = isc.EH.getFocusCanvas();
        if (focusCanvas != this && focusCanvas != null) {

            focusCanvas.focus();
        }
    },

    // Override disableKeyboardEvents - when disabled we always want to be non-editable
    disableKeyboardEvents : function (disabled) {
        this.Super("disableKeyboardEvents", arguments);
        // If we're editable (when enabled) update the handle to be non editable when
        // disabled (or make it editable again when enabled)
        if (this.editable) this._setHandleEditable(disabled ? false : true);
    },

    // ---------- Contents management ---------------
    // We need APIs for the developer to both set and retrieve the HTML contained in the
    // editable area.

    // _rememberContents - stores the contents of the editable area under this.contents.
    // Note: getContents() should be used rather than checking this.contents directly.
    _rememberContents : function () {
        if (!this.isDrawn() || this._drawingFrame) return;
        var contents = this._getContents();
        if (contents != null) this.contents = contents;
    },

    _getContents : function () {
        var contents;
        if (this._useDesignMode()) {
            var c_body = this.getContentBody();
            if (!c_body) return;
            contents = c_body.innerHTML;

        } else {
            var handle = this.getHandle();
            if (handle) contents = handle.innerHTML;
        }
        return contents;
    },

    //>    @method    RichTextCanvas.getContents()    ([])
    // Returns the current HTML contents of the RichTextCanvas.
    // @return (string) (possibly edited) contents
    // @see RichTextCanvas.setContents()
    //<
    getContents : function (dontRemoveMarkup) {
        this._rememberContents();

        // if a syntaxHiliter or line counting is applied, remove line and hiliting information
        // before returning the contents.
        if ((this.syntaxHiliter || this.countLines) && !dontRemoveMarkup) {
            return this.removeMarkup(this.contents);
        } else {
            return this.contents;
        }
    },

    //>    @method    RichTextCanvas.setContents()    ([])
    //      Changes the contents of a widget to newContents, an HTML string.
    //  @param    newContents    (string)    an HTML string to be set as the contents of this widget
    //  @see RichTextCanvas.getContents()
    //<
    setContents : function (contents, force, selectionMarkerIndex, selectionMarkerHTML) {
        // setContents in effect gets called twice in FF because the iframe takes a while
        // to load, so an end user calling setContents() directly effectively ends up
        // setting this.contents which is then picked up via _setupEditArea()
        if (contents == this.contents && !force) return;

        // don't hilite if we're not drawn since in that case _setContents() would just return
        this.contents = contents;
        if (!this.isDrawn() || this._drawingFrame) return;

        this._setContents(this.hiliteAndCount(contents, selectionMarkerIndex, selectionMarkerHTML));
    },

    _setContents : function (contents) {
        this.contents = contents;

        if (!this.isDrawn()) return;

        this._settingContents = true;
        if (this._useDesignMode()) {
            var c_body = this.getContentBody();
            if (c_body != null) c_body.innerHTML = contents;

        } else {
            var handle = this.getHandle();
            if (handle != null) handle.innerHTML = contents;
        }
        this._settingContents = false;

        // contents have changed, so get updated scrollHeight, check for scrollbars, etc
        this.adjustOverflow();
    },

    hiliteAndCount : function (contents, selectionMarkerIndex, selectionMarkerHTML) {
        if (this.syntaxHiliter) {
            // if a syntaxHiliter is applied, pass the new contents through it
            contents = this.syntaxHiliter.hilite(contents, false, selectionMarkerIndex, selectionMarkerHTML);
        }
        if (this.countLines) {
            // if we're initializing an empty richTextEditor and we're going to be doing
            // realtime hiliting, we must have one of our special lines in there - otherwise
            // all related logic will break.  If the contents is blank, insert a <BR> because
            // the regex immediately below relies on it and can't be efficiently changed to
            // capture this case.
            if (contents == isc.emptyString) contents = this._$br;
            contents = contents.replace(/((?:.*?<br>)|(?:.+$))/gi, this._getLineSpanHTML());
        }
        return contents;
    },

    // adds the specified contents to the innerHTML
    appendContents : function (contents, selectionMarkerIndex, selectionMarkerHTML) {
        contents = this.hiliteAndCount(contents, selectionMarkerIndex, selectionMarkerHTML);

        var handle = this._useDesignMode() ? this.getContentBody() : this.getHandle();
        handle.innerHTML += contents;

        // contents have changed, so get updated scrollHeight, check for scrollbars, etc
        this.adjustOverflow();
    },

    // --------------- Rich Text Editing Commands -------------------

    // _execCommand()   Fires the standard 'execCommand()' method to modify the editable
    // content.
    // We currently use the native 'document.execCommand()' method to perform most of our
    // actions on the text of the RTC, so most of our Rich Text APIs fall through to this
    // wrapper method.
    // Will return explicitly return 'false' if the command is not supported.

    _execCommand : function (command, valueString) {
        if (!this.isDrawn() || !this.editable) return;

        // We could use 'queryCommandEnabled()' here to determine whether the command is valid
        // given the current selection, etc.

        if (!isc.Page.isLoaded()) {
            this.logWarn("Unsupported attempt to manipulate RichTextCanvas content style " +
                         "before page load: postponed until the page has done loading.");
            isc.Page.setEvent(
                "Load",
                this.getID() + "._execCommand('"
                                    + command + "','" + valueString + "');"
            );
            return;
        }

        // Ensure we have focus and are selected.
        this.focus();

        var designMode = this._useDesignMode(),
            doc =  designMode ? this.getContentDocument() : document;

        if (!doc) return;

        // If the command is unsupported, return false to notify callers.
        if (!this._commandEnabled(command)) return false;




        try {
            doc.execCommand(command, false, valueString);
        } catch (e) {
            return false;
        }


        if (designMode) {
            // put focus into the window so the user can continue typing and have
            // an effect.
            var cw = this.getContentWindow();
            cw.focus();

        } else {

            // text manipulation is likely to have changed the text insertion point.
            this._rememberSelection();

        }

        // Fire _contentsChanged() - it is likely that the execCommand changed the content
        // of the RTC.
        this._contentsChanged();
    },

    // Helper method to test for command being enabled
    _commandEnabled : function (command)  {
        try {
            var doc = this._useDesignMode() ? this.getContentDocument() : document;
            if (!doc) return false;
            if (!doc.queryCommandEnabled(command)) return false;
        } catch (e) {
            return false;
        }

        return true;
    },

    //>@method  RichTextCanvas.boldSelection
    //  Toggle whether the current text selection is bold or not bold
    //<
    boldSelection : function () {
        this._execCommand("bold")
    },

    //>@method  RichTextCanvas.italicSelection
    //  Toggle whether the current text selection is italic or not
    //<
    italicSelection : function () {
        this._execCommand("italic");
    },

    //>@method  RichTextCanvas.underlineSelection
    //  Toggle whether the current text selection is underlined or not
    //<
    underlineSelection : function () {
        this._execCommand("underline");
    },

    //>@method  RichTextCanvas.strikethroughSelection
    //  Toggle whether the current text selection is strikethrough or not
    //<
    strikethroughSelection : function () {
        this._execCommand("strikethrough");
    },

    //>@method  RichTextCanvas.showClipboardDisabledError
    //  For some browsers the clipboard functions (cut/copy/paste) are disabled by default.
    //  We catch these cases from cutSelection() / copySelection() / pasteOverSelection() and
    //  call this method to warn the user. Default behavior shows a warn dialog with the text
    // <code>"Your browser does not allow web pages to access the clipboard programmatically."</code>
    // May be overridden to change this behavior.
    // @visibility editor_clipboard
    //<
    // Cut/Copy/Paste all disabled by default in Moz.
    // In Safari Cut/Copy work, but Paste always fails.
    // In IE all three methods are enabled and work by default.
    showClipboardDisabledError : function () {

        var errorMessage = "Your browser does not allow web pages to access the clipboard programmatically.";

        // Mozilla allows you to turn on clipboard access for scripts but it's somewhat complex
        // and you can only turn it on for specific URLs
        // There's a note on this at mozilla.org: http://www.mozilla.org/editor/midasdemo/securityprefs.html
        // NOTE: the freeware HTMLArea application available at:
        //  http://www.dynarch.com/projects/htmlarea/   or
        //  http://www.postnukepro.com/modules/pagesetter/guppy/HTMLArea30beta/
        // points the user directly to this page. We currently don't because:
        //  - the steps to enable the script access are complex
        //  - the explanation specifically refers to the Moz "midas" demo app
        //  - we don't know of any way to turn on "Paste" access in Safari.
        isc.warn(errorMessage);
    },

    //>@method  RichTextCanvas.copySelection
    //  Copy the current text selection to the clipboard.
    // @visibility editing_clipboard
    //<

    copySelection : function () {
        if (this._execCommand("copy") == false) this.showClipboardDisabledError();
    },

    //>@method  RichTextCanvas.cutSelection
    //  Copy the current text selection to the clipboard, and remove it from the edit area.
    // @visibility editing_clipboard
    //<
    cutSelection : function () {
        if (this._execCommand("cut") == false) this.showClipboardDisabledError();;
    },

    //>@method  RichTextCanvas.pasteOverSelection
    //  Paste the current clipboard text over the selection
    // @visibility editing_clipboard
    //<

    pasteOverSelection : function () {
        if (this._execCommand("paste") == false) this.showClipboardDisabledError();
    },

    //>@method  RichTextCanvas.deleteSelection
    //  Delete the currently selected text
    //<
    deleteSelection : function () {
        this._execCommand("delete");
    },

    //> @method richTextCanvas.indentSelection
    //  Increases the indent for the currently selected paragraph.  Within a list, increases the
    //  list level.
    // @visibility external
    //<

    indentSelection : function () {
        this._execCommand("indent");
    },

    //> @method richTextCanvas.outdentSelection
    //  Decreases the indent for the currently selected paragraph.  Within a list, decreases the
    //  list level or breaks out of the list.
    // @visibility external
    //<
    outdentSelection : function () {
        this._execCommand("outdent");
    },

    //> @method RichTextCanvas.insertHTML()
    // Replaces the current selection with the given HTML.
    // @param html (HTMLString) the HTML to insert. This must be a valid HTML string.
    //<
    insertHTML : function (html) {
        // IE does not support the insertHTML command.
        if (isc.Browser.isIE) {
            var contentDoc = this.getContentDocument();


            if (isc.Browser.version < 11) {
                var range = contentDoc.selection.createRange(),
                    parentElem = parentElem = range.parentElement();
                range.pasteHTML(html);


                if (parentElem != null) {
                    isc.Element.forEachDescendantHavingClass(parentElem, "_isc_tab", isc.RichTextCanvas._fixTabSpan);
                }

            // IE 11

            } else {
                var sel = contentDoc.getSelection(),
                    range;
                if (sel.rangeCount <= 0) {
                    range = contentDoc.createRange();
                    var lineContainer = this.getLineContainer();
                    if (!lineContainer.lastChild) {
                        lineContainer.appendChild(contentDoc.createTextNode(""));
                    }
                    range.selectNode(lineContainer.lastChild);
                    range.collapse(false);
                } else {
                    range = sel.getRangeAt(0);
                }
                range.deleteContents();
                var docFragment = range.createContextualFragment(html);
                range.insertNode(docFragment);
                range.collapse(false);
                sel.removeAllRanges();
                sel.addRange(range);
            }

        // Use the 'insertHTML' command
        } else {
            this._execCommand("insertHTML", html);
        }
    },

    //>@method  RichTextCanvas.justifySelection
    //  Applies the alignment / justification passed in to the selected paragraph.
    //  Options are "right", "left", "center" (for alignment), and "full" for fully justified
    //  text.
    // @param justification (string)    What justification should be applied to the text.
    //<
    justifySelection : function (justification) {
        if (justification == isc.RichTextCanvas.CENTER) {
            this._execCommand("justifycenter");

        } else if (justification == isc.RichTextCanvas.FULL) {
            this._execCommand("justifyfull");

        } else if (justification == isc.RichTextCanvas.RIGHT) {
            this._execCommand("justifyright");

        } else if (justification == isc.RichTextCanvas.LEFT) {
            this._execCommand("justifyleft");


        /*
        } else {
            this._execCommand("justifynone");
        */

        }
    },

    //>@method  RichTextCanvas.setSelectionColor
    //  Set the font color for the selected text.   Takes the desired color as a parameter.
    // @param color (string)    Color to apply to the text.
    //<
    setSelectionColor : function (color) {
        this._execCommand("forecolor", color);
    },

    //>@method  RichTextCanvas.setSelectionBackgroundColor
    //  Set the background color for the selected text.   Takes the desired color as a parameter.
    // @param color (string)    Color to apply to the text background.
    //<
    setSelectionBackgroundColor : function (color) {
        // In Moz "backcolor" will style the entire containing IFRAME - while 'hilitecolor'
        // will set the background color for just the selected text.
        var command = isc.Browser.isMoz ? "hilitecolor" : "backcolor";
        this._execCommand(command, color);
    },

    //>@method  RichTextCanvas.setSelectionFont
    //  Set the font for the selected text.   Takes the name of a font as a parameter.
    // @param font (string)    Font to apply to the selection
    //<
    setSelectionFont : function (font) {
        this._execCommand("fontname", font);
    },

    //>@method  RichTextCanvas.setSelectionFontSize
    //  Set the size of the font for the selected text. Takes a number between 1 and 7.
    // @param size (number)    Desired font size - a value between 1 and 7.
    //<
    setSelectionFontSize : function (size) {
        this._execCommand("fontsize", size);
    },

    createLink : function (url) {
        this._execCommand("CreateLink", url);
    },

    _getListElementFromSelection : function () {
        var doc = this.getContentDocument();

        if (!isc.Browser._hasDOMRanges) {
            var selElement = null;
            if (this._savedSelection) {
                selElement = this._savedSelection.parentElement();
            } else {
                selElement = isc.Element._getElementFromSelection(doc);
            }
            for (var elem = selElement; elem != null; elem = elem.parentNode) {
                if (elem.nodeType != 1) continue;
                if (elem.tagName === "OL" || elem.tagName === "UL") {
                    return elem;
                }
            }

        } else {
            // Find the lowest common ancestor <ol> or <ul> element. First traverse the ancestors
            // of the current selection's anchorNode to build a list of ancestor <ol> or <ul> elements.
            // Then traverse the ancestors of the current selection's focusNode looking for one
            // of these <ol> or <ul> elements.
            var selectionAnchorNode,
                selectionFocusNode;

            if (this._useDesignMode() || this._savedSelection == null) {
                var selection = doc.defaultView.getSelection();
                selectionAnchorNode = selection.anchorNode;
                selectionFocusNode = selection.focusNode;

            // When using contentEditable, we need to use the last-saved selection's anchorNode
            // and focusNode because when this method is called in response to clicking on the
            // List Properties editor control button, the selection has already moved to the
            // now-focused button.
            } else {
                selectionAnchorNode = this._savedSelectionAnchorNode;
                selectionFocusNode = this._savedSelectionFocusNode;
            }

            var anchorNodeListAncestors = [];

            var elem = selectionAnchorNode;
            for (; elem != null; elem = elem.parentNode) {
                if (elem.nodeType != 1) continue;
                if (elem.tagName === "OL" || elem.tagName === "UL") {
                    anchorNodeListAncestors.add(elem);
                }
            }

            elem = selectionFocusNode;
            for (; elem != null; elem = elem.parentNode) {
                if (elem.nodeType != 1) continue;
                if (elem.tagName === "OL" || elem.tagName === "UL") {
                    if (anchorNodeListAncestors.contains(elem)) {
                        return elem;
                    }
                }
            }
        }

        return null;
    },

    //> @method richTextCanvas.convertSelectionToOrderedList()
    // Converts the selection to an ordered list. If the selection is within an unordered list,
    // the unordered list is converted to an ordered list.
    // @visibility external
    //<
    convertSelectionToOrderedList : function () {
        var listElem = this._getListElementFromSelection();
        if (listElem != null) {

            var listProperties = this.getListProperties(listElem),
                isUnordered = isc.ListPropertiesPane.getListType(listProperties) == "unordered";
            if (isUnordered) {
                this.applyListProperties({
                    style: "decimal"
                }, listElem);

            // Already an ordered list. Ignore.
            } else {
                /*empty*/
            }
        } else {
            this._execCommand("insertOrderedList");
        }
    },

    //> @method richTextCanvas.convertSelectionToUnorderedList()
    // Converts the selection to an unordered list. If the selection is within an ordered list,
    // the ordered list is converted to an unordered list.
    // @visibility external
    //<
    convertSelectionToUnorderedList : function () {
        var listElem = this._getListElementFromSelection();
        if (listElem != null) {
            var listProperties = this.getListProperties(listElem),
                isUnordered = isc.ListPropertiesPane.getListType(listProperties) == "unordered";
            if (!isUnordered) {
                this.applyListProperties({
                    style: "disc"
                }, listElem);

            // Already an unordered list. Ignore.
            } else {
                /*empty*/
            }
        } else {
            this._execCommand("insertUnorderedList");
        }
    },

    //> @method richTextCanvas.getListProperties()
    // @return (ListProperties) the list configuration properties of the currently selected list,
    // or null if no list is selected.
    //<
    getListProperties : function (listElem) {
        listElem = listElem || this._getListElementFromSelection();
        var returnVal = null;
        if (listElem != null) {
            var cs = isc.Element.getComputedStyle(listElem, {
                listStyleType: "list-style-type",
                listStyleImage: "list-style-image"
            });
            var style = cs.listStyleType,
                image = cs.listStyleImage;
            if (image && image != "none") {
                style = "custom-image";
                // Trim off the 'url(' prefix and ')' suffix.
                image = image.substring(4, image.length - 1);
                // In IE and Firefox, need to also trim double quotes around the URL.

                if (image.length >= 2 && image.charAt(0) == '"') {
                    image = image.substring(1, image.length - 1);
                }
            }
            returnVal = {
                style: style,
                image: image
            };
            if (listElem.start != "" &&
                listElem.start != null)
            {
                returnVal.startNumber = listElem.start << 0;
            }
        }

        return returnVal;
    },

    //> @method richTextCanvas.applyListProperties() [A]
    // Applies the given +link{ListProperties} to the currently selected HTML list, if any.
    // If there is no list selected, then this method does nothing.
    // @param listProperties (ListProperties) the list configuration to apply
    // @visibility external
    //<
    applyListProperties : function (listProperties, listElem) {
        listElem = listElem || this._getListElementFromSelection();
        if (listElem != null) {
            listProperties = isc.ListPropertiesPane.getCanonicalListProperties(listProperties);

            var isUnordered = isc.ListPropertiesPane.getListType(listProperties) == "unordered";

            // Do we need to change a <ul> to <ol> or vice versa?

            if ((isUnordered && listElem.tagName != "UL") ||
                (!isUnordered && listElem.tagName != "OL"))
            {
                var doc = listElem.ownerDocument,
                    newListElem = doc.createElement(isUnordered ? "UL" : "OL");
                // Transfer all children of the current list element to the new list element.
                var child;
                while ((child = listElem.firstChild) != null) {
                    newListElem.appendChild(child);
                }

                // Copy attributes (except possibly `start').
                if (listElem.attributes) {
                    for (var i = 0, len = listElem.attributes.length; i < len; ++i) {
                        var attr = listElem.attributes[i];
                        if (!isUnordered || attr.name != "start") {
                            newListElem.setAttributeNode(attr.cloneNode());
                        }
                    }
                }

                listElem.parentNode.replaceChild(newListElem, listElem);
                listElem = newListElem;
            }

            listElem.removeAttribute("start");

            if (isUnordered) {
                if (listProperties.style == "custom-image") {
                    listElem.style.listStyle = "url(\"" + this.getImgURL(listProperties.image) + "\")";
                } else {
                    listElem.style.listStyle = listProperties.style;
                }
            } else {
                listElem.style.listStyle = listProperties.style;
                if (listProperties.startNumber != null) {
                    listElem.start = listProperties.startNumber << 0;
                }
            }
        }
    }


});

isc.RichTextCanvas.registerStringMethods({
    changed : "oldValue,newValue"
});
//!<Deferred






//>    @class RichTextEditor
// RichTextEditing component.  Provides a rich-text editing area along with UI for executing
// rich-text commands on selected content.
// <p>
// The HTML generated from this component may vary by browser, and, as with any HTML
// value created on the client, we recommend values be sanitized on the server before
// storing and displaying to other users.
//
// @treeLocation Client Reference/Foundation
// @visibility external
// @example RichTextEditor
//<

isc.ClassFactory.defineClass("RichTextEditor", "VLayout");


isc.RichTextEditor.addProperties({

    // Edit Area config

    editAreaConstructor: "RichTextCanvas",

    //> @attr richTextEditor.editArea (AutoChild Canvas : null : R)
    // The edit canvas created automatically for this RichTextEditor.
    // @visibility external
    //<

    //> @attr richTextEditor.editAreaBackgroundColor (String : "white" : IR)
    // Background color for the +link{richTextEditor.editArea, edit canvas}.
    // @visibility external
    //<
    editAreaBackgroundColor: "white",

    //> @attr richTextEditor.editAreaClassName (String : null : IR)
    // Edit Area can have a custom class applied.
    //<
    editAreaClassName: "normal",

    //> @attr richTextEditor.value (String : "" : IRW)
    // Initial value for the edit area.    Use <code>getValue()</code> and
    // <code>setValue()</code> to update at runtime.
    // @visibility external
    //<
    value: "",

    // General toolbar config

    //> @attr richTextEditor.toolArea (AutoChild Layout : null : R)
    // Layout used to contain all of the +link{richTextEditor.toolbar, toolbar} AutoChildren
    // that contain the +link{controlGroups}.
    // @visibility external
    //<
    toolAreaDefaults: {
        _constructor: "VLayout",
        width: "100%",
        overflow: isc.Canvas.VISIBLE
    },

    //> @attr richTextEditor.toolbar (MultiAutoChild Layout : null : R)
    // Layout used to contain each of the +link{controlGroups}.
    // @visibility external
    //<
    toolbarConstructor: "HLayout",
    toolbarDefaults: {
        defaultLayoutAlign: isc.Canvas.CENTER,

        // explicitly suppress printing the toolbar by default
        shouldPrint: false,

        // Make the toolbar overflow:"visible" - if it exceeds the availableSpace we'll allow
        // the editor itself to decide whether it should be clipped
        overflow: isc.Canvas.VISIBLE
    },

    toolbarHeight: 24, // should be less but figure this out later!

    //> @attr RichTextEditor.toolbarBackgroundColor  (string : "#CCCCCC" : [IR])
    //  The background color for the toolbar.
    // @visibility external
    //<
    toolbarBackgroundColor: "#CCCCCC",

    toolbarSeparatorSrc: "[SKIN]/RichTextEditor/separator.png",


    // Default width for control buttons
    controlButtonWidth: 20,

    //> @attr richTextEditor.defaultControlConstructor (SCClassName : "Button" : IRA)
    // By default our 'controls' will be of this specified class. Override for specific
    // controls by either implementing a '[controlName]_autoMaker' function which returns the
    // control, or by specifying '[controlName]Constructor' as a pointer to an appropriate
    // SmartClient class.
    //<
    defaultControlConstructor: isc.Button,

    //> @type StandardControlGroup
    // @value "fontControls" +link{RichTextEditor.fontControls,Font controls}
    // @value "formatControls" +link{RichTextEditor.formatControls,Text formatting controls}
    // @value "styleControls" +link{RichTextEditor.styleControls,Text styling controls}
    // @value "colorControls" +link{RichTextEditor.colorControls,Color controls}
    // @value "bulletControls" +link{RichTextEditor.bulletControls,HTML list controls}
    // @visibility external
    //<
    // @value "editControls" - omitted, not functional across all browsers

    //> @attr richTextEditor.controlGroups (Array : ["fontControls", "formatControls", "styleControls", "colorControls"] : IRA)
    // An array of control groups specifying which groups of controls should be included in the
    // editor tool area. The values of this array may be the name of a control group such as
    // one of the +link{StandardControlGroup}s, a +link{Canvas}, or the special string "break"
    // which causes the subsequent control groups to continue onto a new line.
    // <smartclient>
    // <p>
    // For each control group name, this[controlGroupName] should be defined as an array of
    // +link{type:ControlName}s or Canvas instances. This allows the controls of a control
    // group to be customized.
    // </smartclient>
    //
    // @visibility external
    // @example RichTextEditor
    //<
    controlGroups: [
        "fontControls", "formatControls", "styleControls", "colorControls"
        // ,"editControls"  // Don't show the edit controls by default as they're disabled
                            // on Moz and Safari.
    ],

    //>    @type ControlName
    // Names for the standard controls built into the RichTextEditor.  You can use these
    // <code>ControlNames</code> in APIs like +link{richTextEditor.styleControls} to control
    // the order in which controls appear, to omit default controls or to show controls that
    // are not shown by default.
    // <p>
    // Every <code>ControlName</code> is also the name of an +link{AutoChild}, so all the
    // built-in controls can be skinned or otherwise customized via the
    // +link{group:autoChildUsage,AutoChild system}. <smartgwt>Note that the AutoChild
    // name in each case is the camelCaps version of the <code>ControlName</code> value.  For
    // example, use "boldSelection" as the name of the AutoChild for the bold button, not
    // "BOLDSELECTION".</smartgwt>
    //
    // @value "boldSelection"  A button to make the current selection bold.
    // @value "italicSelection"  A button to make the current selection italic.
    // @value "underlineSelection" A button to make the current selection underlined.
    // @value "fontSelector" A select item allowing the user to change the font of the current
    // text selection.
    // @value "fontSizeSelector" A select item allowing the user to change the font size of the
    // current text selection.
    // @value "alignLeft" A button to left-align the selected text.
    // @value "alignRight" A button to right-align the selected text.
    // @value "alignCenter" A button to center the selected text.
    // @value "justify" A button to justify the selected line of text.
    // @value "color" A color-picker allowing the user to set the text color.
    // @value "backgroundColor" A color picker allowing the user to set the text background
    // color.
    // @value "indent" Within text, indents the paragraph. Within a list, increases the list
    // level.
    // @value "outdent" Within text, outdents the paragraph. Within a list, decreases the list
    // level.
    // @value "orderedList" Turns the current selection into an ordered list (HTML &lt;ol&gt;)
    // or converts an unordered list to an ordered list.
    // @value "unorderedList" Turns the current selection into an unordered list (HTML &lt;ul&gt;)
    // or converts an ordered list to an unordered list.
    // @value "listProperties" Shows the +link{RichTextEditor.listPropertiesDialog,listPropertiesDialog}
    // to allow configuring the options of the currently selected HTML list.
    // @visibility external
    //<
    // In addition to the standard ControlNames, custom controls can be added.
    // To add a custom control simply add a new control name (string) to a controlGroup.
    // By default the control will show up as a button with width specified by
    // <code>richTextCanvas.controlButtonWidth</code>.<br>
    // Properties for each control will be picked up from <code>this.[ControlName]Defaults</code>
    // and <code>this.[ControlName]Properties</code>.<br>
    // If no click handler is specified in these property blocks, click will call
    // <code>fireAction()</code> on this editor, passing in the ControlName as an action to fire.<br>
    //
    //  Note - for custom click-handling purposes, default control buttons are created with
    //  a 'creator' property which points back to the richTextEditor that created them.<br>
    //  For completely custom controls to be included in the toolbar, define a method named
    //  [ControlName]_autoMaker on the RichTextEditor instance. This method should return a
    //  widget instance, which will then be added to the toolbar in the appropriate position.

    // Style Control Config --------------------------------------

    //> @attr richTextEditor.styleControls (Array of ControlName : ["boldSelection", "italicSelection", "underlineSelection"] : IRA)
    // Default text styling control group. Consists of an array of +link{type:ControlName}s
    // and/or +link{Canvas} instances. To display this group of controls for some RichTextEditor,
    // include <code>"styleControls"</code> in the +link{RichTextEditor.controlGroups} array.
    // @visibility external
    //<
    styleControls: [
        "boldSelection", "italicSelection", "underlineSelection"
    ],

    //> @attr richTextEditor.fontSelectorItem (AutoChild SelectItem : null : IR)
    // The +link{type:AutoChild} +link{class:SelectItem} used for choosing the font to apply
    // to the current selection.
    // @group i18nMessages
    // @visibility external
    //<

    //> @attr richTextEditor.fontSizeSelectorItem (AutoChild SelectItem : null : IR)
    // The +link{type:AutoChild} +link{class:SelectItem} used for choosing the font-size to
    // apply to the current selection.
    // @group i18nMessages
    // @visibility external
    //<

    //> @attr richTextEditor.fontSelectorPrompt (String : "Set Font..." : IRW)
    // The prompt for the built-in +link{richTextEditor.fontSelectorItem, font selector}.
    // @group i18nMessages
    // @visibility external
    //<
    fontSelectorPrompt: "Set Font...",
    //> @attr richTextEditor.fontSizeSelectorPrompt (String : "Set Font Size..." : IRW)
    // The prompt for the built-in +link{richTextEditor.fontSizeSelectorItem, font-size selector}.
    // @group i18nMessages
    // @visibility external
    //<
    fontSizeSelectorPrompt: "Set Font Size...",

    //> @attr richTextEditor.linkUrlTitle (String : "Hyperlink URL:" : IRW)
    // The prompt displayed when editing a hyperlink.
    // @group i18nMessages
    // @visibility external
    //<
    linkUrlTitle: "Hyperlink URL:",

    // Properties to apply to the style controls.
    // These are picked up based on their name.
    //  NOTE: on a per-instance basis we also pick up this.boldSelectionProperties, etc.

    //> @attr richTextEditor.boldSelectionPrompt (String : "Make selection bold" : IRW)
    // The prompt for the built-in +link{type:ControlName, boldSelection} control.
    // @group i18nMessages
    // @visibility external
    //<
    boldSelectionPrompt: "Make selection bold",
    boldSelectionDefaults: { title: "<b>B</b>" },
    //> @attr richTextEditor.italicSelectionPrompt (String : "Make selection italic" : IRW)
    // The prompt for the built-in +link{type:ControlName, italicSelection} control.
    // @group i18nMessages
    // @visibility external
    //<
    italicSelectionPrompt: "Make selection italic",
    italicSelectionDefaults: { title: "<i>I</i>" },
    //> @attr richTextEditor.underlineSelectionPrompt (String : "Make selection underlined" : IRW)
    // The prompt for the built-in +link{type:ControlName, underlineSelection} control.
    // @group i18nMessages
    // @visibility external
    //<
    underlineSelectionPrompt: "Make selection underlined",
    underlineSelectionDefaults: { title: "<u>U</u>" },
    //> @attr richTextEditor.strikethroughSelectionPrompt (String : "Strike through selection" : IRW)
    // The prompt for the built-in +link{type:ControlName, strikethroughSelection} control.
    // @group i18nMessages
    // @visibility external
    //<
    strikethroughSelectionPrompt: "Strike through selection",
    strikethroughSelectionDefaults: { title: "<del>S</del>" },

    // Font Control Config --------------------------------------

    //> @attr richTextEditor.fontControls (Array of ControlName : ["fontSelector", "fontSizeSelector"] : IRA)
    // Default font control group. Consists of an array of +link{type:ControlName}s and/or
    // +link{Canvas} instances. To display this group of controls for some RichTextEditor,
    // include <code>"fontControls"</code> in the +link{RichTextEditor.controlGroups} array.
    // @visibility external
    //<

    fontControls: [
        "fontSelector", "fontSizeSelector"
    ],

    // TODO: May 2015 - expose the various selectors and buttons as autoChildren

    // Specify the constructor function for the two dynamic form type items

    fontSelectorConstructor: isc.DynamicForm,
    fontSizeSelectorConstructor: isc.DynamicForm,

    //> @attr richTextEditor.fontNames (Object : {} : [IRA])
    // ValueMap of css fontName properties to font name titles to display in the font selector
    // if <code>"fontSelector"</code> is included in +link{RichTextEditor.controlGroups}
    // for this editor.
    // Default value for this attribute:<br>
    // <code> {
    // &nbsp;&nbsp;"arial,helvetica,sans-serif": "Arial",
    // &nbsp;&nbsp;'courier new,courier,monospace': "Courier New",
    // &nbsp;&nbsp;'georgia,times new roman,times,serif': "Georgia",
    // &nbsp;&nbsp;'tahoma,arial,helvetica,sans-serif': "Tahoma",
    // &nbsp;&nbsp;'times new roman,times,serif': "Times New Roman",
    // &nbsp;&nbsp;'verdana,arial,helvetica,sans-serif': "Verdana",
    // &nbsp;&nbsp;"impact": "Impact"}</code>
    // @visibility external
    //<
    //  The default <code>createFontSelector()</code> method will apply this valueMap to the
    //  select item created as the <code>fontSelector</code> control.
    //<
    fontNames: {
        "arial,helvetica,sans-serif": "Arial",
        'courier new,courier,monospace': "Courier New",
        'georgia,times new roman,times,serif': "Georgia",
        'tahoma,arial,helvetica,sans-serif': "Tahoma",
        'times new roman,times,serif': "Times New Roman",
        'verdana,arial,helvetica,sans-serif': "Verdana",
        "impact": "Impact"
    },

    //> @attr richTextEditor.fontSizes (Object : {} : [IRA])
    // ValueMap of css font size property values to font size titles to display in the font size
    // selector if <code>"fontSizeSelector"</code> is included in
    // +link{RichTextEditor.controlGroups}.
    // Default value for this attribute:<br>
    // <code>{
    // &nbsp;&nbsp;"1": "1 (8 pt)",
    // &nbsp;&nbsp;"2": "2 (10 pt)",
    // &nbsp;&nbsp;"3": "3 (12 pt)",
    // &nbsp;&nbsp;"4": "4 (14 pt)",
    // &nbsp;&nbsp;"5": "5 (18 pt)",
    // &nbsp;&nbsp;"6": "6 (24 pt)",
    // &nbsp;&nbsp;"7": "7 (36 pt)"}</code>
    // @visibility external
    //<
    //  The default <code>createFontSizeSelector()</code> method will apply this valueMap to the
    //  select item created as the <code>fontSizeSelector</code> control.
    fontSizes: {
        "1": "1 (8 pt)",
        "2": "2 (10 pt)",
        "3": "3 (12 pt)",
        "4": "4 (14 pt)",
        "5": "5 (18 pt)",
        "6": "6 (24 pt)",
        "7": "7 (36 pt)"
    },

    // Edit Control Config --------------------------------------
    // (Note: edit controls are hidden by default, as cut, copy, paste are disabled for
    //  security reasons in Moz by default, and paste appears to never be supported in Safari).

    //> @attr richTextEditor.editControls (Array of ControlName : [...] : [IRA])
    // Edit control group. Consists of an array of +link{type:ControlName}s.
    //<
    // Leave this @visibility internal until for now.
    editControls: [
        "copySelection", "cutSelection", "pasteSelection"
    ],

    // Defaults for the cut/copy/paste buttons
    //> @attr richTextEditor.copySelectionPrompt (String : "Copy selection" : IRW)
    // The prompt for the built-in +link{type:ControlName, copySelection} control.
    // @group i18nMessages
    // @visibility external
    //<
    copySelectionPrompt: "Copy selection",
    copySelectionDefaults: { icon: "[SKIN]/RichTextEditor/copy.png" },
    //> @attr richTextEditor.cutSelectionPrompt (String : "Cut selection" : IRW)
    // The prompt for the built-in +link{type:ControlName, cutSelection} control.
    // @group i18nMessages
    // @visibility external
    //<
    cutSelectionPrompt: "Cut selection",
    cutSelectionDefaults: { icon: "[SKIN]/RichTextEditor/cut.png" },
    //> @attr richTextEditor.pasteSelectionPrompt (String : "Paste" : IRW)
    // The prompt for the built-in +link{type:ControlName, pasteSelection} control.
    // @group i18nMessages
    // @visibility external
    //<
    pasteSelectionPrompt: "Paste",
    pasteSelectionDefaults: { icon: "[SKIN]/RichTextEditor/paste.png" },


    // Format Control Config --------------------------------------

    //> @attr richTextEditor.formatControls (Array of ControlName : ["alignLeft", "alignRight", "alignCenter", "justify"] : IRA)
    // Default text formatting control group. Consists of an array of +link{type:ControlName}s
    // and/or +link{Canvas} instances. To display this group of controls for some RichTextEditor,
    // include <code>"formatControls"</code> in the +link{RichTextEditor.controlGroups} array.
    // @visibility external
    //<
    formatControls: [
        "alignLeft", "alignRight", "alignCenter", "justify"
    ],

    // Note: click is overridden on the various "justify..." controls as they are going to
    // call 'justifySelection(...)' passing in a parameter to specify the desired justification.

    //> @attr richTextEditor.alignLeftPrompt (String : "Left align selection" : IRW)
    // The prompt for the built-in +link{type:ControlName, alignLeft} control.
    // @group i18nMessages
    // @visibility external
    //<
    alignLeftPrompt: "Left align selection",
    alignLeftDefaults: {
        icon: "[SKIN]/RichTextEditor/text_align_left.png",
        click : function () { this.creator.fireAction('justifySelection', 'left'); }
    },
    //> @attr richTextEditor.alignCenterPrompt (String : "Center selection" : IRW)
    // The prompt for the built-in +link{type:ControlName, alignCenter} control.
    // @group i18nMessages
    // @visibility external
    //<
    alignCenterPrompt: "Center selection",
    alignCenterDefaults: {
        icon: "[SKIN]/RichTextEditor/text_align_center.png",
        click : function () { this.creator.fireAction('justifySelection', 'center'); }
    },
    //> @attr richTextEditor.alignRightPrompt (String : "Right align selection" : IRW)
    // The prompt for the built-in +link{type:ControlName, alignRight} control.
    // @group i18nMessages
    // @visibility external
    //<
    alignRightPrompt: "Right align selection",
    alignRightDefaults: {
        icon: "[SKIN]/RichTextEditor/text_align_right.png",
        click : function () { this.creator.fireAction('justifySelection', 'right'); }
    },
    //> @attr richTextEditor.justifyPrompt (String : "Full justify selection" : IRW)
    // The prompt for the built-in +link{type:ControlName, justify} control.
    // @group i18nMessages
    // @visibility external
    //<
    justifyPrompt: "Full justify selection",
    justifyDefaults: {
        icon: "[SKIN]/RichTextEditor/text_align_justified.png",
        click : function () { this.creator.fireAction('justifySelection', 'full'); }
    },

    // Color Control Config --------------------------------------

    //> @attr richTextEditor.colorControls (Array of ControlName : ["color", "backgroundColor"] : IRA)
    // Control group for modifying text color / background color.
    // Consists of an array of +link{type:ControlName}s and/or +link{Canvas} instances.
    // To display this group of controls for some RichTextEditor,
    // include <code>"formatControls"</code> in the +link{RichTextEditor.controlGroups} array.
    // @visibility external
    //<
    colorControls: [
        "color", "backgroundColor"
    ],

    // color / background color defaults override click handling to prompt the user for a
    // color and apply it to the selection.

    //> @attr richTextEditor.colorPrompt (String : "Set selection text color" : IRW)
    // The prompt for the built-in +link{type:ControlName, color} control.
    // @group i18nMessages
    // @visibility external
    //<
    colorPrompt: "Set selection text color",
    colorDefaults: {
        icon: "[SKIN]/RichTextEditor/text_color.gif",
        click : "this.creator.chooseTextColor()"
    },

    //> @attr richTextEditor.backgroundColorPrompt (String : "Set selection background color" : IRW)
    // The prompt for the built-in +link{type:ControlName, backgroundColor} control.
    // @group i18nMessages
    // @visibility external
    //<
    backgroundColorPrompt: "Set selection background color",
    backgroundColorDefaults: {
        icon: "[SKIN]/RichTextEditor/background_color.gif",
        click : "this.creator.chooseBackgroundColor()"
    },


    insertControls: [
        "link"
    ],

    //> @attr richTextEditor.linkPrompt (String : "Edit Hyperlink" : IRW)
    // The prompt for the built-in +link{type:ControlName, hyperlink} control.
    // @group i18nMessages
    // @visibility external
    //<
    linkPrompt: "Edit Hyperlink",
    linkDefaults: {
        icon: "[SKIN]/RichTextEditor/link_new.png",
        click : "this.creator.createLink()"
    },


    // Lists Config -----------------------------------------------
    //> @attr RichTextEditor.bulletControls (Array of ControlName : ["indent", "outdent", "orderedList", "unorderedList", "listProperties"] : IRA)
    // Default HTML list control group. Consists of an array of +link{type:ControlName}s and/or
    // +link{Canvas} instances. To display this group of controls for some RichTextEditor,
    // include <code>"bulletControls"</code> in the +link{RichTextEditor.controlGroups} array.
    // @visibility external
    //<
    bulletControls: [
        "indent", "outdent", "orderedList", "unorderedList", "listProperties"
    ],

    //> @attr richTextEditor.indentPrompt (String : "Increase indent" : IRW)
    // The prompt for the built-in +link{type:ControlName, indent} control.
    // @group i18nMessages
    // @visibility external
    //<
    indentPrompt: "Increase indent",
    indentDefaults: {
        icon: "[SKIN]/RichTextEditor/indent.png",
        click : "this.creator.indentSelection()"
    },

    //> @attr richTextEditor.outdentPrompt (String : "Decrease indent" : IRW)
    // The prompt for the built-in +link{type:ControlName, outdent} control.
    // @group i18nMessages
    // @visibility external
    //<
    outdentPrompt: "Decrease indent",
    outdentDefaults: {
        icon: "[SKIN]/RichTextEditor/outdent.png",
        click : "this.creator.outdentSelection()"
    },

    //> @attr richTextEditor.orderedListPrompt (String : "Convert to a numbered list" : IRW)
    // The prompt for the built-in +link{type:ControlName, orderedList} control.
    // @group i18nMessages
    // @visibility external
    //<
    orderedListPrompt: "Convert to a numbered list",
    orderedListDefaults: {
        icon: "[SKIN]/RichTextEditor/text_list_numbers.png",
        click : "this.creator.convertToOrderedList()"
    },

    //> @attr richTextEditor.unorderedListPrompt (String : "Convert to a bullet list" : IRW)
    // The prompt for the built-in +link{type:ControlName, unorderedList} control.
    // @group i18nMessages
    // @visibility external
    //<
    unorderedListPrompt: "Convert to a bullet list",
    unorderedListDefaults: {
        icon: "[SKIN]/RichTextEditor/text_list_bullets.png",
        click : "this.creator.convertToUnorderedList()"
    },

    //> @attr richTextEditor.listPropertiesPrompt (String : "Configure the list" : IRW)
    // The prompt for the built-in +link{type:ControlName, listProperties} control.
    // @group i18nMessages
    // @visibility external
    //<
    listPropertiesPrompt: "Configure the list",
    listPropertiesDefaults: {
        icon: "[SKIN]/RichTextEditor/text_list_edit.png",
        click : "this.creator.editListProperties()"
    },


    // For tabbing / focusing purposes, this editor should pass straight through to the
    // editArea
    canFocus: true,
    _useFocusProxy: false,
    _useNativeTabIndex: false,

    //> @attr RichTextEditor.moveFocusOnTab (boolean : true : IRW)
    // @include RichTextCanvas.moveFocusOnTab
    // @setter setMoveFocusOnTab()
    // @visibility external
    //<
    moveFocusOnTab: true

});


isc.RichTextEditor.addClassProperties({
    // Whenever the edit area changes we want to be notified too.
    // Avoid recreating the notification function (fired in the scope of the edit area)
    _canvasContentsChanged : function (oldValue, newValue) {
        this.creator._valueChanged(oldValue, newValue);
    }
});

//!>Deferred
isc.RichTextEditor.addProperties({


    dragStartDistance: 1,


    // On init, create our toolbar / RichTextCanvas contents area.
    initWidget : function () {
        this.Super("initWidget", arguments);
        this.createChildren();
    },

    //> @method richTextEditor.doWarn()
    // Display a warning if Rich Text Editing is not fully supported in this browser.
    // Default behavior logs a warning to the developer console - Override this if a user-visible
    // warning is required
    // @visibility external
    //<
    doWarn : function () {
        isc.logWarn("Warning: Not all Rich Text Editing features are supported in this browser.");
    },

    createChildren : function () {
        // call on a delay to avoid this warning dialog being trapped by the FE as a managed
        // component that gets destroyed when the example is unloaded.  Leads to a crash as we
        // try to reuse the destroyed object.
        if (!this.richEditorSupported()) this.delayCall("doWarn");

        if (!this.autoChildDefaults) this.autoChildDefaults = {};

        // Set up the default width / click handler for control buttons

        this.autoChildDefaults.width = this.controlButtonWidth;
        this.autoChildDefaults.click =
            function () {
                if (this.isControl && isc.isA.StatefulCanvas(this)) this.creator.fireAction(this.controlName)
            }
        if (this.toolbarHeight > 0) this._createToolArea();

        var props = isc.addProperties({ backgroundColor: this.editAreaBackgroundColor },
            this.editAreaProperties,
            {
                top: this.toolbarHeight, className: this.editAreaClassName,
                left: 0, width: "100%", height: "*",
                contents: this.value,
                moveFocusOnTab: this.moveFocusOnTab,
                // We pick up our tabIndex from the RichTextEditor directly when
                // the RTE is written out.

                tabIndex:-1,
                getTabIndex : function () {
                    var ti = (this.parentElement) ? this.parentElement.getTabIndex() : -1;
                    this.tabIndex = ti;
                    return ti;
                },


                _focusInNextTabElement : function (forward, mask) {
                    if (this.parentElement != null) {
                        return this.parentElement._focusInNextTabElement(forward,mask);
                    } else {
                        return this.Super("_focusInNextTabElement", arguments);
                    }
                },
                changed : isc.RichTextEditor._canvasContentsChanged,
                focusChanged : function (hasFocus) {
                    if (hasFocus) {
                        this._resetSelection();
                        this._focussing = false;
                    } else {
                        this._focussing = true;
                    }
                    if (this.parentElement != null) this.parentElement.editAreaFocusChanged();
                },

                getBrowserSpellCheck : function () {
                    return this.parentElement.getBrowserSpellCheck()
                }
            }
        );
        this.addAutoChild("editArea", props);
    },

    //> @method richTextEditor.editAreaFocusChanged()
    // Notification method fired when the edit area receives or loses keyboard focus
    //<
    // Used in richTextItem
    editAreaFocusChanged : function () {
    },

    //> @method richTextEditor.richEditorSupported()
    // Does this browser support the full RichTextEditor feature set.
    // Returns false for browsers in which some features are not natively supported
    // (Safari before version 3.1 and Opera before version 9.50).
    // @return (Boolean) false if this browser doesn't fully support RichTextEditing
    // @visibility external
    //<

    richEditorSupported : function () {
        return !((isc.Browser.isSafari && isc.Browser.minorVersion < 3.1) ||
                 (isc.Browser.isOpera && isc.Browser.minorVersion < 9.5));
    },

    // browserSpellCheck is a boolean property to enable / disable native browser checking of
    // spelling, where supported.
    // This currently only has an effect in Moz
    // By default return this.browserSpellCheck if specified. Overridden for RichTextItems.
    getBrowserSpellCheck : function () {
        return this.browserSpellCheck;
    },

    draw : function () {
        this.Super("draw", arguments);
        // if editArea is visible, resize it to fit the toolArea (which drives parent RTE width)

        if (this.editArea && this.toolArea) {
            this.editArea.setWidth(this.toolArea.getVisibleWidth());
        }
    },

    // Toolbar

    _$break: "break",
    _createToolArea : function () {
        var toolArea = this.addAutoChild("toolArea", {
            backgroundColor: this.toolbarBackgroundColor
        });
        if (toolArea == null) return; // showToolArea == false

        // Picks up HLayout constructor from this.toolbarConstructor
        var currentToolbar = this._createToolbar();

        // this.controlGroups is an array of groups to show.
        // each group is an array of controls to create (within the group).
        var controlGroups = this.controlGroups;

        for (var i = 0, r = 1, c = 0; i < controlGroups.length; ++i) {
            var controlGroup = controlGroups[i];
            if (controlGroup == this._$break) {
                currentToolbar = this._createToolbar();
                ++r;
                c = 0;
                continue;
            }

            if (!isc.isA.Canvas(controlGroup)) {
                var controlGroupName = controlGroup,
                    controlNames = this[controlGroupName];
                if (!controlNames) {
                    this.logWarn("Unable to find controlGroup '" + controlGroupName +
                                 "'. This group should be specified as an array of " +
                                 "control names, but is not present.");
                    continue;
                }

                // Add separators between the groups.
                if (c > 0) currentToolbar.addMember(this._createToolbarSeparator());

                for (var j = 0; j < controlNames.length; ++j) {
                    var control = controlNames[j];

                    if (!isc.isA.Canvas(control)) {
                        var controlName = control;
                        // use 'addAutoChild' to create the controls and add them to the toolbar as
                        // children.

                        this.addAutoChild(controlName,
                            // These properties used by the default click handler for controls
                            {
                                canFocus: true, isControl: true, controlName: controlName,
                                prompt: this[controlName + "Prompt"]
                            },
                            this.defaultControlConstructor,
                            currentToolbar
                        );
                    } else {
                        control.setCanFocus(true);
                        control.isControl = true;
                        currentToolbar.addMember(control);
                    }
                }
            } else {
                // Add separators between the groups.
                if (c > 0) currentToolbar.addMember(this._createToolbarSeparator());

                currentToolbar.addMember(controlGroup);
            }

            ++c;
        }

        // hide toolArea if there are no controls and showToolArea has not been set
        if (controlGroups.length == 0 && this.showToolArea != true) toolArea.hide();

        toolArea.setHeight(r * this.toolbarHeight);
        toolArea.setMembers(this.toolbars);
        return toolArea;
    },

    _createToolbar : function () {
        var toolbar = this.createAutoChild("toolbar", {
            height: this.toolbarHeight,
            backgroundColor: this.toolbarBackgroundColor
        });
        if (this.toolbars == null) {
            this.toolbars = [ toolbar ];
            // For backward compatibility, set `this.toolbar' to the first toolbar.
            this.toolbar = toolbar;
        } else this.toolbars.add(toolbar);
        return toolbar;
    },

    // Separator bar to write between control groups
    _createToolbarSeparator : function () {
        if (!this._separatorProps) {
            this._separatorProps = {
                autoDraw: false,
                width: 12,
                height: "100%",
                src: this.toolbarSeparatorSrc
            };
        }
        return isc.Img.create(this._separatorProps);
    },

    // For tabbing / focussing purposes, this editor should pass straight through to the
    // editArea
    setFocus : function (newFocus) {
        var editArea = this.editArea;
        if (!editArea) return;
        return editArea.setFocus(newFocus);
    },


    _setTabIndex : function (tabIndex, auto) {
        this.Super("_setTabIndex", arguments);
        if (this.editArea) this.editArea._setTabIndex(this.getTabIndex(), auto);
    },

    //> @method richTextEditor.setMoveFocusOnTab()
    // Setter for +link{moveFocusOnTab}.
    // @param moveFocusOnTab (boolean) new value for moveFocusOnTab
    // @visibility external
    //<
    setMoveFocusOnTab : function (moveFocusOnTab) {
        this.moveFocusOnTab = moveFocusOnTab;
        if (this.editArea) this.editArea.moveFocusOnTab = moveFocusOnTab;
    },

    // For the font / font-size selector, we want to show a "choose font" type prompt rather
    // than an empty selector.

    _makeFontMap : function(prompt, options) {
        // Add the empty 'select a font size' message and return
        var map = { _prompt: prompt };

        return isc.addProperties(map, options);
    },

    _makeFontNamesMap : function () {
        return this._makeFontMap(this.fontSelectorPrompt, this.fontNames);
    },
    _makeFontSizesMap : function () {
        return this._makeFontMap(this.fontSizeSelectorPrompt, this.fontSizes);
    },

    // Special constructor functions for font / font-size selector controls

    fontSelector_autoMaker : function (properties) {

        isc.addProperties(properties,
            {
                numCols: 1, cellPadding: 1,
                items: [
                    // Disable tabbing into the select items

                    isc.addProperties({
                        type: "select", name: "fontname", showTitle: false, tabIndex: -1,

                        pickListProperties: {
                            cellHeight: 16,
                            // Override 'getCellValue' to preview the font.
                            getCellValue : function (record, recordNum, fieldNum) {
                                var val = this.Super("getCellValue", arguments),
                                    fontName = record ? record.fontname : null;
                                if (fontName && fontName != "_prompt") {
                                    val = "<SPAN style='font-family:" + fontName + ";'>" + val + "</SPAN>";
                                }
                                return val;
                            }
                        },

                        defaultValue: "_prompt",
                        valueMap: this._makeFontNamesMap(),

                        pickValue : function(value) {
                            this.Super("pickValue", arguments);
                            if (value != "_prompt") {
                                this.form.creator.fireAction('setSelectionFont', value);
                            }
                        }
                    }, this.fontSelectorItemProperties)
                ]
            }
        );

        return this.createAutoChild("fontSelector", properties);
    },


    fontSizeSelector_autoMaker : function (properties) {
        isc.addProperties(properties,
            {
                numCols: 1, cellPadding: 1,
                items:[
                    isc.addProperties({
                        type: "select", name: "fontsize", showTitle: false, tabIndex: -1,
                        defaultValue: "_prompt",
                        valueMap: this._makeFontSizesMap(),
                        // See comments in fontSizeSelector_autoMaker for why we override
                        // pickValue rather than implementing a change handler.
                        pickValue : function(value) {
                            this.Super("pickValue", arguments);
                            if (value != "_prompt") {
                                this.form.creator.fireAction('setSelectionFontSize', value);
                            }
                        }
                    }, this.fontSizeSelectorItemProperties)
                ]
            }
        );

        return this.createAutoChild("fontSizeSelector", properties);
    },


    // convenience method to call a method on the richTextCanvas used as the editArea.  Does
    // nothing if there is no method named "action"
    fireAction : function (action, param) {
        var editArea = this.editArea;
        if (!editArea || !action || !editArea[action] || !isc.isA.Function(editArea[action]))
            return;

        this.editArea[action](param);
    },

    // Special handlers for picking colors:
    // Use a colorChooser widget (for both setting text and background colors)
    // ColorChooser has been superseded by ColorPicker
    chooseColor : function (selectingTextColor) {
        this.colorChooser = isc.ColorPicker.getSharedColorPicker({
            creator: this,
            ID: this.getID() + "_colorChooser",
            // Avoid showing the auto / transparent button for picking a null color

            showNullValue: false,
            colorSelected : function (color) {
                this.creator._colorSelected(color);
            },

            // Override cancel to put focus back into the edit area
            cancel : function () {
                this.Super("cancel", arguments);
                this.creator.editArea.focus();
            }
        })

        this._selectingTextColor = selectingTextColor;
        this.colorChooser.show();
    },

    _colorSelected : function (color) {
        var action = this._selectingTextColor ? "setSelectionColor"
                                              : "setSelectionBackgroundColor";
        delete this._selectingTextColor;
        this.fireAction(action, color);
    },

    chooseTextColor : function () {
        this.chooseColor(true);
    },

    chooseBackgroundColor : function () {
        this.chooseColor(false);
    },

    // Creating links
    createLink : function () {
        var editor = this;
        isc.askForValue(this.linkUrlTitle, function (value) {
            if (value == null) return;
            editor.fireAction("createLink", value);
        }, { defaultValue: "http://", width: 320 });
    },

    // Lists

    indentSelection : function () {
        this.fireAction("indentSelection");
    },

    outdentSelection : function () {
        this.fireAction("outdentSelection");
    },

    convertToOrderedList : function () {
        this.fireAction("convertSelectionToOrderedList");
    },

    convertToUnorderedList : function () {
        this.fireAction("convertSelectionToUnorderedList");
    },

    //> @attr richTextEditor.listPropertiesDialog (AutoChild ListPropertiesDialog : null : R)
    // Dialog shown when the +link{type:ControlName,"listProperties" control} is pressed.
    // Provides options for the user to control formatting of lists.
    // @visibility external
    //<
    listPropertiesDialogDefaults: {
        _constructor: "ListPropertiesDialog",
        autoParent: "none",
        autoCenter: true,
        isModal: true,
        showModalMask: true,

        applyClick : function (listProperties) {
            this.creator.applyListProperties(listProperties);
            this.hide();
        },

        cancelClick : function () {
            this.hide();
        }
    },

    applyListProperties : function (listProperties) {
        this.fireAction("applyListProperties", listProperties);
    },

    //> @attr richTextEditor.listPropertiesWarningText (String : "Place the cursor within a list to configure it" : IRW)
    // The warning message displayed in a dialog when a user tries to configure a list without
    // first putting the cursor in an appropriate place.
    // @group i18nMessages
    // @visibility external
    //<
    listPropertiesWarningText: "Place the cursor within a list to configure it",
    editListProperties : function () {
        if (!this.editArea) return;

        var listProperties = this.editArea.getListProperties();
        if (listProperties == null) {
            isc.warn(this.listPropertiesWarningText);
            return;
        }

        var listPropertiesDialog = this.addAutoChild("listPropertiesDialog");
        // Update the dialog's listPropertiesPane with the currently selected list's configuration.
        listPropertiesDialog.listPropertiesPane.setListProperties(listProperties);
        listPropertiesDialog.show();
    },

    // Retrieving / updating content:

    _valueChanged : function (oldValue, newValue) {
        if (this.valueChanged) this.valueChanged(oldValue, newValue);
    },

    //> @method richTextEditor.valueChanged()
    // StringMethod fired when the user edits the editor's value. Will not be fired in
    // response to an explicit <code>setValue</code> call.
    // @param oldValue Value before the edit
    // @param newValue Value now
    //<

    //> @method richTextEditor.getValue()
    // Retrieves the current value of the edit area.
    // @visibility external
    //<
    getValue : function () {
        if (this.editArea) this.value = this.editArea.getContents();
        return this.value;
    },

    //> @method richTextEditor.setValue()
    // Updates the current value of the edit area.
    // @visibility external
    //<
    setValue : function (newValue) {
        this.value = newValue;
        if (this.editArea) this.editArea.setContents(this.value);
    }

});
//!<Deferred


isc.RichTextEditor.registerStringMethods({
    valueChanged : "oldValue,newValue"
});





//>    @class    RichTextItem
// FormItem for rich text (HTML) editing. Makes use of a +link{RichTextEditor} as the
// editing interface.
// @visibility external
//<

isc.ClassFactory.defineClass("RichTextItem", isc.CanvasItem);

isc.RichTextItem.addProperties({

    // Override canFocus to allow focus to go to the RichTextEditor
    canFocus:true,

    //> @attr RichTextItem.moveFocusOnTab (boolean : true : IRW)
    // @include RichTextCanvas.moveFocusOnTab
    // @setter setMoveFocusOnTab()
    // @visibility external
    //<
    moveFocusOnTab: true,

    //> @attr richTextItem.shouldSaveValue (Boolean : true : IR)
    // @include FormItem.shouldSaveValue
    //<
    shouldSaveValue:true,

    //> @attr RichTextItem.showTitle (Boolean : false : IR)
    // Don't show the title for rich text items by default
    // @visibility external
    //<
    showTitle:false,

    //>@attr RichTextItem.startRow   (Boolean : true : IRW)
    // By default RichTextItems take up an entire row
    // @visibility external
    //<
    startRow:true,

    //>@attr RichTextItem.endRow (Boolean : true : IRW)
    // By default RichTextItems take up an entire row
    // @visibility external
    //<
    endRow:true,

    //>@attr RichTextItem.colSpan (number | string : "*": IRW)
    // By default RichTextItems take up an entire row
    // @visibility external
    //<
    colSpan:"*",

    // Realistically rich text editors take up a lot of space because of their toolbars.
    width:550,


    //> @attr RichTextItem.controlGroups (Array of String : null : IA)
    // +link{RichTextEditor.controlGroups} to display for this editor.
    // Each controlGroup should be a property set either on this item or on the RichTextEditor
    // prototype and should be set to an array of +link{type:ControlName}s.
    // @visibility external
    //<
    // For each named control specified, you can override [controlName]Properties to apply
    // specific properties, [controlName]Constructor to supply a class for the control, and
    // [controlName]_autoMaker to supply a function taht actually creates (and returns) the
    // control.
    //controlGroups : null

    //>@attr RichTextItem.defaultControlConstructor (Array : null : IA)
    // If set, this property will override +link{RichTextEditor.defaultControlConstructor} for
    // this item's RichTextEditor
    // @visibility internal
    //<
    //defaultControlConstructor : null

    canvasConstructor: "RichTextEditor",
    canvasDefaults: {
        getBrowserSpellCheck : function() {
            return this.canvasItem.getBrowserSpellCheck();
        },
        valueChanged : function (oldValue, newValue) {
            this.canvasItem.storeValue(newValue);
        },
        editAreaFocusChanged : function () {
            this.canvasItem.editAreaFocusChanged();
        }
    }

    //>@attr RichTextItem.browserSpellCheck (boolean : null : IRWA)
    // @include FormItem.browserSpellCheck
    // @visibility internal
    //<

});

isc.RichTextItem.addMethods({
    init : function () {
        if (this.value && isc.isA.String(this.value)) {
            this.value = this.value.replaceAll("<BR>", "<br>").replaceAll("<P>", "<p>");
        }

        this.Super("init", arguments);
    },

    // Override _createCanvas to set up a RichTextEditor as this item's canvas
    _createCanvas : function () {
        this._creatingCanvas = true;

        var value = this.getValue();
        // Map "undefined" (etc.) to an empty string
        value = this.mapValueToDisplay(value);

        var properties = {
            ID: this.getID() + "_editor",
            value: value,
            moveFocusOnTab: this.moveFocusOnTab
        };

        var cgs = this.controlGroups;
        if (cgs != null) {
            var propsSuffix = "Properties",
                makerSuffix = "_autoMaker",
                constructorSuffix = "Constructor";

            properties.controlGroups = cgs;
            for (var i = 0; i < cgs.length; i++) {
                if (this[cgs[i]]) {
                    var groupName = cgs[i],
                        group = this[groupName];

                    properties[groupName] = group;

                    // To allow full customization we need to be able to apply properties /
                    // custom maker functions to each control.
                    for (var ii = 0; ii < group.length; ii++) {
                        var propName = group[ii] + propsSuffix,
                            makerName = group[ii] + makerSuffix,
                            constructorName = group[ii] + constructorSuffix;

                        if (this[propName]) properties[propName] = this[propName];
                        if (this[makerName]) properties[makerName] = this[makerName];
                        if (this[constructorName])
                            properties[constructorName] = this[constructorName];
                    }
                }
            }
        }

        if (this.defaultControlConstructor != null) {
            properties.defaultControlConstructor = this.defaultControlConstructor;
        }

        this.canvas = properties;
        this.Super("_createCanvas", arguments);
        delete this._creatingCanvas;
    },

    //> @method RichTextItem.setMoveFocusOnTab()
    // Setter for +link{moveFocusOnTab}.
    // @param moveFocusOnTab (boolean) new value for moveFocusOnTab
    // @visibility external
    //<
    setMoveFocusOnTab : function (moveFocusOnTab) {
        this.moveFocusOnTab = moveFocusOnTab;
        if (this.canvas) this.canvas.setMoveFocusOnTab(moveFocusOnTab);
    },

    // Fire focus/blur on focusChanged on our edit area rather than on the RTE as a whole.
    editAreaFocusChanged : function () {
        this.hasFocus = this.canvas.editArea.hasFocus;
        if (this.hasFocus) {
            this.elementFocus();
            this.form.setFocusItem(this);
        } else {
            this.elementBlur();
        }
    },

    // Override mapValueToDisplay to show null/undefined as ""

    mapValueToDisplay : function (internalValue) {
        var value = isc.FormItem._instancePrototype.mapValueToDisplay.call(this, internalValue);
        // always display the empty string for null values, rather than "null" or "undefined"
        if (value == null) return isc.emptyString;
        return value;
    },

    showValue : function (displayValue, dataValue, form, item) {
        if (!this.canvas) return;
        this.canvas.setValue(displayValue);
    }
});
isc._debugModules = (isc._debugModules != null ? isc._debugModules : []);isc._debugModules.push('RichTextEditor');isc.checkForDebugAndNonDebugModules();isc._moduleEnd=isc._RichTextEditor_end=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc.Log&&isc.Log.logIsInfoEnabled('loadTime'))isc.Log.logInfo('RichTextEditor module init time: ' + (isc._moduleEnd-isc._moduleStart) + 'ms','loadTime');delete isc.definingFramework;if (isc.Page) isc.Page.handleEvent(null, "moduleLoaded", { moduleName: 'RichTextEditor', loadTime: (isc._moduleEnd-isc._moduleStart)});}else{if(window.isc && isc.Log && isc.Log.logWarn)isc.Log.logWarn("Duplicate load of module 'RichTextEditor'.");}

/*

  SmartClient Ajax RIA system
  Version v11.0p_2016-03-31/LGPL Deployment (2016-03-31)

  Copyright 2000 and beyond Isomorphic Software, Inc. All rights reserved.
  "SmartClient" is a trademark of Isomorphic Software, Inc.

  LICENSE NOTICE
     INSTALLATION OR USE OF THIS SOFTWARE INDICATES YOUR ACCEPTANCE OF
     ISOMORPHIC SOFTWARE LICENSE TERMS. If you have received this file
     without an accompanying Isomorphic Software license file, please
     contact licensing@isomorphic.com for details. Unauthorized copying and
     use of this software is a violation of international copyright law.

  DEVELOPMENT ONLY - DO NOT DEPLOY
     This software is provided for evaluation, training, and development
     purposes only. It may include supplementary components that are not
     licensed for deployment. The separate DEPLOY package for this release
     contains SmartClient components that are licensed for deployment.

  PROPRIETARY & PROTECTED MATERIAL
     This software contains proprietary materials that are protected by
     contract and intellectual property law. You are expressly prohibited
     from attempting to reverse engineer this software or modify this
     software for human readability.

  CONTACT ISOMORPHIC
     For more information regarding license rights and restrictions, or to
     report possible license violations, please contact Isomorphic Software
     by email (licensing@isomorphic.com) or web (www.isomorphic.com).

*/

