/*
 * Isomorphic SmartClient
 * Version v9.1p_2014-03-26 (2014-03-26)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */

 






//>	@class	StaticTextItem
//	A FormItem that displays an uneditable value.
// @visibility external
//<
isc.ClassFactory.defineClass("StaticTextItem", "FormItem");
isc.StaticTextItem.addProperties({
    //>	@attr	staticTextItem.height		(number : null : IRW)
	//			don't specify a height so the table cell will expand
	//			to show the entire contents.  Note that this can 
	//			mess up dynamic height calculations in forms.
	//		@group	appearance
	//<
	height:null,

	//> @attr staticTextItem.applyHeightToTextBox (Boolean : false : IRA)
    // If +link{formItem.height} is specified, should it be applied to the
    // item's text box element?
    // <P>
    // Overridden to be <code>false</code> for StaticTextItems by default.
    // <P>
    // See +link{FormItem.shouldApplyHeightToTextBox,shouldApplyHeightToTextBox()} for more information.
    // @visibility external
    //<
	applyHeightToTextBox:false,

    //>	@attr	staticTextItem.width		(number : null : IRW)
	//			If a width is specified, we write out a table to make width consistent,
	//			if <code>null</code> is used, we write out a SPAN which is cheaper.
	//		@group	appearance
	//<
	width:null,

    //>	@attr	staticTextItem.wrap		(Boolean : true : IRW)
	// @include FormItem.wrap
	//		@group	appearance
    // @visibility external
	//<
	wrap:true,

    //>@attr    staticTextItem.clipValue (Boolean : false : IRW)
    // @include FormItem.clipValue
    // @group appearance
    // @visibility external
    //<
    clipValue:false,

    //>	@attr	staticTextItem.textBoxStyle    (FormItemBaseStyle : "staticTextItem" : IRW)
	//  Base CSS class for this item
	// @group   appearance
    // @visibility external
	//<
	textBoxStyle:"staticTextItem",
	
	//> @attr staticTextItem.canSelectText (boolean : true : IRW)
	// Should the user be able to select the text in this item?
	// @visibility external
	//<
	canSelectText:true,
	
	// when dynamically showing/hiding icons we should be able to resize our textBox without
	// redraw.
	redrawOnShowIcon:false,

    //>	@attr	staticTextItem.outputAsHTML (boolean : null : IRW)
	// By default HTML values in a staticTextItem will be interpreted by the browser.
    // Setting this flag to true will causes HTML characters to be escaped, meaning the
    // raw value of the field (for example <code>"&lt;b&gt;AAA&lt;/b&gt;"</code>) is displayed
    // to the user rather than the interpreted HTML (for example <code>"<b>AAA</b>"</code>)
    // @group appearance
    // @visibility external
    // @deprecated in favor of +link{staticTextItem.escapeHTML}
	//<
//	outputAsHTML:false,

    // set useShortDateFormat to false.
    // This will use "toNormalDate()" rather than toShortDate for date values 
    // Other than those in logical "date" type fields (where we don't want to show time).
    // Document this behaviour by explicitly calling it out in the dateFormatter docs for
    // StaticTextItems.
    //>	@attr staticTextItem.dateFormatter (DateDisplayFormat : null : [IRWA])
    // Display format to use for date type values within this formItem.
    // <P>
    // Note that Fields of type <code>"date"</code>, <code>"datetime"</code> or <code>"time"</code> will
    // be edited using a +link{DateItem} or +link{TimeItem} by default, but 
    // this can be overridden - for <code>canEdit:false</code> fields, a
    // +link{StaticTextItem} is used by default, and the developer can always specify 
    // a custom +link{formItem.editorType} as well as +link{formItem.type,data type}.
    // <P>
    // The +link{formItem.timeFormatter} may also be used to format underlying Date values as
    // times (ommitting the date part entirely). If both <code>dateFormatter</code> and
    // <code>timeFormatter</code> are specified on an item, for
    // fields specified as +link{formItem.type,type "time"} the
    // <code>timeFormatter</code> will be used, otherwise the <code>dateFormatter</code>
    // <P>
    // If <code>item.dateFormatter</code> and <code>item.timeFormatter</code> is unspecified,
    // date display format may be defined at the component level via
    // +link{DynamicForm.dateFormatter}, or for fields of type <code>"datetime"</code>
    // +link{DynamicForm.datetimeFormatter}. Otherwise for fields of type "date",
    // default is to use the system-wide default short date format, configured via
    // +link{Date.setShortDisplayFormat()}. For fields of type "datetime" or for Date values
    // in fields whose type does not inherit from the logical "date" type, default is to use
    // the system-wide normal date format configured via +link{Date.setNormalDisplayFormat()} 
    // (using "toNormalDate()" on logical <code>"date"</code> type fields is not desirable as this
    // would display the time component of the date object to the user).<br>
    // Specify any valid +link{type:DateDisplayFormat} to 
    // change the format used by this item.
    // 
    // @see formItem.timeFormatter
    //
	// @group appearance
    // @visibility external
	//<
	//dateFormatter:null,
    useShortDateFormat:false,

    //> @attr staticTextItem.escapeHTML (Boolean : false : IRW)
	// By default HTML values in a staticTextItem will be interpreted by the browser.
    // Setting this flag to true causes HTML characters to be escaped, meaning the
    // raw value of the field (for example <code>"&lt;b&gt;AAA&lt;/b&gt;"</code>) is displayed
    // to the user rather than the interpreted HTML (for example <code>"<b>AAA</b>"</code>)
    // @group appearance
    // @visibility external
	//<
	// implemented at the formItem level - enable via the canEscapeHTML flag
	canEscapeHTML:true,
	escapeHTML:null,
    
     // override 'emptyDisplayValue' to write out "&nbsp;" instead of "" for styling
    emptyDisplayValue:"&nbsp;"
                                       
});
isc.StaticTextItem.addMethods({
	
    // Static text items are used for display only - non editable
    isEditable : function () {
        return false;
    },

    _canFocus : function () {
        if (this.canFocus != null) return this.canFocus;
        // needs to be focusable in screen reader mode because the value will only be read if the item
        // can be tabbed to
        return isc.screenReader;
    },

    // in canEdit: false mode, fields of type="float" become StaticTextItems, but we still want
    // to apply the decimalPad/precision flags on these displayed values.  Note that this code
    // also appears in FloatItem
    mapValueToDisplay : function (value) {
        if (isc.SimpleType.inheritsFrom(this.type, "float")) {
            var floatValue = null;
            if (isc.isA.String(value)) {
                var parsedValue = window.parseFloat(value);
                if (!window.isNaN(parsedValue) && parsedValue == value) {
                    floatValue = parsedValue;
                }
            } else if (isc.isA.Number(value)) {
                floatValue = value;
            }
            if (floatValue != null) {
                if (this.decimalPrecision != null || this.decimalPad != null) {
                    return isc.Canvas.getFloatValueAsString(floatValue,
                        this.decimalPrecision, this.decimalPad);
                } else if (this.precision != null) {
                    return isc.Canvas.getNumberValueAsString(floatValue, 
                        this.precision, "float");
                }
            }
        }
        return this.Super("mapValueToDisplay", arguments);
    }    
});

