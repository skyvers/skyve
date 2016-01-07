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
//>	@class	SpacerItem
// A SpacerItem takes up a single cell in the FormLayout, of arbitrary size.
// @visibility external
//<
isc.ClassFactory.defineClass("SpacerItem", "FormItem");
isc.SpacerItem.addProperties({
    // avoid attempting to save this item in the form's values array
    shouldSaveValue:false,

    //>	@attr	spacerItem.showTitle		(Boolean : false : IRW)
	//			we never show a separate title cell for spacers
	//		@group	appearance
    // @visibility external
	//<	
	showTitle:false,

    //>	@attr	spacerItem.width				(number : 20 : IRW)
	//			default width for the spacer
	//		@group	appearance
    // @visibility external
	//<
	width:20,

	//>	@attr	spacerItem.height			(number : 20 : IRW)
	//			default height for the spacer
	//		@group	appearance
    // @visibility external
	//<
	height:20,

    showHint:false,     // Don't show a hint for this item
    showIcons:false     // even if a user has defined icons for this item, suppress them
});
isc.SpacerItem.addMethods({

    // Override isEditable as this is non editable
    isEditable : function () {
        return false;
    },
    
	//>	@method	spacerItem.getElementHTML()	(A)
	//			output the HTML for this element
	//		@group	drawing
	//
	//		@param	value	(string)	Value of the element [Unused because it is more reliably set by setValue].
	//		@return	(HTML)	HTML output for this element
	//<
	getElementHTML : function (value) {
		return isc.Canvas.spacerHTML(this.width, this.height);
	},
    //>	@method	spacerItem.shouldShowTitle()	(A)
    //      Override formItem.shouldShowTitle to return false - we don't want
    //      to draw a cell for the item title
	//		@group	drawing
	//
	//		@return	(HTML)	title for the formItem
	//<
	shouldShowTitle : function () {
		return false;
	}
});

