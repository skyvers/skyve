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

 





//>	@class	BlurbItem
// FormItem intended for inserting blurbs of instructional HTML into DynamicForms.
// <p>
// Set the <code>defaultValue</code> of this item to the HTML you want to embed in the form.
// @visibility external
//<
isc.ClassFactory.defineClass("BlurbItem", "FormItem");
isc.BlurbItem.addProperties({
    // avoid attempting to save this item in the form's values array
    shouldSaveValue:false,

    //>	@attr	blurbItem.height		(boolean : false : IRW)
	//			don't specify a height so the table cell will expand
	//			to show the entire contents.  Note that this can 
	//			mess up dynamic height calculations in forms.
	//		@group	appearance
	//<
	height:null,

    //>	@attr	blurbItem.showTitle		(Boolean : false : IRW)
	// Blurb items show no title by default.
	//		@group	appearance
    // @visibility external
	//<	
	showTitle:false,

    //>	@attr	blurbItem.colSpan		(number : "*" : IRW)
	// By default, texts span all remaining columns
	//		@group	appearance
    // @visibility external
	//<	
	colSpan:"*",						

    //>	@attr	blurbItem.startRow		(boolean : true : IRW)
	// These items are in a row by themselves by default
	//		@group	appearance
	//<
	startRow:true,						
	
    //>	@attr	blurbItem.endRow			(boolean : true : IRW)
	// These items are in a row by themselves by default
	//		@group	appearance
	//<
	endRow:true,

    //>	@attr	blurbItem.textBoxStyle     (CSSStyleName : "staticTextItem" : IRW)
	//  Base css style for this item.
	//  @group	appearance
    //  @visibility external
	//<
	textBoxStyle:"staticTextItem",
	
	//> @attr blurbItem.canSelectText (boolean : true : IRW)
	// Should the user be able to select the text in this item?
	// @visibility external
	//<
	canSelectText:true,

    
    // override emptyDisplayValue to show &nbsp; so styling will work properly
    emptyDisplayValue:"&nbsp;",

    //>	@attr	blurbItem.wrap		(boolean : null : IRW)
	// @include FormItem.wrap
	//		@group	appearance
    // @visibility external
	//<

    //>@attr    blurbItem.clipValue (Boolean : false : IRW)
    // @include FormItem.clipValue
    // @group appearance
    // @visibility external
    //<
    clipValue:false

});
