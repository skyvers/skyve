/*
 * Isomorphic SmartClient
 * Version v10.0p_2014-09-10 (2014-09-10)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */

 







//>	@class	HeaderItem
// FormItem for showing a header within a DynamicForm.
// <p>
// Set the <code>defaultValue</code> of this item to the HTML you want to embed in the form.
// @visibility external
//<
isc.ClassFactory.defineClass("HeaderItem", "FormItem");
isc.HeaderItem.addProperties({
    // avoid attempting to save this item's value in the form's values array
    shouldSaveValue:false,

    //>	@attr	headerItem.defaultValue (String : "Header" : IRW)
	// Header text
	//		@group	appearance
    // @visibility external
	//<
    defaultValue:"Header",

    //>	@attr	headerItem.height		(number : 20 : IRW)
	// Default height of this item
	//		@group	appearance
	//<
	height:20,							

    //>	@attr	headerItem.showTitle		(Boolean : false : IRW)
	// Don't show a separate title cell for headers
	//		@group	appearance
    // @visibility external
	//<	
	showTitle:false,

    //>	@attr	headerItem.textBoxStyle (FormItemBaseStyle : "headerItem" : IRW)
	//			Base CSS class for this item
	// @group   appearance
    // @visibility external
	//<
	textBoxStyle:"headerItem",		
	
	//> @attr headerItem.canSelectText (boolean : true : IRW)
	// Should the user be able to select the text in this item?
	// @visibility external
	//<
    canSelectText:true,	

    //>	@attr	headerItem.colSpan		(measure : "*" : IRW)
	//			by default, headers span all remaining columns
	//		@group	appearance
    // @visibility external
	//<	
	colSpan:"*",						

    //>	@attr	headerItem.startRow		(Boolean : true : IRW)
	//			these items are in a row by themselves by default
	//		@group	appearance
    // @visibility external
	//<
	startRow:true,
	
    //>	@attr	headerItem.endRow			(Boolean : true : IRW)
	//			these items are in a row by themselves by default
	//		@group	appearance
    // @visibility external
	//<
	endRow:true,
    
    // Override emptyDisplayValue to write out "&nbsp;" so styling will work properly
    emptyDisplayValue:"&nbsp;",
    
    // Indicate that the developer can't interact directly with the header item content
    isEditable : function () {
        return false;
    }				

});

