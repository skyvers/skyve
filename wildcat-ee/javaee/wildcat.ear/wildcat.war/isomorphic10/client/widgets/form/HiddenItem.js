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

 






//>	@class	HiddenItem
// HiddenItems track a value but have no visible appearance and do not take up space in the form
// layout.
// <P>
// When using SmartClient databinding it is usually not necessary to use a HiddenItem, since
// the DynamicForm will track values for which no actual form control exists, and will submit
// these 'extra' values when +link{dynamicForm.saveData()} is called.  HiddenItems only apply
// to forms that are submitted like ordinary HTML forms, via the
// +link{dynamicForm.submitForm()} method.
// 
// @visibility external
//<
isc.ClassFactory.defineClass("HiddenItem", "FormItem");
isc.HiddenItem.addProperties({
    //>	@attr	hiddenItem.showTitle		(Boolean : false : IRW)
	//			we never show a separate title cell for hidden fields
	//		@group	appearance
    // @visibility external
	//<	
	showTitle:false,					
    
    // Don't specify a cell style - we don't want to take up any space
    cellStyle:null,

    //>	@attr	hiddenItem.width		(number : 0 : IRW)
	//			default width of this item
	//		@group	appearance
	//<
	width:0,

    //>	@attr	hiddenItem.height		(number : 0 : IRW)
	//			default height of this item
	//		@group	appearance
	//<
	height:0,

    //>	@attr	hiddenItem.colSpan				(number : 0 : IRW)
	//			hidden fields don't take up any columns
	//		@group	appearance
    // @visibility external
	//<	
	colSpan:0,

    //>	@attr	hiddenItem.rowSpan				(number : 0 : IRW)
	//			hidden fields don't take up any rows
	//		@group	appearance
    // @visibility external
	//<	
	rowSpan:0,

    

    //> @attr   hiddenItem._hasDataElement    (boolean : true : IRW)
    //      Hidden items have a data element.
    // @group formValues
    // @visibility   internal
    // @see     method:FormItem.hasDataElement
    // @see     method:FormItem.getDataElement
    //<
    _hasDataElement:true,
    
    
    //>@attr hiddenItem.canFocus  (boolean : false : IRA)
    //  Override canFocus - can never focus in a hidden item.
    //<
    canFocus:false,

    //> @attr hiddenItem.shouldPrint (boolean : false : IRWA)
    // Hidden items will not print
    //<
    shouldPrint:false
    
});
isc.HiddenItem.addMethods({

    // The user can't interact directly with a hiddenItem.
    isEditable : function () {
        return false
    },
    

	//>	@method	hiddenItem.getInnerHTML()	(A)
	//  Hidden items render out native hidden HTML input elements. Icons will not be displayed
	//		@group	drawing
	//		@param	value	(string)	Value of the element [Unused because it is more reliably set by setValue].
	//		@return	(HTML)	HTML output for this element
	//<
	getInnerHTML : function (value) {
		
		var output = isc.StringBuffer.concat(
                        "<INPUT ID='", this.getDataElementId(), 
                        "' TYPE=HIDDEN NAME=" , this.getElementName() , ">"
                     );	
		return output.toString();
	},
    
    //>	@method	hiddenItem.getRowSpan()	(A)
	//		@group	drawing
	//			Override formItem.getRowSpan() to return zero
    //          Ensures we don't draw a cell for the item
	//
	//<
	getRowSpan : function () {
		return 0;
	},
    
    //>	@method	hiddenItem.getColSpan()	(A)
	//		@group	drawing
	//			Override formItem.getColSpan() to return zero
    //          Ensures we don't draw a cell for the item
	//
	//<
	getColSpan : function () {
		return 0;
	},    
    
    //>	@method	hiddenItem.shouldShowTitle()	(A)
	//		@group	drawing
    //      Override formItem.shouldShowTitle to return false - we don't want
    //      to draw a cell for the item title
	//
	//		@return	(HTML)	title for the formItem
	//<
	shouldShowTitle : function () {
		return false;
	},

	//>	@method	hiddenItem.getErrorHTML()	(A)
	//		@group	error handling
	//			output the HTML for an error message in a hidden field
	//			overridden to show an alert when debugging
	//
	//		@param	error		(string)	error message
	//
	//		@return	(HTML)		(nothing)
	//<
	getErrorHTML : function (error) {
		//>DEBUG
		this.logError("Error in hidden field '" + this.getFieldName() + "':\r  " + error);
		//<DEBUG
		return null;
	},

    //> @method hiddenItem.isStartRow()   (A)
    //      @group drawing
    //          override formItem.isStartRow() to return false
    //<
    isStartRow : function () {
        return false;
    },
    
    //> @method hiddenItem.isEndRow()   (A)
    //      @group drawing
    //          override formItem.isEndRow() to return false
    //<
    isEndRow : function () {
        return false;
    }
    

});


