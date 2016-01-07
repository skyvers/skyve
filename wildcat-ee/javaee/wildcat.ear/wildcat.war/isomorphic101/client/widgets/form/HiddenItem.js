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
    shouldPrint:false,
    
    //> @attr hiddenItem.fetchMissingValues   (Boolean : true : IRWA)
    // If this form item has a specified +link{FormItem.optionDataSource}, should the
    // item ever perform a fetch against this dataSource to retrieve the related record.
    // <P>
    // This is disabled by default for hiddenItems as there is typically no need to
    // perform a fetch and retrieve a display-field value to show the user for a 
    // hidden item. This does mean that if a developer needs access to the related record
    // for a hidden-item's value, they will need to enable both this setting and
    // +link{formItem.alwaysFetchMissingValues}.
    //
    // @group display_values
    // @see formItem.optionDataSource
    // @see formItem.getSelectedRecord()
    // @see formItem.filterLocally
    // @visibility external
    //<
    fetchMissingValues:false
    
    //> @attr hiddenItem.alwaysFetchMissingValues (Boolean : false : IRWA)
    //
    // If this form item has a specified +link{FormItem.optionDataSource} and 
    // +link{formItem.fetchMissingValues} is true, when the item value changes, a fetch will be
    // performed against the optionDataSource to retrieve the related record 
    // if +link{formItem.displayField} is specified and the new item value is not present in any
    // valueMap explicitly specified on the item.
    // <P>
    // Setting this property to true means that a fetch will occur against the optionDataSource 
    // to retrieve the related record even if +link{formItem.displayField} is unset, or the
    // item has a valueMap which explicitly contains this field's value.
    // <P>
    // An example of a use case where this might be set would be if +link{formItem.formatValue}
    // or +link{formItem.formatEditorValue} were written to display properties from the
    // +link{formItem.getSelectedRecord(),selected record}.
    // <P>
    // Note - for efficiency we cache the associated record once a fetch has been performed, meaning
    // if the value changes, then reverts to a previously seen value, we do not kick
    // off an additional fetch even if this property is true. If necessary this cache may be
    // explicitly invalidated via a call to +link{formItem.invalidateDisplayValueCache()}
    // <P>
    // Note: For hiddenItem +link{hiddenItem.fetchMissingValues,fetchMissingValues} is defaulted
    // to <code>false</code> so developers wishing to get access to the record related to
    // the current hiddenItem value would need to explicitly set both that property, and this
    // one to true.
    // 
    // @group display_values
    // @visibility external
    //<
    
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


