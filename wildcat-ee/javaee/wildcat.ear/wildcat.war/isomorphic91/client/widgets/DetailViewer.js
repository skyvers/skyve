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







//>	@class	DetailViewer
//
//  Displays one or more records "horizontally" with one property per line.
//
//  @implements DataBoundComponent
//  @treeLocation Client Reference/Grids
//  @visibility external
//<

isc.ClassFactory.defineClass("DetailViewer", "Canvas", "DataBoundComponent");

// add default properties
isc.DetailViewer.addProperties({

    // Data
	// --------------------------------------------------------------------------------------------

    //>	@attr	detailViewer.data		(Array[] of DetailViewerRecord | Array[] of Record | RecordList : null : IRW)
    // A single record object or an array of them, specifying data. Note that DetailViewers do
    // not observe changes to the data array (in other words they will not automatically
    // re-draw when the data provided via this property is altered).
    //
    // @group basics
    // @visibility external
    //<

    //> @attr detailViewer.dataFetchMode (FetchMode : "basic" : IRW)
    // DetailViewers do not yet support paging, and will fetch and render all available
    // records.
    //
    // @group databinding
    // @visibility external
    //<
    dataFetchMode:"basic",
    
    // dataArity:"either" - DetailViewers support viewing single, or multiple records
    dataArity:"either",

    //> @object DetailViewerRecord
    //
    // A DetailViewerRecord is an object literal with properties that define the values for the
    // various fields of a +link{DetailViewer}.
    // <p>
    // For example a DetailViewer that defines the following fields:
    // <pre>
    // fields : [
    //     {name: "field1"},
    //     {name: "field2"}
    // ],
    // </pre>
    // Might have the following data:
    // <pre>
    // data : [
    //     {field1: "foo", field2: "bar"},
    //     {field1: "field1 value", field2: "field2 value"}
    // ]
    // </pre>
    // Each element in the data array above is an instance of DetailViewerRecord - notice that
    // these are specified simply as object literals with properties.
    //
    // @treeLocation Client Reference/Grids/DetailViewer
    // @visibility external
    //<

    //> @attr detailViewerRecord.linkText (string : null : IRW)
    // The HTML to display in this record for fields with type set to "link". This overrides
    // +link{attr:detailViewerField.linkText}.
    //
    // @see attr:detailViewerField.type
    // @see attr:detailViewerField.linkText
    // @see attr:detailViewer.linkTextProperty
    // @visibility external
    //<

    // Fields
    // ---------------------------------------------------------------------------------------

    //>	@attr	detailViewer.fields		(Array of DetailViewerField : null : IRW)
    //
    // An array of field objects, specifying the order and type of fields to display in this
    // DetailViewer.  In DetailViewers, the fields specify rows.
    //
    // @visibility external
    //<

    //> @object DetailViewerField
    //
    // An object literal with a particular set of properties used to configure the display of
    // and interaction with the rows of a +link{DetailViewer}.
    //
    // @treeLocation Client Reference/Grids/DetailViewer
    // @visibility external
    //<

    //> @attr DetailViewerField.name (String : null : IR)
    // Name property used to identify the field, and determines which attribute from
    // records will be displayed in this field.
    // <P>
    // Must be unique within the DetailViewer as well as a valid JavaScript identifier,
    // as specified by ECMA-262 Section 7.6 (the <smartclient>+link{String.isValidID()}</smartclient>
    // <smartgwt>StringUtil.isValidID()</smartgwt> function can be used to test whether
    // a name is a valid JavaScript identifier).
    // <P>
    // The attribute of the records to display in this field may also be set by
    // +link{displayField}.
    // @visibility external
    //<

    //> @attr DetailViewerField.displayField (String : null : IR)
    // If <code>displayField</code> is defined for the field then the DetailViewer will
    // display the <code>displayField</code> attribute of records instead of the attribute
    // given by the +link{name} of the field.
    // @visibility external
    //<
    
    //> @attr DetailViewerField.dataPath (String : null : IRA) 
    // dataPath property allows this field to display detail from nested data structures
    // @visibility external
    //<

    //> @method DetailViewerField.formatCellValue()
    // Optional method to format the value to display for this field's cells. Takes precedence 
    // over +link{DetailViewer.formatCellValue()} for cells in this field.
    // @param value (string) the raw value of the cell
    // @param record (detailViewerRecord) the record being displayed
    // @param field (detailViewerField) the field being displayed
    // @param viewer (detailViewer) the detailViewer containing this field
    // @visibility external
    //<
    
    //> @attr detailViewerField.dateFormatter (DateDisplayFormat : null : [IRW])
    // Display format to use for date type values within this field. 
    // <P>
    // The +link{detailViewerField.timeFormatter} may also be used to format underlying Date values as
    // times (ommitting the date part entirely). If both <code>dateFormatter</code> and
    // <code>timeFormatter</code> are specified on a field, for
    // fields specified as +link{detailViewerField.type,type "time"} the
    // <code>timeFormatter</code> will be used, otherwise the <code>dateFormatter</code>
    // <P>
    // If <code>field.dateFormatter</code> and <code>field.timeFormatter</code> is unspecified,
    // date display format may be defined at the component level via
    // +link{detailViewer.dateFormatter}, or for fields of type <code>"datetime"</code>
    // +link{detailViewer.datetimeFormatter}. Otherwise the
    // default is to use the system-wide default normal date format, configured via
    // +link{Date.setNormalDisplayFormat()}.  Specify any valid +link{type:DateDisplayFormat} to 
    // change the format used by this item.
    //
    // @see listGrid.dateFormatter
    // @see listGrid.datetimeFormatter
    // @see listGridField.timeFormatter
    // @visibility external
    //<
    
    //>	@attr detailViewerField.timeFormatter (TimeDisplayFormat : null : [IRWA])
    // Time-format to apply to date type values within this field.  If specified, any
    // dates displayed in this field will be formatted as times using the appropriate format.
    // This is most commonly only applied to fields specified as type <code>"time"</code> though
    // if no explicit +link{detailViewerField.dateFormatter} is specified it will be respected for other 
    // fields as well.
    // <P>
    // If unspecified, a timeFormatter may be defined 
    // +link{detailViewer.timeFormatter,at the component level} and will be respected by fields
    // of type <code>"time"</code>.
    //
    // @group appearance
    // @visibility external
    //<
    //timeFormatter:null

    //> @attr detailViewerField.decimalPrecision (number : null : [IRW])
    // @include dataSourceField.decimalPrecision
    //
    // @group appearance
    // @serverDS allowed
    // @visibility external
    //<

    //> @attr detailViewerField.decimalPad (number : null : [IRW])
    // @include dataSourceField.decimalPad
    //
    // @group appearance
    // @serverDS allowed
    // @visibility external
    //<

    //>	@attr	detailViewerField.imageSize (number : null : IRW)
    // Size of images shown for fieldTypes image in this field.
    // <P>
    // If set to a String, assumed to be a property on each record that specifies the image
    // height.  For example, if <code>field.imageSize</code> is "logoSize",
    // <code>record.logoSize</code> will control the size of the image.
    //
    // @see attr:detailViewerField.imageWidth
    // @see attr:detailViewerField.imageHeight
    //
    // @group imageColumns
    // @visibility external
	//<

  	//>	@attr	detailViewerField.imageWidth (number : null : IRW)
    // Width of images shown for fieldTypes image in this field.
    // <P>
    // If set to a String, assumed to be a property on each record that specifies the image
    // width.  For example, if <code>field.imageWidth</code> is "logoWidth",
    // <code>record.logoWidth</code> will control the width of the image.
    //
    // @see attr:detailViewerField.imageSize
    // @see attr:detailViewerField.imageHeight
    //
    // @group imageColumns
    // @visibility external
	//<

  	//>	@attr	detailViewerField.imageHeight (number : null : IRW)
    // Height of image shown for fieldTypes image in this field.
    // <P>
    // If set to a String, assumed to be a property on each record that specifies the image
    // height.  For example, if <code>field.imageHeight</code> is "logoHeight",
    // <code>record.logoHeight</code> will control the height of the image.
    //
    // @see attr:detailViewerField.imageSize
    // @see attr:detailViewerField.imageWidth
    //
    // @group imageColumns
    // @visibility external
	//<

    //> @attr   detailViewerField.imageURLPrefix (string : null : IRWA)
    // If this field has type set to <code>"image"</code>
    // and the URL for the image displayed is not absolute, the path of the URL will be relative
    // to this string<br>
    //
    // @group imageColumns
    // @visibility external
    //<

    //> @attr detailViewerField.linkText (String : null : IRW)
    // The HTML to display for values of this field if the field type is set to "link". 
    // <P>
    // This property sets linkText that will be the same for all records.  You can set linkText
    // on a per-record basis via +link{attr:detailViewerRecord.linkText}.
    //
    // @see attr:detailViewerField.type
    // @see attr:detailViewerRecord.linkText
    // @see attr:detailViewer.linkTextProperty
    // @see attr:detailViewerField.linkTextProperty
    // @visibility external
    //<

    //> @attr detailViewerField.linkTextProperty (string : null : IRW)
    // Name of the property in a DetailViewerRecord that holds the HTML to display for values
    // of this field if the field type is set to "link".
    //
    // @see attr:detailViewerField.type
    // @see attr:detailViewerRecord.linkText
    // @see attr:detailViewerField.linkText
    // @see attr:detailViewer.linkTextProperty
    // @visibility external
    //<

    //> @attr detailViewerField.linkURLPrefix (string : null : IRWA)
    // If this field has type set to <code>"link"</code>, setting this property will apply a
    // standard prefix to the link URL when displaying values of this field.
    // @see attr:detailViewerField.type
    // @visibility external
    //<

    //> @attr detailViewerField.linkURLSuffix (string : null : IRWA)
    // If this field has type set to <code>"link"</code>, setting this property will apply a
    // standard suffix to the link URL when displaying values of this field.
    // @see attr:detailViewerField.type
    // @visibility external
    //<

    //> @attr detailViewerField.target (string : "_blank" : IRW)
    // By default, clicking a link rendered by this item opens it in a new browser window.  You 
    // can alter this behavior by setting this property.  The value of this property will be 
    // passed as the value to the <code>target</code> attribute of the anchor tag used to render 
    // the link.  <code>target</code> is applicable only if the field type is set to "link".
    // @see detailViewerField.type
    // @visibility external
    //<

    //> @attr detailViewerField.format (FormatString : null : IR)
    // +link{FormatString} for numeric or date formatting.  See +link{dataSourceField.format}.
    // @group exportFormatting
    // @visibility external
    //<

    //> @attr detailViewerField.exportFormat (FormatString : null : IR)
    // +link{FormatString} used during exports for numeric or date formatting.  See
    // +link{dataSourceField.exportFormat}.
    // @group exportFormatting
    // @visibility external
    //<

    // ------------
    // Hilite Icons
    // ------------
     
    //> @attr detailViewer.hiliteIcons (Array of String : ["[SKINIMG]/Dialog/notify.png", "[SKINIMG]/Dialog/warn.png", "[SKINIMG]/actions/approve.png"] : IR)
    // @include dataBoundComponent.hiliteIcons
    // @group hiliting
    // @visibility external
    //<
    
    //> @attr detailViewer.hiliteIconPosition (HiliteIconPosition : "before" : IR)
    // @include dataBoundComponent.hiliteIconPosition
    // @group hiliting
    // @visibility external
    //<

    //> @attr detailViewer.hiliteIconSize (number : 12 : IRW)
    // @include dataBoundComponent.hiliteIconSize 
    // @see hiliteIconWidth
    // @see hiliteIconHeight
    // @see DetailViewerField.hiliteIconSize
    // @group hiliting
    // @visibility external
    //<
        
    //> @attr detailViewer.hiliteIconWidth (number : null : IRW)
    // @include dataBoundComponent.hiliteIconWidth
    // @group hiliting
    // @visibility external
    //<

    //> @attr detailViewer.hiliteIconHeight (number : null : IRW)
    // @include dataBoundComponent.hiliteIconHeight
    // @group hiliting
    // @visibility external
    //<
    
    //> @attr   detailViewer.hiliteIconLeftPadding (number : 2 : IRW)
    // @include dataBoundComponent.hiliteIconLeftPadding
    // @group hiliting
    // @visibility external
    //<

    //> @attr   detailViewer.hiliteIconRightPadding (number : 2 : IRW)
    // @include dataBoundComponent.hiliteIconRightPadding
    // Can be overridden at the field level    
    // @group hiliting
    // @visibility external
    //<
  
    //> @attr detailViewerField.canHilite (boolean : null : IRW)
    // @include listGridField.canHilite
    // @group hiliting
    // @visibility external
    //<

    //> @attr detailViewerField.hiliteIconPosition (HiliteIconPosition : null : IR)
    // When +link{detailViewer.hiliteIcons} are present, where the hilite icon will be placed 
    // relative to the field value.  See +link{type:HiliteIconPosition}.
    // Overrides +link{detailViewer.hiliteIconPosition}
    // @group hiliting
    // @visibility external
    //<

    //> @attr detailViewerField.hiliteIconSize (number : null : IRW)
    // Default width and height of +link{detailViewer.hiliteIcons, hilite icons} in this field.
    // Takes precedence over hiliteIconWidth, hiliteIconHeight and hiliteIconSize specified at
    // the component level.
    // Can be overridden via +link{hiliteIconWidth,hiliteIconWidth} and +link{hiliteIconHeight,hiliteIconHeight}
    // @group hiliting
    // @see detailViewer.hiliteIconSize
    // @see detailViewerField.hiliteIconWidth
    // @see detailViewerField.hiliteIconHeight
    // @visibility external
    //<

    //> @attr detailViewerField.hiliteIconWidth (number : null : IRW)
    // Width for hilite icons for this field.
    // Overrides +link{detailViewer.hiliteIconSize}, +link{detailViewer.hiliteIconWidth}, and
    // +link{detailViewerField.hiliteIconSize}.
    // @group hiliting
    // @visibility external
    //<
        
    //> @attr detailViewerField.hiliteIconHeight (number : null : IRW)
    // Height for hilite icons for this field.
    // Overrides +link{detailViewer.hiliteIconSize}, +link{detailViewer.hiliteIconHeight}, and
    // +link{DetailViewerField.hiliteIconSize}.
    // @group hiliting
    // @visibility external
    //<

    //> @attr   detailViewerField.hiliteIconLeftPadding (number : null : IRW)
    // How much padding should there be on the left of +link{detailViewer.hiliteIcons, hilite icons} 
    // for this field?
    // Overrides +link{detailViewer.hiliteIconLeftPadding}
    // @group hiliting
    // @visibility external
    //<
        
    //> @attr   detailViewerField.hiliteIconRightPadding (number : null : IRW)
    // How much padding should there be on the right of +link{DetailViewer.hiliteIcons, hilite icons} 
    // for this field?
    // Overrides +link{detailViewer.hiliteIconRightPadding}
    // @group hiliting
    // @visibility external
    //<


    //> @method detailViewerField.showIf
    //
    // If specified on a field, this method is evaluated at draw time to determine whether or
    // not to show this particular field.
    // <p>
    // This method can be specified either as a function or a string that will be
    // auto-converted to a function.
    //
    // @param viewer (DetailViewer) The DetailViewer
    // @param valueList (List of DetailViewerRecord)
    //
    // @return (boolean) true to show the field, false to not show it.
    //
    // @visibility external
    //<

    //> @attr detailViewerField.type (String : null : IR)
    //
    // Specifies the type of this DetailViewerField.  By default (value is <code>null</code>)
    // the field shows a field title on the left and the field value on the right.  There are
    // four special values for this attribute:
    // <ul>
    // <li>"header" - If you specify type "header", the field spans both the field name and
    // field value columns and contains text defined in the +link{DetailViewerField.value}
    // attribute with the style specified by +link{DetailViewer.headerStyle}.  You can use this
    // field type as a titled separator.
    // <li>"separator" - If you specify type "separator", the field spans both the field name
    // and the field value columns with no text, and is styled using the style specified via
    // +link{DetailViewer.separatorStyle}.  The height of the separator field can be controlled
    // via +link{DetailViewerField.height}.
    // <li>"image" For viewing, a thumbnail image is rendered in the field.  The URL of the
    // image is the value of the field, and should be absolute. The size of the image is
    // controlled by +link{attr:DetailViewerField.imageSize},
    // +link{attr:DetailViewerField.imageWidth}, +link{attr:DetailViewerField.imageHeight}
    // <li><p>"link" For viewing, a clickable html link (using an HTML anchor tag: &lt;A&gt;)
    // is rendered in the field.  The target URL is the value of the field, which is also the
    // default display value.  You can override the display value by setting
    // +link{attr:detailViewerRecord.linkText} or +link{attr:detailViewerField.linkText}.</p>
    // <p>
    // Clicking the link opens the URL in a new window by default.  To change this behavior,
    // you can set <code>field.target</code>, which works identically to the "target"
    // attribute on an HTML anchor (&lt;A&gt;) tag.  See +link{detailViewerField.target} for
    // more information.</p></li>
    // </ul>
    //
    // @visibility external
    //<

    //> @attr detailViewerField.title (HTMLString : null : IR)
    //
    // The title of the field as displayed on the left-hand side.  If left unspecified, the
    // title of the field is derived by looking up the value of
    // +link{DetailViewer.fieldIdProperty} on this field.  So, by default, the title of a field
    // is the value of its "name" property.
    //
    // @see DetailViewer.fieldIdProperty
    // @visibility external
    //<

    //> @attr detailViewerField.valueMap (object : null : IR)
    //
    // A property list (or an expression that evaluates to a property list)
	// specifying a mapping of internal values to display values for the field (row).
    //
    // @visibility external
    //<

    //> @attr detailViewerField.escapeHTML (boolean : null : IR)
    // By default HTML values in DetailViewer cells will be interpreted by the browser.
    // Setting this flag to true will causes HTML characters to be escaped, meaning the
    // raw value of the field (for example <code>"&lt;b&gt;AAA&lt;/b&gt;"</code>) is displayed
    // to the user rather than the interpreted HTML (for example <code>"<b>AAA</b>"</code>)
    //
    // @visibility external
    //<

    //> @attr detailViewerField.value (HTMLString : "undefined" : IR)
    //
    // When a field specifies its +link{detailViewerField.type} to be "header", the value of
    // this attribute specifies the header text.
    //
    // @visibility external
    //<

    
    //> @attr detailViewerField.width (Number : null : IR)
    //
    // @visibility internal
    //<

    //> @attr detailViewerField.height (Number : null : IR)
    //
    // For +link{DetailViewerField.type}: <code>"separator"</code>, this attribute specifies
    // the height of the separator.
    //
    // @visibility external
    //<

    //>	@method	detailViewerField.getCellStyle()
    // Optional method to return the CSS class for cells in this field. If specified, this method
    // will be called from +link{detailViewer.getCellStyle()}, and should return a css class name.
    //
    //		@param	value		(string) actual value of this cell
    //		@param	field      (object)	field object for this cell
    //		@param	record      (object) record object for this cell
    //      @param  viewer      (DetailViewer) the viewer instance to which this cell belongs
    //
    //		@return	(CSSStyleName)	CSS style for this cell
    // @group	appearance
    // @visibility external
    //<
    
    
    //>	@attr	detailViewerField.cellStyle (CSSClassName : null : IRW)
    // If specified, cells in this field will be rendered using this css className rather than
    // +link{detailViewer.cellStyle}
    // @visibility external
    //<
    
    //>	@attr	detailViewerField.printCellStyle (CSSClassName : null : IRW)
    // If specified, when generating print HTML for this detailViewer, 
    // cells in this field will be rendered using this css className rather than
    // +link{detailViewer.printCellStyle}
    // @visibility external
    //<
    

    //> @attr detailViewerField.showFileInline    (boolean : null : [IR])
    // For a field of type:"imageFile", indicates whether to stream the image and display it
    // inline or to display the View and Download icons.
    // 
    // @visibility external
    //<

    //> @attr detailViewerField.canExport (Boolean : null : IR)
    //	Dictates whether the data in this field be exported.  Explicitly set this
    //  to false to prevent exporting.  Has no effect if the underlying 
    //  +link{dataSourceField.canExport, dataSourceField} is explicitly set to 
    //  canExport: false.
    //
    // @visibility external
    //<

    //>	@attr	detailViewer.fieldIdProperty (string : "name" : IRWA)
	// Name of the field in the DetailViewerRecord which specifies the data property for that record.
    // @visibility external
	//<
	fieldIdProperty:"name",
	
	//> @attr detailViewerField.includeFrom (string : null : [IR])
	// Indicates this field's values come from another, related DataSource.  
	// The individual field will inherit settings such as +link{DetailViewerField.type,field.type}
	// and +link{DetailViewerField.title,field.title} from the related DataSource just like
	// fields from the primary DataSource.
	//
	// @visibility crossDS
	//<

    // Multi-record display
	// --------------------------------------------------------------------------------------------

    //>	@attr	detailViewer.recordsPerBlock		(number : 1 : [IRW])
    //          The number of records to display in a block. A block is a horizontal row on a page
    //          containing one or more records, as specified by the value of recordsPerBlock. The
    //          height of a block is equal to the height of a single record. The default setting of
    //          1 causes each record to appear by itself in a vertical row. Setting recordsPerBlock
    //          to 2 would cause records to appear side by side in groups of two.
    //          Use a value of "*" to indicate all records.
    // @group appearance
    // @visibility external
    //<
	recordsPerBlock:1,

    //>	@attr detailViewer.blockSeparator (HTMLString : "<br><br>" : [IRW])
    // A string (HTML acceptable) that will be written to a page to separate blocks.
    // @group appearance
    // @visibility external
    //<
	blockSeparator:"<br><br>",

    // Empty values
	// --------------------------------------------------------------------------------------------

    //>	@attr	detailViewer.showEmptyField		(Boolean : true : IRWA)
	// Whether to show the field when the value is null
    // @group appearance
    // @visibility external
	//<
	showEmptyField:true,

    //> @attr detailViewerField.emptyCellValue (HTMLString : null : IR)
    // The value to display for a cell whose value is null or the empty
    // string after applying formatCellValue and valueMap (if any).
    // <p>
    // This is the field-specific attribute.  You may also set the emptyCellValue at the viewer
    // level to define the emptyCellValue for all empty fields in the viewer. 
    // 
    // @group appearance
    // @see detailViewer.emptyCellValue
    // @visibility external
    //<

    //>	@attr	detailViewer.emptyCellValue		(HTMLString : "&nbsp;" : IRWA)
	// Text to show for an empty cell
    // @group appearance
    // @visibility external
	//<
	emptyCellValue:"&nbsp;",

    // Labels
	// --------------------------------------------------------------------------------------------
    //>	@attr detailViewer.labelPrefix (HTMLString : "" : IRW)
	// text to put before a label
    // @group labels
    // @visibility external
	//<
	labelPrefix:"",

    //>	@attr detailViewer.labelSuffix (HTMLString : ":" : IRW)
	// text to put after a label
    // @group labels
    // @visibility external
	//<
	labelSuffix:":",

    //> @attr detailViewer.valueAlign (alignEnum : "left" : IRW)
    // horizontal alignment of values
    // @group values
    //<
    valueAlign:"left",

    //>	@attr	detailViewer.wrapLabel		(Boolean : false : IRW)
	// Should the label be allowed to wrap, or be fixed to one line no matter how long
    // @group labels
    // @visibility external
	//<

    //> @attr detailViewer.wrapValues (Boolean : true : IR)
    // Whether values should be allowed to wrap by default, or should be shown on one line
    // regardless of length.
    //
    // @group labels
    // @visibility external
    //<
    wrapValues: true,

    // internal property used by tileGrid to force table size to be width 100%
    useInnerWidth: true,
    // internal property to clip cell values
    clipValues: false,

	// CSS styles
	// --------------------------------------------------------------------------------------------

    //>	@attr detailViewer.styleName (CSSStyleName : "detailViewer" : IRW)
	// CSS style for the component as a whole.
    // @group appearance
    // @visibility external
	//<
	styleName:"detailViewer",

    //>	@attr detailViewer.blockStyle (CSSStyleName : "detailBlock" : IRW)
	// CSS style for each block (one record's worth of data).
    // @group appearance
    // @visibility external
	//<
	blockStyle:"detailBlock",

    //>	@attr	detailViewer.labelStyle		(CSSStyleName : "detailLabel" : IRW)
	//			CSS style for a normal detail label
    // @group appearance
    // @visibility external
	//<
	labelStyle:"detailLabel",

    //>	@attr	detailViewer.cellStyle		(CSSStyleName : "detail" : IRW)
	//			CSS style for a normal value
    // @group appearance
    // @visibility external
	//<
	cellStyle:"detail",

    //>	@attr	detailViewer.headerStyle		(CSSStyleName : "detailHeader" : IRW)
	//			CSS style for a header
    // @group appearance
    // @visibility external
	//<
	headerStyle:"detailHeader",

    //> @attr detailViewer.printCellStyle (CSSStyleName : null : IRW)
    // Optional CSS style for a cell in printable HTML for this component. If unset
    // +link{detailViewer.cellStyle} will be used for printing as well as normal presentation.
    // @group printing
    // @visibility external
    //<

    //> @attr detailViewer.printLabelStyle (CSSStyleName : null : IRW)
    // Optional CSS style for a label cell in printable HTML for this component. If unset
    // +link{detailViewer.labelStyle} will be used for printing as well as normal presentation.
    // @group printing
    // @visibility external
    //<

    //> @attr detailViewer.printHeaderStyle (CSSStyleName : null : IRW)
    // Optional CSS style for a header in printable HTML for this component. If unset
    // +link{detailViewer.headerStyle} will be used for printing as well as normal presentation.
    // @group printing
    // @visibility external
    //<

    //>	@attr	detailViewer.separatorStyle		(CSSStyleName : "detail" : IRW)
	//			CSS style for a separator
    // @group appearance
    // @visibility external
	//<
	separatorStyle:"detail",

    //>	@attr	detailViewer.cellPadding		(number : 3 : [IRW])
    //          The amount of empty space, in pixels, surrounding each detailViewer value in its
    //          cell.
    //<
	cellPadding:3,

    //> @attr detailViewer.dateFormatter (DateDisplayFormat : null : [IRW])
    // How should Date type values be displayed in this DetailViewer by default?
    // <P>
    // This property specifies the default DateDisplayFormat to apply to Date values
    // displayed in this grid for all fields except those of +link{detailViewerField.type,type "time"}
    // (See also +link{detailViewer.timeFormatter}).<br>
    // If +link{detailViewer.datetimeFormatter} is specified, that will be applied by default
    // to fields of type <code>"datetime"</code>.
    // <P>
    // Note that if +link{detailViewerField.dateFormatter} or +link{detailViewerField.timeFormatter} are
    // specified those properties will take precedence over the component level settings.
    // <P>
    // If unset, date values will be formatted according to the system wide 
    // +link{Date.setNormalDisplayFormat(),normal display format}.
    //
    // @visibility external
    //<
    //dateFormatter:null,


    //> @attr detailViewer.datetimeFormatter (DateDisplayFormat : null : [IRW])
    // Display format to use for fields specified as type 'datetime'.  Default is to use the
    // system-wide default long ("normal") date time format, configured via
    // +link{Date.setNormalDatetimeDisplayFormat()}.  Specify any
    // valid +link{type:DateDisplayFormat} to change the display format for datetimes used by this 
    // viewer.
    // <smartclient>
    // May be specified as a function. If specified as  a function, this function will
    // be executed in the scope of the Date
    // and should return the formatted string.
    // </smartclient>
    // <P>
    // May also be specified at the field level via
    // +link{detailViewerField.dateFormatter}
    // 
    // @see listGridField.dateFormatter
	// @group appearance
    // @visibility external
    //<
    
    //>	@attr detailViewer.timeFormatter (TimeDisplayFormat : null : [IRW])
	// Display format to use for fields specified as type 'time'.  May also be specified at 
    // the field level via +link{detailViewerField.timeFormatter}.<br>
    // If unset, time fields will be formatted based on the system wide 
    // +link{Time.setNormalDisplayFormat()}
	// @group appearance
    // @visibility external
	//<

    //> @attr detailViewer.linkTextProperty (string : "linkText" : [IRW])
    // Property name on a record that will hold the link text for that record.
    // <p>
    // This property is configurable to avoid possible collision with data values in the
    // record.
    // <p>
    // Use +link{detailViewerField.linkTextProperty} if you have more than one link field and
    // the fields' records do not use the same property to store the linkText.
    // @see attr:detailViewerField.linkText
    // @see attr:detailViewerField.linkTextProperty
    // @visibility external
    //<
    linkTextProperty : "linkText",


    
	// Empty Message
	// --------------------------------------------------------------------------------------------

	//>	@attr	detailViewer.showEmptyMessage		(Boolean : true : IRWA)
	// Show +link{attr:detailViewer.emptyMessage} when there is no data to display?
    // @see emptyMessage
    // @group emptyMessage
    // @visibility external
	//<
	showEmptyMessage:true,

    //>	@attr detailViewer.emptyMessage (HTMLString : "No items to display." : IRW)
    //          The string to display in the body of a detailViewer with no records.
    // @group emptyMessage
    // @visibility external
    //<
	emptyMessage:"No items to display.",

    //>	@attr	detailViewer.emptyMessageStyle		(CSSStyleName : "normal" : IRWA)
    // CSS style to display this message in
    // @group emptyMessage
    // @visibility external
	//<
	emptyMessageStyle:"normal",

    //>	@attr	detailViewer.loadingMessage (HTMLString : "&amp;nbsp;\${loadingImage}" : IRW)
    // The string to display in the body of a detailViewer which is loading records.
    // Use <code>"\${loadingImage}"</code> to include +link{Canvas.loadingImageSrc,a loading image}.
    // @group emptyMessage
    // @visibility external
    //<
    loadingMessage:"&nbsp;${loadingImage}",

    //>	@attr	detailViewer.loadingMessageStyle		(CSSStyleName : "normal" : IRWA)
    // CSS style to use for the +link{loadingMessage}.
    // @group emptyMessage
    // @visibility external
	//<
    loadingMessageStyle:"normal",

    // ---------------------------------------------------------------------------------------
    // About two values worth of data.   Keeps the DV from taking up the 100px default height
    // without being unexpectedly small when it has no data.
    defaultHeight:35 ,

    showLabel: true
});


// add methods
isc.DetailViewer.addMethods({


//>	@method	detailViewer.initWidget()	(A)
//			initializes the list of fields
//			sets up the data (if specified)
//
//		@param	[all arguments]	(object)	objects with properties to override from default
//<
initWidget : function () {
	// call the superclass function
	this.Super("initWidget",arguments);

    // set field state if necessary, call setFields() otherwise
    if (this.fieldState != null) this.setFieldState(this.fieldState);
    else this.setFields(this.fields);

    // create context menu for field picker
    if (this.canPickFields) {
        this.contextMenu = isc.Menu.create({
            data: [ this.createFieldPickerWindowMenuItem(this.configureFieldsText) ]
        });
    }
},

//>	@method	detailViewer.setData()  ([])
// Sets the data displayed by this detail viewer.
//
//      @visibility external
//		@param	newData		(object or array)	new data to be displayed
//<
setData : function (newData) {

    // clear old observation
    if (this.data) this.ignore(this.data, "dataChanged");

    this.invalidateUserCache();

	// remember the new data
	this.data = newData;

    if (this.data && this.data.dataChanged) {
        this.observe(this.data, "dataChanged", "observer.dataChanged()");
    }

	// and mark the viewer as dirty so it'll be redrawn
	this.markForRedraw("new data");
},

dataChanged : function () {
    // call setFields() on dataChanged - causes showIf to be re-evaluated
    this.setFields(this.completeFields);
    this.invalidateUserCache();
    this.applyHilites();
    this.markForRedraw();
},

//>	@method	detailViewer.getData()	(A)
//			return the data to be displayed
//
//		@return	(Object)	data for this widget - either Object or Array
//<
getData : function () { return this.data; },


//>	@method	detailViewer.getFields()	(A)
//			return the list of fields to display
//
//		@return	(List of DetailViewerField)	array of objects to display
//<
getFields : function () { return this.fields; },


//>	@method	detailViewer.getInnerHTML()	(A)
//			return the HTML for this widget
//		@return	(string)	HTML to display
//<
getInnerHTML : function () {
	// get the data to display
	var valueList = this.getData();

	//>DEBUG
	if (this.fields == null || this.fields.length == 0) {
		return "Note: you must define detailViewer.fields to specify what to display!";
	}
	//<DEBUG

    // If the data is a ResultSet, poke the ResultSet to fetch data and return the loading
    // message.  Note that if fetchData() is called, this isn't the codepath that causes the
    // initial fetch - see DataBoundComponent.requestVisibleRows.
    if (isc.ResultSet != null && isc.isA.ResultSet(valueList) && !valueList.lengthIsKnown()) {
        // request only the first row.  If this ResultSet is using fetchMode:"paged" (not the
        // default for DetailViewer) and has already issued a request for data, asking for
        // anything beyond the current rs.resultSize will initiate additional fetches, possibly
        // for rows that don't exist, but the ResultSet doesn't know that while
        // !lengthIsKnown().
        valueList.get(0);
        return this.loadingMessageHTML();
    }
    
	if (valueList == null || (valueList.getLength && valueList.getLength() == 0)) { 
		return this.emptyMessageHTML();
	}

	// normalize the data into an array
	if (!isc.isA.List(valueList)) valueList = [valueList];

    // With DV, we have a situation where a failed load results in lengthIsKnown being true,
    // but the data actually consists of a single loading marker.  The upshot of this is that
    // the DetailViewer displays an empty record, which is probably fair enough (since the 
    // load probably failed because the record doesn't exist).  However, if the load failed 
    // because we're offline, we want to show that specifically, so treat it as a special case
    if (Array.isLoading(valueList.get(0)) && this.isOffline())  {
        return this.emptyMessageHTML();
    }

	// if there's only one item or we're supposed to show all columns together
	if (valueList.getLength() == 1 || this.recordsPerBlock == "*") {
		// call the blockHTML routine with all items
		return this.getBlockHTML(valueList);
	} else {
		// otherwise call it for each item separately
		var output = isc.StringBuffer.newInstance();
		for (var startRow = 0; startRow < valueList.getLength(); startRow += this.recordsPerBlock) {
			output.append(this.getBlockHTML(valueList.getRange(startRow, startRow + this.recordsPerBlock)), this.blockSeparator);
		}
		return output.toString();
	}
},

//>	@method	detailViewer.getBlockHTML()	(A)
// Return the HTML for either a single object or a set of objects where each object gets one
// column.
//		@return	(string)	HTML to display
//<
getBlockHTML : function (valueList) {
    
    if (valueList.getLength == null) {
        var newArray = [];
        for (var i = 0; i < valueList.length; i++) {
            newArray[i] = valueList[i];
        }
        valueList = newArray;
    }

	// how many separate value objects are we dealing with ?
	var numValues = valueList.getLength();
	// start the table to display the output
    var output = "<TABLE BORDER=0 CELLSPACING=0 CLASS='" + this.blockStyle +
            "' CELLPADDING='" + this.cellPadding +
            "' style='width:" + (this.useInnerWidth && !this.isPrinting
                                 ? this.getInnerWidth() + "px"
                                 : "100%") +
            (this.clipValues ? ";table-layout:fixed" : "") +
            "'>";


	// output the data

	// get the list of fields to output
	var fields = this.fields;

	// iterate through each of the keys in detailFields and output the info for each field
	for (var fieldNum = 0, fieldLength = fields.length; fieldNum < fieldLength; fieldNum++) {
		var field = fields[fieldNum];
		if (!field || field.hidden || field.visible == false) continue;

		// if the field has a showIf property
		if (field.showIf) {
			// CALLBACK API:  available variables:  "viewer,valueList"
			// Convert a string callback to a function

			if (!isc.isA.Function(field.showIf)) {
				isc.Func.replaceWithMethod(field, "showIf", "viewer,valueList");
			}

			// skip this if the showIf returns false
			if (field.showIf(this, valueList) == false) continue;
		}

		// MAE: if we don't want to show fields that have empty/null values
		// check the appropriate values and skip if they are all empty/null
		// This does not apply to headers and separators
		var type = field.type ? field.type : "";
		if (type != "separator" && type != "header" && !this.showEmptyField) {
			var valuesAreEmpty = true;
			for (var i = 0; i < valueList.getLength(); i++) {
				var value = valueList.get(i)[field[this.fieldIdProperty]]
				if (!(value == null || value == "")) {
					valuesAreEmpty = false;
					break;
				}
			}
			// if no values were found, continue to the next field
			if (valuesAreEmpty) continue;
		}

		// if there is a specific output function for this field, call that
		if (field.output) {
			// CALLBACK API:  available variables:  "fieldNum,field,valueList"
			// Convert a string callback to a function
			if (!isc.isA.Function(field.output)) {
				isc.Func.replaceWithMethod(field, "output", "fieldNum,field,valueList");
			}
			output += field.output(fieldNum, field, valueList);
		} else {
			// output this particular field
			output += this.outputItem(fieldNum, field, valueList);
		}
	}
	// end the table
	output += "</TABLE>";
	// and return the output
	return output;
},

//>	@method	detailViewer.fieldIsVisible()
// Check whether a field is currently visible
//
// @param	field  (String | DetailViewerField)	field to be checked
// @return (boolean) true if the field is currently visible, false otherwise.
// @visibility external
//<
fieldIsVisible : function (field) {
    var fieldObj = field;
	// If passed a field ID, look for it in the completeFields array rather than the fieldsArray
	// as it is may be defined, but not visible    
    if (!isc.isAn.Object(fieldObj)) fieldObj = this.getSpecifiedField(field);

    return this.fields.contains(fieldObj);
},

//>	@method	detailViewer.outputItem()	(A)
// Output one row of the data as HTML
//		@param	fieldNum		(number)		number of the field to output
//		@param	field		(object)		pointer to the field to output
//		@param	valueList	(array)			list of values to output
//
//		@return	(string)	HTML output
//<
outputItem : function (fieldNum, field, valueList) {
    
	var type = (field.type ? field.type : "value"),
		// functionName == name of a function to call to ouput this particular type of object
		functionName = "output_"+type,
		output = ""
	;
	// if a function by that name cannot be found, default to output_value
	if (!this[functionName]) functionName = "output_value";
	// start the table row
	output += "<TR"
			+ (this.rowClass != null ? " CLASS='" + this.rowClass + "'" : "")
			+ ">";

	// output
	output += this[functionName](fieldNum, field, valueList);

	// end the row
	output += "</TR>\r";

	// return the output
	return output;
},


//
//	OUTPUT HTML FOR DIFFERENT TYPES OF OBJECTS
//


//>	@method	detailViewer.output_value()	(A)
//			output a normal value for each field in valueList
//		@param	fieldNum		(number)		number of the field to output
//		@param	field		(object)		pointer to the field to output
//		@param	valueList	(array)			list of values to output
//
//		@return	(string)	HTML output
//<
output_blob : function (fieldNum, field, valueList) {
    return this.output_binary(fieldNum, field, valueList);
},
output_upload : function (fieldNum, field, valueList) {
    return this.output_binary(fieldNum, field, valueList);
},
output_binary : function (fieldNum, field, valueList) {
	// output the label
	var output = "<TD WIDTH=10% CLASS='" +
            (this.isPrinting ? this.printLabelStyle || this.labelStyle : this.labelStyle) +
            "' ALIGN=RIGHT"
			+ (this.wrapLabel ? ">" : " NOWRAP><NOBR>")
			+ this.labelPrefix + (field.title ? field.title : field[this.fieldIdProperty]) + this.labelSuffix
			+ "<\/NOBR><\/TD>"
	;

	// iterate for each object in valueList, outputing the object[field[this.fieldIdProperty]]
	for (var i = 0; i < valueList.getLength(); i++) {

        var record = valueList.get(i),
            index = this.getData().indexOf(record),
            filenameField = this.dataSource ? 
                                isc.DS.getDataSource(this.dataSource).getFilenameField(field.name) :
                                field.name + "_filename",
            name = record[filenameField],
            viewAction = "'" + this.getID() +".view",
            dlAction = "'" + this.getID() +".download",
            completion = "";
            if (field && field.name) {
                completion = "Cell(" + index + ", \"" + field.name + "\")'";
            } else {
                completion = "Row(" + index + ")'";
            }
            var viewIconHTML = isc.Canvas.imgHTML("[SKIN]actions/view.png", 16, 16, null,
                            "style='cursor:"+isc.Canvas.HAND+"' onclick="+viewAction+completion);
            var viewIconHTML = isc.Canvas.imgHTML("[SKIN]actions/view.png", 16, 16, null,
                        "style='cursor:"+isc.Canvas.HAND+"' onclick="+viewAction+completion),
            downloadIconHTML = isc.Canvas.imgHTML("[SKIN]actions/download.png", 16, 16, null,
                        "style='cursor:"+isc.Canvas.HAND+"' onclick="+dlAction+completion),
            value = viewIconHTML + "&nbsp;" + downloadIconHTML + 
                (name ? "&nbsp;" + name : "");

		// output the value as a cell
		output += "<TD CLASS='" + this.getCellStyle(value, field, record, this) + "'>"
                  + value + "<\/TD>";
	}
	return output;

},

viewRow : function (index) {
    isc.DS.get(this.dataSource).viewFile(this.getData().get(index));
},

downloadRow : function (index) {
    isc.DS.get(this.dataSource).downloadFile(this.getData().get(index));
},

viewCell : function (index, fieldName) {
    isc.DS.get(this.dataSource).viewFile(this.getData().get(index), fieldName);
},

downloadCell : function (index, fieldName) {
    isc.DS.get(this.dataSource).downloadFile(this.getData().get(index), fieldName);
},

// given an Array of records (valueList) output one complete DetailViewer row, which will have
// multiple data columns if recordsPerBlock > 1
output_value : function (fieldNum, field, valueList) {

	// output the label
    var output;
    if (this.showLabel) {
        output = "<TD WIDTH="+(this.labelWidth != null ? this.labelWidth : "10%")+" CLASS='" +
                (this.isPrinting ? this.printLabelStyle || this.labelStyle : this.labelStyle) + "' ALIGN=RIGHT"
            + (this.wrapLabel ? ">" : " NOWRAP><NOBR>")
            + this.labelPrefix + (field.title ? field.title : field[this.fieldIdProperty]) + this.labelSuffix
            + "<\/NOBR><\/TD>"
        ;
    } else {
        output = "";
    }
    // resolve field level valueMap reference strings to objects before going into the for loop
    if (field.valueMap && isc.isA.String(field.valueMap))
        field.valueMap = this.getGlobalReference(field.valueMap);

	// iterate for each object in valueList, outputing the object[field.name]
	for (var i = 0; i < valueList.getLength(); i++) {
        var record = valueList.get(i),
            formattedValue;
        if (field.type == "image") {
            // if any of field.imageWidth/Height/Size are set as strings, assume they are property
            // names on the record
            var dimensions = isc.Canvas.getFieldImageDimensions(field, record);
            
            var src = this.getCellValue(record, field), prefix =
                field.imageURLPrefix || field.baseURL || field.imgDir;

            if (src == this._resolveEmptyDisplayValue(field)) {
                formattedValue = this._resolveEmptyDisplayValue(field);
            } else {
                formattedValue = this.imgHTML(src, dimensions.width, dimensions.height, null, 
                    field.extraStuff, prefix, field.activeAreaHTML);
            }
        } else if (field.type == "link") {
            // The value of the field is the URL for a link.  The URL will also be used for
            // the text of the link, unless it is overridden by a linkText property on the
            // field or on the record.
            var target = field.target || "_blank",
                linkTextProperty = field.linkTextProperty || this.linkTextProperty,
                value = this.getCellValue(record, field),
                linkText = record[linkTextProperty] || field.linkText || value,
                href = value;

            if (href == this._resolveEmptyDisplayValue(field)) {
                formattedValue =  this._resolveEmptyDisplayValue(field);
            } else {
                // Each field may add a prefix and/or suffix onto the URL.
                if (field.linkURLPrefix) {
                    href = field.linkURLPrefix + href;
                }
                if (field.linkURLSuffix) {
                   href = href + field.linkURLSuffix;
                }
                formattedValue = this.linkHTML(href, linkText, target);
            }

        } else {
            // NOTE: calls formatCellValue()
            formattedValue = this.getCellValue(record, field);
        }

        // determine the cell style
        var rawValue = this.getRawValue(record,field);
        var cellStyle;
        if (field.getCellStyle) {
            cellStyle = field.getCellStyle(rawValue, field, record, this);
        } else {
            cellStyle = (this.getCellStyle(rawValue, field, record, this) || this.cellStyle);
        }

        // put together a STYLE attribute 
        var styleStr = " style='";
        if (this.clipValues) styleStr += "overflow:hidden;";
        styleStr += "text-align:" + this.valueAlign;

        // allow custom CSS text per field
        if (this.getCellCSSText) {
            var cssText = this.getCellCSSText(rawValue, field, record, this);
            if (cssText != null) styleStr += isc.semi + cssText;
        }

        // output the value as a cell
        styleStr += "'";
        output += "<TD CLASS='" + cellStyle + "'" + styleStr +
            (field.height?"HEIGHT='"+field.height+"' ":"") +
            (this.wrapValues ? ">" : " NOWRAP><NOBR>") +
            formattedValue +
            (this.wrapValues ? "<\/NOBR>" : "") +
            "<\/TD>";
	}
	return output;
},

//>	@method	detailViewer.getCellCSSText()
// Return CSS text for styling this cell, which will be applied in addition to the CSS class
// for the cell, as overrides.
// <p>
// "CSS text" means semicolon-separated style settings, suitable for inclusion in a CSS
// stylesheet or in a STYLE attribute of an HTML element.
//
// @param value (any) actual value of this cell
// @param field (DetailViewerField)	field object for this cell
// @param record (Record) record object for this cell
// @param viewer (DetailViewer) the viewer instance to which this cell belongs
// @return (CSSText) CSS text to add to this cell
//
// @group appearance
// @visibility external
//<
getCellCSSText : function (value, field, record, viewer) {
    
    return this.getRecordHiliteCSSText(record, "", field, true);
},

//>	@method	detailViewer.getCellStyle()
// Return the CSS class for a cell. Default implementation calls
// +link{detailViewerField.getCellStyle(), getCellStyle()} on the field if defined, otherwise
// returns +link{detailViewer.cellStyle,this.cellStyle}
// @param value (any) actual value of this cell
// @param field (DetailViewerField)	field object for this cell
// @param record (Record) record object for this cell
// @param viewer (DetailViewer) the viewer instance to which this cell belongs
// @return (CSSStyleName) CSS style for this cell
// @group appearance
// @visibility external
//<
// <P>
// Note: if +link{detailViewer.printCellStyle} is specified this will be used as the default
// styling for cells instead of <code>this.cellStyle</code> when generating printable HTML for
// this component.
getCellStyle : function (value, field, record, viewer) {
    if (field) {
        if (field.getCellStyle) return field.getCellStyle(value,field,record,viewer);
        if (this.isPrinting && field.printCellStyle) {
            return field.printCellStyle;
        }
        if (field.cellStyle) {
            return field.cellStyle;
        }
    }
    return (this.isPrinting && this.printCellStyle != null) ? this.printCellStyle
                                                              : this.cellStyle;
},

//> @method DetailViewer.formatCellValue()
// Optional method to format the value to display for cells in this DetailViewer.
// Note that if +link{detailViewerField.formatCellValue()} is specified this method will not
// be called for cells within that field.
// @param value (string) the raw value of the cell (may be formatted by
//   +link{detailViewerField.formatCellValue()}
// @param record (detailViewerRecord) the record being displayed
// @param field (detailViewerField) the field being displayed
// @visibility external
//<

getSelectedRecord : function() {
    return this.data.get(0);
},

// getCellValue - method to actually get the value for a record.
// Called from 'output_value', which handles writing the <TD> tags around the value and
// outputting a whole block of records.
// Can be overridden by a user.
// Also if 'getCellValue()' is specified at a field level, will apply it to this cell.

getCellValue : function (record, field) {

    // get the value of this key for that field
    var value = this.getRawValue(record,field);
    if (isc.isA.String(field.formatCellValue)) {
        field.formatCellValue = isc.Func.expressionToFunction("value,record,field,viewer",
                                                              field.formatCellValue);
    }
    if (field.getCellValue != null) {
        if (isc.isA.String(field.getCellValue)) {
            field.getCellValue = isc.Func.expressionToFunction("value,record,field,viewer",
                                                                field.getCellValue);
        }
        // note the 'value' passed into field.getCellValue() is the raw value, not valueMapped, etc.
        // This matches the ListGrid's field-level getCellValue behavior.
        value = field.getCellValue(value, record, field, this);
        if (field.formatCellValue) value = field.formatCellValue(value, record, field, this);
    } else {

        // if the col has an 'valueMap' parameter, treat the value as a key in the valueMap
        if (field.valueMap != null) value = isc.getValueForKey(value, field.valueMap);

        if (field.formatCellValue) value = field.formatCellValue(value, record, field, this);
    }

    // Support formatCellValue if specified.
    // - to match ListGrid behavior, don't run both field.formatCellValue and DV.formatCellValue()
    if (field.formatCellValue == null && this.formatCellValue) {
        value = this.formatCellValue(value, record, field);
    } else {
        // if no value was specified, output the emptyCellValue
        if (value == null || isc.is.emptyString(value)) {
            value = this._resolveEmptyDisplayValue(field);
        } else {
            // _formatDataType ensures that non string values get formatted as strings as appropriately
            value = this._formatDataType(record, field, value);

            // field.escapeHTML is a boolean - if true, escape HTML chars in the value such as "<".
            var escapeHTML = field.escapeHTML;
            // asHTML was old name
            if (escapeHTML == null) escapeHTML = field.asHTML;
            if (escapeHTML) value = value.asHTML();
        }
    }

    // handle formula and summary fields
    if (field) {
        if (field.userFormula) value = this.getFormulaFieldValue(field, record);
        else if (field.userSummary) value = this.getSummaryFieldValue(field, record);
        else if (field.type=="imageFile") {
            var filenameField = this.dataSource ? 
                        isc.DS.getDataSource(this.dataSource).getFilenameField(field.name) :
                        field.name + "_filename";
            if (record[filenameField] != null) {
                if (field.showFileInline != false) {
                    if (!record[field[this.fieldIdProperty] + "_imgURL"]) {
                        var dimensions = isc.Canvas.getFieldImageDimensions(field, record), 
                            image = this.getDataSource().streamFile(record, field[this.fieldIdProperty]);
                        value = record[field[this.fieldIdProperty] + "_imgURL"] = 
                            this.imgHTML(image, dimensions.width, dimensions.height);
                    } else 
                        value = record[field[this.fieldIdProperty] + "_imgURL"];
                } else {
                    value = this.getViewDownloadHTML(field, record);
                }
            }
        } else if (field.showFileInline == true) { // non-imageFile field
            this.logWarn("getCellValue(): Unsupported field-type for showFileInline: "+field.type);
        }
        if (isc.isA.Number(value)) {
            if (isc.SimpleType.inheritsFrom(field.type, "float") &&
                (field.decimalPrecision != null || field.decimalPad != null))
            {
                value = isc.Canvas.getFloatValueAsString(value, field.decimalPrecision,
                                                         field.decimalPad);
            } else if (field.precision != null) {
                value = isc.Canvas.getNumberValueAsString(value, field.precision, field.type);
            }
        }
    }

    // apply hilites to capture htmlBefore/after
    var hilites = this.getFieldHilites(record, field);
    if (hilites != null) {
        value = this.applyHiliteHTML(hilites, value);
        // Finally, apply the hiliteIcon if present
        value = this.applyHiliteIcon(hilites, field, value);
    }

    return value;
},

getViewDownloadHTML : function (field, record) {

    if (record == null) return null;

    var filenameField = this.dataSource ? 
                isc.DS.getDataSource(this.dataSource).getFilenameField(field.name) :
                field.name + "_filename",
        name = record[filenameField];

    
    if (!field.filenameSuppressed && (name == null || isc.isA.emptyString(name))) {
		return "&nbsp;";
	}

    var viewIconHTML = isc.Canvas.imgHTML("[SKIN]actions/view.png", 16, 16, null,
	        "style='cursor:"+isc.Canvas.HAND+
            "' onclick='"+this.getID()+".viewFile("+record+","+field+")'");
    var downloadIconHTML = isc.Canvas.imgHTML("[SKIN]actions/download.png", 16, 16, null,
            "style='cursor:"+isc.Canvas.HAND+
            "' onclick='alert('running');"+this.getID()+".downloadFile("+record+","+field+")'");

    return "<nobr>" + viewIconHTML + "&nbsp;" + downloadIconHTML + "&nbsp;" + name + "</nobr>";
},

viewFile : function (record, field) {
    isc.DS.get(this.dataSource).viewFile(record, field.name);
},

downloadFile : function (record, field) {
    isc.DS.get(this.dataSource).downloadFile(record, field.name);
},

// _formatDataType, format a cell value based on data type.
// At this point we've checked for field.formatValue etc. Current implementation:
// - if a formatter is defined on the SimpleType make use of it
// - otherwise format dates according to standard DBC rules:
//  - use field.dateFormatter or field.timeFormatter if specified
//  - otherwise use component.dataFormatter, component.timeFormatter, component.datetimeFormatter
//    depending on field type.
_$date:"date",
_formatDataType : function (record, field, value, isMultipleElement) {

    if (!isMultipleElement && field && field.multiple && isc.isA.Array(value)) {
        var values = [];
        for (var i = 0; i < value.length; i++) {
            values[i] = this._formatDataType(record, field, value[i], true);
        }
        return values.join(field.multipleValueSeparator || ", ");
    }
    
    // Apply declarative FormatString if present
    if (field && (isc.isA.Number(value) || isc.isA.Date(value)) && field.format) {
        return isc.isA.Number(value) ? isc.NumberUtil.format(value, field.format)
                                     : isc.DateUtil.format(value, field.format);
    }

    // Format date values, according to standard DBC rules
    // Respect field level dateFormatter/timeFormatter if specified
    // Otherwise respect component level dateFormatter/datetimeFormatter/timeFormatter depending
    // on specified field-type.
    if (isc.isA.Date(value)) {
        if (this._formatAsTime(field)) {
            var isLogicalTime = isc.SimpleType.inheritsFrom(field.type, "time");
            value = isc.Time.toTime(value, this._getTimeFormatter(field), isLogicalTime);
        } else {
            // If the field is a logical date field
            //  use short formatter, since the "time" part of
            // the date has no real meaning (and no long formatters suppress time).
            // Otherwise use the long format by default for DetailViewers.
            if (isc.SimpleType.inheritsFrom(field.type, "date") && 
                !isc.SimpleType.inheritsFrom(field.type, "datetime")) 
            {
                
                value = value.toShortDate(this._getDateFormatter(field), false);
            } else {
                value = value.toNormalDate(this._getDateFormatter(field));
            }
        }
    }

    // If normalDisplayFormatter is specified on the simpleType make use of it
    if (field._normalDisplayFormatter != null) {
        value = field._simpleType.normalDisplayFormatter(value, field, this, record);
    }

    return isc.iscToLocaleString(value);
},

_formatAsTime : function (field) {
    if (field == null) return false;
    if (field.dateFormatter == null && field.timeFormatter != null) return true;
    if (field.timeFormatter == null && field.dateFormatter != null) return false;
    return isc.SimpleType.inheritsFrom(field.type, "time");
},

_getDateFormatter : function (field) {
    if (field.dateFormatter) return field.dateFormatter;
    if (field.displayFormat != null && isc.SimpleType.inheritsFrom(field.type, "date")) {
        return field.displayFormat;
    }
    if (this.datetimeFormatter != null && isc.SimpleType.inheritsFrom(field.type, "datetime")) {
        return this.datetimeFormatter;
    }
    return this.dateFormatter;
},

_getTimeFormatter : function (field) {
    if (field.timeFormatter) return field.timeFormatter;
    if (field.displayFormat != null && isc.SimpleType.inheritsFrom(field.type, "time")) {
        return field.displayFormat;
    }
    return this.timeFormatter;
},

//> @method detailViewer.getRecordIndex()
// @param record (Record) the record whose index is to be retrieved
// @return index (Number) index of the record, or -1 if not found
// @include dataBoundComponent.getRecordIndex
// @visibility external
//<
getRecordIndex : function (record) {
    var result = this.Super('getRecordIndex', arguments);
    if (result==-1) result = 0;
    return result;
},


//>	@method	detailViewer.output_header()	(A)
//			output a header field
//		@param	fieldNum		(number)		number of the field to output
//		@param	field		(object)		pointer to the field to output
//		@param	valueList	(array)			list of values to output
//
//		@return	(string)	HTML output
//<
output_header : function (fieldNum, field, valueList) {
	return "<TD COLSPAN=" + (valueList.getLength() + 1) + " CLASS='" +
    (this.isPrinting && this.printHeaderStyle ? this.printHeaderStyle : this.headerStyle) +
    "'>"+field.value+"</TD>";
},


//>	@method	detailViewer.output_separator()	(A)
//			output a separator field
//		@param	fieldNum		(number)		number of the field to output
//		@param	field		(object)		pointer to the field to output
//		@param	valueList	(array)			list of values to output
//
//		@return	(string)	HTML output
//<
output_separator : function (fieldNum, field, valueList) {
	var width = (field.width == null ? field.defaultSeparatorWidth : field.width),
		height = (field.height == null ? field.defaultSeparatorHeight : field.height)
	;
	return "<TD COLSPAN=" + (valueList.getLength() + 1) + " CLASS='" + this.separatorStyle + "'>"
			+ isc.Canvas.spacerHTML(width, height)
			+ "</TD>";
},


//>	@method	detailViewer.getEmptyMessage()	(A)
//			return the empty message to display when no data was specified
//			this is a function so you can override it in complex cases
//			in simple cases, just returns this.emptyMessage
//
//		@return	(string)	HTML output
//<
getEmptyMessage : function () {
	return this.emptyMessage;
},
getLoadingMessage : function () {
	return this.loadingMessage == null ? "&nbsp;" : this.loadingMessage.evalDynamicString(this, {
        loadingImage: this.imgHTML(isc.Canvas.loadingImageSrc, 
                                   isc.Canvas.loadingImageSize, 
                                   isc.Canvas.loadingImageSize)
        });
},


//>	@method	detailViewer.emptyMessageHTML()	(A)
// Return the message to show if the component has no data. Default implementation returns a 
// centered +link{detailViewer.emptyMessage} or "&amp;nbsp;" if showEmptyMessage is false.  If
// the component has no data because the browser is offline, we instead display the 
// +link{dataBoundComponent.offlineMessage} or "&amp;nbsp;" if showOfflineMessage is false
// @return	(string)	HTML output
// @visibility external
//<
emptyMessageHTML : function () {
    
    if (this.isOffline()) {
        if (!this.showOfflineMessage) return "&nbsp;";
    } else {
        if (!this.showEmptyMessage) return "&nbsp;";
    }

	return "<TABLE WIDTH=100%>"
			+ "<TR><TD CLASS='" + this.emptyMessageStyle + "' ALIGN=CENTER><BR><BR>"
			+ (this.isOffline() ? this.offlineMessage : this.getEmptyMessage())
			+ "<\/TD><\/TR><\/TABLE>";
            
},

//>	@method	detailViewer.loadingMessageHTML()	(A)
//			return the message to display while the data is loading
//
//		@return	(string)	HTML output
//<
loadingMessageHTML : function () {
	return "<TABLE WIDTH=100%>"
			+ "<TR><TD CLASS='" + this.loadingMessageStyle + "' ALIGN=CENTER><BR><BR>"
			+ this.getLoadingMessage()
			+ "<\/TD><\/TR><\/TABLE>";
},

setFieldState : function (fieldState) {
    if (fieldState == null && this.fieldState != null) {
        if (isc.isA.String(this.fieldState)) {
            fieldState = this.evalViewState(this.fieldState, "fieldState")
        }
    } else fieldState = this.evalViewState(fieldState, "fieldState");

    this.completeFields = this._setFieldState(fieldState, true);
    this.setFields(this.completeFields);
    this.markForRedraw();
    this.fieldStateChanged();
},

// minimal implementation of setFields()
setFields : function (newFields) {
    if (this.completeFields == null || this.fields == null) this.fields = [];

	// bind the passed-in fields to the DataSource and store
    this.completeFields = this.bindToDataSource(newFields);

    if (this.completeFields == null) this.completeFields = [];

	this.deriveVisibleFields();
    this.invalidateUserCache();
},

// determine which fields should be shown, and add them to the visible fields array.
// (Used as an internal helper - developers should call 'refreshFields' instead)
deriveVisibleFields : function () {
	// NOTE: we use setArray() so that this.fields remains the same array instance.
    this.fields.setArray(this.getVisibleFields(this.completeFields));
},

getVisibleFields : function (fields) {
	var valueList = this.getData(),
        returnFields = fields.duplicate();
    for (var i=0; i<fields.length; i++) {
        var field = fields.get(i);
        if (!this.fieldShouldBeVisible(field, valueList) || 
            field.visible==false) returnFields.remove(field);
    }
	return returnFields;
},

fieldShouldBeVisible : function (field, valueList) {
    // evaluate a showIf expression if present
    if (field.showIf != null) {
        // CALLBACK API:  available variables:  "viewer,valuesList"
        // Convert a string callback to a function
        
        if (field.showIf == this._$false || field.showIf == this._$falseSemi) return false;
        isc.Func.replaceWithMethod(field, "showIf", "viewer,valueList");
        if (!field.showIf(this, valueList)) return false;
    }
    return true;
},

// Formula/summary -related overrides from DBC
getTitleFieldValue : function (record) {
    var titleField = this.getDataSource().getTitleField(),
        title = this.getCellValue(record, this.getDataSource().getField(titleField));

    return title;
},

// DBC level override to call local getCellValue implementation - Formula/Summary builders
getStandaloneFieldValue : function (record, fieldName) {
	var field = this.getField(fieldName),
	    value = this.getCellValue(record, field);
	// In DetailViewer we apply hilite CSS to the cell.
	// Therefore write a span with the appropriate cssText around the actual value
	// in order to pick up the hilite styling
	value = this.addHiliteSpan(record, field, value);
	return value;
},

// basic show and hide methods
hideField : function (fieldName) {
    this.toggleField(fieldName, false);
},
showField : function (fieldName) {
    this.toggleField(fieldName, true);
},
toggleField : function (fieldName, showNow) {
    var field = this.getField(fieldName);

    field.showIf = showNow ? "true" : "false";
    field.visible = showNow;
    this.setFields(this.getAllFields());
    this.markForRedraw();
    this.fieldStateChanged();
},

//>	@method	detailViewer.getField()	(A)
// Return a field by fieldName
//
//		@return	(DetailViewerField) requested field or null
//<
getField : function (fieldName) {
    var allFields = this.getAllFields(),
	    fields = this.fields,
		field;

    if (isc.isAn.Object(fieldName) && fieldName[this.fieldIdProperty] != null) {
        // passed a field-object, check it's real by finding it by name
        field = allFields.find(this.fieldIdProperty, fieldName[this.fieldIdProperty]) || 
            fields.find(this.fieldIdProperty, fieldName[this.fieldIdProperty]);
    } else if (isc.isA.Number(fieldName)) {
		field = allFields[fieldName] || fields[fieldName];
	} else {
		field = allFields.find(this.fieldIdProperty, fieldName) ||
		    fields.find(this.fieldIdProperty, fieldName)
	}

    return field;
},

// Overridden from DBC - use 'getCellValue' to return the "formatted" value
getFormattedValue : function (record, fieldName, value) {
    return this.getCellValue(record, this.getSpecifiedField(fieldName));
},
//> @method detailViewer.getPivotedExportData()
// Export visual description of DetailViewer data into a form suitable for external
// processing.
// @param settings (Object) contains configuration settings for the export, including:<br/>
//        includeHiddenFields (Boolean) - controls if hidden fields should be exported<br/>
//        allowedProperties (Array) optional array of CSS property names (camelCaps format)
//             constraining the allowed properties to be returned
// @return value (String) exported data
//<
// * Data is exported as an array of objects, with one object per visual row of the
//   DetailViewer grid - meaning one row per field.
// * The title of each visible field is mapped to the property "title" for each object.
// * Each record's value for the corresponding field is mapped to the properties
//   "value1", "value2", ..., up to the number of records specified in this.recordsPerBlock.
//   Records extending beyond this.recordsPerBlock are not exported.
// * Additionally, if CSS hilighting styles are present on a record's field, the CSS text is
//   converted into an object mapping CSS properties in camelCaps format to CSS values, and the
//   object is stored in <property name>$style.
// * Null record values are converted to empty strings.
//
// Example object:
//  [
//  { title: "Foo Fighter", value1: "1", "value1$style": { backgroundColor: "#f00000" } },
//  { title: "bar", value1: "baz" },
//  { title: "xyzzy", value1: "" },
//  { title: "Summary Field", value1: "1 --- baz", "value1$style": { font-weight: "bold" } }
//  ]
getPivotedExportData : function (settings) {
    var exportOutput = [],
        fields = this.getAllFields(),
        data = this.data,
        includeHiddenFields,
        allowedProperties,
        alwaysExportExpandedStyles
        ;

    if (isc.isA.Object(settings)) {
        includeHiddenFields = settings.includeHiddenFields;
        allowedProperties = settings.allowedProperties;
        alwaysExportExpandedStyles = settings.alwaysExportExpandedStyles;
    }
    if (isc.isA.ResultSet(data)) data = data.getAllLoadedRows();
    if (!isc.isA.Array(data)) data = [data];
    
    for (var fieldIndex = 0; fieldIndex < fields.length; fieldIndex++) {
        var field = fields[fieldIndex],
            exportObject = {},
            recordsPerBlock = this.recordsPerBlock;
            
        exportObject.title = field.title || field.name;
        if (isc.isA.String(exportObject.title)) exportObject.title = this.htmlUnescapeExportFieldTitle(exportObject.title);

        // Implement default value of 1 and "*" -> unbounded
        if (recordsPerBlock == null) recordsPerBlock = 1;
        if (recordsPerBlock == "*") recordsPerBlock = 100000;
            
        if ((!this.fields.contains(field)) && !includeHiddenFields) continue;
        
        for (var rowIndex = 0;
             rowIndex < recordsPerBlock && rowIndex < data.getLength();
             rowIndex++)
        {
            var record = data[rowIndex],
                fieldNum = this.getFieldNum(field.name),
                exportProp = "value" + (rowIndex+1),
                styleProp = exportProp + "$style";
            
            var value = this.getExportFieldValue(record, field.name, fieldNum);
            
            if (!(value == null || value == "&nbsp;")) exportObject[exportProp] = value;
            
            this.addDetailedExportFieldValue(exportObject, styleProp, record, field, fieldNum, 
                                             allowedProperties, alwaysExportExpandedStyles);
            if (exportObject[styleProp] == null || exportObject[styleProp] == "&nbsp;")
                delete exportObject[styleProp];
        }
        exportOutput.push(exportObject);
    }
    return exportOutput;
},


//> @type DetailViewerViewState  
// An object containing the stored grouping information for a detailViewer.
// Note that this object is not intended to be interrogated directly, but may be stored 
// (for example) as a blob on the server for state persistence across sessions.
// 
// @group viewState
// @visibility external
//<

//>	@method	detailViewer.getViewState() 
// Returns a snapshot of the current view state of this DetailViewer.<br>
// This includes the field, sort and hilite state of the grid, returned as a
// +link{type:DetailViewerViewState} object.<br>
// This object can be passed to +link{detailViewer.setViewState()} to reset this detail viewer's vew state
// to the current state (assuming the same data / fields are present in the detail viewer).<br>
// @group viewState
// @see type:DetailViewerViewState
// @see detailViewer.setViewState();
// @visibility external
// @return (DetailViewerViewState) current view state for the detail viewer.
//<    
getViewState : function (returnObject) {

    var state = {
        field:this.getFieldState(),
        sort:this.getSortState(),
        hilite:this.getHiliteState()
    };

    // Available so TG can call Super() and get an object back
    if (returnObject) return state;
    return "(" + isc.Comm.serialize(state,false) + ")";
},

//>	@method	detailViewer.setViewState() 
// Reset this detail viewer's view state to match the +link{type:DetailViewerViewState} object passed in.<br>
// Used to restore previous state retrieved from the detail viewer by a call to 
// +link{detailViewer.getViewState()}.
//
// @param viewState (DetailViewerViewState) Object describing the desired view state for the
// detail viewer
// @group viewState
// @see detailViewer.getViewState()
// @visibility external
//<
setViewState : function (state) {
    state = this.evalViewState(state, "viewState")
    if (!state) return;

    // Order is somewhat important - for example show fields before potentially sorting 
    // by them, etc
    if (state.field) this.setFieldState(state.field);
    this.setSortState(state.sort);
    this.setHiliteState(state.hilite);
},


//>	@attr detailViewer.showDetailFields (Boolean : true : IR)
// @include dataBoundComponent.showDetailFields
//<

//> @attr detailViewer.fieldPickerWindow (AutoChild FieldPickerWindow : null : IR)
// Instance of +link{FieldPickerWindow} used if +link{canPickFields} is set.
// @visibility external
//<

fieldPickerWindowDefaults : {
    autoParent: "none",
    _constructor: "FieldPickerWindow"
},

//> @attr detailViewer.canPickFields (Boolean : false : IR)
// If set, right-clicking on the DetailViewer will show a context menu that offers to bring up a
// +link{FieldPicker} for configuring which fields are displayed and their order.
// @visibility external
//<
canPickFields: false,

//> @attr detailViewer.configureFieldsText (String : "Configure Fields..." : [IR])
// The title for the Configure Fields menu item.
// @group i18nMessages
// @visibility external
//<    
configureFieldsText: "Configure Fields...",


//> @attr detailViewer.fieldPickerFieldProperties (Array of String : null : IR)
// Names of properties on +link{DetailViewerField} for which the +link{fieldPicker} should
// show an editing interface, for convenience.
// <P>
// For example, specify ["decimalPad", "decimalPrecision"] to allow end users to modify
// +link{detailViewerField.decimalPad} and +link{detailViewerField.decimalPrecision} respectively.
// @visibility external
//<
fieldPickerFieldProperties: null

});	// END isc.DetailViewer.addMethods()

// Register stringMethods for this class - instance methods that can be defined as strings using
// specified keywords (replaced by arguments in the function created)
isc.DetailViewer.registerStringMethods({

    getCellValue:"record,field",
    getCellStyle:"value,field,record,viewer",
    getCellCSSText:"value,field,record,viewer",
    formatCellValue:"value,record,field,viewer",
    fieldStateChanged:""
});
