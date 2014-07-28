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






//> @class FormItem
// A UI component that can participate in a DynamicForm, allowing editing or display of one of
// the +link{dynamicForm.values,values tracked by the form}.
// <P>
// <smartclient>FormItems are never created via the +link{Class.create(),create()} method,
// instead, an Array of plain +link{type:Object,JavaScript objects} are passed as
// +link{DynamicForm.items} when the form is created.</smartclient>
//
// <smartgwt>FormItems do not render themselves, instead, they are provided to a
// +link{DynamicForm} via +link{DynamicForm.setItems()}</smartgwt>
// <p>
// See the +link{DynamicForm} documentation for details and sample code.
//
// @treeLocation Client Reference/Forms/Form Items
// @visibility external
//<
isc.ClassFactory.defineClass("FormItem");



// Copy across the canvas method to generate DOM IDs for the various elements we will be
// creating
isc.FormItem.addMethods({
    // we use getDOMID to generate our elements' unique dom ids
    // If we're writing out 'inactiveHTML' we may be rendering multiple elements on the page
    // with the same 'partName' but we want them to have separate unique IDs. In this
    // case we'll modify the partName to ensure unique IDs for all the inactive elements.
    _inactiveTemplate:[null, "_inactiveContext", null],
    _getDOMID : function (partName, dontCache, dontReuse, inactiveContext) {
        
        // If we're in the process of writing out inactive HTML, pick up the current 
        // inactiveContext ID, or lazily create a new one if we haven't got one yet.
        
        if (inactiveContext == null && this.isInactiveHTML()) {
            inactiveContext = this._currentInactiveContext;
        }
        // see if this becomes expensive (string concat)
        if (inactiveContext != null) {
            this._inactiveTemplate[0] = partName;
            this._inactiveTemplate[2] = inactiveContext;
            partName = this._inactiveTemplate.join(isc.emptyString);
            if (this.logIsDebugEnabled("inactiveEditorHTML")) {
                this.logDebug("_getDOMID called for inactive HTML -- generated partName:"
                    + partName, "inactiveEditorHTML");
            }
            
            // ignore 'dontCache' if we're writing out inactive context.
            
            dontCache = false;
            
        }
        return isc.Canvas.getPrototype()._getDOMID.apply(this, [partName,dontCache,dontReuse]);
    },
    
    _getDOMPartName:isc.Canvas.getPrototype()._getDOMPartName,
    
    
    _releaseDOMIDs:isc.Canvas.getPrototype()._releaseDOMIDs,
    reuseDOMIDs:false
});

isc.FormItem.addClassMethods({
    
    //> @classMethod FormItem.create()
    // FormItem.create() should never be called directly, instead, create a +link{DynamicForm}
    // and specify form items via +link{DynamicForm.items,form.items}.
    //
    // @visibility external
    //<
    // Log a warning if called directly
    create : function (A,B,C,D,E,F,G,H,I,J,K,L,M) {
        this.logWarn(
            "Unsupported call to " + this.getClassName() + ".create(). FormItems must be created " +
            "by their containing form. To create form items, use the 'items' property of a DynamicForm " +
            "instance. See documentation for more details."
        );
        // If we're passed properties combine them into a single raw object - if this is then
        // assigned to a form's "items" attribute the developer will likely get the expected
        // behavior.
        // (No need to call Super)
        return isc.addProperties({}, A,B,C,D,E,F,G,H,I,J,K,L,M);
    },

    // getNewTagID() -- a method to broker out IDs for the form element tags, if no name is
    // specified for the form element
    // (If a name is specified we'll use that instead)
    getNewTagID : function () {
        if (this._currentTagIDNumber == null) this._currentTagIDNumber = 0;
        this._currentTagIDNumber += 1;
        return "isc_FormItemElement_ID_" + this._currentTagIDNumber;
    },
    
    // setElementTabIndex()
    // Given a DOM element (a form item element), and a tabIndex, update the tabIndex on
    // the appropriate element.
    setElementTabIndex : function (element, tabIndex) {
        // Set the tabIndex property on the element
        element.tabIndex = tabIndex;
        
        // In mozilla setting a tabIndex to -1 is not sufficient to remove it from the
        // page's tab order -- update the 'mozUserFocus' property as well to achieve this
        // if we're passed a desired tabIndex less than zero (or revert this property if
        // necessary from a previous exclusion from the page's tab order)
        
        if (isc.Browser.isMoz) {
            element.style.MozUserFocus = (tabIndex < 0 ? "ignore" : "normal");
        }
    },
    
    
    
    _aboutToFireNativeElementFocus : function (item) {
        if (!isc.Browser.isIE) return;
        var activeElement = this.getActiveElement();
        
        if (activeElement && activeElement.tagName == null) activeElement = null;
        
        // Note: this will work for elements in the DOM that are not part of ISC form items.
        if (activeElement && 
            ((activeElement.tagName.toLowerCase() == this._inputElementTagName && 
              activeElement.type.toLowerCase() == this._textElementType) || 
              activeElement.tagName.toLowerCase() == this._textAreaElementTagName)) 
        {
            // IE proprietary API
            var range = activeElement.createTextRange();
            range.execCommand("Unselect");
        }
    },
    

    // Helper method to determine if the item passed in is text based
    _textBasedItem : function (item, checkForPopUp) {
        if (isc.isA.FormItem(item)) item = item.getClassName();
        
        if (!this._textClassNames) {
            this._textClassNames = {
                text:true,
                TextItem:true,
                textItem:true,
                textArea:true,
                TextAreaItem:true,
                textAreaItem:true
            }
            this._popUpClassNames = {
                popUpTextArea:true,
                PopUpTextAreaItem:true,
                popUpTextAreaItem:true
            }
        }
        
        return this._textClassNames[item] || (!checkForPopUp || this._popUpClassNames[item]);
    },
    
    // Native handlers to be applied to elements written into the DOM
    // --------------------------------------------------------------------------------------
    
    // Focus/blur handelers to be applied to Form item elements.
    // Applied directly to the element, so we need to determine which item we are a part of
    // and call the appropriate focus/blur handler method on that item.
    _nativeFocusHandler : function () {
        if (!window.isc || !isc.DynamicForm) return;

        isc.EH._setThread("IFCS");

        var result;
        if (isc.Log.supportsOnError) {
            result = isc.FormItem.__nativeFocusHandler(this);
        } else {
            try {
                result = isc.FormItem.__nativeFocusHandler(this);
            } catch (e) {
                isc.Log._reportJSError(e);
            }
        }
        isc.EH._clearThread();
        return result;
    },
    __nativeFocusHandler : function (element) {
        
        //!DONTCOMBINE
        var itemInfo = isc.DynamicForm._getItemInfoFromElement(element),
            item = itemInfo.item;
        
        
        if (item && item.isDisabled()) {
            element.blur();        
            return;
        }
            
        if (item) {
            return item._nativeElementFocus(element, item);
        }
        isc.EH._clearThread();
    },
    
    _nativeBlurHandler : function () {
        // Check for blur being fired on page unload (when the isc object is out of scope)
        if (!window.isc || !isc.DynamicForm) return;

        isc.EH._setThread("IBLR");
        var result;
        if (isc.Log.supportsOnError) {
            result = isc.FormItem.__nativeBlurHandler(this);
        } else {
            try {
                result = isc.FormItem.__nativeBlurHandler(this);
            } catch (e) {
                isc.Log._reportJSError(e);
            }
        }
        isc.EH._clearThread();
        return result;
    },
    __nativeBlurHandler : function (element) {
        //!DONTCOMBINE
        var itemInfo = isc.DynamicForm._getItemInfoFromElement(element),
            item = itemInfo.item;
        if (item && item.hasFocus) {
            return item._nativeElementBlur(element, item);
        }
    },
    
    // IE specific handler for oncut / onpaste
    _nativeCutPaste : function () {
        if (!window.isc) return;
        var element = this,
            itemInfo = isc.DynamicForm._getItemInfoFromElement(element),
            item = itemInfo.item;
        if (item && item.hasFocus) {
            return item._nativeCutPaste(element, item);
        }
    },
    
    // For some form items we make use of the native onchange handler.
    // This is a single function that will be applied directly to elements as a change handler
    // Currently used by the nativeSelectItem class and the checkboxItem class (and UploadItem)
    _nativeChangeHandler : function () {
        
        //!DONTCOMBINE
        if (!window.isc || !isc.DynamicForm) return;

        var element = this,
            itemInfo = isc.DynamicForm._getItemInfoFromElement(element),
            item = itemInfo.item;
        if (item) return item._handleElementChanged();
    },

    // Focus / blur handlers applied directly to icons
    _nativeIconFocus : function () {
        //!DONTCOMBINE

        var element = this,
            itemInfo = isc.DynamicForm._getItemInfoFromElement(element),
            item = itemInfo.item,
            iconID = itemInfo.overIcon;
        if (item) {
            
            if (item.iconIsDisabled(iconID)) element.blur();
            else return item._iconFocus(iconID, element);
        }
    },
    
    _nativeIconBlur : function () { 
        //!DONTCOMBINE
        if (!window.isc) return;
        
        var element = this,
            itemInfo = isc.DynamicForm._getItemInfoFromElement(element),
            item = itemInfo.item,
            iconID = itemInfo.overIcon;
        if (item && !item.iconIsDisabled(iconID)) return item._iconBlur(iconID, element);
    },

    // Native click handler for icons can just return false. This will cancel navigation.
    // We will fire icon.click() via the standard DynamicForm.handleClick method    
    _nativeIconClick : function () {
        return false;
    },
    
    
    _testStuckSelectionAfterRedraw : function (formItem) {
        if (!isc.Browser.isIE) return;
        this._testFocusAfterRedrawItem = formItem;
        this.fireOnPause("testStuckSelection", {target:this, methodName:"_testStuckSelection"});
    },
    _testStuckSelection : function () {
        var item = this._testFocusAfterRedrawItem;
        // Focus may have moved elsewhere etc since the refocusAfterRedraw
        if (item == null ||
            item.destroyed ||
            !item.isDrawn() ||
            !item.isVisible() ||
            !item.hasFocus) 
        {
            return;
        }
        if (item._IESelectionStuck()) {
            
            item.focusInItem();
        }
    },
    
    // Helper method to return a prompt string to show in hovers over error icons
    getErrorPromptString : function (errors) {
        var errorString = "";
        if (!isc.isAn.Array(errors)) errors = [errors];
        for (var i =0; i< errors.length; i++) {
            errorString += (i > 0 ? "<br>" : "") + errors[i].asHTML();
        };
        return errorString;
    },
    
    
    // HTML templating involving no-style-doubling string [which may change at runtime]
    _getOuterTableStartTemplate : function () {
        if (!this._observingDoublingStrings) {
            isc.Canvas._doublingStringObservers.add({
                target:this, 
                methodName:"_doublingStringsChanged"
            });
            this._observingDoublingStrings = true;
        }
        if (this._$outerTableStartTemplate == null) {
            this._$outerTableStartTemplate = [
                "<TABLE role='presentation' CELLSPACING=0 CELLPADDING=0 BORDER=0 ID='",         // 0
                ,                                                           // 1 [ID for outer table]
                // We'll apply the 'cellStyle' for the item to the outer table as styles won't
                // be inherited by sub elements of the table.
                // Explicitly avoid getting doubled borders etc.
                "' STYLE='" + isc.Canvas._$noStyleDoublingCSS,              // 2
                ,                                                           // 3 [css to override class attrs]
                "' CLASS='",                                                // 4
                ,                                                           // 5 [pick up the cellStyle css class]
                
                "'><TR>",                                                   // 6
                ,                                                           // 7 Potential first cell for 
                                                                            //   error on left...
                // Main cell - If we're showing a picker this will contain the 'control' table
                // If we're not showing a picker, this wll contain the 'text box'                                                                    
                "<TD style='",                                              // 8
                ,                                                           // 9 [possibly css for text box cell]
                "' VALIGN=",                                                // 10 
                        
                ,                                                           // 11   [v align]
                ">"                                                         // 12
                // Either the text box element (returned by getElementHTML()) or an inner control table
                
            ];
        }
        return this._$outerTableStartTemplate;
    },

    _getIconsCellTemplate : function () {
        if (!this._observingDoublingStrings) {
            isc.Canvas._doublingStringObservers.add({
                target:this, 
                methodName:"_doublingStringsChanged"
            });
            this._observingDoublingStrings = true;
        }

        if (this._$iconsCellTemplate == null) {
            this._$iconsCellTemplate = [
                "</TD><TD VALIGN=",     // 0
                ,                       // 1 [v align property for icons]
                
                " WIDTH=",              // 2
                ,                       // 3 [total icons width]
                " style='" + isc.Canvas._$noStyleDoublingCSS + "line-height:",
                ,                       // 5 iconHeight
                "px' class='",          // 6
                ,                       // 7 Apply standard cell style to the item
                "' ID='",               // 8
                ,                       // 9 ID for cell 
                                        //  (allows us to show/hide icons by writing into the cell)
                "'>",                   // 10
                null                    // 11 [icons HTML]
            ];
        }
        return this._$iconsCellTemplate;
    },
    _doublingStringsChanged : function () {
        this._$outerTableStartTemplate = null;
        this._$iconsCellTemplate = null;
    }

});

isc.FormItem.addClassProperties({
    
    _inputElementTagName : "input",
    _textElementType : "text",
    _textAreaElementTagName : "textarea",
    _cellStyleCache: {},
    _rtlCellStyleCache: {}
});

isc.FormItem.addProperties({

    // Basics
    // ---------------------------------------------------------------------------------------

    //> @type FormItemType
    // DynamicForms automatically choose the FormItem type for a field based on the
    // <code>type</code> property of the field.  The table below describes the default FormItem
    // chosen for various values of the <code>type</code> property.
    // <P>
    // You can also set +link{FormItem.editorType,field.editorType} to the classname of a
    // +link{FormItem} to override this default mapping.  You can alternatively override
    // +link{dynamicForm.getEditorType()} to create a form with different rules for which
    // FormItems are chosen.
    // <P>
    // @value "text"    Rendered as a +link{class:TextItem}, unless the length of the field (as
    // specified by +link{attr:dataSourceField.length} attribute) is larger than the value
    // specified by +link{attr:dynamicForm.longTextEditorThreshold}, a
    // +link{class:TextAreaItem} is shown.
    //
    // @value "boolean"   Rendered as a +link{class:CheckboxItem}
    //
    // @value "integer"   Same as <code>text</code> by default.  
    //                    Consider setting editorType:+link{SpinnerItem}.
    // @value "float"     Same as <code>text</code> by default.  
    //                    Consider setting editorType:+link{SpinnerItem}.
    // @value "date"      Rendered as a +link{class:DateItem}
    // @value "time"      Rendered as a +link{class:TimeItem}
    // @value "enum"      Rendered as a +link{class:SelectItem}.  Also true for any field that
    //                    specifies a +link{formItem.valueMap}.  
    //                    Consider setting editorType:+link{ComboBoxItem}.
    // @value "sequence"  Same as <code>text</code>
    // @value "link"      If +link{dataSourceField.canEdit}<code>:false</code> is set on the field,
    //                    the value is rendered as a +link{class:LinkItem}.  Otherwise the field 
    //                    is rendered as a +link{class:TextItem}.
    // @value "image"     Rendered as an image if not editable, or as a +link{TextItem} to edit
    //                    the URL or partial URL if editable
    // @value "imageFile" Rendered as a +link{class:FileItem}, or a +link{ViewFileItem} if not editable
    // @value "binary"    Rendered as a +link{class:FileItem}, or a +link{ViewFileItem} if not editable
    //
    // @see attr:FormItem.type
    // @see type:FieldType
    // @visibility external
    //<
    
    //> @attr formItem.type (FormItemType : "text" : [IR])
    // The DynamicForm picks a field renderer based on the type of the field (and sometimes other
    // attributes of the field).
    //
    // @see type:FormItemType
    // @see type:FieldType
    // @group appearance
    // @visibility external
    //< 

    //> @attr formItem.editorType (FormItem class : null : [IR])
    // Name of the FormItem to use for editing, eg "TextItem" or "SelectItem".
    // <P>
    // The type of FormItem to use for editing is normally derived automatically from
    // +link{formItem.type,field.type}, which is the data type of the field, by the rules
    // explained +link{type:FormItemType,here}.
    //
    // @see type:FormItemType
    // @see type:FieldType
    // @group appearance
    // @visibility external
    //<

    //> @attr formItem.readOnlyDisplay (ReadOnlyDisplayAppearance : "readOnly" : IRW)
    // If +link{formItem.canEdit} is set to <code>false</code>, how should this item
    // be displayed to the user?
    // @visibility external
    //<
    getReadOnlyDisplay : function () {
        return this.readOnlyDisplay || (this.form ? this.form.readOnlyDisplay : "readOnly");
    },

    //> @method formItem.setReadOnlyDisplay()
    // Setter for the +link{readOnlyDisplay} attribute.
    // @param appearance (ReadOnlyDisplayAppearance) New read-only display appearance.
    // @visibility external
    //<
    setReadOnlyDisplay : function (appearance, skipStorage) {
        if (!skipStorage) {
            this.readOnlyDisplay = appearance;
            if (isc.isA.CanvasItem(this) && isc.isA.DynamicForm(this.canvas)) {
                this.canvas.setReadOnlyDisplay(appearance);
            }
            if (this.items) {
                for (var i=0; i<this.items.length; i++) {
                    this.items[i].setReadOnlyDisplay(appearance, skipStorage);
                }
            }
        }
        if (!this.getCanEdit() && this.isDrawn()) {
            if (this.readOnlyDisplayChanged) this.readOnlyDisplayChanged(appearance);
            this.redraw();
        }
    },

    //> @attr formItem.readOnlyTextBoxStyle (FormItemBaseStyle : "staticTextItem" : IRW)
    // TextBoxStyle to apply to +link{formItem.canEdit,canEdit:false} items
    // with +link{formItem.readOnlyDisplay} set to <code>"static"</code>.
    // @visibility external
    //<
    getReadOnlyTextBoxStyle : function () {
        return this.readOnlyTextBoxStyle || 
                    (this.form ? this.form.readOnlyTextBoxStyle : "staticTextItem");
    },

    //> @attr formItem.readOnlyClipValue (Boolean : false : IRW)
    // If this item is +link{formItem.canEdit,canEdit:false}, and has
    // +link{formItem.readOnlyDisplay} set to <code>"static"</code>, should the
    // item value be clipped if it overflows the specified size of the item?
    // @visibility external
    //<
    getReadOnlyClipValue : function () {
        return this.readOnlyClipValue || (this.form ? this.form.readOnlyClipValue : false);
    },

    //> @attr formItem.name (identifier : null : IR)
    // Name for this form field.
    // <P>
    // The FormItem's name determines the name of the property it edits within the form. Must be
    // unique within the form as well as a valid JavaScript identifier, as specified by ECMA-262
    // Section 7.6 (the <smartclient>+link{String.isValidID()}</smartclient><smartgwt>StringUtil.isValidID()</smartgwt>
    // function can be used to test whether a name is a valid JavaScript identifier).
    // 
    // @group basics
    // @visibility external
    //<
    
    //> @attr formItem.dataPath (DataPath : null : IR)
    // dataPath for this item. Allows the user to edit details nested data structures in a
    // flat set of form fields
    // @visibility external
    //<

    //> @attr formItem.title             (String : null : IRW)
    // User visible title for this form item.
    // 
    // @group basics
    // @visibility external
    //<    

    //> @attr formItem.defaultValue       (any : null : IRW)
    // Value used when no value is provided for this item. Note that whenever this item's value
    // is cleared programmatically (for example via <code>item.setValue(null)</code>), it will be
    // reverted to the <code>defaultValue</code>. Developers should use the
    // +link{DynamicForm.values} object if their intention is to provide an initial value for a
    // field in a form rather than a value to use in place of <code>null</code>.
    // 
    // @see method:defaultDynamicValue
    // @group basics
    // @visibility external
    // @example fieldEnableDisable
    //<
    
    
    //> @attr formItem.value (any : null : IR)
    // Value for this form item.
    // <smartclient>This value may be set directly on the form item initialization
    // block but is not updated on live items and should not be directly accessed.
    // Once a form item has been created by the dynamicForm use +link{FormItem.setValue()} and
    // +link{FormItem.getValue()} directly.</smartclient>
    // @group basics
    // @visibility external
    //<
    
    //> @attr formItem.ID (identifier : null : IRW)
    // Global identifier for referring to the formItem in JavaScript.  The ID property is
    // optional if you do not need to refer to the widget from JavaScript, or can refer to it
    // indirectly (for example, via <code>form.getItem("<i>itemName</i>")</code>).
    // <P>
    // An internal, unique ID will automatically be created upon instantiation for any formItem
    // where one is not provided.
    //
    // @group basics
    // @visibility external
    //<

    //> @attr formItem.emptyDisplayValue (string : "" : IRW)
    // Text to display when this form item has a null or undefined value.
    // <P>
    // If the formItem has a databound pickList, and its +link{formItem.displayField} or
    // +link{formItem.valueField} (if the former isn't set) has an undefined emptyCellValue
    // field, that field will automatically be set using the emptyDisplayValue property.
    //
    // @group display_values
    // @visibility external
    //<
    emptyDisplayValue:"",

    // ValueMap
    // -----------------------------------------------------------------------------------------
        
    //> @attr formItem.valueMap (Array or Object: null : IRW)
    // In a form, valueMaps are used for FormItem types that allow the user to pick from a
    // limited set of values, such as a SelectItem.  The valueMap can be either an Array of
    // legal values or an Object where each property maps a stored value to a user-displayable
    // value.
    // <P>
    // To set the initial selection for a form item with a valueMap, use
    // +link{formItem.defaultValue}.
    // <P>
    // See also +link{dataSourceField.valueMap}.
    // 
    // @group valueMap
    // @visibility external
    //<
    
    // optionDataSource
    // ----------------------------------------------------------------------------------------

    //> @attr formItem.optionDataSource        (DataSource | String : null : IR)
    // If set, this FormItem will map stored values to display values as though a
    // +link{valueMap} were specified, by fetching records from the 
    // specified <code>optionDataSource</code> and extracting the
    // +link{formItem.valueField,valueField} and 
    // +link{formItem.displayField,displayField} in loaded records, to derive one
    // valueMap entry per record loaded from the optionDataSource.
    // <P>
    // With the default setting of +link{formItem.fetchMissingValues,fetchMissingValues}, fetches will be initiated against
    // the optionDataSource any time the FormItem has a non-null value and no corresponding
    // display value is available.  This includes when the form is first initialized, as well
    // as any subsequent calls to +link{formItem.setValue()}, such as may happen when
    // +link{DynamicForm.editRecord()} is called.  Retrieved values are automatically cached by
    // the FormItem.
    // <P>
    // Note that if a normal, static +link{formItem.valueMap,valueMap} is <b>also</b> specified for
    // the field (either directly in the form item or as part of the field definition in the
    // dataSource), it will be preferred to the data derived from the optionDataSource for
    // whatever mappings are present.
    // <P>
    // In a databound form, if +link{FormItem.displayField} is specified for a FormItem and 
    // <code>optionDataSource</code> is unset, <code>optionDataSource</code> will default to
    // the form's current DataSource 
    //
    // @see FormItem.invalidateDisplayValueCache()
    // @group display_values
    // @visibility external
    // @getter getOptionDataSource()
    // @example listComboBox
    //<

    //> @attr FormItem.optionFilterContext     (RPCRequest Properties : null : IRA)
    // If this item has a specified <code>optionDataSource</code>, and this property is
    // not null, this will be passed to the datasource as +link{rpcRequest} properties when
    // performing the fetch operation on the dataSource to obtain a data-value to display-value
    // mapping
    // @visibility external
    //<
    
    //> @attr FormItem.optionCriteria     (criteria : null : IRA)
    // If this item has a specified <code>optionDataSource</code>, and this property may be used
    // to specify criteria to pass to the datasource when
    // performing the fetch operation on the dataSource to obtain a data-value to display-value
    // mapping
    // @visibility external
    //<

    //> @attr FormItem.optionOperationId     (string : null : IRA)
    // If this item has a specified <code>optionDataSource</code>, this attribute may be set
    // to specify an explicit +link{DSRequest.operationId} when performing a fetch against the
    // option dataSource to pick up display value mapping.
    // @visibility external
    //<

    //> @attr formItem.valueField  (string : null : IR)
    // If this form item maps data values to display values by retrieving the 
    // +link{FormItem.displayField} values from an 
    // +link{FormItem.optionDataSource,optionDataSource}, this property 
    // denotes the the field to use as the underlying data value in records from the 
    // optionDataSource.<br>
    // If unset, assumed to be the +link{FormItem.name} of this form item.
    // @group display_values
    // @visibility external
    // @getter getValueFieldName()
    //<

    //> @attr formItem.displayField   (string : null : IR) 
    // Specifies an alternative field from which display values should be retrieved for this
    // item.
    // <P>
    // The display field can be either another field value in the same record or a field that
    // must be retrieved from a related +link{formItem.optionDataSource,optionDataSource}.
    // <P>
    // If this item is not databound (+link{FormItem.optionDataSource} is unset), or bound 
    // to the same dataSource as the form as a whole, this item will call
    // +link{dynamicForm.getValue,form.getValue()} 
    // the form named after is implemented by picking up the
    // value of the specified field from the Form's values object.
    // <P>
    // Otherwise this item will attempt to map its underlying value to a display value
    // by retrieving a record from the +link{FormItem.optionDataSource} where the 
    // +link{FormItem.valueField} matches this item's value, and displaying the 
    // <code>displayField</code> value from that record.
    // Note that if <code>optionDataSource</code> is set and this value is not
    // set, +link{formItem.getDisplayFieldName()} will return the dataSource title field by default.
    // <P>
    // This essentially enables the specified <code>optionDataSource</code> to be used as
    // a server based +link{group:valueMap}.
    //
    // @see FormItem.invalidateDisplayValueCache()
    // @group display_values
    // @visibility external
    // @getter getDisplayFieldName()
    //<

    //> @attr formItem.multipleValueSeparator   (string : ', ' : IR) 
    // If this item is displaying multiple values, this property will be the
    // string that separates those values for display purposes.
    //
    // @group display_values
    // @visibility external
    //<
    multipleValueSeparator: ", ",
    
    //> @attr formItem.fetchMissingValues   (Boolean : true : IRWA)
    // If this form item has a specified +link{FormItem.optionDataSource}, should the
    // item ever perform a fetch against this dataSource to retrieve the related record.
    // <P>
    // The fetch occurs if the item value is non null on initial draw of the form
    // or whenever setValue() is called. Once the fetch completes, the returned record 
    // is available via the +link{FormItem.getSelectedRecord()} api.
    // <P>
    // By default, a fetch will only occur if +link{formItem.displayField} is specified, and
    // the item does not have an explicit +link{formItem.valueMap} containing the
    // data value as a key.<br>
    // However you can also set +link{formItem.alwaysFetchMissingValues} to have a fetch occur
    // even if no <code>displayField</code> is specified. This ensures 
    // +link{formItem.getSelectedRecord()} will return a record if possible - useful for
    // custom formatter functions, etc.
    // <P>
    // Note - for efficiency we cache the associated record once a fetch has been performed, meaning
    // if the value changes, then reverts to a previously seen value, we do not kick
    // off an additional fetch to pick up the display value for the previously seen data value.
    // If necessary this cache may be explicitly invalidated via a call to 
    // +link{formItem.invalidateDisplayValueCache()}
    //
    // @group display_values
    // @see formItem.optionDataSource
    // @see formItem.getSelectedRecord()
    // @see formItem.filterLocally
    // @visibility external
    //<
    fetchMissingValues:true,
    
    //> @attr formItem.alwaysFetchMissingValues (Boolean : false : IRWA)
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
    //
    // @group display_values
    // @visibility external
    //<
    alwaysFetchMissingValues:false,
    
    //> @attr formItem.loadingDisplayValue (String : "Loading..." : IRW)
    // Value shown in field when +link{fetchMissingValues,fetchMissingValues} is active and a
    // fetch is pending. The field is also read-only while fetch is pending.
    // <P>
    // Set to <code>null</code> to show actual value until display value is loaded.
    // @group display_values
    // @group i18nMessages
    // @visibility external
    //<
    loadingDisplayValue:"Loading...",
    
    
    //> @attr formItem.filterLocally (boolean : null : IRA)
    // If this form item is mapping data values to a display value by fetching records from a 
    // dataSource (see +link{FormItem.optionDataSource}, +link{FormItem.displayField} 
    // and +link{FormItem.fetchMissingValues}), setting this property to true ensures that when
    // the form item value is set, entire data-set from the dataSource is loaded at once and 
    // used as a valueMap, rather than just loading the display value for the current value.
    // This avoids the need to perform fetches each time setValue() is called with a new value.
    // <P>
    // See also +link{PickList.filterLocally} for behavior on form items such as SelectItems
    // that show pick-lists.
    //
    // @group display_values
    // @visibility external
    //<
    
    // Data Type Formatters
    // ---------------------------------------------------------------------------------------
    // Note: dateFormatter and timeFormatter provide a way to control format of date or 
    // time data in a generic form item such as a static text item.
    // Consistent name with ListGrid.dateFormatter / timeFormatter
    
    //> @attr formItem.dateFormatter (DateDisplayFormat : null : [IRWA])
    // Display format to use for date type values within this formItem.
    // <P>
    // Note that Fields of type <code>"date"</code>, <code>"datetime"</code> or 
    // <code>"time"</code> will be edited using a +link{DateItem} or +link{TimeItem} by 
    // default, but this can be overridden - for <code>canEdit:false</code> fields, a
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
    // +link{DynamicForm.datetimeFormatter}. Otherwise the
    // default is to use the system-wide default short date format, configured via
    // +link{Date.setShortDisplayFormat()}.  Specify any valid +link{type:DateDisplayFormat} to 
    // change the format used by this item.
    // <P>
    // Note that if this is a freeform editable field, such a +link{TextItem}, with type
    // specified as <code>"date"</code> or <code>"datetime"</code> the system will automatically
    // attempt to parse user entered values back to a Date value, assuming the entered string
    // matches the date format for the field. Developers may further customize this via an
    // explicit +link{formItem.inputFormat} or via entirely custom
    // <smartclient>
    // +link{formItem.formatEditorValue} and +link{formItem.parseEditorValue} methods.
    // </smartclient>
    // <smartgwt>
    // <code>setEditorValueFormatter</code> and <code>setEditorValueParser</code> methods.
    // </smartgwt>
    // 
    // @see formItem.timeFormatter
    // @see formItem.format
    //
    // @group appearance
    // @visibility external
    //<
    //dateFormatter:null
    
    // Undocumented flag -- if no formatter is explicitly specified and we're looking at
    // a js date value should we use "normal" or "short" formatter by default.
    // Won't effect fields of type "date" since we never want to show the time (which is
    // always displayed in the "normal" format.
    useShortDateFormat:true,
    
    //> @attr formItem.timeFormatter (TimeDisplayFormat : null : [IRWA])
    // Time-format to apply to date type values within this formItem.  If specified, any
    // dates displayed in this item will be formatted as times using the appropriate format.
    // This is most commonly only applied to fields specified as type <code>"time"</code> though
    // if no explicit +link{formItem.dateFormatter} is specified it will be respected for other 
    // fields as well.
    // <P>
    // If unspecified, a timeFormatter may be defined 
    // +link{DynamicForm.timeFormatter,at the component level} and will be respected by fields
    // of type <code>"time"</code>.
    //
    // @see formItem.format
    // @group appearance
    // @visibility external
    //<
    //timeFormatter:null
    
    //> @attr formItem.displayFormat (varies : null : [IRWA])
    // Fields of type <code>"date"</code> or <code>"time"</code> will be edited using
    // a +link{DateItem} or +link{TimeItem} by default.
    // <P>
    // However this can be overridden - for <code>canEdit:false</code> fields, a
    // +link{StaticTextItem} is used by default, and the developer can always specify 
    // a custom +link{formItem.editorType} as well as +link{formItem.type,data type}.
    // <P>
    // For fields of type <code>"date"</code>, set this property to a valid
    // +link{dateDisplayFormat} to specify how the date should be formatted.
    // <br>
    // For fields of type <code>"time"</code>, set this property to a valid 
    // +link{type:TimeDisplayFormat, TimeDisplayFormat} to specify how the time should be formatted.
    // <br>
    // Note that if +link{formItem.dateFormatter} or +link{formItem.timeFormatter} are specified
    // they will take precedence over this setting.
    // <P>
    // If this field is of type <code>"date"</code> and is editable, the 
    // +link{formItem.inputFormat} may be used to specify how user-edited date strings will
    // be parsed.
    //
    // @deprecated in favor of +link{formItem.format}, +link{formItem.dateFormatter} and 
    // +link{formItem.timeFormatter}
    // @see formItem.format
    // @see formItem.inputFormat
    // @see formItem.dateFormatter
    // @see formItem.timeFormatter
    // @visibility external
    //<
    
    //> @attr formItem.inputFormat (DateInputFormat : null : [IRWA])
    // For fields of type <code>"date"</code>, if this is an editable field such as a
    // +link{TextItem}, this property 
    // allows you to specify the +link{DateItem.inputFormat, inputFormat} applied to the item.
    // @see formItem.dateFormatter
    // @visibility external
    //<

    //> @attr formItem.decimalPrecision (number : null : [IRW])
    // @include dataSourceField.decimalPrecision
    //
    // @group appearance
    // @serverDS allowed
    // @visibility external
    //<

    //> @attr formItem.decimalPad (number : null : [IRW])
    // @include dataSourceField.decimalPad
    //
    // @group appearance
    // @serverDS allowed
    // @visibility external
    //<

    //> @attr formItem.format (FormatString : null : IR)
    // +link{FormatString} for numeric or date formatting.  See +link{dataSourceField.format}.
    // @group exportFormatting
    // @visibility external
    //<

    //> @attr formItem.exportFormat (FormatString : null : IR)
    // +link{FormatString} used during exports for numeric or date formatting.  See
    // +link{dataSourceField.exportFormat}.
    // @group exportFormatting
    // @visibility external
    //<




    // ValueIcons
    // ---------------------------------------------------------------------------------------
    //> @attr formItem.valueIcons   (Object : null : IRW)
    // A mapping of logical form item values to URLs.
    // If specified, when the form item is set to the value in question, an icon will be 
    // displayed with the appropriate source URL.
    // @group   valueIcons
    // @setter  setValueIcons()
    // @see     formItem.getValueIcon()
    // @visibility external
    //<

    //> @attr formItem.emptyValueIcon (string : null : IRW)
    // This property allows the developer to specify an icon to display when this item has
    // no value. It is configured in the same way as any other valueIcon 
    // (see +link{formItem.valueIcons})
    // @group valueIcons
    // @visibility external
    //<
    
    //> @attr formItem.showValueIconOnly (boolean : null : IRWA) 
    // If +link{FormItem.valueIcons} is set, this property may be set to show the valueIcon
    // only and prevent the standard form item element or text from displaying
    // @group valueIcons
    // @visibility external
    //<
    //> @attr formItem.suppressValueIcon (boolean : null : IRWA)
    // If +link{FormItem.valueIcons} is set, this property may be set to prevent the value
    // icons from showing up next to the form items value
    // @group valueIcons
    // @visibility external
    //<
    
    //> @attr formItem.valueIconWidth (number : null : IRW)
    // If +link{formItem.valueIcons} is specified, use this property to specify a width for
    // the value icon written out.
    // @see FormItem.valueIconHeight
    // @see FormItem.valueIconSize
    // @group valueIcons
    // @visibility external
    //<
    
    //> @attr formItem.valueIconHeight (number : null : IRW)
    // If +link{formItem.valueIcons} is specified, use this property to specify a height for the
    // value icon written out.
    // @see FormItem.valueIconWidth
    // @see FormItem.valueIconSize
    // @group valueIcons
    // @visibility external
    //<
    
    //> @attr formItem.valueIconSize (number : 16 : IRW)
    // If +link{formItem.valueIcons} is specified, this property may be used to specify both
    // the width and height of the icon written out.
    // Note that +link{FormItem.valueIconWidth} and +link{formItem.valueIconHeight} take
    // precedence over this value, if specified.
    // @see FormItem.valueIconWidth
    // @see FormItem.valueIconHeight
    // @group valueIcons
    // @visibility external
    //<
    valueIconSize:16,

    //> @attr formItem.valueIconLeftPadding (number : 0 :  IRW)
    // If we're showing a value icon, this attribute governs the amount of space between the 
    // icon and the start edge of the form item cell.
    // <p>
    // <b>NOTE:</b> In RTL mode, the valueIconLeftPadding is applied to the <em>right</em> of
    // the value icon.
    // @see FormItem.valueIcons
    // @visibility external
    // @group valueIcons
    //<
    valueIconLeftPadding:0,

    //> @attr formItem.valueIconRightPadding (number : 3 :  IRW)
    // If we're showing a value icon, this attribute governs the amount of space between the 
    // icon and the value text.
    // <p>
    // <b>NOTE:</b> In RTL mode, the valueIconRightPadding is applied to the <em>left</em> of
    // the value icon.
    // @see FormItem.valueIcons
    // @visibility external
    // @group valueIcons
    //<
    valueIconRightPadding:3,

    //> @attr formItem.imageURLPrefix (string : null : IRWA)
    // Prefix to apply to the beginning of any +link{FormItem.valueIcons} when determining the
    // URL for the image.
    // Will not be applied if the <code>valueIcon</code> URL is absolute.
    // @group valueIcons
    // @visibility external
    //<

    //> @attr formItem.imageURLSuffix (string : null : IRWA)
    // Suffix to apply to the end of any +link{FormItem.valueIcons} when determining the URL for
    // the image. A common usage would be to specify a suffix of <code>".gif"</code> in which
    // case the <code>valueIcons</code> property would map values to the names of images without
    // the <code>".gif"</code> extension.
    // @group valueIcons
    // @visibility external
    //<
            
    // Internal
    // ---------------------------------------------------------------------------------------

    //> @attr formItem.form     (DynamicForm : null : R)
    // A Read-Only pointer to this formItem's DynamicForm widget.
    // @visibility external
    //<
    // Handles values for the form item.  Also handles writing the item's HTML by default.
    
    //> @attr formItem.containerWidget  (Canvas : null : RA)
    // A Read-Only pointer to the SmartClient canvas that holds this form item. In most cases this
    // will be the +link{formItem.form,DynamicForm} containing the item but in some cases
    // editable components handle writing out form items directly. An example of this
    // is +link{group:editing,Grid Editing} - when a listGrid shows per-field editors, the
    // <code>containerWidget</code> for each item will be the listGrid body.
    // <P>
    // Note that even if the <code>containerWidget</code> is not a DynamicForm, a DynamicForm
    // will still exist for the item (available as +link{formItem.form}), allowing access
    // to standard APIs such as +link{dynamicForm.getValues()}
    // @visibility external
    //<
    

    // RelationItem
    // ---------------------------------------------------------------------------------------
    
    //> @attr formItem.dataSource (DataSource or String : null : [IRWA])
    //
    // If this FormItem represents a foreignKey relationship into the dataSource of the form
    // containing this item, specify it here.
    //
    //  @visibility experimental
    //<


    // Picker Icon
    // -----------------------------------------------------------------------------------------

    //> @attr formItem.showPickerIcon (Boolean : null : IRW)
    // Should we show a special 'picker' +link{FormItemIcon,icon} for this form item? Picker
    // icons are customizable via +link{formItem.pickerIconProperties,pickerIconProperties}. By default
    // they will be rendered inside the form item's "control box" area, and will call
    // +link{FormItem.showPicker()} when clicked.
    // @group pickerIcon
    // @visibility external
    //<

    //> @attr formItem.showFocusedPickerIcon (Boolean : false : [IRW])
    // If +link{FormItem.showPickerIcon} is true for this item, should the picker icon show
    // a focused image when the form item has focus?
    // @group pickerIcon
    // @visibility external
    //<
    showFocusedPickerIcon:false,

    // We draw the icon into an exactly sized table cell - don't draw with any margin
    
    pickerIconHSpace:0,

    //> @attr formItem.pickerIconDefaults (FormItemIcon Properties : : IRWA)
    // Block of default properties to apply to the pickerIcon for this widget.
    // Intended for class-level customization: To modify this value we recommend using
    // +link{Class.changeDefaults()} rather than directly assigning a value to the property.
    // @group pickerIcon
    // @visibility external
    //<
    pickerIconDefaults: {
        click : function (form, item, icon) {
            item.showPicker();
        }
    },

    //> @attr formItem.pickerIconProperties (FormItemIcon Properties : null : IRWA)
    // If +link{showPickerIcon,showPickerIcon} is true for this item, this block of properties will
    // be applied to the pickerIcon. Allows for advanced customization of this icon.
    // @group pickerIcon
    // @visibility external
    //<

    //> @attr formItem.pickerIconName (identifier : "picker" : IRA)
    // If +link{showPickerIcon,showPickerIcon} is true, this attribute specifies the
    // +link{formItemIcon.name} applied to the picker icon
    // @group pickerIcon
    // @visibility external
    //<
    pickerIconName:"picker",

    //> @attr formItem.pickerIconSrc (SCImgURL : "" : IRWA)
    // If +link{showPickerIcon,showPickerIcon} is true for this item, this property governs the
    // +link{FormItemIcon.src,src} of the picker icon image to be displayed.
    // @group pickerIcon
    // @visibility external
    //<
    pickerIconSrc:"",

    //> @attr formItem.pickerIconWidth (int : null : IRWA)
    // If +link{showPickerIcon,showPickerIcon} is true for this item, this property governs the
    // size of the picker icon. If unset, the picker icon will be sized as a square to fit in the
    // available height for the icon.
    // <p>
    // It is not recommended to change the pickerIconWidth from the default value if +link{group:skinning,spriting}
    // is enabled because the image sprites are set up assuming specific, fixed dimensions of the picker
    // icon. If the pickerIconWidth must be changed, then the +link{formItem.pickerIconStyle,pickerIconStyle}
    // should be changed to a custom CSS style name.
    // @group pickerIcon
    // @visibility external
    //<

    //> @attr formItem.pickerIconHeight (int : null : IRWA)
    // If +link{showPickerIcon,showPickerIcon} is true for this item, this property governs the
    // size of the picker icon. If unset, the picker icon will be sized as a square to fit in the
    // available height for the icon.
    // <p>
    // It is not recommended to change the pickerIconHeight from the default value if +link{group:skinning,spriting}
    // is enabled because the image sprites are set up assuming specific, fixed dimensions of the picker
    // icon. If the pickerIconHeight must be changed, then the +link{formItem.pickerIconStyle,pickerIconStyle}
    // should be changed to a custom CSS style name.
    // @group pickerIcon
    // @visibility external
    //<

    //> @attr formItem.pickerIconPrompt (HTMLString : null : IR)
    // Prompt to show when the user hovers the mouse over the picker icon.
    // @group pickerIcon
    // @group i18nMessages
    // @visibility external
    //<

    // Picker Widget (pop-up launched by picker icon)
    // -----------------------------------------------------------------------------------------

    //> @attr formItem.picker (AutoChild Canvas : null : [IRW])
    // The component that will be displayed when +link{showPicker()} is called due to a click
    // on the +link{showPickerIcon,picker icon}.
    // <P>
    // Can be specified directly as a Canvas, or created automatically via the
    // +link{type:AutoChild} pattern.
    // <P>
    // Note that the picker is not automatically destroyed with the FormItem that uses it, in
    // order to allow recycling of picker components.  To destroy a single-use picker, override
    // +link{Canvas.destroy()}.
    //
    // @visibility external
    //<

    //> @attr formItem.pickerConstructor (SCClassName : null : [IRW])
    // Class name of the picker to be created.
    //
    // @visibility external
    //<
    
    //> @attr formItem.pickerProperties (Canvas Properties : {} : [IRW])
    // Default properties for the picker.
    //
    // @visibility external
    //<
    

    // Validation
    // -----------------------------------------------------------------------------------------

    //> @attr formItem.validators     (Array of Validator : null : IR)
    // Validators for this form item.  
    // <P>
    // <b>Note:</b> these validators will only be run on the client; to
    // do real client-server validation, validators must be specified on the DataSource.
    // @visibility external
    //<

    //> @attr formItem.required       (boolean : null : [IR])
    // Whether a non-empty value is required for this field to pass validation.
    // <P>
    // If the user does not fill in the required field, the error message to be shown will
    // be taken from these properties in the following order: +link{FormItem.requiredMessage},
    // +link{DynamicForm.requiredMessage}, +link{DataSource.requiredMessage}, 
    // +link{Validator.requiredField}.
    // <P>
    // <b>Note:</b> if specified on a FormItem, <code>required</code> is only enforced on the
    // client.  <code>required</code> should generally be specified on a
    // +link{class:DataSourceField}.
    //
    // @group validation
    // @visibility external
    // @example formShowAndHide
    //<

    //> @attr   formItem.requiredMessage     (string : null : [IRW])
    // The required message for required field errors.
    // @group validation
    // @visibility external
    //<
    
    // Status
    // -----------------------------------------------------------------------------------------
    
    //> @attr formItem.visible (Boolean : true : IRW)
    // Whether this item is currently visible.
    // <P>
    // <code>visible</code> can only be set on creation.  After creation, use
    // +link{formItem.show()} and +link{formItem.hide()} to manipulate visibility.
    //
    // @group appearance
    // @visibility external
    //<
    visible:true,

    //>	@attr	formItem.alwaysTakeSpace    (boolean : false : IRW)
    // If this form item is not visible, should they form it is contained in re-layout to
    // take advantage of the additional space, or should it continue to flow as if the
    // item were visible.  Set to true to have the form continue to flow around the item
    // even if it's not written out.
    //
    // @group appearance
    // @visibility internal
    //<

    //> @attr formItem.canEdit  (boolean : null : IRW)
    // Is this form item editable (canEdit:true) or read-only (canEdit:false)? Setting the
    // form item to non-editable causes it to render as read-only. Can be updated at runtime via
    // the +link{formItem.setCanEdit(),setCanEdit()} method.
    // <P>
    // Read-only appearance may be specified via +link{formItem.readOnlyDisplay}.
    // The default setting for this value (<code>"readOnly"</code>) differs from
    // the disabled state in that the form item is not rendered with disabled styling and
    // most form items will allow copying of the contents while read-only but do not while
    // disabled.
    // <P>
    // Note that for forms bound to a +link{DataSource}, if this property is not explicitly
    // set at the item level, its default value will match the
    // +link{DynamicForm.canEditFieldAttribute} on the associated dataSource field.
    // <P>
    // Developers should also be aware that the +link{readOnlyDisplay} attribute is
    // unrelated to the +link{dataSourceField.readOnlyEditorType} attribute. When a
    // DynamicForm is first bound to a dataSource, for
    // +link{dataSourceField.canEdit,canEdit:false} DataSourceFields,
    // +link{dataSourceField.readOnlyEditorType} will determine what +link{FormItemType}
    // should be created for the field. Once created, a FormItem's type can not be changed.
    // Setting +link{formItem.canEdit} at runtime will simply change the appearance
    // of the item to allow or disallow editing of the item.
    //
    // @setter setCanEdit()
    // @group readOnly
    // @see FormItem.setCanEdit()
    // @see DynamicForm.setCanEdit()
    // @visibility external
    //<

    //>	@attr	formItem.disabled		(Boolean : false : IRW)
    // Whether this item is disabled.  Can be updated at runtime via the <code>setDisabled()</code>
    // method.  Note that if the widget containing this formItem is disabled, the formItem will
    // behave in a disabled manner regardless of the setting of the item.disabled property.
    // <p>
    // Note that not all items can be disabled, and not all browsers show an obvious disabled style
    // for native form elements.
    // @group appearance
    // @setter setDisabled()
    // @see FormItem.setDisabled()
    // @visibility external
    // @example fieldEnableDisable
    //<

    //> @attr formItem.disableIconsOnReadOnly (Boolean : true : IRW)
    // If +link{formItem.canEdit} is set to false, should +link{formItem.icons,icons} be disabled
    // by default?
    // <P>
    // This may also be specified at the icon level. See +link{formItemIcon.disableOnReadOnly}.
    // @group formIcons
    // @visibility external
    //<
    disableIconsOnReadOnly:true,

    // Keyboard handling
    // -----------------------------------------------------------------------------------------
    
    //>@attr formItem.canFocus  (boolean : null : IRA)
    // Is this form item focusable? Setting this property to true on an otherwise
    // non-focusable element such as a +link{StaticTextItem} will cause the item to be
    // included in the page's tab order and respond to keyboard events.
    // @group focus 
    // @visibility external
    //<
    // If not set focusability is determined by whether this item has a data element by default.
    // If set to true, and this item has no data element we write out HTML to allow focus
    // on the item's text-box.
    
    //> @attr formItem.accessKey  (keyChar : null : IRW)
    // If specified this governs the HTML accessKey for the item.
    // <P>
    // This should be set to a character - when a user hits the html accessKey modifier for
    // the browser, plus this character, focus will be given to the item.
    // The accessKey modifier can vary by browser and platform. 
    // <P>
    // The following list of default behavior is for reference only, developers should also
    // consult browser documentation for additional information.
    // <ul>
    // <li><b>Internet Explorer (all platforms)</b>: <code>Alt</code> + <i>accessKey</i></li>
    // <li><b>Mozilla Firefox (Windows, Unix)</b>: <code>Alt+Shift</code> + <i>accessKey</i></li>
    // <li><b>Mozilla Firefox (Mac)</b>: <code>Ctrl+Opt</code> + <i>accessKey</i></li>
    // <li><b>Chrome and Safari (Windows, Unix)</b>:  <code>Alt</code> + <i>accessKey</i></li>
    // <li><b>Chrome and Safari (Mac)</b>:  <code>Ctrl+Opt</code> + <i>accessKey</i></li>
    // </ul>
    //
    // @group focus
    // @visibility external
    //<
    
    accessKey:null,                                        
    
    //> @attr formItem.tabIndex (integer : null : IRW)
    // TabIndex for the form item within the form, which controls the order in which controls
    // are visited when the user hits the tab or shift-tab keys to navigate between items. 
    // <P>
    // tabIndex is automatically assigned as the order that items appear in the
    // +link{dynamicForm.items} list.
    // <P>
    // To specify the tabindex of an item within the page as a whole (not just this form), use
    // +link{globalTabIndex} instead.
    //
    //  @visibility external
    //  @group focus
    //<                          
    //tabIndex:null,

    //> @attr formItem.globalTabIndex (integer : null : IRWA)
    // TabIndex for the form item within the page. Takes precedence over any local tab index
    // specified as +link{tabIndex,item.tabIndex}.
    // <P>
    // Use of this API is <b>extremely</b> advanced and essentially implies taking over
    // management of tab index assignment for all components on the page.
    //
    // @group focus
    // @visibility external
    //<                          
    //globalTabIndex:null,
    
    //> @attr   formItem.selectOnFocus  (boolean : null : IRW)
    // Allows the +link{dynamicForm.selectOnFocus,selectOnFocus} behavior to be configured on a
    // per-FormItem basis.  Normally all items in a form default to the value of
    // +link{dynamicForm.selectOnFocus}.
    //
    // @group focus
    // @visibility external
    //<
    // Exposed on TextItem and TextAreaItem directly since it won't apply to other items.

    //> @attr   formItem.selectOnClick  (boolean : null : IRW)
    // Allows the +link{dynamicForm.selectOnClick,selectOnClick} behavior to be configured on a
    // per-FormItem basis.  Normally all items in a form default to the value of
    // +link{dynamicForm.selectOnClick}.
    //
    // @group focus
    // @visibility external
    //<
    // Exposed on TextItem and TextAreaItem directly since it won't apply to other items.
    
    
    //> @attr formItem.changeOnKeypress (Boolean : true : IRW)
    // Should this form item fire its +link{formItem.change(),change} handler (and store its
    // value in the form) on every keypress? Set to <code>false</code> to suppress the 'change'
    // handler firing (and the value stored) on every keypress.
    // <p>
    // Note: If <code>false</code>, the value returned by +link{formItem.getValue(),getValue}
    // will not reflect the value displayed in the form item element as long as focus is in
    // the form item element.
    // 
    // @group  eventHandling, values
	// @visibility external
    //<    
    changeOnKeypress:true,
    
    // maintainSelectionOnTransform
    // Internal, but non obfuscated property.
    // Ensure selection is maintained if:
    // - the value is reverted due to a change handler returning false during typing
    // - the value is modified during typing, but either the value is unchanged except for
    //   case shifting, or the entire value was selected before typing.
    // If this causes a performance hit in any cases, we can disable it.
    maintainSelectionOnTransform:true,
    
    
    //> @attr   formItem.dirtyOnKeyDown (boolean : true : RW)
    //  Should this form item get marked as dirty on every keyDown?
    //  @group  eventHandling, values
    //<    
    dirtyOnKeyDown:true, 

    // Titles
    // --------------------------------------------------------------------------------------------
    //> @attr formItem.showTitle (Boolean : true : IRW)
    // Should we show a title cell for this formItem?
    // <p>
    // Note: the default value of this attribute is overridden by some subclasses.
    //
    // @group title
    // @visibility external
    //<
    showTitle:true,
    
    //> @attr formItem.titleOrientation (TitleOrientation : isc.Canvas.LEFT : IRW)
    // On which side of this item should the title be placed.  +link{type:TitleOrientation}
    // lists valid options.
    // <P>
    // Note that titles on the left or right take up a cell in tabular
    // +link{group:formLayout,form layouts}, but titles on top do not.
    // 
    // @group title
    // @see DynamicForm.titleOrientation
    // @visibility external
    //<
// titleOrientation:isc.Canvas.LEFT, // dynamically defaulted in DynamicForm

    //> @attr formItem.titleAlign (Alignment : null : IRW)
    // Alignment of this item's title in its cell.
    // <p>
    // If null, dynamically set according to text direction.
    // @group title
    // @visibility external
    //<

    //> @attr formItem.titleVAlign (VerticalAlignment : isc.Canvas.CENTER : IRW)
    // Vertical alignment of this item's title in its cell. Only applies when
    // +link{formItem.titleOrientation} is <code>"left"</code> or <code>"right"</code>.
    // @group title
    // @visibility external
    //<
//    titleVAlign:isc.Canvas.CENTER, // dynamically defaulted in getTitleVAlign

    //> @attr formItem.clipTitle (boolean : null : [IRW])
    // If the title for this form item is showing, and is too large for the available space
    // should the title be clipped?
    // <p>
    // Null by default - if set to true or false, overrides +link{DynamicForm.clipItemTitles}.
    // @group title
    // @visibility external
    //<
    clipTitle:null,

    //> @attr formItem.wrapTitle (boolean : null : [IRW])
    // If specified determines whether this items title should wrap.
    // Overrides +link{DynamicForm.wrapItemTitles,wrapItemTitles} at the DynamicForm level.
    // @group title
    // @visibility external
    //<
//    wrapTitle:null,

    // Change handling
    // --------------------------------------------------------------------------------------------

    //> @attr formItem.redrawOnChange (Boolean : false : IRW)
    // If true, this item will cause the entire form to be redrawn
    // when the item's "elementChanged" event is done firing
    // @group changeHandling
    // @visibility external
    //<

    //> @attr formItem.validateOnChange (Boolean : false : IRW)
    // If true, form items will be validated when each item's "change" handler is fired
    // as well as when the entire form is submitted or validated.
    // <p>
    // Note that this property can also be set at the form level or on each validator;
    // If true at the form or field level, validators not explicitly set with
    // <code>validateOnChange:false</code> will be fired on change - displaying errors and
    // rejecting the change on validation failure.
    // @group changeHandling
    // @visibility external
    // @see DynamicForm.validateOnChange
    //<
    
    
    //> @attr formItem.validateOnExit (Boolean : false : IRW)
    // If true, form items will be validated when each item's "editorExit" handler is fired
    // as well as when the entire form is submitted or validated.
    // <p>
    // Note that this property can also be set at the form level.
    // If true at either level the validator will be fired on editorExit.
    // @visibility external
    // @see dynamicForm.validateOnExit
    //<

    //> @attr formItem.stopOnError (boolean : null : IR)
    // Indicates that if validation fails, the user should not be allowed to exit
    // the field - focus will be forced back into the field until the error is corrected.
    // <p>
    // This property defaults to +link{DynamicForm.stopOnError} if unset.
    // <p>
    // Enabling this property also implies +link{FormItem.validateOnExit} is automatically
    // enabled. If there are server-based validators on this item, setting this property
    // also implies that +link{FormItem.synchronousValidation} is forced on.
    // 
    // @visibility external
    //<

    //> @attr formItem.rejectInvalidValueOnChange (Boolean : false : IRWA)
    // If validateOnChange is true, and validation fails for this item on change, with no suggested
    // value, should we revert to the previous value, or continue to display the bad value entered
    // by the user. May be set at the item or form level.
    // @visibility external
    //<
    // Introduced for back-compat: pre 7.0beta2 this was the default behavior, so enable this flag
    // at the item or form level if required for backcompat.
    //rejectInvalidValueOnChange:null,
    

    //>	@attr formItem.changeOnBlur (boolean : false : IRWA)
    // If true, this item will fire it's elementChanged message on BLUR in a field (eg: when the
    // user tabs through without making changes as well as when they actually change something), if
    // false, the message will only fire when the field is actually changed.  This is useful for
    // fields that do validation/value setting on change (such as the TimeItem), to work around a
    // bug in IE where the change event doesn't always fire correctly when you manually set the
    // value of the field in its ONCHANGE handler.  Note that it is not recommended that you set
    // both changeOnBlur==true AND redrawOnChange==true, as this will cause the form to redraw every
    // time you tab through that item.
    // @group changeHandling
    //<
    

    //> @attr  formItem.synchronousValidation (boolean : null : IR)
    // If enabled, whenever validation is triggered and a request to the server is required,
    // user interactivity will be blocked until the request returns. Can be set for the entire
    // form or individual FormItems.
    // <p>
    // If false, the form will try to avoid blocking user interaction until it is strictly
    // required. That is until the user attempts to use a FormItem whose state could be
    // affected by a server request that has not yet returned.
    //
    // @visibility external
    //<

    // Size and Layout
    // --------------------------------------------------------------------------------------------
    //> @attr formItem.width (int | String : "*" : IRW)
    // Width of the FormItem.  Can be either a number indicating a fixed width in pixels, or
    // "*" indicating the FormItem fills the space allocated to it's column (or columns, for a
    // +link{colSpan,column spanning} item).
    // <P>
    // <smartgwt>If width is specified as a String, getWidth() will return -1.  Use 
    // +sgwtLink{getWidthAsString()} in this case.<p></smartgwt>
    // See the +link{group:formLayout} overview for details.
    //
    // @group formLayout
    // @visibility external
    // @example columnSpanning
    //<
    width:"*",

    //> @attr formItem.height (int | String : 20 : IRW)
    // Height of the FormItem.  Can be either a number indicating a fixed height in pixels, a
    // percentage indicating a percentage of the overall form's height, or "*" indicating take
    // whatever remaining space is available. See the +link{group:formLayout} overview for details.
    // <p>
    // <smartgwt>If height is specified as a String, getHeight() will return -1.  Use 
    // +sgwtLink{getHeightAsString()} in this case.<p></smartgwt>    
    // For form items having a +link{showPickerIcon,picker icon} (e.g. +link{SelectItem}, +link{ComboBoxItem})
    // and +link{SpinnerItem}s, if +link{group:skinning,spriting} is enabled, it is not recommended
    // to change the height of the form item from the default because the image sprites are set up
    // assuming a specific, fixed height of the item. If the item height must be changed, then
    // the +link{pickerIconStyle,pickerIconStyle} should be changed to a custom CSS style name.
    // Or, in the case of +link{SpinnerItem}s, the +link{FormItemIcon.baseStyle,baseStyle} and
    // +link{FormItemIcon.src,src} of the +link{SpinnerItem.increaseIcon} and +link{SpinnerItem.decreaseIcon}
    // AutoChildren should be customized.
    //
    // @group formLayout
    // @visibility external
    // @example formLayoutFilling
    //<
    
    height:20,

    //> @attr formItem.cellHeight (number : null : IRW)
    // If specified, this property will govern the height of the cell in which this form
    // item is rendered.
    // Will not apply when the containing DynamicForm sets <code>itemLayout:"absolute"</code>.
    // @visibility external
    //<


    //> @attr formItem.titleColSpan  (number : 1 : IRW)
    // Number of columns that this item's title spans.  
    // <P>
    // This setting only applies for items that are showing a title and whose
    // +link{titleOrientation} is either "left" or "right".
    //
    // @group formLayout
    // @visibility external
    //<
    titleColSpan:1,

    //> @attr formItem.colSpan (number : 1 : IRW)
    // Number of columns that this item spans.  
    // <P>
    // The <code>colSpan</code> setting does not include the title shown for items with
    // +link{showTitle}:true, so the effective <code>colSpan</code> is one higher than this
    // setting for items that are showing a title and whose +link{titleOrientation} is either
    // "left" or "right".
    //
    // @group formLayout
    // @visibility external
    //<
    colSpan:1,

    //> @attr formItem.rowSpan (number : 1 : IRW)
    // Number of rows that this item spans
    // @group formLayout
    // @visibility external
    //<
    rowSpan:1,

    //> @attr formItem.startRow (Boolean : false : IRW)
    // Whether this item should always start a new row in the form layout.
    // @group formLayout
    // @visibility external
    //<

    //> @attr formItem.endRow (Boolean : false : IRW)
    // Whether this item should end the row it's in in the form layout
    // @group formLayout
    // @visibility external
    //<
    
    // The align property is used by the dynamic form to align the item as a whole (control table
    // and all) in its cell.
    // In addition to this we support textAlign to align the contents within the text-box
     
    
    //> @attr formItem.align (Alignment : isc.Canvas.LEFT : IRW)
    // Alignment of this item in its cell. Note that the alignment of content within this item
    // can be separately controlled via +link{formItem.textAlign} (typically only applies to items
    // showing a "textBox", such as +link{textItem,textItems} or +link{selectItem,selectItems}).
    // @group appearance
    // @visibility external
    //<

    //> @attr formItem.vAlign (VerticalAlignment : isc.Canvas.CENTER : IRW)
    // Vertical alignment of this item within its cell. This property governs the position
    // of the item's text box within the cell (not the content within the text box).
    // If +link{shouldApplyHeightToTextBox()} is true, for this to have a visible effect,
    // the cell height must exceed the specified height of the item, either due to
    // an explicit +link{cellHeight} specification, or due to the row being expanded
    // by another taller item.
    // <P>
    // Has no effect if +link{dynamicForm.itemLayout} is set to <code>"absolute"</code> for the
    // form.
    //
    // @group title
    // @visibility external
    //<

    //> @attr   formItem.textAlign  (Alignment : isc.Canvas.LEFT : IRW)
    // Alignment of the text / content within this form item. Note that +link{formItem.align} may
    // be used to control alignment of the entire form item within its cell. May not apply to all
    // form item types.
    // @group appearance
    // @visibility external
    //<

    //> @attr formItem.left (int : 0 : IRWA)
    // Left coordinate of this item in pixels.  Applies only when the containing DynamicForm
    // sets <code>itemLayout:"absolute"</code>.
    // @visibility absForm
    //<

    //> @attr formItem.top (int : 0 : IRWA)
    // Top coordinate of this item in pixels.  Applies only when the containing DynamicForm
    // sets <code>itemLayout:"absolute"</code>.
    // @visibility absForm
    //<

    //> @attr formItem.wrap (boolean : null : IRW)
    // If true, item contents can wrap. If false, all the contents should appear on a single line.
    // @visibility internal
    //<

    //> @attr formItem.clipValue  (boolean : null : IRW)
    // If true, text that exceeds the specified size of the form item will be clipped.
    // @visibility internal
    //<
    


    // AutoComplete
    // --------------------------------------------------------------------------------------------
    //> @attr formItem.autoComplete   (AutoComplete : null : IRW)
    // Whether to do inline autoComplete.
    // <p>
    // If unset, defaults to form.autoComplete
    //
    // @see dynamicForm.autoComplete
    // @visibility autoComplete
    //<

    //> @attr formItem.uniqueMatch    (boolean : null : IRW)
    // When autoComplete is enabled, whether to offer only unique matches to the user.
    // <p>
    // If unset, defaults to form.uniqueMatch.
    //
    // @see dynamicForm.uniqueMatch
    // @visibility autoComplete
    //<

    //> @attr formItem.autoCompleteCandidates (Array : null : IRW)
    // Optional set of candidate completions.
    // <p>
    // If candidates are not specified, the values in the valueMap, if any, will be used, or
    // within a DataBound form, the other values currently present in the loaded portion of the
    // dataset.
    // @visibility autoComplete
    //<

    
    //>@attr    formItem.browserSpellCheck  (boolean : null : IRWA)
    // If this browser supports spell-checking of text editing elements, do we want this
    // enabled for this item? If unset the property will be inherited from the containing form.
    // <P>
    // Notes:<br>
    // - this property only applies to text based items such as TextItem and TextAreaItem.<br>
    // - this property is not supported on all browsers.
    // @see dynamicForm.browserSpellCheck
    // @visibility external
    //<
    
    // In addition to spell-check Safari on iPhone / iPad supports the following features on
    // text items:
    // - auto capitalizing 
    //>@attr formItem.browserAutoCapitalize (boolean : null : IRWA)
    // @visibility internal
    //<
    
    // - auto correct
    //>@attr formItem.browserAutoCorrect (boolean : null : IRWA)
    // @visibility internal
    //<

    // - which keyboard to display (text, phone, url, email, zip)
    //>@attr formItem.browserInputType (String : null : IRA)
    // Form item input type - governs which keyboard should be displayed for
    // mobile devices (supported on iPhone / iPad)
    // @visibility internal
    //<
    browserInputTypeMap:{
        "digits": "number",

        // likely synonym
        "phone": "tel"
    },
    
    // Icons
    // --------------------------------------------------------------------------------------------
    
    //>@attr    formItem.icons      (Array of FormItemIcon Properties : null  : IRW)
    //  An array of descriptor objects for icons to display in a line after this form item.
    //  These icons are clickable images, often used to display some kind of helper for 
    //  populating a form item.
    //  @group formIcons
    //  @see    class:FormItemIcon
    //  @visibility external
    //  @example formIcons
    //<

    //> @attr   formItem.defaultIconSrc      (SCImgURL : "[SKIN]/DynamicForm/default_formItem_icon.gif" : IRWA)
    // Default icon image source.     
    // Specify as the partial URL to an image, relative to the imgDir of this component.
    // To specify image source for a specific icon use the <code>icon.src</code> property.<br>
    // If this item is drawn in the disabled state, the url will be modified by adding 
    // "_Disabled" to get a disabled state image for the icon.
    // If <code>icon.showOver</code> is true, this url will be modified by adding "_Over" to get
    // an over state image for the icon.
    //  @group  formIcons
    //  @visibility external    
    //<
    defaultIconSrc:"[SKIN]/DynamicForm/default_formItem_icon.gif",

    //> @attr   formItem.showOverIcons  (boolean : null : IRWA)
    //  If we're showing icons, should we change their image source to the appropriate <i>over</i>
    //  source when the user rolls over (or puts focus onto) them?  Can be overridden on a per
    //  icon basis by the formItemIcon <code>showOver</code> property.
    //  @group formIcons
    //  @visibility external
    //<
    
    //> @attr   formItem.showFocusedIcons (boolean : null : IRWA)
    //  If we're showing icons, should we change their image source to the appropriate
    //  <i>focused</i>  source when this item has focus?  Can be overridden on a per
    //  icon basis by the formItemIcon <code>showFocused</code> property.
    //  @group formIcons
    //  @visibility external
    //<

    //> @attr   formItem.iconHSpace (integer : 3 : IR)
    // Horizontal space (in px) to leave between form item icons. The space
    // appears to the left of each icon. May be overridden at the icon level via
    // +link{formItemIcon.hspace}.
    //  @group  formIcons
    // @visibility external
    //<
    iconHSpace:3,         
                                   
    //> @attr   formItem.iconVAlign (VerticalAlignment: "bottom" : [IRWA])
    //      How should icons be aligned vertically for this form item.
    //  @group  formIcons
    //  @visibility external
    //<
    // Options are "top", "center", "bottom" - Implemented via the css 'vertical-alignment'
    // property
    iconVAlign:isc.Canvas.BOTTOM,
      
    //> @attr   formItem.iconHeight (number : 20 : IRWA)
    //      Default height for form item icons
    //  @group  formIcons
    //  @visibility external    
    //<
    iconHeight:20,

    //> @attr   formItem.iconWidth (number : 20 : IRWA)
    //      Default width for form item icons
    //  @visibility external    
    //  @group  formIcons
    //<
    iconWidth:20,

    //> @attr formItem.prompt (string : null : IRW)
    //
    // This text is shown as a tooltip prompt when the cursor hovers over this item.
    //
    // @group basics
    // @visibility external
    //<
    
    // FormItem.implementsPromptNatively:
    // By default we show prompts in an ISC hover. However for some form items we'll write
    // out HTML that will cause a native hover prompt to show up instead. In these cases
    // we suppress the ISC hover
    
    //implementsPromptNatively:false,

    //> @attr formItem.iconPrompt (string : "" : IRWA)
    // Default prompt (and tooltip-text) for icons.
    // @group formIcons
    // @visibility external
    //<
    
    iconPrompt:"",

    //> @attr   formItem.showIcons (Boolean : true : IRWA)
    // Set to false to suppress writing out any +link{formItem.icons} for this item.
    //  @group  formIcons
    //  @visibility external
    //<
    
    showIcons:true,
    
    

    //> @attr   formItem.redrawOnShowIcon (boolean : true : IRWA)
    //      When dynamically showing/hiding icons for this form  item, should we force a
    //      redraw of the entire form?
    //  @group  formIcons
    //<
    
    redrawOnShowIcon:false,
    
    //> @attr   formItem.canTabToIcons  (boolean : null : IRWA)
    // If set, this property determines if this form item's icons should be included in 
    // the page's tab order, with the same tabIndex as this form item?
    // <P>
    // Note that if this form item has tabIndex -1, neither the form item nor the icons
    // will be included in the page's tab order.
    //
    // @group  formIcons
    // @visibility advancedIcons
    //<
    //canTabToIcons:null,
    
    // Validation Error Icon
    // We write out a special icon to indicate validation errors. This will not be part of the
    // icons array but will be rendered using some of the same code.

    //> @attr   formItem.errorIconHeight    (number : 16 : IRW)
    // Height of the error icon, if we're showing icons when validation errors occur.
    // @group  errorIcon
    // @see FormItem.showErrorIcon
    // @visibility external
    //<
    errorIconHeight: 16,
    
    //> @attr   formItem.errorIconWidth    (number : 16 : IRW)
    // Height of the error icon, if we're showing icons when validation errors occur.
    // @group  errorIcon
    // @see FormItem.showErrorIcon
    // @visibility external
    //<    
    errorIconWidth: 16,
    
    //> @attr   formItem.errorIconSrc    (SCImgURL : "[SKIN]/DynamicForm/validation_error_icon.png" : IRW)
    // URL of the image to show as an error icon, if we're showing icons when validation
    // errors occur.
    // @group  errorIcon
    // @see FormItem.showErrorIcon
    // @visibility external
    //<    
    errorIconSrc: "[SKIN]/DynamicForm/validation_error_icon.png",    
    
    //> @attr   formItem.showErrorIcon (boolean : null : IRW)
    // @include dynamicForm.showErrorIcons
    //  @group  errorIcon, validation, appearance
    //  @visibility external
    //<    
    // Note Actually writing this item (and the error text) into the DOM is handled by the
    // Form (or containerWidget) - but this property governs whether we include the icon in the
    // errorHTML we return.
    //showErrorIcon: null,
    
    //> @attr   formItem.showErrorText (boolean : null : IRW)
    // @include dynamicForm.showErrorIcons
    // @group  validation, appearance
    // @visibility external
    //<    
    //showErrorText: null,
    
    //> @attr formItem.showErrorStyle     (boolean : null : IRW)
    // @include dynamicForm.showErrorIcons
    // @group validation, appearance
    // @visibility external
    // @see FormItem.cellStyle
    //<    
    //showErrorStyle: null,
    
    //> @attr formItem.errorOrientation (align : null : IRW)
    // If +link{dynamicForm.showInlineErrors} is true, where should the error icon and text appear
    // relative to the form item itself. Valid options are <code>"top"</code>, 
    // <code>"bottom"</code>, <code>"left"</code> or <code>"right"</code>.<br>
    // If unset the orientation will be derived from +link{dynamicForm.errorOrientation}. 
    // @group validation, appearance
    // @visibility external
    //<
    //errorOrientation: null,

    // Define a jsdoc pseudo-class for form item icon descriptor objects.
    // ------

        //> @object FormItemIcon
        //
        //  Form item icon descriptor objects used by Form Items to specify the appearance and
        //  behavior of icons displayed after the item in the page flow.
        //  
        //  @treeLocation   Client Reference/Forms/Form Items/FormItem
        //  @see attr:FormItem.icons
        //  @group formIcons
        //  @visibility external
        //  @example formIcons
        //<

        //> @attr formItemIcon.baseStyle (CSSStyleName : null : IRWA)
        // Base CSS style. If set, as the component changes state and/or is focused, suffixes
        // will be added to the base style. Possible suffixes include "Over" if the user mouses
        // over the icon and +link{FormItemIcon.showOver,this.showOver} is true, "Disabled" if
        // the icon is disabled, and "Focused". In addition, if +link{FormItemIcon.showRTL,showRTL}
        // is enabled, then an "RTL" suffix will be added.
        // @visibility external
        //<

        //> @attr formItemIcon.name (identifier : null : IR)
        // Identifier for this form item icon. This identifier (if set) should be unique
        // within this form item and may be used to get a pointer to the icon object
        // via +link{FormItem.getIcon()}.
        // @visibility external
        //<
        // Note: Also used by the AutoTest subsystem - if the name is autoGenerated, we can't
        // guarantee it won't change as the application changes (specifically the order of
        // icons for this form item changes).
        // For auto-generated icons, such as the picker we should always provide a reliable
        // standard name

        //> @attr formItemIcon.src (SCImgURL : null : IRW)
        // If set, this property determines this icon's image source.
        // If unset the form item's <code>defaultIconSrc</code> property will be used
        // instead.<br>
        // As with <code>defaultIconSrc</code> this URL will be modified by adding
        // "_Over" or "_Disabled" if appropriate to show the icon's over or disabled state.
        // <p>
        // If +link{FormItemIcon.showRTL,showRTL} is enabled, then "_rtl" will be added to the
        // source URL before the extension.
        // <p>
        // The special value "blank" means that no image will be shown for this icon. This
        // is particularly useful together with +link{FormItemIcon.baseStyle} to implement
        // spriting of the different icon states.
        //
        // @group formIcons
        // @see attr:formItem.defaultIconSrc
        // @example formIcons
        // @visibility external
        //<

        //> @attr formItemIcon.showOver (boolean : null : IRWA)
        // Should this icon's image and/or +link{FormItemIcon.baseStyle,baseStyle} switch to the
        // appropriate "Over" value when the user rolls over or focuses on the icon?
        // @group  formIcons
        // @visibility external
        // @see attr:formItem.showOverIcons
        //<

        //> @attr formItemIcon.showFocused (Boolean : null : IRWA)
        // Should this icon's image and/or +link{FormItemIcon.baseStyle,baseStyle} switch to the
        // appropriate "Focused" value when the user puts focus on the form item or icon?
        // @see formItem.showFocusedIcons
        // @see formItemIcon.showFocusedWithItem
        // @group  formIcons
        // @visibility external
        //<

        //> @attr formItemIcon.showRTL (Boolean : null : IRA)
        // Should this icon's +link{FormItemIcon.src,src} and/or +link{FormItemIcon.baseStyle,baseStyle}
        // switch to the appropriate RTL value when the FormItem is in RTL mode? If true, then
        // the image URL for all states will have "_rtl" added before the extension. Also, if
        // baseStyle is set, all style names will have an "RTL" suffix. This should only be
        // enabled if RTL media is available.
        // <p>
        // For example, if an icon's src is "[SKINIMG]formItemIcons/myFormIcon.png" and the baseStyle
        // is "myFormIcon", then in the "Down" state, SmartClient will use "[SKINIMG]formItemIcons/myFormIcon_Down_rtl.png"
        // for the image source and "myFormIconDownRTL" for the style name.
        // @group RTL
        // @group formIcons
        // @visibility external
        //<

        //> @attr formItemIcon.showFocusedWithItem (boolean : null : IRWA)
        // If this icon will be updated to show focus (see +link{formItemIcon.showFocused}, 
        // +link{formItem.showFocusedIcons}), this property governs whether the focused state should
        // be shown when the item as a whole receives focus or just if the icon receives focus.
        // If this property is unset, default behavior is to show focused state when the item
        // receives focus. 
        // @see formItem.showFocusedIcons
        // @see formItemIcon.showFocused
        // @group  formIcons
        // @visibility external
        //<
        
        //> @attr formItemIcon.neverDisable (boolean : null : IRWA)
        // If <code>icon.neverDisable</code> is true, when this form item is disabled, the 
        // icon will remain enabled. 
        // Note that disabling the entire form will disable all items, together with their 
        // icons including those marked as neverDisable - this property only has an effect 
        // if the form is enabled and a specific item is disabled within it.
        // <P>
        // If this property is true, the icons will also remain enabled if a form item 
        // is marked as +link{formItem.canEdit,canEdit:false}. For finer grained control over
        // whether icons are enabled for read-only icons see +link{formItem.disableIconsOnReadOnly}
        // and +link{formItemIcon.disableOnReadOnly}
        //
        // @group  formIcons
        // @visibility external
        //<
        
        //> @attr formItemIcon.disableOnReadOnly (boolean : null : IRWA)
        // If +link{formItem.canEdit} is set to false, should this icon be disabled.
        // If unset this is determined by +link{formItem.disableIconsOnReadOnly}.
        // Note that if +link{formItemIcon.neverDisable} is set to true, the icons will
        // be rendered enabled regardless of this setting and whether the item is editable.
        // @group formIcons
        // @visibility external
        //<
        
        //> @attr formItemIcon.tabIndex (int : null : IRA)
        // TabIndex for this formItemIcon.
        // <P>
        // Set to -1 to remove the icon from the tab order, but be cautious doing so: if the
        // icon triggers important application functionality that cannot otherwise be accessed
        // via the keyboard, it would be a violation of accessibility standard to remove the
        // icon from the tab order.
        // <P>
        // Any usage other than setting to -1 is extremely advanced in the same way as using
        // +link{formItem.globalTabIndex}.
        //
        // @group formIcons
        // @visibility external
        //<

        //> @method formItemIcon.click()
        //  Called when this icon is clicked. The default action is to call +link{FormItem.showPicker()}.
        //  @param  form (DynamicForm)  The Dynamic Form to which this icon's item belongs.
        //  @param  item (FormItem)     The Form Item containing this icon
        //  @param  icon (FormItemIcon) A pointer to the form item icon clicked
        //  @group  formIcons
        //  @visibility external
        //  @example formIcons
        //<    

        //> @method formItemIcon.keyPress()
        //      StringMethod action to fire when this icon has focus and receives a keypress
        //      event.
        //      If unset the form item's <code>iconKeyPress</code> method will be fired instead 
        //      (if specified).
        //  @param  keyName (string)    Name of the key pressed
        //  @param  character (character) character produced by the keypress
        //  @param  form (DynamicForm)  The Dynamic Form to which this icon's item belongs.
        //  @param  item (FormItem)     The Form Item containing this icon
        //  @param  icon (FormItemIcon) A pointer to the form item icon 
        //  @group  formIcons
        //  @visibility external
        //<
                
        //> @attr   formItemIcon.width (number : null : IRW)
        //      If set, this property determines the width of this icon in px.
        //      If unset the form item's <code>iconWidth</code> property will be used instead.
        //  @group  formIcons
        //  @visibility external
        //  @see    attr:formItem.iconWidth
        //<
        
        //> @attr   formItemIcon.height (number : null : IRW)
        //      If set, this property determines the height of this icon in px.
        //      If unset the form item's <code>iconHeight</code> property will be used instead.
        //  @group  formIcons
        //  @visibility external
        //  @see    attr:formItem.iconHeight
        //<    
        
        //> @attr   formItemIcon.prompt (string : null : IRWA)
        // If set, this property will be displayed as a prompt (and tooltip text) for this form
        // item icon.
        // <P>
        // If unset the form item's <code>iconPrompt</code> property will be used instead.
        //
        //  @group  formIcons
        //  @visibility external
        //  @see    attr:formItem.iconPrompt
        //<

        //> @attr   formItemIcon.hspace (integer : null : IR)
        //      If set, this property determines the number of pixels space to be displayed on 
        //      the left of this form item icon.<br>
        //      If unset the form item's <code>iconHSpace</code> property will be used instead.
        //  @group  formIcons
        //  @see    attr:formItem.iconHSpace
        // @visibility external
        //<
        
        //> @method formItemIcon.showIf ()
        // If specified, <code>icon.showIf</code> will be evaluated when the form item is
        // drawn or redrawn. Return true if the icon should be visible, or false if it
        // should be hidden. Note that if +link{formItem.showIcon()} or +link{formItem.hideIcon()}
        // is called, this method will be overridden.
        // @param form (DynamicForm) the DynamicForm in which the icon is embedded
        // @param item (FormItem) the item to which this icon is attached.
        // @return (boolean) Return true if the icon should be visible, false otherwise.
        // @visibility external
        //<
    

    // Hints
    // --------------------------------------------------------------------------------------------

    //> @attr formItem.hint (HTMLString : null : IRW)
    // Specifies "hint" string to show next to the form item to indicate something to the user.
    // This string generally appears to the right of the form item.
    // 
    // @see hintStyle
    // @group appearance
    // @visibility external
    // @example formHints
    //<

    //> @attr formItem.showHint (Boolean : true : IRWA)
    // If a hint is defined for this form item, should it be shown?
    //
    // @group appearance
    // @visibility external
    //<
    showHint:true,

    // Styles
    // --------------------------------------------------------------------------------------------

    //> @attr formItem.showFocused     (Boolean : false : IRWA)
    // When this item receives focus, should it be re-styled to indicate it has focus?
    //
    // @group appearance
    // @visibility external
    // @see formItem.cellStyle
    //<
    showFocused:false,

    //> @attr formItem.showDisabled (Boolean : true : IRWA)
    // When this item is disabled, should it be re-styled to indicate its disabled state?
    //
    // @group appearance
    // @visibility external
    // @see cellStyle
    //<
    showDisabled:true,

    //> @attr formItem.showRTL (boolean : false : IRA)
    // When this item is in RTL mode, should its style name include an "RTL" suffix?
    // @group RTL
    // @group appearance
    // @visibility external
    // @see cellStyle
    //<
    showRTL:false,

    // Discussion on compound item / skinning for jsdoc. This is referred to by other JSDoc 
    // entries but doesn't need to show up in the doc-tree.
    //> @groupDef CompoundFormItem_skinning
    // When skinning basic FormItems like SelectItem and TextItem, consider that compound form
    // items like DateItem and ComboBox reuse simpler items like SelectItem and TextItem, so adding
    // a border to SelectItem would also apply a border to each select item within DateItem.<br>
    // To avoid such side-effects, if you want to add styling to all SelectItems used in your 
    // application, you can create an application-specific subclass like MySelectItem and apply 
    // properties there.<br>
    // @visibility external
    //<

    //> @type FormItemBaseStyle
    // This string is the base CSS class name applied to a FormItem (or some part of a form item).
    // The style name will be modified as the 'state' of the form item changes. Specifically:<ul>
    // <li>If +link{FormItem.showFocused} is true, when the form item receives focus, this
    //     style will have the suffix "Focused" appended to it.</li>
    // <li>If +link{FormItem.showErrorStyle} is true, if the form item has errors, this
    //     style will have the suffix "Error" appended to it.</li>
    // <li>If +link{FormItem.showDisabled} is true, when the form item is disabled, this
    //     style will have the suffix "Disabled" appended to it.</li>
    // <li>Finally, if +link{FormItem.showRTL} is true, when the form item is in RTL mode, this
    //     style will have the suffix "RTL" appended to it.</ul>
    // So for example if the cellStyle for some form item is set to "formCell" and showFocused
    // is true, when the form item receives focus, the form item's cell will have the "formCellFocused"
    // style applied to it.
    // @visibility external
    // @group appearance
    //<

    //> @attr formItem.cellStyle  (FormItemBaseStyle : "formCell" : IRW)
    // CSS style applied to the form item as a whole, including the text element, any icons, and
    // any hint text for the item. Applied to the cell containing the form item.
    // <P>
    // NOTE: See the +link{group:CompoundFormItem_skinning} discussion for special skinning considerations.
    // @visibility external
    // @group appearance
    //<
    cellStyle:"formCell",

    //> @attr formItem.hintStyle      (string : "formHint" : IRW)
    // CSS class for the "hint" string.
    //
    // @see hint
    // @group appearance
    // @visibility external
    //<
    
    hintStyle:"formHint",
    
    //> @attr formItem.titleStyle (FormItemBaseStyle : "formTitle" : IRW)
    // Base CSS class name for a form item's title. Note that this is a +link{FormItemBaseStyle} so
    // will pick up stateful suffixes on focus, disabled state change etc. by default.
    // <p>
    // Note the appearance of the title is also affected by
    // +link{dynamicForm.titlePrefix}/+link{dynamicForm.titleSuffix,titleSuffix} and 
    // +link{dynamicForm.requiredTitlePrefix}/+link{dynamicForm.requiredTitleSuffix,requiredTitleSuffix}.
    //
    // @visibility external
    // @see formItem.cellStyle
    //<
    titleStyle:"formTitle",
    
    //> @attr formItem.printTitleStyle (FormItemBaseStyle : null : IRW)
    // Base CSS stylename for a form item's title when generating print HTML for the item.
    // If unset +link{formItem.titleStyle} will be used instead.
    // @group printing
    // @visibility external
    //<

    //> @attr formItem.textBoxStyle (FormItemBaseStyle : null : IRW)
    // Base CSS class name for a form item's text box element.
    // <P>
    // NOTE: See the +link{group:CompoundFormItem_skinning} discussion for special
    // skinning considerations.
    // @group appearance
    // @visibility external
    // @see formItem.cellStyle
    //<
    //textBoxStyle:null,
    
    //> @attr formItem.printTextBoxStyle (FormItemBaseStyle : null : IRW)
    // Base CSS class name for a form item's text box element when getting printable HTML for the
    // form. If unset +link{formItem.textBoxStyle} will be used instead. Note that focused styling
    // will never be displayed while printing, though error and disabled styling will.
    // 
    // @group printing
    // @visibility external
    //<
    //printTextBoxStyle:null,

    //> @attr formItem.pickerIconStyle (FormItemBaseStyle : null : IRW)
    // Base CSS class name for a form item's picker icon cell. If unset, inherits from
    // this item's +link{controlStyle,controlStyle}.
    // @group pickerIcon
    // @group appearance
    // @see formItem.cellStyle
    // @visibility external
    //<
    //pickerIconStyle:null,

    //> @attr formItem.controlStyle (FormItemBaseStyle : null : IRW)
    // Base CSS class name for a form item's control box (surrounds text box and picker).
    // <P>
    // NOTE: See the +link{group:CompoundFormItem_skinning} discussion for special skinning considerations.
    // @group appearance
    // @group pickerIcon
	// @visibility external
    // @see formItem.cellStyle
    //<
    //controlStyle:null,
    
    //> @attr formItem.editPendingCSSText (string : "color:#0066CC;" : [IRWA])
    // Custom CSS text to be applied to cells with pending edits that have not yet been
    // submitted.
    // @visibility external
    // @group appearance
    //<
    editPendingCSSText:"color:#0066CC;",

    //> @attr formItem.showFocusedErrorState (Boolean : false : IRWA)
    // If set to true, when an item has errors and is focused, an "ErrorFocused" suffix 
    // will appear on the stylename.
    //
    // @group appearance
    // @visibility external
    // @see formItem.cellStyle
    //<
    showFocusedErrorState:false,

    // -------------------------------
    // Deprecated styling properties:
    
    //> @attr formItem.cellClassName (CSSStyleName : "formCell" : IRW)
    // CSS class for a form item's cell in the form layout
    // 
    // @group appearance
    // @visibility external
    // @deprecated As of SmartClient version 5.5, deprecated in favor of +link{formItem.cellStyle}
    //<

    //> @attr formItem.errorCellClassName (CSSStyleName : "formError" : IRW)
    // CSS class for a form item's cell when a validation error is showing.
    //
    // @group appearance
    // @visibility external
    // @deprecated As of SmartClient version 5.5 deprecated in favor of +link{formItem.cellStyle}
	//<

	//>	@attr formItem.titleClassName (CSSStyleName : "formTitle" : IRW)
    // CSS class for the form item's title.
    // @group title
    // @visibility external
    // @deprecated As of SmartClient Version 5.5, use +link{formItem.titleStyle} instead
    //<
	//titleClassName : "formTitle",

	//>	@attr formItem.titleErrorClassName (CSSStyleName : "formTitleError" : IRW)
    // CSS class for a form item's title when a validation error is showing.
    // @group title
    // @visibility external
    // @deprecated As of SmartClient Version 5.5, use +link{formItem.titleStyle} instead    
    //<
	//titleErrorClassName : "formTitleError",
    
    //>	@attr formItem.hintClassName (CSSStyleName : "formHint" : IRW)
	// CSS class for the "hint" string.
    //
    // @see hint
	// @group appearance
    // @visibility external
    // @deprecated As of SmartClient version 5.5, deprecated in favor of +link{FormItem.hintStyle}
	//<

    // Internal flag designating whether this element type has a data element 
    // (an actual HTML form element, holding a value).  Accessed via the 'hasDataElement()'
    // method.
    _hasDataElement:false,
    
    // Hovers
    // -----------------------------------------------------------------------------------------
    //> @attr formItem.hoverDelay (number : null : IRWA)
    // If specified, this is the number of milliseconds to wait between the user rolling over 
    // this form item, and triggering any hover action for it.<br>
    // If not specified <code>this.form.itemHoverDelay</code> will be used instead.
    // @group Hovers
    // @visibility external
    //<
    //,hoverDelay:null
        
    //> @attr formItem.hoverWidth (measure : null : [IRW])
    // Option to specify a width for any hover shown for this item.
    // @see DynamicForm.itemHoverWidth
    // @group Hovers
    // @visibility external
    //<
    
    //> @attr FormItem.hoverHeight  (measure : null : [IRW])
    // Option to specify a height for any hover shown for this item.
    // @see DynamicForm.itemHoverHeight
    // @group Hovers
    // @visibility external
    //<
    
    //> @attr FormItem.hoverAlign (Alignment  : null : [IRW])
    // Text alignment  for text displayed in this item's hover canvas, if shown.
    // @see DynamicForm.itemHoverAlign
    // @group Hovers
    // @visibility external
    //<
    
    //> @attr FormItem.hoverVAlign (VerticalAlignment : null : [IRW])
    // Vertical text alignment  for text displayed in this item's hover canvas, if shown.
    // @see DynamicForm.itemHoverVAlign
    // @group Hovers
    // @visibility external
    //<
    
    //> @attr FormItem.hoverStyle (CSSStyleName  : null : [IRW])
    // Explicit CSS Style for any hover shown for this item.
    // @see DynamicForm.itemHoverStyle
    // @group Hovers
    // @visibility external
    //<
    
    //> @attr FormItem.hoverOpacity (number : null : [IRW])
    // Opacity for any hover shown for this item
    // @see DynamicForm.itemHoverOpacity
    // @group Hovers
    // @visibility external
    //<
    
    //> @attr FormItem.hoverRect (object : null : [IRWA])
    // Explicit placement information for any hover shown for this item.
    // Should be specified as an object of the form <br>
    // <code>{left:[value], top:[value], width:[value], height:[value]}</code>
    // @see DynamicForm.itemHoverRect
    // @group Hovers
    // @visibility internal
    //<
    

    //> @attr formItem.showClippedTitleOnHover (boolean : true : [IRW])
    // If true and the title is clipped, then a hover containing the full title of this item
    // is enabled.
    // <p>
    // <smartclient>The +link{formItem.titleHover()} method is called before the
    // hover is displayed, allowing the hover to be canceled if desired. The HTML shown in the
    // hover can be customized by overriding +link{formItem.titleHoverHTML()}.</smartclient>
    // <smartgwt>A <code>TitleHoverEvent</code> is fired before the hover is displayed,
    // allowing the hover to be canceled if desired. The HTML shown in the hover can be customized
    // by setting a <code>FormItemHoverFormatter</code> on either this <code>FormItem</code>
    // or the <code>DynamicForm</code>. See <code>setItemTitleHoverFormatter()</code>.</smartgwt>
    // @group Hovers
    // @visibility external
    //<
    showClippedTitleOnHover:true,

    //> @attr FormItem.showClippedValueOnHover (Boolean : true : [IRW])
    // If true and the value is clipped, then a hover containing the full value of this item
    // is enabled.
    // <p>
    // <smartclient>The +link{FormItem.valueHover()} method is called before the
    // hover is displayed, allowing the hover to be canceled if desired. The HTML shown in the
    // hover can be customized by overriding +link{FormItem.valueHoverHTML()}.</smartclient>
    // <smartgwt>A <code>ValueHoverEvent</code> is fired before the hover is displayed,
    // allowing the hover to be canceled if desired. The HTML shown in the hover can be customized
    // by setting a <code>FormItemHoverFormatter</code> on either this <code>FormItem</code>
    // or the <code>DynamicForm</code>. See <code>setItemValueHoverFormatter()</code>.</smartgwt>
    // @group Hovers
    // @visibility external
    //<
    showClippedValueOnHover:true

    // Criteria and Operators
    // -----------------------------------------------------------------------------------------
    
    //> @attr formItem.operator (OperatorId : null : IR)
    // +link{OperatorId} to be used when +link{dynamicForm.getValuesAsCriteria()} is called.
    // <P>
    // <code>item.operator</code> can be used to create a form that offers search functions such
    // as numeric range filtering, without the more advanced user interface of the
    // +link{FilterBuilder}.  For example, two SpinnerItems could be created with
    // <code>formItem.operator</code> set to "greaterThan" and "lessThan" respectively to
    // enable filtering by a numeric range.
    // <P>
    // When <code>item.operator</code> is set for any FormItem in a form,
    // <code>form.getValuesAsCriteria()</code> will return an +link{AdvancedCriteria} object
    // instead of a normal +link{Criteria} object.  Each FormItem will produce one
    // +link{Criterion} affecting the DataSource field specified by +link{formItem.criteriaField}.
    // The criteria produced by the FormItems will be grouped under the logical operator
    // provided by +link{dynamicForm.operator}.
    // <P>
    // If <code>operator</code> is set for some fields but not others, the default operator is
    // "equals" for fields with a valueMap or an optionDataSource, and for fields of type "enum"
    // (or of a type that inherits from "enum").  The default operator for all other fields is
    // controlled by +link{dynamicForm.defaultSearchOperator}. 
    // <P>
    // <b>Note:</b> <code>formItem.operator</code> is only supported for a form that has a 
    // +link{dataBoundComponent.dataSource,dataSource}.  In a form with no DataSource, 
    // setting <code>formItem.operator</code> will have no effect. 
    //
    // @group criteriaEditing
    // @visibility external
    //<
    
    //> @attr formItem.criteriaField (identifier : null : IR)
    // When using +link{formItem.operator}, the name of the DataSource field for the
    // +link{Criterion} this FormItem generates.  If not specified, defaults to
    // +link{FormItem.name}.
    // <P>
    // Generally, because <code>criteriaField</code> defaults to <code>item.name</code>, you don't
    // need to specify it.  However, if more than one FormItem specifies criteria for the same
    // DataSource field, they will need unique values for +link{formItem.name} but should set
    // +link{formItem.criteriaField} to the name of DataSource field they both target.
    // <P>
    // For example, if two DateItems are used to provide a min and max date for a single field called
    // "joinDate", set +link{formItem.criteriaField} to "joinDate" on both fields but give the fields
    // distinct names (eg "minDate" and "maxDate") and use those names for any programmatic access,
    // such as +link{dynamicForm.setValue()}.
    //
    // @visibility external
    //<

    // -----------------------------------------------------------------------------------------

    //> @attr formItem.saveOnEnter (Boolean : null : IRW)
    // Set this to true to allow the parent form to save it's data when 'Enter' is pressed on 
    // this formItem and +link{DynamicForm.saveOnEnter,saveOnEnter} is true on the parent form.
    // @visibility external
    //<
    //saveOnEnter:false,
    
});

isc.FormItem.addMethods({
	//>	@method	FormItem.init()	(A)
	//			initialize the formItem object 
	//
	//		@param	[all arguments]	(object)	objects with properties to override from default
	//<
    _$height:"height", _$width:"width",
    _$colSpan:"colSpan", _$rowSpan:"rowSpan",
	init : function () {
        if (isc._traceMarkers) arguments.__this = this;
        
        // get a global ID so we can be called in the global scope
        // If getID() is called before this (typically only likely in an override of init),
        // we will already have a global ID - in this case avoid clobbering it.
        if (this.ID == null || window[this.ID] != this) {
            isc.ClassFactory.addGlobalID(this); 
        }
	
		// if "options" was specified, switch to "valueMap"
		if (this.options && !this.valueMap) {
			this.valueMap = this.options;
			delete this.options;
		}
        
        // Make sure that any 'measure' properties are in the correct format
        
        this._convertRawToMeasure(this._$height);
        this._convertRawToMeasure(this._$width);
        this._convertRawToMeasure(this._$colSpan);
        this._convertRawToMeasure(this._$rowSpan);
        
        // Start with our default value
        // - we do this rather than calling 'this.setValue(this.getDefaultValue())' for a
        //   couple of reasons:
        //   a) At this point the form's "values" object may not be initialized
        //   b) In subclass overrides, such as the container item, setValue() makes use of
        //      properties that get set up after this (for them Superclass) init()
        //      implementation
        // - setValue() would also call form.saveItemValue() - it's ok to skip this at this
        //   stage as after the form item has been created this call would be made after form
        //   item creation via 'setValues()' for any items where 'shouldSaveValue' is true.
        // - setValue() would also call setElementValue() - ok to skip as our elements haven't
        //   been set up until draw().
        this._value = this.getDefaultValue();
        // Note that this is the default value.
        this._setToDefault = true;
	
        this._setUpIcons();

        // If any validators have stopOnError set, this form item must be marked
        // validateOnExit:true. SynchronousValidation is also enabled.
        
        if ((!this.validateOnExit || !this.synchronousValidation) &&
            this.validators && this.validators.length > 0)
        {
            for (var i = 0; i < this.validators.length; i++) {
                if (this.validators[i].stopOnError) {
                    this.validateOnExit = true;
                    this.synchronousValidation = true;
                    break;
                }
            }
        }
        // If any form or form item has stopOnError set, this form item must be marked
        // validateOnExit:true. SynchronousValidation is also enabled.
        if ((!this.validateOnExit || !this.synchronousValidation) &&
            ((this.stopOnError == null && this.form && this.form.stopOnError) || this.stopOnError))
        {
            this.validateOnExit = true;
            this.synchronousValidation = true;
        }

        this.onInit(this);
    },

    // onInit() - notification method fired on initialization
    
    onInit:function (item) {
    },

    isRTL : function () {
        return this.containerWidget == null ? isc.Page.isRTL() : this.containerWidget.isRTL();
    },

    
    _$star:"*",
    _convertRawToMeasure : function (property) {
        var value = this[property];        
        if (value == null || isc.isA.Number(value) || value == this._$star) return value;
        var numericVal = parseInt(value);
        if (numericVal == value) {
            this[property] = numericVal;
            return value;
        }
        return value;
    },

    destroy : function (a,b,c,d,e) {

        
        if (isc.FormItem._pendingEditorExitCheck == this) {
            isc.FormItem._pendingEditorExitCheck.checkForEditorExit(true, true);
        }

        this.invalidateDisplayValueCache(true);

        if (this.isDrawn()) this.cleared();

        // If this is a form item that shows a unique pickList, destroy it too
        var pickList = this.pickList;
        this.pickList = null;
        if (pickList != null) {
            if (pickList.formItem == this) delete pickList.formItem;
            if (pickList.isVisible()) pickList.hide();
            if (!this.reusePickList()) pickList.destroy();
            else if (pickList.body) pickList.body._reused = true;
        }

        this.destroyed = true;
        this.form = null;
        this._dataElement = null;
        var undef;
        
        isc.ClassFactory.dereferenceGlobalID(this);
        this._releaseDOMIDs();
        
        // NOTE: we assume picker recycling as a default
        
        if (isc.EH._focusTarget == this) isc.EH._focusTarget = null;
        this.invokeSuper(isc.FormItem, "destroy", a,b,c,d,e);
    },

    clear : function () {
        if (this.picker) this.picker.clear();
    },
    
    // Override toString() so form items show their names rather than their IDs by default
    toString : function () {
        var name = this.getFieldName(),
            ID = this.ID,
        
            string = "[" + this.Class + " instance " + 
                        (name != null ? " name ='" + name + "', " : "") +
                        "global ID=" + ID + "]";
        return string;                        
        
    },
    
    getDataSource : function () {
        if (isc.isA.String(this.dataSource)) return isc.DS.get(this.dataSource);
        return this.dataSource;
    },
    
    
    registerWithDataView : function (dataView) {
        if (!this.inputDataPath) return;
        
        if (!dataView) {
            dataView = this.form;
            while (dataView && !isc.isA.DataView(dataView)) dataView = dataView.parentElement;
        }
        
        if (!dataView) {
            this.logWarn("Component initialized with an inputDataPath property, but no DataView " +
                         "was found in the parent hierarchy. inputDataPath is only applicable to " +
                         "DataBoundComponents and FormItems being managed by a DataView");
            return;
        }
        
        dataView.registerItem(this);
    },


    // IDs and names
	// --------------------------------------------------------------------------------------------
    
	//>	@method	formItem.getFieldName()	(A)
	//			Return the name for the this formItem.
	//		@group	drawing
	//
	//		@return	(string)	name for this form item
    // @visibility external
	//<
	getFieldName : function () {
        return this.name;
	},
    
    //>	@method	formItem.getDataPath() (A)
    // Return the dataPath for the this formItem.
    // @return (dataPath) dataPath for this form item
    // @visibility external
    //<
    getDataPath : function () {
        return this.dataPath;
    },
    
    // returns this.datapath, but if the path is absolute and includes
    // this form's dataPath, trims that off (so it's the datapath relative to the containing form)
    getTrimmedDataPath : function () {
        var dp = this.getDataPath();
        if (dp && this.form && this.form.dataPath) {
            dp = this.form._trimDataPath(dp);
        }
        if (dp && dp.endsWith("/")) dp = dp.substring(0, dp.length-1);
        return dp;
    },
    
    //>	@method	formItem.getFullDataPath() (A)
    // Return the fully-qualified dataPath for the this formItem (ie, the dataPath expressed 
    // in absolute terms from the root of the hierarchy, rather than relative to the item's 
    // parent form).  Note that the item's name is substituted into the full dataPath if the
    // item does not specify an explicit dataPath.  For example, if we have a field called 
    // <code>name</code> that specifies no dataPath, on a form that specifies a dataPath of 
    // <code>/order/items</code>, this method will return <code>/order/items/name</code>
    // @return (DataPath) Fully-qualified dataPath for this form item
    // @visibility external
    //<
    getFullDataPath : function () {
        var localDP =  this.getDataPath() || this.getFieldName();
        if (!localDP) {
            if (this.shouldSaveValue) {
                this.logWarn("Encountered field with neither name nor dataPath: " + 
                                this.echo(this));
            }
            localDP = "";
        }
        // convert numbers - historically it was allowed to have field names that are numbers...
        if (!isc.isA.String(localDP)) localDP = localDP+"";
        if (localDP.startsWith(isc.Canvas._$slash)) return localDP;
        var parentDP = this.form.getFullDataPath();
        if (parentDP && parentDP != isc.Canvas._$slash) {
            return parentDP + isc.Canvas._$slash + localDP;
        }
        return localDP;
    },
    
    //>	@method	formItem.shouldSaveOnEnter() (A)
    // Returns true if 'Enter' key presses in this formItem should allow a saveOnEnter: true
    // parent form to save it's data.  The default implementation returns the value of
    // +link{FormItem.saveOnEnter} or false if that property is unset.
    // @return (Boolean) boolean indicating whether saving should be allowed to proceed
    // @visibility external
    //<
    shouldSaveOnEnter : function () {
        var result = this.saveOnEnter != null ? this.saveOnEnter : false;
        return result;
    },


    //>	@method	formItem.getItemName()	(A)
	//			Return the name for the this formItem.  Synonym for getFieldName()
	//		@group	drawing
	//
	//		@return	(string)	name for this form item
    // @visibility internal
	//<
    getItemName : function () {
        return this.getFieldName();
    },


	//>	@method	formItem.getElementName()	(A)
	//			Return the name to be written into this form item's HTML element.
    //          This will not necessarily match the value returned by this.getFieldName().
	//		@group	drawing
	//
	//		@return	(string)	name of the form element
	//<
    
    _$underscore:"_",
    _$value:"value",
    getElementName : function () {

        
        if (this.isInactiveHTML()) return "";
		var name = this.getFieldName();

		// if this item has a parentItem, prepend the name of the parentItem
        // This is just for uniqueness - we will not be trying to parse this string to determine
        // what an items' parent element is
		if (this.parentItem) {
			var masterName = this.parentItem.getElementName();
			if (name == isc.emptyString) name = masterName;
			else name = [masterName, this._$underscore, name].join(isc.emptyString);
		}
        
        // If we still don't have a name, or the name matches the ID for the item,
        // return a unique 'name' string for this item.
        // - Note: we can't actually use the ID of the item, because when we write handlers 
        //   into the form items, we want to be able to refer to the item by it's ID. Because 
        //   the handlers are executed in the scope of the native form object in the 
        //   document.forms array, if the native name of the Form Element matches the ID of 
        //   the item, the ID would give us a pointer to the Form Element rather than the item.
        if (name == null || name == this.getID() || name == isc.emptyString) {
            name = this._getDOMID(this._$value);
        }
        
		return name;
	},

    //> @method formItem.getBrowserInputType() (A)
    // Gets the value to use for the INPUT element's type attribute if
    // +link{FormItem.browserInputType,browserInputType} is specified.
    //<
    getBrowserInputType : function () {
        var browserInputType = this.browserInputType;
        if (browserInputType == null) return null;
        if (this.browserInputTypeMap.hasOwnProperty(browserInputType)) {
            browserInputType = this.browserInputTypeMap[browserInputType];
        }
        return browserInputType;
    },

	//>	@method	formItem.getDataElementId()	(A)
	// Return the ID for this form item's data element.
    //		@group	drawing
	//		@return	(string)	name of the form element
    // @visibility testAutomation
	//<
    
    _$dataElement:"dataElement",
    getDataElementId : function () {
        // inactiveHTML depends on context so we can't simply cache...
        if (this.isInactiveHTML()) return this._getDOMID(this._$dataElement);
        
        if (this.__tagId == null) {
            this.__tagId = this._getDOMID(this._$dataElement, true);
        }
        // This will be a unique ID that can be written into the element tag.
        // It's used for getting a reference to the form item element to be used by the
        // "FOR" property written into the label for the form item.
        // Doesn't necessarily reflect anything about the information carried by the form item.
        return this.__tagId;
        
    },
    
	//>	@method	formItem.getItemID()	(A)
	//		Return the unique global ID for this form item instance.
    //      The item is available in the global scope via this ID as window[itemID].
    //      Synonym for getID().
	//		@group	drawing
	//
	//		@return	(string)	ID of the form item.
	//<
	getItemID : function () {
        return this.getID();
	},
   
    //>	@method	formItem.getID()    (A)
	//		Return the unique global ID for this form item instance.
    //      The item is available in the global scope via this ID as window[itemID].
	//		@group	drawing
	//
	//		@return	(string)    ID of the item.
	//<
    getID : function () {
        
        if (this.ID == null) {
            isc.ClassFactory.addGlobalID(this);
        }
        return this.ID;
    },
    
    
    
    // Titles
	// --------------------------------------------------------------------------------------------
    
   	//>	@method	formItem.shouldShowTitle()	(A)
	//	Draw a cell for this item title?
	//		@group	drawing
	//
	//		@return	(boolean)	true if item title cell should be drawn
	//<
	shouldShowTitle : function () {
		return this.showTitle;
	},
    
	//>	@method	formItem.getTitleHTML()	(A)
	//	Return the HTML for the title of this formItem
	//		@group	drawing
	//
	//		@return	(HTML)	title for the formItem
	//<
	getTitleHTML : function () {
        var title = this.getTitle();

        // If we don't have an element, simply return the title (no need for the "label for..."
        // functionality).
        if (!this._canFocus()) return title;

        if (this.accessKey != null) {
            // underline the accessKey char within the title
            title = isc.Canvas.hiliteCharacter(title, this.accessKey);
        }
        
        
        // Note: the <LABEL> tag allows us to set an accessKey on the element without writing it
        // directly into the element's HTML.  It also improves on (for example) screen reader 
        // support. It also means clicking the title will put focus into the target form item.
        
        var focusElementId;
        if (this.hasDataElement()) focusElementId = this.getDataElementId();
        
        
        if (!focusElementId) return title;

        return isc.SB.concat(
            "<LABEL FOR=", focusElementId,
             (this.accessKey != null ? " ACCESSKEY=" + this.accessKey : isc._emptyString),
            ">", title, "</LABEL>");
	},
	
	//>	@method	formItem.getTitle()	(A)
	//	Return the title of this formItem
	//		@group	drawing
	//
	//		@return	(HTML)	title for the formItem
    // @visibility external
	//<
    getTitle : function () {
        var undef;
        if (!this.form) return;
        // allow a developer to actually specify a null title, but showTitle true as an obvious
        // way to leave alignment all the same but not show annoying ":" next to the title cell.
        if (this[this.form.titleField] !== undef) return this[this.form.titleField];
        return this[this.form.fieldIdProperty];
    },

    // Defer to DF to pick up the form's default titleOrientation
    getTitleOrientation : function () { return this.form.getTitleOrientation(this); },

    // Layout
	// --------------------------------------------------------------------------------------------

    //> @method formItem.isVisible()    ()
    // Return true if the form item is currently visible. Note that like the similar
    // +link{canvas.isVisible(),Canvas API}, it indicates visibility settings only and so
    // will return true for an item that is not drawn.
    // 
    //  @group  visibility
    //  @return (Boolean)   true if the form item is visible
    // @visibility external
    //<
    isVisible : function () {
        if (!this.containerWidget.isVisible()) return false;
            
        // If we have a showIf(), which evaluated to false, we will have been marked as
        // visibility false (done in DynamicForm.getInnerHTML()).
        if (this.visible == false) return false;
        // If we're a child of a container item, check whether the container item has been
        // marked as not-visible.
        if (this.parentItem && !this.parentItem.isVisible()) return false;
        return true;
    },
	
   	//>	@method	formItem.getRowSpan()	(A)
	// Return the rowSpan for this item
	//		@group	formLayout
	//
	//		@return	(number)	rowSpan
	//<
	getRowSpan: function () {
		return this.rowSpan;
	},

   	//>	@method	formItem.getColSpan()	(A)
	// Return the colSpan for this item
	//		@group	formLayout
	//
	//		@return	(number)	colSpan
	//<
	getColSpan : function () {
        // disallow colSpan of zero
        if (this.colSpan == 0) this.colSpan = 1;
		return this.colSpan;
	},

    //> @method formItem.getTitleColSpan() (A)
    // Return the titleColSpan for this item
    // @group formLayout
    //
    // @return (number) titleColSpan
    //<
    getTitleColSpan : function () {
        // disallow titleColSpan of zero
        if (this.titleColSpan == 0) this.titleColSpan = 1;
        return this.titleColSpan;
    },

    //> @method formItem.isStartRow()   (A)
    // Should this item be drawn on a new row?
    //      @group tableLayout
    //      @return (boolean) true if a new row should start for this item
    //<
    isStartRow : function () {
        return this.startRow
    },
    
    //> @method formItem.isEndRow()   (A)
    // Should this be the last item on a row?
    //      @group tableLayout
    //      @return (boolean) true if a new row should start after this item
    //<
    isEndRow : function () {
        return this.endRow
    },

    //>	@method	formItem.getRect()
    // Return the coordinates of this object as a 4 element array.
    //		@group	positioning, sizing
    //		
    //		@return	(array)		[left, top, width, height]
    // @visibility external
    //<
    getRect : function () {
	    return [this.getLeft(), this.getTop(), this.getVisibleWidth(), this.getVisibleHeight()];
    },
  
    //>	@method	formItem.getPageRect()
    // Return the page-level coordinates of this object as a 4 element array.
    //		@group	positioning, sizing
    //		
    //		@return	(array)		[left, top, width, height]
    // @visibility external
    //<
    getPageRect : function (includeTitle) {
        if (includeTitle) return this.getPageRectIncludingTitle();
        return [this.getPageLeft(), this.getPageTop(), 
                this.getVisibleWidth(), this.getVisibleHeight()];
    },

    getPeerRect : function () {
        return this.getPageRect();
    },
    
    getPageRectIncludingTitle : function () {
        var left = this.getPageLeft(),
            top = this.getPageTop(),
            width = this.getVisibleWidth(),
            height = this.getVisibleHeight();
            
        if (this.showTitle) {
            var titleLeft = this.getTitlePageLeft(),
                titleTop = this.getTitlePageTop(),
                titleWidth = this.getVisibleTitleWidth(),
                titleHeight = this.form.getTitleHeight(this);;
            if (this.titleOrientation == "left" || this.titleOrientation == "left" ||
                this.titleOrientation == null) 
            {
                left = left < titleLeft ? left : titleLeft;
                width += titleWidth; 
            } else {
                left = left < titleLeft ? left : titleLeft;
                width = width > titleWidth ? width : titleWidth;
                if (isc.isA.Number(titleHeight)) height += titleHeight;
            }
        }
        return [left, top, width, height];        
    },

    
    getCellHeight : function (reportOverflowedSize) {
        if (isc._traceMarkers) arguments.__this = this;
        
        if (this.cellHeight != null) {
            return this.cellHeight;
        }
        
        var height = this.getHeight(reportOverflowedSize);
        if (!isc.isA.Number(height)) return height;

        // never report a height lower than that required by our visible icons
        // (these are our external icons - not our picker icon)
        var iconsHeight = this.getIconsHeight();
        if (height < iconsHeight) {
            height = iconsHeight;
        }
        
        // If we are showing a picker icon, and it has a specified height, that may also cause
        // our height to be larger than expected.
        // If no specified height, sized to fit in available space, so won't expand the item.
        if (this._shouldShowPickerIcon() && this.pickerIconHeight) {
            var pickerIconHeight = this.pickerIconHeight + this._getPickerIconVPad();
            if (pickerIconHeight > height) height = pickerIconHeight;
        }

        var form = this.containerWidget;
        if (this._absPos() || !isc.isA.DynamicForm(form)) return height;

        height += this._getCellVBorderPadSpacing();
        
        // If titleOrientation is TOP, and we're showing a title, add our title height to our
        // reported cellHeight, so tableLayoutPolicy code will take it into account
        
        if (this.showTitle && this.form.getTitleOrientation(this) == isc.Canvas.TOP) {
            height += this.form.getTitleHeight(this);
        }
        return height;
    },
    // Forms only write a height into the cell containing the form item if shouldFixRowHeight
    // is true.
    // If we have an explicit cellHeight specified, consider the height of this item "fixed"!
    // If we have an explicit height but are not applying it to the text box, also apply
    // the height to the cell.
    shouldFixRowHeight : function () {
        return this.cellHeight != null || 
            (!this.shouldApplyHeightToTextBox() && this.getHeight() != null);
    },
    
    // Returns the space taken up around this form item by the cell (determined from
    // cellSpacing, border and padding).
    _getCellVBorderPadSpacing : function () {
    
        var height = 0,
            form = this.form,
            cellStyle = this.getCellStyle();

        // For items written into containerIems, cellSpacing/padding will be defined on the
        // parent item, not the form.
        if (this.parentItem) form = this.parentItem;            
            
        // Spacing around cells (above and below)
        height += 2*form.cellSpacing;            
    
        
        var cellPadding = isc.isA.Number(form.cellPadding) ? form.cellPadding : 0,
            paddingTop = isc.Element._getTopPadding(cellStyle, true);
        if (paddingTop == null) paddingTop = cellPadding
        
        var paddingBottom = isc.Element._getBottomPadding(cellStyle, true);
        if (paddingBottom == null) paddingBottom = cellPadding;
        
        height += paddingTop;
        height += paddingBottom;
        height += isc.Element._getVBorderSize(cellStyle);
        
        return height;
    },
    _getCellHBorderPadSpacing : function () {
    
        var height = 0,
            form = this.form,
            cellStyle = this.getCellStyle();
        
        // For items written into containerIems, cellSpacing/padding will be defined on the
        // parent item, not the form.
        if (this.parentItem) form = this.parentItem;
            
        // Spacing around cells (above and below)
        if (isc.isA.Number(form.cellSpacing)) height += 2*form.cellSpacing;            
    
        
        // We have seen a case where a developer set form.cellPadding to a string 
        // ("2" rather than 2), which led to us assembling an inappropriate string like "0220"
        // in this method. If cellPadding is specified as a string just ignore it.
        var formCellPadding = isc.isA.Number(form.cellPadding) ? form.cellPadding : 0,
            paddingLeft = isc.Element._getLeftPadding(cellStyle, true);
        if (paddingLeft == null) paddingLeft = formCellPadding;
        
        var paddingRight = isc.Element._getRightPadding(cellStyle, true);
        if (paddingRight == null) paddingRight = formCellPadding;
        
        height += paddingLeft;
        height += paddingRight;
        height += isc.Element._getHBorderSize(cellStyle);
        
        return height;
    },

    //>@method  FormItem.getInnerHeight()   
    // Returns the available space for content of this FormItem, based on the specified
    // height for the item, and any styling.
    // @return (number) height in px.
    //<
    // This method returns the space within the cell derived either from item.cellHeight
    // (less padding etc), or item.height.
    // This means if you have both item.height and item.cellHeight specified, the
    // cellHeight is used.
    // Implementation note: We actually look at item._size in this method - that's set 
    // up by logic at the DynamicForm level which feeds the items into the 
    // TableResizePolicy which in turn calls item.getCellHeight() to figure out
    // the heights.
    
    
    getInnerHeight : function () {
        var form = this.containerWidget;

        if (this._absPos()) return this._getPercentCoord(this.height, true);

        // If we've never run through stretch-resize-policy, this.height/width may be
        // specified as a string.
        // If we're being written out as standalone item HTML in a non-form containerWidget,
        // give that widget a chance to size the item (resolving "*" etc sizes)
        
        if (this._size == null && this.height != null && isc.isA.String(this.height) &&
            this.containerWidget && !isc.isA.DynamicForm(this.containerWidget) &&
            this.containerWidget.sizeFormItem != null)
        {
            this.containerWidget.sizeFormItem(this);
        }

        
        if (this._size) {            
            var height = this._size[1];
            if (!isc.isA.Number(height)) return height;
            
                            
            if (this._writtenIntoCell()) {
                height -= this._getCellVBorderPadSpacing();
            }
            return height;
        }
        return this.getHeight();
    },

    
    getInnerWidth : function (adjustForIcons) {
        var form = this.containerWidget;

        if (this._absPos()) return this._getPercentCoord(this.width);
        
        // If we've never run through stretch-resize-policy, this.height/width may be
        // specified as a string.
        // If we're being written out as standalone item HTML in a non-form containerWidget,
        // give that widget a chance to size the item (resolving "*" etc sizes)
        
        if (this._size == null && this.width != null && isc.isA.String(this.width) &&
            this.containerWidget && !isc.isA.DynamicForm(this.containerWidget) &&
            this.containerWidget.sizeFormItem != null)
        {
            this.containerWidget.sizeFormItem(this);
        }

        var width = this._size ? this._size[0] : this.width;

        // happens if StretchResize hasn't been run and size is specified as eg "*".  In this
        // case the FormItem may not handle the size in string form anyway, but we shouldn't
        // try to do math on it.
        if (!isc.isA.Number(width)) {
            return width;
        }
        // _size refers to the total area taken up by this items cell - to get the innerWidth
        // (available for the item and it's icons), deduct the border / padding / spacing
        // of the cell)
                        
        if (this._writtenIntoCell()) {        
            width -= this._getCellHBorderPadSpacing();
        }
        return width;
    },
    
    // getColWidth()
    // If this item is being written into a standard dynamic form cell, determine the width for
    // the column this item is written into.
    
    getColWidth : function () {
        var items = this.form ? this.form.items : null;
        if (items && items._colWidths != null && this._tablePlacement != null) {
            // this._tablePlacement stored as [startCol, startRow, endCol, endRow]
            var startCol = this._tablePlacement[0],
                endCol = this._tablePlacement[2];
            if (this.showTitle) {
                var titleOrientation = this.getTitleOrientation();
                if (titleOrientation == isc.Canvas.LEFT) startCol += 1;
                else if (titleOrientation == isc.Canvas.RIGHT) endCol -= 1;
            }
            var width = 0;
            for (var c = startCol; c < endCol; c++) {
                width += items._colWidths[c];
            }
            return width;
        }
        return null;
    },


    
    _absPos : function () {
        return (this.containerWidget._absPos && this.containerWidget._absPos());
    },
    
    _writtenIntoCell : function () {
        return (this.containerItem != null || 
                (this.form == this.containerWidget && !this._absPos()));
    },
    // percent coordinate interpretation for absPos forms
    _$percent:"%",
    _getPercentCoord : function (coord, vertical) {
        // decided against since this re-interprets the default size of "*" for many items
        //if (coord == "*") coord = "100%"; 
        if (isc.isA.String(coord) && isc.endsWith(coord, this._$percent)) {
            var parent = this.containerWidget,
                parentSize = vertical ? parent.getInnerHeight() : parent.getInnerWidth();
            return Math.round((parseInt(coord, 10) / 100) * parentSize);
        }
        return coord;
    },

    
    getElementWidth : function () {
        var width = this.getInnerWidth();
        
        if (!isc.isA.Number(width)) return null;
        width -= this.getTotalIconsWidth();

        return (isc.isA.Number(width) ? Math.max(width, 1) : null);
    },
    
    
    // getTextBoxWidth() / height()
    // returns the size of the text box (used for writing out HTML - not retrieved by looking at
    // the DOM element in question).    
    
    
    getTextBoxWidth : function (value) {
        var basicWidth = this.getElementWidth();
        if (!isc.isA.Number(basicWidth)) return basicWidth;

        if (this.textBoxStyle) {
            var className = this.getTextBoxStyle();
            basicWidth -= (isc.Element._getLeftMargin(className) + isc.Element._getRightMargin(className));
            if (this._sizeTextBoxAsContentBox()) {
                basicWidth -= isc.Element._getHBorderPad(className);
            }
        }
        if (this._shouldShowPickerIcon()) {
            basicWidth -= this.getPickerIconWidth();
            var iconProps = this.getPickerIcon();
            if (iconProps.hspace != null) basicWidth -= iconProps.hspace;
            if (this.pickerIconStyle) 
                basicWidth -= isc.Element._getHBorderPad(this.getPickerIconStyle());
            if (this.controlStyle)
                basicWidth -= isc.Element._getHBorderPad(this.getControlStyle());
        }

        
        if (this.hasDataElement() && this._getValueIcon(value)) {
            basicWidth -= ((this.getValueIconWidth() || 0) + 
                                    (this.valueIconLeftPadding + this.valueIconRightPadding));
        }

        // reduce by error width for left or right-oriented errors
        return basicWidth - this._getErrorWidthAdjustment();
    },

    // anticipated width of the error message, if we are showing errors on the left or right
    getErrorWidth : function () {
        // If we are showing errors on the left/right we should adjust the textBox size to account
        // for them. We don't know how long the error strings will be and it's ok for them to wrap
        // so make the default space we leave for them configurable at the item level
        var errorWidth = 0;
        if (this.form.showInlineErrors && this.hasErrors()) {
            var orientation = this.getErrorOrientation();
            if (orientation == isc.Canvas.LEFT || orientation == isc.Canvas.RIGHT) {
                if (this.shouldShowErrorText()) {
                    errorWidth += this.errorMessageWidth;
                } else if (this.shouldShowErrorIcon()) {
                    
                    errorWidth += this.errorIconWidth + this.iconHSpace;
                }
            }
        }
        return errorWidth;
    },
    
    // _getErrorWidthAdjustment
    // If we're showing horizontal-orientated error text/icon - how much do we need to reduce
    // the text box's rendered size by to leave space for the error text
    _getErrorWidthAdjustment : function () {
        var errorWidth = this.getErrorWidth();
        if (errorWidth != 0 && this.expandHintAndErrors && (this.getColWidth() != null)) {
            var additionalColSpace = this.getColWidth() - this.getInnerWidth();
            if (additionalColSpace > 0) errorWidth -= additionalColSpace;
            // don't allow the value to go negative
            if (errorWidth < 0) errorWidth = 0;
        }
        return errorWidth;
    },
    

    

    //> @attr formItem.errorMessageWidth (int : 80 : IRW)
    // When +link{dynamicForm.showInlineErrors} and +link{showErrorText} are both true and
    // +link{errorOrientation} is "left" or "right", errorMessageWidth is the amount to reduce
    // the width of the editor to accommodate the error message and icon.
    // @group validation
    // @visibility external
    //<
    errorMessageWidth:80,
    
    // Helpers to avoid code duplication
    getValueIconHeight : function () {
        var height = this.valueIconHeight;
        if (height == null) height = this.valueIconSize;
        return height;
    },
    
    getValueIconWidth : function () {
        var width = this.valueIconWidth;
        if (width == null) width = this.valueIconSize;
        return width;
    },
        
    //> @attr formItem.applyHeightToTextBox (Boolean : null : IRA)
    // If +link{formItem.height} is specified, should it be applied to the
    // item's text box element?
    // <P>
    // If unset, behavior is determined as described in +link{shouldApplyHeightToTextBox()}
    // @visibility external
    //<
    
    //> @method formItem.shouldApplyHeightToTextBox() [A]
    // If +link{formItem.height} is specified, should it be applied to the
    // item's text box element? If this method returns false, the
    // text box will not have an explicit height applied, though the containing cell
    // will be sized to accomodiate any specified height.
    // <P>
    // This is used in cases where the text box does not have distinctive styling
    // (for example in standard +link{StaticTextItem}s). As the textBox has no explicit
    // height, it fits the content. Since the text box is not visually distinct to 
    // the user, this makes +link{formItem.vAlign} behave as expected with the 
    // text value of the item being vertically aligned within the cell.
    // <P>
    // Default implementation will return +link{applyHeightToTextBox} if explicitly set
    // otherwise <code>false</code> if +link{readOnlyDisplay} is set to 
    // <code>"static"</code> and the item is +link{getCanEdit(),not editable}, otherwise
    // true.
    // @return (boolean) true if the height should be written into the items' text box.
    // @visibility external
    //<
    shouldApplyHeightToTextBox : function () {
        if (this.applyHeightToTextBox != null) return this.applyHeightToTextBox;
        if (this.renderAsStatic()) return false;
        return true;
    },
    
    getTextBoxHeight : function () {

        if (!this.shouldApplyHeightToTextBox()) return null;
        
        
        var basicHeight = this.getPixelHeight(true);
        if (!isc.isA.Number(basicHeight)) return basicHeight;
        
        
        if (this.textBoxStyle) {
            var className = this.getTextBoxStyle();
                            
            basicHeight -= (isc.Element._getTopMargin(className) + 
                            isc.Element._getBottomMargin(className));
            if (this._sizeTextBoxAsContentBox()) {
                basicHeight -= isc.Element._getVBorderPad(className);
            }
        }
        // If we're writing out a control box we also have to adjust the height for the control
        // box's styling
        if (this._shouldShowPickerIcon() && this.controlStyle) {
            basicHeight -= isc.Element._getVBorderPad(this.getControlStyle());
        }
        
        
        if (this.showTitle && this.form.getTitleOrientation(this) == isc.Canvas.TOP &&
            !isc.isA.Number(this.getCellHeight())) 
        {
            basicHeight -= this.form.getTitleHeight(this);
        }        
        
        return basicHeight;
    },
    
    
    _sizeTextBoxAsContentBox : function () {
        return !isc.Browser.isBorderBox;
    },

    
    // getTextPickerIconWidth() / height()
    // returns the size of the picker icon's cell (used for writing out HTML - not retrieved by looking at
    // the DOM element in question).
    getPickerIconWidth : function () {
        return (this.pickerIconWidth != null ? this.pickerIconWidth : this.getPickerIconHeight());
    },
    
    getPickerIconHeight : function () {
        if (this.pickerIconHeight != null) return this.pickerIconHeight;
        else {
            var height = (isc.isA.Number(this.getHeight()) ? this.getHeight() : this.getInnerHeight());
            if (!isc.isA.Number(height)) return null;
            
            height -= this._getPickerIconVPad();
            
            this.pickerIconHeight = height;
            return height;
        }
    },
    
    // Vertical padding between the picker icon and the outer table
    _getPickerIconVPad : function () {
        
        var pad = 0;
        if (this.controlStyle){
            pad += isc.Element._getVBorderPad(this.controlStyle);
        }
        if (this.pickerIconStyle) {
            pad += isc.Element._getVBorderPad(this.pickerIconStyle);
        }
        return pad;
    },
    
	//>	@method	formItem.getHeight()	(A)
	//	Output the height for this element
	// @group	sizing
	// @return	(int | String)	height of the form element
	//<
	getHeight : function () {
		return this.height;		
	},
	
    //> @method formItem.getPixelHeight()
    // Returns the specified +link{formItem.height} of this formItem in pixels. 
    // For heights specified as a percentage value or <code>"*"</code>, the 
    // pixel height may not be available prior to the item being drawn. In cases where
    // the height has not yet been resolved to a pixel value, this method will return
    // <code>-1</code>.
    // @return (int) Specified height resolved to a pixel value.
    // @visibility external
    //<
    // returnRawHeight parameter used internally
    getPixelHeight : function (returnRawHeight) {
        var basicHeight = this.getHeight();
        
        if (!isc.isA.Number(basicHeight)) {
            var innerHeight = this.getInnerHeight();
            if (this.cellHeight != null && isc.isA.String(basicHeight) && 
                basicHeight.endsWith("%")) 
            {
                var percentHeight = parseInt(basicHeight);
                if (isc.isA.Number(innerHeight)) {
                    basicHeight = Math.round(innerHeight * (percentHeight/100));
                } else {
                    basicHeight = innerHeight;
                }
            } else {
                basicHeight = innerHeight;
            }
        }
        if (!isc.isA.Number(basicHeight)) return returnRawHeight ? basicHeight : -1;
        
        // If we're showing a valueIcon, adjust the textBox height to accommodate it if necessary
        if (this.valueIcons != null || this.getValueIcon != null) {
            var valueIconHeight = this.getValueIconHeight();
            if (valueIconHeight > basicHeight) basicHeight = valueIconHeight;
        }
        return basicHeight;
    },

    //>	@method	formItem.getVisibleHeight()	(A)
	//	Output the drawn height for this item in pixels.
    //  Note: this is only reliable after this item has been written out into the DOM.
	//		@group	sizing
	//		@return	(integer)	height of the form item
	// @visibility external
	//<    
    // this returns the height of the outer table for the item 
    getVisibleHeight : function () {
        var element = this.isDrawn() ? this.getOuterElement() : null;
        if (element == null) {
            this.logInfo("getVisibleHeight() - unable to determine drawn height for this item -" +
                         " returning pixel height from specified height", "sizing");
            if (isc.isA.Number(this.height)) {
                return this.height;
            } 
           
            this.logWarn("getVisibleHeight() unable to determine height - returning zero", 
                         "sizing");
            return 0;        
        }
        
        return element.offsetHeight;    
    },

    //>	@method	formItem.getIconHeight()	(A)
	//	Takes an icon definition object, and returns the height for that icon in px.
	//		@group	sizing
    //      @param  icon (object)   icon definition object for this item.
	//		@return	(number)	height of the form item icon in px
    //      @visibility external    
	//<
    getIconHeight : function (icon) {
        // default to the first icon, if possible
        if (icon == null && this.icons != null && this.icons.getLength() > 0) icon = this.icons[0];
        else if (!this._isValidIcon(icon)) {
            this.logWarn("getIconHeight() passed invalid icon:" + isc.Log.echoAll(icon));
            return null;
        }
        
        // Note: we could actually look at the icon element in the DOM, (if it's drawn)
        // but we have full control over the HTML written into form item icons, so this value 
        // should always match the specified size for the icon.
        return (icon.height != null ? icon.height : this.iconHeight);

    },

    getTitleVisibleHeight : function () {
        var titleElement = this.isDrawn() && this.form
                                          ? isc.Element.get(this.form._getTitleCellID(this)) 
                                          : null;
        if (titleElement == null) {
            var warning = "getTitleHeight() Unable to determine position for " + 
                          (this.name == null ? "this item " : this.name) + ". ";
            if (this.isDrawn()) {
                warning += "This method is not supported by items of type " + this.getClass();
            } else {
                warning += "Position cannot be determined before the element is drawn"
            }
            warning += " - returning zero.";
            
            this.form.logWarn(warning);
            return 0;
        }
        return isc.Element.getVisibleHeight(titleElement);
    },
    

	//>	@method	formItem.getWidth()	(A)
	//	Output the width for this element. Note this returns the specified width for the 
    //  element, which may be "*" or a percentage value. Use 'getVisibleWidth()' to get the
    //  drawn width in pixels.
	// @group	sizing
	// @return	(int | String)	width of the form element
	// @visibility external
	//<
	getWidth : function () {
		return this.width
	},
	
    //> @method formItem.getPixelWidth()
    // Returns the specified +link{formItem.width} of this formItem in pixels. 
    // For widths specified as a percentage value or <code>"*"</code>, the 
    // pixel width may not be available prior to the item being drawn. In cases where
    // the width has not yet been resolved to a pixel value, this method will return
    // <code>-1</code>.
    // @return (int) Specified width resolved to a pixel value.
    // @visibility external
    //<
    
    getPixelWidth : function () {
        var basicWidth = this.getWidth();
        if (!isc.isA.Number(basicWidth)) {
            var innerWidth = this.getInnerWidth();
            if (innerWidth != null) basicWidth = innerWidth;
        }
        return isc.isA.Number(basicWidth) ? basicWidth : -1;
    },
 
    //>	@method	formItem.getVisibleWidth()	(A)
	//	Output the drawn width for this item in pixels. This method is only reliable after
	//  the item has been drawn into the page.
	//		@group	sizing
	//		@return	(integer)	width of the form item
	// @visibility external
	//<    
    
    getVisibleWidth : function () {
        var element = this.isDrawn() ? this.getOuterElement() : null;
        if (element == null) {
            this.logInfo("getVisibleWidth() - unable to determine drawn width for this item -" +
                         " returning pixel width from specified width", "sizing");
            if (isc.isA.Number(this.width)) {
                return this.width;
            // HACK: stretchResizePolicy is run on the form when writing out items into the DOM
            // this will resolve non numeric (* and percentage) sizes to pixel widths, and store
            // the specified column sizes on the items object as _colWidths.  If this is present
            // return the appropriate numeric value.
            } else if (this.form && this.form.items._colWidths != null) {
            
                return this.form.items._colWidths[this.form.getItems().indexOf(this)];
            }
        
            this.logWarn("getVisibleWidth() unable to determine width - returning zero", 
                         "sizing");
            return 0;        
        }
        
        return element.offsetWidth;
        
    },
    
    getVisibleTitleWidth : function () {
        var element = this.isDrawn() && this.form
                                     ? isc.Element.get(this.form._getTitleCellID(this)) 
                                     : null;
        if (element == null) {
            this.logInfo("getVisibleTitleWidth() - unable to determine drawn width for this " +
                         "item - returning 0", "sizing");
            return 0;        
        }
        
        return element.offsetWidth;
    },
    
    //>	@method	formItem.getIconWidth()	(A)
	//	Takes an icon definition object, and returns the width for that icon in px.
	//		@group	sizing
    //      @param  icon (object)   icon definition object for this item.
	//		@return	(number)	width of the form item icon in px
    //      @visibility external    
	//<
    getIconWidth : function (icon) {
        // default to the first icon, if possible
        if (icon == null && this.icons != null && this.icons.getLength() > 0) icon = this.icons[0];
        else if (!this._isValidIcon(icon)) {
            this.logWarn("getIconWidth() passed invalid icon:" + isc.Log.echoAll(icon));
            return null;
        }
        
        // Note: we could actually look at the icon element in the DOM, (if it's drawn)
        // but we have full control over the HTML written into form item icons, so this value 
        // should always match the specified size for the icon.
        return (icon.width != null ? icon.width : this.iconWidth);

    },
    
    //> @method formItem.setHeight()    (A)
    // Set the height for this element
    // @group  sizing
    // @param    (int | String)    new height for the form element
    //<
    setHeight : function (height) {
        if("100%" == height) {
        	this.height = "*";    		
    	} else {
    		this.height = height;
    	}
        // redraw the item (default implementation notifies the container widget that the item
        // needs redrawing)
        this.redraw();
    },
    
    //> @method formItem.setWidth()    (A)
    // Set the width for this element
    // @group  sizing
    // @param    (int | String)    new width for the form element
    //<
    setWidth : function (width) {
        if("100%" == width) {
        	this.width = "*";    		
    	} else {
    		this.width = width;
    	}
        this.redraw();
    },

    //> @method formItem.setLeft()    (A)
    // For a form with +link{DynamicForm.itemLayout,itemLayout}:"absolute" only, set the left
    // coordinate of this form item.
    // <P>
    // Causes the form to redraw.
    // @visibility absForm
    //<
    setLeft : function (left) {
        this.left = left;
        this.redraw();
    },
    //> @method formItem.setTop()    (A)
    // For a form with +link{DynamicForm.itemLayout,itemLayout}:"absolute" only, set the top
    // coordinate of this form item.
    // <P>
    // Causes the form to redraw.
    // @visibility absForm
    //<
    setTop : function (top) {
        this.top = top;
        this.redraw();
    },
    
    //> @method formItem.moved()    (A)
    // The container widget is responsible for writing the HTML for form items into the DOM.
    // This is a notification function fired by the container items on form items when their
    // global position changes.
    //<
    moved : function () {
        // No default implementation - can be overridden / observed as required.
    },
    
    //> @method formItem.visibilityChanged()    (A)
    // The container widget is responsible for writing the HTML for form items into the DOM.
    // This is a notification function fired by the container items on form items when they are
    // hidden or shown on the page.  May be caused by parent show() / hide(), or calls to
    // showItem / hideItem.
    //<
    
    visibilityChanged : function () {
        // No default implementation - can be overridden / observed as required.
    },
    
    //> @method formItem.zIndexChanged()    (A)
    // The container widget is responsible for writing the HTML for form items into the DOM.
    // This is a notification function fired by the container items on form items when their
    // zIndex is modified on the page.  
    //<
    zIndexChanged : function () {
        // No default implementation - can be overridden / observed as required.
    },
    
    // HTML generation: element, errors, icons and hints
	// --------------------------------------------------------------------------------------------


    //> @method formItem.getInactiveEditorHTML()
    // This method returns a non-interactive HTML representation of this formItem
    // The HTML may be rendered multiple times on the same page and will not include 
    // standard unique DOM identifiers or error handling.
    // Used by ListGrids for +link{ListGrid.alwaysShowEditors} type functionality.
    // @param includeHint (boolean) passed through to getStandaloneItemHTML
    // @param includeErrrs (boolean ) passed through to getStandaloneItemHTML
    // @param [context] (any) optional arbitrary context for the inactive HTML. This will 
    //  allow us to associate a chunk of HTML with information about the calling code such
    //  as which cell this inactive HTML was written out into in a grid, etc.
    //<
    getInactiveEditorHTML : function (value, includeHint, includeErrors, context) {
        this._retrievingInactiveHTML = true;
        
        // call 'setupInactiveContext()' to generate a new 'inactiveContext' ID and
        // associate any passed in context with it.
        
        this._currentInactiveContext = this.setupInactiveContext(context);
        if (this.logIsDebugEnabled("inactiveEditorHTML")) {
            this.logDebug("getInactiveEditorHTML() called - context passed in:" + this.echo(context) +
                    " generated context ID:" + this._currentInactiveContext, "inactiveEditorHTML");
        }
        
        var HTML = this.getStandaloneItemHTML(value, includeHint, includeErrors);
        delete this._currentInactiveContext;
        delete this._retrievingInactiveHTML;
        return HTML;
    },
    
    // creates a directory of the 'inactiveHTML' contexts we were passed.
    // Returns a unique identifier by which the context was indexed. Also used
    // to create unique DOM IDs for inactive elements
    
    // Use an object rather than an array for the directory - easier to delete spots that
    // are no longer required.
    
    //_inactiveDirectory:null,
    _currentInactiveContextIDCount:1,
    setupInactiveContext : function (context) {
        
        if (context == null) context = {};
        if (this._isPrinting()) context.isPrintHTML = true;
        
        var ID = this._currentInactiveContextIDCount++;
        
        // store the ID directly on the context object so we can manage the directory with just
        // the context object to refer us back to where it's stored!
        context.inactiveContextID = ID;
        context.formItem = this;
        
        // This is important: Don't share a single object across form items
        // Also this._inactiveDirectory == null is a quick check for never
        // having rendered any inactive items 
        if (!this._inactiveDirectory) this._inactiveDirectory = {};
        this._inactiveDirectory[ID] = context;
        return ID;
    },
    
    // helper to delete inactive context?
    clearAllInactiveEditorContexts : function () {
        delete this._inactiveDirectory;
    },
    
    clearInactiveEditorContext : function (context) {
        if (isc.isAn.Object(context)) context = context.inactiveContextID;
        if (this._inactiveDirectory) delete this._inactiveDirectory[context];
        
    },
    
    // based on a live element in the DOM, determine which (if any) inactiveContext
    // it's associated with by looking at the ID
    
    _$inactiveContextParsingRegex:new RegExp(".*_inactiveContext(.*)$"),
    _getInactiveContextFromElement : function (element) {
        if (element && element.id != null && this._inactiveDirectory != null) {
            var id = element.id,
                partName = this._getDOMPartName(id);
            
            if (partName) {
                var inactiveContext = partName.match(this._$inactiveContextParsingRegex);
                if (inactiveContext) {
                    return this._inactiveDirectory[inactiveContext[1]];
                }
            }
        }
        return null;
    },
    // Are we retrieving inactive HTML?
    // This includes the HTML we'll render out into the print window, and 
    // the [what?]
    isInactiveHTML : function () {
        
        if (this.parentItem && this.parentItem.isInactiveHTML()) return true;
        return this._isPrinting() || this._retrievingInactiveHTML;
    },
    
    _isPrinting : function () {
        return this.containerWidget && this.containerWidget.isPrinting;
    },
    //> @method formItem.getStandaloneItemHTML()   (A)
    // This method returns the HTML for any form item not being written into a standard 
    // DynamicForm's table. It allows other widgets to embed form items in their HTML without
    // having to have a DynamicForm as a child.
    //      @group drawing
    //      @return         (HTML)      HTML output for this standalone item's element
    //      @visibility internal
    //<
    // For a widget (other than a DynamicForm) to embed form items, the other widget must
    // - Define a form for it's "standalone" items to belong to.
    // - set the 'containerWidget' property on the form item[s] it wants to write out
    // - use this method to get the HTML for the form item
    // - call 'drawn()' when the HTML for the item has been written out
    // - call 'cleared()' when the HTML for the item is removed from the DOM
    // - call 'redrawn()' when the HTML for the item is re-written in the DOM
    // This is used by the ListGrid to show the edit form items within cells.
    
    _$absDivStart:"<DIV STYLE='position:absolute;left:",
    _$pxSemiTopColon:"px;top:",
    _$pxSemiWidthColon:"px;width:",
    _$pxSemiHeightColon:"px;height:",
    _$pxSemiIDEquals:"px;' ID='",
    _$quoteRightAngle:"'>",
    _$absDivEnd:"</DIV>",
    
    _$standaloneStartTemplate:[
        "<SPAN style='white-space:nowrap;' eventProxy=",        // [0]
        ,                                                       // [1] formID
            // this 'containsItem' property may be used to determine which
            // form item events (passed to the form) occurred over.
            
        " " + isc.DynamicForm._containsItem + "='",                                      // [2]
        ,                                                       // [3] itemID
        "' ID='",                                               // [4]
        ,                                                       // [5] ID for span
        "'>"                                                    // [6]
    ],
    _$standaloneEnd:"</SPAN>",
    _$standaloneSpan:"_standaloneSpan",
    getStandaloneItemHTML : function (value, includeHint, includeErrors) {
        // Write a span around the item with this form's ID as the eventProxy -- this ensures
        // that events are handled by the form rather than whatever the next parent canvas is
        var output = isc.SB.create(),
            form = this.form;
        
        // output a <SPAN> so the EventHandler recognizes which form this item belongs to
        if (form) {
            if (this._absPos()) {
                var left = this._getPercentCoord(this.left), 
                    top = this._getPercentCoord(this.top, true),
                    width = this.getInnerWidth(),
                    height = this.getInnerHeight();
                if (!isc.isA.Number(left)) left = 0;
                if (!isc.isA.Number(top)) top = 0;
                output.append(this._$absDivStart);
                output.appendNumber(left);
                output.append(this._$pxSemiTopColon);
                output.appendNumber(top);
                
                if (isc.isA.Number(width)) {
                    output.append(this._$pxSemiWidthColon);
                    output.appendNumber(width);
                }
                if (isc.isA.Number(height)) {
                    output.append(this._$pxSemiHeightColon);
                    output.appendNumber(height);
                }
                output.append(this._$pxSemiIDEquals, this._getAbsDivID(), this._$quoteRightAngle);
            }
            
            var template = this._$standaloneStartTemplate,
                formID = form.getID(),
                itemID = this.getID();
            
            template[1] = formID;
            template[3] = itemID;
            
            template[5] = this._getDOMID(this._$standaloneSpan);

            output.append(template);


                        
            output.append(this.getInnerHTML(value, includeHint, includeErrors, true));
            output.append(this._$standaloneEnd);
            
            if (this._absPos()) {
                output.append(this._$absDivEnd); 
            }
        }
        // return and relese the buffer so it can be reused  
        return output.release();
    },
    
    _$absDiv:"_absDiv",
    _getAbsDivID : function () {
        return this._getDOMID(this._$absDiv);
    },
    
    // cache the absolute div (when 'cleared()' fires this will get cleared)
    getAbsDiv : function () {
        if (this._absDiv) return this._absDiv;
        if (!this.isDrawn()) return;
        this._absDiv = isc.Element.get(this._getAbsDivID());
        return this._absDiv;
    },

    _hasExternalIcons : function () {
        var icons = this.icons;
        if (!icons) return false;
        for (var i = 0; i < icons.length; i++) {
            if (!icons[i].writeIntoItem) return true; // found external icon
        }
        return false; // all icons internal
    },

    // -- Disabled item event mask --

    // In some browsers (seen in Moz), native mouse events (such as mousemove) are not generated
    // when the user moves over disabled native form item elements. 
    // If this form item is disabled, we write out a div floating over the native form item
    // so we can still get native mouse events and respond by showing hovers, etc.
    //
    // In IE, we get bogus native events (event.srcElement is an object that can't be
    // enumerated - crashes browser and all event properties produce error if poked) when the
    // mouse is over disabled text in textitems that can be fixed with this workaround
    useDisabledEventMask : function () {
        return ((isc.Browser.isMoz && this.hasDataElement()) ||
                (isc.Browser.isIE && isc.isA.TextItem(this))) &&
               this.getHeight() != null;
    },

    
    _eventMaskTemplate:[
        "<DIV isDisabledEventMask='true' style='overflow:hidden;position:absolute;width:",
        null,   // 1 width
        "px;height:",
        null,   // 3 height
        //"px;border:1px solid red;' containsItem='", 
        "px' " + isc.DynamicForm._containsItem + "='",
        null,   // 5 item id
        "' " + isc.DynamicForm._itemPart + "='" + isc.DynamicForm._element + "' ID='",
        ,       // 7 ID for the element - so we can easily clear it from the DOM
        "'>",
        null,   // 9 spacerHTML - we'll lazily add a spacer here, otherwise a &nbsp; - 
                // this needs to be lazy to avoiding trying to load blank.gif before the skin 
                // is loaded
                
        "</DIV>"
    ],
    _getEventMaskHTML : function () {
        var template = this._eventMaskTemplate;
        template[1] = this._getEventMaskWidth();
        template[3] = this.getHeight();
        template[5] = this.getItemID();
        template[7] = this._getDOMID("eventMask");
        template[9] = this._getEventMaskSpacerHTML();

        return template.join(isc.emptyString);        
    },

    _getEventMaskSpacerHTML : function () {
        return isc.Canvas.spacerHTML(1600, 100)
    },

    _getEventMaskElement : function () {
        return isc.Element.get(this._getDOMID("eventMask"));
    },

    
    
    
    _getEventMaskWidth : function () {
        var width = this.getElementWidth();
        if (width == null) {
            if (isc.RadioItem && isc.isA.RadioItem(this) && this.parentItem != null) {
                width = this.parentItem.getElementWidth();
            }

            if (width == null) return 0;
            
        } else {     
            // Shrink to account for error icons if necessary
            if (this.form.showInlineErrors && this.hasErrors()
                     && this.getErrorOrientation() == isc.Canvas.LEFT) 
            {
                width -= this.getErrorWidth();
            }
        }
        return width;
    },
    
    // Browser spell checking
    // Supported on 
    //  Moz Firefox 2.0 beta2 and above
    //  Safari (tested on 5.0)
    //  Chrome (tested on 5.0.375)
    //  Safari/iPad and Safari/iPhone
    // Untested on IE
    // Note: if browserSpellCheck is unset, we pick it up from the containing form item
    
    getBrowserSpellCheck : function () {
        if (this.browserSpellCheck != null) return this.browserSpellCheck;
        return this.form.browserSpellCheck;
    },
    
    // -- Hidden data element --
    
    // If this is an item with no native form element, but this form's value is being 
    // submitted directly to the server, we are going to need a hidden item in the form to 
    // represent its value.
    _useHiddenDataElement : function () {
        return (this.shouldSaveValue && !this.hasDataElement() && this.shouldSubmitValue());
    },
    
    // HTML for the hidden data element
    _$hiddenDataElement:"hiddenDataElement",
    _getHiddenDataElementID : function () {
        return this._getDOMID(this._$hiddenDataElement);
    },
    _getHiddenDataElement : function () {
        return this._getHTMLPartHandle(this._$hiddenDataElement);
    },
    
    _getHTMLPartHandle : function (part) {
        if (!this.isDrawn()) return null;
        
        if (!this._htmlPartHandles) this._htmlPartHandles = {};
    
        // Note: we free up this cache on 'cleared()' / 'redrawn()'
        var handle = this._htmlPartHandles[part];
        if (handle == null) {
            handle = isc.Element.get(this._getDOMID(part));
            if (handle != null) this._htmlPartHandles[part] = handle;
        }
        return handle;
    },
    
    _$control:"control",
    _getControlTableID : function () {
        return this._getDOMID(this._$control);
    },
    _getControlTableElement : function () {
        return this._getHTMLPartHandle(this._$control);
    },
    
    _$textBox:"textBox",
    _getTextBoxID : function () {
        return this._getDOMID(this._$textBox);
    },
    _getTextBoxElement : function () {
        if (this.hasDataElement() && this._dataElementIsTextBox) return this.getDataElement();
        return this._getHTMLPartHandle(this._$textBox);
    },


    _$pickerIconCell:"pickerIconCell",
    _getPickerIconCellID : function () {
        return this._getDOMID(this._$pickerIconCell);
    },
    _getPickerIconCellElement : function () {
        return this._getHTMLPartHandle(this._$pickerIconCell);
    },
      
    
    _getHiddenDataElementHTML : function () {
        return "<INPUT type='hidden' name='" + 
                this.getFieldName() + "' ID='" + this._getHiddenDataElementID() + "'>";
    },
    
    _$hintCell:"hintCell",
    _getHintCellID : function () {
        return this._getDOMID(this._$hintCell);
    },
    _getHintCellElement : function () {
        return this._getHTMLPartHandle(this._$hintCell);
    },

    //> @method formItem.updateState() [A]
    // Update the visual state of a FormItem to reflect any changes in state or any changes in
    // style settings (eg +link{formItem.textBoxStyle}). 
    // <P>
    // Calls to <code>updateState()</code> normally occur automatically as a consequence of
    // focus changes, items becoming disabled, etc.  This method is advanced and intended only
    // for use in workarounds.
    //
    // @visibility external
    //<
    
    _$FormItemStyling:"FormItemStyling",
    updateState : function () {
        
        if (!this.isDrawn()) return;
        
        var showDebugLogs = this.logIsDebugEnabled(this._$FormItemStyling);
        
        // elements to style:
        // - cell
        
        if (this.containerWidget == this.form && !this._absPos()) {
            var cellStyle = this.getCellStyle();
            if (showDebugLogs) this.logDebug("About to apply basic cell style:"+ cellStyle, "FormItemStyling");
        
            // We'll either have a form cell around us, or we'll have written out an abolutely positioned
            // div
            var formCell = this.getFormCell();
            if (formCell) formCell.className = cellStyle;
            // If we have an outer table element we also apply the overall cellstyle to that
            var outerTable = this.getOuterTableElement();
            if (outerTable) outerTable.className = cellStyle;
            
            // Tell the form to update our title cell's state too.
            if (this.showTitle) this.form.updateTitleCellState(this);
        }
      
        if (this._shouldShowPickerIcon()) {
            var controlStyle = this.getControlStyle(),
                pickerIconStyle = this.getPickerIconStyle();
                
                if (showDebugLogs) {
                    this.logDebug("About to apply cell styles to control box and picker icon cell:"+
                                    [controlStyle, pickerIconStyle], "FormItemStyling");
                                    
                }

            // - inner table (control style)
            var controlHandle = this._getControlTableElement();
            if (controlHandle) controlHandle.className = controlStyle;
            // - pickerIconBox
            var pickerIconHandle = this._getPickerIconCellElement();
            if (pickerIconHandle) pickerIconHandle.className = pickerIconStyle;
        }

        // - text box
        var textBoxStyle = !this._showingInFieldHint ? 
                                this.getTextBoxStyle() : this._getInFieldHintStyle();
        if (showDebugLogs) this.logDebug("About to apply text box style:"+ textBoxStyle, "FormItemStyling");

        var textBoxHandle = this._getTextBoxElement();
        if (textBoxHandle) {
            textBoxHandle.className = textBoxStyle;
            if (this.getImplicitSave()) {
                var cssObj = textBoxHandle.style;
                if (this.awaitingImplicitSave) {
                    if (cssObj && this._implicitSaveCSS != true) {
                        this._implicitSaveCSS = true;
                        this._oldCssText = "" + cssObj.cssText;
                        cssObj.cssText = "" + cssObj.cssText + this.editPendingCSSText;
                    }
                } else {
                    if (this.wasAwaitingImplicitSave == true && this._oldCssText) {
                        delete this._implicitSaveCSS;
                        delete this.wasAwaitingImplicitSave;   
                        cssObj.cssText = "" + this._oldCssText;                          
                        delete this._oldCssText;
                    }
                }
            }
        }
        
        
        if (this._writeOutFocusProxy() && textBoxHandle) {
            if (!this._focusOutline) {
                // Size the focus outline to match this item's text box size, adjusted for
                // the fact that we always write out a 1px border
                var width = this.getTextBoxWidth(), height = this.getTextBoxHeight();
                width += isc.Element.getHBorderSize(textBoxHandle) -2;
                if (height != null) height += isc.Element.getVBorderSize(textBoxHandle) -2;
                var focusOutlineID = this._getDOMID("focusOutline");
                isc.Element.insertAdjacentHTML(
                    textBoxHandle,
                    "beforeBegin",
                    "<DIV ID='" +  focusOutlineID + 
                    (this.textBoxStyle ? "' CLASS='" + this.textBoxStyle +  "Focused'" : "'") + 
                    " STYLE='background-image:none;background-color:transparent;position:absolute;width:" 
                    +
                    width +
                    (height == null ? "px;" : "px;height:" + height) + 
                    "px;visibility:hidden;border:1px dotted white;z-index:100;'>&nbsp;</DIV>"
                );
                this._focusOutline = isc.Element.get(focusOutlineID);
            }
            
            if (this.hasFocus) this._focusOutline.style.visibility = "inherit";
            else this._focusOutline.style.visibility = "hidden";
        }
        
        
    },
    
    // We have a number of deprecated classNames as of 5.5 - helper method to log warnings
    _$deprecated:"deprecated",
    _warnDeprecated : function (oldPropertyName, newPropertyName, version) {
        if (!this.logIsInfoEnabled(this._$deprecated)) return;
        // Keep track of which property names we've already warned about on this item.
        if (!this._warnedDeprecated) this._warnedDeprecated = {};
        if (this._warnedDeprecated[oldPropertyName] == true) return;

        if (version == null) version = "5.5";
        var logString = isc.SB.create();
        logString.append(
            "Using '", oldPropertyName, "': ", this[oldPropertyName], 
            " to style this form item.  This property is deprecated as of SmartClient Version ",
            version, " - we recommend removing this property and using '", newPropertyName, "' instead.");
        this.logInfo(logString.release(), "deprecated");
        
        this._warnedDeprecated[oldPropertyName] = true;
    },
    
	//>	@method	formItem.getInnerHTML()		(A)
	//	Return the HTML for this formItem's element(s) and icons.
	//		@group	drawing
	//
	//		@param	value	(string)	Value of the element.
	//		@return			(HTML)	HTML output for this element
	//<
    
    getInnerHTML : function (value, includeHint, includeErrors, returnArray) {
        // Inactive content: such as printHTML:
        // If we're marked as inactive while getting innerHTML set the _currentInactiveContext
        // flag if it hasn't been set already
        // This ensures we generate unique DOMIDs for inactive content which
        // is separate from the default DOMIDs for our active HTML elements on the page.
        // Note: may have already been set up / mapped to an explicit 'context' object via
        // setupInactiveContext() - we do this in getInactiveHTML(). In this case respect the
        // existing context / contextID
        var clearInactiveContext, clearPInactiveContext;
        if (this.isInactiveHTML() && this._currentInactiveContext == null) {
            clearInactiveContext = true;
            // If our parent is inactive pick up the same 'inactiveContext' object.
            
            var parentContext, parentItem = this.parentItem;
            if (parentItem != null && parentItem.isInactiveHTML()) {
                if (parentItem._currentInactiveContext == null) {
                    
                    parentItem.setupInactiveContext();
                    clearPInactiveContext = true;
                }
                parentContext = parentItem._inactiveDirectory[parentItem._currentInactiveContext];
            }
            this._currentInactiveContext = this.setupInactiveContext(parentContext);
            
            if (this.logIsDebugEnabled("inactiveEditorHTML")) {
                this.logDebug("getInnerHTML(): Item is marked as inactive - set up " +
                    "new inactive context ID:" + this._currentInactiveContext,
                    "inactiveEditorHTML");
            }
        }
        
        
        this._gotHintHTML = includeHint && !this._getShowHintInField();
            
        var output;

        // If we need to write out a hidden native data element, do so now.        
        if (this._useHiddenDataElement()) {
            if (!output) output = isc.SB.create();
            output.append(this._getHiddenDataElementHTML());
        }

        // If displaying hint in-field, suppress displaying hint in surrounding table.
        if (this._getShowHintInField()) includeHint = false;

        // Note that the tableHTML is an array
        var tableHTML = this._getTableHTML(value, includeHint, includeErrors);

        
        var returnVal;
        
        if (output != null) {
            output.append(tableHTML);
            if (returnArray) {
                
                returnVal = output.getArray().duplicate();
                // pass true here to suppress the normal "toString()" / return from 
                // StringBuffer.release, which is a small optimization
                output.release(true);
            } else {
                returnVal = output.release();
            }
        } else {
            returnVal = (returnArray ? tableHTML : tableHTML.join(isc.emptyString));
        }

        // If we set the _currentInactiveContext flag, clear it now.
        if (clearInactiveContext) delete this._currentInactiveContext;
        if (this.parentItem && clearPInactiveContext)
            delete this.parentItem._currentInactiveContext;
        return returnVal;
    },
    
    _writeOuterTable : function (includeHint, hasLeftRightErrors) {
        if (hasLeftRightErrors) return true;
        
        if (includeHint && this.getHint() != null) return true;
        
        if (this.icons && this.icons.length > 0 && !this.renderAsStatic()) return true;
    },

    // _getValueIcon()
    // Returns the URL for the value icon to show for this cell, or null if there is none.
    // Checks for the presence of this.getValueIcon, or this.valueIcons
     _$Over:"Over", _$Down:"Down", _$Disabled:"Disabled",
    _getValueIcon : function (value) {
        
        if (this.suppressValueIcon) return null;
        
        var icon,
            undef;
        if (value === undef) value = this.getValue();
        if (this.getValueIcon) icon = this.getValueIcon(value);
        // Default behavior
        else {
            if (value == null) icon = this.emptyValueIcon;
            else if (this.valueIcons != null) icon = this.valueIcons[value];
        }
        
        // We may (and commonly do) just not have a valueIcon
        if (icon == null) return null;
        
        // We need to be able to show over, disabled, focused and 'mouseDown' state 
        // Required for the CheckboxItem
        // This is done independently of the cell style applied to the item's text.
        
        var newState = ((this.isDisabled() || this.isReadOnly()) && 
                        this.showValueIconDisabled ? this._$Disabled : this._iconState);
        if (newState != null) {
            
            // Use caching to speed up image-name generation
            if (!isc.CheckboxItem._valueIconStateCache) isc.CheckboxItem._valueIconStateCache = {};
            var cacheObject = isc.CheckboxItem._valueIconStateCache[icon];
        
            if (!cacheObject) {
                cacheObject = {};
                cacheObject.Over = isc.Img.urlForState(icon, false, false, this._$Over);
                cacheObject.Down = isc.Img.urlForState(icon, false, false, this._$Down);
                cacheObject.Disabled = isc.Img.urlForState(icon, false, false, this._$Disabled);
                
                isc.CheckboxItem._valueIconStateCache[icon] = cacheObject;
            }
            
            icon = cacheObject[newState];
        }
        
        return icon;    
    },

    // _getValueIconHTML - returns the IMG tag to write out as our valueIcon
    // or null if we're not showing a valueIcon
    _$valueIcon:"valueIcon",
    _getValueIconHTML : function (value) {
        var valueIcon = this._getValueIcon(value);
        if (valueIcon == null) {
            return isc.emptyString;
        }

        var prefix = this.imageURLPrefix || this.baseURL || this.imgDir,
            suffix = this.imageURLSuffix;
        if (suffix) valueIcon = valueIcon + suffix;

        var valueIconWidth = this.getValueIconWidth();
        var valueIconHeight = this.getValueIconHeight();

        var isRTL = this.isRTL(),
            valueIconLeftPadding = isRTL ? this.valueIconRightPadding : this.valueIconLeftPadding,
            valueIconRightPadding = isRTL ? this.valueIconLeftPadding : this.valueIconRightPadding;

        
        return isc.Canvas._getValueIconHTML(valueIcon, prefix, valueIconWidth, valueIconHeight,
                                            valueIconLeftPadding, valueIconRightPadding,
                                            this._getDOMID(this._$valueIcon));
    },

    // method to get a pointer to the valueIcon img element
    _getValueIconHandle : function () {
        if (!this.isDrawn()) return null;
        var img = isc.Element.get(this._getDOMID(this._$valueIcon));
        return img;
    },

    _$outerTableEnd:"</TD></TR></TABLE>",

    _$controlTableTemplate:[
       // Control table
       "<TABLE role='presentation' ID='",        // 0
       ,                     // 1 [ID for the table] this._getControlTablID()

       // By marking the control table with the 'itemPart' attributes we simplify determining
       // what "part" of the item received the event.
       "' " + isc.DynamicForm._containsItem + "='",                // 2
       ,                                                           // 3 [formItem ID]
       "' " + isc.DynamicForm._itemPart + "='" + isc.DynamicForm._controlTableString,       // 4
       "' CELLPADDING=0 CELLSPACING=0 STYLE='",                     // 5
       ,                    // 6 [css text for the control table]
       "' CLASS='",         // 7
       ,                    // 8 [control table className]
       
       // Text box cell
       "'><TR><TD style='", // 9
       ,                    // 10 [css text for textBox cell]
       "'>",                // 11

       // Text
       ,                    // 12 [textBox html]
       
       "</TD><TD ID='",     // 13
       ,                    // 14 [picker icon cell ID]
       "' CLASS='",         // 15
       ,                    // 16 [Picker Icon className]
       "' STYLE='",         // 17
       ,                    // 18 [picker icon css]
       "'>",                // 19
       ,                    // 20 [Picker Icon HTML]                
       "</TD></TR></TABLE>"
    ],
    
    _$hintCellTemplate : ["</TD><TD ID='", // 0
                            ,              // 1 this._getHintCellID()
                            "' CLASS='",   // 2
                            ,              // 3 this.getHintStyle() 
                            "'>"           // 4
                                           // 4 hint
    ],
    
    // helper method to return the HTML for the form item's outer element.
    
    
    // Returns an array of strings that form the appropriate HTML.
    

    
    _getTableHTML : function (value, includeHint, includeErrors) {
        var errorOrientation = this.getErrorOrientation(),
            showErrors,
            errorOnLeft = errorOrientation == isc.Canvas.LEFT,
            errorHTML,
            readOnly = this.isReadOnly()
        ;
        if (includeErrors && 
            (errorOnLeft || errorOrientation == isc.Canvas.RIGHT)) 
        {
            var errors = this.getErrors();
            if (errors) {
                showErrors = true;
                errorHTML = this.getErrorHTML(errors);
            }
        }

        var vAlign = this.iconVAlign,
            displayValue = this.mapValueToDisplay(value),
            writeOuterTable = this._writeOuterTable(includeHint, showErrors),
            writeControlTable = this._shouldShowPickerIcon();
        ;

        var template = writeOuterTable ? isc.FormItem._getOuterTableStartTemplate() : [];
        if (writeOuterTable) {

            
            template.length = 13;

            template[1] = this._getOuterTableID();
            template[3] = this.getOuterTableCSS();
            // Apply the cell style to the outer table so (EG) font color / weight get inherited
            // Note that we don't write out the 'cellStyle' at all unless the item is
            // written into a DF cell
            if (this.containerWidget == this.form && !this._absPos()) {            
                template[5] = this.getCellStyle();
            } else {
                template[5] = null;
            }

            // If we show the error on the left this is where we output it...
            if (showErrors && errorOnLeft) {
                template[7] = isc.StringBuffer.concat("<TD STYLE='",
                                isc.Canvas._$noStyleDoublingCSS, "' CLASS='", 
                                this.getCellStyle(), "'>", errorHTML, "</TD>");
            } else template[7] = null;

            // If the first cell of the outer table contains the text box, write out the 
            // appropriate css text            
            if (!writeControlTable) template[9] = this.getTextBoxCellCSS();
            else template[9] = isc.Canvas._$noStyleDoublingCSS;
            // First cell            
            template[11] = vAlign;
        }

        // If the element is disabled, in some browsers we write an event mask over it
        // to capture mouse events.
        // This is required because we don't get any mouse events (at a native level) over 
        // disabled form item elements.     
        if ((this.isInactiveHTML() || this.isDisabled()) && this.useDisabledEventMask()) {
            template[template.length] = this._getEventMaskHTML();
        }
        
        // Logic is quite different for showing a picker icon vs not showing a picker icon.
        if (!writeControlTable) {
            // write the element HTML (text box) directly into the outer table's first cell
            
            // Note - if we are showing a valueIcon, it will be included in the HTML returned
            // from getElementHTML()
            template[template.length] = (readOnly ? this.getReadOnlyHTML(displayValue, value)
                                                  : this.getElementHTML(displayValue, value));
        } else {
            
            var pickerIconStyle = this.getPickerIconStyle(),
                itemID = this.getID(),
                controlStyle = this.getControlStyle(),
                controlTemplate = this._$controlTableTemplate,
                controlHandleID = this._getControlTableID(),
                textBoxID = this._getTextBoxID(),
                pickerCellID = this._getPickerIconCellID()
            ;

            controlTemplate[1] = controlHandleID;
            controlTemplate[3] = itemID;
            controlTemplate[6] = this.getControlTableCSS();
            // If no control table style was explicitly specified, pick up the style for the
            // DF cell containing this item (as it will not cascade up through the table element
            
            if (controlStyle == null && this.containerWidget == this.form && !this._absPos()) {
                controlTemplate[8] = this.getCellStyle();
                controlTemplate[6] += isc.Canvas._$noStyleDoublingCSS;
            } else {
            	controlTemplate[8] = controlStyle
            }
            controlTemplate[10] = this.getTextBoxCellCSS();
            controlTemplate[12] = (readOnly ? this.getReadOnlyHTML(displayValue, value)
                                            : this.getElementHTML(displayValue, value));
            controlTemplate[14] = pickerCellID;
            controlTemplate[16] = pickerIconStyle;
            controlTemplate[18] = this.getPickerIconCellCSS();
            var PI = this.getPickerIcon(),
                showPIFocus = PI && this.hasFocus && this.showFocusedPickerIcon &&
                                (PI.showFocusedWithItem != false); 
            controlTemplate[20] = this.getIconHTML(this.getPickerIcon(), showPIFocus);

            // Actually write out the control table in the cell
            for (var i = 0; i < controlTemplate.length; i++) {
                template[template.length] = controlTemplate[i];
            }
        }

                
        if (writeOuterTable) {
            if (this._hasExternalIcons()) {

                var iconsTemplate = isc.FormItem._getIconsCellTemplate();
                iconsTemplate[1] = vAlign;
                iconsTemplate[3] = this.getTotalIconsWidth();
                iconsTemplate[5] = this.iconHeight;
                iconsTemplate[7] = this.getCellStyle();
                iconsTemplate[9] = this.getIconCellID();
                // that actually gives back the HTML for each icon.
                iconsTemplate[11] = this.getIconsHTML();

                for (var i = 0; i < iconsTemplate.length; i++) {
                    template[template.length] = iconsTemplate[i];
                }
            }

            var showRightError = (showErrors && !errorOnLeft);
            var hint;
            if (includeHint) {
                hint = this.getHint();
                if (isc.isA.emptyString(hint)) hint = null;
            }
            if (hint || showRightError) {
                var hintCellTemplate = this._$hintCellTemplate;
                hintCellTemplate[1] = this._getHintCellID();
                hintCellTemplate[3] = hint ? this.getHintStyle() : null;
                hintCellTemplate[5] = (hint || "") + (showRightError ? errorHTML || "" : "");
                for (var i = 0; i < hintCellTemplate.length; i++) {
                    template[template.length] = this._$hintCellTemplate[i];
                }
            }
            // close the table
            template[template.length] = this._$outerTableEnd;
        }

        return template;
	},
	
	_$iconCell:"iconCell",
	getIconCellID : function () {
	    return this._getDOMID(this._$iconCell);
	},
    
    _$outerTable:"_outerTable",
    _getOuterTableID : function () {
        return this._getDOMID(this._$outerTable);
    },

    // Retrieving Stylenames
    // --------------------------------------

    // Helper to get style name from base style based on current state
    _getCellStyle : function (baseStyle) {
        var hasErrors = this.hasErrors(),
            rtl = this.showRTL && this.isRTL();

        // Use caching to speed up style-name generation
        var cacheObject;
        if (rtl) {
            cacheObject = isc.FormItem._rtlCellStyleCache[baseStyle];
            if (!cacheObject) {
                cacheObject = isc.FormItem._cellStyleCache[baseStyle] = {
                    Normal: baseStyle + "RTL",
                    Error: baseStyle + "ErrorRTL",
                    ErrorFocused: baseStyle + "ErrorFocusedRTL",
                    Focused: baseStyle + "FocusedRTL",
                    Disabled: baseStyle + "DisabledRTL"
                };
            }
        } else {
            cacheObject = isc.FormItem._cellStyleCache[baseStyle];
            if (!cacheObject) {
                cacheObject = isc.FormItem._cellStyleCache[baseStyle] = {
                    Normal: baseStyle,
                    Error: baseStyle + "Error",
                    ErrorFocused: baseStyle + "ErrorFocused",
                    Focused: baseStyle + "Focused",
                    Disabled: baseStyle + "Disabled"
                };
            }
        }

        // if we have an error always just return the error state
        if (hasErrors && this.shouldShowErrorStyle() && this.form.showInlineErrors) {
            return this.showFocusedErrorState && this.hasFocus && !this.isInactiveHTML() ? 
                cacheObject.ErrorFocused : cacheObject.Error;
        } else {
            // suppress focused styling when inactive
            if (this.showFocused && this.hasFocus && !this.isInactiveHTML()) 
                return cacheObject.Focused;
            if (this.showDisabled && this.isDisabled()) return cacheObject.Disabled;
            // Otherwise "normal" state.
            return cacheObject.Normal;
        }
    },

    //>@method  FormItem.getCellStyle() (A)
    // Function to retrieve the css style class name to apply to this form item's cell.
    // Derives the style name from <code>this.cellStyle</code>
    // @return CSSStyleName css class to apply to the cell
    //<
    // In some cases we apply the base cells tyle to sub items within the cell. In this case
    // avoid logging warnings if the deprecated styling property attributes are set, so we
    // don't warn repeatedly per rendered item.
    getCellStyle : function () {
    
        // For items written into a container item, allow the container item to override the
        // cellStyle, so it can re-skin it's child items effectively
        if (this.parentItem != null) {
            if (this.parentItem.itemCellStyle) return this._getCellStyle(this.parentItem.itemCellStyle);
        }
    
        var className = this._getCellStyle(this.cellStyle);
        //>!BackCompat 2006.3.9
        // If the  old styling properties are set have them take precedence over the 
        // new  style names since new names will typically be present from the skin files,
        // but old app code will not know about the new names
        if (!this.hasErrors()) {
            // If the deprecated 'cellClassName' property is set, use that
            if (this.cellClassName != null) {
                this._warnDeprecated("cellClassName", "cellStyle");
                className = this.cellClassName;
            }
        } else {
            // If the deprecated 'errorCellClassName' proeprty is set, use that
            if (this.errorCellClassName != null) {
                this._warnDeprecated("errorCellClassname", "cellStyle");
                className = this.errorCellClassName;
            }
        }
        //<!BackCompat
        return className;
    },

    //>@method  FormItem.getTitleStyle() (A)
    // Function to retrieve the css style class name to apply to this form item's title cell.
    // Derives the style name from <code>this.titleStyle</code>
    // @return CSSStyleName css class to apply to the cell
    //<    
    getTitleStyle : function () {
        // If we are printing default to this.printTitleStyle if specified
        
        if (this._isPrinting() && this.printTitleStyle) {
            return this._getCellStyle(this.printTitleStyle); 
        } 
        var error = this.getErrors();
        if (error == isc.emptyString) error = null;
        var className = this._getCellStyle(this.titleStyle);
        //>!BackCompat 2006.3.9
        if (!error) {
            // If the deprecated 'titleClassName' property is set, use that
            if (this.titleClassName != null) {
                this._warnDeprecated("titleClassName", "titleStyle");
                className = this.titleClassName;
            }
        } else {
            // If the deprecated 'titleErrorClassName' proeprty is set, use that
            if (this.titleErrorClassName != null) {
                this._warnDeprecated("titleErrorClassName", "titleStyle");
                className = this.titleErrorClassName
            }
        }        
        //<!BackCompat
        return className;
        
    },
    
    //>@method  FormItem.getHintStyle() (A)
    // Function to retrieve the css style class name to apply to this form item's hint text
    // Derives the style name from <code>this.hintStyle</code>
    // @return CSSStyleName css class to apply to the cell
    //<
    getHintStyle : function () {
            
        //>!BackCompat 2006.3.9
        if (this.hintClassName != null) {
            this._warnDeprecated("hintClassName", "hintStyle");
            return this.hintClassName;
        }
        //<!BackCompat
        if (this.hintStyle != null) return this.hintStyle;
        
    },

    // The text box is the element that we write into the first cell of the table control
    // table which contains the textual value of the form item.
    // This is written out by this.getElementHTML and by default is a DIV.
    
    getTextBoxStyle : function () {
        if (this._isPrinting() && this.printTextBoxStyle) {
            return this._getCellStyle(this.printTextBoxStyle);
        }

        // use the readOnlyTextBoxStyle with canEdit: false and readOnlyDisplay: "static"
        var tbStyle = (this.getCanEdit() == false && this.renderAsStatic() ? 
                this.getReadOnlyTextBoxStyle() : this.textBoxStyle),
            styleName = this._getCellStyle(tbStyle)
        ;

        //>!BackCompat 2006.3.9
        // deprecated input element style
        if (this.elementClassName != null) {
            this._warnDeprecated("elementClassName", "textBoxStyle");
            styleName = this.elementClassName;
        } 
        //<!BackCompat

        return styleName;
    },

    // Styling applied to the table cell containing the picker icon (if we're showing one)
    getPickerIconStyle : function () {
        if (this.pickerIconStyle != null) return this._getCellStyle(this.pickerIconStyle);
        // allow styling to be inherited from our parent table
        return null;
    },
    
    // Styling applied to the 'control' table - only rendered if we're showing a picker icon - 
    // contains the main text box and the picker icon.
    getControlStyle : function () {
        if (this.controlStyle != null) return this._getCellStyle(this.controlStyle);
        return null;
    },      
    
    // CSS Generation
    // -----------------
    // Method to return custom CSS styling for various parts of the form item

    _$wrapCSS:"white-space:normal;",_$nowrapCSS:"white-space:nowrap;",
    _$minWidthColon:"min-width:", _$minHeightColon:"min-height:",
    _$widthColon:"width:", _$heightColon:"height:", _$pxSemi:"px;", _$semi:";",
    
    _$cachedOuterTableCSS:{},
    getOuterTableCSS : function () {
        
        return this._$wrapCSS;
    },

    // Control table
    
    // Retrieve style text to apply to the controlbox table, if we're writing one out.
    _$defaultCursor:"cursor:default;",
    getControlTableCSS : function () {
        var output = isc.SB.create();
        output.append(this._$defaultCursor);
        
        // The control-table should be sized to the 'innerWidth', minus the size of any
        // external icons. This is currently available as this.getElementWidth()
        var width = this.getElementWidth() - this._getErrorWidthAdjustment();
        if (isc.isA.Number(width)) output.append(this._$widthColon, width, this._$pxSemi);
        
        // no need to specify height - we will pick this up from the text box element

        return output.release();
    },

    // Text Box Cell

    // Apply no-style-doubling css to the cell containing the text box. This will prevent
    // globally applied "td" styles from showing up around items with hints / checkboxes etc
    // May be overridden by subclasses
    
    getTextBoxCellCSS : function () {
        return this.textBoxCellCSS != null ? this.textBoxCellCSS : isc.Canvas._$noStyleDoublingCSS;
    },

    // Retrieve style text to apply directly to the text box
    _$textOverflowEllipsisCSS:"overflow:hidden;" + isc.Browser._textOverflowPropertyName + ":ellipsis;",
    _$textAlignColon:"text-align:",
    _$lineHeightColon:"line-height:",
    _$borderBox:"border-box",
    _$mozBoxSizingColon:"-moz-box-sizing:",
    _$webkitBoxSizingColon:"-webkit-box-sizing:",
    _$boxSizingColon:"box-sizing:",
    getTextBoxCSS : function () {
        var output = isc.SB.create(),
            isPrinting = this._isPrinting();

        
        if (!isPrinting || isc.isA.Number(this.width)) {
            var elementWidth = this.getTextBoxWidth();        
            if (isc.isA.Number(elementWidth)) {
                if ((isc.Browser.isOpera
                     || isc.Browser.isMoz
                     || isc.Browser.isSafari
                     || isc.Browser.isIE9) && !this.clipValue) {
                    output.append(this._$minWidthColon, elementWidth, this._$pxSemi);
                } else {
                    output.append(this._$widthColon, elementWidth, this._$pxSemi);
                }
            }
        }

        var height = this.getTextBoxHeight();
        if (isc.isA.Number(height)) {
            if (!isPrinting && isc.Browser.isMoz && !this.clipValue) {
                output.append(this._$minHeightColon, height, this._$pxSemi);
            } else {
                output.append(this._$heightColon, height, this._$pxSemi);

                
                if (isPrinting) output.append(this._$lineHeightColon, height, this._$pxSemi);
            }
        }

        // Don't allow overflow if clipValue is true.
        if (this.clipValue) output.append(this._$textOverflowEllipsisCSS);

        if (this.wrap) output.append(this._$wrapCSS) 
        else output.append(this._$nowrapCSS);

        if (this.textAlign != null) {
            output.append(this._$textAlignColon, this.textAlign, this._$semi);
        }

        if (isc.Browser.isBorderBox) {
            if (isc.Browser.isMoz) {
                output.append(this._$mozBoxSizingColon, this._$borderBox, this._$semi);
            } else {
                if (isc.Browser.isWebKit) {
                    output.append(this._$webkitBoxSizingColon, this._$borderBox, this._$semi);
                }
                output.append(this._$boxSizingColon, this._$borderBox, this._$semi);
            }
        }

        return output.release();
    },
    
    // custom styling for picker icon cell
    
    _$fontSize:"font-size:",
    getPickerIconCellCSS : function () {
        // Not required in IE
        if (isc.Browser.isIE) return isc.emptyString;
        
        var height = this.getPickerIconHeight();
        if (isc.isA.Number(height) && height < this.getInnerHeight()) {
            return this._$fontSize + height + this._$pxSemi;
        }
        return isc.emptyString;
    },

    // Helper method to get the properties this item's picker icon if 'showPickerIcon' is true.
    
    getPickerIcon : function () {      
        if (this._pickerIcon == null) {
            var pickerIconWidth = this.getPickerIconWidth(),
                pickerIconHeight = this.getPickerIconHeight();

            var props = isc.addProperties({}, this.pickerIconDefaults, this.pickerIconProperties, {
                // Flag this as the picker icon to simplify any special manipulation
                pickerIcon:true,

                writeIntoItem:true,
                showFocused:this.showFocusedPickerIcon,
                hspace:this.pickerIconHSpace,

                // Customizable properties:
                width:pickerIconWidth,
                height:pickerIconHeight,
                src:this.pickerIconSrc,
                prompt: this.pickerIconPrompt
            });

            // apply a name to it to make it a 'valid' icon type object - allows us to get
            // a pointer to its HTML element
            this._setupIconName(props, this.pickerIconName);

            this._pickerIcon = props;

            // We need to have the _disabled flag be set from when the picker icon is
            // first drawn so subsequent enable() / disable()s will update it.
            if (this.iconIsDisabled(props)) props._disabled = true;
        }
        return this._pickerIcon;
    },

    // getElementHTML() - writes out the valueIcon (if present) and text box for the form item
    // For form items using a native HTML Form element such as <input>, this method returns 
    // that element's HTML
    
    
    _$accessKeyEquals:" ACCESSKEY='",
    _$tabIndexEquals:" TABINDEX='",
    _$singleQuote:"'",
    _$textBoxTemplate:[ "<DIV ID='", // 0
                        ,            // 1: ID for text box
                        // By marking the textBox with the 'itemPart' attributes we simplify 
                        // determining what "part" of the item received the event.
                        "' " + isc.DynamicForm._containsItem + "='",                // 2
                        ,            // 3 [formItem ID]
                        "' " + isc.DynamicForm._itemPart + "='" + isc.DynamicForm._textBoxString, // 4
                        "' CLASS='", // 5
                        ,            // 6: this.getTextBoxStyle(),
                        "' STYLE='", // 7
                        ,            // 8: this.getTextBoxCSS(), 
                        "'",         // 9
                        ,            // 10: textBoxFocusAttributes,
                        ">",         // 11
                        ,            // 12: valueIcon HTML (if required)
                        ,            // 13: actual value
                        "</DIV>"
                      ],
    getElementHTML : function (value, dataValue) {

        var output = isc.SB.create(),
            useFocusProxy = this._writeOutFocusProxy();

        
        var canFocus = this._canFocusInTextBox(),
            textBoxFocusAttributes,
            focusProxyString,
            textBoxStyle = this.getTextBoxStyle();

        if (this._fetchMissingValueInProgress() && this.loadingDisplayValue != null) {
            textBoxStyle = this._getInFieldHintStyle();
        }

        if (canFocus) {
            // If we're disabled tabIndex will currently be -1. However we don't clear 
            // this.accessKey, so do an explicit check to avoid writing out an accessKey on
            // a disabled form item
            var tabIndex = this._getElementTabIndex(),
                accessKey = this.isDisabled() ? null : this.accessKey;
            if (useFocusProxy) {
                focusProxyString = isc.Canvas.getFocusProxyString(
                                        this.getID(),
                                        // position the focus proxy at 0,0 in the appropriate
                                        // table cell
                                        
                                        false, 0, 0,
                                        
                                        this.getTextBoxWidth(), this.getTextBoxHeight(),
                                        this.isVisible(), !this.isDisabled(),
                                        tabIndex, accessKey,
                                        // Events on this focus proxy will be handled by the ISC
                                        // eventHandling system 
                                        
                                        false
                            );    
            } else {
                var attrs = isc.SB.create();
                if (accessKey != null) attrs.append(this._$accessKeyEquals, accessKey, this._$singleQuote);
                attrs.append(this._$tabIndexEquals, tabIndex, this._$singleQuote);
                textBoxFocusAttributes = attrs.release();
            }
        }

        if (focusProxyString != null) output.append(focusProxyString);

        var tbTemplate = this._$textBoxTemplate;
        tbTemplate[1] = this._getTextBoxID();
        tbTemplate[3] = this.getID();
        tbTemplate[6] = textBoxStyle;
        tbTemplate[8] = this.getTextBoxCSS();
        tbTemplate[10] = textBoxFocusAttributes; // Will be null if appropriate

        // Pre-pend the value with the valueIconHTML [will be null if appropriate]
        tbTemplate[12] = this._getValueIconHTML(dataValue);
        tbTemplate[13] = (this.showValueIconOnly ? null : value);

        output.append(tbTemplate);

        //this.logWarn("element HTML:"+ output.toString());
        return output.release();                      
    },

    // By default, getElementHTML returns a static display field so we use that.
    // This also allows subclasses that have read-only support inline in getElementHTML()
    // to not require a simple getReadOnlyHTML() override just to call it.
    getReadOnlyHTML : function (value, dataValue) {
        return this.getElementHTML(value, dataValue);
    },

    //> @method formItem.getPrintHTML() (A)
    // @param [printProperties] (PrintProperties)
    // @param [callback] (Callback)
    // @return (HTMLString) print HTML for this item
    // @group printing
    // @visibility internal
    //<
    getPrintHTML : function (printProperties, callback) {
        var value = this.getValue();
        var HTML = this[this.isReadOnly() ? "getReadOnlyHTML" : "getElementHTML"](this.mapValueToDisplay(value), value);
        if (HTML == null) HTML = isc.emptyString;
        return HTML;
    },

    // If we are focusable and not flagged as having an 'inputElement' use a focus proxy 
    // wherever we can't make a DIV natively focusable 
    
    _writeOutFocusProxy : function () {
    
        
        if (this.useFocusProxy != null) return this.useFocusProxy;
        
        // Focus proxies were required for older versions of Safari and Chrome
        // This is no longer the case with the latest versions
        // Tested on:
        // - Chrome 13.0.782.215 on Mac OSX and Windows 7 (reports as safariVersion 535.1).
        // - Safari 5.1 on Mac OSX and Windows 7 (reports as safariVersion 534.5)
        
        return (isc.Browser.isMoz && isc.Browser.geckoVersion < 20051111) && 
                this._canFocus() && !this.hasDataElement();
    },
    
    // Helper method for HTML parts:
    
    _getItemElementAttributeHTML : function () {
        if (!isc.FormItem._itemElementAttrHTML) {
            isc.FormItem._itemElementAttrHTML =  [
                " ", isc.DynamicForm._containsItem, "='", 
                null,   // item ID
                "' ",
                isc.DynamicForm._itemPart, "='", isc.DynamicForm._element, "'"
            ];
        }
        isc.FormItem._itemElementAttrHTML[3] = this.getItemID();
        return isc.FormItem._itemElementAttrHTML.join(isc.emptyString);
    },

    //> @method formItem.isValid() 
    // Returns true if this FormItem has no validation errors.
    // @return (Boolean) 
    //<
    isValid : function () {
        var errors = this.getErrors();
        if (errors == null || isc.isAn.emptyObject(errors)) {
            return true;
        }
        return false;
    },

	//>	@method	formItem.getErrors()	(A)
	// Return the validation errors in the form associated with this item, if any.
    // Errors will be returned as either a string (single error message) or an array of strings
    // (multiple error messages).
	//  @return	(string | array) Error message(s) for this item.
    // @group errors
	//<
	getErrors : function () {
        if (this.form) return this.form.getFieldErrors(this);
	},
    
    // getError() synonym for getErrors() for backcompat
    
    getError : function () {
        //>DEBUG
        this.logWarn("call to deprecated method FormItem.getError()." +
                     " Use FormItem.getErrors() instead."
                     
                     );
        //<DEBUG
        return this.getErrors();
    },
    
    // getErrorMessage - given an error string or array of errors - return it formatted as HTML for
    // display
    getErrorMessage : function (error) {
        return (isc.isAn.Array(error) ? "<UL><LI>" + error.join("</LI><LI>") + "</LI></UL>" 
                                      : error);
    },
    
    // shouldShowErrorIcon / text / style helpers
    // Allows for form level control of whether error icons/text shows up inline
    shouldShowErrorIcon : function () {
        return this.showErrorIcon != null ? this.showErrorIcon : this.form.showErrorIcons; 
    },
    shouldShowErrorText : function () {
        return this.showErrorText != null ? this.showErrorText : this.form.showErrorText;
    },
    shouldShowErrorStyle : function () {
        return this.showErrorStyle != null ? this.showErrorStyle : this.form.showErrorStyle;
    },
    // by default show hover prompts on the icon if we're showing the icon but not the message
    shouldShowErrorIconPrompt : function () {
        return this.shouldShowErrorIcon && !this.shouldShowErrorText();
    },
    
    // should the error show up above / below / left/right of the item?
    getErrorOrientation : function () {
        return this.errorOrientation != null ? this.errorOrientation : this.form.errorOrientation;
    },

	
    //>	@method	formItem.getErrorHTML()	(A)
    // Output the HTML for an error message in a form element. Default behavior respects
    // +link{FormItem.showErrorIcon} and +link{FormItem.showErrorText} as described in the
    // documentation for those attributes.
    // @param error (string | array of string) error message string or array of error messages
    // @return (HTML) HTML to display the error
    // @visibility external
	//<
	
	getErrorHTML : function (error) {
        var showErrorText = this.shouldShowErrorText(),
            showErrorIcon = this.shouldShowErrorIcon();
            
        if (!showErrorText && !showErrorIcon) return isc.emptyString;
        
		var form = this.form,
            // If we are writing out an error icon, use a table to insure:
            // - if we're showing a single error message that wraps it doesn't
            //   wrap UNDERNEATH the error icon
            // - if we're showing multiple error messages in a bulleted list the icon
            //   appears to the left of the list rather than appearing above it on a 
            //   separate line
            // - note in strict mode (inc HTML5 mode), an img followed by a nbsp char
            //   will wrap the nbsp char, causing the image to appear essentially misaligned
            //   handle this by always writing the table if we're writing out the 
            //   error icon in strict mode 
            
            writeTable = (isc.Browser.isStrict ? showErrorIcon :
                          showErrorIcon && showErrorText),
            
            
            writeDivInline = !writeTable && showErrorIcon && 
                            ((this.getErrorOrientation() == isc.Canvas.LEFT) || 
                             (this.getErrorOrientation() == isc.Canvas.RIGHT)),
            
                        // We may want to make this setting hierarchical - so it can be set at 
                        // the item or validator level as well
            titleText = (showErrorText && this.form.showTitlesWithErrorMessages &&
                         this.getTitle() != null ? this.getTitle() + ": " : null),
            output,
            
            messageString = showErrorText ? this.getErrorMessage(error) : null;

        
        // Write out containsItem / itemPart - this allows us to 
        // use partwise events to identify the inline error text.
        // Could be used for custom events, but more importantly, this
        // is helpful for the AutoTest subsystem.
        if (!writeTable) {
            output = isc.SB.concat("<DIV ", 
                    this._getInlineErrorHandleAttributes(),
                    (writeDivInline ? "style='display:inline;'" : null)
                    ," CLASS='", this.getCellStyle() , "'>" 
                    , (showErrorIcon ? this.getErrorIconHTML(error) + "&nbsp;" : null)
                    , titleText
                    , messageString
                    , "</DIV>"
            );
        } else {
            
            output = isc.SB.concat("<TABLE ",
                    this._getInlineErrorHandleAttributes(),
                    "' role='presentation' WIDTH=100% CELLSPACING=0 CELLPADDING=0><TR>",
                    "<TD WIDTH=",this.errorIconWidth,">"
                    // If we're writing a table we know we're always writing out the icon
                    , this.getErrorIconHTML(error)
                    , "</TD><TD STYLE='", isc.Canvas._$noStyleDoublingCSS, "' CLASS='" , 
                        this.getCellStyle() , "'>&nbsp;"                         
                    , titleText
                    , messageString
                    , "</TD></TR></TABLE>"
            );
        }
		return output;
	},
	
	_getInlineErrorHandleAttributes : function () {
	    if (this._inlineErrorAttributeString == null) {
	        this._inlineErrorAttributeString = isc.SB.concat(
	            "ID='", this._getDOMID('inlineErrorHandle'), "' ",
	            isc.DynamicForm._containsItem, "='", this.getID(), "' ",
	            isc.DynamicForm._itemPart, "='", isc.DynamicForm._inlineErrorString, "' "
	        );
	    }
	    return this._inlineErrorAttributeString;
	},
	
	getInlineErrorHandle : function () {
	    return isc.Element.get(this._getDOMID('inlineErrorHandle'));
	},
    
    getErrorIconHTML : function (error) {
        
        
        this._currentIconError = error;
        
        var id = this.getErrorIconId();
 
        var errorString = "";
        // add the error as an aria-label so that we can point to this as an "aria-describedby"
        // element.  This is added as part of the "extraStuff" parameter below
        if (error != null && isc.Canvas.ariaEnabled() && !isc.Canvas.useLiteAria()) {
            if (isc.isAn.Array(error)) error = error.join(",");
            errorString = ' aria-label="' + error.replace("\"", "&quot;") + '"';
        }
       
        
        return this._getIconImgHTML(
                // unique ID for the img
                id, 
                this.errorIconWidth, this.errorIconHeight,
                //vAlign for the icon
                "top", 
                0,  // vMargin
                // No left margin for the icon, no background-color for this icon
                null,
                null,
                
                // Src 
                
                this.form.getImgURL(this.errorIconSrc),
                // special flag to avoid 'display:block' in standards mode
                true,
                // extraStuff for error icon info for event (This will cause error text
                // to show up in a hover)
                // getIconImgHTML doesn't handle this directly since we usually 
                // don't have img-only icons be interactive.
                isc.DynamicForm._containsItem + "='" + this.getID() + "' " +
                
                // Don't use the same ID for the icon part name (used for event handling) 
                // as for the element in the DOM - the 'errorIconId' is retrieved via
                // _getDOMId which guarantees
                // a unique ID within the page (required for img name / dom element ID etc), 
                // but doesn't guarantee consistency across page reloads etc.
                // We want the eventPart type ID to be consistent so the autoTest subsystem
                // can reliably identify error icons.
                isc.DynamicForm._itemPart + "='" + this.errorIconName + "'" +
                errorString
        );
    },
    
    getErrorIconId : function () {
        return this._getDOMID("error");
    },
    errorIconName:"isc_errorIcon",
    
	//>	@method	textItem.getHint()	(A)
	//	Returns the hint text for this item. Default implementation returns +link{FormItem.hint}, or
    //  null if there is no hint to show.
    //
	// @group	appearance
	// @return	(HTML)		HTML to show as the hint for the item
    // @visibility external
	//<
    getHint : function () {
		if (!this.showHint || !this.hint) return null
        return this.hint;
	},
    
    // Drawing
    // ----------------------------------------------------------------------------------------
    // Form items don't write themselves into the DOM - this is typically handled by their
    // dynamicForm, or for 'standalone items', this is handled by the items' containerWidget.
    // The code to write the items out into the DOM should also notify the form items that they
    // have been written into the DOM, to allow us to perform 'isDrawn()' checks and perform
    // any necessary manipulations of the items' data element.
    
    
    //> @method formItem.drawn() 
    // Notification function to be fired on the form item when the item has been written into
    // the DOM by some container widget.
    //
    // @group drawing
    // @visibility internal
    //<
    _$drawing:"drawing",
    drawn : function () {
    	//>DEBUG
        if (this.logIsInfoEnabled(this._$drawing)) {
        	this.logInfo("Form item drawn " + 
                         (this.containerWidget == this.form ? 
                                "in form " + this.form.getID() :
                                "in container widget " + this.containerWidget.getID()) +
                         (this.logIsDebugEnabled("drawing") ? this.getStackTrace() : ""), 
                         "drawing");
        }
        //<DEBUG

        this._drawn = true;
        if (this._gotHintHTML) this._wroteOutHint = true;
        this._gotHintHTML = null;
        this._applyHandlersToElement();

        if (isc.screenReader) this.addContentRoles();
    },
    
    // fired when this item is about to be redrawn
    
    redrawing : function () {
        if (this._hasRedrawFocus(true)) {
            
            this._storeFocusForRedraw(); 
        }
        this.form.clearingElement(this);
        this._absDiv = null;
    },
    
    //> @method formItem.redrawn() 
    // Notification function to be fired on the form item when the items HTML has been redrawn
    // by some container widget.
    //
    // @group drawing
    // @visibility internal    
    //<
    redrawn : function () {
        //>DEBUG
        if (this.logIsInfoEnabled("drawing")) {
        	this.logInfo("Form item redrawn " + 
                         (this.containerWidget == this.form ? 
                                "in form " + this.form.getID() :
                                "in container widget " + this.containerWidget.getID()) +
                         (this.logIsDebugEnabled("drawing") ? this.getStackTrace() : ""), 
                         "drawing");
        }
        //<DEBUG
        
        // clear pointer to data element
        this._clearCachedHandles();

        this._applyHandlersToElement();
        
        if (isc.screenReader) this.addContentRoles();
        
        if (this._hasRedrawFocus(true)) {
            this._refocusAfterRedraw();
        }
    },
    
    // _storeFocusForRedraw()
    // When a dynamicForm is redrawn, if an item has focus, we want it to continue to have the same
    // focus / selection as before the redraw.
    // This method stores the selection / where the focus is (text box vs icons etc), so we
    // can restore it after focus
    
    _storeFocusForRedraw : function () {
        this._hadFocusBeforeRedraw = true;
        
        this.rememberSelection();
        
        if (this.items) {
            for (var i = 0; i < this.items.length; i++) {
                if (this.items[i].hasFocus) {
                    return this.items[i]._storeFocusForRedraw();
                }
            }
        }
        
        var element = this._getCurrentFocusElement();
        
        if (element != null && element != this.getFocusElement()) {
            var picker = this.getPickerIcon();
            if (picker != null && this._getIconLinkElement(picker) == element) {
                this._redrawFocusIcon = picker;
            } else if (this.icons) {
                for (var i = 0; i < this.icons.length; i++) {
                    if (this._getIconLinkElement(this.icons[i]) == element) {
                        this._redrawFocusIcon = this.icons[i];
                        break;
                    }
                }
            }
        }
    },
    
    // _refocusAfterRedraw - fired in response to item.redrawn()
    // If the item had focus when the redraw occurred, put focus back in it (without firing any
    // focus handlers) now.
    _refocusAfterRedraw : function () {
    
        // Sanity checks - don't refocus if
        // - we're not visible or drawn
        // - focus is marked as being elsewhere on the page
        // - focus is marked as being elsewhere on the form
        //  (focus shift implies this was delayed and focus has subsequently moved)
        var shouldRefocus = this.isDrawn() && this.isVisible();
        if (shouldRefocus) {
            var focusCanvas = isc.EH.getFocusCanvas();
            if (focusCanvas != null && focusCanvas != this.form) {
                shouldRefocus = false;
            } else {
                var focusItem = this.form.getFocusSubItem();
                if (focusItem != this && focusItem != this.parentItem && 
                    (!this.items || !this.items.contains(focusItem))) 
                {
                    
                    shouldRefocus = false;
                }
            }
        }
        delete this._hadFocusBeforeRedraw;
        
        
        if (shouldRefocus && isc.Browser.isIE) {
            isc.FormItem._testStuckSelectionAfterRedraw(this);
        }

        if (this.items) {
            for (var i = 0; i < this.items.length; i++) {
                if (this.items[i]._hasRedrawFocus()) {
                    return this.items[i]._refocusAfterRedraw();
                }
            }
        }
        
        
        // we want our refocus to be silent - call the uber-advanced method to suppress 
        // developer-specified focus handlers from firing when focus gets restored
        if (shouldRefocus) {
            this.form._suppressFocusHandlerForItem(this);
            // If the focus item was scrolled out of view, it'll natively jump into
            // view when we refocus on it, scrolling the parent.
            // Use the notifyAncestorsAboutToReflow / notifyAncestorsReflowComplete mechanism
            // to avoid a scroll jump on the containerWidget (even though the redraw has
            // already occurred at this point)
            
            this.containerWidget.notifyAncestorsAboutToReflow();
        }
        
        // If appropriate stick focus back into the icon which previously had it
        var focussed = false;
        if (this._redrawFocusIcon) {
            var icon = this.getIcon(this._redrawFocusIcon);
            delete this._redrawFocusIcon;
            if (icon) {
                if (shouldRefocus) {
                    this.focusInIcon(icon);
                }
                focussed = true;
            }
        }
        
        // still going - we weren't focused in a sub icon or an item - just focus in
        // our standard focus element
        if (shouldRefocus && !focussed) {
            // set a flag noting that we're currently refocussing after redraw
            // on element focus (which may be async) we'll clear this flag
            // This allows us to suppress 'selectOnFocus' behavior.
            this._refocussingAfterRedraw = true;
            this.focusInItem();
        }
        if (shouldRefocus) {
            this.containerWidget.notifyAncestorsReflowComplete();
        }
        
    },
    
    
    _applyHandlersToElement : function () {
        //!DONTCOMBINE

        if (this._canFocus()) {
            var element = this.getFocusElement();
            if (!element) {
                if (this._canFocusInTextBox()) {
                    this.logWarn("Attempting to apply event handlers to this item. " + 
                        "Unable to get a pointer to this item's focus element");
                    return;
                }
            } else {

                if (this._propagateMultiple) {
                    // _propagateMultiple is set on items that can support multiple-files
                    if (this.multiple) {
                        element.multiple = this.multiple;
                    } else {
                        element.multiple = false;
                    }
                }

                if (this.accept) {
                    // FileItem and UploadItem use this to supply filters to the file picker
                    element.accept = this.accept;
                }

                // Apply focus/blur handlers to the focus element. These fall through to 
                // formItem._nativeElementFocus() / formItem._nativeElementBlur()
                element.onfocus = isc.FormItem._nativeFocusHandler;
                element.onblur = isc.FormItem._nativeBlurHandler;
                
                // IE fires proprietary oncut / onpaste events. Set up handlers for these so we 
                // can detect changes due to paste triggered from a menu option as well as from 
                // keypresses.
                
                if (isc.Browser.isIE) {
                    element.onpaste = isc.FormItem._nativeCutPaste;
                    element.oncut= isc.FormItem._nativeCutPaste;
                }
                    
                // Support a generic way to apply native event handlers to the element without
                // overriding this method.
                //  this._nativeEventHandlers is expected to be an object of the format
                //   {nativeHandlerName:function}
                // [Don't apply these handlers to icons!]
                if (this._nativeEventHandlers) {    
                    for (var handler in this._nativeEventHandlers) {
                        if (this._nativeEventHandlers[handler] == null) continue;
                        element[handler] = this._nativeEventHandlers[handler];
                    }
                }
            }
        }
        
        this._setUpIconEventHandlers();
    },
    
    _setUpIconEventHandlers : function () {
        // If we have any icons, we need to apply focus/blur handlers to them as well.
        // Note that we may draw/clear icons independently of redrawing the form item, so we
        // need a separate method to handle them being drawn
        if (this._shouldShowPickerIcon()) this._iconDrawn(this.getPickerIcon());
        if (this.showIcons && this.icons && this.icons.length > 0) {
    
            for (var i = 0; i < this.icons.length; i++) {
                var icon = this.icons[i];
                if (icon && (this._writeIconIntoItem(icon) || this._shouldShowIcon(icon)))
                    this._iconDrawn(icon);
            }
        }    
    },
    
    // Notification function fired whenever an icons is written into the DOM.
    // Allows us to apply event handlers directly to the icon rather than writing them out
    // as part of the icon's HTML
    _$hash:"#",
    _useIconLinkElements:
        
        (!isc.Browser.isSafari) &&
        (!isc.Browser.isMoz),
    _iconDrawn : function (icon) {
        if (!icon.imgOnly) {
            var link = this._getIconLinkElement(icon);

            if (link) {
                link.onfocus = isc.FormItem._nativeIconFocus
                link.onblur = isc.FormItem._nativeIconBlur
                
                if (this._useIconLinkElements) {
                    // The link needs an HREF or it will not be focus-able
                    link.href = this._$hash;

                    // Write out an onclick handler that simply stops us navigating to the href
                    // for the icon's link.  We will fire the icon's click action via
                    // standard form item click handling 
                    link.onclick = isc.FormItem._nativeIconClick;
                }
            }
                
        }
        
        
    
    },
    
    //> @method formItem.cleared() 
    // Notification function to be fired on the form item when the items HTML has been removed
    // from the DOM by some container widget.
    //
    // @group drawing
    // @visibility internal    
    //<
    cleared : function () {
    	//>DEBUG
        if (this.logIsInfoEnabled("drawing")) {
        	this.logInfo("Form item cleared " + 
                         (this.containerWidget == this.form ? 
                                "from within form " + this.form.getID() :
                                "from within container widget " + this.containerWidget.getID()) +
                         (this.logIsDebugEnabled("drawing") ? this.getStackTrace() : ""), 
                         "drawing");
        }
        //<DEBUG
        this.form.clearingElement(this);
        
        this._clearCachedHandles();
        this._wroteOutHint = false;
        this._gotHintHTML = false;
        this._drawn = false;
    },

    _clearCachedHandles : function () {
        this._dataElement = null;
        this._absDiv = null;
        this._focusProxyHandle = null;
        this._htmlPartHandles = {};
    },

    //> @method formItem.isDrawn() 
    // Returns true if this item has been written out into the DOM.
    //
    // @return (Boolean) whether this item is drawn
    // @group drawing
    // @visibility external
    //<
    isDrawn : function () {
        return this._drawn;
    },
 
    // Icons   
	// -----------------------------------------------------------------------------------------
    
    // _setUpIcons called at init time.  This should apply default properties to icons as
    // required.
    _setUpIcons : function () {
        var icons = this.icons;
        if (icons == null) return;
        
        for (var i = 0; i < icons.length; i++) {
            var icon = icons[i];
            
            this._setUpIcon(icon);
        }
        
    },
    
    // _setUpIcon - run by setUpIcon() on each specified icon object to apply required 
    // properties such as ID.
    // Split into a separate method so this can be called separately if icons are applied after
    // setUpIcons has been run (See ExpressionItem for an example of this)
    _setUpIcon : function (icon) {       
        // apply an identifier to the icon (to be written into the DOM as an attribute) 
        // to ensure that the
        // appropriate click action is fired on a click, and allow us to get a pointer
        // back to the icon image / link elements in the DOM
        this._setupIconName(icon);
            
        // Set the '_disabled' flag on the icon. We use this to track whether we need to
        // update HTML when the icon gets enabled / disabled
        if (this.iconIsDisabled(icon)) icon._disabled = true;
       
    },

    
    //> @method	formItem.getIconsHTML()  (A)
    //  Return the HTML to draw any icons to be displayed after the form item
	//  @group  appearance
    //
    //      @return (HTML)      HTML for the icons
    //<
    _iconsTableStart:"<table role='presentation' cellpadding=0 cellspacing=0 margin=0><tr>",
    getIconsHTML : function (includePicker) {
        if (!this.showIcons || 
            (this.icons == null && (!includePicker || !this._shouldShowPickerIcon()))) 
        {
            return "";
        }
        var hasFocus = this._hasRedrawFocus(true);
        
        if (this.showIconsOnFocus && !hasFocus) {
            this.hideAllIcons();
            return "";
        }
        
        var output = isc.SB.create(),
            showingIcons = false;
        
        // Write the icons out into a table with one cell per icon.
        // This will ensure they reliably show up in a row horizontally without
        // relying on <nobr> or css white-space:nowrap;
        
        var icons = this.icons;
        if (includePicker && this._shouldShowPickerIcon()) {
            icons = [this.getPickerIcon()];
            icons.addList(this.icons);
        }
        
        for (var i = 0; i < icons.length; i++) {
            
            
            var icon = icons[i];
            // don't write out the icon if it specified a showIf, which returns false
            if (!this._shouldShowIcon(icon) || this._writeIconIntoItem(icon)) continue;

            if (showingIcons == false) {
                showingIcons = true;
                output.append(this._iconsTableStart);
            }
    
            output.append("<td>");

            var showFocused = hasFocus && this._iconShouldShowFocused(icon, true);
            output.append(this.getIconHTML(icon, null, this.iconIsDisabled(icon), !!showFocused));
            
            output.append("</td>");
        }
        if (showingIcons) output.append("</table>");
        
        return output.release();
    },
    
    // Helper method to determine if an item (or one of it's subItems) has focus before redraw
    _hasRedrawFocus : function (checkSubItems) {
        var hasFocus = this.hasFocus ||  this._hadFocusBeforeRedraw;
        // If we have sub items, check for whether one of those has focus
        if (checkSubItems && !hasFocus && this.items != null) {
            for (var i = 0; i < this.items.length; i++) {
                if (this.items[i].hasFocus || this.items[i]._hadFocusBeforeRedraw) hasFocus = true;
                break;
            }
        }
        
        // Exception: If we're scrolled out of the containerWidget's viewport, don't refocus or
        // we'll natively jump scroll into view.
        
        
        return hasFocus;
    },
    
    // setupIconName
    // Given an icon object, ensure it has a unique name, autoGenerating one if appropriate
    // This method is called on init and on setIcons so should be able to test for 
    // uniqueness here.
    
    _setupIconName : function (icon, name) {
        if (name == null) name = icon.name;
        // Backcompat: We used to use icon._id.
        // This was never exposed so developers shouldn't have been setting this
        // attribute but for safety, if this is set, respect it
        if (name == null && icon._id != null) {
            this.logWarn("Attempting to use '_id' property as icon name - this property has been deprecated in favor of 'name'");
            name = icon._id;
        }
        if (name != null) {
            // test for uniqueness - names must be unique or getIcon / updateIconSrc etc will fail
            var collisions = this.icons ? this.icons.findAll("name", name) : [];
            if (collisions != null && collisions.length > 0 &&
                (collisions.length > 1 || collisions[0] != icon))
            {
                this.logWarn("This form item has more than one icon with the same specified name:"
                    + name + ". Ignoring this name and using an auto-generated one instead.");
                name = null;
            } else {
                icon.name = name;
                return icon;
            }
        }
        if (this._nextIconId == null) this._nextIconId = 0;
        icon.name =  "_" + this._nextIconId++;
        return icon;
    },
    
    
    _getIconVAlign : function (icon) {
        // Don't write out a vertical-align css property for the picker icon
        if (this._pickerIcon && (icon == this._pickerIcon)) return null;
        
        var alignment = this.iconVAlign;
        
        if (alignment == isc.Canvas.TOP) {
            return "top";            
        } else if (alignment == isc.Canvas.BOTTOM) {
            return (isc.Browser.isSafari ? "bottom" : "text-bottom");
        } else if (alignment == isc.Canvas.CENTER) {
            return "middle"
        }
        
        // if we don't recognize the alignment, just return it. 
        return alignment;
    },
    
    // _getIconVMargin - return a value to write as a top / bottom margin onto the icons' img
    // tag
    
    _getIconVMargin : function () {
        return 0;
    },
    
    // Helper to get the prompt for the icon, if there is one.
    getIconPrompt : function (icon) {
        // don't show icon-specific prompt on hover over disabled icon (or disabled item with
        // icon, which is obviously also disabled).
        // We still show item level prompt for disabled items - this is useful for 
        // telling the user why an item is disabled, for example.
        if (this.iconIsDisabled(icon)) return null;
        return icon.prompt || this.iconPrompt;
    },

    // Gets the src for an icon's image.
    _$blank: "blank",
    _$rtl: "rtl",
    getIconURL : function (icon, over, disabled, focused) {        
        var src = icon.src || this.defaultIconSrc;
        if (src == this._$blank) return isc.Canvas._blankImgURL;

        var state = (this.showDisabled && (disabled || this.iconIsDisabled(icon))) 
                            ? isc.StatefulCanvas.STATE_DISABLED 
                            : over ? isc.StatefulCanvas.STATE_OVER : null,
            customState = icon.showRTL && this.isRTL() ? this._$rtl : null;

        src = isc.Img.urlForState(src, false, focused, state, null, customState);
        return src;
    },

    _$RTL: "RTL",
    getIconStyle : function (icon, over, disabled, focused) {
        if (!icon || icon.baseStyle == null) return null;
        var retValue = icon.baseStyle;
        if (this.showDisabled && (disabled || this.iconIsDisabled(icon))) {
            retValue += isc.StatefulCanvas.STATE_DISABLED;
        } else {
            if (focused) retValue += isc.StatefulCanvas.FOCUSED;
            if (over) retValue += isc.StatefulCanvas.STATE_OVER;
        }
        if (icon.showRTL && this.isRTL()) retValue += this._$RTL;
        return retValue;
    },

    // getIconHTML() retrieves the HTML for icons.
    // By default icons are written into the DOM after the form item. However we also use this
    // method for icons written directly into the form item's HTML (see the SelectItem for
    // an example of this).
    getIconHTML : function (icon, over, disabled, focused) {            
        var iconSrc = this.getIconURL(icon, over,disabled,focused),
            width = this.getIconWidth(icon),
            height = this.getIconHeight(icon),
            hspace = (icon.hspace != null ? icon.hspace : this.iconHSpace),
            backgroundColor = icon.backgroundColor,
            formID = this.form.getID(),
            // Remember - this is a global ID for this Form Item Instance, so can be used
            // as window[itemID].foo(), as well as being passed to the 'bubbleEvent()' method
            // on the Form.
            itemID = this.getItemID(),
            iconID = icon.name,
            iconStyle = this.getIconStyle(icon, over, disabled, focused),
            classText = (iconStyle == null ? isc.emptyString : " class='" + iconStyle + this._$singleQuote);
        // If the icon is marked as 'imgOnly', just return the img tag - event handling should
        // be handled by the Form Item itself
        if (icon.imgOnly) {
        
            return this._getIconImgHTML(
                                this._getIconImgId(iconID),
                                width, 
                                height, 
                                this._getIconVAlign(icon),
                                this._getIconVMargin(icon),
                                // If it's just an image we always put hspace onto the image tag
                                // as a left margin
                                hspace,
                                backgroundColor,
                                iconSrc,
                                null,
                                classText
                    );
        
        // We embed the icon in a link to make it focusable
        
        } else {

            
            if (isc.FormItem._iconTemplate == null) {
                isc.FormItem._iconHSpacePrefix = " style='margin-left:";
                isc.FormItem._iconHSpaceRTLPrefix = " style='margin-right:";

                isc.FormItem._iconTemplate = [
                    (this._useIconLinkElements
                        ? "<a role='button' ID='" : "<span role='button' ID='"),    // 0
                    ,                           // 1: link elementID: this._getIconLinkId(icon.name)
                    "'",                        // 2
                      
                    
                    isc.FormItem._iconHSpacePrefix,  // 3
                    ,                           // 4: icon h-space: hspace
                    "px;"
                    + (isc.Browser.isMoz ? "-moz-user-focus:" : ""),    // 5
                    ,                           // 6: normal / ignore (MOZ ONLY)
                    ,                           // 7: cursor:default if disabled, otherwise "hand"

                    "' tabIndex=",               // 8
                    ,                           // 9: Tab index
                    
                    // Identifiers for the form item event handling system
                    " ",                        // 10
                    isc.DynamicForm._containsItem,  // 11
                    "='",                       // 12
                    ,                           // 13: itemID
                    "' ",                       // 14
                    isc.DynamicForm._itemPart,  // 15
                    "='",                       // 16
                    ,                           // 17: iconID

                    // Allow the ISC event handling system to handle events occurring over
                    // this link.                    
                    "' handleNativeEvents=false>", // 18
                    ,                           // 19: this._getIconImgHTML()
                    (this._useIconLinkElements ? "</a>" : "</span>") // 20
                    

                ]
            }
    
            var template = isc.FormItem._iconTemplate;
            

            var disabled = this.iconIsDisabled(icon),
                tabIndex = (disabled || this.canTabToIcons == false) ? -1
                                              : this._getIconTabIndex(icon);

            
            template[1] = this._getIconLinkId(iconID);

            var hspaceToLink = this._applyIconHSpaceToLink(icon);
            if (hspaceToLink) {
                if (this.isRTL()) {
                    template[3] = isc.FormItem._iconHSpaceRTLPrefix;
                } else {
                    template[3] = isc.FormItem._iconHSpacePrefix;
                }
                template[4] = hspace;
            } else {
                template[4] = "0"
            }

            //In Moz we need to set -moz-user-focus to disable focus if tabIndex < 0
            if (isc.Browser.isMoz) template[6] = (tabIndex < 0 ? "ignore;" : "normal;");

            template[7] = disabled ? "cursor:default;" : "cursor:" + isc.Canvas.HAND;

            template[9] = tabIndex;

            if (isc.Canvas.ariaEnabled() && !isc.Canvas.useLiteAria()) {
                template[10] = " ";
                // we use window.status to show the prompt, this won't work for a screenReader
                if (icon.prompt) {
                    
                    template[10] = " aria-label='" + icon.prompt.replaceAll("'", "&apos;") + "' ";
                }
                // advertise disabled state as well
                if (disabled) template[10] += " aria-disabled='true' ";
            }

            template[13] = itemID;
            template[17] = iconID;
            template[19] = this._getIconImgHTML(
                                this._getIconImgId(iconID),
                                width, 
                                height, 
                                this._getIconVAlign(icon),
                                this._getIconVMargin(icon),
                                (!hspaceToLink ? hspace : null),
                                backgroundColor,
                                iconSrc,
                                null,
                                classText
                           );

            return template.join(isc.emptyString);

        }
    },
    
    // Helper method - Wherever possibly we apply icon hspace as margin-left on the Link item
    // around an icon rather than on the img tag. This avoids the dotted focus outline extending
    // to the left of the image when the icon has focus.
    // However 
    // - this doesn't work in IE
    // - in Safari / Chrome we've seen it introduce styling problems
    
    // - In Moz strict mode it also introduces styling problems, causing (for example) 
    //   the date picker icon to appear vertically misaligned with other icons.
    // - in some cases we don't write out a link element
    // So we can't always use this approach
    
    _applyIconHSpaceToLink : function (icon) {
        return (!isc.Browser.isIE && !isc.Browser.isSafari && !icon.imgOnly && !isc.Browser.isStrict);
    },
    
    // Use _getIconImgHTML() to get the HTML for the image, without the link tag 
    // Called from _getIconHTML(), and also used for the error icon
    
    _$vAlignColon:"vertical-align:",
    _iconImgHTMLExtraCSSTextTemplate: [
        // Align the icon vertically as specified.
        
        ,                           // 0 vertical-align:, or null
        ,                           // 1 valign or null (this._getIconVAlign(icon))
        ";margin-top:",             // 2
        ,                           // 3: this._getIconVMargin(icon)
        "px;margin-bottom:",        // 4
        ,                           // 5: this._getIconVMargin(icon)
        "px;",                      // 6
        // Optional left margin for the icon
        ,                           // 7
        // Optional background-color for the icon
        ,                           // 8: background-color='xxx'
        null                        // 9: optional display:block for strict mode
    ],
    _getIconImgHTML : function (imgID, width, height, vAlign, vMargin, lMargin, backgroundColor,
                                src, errorIcon, extraStuff) {
        // Get the icon Img HTML from the Canvas 'imgHTML()' method.  This handles displaying
        // PNG type files as well as other img files.

        
        var template = this._iconImgHTMLExtraCSSTextTemplate;

        if (vAlign != null) {
            template[0] = this._$vAlignColon;
            template[1] = vAlign;
        } else {
            template[0] = null;
            template[1] = null;
        }

        // apply any top / bottom margin to the icon
        
        template[3] = vMargin;
        template[5] = vMargin;

        if (lMargin != null) {
            
            template[7] = (this.isRTL() ? "margin-right:" : "margin-left:") + lMargin + "px;";
        } else {
            template[7] = null;
        }

        template[8] = (backgroundColor != null ? "background-color:" + backgroundColor + ";" : null);

        // display:block - required for strict mode IF the icon is being rendered into a table
        // (which is the default)
        // Avoid this if we're not in strict mode, or this is the error icon
        if (isc.Browser.isStrict && !isc.Browser.isTransitional && !errorIcon) {
            template[9] = "display:block;";
        } else {
            template[9] = null;
        }

        var extraCSSText = template.join(isc._emptyString);

        if (extraStuff == null) {
            extraStuff = " id='" + imgID + "'";
        } else {
            extraStuff += " id='" + imgID + "'";
        }

        
        var imgParams = isc.FormItem._imgParams = isc.FormItem._imgParams || 
                { align:isc.Browser.isSafari ? "absmiddle" : "TEXTTOP" };
        imgParams.src = src;
        imgParams.width = width;
        imgParams.height = height;
        imgParams.extraCSSText = extraCSSText;
        imgParams.extraStuff = extraStuff;
        return isc.Canvas.imgHTML(imgParams);
    },

    // -------------------------
    // icons methods
    //

    // Icons consist of 2 elements - an image surrounded by a link
    // Internal methods _getIconLinkId() and _geticonImgId() return a unique identifier for
    // these elements based on some icon's ID
    _ImgIDCache:{},
    _$_iLink_:"_iLink_",
    _$_iImg_:"_iImg_",
    _getIconLinkId : function (id) {
        // inactiveHTML - avoid caching the IDs here - we'll not be needing to get at the generated
        // link/img elements directly
        if (this.isInactiveHTML()) {
            return this._getDOMID(this._$_iLink_ + id);
        }
        if (!this._iLinkIDCache) this._iLinkIDCache = {};
        var cache = this._iLinkIDCache;
        if (!cache[id]) {
            // doing our own cacheing so don't have _getDOMID also cache the result
            cache[id] = this._getDOMID(this._$_iLink_ + id, true);
        }
        return cache[id];
    },
    _getIconImgId : function (id) {
        // inactiveHTML - avoid caching the IDs here - we'll not be needing to get at the generated
        // link/img elements directly
        if (this.isInactiveHTML()) {
            return this._getDOMID(this._$_iImg_ + id);
        }
        if (!this._iImgIDCache) this._iImgIDCache = {};
        var cache = this._iImgIDCache;
        if (!cache[id]) {
            // doing our own cacheing so don't have _getDOMID also cache the result
            cache[id] = this._getDOMID(this._$_iImg_ + id, true);
        }
        return cache[id];
    },
    

    // Internal methods to get a pointer to Icon's HTML elements in the DOM
    
    _getIconLinkElement : function (icon) {
        icon = this.getIcon(icon);
        if (icon == null || icon.imgOnly) return null;
        var elementID = this._getIconLinkId(icon.name);       
        return isc.Element.get(elementID);
    },
    
    _getIconImgElement : function (iconName) {
        var icon = this.getIcon(iconName);
        if (icon == null) {
            if (iconName == this.errorIconName) {
                return isc.Element.get(this.getErrorIconId());
            }
            return null;
        }

        var elementID = this._getIconImgId(icon.name);
        return isc.Element.get(elementID);
    },
    
    // Helper method - determines whether the some event occurred over one of our icons based
    // on the native target element for the event.
    
    _getTargetIcon  : function (element) {
        if (!element || !this.icons) return null;
        
        var itemInfo = isc.DynamicForm._getItemInfoFromElement(element);
        if (!itemInfo || itemInfo.item != this) return null;
        return itemInfo.icon;
    },
    
    // _shouldShowIcon() helper method to evaluate 'showIf' property on form item icons
    _$true:"true",
    _$false:"false",
    _shouldShowIcon : function (icon) {
        // if printing, or if canEdit is false and readOnlyDisplay is "static", show no icon
        if (this._isPrinting() || this.renderAsStatic()) return false;
        if (icon.showIf == null) return true;
        // If specified as a boolean or true/false string, we don't need to build a function, etc
        if (icon.showIf === true || icon.showIf == this._$true) return true;
        if (icon.showIf === false || icon.showIf == this._$false) return false;
        // Note - icons are simple objects and have no stringMethodRegistry, so we must
        // use replaceWithMethod() to convert to a method (if it's currently a string)
        isc.Func.replaceWithMethod(icon, "showIf", "form,item");
        return !!icon.showIf(this.form, this);
    },
    
    _shouldShowPickerIcon : function () {
        return this.showPickerIcon && this._shouldShowIcon(this.getPickerIcon())
            && !this._isPrinting();
    },

    
    _writeIconIntoItem : function (icon) {
        if (icon.writeIntoItem) return true;
        return false;
    },

    _mayShowIcons : function () {
        if (!this.showIcons || this.icons == null ||
            (this.showIconsOnFocus && !this.hasFocus)) return false;
        return true;
    },

    // getTotalIconsWidth()
    // Method to return the horizontal drawn space taken up by all this form item's icons.
    // This enables us to size the form's HTML element appropriately.
    getTotalIconsWidth : function () {
        if (!this._mayShowIcons()) return 0;

        var width = 0;
        for (var i = 0; i < this.icons.length; i++) {
            var icon = this.icons[i];
            if (!this._shouldShowIcon(icon) || this._writeIconIntoItem(icon)) continue;

            width += (icon.width != null ? icon.width : this.iconWidth) + 
                        (icon.hspace != null ? icon.hspace : this.iconHSpace);
        }
        return width;
    },

    getIconsHeight : function () {
        if (!this._mayShowIcons()) return 0;

        var maxHeight = 0;
        for (var i = 0; i < this.icons.length; i++) {
            var icon = this.icons[i];
            if (!this._shouldShowIcon(icon) || this._writeIconIntoItem(icon)) continue;
            var iconHeight = (icon.height != null ? icon.height : this.iconHeight);
            // If we're writing margins out, the icons will take up more space
            iconHeight += this._getIconVMargin() *2;
            if (iconHeight > maxHeight) maxHeight = iconHeight;
        }

        return maxHeight;
    },
    
    //>@method setIcons() 
    //  Programmatically update the icons for this Form Item.  Will redraw the form item to show
    //  the new icons
    //  @param  icons   (Array) Array of icon definition objects
    //<
    
    setIcons : function (icons) {
        this.icons = icons;
        this._setUpIcons();
        this.redraw();
    },
    
    addIcon : function (icon) {
        if (!this.icons) this.icons = [];
        this.icons.add(icon);
        this.setIcons(this.icons);
        return icon;
    },

    getIconByProperty : function (key, value) {
        if (this.icons) return this.icons.find(key, value);
    },

    // enable / disable icons at runtime
    // (Used by 'setDisabled')
    setIconEnabled : function (icon) {
        
        icon = this.getIcon(icon);
        if (!icon) return;
        
        // Track the enabled / disabled state on the icon. This avoids us updating the 
        // HTML if we don't have to.
        var enabled = !this.iconIsDisabled(icon);
        if (!!icon._disabled != enabled) return;
        if (!enabled) icon._disabled = true;
        else delete icon._disabled;
        
        if (!this.isDrawn()) return;

        var linkElement = this._getIconLinkElement(icon),
            imgElement = this._getIconImgElement(icon);
        
        // Enabling / disabling an icon will modify:
        // - tabIndex (can't tab to disabled icons);
        //   - focus altogether
        // - click action should no-op
        // - disabled image should be shown if there is one.
        if (linkElement) {    
            // Note - if we did a 'this.setElementTabIndex(-1)' on 'setDisabled(true)' there
            // would be no need for this, as that will also update the tabIndex of icons.
            // However we don't by default because applying the native 'disabled' state to
            // the native HTML form elements will already remove them from the page's tab order.
            if (!enabled) {
                isc.FormItem.setElementTabIndex(linkElement, -1);
                linkElement.style.cursor = "default"
            } else {
                isc.FormItem.setElementTabIndex(linkElement, this._getIconTabIndex(icon))
                linkElement.style.cursor = "";
            }
        }    
        if (imgElement) {
            var src = this.getIconURL(icon, null, !enabled);
            isc.Canvas._setImageURL(imgElement, src);
            var iconStyle = this.getIconStyle(icon, null, !enabled);
            if (iconStyle != null) imgElement.className = iconStyle;
        }
    },
    
    
    //> @method  formItem.showIcon()  
    // This method will show some icon in this item's +link{formItem.icons} array, if it is not
    // already visible. Note that once this method has been called, andy previously specified
    // +link{formItemIcon.showIf} will be discarded.
    // <P>
    // Note that if the form item's showIcons property is set to false, no icons will be displayed
    // for the item. In this case this method will not cause the icon to be displayed.
    //
    // @param icon (String) +link{FormItemIcon.name,Name} of the icon to be shown.
    // @visibility external
    //<
    
    showIcon : function (icon, focused) {
        // all icons are no longer hidden!
        delete this._allIconsHidden;

        // icon param doc'd as being icon name but support index or raw icon object too.
        if (isc.isA.String(icon) || isc.isA.Number(icon)) icon = this.getIcon(icon);
        if (!isc.isAn.Object(icon)) return;

        // If the icon's ID hasn't been set yet, set it now
        
        if (icon.name == null) {
            this._setupIconName(icon);
        }

        var currentlyVisible = this._shouldShowIcon(icon);

        // even if the icon is currently visible, set showIf to ensure it is always visible
        // from this point on.
        // For icons written into the form item, the 'getElementHTML()' method should handle
        // this as appropraite.
        icon.showIf = function () {return true;}
        
        // Only force a redraw / insert into the DOM if the icon wasn't previously visible
        if (!currentlyVisible && this.showIcons && this.containerWidget.isDrawn() && 
            this.isVisible()) 
        {
            // If the redrawOnShowIcon property is set, simply mark the form for redraw
            // If writeIntoItem is true we have to redraw since we will be changing the HTML
            // of the whole form item.
            if (this.redrawOnShowIcon || icon.writeIntoItem) {
                this.redraw();
                
            // Otherwise we're going to show/hide the icon without redrawing the whole form
            } else {
                var iconCellElement = isc.Element.get(this.getIconCellID());
                
                if (iconCellElement != null) {
                
                    // If no icons are visible just get getIconsHTML to get full HTML, including
                    // an outer table we write out to ensure we don't wrap icons
                    if (iconCellElement.childNodes.length == 0) {

                        
                        // Note that in some cases we write the pickerIcon into the item, in others
                        // we dont, so getIconsHTML() can include the picker.
                        // In this case we're always writing out exactly one icon, so we only want
                        // getIconsHTML() to include the picker HTML if it is the picker icon.
                        iconCellElement.innerHTML = this.getIconsHTML(icon == this.getPickerIcon());

                    } else {
                        var iconHTML = this.getIconHTML(icon, null, this.isDisabled(), focused),
                            // We write icons into separate cells of a table...
                            cellHTML = "<td>" + iconHTML + "</td>",
                            iconTable = iconCellElement.firstChild, 
                            index = 0;
                        for (var i = 0; i < this.icons.length; i++) {
                            if (this.icons[i] == icon) break;
                            if (this._shouldShowIcon(this.icons[i])) {
                                index ++;
                            }
                        }
                        if (index == 0) {
                            isc.Element.insertAdjacentHTML(iconTable.rows[0], "afterBegin", cellHTML);
                        } else {
                            isc.Element.insertAdjacentHTML(iconTable.rows[0].cells[index-1], "beforeEnd", cellHTML);
                        }
                    }

                    // Fire _iconVisibilityChanged().  This method will handle resizing the form
                    // item element to accommodate the space taken up by the newly shown icon.
                    this._iconVisibilityChanged();
                    // notify the icon that it has been written into the DOM so we can set u
                    // eventHandlers for it.
                    
                    this._iconDrawn(icon);

                        
                // No icon cell element - must redraw.  
                // This could happen if this.icons was null so we didn't write an outer-table
                // at all
                } else {
                    
                    this.logInfo("showIcon(): Unable to dynamically update icon visibility - " +
                                 "redrawing the form", "formItemIcons");
                    return this.redraw();
                }
            }
        }
    },
    
    //> @method  formItem.hideIcon()  
    // This method will hide some icon in this item's +link{formItem.icons} array, if it is 
    // currently visible. Note that once this method has been called, andy previously specified
    // +link{formItemIcon.showIf} will be discarded.
    //
    // @param icon (String) +link{FormItemIcon.name,Name} of the icon to be hidden.
    // @visibility external
    //<
    hideIcon : function (icon) {
        if (isc.isA.String(icon) || isc.isA.Number(icon)) icon = this.getIcon(icon);
        if (!isc.isAn.Object(icon)) return;
        var currentlyVisible = this._shouldShowIcon(icon);
        icon.showIf = function () {return false;}
        
        // Only force a redraw / remove from the DOM if the widget was previously visible
        if (currentlyVisible && this.showIcons && this.containerWidget.isDrawn() && 
            this.isVisible()) 
        {
            // If the redrawOnShowIcon property is set, simply mark the form for redraw
            if (this.redrawOnShowIcon || icon.writeIntoItem) {
                this.redraw();
            }
            // Otherwise we're going to show/hide the icon without redrawing the whole form
            else {
                var element = icon.imgOnly  ? this._getIconImgElement(icon) 
                                            : this._getIconLinkElement(icon);
                
                if (element == null) {
                    this.logInfo("hideIcon(): Unable to dynamically update icon visibility - " +
                                 "redrawing the form");
                    return this.redraw();
                }

                //this.logWarn("would remove element: " + this.echo(element) + 
                //             " from parentNode: " + this.echo(element.parentNode));
                var cell = element.parentNode;
                // sanity check - the external icons are all written into a table - verify
                // that the parent element *is* a td element
                if (cell.tagName != "TD") {
                    isc.Element.clear(element);
                } else {
                    
                    cell.parentNode.removeChild(cell);
                }

                // Fire _iconVisibilityChanged().  This method will handle resizing the form
                // item element to accommodate the space taken up by the newly shown icon.
                this._iconVisibilityChanged();
            }
        }
    },
    
    // _iconVisibilityChanged()
    // Notification fired when showIcon() or hideIcon() succussfully completes having 
    // manipulated the DOM to show/hide an icon.
    // Default implementation will resize the form item element to accommodate the space
    // taken up by it's visible icons.
    // Will not be fired if showIcon() or hideIcon() fell through to form.markForRedraw().
    _iconVisibilityChanged : function () {
        this._resetWidths();
    },

    // showAllIcons / hideAllIcons:
    // Used by 'showIconsOnFocus' / 'hideIconsOnKeypress' behavior.
    showAllIcons : function (focused) {
        
        if (this._hideAllIconsEvent != null) {
            isc.Timer.clear(this._hideAllIconsEvent);
            delete this._hideAllIconsEvent;
        }
        this._showIcons(this.icons, focused);
    },
    
    hideAllIcons : function () {
        if (this._hideAllIconsEvent != null) delete this._hideAllIconsEvent;
        this._hideIcons(this.icons);
        this._allIconsHidden = true;
    },

    // _showIcons / _hideIcons -- helper functions to show/hide multiple icons at a time.
    _showIcons : function (icons, focused) {  
        if (icons == null || icons.length == 0) return;
        for (var i = 0; i < icons.length; i++) {
            focused = focused && this._iconShouldShowFocused(icons[i], true);
            this.showIcon(icons[i], focused);
        }
    },
    
    
    _hideIcons : function (icons) {
        if (icons == null || icons.length == 0) return;

        for (var i = 0; i < icons.length; i++) {
            this.hideIcon(icons[i]);
        }
    },
    
    //> @method FormItem.getIcon()
    // Given an +link{formItemIcon.name} return a pointer to the icon definition
    // @param name (string) specified +link{formItemIcon.name}
    // @return (FormItemIcon) form item icon matching the specified name
    // @visibility external
    //<
    getIcon : function (name) {
        if (name == null) return;
    
        var icon;
        if (this.icons) {
            if (isc.isA.Number(name)) {
                return this.icons[name];
            }
            for (var i = 0; i < this.icons.length; i++) {
                // make sure we fire the click action of the appropriate object in the 'icons' array
                if (this.icons[i] == name || this.icons[i].name == name) icon = this.icons[i];
            }
        }
        if (!icon && this.showPickerIcon) {
            if (isc.isAn.Object(name)) name = name.name;
            var pickerIcon = this.getPickerIcon();
            if (pickerIcon && pickerIcon.name == name) icon = pickerIcon;
        }
        if (!icon) {
            this.logInfo("FormItem unable to get pointer to icon with name:"+ name +
                         " - Invalid name, or icons array has been inappropriately modified." +
                         " To update icon[s] for some form item, use the method 'setIcons()'.")
        }
        return icon;
    },
    
    
    // _setIconImgState() - an internal method to show the 'over' / 'focused' image for an icon.
    _setIconImgState : function (icon, over, focused) {
        // We should already be showing the disabled image if we're marked as disabled.
        if (this.iconIsDisabled(icon)) return;
        
        // If we weren't explicitly passed 'focused', look at this.hasFocus 
        if (focused == null) focused = this.hasFocus && this._iconShouldShowFocused(icon, true)
        
        var iconImg = this._getIconImgElement(icon);
        if (iconImg != null) {

            var src = this.getIconURL(icon, over, null, focused);
            isc.Canvas._setImageURL(iconImg, src);
            var iconStyle = this.getIconStyle(icon, over, null, focused);
            if (iconStyle != null) iconImg.className = iconStyle;
        }
    },
    
    _iconShouldShowOver : function (icon) {
        if (!icon || this.iconIsDisabled(icon)) return false;
        if (icon.showOver != null) return icon.showOver;
        return this.showOverIcons;
    },
    
    _iconShouldShowFocused : function (icon, itemFocus) {
        if (!icon || this.iconIsDisabled(icon)) return false;
        if (itemFocus && icon.showFocusedWithItem == false) return false;
        if (icon.showFocused != null) return icon.showFocused;
        return this.showFocusedIcons;
    },
    
    // setIconBackgroundColor() - change the backgroundColor of an icon
    // (used by the colorItem).
    setIconBackgroundColor: function (icon, color) {
        
        icon.backgroundColor = color;        
        var iconImg = this._getIconImgElement(icon);
        if (iconImg != null) {
            
            try {iconImg.style.backgroundColor = color;} catch (e) {}
        }
    },
    
    // Picker
	// -----------------------------------------------------------------------------------------

    //> @method formItem.showPicker() (A)
    // Method to show a picker for this item. By default this method is called if the user
    // clicks on a +link{showPickerIcon,pickerIcon}.  May also be called programmatically.
    // <P>
    // Default implementation lazily creates and shows the +link{FormItem.picker,Picker Autochild}.
    // May be overridden to implement some custom picker for this item.
    //
    // @visibility external
    //<
    

    showPicker : function (modal, icon, pickerProperties, rect) {

        var picker = this.picker;

        pickerProperties = isc.addProperties(pickerProperties || {}, {
            callingForm: this.form, 
            callingFormItem: this
        });

        // support being passsed the global ID of a picker
        if (isc.isA.String(picker) && isc.isA.Canvas(window[picker])) {
            picker = this.picker = window[picker];
        }

        // lazily create the picker
        if (!picker) {
            picker = this.picker = this.createPicker(pickerProperties);

            // provide observeable dataChanged function to all pickers
            if (!isc.isA.Function(picker.dataChanged)) {
                picker.dataChanged = isc.Class.NO_OP;
            }

            // make sure the picker doesn't drift too far away from the original coordinates or
            // off the screen by resizing (items being added or removed)
            picker.observe(picker, "resized",
                                   "observed.placeNear(observed.lastShowRect)");

            // observe dataChanged 
            
            if (this.pickerDataChanged && picker.dataChanged) {
                this.observe(picker, "dataChanged", "observer.pickerDataChanged(observed)");
            }
        } else {
            isc.addProperties(picker, pickerProperties);
        }
        var pickerID = picker.getID();

        //this.logWarn("showPicker with rect: " + this.echo(rect) +
        //             ", getPickerRect: " + this.echoLeaf(this.getPickerRect) +
        //             ", icon: " + this.echo(icon));

        // if no position was specified, use either the top left of the (if it
        // exists) or the last mouse position
        if (!rect) {
            if (this.getPickerRect) {
                rect = this.getPickerRect();
            } else if (icon) {
                var iconRect = this.getIconPageRect(icon);
                rect = [iconRect[0],iconRect[1]]
            }
            else rect = [isc.EH.getX(), isc.EH.getY()];
        }
        // storing the lastShowRect allows the picker to reposition itself if it resizes
        picker.lastShowRect = rect;

        picker.setRect(rect);
        // draw the picker offscreen to get a size before placing it
        if (!picker.isDrawn()) {
            picker.moveTo(null, -9999);
            picker.draw();
        }
        // use placeNear so we don't get clipped by the window.
        this.picker.placeNear(rect);

        // set the picker data.  A picker advertises its desire to have data set on it by
        // defining a setData method.  If the formItem defines the special getPickerData()
        // function, call that - otherwise call getValue() which works for all formItems
        if (isc.isA.Function(picker.setData)) {
            if (picker._ignorePickerSetData) {
                // flag set by, eg, RelativeDateItem, which has already called setData()
                delete picker._ignorePickerSetData;
            } else {
                if (isc.isA.Function(this.getPickerData)) {
                    picker.setData(this.getPickerData(picker));
                } else picker.setData(this.getValue(picker));
            }
        }

        // show a clickmask.  When the clickmask is clicked notify the picker if it has the
        // clickMaskClicked method defined.  If we're asked to open a modal picker, the picker
        // needs to take care of hiding itself and clearing the clickMask.
        var clickAction = modal ? null : pickerID+".hide()";
        if (modal && isc.isA.Function(picker.clickMaskClicked)) 
            clickAction = pickerID+".clickMaskClicked()";
        
        picker.showClickMask(clickAction, !modal, picker);
        if (modal != null && picker.isModal == null) picker.isModal = modal;
        picker.show();
        picker.bringToFront();
        picker.focus();
        
        // Return false to suppress default click handling from firing
        return false;
    },
    
    createPicker : function (pickerProperties) {
        return this.createAutoChild("picker", pickerProperties);
    },

    hidePicker : function () {
        if (!this.picker) return;
        this.picker.hideClickMask();
        this.picker.hide();
    },

	// ----------------------------------------------------------------------------------------

    

    //> @method	formItem.redraw()  (I)
    // Redraw this form item.  Default implementation will notify containerWidget by calling 
    // containerWidget.redrawFormItem() (if it's present), otherwise just mark the containerWidget
    // for redraw.
    //<
    // DynamicForm 'redrawFormItem()' simply marks the form for redraw.
    
    redraw : function (reason) {
        // we can get redraw() attempts during init before we're actually drawn, which have no
        // effect on a DynamicForm, but will affect inline editing by redrawing the grid body
        if (!this.isDrawn()) return;

        // Note - We record whether we had focus before the redraw.
        // This is required because the form will blur the focus item during redraw, and then
        // refocus when redraw is complete.
        // In some cases getInnerHTML() will return different HTML for an item with focus
        // so we need this flag as the item will not have real focus until after redraw is 
        // complete.
        // This flag is cleared by DynamicForm._refocusAfterRedraw()
        
        
        if (this.hasFocus) this._hadFocusBeforeRedraw = true;
        if (!this.hasFocus && this.items != null) {
            for (var i = 0; i < this.items.length; i++) {
                if (this.items[i].hasFocus) this._hadFocusBeforeRedraw = true;
            }
        }
        if (this.containerWidget.redrawFormItem) {
            this.containerWidget.redrawFormItem(this, reason);
        } else {
            this.containerWidget.markForRedraw("Form item redrawn"+ (reason ? ": " + reason : isc.emptyString));
        }
    },
    
    // adjustOverflow
    // Called when content changes (which may cause size change)
    // By default calls adjustOverflow on DynamicForm 
    
    adjustOverflow : function (reason) {
        if (!this._adjustOverflowReason) {
            this._adjustOverflowReason = [this.getID(), "  overflow changed: "]
        }
        if (reason == null) this._adjustOverflowReason[2] = "No Reason Specified.";
        else this._adjustOverflowReason[2] = reason;
        
        if (isc.isA.DynamicForm(this.containerWidget)) {
            // shift canvasItems around if necessary
            this.containerWidget._placeCanvasItems();
            this.containerWidget.adjustOverflow(this._adjustOverflowReason.join(isc.emptyString));
        }
        
    },

    //> @method	formItem.show()  (I)
    // Show this form item.
    // <BR><BR>
    // This will cause the form to redraw.  If this item had an item.showIf expression, it will
    // be destroyed.
    // @visibility external
    //<
    // If the container widget has a redrawFormItem method we use that. This is currently only
    // implemented on the DynamicForm class where it is used to invalidate cached 
    // tableResizePolicy information.
    // If there is no redrawFormItem method we just mark the container widget for redraw
    
    show : function (preserveShowIf) {
        if (this.visible == true) return;
        this.visible = true;
        if (!preserveShowIf) this.showIf = null;
        if (this.containerWidget.redrawFormItem) this.containerWidget.redrawFormItem(this, "showing form item");
        else this.containerWidget.markForRedraw("showing form item");
        
        this.visibilityChanged(true);
    },

    //> @method	formItem.hide()  (I)
    // Hide this form item.
    // <BR><BR>
    // This will cause the form to redraw.  If this item had an item.showIf expression, it will
    // be destroyed.
    // @visibility external
    //<    
    hide : function (preserveShowIf) {
        if (this.visible == false) return;
        this.visible = false;
        if (!preserveShowIf) this.showIf = null;
        if (this.containerWidget.redrawFormItem) this.containerWidget.redrawFormItem(this, "hiding form item");
        else this.containerWidget.markForRedraw("hiding form item");
        
        this.visibilityChanged(true);        
    },
    
    //>Safari
    
    _updateHTMLForPageLoad : function () {
        if (!isc.Browser.isSafari || !this.isDrawn()) return;
        
        this._resetWidths();
    },
    
    _resetWidths : function () {
        if (!this.isDrawn()) return;
        var shouldClip = this.clipValue;
        
        var outerTable = this.getOuterTableElement();
        if (outerTable) outerTable.style.width = this.getInnerWidth();
        
        if (this._shouldShowPickerIcon()) {
            var controlTable = this._getControlTableElement();
            if (controlTable) controlTable.style.width = this.getElementWidth();
            
            var iconDef = this.getPickerIcon(),
                img = this._getIconImgElement(iconDef);
                if (img) {
                    img.style.height = this.getPickerIconHeight();
                    img.style.width = this.getPickerIconWidth();
                }
        }
        
        var textBoxWidth = this.getTextBoxWidth(),
            widthCSSText = (textBoxWidth == null ? isc.emptyString : textBoxWidth + isc.px),
            textBoxHeight = this.getTextBoxHeight(),
            heightCSSText = (textBoxHeight == null ? isc.emptyString : textBoxHeight + isc.px),
            textBox = this._getTextBoxElement();
        if (textBox) {
            
            if (shouldClip) textBox.style.width = widthCSSText;
            else textBox.style.minWidth = widthCSSText;
            textBox.style.height = heightCSSText;
        }
        if (this._writeOutFocusProxy()) {
            var focusProxy = this.getFocusElement()
            if (focusProxy) {
                focusProxy.style.width = widthCSSText;
                focusProxy.style.height = heightCSSText;
            } 
        }
    },
    //<Safari
 
    // Element management   
	// --------------------------------------------------------------------------------------------


	//>	@method	formItem.hasElement()
	//	Deprecated form of hasDataElement() - kept for backwards compat.
	//		@group	elements
	//		@return	(boolean)		true == item has a form element containing a value for the item
    //      @see    hasDataElement()
    //      @deprecated As of SmartClient 5.5, use +link{formItem.hasDataElement}.
	//<
    hasElement : function () {
        return this.hasDataElement();
	},
    
    //> @method     formItem.hasDataElement()   
    // Does this form item type have an associated form element in the DOM, containing a value?
    // Note - if hasDataElement() returns true, this implies that this data element type
    // has a data element - it doesn't imply that the form is drawn, or that the data element
    // is currently written into the DOM.
    // <P>
    // Use 'getDataElement()' to get a pointer to the data element (will return null if the
    // data element is not found).
    //
    // @group formValues
    // @visibility   internal
    // @see     method:FormItem.getDataElement
    //<
    
    hasDataElement : function () {
        // Most FormItems either always have or always do not have an element, however we make
        // this a function rather than accessing the _hasDataElement flag directly because
        // subclasses such as the ContainerItem class may do more complicated things which 
        // make this method's return value vary.
        if (this.showValueIconOnly) return false;
        return this._hasDataElement;
    },

	//>	@method	formItem.getElement()
	//  Deprecated form of getDataElement() - kept for backwards compatability.
	//		@group	elements
	//
	//		@param	[itemName] 	(string)	Item to get the element for.  If null, use this item.
	//		@return	(Element)		DOM element subclass
    //      @deprecated As of SmartClient 5.5, use +link{formItem.getDataElement}.
	//<
	getElement : function (itemName) {
        return this.getDataElement(itemName);
    },

    //> @method formItem.getFocusElement()
    // Returns the HTML element that should receive focus when 'focusInItem()' is called on this
    // form item.
    // Default implementation returns the data element for the form item. May be overridden by
    // subclasses
    //  @group events
    //  @return (Element) DOM element to receive native focus
    //  @visibility internal
    //<
    getFocusElement : function () {
        if (!this.isDrawn() || !this._canFocus()) return null;
        if (this.hasDataElement()) return this.getDataElement();
        if (this._writeOutFocusProxy()) {
            if (!this._focusProxyHandle) {
                // this ID is created by the Canvas-level focusProxy string generation
                this._focusProxyHandle = isc.Element.get(this.getID() + "__focusProxy");
            }
            return this._focusProxyHandle;
        }
        return this._canFocusInTextBox() ? this._getTextBoxElement() : null;
    },
    
    // _getCurrentFocusElement()
    // Since form items can consist of multiple focusable HTML elements (most commonly an input 
    // element and a number of icons (defined as <A> tags), we need a way to determine
    // which DOM element has native focus when formItem.hasFocus is true.
    // In IE we could rely on 'document.activeElement', but there is no equivalent in the
    // other browsers, so instead we hang a flag onto the form item on element focus, 
    // (via the 'nativeFocusHandler()' and 'iconFocus()' methods) and clear it on blur (or 
    // update on focus to a different element).
    _getCurrentFocusElement : function () {
        if (this.hasFocus == null && !isc.EH._lastFocusTarget == this) {
            return null;
        }
        var element = this._currentFocusElement;
        // double check for IE using the native document.activeElement - should not be 
        // necessary
        
        if (isc.Browser.isIE && element != this.getActiveElement()) {
            this.logInfo("not returning focus element " + this.echoLeaf(element) + 
                         " since it's not active" + isc.EH._getActiveElementText(),
                         "nativeFocus");
            if (this.hasFocus) {
                this.hasFocus = false;
                this.elementBlur();
            }
            this._currentFocusElement = null;
            return null;
        }
        
        return element;
    },

	//>	@method	formItem.getDataElement()
    //      Return a pointer to the form element containing the value for this form item, or
    //      null if it doesn't currently exist.
    //      Will always return null if this form item type does not have an associated data 
    //      element which can be determined by formItem.hasDataElement()
    //
	//		@group	elements
	//
	//		@param	[itemName]    (String)    Optional form item name - if passed will return that 
    //                                      item's element.  (Item should be a member of the same 
    //                                      form) 
	//		@return	(Element)		DOM element subclass (or null)
	//<
    getDataElement : function (itemName) {
		// if no itemName was specified, assume they mean us!
		if (itemName == null) {
			var item = this;
		} else {
			// otherwise have the form get a pointer to the item
            var item = this.form.getItem(itemName);
		}
        
        // If the item does not have a data element, return null.
        if (!item.hasDataElement()) return null;
        
        // If the item is not marked as drawn() bail.
        
        if (!this.isDrawn()) return;
        
        

        // cache the result of getElementById, cleared in redrawn()/cleared()/destroy()   
        var dataElement = this._dataElement;
        if (dataElement == null) {
            dataElement = (this._dataElement = isc.Element.get(this.getDataElementId()));
        }
        return dataElement
	},
    
    // This method returns a pointer to the outer element of the form item
    
    getOuterElement : function () {
        if (!this.isDrawn()) return null;
        
        // If the "includeHint" parameter was passed to getInnerHTML() when we were written out
        // we pass this on to the method determining whether we wrote an outer table.
        var hasHint = this._wroteOutHint;
        if (this._writeOuterTable(hasHint)) {   
            return this.getOuterTableElement();
        }
        if (this._shouldShowPickerIcon()) {
            return this._getControlTableElement();
        }
        var element = this._getTextBoxElement();
        // If all else fails (possible due to custom innerHTML) back off to the 
        // containing element for the entire item
        if (element == null) {  
            element = this.getHandle();
        }               
        return element;
    },    

    // getHandle() returns a pointer to the element that contains this form item.
    // One of:
    // - form cell
    // - abs div
    // - standalone 'span' element
    getHandle : function () {
        if (!this.isDrawn()) return null;
        if (this._absPos()) return this.getAbsDiv();
        if (this.containerWidget == this.form) return this.getFormCell();
        return isc.Element.get(this._getDOMID(this._$standaloneSpan));
    },
    
    // pointer to the table around this form item's content
    getOuterTableElement : function () {
        return this._getHTMLPartHandle(this._$outerTable);
    },
    
    // Which part of the form item did the event occur over?
    _overElement : function (event) {
        if (!event) event = isc.EH.lastEvent;
        var itemInfo = event.itemInfo;
        return (itemInfo && itemInfo.overElement);
    },
    
    _overTextBox : function (event) {
        if (!event) event = isc.EH.lastEvent;
        var itemInfo = event.itemInfo;
        
        return (itemInfo && (itemInfo.overTextBox || itemInfo.overElement));
    },

    // control table comprises the text box, picker icon and surrounding table
    _overControlTable : function (event) {
        if (!event) event = isc.EH.lastEvent;
        var itemInfo = event.itemInfo;
        return (itemInfo &&
                (itemInfo.overControlTable || this._overTextBox(event) ||
                 (itemInfo.overIcon && this.getIcon(itemInfo.overIcon) == this.getPickerIcon()) 
                )
               );
    },

    _$cell:"cell",
    getFormCellID : function () {
        return this._getDOMID(this._$cell);
    },
    getFormCell : function () {
        return isc.Element.get(this.getFormCellID());
    },

    // ValueMaps
	// --------------------------------------------------------------------------------------------
    
    //>	@method	formItem.getDisplayValue()
    // Returns this item's value with any valueMap applied to it - the value as currently
    // displayed to the user.
    // @param [value] optional stored value to be mapped to a display value.  Default is to
    //                use the form's current value
	// @return (any) value displayed to the user
    // @group valueMap
    // @visibility external
	//<
    
    getDisplayValue : function (value) {
        var undef;
        if (this.multiple) {
            var useCurrentValue = false;

            if (value === undef) {
                value = this.getValue();
                useCurrentValue = true;
            }

            if (!(value == null || isc.isAn.Array(value))) {
                if (useCurrentValue) {
                    this.logWarn(
                            "getDisplayValue - this is a multiple FormItem but the value obtained " +
                            "from getValue() was not null and was not an array.");
                    value = [value];
                } else {
                    // The form item is `multiple: true` and the caller passed in a value that
                    // is not null and not an array.  Assume that the caller is seeking the
                    // display value for the single value that was passed in.
                    return this.mapValueToDisplay(value);
                }
            }

            if (value != null) {
                var displayValue = [];
                for (var i = 0, len = value.length; i < len; ++i) {
                    displayValue.push(this.mapValueToDisplay(value[i]));
                }
                return displayValue;
            }
        }

        return this.mapValueToDisplay(value !== undef ? value : this.getValue());
    },
    
	//>	@method	formItem.mapValueToDisplay()
	//  Given a value for this form item, return the value to be displayed.
    //  Default implementation will apply valueMap if there is one.  May be overridden for 
    //  other implementations by subclasses
	//	@param  value  (any) value to be mapped to a display value
    //  @return (any)        value to display
	//<
    
    _$nbsp:"&nbsp;",
    mapValueToDisplay : function (value) {
        // escapeHTML is not doc'd at the formItem level. It doesn't make sense for
        // all form item types, such as those with a native HTML input element, so will
        // be enabled via a flag where we need it.
        var asHTML = this.canEscapeHTML && 
                    // outputAsHTML / asHTML are old and deprecated
                    (this.escapeHTML || this.outputAsHTML || this.asHTML);

        var displayValue;
        
        if (this.multiple && isc.isAn.Array(value)) {
            // For multi-select items, value might be an array of selected values.
            // Display them as a list of display values, separated by the multipleValueSeparator.
            displayValue = "";
            for (var i = 0, length = value.length; i < length; i++) {
                var key = value[i],
                    valueIconHTML = this._getValueIconHTML(key);

                if (valueIconHTML != null && length > 1) {
                    displayValue += valueIconHTML;
                }

                displayValue += this.mapValueToDisplay(key);

                if (i != length - 1) {
                    displayValue += this.multipleValueSeparator;
                }
            }
        } else {
            displayValue = this._mapKey(value, true);
            if (displayValue == null) {
                var displayField = this.getDisplayFieldName();
                if (displayField != null) {
                    // Try looking in the option data source's cache data.
                    var ods = this.getOptionDataSource();
                    var odsCacheData = (ods == null ? null : ods.getCacheData());
                    if (odsCacheData != null) {
                        var optionRecord = odsCacheData.find(this.getValueFieldName(), value);
                        if (optionRecord != null) displayValue = optionRecord[displayField];
                    }
                }
            }
            displayValue = this._formatDataType(displayValue != null ? displayValue : value);

            // Don't escape &nbsp; unless that's actually the data value!
            if (asHTML && (value == null || value == isc.emptyString)
                && displayValue == this._$nbsp)
            {
                asHTML = false;
            }
            if (asHTML && isc.isA.String(value)) {
                displayValue = displayValue.asHTML();
            }
        }
        return displayValue;
    },
    canEscapeHTML:false,
    
    //> @method formItem.formatValue()
    // Allows customization of how the FormItem's stored value is formatted for display.
    // <p>
    // By default, this formatter will only be applied to static displays such
    // as +link{StaticTextItem} or +link{SelectItem}, and does not apply to values 
    // displayed in a freely editable text entry field 
    // (such as a +link{TextItem} or +link{TextAreaItem}).
    // <p>
    // To define formatting logic for editable text, developers may:
    // <ul>
    // <li>set +link{textItem.formatOnBlur} to true, which causes the static formatter
    // to be applied while the item does not have focus, and then be cleared when the user
    // moves focus to the text field</li>
    // <li>use +link{formatEditorValue} and supply a
    // corresponding +link{parseEditorValue} that can convert a formatted and subsequently
    // user-edited value back to a stored value.</li>
    // </ul>
    // @param value (any) Underlying data value to format. May be null.
    // @param record (ListGridRecord) The record currently being edited by this form.
    //      Essentially the form's current values object.
    // @param form (DynamicForm) pointer to the DynamicForm
    // @param item (FormItem) pointer to the FormItem
    // @return (string) Display value to show.
    // 
    // @example formatRelatedValue
	// @visibility external
	//<
    
    //> @method formItem.formatEditorValue()
    // Allows customization of how the FormItem's stored value is formatted for display
    // in an editable text entry area, such as a +link{TextItem} +link{TextAreaItem}.  For
    // display values which will not be directly editable by the user, use
    // +link{formItem.formatValue()} instead.
    // <p>
    // When customizing how values are displayed during editing, it is almost always necessary
    // to provide a +link{formItem.parseEditorValue()} as well, in order to convert a formatted
    // and subsequently user-edited value back to a stored value.
    //
    // @param value (any) Underlying data value to format. May be null.
    // @param record (ListGridRecord) The record currently being edited by this form.
    //      Essentially the form's current values object.
    // @param form (DynamicForm) pointer to the DynamicForm
    // @param item (FormItem) pointer to the FormItem
    // @return (string) display value to show in the editor.
    // 
	// @visibility external
	//<
    
    //> @method formItem.parseEditorValue()
    // Allows customization of how a used-entered text value is converted to the FormItem's
    // logical stored value (the value available from +link{getValue()}).  
    // <p>
    // This method only applies to form items which show an editable text entry area, such at
    // the +link{TextItem} or +link{TextAreaItem}.
    // <p>
    // See also +link{formItem.formatEditorValue()}
    //
    // @param value (string) value as entered by the user
    // @param form (DynamicForm) pointer to the dynamicForm containing this item
    // @param item (FormItem) pointer to this item
    // @return (any) Data value to store for this item.
    // 
    // @visibility external
    //<
    
    //> @method formItem.formValuesChanged()
    // Notification that fires when the parent form's values are changed by a
    // +link{ValuesManager}.
    // @visibility internal
    //<    

    // should we apply static formatters to the display value
    // We typically want to do this for readOnly fields.
    // For fields with a freeform text entry area (text / textArea) we don't want to use the
    // static formatter -- instead we'll use the special "edit value" formatters which should
    // have a corollary parser method.
    // Note that for some formItems, such as LinkItem, this is flipped based on the
    // 'canEdit' setting for the item
    // If formatOnBlur is true, we always apply the static format while the item is unfocussed
    // This gives developers an easy way to specify a formatter without needing a
    // corollary parser method.
    applyStaticTypeFormat:true,
    shouldApplyStaticTypeFormat : function () {
        if (this.applyStaticTypeFormat) return true;
        if (this.formatOnBlur) {
            var hasFocus = this.hasFocus;
            // If we're blurred, apply the staticTypeFormat
            return !hasFocus;
        }
        return false;
    },
    
    // If we have a non-string value, use the appropriate formatter to display it as a string.
    
    _formatDataType : function (value, applyStaticTypeFormat) {
        if (applyStaticTypeFormat == null) {
            applyStaticTypeFormat = this.shouldApplyStaticTypeFormat();
        }
        if (applyStaticTypeFormat) {
            if (this.formatValue != null) {
                
                var form = this.form,
                    record = this.form ? this.form.values : {};
                return this.formatValue(value,record,form,this);
                
            } else {
                if ((isc.isA.Number(value) || isc.isA.Date(value)) && this.format) {
                    return isc.isA.Number(value) ? isc.NumberUtil.format(value, this.format)
                                                 : isc.DateUtil.format(value, this.format);
                }
            }
        } else if (this.formatEditorValue != null) {
            var form = this.form,
                record = this.form ? this.form.values : {};
            return this.formatEditorValue(value,record,form,this);
            
        } else if (this._editFormatter != null) {
            
            var form = this.form,
                record = this.form ? this.form.values : {};
            // if we have a _simpleType - editFormatter was presumably derived from it.
            // However don't crash if _simpleType.editFormatter is undefined.
            
            if (this._simpleType && this._simpleType.editFormatter) {
                return this._simpleType.editFormatter(value, this, form, record);
            } else {    
                return this._editFormatter(value, this, form, record);
            }
        }
    
        // If the value is a native Date object format it according to the following rules:
        // - if this.dateFormatter or this.timeFormatter is specified, respect it (If both are
        //   specified, favor 'dateFormatter' unless field is explicitly of type "time")
        // - if this.displayFormat is specified respect it as either a dateFormatter or timeFormatter
        //   depending on specified field type.
        // - otherwise check for form.timeFormatter for time fields, form.datetimeFormatter for
        //   datetime fields, or form.dateFormatter for all other field types.
        if (isc.isA.Date(value)) {
            if (this._formatAsTime()) {
                var formatter = this._getTimeFormatter();
                var isLogicalTime = isc.SimpleType.inheritsFrom(this.getType(), "time");
                return isc.Time.toTime(value, formatter, isLogicalTime);
            } else {
                
                var formatter = this._getDateFormatter();
                var type = this.getType(),
                    dateField = isc.SimpleType.inheritsFrom(type, "date"),
                    datetimeField = isc.SimpleType.inheritsFrom(type, "datetime");
                // Logical date fields -- always use short format (time is meaningless) and pass
                // in the "logicalDate" parameter so we ignore any custom timezone.
                if (dateField && !datetimeField) {
                    return value.toShortDate(formatter, false);
                
                // Otherwise, if showing short format, use toShortDate or toShortDatetime for
                // explicit datetime fields -- or for long format use default "normal" formatter.
                
                } else {
                    if (this.useShortDateFormat) {
                        return datetimeField ? value.toShortDatetime(formatter, true) 
                                            : value.toShortDate(formatter, true);
                    } else {
                        return value.toNormalDate(formatter);
                    }
                }
            }
        }

        // _normalDisplayFormatter and _shortDisplayFormatter is picked up from SimpleType
        // logic.
        
        //
        // Note: if the simpleType with the formatter exists on the field, use that in
        // preference to the method patched from the simpleType because use of 'this' within
        // said formatter then correctly references the simpleType and not the field.
        if (this._simpleType && isc.isA.Function(this._simpleType.normalDisplayFormatter) &&
            applyStaticTypeFormat) 
        {
            
            value = this._simpleType.normalDisplayFormatter(value, this, this.form, this.form.values);

        } else if (this._normalDisplayFormatter && applyStaticTypeFormat) {
            
            value = this._normalDisplayFormatter(value, this, this.form, this.form.values);

        } else if (value != null) {
            
            value = isc.iscToLocaleString(value);
            // map "" to our 'emptyDisplayValue' - allows subclasses such as selectItems
            // to style the content properly by writing out "&nbsp;" rather than ""
            if (this.emptyDisplayValue != isc.emptyString && value == isc.emptyString) 
                value = this.emptyDisplayValue;

        }
        if (value == null) value = this.emptyDisplayValue;
        return value;
    },
    
    // Helper methods to determine how to format date values.
    _formatAsTime : function () {
        var type = this.getType(),
            isTime = isc.SimpleType.inheritsFrom(type, "time"),
            formatAsTime = isTime;
            
        // at the item level, the presence of timeFormatter but no date, or vice-versa
        // implies we should use the formatter regardless of type.
        if (this.timeFormatter == null && this.dateFormatter != null) formatAsTime = false;
        if (this.dateFormatter == null && this.timeFormatter != null) formatAsTime = true;
        // If neither are set, rely on type inheriting from time - anything will format as a date.
        return formatAsTime;
    },
    _getDateFormatter : function () {
    
        if (this.dateFormatter != null) return this.dateFormatter;
        var type = this.getType(),
            isDate = isc.SimpleType.inheritsFrom(type, "date"),
            isDatetime = isc.SimpleType.inheritsFrom(type, "datetime");
        
        // 'displayFormat' may also be specified as either a date or time formatter
        // this expects the field to have a specified "type".
        
        if (isDate && this.displayFormat != null) return this.displayFormat;
        
        if (isDatetime && this.form.datetimeFormatter != null) return this.form.datetimeFormatter;
        return this.form.dateFormatter;
    },
    _getTimeFormatter : function () {
        if (this.timeFormatter != null) return this.timeFormatter;
        // 'displayFormat' may also be specified as either a date or time formatter
        // this expects the field to have a specified "type".
        
        if (this.displayFormat != null && isc.SimpleType.inheritsFrom(this.type, "time")) {
            return this.displayFormat;
        }
        return this.form.timeFormatter;
    },

    
    //>	@method	formItem.mapDisplayToValue()
	//  Converts a display value for this item to a value to be saved out.
    //  Default implementation will map backwards based on the valueMap specified if there is 
    //  one.
	//	@param  value  (any) display value
    //  @return (any)        value re-mapped for storing
	//<
    
    
    mapDisplayToValue : function (value) {
        value = this._parseDisplayValue(value);
        return this._unmapKey(value);
    },    
    
    // If this is an item with data type set to "time", and the user enters an 
    // unconvertible string, should we accept it?
    
    forceTimeConversion : function () {
        return false;
    },
    
    _parseDisplayValue : function (value) {
        var applyStaticTypeFormat = this.shouldApplyStaticTypeFormat();
        if (!applyStaticTypeFormat) {
            if (this.parseEditorValue != null) {
                value = this.parseEditorValue(value, this.form, this);
            } else if (this._parseInput != null) {
                var form = this.form,
                    record = form ? form.values : {};
                // fire it in the scope of the simpleType
                if (this._simpleType && this._simpleType.parseInput) {
                    value = this._simpleType.parseInput(value, this, form, record);
                } else {
                    value = this._parseInput(value, this, form, record);
                }
            }
            // Handle parsing values to Dates for fields of type "date"
            // This is rarely going to be required but would handle something special like
            // the developer showing a date type field with 'editorType' explicitly set to
            // "TextItem"
            
            if (value != null && isc.isA.String(value)) {
                var type = this.getType();
                var isDate = isc.SimpleType.inheritsFrom(type,"date"),
                    isTime = isc.SimpleType.inheritsFrom(type,"time"),
                    isDatetime = isDate && isc.SimpleType.inheritsFrom(type, "datetime"),
                    isEmptyString = (value == "")
                ;

                if (isDate || isTime) {
                    if (this._formatAsTime()) {
                        if (isEmptyString && this.allowEmptyValue) {
                            // handle empty time-values
                            value = null;
                        } else {
                            
                            var baseDate;
                            if (!isTime && isc.isA.Date(this._value)) {
                                baseDate = this._value;
                            }
                            var timeVal = isc.Time.parseInput(
                                            value, !this.forceTimeConversion(), 
                                            false, !isTime, baseDate);
                            if (isc.isA.Date(timeVal)) value = timeVal;
                        }
                    } else {
                        var inputFormat = this.inputFormat;
                        if (inputFormat == null) {
                            inputFormat = Date.mapDisplayFormatToInputFormat(this._getDateFormatter());
                        }
                        var logicalDate = isDate && !isDatetime;
                        
                        var dateVal = Date.parseInput(value, inputFormat, this.centuryThreshold, 
                                        false, !logicalDate);
                        if (isc.isA.Date(dateVal)) value = dateVal;
                    }
                }
            }
        }
        return value;
    
    },
    
    // getType() - returns the specified 'type' for this item
    // If this.criteriaField is specified, type will be picked up from that field
    
    getType : function () {
        if (this.type != null) return this.type;
        if (this.criteriaField && this.form && this.form.dataSource) {
            var ds = isc.DataSource.get(this.form.dataSource);
            var criteriaField = ds.getField(this.criteriaField);
            if (criteriaField) return criteriaField.type;
        }
        return null;
    },
    
    // Helper to set the time on a date to zero for a datetime
    
    setToZeroTime : function (date) {
        Date.setToZeroTime(date);
    },
	
	//>	@method	formItem._mapKey() (A)
	// Map a key value through the item.valueMap, if defined,
	// to return the display value that we should show to the user.
    // By default returns the key if no mapping was found. 2nd parameter allows the developer
    // to suppress this behavior, and return null if no mapping was found.
	//<
	_mapKey : function (key, dontReturnKey) {
        // assert !isc.isAn.Array(key)

        var defaultValue = dontReturnKey ? null : key;

		var map = this.getValueMap();
		if (!map) return defaultValue;		
		if (isc.isA.String(map)) map = this.getGlobalReference(map);

        // If it's an array, just return the key.  It's either in the array or not - no need
        // to transform.
        if (isc.isAn.Array(map)) return defaultValue;

        return isc.getValueForKey(key, map, defaultValue);
	},

	//>	@method	formItem._unmapKey() (A)
	//		Map a display value through the item.valueMap, if defined,
	//		to return the key value used internally.
	//<
	_unmapKey : function (value) {
//JMD: handle null value in isc.getKeyForValue instead?
		var map = this.getValueMap();
		if (!map) return value;		
		if (isc.isA.String(map)) map = this.getGlobalReference(map);

        // if it's an array, just return the value, it's either in the array or not - no need
        // to transform.
        if (isc.isAn.Array(map)) return value;
        
		var result = isc.getKeyForValue(value, map);
        // if getKeyForValue returns the same value it was passed, and that happens to also
        // be the emptyDisplayValue for this item, don't allow the emptyDisplayValue to be
        // promoted to the internal value
        if (result == value && result == this.emptyDisplayValue) {
            result = "";

            var valueField = this.getValueFieldName();
            if (valueField != null) {
                // Try looking in the option data source's cache data.
                var ods = this.getOptionDataSource();
                var odsCacheData = (ods == null ? null : ods.getCacheData());
                if (odsCacheData != null) {
                    var optionRecord = odsCacheData.find(this.getDisplayFieldName(), value);
                    if (optionRecord != null) result = optionRecord[valueField];
                }
            }
        }
        return result;
	},

	//>	@method	formItem.setValueMap()	(A)
	// Set the valueMap for this item.
	// @group	valueMap
	// @param	valueMap (Array or Object) new valueMap
    // @see attr:valueMap
    // @visibility external
	//<
	setValueMap : function (valueMap) {
		this.valueMap = valueMap;
        
        this.updateValueMap();
	},
	
	//> @method formItem.setOptionDataSource() [A]
	// Method to set the +link{formItem.optionDataSource} at runtime
	// @param dataSource (DataSource) new optionDatasource
	// @visibility external
	//<
	setOptionDataSource : function (dataSource) {
	    if (isc.isA.String(dataSource)) dataSource = isc.DataSource.get(dataSource);
	    if (this.getOptionDataSource() == dataSource) {
	        return;
	    }
	    this.ignoreOptionDataSource();
	    this.optionDataSource = dataSource;
	    // This in turn calls updateValueMap
	    this.invalidateDisplayValueCache();
	},
    
    //> @method formItem.setValueIcons ()
    // Set the valueIcons for this item
    // @param map (object) mapping of logical values for this item to icon src URLs
    // @group valueIcons
    // @visibility external
    //<
    setValueIcons : function (map) {
        this.valueIcons = map;
        if (this.isDrawn()) this.redraw();
    },

	//>	@method	formItem.setOptions()	(A)
	// Set the options for this item (a select or a radioGroup, etc.).  Synonymous with
    // setValueMap().
	//		@group	valueMap
	//		@param	valueMap (Array or Object) new valueMap
	//<
	setOptions : function (valueMap) {
		return this.setValueMap(valueMap);
	},

    //> @method formItem.updateValueMap()
    // Helper method fired whenever the valueMap is modified.
    // Will refresh the displayed value if appropriate.
    // @param   refreshDisplay  (boolean)   Can be passed to explicitly indicate that the new
    //                                      valueMap effects the currently displayed value so
    //                                      a refresh is required, or vice versa. If not passed
    //                                      we always refresh.
    //<
    updateValueMap : function (refreshDisplay) {
        if (refreshDisplay != false && !this._showingInFieldHint) {
            this._setElementValue(this.getDisplayValue());
        }
		if (this.hasElement()) this.setElementValueMap(this.getValueMap());
    },
    
	//>	@method	formItem.setElementValueMap()	(A)
	// Set the valueMap in the form representation for this object.<p>
    //
	// Default implementation does nothing -- override in a subclass to actually manipulate the
    // form.
	//		@group	valueMap
	//		@param	valueMap (Array or Object) new valueMap
	//<
	setElementValueMap : function (valueMap) {
		// no default implementation
	},
	
	//>	@method	formItem.getValueMap()	(A)
	// Internal method to compute the actual valueMap from the author-specified valueMap and
    // other properties.
	//		@group	valueMap
	//		@return	(Object) the valueMap
	//<
	getValueMap : function () {

		// get the valueMap from the item
		var valueMap = this.valueMap;
	
		// if valueMap are specified as a string, treat it as a global reference to the actual
        // list
		if (isc.isA.String(valueMap)) {
			valueMap = this.getGlobalReference(valueMap);
		}

        // for FormItems with displayFields, this._displayFieldValueMap is a special map between 
        // data field values and display field values in the items' optionDataSource.
        // Set up in 2 ways:
        // - if the optionDataSource matches the dataSource for the form, this is picked up
        //   from a call to setValues() on the form as a whole (EG editing records)
        // - if the value for the item is set to an unrecognized value as part of 
        //   item.setValue(), mapValueToDisplay will perform an explicit fetch against the 
        //   dataSource to retrieve the displayValue for the value passed in.
        // Combine this special map with the explicitly specified valueMap.
        var displayMap = this._displayFieldValueMap;
        if (displayMap != null) {
            if (valueMap == null) valueMap = displayMap;
            
            else {
                // if the explicit map is an array, convert it to an object
                if (isc.isAn.Array(valueMap)) {
                    var explicitMap = valueMap;
                    valueMap = {};
                    for (var i = 0; i < explicitMap.length; i++) {
                        valueMap[explicitMap[i]] = explicitMap[i];
                    }
                }
                // Add entries for the special displayFieldValueMap
                // Note that the explicitly specified entries should take precedence
                valueMap = isc.addProperties({}, valueMap);
                var undef;
                for (var prop in displayMap) {
                    if (valueMap[prop] === undef) valueMap[prop] = displayMap[prop];
                }
            }
        }
        
        return valueMap;
    },

    //> @method FormItem.getValueFieldName()
    // Getter method to retrieve the +link{FormItem.valueField} for this item.
    // If unset, default behavior will return the +link{FormItem.name} of this field.
    // @group display_values
    // @return (string) fieldName to use a "value field" in records from this items 
    //              +link{FormItem.optionDataSource}
    // @visibility external
    //<
    
    getValueFieldName : function () {
        if (this.valueField) return this.valueField;
        
        if (this.form && this.form.dataSource && this.foreignKey) 
            return isc.DS.getForeignFieldName(this, this.form.dataSource);

        var fieldName = this.getFieldName();

        
        
        
        return fieldName || "name";
    },
    
    //> @method   FormItem.getDisplayFieldName()
    // Returns the <code>displayField</code> for this item. This will typically be
    // specified explicitly via the +link{formItem.displayField} attribute. However, if 
    // that property is unset, and the +link{formItem.valueField} for this item is 
    // hidden in the +link{formItem.optionDataSource}, this method
    // will return the title field for the <code>optionDataSource</code>.
    //
    // @return (String) display field name, or null if there is no separate display field to use.
    // @visibility external
    //<
    getDisplayFieldName : function () {
        if (this.displayField) return this.displayField;
        var optionDataSource = this.getOptionDataSource();
        
        var valueFieldName = this.getValueFieldName();

        if (optionDataSource && 
            optionDataSource != isc.DataSource.getDataSource(this.form.dataSource) && 
            
            optionDataSource.getField(valueFieldName) &&
            optionDataSource.getField(valueFieldName).hidden == true) {
                return optionDataSource.getTitleField();
        }
    },


    // If this item has a specified displayField, and no specified optionDataSource
    // we can pick up the display value for the field from the displayField value of the form's
    // values object
    // (This is based on the assumption that we are editing a 'record' - similar behavior
    // to the ListGrid).
    // See DynamicForm._useDisplayFieldValueFromRecord()
    
    _displayFieldValueFromFormValues : function () {
        // for items with an option dataSource and a specified displayField, display the 
        // form's displayField value by default
        
        if (this.displayField != null) {
            var vals = this.form.getValues(),
                dataVal = vals[this.getFieldName()],
                displayVal = vals[this.displayField];
            if (displayVal != null) {
                var valueMap = {};
                valueMap[dataVal] = displayVal;
            }
            this._displayFieldValueMap = valueMap;
        }
    },
	
	//>	@method	formItem.getOptions()	(A)
	// Return the valueMap for this item.  Synonymous with getValueMap()
	//		@group	valueMap
	//
	//		@return	(Object) the valueMap
	//<
	getOptions : function () {
		return this.getValueMap()
	},
    

    //> @method FormItem.getOptionDataSource()
    // Returns the +link{FormItem.optionDataSource} for this item.  
    // <p>
    // Always uses <code>item.optionDataSource</code> if specified.  Otherwise, if
    // +link{dataSourceField.foreignKey} was specified, uses the target DataSource.  Otherwise,
    // uses the DataSource of this item's form (if one is configured).  
    //
    // @return (DataSource) the optionDataSource, or null if none is configured
    // @group display_values
    // @visibility external
    //<
    getOptionDataSource : function () {
        var ods = this.optionDataSource;
       
        if (ods == null) {
            var formDS = this.form ? this.form.getDefaultOptionDataSource(this) : null;

            // use foreignKey if specified
            // Will back off to form-ds if the foreignKey is unqualified (no dot)
            if (this.foreignKey) ods = isc.DS.getForeignDSName(this, formDS);

            // otherwise fall back to DataSource for form as a whole
            if (ods == null && formDS) ods = formDS;
        }        
        // convert identifiers to an actual datasource object
        if (isc.isA.String(ods)) ods = isc.DataSource.getDataSource(ods);
        
        return ods;
    },
    
	//>	@method	formItem.getValueMapTitle()	(A)
	// Return the title associated with a particular value
	//		@group	valueMap
	//		@return	(string)	title of the option in question
	//<
	getValueMapTitle : function (value) {
		var valueMap = this.getValueMap();
		// return the value as the title if it exists in the valueMap array
		if (isc.isAn.Array(valueMap)) return (valueMap.contains(value) ? value : "");
		return valueMap[value];
	},
	
	// --------------------------------------------------------------------------------------------

	//>	@method	formItem.saveValue()
	// Store a value for this form item internally, and at the form level.<br>
    // This method will update our internal "_value" property and the value stored in the form's
    // "values" array.
    // It is used in 'setValue()', and in  'elementChanged()', and 'handleKeyPress()' to ensure the 
    // stored values for this item reflect the value displayed in this form item's element.
    //      @visibility internal
	//		@group formValues
	//
    // @param	value 	(any)				value to save for this item
    // @param [isDefault] (boolean) Indicates that this value was derived from the default 
    //  value for this item (allowing us to re eval dynamic defaults in setItemValues())
	//<
    saveValue : function (value, isDefault) {
    
        //this.logWarn("saving value: " + value + this.getStackTrace());
        
        var undef;
        this._value = value;
        // set or clear the flag indicating whether this is a default value.
        this._setToDefault = isDefault;

        // This value is going to be saved on the form itself under form.values.
        // If we have a hidden data element (for direct submission), update it now so that
        // when the form gets submitted the element value is present.
        if (this.isDrawn()) {
            if (this._useHiddenDataElement()) this._setHiddenDataElementValue(value);      
        }
               
        if (this.form == null) return;

        if (value == undef && this._clearingValue) {
            this.form.clearItemValue(this);
        } else {
            this.form.saveItemValue(this, value);
        }
    },
    
    // If we're using a hidden data element, this method will set its value, so when the form
    // is natively submitted the value is available to the server.
    _setHiddenDataElementValue : function (value) {
        var hde = this._getHiddenDataElement();
        if (hde) hde.value = value;
    },
    
    //>	@method	formItem.setValue()
	// Set the value of the form item to the value passed in
    // <p>
	// NOTE: for valueMap'd items, newValue should be data value not displayed value
    // @visibility external
	// @param	newValue 	(any)				value to set the element to
    //<
    // @param   [allowNullValue]   (boolean) Internal parameter to avoid setting to default when
    // passed a null value. Used when redrawing a form item that the user has explicitly set
    // to null as opposed to a call to 'setValue(null)' which will reset to default.
    _$smart:"smart",
	setValue : function (newValue, allowNullValue, timeCritical, dontResetCursor) {

        
        this._setValueCalled = true;

        // If we have focus, remember the selection so we can retain the cursor insertion point
        // - useful for the case where this is a simple data transform, such as case-shifting
        var resetCursor = !dontResetCursor && 
                           (this.maintainSelectionOnTransform && this.hasFocus && 
                           (this._getAutoCompleteSetting() != this._$smart));

        
        if (resetCursor && isc.Browser.isIE) {
            if (!this._hasNativeFocus()) {
                resetCursor = false;
            }
        }
                           
        if (resetCursor) this.rememberSelection(timeCritical);
        
        // since we're being set to an explicit value, cancel delayed save on keyPress
        if (this._pendingUpdate != null) {
            isc.Timer.clearTimeout(this._pendingUpdate);
            this._pendingUpdate = null;
        }
        
        // use the default value if necessary.
        
        var isDefault;

        if (newValue == null && !allowNullValue) {
            var defaultVal = this.getDefaultValue();
            // don't apply the default value if it's not set - this allows for the distinction 
            // between setting the value to 'null' vs 'undefined'
            if (defaultVal != null) {
                isDefault = true;
                newValue = defaultVal;
            }
        }
        // If the form item is `multiple` then the value of the form must be an array.
        if (this.multiple && newValue != null && !isc.isAn.Array(newValue)) {
            newValue = [newValue];
        }
		// truncate newValue to the length of the field, if specified
		if (this.length != null && newValue != null && isc.isA.String(newValue) &&
            newValue.length > this.length) 
        {
			newValue = newValue.substring(0, this.length);
		}         
        // saveValue will store the value as this._value, and will save the value in the form 
        // if this.shouldSaveValue is true
        this.saveValue(newValue, isDefault);

        this._showValue(newValue, resetCursor);

        return newValue
	},

	
	_showValue : function (newValue, resetCursor) {
	    // shouldFetchMissingValue() tests for whether we should fetch values at all 
	    // (option dataSource, fetchMissingValues etc) and whether we already have the
	    // value cached.
	    if (newValue != null) {
	        if (this.multiple) {
	            // assert isc.isAn.Array(newValue) // enforced above
	            var shouldFetchValues = [];
	            for (var i = 0, len = newValue.length; i < len; ++i) {
	                var val = newValue[i];
	                if (val != null && this.shouldFetchMissingValue(val)) {
	                    shouldFetchValues.push(val);
	                }
	            }
	            this._clearSelectedRecord();
	            this._checkForDisplayFieldValue(shouldFetchValues);
	        } else if (this.shouldFetchMissingValue(newValue)) {
	            // _checkForDisplayFieldValue() will kick off a fetch (Unless we're already pending a
	            // response for this value).
	            // Drop the current selected record - it's invalid right now and will be
	            // repopulated when the fetch completes
	            this._clearSelectedRecord();
	            this._checkForDisplayFieldValue(newValue);
	        }
	    } else {
	        // update the selected record from cache unless we already have it set up correctly.
	        if (this._selectedRecordValue == null || 
	                !this.compareValues(this._selectedRecordValue, this._value))
	        {
	            this._updateSelectedRecord();
	        }
	    }

	    // map the value passed to the visible value as necessary
	    var displayValue = this.getDisplayValue(newValue);

	    // set the value of the item
	    
	    this._setElementValue(displayValue, newValue);

	    // On simple data transforms (currently case shifting only), we will retain the
	    // cursor positon across setValue() calls if the item has focus
	    if (resetCursor) this.resetToLastSelection(true);
	},

	//>@method formItem.shouldFetchMissingValue()
	// If this field has a specified +link{optionDataSource}, should we perform a fetch against
	// that dataSource to find the record that matches this field's value?
	// <P>
	// If the value is non-null, this method is called when the item is first rendered 
	// or whenever the value is changed via a call to +link{setValue()}. If it returns 
	// true, a fetch will be dispatched against the optionDataSource to get the record
	// matching the value
	// <P>
	// When the fetch completes, if a record was found that matches the
	// data value (and the form item value has not subsequently changed again), 
	// the item will be re-rendered to reflect any changes to the display value,
	// and the record matching the value
	// will be available via +link{FormItem.getSelectedRecord(),this.getSelectedRecord()}.
	// <P>
	// Default behavior will return false if +link{FormItem.fetchMissingValues,this.fetchMissingValues} is 
	// set to false. Otherwise it will return true if +link{FormItem.alwaysFetchMissingValues,this.alwaysFetchMissingValues} is
	// set to true, or if a +link{displayField} is specified for this item and the item 
	// value is not already present in the item's valueMap.
    //
    // @param newValue (any) The new data value of the item.
	// @return (Boolean) should we fetch the record matching the new value from the
	//   item's optionDataSource?
	// @visibility external
	//<
	shouldFetchMissingValue : function (newValue) {
	    if (this.fetchMissingValues == false) return false;
	    if (this.getOptionDataSource() == null) return false;
	    // If we already saw this data value and performed a fetch against it, don't kick off another
	    // fetch even if alwaysFetchValues is true.
	    var inCache = false;
	    if (this._displayFieldCache != null && 
	        // _gotAllOptions basically indicates that filterLocally was true when we 
	        // populated our cache, so even if we can't find the record we don't need to
	        // fetch again.
	        (this._gotAllOptions ||
	        this._displayFieldCache.find(this.getValueFieldName(), newValue) != null)) 
	    {
	        inCache = true;
	    }
	    if (inCache) return false;
	    // Fetch missing value if the flag to always fetch is true, or if we have
	    // a displayField specified and don't have the value explicitly in our valueMap already.
	    if (this.alwaysFetchMissingValues) return true;
	    
	    // return true if we have a displayField set and we don't have the
	    // value in our valueMap
	    if (this.getDisplayFieldName() == null) return false;
	    var inValueMap = (this._mapKey(newValue, true) != null);
	    return !inValueMap;
	},
	
    // used by Visual ISC only
    setDefaultValue : function (newValue) {
        var prevDefaultValue = this.defaultValue, undef;
        this.defaultValue = newValue;
        if (this.isSetToDefaultValue() || (this._value == null && prevDefaultValue === undef)) 
            this.clearValue();
    },
        
    _checkForDisplayFieldValue : function (newValue) {
        // Prevent redraw/fetch cycle
        if (this._skipCheckForDisplayFieldValue) {
            delete this._skipCheckForDisplayFieldValue;
            return;
        }
        if (!this._fetchingMissingValues) this._fetchingMissingValues = {}; 

        // Flag to indicate we're currently getting this missing value from the server
        // so we don't kick off another fetch for the same value.
        // This will be cleared when we get the display value back (at which point the
        // display value will show up in the result of this.getValueMap())
        var needFetch = false;
        if (isc.isAn.Array(newValue)) {
            for (var i = 0, len = newValue.length; i < len; ++i) {
                var val = newValue[i];
                if (!this._fetchingMissingValues[val]) {
                    this._fetchingMissingValues[val] = needFetch = true;
                }
            }
        } else if (!this._fetchingMissingValues[newValue]) {
            this._fetchingMissingValues[newValue] = needFetch = true;
        }

        if (needFetch) {

            // Show "Loading" message and set field read-only until loaded
            this._setLoadingDisplayValue();

            // when deriving a valueMap from a DataSource, respect optionCriteria, 
            // optionFetchContext etc as we do in ListGrid fields and PickList based items
            var recordCrit = isc.addProperties({}, this.optionCriteria);
            if (!this.filterLocally) {
                var valueCriterion = {};
                valueCriterion[this.getValueFieldName()] = newValue;
                recordCrit = isc.DataSource.combineCriteria(recordCrit, valueCriterion);
            }

            var context = isc.addProperties(
                {},
                this.optionFilterContext,
                {showPrompt:false, 
                 internalClientContext:{dataValue:newValue, filterLocally:this.filterLocally},
                 componentId:this.containerWidget.getID(), 
                 componentContext:this.getFieldName() }
            );
            
            var undef;
            if (this.optionOperationId !== undef) {
                context.operationId = this.optionOperationId;
            }
            this.getOptionDataSource().fetchData(
                recordCrit, 
                {
                    target:this, 
                    methodName:"fetchMissingValueReply"
                },
                context
            );
        }
    },
    
    // Callback method fired when the server returns with the display value from 
    // our optionDataSource.
    // Fold this new value into our valueMap, and if necessary refresh to display it.
    fetchMissingValueReply : function (response, data, request) {

        // If we fetched all the values in the data-set, use array.find to find the appropriate
        // one
        var dataVal = response.internalClientContext.dataValue,
            // Look at filterLocally as it was set when the fetch was initialized as that
            // governs what the criteria were - could have been subsequently changed.
            filterLocally = response.internalClientContext.filterLocally,
            displayField = this.getDisplayFieldName(),
            valueField = this.getValueFieldName();

        if (!isc.isAn.Array(dataVal)) dataVal = [dataVal];
        
        var filteredData;
        if (!filterLocally) {
            filteredData = [];
        }

        var notFoundCount = 0;
        for (var i = 0, len = dataVal.length; i < len; ++i) {
            // Clean up the _fetchingMissingValues object
            delete this._fetchingMissingValues[dataVal[i]];

            var record = data ? data.find(valueField, dataVal[i]) : null;
            if (!record) {
                //>DEBUG

                this.logInfo("Unable to retrieve display value for data value:" + dataVal[i] +
                             " from dataSource " + this.getOptionDataSource());
                //<DEBUG

                ++notFoundCount;
            } else if (!filterLocally) {
                filteredData.push(record);
            }
        }

        var dataLength = data ? data.getLength() : 0,
            dataValLength = dataVal.getLength();
        if (!filterLocally && (dataLength > (dataValLength - notFoundCount))) {
            this.logWarn("FetchMissingValues - filterLocally is false yet optionDataSource " +
                         "fetch included records that do not match our current data value. Ignoring " +
                         "these values.", "fetchMissingValues");
            this.logDebug("Data returned:" + this.echoAll(data), "fetchMissingValues");

            data = filteredData;
        }

        // Cache the returned results in our 'displayFieldCache' array. This has 2 advantages:
        // - on 'setValue()' to a value we've already seen we can update the selected record
        //   without requiring an additional fetch
        // - We can maintain cache-synch with the dataSource by observing dataChanged 
        //   [like resultsets]. Caching the entire record rather than just the valueMap means we
        //   can handle sparse updates which refer only to primaryKeys [just deletion, probably].
        
        var needsRefresh = this._addDataToDisplayFieldCache(data) &&
                this._refreshForDisplayValueChange();
        
        // If we retrieved the entire dataSet, set a flag to avoid future fetches that 
        // would otherwise occur if 'setValue()' was called passing in a value that's 
        // not present in this valueMap
        if (filterLocally) this._gotAllOptions = true;
        
        // We need to refresh our displayed value if we're still showing the 
        // data value
         
        this.updateDisplayValueMap(needsRefresh);

        // If field was set to read-only during Loading message, make it editable now
        this._clearLoadingDisplayValue(notFoundCount);
    },

    _clearPendingMissingValue : function (value) {
        if (this._fetchingMissingValues) delete this._fetchingMissingValues[value];
    },

    _fetchMissingValueInProgress : function () {
        return (this._fetchingMissingValues && !isc.isAn.emptyObject(this._fetchingMissingValues));
    },

    _setLoadingDisplayValue : function () {
        if (this.loadingDisplayValue != null) {
            if (!this.isReadOnly()) {
                this.setCanEdit(false);
                // Keep record of changing the read-only status of the field
                // so we know to reset it when value is loaded.
                this._readOnlyFetchMissingValue = true;
            }
            this._hideInFieldHint();
            this.setElementValue(this.loadingDisplayValue);
        }
    },
    
    _clearLoadingDisplayValue : function (notFoundCount) {
        // The message clears itself because of the new value assigned, however,
        // if the field was set to read-only during Loading message, make it editable now
        if (!this._fetchMissingValueInProgress() && this._readOnlyFetchMissingValue) {
            
            if (notFoundCount && notFoundCount > 0) this._skipCheckForDisplayFieldValue = true;

            this.setCanEdit(true);
            delete this._readOnlyFetchMissingValue;
        }
    },

    _addRecordToDisplayFieldCache : function (record) {
        if (record != null) {
            return this._addDataToDisplayFieldCache([record]);
        } else {
            return false;
        }
    },

    _addDataToDisplayFieldCache : function (data) {
        if (data != null) {
            return this._modifyDataInDisplayFieldCache(data, true, true, false, true);
        } else {
            return false;
        }
    },

    _removeValueFromDisplayFieldCache : function (value) {
        var cache = this._displayFieldCache;
        if (cache) {
            var valueField = this.getValueFieldName(),
                record = cache.find(valueField, value);

            if (record != null) {
                return this._modifyDataInDisplayFieldCache([record], false, false, true, true);
            }
        }
        return false;
    },

    // Add a list of records to the displayValue cache.  The `add`, `update`, and `remove`
    // arguments are flags that determine the action taken.  If `returnNeedsRefresh` is true
    // then this method also returns whether or not the changes to the cache affect the
    // the displayField values of the current value of the form item.
    _modifyDataInDisplayFieldCache : function (data, add, update, remove, returnNeedsRefresh) {

        if (this._displayFieldCache == null) {
            this._displayFieldCache = [];
        }

        var cache = this._displayFieldCache,
            valueField = this.getValueFieldName(),
            displayField = this.getDisplayFieldName(),
            addOnly = add && !(update || remove);

        if (returnNeedsRefresh) {
            var value = this.getValue(),
                needsRefresh = false;

            if (!isc.isAn.Array(value)) value = [value];
        }

        for (var i = 0; i < data.length; i++) {
            var record = data[i],
                recordValue = record[valueField],
                j = cache.findIndex(valueField, recordValue),
                maybeNeedsRefresh = false;

            if (j == -1) {
                if (add) {
                    cache.push(record);
                    maybeNeedsRefresh = true;
                }
            } else if (update || remove) {
                var cachedRecord = cache[j],
                    changed = (record[displayField] != cachedRecord[displayField]);

                if (update && changed) {
                    cache[j] = record;
                    maybeNeedsRefresh = true;
                } else if (remove) {
                    cache.splice(j, 1);
                    maybeNeedsRefresh = true;
                }
            }

            if (returnNeedsRefresh && maybeNeedsRefresh && !needsRefresh) {
                needsRefresh = (value.indexOf(recordValue) != -1);
            }
        }
        
        // As with ResultSets, observe dataChanged on the dataSource so we can update our
        // cache automatically when records cached in our displayFieldCache are modified.
        var dataSource = this.getOptionDataSource();
        if (!this.isObserving(dataSource, "dataChanged")) {
            this.observe(dataSource, 
                "dataChanged", "observer.dataSourceDataChanged(observed,dsRequest,dsResponse)");
        }

        if (returnNeedsRefresh) return needsRefresh;
    },

    _refreshForDisplayValueChange : function () {
        return true;
    },

    updateDisplayValueMap : function (needsRefresh) {
        // update this._selectedRecord from the _displayFieldCache
        this._updateSelectedRecord();

        var data = this._displayFieldCache,
            displayField = this.getDisplayFieldName(),
            valueField = this.getValueFieldName();
        
        // Add to the special 'displayFieldValueMap'
        // This is combined with any explicitly specified valueMap by 'getValueMap()'
        
        var valueMap = this._displayFieldValueMap = {};
        
        var undef;
        for (var i = 0; i < data.length; i++) {
            var record = data[i];
            var value = record[valueField], display = record[displayField];
            
            // Note: We assume uniqueness here - if multiple records are returned with the same
            // data value, we'd expect them to have the same display value (and we can ignore
            // the later rows).
            if (valueMap[value] !== undef) {
                if (valueMap[value] != display) {
                    // Log a warning if we hit duplicate entries with non duplicate display
                    // values
                    this.logWarn("Deriving valueMap for '" + valueField + 
                                    "' from dataSource based on displayField '" + displayField + 
                                    "'. This dataSource contains more than one record with " + valueField 
                                    + " set to " + value + " with differing " + displayField + " values." 
                                    + " Derived valueMap is therefore unpredictable.",
                                "fetchMissingValues");
                }
                continue;
            }
            
            valueMap[record[valueField]] = displayField != null ? display : value;
        }
        // UpdateValueMap actually combines the displayFieldValueMap with any user-specified VM.
        this.updateValueMap(needsRefresh);
    },
    
    //> @method formItem.invalidateDisplayValueCache()
    // If this item has a specified +link{formItem.displayField}, the value displayed to the
    // user for this item may be derived from another field.
    // <P>
    // The display field can be either another field value in the same record or a field that
    // must be retrieved from a related +link{formItem.optionDataSource,optionDataSource} if
    // +link{FormItem.fetchMissingValues} is true. In this latter case, we perform a fetch against
    // the option dataSource when the item value changes in order to determine the
    // display value to show (and we make the associated record available via
    // +link{formItem.getSelectedRecord()}).
    // <P>
    // We cache this data on the form item, so if the item value changes to a new value, then reverts
    // to a previously-seen value, the display value and selected record are already available 
    // without the need for an additional fetch. The cached values will also be kept in synch with
    // the dataSource data assuming it is modified via standard add, update or delete operations.
    // <P>
    // This method explicitly invalidates this cache of optionDataSource data, and if the item value
    // is non null and fetchMissingValues is still true, re-fetches the data.
    // 
    // @group display_values
    // @visibility external
    //<
    // Internal destroying parameter allows us to clean up optionDataSources / missingValues type
    // stuff without instantiating a new fetch.
    invalidateDisplayValueCache : function (destroying) {
        // drop the generated 'displayFieldValueMap' / 'displayFieldCache'
        this._displayFieldValueMap = null;
        this._displayFieldCache = null;
        this._clearSelectedRecord();
        this._gotAllOptions = false;
        
        this.ignoreOptionDataSource();
        if (destroying) return;
        
        // If we are just showing values from the form as a whole, regenerate
        if (this.form._useDisplayFieldValue(this)) {
            this._displayFieldValueFromFormValues();
        // Otherwise call _checkForDisplayFieldValue which will re-fetch against the OptionDataSource
        // unless fetchMissingValues is false, etc.
        } else if (this._value != null && this.shouldFetchMissingValue(this._value)) {
            this._clearSelectedRecord();
            this._checkForDisplayFieldValue(this._value);
        }
        // updateValueMap should reset our display value - of course if an asynch fetch occurred
        // we'll temporarily show the data value, until the fetch completes.
        this.updateValueMap();
    },
 
    ignoreOptionDataSource : function () {
        // Drop optionDataSource observation. We'll re-set it up in fetchMissingValueReply if
        // fetchMissingValues is true
        
        var ODS = this.getOptionDataSource();
        if (ODS != null && this.isObserving(ODS, "dataChanged")) {
            this.ignore(ODS, "dataChanged");
        }
    },
    
    // dataSourceDataChanged
    // if optionDataSource and fetchMissingValues is specified we pick up DataSource records
    // and build a valueMap from them.
    // As with ResultSets, we observe dataChanged on the dataSource so we can keep these cached
    // records / valueMap synched with the dataSource.
    dataSourceDataChanged : function (dataSource,dsRequest,dsResponse) {
        var logCacheSynch = this.logIsDebugEnabled("fetchMissingValues");
        if (logCacheSynch) {
            this.logDebug("dataSourceDataChanged is firing for request:" + this.echo(dsRequest),
             "fetchMissingValues");
        }
        var cache = this._displayFieldCache;
        if (cache == null) return;

        if (dsResponse.invalidateCache) {
            if (logCacheSynch) {
                this.logDebug("Request had invalidateCache set, dropping cached display values", 
                    "fetchMissingValues");
            }
            this.invalidateDisplayValueCache();
        } else {
            
            var displayField = this.getDisplayFieldName(),
                valueField = this.getValueFieldName();

            var updateData = dataSource.getUpdatedData(dsRequest, dsResponse, true),
                isAdd = dsRequest.operationType == "add",
                isUpdate = dsRequest.operationType == "update",
                isRemove = dsRequest.operationType == "remove";
            
            if (logCacheSynch) {
                this.logDebug("Operation type:" + dsRequest.operationType + ", updateData:" + 
                        this.echoAll(updateData), "fetchMissingValues");
            }
            
            // Bail if no change was actually made or we don't understand the operation
            // in question
            
            if (updateData == null || (!isAdd && !isRemove && !isUpdate)) return;
            
            if (!isc.isAn.Array(updateData)) {
                updateData = [updateData];
            }
            var dataValueModified = false,
                valueField = this.getValueFieldName();
            
            if (isAdd) {
                cache.addList(updateData);
                if (this.multiple) {
                    var this_value = this._value;
                    if (!(this_value == null || isc.isAn.Array(this_value))) {
                        this.logInfo(
                                "dataSourceDataChanged - this is a multiple FormItem but this._value " +
                                "is not null and is not an array.");
                        this_value = [this_value];
                    }

                    if (this_value != null) {
                       var len = this_value.getLength();
                       for (var i = 0; !dataValueModified && i < len; ++i) {
                           dataValueModified = updateData.find(valueField, this_value[i]) != null;
                       }
                   }
                } else {
                    dataValueModified = updateData.find(valueField, this._value) != null;
                }
            } else {
                var keyColumns = dataSource.getPrimaryKeyFields();
	            for (var i = 0; i < updateData.length; i++) {
                    var updateRow = updateData[i],
                        keyValues = isc.applyMask(updateRow, keyColumns);
                    
                    // find the index of the old row
                    var index = dataSource.findByKeys(keyValues, cache);
                    if (index == -1) {
                        if (isRemove) continue;
                        // else - update, if we didn't have the record add it
                        
                        cache.add(updateRow);
                    } else {
                        var recordValue = cache[index][valueField];

                        if (this.multiple) {
                            var this_value = this._value;
                            if (!(this_value == null || isc.isAn.Array(this_value))) {
                                this.logWarn(
                                        "dataSourceDataChanged - this is a multiple FormItem but " +
                                        "this._value is not null and is not an array.");
                                this_value = [this_value];
                            }

                            if (this_value != null) {
                                var len = this_value.getLength();
                                for (var k = 0; !dataValueModified && k < len; ++k) {
                                    dataValueModified = recordValue == this_value[k];
                                }
                            }
                        } else if (recordValue == this._value) {
                            dataValueModified = true;
                        }
                        if (isRemove) {
                            cache.removeAt(index);
                        } else {
                            cache[index] = updateRow;
                        }
                    }
                }
            }
            
            // Now rebuild the valueMap from the new set of cache data, and the 'selectedRecord'
            // if necessary.
            this.updateDisplayValueMap(dataValueModified && this._refreshForDisplayValueChange());
        }
    },
    
    //> @method formItem.getSelectedRecord()
    // Get the record returned from the +link{optionDataSource} when +link{formItem.fetchMissingValues,fetchMissingValues}
    // is true, and the missing value is fetched.
    // <P>
    // +link{formItem.fetchMissingValues} kicks off the fetch when the form item is initialized
    // with a non null value or when setValue() is called on the item. Note that this method
    // will return null before the fetch completes, or if no record is found in the
    // optionDataSource matching the underlying value.
    // @return (ListGridRecord) selected record
    // @group display_values
    // @visibility external
    //<
    getSelectedRecord : function () {
        if (this._selectedRecordValue != null) {
            if (!this.compareValues(this._selectedRecordValue, this._value)) {
                this.logInfo("getSelectedRecord - cached record doesn't match new value - dropping",
                            "fetchMissingValues");
                this._clearSelectedRecord();
            }
        }
        return this._selectedRecord;
    },

    _updateSelectedRecord : function () {
        if (this._value == null || this._displayFieldCache == null) {
            this._clearSelectedRecord();
        } else {
            var valueField = this.getValueFieldName();
            this._selectedRecordValue = this._value;
            if (this.multiple) {
                var this_value = this._value;
                if (!(this_value == null || isc.isAn.Array(this_value))) {
                    this.logWarn(
                            "_updateSelectedRecord - this is a multiple FormItem but this._value " +
                            "is not null and is not an array");
                    this_value = [this_value];
                }

                if (this_value == null) {
                    this._selectedRecord = null;
                } else {
                    // assert isc.isAn.Array(this._value)
                    this._selectedRecord = [];
                    for (var i = 0, len = this_value.length; i < len; ++i) {
                        this._selectedRecord.push(
                                this._displayFieldCache.find(valueField, this_value[i]));
                    }
                }
            } else {
                this._selectedRecord = this._displayFieldCache.find(valueField, this._value);
            }
        }
    },
    _clearSelectedRecord : function () {
         delete this._selectedRecord;
         delete this._selectedRecordValue;
    },

    //>	@method	formItem.clearValue()
	// Clear the value for this form item.
    // <P>
    // Note that if a default value is specified, value will be set to that default value,
    // otherwise value will be cleared, (and removed from the containing form's values).
    // @visibility external
	//<
    
    clearValue : function () {
        
        this._clearingValue = true;
        this.setValue();
        delete this._clearingValue;
    },

    // Update the form item to display the value passed in.  This method basically calls
    // setElementValue().  In the case of `multiple: true` FormItems this method
    // converts the input value (assumed to be an array of strings) to a string using the
    // multipleValueSeparator to join the values.
    // Note that the <code>newValue</code> passed in is expected to be the display value
    // (which is, in the case of a multiple FormItem, an array of display values), 
    // rather than a raw value.
    
    _setElementValue : function (newValue, dataValue) {
        if (this.multiple && newValue != null && isc.isAn.Array(newValue)) {
            var newValueStr = "";
            for (var i = 0, len = newValue.length; i < len; i++) {
                if (newValueStr != "") newValueStr += this.multipleValueSeparator;
                newValueStr += newValue[i];
            }
            newValue = newValueStr;
        }
        return this.setElementValue(newValue, dataValue);
    },
    
	//>	@method	formItem.setElementValue()
	// Update the form item to display the value passed in.  If this item has a true form data
    // element (text box, checkbox, etc), this method will set the value of that element.
    // Otherwise updates the necessary HTML for the form item to display the new value.
    // Note that the <code>newValue</code> passed in is expected to be the display value, 
    // rather than the raw value (should have  already been passed through 
    // <code>this.mapValueToDisplay()</code>).
    //
	//		@group	elements
	//		@param	newValue 	(any)	value to set the element to 
	//<
    // Note this method also update any valueIcon to display the appropriate value for the
    // current form item value
    
	setElementValue : function (newValue, dataValue) {
        if (!this.isDrawn()) return;
        var undef;
        if (dataValue === undef) {  
            dataValue = this._value;
        }
        if (this._fetchMissingValueInProgress() && this.loadingDisplayValue != null) {
            newValue = this.loadingDisplayValue;
        }

        // If we hae a data element we always set element.value
        if (this.hasDataElement()) {
            
        
        
		    // get a pointer to the native form element for this item
    		var element = this.getDataElement();
            if (element != null) {
                this._updateValueIcon(dataValue);
                // If the element.value already matches the new value don't explicitly 
                // reassign
                
                
                if (element.value !== newValue) {
                    var scrollLeft = element.scrollLeft,
                        scrollTop = element.scrollTop;
                    
                    var mapToString = isc.Browser.isIE && isc.isA.TextItem(this);
                    element.value = (mapToString && newValue == null) ? isc.emptyString 
                                                                      : newValue;
                    
                    if (isc.Browser.isIE && isc.Browser.version >= 10) {
                        element.scrollLeft = scrollLeft;
                        element.scrollTop = scrollTop;
                    }
                }
                if (newValue === undef || newValue == null || isc.isAn.emptyString(newValue)) {
                    
                    this._showingInFieldHint = false;
                }
                return newValue;
            }
        }
        // otherwise if we have no data element, just redraw the content of our text box
        var textBox = this._getTextBoxElement();
        if (textBox != null) {
            if (this.showValueIconOnly) newValue = isc.emptyString;
            var valueIconHTML = this._getValueIconHTML(dataValue);        
            if (valueIconHTML != null) 
                newValue = valueIconHTML + (newValue != null ? newValue :  isc.emptyString);
            
            
            if (isc.Browser.isIE) {
                if (newValue && newValue.startsWith("<nobr>")) 
                    newValue = newValue.substring(6);
                if (newValue && newValue.endsWith("</nobr>"))
                    newValue = newValue.substring(0,newValue.length-7);
                try {
                    textBox.innerHTML = newValue;
                } catch (e) {
                    var newSpan = document.createElement("span");
                    newSpan.innerHTML = newValue;
                    textBox.innerHTML = "";
                    textBox.appendChild(newSpan);
                }
            } else {
                textBox.innerHTML = newValue;
            }
            
            if (!this.clipValue || this.height == null || this.width == null) {
                this.adjustOverflow("textBox value changed");
            }
        }

        // If we didn't get a pointer to our text box, we would expect the sub item to
        // implement an appropriate override to setElementValue()
	},

    // _updateValueIcon
    // Explicitly updates the valueIcon image src based on the data value passed in.
    _updateValueIcon : function (value) {
        if (this.suppressValueIcon || !this.isDrawn()) return;
        if (value == null) value = this.getValue();
        var src = this._getValueIcon(value),
            valueIconHandle = this._getValueIconHandle();
        if (src != null) {
            if (this.imageURLSuffix != null) src += this.imageURLSuffix;
            var imgDir = this.imageURLPrefix || this.baseURL || this.imgDir;

            // If the image is already written out, just update its src
            if (valueIconHandle != null) {
                isc.Canvas._setImageURL(valueIconHandle, src, imgDir);

            // In this case the valueIcon has never been written out.
            // Positioning of the valueIcon will vary by form item. 
            // - for data element based items, such as text items, we write the icon out before
            //   the data element
            // - for non data element based items, such as (synthetic) selects, we write the
            //   icon out inside the text box
            
            } else {
                src = isc.Canvas.getImgURL(src, imgDir);

                var inserted = false;
                if (this.hasDataElement()) {
                    var element = this.getDataElement();
                    if (element != null) {
                        isc.Element.insertAdjacentHTML(
                            element, "beforeBegin", this._getValueIconHTML(value)            
                        );
                        element.style.width = this.getTextBoxWidth(value);
                        inserted = true;
                    }
                } else {
                    var textBox = this._getTextBoxElement();
                    if (textBox != null) {
                        isc.Element.insertAdjacentHTML(
                            textBox, "afterBegin", this._getValueIconHTML(value)                     

                        );
                        inserted = true;
                    }
                }
                // sanity check - if we failed to insert the icon, redraw
                if (!inserted) this.redraw();
            }

        // If we have no current value icon, clear the handle if its present.
        
        } else if (valueIconHandle != null && !(isc.isAn.Array(value) && value.length > 1) ) {
            isc.Element.clear(valueIconHandle);
            if (this.hasDataElement()) {
                var element = this.getDataElement();
                element.style.width = this.getTextBoxWidth(value);
            }
        }
    },

    //>@method formItem.setPrompt()
    // Set the +link{formItem.prompt} for this item
    // @param prompt (string) new prompt for the item.
    // @visibility external
    //<
    setPrompt : function (prompt) {
        this.prompt = prompt;
        // no need for a redraw - we don't rely on native HTML tooltips, but react to hover events to
        // show prompts
    },
    
    //>@method formItem.setHint()
    // Set the hint text for this item
    // @param hintText (string) new hint for the item
    // @visibility external
    //<
    setHint : function (hintText) {
        this.hint = hintText;
        if (this.showHint) this.redraw();
    },
    
    //>@method formItem.setHintStyle()
    // Set the hintStyle for this item
    // @param hintStyle (CSSStyleName) new style for hint text
    // @visibility external
    //<
    setHintStyle : function (style) {
        if (!this._getShowHintInField() && this.getHint()) {
            var hintHandle = this._getHintCellElement();
            if (hintHandle) hintHandle.className = style;
        }
    },

    // Internal methods to show/hide hints within field
    // _showingInFieldHint maintains the visibility state of hint within field
    
    _showInFieldHint : function () {
        if (!this._showingInFieldHint) {
            // Set field class to our hint style
            var element = this.getDataElement();
            if (element) {
                element.className = this._getInFieldHintStyle();
                // switch password items to plain-text while hint is showing
                if (this.isA.TextItem) {
                     this._preHintElementType = element.type;
                     element.type = "text";
                 }
            } else {
                var textBox = this._getTextBoxElement();
                if (textBox != null) {
                    textBox.className = this._getInFieldHintStyle();
                }
            }
            // Show the hint in the field
            // Note that hint is HTML which may not display correctly within the field.
            // To improve the situation, unescape common HTML sequences first.
            var hint = this.getHint();
            if (hint) hint = hint.unescapeHTML();
            this.setElementValue(hint);
            this._showingInFieldHint = true;
        }
    },
    _hideInFieldHint : function (clearStyleOnly) {
        if (this._showingInFieldHint) {
            // Reset field class to the default style
            var element = this.getDataElement();
            if (element) {
                element.className = this.getTextBoxStyle();
                if (this._preHintElementType) {
                    element.type = this._preHintElementType;
                    delete this._preHintElementType;
                }
            } else {
                var textBox = this._getTextBoxElement();
                if (textBox != null) {
                    textBox.className = this.getTextBoxStyle();
                }
            }
            // Clear the hint text from the field
            if (!clearStyleOnly) this.setElementValue(isc.emptyString);
            this._showingInFieldHint = false;
        }
    },

    // Internal method to define hint style
    _getInFieldHintStyle : function() {
        var rtl = this.showRTL && this.isRTL();
        if (this.showDisabled && this.isDisabled()) {
            return this.textBoxStyle + (rtl ? "DisabledHintRTL" : "DisabledHint");
        } else {
            return this.textBoxStyle + (rtl ? "HintRTL" : "Hint");
        }
    },

    // Does field support in-field hints and are these hints enabled?
    _getShowHintInField : function() {
        return false;
    },

    // Value Management
	// --------------------------------------------------------------------------------------------

	//>	@method	formItem.getDefaultValue()
	// Return the default value for this item
	//		@group	elements
	//
	//		@return	(any)		value of this element
	//<
	getDefaultValue : function () {
		if (this.defaultDynamicValue) {
			// CALLBACK API:  available variables:  "item,form,values"
			// Convert a string callback to a function
            this.convertToMethod("defaultDynamicValue");
			var item = this,
				form = this.form,
				values = this.form.getValues()
			;
			return this.defaultDynamicValue(item,form,values);
		}
        // Return this.defaultValue - note that this will return null (technically 'undef') if no
        // default value has been set, which is appropriate - allows null values in form items.
		return this.defaultValue;
	},
	
	//>	@method	formItem.setToDefaultValue()
	// Set the value for this item to the default value stored in the item
	//		@group	elements
	//		@return	(any)		value of this element
	//<
    // Since a defaultValue means we don't support setting to null, this is really just a 
    // synonym for clearValue(), which itself calls 'setValue(null)' and lets setValue figure
    // out the defaultValue.
	setToDefaultValue : function () {
        return this.clearValue();
	},
    
    //> @method formItem.isSetToDefaultValue()
    // Is the current value displayed by the form item derived from the default value for the
    // item.
    // @return (boolean) True if this item's value is derived from the default
    //<
    isSetToDefaultValue : function () {
        return (this._setToDefault == true);
    },

    _completionAcceptKeys : {
        "Tab":true,
        "Arrow_Left":true,
        "Arrow_Right":true,
        "Arrow_Up":true,
        "Arrow_Down":true,
        "Home":true,
        "End":true,
        "Page_Up":true,
        "Page_Down":true,
        "Enter":true
    },

    //> @method formItem.updateValue()
    // Update the stored value for this form item based on the current display value.
    // 
    //  @see saveValue()
    //  @see handleChange()
    //  @see mapDisplayToValue()
    //  @visibility internal
    //<
    // Performs the following steps:
    // - takes the current value of this item's form element
    // - maps it to the appropriate value for storage using 'mapDisplayToValue()'
    // - perform validation of the form item [if validateOnChange is true]
    //      - if the resulting value from the validator differs from the value passed in,
    //        update the display and stored value to reflect this.
    // - fire any 'transformInput()' method on the value passed in. If the value is changed,
    //   store and display this new value
    // - fire the change handler for the field.
    //      - if the change handler returns false, revert to the previous value
    // - return false if the change was "rejected". IE:
    //      - a validator failed
    //      - a change handler returned false.
    
    updateValue : function () {
        // If "Loading..." message is display during missing value fetch, ignore any element changes.
        if (this._fetchMissingValueInProgress() && this.loadingDisplayValue != null) return;

        // this is effectively meaningless if we have no element (override for special cases
        // like container items)
        if (!this.hasElement() || this.getDataElement() == null) return;

        var newValue = this.getElementValue();
        return this._updateValue(newValue);
    },
    _updateValue : function (newValue) {
         
        // avoid spurious changes with auto-completion
        if (this._pendingCompletion) {
            newValue = this._handleChangeWithCompletion(newValue);
        }
            
        // unmap the value if necessary 
        newValue = this.mapDisplayToValue(newValue);
        return this.storeValue(newValue);
    },

    //> @method formItem.storeValue()
    // Store (and optionally show) a value for this form item. 
    // <p>
    // This method will fire standard +link{formItem.change()} and
    // +link{dynamicForm.itemChanged()} handlers, and store the value passed in such that
    // subsequent calls to +link{formItem.getValue()} or +link{dynamicForm.getValue()} will
    // return the new value for this item.
    // <P>
    // This method is intended to provide a way for custom formItems - most commonly 
    // +link{canvasItem,canvasItems} - to provide a new interface to the user, allowing them
    // to manipulate the item's value, for example in an embedded +link{CanvasItem.canvas},
    // or a pop-up dialog launched from an +link{FormItemIcon,icon}, etc.  Developers
    // should call this method when the user interacts with this custom 
    // interface in order to store out the changed value.
    // <P>
    // If you cannot easily detect interactions that should change the value as the 
    // user performs them, a workaround is to call
    // <code>storeValue</code> right before the form saves.
    // <P>
    // Note that this method is not designed for customizing a value which is already being
    // saved by a standard user interaction. For example you should not call this method
    // from a +link{formItem.change(),change handler}. Other APIs such as 
    // +link{formItem.transformInput()} exist for this.
    // 
    // @example canvasItem
    //
    // @param value (any) value to save for this item
    // @param [showValue] (Boolean) Should the formItem be updated to display the new value?
    // @visibility external
    //<
    
    storeValue : function (newValue, showValue) {
        // Bail if we have already saved the value (avoids firing change on arrow keypresses,
        // etc.)
        if (this.compareValues(newValue, this._value)) {
            //this.logWarn("FI._updateValue: not saving, value unchanged: " + this._value);
            return true;
        }
        
        // This method may have been tripped by the developer's change handler somehow
        // (most common example - causing formItem to blur() when changeOnBlur is true)
        // If this is the case, bail unless the value passed in differs from the value we're
        // about to save (stored as this._changeValue)
        if (this._changingValue) {
            if (this.compareValues(newValue, this._changeValue)) {
                //this.logWarn("FI._updateValue: bailing on redundant change: " + this._changeValue);
                return true;
            }
            
        }
       
        // fire the change handler, (handles validation etc)
        // Notes:
        // - handleChange may modify the value to be saved (due to validator.suggestedValue,
        //   change handler returning false, etc). 
        //   In this case:
        //   - it will also actually save the value / reset the elementValue via a call to 
        //      setValue()
        //   - It will store the resulting value from the change handler 
        //     (whether modified and saved or not) as this._changeValue. We can then save
        //     this value out iff it hasn't already been saved (!= this._value).
        // - We consider some interactions a change "failure"- such as the change handler
        //   explicitly returning false. In these cases the handleChange() method will return
        //   false. We simply return this to our calling method in case there is any special
        //   handling to be performed.
        var returnVal = this.handleChange(newValue, this._value);
        
        // bail if handleChange() returned false
        if (!returnVal) return false;
        
        // The change handler may call 'setItems' on the form (particularly likely in LG editing)
        // in which case we'll be destroyed
         
        if (this.destroyed) return;
                
        // Ensure we have the latest value (stored as this._changeValue)
        newValue = this._changeValue;
        // We may need to perform some visual updates based on the new value - do this here
        this.updateAppearance(newValue);
 
        // save the value
        //this.logWarn("FI._updateValue: old value: " + this._value + ", newValue: " + newValue + 
        //             ", will save: " + (!this.compareValues(newValue, this._value)));
        if (!this.compareValues(newValue, this._value)) {
            // if the saved value is null and newValue is the emptyDisplayValue,
            // don't store it as the new saved value
            if (!(this._value == null && newValue && newValue == this.emptyDisplayValue)) 
                this.saveValue(newValue);
        }
        
        delete this._changeValue;
        
        // fire any specified 'changed' handler for this item.
        
        this.handleChanged(this._value);    

        // If updated value should be shown, do that now
        if (showValue) this._showValue(newValue);

        return returnVal;
    },

    //> @attr formItem.implicitSave (Boolean : false : IRW)
    // When true, indicates that changes to this item will cause an automatic save on a 
    // +link{dynamicForm.implicitSaveDelay, delay}, as well as when the entire form is
    // submitted.  Unless implicitSaveOnBlur is set to false on either this 
    // +link{formItem.implicitSaveOnBlur, formItem} or it's  
    // +link{dynamicForm.implicitSaveOnBlur, form} changes will also be automatically saved on 
    // editorExit.
    // @visibility external
    //<	

    //> @attr formItem.implicitSaveOnBlur (Boolean : false : IRW)
    // If not set to false, form item values will be saved when this item's "editorExit" 
    // handler is fired as well as on a delay and when the entire form is submitted.
    // @visibility external
    //<	

    // handleChanged() - helper to fire any user-specified "changed" handler on this item.
    handleChanged : function (value) {
        // Give any Rules associated via a rulesEngine a chance to fire.
        if (this.form.rulesEngine != null) {
           this.form.rulesEngine.processChanged(this.form, this);
        }

        if (this.changed) this.changed(this.form, this, value);
        if (this.form) {
            if (!this.suppressItemChanged && this.form.itemChanged != null) 
                this.form.itemChanged(this, value);

            this.checkForImplicitSave();
        }
    },

    checkForImplicitSave : function () {
        if (this.getImplicitSave()) {
            var _this = this;
            this.form._addItemToImplicitSaveUpdateArray(this);
            this.form.awaitingImplicitSave = true;
            this.form.fireOnPause("fiImplicitSave", 
                function () {
                    if (_this.form.awaitingImplicitSave) {
                        _this.form.performImplicitSave(_this, true);
                    }
                }, this.form.implicitSaveDelay
            );
        }
    },

    // updateAppearance() - helper method fired in response to updateValue when we have
    // a new value (entered by the user)
    // Default implementation will just rewrite any valueIcon's URL.
    
    updateAppearance : function (newValue) {
        if (this.valueIcons || this.getValueIcon) {
            this._updateValueIcon(newValue);
        }
    },
    

	//>	@method	formItem.getValue()
	// Return the value tracked by this form item.
    // <P>
    // Note that for FormItems that have a +link{ValueMap} or where a
    // +link{formatValue(),formatter} has been defined, <code>getValue()</code> returns the
    // underlying value of the FormItem, not the displayed value.
    //
    // @group formValues
	// @return (any) value of this element
    // @visibility external
	//<
	getValue : function () {
	    if (this.destroyed || this.destroying) return;
	    
        // We return this._value, rather than looking at the form item's element and deriving
        // the value from there.
        // This is appropriate for a number of reasons.
        //  - this._value may be of a type not supported by the form element, which usually only 
        //    supports strings.  We need to track booleans, null (as distinct from null
        //    string), or a number (as opposed to a number in String form). 
        //    The value set by user interaction in the form may need to be processed before it
        //    can be stored as this._value.
        //  - We want to ensure that a change handler is fired before the value is updated
        //    (allowing users to cancel a change).  
        //    If getValue() were to return the value derived from the element, in some cases we 
        //    would not have received a change notification, but the value returned would be 
        //    different from the last stored value.
        //
        // We keep this._value up to date via the 'updateValue()' method, which will fire
        // change handlers and validators for the item, then store the value via 'saveValue()'
        // updateValue() is called whenever the value may have changed (depending on the form
        // item type this may be a result of native onchange, keypress or other event[s]).
        var undef;
        if (this._value !== undef) {
            return this._value;
        }
        
        // If no value has been stored for this element, return the value the form has for this 
        // element.
        
        return this.form.getSavedItemValue(this);
	},

    //> @method	formItem.getElementValue()
    // Return the value stored in the form element(s) for this item without modification
    //  @group	elements
    //
    //  @return	(any)		value of this element
    //<
    
    getElementValue : function () {
        
        
        // If showing an in-field hint, simulate an element value of undefined
        if (this._showingInFieldHint) return null;

        // get a pointer to the element for this item
        var element = this.getDataElement();
		
        // if no element was found, bail
        if (!element) return null;

        var value = "";
        
        
        if (this._propagateMultiple && element.files && element.files.length > 1) {
            for (var i=0; i < element.files.length; i++) {
                if (value != "") value += this.multipleValueSeparator;
                value += element.files[i].name;
            }
        } else {
            value = element.value;
        }

        return value;
    },
    
	
	//>	@method	formItem.resetValue()
	// Reset the value for this item to the value stored in the last save point for the form
	//		@group	elements
	//<
	resetValue : function () {
		var oldValue = this.form._oldValues[this.getFieldName()];
		this.setValue(oldValue);
	},
	
    //>	@attr formItem.shouldSaveValue (Boolean : true : IR)
    // Should this item's value be saved in the form's values and hence returned from
    // +link{dynamicForm.getValues,form.getValues()}?
    // <p>
    // <code>shouldSaveValue:false</code> is used to mark formItems which do not correspond to
    // the underlying data model and should not save a value into the form's
    // +link{dynamicForm.values,values}.  Example includes visual separators, password re-type fields,
    // or checkboxes used to show/hide other form items.
    // <p>
    // A <code>shouldSaveValue:false</code> item should be given a value either via
    // +link{formItem.defaultValue} or by calling
    // +link{dynamicForm.setValue,form.setValue(item, value)} or 
    // +link{formItem.setValue,formItem.setValue(value)}.  Providing a value via
    // +link{dynamicForm.values,form.values} or +link{dynamicForm.setValues,form.setValues()} 
    // will automatically switch the item to <code>shouldSaveValue:true</code>.
    // <P>
    // Note that <ul>
    // <li>if an item is shouldSaveValue true, but has no name, a warning is logged, and 
    //     shouldSaveValue will be set to false.
    // </li></ul>
    //
    // @group formValues
    // @visibility external
    //<
    shouldSaveValue:true,
    
    // Will this form item's value be submitted directly to the server via a native form submit?
    shouldSubmitValue : function () {
        return this.form._formWillSubmit();
    },

    //> @method formItem.setCanEdit()
    // Is this form item editable (canEdit:true) or read-only (canEdit:false)?
    // Setting the form item to non-editable causes it to render as read-only,
    // using the appearance specified via +link{formItem.readOnlyDisplay}.
    // <P>
    // The default appearance for canEdit:false items
    // (<code>readOnlyDisplay:"readOnly"</code>) differs from the disabled state in that
    // the form item is not rendered with disabled styling and
    // most form items will allow copying of the contents while read-only but do not while
    // disabled.
    //
    // @param canEdit (boolean) Can this form item be edited?
    // @group readOnly
    // @see attr:FormItem.canEdit
    // @see setDisabled()
    // @visibility external
    //<
    setCanEdit : function (canEdit) {
        var wasEditable = !this.isReadOnly();
        this.canEdit = canEdit;
        var isEditable = !this.isReadOnly();
        if (wasEditable != isEditable) {
            this.updateCanEdit();
            if (this.canEditChanged) this.canEditChanged(canEdit);
            
            this.redraw();
        }
    },

    renderAsStatic : function () {
        return this.getCanEdit() == false && this.getReadOnlyDisplay() == "static";
    },
    renderAsReadOnly : function () {
        return this.getCanEdit() == false && this.getReadOnlyDisplay() == "readOnly";
    },
    renderAsDisabled : function () {
        return this.getCanEdit() == false && this.getReadOnlyDisplay() == "disabled";
    },
    
    //> @method formItem.getCanEdit()
    // Is this form item editable or read-only?
    // <P>
    // This setting differs from the enabled/disabled state in that most form items will
    // allow copying of the contents while read-only but do not while disabled.
    // @return (boolean) true if editable
    // @group readOnly
    // @visibility external
    //<
    getCanEdit : function () {
        return !this.isReadOnly();
    },
    
    // updateCanEdit - helper method to update the form item to reflect it's read-only state
    updateCanEdit : function () {
        // Disabled state takes precedence over read-only.
        if (this.isDisabled()) return;

        var isReadOnly = this.isReadOnly();
        this.setElementReadOnly(isReadOnly);
        this._setIconsEnabled();
        // update the valueIcon if we have one
        this._updateValueIcon();
        // UpdateState is a catch-all method that updates the css classes applied to our elements
        // to reflect the 'disabled' versions
        this.updateState();
        
        if (this.canEditChanged) this.canEditChanged(this.canEdit);
    },

    //> @method formItem.setElementReadOnly()
    // Change the read-only state of the form element immediately.
    //<
    // Unless the specific form item overrides this method and can change the state directly,
    // redraw the item using the new state.
    setElementReadOnly : function (readOnly) {
        if (this.hasDataElement()) this.redraw();
    },

    // _setElementReadOnly()
    // Actually update the HTML to mark the data element as enabled / disabled
    
    _setElementReadOnly : function (readOnly) {
        if (this.hasDataElement()) {     
            var element = this.getDataElement();
            if (element) {
                if (!readOnly && !element.readOnly) {
                    // must be using disabled state
                    element.disabled = readOnly;
                } else {
                    element.readOnly = readOnly;
                    if (isc.screenReader) {
                        if (element.setAttribute) element.setAttribute("aria-readonly", "true");
                    }
                }
                element.tabIndex = this._getElementTabIndex();
            }
        } else if (this._canFocus()) {
            var element = this.getFocusElement();
            if (element) element.tabIndex = this._getElementTabIndex();
        }
    },

    // isReadOnly - helper method to determine whether a field is editable
    // the public 'getCanEdit()' method falls through to this.
    isReadOnly : function () {
        
        var widget = this.form;
        
        // Check container(s)
        var item = this;
        while (item.parentItem) {
            if (item.canEdit != null) return !item.canEdit;
            item = item.parentItem;
        }
        return !isc.DynamicForm.canEditField(item, widget);
    },

    //> @method formItem.isEditable()
    // Can this items value be edited by the user
    // @return (boolean)    true if the user can edit this item's value.
    //<
    // Most form items are editable so return true by default an override for the cases where
    // items are just there to display static values, even though technically an instance of 
    // the FormItem class would not be editable.
    
    isEditable : function () {
        return true;
    },

    //> @method formItem.getCriteriaFieldName()
    // Returns the name of the field to use in a Criteria object for this item
    //
    // @return (String)    the name of the field to use in a Criteria object for this item
    // @group criteriaEditing
    //<
    // The main current use for this method is when filtering a field that has a displayField,
    // using a ComboBoxItem. Depending on whether the user selected from the picklist or just 
    // typed some partial filter into the combo box, we will want to filter on either the 
    // underlying field or the display field.  Done this way to make it extensible.
    
    getCriteriaFieldName : function () {
        var fieldName = this.criteriaField || this.includeFrom || this.getDataPath() || this.getFieldName();
        if (this.form.dataPath != null) {
            fieldName = this.form._trimDataPath(fieldName);
            if (fieldName.endsWith("/")) fieldName = fieldName.substring(0, fieldName.length-1);
        }
        return fieldName;
    },

    //> @method formItem.getCriteriaValue()
    // Returns the value of the field to use in a Criteria object for this item
    //
    // @return (String)    the value of the field to use in a Criteria object for this item
    // @group criteriaEditing
    //<
    getCriteriaValue : function () {
        return this.getValue();
    },
    
    // Purposely undocumented parameter used to detect if getCriteria function
    //  has been overridden. When this situation occurs it could be valid to 
    //  return just about any value, including null or blank. So we always
    //  return an AdvancedCriteria and thusly hasAdvancedCriteria is always
    //  true once this flag is set.
    //
    _hasCustomCriteriaMethod:false,
    
    //> @method formItem.hasAdvancedCriteria()
    // Does this form item produce an +link{AdvancedCriteria} sub criterion object?
    // If this method returns true, +link{dynamicForm.getValuesAsCriteria()} on the
    // form containing this item will always return an +link{AdvancedCriteria} object, calling
    // <smartclient>+link{formItem.getCriterion()}</smartclient>
    // <smartgwt><code>FormItemCriterionGetter.getCriterion()</code></smartgwt>
    // on each item to retrieve the individual criteria.
    // <P>
    // Default implementation will return <code>true</code> if +link{formItem.operator} is
    // explicitly specified.
    //
    // @return (Boolean) true if this item will return an AdvancedCriteria sub-criterion.
    //
    // @group criteriaEditing
    // @visibility external
    //<
    hasAdvancedCriteria : function () {
        var value = [null, undefined].contains(this._value) ? null : this._value;
        return this._hasCustomCriteriaMethod || 
            ( value != null && (this.operator != null || this._shouldAllowExpressions()) );
    },
    
    _shouldAllowExpressions : function () {
        // by default, text, textArea, canvas, container and date (which calls through to this
        // function from it's own override) Items allow expressions
        var allow = isc.isA.TextItem(this) || isc.isA.TextAreaItem(this) || 
                isc.isA.CanvasItem(this) || isc.isA.ContainerItem(this) ||
                isc.isA.DateItem(this)
        ;

        if (!allow) return false;

        allow = this.allowExpressions;
        if (allow == null) allow = this.form.allowExpressions;
        return allow;
    },
    
    // getOperator() returns the operator for this form item
    // will return this.operator if specified, otherwise the default operator for the type
    // being edited by this item.    
    getOperator : function (textMatchStyle, isMultiValued) {
        var operator;
        if (this.operator) {
            operator = this.operator;
        } else if (isMultiValued) {
            operator = "inSet"
        } else {
            var type = this.getType();
            if (this.valueMap || this.optionDataSource || 
                isc.SimpleType.inheritsFrom(type, "enum") ||
                isc.SimpleType.inheritsFrom(type, "boolean") ||
                isc.SimpleType.inheritsFrom(type, "float") ||
                isc.SimpleType.inheritsFrom(type, "integer") ||
                isc.SimpleType.inheritsFrom(type, "time"))
            {
                operator = "equals";
            } else {
                if (textMatchStyle == null) textMatchStyle = "substring";
                // Don't pass in a value - this is appropriate for text-based items
                // we'll override for other items if necessary.
                
                var defaultOperator = this.form.defaultSearchOperator ||
                    (this.form.allowExpressions ? "iContainsPattern" : "iContains");
                operator = isc.DataSource.getCriteriaOperator(null, textMatchStyle, defaultOperator);
            }
        }

        return operator;
    },
    
    //> @method formItem.canEditCriterion() [A]
    // When a dynamic form is editing an advanced criteria
    // object via +link{DynamicForm.setValuesAsCriteria()}, this method is used to determine
    // which sub-criteria apply to which form item(s).
    // <P>
    // This method will be called on each item, and passed the sub-criterion of the
    // AdvancedCriteria object. It should return true if the item can edit the criterion,
    // otherwise false. If it returns true, setValuesAsCriteria() will call 
    // +link{formItem.setCriterion()} to actually apply the criterion to the form item, and
    // +link{dynamicForm.getValuesAsCriteria()} can subsequently retrieve the edited criterion
    // by calling +link{formItem.getCriterion()}.
    // <P>
    // Default implementation will return true if the criterion <code>fieldName</code> and
    // <code>operator</code> match the fieldName and operator (or default operator) for
    // this item.
    //
    // @param criterion (Criterion) sub-criterion from an AdvancedCriteria object
    // @return (boolean) return true if this item can edit the criterion in question.
    //
    // @group criteriaEditing
    // @visibility external
    //<
    
    canEditCriterion : function (criterion, warnOnField) {
        var thisOperator = this.getOperator(null, isc.isAn.Array(criterion.value));
        if (criterion.fieldName != null && criterion.fieldName == this.getCriteriaFieldName()
            && criterion.operator == thisOperator)
        {
            return true;
        }

        if (this._shouldAllowExpressions()) {
            var fieldNames = isc.DS.getCriteriaFields(criterion, 
                this.form.expressionDataSource || this.form.dataSource,
                true);
            return fieldNames.contains(this.getCriteriaFieldName());
        } else {
            var isAdvanced = isc.DS.isAdvancedCriteria(criterion);
            if (this.multiple && isAdvanced && criterion.operator == "or") {
                return true;
            }
        }

        return false;
    },
    
    //> @method formItem.canEditSimpleCriterion() [A]
    // Is this FormItem responsible for editing the simple criterion value for the specified 
    // fieldName? Default implementation will return true if the fieldName matches the specified
    // +link{criteriaField} if there is one, otherwise the +link{name} for this item.
    // @return (boolean) true if this item can edit the specified fieldName
    //<
    canEditSimpleCriterion : function (fieldName) {
        var cField = this.getCriteriaFieldName();
        return cField == fieldName;
    },
    
    // method called from setValuesAsCriteria to actually apply a simple criterion to this item.
    
    setSimpleCriterion : function (value, fieldName) {
        this.setValue(value);
    },
        
    //> @method formItem.getCriterion() (A)
    // Override this method if you need to provide a specialized criterion from this formItem
	// when creating an AdvancedCriteria via +link{dynamicForm.getValuesAsCriteria()}.
	// <P>
	// This API is provided to allow you to specify a more complex criterion than the 
	// "field-operator-value" criterions that are built-in.  Note that the built-in behavior is
	// generally quite flexible and powerful enough for most requirements.  An example of a case
    // where you might want to override this method is if you wanted to implement a date range 
    // selection (ie, date &gt; x AND date &lt; y) on a form that was combining its other criteria 
    // fields with an "or" operator.
    // <P>
    // Note that this method is part of the criteria editing subsystem: if overridden, it
    // is likely that you will want to also override +link{formItem.hasAdvancedCriteria()} to
    // ensure this method is called by the form, and to support editing of existing advanced
    // criteria you may also need to override +link{formItem.canEditCriterion()} and 
    // +link{formItem.setCriterion()}.
    // <P>
    // The default implementation will return a criterion including the form item value, fieldName
    // and specified +link{formItem.operator}, or a default operator derived from the
    // form item data type if no explicit operator is specified.
    //
    // @param [textMatchStyle] (TextMatchStyle) If passed assume the textMatchStyle
    //   will be used when performing a fetch operation with these criteria. This may impact
    //   the criterion's operator property.
    // @return (Criterion) criterion object based on this fields current edited value(s).
    // @group criteriaEditing
    // @visibility external
    //<
    getCriterion : function (textMatchStyle) {
        var value = this.getCriteriaValue();

        if (value == null || isc.is.emptyString(value)) return;
		// multi-selects are returned as an array.  
		if (isc.isAn.Array(value)) {
             // If nothing is selected, or if blank is selected, no criteria
            var uniqueItems = value.getUniqueItems();
            if (value.length == 0 || (uniqueItems.length == 1 && isc.isA.String(value[0]) 
                && isc.is.emptyString(value[0]))) return;
        }

        var operator = this.getOperator(textMatchStyle, isc.isAn.Array(value)),
            fieldName = this.getCriteriaFieldName();

        var result = {
            fieldName: fieldName,
            operator: operator, 
            value: value
        };

        if (this._shouldAllowExpressions()) {
            var crit = this.parseValueExpressions(value, fieldName, operator);
            if (crit != null) result = crit;
        }

        return result;
    },

    //> @method formItem.setCriterion() [A]
    // Update this form item to reflect a criterion object from within an AdvancedCriteria.
    // Called by +link{DynamicForm.setValuesAsCriteria()} when +link{formItem.canEditCriterion()}
    // returns true for this item.
    // <P>
    // Default implementation simply calls +link{formItem.setValue} with the <code>value</code>
    // of the criterion passed in
    // @param criterion (Criterion) criterion to edit
    // @group criteriaEditing
    // @visibility external
    //<
    setCriterion : function (criterion) {
        var allowEx = this._shouldAllowExpressions(),
            value = criterion ? criterion.value : null
        ;

        if (allowEx) {
            value = this.buildValueExpressions(criterion);
        } else {
            if (this.multiple) {
                var isAdvanced = isc.DS.isAdvancedCriteria(criterion);
                if (isAdvanced && criterion.operator == "or") {
                    // if the passed advancedCriteria is a flat list of ORs with the same 
                    // operator and field (this one), build an array of values from them and 
                    // pass that to setValue
                    var newValue = [],
                        critArray = criterion.criteria,
                        fieldName = this.getCriteriaFieldName(),
                        firstOp = critArray[0] ? critArray[0].operator : null,
                        failed = false
                    ;
                    for (var i=0; i<critArray.length; i++) {
                        var subCrit = critArray[i];
                        if (subCrit.criteria || subCrit.fieldName != fieldName ||
                                subCrit.operator != firstOp)
                        {
                            failed = true;
                            break;
                        }
                        newValue.add(subCrit.value);
                    }
                    if (!failed) value = newValue;
                }
            }
        }
        this.setValue(value);
    },

    _getTextBoxScrollWidth : function (textBoxHandle) {
        return textBoxHandle.scrollWidth;
    },

    //> @method formItem.valueClipped()
    // Is the value clipped?
    // <p>
    // The form item must have value clipping enabled. If a form item type supports the
    // clipValue attribute, then clipValue must be true. +link{TextItem}s and derivatives
    // (e.g. +link{SpinnerItem}) automatically clip their values.
    //
    // @return (boolean) true if the value is clipped; false otherwise.
    // @visibility external
    //<
    valueClipped : function () {
        var textBoxHandle;
        return (this.clipValue &&
                (textBoxHandle = this._getTextBoxElement()) != null &&
                isc.Element.getClientWidth(textBoxHandle) < this._getTextBoxScrollWidth(textBoxHandle));
    },

    // Errors
	// --------------------------------------------------------------------------------------------
	
	//>	@method	formItem.clearErrors()
	//			Clear all error messages for this item
	//		@group	errorHandling
	//<
	clearErrors : function (suppressAutoFocus) {
		var name = this.getFieldName();
		if (name) this.form.clearFieldErrors(name, true, suppressAutoFocus);
	},

	//>	@method	formItem.setError()
	// Set the error message for this item
	//		@group	errorHandling
	//		@param	message	(string) error message
	//<
	setError : function (message) {
		var name = this.getFieldName();
		if (name) this.form.setError(name, message);
	},

	//>	@method	formItem.hasErrors()
    //		Return whether this item currently has any validation errors as
    //		 a result of a previous validation pass.
    //		@group	errorHandling
    //	@return	(boolean)	true == item currently has validation errors.
	//<
    hasErrors : function () {
        // recurse up parent tree to find the root form item and get the correct error status
        if (this.parentItem != null) return this.parentItem.hasErrors();
        var name = this.getFieldName();
        
        if (name && this.form) return this.form.hasFieldErrors(name);
        var dp = this.getDataPath();
        if (dp && this.form) return this.form.hasFieldErrors(dp);
        return false;
    },
    	
    //> @method formItem.validate()
    // Validate this item.
    // 
    // @return (Boolean) returns true if validation was successful (no errors encountered), false
    //                   otherwise.
    // @visibility external
    //<
    validate : function () {
        var hadErrorsBefore = this.hasErrors(),
            fieldErrors = [],
            allErrors = null,
            stopOnError = false
        ;
        
        // Wrap field validation in a queue so that server validators are
        // sent as a single request.
        var wasAlreadyQueuing = isc.rpc.startQueue();

        // Process all validators on field that are applicable.
        // Note that validateFieldAndDependencies may modify the record so we pass
        // a copy of our current values.
        var value = this.getValue(),
            record = isc.addProperties({}, this.form.getValues()),
            validationOptions = {unknownErrorMessage: this.form.unknownErrorMessage,
                                typeValidationsOnly:this.form.validateTypeOnly}
        ;
        var fieldResult = this.form.validateFieldAndDependencies(this, this.validators, value,
                                                                 record, validationOptions);
        
        var storeErrorAs = this.name;
        if (storeErrorAs == null) storeErrorAs = this.getDataPath();
        if (storeErrorAs == null) {
            this.logWarn("item has no specified name or dataPath - " +
                "unable to meaningfully store validation errors.");
        }
        
        // Submit server validation requests queue
        if (!wasAlreadyQueuing) isc.rpc.sendQueue();

        if (fieldResult != null) {
            // if the validator returned a resultingValue, use that as the new value
            // whether the validator passed or failed.  This lets us transform data
            // (such as with the mask validator).
            if (fieldResult.resultingValue != null) { 
                // Update field value
                this.setValue(fieldResult.resultingValue);
            }
            if (!fieldResult.valid) {
                fieldErrors = fieldResult.errors[storeErrorAs];
                if (fieldErrors == null) fieldErrors = [];
            }
            stopOnError = fieldResult.stopOnError;

            // Even though the changed field may be valid, there may be other fields
            // that are no longer valid because of a dependency. These errors should
            // be shown on the form.
            allErrors = fieldResult.errors;
        }

        // If any errors are set or cleared, mark that a redraw is needed.
        var redrawRequired = false;

        // If we failed validation or validation is clearing previous errors,
        // update the errors on the field
        if (fieldErrors.length > 0 || hadErrorsBefore) {
            if (fieldErrors.length > 0) {
                this.form.setFieldErrors(storeErrorAs, fieldErrors, false);
            // otherwise clear old errors if there were any
            } else {
                this.form.clearFieldErrors(storeErrorAs, false);
            }
            redrawRequired = true;

            // If validation failed and we shouldn't leave field, force focus back.
            if (stopOnError) this.focusInItem();
        }

        // If other fields on the form have been validated, show/clear their error(s)
        
        if (allErrors) {
            for (var errorFieldName in allErrors) {
                if (errorFieldName != storeErrorAs) {
                    var errors = allErrors[errorFieldName];
                    if ((errors != null && !isc.isAn.emptyObject(errors)) ||
                        this.form.hasFieldErrors(errorFieldName))
                    {
                        this.form.setFieldErrors(errorFieldName, errors, false);
                        redrawRequired = true;
                    }
                }
            }
        }
        if (redrawRequired) {
            
            this.redraw();
        }

        return (fieldErrors.length == 0);
    },
    
    //> @method formItem.setRequired()
    // Setter to mark this formItem as +link{formItem.required}, or not required at runtime.
    // Note that an alternative approach to updating the <code>required</code> flag directly
    // would be to simply use a +link{ValidatorType,requiredIf} type validator.
    // <P>
    // Note that this method will not re-validate this item by default or clear any 
    // existing validation errors. If desired, this may be achieved by calling
    // +link{formItem.validate()} or +link{dynamicForm.clearErrors()}.
    // @param required (boolean) new +link{formItem.required} value.
    // @visibility external
    //<
    setRequired : function (required) {
        if (required == this.required) return;
        this.required = required;
        if (this.form == null) return;
        
        if (required) {
            // getRequiredValidator defined in dataBoundComponent
            var requiredValidator = this.form.getRequiredValidator(this);
            this.addValidator(requiredValidator);
        } else {
            this.removeValidator({type:"required"});
        }
        // redraw the item - this'll refresh the form / refreshing the item title to
        // show / hide the bold prefix / suffix...
        this.redraw();
    },
    
    
    addValidator : function (validator) {
        if (this.validators == null) this.validators = [];
        else if (!isc.isAn.Array(this.validators)) this.validators = [this.validators];
        
        if (this.validators._typeValidators) {
            this.validators = this.validators.duplicate();
        }
        this.validators.add(validator);
    },
    
    removeValidator : function (validator) {
        if (this.validators == null) return;
        if (!isc.isAn.Array(this.validators)) this.validators = [this.validators];
        if (this.validators._typeValidators) {
            this.validators = this.validators.duplicate();
        }
        
        // Handle being passed a properties block rather than a pointer to the
        // live object...
        var liveVal = this.validators.find(validator);
        this.validators.remove(liveVal);
    },

    // AutoComplete
	// -----------------------------------------------------------------------------------------

    // change fires on keyPresses that change value

    // intended key behaviors: no changes needed to accomplish these, generally
    // - accept match:
    //   - Navigate away from field: Tab/Shift-Tab
    //     - inline editing: Arrow Up/Down, Enter
    //   - Enter (w/o inline editing)
    // - remove match:
    //   - Delete/Backspace

    //> @method formItem.setAutoComplete()
    // Change the autoCompletion mode for this form field.
    //
    // @param   newSetting (AutoComplete)  new setting
    // @visibility autoComplete
    //<
    setAutoComplete : function (newSetting) {
        this.autoComplete = newSetting;
        this._handleAutoCompleteChange();
    },

    _handleAutoCompleteChange : function () {
        // get cascaded setting
        var setting = this._getAutoCompleteSetting();
        // toggle setting on native element
        if (isc.Browser.isIE && this.hasDataElement()) {
            var element = this.getDataElement();
            if (element) element.autoComplete = (setting == "native" ? "" : "off");
        }
    },

    // get cascaded autoComplete setting
    _getAutoCompleteSetting : function () {
        if (this.autoComplete != null) return this.autoComplete;
        return this.form.autoComplete;
    },

    // whether ISC auto complete is enabled
    autoCompleteEnabled : function () {
        // unsupportable at the moment
        if (isc.Browser.isSafari) return false;

        return this._getAutoCompleteSetting() == "smart";
    },

    // whether we're set to show unique matches only 
    uniqueMatchOnly : function () {
        if (this.uniqueMatch != null) return this.uniqueMatch;
        return this.form.uniqueMatch;
    },

    // get candidates for autoCompletion
    getCandidates : function () {
        var candidates = this.autoCompleteCandidates;
        
        if (candidates == null) {
            var valueMap = this.getValueMap();
            if (valueMap != null) {
                if (isc.isAn.Array(valueMap)) candidates = valueMap;
                else candidates = isc.getValues(valueMap);
            // return values from adjacent records in the dataset, if available
            } else if (this.form.grid) {
                var data = this.form.grid.data;

                // return all values that happen to be cached
                if (isc.isA.ResultSet!=null && isc.isA.ResultSet(data)) candidates = data.getValuesList(this.name);
                // return all values for the column
                else candidates = data.getProperty(this.name);
            }
        }

        // Clear out duplicates from the candidates - if we have a ListGrid with multiple
        // instances of some string in the results, we want to allow autoCompletion to that
        // string.
        // (Note this will NOT clear out strings that are identical except for case, even 
        // though they don't autoComplete differently, which is appropriate)
        if (candidates != null) candidates = candidates.getUniqueItems();
        return candidates;
    },
    
    // get the completion, if any, for this value
    getCompletion : function (base) {
        if (base == null) return;
        var candidates = this.getCandidates();
        if (candidates == null || candidates.length == 0) return;
        
        var upperBase = base.toUpperCase(),
            uniqueMatchOnly = this.uniqueMatchOnly(),
            firstMatch;        
        for (var i = 0; i < candidates.length; i++) {
            var candidate = candidates[i],
                upperCandidate = candidate != null ? candidate.toUpperCase() : null;
            // if the user has exactly typed one of our auto-complete options, don't show
            // any completions
                        
            if (upperCandidate == upperBase) return null;
            if (isc.startsWith(upperCandidate, upperBase)) {
                // return the first match
                if (!uniqueMatchOnly) return candidate;
     
                // only return a unique match
                if (firstMatch != null) return null;           
                firstMatch = candidate;
            }
        }
        return firstMatch;
    },
    
    // show an autoComplete value, if there's a valid match
    showCompletion : function (value) {

        // drop any existing completion
        this.clearCompletion();

        // check for whether autoCompletion makes sense for this type of FormItem, whether we
        // currently have an element
        if (!this.canAutoComplete || !this.hasDataElement() || 
            !this.autoCompleteEnabled()) return;

        // don't autoComplete on backspace or delete, despite change to form value
        var keyName = isc.EH.lastEvent.keyName;
        
        if (keyName == "Backspace" || keyName == "Delete") return;

        var completion = this.getCompletion(value);
        if (completion == null) {
            
            return;
        }

        // set the autocompletion value
        // NOTE: preserve upper/lowercase of typed-in value; we only convert to the
        // completion's casing when the user accepts the completion

        // Blur, and re-focus in the form item (without firing handlers).
        // This is required if the Input Method Editor (IME) is active to get out of the IME
        // mode, so that additional keypresses will overwrite the (selected) completion 
        // characters
        // (The IME is used to enter multibyte chars, such as Japanese, using a western 
        // keyboard, with multiple keystrokes returning a single character)
        
     
        this.form._blurFocusItemWithoutHandler();
        this.form._focusInItemWithoutHandler(this);


        this.setElementValue(value + completion.substring(value.length));
        this._baseValue = value;
        this._pendingCompletion = completion;

        // select the completion
        this.setSelectionRange(value.length, completion.length);
        
        
    },

    
    _handleChangeWithCompletion : function (newValue) {
        var completion = this._pendingCompletion,
            keyName = isc.EH.lastEvent.keyName;
        
        if (this._completionAcceptKeys[keyName] == true) {
            // if the completion is accepted, switch value to the exact letter case
            // of the completion value.
            // Note that with the exception of the "Enter" key, all completionAcceptKeys are
            // navigation keys that will modify the selection / text insertion point in the
            // text box.
            // If the user hit Enter, always put focus at the end of the word so the user can
            // continue typing.
            // Otherwise, respect wherever the browser natively put the cursor.
            
            this.acceptCompletion(keyName == this._$Enter);
            return completion;
        }

        var offeredText = completion.substring(this._baseValue.length);
        

        // if the field value doesn't end with the completion, the user must have typed
        // something over the completion, or deleted some characters, etc - handle as a
        // normal change.
        if (!newValue.endsWith(offeredText)) {
            this.clearCompletion();
            return newValue;
        }

        

        // if the completion is still selected, override the value in the field, which contains
        // the completion, returning instead the text as it was when the completion was offered
        if (this.getSelectedText() == offeredText) {
            
            return this._baseValue;
        }
    
        // otherwise the completion is no longer selected, use the value in the field
        
        this.clearCompletion();
        return newValue;

        // alternate approach: 
        //var charValue = isc.EH.lastEvent.characterValue;
        //if (charValue != null) return newValue;
        //this.logWarn("no change, trimmed value to: " + newValue);
        //return this._baseValue;
    },

    clearCompletion : function () {
        delete this._pendingCompletion;
        delete this._baseValue;
    },

    // accept any pending autoCompletion
    acceptCompletion : function (cursorAtEnd) {
        var completion = this._pendingCompletion;
        if (!completion) return;

        if (this.autoCompleteEnabled()) {
            

            // Cursor insertion position:
            // The user can accept completion in several ways. Depending on what interaction
            // occurred we may need to change the cursor insertion point after setting the
            // form item element value.
            // Completion tripped by:
            // - taking focus from the field.
            //      No need to worry about cursor insertion position
            // - via various 'navigation' type keypresses (arrow left, home, etc)
            //      In this case rely on native browser behavior to 'do the right thing'
            //      Observed behavior: On right arrow focus goes to the end of the completion.
            //      On Left arrow, behavior varies by browser
            //      Remember the position before changing the element value so we can re-set 
            //      to that position.
            // - enter keypress
            //      Explicitly put focus at the end of the field so the user can continue typing
            //      We achieve this by passing in a special 'focusAtEnd' parameter if the 
            //      completion was accepted via an enter keypress.
            var selectionRange = cursorAtEnd ? [completion.length, completion.length] : null;
            if (this.getElementValue() != completion) {
                if (!cursorAtEnd) selectionRange = this.getSelectionRange();
                this.setElementValue(completion);
            }
            
            if (this.hasFocus && selectionRange) 
                    this.setSelectionRange(selectionRange[0], selectionRange[1]);
        }
        this.clearCompletion();
    },

    // Text Selection
	// ----------------------------------------------------------------------------------------

    //> @method formItem.setSelectionRange()
    // Puts focus into this form item and selects characters between the given indices.
    // Only applies to drawn text based items.
    // @param start (int) selection starting character index
    // @param end (int) end of selection character index
    // @visibility internal
    //<
    // exposed on textItem / textAreaItem
    _$character:"character",
    setSelectionRange : function (start, end) {
        // applies only to text items (and subclasses)
        if (!isc.isA.TextItem(this) && !isc.isA.TextAreaItem(this)) return;
        
        // bail if undrawn
        if (!this.isDrawn()) return;

        if (!isc.isA.Number(start)) start = 0;
        if (!isc.isA.Number(end)) end = 0;
        if (start > end) {
            var newStart = end;
            end = start;
            start = newStart;
        }

        var element = this.getDataElement();
        if (element == null) return;

        if (this.logIsInfoEnabled("nativeFocus") && !this._hasNativeFocus()) {
            this.logInfo("setSelectionRange() about to change focus " + isc.EH._getActiveElementText() +
                          (this.logIsDebugEnabled("traceFocus") ? this.getStackTrace() : ""),
                         "nativeFocus");
        }

        if (isc.Browser.isIE) {
            // IE proprietary API
            
            
            isc.EH._settingTextSelection = true;
            var range = element.createTextRange();
            range.collapse(true);
            range.moveStart(this._$character, start);
            range.moveEnd(this._$character, (end-start));            
            range.select();
            delete isc.EH._settingTextSelection;
        } else {
            // DOM API, known to be supported by Moz and Safari (as-of circa 2.0 (2006?))
            element.focus();
            element.setSelectionRange(start, end);
        }
        

        if (end > start) this._lastSelectRange = [start, end];
    },
    
    //> @method formItem.selectValue()
    // Put focus in this item and select the entire value.
    // Only applies to text based items
    // @visibility internal
    //<
    selectValue : function () {
        var val = this.getElementValue(),
            end = isc.isA.String(val) ? val.length : 0;
        this.setSelectionRange(0,end);
    },
    
    //> @method formItem.deselectValue()
    // If this item currently has focus, clear the current selection. leaving focus in the item.
    // Has no effect if the item is undrawn or unfocused.
    // Only applies to text-based items.
    // @param [start] (Boolean) By default the text insertion cursor will be moved to the end of the
    //   current value - pass in this parameter to move to the start instead
    // @visibility internal
    //<
    deselectValue : function (start) {
        if (!this.hasFocus) return;
        if (start) this.setSelectionRange(0,0);
        else {
            var val = this.getElementValue(),
                end = isc.isA.String(val) ? val.length : 0;
            this.setSelectionRange(end,end);
        }
    },

    //> @method formItem.getSelectionRange()
    // For text-based items, this method returns the indices of the start/end of the current
    // selection if the item currently has the focus. In browsers other than Internet Explorer 6-9,
    // if this item does not have focus, then this method returns the indices of the start/end
    // of the selection the last time that this item had focus. In IE 6-9, returns null if the
    // item does not have focus.
    // <P>
    // In all browsers, clicking anywhere outside of the item causes the item to lose focus;
    // hence, in IE 6-9, this method will not work in other components' event handlers for
    // certain events. For example, within the +link{Canvas.click(),click()} handler of a button,
    // this item will have already lost focus, so in IE 6-9, this method will return null
    // if called within the button's click() handler. One cross-browser solution to this issue
    // is to save the selection range for later in a +link{Canvas.mouseDown(),mouseDown()} or
    // +link{Canvas.mouseOver(),mouseOver()} handler.
    // <P>
    // Notes:
    // <UL>
    //   <LI>In browsers other than IE 6-9, calling +link{formItem.setValue(),setValue()}
    // or otherwise changing the +link{getEnteredValue(),entered value} invalidates the past
    // selection range.</LI>
    //   <LI>The returned indices are indices within the entered value rather than the item's
    // value as returned by +link{FormItem.getValue(),getValue()}.
    // The distinction is particularly important for +link{TextAreaItem}s because browsers
    // normalize the line endings in the <code>&lt;textarea&gt;</code> element's value.
    // Internet Explorer 6, 7, and 8 convert line endings to "\r\n" while other browsers
    // convert line endings to "\n"
    // +externalLink{http://www.w3.org/TR/html5/forms.html#concept-textarea-api-value,as specified by the HTML5 standard}.</LI>
    // </UL>
    // @return (Array of int) 2 element array showing character index of the current or past
    // selection's start and end points within this item's +link{getEnteredValue(),entered value}.
    // In IE 6-9, returns null if the item does not have focus.
    // @visibility internal
    //<
    // Exposed on TextItem / TextAreaItem
    _$EndToEnd:"EndToEnd", _$EndToStart:"EndToStart",
    _$StartToEnd: "StartToEnd", _$StartToStart: "StartToStart",
    _$character:"character",
    getSelectionRange : function (timeCritical) {
        // applies only to text items (and subclasses)
        if (!isc.isA.TextItem(this) && !isc.isA.TextAreaItem(this)) return;
        
        if (isc.isA.UploadItem(this)) return;

        var element = this.getDataElement();
        if (element == null) return;
        if (isc.Browser.isIE && isc.Browser.version < 10) {
            if (!this._hasNativeFocus()) return null;

            var selectedRange = this._getIESelectionRange();
            if (selectedRange == null) return null;

            if (isc.isA.TextAreaItem(this)) {
                if (!this.supportsSelectionRange) return null;

                
                var testRange = selectedRange.duplicate();
                testRange.moveToElementText(element);
                var length = testRange.text.length;
                testRange.setEndPoint(this._$StartToStart, selectedRange);
                var i = length - testRange.text.length;
                return [i, i + selectedRange.text.length];
            } else {
                var rangeArray = [],
                    testRange = element.createTextRange();
                if (testRange == null) return null;
                // does the selection end at the end of the input?
                if (testRange.compareEndPoints(this._$EndToEnd, selectedRange) == 0) {

                    rangeArray[1] = testRange.text.length;
                } else {
                    testRange.setEndPoint(this._$EndToEnd, selectedRange);
                    rangeArray[1] = testRange.text.length;
                }

                testRange.setEndPoint(this._$EndToStart, selectedRange);
                rangeArray[0] = testRange.text.length;
                return rangeArray;
            }
        
        } else if (isc.Browser.isMoz || isc.Browser.isSafari || isc.Browser.isOpera || isc.Browser.isIE) {
            return [element.selectionStart, element.selectionEnd];
        }
        
    },
   
    // return true if this item has focus as reported by the browser natively
    
    _hasNativeFocus : function () {
        var focusElement = this.getFocusElement(),
            activeElement = this.getActiveElement()
        ;
        return (focusElement == activeElement);
    },

    // return the selected text within the form item
    getSelectedText : function () {
        // Only support getting selected text of a textual edit field
        if (!isc.isA.TextItem(this) && !isc.isA.TextAreaItem(this)) {
            return;
        }
        

        if (isc.Browser.isIE) {
            
            var range = this._getIESelectionRange();
            if (range) return range.text;
        } else if (isc.Browser.isMoz || isc.Browser.isSafari) {
            
            var element = this.getElement();
            if (element != null) {
                return element.value.substring(element.selectionStart, element.selectionEnd);
            }
        }
    },

    
    _IESelectionStuck : function () {
        if (!isc.Browser.isIE) return false;
        try {
            var typeDetail = document.selection ? document.selection.typeDetail : null;
        } catch (e) {
            this.logDebug("Internet explorer native 'stuck focus' state detected");
            return true;
        }
        return false;
    },

    // Helper method for determining form item selection in IE
    _getIESelectionRange : function () {
        if (!isc.Browser.isIE) return;
        
        if (isc.isA.TextAreaItem(this) && !this.supportsSelectionRange) return null;


        
        var selection = this.getDocument().selection,
            range = (selection != null ? selection.createRange() : null);

        if (range != null && range.parentElement().id == this.getDataElementId()) return range;
        return null;
    },

    // helper methods to remember the current text selection / text insertion point within
    // some form item.
    
    rememberSelection : function (timeCritical) {
        // No op if we're not drawn
        if (!this.isDrawn()) return;
        
        // applies only to text items (and subclasses)
        if (!isc.isA.TextItem(this) && !isc.isA.TextAreaItem(this)) return;

        // If the field is empty we can skip remembering the insertion point!
        var elementValue = this.getElementValue();
        if (elementValue == isc.emptyString) return;

        this._valueAtLastSelection = elementValue;
        
        var range = this.getSelectionRange(timeCritical);        
        if (range) {      
            this._lastSelectionStart = range[0];
            this._lastSelectionEnd = range[1];
        }
        
    },
    
    // Reset the selection to whatever was selected when 'rememberSelection' last ran
    resetToLastSelection : function (dataTransform) {
        // If we aren't drawn, or don't have a remembered selection we can't reset it!
        if (!this.isDrawn() || this._lastSelectionStart == null) return;
        var shouldReset,
            elementValue = this.getElementValue(),
            // valueAtLastSelection will always be a string.
            oldValue = this._valueAtLastSelection;
        if (!dataTransform) shouldReset = (elementValue == oldValue);
        else {
            shouldReset = true;
            // if everything was selected pre-change, select everything again, regardless
            // of how the value changed.
            if (this._lastSelectionStart == 0 && this._lastSelectionEnd == oldValue.length) {
                // Shift the end to ensure we select the entire new value
                this._lastSelectionEnd = elementValue.length;

            // Otherwise, if we're just shifting case retain the remembered selection.
            
            } else {
                if (elementValue.toLowerCase() != oldValue.toLowerCase()) {
                    this._lastSelectionStart = this._lastSelectionEnd = elementValue.length;
                }
            }
        }
        
        if (shouldReset) this.setSelectionRange(this._lastSelectionStart, this._lastSelectionEnd);
        delete this._lastSelectionStart;
        delete this._lastSelectionEnd;
        delete this._valueAtLastSelection;
    },
    

    // Event handling
	// ----------------------------------------------------------------------------------------
    
      
    //> @method formItem.handleChange()
    //      Internal method called whenever this item value is modified by user interaction
    //      (Called from 'updateValue()').<br>
    //      Calls call validators on this item if this.validateOnChange is true<br>
    //      Calls any 'change' handler specified for this item.<br>
    //      If validation fails or the change handler returns false, this method will reset the
    //      element to display the current item value, or validator suggested value (rejecting the 
    //      change).
    //  @return (boolean)   false if the change was rejected
    //  @see updateValue()
    //  @see change()
    //  @see validateOnChange
    //  @visibility internal
    //<
    
    handleChange : function (value, oldValue) {
        var oldErrors = this.form.getFieldErrors(this.name);
        
        if (this._changingValue && this.compareValues(value, this._changeValue)) return true;
        
        // Set the flag to indicate that we're performing a change
        this._changingValue = true;
        // By default we will not modify the value passed in.
        this._changeValue = value;
        
        // If the value changes due to a validator, etc. we have to know about it so we can
        // call this.setValue()
        var originalValue = value;
        // Handle the kinds of data that get passed around by reference
        if (isc.isA.Date(originalValue)) originalValue = originalValue.duplicate();
        else if (isc.isAn.Array(originalValue)) originalValue = originalValue.duplicate();
        else if (isc.isAn.Object(originalValue)) originalValue = isc.addProperties({}, originalValue);
    
        // If there's a transformInput method specified, allow it to update the value before
        // we proceed with validation, etc.
        if (this.transformInput) {
            value = this.transformInput(this.form, this, value, oldValue);
        }
    
        var hadErrorsBefore = this.hasErrors(),
            cancelSave = false,
            fieldErrors = [],
            allErrors = null,
            suggestedValue
        ;
        
        // If we have a specified length, and the value is a string that exceeds that length
        // trim it down now.
        if (this.length != null && isc.isA.String(value) && value.length > this.length) {
            value = value.substring(0, this.length);
        }

        // Process all validators on field that are applicable for validateOnChange
        // along with any dependent fields. Note that validateFieldAndDependencies may
        // modify the record so we pass a copy of our current values.

        // Wrap field validation in a queue so that server validators are
        // sent as a single request.
        var wasAlreadyQueuing = isc.rpc.startQueue();

        // NOTE: Do not call this.form.getValues(). If this field has both a edit parser
        // and formatter to handle a value that is an object, the returned object from
        // the parser is likely to be a new object each time even if the entered value
        // did not change. In that case, _updateValue() does not see the redundant update
        // and stop an infinite recursion via: DF.getValues() -> DF.updateFocusItemValue
        // -> FI.updateValue -> FI._updateValue -> FI.mapDisplayToValue -> FI.handleChange.
        var record = isc.addProperties({}, this.form.values),
            validationOptions = {unknownErrorMessage: this.form.unknownErrorMessage,
                                 changing: true},
            fieldResult = this.form.validateFieldAndDependencies (this, this.validators, value,
                                                                   record, validationOptions)
        ;

        // Submit server validation requests queue
        if (!wasAlreadyQueuing) isc.rpc.sendQueue();
        
        var validationFailed = false;

        if (fieldResult != null) {
            validationFailed = !fieldResult.valid;
            // if the validator returned a resultingValue, use that as the new value
            // whether the validator passed or failed.  This lets us transform data
            // (such as with the mask validator).
            if (fieldResult.resultingValue != null) { 
                // remember that value in the values list
                suggestedValue = fieldResult.resultingValue;
            }
            if (!fieldResult.valid) {
                fieldErrors = fieldResult.errors[this.name];
                if (fieldErrors == null) fieldErrors = [];
            }

            // Even though the changed field may be valid, there may be other fields
            // that are no longer valid because of a dependency. These errors should
            // be shown on the form.
            allErrors = fieldResult.errors;
        }

        
        var undef;
        if (validationFailed && suggestedValue === undef && this._rejectInvalidValueOnChange()) 
        {
            cancelSave = true;
            suggestedValue = oldValue;
            
            if (oldValue == null) suggestedValue = null;
        }
                
        // hang onto any suggested value as our working value (so it gets passed to any
        // change handler we have)
        if (suggestedValue !== undef) value = suggestedValue;

        // If we failed validation, update the errors object on the form
        
        
        var errorsDiffer;
        if (validationFailed) {
        
            if (!isc.isAn.Array(oldErrors)) {
                oldErrors =  oldErrors == null ? [] : [oldErrors];
            }
            var fieldErrorsArr = fieldErrors;
            if (!isc.isAn.Array(fieldErrorsArr)) {
                fieldErrorsArr = [fieldErrorsArr];
            }
            var errorsDiffer = fieldErrorsArr.length != oldErrors.length;
            if (!errorsDiffer) {
                for (var i = 0; i < oldErrors.length; i++) {
                    if (!fieldErrorsArr.contains(oldErrors[i])) {
                        errorsDiffer = true;
                        break;
                    }
                }
            }
            if (errorsDiffer) {
                this.clearErrors(true);
                this.setError(fieldErrors);
            }
        // otherwise clear old errors if there were any
        } else if (fieldResult != null && hadErrorsBefore) {
            errorsDiffer = true;
            this.clearErrors(true);
        }
        
        // Fire the change handler if 
        // - we passed validation 
        // - or we're supposed to fire the change handler whether an error was found or not
        this._setValueCalled = false; // setValue(), if called, sets this to true
        if ((!validationFailed || this.changeOnError)) {

            // if either change handler returns false, we'll reset to the old value     
            if (this.change != null) {
                if (this.change(this.form, this, value, oldValue) == false) {
                    value = oldValue;
                    cancelSave = true;             
                }
            }          
            // the change handler can do anything including setItems on the form.
            // therefore check whether we got destroyed before continuing with this thread
            if (this.destroyed) return;

            if (!cancelSave && this.form && this.form.itemChange != null) {
                if (this.form.itemChange(this, value, oldValue) == false) {
                    value = oldValue;
                    cancelSave = true;
                }
            }
        }

        
        var userCalledSetValue = this._setValueCalled;
        // if an error was found, or another value was suggested, set the value now
        
        var valueChanged = !this.compareValues(value, originalValue);
        if ((cancelSave || valueChanged) && !userCalledSetValue)
        {
            // If we cancelSave, avoid having setValue attempt to reset the cursor
            // - we revert to the pre change selection below.
            this.setValue(value, null, true, cancelSave);
            // Reset the selection to whatever it was BEFORE the change occurred if change /
            // validators reset to the old value
            // Note that "cancelSave" specifically indicates we're rejecting the
            // change (and resetting to the original value).
            if (cancelSave && this.maintainSelectionOnTransform) {
                this._revertToPreChangeSelection();
            }
        }
        
        if (this._setValueCalled) this._changeValue = this._value;

        // if this item wants to redraw the form when it's changed, 
        // or an error was found in validator, redraw the form
        // or the item had errors before but they are now cleared, redraw the form
        if (this.redrawOnChange || errorsDiffer) {
            this.redraw();
        }

        // If other fields on the form have been validated, show/clear their error(s)
        
        if (allErrors) {
            for (var errorFieldName in allErrors) {
                if (errorFieldName != this.name) {
                    this.form.setFieldErrors(errorFieldName, allErrors[errorFieldName], true);
                }
            }
        }
        
        // Avoid showing completion if focus is being taken from this item
        if (!cancelSave && this.hasFocus) this.showCompletion(value);
        // Clear out this._changingValue - we're done with our change handler
        // Leave this._changeValue in place, this is used by the calling method to determine
        // the result of the change handler.
        delete this._changingValue;
        
        
        return (!cancelSave);
    },
    
    // if this.validateOnChange is true, and validation fails with no suggested value, should
    // we revert to the previous value, or allow the bad value to be displayed along with the
    // validation error?
    _rejectInvalidValueOnChange : function () {
        return (this.rejectInvalidValueOnChange != null) ? this.rejectInvalidValueOnChange 
                                                           : this.form.rejectInvalidValueOnChange;
    },
    
    // compareValues - undocumented (non obfuscated) helper method: do 2 possible values for
    // this item match
    
    compareValues : function (value1, value2) {    
        // comparison implemented on the DynamicForm class directly
        var compareValues = isc.DynamicForm.compareValues;
        if (this.multiple) {
            if (!(value1 == null || isc.isAn.Array(value1))) {
                if (!this._propagateMultiple) this.logWarn(
                        "compareValues - this is a multiple FormItem but compareValues was " + 
                        "called with a non-null first argument `value1` that is not an array.");
                value1 = [value1];
            }
            if (!(value2 == null || isc.isAn.Array(value2))) {
                if (!this._propagateMultiple) this.logWarn(
                        "compareValues - this is a multiple FormItem but compareValues was " +
                        "called with a non-null second argument `value2` that is not an array.");
                value2 = [value2];
            }

            if (value1 == null && value2 == null) {
                return true;
            } else if (value1 == null || value2 == null) {
                return false;
            }

            if (value1.length != value2.length) {
                return false;
            }
            var i = 0, len = value1.length;
            while (i < len && compareValues(value1[i], value2[i])) {
                ++i;
            }
            return (i == len);
        }
        return compareValues(value1, value2);
    },
    
    
	// Handle a change event from an element.  Called directly by handlers on the native HTML
    // element
	elementChanged : function () {
        // we sometimes call this method synthetically
        var inThread = (isc.EH._thread != null);
        if (!inThread) isc.EH._setThread("ICHG");

        this.logDebug("native change");
        // updateValue() will handle firing any validators, validate-on-change change handlers,
        // and will save the value.
        if (isc.Log.supportsOnError) {
            this.updateValue();
        } else {
            try {
                this.updateValue();
            } catch (e) {
                isc.Log._reportJSError(e);
            }
        }
    
        if (!inThread) isc.EH._clearThread();

		// return true so the event terminates normally (and the user can leave the field)
        
		return true;
	},
    
    // Inactive Editor events
    // We sometimes write out "inactive" editor HTML for items - these match the live item
    // elements appearance-wise but are non editable and have limited interactivity
    // [used for printing / ListGrid.alwaysShowEditors]
    // All interactivity on these items will go through handleInactiveEditorEvent - 
    // Given an event type such as 'click' check for the presence of a handler
    // named "inactiveEditor" + event type - EG "inactiveEditorClick" - if present fire it
    // passing in the inactiveContext set up when the HTML was generated and the standard
    // itemInfo which will include where over the inactive content the event occurred
    // [giving us potential support for inactive clicks on icons, etc]
    
    // Lazily created map of standard event names to 'inactiveEditor' event names
    _inactiveEditorEvents:{
    },
    _handleInactiveEditorEvent : function (eventType, inactiveContext, itemInfo) {
        
        if (this.logIsDebugEnabled("inactiveEditorHTML")) {
            this.logDebug("handling inactive editor event:" + eventType + ", inactive context:" +   
                   this.echo(inactiveContext), "inactiveEditorHTML");
        }
        var eventName = this._inactiveEditorEvents[eventType];
        if (eventName == null) {
            eventName = this._inactiveEditorEvents[eventType] =
                "inactiveEditor" + eventType.substring(0,1).toUpperCase() + eventType.substring(1);
        }
        
        if (this[eventName] != null) {
            return this[eventName](inactiveContext, itemInfo);
        }
    },
    
    // Most of our handlers are stringMethods which take 2 params, form and item.
    // To avoid code duplication have a 'fireStandardHandler' method to handle this pattern.
    _fireStandardHandler : function (handlerName) {
        this.convertToMethod(handlerName);
        return this[handlerName](this.form, this, isc.EH.lastEvent);
    },
    
    
	//>	@method	formItem.handleTitleClick()
	// Handle a click event from this items title cell.
	//		@group	event handling
	//<
    handleTitleClick : function () {
        if (this.isDisabled()) return;
        //>EditMode
        if (this.editingOn) {
            this.editClick();
            // Also fire normal click event in EditMode, so we know to hilite the item
            this.handleClick();
            return false;
        }
        //<EditMode
        return this._fireStandardHandler("titleClick");
    },
    
    //>	@method	formItem.handleTitleDoubleClick()
	// Handle a double click event from this items title cell.
	//		@group	event handling
	//<
    handleTitleDoubleClick : function () {
        if (this.isDisabled()) return;
        return this._fireStandardHandler("titleDoubleClick");
    },

	//>	@method	formItem.handleClick()
	// Handle a click event over this form item
	//		@group	event handling
	//<
	handleClick : function () {
        //>EditMode
        if (this.editingOn) {
            isc.EditContext.selectCanvasOrFormItem(this, true);
            return false;
        }
        //<EditMode
        if (this.isDisabled()) return;    
        return this._fireStandardHandler("click");
	},

    //>	@method	formItem.handleDoubleClick()
	// Handle a double click event over this form item
	//		@group	event handling
	//<
	handleDoubleClick : function () {
        if (this.isDisabled()) return;    
        return this._fireStandardHandler("doubleClick");
	},
    
	//>	@method	formItem.handleCellClick()
	// Handle a click event from an enclosing cell
	//		@group	event handling
	//<
	handleCellClick : function () {
        if (this.isDisabled()) return;    
        return this._fireStandardHandler("cellClick");
	},
    
    //>	@method	formItem.handleCellDoubleClick()
	// Handle a double click event from an enclosing cell
	//		@group	event handling
	//<
	handleCellDoubleClick : function () {
        if (this.isDisabled()) return;    
        return this._fireStandardHandler("cellDoubleClick");
	},
    
    
    _handleElementChanged : function () {
        return this.form.elementChanged(this.getID());
    },

    
    // Handlers for mouseOver/Move/Out events (sent from the Form)
    // Fires developer specified mouseOver/move/out and titleOver/move/out handlers if present.
    // handleMouseMove also handles showing icon prompts in the Hover canvas.
    
    handleMouseMove : function () {

        if (!this.isDisabled() && (this.showValueIconOver || this.showValueIconDown)) {
            var itemInfo = isc.EH.lastEvent.itemInfo,
                overItem = (itemInfo.overElement || itemInfo.overTextBox || 
                            itemInfo.overControlTable),
                iconState = this._iconState;
                
            if (overItem) {
                // If appropriate show the 'over' version of the valueIcon
                // _mouseIsDown is a flag set when the user does a mouseDown over the item and 
                // cleared on mouseUp. If this flag is set, the user did a mouseDown on the 
                // item, moved the mouse off, and back on without releasing the mouse, so we 
                // want to show down rather than over state
                if (this._mouseIsDown && this.showValueIconDown) {
                    if (iconState != this._$Down) {
                        this._iconState = this._$Down;
                        this._updateValueIcon();
                    }
                } else if (this.showValueIconOver && iconState != this._$Over) {
                    this._iconState = this._$Over;
                    this._updateValueIcon();
                }
            } else {
                
                var expectedState = (this.showValueIconFocused && this.showValueIconOver 
                                     && this.hasFocus) ? this._$Over : null;
                if (iconState != expectedState) {
                    this._iconState = expectedState;
                    this._updateValueIcon();
                }
            }
        }
        
        if (this._fireStandardHandler("mouseMove") == false) return false;
    },    
    
    handleMouseOver : function () {
        // set up the hover to show a prompt for this cell if appropriate      
        isc.Hover.setAction(this, this._handleHover, null, this._getHoverDelay());
        
        return this._fireStandardHandler("mouseOver");
    },
    handleMouseOut : function () {
        // Clear any valueIcon 'over' state when the user moves off the item
        
        
        var expectedState = (this.showValueIconFocused && this.showValueIconOver 
                              && this.hasFocus) ? this._$Over : null;
        if (this._iconState != expectedState) {
            this._iconState = expectedState;
            this._updateValueIcon();   
        }

        // Clear the hover set up for this item
        
        this.stopHover();    
        return this._fireStandardHandler("mouseOut");
    },
    
    handleMouseDown : function () {
        var itemInfo = isc.EH.lastEvent.itemInfo,
            inactiveContext = itemInfo.inactiveContext;
        if (inactiveContext != null) {
            return this.form.bubbleInactiveEditorEvent(this, "mouseDown", itemInfo);
        }
               
        if (!this.isDisabled() && this.showValueIconDown) {
            var overItem = (itemInfo.overElement || itemInfo.overTextBox || itemInfo.overControlTable);
            if (overItem) {
                this._iconState = this._$Down;
                this._mouseIsDown = true;
                
                isc.Page.setEvent(isc.EH.MOUSE_UP, this, isc.Page.FIRE_ONCE, "_clearMouseDown");
                this._updateValueIcon();
            }
        }
        if (this.mouseDown) return this._fireStandardHandler("mouseDown");
    },

    //> @method FormItem.stopHover()    [A]
    // This method is fired when the user rolls off this item (or the title for this item) and
    // will clear any hover canvas shown by the item.
    // @group Hovers
    // @visibility external
    //<
    stopHover : function () {
        isc.Hover.clear();  
    },
    
    // _clearMouseDown fired on mouseUp to clear valueIcon mouseDown state.
    // (Fires whether the mouse is over this icon or not - this is how we track the case of
    // the user doing a mouseDown over us, moving off, then back on without releasing the mouse)
    _clearMouseDown : function () {
        this._mouseIsDown = null;
        // If the mouse is over us, we will be in state "Down" - in this case reset to Over
        if (this._iconState == this._$Down) {
            this._iconState = this.showValueIconOver ? this._$Over : null;
            this._updateValueIcon();
        }
    },
    
    handleMouseStillDown : function (event) {
        if (this.mouseStillDown) {
            return this._fireStandardHandler("mouseStillDown");
        }
    },
    
    // Helper method - how long should we delay before showing hovers?
    _getHoverDelay : function () {
        return this.hoverDelay != null ? this.hoverDelay : this.form.itemHoverDelay;
    },
    
    handleTitleMove : function () {
        return this._fireStandardHandler("titleMove");
    },
    
    handleTitleOver : function () {
        // set up the hover to show a prompt for this cell if appropriate
        isc.Hover.setAction(this, this._handleTitleHover, null, this._getHoverDelay());
    
        return this._fireStandardHandler("titleOver");
    },
    handleTitleOut : function () {
        // clear the hover event set up on this item title
        this.stopHover();
        return this._fireStandardHandler("titleOut");
    },

    handleTextBoxMove : function () {
        return this._fireStandardHandler("textBoxMove");
    },

    handleTextBoxOver : function () {
        isc.Hover.setAction(this, this._handleTextBoxHover, null, this._getHoverDelay());
        return this._fireStandardHandler("textBoxOver");
    },
    handleTextBoxOut : function () {
        this.stopHover();
        return this._fireStandardHandler("textBoxOut");
    },

    // Icon events:
    
    // On icon focus and blur, update the icon appearance (showing the over style), if showOver 
    // is true, and show the icon prompt in the window's status bar (rather than the href of 
    // the link).
    // Fire _nativeElementFocus() on the form item to handle default focus behavior
    // Note that Safari doesn't fully support focus/blur on icons - see comments in 
    // getIconHTML() for further details
    _iconFocus : function (id, element) {        
        var icon = this.getIcon(id);
        if (icon != null) {
            
            var prompt = (icon.prompt != null ? icon.prompt : this.iconPrompt)    
            window.status=prompt;
            
            if (this._iconShouldShowFocused(icon)) {
                this._setIconImgState(icon, false, true);
            }
            else if (this._iconShouldShowOver(icon)) this._setIconImgState(icon, true);
        }
        return this._nativeElementFocus(element,this);
    },
    
    _iconBlur : function (id, element) {
        var icon = this.getIcon(id);
        if (icon != null) {

            window.status="";

            // If we're showing the icon's focused state, clear it if showFocusedWithItem is false
            // If showFocusedWithItem is true, clear if this.showIconsOnFocus is false since
            // if focus goes to another element within the item, the focus styling will be
            // reapplied, and if showIconsOnFocuse is true and focus goes outside the item the
            // icon will be hidden in any case.
            var showFocused = this._iconShouldShowFocused(icon),
                showFocusedWithItem = icon.showFocusedWithItem != false,
                showOnFocus = this.showIconsOnFocus;                
            if (showFocused && (!showOnFocus || !showFocusedWithItem)) {
                this._setIconImgState(icon, false, false);
            }
            if (this._iconShouldShowOver(icon)) this._setIconImgState(icon, false, false);
        }        
        return this._nativeElementBlur(element,this);        
    },
    
    
    
    // On icon mouseOver / mouseOut, 
    // - update the icon's appearance if showOver is true.
    // - show the prompt in a hover if there is a prompt defined for the icon
    // - update window.status to also show the prompt.
    _iconMouseOver : function (id) {
        // We use the standard icon code to write out our validation error icon - if this is
        // where the event occurred, pass that through to a separate handler.
        if (id == this.errorIconName) return this._handleErrorIconMouseOver();
        var icon = this.getIcon(id);
        if (icon != null) {

            // If appropriate set the 'over' state for the icon img
            if (this._iconShouldShowOver(icon)) this._setIconImgState(icon, true);
            
            // Set up the hover action on this item:
            // Remember which icon we're over, then set hover action (will fire instantly if
            // the hover is already up)
            this._lastPromptIcon = icon;
            isc.Hover.setAction(this, this._handleIconHover, null, this._getHoverDelay());
            
            
            var prompt = (icon.prompt != null ? icon.prompt : this.iconPrompt)    
            window.status=prompt;
            return true;
        }
    },

    _iconMouseOut : function (id) {
        if (id == this.errorIconName) return this._handleErrorIconMouseOut();    
        var icon = this.getIcon(id);
        if (icon != null) {
            window.status = "";
            
            if (this._iconShouldShowOver(icon)) this._setIconImgState(icon, false);        

            // Reset the hover action to show the hover for the item as a whole
            delete this._lastPromptIcon;
            isc.Hover.setAction(this, this._handleHover, null, this._getHoverDelay());
            
            return true;
        }
    },

    
    _iconMouseMove : isc.Class.NO_OP,

    //> @method	formItem._iconClick()  (I)
    // Handle a click on a form item icon.  Fires the click action defined for the icon.
	//  @group  appearance, events
    //
    //<
    _iconClick : function (id) {
        var icon = this.getIcon(id);
        if (icon == null) return;
        if (this.iconIsDisabled(icon)) return;
        if (icon.click != null) {
            // Note - can't use 'convertToMethod' on the icon object, as it has no registry of 
            // stringMethods.  Must use 'replaceWithMethod' instead.
            if (!isc.isA.Function(icon.click)) {
                isc.Func.replaceWithMethod(icon, "click", "form,item,icon");
            }
            if (icon.click(this.form, this, icon) == false) return false;
        }

        if (icon.pickerIcon && this.pickerIconClick) this.pickerIconClick(this.form, this, icon);

        if (this.iconClick) this.iconClick(this.form, this, icon);
    },


    _$Enter:"Enter",
    _$Space:"Space",
    iconClickOnEnter:true,
    iconClickOnSpace:true,
    _iconKeyPress : function (id) {
        var icon = this.getIcon(id);
        if (icon) {
            var keyName = isc.EH.getKey(),
                character = isc.EH.getKeyEventCharacter();
            if (icon.keyPress) {
                // Note - can't use 'convertToMethod' on the icon object, as it has no registry 
                // of  stringMethods.  Must use 'replaceWithMethod' instead.
                if (!isc.isA.Function(icon.keyPress)) {
                    isc.Func.replaceWithMethod(icon, "keyPress", "keyName, character,form,item,icon");
                }
                if (icon.keyPress(keyName, character, this.form, this, icon) == false) 
                    return false;
            }
            if (this.iconKeyPress) this.iconKeyPress(keyName, character, this.form, this, icon);

            // by default we always have "enter" or "space" fire the icon's click action
            if ((this.iconClickOnEnter && keyName == this._$Enter) || 
                (this.iconClickOnSpace && keyName == this._$Space))
            {
                if (this._iconClick(icon) == false) return false;
            }
            
        }
    },
        
    // error icon events
    
    _handleErrorIconMouseOver : function () {
        isc.Hover.setAction(this, this._handleErrorIconHover, null, this._getHoverDelay());
    },
    
    _handleErrorIconMouseOut : function () {
        isc.Hover.setAction(this, this._handleHover, null, this._getHoverDelay());

    },
    
    _handleErrorIconHover : function () {
        //!DONTCOMBINE
        if (this.itemHover && this.itemHover(this, this.form) == false) return false;
        
           
        var promptString = this.shouldShowErrorIconPrompt() 
                            ? isc.FormItem.getErrorPromptString(this._currentIconError) 
                            : isc.emptyString;
                            
        if (promptString && !isc.is.emptyString(promptString)) 
            isc.Hover.show(promptString, this.form._getHoverProperties(this));
        else isc.Hover.setAction(this, this._handleHover, null, this._getHoverDelay());        
    },
    
    
    // Hover events ---------------------------------------------------------------------------
        
    //_handleHover / _handleTitleHover fired when the user hovers over this item/title.
    //
    // Fire any custom hover-handler for the item.
    // If the custom handler does not return false, show a hover canvas for this item.
    // contents for the hover derived from item.itemHoverHTML() or form.itemHoverHTML()
    // (default implementation at the form level shows item prompt)
    _handleHover : function (event) {
        // Note itemHover / titleHover registered as stringMethods
        if (this.itemHover && this.itemHover(this, this.form) == false) return false;

        var HTML;
        if (this.itemHoverHTML) HTML = this.itemHoverHTML(this, this.form);
        else HTML = this.form.itemHoverHTML(this, this.form);
        
        this.form._showItemHover(this, HTML);
    },

    _handleTitleHover : function (event) {
        //!DONTCOMBINE
        if (!this.showClippedTitleOnHover || !this.form.titleClipped(this)) return;

        if (this.titleHover && this.titleHover(this, this.form) == false) return false;

        var HTML;
        if (this.titleHoverHTML) HTML = this.titleHoverHTML(this, this.form);
        else HTML = this.form.titleHoverHTML(this, this.form);

        this.form._showItemHover(this, HTML);
    },

    _handleTextBoxHover : function (event) {
        //!DONTCOMBINE

        
        if ((this.itemHoverHTML ||
             (this.form.itemHoverHTML !== isc.DynamicForm._defaultItemHoverHTMLImpl)) &&
            !this.valueHover &&
            this.form.valueHoverHTML === isc.DynamicForm._defaultValueHoverHTMLImpl)
        {
            return this._handleHover(event);
        }

        if (!this.showClippedValueOnHover || !this.valueClipped()) return this._handleHover(event);

        if (this.valueHover && this.valueHover(this, this.form) == false) return false;

        var HTML;
        if (this.valueHoverHTML) HTML = this.valueHoverHTML(this, this.form);
        else HTML = this.form.valueHoverHTML(this, this.form);

        this.form._showItemHover(this, HTML);
    },

    // _handleIconHover: helper method fired when the user hovers over an icon.  Only fired if
    // the hovered-over icon has a prompt to show.
    // Call 'itemHover()' if defined *(allows the user to suppress the prompt), and then show
    // the icon prompt.
    _handleIconHover : function () {
        // note: we don't want to show the icon prompt if the item level 'itemHover' method 
        // returns false.
        
        //!DONTCOMBINE
        if (this.itemHover && this.itemHover(this, this.form) == false) return false;
        var icon = this._lastPromptIcon,
            prompt = this.getIconPrompt(icon);
        if (prompt && !isc.is.emptyString(prompt)) 
            isc.Hover.show(prompt, this.form._getHoverProperties(this));
        // If there's no prompt, the standard item hover to show the appropriate HTML
        // (will get shown synchronously since the hover's already up)
        else isc.Hover.setAction(this, this._handleHover, null, this._getHoverDelay());        
    },
    
    //> @method formItem.itemHover()     (A)
    // Optional stringMethod to fire when the user hovers over this item.
    // Return false to suppress default behavior of showing a hover canvas containing the
    // HTML returned by <code>formItem.itemHoverHTML()</code> / 
    // <code>form.itemHoverHTML()</code>.
    //
    // @param  item (FormItem)     Pointer to this item
    // @param  form (DynamicForm)  This items form
    // @group Hovers
    // @see FormItem.titleHover()
    // @see FormItem.itemHoverHTML()
    // @visibility external
    //<

    //> @method formItem.titleHover()     (A)
    // Optional stringMethod to fire when the user hovers over this item's title.
    // Return false to suppress default behavior of showing a hover canvas containing the
    // HTML returned by <code>formItem.titleHoverHTML()</code> / 
    // <code>form.titleHoverHTML()</code>.
    //
    // @param  item (FormItem)     Pointer to this item
    // @param  form (DynamicForm)  This items form
    // @group Hovers
    // @see FormItem.itemHover()
    // @see FormItem.titleHoverHTML()
    // @visibility external    
    //<

    //> @method formItem.valueHover()     (A)
    // Optional stringMethod to fire when the user hovers over this item's value.
    // Return false to suppress default behavior of showing a hover canvas containing the
    // HTML returned by +link{FormItem.valueHoverHTML()} / +link{DynamicForm.valueHoverHTML()}.
    //
    // @param  item (FormItem)     Pointer to this item
    // @param  form (DynamicForm)  This items form
    // @group Hovers
    // @see FormItem.itemHover()
    // @visibility external
    //<


    //> @method formItem.titleHoverHTML()     (A)
    // If defined, this method should return the HTML to display in a hover canvas when the 
    // user holds the mousepointer over this item's title.  Return null to suppress the hover 
    // canvas altogether.
    // <P>
    // If not defined, +link{DynamicForm.titleHoverHTML()} will be evaluated to determine
    // hover content instead.
    //
    // @param  item (FormItem)     Pointer to this item
    // @param  form (DynamicForm)  This items form
    // @return (HTMLString) HTML to be displayed in the hover
    // @group Hovers
    // @see FormItem.prompt
    // @see FormItem.titleHover()
    // @see FormItem.itemHoverHTML()
    // @see FormItem.showClippedTitleOnHover
    // @visibility external    
    //<

    //> @method formItem.valueHoverHTML()     (A)
    // If defined, this method should return the HTML to display in a hover canvas when the
    // user holds the mousepointer over this item's value.  Return null to suppress the hover
    // canvas altogether.
    // <p>
    // If not defined, +link{DynamicForm.valueHoverHTML()} will be evaluated to determine
    // hover content instead.
    //
    // @param  item (FormItem)     Pointer to this item
    // @param  form (DynamicForm)  This items form
    // @return (HTMLString) HTML to be displayed in the hover
    // @group Hovers
    // @see FormItem.showClippedValueOnHover
    // @visibility external
    //<

    //> @method formItem.itemHoverHTML()     (A)
    // If defined, this method should return the HTML to display in a hover canvas when the 
    // user holds the mousepointer over this item.  Return null to suppress the hover 
    // canvas altogether.
    // <P>
    // If not defined, <code>dynamicForm.itemHoverHTML()</code> will be evaluated to 
    // determine hover content instead.
    //
    // @param  item (FormItem)     Pointer to this item
    // @param  form (DynamicForm)  This items form
    // @return (HTMLString) HTML to be displayed in the hover
    // @group Hovers
    // @see FormItem.prompt
    // @see FormItem.itemHover()
    // @see FormItem.titleHoverHTML()
    // @example itemHoverHTML
    // @visibility external    
    //<

    
    
    getGlobalTabIndex : function () {
        if (this.globalTabIndex == null) {
            if (this.tabIndex == -1) this.globalTabIndex = -1;
            else {
                var formIndex = this.form.getTabIndex(),
                    localTabIndex = this.getTabIndex();                
                if (formIndex == -1) return -1;
                return (formIndex + localTabIndex);
            }
        }
        return this.globalTabIndex;
    },
    
    // getTabIndex() - returns the local tabIndex for this item.
    // Probably only to be used by internally
    getTabIndex : function () {
        if (this.tabIndex != null) return this.tabIndex;
        if (this.globalTabIndex || !this._canFocus()) return null;
        if (this._localTabIndex == null) {
            this.form._assignTabIndices();
        }
        return this._localTabIndex;
    },
    
    // setTabIndex() / setGlobalTabIndex()
    // force the form to redraw so the HTML is updated to reflect the changes in tabIndex
    setGlobalTabIndex : function (index) {
        this.globalTabIndex = index;
        this._setElementTabIndex(index);

    },
    setTabIndex : function (tabIndex) {
        this.globalTabIndex = null;
        this.tabIndex = tabIndex;
        
        this._setElementTabIndex(tabIndex);
    },

    // _getElementTabIndex() returns the tab index to actually write into the element.
    // This may differ from the result of this.getGlobalTabIndex() to allow (for example) 
    // taking form items out of the page's tab order without forgetting their global tab index.
    
    _getElementTabIndex : function (ignoreDisabled) {
        //!DONTCOMBINE
        if (this.isInactiveHTML() || 
            (!ignoreDisabled && this.isDisabled())) 
        {
            return -1;
        }
        if (this._elementTabIndex != null) return this._elementTabIndex;
        return this.getGlobalTabIndex();
    },
    
    // _setElementTabIndex() - update the tab index written into the HTML element for this 
    // form item.
    
    _setElementTabIndex : function (tabIndex) {
    
        // remember the tabIndex passed in. 
        this._elementTabIndex = tabIndex;

        // If we can't accept focus, or aren't drawn/visible just bail
        if (!this._canFocus() || !this.isDrawn()) return;
        
        // Default implementation will set the tabIndex on whatever element is returned by
        // this.getFocusElement().
        // Note that this may not work for all items - for example items with multiple elements
        // in the DOM.
        if (this.getFocusElement() != null) {
            isc.FormItem.setElementTabIndex(this.getFocusElement(), tabIndex);
            
            // Also update any form item icons.
            // Note that we are only doing this if we have an element, because if we do not
            // the redraw (below) is required in any case, and will cause the icons' tab index
            // to be updated.
            this._updateIconTabIndices();
            
        } else {
            // Make the default implementation for form items with no 'focusElement' to redraw
            // the form - this should reset the innerHTML of the element to whatever is 
            // required for an updated tabIndex in most cases.
            
            this.redraw("set tab index");
        }
    }, 

    // Our element tab index is typically derived based on a local offset from the form's tab-index
    // This notification is fired when the form's tabIndex changes, and allows us to
    // update our element tab index if appropriate
    updateTabIndex : function () {
        if (!this._canFocus() || !this.isDrawn() || this.isDisabled()) return;
        
        var gti = this.getGlobalTabIndex();
        if (this._elementTabIndex != gti) this._setElementTabIndex(gti);
    },
    
    // returns the tab index for some icon
    _getIconTabIndex : function (icon) {
        // We want the developer to be able to specify tabIndex -1 on icons
        
        if (icon.tabIndex == -1 || this.iconIsDisabled(icon)) return -1;
        // Pass in the param to avoid returning -1 if the item is disabled - this allows us to
        // leave 'neverDisable' icons in the tab-order for the page.
        return this._getElementTabIndex(true);
    },
    
    // Helper method to iterate through this item's icons, and update all their tab indices.
    _updateIconTabIndices : function () {
        var icons = [];
        icons.addList(this.icons);
        if (this._shouldShowPickerIcon()) icons.add(this.getPickerIcon());
        
        for (var i = 0; i < icons.length; i++) {     
            var icon = icons[i];
            if (!icon || icon.imgOnly) continue;
            var iconElement = this._getIconLinkElement(icon);
            
            if (iconElement != null) {
                isc.FormItem.setElementTabIndex(iconElement, this._getIconTabIndex(icon));
            }
            
        }
    },
    
    //> @method formItem.setDisabled()    (A)
    //  Set this item to be enabled or disabled at runtime. 
    //  @param  disabled (boolean)   true if this item should be disabled
    //  @see    attr:FormItem.disabled
    // @see setCanEdit()
    // @group enable
    //  @visibility external
    //<
    setDisabled : function (disabled) {
        var wasDisabled = this.isDisabled();
        this.disabled = disabled;
        var isDisabled = this.isDisabled();
        if (wasDisabled != isDisabled) this.updateDisabled();
    },
    
    //> @method formItem.setShowDisabled()
    // Setter method for +link{formItem.showDisabled}
    // @param showDisabled (boolean) new showDisabled setting
    // @visibility external
    //<
    setShowDisabled : function (showDisabled) {
        this.showDisabled = showDisabled;
        this.updateDisabled();
    },
    
    // updateDisabled - helper method to update the form item to reflect it's enabled/disabled
    // state
    
    updateDisabled : function () {
        var disabled = this.isDisabled();
        this._setElementEnabled(!disabled);
        this._setIconsEnabled();
        // update the valueIcon if we have one
        this._updateValueIcon();
        // UpdateState is a catch-all method that updates the css classes applied to our elements
        // to reflect the 'disabled' versions
        
        if (this.showDisabled) this.updateState();

    },
    
    // deprecated corollary to setEnabled()
    setEnabled : function (enabled) {
        return this.setDisabled(!enabled);
    },
    
    //> @method formItem.isDisabled()    (A)
    //  Is this item disabled?
    //  @return disabled (Boolean)   true if this item is be disabled
    //  @see    attr:FormItem.disabled
    // @group enable
    //  @visibility external
    //<
    isDisabled : function () {
        
        if (this.form == null) return this.disabled;
        
        var disabled = this.disabled || this.renderAsDisabled();
        // For members of containerItems, inherit the disabled-ness of the parent item
        if (!disabled) {
            if (this.parentItem != null) disabled = this.parentItem.isDisabled();
            else {
                disabled = this.form.isDisabled();
        
                // Allow disabled-ness to be inherited from either the form or the containerWidget
                if (!disabled && this.containerWidget != this.form) disabled = this.containerWidget.isDisabled();
            }
        }
        return disabled;
        
    },
    
    //>@method formItem.enable()
    // Set this item to be enabled at runtime.
    // @see attr:FormItem.disabled
    // @group enable
    // @visibility external
    //<
    enable : function () {
        this.setDisabled(false);
    },    
    
    //>@method formItem.disable()
    // Set this item to be disabled at runtime.
    // @see attr:FormItem.disabled
    // @group enable
    // @visibility external
    //<
    disable : function () {
        this.setDisabled(true);
    },    
    
    // _setElementEnabled()
    // Actually update the HTML to mark the data element as enabled / disabled
    // (Overridden where appropriate by subclasses)
    
    _setElementEnabled : function (enabled) {
        if (this.hasDataElement()) {     
            var element = this.getDataElement();
            if (element) {
                element.disabled = !enabled;
                element.tabIndex = this._getElementTabIndex();
                // If we use an 'eventMask' clear it out if we're being enabled, or write it
                // over the native form item element if we're being disabled.
                if (this.useDisabledEventMask()) {
                    var maskElement = this._getEventMaskElement();
                    if (maskElement && (!maskElement.getAttribute || 
                        maskElement.getAttribute("isDisabledEventMask") != "true"))
                    {
                        maskElement = null;
                    }
                            
                    if (enabled && maskElement) {
                        isc.Element.clear(maskElement);
                    } else if (!enabled && !maskElement) {
                        isc.Element.insertAdjacentHTML(element, "beforeBegin", this._getEventMaskHTML());
                    }
                }
            }
        } else if (this._canFocus()) {
            var element = this.getFocusElement();
            if (element) element.tabIndex = this._getElementTabIndex();
        }
    },
    // _setIconsEnabled()
    // Update all icons' html to match a new enabled state
    _setIconsEnabled : function () {
        if (this.showPickerIcon) {
            var pickerIcon = this.getPickerIcon();
            this.setIconEnabled(pickerIcon);
        }
        if (!this.icons || this.icons.length < 1) return;
        for (var i = 0; i< this.icons.length; i++) {
            this.setIconEnabled(this.icons[i]);
        }
    },
    
    iconIsDisabled : function (icon) {        
        icon = this.getIcon(icon);
        if (!icon) return;
        // if we're in a disabled container that trumps 'neverDisable'
        if (this.containerWidget && this.containerWidget.isDisabled()) return true;
        if (icon.neverDisable) return false;
        // disabled at the item level - trumps read-only
        if (this.isDisabled()) return true;
        // Check for flag to see if the icon shoudl be disabled if the item is readOnly
        var disableOnReadOnly = icon.disableOnReadOnly;
        if (disableOnReadOnly == null) {
            disableOnReadOnly = this.disableIconsOnReadOnly;
        }
        if (disableOnReadOnly) return this.isReadOnly();
        return false;
    },
    
    //> @method formItem._canFocus()    ()
    //  Return true if the form item can accept keyboard focus
    //  @group  visibility
    //  @return (boolean)   true if the form item is visible
    //<
    
    _canFocus : function () {
        // If there's an explicit 'canFocus' property, respect it.
        if (this.canFocus != null) return this.canFocus;
        return this.hasDataElement();
    },
    
    // Should we write out focus-attributes on the textBox [or if required, a focusProxy]
    _canFocusInTextBox : function () {
        return this._canFocus();
    },

    //> @method formItem.getCanFocus()
    // Returns true for items that can accept keyboard focus such as data items 
    // (+link{TextItem,TextItems}, +link{TextAreaItem,TextAreaItems}, etc), 
    // +link{CanvasItem,CanvasItems} with a focusable canvas, or items where +link{canFocus}
    // was explicitly set to true.
    // 
    // @return (boolean)   true if the form item is visible
    // @group focus
    // @visibility external 
    //<
    getCanFocus : function () {
        return this._canFocus();
    },
    
    
	    
    //>	@method formItem.focusInItem()
    //			Move the keyboard focus into this item's focusable element
    //		@group eventHandling, focus    
    // @visibility external
    //<
    focusInItem : function () {
        // Verify that the form is visible (Script errors occur if you attempt to focus
        // on a hidden item)
        var canFocus = this.isVisible() && this._canFocus() && !this.isDisabled(),
            element = canFocus ? this.getFocusElement() : null;     

        if (!canFocus || !element) {
            
            // This method will return null if we don't have an HTML element, or the
            // element is currently not drawn into the DOM
            return;
        }
        if (element.focus) {
        
            var suppressFocus = false;
            
            if (this._IESelectionStuck()) {
                document.selection.clear();
                
                //>DEBUG
                this.logDebug("focusInItem(): Internet Explorer selection is currently " +
                    "wedged due to a native bug tripped by synchronous focus manipulation " +
                    "and redraw. Explicitly clearing selection before resetting native focus.", 
                     "nativeFocus");
                //<DEBUG

            // In IE - avoid calling 'focus' on an element that already has native focus
            
            } else if (isc.Browser.isIE && element == this.getActiveElement()) {
                suppressFocus = true;
            }
            if (!suppressFocus) {
                //>DEBUG
                this.logInfo("about to call element.focus() " + isc.EH._getActiveElementText() +
                             (this.logIsDebugEnabled("traceFocus") ? this.getStackTrace() : ""), 
                             "nativeFocus");
                //<DEBUG
                // Fire a notification function centrally so we know a programmatic focus
                // change has been triggered
                isc.FormItem._aboutToFireNativeElementFocus(this);
                isc.EventHandler._unconfirmedFocus = this;
                element.focus();

                //if (isc.Browser.isIE) {
                //    this.logDebug("called element focus" + isc.EH._getActiveElementText(),
                //                  "nativeFocus");
                //}
            } else {
                this.logInfo("element already focused, not focus()ing", "nativeFocus");
            }
            
            
            
            if (isc.Browser.isIE) {
                isc.EH._lastFocusTarget = this;
                this._currentFocusElement = element;
            }
        } else {
            //>DEBUG
            this.logInfo("can't call element focus, no element", "nativeFocus");
            //<DEBUG
        }
        var selectOnFocus = this.selectOnFocus;
        if (selectOnFocus == null && this.form) selectOnFocus = this.form.selectOnFocus;
        // if we're just refocussing after redraw, don't selectOnFocus -- we want to retain
        // whatever selection was before focus

        if (this._refocussingAfterRedraw) selectOnFocus = false;
        if (selectOnFocus && element.select) element.select();

    },

    //>	@method formItem.blurItem()
    //			Takes focus from this form item's focusable element.
    //		@group eventHandling, focus   
    // @visibility external 
    //<
    blurItem : function () {
        if (!this.isVisible() || !(this.hasFocus || isc.EH._lastFocusTarget == this)) return;

        // Call 'blur()' on whatever element has been recorded as having native focus.
        // We record the current focus element in '_nativeElementFocus()'
         
        var element = this._getCurrentFocusElement();

        if (element && element.blur) {
            //>DEBUG
            this.logInfo("about to call element blur" + isc.EH._getActiveElementText() +
                         (this.logIsDebugEnabled("traceBlur") ? this.getStackTrace() : ""), 
                         "nativeFocus");                         
            //<DEBUG
            isc.EH._unconfirmedBlur = this;   

            
            if (isc.Browser.isIE) {
                try {
                    element.blur();
                } catch (e) {
                    
                }
            } else {
                element.blur();
            }

            //if (isc.Browser.isIE) {
            //    this.logDebug("called element blur, active element now: " +
            //              document.activeElement.id, "nativeFocus");
            //}
        } else {
            //>DEBUG
            this.logInfo("can't call element blur, no element", "nativeFocus");
            //<DEBUG
            // Note: if this item was marked as having focus, _getCurrentFocusElement()
            // ensures that flag gets cleared
        }

        
	},

    // focusInIcon()
    // - explicitly puts focus into an icon
    focusInIcon : function (icon) {
        icon = this.getIcon(icon);
        if (icon == null || icon.imgOnly) return;
        var element = this._getIconLinkElement(icon);     
        if (element != null) element.focus();
    },
    
    // blurIcon()
    // - take focus from an icon
    blurIcon : function (icon) {
        if (isc.isA.String(icon)) icon = this.getIcon(icon);    
        if (icon == null || !this.icons || !this.icons.contains(icon) || icon.imgOnly) return;
        var element = this._getIconLinkElement(icon);

        // Note - we are checking for the icon being present and drawn, but not whether it
        // actually has focus in this method - it is relying on the fact that this should only
        // be called if the passed in icon has focus.
        if (element != null) element.blur();
    },
    
    // _nativeElementFocus
    // Internal method fired when an element of this form item receives focus.
    // (Fired from focus on the data-element, or icons for most form item types)
    // Sets up formItem.hasFocus, and remembers which native element has focus, before firing
    // Form.elementFocus() to handle bubbling the focus event through the Form item hierarchy.
    
    _nativeElementFocus : function (element, itemID) {
        if (this._refocussingAfterRedraw) delete this._refocussingAfterRedraw;
        if (isc.EH._unconfirmedFocus == this) delete isc.EH._unconfirmedFocus
        isc.EH._logFocus(this, true);
        
        
        if (isc.Browser.isMoz && !this.isVisible()) {
            this.logWarn("calling element.blur() to correct focus in hidden item: " + this, 
                         "nativeFocus");
            element.blur();
            return;
        }

        // set this.hasFocus:
        this.hasFocus = true;
        
        // remember which element got focus:
        this._currentFocusElement = element;
        
        var result = this.form.elementFocus(element, itemID);

        return result;
    },
    
    _nativeElementBlur : function (element, itemID) {
        if (isc.EH._unconfirmedBlur == this) delete isc.EH._unconfirmedBlur

        // If we're pending an update, and we've lost focus, update now 
        if (this._pendingUpdate != null) {
            isc.Timer.clearTimeout(this._pendingUpdate);
            this._delayedUpdate();
        }

        isc.EH._logFocus(this);      
        this.hasFocus = false;
        delete this._currentFocusElement;
        
        var result = this.form.elementBlur(element, itemID);
        return result;
    },

	//>	@method	formItem.elementFocus()
	// Handle a focus event from an element
    // @param suppressHandlers (boolean) If passed, don't fire any developer-visible focus 
    //                                  handler functions.
	//		@group	event handling
	//<
    // Note: currently our onfocus handler will call 'elementFocus' at the form level, which
    // will then 
    // - mark "hasFocus" as true on this item
    // - call this method.
    // We may want to shift the 'hasFocus' into this file to make it clearer when we get rid of
    // the 'standalone' behavior.
    
	elementFocus : function (suppressHandlers) {  
		// if the item specifies a prompt, show it in the status bar
        
		if (this.prompt) this.form.showPrompt(this.prompt);
        
        // If 'showIconsOnFocus' is set, show the icons
        if (this.showIconsOnFocus && this.showIcons) {
            this.showAllIcons(true);
        } else {
            // if the icons are already visible update their appearance to show "Focused" image
            if (this.icons) this.updateIconsForFocus(this.icons, true);
        }
        
        // if formatOnBlur is true, update the element value to get rid 
        // of the static formatter
        if (this.formatOnBlur) {
            var displayValue = this.getDisplayValue();
        }
        
        if (this.showFocusedPickerIcon && this._shouldShowPickerIcon()) {
            var iconDef = this.getPickerIcon();
            if (iconDef) this.updateIconsForFocus(iconDef, true);
        }
        
        // Update the className of our various bits of HTML to show focused state
        if (this.showFocused) this.updateState();
        // If we're showing a valueIcon, put it into 'over' state if necessary
        
        if (this.showValueIconFocused && this.showValueIconOver && this._iconState == null) {
            this._iconState = this._$Over;
            this._updateValueIcon();
        }
        
        if (suppressHandlers) return;


        // If there are pending server validations that could affect this field,
        // block UI interaction until they complete. Skip any further handlers as well.
        
        if ((this.grid && this.grid.blockOnFieldBusy(this))
            || (!this.grid && this.form.blockOnFieldBusy(this)))
        {
            return false;
        }

        // If necessary fire this.editorEnter
        this.handleEditorEnter()
        
		// if the item has a "focus" handler
		if (this.focus) {
			// CALLBACK API:  available variables:  "form,item"
			// Convert a string callback to a function
            this.convertToMethod("focus");
			return this.focus(this.form, this);
		}
		
		return true;
	},
    
    updateIconsForFocus : function (icons, hasFocus) {
        
        if (icons == null) return;
        // force hasFocus to a boolean
        hasFocus = !!hasFocus;
        if (!isc.isAn.Array(icons)) icons = [icons];
        for (var i = 0 ; i < icons.length; i++) {
            if (this._iconShouldShowFocused(icons[i], true)) {
                var img = this._getIconImgElement(icons[i]);
                if (img != null) { 
                    isc.Canvas._setImageURL(img,
                                    this.getIconURL(icons[i], false, null, hasFocus));
                    var iconStyle = this.getIconStyle(icons[i], false, null, hasFocus);
                    if (iconStyle != null) img.className = iconStyle;
                }
            }
        }
    },
	
    //>	@method	formItem.elementBlur()
	// Handle a blur event from an element
	//		@group	event handling
	//<
	elementBlur : function () {  
		// if the item specified a prompt, clear it from the status bar since it no longer 
        // applies
		if (this.prompt) this.form.clearPrompt();
        
        // If we're showing icons on focus, we should hide them on blur.
        
        if (this.showIconsOnFocus && this.showIcons) { 

            //this.logWarn("setting icon hide timer") 
            if (this._hideAllIconsEvent == null) {
                this._hideAllIconsEvent = this.delayCall("hideAllIcons", [], 0);
            }
        // If we're hiding icons on keypress, and not showing on focus, we want to have them
        // re-show when the element gets a blur event.                
        } else if (this.hideIconsOnKeypress && this.showIcons) {
            this.showAllIcons();

        // If we just hid, or showed the icons, no need to update state for blur - otherwise
        // we may need to clear "focused" state
        } else {
            if (this.icons) this.updateIconsForFocus(this.icons, false);
        }

        // if formatOnBlur is true, update the element value to apply 
        // the static formatter
        if (this.formatOnBlur) {
            var displayValue = this.getDisplayValue();
            this._setElementValue(displayValue, this._value);
        }
        
        
         
        if (this.showFocusedPickerIcon && this._shouldShowPickerIcon()) {
            var iconDef = this.getPickerIcon();
            if (iconDef) this.updateIconsForFocus(iconDef, false);
        }        
        
        // Update the className of our various bits of HTML to show focused state 
        if (this.showFocused) {
            this.updateState();
        }
        
        // If we're showing a valueIcon, we use the "Over" state to indicate focus - 
        // clear this if appropriate
        
        if (this._iconState == this._$Over) {
            this._iconState = null
            this._updateValueIcon();
        }
				

        // if there's a pending autoCompletion, accept it now
        // No need for 'focusAtEnd()' - the element will no longer have focus 
        this.acceptCompletion();
        
        // If necessary fire this.editorExit();
        this.checkForEditorExit();

		// if the item has a "blur" handler
		if (this.blur) {
			// CALLBACK API:  available variables:  "form,item"
			// Convert a string callback to a function
            this.convertToMethod("blur");
			return this.blur(this.form, this);
		}
		
		return true;
	},
	
	// Helper method - we got an onblur but focus may have moved to a sub-element within this
	// item (EG going to an icon).
	// Only fire editorExit if focus has actually gone elsewhere on the page.
	checkForEditorExit : function (delayed, fromFocusEvent) {
        
        if (!delayed && !isc.Browser.isIE) {
            isc.FormItem._pendingEditorExitCheck = this;
            this._delayedEditorExitCheck = this.delayCall("checkForEditorExit", [true]);
            return;
        }
        
        if (fromFocusEvent && this._delayedEditorExitCheck != null) {
            isc.Timer.clearTimeout(this._delayedEditorExitCheck);
        }
        // If we got this far, any delayedEditorExitCheck is no longer applicable.
        this._delayedEditorExitCheck = null;
        if (isc.FormItem._pendingEditorExitCheck == this) {
            isc.FormItem._pendingEditorExitCheck = null;
        }
	    
        var activeElement = this.getActiveElement();
        if (activeElement != null) {
            
            var itemInfo = isc.DynamicForm._getItemInfoFromElement(activeElement);
            if (itemInfo != null) {
                var item = itemInfo.item;
                while (item) {
                    if (item == this) return;
                    // Check if focus moved to a sub-item of a container item.
                    item = item.parentItem;
                }
            }
        }
        this.handleEditorExit();
    },
    
    // _moveFocusWithinItem() - helper method to simulate a tab / shift tab while the
    // user is focused in this form item.
    // Shifts focus to the next focusable element (may be an icon, or for containerItems a
    // sub element)
    // Returns true if focus was succesfully shifted within this item
    
    _moveFocusWithinItem : function (forward) {
        var items = this.items,
            icons = this.icons;
        if (this._pickerIcon != null) {
            icons = [this._pickerIcon];
            icons.addList(this.icons);
        }
        
        // catch the common case where we have only one natively focusable element
        if ((items == null || items.length == 0) && (icons == null || icons.length == 0)) {
            return false;
        }

        var iconIndex = this.getFocusIconIndex(true),
            itemIndex;
        if (iconIndex == null) {
            var targetItem = isc.EventHandler.lastEvent.keyTarget;
            if (targetItem == this) itemIndex = 0;
            else if (items) {
                itemIndex = items.indexOf(targetItem);
            }
        }

        // If we don't have focus, no-op
        if ((itemIndex == null || itemIndex == -1) && iconIndex == null) {
            return false;
        }

        // Now determine where focus should go, based on whether this is a tab or shift
    	// tab, and where the event occurred:
        if (forward) {

        	// We're moving forwards, so start with finding the next sub-item, if there 
        	// is one
            if (itemIndex != null && items != null) {
                while (itemIndex < items.length-1) {
                    itemIndex += 1
                    var focusItem = items[itemIndex];
                        
                    if (focusItem._canFocus()) {
                        this.logInfo("FormItem.moveFocusWithinItem(" + forward +
                                 "): Moving to item:" + focusItem, "syntheticTabIndex");
                    	// Found another focusable item after the one that had focus
                        focusItem.focusInItem();

                        // return true to indicate that we shifted the focus
                        return true;
                    }
                }
            }

        	// at this point we know that focus will have to go the next focusable
        	// icon if there is one.
            if (iconIndex == null) iconIndex = -1;

            if (this.canTabToIcons != false && this.showIcons && icons != null) {
       
                while (iconIndex < icons.length -1) {
                    iconIndex += 1;
                    var icon = icons[iconIndex];
                    if (this._shouldShowIcon(icon) && !icon.imgOnly && icon.tabIndex != -1) {
                        this.logInfo("FormItem.moveFocusWithinItem(" + forward +
                                 "): Moving to icon:" + this.echo(icon), "syntheticTabIndex");
                    
                    	// Found a visible icon after the last focus element - focus
                    	// in it and return.
                        this.focusInIcon(icon);
                        return true;
                    }
                }
            }

        	// If we are here, the user has hit tab on the last focusable sub item or 
        	// icon in this form item.
            return false;

        } else {
        	// No need to check for this.icons == null or this.showIcons, as iconIndex
        	// will only be set if we are currently focused on an icon
        	// We do need to check for 'canTabToIcons', since we are explicitly putting
        	// focus onto the form item icons, rather than relying on their tabindex
            if (this.canTabToIcons != false && iconIndex != null) {
                while (iconIndex > 0) {
                    iconIndex -= 1;
                	// This icon should get focus - focus on it and return.
                    var icon = icons[iconIndex]
                    if (this._shouldShowIcon(icon) && !this.imgOnly && icon.tabIndex != -1) {
                        this.logInfo("FormItem.moveFocusWithinItem(" + forward +
                                 "): Moving to icon:" + this.echo(icon), "syntheticTabIndex");
                        this.focusInIcon(icon);
                        return true;
                    }
                }
            }
            
        	// If itemIndex is null, the event was on the first visible icon - start
        	// checking the last visible sub item
            if (itemIndex == null) 
                itemIndex = items != null ? items.length : 1;
                
        	// If we got here we have a valid itemIndex (may be 1 higher than the 
        	// number of subItems).
            while (itemIndex > 0) {
                var focusItem;
                itemIndex -= 1

                if (items == null) {
                    focusItem = this;
                } else {
                    focusItem = items[itemIndex]
                }
                
                if (focusItem._canFocus()) {
                    this.logInfo("FormItem.moveFocusWithinItem(" + forward +
                             "): Moving to item:" + focusItem, "syntheticTabIndex");
                
                	// This sub-item (or this item, if we have no sub items) should get
                	// focus - focus on it and return false to cancel the event.
                    focusItem.focusInItem();
                    return true;
                }
            }
            
            return false
        }
    },

    // Helper method to determine the index of the icon with focus (or null if no icon has 
    // focus) based on the focus'd HTML element 
    
    getFocusIconIndex : function (includePickerIcon) {
        var currentFocusElement = this._getCurrentFocusElement();
        var icons;
        if (includePickerIcon && this._pickerIcon != null) {
            icons = [this._pickerIcon];
            icons.addList(this.icons);
        } else {
            icons = this.icons;
        }
        if (currentFocusElement == null || icons == null || icons.length == 0) return null;
        for (var i = 0; i < icons.length; i++) {
            if (this._getIconLinkElement(icons[i]) == currentFocusElement) return i;
        }
        return null;
    },

    _$img:"img",
    _allowNativeTextSelection : function (event, itemInfo) {
        if (itemInfo.overTitle) return;
        // Suppress native dragging of icons
        if (itemInfo.overIcon) return false;
        // Note that imgOnly icons and valueIcons won't set the "overIcon" attribute.
        // Explicitly check for the user attempting to select or drag an IMG tag
        if (event == null) event = isc.EH.lastEvent;
        if (event.nativeTarget && (event.nativeTarget.tagName.toLowerCase() == this._$img))
            return false;
        
        return this.canSelectText != false;
    },
    
    
    handleEditorExit : function () {
        // Don't crash if this is tripped post destroy somehow.
        if (!this.form) return;
        
        if (!this._hasEditorFocus) return;
        this._hasEditorFocus = null;

        var value = this.getValue();

        // validateOnExit if necessary.
        // _suppressValidateOnEditorExit flag introduced for ComboBoxItem where we have
        // 'completeOnTab' behavior, which completes based on teh values in the pickList for
        // the item. These values may be being fetched asynchronously meaning the completion can
        // occur after focus has truly left the field. In this case it doesn't make sense to 
        // validate the temp value that's entered while the fetch is occuring. 
        // Instead validate when the fetch completes and we perform tab auto-completion
        if (!this._suppressValidateOnEditorExit) this._performValidateOnEditorExit(value);

        // If implicitSaving and value in editor changed, call the parent form to save
        if (this.getImplicitSave() && this.form && this.form.awaitingImplicitSave 
                && !this.form.implicitSaveInProgress && this._itemValueIsDirty()
                && this.getImplicitSaveOnBlur() != false) 
        {
            this.form.performImplicitSave(this, false);
        }

        if (this.editorExit) this.editorExit(this.form, this, value);
    },

    getImplicitSave : function () {
        // Just bail if we've already been removed from our form
        
        if (this.form == null) return false;
        return (this.implicitSave != null ? this.implicitSave : this.form.implicitSave);
    },

    getImplicitSaveOnBlur : function () {
        if (this.getImplicitSave() == false) return false;
        return (this.implicitSaveOnBlur != null ? 
            this.implicitSaveOnBlur : this.form.implicitSaveOnBlur);
    },

    // helper to validate() on editor exit if necessary.
    // Checks for this form not being in update mode or the value in the editor having changed since
    // editorEnter.
    // Fired from handleEditorExit
    // In ComboBoxItem we also fire this from 'fireTabCompletion()' in the case where we
    // had a delayed tab completion due to a pending fetch.
    _performValidateOnEditorExit : function (value) {
        if (this.form == null) return;
        if (this.validateOnExit || this.form.validateOnExit) {
            // _forceValidateOnExit allows items to force a re-validation. We use this in
            // dateItem where if the user picks a date from the picker we actually change values
            // then focus in the item so wouldn't catch the _editorEnterValue change.
            if (this._forceValidateOnExit ||
                (this.form.getSaveOperationType && this.form.getSaveOperationType() != "update") || 
                !this.compareValues(value, this._editorEnterValue))
            {
                this.validate();
            }
            this._forceValidateOnExit = null;
            this._editorEnterValue = null;
        }
        var rulesEngine = this.form.rulesEngine;
        if (rulesEngine != null) {
            rulesEngine.processEditorExit(this.form, this);
        }
    },
    
    
    // Documented in actionMethods
    handleEditorEnter : function () {
        if (isc.FormItem._pendingEditorExitCheck != null) {
            isc.FormItem._pendingEditorExitCheck.checkForEditorExit(true, true);
        }
        if (this._hasEditorFocus) return;
        this._hasEditorFocus = true;

        var value = this.getValue();
        // Save starting value for validateOnExit
        if (this.validateOnExit || this.form.validateOnExit) this._editorEnterValue = value;

        if (this.editorEnter) this.editorEnter(this.form, this, value);
        
        if (this.form.rulesEngine != null) {
            this.form.rulesEngine.processEditorEnter(this.form, this);
        }
    },

    
    _setupFocusCheck : function () {
        var formItem = this;
        this._nativeFocusCheckEvent = isc.Page.setEvent(
                isc.EH.MOUSE_UP, function () {
                    if (!formItem.destroyed) formItem._checkNativeFocus();
                });
        
    },
    _checkNativeFocus : function () {
        // clear out the event so we don't fire on future clicks
        isc.Page.clearEvent(isc.EH.MOUSE_UP, this._nativeFocusCheckEvent);
        delete this._nativeFocusCheckEvent;
        
        if (this.getActiveElement() == document.body) {
            //this.logWarn("Catching native focus issue");
            this.focusInItem()
        }
    },
    
    // _willHandleInput / _handleInput
    
    _willHandleInput : function () {
        // Only returns true for supported browsers, and for items where we explicitly write out
        // an oninput handler.
        return false;
    },
    _handleInput : function () {   
        isc.EH._setThread("INP");
        this.__handleInput();
        isc.EH._clearThread();
    },
    __handleInput : function () {
        // If masked edit is enabled, changeOnKeypress is handled there.
        if (!this.mask) {
            if (this.changeOnKeypress) {
                if (isc.Log.supportsOnError) {
                    this.updateValue();
                } else {
                    try {
                        this.updateValue();
                    } catch (e) {
                        isc.Log._reportJSError(e);
                    }
                }

            } else {
                var elementValue = this.getElementValue();
                this._minimalUpdateValue(elementValue);
            }
        }
    },
    // Called when changeOnKeypress is false to minimally handle an 'input' event (i.e. no
    // change event is fired, but this gives us a chance to apply some behaviors that are
    // otherwise only done in the full updateValue() path).
    _minimalUpdateValue : function (elementValue) {
        // normally we enforce 'length' on change / updateValue. If we're changeOnKeypress
        // false we still want to enforce length - do this here.
        if (this.hasDataElement() && this.length != null) {
            if (isc.isA.String(elementValue) && elementValue.length > this.length) {
                this.setElementValue(elementValue.substring(0,this.length));
            }
        }
    },

    _handleSelect : function () {   
        isc.EH._setThread("SEL");
        this.__handleSelect();
        isc.EH._clearThread();
    },
    __handleSelect : isc.Class.NO_OP,

    // Native oncut / onpaste handlers for IE
    // Fires before the value is pasted into the form item, so returning false would cancel the
    // paste.
    // Perform update on a delay so we have the new value available from the form item element.
    _nativeCutPaste : function (element, item) {

        // Fire change handlers on paste.
        
        if (this.changeOnKeypress) this._queueForUpdate();
    },

    
    

    // This is called by EventHandler.bubbleEvent - we make use of it to give the special
    // form item specific parameters to the keyPress string method, and to call 'itemKeyPress'
    // on the form.
    _$Tab:"Tab",
    handleKeyPress : function (event, eventInfo) {
        
        // if we don't have a form - we've presumably been removed from our form via a
        // 'setItems()' call since the native keyPress event
        
        if (!this.form) return;
        
        var targetInfo = this.form._getEventTargetItemInfo(event),
            keyName = event.keyName;

        // Fire iconKeyPress if approrpriate
        if (targetInfo.overIcon) {
            if (this._iconKeyPress(targetInfo.overIcon) == false) return false;
            
        // Only update value if focus was on the item itself (not on an icon)
        // Similarly only hideIconsOnKeypress if we were not focused on an icon
        } else {
    
            // If we're changing on every keypress, set this up to happen asynchronously so
            // the new value is available
            
            if (!this._willHandleInput() && this.changeOnKeypress) this._queueForUpdate();
    
            // If hideIconsOnKeypress is true, we want to hide all the icons (gives the user more
            // space to type).
            // Only do this if this is not the Tab key (in which case the user is just navigating 
            // through the field), and the user is not currently focused on an icon's link element.
            
            if (this.hideIconsOnKeypress && !this._allIconsHidden && keyName != this._$Tab) {
                this.hideAllIcons();
            }
        }
        
        // Fire this.keyPress and this.form.itemKeyPress
        return this._fireKeyPressHandlers(this,this.form,keyName,event.characterValue);
    },
    
    // _fireKeyPressHandlers - will fire item.keyPress and form.itemKeypress
    _fireKeyPressHandlers : function (item, form, keyName, characterValue) {
        if (this.keyPress != null && this.keyPress(item, form, keyName, characterValue) == false) {
            return false;
        }
        
        // it's possible for the keyPress handler to not return false, but to destroy the form
        // - for example an implementation that uses arrow_up/down to move the inline editor in
        // a ListGrid.
        if (!this.form) return false;

        // Let masked field handle keyPress
        
        if (this._maskKeyPress != null
            && this._maskKeyPress(item, form, keyName, characterValue) == false)
        {
            return false;
        }

        // itemKeyPress is a method fired on the form when an item receives a keypress.
        // Differs from "keyPress" on the form in that:
        // - the event is guaranteed to have come from a keypress in an item (not just a
        //   keypress on the form itself)
        // - it tells the form which item generated the event.
        // - itemKeyPress will not bubble up from the form to parent widgets as we're 
        //   calling it directly.
        
        // Don't fire itemKeypress directly from a sub item of a container item. The Container
        // is the logical form item as far as the form is concerned, and the developer can
        // get back to the sub item via event.keyTarget anyway, so wait for the keyPress to
        // bubble to the container and have that fire itemKeyPress at the form level.
        
        if (this.parentItem == null && this.form.itemKeyPress != null) {
            return this.form.itemKeyPress(item , keyName , characterValue);
        }
    },
    
    // If we're firing change on every keypress, we actually do this asynchronously on a timer
    // so that the value is available in the form item when change fires.
    
    _$delayedUpdate:"_delayedUpdate",
    _queueForUpdate : function () {
        if (this._pendingUpdate != null) {
            isc.Timer.clearTimeout(this._pendingUpdate);
            this._delayedUpdate();
        }
        // If we're changing on keypress, remember the current insertion point, so that if
        // the change handler fires and returns FALSE, we can reset the cursor insertion point.
        
        if (this.maintainSelectionOnTransform && 
            (this._getAutoCompleteSetting() != this._$smart)) 
        {
            this._rememberPreChangeSelection();
        }
        
        this._pendingUpdate = isc.Timer.setTimeout({target:this, methodName:this._$delayedUpdate}, 
                                                   0);
    },
    _delayedUpdate : function () {
        delete this._pendingUpdate;
        this.updateValue();
        this._clearPreChangeSelection();
    },    
    
    // Helper methods for resetting the selection if a change handler triggered by a keyPress
    // returns false.
    _rememberPreChangeSelection : function () {
        // This should never happen - but if it does, just return
        if (this._preChangeStart != null) return;
        var preChangeRange = this.getSelectionRange(true);
        if (preChangeRange) {
            this._preChangeStart = preChangeRange[0];
            this._preChangeEnd = preChangeRange[1];
        }
    },
    
    _revertToPreChangeSelection : function () {    
        // No op if this change didn't come from a keypress
        if (this._preChangeStart == null) return;
        this.setSelectionRange(this._preChangeStart, this._preChangeEnd);
    },
    _clearPreChangeSelection : function () {
        delete this._preChangeStart;
        delete this._preChangeEnd;
    },

    
    // HandleKeyDown overridden to mark the item dirty.
    // Also calls formItem.keyDown with appropriate parameters
    handleKeyDown : function (event, eventInfo) {

        // Mark the value as dirty if appropriate
        if (this.dirtyOnKeyDown) this._markValueAsDirty();
        
        var item = this, 
            form = this.form, 
            keyName = event.keyName;
        
        // fire keyDown stringMethod
        if (this.keyDown != null && this.keyDown(item, form, keyName) == false) return false
	},

	//>	@method	formItem._itemValueIsDirty()	(IA)
	//      Is this form item marked as having a dirty value?
	//
    //      @return (boolean)   true if the value is marked as being dirty via the _valueIsDirty 
    //                          flag
	//<
    // Return this value from a method to allow overriding by container items
    _itemValueIsDirty : function () {        
        return this._valueIsDirty == true;
    },
    
    _markValueAsDirty : function () {
        this._valueIsDirty = true;
    },
    
    _markValueAsNotDirty : function () {
        this._valueIsDirty = false;
    },
    
    // handleKeyUp
    // Overridden to call this.keyUp with appropriate parameters.
    handleKeyUp : function (event, eventInfo) {
        // if we don't have a form - we've presumably been removed from our form via a
        // 'setItems()' call since the native keyUp event
        
        if (!this.form) return;

        var item = this, 
            form = this.form, 
            keyName = event.keyName;
        
        // Fire keyUp stringMethod
        if (this.keyUp != null && this.keyUp(item, form, keyName) == false) return false;
	},
    
    // Serialization
	// ----------------------------------------------------------------------------------------
    
	getSerializeableFields : function(removeFields, keepFields) {
        removeFields = removeFields || [];
		
        // form is a backref
		removeFields.addList(["form"]); 
		return this.Super("getSerializeableFields", [removeFields, keepFields], arguments);
	},
 
    // Element coordinates
	// ----------------------------------------------------------------------------------------
   
	// These are needed by elements that create Canvii that float in the vicinity of the item,
    // such as the ComboBox.
    //
    

    //>@method formItem.getLeft()
    // Returns the left coordinate of this form item in pixels. Note that this method
    // is only reliable after the item has been drawn.
    // @return (integer) left coordinate within the form in pixels\
    // @group positioning,sizing
    // @visibility external
    //<
    getLeft : function () {
        var tableElement = this.isDrawn() ? this.getOuterElement() : null;
        if (tableElement == null) {
            var warning = "getLeft() Unable to determine position for " + 
                          (this.name == null ? "this item " : this.name) + ". ";
            if (this.isDrawn()) {
                warning += "This method is not supported by items of type " + this.getClass();
            } else {
                warning += "Position cannot be determined before the element is drawn"
            }
            warning += " - returning zero.";

            this.form.logWarn(warning);
            return 0;
        }
        return this._getElementLeft(tableElement);
    },

    getTitleLeft : function () {
        var titleElement = this.isDrawn() && this.form 
                                          ? isc.Element.get(this.form._getTitleCellID(this)) 
                                          : null;
        if (titleElement == null) {
            var warning = "getTitleLeft() Unable to determine position for " + 
                          (this.name == null ? "this item " : this.name) + ". ";
            if (this.isDrawn()) {
                warning += "This method is not supported by items of type " + this.getClass();
            } else {
                warning += "Position cannot be determined before the element is drawn"
            }
            warning += " - returning zero.";
            
            this.form.logWarn(warning);
            return 0;
        }
        return this._getElementLeft(titleElement);
    },

    // Separate out the method to get the position based on an HTML element.  This simplifies
    // overriding 'getLeft()' to look at something other than the result of this.getElement()
    // for items with no data element.
    _getElementLeft : function (element) {
        var currentNode = element.offsetParent,
            formElement = this.containerWidget.getHandle(),
            formParent = formElement.offsetParent,
            left = isc.Element.getOffsetLeft(element);

        // iterate up until we reach the targetElement, or the targetElement's offsetParent
        
        while (currentNode && currentNode != formElement && currentNode != formParent) {

            // Add the currentNode's offsetLeft - left w.r.t. its offsetParent
            left += isc.Element.getOffsetLeft(currentNode) 

            // Deduct the scrollLeft
            left -= (currentNode.scrollLeft || 0);
            // Add the border thickness 
            // (last offsetLeft relative to inside of border, this one relative to outside of border)
            var borderLeftWidth = parseInt(isc.Element.getComputedStyleAttribute(currentNode, "borderLeftWidth"));

            if (isc.isA.Number(borderLeftWidth)) left += borderLeftWidth;

            // getOffsetLeft() will give the distance from the outside of this element's margin
            // to the parent element -- we want the distance from the inside of the margin, so
            // add the margin thickness
            var marginLeftWidth = parseInt(isc.Element.getComputedStyleAttribute(currentNode, "marginLeft"));
            
            if (isc.isA.Number(marginLeftWidth)) left += marginLeftWidth;

            // Move up the DOM chain
            currentNode = currentNode.offsetParent;
        }

        // OffsetLeft from the last iteration was relative to the content of the offsetParent
        if (currentNode == formParent) {
            // deduct the targetElement's offsetLeft
            // No need to adjust for border / padding in this case
            left -= isc.Element.getOffsetLeft(formElement)
        }           

        return left;
    },

    _isValidIcon : function (icon) {
        return (icon != null && 
                (this.icons && this.icons.contains(icon) || 
                 this.showPickerIcon && this.getPickerIcon() == icon));
    },

    //>@method  getIconLeft()    (A)
    //  Returns the (offset) left-coordinate of an icon within its containing widget.
    //  @param  icon    (object)    icon definition
    //  @return (number)    icon left position in px
    //  @visibility external    
    //<
    getIconLeft : function (icon) {
        // default to the first icon, if possible
        if (icon == null && this.icons != null && this.icons.getLength() > 0) icon = this.icons[0];
        else  if (!this._isValidIcon(icon)) {
            this.logWarn("getIconLeft() passed invalid icon:" + isc.Log.echoAll(icon));
            return null;
        }
        var iconElement = this._getIconImgElement(icon);
        if (iconElement == null) {
            this.logWarn("getIconLeft() unable to return position of icon - " +
                         "this icon is not currently drawn into the page. Returning null");
            return null;
        }
        
        // Determine offsetLeft wrt containing widget
        return isc.Element.getLeftOffset(iconElement, this.containerWidget.getClipHandle());
    },
    
    // Methods to get the rendered position of the form item.
    // Note that these rely on the standard nested table element format - if getInnerHTML() is
    // overridden these may need to also be overridden
    //>@method formItem.getTop()
    // Returns the top coordinate of the form item in pixels. Note that this method is only 
    // reliable after the item has been drawn out.
    // @return (integer) top position in pixels
    // @group positioning,sizing
    // @visibility external
    //<
    getTop : function () {
        var element = this.isDrawn() ? this.getOuterElement() : null;
        if (element == null) {
            // We will not find an element if we are not drawn into the DOM, or if we don't 
            // have a data element.
            // In either case, bail with an appropriate warning.
            var warning = "getTop() Unable to determine position for " + 
                          (this.name == null ? "this item " : this.name) + ". ";
            if (this.isDrawn()) {
                warning += "This method is not supported by items of type " + this.getClass();
            } else {
                warning += "Position cannot be determined before the element is drawn"
            }
            warning += " - returning zero.";
            
            this.form.logWarn(warning);
            return 0;                              
        }   
        var top = this._getElementTop(element);     
        return top;
    },

    getTitleTop : function () {
        var titleElement = this.isDrawn() && this.form
                                          ? isc.Element.get(this.form._getTitleCellID(this)) 
                                          : null;
        if (titleElement == null) {
            var warning = "getTitleTop() Unable to determine position for " + 
                          (this.name == null ? "this item " : this.name) + ". ";
            if (this.isDrawn()) {
                warning += "This method is not supported by items of type " + this.getClass();
            } else {
                warning += "Position cannot be determined before the element is drawn"
            }
            warning += " - returning zero.";
            
            this.form.logWarn(warning);
            return 0;
        }
        return this._getElementTop(titleElement);
    },
    
    _getElementTop : function (element) {
        var formElement = this.containerWidget.getHandle(),
            formParent = formElement.offsetParent,
            currentNode = element.offsetParent,
            top = isc.Element.getOffsetTop(element)
        
        ;

        // iterate up until we reach the targetElement, or the targetElement's offsetParent
        // We could also check for documentBody to avoid crashing in the case where we were 
        // passed bad params.
        while (currentNode && currentNode != formElement && currentNode != formParent) {
        
            // Add the currentNode's offsetTop - top w.r.t. its offsetParent
            top += isc.Element.getOffsetTop(currentNode) 
            // Deduct the scroll top
            top -= (currentNode.scrollTop || 0);
            // Add the border thickness 
            // (last offsetTop relative to inside of border, this one relative to outside of border)
            
            var borderTopWidth = (isc.Browser.isMoz ? 0 :
                                  (isc.Browser.isIE ? 
                                    parseInt(currentNode.currentStyle.borderTopWidth) :
                                    parseInt(isc.Element.getComputedStyleAttribute(currentNode, 
                                                                                "borderTopWidth"))
                                  )
                                 );
                                                                                                            
            if (isc.isA.Number(borderTopWidth)) top += borderTopWidth;

            // getOffsetTop() will give the distance from the outside of this element's margin
            // to the parent element -- we want the distance from the inside of the margin, so
            // add the margin thickness
            
            var marginTopWidth = (isc.Browser.isIE ? 
                                    parseInt(currentNode.currentStyle.marginTop) :
                                    parseInt(isc.Element.getComputedStyleAttribute(currentNode, "marginTop")));
            if (isc.isA.Number(marginTopWidth)) top += marginTopWidth;

            // Move up the DOM chain
            currentNode = currentNode.offsetParent;
        }

        // OffsetTop from the last iteration was relative to the content of the offsetParent
        if (currentNode == formParent) {
            // deduct the targetElement's offsetTop
            // No need to adjust for border / padding in this case
            top -= isc.Element.getOffsetTop(formElement)
        }
        
        return top;
                      
    },
    
    //>@method  getIconTop()    (A)
    //  Returns the (offset) top-coordinate of an icon within its containing widget.
    //  @param  icon    (object)    icon definition
    //  @return (number)    icon top position in px
    //  @visibility external    
    //<
    getIconTop : function (icon) {
        // default to the first icon, if possible
        if (icon == null && this.icons != null && this.icons.getLength() > 0) icon = this.icons[0];
        else if (!this._isValidIcon(icon)) {
            this.logWarn("getIconTop() passed invalid icon:" + isc.Log.echoAll(icon));
            return null;
        }
        // Note we're interested in the position of the img rather than the actual link element
        var iconElement = this._getIconImgElement(icon);
        if (iconElement == null) {
            this.logWarn("getIconTop() unable to return position of icon - " +
                         "this icon is not currently drawn into the page. Returning null");
            return null;
        }

        // Determine offsetTop wrt containing widget
        return isc.Element.getTopOffset(iconElement, this.containerWidget.getClipHandle());
    },

    //> @method formItem.getPageLeft()
    // Returns the drawn page-left coordinate of this form item in pixels.
    // @return (int) page-left coordinate in px
    // @group positioning
    // @visibility external
    //<
    
    getPageLeft : function () {
        var cw = this.containerWidget;
        var adjustForRTL = (this.isRTL() && cw.overflow != isc.Canvas.VISIBLE);
        if (!adjustForRTL) {
            return this.getLeft() + 
                   ((this.containerWidget.getPageLeft() 
                        + this.containerWidget.getLeftMargin() 
                        + this.containerWidget.getLeftBorderSize())
                   - this.containerWidget.getScrollLeft());
        } else {
            var maxScroll = cw.getScrollRight();
            return this.getLeft() +
                (this.containerWidget.getPageLeft() 
                        + this.containerWidget.getLeftMargin() 
                        + this.containerWidget.getLeftBorderSize()
                        + (cw.vscrollOn ? cw.getScrollbarSize() : 0))
                - (cw.getScrollLeft() - maxScroll);
        }
    },

    //> @method formItem.getPageTop()
    // Returns the drawn page-top coordinate of this form item in pixels.
    // @return (int) page-top coordinate in px
    // @group positioning
    // @visibility external
    //<
    getPageTop : function () {
        return this.getTop() + 
                ((this.containerWidget.getPageTop() 
                    + this.containerWidget.getTopMargin() 
                    + this.containerWidget.getTopBorderSize())
                - this.containerWidget.getScrollTop());
    },

    getTitlePageLeft : function () {
        return this.getTitleLeft() + 
               ((this.containerWidget.getPageLeft() 
                    + this.containerWidget.getLeftMargin() 
                    + this.containerWidget.getLeftBorderSize())
                - this.containerWidget.getScrollLeft());
    },
    getTitlePageTop : function () {
        return this.getTitleTop() + 
                ((this.containerWidget.getPageTop() 
                    + this.containerWidget.getTopMargin() 
                    + this.containerWidget.getTopBorderSize())
                - this.containerWidget.getScrollTop());
    },

    //>@method  getIconRect()    (A)
    //  Returns the size / position of an icon with respect to the widget rendering out the
    //  form item as an array of coordinates.
    //  @param  icon    (object)    icon definition for the icon you want to determine the 
    //                              position of (defaults to first icon in this.icons).
    //
    //  @return (array)    four element array representing the Left, Top, Width, and Height of
    //                      the icon in px.
    //  @visibility external
    //<
	getIconRect : function (icon) {
        // if the icon param is empty, it will be defaulted to the first icon by each of the
        // getIcon...() methods.
        return [this.getIconLeft(icon), 
                this.getIconTop(icon), 
                this.getIconWidth(icon), 
                this.getIconHeight(icon)];
    },

    //>@method  getIconPageRect()    (A)
    //  Returns the size / position of an icon on the page as an array of coordinates.
    //  @param  icon    (object)    icon definition for the icon you want to determine the 
    //                              position of (defaults to first icon in this.icons).
    //
    //  @return (array)    four element array representing the Left, Top, Width, and Height of
    //                      the icon in px.
    //  @visibility external
    //<
    getIconPageRect : function (icon) {
        var rect = this.getIconRect(icon);
        rect[0] += this.containerWidget.getPageLeft();
        rect[1] += this.containerWidget.getPageTop();
        return rect;
    },
    
    
    // Mark the form for redraw if 'setProperties()' is called passing in
    // any properties that would effect the layout of the form.
    // Call 'updateState()' if setProperties modifies any properties that would effect the
    // styling of the form item elements
    _relayoutProperties : {
        colSpan:true,
        rowSpan:true,
        startRow:true,
        endRow:true,
        showTitle:true,
        showHint:true
    },
    _stylingProperties:{
        baseStyle:true,
        showErrorStyle:true,
        //showDisabled:true // handled by explicit setShowDisabled method
        showFocused:true,
        showErrorStyle:true,
        controlStyle:true,
        pickerIconStyle:true,
        textBoxStyle:true
    },
    _$itemCellStyle:"itemCellStyle",
    propertyChanged : function (prop, val) {
        if (this._relayoutProperties[prop] == true) this._requiresFormRedraw = true;
        if (this._stylingProperties[prop] == true) this.updateState();
        if (prop == this._$itemCellStyle && this.items) {
            for (var i = 0; i< this.items.length; i++) {
                this.items[i].updateState();
            }
        }
    },
    
    doneSettingProperties : function () {
        if (this._requiresFormRedraw) {
            var form = this.form, items = form.items;
            
            items._rowTable = null;
            form.markForRedraw();
        }
        delete this._requiresFormRedraw;
    },
    
    //>EditMode
    // ---------------------------------------------------------------------------------------
    // EditMode.  Ideally, these methods would be defined in EditMode.js, but it introduces 
    // dependency issues and causes JS errors when loading some SmartClient apps (not the 
    // DocViewer or Visual Builder, for some reason)
    
    setEditMode : function (editingOn, editContext, editNode) {
        if (editingOn == null) editingOn = true;
        if (this.editingOn == editingOn) return;
        this.editingOn = editingOn;

        if (this.editingOn) {
            this.editContext = editContext;
        }
        
        this.editNode = editNode;
    },
    
    editClick : function () {
        var left = this.getTitlePageLeft(),
            width = this.getVisibleTitleWidth(),
            top, titleTop,
            height, titleHeight;
            
        titleTop = this.getTitlePageTop();
        titleHeight = this.getTitleVisibleHeight();
        height = this.getVisibleHeight();
        top = (titleHeight == height) ? titleTop : titleTop + ((titleHeight - height) / 2);
            
        isc.EditContext.manageTitleEditor(this, left, width, top, null);
    },
    //<EditMode

    // Expression parsing

    //> @attr formItem.allowExpressions (boolean : null : IRW)
    // For a form that produces filter criteria
    // (see +link{dynamicForm.getValuesAsCriteria,form.getValuesAsCriteria()}), allows the user
    // to type in simple expressions to cause filtering with that operator.  For
    // example, entering "&gt;5" means values greater than 5, and "&gt;0 and &lt;5" means values between
    // 0 and 5.
    // <P>
    // The following table lists character sequences that can be entered as a prefix to a value, 
    // and the corresponding +link{OperatorId,operator} that will be used. 
    // <P>
    // <table style='font-size:14;'>
    // <tr><td><b>Prefix</b></td><td><b>Operator</b></td></tr>
    // <tr><td>&lt;</td><td>lessThan</td></tr>
    // <tr><td>&gt;</td><td>greaterThan</td></tr>
    // <tr><td>&lt;=</td><td>lessThanOrEqual</td></tr>
    // <tr><td>&gt;=</td><td>greaterThanOrEqual</td></tr>
    // <tr><td>someValue...someValue</td><td>betweenInclusive</td></tr>
    // <tr><td>!</td><td>notEqual</td></tr>
    // <tr><td>^</td><td>startsWith</td></tr>
    // <tr><td>|</td><td>endsWith</td></tr>
    // <tr><td>!^</td><td>notStartsWith plus logical not</td></tr>
    // <tr><td>!@</td><td>notEndsWith plus logical not</td></tr>
    // <tr><td>~</td><td>contains</td></tr>
    // <tr><td>!~</td><td>notContains</td></tr>
    // <tr><td>#</td><td>isNull</td></tr>
    // <tr><td>!#</td><td>isNotNull</td></tr>
    // <tr><td>==</td><td>exact match (for fields where 'contains' is the default)</td></tr>
    // </table>
    // <P>
    // Two further special notations are allowed:
    // <ul>
    // <li> /<i>regex</i>/ means the value is taken as a regular expression and applied via the
    // "regexp" operator
    // <li> =.<i>fieldName</i> means the value should match the value of another field.  Either the
    // user-visible title of the field (field.title) or the field's name (field.name) may be used.
    // </ul>
    // <P>
    // In all cases, if an operator is disallowed for the field (via
    // +link{dataSourceField.validOperators,field.validOperators} at either the dataSource or field
    // level), the operator character is ignored (treated as part of a literal value).
    // <P>
    // By default, the case-insensitive version of the operator is used (eg, startsWith will
    // actually use "iStartsWith").  To avoid this, explicitly set item.operator (the default
    // operator) to any case sensitive operator (eg "equals" or "contains") and case sensitive
    // operators will be used for user-entered expressions.
    // <P>
    // Compound expressions (including "and" and "or") are allowed only for numeric or date/time
    // types.
    // <P>
    // Note that if the user does not type a prefix or use other special notation as described
    // above, the operator specified via +link{formItem.operator} is used, or if
    // <code>formItem.operator</code> is unspecified, a default operator chosen as described
    // under +link{formItem.operator}.  
    // <p>
    // Also note that whatever you enter will be used literally, including any whitespace
    // characters. For example if you input '== China ' then ' China ' will be the value.
    // <p>
    // The <code>allowExpression</code> behavior can be enabled for every field in a form via
    // +link{dynamicForm.allowExpressions}.
    // <P>
    // Finally, note that, like +link{formItem.operator}, enabling
    // <code>allowExpressions:true</code> causes
    // +link{dynamicForm.getValuesAsCriteria,form.getValuesAsCriteria()}) to return
    // +link{AdvancedCriteria}.
    //
    // @group advancedFilter
    // @visibility external
    //<
    

    //> @attr formItem.validOperators (Array of OperatorId : null : IR)
    // Array of valid filtering operators (eg "greaterThan") that are legal for this FormItem.
    // <P>
    // Applies only to form/formItem when +link{formItem.allowExpressions} is true, allowing the
    // user to input expressions.
    // @group advancedFilter
    // @visibility external
    //<

    
    parseValueExpressions : function (value, fieldName, operator) {
        var type = this.getType(),
            typeInheritsFromTime = isc.SimpleType.inheritsFrom(type, "time"),
            isValidLogicType = (isc.SimpleType.inheritsFrom(type, "integer") ||
                isc.SimpleType.inheritsFrom(type, "float") ||
                isc.SimpleType.inheritsFrom(type, "date") ||
                typeInheritsFromTime
            ),
            opIndex = isc.DynamicForm.getOperatorIndex(),
            validOps = isc.getKeys(opIndex),
            result = { operator: "and", criteria: [] },
            crit = result.criteria,
            valueParts = [],
            allowEx = this._shouldAllowExpressions(),
            ds = isc.DS.get(this.form.expressionDataSource || this.form.dataSource)
        ;

        if (!value) value = this.getValue();
        if (!value) return;

        if (!isc.isA.String(value)) value += "";

        if (typeInheritsFromTime) {
            value = isc.Time._prepForParseValueExpressions(value);
        }

        var defOpName = this.getOperator();
        if (defOpName) validOps.add(defOpName);

        var defOp = ds ? ds.getSearchOperator(defOpName) : { id: defOpName };
        
        var insensitive = defOp.caseInsensitive;

        if (isValidLogicType && value.contains(" and ")) {
            valueParts = value.split(" and ");
        } else if (isValidLogicType && value.contains(" or ")) {
            valueParts = value.split(" or ");
            result.operator = "or";
        } else if (value.contains("...")) {
            valueParts = value.split("...");
            if (valueParts.length == 2) {
                var tempOps = opIndex["..."],
                    tempOp;

                if (tempOps) tempOp = (insensitive ? tempOps.find("caseInsensitive", true) : tempOps[0]);

                var field = ds ? ds.getField(fieldName) : null;

                if (field) {
                    if (isc.SimpleType.inheritsFrom(field.type, "date")) {
                        valueParts[0] = new Date(Date.parse(valueParts[0]));
                        valueParts[0].logicalDate = true;
                        valueParts[1] = new Date(Date.parse(valueParts[1]));
                        valueParts[1].logicalDate = true;
                    } else if (isc.SimpleType.inheritsFrom(field.type, "time")) {
                        var baseDatetime = isc.Time.createLogicalTime(0, 0, 0, 0);
                        valueParts[0] = isc.Time.parseInput(valueParts[0], false, false, false, baseDatetime);
                        baseDatetime.setSeconds(59, 999);
                        valueParts[1] = isc.Time.parseInput(valueParts[1], false, false, false, baseDatetime);
                    } else if (field.type == "text") {
                        
                        if (!valueParts[1].endsWith(this._betweenInclusiveEndCrit)) {
                            valueParts[1] += this._betweenInclusiveEndCrit;
                        }
                    }
                }

                return { fieldName: fieldName, operator: tempOp.ID, 
                    start: valueParts[0], end: valueParts[1] };
            }
        } else {
            valueParts = [value];
        }

        var skipTheseOps = [ " and ", " or " ];

        for (var i=0; i<valueParts.length; i++) {
            var valuePart = valueParts[i],
                subCrit = { fieldName: fieldName }
                field = ds ? ds.getField(fieldName) : null,
                isDateField = (field ? field && isc.SimpleType.inheritsFrom(field.type, "date") : false),
                isTimeField = (field ? field && isc.SimpleType.inheritsFrom(field.type, "time") : false),
                valueHasExpression = false
            ;

            var exactMatchKey = null;
            // Looking for exact operator match, if we will found it we should consider only this
            // operator
            for (var key in opIndex) {
                if (!key) continue;
                if (isc.isA.String(valuePart) && valuePart.startsWith(key)) {
                    exactMatchKey = key;
                    break;
                }
            }

            for (var key in opIndex) {
                if (!key) continue;
                if (exactMatchKey && key != exactMatchKey) continue;
                var ops = opIndex[key],
                    wildCard = false,
                    op
                ;

                if (key == "==" && isc.isA.String(valuePart) && valuePart.startsWith("=") && 
                        !valuePart.startsWith("==") && !valuePart.startsWith("=(")) 
                {
                    wildCard = true;
                }

                if (ops && ops.length) {
                    var finalOps = ops.findAll("caseInsensitive", insensitive);
                    if (finalOps == null || finalOps.length == 0) finalOps = ops;
                    if (finalOps.length > 1) {
                        
                    }
                    op = finalOps[0];
                }                

                if (!op || !op.symbol || skipTheseOps.contains(op.symbol)) {
                    continue;
                }
                
                if (validOps.contains(op.symbol) && (
                        (isc.isA.String(valuePart) && (valuePart.startsWith(op.symbol) || 
                            
                            (op.symbol == "..." && valuePart.contains(op.symbol)))
                        ) 
                        || wildCard))
                {
                    valueHasExpression = true;
                
                    if (valuePart.startsWith(op.symbol)) {
                        valuePart = valuePart.substring(op.symbol.length - (wildCard ? 1 : 0));
                    }

                    if (op.closingSymbol) {
                        // this is a containing operator (inSet, notInSet), with opening and 
                        // closing symbols...  check that the value endsWith the correct 
                        // closing symbol and strip it off - op.processValue() will split 
                        // the string for us later
                        if (valuePart.endsWith(op.closingSymbol)) {
                            valuePart = valuePart.substring(0, valuePart.length - op.closingSymbol.length);
                        }
                    }

                    if (valuePart.contains("...")) {
                        // allow range operators as well as conjunctives
                        var rangeValueParts = valuePart.split("...");
                        if (rangeValueParts.length == 2) {
                            var tempOps = opIndex["..."],
                                tempOp;

                            if (tempOps) tempOp = (insensitive ? tempOps.find("caseInsensitive", true) : tempOps[0]);

                            var field = ds ? ds.getField(fieldName) : null;

                            if (field) {
                                if (isc.SimpleType.inheritsFrom(field.type, "date")) {
                                    rangeValueParts[0] = new Date(Date.parse(rangeValueParts[0]));
                                    rangeValueParts[0].logicalDate = true;
                                    rangeValueParts[1] = new Date(Date.parse(rangeValueParts[1]));
                                    rangeValueParts[1].logicalDate = true;
                                } else if (isc.SimpleType.inheritsFrom(field.type, "time")) {
                                    var baseDatetime = isc.Time.createLogicalTime(0, 0, 0, 0);
                                    rangeValueParts[0] = isc.Time.parseInput(rangeValueParts[0], false, false, false, baseDatetime);
                                    baseDatetime.setSeconds(59, 999);
                                    rangeValueParts[1] = isc.Time.parseInput(rangeValueParts[1], false, false, false, baseDatetime);
                                } else if (field.type == "text") {
                                    
                                    if (!rangeValueParts[1].endsWith(this._betweenInclusiveEndCrit)) {
                                        rangeValueParts[1] += this._betweenInclusiveEndCrit;
                                    }
                                }
                            }

                            result.criteria.add({ fieldName: fieldName, operator: tempOp.ID, 
                                start: rangeValueParts[0], end: rangeValueParts[1] 
                            });

                            continue;
                        }
                    }

                    if (isDateField) {
                        valuePart = new Date(Date.parse(valuePart));
                        valuePart.logicalDate = true;
                    } else if (isTimeField) {
                        var baseDatetime = null;

                        // lessThan, lessOrEqual
                        if (op.upperBounds) {
                            if (op.inclusive) baseDatetime = isc.Time.createLogicalTime(0, 0, 59, 999);
                            else baseDatetime = isc.Time.createLogicalTime(0, 0, 0, 0);

                        // greaterThan, greaterOrEqual
                        } else if (op.lowerBounds) {
                            if (op.inclusive) baseDatetime = isc.Time.createLogicalTime(0, 0, 0, 0);
                            else baseDatetime = isc.Time.createLogicalTime(0, 0, 59, 999);
                        }

                        valuePart = isc.Time.parseInput(valuePart, false, false, false, baseDatetime);
                    }

                    subCrit.operator = op.ID;

                    if (op.processValue) {
                        valuePart = op.processValue(valuePart, ds);
                    }

                    if (op.wildCard && isc.isA.String(valuePart) && valuePart.contains(op.wildCard)) {
                        // this is an operator that supports wildCards (equals, notEquals)...
                        

                        // Convert the existing wildcard characters into something the DataSource
                        // will understand, namely the DataSource.patternMultiWildcard value.
                        var matchesPatternWildcard = ds ? ds.patternMultiWildcard : "*";
                        matchesPatternWildcard = isc.isA.Array(matchesPatternWildcard) ? matchesPatternWildcard[0] : matchesPatternWildcard;
                        var convertedValue = valuePart.replaceAll(op.wildCard, matchesPatternWildcard);

                        // Add the matchesPattern criteria and let the DataSource handle the
                        // conversion from here.
                        var op = insensitive ? "iMatchesPattern" : "matchesPattern";
                        if (operator) {
                            op = operator;
                        }
                        result.criteria.add({
                            operator: op,
                            fieldName: fieldName,
                            value: convertedValue
                        });

                        subCrit.operator = null;
                        this._lastValueHadWildCards = true;
                    } else {
                        // set the value if one is required for the op
                        if (op.valueType != "none") subCrit.value = valuePart;
                    }

                    break;
                }
                
            }
            if (!valueHasExpression) {
                // this was a straight expression like "10"
                subCrit.operator = defOpName;
                subCrit.value = valuePart;
            }
            if (subCrit.operator) result.criteria.add(subCrit);
        }
//        this.logWarn("Parsed expression:" + value + " to criterion:" + this.echo(result));
        if (result.criteria.length == 1) result = result.criteria[0];
        if (result.criteria && result.criteria.length == 0) result = null;

        return result;
    },

    flattenExpressionCriteria : function (crit) {
        var result = [];

        for (var i=0; i<crit.length; i++) {
            var subCrit = crit[i];

            if (!subCrit.criteria) {
                result.add(subCrit);
            } else {
                result.addList(this.flattenExpressionCriteria(subCrit.criteria))
            }
        }

        return result;
    },

    
    
    useWildCardsByDefault: true,
    _betweenInclusiveEndCrit: "ZZZZZZZZZZ",
    buildValueExpressions : function (advancedCriteria) {
        var fullCrit = advancedCriteria,
            crit = isc.shallowClone(fullCrit),
            defaultConjunctive = " " + crit.operator + " ",
            // we're going to support more than one identical conjunctive (like 10 or 20 or 30)
            // and also multiple "between" operators simultaneaously (like 10...20 and 20...30)
            conjunctives = [defaultConjunctive],
            values = [],
            result = "",
            ds = isc.DS.get(this.form.expressionDataSource || this.form.dataSource)
        ;

        if (fullCrit.criteria) {
            crit.criteria = this.flattenExpressionCriteria(fullCrit.criteria);
        } 

        var opIndex = isc.DynamicForm.getOperatorIndex(),
            opList = isc.getKeys(opIndex),
            validOps = this.validOperators
        ;

        if (!validOps) {
            validOps = [];
            for (var j=0; j< opList.length; j++) {
                var opSymbol = opIndex[opList[j]];
                validOps.addList(opSymbol.getProperty("ID"));
            }
        }

        var defOpName = this.getOperator();
        if (defOpName) validOps.add(defOpName);

        var defOp = ds ? ds.getSearchOperator(defOpName) : { id: defOpName };

        var insensitive = defOp.caseInsensitive,
            hasWildCards = false,
            wildCard
        ;

        if (!crit.criteria) {
            var critArray = [ crit ];
            crit = { criteria: critArray };
        }

        var opsWithWildCards = ["startsWith", "iStartsWith", "contains", "iContains",
                "endsWith", "iEndsWith"
        ];
        
        if (this.useWildCardsByDefault && this.type == "text" && 
                (crit.criteria.length > 1 ||
                (crit.criteria.length == 1 && 
                    opsWithWildCards.contains(crit.criteria[0].operator) &&
                    crit.criteria[0].value && crit.criteria[0].value.startsWith("=") &&
                    !crit.criteria[0].value.startsWith("==") && !crit.criteria[0].value.startsWith("=(")
                ) || this._lastValueHadWildCards

            )) 
        {
            
            hasWildCards = true;
            var opSymbol = opIndex["=="];
            var equalsOp = opSymbol.find({ "ID": "equals" });
            wildCard = equalsOp.wildCard;
            conjunctives[0] = "";
        }
        
        var conjunctiveOffset=0;

        for (var i=0; i < crit.criteria.length; i++) {
            var subCrit = crit.criteria[i],
                subOp = subCrit.operator,
                value = subCrit.value,
                field = ds ? ds.getField(subCrit.fieldName) : null
            ;

            for (var j=0; j< opList.length; j++) {
                var opSymbol = opIndex[opList[j]];
                var tempOp = opSymbol.find({ "ID": subOp });
                if (tempOp) {
                    subOp = tempOp;
                    break;
                }
            }

            if (i>0) {
                conjunctives.add(defaultConjunctive);
            }

            if (isc.isA.String(subOp)) {
                this.logWarn("Unknown filter-expression operator: '" + subOp + "'");
            } else if (hasWildCards) {
                // we have wildCards
                if (subOp.ID == "contains" || subOp.ID == "iContains") {
                    if (values[values.length-1] != wildCard) values.add(wildCard);
                    values.add(subCrit.value);
                    values.add(wildCard);
                } else if (subOp.ID == "startsWith" || subOp.ID == "iStartsWith") {
                    values.add(subCrit.value);
                    values.add(wildCard);
                } else if (subOp.ID == "endsWith" || subOp.ID == "iEndsWith") {
                    if (values[values.length-1] != wildCard) values.add(wildCard);
                    values.add(subCrit.value);
                }
            } else if (subOp.ID == defOpName) {
                values.add(this._formatCriterionValue(subCrit.value));
            } else if (subOp.ID == "betweenInclusive" || subOp.ID == "iBetweenInclusive") {
                if (crit.criteria.length > 1) conjunctives.addAt(subOp.symbol, conjunctiveOffset);
                else conjunctives[conjunctiveOffset] = subOp.symbol
                // make sure the ... conjunctive is in the correct place 
                conjunctiveOffset++;
                
                var endVal = subCrit.end;
                if (field && field.type == "text") {
                    if (endVal && endVal.endsWith(this._betweenInclusiveEndCrit)) {
                        endVal = endVal.replace(this._betweenInclusiveEndCrit, "");
                    }
                }
                var startVal = this._formatCriterionValue(subCrit.start);
                endVal = this._formatCriterionValue(endVal);
                if (startVal != endVal) values.addList([ startVal, endVal ]);
                else values.add(startVal);
            } else if (subOp.ID == "isNull" || subOp.ID == "notNull") {
                values.add(subOp.symbol);
            } else if (validOps.contains(subOp.ID)) {
                var op = subOp;
                if (isc.isAn.Array(value)) value = value.join(subOp.valueSeparator);
                if (op.ID != defOp) {
                    value = (op && op.symbol ? op.symbol : "") + this._formatCriterionValue(value);
                    if (op.closingSymbol) value += op.closingSymbol;
                }
                values.add(value);
            } else if (subOp.ID.startsWith("i")) {
                var otherOp = subOp.ID.substring(1),
                    initial = otherOp.charAt(0)
                ;
                otherOp = initial.toLowerCase() + otherOp.substring(1)
                if (validOps.contains(otherOp)) {
                    var op2 = opList.find("ID", otherOp);
                    if (op2.ID != defOp) {
                        value = (op && op.symbol ? op.symbol : "") + this._formatCriterionValue(value);
                        if (op.closingSymbol) value += op.closingSymbol;
                    }
                    values.add(value);
                }
            }
            
            
            conjunctiveOffset++;
        }

        if (hasWildCards) values.addAt("=", 0);
        
        if (conjunctives.length > 1) {
            // if the expression includes multiple expressions, the individual ones may also 
            // contain a between conjunctive - build the string up manually
            for (var i=0; i<values.length; i++) {
                result += values[i];
                if (i<values.length-1) result += conjunctives[i];
            }
        } else {
            result = values.join((values.length > 1 ? conjunctives[0] : ""));
        }

        delete this._lastValueHadWildCards;

        return result.length > 0 ? result : null;
    },

    _formatCriterionValue : function (value) {
        return String(value);
    }
});

isc.FormItem.registerStringMethods({
	//>	@method formItem.showIf() (A)
	// Expression that's evaluated to see if an item should be dynamically hidden.
    // <p>
    // <code>showIf()</code> is evaluated whenever the form draws or redraws.
    // <P>
    // Note that explicit calls to +link{formItem.show()} or +link{formItem.hide()} will
    // will wipe out the <code>showIf</code> expression.
    //
    // @param	item	(FormItem)    the form item itself (also available as "this")
    // @param	value   (any)         current value of the form item
    // @param	form    (DynamicForm) the managing DynamicForm instance
    // @param   values  (Object)      the current set of values for the form as a whole
    // @return (boolean) whether the item should be shown
    // 
    // @example formShowAndHide
    // @visibility external
	//<
    showIf : "item,value,form,values",

	//> @method formItem.defaultDynamicValue() (A)
    // Expression evaluated to determine the +link{FormItem.defaultValue} when no value is 
    // provided for this item.
    // <P>
    // If you don't need dynamic evaluation, you can just use <code>item.defaultValue</code>.
    //
    // @param	item	(FormItem)    the form item itself (also available as "this")
    // @param	form    (DynamicForm) the managing DynamicForm instance
    // @param   values  (Object)      the current set of values for the form as a whole
    // 
    // @see attr:defaultValue
    // @group formValues
    // @visibility external
    //<
    defaultDynamicValue : "item,form,values",

    //> @method formItem.focus
    // Called when this FormItem receives focus.
    // 
    // @param	form    (DynamicForm) the managing DynamicForm instance
    // @param	item	(FormItem)    the form item itself (also available as "this")
    // @group eventHandling
    // @visibility external
    //<
    focus : "form,item",

    //> @method formItem.blur
    // Called when this FormItem loses focus.
    // 
    // @param	form    (DynamicForm) the managing DynamicForm instance
    // @param	item	(FormItem)    the form item itself (also available as "this")
    // @group eventHandling
    // @visibility external
    //<
    blur : "form,item",
    
        

    //> @method formItem.editorEnter()
    // Notification method fired when the user enters this formItem.
    // Differs from +link{formItem.focus()} in that while <code>focus</code> and <code>blur</code>
    // may fire multiple as the user navigates sub elements of an item (such as interacting
    // with a pick list), <code>editorEnter</code> will typically fire once when the user 
    // starts to edit this item as a whole, and once when the user moves onto a different
    // item or component
    // @param form (DynamicForm) form containing this item
    // @param item (FormItem) form item recieving focus
    // @param value (Any) current item value.
    // @group eventHandling
    // @visibility external
    //<
    editorEnter : "form,item,value",
    
    //> @method formItem.editorExit
    // Notification method fired when the user leaves this formItem.
    // Differs from +link{formItem.blur()} in that while <code>focus</code> and <code>blur</code>
    // may fire multiple as the user navigates sub elements of an item (such as interacting
    // with a pick list), <code>editorEnter</code> will typically fire once when the user 
    // starts to edit this item as a whole, and <code>editorExit</code> fires once when the 
    // user moves onto a different item or component
    // @param form (DynamicForm) form managing this form item
    // @param item (FormItem) pointer to the form item being managed
    // @param value (any) current value of the form item
    // @group eventHandling
    // @visibility external
    //<
    editorExit : "form,item,value",
 
    //> @method formItem.click
    // Called when this FormItem is clicked on.
    // <P>
    // Note: <code>click()</code> is available on StaticTextItem, BlurbItems, ButtonItem, and
    // derivatives.  Other form items (such as HiddenItem) do not support <code>click()</code>.
    //
    // @param	form    (DynamicForm) the managing DynamicForm instance
    // @param	item	(FormItem)    the form item itself (also available as "this")
    // @return (boolean) Return false to cancel the click event. This will prevent the event from
    //   bubbling up, suppressing
    //   +link{canvas.click,click} on the form containing this item.
    // @group eventHandling
    // @visibility external
    //<
    click : "form,item",

    //> @method formItem.doubleClick
    // Called when this FormItem is double-clicked.
    //
    // @param	form    (DynamicForm) the managing DynamicForm instance
    // @param	item	(FormItem)    the form item itself (also available as "this")
    // @return (boolean) Return false to cancel the doubleClick event. This will prevent the event from
    //   bubbling up, suppressing
    //   +link{canvas.doubleClick,doubleClick} on the form containing this item.
    // @group eventHandling
    // @visibility external
    //<
    doubleClick : "form,item",

    //> @method formItem.pickerIconClick()
    // Notification method called when the +link{showPickerIcon,picker icon} is clicked.
    // @param form (DynamicForm) the DynamicForm containing the picker icon's item.
    // @param item (FormItem) the FormItem containing the picker icon.
    // @param pickerIcon (FormItemIcon) the picker icon.
    // @group pickerIcon
    // @visibility external
    //<
    pickerIconClick : "form,item,pickerIcon",

    //> @method formItem.iconClick()
    //  Notification method called when the user clicks on a form item icon.
    //  <p>
    //  The icon's +link{FormItemIcon.click()} method if any is called first. Then, if the clicked
    //  icon is the +link{showPickerIcon,picker icon}, the +link{pickerIconClick()} method is
    //  called. Then, this method is called.
    //  @group  formIcons
    //  @visibility external
    //  @param form (DynamicForm)   a pointer to this item's form
    //  @param  item    (FormItem)  a pointer to this form item
    //  @param  icon    (FormItemIcon)  a pointer to the icon that received the click event.
    //<
    // Note - developers would be more likely to set a click handler on each icon.
    iconClick : "form,item,icon",    

    //> @method formItem.iconKeyPress()
    //      StringMethod.
    //      Default action to fire when an icon has keyboard focus and the user types a key.
    //      May be overridden by setting <code>keyPress</code> on the form item icon directly.
    //  @group  formIcons
    //  @visibility external
    //  @param keyName (string) name of the key pressed
    //  @param character (character) character produced by the keypress
    //  @param form (DynamicForm)   a pointer to this item's form
    //  @param  item    (FormItem)  a pointer to this form item
    //  @param  icon    (FormItemIcon)  a pointer to the icon that received the click event.
    //<
    iconKeyPress : "keyName,character,form,item,icon",    

    //> @method formItem.change()
    // Called when a FormItem's value is about to change as the result of user interaction.  This
    // method fires after the user performed an action that would change the value of this field,
    // but before the element itself is changed.  
    // <P>
    // Returning false cancels the change.  Note that if what you want to do is
    // <b>transform</b> the user's input, for example, automatically change separator
    // characters to a standard separator character, you should implement
    // +link{formItem.transformInput,transformInput} rather than using a combination of
    // change() and setValue() to accomplish the same thing.  Returning false from
    // <code>change</code> is intended for rejecting input entirely, such as typing invalid
    // characters.
    // <p>
    // Note that if you ask the form for the current value in this handler, you will get the old
    // value because the change has not yet been committed.  The new value is available as a
    // parameter to this method.
    //
    // @param	form    (DynamicForm) the managing DynamicForm instance
    // @param	item	(FormItem)    the form item itself (also available as "this")
    // @param   value   (any)         The new value of the form item
    // @param   oldValue    (any)     The previous value of the form item
    //
    // @return  (boolean) In your handler, return false to cancel the change, true to allow the change
    // @group eventHandling
    // @visibility external
    // @example fieldEnableDisable
    //<
	change : "form,item,value,oldValue",
    
    //> @method formItem.changed()
    // Called when a FormItem's value has been changed as the result of user interaction.  This
    // method fires after the newly specified value has been stored.
    // @param	form    (DynamicForm) the managing DynamicForm instance
    // @param	item	(FormItem)    the form item itself (also available as "this")
    // @param   value   (any)         The current value (after the change).
    // @group eventHandling
    // @visibility external
    //<
    changed : "form,item,value",
	


    //> @method formItem.transformInput()
    // Called when a FormItem's value is about to change as the result of user interaction.  This
    // method fires after the user performed an action that would change the value of this field,
    // and allows the developer to modify / reformat the value before it gets validated / saved.
    // Fires before +link{formItem.change}.
    // <P>
    // Return the reformatted value.
    //
    // @param	form    (DynamicForm) the managing DynamicForm instance
    // @param	item	(FormItem)    the form item itself (also available as "this")
    // @param   value   (any)         The new value of the form item
    // @param   oldValue    (any)     The previous (current) value of the form item
    //
    // @return  (any) The desired new value for the form item
    // @visibility external
    //<
    transformInput : "form,item,value,oldValue",
    
    
    cellClick : "form,item",
    cellDoubleClick : "form,item",
    
    //> @method formItem.titleClick()
    // Notification method fired when the user clicks the title for this item
    // @param form (DynamicForm) the managing DynamicForm instance
    // @param item (FormItem) the form item whose title was clicked
    // @return (boolean) Return false to cancel the click event. This will prevent the event from
    //   bubbling up, suppressing
    //   +link{canvas.click,click} on the form containing this item.
    // @visibility external
    //<
    titleClick : "form,item",
    //> @method formItem.titleDoubleClick()
    // Notification method fired when the user double-clicks the title for this item
    // @param form (DynamicForm) the managing DynamicForm instance
    // @param item (FormItem) the form item whose title was double-clicked
    // @return (boolean) Return false to cancel the doubleclick event. This will prevent the event from
    //   bubbling up, suppressing
    //   +link{canvas.doubleClick,doubleClick} on the form containing this item.
    // @visibility external
    //<
    titleDoubleClick : "form,item",

    mouseMove : "form,item", 
    mouseOver : "form,item", 
    mouseOut : "form,item", 
    titleMove : "form,item", 
    titleOver : "form,item",
    titleOut : "form,item",
    textBoxMove : "form,item",
    textBoxOver : "form,item",
    textBoxOut : "form,item",

        
    itemHover : "item,form",
    titleHover : "item,form",
    valueHover : "item,form",

    //> @method formItem.keyPress()
    // StringMethod fired when the user presses a key while focused in this form item.
    //
    // @param item (FormItem) Item over which the keypress occurred
    // @param form (DynamicForm) Pointer to the item's form
    // @param keyName (KeyName) Name of the key pressed (Example: <code>"A"</code>, <code>"Enter"</code>)
    // @param characterValue (number) If this was a character key, this is the numeric value
    //        for the character
    //
    // @return (boolean) return false to attempt to cancel the event.  Note for general purpose
    //                   APIs for managing whether user input is allowed, use +link{change()} 
    //                   or +link{transformInput()} instead.
    // 
    //
    // @group eventHandling
    // @visibility external
    //<
    keyPress : "item, form, keyName, characterValue",  // was keyNum, form, item

    // NOTE: characterValue not passed to keyDown/keyUp because it's not guaranteed to be
    // available for these events
    
    //> @method formItem.keyDown()
    // StringMethod fired in response to a keydown while focused in this form item.
    //
    // @param item (FormItem) Item over which the keydown occurred
    // @param form (DynamicForm) Pointer to the item's form
    // @param keyName (KeyName) Name of the key pressed (Example: <code>"A"</code>, <code>"Enter"</code>)
    // @return (boolean) return false to attempt to cancel the event.  Note for general purpose
    //                   APIs for managing whether user input is allowed, use +link{change()} 
    //                   or +link{transformInput()} instead.
    //
    // @group eventHandling
    // @visibility external
    //<    
    keyDown : "item,form,keyName",

    //> @method formItem.keyUp()
    // StringMethod fired in response to a keyup while focused in this form item.
    //
    // @param item (FormItem) Item over which the keyup occurred
    // @param form (DynamicForm) Pointer to the item's form
    // @param keyName (KeyName) Name of the key pressed (Example: <code>"A"</code>, <code>"Enter"</code>)
    // @return (boolean) return false to attempt to cancel the event.  Note for general purpose
    //                   APIs for managing whether user input is allowed, use +link{change()} 
    //                   or +link{transformInput()} instead.
    //
    // @group eventHandling
    // @visibility external
    //<    
    keyUp : "item,form,keyName",
    
    //> @method formItem.getValueIcon()
    // If specified this stringMethod allows the developer to specify the image source for an 
    // icon to be displayed for the current form item value.
    // <P>
    // Takes precedence over +link{FormItem.valueIcons}
    //
    // @param value (any) value of the item for which an item should be returned.
    //
    // @group valueIcons
    // @visibility external
    //<
    getValueIcon : "value",

    // called via DF.saveData() callback.  Return false from this method to perform async
    // processing before saveData() callback is called.  Then call form.saveDataComplete() to
    // tell the form to proceed.
    formSaved: "request,response,data",
    
    
    // Custom formatters and parsers, documented above
    formatValue:"value,record,form,item",
    formatEditorValue:"value,record,form,item",
    parseEditorValue:"value,form,item"
    
});

isc.FormItem.getPrototype().toString = function () {
    return "[" + this.Class + " ID:" + this.ID +
            (this.name != null ? " name:" + this.name : "") + "]";
};
