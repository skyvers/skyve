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
isc.defineClass("FilterEditorBody", "GridBody").addProperties({

// For iOS, we need the edit fields to be within an actual <form> element so that the return
// key will be a blue Search key.
getInnerHTML : function (printCallback) {
    var wentAsynchronous;

    var localPrintCallback = {
        target: this,
        method: function (HTML) {
            
            if (!isc.Browser.isIE) {
                HTML = "<form action='javascript:void(0)' onsubmit='return false;'>" + HTML + "</form>";
            }
            
            if (wentAsynchronous) {
                if (printCallback != null) this.fireCallback(printCallback, "HTML", [HTML]);
                return false;
            } else {
                return HTML;
            }
        }
    };

    var superInnerHTML = this.Super("getInnerHTML", [localPrintCallback], arguments);
    wentAsynchronous = (superInnerHTML === false);
    if (wentAsynchronous) {
        
        return false;
    } else {
        return localPrintCallback.method.call(this, superInnerHTML);
    }
}

});


//>	@class	RecordEditor
//
//  Component for editing a single record.<br>
//  RecordEditors are implemented as a subclass of ListGrid, showing no header and a single 
//  row always drawn in the editable state, allowing the user to modify the values at any time.
//  The +link{recordEditor.actionButton} is automatically shown as a way for the user to act 
//  upon the edited values.
//  <P>
//  The RecordEditor class exists as a helper class for ListGrids, used to provide
//  an interface for editing criteria when +link{listGrid.showFilterEditor,filterEditor} 
//  is set to true.
//
// @see listGrid.showFilterEditor
// @see listGrid.filterEditor
// @treeLocation Client Reference/Grids/ListGrid
// @visibility external
//<


// declare the class itself
isc.ClassFactory.defineClass("RecordEditor", "ListGrid");

isc.RecordEditor.addProperties({
    
    //>	@attr	recordEditor.sourceWidget   (ListGrid : undefined : R)
    //          Target ListGrid widget for which we're editing data.  Should be defined on
    //          a per-instance basis at init time.
    //<
    //    sourceWidget:null,

    autoDraw:false,
    
    cellSpacing:0, cellPadding:0,

    selectionType:"none",
    showRollOver:false,
    //> @attr recordEditor.baseStyle (CSSStyleName : "recordEditorCell" : [IR])
    // @include listGrid.baseStyle
    // @visibility external
    //<
    baseStyle:"recordEditorCell",

    // Don't show the header for the list
    showHeader:false,
    
    // If we're set up with no fields, we won't be showing the edit row until setFields is
    // called. In this case don't show the empty message.
    showEmptyMessage:false,

    
    
    bodyOverflow:"hidden",
    
    fixedRecordHeights:true,
    
    drawAllMaxCells:0,
    
    // disable all field autoFit on a record editor - we'll size fields based on our 
    // target grid.
    shouldAutoFitField:function () {return false},
    
    //> @attr recordEditor.skinImgDir (URL : "images/RecordEditor/" : IR)
    // 	Where do 'skin' images (those provided with the class) live?
    // @visibility external
    //<
    // [Note we are not showing a header so do not need access to the standard listGrid images]
	skinImgDir:"images/RecordEditor/",

	//> @attr recordEditor.saveImg (SCImgURL : "[SKIN]add.png" : IR)
	// +link{Button.icon,Icon} to show on the +link{actionButton} if this 
	// component is being used for editing records
    // <P>
	// Note that this +link{SCImgURL} will be resolved using the +link{skinImgDir} defined
	// for the RecordEditor.
    //
	// @visibility internal
	//<
	// Internal for now - we currently have no external usage for RecordEditors as
	// true "editors".
    saveImg:"[SKIN]add.png",
    
    //> @attr recordEditor.filterImg (SCImgURL : "[SKIN]filter.png" : IR)
	// +link{Button.icon,Icon} to show on the +link{actionButton} if this 
	// component is being used as a +link{listGrid.filterEditor}.
	// <P>
	// Note that this +link{SCImgURL} will be resolved using the +link{skinImgDir} defined
	// for the RecordEditor.
	//
	// @visibility external
	//<
    filterImg:{src:"[SKIN]filter.png", width:50, height:50, showOver:true},
    
    //> @attr recordEditor.actionButton (Button AutoChild : null : R)
    // Automatically created Button auto-child shown at the edge of the recordEditor.
    // For a recordEditor acting as a +link{listGrid.filterEditor}, this button will
    // show the +link{filterImg} as an +link{button.icon} by default.
    // <P>
    // Clicking this button will call +link{recordEditor.performAction()} on the editor.
    // <P>
    // May be customized using the standard +link{AutoChild} pattern, by overriding
    // +link{actionButtonProperties}.
    // @visibility external
    //< 
    
    actionButtonConstructor:isc.Button,

    // Hide the title in case the developer changes the filterImg to a blank img.
    actionButtonDefaults:{
        title:"",
        showOver:true,
        showFocusedAsOver:false
    },

    //> @attr recordEditor.actionButtonProperties (Button Properties : null : IRA)
    // Properties to apply to the automatically generated +link{recordEditor.actionButton}.
    // <P>
    // Note that for a recordEditor being used as a +link{listGrid.filterEditor}, the
    // +link{listGrid.filterButtonProperties} can be used to specify actionButton properties
    // directly at the grid level.
    //
    // @visibility external
    //<
    
    //> @attr recordEditor.actionButtonStyle (CSSStyleName : "normal" : IR)
    // +link{Button.baseStyle,baseStyle} for the +link{actionButton}
    // @visibility external
    //<
    actionButtonStyle:"normal",


    // Setting listEndEditAction to "next" allows this list to show an edit row with no associated 
    // record in the data object
    listEndEditAction:"next",
        
    // Don't show the edit click mask - we are not dismissing edits on click outside.
    _showEditClickMask : function () {},

    // specify canEdit true to allow the editRowForm to be shown.
    // Note that we aren't setting an editEvent - this widget is *always* in editable state                                
    canEdit:true,
    
    // Always show the entire row as editable
    editByCell:false,
    
    // set _resizeWithMaster to false.  We want to resize horizontally with the master, but
    // not vertically - we need to move in order to remain in position below the master instead
    // This is handled by custom logic in listGrid.resizePeersBy()
    _resizeWithMaster:false,
    
    // Apply "normal" style to the editor and the  body - we don't expect it to have
    // any borders, etc which could get applied to the GR class as a whole
    bodyStyleName:"normal",
    styleName:"normal"
    
});

//!>Deferred
isc.RecordEditor.addMethods({

    initWidget : function () {
        
        if (this.sourceWidget != null) {
            
            
            // We want the width to match the sourceWidget's width.  
            // This allows the fields to align with the sourceWidget's body columns
            var source = this.sourceWidget;
            
            this.setWidth(source.getInnerContentWidth(true));
            
            this.observe(source, "resized", "observer.sourceWidgetResized(observed)");
            
            // If the sourceWidget is not leaving a scrollbar gap, we shouldn't either - the
            // button will still float over the scrollbar area, but this ensures that any 
            // fields of width "*" will size the same in this widget as in the sourceWidget
            this.leaveScrollbarGap = this.sourceWidget.leaveScrollbarGap;
            
            // If this is a filter editor, perform a filter whenever the user changes fields
            // if this.sourceWidget.filterByCell is true
            if (this.isAFilterEditor()) {
                this.actOnCellChange = this.sourceWidget.filterByCell;
                this.actOnKeypress = this.sourceWidget.filterOnKeypress;
            } else {
                // Otherwise, if saveByCell is true, perform a save whenever the user changes
                // fields.
                this.actOnCellChange = this.sourceWidget.saveByCell;
            }
            
            // ensure that the editForm is autoFocus:false, we don't want to jump focus into
            // the form every time it gets redrawn.
            isc.addProperties(this.editFormDefaults, {autoFocus:false});

            // Pick up field ID and fields from the source widget.
            this.fieldIDProperty = this.sourceWidget.fieldIDProperty;
            this.fields = this.sourceWidget.completeFields.duplicate();
            
            // always size our row to fit inside ourselves
            this.cellHeight = this.getInnerHeight();
            
        } else {
            this.logWarn("RecordEditor initialized without a sourceWidget property. " +
                         "This widget is not supported as a standalone component.");
        }
        
        return this.Super(this._$initWidget);
    },
    
    destroy : function () {
        this.ignore(this.sourceWidget, "resized");
        this.Super("destroy", arguments);
    },
    
    // Ensure our width always fits that of our source widget
    // Note that we have to use the *visible* width -- the source grid could be autoFitData:"horizontal"
    // in which case it overflows specified size horizontally.
    sourceWidgetResized : function (source) {
        this.setWidth(source.getInnerContentWidth(true));
    },
    
    _$filter:"filter",
    isAFilterEditor : function () {
        return (this.actionType == this._$filter);
    },

    // Override formatCellValue - if this is the removeField, don't show an icon
    // No need to explicitly suppress the "removeRecord" behavior - we already
    // suppress all 'recordClick' behavior for fields
    
    _formatCellValue : function (value, record, field, rowNum, colNum) {
        if (field.isRemoveField) return "&nbsp;"
        return this.Super("_formatCellValue", arguments);
    },
    
    // Override _useDisplayFieldValue to always return false.
    // If a field has a specified displayField we don't want to be attempting to update that
    // field in the criteria specified when a user changes the editValue for this item.
    
    _useDisplayFieldValue : function (field) {
        return false;
    },

    // Override 'draw()' to ensure that we're editable
    draw : function () {
        // Ensure we're correctly sized
        
        var source = this.sourceWidget;
        this.setWidth(source.getInnerContentWidth(true));

        // findNextEditCell signature:
        // rowNum, colNum, direction, stepThroughFields, checkStartingCell,
        //          checkPastListEnd, dontCheckPastRowEnd, ignoreFocus
        // Pass in ignoreFocus - ensures we catch any fields where canEdit is true even
        // if the editor type is not canFocus true (Example: MiniDateRange)
        var firstEditCell = this.findNextEditCell(0,0,1,true,true,false,true,true);
        if (firstEditCell == null) {
            this.logInfo("No editable fields in this record editor." + 
                        (this.isAFilterEditor() ? 
                            " Check the 'canFilter' property for each field in "
                         :
                            " Check the 'canEdit' property for each field in "
                        ) + this.sourceWidget.getID());
        } else {
            // Set up a starting set of edit values based on
            // for filtering: the current filter if there is one, otherwise the field-wise 
            //                defaultFilterValue for each field
            // for editing: the default value of the field

            var isFilter = this.isAFilterEditor(),
                vals;
            if (isFilter) {
                vals = this.sourceWidget._getFilterEditorValues();            
            } else {
                var undef;

                vals = {};
                for (var i = 0; i < this.fields.length; i++) {
                    var field = this.fields[i];

                    if (field.defaultValue !== undef) {
                        vals[field[this.fieldIdProperty]] = field.defaultValue;
                    }
                }
            }

            if (this.isAFilterEditor()) this.setValuesAsCriteria(vals);
            else this.setEditValues(0, vals);
            
            var firstEditCol = firstEditCell[1];
            // Note if we don't have any fields, this is a no-op
            this._startEditing(0,firstEditCol);
        }
        
        this.Super("draw", arguments);
        // Ensure the actionButton shows up on top of the body
        this.actionButton.bringToFront();
    },
    
    setFields : function () {
        var form = this.getEditForm();
        if (this.isAFilterEditor()) {
            // for FilterEditors, if the form exists, remember it's criteria before
            // calling Super(), which will re-create it - re-apply this criteria later
            this._formCrit = form ? this.getValuesAsCriteria() : null;
        }
        this.Super("setFields", arguments);
        
        if (this._formCrit) {
            form.setValuesAsCriteria(this._formCrit);
            delete this._formCrit;
        }
        
        // If we're drawn(), but we didn't have fields before this setFields call, startEditing
        // now
        this.initializeEdit();
    },
    // If we're not already editing, start editing.
    initializeEdit : function () {
        if (!this.isDrawn() || this._editorShowing) return;
        
        var firstEditCell = this.findNextEditCell(0,0,1,true,true);
        // If we're not showing any filterable fields, firstEditCell may be null
        // In this case we'll just call 'startEditing' on the first cell - it will be showing
        // a static text item so won't really be editable but ensures we show the edit form for
        // when the user shows more fields
        if (firstEditCell == null) firstEditCell = [0,0];
        this._startEditing(0, firstEditCell[1]);
    },
    showField : function () {
        this.Super("showField", arguments);
        // If we're drawn(), but we didn't have any editable fields before this showField
        // call, startEditing now
        this.initializeEdit();
    },

    // Override createChildren to make the actionButton
    createChildren : function () {    
        this.Super("createChildren", arguments);

        var editForm = this.getEditForm();
        if (editForm != null && this._initialCriteria) {
            editForm.setValuesAsCriteria(this._initialCriteria);
            delete this._initialCriteria;

            if (this.isAFilterEditor()) editForm.isSearchForm = true;
        }

        // Never allow the body to protrude past the end of the action-button
        if (!this.actionButton) this.makeActionButton();
    },

    // MakeActionButton -- this is a button that will float on top of the RecordEditor body, 
    // aligned with the scrollbar of the sourceWidget, with click set to perform the filter
    // or save action using the current set of edit values.
    makeActionButton : function () {
        var actionIcon;

        if (this.isAFilterEditor()) {
            actionIcon = this.filterImg;
        } else {
            actionIcon = this.saveImg;
        }

        
        var showOver, showFocused
        if (isc.isAn.Object(actionIcon)) {
            showOver = actionIcon.showOver;
            showFocused = actionIcon.showFocused;
            actionIcon = actionIcon.src;
        }

        var sorterWidth = this.sourceWidget._getSorterWidth();
        var dynamicProperties = {
            recordEditor:this,

            // Note we want the height to match the rendered inner height of this ListGrid
            // which is the visible height of the body.  However the body hasn't been drawn
            // yet, so we handle this via an override to adjustOverflow(), which also ensures
            // we'll be resized if the body is resized
            //height:this.body.getVisibleHeight(),
            // Float the button over where the body scrollbar would normally appear
            left:this.isRTL() ? 0 : this.getInnerWidth() - sorterWidth,
            width:sorterWidth,
            // Height is matched to the height of the RecordEditor as a whole.

            autoDraw: false,

            // use button icon only, no title
            baseStyle: this.actionButtonStyle,
            skinImgDir: this.skinImgDir,
            icon: actionIcon,
            iconSize: sorterWidth,

            prompt: this.actionButtonPrompt,

            click : function () {this.recordEditor.performAction();}
        }
        if (showOver != null) dynamicProperties.showRollOverIcon = showOver;
        if (showFocused != null) dynamicProperties.showFocusedIcon = showFocused;

        
        if (this.tabIndex != null && !this._autoTabIndex) {
            dynamicProperties.tabIndex = this.tabIndex;
        }

        this.actionButton = this.createAutoChild("actionButton", dynamicProperties);
        this.addChild(this.actionButton);
        
    },

    //> @method recordEditor.performAction()
    // Fired when the user clicks the +link{actionButton} for this RecordEditor.
    // May also be triggered from other user interaction with edit values (for
    // example filter-editor change - see +link{listGrid.filterOnKeypress}).
    // <P>
    // This is the method which initiates a filter in a listGrid
    // +link{listGrid.showFilterEditor,filter editor}. Note that for custom
    // filtering behavior, developers can use the +link{listGrid.filterEditorSubmit()}
    // notification method rather than overriding this method directly.
    //
    // @visibility external
    //<
    performAction : function (suppressPrompt, callback) {
        if (this.isAFilterEditor()) this.performFilter(suppressPrompt, callback);
        else this.performSave(suppressPrompt, callback);
    },

    // setValuesAsCriteria - for when this component is acting as a filterEditor
    // applies criteria (possibly AdvancedCriteria) to the edit form for display / modification
    setValuesAsCriteria : function (criteria, refresh, dropExtraCriteria, dropCriteriaFields) {
        var form = this.getEditForm();
        if (form == null) {
            this._initialCriteria = criteria;
            return;
        }
        form.setValuesAsCriteria(criteria, null, dropExtraCriteria, dropCriteriaFields);
        if (refresh) this.refreshRow(0);
    },
    
    // Override makeEditForm()
    // This method is called to initially create the edit form, and to update its items
    // when fields are shown/hidden / incrementally rendered etc.
    // Default implementation sets the value based on the current editValues for the row
    // When we're acting as a filterEditor, we need to apply criteria to the form instead.
    makeEditForm : function () {
        var props = this.editFormProperties = this.editFormProperties || {};
        if (props.allowExpressions == null && this.allowFilterExpressions != null) 
            props.allowExpressions = this.allowFilterExpressions;

        props.expressionDataSource = this.expressionDataSource;
        if (this.isAFilterEditor()) props.storeAtomicValues = true;

        var editForm = this.getEditForm(),
            criteria;

        props._parseExtraCriteria = true;    
        
        if (editForm == null) {
            criteria = this._initialCriteria;
        } else {
            // If the form already exists simply pick up the current values from it and
            // ensure they're still applied after the items have been reworked
            criteria = editForm.getValuesAsCriteria();
            editForm.expressionDataSource = this.expressionDataSource;
        }
        this.Super("makeEditForm", arguments);
        
        editForm = this.getEditForm();
        if (editForm != null) {
            editForm.setValuesAsCriteria(criteria);
            delete this._initialCriteria;

            if (this.isAFilterEditor()) editForm.isSearchForm = true;
        }
    },

    updateDataSource : function (ds) {
        this.expressionDataSource = ds;
        var form = this.getEditForm();
        if (form) form.expressionDataSource = this.expressionDataSource;
    },

    // Override getEditDisplayValue()
    // This method is called from refreshCellValue when we are newly showing an edit item,
    // and should return the current value to display in the item.
    // For filterEditors rely on the fact that we set the criteria directly on the DynamicForm
    // based on the criteria applied to the grid when the form is created, and when 
    // the criteria is changed programmatically. The values stored in the form are always
    // up to date.
    
    getEditDisplayValue : function (rowNum, colNum, record) {
        if (this.isAFilterEditor() && this._editRowForm != null) {
            var fieldName = this.getFieldName(colNum);
            return this._editRowForm.getValue(fieldName);
        }
        return this.Super("getEditDisplayValue", arguments);
    },
    // Also override getEditDisplayValues() - this is called from makeEditForm()
    // which gets called from "setFields()". We do this when showing
    // and hiding fields on the main grid
    getEditDisplayValues : function (rowNum, colNum) {
        if (this.isAFilterEditor() && this._editRowForm != null) {
            return this._editRowForm.getValues();
        }
        return this.Super("getEditDisplayValues", arguments);
    },

    // performFilter() - filter the source widget with the values from this widget
    // There is a problem with doing this, reported on the forums: http://forums.smartclient.com/showthread.php?t=1933
    // The problem is: if you have a datasource with a hidden field, and you programmatically 
    // filter on that hidden field, and you then try to filter through the filter editor, you lose
    // your programmatic filter.  The solution is to combine the filter editor's values with 
    // values in the existing criteria for fields we don't know about.
    performFilter : function (suppressPrompt, callback) {
        
        var criteria = this._getFilterCriteria(this.sourceWidget.autoFetchTextMatchStyle);
        var context = {};
        if (suppressPrompt) context.showPrompt = false;

        // If criteria is cleared be sure to reset match style back to
        // initial LG.autoFetchTextMatchStyle.
        criteria = isc.DS.checkEmptyCriteria(criteria);
        if (criteria == null || isc.getKeys(criteria).length == 0) {
            // pick up textMatchStyle from LG.autoFetchTextMatchStyle
            context.textMatchStyle = this.sourceWidget.autoFetchTextMatchStyle;
        }

        // If we're going to hit the server, build in a pause so we don't keep diving off whilst
        // the user is typing rapidly
        var rs = this.sourceWidget.data;

        
        if (
            (isc.isA.ResultSet(rs) && rs.willFetchData(criteria, context.textMatchStyle)) ||
            (isc.isA.Tree(rs) && this.sourceWidget.dataSource != null)
            ) 
        {
            this.fireOnPause("performFilter", {
                target:this.sourceWidget, 
                methodName:"handleFilterEditorSubmit", 
                args:[criteria, context, callback]
            }, 
            this.fetchDelay);
        } else {
            this.sourceWidget.handleFilterEditorSubmit(criteria, context, callback);
        }

    },
    
    _getFilterCriteria : function (textMatchStyle) {
        var form = this.getEditForm();
        if (form == null) {
            return this._initialCriteria;
        }

        // live values from the edit row form:

        // Note that the 'return nulls' param ensures we get entries back for fields with
        // no value so we (correctly) wipe out the value in oldCriteria
        var crit = form.getValuesAsCriteria(null, textMatchStyle);
        var compressedCrit = isc.DS.compressNestedCriteria(crit);
        return compressedCrit;
   },
    

    // Add the record to the source widget's data object
    // This method is not completely functional yet - see comments within the method body
    performSave : function (suppressPrompt, callback) {
    
        // ensure we save the value from the current edit field before saving out the
        // entire set of values.
        var rowNum = this.getEditRow(),
            colNum = this.getEditCol(),
            fieldName = this.getFieldName(colNum),
            newValue = this._editRowForm.getValue(fieldName);
        this.setEditValue(rowNum, colNum, newValue);

        // validate the entire row, and save only if validation succeeds:
        var newValues = this.getEditValues(0),
            fields = this.getFields().getProperty(this.fieldIdProperty);

        // This method will show the validation errors, and put focus into the field for 
        // which the validation failed
        if (!this.validateRowValues(newValues, {}, 0, fields)) return;
        
        // xxx
        // At this point we want to fall through to 'saveEditedValues()', but we need to 
        // ensure that the saveEditedValues method on the sourceWidget is aware that we
        // are adding a record rather than overriding existing records.  
        // Currently this does not happen, so we will end up overriding existing records
//        targetList.saveEditedValues(
//            targetList.data.getLength(), 0, null, newValues, {}, isc.ListGrid.PROGRAMMATIC
//        );
  
        // clear out existing edit values, and call startEditing again to start a fresh edit      
        this._clearEditValues(0);
        this._startEditing(0,0);
        // This ensures the edit form values get cleared out.
        for (var fieldName in newValues) {
            this.refreshCell(0,colNum);
        }

        
        if (callback) this.fireCallback(callback);
    },
    
    // A method to get the current edit-values
    getValues : function () {
        var colNum = this.getEditCol(),
            fieldName = this.getEditFieldName();
        this.setEditValue(0, colNum, this._editRowForm.getValue(fieldName));

        
        //return isc.addProperties({},this.getEditValues(0));
        var criteria = this._getFilterCriteria();
        return criteria;
            
    },

    // A method to get the current edit-values as AdvancedCriteria
    getValuesAsCriteria : function (textMatchStyle) {
        var colNum = this.getEditCol(),
            fieldName = this.getEditFieldName();

        var criteria = this._getFilterCriteria(textMatchStyle);
        return criteria;
    },
    
    // Inline Editor overrides:

    // override canEditCell - if this is a filter we want to look at the static 'canFilter'
    // property on the field (inherited from the source widget).  Otherwise just fall through 
    // to the canEditCell implementation on the sourceWidget.
    canEditCell : function (rowNum, colNum) {
        if (this.isAFilterEditor()) {
            var field = this.getField(colNum);

            if (field == null) return false;
            return (field.canFilter != false);
        } else {
            return this.sourceWidget.canEditCell(rowNum, colNum);
        }
    },

    // Override the various methods to determine the edit form item properties.
    // If this is a filter edit row, we want to use the appropriate 'getFilterEditor...'
    // methods on the source widget -- if this is an editor, we simply inherit the 
    // 'getEditor...' methods from the source widget.
    
    getEditorValueMap : function (field, values) {
        if (this.isAFilterEditor()) {
            return this.sourceWidget.getFilterEditorValueMap(field);
        } else {
            return this.sourceWidget.getEditorValueMap(field, values);
        }
    },
    
    getEditorType : function (field, values) {
        if (this.isAFilterEditor()) {
            return this.sourceWidget.getFilterEditorType(field);
        } else {
            return this.sourceWidget.getEditorType(field, values);
        }
    },
    
    // function to re-use as change handler on all our items
    _editorChanged : function () {
        this.form.grid.editorChanged(this);
    },
    
    // Technically this fires on changed, not keypress but it's a cleaner name.
    editorChanged : function (item) {
        var actOnKeypress = item.actOnKeypress != null ? item.actOnKeypress : this.actOnKeypress;
        if (actOnKeypress) {
            // check whether the value of the item is a partially entered filterExpression
            if (this.sourceWidget.allowFilterExpressions && !item.enteredCompleteExpression()) return;
            // if this is a filterEditor, filters caused by keypress events should not select
            // the value on refocus, since subsequent keypresses would then clear the value
            if (this.isAFilterEditor()) this._editorCursorAtEnd = true;
            this.performAction(true);
        }
    },
    
    getEditorProperties : function (field) {
        // Default all items to match this.cellHeight
        var props = {height:this.cellHeight};
            
        if (this.isAFilterEditor()) {
            // For filter editors always allow empty values
            props.allowEmptyValue = true;
            
            // fire our special changed handler on 'changed'
            props.changed = this._editorChanged;  
            props.actOnKeypress = field.filterOnKeypress;
            
            return isc.addProperties(props, this.sourceWidget.getFieldFilterEditorProperties(field));
        } else {
            return isc.addProperties(props, this.sourceWidget.getEditorProperties(field));
        }
    },

    getEditItem : function (editField, record, editedRecord, rowNum, colNum, width) {
        var item = this.Super("getEditItem", arguments);
        if (!this.isAFilterEditor()) return item;

        item.getElementName = function () {
            return this.Super("getElementName", arguments) + "_search";
        };

        var filterEditorProps = this.sourceWidget.getFieldFilterEditorProperties(editField),
            undef;
        if (!filterEditorProps) filterEditorProps = {};

        // For filter editors, don't pick up editing related properties such as custom change 
        // handlers from the field. 
        // These are not appropriate on the filter editor since
        // we're not actually performing an edit of a record in the source widget.
        // Note that the developer can still customize these attributes via the
        // filterEditorProperties block for the field.
        // Note: delete rather than setting to explicit null - this ensures the item can pick up
        // the FormItem class default properties for these attributes
        if (editField.defaultValue!=null && filterEditorProps.defaultValue === undef) {
            delete item.defaultValue;
        }
        if (editField.change != null && filterEditorProps.change === undef) {
            delete item.change;
        }
        // changed already overridden to '_editorChanged'
        if (editField.defaultDynamicValue != null &&
            filterEditorProps.defaultDynamicValue != null) 
        {
            delete item.defaultDynamicValue;
        }
        if (editField.icons != null && filterEditorProps.icons === undef) {
            delete item.icons;
        }
        if (editField.showPickerIcon != null && filterEditorProps.showPickerIcon === undef) {
            delete item.showPickerIcon;
        }

        return item;
    },

    // override getDefaultEditValue - for filterEditors, our value will always be explicitly set,
    // either from defaultFilterValue on the source widget, or from specified criteria on a 
    // filter operation
    getDefaultEditValue : function (fieldName, field) {
        if (this.isAFilterEditor()) return null;
        return this.Super("getDefaultEditValue", arguments);
    },
    
    // override getValueIcon() - for canFilter:false fields we want to suppress whatever
    // icon would correspond to "null" from showing in lieu of an editor - simply
    // showing a blank cell is a better indication of non-editability.
    // This is most obvious with boolean fields.
    getValueIcon : function (field, value, record, rowNum) {
        if (this.isAFilterEditor()) return null;
        return this.Super("getValueIcon", arguments);
    },
    
    // cellEditEnd() is fired when the user completes an edit for some cell by tabbing out
    // of that cell, hitting enter or escape, etc.
    // We override the default implementation to avoid cancelling the edit, or saving the
    // edit values into this.values, and to allow us to fire our default action in response
    // to an enter keypress (or a field change).
    cellEditEnd : function (editCompletionEvent, newValue) {

        // Possible editCompletionEvents are:
        // - CLICK_OUTSIDE - suppressed as we don't show the clickMask for this ListGrid
        // - ESCAPE_KEYPRESS - ignore
        // - UP_ARROW_KEYPRESS - ignore
        // - DOWN_ARROW_KEYPRESS - ignore
        // - EDIT_ROW_CHANGE - will not happen as we don't have more than one row
        // - PROGRAMMATIC - ignore
        //
        // - ENTER_KEYPRESS - fire this.performAction - will perform a filter if this
        //                    is a filter editor (or a save if this is an editor)
        // - TAB_KEYPRESS - 
        // - SHIFT_TAB_KEYPRESS - 
        // - EDIT_FIELD_CHANGE - 
        //              For all field changes, save out the edit values, and move to the 
        //              appropriate fields.
        //              If this.actOnCellChange is true, also fire this.performAction().
        if (editCompletionEvent != isc.ListGrid.ENTER_KEYPRESS &&
            editCompletionEvent != isc.ListGrid.TAB_KEYPRESS && 
            editCompletionEvent != isc.ListGrid.SHIFT_TAB_KEYPRESS &&
            editCompletionEvent != isc.ListGrid.EDIT_FIELD_CHANGE) return true;

        var undef;
        if (newValue === undef) newValue = this.getUpdatedEditorValue();
            
        var rowNum = this.getEditRow(),
            colNum = this.getEditCol();

        // update the locally stored edit info with the new value for the appropriate field
        this.setEditValue(rowNum, colNum, newValue);

        // Save / query on enter keypress
        if (editCompletionEvent == isc.ListGrid.ENTER_KEYPRESS || this.actOnCellChange) {
            this.performAction();
            if (editCompletionEvent == isc.ListGrid.ENTER_KEYPRESS) return;            
        }
                
        var nextCell = this.getNextEditCell(rowNum, colNum, editCompletionEvent);
            
        if (nextCell == null || nextCell[0] != rowNum) {
            // if we're tabbing past the end of the row, we want to put focus onto the
            // appropriate object on the page.
            // We have to do this explicitly, because we've overridden the key press
            // method on the editor form items to suppress the native tab-behavior.
            
            if (this.tabIndex != null && !this._autoTabIndex) {
                this._letTabKeypressThrough = true;
                return;
            } else {
                if (editCompletionEvent == isc.ListGrid.TAB_KEYPRESS) {
                    this.body._focusInNextTabElement(true);
                } else {
                    // On shift tab, move focus to the previous widget in the tab order.
                    this._focusInNextTabElement(false);
                }
                return;
            }
        }
        
        // In this case we want to fall through to the superclass implementation - this
        // will validate the cell value (if necessary), save the editValue locally, and
        // move focus to the appropriate cell.
        // xxx - Could just re-implement the relevant code here rather than calling Super
        //       to avoid (EG) calling 'getNextEditCell()' more than once.
        return this.Super("cellEditEnd", arguments);
    },
    editorKeyPress : function (item, keyName, characterValue) {
        var rv = this.Super("editorKeyPress", arguments);
        if (this._letTabKeypressThrough) {
            this._letTabKeypressThrough = null;
            return true;
        }
        return rv;
    },
    
    // clearEditValue on the superclass will dismiss the editor if no editValues are left.  The
    // recordEditor never wants to do this.
    clearEditValue : function (editValuesID, fieldName) {
        return this.Super("clearEditValue", [editValuesID, fieldName, null, true]);
    },

    // UI Synching:

    // Ensure the action button stays positioned / sized correctly
    // Override layoutChildren to keep the action button positioned at the right edge of this
    // widget
    layoutChildren : function () {
        this.Super("layoutChildren", arguments);
        // Stick the action button to the left
        var sorterWidth = this.sourceWidget._getSorterWidth();
        if (this.actionButton) 
            this.actionButton.setLeft(this.isRTL() ? 0 : this.getInnerWidth() - sorterWidth);

        // Ensure the body does not extend behind the action button - this avoids issues
        // where we can't scroll the source widget horizontally to get at stuff under the
        // V-Scrollbar
        if (this.body) {
            var body = this.body;
            if (this.bodyLayout) body = this.bodyLayout;
            body.setWidth(Math.max(1, this.getInnerWidth() - sorterWidth));
            if (this.actionButton && this.isRTL()) {
                body.setLeft(sorterWidth);
            }
        }


        // layoutChildren sets up a layout's member tab-indices
        // Action button isn't a member, but slot it after the body.
        // Note that we explicitly force focus into it in response to cell-edit-end but this
        // allows tabbing backward through a page to work properly (even in the case where
        // we have a modal mask up and are doing synthetic tab-index mgmt)
        if (this.actionButton && this.body) this.actionButton._setTabAfter(this.body);
    },


    // Override adjustOverflow to render the actionButton as tall as this widget
    adjustOverflow : function () {
        this.Super("adjustOverflow", arguments);
        if (this.actionButton) {
            this.actionButton.setHeight(this.body.getVisibleHeight());
        }            
    },
    
    // We need to keep our UI in sync with our source widget.
    // This means:
    // - Show and Hide with source widget.
    //      Handled by standard peer relationship
    // - Move with source widget
    //      Handled by standard peer relationship
    // - Resize with source widget, and remain positioned below source widget
    //      Handled by ListGrid.resizePeersBy() method
    // - Scroll body with the source widget body
    //      RecordEditor bodyScrolled() overridden below to keep source widget body in sync
    //      with us.
    //      ListGrid bodyScrolled() method handles keeping us in sync with source widget body
    //      scroll position.
    // - Resize fields with the source widget
    //      Handled by source widget resizeField()
    // - Reorder / Change fields with the source widget   
    //      Handled by source widget reorderFields()

    // Override bodyScrolled() to keep the body scrolling in sync.
    // Note that bodyScrolled implementation on the source widget will handle scrolling this
    // widget into place.
    bodyScrolled : function (left, top) {
        this.Super("bodyScrolled", arguments);
        // Sanity check - avoid attempting to scroll while not actually drawn/visible
        if (!this.isVisible() || !this.isDrawn()) return;

        // Scroll the sourceWidget body to our position
        // Only do this if it's not already scrolled to the same position as us
        
        if (this._syncSourceEvent != null) {
            isc.Timer.clear(this._syncSourceEvent);
        }
        this._syncSourceEvent = this.delayCall("syncSourceScrolling", [], 0);
    },

    syncSourceScrolling : function () {
        var sourceWidget = this.sourceWidget,
            sourceWidgetBody;
        if (!sourceWidget || !(sourceWidgetBody = sourceWidget.body)) return;

        var left = this.body.getScrollLeft();

        // RTL mode - account for the fact that scrollLeft zero (i.e. hard left) on the source
        // widget body might not be scrollLeft zero (hard left) on the filter editor body since
        // their viewport left coords might not align. For example, if the source widget body
        // does not have a vertical scrollbar, then scrollLeft zero on the source widget body
        // corresponds to scrollLeft 16 (or whatever the filter button width is) on the filter
        // editor body.
        if (this.isRTL()) {
            var offset = sourceWidgetBody.getViewportWidth() - this.body.getViewportWidth();
            left -= offset;
            if (left < 0) left = 0;
        }

        var maxScrollLeft = sourceWidgetBody.getScrollRight();
        if (left > maxScrollLeft) left = maxScrollLeft;
        if (sourceWidgetBody.getScrollLeft() != left) {
            sourceWidgetBody.scrollTo(left);
        }
    },

    // override 'getFieldWidths' to get the source's field widths.
    
    getFieldWidths : function () {
        var widths = this.sourceWidget.getFieldWidths();
        // duplicate the widths so modifications on the LG widths array won't directly affect
        // our widths
        if (isc.isA.Array(widths)) {
            widths = widths.duplicate();

            if (this._correctForActionButtonClipping()) {
                var sorterWidth = this.sourceWidget._getSorterWidth();
                var taken = widths.sum(),
                    space = this.getInnerWidth() - sorterWidth;

                if (taken > space) {
                    widths[widths.length-1] -= Math.min(sorterWidth, (taken - space));
                }
            }
        }
        return widths;
    },
    // getEditFormItemFieldWidths():
    // Overridden to reduce the size of the last edit item to ensure it fits in the available
    // space (isn't clipped by the filter button), even if the sourceWidget's field is sized larger
    // than the available space
    // Happens when the source widget shows an HScrollbar but no VScrollbar
    _correctForActionButtonClipping : function () {
        var sourceWidget = this.sourceWidget;
        if (sourceWidget != null && !sourceWidget._showSortButton() && sourceWidget.body != null &&
            !sourceWidget.body.vscrollOn)
        {
            
            return true;
        }
    },
    getEditFormItemFieldWidths : function (record) {
        var widths = this.Super("getEditFormItemFieldWidths", arguments);
        if (this._correctForActionButtonClipping()) {
            var sorterWidth = this.sourceWidget._getSorterWidth();
            var taken = widths.sum(),
                space = this.getInnerWidth() - sorterWidth;

            if (taken > space) {
                widths[widths.length-1] -= Math.min(sorterWidth, (taken - space));
            }
        }
        return widths;
    },

    // override getCellValue to avoid showing checkbox icons for the checkboxField
    
    getCellValue : function (record, rowNum, colNum, gridBody) {
        var field = this.fields[colNum];
        if (field && 
            (this.isCheckboxField(field) || this.isExpansionField(field) ||
                this.isRowNumberField(field))) 
            return "&nbsp;"
        var value = this.Super("getCellValue", arguments);
        if (colNum == this.fields.length - 1) {
            if (isc.Browser._needOldFilterButtonOffsetApproach) {
                var field = this.getField(colNum),
                    form = this.getEditForm(),
                    item = form != null ? form.getItem(field.name) : null,
                    width = item != null ? item.getWidth() : null;
                if (width != null) {
                    value = ("<div style='text-align:" +
                             this.getCellAlign(record, rowNum ,colNum, true) +
                             ";width:" + width + "px;'>" + value + "</div>");
                }
            }
        }
        return value;
    },

    // Override rebuildForFreeze to no-op
    // We'll instead respond to our source-element's rebuild for freeze
    rebuildForFreeze : isc.Class.NO_OP,

    // Override rowClick / rowDoubleClick to no-op
    // This means if we have any fields which are canFilter:false, we'll avoid executing the
    // field level recordClick / recordDoubleClick handlers when the user clicks them.
    rowClick : isc.Class.NO_OP,
    rowDoubleClick : isc.Class.NO_OP
});

//>IE7
isc.Browser._needOldFilterButtonOffsetApproach = isc.Browser.isIE && !isc.Browser.isIE8Strict;
//<IE7


if (isc.Browser._needOldFilterButtonOffsetApproach) {
    isc.RecordEditor.addProperties({
        getCellAlign : function (record, rowNum, colNum, returnDefault) {
            if (!returnDefault && colNum == this.fields.length - 1) {
                return this.isRTL() ? "right" : "left";
            }
            return this.Super("getCellAlign", arguments);
        }
    });
}
//!<Deferred
