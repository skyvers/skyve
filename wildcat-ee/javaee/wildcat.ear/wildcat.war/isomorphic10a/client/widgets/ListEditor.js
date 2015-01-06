/*
 * Isomorphic SmartClient
 * Version v10.0p_2015-01-04 (2015-01-04)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */


// avoid errors if DataBinding module is loaded without either Grids or Forms
if (isc.ListGrid && isc.DynamicForm) {



//>	@class ListEditor
//
// @implements DataBoundComponent	
// @visibility listEditor
//<
isc.defineClass("ListEditor", isc.Layout);
    
isc.ListEditor.addProperties({
    //> @attr listEditor.inlineEdit (boolean : false : IR)
    // Whether to allow inline editing in the grid.  
    // <P>
    // If enabled, the form will show as a modal dialog instead of being displayed side by side
    // with the grid, to prevent ambiguous simultaneous editing within both the grid and form.
    // This mode is suitable for rapid entry/update of records where few properties are
    // normally set.
    //
    // @visibility listEditor
    //<
    //inlineEdit :false,

    //> @attr listEditor.vertical (boolean : false : IR)
    // Whether the form and grid should be shown vertically stacked or horizontally adjacent.
    // <P>
    // Ignored when +link{inlineEdit} is true, since form is then shown in a pop-up.
    //
    // @visibility listEditor
    //< 
    vertical:false,

    // Grid subcomponent
    // ---------------------------------------------------------------------------------------
	gridConstructor: isc.ListGrid,
    gridDefaults:{
        editEvent:"click",
        listEndEditAction:"next",
        autoParent:"gridLayout",
        selectionType:isc.Selection.SINGLE,
        recordClick:"this.creator.recordClick(record)",
        editorEnter:"if (this.creator.moreButton) this.creator.moreButton.enable()",
        selectionChanged: function() {
            if (this.anySelected() && this.creator.moreButton) {
                this.creator.moreButton.enable();
            }
        },
        contextMenu : {
            data : [
                {title:"Remove", click: "target.creator.removeRecord()" }
            ]
        }
    },

    // List Buttons
    // ---------------------------------------------------------------------------------------
    gridButtonsDefaults:{
        _constructor:isc.HLayout,
        autoParent:"gridLayout",
        height:10, width:10, layoutMargin:6, membersMargin:10,
        overflow:isc.Canvas.VISIBLE
    },

    newButtonTitle:"New",
    newButtonDefaults:{
        _constructor:isc.AutoFitButton,
        autoParent:"gridButtons",
        click:"this.creator.newRecord()"
    },

    moreButtonTitle:"More..",
    moreButtonDefaults:{
        _constructor:isc.AutoFitButton,
        autoParent:"gridButtons",
        click:"this.creator.editMore()",
        disabled:true
    },

    removeButtonTitle:"Remove",
    removeButtonDefaults:{
        _constructor:isc.AutoFitButton,
        autoParent:"gridButtons",
        click:"this.creator.removeRecord()"
    },

    // Form subcomponent
    // ---------------------------------------------------------------------------------------
    formDefaults:{
        _constructor:isc.DynamicForm,
        autoParent:"formLayout",
        overflow:isc.Canvas.AUTO
    },

    // Form Buttons
    // ---------------------------------------------------------------------------------------
    formButtonsDefaults:{
        _constructor:isc.HLayout,
        autoParent:"formLayout",
        height:10, width:10, layoutMargin:6, membersMargin:10,
        overflow:isc.Canvas.VISIBLE
    },

    saveButtonTitle:"Save",
    saveButtonDefaults:{
        _constructor:isc.AutoFitButton,
        autoParent:"formButtons",
        click:"this.creator.saveRecord();"
    },

    cancelButtonTitle:"Cancel",
    cancelButtonDefaults:{
        _constructor:isc.AutoFitButton,
        autoParent:"formButtons",
        click:"this.creator.cancelChanges()"
    },

    resetButtonTitle:"Reset",
    resetButtonDefaults:{
        _constructor:isc.AutoFitButton,
        autoParent:"formButtons",
        click:"this.creator.form.resetValues()"
    },

    // Sublayouts
    // ---------------------------------------------------------------------------------------
    gridLayoutDefaults : {
        _constructor:isc.VLayout
    },

    gridButtonsOrientation:"left",

    formLayoutDefaults : {
        _constructor:isc.VLayout,
        autoFocus:true
    },

    animateMembers:true,
    membersMargin:10,

    // Changes dialog
    // ---------------------------------------------------------------------------------------
    confirmLoseChangesMessage:"Discard changes?", 


    // AutoChildren
    // ---------------------------------------------------------------------------------------
    draw : function () {
        if (isc._traceMarkers) arguments.__this = this;

    	if (!this.readyToDraw()) return this;

        return this.Super("draw", arguments);
    },
    
    initWidget : function () {
        this.Super("initWidget", arguments);
        // don't show the edit button by default if we're allowing inline editing, since
        // just clicking triggers editing
        if (!this.inlineEdit) this.showMoreButton = this.showMoreButton || false;

        this.addAutoChild("gridLayout");
		this.addAutoChild("grid", { _constructor: this.gridConstructor } );
        this.addAutoChildren(this.gridButtonsGroup);
        this.addAutoChildren(this.formGroup);
        
    },

    formGroup : [
        "formLayout", "form", "formButtons", "saveButton", "cancelButton", "resetButton"
    ],
    gridButtonsGroup : [
        "gridButtons", "newButton", "moreButton"
    ],

    configureAutoChild : function (child, childName) {
        if (isc.isA.Button(child)) child.title = this[childName + "Title"];

        if (child == this.grid) {
            child.dataSource = this.dataSource;
            child.fields = this.fields;
            child.saveLocally = this.saveLocally;
            child.canEdit = this.inlineEdit;
        }

        if (this.gridButtonsOrientation == isc.Canvas.RIGHT) {
            // place buttons to right of list
            if (child == this.gridLayout) child.vertical = false;
            if (child == this.formLayout) child.vertical = false;
            // stack buttons vertically
            if (child == this.gridButtons) child.vertical = true;
            if (child == this.formButtons) child.vertical = true;
        }

        if (child == this.form) {
            child.dataSource = this.dataSource;
            child.fields = this.formFields;
        }
        if (this.inlineEdit) {
            if (child == this.formLayout) child.visibility = isc.Canvas.HIDDEN;
        } else {
            if (child == this.gridLayout) child.showResizeBar = true;
        }
    },

    // DataSources and Data
    // ---------------------------------------------------------------------------------------

    setDataSource : function (dataSource, fields) {
        this.dataSource = dataSource || this.dataSource;
        if (this.grid != null) {
            this.grid.setDataSource(dataSource, fields);
            this.form.setDataSource(dataSource, fields);
        }
    },
    setData : function (data) {

        if (data == null) data = [];

        if (data.dataSource) this.setDataSource(data.dataSource);
        if (this.grid != null) {
            this.grid.setData(data);
            this.form.clearValues();
        } else {
            isc.addProperties(this.gridDefaults, this.gridProperties || {}, {data:data});
        }
    },
    getData : function () {
        // on a getData call, always save the current edit to the dataSet before returning
        if (this.inlineEdit) this.grid.endEditing();
        return this.grid.getData();
    },

    // Button / Menu actions
    // ---------------------------------------------------------------------------------------
    
    // cancel button while editing in form
    cancelChanges : function () {
        this.form.clearValues();
        this.showList();
    },

    showList : function () {
        if (this.inlineEdit) {
            this.formLayout.animateHide({effect:"wipe", startFrom:"R"});
            this.gridLayout.animateShow({effect:"wipe", startFrom:"R"});
        }
    },
    showForm : function () {
        if (this.inlineEdit) {
            this.gridLayout.animateHide({effect:"wipe", startFrom:"R"});
            this.formLayout.animateShow({effect:"wipe", startFrom:"R"});
        }
    },

    // edit via form on recordClick in the list, unless inline editing is allowed
    recordClick : function (record) {
        if (this.inlineEdit) return;
        
        
        var _this = this;
        
        var proceed  = function (ok) {
            if (ok) {
                _this.currentRecord = record;
                if (!_this.inlineEdit) _this.form.editRecord(record);
                _this.form.setValues(isc.addProperties({}, _this.grid.getSelectedRecord()));
            }
        }

        // editing in parallel form: if there are changes, pop up a warning that this will
        // abandon changes to the currently edited item
        if (!this.form.valuesHaveChanged()) proceed(true);
        else this.confirmLoseChanges(proceed);
    },

    getEditRecord : function () {
        var editRowNum = this.grid.getEditRow();
        if (editRowNum != null) {
            return this.grid.getEditedRecord(editRowNum);
        } else {
            return isc.addProperties({}, this.grid.getSelectedRecord());
        }
    },

    // More... button, inlineEdit only
    editMore : function () {
        this.currentRecord = this.getEditRecord();
        this.showForm();
        this.form.setValues(this.currentRecord);
    },

    newRecord : function () {
        if (this.inlineEdit) return this.grid.startEditingNew();
        
        var _this = this;

        var proceed = function (ok) {
            if (ok) {
                _this.grid.deselectAllRecords();
                _this.showForm();
                _this.form.editNewRecord();
            }
        }

        // editing in parallel form: if there are changes, pop up a warning that this will
        // abandon changes to the currently edited item
        if (!this.form.valuesHaveChanged()) proceed(true);
        else this.confirmLoseChanges(proceed);
    },

    // remove record context click
    removeRecord : function () {
        this.form.clearValues();
        this.grid.removeSelectedData();
    },
    saveRecord : function () {
        if (!this.form.validate()) return false;
        var values = this.form.getValues();  
    
        this.showList(); 

        if (this.form.saveOperationType == "add") { // new record
            this.grid.addData(values);
        } else {
            // if inline editing is occurring just apply the values as editValues.  We can't
            // count on updateData() because we may have edited a new row that doesn't have a
            // primary key
            if (this.inlineEdit && this.grid.getEditRow() != null) {
                var rowNum =this.grid.getEditRow();
                
                if (this.grid.data[rowNum] != null) this.grid.updateData(values)
                else this.grid.setEditValues(rowNum, values);
            } else {
                this.grid.updateData(values);
            }
        }
        
        return true;
    },

    confirmLoseChanges : function (callback) {
        isc.confirm(this.confirmLoseChangesMessage, callback);
    },

    //
    // ---------------------------------------------------------------------------------------
    validate : function () { 
        if (this.form.isVisible() && this.form.valuesHaveChanged()) {
            return this.form.validate(); 
        }
        return true;
    }
});


} // end if (isc.ListGrid && isc.DynamicForm)
