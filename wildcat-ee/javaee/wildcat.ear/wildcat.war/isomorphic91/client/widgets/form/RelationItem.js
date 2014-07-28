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

 

// Class will not work without the ListGrid
if (isc.ListGrid) {



//>	@class RelationItem
//
// Enables editing and saving of records related to the one being displayed in the "master" form
// (the form containing this item).
//
// @treeLocation Client Reference/Forms/Form Items
// @visibility experimental
//<
isc.ClassFactory.defineClass("RelationItem", "CanvasItem");
isc.RelationItem.addProperties({

    canvasConstructor: "ListGrid",
    canvasDefaults: {
        canEdit: true
    },

    pickerConstructor: "RelationPicker",
    showEditButton: true,
    editButtonDefaults: {
        click: "item.showPicker(item.showPickerModal(), icon)",
        prompt: "Edit new/selected item"
    },

    showPickerModal : function () {
        return this.form && !this.form.saveOperationIsAdd();
    },

    showRemoveButton: true,
    removeButtonDefaults: {
        src: "[SKIN]DynamicForm/Remove_icon.gif",
        click: "item.removeSelectedData()",
        prompt: "Remove selected item"
    },

    canEditWithNoMasterRecord: false
});

//!>Deferred
isc.RelationItem.addMethods({

init : function () {
    this.hasMasterRecord = false;

    this.Super("init", arguments);

    // if we're showing the various buttons, add them now
    if (this.showEditButton) this.editButton = this.addIcon(this.editButtonDefaults);
    if (this.showRemoveButton) this.removeButton = this.addIcon(this.removeButtonDefaults);
},

getPickerData : function () {
    // return the first selected record, if available;
    var selectedRecord = this.canvas.getSelectedRecord();
    if (selectedRecord) return selectedRecord;
    
    // new record
    return this.getDataSource().getForeignKeysByRelation(this.form.getValues(), this.form.dataSource);
},

showPicker : function (modal, icon, pickerProperties, rect) {
    if (pickerProperties == null) pickerProperties = {};
    pickerProperties.dataSource = this.dataSource;
    
    this.Super("showPicker", [modal, icon, pickerProperties, rect], arguments);

    // propagate the masterRecord at show() time in case the use saves directly out of the picker.
    var foreignKeyValues = {};
    if (!this.form.saveOperationIsAdd())
        foreignKeyValues = this.getDataSource().getForeignKeysByRelation(this.form.getValues(), this.form.dataSource);
    this.picker.setForeignKeyValues(foreignKeyValues);
},

// XXX (why) do we need this?
getValue : function () {
    return;
},

removeSelectedData : function () {
    this.canvas.removeSelectedData();
},

// A setValue means that a new master record has been selected.
setValue : function () {       
    // use the primary key to issue a re-filter.  Must do this on a timeout because we're in the
    // middle of setValues() and will need to call getValues() on the DF.
    this.delayCall("filterRelation");
},

filterRelation : function () {
    var values = this.form.getValues();
    var wasDisabled = this.isDisabled();

    if (this.form.saveOperationIsAdd()) {
        // the record doesn't have values for all primary key fields - we're adding a new record,
        // so set the data to an empty array because no relations exist yet for this record.
        this.canvas.setData([]);
        this.hasMasterRecord = false;
    } else {
        // we're editing an existing record (has primary keys), so filter the relations view by the
        // primaryKeys
        var ds = this.getDataSource();
        if (ds) {
            this.canvas.filterData(ds.getForeignKeysByRelation(values, this.form.dataSource));
            this.hasMasterRecord = true;
        }
    }
    if (wasDisabled != this.isDisabled()) this.updateDisabled();

    if (this.picker) this.picker.clearData();
},

isDisabled : function () {
    var dis = this.Super("isDisabled", arguments);
    if (dis) return true;
    if (this.canEditWithNoMasterRecord) return false;
    return !!this.hasMasterRecord;
},

_shouldAllowExpressions : function () {
    return false;
}

});
//!<Deferred

isc.defineClass("RelationPicker", "VLayout").addProperties({
    className: "dialogBackground"
});

//!>Deferred
isc.RelationPicker.addMethods({

creatorName:"picker", 
initWidget : function () {
    this.Super("initWidget", arguments);
    
    this.addAutoChild("editor", { dataSource: this.dataSource }, "DynamicForm");

    this.addAutoChild("toolbar", {
        membersMargin: 2
    }, "HLayout");
    this.addAutoChild("saveButton", {
        title: "Save", 
        click: "this.picker.editor.saveData(this.picker.getID()+'.hide()')"        
    }, "AutoFitButton", this.toolbar);
    this.addAutoChild("clearButton", {
        title: "Clear", 
        click: "this.picker.clearData();"
    }, "AutoFitButton", this.toolbar);
    this.addAutoChild("cancelButton", {
        title: "Cancel", 
        click: "this.picker.hide();this.picker.clearData()"
    }, "AutoFitButton", this.toolbar);

},

hide : function () {
    this.Super("hide", arguments);
    this.hideClickMask();
},

setData : function (data) { 
    this.editor.setData(data);
},

getData : function () {
    return this.editor.getValues();
},

clearData : function () {
    this.editor.clearValues();
    this.setData(this.foreignKeyValues);
},

dataChanged : function () {

},

setForeignKeyValues : function (values) {
    this.foreignKeyValues = values;
}

});
//!<Deferred

}
