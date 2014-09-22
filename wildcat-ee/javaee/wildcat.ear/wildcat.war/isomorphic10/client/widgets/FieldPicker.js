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



            
//////////////////////////////////////////////////////////////////////////////
// Class FieldPickerField

//>	@class FieldPickerField
// Class containing properties that configure the display of
// and interaction with the fields showing in a +link{FieldPicker}.
//
// @treeLocation Client Reference/Data Binding/FieldPicker
// @visibility external    
//<
isc.ClassFactory.defineClass("FieldPickerField");

isc.FieldPickerField.addClassProperties({

//> @classAttr fieldPickerField.frozenTitle (String : "Is Frozen" : IR)
// The title displayed for the frozen property
// @group i18nMessages
// @visibility external
//<
frozenTitle: "Is Frozen",

//> @classAttr fieldPickerField.precisionTitle (String : "Precision" : IR)
// The title displayed for the precision property
// @group i18nMessages
// @visibility external
//<
precisionTitle: "Precision",

//> @classAttr fieldPickerField.decimalPrecisionTitle (String : "Decimal Precision" : IR)
// The title displayed for the decimalPrecision property
// @group i18nMessages
// @visibility external
//<
decimalPrecisionTitle: "Decimal Precision",

//> @classAttr fieldPickerField.decimalPadTitle (String : "Decimal Pad" : IR)
// The title displayed for the decimalPad property
// @group i18nMessages
// @visibility external
//<
decimalPadTitle: "Decimal Pad",

//> @classAttr fieldPickerField.alignTitle (String : "Header Text Alignment" : IR)
// The title displayed for the align property
// @group i18nMessages
// @visibility external
//<
alignTitle: "Header Text Alignment",

//> @classAttr fieldPickerField.cellAlignTitle (String : "Data Alignment" : IR)
// The title displayed for the cellAlign property
// @group i18nMessages
// @visibility external
//<
cellAlignTitle: "Data Alignment"

});

//////////////////////////////////////////////////////////////////////////////
// Interface FieldPickerGrid



isc.ClassFactory.defineInterface("FieldPickerGrid").addInterfaceProperties({

// filtering

clearCriteriaIfPresent : function () {
    if (!isc.isA.emptyObject(this.getCriteria())) {
        if (this.willFetchData(null, "substring")) {
            this.logWarn("ListGrid.willFetchData() has returned true unexpectedly " +
                         "while trying to clear the filter criteria prior to save.");
        }
        this.filterData();
    }
},

// record expansion/collapse

getExpansionComponent : function (record) {
    var view = this.parentElement,
        changedRecords = view.shuttle.changedRecords,
        changedRecord = changedRecords[record.name];

    if (changedRecord) {
        record = isc.shallowClone(record);
        isc.addProperties(record, changedRecord);
    }
    
    return view.createExpansionDynamicForm(this.editableProperties, record);
},

saveAndCollapseRecord : function (member, component, record) {

    var view = this.parentElement,
        values = member.getChangedValues(),
        changedRecords = view.shuttle.changedRecords;

    if (changedRecords[record.name]) isc.addProperties(changedRecords[record.name], values);
    else changedRecords[record.name] = values;

    this._collapseRecord(record, component);
},

setCanExpandRecords : function (canExpand) {
    var recalculate = canExpand && !this.canExpand;

    this.Super("setCanExpandRecords", arguments);

    
    if (recalculate) this.setRecordExpansion(0, this.getTotalRows());
},

setRecordExpansion : function (startRow, endRow) {

    var nFloatApplicableProperties = 0,
        nNumberApplicableProperties = 0,
        nAllTypeApplicableProperties = 0;

    var shuttle = this.parentElement.shuttle,
        dataSource = shuttle.picker.expansionDataSource,
        fieldNames = dataSource.getFieldNames(),
        fields = dataSource.getFields();

    // build a set of all properties and non-float properteis

    for (var i = 0; i < fieldNames.length; i++ ) {
        var name = fieldNames[i];
        if (this.editableProperties.contains(name)) {
            switch (name) {
            default:
                nAllTypeApplicableProperties++;
                // fall through - if applicable to all types it's also applicable to a number
            case "precision":
                nNumberApplicableProperties++;
                // fall through - if applicable to a number it's also applicable to a float
            case "decimalPad":
            case "decimalPrecision":
                nFloatApplicableProperties++;
                break;
            }
        }
    }

    // now, using those property sets, sweep through records

    var dirty = false,
        canExpandProperty = this.canExpandRecordProperty;
    
    for (var i = startRow; i < endRow; i++) {
        var expand = true, record = this.getFieldRecord(i);

        // don't disable expansion if userSummary/userFormula are present;
        // buttons will be present to allow editing using Builder
        if (!record.userSummary && !record.userFormula) {
            if (isc.SimpleType.inheritsFrom(record.type, "float")) {
                expand = nFloatApplicableProperties > 0;
            } else if (isc.SimpleType.inheritsFrom(record.type, "integer")) {
                expand = nNumberApplicableProperties > 0;
            } else {
                expand = nAllTypeApplicableProperties > 0;
            }
        }

        // redraw grid if at least one record's expansion property changed
        if (record[canExpandProperty] == null || record[canExpandProperty] == true) {
            if (!expand) { record[canExpandProperty] = false; dirty = true; }
        } else {
            if ( expand) { delete record[canExpandProperty];  dirty = true; }
        }
    }

    if (dirty) this.markForRedraw();
},

// force expanded records closed when they leave the FieldPickerGrid

collapseRecordList : function (recordList, revertTitle) {
    for (var i = 0; i < recordList.length; i++) {
        var record = recordList[i];
        if (this.isExpanded(record)) this.collapseRecord(record);
        // revert any title change in the record
        if (revertTitle) record._title = record.title;
    }
},

collapseAllRecords : isc.ClassFactory.TARGET_IMPLEMENTS,

filterData : function () {
    this.collapseAllRecords();
    return this.Super("filterData", arguments);
},

// support validation of changes to the records (DataBoundComponent Fields)

expandRecord : function (record) {
    this.Super("expandRecord", arguments);
    var form = this.getCurrentExpansionComponent(record);
    if (form != null) form.validate();
},

validateFieldRecord : function (record) {

    // if record is already expanded, simply use the DynamicForm validators to validate
    if (this.isExpanded(record)) return this.getCurrentExpansionComponent(record).validate();

    // otherwise, our cached values must be validated
    var shuttle = this.parentElement.shuttle,
        properties = shuttle.changedRecords[record.name];

    if (!properties) return true;

    return shuttle.picker.validateDataSourceField(properties);
},

validateRecordList : function (recordList) {

    if (!this.canExpandRecords) return true;
    var canExpandProperty = this.canExpandRecordProperty;

    for (var i = 0; i < recordList.length; i++) {
        var record = recordList[i];
        if (record[canExpandProperty] != false && !this.validateFieldRecord(record)) {
            // if not already expanded, expand offending record
            if (!this.isExpanded(record)) {
                this.expandRecord(record);
                this.getCurrentExpansionComponent(record).validate();
            }
            // always make the offending record clearly visible
            this.scrollToRow(this.getRecordIndex(record));
            return false;
        }
    }
    return true;
},

validateAllRecords : isc.ClassFactory.TARGET_IMPLEMENTS,

// support for canHide and canReorder ListGridField properties

_filterDropRecords : function (records, crossDrop, resultOnly) {
    var excluded = [],
        component = this.creator.dataBoundComponent,
        canReorder = !isc.isA.ListGrid(component) || component.canReorderFields;

    for (var i = 0; i < records.length; i++) {
        var record = records[i];
        if (crossDrop) {
            if (record.canHide == false) excluded.add(record);            
        } else {
            if (record.canReorder == null && !canReorder ||
                record.canReorder == false) excluded.add(record);
        }
    }

    // return true if and only if there are still records to drop
    if (resultOnly) return records.length > excluded.length;
    else {
        if (excluded.length > 0) records.removeList(excluded);
        return records.length > 0;
    }
},

willAcceptDrop : function () {
    var EH = this.ns.EH,
        result = this.Super("willAcceptDrop");
    if (!result) return false;
    return this._filterDropRecords(EH.dragTarget.getDragData(), 
                                   EH.dragTarget != this, true);
},

_syntheticTransferDone : function () {},

// Show hovers based on field.prompt if possible
canHover:true,
showHover:true,
cellHoverHTML : function (record, rowNum, colNum) {
    if (!this.creator.showFieldPrompts) return null;
    if (record && record.prompt) return record.prompt;
    return null;
},

// shifting records up and down (current grid only)

getContiguousSelection : function () {
    var selection = this.getSelectedRecords(),
        selectionLength = (selection == null ? 0 : selection.length);
    if (selectionLength == 0) return null;

    // we allow multiple selections only for contiguous records
    var firstIndex = this.getRecordSetBounds(selection, -1);

    var deselected = [];
    for (var i = 1; i < selectionLength; ++i) {
        var record = selection[i];
        if (deselected.length == 0 && this.getRecord(firstIndex + i) != record ||
           deselected.length > 0) deselected.add(record);
    }
    this.deselectRecords(deselected);

    return this.getSelectedRecords();
},

getRecordSetBounds : function (records, direction, offset) {
    if (offset == null) offset = 0;
    var nRows = this.getTotalRows();
    switch (direction) {
    case -1:
        var index = this.getRecordIndex(records.first());
        if (index >= 0) {
            return Math.max(index - offset, 0);
        }
        break;
    case 1:
        var index = this.getRecordIndex(records.last());
        if (index >= 0) {
            return Math.min(index + offset + 1, nRows);
        }
        break;        
    }
    return null;
},

getCellValue : function (record, recordNum, fieldNum) {
    var result = this.Super("getCellValue", arguments);

    // return the superclass result unless field is title field and no title is defined
    var field = this.getField(fieldNum);
    if (field.name != "_title" || result != this.emptyCellValue) return result;

    // call +link{dataBoundComponent.getFieldTitle()} if available for the title
    var component = this.creator.dataBoundComponent;
    if (isc.isA.Function(component.getFieldTitle)) {
        var componentField = component.getField(record.name);
        if (componentField) return component.getFieldTitle(componentField);
    }

    // as fallback use field name
    return record.name;
}
    
});

//////////////////////////////////////////////////////////////////////////////
// Class FieldPickerListGrid

isc.ClassFactory.defineClass("FieldPickerListGrid", "ListGrid", "FieldPickerGrid");

isc.FieldPickerListGrid.addMethods({

// access to the field records

getFieldRecord :       isc.ListGrid.getPrototype().getRecord,
getTotalFieldRecords : isc.ListGrid.getPrototype().getTotalRows,
clearFieldRecords : function () {},

// expansion

dataArrived : function (startRow, endRow) {
    if (this.canExpandRecords && startRow < endRow) this.setRecordExpansion(startRow, endRow);
},
    
collapseAllRecords : function () {
    if (this.canExpandRecords) {
        var recordList = this.data.getAllRows();
        this.collapseRecordList(recordList);
    }
},

validateAllRecords : function () {
    if (this.canExpandRecords) {
        var recordList = this.data.getAllRows();
        return this.validateRecordList(recordList);
    }
    return true;    
},

// support for autochaining an in-widget reorder for between widget drags


requestSyntheticReorder : function (dropRecords, targetRecord) {
    // abort reorder if another in progress or not allowed
    if (this._reorderRequest || !this.canReorderRecords) return;
    // listen for completion call
    if (!this.isObserving(this.data, "handleUpdate")) {
        this.observe(this.data, "handleUpdate", "observer.applySyntheticReorder(arguments[1])");
    }
    // set up the reorder request
    var i, keys = {};
    for (i = 0; i < dropRecords.length; i++) keys[dropRecords[i].name] = true;
    this._reorderRequest = {
        target: targetRecord.name,
        total: dropRecords.length,
        arrived: 0,
        keys: keys
    };
},

applySyntheticReorder : function (newRows) {
    var request = this._reorderRequest;

    // if we shouldn't be here, make sure we don't show up again
    if (!request) { this.ignore(this.data, "handleUpdate"); return; }
    // ensure newRows is a list for standard processing
	if (!isc.isAn.Array(newRows)) newRows = [newRows];
    if (newRows == null) return;

    // wait for transferred records to arrive
    for (var i = 0; i < newRows.length; i++) {
        if (request.keys[newRows[i].name]) request.arrived++;
    }
    if (request.arrived != request.total) return;

    // if all records are present, launch reorder
    if (!this.data.lengthIsKnown()) return;
    var records = [],
        length = this.data.getLength(),
        startPos = Math.max(0, length - request.total),
        index = this.data.findIndex("name", request.target);
    // due to filter, records may be hidden
    for (var i = startPos; i < length; i++) {
        var record = this.data.get(i) || {};
        if (request.keys[record.name]) records.add(record);
    }

    this._reorderRequest = null;
    this.ignore(this.data, "handleUpdate");
    this.delayCall("_finishApplySyntheticReorder", [request, records, index]);
},

_finishApplySyntheticReorder : function (request, records, index) {
    this.transferRecords(records, request.target, index, this, this._syntheticTransferDone);
},

// detection of changes for save prompt

dragComplete: function () { 
    var view = this.parentElement;
    view.shuttle.markForSave();
},

transferRecords : function (dropRecords, targetRecord, index, sourceWidget, callback) {

    var view      = this.parentElement,
        crossDrop = this != sourceWidget,
        synthetic = callback == this._syntheticTransferDone;
    
    if (!synthetic) this._filterDropRecords(dropRecords, crossDrop);

    
    if (crossDrop && !sourceWidget.validateRecordList(dropRecords)) dropRecords.clear();

    if (dropRecords.length > 0) view.shuttle.markForSave();

    // collapse any open records before starting the drop
    if (crossDrop) {
        sourceWidget.collapseRecordList(dropRecords, true);
    }

    // autochain an in-widget reorder for between widget drags
    if (crossDrop && targetRecord != null) {
        this.requestSyntheticReorder(dropRecords, targetRecord);
    }

    return this.Super("transferRecords", arguments);
},

// overrides from FieldPickerGrid

filterData :            isc.FieldPickerGrid.getPrototype().filterData,
expandRecord :          isc.FieldPickerGrid.getPrototype().expandRecord,
willAcceptDrop:         isc.FieldPickerGrid.getPrototype().willAcceptDrop,
setCanExpandRecords :   isc.FieldPickerGrid.getPrototype().setCanExpandRecords,
getExpansionComponent : isc.FieldPickerGrid.getPrototype().getExpansionComponent,
saveAndCollapseRecord : isc.FieldPickerGrid.getPrototype().saveAndCollapseRecord,
cellHoverHTML :         isc.FieldPickerGrid.getPrototype().cellHoverHTML,
getCellValue :          isc.FieldPickerGrid.getPrototype().getCellValue

});

//////////////////////////////////////////////////////////////////////////////
// Class FieldPickerTreeGrid

isc.ClassFactory.defineClass("FieldPickerTreeGrid", "TreeGrid", "FieldPickerGrid");

isc.FieldPickerTreeGrid.addMethods({

createResultTree : function (criteria, callback, requestProperties) {
    var picker = this.parentElement.shuttle.picker;
    if (!requestProperties.dataProperties) requestProperties.dataProperties = {};
    isc.addProperties(requestProperties.dataProperties,{
        modelType: "parent",
        defaultIsFolder: true,
        reportCollisions: false,
        idField: picker.pickerIdField,
        parentIdField: "_" + picker.pickerParentIdField
    });

    var tree = this.Super("createResultTree", arguments);

    
    if (this.isObserving(this.data, "dataSourceDataChanged")) {
        this.ignore(this.data,"dataSourceDataChanged");
    }
    this.observe(tree, "dataSourceDataChanged", "observer.invalidateCacheIfMissingNodes()");

    return tree;
},

// access to the field records

getTotalFieldRecords : function () { 
    this._nodeRecords = this.data.getDescendants();
    return this._nodeRecords.length;
},

getFieldRecord : function (rowNum) {
    return this._nodeRecords[rowNum];
},

clearFieldRecords : function () {
    delete this._nodeRecords;
},

// expansion

dataArrived : function (parentNode) {
    if (this.canExpandRecords && isc.isA.Object(parentNode)) {
        this._nodeRecords = this.data.getChildren(parentNode);
        this.setRecordExpansion(0, this._nodeRecords.getLength());
        delete this._nodeRecords;
    }
},
    
collapseAllRecords : function () {
    if (this.canExpandRecords) {
        var nodeList = this.data.getDescendants();
        this.collapseRecordList(nodeList);
    }
},

validateAllRecords : function () {
    if (this.canExpandRecords) {
        var nodeList = this.data.getDescendants();
        return this.validateRecordList(nodeList);
    }
    return true;    
},

// detection of changes for save prompt

dragComplete: function () { 
    var view = this.parentElement;
    view.shuttle.markForSave();
},

transferNodes : function (nodes, folder, index, sourceWidget) {

    var view      = this.parentElement,
        crossDrop = this != sourceWidget;

    this._filterDropRecords(nodes, crossDrop);

    
    if (crossDrop && !sourceWidget.validateRecordList(nodes)) nodes.clear();

    if (nodes.length > 0) view.shuttle.markForSave();

    sourceWidget._allowInvalidateCache = true;

    // collapse any open records before starting the drop
    if (crossDrop) {
        sourceWidget.collapseRecordList(nodes, true);
    }

    this.Super("transferNodes", arguments);
},

// force sync from DataSource if ResultTree is missing nodes

invalidateCacheIfMissingNodes : function () {
    if (this.data.getAllNodes().length < 
        this.dataSource.cacheData.length) {
        // only invalidate the cache once per transfer
        if (!this._allowInvalidateCache) return;
        delete this._allowInvalidateCache;
        this.invalidateCache();
    }
},    

// overrides from FieldPickerGrid

filterData :            isc.FieldPickerGrid.getPrototype().filterData,
expandRecord :          isc.FieldPickerGrid.getPrototype().expandRecord,
willAcceptDrop:         isc.FieldPickerGrid.getPrototype().willAcceptDrop,
setCanExpandRecords :   isc.FieldPickerGrid.getPrototype().setCanExpandRecords,
getExpansionComponent : isc.FieldPickerGrid.getPrototype().getExpansionComponent,
saveAndCollapseRecord : isc.FieldPickerGrid.getPrototype().saveAndCollapseRecord,
cellHoverHTML :         isc.FieldPickerGrid.getPrototype().cellHoverHTML,
getCellValue :          isc.FieldPickerGrid.getPrototype().getCellValue
                              
});

//////////////////////////////////////////////////////////////////////////////
// Class FieldPickerGridView

isc.ClassFactory.defineClass("FieldPickerGridView", "VLayout");

isc.FieldPickerGridView.addMethods({

initWidget : function () {

    // call the superclass initWidget
    this.Super(this._$initWidget);

    var sectionStack = isc.SectionStack.create({
        height: 5,
        overflow: "visible",
        sections: [{ title: this.title, 
                     controls: this.controls,
                     controlsLayoutProperties: {
                         membersMargin: 2, layoutEndMargin: 2
                     }, 
                     canCollapse: false }]
    });
    this.addMember(sectionStack);
},

addMember : function (newMember) {

    // call the superclass addMember
    this.Super("addMember", arguments);

    // create a reference to autochild ListGrid when added
    if (isc.isA.ListGrid(newMember)) this.grid = newMember;
},

expansionFormDefaults: {
    _constructor: "DynamicForm",
    width: 5,
    titleAlign: isc.Page.isRTL() ? "right" : "left",
    overflow: "visible",
    wrapItemTitles: false,
    validateOnExit: true,
    validateOnChange: true    
},
createExpansionDynamicForm : function (editableProperties, record) {

    var fields = [],
        component = this.shuttle.picker.dataBoundComponent,
        dataSource = this.shuttle.picker.expansionDataSource,
        fieldNames = dataSource.getFieldNames();

    // add fields requested if they're in the expansion DataSource

    for (var i = 0; i < fieldNames.length; i++ ) {
        var name = fieldNames[i],
            show = editableProperties.contains(name);

        switch (name) {
        case "decimalPad":
        case "decimalPrecision":
            if (!isc.SimpleType.inheritsFrom(record.type, "float")) show = false;
            break;
        case "precision":
            
            if (!isc.SimpleType.inheritsFrom(record.type, "float") &&
                !isc.SimpleType.inheritsFrom(record.type, "integer")) show = false;
            break;
        }

        if (show) fields.add({name: name});
    }

    // allow summary/formula fields to be edited

    if (record.userSummary) {
        fields.add({ 
            type: "text",
            autoFit: true,
            endRow: false,
            record: record,
            shuttle: this.shuttle,
            editorType: "ButtonItem",
            name: "_fieldPickerUserSummary",
            title: component.editSummaryFieldText,
            click: function () {
                this.shuttle.confirmSaveOK('editSummaryField', this.record);
            }
        });
    }

    if (record.userFormula) {
        fields.add({ 
            type: "text",
            autoFit: true,
            endRow: false,
            record: record,
            shuttle: this.shuttle,
            editorType: "ButtonItem",
            name: "_fieldPickerUserFormula",
            title: component.editFormulaFieldText,
            click: function () {
                this.shuttle.confirmSaveOK('editFormulaField', this.record);
            }
        });
    }

    if (record.userSummary || record.userFormula) {
        fields.add({ 
            type: "text",
            autoFit: true,
            startRow: false,
            record: record,
            shuttle: this.shuttle,
            editorType: "ButtonItem",
            name: "_fieldPickerRemoveField",
            title: "Remove Field",
            click: function () {
                this.shuttle.confirmRemoveOK(this.record);
            }
        });
    }

    // create expansion form

    var form = this.createAutoChild("expansionForm", {
        dataSource: fields.length > 0 ? dataSource : null,
        fields: fields
    });
    form.setValues(record);

    return form;
},

cleanup : function () {
    this.grid.collapseAllRecords();
}

});

//////////////////////////////////////////////////////////////////////////////
// Class FieldPickerShuttle

isc.ClassFactory.defineClass("FieldPickerShuttle", "HLayout");

isc.FieldPickerShuttle.addMethods({

// unsaved modified field properties

changedRecords: {},

// apply all pending changes to the DataBoundComponent

applyCompleteFields : function (dataBoundComponent) {

    var i, field, record, completeFields = [];

    var shouldUseTrees = this.picker.shouldUseTrees(),
        parentIdField = this.picker.pickerParentIdField;

    // add the visible fields in the user-specified order

    var visibleFieldsGrid = this.currentView.grid,
        nVisibleRows = visibleFieldsGrid.getTotalFieldRecords();

    for (i = 0; i < nVisibleRows; i++) {
        record = visibleFieldsGrid.getFieldRecord(i);
        
        field = this.picker.getOriginalField(record);
        field.title = record._title;

        // apply any pending property changes from expanded records
        if (visibleFieldsGrid.isExpanded(record)) {
            var form = visibleFieldsGrid.getCurrentExpansionComponent(record);
            if (form && form.valuesHaveChanged()) {
                var values = form.getChangedValues();
                isc.addProperties(field, values);
                form.setValues(values);
            }
        }
        
        // commit any reparenting of nodes that has occurred 
        if (shouldUseTrees) field[parentIdField] = record["_" + parentIdField];

        // XXX review: we have a number of possible settings and some components use one or
        // more of these.  Be absolutely explicit given that the user is telling us exactly
        // what they want
        field.showIf = "true";
        field.visible = true;
        field.hidden = false;
        completeFields.add(field);
    }

    visibleFieldsGrid.clearFieldRecords();

    // now add hidden fields since we must include all fields

    var hiddenFieldsGrid = this.availableView.grid,
        nHiddenRows = hiddenFieldsGrid.getTotalFieldRecords();

    for (i = 0; i < nHiddenRows; i++) {
        record = hiddenFieldsGrid.getFieldRecord(i);
        
        field = this.picker.getOriginalField(record);

        // commit any reparenting of nodes that has occurred 
        if (shouldUseTrees) field[parentIdField] = record["_" + parentIdField];

        // XXX review: we have a number of possible settings and some components use one or
        // more of these.  Be absolutely explicit given that the user is telling us exactly
        // what they want
        field.showIf = "false";
        field.visible = false;
        field.hidden = true;
        completeFields.add(field);
    }

    hiddenFieldsGrid.clearFieldRecords();

    // add in any pending property changes from expansion editing

    for (i = 0; i < completeFields.length; i++ ) {
        var changes = this.changedRecords[completeFields[i].name];
        if (changes) isc.addProperties(completeFields[i], changes);
    }

    dataBoundComponent.setFields(completeFields);

    this.clearChanges();
},

// reset our state now to reflect no pending changes

clearChanges : function () {
    this.changedRecords = {};
    this.recordsMoved = false;
},

// track changes to the fields

markForSave : function () {
    this.recordsMoved = true;
},

requiresSave : function () {

    // check for open records not yet tracked
    var grid = this.currentView.grid,
        nVisibleRows = grid.getTotalFieldRecords();

    for (var i = 0; i < nVisibleRows; i++) {
        var record = grid.getFieldRecord(i);
        if (grid.isExpanded(record)) {
            var form = grid.getCurrentExpansionComponent(record);
            if (form && form.valuesHaveChanged()) return true;
        }
    }

    grid.clearFieldRecords();

    var changedRecordNames = isc.getKeys(this.changedRecords);
    return this.recordsMoved || changedRecordNames.length > 0;
},

// warn user when a field is about to be removed

confirmRemoveOK : function (record) {
    // OK since we're modal
    this._pendingArgument = record;

    isc.confirm(this.picker.removeText, {target: this, methodName: "completeConfirmRemoveOK"});
},

completeConfirmRemoveOK : function (value) {

    var record = this._pendingArgument;
    delete this._pendingArgument;

    if (value) this.currentView.grid.removeData(record);
},

// warn user if/when changes must be saved out

confirmSaveOK : function (methodName, argument) {

    // OK since we're modal
    this._pendingMethodName = methodName;
    this._pendingArgument   = argument;

    if (this.requiresSave()) {
        isc.confirm(this.picker.confirmText, {target: this, methodName: "completeSaveOK"});
    } else {
        this.completeSaveOK(true);
    }
},

completeSaveOK : function (value) {

    var methodName = this._pendingMethodName,
        argument = this._pendingArgument;

    delete this._pendingMethodName;
    delete this._pendingArgument;

    if (value) this.saveAndExecuteMethod(methodName, argument);
},

saveAndExecuteMethod : function (methodName, argument) {
    var picker = this.picker;

    // do not allow save if records (DBC fields) fail validation
    if (!picker.currentFieldsGrid.validateAllRecords()) return false;

    // ensure that title edits are not lost
    picker.currentFieldsGrid.endEditing();

    // clear any filters so that all records are applied
    picker.currentFieldsGrid.clearCriteriaIfPresent();
    picker.availableFieldsGrid.clearCriteriaIfPresent();

    picker.currentFieldsGrid.collapseAllRecords();
    this.applyCompleteFields(picker.dataBoundComponent);
    if (methodName != null) this.openEditor(methodName, argument);

    return true;
},

openEditor : function (methodName, argument) {

    var dataBoundComponent = this.picker.dataBoundComponent;
    dataBoundComponent[methodName](argument);

    var editBuilderField = true;

    switch(methodName) {
    case "addSummaryField":
    case "addFormulaField":
        editBuilderField = false;
        // fall through by design
    case "editSummaryField":
    case "editFormulaField":
        var window = dataBoundComponent.fieldEditorWindow;
        if (window) {
            var builder = window.items[0];
            if (editBuilderField) window.items[0].saveAddAnotherButton.hide();
            this.observe(builder, "fireOnClose", "observer.picker.refresh(observed)");
            builder.testRecord = this.picker.sampleRecord;
        }
        break;
    case "editHilites":
        var window = dataBoundComponent.hiliteWindow;
        if (window) {
            this.observe(window, "hide", "observer.picker.refresh(null, observed)");
        }
        break;
    }
},

// build tracker - two grids and directional arrows

availableViewDefaults: {
    _constructor: "FieldPickerGridView"
},
currentViewDefaults: {
    _constructor: "FieldPickerGridView"
},

buttonStackDefaults: {
    align: "center",
    overflow: "visible",
    layoutAlign: "center"
},

hilitesButtonConstructor: "IButton",
hilitesButtonDefaults: {
    autoFit: true,
    click: "this.shuttle.confirmSaveOK('editHilites')"
},

initWidget : function () {

    // call the superclass initWidget
    this.Super(this._$initWidget);

    var picker = this.picker,
        component = picker.dataBoundComponent;

    // create the grid for the available fields

    this.availableView = this.createAutoChild("availableView", {
        shuttle: this,
        controls: picker.availableFieldsHeaderControls,
        title: picker.availableFieldsTitle
    });

    // build list of buttons for the current fields

    var currentControls = [];
    var availableControls = picker.currentFieldsHeaderControls;
    if (availableControls == null) {
        availableControls = [];
        if (picker.showHilitesButton) availableControls.add("hilitesButton");
        if (component.canAddFormulaFields || component.canAddSummaryFields) availableControls.add("customFieldsMenuButton");
    }
    for (var i = 0; i < availableControls.length; i++) {
        var availableControl = availableControls[i];
        if (availableControl == "hilitesButton") {
            this.hilitesButton = this.createAutoChild("hilitesButton", {
                shuttle: this,
                title: picker.hilitesText
            });
            currentControls.add(this.hilitesButton);
        } else if (availableControl == "customFieldsMenuButton") {
            var items = [];
            if (component.canAddSummaryFields) items.add({
                shuttle: this,
                title: component.addSummaryFieldText,
                click: "item.shuttle.confirmSaveOK('addSummaryField')",
                icon: "[SKINIMG]ListGrid/formula_menuItem.png"
            });
            if (component.canAddFormulaFields) items.add({
                shuttle: this,
                title: component.addFormulaFieldText,
                click: "item.shuttle.confirmSaveOK('addFormulaField')",
                icon: "[SKINIMG]ListGrid/formula_menuItem.png"
            });
            currentControls.add(isc.MenuButton.create({
                autoFit: true,
                title: picker.addCustomFieldsButtonTitle,
                menu: isc.Menu.create({ data: items })
            }));
        } else if (isc.isA.Canvas(availableControl)) {
            availableControl.addProperties({
                shuttle: this,
                picker: picker
            });
            currentControls.add(availableControl);
        }
    }
    
    // now use the button list to create current fields grid

    this.currentView = this.createAutoChild("currentView", {
        shuttle: this,
        controls: currentControls,                                                            
        title: picker.currentFieldsTitle
    });

    this.initTransferArrows();

    this.addMembers([
        this.availableView, this.horizontalArrows, 
        this.currentView,   this.verticalArrows
    ]);

},

// add the arrow stacks to the shuttle, with appropriate handlers
initTransferArrows : function () {

    var current = this.currentView,
        available = this.availableView;
        
    this.horizontalArrows = isc.VLayout.create({ 
        width: 1,
        members: [
            isc.ImgButton.create({
                size: 16, showDown:false,
                align: "center",
                src: "[SKINIMG]TransferIcons/right.png",
                click : function () {
                    current.grid.transferSelectedData(available.grid);
                }
            }), 
            isc.LayoutSpacer.create({ height: 5 }),
            isc.ImgButton.create({
                size: 16, showDown:false,
                src: "[SKINIMG]TransferIcons/left.png",
                click : function () {
                    available.grid.transferSelectedData(current.grid);
                }
            })]
    }, this.buttonStackDefaults);

    var moveToBoundarySlot = function (direction) {
        var grid = current.grid,
            selection = grid.getSelection() || [],
            targetIndex = direction == -1 ? 0 : grid.getTotalRows();
        grid.transferRecords(selection, null, targetIndex, grid);
        grid.scrollToRow(targetIndex);
    };
    var moveByOneSlot = function (direction) {
        var grid = current.grid,
            selection = grid.getContiguousSelection();
        if (selection) {
            var targetIndex = grid.getRecordSetBounds(selection, direction, 1);
            grid.transferRecords(selection, null, targetIndex, grid);
            grid.scrollToRow(targetIndex - (direction + 1) / 2);
        }
    };

    if (this.picker.showFieldOrderButtons) {
        this.verticalArrows = isc.VLayout.create({ 
            width: 1,
            members: [
                isc.ImgButton.create({
                    size: 16, showDown:false,
                    src: "[SKINIMG]TransferIcons/up_first.png",
                    click : function () { moveToBoundarySlot(-1); }
                }), 
                isc.LayoutSpacer.create({ height: 5 }),
                isc.ImgButton.create({
                    size: 16, showDown:false,
                    src: "[SKINIMG]TransferIcons/up.png",
                    click : function () { moveByOneSlot(-1); }
                }),
                isc.LayoutSpacer.create({ height: 5 }),
                isc.ImgButton.create({
                    size: 16, showDown:false,
                    src: "[SKINIMG]TransferIcons/down.png",
                    click : function () { moveByOneSlot(1); }
                }),
                isc.LayoutSpacer.create({ height: 5 }),
                isc.ImgButton.create({
                    size: 16, showDown:false,
                    src: "[SKINIMG]TransferIcons/down_last.png",
                    click : function () { moveToBoundarySlot(1); }
                })]
                                                
        }, this.buttonStackDefaults);
    }
},

cleanup : function () {
    this.currentView.cleanup();
    this.clearChanges();
}

});
                                 
//////////////////////////////////////////////////////////////////////////////
// Class FieldPicker

isc.ClassFactory.defineClass("FieldPicker", "VLayout");

//> @class FieldPicker
// FieldPicker provides a configuration dialog that displays, side-by-side, the available and
// currently-displayed fields of a +link{dataBoundComponent}. It allows for easy customization 
// of the order in which the fields of a +link{dataBoundComponent} are displayed, and of which
// are visible.  If so configured, it also allows for convenient launching of the HiliteEditor,
// FormulaBuilder, and SummaryBuilder.  A FieldPicker instance runs in its own window, 
// a +link{fieldPickerWindow}
// @treeLocation Client Reference/Data Binding
// @visibility external
//<

isc.FieldPicker.addProperties({

layoutMargin: 10,
membersMargin: 10,
originalFields: {},

//> @attr fieldPicker.dataBoundComponent (Canvas : null : IR)
// The component whose fields should be edited.
// @visibility external
//<
dataBoundComponent : null,

//> @attr fieldPicker.dataSource (DataSource : null : IR)
// An optional DataSource that is used to create a disposable
// +link{fieldPicker.dataBoundComponent} if none is provided.
// Has no effect if a +link{fieldPicker.dataBoundComponent} is specified.
// @visibility external
//<

//> @method fieldPicker.callback
// Callback invoked when picker changes are committed, if a disposable 
// +link{dataBoundComponent} is present.
// @param fields (Array of ListGridFields) committed fields from disposable component
// @param hilites (Array of Hilite) Array of hilite objects
// @see fieldPicker.dataSource
// @visibility external
//<

//> @type DefaultSampleRecord 
// Some interfaces, for example the +link{class:FieldPicker, FieldPicker} and 
// +link{class:HiliteEditor, HiliteEditor} widgets, can use data from an associated 
// DataBoundComponent to express live sample values at runtime.
// @value "first" Uses the first record in the DataBoundComponent as sample data
// @value "firstOpenLeaf" Uses the first open leaf-node in the DataBoundComponent as sample data
// @visibility external
//<

//> @attr fieldPicker.sampleRecord (Record | DefaultSampleRecord : "first" : IR)
// If a <code>sampleRecord</code> is provided, the FieldPicker will show a second column in the
// Current Fields dialog showing the cell value that will appear for that field given the
// provided sample record.
// <br>
// A value of "first" means the first record.  If the underlying
// +link{fieldPicker.dataBoundComponent} is a +link{TreeGrid}, you can specify "firstOpenLeaf"
// to use the first open leaf as the sampleRecord (this is often desirable in trees where the
// first record may be a folder that's used for organizational purposes only and hence would
// have no actual data for columns other than the tree column).
// @visibility external
//<
sampleRecord: "first",

//> @attr fieldPicker.showHilitesButton (boolean : true : IR)
// Shows a "Highlights..." button that shows an interface for editing hilites in the attached
// DataBoundComponent.
// @visibility external
//<

//showHilitesButton: true,

//> @attr fieldPicker.showFieldOrderButtons (boolean : true : IR)
// When set to false, hides the right-most set of buttons, used for re-ordering fields in the
// Visible Fields list.
// @visibility external
//<
showFieldOrderButtons: true,

//> @attr fieldPicker.hilitesText (String : "Highlights..." : [IR])
// @group i18nMessages
// @visibility external
//<
hilitesText: "Highlights...",

//> @attr fieldPicker.availableFieldsTitle (String : "Available Fields" : [IR])
// @group i18nMessages
// @visibility external
//<    
availableFieldsTitle: "Available Fields",

//> @attr fieldPicker.currentFieldsTitle (String : "Visible Fields" : [IR])
// @group i18nMessages
// @visibility external
//<    
currentFieldsTitle: "Visible Fields",

//> @attr fieldPicker.addCustomFieldsButtonTitle (String : "Add Custom Fields" : IR)
// The title displayed for the Add Custom Fields Button
// @group i18nMessages
// @visibility external
//<
addCustomFieldsButtonTitle: "Add Custom Fields",

//> @attr fieldPicker.availableTitleTitle (String : "Name" : IR)
// The title displayed for the title property of the available fields
// @group i18nMessages
// @visibility external
//<
availableTitleTitle: "Name",

//> @attr fieldPicker.currentTitleTitle (String : "Field Title" : IR)
// The title displayed for the title property of the current fields
// @group i18nMessages
// @visibility external
//<
currentTitleTitle: "Field Title",

//> @attr fieldPicker.sampleValueTitle (String : "Sample Value" : IR)
// The title displayed for the sample value property of the current fields
// @group i18nMessages
// @visibility external
//<
sampleValueTitle: "Sample Value",

//> @attr fieldPicker.sampleValueField (String : "_sampleValue" : IR)
// The name used for the sample value property of the complete fields
//<
sampleValueField: "_sampleValue",


//> @attr fieldPicker.showAvailableSampleValue (Boolean : true : IR)
// When set to true, also show the sample value in the available fields list.  Set to false to
// show field names only.
//<
showAvailableSampleValue: true,

//> @attr fieldPicker.showSampleValues (Boolean : true : IR)
// When set to false, sample values are never shown. This property applies to the entire FieldPicker.
//<
showSampleValues: true,

//> @attr fieldPicker.confirmText (String : "Must save pending changes to proceed. OK?" : [IR])
// @group i18nMessages
// @visibility external
//<
confirmText: "Must save pending changes to proceed. OK?",

//> @attr fieldPicker.removeText (String : "You are about to remove the field. Are you sure?" : [IR])
// @group i18nMessages
// @visibility external
//<
removeText: "You are about to remove the field. Are you sure?",

//> @attr fieldPicker.instructions (String : "Drag and drop or use arrows to move fields.  Drag reorder to change field order." : [IR])
// @group i18nMessages
// @visibility external
//<    
instructions: "Drag and drop or use arrows to move fields.  Drag reorder to change field order.",

//> @attr fieldPicker.instructionLabel (AutoChild Label : null : IR)
// A +link{class:Label, label} displaying the text assigned as the FieldPicker's
// +link{fieldPicker.instructions, instructions}.  Shown across the top of the widget.
// @visibility external
//<
instructionLabelConstructor: "Label",
instructionLabelDefaults: {
    height: 5,
    overflow: "visible"
},

//> @attr fieldPicker.saveAndExitButtonTitle (String : "Apply" : IR)
// The title shown on the Save and Exit button
// @group i18nMessages
// @visibility external
//<
saveAndExitButtonTitle: "Apply",

//> @attr fieldPicker.cancelButtonTitle (String : "Cancel" : IR)
// The title shown on the Cancel button
// @group i18nMessages
// @visibility external
//<
cancelButtonTitle: "Cancel",

//> @attr fieldPicker.removeItemTitle (String : "Remove" : IR)
//The title shown on the 'Visible Fields' grid's context menu item, whose click handler puts the 
//selected item back in the 'Available Fields' collection.
//@group i18nMessages
//@visibility external
//<
removeItemTitle: "Remove",


//> @attr fieldPicker.emptyTitleHint (String : "[No title specified]" : IR)
// The hint shown when editing a field with no title defined.
// @group i18nMessages
// @visibility external
//<
emptyTitleHint: "[No title specified]",

//> @attr fieldPicker.showFieldPrompts (boolean : true : IR)
// If a +link{listGridField.prompt,prompt} is specified for the field, should it be 
// displayed when the user hovers over the record correlating to the field in the
// fields grids?
// @visibility fieldPicker
//<
showFieldPrompts:true,

//> @attr fieldPicker.sortAvailableFields (boolean : true : IR)
// Should the available fields grid be sorted by default? Grid will be sorted by
// +link{fieldPicker.availableFieldsSortDirection}.
// @visibility fieldPicker
//<
sortAvailableFields:true,

//> @attr fieldPicker.availableFieldsSortDirection (SortDirection : "ascending" : IR)
// If +link{fieldPicker.sortAvailableFields} is <code>true</code>, this property will
// govern the sort-direction for the initial sort applied to the available fields grid.
// @visibility fieldPicker
//<
availableFieldsSortDirection:"ascending",

//> @attr fieldPicker.availableFieldsHeaderControls (Array of Canvas : null : IR)
// Provides a set of controls to appear as 
// +link{sectionHeader.controls,section header controls} above the available fields grid.
// @visibility external
//<
availableFieldsHeaderControls: null,

//> @attr fieldPicker.availableFieldsGrid (AutoChild ListGrid : null : IR)
// A +link{class:ListGrid, ListGrid} showing the list of available fields.
// @visibility external
//<
availableFieldsGridDefaults : {
    canGroupBy: false,
    dataFetchMode: "basic",
    dragDataAction: "move",
    canFreezeFields: false,
    showFilterEditor: true,
  	filterOnKeypress: true,
    canDragRecordsOut: true,
    loadDataOnDemand: false,
    dragRecategorize: "never",
    keepParentsOnFilter: true,
    autoFitWidthApproach: "both",
    useAllDataSourceFields: true,
    canAcceptDroppedRecords: true,
    createDefaultTreeField: false,
    recordEnabledProperty: "_enabled",
    autoFetchTextMatchStyle: "substring"
},


getOriginalField : function (pickerRecord) {
    return this.originalFields[pickerRecord[this.primaryKeyField]];
},

// datasource determines whether tree or list is used for picking field

shouldUseTrees : function () {
    if (this.dataBoundComponent) {
        var dataSource = this.dataBoundComponent.dataSource;
        return dataSource != null && !!dataSource.showFieldsAsTree;
    }
    return false;
},

// copy the list of supplied fields for use by the FieldPicker; validate fields

duplicateAndValidateFields : function (fields) {
    var result = [],
        component = this.dataBoundComponent,
        canReorder = !isc.isA.ListGrid(component) || component.canReorderFields;
    
    for (var i = 0; i < fields.length; i++) {
        var field = fields[i];
        if (!field.excludeFromState) result.add(field);
        if ((field.canReorder == false || field.canReorder == null && !canReorder) &&
            field.canHide == false) field._enabled = false;
    }
    return result;
},

// create a DataSource for the available or current fields ListGrid

createDataSourceFromFields : function (fields, exclusions, available, skipDuplicate) {
    var includedFields = skipDuplicate ? fields : this.duplicateAndValidateFields(fields);

    if (exclusions != null) includedFields.removeList(exclusions);

    var fields = [],
        component = this.dataBoundComponent;

    
    if (available) {
        includedFields = includedFields.filter(function (field) { 
            return field.canHide != false; 
        });
    }

    var auto, title = available ? this.availableTitleTitle: this.currentTitleTitle;
    
    if (isc.isA.String(this.sampleRecord)) {
        if (this.sampleRecord == "first") {
            
            if (isc.isA.ListGrid(component)) {
                if (component.fieldPickerShowSampleValues) {
                    var data = component.getOriginalData();
                    this.sampleRecord = data && data.getLength() > 0 ? data.get(0) : null;
                }
            } else if (this.showSampleValues) {
                this.sampleRecord = component.getRecord(0);
            }
            // ignore e.g. loading markers
            if (!isc.isAn.Object(this.sampleRecord)) this.sampleRecord = null;
        } else if (this.sampleRecord == "firstOpenLeaf" && isc.isA.TreeGrid(component)) {
            var data = component.getData();
            var openList = data.getOpenList(data.getRoot(), isc.Tree.LEAVES_ONLY);
            if (openList.length) this.sampleRecord = openList.get(0);
            else this.sampleRecord = component.getRecord(0);
        }
    }
    
    if (isc.isA.Object(this.sampleRecord)) {
        for (var i = 0; i < includedFields.length; i++) {
            var field = includedFields[i];
            field[this.sampleValueField] = 
                component.getStandaloneFieldValue(this.sampleRecord, field.name);
        }
        if (!available || this.showAvailableSampleValue) {
            fields.add({ 
                name: this.sampleValueField, 
                title: this.sampleValueTitle,
                canFilter: false,
                canEdit: false,
                type: "any"
            });
            auto = true;
        }
    }

    var nameField = {name: "name",   title: title, autoFitWidth: auto, primaryKey: true},
       titleField = {name: "_title", title: title, autoFitWidth: auto,
                     editorProperties: { showHintInField: true, hint: this.emptyTitleHint},
                     canEdit: this.canEditTitles && !available};

    if (this.useTitleField) { nameField.hidden = true; titleField.treeField = true; }
    else                    { titleField.hidden = true; nameField.treeField = true; }

    fields.addListAt([nameField, titleField], 0);

    // if picker is in tree mode, add required extra dataSource fields

    if (this.shouldUseTrees()) {
        var id       = this.pickerIdField,
            parentId = this.pickerParentIdField,
            rootValue = component.dataSource.fieldTreeRootValue;

        fields.addList([
            {name: id,             hidden: true, primaryKey: true },
            {name: "_" + parentId, hidden: true, foreignKey: id, rootValue: rootValue}]);

        if (this.useTitleField) titleField.canFilter = true;
        else                    nameField.canFilter  = true;

        delete nameField.primaryKey;
        
        includedFields.map(function (field) { field["_" + parentId] = field[parentId]; });
    }

    // our DataSources may receive copied records, so store originals

    for (var i = 0; i < includedFields.length; i++) {
        var field = includedFields[i];
        this.originalFields[field[this.primaryKeyField]] = field;
        if (field.userSummary || field.userFormula) this.customFields = true;
        field._title = field.title;
    }

    var dataSource = isc.DataSource.create({
        fields: fields,
        clientOnly: true,
        dataProtocol: "clientCustom",
        transformRequest : function (dsRequest) {
            var dsResponse = this.getClientOnlyResponse(dsRequest, null);
            this.processResponse(dsRequest.requestId, dsResponse);
            return dsRequest.data;
        }
    });

    dataSource.setCacheData(includedFields);
    return dataSource;
},

// create the DataSource for use by the DynamicForm record editor

createExpansionDataSource : function () {

    

    

    var fields = [{ name: "frozen",
                    title: isc.FieldPickerField.frozenTitle,
                    showTitle: false,
                    type: "boolean" },
                  { name: "precision", validators: [
                        { type: "regexp", errorMessage:
                          "Must be between 1 and 21 inclusive",
                          expression: "^([1-9]|1[0-9]|2[0-1])?$" }
                    ],
                    title: isc.FieldPickerField.precisionTitle,
                    type: "integer" },
                  { name: "decimalPrecision",
                    title: isc.FieldPickerField.decimalPrecisionTitle,
                    type: "integer" },
                  { name: "decimalPad", validators: [ 
                        { type: "regexp", errorMessage:
                          "Must be between 0 and 20 inclusive",
                          expression: "^([0-9]|1[0-9]|20)?$"}
                    ],
                    title: isc.FieldPickerField.decimalPadTitle,
                    type: "integer" },
                  { name: "align",
                    title: isc.FieldPickerField.alignTitle,
                    valueMap: ["left", "center", "right"]},
                  { name: "cellAlign",
                    title: isc.FieldPickerField.cellAlignTitle,
                    valueMap: ["left", "center", "right"]}];

    // remove inappropriate properties for the DetailViewer

    if (isc.isA.DetailViewer(this.dataBoundComponent)) {
        var filterFunction = function (object) { 
            switch(object.name) {
            case "frozen":
            case "align":
            case "cellAlign":
                return false;
            default:
                return true;
            }};
        fields = fields.filter(filterFunction);
    }

    this.expansionDataSource = isc.DataSource.create({
        clientOnly: true,
        fields: fields
    });
},

updateFieldConfiguration: function (firstField) {
    this.useTitleField = firstField && firstField.title != null;
    this.primaryKeyField = this.shouldUseTrees() ? this.pickerIdField : "name";
},

updateEditableProperties : function () {
    var properties = this.dataBoundComponent.fieldPickerFieldProperties,
        grid = this.currentFieldsGrid;

    isc.addProperties( grid, {
        editableProperties: properties || [],        
        autoFitFieldWidths: isc.isA.Object(this.sampleRecord)
    });
    // expansion needed if either editable field properties or summary/formula fields present
    grid.setCanExpandRecords(isc.isA.Array(properties) && properties.length > 0 || 
                             this.customFields);
},

// validate DataSourceField properties

validateDataSourceField : function (field) {
    // validate argument to dataSourceField.toPrecision
    var precision = field.precision;
    if (precision != null && (precision < 1 || precision > 21)) return false;

    // validate argument to dataSourceField.toFixed
    var decimalPad = field.decimalPad;
    if (decimalPad != null && (decimalPad < 0 || decimalPad > 20)) return false;

    return true;
},

// update the FieldPicker to deal with a changed DataBoundComponent

refresh : function (builder, hiliter) {

    if (builder) {
        var shuttle = this.shuttle;
        shuttle.ignore(builder, "fireOnClose");
        var newWindow = this.dataBoundComponent.fieldEditorWindow;
        // add new observer if builder window has respawned via "save & add another"
        if (!newWindow.destroying && !newWindow.destroyed) {
            shuttle.observe(newWindow.items[0], "fireOnClose", 
                            "observer.picker.refresh(observed)");
        }
    }
    if (hiliter) { this.shuttle.ignore(hiliter, "hide"); }

    var currentFields  = this.dataBoundComponent.fields,
        completeFields = this.dataBoundComponent.completeFields,
        dataSource;

    this.updateFieldConfiguration(completeFields[0]);

    this.availableFieldsGrid.setDataSource(
        this.createDataSourceFromFields(completeFields, currentFields, true));
    this.availableFieldsGrid.fetchData();

    this.currentFieldsGrid.setDataSource(
        this.createDataSourceFromFields(currentFields));
    this.currentFieldsGrid.fetchData();

    this.updateEditableProperties();

    this.needsRefresh = false;
},

initWidget : function () {

    // call the superclass initWidget
    this.Super(this._$initWidget);
    
    // If no DataBoundComponent is supplied, create a disposable DBC based on the 
    // fields of supplied DataSource, and return DBC fields, etc. in a callback
    if (this.dataBoundComponent == null && this.dataSource != null) {
        var picker = this,
            properties = isc.addProperties({}, this.creator.fieldPickerProperties, {
                autoDraw: false,
                fieldStateChanged : function () {
                    if (isc.isA.Function(picker.callback)) {
                        picker.callback(this.completeFields, this.getHilites());
                    }
                }}
            );
        var component = isc.ListGrid.create(properties);
        component.setFields(component.fields);
        this.dataBoundComponent = component;
    }

    if (this.showHilitesButton == null) {
        var dbc = this.dataBoundComponent;
        this.showHilitesButton = dbc && dbc.canEditHilites;
    }

    this.instructionLabel = this.createAutoChild("instructionLabel", 
        { contents: this.instructions }
    );
    this.addMember(this.instructionLabel);

    // create the shuttle widget for dragging/dropping fields

    var shuttle = isc.FieldPickerShuttle.create({
        picker: this,
        membersMargin: 10
    });
    this.shuttle = shuttle;
    this.addMember( shuttle );

    // AutoChildren class depends upon the DataSource properties
    var gridClass = this.shouldUseTrees() ? isc.FieldPickerTreeGrid : 
                                            isc.FieldPickerListGrid;

    // create the ListGrid AutoChildren available/current Fields

    var currentFields  = this.dataBoundComponent.fields,
        completeFields = this.dataBoundComponent.completeFields;

    this.updateFieldConfiguration(completeFields[0]);

    var availableFieldsSort;
    if (this.sortAvailableFields) {
        availableFieldsSort = [{
            property:"_title",
            direction:this.availableFieldsSortDirection
        }];
    }

    this.addAutoChild("availableFieldsGrid", {
        autoFetchData: true,
        initialSort:availableFieldsSort,
        autoFitExpandField: this.showAvailableSampleValue ? this.sampleValueField : null,
        dataSource: this.createDataSourceFromFields(completeFields, currentFields, true),
        rowDoubleClick : function (record, recordNum, fieldNum) {
            var current = shuttle.currentView.grid;
        	current.transferSelectedData(this);
        }
    }, gridClass, shuttle.availableView);

    this.addAutoChild("currentFieldsGrid", {
        autoFetchData: true,
        autoFitExpandField: this.sampleValueField,
        dataSource: this.createDataSourceFromFields(currentFields),
        contextMenu : isc.Menu.create({
        	autoDraw: false,
        	data : [{
        		title : this.removeItemTitle, 
        		click : function () {
        			var current = shuttle.currentView.grid;
        			var available = shuttle.availableView.grid;
        			available.transferSelectedData(current);
        		}
        	}]
        })
    }, gridClass, shuttle.currentView);

    this.createExpansionDataSource();
    this.updateEditableProperties();

    // create save/cancel buttons

    this.addAutoChild("buttonLayout");
    if (this.buttonLayout) {
        this.saveAndExitButton = this.createAutoChild("saveAndExitButton", {
            picker: this,
            title: this.saveAndExitButtonTitle
        });
        this.cancelChangesButton = this.createAutoChild("cancelChangesButton", {
            picker: this,
            title: this.cancelButtonTitle
        });
        this.buttonLayout.addMembers([this.saveAndExitButton, this.cancelChangesButton]);
        this.addMember(this.buttonLayout);
    }

},

//> @attr fieldPicker.buttonLayout (AutoChild HLayout : null : IR)
// A +link{class:HLayout, horizontal layout} used to show the 
// +link{fieldPicker.saveAndExitButton, Save} and +link{fieldPicker.cancelChangesButton, Cancel} 
// buttons.
// @visibility external
//<
buttonLayoutConstructor: "HLayout",
buttonLayoutDefaults: {
        height: 5,
        align: "right",
        overflow: "visible",
        membersMargin: 10,
    defaultLayoutAlign: "center"
},

//> @attr fieldPicker.saveAndExitButton (AutoChild IButton : null : IR)
// An AutoChild +link{class:IButton, button} that saves the current field-set and exits the 
// Field Picker.
// @visibility external
//<
saveAndExitButtonConstructor: "IButton",
saveAndExitButtonDefaults: {
    click: "this.picker.saveClick()"
},
//> @attr fieldPicker.cancelChangesButton (AutoChild IButton : null : IR)
// An AutoChild +link{class:IButton, button} that saves the current field-set and exits the 
// Field Picker.
// @visibility external
//<
cancelChangesButtonConstructor: "IButton",
cancelChangesButtonDefaults: {
    click: "this.picker.closeClick()"
},

//> @method fieldPicker.setAvailableFields()
// Provides a new set of available fields.
// @param newFields (Array of DataSourceField) 
// @visibility external
//<
setAvailableFields : function (newFields) {

    var i, undef, fieldPool = {};

    for (i = 0; i < newFields.length; i++) {
        var newField = newFields[i];
        fieldPool[newField.name] = newField;
    }

    var currentGrid = this.currentFieldsGrid,
        currentFields = currentGrid.dataSource.cacheData;

    var newCurrentFields = [];
    for (var i = 0; i < currentFields.length; i++) {
        var field = currentFields[i];
        if (fieldPool[field.name] !== undef) {
            newCurrentFields.add(field);
            delete fieldPool[field.name];
        }
    }
    this.currentFieldsGrid.setDataSource(
        this.createDataSourceFromFields(newCurrentFields, null, false, true));
    this.currentFieldsGrid.fetchData();

    var availableGrid = this.availableFieldsGrid,
        availableFields = availableGrid.dataSource.cacheData;

    var newAvailableFields = [];
    for (var i = 0; i < availableFields.length; i++) {
        var field = availableFields[i];
        if (fieldPool[field.name] !== undef) {
            newAvailableFields.add(field);
            delete fieldPool[field.name];
        }
    }

    for (var name in fieldPool) {
        if (fieldPool.hasOwnProperty(name)) {
            var title = isc.DataSource.getAutoTitle(name);
            newAvailableFields.add({name: name, title: title });
        }
    }

    this.availableFieldsGrid.setDataSource(
        this.createDataSourceFromFields(newAvailableFields, null, true, true));
    this.availableFieldsGrid.fetchData();
},

// save/cancel handling and cleanup

saveClick : function () {
    // only notify the DBC of the field state change and close the
    // window if the save succeeded; otherwise validation failed
    if (this.shuttle.saveAndExecuteMethod()) {
        var component = this.dataBoundComponent;
        component.markForRedraw();
        // not all DBC's define this method!
        if (component.handleFieldStateChanged) {
            component.handleFieldStateChanged();
        }
        // if there's a callback installed, fire it now
        if (this.callback) this.callback(component.completeFields, component.getHilites());
        this.creator.closeClick();
    }
},

closeClick : function () {},

cleanup: function () {

    this.shuttle.cleanup();

    var completeFields = this.dataBoundComponent.completeFields,
        canExpandProperty = this.currentFieldsGrid.canExpandrecordProperty;

    for (var i = 0 ; i < completeFields.length; i++) {
        var field = completeFields[i];
        delete field[this.sampleValueField];
        delete field[canExpandProperty];
        delete field["_" + this.pickerParentIdField];
        delete field._enabled;
        delete field._title;
    }

    this.originalFields = {};
}

});

// Since object initialization order in JavaScript is not specified, initialize
// the currentFieldsGrid AutoChild default properties here to allow us to reuse
// the availableFieldsGrid AutoChild default properties.

isc.FieldPicker.addProperties({

//> @attr fieldPicker.currentFieldsGrid (AutoChild ListGrid : null : IR)
// A +link{class:ListGrid, ListGrid} showing the list of currently selected fields.
// @visibility external
//<
currentFieldsGridDefaults : isc.addProperties(
    isc.shallowClone(isc.FieldPicker.getPrototype().availableFieldsGridDefaults), {
        canSort: false,
        expansionMode: "editor",
        canReparentNodes: true,
        canReorderRecords: true,
        canExpandRecordProperty: "_canExpand"
    })

});

//////////////////////////////////////////////////////////////////////////////
// Class FieldPickerWindow
//> @class FieldPickerWindow
// A dialog for picking fields to display from among the available fields.
// <p>
// This is typically useful in scenarios where there are many more fields than can reasonably
// fit on screen. The application can start off displaying a few of the fields by default (such
// as the most commonly-needed fields), and show a FieldPickerWindow to allow the user to
// customize which fields to display as well as the order in which to display them.
// @example fieldPicker
// @treeLocation Client Reference/Data Binding/FieldPicker
// @visibility external
//<

if (isc.Window != null) {
    
    isc.ClassFactory.defineClass("FieldPickerWindow", "Window");
    
    isc.FieldPickerWindow.addProperties({
        
    //> @attr fieldPickerWindow.title (String : "Field Picker" : [IR])
    // @group i18nMessages
    // @visibility external
    //<    
    title: "Field Picker",
    
    width: 800,
    height: 425,
    
    isModal: true,
    canDragResize: true,

    // autoCenter by default, rather than calling centerInPage() at draw time - means
    // devs can override the centering behavior
    autoCenter: true,

    //> @attr fieldPickerWindow.fieldPicker (AutoChild FieldPicker : null : IR)
    // A +link{class:FieldPicker, FieldPicker} for altering the working field-set in a 
    // +link{class:DataBoundComponent, Data-bound component}.
    // @visibility external
    //<
    fieldPickerConstructor: "FieldPicker",
    fieldPickerDefaults: {
        autoParent: "none",
        pickerIdField: "fieldTreeId",
        pickerParentIdField: "fieldTreeParentId"
    },
    
    initWidget : function () {
        
        // call the superclass initWidget
        this.Super(this._$initWidget);
    
        this.addAutoChild("fieldPicker");
        this.addItem(this.fieldPicker);
    
        this.observe(this.fieldPicker, "closeClick", "observer.closeClick()");
    },
    
    show : function () {
        if (this.needsRefresh) this.fieldPicker.refresh();
        return this.Super("show", arguments);
    },
    
    closeClick : function () {
        this.fieldPicker.cleanup();
        this.needsRefresh = true;
        return this.Super("closeClick", arguments);
    }
    
    });
} else {
    isc.Log.logInfo("Source for standard FieldPickerWindow class included in this module, but required " +
        "related class (Window) is not loaded. This can occur if the Grid module is " +
        "loaded without the Containers module.", "moduleDependencies");

}
