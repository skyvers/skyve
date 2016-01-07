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
// ----------------------------------------------------------------------------------------

// If ListGrid, or DynamicForm isn't loaded don't attempt to create this class - it's a requirement.
if (isc.ListGrid != null && isc.DynamicForm != null) {

// Utility class for picking foreignKey and includeFrom properties in the DataSourceEditor.
// Provide validDsNames on creation (or set them when showing the picker).
// Fires "changed(form, value)" when the combinedValue changes, returning it as dsName.fieldName
isc.ClassFactory.defineClass("DataSourceFieldPicker", "DynamicForm");

isc.DataSourceFieldPicker.addClassProperties({
    // The warning text where we can't guess foreign keys. 
    FOREIGN_KEY_WARNING: "Could not guess which foreignKey to use. " +
                          "Determine which of your fields is the foreign key, " +
                          "and make its foreignKey property point to a field in ",
                          
    getForeignKeyWarning : function(foreignDsName) {
        return isc.DataSourceFieldPicker.FOREIGN_KEY_WARNING + "'" + foreignDsName + "'.";
    }
});

isc.DataSourceFieldPicker.addProperties({
    // An array of valid DataSource names. The DataSources need not be loaded -- the picker
    // will lazily call isc.DS.load when necessary to get field names.
    // validDsNames: null,

    // An array of records representing all known DataSources, even if we don't know that they 
    // are valid yet. If chosen, they can be lazily validated via a callback.
    // If both validDsNames and allDSNames are provided, then we will offer both lists in the
    // drop down menu, with a separator in between.
    // allDsRecords: null,

    // The required base type of the field. Leave null if any field type is fine.
    // requiredBaseType: null,

    // Try to guess a foreign key with this datasource record. The record should contain
    // at least an ID and an array of field records
    // warnIfNoForeignKey: null,

    // The value of the dsName and fieldName, in the form dsName.fieldName
    // combinedValue: "",

    fields: [{
        name: "DataSource",
        type: "Select",

        // Restore default titleStyle, because the tool skin doesn't seem to
        // be applied consistently in the picker
        titleStyle: "formTitle",
        
        // Allow empty values so that the user can choose no DataSourceField
        allowEmptyValue: true,
       
        valueField: "ID",
        getClientPickListData : function() {
            return this.form.getDatasourcePickListData();
        },

        changed : function(form, item, value) {
            form.handleDsNameChanged(value);
        }
    },{
        name: "Field",
        type: "Select",
        
        // Restore default titleStyle, because the tool skin doesn't seem to
        // be applied consistently in the picker
        titleStyle: "formTitle",
        
        changed : function(form, item, value) {
            form.handleChanged();
        }
    }],

    getDatasourcePickListData : function () {
        // Setting validDsNames or allDsRecords will reset _datasourcePickListData, causing a
        // lazy recalculation here.
        if (!this._datasourcePickListData) {
            // Always allow an empty value!
            this._datasourcePickListData = [""];
            
            if (this.validDsNames && this.validDsNames.getLength() > 0) {
                this._datasourcePickListData.addList(this.validDsNames.map(function (dsName) {
                    return {ID: dsName};
                }));
            }

            if (this.allDsRecords && this.allDsRecords.getLength() > 0) {
                if (this._datasourcePickListData.getLength() > 0) {
                    this._datasourcePickListData.add({isSeparator: true});
                }

                // allDsRecords is already an array of records ...
                this._datasourcePickListData.addList(this.allDsRecords);
            }
        }

        return this._datasourcePickListData;
    },

    setValidDsNames : function(validDsNames) {
        this.validDsNames = validDsNames;
        this._datasourcePickListData = null;
    },

    setAllDsRecords : function(allDsRecords) {
        this.allDsRecords = allDsRecords;
        this._datasourcePickListData = null;
    },

    setWarnIfNoForeignKey : function(dsRecord) {
        this.warnIfNoForeignKey = dsRecord;
    },
    
    setCombinedValue : function(value) {
        var dsItem = this.getItem("DataSource");
        var fieldItem = this.getItem("Field");
        
        var parts = (value || "").split(".");
        dsItem.setValue(parts[0]);
        fieldItem.setValue(parts[1]);

        this.handleDsNameChanged(parts[0]);
    },

    getCombinedValue : function() {
        var value = this.getValue("DataSource");
        var fieldName = this.getValue("Field");
        if (fieldName) value = value + "." + fieldName;
        return value;
    },

    initWidget : function() {
        this.Super("initWidget", arguments);
        
        if (this.combinedValue) this.setCombinedValue(this.combinedValue);
    },

    _warnIfCannotGuessForeignKey : function (foreignDS) {
        var ourDsRec = this.warnIfNoForeignKey;
        
        if (!ourDsRec || !ourDsRec.fields) return;

        // If we have a field with a foreignKey defined that points to the foreignDS,
        // then we're fine.
        var foreignKeys = ourDsRec.fields.map(function (field) {
            return field.foreignKey ? field.foreignKey.split('.')[0] : null;
        });
        if (foreignKeys.contains(foreignDS.ID)) return;

        // We would also be fine if the foreignDS has a field with a foreignKey that
        // points back to us.
        foreignKeys = foreignDS.getFieldNames().map(function (fieldName) {
            var field = foreignDS.getField(fieldName);
            return field.foreignKey ? field.foreignKey.split('.')[0] : null;
        });
        if (foreignKeys.contains(ourDsRec.ID)) return;
    
        // We are also fine if there is a field in ourDsRec and foreignDsRec with
        // matching names
        var ourFieldNames = ourDsRec.fields.getProperty("name");
        var foreignFieldNames = foreignDS.getFieldNames();
        if (ourFieldNames.intersect(foreignFieldNames).getLength() > 0) return;
        
        // If we've gotten this far, then we can't guess the foreignKey. So, we'll
        // display a warning. Note that while we're using the standard error mechanism
        // for DyanmicForms, we don't actually prevent the update from occurring -- the
        // user can save this value if they like -- it's just a warning.
        this.addFieldErrors("DataSource", 
            isc.DataSourceFieldPicker.getForeignKeyWarning(this.getValue("DataSource")), 
            true);
    },

    // Callback when we load the live DS that corresponds to the dsName chosen
    // Can be called with null if loading the DS from the server has failed
    handleLiveDs : function(ds) {
        var fieldNames = [];
        var field = this.getField("Field");
        
        if (ds) {
            // Figure out the available field names
            fieldNames = ds.getFieldNames();
            if (this.requiredBaseType) {
                var self = this;
                fieldNames = fieldNames.findAll(function(field) {
                    var baseType = isc.SimpleType.getBaseType(ds.getField(field).type, ds);
                    return baseType == self.requiredBaseType;
                });
            }
            
            // If there is only one possible value, we may as well autodetect it
            if (fieldNames.getLength() == 1) {
                field.setValue(fieldNames[0]);
                this.handleChanged();
            }

            // If our current value isn't possible, then reset it
            if (!fieldNames.contains(field.getValue())) {
                field.setValue("");
                this.handleChanged();
            }

            // If we're checking for possible foreignKeys, then validate them
            if (this.warnIfNoForeignKey) this._warnIfCannotGuessForeignKey(ds);
        }

        field.setValueMap(fieldNames);
    },

    handleDsNameChanged : function(dsName) {
        // If value is empty, then may as well reset the field too
        if (!dsName) this.getField("Field").setValue("");
        
        // And fire the change event
        this.handleChanged();
       
        // Reset the value map for the fields. This will get filled in to
        // the available values when we get the live DS.
        this.getField("Field").setValueMap([]);
        
        // And reset any warnings about foreign keys -- that will also get
        // checked when we get the live DS.
        this.clearFieldErrors("DataSource", true);

        // Try to get the live DS that corresponds to the dsName chosen,
        // either synchronously or asynchronously
        if (dsName) {
            var ds = isc.DS.get(dsName);
            if (ds) {
                this.handleLiveDs(ds);
            } else {
                var self = this;
                isc.DS.load(dsName, function() {
                    ds = isc.DS.get(dsName);
                    if (!ds) self.logWarn("Loading dataSource from server was unsuccessful for " + dsName);
                    self.handleLiveDs(ds);
                }); 
            }
        }
    },

    handleChanged : function() {
        if (this.changed) this.changed(this, this.getCombinedValue());
    }
});

isc.DataSourceFieldPicker.registerStringMethods({
    // Fired when the combinedValue in the picker changes
    changed : "form, value"
});

//> @class DataSourceEditor
// Provides a UI for creating and editing +link{DataSource, DataSources).
// 
// @visibility devTools
//<
isc.ClassFactory.defineClass("DataSourceEditor", "VLayout");

isc.DataSourceEditor.addProperties({
// attributes 
overflow: "visible",


//> @attr dataSourceEditor.dataSource (DataSource or ID : null : IRW)
// DataSource being edited.
//
// @visibility devTools
//<

//> @attr dataSourceEditor.dsDataSource (DataSource or ID : null : IRW)
// DataSource to be used to load and save ds.xml files, via fileSource operations.
//
// @visibility devTools
//<

//> @attr dataSourceEditor.knownDataSources (Array : null : IRW)
// A list of all known DataSources, to be used when editing foreign keys.
// Each element of the array should be a record with at least ID and type properties.
//
// @visibility devTools
//<

//> @attr dataSourceEditor.mainEditor (AutoChild ComponentEditor : null : IRW)
//
// @visibility devTools
//<
mainEditorDefaults: {
    _constructor: "ComponentEditor",
    autoDraw:false,
    numCols:8,
    overflow:"visible",
//    backgroundColor: "black",
    dataSource:"DataSource",
    fields : [
        {name:"ID", title: "ID", required:true},
        {name:"dropExtraFields"},
        {name:"autoDeriveSchema"},
        //{name:"dataFormat", defaultValue:"iscServer", redrawOnChange:true},

        {type:"section", defaultValue:"XPath Binding", showIf:"values.dataFormat != 'iscServer'",
         itemIds:["dataURL", "selectBy", "recordXPath", "recordName"]},
        {name:"dataURL", showIf:"values.dataFormat != 'iscServer'"},
        {name:"selectBy", title:"Select Records By", 
         shouldSaveValue:false,
         valueMap:{ tagName:"Tag Name", xpath:"XPath Expression" },
         defaultValue:"xpath",
         redrawOnChange:true,
         // can't use tagName in JSON
         showIf:"values.dataFormat == 'xml'"},
        // allowed in XML or JSON
        {name:"recordXPath", 
         showIf:"values.dataFormat != 'iscServer' && form.getItem('selectBy').getValue() == 'xpath'"},
        // allow in XML only
        {name:"recordName", 
         showIf:"values.dataFormat == 'xml' && values.selectBy == 'tagName'"},

        {type:"section", defaultValue:"SQL Binding", 
         showIf:"values.serverType == 'sql' || values.serverType == 'hibernate'",
         itemIds:["dbName", "schema", "tableName", "quoteTableName", "beanClassName"]},
        {name:"dbName", showIf:"values.serverType == 'sql'"}, 
        {name:"schema", showIf:"values.serverType == 'sql'"}, 
        {name:"tableName", 
         showIf:"values.serverType == 'sql' || values.serverType == 'hibernate'"},
        {name:"quoteTableName", showIf:"values.serverType == 'sql'"}, 
        {name:"beanClassName", 
         showIf:"values.serverType == 'sql' || values.serverType == 'hibernate'"},

        {type:"section", defaultValue:"Record Titles", sectionExpanded:false,
         itemIds:["title", "pluralTitle", "titleField"]},
        {name:"title"},
        {name:"pluralTitle"},
        {name:"titleField"}
    ]
},

fieldEditorDefaults: {
    _constructor: "ListEditor",
    autoDraw:false,
    inlineEdit:true,
    dataSource:"DataSourceField",
    saveLocally:true,
    gridButtonsOrientation:"right",
    fields:[
        {name:"name", treeField: true,
            // Where includeFrom has been used, the name defaults to includeFrom's name.
            // So as well show that instead of nothing. We'll put it in italics to indicate
            // that it is special.
            //
            // In fact, we may as well show the includeFrom value in all cases (where
            // present) -- this will help avoid confusion where the name has been edited.
            formatCellValue : function(value, record, rowNum, colNum, grid) {
                var formattedValue = this._nameFromValueOrIncludeFrom(value, record.includeFrom);                                
                if (record.includeFrom) {
                    formattedValue +=" <i>[" + record.includeFrom + "]</i>";
                }
                return formattedValue;
            },

            // If the value is present, return it. Otherwise, return the last
            // part of the includeFrom -- which is what the name defaults to.
            _nameFromValueOrIncludeFrom : function(value, includeFrom) {
                if (value) {
                    return value;
                } else {
                    var dotIndex = includeFrom.lastIndexOf(".");
                    if (dotIndex == -1) {
                        return value;
                    } else {
                        return includeFrom.substring(dotIndex + 1);
                    }
                }
            },

            // Note that name is *required* in the schema. This isn't literally true
            // anymore, since now name is optional when includesFrom is specified.
            // We could make it optional in the schema, but that may cause difficulties
            // elsewhere. So, for the moment, we're doing some massaging here.
            //
            // For editing, we'll display the last part of the includeFrom if the name
            // is blank -- that is what the default actually is, so it is a reasonable
            // starting point for editing.
            formatEditorValue : function(value, record, form, item) {
                return this._nameFromValueOrIncludeFrom(value, record.includeFrom);
            },

            // If the user blanks the value, it would normally be an error (since
            // the name is required. So, let's simply supply the default in that
            // case -- that is, use the includeFrom's name. The alternative would
            // be to allow the blank, but that would mean changing the schema so that
            // name would not be required.
            parseEditorValue : function(value, record, rowNum, colNum, grid) {
                return this._nameFromValueOrIncludeFrom(value, record.includeFrom);
            }
        },
        {name:"title"},
        {name:"type", width:60},
        {name:"required", title:"Req.", width:40, canToggle:true},
        {name:"hidden", width:40},
        {name:"length", width:60},
        {name:"primaryKey", title:"is PK", width:40}
    ],
    formProperties: { 
        numCols:4,
        initialGroups:10
    },
    formFields : [
        {name:"name", canEdit:false},
        {name:"type"},
        {name:"title"},
        {name:"primaryKey"},
        {name:"valueXPath", colSpan:2, 
            showIf:function () {
                var grid = this.form.creator,
                    mainEditor = grid ? grid.creator.mainEditor : null;
                return (mainEditor && mainEditor.getValues().dataFormat != 'iscServer');
                
            }
        },

        {type:"section", defaultValue:"Value Constraints",
         itemIds:["required", "length", "valueMap"] },
        {name:"valueMap", rowSpan:2},
        {name:"required"},
        {name:"length"},

        {type:"section", defaultValue:"Component Binding", 
         itemIds:["hidden", "detail", "canEdit"] },
        {name:"canEdit"},
        {name:"hidden"},
        {name:"detail"},

        {type:"section", defaultValue:"Relations", sectionExpanded:true,
         itemIds:["foreignKey", "rootValue", "includeFrom"] },
        {
            name: "foreignKey",
            type: "staticText",

            showPickerIcon: true,
            pickerConstructor: "DataSourceFieldPicker",
            pickerProperties: {
                width : 160,
                changed : function(form, value) {
                    form.creator.setValue(value);
                }
            },

            // Override showPicker to set up the valid DataSources based on whatever
            // edits we've done.
            showPicker : function() { 
                // Look up the creator chain for the DataSourceEditor
                var dsEditor = this;
                while (dsEditor && !isc.isA.DataSourceEditor(dsEditor)) dsEditor = dsEditor.creator;
                if (!dsEditor) {
                    this.logWarn("Could not find the DataSourceEditor");
                    return;
                }
                if (!dsEditor.knownDataSources) {
                    this.logWarn("DataSourceEditor.knownDataSources has not been set");
                    return;
                }

                // Actually show the picker and set the valid Datasources
                this.Super("showPicker", arguments);    
                
                var dsData = dsEditor.getDatasourceData();
                
                var validDS = dsEditor.knownDataSources.findAll({dsType: dsData.serverType});
                this.picker.setValidDsNames(validDS.getProperty("ID"));

                // Try to get the live DS we are editing, in case there are types defined on the DS
                var ds = isc.DS.get(dsData.ID);
                this.picker.requiredBaseType = isc.SimpleType.getBaseType(this.form.getValue("type"), ds);

                this.picker.setCombinedValue(this.getValue());
            },

            // Pickers aren't destroyed by default, so we'll do that here
            destroy : function() {
                if (this.picker) this.picker.destroy();
                this.Super("destroy", arguments);
            }
        },
        {name:"rootValue"},
        {
            name: "includeFrom", 
            type: "staticText", 

            showPickerIcon: true, 
            pickerConstructor: "DataSourceFieldPicker",
            pickerProperties: {
                changed : function(form, value) {
                    form.creator.setValue(value);
                }
            },

            // Override showPicker to set up the valid DataSources based on whatever
            // edits we've done.
            showPicker : function() { 
                // Look up the creator chain for the DataSourceEditor
                var dsEditor = this;
                while (dsEditor && !isc.isA.DataSourceEditor(dsEditor)) dsEditor = dsEditor.creator;
                if (!dsEditor) {
                    this.logWarn("Could not find the DataSourceEditor");
                    return;
                }

                var dsData = dsEditor.getDatasourceData();

                // The known valid datasources are the ones for which we have a foreignKey
                // defined, since includeFrom only works one level at a time. So, as a first
                // approximation, we can just collect the foreignKey's we've defined for this
                // dataSource. Note that we get them from the form, rather than the real
                // dataSource, so that we can immediately react to any changes.
                var editedFieldData = dsData.fields;                

                var fieldsWithForeignKeys = editedFieldData.findAll(function (field) {
                    return field.foreignKey;
                }) || [];
                var validDsNames = fieldsWithForeignKeys.map(function (field) {
                    return field.foreignKey.split(".")[0];
                }).getUniqueItems();

                // It is also possible that foreignKeys can be guessed for other datasources,
                // but we won't know until they are chosen and lazily loaded. So, we also
                // supply a list of datasources of the same type as ours
                var allDsRecords = null;
                if (dsEditor.knownDataSources) {
                    var ourType = dsData.serverType;
                    if (ourType) {
                        allDsRecords = dsEditor.knownDataSources.findAll({dsType: ourType});
                    } else {
                        allDsRecords = dsEditor.knownDataSources;
                    }
                }

                // Actually show the picker (possibly creating it)
                this.Super("showPicker", arguments);
    
                this.picker.setValidDsNames(validDsNames);
                if (allDsRecords) this.picker.setAllDsRecords(allDsRecords);
                            
                // In order to allow validation of lazily loaded datasources, we provide
                // our ID and field records
                this.picker.setWarnIfNoForeignKey(dsData);
                
                this.picker.setCombinedValue(this.getValue());
            },

            // Pickers aren't destroyed by default, so we'll do that here
            destroy : function() {
                if (this.picker) this.picker.destroy();
                this.Super("destroy", arguments);
            }
        }
    ],
    gridDefaults:{ 
        editEvent:"click",
        
        listEndEditAction:"next",
        autoParent:"gridLayout",
        selectionType:isc.Selection.SINGLE,
        recordClick:"this.creator.recordClick(record)",
        modalEditing:true,
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
        },
        // get rid of default LG borders
        styleName:"rightBorderOnly",
        validateByCell:true,
        leaveScrollbarGap:false,
        alternateRecordStyles:true,
        // show a delete column
        canRemoveRecords:true,
        canEdit: true,
        canEditCell : function (rowNum, colNum) {
            var record = this.getRecord(rowNum),
                field = this.getField(colNum),
                fieldName = field[this.fieldIdProperty],
                isNameOrTitle = (fieldName == "name" || fieldName == "title");
            if (isc.isA.TreeGrid(this)) {
                if (record.isFolder &&
                !(isNameOrTitle || fieldName == "required" || fieldName == "hidden")) {
                    return false;
                }
            }
            else {
                if (this.getDataSource().fieldIsComplexType(field) && !isNameOrTitle) 
                    return false;
            }
            return this.Super('canEditCell', arguments);
        }

    },

    newRecord : function () {
        if (this.creator.canEditChildSchema) {
            var grid = this.grid,
                tree = grid.data,
                selectedNode = this.getSelectedNode();
                
            if (!selectedNode) selectedNode = tree.root;
            var parentNode = tree.getParent(selectedNode)

            if (selectedNode) {
                if (!selectedNode.isFolder) selectedNode = parentNode;
                var newNode = { 
                    name: this.getNextUniqueFieldName(selectedNode, "field"),
                    id: this.getNextUnusedNodeId(),
                    parentId: selectedNode ? selectedNode.id : null
                };
                this.addNode(newNode, selectedNode);
            }
        } else this.Super("newRecord", arguments);
    },
    getSelectedNode : function () {
        return this.grid.getSelectedRecord();
    },
    addNode : function (newNode, parentNode) {
        var tree = this.grid.data;

        tree.linkNodes([newNode]);
    },
    getNextUniqueFieldName : function (node, prefix) {
        var childFields = node ? node.fields || [] : [],
	        inc=1;

        if (!prefix || prefix.length == 0) prefix = "field";
        if (childFields && childFields.length > 0) {
            for (var i = 0; i < childFields.length; i++) {
                var item = childFields.get(i), 
                    itemName = item.name;
                if (itemName.substring(0, prefix.length) == prefix && itemName.length > prefix.length) {
                    var thisInc = parseInt(itemName.substring(prefix.length));
                    if (!isNaN(thisInc) && thisInc >= inc) 
                        inc = thisInc+1;
                }
            }
        }
        return prefix + inc;
    },
    getNextUnusedNodeId : function () {
        var tree = this.grid.data;
        for (var i = 1; i<10000; i++) {
            var item = tree.findById(i);
            if (!item) return i;
        }
        return 1;
    }
},

newButtonDefaults:{
    _constructor:isc.AutoFitButton,
    autoParent:"gridButtons",
    title: "New Field",
    click:"this.creator.newRecord()"
},

moreButtonDefaults:{
    _constructor:isc.AutoFitButton,
    autoParent:"gridButtons",
    click:"this.creator.editMore()",
    disabled:true
},

buttonLayoutDefaults: {
    _constructor: "HLayout",
    width: "100%",
    membersMargin: 5
},

saveButtonDefaults: {
    _constructor: "IButton",
    autoDraw: false,
    title: "Save",
    autoFit: true,
    autoParent: "buttonLayout",
    click: function(){
        var valid=true;
        if (this.creator.showMainEditor != false) valid = this.creator.mainEditor.validate();
        var fieldEditor = this.creator.fieldEditor;
        if (valid && fieldEditor.validate()) {
            if (fieldEditor.isVisible()) {
                fieldEditor.saveRecord();
            }
            this.creator.save();
        }
    }
},

addTestDataButtonDefaults: {
    _constructor: "IButton",
    autoDraw: false,
    title: "Add Test Data",
    autoFit: true,
    click: function(){
        var dsData = isc.addProperties({}, 
            this.creator.mainEditor ? this.creator.mainEditor.getValues() : this.creator.mainEditorValues
        )
        var dataImportDialog = isc.DataImportDialog.create({
            ID: "dataImportDialog",
            targetDataSource: dsData.ID
        });
        dataImportDialog.show();
    }
},

addChildButtonDefaults: {
    _constructor: "IButton",
    autoDraw: false,
    title: "Add Child Object",
    autoFit: true,
    click: function() {
        var editor = this.creator.fieldEditor,
            grid = editor.grid,
            tree = grid.data,
            selectedNode = grid.getSelectedRecord() || tree.root,
            parentNode = tree.getParent(selectedNode),
            newNode = {
                isFolder: true,
                children: [],
                multiple: true,
                childTagName: "item"
            }
        ;

        if (selectedNode) {
            if (!selectedNode.isFolder) selectedNode = parentNode;
            newNode.name = editor.getNextUniqueFieldName(selectedNode, "child"),
            newNode.id = editor.getNextUnusedNodeId(),
            newNode.parentId = selectedNode.id;
            tree.linkNodes([newNode], parentNode);
            tree.openFolder(newNode);
        }

    }
},

mainStackDefaults: {
    _constructor: "SectionStack",
    overflow: "visible",
    width: "100%", height:"100%",
    visibilityMode: "multiple"
},

instructionsSectionDefaults: {
    _constructor: "SectionStackSection",
    title: "Instructions",
    expanded:true, canCollapse:true
},

instructionsDefaults: {
    _constructor: "HTMLFlow", 
    autoFit:true,
    padding:10
},

mainSectionDefaults: {
    _constructor: "SectionStackSection",
    title:"DataSource Properties", 
    expanded:true, canCollapse:false, showHeader: false
},

fieldSectionDefaults: {
    _constructor: "SectionStackSection",
    title:"DataSource Fields &nbsp;<span style='color:#BBBBBB'>(click to edit or press New)</span>", 
    expanded:true, canCollapse: true
},

deriveFieldsSectionDefaults: {
    _constructor: "SectionStackSection",
    title:"Derive Fields From SQL",
    expanded:false, canCollapse: true
},

bodyProperties:{
    overflow:"auto",
//    backgroundColor: "black",
    layoutMargin:10
},

deriveFormDefaults: {
    _constructor: "DynamicForm"
},

previewGridDefaults: {
    _constructor: "ListGrid",
    showFilterEditor: true    
},

// properties
canEditChildSchema: false,
canAddChildSchema: false,

// methods
editNew : function (dataSource, callback, instructions) {
    this.addTestDataButton.hide();
    if (dataSource.defaults) {
        this.paletteNode = dataSource;
        this.start(dataSource.defaults, callback, true, instructions);
    } else {
        this.start(dataSource, callback, true, instructions);
    }
},
    
editSaved : function (dataSource, callback, instructions) {
    this.addTestDataButton.show();
    this.start(dataSource, callback, false, instructions);
},

start : function (dataSource, callback, isNew, instructions) {
    if (instructions) {
        this.mainStack.showSection(0);
        this.instructions.setContents(instructions);
    } else { 
        this.mainStack.hideSection(0);
    }

    if (this.mainEditor) this.mainEditor.clearValues();
    if (this.fieldEditor) this.fieldEditor.setData(null);

    // to be called when editing completes
    this.saveCallback = callback;

    this.logWarn("editing " + (isNew ? "new " : "" ) + 
                 "DataSource: " + this.echo(dataSource));

    if (!dataSource) {       
        // no initial dataSource properties at all, start editing from scratch 
        return this.show(); 
    }

    this.dsClass = dataSource.Class;
    if (isNew) {
        // dataSource has never been saved
        if (isc.isA.DataSource(dataSource)) {
            // serializeableFields picks up the fields data - also pick up the
            // sfName if it's defined
            var sfName = dataSource.sfName;
            // currently used only for web service / SalesForce pathways, where we
            // dynamically retrieve a DataSource generated from XML schema.
            dataSource = dataSource.getSerializeableFields();
            if (sfName) dataSource.sfName = sfName;
            
            this.logWarn("editing new DataSource from live DS, data: " + 
                         this.echo(dataSource));
        } else {
            dataSource.ID = this.getUniqueDataSourceID();
        }
        this._startEditing(dataSource);
    } else {
        // we need the clean initialization data for this DataSource (the live data
        // contains various derived state)
        var self = this;

        this.dsDataSource.getFile({
            fileName: dataSource.ID,
            fileType: "ds",
            fileFormat: "xml"
        }, function (dsResponse, data, dsRequest) {
            isc.DMI.callBuiltin({
                methodName: "xmlToJS",
                arguments: [data],
                callback : function (rpcResponse, data) {
                    self._loadSchemaReply(data);
                }
            });
        });
    }
},

// override point to provide a unique datasource-id
getUniqueDataSourceID : function () {
    return "newDataSource";
},

_loadSchemaReply : function (data) {
    //!OBFUSCATEOK
    // instantiate the DataSource in "captureDefaults" mode, where Class.create()
    // returns a editComponent instead
    isc.captureDefaults = true;
    var dsComponent = isc.eval(data);
    isc.captureDefaults = null;

    var defaults = dsComponent.defaults;
    this.logWarn("captured DS defaults: " + this.echo(defaults));

    // do some automatic defaulting otherwise done at DataSource.init()
    if (defaults.serverType == "sql") defaults.dataFormat = "iscServer";
    if (defaults.recordXPath != null && defaults.dataFormat == null) {
        defaults.dataFormat = "xml";
    }

    this._startEditing(defaults);
},
_startEditing : function (defaults) {
    if (this.mainEditor) this.mainEditor.setValues(defaults);
    else this.mainEditorValues = defaults;
    var fields = defaults.fields;

    if (!isc.isAn.Array(fields)) fields = isc.getValues(defaults.fields);

    if (this.fieldEditor) {
        if (this.canEditChildSchema) {
            this.setupIDs(fields, 1, null);

            var tree = isc.Tree.create({
                modelType: "parent",
                childrenProperty: "fields",
                titleProperty: "name",
                idField: "id",
	            nameProperty: "id",
                root: { id: 0, name: "root"},
                data: fields
            });
            tree.openAll();
            this.fieldEditor.setData(tree);
        } else this.fieldEditor.setData(fields);
        this.fieldEditor.formLayout.hide();
        this.fieldEditor.gridLayout.show();
    }
    this.show();
},

setupIDs : function (fields, nextId, parentId) {
    var index=nextId,
        item,
        subItem
    ;

    if (!index) index = 1;
    for (var i = 0; i < fields.length; i++) {
        var item = fields.get(i);
        item.parentId = parentId;
        item.id = index++;
        if (item.fields) {
            if (!isc.isAn.Array(item.fields)) item.fields = isc.getValues(item.fields);
            index = this.setupIDs(item.fields, index, item.id);
        }
    }
    return index;
},

getDatasourceData : function () {
    // NOTE: dsClass is set when we begin editing
    var dsClass = this.dsClass || "DataSource",
        dsData = isc.addProperties({}, 
            this.mainEditor ? this.mainEditor.getValues() : this.mainEditorValues
        )
    ;
    
    if (this.canEditChildSchema) {
        var tree = this.fieldEditor.grid.data,
            fields = tree.getCleanNodeData(tree.getRoot(), true).fields;

        dsData.fields = this.getExtraCleanNodeData(fields);
    } else { 
        dsData.fields = this.fieldEditor.getData();
    }

    return dsData;
},

save : function () {
    var dsData = this.getDatasourceData();

    if (dsData.serverType == "sql" || dsData.serverType == "hibernate") {
        if (!dsData.fields.getProperty("primaryKey").or()) {
            isc.warn("SQL / Hibernate DataSources must have a field marked as the primary key");
            return; 
        }
    }

    // Possibly hacky fix for a problem saving these values when they are null ...
    ["dbName", "schema", "tableName", "quoteTableName", "beanClassName", "dropExtraFields", "autoDeriveSchema"].map(function (removeNull) {
        if (dsData[removeNull] == null) delete dsData[removeNull];
    });
        
    // And remove _constructor: DatabaseBrowser if present ... not sure where that comes from
    if (dsData._constructor == "DatabaseBrowser") delete dsData._constructor;

    this.doneEditing(dsData);

},

getExtraCleanNodeData : function (nodeList, includeChildren) {
    if (nodeList == null) return null;

    var nodes = [], 
        wasSingular = false;
    if (!isc.isAn.Array(nodeList)) {
        nodeList = [nodeList];
        wasSingular = true;
    }

    for (var i = 0; i < nodeList.length; i++) {
        var treeNode = nodeList[i],
            node = {};
        // copy the properties of the tree node, dropping some further Tree/TreeGrid artifacts
		for (var propName in treeNode) {
            if (propName == "id" || propName == "parentId" || propName == "isFolder") continue;

            node[propName] = treeNode[propName];

            // Clean up the children as well (if there are any)
            if (propName == this.fieldEditor.grid.data.childrenProperty && isc.isAn.Array(node[propName])) {
                node[propName] = this.getExtraCleanNodeData(node[propName]);
            }
        }
        nodes.add(node);
    }
    if (wasSingular) return nodes[0];
    return nodes;
},

doneEditing : function (dsData) {
    // handle custom subclasses of DataSource for which there is no schema defined by
    // serializing based on the DataSource schema but adding the _constructor property to
    // get the correct class.
    // XXX problem: if you ask an instance to serialize itself, and there is no schema for
    // it's specific class, it uses the superClass schema but loses it's Constructor
    // XXX we to preserve the class, we need to end up with the "constructor" property set
    // in XML, but this has special semantics in JS
    var dsClass = this.dsClass || "DataSource",
        schema;
    if (isc.DS.isRegistered(dsClass)) {
        schema = isc.DS.get(dsClass);
    } else {
        schema = isc.DS.get("DataSource");
        dsData._constructor = dsClass;
    }

    // explicit class properties:
    // - in XML: "constructor" or xsi:type in instances, or "instanceConstructor" in schema
    // - for ClassFactory.newInstance(): _constructor

    // serialize to XML and save to server
    var xml = schema.xmlSerialize(dsData);
    this.logWarn("saving DS with XML: " + xml);
  
    this.dsDataSource.saveFile({
        fileName: dsData.ID,
        fileType: "ds",
        fileFormat: "xml"
    }, xml);

    // create a live instance
    var liveDS = isc.ClassFactory.getClass(dsClass).create(dsData);

    // fire the callback passed in when editing began
    this.fireCallback(this.saveCallback, "dataSource", [liveDS]);
    this.saveCallback = null;
},
clear : function () {
    if (this.mainEditor) this.mainEditor.clearValues();
    else this.mainEditorValues = null;
    this.fieldEditor.setData([]);
},

initWidget : function () {
    this.Super('initWidget', arguments);

    this.addAutoChildren(["mainStack", "instructions", "mainEditor", "buttonLayout"]);
    this.buttonLayout.addMember(this.createAutoChild("saveButton"));
    this.addTestDataButton = this.createAutoChild("addTestDataButton");
    this.buttonLayout.addMember(this.addTestDataButton);

    if (this.dsDataSource) this.dsDataSource = isc.DataSource.get(this.dsDataSource);
    
    if (this.canAddChildSchema) {
        this.canEditChildSchema = true;
        this.addAutoChild("addChildButton");
    }

    this.addAutoChild("fieldEditor", {
        // NOTE: provided dynamically because there's currently a forward dependency: DataSourceEditor is
        // defined in ISC_DataBinding but ComponentEditor is defined in ISC_Tools
        formConstructor:isc.TComponentEditor || isc.ComponentEditor,
		gridConstructor: this.canEditChildSchema ? isc.TreeGrid : isc.ListGrid,
        showMoreButton: this.showMoreButton,
        newButtonTitle: "New Field",
		newButtonDefaults: this.newButtonDefaults,
        newButtonProperties: this.newButtonProperties,
		moreButtonDefaults: this.moreButtonDefaults,
        moreButtonProperties: this.moreButtonProperties
    });
    this.moreButton = this.fieldEditor.moreButton;
    this.newButton = this.fieldEditor.newButton;

    if (this.canAddChildSchema) this.fieldEditor.gridButtons.addMember(this.addChildButton);

    var stack = this.mainStack;

    stack.addSections([isc.addProperties(this.instructionsSectionDefaults,
        this.instructionsSectionProperties,
        { items:[this.instructions] }
    )]);

	stack.addSections([isc.addProperties(this.mainSectionDefaults,
        this.mainSectionProperties,
        { items:[this.mainEditor] }
    )]);
    if (this.showMainEditor==false) stack.hideSection(1);

    stack.addSections([isc.addProperties(this.fieldSectionDefaults,
        this.fieldSectionProperties,
        { items:[this.fieldEditor] }
    )]);


    var _this = this;
    this.deriveForm = this.createAutoChild("deriveForm", {
        fields: [
            {name: "sql", showTitle: false, formItemType: "AutoFitTextAreaItem",
             width: "*", height: 40, colSpan: "*",
             keyPress:function (item, form, keyName) {
                if (keyName == 'Enter' && isc.EH.ctrlKeyDown()) {
                   if (isc.Browser.isSafari) item.setValue(item.getElementValue());
                   _this.execSQL();
                   if (isc.Browser.isSafari) return false;
                }
            }},
            {type: "button", title: "Execute", startRow: true, click: this.getID()+".execSQL()"}
        ]
    });

    /*
    // disabled - would need to add some instructions and error handling before this can be shown
    stack.addSections([isc.addProperties(this.deriveFieldsSectionDefaults,
        this.deriveFieldsSectionProperties,
        { items:[this.deriveForm] }
    )]);

    //this.operationsGrid = this.createAutoChild("operationsGrid");
    //stack.addSection({ID: "operationsSection", title: "Operations", expanded: false, items: [this.operationsGrid]});

    this.previewGrid = this.createAutoChild("previewGrid");
    stack.addSection({ID: "previewSection", title: "Preview", expanded: false, items: [this.previewGrid]});
    */

    stack.addSections({expanded: true, showHeader: false, items: [this.saveButton]});

},

execSQL : function () {
    var sql = this.deriveForm.getValue("sql");
    if (sql) {
        // strip whitespaces and trailing semicolons - these produce a syntax error when passed
        // to the JDBC tier
        sql = sql.trim().replace(/(.*);+/, "$1");
        var ds = isc.DataSource.get("DataSourceStore");
        ds.performCustomOperation("dsFromSQL", {dbName: this.mainEditor.getValue("dbName"), sql: sql}, this.getID()+".deriveDSLoaded(data)");
    }
},

deriveDSLoaded : function (data) {
    var ds = data.ds;
    this.dsLoaded(data.ds);
},

dsLoaded : function (dsConfig) {
    var ds = isc.DataSource.create(dsConfig);
    this.currentDS = ds;

    this.deriveFields(ds);
    this.previewGrid.setDataSource(ds);

    /* 
    var ob = ds.operationBindings;
    if (ob && ob.length > 0) {
        this.fetchOperationForm.setValues(ob[0]);
    }
    */
},

deriveFields : function (ds) {
    var fields = ds.getFieldNames();

    var newFields = [];
    for (var i = 0; i < fields.length; i++) {
        var fieldName = fields[i]
        var field = {};
        var dsField = ds.getField(fieldName);
        for (var key in dsField) {
            if (isc.isA.String(key) && key.startsWith("_")) continue;
            field[key] = dsField[key];
        }
        newFields.add(field);
    }

    var tree = isc.Tree.create({
        modelType: "parent",
        childrenProperty: "fields",
        titleProperty: "name",
        idField: "id",
	    nameProperty: "id",
        root: { id: 0, name: "root"},
        data: newFields
    });
    this.fieldEditor.setData(tree);
}


});

}
