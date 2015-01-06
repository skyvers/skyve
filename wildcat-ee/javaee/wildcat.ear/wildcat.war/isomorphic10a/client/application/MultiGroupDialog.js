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

 




//> @object GroupSpecifier
// A Javascript object defining the details of a single group operation.
//
// @treeLocation Client Reference/Data Binding
// @visibility external
//< 

//> @attr groupSpecifier.property (String : null : IR)
// The property name, eg a +link{ListGridField, field name}, to which this groupSpecifier applies.
//
// @visibility external
//< 

//> @attr groupSpecifier.grouping (String : null : IR)
// The grouping in which this specifier should group.
//
// @visibility external
//< 

//> @attr groupSpecifier.normalizer (Function : null : IR)
// A normalizer function which this groupSpecifier will use to group.
//
// @visibility external
//< 

//> @attr groupSpecifier.context (DataBoundComponent : null : IR)
// A DataBoundComponent providing the context for the group-normalizer.
//
// @visibility external
//< 


//> @class MultiGroupPanel
// A widget that allows the user to set up complex grouping arrangements by defining a group of
// +link{GroupSpecifier}s.
// <P>
// Each +link{GroupSpecifier} applies to a single property and grouping - so, for instance, in 
// a grid with two columns, <code>Nationhood</code> and <code>Country</code>, you could group first 
// by <code>Nationhood</code> with its selected groupingMode and then by <code>Country</code> with its selected groupingMode. 
// 
// @treeLocation Client Reference/Data Binding
// @visibility external
//<
isc.defineClass("MultiGroupPanel", "Layout");

isc.MultiGroupPanel.addProperties({
    vertical: true,
    overflow: "visible",

    //> @attr multiGroupPanel.fields (Array of Field : null : IR)
    // The list of fields which the user can choose to group by.
    // @visibility external
    //<

    // i18n text constants
    //> @attr multiGroupPanel.addLevelButtonTitle (String : "Add Level" : IR)
    // The title-text to appear on the addLevelButton
    // @visibility external
    // @group i18nMessages
    //<
    addLevelButtonTitle: "Add Level",
    //> @attr multiGroupPanel.deleteLevelButtonTitle (String : "Delete Level" : IR)
    // The title-text to appear on the deleteLevelButton
    // @visibility external
    // @group i18nMessages
    //<
    deleteLevelButtonTitle: "Delete Level",
    //> @attr multiGroupPanel.copyLevelButtonTitle (String : "Copy Level" : IR)
    // The title-text to appear on the copyLevelButton
    // @visibility external
    // @group i18nMessages
    //<
    copyLevelButtonTitle: "Copy Level",

    //> @attr multiGroupPanel.invalidListPrompt (String : "Columns may only be used once: '\${title}' is used multiple times." : IR)
    // This is a dynamic string - text within <code>\${...}</code> will be evaluated as JS code
    // when the message is displayed.
    // <P>
    // Default value returns <P>
    // <code>
    // <i>Columns may only be used once: <code>[some field's title]</code> is used multiple times</i>
    // </code>
    // @visibility external
    // @group i18nMessages
    //<
    invalidListPrompt: "Columns may only be used once: '\${title}' is used multiple times.",

    //> @attr multiGroupPanel.propertyFieldTitle (String : "Column" : IR)
    // The title-text to appear in the header of the "property" field.
    // @visibility external
    // @group i18nMessages
    //<
    propertyFieldTitle: "Column",

    //> @attr multiGroupPanel.groupingFieldTitle (String : "Grouping" : IR)
    // The title-text to appear in the header of the "grouping" field.
    // @visibility external
    // @group i18nMessages
    //<
    groupingFieldTitle: "Grouping",

    //> @attr multiGroupPanel.firstGroupLevelTitle (String : "Group by" : IR)
    // The title-text to appear in the first column for the first group-level.
    // @visibility external
    // @group i18nMessages
    //<
    firstGroupLevelTitle: "Group by",

    //> @attr multiGroupPanel.otherGroupLevelTitle (String : "Then by" : IR)
    // The title-text to appear in the first column for all group-levels other than the first.
    // @visibility external
    // @group i18nMessages
    //<
    otherGroupLevelTitle: "Then by",
    
    topLayoutDefaults: {
        _constructor: "HLayout",
        overflow: "visible",
        height: 22,
        align: "left",
        membersMargin: 5,
        extraSpace: 5
    },

    //> @attr multiGroupPanel.addLevelButton (AutoChild IButton : null : RA)
    // Automatically generated +link{class:IButton} providing a mechanism for adding new levels
    // to the group configuration.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>multiGroupPanel.addLevelButtonProperties</code> and 
    // <code>multiGroupPanel.addLevelButtonDefaults</code>.
    //
    // @visibility external
    //<
    addLevelButtonDefaults: {
        _constructor: "IButton",
        icon: "[SKINIMG]actions/add.png",
        autoFit: true,
        height: 22,
        showDisabled: false,
        autoParent: "topLayout",
        click: "this.creator.addLevel()"
    },

    //> @attr multiGroupPanel.deleteLevelButton (AutoChild IButton : null : RA)
    // Automatically generated +link{class:IButton} providing a mechanism for deleting levels
    // from the group configuration.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>multiGroupPanel.deleteLevelButtonProperties</code> and 
    // <code>multiGroupPanel.deleteLevelButtonDefaults</code>.
    //
    // @visibility external
    //<
    deleteLevelButtonDefaults: {
        _constructor: "IButton",
        icon: "[SKINIMG]actions/remove.png",
        autoFit: true,
        height: 22,
        showDisabled: false,
        autoParent: "topLayout",
        click: "this.creator.deleteSelectedLevel()"
    },

    //> @attr multiGroupPanel.copyLevelButton (AutoChild IButton : null : RA)
    // Automatically generated +link{class:IButton} providing a mechanism for duplicating levels
    // in the group configuration.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>multiGroupPanel.copyLevelButtonProperties</code> and 
    // <code>multiGroupPanel.copyLevelButtonDefaults</code>.
    //
    // @visibility external
    //<
    copyLevelButtonDefaults: {
        _constructor: "IButton",
        icon: "[SKINIMG]RichTextEditor/copy.png",
        autoFit: true,
        height: 22,
        showDisabled: false,
        autoParent: "topLayout",
        click: "this.creator.copySelectedLevel()"
    },

    //> @attr multiGroupPanel.levelUpButtonTitle (String : "Move Level Up" : IR)
    // The hover-prompt for the Level Up button.
    // @visibility external
    // @group i18nMessages
    //<
    levelUpButtonTitle: "Move Level Up",

    //> @attr multiGroupPanel.levelUpButton (AutoChild ImgButton : null : RA)
    // Automatically generated +link{class:ImgButton} providing a mechanism for moving existing
    // group-levels up in the group configuration.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>multiGroupPanel.levelUpButtonProperties</code> and 
    // <code>multiGroupPanel.levelUpButtonDefaults</code>.
    //
    // @visibility external
    //<
    levelUpButtonDefaults: {
        _constructor: "ImgButton",
        //src: "[SKINIMG]common/arrow_up.gif",
        src: "[SKINIMG]TransferIcons/up.png",
        height: 22,
        width: 24,
        imageType: "center",
        showDisabled: false,
        showRollOver: false,
        showDown: false,
        showFocused: false,
        autoParent: "topLayout",
        click: "this.creator.moveSelectedLevelUp()"
    },

    //> @attr multiGroupPanel.levelDownButtonTitle (String : "Move Level Down" : IR)
    // The hover-prompt for the Level Down button.
    // @visibility external
    // @group i18nMessages
    //<
    levelDownButtonTitle: "Move Level Down",

    //> @attr multiGroupPanel.levelDownButton (AutoChild ImgButton : null : RA)
    // Automatically generated +link{class:ImgButton} providing a mechanism for moving existing
    // group-levels down in the group configuration.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>multiGroupPanel.levelDownButtonProperties</code> and 
    // <code>multiGroupPanel.levelDownButtonDefaults</code>.
    //
    // @visibility external
    //<
    levelDownButtonDefaults: {
        _constructor: "ImgButton",
        //src: "[SKINIMG]common/arrow_down.gif",
        src: "[SKINIMG]TransferIcons/down.png",
        height: 22,
        width: 24,
        imageType: "center",
        showDisabled: false,
        showRollOver: false,
        showDown: false,
        showFocused: false,
        autoParent: "topLayout",
        click: "this.creator.moveSelectedLevelDown()"
    },

    //> @attr multiGroupPanel.optionsGrid (AutoChild ListGrid : null : IR)
    // Automatically generated +link{class:ListGrid} allowing the user to configure a set of 
    // +link{GroupSpecifier}s.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>multiGroupPanel.optionsGridProperties</code> and 
    // <code>multiGroupPanel.optionsGridDefaults</code>.
    //
    // @visibility external
    //<
    optionsGridDefaults: {
        _constructor: "ListGrid",
        width:"100%",
        height: "*",
        canSort: false,
        canReorderFields: false,
        canResizeFields: false,
        canEdit: true,
        canEditNew: true,
        selectionType: "single",
        selectionProperty: "_selection_1",
        canEditCell : function (rowNum, colNum) {
            var record = this.getRecord(rowNum);
            if (colNum == 2) {
                if (!this.creator.verifyGroupingModeAvailableField(record, rowNum)) return false;
            }
            // use default rules for all other fields
            return this.Super("canEditCell", arguments);
        },
        recordDoubleClick : function (viewer, record, recordNum, field, fieldNum, value, rawValue) {
            this.creator.getGroupingModeAvailableField(record, recordNum);
        },
        defaultFields: [
            { name: "groupSequence", title: "&nbsp;", showTitle: false, canEdit: false, width: 80, canHide: false,
                showDefaultContextMenu: false,
                formatCellValue : function (value, record, rowNum, colNum, grid) {
                    return rowNum == 0 ? grid.creator.firstGroupLevelTitle : 
                        grid.creator.otherGroupLevelTitle;
                }
            },
            { name: "property", title: " ", type: "select",
                defaultToFirstOption: true,
                showDefaultContextMenu: false,
                changed:"item.grid.creator.getGroupingModeAvailableField(null, null)"
            },
            { name: "grouping",  title: " ", width: 100,
                showDefaultContextMenu: false,
                defaultToFirstOption: true,
                type: "text", editorType: "select",
                displayField: "groupingTitle",
                changed : function (form, item, value) {
                    var vm = item.valueMap || item.editorValueMap;
                    form.setValue("groupingTitle", vm[value]);
                }
            },
            { name: "groupingTitle",  showIf: "false", showDefaultContextMenu: false }
        ],
        recordClick : function (viewer, record, recordNum) {
            this.creator.setButtonStates();
        },
        bodyKeyPress: function (event) {
            if (event.keyName == "Delete" && this.anySelected()) this.removeSelectedData();
            else this.Super("bodyKeyPress", arguments);
        },
        extraSpace: 5
    },

    propertyFieldNum: 1,
    directionFieldNum: 2,

    topAutoChildren: ["topLayout", "addLevelButton", "deleteLevelButton", "copyLevelButton"]

    //> @attr multiGroupPanel.initialGrouping (Array: null : IR)
    // The initial group configuration to show in the 
    // +link{multiGroupPanel.optionsGrid, optionsGrid}.
    //
    // @visibility external
    //< 

    //> @attr multiGroupPanel.maxLevels (number : null : IR)
    // The maximum number of levels of grouping that can be applied.  Since each group-property or
    // field-name can be used only once in a given multi-group operation, if no maxLevels value
    // or a value larger than the total number of available properties is specified, it will 
    // default to the total number of available properties.
    //
    // @visibility external
    //< 

});

isc.MultiGroupPanel.addProperties({
    //> @method multiGroupPanel.getNumLevels()
    // Return the number of levels of grouping that have been configured.
    //
    // @return (number) The number of levels of grouping that have been configured
    // @visibility external
    //<
    getNumLevels : function () {
        return this.optionsGrid.data.length;
    },

    //> @method multiGroupPanel.getGroup()
    // Returns all configured grouping levels, as an array of +link{GroupSpecifier}s.
    //
    // @return (Array of GroupSpecifier) the GroupSpecifiers for all configured grouping levels
    // @visibility external
    //<
    getGroup : function () {
        var grid = this.optionsGrid,
            data = grid.data.duplicate(),
            editRowNum = grid.getEditRow(),
            editValues = isc.isA.Number(editRowNum) ? grid.getEditValues(editRowNum) : null
        ;
        if (editValues) data[editRowNum] = isc.addProperties(data[editRowNum], editValues); 
        return this.getGroupSpecifiers(data);
    },

    //> @method multiGroupPanel.validate()
    // Validate that no two +link{GroupSpecifier}s group on the same 
    // +link{groupSpecifier.property, property}.
    //
    // @return (boolean) True if validation succeeds, false if any property is used twice
    // @visibility external
    //<
    validate : function () {
        var grid = this.optionsGrid,
            data = grid.data,
            specifiers = []
        ;

        for (var i = 0; i<data.length; i++) {
            var item = data.get(i);
            if (specifiers.contains(item.property)) {
                var _this = this,
                    title = this.optionsGrid.getField("property").valueMap[item.property],
                    message = this.invalidListPrompt.evalDynamicString(this, { title: title });
                isc.warn(message,
                    function () {
                        _this.recordFailedValidation(item, i);
                    }
                );
                return false;
            }
            specifiers.add(item.property);
        }

        return true;
    },

    recordFailedValidation : function (record) {
        var grid = this.optionsGrid,
            recordIndex = (isc.isA.Number(record) ? record : grid.getRecordIndex(record)),
            record = (!isc.isA.Number(record) ? record : grid.data.get(record))
        ;
        grid.selectSingleRecord(record);
        grid.startEditing(recordIndex, 1);
    },

    getGroupSpecifiers : function (data) {
        return this.optionsGrid.removeSelectionMarkers(data);
    },

    setInitialGrouping : function (data) {
        // update the initial grouping rows - the grid relies on a groupingTitle value which is
        // derived from the appropriate groupingModes valueMap
        for (var i=0; i<data.length; i++) {
            var record = data[i];
            if (record.grouping) {
                var field = this.fields.find("name", record.property),
                    modes = this._getFieldGroupingModes(field) || {}
                ;
                record.groupingTitle = modes[record.grouping];
            }
        }
        this.optionsGrid.setData(data);
    },

    initWidget : function () {
        this.Super("initWidget", arguments);

        // store the maxLevels for use with runtime calls to setFields()
        this._maxLevels = this.maxLevels;

        this.addAutoChildren(this.topAutoChildren);
        
        this.addAutoChild("levelUpButton", { prompt: this.levelUpButtonTitle });
        this.addAutoChild("levelDownButton", { prompt: this.levelDownButtonTitle });

        this.addAutoChild("optionsGrid");
        this.setGroupFields();
        this.setGroupingMode();
        this.setButtonTitles();

        this.addMember(this.topLayout);
        this.addMember(this.optionsGrid);

        this.setButtonStates();

        if (this.initialGrouping) this.setInitialGrouping(this.initialGrouping);
        else this.addLevel();
    },

    setButtonTitles : function (enable) {
        if (this.addLevelButton) this.addLevelButton.setTitle(this.addLevelButtonTitle);
        if (this.deleteLevelButton) this.deleteLevelButton.setTitle(this.deleteLevelButtonTitle);
        if (this.copyLevelButton) this.copyLevelButton.setTitle(this.copyLevelButtonTitle);
    },

    setButtonStates : function () {
        var numLevels = this.getNumLevels(),
            maxLevels = this.maxLevels,
            grid = this.optionsGrid,
            anySelected = grid.anySelected(),
            selectedIndex = grid.getRecordIndex(grid.getSelectedRecord())
        ;
        if (this.addLevelButton) this.addLevelButton.setDisabled(numLevels >= maxLevels);
        if (this.deleteLevelButton) this.deleteLevelButton.setDisabled(!anySelected);
        if (this.copyLevelButton) this.copyLevelButton.setDisabled(!anySelected || numLevels >= maxLevels); 
        if (this.levelUpButton) this.levelUpButton.setDisabled(!anySelected || selectedIndex == 0);
        if (this.levelDownButton) this.levelDownButton.setDisabled(!anySelected || selectedIndex == numLevels-1); 
    },

    // support setting the fields array after creation-time
    setFields : function (fields) {
        if (isc.DataSource && isc.isA.DataSource(fields)) fields = isc.getValues(fields.getFields());
        this.fields = fields;
        this.setGroupFields();
        this.optionsGrid.refreshFields();
        this.setButtonStates();
    },

    setGroupFields : function () {
        var fields = [];

        this.optionsGrid.getField("property").title = this.propertyFieldTitle;
        
        if (!this.fields) return;
        
        for (var i=0; i<this.fields.length; i++) {
            var field = this.fields[i];
            if (this.optionsGrid._canGroupByField(field)) fields.add(field);
        }
        this.fields = fields;
        var grid = this.optionsGrid,
            fieldMap = this.fields ? this.fields.getValueMap(grid.fieldIdProperty, "title") : 
                { none: "No fields" },
            keyCount = isc.getKeys(fieldMap).length
        ;

        for (var key in fieldMap) {
            // if there's no title, use DS.getAutoTitle() (!value seems to detect empty strings
            // too, but checking it seperately just to be safe)
            if (isc.DataSource && (!fieldMap[key] || isc.isAn.emptyString(fieldMap[key])))
                fieldMap[key] = isc.DataSource.getAutoTitle(key);
        }
        
        this.optionsGrid.setValueMap("property", fieldMap);
        if (!this._maxLevels || this.maxLevels > keyCount) this.maxLevels = keyCount;
    },
    
    setGroupingMode : function () {
        var field = this.optionsGrid.getField("grouping");
        field.title = this.groupingFieldTitle;
        field.valueMap = this.getGroupingModeFirstItem();
    },

    getGroupingModeFirstItem : function () {
        var groupingField = this.optionsGrid.getField("grouping");
        var field = this.fields[0],
            valueMap = this._getFieldGroupingModes(field)
        ;
        //groupingField.valueMap = valueMap;
        groupingField.editorValueMap = this._getFieldGroupingModes(field);
    },

    _getFieldGroupingModes : function (field) {
        if (!field) return;
        if (!this._fieldGroupingModes) this._fieldGroupingModes = {};
        var modes = this._fieldGroupingModes[field.name];

        if (!modes) {
            modes = field.groupingModes ? field.groupingModes :
                (!field.getGroupValue ? ( field._simpleType ? (field._simpleType.getGroupingModes ?
                                                           field._simpleType.getGroupingModes() :
                                                           field._simpleType.groupingModes) : false )
                : false)
            ;
            this._fieldGroupingModes[field.name] = modes;
        }

        return modes;
    },
    
    getGroupingModeAvailableField : function (record, rowNum) {
        var grid = this.optionsGrid,
            editRowNum = grid.getEditRow(),
            editValues = isc.isA.Number(editRowNum) ? grid.getEditValues(editRowNum) : null
        ;
        var recordNum = (rowNum == null)?editRowNum:rowNum;
        var propertyName = (record == null)?editValues.property:record.property;
        for (var i=0; i<this.fields.length; i++) {
            var field = this.fields[i];
            if (propertyName == field.name) {
                var groupingModes = this._getFieldGroupingModes(field);
                if (groupingModes != null) {
                    grid.setValueMap("grouping", groupingModes);
                    if (rowNum == null) {
                        if (field.groupingMode != null) {
                            grid.setEditValue(recordNum, 2, field.groupingMode);
                        } else if (field.defaultGroupingMode != null) {
                            grid.setEditValue(recordNum, 2, field.defaultGroupingMode);
                        } else {
                            grid.setEditValue(recordNum, 2, groupingModes[0]);
                        }
                        grid.refreshCell(recordNum,2);
                    }
                } else {
                    grid.setEditorValueMap("grouping", null);
                    grid.setEditValue(recordNum, 2, null);
                    grid.refreshCell(recordNum,2);
                }
                break;
            }
        }
    },

    verifyGroupingModeAvailableField : function (record, rowNum) {
        var grid = this.optionsGrid,
            editValues = isc.isA.Number(rowNum) ? grid.getEditValues(rowNum) : null
        ;
        var propertyName = (typeof editValues.property === 'undefined')?record.property:editValues.property;
        for (var i=0; i<this.fields.length; i++) {
            var field = this.fields[i];
            if (propertyName == field.name) {
                var groupingModes = this._getFieldGroupingModes(field);
                if (groupingModes != null) {
                    var size = isc.getKeys(groupingModes).length;
                    if (size == 1) {
                        return false;
                    } else {
                        return true;
                    }
                } else {
                    return false;
                }
            }
        }
        return true;
    },

    addLevel : function () {
        var grid = this.optionsGrid,
            selectedIndex = grid.getRecordIndex(grid.getSelectedRecord()),
            columnField = grid.getField("property"),
            rowNum = selectedIndex >=0 ? selectedIndex+1 : grid.data.length,
            record = { 
                property: isc.firstKey(columnField.valueMap)
            }
        ;

        grid.data.addAt(record, rowNum);
        this.editRecord(rowNum);
        this.setButtonStates();
        this.fireChangeEvent();
        this.getGroupingModeAvailableField(record,null);
    },

    deleteSelectedLevel : function () {
        var grid = this.optionsGrid,
            selectedIndex = grid.getRecordIndex(grid.getSelectedRecord())
        ;
        if (selectedIndex >= 0) {
            grid.data.removeAt(selectedIndex);
            this.setButtonStates();
            this.fireChangeEvent();
        }
    },

    copySelectedLevel : function () {
        var grid = this.optionsGrid,
            editRowNum = grid.getEditRow(),
            selectedIndex = grid.getRecordIndex(grid.getSelectedRecord()),
            record = isc.shallowClone(grid.getRecord(selectedIndex))
        ;
        if (selectedIndex >= 0) {
            grid.data.addAt(record, selectedIndex+1);
            this.editRecord(selectedIndex+1);
            this.setButtonStates();
            this.fireChangeEvent();
            this.getGroupingModeAvailableField(record,null);
            grid.setEditValue(selectedIndex+1, 2, record.grouping);
            grid.refreshCell(selectedIndex+1,2);
        }
    },

    editRecord : function (rowNum) {
        // select and edit a record
        this.optionsGrid.selectSingleRecord(rowNum);
        this.optionsGrid.startEditing(rowNum, this.propertyFieldNum);
    },
    
    moveSelectedLevelUp : function () {
        var grid = this.optionsGrid,
            selectedIndex = grid.getRecordIndex(grid.getSelectedRecord())
        ;
        if (selectedIndex>0) {
            grid.data.slide(selectedIndex, selectedIndex-1);
            this.setButtonStates();
            this.fireChangeEvent();
            this.optionsGrid.selectSingleRecord(selectedIndex-1);
        }
    },

    moveSelectedLevelDown : function () {
        var grid = this.optionsGrid,
            selectedIndex = grid.getRecordIndex(grid.getSelectedRecord())
        ;
        if (selectedIndex >= 0 && selectedIndex < grid.data.length-1) {
            grid.data.slide(selectedIndex, selectedIndex+1);
            this.setButtonStates();
            this.fireChangeEvent();
            this.optionsGrid.selectSingleRecord(selectedIndex+1);
        }
    },

    fireChangeEvent : function () {
        this.groupChanged(isc.shallowClone(this.getGroup()));
    },

    //> @method multiGroupPanel.groupChanged()
    // Fired whenever the group configuration changes.  The single parameter is an array of
    // +link{GroupSpecifier}s that represent the list of group-levels as they appear after 
    // whatever change has occurred.
    //
    // @param groupLevels (Array of GroupSpecifier) The current group configuration, after any changes
    // @visibility external
    //<
    groupChanged : function (groupLevels) {
    }

});




//> @class MultiGroupDialog
// A dialog that allows the user to set up complex grouping arrangements by defining a group of
// +link{GroupSpecifier}s.
// <P>
// Each +link{GroupSpecifier} applies to a single property and grouping - so, for instance, in 
// a grid with two columns, <code>Nationhood</code> and <code>Country</code>, you could group first 
// by <code>Nationhood</code> with its selected groupingMode and then by <code>Country</code> with its selected groupingMode.
// <P>
// <i><b>Important Note:</b> this class should not be used directly - it is exposed purely for
// +link{group:i18nMessages, i18n reasons.}</i>
// 
// @treeLocation Client Reference/Data Binding
// @visibility external
//<
isc.defineClass("MultiGroupDialog", "Window");

isc.MultiGroupDialog.addClassMethods({
    //> @classMethod multiGroupDialog.askForGrouping()
    // Launches a MultiGroupDialog and obtains a group-definition from the user.
    //
    // @param fieldSource (Array of Field or DataSource or DataBoundComponent) A source for Fields which the user can choose to group by
    // @param initialGrouping (Array) The initial group definition.
    // @param callback (Callback) Called when the user defines and accepts one or more 
    // +link{GroupSpecifier}s.  Single parameter <code>groupLevels</code> is an Array of 
    // GroupSpecifier or null if the user cancelled the dialog.
    // @visibility external
    //<
    askForGrouping : function (fieldSource, initialGrouping, callback) {
        var fields = isc.isAn.Array(fieldSource) ? fieldSource :
                isc.DataSource && isc.isA.DataSource(fieldSource) ? isc.getValues(fieldSource.getFields()) :
                isc.isA.DataBoundComponent(fieldSource) ? fieldSource.getFields() : null
        ;
        if (!fields) return;
        var grouping = [];
        if ((typeof initialGrouping === 'undefined') || (initialGrouping == null)){
            grouping = initialGrouping;
        } else {
            for (var i = 0; i < initialGrouping.length; i++) {
                var groupField = initialGrouping[i];
                for (var ii = 0; ii < fields.length; ii++) {
                    var field = fields[ii];
                    if (groupField == field.name) {
                        var record = {
                            property: field.name,
                            grouping: field.groupingMode
                        };
                        grouping.add(record);
                    }
                }
            }
        }
        var props = {
            autoDraw:true,
            fields: fields,
            initialGrouping: grouping,
            callback: callback
        };
        isc.MultiGroupDialog.create(props);
    }
});

isc.MultiGroupDialog.addProperties({
    isModal: true,
    width: 500,
    height: 250,
    vertical: true,
    autoCenter: true,
    showMinimizeButton: false,

    mainLayoutDefaults: {
        _constructor: "VLayout",
        width: "100%",
        height: "100%",
        layoutMargin: 5
    },

    multiGroupPanelDefaults: {
        _constructor: "MultiGroupPanel",
        width: "100%",
        height: "*",
        autoParent: "mainLayout"
    },

    // i18n text constants - passthrough to this.multiGroupPanel
    //> @attr multiGroupDialog.title (String : "Group" : IR)
    // The title-text to appear in this Dialog's Header-bar.  
    // 
    // @visibility external
    // @group i18nMessages
    //<
    title: "Group",

    //> @attr multiGroupDialog.addLevelButtonTitle (String : "Add Level" : IR)
    // The title-text to appear on the addLevelButton.  
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the 
    // +link{class:MultiGroupPanel, MultiGroupPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiGroupPanel for i18n.
    // 
    // @visibility external
    // @group i18nMessages
    //<
    
    //> @attr multiGroupDialog.deleteLevelButtonTitle (String : "Delete Level" : IR)
    // The title-text to appear on the deleteLevelButton
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the 
    // +link{class:MultiGroupPanel, MultiGroupPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiGroupPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<
    
    //> @attr multiGroupDialog.copyLevelButtonTitle (String : "Copy Level" : IR)
    // The title-text to appear on the copyLevelButton
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the 
    // +link{class:MultiGroupPanel, MultiGroupPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiGroupPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiGroupDialog.levelUpButtonTitle (String : "Move Level Up" : IR)
    // The hover-prompt for the Level Up button.
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiGroupDialog.levelDownButtonTitle (String : "Move Level Down" : IR)
    // The hover-prompt for the Level Down button.
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiGroupDialog.invalidListPrompt (String : "Columns may only be used once: '\${title}' is used multiple times." : IR)
    // This is a dynamic string - text within <code>\${...}</code> will be evaluated as JS code
    // when the message is displayed.
    // <P>
    // Default value returns <P>
    // <code>
    // <i>Columns may only be used once: <code>[some field's title]</code> is used multiple times</i>
    // </code>
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the 
    // +link{class:MultiGroupPanel, MultiGroupPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiGroupPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiGroupDialog.propertyFieldTitle (String : "Column" : IR)
    // The title-text to appear in the header of the "property" field.
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the 
    // +link{class:MultiGroupPanel, MultiGroupPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiGroupPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiGroupDialog.groupingFieldTitle (String : "Grouping" : IR)
    // The title-text to appear in the header of the "property" field.
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the 
    // +link{class:MultiGroupPanel, MultiGroupPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiGroupPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiGroupDialog.initialGrouping (Array : null : IR)
    // The initial group configuration to show in the 
    // +link{multiGroupPanel.optionsGrid, optionsGrid}.
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the 
    // +link{class:MultiGroupPanel, MultiGroupPanel} contained in this dialog.
    //
    // @visibility external
    //< 

    //> @attr multiGroupDialog.maxLevels (number : null : IR)
    // The maximum number of levels of grouping that can be applied.  Since each group-property or
    // field-name can be used only once in a given multi-group operation, if no maxLevels value
    // or a value larger than the total number of available properties is specified, it will 
    // default to the total number of available properties.
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the 
    // +link{class:MultiGroupPanel, MultiGroupPanel} contained in this dialog.
    //
    // @visibility external
    //< 


    //> @attr multiGroupDialog.applyButtonTitle (String : "Apply" : IR)
    // The title-text to appear on the applyButton
    // @visibility external
    // @group i18nMessages
    //<
    applyButtonTitle: "Apply",
    //> @attr multiGroupDialog.cancelButtonTitle (String : "Cancel" : IR)
    // The title-text to appear on the cancelButton
    // @visibility external
    // @group i18nMessages
    //<
    cancelButtonTitle: "Cancel",

    bottomLayoutDefaults: {
        _constructor: "HLayout",
        width: "100%",
        height: 22,
        align: "right",
        membersMargin: 5,
        autoParent: "mainLayout"
    },

    //> @attr multiGroupDialog.applyButton (AutoChild IButton : null : RA)
    // Automatically generated +link{class:IButton} providing the mechanism for accepting
    // the current group configuration.  Fires the passed callback with a single parameter,
    // groupLevels, representing the current group configuration as an array of 
    // +link{GroupSpecifier}s.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>multiGroupPanel.applyButtonProperties</code> and 
    // <code>multiGroupPanel.applyButtonDefaults</code>.
    //
    // @visibility external
    //<
    applyButtonDefaults: {
        _constructor: "IButton",
        autoFit: true,
        height: 22,
        autoParent: "bottomLayout",
        click: "this.creator.apply()"
    },

    //> @attr multiGroupDialog.cancelButton (AutoChild IButton : null : RA)
    // Automatically generated +link{class:IButton} providing the mechanism for closing this
    // Dialog without accepting the current group configuration.  The passed callback is fired 
    // with a single null parameter, indicating that the operation was cancelled.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>multiGroupPanel.cancelButtonProperties</code> and 
    // <code>multiGroupPanel.cancelButtonDefaults</code>.
    //
    // @visibility external
    //<
    cancelButtonDefaults: {
        _constructor: "IButton",
        autoFit: true,
        height: 22,
        autoParent: "bottomLayout",
        click: "this.creator.cancel()"
    },

    bottomAutoChildren: ["bottomLayout", "applyButton", "cancelButton"]

    //> @attr multiGroupDialog.addLevelButton (AutoChild IButton : null : RA)
    // @include multiGroupPanel.addLevelButton
    //<

    //> @attr multiGroupDialog.deleteLevelButton (AutoChild IButton : null : RA)
    // @include multiGroupPanel.deleteLevelButton
    //<

    //> @attr multiGroupDialog.copyLevelButton (AutoChild IButton : null : RA)
    // @include multiGroupPanel.copyLevelButton
    //<

    //> @attr multiGroupDialog.levelUpButton (AutoChild ImgButton : null : RA)
    // @include multiGroupPanel.levelUpButton
    //<

    //> @attr multiGroupDialog.levelDownButton (AutoChild ImgButton : null : RA)
    // @include multiGroupPanel.levelDownButton
    //<

    //> @attr multiGroupDialog.fields (Array of Field : null : IR)
    // @include multiGroupPanel.fields
    //<

    //> @attr multiGroupDialog.optionsGrid (AutoChild ListGrid : null : IR)
    // @include multiGroupPanel.optionsGrid
    //<

});

isc.MultiGroupDialog.addProperties({
    initWidget : function () {
        this.Super("initWidget", arguments);

        // copy the unset properties
        this.copyUnsetProperties();

        this.addAutoChild("mainLayout");
        this.addAutoChild("multiGroupPanel", this.getPassthroughProperties());

        this.addAutoChildren(this.bottomAutoChildren);
        this.addItem(this.mainLayout);
        // grab a local copy of the panel's optionsGrid
        this.optionsGrid = this.multiGroupPanel.optionsGrid;
        // setup the button-states
        this.setButtonStates();
    },

    _passthroughs: [ "fields", "initialGrouping", "maxLevels", "invalidListPrompt",
        // autoChildren & i18nMessages
        "addLevelButtonTitle", "addLevelButtonDefaults", "addLevelButtonProperties",
        "deleteLevelButtonTitle", "deleteLevelButtonDefaults", "deleteLevelButtonProperties",
        "levelUpButtonTitle", "levelDownButtonTitle",
        "copyLevelButtonTitle", "copyLevelButtonDefaults", "copyLevelButtonProperties",
        // grid properties and titles
        "optionsGridDefaults", "optionsGridProperties",
        "firstGroupLevelTitle", "propertyFieldTitle", "groupingFieldTitle", "otherGroupLevelTitle"
    ],
    
    getPassthroughProperties : function () {
        var propNames = this._passthroughs,
            props = {};

        for (var i = 0; i < propNames.length; i++) {
            var name = propNames[i];
            if (this[name] != null) props[name] = this[name];
        }
        return props;
    },

    _unsetPropertiesPanel: [ "addLevelButtonTitle", "deleteLevelButtonTitle", "levelUpButtonTitle", "levelDownButtonTitle",
        "copyLevelButtonTitle", "firstGroupLevelTitle", "propertyFieldTitle",
        "groupingFieldTitle", "otherGroupLevelTitle", "invalidListPrompt"
    ],

    _unsetPropertiesDialog: [ "applyButtonTitle", "cancelButtonTitle", "title"
    ],

    copyUnsetProperties : function () {
        var propNames = this._unsetPropertiesPanel,
            props = {};
        for (var i = 0; i < propNames.length; i++) {
            var name = propNames[i];
            if (isc.MultiGroupPanel.getInstanceProperty(name) == null) {
                props[name] = isc.MultiSortPanel.getInstanceProperty(name);
            }
        }
        isc.MultiGroupPanel.addProperties(props);

        propNames = this._unsetPropertiesDialog;
        props = {};
        for (var i = 0; i < propNames.length; i++) {
            var name = propNames[i];
            if (isc.MultiGroupDialog.getInstanceProperty(name) == null) {
                props[name] = isc.MultiSortDialog.getInstanceProperty(name);
            }
        }
        isc.MultiGroupDialog.addProperties(props);
    },

    setButtonStates : function () {
        this.multiGroupPanel.setButtonStates();
        this.applyButton.setTitle(this.applyButtonTitle);
        this.cancelButton.setTitle(this.cancelButtonTitle);
    },

    //> @method multiGroupDialog.getNumLevels()
    // @include multiGroupPanel.getNumLevels
    //<
    getNumLevels : function () {
        return this.multiGroupPanel.getNumLevels();
    },

    //> @method multiGroupDialog.getGroup()
    // @include multiGroupPanel.getGroup
    //<
    getGroup : function () {
        return this.multiGroupPanel.getGroup();
    },

    //> @method multiGroupDialog.validate()
    // @include multiGroupPanel.validate
    //<
    validate : function () {
        return this.multiGroupPanel.validate();
    },

    closeClick : function () {
        this.cancel();
        return false;
    },
    
    cancel : function () {
        this.hide();
        this.markForDestroy();
    },

    apply : function () {
        // end the current edit, if there is one
        if (this.optionsGrid.getEditRow() != null) this.optionsGrid.endEditing();
        if (!this.validate()) return;
        if (this.callback) {
            // get the array of GroupSpecifiers and fire the callback is one was provided
            var specifiers = isc.shallowClone(this.getGroup());
            this.fireCallback(this.callback, "groupingMode", [specifiers]);
        }
        this.hide();
        this.markForDestroy();
    }    

});


