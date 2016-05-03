/*
 * Isomorphic SmartClient
 * Version v11.0p_2016-03-31 (2016-03-31)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */
//> @object SortSpecifier
// A Javascript object defining the details of a single sort operation.
// <P>
// You can convert between SortSpecifiers and the string format required by 
// +link{dsRequest.sortBy} by calling +link{DataSource.getSortBy()} and 
// +link{DataSource.getSortSpecifiers()}.
//
// @treeLocation Client Reference/Data Binding
// @visibility external
//< 

//> @attr sortSpecifier.property (String : null : IR)
// The property name, eg a +link{ListGridField, field name}, to which this sortSpecifier applies.
//
// @visibility external
//< 

//> @attr sortSpecifier.direction (SortDirection : null : IR)
// The direction in which this specifier should sort.
//
// @visibility external
//< 

//> @attr sortSpecifier.normalizer (Function : null : IR)
// A normalizer function which this sortSpecifier will use to sort.
//
// @visibility external
//< 

//> @attr sortSpecifier.context (DataBoundComponent : null : IR)
// A DataBoundComponent providing the context for the sort-normalizer.
//
// @visibility external
//< 


//> @class MultiSortPanel
// A widget that allows the user to set up complex sorting arrangements by defining a group of
// +link{SortSpecifier}s.
// <P>
// Each +link{SortSpecifier} applies to a single property and direction - so, for instance, in 
// a grid with two columns, <code>year</code> and <code>monthNumber</code>, you could sort first 
// by <code>year</code> in descending order and then by <code>monthNumber</code> in ascending 
// order.  This would producing a grid sorted by year from largest (most 
// recent) to smallest (least recent) and, within each year, by monthNumber from smallest 
// (January) to largest (December).
// 
// @treeLocation Client Reference/Data Binding
// @visibility external
//<
isc.defineClass("MultiSortPanel", "Layout");

isc.MultiSortPanel.addProperties({
    vertical: true,
    overflow: "visible",

    //> @attr multiSortPanel.fields (Array of DataSourceField : null : IR)
    // The list of fields which the user can choose to sort by.
    // @visibility external
    //<

    // i18n text constants
    //> @attr multiSortPanel.addLevelButtonTitle (String : "Add Level" : IR)
    // The title-text to appear on the addLevelButton
    // @visibility external
    // @group i18nMessages
    //<
    addLevelButtonTitle: "Add Level",
    //> @attr multiSortPanel.deleteLevelButtonTitle (String : "Delete Level" : IR)
    // The title-text to appear on the deleteLevelButton
    // @visibility external
    // @group i18nMessages
    //<
    deleteLevelButtonTitle: "Delete Level",
    //> @attr multiSortPanel.copyLevelButtonTitle (String : "Copy Level" : IR)
    // The title-text to appear on the copyLevelButton
    // @visibility external
    // @group i18nMessages
    //<
    copyLevelButtonTitle: "Copy Level",

    //> @attr multiSortPanel.invalidListPrompt (HTMLString : "Columns may only be used once: '${title}' is used multiple times." : IR)
    // This is a dynamic string - text within <code>&#36;{...}</code> will be evaluated as JS code
    // when the message is displayed.
    // <P>
    // Default value returns <P>
    // <code>
    // <i>Columns may only be used once: <code>[some field's title]</code> is used multiple times</i>
    // </code>
    // @visibility external
    // @group i18nMessages
    //<
    invalidListPrompt: "Columns may only be used once: '${title}' is used multiple times.",

    //> @attr multiSortPanel.propertyFieldTitle (String : "Column" : IR)
    // The title-text to appear in the header of the "property" field.
    // @visibility external
    // @group i18nMessages
    //<
    propertyFieldTitle: "Column",
    
    //> @attr multiSortPanel.directionFieldTitle (String : "Order" : IR)
    // The title-text to appear in the header of the "direction" field.
    // @visibility external
    // @group i18nMessages
    //<
    directionFieldTitle: "Order",

    //> @attr multiSortPanel.ascendingTitle (String : "Ascending" : IR)
    // The title-text to appear in the "direction" field's SelectItem for an "ascending" sort
    // @visibility external
    // @group i18nMessages
    //<
    ascendingTitle: "Ascending",
    //> @attr multiSortPanel.descendingTitle (String : "Descending" : IR)
    // The title-text to appear in the "direction" field's SelectItem for a "descending" sort
    // @visibility external
    // @group i18nMessages
    //<
    descendingTitle: "Descending",

    //> @attr multiSortPanel.firstSortLevelTitle (String : "Sort by" : IR)
    // The title-text to appear in the first column for the first sort-level.
    // @visibility external
    // @group i18nMessages
    //<
    firstSortLevelTitle: "Sort by",
    //> @attr multiSortPanel.otherSortLevelTitle (String : "Then by" : IR)
    // The title-text to appear in the first column for all sort-levels other than the first.
    // @visibility external
    // @group i18nMessages
    //<
    otherSortLevelTitle: "Then by",
    
    topLayoutDefaults: {
        _constructor: "HLayout",
        overflow: "visible",
        height: 22,
        align: "left",
        membersMargin: 5,
        extraSpace: 5
    },

    //> @attr multiSortPanel.addLevelButton (AutoChild IButton : null : RA)
    // Automatically generated +link{class:IButton} providing a mechanism for adding new levels
    // to the sort configuration.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>multiSortPanel.addLevelButtonProperties</code> and 
    // <code>multiSortPanel.addLevelButtonDefaults</code>.
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

    //> @attr multiSortPanel.deleteLevelButton (AutoChild IButton : null : RA)
    // Automatically generated +link{class:IButton} providing a mechanism for deleting levels
    // from the sort configuration.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>multiSortPanel.deleteLevelButtonProperties</code> and 
    // <code>multiSortPanel.deleteLevelButtonDefaults</code>.
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

    //> @attr multiSortPanel.copyLevelButton (AutoChild IButton : null : RA)
    // Automatically generated +link{class:IButton} providing a mechanism for duplicating levels
    // in the sort configuration.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>multiSortPanel.copyLevelButtonProperties</code> and 
    // <code>multiSortPanel.copyLevelButtonDefaults</code>.
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

    //> @attr multiSortPanel.levelUpButtonTitle (String : "Move Level Up" : IR)
    // The hover-prompt for the Level Up button.
    // @visibility external
    // @group i18nMessages
    //<
    levelUpButtonTitle: "Move Level Up",

    //> @attr multiSortPanel.levelUpButton (AutoChild ImgButton : null : RA)
    // Automatically generated +link{class:ImgButton} providing a mechanism for moving existing
    // sort-levels up in the sort configuration.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>multiSortPanel.levelUpButtonProperties</code> and 
    // <code>multiSortPanel.levelUpButtonDefaults</code>.
    //
    // @visibility external
    //<
    levelUpButtonDefaults: {
        _constructor: "ImgButton",
        src: "[SKINIMG]common/arrow_up.gif",
        height: 22,
        width: 20,
        imageType: "center",
        showDisabled: false,
        showRollOver: false,
        showDown: false,
        showFocused: false,
        autoParent: "topLayout",
        click: "this.creator.moveSelectedLevelUp()"
    },

    //> @attr multiSortPanel.levelDownButtonTitle (String : "Move Level Down" : IR)
    // The hover-prompt for the Level Down button.
    // @visibility external
    // @group i18nMessages
    //<
    levelDownButtonTitle: "Move Level Down",

    //> @attr multiSortPanel.levelDownButton (AutoChild ImgButton : null : RA)
    // Automatically generated +link{class:ImgButton} providing a mechanism for moving existing
    // sort-levels down in the sort configuration.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>multiSortPanel.levelDownButtonProperties</code> and 
    // <code>multiSortPanel.levelDownButtonDefaults</code>.
    //
    // @visibility external
    //<
    levelDownButtonDefaults: {
        _constructor: "ImgButton",
        src: "[SKINIMG]common/arrow_down.gif",
        height: 22,
        width: 20,
        imageType: "center",
        showDisabled: false,
        showRollOver: false,
        showDown: false,
        showFocused: false,
        autoParent: "topLayout",
        click: "this.creator.moveSelectedLevelDown()"
    },

    //> @attr multiSortPanel.optionsGrid (AutoChild ListGrid : null : IR)
    // Automatically generated +link{class:ListGrid} allowing the user to configure a set of 
    // +link{SortSpecifier}s.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>multiSortPanel.optionsGridProperties</code> and 
    // <code>multiSortPanel.optionsGridDefaults</code>.
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

//        alwaysShowEditors: true,
        defaultFields: [
            { name: "sortSequence", title: "&nbsp;", showTitle: false, canEdit: false, width: 80, canHide: false,
                showDefaultContextMenu: false,
                formatCellValue : function (value, record, rowNum, colNum, grid) {
                    return rowNum == 0 ? grid.creator.firstSortLevelTitle : 
                        grid.creator.otherSortLevelTitle;
                }
            },
            { name: "property", title: " ", type: "select",
                defaultToFirstOption: true,
                showDefaultContextMenu: false,
                changed : function (form, item, value) {
                    // clear out the stored normalizer if the sort property changes - it will 
                    // be re-calculated by setSort()
                    item.grid.getRecord(item.rowNum).normalizer = null;
                    item.grid.creator.fireChangeEvent();
                }
            },
            { name: "direction",  title: " ", type: "select", width: 100,
                showDefaultContextMenu: false,
                defaultToFirstOption: true,
                changed: "item.grid.creator.fireChangeEvent()"
            }
        ],
        selectionUpdated: function (record, recordList) {
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

    //> @attr multiSortPanel.initialSort (Array of SortSpecifier : null : IR)
    // The initial sort configuration to show in the 
    // +link{multiSortPanel.optionsGrid, optionsGrid}.
    //
    // @visibility external
    //< 

    //> @attr multiSortPanel.maxLevels (number : null : IR)
    // The maximum number of levels of sorting that can be applied.  Since each sort-property or
    // field-name can be used only once in a given multi-sort operation, if no maxLevels value
    // or a value larger than the total number of available properties is specified, it will 
    // default to the total number of available properties.
    //
    // @visibility external
    //< 

});

isc.MultiSortPanel.addMethods({
    //> @method multiSortPanel.getNumLevels()
    // Return the number of levels of sorting that have been configured.
    //
    // @return (number) The number of levels of sorting that have been configured
    // @visibility external
    //<
    getNumLevels : function () {
        return this.optionsGrid.data.length;
    },

    //> @method multiSortpanel.getSortLevel()
    // Return a +link{SortSpecifier} object for the requested levelNum.
    //
    // @param levelNum (number) The index of the level to return a SortSpecifier for
    // @return (SortSpecifier) A SortSpecifier representing the requested levelNum
    // @visibility external
    //<
    getSortLevel : function (levelNum) {
        return this.getSortSpecifier(this.data.get(levelNum));
    },

    //> @method multiSortPanel.getSort()
    // Returns all configured sorting levels, as an array of +link{SortSpecifier}s.
    //
    // @return (Array of SortSpecifier) the SortSpecifiers for all configured sorting levels
    // @visibility external
    //<
    getSort : function () {
        var grid = this.optionsGrid,
            data = grid.data.duplicate(),
            editRowNum = grid.getEditRow(),
            editValues = isc.isA.Number(editRowNum) ? grid.getEditValues(editRowNum) : null
        ;

        if (editValues) data[editRowNum] = isc.addProperties(data[editRowNum], editValues); 
        return this.getSortSpecifiers(data);
    },

    //> @method multiSortPanel.setSort()
    // Sets the sort configuration being displayed after initialization
    //
    // @param sortSpecifiers (Array of SortSpecifier) The sort configuration to set in the +link{optionsGrid}
    //<
    setSort : function (sortSpecifiers) {
        this.setSortSpecifiers(sortSpecifiers);
    },


    //> @method multiSortPanel.validate()
    // Validate that no two +link{SortSpecifier}s sort on the same 
    // +link{sortSpecifier.property, property}.
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

    getSortSpecifier : function (record) {
        if (isc.isA.Number(record)) record = this.optionsGrid.data.get(record);
        return this.optionsGrid.removeSelectionMarkers(record);
    },

    getSortSpecifiers : function (data) {
        return this.optionsGrid.removeSelectionMarkers(data);
    },

    setSortSpecifiers : function (data) {
        if (data && data.length > 0) {
            for (var i=0; i<data.length; i++) {
                if (data[i].owningField) {
                    data[i].property = data[i].owningField;
                }
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
        this.setSortFields();
        this.setSortDirections();
        this.setButtonTitles();

        this.addMember(this.topLayout);
        this.addMember(this.optionsGrid);

        this.setButtonStates();

        if (this.initialSort) this.setSortSpecifiers(this.initialSort);
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
        this.fields = isc.shallowClone(fields);
        this.setSortFields();
        this.optionsGrid.refreshFields();
        this.setButtonStates();
    },

    setSortFields : function () {
        var fields = [];

        this.optionsGrid.getField("property").title = this.propertyFieldTitle;
        
        if (!this.fields) return;
        
        // parse the fields array removing any canSort: false fields
        for (var i=0; i<this.fields.length; i++) {
            var field = this.fields[i];
            if (field.canSort != false) fields.add(field);
            
        }
        this.fields = fields;
        var grid = this.optionsGrid,
            fieldMap = this.fields ? this.fields.getValueMap(grid.fieldIdProperty, "title") : 
                { none: "No fields" },
            keyCount = isc.getKeys(fieldMap).length
        ;

        for (var key in fieldMap) {
            // if there's no title, use DS.getAutoTitle() (!value seems to detect empty strings
            // too, but checking it separately just to be safe)
            if (isc.DataSource && (!fieldMap[key] || isc.isAn.emptyString(fieldMap[key])))
                fieldMap[key] = isc.DataSource.getAutoTitle(key);
        }
        
        if (this.creator.headerSpans && this.creator.showHeaderSpanTitles) {
            this.applyHeaderSpans(this.creator.headerSpans, fieldMap, "");
        }

        this.optionsGrid.setValueMap("property", fieldMap);
        if (!this._maxLevels || this.maxLevels > keyCount) this.maxLevels = keyCount;
    },
    
    applyHeaderSpans : function (spans, fieldMap, paramTitle) {
        // This method prepends header span titles to field titles.  This can be important for
        // disambiguation - for example, a grid that shows "Q1" and "Q2" columns under header
        // spans of "North", "South" "East" and "West".  The user sees, eg, "East - Q1" in the
        // list of fields, rather than a bunch of unqualified "Q1" and "Q2" references.
        for (var i = 0; i < spans.length; i++) {
            var title = paramTitle;
            var span = spans[i];
            title += span.title + this.creator.spanTitleSeparator;
            if (span.spans) {
                this.applyHeaderSpans(span.spans, fieldMap, title);
            } else {
                for (var j = 0; j < span.fields.length; j++) {
                    var fieldName = span.fields[j];
                    // skip fields not present in the valueMap (canSort:false)
                    if (fieldMap[fieldName] == null) {
                    } else {
                    fieldMap[fieldName] = title + fieldMap[fieldName];
                }
            }
        }
        }
    },

    setSortDirections : function () {
        this.optionsGrid.getField("direction").title = this.directionFieldTitle;
        this.optionsGrid.getField("direction").valueMap = { 
            "ascending" : this.ascendingTitle, 
			"descending" : this.descendingTitle 
        };
    },

    addLevel : function () {
        var grid = this.optionsGrid,
            selectedIndex = grid.getRecordIndex(grid.getSelectedRecord()),
            columnField = grid.getField("property"),
            orderField = grid.getField("direction"),
            rowNum = selectedIndex >=0 ? selectedIndex+1 : grid.data.length,
            record = { 
                property: isc.firstKey(columnField.valueMap),
                direction: isc.firstKey(orderField.valueMap)
            }
        ;

        grid.data.addAt(record, rowNum);
        this.editRecord(rowNum);
        this.setButtonStates();
        this.fireChangeEvent();
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
            selectedIndex = grid.getRecordIndex(grid.getSelectedRecord()),
            record = isc.shallowClone(grid.getRecord(selectedIndex))
        ;
        if (selectedIndex >= 0) {
            grid.data.addAt(record, selectedIndex+1);
            this.editRecord(selectedIndex+1);
            this.setButtonStates();
            this.fireChangeEvent();
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
            this.fireChangeEvent();
            this.optionsGrid.selectSingleRecord(selectedIndex+1);
        }
    },

    fireChangeEvent : function () {
        this.sortChanged(isc.shallowClone(this.getSort()));
    },

    //> @method multiSortPanel.sortChanged()
    // Fired whenever the sort configuration changes.  The single parameter is an array of
    // +link{SortSpecifier}s that represent the list of sort-levels as they appear after 
    // whatever change has occurred.
    //
    // @param sortLevels (Array of SortSpecifier) The current sort configuration, after any changes
    // @visibility external
    //<
    sortChanged : function (sortLevels) {
    }

});




//> @class MultiSortDialog
// A dialog that allows the user to set up complex sorting arrangements by defining a group of
// +link{SortSpecifier}s.
// <P>
// Each +link{SortSpecifier} applies to a single property and direction - so, for instance, in 
// a grid with two columns, <code>year</code> and <code>monthNumber</code>, you could sort first 
// by <code>year</code> in descending order and then by <code>monthNumber</code> in ascending 
// order.  This would producing a grid sorted by year from largest (most 
// recent) to smallest (least recent) and, within each year, by monthNumber from smallest 
// (January) to largest (December).
// <P>
// See +link{MultiSortDialog.askForSort()}, +link{dataBoundComponent.askForSort()}
// 
// @treeLocation Client Reference/Data Binding
// @visibility external
//<
isc.defineClass("MultiSortDialog", "Window");

isc.MultiSortDialog.addClassMethods({
    //> @classMethod multiSortDialog.askForSort()
    // Launches a MultiSortDialog and obtains a sort-definition from the user.
    //
    // @param fieldSource (DataBoundComponent | DataSource | Array of DataSourceField) A source for Fields which the user can choose to sort by
    // @param initialSort (Array of SortSpecifier) The initial sort definition.
    // @param callback (Callback) Called when the user defines and accepts one or more 
    // +link{SortSpecifier}s.  Single parameter <code>sortLevels</code> is an Array of 
    // SortSpecifier or null if the user cancelled the dialog.
    // @param [properties] (MultiSortDialog Properties) Configuration to apply to the
    //  generated dialog
    //
    // @visibility external
    //<
    askForSort : function (fieldSource, initialSort, callback, properties) {
        var fields = isc.isAn.Array(fieldSource) ? fieldSource :
                isc.DataSource && isc.isA.DataSource(fieldSource) ? isc.getValues(fieldSource.getFields()) :
                isc.isA.DataBoundComponent(fieldSource) ? fieldSource.getAllFields() : null
        ;
        if (!fields) return;
        var props = isc.addProperties({
        	// this will cause initial draw
            autoDraw:true,
            fields: fields,
            initialSort: initialSort,
            callback: callback
        }, properties);
        if (isc.ListGrid && isc.isA.ListGrid(fieldSource) && fieldSource.headerSpans) {
            props.headerSpans = fieldSource.headerSpans;
            props.showHeaderSpanTitles = fieldSource.showHeaderSpanTitlesInSortEditor;
            props.spanTitleSeparator = fieldSource.sortEditorSpanTitleSeparator;
        }
        return isc.MultiSortDialog.create(props);

    }
});

isc.MultiSortDialog.addProperties({
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
    
    //> @attr multiSortDialog.multiSortPanel (AutoChild MultiSortPanel : null : R)
    // Automatically generated +link{MultiSortPanel} displayed within this
    // component.
    //
    // @visibility external
    //<
    
	multiSortPanelDefaults: {
        _constructor: "MultiSortPanel",
        width: "100%",
        height: "*",
        autoParent: "mainLayout"
    },

    // i18n text constants - passthrough to this.multiSortPanel
    //> @attr multiSortDialog.title (String : "Sort" : IR)
    // The title-text to appear in this Dialog's Header-bar.  
    // 
    // @visibility external
    // @group i18nMessages
    //<
    title: "Sort",

    //> @attr multiSortDialog.addLevelButtonTitle (String : "Add Level" : IR)
    // The title-text to appear on the addLevelButton.  
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the 
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiSortPanel for i18n.
    // 
    // @visibility external
    // @group i18nMessages
    //<
    
    //> @attr multiSortDialog.deleteLevelButtonTitle (String : "Delete Level" : IR)
    // The title-text to appear on the deleteLevelButton
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the 
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiSortPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<
    
    //> @attr multiSortDialog.copyLevelButtonTitle (String : "Copy Level" : IR)
    // The title-text to appear on the copyLevelButton
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the 
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiSortPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiSortDialog.levelUpButtonTitle (String : "Move Level Up" : IR)
    // The hover-prompt for the Level Up button.
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiSortDialog.levelDownButtonTitle (String : "Move Level Down" : IR)
    // The hover-prompt for the Level Down button.
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiSortDialog.invalidListPrompt (HTMLString : "Columns may only be used once: '${title}' is used multiple times." : IR)
    // This is a dynamic string - text within <code>&#36;{...}</code> will be evaluated as JS code
    // when the message is displayed.
    // <P>
    // Default value returns <P>
    // <code>
    // <i>Columns may only be used once: <code>[some field's title]</code> is used multiple times</i>
    // </code>
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the 
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiSortPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiSortDialog.propertyFieldTitle (String : "Column" : IR)
    // The title-text to appear in the header of the "property" field.
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the 
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiSortPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<
    
    //> @attr multiSortDialog.directionFieldTitle (String : "Order" : IR)
    // The title-text to appear in the header of the "direction" field.
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the 
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiSortPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiSortDialog.ascendingTitle (String : "Ascending" : IR)
    // The title-text to appear in the "direction" field's SelectItem for an "ascending" sort
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the 
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiSortPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<
    
    //> @attr multiSortDialog.descendingTitle (String : "Descending" : IR)
    // The title-text to appear in the "direction" field's SelectItem for a "descending" sort
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the 
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiSortPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiSortDialog.firstSortLevelTitle (String : "Sort by" : IR)
    // The title-text to appear in the first column for the first sort-level.
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the 
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiSortPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiSortDialog.otherSortLevelTitle (String : "Then by" : IR)
    // The title-text to appear in the first column for all sort-levels other than the first.
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the 
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiSortPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiSortDialog.initialSort (Array of SortSpecifier : null : IR)
    // The initial sort configuration to show in the 
    // +link{multiSortPanel.optionsGrid, optionsGrid}.
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the 
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.
    //
    // @visibility external
    //< 

    //> @attr multiSortDialog.maxLevels (number : null : IR)
    // The maximum number of levels of sorting that can be applied.  Since each sort-property or
    // field-name can be used only once in a given multi-sort operation, if no maxLevels value
    // or a value larger than the total number of available properties is specified, it will 
    // default to the total number of available properties.
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the 
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.
    //
    // @visibility external
    //< 


    //> @attr multiSortDialog.applyButtonTitle (String : "Apply" : IR)
    // The title-text to appear on the applyButton
    // @visibility external
    // @group i18nMessages
    //<
    applyButtonTitle: "Apply",
    //> @attr multiSortDialog.cancelButtonTitle (String : "Cancel" : IR)
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

    //> @attr multiSortDialog.applyButton (AutoChild IButton : null : RA)
    // Automatically generated +link{class:IButton} providing the mechanism for accepting
    // the current sort configuration.  Fires the passed callback with a single parameter,
    // sortLevels, representing the current sort configuration as an array of 
    // +link{SortSpecifier}s.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>multiSortDialog.applyButtonProperties</code> and 
    // <code>multiSortDialog.applyButtonDefaults</code>.
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

    //> @attr multiSortDialog.cancelButton (AutoChild IButton : null : RA)
    // Automatically generated +link{class:IButton} providing the mechanism for closing this
    // Dialog without accepting the current sort configuration.  The passed callback is fired 
    // with a single null parameter, indicating that the operation was cancelled.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>multiSortDialog.cancelButtonProperties</code> and 
    // <code>multiSortDialog.cancelButtonDefaults</code>.
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

    //> @attr multiSortDialog.addLevelButton (AutoChild IButton : null : RA)
    // @include multiSortPanel.addLevelButton
    //<

    //> @attr multiSortDialog.deleteLevelButton (AutoChild IButton : null : RA)
    // @include multiSortPanel.deleteLevelButton
    //<

    //> @attr multiSortDialog.copyLevelButton (AutoChild IButton : null : RA)
    // @include multiSortPanel.copyLevelButton
    //<

    //> @attr multiSortDialog.levelUpButton (AutoChild ImgButton : null : RA)
    // @include multiSortPanel.levelUpButton
    //<

    //> @attr multiSortDialog.levelDownButton (AutoChild ImgButton : null : RA)
    // @include multiSortPanel.levelDownButton
    //<

	
    //> @attr multiSortDialog.fields (Array of DataSourceField : null : IR)
    // @include multiSortPanel.fields
    //<

    //> @attr multiSortDialog.optionsGrid (AutoChild ListGrid : null : IR)
    // @include multiSortPanel.optionsGrid
    //<

});

isc.MultiSortDialog.addMethods({
    initWidget : function () {
        this.Super("initWidget", arguments);

        this.addAutoChild("mainLayout");
        this.addAutoChild("multiSortPanel", this.getPassthroughProperties());

        this.addAutoChildren(this.bottomAutoChildren);
        this.addItem(this.mainLayout);
        // grab a local copy of the panel's optionsGrid
        this.optionsGrid = this.multiSortPanel.optionsGrid;
        // setup the button-states
        this.setButtonStates();
    },

    _passthroughs: [ "initialSort", "maxLevels", "invalidListPrompt",
        // autoChildren & i18nMessages
        "addLevelButtonTitle", "addLevelButtonDefaults", "addLevelButtonProperties",
        "deleteLevelButtonTitle", "deleteLevelButtonDefaults", "deleteLevelButtonProperties",
        "levelUpButtonTitle", "levelDownButtonTitle", 
        "copyLevelButtonTitle", "copyLevelButtonDefaults", "copyLevelButtonProperties",
        // grid properties and titles
        "optionsGridDefaults", "optionsGridProperties",
        "firstSortLevelTitle", "propertyFieldTitle", "directionFieldTitle",
        "descendingTitle", "ascendingTitle", "otherSortLevelTitle"
    ],
    
    getPassthroughProperties : function () {
        var propNames = this._passthroughs,
            props = {};

        for (var i = 0; i < propNames.length; i++) {
            var name = propNames[i];
            if (this[name] != null) props[name] = this[name];
        }
        
        if (this.fields) props.fields = isc.shallowClone(this.fields);

        return props;
    },

    setButtonStates : function () {
        this.multiSortPanel.setButtonStates();
        this.applyButton.setTitle(this.applyButtonTitle);
        this.cancelButton.setTitle(this.cancelButtonTitle);
    },

    //> @method multiSortDialog.getNumLevels()
    // @include multiSortPanel.getNumLevels
    //<
    getNumLevels : function () {
        return this.multiSortPanel.getNumLevels();
    },

    //> @method multiSortDialog.getSortLevel()
    // @include multiSortPanel.getSortLevel
    //<
    getSortLevel : function (levelNum) {
        return this.multiSortPanel.getSortLevel(levelNum);
    },

    //> @method multiSortDialog.getSort()
    // @include multiSortPanel.getSort
    //<
    getSort : function () {
        return this.multiSortPanel.getSort();
    },

    //> @method multiSortDialog.validate()
    // @include multiSortPanel.validate
    //<
    validate : function () {
        return this.multiSortPanel.validate();
    },
    
    //> @attr multiSortDialog.autoDestroy (boolean : true : IRW)
    // Should this dialog auto-destroy when the user dismisses it?
    // Set this property to false for a re-usable multiSortDialog
    //<
    autoDestroy:true,

    closeClick : function () {
        this.cancel();
        return false;
    },
    
    cancel : function () {
        if (this.callback) 
            this.fireCallback(this.callback, ["sortLevels"], [null]);

        this.clear();
        if (this.autoDestroy) this.markForDestroy();
    },

    apply : function () {
        // end the current edit, if there is one
        if (this.optionsGrid.getEditRow() != null) this.optionsGrid.endEditing();
        if (!this.validate()) return;
        if (this.callback) {
            // get the array of SortSpecifiers and fire the callback is one was provided
            var specifiers = isc.shallowClone(this.getSort());
            this.fireCallback(this.callback, ["sortLevels"], [specifiers]);
        }
        this.clear();
        if (this.autoDestroy) this.markForDestroy();
    }    

});


