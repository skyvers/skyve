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
// NOTE: This class does not support composite primary keys




if (isc.Portal) {

isc.defineClass("EntityEditorHeader", "VLayout");

isc.EntityEditorHeader.addProperties({
    height: 1,
    padding: 10,
    border: "2px solid black",

    headerLayoutDefaults: {
        _constructor: "VLayout",
        width: "100%",
        height: 1,
        membersMargin: 5
    },
    
    headerLabelTitle: "<B><H2>Editing ${entityType}</H2><br>"+
        "<H3>This UI lets you edit the entire data-structure for this Entity-type</H3></B>",
    headerLabelDefaults: {
        _constructor: "Label",
        width: "100%",
        height: 30,
        autoParent: "headerLayout"
    },
    showDetailLabel: false,
    defaultDetailLabelTitle: "<B><H3>This UI lets you edit the entire data-structure for this Entity-type</H3></B>",
    detailLabelTitle: "<B><H3>$entityComment</H3></B>",
    detailLabelDefaults: {
        _constructor: "Label",
        width: "100%",
        height: 20,
        autoParent: "headerLayout"
    },

    unknownEntityTitle: "[Unknown Entity-type]",

    initWidget : function () {
        var headerTitle = this.headerLabelTitle;
        var detailTitle = this.detailLabelTitle;

        if (this.dataSource) this.getDataSource(this.dataSource);
        if (!this.entityName) this.entityName = this.getEntityName(this.dataSource);
        if (!this.entityComment) this.entityComment = this.getEntityComment(this.dataSource);

        if (this.entityName) 
            headerTitle = headerTitle.evalDynamicString(this, { entityType: this.entityName });
        if (this.entityComment)
            detailTitle = detailTitle.evalDynamicString(this, { entityType: this.entityComment });

        this.headerLayout = this.addAutoChild("headerLayout");
        this.headerLabel = this.addAutoChild("headerLabel", {contents: headerTitle});
        this.detailLabel = this.addAutoChild("detailLabel", {contents: detailTitle});

        this.headerLayout.addMembers([this.headerLabel, this.detailLabel]);
        //this.headerLayout.addMember(this.detailLabel);
        
        this.addMember(this.headerLayout);
    },

    getEntityName : function (dataSource) {
        var result = this.unknownEntityTitle;

        if (dataSource) {
            if (isc.isA.Function(dataSource.getEntityName))
                result = dataSource.getEntityName();
            else result = dataSource.ID;
        }

        return result;
    },

    getEntityComment : function (dataSource) {
        var result = this.defaultDetailLabelTitle;

        if (dataSource) {
            if (isc.isA.Function(dataSource.getEntityComment))
                result = dataSource.getEntityComment();
            else result = "Allows hierarchical editing of data in "+dataSource.ID+" DataSource";
        }
        
        return result;
    }
    
    
});

isc.defineClass("EntityEditorForm", "Portlet").addProperties({
//isc.defineClass("EntityEditorForm", "VLayout").addProperties({
    //height: 1,
    //padding: 10,

    addButtonDefaults: {
        _constructor: "IButton",
        title: "Add",
        autoFit: true,
        layoutAlign: "right",
        click : function () {
            this.creator.addRecord();
        }
    },

    formDefaults: {
        _constructor: "DynamicForm",
        numCols: 6,
        colWidths: ["*", "*", "*", "*", "*", "*"],
		width: "100%",
        implicitSave: true,
        initWidget : function () {
            this.Super("initWidget", arguments);

            if (this.record && this.relation) {
                var criteria = {};
                if (!this.relation.direction) {
                        // top-level entity - fetch via direct pk criteria
                    criteria[this.relation.baseFieldName] = this.record[this.relation.baseFieldName];
                } else {
                    // we're a child-entity of some parent which refers to us by PK - fetch via a link
                    criteria[this.relation.baseFieldName] = this.record[this.relation.relatedFieldName];
                }
                this.fetchData(criteria);
            }
        },
        implicitSaveCallback : function (data) {
            if (!this.record || !this.record[this.relation.baseFieldName]) {
                this.creator.setRecord(data);
                this.setValues(data);
                this.saveOperationType = "update";
            }
        }
    },

    initWidget : function () {
        this.Super("initWidget", arguments);

        this.addAutoChild("addButton");

        this.addAutoChild("form", 
            isc.addProperties({}, this.formProperties, 
                {
                    dataSource: this.dataSource,
                    title: this.title,
                    record: this.record,
                    relation: this.relation
                }
            )
        );

        if (isc.isA.Portlet(this)) this.addItems([this.addButton, this.form]);
        else this.addMembers([this.addButton, this.form]); 
        
    },

    fetchData : function () {
        var criteria = {};
        if (!this.relation.direction) {
            if (this.isTopLevel()) 
            // top-level entity - fetch via direct pk criteria
                criteria[this.relation.baseFieldName] = this.record[this.relation.baseFieldName];
        } else {
            // we're a child-entity of some parent which refers to us by PK - fetch via a link
            criteria[this.relation.baseFieldName] = this.record[this.relation.relatedFieldName];
        }
        this.form.fetchData(criteria);
    },

    addRecord : function () {
        var criteria = {};
        if (!this.relation.direction) {
            if (!this.isTopLevel()) 
                criteria[this.relation.baseFieldName] = this.record[this.relation.baseFieldName];
        } else {
            // we're a child-entity of some parent which refers to us by PK - fetch via a link
            criteria[this.relation.baseFieldName] = this.record[this.relation.relatedFieldName];
        }

        this.record = criteria;
        this.form.editNewRecord(criteria);
        //this.creator.updateTopLevel();
    },

    isTopLevel : function () {
        return this.relation.relatedDS == null;
    },

    setRecord : function (record) {
        if (this.isTopLevel()) {
            this.creator.updateTopLevel();
        } else {
            this.fetchData();
        }
        this.record = record;
        this.form.record = record;
        if (this.addButton) this.addButton.setDisabled(this.record == null);
    },

    getData : function () {
        return null;
    },
    
    getCriteria : function () {
        return this.form.getValuesAsCriteria();
    },

    enterSearchMode : function (criteria) {
        this.addButton.setDisabled(true);

        this.record = this.form.record = null;
        this.form.implicitSave = false;
        this.form.setData([]);
        if (criteria) this.form.setValues(criteria);
    },

    exitSearchMode : function () {
    }

});

isc.defineClass("EntityEditorGrid", "Portlet").addProperties({
//isc.defineClass("EntityEditorGrid", "VLayout").addProperties({
//    overflow: "visible",
//    autoSize: true,
    //padding: 10,

    addButtonDefaults: {
        _constructor: "IButton",
        title: "Add",
        autoFit: true,
        layoutAlign: "right",
        click : function () {
            this.creator.addRecord();
        }
    },
    
    gridDefaults: {
        _constructor: "ListGrid",
		width: "100%",
        height: "100%",
//        autoFitData: "vertical",
        autoFitMaxRecords: 4,
        autoFetchData: false,
        canEdit: true,
        autoSaveEdits: true,
        initWidget : function () {
            if (this.record && this.relation) {
                this.initialCriteria = {};
                this.initialCriteria[this.relation.baseFieldName] = this.record[this.relation.relatedFieldName];
            }

            this.Super("initWidget", arguments);
        }
    },

    initWidget : function () {
    
        this.addAutoChild("addButton");

        if (!isc.isA.DataSource(this.dataSource)) 
            this.dataSource = isc.DS.get(this.dataSource);

        this.addAutoChild("grid", 
            isc.addProperties({}, this.gridProperties, 
                {
                    dataSource: this.dataSource,
                    title: this.title,
                    record: this.record,
                    relation: this.relation
                }
            )
        );

        if (this.record) this.fetchData();

        if (this.addButton) this.addButton.setDisabled(this.record == null);

        if (isc.isA.Portlet(this)) this.addItems([this.addButton, this.grid]);
        else this.addMembers([this.addButton, this.grid]);

        this.Super("initWidget", arguments);
    },

    setData : function (data) {
    },

    fetchData : function () {
        var criteria = {};
        if (this.record) {
            // we're a child-entity of some parent which refers to us by PK - fetch via a link
            criteria[this.relation.baseFieldName] = this.record[this.relation.relatedFieldName];
        }
        this.grid.fetchData(criteria);
    },

    addRecord : function () {
        var criteria = {};
        // we're a child-entity of some parent which refers to us by PK - fetch via a link
        criteria[this.relation.baseFieldName] = this.record[this.relation.relatedFieldName];

        this.grid.startEditingNew(criteria);
    },

    setRecord : function (record) {
        this.record = record;
        this.fetchData();
        if (this.addButton) this.addButton.setDisabled(this.record == null);
    },
    
    getData : function () {
        return null;
    },

    getCriteria : function () {
        return this.grid.getFilterEditorCriteria();
    },
    
    enterSearchMode : function (criteria) {
        this.addButton.setDisabled(true);

        this.record = this.grid.record = null;
        //this.grid.clearCriteria();
        this.grid.setData([]);
        this.grid.setShowFilterEditor(true);
        //this.grid.redraw();
        //this.redraw();
        if (criteria) this.grid.setCriteria(criteria);
    },

    exitSearchMode : function () {
    }

});


// Entity Editor
// ---------------------------------------------------------------------------------------
// Interface for defining and editing a complete data-structure for a database entity.

//>	@class EntityEditor
// Interface for defining and editing a complete data-structure for a database entity.
//
// @visibility entityEditor
//<
isc.defineClass("EntityEditor", "VLayout");

isc.EntityEditor.addProperties({
    //height: 1,
    membersMargin: 10,
    padding: 10,

    //> @attr entityEditor.dataSource (DataSource : null : IR)
    // The dataSource providing the initial top-level Entity, which we'll check for 
    // relational links and also display the data from those tables in appropriate UIs.
    //
    // @visibility entityEditor
    //<
    dataSource: "",

    modeFormDefaults: {
        _constructor: "DynamicForm",
        height: 1,
        width: "100%",
        fields: [
            {name: "edit", title: "options", defaultToFirstOption: true,
                valueMap: ["Edit", "Search"],
                changed : function (form, item, value) {
                    form.creator.setMode(value);
                }
            }
        ]
    },

    formEntityDefaults: {
        _constructor: "EntityEditorForm",
        height: 1,
        width: "100%"
    },

    gridEntityDefaults: {
        _constructor: "EntityEditorGrid",
        height: 1,
        width: "100%"
    },

    showTabset: false,
    tabsetDefaults: { 
        _constructor: "TabSet",
        width: "100%",
        height: "100%"
    },

    //showPortal: false,
    portalDefaults: { 
        _constructor: "PortalLayout",
        width: "100%",
        height: "100%",
        showColumnMenus: false,
        numColumns: 1
    },

    getDataSourceHierarchy : function (ds, doneList) {
        var result = [];

        if (!isc.isA.DataSource(ds)) 
            ds = isc.DS.getDataSource(ds);

        if (!isc.isA.DataSource(ds)) {
            this.logWarn("No datasource provided..");
            return result;
        } 

        if (!doneList) doneList = [];
        
        if (doneList.contains(ds.ID)) {
            // already processed this DS - skip it now
            return null;
        }

        var rawFields = isc.getValues(ds.getFields()),
            dsFields = []
        ;

        for (var i=0; i<rawFields.length; i++) {
            if (rawFields[i].foreignKey != null) {
                dsFields.add(rawFields[i]);
            }
        }
        
        var dsName = ds.ID,
            localDoneList = [],
            fields = [];

        isc.DS.registerDataSource(ds);

        var childDSArray = ds.getChildDataSources();

        if (childDSArray) {
            for (var i=0; i<childDSArray.length; i++) {
                var foreignDS = childDSArray[i],
                    foreignFields = isc.getValues(foreignDS.getFields())
                ;

                for (var j=0; j<foreignFields.length; j++) {
                    var foreignField = foreignFields[j];

                    if (!foreignField.foreignKey || 
                        isc.DS.getForeignDSName(foreignField, foreignDS) != ds.ID) continue;

                    var localFieldName = isc.DS.getForeignFieldName(foreignField, foreignDS);
                    var field = rawFields.find("name", localFieldName);

                    if (field && foreignField.entityEditMode != "picker") {
                        // this is a relationship from another table into this entity 
                        // (eg, from orderLine.orderId to order.orderId)
                        var thisDSName = dsName,
                            thisFieldName = field.name,
                            baseDSName = foreignDS.ID,
                            baseFieldName = foreignField.name,
                            relationArity = field.relationArity,
                            direction = "out"
                        ;

                        if (!doneList.contains(baseDSName)) {
                            //doneList.add(baseDSName);

                            result.add({
                                baseDS: baseDSName,
                                baseFieldName: baseFieldName,
                                relatedDS: thisDSName,
                                relatedFieldName: thisFieldName,
                                relationArity: field.relationArity
                                //,
                                //relations: this.getDataSourceHierarchy(baseDSName, doneList)
                            });
                        }
                    }
                }
            }
        }

        //doneList = [];
        
        for (var j=0; j<dsFields.length; j++) {
            var field = dsFields[j];

            if (!field.foreignKey) continue;

            var foreignFieldName = isc.DS.getForeignFieldName(field, ds),
                foreignDS = isc.DS.getDataSource(isc.DS.getForeignDSName(field, ds)),
                foreignFields = isc.getValues(foreignDS.getFields());

            var foreignField = foreignFields.find("name", foreignFieldName);

            if (foreignField && field.entityEditMode != "picker") {
                // this is a relationship from this table out to another entity 
                // (eg, from order.customerId to customer.customerId)
                var thisDSName = dsName,
                    thisFieldName = field.name,
                    baseDSName = this.getDSName(foreignDS),
                    baseFieldName = foreignField.name,
                    relationArity = field.relationArity,
                    direction = "out"
                ;

                if (!doneList.contains(baseDSName)) {
                    //doneList.add(baseDSName);

                    result.add({
                        baseDS: baseDSName,
                        baseFieldName: baseFieldName,
                        relatedDS: thisDSName,
                        relatedFieldName: thisFieldName,
                        relationArity: relationArity,
                        direction: direction
                        //,
                        //relations: this.getDataSourceHierarchy(baseDSName, doneList)
                    });
                }
            }
        }

        return result;
    },

    getDSName : function (ds) {
        if (isc.isA.String(ds)) return ds;
        if (isc.isA.DataSource(ds)) return ds.ID;
        return null;
    }



});

isc.EntityEditor.addMethods({

    initWidget: function () {
        this.vertical = true;

        if (!isc.isA.DataSource(this.dataSource)) 
            this.dataSource = isc.DS.getDataSource(this.dataSource);

        if (!this.dataSource) this.logWarn("No dataSource provided - no entity to edit");
        else this.entityTree = this.getEntityTree();

        this.addAutoChild("modeForm");

        this.addAutoChild("tabset");
        this.addAutoChild("portal");
        
        this.addMember(this.modeForm);
        if (this.tabset) this.addMember(this.tabset);
        if (this.portal) this.addMember(this.portal);

        //this.Super("initWidget", arguments);
        
        //this.showEntity();
    },

    getEntityTree : function () {
        if (!isc.isA.DataSource(this.dataSource)) 
            return { baseDS: "NoDSProvided", relations: [] };

        var result = { 
            baseDS: this.dataSource.ID, 
            baseFieldName: this.dataSource.getPrimaryKeyFieldNames()[0], 
            relations: [] 
        };

        result.relations = this.getDataSourceHierarchy(this.dataSource);

        return result;
    },    
    
    fetchDataByPK : function (criteria) {
        if (!this.dataSource) return;

        var _this = this;
        this.dataSource.fetchData( criteria, 
            function (dsResponse, data) {
                _this.fetchDataReply(data);
            }
        ); 
    },

    fetchDataReply : function (data) {
        this.clearEntity();
        this.record = data[0];
        
        this.showEntity();
    },

    clearEntity : function () {
        if (this.entities && this.entities.length > 0) { 
            if (this.showTabset) {
                for (var i=this.tabset.tabs.length-1; i>=0; i--) {
                    this.entities[i].markForDestroy();
                    this.tabset.removeTab(i);
                }
            } else if (this.portal) {
                this.portal.members.removeAll();
                for (var i=this.entities.length-1; i>=0; i--) {
                    this.entities[i].markForDestroy();
                }
            } else {
                for (var i=this.members.length-1; i>=1; i--) {
                    this.removeMember(i);
                    this.entities[i-1].markForDestroy();
                }
            }
        }

        this.entities = [];
        this.record = null;
    },

    showEntity : function () {
        var tree = this.entityTree;

        if (!this.entities) this.entities = [];

        if (!this.entityTree) return;

        this.addEditor(tree);

        this.topLevelComponent = this.entities[0];

        if (tree && tree.relations && tree.relations.length > 0) {
            for (var i=0; i<tree.relations.length; i++) {
                var relation = tree.relations[i];
                if (this.shouldShowEntity(relation.baseDS)) {
                    if (relation.relationArity == "one") {
                        this.addEditor(relation);
                    } else {
                        this.addGrid(relation);
                    }
                }
            }
        }
    },

    updateTopLevel : function () {
        for (var i=1; i<this.entities.length; i++) {
            this.entities[i].setRecord(this.topLevelComponent.record);
        }
    },

    getData : function () {
        return [];
    },

    getDataSourceSpec : function (dsName, baseFieldName) {
        if (this.dataSources) {
            var dsProps = this.dataSources[dsName];
            if (!dsProps) dsProps = this.dataSources[dsName + "!" + baseFieldName];
            return dsProps;
        }
        return null;
    },

    getRelatedEditorProperties : function (dsName, baseFieldName) {
        var dsProps = this.getDataSourceSpec(dsName, baseFieldName);
        if (dsProps) return dsProps.editorProperties;
        return null;
    },

    shouldShowEntity : function (dsName) {
        if (this.dataSources) {
            var dsProps = this.dataSources[dsName],
                undef;
            if (dsProps === undef) return true;
            return (dsProps !== null);
        }
        return true;
    },

    addEditor : function (relation) {
        var entity = this.createAutoChild("formEntity", {
            height: "100%",
            width: "100%",
            dataSource: relation.baseDS,
            title: this.getEntityTitle(relation),
            record: this.record,
            relation: relation,
            formProperties: this.getRelatedEditorProperties(relation.baseDS, relation.relatedFieldName)
        });

        this.addEntityLink(entity);
        this.logWarn("adding linked single-record entity");
    },

    addGrid : function (relation) {
        var entity = this.createAutoChild("gridEntity", {
            height: "100%",
            width: "100%",
            //overflow: "visible",
            dataSource: relation.baseDS,
            title: this.getEntityTitle(relation),
            record: this.record,
            relation: relation,
            gridProperties: this.getRelatedEditorProperties(relation.baseDS, relation.relatedFieldName)
        });

        this.addEntityLink(entity);
        //entity.markForRedraw();

        this.logWarn("added linked multiple-record entity");
    },

    addEntityLink : function (widget) {
        // add the entity to a local array of entities
        this.entities.add(widget);

        if (this.showTabset) {
            this.addEntityTab(widget);
        } else {
            var rel = widget.relation,
                dsProps = this.getDataSourceSpec(rel.baseDS, rel.relatedFieldName),
                rowNum = (dsProps && dsProps.rowNum != null ? dsProps.rowNum : -1),
                offsetInRow = (dsProps && dsProps.offsetInRow != null ? dsProps.offsetInRow : -1)
            ;

            if (this.portal) {
                if (dsProps && dsProps.userHeight != null) widget._userHeight = dsProps.userHeight;
                if (rowNum != -1) {
                    this.portal.getColumn(0).addPortletToExistingRow(widget, rowNum, offsetInRow);
                } else {
                    this.portal.getColumn(0).addPortlet(widget);
                }
            }
            else this.addMember(widget);
        }
    },

    getEntityName : function (relation) {
        var baseDSName = this.getDSName(relation.baseDS),
            relatedDSName = this.getDSName(relation.relatedDS),
            fieldName = relation.relatedFieldName || relation.baseFieldName
        ;

        return baseDSName + "_" + fieldName;
    },
    
    getEntityTitle : function (relation) {
        var dsProps = this.getDataSourceSpec(relation.baseDS, relation.baseFieldName),
            baseDSName = this.getDSName(relation.baseDS),
            relatedDSName = this.getDSName(relation.relatedDS),
            fieldName = relation.relatedFieldName || relation.baseFieldName,
            title
        ;

        if (dsProps && dsProps.entityTitle) {
            title = dsProps.entityTitle;
        } else if (!relatedDSName) {
            title = isc.DS.getAutoTitle(baseDSName);
        } else if (baseDSName != relatedDSName) {
            title = isc.DS.getAutoTitle(baseDSName);
        } else {
            title = isc.DS.getAutoTitle(fieldName);
        }
        
        return title;
    },

    addEntityTab : function (paneWidget) {
        var name = this.getEntityName(paneWidget.relation), 
            title = this.getEntityTitle(paneWidget.relation);

        if (paneWidget) {
            paneWidget.setWidth("100%");
            paneWidget.setHeight("100%");
        }

        this.tabset.addTab({ 
            name: name, 
            title: title, 
            pane: paneWidget, 
            relation: paneWidget.relation 
        });
    },
    
    setMode : function (newMode) {
        //alert("setMode called with new mode: "+newMode);
        var i, entity;
        
        if (newMode == "Search") {
            if (this.entities && this.entities.length > 0) {
                for (i=0; i<this.entities.length; i++) {
                    entity = this.entities[i];
                    if (isc.isA.EntityEditorGrid(entity)) {
                        entity.enterSearchMode(this.getEntityCriteria(entity));
                        entity.markForRedraw();
                    } else if (isc.isA.EntityEditorForm(entity)) {
                        entity.enterSearchMode(this.getEntityCriteria(entity));
                        entity.markForRedraw();
                    }
                }
            }
        } else {
            // gather up the criteria from the various forms and grids
            if (!this.entityCriteria) this.entityCriteria = {};

            if (this.entities && this.entities.length > 0) {
                for (i=0; i<this.entities.length; i++) {
                    entity = this.entities[i];
                    var criteria = entity.getCriteria();

                    if (criteria) {
                        this.entityCriteria[entity.getID()] = 
                            { 
                                relation: entity.relation,
                                criteria: criteria
                            }
                        ;
                    }
                }
                //alert("Search Criteria: \n\n" + isc.echoFull(this.entityCriteria));
            }
        }
    },

    getEntityCriteria : function (entity) {
        if (this.entityCriteria) {
            return this.entityCriteria[entity.getID()];
        }

        return null;
    }

});

}
