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

// =======================================================================================
// DSRegistryList
// =======================================================================================
isc.defineClass("DSRegistryList", "ListGrid").addProperties({

dataSource: "RepoRegistry",

initWidget : function () {
    this.Super("initWidget", arguments);
}

});

// =======================================================================================
// DSList
// =======================================================================================
isc.defineClass("DSList", "ListGrid").addProperties({

initWidget : function () {
    this.Super("initWidget", arguments);
}

});


// =======================================================================================
// DSBrowser
// =======================================================================================
isc.defineClass("DSBrowser", "VLayout").addClassProperties({

showWindow : function (windowProps, componentProps) {
    isc.Window.create({
        title: "DS Builder",
        width: "100%",
        height: "100%",
        canDragReposition: false,
        closeClick : function () { this.destroy(); },
        items: [
            isc.DSBrowser.create({autoDraw: false}, componentProps)
        ]
    }, windowProps).show();
}

});

isc.DSBrowser.addProperties({

dsRegistryListDefaults: {
    _constructor: "DSRegistryList",
    height: 150,
    autoFetchData: true,
    canHover: true,
    defaultFields: [
        {name: "ID", title: "Name"}
    ],
    recordClick : "this.creator.dsRegistryChanged(record)"
},

dsRegistryListRefreshButtonDefaults: {
    _constructor: "Img",
    size: 16,
    src: "[SKIN]/actions/refresh.png",    
    click: "this.creator.dsRegistryList.invalidateCache()"
},


dsListDefaults: {
    _constructor: "DSList",
    canHover: true,
    showFilterEditor: true,
    defaultFields: [
        {name: "ID", title: "Name"}
    ],
    recordClick : "this.creator.dsChanged(record)"
},

dsListAddButtonDefaults: {
    _constructor: "Img",
    size: 16,
    src: "[SKIN]/actions/add.png",
    click: "this.creator.dsList.startEditingNew()"
},
dsListRemoveButtonDefaults: {
    _constructor: "Img",
    size: 16,
    src: "[SKIN]/actions/remove.png",    
    click: "this.creator.dsList.removeSelectedData()"
},
dsListRefreshButtonDefaults: {
    _constructor: "Img",
    size: 16,
    src: "[SKIN]/actions/refresh.png",    
    click: "this.creator.dsList.invalidateCache()"
},


leftSectionDefaults: {
    _constructor: "SectionStack",
    headerHeight: 25,
    width: 300,
    showResizeBar: true,
    animateSections: isc.Browser.isSafari,
    visibilityMode: "visible",
    autoParent: "mainLayout"
},

mainLayoutDefaults: {
    _constructor: "HLayout",
    height: "*"
},

rightPaneDefaults: {
    _constructor: "TabSet",
    tabs: [
        {name: "welcome", title: "Welcome", ID: "dsb_welcome_tab", canClose: true, 
         pane: isc.Label.create({
             height: 10,
             autoDraw: false,
             overflow: "visible",
             contents: "Select a datasource registry on the left..."
         })
        }
    ]
},

autoChildren: ["mainLayout"],


initWidget : function () {
    this.Super("initWidget", arguments);

    this.dsRegistryList = this.createAutoChild("dsRegistryList");
    this.dsRegistryListRefreshButton = this.createAutoChild("dsRegistryListRefreshButton");

    this.dsList = this.createAutoChild("dsList");
    this.dsListAddButton = this.createAutoChild("dsListAddButton");
    this.dsListRemoveButton = this.createAutoChild("dsListRemoveButton");
    this.dsListRefreshButton = this.createAutoChild("dsListRefreshButton");

    this.leftSection = this.createAutoChild("leftSection", {
        sections: [
            {name: "registries", title: "DataSource Registries", expanded: true, controls: [this.dsRegistryListRefreshButton], items: [
                this.dsRegistryList                       
            ]},
            {name: "datasources", title: "DataSources", expanded: true, controls:[this.dsListAddButton,this.dsListRemoveButton,this.dsListRefreshButton], items: [
                this.dsList
            ]}
        ]
    });

    this.addAutoChildren(this.autoChildren);
    this.mainLayout.addMember(this.leftSection);

    this.rightPane = this.createAutoChild("rightPane");
    this.mainLayout.addMember(this.rightPane);
},


dsRegistryChanged : function (record) {
    this.currentRegistry = record;
    isc.DMI.call("isc_builtin", "com.isomorphic.tools.BuiltinRPC", "dsFromXML", 
                 record.object,
                 this.getID()+".dsLoaded(data)");    
},
dsLoaded : function (ds) {
    this.currentDS = ds;
    this.showDSRegistryPane();
    this.dsList.setDataSource(ds);
    this.dsList.setFields([
        {name: "ID", title: "Name"}
    ]);
    this.dsList.filterData();
},
dsRegistryPaneDefaults: {
    _constructor: "DSRegistryPane"
},
showDSRegistryPane : function () {
    var registry = this.currentRegistry;
    this.showPane({ID: this.escapeForId("registryPane_"+registry.ID), title: registry.ID, paneClass:"dsRegistryPane"},registry);
},


dsChanged : function (record) {
    this.currentDS = record;
    this.showDSPane();
},
showDSPane : function () {
    var ds = this.currentDS;
    this.showDSRegistryPane();
    var props = {};
    isc.addProperties(props, ds, {registry: isc.clone(this.currentRegistry)});
    this.currentPane.showDSPane(props);
},


escapeForId : function (s) {
    return isc.isA.String(s) ? s.replace(/(\/|\.)/g, '_') : s;
},

showPane : function (props, childConfig) {
    var tab = this.rightPane.getTab(props.ID);
    if (tab) {
        this.currentPane = tab.pane;
        this.rightPane.selectTab(tab);
        return;
    }
    tab = {};

    isc.addProperties(tab, props, {canClose: true, pane: this.createAutoChild(props.paneClass, {config:childConfig})});

    var firstTab = this.rightPane.getTab(0);
    if (firstTab && firstTab.name == "welcome") this.rightPane.removeTab(0);

    this.rightPane.addTab(tab);
    this.rightPane.selectTab(tab);
    this.currentPane = tab.pane;
}

});


// ==================================================================================================================
// DSRegistryPane
// ==================================================================================================================
isc.defineClass("DSRegistryPane", "TabSet").addProperties({

initWidget : function () {
    this.Super("initWidget", arguments);
},


dsPaneDefaults: {
    _constructor: "DSEditor"
},
showDSPane : function (ds) {
    var tabId = this.escapeForId("dsPane_"+this.config.ID+'_'+ds.ID);
    this.showPane({ID: tabId, title: ds.ID, paneClass: "dsPane"}, ds);
},
escapeForId : function (s) {
    return isc.isA.String(s) ? s.replace(/(\/|\.)/g, '_') : s;
},
showPane : function (props, childConfig) {
    var tab = this.getTab(props.ID);
    if (tab) {
        this.selectTab(tab);
        return;
    }
    tab = {};

    isc.addProperties(tab, props, {canClose: true, pane: this.createAutoChild(props.paneClass, {config:childConfig})});

    this.addTab(tab);
    this.selectTab(tab);
    this.currentPane = tab.pane;
}

});



// ==================================================================================================================
// DSRegistryPane
// ==================================================================================================================
isc.defineClass("DSEditor", "SectionStack").addProperties({

visibilityMode: "visible",

fieldGridDefaults: {
    _constructor: "ListGrid",
    canReorderRecords: true,
    canDragRecordsOut: false,
    canEdit: true,
    autoSaveEdits: true
},

fieldGridAddButtonDefaults: {
    _constructor: "Img",
    size: 16,
    src: "[SKIN]/actions/add.png",
    click: "this.creator.fieldGrid.startEditingNew()"
},
fieldGridRemoveButtonDefaults: {
    _constructor: "Img",
    size: 16,
    src: "[SKIN]/actions/remove.png",    
    click: "this.creator.fieldGrid.removeSelectedData()"
},
fieldGridRefreshButtonDefaults: {
    _constructor: "Img",
    size: 16,
    src: "[SKIN]/actions/refresh.png",    
    click: "this.creator.fieldGrid.invalidateCache()"
},


deriveFormDefaults: {
    _constructor: "DynamicForm"
},

dbListDefaults: {
    _constructor: "DBCompactList"
},

showSQLBrowserButtonDefaults: {
    _constructor: "IButton",
    title: "Show SQL Browser",
    width: 150,
    click: function () {
       isc.SQLBrowser.showWindow({
           width: "95%",
           height: "95%",
           isModal: true,
           autoCenter: true
       });
    }
},

fetchOperationFormDefaults: {
    _constructor: "DynamicForm",
    fields: [
        //{name: "customSQL", title: "Custom SQL", type: "textarea"},
        {name: "selectClause", title: "SELECT", formItemType: "AutoFitTextAreaItem", height: 10, width: "*", colSpan: "*", defaultValue: "*"},
        {name: "tableClause", title: "FROM", formItemType: "AutoFitTextAreaItem", height: 10, width: "*", colSpan: "*", defaultValue: ""},
        {name: "whereClause", title: "WHERE", formItemType: "AutoFitTextAreaItem", height: 10, width: "*", colSpan: "*", defaultValue: "$defaultWhereClause"},
        {name: "groupClause", title: "GROUP BY", formItemType: "AutoFitTextAreaItem", height: 10, width: "*", colSpan: "*", defaultValue: "$defaultGroupClause"},
        {name: "orderClause", title: "ORDER BY", formItemType: "AutoFitTextAreaItem", height: 10, width: "*", colSpan: "*", defaultValue: "$defaultOrderClause"}        
    ]
},

actionBarDefaults: {
    _constructor: "HLayout",
    height: 20
},

tryButtonDefaults: {
    _constructor: "IButton",
    title: "Try it",
    click: "this.creator.tryIt()",
    autoParent: "actionBar"
},

saveButtonDefaults: {
    _constructor: "IButton",
    title: "Save",
    click: "this.creator.saveDS()",
    autoParent: "actionBar"
},

previewGridDefaults: {
    _constructor: "ListGrid",
    showFilterEditor: true    
},


initWidget : function () {
    this.Super("initWidget", arguments);

    this.fieldGrid = this.createAutoChild("fieldGrid", {
        fields: [
            //{name: "showIf", title: "Visible", canToggle: true, type: "boolean", width: 50},
            {name: "title", title: "Title"},
            {name: "name", title: "Name"},
            {name: "width", title: "Width"},
            {name: "height", title: "Height"},
            {name: "operator", title: "Operator", valueMap : [
                "equals",
                "notEqual",
                "greaterThan",
                "lessThan",
                "greaterOrEqual",
                "lessOrEqual",
                "contains",
                "startsWith",
                "endsWith",
                "iContains",
                "iStartsWith",
                "iEndsWith",
                "notContains",
                "notStartsWith",
                "notEndsWith",
                "iNotContains",
                "iNotStartsWith",
                "iNotEndsWith",
                "regexp",
                "iregexp",
                "isNull",
                "notNull",
                "inSet",
                "notInSet",
                "equalsField",
                "notEqualField",
                "and",
                "not",
                "or",
                "between",
                "betweenInclusive"
            ]},
            {name: "formItemType", title: "Form Item Type"},
            {name: "tableName", title: "Table Name"},
            {name: "type", title: "Type"}
        ]
    });

    this.fieldGridAddButton = this.createAutoChild("fieldGridAddButton");
    this.fieldGridRemoveButton = this.createAutoChild("fieldGridRemoveButton");
    this.addSection({
        ID: "fields", title: "Fields", expanded: true, items: [this.fieldGrid],
        controls: [this.fieldGridAddButton, this.fieldGridRemoveButton]
    });

    var dsEditor = this;
    this.deriveForm = this.createAutoChild("deriveForm", {
        fields: [
            {name: "sql", showTitle: false, formItemType: "AutoFitTextAreaItem",
             width: "*", height: 40, colSpan: "*",
             keyPress:function (item, form, keyName) {
                if (keyName == 'Enter' && isc.EH.ctrlKeyDown()) {
                   if (isc.Browser.isSafari) item.setValue(item.getElementValue());
                   dsEditor.execSQL();
                   if (isc.Browser.isSafari) return false;
                }
            }},
            {type: "button", title: "Execute", startRow: true, click: this.getID()+".execSQL()"}
        ]
    });
    this.dbList = this.createAutoChild("dbList");
    this.showSQLBrowserButton = this.createAutoChild("showSQLBrowserButton");
    this.addSection({
        ID: "derive", title: "Derive Fields From SQL", expanded: false, items: [this.deriveForm],
        controls: [this.dbList, this.showSQLBrowserButton]
    });

    this.fetchOperationForm = this.createAutoChild("fetchOperationForm");
    this.addSection({ID: "fetchOperation", title: "Fetch Operation", expanded: true, items: [this.fetchOperationForm]});

    this.actionBar = this.createAutoChild("actionBar");    
    this.addSection({ID: "actionBar", showHeader: false, expanded: true, items: [this.actionBar]});
    this.addAutoChildren(["tryButton", "saveButton"]);

    this.previewGrid = this.createAutoChild("previewGrid");
    this.addSection({ID: "preview", title: "Preview", items: [this.previewGrid]});

    this.loadDS(this.config);
},

execSQL : function () {
    var sql = this.deriveForm.getValue("sql");
    if (sql) {
        // strip whitespaces and trailing semicolons - these produce a syntax error when passed
        // to the JDBC tier
        sql = sql.trim().replace(/(.*);+/, "$1");
        var ds = isc.DataSource.get("DataSourceStore");
        ds.performCustomOperation("dsFromSQL", {dbName: this.dbList.getSelectedDB(), sql: sql}, this.getID()+".deriveDSLoaded(data)");
    }
},

deriveDSLoaded : function (data) {
    var ds = data.ds;
    this.dsLoaded(data.ds);
},

loadDS : function (record) {
    this.currentRegistry = record;
    isc.DMI.call("isc_builtin", "com.isomorphic.tools.BuiltinRPC", "dsConfigFromXML", 
                 record.object,
                 this.getID()+".dsLoaded(data)");     
},
dsLoaded : function (dsConfig) {
    var ds = isc.DataSource.create(dsConfig);
    this.currentDS = ds;
    ds.repo = this.config.registry.ID;

    this.deriveFields(ds);
    this.previewGrid.setDataSource(ds);
    if (ds.dbName) this.dbList.setSelectedDB(ds.dbName);
 
    var ob = ds.operationBindings;
    if (ob && ob.length > 0) {
        this.fetchOperationForm.setValues(ob[0]);
    }
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
    this.fieldGrid.setData(newFields);
},

tryIt : function () {
    var dsConfig = this.buildDSConfig(this.config.ID+"_test");

    var ds = isc.DataSource.get("DataSourceStore");
    ds.performCustomOperation("dsFromConfig", {config: dsConfig}, this.getID()+".tryItCallback(data)");    
},

tryItCallback : function (data) {
    this.expandSection("preview");
    this.previewGrid.setDataSource(data.ds);
    this.previewGrid.filterData();    
},

saveDS : function () {
    var dsConfig = this.buildDSConfig(this.config.ID);

    var ds = isc.DataSource.get("DataSourceStore");
    ds.performCustomOperation("dsFromConfig", {config: dsConfig}, this.getID()+".xmlLoaded(data)");    
},

xmlLoaded : function (data) {
    var repoDS = isc.DataSource.get(this.config.registry.ID);
    repoDS.updateData({pk: this.config.pk, object: data.dsXML});
},

buildDSConfig : function (ID) {
    var dsConfig = {
        ID: ID,
        serverType: "sql",
        dbName: this.dbList.getSelectedDB(),
        __autoConstruct: "DataSource",
        operationBindings: [
            isc.addProperties({operationType: "fetch",skipRowCount:"true",qualifyColumnNames:false}, this.fetchOperationForm.getValues())
        ],
        fields: this.fieldGrid.data
    };
    return dsConfig;
}

});