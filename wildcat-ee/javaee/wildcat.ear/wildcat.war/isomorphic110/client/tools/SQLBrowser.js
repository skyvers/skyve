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
isc.defineClass("SQLBrowser", "VLayout").addClassProperties({

showWindow : function (windowProps, sqlBrowserProps) {
    isc.Window.create({
        title: "SQL Browser",
        width: "100%",
        height: "100%",
        canDragReposition: false,
        closeClick : function () { this.destroy(); },
        items: [
            isc.SQLBrowser.create({autoDraw: false}, sqlBrowserProps)
        ]
    }, windowProps).show();
}

});

isc.SQLBrowser.addProperties({

dbListDefaults: {
    _constructor: "DBList",
    height: 150,
    canDragSelectText: true,
    autoFetchData: true,
    canHover: true,
    defaultFields: [
        {name: "name"},
        {name: "status"}
    ]
},

dbListRefreshButtonDefaults: {
    _constructor: "Img",
    size: 16,
    src: "[SKIN]/actions/refresh.png",    
    click: "this.creator.dbList.invalidateCache()"
},

dbSchemaTreeDefaults: {
    _constructor: "DBSchemaTree",
    canDragSelectText: true,
    animateFolders: false,
    showConnectors: false,
//    showAllRecords: true,
    recordClick : function (viewer, record) {
        this.creator.showTablePane(record);
    }
},

dbSchemaRefreshButtonDefaults: {
    _constructor: "Img",
    size: 16,
    src: "[SKIN]/actions/refresh.png",    
    click: "this.creator.dbSchemaTree.invalidateCache()"
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
             contents: "Select a database on the left..."
         })
        }
    ]
},

autoChildren: ["mainLayout"],

initWidget : function () {
    this.Super("initWidget", arguments);

    this.dbList = this.createAutoChild("dbList", {
        selectionChanged : "if (state) this.creator.databaseChanged(record)"
    });
    this.dbListRefreshButton = this.createAutoChild("dbListRefreshButton");

    this.dbSchemaTree = this.createAutoChild("dbSchemaTree", {

    });
    this.dbSchemaRefreshButton = this.createAutoChild("dbSchemaRefreshButton");

    this.leftSection = this.createAutoChild("leftSection", {
        sections: [
            {name: "databases", title: "Databases", expanded: true, controls: [this.dbListRefreshButton], items: [
                this.dbList                       
            ]},
            {name: "tables", title: "Tables & Views", expanded: true, controls:[this.dbSchemaRefreshButton], items: [
                this.dbSchemaTree                       
            ]}
        ]
    });

    this.addAutoChildren(this.autoChildren);
    this.mainLayout.addMember(this.leftSection);

    this.rightPane = this.createAutoChild("rightPane");
    this.mainLayout.addMember(this.rightPane);
},

dbPaneDefaults: {
    _constructor: "DBPane"
},
showDBPane : function () {
    var db = this.db;    
    this.showPane({ID: this.escapeForId("db_"+db.name), title: db.name, paneClass: "dbPane"}, db);
},
databaseChanged : function (db) {
    if (db.status == "OK") {
        this.db = db;
        this.dbSchemaTree.loadSchema(db);
        this.showDBPane();
    }
},


showTablePane : function (record) {
    this.showDBPane();
    this.currentPane.showTableBrowser(record);
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
