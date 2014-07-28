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
isc.defineClass("SQLEditor", "VLayout").addProperties({

sqlInputFormDefaults: {
    _constructor: "DynamicForm",
    height: 150,
    showResizeBar: true
},

actionButtonsDefaults: {
    _constructor: "HLayout",
    layoutMargin: 5,
    membersMargin: 5,
    height: 20
},

execSQLButtonDefaults: {
    _constructor: "IButton",
    title: "Exec SQL",
    click: "this.creator.execSQL();",
    autoParent: "actionButtons"
},

previewGridDefaults: {
    _constructor: "ListGrid",
    dataProperties: {
        progressiveLoading: true
    },
    minFieldWidth: 100,
    autoFetchData: false
},

previewGridStripDefaults: {
    _constructor: "GridToolStrip",
    width: "100%",

    generateDSButtonDefaults: {
        _constructor: "IAutoFitButton",
        title: "Show DataSource",
        layoutAlign: "center",
        click: "this.creator.creator.showDS()"
    },

    members: ["autoChild:exportButton",
              "starSpacer",
              "autoChild:refreshButton", "autoChild:totalRowsIndicator"
    ]
},

initWidget : function () {
    this.Super("initWidget", arguments);

    var sqlEditor = this;
    this.addAutoChild("sqlInputForm", {
        fields: [
            {name: "sql", showTitle: false, type: "textarea",
             width: "*", height: "*", colSpan: "*",
             keyPress:function (item, form, keyName) {
                if (keyName == 'Enter' && isc.EH.ctrlKeyDown()) {
                   if (isc.Browser.isSafari) item.setValue(item.getElementValue());
                   sqlEditor.execSQL();
                   if (isc.Browser.isSafari) return false;
                }
            }}
        ]
    });

    this.addAutoChildren(["actionButtons", "execSQLButton"]);
},

execSQL : function () {
    var sql = this.sqlInputForm.getValue("sql");
    if (sql) {
        // strip whitespaces and trailing semicolons - these produce a syntax error when passed
        // to the JDBC tier
        sql = sql.trim().replace(/(.*);+/, "$1");
        var ds = isc.DataSource.get("DataSourceStore");
        ds.performCustomOperation("dsFromSQL", {dbName: this.config.name, sql: sql}, this.getID()+".dsLoaded(data)");
    }
},

dsLoaded : function (data) {
    var ds = data.ds;
    if (!this.previewGrid) this.addAutoChild("previewGrid", {dataSource: ds});
    else this.previewGrid.setDataSource(ds);
    this.previewGrid.fetchData();

    this.addAutoChild("previewGridStrip", {
        grid: this.previewGrid
    });
}

});
