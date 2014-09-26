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

isc.defineClass("SQLTableBrowser", "VLayout").addProperties({

previewGridDefaults: {
    _constructor: "ListGrid",
    canDragSelectText: true,
    autoFetchData: false,
    height: "*",
    minFieldWidth: 100,
    showFilterEditor: true,
    canEdit: true,
    dataProperties: {
        progressiveLoading: true
    }
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

    members: ["autoChild:removeButton", "autoChild:addButton", "autoChild:exportButton", "autoChild:generateDSButton",
              "starSpacer",
              "autoChild:refreshButton", "autoChild:totalRowsIndicator"
    ]
},

initWidget : function () {
    this.Super("initWidget", arguments);

    var ds = isc.DataSource.get("DataSourceStore");
    ds.performCustomOperation("dsFromTable", {schema:this.schema,dbName: this.dbName, tableName: this.config.name}, this.getID()+".dsLoaded(data)");
},


dsLoaded : function (data) {
    this.dataSource = data.ds;
    this.dataSourceXML = data.dsXML;

    this.addAutoChild("previewGrid", {
        dataSource: this.dataSource
    });
    this.addAutoChild("previewGridStrip", {
        grid: this.previewGrid
    });
    this.previewGrid.filterData();
},

showDS : function () {
    // server returns a bunch of internal flags as part of <DataSource> tag, so rebuild that
    // part from our config.
    var dsXMLHeader = "<DataSource ID=\""+this.config.name+"\" serverType=\"sql\" dbName=\""+this.dbName+"\" tableName=\""+this.config.name+"\"";
    if (this.schema) dsXMLHeader += " schema=\""+this.schema+"\"";
    dsXMLHeader += ">";

    var dsXML = this.dataSourceXML;
    dsXML = dsXML.substring(dsXML.indexOf(">")+1);
    dsXML = dsXMLHeader + dsXML;

    
    isc.Window.create({
        title: "DataSource XML for table: " + this.config.name,
        autoDraw: true,
        autoSize: true,
        autoCenter: true,
        items: [
            isc.DynamicForm.create({
                numCols: 1,
                width: 600,
                height: 600,
                autoFocus: true,
                selectOnFocus: true,
                fields: [
                    {name: "dsData", showTitle: false, type: "textArea", wrap: isc.TextAreaItem.OFF, defaultValue: dsXML, width: "*", height: "*"}
                ],
                closeClick : function () {
                     this.destroy();
                     return false;
                }
            })
        ]
    });
}

});
