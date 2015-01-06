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

isc.defineClass("DBCompactList", "DynamicForm").addProperties({

width: 200,
numCols: 2,
colWidths: [80, "*"],

initWidget : function () {
    this.fields = [{
        name: "dbName", title: "Database", type: "select", width: "*", type: "select", width: 120,
        optionDataSource: "DBListDS", displayField: "name", valueField: "name",
        change:"if (this.form.databaseChanged) this.form.fireCallback('databaseChanged', 'dbName', [value])", valueMap: {}
    }];
    this.Super("initWidget", arguments);
},

getSelectedDB : function () {
    return this.getValue("dbName");
},

setSelectedDB : function (db) {
    return this.setValue("dbName", db);
}

});

isc.DBCompactList.registerStringMethods({
    databaseChanged: "dbName"
});  