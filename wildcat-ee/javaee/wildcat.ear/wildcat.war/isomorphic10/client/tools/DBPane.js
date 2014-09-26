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

isc.defineClass("DBPane", "TabSet").addProperties({

sqlEditorDefaults: {
    _constructor: "SQLEditor"
},


initWidget : function () {
    this.Super("initWidget", arguments);

    this.sqlEditor = this.createAutoChild("sqlEditor", {config: this.config});
    this.addTab({title: "SQL Editor", pane: this.sqlEditor});
},

tablePaneDefaults: {
    _constructor: "SQLTableBrowser"
},
showTableBrowser : function (table) {
    var tabId = this.escapeForId(this.config.name+'_'+table.name);
    this.showPane({ID: tabId, title: table.name, paneClass: "tablePane"}, table);    
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

    isc.addProperties(tab, props, {canClose: true, pane: this.createAutoChild(props.paneClass, {config:childConfig,dbName:this.config.name})});

    this.addTab(tab);
    this.selectTab(tab);
    this.currentPane = tab.pane;
}

});
