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

isc.defineClass("ClassBrowser", "VLayout").addClassProperties({

showWindow : function (windowProps, props) {
    isc.Window.create({
        title: "Class Browser",
        width: "100%",
        height: "100%",
        canDragReposition: false,
        closeClick : function () { this.destroy(); },
        items: [
            isc.ClassBrowser.create({autoDraw: false}, props)
        ]
    }, windowProps).show();
}

});

isc.ClassBrowser.addProperties({


classTreeDefaults: {
    _constructor: "JVMClassTree",
    autoDraw: false,
    autoFetchData: true,
    recordDoubleClick : function (viewer, record) {
        if (this.data.isLeaf(record)) this.creator.showClassPane(record);
    }
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
             contents: "Select a class on the left..."
         })
        }
    ]
},

autoChildren: ["mainLayout"],

initWidget : function () {
    this.Super("initWidget", arguments);

    this.classTree = this.createAutoChild("classTree", {
        selectionChanged : "if (state) this.creator.classChanged(record)"
    });

    this.leftSection = this.createAutoChild("leftSection", {
        sections: [
            {name: "classes", title: "Classes", expanded: true, controls: [], items: [
                this.classTree                       
            ]}
        ]
    });

    this.addAutoChildren(this.autoChildren);
    this.mainLayout.addMember(this.leftSection);

    this.rightPane = this.createAutoChild("rightPane");
    this.mainLayout.addMember(this.rightPane);
},

classChanged : function (record) {
    this.showClassPane(record);
},

classPaneDefaults: {
    _constructor: "JavaClassPane"
},
showClassPane : function (record) {
    var tabId = "class_"+this.escapeForId(record.path);
    this.showPane({ID: tabId, title: "Class: "+record.name, paneClass: "classPane"}, record);
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