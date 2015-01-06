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


isc.defineClass("RobotServerPicker", "Window").addProperties({

autoCenter: true,
autoSize: true, 
isModal: true,
title: "Select Robot Server",


formConstructor: "DynamicForm",
formDefaults: {
    width: 300,
    numCols: 2,
    colWidths: [150, "*"],
    defaultItems: [
        {name: "robotServerURL", title: "Robot Server URL", defaultValue: "http://127.0.0.1:50080"},
//        {name: "robotServerURL", title: "Robot Server URL", defaultValue: "http://10.10.1.161:50080"},
        {name: "next", type: "button", title: "Next", click : "form.creator.nextClick()", startRow: true},
        {name: "cancel", type: "button", title: "Cancel", click: "form.creator.hide()", endRow: false, startRow: false}
    ]
},

myAutoChildren: ["form"],

initWidget : function () {
    this.Super("initWidget", arguments);
    this.form = this.createAutoChild("form");
    this.addItem(this.form);
},

nextClick : function () {
    var robotServerURL = this.form.getValue("robotServerURL");
    window.robotServerURL = robotServerURL;
    this.hide();
    this.fireCallback("robotServerSelected", "robotServerURL", [robotServerURL]);
}

});
isc.RobotServerPicker.registerStringMethods({
    robotServerSelected: "robotServerURL"
});
