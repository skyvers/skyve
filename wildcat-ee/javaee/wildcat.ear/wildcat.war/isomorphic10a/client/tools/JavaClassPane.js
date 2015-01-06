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

isc.defineClass("JavaClassPane", "VLayout").addProperties({

sourceViewDefaults: {
    _constructor: "HTMLFlow",
    autoDraw: false,
    height: "*"
},

initWidget : function () {
    this.Super("initWidget", arguments);
    
    this.sourceView = this.createAutoChild("sourceView", {
        contents: "Loading..."
    });
    this.addMember(this.sourceView);

    this.loadSource();
},

loadSource : function () {
    isc.DMI.call("isc_builtin", "com.isomorphic.tools.BuiltinRPC", "getJavaSource", 
                 this.config.path,
                 this.getID()+".loadSourceReply(data)");
},    

loadSourceReply : function (data) {
    var sh = isc.JSSyntaxHiliter.create();
    this.sourceView.setContents(sh.hilite(data));
}

});     