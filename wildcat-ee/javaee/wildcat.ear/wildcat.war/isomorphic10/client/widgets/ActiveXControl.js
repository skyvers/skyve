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

 






//>	@class ActiveXControl
//  
//  ISC Abstraction for ActiveX controls
//
//  @treeLocation Client Reference/Client Bridges
//  @requiresModules PluginBridges
//  @visibility PluginBridges
//<

isc.ClassFactory.defineClass("ActiveXControl", "BrowserPlugin");

isc.ActiveXControl.addProperties({

    //> @attr activeXControl.id (string : null : IR)
    //
    //  Sets the 'id' attribute on the object.  If a name is not provided it will be
    //  auto-generated.  Note that in general you don't need to set this.  If you have a reference to
    //  your ISC ActiveX control object you can simply call
    //  +link{method:ActiveXControl.getPluginHandle()} to get a handle to the element.
    //
    //  @see method:ActiveXControl.getPluginHandle()
    //  @see method:ActiveXControl.getPluginID()
    //
    //  @visibility PluginBridges
    //<

    //> @attr activeXControl.params (object : null : IR)
    //
    //  A map of key/value pairs to pass to the Active X control as parameters.
    //
    //  @visibility PluginBridges
    //<

    //> @attr activeXControl.uuid (string : null : IR)
    //
    //  Set this to the uuid of your Active X control - ISC will then generate the appropriate
    //  classID entry for you.
    //
    //  @visibility PluginBridges
    //<

    //> @attr activeXControl.classID (string : null : IR)
    //
    //  This sets the value of the classID property on the object.  This is meant to give you
    //  complete control over the generated HTML.  In practice it may be more handy to set the uuid
    //  property on this object and let the classID be generated from that.
    //
    //  @see attr:ActiveXControl.uuid
    //
    //  @visibility PluginBridges
    //<
    
    //> @attr activeXControl.codeBase (URL : null : IR)
    //
    //  Specifies the URL from which to load the ActiveX control.
    //
    //  @visibility PluginBridges
    //<

getInnerHTML : function () {

    var accum = isc.StringBuffer.create();

    // if classID is defined, use that verbatim - othwerwise assemble the classID using the 
    // specified uuid.
    var classID = this.classID ? this.classID : "clsid:"+this.uuid;

    accum.append("<object classid='", classID, "' codebase='", this.codeBase,
                "' id='"+this.getPluginID(), "' width='100%' height='100%'");

    // add extraHTML if any
    if (this.extraHTML) accum.append(" ", this.extraHTML);
    accum.append(">"); // close object tag

    accum.append("<param name='iscCanvasID' value='", this.getID(), "'>");
    
    if (this.params) {
        for (var key in this.params) {
            accum.append("<param name='", key, "' value='", this.params[key], "'>");
        }
    }

    accum.append("</object>");

    return accum.release(false);
},

//> @method activeXControl.getPluginID()   
//  Returns the ID for this ISC ActiveX control object.
//  If the <code>id</code> property was specified for the object, that will be used, otherwise 
//  the ID will be auto-generated.
//  @visibility PluginBridges
//<
getPluginID : function() {
    if(!this.id) this.id = this.getID() + "_activeXControl";
    return this.id;
},

//> @method activeXControl.getPluginHandle()   
//  Returns a handle to the element for this ISC ActiveX control object.
// @return (HTML Element) pointer to the plugin element in the DOM
// @visibility PluginBridges
//<
getPluginHandle : function() {
    return window[this.getPluginID()];
}

});