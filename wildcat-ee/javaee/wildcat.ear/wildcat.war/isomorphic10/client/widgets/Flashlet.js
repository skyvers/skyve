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

 






//>	@class Flashlet
//  
//  ISC abstraction for Flashlets. 
//
//  @treeLocation Client Reference/Client Bridges
//  @requiresModules PluginBridges
//  @visibility PluginBridges    
//<


isc.ClassFactory.defineClass("Flashlet", "BrowserPlugin");

isc.Flashlet.addClassMethods({

    

    //> @classMethod Flashlet.flashAvailable()
    // Is Shockwave Flash installed on this browser?
    // @return (boolean) <code>true</code> if Flash is installed; <code>false</code> otherwise.
    // @visibility PluginBridges
    //<
    flashAvailable : function () {
        if (this.flashSupported != null) return this.flashSupported;
        isc.Flashlet.flashVersion = 0;
        isc.Flashlet.flashSupported = false;
        if (window.ActiveXObject) {
            try {
                var flashControl = new ActiveXObject("ShockwaveFlash.ShockwaveFlash");
                if (flashControl) {
                    var version = flashControl.GetVariable("$version")
                                              .replace(/[^0-9]+[\s]+([0-9]+)[,.][\s]*([0-9]+).*/, "$1.$2");
                    isc.Flashlet.flashSupported = true;
                    isc.Flashlet.flashVersion = parseFloat(version);
                }
            } catch (e) {
                this.logInfo("Unable to create sample flash ActiveX object: " + e);
            }

        
        } else {
            var flashPlugin;
            if (navigator.mimeTypes) {
                var mimeType = navigator.mimeTypes["application/x-shockwave-flash"];
                flashPlugin = mimeType && mimeType.enabledPlugin;
            } else {
                flashPlugin = navigator.plugins["Shockwave Flash"];
                // Special case - version 2.0 had its own name
                if (flashPlugin == null) flashPlugin = navigator.plugins["Shockwave Flash 2.0"];
            }

            if (flashPlugin != null) {
                this.flashSupported = true;
                // Note: plugin.description is always of the form:
                // "Shockwave Flash 9.0 r22"
                // Trim off the first 16 chars to get at the full version string
                var versionString = flashPlugin.description.substring(16),
                // Resolve the versionString to a float to get the version number
                    versionNum = parseFloat(versionString.split(" ")[0]);

                this._fullFlashVersion = versionString;
                this.flashVersion = versionNum;
            } else {
                this.flashSupported = false;
            }
        }

        return this.flashSupported;
    },

    //> @classMethod Flashlet.getFlashVersion()
    // Which version of Flash is installed on this browser?
    // @return (number) flash version number, or null if flash is not installed
    // @visibility PluginBridges
    //<
    getFlashVersion : function () {
        // flashAvailable() will set up flashVersion if it's not already cached.
        if (this.flashAvailable()) return this.flashVersion;
    }
});

isc.Flashlet.addProperties({
    
    useClipDiv: false,

    // Flashlets do not burn through or swallow events in either FF or IE
    useDragMask: false,

    //> @attr flashlet.name (string : null : IR)
    //
    //  Sets the 'name' attribute on the flashlet object.  If a name is not provided it will be
    //  auto-generated.  Note that in general you don't need to set this.  If you have a handle to
    //  your ISC Flashlet object you can simply call +link{method:Flashlet.getPluginHandle()} to get a
    //  handle to the element.
    //
    //  @see method:Flashlet.getPluginHandle()
    //
    //  @visibility PluginBridges
    //<
    

    //> @attr flashlet.params (object : null : IR)
    //
    //  A map of key/value pairs to pass to the flashlet as parameters.  Note that these will be set
    //  on the outer &lt;object&gt; element as well as the inner &lt;embed&gt; element.
    //
    //  @visibility PluginBridges
    //<

    //> @attr flashlet.classID (string : see below : IR)
    //
    //  This attribute specifies the clsid of the outer &lt;object&gt; tag.
    //  <p>
    //  The default classID is: "clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"
    //  <p>
    //
    //  @visibility PluginBridges
    //<
    classID: "clsid:D27CDB6E-AE6D-11cf-96B8-444553540000",

    //> @attr flashlet.codeBase (URL : see below : IR)
    //
    //  This attribute specifies the minimum version of the flash player required to show this
    //  flashlet.
    //  <p>
    //  The default codeBase is: "http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0"
    //  <p>
    //
    //  @visibility PluginBridges
    //<    
    codeBase: "http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0",

    //> @attr flashlet.pluginsPage (URL : see below : IR)
    //
    //  This attribute specifies the page the user should go to to get the plugin required to view
    //  this flashlet.
    //  <p>
    //  The default pluginsPage is: "http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash"
    //  <p>
    //
    //  @visibility PluginBridges
    //<    
    pluginsPage: "http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash",

    //> @attr flashlet.src (URL : null : IR)
    //
    //  Location from which to load the Flashlet.
    //
    //  @visibility PluginBridges
    //< 

    // probably not changeable by end user...
    type: "application/x-shockwave-flash"
});

isc.Flashlet.addMethods({

//> @method flashlet.setSrc()
// 
//  Sets the source file for the flash component  
//
//  @visibility PluginBridges
//<
setSrc : function (src) {
    this.src = src;
    this.markForRedraw();
},

getInnerHTML : function () {

    var accum = isc.SB.create();

    // if the flashlet hasn't been assigned a name, use getPluginID() to fetch one
    if (this.name == null) this.name = this.getPluginID();

    var protocol = window.location.protocol,
        codeBase = this.codeBase
    ;

    if (protocol && protocol.startsWith("https") && codeBase && codeBase.startsWith("http://")) {
        codeBase = codeBase.replace("http://", "https://");
    }

    accum.append("<object classid='", this.classID, "' codebase='", codeBase,
                 "' width='100%' height='100%' ID='", this.name, "'");

    // add extraHTML if any
    if (this.extraObjectHTML) accum.append(" ", this.extraObjectHTML);
    accum.append(">"); // close object tag

    // pick up this.src || this.movie, copy params so as to not alter the data the user passed in.
    var params = {};
    isc.addProperties(params, this.params);
    if (!params.movie) params.movie = this.src || this.movie;

    // setting wmode to opaque prevents burnthrough in Firefox / Safari
    if (!params.wmode) params.wmode = "opaque"

    // emit <object> params
    for (var key in params)
        accum.append("<param name='", key, "' value='", params[key], "'>");

    accum.append("<embed width='100%' height='100%' name='", this.name, "' src=\"", this.src,
                 "\" pluginspage=\"", this.pluginsPage, "\" type='", this.type, "'");
    
    // emit <embed> params
    for (var key in params)
        accum.append(" ", key, "='", params[key], "'");
    
    if(this.extraEmbedHTML) accum.append(" ", this.extraEmbedHTML);
    accum.append(">");  // close embed tag

    accum.append("</embed>");
    accum.append("</object>");

    return accum.release(false);
},

getPluginID : function () {
    return this.getID() + "_flash";
},

//> @method flashlet.getPluginHandle() (A)
// 
//  Returns a handle to the flashlet DOM element (valid only after the component has been drawn).  
//
// @return (HTML Element) pointer to the plugin element in the DOM
//  @visibility PluginBridges
//<
getPluginHandle : function () {
    if (this.name == null) return null;
    if (isc.Browser.isIE) return window[this.name];
    return document[this.name];
}


});
