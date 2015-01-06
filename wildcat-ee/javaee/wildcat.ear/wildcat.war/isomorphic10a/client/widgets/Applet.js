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




//>	@class	Applet
//  
//  SmartClient abstraction for Applets.  This class provides Java&lt;-&gt;SmartClient
//  interoperability.  When embedding Applets into SmartClient-based pages, please use this
//  wrapper class to instantiate the Applet.  Doing so will provide the following benefits:
//  <p>
//  <ul>
//  <li>Drag and drop interactions on the page can target the Applet (can drop on applet).
//  <li>When a drag interaction transits an Applet, SmartClient will ensure that the dragTarget
//  appears above the Applet (prevents burn-through).
//  <li>Allows you to easily embed the Applet into SmartClient containers such as Windows and
//  Layouts, providing automatic masking to ensure the correct z-index order.  But see notes
//  below for limitations.
//  </ul>
//  <p>
//  <u>Known issues</u><br><br>
//  <ul>
//  <li>Resizing Applets can cause Firefox to lock up and require the user to terminate the
//  process manually.  The cause is currently unknown, but likely causes include applets
//  attempting to control their own size, or to attempt to re-render on resize.  More recent
//  JVMs (1.6+) appear to make this problem less frequent.  This may also simply be caused by
//  applet code being unable to deal with certain combination sizes and crashing.
//  <li>In Firefox, when using a JVM older than 1.6, Applets cannot be resized smaller than
//  their originally drawn size.  Doing so causes them to stick at the originally drawn size,
//  and to overflow the boundary of any SmartClient component they are embedded in.
//  <li>In Firefox, Applets do not handle the z-index ordering correctly with respect to
//  IFRAMEs or other Applets.  Therefore, in Firefox you cannot layer Applets on top of each
//  other or on top of SmartClient components containing IFRAMEs and have them render in the
//  correct order.
//  </ul>
//
//  For a general overview of applets and an applet tutorial see: 
//  +externalLink{http://java.sun.com/docs/books/tutorial/applet/}
//
//  @treeLocation Client Reference/Client Bridges
//  @requiresModules PluginBridges
//  @visibility PluginBridges    
//<


isc.ClassFactory.defineClass("Applet", "BrowserPlugin");

isc.Applet.addClassProperties({
    // How often should we scan the page for new applets for which to proxy events (milliseconds)
    appletScanInterval: 500
});

isc.Applet.addClassMethods({

initComplete : function (version) {
    this.jvmVersionString = version;
    this.jvmVersion = parseFloat(version);

    this.logInfo("ISCEventProxy init complete - jvmVersion: "+version+" - derived version: " + this.jvmVersion);
},

idForName : function (name) {
    if (name && name.endsWith("_applet")) return name.substring(0, name.length-7);
},


// starts the eventProxy applet if it hasn't already been started.
startJavaEventProxy : function () {
    // don't start it twice
    if(this.eventProxyApplet) return;
    
    //this.logWarn("startEventProxy()");
    this.eventProxyApplet = isc.Applet.create({
        top: -1000,
        width: 10,
        height: 10,
        autoDraw: false,
        useJavaEventProxy: false,
        useDragMask: true,
        params : {
            debug: this.debug,
            useEventMasks: this.useEventMasks,
            appletScanInterval: this.appletScanInterval
        },
        _showDragMask : function () {
            var handle = this.getPluginHandle();
            if (handle) handle.showDragMask();
        },
        _hideDragMask : function () {
            var handle = this.getPluginHandle();
            if (handle) handle.hideDragMask();
        },
        ID: "isc_eventProxyApplet",
        archive: isc.Page.getURL("[HELPERS]isomorphic_applets.jar"),
        code: "com/isomorphic/applets/ISCEventProxy.class"
    });
    
    // Note: draw() after create() instead of autoDraw: true because draw() calls us and we check
    // for this.eventProxyApplet.
    this.eventProxyApplet.draw();
}

});

isc.Applet.addProperties({
    //> @attr applet.name (string : null : IR)
    //
    //  Sets the 'name' attribute on the applet object.  If a name is not provided it will be
    //  auto-generated.  Note that in general you don't need to set this.  If you have a handle to
    //  your ISC Applet object you can simply call +link{method:Applet.getPluginHandle()} to get a
    //  handle to the element.
    //
    //  @see method:Applet.getPluginHandle()
    //
    //  @visibility PluginBridges
    //<

    //> @attr applet.code (URL : null : IR)
    //
    //  A URL that points to the class of the applet.
    //
    //  @see attr:Applet.codeBase
    //  @see attr:Applet.archive
    //
    //  @visibility PluginBridges
    //<

    //> @attr applet.src (URL : null : IR)
    //
    //  A URL that points to the class of this applet.  This is the same as the 'code' attribute.
    //  Note that if you specify both the src and the code attributes, the src attribute will be
    //  ignored in favor of the code attribute.
    //
    //  @visibility PluginBridges
    //< 

    //> @attr applet.codeBase (URL : null : IR)
    //
    //  The base URL of the applet if the +link{attr:applet.code} attribute is relative
    //
    //  @see attr:Applet.code
    //  @see attr:Applet.archive
    //
    //  @visibility PluginBridges
    //<

    //> @attr applet.archive (URL : null : IR)
    //
    //  URL to the applet JAR or ZIP archive.
    //
    //  @see attr:Applet.code
    //  @see attr:Applet.codeBase
    //
    //  @visibility PluginBridges
    //<

    //> @attr applet.alt (string : null : IR)
    //
    //  This text is displayed if the browser understands the tag used for the applet html element,
    //  but can't run the applet for some reason.
    //
    //  @see attr:Applet.altHTML
    //  @see attr:Applet.useTag
    //
    //  @visibility PluginBridges
    //<

    //> @attr applet.altHTML (HTML : null : IR)
    //
    //  This text is displayed if the browser does not understand the tag used for the applet html
    //  element.
    //
    //  @see attr:Applet.alt
    //  @see attr:Applet.useTag
    //
    //  @visibility PluginBridges
    //<

    //> @attr applet.params (object : null : IR)
    //
    //  A map of key/value pairs to pass to the applet as parameters.  These are typically
    //  accessible within the Java applet code.
    //
    //  @visibility PluginBridges
    //<

    //> @attr applet.mayScript (Boolean : true : IR)
    //
    //  Usable only in IE: if true allows Java->JS LiveConnect (other platforms that support
    //  LiveConnect have it enabled by default and do not allow it to be disabled).
    //
    //  @platformNotes Relevant on IE only.
    //  @see applet.scriptable
    //
    //  @visibility PluginBridges
    //<
    mayScript: true, // enable Java->JS LiveConnect in IE?

    //> @attr applet.scriptable (Boolean : true : IR)
    //
    //  Usable only in IE: if true allows JS->Java LiveConnect (other platforms that support
    //  LiveConnect have it enabled by default and do not allow it to be disabled).
    //
    //  @platformNotes Relevant only on IE.
    //  @see applet.mayScript
    //
    //  @visibility PluginBridges
    //<
    scriptable: true, // enable JS->Java LiveConnect in IE?
    
    //> @attr applet.classID (string : see below : IR)
    //
    //  This attribute is used only with useTag: "object".  In combination with 
    //  +link{attr:Applet.objectCodeBase} this tag specifies the minimum JVM version required to view
    //  this applet and the URL from which the JVM can be downloaded.
    //  <p>
    //  The default settings match the default behavior of the &lt;applet&gt; tag which requires JVM
    //  1.3 or higher.
    //  <p>
    //  The default classID is: "clsid:8AD9C840-044E-11D1-B3E9-00805F499D93"
    //  <p>
    //  For notes on this feature see: +externalLink{http://java.sun.com/j2se/1.4.2/docs/guide/plugin/developer_guide/using_tags.html#in-ie}
    //
    //  @see attr:Applet.objectCodeBase
    //
    //  @visibility PluginBridges
    //<
    classID: "clsid:8AD9C840-044E-11D1-B3E9-00805F499D93",

    //> @attr applet.objectCodeBase (string : see below : IR)
    //
    //  This attribute is used only with useTag: "object".  In combination with 
    //  +link{attr:Applet.classID} this tag specifies the minimum JVM version required to view
    //  this applet and the URL from which the JVM can be downloaded.
    //  <p>
    //  The default settings match the default behavior of the &lt;applet&gt; tag which requires JVM
    //  1.3 or higher.
    //  <p>
    //  The default objectCodeBase is: "http://java.sun.com/products/plugin/1.3/jinstall-13-win32.cab#Version=1,3,0,0"
    //  <p>
    //  For notes on this feature see: +externalLink{http://java.sun.com/j2se/1.4.2/docs/guide/plugin/developer_guide/using_tags.html#in-ie}
    // 
    //  @see attr:Applet.classID
    //
    //  @visibility PluginBridges
    //<    
    objectCodeBase: "http://java.sun.com/products/plugin/1.3/jinstall-13-win32.cab#Version=1,3,0,0",

    //> @attr applet.extraHTML (Boolean : true : IR)
    //
    //  Any additional HTML you want to throw into the tag that specifies the applet element.
    //
    //  @visibility PluginBridges
    //<
    
    //> @attr applet.useTag (string : "applet" : IR)
    //
    //  Specifies the type of tag to use for the applet html.  See
    //  +externalLink{http://java.sun.com/j2se/1.4.2/docs/guide/plugin/developer_guide/using_tags.html} for a
    //  discussion of different tags.
    //
    //  @value "applet"     Use the &lt;applet&gt; tag.  Works on all browsers.
    //  @value "object"     Use the &lt;object&gt; tag.  Works on IE.
    //
    //  @visibility PluginBridges
    //<
    useTag: "applet", // type of tag to use: applet, object or embed (note: embed not yet supported)

    
    useClipDiv: false,

    // Only necessary in IE because in FF we can use the standard dragMask.  Also, FF, as of
    // version 2.0.7 is unstable with our plugin.  It works most of the time, but can crash the
    // browser apparently at random.  This may have to do with the fact that we use a thread in
    // our plugin to scan for other applets periodically, or possibly is a typical side-effect
    // of cross-applet or of the Java LiveConnect technology in general.  Best to avoid using
    // applets ourselves in FF, when we can.
    //
    //
    useJavaEventProxy: isc.Browser.isIE,

    //we use the Java event proxy in IE
    useDragMask: !isc.Browser.isIE,

    // We have event capture mechanisms for all relevant browsers, so don't need to hide the
    // applet on drag
    usePlaceholderDragMask: false,

    // IFRAME backmask does burn through Applets in Moz
    backMaskCausesBurnThrough : isc.Browser.isMoz,

// Start the event proxy on draw() 
draw : function () {
    if (this.useJavaEventProxy) isc.Applet.startJavaEventProxy();
    this.Super("draw", arguments);
},


getInnerHTML : function () {
    var accum = isc.StringBuffer.create();

    // allow the src property to be used as the location of the applet source
    if (this.code == null && this.src != null) this.code = this.src;  

    if (this.useTag == "applet") {
        // use applet tag
        accum.append("<applet name='", this.getPluginID(), 
                     "' width='100%' height='100%'",
                     " iscCanvasID='", this.getID(), "'");
        if (this.mayScript) accum.append(" mayScript"); 
        if (this.scriptable) accum.append(" scriptable");
        if (this.code) accum.append(" code='", this.code, "'");
        if (this.codeBase) accum.append(" codeBase='", this.codeBase, "'");
        if (this.archive) accum.append(" archive='", this.archive, "'");
        if (this.alt) accum.append(" alt='", this.alt, "'");

        // add extraHTML if any
        if (this.extraHTML) accum.append(" ", this.extraHTML);

        // close applet tag
        accum.append(">");

        if (this.params) {
            for (var key in this.params) {
                accum.append("<param name='", key, "' value='", this.params[key], "'>");
            }
        }
    
        if (this.altHTML) accum.append(this.altHTML);
        accum.append("</applet>");
    } else if (this.useTag == "object") {
        accum.append("<object classid='", this.classID, "' codebase='", this.objectCodeBase,
                     "' width='100%' height='100%'");

        // add extraHTML if any
        if (this.extraHTML) accum.append(" ", this.extraHTML);
        accum.append(">"); // close object tag
        
        accum.append("<param name='name' value='", this.getPluginID(), "'>"); 
        accum.append("<param name='iscCanvasID' value='", this.getID(), "'>"); 

        if (this.mayScript) accum.append("<param name='mayscript' value='true'>"); 
        if (this.scriptable) accum.append("<param name='scriptable' value='true'>");
        if (this.code) accum.append("<param name='code' value='", this.code, "'>");
        if (this.codeBase) accum.append("<param name='codeBase' value='", this.codeBase, "'>");
        if (this.archive) accum.append("<param name='archive' value='", this.archive, "'>");
        if (this.alt) accum.append("<param name='alt' value='", this.alt, "'>");

        if (this.params) {
            for (var key in this.params) {
                accum.append("<param name='", key, "' value='", this.params[key], "'>");
            }
        }

        accum.append("</object>");
    }

    return accum.release(false);
},

//> @attr applet.pluginID (String : null : IR)
// ID used for the &lt;applet&gt; tag generated by this component.
// @visibility PluginBridges
//< 
pluginID : null,

//> @method applet.getPluginID()
//
//  @return	(String)    ID used for the &lt;applet&gt; or &lt;object&gt; DOM element generated by this component.
//
//  @visibility PluginBridges
//<
getPluginID : function () {
    if(!this.pluginID) {
        if(!this.name) this.name = this.getID() + "_applet";
        return this.name;
    } else {
        return this.pluginID;
    }
},

//> @method applet.setPluginID()    ([])
//
//  Sets the ID to be used for the &lt;applet&gt; or &lt;object&gt; DOM element generated by this
//  component.  Can only be set before draw.
//
//  @param    pluginID         (string)
//
//  @visibility PluginBridges
//<
setPluginID : function (pluginID) {
    this.pluginID = pluginID;
},

//> @attr applet.ID (String : null : IR)
//
//  Global ID for this Applet component.  Note this is not the same as the ID used for the
//  &lt;applet&gt; element in the DOM - see +link{applet.pluginID}.
//
//  @visibility PluginBridges
//<

//> @method applet.getPluginHandle() (A)
// 
//  @return	(DOMElement)    Returns a handle to the applet DOM element (valid only after the component has been drawn).
//
//  @visibility PluginBridges
//<
getPluginHandle : function () {
    return document[this.getPluginID()];
},

repaint : function () {
    var handle = this.getPluginHandle();
    if (handle) handle.repaint();
},


repaintOnDragStop : function () {
    return this.useJavaEventProxy && isc.Applet.jvmVersion < 1.4;
},

_hideDragMask : function () {
    this.Super("_hideDragMask", arguments);

    if (this.repaintOnDragStop()) this.repaint();
}

});
