/*
 * Isomorphic SmartClient
 * Version v11.0p_2016-03-31 (2016-03-31)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */
//>	@class SVG
//  
//  ISC Abstraction for SVG controls
//
//  @treeLocation Client Reference/Client Bridges
//  @requiresModules PluginBridges
//  @visibility PluginBridges
//<



isc.ClassFactory.defineClass("SVG", "BrowserPlugin");

isc.SVG.addProperties({

    
    useNativeMask : true,

    //> @attr svg.pluginsPage (URL : see below : IR)
    //
    //  This attribute specifies the page the user should go to to get the plugin required to view
    //  this SVG.
    //  <p>
    //  The default pluginsPage is: "http://www.adobe.com/svg/viewer/install/"
    //  <p>
    //
    //  @visibility PluginBridges
    //< 
    pluginsPage: "http://www.adobe.com/svg/viewer/install/",

    //> @attr svg.src (URL : [HELPERS]svgCanvas.svg : IR)
    //
    //  Location from which to load the SVG.
    //  <p>
    //  Note: if you do not specify a src value, ISC will load the special svg 'svgCanvas.svg' from
    //  the helpers directory.  This SVG is simply an empty root element - essentially a blank
    //  canvas.  You can use this feature to write components that programmatically manipulate the
    //  SVG DOM without needing to ship placeholder SVG files.
    //  <p>
    //
    //  @visibility PluginBridges
    //< 
    src: isc.Page.getHelperDir() + "svgCanvas.svg"
});

isc.SVG.addClassMethods({

// Call 'if(isc.SVG) SVG.register(evt)' onload() of your SVG to set up bidirectional pointers
// to/from the SVG to its ISC class instance.
register : function(evt) {
    var svgElement = evt.getTarget();
    var svgDocument = svgElement.getOwnerDocument();

    var svgInstance = this.getSVGCanvas(svgDocument);

    // these are the properties available on the ISC SVG object once SVG.register() is called.
    svgInstance.addProperties({
        svgElement: svgElement,
        svgDocument: svgDocument,
        svgDoc: svgDocument,
        svgRoot: svgDocument.getRootElement()
    });
    
    // patch ourselves onto the window object of the SVG (if 3.0+) it so that
    // svg handler declared in the xml can call methods on this class
    //
    // in SVG xml elements you can reference svgInstance to get to the ISC SVG class instance
    // (only in 3.0+)
    var embedHandle = svgInstance.getPluginHandle();
    if(embedHandle["window"]) embedHandle["window"].svgCanvas = svgInstance;

    if(isc.isA.Function(svgInstance.initSVG)) svgInstance.initSVG();
    else if(isc.isA.Function(svgInstance.initsvg)) svgInstance.initsvg();
    

    if(svgInstance.useNativeMask) svgInstance._makeSVGEventMask();
    if(svgInstance._deferShowNativeMask) svgInstance.showNativeMask();
},

// From SVG 2.0 you can call SVG.getSVGCanvas(svgDocument) to get at the svgCanvas instance.
getSVGCanvas : function(svgDocument) {
    var url = svgDocument.getURL();
    
    if(url.indexOf("#") == -1) {
        this.logError("Can't locate svgCanvas for svgDocument. Use SVG.create() to render SVGs");
        return null;
    }

    var svgID = url.substring(url.indexOf("#") + 1, url.length);
    var svgInstance = window[svgID];
    
    if(!svgInstance) {
        this.logError("Can't locate svg instance for id: " + svgID 
                      + " did you call SVG.register(evt)?");
        return null;
    }
    
    return svgInstance;
}

});

isc.SVG.addMethods({

draw : function () {
    this.Super("draw", arguments);
    if(isc.Browser.isIE) isc.EH.registerMaskableItem(this, true);
},

mouseOut : function () {
    if(this.useNativeMask) {
        this.hideNativeMask();
        this.Super("_hideDragMask");
    }
},


_showDragMask : function () {
    if(this.useNativeMask) this.showNativeMask();
    else this.Super("_showDragMask");
},

_hideDragMask : function () {
    if(this.useNativeMask) this.hideNativeMask();
    else this.Super("_hideDragMask");
},


handleSVGEvent : function (evt) {
    // suppress redundant mousemoves: the SVG component will send mousemove events every Xms even
    // if the cursor has not moved - this can create a kind of jitterring effect on the dragTarget
    if(evt.type == "mousemove") {
        if(this.lastMouseMoveX == evt.clientX && this.lastMouseMoveY == evt.clientY) return;
        this.lastMouseMoveX = evt.clientX;
        this.lastMouseMoveY = evt.clientY;
    }

    // create a synthetic event
    var event = {
        type: evt.type,
        target: this,
        clientX: evt.clientX,
        clientY: evt.clientY
    };
    isc.EventHandler.handleSyntheticEvent(event);
},

_makeSVGEventMask : function () {
    this._svgMask = this.svgDoc.createElement("rect");
    this._svgEventMaskID = this.getID() + "_SVGEventMask";

    var maskAttributes = {
        id: this._svgEventMaskID,
        // XXX SVG has a concept of a viewport and a user coordianate system.  This means we can't
        // assume that the origin is at top left and 100%x100% width x height covers the whole
        // viewport.  This is kind of a cheesy hack, but a more intelligent approach will need to be
        // well tested and could be defeated by weird viewport/coordinate system settings.
        //
        // Further, we can't just set the mask's params to the outermost component because the
        // svg embed tag can specify a larger size than the viewport which would leave uncovered
        // space.
        //
        // Set the width and height to MAXINT and the left/right (x/y) to Math.floor(-MAXINT/2)
        // This should work for all but the strangest coordinate/viewport settings
        x: "-1073741823",
        y: "-1073741823",
        width: "2147483647",
        height: "2147483647",
        // make it transparent
        opacity: "0.0",
        // hidden to start with
        visibility: "hidden",
        // plumb all mouse events to us
        // XXX: we get a mousedown when dragging over an SVG (with the mouse down) which causes EH
        // to set up a mousestilldown interaction on the native svg element.  Really - we shouldn't
        // be getting a mousedown when simply mousing over.
        //
        // Since the mask only gets shown during drag interactions, it doesn't currently make sense
        // to capture mousedown
//        onmousedown: "svgCanvas.handleSVGEvent(evt)",
        onmousemove: "svgCanvas.handleSVGEvent(evt)",
        onmouseup: "svgCanvas.handleSVGEvent(evt)",
        onmouseout: "svgCanvas.handleSVGEvent(evt)",
        onclick: "svgCanvas.handleSVGEvent(evt)",
        oncontextmenu: "svgCanvas.handleSVGEvent(evt)"
    };

    // actually apply the above attributes to the element
    for(var key in maskAttributes) this._svgMask.setAttribute(key, maskAttributes[key]);

    // and att the mask to the outer svg element.
    this.svgRoot.appendChild(this._svgMask);
},

setZIndex: function () {
    // NO.  Do not manipulate the Z index on SVG objects.  IE loses the status bar and corrupts
    // state until you restart it.  
},

showNativeMask : function () {
    // we can't show the mask until we have a handle to the root element of the SVG.  Typical usage,
    // however is to do an SVG.create() and immediately show the native mask.  We set a special defer
    // variable here and check it in SVG.register()
    if(!this.svgDoc) {
        this.logWarn("showNativeMask called before SVG.register() - deferring until SVG.register()");
        this._deferShowNativeMask = true;
        return;
    }
    // push the mask to the front of the svg element stack so that it actually masks everything
    // in the svg.  Need to do this on every show() because the SVG DOM may have been dynamically
    // modified since we last showed the mask.
    if(this._svgMask) {
        this.svgRoot.removeChild(this._svgMask);
        this.svgRoot.appendChild(this._svgMask);
    } else {
        this._makeSVGEventMask();    
    }
    this._svgMask.setAttribute("visibility", "visible");
},

hideNativeMask : function () {
    if(this._svgMask) this._svgMask.setAttribute("visibility", "hidden");
},    

getInnerHTML : function () {
    if(isc.Browser.isIE) {
        return "<embed name='" + this.getPluginID() + "' src=\"" + isc.Page.getURL(this.src) 
               + "#" + this.getID() + "\" width='100%' height='100%'"
               + (this.installPlugin ? "pluginspage='"+this.pluginsPage+"'" : "")
               + " type='image/svg+xml' " 
               + this.extraHTML + " >";
    }
    return this.Super("getInnerHTML", arguments);
},

destroy : function () {
    // remove all handles to the SVG DOM just in case they leak
    if(this._svgMask) delete this._svgMask;
    var embedHandle = this.getPluginHandle();
    if(embedHandle && embedHandle["window"]) delete embedHandle["window"].svgCanvas;
    this.Super("destroy", arguments);
},

//---------------------------------
// helpers
//---------------------------------

// calls set attribute with key/value pairs supplied as object literal
setNodeAttributes : function (obj, attrs) {
    for(var key in attrs) obj.setAttribute(key, attrs[key]);
}   

});

