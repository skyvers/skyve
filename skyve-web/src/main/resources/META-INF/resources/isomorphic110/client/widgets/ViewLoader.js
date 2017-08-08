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
//>	@class	ViewLoader
//
// The ViewLoader component can be used to load new SmartClient-based user interfaces into a
// running application.
// <P>
// <b>NOTE:</b> before using a ViewLoader, be sure that you have read about and understood the
// +link{group:smartArchitecture,SmartClient Architecture}.  The most responsive and
// scalable application architecture preloads views rather than using ViewLoaders.
// <P>
// A ViewLoader is a Canvas, and can be provided anywhere a Canvas can be provided: as a Tab
// pane, and Layout member, etc.  When a ViewLoader draws, it shows a
// +link{viewLoader.loadingMessage,loading message}, then
// performs an RPC to the +link{viewLoader.viewURL,viewURL} to load components.
// <P>
// The response from the viewURL should be SmartClient components defined in JavaScript, with no
// surrounding &lt;SCRIPT&gt; tags or other HTML framing.  The returned script can be
// dynamically generated, for example, it may be the result of a JSP containing an XML view
// description enclosed in +link{group:xmlTag,&lt;isomorphicXML&gt;} tags.
// <P>
// In the returned script, the special variable "viewLoader" is available to refer to the
// ViewLoader instance that is loading components.  The intended usage is that the returned
// script creates a view consisting of SmartClient components, then calls
// <code>viewLoader.setView(myView)</code> to place the loaded view into the ViewLoader.
// If the view does not call setView() explicitly, the viewLoader will find the last top-level
// UI component (Canvas subclass) created by the view and set that as the current view.
// Top-level in this case means that the UI component is not contained in another UI component
// as a member or child.
// <p>
// The ViewLoader relies on the XMLHttpRequest object which can be disabled by end-users in some
// supported browsers.  See +link{group:platformDependencies} for more information.
// 
// @see rpcRequest.evalResult
// @see group:smartArchitecture for general discussion of architectural best practices in
//      SmartClient
// @group viewLoading
// @treeLocation Client Reference/Foundation
// @visibility external
// @example viewLoading
//<



// NOTE: we are a subclass of Label as a means of showing the loading message
isc.ClassFactory.defineClass("ViewLoader", isc.Label);

isc.ViewLoader.addProperties({

//> @attr viewLoader.loadingMessage  (HTMLString : "Loading View...&nbsp;${loadingImage}" : IR)
// Message to show while the view is loading.
// Use <code>"&#36;{loadingImage}"</code> to include +link{Canvas.loadingImageSrc,a loading image}.
//
// @group viewLoading
// @visibility external
//<
loadingMessage:"Loading View...&nbsp;${loadingImage}",
align:isc.Canvas.CENTER,
allowContentAndChildren:true,

//> @attr viewLoader.viewURL         (URL : null : IR) 
// URL to load components from.
//
// @visibility external
// @example viewLoading
//<
//viewURL: null,

//> @attr viewLoader.viewURLParams   (Object : null : IR) 
// Parameters to be sent to the viewURL when fetching the view.
// @visibility external
//< 
//viewURLParams: null,

//> @attr viewLoader.viewRPCProperties (RPCRequest Properties : null : IRA) 
// RPCRequest properties to be sent with every RPCRequest issued by the ViewLoader.  Very
// advanced; could be used to, for example, set HTTP headers.
// @visibility external
//<

//> @attr viewLoader.httpMethod         (String : "GET" : IRW)
// Selects the HTTP method that will be used when fetching content.  Valid values are "POST"
// and "GET".
// @group contentLoading
// @visibility external
//<
httpMethod:"GET", // default would POST

useSimpleHttp:true, // don't send stuff that RPC layer usually sends (serialized transaction, etc)

//> @attr viewLoader.allowCaching (Boolean : false : IR)
// By default a ViewLoader will explicitly prevent browser caching.
// <P>
// Set to true to allow browser caching <b>if the browser would normally do so</b>, in other
// words, if the HTTP headers returned with the response indicate that the response can be
// cached.
//
// @visibility external
//<

transformXML: true, // transform .xml file contents via client-side XML parser?

// so that we get allocated space in Layouts, instead of autoFitting
overflow:"hidden"
});

isc.ViewLoader.addMethods({

initWidget : function () {
    this.Super(this._$initWidget, arguments);

    // if we've been given a placeholder widget, add it
    if (this.placeholder) this.addChild(this.placeholder);
    // otherwise show the loading message
    else this.contents = this.getLoadingMessage();
},

draw : function () {
    if (!this.readyToDraw()) return this;
    this.Super("draw", arguments);

    if (this.view) {
        this.addChild(this.view);
        this.view.show();
    } else if (this.viewURL && !this.loadingView()) {
        // fetch from server if so configured
        this.setViewURL();
    }
    return this;
},

// simple layout policy just fills the view
layoutChildren : function () {
    this.Super("layoutChildren", arguments);
    var children = this.children;
    if (!children || children.length == 0) return;

    var child = this.children[0],
        width = this.getWidth(),
        height = this.getHeight();

    // don't resize a loaded view that has specific sizes set on it
    if (child._userWidth != null) width = null;
    if (child._userHeight != null) height = null;

    // NOTE: we intentionally occlude styling such as borders, if any, which are only meant to
    // exist while we are showing the loading message
    child.setRect(0, 0, width, height);
},

destroy : function () {
    if (this.placeholder) this.placeholder.destroy();
    if (this.view) this.view.destroy();
    this.Super("destroy", arguments);
},

// dynamically sets a custom placholder
setPlaceholder : function (placeholder) {
    if (this.placeholder) this.placeholder.destroy();
    this.placeholder = placeholder;
    this.addChild(placeholder);
    this.placeholder.sendToBack();
},


//> @method viewLoader.setViewURL()
// Change the URL this component loads a view from.  Triggers a fetch from the new URL.
// <P>
// Can also be called with no arguments to reload the view from the existing
// +link{viewLoader.viewURL}.
//
// @param [url]    (URL)     URL to retrieve view from
// @param [params] (Object)  Parameters to send to the viewURL.  Merged with
//                           <code>component.viewURLParams</code> if both are set.
// @param [rpcProperties] (RPCRequest Properties)  Additional properties for the RPCRequest 
//                        sent by the ViewLoader.  Very advanced; could be used to, for
//                        example, set HTTP headers.
// @group viewLoading
// @visibility external
//<
setViewURL : function (url, params, rpcProperties) {
    if (url != null) this.viewURL = url;
    url = this.viewURL;

    if (this.placeholder) {
        this.placeholder.show();
        this.placeholder.bringToFront();
    }
    // change contents back to loading message on reload
    if (this.view != null) {
        this.view.hide();
        this.setContents(this.getLoadingMessage());
    }

    var baseParams = {},
        useSimpleHttp = this.useSimpleHttp,
        httpMethod = this.httpMethod,
        evalResult = false;

    // normally we send an XMLHttpRequest and expect a pure JavaScript response, however if ActiveX
    // is off in IE, we can't use XMLHttpRequest, so we send a normal, frames-based RPC instead.
    // This means whatever is at the viewURL must use the server-side RPCManager APIs and send the data
    // back as a String.
    if (!isc.rpc.xmlHttpRequestAvailable()) {
        this.logInfo("XMLHttpRequest not available, using frames comm and expecting RPCResponse");
        baseParams = { };
        useSimpleHttp = false; // useSimpleHttp: true not supported for frames comm 
        httpMethod = "POST";
        evalResult = false;
    }

    var reloadRequest = isc.addProperties({
        showPrompt: false,
        actionURL: this.viewURL, 
        httpMethod: httpMethod,
        useSimpleHttp: useSimpleHttp,
        // IE caches very aggressively by default, which can be confusing, so disable caching
        // unless this (currently undocumented) flag is set
        bypassCache: !this.allowCaching,
        params: isc.addProperties(baseParams, this.viewURLParams, params)
    },
    this.viewRPCProperties, // widget rpc properties
    rpcProperties, // method rpc properties
    // non-overridable
    {
        evalResult: evalResult,
        suppressAutoDraw: true,
        willHandleError: true,
        callback: "if(window."+this.getID()+")"+this.getID()+"._loadViewReply(rpcRequest, rpcResponse, data)"
    });

    // add this component to evalVars
    if (!reloadRequest.evalVars) reloadRequest.evalVars = {};
    reloadRequest.evalVars.viewLoader = this; 

    this._loadNumber = isc.rpc.sendProxied(reloadRequest, true).transactionNum;
},

loadingView : function () {
    return this._loadNumber != null;
},

_loadViewReply : function (rpcRequest, rpcResponse, data) {
    // handles case of setViewURL() being called while we're fetching from some other URL
    // ignore all but the most recent load
    if (rpcRequest.transactionNum != this._loadNumber) {
        return;
    }
    
    delete this._loadNumber;
    
    this._viewSet = false;
    
    if (rpcResponse.status != isc.RPCResponse.STATUS_SUCCESS) {     
        if (this.handleError(rpcRequest, rpcResponse) === false) return;
    }
    
    try {
        // if the target file was an xml file, then components are assumed to be in XML format,
        // client-transalteable via toComponents().  Ideally we would have a toComponents() mode
        // that would return the JS for the passed XML so we can follow the globalEvalWithCapture
        // path below and support evalVars.  In the absense of that, we do the "last top-level
        // canvas is the view".  
        //
        // Since in the XML case we don't have the list of created globals (since we don't
        // globalEvalWithCapture()), we look at isc.Canvas._canvasList to find the same information
        if (rpcRequest.actionURL.endsWith(".xml") && this.transformXML) {
            var _canvasList = isc.Canvas._canvasList;
    
            // fixate the length of the canvasList prior to creating the new view - everything
            // after this index are the new components that we'll search for the view
            var lastIndex = _canvasList.length;

            isc.xml.toComponents(data);
            
            if (!this._viewSet) {
                for (var i = _canvasList.length; i >= lastIndex; i--) {
                    var obj = _canvasList[i];
                    if (obj != null && isc.isA.Canvas(obj) &&
                        obj.parentElement == null && obj.masterElement == null) 
                    {
                        this.setView(this.transformView(obj));
                        break;
                    }
                }
            }

            // complete view loading
            this._loadViewReplyComplete();

        } else { 
            var viewLoader = this;
            isc.Class.globalEvalWithCapture(data, function (globals, error) {
                isc.Log.logWarn("firing the callback from global eval with...");
                isc.Log.logWarn('viewLoader is:' + viewLoader);
                if (error) {
                    viewLoader.handleError(rpcRequest, rpcResponse, error);
                } else {
                    viewLoader._loadViewReplyComplete(globals);
                }
            }, rpcRequest.evalVars);
        }
    } catch (e) {        
        this.handleError(rpcRequest, rpcResponse, e);
    }
},

_loadViewReplyComplete : function (globals) {    
    
    if (!this._viewSet && globals) {
        // if view we just loaded didn't call setView(), automatically find the last top-level
        // Canvas in the response and set that as the view.
        //
        // Note: globalEvalWithCapture return globalIDs in the order they were created.
        // Typically the top-level container is declared last since it incorporates other
        // Canvii declared before it, so we count down from the last created Canvas here.
        for (var i = globals.length; i >= 0; i--) {
            var global = globals[i];
            var obj = window[global]; // globals are IDs, dereference
            if (obj && isc.isA.Canvas(obj) && !obj.canvasItem &&
                obj.parentElement == null && obj.masterElement == null) 
            {
                this.setView(this.transformView(obj));
                break;
            }
        }
    }

    if (!this._viewSet) {
        this.logWarn("setView() not explicitly called by loaded view and could"
                     + " not be autodetected for view: " + this.getID());
    }

    this.viewLoaded(this.view); // notify observers
},

// end-user hook to last minute transform betfore we set the view
transformView : function (obj) {
    return obj;    
},


//> @method viewLoader.handleError()
//
// This method is called when a transport error occurs.  Typically, this is the result of the
// server returning an HTTP error code such as 404 - document not found.  You can inspect the
// RPCResponse object for the reasons for the error and take appropriate action.  Typical
// properties to look at are rpcResponse.status, and rpcResponse.httpResponseCode.
// <p>
// This method is called from the response processing pipeline.  If you want to provide your
// own HTML response as the result of the error, you can do
// so by setting rpcResponse.data to your HTML string.  Returning false from this method
// suppresses any further response handling.  The default implementation of this method causes
// an error message to be logged to the Developer Console and sets the HTML to the error
// string.
//
// @param rpcRequest    (RPCRequest)      The RPCRequest that was made to the server
// @param rpcResponse   (RPCResponse)     The RPCResponse that was received
//
// @return (boolean) false to suppress further response processing
// 
// @group contentLoading
// @visibility external
//<
handleError : function (rpcRequest, rpcResponse, jsError) {
    this.logWarn("ViewLoader received bad response:\n" + isc.echo(rpcResponse.data));
    this.setView(isc.Label.create({
        contents: jsError ? jsError.toString() : rpcResponse.data
    }));
    return false;
},


setView : function (view) {
    if (view != null && view == this.view) return;

    this._viewSet = true;
    this.setContents("&nbsp;");

    if (this.view) this.view.destroy();
    this.view = view;

    if (view == null) return;

    // add the view as a child, suppressing drawing until we have a chance to size it
    this.addChild(view, null, false);
    this.layoutChildren();
    view.draw();
    this.logInfo("showing view: " + view);

    if (this.placeholder) this.placeholder.hide();
    // hide loading message
    this.contents = "&nbsp;";
},

//> @method viewLoader.getView()
// Retrieve the current view.  May be null if the view has not yet been loaded, or has been
// explicitly set to null.
//
// @return (Canvas) the current view
//
// @group contentLoading
// @visibility external
//<
getView : function () {
    return this.view;
},

//> @method viewLoader.viewLoaded()
// StringMethod fired when the view has been loaded.  Has no default implementation.  May be
// observed or overridden to fire custom logic when loading completes.
//
// @param view (Canvas) the view that was loaded 
//
// @group contentLoading
// @visibility external
//<
viewLoaded : function (view) { 
    // observable/overrideable
},

getLoadingMessage : function () {
    return this.loadingMessage == null ? "&nbsp;" : this.loadingMessage.evalDynamicString(this, {
        loadingImage: this.imgHTML(isc.Canvas.loadingImageSrc, 
                                   isc.Canvas.loadingImageSize, 
                                   isc.Canvas.loadingImageSize)
        });
}
});
