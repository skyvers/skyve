/*
 * Isomorphic SmartClient
 * Version v9.1p_2014-03-26 (2014-03-26)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */

 






//>	@class Comm
// Provide background communication with an HTTP server
//<

// create the Comm singleton object
isc.ClassFactory.defineClass("Comm");

// add properties to the Comm object
isc.Comm.addClassProperties( {

    //>	@classAttr	isc.Comm.sendMethod (string : isc.Comm.sendMethod : I)
	//			communication method
	//		@group	communication
	//<
	sendMethod:	"POST",

	//>	@type	SendMethod
	//			@visibility external
	//			@group	communication
	// @value	"GET"     GET method (regular URL encoding)		
	// @value	"POST"    POST method (form field encoding)
	//<

	// Sequence number for forms created in IE to send data to the server.  Used to give each
    // form a unique name.
	_generatedFormNumber : 0

});


isc.Comm.addClassMethods({

// XMLHttpRequest vs IFrame Comm
//
// Advantages:
// - doesn't add to history in older Moz
// - no request for empty.html in HTTPS
// - doesn't add to document.frames and potentially screw up numbering
// - ability to detect load with a server-initiated callback
// - synchronous option
//
// Disadvantages / neutral
// - can't handle upload fields
// - is subject to concurrent requests limit in IE (test by adding Thread.sleep(5000) to
//   IDACall - responses come in closely-spaced pairs)
// - no persistent connections in IE (server push), see:
//    http://jszen.blogspot.com/2005/03/xmlhttp-and-readystate.html
// - no compression in IE 5.5
//
// XMLHttpRequest behavior with file:// URL - works on FF, Safari, and IE but on all these
// platforms the URL must be relative or it doesn't work.  SO e.g. ../foo.html is ok, but
// /foo.html is not.  Haven't tested specifying the full path (e.g. c:\foo\bar.html).  
//
// For XMLHttpRequest, FF uses the baseURL of the window object that started thread that
// eventually calls XMLHttpRequest.send() as the baseURL for resolving the relative reference.
// This means that if you send an RPC using XMLHttpRequest out of the eval area of the
// Developer Console, your base URL will be /isomorphic/system/helpers - because that's where
// Log.html came from.  You can use a timeout to restart the thread in the appropriate context
// - just be aware of this oddity.

_fireXMLCallback : function (request, callback, delayedCall) {
    

    if (!delayedCall) isc.EH._setThread("XRP");

    // NOTE: last param tells fireCalllback to trap errors in Moz
    isc.Class.fireCallback(callback, "xmlHttpRequest", [request], null, true);

    if (!delayedCall) isc.EH._clearThread();
},

// ==========================================================================================
// IMPORTANT: If you update the XMLHttp code here, also update FileLoader.js
// ==========================================================================================
_getStateChangeHandler : function () {
    return function () {
        var request = arguments.callee.request;
 
        // IE 5.5 manages to call this handler after readyState changes to 4 and we process the
        // request, which would cause the whole RPC reply logic to run more than once which
        // breaks badly.  If we run more than once, the request will be null becase we null it out
        // below, so just trap this case and return.
        if (!request) return;
        
        if (request.readyState != 4) return;

        arguments.callee.request = null;

        // Bizarre IE-only bug: in the middle of a readyStateChange thread, we draw a
        // new widget, and on touching the newly created handle in
        // Canvas._browserDoneDrawing(), IE decides it's a good time to interrupt the current
        // thread and fire readyStateChange from another xmlHttpRequest that has arrived in the
        // meantime - this can actually be seen in the stack trace, which shows this handler
        // being called (impossibly) by _browserDoneDrawing().  
        // So far as we know, IE is not willing to similarly interrupt a timer thread, so we
        // fire the callback on a 0ms timer instead, which should be order-preserving.
        // NOTE: do not use a closure here or a leak will be introduced
        //isc.Comm._fireXMLCallback(request, arguments.callee.callback);
        isc.Timer.setTimeout({ target:isc.Comm, methodName:"_fireXMLCallback",
                               args:[request, arguments.callee.callback, true] }, 0);
    }
},


// Upon shipping XML3.0, Microsoft started installing XML parsers in "side by side mode"
// meaning the previous version is left intact and a new version is installed parallel to
// the existing one.  Prior to this the latest XML parser being installed by some piece of
// software would overwrite the previous verison and potentially break the software that
// depended on that version.  The catch is that to get the later versions of the parser, you
// need to specify a version-specific prefix.
//
// We use a version independent prefix to get the 2.0 parser in IE6.0 and IE5.5 and then
// fall back on version-specific prefixes if that fails for some reason.  But we expect
// MSXML2.XMLHTTP or Microsoft.XMLHTTP to actually work.
//
// MSXML2 prefix specifies the version independent control.  The "Microsoft" and "MSXML"
// prefixes are old-style version independent controls.  MSXML3 is version specific 3.0 parser.
//                     IE6+               IE5.5               pre-IE5.5 style  IE6+
xmlHttpConstructors : ["MSXML2.XMLHTTP", "Microsoft.XMLHTTP", "MSXML.XMLHTTP", "MSXML3.XMLHTTP"],

// ==========================================================================================
// IMPORTANT: If you update this function, also update its copy in FileLoader.js
// ==========================================================================================
createXMLHttpRequest : function () {
    
    if (isc.Browser.isIE) {

        var xmlHttpRequest;

        // We prefer the  ActiveX version of XMLHttpRequest if it's available because IE7's
        // native implementation has some quirks - for example it doesn't allow requests to
        // file:// URLs no matter what overrides you set in IE's options panel.  Also there
        // are scattered reports of the native implementation being less performant.
        if (this.preferNativeXMLHttpRequest) {
            xmlHttpRequest = this.getNativeRequest();
            if (!xmlHttpRequest) xmlHttpRequest = this.getActiveXRequest();
        } else {
            xmlHttpRequest = this.getActiveXRequest();        
            if (!xmlHttpRequest) xmlHttpRequest = this.getNativeRequest();
        }
   
        if (!xmlHttpRequest) isc.rpc.logWarn("Couldn't create XMLHttpRequest");
        return xmlHttpRequest;
    } else {
        // Moz, Safari
        return new XMLHttpRequest();
    }
},

getNativeRequest : function () {
   var xmlHttpRequest;
    if (isc.Browser.version >= 7) {
        isc.rpc.logDebug("Using native XMLHttpRequest");
        xmlHttpRequest = new XMLHttpRequest();
    }
    return xmlHttpRequest;
},

getActiveXRequest : function () {
    var xmlHttpRequest;

    if (!this._xmlHttpConstructor) {
        for (var i = 0; i < this.xmlHttpConstructors.length; i++) {
            try {
                var cons = this.xmlHttpConstructors[i];
                xmlHttpRequest = new ActiveXObject(cons);
                // cache selected constructor
                if (xmlHttpRequest) {
                    this._xmlHttpConstructor = cons;
                    break;
                }
            } catch (e) { }
        }
    } else {
        xmlHttpRequest = new ActiveXObject(this._xmlHttpConstructor);    
    }

    if (xmlHttpRequest) isc.rpc.logDebug("Using ActiveX XMLHttpRequest via constructor: " + this._xmlHttpConstructor);
    return xmlHttpRequest;
},


// Comm.sendHiddenFrame() defined in `application/SCServer.js'.


// _transactionCallbacks - array of callbacks for outstanding transactions
// fired from the various 'reply' methods
_transactionCallbacks:[],

// scriptInclude transport
// ---------------------------------------------------------------------------------------

sendScriptInclude : function (request) {
    var URL = request.URL,
        fields = request.fields,
        data = request.data,
        callbackParam = request.callbackParam,
        transaction = request.transaction
    ;
    
    // create a function for server-generated code to call, and capture the
    // transactionNum by having the function share this scope
    var cbName = "_scriptIncludeReply_"+transaction.transactionNum;
    this[cbName] = function () {
        // copy arguments to a real array
        var values = arguments.length == 1 ? arguments[0] : [];
        if (arguments.length > 1) {
            for (var i = 0; i < arguments.length; i++) values[i] = arguments[i];
        }
        isc.Comm.performScriptIncludeReply(transaction.transactionNum, values);
    }
    var callback = "isc.Comm."+cbName;

    var callbackParamObj = {};
    callbackParamObj[callbackParam ? callbackParam : "callback"] = callback;
    URL = isc.rpc.addParamsToURL(URL, fields);
    URL = isc.rpc.addParamsToURL(URL, callbackParamObj);
    if (transaction) transaction.mergedActionURL = URL;

    isc.rpc.logInfo("scriptInclude call to: " + URL);
    
    // store the transactionCallback to fire when the server returns
    this._transactionCallbacks[transaction.transactionNum] = transaction.callback;

    
    var document = this.getDocument(),
        body = this.getDocumentBody(),
        scriptElement = document.createElement("script");
    scriptElement.src = URL;
    body.appendChild(scriptElement);
},


performScriptIncludeReply : function (transactionNum, values) {
    // destroy auto-generated function for this transaction
    delete this["_scriptIncludeReply_"+transactionNum];
    var callback = this._transactionCallbacks[transactionNum];
    delete this._transactionCallbacks[transactionNum];
    
    this.logDebug("scriptInclude reply for transactionNum: " + transactionNum +
                  ", data: " + this.echoLeaf(values), "xmlBinding");

    this.fireCallback(callback, "transactionNum,results,wd", [transactionNum, values]);
},


// send via an xmlHttpRequest
sendXmlHttpRequest : function (request) {

    var URL = request.URL,
        fields = request.fields,
        httpMethod = request.httpMethod,
        contentType = request.contentType,
        headers = request.httpHeaders,
        data = request.data,
        transaction = request.transaction,
        blocking = request.blocking != null ? request.blocking : false
    ;


    
    
    this._transactionCallbacks[transaction.transactionNum] = transaction.callback;
    
    
    // set up a callback to notify us when the request completes
    var callback = "isc.Comm.performXmlTransactionReply(" +transaction.transactionNum+
                    ", xmlHttpRequest)";
                    
    if (!httpMethod) httpMethod = "POST";
    var xmlHttpRequest = this.createXMLHttpRequest();

    var loadFunc;
    if (isc.Browser.isIE) {
        
        loadFunc = this._getStateChangeHandler();
        loadFunc.request = xmlHttpRequest;
        loadFunc.callback = callback;

    } else {
        // we'll install this function to fire onreadystatechange
        loadFunc = function () {
        
        if (xmlHttpRequest.readyState != 4) return;
        isc.Comm._fireXMLCallback(xmlHttpRequest, callback);
        }   
    }
    xmlHttpRequest.onreadystatechange = request.onreadystatechange || loadFunc;

    if (isc.rpc.logIsDebugEnabled()) {
        this.lastXmlHttpRequest = xmlHttpRequest; // HACK for debugging
    }

    if (httpMethod == "POST" || httpMethod == "PUT") {
        // if data was passed in, use that as the body and encode any fields into the query
        // params
        if (data) {
            // assume the body being posted is XML if contentType is unset
            contentType = contentType || "text/xml";             
            URL = isc.rpc.addParamsToURL(URL, fields);
        } else {
            // send fields like a form post
            contentType = contentType || "application/x-www-form-urlencoded; charset=UTF-8";
            data = isc.SB.create();
            var first = true;
            for (var fieldName in fields) {
                if (!first) data.append("&");
                var value = fields[fieldName];
                data.append(isc.rpc.encodeParameter(fieldName, value));
                first = false;
            }
            data = data.toString();                        
        }
        if (isc.rpc.logIsDebugEnabled()) {
            isc.rpc.logDebug("XMLHttpRequest POST to " + URL + " contentType: " + contentType 
                             + " with body -->"+decodeURIComponent(data)+"<--");
        }
        xmlHttpRequest.open(httpMethod, URL, !blocking);
        
        var contentTypeSet = this._setHttpHeaders(xmlHttpRequest, headers);
        if (!contentTypeSet) {
            contentType == xmlHttpRequest.setRequestHeader("Content-Type", contentType);
        }
        if (transaction) {
            transaction.xhrHeaders = headers;
            transaction.xhrData = data;
        }
        if (data != null && !isc.isA.String(data)) {
            this.logWarn("Non-string data object passed to sendXML as request.data:"+ this.echo(data) +
                        " attempting to convert to a string.");
            data = data.toString ? data.toString() : "" + data;
        }
        xmlHttpRequest.send(data);
    } else {  // httpMethod == GET, DELETE, HEAD
        var urlWithFields = isc.rpc.addParamsToURL(URL, fields);
        xmlHttpRequest.open(httpMethod, urlWithFields, !blocking);

        // If bypassCache is set, use if-modified-since header to prevent cacheing of
        // XMLHttp GET responses.
        if (request.bypassCache) {
              
            xmlHttpRequest.setRequestHeader("If-Modified-Since", "Thu, 01 Jan 1970 00:00:00 GMT");
        }

        this._setHttpHeaders(xmlHttpRequest, headers);

        if (isc.rpc.logIsDebugEnabled()) {
            isc.rpc.logDebug("XMLHttpRequest GET from " + URL + 
                             " with fields: " + isc.Log.echoAll(fields) + 
                             " full URL string: " + urlWithFields);
        }
        
        xmlHttpRequest.send(null);
    }
    return xmlHttpRequest;
},




performXmlTransactionReply : function (transactionNum, xmlHttpRequest) {
    //!OBFUSCATEOK
    var callback = this._transactionCallbacks[transactionNum]
    delete this._transactionCallbacks[transactionNum];
    this.fireCallback(callback, "transactionNum,results,wd", [transactionNum, xmlHttpRequest]);
},

_setHttpHeaders : function (request, headers) {
    if (headers == null) return;
    var sawContentType = false;
    for (var headerName in headers) {
        var headerValue = headers[headerName];
        if (headerValue != null) {
            request.setRequestHeader(headerName, headerValue);
            if (headerName.toLowerCase() == "content-type") sawContentType = true;
        }
    }
    return sawContentType;
}

});

