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

 



//>	@class RPCManager
// 
// RPCManager is a static singleton class that manages transparent client/server RPC (remote
// procedure call).  This class provides a generic, low-level client/server communication
// integration point.
// <P>
// SmartClient's powerful databinding subsystem (see +link{DataSource}, 
// +link{DataBoundComponent,DataBoundComponents}) automatically make use of this class to issue
// RPCs as necessary, based on the 
// +link{group:dataSourceOperations,DataSource protocol}. To integrate DataBoundComponents
// with your server, +link{group:clientServerIntegration,start here}.
// <P>
// For arbitrary client/server interactions outside of the DataSource subsystem, the
// SmartClient server also provides the +link{group:dmiOverview,Direct Method Invocation} feature.
// <P>
// The RPCManager class can also be used <i>directly</i> to send data to a URL of your
// choosing and optionally be called back with server-returned data when the server replies.
// <P>
// The SmartClient +link{group:iscServer,server code} has APIs for processing RPC requests 
// providing features such as automatic Java &lt;--&gt; JavaScript object translation 
// and handling of queued requests.<br>
// The +link{group:servletDetails,IDACall servlet} makes use of these features to handle standard
// +link{DataSource} requests and +link{DMI} calls. Developers can also override the
// <code>actionURL</code> of specific requests and use these APIs directly in a 
// JSP, Servlet or Filter.
// <P>
// Note: the client-side RPCManager class can also be used without the SmartClient server.
// For an overview of client/server interactions without the SmartClient server, see
// +link{group:nonJavaBackend,this overview}.
// <P>
// <u>Simple arbitrary Remote Procedure Call example (client code):</u>
// <smartclient>
// <P>
// <pre>
// var data = { here: "is some data", to: ["send to the server"]};
// isc.RPCManager.sendRequest({ data: data, callback: "myCallback(data)", actionURL: "/rpcHandler.jsp"});
// function myCallback(data) { alert("response from the server: " + data); }
// </pre>
// </smartclient>
// <smartgwt>
// <P>
// <pre>
//  RPCRequest request = new RPCRequest();
//  // Note data could be a String, Map or Record
//  request.setData("Some data to send to the client");
//  request.setActionURL("/rpcHandler.jsp");
// 
//  RPCManager.sendRequest(request, 
//      new RPCCallback () {
//          public void execute(RPCResponse response, Object rawData, RPCRequest request) {
//              SC.say("Response from the server:" + rawData);
//          }
//      }
//  );
// </pre>
// </smartgwt>
// <P>
// <u>Simple arbitrary Remote Procedure Call example (server code: /rpcHandler.jsp):</u>
// <br><br><pre>
// RPCManager rpc = new RPCManager(request, response, out);
// Object data = rpc.getData();
// System.out.println("client sent: " + data.toString());
// rpc.send("here's a response");
// </pre>
// <P>
// <u><b>Queuing</b></u>
// <br>
// Because of browser limitations on the total number of simultaneous HTTP connections to a given
// server, batching multiple RPC requests into a single HTTP request is highly advisable whenever
// possible.  The RPCManager provides a queuing mechanism that allows this.
// <br><br>
// <u>Queuing example (client code):</u>
// <smartclient>
// <pre>
// var wasQueuing = isc.RPCManager.startQueue();
// isc.RPCManager.send("a string of data", "myCallback(data)", {actionURL: "/rpcHandler.jsp"});
// isc.RPCManager.sendRequest({ data: ["some", "more data", 2], callback: "myCallback(data)", actionURL: "/rpcHandler.jsp"});
// isc.RPCManager.sendRequest({ data: "different callback", callback: "myCallback2(data)", actionURL: "/rpcHandler.jsp"});
// if (!wasQueuing) isc.RPCManager.sendQueue();
// 
// function myCallback(data) { alert("response from the server: " + data); }
// function myCallback2(data) { alert("response from the server (other callback): " + data); }
// </pre>
// </smartclient>
// <smartgwt>
// <pre>
// boolean wasQueuing = RPCManager.startQueue();
//	 
// RPCCallback callback = new RPCCallback() {
//     public void execute(RPCResponse response, Object rawData, RPCRequest request) {
//         Window.alert("response from server:" + rawData);
//     }
// };
//		 
// RPCRequest request1 = new RPCRequest();
// request1.setActionURL("/rpcHandler.jsp");
// request1.setData("A String of Data");
// RPCManager.sendRequest(request1, callback);
//		 
// RPCRequest request2 = new RPCRequest();
// request2.setActionURL("/rpcHandler.jsp");
// request2.setData("Another String of Data");
// RPCManager.sendRequest(request2, callback);
//		 
// if (!wasQueuing) RPCManager.sendQueue();
// </pre>
// </smartgwt>
// <p>
// <u>Queuing example (server code: /rpcHandler.jsp):</u>
// <br><br><pre>
// RPCManager rpc = new RPCManager(request, response, out);
//
// for(Iterator i = rpc.getRequests().iterator(); i.hasNext();) {
//     RPCRequest rpcRequest = (RPCRequest)i.next();
//     Object data = rpcRequest.getData();
//     System.out.println("client sent:" + data.toString());
//
//     //send back the data sent to us by the client
//     rpc.send(rpcRequest, new RPCResponse(data));
// }<br>
// </pre>
// <br><br>
// <u><b>Error Handling</b></u><br><br>
// Please see this +link{group:errorHandling,separate article} on error handling.
// <br>
//
// @treeLocation Client Reference/RPC
// @visibility external
//<
isc.ClassFactory.defineClass("RPCManager");
isc.RPC = isc.rpc = isc.RPCManager;
//>Offline
isc.Page.observe(isc, "goOffline", "isc.rpc.goOffline()");
isc.Page.observe(isc, "goOnline", "isc.rpc.goOnline()");
//<Offline



// ---------------------------------------------------------------------------------------
//> @method Callbacks.LoadScreenCallback
// A +link{type:Callback} to evaluate when a screen is loaded via +link{RPCManager.loadScreen()}.
//
// @param [screen] (Canvas) The last top-level component loaded
// @param [rpcResponse] (RPCResponse)
// @param [suppressedGlobals] (Map) A collection of suppressed globals.
//
// @visibility external
//<

isc.ClassFactory.defineClass("LoadScreenCallback");
isc.LoadScreenCallback.addProperties({
    execute : function() {
    }
});

// ---------------------------------------------------------------------------------------
//>	@class RPCRequest
// 
// Encapsulates a client/server RPC request.  You'll need to provide an instance of this class (or a
// constructor for it) to the +link{classMethod:RPCManager.sendRequest()} method.  If you use the 
// +link{classMethod:RPCManager.send()} method, an instance of RPCRequest will be created for you.
//
// @see RPCManager.send()
// @see RPCManager.sendRequest()
// @visibility external
// @treeLocation Client Reference/RPC
//<
isc.ClassFactory.defineClass("RPCRequest");

isc.RPCRequest.addClassMethods({
    //> @classMethod RPCRequest.create()
    // RPCRequest shouldn't be created directly. Instead, pass +link{Properties} to 
    // +link{RPCManager.sendRequest()} and +link{RPCManager.send()}.
    // @visibility external
    //<
    // Log a warning if called directly
    create : function (A,B,C,D,E,F,G,H,I,J,K,L,M) {
        this.logWarn(
            "An RPCRequest does not need to be created. Instead, pass properties to methods " +
            "such as RPCManager.send() and RPCManger.sendRequest."
        );
       
        return isc.addProperties({}, A,B,C,D,E,F,G,H,I,J,K,L,M);
    }
});

//> @attr rpcRequest.data (String | Record | Object : null : IRW)
//
// This attribute specifies the payload of the RPCRequest.  
// <smartclient>
// When using the +link{group:iscServer,SmartClient server},
// any JavaScript simple type or arbitrarily nested set of Objects and Arrays can be sent
// to server and automatically translated to Java Objects.  
// </smartclient>
// <smartgwt>
// When using the +link{group:iscServer,SmartClient server}, objects sent to the server as
// <code>request.data</code> will be available on the server-side <code>RPCRequest</code>
// object as Java Objects. This is achieved by serializing the client side data
// in a JSON type format and generating Java Objects on the server from this serialized data.
// <P>
// If the client side <code>request.data</code> is set to a Java object in your SmartGWT code
// it will be serialized as JSON as follows:<br>
// - Numeric client side values (int, Integer, etc) will be serialized as JavaScript numbers.<br>
// - String values will be serialized as JavaScript strings.<br>
// - Date values will be serialized as JavaScript dates.<br>
// - Maps or Record objects will be serialized as JavaScript Objects.<br>
// - Arrays or Lists will become JavaScript arrays<br>
// Serialization of Maps and Arrays is recursive - each entry in an Array, or attribute
// on a Map will also be serialized according to the above rules.<br>
// Note that you can also set <code>request.data</code> directly to a JavaScriptObject,
// and use the <code>JSOHelper</code> class or <code><i>SomeObject.</i>getJSObj()</code> to 
// perform your own data conversions on the client. The serialized JavaScript will then be
// converted back to Java on the server according to the following rules.
// </smartgwt>
// <P>
// Here are the 
// mapping of JavaScript types to their corresponding server object types:<br><br>
//
// <table class='normal' border='1'>
//   <tr><td><b>JS Type</b></td>     <td><b>Java Type</b></td></tr>
//   <tr><td>Object: {}</td>         <td>Map</td></tr>
//   <tr><td>Array: []</td>          <td>List</td></tr>
//   <tr><td>String</td>             <td>String</td></tr>
//   <tr><td>Number</td>             <td>Long|Double</td></tr>
//   <tr><td>Boolean</td>            <td>Boolean</td></tr>
//   <tr><td>Date</td>               <td>java.util.Date</td></tr>
//   <tr><td>String</td>             <td>com.smartgwt.client.types.ValueEnum</td></tr>
//
// </table>
// <br><br>
// Note that the order of keys/values in the Maps created on the server is not guaranteed
// because JavaScript Object literals do not guarantee order.
// <p>
// Server->client conversion follows this table as well, with some extras.  See the toJS()
// method on JSTranslater in the server documentation for a description of additional
// behaviors.
// <P>
// When <b>not</b> communicating with the SmartClient server, <code>rpcRequest.data</code>
// becomes simple HTTP parameters or an HTTP request body - see +link{rpcRequest.useSimpleHttp}
// for details.
//
// @see RPCResponse.data
// @visibility external
//<

//> @attr rpcRequest.actionURL (URL : RPCManager.actionURL : IRW)
//
// Overrides RPCManager.actionURL for this request only.  If you're using queuing, note that queues
// as per-URL - in other words all RPCRequests in a queue must go to a single URL.  If you attempt
// to send a request with an actionURL that is different from those already in the queue, it
// will be sent to the server separately, ahead of the queue,  and a warning will be logged to
// the Developer Console.
// 
// @see classAttr:RPCManager.actionURL
//
// @visibility external
//<

//> @attr rpcRequest.useHttpProxy (boolean : null : IR)
// Indicates whether this request should use the HttpProxyServlet in order to enable contacting
// hosts other than the origin server (available only in Pro Edition or better).
// <P>
// When various UI components issues requests automatically, or when a call to
// +link{RPCManager.sendProxied()} is made, the HttpProxy will automatically be used for a URL
// that starts with "http" and uses a hostname other than "localhost" or
// <code>window.location.hostname</code>, or if the port number differs.
// <P>
// <code>rpcRequest.useHttpProxy</code> should only be used to force requests to go through the
// HttpProxy when the above rules don't work, or to avoid using the HttpProxy when contacting
// hosts that allow cross-site calls via the
// +externalLink{http://www.google.com/search?q=http+access+control,Http Access Control}
// standard.
// <P>
// You can also set +link{RPCManager.useHttpProxy}:false to avoid ever using the
// HttpProxyServlet.
//
// @visibility external
//<

//> @attr rpcRequest.httpProxyURL (string : null : IR)
// The proxy URL to use for this request (if +link{rpcRequest.useHttpProxy} is set for this
// request).  If unset, the value of +link{RPCManager.httpProxyURL} will be used instead.
//
// @see RPCManager.httpProxyURL
// @visibility external
//<

//> @groupDef rpcPrompt
// The properties in this group all deal with setting and styling a modal prompt during an RPC
// call to the server.
// @visibility external
//<

//> @attr rpcRequest.promptStyle (PromptStyle : RPCManager.promptStyle : IRW)
// Controls the prompt style for this request only.  Defaults to
// +link{RPCManager.promptStyle}.
//
// @see RPCManager.promptStyle
// @group rpcPrompt
// @visibility external
//<

//> @classAttr rpcRequest.useCursorTracker (boolean : platform-dependent : IRW)
//
// If true, an image is shown to the right of the cursor when +link{rpcRequest.promptStyle} is
// set to "cursor", otherwise the cursor itself is modified via css to the value of
// +link{rpcRequest.promptCursor}.  The default is platform-dependent.  In Safari, IE 5.5 and
// Firefox 1.0 the default is true, on all other platforms it is false.  The reason for this
// split is that the above browsers require that the cursor move before CSS settings are
// re-evaluated - this means the progress cursor can stick until the user moves the mouse.
// <p>
// If left unspecified, the default value is set by +link{RPCManager.useCursorTracker}.
//
// @see RPCManager.useCursorTracker
// @group rpcPrompt
// @visibility external
//<

//> @attr rpcRequest.promptCursor (String : browser-dependent : IRW)
// Controls the cursor shown when +link{rpcManager.promptStyle} is set to
// <code>"cursor"</code> for this request only. Defaults to +link{RPCManager.promptCursor}.
// <p>
// In Safari, IE 5.5 and Firefox 1.0 the default value is "wait", on all other platforms it is
// "progress".  The reason for this split is that the above-mentioned browsers do not support
// CSS2.1 - which is required for the "progress" cursor type.
//
// @see RPCManager.promptCursor
// @group rpcPrompt
// @visibility external
//<


//> @attr rpcRequest.prompt (HTMLString : RPCManager.defaultPrompt : IRW)
//
// Overrides RPCManager.defaultPrompt for this request only.  If you're using queuing, note that the
// prompt string from the first request in the queue is the one that is shown to the user.
// 
// @see classAttr:RPCManager.defaultPrompt
// @see classAttr:RPCManager.showPrompt
// @see classAttr:RPCManager.promptStyle
// @see classAttr:RPCManager.promptCursor
// @see attr:rpcRequest.showPrompt
// @see attr:rpcRequest.promptStyle
// @see attr:rpcRequest.promptCursor
//
// @group rpcPrompt
// @visibility external
//<

//> @attr rpcRequest.showPrompt (Boolean : null : IRW)
// Overrides <code>RPCManager.showPrompt</code> for this request only.
// <p>
// If you're using queuing, note that if any of the requests in the queue specify
// showPrompt:true, then a prompt will be shown for the entire queue with the prompt text of
// the first request in the queue to specify a custom prompt if promptStyle is set to "dialog".
// <p>
// If promptStyle is set to "cursor" for the request that specified showPrompt: true, then the
// entire queue uses the "cursor" style for the prompt.
//
// @see classAttr:RPCManager.showPrompt
// @group rpcPrompt
// @visibility external
//<

//> @attr rpcRequest.promptDelay (number : RPCManager.promptDelay : IRWA)
//
// Overrides RPCManager.promptDelay for this request only. Defaults to +link{RPCManager.promptDelay}.
// <p>
// If you're using queuing, note that the promptDelay of the first request is used for the entire queue.
//
// @see attr:rpcRequest.showPrompt
// @see classAttr:RPCManager.promptDelay
// @group rpcPrompt
// @visibility external
//<

//> @attr rpcRequest.callback (RPCCallback : null : IRW)
// 
// If you expect to receive a response to your RPC request, you can specify a callback that
// will be called with an instance or RPCResponse class as sent by the server.  Queuing does
// not affect callbacks in any way - your specified callback will be invoked for each
// RPCRequest that contained a callback regardless of whether the request was sent as part of a
// queue or not.
// <P>
// Note that if the request encounters an error (such as 500 server error), by default the
// callback will <b>not</b> be fired, instead, +link{RPCManager.handleError()} is called to
// invoke the default system-wide error handling.  Set +link{willHandleError}:true to have your
// callback invoked regardless of whether there are errors, however, make sure your callback
// properly handles malformed responses when +link{RPCResponse.status} is non-zero.  See the
// +link{group:errorHandling,error handling overview} for more details.
//
// @group errorHandling
// @visibility external
//<

//> @attr rpcRequest.clientContext (Object : null : IRW)
// 
// An object to be held onto for the duration of the RPC turnaround to track
// application-specific context.
// <br>
// When an RPC turnaround completes, the <code>clientContext</code> is available in the
// +link{Callbacks.RPCCallback,RPCCallback} as <code>rpcResponse.clientContext</code>.  The
// <code>clientContext</code> is never sent to the server.
// <br>
// The <code>clientContext</code> is useful for holding onto state that will be used when the
// +link{Callbacks.RPCCallback,RPCCallback} fires, such as the name of a component that will receive the
// returned data.
// 
// @see RPCResponse.clientContext
//
// @visibility external
//<

//> @attr rpcRequest.willHandleError (Boolean : false : IRW)
//
// With willHandleError:false, rpcResponses that indicate an error go through centralized
// handling in the RPCManager and rpcRequest.callback is never invoked.
// <P>
// Setting willHandleError:true means that your rpcRequest.callback will receive rpcResponses
// that have an error status and must handle them.
// <P>
// See also the error handling section in the +link{class:RPCManager} docs.
// 
// @group errorHandling
// @see class:RPCManager
//
// @visibility external
//<

//> @attr rpcRequest.timeout (int : null : IRWA)
// Sets the timeout on this request.  Default is to use +link{RPCManager.defaultTimeout}.  
// <p>
// If you're using +link{RPCManager.startQueue,queuing}, note that the timeout setting derived
// from the last request in the queue is used for the entire queue.  If you want to override
// the timeout for the queue, make sure to set your override at least on the last request in
// the queue.
// <p>
// For the "xmlHttpRequest" +link{rpcRequest.transport,transport}, this timeout can only happen
// if the server actually fails to respond within the specified number of milliseconds.  For
// the "hiddenFrame" transport, this timeout will occur for non-200 (HTTP_OK) responses.
// <p>
// If <code>timeout</code> is set to zero, the RPCManager will not enforce a timeout for this
// request.  However, note that all browsers enforce their own timeouts on HTTP requests, and
// may have different timeouts for different kinds of failures (no response at all from server,
// hung response after receiving headers, hung response after receiving partial data, etc).
// Also, intervening web proxies or firewalls may impose timeouts of their own.
// <p>
// As a rough rule of thumb, if your server response will have a lengthy pause before data
// begins to be sent, 1-2 minutes is the maximum allowable pause for a public site and still may
// not work for a minority of users, but up to 4 minutes may be allowable in a controlled
// environment (intranet or extranet with well-known user base).
// <p>
// Above these limits, your code should return some kind of immediate response to the browser,
// then kick off a server-side process to complete processing.  The browser can then either
// poll for completion, or use a server-push notification system such as SmartClient Real-Time
// Messaging (see +externalLink{http://smartclient.com/product}).
//
// @see classAttr:RPCManager.defaultTimeout
//
// @visibility external
//<

//> @attr rpcRequest.clientOnly (boolean : false : IRWA)
// 
// Used for testing/prototyping without a server.  <code>clientOnly</code> requests don't get
// sent to the server, but the standard callback chain is still invoked.  If all requests in a
// transaction are clientOnly then the callbacks are called immediately otherwise they're
// called when the server returns responses to the non-clientOnly requests.
// 
// @visibility internal
//<

//> @attr rpcRequest.params (Object : null : IRW)
// 
// Values to be sent as simple HTTP params, as a JavaScript Object where each property/value
// pair will become an HTTP parameter name and value.  These parameters are then accessible on
// the server, for example, using servletRequest.getParameter(paramName) in Java Servlets.  
// <P>
// Array-valued parameters will be submitted as multiple instances of the same parameter,
// similar to an HTML form with a multi-select (?paramName=value1&amp;paramName=value2 ...),
// accessible as getParameterValues(paramName) in Java Servlets.  Any non-atomic type, such as
// an Object, will be serialized to +externalLink{http://www.json.org/,JSON} by the
// +link{JSONEncoder}.  If this isn't desirable, serialize the data in advance so that the
// value provided in <code>rpcRequest.params</code> is a String.
// <P>
// Note that this API is primarily used in combination with +link{rpcRequest.useSimpleHttp} -
// when contacting the SmartClient Server, use +link{RPCRequest.data} instead, which provides
// full JavaScript &lt;-&gt; Java translation of arbitrary structures.
// <code>rpcRequest.params</code> can also be used with the SmartClient Server, where it
// provides an an opportunity to send additional data aside from the main
// +link{rpcRequest.data} payload.  This is useful for adding data to DataSource requests which
// will be kept separate from the automatically sent DataSource data, or for making parts of
// the request visible in the URL for HTTP-level logging or layer 4 switches.
// <P>
// Note that in contrast to +link{rpcRequest.data} object, the data in
// <code>rpcRequest.params</code> is not deserialized by the SmartClient server, and
// all values arrive on the server as String type (like HTTP parameters always do).
// <p>
// <smartclient>
// The params value can also be specified as a componentID or component instance that provides
// a method getValues() that returns an Object containing parameter names and values.
// SmartClient components +link{class:DynamicForm}, +link{class:ValuesManager} are two such
// classes.  Lastly, you may specify the ID of a native form element (retrievable via
// getElementById()) and the params will be populated from there.  If there is an error
// resolving your params directive, it will be logged to the Developer Console.
// </smartclient>
// <p>
// Note: The params are submitted once per http transaction.  If you are using 
// +link{RPCManager.startQueue(),request queuing} to bundle multiple RPCRequests or DSRequests
// into a single HTTP turnaround, the params from the various RPCRequests will be merged,
// with the later-queued transactions winning on parameter name collisions.  A warning will be
// logged in the Developer Console if multiple RPCRequests specified params.
//
// @visibility external
//<

//> @attr rpcRequest.evalResult (Boolean : false : IRWA)
// 
// This works similarly to +link{RPCRequest.serverOutputAsString} except the resulting String
// is automatically evaluated as JavaScript.  The result of the evaluation is then passed to
// any specified +link{RPCRequest.callback} as +link{RPCResponse.data}.
// <p>
// This feature can be used to dynamically load new application modules into a running
// application.  An RPCRequest with <code>evalResult</code> enabled can be used to fetch a
// static .js file or JavaScript dynamically generated by the server.  The returned JavaScript
// can contain anything that a JavaScript file loaded at init time can contain, including new
// views and new SmartClient class definitions.
// <p>
// <i>Example usage with +link{RPCManager.sendRequest()}:</i>
// <pre>
// isc.RPCManager.sendRequest({
//     evalResult:true,
//     actionURL:"js/loadLabel.js",
//     evalVars:{var1:"A Value"}
// });
// </pre>
// This call would execute the code from <code>loadLabel.js</code>, and make the variable
// <code>var1</code> available to that code. Therefore if the .js file contained this code:
// <pre>
// isc.Label.create({
//     contents:var1
// })
// </pre>
// A label would be created with contents set to the value of <code>var1</code> - the string
// <code>"A Value"</code>.
// 
// <p>
// This feature relies on the XMLHttpRequest object which can be disabled by end-users in some
// supported browsers.  See +link{group:platformDependencies} for more information.
//
// @see class:ViewLoader
// @see rpcRequest.evalVars
// @group viewLoading
// @visibility external
//<

//> @attr rpcRequest.evalVars (Object : null : IRWA)
// 
// If you've set +link{RPCRequest.evalResult} : true, then the property values of this object
// will be available in the evaluation scope of the result under the variable names
// specified by the property names. 
// <p>
// So e.g. if evalVars is: <code>{foo: "bar"}</code> then a reference to the
// variable <code>foo</code> in the result will evaluate to <code>"bar"</code>.
//
// @group viewLoading
// @visibility external
//<

//> @attr rpcRequest.callbackParam (String : "callback" : IRW) 
//
// For use only with the +link{type:RPCTransport,scriptInclude} transport, this attribute
// specifies the name of the URL parameter which is used to specify the callback function that
// the server is expected to call by writing out JavaScript code.  The actual function to call
// is automatically generated and differs for every request (to allow concurrency).
// <P>
// For example, with <code>callbackParam</code> set to it's default value of "callback", the
// server might be contacted with a URL like:
// <pre>
//    loadData?callback=isc_scriptIncludeCallback_5
// </pre>
// .. then the server's response should look like:
// <pre>
//    isc_scriptIncludeCallback_5({ .. data .. });
// </pre>
// The name "isc_scriptIncludeCallback_5" is automatically generated and will differ each time
// the server is contacted.
// <P>
// SmartClient makes of this server-provided callback mechanism, then calls
// +link{rpcRequest.callback} normally.
// <p>
// <code>rpcRequest.callbackParam</code> is ignored by all transport other than
// <code>scriptInclude</code>.
//
// @visibility external
//<

//> @attr rpcRequest.suppressAutoDraw (Boolean : true : IRWA)
// 
// If +link{attr:RPCRequest.evalResult} is set, setting this property to true causes
// +link{attr:Canvas.autoDraw} to be set to false for the duration of the result evaluation -
// which is generally what you want if you're returning new components from the server.
// <P>
// This also effects components loaded via the +link{RPCManager.loadScreen} API.
//
// @visibility external
//<

//> @attr rpcRequest.serverOutputAsString (Boolean : false : IRWA)
//
// Setting this flag makes the body of the HTTP response available as a String in the
// +link{RPCRequest.callback} as +link{RPCResponse.data}.  This is typically only useful if you
// are sending a request that will <b>not</b> be received by the SmartClient Java Server,
// however in that case, set +link{useSimpleHttp}:true instead, which implies
// <code>serverOutputAsString:true</code>.
// <P>
// <code>serverOutputAsString:true</code> allows you to, for example, load the contents of
// static files off your webserver into a string for processing on the client with no server
// support.  The +link{RPCRequest.actionURL} must be in the same domain as the current page for
// this to work.
// <p>
// This feature relies on the XMLHttpRequest object which can be disabled by end-users in some
// supported browsers.  See +link{group:platformDependencies} for more information.
// <p>
// Generally this API is used for either +link{group:nonJavaBackend,non-Java backends} 
// or for advanced usage such as content that requires processing before it can be used in
// SmartClient components (such as client-side web scraping).  Note that SmartClient provides
// higher-level APIs for loading common types of data, see eg +link{HTMLFlow} for HTML content,
// +link{ViewLoader} for loading SmartClient components, +link{XMLTools.loadXML()} for loading
// XML, +link{RPCRequest.evalResult} for loading +externalLink{http://www.json.org/,JSON}, and
// +link{DataSource} for loading structured data in various formats.
//
// @visibility external
//<

//> @attr RPCRequest.allowIE9Leak (boolean : null : IRA)
// Advanced flag to avoid a potential memory leak in Internet Explorer 9 for requests
// with JSON formatted responses.
// <P>
// This attribute may be set to <code>false</code> to explicitly enable the 
// workaround described +link{RPCManager.allowIE9Leak,here} for this request, 
// avoiding a potential memory leak in Internet Explorer 9.
// <P>
// This workaround has a limitation in that if parsing the JSON response generates
// certain object types including JavaScript <code>Date</code> or <code>function</code>
// objects, attempts to interact with these objects can subsequently lead to a 
// JavaScript error with the message <code>"Can't execute code from a freed script"</code>.
// <P>
// This workaround therefore may not be suitable for all transactions or dataSources
// within a given application.
// <P>
// This property may also be set globally within an application (via 
// +link{RPCManager.allowIE9Leak})_.
// <P>
// Note: This memory leak and workaround is discussed further in the online 
// <a href="http://forums.smartclient.com/showthread.php?t=8159">SmartClient FAQ</a>.
//
// @visibility external
//<

//> @attr rpcRequest.transport (RPCTransport : RPCManager.defaultTransport : IRWA)
//
// Selects the transport used for this RPCRequest.  If unset, the value of
// +link{RPCManager.defaultTransport} will be used.
// <p>
// If you're using queueing, note that all requests in the queue must use the same transport.
// If you attempt to send a request via a different transport than those that are currently on
// the queue, it will be sent to the server separately, ahead of the queue, and a warning will
// be logged to the Developer Console.
// <p>
// If you specify an unknown transport, an error will be logged to the DeveloperConsole and
// +link{RPCManager.defaultTransport} will be used instead.
// <p>
// If you specify the <code>xmlHttpRequest</code> transport and it is not available, a warning will be
// logged to the Developer Console and the RPCManager will attempt to use the
// <code>hiddenFrame</code> transport instead for this request.  Note that some features like
// +link{RPCRequest.serverOutputAsString} require the <code>xmlHttpRequest</code> transport and will not
// work if the <code>xmlHttpRequest</code> transport is unavailable (this can happen if the end user is
// using Internet Explorer and has disabled ActiveX).  You can check whether or not the
// <code>xmlHttpRequest</code> transport is currently available by calling
// +link{RPCManager.xmlHttpRequestAvailable}.
//
// @see RPCManager.defaultTransport
//
// @visibility external
//<

//> @attr rpcRequest.useXmlHttpRequest (boolean : RPCManager.useXmlHttpRequest : IRWA)
//  
// Selects the default http transport for this RPCRequest.  If set to true, this request will use
// XMLHttpRequest for the transport to the server.  If set to false it will use a hidden frame.  If
// left unset, the transport mechanism is determined from the RPCManager default set in 
// +link{RPCManager.useXmlHttpRequest}
// <p>
// If you're using queueing, note that all requests in the queue must use the same transport.
// If you attempt to send a request via a different transport than those that are currently on
// the queue, it will be sent to the server separately, ahead of the queue, and a warning will
// be logged to the Developer Console.
// <p>
// If you specify <code>true</code> for this attribute and XMLHttp is not available, a warning
// will be logged to the Developer Console and RPCManager will attempt to use the frames
// transport for this request.  Note that some features like
// +link{RPCRequest.serverOutputAsString} require the XMLHttp transport and will not work if the
// XMLHttp transport is unavailable (this can happen if the end user is using Internet Explorer
// and has disabled ActiveX).  You can query the availability of XMLHttp by calling
// +link{RPCManager.xmlHttpRequestAvailable()}
// 
// @deprecated As of SmartClient 5.5, use +link{RPCRequest.transport}.  If you specify a value
// for this property, it will take precedence over +link{RPCRequest.transport}.
//
// @see RPCManager.useXmlHttpRequest
// @see RPCManager.xmlHttpRequestAvailable()
//
// @visibility external
//<

					
//> @attr RPCRequest.httpMethod (String : "POST" : IRW)
//
// Selects the HTTP method that will be used for the request.  Typical values are "POST" and
// "GET".
// <P>
// The more obscure "PUT", "DELETE" and "HEAD" methods are also valid, however, none of these
// are supported by the Safari browser previous to version 3.0.
//
// @visibility external
//<

					
//> @attr RPCRequest.contentType (String : "application/x-www-form-urlencoded" : IRW)
//
// Valid with the xmlHttpRequest transport only and only when
// +link{attr:RPCRequest.httpMethod} is set to "POST".  
//
// @visibility external
//<
					
//> @attr RPCRequest.httpHeaders (Object : null : IRW)
// HTTP headers to send, as a Object mapping Header name -> Header value, eg<br>
// { "Content-Type" : "text/xml" }
// <P>
// Valid with the xmlHttpRequest +link{rpcRequest.transport,transport} only.
//
// @visibility external
//<


//> @attr RPCRequest.containsCredentials (Boolean : false : IRWA)
// For use during +link{group:relogin,Relogin}, this property marks this request an attempt to
// login, therefore a response containing the <code>loginRequiredMarker</code> is a normal
// condition and should result in the status code +link{RPCResponse.STATUS_LOGIN_INCORRECT}
// rather than a call to +link{RPCManager.loginRequired(),loginRequired()}.
// <P>
// It is not required to set <code>containsCredentials</code>, however, it does typically
// simplify relogin logic by separating the handling of RPCs that are login attempts from RPCs
// that are not.
// 
// @group relogin
// @visibility external
//<

//> @attr RPCRequest.canDropOnDelay (boolean : false : IRWA)
//  
// If the transaction containing this request is requested to be delayed for some reason
// (Authentication relogin is one case), then this flag notifies the server that this request does
// not have to be ultimately fulfilled when the transaction is unblocked.<p>
//
// Typically you would set this flag on requests that periodically refresh a component every N
// seconds, so only the last update is important.
//
// @visibility internal
//<

//> @attr RPCRequest.ignoreTimeout (Boolean : false : IRWA)
//
// When set to true, no reply is expected from the server.  However, if a reply is received, it will
// be processed.<p>
//
// Note: setting this to true, forces +link{attr:RPCRequest.sendNoQueue} to <code>true</code> for
// this request.
//
// @visibility external
//<

//> @attr RPCRequest.sendNoQueue (Boolean : false : IRWA)
//
// When set to true, this request is sent to the server immediately, bypassing any current queue.
//
// @visibility external
//<

//> @attr RPCRequest.paramsOnly (boolean : false : IRWA)
//
// When set to true, assume the request is not going to the SmartClient server, and hence send
// a simple HTTP request.  Values specified in +link{attr:RPCRequest.params} are sent to to the
// server as HTTP request parameters.  If +link{httpMethod} method is POST and
// +link{rpcRequest.data} is supplied, it is assumed to be a string to post as the HTTP
// requestBody.
// <p>
// Setting this to true automatically defaults +link{RPCRequest.serverOutputAsString} to true
// as well.
//
// @deprecated As of SmartClient 5.6, use +link{RPCRequest.useSimpleHttp} instead.
// @visibility external
//<

//> @attr RPCRequest.useSimpleHttp (Boolean : false : IRWA)
//
// When set to true, assume the request is not going to the SmartClient server, and hence send
// a simple HTTP request that does not use SmartClient-specific request encoding.
// <P>
// Values specified in +link{attr:RPCRequest.params} are sent to to the server as HTTP request
// parameters.  If +link{httpMethod} is "GET", parameters appear in the request URL, otherwise
// if httpMethod is "POST", parameters are encoded in the request body (exactly like an HTML form
// does).  These parameters are then accessible via typical server-side APIs for retrieving
// HTTP parameters, eg, servletRequest.getParameter(paramName) in Java Servlets.  
// <P>
// Note that if +link{httpMethod} method is POST and +link{rpcRequest.data} is supplied,
// +link{rpcRequest.data} is assumed to be a string to post as the HTTP request body, and
// +link{rpcRequest.params} are sent as URL parameters instead.  This usage is for sending
// custom request bodies such as the XML payloads used for SOAP.  In this case,
// +link{rpcRequest.contentType} is typically also set to indicate the content type of the
// request body.
// <p>
// Setting <code>useSimpleHttp</code> to true also automatically sets
// +link{RPCRequest.serverOutputAsString} to true as well.
//
// @visibility external
//<

//> @attr RPCRequest.bypassCache (Boolean : false : IRWA)
//
// For xmlHttp transport + httpMethod: "GET" only, set to true to force a conditional
// GET request even if the browser thinks it has a current cached response.
//
// @visibility external
//<


//> @attr RPCRequest.omitNullMapValuesInResponse (Boolean : false : IRWA)
//
// If enabled, the server omits any key/value pairs in map that have null values from the
// response.  This can reduce the size of the response when many fields have null values.
// <p>
// To enable this globally for all responses you can set RPCManager.omitNullMapValuesInResponse
// in +link{group:server_properties,server.properties}.
//
// @visibility external
//<

//> @attr RPCRequest.downloadResult (Boolean : false : IRWA)
//
// If enabled, causes the RPCRequest to download the requested resource as a file, either 
// showing the browser's Save dialog or displaying the file-content in 
// +link{rpcRequest.downloadToNewWindow, a new browser window}.
// <P>
// Setting this attribute to true means that no callback will be fired and implies that the 
// request will silently use +link{rpcRequest.transport, transport}: "hiddenFrame".
// 
// @visibility external
//<

//> @attr RPCRequest.downloadToNewWindow (Boolean : false : IRWA)
// 
// When +link{rpcRequest.downloadResult, downloadResult} is true, setting this attribute to
// true causes the content of the downloaded file to be displayed in a new browser window. 
//
// @visibility external
//<

// ---------------------------------------------------------------------------------------
//> @method Callbacks.RPCCallback
// A +link{type:Callback} to evaluate when an RPCRequest completes.
// <smartclient><p>
// Parameters passed to this callback are:
// <ul>
// <li>rpcResponse: an +link{class:RPCResponse} encapsulating the server response to your
//     request
// <li>data: just the "data" property from the RPCResponse, for convenience
// <li>rpcRequest: the +link{class:RPCRequest} that was sent.  You can use
//     +link{attr:rpcRequest.clientContext} to track state during the server turnaround.
// </ul>
// For example, to take the data returned by the server and display it in a previously created
// ListGrid with the ID "myGrid":
// <pre>
//     isc.RPCManager.send("getData", "myGrid.setData(data)");
// </pre>
// Or
// <pre>
//     isc.RPCManager.send("getData", function (rpcResponse, data, rpcRequest) { 
//                                        myGrid.setData(data)
//     });
// </pre></smartclient>
//
// @param response (RPCResponse) response a RPCResponse encapsulating the server response to your request
// @param rawData  (any) rawData The "data" property from the RPCResponse, for convenience.  The data can
// also be obtained via {@link RPCResponse#getDataAsMap()}, {@link RPCResponse#getDataAsString()},
// or {@link RPCResponse#getDataAsObject()}, depending on the type of data that is expected to be
// returned from the server.
// @param request (RPCRequest) the RPCRequest that was sent.
//
// @see class:RPCRequest
// @see class:RPCResponse
// @visibility external
//<



// ---------------------------------------------------------------------------------------
//>	@class	RPCResponse
//
// Encapsulates an RPC response from the server.  Instances of this class are automatically created
// and optionally passed to you in the callback you specify as part of your RPCRequest.
//
// @see class:RPCRequest
// @see method:Callbacks.RPCCallback
// @visibility external
// @treeLocation Client Reference/RPC
//<
isc.ClassFactory.defineClass("RPCResponse");

isc.RPCResponse.addClassMethods({
    //> @classMethod RPCResponse.create()
    // RPCResponses shouldn't be created directly. Instances of this class are automatically
    // created and optionally passed to you in the callback you specify as part of your 
    // +link{RPCRequest}.
    // 
    // @visibility external
    //<
    // Log a warning if called directly
    create : function (A,B,C,D,E,F,G,H,I,J,K,L,M) {
        this.logWarn("RPCResponses shouldn't be created directly. Instances of this class " +
                     "are automatically created and optionally passed to you in the callback " +
                     "you specify as part of your RPCRequest.");
       
        return isc.addProperties({}, A,B,C,D,E,F,G,H,I,J,K,L,M);
    }
});

//> @attr rpcResponse.data (String | Object : false : R)
// The data sent by the server.
// <P>
// When communicating with the SmartClient server, rpcResponse.data is the data passed to the
// server-side method RPCResponse.setData() by your Java code.
// <smartclient>This data is translated into JavaScript
// objects by the rules described under +link{rpcRequest.data}.</smartclient>
// <smartgwt>This data is translated into JavaScript
// objects by the rules described under +link{rpcRequest.data}. Simple types (Numeric values,
// Strings, Dates, Booleans) will be available as their equivalent Java types in your
// client side GWT code. Complex objects (such as serialized Maps or Lists from the server)
// will not be automatically translated back into Java on the client - they will arrive as
// <code>JavaScriptObject</code> instances. You can easily convert
// to the appropriate type yourself using the <code>JSOHelper</code> class. The 
// +sgwtLink{JSOHelper.convertToJava()} method performs a recursive conversion of JavaScriptObjects
// returning a List (or array) for JavaScript arrays or a Map for simple
// JavaScript objects (key:value pairs).
// </smartgwt>
// <P>
// When not communicating with the SmartClient server rpcResponse.data contains the
// raw HTTP response body. See +link{rpcRequest.useSimpleHttp},
// +link{rpcRequest.serverOutputAsString}, +link{rpcRequest.evalResult} for details.
// @visibility external
//<



// Server->client conversion follows the this table as well, with some extras.  See the toJS()
// method on JSTranslater in the server documentation for a description of additional
// behaviors.
// <P>



//> @attr rpcResponse.status (int : 0 : IR)
// 
// Status code for this response.  Status codes less than zero are considered errors by the
// RPCManager, those greater than or equal to zero are considered successes.  Please see the
// error handling section the +link{class:RPCManager,RPCManager docs} for more information on
// what the RPCManager does with the status code and how you can override this behavior.
// <P>
// When using the SmartClient server you can set the rpcResponse.status by calling the
// server-side method RPCResponse.setStatus().  
// <P>
// When not using the SmartClient server, the RPCManager makes no assumptions about the
// structure of the response, so the status code just reflects the
// +link{attr:RPCResponse.httpResponseCode}: status will be 
// +link{RPCResponse.STATUS_TRANSPORT_ERROR,STATUS_TRANSPORT_ERROR} if an HTTP-level error
// occurred such as "500 server error".  If you have a status code you need to transmit you can
// simply embed it in the response (as part of +link{rpcResponse.data}) and interpret it from
// the callback.
// <P>
// With or without the SmartClient server, the +link{group:relogin} status codes (such as 
// +link{STATUS_LOGIN_REQUIRED}) are triggered whenever special markers, such as the
// loginRequiredMarker, appear in the body of the response.  See the +link{group:relogin,Relogin
// Overview} for details.
//
// @visibility external
//<

//> @attr rpcResponse.httpResponseCode (integer : null : R) 
//
// This attribute (available when using the the <code>xmlHttpRequest</code> transport) contains
// the HTTP response code sent by the server.
// <p>
// Note that this is different from +link{attr:RPCResponse.status} - that attribute is used to
// indicate a status code for the RPC itself whereas httpResponseCode is the raw HTTP response
// code for the HTTP request that contained the RPCRequest.
// <p>
// This feature relies on the XMLHttpRequest object which can be disabled by end-users in some
// supported browsers.  See +link{group:platformDependencies} for more information.
// <p>
// If you're using this attribute, you'll typically want to avoid the default error
// handling response of RPCManager.  To do so, set
// +link{attr:rpcRequest.willHandleError} to <code>true</code>.
//
// @visibility external
//<

//> @attr rpcResponse.httpHeaders (Object : null : R)
// HTTP headers returned by the server as a map from header name to header value.
// <P>
// Headers are available only when the default +link{RPCTransport} "xmlHttpRequest" is in use,
// and browsers may limit access to headers for cross-domain requests or in other
// security-sensitive scenarios.
//
// @visibility external
//<

//> @attr rpcResponse.httpResponseText (String : null : R)
// The actual text of the HTTP response.  Only available when the default 
// +link{RPCTransport} "xmlHttpRequest" transport is in use,
// @visibility external
//<

//> @attr rpcResponse.clientContext (Object : null : R)
//
// The +link{RPCRequest.clientContext} object as set on the +link{RPCRequest}.
//
// @see rpcRequest.clientContext
//
// @visibility external
//<

//> @attr rpcResponse.transactionNum (int : null : R)
// ID of the transaction sent to the server via +link{RPCManager.sendQueue()} containing the
// +link{RPCRequest} associated with this response.
// @visibility external
//<

//> @type RPCTransport
//
// SmartClient supports multiple RPC transports for maximum compatibility and feature richness.
// All of transports use HTTP as the underlying protocol, but use different mechanisms for
// sending the HTTP request and processing the response.  The transport is typically
// auto-selected for by based on the feature being used and the current browser settings.  For
// advanced use cases, +link{RPCRequest.transport} and +link{RPCManager.defaultTransport} are
// exposed as override points.
// <p>
// @value "xmlHttpRequest"  Uses the XMLHttpRequest object to make the request to the server.
// Note that in some browsers with certain configurations, this transport may not be
// available.  See +link{group:platformDependencies} for more information.  This transport is
// not useful with file uploads.  Cannot be used to target cross-domain URLs directly.
//
// @value "scriptInclude"   Write a SCRIPT tag into the DOM with a SRC attribute that targets
// an arbitrary URL.  This transport is the only one that allows direct cross-domain URL
// access.  
// <P>
// For +link{rpcRequest.callback} to work, the server being contacted must support the ability
// to generate JavaScript code in the response that will call a JavaScript function generated
// by SmartClient.  SmartClient passes the name of the function to call via a URL parameter,
// which can be controlled with +link{rpcRequest.callbackParam}.  This callback mechanism is
// sometimes called the "JSONP" (JSON with Padding) approach.
//
// @value "hiddenFrame"     Available with SmartClient Server only.  An HTML form is
// dynamically assembled that targets a hidden IFRAME.  This mechanism is supported on all
// browsers and cannot be disabled by end users.  
// <P>
// If using the SmartClient Server and using 
// +link{group:serverDataIntegration,Server-side data integration}, the "hiddenFrame" transport
// is automatically used for all RPCManager and DataSource requests if the "xmlHttpRequest"
// transport is not available.
// <P>
// Cannot be used to target cross-domain URLs directly.
//
//
// @visibility external
//<

//> @groupDef platformDependencies
//
// Client-side processing of web services, XML parsing, and some UI loading mechanisms rely on
// a native in-browser XML parser and/or the XMLHttpRequest object - one or both of which will
// not be available if the end user disables ActiveX support in Internet Explorer.  Note that
// these features do not require plugins or downloads of any kind - IE simply exposes certain
// built-in functionality like the XML parser and XMLHttpRequest through the ActiveX
// interface.  Disabling ActiveX also disables all browser plugins such as Flash, Java, SVG, etc.
// <p>
// Barring ActiveX being disabled, the XMLHttpRequest object is available to SmartClient on all
// supported browsers and an XML parser is available on all supported browsers except Safari
// versions prior to 3.0.3.
// <p>
// SmartClient client-server communication is not affected by the lack of an XML parser or the
// XMLHttpRequest object, but the <code>xmlHttpRequest</code> transport will not be available
// if the XMLHttpRequest object is not available.  Instead, the <code>hiddenFrame</code> or the
// <code>scriptInclude</code> transports are used for client-server communication.
// <p>
// <b><u>XML Parser</u></b>
// <p>
// If an XML Parser is not available to SmartClient, all client-side web service bindings and
// related methods will be unavailable.  Turning off ActiveX disables integration paths 2 and 3
// in the diagram below.  If you want to bind to web services and require deployment to IE
// without ActiveX (or you need to support Safari pre 3.0.3), you'll need to do all XML processing on the
// server and use either the SmartClient DSRequest or JSON operation pathways (integration
// paths 1 and 4 in the diagram below).  See the discussion in +link{clientServerIntegration}
// for more information on the integration paths shown in the diagram below.
// <p>
// You call +link{XMLTools.nativeXMLAvailable()} to check for the availability of a native XML
// parser at runtime.
// <p>
// <img src="${isc.DocViewer.instance.referenceRoot}skin/ds_bindings.png" width=763 height=475>
// <p>
// <b><u>XMLHttpRequest</u></b>
// <p>
// The XMLHttpRequest object is used for the <code>xmlHttpRequest</code> +link{RPCTransport}.
// Safari, Mozilla, Firefox, and IE 7 provide a native XMLHttpRequest implementation that is
// not affected by ActiveX being disabled (although the native IE 7 implementation can still be
// explicitly disabled by the end user).  IE 5.5 and IE 6.0 rely on the ActiveX bridge to
// support XMLHttpRequest, so if ActiveX is disabled in these browsers, XMLHttpRequest will not
// be available.
// <p>
// The lack of the XMLHttpRequest objects affects UI loading features like +link{ViewLoader},
// and +link{HTMLFlow} when used in remote loading mode (via +link{HTMLFlow.contentsURL},
// +link{HTMLFlow.setContentsURL}, but does not affect the typical client/server communication
// pathways (integration paths 1 and 5 in the diagram above).
// <p>
// Also affected are low level features +link{RPCRequest.serverOutputAsString},
// +link{RPCRequest.evalResult}, and +link{RPCResponse.httpResponseCode}.
// <p>
// In all of the above cases, it is possible to use the <code>hiddenFrame</code> transport to
// support these features when XMLHttpRequest is not available.  SmartClient will automatically
// send the request using the <code>hiddenFrame</code> transport when it detects that
// XMLHttpRequest is unavailable.  To support the above features, you'll need to use the
// RPCManager APIs on the server to send back the data that would normally be returned by
// XMLHttpRequest.  Since XMLHttpRequest cannot target URLs outside of the current domain, this
// strategy applies also to using the above features with cross-domain URLs.
// <p>
// You can call +link{RPCManager.xmlHttpRequestAvailable()} to check for the availability of
// XMLHttpRequest at runtime.
//
// @title Platform Dependencies
// @treeLocation /Client Reference/System
// @visibility external
//<

isc.RPCResponse.addClassProperties({
//> @groupDef  statusCodes
// Status codes returned by the server as rpcResponse.status.<br>
// See the error handling doc section in +link{class:RPCManager, RPCManager} for more 
// information on these codes
// @visibility external
//<

// NOTE: error codes are both added as a subobject (to allow code -> text name lookup) and
// directly (via addProperties below)
errorCodes : {


    //> @classAttr rpcResponse.STATUS_SUCCESS (int : 0 : R)
    //
    // Indicates successful completion of the request.  This is the default status and is
    // automatically used by the RPCResponse on the server unless you override it with
    // setStatus().
    // <br><br>
    // See the error handling section in +link{class:RPCManager, RPCManager documentation}
    // for more information.
    // 
    // @see class:RPCRequest
    // @group statusCodes, constant
    // @visibility external
    //<
    STATUS_SUCCESS: 0,

    //> @classAttr rpcResponse.STATUS_OFFLINE (int : 1 : R)
    //
    // Indicates that the browser is currently offline, and that we do not hold a cached 
    // response for the request.  
    // 
    // @see class:RPCRequest
    // @group statusCodes, constant, offlineGroup
    // @visibility external
    //<
    STATUS_OFFLINE: 1,

    //> @classAttr rpcResponse.STATUS_FAILURE (int : -1 : R)
    //
    // Indicates a generic failure on the server.  
    // See the error handling section in +link{class:RPCManager, RPCManager documentation}
    // for more information.
    // 
    // @see class:RPCRequest
    // @group statusCodes, constant
    // @visibility external
    //<
    STATUS_FAILURE: -1,

    //> @classAttr rpcResponse.STATUS_VALIDATION_ERROR (int : -4 : R)
    //
    // Indicates a validation failure on the server.
    // See the error handling section in +link{class:RPCManager, RPCManager documentation}
    // for more information.    
    // 
    // @see class:RPCRequest
    // @group statusCodes, constant
    // @visibility external
    //<
    STATUS_VALIDATION_ERROR: -4,

    //> @classAttr rpcResponse.STATUS_LOGIN_INCORRECT (int : -5 : R)
    //
    // Indicates that the RPC has been intercepted by an authenticator that requires the user
    // to log in.
    // 
    // @see class:RPCRequest
    // @group statusCodes, constant
    // @visibility external
    //<
    STATUS_LOGIN_INCORRECT: -5,

    //> @classAttr rpcResponse.STATUS_MAX_LOGIN_ATTEMPTS_EXCEEDED (int : -6 : R)
    //
    // Indicates that too many authentication attempts have been made and the server refuses to
    // accept any more login attempts.
    // 
    // @see class:RPCRequest
    // @group statusCodes, constant
    // @visibility external
    //<
    STATUS_MAX_LOGIN_ATTEMPTS_EXCEEDED: -6,

    //> @classAttr rpcResponse.STATUS_LOGIN_REQUIRED (int : -7 : R)
    //
    // Indicates that a login is required before this RPCRequest can proceed.
    // <P>
    // Applications do not directly set this status code, instead, to trigger the relogin flow,
    // return the loginRequiredMarker in the response sent by your server when login is
    // required.  See the +link{group:relogin,Relogin Overview} for details.
    // 
    // @see class:RPCRequest
    // @group statusCodes, constant
    // @visibility external
    //<
    STATUS_LOGIN_REQUIRED: -7,

    //> @classAttr rpcResponse.STATUS_LOGIN_SUCCESS (int : -8 : R)
    //
    // Indicates that the login succeeded.
    // 
    // @see class:RPCRequest
    // @group statusCodes, constant
    // @visibility external
    //<
    STATUS_LOGIN_SUCCESS: -8,

    //> @classAttr rpcResponse.STATUS_UPDATE_WITHOUT_PK_ERROR (int : -9 : R)
    //
    // Indicates that the client attempted an update or remove operation without providing 
    // primary key field(s)
    // 
    // @see class:RPCRequest
    // @group statusCodes, constant
    // @visibility external
    //<
    STATUS_UPDATE_WITHOUT_PK_ERROR: -9,

    //> @classAttr rpcResponse.STATUS_TRANSACTION_FAILED (int : -10 : R)
    //
    // Indicates that the request was either never attempted or was rolled back, because 
    // automatic or user transactions are in force and another request in the same transaction
    // failed.  Note that the request(s) that actually failed will have a code specific to the
    // failure; it is only the requests that would otherwise have succeeded that are marked 
    // with this failure code.
    // 
    // @see class:RPCRequest
    // @group statusCodes, constant
    // @visibility external
    //<
    STATUS_TRANSACTION_FAILED: -10,

	//> @classAttr rpcResponse.STATUS_MAX_FILE_SIZE_EXCEEDED (int : -11 : R)
    //
    // Indicates that uploaded file exceeded max file size allowed.
    // 
    // @see class:RPCRequest
    // @group statusCodes, constant
    // @visibility external
    //<
	STATUS_MAX_FILE_SIZE_EXCEEDED: -11,

	//> @classAttr rpcResponse.STATUS_MAX_POST_SIZE_EXCEEDED (int : -12 : R)
    //
    // Indicates that the total size of the data sent to the server was more than the server is
    // configured to allow.  Most servers limit the post size to prevent out of memory style
    // attack vectors that push a bunch of data at the server.  Apache Tomcat, for example,
    // is pre-configured to limit post size to 2mb.
    // <P>
    // On internal networks, these limits can typically be safely raised or removed.  With
    // Tomcat, for example, you can remove the post limit by specifying the following attribute
    // on the &lt;Connector&gt; element in conf/server.xml:
    // <br><pre>
    // maxPostSize="-1"
    // </pre>
    // <p>
    // <b>NOTE</b>: this status code is used whenever the server framework receives a request
    // where the POST data has been removed, however, there are other possible causes,
    // including:
    // <ul>
    // <li> security software installed on the server or network that erroneously detects some
    //      kind of exploit attempt, if its behavior is to just strip the POST data but allow
    //      the rest of the request through (SiteMinder is one product known to do this)
    // <li> incorrectly written filter servlets that drop POST'd data
    // </ul>
    // 
    // @see class:RPCRequest
    // @group statusCodes, constant
    // @visibility external
    //<
	STATUS_MAX_POST_SIZE_EXCEEDED: -12,
	
    //> @classAttr rpcResponse.STATUS_TRANSPORT_ERROR (int : -90 : R)
    //
    // This response code is usable only with the XMLHttpRequest transport and indicates that
    // the server returned an HTTP response code outside the range 200-299 (all of these statuses
	// indicate success, but ordinarily only 200 is used).  To get the actual
    // response code, you can query rpcResponse.httpResponseCode in your callback.
    // <p>
    // Note that currently this error code will never occur for the <code>hiddenFrame</code>
    // transport - instead, use +link{RPCResponse.STATUS_SERVER_TIMEOUT} to detect
    // <code>hiddenFrame</code> transport errors.
    //
    // @group statusCodes, constant
    // @visibility external
    //<
    STATUS_TRANSPORT_ERROR: -90,

    //> @classAttr rpcResponse.STATUS_UNKNOWN_HOST_ERROR (int : -91 : R)
    //
    // This response code only occurs when using the HTTP proxy.  It is issued by the proxy 
    // servlet when the target host is unknown (ie, cannot be resolved through DNS).  This
	// response probably indicates that you are attempting to contact a nonexistent server 
    // (though it might mean that you have DNS problems).
    //
    // @group statusCodes, constant
    // @visibility external
    //<
    STATUS_UNKNOWN_HOST_ERROR: -91,

    //> @classAttr rpcResponse.STATUS_CONNECTION_RESET_ERROR (int : -92 : R)
    // 
    // This response code only occurs when using the HTTP proxy.  It is issued by the proxy
    // servlet when the attempt to contact the target server results in a Java SocketException.
	// This response probably indicates that the target server is currently down.
    //
    // @group statusCodes, constant
    // @visibility external
    //<
    STATUS_CONNECTION_RESET_ERROR: -92,

    //> @classAttr rpcResponse.STATUS_SERVER_TIMEOUT (int : -100 : R)
    //
    // Indicates a request timed out with no server response.
    // <p>
    // This is a client-only error code - never sent by the server (since it's the server
    // that times out).
    // <p>
    // NOTE that if using <code>hiddenFrame</code> as the transport (not the default), a
    // malformed response such as a "500 Server Error" or 404 errors will be reported as a
    // timeout. 
    //
    // @group statusCodes, constant
    // @visibility external
    //<
    STATUS_SERVER_TIMEOUT: -100
}

});
isc.RPCResponse.addClassProperties(isc.RPCResponse.errorCodes);

// alias DSResponse to RPCResponse so that end users can use the response codes as
// e.g. DSResponse.STATUS_SUCCESS
isc.addGlobal("DSResponse", isc.RPCResponse);

isc.RPCManager.addClassProperties({
    // truncate warn() dialog with server error to this many chars
    maxErrorMessageLength: 1000,

    // truncate extra long RPCManager log messages unless RPCManagerResponse has been set
    maxLogMessageLength: 25000,

    //> @classAttr RPCManager.defaultTimeout (int : 240000 : RW)
    // In milliseconds, how long the RPCManager waits for an RPC request to complete before
    // returning an error.  
    // <p>
    // Default of 240000 milliseconds is four minutes.  If set to zero, the RPCManager will not
    // enforce a timeout, however, see +link{rpcRequest.timeout} for a discussion of default
    // timeouts that are built into browsers.
    // 
    // @visibility external
    //<
    defaultTimeout: 240000, 

    //> @groupDef i18nMessages
    // The properties in this group are default system messages that a localized 
    // application will want to override on a per-locale basis
    // @see group:i18n
    // @visibility external
    //<
    
    //> @classAttr RPCManager.defaultPrompt (HTMLString : "Contacting Server..." : IRW)
    //
    // If showPrompt is enabled for a given transaction, this is the defaultPrompt to be shown
    // to the user in a modal dialog while the transaction occurs.
    // May be overridden at the request level via +link{attr:rpcRequest.prompt}.<br>
    // More targetted default prompts are also supported for certain code-paths. See the following 
    // set of properties for details:<ul>
    // <li>+link{RPCManager.removeDataPrompt}</li>
    // <li>+link{RPCManager.saveDataPrompt}</li>
    // <li>+link{RPCManager.fetchDataPrompt}</li>
    // </ul>
    //
    // @see classAttr:RPCManager.showPrompt
    // @see classAttr:RPCManager.promptStyle
    // @see classAttr:RPCManager.promptCursor
    // @see attr:rpcRequest.showPrompt
    // @see attr:rpcRequest.prompt
    // @see attr:rpcRequest.promptStyle
    // @see attr:rpcRequest.promptCursor
    //
    // @visibility external
    // @group rpcPrompt
    // @group i18nMessages
    //<
    defaultPrompt:"Contacting server...",
    
    //> @classAttr RPCManager.timeoutErrorMessage   (HTMLString : "Operation timed out" : IRW)
    // Default message displayed to user when an operation fails to return from the server within
    // the timeout period specified by +link{RPCManager.defaultTimeout}.
    // @see classAttr:RPCManager.defaultTimeout
    // @visibility external
    // @group i18nMessages
    //<
    timeoutErrorMessage:"Operation timed out",
    
    //> @classAttr RPCManager.removeDataPrompt  (HTMLString : "Deleting Record(s)..." : IRW)
    // Default prompt displayed to user while an operation is running to remove data from
    // the server.<br>
    // Displayed as a result of the +link{ListGrid.removeSelectedData()} code path.
    // @visibility external
    // @group i18nMessages
    //<
    removeDataPrompt:"Deleting record(s)...",
    
    
    //> @classAttr  RPCManager.saveDataPrompt   (HTMLString : "Saving form..." : IRW)
    // Default prompt displayed to the user while an operation is running to save data to
    // the server.<br>
    // Displayed as a result of the +link{DynamicForm.saveData()} code path.
    // @visibility external
    // @group i18nMessages
    //<
    saveDataPrompt:"Saving form...",
    
    //> @classAttr  RPCManager.validateDataPrompt   (HTMLString : "Validating..." : IRW)
    // Default prompt displayed to the user while a server validation is pending.
    // @visibility external
    // @group i18nMessages
    //<
    validateDataPrompt:"Validating...",
    
    //> @type PromptStyle
    //
    // @value "dialog" Displays a centered modal prompt with text specified by
    //                 +link{rpcRequest.prompt}
    // @value "cursor" Changes the current cursor to the style specified by
    //                 +link{rpcRequest.promptCursor}
    //
    // @visibility external
    //<

    //> @classAttr  RPCManager.promptStyle   (PromptStyle : "dialog" : IRW)
    // Controls the default prompt style.  Overrideable by +link{attr:rpcRequest.promptStyle}.
    //
    // @visibility external
    // @see attr:rpcRequest.promptStyle
    // @group rpcPrompt
    //<
    promptStyle: isc.Dialog ? "dialog" : "cursor",

    //> @classAttr RPCManager.useCursorTracker (boolean : platform-dependent : IRW)
    //
    // If true, an image is shown to the right of the cursor when +link{rpcRequest.promptStyle} is
    // set to "cursor", otherwise the cursor itself is modified via css to the value of
    // +link{rpcRequest.promptCursor}.  The default is platform-dependent.  In IE 5.5 and
    // Firefox 1.0 the default is true, on all other platforms it is false.  The reason for
    // this split is that, the above browsers require that the cursor move before CSS settings are
    // re-evaluated - this means the progress cursor can stick until the user moves the mouse.
    // <p>
    // This value can be overridden on a per-request basis via +link{rpcRequest.useCursorTracker}.
    //
    // @see rpcRequest.useCursorTracker
    // @group rpcPrompt
    // @visibility external
    //<
    useCursorTracker: (isc.Browser.isMoz && isc.Browser.geckoVersion < 20051111),
    cursorTrackerConstructor: "Img",
    cursorTrackerDefaults: {
        src : "[SKINIMG]shared/progressCursorTracker.gif",
        size: 16,
        offsetX: 12,
        offsetY: 0,
        _updatePosition : function (init) {
            var left = (isc.EH.getX()+this.offsetX),
                top = (isc.EH.getY()+this.offsetY);

            // hide the cursorTracker when we hit the right or bottom edge of the browser so it
            // doesn't cause overflow and introduce scrolling
            if (left+this.size >= isc.Page.getWidth() || top+this.size >= isc.Page.getHeight()) {
                this.hide();
                return;
            }

            // we've seen cases where we can get non numeric values here - in this case
            // calling setLeft/top can mess up sizing of the wait img
            if (isNaN(left)) left = 0;
            if (isNaN(top)) top = 0;
            this.setLeft(left);
            this.setTop(top);
            if (!init && !this.isVisible()) this.show();
        },
        initWidget : function () {
            this.Super("initWidget", arguments);
            this._updatePosition(true);
            this._updateEvent = isc.Page.setEvent("mouseMove", this.getID()+"._updatePosition()");
            this._mouseOutEvent = isc.Page.setEvent("mouseOut", this.getID() + ".hide()");
            this.bringToFront();
        },
        destroy : function () {
            isc.Page.clearEvent("mouseMove", this._updateEvent);
            isc.Page.clearEvent("mouseOut", this._mouseOutEvent);
            this.Super("destroy", arguments);
        }
    },

    //> @classAttr  RPCManager.promptCursor   (String : browser-dependent : IRW)
    // Controls the default cursor shown when +link{rpcManager.promptStyle} is set to
    // <code>"cursor"</code>.  Overrideable by +link{attr:rpcRequest.promptCursor}.
    // <p>
    // In Safari, IE 5.5 and Firefox 1.0 the default value is "wait", on all other platforms it is
    // "progress".  The reason for this split is that the above-mentioned browsers do not support
    // CSS2.1 - which is required for the "progress" cursor type.
    //
    // @visibility external
    // @see attr:rpcRequest.promptCursor
    // @group rpcPrompt
    //<
    // FF1.0, Safari don't support CSS2.1, so must use 'wait' - otherwise 'progress'
    promptCursor: isc.Browser.isSafari || 
        (isc.Browser.isMoz && isc.Browser.geckoVersion < 20051111) || 
        (isc.Browser.isIE && isc.Browser.minorVersion <= 5.5) ? "wait" : "progress",

    //> @classAttr  RPCManager.fetchDataPrompt  (string : "Finding Records that match your criteria..." : IRW)
    // Default prompt displayed to the user while an operation is running to fetch data from
    // the server.<br>
    // Displayed as a result of +link{ListGrid.filterData()}, +link{ListGrid.fetchData()} and
    // +link{ListGrid.clearCriteria()} code paths.
    // @visibility external
    // @group i18nMessages
    //<
	fetchDataPrompt:"Finding records that match your criteria...",
    
    
    // Hidden prompts (not currently used by any exposed code path)
    // ---------------------------------------------------------------
    
    getViewRecordsPrompt:"Loading record...",
    	
    //> @classAttr RPCManager.showPrompt (boolean : RPCManager.showPrompt : RW)
    // If set to <code>true</code>, the RPCManager will block the UI with a modal dialog containing
    // the text from RPCManager.defaultPrompt (or the per-RPCRequest override) until the RPC to the
    // server completes.
    // <p>
    // If set to <code>false</code>, the RPC happens transparently, allowing the user to continue
    // interacting with the UI.
    // <p>
    // DataSource requests, which are a particular type of RPCRequest, are controlled by the
    // more-specific DataSource-level setting +link{DataSource.showPrompt}.
    //
    // @see classAttr:RPCManager.defaultPrompt
    // @see attr:rpcRequest.showPrompt
    // @group rpcPrompt
    // @visibility external
    //<
    showPrompt: false,

    //> @classAttr RPCManager.promptDelay (number : 300 : IRWA)
    //
    // If the request is configured to block user interactivity (+link{rpcRequest.showPrompt}), 
    // this property controls the delay in milliseconds before a visual indication is shown to the
    // user that interactivity is blocked.
    // <P>
    // Studies have shown that users will perceive a short operation as occurring faster if they
    // are not shown a wait cursor, throbber or other busy indicator, but that a busy indicator
    // <i>must</i> appear after a briefy delay or the user will perceive the system as broken or
    // hung.
    // <P>
    // Note that, regardless of this setting, interactivity is immediately blocked if showPrompt
    // is true, since the purpose of blocking is to prevent duplicate requests or prevent
    // interacting with components while they are in transition.  This setting controls only how
    // fast a visual indication of blocking is shown.
    //
    // @see classAttr:RPCManager.showPrompt
    // @see attr:rpcRequest.promptDelay
    // @group rpcPrompt
    // @visibility external
    //<
    promptDelay: 300,

    
    neverShowPrompt: false,

    //> @classAttr RPCManager.actionURL (URL : RPCManager.actionURL : RW)
    // Specifies the default URL for RPCRequests and DSRequests that do not specify a
    // URL.
    // <p>
    // URLs can be set on a per-request basis via +link{rpcRequest.actionURL}, or on a
    // per-DataSource or per-operationType basis via +link{DataSource.dataURL} and
    // +link{operationBinding.dataURL} respectively.  However, note that in order to be able to
    // make use of +link{RPCManager.startQueue,queuing}, you should have all data loading and
    // saving requests go to a single URL unless you are forced to use distinct URLs by legacy
    // services.
    // <p>
    // The primary use case for setting the default <code>actionURL</code> is to add a CSRF / XSRF 
    // (+externalLink{http://en.wikipedia.org/wiki/Cross-site_request_forgery,Cross-site Request Forgery}) 
    // token.  Assuming you are using a single URL for all data requests as covered above,
    // adding a CSRF token to the default <code>actionURL</code> as a simple HTTP parameter
    // will cause the CSRF token to be included in all RPCRequests and DSRequests from all
    // DataSources without further effort.
    // 
    // @visibility external
    //<
	actionURL:"[ISOMORPHIC]/IDACall",	
    
    //> @classAttr RPCManager.screenLoaderURL (URL : RPCManager.screenLoaderURL : RW)
    //
    // The screenLoaderURL specifies the URL where ScreenLoaderServlet is installed.
    // 
    // @visibility external
    //<
	screenLoaderURL:"[ISOMORPHIC]/screenLoader",	
    
    //> @classAttr RPCManager.ALL_GLOBALS (string : "-ALL_GLOBALS" : R)
    //
    // Passing this special value to +link{RPCManager.loadScreen,loadScreen} indicates that
    // all global names should be preserved when evaluating loaded screen.<br/>
    // 
    // @see RPCManager.loadScreen
    // @visibility external
    //<
    ALL_GLOBALS: "-ALL_GLOBALS",

    //> @classAttr RPCManager.useXmlHttpRequest (boolean : true if XMLHttpRequest is supported, false otherwise : RW)
    //  
    // Selects the default http transport for all RPC requests.  If set to true, RPCManager
    // will use XMLHttp for requests to the server.  If set to false, it will use hidden
    // frames.  Overrideable on a per-request basis via +link{RPCRequest.useXmlHttpRequest}.
    // <p>
    // Note that if the end user disables ActiveX controls in Internet Explorer, the XMLHttpRequest
    // object will not be available and SmartClient will automatically fall back on frames
    // communication.
    //
    // @deprecated As of SmartClient 5.5, use +link{RPCManager.defaultTransport}.  If you
    // specify a value for this property, it will take precedence over
    // +link{RPCManager.defaultTransport} for requests that do not specify a
    // +link{RPCRequest.transport} or +link{RPCRequest.useXmlHttpRequest}.
    //
    // @see attr:rpcRequest.useXmlHttpRequest
    //
    // @visibility external
    //<

    //> @classAttr RPCManager.defaultTransport (RPCTransport : "xmlHttpRequest": IRW)
    //
    // Selects the transport use for RPC requests by default.  You can override this setting on
    // a per-request basis by setting +link{rpcRequest.transport}.
    //
    // @see rpcRequest.transport
    // @visibility external
    //<
    defaultTransport: "xmlHttpRequest",

    //> @classAttr RPCManager.useHttpProxy (Boolean : true : IR)
    // Whether the +link{group:servletDetails,HttpProxyServlet} should be used in order to get
    // around the "same origin policy" that prevents web pages from contacting other sites.
    // <p>
    // Default behavior is to use the HttpProxyServlet whenever a URL appears to be pointing to
    // another site.  Set +link{rpcRequest.useHttpProxy} false to have a particular request
    // avoid using the HttpProxyServlet even when it appears to be necessary, or set
    // <code>RPCManager.useHttpProxy</code> to false to avoid ever attempting to use the
    // HttpProxyServlet.
    //
    // @visibility external
    //< 
    
	

    //> @classAttr RPCManager.httpProxyURL (string : "[ISOMORPHIC]/HttpProxy" : IR)
    // The URL to use for proxied requests.  This is a global system-wide setting.
    //
    // @see attr:rpcRequest.httpProxyURL
    // @visibility external
    //< 
    httpProxyURL: "[ISOMORPHIC]/HttpProxy",

    //> @classAttr RPCManager.dataEncoding (string : RPCManager.dataEncoding : RWA)
    //
    // Controls the encoding of the _transaction field of the client->server comm.  Valid values are
    // "XML" and "JS" and enable the XML and Javascript encoding, respectively.
    //
    // @visibility internal
    //<
    dataEncoding: "XML",

    //> @classAttr RPCManager.preserveTypes (boolean : RPCManager.preserveTypes : RWA)
    //
    // If true, numbers and booleans become Number and Boolean on the server.  Otherwise they are
    // Strings.  A true value is currently supported only with dataEncoding: XML.
    //
    // @visibility internal
    //<
    preserveTypes: true,

    //> @classAttr RPCManager.credentialsURL (string : RPCManager.credentialsURL : RWA)
    //
    // Specifies URL where credentials should be submitted to attempt relogin when session
    // timeout is encountered during a background RPC.  See +link{group:relogin,Relogin}
    //
    // @group relogin
    // @visibility external
    //<
    credentialsURL: isc.Page.getIsomorphicDir()+"login/loginSuccessMarker.html",

    // XXX document
    loginWindowSettings: "WIDTH=550,HEIGHT=250",

    // don't scan RPC responses longer than this for relogin commands: 1 Megabyte.  Scanning a
    // file this large for the relogin string on a Core 2 Duo 6700 CPU in IE takes 2.66ms
    maxLoginPageLength: 1048576,

    // outstanding transactions to the server and counter.  A transaction is a set of
    // RPCRequests sent to one URL 
    _transactions: [],

    _nextTransactionNum: 0,
    _activeTransactions:[],
    getTransactions : function () { return this._transactions; },
    getActiveTransactions : function () { return this._activeTransactions },
    pendingRpcs: 0
});


isc.RPCManager.addClassMethods({
    
    //> @classMethod RPCManager.queueSent()
    //
    // This method is called by the RPCManager every time it sends a queue of requests to the
    // server (note that if you are not using queuing, the system simply sends queues
    // containing just one request, so this API is valid regardless).<p>
    // There is no default implementation of this method; it is simply an override point.  
    // It is intended to be used by user code that needs to be notified when SmartClient sends 
    // requests to the server.  Note that the list of +link{class:RPCRequest}s passed to this
    // method is strictly <b>read-only</b>.
    //
    // @param requests (List of RPCRequest) The queue of +link{class:RPCRequest}s that was sent
    //
    // @visibility external
    //<
	
    //> @method Callbacks.QueueSentCallback
    // <p>This method is called by the RPCManager every time it sends a queue of requests to the
    // server (note that if you not using queuing, the system simply sends queues containing
    // just one request, so this API is valid regardless).
    // <p/>
    // It is intended to be used by user code that needs to be notified when SmartGWT sends
    // requests to the server.  Note that the list of {@link com.smartgwt.client.rpc.RPCRequest}'s passed to this
    // method is strictly <b>read-only</b>.
	//
	// @param response (Array of RPCRequest) array of requests sent
	//
	// @visibility external
	//<

    
    //> @classMethod RPCManager.xmlHttpRequestAvailable()
    //
    // Returns true if the XMLHttpRequest object is available, false otherwise.  See
    // +link{group:platformDependencies} for more information on when XMLHttpRequest parser may
    // not available and what features are
    // impacted as a result.
    //
    // @return (Boolean) true if XMLHttpRequest is available, false otherwise.
    //
    // @visibility external
    //<
    xmlHttpRequestAvailable : function () {
        // createXMLHttpRequest() actually does a new ActiveXObject() in IE, but
        // we don't cache the result because the user can change ActiveX settings on the fly.
        // This probably won't happen in actual usage, but developers will almost certainly try
        // it this way and it needs to work
        if (isc.Browser.isIE) return (isc.Comm.createXMLHttpRequest() != null);
        return true;
    },

	// return the action URL, dereferencing any special local directories.
    // used internally by the RPCManager for url interpolation
	getActionURL : function () { return isc.Page.getURL(this.actionURL); },

    //> @classMethod RPCManager.send()
    //
    // This method is a convenience wrapper on <code>RPCManager.sendRequest()</code> - it calls
    // through to sendRequest().
    //
    // @param data            (any)           data to be passed to the server
    // @param [callback]      (RPCCallback)   method to call on RPC completion
    // @param [requestParams] (Object)        object literal containing any additional properties
    //                                        you want to set - these will be applied to the
    //                                        RPCRequest object that will be auto-created for you.
    //
    // @see RPCManager.sendRequest()
    // @see class:RPCRequest
    //
    // @visibility external
    //<
    send : function (data, callback, requestParams) { 
        var rpcRequest = (requestParams || {});
        isc.addProperties(rpcRequest, {
            data: data,
            callback: callback
        });
        return this.sendRequest(rpcRequest);
    },


    _warnIfXmlHttpRequestUnavailable : function (featureName) {
        if (this.xmlHttpRequestAvailable() || !this.logIsWarnEnabled()) return false;
        
        var message = "Feature "+featureName+" requires the xmlHttpRequest transport"
                     +" which is not currently available because ActiveX is disabled."
                     +" Please see the 'Features requiring ActiveX or Native support'"
                     +" topic in the client-side reference under Client Reference/System"
                     +" for more information.";
        this.logWarn(message);
        return true;
    },

// sendProxied() : send an RPC through an HTTP Proxy
// ---------------------------------------------------------------------------------------
//> @classMethod RPCManager.sendProxied()
// Send an HTTP request to a remote host, potentially through the HttpProxy servlet installed
// on the SmartClient Server.
// <P>
// This API allows contacting services which are hosted on servers other than the origin server
// if the HttpProxy servlet is enabled on the SmartClient Server.
// <P>
// The HttpProxy will be used if the +link{rpcRequest.actionURL} starts with "http" and uses a
// hostname other than "localhost" or <code>window.location.hostname</code>, or if the port
// number differs, or if <code>request.useHttpProxy</code> is explicitly set.  Otherwise the
// request goes to the origin server (the server that returned the current page).
// <P>
// The +link{RPCRequest} properties that will be respected when relaying requests via the
// HttpProxy are: 
// +link{RPCRequest.actionURL,actionURL}, +link{RPCRequest.httpMethod,httpMethod}, 
// +link{RPCRequest.params,params}, +link{RPCRequest.contentType,contentType}, 
// +link{RPCRequest.httpHeaders,httpHeaders}, and +link{RPCRequest.data,data}.  In this case
// "data", if set, will be used as the request body for an HTTP POST.
// <P>
// Higher-level APIs like +link{DataSource} or +link{WebService} call through this API, and so
// automatically use the HttpProxy if +link{dataSource.dataURL} or
// +link{webService.setLocation(),webService.location} is set to a foreign server.
// <P>
// This API is only suitable for direct use when loading unstructured data that will not be
// shown in a +link{DataBoundComponent}.  For a WSDL-described web service, use
// +link{XMLTools.loadWSDL()} instead.  For other web services, use a +link{DataSource} with
// +link{DataSource.dataURL,dataURL}, and use +link{DataSource.transformRequest()} and
// +link{DataSource.transformResponse()} as necessary to form requests for the service and
// transform responses for display.
//
// @param request (RPCRequest Properties) rpcRequest to be routed through the HttpProxy
// @requiresModules SCServer
// @visibility external
//<
sendProxied : function (request, allowRPCFormat) {
    

    request.serverOutputAsString = request.transport != "scriptInclude";
    if (!request.isRestRequest) request.sendNoQueue = true; // don't use ISC-format multi-op

    var url = request.actionURL || isc.RPCManager.actionURL;
    //this.logWarn("url is: " + url);
    // use the proxy if request.useHttpProxy has been specifically set for this request, or..
    var useProxy = (request.useHttpProxy != null ? request.useHttpProxy :  
            // the proxy is available and ..
            (isc.RPCManager.useHttpProxy && 
                // and the URL appears to be remote (starts with http and not obviously local)
                url.startsWith("http") && !this.isLocalURL(url)));

	
	    if (!isc.RPCManager.allowCrossDomainCalls) {
            if (!useProxy && url.startsWith("http") && !this.isLocalURL(url)) {
                isc.warn("SmartClient can't directly contact URL '" + url + "' due to " +
                    "browser same-origin policy.  Remove the host and port number " +
                                    "(even if localhost) to avoid this problem, or use XJSONDataSource " +
                                    "for JSONP protocol (which allows cross-site calls), or use the " +
                                    "server-side HttpProxy included with SmartClient Server." +
                                    "<BR>" +
                                    "This warning may be suppressed by setting " +
                                    "<b>RPCManager.allowCrossDomainCalls</b> to true.");
            }
        }
	
	
    if (!useProxy)
    {
        // contact origin server directly, but don't send the ISC-specific transaction
        // structure        
        if (!allowRPCFormat) request.useSimpleHttp = true;
        
    } else {
        // prefer request-specific setting, back off to default
        //
        // backcompat: check for XMLTools.httpProxyURL - this is where we used to set this
        // default, but this clearly belongs on RPCManager (and is now publicly doc'd as such),
        // but for back-compat, still check and prefer the XMLTools setting (which does not
        // exist in framework anymore, but if user supplies it, we honor it)
        var proxyURL = request.httpProxyURL || isc.XMLTools.httpProxyURL || isc.RPCManager.httpProxyURL;

        // contact foreign server by way of HttpProxy, which expects an RPCRequest where "data"
        // contains configuration for sending an HTTPRequest
        request = isc.addProperties({}, request, {
            actionURL : proxyURL,
            // mark as proxied so we can perform better error reporting
            isProxied: true,
            // data is parameters the HttpProxy understands
            useSimpleHttp: true,
            proxiedURL: url,
            params : {
                data : isc.Comm.xmlSerialize("data", {
                    url : url,
                    httpMethod : request.httpMethod,
                    params : request.params,
                    contentType : request.contentType,
                    requestBody : request.data,
                    username : request.username,
                    password : request.password,
                    // NOTE: the only header the proxy actually supports sending through is
                    // "SOAPAction", but we send all headers to the server in case someone wants to
                    // customize this
                    httpHeaders : request.httpHeaders,
                    uploadFileName : request.uploadFileName,
                    // if we're using the scriptInclude transport, pass through the
                    // callbackParam to the proxy so that it can push this value through to the
                    // proxied target
                    callbackParam: request.transport == "scriptInclude" ? request.callbackParam : null
                })
            },
            // Force XHR on for this request because the proxy will feed us the response
            // directly and this is the only way to capture it without additional server
            // support.  Except, of course for the scriptInclude transport.
            transport: request.transport == "scriptInclude" ? "scriptInclude" : "xmlHttpRequest",
            // wipe out these properties since they apply only to the relayed request, not to
            // the request intended for the HttpProxy servlet.  NOTE: we leave httpHeaders
            // intact in case someone has some kind of advanced usage in mind; RPCManager
            // ignores all headers by default
            httpMethod: null, data: null, contentType:null
        });
        //this.logWarn("proxied request: " + this.echo(request) + 
        //             ", data for proxy: " + this.echo(request.data) +
        //             ", requestBody: " + this.echo(request.data.requestBody));
    }
    return isc.rpc.sendRequest(request);
},

//> @classAttr RPCManager.allowCrossDomainCalls (boolean : false : IRWA)
// By default SmartClient will show a warning message on attempted requests to another domain as
// this is usually not supported at the browser level by default due to 
// security considerations.
// <P>
// Some browsers now do support cross domain requests through the use of Http Access Control headers
// (See the +externalLink{http://www.w3.org/TR/cors/,W3C Cross-Origin Resource Sharing recommendation}).
// If your application intends to rely on this behavior to perform cross-domain requests, 
// you can set <code>allowCrossDomainCalls</code> to true to disable the standard SmartClient 
// warning when such calls occur.
// <P>
// Note also that this is typically not an issue if you are using the SmartClient server 
// (part of Pro, Power and Enterprise editions of SmartClient), as this includes the 
// +link{RPCManager.sendProxied,HTTPProxy servlet}.
// @visibility external
//<
allowCrossDomainCalls:false,

// given a URL, get the host without port
_getHostAndPort : function (url) {
    var protocol = isc.Page.getProtocol(url),
        // first slash after the protocol
        endHostSlash = url.indexOf("/", protocol.length),
        host = url.substring(protocol.length, endHostSlash),
        port
    ;
    var colIndex = host.indexOf(":");
    if (colIndex != -1) {
       port = host.substring(colIndex+1);
       host = host.substring(0, colIndex);
    }
       
    return [host, port];
},

// see if this is a URL that we can access locally
isLocalURL : function (url) {
    var hostAndPort = this._getHostAndPort(url),
        host = hostAndPort[0],
        port = hostAndPort[1]
    ;
    if (port == null || port == "") port = 80;
    
    // NOTE: bad case: might be accessing wrath.isomorphic.com as just "wrath", in
    // which case we can't detect that wrath.isomorphic.com is actually a local URL.
    // To make this check better we might need to actually attempt an XMLHttpRequest for the
    // ambiguous cases, catch the error, and cache the URL as known good or bad.  However
    // depending on security settings, attempting to access a foreign URL may launch a
    // confirmation dialog, so the best we can do is probably to try to detect whether the
    // HttpProxy servlet is installed (whether via a flag dumped by the loadISC tag or a
    // dynamic request to the server), and assume direct access if its missing
    var liveLocation = this.getWindow().location,
        liveHost = liveLocation.hostname,
        livePort = liveLocation.port;
    if (livePort == null || livePort == "") livePort = 80;
    
    return (host == "localhost" || host == liveHost)
           && port == livePort
    ;

    // Theoretically document.domain would allow xmlHttpRequests throughout a domain,
    // but quick testing reveals that setting document.domain to "isomorphic.com" on a page
    // served from "wrath.isomorphic.com" causes security exceptions in Moz even for attempts
    // to contact "wrath.isomorphic.com", the origin server.  IE also wedges.
    //return this._getHost(url).endsWith(this.getWindow().document.domain);
},

// ---------------------------------------------------------------------------------------
    

    //> @classMethod RPCManager.sendRequest()
    //
    // Send the passed <code>RPCRequest</code> to the server.  If queuing is in effect, this queues
    // the request instead.
    //
    // @param rpcRequest  (RPCRequest Properties)  RPCRequest to send to the server
    // 
    // @visibility external
    //<
    sendRequest : function (request) {
        // for Developer Consoel RPC->Call Stack view 
        if ((this._trackRPC || !this._initializedTrackRPC) && !request.doNotTrackRPC) {
            try {
                request._callStack = this.getStackTrace();
            } catch (e) {
                request._callStack = "N/A due to: " + e;
            }
        }
        // handle call to sendRequest with useHttpProxy explicitly set - you're really supposed
        // to call sendProxied() but this is a common mistake.  Checking for the isProxied flag
        // avoids a loop since sendProxied() calls sendRequest() after reformatting the
        // request.
        if (request.useHttpProxy && !request.isProxied) return this.sendProxied(request);

        // we are delaying transactions and this is a periodic polling or similar request that
        // can be ignored in this circumstance, so drop it
        if (request.canDropOnDelay && this.delayingTransactions) return;

        // make a copy of the request to allow callers to re-use the same object, with
        // modifications, when calling sendRequest() - note: shallow copy
        request = isc.addProperties({}, request);
        
        // default is true, so set it unless it's been explicitly disabled
        if (request.suppressAutoDraw !== false) request.suppressAutoDraw = true;

        // actionURL can also be specified as URL or url
        request.actionURL = 
                // NOTE use Page.getURL() to support special directories such as "[APPFILES]"
                isc.Page.getURL(request.actionURL || request.url || 
                                request.URL || this.getActionURL());


        // check if requested transport is available and fall back if necessary
        // ---------------------------------------------------------------------------------------
        
        
        // if the request specifies an explicit transport, we use that.  Otherwise, check
        // backcompat APIs first, then use RPCManager.defaultTransport    
        var explicitTransport = request.transport;
        if (!explicitTransport) {
            if (request.useXmlHttpRequest != null || this.useXmlHttpRequest != null) {
                // use of backcompat API on RPCRequest or RPCManager
                if (request.useXmlHttpRequest == null) {
                    if (this.useXmlHttpRequest != null) {
                        request.transport = this.useXmlHttpRequest ? "xmlHttpRequest" : "hiddenFrame";
                    } else {
                        request.transport = this.defaultTransport;
                    }
                } else {
                    request.transport = explicitTransport = request.useXmlHttpRequest ? "xmlHttpRequest" : "hiddenFrame";
                }
            } else {
                request.transport = this.defaultTransport;
            }
        }
        
        // Verify that the transport is available
        this.checkTransportAvailable(request, (explicitTransport != null));


        //>!BackCompat 2007.2.14 paramsOnly renamed to useSimpleHttp 
        if (request.useSimpleHttp == null) request.useSimpleHttp = request.paramsOnly;
        //<!BackCompat

    
        // default prompt-related settings on request to RPCManager defaults
        // ---------------------------------------------------------------------------------------
        isc.addDefaults(request, {
            showPrompt: this.showPrompt,
            promptStyle: this.promptStyle,
            promptCursor: this.promptCursor,
            useCursorTracker: this.useCursorTracker,
            cursorTrackerConstructor: this.cursorTrackerConstructor
        });

        request.cursorTrackerProperties = isc.addProperties({}, this.cursorTrackerDefaults, 
                                                            this.cursorTrackerProperties, 
                                                            request.cursorTrackerProperties);

        if (request.cursorTrackerProperties == null) 
            request.cursorTrackerProperties = this.cursorTrackerProperties;

        // if request.operation is supplied by callers doing DataSource operations.  If not
        // supplied, add it so we can assume its there for logging, etc.
        if (!request.operation) {
            request.operation = {
                ID: "custom",
                type: "rpc"
            }
        }

        var transformedData = this.transformRequest(request);
        // correct the common error of returning the rpcRequest itself incorrectly, which is
        // never right since the rpcRequest contains various widgets and other data
        // inappropriate to send to the server.
        if (transformedData !== request) {
            request.data = transformedData;
        }

        // support new RPCManager.requestsArePending() API
        this.pendingRpcs++;

        if (this.canQueueRequest(request, (explicitTransport!=null))) {
            if (!this.currentTransaction) this.currentTransaction = this._createTransaction(request.doNotTrackRPC);
            this._addRequestToTransaction(request, this.currentTransaction);
            // if we're not queuing, send it off
            if (!this.queuing) return this.sendQueue();
            return request;

        } else {
            return this.sendNoQueue(request);
        }
    },
    
    // verify that the specified request.transport is available
    checkTransportAvailable : function (request, isExplicit) {
    
        var xmlHttpRequestAvailable = this.xmlHttpRequestAvailable();
        
        var transport = request.transport || this.defaultTransport;
        if (!xmlHttpRequestAvailable) {
            if (transport == "xmlHttpRequest") {
                if (isExplicit) {
                    this.logWarn("RPC/DS request specifically requesting the xmlHttpRequest" +
                                " transport, but xmlHttpRequest not currently available -" +
                                " switching transport to hiddenFrame.");
                } else {
                    this.logWarn("RPCManager.defaultTransport specifies xmlHttpRequest, but"
                             +" xmlHttpRequest not currently available - switching transport " 
                             + "to hiddenFrame.");
                }
            }
            request.transport = "hiddenFrame";
        }
    },

    // determine whether a request can be queued
    canQueueRequest : function (request, transportIsExplicit) {
        // untracked RPCs are not queuable because the doNotTrack applies per-transaction, so
        // has to be only request in transaction
        if (request.doNotTrackRPC) return false;

        // since timeouts are controlled on a per-transaction basis, this type of request must be
        // sent separately of any queue
        if (request.ignoreTimeout) request.sendNoQueue = true;
        
        var transport = request.transport;
        

        // relogin flow: request is attempting login while transactions are suspended - bypass
        // queuing
        if (request.containsCredentials) {
            return false
        }

        // explicitly avoid any existing queue
        // scriptInclude-based requests can't be queued by nature
        if (request.sendNoQueue || request.transport == "scriptInclude") return false;

        // this tracks if we have at least one request on the queue - we can't use this.queuing here
        // because queuing can be disabled while there are still requests on the queue:
        // startQueue(false) - also for the checks below we really only care about requests that go
        // to the server, so we can ignore clientOnlyRequests
        var haveServerRequestsOnQueue = (this.currentTransaction && 
            this.currentTransaction.requestData.operations.length > 0);
            
        // - current request specifies URL other than that for which we're queueing
        //     - send errant request, continue queueing
        if (haveServerRequestsOnQueue && (request.actionURL != this.currentTransaction.URL)) { 
            //>DEBUG
            this.logWarn("RPCRequest specified (or defaulted to) URL: " + request.actionURL
                + " which is different than the URL for which the RPCManager is currently queuing: "
                + this.currentTransaction.URL + " - sending this request to server and continuing to queue");
            //<DEBUG
            return false;
        }

        // Since we parse responses in a single block we can't queue requests expected
        // to issue "strict" JSON responses with those expected to issue standard
        // "eval" type responses.
        if (haveServerRequestsOnQueue && 
            (!!request.useStrictJSON != !!this.currentTransaction.useStrictJSON)) 
        {
            this.logWarn("RPCManager - attempt to queue request specified 'useStrictJSON:" +
                        request.useStrictJSON + ". This conflicts with this setting for " +
                        "other queued requests - sending the request to server and " + 
                        "continuing to queue.");
            return false;
        }

        // - multiop with mixed xmlHttp/frames transports
        //      - send the offending current request, continue queuing
        if (haveServerRequestsOnQueue && 
            (this.currentTransaction.transport != request.transport)) 
        {
            //>DEBUG
            this.logWarn("RPCRequest with conflicting transport while queuing, sending request to"
                + " server and continuing to queue.");
            //<DEBUG
            return false;
        }
        
        // can queue
        return true;
    },

    // send a request immediately, bypassing the current queue.
    sendNoQueue : function (request) {
        var currentTransaction = this.currentTransaction;
        var queuing = this.queuing;
        this.currentTransaction = this._createTransaction(request.doNotTrackRPC);
        this._addRequestToTransaction(request, this.currentTransaction);
        var sendResult = this.sendQueue();
        this.queuing = queuing;
        this.currentTransaction = currentTransaction;
        return sendResult;
    },

    _createTransaction : function (doNotTrackRPC) {
        // create a new transaction
    	var transactionNum = this._nextTransactionNum++;

  	    var transaction = {
            timeout: this.defaultTimeout,
			transactionNum:transactionNum,
			// the request data and context for all operations in the transaction
			operations:[], 
            responses:[],
			// the actual data to go to the server
			requestData:{transactionNum:transactionNum, operations:[]},
            // prompt to show
            prompt: this.defaultPrompt,
            showPrompt: false,
            doNotTrackRPC: doNotTrackRPC,
            pushedToDebugMaster: false,
            changed : function () {       
                if (!this.doNotTrackRPC) isc.RPCManager.pushRPCUpdate(transaction);
            }
	    };
        // explicitly notify RPCTracker that this is the changed transaction
		this._transactions.add(transaction);
        transaction.changed();

        return transaction;
    },

    _addRequestToTransaction : function (request, transaction) {
        transaction.URL = request.actionURL;
                
        // mark transaction as a loginRPC if the request specifies it so we can quickly
        // determine this later (w/o scanning requests)
        if (request.containsCredentials) transaction.containsCredentials = true;

        if(!transaction.download_filename) transaction.download_filename = request.download_filename;

        if (((request.downloadResult || request.downloadToNewWindow) && request.download_filename )
                || request._returnStreamFileURL)
        {
            transaction.download_filename = request.download_filename;
            if (request.download_filename) {
                transaction.URL = this.addPathToURL(transaction.URL, 
                                      "/" + encodeURIComponent(request.download_filename));
            }

            // hitting 'cancel' on a download box can cause the server to stall and eventually time
            // out - ignore this error case
            transaction.ignoreError = true;
        } else if (request.exportFilename) {
            // for operations that result in downloaded file, make the browser give the
            // downloaded file a particular default name
            transaction.URL = this.addPathToURL(transaction.URL, "/" + encodeURIComponent(request.exportFilename));
        }


        // grab the prompt of the first operation to define one
		if (request.prompt && !transaction.customPromptIsSet) {
			//>DEBUG
            this.logDebug("Grabbed prompt from first request that defined one: " + request.prompt);
			//<DEBUG
			transaction.prompt = request.prompt;
            transaction.customPromptIsSet = true;
		}



        // set the showPrompt for the transaction and mark the request that forced the prompt so we
        // can hide it once we're done processing that request
        if (request.showPrompt && !transaction.showPrompt && !this.neverShowPrompt) {
            request.showedPrompt = true;
            isc.addProperties(transaction, {
                showPrompt: true,
                promptStyle: request.promptStyle,
                promptCursor: request.promptCursor,
                useCursorTracker: request.useCursorTracker,
                cursorTrackerConstructor: request.cursorTrackerConstructor,
                cursorTrackerProperties: request.cursorTrackerProperties
            });
        }

        if (request.isProxied) {
            isc.addProperties(transaction, {
                isProxied: true,
                proxiedURL: request.proxiedURL
            });
        }

        transaction.transport = request.transport;    
        transaction.useStrictJSON = request.useStrictJSON;
        
        // if any request in a transaction specifies ignoreReloginMarkers, then it applies to
        // the whole transaction since relogin marker processing happens on the whole
        // transaction response.
        if (request.ignoreReloginMarkers) transaction.ignoreReloginMarkers = true;

        transaction.operations.add(request);
    
        // add request.data to list of operations.
        // XXX null and empty strings become <elem></elem> on the server which means they can't be
        // disambiguated from one another.  Also <elem></elem> doesn't translate to an entry of any
        // kind in the operations list on the server (e.g. a list with just <elem></elem> is a zero
        // length list).  This means we have to encode these two values to reliably pick them up on
        // the server.
        var data = request.data;
        if (data == null) data = "__ISC_NULL__";
        else if (data === "") data = "__ISC_EMPTY_STRING__";

        // if this is a clientOnly request, no data will be sent to the server for it
        if (!request.clientOnly) transaction.requestData.operations.add(data);

        // omit nulls must be on for all requests in transaction for us to enable the flag
        if (transaction.omitNullMapValuesInResponse !== false && request.omitNullMapValuesInResponse != null) {
            transaction.omitNullMapValuesInResponse = 
                transaction.requestData.omitNullMapValuesInResponse = request.omitNullMapValuesInResponse;
        } else {
           transaction.omitNullMapValuesInResponse = false;
        }

        if (request.ignoreTimeout) transaction._clearOnTimeout = true;

        request.transactionNum = transaction.transactionNum;

        // XXX expand this?
        if (request.timeout || request.timeout === 0) transaction.timeout = request.timeout;

        transaction.changed();
    },

    //> @classMethod RPCManager.startQueue()    
    //
    // Start queuing requests.  When queuing requests, an HTTP request will not be sent to
    // the server until RPCManager.sendQueue() is called.
    // <p>
    // All requests in a given queue must go to the same actionURL and use the same transport
    // (XMLHttp or frames).  If a request specifies a different actionURL or transport than
    // that of the requests currently on the queue, it will be sent to the server separately,
    // ahead of the queue, and a warning will be logged to the Developer Console.
    // <p>
    // Note that the server must process all requests sent as part of the queue before any
    // response is sent to the client.  You should avoid batching a request that will take a
    // long time to process on the server with any other requests because it will delay the
    // response of all requests in the queue.
    //
    // @param [shouldQueue] (boolean) whether queuing should be enabled, default true.  Passing false
    //                      will disable queuing but not send the queue yet, so that any
    //                      queued requests will be sent along with the next
    //                      send()/sendRequest()
    //
	// @return	(boolean)	whether queuing was already enabled before we called
    //
    // @see RPCManager.sendQueue()
    //
    // @requiresModules SCServer
    // @visibility external
    //<       
	startQueue : function (shouldQueue) {
		var wasAlreadyQueuing = this.queuing;
		this.queuing = (shouldQueue == null ? true : shouldQueue);
		return wasAlreadyQueuing;
	},

    _promptCounter: 0,
    doShowPrompt : function (transaction, prompt, promptDelay) {
        if (this._promptCounter++ != 0) return;

        

        if (promptDelay && !this._showedMask) {
            // When the prompt, dialog or cursor, is delayed we show the click mask 
            // immediately to prevent any user interaction before the prompt is shown.
            // Note: EH scribbles on the unmaskedTargets array, so duplicate
            isc.EH.showClickMask(null, "hard", this.unmaskedTargets ? this.unmaskedTargets.duplicate() : null, "blockingRPC");
            this._showedMask = true;
        }
        
        var _this = this;
        var showPrompt = function () {
            if (transaction.promptStyle == "dialog" && prompt != null) {
                isc.showPrompt(prompt);
                _this._showedPrompt = true;
                // If we showed a click mask before the dialog was shown we can
                // remove it now. The modal dialog shows a mask as well - no
                // need to keep two masks in place.
                if (_this._showedMask) {
                    isc.EH.hideClickMask("blockingRPC");
                    _this._showedMask = null;
                }
            } else {
                // cursor
                if (!_this._showedMask) {
                    // Note: EH scribbles on the unmaskedTargets array, so duplicate
                    isc.EH.showClickMask(null, "hard", _this.unmaskedTargets ? _this.unmaskedTargets.duplicate() : null, "blockingRPC");
                    _this._showedMask = true;
                }
                if (transaction.useCursorTracker) {
                    _this._cursorTracker = isc.ClassFactory.getClass(transaction.cursorTrackerConstructor)
                        .create(transaction.cursorTrackerProperties);
                    _this._cursorTracker.show();
                } else {
                    isc.EH._screenSpan.setCursor(transaction.promptCursor);
                }
            }
        };

        if (promptDelay) {
            this._delayedShowPromptTimer = isc.Timer.setTimeout(showPrompt, promptDelay);
        } else {
            showPrompt();
        }
    },

    // cursors: http://www.gtalbot.org/DHTMLSection/Cursors.html
    doClearPrompt : function (transaction) {
    	// Cancel delay timer
        if (this._delayedShowPromptTimer) {
            isc.Timer.clear(this._delayedShowPromptTimer);
            this._delayedShowPromptTimer = null;
        }

        if (transaction.clearedPrompt) return;
        transaction.clearedPrompt = true;

        if (--this._promptCounter != 0) {
            // safety net - whouldn't need to reset this, but we check for zero explicitly in
            // doShowPrompt
            if (this._promptCounter < 0 ) this._promptCounter = 0;
            return;
        }

        if (this._showedPrompt) {
            isc.clearPrompt();
        } else {
            if (this._cursorTracker) {
                this._cursorTracker.destroy();
                this._cursorTracker = null;
            } else {
                if (isc.EH._screenSpan) isc.EH._screenSpan.setCursor(isc.Canvas.DEFAULT);
            }
        }
        this._showedPrompt = null;

        if (this._showedMask) {
            isc.EH.hideClickMask("blockingRPC");
            this._showedMask = null;
        }
    },

    //> @classMethod RPCManager.getCurrentTransactionId()
    // Same as getQueueTransactionId().
    // 
    // @return (int) the transactionNum of the current transaction.
    // @visibility external
    //<
    getCurrentTransactionId : function () {
        return this.currentTransaction ? this.currentTransaction.transactionNum : null;
    },

    //> @classMethod RPCManager.getQueueTransactionId()
    // Returns the id of the current transaction (a queue of requests).
    // <P>
    // This method must be called after startQueue() has been called and
    // at least one request has been issued.
    // <P>
    // NOTE: This method will return null if there's no queued requests.
    // 
    // @return (Integer) the transactionNum of the current transaction.
    // @visibility external
    //<
    getQueueTransactionId : function () {
        return this.currentTransaction ? this.currentTransaction.transactionNum : null;
    },
    
    //> @classMethod RPCManager.cancelQueue()
    // Cancel a queue of requests (also called a transaction).
    // <P>
    // If a transactionId is passed, that transaction will be cancelled, otherwise, the current 
    // (not yet sent) transaction is cancelled.  You can retrieve the id of the current 
    // transaction, if there is one, by calling 
    // +link{RPCManager.getQueueTransactionId(), getQueueTransactionId()} before the
    // transaction has been sent.
    // <P>
    // Note that cancelQueue() calls +link{RPCManager.clearTransaction(), clearTransaction()}
    // and attempts to abort the request.  However, note also that whilst cancelling a 
    // transaction that has already been sent will not necessarily stop the HTTP request that 
    // has been issued - this is only possible on some browsers and with some transports - it 
    // will reliably cause SmartClient to ignore any response returned by the server and not 
    // fire any callbacks that have been passed in.
    // 
    // @param [transactionNum] (int) transactionId of the queue.
    // @visibility external
    //<
    cancelQueue : function (transactionNum) {
        if (transactionNum == null) {             
            // cancel the current transaction
            transactionNum = this.currentTransaction;
        }
        var transaction = this.getTransaction(transactionNum);
        if (transaction == null) return;        
        else if (transaction == this.currentTransaction) this.currentTransaction = null; // clear if current

        // clear the prompt if any of our operations showed it.
        if (transaction.showPrompt) this.doClearPrompt(transaction);

        // If an abort function is present on the transportRequest object
        // returned from the transport function, fire it to kill the transport
        // Note that this is not guaranteed to be present - depends on the implementation of
        // the transport function called by sendQueue
        if (transaction.transportRequest && transaction.transportRequest.abort) {
            transaction.transportRequest.abort();
        }
        transaction.cancelled = true;

        this.clearTransaction(transaction.transactionNum);
    },

    // Returns the transaction object for a transasctionNum.  If passed a transaction object,
    // returns that.  Can also take a native window object representing a HiddenFrame and
    // return the transaction occurring in that frame.
    getTransaction : function (transaction) {
        if (transaction == null) return null;

        // window object
        if (transaction.location && transaction.document) {
            var win = transaction;
            // no transactionNum in the URL - this is probably because the server send an
            // HTTP redirect to a login URL instead of just feeding the login page contents
            // in response to our RPC.
            var hiddenFrames = isc.HiddenFrame._hiddenFrames;
            for (var i = 0; i < hiddenFrames.length; i++) {
                if (win == hiddenFrames[i].getHandle()) {
                    transaction = hiddenFrames[i].transactionNum;
                    break;
                }
            }   
            // still haven't found it
            if (transaction == win) {
                this.logDebug("Can't find transactionNum in getTransaction from iframe");
                return null;
            }
        }

        // transaction id passed
        //
        // NOTE: this has to be last, because the code above that finds the transaction via the
        // window object depends on leaving transaction as a Number when it's done
        if (isc.isA.Number(transaction) || isc.isA.String(transaction)) {
            transaction = this._transactions.find({transactionNum: transaction});
        }

        // NOTE: when trackRPCs mode is activated in the Developer Console, we retain
        // transactions that have been cleared.  But these should not be returned.
        if (transaction && transaction.cleared) return null;

        return transaction;        
    },
    
    getCurrentTransaction : function () {
        return this.currentTransaction;
    },

    getLastSubmittedTransaction : function () {
        return this._transactions[this._transactions.length-1];
    },

    //> @classMethod RPCManager.clearTransaction() [A]
    // Erase all client-side record of a transaction, such that any response from the server
    // will be ignored.
    // <P>
    // A transaction means a batch of one or more RPCRequests that have already been sent to
    // the server via +link{RPCManager.sendQueue()}.
    // <P>
    // You can retrieve the id of the current transaction, if there is one, by 
    // +link{RPCManager.getQueueTransactionId(), getQueueTransactionId()} before the 
    // transaction is sent.
    // 
    // @param transactionNum (int) id of the transaction to be cleared
    // @see group:relogin
    // @visibility external
    //<
    clearTransaction : function (transactionNum) {
        var transaction = this.getTransaction(transactionNum);
        if (transaction == null) {
            this.logWarn("clearTransaction: no such transaction: " + this.echo(transactionNum));
            return;
        }
        this.clearTransactionTimeout(transaction);
        

        // Don't remove RPCs from the transaction queue until we've had a chance to read the
        // log cookie that tells us whether the user wants to track them or not
        //
        // Have to wait until page load to read cookies
        if (!this._initializedTrackRPC && isc.Page.isLoaded()) {
            var globalLogCookie = isc.LogViewer.getGlobalLogCookie();   
            this.setTrackRPC(globalLogCookie ? globalLogCookie.trackRPC : false);
            this._initializedTrackRPC = true;
        }

        // flag transaction as cleared, if not tracking, remove from transactions queue
        transaction.cleared = true;
        if (!this._trackRPC) this._transactions.remove(transaction);
        else transaction.changed();

        isc.RPCManager._activeTransactions.remove(transaction.transactionNum);
    },

    // internal method to flip the _trackRPC bit.  If tracking is disabled, clear any
    // transactions from the transactions queue that have completed (were marked as cleared)
    setTrackRPC : function (track) {
        this._trackRPC = track;
        // store cookie
        isc.LogViewer.setGlobalLogCookieValue("trackRPC", track);

        var transactions = this.getTransactions();
        if (!track) this.removeClearedRPC();
    },
    pushRPCUpdate : function (transaction) {
        if (!this._trackRPC || transaction.doNotTrackRPC) return;
        if (isc.debugTarget) isc.debugTarget.pushRPCUpdate(transaction);
        // once we push a cleared transaction to the debugMaster, remove it
        if (transaction.pushedToDebugMaster && transaction.cleared) this._transactions.remove(transaction);
    },
    pushBufferedTransactionsToDebugMaster : function () {
        var bufferedTransactions = this._transactions.findAll("pushedToDebugMaster", false);
        if (!bufferedTransactions) return;

        for (var i = 0; i < bufferedTransactions.length; i++) {
            this.pushRPCUpdate(bufferedTransactions[i]);
        }
    },  

    removeClearedRPC : function () {
        var cleared = this._transactions.findAll("cleared", true);
        if (cleared) this._transactions.removeList(cleared);
    },

    // Usually called by server to block new transactions and delay outstanding ones from timing out
    // while waiting for the user to do something - e.g. complete reauthentication.  

    //> @classMethod RPCManager.delayAllPendingTransactions()
    // 
    // Clears the timeouts for all currently pending RPCRequests/DSRequests and forces any
    // future RPCRequests/DSRequests into an internal queue that doesn't go to the server until
    // you call +link{RPCManager.resendTransaction()}.
    // 
    // @see RPCManager.resendTransaction()
    //<     
    delayAllPendingTransactions : function () {

        // set a global flag so any newly created transactions don't get sent to the server
        this.delayingTransactions = true;

        // clear the timeouts for outstanding transactions
        for (var i = 0; i < this._transactions.length; i++) {
            var transaction = this._transactions[i];
            this.delayTransaction(transaction);
        }
    },

    //> @classMethod RPCManager.suspendTransaction() [A]
    // Suspends the current transaction, such that all processing of the transaction is halted,
    // any remaining +link{rpcRequest.callback,callbacks} in the transaction won't fire, and
    // the transaction can never +link{rpcRequest.timeout,timeout}.
    // <P>
    // <code>suspendTransaction()</code> is typically used to handle total failures for an
    // entire transaction, such as HTTP status 500, or session timeout resulting in
    // +link{RPCManager.loginRequired,loginRequired()} being called.  In both cases the intent
    // is to put the transaction on hold so that a transient problem can be resolved, and then
    // the transaction can be re-sent successfully.  By using suspendTransaction(), components
    // that submitted requests never realize there was a transient failure, and so error
    // handling logic does not have to be implemented in every component.
    // <P>
    // Generally you can only validly suspend a transaction from either
    // +link{RPCManager.loginRequired()} or +link{RPCManager.handleError()}, and in the case of
    // handleError(), only when the first response in the transaction has an error.  Suspending
    // and re-sending a partially processed transaction means that some responses will be
    // processed twice, with undefined results for requests issued automatically by UI
    // components.
    // <P>
    // A suspended transaction must ultimately be either cleared via
    // +link{clearTransaction()} or re-sent via +link{resendTransaction()} or memory will be
    // leaked.
    //
    // @param [transaction] (transaction Obj or ID) transaction to delay.  Defaults to the
    //                      current transaction if there is one
    // @see RPCManager.resendTransaction()
    // @visibility external 
    //<     
    
    suspendTransaction : function (transactionNum) {
        var transaction = this.getTransaction(transactionNum) ||
                            this.getCurrentTransaction();
        if (transaction == null) {
            this.logWarn("No transaction to suspend");
            return;
        }
        if (transaction.suspended) return;

        transaction.suspended = true;
        
        // If we're currently in a thread kicked off from a transaction response, set
        // an additional flag to suppress subsequent callbacks in the same thread, even if 
        // the transaction is no longer marked as suspended (has been re-submitted)
        if (transaction._handlingResponse) transaction.abortCallbacks = true;
        
        this.clearTransactionTimeout(transaction);        
        if (transaction.showPrompt) this.doClearPrompt(transaction);

        transaction.changed();
    },

    // this is called by the internal RPC page load defer mechanism
    delayTransaction : function (transaction) {
        transaction = this.getTransaction(transaction);
        if (transaction.delayed) return;
        transaction.delayed = true;
        this.clearTransactionTimeout(transaction);

        transaction.changed();
    },
    

    //>Offline
    onLine: !isc.isOffline(),

    goOffline : function () {
        this.logInfo("Going offline...");
        this.onLine = false;
    },
    goOnline : function () {
        this.logInfo("Going online...");
        // replay transactions
        this.offlinePlayback = true;
        this.playbackNextOfflineTransaction();
    },

    // observable
    offlineTransactionPlaybackComplete : function () { },

    playbackNextOfflineTransaction : function () {
        var transaction = this.offlineTransactionLog ? this.offlineTransactionLog.removeAt(0) : null;
        if (transaction == null) {
            this.logInfo("Offline transaction playback complete");
            // no more to playback, go online if the browser is online
            this.offlinePlayback = false;
            this.onLine = !isc.isOffline();            
            this.offlineTransactionPlaybackComplete();
            return;
        }

        // play it back
        // We'll get called back by performTransactionReply()
        this.resubmitTransaction(transaction);  
    },

    offlineTransaction : function (transaction) {
        if (transaction.offline) return;
        transaction = this.getTransaction(transaction);
        transaction.offline = true;
        
        this.clearTransactionTimeout(transaction);

        if (!this.offlineTransactionLog) {
            this.offlineTransactionLog = [];
            // keep it sorted by timestamp.  this is important to keep transactions that are
            // added via transactionTimeout(0 in order
            this.offlineTransactionLog.sortByProperty("timestamp", Array.ASCENDING);
        }
        this.offlineTransactionLog.add(transaction);

        transaction.changed();

        // fire fake reply callbacks
        var requests = transaction.operations;
        for (var i = 0; i < requests.length; i++) {
            var request = requests[i];
            var response = this.createRPCResponse(transaction, request, {
                httpResponseCode: 200,
                offlineResponse: true
            });

            this.delayCall("fireReplyCallbacks", [request, response], 0);
        }
    },
    //<Offline

    // if we're delaying transactions, this resubmits outstanding transactions and allows new ones
    // to go to the server.

    //> @classMethod RPCManager.resendTransaction() [A]
    // Resend a suspended transaction to the server.  See +link{suspendTransaction()} for
    // context.  
    // <P>
    // Note that the transaction must have been previously suspended, and in particular
    // suspended validly according to the rules described in the docs for
    // +link{suspendTransaction()}, or undefined results will occur.
    // <P>
    // You can resend <b>all</b> suspended transactions by calling
    // +link{resendTransaction()} with no arguments.
    //
    // @param [transactionNum] (int) id of the transaction to be re-sent, or null to resend all
    //                              suspended transactions
    // @see group:relogin
    // @visibility external
    //<     
    resendTransaction : function (transaction) {
        this.resendTransactionsFlagged(transaction, "suspended");
    },

    // this is called by the internal RPC page load defer mechanism
    resendDelayedTransactions : function () { 
        this.delayingTransactions = false;
        this.resendTransactionsFlagged(null, "delayed");
    },
    resendTransactionsFlagged : function (transaction, flag) {
        // resend either just the transaction passed in or all intercepted transactions (no
        // args)
        var transactions = transaction ? [this.getTransaction(transaction)] : this._transactions;

        for (var i = 0; i < transactions.length; i++) {
            transaction = transactions[i];
            if (transaction[flag]) {
                delete transaction[flag];
                this.resubmitTransaction(transaction);
            }
        }
    },

    // returns all rpcRequests for a given operation
    //
    // This may be needed for some very advanced cases of relogin or other queuing and
    // cancelling logic.
    getTransactionRequests : function (transaction) {
        return this.getTransaction(transaction).operations;
    },

    _setTransactionTimeoutTimer : function (transaction) {
        transaction = this.getTransaction(transaction);

        var timeout = transaction.timeout;
        if(!timeout && timeout !== 0) timeout = this.defaultTimeout;
        if(timeout == 0) return;

        transaction.timeoutTimer = 
            isc.Timer.setTimeout("isc.RPCManager._timeoutTransaction(" 
                                 + transaction.transactionNum + ")", timeout);
    },
    
    clearTransactionTimeout : function (transaction) {
        transaction = this.getTransaction(transaction) || 
                      this.getCurrentTransaction() ||
                      this.getLastSubmittedTransaction();
        if(!transaction) return;

        isc.Timer.clear(transaction.timeoutTimer);
    },

    _timeoutTransaction : function (transaction) {
        transaction = this.getTransaction(transaction);
        
        if (transaction._clearOnTimeout) {
            
            this.clearTransaction(transaction);
            return;
        }

        //>Offline
        if (!this.onLine) {
            // we went offline after the transaction was sent - consider this transaction
            // offline instead of returning an error
            this.offlineTransaction(transaction);
            return;
        }
        //<Offline

        transaction.results = this._makeErrorResults(transaction, {
             data: isc.RPCManager.timeoutErrorMessage,
             status: isc.RPCResponse.STATUS_SERVER_TIMEOUT
        });
        this._performTransactionReply(transaction.transactionNum)
    },

    _makeErrorResults : function (transaction, result) {
        var results = [];
        for(var i = 0; i < transaction.operations.length; i++) {
            if (transaction.operations[i].dataFormat == "xml") {
                results[i] = isc.Comm.xmlSerialize("response", result);
            } else {
                results[i] = {response:isc.clone(result)};
            }
        }
        return results;
    },

    // resubmit a transaction to the server.  Used to proceed after a transient or temporary
    // error, like being unable to contact the server or needing to authenticate again because
    // your session timed out.
    resubmitTransaction : function (transaction) {
        transaction = this.getTransaction(transaction) || 
                      this.getLastSubmittedTransaction();

        // wipe out stored status (eg transport error)
        transaction.status = null;

        // could be queuing a new transaction - save it off
        var queuingTransaction = this.currentTransaction;
    
        // sendQueue() operates on this.currentTransaction, so set it to the resubmitTransaction
        this.currentTransaction = transaction;

        if (transaction != null) {
            //>DEBUG
            this.logInfo("Resubmitting transaction number: " + transaction.transactionNum);
            //<DEBUG

            // if there is an operation window open from the transaction being
            // resubmitted, get rid of it
            
            
            
            delete transaction.suspended;
            delete transaction.clearedPrompt;  

            // if this transaction was deferred because it was sent before page load,
            // re-instate the arguments originally passed to sendQueue() if any
            var args = transaction._args || isc.emptyObject;
            this.sendQueue(args.callback, args.prompt, args.URL);
        //>DEBUG
        } else {
            this.logWarn("No transaction to resubmit: transaction number " 
                         + transaction + " does not exist");
        //<DEBUG 
        }
        // reinstate the original currentTransaction
        this.currentTransaction = queuingTransaction;
    }, 

    // called by the SmartClient server to work around an IE issue specific to a single service
    // pack, where the browser occasionally submits an empty form
    retryOperation : function (commFrameID) {
        this.logDebug("Server-initiated operation retry for commFrameID: " + commFrameID);
        var commFrame = window[commFrameID];
        if(!commFrame) {
            this.logError("comm operation retry failed - can't locate object: " + commFrameID);
            return;
        }
        commFrame.sendData(true);
    },

    transactionAsGetRequest : function (transaction, baseURL, params) {
        if (!transaction.cleared) transaction = this.getTransaction(transaction) || this.getCurrentTransaction();
        baseURL = (baseURL || transaction.URL || this.getActionURL());
        if(!params) params = {};
        params._transaction = this.serializeTransaction(transaction);
        
        return this.addParamsToURL(this.markURLAsRPC(baseURL), params);
    },

    // encode a parameter for use in a URL query string or form-style HTTP POST.
    // What to do with non-String param values:
    // While Strings, Numbers, and Booleans can be sent just as an HTML form sends
    // them, we're not in a context here to know what the server expects for Dates or
    // other types - if a particular caller wants a specific type of serialization that
    // caller needs to serialize the data before it gets to this layer.
    // NOTE: Dates: JSON.encode() would currently return "new Date(.." for a Date;
    // using the XML Schema format is a better default.  Note also that we support a flag
    // on Dates "logicalDate" that causes it to be serialized showing just the date with no
    // time values, or "logicalTime" to serialize just the time value (not the date).
    encodeParameter : function (paramName, paramValue) {
        if (isc.isA.Date(paramValue)) {
            isc.Comm.xmlSchemaMode = true;
            paramValue = paramValue.toSchemaDate();
            isc.Comm.xmlSchemaMode = null;
        } else if (isc.isA.Array(paramValue)) {
            // for Array values, list the same parameter multiple times.  This
            // matches what HTML forms do for multiple selects
            var output = isc.SB.create();
            for (var i = 0; i < paramValue.length; i++) {
               output.append(this.encodeParameter(paramName, paramValue[i]));
               if (i < paramValue.length-1) output.append("&");
            }
            return output.release(false);
        } if (!isc.isA.String(paramValue)) {
            paramValue = isc.JSON.encode(paramValue, {prettyPrint:false});
        }
        return isc.SB.concat(encodeURIComponent(paramName), "=",
                             encodeURIComponent(paramValue));
    },

    addParamsToURL : function (baseURL, params) {
        var result = baseURL;
        if (!params) return baseURL;
        for (var paramName in params) {
            var paramValue = params[paramName];

            result += result.contains("?") ? "&" : "?";
            result += this.encodeParameter(paramName, paramValue);
        }
        return result;
    },

    addPathToURL : function (baseURL, path) {
        var newURL;

        var splitIndex = baseURL.indexOf("?");
        if (splitIndex == -1) {
            newURL = baseURL+path;
        } else {
            newURL = baseURL.substring(0, splitIndex)+path+baseURL.substring(splitIndex);
        }

        return newURL;
    },  

	// based on the dataEncoding setting returns the string-serialized version of the
	// requestData member of the passed in transaction object.
	serializeTransaction : function (transaction) {
        var result;

        // If we're in SGWT the 'data' for the transaction may include native
        // Java objects assigned by a call to JSOHelper.setAttribute or
        // JSOHelper.setAttributeAsJavaObject.
        // We have no way to serialize such an object for transmission to the server,
        // so throw an exception in the serialize logic if such an object is
        // encountered.
        
        if (isc.Browser.isSGWT) {
            window.SmartGWT.warnOnSerializeError = true;
            window.SmartGWT.serializeErrorMessage = 
                "Object is part of a request being serialized for " +
                "transmission to the server. See SmartClient documentation of " +
                "RPCRequest.data for a table of Java types that can be converted automatically.";
        }                
        if (this.dataEncoding == "JS") {
            
            isc.Comm._legacyJSMode = true;
            if (isc.isAn.Array(transaction.operations)) {
                var ds = isc.DataSource.get(transaction.operations[0].dataSource);
                if (ds) {
                    isc.Comm._trimMillis = !!ds.trimMilliseconds;
                }
            }
            result = isc.Comm.serialize(transaction.requestData);
            isc.Comm._trimMillis = null;
            isc.Comm._legacyJSMode = null;
        } else {
            isc.Comm._explicitNils = true;
            if (isc.isAn.Array(transaction.operations)) {
                // NOTE: this implies that you can only have one trimMilliseconds setting per
                // queue - but that's OK because we only anticipate the need for different 
                // millisecond settings if you are going to different servers
                var ds = isc.DataSource.get(transaction.operations[0].dataSource);
                if (ds) {
                    isc.Comm._trimMillis = !!ds.trimMilliseconds;
                }
            }
            result = isc.Comm.xmlSerialize("transaction", transaction.requestData);
            isc.Comm._trimMillis = null;
            isc.Comm._explicitNils = null;
        }
        if (isc.Browser.isSGWT) {
            window.SmartGWT.warnOnSerializeError = false;
            window.SmartGWT.serializeErrorMessage = null;
        }                
        //this.logWarn("serialized transaction: " + result);
        return result;
	},

    
    markURLAsRPC : function (URL) {
        if(!URL.contains("isc_rpc=")) URL += (URL.contains("?") ? "&" : "?")  
                                                    + "isc_rpc=1&isc_v="+isc.versionNumber;
        return URL;
    },
    
    markURLAsXmlHttp : function (URL) {
        if(!URL.contains("isc_xhr=")) URL += (URL.contains("?") ? "&" : "?")  + "isc_xhr=1";
        return URL;
    },

    addDocumentDomain : function (URL) {
        if(!URL.contains("isc_dd=")) URL += (URL.contains("?") ? "&" : "?")  + "isc_dd="+document.domain;
        return URL;
    },
    
    registerUnmaskedTarget : function (unmaskedTarget) {
        if (!this.unmaskedTargets) this.unmaskedTargets = [];
        this.unmaskedTargets.add(unmaskedTarget);
    },
    unregisterUnmaskedTarget : function (unmaskedTarget) {
        if (this.unmaskedTargets) this.unmaskedTargets.remove(unmaskedTarget);
    },

    //> @classMethod RPCManager.sendQueue()
    //
    // Send all currently queued requests to the server.  You need only call this method if you are
    // using queuing otherwise your requests are synchronously submitted to the server.
    // <br><br>
    // This method will do nothing and the callback will not be called if no requests have actually 
    // been queued. You can detect whether the queue is empty by calling 
    // +link{RPCManager.getQueueTransactionId(), getQueueTransactionId()}.
    // <br><br>
	// NOTE: if you aren't the caller who first enables queuing (startQueue() returns
    // true), you should in general avoid calling sendQueue(), because whoever was
    // first to enable queuing may have more requests to add to the same queue.
    //
    // @param [callback] (RPCQueueCallback) Callback to fire when the queued operations complete. Callback
    // will be fired with 1 parameter: <code>responses</code> an array of +link{DSResponse} or 
    // +link{RPCResponse} objects that were part of the transaction fired by this method.
    // 
    // @see classMethod:RPCManager.send()
    // @see classMethod:RPCManager.sendRequest()
    // @see classMethod:RPCManager.startQueue()
    //
    // @requiresModules SCServer
    // @visibility external
    //<
    //> @method Callbacks.RPCQueueCallback
	// Callback to fire when a queue of requests sent via 
	// {@link com.smartgwt.client.rpc.RPCManager#sendQueue(RPCQueueCallback)} returns.
	// <P>Note that the Array of RPCResponses passed to this callback
	// will actually be DSResponse objects for any requests that were actually 
	// DSRequests. 
	// <smartgwt>DSResponse is a subclass of RPCResponse, and you can "typecast" the
	// underlying JavaScript object to a DSResponse like so:<br>
	// <code>new DSResponse(rpcResponse.getJsObj());</code></smartgwt>
	//
	// @param response (Array of RPCResponse) array of responses returned from the sent queue of requests
	//
	// @visibility external
	//<
    
	sendQueue : function (callback, prompt, URL, delay) {
		var transaction = this.currentTransaction;

        // we're going to submit this transaction or error out in some way - in either way we're not
        // going to continue queueing
        this.currentTransaction = null;
		this.queuing = false;

		if (!transaction) {
			//>DEBUG Note this can happen easily if rpcRequests have been deferred because they
            // are attempted before page load.
			this.logInfo("sendQueue called with no current queue, ignoring");
			//<DEBUG
			return false;
		}
		if (delay) this.delayCall("_sendQueue", [callback,prompt,URL,transaction]);
		else return this._sendQueue(callback,prompt,URL,transaction);
    },

    _sendQueue : function (callback,prompt,URL,transaction) {
        // for flags such as "directSubmit" that affect the entire transaction, use the first
        // request
        var request = transaction.operations[0];

        
        if ((!isc.Page.isLoaded() || this.delayingTransactions) && !request._returnStreamFileURL) {
            transaction._args = {
                callback: callback,
                prompt: prompt,
                URL: URL
            };           
            if (!this.delayingTransactions) {
                isc.Page.setEvent("load", this, isc.Page.FIRE_ONCE, "resendDelayedTransactions");
                this.delayingTransactions = true;
            }
            this.delayTransaction(transaction);
            return request;
        }

        //>Offline
        // keep a timestamp of when the transaction was sent for offline mode
        transaction.timestamp = new Date().getTime();
        if (!this.onLine && !this.offlinePlayback) {
            // check internal onLine flag instead of isc.isOffline() because we want to make
            // sure that offline transaction playback completes before new requests are sent to
            // the server, preserving order
            this.offlineTransaction(transaction);
            return request;
        }
        //<Offline

        // if all the operations are clientOnly, we don't need to go to the server at all
		var allClientOnly = true;
		for (var i = 0; i < transaction.operations.length; i++) {
			if (!transaction.operations[i].clientOnly) {
				allClientOnly = false;
				break;
			}
		}
		if (allClientOnly) {
            transaction.allClientOnly = true;   
		    transaction.sendTime = isc.timeStamp();
		    if (callback != null) {
                transaction._userCallback = callback;
            }
            
            this.delayCall("_performTransactionReply", [transaction.transactionNum], 0);
			return request;
		}
		
        // figure out the prompt and URL, saving them on the transaction object in case we need to
        // delay
        // NOTE use Page.getURL() to support special directories such as "[APPFILES]"
        URL = transaction.URL = isc.Page.getURL(URL || transaction.URL || this.getActionURL());        
        
        // do not log non-trackable RPCs - these are used form DevConsole comm and create a lot
        // of noise in the logs
        if (request.doNotTrackRPC) URL = this.addParamsToURL(URL, {isc_noLog: "1"});

        var addTNumToParams = false;
        if (!request.useSimpleHttp && transaction.transport != "scriptInclude") {
            URL = this.markURLAsRPC(URL);
            // Add transport / transactionNum to URL
            
            if (transaction.transport == "xmlHttpRequest") {
                URL = this.markURLAsXmlHttp(URL);
                addTNumToParams = true;
            } else {
            URL = this.addParamsToURL(URL, {isc_tnum: transaction.transactionNum});
        }
            if (document.domain != location.hostname) URL = this.addDocumentDomain(URL);
        }
        prompt = transaction.prompt = ((transaction.showPrompt == null || transaction.showPrompt) ? 
            (prompt || transaction.prompt || this.defaultPrompt) : null);

        if (prompt) this.doShowPrompt(transaction, prompt, transaction.promptDelay || this.promptDelay);

        // support RPCRequest.params
        var transactionParams = {};
        var haveParams = false;
        for (var i = 0; i < transaction.operations.length; i++) {
            var rpcRequest = transaction.operations[i];
            var params = rpcRequest.params,
                requestHasParams = params != null;
            
            if (addTNumToParams) {
                params = params || {};
                params.isc_tnum = transaction.transactionNum;
            }
            
            var queryParams = rpcRequest.queryParams;
            var origParams = params;

            // undocumented - request.queryParams applied to URL - key collisions
            // result in multivalued param on server (which is in contrast to the way
            // request.params works, where last key overrides)
            if (queryParams && isc.isAn.Object(queryParams)) {
                URL = transaction.URL = this.addParamsToURL(URL, queryParams);
            }

            //>DEBUG
            if (requestHasParams && haveParams) {
                this.logWarn("Multiple RPCRequests with params attribute in one transaction - merging");
            }
            //<DEBUG

            if (params) {
                if (isc.isA.String(params)) {
                    if (window[params]) params = window[params]; // component
                    else if (isc.Canvas.getForm(params)) params = isc.Canvas.getForm(params); // native form
                    else {
                        //>DEBUG
                        this.logWarn("RPCRequest: " + isc.Log.echo(rpcRequest) 
                            + " was passed a params value: " + params 
                            + " which does not resolve to a component or a native"
                            + " form - request to server will not include these params");
                        //<DEBUG
                        params = null;
                    }   
                }
                if (isc.isA.Class(params)) {
                    if (params.getValues) params = params.getValues();
                    //>DEBUG
                    else {
                        this.logWarn("RPCRequest: " + isc.Log.echo(rpcRequest)
                            + " was passed an instance of class " + params.getClassName()
                            + " (or a global ID that resolved to this class)"
                            + " - this class does not support the getValues() method - request to"
                            + " server will not include these params");
                    }
                    //<DEBUG
                }
            
                if (params && !isc.isAn.Object(params)) {
                    //>DEBUG
                    this.logWarn("params value: " + origParams + " for RPCrequest: " 
                        + isc.Log.echo(rpcRequest) + " resolved to non-object: " 
                        + isc.Log.echo(params) + " - request to server will not include these params");
                    //<DEBUG
                    params = null;
                }

                if (params) {
                    isc.addProperties(transactionParams, params);
                    haveParams = true;
                }
            }
        }

		//>DEBUG
        if (this.logIsInfoEnabled()) {
    		this.logInfo("sendQueue[" + transaction.transactionNum + "]: " +
	    				 transaction.operations.length + " RPCRequest(s); transport: " +
                         transaction.transport + "; target: " + URL);
        }
		//<DEBUG
        
        if (isc.DataSource) {
            for (var i = 0; i < transaction.operations.length; i++) {
                var rpcRequest = transaction.operations[i];
                isc.DataSource.recordTimingData(rpcRequest, "performDSOperation", "end");
                isc.DataSource.recordTimingData(rpcRequest, "Marshall and send request", "start");
            }
        }
        // about to send
        transaction.changed();
        // call performTransactionReply when the transaction completes (regardless of transport)
        //if (callback) transaction.callback = callback;
        transaction.callback = "isc.RPCManager.performTransactionReply(transactionNum,results,wd)";
        if (callback) transaction._userCallback = callback;
               
            // send the request to the server via the configured transport
            var params = transactionParams;

            // Call isc.Comm.send<TransportType>(...)
            var transport = transaction.transport,
                transportMethodName = "send" + (transport.substring(0,1).toUpperCase()) + transport.substring(1);

            if (isc.Comm[transportMethodName] == null) {
                this.logWarn("Attempt to send transaction with specified transport '"
                             + transaction.transport + "' failed - unsupported transaction type.");
                return;
            }

            this._setTransactionTimeoutTimer(transaction);
            
            
            var requestData;
            var restRequests = []
            for (var i = 0; i < transaction.operations.length; i++) {
                if (transaction.operations[i].isRestRequest) {
                  restRequests.push(transaction.operations[i])
                }
            }
            if (restRequests.length > 1) {
                if (restRequests[0].contentType == this._$XML_MIME_TYPE) {
                    requestData = "<transaction transactionNum=\"" + transaction.transactionNum + "\">";
                    requestData += "<operations>"
                    for (var i = 0; i < restRequests.length; i++) {
                        requestData += restRequests[i].data;
                    }
                    requestData += "</operations>";
                    requestData += "</transaction>";
                } else {  // Not XML, must be JSON
                    requestData = "{ \"transaction\": { \"transactionNum\": " + transaction.transactionNum + ", ";
                    requestData += "\"operations\": [";
                    for (var i = 0; i < restRequests.length; i++) {
                        if (i > 0) requestData += ", ";
                        // data parameter could be cleared during DataSource.sendDSRequest method
                        var requestParams = restRequests[i].data || restRequests[i].params;
                        if (!isc.isA.String(requestParams)) {
                            requestParams = isc.JSON.encode(requestParams);                            
                        }
                        requestData += requestParams;
                    }
                    requestData += "]}}";
                }
             } 
             else if(restRequests.length==1) {
                  requestData = restRequests[0].data;
             }
             else if (request.useSimpleHttp) {
                requestData = request.data;
             }
            // Track the transactions that have been sent but not yet returned
            
            isc.RPCManager._activeTransactions.add(transaction.transactionNum);

            // Allow the transport method to return a transactionRequest object 
            transaction.transactionRequest = isc.Comm[transportMethodName]({
                URL: URL, 
                httpMethod: request.httpMethod,
                contentType: request.contentType,
                httpHeaders: request.httpHeaders,
                bypassCache: request.bypassCache,
                data: requestData,
                fields: params, 
                target: request.target,
                // valid only for scriptInclude
                callbackParam: request.callbackParam,
                transport: transaction.transport,
                blocking: request.blocking,
                useSimpleHttp:request.useSimpleHttp,
                transactionNum: transaction.transactionNum,
                transaction: transaction,
                xmlHttpRequestResponseType: request.xmlHttpRequestResponseType
            });

        

        // Invoke user hook
        if (isc.isA.Function(this.queueSent)) this.queueSent(transaction.operations);

		//>DEBUG
		transaction.sendTime = isc.timeStamp();
		//<DEBUG

        if (isc.DataSource) {
            for (var i = 0; i < transaction.operations.length; i++) {
                var rpcRequest = transaction.operations[i];
                isc.DataSource.recordTimingData(rpcRequest, "Marshall and send request", "end");
                isc.DataSource.recordTimingData(rpcRequest, "Client processing", "end");
            }
        }

		return request;
	},

    _$XML_MIME_TYPE: "text/xml",

	// called by the rpcRequest.transport with the server results.
    // - for xmlHttpRequest transport, "results" is an xmlHttpRequest, and it's contents may be
    //   eval'd or delivering directly based on settings like evalResult
    // - for other transports, a JavaScript object is delivered
	performTransactionReply : function (transactionNum, results, wd) {
    	// look up this transaction
		var transaction = this.getTransaction(transactionNum);
		if (!transaction) {
            
			//>DEBUG
			this.logWarn("performTransactionReply: No such transaction " + transactionNum);
			//<DEBUG
			return false;
		}
        
        
        delete transaction._handlingResponse;
        delete transaction.abortCallbacks;
        
        transaction.receiveTime = isc.timeStamp();
        transaction.changed();
        
        isc.RPCManager._activeTransactions.remove(transactionNum);
		//>DEBUG
		this.logInfo("transaction "+transactionNum + " arrived after " +
					 (transaction.receiveTime - transaction.sendTime) + "ms");
		//<DEBUG

        

		// if there are no results, something catastrophic happened
		if (results == null) {
			//>DEBUG
			this.logFatal("No results for transaction " + transactionNum);
			//<DEBUG
            
			return false;
		}

        // results is the xmlHttpRequest object - the results is the responseText
        if (transaction.transport == "xmlHttpRequest") {
            var xmlHttpRequest = results;

            transaction.xmlHttpRequest = xmlHttpRequest;

            // An InvalidStateError is thrown if the XHR responseType is not '' (the default) or 'text'.
            // Example: "Failed to read the 'responseText' property from 'XMLHttpRequest': The
            // value is only accessible if the object's 'responseType' is '' or 'text' (was 'blob')."
            results = "response" in xmlHttpRequest ? xmlHttpRequest.response : xmlHttpRequest.responseText;

            // Crazy FF bug - if the network cable is unplugged, accessing xhr.status throws a
            // chrome exception and stops execution - but accessing other attributes such as
            // responseText works fine
            var status;
            try {
                status = xmlHttpRequest.status;
            } catch (e) {
                this.logWarn("Unable to access XHR.status - network cable unplugged?");
                status = -1;
            }

            // In IE, the status code will sometimes be reported as "1223" when IE has
            // actually received "204: No content".  We've seen this with the local HTTPProxy
            // relaying a 204 response from Amazon S3, and this has been publicly reported in
            // connection with "kupu", try searching for "http status 1223"
            if (status == 1223) status = 204;

            // using XMLHttpRequest against the filesystem results in an HTTP code of zero -
            // very strange, but need to support this as a non-error case.
            //
            // Note that status == 0 can also mean a security violation whereby an XHR request
            // to the origin server receives a 302 redirect to a non-origin server.  IE reports
            // this is status 0, so we check against location.protocol here to disambiguate.
            // Note the "app-resource:" protocol is returned by Adobe AIR when accessing 
            // files as local resources
            if (status == 0 && 
                (location.protocol == "file:" || location.protocol == "app-resource:")) 
                    status = 200;

            // actually modify the response code so upstream code works without handling these
            // cases
            transaction.httpResponseCode = status;
            try {
                transaction.httpResponseText = xmlHttpRequest.responseText;
            } catch (e) { /*ignore*/ }
            transaction.httpResponse = xmlHttpRequest.response;

            // relogin support
            // users can specify the ignoreReloginMarkers flag to have login response code
            // processing be disabled - useful for custom network formats where the relogin
            // marker could appear
            if (status != -1 && !transaction.ignoreReloginMarkers &&
                transaction.httpResponseText != null &&
                this.processLoginStatusText(xmlHttpRequest, transactionNum))
            {
                return;
            }

            // additional end-user hook just in case
            if (status != -1 && this.responseRequiresLogin(xmlHttpRequest, transactionNum)) {
                this.handleLoginRequired(transactionNum);
                return;
            }

            if (status != -1 && this.responseIsRelogin(xmlHttpRequest, transactionNum)) {
                this.handleLoginRequired(transactionNum);
                return;
            }

            // see: http://danweber.blogspot.com/2007/04/ie6-and-error-code-12030.html
            if (status == 12030 && isc.Browser.isIE) {
                this.logWarn("Received HTTP status code 12030, resubmitting request");
                this.resubmitTransaction(transactionNum);
                return;
            }

            
            var url = transaction.URL;
            var realStatus;
            if (transaction.isProxied) {
                url = transaction.proxiedURL+" (via proxy: " + url +")";

                
                var headers = this.getHttpHeaders(xmlHttpRequest, transaction);
                var proxyHeader;
                if (headers) {
                    
                    for (var key in headers) {
                        if (key.toLowerCase() == "x-isc-httpproxy-status") {
                            proxyHeader = headers[key];
                            break;
                        }
                    }
                }
                if (proxyHeader && proxyHeader == "-91") realStatus = -91;
                if (proxyHeader && proxyHeader == "-92") realStatus = -92;
            }

            if (realStatus) status = 500;

            // All HTTP 2xx codes indicate success.  Success codes other than 200 OK are
            // somewhat obscure, but are used by Amazon S3 and possibly other REST APIs
            if (status > 299 || status < 200) { //error
                results = this._makeErrorResults(transaction, {
                    data : "Transport error - HTTP code: "+ status
                           +" for URL: " + url
                           + (status == 302 ? " This error is likely the result"
                              + " of a redirect to a server other than the origin"
                              + " server or a redirect loop." : ""),
                    status: realStatus ? realStatus : isc.RPCResponse.STATUS_TRANSPORT_ERROR
                });

                //>DEBUG
                this.logDebug("RPC request to: " + url 
                             + " returned with http response code: " 
                             + status +". Response text:\n"
                             + transaction.httpResponseText);
                //<DEBUG
                transaction.status = realStatus ? realStatus : isc.RPCResponse.STATUS_TRANSPORT_ERROR;

                // Give the developer an opportunity to intercept any transport errors BEFORE we fire
                // any request callbacks
                // Use case: This would give the developer an opportunity to handle intermittent
                // server errors by suspending and resubmitting the transaction before any callback
                // logic attempts to handle the individual responses.
                // Note that this is fired regardless of individual request.willHandleError settings
                //
                // _handlingResponse flag allows us to catch the case where we suspend and
                // immediately resubmit the transaction 
                transaction._handlingResponse = true;
                this.handleTransportError(transactionNum, transaction.status,
                                         transaction.httpResponseCode,
                                         transaction.httpResponseText);
                if (transaction.suspended || transaction.abortCallbacks) {
                    delete transaction.abortCallbacks;
                    delete transaction._handlingResponse
                    return;
                }
                delete transaction._handlingResponse;
            }

            if (transaction.httpResponseText != null && 
               transaction.httpResponseText.contains("parent.isc.RPCManager.handleMaxPostSizeExceeded(window.name);"))
            {
                this._handleMaxPostSizeExceeded(transactionNum);
                return;
            }
        } else if (transaction.transport == "hiddenFrame") {

            // relogin support
            // users can specify the ignoreReloginMarkers flag to have login response code
            // processing be disabled - useful for custom network formats where the relogin
            // marker could appear
            if (!transaction.ignoreReloginMarkers && 
                 this.processLoginStatusText(results, transactionNum))
            {
                return;
            }
            
            
        }

        // keep a copy of the results in serialized form for developer console to ensure that
        // we can show the actual server response before e.g. the user messes with the data by
        // handing it to a Tree or hand-processing it in some way
        // 
        // Note that for XHR and HiddenFrame transports, 'results' is already a string here so
        // no serialization will take place here.  The serialization is really only there for
        // error cases and scriptInclude transport
        if (this._trackRPC || !this._inititalizedTrackRPC) {
            if (isc.isA.String(results)) {
                transaction.serializedCommResults = results;
            } else {
                transaction.serializedCommResults = isc.JSON.encode(results, 
                                                 {prettyPrint:true,
                                                  strictQuoting:false,
                                                  serializeInstances:"short",
                                                  skipInternalProperties:false});
                transaction.serializedCommResultsAreFormatted = true;
            }
        }

        // store the results array in the transaction results
        transaction.results = results;
        
        
        this._performTransactionReply(transactionNum);
		return true;
	},

    _handleMaxPostSizeExceeded : function (transactionNum) {
		var transaction = this.getTransaction(transactionNum);

        var results = this._makeErrorResults(transaction, {
            data : "The server did not receive the data that was sent to it.  "
                  + "Please see the documentation for isc.RPCResponse.STATUS_MAX_POST_SIZE_EXCEEDED",
            status: isc.RPCResponse.STATUS_MAX_POST_SIZE_EXCEEDED
        });
        transaction.status = isc.RPCResponse.STATUS_MAX_POST_SIZE_EXCEEDED

        // store the results array in the transaction results
        transaction.results = results;
        
        this._performTransactionReply(transactionNum);        
    },
    handleMaxPostSizeExceeded : function (commFrameID) {
        var commFrame = window[commFrameID];
        if(!commFrame) {
            this.logError("Unable to handle max post size exceeded - can't locate object: " + commFrameID);
            return;
        }
        var transaction = this.getTransaction(commFrame);
        if (!transaction) {
           this.logError("Unable to find transaction for comm frame id: "+ commFrameID);
           return;
        }
        this._handleMaxPostSizeExceeded(transaction.transactionNum);
    },

    
    handleRequestAborted : function (transactionNum) {
    	var transaction = this.getTransaction(transactionNum);

        var results = this._makeErrorResults(transaction, {
            data : "The server was unable to complete the request as delivered. "
                 + "This may be caused by attempting to upload an unusually large file, for example.  Please check the server log for details of the failure.",
            status: isc.RPCResponse.STATUS_FAILURE
        });
        transaction.status = isc.RPCResponse.STATUS_FAILURE;

        // store the results array in the transaction results
        transaction.results = results;
        
        this._performTransactionReply(transactionNum);
    },
    
    responseIsRelogin : function (xmlHttpRequest, transactionNum) {
        var status = xmlHttpRequest.status;

        // When the user session has timed out, an HTTP authenticator will typically respond to
        // a request with a 302 redirect.  When document.domain is set, and the redirect
        // targets what looks to the browser like a non-origin server, browsers behave
        // inconsistently (probably because the XHR spec doesn't deal with this issue).  The
        // problem is that since the content targeted by the XHR request doesn't execute,
        // there's no way for the browser to know that it's safe relative to the document.domain
        // setting of the page.  As a result, browsers simply block access to the content that
        // ultimately comes back via the 302 redirect.
        //
        // This is a problem for us because we rely on our relogin markers to execute or arrive
        // as a string so we can tell that a relogin is required.  The solution is as follows:
        //
        // FF: when the above occurs, FF returns an HTTP status code of 302, which is normally
        // impossible, except for a redirect loop.  If the user sets the special
        // treatRedirectAsRelogin flag, we'll use that as the discriminator.  One problem with
        // this is that if the page has set document.domain and the server returns a 302, even
        // one that targets the origin server, FF will report the status as 302.  This means
        // that relogin in FF with document.domain set is not transparently compatible with
        // other logic on the page that may rely on local redirects.
        //
        // IE: If the request that triggered the redirect contained no body, IE reports a
        // status code of 0.  If there was a body, IE bogusly reports status 200, but the xhr
        // contains an empty responseText and empty response headers, so we use these two
        // discriminators in IE.
        //
        // Safari untested.
        if (document.domain != location.hostname &&
            (
             // FF
             (status == 302 && this.treatRedirectAsRelogin) ||
             // IE - no body sent in request that generated the off-site redirect
             (status == 0) ||
             // IE - body was sent in request that generated the off-site redirect
             (status == 200 && xmlHttpRequest.getAllResponseHeaders() == isc.emptyString &&
              xmlHttpRequest.responseText == isc.emptyString)
            ))
        {
            this.logDebug("Detected document.domain 302 relogin condition - status: "+status);
            return true;
        }
        return false;
    },

    //> @classAttr RPCManager.loginStatusCodeMarker (String : RPCManager.loginStatusCodeMarker : IRWA)
    // String sequence which marks the response as a one which contains login status information.
    //
    // @see RPCManager.loginRequiredMarker
    //
    // @group relogin
    // @visibility external
    //<
    loginStatusCodeMarker: "<SCRIPT>//'\"]]>>isc_",

    //> @classAttr RPCManager.loginRequiredMarker (string : RPCManager.loginRequiredMarker : IRWA)
    // Marker the system will look for in order to detect when login is required.
    // <P>
    // The default loginRequired marker should generally <b>not</b> be customized.  It is
    // designed to be safe to insert into any HTML page or other server response without
    // affecting display or functionality, for example, within an HTML comment.  You should
    // *only* customize the <code>loginRequiredMarker</code> if you have absolutely no ability
    // to change the response that the server will send when login is required.
    // <P>
    // If you do customize the <code>loginRequiredMarker</code>, then the loginRequiredMarker,
    // +link{loginSuccessMarker} and +link{maxLoginAttemptsExceededMarker} should all start with the 
    // +link{loginStatusCodeMarker}.  If they do not, there will be a small impact on 
    // performance as every response must be separately scanned for each marker, instead of just scanning
    // once for the +link{RPCManager.loginStatusCodeMarker}.
    // <P>
    // In addition, the +link{loginStatusCodeMarker} should ideally contain text that could not possibly
    // validly appear as a data value in a normal response, since if that were possible, end
    // users could enter the loginRequiredMarker as a data value and cause SmartClient to
    // falsely detect session timeout when handling an ordinary data response.  This is why the
    // default marker has characters that make it impossible for it to be validly interpreted
    // as a JavaScript String, XML document or HTML content - there is no way that an end user
    // could enter this as a data value in an application and have it appear verbatim in a
    // server response.
    //
    // @group relogin
    // @visibility external
    //<
    loginRequiredMarker: "<SCRIPT>//'\"]]>>isc_loginRequired",

    // NOTE: the default loginStatusCodeMarker avoids being a valid data value, since:
    // - it can't appear as a JS string or XML attribute (embedded quotes of each type)
    // - it can't appear an a tag value in an XML file (CDATA terminator, followed by additional >)
    // - it can't appear in user-entered HTML content, because the most rudimentary data
    //   cleaners should remove or deactivate <SCRIPT> in user data displayed within HTML
    
    //> @classAttr RPCManager.loginSuccessMarker (String : RPCManager.loginSuccessMarker : IRWA)
    //
    //  Marker the system will look for in order to detect when login was successfull.
    //
    // @see RPCManager.loginRequiredMarker
    //
    // @group relogin
    // @visibility external
    //<
    loginSuccessMarker: "<SCRIPT>//'\"]]>>isc_loginSuccess",

    //> @classAttr RPCManager.maxLoginAttemptsExceededMarker (String : RPCManager.maxLoginAttemptsExceededMarker : IRWA)
    //
    //  Marker the system will look for in order to detect when the number of maximum logins was exceeded.
    //
    // @see RPCManager.loginRequiredMarker
    //
    // @group relogin
    // @visibility external
    //<
    maxLoginAttemptsExceededMarker: "<SCRIPT>//'\"]]>>isc_maxLoginAttemptsExceeded",

    processLoginStatusText : function (xhr, transactionNum) {
        // don't scan long RPC responses for relogin
        // NOTE: Although the variable is called "xhr", it might actually be a string of HTML,
        // now that we support relogin with the hiddenFrame transport
        var text = xhr.responseText || xhr;
        // In Safari, if you target a file that is empty, the response text is null
        if (text && text.length < this.maxLoginPageLength) {

            // if all the markers start with loginStatusCodeMarker, then only handle markers
            // if loginStatusCodeMarker was found in the text
            if (this.loginSuccessMarker.indexOf(this.loginStatusCodeMarker) != 0 
                || this.maxLoginAttemptsExceededMarker.indexOf(this.loginStatusCodeMarker) != 0
                || this.loginRequiredMarker.indexOf(this.loginStatusCodeMarker) != 0) {
            } else {
                var iscIndex = text.indexOf(this.loginStatusCodeMarker);
                if (iscIndex == -1) return false;
            }

            // otherwise scan for each marker individually (slower)
            if (text.indexOf(this.loginRequiredMarker, iscIndex) != -1) {
                this.handleLoginRequired(transactionNum);
                return true;
            } else if (text.indexOf(this.loginSuccessMarker, iscIndex) != -1) {
                this.handleLoginSuccess(transactionNum);
                return true;
            } else if (text.indexOf(this.maxLoginAttemptsExceededMarker, iscIndex) != -1) {
                this.handleMaxLoginAttemptsExceeded(transactionNum);
                return true;
            }
        }
        return false;
    },
    processLoginStatusCode : function (rpcResponse, transactionNum) {
        if (rpcResponse.status == isc.RPCResponse.STATUS_LOGIN_REQUIRED) {
            this.handleLoginRequired(rpcResponse.transactionNum);
            return true;
        } else if (rpcResponse.status == isc.RPCResponse.STATUS_LOGIN_SUCCESS) {
            this.handleLoginSuccess(rpcResponse.transactionNum);
            return true;
        } else if (rpcResponse.status == isc.RPCResponse.STATUS_MAX_LOGIN_ATTEMPTS_EXCEEDED) {                
            this.handleMaxLoginAttemptsExceeded(rpcResponse.transactionNum);
            return true;
        }
        return false;
    },
    // extra end-user hook
    responseRequiresLogin : function (xmlHttpRequest, transactionNum) {
        return false;
    },

    // create an RPCResponse, defaulting various fields based on the HTTP transaction it was
    // part of
    createRPCResponse : function (transaction, request, props) {
        var response = isc.addProperties({
            operationId: request.operation.ID,
            // expose the passed clientContext
            clientContext: request.clientContext,
            internalClientContext: request.internalClientContext,
            // backcompat: expose the entirety of the dsRequest as context (flags used to be
            // merged with preserved data)
            context: request,
            transactionNum: transaction.transactionNum, 
            httpResponseCode: transaction.httpResponseCode,
            httpResponseText: transaction.httpResponseText,
            xmlHttpRequest: transaction.xmlHttpRequest,
            transport: transaction.transport,
            status: transaction.status,
            clientOnly: request.clientOnly
        }, props);

        if (transaction.transport == "xmlHttpRequest") {
            isc.addProperties(response, {
                httpHeaders: this.getHttpHeaders(transaction.xmlHttpRequest, transaction)
            });
        }
        
        return response;
    },
    
    getHttpHeaders : function (xhr, transaction) {
    
    
        // A clientOnly request will of course not have an HTTP response of any kind
        if (transaction.allClientOnly) {
            //this.logWarn("Skipping getHttpHeaders for clientOnly request");
            return;
        } 

        // Otherwise the xmlHttpRequest should be valid
        if (!xhr) {
            this.logWarn("getHttpHeaders called with a null XmlHttpRequest object"); 
            return;
        }

        // This check for xhr.getAllResponseHeaders fails with a JS error ("Object does not 
        // support method or property") in all versions of Internet Explorer
        if (!isc.Browser.isIE && !xhr.getAllResponseHeaders) {
            // A normal case - probably just indicates that we're in an older browser
            return null;
        }
        
        var headersString;
        try {
            headersString = xhr.getAllResponseHeaders();
        } catch (e) {
            this.logWarn("Exception thrown by xmlHttpRequest.getAllResponseHeaders(): " + e); 
        }
        
        if (!headersString) {
            this.logWarn("xmlHttpRequest.getAllResponseHeaders() returned null"); 
            return null;
        }
        
        var headers = headersString.split('\n');
        var headersObj = {};
        for (var i = 0; i < headers.length; i++) {
            if (headers[i].replace(/^\s+|\s+$/g, '') == "") continue;
            var colonPos = headers[i].indexOf(':');
            if (colonPos == -1) {
                this.logWarn("GetAllResponseHeaders string had malformed entry at line " + 1 + 
                             ".  Line reads " + headers[i]);
                continue;
            }
            var key = headers[i].substring(0, colonPos);
            headersObj[key] = headers[i].substring(colonPos+1).replace(/^\s+|\s+$/g, '');
            // Just to be helpful...
            if (headersObj[key] == "true") headersObj[key] = true;
            if (headersObj[key] == "false") headersObj[key] = false;
        }

        // note that 'Set-Cookie' is renamed by the proxy server - copy across if present
        if (headersObj["X-Proxied-Set-Cookie"] != null) {
            headersObj["Set-Cookie"] = headersObj["X-Proxied-Set-Cookie"];
        }
        return headersObj;

    },

    
    _restXMLTransactionStart : "<responses>",
    _restXMLTransactionEnd : "</responses>",
    _restXMLResponseStart : "<response>",
    _restXMLResponseEnd : "</response>",
    _stripXMLTransactionTags : function (restResponse) {
        var startIndex = restResponse.indexOf(this._restXMLTransactionStart);
        if (startIndex != -1) {
            return restResponse.substring(startIndex + this._restXMLTransactionStart.length,
                                        restResponse.lastIndexOf(this._restXMLTransactionEnd));
        }
        return restResponse;
    },
    _getXMLResponses : function (xmlText) {
        xmlText = this._stripXMLTransactionTags(xmlText);
        var responses = [];
        var endIndex = 0;
        if (xmlText) {
            while (true) {
                var startIndex = xmlText.indexOf(this._restXMLResponseStart, endIndex);
                if (startIndex == -1) break;
                endIndex = xmlText.indexOf(this._restXMLResponseEnd, startIndex);
                if (endIndex == -1) break;   // XXX: Should probably log something here...
                responses.add(xmlText.substring(startIndex, 
                                                endIndex + this._restXMLResponseEnd.length));
            }
        }
        return responses;
    },
    _stripRestTags : function (restResponse, ds) {
        if (!restResponse || !ds) return restResponse;
        var startIndex = ds.jsonPrefix ? restResponse.indexOf(ds.jsonPrefix) : 0,
            prefixLength = ds.jsonPrefix ? ds.jsonPrefix.length : 0,
            endIndex = ds.jsonSuffix ? restResponse.lastIndexOf(ds.jsonSuffix) : restResponse.length;
        if (startIndex == -1) {
            this.logWarn("DataSource " + ds.ID + ": REST response did not contain the " +
                         "jsonPrefix configured for this DataSource ('" + ds.jsonPrefix + 
                         "'). Response evaluation may well fail as a result. Check your " +
                         "server logic and/or DataSource definition.");
            startIndex = 0;
        }
        if (endIndex == -1) {
            this.logWarn("DataSource " + ds.ID + ": REST response did not contain the " +
                         "jsonSuffix configured for this DataSource ('" + ds.jsonSuffix + 
                         "'). Response evaluation may well fail as a result. Check your " +
                         "server logic and/or DataSource definition.");
            startIndex = restResponse.length;
        }
        return restResponse.substring(startIndex + prefixLength, endIndex);
    },
    
    //> @classAttr RPCManager.allowIE9Leak (boolean : true : IRWA)
    // In Internet Explorer 9, when a string of 
    // JavaScript is evaluated via the native <code>eval()</code> function, objects created 
    // within that evaluation are not released from browser memory until the page is 
    // reloaded.
    // <P>
    // SmartClient uses the <code>eval()</code> function to evaluate JSON formatted
    // responses to RPCRequests by default, making long running applications potentially
    // susceptible to memory leaks over time.
    // <P>
    // Note that this does not apply to DataSources which use
    // +link{DataSource.useStrictJSON} formatted responses as the framework avoids
    // calling eval() altogether and makes use of the native browser 
    // <code>JSON.parse()</code> method which does not have this issue. By default
    // we also use strict json formatted responses for all
    // +link{DataSource.dataFormat,dataFormat:"iscServer"} dataSources in IE9, so these
    // leaks are mainly a concern only for dataSources with +link{dataSource.dataFormat}
    // set to "json".
    // <P>
    // Setting this property to <code>false</code> enables a workaround suggested on the
    // <a href="http://support.microsoft.com/kb/2572253">Microsoft Knowledge Base</a> to 
    // avoid such memory leaks by evaluating script in a hidden iframe and periodically
    // refresh that frame. However developers should be aware of the following
    // limitation with this setting: 
    // attempting to access certain object types including
    // <code>Date</code> or <code>function</code> objects generated from such an 
    // evaluation can subsequently lead to a JavaScript error with the message
    // <code>"Can't execute code from a freed script"</code>.
    // <P>
    // This workaround therefore may not be suitable for all transactions or dataSources
    // within a given application.
    // <P>
    // This property may also be specified for specific +link{RPCRequest.allowIE9Leak,RPCRequests}.
    // <P>
    // This issue is discussed further in the online 
    // <a href="http://forums.smartclient.com/showthread.php?t=8159">SmartClient FAQ</a>.
    //
    // @visibility external
    //<
    allowIE9Leak:true,
    
    // useJSONParse_IE9
    // This undocumented flag enables a hedge whereby for all iscServer dataSource requests
    // if we're in IE9 we default to using strict JSON (but also have a flag to
    // fallback to eval in case the response doesn't meet strict-JSON formatting restrictions.)
    useJSONParse_IE9:true,
    
	_performTransactionReply : function (transactionNum) {
        //!OBFUSCATEOK
        
		// get a pointer to the specified transaction
		var transaction = this.getTransaction(transactionNum);

        // we no longer clear transactions here as they can be suspended during an rpcResponse
        // callback, but we do need to clear the timeout here since the server has definitely
        // replied.
        this.clearTransactionTimeout(transactionNum);

        // The transaction could have timed out, but the server could in theory return just
        // after the timeout.  Since the timeout will have cleared the transaction object,
        // check it here.
		if (!transaction) return;

        //>DEBUG
        if (this.logIsDebugEnabled()) {
            this.logDebug("Result string for transaction "+transactionNum+": " + isc.Log.echoAll(transaction.results));
        }
        //<DEBUG
        
        
        var responseIsStructured;
        var allowIE9Leak = false,
            useStrictJSON = false,
            jsonReviverFunction = null;
            
        if (transaction.operations[0]
                 && transaction.operations[0].allowIE9Leak != null)
        {
             allowIE9Leak = transaction.operations[0].allowIE9Leak;
        } else if (isc.RPCManager.allowIE9Leak) {
            allowIE9Leak = true;
        }
        if (transaction.operations[0] && transaction.operations[0].useStrictJSON != null) {
            useStrictJSON = transaction.operations[0].useStrictJSON;
            
            
            
            jsonReviverFunction = transaction.operations[0].jsonReviver;
        }

        if (isc.Log.logIsInfoEnabled("RpcTabTiming")) {
            transaction.parseOrEvalResponseStart = isc.timeStamp();
        }
                
        if (transaction.transport == "scriptInclude") {
            // not structured, results are values
        } else if (isc.isAn.Array(transaction.results)) {
            // error - don't do anything here, just drop through to the logic below
            responseIsStructured = true;
            
        } else if (transaction.allClientOnly) {
            

            transaction.results = {status:0};
            transaction.receiveTime = isc.timeStamp();
        } else {
            // the results are available as a single string, which we can eval to get JS
            // objects.  
            
            
                // For REST queues, we assume that the settings that apply to the first request 
                // apply to all requests in the queue (this assumption is publicly documented)
                var request = transaction.operations[0];
                if (request && request.isRestRequest) {
                    var ds = isc.DataSource.get(request.dataSource);
                    if (ds) {
                        if (request && request.dataFormat == "json" && ds.jsonPrefix == null) 
                        {
                            responseIsStructured = true;
                        } else {
                            var restRPCStartIndex = transaction.results ?
                                transaction.results.indexOf(ds.jsonPrefix) : -1;
                            responseIsStructured = restRPCStartIndex != -1;
                        }
                        if (responseIsStructured) {
                            var restResponse = transaction.results;
                            restResponse = this._stripRestTags(restResponse, ds);
                            try {
                                if (useStrictJSON) {
                                    transaction.results = 
                                        isc.Class.parseStrictJSON(restResponse, jsonReviverFunction);
                                } else {
                                    // Pass in the 'isJSON' param to evaluate in an IFrame
                                    // in IE9, working around the known IE9 leak on eval.
                                    transaction.results = 
                                        isc.eval("(" + restResponse + ")",
                                            (allowIE9Leak ? false: true));
                                }
                            } catch (e) {
                                this.logWarn("Error evaling REST RPC response: " + e 
                                             + " response text: " + restResponse);
                            }
                        }
                    }
                }
            
            
            if (!responseIsStructured) {
                var operation = isc.isAn.Array(transaction.operations) ? transaction.operations[0]
                                                                       : null;
                if (operation && operation.isRestRequest && transaction.operations.length > 1) {
                    // NOTE: We assume that a queue whose first entry is a RestRequest consists
                    // entirely of RestRequests, and that all these RestRequests use the same 
                    // dataFormat
                    if (operation.dataFormat == "json") {
                        // This should have already been dealt with by the earlier section 
                        // checking for _restRPCStart - we must have a JSON response that was
                        // not wrapped in start/end tags.  This is unexpected, so we'll log it
                        // and bail
                        this.logWarn("Found a REST request that appears to be in JSON format, " +
                            "but the response was not wrapped as configured by the jsonPrefix " +
                            "and jsonSuffix properties - aborting")
                        return;
                    } else {
                        // This is a queued REST request in XML format.  Trim the outer bits off
                        // and then convert the XML string into an array of XML substrings
                        transaction.results = this._getXMLResponses(transaction.results);
                        responseIsStructured = transaction.results != null;
                    }
                }
            }
        }

        
        if (isc.Log.logIsInfoEnabled("RpcTabTiming")) {
            transaction.parseOrEvalResponseEnd = isc.timeStamp();
        }

        
        var results = transaction.results;
 	    if (responseIsStructured && !isc.isAn.Array(results)) results = [results];

        var requests = transaction.operations,
            responses = [];
        
        // set up a flag noting that we're firing transaction callbacks
        
        transaction._handlingResponse = true;
       
        // pair up the requests with their results.  
		for (var i = 0, j = 0; i < requests.length; i++) {
            
            var request = requests[i];
            
            // Handle Queue (so structured responses) 
            // containing both clientOnly requests and real http turnarounds
            // For any clientOnly requests the response is simply "success" and we rely on 
            // the callback at the DS level to populate with meaningful results
            var response;
            if (responseIsStructured && request.clientOnly) {
                response = isc.addProperties(this.createRPCResponse(transaction, request), {
                
                        isStructured: false
                });
            } else {
                response = isc.addProperties(this.createRPCResponse(transaction, request), {
                    
                    isStructured: responseIsStructured,
                    
                    // for scriptInclude, make all values available via callbackArgs - typically
                    // there will be only one - and that's accessible via "data", but all args
                    // are available via this callbackArgs - not currently exposed
                    callbackArgs : transaction.transport == "scriptInclude" ? results : null,
                    
                    // get the results from the server results array; 
                    //
                    // if the response is not an rpc, then the same response applies to all
                    // requests.  In general we really expect there to be only one request for non
                    // RPC responses.
                    results: responseIsStructured ? results[j] : results
                });
                if (responseIsStructured && request.isRestRequest) {
                    isc.addProperties(response, {data: results[j]});
                }
                j++;
            }

            // if no status has been set on the response, set to SUCCESS.  This can happen with
            // unstructured responses that simply load a file.
            if (response.status == null) response.status = 0;

            if (response.isStructured) {
                if (response.results && response.results.errors) {
                    var errors = response.results.errors;
                    // if the errors array contains only a single map, strip the enclosing array.
                    if (isc.isAn.Array(errors) && errors.length == 1) {
                        errors = errors[0];
                    }
                }
                
                
                if (response.results && !request.isRestRequest) {
                    // this makes status, data, startRow, endRow, etc available directly on 
                    // "rpcResponse"/"dsResponse", so it's not necessary to use eg
                    // "response.results.data"
                    // XXX: "results" is still a String at this point for XML REST requests, 
                    // so we really, really DON'T want to do this for such requests - if we 
                    // do, the response will end up with one new attribute per letter of text,
                    // like  0: "<", 1: "r", 2: "e", 3: "s", 4: "p", 5: "o", 6: "n" ... etc ...
                    if (!response.results.response) {
                        isc.addProperties(response, response.results);
                    } else {
                        // For some reason, if the response is reporting a transport error
                        // (a 404, for example), the "results" parameters are wrapped inside
                        // a "response" property, instead of being direct properties of the 
                        // results object
                        isc.addProperties(response, response.results.response);
                    }
                }
            }
            responses[i] = response;
            transaction.responses[i] = response;
		}
        transaction.changed();

        // Clear the prompt before we fire the operation replies.
                
        if (transaction.showPrompt) this.doClearPrompt(transaction);

        var requestNum = 0;
        while (requestNum < requests.length && !transaction.suspended && 
                !transaction.abortCallbacks) 
        {
            var request = requests[requestNum],
                response = responses[requestNum];
                
            if (isc.DataSource) {
                isc.DataSource.recordTimingData(response, "Parse/eval response", "start", 
                                                    transaction.parseOrEvalResponseStart);
                isc.DataSource.recordTimingData(response, "Parse/eval response", "end", 
                                                    transaction.parseOrEvalResponseEnd);
                isc.DataSource.recordTimingData(response, "performOperationReply", "start"); 
            }
            this.performOperationReply(request, response);
            if (isc.DataSource) {
                isc.DataSource.recordTimingData(response, "performOperationReply", "end"); 
            }
            
            requestNum++;
        }

        // optional transactionComplete API, fired for every transaction
        if (this.transactionComplete != null) {
            this.transactionComplete(transaction);
        }

        // cleanup the transaction unless it's been suspended
        if (!transaction.suspended && !transaction.abortCallbacks) {
            this.clearTransaction(transactionNum);
        }

        delete transaction.abortCallbacks;
        delete transaction._handlingResponse;
        
        // if this was an offline transaction, we're in playback mode - playback the next one
        if (transaction.offline) this.playbackNextOfflineTransaction();
        
        // Log the point at which we hand back to user code
        if (isc.DataSource) {
            var userCallbackInvoked = isc.timeStamp();
            var requestNum = 0;
            while (requestNum < requests.length && !transaction.suspended && 
                    !transaction.abortCallbacks) 
            {
                isc.DataSource.recordTimingData(responses[requestNum++], "User callback invoked",
                                                    "start", userCallbackInvoked);
            }
                
        }
        
        // fire the callback passed into sendqueue()
        if (transaction._userCallback) {
            var application = request.application ? request.application 
                                              : this.getDefaultApplication();
            if (isc.isA.String(application)) application = window[application];
            
            application.fireCallback(transaction._userCallback, "responses", [transaction.responses]);        
        }
        
        if (isc.DataSource) {
            var userCallbackFinished = isc.timeStamp();
            var requestNum = 0;
            while (requestNum < requests.length && !transaction.suspended && 
                    !transaction.abortCallbacks) 
            {
                isc.DataSource.recordTimingData(responses[requestNum], "User callback invoked",
                                                    "end", userCallbackFinished);
                isc.DataSource.recordTimingData(responses[requestNum++], "Client processing",
                                                    "end", userCallbackFinished);
            }
                
        }
	},

	performOperationReply : function (request, response) {
		var results = response.results,
			operation = request.operation;

		//>DEBUG
        if (this.logIsInfoEnabled()) {
            
                this.logInfo("rpcResponse(unstructured) results -->" 
                    + isc.Log.echoAll(results) + "<--");
            
        }
		//<DEBUG

        // support new RPCManager.requestsArePending() API
        this.pendingRpcs--;

        var modifiedResponse = isc.RPC.transformResponse(response, request, request.data);
        response = modifiedResponse || response;

        // support override of login response code handling
        if (this.processLoginStatusCode(response, response.transactionNum)) return;

        

        return this.fireReplyCallbacks(request, response);
	},

    fireReplyCallback : function (callback, request, response, data) {
        
        var application = request.application ? request.application 
                                              : this.getDefaultApplication();
        if (isc.isA.String(application)) application = window[application];
        var callbackResult = application.fireCallback(callback,
                                "rpcResponse,data,rpcRequest", [response, data,request]);
        return callbackResult;
    },

    evalResult : function (request, response, results) {
        var evalVars = request.evalVars;
        this.logDebug("evaling result" + (evalVars ? " with evalVars: " + isc.Log.echo(evalVars) : ""));

        var origAutoDraw = isc.Canvas.getInstanceProperty("autoDraw");
        if (request.suppressAutoDraw) isc.Canvas.setInstanceProperty("autoDraw", false);
        
        
        // results at this point is the string to eval
        // eval barfs on "{ ...  }", thinking it's a closure - work around this so developers
        // can return simple JSON format objects
        // Note: FF1.0.7 bug: curly must be escaped in the following regex.  Not an issue in IE
        // or FF1.5
        if (results.match(/^\s*\{/)) {
            
            results = "var evalText=" + results + ";evalText;"
        }
        
        
        var evalResult = isc.Class.evalWithVars(results, evalVars);
        if (request.suppressAutoDraw) isc.Canvas.setInstanceProperty("autoDraw", origAutoDraw);
        return evalResult;
    },

    // called by performOperationReply
    fireReplyCallbacks : function (request, response) {
        var operation = request.operation,
            results = response.results,
            // ignore evalResult for scriptInclude - not relevant
            evalResult = request.evalResult && request.transport != "scriptInclude" ? 
                this.evalResult(request, response, results) : null
        ;

        // for RPC rsponses, 'data' is the data value in the response object literal.  For
        // evalResult, it's the result of the eval (see above).  For serverOutputAsString, it's
        // the server string
        var data;
        
            data = (request.evalResult ? evalResult : results);

        // always have 'data' be available on the response object as well
        response.data = data;

        var transaction = this.getTransaction(response.transactionNum);

        // fire all callbacks set on the request
        // call callbacks:
        // - request.callback is passed to RPCManager.send/sendRequest
		// - context.afterFlowCallback is passed to actionMethods such as ListGrid.fetchData(),
        //   generally by application code that needs to do something after a canonical flow
        //   completes
        // Really, "flows" implemented on top of the RPC mechanism should provide their own,
        // chained callback to any higher layer.  Instead, for the moment, the RPCManager fires
        // exactly two level of callbacks in series.
		var callback = request.callback;
        if (callback != null) {

            // use fireCallback() to fire the callback
            this.fireReplyCallback(callback, request, response, data);
		}
    },

    // Error handling
	// --------------------------------------------------------------------------------------------
	
    //> @classMethod RPCManager.handleError()
    // <code>handleError</code> will be called if +link{rpcResponse.status} is negative and
    // +link{rpcRequest.willHandleError} was not set.
    // <p>
    // This method is called for both +link{DSResponse}s and +link{RPCResponse}s that have a
    // non-success status.  You can check whether the response is a DSResponse by checking
    // <code>response.isDSResponse</code>.
    // <p>
    // By default <code>handleError()</code> always logs a warning.  In addition, if
    // +link{rpcResponse.data, response.data} was set to a String, a warning dialog will be
    // shown to the user with response.data as the message, which allows the server to send
    // user error messages back without writing custom client-side error handling.
    // <p>
    // To do custom error handling that is specific to a particular component or type of
    // request, set +link{rpcRequest.willHandleError} and deal with errors in the
    // rpcRequest.callback.  To change the default system-wide error handling, override this
    // method.  Note that since <code>handleError()</code> is a class method, to override it
    // you will call +link{Class.addClassProperties(),addClassProperties()} rather than
    // addProperties(), like so:
    // <pre>
    //     isc.RPCManager.addClassProperties({
    //         handleError : function (response, request) { .. custom handling .. }
    //     })
    // </pre>
    // <p>
    // If you're using the xmlHttpRequest +link{rpcRequest.transport}, you can access the 
    // +externalLink{http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html,HTTP status code}
    // of the response (eg 404 Not Found or 500 Server Error) as +link{rpcResponse.httpResponseCode}.
    // <P>
    // For very advanced usage, the response.xmlHttpRequest contains the native XMLHttpRequest
    // object used to make the request.  Accessing this object is subject to possible
    // cross-platform bugs and inconsistencies, and Isomorphic recommends that you wrap any
    // access to the XMLHttpRequest object in a try/catch block because some browsers may throw
    // exceptions when certain attributes of this object are accessed.  For example, if you try
    // to access XMLHttpRequest.status (for the HTTP status code) when the network cable 
    // is unpluged in Windows, you'll get an Exception in Firefox.
    //
    // @param response (Response) the RPCResponse or DSResponse object returned from the server
    // @param request (Request) the RPCRequest or DSRequest that was sent to the server
    // 
    // @see DataSource.handleError()
    // @group errorHandling
    // @group operations
    // @visibility external
    //<

    //> @method Callbacks.HandleErrorCallback
    // Called when a session timeout is encountered while trying to do a background RPC.
    //
    // @param response (DSResponse) response the response
    // @param request (DSRequest) request the request
    //
    // @visibility sgwt
    //<
    _handleError : function (response, request) {
        if (response.ignoreError) return;

        // if this is a dsRequest, check for a handleError() method on the DataSource and call
        // that if it exists    
        if (request.dataSource) {
            var ds = isc.DataSource.get(request.dataSource);
            if (ds && ds.handleError) {
                // call the handler - if it returns false, suppress call to handleError
                var val = ds.handleError(response, request);
                if (val == false) return;
            }
        }
        return this.handleError(response, request);
    },
	handleError : function (response, request) {
		var context = (response.context ? response.context : {}),
            message;
		if (isc.isA.String(response.data)) {
			// this lets the developer show an arbitrary error message generated by the server,
            // for things like authorization code that a developer doesn't want to build
            // client-side
			message = response.data;
            if (isc.isA.String(message)) {
                var messageToShow = message;
                if (messageToShow.length > this.maxErrorMessageLength) {
                    var delta = messageToShow.length - this.maxErrorMessageLength;
                    messageToShow = messageToShow.substring(0, this.maxErrorMessageLength)
                                    + "<br><br>...("+delta+" bytes truncated - set"
                                    + " isc.RPCManager.maxErrorMessageLength > "
                                    + this.maxErrorMessageLength 
                                    + " to see more or check the Developer Console for full error)...";
                }
                this.reportError(messageToShow);
            }
		} else {
            // find the name of the constant for the reported error num, if there is one
            var codeName = isc.getKeyForValue(response.status, isc.RPCResponse.errorCodes);
            if (isc.isA.String(codeName)) { 
                if (codeName.startsWith("STATUS_")) codeName = codeName.substring(7);
            } else {
                codeName = (response.status != null ? "error code: " + response.status 
                                                    : "unknown error code");
            }
            var opName = response.operationId || response.operationType,
                data = request.data,
                message = "",
                extraText = "";

            // provide RPCDMI details if any
            if (data && data.is_ISC_RPC_DMI) {
                if (data.appID == "isc_builtin") {
                    message = "Builtin RPC: " + data.methodName + ": ";
                } else {
                    message = "RPCDMI: appId: '" + data.appID +
                               "', className: '" + data.className +
                              "', methodName: '" + data.methodName + "': ";
                }
            }
            
			if (codeName == "MAX_FILE_SIZE_EXCEEDED") {
				message += isc.DataSource.maxFileSizeExceededMessage.evalDynamicString(this, {
                        maxFileSize: response.maxFileSize,
                        uploadedFileName: response.uploadedFileName,
						uploadedFileSize: response.uploadedFileSize
                    });
                extraText = "\nSet rpcRequest.willHandleError:true on your request to handle this " +
                    "error yourself, or add a custom handleError to RPCManager to change " +
                    "system-wide default error reporting";
			} else if (codeName == "VALIDATION_ERROR") {
                message += "Server returned validation errors: " + isc.echoFull(response.errors);
                extraText = "\nSet rpcRequest.willHandleError:true on your request to handle this " +
                    "error yourself, or add a custom handleError to RPCManager to change " +
                    "system-wide default error reporting";
            } else {
                message += "Server returned " + codeName + " with no error message" +
                    (opName ? " performing operation '" + opName + "'." : ".");
            }

            this.reportError(message, response.stacktrace);

		}
        // log regardless
        this.logWarn(message + extraText + " - response: " + this.echo(response));
		
		// return false meaning don't proceed 
		return false;
	},
    
    //> @classMethod RPCManager.handleTransportError()
    // Method to handle server error responses to submitted transactions.
    // When the server responds to a submitted transaction with an HTTP error code this method will
    // be called before any individual response callbacks are fired, regardless of whether
    // +link{RPCRequest.willHandleError} was specified on the submitted request[s].
    // <P>
    // This provides the developer with an opportunity to handle a server error by (for example)
    // suspending and resubmitting the transaction before any other handling occurs.
    // <P>
    // The default implementation takes no action - by default transport errors 
    // are handled via +link{RPCManager.handleError()}, or by the standard request callback
    // methods, depending on request.willHandleError. 
    // To perform custom handing for transport errors this classMethod may be overridden as
    // follows
    // <pre>
    //     isc.RPCManager.addClassProperties({
    //         handleTransportError : function (transactionNum, status, httpResponseCode, httpResponseText) 
    //         {
    //                .. custom handling .. 
    //         }
    //     })
    // </pre>
    // <P>
    // Note: This method only applies to operations submitted via
    // +link{RPCTransport,XMLHttpRequest} - it is not possible to provide similar error handling
    // for other transports.
    //
    // @param transactionNum (int) The submitted client-server transaction number
    // @param status (integer) The RPCResponse status code
    // @param httpResponseCode (integer) The HTTP Response code reported by the server
    // @param httpResponseText (text) The raw HTTP Response text
    //
    // @group errorHandling
    // @visibility external
    //<
    

    //> @method Callbacks.HandleTransportErrorCallback
	// RPCManager transport error callback.
	//
    // @param transactionNum (int) The submitted client-server transaction number
    // @param status (int) The RPCResponse status code
    // @param httpResponseCode (int) The HTTP Response code reported by the server
    // @param httpResponseText (text) The raw HTTP Response text
	//
	// @visibility sgwt
	//<

    handleTransportError : function (transactionNum, status, httpResponseCode, httpResponseText) {
    },
	
	
//> @groupDef relogin
// 
// When a user's session has expired and the user tries to navigate to a protected resource,
// typical authentication systems will redirect the user to a login page.  With Ajax systems
// such as SmartClient, this attempted redirect may happen in response to background data
// operations such as a form trying to save.  In this case, the form perceives the login page
// as a malformed response and displays a warning, and the login page is never displayed to the
// user.
// <P>
// The ideal handling of this scenario is that the form's attempt to save is "suspended" while the
// user re-authenticates, then is completed normally.  SmartClient makes it easy to
// implement this ideal handling <i>without</i> having to implement session timeout handling in
// every codepath that contacts the server, by providing central notification of session timeout,
// and the ability to re-send a transaction that encountered session timeout.
// <P>
// <h3>Detecting session timeout</h3>
// <P>
// To enable SmartClient to detect that session timeout has occurred, a special marker needs to
// be added to the HTTP response that is sent when a user's session has timed out.  This is
// called the <code>loginRequiredMarker</code>.
// <P>
// If your authentication system will redirect to a login page when a user's session is timed
// out, it's sufficient to simply embed the <code>loginRequiredMarker</code> in the login page.  The
// <code>loginRequiredMarker</code> is valid HTML and will have no effect on the behavior or
// appearance of the page.  The <code>loginRequiredMarker</code> is found in
// <smartclient>smartclientSDK/isomorphic/login/loginRequiredMarker.html</smartclient>
// <smartgwt>docs/loginRequiredMarker.html</smartgwt>
// in your SDK.  Simply copy the contents of this file verbatim into your login page anywhere
// inside the &lt;body&gt; tag; it does not need to be customized in any way for your application.
// <P>
// If it's a problem to modify the login page (even with a marker that has no effect on
// appearance or behavior), see if you can configure your authentication system to return a
// special response specifically for background requests for data.  By default, when using the
// SmartClient Server Framework, all such requests go to the +link{RPCManager.actionURL} and
// include an HTTP query parameter "isc_rpc=1"; various authentication systems can be
// configured to detect these requests and handle them separately.  One approach is to simply
// copy loginRequiredMarker.html into your application in an area not protected by
// authentication and redirect to it when a background data request with an expired session is
// detected.
// <P>
// <h3>Handling session timeout</h3>
// <P>
// When SmartClient detects the <code>loginRequiredMarker</code>, the transaction that
// encountered session timeout is put on hold, and 
// <smartclient>+link{RPCManager.loginRequired()} is called.</smartclient>
// <smartgwt>the RPCManager LoginRequired event is raised.</smartgwt>
// At this point you have a few options:
// <ol>
// 
// <li> Leave the SmartClient application and take the user to the login page, by simply doing a
// <code>window.location.replace(<i>myLoginURL</i>)</code>, the simplest but least user friendly
// option.
// 
// <li> Open a new browser window that goes to your plain HTML login form (or offer a link that
// opens such a browser window), using a modal dialog in the application page that prompts the
// user to login before continuing, then re-send the intercepted transaction
// (+link{RPCManager.resendTransaction()} when the user indicates he has logged in.
// This is simple, does not drop context, but is not seamless.
// 
// <li> Use a SmartClient interface, typically a DynamicForm in a Window, to collect credentials,
// perform login as a background RPC, and on success re-send the intercepted transaction
// (+link{RPCManager.resendTransaction()}.  <smartclient>A complete example of this,
// which assumes an authentication system that can take credentials as HTTP POST params, is
// included in the SDK as isomorphic/login/reloginFlow.js.</smartclient>
// 
// </ol>
// <B>Authentication via background RPC form POST</B>
// <smartclient>
// <P>
// The approach shown in reloginFlow.js posts the credentials gathered from the user to
// +link{RPCManager.credentialsURL}.  To make this work with an authentication system that can
// accept credentials via HTTP POST:
// <ol>
// <li> set the RPCManager.credentialsURL to the URL where credentials should be POST'd
// <li> include reloginFlow.js in your page, modified, if necessary, so that the names of the
// USERNAME and PASSWORD params match what your authentication system uses
// <li> configure your authentication system to send back the loginSuccessMarker as part of a
// successful login response, and the loginRequiredMarker as part of a failed login response
// </ol>
// If your authentication system can accept POST'd credentials at any URL it protects, the last
// step may be as simple as configuring the loginSuccessMarker file itself as a protected
// resource (<code>isomorphic/login/loginSuccessMarker.html</code>).
// </smartclient>
// <smartgwt>

// <P>
// To relogin against any system that can accept credentials as an HTTP POST:
// <ol>
// <li> when the LoginRequired event is raised, show a login form in a modal dialog.  The
//      +link{isc.showLoginDialog,LoginWindow} component is a simple version of this, or you can create your own
// <li> when the user enters credentials, POST them using code like the following:
// <pre>
//    RPCRequest request = new RPCRequest();
//    request.setContainsCredentials(true);
//    request.setActionURL(credentialsURL);
//    request.setUseSimpleHttp(true);
//    request.setShowPrompt(false);
//    Map<String,String> params = new HashMap<String,String>();
//    // adjust parameter names to match your authentication system
//    params.put("j_username",<i>username</i>);
//    params.put("j_password",<i>password</i>);
//    request.setParams(params);
//    RPCManager.sendRequest(request,new RPCCallback(){
//        public void execute(RPCResponse response, Object rawData, RPCRequest request) {
//            if (response.getStatus() == RPCResponse.STATUS_SUCCESS) {
//                // get rid of login window
//                RPCManager.resendTransaction();
//            } else if (response.getStatus() == RPCResponse.STATUS_LOGIN_INCORRECT) {
//                // show an error in the login window
//            }
//        }
//    });
// </pre>
// <li> configure your authentication system to send back the loginSuccessMarker as part of a
// successful login response, and the loginRequiredMarker as part of a failed login response
// </ol>
// If your authentication system can accept POST'd credentials at any URL it protects, the last
// step may be as simple as configuring the <code>loginSuccessMarker</code> file itself as a
// protected resource.  The <code>loginSuccessMarker</code> is found in
// <code>docs/loginSuccessMarker.html</code> in your SDK.
// </smartgwt>
// <P>
// <B>Authentication via background SmartClient server RPC/DMI</B>
// <P>
// If you are using the SmartClient Java server and your authentication system allows you to mark
// a user as authenticated from Java, you can perform a normal RPC or DMI with the credentials
// gathered from the user and send back success or failure indications as normal RPC or DMI
// responses.  This can be useful if, in addition to logging in, you want to send back additional
// data.
// <P>
// <B>Advanced: concurrency</B>
// <P>
// If, after loginRequired() has fired and before the user has re-authenticated, you send
// additional RPCs to protected URLs, you will get additional loginRequired() notifications.  This
// may happen to applications that poll for data or periodically save without user action.  You
// may wish to avoid this by setting an application-specific flag to avoid firing requests during
// the relogin process.  However, you must ultimately either
// +link{RPCManager.resendTransaction(),resend} or +link{RPCManager.clearTransaction(),discard}
// every transaction for which loginRequired() fires, or you will have a memory leak due to
// suspended transactions.
// <P>
// Note also that there is no requirement that the relogin process blocks user interaction.
// Applications that access multiple services may choose to simply show an unobtrusive error
// indication such that the user can log back in at his leisure, or even log the user back in
// automatically.
//
// @title Relogin
// @visibility external
// @treeLocation Client Reference/RPC
//<



    // Relogin design:
    //  - NOTE: relogin is unlike other error codes that might be returned in an RPCResponse
    //    because the ideal handling is to process relogin and then proceed with the original
    //    request, transparently to the requesting code.  Since the requesting code ultimately
    //    receives a normal response, representing a login failure as an RPCResponse would mean
    //    two responses to the same request
    //  - two types of interceptor:
    //    - 1. server code is capable of adding a canned snippet to indicate login required
    //      - this is how the SmartClient server authenticator works
    //      - this covers SiteMinder-style interceptors that send back HTML pages 
    //    - 2. non-ISC backends that send WSDL, XML or JSON responses with error codes, which
    //        cannot be modified to return special snippets, hence must be interpreted by
    //        custom logic in the client-side DataSource layer 
    //    - NOTE: the above only covers detection scenarios - the other half of the problem is
    //      how to pull off reauthentication in the background with various types of
    //      authentication systems
    //
    // - SiteMinder-style intercept relogin flow: authentication system will send back an HTML
    //   page rather than a clean RPC response
    //   - a snippet added to the login page calls rpc.loginRequired
    //     - in xmlHttp this is detected by scanning the response.  In hiddenFrames comm the
    //       code executes automatically and calls RPC layer 
    //   - relogin flows:
    //     - case 1: login is possible via get/post of params or other background RPC
    //     - case 2: login is only possible via using the login form sent back by the
    //       interceptor, because eg it writes out a form with some state in it
    //   - for case 1, you could:
    //     1. use an ISC-based relogin form to gather credentials from the user
    //     2. relogin via RPC, either:
    //        a. as a normal RPC with the ISC server
    //        b. for non-ISC server: embed standardized response markers that ISC recognizes as
    //           success vs failure
    //          - NOTE: someone could roll their own success/failure detection here, but it's
    //            complicated to do so if you need hiddenFrames comm backCompat, and adds
    //            complexity to the explanation
    //     3. call resubmitTransaction() once relogin has been achieved
    //     .. or your could take the approach for case 2, which might be simpler
    //   - for case 2
    //     1. use either an ISC Window or a separate browser window to show the actual HTML
    //        login form sent by the interceptor.  This window (either kind) should be directed
    //        to isomorphic/login/loginSuccessMarker.html and this resource should be
    //        protected by the interceptor such that, after successful login,
    //        loginSuccessMarker.html is loaded
    //        - reloginSuccess.html has a snippet of code that calls rpc.loginSuccess, the
    //          default implementation being to resubmit transactions that were intercepted
    //
    // - DataSource error code-based relogin
    //   - need for relogin is indicated by some error code that can only be detected in the
    //     DataSource layer
    //   - NOTE: in eg DataSource.transformResponse() the appropriate spot to interpret error
    //     codes, we are still in a position to transparently resubmit an intercepted request:
    //     - the custom DS layer is responsible for firing the DBComponent-level callback, so
    //       can leave the DBComponent in limbo during relogin interaction
    //     - the RPC layer will not remove the prompt until the DS layer is done processing 
    //   - call suspendTransaction(dsResponse.transactionNum)
    //   - show UI and accomplish relogin as for SiteMinder case
    //
    //  Note: while we are handling relogin, eg, waiting for the user to type in credentials,
    //  further requests to the same protected resource will trigger repeated loginRequired()
    //  notifications.  This situation is fairly rare but could arise with eg, polling.
    //  We shouldn't try to handle this by blocking all RPCs because an application may consume
    //  multiple services and requiring login may not be an application-halting event (eg,
    //  multi-service IM client).  So we expect that in this case, the application developer
    //  either sets a flag to avoid doing more RPCs during relogin or handles multiple
    //  loginRequired() notifications.
    //  Note that, for the case of polling in particular, it is basically required that either
    //  polling stops or polling requests are cleared immediately from loginRequired(), as
    //  otherwise if the application hits session timeout with the user not around, multiple
    //  hours worth of polling requests might queue up before the user comes back.
    handleLoginRequired : function (transactionNum) {
        // in the default flow at the bottom of this method, we open a new browser window to
        // the login page - that'll call handleLoginRequired(), so ignore that call so we don't
        // loop
        if (this._iscReloginWindow && this._iscReloginWindow == transactionNum) return;

        var transaction = this.getTransaction(transactionNum);
    
        // bogus call - default login.html with a new window - e.g. password-protected
        // DevConsole
        if (transaction == null) return;

        

        transactionNum = transaction.transactionNum;

        // clear the timeout on the transaction to let the user deal with reauth without getting
        // a timeout - this is universally useful, so we do it by default.  If the user wants
        // to cancel the transaction due to the auth delay, he's still free do so in a custom
        // rpc.loginRequired() implementation
        this.clearTransactionTimeout(transaction);
    
        var rpcRequest = transaction.operations[0],
            rpcResponse = this.createRPCResponse(transaction, rpcRequest);

        this.logInfo("loginRequired for transaction: " + transactionNum +
                     (transaction.containsCredentials ? 
                      ", transaction containsCredentials" : ""));

        if (transaction.containsCredentials) {
            // the user used an RPC to attempt a login, but auth failed for some reason - don't
            // if a callback was set on the request, then the user is expected to be able to
            // handle the AUTH_REQUIRED state - otherwise we'll just call loginRequired()
            // again.
            if (rpcRequest.callback) {
                rpcResponse.status = isc.RPCResponse.STATUS_LOGIN_INCORRECT;
                this.fireReplyCallbacks(rpcRequest, rpcResponse);
    
                this.clearTransaction(transaction);
                return;
            }
            this.clearTransaction(transaction);
        }

        this.suspendTransaction(transaction);

        // A non-auth RPC was trapped by auth required. 
        if (this.loginRequired) {
            // user has specified a custom loginRequired function , call through
            rpcResponse.status = isc.RPCResponse.STATUS_LOGIN_REQUIRED;
            this.loginRequired(transactionNum, rpcRequest, rpcResponse);
            return;
        }

        // default implementation - just open a new window to the login page.  This is
        // guaranteed to work, but isn't the best solution since the user can just close the
        // login window and then be forced to reload the app
    
        // don't allow page to be cached
        var URL = this.addParamsToURL(this.credentialsURL, {ts:new Date().getTime()});
        this._iscReloginWindow = window.open(URL, this.loginWindowSettings);
    },
    
//> @classMethod RPCManager.loginRequired()
// Called when a session timeout is encountered while trying to do a background RPC.  See
// +link{group:relogin,Relogin}.
// <P>
// The transaction with the passed <code>transactionId</code> is suspended, and should either
// be +link{RPCManager.clearTransaction,cleared} or +link{RPCManager.resendTransaction,resent}
// after the user has been re-authenticated.  
// <P>
// The <code>rpcRequest</code> parameter can be used to determine whether the suspended
// transaction can simply be dropped (eg, it's periodic polling request).
// <P>
// The <code>rpcResponse</code> parameter has rpcResponse.data set to the raw text of the
// response that triggered <code>loginRequired()</code>.  Some very advanced relogin strategies
// may need to inspect the raw response to get information needed for re-authentication.
//
// @param transactionNum (int) id of the transaction
// @param rpcRequest (RPCRequest) first RPCRequest of the transaction
// @param rpcResponse (RPCResponse) RPCResponse containing the session timeout response that
//                                  caused loginRequired() to be invoked
// @group relogin
// @visibility external
//<

//> @method Callbacks.LoginRequiredCallback
// Called when a session timeout is encountered while trying to do a background RPC.
//
// @param transactionNum (int) id of the transaction
// @param rpcRequest (RPCRequest) first RPCRequest of the transaction
// @param rpcResponse (RPCResponse) RPCResponse containing the session timeout response that
//                                  caused loginRequired() to be invoked
//
// @visibility sgwt
//<
    
    // called by AuthenticationFilter when relogin succeeds (by way of normal
    // performOperationReply() and transaction.containsCredentials detection and by canned "dumb http
    // server" code snippet in reloginSuccess.html
    handleLoginSuccess : function (transactionNum) {
        var transaction = this.getTransaction(transactionNum);
        // transaction may be null if the user popped a visible iframe that targets
        // loginSuccess and is using that as the UI for the user instead of e.g. a SmartClient
        // form that makes login RPCs
        if (transaction && transaction.containsCredentials) {
            this.clearTransactionTimeout(transaction);
            var rpcRequest = transaction.operations[0];
            if (rpcRequest.callback) {
                var rpcResponse = this.createRPCResponse(transaction, rpcRequest, {
                    status: isc.RPCResponse.STATUS_SUCCESS
                });
                this.fireReplyCallbacks(rpcRequest, rpcResponse);
                this.clearTransaction(transaction);
                return;
            }
            this.clearTransaction(transaction);
        }

        if (this._iscReloginWindow) this._iscReloginWindow.close();
        if (this.loginSuccess && this.loginSuccess() === false) return;

        // default handling
        this.resendTransaction();

    },

    // called by AuthenticationFilter or by canned "dumb http server" code snippet in
    // maxLoginAttemptsExceeded.html when too many login attempts have been made
    handleMaxLoginAttemptsExceeded : function (transactionNum) {
        var transaction = this.getTransaction(transactionNum);
        // transaction may be null if the user popped a visible iframe that targets
        // loginSuccess and is using that as the UI for the user instead of e.g. a SmartClient
        // form that makes login RPCs
        if (transaction && transaction.containsCredentials) {
            this.clearTransactionTimeout(transaction);
            var rpcRequest = transaction.operations[0];
            if (rpcRequest.callback) {
                var rpcResponse = this.createRPCResponse(transaction, rpcRequest, {
                    status: isc.RPCResponse.STATUS_MAX_LOGIN_ATTEMPTS_EXCEEDED
                });
                this.fireReplyCallbacks(rpcRequest, rpcResponse);
                this.clearTransaction(transaction);
                return;
            }
            this.clearTransaction(transaction);
        }
    
        // default handling
        if (this._iscReloginWindow) this._iscReloginWindow.close();

        if (this.maxLoginAttemptsExceeded) this.maxLoginAttemptsExceeded();
        else {
            var message = "Max login attempts exceeded.";
            if (isc.warn) isc.warn(message);
            else alert(message);
        }
    },

    // HTML needed to continue event processing when child window is opened
    _$fireOpenerTimeoutsHTML: "<HTML><SCRIPT>" + "setInterval(function () {" +
        "window.opener.isc.Timer.firePendingTimeouts();},1);</SCRIPT></HTML>",

    //> @classMethod RPCManager.exportContent()
    // Converts +link{canvas.getPrintHTML,printable HTML} generated from live UI components
    // into a .pdf and downloads it ("Save As.." dialog).
    // <P>
    // For +link{DrawPane} and subclasses (e.g. +link{FacetChart}) to export properly, the canvas
    // parameter must be the widget itself, not the HTML obtained with +link{canvas.getPrintHTML(),
    // getPrintHTML()} unless the +link{PrintProperties} passed to getPrintHTML() had
    // +link{PrintProperties.printForExport,printForExport}:true.
    // <P>
    // You can use a custom skin when exporting your HTML content. To use a custom skin,
    // add a line to +link{group:server_properties,server.properties}:
    // <pre>
    //   skin.{skinName}.location: custom/skin
    // </pre>
    // Where {skinName} is the name of your custom skin, and the value is the path to your 
    // skin resources from the application webroot.
    // <P>
    // Requires the SmartClient server framework, but does not require use of server-based
    // databinding - no .ds.xml files need to exist.
    // <P> 
    // See server-side docs for com.isomorphic.contentexport.PdfExport for more details on
    // server-side processing and code samples for redirecting PDF output to a file or
    // in-memory buffer, as well as instructions for adding additional stylesheets.
    // <p>
    // You can also inject a small amount of CSS from the browser via
    // +link{dsRequest.exportCSS} - this is intended primarily for switching the page size on
    // the fly, for exceptionally wide or tall exports.
    //
    // @param canvas (Canvas | Array[] of Canvas | HTMLString) Canvas or canvas list that has exportable widgets,
    //                    or HTML fragment derived from +link{canvas.getPrintHTML(),getPrintHTML()}
    // @param [requestProperties] (DSRequest properties) Request properties for the export to pdf object
    // @example pdfExportCharts
    // @visibility external
    //<
    
    exportContent : function (canvas, requestProperties) {
        if (requestProperties == null) requestProperties = {};

        var defaultSkinName = isc.Page.getSkinDir(),
            skinName = defaultSkinName.trim("/").split("/").last();

        // define the settings to pass to getPdfObject
        var settings = {
            pdfName: requestProperties.exportFilename || 
                     requestProperties.pdfName || "export",
            skinName: requestProperties.skinName || skinName,
            exportCSS: requestProperties.exportCSS,
            defaultSkinName: defaultSkinName
        };

        // define RPCRequest properties
        var serverProps = isc.addProperties({
            showPrompt: false,
            transport: "hiddenFrame",
            exportResults: true,
            downloadResult: true,
            downloadToNewWindow: false,
            download_filename: null
        }, requestProperties);

        // remove settings-specific properties
        delete serverProps.pdfName;
        delete serverProps.skinName;
        delete serverProps.exportCSS;

        
        if (isc.Browser.isMobileSafari && serverProps.downloadToNewWindow == true) {
            var windowName = serverProps.newDownloadWindow = 
                serverProps.download_filename || "request_" + isc.timeStamp();
            window.open(null, windowName).document.write(this._$fireOpenerTimeoutsHTML);
        }

        // generate the print HTML and request PDF from server
        var callback = function (html) {
            isc.DMI.callBuiltin({
                methodName: "getPdfObject",
                arguments: [ html, settings ],
                requestParams: serverProps
            });
        };
        var HTML = isc.Canvas.getPrintHTML(canvas, {
            printForExport: true
        }, callback);
        
        
        //if (HTML != null) {
        //    callback(HTML);
        //}
    },

    //> @classMethod RPCManager.exportImage()
    // Converts an +link{drawPane.getSvgString,SVG string} to one of several possible image formats,
    // and can either initiate a download or return the base64-encoded image data.
    // <p>
    // Control the image format via +link{dsRequest.exportImageFormat}.
    // <p>
    // Default is to download the image (triggering the browser's save dialog).
    // +link{dsRequest.exportFilename} can be used to control the default filename provided in the
    // save dialog.
    // <p>
    // To instead return the data as a normal DSResponse, set +link{dsRequest.exportDisplay} to
    // "return".  In this case the data is always base64 encoded.
    // <p>
    // Requires the SmartClient server framework, with the same set of
    // +link{group:javaModuleDependencies,required .jars} as are required for PDF export of charts in
    // legacy IE.  
    // <p>
    // See also +link{drawPane.getSvgString()} and +link{drawPane.getDataURL()}. 
    //
    // @param svgString (String) XML string containing SVG data
    // @param [requestProperties] (DSRequest Properties) request properties controlling options for export
    // @param [callback] (ExportImageCallback) optional callback when using 
    //                                         <code>exportDisplay</code>:"return".  <b>Does not
    //                                         fire</b> for other <code>exportDisplay</code> modes
    // @example chartImageExport
    // @visibility external
    //<
    _$return: "return",
    exportImage : function (svgString, requestProperties, callback) {
        var props = requestProperties || {},
            returnImageData = (props.exportDisplay != this._$return);
        var serverProps = {
            downloadResult: returnImageData
        };
        isc.DMI.callBuiltin({
            methodName: "exportImage",
            arguments: [ svgString, requestProperties ],
            requestParams: serverProps,
            callback : function _handleExportImageReply(rpcResponse, data, rpcRequest) {
                if (callback) {
                    isc.Class.fireCallback(callback, "imageData", [data.base64]);
                }
            }
        });
    },

    //> @method Callbacks.ExportImageCallback
    // Callback for +link{RPCManager.exportImage}.
    // @param imageData (String) image data from the server, in base64 format
    // @visibility external
    //<

    // API to report errors for RPC requests/responses
    reportError : function (errorMessage) {
        isc.warn(errorMessage.asHTML());
    },
    // transformResponse API, similar to the one for DataSource
    transformResponse : function (rpcResponse, rpcRequest, data) {
        return rpcResponse;
    },
    // transformRequest API, similar to the one for DataSource               
    transformRequest : function (rpcRequest) {
        return rpcRequest.data;
    },

    //> @classMethod RPCManager.loadScreen()
    // Loads a screen saved in +link{group:componentXML,Component XML} format, using the
    // +link{group:servletDetails,ScreenLoaderServlet}.
    // <P>
    // The ScreenLoaderServlet will look for a file named <i>screenName</i>.ui.xml in 
    // the directory given by the "project.ui" setting, which defaults
    // <i>webroot</i>/shared/ui and can be configured in 
    // +link{group:server_properties,server.properties}.
    // <p>
    // The <code>screen</code> provided by the callback will be the outermost component if your
    // loaded screen consists of a hierarchy of widgets all contained under one parent (which
    // is true of any screens created in Visual Builder).  
    // <p>
    // If you have multiple widget hierarchies in your screen, the <code>screen</code> returned
    // will be the last top-level component created.
    // <P>
    // By default, components in the loaded screens that have +link{Canvas.ID,global IDs} will not
    // actually be allowed to take those global IDs - instead, only widgets that have one of the
    // global IDs passed as the <code>globals</code> parameter will actually receive their global
    // IDs.  To override this behavior, pass the special value +link{RPCManager.ALL_GLOBALS}
    // for the <code>globals</code> parameter.
    // <p>
    // When globals are being suppressed, the <code>screen</code> available in the callback
    // will provide access to widgets that did not receive their global IDs via
    // +link{canvas.getByLocalId()}, and the <code>suppressedGlobals</code> available in the
    // callback will be a mapping from suppressed global ID to the widget or other component
    // that would have used that global ID if globals were not suppressed.  In addition, any
    // other <code>Canvas</code> loaded with the screen also provides access to any suppressed
    // globals from the screen via <code>getByLocalId()</code>.
    // <p>
    // To load multiple screens at once, use +link{cacheScreens()} and +link{createScreen()}
    // instead.
    // <P>
    // Components in the screen will default to having +link{canvas.autoDraw} set to false.
    // This may be overridden by setting the +link{RPCRequest.suppressAutoDraw} attribute
    // explicitly to <code>false</code> on the request properties object.
    // <P>
    // You can optionally provide a locale name to use when resolving any i18n tags in the 
    // screen's component XML.  If you do not supply this, the locale will be derived from 
    // the servlet API, and so will generally be a locale appropriate to the client's operating
    // system settings.  Only provide a locale manually if you have a special requirement that 
    // requires the user's operating system locale to be overridden in your application.  If 
    // you provide a locale name, it should be of the form "xx" or "xx_YY", where "xx" is a 
    // valid language code and "YY" is a valid country code.  For example, "fr" or "en_GB".
    // <P> 
    // This API assumes the ScreenLoaderServlet is installed at the default location - to use a
    // different location, use the <code>requestProperties</code> parameter to specify a different
    // URL via +link{rpcRequest.actionURL}.  The <code>requestProperties</code> parameter can also
    // be used to pass additional params to a custom ScreenLoaderServlet - see the "Dynamic
    // Component XML" section of the +link{group:componentXML,Component XML overview}.
    //
    // @param screenName (String) name of the screen to load
    // @param callback (LoadScreenCallback) callback for notification of screen being loaded
    // @param [globals] (Array of String) widgets to allow to take their global IDs
    // @param [locale] (String) The name of a locale to use for resolving i18n tags in the 
    //        component XML of the screen
    // @param [requestProperties] (RPCRequest Properties) optional properties for the request
    //
    // @visibility external
    //<
    loadScreen : function (screenName, callback, globals, locale, requestProperties) {
        if (!screenName) {
            this.logWarn("No screen names passed in.");
            this.fireCallback(callback, "data", [null]);
            return;
        }
        // Note: this logic will handle multiple screens, but this is intentionally not
        // documented and not supported.  The right way to load multiple screens is to use
        // RPCManager.cacheScreens().
        if (!isc.isAn.Array(screenName)) screenName = [screenName];
        if (screenName.length <= 0) {
            this.logWarn("No screen names passed in.");
            this.fireCallback(callback, "data", [null]);
            return;
        }
        if (!globals) globals = [];
        if (!isc.isAn.Array(globals)) globals = [globals];
        var request = {};
        if (requestProperties) isc.addProperties(request, requestProperties);
        if (request.params == null) request.params = {};
        request.params.screenName = screenName.join(",");
        if (locale) {
            isc.addProperties(request.params, {locale: locale});
        }
        if (!request.actionURL) request.actionURL = this.screenLoaderURL;
        request.useSimpleHttp = true;

        var _this = this;
        request.callback = function (rpcResponse, data, rpcRequest) {
            _this._makeScreen(rpcResponse, data, rpcRequest, callback, globals);
        };
        this.sendRequest(request);
    },

    //> @classMethod RPCManager.createScreen()
    // Creates a screen previously cached by a call to +link{cacheScreens()}.
    // <p>
    // As with +link{loadScreen()}, the default behavior is to prevent any global widget IDs from
    // being established, the returned Canvas will be the outermost component of the screen,
    // and that Canvas will provide access to other widgets in the screen via +link{canvas.getByLocalId(),getByLocalId()}
    // <p>
    // Alternatively, as with +link{loadScreen()}, a list of IDs that should be allowed to become
    // globals can be passed, allowing those widgets to be retrieved via a call to
    // +link{Canvas.getById()} after the screen has been created.
    // <p>
    // If you do not pass <code>globals</code> and avoid depending on global IDs within the screen
    // definition itself (for example, by embedding JavaScript event handlers in the screen definition
    // that use global IDs), you can create the same screen multiple times.
    // 
    // @param screenName (String) name of the screen to create
    // @param [globals] (Array of String) widgets to allow to take their global IDs
    // @return (Canvas) last top-level widget in the screen definition
    //
    // @visibility external
    //<
    createScreen : function(screenName, globals) {
        if (!screenName) {
            this.logWarn("No screen names passed in.");
            return null;
        }

        if (!globals) globals = [];

        if (!isc.isAn.Array(globals)) globals = [globals];

        var _this = this;
        var data = _this._cachedScreens[screenName];

        if (!data) { return null;}

        return this._makeScreen(null, data, null, null, globals);
    },

    _makeScreen : function(rpcResponse, data, rpcRequest, callback, globals) {
        if (!rpcRequest) { rpcRequest = {} };        

        var origAutoDraw = isc.Canvas.getInstanceProperty("autoDraw"),
            suppressAutoDraw = rpcRequest.suppressAutoDraw == null ? true : rpcRequest.suppressAutoDraw;
        if (suppressAutoDraw) isc.Canvas.setInstanceProperty("autoDraw", false);

        var _this = this; 
        if (globals.length == 1 && globals[0] == _this.ALL_GLOBALS) {
            var result = isc.Class.globalEvalWithCapture(data, function (globals, error) {
                if (error != null) isc.Log._reportJSError(error, null, null, null, 
                                                  "Error when executing loaded screen");
                // get the top level component
                var _screen = isc.Canvas._getTopLevelWidget(globals);
                // globalEvalWithCapture does not set suppressedGlobals in the fired callback. Should we set it to empty?
                var suppressedGlobals = {};
                
                // restore autoDraw here, to be on safe side if error occurs in the callback
                if (suppressAutoDraw) isc.Canvas.setInstanceProperty("autoDraw", origAutoDraw);

                _this.fireCallback(callback, "screen,rpcResponse,suppressedGlobals", 
                    [_screen, rpcResponse, suppressedGlobals]);
            }, null, false);

            var _screen = isc.Canvas._getTopLevelWidget(result.globals);
            return _screen;
        } else {
            var result =  isc.Class.globalEvalAndRestore(data, globals, function (globals, error, suppressedGlobals) {
                if (error != null) isc.Log._reportJSError(error, null, null, null, 
                                                  "Error when executing loaded screen");
                // get top level view
                var _screen = isc.Canvas._getTopLevelWidget(globals);

                // restore autoDraw here, to be on safe side if error occurs in the callback
                if (suppressAutoDraw) isc.Canvas.setInstanceProperty("autoDraw", origAutoDraw);

                // filter supressed globals so only allowed items get through
                // this is required to prevent exception on load when there are other elements, 
                // like DataSource definitions in the ui.xml file.
                var tmp = {};
                var keys = isc.getKeys(suppressedGlobals)
                for (var i = 0; i < keys.length; i++) {
                    var global = keys[i];
                    var obj = window[global]; // globals are IDs, dereference

                    if (obj && (isc.isA.Canvas(obj) || isc.isA.FormItem(obj)) ) {
                        tmp[global] = obj;
                    }
                }

                suppressedGlobals = tmp;

                _this.fireCallback(callback, "screen,rpcResponse,suppressedGlobals", 
                    [_screen, rpcResponse, suppressedGlobals]);
            }, null, false, true);

            // get top level view
            var _screen = isc.Canvas._getTopLevelWidget(result.globals);

            return _screen;
        }
    },

    //> @classMethod RPCManager.cacheScreens()
    // Loads the definitions of a set of screens saved in +link{group:componentXML,Component XML}
    // format, using the +link{group:servletDetails,ScreenLoaderServlet}.
    // <p>
    // Unlike +link{loadScreen()}, <code>cacheScreens()</code> does not cause any UI components to be
    // created or drawn, it just loads the definitions of the screens.  This allows a subsequent,
    // synchronous call to +link{createScreen()} to create the actual screen, rather than
    // contacting the <code>ScreenLoader</code> servlet and showing a loading message.
    // <p>
    // See +link{loadScreen()} for the meaning of the <code>locale</code> parameter.
    // <p>
    // Calling <code>cacheScreens</code> twice with the same screenName will re-load the definition of
    // that screen from the server such that subsequent calls to <code>createScreen()</code> will use
    // the new definition.
    // 
    // @param screenName (Array of String) name of the screens to cache
    // @param callback (Function) callback for notification of screens being successfully cached
    // @param [locale] (String) The name of a locale to use for resolving i18n tags in the 
    //                         component XML of the screen
    // @param [requestProperties] (RPCRequest Properties) optional properties for the request
    //
    // @visibility external
    //<
    cacheScreens : function(screenName, callback, locale, requestProperties) {
        if (!screenName) {
            this.logWarn("No screen names passed in.");
            this.fireCallback(callback, "data", [null]);
            return;
        }
        // Note: this logic will handle multiple screens, but this is intentionally not
        // documented and not supported.  The right way to load multiple screens is to use
        // RPCManager.cacheScreens().
        if (!isc.isAn.Array(screenName)) screenName = [screenName];
        if (screenName.length <= 0) {
            this.logWarn("No screen names passed in.");
            this.fireCallback(callback, "data", [null]);
            return;
        }

        var request = {};
        if (requestProperties) isc.addProperties(request, requestProperties);
        request.params = {screenName: screenName.join(",")};
        if (locale) {
            isc.addProperties(request.params, {locale: locale});
        }
        isc.addProperties(request.params, {structuredResponse:true});

        if (!request.actionURL) request.actionURL = this.screenLoaderURL;
        request.useSimpleHttp = true;
        var _this = this;

        request.callback = function (rpcResponse, data, rpcRequest) {
        
            isc.Class.globalEvalWithCapture(data, function (globals, error) {
                if (error != null) isc.Log._reportJSError(error, null, null, null, 
                                                  "Error when executing cache screen");
                if (!_this._cachedScreens) {
                    _this._cachedScreens = {};
                }

                var json = isc.Class.evaluate(data);
                for (var i=0;i<json.length;i++) {
                    if (_this._cachedScreens[json[i].screenName]) {
                        _this.logWarn("Screen " + json[i].screenName + " is already cached. Replacing.");
                    }
                    _this._cachedScreens[json[i].screenName] = json[i].source;
                }

                _this.fireCallback(callback, "data,rpcResponse", [json, rpcResponse]);
            }, null, false);
        };

        this.sendRequest(request);
    },

    //> @classMethod RPCManager.requestsArePending()
    // Returns whether there are any pending RPC requests.
    // <P>
    // @return (Boolean) true if one or more RPC requests are pending, false otherwise.
    // @visibility external
    //<
    requestsArePending : function () {
        return this.pendingRpcs != 0;
    }

});
// patch RPCManager logging to truncate anything abot maxLogMessageLength with a warning and
// allow a second category to enable full logs.  We do this to prevent accidentall logging of
// large responses such as roundripping a WSDL file or toJS.
isc.RPCManager.rpc_logMessage = isc.RPCManager.logMessage;
isc.RPCManager.logMessage = function (priority, message, category, timestamp) {
    if (this.logIsEnabledFor(priority, category)) {
        if (isc.isA.String(message) && message.length > this.maxLogMessageLength
            && !this.logIsEnabledFor(priority, "RPCManagerResponse")) 
        {
            var delta = message.length - this.maxLogMessageLength;
            message = message.substring(0, this.maxLogMessageLength)
                +"\n...("+delta+" bytes truncated).  Enable RPCManagerResponse log at same threshold to see full message."
        }
    }
    this.rpc_logMessage(priority, message, category, timestamp);
};

//>	@class	InstantDataApp
// An InstantDataApp provides the ability to send data to an ISC server and retrieve results, and
// also to determine what users are authorized to perform what operations.
// <br><br>
// All server contact in the ISC system goes through InstantDataApp.  In higher-level APIs, such as
// the component databinding methods, the InstantDataApp is not directly visible because of the
// concept of the "default application", however, all such methods boil down to the
// <code>performOperation()</code> call, which allows arbitrary data to be sent and retrieved.
// <br><br>
// An InstantDataApp is created by loading an ISC Application File (.app.xml), which is read by both
// the ISC server and ISC client system.  The Application File allows you to specify the server-side
// Class which should handle operations submitted by the client, so that you can write custom
// operations. 
// 
// @see classMethod:InstantDataApp.getDefaultApplication()
// @group operations
// @treeLocation Client Reference/Data Binding
// @visibility ida
//<
isc.addGlobal("InstantDataApp", isc.RPCManager);
isc.isA.InstantDataApp = isc.isA.RPCManager; // make isA.InstantDataApp(app) work

isc.InstantDataApp.addClassMethods({

	// Operations
	// ----------------------------------------------------------------------------------------

    // legacy compatibility for performOperation(): ensures a client-side operation definition
    // exists for the operation name, because performOperation() needs one to exist, since for
    // DataSource operations the parameters to performOperation are insufficient to give
    // dataSource name and operationType
    addDefaultOperation : function (context, dataSource, operationType) {
		if (!context) context = {};
        
        context.operation = isc.DataSource.makeDefaultOperation(dataSource, operationType,
                                                                context.operation);

		return context;
    },

    
	// Default Application handling
	// --------------------------------------------------------------------------------------------
	setDefaultApplication : function (defaultApplication) {
		isc.InstantDataApp.defaultApplication = defaultApplication;

        
	},
 
    //> @classMethod RPCManager.getDefaultApplication()
    // Get the default application.
    // <br><br>
    // The default application is the first RPCManager created in a given page, or if no
    // RPCManagers are created, an automatically generated RPCManager with the ID
    // "builtinApplication".
    // <br><br>
    // The default application will be used any time an operation needs to be performed and no
    // application has been explicitly specified.  For example, a databound ListGrid is asked to
    // fetchData() and no application is passed.
    // <br><br>
    // The special automatically-generated "builtinApplication" is for rapid prototyping; it allows
    // you to perform the built-in operations against any DataSource, so that you can prototype a
    // complete client-server application without writing any server code and without writing an ISC
    // Application File (.app.xml file).
    // 
    // @return (RPCManager) the default application
    // @visibility internal
    //<
	getDefaultApplication : function () {
        // If the default application has never been created or has been destroyed we need to
        // create a new one.
		if (this.defaultApplication == null) {
            // create a barebones default application, which will automatically register itself
            // as the default application
            this.create({
                ID:"builtinApplication",
				dataSources:[],
                operations:{},
                // Ensure that should the default app get destroy()d, our pointer to it gets
                // cleared
                pointersToThis:[{object:this, property:"defaultApplication"}]
			});
		}
		return this.defaultApplication;
	},
    app : function () { return this.getDefaultApplication(); }
});

isc.InstantDataApp.addMethods({

	init : function () {
		// Create a global ID for this application so we can refer to it in the global scope.
		// If the app already has an ID property, this will be used as its global ID.
		if (this.ID != "builtinApplication") isc.ClassFactory.addGlobalID(this);

        // if there's no current default application, or the auto-generated
        // "builtinApplication" has been set as the default application, replace it
		if (isc.rpc.defaultApplication == null || 
            isc.rpc.defaultApplication.getID() == "builtinApplication") 
        {
			isc.rpc.setDefaultApplication(this);
		}
	}

});


// hooks into RPCManager and provides notifications of queue status
isc.defineInterface("IRPCStatusListener").addInterfaceProperties({

initInterface : function () {
    this.observe(isc.RPCManager._transactions, "dataChanged", "observer.transactionsChanged()");
},

destroyInterface : function () {
    this.ignore(isc.RPCManager._transactions, "dataChanged");
},

transactionsChanged : function () {

},

getActiveTransactions : function () {
    var activeTransactions = [];
    var transactions = isc.RPCManager.getTransactions();

    for (var i = 0; i < transactions.length; i++) {
        var transaction = transactions[i];
        // XXX also exclude transaction._clearOnTimeout?  But the response from those would be processed...
        if (transaction && !transaction.cleared && transaction.operations) {
            var abortableTransaction = true;
            for (var j = 0; j < transaction.operations.length; j++) {
                var operation = transaction.operations[j];
                if (operation.downloadResult || operation._returnStreamFileURL) {
                    abortableTransaction = false;
                    break;
                }
            }
            if (abortableTransaction) activeTransactions.add(transaction);
        }
    }
    return activeTransactions;
},

haveActiveTransactions : function () {
    return this.getActiveTransactions().length > 0;
}
  
});


isc.defineInterface("IRPCUnmaskedTarget").addInterfaceProperties({

initInterface : function () {
    isc.RPCManager.registerUnmaskedTarget(this);
},

destroyInterface : function () {
    isc.RPCManager.unregisterUnmaskedTarget(this);
}

});

 

isc.defineClass("UnmaskedTargetLandingPad", "Canvas").addProperties({
    
overflow: "visible",
height: 1,
width: 1,
observationHooks: ["draw", "clear", "moved", "parentMoved", "parentResized", "parentVisibilityChanged"],

initWidget : function () {
    this.Super("initWidget", arguments);
    for (var i = 0; i < this.observationHooks.length; i++) {
        this.observe(this, this.observationHooks[i], "observer.manageUnmaskedTarget()");
    }    
    this.setUnmaskedTarget(this.unmaskedTarget);
},

setUnmaskedTarget : function (unmaskedTarget) {
    this.unmaskedTarget = unmaskedTarget;
    if (this.unmaskedTarget) {
        this.observe(this.unmaskedTarget, "resized", "observer.manageUnmaskedTarget()");
    }
},

manageUnmaskedTarget : function () {
    if (!this.unmaskedTarget) return;

    if (this.isVisible() && this.isDrawn()) {
        // Note update our width/height first before repositioning the unmaskedTarget because
        // we are likely to be contained whereas the unmaskedTarget is not, so when it changes
        // size it overflows to the right whereas we may need to overflow to the left
        if (this.getVisibleWidth() < this.unmaskedTarget.getVisibleWidth()) this.setWidth(this.unmaskedTarget.getVisibleWidth());
        if (this.getVisibleHeight() < this.unmaskedTarget.getVisibleHeight()) this.setHeight(this.unmaskedTarget.getVisibleHeight());
        
        // record page-level coordinates before reparent       
        var left = this.getPageLeft(),
            top = this.getPageTop();

        this.unmaskedTarget.moveTo(left, top);
        this.unmaskedTarget.bringToFront();
        this.unmaskedTarget.show();
    } else {
        this.unmaskedTarget.hide();
    }
},

destroy : function () {
    for (var i = 0; i < this.obsevationHooks.length; i++) {
        this.ignore(this, this.observationHooks[i]);
    }
    // for the resized event, watch the unmaskedTarget and apply the size change to ourselves.
    // The unmaskedTarget is the one that can overflow due to e.g. prompt changes whereas the
    // placeholder (this class) doesn't have any content and just needs to expand to fill space
    // under the unmaskedTarget
    if (this.unmaskedTarget) this.ignore(this.unmaskedTarget, "resized");

    this.Super("destroy", arguments);
}

});


isc.defineClass("RPCStatusControl", "UnmaskedTargetLandingPad").addProperties({

statusIndicatorDefaults: {
    _constructor: "RPCStatusMenuButton"
},

initWidget : function () {
    this.Super("initWidget", arguments);

    this.statusIndicator = this.createAutoChild("statusIndicator", {
        canCancelQueue: this.canCancelQueue
    });
    this.setUnmaskedTarget(this.statusIndicator);
}    

});


if (isc.MenuButton) {

isc.defineClass("RPCStatusMenuButton", "MenuButton", ["IRPCStatusListener", "IRPCUnmaskedTarget"]).addProperties({

width: 40,
height: 20,

defaultPrompt: "Idle",

progressImage: "[SKINIMG]loadingSmall.gif",
progressImageWidth: 16,
progressImageHeight: 16,

noProgressImage: "[SKINIMG]loadingSmallFrozen.gif",
noProgressImageWidth: 16,
noProgressImageHeight: 16,

canCancelQueue: false,
confirmCancelQueue: true,
cancelQueueConfirmationMessage: "Cancelling a request may have unintended consequences. "
                                + " You should only do this if you requested data that is taking"
                                + " too long to load, but not if you are saving data.<br><br>"
                                + " Are you sure you want to cancel this request?",

cancelQueue : function () {
    var activeTransactions = this.getActiveTransactions();
    for (var i = 0; i < activeTransactions.length; i++) {
        this.logWarn("cancel queue: " + i + " with value: " + isc.echoFull(activeTransactions[i].transactionNum));
        isc.RPCManager.cancelQueue(activeTransactions[i]);
    }
},

initWidget : function () {
    this.Super("initWidget", arguments);

    var menuItems = [];
    menuItems.add({title: isc.RPCManager.defaultPrompt, isPrompt: true, enabled: false});
    if (this.canCancelQueue) {
        var _this = this;
        menuItems.add({title: "Cancel all requests", icon: "[SKINIMG]actions/close.png", click : function () {
            if (_this.confirmCancelQueue) {
                isc.confirm(_this.cancelQueueConfirmationMessage, function (yes) {
                    if (yes) _this.cancelQueue();
                });
            } else {
                _this.cancelQueue();
            }
        }});      
    }
    this.menu = isc.Menu.create({
        data: menuItems
    });
    this.menuButton = this.createAutoChild("menuButton", {
        menu: this.menu
    });
    this.updateState();
    
    this.addAutoChildren(this.autoChildren);
},

updateState : function () {
    var activeTransactions = this.getActiveTransactions();

    var lastTransaction = isc.RPCManager.getTransaction(activeTransactions[activeTransactions.length-1]);

    var title = "";
    var promptMenuItem = this.menu.data.find("isPrompt", true);
    if (lastTransaction) {  
        title += isc.Canvas.getImgHTML(this.progressImage, this.progresImageWidth, this.progressImageHeight);

        if (promptMenuItem) this.menu.setItemTitle(promptMenuItem, lastTransaction.prompt);

        this.setDisabled(false);
    } else {
        title += isc.Canvas.getImgHTML(this.noProgressImage, this.noProgresImageWidth, this.noProgressImageHeight);

        if (promptMenuItem) this.menu.setItemTitle(promptMenuItem, this.defaultPrompt);

        this.setDisabled(true);
    }   
    this.setTitle(title);
},

transactionsChanged : function () {
    this.updateState();
}

});
}

//> @groupDef operations
// SmartClient Operations are dynamic, transparent communications made from the client-side
// SmartClient system running in the browser, to the server-side SmartClient system running in
// a servlet engine, or to other non-SmartClient servers available via HTTP. Operations are
// used to load new data or new behavior into a running SmartClient application. Operations are
// also used to save data entered by users, and in general, to get the result of any process
// which must be run on the server for security reasons.
// <br><br>
//
// <b>RPC Operations</b>
// <br><br>
// RPC Operations are low-level communications that send and retrieve arbitrary data.  RPC
// Operations are supported by the +link{class:RPCManager} class, which when used with the
// SmartClient server, provides Java to JavaScript +link{rpcRequest.data,2-way translation} of
// basic data structures.  The RPCManager also provides a mechanism for client-side code to be
// invoked when an operation completes (called a "callback").  RPC Operations are intended for
// unstructured data; data that is ultimately destined for display in SmartClient components
// will generally come from DataSource operations.
// <br><br>
//
// <b>DataSource Operations and DataBound Components</b>
// <br><br>
// A +link{group:dataSourceOperations,DataSource Operation} is an operation that acts on a
// DataSource, performing one of the basic actions that makes sense on a set of similar
// records: "fetch", "add", "update" or "remove".  Unlike RPC operations, DataSource operations
// have specific request data and response data, for example, in the "fetch" DataSource
// operation, the request data is expected to be search criteria, and the response data is
// expected to be a list of matching DataSource records.  Although DataSource operations can be
// invoked manually from the client, they are generally automatically invoked by DataBound
// components.
// <br><br>
// DataBound Components are components that understand DataSources.  Databound components
// configured with a DataSource are able to offer complete user interactions without further
// configuration (extensive customization is also supported).
// <br><br>
// For example, given a DataSource, the ListGrid component supports a sophisticated inline
// editing interaction, complete with automatically chosen editors like date pickers for dates,
// type-aware validation, saving, and error reporting. 
// <br><br>
// A DataBound component supporting an interaction such as inline editing will automatically
// submit DataSource operations to the server at appropriate times.
// <br><br>
//
// <b>DataSource Operation Integration</b>
// <br><br>
// Integrating DataSource operations with an existing system is best approached by implementing
// the the 4 basic DataSource operations in terms of your existing object model or data store.
// With these 4 operations implemented, the entire range of user interactions supported by
// SmartClient +link{dataBoundComponent,databinding-capable components} becomes applicable to
// your server.  At that point authentication, authorization and other business rules can be
// layered on top.
// <br><br>
//
// <b>Built-in SQL Connectivity</b>
// <br><br>
// The SmartClient Server comes with a built-in +link{group:sqlDataSource,SQLDataSource}
// which can be used without any server-side code needing to be written.  In contrast,
// any operation which uses custom server-side code is called a "Custom Operation".
// <br><br>
// Generally it makes sense to prototype an application using Built-in DataSource Operations,
// then on the backend, create Custom DataSource Operations to retrieve data from the data
// store you will use in production (though don't rule out using the SQL DataSource in 
// production - see +link{group:sqlVsJPA,this discussion} of the advantages of doing so}.  
// As you switch from using Built-in DataSources to Custom Operations, no client-side code 
// changes will be required, because the client cares only about the DataSource definition, 
// not the data store which the data is ultimately retrieved from.
// <br><br>
//
// <b>Data Managers: ResultSet and ResultTree</b>
// <br><br>
// Data Managers manage datasets retrieved from DataSources.  Data Managers are automatically
// created by DataBound components, but can be created directly when more control is needed.
// <br><br>
// Data Managers provide load-on-demand for datasets too large to be loaded on the client,
// automatically invoking DataSource operations as necessary to retrieve data as it is
// requested, and optionally fetching ahead to anticipate further requests.   Data Managers
// will automatically perform actions locally when it is possible, for example, a sort
// action can be performed locally with a complete cache.  Data Managers also automatically
// manage the consistency of the client-side cache, observing update operations performed
// against DataSources and integrating updated rows automatically.
// <br><br>
// 
//
// @see class:RPCManager for RPC Operations
// @see interface:DataBoundComponent for information on DataBound Components
// @see group:dataSourceOperations for more information on DataSource Operations
// @see group:clientServerIntegration for information on integrating DataSource Operations with existing servers
// @see method:DataSource.fetchData() for manually invoked DataSource operations
// @see class:ResultSet for managing lists of records
// @see class:ResultTree for managing trees of records
// 
// @title Operations Overview
// @treeLocation Client Reference/Data Binding
// @visibility external
//<

//> @groupDef dataSourceOperations
// A DataSource Operation is a type of +link{group:operations,operation} that acts on the set
// of stored objects represented by a +link{DataSource}, performing one of the basic actions
// that makes sense on a set of similar records: "fetch", "add", "update" or "remove".  There
// is also a fifth DataSource Operation, "custom", which is intended for arbitrary server
// operations that are more complex than a fetch of some records, or an update to a single
// record.
// <P>
// Each DataSource operation has specific request and response data, for example, in the
// "fetch" DataSource operation, the request data is expected to be search criteria, and the
// response data is expected to be a list of matching DataSource records.  Listed below are the
// request data and response data for each DataSource operation type, and what they mean.  
// <P>
// DataSource records are represented on the client by a JavaScript Object,
// where each property in the Object maps a DataSource field name to the field value - hence
// the DataSource operations below are in essence a way of exchanging records from client to
// server and back.
// <P>
// If you are using +link{group:serverDataIntegration,server-side data integration} with the
// SmartClient Java server, see the +docTreeLink{javaServerReference,Java Server Reference} for
// information about how DataSource Requests arrive on the server (specifically
// com.isomorphic.datasource.DSRequest) and how to provide responses 
// (specifically com.isomorphic.datasource.DSResponse.setData()).
// <P>
// If you are using +link{group:clientDataIntegration,client-side data integration} to directly
// consume services that use XML, JSON or other formats, see the "Editing and Saving" section
// of the +link{group:clientDataIntegration,client-side data integration} topic.
// <P>
//
// <b>fetch</b>
// <ul>
// <li>Request data: filter criteria, as an Object
// <li>Response data: matching records, as an Array of Objects
// </ul>
//
// <b>add</b>
// <ul>
// <li>Request data: new record, as an Object
// <li>Response data: new record as stored, as an Object
// </ul>
//
// <b>update</b>
// <ul>
// <li>Request data: primary keys of record to update, and new values (or just complete updated
// record), as an Object
// <li>Response data: new record as stored, as an Object
// </ul>
//
// <b>remove</b>
// <ul>
// <li>Request data: primary keys of record to delete, as an Object
// <li>Response data: minimally the primary keys of deleted record (can be complete record), as
// an Object
// </ul>
//
// <b>custom</b>
// <ul>
// <li>Request data: whatever the custom operation requires
// <li>Response data: custom operations can return whatever they like, including nothing.  
// Custom operations are like RPC calls in this respect - the exchanged data is unstructured, 
// so it is up to you to make sure the client and server agree.  Note also that, because of
// this unstructured data exchange, cache synchronization does not work with custom operations.
// </ul>
//
// @title DataSource Operations
// @treeLocation Client Reference/Data Binding
// @visibility external
//<

//> @groupDef clientServerIntegration
//
// Like client-server desktop applications, SmartClient browser-based applications interact
// with remote data and services via background communication channels. Background requests
// retrieve chunks of data rather than new HTML pages, and update your visual components in
// place rather than rebuilding the entire user interface.
// <P>
// <b>DataSources</b>
// <p>
// First you must create +link{class:DataSource,DataSources} that describe the objects from
// your object model that will be loaded or manipulated within your application.  All of 
// SmartClient's most powerful functionality builds on the concept of a DataSource, and because 
// of SmartClient's databinding framework (see +link{DataBoundComponent}), it's as easy to 
// create a DataSource that can configure an unlimited number of components as it is to 
// configure a single component.
// <P>
// For background information on how to create DataSources, +link{DataBoundComponent, bind}
// components to DataSources and initiate +link{DSRequest}s, please see the <em>Data 
// Binding</em> chapter of the <em>SmartClient Quickstart Guide</em>.
// <P>
// <b>Data Integration</b>
// <P>
// DataSources provide a data-provider agnostic API to SmartClient Visual Components that 
// allow them to perform the 4 CRUD operations (<b>C</b>reate, <b>R</b>etrieve, 
// <b>U</b>pdate, <b>D</b>elete).  By "agnostic" we mean that the implementation details - 
// the nuts and bolts of how a given DataSource actually retrieves or updates data - are 
// unknown to bound SmartClient components.  One effect of this is that DataSources are 
// "pluggable": they can be replaced without affecting the User Interface.
// <p>
// When a visual component, or your own custom code, performs a CRUD operation on a DataSource,
// the DataSource creates a +link{DSRequest} (DataSource Request) representing the operation.
// "Data Integration" is the process of fulfilling that DSRequest by creating a corresponding
// +link{DSResponse} (DataSource Response), by using a variety of possible approaches to 
// connect to the ultimate data provider.  
// <p>
// There are two main approaches to integrating DataSources with your server technology: 
// <ul>
// <li><b>Server-side integration</b>: DataSource requests from the browser arrive as Java 
// Objects on the server. You deliver responses to the browser by returning Java Objects. The
// various server-side integration possibilities are discussed later in this article.</li>
// <li>+link{clientDataIntegration,Client-side integration}: DataSource requests arrive as 
// simple HTTP requests which your server code receives directly (in Java, you use the 
// Servlet API or .jsps to handle the requests). Responses are sent as XML or JSON which you 
// directly generate.</li>
// </ul>
// The possible approaches are summarized in the diagram below. Paths 2, 3 and 4 are 
// client-side integration approaches, and path 1 includes all server-side integration 
// approaches. 
// <p>
// <img src="skin/ClientServerIntegration.png" width="866px" height="495px">
// <p>
// SmartClient supports, out of the box, codeless connectivity to various kinds of common data
// providers, including SQL and Hibernate.  SmartClient also provides functionality and tools
// for accelerated integration with broad categories of data providers, such as Java
// Object-based persistence mechanisms (JPA, EJB, Ibatis, in-house written systems), and REST 
// and WSDL web services in XML or JSON formats.  Ultimately, a DataSource can be connected to 
// anything that is accessible via HTTP or HTTPS, and also to in-browser persistence engines 
// such as +externalLink{http://gears.google.com,Google Gears}.
// <p>
// <b>Choosing a Data Integration Approach</b><p>
// This section aims to help you decide which of the many possible data integration approaches
// is best for your particular circumstances.  The recommendations given here will guide you
// to the approach that involves the least effort.<p>
// <img src="skin/dataIntegrationFlowchart.png" width="640px" height="300px">
// <p>
// <ul>
// <li>If you have a Java server:</li>
// <ul>
//   <li>If your ultimate storage is a SQL database:</li>
//     <ul>
//       <li>Use the SQLDataSource unless you have a very large amount of pre-existing
//           JPA or Hibernate code - small amounts of business logic can be easily migrated.
//           Be sure to read the overview of +link{group:sqlVsJPA,SQLDataSource vs JPA/Hibernate} 
//           in order to understand the large benefits the SQLDataSource provides</li>
//       <li>Derive DataSource definitions from existing tables or Hibernate mappings using the
//           +link{dataSource.autoDeriveSchema,autoDeriveSchema} feature, or from Java Beans
//           via the +link{dataSource.schemaBean,schemaBean} feature.
//           Or, use the +link{adminConsole,Admin Console} to generate tables from DataSource
//           definitions you create by hand</li>
//     </ul>
//   <li>If your ultimate storage is not a SQL database:</li>
//     <ul>
//       <li>If your persistence is based on Java Beans, use the
//           +link{dataSource.schemaBean,schemaBean} feature to derive DataSource definitions from
//            any Java bean</li>
//       <li>write a +link{group:writeCustomDataSource,custom DataSource} that provides the
//           CRUD operations you want to support.</li>
//     </ul>
//   <li>Whether or not your storage is SQL, add business logic either declaratively in the 
//       DataSource definition, via +link{dmiOverview,DMI}, or any combination of the two:
//     <ul>
//       <li>The &lt;criteria&gt; and &lt;values&gt; properties of an +link{class:OperationBinding}
//           allow you to dynamically set data values at transaction-processing time, using 
//           built-in +link{group:velocitySupport,Velocity support}</li>
//       <li>Override the <code>validate()</code> method of the DataSource to provide extra
//           custom validations - just call <code>super</code> to obtain the list of errors 
//           derived from SmartClient validations, then add to that list as required with your
//           own custom code</li>
//       <li>Override the <code>execute()</code> method of the DataSource to add extra processing
//           either before or after the SmartClient processing</li>
//       <li>Use +link{group:transactionChaining,Transaction Chaining} to dynamically set
//           data values according to the results of earlier transactions</li>
//       <li>For SQL DataSources, use +link{group:customQuerying,SQL Templating} to change, 
//           add to or even completely replace the SQL sent to the database, and to implement
//           special query requirements</li>
//       <li>For JPA DataSources, use +link{attr:OperationBinding.customJQL,custom JQL queries}
//           to implement special query requirements</li>
//       <li>For Hibernate DataSources, use +link{attr:OperationBinding.customHQL,custom HQL queries}
//           to implement special query requirements</li>
//      </ul>
//      Read more about the server-side request processing flow and how to customize it in
//      +link{group:serverDataIntegration,the server integration overview}.
// </ul>
// </ul>
// <ul>
// <li>If you do not have a Java server:</li>
//   <ul><li>If you are not obliged to use a pre-existing network protocol, use the 
//           +link{class:RestDataSource}</li>
//       <li>Otherwise, use +link{clientDataIntegration,client-side data integration} features
//           to create a custom client-side DataSource that adapts the DataSource protocol to 
//           your existing services</li>
//   </ul>
// </ul>
// <p><br>
// <b>RPCs: Unstructured Server Communication</b>
// <P>
// SmartClient also supports "unstructured" client-server operations.  These 
// +link{RPCRequest}s (Remote Procedure Call Requests) are a low-level, very flexible 
// mechanism for custom client-server communications.  In an nutshell, RPCRequests:
// <ul>
// <li> may contain arbitrary data
// <li> are always initiated by custom code (a call to +link{RPCManager.send()}), and have
// their responses handled by custom code (the callback passed to <code>send()</code>)
// </ul>
// <P>
// RPCRequests are relatively rare.  Most client-server communications are better done in a 
// structured fashion using a +link{DSRequest} (DataSource Request).  Note that <em>any</em>
// RPCRequest can alternatively be framed as a +link{method:dataSource.fetchData,DataSource fetch}; 
// depending on the circumstances, this may be more convenient.
// <P>
// See the +link{RPCManager} documentation for further information on RPCRequests.
// 
// @title Client-Server Integration
// @treeLocation Concepts
// @visibility external
//<

//> @groupDef writeCustomDataSource
// Out of the box, and with no code to write, SmartClient supports SQL, JPA and Hibernate for 
// persistence, which includes EJB 3.0, EclipseLink and other Java persistence systems accessible
// via JPA.  For other Java-based persistence systems, such as legacy EJBs or systems
// proprietary to your company, you write a custom DataSource class in Java.  In most cases, it
// is possible to write a single, generic DataSource class that provides access to all data
// that is a available from a given persistence mechanism; for example, a single DataSource
// class can typically be written for accessing all data accessible via legacy EJB.
// <p>
// Note that a majority of the features of the SmartClient Server framework apply even when
// using your own persistence mechanism.  As with the features supported by SmartClient's
// browser-based visual components, SmartClient's server-side features rely only on the 
// concept of a DataSource and not on the details of the ultimate persistence mechanism.  Hence
// they are usable with a custom DataSource regardless of the final data provider.
// <p>
// We provide a complete working example of a custom DataSource in the SmartClient Feature
// Explorer; you can see it in action +explorerExample{ormDataSource,here}.  This example 
// "ormDataSource" is an adaptor for Hibernate which supports the 4 CRUD operations, 
// data paging, server-side sort and filter, and which participates correctly in 
// +link{ResultSet,cache synchronization}.  The code required is minimal, and the approaches
// taken generalize to any ORM system.  Studying the Java source code for this DataSource -
// which is available in the "ORMDataSource.java" tab in the example linked to above - is the
// best way to get a start on implementing your own custom DataSource.
// <p>
// <ul>
// <li><code>ORMDataSource</code> extends <code>BasicDataSource</code>.
// <li><code>ORMDataSource</code> is primarily an implementation of four key methods:
//     <code>executeFetch</code>, <code>executeAdd</code>, <code>executeUpdate</code> and 
//     <code>executeRemove</code>.  All the logic related to the actual CRUD data operation
//     takes place in one of these methods.  This is the recommended approach.</li>
// <li>The class also implements the <code>execute</code> method.  This is an override of the
//     method that is actually called by the framework, and as such is an appropriate place to
//     set up shared objects that will be used in more than one CRUD operation, and to perform
//     shared pre- and post-processing.  As you can see, the example is setting up a Hibernate
//     session and transaction, and then calling <code>super.execute</code> - this calls back
//     into the framework and ultimately leads to the appropriate data operation method being
//     called.</li>
// <li>Note how each of the <code>executeXxx</code> methods conforms to the 
//     +link{dataSourceOperations,DataSource protocol}.  To take <code>executeFetch</code> as 
//     an example, note how it:
//     <ul><li>Retrieves the criteria for the fetch from the supplied <code>DSRequest</code></li>
//         <li>Implements logic to obey the <code>startRow</code>, <code>endRow</code> and 
//             <code>batchSize</code> values.  This is only necessary for a DataSource that 
//             intends to support automatic data paging.</li>
//         <li>Retrieves <code>sortByFields</code> from the supplied <code>DSrequest</code>, 
//             and uses that value to change the order of the resultset.  This is only 
//             necessary for a DataSource that intends to support server-side sorting.</li>
//         <li>Populates <code>startRow</code>, <code>endRow</code> and <code>totalRows</code>
//             on the <code>DSResponse</code>.</li>
//         <li>Populates the <code>DSResponse</code>'s <code>data</code> member with the list of 
//             objects retrieved by the Hibernate call.</li>
//     </ul><br>
//     These are the only parts of this method that are of significance as far as SmartClient 
//     is concerned - the rest of the method is concerned with communicating with the 
//     data provider, which is of no interest to SmartClient as long as the method conforms to
//     the DataSource protocol for a "fetch" operation.</li>
// </ul>
// <p><br>
// <b>The DataSource descriptor</b>
// <p>
// Once your custom DataSource is implemented, you need to to create a descriptor for each 
// instance of the DataSource.  As noted above, it is generally possible to write one custom 
// DataSource class that is capable of handling all data access for a particular persistence 
// mechanism.  DataSource descriptors, on the other hand, are written per entity.
// <p>
// A DataSource descriptor is an XML file with the special suffix <code>.ds.xml</code>.  The 
// descriptor for a custom DataSource is, for the most part, identical to the descriptor for 
// a built-in DataSource: it is the central place where you describe the DataSource instance
// to the system - its fields, validations, security constraints, special data operations, 
// transaction chaining expressions and so on (see the +link{class:DataSource,DataSource docs} 
// for full details).
// <p>
// One property that is always required for a custom DataSource is 
// +link{attr:DataSource.serverConstructor,serverConstructor}.  This fully-qualified class 
// name tells SmartClient what to instantiate when data operations for this DataSource arrive
// on the server - in other words, it is how you tell SmartClient to use your custom class.  
// In the +explorerExample{ormDataSource,ORM DataSource example}, on the 
// <code>ormDataSource_country</code> tab, you will see how we use this property to tie the
// <code>ormDataSource_country</code> DataSource <em>instance</em> to the 
// <code>ormDataSource</code> DataSource <em>implementation</em>.
// <p>
// Finally, if your data model is based on Javabeans, or on POJOs that broadly follow the 
// Javabean conventions (basically, if they have private state variables accessible via public 
// getters and setters), SmartClient can automatically generate basic DataSource definitions 
// for your beans that will only need minimal change (ie, specifying a 
// <code>serverConstructor</code>) to be fully operational.  Both the 
// +explorerExample{javabeanWizard,Visual Builder Javabean Wizard} and the Batch DataSource 
// Generator can create DataSource descriptors from existing beans.
// <p>
// <b>Server framework features relevant to custom DataSources</b>
// <P>
// The vast majority of the SmartClient Server framework's key features are not specific to the
// built-in SQL and Hibernate connectors, and still apply even when using a custom persistence
// mechanism.  See +link{group:featuresCustomPersistence,this overview} of which features apply
// when using a custom persistence mechanism and how best to leverage those features.
//
// @title Custom Server DataSources
// @treeLocation Concepts/Persistence Technologies
// @visibility external
//<

//> @groupDef featuresCustomPersistence 
// The vast majority of the SmartClient Server framework's key features are not specific to the
// built-in SQL and Hibernate connectors, and still apply even when using a custom persistence
// mechanism.
// <P>
// See the listing below of major features and how to apply them with custom persistence:
// <p>
// <b>Server Data Binding:</b> Using the SmartClient Server framework means that the starting
// point for connecting to custom persistence logic is a clean Java API.  SmartClient provides
// Java <code>DSRequest</code> and <code>DSResponse</code> objects with all of the methods
// necessary to handle data paging, sorting, validation error reporting, and other features.
// In most cases, you can fulfill a DSResponse by simply returning one of your Java
// business objects rather than worrying about how to encode objects to XML or JSON.
// Communication with the browser is automatically handled with an efficient, compressed
// protocol.  
// +explorerExample{ormDataSource,Custom DataSource example}, 
// +explorerExample{DMI,DMI example}
// <p>
// <b>Data Selection (No DTOs):</b> When using a DataSource, Java data you return in your
// <code>DSResponse</code> is automatically trimmed to just the fields declared in the
// DataSource before delivery to the browser (see
// +link{DataSource.dropExtraFields,dropExtraFields}).  This eliminates the need to create 
// redundant +externalLink{http://en.wikipedia.org/wiki/Data_transfer_object,Data Transfer Objects} 
// to express the list of fields that need to be delivered to the UI - the DataSource already
// has this information, and can serve two purposes by both configuring UI components and
// trimming relevant data, from a single definition.
// <P>
// Furthermore, DataSources can extract specific fields from complex nested 
// object graphs via XPath expressions, for both loading and saving of data.
// +explorerExample{flattenedBeans,XPath Binding example}
// <p>
// <b>Server Validation:</b> Both client and server validation are driven from declarations in
// a single DataSource definition.  You already have a need to declare validators to
// drive SmartClient's client-side validation; when you use the SmartClient Server framework
// you get automatic server-side enforcement of the same validation rules, without the need to
// write any additional code.
// +explorerExample{serverValidation,Server Validation example}
// <p>
// <b>Queuing:</b> Queuing allows multiple data load or save requests from different UI 
// components to be transparently combined into a single HTTP request with guaranteed in-order
// execution.  The type of DataSource handling each request is not important, so queuing will
// work with your custom DataSource; in fact, a single queue could contain operations to be 
// handled by many different types of DataSource.  
// +explorerExample{transactionsFolder,Queuing examples}
// <p>
// <b>Declarative security:</b> The server framework provides robust authentication and 
// authorization integration, to allow you to secure both DataSource operations (data fetch 
// and update requests) and individual DataSource fields at various granularities.  The 
// declarative security rules are expressed directly in the <code>.ds.xml</code> file, and 
// allow you declare rules as simple as "all operations on this DataSource require an 
// authenticated user", or as complex as "a user can only update this field if he has role 
// 'admin' and a call to the <code>isAuthorized()</code> method on a security checking object
// we have stored in the <code>HttpSession</code> returns true".  Security rules are applied 
// by the framework before and after a DSRequest is executed, so they are applied even to 
// operations on completely custom DataSources. See 
// +link{OperationBinding.requiresRole} and the properties it links to for more details on 
// operation-level security, and +link{DataSourceField.viewRequiresAuthentication} and the 
// properties it links to for more details on field-level security.
//
// <p>
// <b>Transaction Chaining:</b> allows one request in a queue of operations to incorporate
// values from previously executed requests in the same queue.  This allows a series of
// dependent operations - such as fetching a value from one DataSource to be used in a query on
// another - to be defined with simple declarations right in the DataSource definition, with no
// need to write a custom Java class to manually pass data between the different operations.
// Since Transaction Chaining works strictly in terms of DataSource requests and responses and
// knows nothing about the underlying persistence mechanism, it works with any persistence
// strategy.  See the +link{group:transactionChaining,Transaction Chaining overview}.
// <p>
// <b>Java / JS Reflection:</b> Any Java object can be delivered to the browser as a JavaScript 
// object, and vice versa.  As a developer of a custom DataSource, you do not need to concern 
// yourself with dealing with translations to and from JSON or XML; you work directly with
// native Java objects.  For example, a method you write to fulfill a "fetch" operation can
// simply return a <code>Collection</code> of Java beans; the SmartClient Server framework would 
// transparently handle converting this into a matching Javascript structure.
// +explorerExample{masterDetail,Saving nested objects example}
// <p>
// <b>Visual Builder:</b> The DataSource Wizards in Visual Builder are pluggable; we provide
// wizards for SQL and Hibernate DataSources, and it is easy to write a new wizard to integrate 
// your custom DataSource into Visual Builder.  +explorerExample{sqlWizard,SQL Wizard screenshots},
// +explorerExample{hibernateWizard,Hibernate Wizard screenshots}
// <p>
// <b>Batch DataSource Generator:</b> If the persistence scheme you are implementing your 
// custom DataSource for is based on collections of Javabeans, the Batch DataSource Generator 
// can generate DataSource definition files for instances of your custom DataSource.  This is
// out of the box behavior, but you can also alter and extend the DataSource Generator to suit 
// your exact needs - we supply the source and it has been specifically designed to be easy 
// to modify.
// <p>
// <b>Batch Uploader:</b> A user interface for end-to-end batch upload of data as a pre-built, 
// customizable component.  This component - like any SmartClient databound component - only
// cares that a DataSource is a DataSource, so custom DataSources will work just like built-in
// ones.  +explorerExample{batchUpload,Batch Uploader example}
// <p>
// <b>File Upload:</b> Single and multiple file uploads can be handled as a normal DataSource 
// operation, including normal handling of validation errors. Optional automatic storage to SQL 
// (no server code required) means you can upload to SQL tables for holding files, which can be
// related to Java Objects by id (eg, a User's uploaded files). 
// +explorerExample{upload,File Upload example}
// <p>
// <b>Export:</b> Allows any grid component to export its current dataset in CSV, XML or JSON
// format.  This feature works by issuing the DataSource with an ordinary "fetch", and then 
// changing the <code>DSResponse</code> to send back an import file rather than a resultset.
// Accordingly, this just works with custom DataSources.  +explorerExample{export,Export example}
// <p>
// <b>HTTP Proxy:</b> The HTTP Proxy allows an application to access web services hosted on 
// remote servers which are not normally accessible to web applications due to the 
// +externalLink{http://www.google.com/search?q=same+origin+policy,"same origin policy"}).
// This is a general feature of the SmartClient Server framework that does not directly apply
// to DataSources.  +explorerExample{rssFeed,HTTP Proxy example}
// <p>
// <b>Lightweight Persistence / Reporting:</b> Even while using a custom DataSource to connect
// to a custom ORM system, you can still make use of the SQL DataSource for simple storage-only
// entities where an object-based representation is a waste of time.  You can also do
// this for reporting scenarios that don't correspond to the object model.
//
// @title Server Features and Custom Persistence
// @treeLocation Concepts/Persistence Technologies
// @visibility external
//<


//> @groupDef sqlVsJPA
// If you are free to choose which persistence mechanism your application will use, you should
// consider using the SmartClient SQL DataSource instead of a more heavyweight, bean-based 
// solution.  This article discusses the advantages of doing so.
// <p>
// <b>Simplicity</b>
// <p>
// With the SmartClient SQL DataSource, simple CRUD connectivity can be set up via a 
// +explorerExample{sqlWizard,wizard} and requires zero server side code.  Only a DataSource 
// descriptor (.ds.xml file) needs to exist; this descriptor can be generated by the wizard 
// or created by hand.  The descriptor actually serves double duty by also providing the 
// configuration for UI components - in other words, this is information that you would need 
// to express anyway.
// <p>
// Semi-technical product managers, testers, business analysts and IT staff who have no
// familiarity with Java can easily comprehend DataSource definitions and even customized
// SQL queries, allowing them to go further with prototyping efforts, provide more specific
// feedback and capture more relevant diagnostics when reporting issues.
// <p>
// This level of simplicity is lost when using more heavyweight systems.  JPA / EJB 
// best practices indicate creation of a bean class for every domain object, as well as 
// related "services" or "session beans", DTOs 
// (+externalLink{http://en.wikipedia.org/wiki/Data_Transfer_Object,Data Transfer Objects}) and 
// other unnecessary scaffolding.  Ibatis avoids some of this scaffolding, but requires every 
// SQL query to be written by hand.  In contrast the SQL DataSource supports basic CRUD queries
// out of the box.
// <p>
// <b>Performance</b>
// <p>
// Systems like JPA work nicely when dealing with a single object at a time, but enterprise
// applications routinely work with lists or trees of objects that draw data from multiple
// tables.  In these situations, it's trivial to express an efficient SQL query for retrieving
// the desired results (as shown in +explorerExample{largeValueMap,this example}).  Fetching the
// same data using getter methods on Java Beans often leads to nightmare performance scenarios
// (such as 3 or more separate SQL queries per object retrieved).
// <P>
// Trying to "trick" the persistence system into generating efficient queries doesn't make
// sense - this just leads to a far more complex and fragile solution that now requires deep
// knowledge of how the ORM system generates SQL as well as SQL itself. 
// <P>
// SQLDataSource allows you to directly write SQL when it makes sense, and 
// +link{attr:DataSource.beanClassName,to use beans} when object oriented approaches are
// clearer and simpler.  When you do write SQL directly, you override just the parts of the
// query that you need to change - you still leverage SQLDataSource's ability to generate
// cross-database SQL for complex search criteria, efficient data paging and sorting, even in a
// complex reporting query (see +explorerExample{dynamicReporting,this example}).
// <p>
// <b>Portability</b>
// <p>
// SmartClient DataSources provide cross-database portability like JPA and other solutions.
// However, DataSources can also be replaced with an entirely different integration strategy or
// entirely different server platform, such as a SOA architecture where the browser contacts
// WSDL web services directly.  The clear data requirements definition represented by a
// DataSource makes such drastic technology changes much easier with the SQL DataSource than
// with any other technology.
// <p>
// <b>Power</b>
// <p>
// The SQL DataSource has out of the box support for server-side advanced filtering without
// the need to write any code (see the 
// +explorerExample{filterBuilderBracket,SQL Advanced Filtering example}), and SmartClient
// provides +link{class:FilterBuilder,pre-built user interfaces for filtering}.  The effort
// required to develop similar functionality with another persistence mechanism would vary from 
// substantial to spectacular.
// <p>
// You can leverage advanced, automatic SQL generation, such as advanced filter criteria, 
// GROUP BY and ORDER BY clauses, and selection of row ranges, even in very heavily customized
// queries.  The +explorerExample{dynamicReporting,Dynamic Reporting example} shows this.
// <p>
// With the SQL DataSource and +link{group:transactionChaining,Transaction Chaining}, you can 
// chain together multiple SQL queries, or a mixture of SQL queries and other data access, with
// simple declarations right in the DataSource, as +explorerExample{queuedAdd,this example} 
// demonstrates.
// <p>
// Because you write the SQL, you can use database-specific features when absolutely 
// necessary.  Features such as query optimizer hints or stored procedures are thus accessible
// but, importantly, are within the same processing model used for all other data access.
// <p>
// <b>Security</b>
// <p>
// Because the central DataSource definition expresses all the available operations, how they
// are performed and who has access to them, things are clear and simple.  It's much easier to 
// understand and audit a DataSource definition than a slew of Java classes.
// <p>
// There is no information leakage from server to client with the SQL DataSource.  All 
// server-side declarations, such as SQL templates, are automatically stripped out of the 
// DataSource definition before the browser sees it.
// <p>
// Custom SQL in a SmartClient SQL DataSource is protected from SQL injection attacks.  It is
// impossible for a developer to write a SQL template that is vulnerable to SQL injection 
// without going through the +link{group:velocitySupport,$rawValue} feature, a rarely used
// feature that is very prominently flagged in the documentation as requiring special care.
// Other ORM systems tend to require hand-coded SQL queries for advanced use cases such as
// reporting; these hand-written queries are where most security holes appear.  By providing a
// safe environment for SQL customizations, SQL DataSource removes these risks.
//
// @title SQL DataSource vs JPA, EJB, Ibatis and other technologies
// @treeLocation Concepts/Persistence Technologies
// @visibility external
//<


//> @groupDef serverDataIntegration
// Server Data Integration means:
// <ul>
// <li> You +link{iscInstall,install} the 
//      +link{group:iscServer,SmartClient Java Server Framework} into any J2SE/J2EE
//      environment, including any existing web application
// <li> You +link{group:dataSourceDeclaration,create DataSources} via an XML declaration,
// possibly on-the-fly from +link{group:metadataImport,existing metadata}.  
// <li> Server communication for components bound to these DataSources is handled
// automatically with a highly efficient, compressed protocol.  You work with clean Java APIs
// instead of dealing with the details of XML or JSON over HTTP.
// <li> You can use built-in connectors for SQL, Hibernate and other common data providers
// without writing any code, or you can easily build your own connectors in Java.
// <li> Whether using the built-in connectors or custom connectors, declarations in your
// DataSource control a large set of server features that can make common types of business
// logic entirely declarative
// </ul>
// This approach is in contrast to 
// +link{group:clientDataIntegration,Client-side Data Integration} in which client-side
// DataSources are configured to send and receive HTTP messages containing XML, JSON
// or other content.
// <P>
// <B>Server-side Request Processing</B>
// <P>
// Client-side +link{DataBoundComponent,DataBoundComponents} will send
// +link{DSRequest,DSRequests} to the SmartClient Server as background communications transparent
// to the user.  Integrating SmartClient's DataSource layer with your data model is a matter of
// handling these DSRequests and sending back DSResponses, in order to fulfill the 4 basic
// operations of the +link{group:dataSourceOperations,DataSource Protocol}.
// <P>
// Out of the box, SmartClient is set up to route all DSRequests through a special servlet
// called <code>IDACall</code>.  Requests that go through <code>IDACall</code> have the 
// following lifecycle:
// <ul>
// <li>The overall HTTP request is received by the IDACall servlet.  SmartClient supports 
// queuing of transactions, so each HTTP request might contain multiple DSRequests.</li>
// <li>IDACall sets up an instance of <code>RPCManager</code> to manage the processing of
// the entire queue of transactions.  For every DSRequest in the queue, this RPCManager:</li>
//   <ul>
//   <li>Validates the DSRequest</li>
//   <li>Sends the DSRequest through +link{dmiOverview,DMI} - in other words, your code - if this is 
//       configured in the DataSource.  As described later in this section, your code can
//       perform some custom logic here: either completely fulfilling the request, or
//       alternatively modifying the request and causing the default
//       processing of the request to continue</li>
//   <li>Calls the DataSource's <code>execute</code> method to obtain a DSResponse.</li>
//   </ul>
// <li>Having processed all requests, the RPCManager now serializes all the DSResponses 
// and sends them back to the browser as a single HTTP response</li>
// </ul>
// <p>
// This basic request handling flow can be customized at a number of points:
// <ul>
// <li>If you need an overarching authentication service, this is best implemented using 
// <a href=http://java.sun.com/products/servlet/Filters.html>servlet Filters</a> to intercept
// unauthenticated requests before they reach the <code>IDACall</code> servlet</li>
// <li>If you are not using one of the built-in persistence mechanisms (SQL and Hibernate), 
// hook into the <code>IDACall</code> flow by 
// +link{writeCustomDataSource,writing a custom DataSource}.  This approach lets you write and 
// maintain the minimal amount of custom code, while taking full advantage of
// DataSource-agnostic features of the SmartClient Server, like validation, queuing,
// transaction chaining, support for Velocity templating, and so on.</li>
// <li>Custom validation can be added by writing a custom DataSource (extending SQLDataSource
// or HibernateDataSource if appropriate) and overriding its <code>validate()</code> method, 
// as described +link{DataSource.serverConstructor,here}.</li>
// <li>General custom business logic can be added in a number of ways, both declaratively and
// programmatically:</li>
// <ul>
//   <li>The &lt;criteria&gt; and &lt;values&gt; properties of an +link{class:OperationBinding} 
//       allow you to dynamically set data values at transaction-processing time, using 
//       built-in +link{group:velocitySupport,Velocity support}</li>
//   <li>Override the <code>execute()</code> method of the DataSource to add extra processing
//       before and/or after the SmartClient processing</li>
//   <li>Use +link{group:transactionChaining,Transaction Chaining} to dynamically set data values 
//       according to the results of earlier transactions
//   <li>For SQL DataSources, use +link{group:customQuerying,SQL Templating} to change, add 
//       to or even completely replace the SQL sent to the database, including calling
//       stored procedures</li>
//   <li>Use +link{dmiOverview,Direct Method Invocation} to call directly into your own Java 
//       classes.  DMIs allow you to modify the <code>DSRequest</code> before it executes,
//       modify the <code>DSResponse</code> before it returns, or take unrelated actions.  They
//       can be used to add business logic to a persistence operation without destroying
//       the default behavior</li> 
//   <li>Use +link{operationBinding.script,DMI Scripts} to add small amounts of business logic 
//       right in the &lt;operationBinding&gt; tag.  DMI scripts allow you to add business
//       logic just like normal DMIs, but don't require the logic to be in a separate .java
//       file.</li>
// </ul><br>
// <li>If you need to use a Front Controller servlet for some other reason than authentication -
// for example, you are using Spring, Struts, or some other similar system which requires that 
// all requests go through some particular servlet - just call 
// <code>RPCManager.processRequest()</code> within your Spring Controller, Struts Action, or 
// whatever the equivalent is in the framework in use.
// <p>
// However, note carefully that taking this approach is often a sign that the SmartClient 
// architecture has not been correctly understood.  SmartClient is architected for 
// <em>client-server</em> data communication, as opposed to early web MVC frameworks which 
// do everything on the server.  In particular, it is absolutely incorrect to represent every 
// individual DataSource operation - or even every DataSource - as a separate Struts Action 
// or Spring Controller, because this implies different URLs for different operations.  All 
// DataSource operations should go through a single URL in order to allow 
// +link{class:RPCManager,transaction queuing} - see these 
// +explorerExample{transactionsFolder,Queuing examples}.</li>
// </ul>
// <P>
// For more information on the DMI subsystem, see the +link{dmiOverview,DMI overview},
// +link{DMI,DMI class} and the 
// +explorerExample{DMI,DMI example} in the Feature Explorer.
// <P>
// Note that, as you continue to integrate your prototype with your backend, you can use a
// mixture of DataSources that have been fully integrated with your backend and DataSources
// that are running in "client-only" mode (see +link{group:clientOnlyDataSources}).
// <P>
// <b>Important methods for handling DataSource requests</b>
// <P>
// The basic flow of logic for handling DataSource requests is:
// <P>
// <table class="normal" border=1 width="700">
// <tr>
// <td>1. Determine operation type (Fetch, Add, Update, Remove) for a single request.  Not 
// necessary if you follow the recommendations for 
// +link{group:writeCustomDataSource,writing a custom DataSource} and provide your 
// implementation via <code>executeFetch(), executeAdd()</code>, et al.</td>
// <td>dsRequest.getOperationType()</td>
// </tr>
//
// <tr>
// <td>2. Get inbound values (Add, Update) and/or criteria (Fetch, Update, Remove) for this
// request.</td>
// <td>dsRequest.getFieldValue()<br>
// dsRequest.getValues()<br> 
// dsRequest.getCriteria()</td>
// </tr>
//
// <tr>
// <td>3. Business logic, validation, calls to data and service tiers... anything you can code.
// </td>
// <td><b>execute custom logic</b></td>
// </tr>
//
// <tr>
// <td>4. Set status and data for the response.</td>
// <td>dsResponse.setStatus()<br>
// dsResponse.setData()</td>
// </tr>
// </table>
// <P>
// For more information, see the +link{RPCManager,RPCManager documentation}, and the 
// +explorerExample{ormDataSource,Custom ORM DataSource example}. 
//
// @title Server DataSource Integration
// @treeLocation Concepts/Client-Server Integration
// @treeLocation Java Server Reference
// @visibility external
//<

//> @groupDef metadataImport
// In SmartClient, metadata is expressed through +link{DataSource,DataSources}, which in turn
// drive +link{DataBoundComponent,DataBoundComponents}.  If you have existing metadata, there
// are several possible approaches to transforming it to SmartClient DataSources, either one
// time or on the fly.
// <P>
// There are two possible targets for metadata import: XML format or JavaScript format.
// The XML format is more general purpose, since the ISC server can transform it to JavaScript
// via the +link{group:loadDSTag,loadDS tag}, and DataSources in XML format can be used by the
// ISC server for server-side validation (this split is covered in more detail under
// +link{dataSourceDeclaration,Data Source Declaration}).
// <P>
// You may also transform your metadata dynamically (while the application is running in
// production) or statically (one time ever or at packaging time).  Generally for a static
// or dynamic transform targetting JavaScript format you will want to produce one .js file
// containing all your DataSource definitions, to be loaded by your application via a normal
// &lt;SCRIPT SRC&gt; tag.  For a static transform targetting XML format, you will want to
// produce a series of .ds.xml files and place them in the directories expected by the ISC
// server (see +link{group:dataSourceDeclaration,DataSource Declaration}).  Statically generated
// XML DataSources can be delivered to the browser as a single .js file via a .jsp containing
// several +link{loadDSTag,<code>loadDS</code> tags}.
// <P>
// If you want to do dynamic transform targetting XML format and use ISC server-side
// validation, the server-side API DataSource.fromXML() can be used to create a DataSource
// dynamically from XML, so that you can then call DataSource.validate().  Either the XML
// DataSource definition or the live DataSource itself can be passed to the server-side API
// XML.toJS() to produce JavaScript.
// <P>
// How to actually produce JavaScript or XML DataSource definitions from your existing metadata
// depends on the format of your metadata.
// <P>
// <b>XML Schema</b>
// <P>
// The method +link{XMLTools.loadXMLSchema()} and the 
// +link{group:loadXMLSchemaTag,loadXMLSchema JSP tag} provide dynamic transform of XML Schema
// to JavaScript.  This is essentially accomplished by running
// <code>isomorphic/system/schema/schemaTranslator.xsl</code> on the XML schema file to produce
// XML DataSource definitions, and then translating those to JavaScript.  You can run the
// <code>schemaTranslator</code> stylesheet using any standard XSLT processor and capture the
// XML output.
// <P>
// <b>Java Beans</b>
// <P>
// Metadata available via Java's "reflection" APIs allows a basic DataSource to be generated
// from Java beans.  Sample Java code can be found in
// <code>examples/server_integration/DataSourceGenerator.java</code>.  See also the last section
// on this page for an automatic option for generating DataSource definitions via Reflection.
// <P>
// <b>Other XML formats</b>
// <P>
// If you are familiar with XSLT or other XML transform languages, you
// could use it to do an XML to XML transform, and then use XML.toJS() to get to JavaScript.
// <P>
// <b>Schema represented as Java Objects</b>
// <P>
// If you are targetting XML, hand-coded generation of DataSource XML is straightforward, and
// from XML you can use XML.toJS() to get to JavaScript.
// <P>
// <b>Database Schema, Hibernate mappings or Java class definitions</b>
// <P>
// <em><b>Note: This is an Enterprise feature.</b>  It is not available to users of the LGPL,
// Pro or Power Editions of SmartClient.  Please see the <a href=http://smartclient.com/licensing>
// licensing page</a> for details.</em>
// <p>
// If you have existing database tables, Hibernate classes or Java POJOs that follow the Javabean
// semantics, you can use these to automatically produce basic XML DataSource definitions.  
// We provide the Batch DataSource Generator, a supported tool that makes use of SmartClient 
// Server APIs to generate XML DataSource definitions and save them as <code>.ds.xml</code> files 
// in your <code>shared/ds</code> folder.  The source for the tool is provided in 
// <code>tools/batchDSGenerator.jsp</code>.  
// <p>
// If your metadata source is existing database tables, the Batch DS Generator can also extract 
// data from those tables and save it as test data in your <code>shared/ds/test_data</code> 
// folder.  With this option, you have a complete round-trip facility from a populated database 
// table to a client-side DataSource that does not require a server. The database
// table can be recreated and repopulated from the XML files using the SmartClient Admin 
// Console, giving you a portable schema and dataset that can be used to initialize any database.
// <p>
// The tool is also able to output the DataSource definitions and test data in Javascript format
// rather than XML, for direct inclusion in client-side programs.  In this mode, it will create 
// <code>.ds.js</code> and <code>.data.js</code> files, rather than files with a <code>.xml</code>
// extension.
// <p>
// To use the tool, direct your web browser to the <code>tools/batchDSGenerator.jsp</code> 
// resource of your SmartClient server.  If you pass no parameters to the JSP, a SmartClient 
// window will be shown, prompting you for the various generation parameters.  Alternatively, 
// you can effect a true batch operation by passing these parameters in directly.  The source 
// code is extensively commented and explains the meanings of all the parameters should you wish
// to take this latter approach.
// <p>
// Example simple usage (via the UI):<br/>
// <code>http://localhost:8080/tools/batchDSGenerator.jsp</code>
// <p>
// Example batch usage (providing the parameters by hand):<br/>
// <code>http://localhost:8080/tools/batchDSGenerator.jsp?dbName=Mysql&tableName=foo&tableName=bar&className=com.bar.foo.SomeClass&overwrite=true</code>
// 
// @title Metadata Import
// @treeLocation Client Reference/Data Binding
// @visibility external
//<


//> @groupDef errorHandling
//
// +link{RPCResponse} and +link{DSResponse} objects have an integer status field that the 
// RPCManager inspects when the response is received from the server. If the value of this 
// field is less than zero, the request is considered to have failed.  Otherwise it is 
// considered to have succeeded.  This value is settable via the setStatus() method call 
// on the server-side DSResponse and RPCResponse objects.
// <p>
// Errors in a SmartClient application fall into two main categories:<ul>
// <li>Validation errors, which arise as a result of rules in the application's business logic
// being broken.  These are part of the normal operation of the system.  A response with 
// validation errors has a status of +link{RPCResponse.STATUS_VALIDATION_ERROR}</li>
// <li>Unrecoverable errors, which are errors with the system itself.  These are not part of 
// the normal operation of the system</li>
// </ul>
// <b>Validation errors</b> are treated differently from other errors, precisely because they
// are an expected part of the normal operation of the system.  If 
// +link{DynamicForm.validate,validation} of a form results in errors, the form is redrawn to
// display those errors to the user.  How the user sees those errors is completely configurable -
// for example, see the DynamicForm properties +link{DynamicForm.showErrorIcons,showErrorIcons},
// +link{DynamicForm.showErrorText,showErrorText}, 
// +link{DynamicForm.showInlineErrors,showInlineErrors}, and indeed most DynamicForm properties
// that contain the workd "Error" - but the default in most skins is to highlight the field 
// with some kind of error icon, and provide the actual error text message in a floating box 
// when the user hovers over the field.
// <p>
// The remainder of this article concerns <b>unrecoverable errors</b>.  These are errors with
// the system itself, for example:<ul>
// <li>A network transport problem</li>
// <li>A server-side crash</li>
// <li>An update failed because a transaction was rolled back</li>
// </ul>
// Errors like this can either be handled centrally, or you can choose to handle them in your
// regular callback code.  +link{class:DSRequest} calls default to centralized handling; 
// +link{class:RPCRequest} calls default to user error handling in the callback.
// <p>
// <b>Centralized Error Handling</b><br>
// If the status field shows a failure, the RPCManager will invoke 
// +link{RPCManager.handleError}.  By default, this logs a warning and shows a dialog 
// with the contents of the response's +link{rpcResponse.data,data} field (which is assumed 
// to contain a meaningful description of the error that occurred).  If you specified a 
// callback in your request, it will <b>not</b> be called if the status shows a failure 
// (see the section on custom error handling below for how to change this).  This default 
// arrangement means that any SmartClient application has a basic handling mechanism for 
// unrecoverable errors, without any code to write.
// <p>
// You can customize centralized error handling at two levels:<ul>
// <li>
// <smartclient>Override +link{RPCManager.handleError}</smartclient>
// <smartgwt>Use 
// {@link com.smartgwt.client.rpc.RPCManager#setHandleErrorCallback(com.smartgwt.client.rpc.HandleErrorCallback)}</smartgwt>
// to provide your own error handling logic (note
// that customization takes place at the static class level, not per-instance)</li>
// <li>
// <smartclient>Override +link{RPCManager.handleTransportError}.</smartclient>
// <smartgwt>Use 
// {@link com.smartgwt.client.rpc.RPCManager#setHandleTransportErrorCallback(com.smartgwt.client.rpc.HandleTransportErrorCallback)}.</smartgwt>
// This logic is called earlier than
// handleError, and it is called even when you are using custom error handling (discussed
// below).  It is intended to allow your code to inspect the failed response early in the 
// handling flow, to see if it is really unrecoverable.  For example, a failure might have 
// happened because of a temporary network problem, so resubmitting the request may be a valid
// thing to do to try to resolve the error.  Note, as with handleError, this is a static 
// class-level customization</li>
// </ul>
// <p>
// <b>Custom Error Handling</b><br>
// As an alternative to handling errors centrally, you can handle them in your regular callback
// methods.  To do this, specify +link{RPCRequest.willHandleError,willHandleError} as a 
// request property.  When you do this, centralized error handling is bypassed (as mentioned 
// above, <code>handleTransportError()</code> will still be called) and your callback is 
// invoked as normal.  Your callback code determines that it is in error state by inspecting
// the status property on the response - if it is negative, there has been an error.  Note 
// that validation errors are treated specially, in that your callback is invoked, but the 
// normal behavior of populating the field errors onto the form and redrawing it <b>also</b>
// takes place.
// <p>
// Note, when you are handling errors in user callbacks, a negative status in the response 
// indicates some sort of serious, unrecoverable error (except in the case of 
// +link{RPCResponse.STATUS_VALIDATION_ERROR}).  Therefore, ensure that your error handling 
// code does not assume that the response will be properly formed or contain particular 
// elements.
// <p>
// You can specify <code>willHandleError</code> (or any other DSRequest/RPCRequest property)
// on a component request by providing the DSRequest Properties parameter.  For example, on
// a +link{ListGrid.fetchData()}:
// <smartclient><pre>
//     listGrid.fetchData({}, function(dsResponse, data, dsRequest) {
//         if (dsResponse.status < 0) {
//             // Error handling here...
//         } else {
//             // Normal processing here
//         }
//     }, <b>{willHandleError: true}</b>);
// </pre>
// </smartclient>
// <smartgwt><pre>
//     DSRequest properties = new DSRequest();
//     properties.setWillHandleError(true);
//     listGrid.fetchData(new Criteria(), new DSCallback() {
//         public void execute(DSResponse response, Object rawData, DSRequest request) {
//             if (response.getStatus() < 0) {
//                 // Error handling here
//             } else {
//                 // Normal processing here
//             }
//         }
//     }, <b>properties</b>);
// </pre>
// </smartgwt>
// <b>Error Status Codes</b><br>
// The error status codes used by the framework are documented as class variables of the 
// +link{class:RPCRequest,RPCRequest class}.  Status codes in the range -1 to -100 are 
// reserved for use by the framework; if you wish to introduce new custom error statuses for 
// your own use, avoid this range.
// <p>
// <b>Errors indicating login is required</b><br>
// Some of the framework error statuses indicate that login is required, typically because a 
// user's HTTP session has timed out.  The best way to handle this situation is to use the 
// built-in +link{group:relogin,re-login flow} to automatically prompt users to log back
// in as required, and then resubmit the requests that triggered the login required response.
//
// @title Error Handling Overview
// @treeLocation Client Reference/Data Binding
// @visibility external
//<

    // Comm and Transactions
	// ----------------------------------------------------------------------------------------
 
	// ---------------------------------------------------------------------------------------
    //> @object OperationContext
    // OperationContext is the bundle of settings that can be passed to performOperation() as
    // the "context" argument, or to any method that ultimately performs an operation and
    // supports a context argument.
    // <P>
    // There is no need to instatiate an OperationContext instance.  Just pass a normal
    // JavaScript object with the desired properties.
    //
    // @treeLocation Client Reference/Data Binding/RPCManager
    // @visibility ida
    //<

    //> @attr operationContext.showPrompt (boolean : true : IR)
    //  Whether to show a prompt in a modal dialog while this operation is being performed, in
    //  order to prevent user interaction. 
    // @visibility ida
    //<

    //> @attr operationContext.prompt (HTMLString : "Contacting server..." : IR)
    //  Text to show in the operation prompt if one is shown.
    // @see showPrompt
    // @visibility ida
    //<

    //> @attr operationContext.afterFlowCallback (callback : null : IR)
    //  An additional callback that will be invoked after the callback passed as an argument to
    //  performOperation.
    // @visibility ida
    //<

    //> @attr operationContext.willHandleError (boolean : false : IR)
    // Set this flag to indicate you are willing to handle error results from the server.  If
    // this flag is not set, standard RPCManager/DataSource error handling will be invoked.
    // <p>
    // The most common use of this flag is to directly handle validation errors.
    // <p>
    // Note that if you see context.willHandleError, the OperationResult object passed to your
    // IDACallback may be missing properties or be otherwise malformed, depending on
    // the severity of the server error.
    // <p>
    //
    // @see class:OperationResult
    // @see RPCManager.handleError()
    // @see dataSource.handleError()
    // @visibility ida
    //<

    //> @attr operationContext.operation (OperationID : null : IR)
    // Only applicable when calling Action Methods, specifies the operation ID to use instead
    // of the default DataSource operation.
    //
    // @group dataBoundComponentMethods
    // @see group:dataBoundComponentMethods
    //<


	// ---------------------------------------------------------------------------------------
    //> @type IDACallback
    // A function to call or expression to evaluate when an operation completes.
    // <p>
    // An IDACallback can be:
    // <ul>
    // <li>a string expression
    // <li>an object with the properties "caller" and "methodName", indicating an object to be
    // called and the method to call on it
    // </ul>
    // Regardless of the form of the callback, the callback method will be invoked with the
    // arguments (operationResult, results, data, context) with the meaning:
    // <ul>
    // <li>operationResult: an OperationResult object containing all the data returned by the
    // server for this operation
    // <li>data: just the "result.data" property from the OperationResult, for convenience
    // <li>context: just the "context" property from the OperationResult, for convenience
    // </ul>
    // @see class:OperationResult
    // @treeLocation Client Reference/Data Binding/RPCManager
    // @visibility ida
    //<
    // NOTE: intentionally omitted mention of passing an IDACallback that is the name of a
    // function on the IDA instance.


//> @groupDef smartArchitecture
// SmartClient can add interactivity and performance benefits to any web application with a
// variety of integration approaches.  This topic discusses the optimal architecture for a
// SmartClient application, which can be adopted in whole or in part.
// <p>
// In a typical HTML-based web application, every time a new view is shown to a user, a round
// trip to the server is required to retrieve new presentation information, such as a search
// screen.  However in an ISC-based application, showing a new view can be accomplished by
// simply hiding some components and showing others.
// <p>
// Because ISC components are expressed in a concise declarative form, and because ISC
// components have essentially no runtime performance impact until used, dozens of application
// views can be downloaded to the browser using the same bandwidth that would have been
// required to render just the initial view in plain HTML.  
// <p> 
// This architectural pattern of "preloading views" has tremendous benefits.  View transitions
// which do not require new data from the server can be performed near-instantaneously and
// without server involvement, boosting both interactivity and scalability.
// <p>
// Showing a dialog containing a "wizard" is a straightforward example of showing a "preloaded
// view".  For example:
// <pre>
//    function showNewUserWizard() {
//        Window.create({
//            items:[
//                DynamicForm.create({ ... })
//            ]
//        });
//    }
//    Button.create({
//        title:"New User..",
//        click:"showNewUserWizard()"
//    });
// </pre>
// In this example, none of the components involved in a potentially multi-pane wizard are 
// created until they are needed.  Showing the wizard has near-instantaneous response and
// causes no server load.
// <p>
// However, let's say that the first pane of the wizard is going to incorporate some dynamic
// user-specific data, such as the current user's name.  To load the username, we'll use an RPC
// operation targetting a .jsp called "getUserName.jsp" and show the wizard when it completes
// (see +link{class:RPCManager} for information on RPCs and how to construct a .jsp that can
// send an RPC response).
// <pre>
//    function showNewUserWizard() {
//        RPCManager.sendRequest({
//            actionURL:"getUserName.jsp",
//            callback:"doShow(rpcResponse)"
//        });
//    }
//    function doShow(rpcResponse) {
//        Window.create({
//            items:[
//                Canvas.create({contents:"Hello, " + rpcResponse.userName}),
//                DynamicForm.create({ ... })
//            ]
//        });
//    }
//    Button.create({
//        title:"New User..",
//        click:"showNewUserWizard()"
//    });
// </pre>
// In this example, we've simply incorporated a user name into the first pane of a wizard.
// However, this pattern allows us to arbitrarily change user interactions based on data from
// the server.  For example, the RPCResponse might have contained a flag indicating that the
// wizard should skip the first two steps, or an arbitrary warning message for the user, or
// even JavaScript code to be evaluated on the client.
// <P>
// This architecture has several key advantages:
// <dl>
//
// <dt><b>Performance: Cacheable UI</b></dt>
// <dd>
// A dynamic, data-driven UI can be expressed completely in <i>cacheable</i> JavaScript.
// This is in contrast to any architecture based on server-side HTML generation, where static
// parts of the presentation are mixed in with dynamic data, preventing cacheability so that
// bandwidth and server time are wasted repeatedly delivering the same static presentation
// data.
// <br>
// Even generated JavaScript is cacheable.  For example, a SmartClient View expressed in XML
// and contained within a JSP is still a separately cacheable resource when loaded via a
// &lt;SCRIPT SRC&gt; tag and advertised as a cacheable resource via HTTP headers, because it
// is ultimately delivered to the browser as simple JavaScript.  Hence standard 
// +link{group:i18n,internationalization} techniques such as using JSTL tags in a JSP remain
// applicable.
// <br>
// The SmartClient Architecture even allows you to capture all the gradations of cacheability
// from completely static (changes once per application rollout) to completely dynamic
// (timestamp).  In the example above, the user name wouldn't actually change for the lifetime
// of the page, so could be loaded once only.
// <br>&nbsp;</dd>
//
// <dt><b>Performance: Minimal Server State</b></dt>
// <dd>
// Any architecture that relies on component descriptions being generated by the server
// must track a great deal of state, which, in the SmartClient Architecture, is either
// completely eliminated or greatly reduced.
// <br>&nbsp;</dd>
//
// <dt><b>True Presentation / Business Logic separation</b></dt>
// <dd>
// The RPCResponse object represents the client's exact, minimal needs for server data.
// This is much easier to understand and to audit than a slew of .jsp files which access and
// update miscellaneous state.  It is also far easier to spot reusable patterns of data access,
// which in server-side HTML generation systems often end up as duplicated code.
// <br>&nbsp;</dd>
//
// <dt><b>Parallel Development and Testability</b></dt>
// <dd>
// Using the SmartClient architecture allows you to build a complete, working application
// that can run without a server, based on sample data.  In the example above, it would be 
// straightforward to create a testing mode that returned a faked RPC response consisting of
// simply <code>{ userName : "Bob" }</code>.
// <br>
// This allows better parallel development by enabling the client side of the system to be
// tested in isolation, and creates clearer communication between client and server-side
// developers since creation of test data tends to develop into data requirements
// specifications.
// <br>
// For more info on creating applications that support client-only testing, see
// +link{group:clientOnlyDataSources,Client Only DataSources}.
// <br>&nbsp;</dd>
// </dl>
// <br>
// <h3>Refinements</h3>
// <br>
// <b>Creating vs Showing a View</b>
// <br>
// Many views will be shown to the user repeatedly, for example, the user may repeatedly switch
// back and forth between two panes of a TabSet.  In that usage it makes sense to make a
// distinction between <i>creating</i> a view and <i>showing</i> an existing view.  When
// showing an existing view, the same components and/or data may be able to be reused.
// <br>
// In the following variant on the original example, we only create the Window object and
// do the RPC to retrieve the user name the first time <code>showNewUserWizard()</code> is
// called.  Subsequently we reuse the existing window, and we assume the user name has not
// changed, so we need not do the RPC again. (<i>Note: "New User" button omitted for brevity
// from here on</i>)
// <pre>
//    function showNewUserWizard() {
//        if (!window.myWindow) {
//            Window.create({
//                ID:"myWindow",
//                autoDraw:false,
//                items:[
//                    Canvas.create({ ID: "welcomeCanvas" }),
//                    DynamicForm.create({ ... })
//                ]
//            });
//            RPCManager.sendRequest({
//                actionURL:"getUserName.jsp",
//                callback:"doShow(rpcResponse)"
//            });
//        } else {
//            myWindow.show();
//        }
//    }
//    function doShow(rpcResponse) {
//        welcomeCanvas.setContents("Hello, " + rpcResponse.userName);
//        myWindow.show();
//    }
// </pre>
// 
// <b>Batching Operations</b>
// <br>
// A view may incorporate multiple components, each of which requires data.  In the following
// example, a DataBound ListGrid has been incorporated into the wizard, and we'd like to fetch
// the user's name and the beginning dataset for the grid in the same batch.  We use 
// +link{RPCManager.startQueue()} to do so.
// <pre>
//    function showNewUserWizard() {
//        if (!window.myWindow) {
//            Window.create({
//                ID:"myWindow",
//                autoDraw:false,
//                items:[
//                    Canvas.create({ ID: "welcomeCanvas" }),
//                    <b>ListGrid.create({ 
//                        ID: "myGrid",
//                        dataSource:"myDataSource"
//                    }),</b>
//                    DynamicForm.create({ ... })
//                ]
//            });
//            <b>RPCManager.startQueue();
//            myGrid.fetchData();</b>
//            RPCManager.sendRequest({
//                actionURL:"getUserName.jsp",
//                callback:"doShow(rpcResponse)"
//            });
//            <b>RPCManager.sendQueue();</b>
//        } else {
//            myWindow.show();
//        }
//    }
//    function doShow(rpcResponse) {
//        welcomeCanvas.setContents("Hello, " + rpcResponse.userName);
//        myWindow.show();
//    }
// </pre>
//
// <b>Segmenting very large Applications</b>
// <P>
// If an application has many hundreds of views, but only a handful of views are used by a
// given user in a typical session, for the fastest loading performance you should consider
// loading only the most commonly used views initially then loading further views on demand.
// <P>
// You can use +link{FileLoader.loadJSFiles()} to load a set of JavaScript files
// compromising an application module that defines a set of related views.  The loaded
// JavaScript files may define new component classes and new DataSources in addition to
// defining new views and their associated logic.
//
// @title SmartClient Architecture
// @treeLocation Concepts
// @visibility external
//<

