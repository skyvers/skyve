/*
 * Isomorphic SmartClient
 * Version SNAPSHOT_v10.1p_2015-12-10 (2015-12-10)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */
//>	@object	Params
//
//	Generate an array of parameters for a particular window/frame or URL.
//	One is generated automatically for the default frame and called "isc.params"
//	or you can create one for any other window, frame or URL.
//
//	To access the parameters of the window by name, simply access that
//	property of the params object:
//
//		alert("Parameter 'action' of this page is " + params.action);
//
//	To create a new params object, call the window level function and pass a window handle:
//
//		var otherWindow = window.open(...);
//		var otherWindowParams = getParams(otherWindow)
//
//	or pass a URL
//
//		var myParams = getParams("http://yoursite.com/page.html?foo=bar");
//
//	NOTE: this is not a class, but rather a simple JS object since
//		we do not want to potentially conflict the values of the params
//		with the built-in stuff in the Class object.
//<
isc.addGlobal("Params", function (frame) {
	// if no frame passed in, use the window this executes in
	if (!frame) frame = window;
	// convert the frame to an href string
    // Note: can't use isA because Params is part of the ISC_FileLoader module, which does not
    // include ISA
    var url = typeof frame == "string" ? frame : frame.location.href;

	// get the location of the question mark
	var questionIndex = url.indexOf("?"),
        // The params end at the first "#", or the end of the url
        hashIndex = url.indexOf("#");
    if (hashIndex < 0 || hashIndex< questionIndex) hashIndex = url.length;
    
	if (questionIndex != -1) {        
		var params = url.substring(questionIndex+1, hashIndex).split("&");
        //alert("paramPairs: " + params);
		for (var i = 0, param, equalIndex; i < params.length; i++) {
			param = params[i];
			if (!param) continue;
			equalIndex = param.indexOf("=");
            //alert("param: " + [it.substring(0, equalIndex),unescape(it.substring(equalIndex+1))]);
			this[param.substring(0, equalIndex)] = unescape(param.substring(equalIndex+1));
		}
    }
})


// create a default "params" object for applications to use
isc.params = new isc.Params();

//>	@function	getParams()
//		Create a top-level function called getParams() that creates a new params object for you.
//		Access parameters of the window in question by direct access on the returned object:
//		
//			var myParams = getParams(someOtherWindow);
//			alert(myParams.someNamedParameter);
//
//		@param	window		(window | frame | string)		window to get params for
//<
isc.getParams = function (window) { return new isc.Params(window) }
