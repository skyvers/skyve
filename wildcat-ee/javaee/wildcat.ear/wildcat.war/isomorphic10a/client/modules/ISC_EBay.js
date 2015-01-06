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



//
// This script will load all of the Isomorhic SmartClient Application Framework libraries for you
//
// The idea is that in your app file you can just load the script "Isomorphic_SmartClient.js" which
// in a production situation would be all of the scripts jammed together into a single file.
//
// However, it's easier to work on the scripts as individual files, this file will load all of the
// scripts individually for you (with a speed penalty).
//		
var libs = 
	[
        "application/eBaySvc_wsdl",
        "application/EBay"
	];

//<STOP PARSING 

// The following code only executes if the script is being dynamically loaded.

// the following statement allows a page that is not in the standard location to take advantage of
// dynamically loaded scripts by explicitly setting the window.isomorphiDir variable itself.
if (! window.isomorphicDir) window.isomorphicDir = "../isomorphic/";

// dynamic loading
(function () {
    function loadLib(lib, hash) {
        if (hash == null) hash = "";
        document.write("<"+"script src='" + window.isomorphicDir + "client/" + lib + ".js" + hash + "' type='text/javascript' charset='UTF-8'><"+"/script>");
    }

    loadLib("language/startDefiningFramework", "#module=EBay");
    for (var i = 0, l = libs.length; i < l; ++i) {
        if (!libs[i]) continue;
        if (window.UNSUPPORTED_BROWSER_DETECTED == true) break;
        loadLib(libs[i]);
    }
    loadLib("language/stopDefiningFramework", "#module=EBay");
})();
