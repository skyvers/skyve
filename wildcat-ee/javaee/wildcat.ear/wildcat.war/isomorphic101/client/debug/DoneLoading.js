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
//>DEBUG
isc.Page.logInfo("SmartClient Core ("+isc.version+" "+isc.buildDate+") initialized: " + 
                 (isc.timeStamp() - isc._start) + "ms");
//<DEBUG

isc.Page.logInfo("document.compatMode: " + document.compatMode);

if (isc.Log.hasFireBug()) {
	isc.Log.logWarn("NOTE: Firebug is enabled. Firebug greatly slows the performance of " +
		"applications that make heavy use of JavaScript. Isomorphic highly recommends Firebug " +
		"for troubleshooting, but Firebug and other development tools should be disabled when " +
		"assessing the real-world performance of SmartClient applications.");
}

