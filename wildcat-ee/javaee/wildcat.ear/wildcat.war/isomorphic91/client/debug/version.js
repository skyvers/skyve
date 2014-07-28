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


if (window.isc && isc.version != "v9.1p_2014-03-26/LGPL Development Only") {
    isc.logWarn("SmartClient module version mismatch detected: This application is loading the core module from "
        + "SmartClient version '" + isc.version + "' and additional modules from 'v9.1p_2014-03-26/LGPL Development Only'. Mixing resources from different "
        + "SmartClient packages is not supported and may lead to unpredictable behavior. If you are deploying resources "
        + "from a single package you may need to clear your browser cache, or restart your browser."
        + (isc.Browser.isSGWT ? " SmartGWT developers may also need to clear the gwt-unitCache and run a GWT Compile." : ""));
}
