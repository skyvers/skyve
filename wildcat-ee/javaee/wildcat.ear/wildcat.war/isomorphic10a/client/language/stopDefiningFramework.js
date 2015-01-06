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


(function () {
    delete isc.definingFramework;

    if (isc._currentModule) {
        var moduleName = isc._currentModule;
        if (isc.Page) {
            isc._moduleEnd = isc["_" + moduleName + "_end"] = (isc.timestamp ? isc.timestamp() : new Date().getTime());
            isc.Page.handleEvent(null, "moduleLoaded", {
                moduleName: moduleName,
                loadTime: (isc._moduleEnd - isc._moduleStart)
            });
        }

        isc._lastModule = moduleName;
        delete isc._currentModule;
    }
})();
