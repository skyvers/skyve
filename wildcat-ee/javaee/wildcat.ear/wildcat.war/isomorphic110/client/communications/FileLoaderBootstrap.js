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
// define a FileLoader stub for bootstrapping, if FileLoader has not been loaded
if (!isc.FileLoader) {
    isc.defineClass("FileLoader").addClassProperties({   
        // flag for SA_Core -> defineClass, allowing redefinition
        _isStub: true,

        // Some of the required modules may be optional modules (RealtimeMessaging, for
        // example).  But even if the user has not purchased a license to these, we still allow
        // the use of them specifically for RemoteDebugging - which requires the bootstrap
        // So: load from development/
        modulesDir: "system/development/",

        ensureLoaded : function (callback) {
            isc.fileLoaderLoaded = function () {
                isc.fileLoaderLoaded = null;
                callback();
            };
            isc.Comm.sendScriptInclude({
                // Note: no callback param - since we control the code in FileLoader,   
                // we have logic there in _pageLoad() to simply call a function on the isc
                // object, if registered once loaded.  This way we don't require server collusion
                URL: isc.Page.getIsomorphicDir()+this.modulesDir+"/ISC_FileLoader.js"
            });
        }
    });

    // FL synonym for FileLoader
    isc.addGlobal("FL", isc.FileLoader);
}
