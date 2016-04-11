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



(function () {
    isc.definingFramework = true;

    function isc_startDefiningFramework(moduleName) {
        isc["module_" + moduleName] = 1;
        isc._moduleStart = isc["_" + moduleName + "_start"] = (isc.timestamp ? isc.timestamp() : new Date().getTime());
        isc._currentModule = moduleName;
    }

    function isc_processCurrentScriptSrc(src) {
        if (src == null) return;
        src = String(src);
        var moduleNamePrefix = "#module=",
            pos = src.indexOf(moduleNamePrefix);
        if (pos >= 0) isc_startDefiningFramework(src.substring(pos + moduleNamePrefix.length));
    }

    if (document.currentScript != null) {
        isc_processCurrentScriptSrc(document.currentScript.src);
    } else {
        var stack = new Error().stack;
        if (stack != null) {
            

            var atText = stack.indexOf(" at ") >= 0 ? " at " : "@";
            var lastAtPos = stack.lastIndexOf(atText);
            if (lastAtPos >= 0) {
                var src = stack.substring(lastAtPos + atText.length);

                // Remove the trailing lineno/colno.
                var re = new RegExp(":\\d+\\s*$");
                var result = re.exec(src);
                if (result) {
                    src = src.substring(0, result.index);
                    result = re.exec(src);
                    if (result) {
                        src = src.substring(0, result.index);
                    }
                }

                isc_processCurrentScriptSrc(src);
            }
        } else if (document.documentMode >= 8) {
            var oldOnerrorHandler = window.onerror;
            window.onerror = function (message, url, lineno) {
                alert(url);
                isc_processCurrentScriptSrc(url);
                window.onerror = oldOnerrorHandler;
                return true;
            };

            window.noSuchMethod();
        } else {
            var scriptElems = document.getElementsByTagName("script");
            var lastScriptElem = scriptElems[scriptElems.length - 1];
            isc_processCurrentScriptSrc(lastScriptElem.src);
        }
    }
})();
