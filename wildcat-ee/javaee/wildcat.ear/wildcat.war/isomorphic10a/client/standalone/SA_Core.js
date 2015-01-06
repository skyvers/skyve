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

 
//--------------------------------------------------------------------------------------------------
// partial addProperties support
//--------------------------------------------------------------------------------------------------
// define addProperties(), but don't redefine it if FileLoader was loaded after ISC
// Note: copied partially from Object.js
if (isc.addProperties == null) {
    isc.addGlobal("addProperties", function (destination, source) {
        for (var propName in source)
            destination[propName] = source[propName];
        return destination;
    });
}

isc.addGlobal("evalSA", function (expression) {
    //!OBFUSCATEOK
    if (isc.eval) isc.eval(expression);
    else eval(expression);                        
});

isc.addGlobal("defineStandaloneClass", function (className, classObj) { 
    if (isc[className]) {
        if (className == "FileLoader" && isc.FileLoader._isStub) {
            // redefinition of FileLoader stub is allowed
            isc[className] = null;
        } else {
            return;  // don't redefine
        }
    }

    isc.addGlobal(className, classObj);
    isc.addProperties(classObj, {
        _saClassName: className,

        fireSimpleCallback : function (callback) {
            callback.method.apply(callback.target ? callback.target : window, 
                                  callback.args ? callback.args : []);  
        },

        // Logging - log to a special array that gets dumped into the the DevConsole logs by
        // Log.js.  Timestamps will be accurate.  If you're not loading Core, you can use
        // getLogs() to get the logs.
        logMessage : function (priority, message, category) {
            if (isc.Log) {
                isc.Log.logMessage(priority, message, category);
                return;
            }
            if (!isc._preLog) isc._preLog = [];
            isc._preLog[isc._preLog.length] = {
                priority: priority,
                message: message,
                category: category,
                timestamp: new Date()
            };
        },
        

        // NOTE: log priorities copied from Log.js
        logError : function (message) {
            this.logMessage(2, message, this._saClassName);
        },
        logWarn : function (message) {
            this.logMessage(3, message, this._saClassName);
        },
        logInfo : function (message) {
            this.logMessage(4, message, this._saClassName);
        },
        logDebug : function (message) {
            this.logMessage(5, message, this._saClassName);
        },
        // end logging

        _assert : function (b, message) {
            if (!b) {
                throw (message || "assertion failed");
            }
        },

        //--------------------------------------------------------------------------------------------------
        // IsA support
        //--------------------------------------------------------------------------------------------------
        // Note: can't provide this as isc.isA because in Core.js we load Object before isA and Object
        // has conditional logic that uses isA
        // 
        // Also, ClassFactory.makeIsAFunc() expect isA to always be a function, so don't stick
        // an isA object literal on here or it will crash
        isAString : function (object) {
            if (object == null) return false;
            if (object.constructor && object.constructor.__nativeType != null) {
                return object.constructor.__nativeType == 4;
            }
            return typeof object == "string";
        },

        _singleQuoteRegex: new RegExp("'", "g"),
        _doubleQuoteRegex: new RegExp("\"", "g"),
        _asSource : function (string, singleQuote) {
            if (!this.isAString(string)) string = String(string);

            var quoteRegex = singleQuote ? this._singleQuoteRegex : this._doubleQuoteRegex,
                outerQuote = singleQuote ? "'" : '"';
            return outerQuote +
                       string.replace(/\\/g, "\\\\")
                             // quote whichever quote we use on the outside
                             .replace(quoteRegex, '\\' + outerQuote)
                             .replace(/\t/g, "\\t")
                             .replace(/\r/g, "\\r")
                             .replace(/\n/g, "\\n")
                             .replace(/\u2028/g, "\\u2028")
                             .replace(/\u2029/g, "\\u2029") + outerQuote;
        },

        _asHTML : function (string, noAutoWrap) {
            if (!this.isAString(string)) string = String(string);
            var s = string.replace(/&/g, "&amp;")
                        .replace(/</g, "&lt;")
                        .replace(/>/g,"&gt;")
                        // if we don't do this, we lose the leading space after a crlf because all
                        // browsers except IE in compat (non-standards) mode treat a <BR> followed by a
                        // space as just a <BR> (the space is ignored)
                        .replace(/(\r\n|\r|\n) /g,"<BR>&nbsp;")
                        .replace(/(\r\n|\r|\n)/g,"<BR>")
                        .replace(/\t/g,"&nbsp;&nbsp;&nbsp;&nbsp;");
            // in autoWrap mode, replace two spaces with a space and an &nbsp; to preserve wrapping to
            // the maximum extent possible
            return (noAutoWrap ? s.replace(/ /g, "&nbsp;") : s.replace(/  /g, " &nbsp;"));
        }

    });

    // alias
    classObj.isAn = classObj.isA;

    return classObj; 
});
