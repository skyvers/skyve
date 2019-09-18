/*

  SmartClient Ajax RIA system
  Version v12.0p_2019-08-11/LGPL Deployment (2019-08-11)

  Copyright 2000 and beyond Isomorphic Software, Inc. All rights reserved.
  "SmartClient" is a trademark of Isomorphic Software, Inc.

  LICENSE NOTICE
     INSTALLATION OR USE OF THIS SOFTWARE INDICATES YOUR ACCEPTANCE OF
     ISOMORPHIC SOFTWARE LICENSE TERMS. If you have received this file
     without an accompanying Isomorphic Software license file, please
     contact licensing@isomorphic.com for details. Unauthorized copying and
     use of this software is a violation of international copyright law.

  DEVELOPMENT ONLY - DO NOT DEPLOY
     This software is provided for evaluation, training, and development
     purposes only. It may include supplementary components that are not
     licensed for deployment. The separate DEPLOY package for this release
     contains SmartClient components that are licensed for deployment.

  PROPRIETARY & PROTECTED MATERIAL
     This software contains proprietary materials that are protected by
     contract and intellectual property law. You are expressly prohibited
     from attempting to reverse engineer this software or modify this
     software for human readability.

  CONTACT ISOMORPHIC
     For more information regarding license rights and restrictions, or to
     report possible license violations, please contact Isomorphic Software
     by email (licensing@isomorphic.com) or web (www.isomorphic.com).

*/

var isc = window.isc ? window.isc : {};if(window.isc&&!window.isc.module_History){isc.module_History=1;isc._moduleStart=isc._History_start=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc._moduleEnd&&(!isc.Log||(isc.Log && isc.Log.logIsDebugEnabled('loadTime')))){isc._pTM={ message:'History load/parse time: ' + (isc._moduleStart-isc._moduleEnd) + 'ms', category:'loadTime'};
if(isc.Log && isc.Log.logDebug)isc.Log.logDebug(isc._pTM.message,'loadTime');
else if(isc._preLog)isc._preLog[isc._preLog.length]=isc._pTM;
else isc._preLog=[isc._pTM]}isc.definingFramework=true;


if (!window.isc || typeof isc.Packager != "object") {


//> @object isc
// The <code>isc</code> object contains global methods and objects of the Isomorphic SmartClient
// framework.
// <P>
// See also +link{group:simpleNamesMode,Simple Names mode}.
//
// @treeLocation Client Reference/System
// @visibility external
//<

//> @groupDef simpleNamesMode
// When SmartClient runs in "simple names" mode (the default), all ISC Classes and several
// global methods are installed as JavaScript global variables, that is, properties of the
// browser's "window" object.  When simple names mode is disabled (called "portal mode"),
// the framework uses only the global variable: "isc" and global variables prefixed with
// "isc_".
// <P>
// Portal mode is intended for applications which must integrate with fairly arbitrary
// JavaScript code written by third-party developers, and/or third party JavaScript frameworks,
// where it is important that each framework stays within it's own namespace.
// <P>
// <smartclient>
// In portal mode, all references to ISC classes and global functions must be prefixed with
// "isc.", for example:<pre>
//
//      Canvas.create(addProperties({}, myDefaults))
//
// </pre>would become<pre>
//
//      isc.Canvas.create(isc.addProperties({}, myDefaults));
//
// </pre>
// </smartclient>
// Portal mode is enabled by setting <code>window.isc_useSimpleNames = false</code> <b>before</b>
// SmartClient is loaded, generally inside the &lt;head&gt; element.
//
// @treeLocation Client Reference/System
// @title Simple Names mode
// @visibility external
//<





var isc = window.isc ? window.isc : {};
isc._start = new Date().getTime();

// versioning - values of the form ${value} are replaced with user-provided values at build time.
// Valid values are: version, date, project (not currently used)
isc.version = "v12.0p_2019-08-11/LGPL Deployment";
isc.versionNumber = "v12.0p_2019-08-11";
isc.buildDate = "2019-08-11";
isc.expirationDate = "";

isc.scVersion = "12.0p";
isc.scVersionNumber = "12.0";
isc.sgwtVersion = "12.0p";
isc.sgwtVersionNumber = "12.0";

// these reflect the latest stable version relative to the branch from which this build is
// created.  So for example for 11.0d/6.0d, this will be 10.1/5.1.  But for 10.0/5.0 this will
// be 10.0/5.0.
isc.scParityStableVersionNumber = "12.0";
isc.sgwtParityStableVersionNumber = "12.0";

// license template data
isc.licenseType = "LGPL";
isc.licenseCompany = "Isomorphic Software";
isc.licenseSerialNumber = "ISC_LGPL_NIGHTLY";
isc.licensingPage = "http://smartclient.com/product/";

isc._$debugModules = "debugModules";
isc._$nonDebugModules = "nonDebugModules";
isc.checkForDebugAndNonDebugModules = function () {
    if (isc.checkForDebugAndNonDebugModules._loggedWarning) return;
    var debugModules = isc['_' + this._$debugModules],
        haveDebugModules = debugModules != null && debugModules.length > 0,
        nonDebugModules = isc['_' + this._$nonDebugModules],
        haveNonDebugModules = nonDebugModules != null && nonDebugModules.length > 0;

    if (haveDebugModules && haveNonDebugModules) {
        isc.logWarn("Both Debug and non-Debug modules were loaded; the Debug versions of '" +
        debugModules.join("', '") + "' and the non-Debug versions of '" + nonDebugModules.join("', '") +
        "' were loaded. Mixing Debug and non-Debug modules is not supported and may lead to " +
        "JavaScript errors and/or unpredictable behavior. " +
        "To fix, ensure that only modules in the modules/ folder or the modules-debug/ " +
        "folder are loaded and clear the browser cache. If using Smart GWT, also clear the " +
        "GWT unit cache and recompile.");
        isc.checkForDebugAndNonDebugModules._loggedWarning = true;
    }
};

isc._optionalModules = {
    SCServer: {present: "false", name: "SmartClient Server", serverOnly: true, isPro: true},
    Drawing: {present: "true", name: "Drawing Module"},
    PluginBridges: {present: "true", name: "PluginBridges Module"},
    RichTextEditor: {present: "true", name: "RichTextEditor Module"},
    Calendar: {present: "true", name: "Calendar Module"},
    Analytics: {present: "false", name: "Analytics Module"},
    Charts: {present: "false", name: "Charts Module"},
    Tools: {present: "${includeTools}", name: "Dashboards and Tools Module"},
    NetworkPerformance: {present: "false", name: "Network Performance Module"},
    // alias for NetworkPerformance
    FileLoader: {present: "false", name: "Network Performance Module"},
    RealtimeMessaging: {present: "false", name: "RealtimeMessaging Module"},
    // Enterprise Features
    serverCriteria: {present: "false", name: "Server Advanced Filtering", serverOnly: true, isFeature: true},
    customSQL: {present: "false", name: "SQL Templating", serverOnly: true, isFeature: true},
    chaining: {present: "false", name: "Transaction Chaining", serverOnly: true, isFeature: true},
    batchDSGenerator: {present: "false", name: "Batch DS-Generator", serverOnly: true, isFeature: true},
    batchUploader: {present: "false", name: "Batch Uploader", serverOnly: true, isFeature: true},
    transactions: {present: "false", name: "Automatic Transaction Management", serverOnly: true, isFeature: true}
};
isc.canonicalizeModules = function (modules) {
    if (!modules) return null;

    // canonicalize to Array, split on comma
    if (isc.isA.String(modules)) {
        if (modules.indexOf(",") != -1) {
            modules = modules.split(",");
            var trimLeft = /^\s+/, trimRight = /\s+$/;
            for (var i=0; i<modules.length; i++) {
                modules[i] = modules[i].replace(trimLeft, "").replace(trimRight, "");
            }
        } else modules = [modules];
    }
    return modules;
};
isc.hasOptionalModules = function (modules) {
    // ease of use shortcut, null value means no optional module requirements
    if (!modules) return true;

    modules = isc.canonicalizeModules(modules);

    for (var i = 0; i < modules.length; i++) if (!isc.hasOptionalModule(modules[i])) return false;
    return true;
};
isc.getMissingModules = function (requiredModules) {
    var result = [];
    requiredModules = isc.canonicalizeModules(requiredModules);
    for (var i = 0; i < requiredModules.length; i++) {
        var module = requiredModules[i];
        if (!isc.hasOptionalModule(module)) result.add(isc._optionalModules[module]);
    }
    return result;
};
isc.hasOptionalModule = function (module) {
    var v = isc._optionalModules[module];
    if (!v) {
        if(isc.Log) isc.Log.logWarn("isc.hasOptionalModule - unknown module: " + module);
        return false;
    }
    // has module or devenv
    return v.present == "true" || v.present.charAt(0) == "$";
};
isc.getOptionalModule = function (module) {
    return isc._optionalModules[module];
};


isc.$P5hy05Xgj7AN = function (moduleName) {
    if (this.hasOptionalModule(moduleName)) return;
    var moduleEntry = isc._optionalModules[moduleName];
    if (moduleEntry) moduleEntry.present = !!moduleName + "";
};

// default to "simple names" mode, where all ISC classes are defined as global variables
isc._useSimpleNames = window.isc_useSimpleNames;
if (isc._useSimpleNames == null) isc._useSimpleNames = true;

// register with the OpenAjax hub, if present
if (window.OpenAjax) {
    // OpenAjax insists on only numbers and dots.  This regex will convert eg 5.6b3 to 5.6.03,
    // which is not really accurate
    isc._numericVersion = isc.versionNumber.replace(/[a-zA-Z_]+/, ".0");
    OpenAjax.registerLibrary("SmartClient", "http://smartclient.com/SmartClient",
                             isc._numericVersion,
                             { namespacedMode : !isc._useSimpleNames,
                               iscVersion : isc.version,
                               buildDate : isc.buildDate,
                               licenseType : isc.licenseType,
                               licenseCompany : isc.licenseCompany,
                               licenseSerialNumber : isc.licenseSerialNumber });
    OpenAjax.registerGlobals("SmartClient", ["isc"]);
}


isc._longDOMIds = window.isc_useLongDOMIDs;

// add a property to global scope.  This property will always be available as "isc[propName]" and
// will also be available as "window[propName]" if we are in "simpleNames" mode.
// NOTE: even in simpleNames mode, where we assume it's OK to put things into global scope, we
// should still think carefully about creating globals.  Eg a variable like "params" which holds the
// current URL parameters (which we used to have) could easily get clobbered by some sloppy global
// JS, causing mysterious crashes.  Consider creating a class method (eg Page.getWidth()) or class
// property (Log.logViewer) instead, or making the variable isc.myMethod() or isc.myProperty.
isc._$iscPrefix = "isc.";
isc.addGlobal = function (propName, propValue) {
    if (propName.indexOf(isc._$iscPrefix) == 0) propName = propName.substring(4);
    isc[propName] = propValue;
    if (isc._useSimpleNames) window[propName] = propValue;
}





//>Offline

//XXX need to determine this flag correctly at load time
isc.onLine = true;

isc.isOffline = function () {
    return !isc.onLine;
};
isc.goOffline = function () { isc.onLine = false; };
isc.goOnline = function () { isc.onLine = true; };
if (window.addEventListener) {
    window.addEventListener("online", isc.goOnline, false);
    window.addEventListener("offline", isc.goOffline, false);
}
//<Offline


}




if (typeof isc.Browser != "object") {




//> @class Browser
// The <code>Browser</code> class contains various class attributes that indicate basic properties
// of the browser and whether certain features are enabled.
// @treeLocation Client Reference/Foundation
// @visibility external
//<
isc.addGlobal("Browser", {
    isSupported: false


    ,_assert : function (b, message) {
        if (!b) {
            message = "assertion failed" + (message ? " with message: '" + message + "'" : "");

            if (isc.logWarn) {
                isc.logWarn(message + ". Stack trace:" + isc.Class.getStackTrace());

            } else if (console) { // useful fallback crash info before logging loads
                console.log(message);
                console.trace();
            }


        }
    }


});


// ----------------------------------------------------------------
// Detecting browser type
// ----------------------------------------------------------------
// Bot/Crawler discriminators.  In many cases the bots will use something approximating a
// browser engine to "run your javascript", but the UA string, which we use to determine the
// proper rendering path, either does not reflect the engine type at all or reflects a generic
// "Mozilla/5.0" or similar.  So first, we detect the bot type, and then this is used below to
// also set browser type - e.g. isGoogleBot -> isChrome + specific version from docs

// https://support.google.com/webmasters/answer/1061943?hl=en
isc.Browser.isGoogleBot = navigator.userAgent.indexOf("Googlebot/") != -1;
// https://www.bing.com/webmaster/help/which-crawlers-does-bing-use-8c184ec0
isc.Browser.isBingBot = navigator.userAgent.indexOf("bingbot/") != -1;
// https://perishablepress.com/list-all-user-agents-top-search-engines/
// Note that DuckDuckGo appears to use Bing, Yahoo, and Yandex and only go out directly for
// "answers": https://duck.co/help/results/sources
isc.Browser.isDuckDuckBot = navigator.userAgent.indexOf("DuckDuckBot/") != -1;
// https://help.yahoo.com/kb/SLN22600.html?guccounter=1
isc.Browser.isYahooBot = navigator.userAgent.indexOf("Slurp;") != -1;
// http://www.baiduguide.com/baidu-spider/
isc.Browser.isBaiduBot = navigator.userAgent.indexOf("Baiduspider") != -1;
// https://yandex.com/support/webmaster/robot-workings/check-yandex-robots.xml
isc.Browser.isYandexBot = navigator.userAgent.indexOf("YandexBot/") != -1;

// generic bot discriminator
isc.Browser.isBot = isc.Browser.isGoogleBot || isc.Browser.isBingBot ||
        isc.Browser.isDuckDuckBot || isc.Browser.isYahooBot || isc.Browser.isBaiduBot ||
        isc.Browser.isYandexBot;

//>    @classAttr    Browser.isOpera        (boolean : ? : R)
//        Are we in Opera ?
//<

isc.Browser.isOpera = (navigator.appName == "Opera" ||
                    navigator.userAgent.indexOf("Opera") != -1);

//console.log("navigator.appName:" + navigator.appName
//            + ", navigator.userAgent:" + navigator.userAgent);
//console.log("is opera?:" + isc.Browser.isOpera);

//>    @classAttr    Browser.isNS (boolean : ? : R)
//        Are we in Netscape (including Navigator 4+, NS6 & 7, and Mozilla)
//      Note: Safari also reports itself as Netscape, so isNS is true for Safari.
//<
isc.Browser.isNS = (navigator.appName == "Netscape" && !isc.Browser.isOpera);
//console.log("is NS?:" + isc.Browser.isNS);

//>    @classAttr    Browser.isIE        (boolean : ? : R)
//        Are we in Internet Explorer?
//<
isc.Browser.isIE = (navigator.appName == "Microsoft Internet Explorer" &&
                    !isc.Browser.isOpera) ||
                   navigator.userAgent.indexOf("Trident/") != -1;
//console.log("is IE?:" + isc.Browser.isIE);

//>    @classAttr    Browser.isMSN        (boolean : ? : R)
//      Are we in the MSN browser (based on MSIE, so isIE will be true in this case)
//<
isc.Browser.isMSN = (isc.Browser.isIE && navigator.userAgent.indexOf("MSN") != -1);
//console.log("is MSN?:" + isc.Browser.isMSN);


//>    @classAttr    Browser.isMoz        (boolean : ? : R)
//        Are we in any Mozilla-derived browser, that is, a browser based on Netscape's Gecko
//      engine? (includes Mozilla and Netscape 6+)
//<
isc.Browser.isMoz = (navigator.userAgent.indexOf("Gecko") != -1) &&
    // NOTE: Safari sends "(like Gecko)", but behaves differently from Moz in many ways

    (navigator.userAgent.indexOf("Safari") == -1) &&
    (navigator.userAgent.indexOf("AppleWebKit") == -1) &&
    !isc.Browser.isIE;
//console.log("is Moz?:" + isc.Browser.isMoz);

//>    @classAttr    Browser.isCamino (boolean : false : R)
//  Are we in Mozilla Camino?
//<
isc.Browser.isCamino = (isc.Browser.isMoz && navigator.userAgent.indexOf("Camino/") != -1);
//console.log("is Camino?:" + isc.Browser.isCamino);


//>    @classAttr    Browser.isFirefox (boolean : false : R)
//  Are we in Mozilla Firefox?
//<
isc.Browser.isFirefox = (isc.Browser.isMoz && navigator.userAgent.indexOf("Firefox/") != -1);
//console.log("is Fire Fox?:" + isc.Browser.isFirefox);


//> @classAttr  Browser.isAIR    (boolean : ? : R)
// Is this application running in the Adobe AIR environment?
//<
isc.Browser.isAIR = (navigator.userAgent.indexOf("AdobeAIR") != -1);
//console.log("is AIR?:" + isc.Browser.isAIR);


//>    @classAttr    Browser.isWebKit (boolean : ? : R)
// Are we in a WebKit-based browser (Safari, Chrome, mobile Safari and Android, others).
//<
isc.Browser.isWebKit = navigator.userAgent.indexOf("WebKit") != -1;
//console.log("is webkit?:" + isc.Browser.isWebKit);


//>    @classAttr    Browser.isSafari (boolean : ? : R)
// Are we in Apple's "Safari" browser? Note that this property will also be set for other
// WebKit based browsers (such as Google Chrome).
//<
// As far as we know all "true" Safari implementations identify themselves in the userAgent with
// the string "Safari", but so does Microsoft Edge, so that can't be used to identify "true"
// Safari.  GWT hosted mode browser on OSX is also based on apple webkit and should be treated
// like Safari but is not a Safari browser and doesn't identify itself as such in the userAgent.
// Reported UserAgent:
//  Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_5_5; en-us) AppleWebKit/525.18 (KHTML, like Gecko)
isc.Browser.isSafari = isc.Browser.isAIR || navigator.userAgent.indexOf("Safari") != -1 ||
        navigator.userAgent.indexOf("AppleWebKit") != -1 ||
        // GoogleBot uses the Chrome engine: https://developers.google.com/search/docs/guides/rendering
        // and isChrome implies isSafari (see below in the isChrome definition)
        isc.Browser.isGoogleBot;
//console.log("is Safari?:" + isc.Browser.isSafari);

//> @classAttr Browser.isEdge (boolean : ? : R)
// Are we in the Microsoft Edge browser?
//<

isc.Browser.isEdge = isc.Browser.isSafari && (navigator.userAgent.indexOf("Edge/") != -1);
//console.log("is Edge?:" + isc.Browser.isEdge);


//> @classAttr Browser.isChrome (boolean : ? : R)
// Are we in the Google Chrome browser?
//<
// Behaves like Safari in most ways.  Note: do not detect Edge as Chrome - causes odd scrollbar
// misrenderings.  As of 7/30/2015 appears to work better with the isSafari codepaths
// Note: the Google crawler does not identify itself as Chrome, even though it is in fact using
// the Chrome engine.  This leads to us mis-detecting the render path and producing all sorts
// of errors that result in a failure to render at all as far as the crawler is concerned.
isc.Browser.isChrome = (isc.Browser.isSafari && !isc.Browser.isEdge &&
                        (navigator.userAgent.indexOf("Chrome/") != -1)) ||
        // GoogleBot uses the Chrome engine: https://developers.google.com/search/docs/guides/rendering
        isc.Browser.isGoogleBot;
//console.log("is Chrome?:" + isc.Browser.isChrome);

//>    @classAttr    Browser.isSafariStrict (boolean : ? : R)
// Are we in Apple's "Safari" browser? This property is only set on "true" Safari browsers.
//<

isc.Browser.isSafariStrict = isc.Browser.isSafari &&
        !isc.Browser.isAIR && !isc.Browser.isEdge && !isc.Browser.isChrome;
//console.log("is \"true\" Safari?:" + isc.Browser.isSafariStrict);


if (!isc.Browser.isIE && !isc.Browser.isOpera && !isc.Browser.isMoz &&
    !isc.Browser.isAIR && !isc.Browser.isWebkit && !isc.Browser.isSafari)
{
    if (navigator.appVersion.indexOf("MSIE") != -1) {
        isc.Browser.isIE = true;
        //console.log("is IE (inside embedded browser, etc)?:" + isc.Browser.isIE);

    }
}

// ----------------------------------------------------------------
// END Detecting browser type
// ----------------------------------------------------------------


//>    @classAttr Browser.minorVersion        (number : ? : R)
//        Browser version, with minor revision included (4.7, 5.5, etc).
//
// NOTE: In Firefox 16+, Browser.minorVersion will equal Browser.version by design. See
// Firefox +externalLink{https://bugzilla.mozilla.org/show_bug.cgi?id=728831,Bug 728831}.
//<
if (navigator.userAgent.indexOf("Trident/") >= 0 &&
    navigator.userAgent.lastIndexOf("rv:") >= 0)
{

    isc.Browser.minorVersion = parseFloat(navigator.userAgent.substring(navigator.userAgent.lastIndexOf("rv:") + "rv:".length));
} else {
    isc.Browser.minorVersion = parseFloat(isc.Browser.isIE
                                      ? navigator.appVersion.substring(navigator.appVersion.indexOf("MSIE") + 5)
                                      : navigator.appVersion );
}

if (isc.Browser.isIE) {
    // IE won't allow a documentMode higher than the version you are on.

    if (document.documentMode != null) {
        isc.Browser.minorVersion = Math.max( isc.Browser.minorVersion, document.documentMode );
    }
} else (function () {



    // per https://developers.google.com/search/docs/guides/rendering, accurate as of 6/5/2018
    // This number is not surfaced by the bot, so must hardcode
    if (isc.Browser.isGoogleBot) {
        isc.Browser.minorVersion = 41;
        return;
    }

    var needle, pos;
    if (navigator.appVersion) {
        // Safari
        needle = "Version/";
        pos = navigator.appVersion.indexOf(needle);
        if (pos >= 0) {
            isc.Browser.minorVersion = parseFloat(navigator.appVersion.substring(pos + needle.length));
            return;
        }
    }

    var ua = navigator.userAgent;

    needle = "Chrome/";
    pos = ua.indexOf(needle);
    if (pos >= 0) {
        isc.Browser.minorVersion = parseFloat(ua.substring(pos + needle.length));
        return;
    }

    // Handle Camino before Firefox because Camino includes "(like Firefox/x.x.x)" in the UA.
    needle = "Camino/";
    pos = ua.indexOf(needle);
    if (pos >= 0) {
        isc.Browser.minorVersion = parseFloat(ua.substring(pos + needle.length));
        return;
    }

    needle = "Firefox/";
    pos = ua.indexOf(needle);
    if (pos >= 0) {
        isc.Browser.minorVersion = parseFloat(ua.substring(pos + needle.length));
        return;
    }

    if (ua.indexOf("Opera/") >= 0) {
        needle = "Version/";
        pos = ua.indexOf(needle);
        if (pos >= 0) {
            isc.Browser.minorVersion = parseFloat(ua.substring(pos + needle.length));
            return;
        } else {
            // Opera 9.64
            needle = "Opera/";
            pos = ua.indexOf(needle);
            isc.Browser.minorVersion = parseFloat(ua.substring(pos + needle.length));
            return;
        }
    }
})();

//>    @classAttr    Browser.version        (number : ? : R)
//        Browser major version number (integer: 4, 5, etc).
//<
isc.Browser.version = parseInt(isc.Browser.minorVersion);

// actually means IE6 or earlier, which requires radically different optimization techniques
isc.Browser.isIE6 = isc.Browser.isIE && isc.Browser.version <= 6;

//> @classAttr Browser.supportsHTML5Audio (boolean : varies : RA)
// Does this browser support HTML5 Audio via the &lt;AUDIO&gt; element?
//<
isc.Browser.supportsHTML5Audio = (document.createElement("audio") != null) ? "play" : null;

//>    @classAttr    Browser.caminoVersion (String : ? : R)
//        For Camino-based browsers, the Camino version number.
//<
if (isc.Browser.isCamino) {
    // Camino Version is the last thing in the userAgent
    isc.Browser.caminoVersion =
        navigator.userAgent.substring(navigator.userAgent.indexOf("Camino/") +7);
}

if (isc.Browser.isFirefox) {
//>    @classAttr    Browser.firefoxVersion (String : ? : R)
//        For Firefox-based browsers, the Firefox version number.
//          - 0.10.1    is Firefox PR 1
//      After this the version numbers reported match those in the about dialog
//          - 1.0       is Firefox 1.0
//          - 1.0.2     is Firefox 1.0.2
//          - 1.5.0.3   is Firefox 1.5.0.3
//<
    var userAgent = navigator.userAgent,
        firefoxVersion = userAgent.substring(userAgent.indexOf("Firefox/")+ 8),
        majorMinorVersion = firefoxVersion.replace(/([^.]+\.[^.]+)\..*/, "$1");
    isc.Browser.firefoxVersion          = firefoxVersion;
    isc.Browser.firefoxMajorMinorNumber = parseFloat(majorMinorVersion);
}

//>    @classAttr    Browser.geckoVersion (Integer : ? : R)
//        For Gecko-based browsers, the Gecko version number.
//      Looks like a datestamp:
//          - 20011019 is Netscape 6.2
//          - 20020530 is Mozilla 1.0
//          - 20020823 is Netscape 7.0
//          - 20020826 is Mozilla 1.1
//          - 20021126 is Mozilla 1.2
//          - 20030312 is Mozilla 1.3
//          - 20030624 is Mozilla 1.4
//          - 20031007 is Mozilla 1.5
//          - 20031120 is Mozilla 1.5.1 (Mac only release)
//          - 20040113 is Mozilla 1.6
//          - 20040616 is Mozilla 1.7
//          - 20040910 is Mozilla 1.73
//          - 20041001 is Mozilla Firefox PR1 (-- also see firefox version)
//          - 20041107 is Mozilla Firefox 1.0
//          - 20050915 is Mozilla Firefox 1.0.7
//          - 20051107 is Mozilla Firefox 1.5 RC2
//          - 20051111 is Mozilla Firefox 1.5 final
//          - 20060426 is Mozilla Firefox 1.5.0.3
//          - 20061010 is Mozilla Firefox 2.0
//          - 20070321 is Netscape 8.1.3 - LIES - really based on Firefox 1.0 codebase
//          - 20071109 is Firefox 3.0 beta 1
//          - 20080529 is Firefox 3.0
//          - 20100101 is Firefox 4.0.1
//<

if (isc.Browser.isMoz) {
    isc.Browser._geckoVIndex = navigator.userAgent.indexOf("Gecko/") + 6;
    // The 'parseInt' actually means we could just grab everything from the
    // end of "Gecko/" on, as we know that even if the gecko version is followed
    // by something, there will be a space before the next part of the UA string
    // However, we know the length, so just use it
    isc.Browser.geckoVersion = parseInt(
        navigator.userAgent.substring(
            isc.Browser._geckoVIndex, isc.Browser._geckoVIndex+8
        )
    );



    if (isc.Browser.isFirefox) {
        // clamp 1.0.x series to last known pre 1.5 version (1.0.7)
        if (isc.Browser.firefoxVersion.match(/^1\.0/)) isc.Browser.geckoVersion = 20050915;
        // clamp 2.0.x series to one day before near-final FF3 beta
        else if (isc.Browser.firefoxVersion.match(/^2\.0/)) isc.Browser.geckoVersion = 20071108;
    }


    if (isc.Browser.version >= 17) isc.Browser.geckoVersion = 20121121;
}

// Doctypes
//  Are we in strict standards mode.  This applies to IE6+ and all Moz 1.0+.
//
//  In strict mode, browsers attempt to behave in a more standards-compliant manner.  Of course,
//  standards interpretation varies pretty drastically between browser makers, so this is in effect
//  just another fairly arbitrary set of behaviors which continues to vary across browser makers,
//  and now also across modes within the same browser.
//
// Traditionally, we have essentially 3 cases to consider:
// - BackCompat / Quirks mode. This is the rendering used if docType is not specified, or if
//   specified as 'Transitional' or 'Frameset' / with no URI
//   (EG: <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">)
//   This is the default mode.
// - Strict. Completely standards complient.
//   Triggered by
//   <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
// - "Almost Strict" (AKA Transitional).
//   In IE this matches Strict mode completely.
//   In Moz it matches strict mode except for rendering of images within tables - see
//   http://developer.mozilla.org/en/docs/Images%2C_Tables%2C_and_Mysterious_Gaps
//   Triggered "transitional" doctype with URI
//   Reports document.compatMode as "CSS1Compat"
// - http://developer.mozilla.org/en/docs/Gecko%27s_%22Almost_Standards%22_Mode
// - http://www.htmlhelp.com/reference/html40/html/doctype.html
// - http://developer.mozilla.org/en/docs/Mozilla%27s_DOCTYPE_sniffing
//
// - we also have the HTML5 doctype to consider - <!DOCTYPE html>. Only applies to modern
//   browsers, and required for some of our more recent features (EG some drawing approaches)
//   We don't explicitly have a flag to differentiate between this and "isStrict"

//> @classAttr  Browser.isStrict    (boolean : ? : R)
//  Are we in strict standards mode.
//<
// HACK: Netscape6 does not report document.compatMode, so we can't tell that a DOCTYPE has been
// specified, but Netscape6 IS affected by a DOCTYPE.  So, in Netscape6, assume we're always in
// strict mode.  At the moment (3/30/03) all strict mode workarounds have identical behavior in
// normal mode.

isc.Browser.isStrict = document.compatMode == "CSS1Compat";
if (isc.Browser.isStrict && isc.Browser.isMoz) {

    isc.Browser._docTypePublicID = document.doctype.publicId;
    isc.Browser._docTypeSystemID = document.doctype.systemId;

}

// See http://developer.mozilla.org/en/docs/Mozilla%27s_DOCTYPE_sniffing
// See Drawing.test.html for some test cases
isc.Browser.isTransitional = /.*(Transitional|Frameset)/.test((document.all && document.all[0] && document.all[0].nodeValue) || (document.doctype && document.doctype.publicId));

isc.Browser.isIE7 = isc.Browser.isIE && isc.Browser.version == 7;

//> @classAttr Browser.isIE8 (boolean : ? : R)
// Returns true if we're running IE8 and we're in IE8 mode
// IE8 has a 'back-compat' type mode whereby it can run using IE7 rendering logic.
// This is explicitly controlled via the meta tags:
//
//    &lt;meta http-equiv="X-UA-Compatible" content="IE=8" /&gt;
// or
//    &lt;meta http-equiv="X-UA-Compatible" content="IE=7" /&gt;
//
// In beta versions IE8 reported itself version 7 and ran in IE7 mode unless the explicit IE8
// tag was present
// In final versions (observed on 8.0.6001.18702) it reports a browser version of 8 and runs
// in IE8 mode by default - but can be switched into IE7 mode via the explicit IE=7 tag.
//
// We therefore want to check the document.documentMode tag rather than just the standard
// browser version when checking for IE8
//<
isc.Browser.isIE8 = isc.Browser.isIE && isc.Browser.version>=8 && document.documentMode == 8;

//<
//> @classAttr Browser.isIE8Strict (boolean : ? : R)
// Are we in IE8 [or greater] strict mode.
// <P>
// In IE8 when the meta tag is present to trigger IE7 / IE8 mode the document is in
//
//    &lt;meta http-equiv="X-UA-Compatible" content="IE=8" /&gt;
//    &lt;meta http-equiv="X-UA-Compatible" content="IE=7" /&gt;
//
// If this tag is present, the document is in strict mode even if no DOCTYPE was present.
// The presence of this tag can be detected as document.documentMode being 8 rather than 7.
// document.compatMode still reports "CSS1Compat" as with earlier IE.
//<
// IE9 running in IE9 mode will report as IE8Strict:true. This makes sense since rendering quirks
// introduced in IE8 Strict, such as requiring explicit "overflow:hidden" in addition
// to table-layout-fixed in order to clip cells horizontally in tables apply in both places.
// For cases where we really need to distinguish we can check isc.Browser.version or isc.Browser.isIE9

isc.Browser.isIE8Strict = isc.Browser.isIE &&
                            (isc.Browser.isStrict && document.documentMode ==8) ||
                            document.documentMode > 8;

//> @classAttr Browser.isIE9 (boolean : ? : R)
// True if we're running IE9 or later, actually running in the IE9+ documentMode.
//<

isc.Browser.isIE9 = isc.Browser.isIE && isc.Browser.version>=9 && document.documentMode >= 9;

isc.Browser.isIE10 = isc.Browser.isIE && isc.Browser.version >= 10;

isc.Browser.isIE11 = isc.Browser.isIE && isc.Browser.version >= 11;

//> @classAttr  Browser.AIRVersion (String : ? : R)
// If this application running in the Adobe AIR environment, what version of AIR is
// running. Will be a string, like "1.0".
//<
isc.Browser.AIRVersion = (isc.Browser.isAIR ? navigator.userAgent.substring(navigator.userAgent.indexOf("AdobeAir/") + 9) : null);


//>    @classAttr    Browser.safariVersion (number : ? : R)
//        in Safari, what is is the reported version number
//<

if (isc.Browser.isSafari) {

    if (isc.Browser.isAIR) {

        isc.Browser.safariVersion = 530;
    } else {
        if (navigator.userAgent.indexOf("Safari/") != -1) {
            isc.Browser.rawSafariVersion = navigator.userAgent.substring(
                        navigator.userAgent.indexOf("Safari/") + 7
            );
        } else if (navigator.userAgent.indexOf("AppleWebKit/") != -1) {
            isc.Browser.rawSafariVersion = navigator.userAgent.substring(
                        navigator.userAgent.indexOf("AppleWebKit/") + 12
            );

        } else {
            isc.Browser.rawSafariVersion = "530"
        }



        isc.Browser.safariVersion = (function () {
            var rawVersion = isc.Browser.rawSafariVersion,
                currentDot = rawVersion.indexOf(".");

            if (currentDot == -1) return parseInt(rawVersion);
            var version = rawVersion.substring(0,currentDot+1),
                nextDot;
            while (currentDot != -1) {
                // Check AFTER the dot
                currentDot += 1;
                nextDot = rawVersion.indexOf(".", currentDot);
                version += rawVersion.substring(currentDot,
                                                (nextDot == -1 ? rawVersion.length: nextDot));
                currentDot = nextDot;
            }
            return parseFloat(version);
        })();
    }
}

// -------------------------------------------------------------------
// Platform information
// -------------------------------------------------------------------

//>    @classAttr    Browser.isWin        (boolean : ? : R)
//        Is this a Windows computer ?
//<
isc.Browser.isWin = navigator.platform.toLowerCase().indexOf("win") > -1;
if (isc.Browser.isWin) {
    // install winVersion as float
    isc.Browser.winVersion = function() {
        var windowsMatch = navigator.userAgent.match(/Windows NT[ ]*([0-9.]+)[^0-9.]/i);

        if (windowsMatch) return parseFloat(windowsMatch[1]);
    }();

}

// NT 5.0 is Win2k, NT 5.0.1 is Win2k SP1
isc.Browser.isWin2k = isc.Browser.winVersion >= 5.0 && isc.Browser.winVersion < 5.1;


//>    @classAttr    Browser.isMac        (boolean : ? : R)
//        Is this a Macintosh computer ?
//<
isc.Browser.isMac = navigator.platform.toLowerCase().indexOf("mac") > -1;

isc.Browser.isUnix = (!isc.Browser.isMac &&! isc.Browser.isWin);

//> @groupDef mobileDevelopment
// SmartClient is designed to automatically adapt to smaller screen sizes and the lower
// accuracy of touch-based interfaces.
// <p>
// In general, a SmartClient application written with complete ignorance of mobile development
// will still be highly usable on tablet or handset-sized touch devices.  This topic explains
// all the automatic behaviors that make this possible, and the few areas developers need to
// consider in order to optimize the mobile experience, the most important being:
// <p>
// <ul>
// <li> read about potential issues created by the automatically shown and hidden browser
//      toolbars in Safari on iOS7+, discussed under "minimal-ui" below.  SmartClient
//      automatically handles this, but most applications will want to create a non-interactive
//      banner to fill the blank screen area that is rendered unusable by iOS' behavior
// <li> read about "Automatic touch scrolling" below - if your application does not already
//      have alternative UIs for performing drag operations (as is required anyway for
//      +link{group:accessibility,accessibility reasons}), this section discusses options for
//      controlling drag scrolling vs dragging of data
// <li> review your application for the rare screen that has a fixed, very wide width and
//      doesn't allow scrolling.  Such screens would already be unusable for narrow desktop
//      browsers but are more of a problem for fixed-size mobile screens.  The section
//      "Exceptionally wide screens" below explains strategies for dealing with this.
// </ul>
// <p>
// <h3>Supported Browsers</h3>
// <P>
// <ul>
// <li> Safari on iOS devices (iPad, iPhone, iPod Touch)
// <li> Android's default (WebKit-based) browser <b>*</b>
// <li> Windows Phone default browser, latest release only <b>**</b>
// <li> Blackberry 10+ default (WekKit-based) browser <b>**</b>
// </ul>
// <b>*</b>: Android issues that occur <i>exclusively</i> on rare devices (under a certain
// percent of market share) will not normally be covered by a Support plan.  This is a
// necessity because highly customized versions of Android are used for a variety of niche
// devices (even microsatellites)<br>
// <b>**</b>: These browsers generally work and bug reports are accepted, but they do not yet
// fall under the normal Enterprise+ Support guarantee of fixing every confirmed bug
// <p>
// If you would like to check whether a specific device falls under normal Support, or would
// like a quote for a Support plan that would include a specific device or platform,
// +externalLink{http://smartclient.com/company/contact.jsp,contact Isomorphic here}.
// <P>
// <h3>Adaptive Components</h3>
// <p>
// Many SmartClient components automatically change their behavior and/or appearance when used
// with touch devices in general, or tablets and handsets specifically.  There are too many
// adaptations to comprehensively list, but some of the more obvious behaviors are listed below:
// <ul>
// <li> +link{SelectItem} and +link{ComboBoxItem} controls automatically fill the entire screen
//      or a major portion of the screen when activated, and add a control to dismiss the
//      full-screen interface.  See +link{ComboBoxItem.pickListPlacement} for details
// <li> +link{Menu} components likewise fill the entire screen or a major portion, and offer
//      submenu navigation via a slide-in animation and back button instead of displaying the
//      origin menu and submenu simultaneously
// <li> +link{Calendar.minimalUI,Calendar} eliminates the tabs normally used to switch between
//      Day, Week and Month view, instead using device pivot to switch between Day and Week
//      views and offering a compact link to Month view
// <li> Windows and Dialogs fill the screen by default and remove rounded edges to save space
// <li> many controls implement an expanded hit area for clicks or drags so that finger touches
//      that are technically outside of the drawn area of the control still activate the
//      control.  This accomodates the imprecision of finger touches as compared to mouse
//      clicks, while still showing the same compact appearance as is used for desktop
//      interfaces.  This includes the +link{Slider} thumb, +link{Window.headerControls},
//      +link{canvas.resizeFrom,edge-based resizing}, and many other controls.
// <li> +link{SpinnerItem} switches to side-by-side +/- controls instead of the very small,
//      vertically stacked +/- control typical of desktop interfaces
// <li> +link{AdaptiveMenu} can either display menu items inline, or in a drop-down,
//        or mix the two modes according to available space.
// </ul>
// <p>
// In addition to automatic behavior, SmartClient offers Adaptive Layout whereby a +link{Layout}
// member may be <i>designed</i> to render itself at multiple possible sizes, in order to fit
// into the amount of space available in the Layout.  Unlike simply indicating a flexible size
// on a member, setting an adaptive width or height indicates that the member has two (or more)
// different <i>ways</i> of rendering itself with different <i>discrete</I> sizes, but does not
// have the ability to use every additional available pixel.
// <p>
// For more guidance, see the documentation under +link{canvas.canAdaptWidth} and the
// +explorerExample{inlinedMenuMobileSample, Inlined Menu Mobile} and
// +explorerExample{adaptiveMenuMobileSample, Adaptive Menu} samples.
// <p>
// <h3>Finger / touch event handling</h3>
// <P>
// Mobile and touch devices support "touch events" that correspond to finger actions on the
// screen.  By default, SmartClient simply sends touch events to UI components as normal mouse
// events.  Specifically:
// <ul>
// <li> a finger tap gesture will trigger mouseDown, mouseUp and click events
// <li> a touch-and-slide interaction will trigger drag and drop, firing the normal SmartClient
//      sequence of dragStart, dragMove, and dragStop
// <li> a touch-and-hold interaction will trigger a contextMenu event, and will trigger a hover
//      if no contextMenu is shown
// </ul>
// This means that most applications that are written with mouse interaction in mind will
// function with a touch-based UI without special efforts.  Some interfaces which rely heavily
// on mouse hovers may want to display instructions to explicitly tell the user that they have
// to touch a given element to see more information.
// <p>
// <h3>Automatic touch scrolling</h3>
// <p>
// Components that normally show scrollbars on desktop browsers will, by default, hide
// scrollbars and allow scrolling via finger dragging instead.
// <p>
// If you are using drag and drop features such as +link{listGrid.canReorderRecords}, this
// obviously conflicts with using finger drags for scrolling.  There are two options:
// <p>
// <ol>
// <li> Leave touch scrolling active for the grid, but provide additional controls, such as
//      buttons, that enable users to perform the drag operation in a different way.
//      Optionally display scrollbars <em>in addition to</em> leaving touch scrolling active
//      by setting +link{Canvas.alwaysShowScrollbars} to <code>true</code>.
// <li> Set +link{canvas.useTouchScrolling,useTouchScrolling} to <code>false</code> on the component.
//      Scrollbars will be shown, and finger drags will no longer cause scrolling, so that
//      finger drags can now be used for the drag and drop operation configured on the
//      component
// </ol>
// Option #1 above is generally preferred, since it is also considered an
// +link{group:accessibility} violation if drag and drop is the sole way to trigger an
// operation (keyboard-only users cannot use drag and drop), and also because scrollbars are
// not usually found in touch interfaces.
// <p>
// If your application is not required to be keyboard accessible, and you prefer to show
// scrollbars and use finger drags for normal drag operations, you can use
// +link{Canvas.disableTouchScrollingForDrag} to make this choice system-wide or on a
// per-component-type basis.
// <p>
// <h3>Exceptionally wide screens / forms</h3>
// <p>
// If you have designed a screen for desktop use and it is too wide to fit on a handset or
// tablet-sized screen, there are several possible strategies:
// <ul>
// <li> <b>use +link{SplitPane}</b>: any time you have two or more panes where a choice in one
//      pane decides what is displayed in the other.  See the "SplitPane" section further down
//      for details
// <li> <b>rely on horizontal scrolling</b>: if you have something like a +link{DynamicForm}
//      that has 3 columns of input fields, as long as the form itself or some parent has
//      +link{canvas.overflow,overflow:"auto"} set, horizontal touch scrolling will be
//      available to reach fields that initially render offscreen.  Most of the time, there is
//      already an <code>overflow:"auto"</code> parent component as a result of default
//      framework behaviors or application settings that also make sense for desktop mode,
//      so nothing needs to be done.
//      <p>
//      However, consider whether scrolling is already in use for other purposes: if you have a
//      grid plus an adjacent component to the right, if the adjacent component is entirely
//      offscreen, attempting touch scrollng on the grid will just scroll the grid as such and
//      won't reveal the adjacent component.  In this kind of situation, you can:
//   <ul>
//   <li> <i>use +link{SplitPane}</i> as described above, a grid with something adjacent is
//        frequently a good candidate for conversion to <code>SplitPane</code>
//   <li> <i>make the scrolling component smaller or flexible size</i>.  Whether it's a grid or
//        other scrollable component on the left, this situation usually arises because an
//        inappropriately large fixed size has been set, instead of a
//        +link{canvas.width,flexible size}.
//   <li> <i>leave some blank space</i> above or below the grid - this gives the user somewhere
//        to use touch scrolling to move both the grid and adjacent component
//   <li> <i>force scrollbars to appear</i> by setting
//        +link{canvas.useTouchScrolling,useTouchScrolling} to false.  This is another way to
//        give the user a place they can touch in order to scroll the both the grid and
//        adjacent component together
//   </ul>
// <li> <b>use +link{FlowLayout}</b>: a <code>FlowLayout</code> can automatically take two
//      side-by-side elements and switch them to vertical stacking when the screen is narrow
// </ul>
// <p>
// <h3>SplitPane</h3>
// <p>
// The +link{SplitPane} component implements the common pattern of rendering
// two or three panes simultaneously on desktop machines and on tablets in landscape
// orientation, while switching to showing a single pane for handset-sized devices or tablets
// in portrait orientation.
// <p>
// Use <code>SplitPane</code> anywhere you have two or more panes in your application where a
// choice in one pane decides what is displayed in the other pane.  For example, you may have a
// list of Records where details of a single selected Record are shown next to the list.  A
// <code>SplitPane</code> is well-suited to this interface since it provides automatic "Back"
// navigation and a place to show the title of the selected record when only the detail view is
// showing.
// <p>
// Note that you do not need to use a <code>SplitPane</code> as your top-level component
// containing the whole application, and it <i>does</i> makes sense to use multiple
// <code>SplitPane</code> components in a single application.  For example, your top-level
// container component might be a +link{TabSet}, and a +link{SplitPane} would be used to manage
// components in tabs which normally show 2 panes side-by-side on desktop browsers.
// <P>
// <h3>Device type and overriding</h3>
// <p>
// In most cases SmartClient will correctly detect the device running your application, and set
// the flags +link{Browser.isTouch}, +link{Browser.isHandset}, +link{Browser.isTablet} and
// +link{Browser.isDesktop} appropriately.
// <p>
// For any uncommon device for which these variables are not set correctly, you can use
// +link{Browser.setIsTablet()}, +link{Browser.setIsHandset()} and +link{Browser.setIsTouch()}
// to override the auto-detected settings.  If you use these APIs, call them <b>before</b>
// creating or drawing any SmartClient components or using any other SmartClient APIs.
// <p>
// Note that the various automatic behaviors triggered by flags on the +link{Browser} class can
// be overriden at a fine-grained level on individual components.  For example,
// +link{SplitPane} will use 2-pane display when a tablet is detected, however, for a
// particularly large, high-resolution tablet device, you could instead use 3-pane display by
// setting +link{SplitPane.deviceMode} to "desktop".
// <p>
// <h3>Mobile look and feel</h3>
// <P>
// We recommend using either the Enterprise, EnterpriseBlue or Graphite skins for applications
// that support mobile (or a custom skin based on one of these skins).  These skins make
// maximum use of CSS3 to minimize the number of images that need to be loaded and the number
// of DOM elements used to create components.
// <p>
// We also do <b>not</b> recommend attempting to mimic the native UI of each particular mobile
// platform, because:
// <ul>
// <li> if users access the same application via desktop and mobile browsers, consistent
// appearance and behavior between the desktop and mobile rendering of the application is more
// important for familiarity than looking similar to other applications on the mobile device
// <li> mobile platform design overhauls, such as the major changes from iOS6 to iOS7, can
// easily invalidate efforts to look like native applications on the device
// <li> there is no single consistent appearance across Android devices because different
// manufacturers customize the platform a great deal, so efforts to closely mimic any one
// device won't yield any real consistency
// </ul>
// <P>
// <h3>iOS 7, browser toolbars and "minimal-ui" setting</h3>
// <p>
// Safari in iOS 7.0 will automatically hide and show browser toolbars as the user scrolls
// around a normal web page, pivots, or touches near edges of the screen.  This creates serious
// problems for web applications, partly because notifications are not reliably fired when
// toolbars are shown and hidden, and partly because it introduces "dead zones" where an
// application cannot place interactive controls, since touching there shows browser toolbars
// instead.
// <p>
// iOS 7.1 introduces a "minimal-ui" setting on the viewport <code>meta</code> tag which
// eliminates most of these problems, by requiring that the user specifically touch the
// URL bar to reveal browser toolbars.  Even with this setting, the top 20px of space <i>in
// landscape orientation only</i> is still a "dead zone".
// <p>
// SmartClient automatically uses the minimal-ui setting whenever iOS is detected, and also
// sets +link{canvas.defaultPageSpace} to 20px in landscape orientation to avoid components
// being placed in the dead zone.  These default behaviors can be disabled by defining the
// <code>isc_useMinimalUI</code> global variable with the value <code>false</code> before the
// framework is loaded:
// <pre> &lt;script type="text/javascript"&gt;
// window.isc_useMinimalUI = false;
// &lt;/script&gt;</pre>
// <p>
// Whether minimal-ui is used or not, it is recommend to place some kind of non-interactive
// widget or content in the dead zones created by browser toolbars, for example, a +link{Label}
// showing your company name or application name.  When using +link{canvas.defaultPageSpace} to have
// all components avoid a dead zone at the top of the page, you can set
// +link{canvas.leavePageSpace,leavePageSpace:0} to allow individual components to place
// themselves in a dead zone.
// <p>
// <h3>Configuring the viewport</h3>
// <p>
// When a SmartClient application loads, by default a viewport &ltmeta&gt; tag is added to the
// page which, on touch devices, fixes the page zoom to 100% and disables the pinch-zoom gesture.
// This is usually the expected behavior of a touch-enabled web application because it makes
// the application look and feel more like a native app. This default setting can be disabled
// by defining the <code>isc_useDefaultViewport</code> global variable with the value
// <code>false</code> before the framework is loaded:
// <pre> &lt;script type="text/javascript"&gt;
// window.isc_useDefaultViewport = false;
// &lt;/script&gt;</pre>
// For more information on the mobile device viewport, see:
// <ul>
// <li>+externalLink{http://developer.apple.com/safari/library/documentation/AppleApplications/Reference/SafariWebContent/UsingtheViewport/UsingtheViewport.html,Configuring the Viewport - Safari Web Content Guide}</li>
// <li>+externalLink{https://developer.mozilla.org/en-US/docs/Mozilla/Mobile/Viewport_meta_tag,Using the viewport meta tag to control layout on mobile browsers - MDN}</li>
// </ul>
// <p>
// <h3>Orientation Change &amp; Screen Size</h3>
// <P>
// When orientation changes, this is treated identically to resizing the browser on a desktop
// machine.  If you've already created a UI that fills the browser and makes good use of
// available screen space for desktop browsers, the same behaviors will automatically apply
// when your application runs on mobile devices and the device is pivoted.
// <P>
// If you want to build specialized interfaces that respond to device orientation, the
// +link{Page.getOrientation()} API may be used to determine the current orientation of the
// application, and +link{pageEvent,the page orientationChange event} will fire whenever the
// user rotates the screen allowing applications or components to directly respond to the user
// pivoting their device.
// <p>
// <h3>Launching native helper apps (phone, facetime, maps..)</h3>
// <p>
// Generally, all that's required to launch native mobile apps is to create an ordinary HTML
// hyperlink (<code>&lt;a&gt;</code> tag) with a special prefix for the URL specified in the
// <code>href</code> attribute.  For example, the following HTML link will place a call when
// the user finger-taps it:
// <pre>
//   &lt;a href="tel:8675309"&gt;Call Jenny&lt;/a&gt;</pre>
// You can provide HTML like this as +link{HTMLFlow.contents}.  Or use a field of
// +link{type:FieldType,type:"link"} to cause various
// +link{DataBoundComponent,DataBoundComponents} to render a DataSourceField value as a
// clickable URL.
// <p>
// The URL prefixes that are valid for iOS are documented
// +externalLink{https://developer.apple.com/library/ios/featuredarticles/iPhoneURLScheme_Reference/Introduction/Introduction.html#//apple_ref/doc/uid/TP40007899-CH1-SW1,at Apple.com}.
// Typically, the same prefixes also work for Android, Windows Phone and others.
// <p>
// <h3>Configure the soft keyboard</h3>
// <p>
// +link{TextItem.browserInputType} can be set to various values such as "email" or "tel"
// (telephone number) to hint to mobile devices to use a different software keyboard with
// specialized keys appropriate for entering certain types of data values.
// <p>
// <h3>Note on mobile platform performance</h3>
// <p>
// When the first modern smartphones were released, it was necessary to use tiny,
// mobile-specific frameworks to get adequate performance for mobile web applications.
// <p>
// The situation is now completely different: through a combination of hardware improvements,
// optimizations in mobile browsers and vastly improved network speeds, typical mobile devices
// are easily able to run applications built with full-featured web platforms like SmartClient.
// For an application that supports both desktop and mobile interfaces, the worst case scenario
// for platform performance is often <b>not</b> a mobile phone, but an older desktop machine
// running Internet Explorer.
// <p>
// Unfortunately, there is a lot of out-of-date advice on the web about mobile web development
// that still advises using ultra-light, feature-poor frameworks for performance reasons.
// Carefully consider the source and recency of any such advice - the reality is that using
// such feature-poor frameworks means you will under-deliver with both your desktop <i>and</i>
// mobile interfaces.
// <p>
// For more background on choosing the right technologies for mobile and desktop web
// applications, see the
// +externalLink{http://smartclient.com/product/mobileStrategy.jsp,Mobile Strategy Page} at
// smartclient.com.
// <P>
// <h3>Offline Operation</h3>
// <P>
// SmartClient applications support "offline" operation (continuing to work without network
// access).
// <P>
// Permanent caching of resources such as .js, .css files and images are handled via the standard
// +externalLink{https://www.google.com/search?q=html5+manifest,HTML5 Manifest} - just list all
// the static files your application needs in a manifest file and mobile browsers will cache
// those resources.
// <P>
// Dynamic data is handled via the +link{Offline} APIs as well as special DataSource support
// enabled by +link{DataSource.useOfflineStorage}.
// <P>
// The end result is that you can bookmark a SmartClient application to a phone's home screen
// and use it offline with cached data, much like an installed native application.
// <P>
// <h2>Packaging as a native application</h2>
// <P>
// Via "packaging" technologies such as PhoneGap/Cordova and Titanium, a SmartClient web application
// can be packaged as an installable native application that can be delivered via the "App Store"
// for the target mobile platform.  Applications packaged in this way have access to phone-specific
// data and services such as contacts stored on the phone, or the ability to invoke the device's camera.
// <P>
// Both Titanium and PhoneGap provide access to the underlying native device APIs such as the
// accelerometer, geolocation, and UI. Both frameworks enable application development using
// only JavaScript, CSS and HTML. Additionally they provide development environments that work
// across a wide variety of devices.
// <P>
// PhoneGap has good support for native device APIs as noted +externalLink{http://www.phonegap.com/about/feature,here}.
// Titanium has similar support. There are differences between the two environments and how they
// expose their APIs, though both provide Xcode-compatible projects that can be compiled and run from the Xcode IDE.
// See +link{titaniumIntegration,Integration with Titanium} and +link{phonegapIntegration,Integration with PhoneGap}
// for more information.
//
// @title Mobile Application Development
// @treeLocation Concepts
// @visibility external
//<


//> @groupDef titaniumIntegration
// Titanium provides an extensive Javascript API to access a native device's UI, phone, camera, geolocation, etc.
// Documentation, getting started, programming guides are +externalLink{http://developer.appcelerator.com/documentation,here}.
// Titanium provides a consistent API across devices including the ability to mix webviews with native controls.
// <P>
// The Titanium sample application provides an example of accessing a device's Contacts db using SmartClient.
// The application presents 2 tabs 'Customers' and 'Contacts' and allows the user to import Customer contacts into
// his/her contacts db resident on the device. Selecting a Customer's Contact address will show a map of the contact.
// Selecting a Customer's phone number will call the customer or prompt to import the contact into the user's
// contacts. The latter option is default behavior on the iPad. Calling the customer contact is default behavior for
// devices such as the iPhone or Android.
// <P>
// The Titanium Contact object holds the following properties:
// <ul>
// <li>URL</li>
// <li>address</li>
// <li>birthday</li>
// <li>created</li>
// <li>date</li>
// <li>department</li>
// <li>email</li>
// <li>firstName</li>
// <li>firstPhonetic</li>
// <li>fullName</li>
// <li>image</li>
// <li>instantMessage</li>
// <li>jobTitle</li>
// <li>kind</li>
// <li>lastName</li>
// <li>lastPhonetic</li>
// <li>middleName</li>
// <li>middlePhonetic</li>
// <li>modified</li>
// <li>nickname</li>
// <li>note</li>
// <li>organization</li>
// <li>phone</li>
// <li>prefix</li>
// <li>relatedNames</li>
// <li>suffix</li>
// </ul>
// <P>
// The following Titanium API's are used:
// <ul>
// <li>Titanium.App.addEventListener</li>
// <li>Titanium.App.fireEvent</li>
// <li>Titanium.Contacts.getAllPeople</li>
// <li>Titanium.Geolocation.forwardGeocoder</li>
// <li>Titanium.Map.STANDARD_TYPE,</li>
// <li>Titanium.Map.createView</li>
// <li>Titanium.UI.createTab</li>
// <li>Titanium.UI.createTabGroup</li>
// <li>Titanium.UI.createWebView</li>
// <li>Titanium.UI.createWindow</li>
// <li>Titanium.UI.setBackgroundColor</li>
// </ul>
// <P>
// The following SmartClient Components are used
// <ul>
// <smartclient>
// <li>isc.DataSource</li>
// <li>isc.ListGrid</li>
// </smartclient>
// <smartgwt>
// <li>DataSource</li>
// <li>ListGrid</li>
// </smartgwt>
// </ul>
// <P>
// The following SmartClient Resources are bundled in the Titanium application
// <ul>
// <li>ISC_Containers.js</li>
// <li>ISC_Core.js</li>
// <li>ISC_DataBinding.js</li>
// <li>ISC_Foundation.js</li>
// <li>ISC_Grids.js</li>
// <li>load_skin.js</li>
// <li>skins/Mobile/images/black.gif</li>
// <li>skins/Mobile/images/blank.gif</li>
// <li>skins/Mobile/images/checked.png</li>
// <li>skins/Mobile/images/formula_menuItem.png</li>
// <li>skins/Mobile/images/grid.gif</li>
// <li>skins/Mobile/images/group_closed.gif</li>
// <li>skins/Mobile/images/group_opened.gif</li>
// <li>skins/Mobile/images/headerMenuButton_icon.gif</li>
// <li>skins/Mobile/images/loading.gif</li>
// <li>skins/Mobile/images/loadingSmall.gif</li>
// <li>skins/Mobile/images/opacity.png</li>
// <li>skins/Mobile/images/pinstripes.png</li>
// <li>skins/Mobile/images/row_collapsed.gif</li>
// <li>skins/Mobile/images/row_expanded.gif</li>
// <li>skins/Mobile/images/sort_ascending.gif</li>
// <li>skins/Mobile/images/sort_descending.gif</li>
// <li>skins/Mobile/skin_styles.css</li>
// </ul>
//
// @title Integration with Titanium
// @treeLocation Concepts/Mobile Application Development
// @visibility external
//<

//> @groupDef phonegapIntegration
// <P>
// PhoneGap documentation, quick start information, and programming guides are available at +externalLink{http://phonegap.com,http://phonegap.com}.
// <P>
// PhoneGap exposes a Contacts API which allows one to find, create and remove contacts from the device's contacts database.
// Unlike Titanium, which provides many native UI components, PhoneGap relies on 3rd party frameworks for
// UI components. Additionally, PhoneGap provides no transitions or other animation effects normally
// accessible in native applications.
// <P>
// <em>In the following guide, the name "MyMobileApp" refers to a SmartClient mobile application.
// The instructions are intended to be general, and applicable to other apps by simply substituting
// the application name and the few other app-specific details.</em>
//
// <h3>Installing PhoneGap</h3>
// Beginning with PhoneGap 2.9.0, PhoneGap is an NPM (Node.js Packager Manager) package.
// You will need to install Node.js first in order to install PhoneGap. (<b>Tip for Mac users:</b>
// +externalLink{http://brew.sh,Homebrew} is a simple and easy way
// to install the latest version of Node.js and npm: <code>brew install node</code>)
//
// <p>Once Node.js is installed, see +externalLink{http://phonegap.com/install/,http://phonegap.com/install/} for
// instructions on installing PhoneGap.
//
// <h3>Creating the PhoneGap Project</h3>
// Use the +externalLink{http://docs.phonegap.com/en/edge/guide_cli_index.md.html,<code>phonegap</code> command line utility}
// to create a new folder containing the project files:
//
// <pre style="white-space:nowrap">phonegap create --id com.mycompany.apps.MyMobileApp --name "MyMobileApp" path/to/project_folder</pre>
//
// <p>The project ID and name should be changed for your app.
//
// <h3>General Instructions</h3>
// Within the project folder, PhoneGap creates a special <code>www/</code> folder which contains
// the application JavaScript code and other assets. Within this folder, only <code>config.xml</code>
// is needed. All other files of the default "Hello PhoneGap" app can be deleted.
//
// <p>You will need to open the application's main HTML file in a text editor to make a few changes:
// <ul>
//   <li>Change the DOCTYPE to the HTML5 DOCTYPE: <code>&lt;!DOCTYPE html&gt;</code></li>
//   <li>Add a <code>&lt;script&gt;</code> tag to the <code>&lt;head&gt;</code> element to load <code>cordova.js</code>:
//       <pre>&lt;script type="text/javascript" charset="UTF-8" src="cordova.js"&gt;&lt;/script&gt;</pre>
//
//       <p><b>NOTE:</b> The <code>www/</code> folder should not contain <code>cordova.js</code>.
//       In other words, don't try to copy <code>cordova.js</code> into the <code>www/</code> folder.
//       PhoneGap automatically adds the appropriate version of this script, which is different for
//       each platform.</li>
//   <li>Ensure that the following <code>&lt;meta&gt;</code> tags are used, also in the <code>&lt;head&gt;</code> element:
//       <pre>&lt;meta http-equiv="Content-Type" content="text/html;charset=UTF-8"&gt;
//&lt;meta name="format-detection" content="telephone=no"&gt;
//&lt;meta name="viewport" content="initial-scale=1, width=device-width, user-scalable=no, minimum-scale=1, maximum-scale=1"&gt;</pre></li>
// </ul>
//
// <p>After making those changes, you will need to defer starting the application until the
//    <code>+externalLink{http://docs.phonegap.com/en/edge/cordova_events_events.md.html#deviceready,deviceready}</code> event has fired,
//    particularly if your application invokes any PhoneGap API function.
//
//        <smartclient>In SmartClient, deferring the application can be accomplished by wrapping all application code within a 'deviceready' listener:
//        <pre class="sourcefile">&lt;script type="text/javascript"&gt;
//document.addEventListener("deviceready", function onDeviceReady() {
//    // application code goes here
//}, false);
//&lt;/script&gt;</pre></smartclient>
//
//        <smartgwt>To accomplish this in Smart&nbsp;GWT, it is helpful to use a utility class together with a bit of JavaScript.
//
// <p>The following utility class can be used to defer the <code>onModuleLoad</code> code until PhoneGap is ready:
//
// <pre class="sourcefile">package com.mycompany.client;
//
//import com.google.gwt.core.client.EntryPoint;
//
//public abstract class CordovaEntryPoint implements EntryPoint {
//
//    &#x40;Override
//    public final native void onModuleLoad() &#x2F;*-{
//        var self = this;
//        if ($wnd.isDeviceReady) self.&#x40;com.mycompany.client.CordovaEntryPoint::onDeviceReady()();
//        else {
//            var listener = $entry(function () {
//                $doc.removeEventListener("deviceready", listener, false);
//                self.&#x40;com.mycompany.client.CordovaEntryPoint::onDeviceReady()();
//            });
//            $doc.addEventListener("deviceready", listener, false);
//        }
//    }-*&#x2F;;
//
//    protected abstract void onDeviceReady();
//}</pre>
//
// <p>The <code>CordovaEntryPoint</code> class is used in conjunction with the following JavaScript,
//        which should be added before the closing <code>&lt/body&gt;</code> tag:
//
//     <pre class="sourcefile">&lt;script type="text/javascript"&gt;
//document.addEventListener("deviceready", function onDeviceReady() {
//    window.isDeviceReady = true;
//    document.removeEventListener("deviceready", onDeviceReady, false);
//}, false);
//&lt;/script&gt;</pre>
//
// <p>After compiling your application with PhoneGap/Cordova support, copy the compiled Smart&nbsp;GWT
// application to the <code>www/</code> folder.
// </smartgwt>
//
// <h3>iOS Platform (iPhone &amp; iPad)</h3>
//
// <ol>
// <li>Open <b>Terminal</b>, <code>cd</code> into the project folder, and run:
// <pre>phonegap build ios</pre></li>
// <li>Within the newly-created <code>platforms/ios/</code> folder, open the Xcode project <code>MyMobileApp.xcodeproj</code>.</li>
// <li>In Xcode, set the active scheme to <b>MyMobileApp &gt; iPhone Retina (4-inch) &gt; iOS 7.0</b> or some other simulator destination.
//     Then click the <b>Run</b> button. Xcode will start the iPhone Simulator and run the app.</li>
// <li>When you are finished testing the application in the simulator, click the <b>Stop</b> button.</li>
// </ol>
//
// <p>It is helpful to pay attention to the output window when testing the app within iOS Simulator.
// The output window contains all logs to <code>+externalLink{https://developer.mozilla.org/en-US/docs/Web/API/console,window.console}</code> and messages from the Cordova
// framework itself. One common issue is <code>ERROR whitelist rejection: url='SOMEURL'</code>,
// which means that SOMEURL has not been added to <code>&lt;access origin="..."/&gt;</code> in <code>config.xml</code>.
// Refer to the +externalLink{http://docs.phonegap.com/en/edge/guide_whitelist_index.md.html#Domain%20Whitelist%20Guide,Domain Whitelist Guide}
// for more information.
//
// <p>Once you have completely tested the application within the simulator, you should test the app on
// real hardware. Refer to Apple's +externalLink{https://developer.apple.com/library/ios/documentation/IDEs/Conceptual/AppDistributionGuide/Introduction/Introduction.html,App Distribution Guide} for complete instructions on provisioning the app for testing devices, in particular, the section titled
// +externalLink{https://developer.apple.com/library/ios/documentation/IDEs/Conceptual/AppDistributionGuide/TestingYouriOSApp/TestingYouriOSApp.html#//apple_ref/doc/uid/TP40012582-CH8-SW1,Beta Testing Your iOS App}.
//
// <p>Apple has deprecated UIWebView and we recommend switching to the officially supported
// +externalLink{https://github.com/apache/cordova-plugin-wkwebview-engine,WKWebView} plugin to
// resolve momentum scrolling issues and obtain more Safari-like behavior.
//
// <h3>Android Platform</h3>
// To begin targeting Android devices, follow the instructions on the
// +externalLink{http://docs.phonegap.com/en/edge/guide_platforms_android_index.md.html,Android Platform Guide}.
//
// <p>It is helpful to monitor the LogCat view in Eclipse to verify that your application is working correctly.
// Common errors include:
// <ul>
// <li><code>Application Error The protocol is not supported. (gap://ready)</code>
//     <p>This means that the incorrect <code>cordova.js</code> script is being used. You
//     must use the <code>cordova.js</code> for Android.<!-- http://community.phonegap.com/nitobi/topics/error_starting_app_on_android -->
//     <p>Try updating the 'android' platform to fix the problem:
//     <pre>phonegap platform update android</pre>
//     </li>
// <li><code>Data exceeds UNCOMPRESS_DATA_MAX</code>
//     <p>In older versions of Android (pre-2.3.3), there is a 1 Megabyte limit on the size of individual
//        Android app assets. This error message means that one asset file exceeds this limit.
//        You should see a popup alert dialog containing the name of the problematic file, and then the app will crash.
//     <p>The "Data exceeds UNCOMPRESS_DATA_MAX" error can be seen if, for example, the SmartGWT.mobile application
//        was compiled in DETAILED or PRETTY mode.
//     </li>
// </ul>
//
// <h3>Samples</h3>
// <smartclient>
// <p>The SmartClient SDK package has a sample application called MyContacts which demonstrates how
// to work with the PhoneGap API in a SmartClient app. The main SmartClient code is located in
// <code>smartclientSDK/examples/phonegap/MyContacts</code>. An Xcode project used to package the app for iOS
// devices is located at <code>smartclientSDK/examples/phonegap/MyContacts-iOS</code>. An Eclipse project used
// to package the app for Android devices is located at <code>smartclientSDK/examples/phonegap/MyContacts-Android</code>.
// </smartclient><smartgwt>
// <p>The Smart&nbsp;GWT Google Code project has a sample application called
// +externalLink{https://github.com/isomorphic-software/smartgwt/tree/master/samples/phonegap/MyContacts,MyContacts}
// which demonstrates how to work with the PhoneGap API in a Smart&nbsp;GWT app. The main Smart&nbsp;GWT code is located at
// <code>+externalLink{https://github.com/isomorphic-software/smartgwt/tree/master/samples/phonegap/MyContacts,trunk/samples/phonegap/MyContacts}</code>.
// An Xcode project used to package the app for iOS devices is located at <code>
// +externalLink{https://github.com/isomorphic-software/smartgwt/tree/master/samples/phonegap/MyContacts-iOS,trunk/samples/phonegap/MyContacts-iOS}</code>.
// An Eclipse project used to package the app for Android devices is located at <code>
// +externalLink{https://github.com/isomorphic-software/smartgwt/tree/master/samples/phonegap/MyContacts-Android,trunk/samples/phonegap/MyContacts-Android}</code>.
//
// <p>This sample application utilizes the script changer technique to load the correct <code>cordova.js</code>.
// Additionally, GWT's +externalLink{http://www.gwtproject.org/doc/latest/DevGuideCodingBasicsOverlay.html,JavaScript overlay types}
// feature is used to easily wrap the PhoneGap Contacts API for use by the Smart&nbsp;GWT app.
// </smartgwt>
//
// @title Integration with PhoneGap
// @treeLocation Concepts/Mobile Application Development
// @visibility external
//<

isc.Browser.isAndroid = navigator.userAgent.indexOf("Android") > -1;

if (isc.Browser.isAndroid) {
    var pos = navigator.userAgent.indexOf("Android");
    if (pos >= 0) {
        isc.Browser.androidMinorVersion = parseFloat(navigator.userAgent.substring(pos + "Android".length));
        // Firefox for Android does not say which version of Android it's running on.
        // See also:
        // - https://developer.mozilla.org/en/Gecko_user_agent_string_reference#Mobile_and_Tablet_indicators
        // - Bug 625238 - Add device info to User-Agent
        //   https://bugzilla.mozilla.org/show_bug.cgi?id=625238
        if (window.isNaN(isc.Browser.androidMinorVersion)) delete isc.Browser.androidMinorVersion;
    }

    // Is the browser a WebView? This is true for the stock Android Browser and third-party apps'
    // WebViews (such as when using Cordova/PhoneGap), but should be false for other Android browsers.
    // From https://developers.google.com/chrome/mobile/docs/webview/overview#what_is_the_default_user-agent
    // "If you're attempting to differentiate between the WebView and Chrome for Android, you
    // should look for the presence of the Version/X.X string in the WebView user-agent string.
    // Don't rely on the specific Chrome version number, 30.0.0.0 as this may change with future
    // releases."
    isc.Browser.isAndroidWebView = navigator.userAgent.indexOf("Version/") >= 0;
}


isc.Browser.isRIM = isc.Browser.isBlackBerry =
    navigator.userAgent.indexOf("BlackBerry") > -1 || navigator.userAgent.indexOf("PlayBook") > -1;

isc.Browser.isMobileIE = navigator.userAgent.indexOf("IEMobile") > -1;

// Is the browser Mobile Firefox?
// https://wiki.mozilla.org/Compatibility/UADetectionLibraries
// https://developer.mozilla.org/en-US/docs/Gecko_user_agent_string_reference#Mobile_and_Tablet_indicators
isc.Browser.isMobileFirefox = isc.Browser.isFirefox && (navigator.userAgent.indexOf("Mobile") > -1 ||
                                                        navigator.userAgent.indexOf("Tablet") > -1);


isc.Browser.isMobileWebkit = (isc.Browser.isSafari &&
        (navigator.userAgent.indexOf(" Mobile/") > -1 || navigator.userAgent.indexOf("(iPad") > -1)
    || isc.Browser.isAndroid
    || isc.Browser.isBlackBerry) && !isc.Browser.isFirefox;

// intended for general mobile changes (performance, etc)
isc.Browser.isMobile = (isc.Browser.isMobileFirefox ||
                        isc.Browser.isMobileIE ||
                        isc.Browser.isMobileWebkit);

//> @classAttr browser.supportsDualInput (boolean : varies : RW)
// Does the browser support both mouse and touch input?
// @visibility external
//<

isc.Browser.supportsDualInput = window.isc_useDualInput != false &&
        isc.Browser.isWin && isc.Browser.winVersion >= 6.2 &&
        (isc.Browser.isMoz ||
         ((isc.Browser.isChrome || isc.Browser.isIE11 || isc.Browser.isEdge) &&
          navigator.maxTouchPoints > 0));

isc.Browser._useTouchMoveImageCSS = isc.Browser.supportsDualInput &&
        (isc.Browser.isIE11 || isc.Browser.isEdge);

isc.Browser._useTouchMoveCanvasCSS = isc.Browser._useTouchMoveImageCSS &&
        window.isc_useNativeTouchScrolling == false;

//> @classAttr browser.isTouch (boolean : auto-detected based on device : RW)
// Is the application running on a touch device (e.g. iPhone, iPad, Android device, etc.)?
// <p>
// SmartClient's auto-detected value for <code>isTouch</code> can be overridden via
// +link{Browser.setIsTouch()}.
//
// @visibility external
//<

isc.Browser.isTouch = isc.Browser.isMobileFirefox || isc.Browser.isMobileIE ||
                      isc.Browser.isMobileWebkit ||
                      isc.Browser.supportsDualInput && !!window.isc_useDualInput;

//> @classMethod browser.setIsTouch() (A)
// Setter for +link{Browser.isTouch} to allow this global variable to be changed at runtime.
// This advanced method is provided to override SmartClient's auto-detection logic, since the
// framework can only detect touch devices that existed at the time the platform was released.
// Any change to +link{Browser.isTouch} must be made before any component is created;
// <strong>it is an application error</strong> to attempt to change <code>isTouch</code> after
// components have been created.
// <p>
// Note that setting <code>Browser.isTouch</code> might affect the values of
// +link{Browser.isDesktop}, +link{Browser.isTablet}, and/or +link{Browser.isHandset}.
//
// @param isTouch (boolean) new setting for <code>Browser.isTablet</code>.
// @visibility external
//<
isc.Browser.setIsTouch = function (isTouch) {
    var Browser = this;

    isTouch = Browser.isTouch = !!isTouch;

    if (Browser.isDesktop) {
        Browser.isHandset = false;
        Browser.isTablet = false;
    } else {
        Browser.isHandset = isTouch && !Browser.isTablet;
        Browser.isTablet = !Browser.isHandset;
    }

    Browser.hasNativeDrag = !isTouch && "draggable" in document.documentElement &&
        !(Browser.isIE || Browser.isEdge);

    Browser.nativeMouseMoveOnCanvasScroll = !isTouch && (Browser.isSafari || Browser.isChrome);


};

//> @classAttr browser.pointerEnabled (boolean : varies : RW)
// Does the browser support pointer events as a means of capturing both touch and mouse
// interactions?  This simplifies event handling for capable browsers.
//<

isc.Browser.pointerEnabled = window.PointerEvent != null &&
        navigator.pointerEnabled != false && navigator.msPointerEnabled != false &&
        (isc.Browser.isIE || isc.Browser.isEdge) && !isc.Browser.isMobileIE;

//> @classAttr browser.hasDualInput (boolean : false : RW)
// is the browser currently sending both mouse and touch input?  For example, Microsoft Surface
// devices are touch devices that run Windows 10 but also allow the connection of USB mice.  In
// such an environment, we may not be able to auto-detect that touch input is present, so the
// switch to hasDualINput: true will only happen at the moment a touch event actually arrives.
//<
isc.Browser.hasDualInput = !!window.isc_useDualInput;

// helper called by EventHandler to switch to dual input mode if a touch event arrives
isc.Browser.setHasDualInput = function () {

    if (this.hasDualInput == true) return;



    if (isc.logInfo) {
        isc.logInfo("Switching to dual input mode to handle touch events");
    }

    this.setIsTouch(true);
    this.hasDualInput = true;


    if (isc.Canvas) {
        isc.Canvas.addProperties({
            overflowStyle: "none",
            _browserSupportsNativeTouchScrolling: isc.Browser._getSupportsNativeTouchScrolling()
        });
    }
    if (isc.ListGrid) {
        isc.ListGrid.addProperties({
            showRollOver: false,
            // if a mouse event is received, switch rollover back on
            handleMouseMove : function (event, eventInfo) {
                return this._handleDualInputMouseMove(event, eventInfo);
            }
        });
    }
}

// iPhone OS including iPad.  Search for iPad or iPhone.

isc.Browser.isIPhone = (isc.Browser.isMobileWebkit &&
                        (navigator.userAgent.indexOf("iPhone") > -1 ||
                         navigator.userAgent.indexOf("iPad") > -1));

if (isc.Browser.isIPhone) {
    // adapted from SmartGWT.mobile
    var match = navigator.userAgent.match(/CPU\s+(?:iPhone\s+)?OS\s*([0-9_]+)/i);
    if (match != null) {
        isc.Browser.iOSMinorVersion = window.parseFloat(match[1].replace('_', '.'));
        isc.Browser.iOSVersion = isc.Browser.iOSMinorVersion << 0;
    }

    // The UIWebView user agent is different from the Mobile Safari user agent in that it does
    // not contain the word "Safari".
    isc.Browser.isUIWebView = navigator.userAgent.indexOf("Safari") < 0;

    isc.Browser.isMobileSafari = !isc.Browser.isUIWebView &&
                                 // Exclude Chrome for iOS
                                 // https://developers.google.com/chrome/mobile/docs/user-agent#chrome_for_ios_user-agent
                                 navigator.userAgent.indexOf("CriOS/") < 0;
}

// iPad.  Checks for "iPhone" OS + "iPad" in UA String.
isc.Browser.isIPad = (isc.Browser.isIPhone &&
                        navigator.userAgent.indexOf("iPad") > -1);

isc.Browser.isWindowsPhone = navigator.userAgent.indexOf("Windows Phone") > -1;


if (isc.Browser.isIPad && isc.Browser.isMobileSafari && isc.Browser.iOSVersion == 7) {

    var iOS7IPadStyleSheetID = "isc_iOS7IPadStyleSheet";
    if (document.getElementById(iOS7IPadStyleSheetID) == null) {
        var styleElement = document.createElement("style");
        styleElement.id = iOS7IPadStyleSheetID;
        document.head.appendChild(styleElement);
        var s = styleElement.sheet;
        s.insertRule("\n@media (orientation:landscape) {\n" +
                         "html {" +
                             "position: fixed;" +
                             "bottom: 0px;" +
                             "width: 100%;" +
                             "height: 672px;" +
                         "}" +
                         "body {" +
                             "position: fixed;" +
                             "top: 0px;" +
                             "margin: 0px;" +
                             "padding: 0px;" +
                             "width: 100%;" +
                             "height: 672px;" +
                         "}\n" +
                     "}\n", 0);
    }


    (function () {
        var isFormItemElement = function (element) {
            if (element == null) return false;
            var tagName = element.tagName;
            return (tagName === "INPUT" ||
                    tagName === "SELECT" ||
                    tagName === "TEXTAREA");
        };

        var scrollToTopTimerID = null;
        window.addEventListener("scroll", function () {
            if (document.body == null) return;
            var scrollTop = document.body.scrollTop;
            if (scrollTop == 0) return;

            var activeElement = document.activeElement;
            if (isFormItemElement(activeElement)) {
                var onBlur = function onBlur(blurEvent) {
                    activeElement.removeEventListener("blur", onBlur, true);

                    if (scrollToTopTimerID != null) clearTimeout(scrollToTopTimerID);
                    scrollToTopTimerID = setTimeout(function () {
                        scrollToTopTimerID = null;

                        activeElement = document.activeElement;

                        // If another form item element is active, then wait for that element to lose focus.
                        if (activeElement !== blurEvent.target && isFormItemElement(activeElement)) {
                            activeElement.addEventListener("blur", onBlur, true);

                        } else {
                            document.body.scrollTop = 0;
                        }
                    }, 1);
                };
                activeElement.addEventListener("blur", onBlur, true);
            } else {
                document.body.scrollTop = 0;
            }
        }, false);
    })();
}


//> @type DeviceMode
// Possible layout modes for UI components that are sensitive to the device type being used
// (a.k.a. "responsive design").  See for example +link{SplitPane.deviceMode}.
// @value "handset" mode intended for handset-size devices (phones).  Generally only one UI
//                  panel will be shown at a time.
// @value "tablet" mode intended for tablet-size devices.  Generally, up to two panels are
//                 shown side by side in "landscape" +link{type:PageOrientation}, and only one
//                 panel is shown in "portrait" orientation.
// @value "desktop" mode intended for desktop browsers.  Three or more panels may be shown
//                  simultaneously.
// @visibility external
//<

//> @classAttr browser.isTablet (boolean : auto-detected based on device : RW)
// Is the application running on a tablet device (e.g. iPad, Nexus 7)?
// <p>
// SmartClient can correctly determine whether the device is a tablet in most cases. On any
// uncommon device for which this variable is incorrect, you can define the <code>isc_isTablet</code>
// global with the correct value, and SmartClient will use <code>isc_isTablet</code> for
// <code>Browser.isTablet</code> instead of its own detection logic. Alternatively, you can use
// +link{Browser.setIsTablet()} to change this global variable before any components are
// created.
// <p>
// The value of this variable is only meaningful on touch devices.
//
// @setter setIsTablet()
// @visibility external
//<

if (window.isc_isTablet != null) {
    isc.Browser.isTablet = !!window.isc_isTablet;
} else {
    isc.Browser.isTablet = isc.Browser.isIPad ||
                           (isc.Browser.isRIM && navigator.userAgent.indexOf("Tablet") > -1) ||
                           (isc.Browser.isAndroid && navigator.userAgent.indexOf("Mobile") == -1);
}
isc.Browser._origIsTablet = isc.Browser.isTablet;

//> @classMethod browser.setIsTablet() (A)
// Setter for +link{Browser.isTablet} to allow this global variable to be changed at runtime.
// This advanced method is provided to override SmartClient's detection of devices, since the
// framework can only detect devices that existed at the time the platform was released. Any
// changes to +link{Browser.isDesktop}, +link{Browser.isHandset}, or +link{Browser.isTablet}
// must be made before any component is created;
// <strong>it is an application error</strong> to attempt to change <code>isDesktop</code>,
// <code>isHandset</code>, or <code>isTablet</code> after components have been created.
// <p>
// Note that setting <code>Browser.isTablet</code> might affect the values of
// +link{Browser.isDesktop} and +link{Browser.isHandset}.
//
// @param isTablet (boolean) new setting for <code>Browser.isTablet</code>.
// @visibility external
//<
isc.Browser.setIsTablet = function (isTablet) {
    isTablet = isc.Browser.isTablet = !!isTablet;
    isc.Browser.isHandset = (isc.Browser.isTouch && !isc.Browser.isTablet);
    isc.Browser.isDesktop = (!isc.Browser.isTablet && !isc.Browser.isHandset);


};

//> @classAttr browser.isHandset (boolean : auto-detected based on device: RW)
// Is the application running on a handset-sized device, with a typical screen width of around
// 3-4 inches?
// <p>
// This typically implies that the application will be working with only 300-400 pixels.
//
// @setter setIsHandset()
// @visibility external
//<

isc.Browser.isHandset = (isc.Browser.isTouch && !isc.Browser.isTablet);

//> @classMethod browser.setIsHandset() (A)
// Setter for +link{Browser.isHandset} to allow this global variable to be changed at runtime.
// This advanced method is provided to override SmartClient's detection of devices, since the
// framework can only detect devices that existed at the time the platform was released. Any
// changes to +link{Browser.isDesktop}, +link{Browser.isHandset}, or +link{Browser.isTablet}
// must be made before any component is created;
// <strong>it is an application error</strong> to attempt to change <code>isDesktop</code>,
// <code>isHandset</code>, or <code>isTablet</code> after components have been created.
// <p>
// Note that setting <code>Browser.isHandset</code> might affect the values of
// +link{Browser.isDesktop} and +link{Browser.isTablet}.
//
// @param isHandset (boolean) new setting for <code>Browser.isHandset</code>.
// @visibility external
//<
isc.Browser.setIsHandset = function (isHandset) {
    isHandset = isc.Browser.isHandset = !!isHandset;
    isc.Browser.isTablet = (isc.Browser.isTouch && !isc.Browser.isHandset);
    isc.Browser.isDesktop = (!isc.Browser.isTablet && !isc.Browser.isHandset);


};

//> @classAttr browser.isDesktop (boolean : auto-detected based on device : RW)
// Is the application running in a desktop browser? This is true if +link{Browser.isTablet}
// and +link{Browser.isHandset} are both <code>false</code>.
//
// @setter setIsDesktop()
// @visibility external
//<

isc.Browser.isDesktop = (!isc.Browser.isTablet && !isc.Browser.isHandset);

//> @classMethod browser.setIsDesktop() (A)
// Setter for +link{Browser.isDesktop} to allow this global variable to be changed at runtime.
// This advanced method is provided to override SmartClient's detection of devices, since the
// framework can only detect devices that existed at the time the platform was released. Any
// changes to +link{Browser.isDesktop}, +link{Browser.isHandset}, or +link{Browser.isTablet}
// must be made before any component is created;
// <strong>it is an application error</strong> to attempt to change <code>isDesktop</code>,
// <code>isHandset</code>, or <code>isTablet</code> after components have been created.
// <p>
// Note that setting <code>Browser.isDesktop</code> might affect the values of
// +link{Browser.isHandset} and +link{Browser.isTablet}.
//
// @param isDesktop (boolean) new setting for <code>Browser.isDesktop</code>.
// @visibility external
//<
isc.Browser.setIsDesktop = function (isDesktop) {
    isDesktop = isc.Browser.isDesktop = !!isDesktop;
    if (isDesktop) {
        isc.Browser.isHandset = false;
        isc.Browser.isTablet = false;
    } else {
        isc.Browser.isTablet = isc.Browser._origIsTablet;
        isc.Browser.isHandset = !isc.Browser.isTablet;
    }


};

//> @classAttr  Browser.isBorderBox    (boolean : ? : R)
// Do divs render out with "border-box" sizing by default.
//<
// See comments in Canvas.adjustHandleSize() for a discussion of border-box vs content-box sizing

isc.Browser.isBorderBox = (isc.Browser.isIE && !isc.Browser.isStrict);

//>    @classAttr    Browser.lineFeed    (String : ? : RA)
//        Linefeed for this platform
//<

isc.Browser.lineFeed = (isc.Browser.isWin ? "\r\n" : "\n");

//>    @classAttr    Browser._supportsMethodTimeout    (String : ? : RA)
//        setTimeout() requires text string parameter in MacIE or IE 4
//<
isc.Browser._supportsMethodTimeout = false;//!(isc.Browser.isIE && (isc.Browser.isMac || isc.Browser.version == 4));

//>    @classAttr    Browser.isDOM (String : ? : RA)
//        Whether this is a DOM-compliant browser.  Indicates general compliance with DOM standards,
//      not perfect compliance.
//<
isc.Browser.isDOM = (isc.Browser.isMoz || isc.Browser.isOpera ||
                     isc.Browser.isSafari || (isc.Browser.isIE && isc.Browser.version >= 5));

//> @classAttr browser.isSupported (boolean : auto-detected based on browser : R)
// Whether SmartClient supports the current browser.
// <P>
// Note that this flag will only be available on browsers that at least support basic
// JavaScript.
//
// @visibility external
//<
isc.Browser.isSupported = (
    // we support all versions of IE 5.5 and greater on Windows only
    (isc.Browser.isIE && isc.Browser.minorVersion >= 5.5 && isc.Browser.isWin) ||
    // Mozilla and Netscape 6, all platforms
    isc.Browser.isMoz ||
    isc.Browser.isOpera ||
    isc.Browser.isSafari || // NB: really means "is Webkit", so includes Chrome, mobile Safari
    isc.Browser.isAIR
);


isc.Browser.nativeMouseMoveOnCanvasScroll =
    !isc.Browser.isTouch && (isc.Browser.isSafari || isc.Browser.isChrome);

//> @classAttr Browser.seleniumPresent (boolean : varies : R)
// Whether current page has been loaded by Selenium WebDriver.
//<
isc.Browser.seleniumPresent = (function () {
    var match = location.href.match(/[?&](?:sc_selenium)=([^&#]*)/);
    return match && match.length > 1 && "true" == match[1];
})();


if (isc.Browser.isSafariStrict) {
    isc.Browser._writingModeCSS = {
        vertical_ltr: "vertical-rl;",
        vertical_rtl: "vertical-lr;",
        rotate_ltr: true,
        rotate_rtl: true,
        horizontal: "horizontal-tb;"
    };
} else if (isc.Browser.isEdge) {
    isc.Browser._writingModeCSS = {
        vertical_ltr: "tb-rl;",
        vertical_rtl: "tb;",
        rotate_ltr: true,
        rotate_rtl: true,
        horizontal: "unset;"
    };
} else {
    isc.Browser._writingModeCSS = {
        vertical_ltr: "tb-rl;",
        vertical_rtl: "tb;",
        rotate_ltr: true,
        rotate_rtl: true,
        horizontal: "lr;"
    };
}

//> @type Autotest
// @value isc.Browser.SHOWCASE autotest is targeting SmartClient or SGWT showcases
// @value isc.Browser.SELENESE autotest is targeting a single sample with Selenese
// @value isc.Browser.RUNNER autotest is targeting TestRunner-based JS tests
//<

//> @classAttr Browser.SHOWCASE (Constant : "showcase" : [R])
// A declared value of the enum type
// +link{type:Autotest,Autotest}.
// @constant
//<
isc.Browser.SHOWCASE = "showcase";

//> @classAttr Browser.SELENESE (Constant : "selenese" : [R])
// A declared value of the enum type
// +link{type:Autotest,Autotest}.
// @constant
//<
isc.Browser.SELENESE = "selenese";

//> @classAttr Browser.RUNNER (Constant : "runner" : [R])
// A declared value of the enum type
// +link{type:Autotest,Autotest}.
// @constant
//<
isc.Browser.RUNNER = "runner";

//> @classAttr Browser.autotest (Autotest : varies : R)
// The current mode of the autotest system (null if not in autotest mode)
//<
isc.Browser.autotest = (function () {
    var match = location.href.match(/[?&](?:autotest)=([^&#]*)/);
    return match && match.length > 1 ? match[1] : null;
})();

//>    @classAttr    Browser.allowsXSXHR    (boolean : ? : RA)
//    Traditionally, web browsers reject attempts to make an XmlHttpRequest of a server other than the origin
//  server. However, some more recent browsers allow cross-site XmlHttpRequests to be made, relying on the
//  server to accept or reject them depending on what the origin server is.
//<
isc.Browser.allowsXSXHR = (
    (isc.Browser.isFirefox && isc.Browser.firefoxMajorMinorNumber >= 3.5) ||
    // Chrome auto-updates to latest stable version every time you start it, and there is no option to prevent
    // this from happening, so there's no point in querying version
    (isc.Browser.isChrome) ||
    (isc.Browser.isSafari && isc.Browser.safariVersion >= 531)
);

//> @classAttr Browser.useCSSFilters (boolean : ? : R)
// Whether the current browser supports gradients and whether SmartClient is
// configured to use gradients (via the setting of window.isc_useGradientsPreIE9).
//<


var isc_useGradientsPreIE9 = window.isc_useGradientsPreIE9;
isc.Browser.useCSSFilters =
    !isc.Browser.isIE || isc.Browser.isIE9 || isc_useGradientsPreIE9 != false;

//> @classAttr Browser.isSGWT (boolean : ? : RA)
// Are we running in SGWT.
// This is set up by SmartGWT wrapper code in JsObject.init().
// Obviously only applies to internal SmartClient code since developer code for an SGWT app
// would be written in Java and there'd be no need to check this var!
// @visibility internal
//<

//> @classAttr browser.useCSS3 (boolean : see below : R)
// Whether the current browser supports CSS3 and whether SmartClient is configured to use
// CSS3 features (via the setting of window.isc_css3Mode).
// <P>
// If isc_css3Mode is "on" then useCSS3 is set to true.  If isc_css3Mode is set to
// "supported", "partialSupport", or is unset, then useCSS3 is set to true only if the browser
// is a WebKit-based browser, Firefox, IE 9 in standards mode, or IE 10+.  If isc_css3Mode is set
// to "off" then useCSS3 is set to false.
// @visibility external
//<
var isc_css3Mode = window.isc_css3Mode;
if (isc_css3Mode == "on") {
    isc.Browser.useCSS3 = true;
} else if (isc_css3Mode == "off") {
    isc.Browser.useCSS3 = false;
} else if (isc_css3Mode == "supported" ||
           isc_css3Mode == "partialSupport" ||
           (typeof isc_css3Mode) === "undefined")
{
    isc.Browser.useCSS3 = isc.Browser.isWebKit ||
                          isc.Browser.isFirefox ||
                          (isc.Browser.isIE && (isc.Browser.isIE9 || isc.Browser.version >= 10));
} else {
    isc.Browser.useCSS3 = false;
}

var isc_spriting = window.isc_spriting;
if (isc_spriting == "off") {
    isc.Browser.useSpriting = false;
} else {
    isc.Browser.useSpriting = (!isc.Browser.isIE || isc.Browser.version >= 7);
}

isc.Browser.useInsertAdjacentHTML = !!document.documentElement.insertAdjacentHTML;

//> @classAttr Browser.supportsFlatSkins (boolean : varies : IR)
// Whether browser is capable of rendering flat skins (e.g. Tahoe).
// @visibility sgwt
//<
isc.Browser.supportsFlatSkins = isc.Browser.useCSS3 && isc.Browser.useSpriting;

//> @classAttr Browser.defaultSkin (String : varies : IR)
// Preferred default skin if none is specified.
// @visibility sgwt
//<
isc.Browser.defaultSkin = isc.Browser.supportsFlatSkins ? "Tahoe" : "Enterprise";

//> @classAttr Browser.defaultFontIncrease (int : varies : IR)
// Preferred font size increase if none is specified.
// @visibility sgwt
//<
isc.Browser.defaultFontIncrease = isc.Browser.seleniumPresent ? 0 :
        (isc.Browser.supportsFlatSkins ? 3 : 1);

//> @classAttr Browser.defaultSizeIncrease (int : varies : IR)
// Preferred control size increase if none is specified.
// @visibility sgwt
//<
isc.Browser.defaultSizeIncrease = isc.Browser.seleniumPresent ? 0 :
        (isc.Browser.supportsFlatSkins ? 10 : 2);


isc.Browser.useInsertAdjacentHTMLForSVG = (function () {
    if (!!document.createElementNS) {
        var svgGElem = document.createElementNS("http://www.w3.org/2000/svg", "g");
        if ((typeof svgGElem.insertAdjacentHTML) === "function") {
            try {
                svgGElem.insertAdjacentHTML("beforeend", "<rect/><ellipse/>");
                return (svgGElem.childNodes.length == 2 &&
                        svgGElem.childNodes[1].namespaceURI === "http://www.w3.org/2000/svg");
            } catch (e) {
                // ignored
            }
        }
    }
    return false;
})();

// Test for availability of the Range.getBoundingClientRect() method which was added to
// CSSOM View as of the 04 August 2009 Working Draft.
// http://www.w3.org/TR/2009/WD-cssom-view-20090804/

isc.Browser.hasNativeGetRect = (!isc.Browser.isIE &&
                                (!isc.Browser.isSafari || !isc.Browser.isMac || isc.Browser.version >= 6) &&
                                !!document.createRange &&
                                !!(document.createRange().getBoundingClientRect));


// isc.Browser.useClipDiv - if true we write out 2 handles for each widget
// the content div and the clip div. This is required to allow reliable measuring of content,
// sizing, etc. in some browsers


isc.Browser.useClipDiv = (isc.Browser.isMoz || isc.Browser.isSafari || isc.Browser.isOpera);

// _useNewSingleDivSizing: Use a single div rather than double-div structure for
// widgets with overflow settings where this is supportable.
// Only has an impact if useClipDiv is true.

isc.Browser._useNewSingleDivSizing = !((isc.Browser.isIE && isc.Browser.version < 10 && !isc.Browser.isIE9) ||
                                       (isc.Browser.isWebKit && !(parseFloat(isc.Browser.rawSafariVersion) >= 532.3)));




isc.Browser.hasTextOverflowEllipsis = (!isc.Browser.isMoz || isc.Browser.version >= 7) &&
                                      (!isc.Browser.isOpera || isc.Browser.version >= 9);

// https://developer.mozilla.org/en-US/docs/CSS/text-overflow
isc.Browser._textOverflowPropertyName = (!isc.Browser.isOpera || isc.Browser.version >= 11 ? "text-overflow" : "-o-text-overflow");


isc.Browser._hasGetBCR = !isc.Browser.isSafari || isc.Browser.version >= 4;


isc.Browser._hasElementPointerEvents = ("pointerEvents" in document.documentElement.style &&
                                        !isc.Browser.isOpera &&
                                        (!isc.Browser.isIE || isc.Browser.version >= 11));

// Does the browser support HTML5 drag and drop?
// http://caniuse.com/#feat=dragndrop
// http://www.whatwg.org/specs/web-apps/current-work/multipage/dnd.html#dnd
//
// This is set to false in IE and Edge because cross-window drags are not possible.

isc.Browser.hasNativeDrag = !isc.Browser.isTouch && "draggable" in document.documentElement &&
        !(isc.Browser.isIE || isc.Browser.isEdge);

// http://dom.spec.whatwg.org/#ranges
isc.Browser._hasDOMRanges = !!(window.getSelection && document.createRange && window.Range);

// Whether the browser supports Range.createContextualFragment() generally.

isc.Browser._supportsCreateContextualFragment = isc.Browser._hasDOMRanges && !!document.createRange().createContextualFragment;

// Whether the browser supports Range.createContextualFragment() in SVG contexts.

isc.Browser._supportsSVGCreateContextualFragment = ((isc.Browser.isMoz && isc.Browser.version >= 36) ||
                                                    (isc.Browser.isChrome && isc.Browser.version >= 42));

// Whether the browser supports the CSS `background-size' property.
// https://developer.mozilla.org/en-US/docs/Web/CSS/background-size
isc.Browser._supportsBackgroundSize = "backgroundSize" in document.documentElement.style;

// Does the browser support CSS3 transitions?
// http://caniuse.com/#feat=css-transitions
// Note: No need to check for "msTransition" because IE10 was the first version of IE to have
// CSS3 transitions support and this is unprefixed.

isc.Browser._supportsCSSTransitions = (("transition" in document.documentElement.style ||
                                        "WebkitTransition" in document.documentElement.style ||
                                        "OTransition" in document.documentElement.style) &&
                                       (!isc.Browser.isMoz ||
                                        (!isc.Browser.isTouch && isc.Browser.version >= 34)));


isc.Browser._transitionEndEventType = ("WebkitTransition" in document.documentElement.style
                                       ? "webkitTransitionEnd"
                                       : ("OTransition" in document.documentElement.style
                                          ? (isc.Browser.isOpera && isc.Browser.version >= 12 ? "otransitionend" : "oTransitionEnd")
                                          : "transitionend"));

// Whether the browser supports native touch scrolling.
// This is a classMethod rather than a classAttr because it depends on isTouch, which is settable
// by the application any time up to creation of the first widget. See setIsTouch().
isc.Browser._getSupportsNativeTouchScrolling = function () {
    if (window.isc_useNativeTouchScrolling == false) return false;
    return this.isTouch && (!this.isMoz || !this.isWin) &&
        (!(this.isIPhone || this.isIPad) || this.iOSVersion >= 6);
};

isc.Browser._supportsWebkitOverflowScrolling = isc.Browser.iOSVersion >= 6 &&
                   ("webkitOverflowScrolling" in document.documentElement.style)
;

// Does the browser support CanvasRenderingContext2D.isPointInStroke()?
isc.Browser._supportsCanvasIsPointInStroke = (function () {
    var canvas = document.createElement("canvas");
    if (canvas.getContext != null) {
        var context = canvas.getContext("2d");
        return !!context.isPointInStroke;
    }
    return false;
})();


isc.Browser._supportsNativeNodeContains = ("contains" in document.documentElement);
// Node.contains() was introduced in Gecko 9.
if (!isc.Browser._supportsNativeNodeContains && window.Node != null) {
    Node.prototype.contains = function (otherNode) {
        for (; otherNode != null; otherNode = otherNode.parentNode) {
            if (this === otherNode) return true;
        }
        return false;
    };
}


isc.Browser._supportsMinimalUI = (isc.Browser.isIPhone && !isc.Browser.isIPad &&
                                  isc.Browser.isMobileSafari &&
                                  7.1 == isc.Browser.iOSMinorVersion);


isc.Browser._svgElementsHaveParentElement = (!!document.createElementNS && "parentElement" in document.createElementNS("http://www.w3.org/2000/svg", "svg"));
if (!isc.Browser._svgElementsHaveParentElement && window.SVGElement != null && Object.defineProperty) {
    Object.defineProperty(SVGElement.prototype, "parentElement", {
        enumerable: true,
        "get" : function () {
            var parentElement = this.parentNode;
            while (parentElement != null && parentElement.nodeType != 1) {
                parentElement = parentElement.parentNode;
            }
            return parentElement;
        }
    });
}

isc.Browser._svgElementsHaveContains = (!!document.createElementNS && "contains" in document.createElementNS("http://www.w3.org/2000/svg", "svg"));
if (!isc.Browser._svgElementsHaveContains && window.SVGElement != null) {
    SVGElement.prototype.contains = function (otherNode) {
        for (; otherNode != null; otherNode = otherNode.parentNode) {
            if (this === otherNode) return true;
        }
        return false;
    };
}

// Does the browser support the HTML5 'placeholder' attribute?

isc.Browser._supportsPlaceholderAttribute = ("placeholder" in document.createElement("input") &&
                                             "placeholder" in document.createElement("textarea"));

isc.Browser._supportsIOSTabs = isc.Browser.isMobileWebkit && "webkitMaskBoxImage" in document.documentElement.style;

// Does the browser support the Screen Orientation API?
// https://w3c.github.io/screen-orientation/
// http://caniuse.com/#feat=screen-orientation

isc.Browser._supportsScreenOrientationAPI = (window.screen != null && "orientation" in screen && "type" in screen.orientation);

// Does the browser support the SVGSVGElement.getIntersectionList() SVG 1.1 DOM method?

isc.Browser._supportsSVGGetIntersectionList = (!isc.Browser.isSafari &&
                                               !isc.Browser.isChrome &&
                                               !!document.createElementNS &&
                                               "getIntersectionList" in document.createElementNS("http://www.w3.org/2000/svg", "svg") &&
                                               "createSVGRect" in document.createElementNS("http://www.w3.org/2000/svg", "svg"));

isc.Browser._supportsJSONObject = (window.JSON != null &&
                                   typeof window.JSON.parse === "function" &&
                                   typeof window.JSON.stringify === "function" &&
                                   window.JSON.stringify("\u0013") === "\"\\u0013\"");




//> @classAttr Browser.useHighPerformanceGridTimings (boolean : see below : I)
// Controls how agressive components based on the +link{class:GridRenderer} are with respect to
// redraws and data fetches. Modern browsers can generally handle much more frequent redraws
// and most server configurations can handle fetching more data more frequently in order to
// reduce the lag the end user perceives when scrolling through databound grids.  Starting with
// SmartClient 11.0/SmartGWT 6.0, this more aggressive redraw and fetch behavior us the
// default, but can be reverted to the old behavior if desired - see below.
// <P>
// This flag controls the defaults for several other properties (value on left is default for
// high performance mode, value on right is default when this mode is disabled.
// <ul>
// <li> +link{attr:ListGrid.dataFetchDelay} 1 -> 300
// <li> +link{attr:ListGrid.drawAheadRatio} 2.0 -> 1.3
// <li> +link{attr:ListGrid.quickDrawAheadRatio} 2.0 -> 1.3
// <li> +link{attr:ListGrid.scrollRedrawDelay} 0 -> 75
// <li> +link{attr:ListGrid.scrollWheelRedrawDelay} 0 -> 250
// <li> +link{attr:ListGrid.touchScrollRedrawDelay} 0 -> 300
// </ul>
// Note: since +link{class:TreeGrid} is a subclass of +link{class:ListGrid}, the above settings
// also apply to +link{class:TreeGrid}s.
// <ul>
// <li> +link{attr:GridRenderer.drawAheadRatio} 2.0 -> 1.3
// <li> +link{attr:GridRenderer.quickDrawAheadRatio} 2.0 -> 1.3
// <li> +link{attr:GridRenderer.scrollRedrawDelay} 0 -> 75
// <li> +link{attr:GridRenderer.touchScrollRedrawDelay} 0 -> 300
// </ul>
// <P>
// By default, for all browsers except Android-based Chrome, this flag is set to true, but can
// be explicitly disabled by setting <code>isc_useHighPerformanceGridTimings=false</code> in a
// script block before loading SmartClient modules.  Turning off high performance timings
// effectively enables the original SmartClient/SmartGWT behavior prior to the SmartClient
// 11.0/SmartGWT 6.0 release.
//
// @visibility external
//<
isc.Browser.canUseAggressiveGridTimings = !isc.Browser.isAndroid;
isc.Browser.useHighPerformanceGridTimings = window.isc_useHighPerformanceGridTimings == null ?
    isc.Browser.canUseAggressiveGridTimings : window.isc_useHighPerformanceGridTimings && isc.Browser.canUseAggressiveGridTimings;


isc.Browser._usePointerCursorForHand =
        isc.Browser.isMoz || (isc.Browser.isSafari && isc.Browser.isStrict) ||
        (isc.Browser.isIE && isc.Browser.version >= 9 && isc.Browser.isStrict);

}


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
            // upgrade: when ISC_Core is available, defer to that code
            if (isc.isA) return isc.isA.String(object);
            return typeof object == "string";
        },

        isAnArray : function (object) {
            // upgrade: when ISC_Core is available, defer to that code
            if (isc.isA) return isc.isAn.Array(object);
            return typeof object == "array";
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


isc.defineStandaloneClass("SA_Page", {

_isLoaded: (isc.Page && isc.Page.isLoaded()) || false,
_pageLoadCallbacks: [],

isLoaded : function () {
    return this._isLoaded;
},

onLoad : function (callback, target, args) {
    this._pageLoadCallbacks.push({
        method: callback,
        target: target,
        args: args
    });

    if (!this._registeredOnload) {
        this._registeredOnload = true;
        // HACK: Opera: addEventListener("load") fires seemingly on every externally loaded
        // file in Opera.  But Opera emulates IE's attachEvent(), and fires load normally.
        if ((isc.Browser.isIE && isc.Browser.version < 11) || isc.Browser.isOpera) {
            window.attachEvent("onload", function () { isc.SA_Page._firePageLoadCallbacks(); });
        } else {
            window.addEventListener("load", function () { isc.SA_Page._firePageLoadCallbacks(); }, true);
        }
    }
},

_firePageLoadCallbacks : function () {
    // Moz/FF has a bug: if you register a page onload event, but navigate away from the page
    // before the page finishes loading, the onload event may fire on the page that you
    // navigated away to - even if it's a completely different site.  This typically results in
    // a JS error.
    //
    // Also - this can be fored from EventHandler.handeLoad(), so trap double call.
    if (!window.isc || this._isLoaded) return;

    // flag page as loaded
    this._isLoaded = true;

    // process all callbacks
    for (var i = 0; i < this._pageLoadCallbacks.length; i++) {
        var callback = this._pageLoadCallbacks[i];
        this.fireSimpleCallback(callback);
    }
    delete this._pageLoadCallbacks;
}

});

if (!isc.SA_Page.isLoaded()) {
    isc.SA_Page.onLoad(function () { this._isLoaded = true; }, isc.SA_Page);
}


// Synthetic History Support
// --------------------------------------------------------------------------------------------
//



//> @object History
//
// This class provides synthetic history support.  Using this class, you can create history
// entries at any point and be called back when the user next navigates to any of these history
// entries via any of the browser mechanisms that enable navigation: back/forward buttons,
// history dropdown and bookmarks.
// <p>
// The history entries created using this mechanism work just like history entries created
// natively by the browser, except you get a callback whenever a transition occurs.  This
// implementation correctly handles "deep" history - i.e. it correctly maintains forward and
// back history when the user navigates forward or back away from the page that uses this
// module.
// <p>
// This module is usable independent of the rest of SmartClient - you can use it on pages that
// don't load any other modules.
// <p>
// <b>Platform Notes:</b><br>
// In Safari (4.0 and above), this module has the limitation that the arbitrary data parameter
// in addHistoryEntry() is not reliable.<br>
// Internet Explorer: If you set document.domain on the top-level page, the History
// mechanism will behave sub-optimally in IE - three clicks one the forward/back buttons will
// be required to transition to the next history entry.
// <p>
// <b>Usage overview</b><br>
// Synthetic history entries are added to the browser history via +link{History.addHistoryEntry()}.
// When this method is called, the page's URL will be modified and the native browser back button
// will become active.<br>
// The +link{History.registerCallback()} allows the developer to register a callback method to
// fire when the user navigates to these generated history entries. This method will be fired
// with an appropriate history ID when the user hits the back-button or explicitly navigates to
// the URL generated for some synthetic history entry.
//
// @treeLocation Client Reference/System
// @visibility external
//<
//--------------------------------------------------------------------------------------------------
isc.defineStandaloneClass("History", {

//> @staticMethod History.registerCallback()
// Registers a callback to be called when the user navigates to a synthetic history entry.
// <p>
// <b>NOTE:</b> Only one primary callback can be registered at a time. Unless <code>isAdditional</code>
// is true, then <code>registerCallback()</code> registers the primary callback. To register
// a callback that is called in addition to the primary callback, if set, pass <code>true</code>
// for <code>isAdditional</code>.
// <p>
// If the SmartClient Core module is loaded on the page where you're using the History module,
// you can use any format acceptable to +link{Class.fireCallback} as the callback.  The
// parameters 'id' and 'data' will be passed to your callback, in that order.
// <p>
// If the SmartClient Core module is not loaded on the page, you can use one of the following
// formats:
// <ul>
// <li>A function that takes an id and a data argument, in that order.
// <li>An object literal with a property named 'method' whose value is a function that takes
// an id and a data argument, in that order; and a property named 'target' that specifies the
// object on which the callback function should be applied.  So, e.g:
// <pre>
// {target: myObj, method: myObj.myFunction}
// </pre>
// </ul>
// The user can navigate to a synthetic history entry (and trip this callback) in one of two ways:
// <ul>
// <li>When +link{History.addHistoryEntry()} method is called, a new URL associated with the
//     history entry is generated, and the browser's back/forward navigation buttons become active.
//     The user can then navigate back to a stored history entry via standard browser history
//     navigation, or by explicitly hitting the appropriate URL. In this case both the ID and
//     data parameter passed to +link{History.addHistoryEntry()} will be available when the
//     callback fires.</li>
// <li>Alternatively the user can store a generated history URL (for example in a browser bookmark)
//     and navigate directly to it in a new browser session. In this case the 'addHistoryEntry()'
//     may not have been fired within the browser session. This callback will still fire with the
//     appropriate history ID but the data parameter will be null. You can disable this behavior
//     by passing in the <code>requiresData</code> parameter.</li>
// </ul>
//
// If this method is called before the page has loaded, and the page initially has a URL with
// a history ID, the callback will be fired with the appropriate ID on page load.
// However if a history callback is registered after the page has loaded, it will not be fired
// until the user moves to a new synthetic history entry. If you wish to explicitly check the
// current URL for a history entry, you can use the +link{History.getCurrentHistoryId()} method.
// <p>
// When the user transitions to the history entry immediately before the first synthetic
// history entry, the callback is fired with an id of null.
//
// @param callback (String | Object) The callback to invoke when the user navigates to a
// synthetic history entry.
// @param requiresData (boolean) If passed, this callback will only be fired if the user is
// navigating to a history entry that was explicitly generated in this browser session.
// @param [isAdditional] (boolean) If false or unspecified, then the callback is considered to
// be the primary callback, replacing the previous primary callback if the primary callback was
// previously registered. If true, then the callback is an additive callback; that is, it is
// called in addition to the primary callback, and after the primary callback is called.
// @return (int) the ID of the callback. This can be passed to +link{History.unregisterCallback()}
// to remove the callback.
// @visibility external
//<
_callbacksRegistry: [],
_nextCallbackID: 1, // 0 is currently reserved for the primary callback, but this is an internal
                    // detail that may change without notice
registerCallback : function (callback, requiresData, isAdditional) {

    if (callback == null) {
        if (!isAdditional) this.unregisterCallback(0);
        return -1;
    }

    var historyId;
    if (isAdditional) {
        historyId = this._nextCallbackID++;
    } else {
        // unregister the previous primary callback, if set
        this.unregisterCallback(0);

        historyId = 0;
    }

    var r = {
        callback: callback,
        requiresData: !!requiresData,
        ID: historyId
    };

    if (isAdditional) {
        this._callbacksRegistry[this._callbacksRegistry.length] = r;
    } else {
        // make sure that the primary callback is at the beginning of the _callbacksRegistry
        // array so that it is called first.
        this._callbacksRegistry.unshift(r);
    }
    return historyId;
},

//> @staticMethod History.unregisterCallback()
// Unregisters a callback so that it will no longer be called when the user navigates to a synthetic
// history entry.
//
// @param id (int) the ID of the callback that was returned by +link{History.registerCallback()}.
// @return (boolean) <code>true</code> if the callback registration was located and removed;
// <code>false</code> otherwise.
// @visibility external
//<
unregisterCallback : function (historyId) {
    var pos;
    var registry = this._callbacksRegistry;

    // we can't use the Array.findIndex() utility here because the History module may be
    // used standalone, without ISC_Core being loaded
    for (pos = 0; pos < registry.length; ++pos) {
        var r = registry[pos];
        if (r.ID == historyId) break;
    }

    // not found
    if (pos >= registry.length) return false;


    registry.splice(pos, 1);
    return true;
},

//> @staticMethod History.getCurrentHistoryId()
//
// Returns the current history id as reflected by the current URL.
//
// @return (String) The current history id as reflected by the current URL.
// @visibility external
//<
getCurrentHistoryId : function () {
    var historyId = this._getHistory(location.href);
    if (historyId == "_isc_H_init") return null;
    return historyId;
},


//> @staticMethod History.getHistoryData()
//
// Returns the data associated with the specified history id.
//
// @param id (String) The id for which to fetch history data.
// @return (Any) The data associated with the specified history id.
// @visibility external
//<
getHistoryData : function (historyId) {
    return this.historyState ? this.historyState.data[historyId] : null;
},


//> @staticMethod History.setHistoryTitle()
//
// Sets the title associated with all history entries.  This is the string that appears in the
// history drop-down.  If left unset, this default to the history id that is passed into
// +link{History.addHistoryEntry}.
// <p>
// Note: Currently, this works in IE only.  You may call this method in all other browsers,
// but it will not change what's displayed in the history drop-down.
//
// @param title (String) The title to show in the history drop-down.
// @visibility external
//<
setHistoryTitle : function (title) {
    this.historyTitle = title;
},

//> @staticMethod History.addHistoryEntry()
//
// Call this method to add a synthetic history entry.  The new history entry is added in the
// history stack after the currently visible page - in exactly the same way as the browser
// would treat a new page transition at this point.  In other words, if the user has navigated
// ten pages using, say, a mixture of synthetic and real history entries, then presses back
// five times and then triggers a call to this method, the history entry will be created at the
// 6th position in the history stack and any history entries forward of that will be discarded.
// <p>
// <b>NOTE:</b> Browsers including Chrome and Firefox require a delay, even a minimal 1 millisecond
// timeout, between additions to the browser's history stack or else only the last addition
// will have an effect. The History module does not allow two different (by id) history entries
// to be added in the same thread of execution. To check whether another history entry can be
// added by the current thread, call +link{History.readyForAnotherHistoryEntry()}.
// <p>
// This method must be called with an id.  This id can be any string - it will be URL-encoded
// and added to the current page URL as an anchor (e.g. #foo).  This URL change allows the user
// to bookmark this particular application state.  When the user next navigates to this history
// entry, the id you supplied here will be passed back to the callback you supplied via
// +link{History.registerCallback}.
// <p>
// You may also optionally supply some arbitrary data to associate with this history entry.
// If you do this, the data you passed in will be passed back to you as part of the callback
// you specified via +link{History.registerCallback}.  This data object can be anything you
// want, but there are some caveats:
// <ul>
// <li>The data parameter is currently supported by all SmartClient-supported browsers except
// <b>Safari</b></li>
// <li>As long as the user has not navigated away from the top-level page (i.e. the user is
// navigating within synthetic history entries only), whatever data you pass in will be handed
// back to you.
// <li>When the user navigates away from the current page, SmartClient will attempt to
// serialize the data into a string so that when/if the user comes back to this history entry,
// it can be deserialized and passed back to your logic.  To take advantage of this, you need
// to make sure that your data is serializeable.  As long as your data is a native datatype
// (String, Number, Boolean) or a collection of such datatypes (collections meaning object
// literals and arrays), then it will serialize correctly.  Things like pointers to the
// document object and functions cannot be serialized.
// <li>In order for the serialization to occur on a page transition, you must have the
// SmartClient Core module loaded on the page at the time of the transition.  If it's not
// available, the data will be lost, but you will still get a callback with the id you specify
// if the user navigates back to this history entry later.
// <li>The data associated with this history entry will persist as long as at least one
// instance of the browser remains open on the user's machine.  Once the user closes all
// browser instances, the data will be lost.
// <li>Also, the user can trigger a history callback at any time by navigating to a bookmarked
// history entry that may have been created in a past session, such that no data is associated
// with that id in the current session.  How you choose to handle that situation is up to you.
// </ul>
// <p>
// You're always guaranteed to receive the id you associate with a history entry in the
// callback that you specify, but the data you associated may or may not be available, so be
// careful about how you use it. Note that by passing the <code>requiresData</code> parameter
// to +link{History.registerCallback()} you can suppress the callback from firing unless the stored
// data object is actually available.
//
// @param id (String) The id you want to associate with this history entry.  This value will
// appear as an anchor reference at the end of the URL string.  For example, if you pass in
// "foo" as the id, the URL will then have a #foo tacked on the end of it.  This id will be
// passed back to the callback you specified in +link{History.registerCallback} when the user
// navigates to this history entry in the future.
//
// @param [title] (String) The title to show in the history drop-down for this history entry.  If
// not specified, the <code>id</code> is used, unless you've set an explicit history title via
// +link{History.setHistoryTitle}.  Note: this currently works in IE only.  You may pass a
// title in any other browser, but it will not change what's displayed in the history
// drop-down.
//
// @param [data] (Any) Arbitrary data to associate with this history entry.  When the user next
// navigates to this history entry, this data will be provided as an argument to your callback
// function.  Note that the SmartClient Core module is also required to be loaded on the page
// for this particular feature to work.
//
// @visibility external
//<
_finishAddingHistoryEntryTEAScheduled: false,
addHistoryEntry : function (historyId, title, data) {
    //>DEBUG
    this.logDebug("addHistoryEntry: id=" + historyId + " data=" + (isc.echoAll ? isc.echoAll(data) : String(data)));
    //<DEBUG

    // Avoid #null situations. Unfortunately we can't remove the anchor entirely (see below)
    if (historyId == null) historyId = "";

    if (isc.Browser.isSafari && isc.Browser.safariVersion < 500) {
        // We'd like to simply change the hash in the URL and call it a day.  That would at
        // least allow the user to bookmark the page.  Unfortunately this doesn't work - Canvas
        // rendering magically breaks after this is done, producing "DOM Exception 8".
        //
        // I tried dynamically inserting an anchor tag with the name of the id, just in case
        // Safari was angry with the lack of an actual anchor target for the new URL, but that
        // didn't change anything.  Revisit later.
        //
        // Last tested in Safari 2.0.4 (419.3)
        //location.href = this._addHistory(location.href, historyId);
        return;
    }


    if (!isc.SA_Page.isLoaded()) {
        this.logWarn("You must wait until the page has loaded before calling "
                     +"isc.History.addHistoryEntry()");
        return;
    }



    // clean up the history stack if the ID of the current URL isn't at the top of the stack.
    var currentId = this._lastHistoryId;

    // if no data was passed in, store explicit null rather than leaving undefined
    // we use this to detect that this was a registered history entry (this session)
    var undef;
    if (data === undef) data = null;

    // disallow sequentual duplicate entries
    if (currentId == historyId) {
        // treat it as overwrite of data
        if (this.historyState.data.hasOwnProperty(historyId)) {
            this.historyState.data[historyId] = data;
            this._saveHistoryState();
        }
        return;
    }

    if (this._finishAddingHistoryEntryTEAScheduled) {
        this.logError("History.addHistoryEntry() cannot be called with different IDs in the " +
                      "same thread. In the current thread of execution, addHistoryEntry() was " +
                      "previously called with id:'" + this.historyState.stack[this.historyState.stack.length - 1] +
                      "', but an attempt was made to add a history entry with id:'" + historyId + "'. " +
                      "Only the first history entry will be added.");
        return;
    }

    // remove orphaned history entries
    while (this.historyState.stack.length) {
        var topOfStack = this.historyState.stack.pop();
        if (topOfStack == currentId) {
            this.historyState.stack.push(topOfStack);
            break;
        }
        // delete data associated with this id
        delete this.historyState.data[topOfStack];
    }
    this.historyState.stack[this.historyState.stack.length] = historyId;
    this.historyState.data[historyId] = data;

    this._saveHistoryState();

    if (this.usePushState) {

        if (this.pushStateMode == "queryParam" && this._handleHashChange && location.hash) {
            window.history.replaceState({historyId: decodeURI(location.hash.substring(1))}, '');
        }
        window.history.pushState({historyId: historyId}, '', this._addHistory(location.href, historyId));
    } else {
        if (isc.Browser.isIE) {
            if (historyId != null && document.getElementById(historyId) != null) {
                this.logWarn("Warning - attempt to add synthetic history entry with id that conflicts"
                             +" with an existing DOM element node ID - this is known to break in IE");
            }

            // navigate the iframe forward
            //

            // if this is the very-first synthetic history entry, add an extra entry for the
            // current URL
            if (currentId == null) {
                // the title for this first entry is the title of this page - which is the <title>
                // if there's one on the page, or, failing that, the href of the page.
                var initTitle = location.href;
                var docTitle = document.getElementsByTagName("title");
                if (docTitle.length) initTitle = docTitle[0].innerHTML;
                this._iframeNavigate("_isc_H_init", initTitle);
            }
            this._iframeNavigate(historyId, title);
        } else {
            // Moz/FF
            // update the visible URL (this actually creates the history entry)
            location.href = this._addHistory(location.href, historyId);
        }
    }
    this._lastURL = location.href;

    if (isc.EH) {
        isc.EH._setThreadExitAction(this._finishAddingHistoryEntry);
    } else {
        setTimeout(this._finishAddingHistoryEntry, 0);
    }
    this._finishAddingHistoryEntryTEAScheduled = true;
    this._lastHistoryId = historyId;
},

_finishAddingHistoryEntry : function () {

    // _finishAddingHistoryEntry() will be called with `this === window'
    if (isc.History._finishAddingHistoryEntryTEAScheduled) {
        isc.History._finishAddingHistoryEntryTEAScheduled = false;
    }
},

//> @staticMethod History.readyForAnotherHistoryEntry()
//
// Can another history entry be added to the browser's history stack in the current thread?
// <p>
// Browsers including Chrome and Firefox require a delay, even a minimal 1 millisecond
// timeout, between additions to the browser's history stack or else only the last addition
// will have an effect. The History module does not allow two different (by id) history entries
// to be added in the same thread of execution.
//
// @return (boolean) whether another history entry can be added via +link{History.addHistoryEntry()}.
// @visibility external
//<
readyForAnotherHistoryEntry : function () {
    return !this._finishAddingHistoryEntryTEAScheduled;
},

_iframeNavigate : function (historyId, title) {
    this._ignoreHistoryCallback = true;

    // need to quote special chars because we're document writing this id into the the iframe
    var escapedId = this._asSource(historyId);
    title = (title != null ? title
                           : (this.historyTitle != null ? this.historyTitle
                                                        : historyId));

    var html = "<HTML><HEAD><TITLE>"+
               (title == null ? "" : this._asHTML(title))+
               "</TITLE></HEAD><BODY><SCRIPT>var pwin = window.parent;if (pwin && pwin.isc)pwin.isc.History.historyCallback(window, " + escapedId.replace(/<\/script\s*>/gi, "</\"+\"script>") + ");</SCRIPT></BODY></HTML>";
    var win = this._historyFrame.contentWindow;
    win.document.open();
    win.document.write(html);
    win.document.close();
},

// in IE, this method will always return false before pageLoad because historyState is not
// available until then.  In Moz/FF, this method will return accurate data before pageLoad.
haveHistoryState : function (historyId) {
    if (isc.Browser.isIE && !isc.SA_Page.isLoaded()) {
        this.logWarn("haveHistoryState() called before pageLoad - this always returns false"
                    +" in IE because state information is not available before pageLoad");
    }
    var undef;
    return this.historyState && this.historyState.data[historyId] !== undef;
},


_getIsomorphicDir : function () {
    return window.isomorphicDir ? window.isomorphicDir : "../isomorphic/";
},

// prefer pushState if browser supports it and not explicitly disabled
usePushState: window.history != null && window.history.pushState != null && window.isc_history_usePushState !== false,
// valid values: 'queryParam', 'hashFragment'
pushStateMode: "hashFragment",
pushStateQueryParamName: "hid",

// this method is called before pageLoad at the end of this file
_init : function () {
    this.logInfo("History initializing");
    if (this._trackingHistory) return;
    this._trackingHistory = true;

    // in safari we only support chaning the top-level URL to something bookmarkable, but
    // history support is non-existant at present
    if (isc.Browser.isSafari && isc.Browser.safariVersion < 500) return;

    // write out a form that will store serialized data associated with each history id.  We'll
    // use this to support cross-page transition history in IE.  Also, this allows the user to
    // associate arbitrary data with an id, which will be available in his callback.
    //
    // This allows the pattern of registering a callback that simply does eval(data) where
    // that's appropriate.
    //
    // Note: setting visibility:hidden on the form breaks body styling in IE.  Setting
    // display:none on the form breaks page rendering completely in IE.   But setting
    // display:none on the textarea works around that problem.
    //
    var formHTML = "<form style='position:absolute;top:-1000px' id='isc_historyForm'>"
           + "<textarea id='isc_historyField' style='display:none'></textarea></form>";
    document.write(formHTML);

    if (isc.Browser.isIE) {
        var frameHTML = "<iframe id='isc_historyFrame' src='" + this.getBlankFrameURL() +
                        "' style='position:absolute;visibility:hidden;top:-1000px'></iframe>";
        document.write(frameHTML);
        this._historyFrame = document.getElementById('isc_historyFrame');

        // make sure the frame isn't the last thing in the BODY tag - see comments in
        // createAbsoluteElement() for notes on this.
        document.write("<span id='isc_history_buffer_marker' style='display:none'></span>");
    }

    // init() calls _completeInit() in Moz, but in IE we must wait for page load before we can
    // get the form auto-fill data out.
    if (isc.Browser.isIE) {
        isc.SA_Page.onLoad(function () { this._completeInit() }, this);
    } else {
        this._completeInit();
    }

    if (this.usePushState) {
        this._initialURL = location.href;
        window.onpopstate = function (event) {
            // we get an onpopstate with a null state on a hash fragment change - ignore here
            // and handle below with onhashchange
            if (!event.state) return;
            isc.History._fireHistoryCallback(event.state.historyId);
        }

        if (this.pushStateMode == "hashFragment" && !isc.Browser.isIE) {
            // also support firing callbacks on location.hash changes in this mode

            window.onhashchange = function (event) {
                if (event.newURL && event.newURL.indexOf("#") != -1) {
                    var historyId = decodeURIComponent(event.newURL.substring(
                                                           event.newURL.indexOf("#") + 1));
                    isc.History._fireHistoryCallback(historyId);
                } else {
                    isc.History.logWarn("Unable to fire history callback onhashchange from: " +
                        event.oldURL + " to: " + event.newURL + "  - reason: no hash in newURL");
                }
            }
            // explicit flag since onhashchange() might have been set elsewhere
            this._handleHashChange = true;
        }
    }
},

// getBlankFrameURL(): When we write out the history frame, we need to give it an explicit src URL
// to avoid warnings about secure/non-secure items in IE6 / https

getBlankFrameURL : function () {
    if (isc.Page) return isc.Page.getBlankFrameURL();

    // the special directories handling hasn't been set up yet so figure out the URL
    // explicitly
    if (isc.Browser.isIE && ("https:" == window.location.protocol ||
                                document.domain != location.hostname ))
    {
        var path,
            isomorphicDir = window.isomorphicDir;
        // check for absolute isomorphicDir get the path
        if (isomorphicDir &&
            (isomorphicDir.indexOf("/") == 0 || isomorphicDir.indexOf("http") == 0))
        {
            path = isomorphicDir;
        } else {
            // combine the page base URL with the (relative) isomorphicDir
            path = window.location.href;
            if (path.charAt(path.length-1) != "/") {
                path = path.substring(0, path.lastIndexOf("/") + 1);
            }
            path += (isomorphicDir == null ? "../isomorphic/" : isomorphicDir);
        }
        path += "system/helpers/empty.html";
        return path;
    }
    return "about:blank";

},

_getFormValue : function () {
    var field = document.getElementById("isc_historyField");
    return field ? field.value : null;
},

_setFormValue : function (value) {
    var field = document.getElementById("isc_historyField");
    if (field) field.value = value;
},

// this method is called at the end of this file - after pageLoad in IE and synchronously after
// init() in Moz/FF
_completeInit : function () {
    // grab the serialized historyState from form auto-fill
    var historyState = this._getFormValue();
    if (historyState) {

        historyState = new Function("return ("+historyState + ")")();
    }

    // historyState = {
    //     stack: [id1, id2, id3 ... idN],
    //     data: {
    //         id1: data,
    //         id2: data
    //         ...
    //         idN: data
    //     }
    // }

    // if we had no persisted historyState, init a skeleton
    if (!historyState) historyState = { stack: [], data: {} };
    this.historyState = historyState;
    this.logInfo("History init complete");

    // in Moz, the only way to detect history changes is to watch the URL of the top-level page
    // for a delta.  In IE, we don't need this because we get an onload event from the iframe,
    // but we still need to watch the top-level URL because the user may use a bookmark to
    // navigate to a different history entry, or click on a series of links in another page
    // that points to history ids on this page and since, in that case, there is no navigation
    // of the iframe, we won't get a history navigation event.
    //
    // FIXME: We currently use the Moz workaround for Opera as well, but Opera has the
    // mindnumbing feature of suspending timeouts whenever the chrome back/forward buttons are
    // pressed and only resuming them when the mouse moves over the page.  So if you're just
    // hitting back/forward in the chrome, you're out of luck - until you mouse over the page.
    // Using z/x keyboard shortcuts works and so does the page context menu.  This sucks and
    // is completely stupid because it's not like that's what you'd ever want.  This
    // problem would affect sites that set up timers to do things on the page (like rotate ads)
    // and also use anchors on the page.  User hits an anchor, hits back, and all animations
    // stop. Nice one guys.
    this._lastURL = location.href;
    if (!this.usePushState) {
        // no need to poll if we have pushState support
        this._historyTimer = window.setInterval("isc.History._statHistory()", this._historyStatInterval);
    }

    // fire the initial history callback here
    // Note In IE we use an IFRAME to track state across page transitions.
    // 2 possibilities here:
    // 1) we are hitting the page directly with a URL matching a history entry. In this case
    //    the iframe contains no info about any stored current history entries.
    //    In this case we do the same logic as in Moz and simply fireInitialHistoryCallback on load
    // 2) If a history entry was added, then the user navigated off this page, then came back to it
    //    the IFRAME load event will trigger a standard history navigation.
    // In this second case we'll essentially get two calls to the history callback method.
    // We catch this by simply suppressing firing the history callback twice in a row with the
    // same history entry ID.
    isc.SA_Page.onLoad(this._fireInitialHistoryCallback, this);
},

_fireInitialHistoryCallback : function () {

    // only fire once
    if (this._firedInitialHistoryCallback) return;

    // fire the initial history callback once we a) have a callback registered and b) pageLoad
    // has occurred.
    if (this._callbacksRegistry.length != 0 && isc.SA_Page.isLoaded()) {
        this._firedInitialHistoryCallback = true;

        // if we have history state, then it's a history transition for the initial load.
        var historyId = this._getHistory(location.href);
        this._fireHistoryCallback(historyId);
    }
},

// helper methods to get and add history to URLs.  Anchor values are automatically
// encoded/decoded by these.

_addHistory : function (url, historyId) {
    if (this.usePushState && this.pushStateMode == "queryParam") {
        url = this._initialURL;
        var paramName = this.pushStateQueryParamName;
        if (new RegExp("(\\?|&)"+paramName+"=").test(url)) {
            // already contains this query param - overwrite value
            url = url.replace(new RegExp("(\\?|&)"+paramName+"=(.*?)(&|$)"), "$1"+paramName+"="+encodeURIComponent(historyId)+"$3");
        } else {
            url += (url.contains("?") ? "&" : "?") + encodeURIComponent(paramName)+"="+encodeURIComponent(historyId);
        }
        return url;
    } else {
        var match = url.match(/([^#]*).*/);
        return match[1]+"#"+encodeURI(historyId);
    }
},

_getHistory : function (url) {
    if (this.usePushState && this.pushStateMode == "queryParam") {
        url = this._initialURL;
        var paramName = this.pushStateQueryParamName;
        var match = new RegExp("(\\?|&)"+paramName+"=(.*?)(&|$)").exec(url);
        return match ? decodeURIComponent(match[2]) : null;
    } else {
        var match = location.href.match(/([^#]*)#(.*)/);
        return match ? decodeURIComponent(match[2]) : null;
    }
},


// How often do we poll to see if the URL has changed?
_historyStatInterval: 100,


_saveHistoryState : function() {
    if (isc.Comm) {
        this._setFormValue(isc.Comm.serialize(this.historyState));
    }
},

// Moz, Opera
_statHistory : function () {
    if (location.href != this._lastURL) {
        var historyId = this._getHistory(location.href);
        this._fireHistoryCallback(historyId);
    }
    this._lastURL = location.href;
},


// IE only - called by callback.html
historyCallback : function (win, currentFrameHistoryId) {
    // never show the user the special "init" ID
    if (currentFrameHistoryId == "_isc_H_init") currentFrameHistoryId = "";
    var newURL = this._addHistory(location.href, currentFrameHistoryId);
    // navigation has occurred, update the top-level URL to reflect the current id
    if (isc.SA_Page.isLoaded()) {
        location.href = newURL;
        // update _lastURL so that _statHistory() doesn't double-fire this callback
        this._lastURL = newURL;
    } else {
        // update _lastURL so that _statHistory() doesn't double-fire this callback
        isc.SA_Page.onLoad(function () {
            location.href = this._addHistory(location.href, currentFrameHistoryId);
            this._lastURL = newURL;
        }, this);
    }

    // if this callback is the result of the creation of a new synthetic history entry, don't
    // fire the callback
    if (this._ignoreHistoryCallback) {
        this._ignoreHistoryCallback = false;
        return;
    }

    if (isc.SA_Page.isLoaded()) {
        this._fireHistoryCallback(currentFrameHistoryId);
    } else {
        isc.SA_Page.onLoad(function () { this._fireHistoryCallback(currentFrameHistoryId) }, this);
    }
},

_fireHistoryCallback : function (historyId) {
    // suppress calling the same history callback twice in a row
    if (this._lastHistoryId == historyId) {
        // if this is the first time the callback is fired and _lastHistoryId==historyId,
        // the user has transitioned back to an anchorless URL - let that fire
        if (this._firedHistoryCallback) return;
    }
    this._firedHistoryCallback=true;

    var registry = this._callbacksRegistry;
    if (registry.length == 0) {
        this.logWarn("ready to fire history callback, but no callback registered."
                    +"Please call isc.History.registerCallback() before pageLoad."
                    +" If you can't register your callback before pageLoad, you"
                    +" can call isc.History.getCurrentHistoryId() to get the ID"
                    +" when you're ready.");
        return;
    }

    if (historyId == "_isc_H_init") historyId = null;

    var haveData = this.haveHistoryState(historyId);

    // create a copy of _callbacksRegistry, but appropriately filtered. If, for example, the
    // callback requires data, but we don't have data, then the callback is excluded from the
    // filtered copy.
    var filteredRegistry;
    if (haveData) {
        // no need to go through all of the registrations if we have data. Just create a duplicate.
        filteredRegistry = registry.slice();
    } else {
        filteredRegistry = [];
        for (var i = 0, len = registry.length; i < len; ++i) {
            var r = registry[i];
            if (!r.requiresData) filteredRegistry[filteredRegistry.length] = r;
        }
    }

    if (!haveData) {
        if (filteredRegistry.length == 0) {
            this.logWarn("User navigated to URL associated with synthetic history ID:" + historyId +
            ". This ID is not associated with any synthetic history entry generated via " +
            "History.addHistoryEntry(). Not firing a registered historyCallback as " +
            "all callbacks were registered with parameter requiring a data object. " +
            "This can commonly occur when the user navigates to a stored history entry " +
            "via a bookmarked URL.");
            return;
        }
    }

    var data = this.historyState.data[historyId];

    // store for getLastHistoryId()
    this._lastHistoryId = historyId;

    //>DEBUG
    this.logDebug("history callback: " + historyId);
    //<DEBUG

    // fire all of the callbacks
    for (var i = 0, len = filteredRegistry.length; i < len; ++i) {
        var r = filteredRegistry[i],
            callback = r.callback;
        if (isc.Class) {
            isc.Class.fireCallback(callback, ["id", "data"], [historyId, data]);

        } else {
            var args = [historyId, data];
            if (callback.method != null) {
                callback = isc.addProperties({}, callback);
                callback.args = args;
                this.fireSimpleCallback(callback);
            } else {
                callback.apply(null, args);
            }
        }
    }
}

}); // end History class


// mandatory pre-page load init
isc.History._init();
isc._debugModules = (isc._debugModules != null ? isc._debugModules : []);isc._debugModules.push('History');isc.checkForDebugAndNonDebugModules();isc._moduleEnd=isc._History_end=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc.Log&&isc.Log.logIsInfoEnabled('loadTime'))isc.Log.logInfo('History module init time: ' + (isc._moduleEnd-isc._moduleStart) + 'ms','loadTime');delete isc.definingFramework;if (isc.Page) isc.Page.handleEvent(null, "moduleLoaded", { moduleName: 'History', loadTime: (isc._moduleEnd-isc._moduleStart)});}else{if(window.isc && isc.Log && isc.Log.logWarn)isc.Log.logWarn("Duplicate load of module 'History'.");}
/*

  SmartClient Ajax RIA system
  Version v12.0p_2019-08-11/LGPL Deployment (2019-08-11)

  Copyright 2000 and beyond Isomorphic Software, Inc. All rights reserved.
  "SmartClient" is a trademark of Isomorphic Software, Inc.

  LICENSE NOTICE
     INSTALLATION OR USE OF THIS SOFTWARE INDICATES YOUR ACCEPTANCE OF
     ISOMORPHIC SOFTWARE LICENSE TERMS. If you have received this file
     without an accompanying Isomorphic Software license file, please
     contact licensing@isomorphic.com for details. Unauthorized copying and
     use of this software is a violation of international copyright law.

  DEVELOPMENT ONLY - DO NOT DEPLOY
     This software is provided for evaluation, training, and development
     purposes only. It may include supplementary components that are not
     licensed for deployment. The separate DEPLOY package for this release
     contains SmartClient components that are licensed for deployment.

  PROPRIETARY & PROTECTED MATERIAL
     This software contains proprietary materials that are protected by
     contract and intellectual property law. You are expressly prohibited
     from attempting to reverse engineer this software or modify this
     software for human readability.

  CONTACT ISOMORPHIC
     For more information regarding license rights and restrictions, or to
     report possible license violations, please contact Isomorphic Software
     by email (licensing@isomorphic.com) or web (www.isomorphic.com).

*/

