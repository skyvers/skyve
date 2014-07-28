
/*

  SmartClient Ajax RIA system
  Version v9.1p_2014-03-26/LGPL Deployment (2014-03-26)

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


//> @class isc
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
isc.version = "v9.1p_2014-03-26/LGPL Deployment";
isc.versionNumber = "v9.1p_2014-03-26";
isc.buildDate = "2014-03-26";
isc.expirationDate = "";

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
    Tools: {present: "${includeTools}", name: "Portal and Tools Module"},
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


if (typeof isc.Packager != "object") {


}






//> @class Browser
// The <code>Browser</code> class contains various class attributes that indicate basic properties
// of the browser and whether certain features are enabled.
// @treeLocation Client Reference/Foundation
// @visibility external
//<
isc.addGlobal("Browser", {
    isSupported: false


});


// ----------------------------------------------------------------
// Detecting browser type
// ----------------------------------------------------------------

//>    @classAttr    Browser.isOpera        (boolean : ? : R)
//        Are we in Opera ?
//<
isc.Browser.isOpera = (navigator.appName == "Opera" ||
                    navigator.userAgent.indexOf("Opera") != -1);

//>    @classAttr    Browser.isNS (boolean : ? : R)
//        Are we in Netscape (including Navigator 4+, NS6 & 7, and Mozilla)
//      Note: Safari also reports itself as Netscape, so isNS is true for Safari.
//<
isc.Browser.isNS = (navigator.appName == "Netscape" && !isc.Browser.isOpera);

//>    @classAttr    Browser.isIE        (boolean : ? : R)
//        Are we in Internet Explorer?
//<
isc.Browser.isIE = (navigator.appName == "Microsoft Internet Explorer" &&
                    !isc.Browser.isOpera) ||
                   navigator.userAgent.indexOf("Trident/") != -1;

//>    @classAttr    Browser.isMSN        (boolean : ? : R)
//      Are we in the MSN browser (based on MSIE, so isIE will be true in this case)
//<
isc.Browser.isMSN = (isc.Browser.isIE && navigator.userAgent.indexOf("MSN") != -1);


//>    @classAttr    Browser.isMoz        (boolean : ? : R)
//        Are we in any Mozilla-derived browser, that is, a browser based on Netscape's Gecko
//      engine? (includes Mozilla and Netscape 6+)
//<
isc.Browser.isMoz = (navigator.userAgent.indexOf("Gecko") != -1) &&
    // NOTE: Safari sends "(like Gecko)", but behaves differently from Moz in many ways

    (navigator.userAgent.indexOf("Safari") == -1) &&
    (navigator.userAgent.indexOf("AppleWebKit") == -1) &&
    !isc.Browser.isIE;

//>    @classAttr    Browser.isCamino (boolean : false : R)
//  Are we in Mozilla Camino?
//<
isc.Browser.isCamino = (isc.Browser.isMoz && navigator.userAgent.indexOf("Camino/") != -1);

//>    @classAttr    Browser.isFirefox (boolean : false : R)
//  Are we in Mozilla Firefox?
//<
isc.Browser.isFirefox = (isc.Browser.isMoz && navigator.userAgent.indexOf("Firefox/") != -1);


//> @classAttr  Browser.isAIR    (boolean : ? : R)
// Is this application running in the Adobe AIR environment?
//<
isc.Browser.isAIR = (navigator.userAgent.indexOf("AdobeAIR") != -1);

//>    @classAttr    Browser.isWebKit (boolean : ? : R)
// Are we in a WebKit-based browser (Safari, Chrome, mobile Safari and Android, others).
//<
isc.Browser.isWebKit = navigator.userAgent.indexOf("WebKit") != -1;

//>    @classAttr    Browser.isSafari (boolean : ? : R)
// Are we in Apple's "Safari" browser? Note that this property will also be set for other
// WebKit based browsers (such as Google Chrome).
//<
// As far as we know all "true" Safari implementations idenify themselves in the userAgent with
// the string "Safari".
// However the GWT hosted mode browser on OSX is also based on apple webkit and should be treated
// like Safari but is not a Safari browser and doesn't identify itself as such in the useragent
// Reported UserAgent:
//  Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_5_5; en-us) AppleWebKit/525.18 (KHTML, like Gecko)
isc.Browser.isSafari = isc.Browser.isAIR || navigator.userAgent.indexOf("Safari") != -1 ||
                        navigator.userAgent.indexOf("AppleWebKit") != -1;


//> @classAttr Browser.isChrome (boolean : ? : R)
// Are we in the Google Chrome browser?
//<
// Behaves like Safari in most ways
isc.Browser.isChrome = isc.Browser.isSafari && (navigator.userAgent.indexOf("Chrome/") != -1);


if (!isc.Browser.isIE && !isc.Browser.isOpera && !isc.Browser.isMoz &&
    !isc.Browser.isAIR && !isc.Browser.isWebkit && !isc.Browser.isSafari)
{
    if (navigator.appVersion.indexOf("MSIE") != -1) {
        isc.Browser.isIE = true;
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
if (!isc.Browser.isIE) (function () {


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


//>    @classAttr    Browser.caminoVersion (string : ? : R)
//        For Camino-based browsers, the Camino version number.
//<
if (isc.Browser.isCamino) {
    // Camino Version is the last thing in the userAgent
    isc.Browser.caminoVersion =
        navigator.userAgent.substring(navigator.userAgent.indexOf("Camino/") +7);
}

if (isc.Browser.isFirefox) {
//>    @classAttr    Browser.firefoxVersion (string : ? : R)
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

//>    @classAttr    Browser.geckoVersion (integer : ? : R)
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

//> @classAttr  Browser.AIRVersion (string : ? : R)
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
// NT 5.0 is Win2k, NT5.0.1 is Win2k SP1
isc.Browser.isWin2k = navigator.userAgent.match(/NT 5.01?/) != null;

//>    @classAttr    Browser.isMac        (boolean : ? : R)
//        Is this a Macintosh computer ?
//<
isc.Browser.isMac = navigator.platform.toLowerCase().indexOf("mac") > -1;

isc.Browser.isUnix = (!isc.Browser.isMac &&! isc.Browser.isWin);

//> @groupDef mobileDevelopment
// SmartClient supports building web applications that can be accessed by mobile devices that
// support modern web browsers, specifically:
// <ul>
// <li> Safari on iOS devices (iPad, iPhone, iPod Touch)
// <li> Android's default (WebKit-based) browser
// <li> Windows Phone 7 (future, for 'Mango' and up)
// <li> Blackberry devices that use a WebKit-based browser (future)
// </ul>
// <P>
// <h3>Finger / touch events</h3>
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
// This means that most applications that are written initially to target desktop computers
// need little or no modification in order be able to run on tablet-sized devices (eg the
// iPad).  For handset-sized devices (phones, iPod touch), conditional logic may need to be
// added to make different use of the screen real estate.
// <P>
// <h3>Adapting to tablets and handsets</h3>
// SmartClient provides the +link{Browser.isTablet} variable which can be used to determine
// whether a device is tablet-size (e.g. iPad, Nexus 7) or handset-size (phones, iPod touch,
// and similar smaller form-factor devices). In most cases SmartClient will correctly determine
// whether the device running your application is a tablet. For any uncommon device for which
// this variable is incorrect, you can override SmartClient's detection logic by defining the
// <code>isc_isTablet</code> global with the correct value before SmartClient is loaded. Whenever
// the <code>isc_isTablet</code> global is defined, SmartClient will use this value for
// Browser.isTablet instead of its own detection logic.
// <P>
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
// appearance between the desktop and mobile rendering of the application is more important for
// familiarity than looking similar to other applications on the mobile device
// <li> there is no single consistent appearance across Android devices because different
// manufacturers customize the platform a great deal, so there's no single appearance to mimic
// <li> mobile platform design overhauls, such as the major changes from iOS6 to iOS7, can
// easily invalidate efforts to look like native applications on the device
// </ul>
// <P>
// <h3>Adapting to Screen Size and Orientation Change</h3>
// <P>
// Safari on the Apple iPod/iPhone supports explicitly configuring the viewport as detailed here:
// +externalLink{http://developer.apple.com/safari/library/documentation/AppleApplications/Reference/SafariWebContent/UsingtheViewport/UsingtheViewport.html}.
// Including these meta tags in your bootstrap HTML file will allow you to set
// a default "zoom level" - how many pixels show up on the screen in landscape or portrait
// mode as well as disabling the user's standard zoom interactions. We also have
// +link{Page.updateViewport(),an API} to configure the viewport programmatically at runtime.
// <P>
// It is recommended to start with the following viewport meta tag in the bootstrap HTML file:<br>
// <br>
// <code>&lt;meta name="viewport" content="initial-scale=1"&gt;</code><br>
// <br>
// .. and then use +link{Page.updateViewport()} to update the viewport meta tag. On tablet
// devices (see +link{Browser.isTablet}), it is recommended to scale the viewport to 125% via:<br>
// <code><smartclient>isc.</smartclient>Page.updateViewport(1.25, null, null, true);</code><br>
// <br>
// On handsets, it is recommended to set the viewport width to 700 via:<br>
// <code><smartclient>isc.</smartclient>Page.updateViewport(null, 700, null, true);</code>
// <P>
// Note that the +link{Page.getOrientation()} API may be used to determine the current
// orientation of the application, and +link{pageEvent,the page orientationChange event} will fire
// whenever the user rotates the screen allowing applications to directly respond to the user
// pivoting their device.
// <P>
// <h3>Packaging as a native application</h3>
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
// <li>isc.DataSource</li>
// <li>isc.ListGrid</li>
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
//   <li>Add a <code>&lt;script&gt;</code> tag to the <code>&lt;head&gt;</code> element to load <code>phonegap.js</code>:
//       <pre>&lt;script type="text/javascript" charset="UTF-8" language="JavaScript" src="phonegap.js"&gt;&lt;/script&gt;</pre>
//
//       <p><b>NOTE:</b> The <code>www/</code> folder should not contain <code>phonegap.js</code>.
//       In other words, don't try to copy <code>phonegap.js</code> into the <code>www/</code> folder.
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
//        <pre class="sourcefile">&lt;script type="text/javascript" language="JavaScript"&gt;
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
//     <pre class="sourcefile">&lt;script type="text/javascript" language="JavaScript"&gt;
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
// <h3>Android Platform</h3>
// To begin targeting Android devices, follow the instructions on the
// +externalLink{http://docs.phonegap.com/en/edge/guide_platforms_android_index.md.html,Android Platform Guide}.
//
// <p>It is helpful to monitor the LogCat view in Eclipse to verify that your application is working correctly.
// Common errors include:
// <ul>
// <li><code>Application Error The protocol is not supported. (gap://ready)</code>
//     <p>This means that the incorrect <code>phonegap.js</code> script is being used. You
//     must use the <code>phonegap.js</code> for Android.<!-- http://community.phonegap.com/nitobi/topics/error_starting_app_on_android -->
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
// <p>The Smart&nbsp;GWT Google Code project has a sample application called +externalLink{http://code.google.com/p/smartgwt/source/browse/#svn%2Ftrunk%2Fsamples%2Fphonegap%2FMyContacts,MyContacts} which demonstrates how
// to work with the PhoneGap API in a Smart&nbsp;GWT app. The main Smart&nbsp;GWT code is located at
// <code>+externalLink{http://code.google.com/p/smartgwt/source/browse/#svn%2Ftrunk%2Fsamples%2Fphonegap%2FMyContacts,trunk/samples/phonegap/MyContacts}</code>. An Xcode project used to package the app for iOS
// devices is located at <code>+externalLink{http://code.google.com/p/smartgwt/source/browse/#svn%2Ftrunk%2Fsamples%2Fphonegap%2FMyContacts-iOS,trunk/samples/phonegap/MyContacts-iOS}</code>. An Eclipse project used
// to package the app for Android devices is located at <code>+externalLink{http://code.google.com/p/smartgwt/source/browse/#svn%2Ftrunk%2Fsamples%2Fphonegap%2FMyContacts-Android,trunk/samples/phonegap/MyContacts-Android}</code>.
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

// Is the browser Mobile Firefox?
// https://wiki.mozilla.org/Compatibility/UADetectionLibraries
// https://developer.mozilla.org/en-US/docs/Gecko_user_agent_string_reference#Mobile_and_Tablet_indicators
isc.Browser.isMobileFirefox = isc.Browser.isFirefox && (navigator.userAgent.indexOf("Mobile") > -1 ||
                                                        navigator.userAgent.indexOf("Tablet") > -1);


isc.Browser.isMobileWebkit = (isc.Browser.isSafari && navigator.userAgent.indexOf(" Mobile/") > -1
    || isc.Browser.isAndroid
    || isc.Browser.isBlackBerry) && !isc.Browser.isFirefox;

// intended for general mobile changes (performance, etc)
isc.Browser.isMobile = (isc.Browser.isMobileFirefox ||
                        isc.Browser.isMobileWebkit);

//> @classAttr browser.isTouch (boolean : : RW)
// Is the application running on a touch device (e.g. iPhone, iPad, Android device, etc.)?
// <p>
// SmartClient's auto-detected value for <code>isTouch</code> can be overridden via
// +link{Browser.setIsTouch()}.
//
// @visibility external
//<

isc.Browser.isTouch = (isc.Browser.isMobileFirefox ||
                       isc.Browser.isMobileWebkit);

//> @classMethod browser.setIsTouch() (A)
// Setter for +link{Browser.isTouch} to allow this global variable to be changed at runtime.
// This advanced method is provided to override SmartClient's auto-detection logic, since the
// framework can only detect touch devices that existed at the time the platform was released.
// Any change to +link{Browser.isTouch} must be made before any component is created.
// <p>
// Note that setting <code>Browser.isTouch</code> might affect the values of
// +link{Browser.isDesktop}, +link{Browser.isTablet}, and/or +link{Browser.isHandset}.
//
// @param isTouch (boolean) new setting for <code>Browser.isTablet</code>.
// @visibility external
//<
isc.Browser.setIsTouch = function (isTouch) {
    isTouch = isc.Browser.isTouch = !!isTouch;

    if (isc.Browser.isDesktop) {
        isc.Browser.isHandset = false;
        isc.Browser.isTablet = false;
    } else {
        isc.Browser.isHandset = isTouch && !isc.Browser.isTablet;
        isc.Browser.isTablet = !isc.Browser.isHandset;
    }


};

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

if (isc.Browser.isIPad && isc.Browser.isMobileSafari && isc.Browser.iOSVersion == 7) {

    var iOS7IPadStyleSheetID = "isc_iOS7IPadStyleSheet";
    if (document.getElementById(iOS7IPadStyleSheetID) == null) {
        var styleElement = document.createElement("style");
        styleElement.id = iOS7IPadStyleSheetID;
        document.head.appendChild(styleElement);
        var s = styleElement.sheet;
        s.insertRule("\n@media (orientation:landscape) {\n" +
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

//> @classAttr browser.isTablet (boolean : : RW)
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
// must be made before any component is created.
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

//> @classAttr browser.isHandset (boolean : : RW)
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
// must be made before any component is created.
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

//> @classAttr browser.isDesktop (boolean : : RW)
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
// must be made before any component is created.
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

//>    @classAttr    Browser.lineFeed    (string : ? : RA)
//        Linefeed for this platform
//<
isc.Browser.lineFeed = (isc.Browser.isWin ? "\r\n" : "\r");

//>    @classAttr    Browser._supportsMethodTimeout    (string : ? : RA)
//        setTimeout() requires text string parameter in MacIE or IE 4
//<
isc.Browser._supportsMethodTimeout = false;//!(isc.Browser.isIE && (isc.Browser.isMac || isc.Browser.version == 4));

//>    @classAttr    Browser.isDOM (string : ? : RA)
//        Whether this is a DOM-compliant browser.  Indicates general compliance with DOM standards,
//      not perfect compliance.
//<
isc.Browser.isDOM = (isc.Browser.isMoz || isc.Browser.isOpera ||
                     isc.Browser.isSafari || (isc.Browser.isIE && isc.Browser.version >= 5));

//> @classAttr browser.isSupported (boolean : : R)
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
    // Safari (only available on Mac)
    isc.Browser.isSafari ||
    isc.Browser.isAIR
);


isc.Browser.nativeMouseMoveOnCanvasScroll =
    !isc.Browser.isTouch && (isc.Browser.isSafari || isc.Browser.isChrome);

//> @classAttr Browser.seleniumPresent (boolean : varies : R)
// Whether current page has been loaded by Selenium RC/WebDriver.
//<
isc.Browser.seleniumPresent = (function () {
    var match = location.href.match(/[?&](?:sc_selenium)=([^&#]*)/);
    return match && match.length > 1 && "true" == match[1];
})();

//> @type Autotest
// @value Browser.SHOWCASE autotest is targeting SmartClient or SGWT showcases
isc.Browser.SHOWCASE = "showcase";
// @value Browser.RUNNER autotest is targeting TestRunner-based JS tests
isc.Browser.RUNNER = "runner";
//<

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

//> @classAttr browser.useCSS3 (boolean : : R)
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
           isc_css3Mode === undefined)
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

// Test for availability of the Range.getBoundingClientRect() method which was added to
// CSSOM View as of the 04 August 2009 Working Draft.
// http://www.w3.org/TR/2009/WD-cssom-view-20090804/

isc.Browser.hasNativeGetRect = (!isc.Browser.isIE &&
                                (!isc.Browser.isSafari || !isc.Browser.isMac || isc.Browser.version >= 6) &&
                                !!document.createRange &&
                                !!(document.createRange().getBoundingClientRect));

isc.Browser.useClipDiv = (isc.Browser.isMoz || isc.Browser.isSafari || isc.Browser.isOpera);


isc.Browser._useNewSingleDivSizing = !(isc.Browser.isIE && isc.Browser.version < 10 && !isc.Browser.isIE9);

isc.Browser.useCreateContextualFragment = !!document.createRange && !!document.createRange().createContextualFragment;


isc.Browser.hasTextOverflowEllipsis = (!isc.Browser.isMoz || isc.Browser.version >= 7) &&
                                      (!isc.Browser.isOpera || isc.Browser.version >= 9);

// https://developer.mozilla.org/en-US/docs/CSS/text-overflow
isc.Browser._textOverflowPropertyName = (!isc.Browser.isOpera || isc.Browser.version >= 11 ? "text-overflow" : "-o-text-overflow");


isc.Browser._hasGetBCR = !isc.Browser.isSafari || isc.Browser.version >= 4;



// Does the browser support HTML5 drag and drop?
// http://caniuse.com/#feat=dragndrop
// http://www.whatwg.org/specs/web-apps/current-work/multipage/dnd.html#dnd
//
// This is set to false in IE because cross-window drags are not possible.

isc.Browser.hasNativeDrag = !isc.Browser.isTouch && "draggable" in document.documentElement && !isc.Browser.isIE;

// http://dom.spec.whatwg.org/#ranges
isc.Browser._hasDOMRanges = !!(window.getSelection && document.createRange && window.Range);

// Whether the browser supports the CSS `background-size' property.
// https://developer.mozilla.org/en-US/docs/Web/CSS/background-size
isc.Browser._supportsBackgroundSize = "backgroundSize" in document.documentElement.style;

// Does the browser support CSS3 transitions?
// http://caniuse.com/#feat=css-transitions
// Note: No need to check for "msTransition" because IE10 was the first version of IE to have
// CSS3 transitions support and this is unprefixed.
isc.Browser._supportsCSSTransitions = ("transition" in document.documentElement.style ||
                                       "WebkitTransition" in document.documentElement.style ||
                                       "MozTransition" in document.documentElement.style ||
                                       "OTransition" in document.documentElement.style);


isc.Browser._transitionEndEventType = ("WebkitTransition" in document.documentElement.style
                                       ? "webkitTransitionEnd"
                                       : ("OTransition" in document.documentElement.style
                                          ? (isc.Browser.isOpera && isc.Browser.version >= 12 ? "otransitionend" : "oTransitionEnd")
                                          : "transitionend"))





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
    if (isc[className]) return;  // don't redefine

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
        }

    });

    // alias
    classObj.isAn = classObj.isA;

    return classObj;
});


isc.defineStandaloneClass("SA_Page", {

_isLoaded : false,
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

isc.SA_Page.onLoad(function () { this._isLoaded = true; }, isc.SA_Page);


// Synthetic History Support
// --------------------------------------------------------------------------------------------
//



//> @class History
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

//> @classMethod history.registerCallback()
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
// @param callback (String or Object) The callback to invoke when the user navigates to a
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

    var id;
    if (isAdditional) {
        id = this._nextCallbackID++;
    } else {
        // unregister the previous primary callback, if set
        this.unregisterCallback(0);

        id = 0;
    }

    var r = {
        callback: callback,
        requiresData: !!requiresData,
        ID: id
    };

    if (isAdditional) {
        this._callbacksRegistry[this._callbacksRegistry.length] = r;
    } else {
        // make sure that the primary callback is at the beginning of the _callbacksRegistry
        // array so that it is called first.
        this._callbacksRegistry.unshift(r);
    }
    return id;
},

//> @classMethod history.unregisterCallback()
// Unregisters a callback so that it will no longer be called when the user navigates to a synthetic
// history entry.
//
// @param id (int) the ID of the callback that was returned by +link{History.registerCallback()}.
// @return (boolean) <code>true</code> if the callback registration was located and removed;
// <code>false</code> otherwise.
// @visibility external
//<
unregisterCallback : function (id) {
    var pos;
    var registry = this._callbacksRegistry;

    // we can't use the Array.findIndex() utility here because the History module may be
    // used standalone, without ISC_Core being loaded
    for (pos = 0; pos < registry.length; ++pos) {
        var r = registry[pos];
        if (r.ID == id) break;
    }

    // not found
    if (pos >= registry.length) return false;


    registry.splice(pos, 1);
    return true;
},

//> @classMethod History.getCurrentHistoryId()
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


//> @classMethod History.getHistoryData()
//
// Returns the data associated with the specified history id.
//
// @param id (String) The id for which to fetch history data.
// @return (any) The data associated with the specified history id.
// @visibility external
//<
getHistoryData : function (id) {
    return this.historyState ? this.historyState.data[id] : null;
},


//> @classMethod History.setHistoryTitle()
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

//> @classMethod History.addHistoryEntry()
//
// Call this method to add a synthetic history entry.  The new history entry is added in the
// history stack after the currently visible page - in exactly the same way as the browser
// would treat a new page transition at this point.  In other words, if the user has navigated
// ten pages using, say, a mixture of synthetic and real history entries, then presses back
// five times and then triggers a call to this method, the history entry will be created at the
// 6th position in the history stack and any history entries forward of that will be destroyed.
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
// @param id (string) The id you want to associate with this history entry.  This value will
// appear as an anchor reference at the end of the URL string.  For example, if you pass in
// "foo" as the id, the URL will then have a #foo tacked on the end of it.  This id will be
// passed back to the callback you specified in +link{History.registerCallback} when the user
// navigates to this history entry in the future.
//
// @param [title] (string) The title to show in the history drop-down for this history entry.  If
// not specified, the <code>id</code> is used, unless you've set an explicit history title via
// +link{History.setHistoryTitle}.  Note: this currently works in IE only.  You may pass a
// title in any other browser, but it will not change what's displayed in the history
// drop-down.
//
// @param [data] (any) Arbitrary data to associate with this history entry.  When the user next
// navigates to this history entry, this data will be provided as an argument to your callback
// function.  Note that the SmartClient Core module is also required to be loaded on the page
// for this particular feature to work.
//
// @visibility external
//<

addHistoryEntry : function (id, title, data) {
    //>DEBUG
    this.logDebug("addHistoryEntry: id=" + id + " data=" + (isc.echoAll ? isc.echoAll(data) : String(data)));
    //<DEBUG

    // Avoid #null situations. Unfortunately we can't remove the anchor entirely (see below)
    if (id == null) id = "";

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
        //location.href = this._addHistory(location.href, id);
        return;
    }


    if (!isc.SA_Page.isLoaded()) {
        this.logWarn("You must wait until the page has loaded before calling "
                     +"isc.History.addHistoryEntry()");
        return;
    }



    // clean up the history stack if the ID of the current URL isn't at the top of the stack.
    var currentId = this._getHistory(location.href);

    // if no data was passed in, store explicit null rather than leaving undefined
    // we use this to detect that this was a registered history entry (this session)
    var undef;
    if (data === undef) data = null;

    // disallow sequentual duplicate entries - treat it as overwrite of data
    if (currentId == id && this.historyState.data.hasOwnProperty(id)) {
        this.historyState.data[id] = data;
        this._saveHistoryState();
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
    this.historyState.stack[this.historyState.stack.length] = id;
    this.historyState.data[id] = data;
    //>DEBUG
    this.logDebug("historyState[id]: " + (isc.echoAll ? isc.echoAll(this.historyState.data[id]) : String(this.historyState.data[id])));
    //<DEBUG

    this._saveHistoryState();

    if (isc.Browser.isIE) {
        if (id != null && document.getElementById(id) != null) {
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
        this._iframeNavigate(id, title);
    } else {
        // Moz/FF
        // update the visible URL (this actually creates the history entry)
        location.href = this._addHistory(location.href, id);
        this._lastHistoryId = id;
    }
    this._lastURL = location.href;
},

_iframeNavigate : function (id, title) {
    this._ignoreHistoryCallback = true;

    // need to quote special chars because we're document writing this id into the the iframe
    var escapedId  = !this.isAString(id) ? id : id.replace(/\\/g, "\\\\").replace(/\"/g, "\\\"")
                                                  .replace(/\t/g, "\\t").replace(/\r/g, "\\r")
                                                  .replace(/\n/g, "\\n");
    var html = "<HTML><HEAD><TITLE>"+
               (title != null ? title : this.historyTitle != null ? this.historyTitle : id)+
               "</TITLE></HEAD><BODY><SCRIPT>var pwin = window.parent;if (pwin && pwin.isc)pwin.isc.History.historyCallback(window,\""+escapedId+"\");</SCRIPT></BODY></HTML>";
    var win = this._historyFrame.contentWindow;
    win.document.open();
    win.document.write(html);
    win.document.close();
},

// in IE, this method will always return false before pageLoad because historyState is not
// available until then.  In Moz/FF, this method will return accurate data before pageLoad.
haveHistoryState : function (id) {
    if (isc.Browser.isIE && !isc.SA_Page.isLoaded()) {
        this.logWarn("haveHistoryState() called before pageLoad - this always returns false"
                    +" in IE because state information is not available before pageLoad");
    }
    var undef;
    return this.historyState && this.historyState.data[id] !== undef;
},


_getIsomorphicDir : function () {
    return window.isomorphicDir ? window.isomorphicDir : "../isomorphic/";
},

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
    } else if (isc.Browser.isMoz || isc.Browser.isOpera || (isc.Browser.isSafari && isc.Browser.safariVersion >= 500)) {
        // in Moz, the form auto-fill values are available synchronously right after
        // document.write(), but in IE the values are not present until page load.
        this._completeInit();
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
    this._historyTimer = window.setInterval("isc.History._statHistory()", this._historyStatInterval);

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

    if (isc.Browser.isIE || isc.Browser.isMoz || isc.Browser.isOpera ||
        (isc.Browser.isSafari && isc.Browser.safariVersion >= 500))
    {
        isc.SA_Page.onLoad(this._fireInitialHistoryCallback, this);
    }
},

_fireInitialHistoryCallback : function () {

    // only fire once
    if (this._firedInitialHistoryCallback) return;

    // fire the initial history callback once we a) have a callback registered and b) pageLoad
    // has occurred.
    if (this._callbacksRegistry.length != 0 && isc.SA_Page.isLoaded()) {
        this._firedInitialHistoryCallback = true;

        // if we have history state, then it's a history transition for the initial load.
        var id = this._getHistory(location.href);
        this._fireHistoryCallback(id);
    }
},

// helper methods to get and add history to URLs.  Anchor values are automatically
// encoded/decoded by these.

_addHistory : function (url, id) {
    var match = url.match(/([^#]*).*/);
    return match[1]+"#"+encodeURI(id);
},

_getHistory : function (url) {
    var match = location.href.match(/([^#]*)#(.*)/);
    return match ? decodeURI(match[2]) : null;
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
        var id = this._getHistory(location.href);
        this._fireHistoryCallback(id);
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

_fireHistoryCallback : function (id) {
    // suppress calling the same history callback twice in a row
    if (this._lastHistoryId == id) {
        // if this is the first time the callback is fired and _lastHistoryId==id,
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

    if (id == "_isc_H_init") id = null;

    var haveData = this.haveHistoryState(id);

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
            this.logWarn("User navigated to URL associated with synthetic history ID:" + id +
            ". This ID is not associated with any synthetic history entry generated via " +
            "History.addHistoryEntry(). Not firing a registered historyCallback as " +
            "all callbacks were registered with parameter requiring a data object. " +
            "This can commonly occur when the user navigates to a stored history entry " +
            "via a bookmarked URL.");
            return;
        }
    }

    var data = this.historyState.data[id];

    // store for getLastHistoryId()
    this._lastHistoryId = id;

    //>DEBUG
    this.logDebug("history callback: " + id);
    //<DEBUG

    // fire all of the callbacks
    for (var i = 0, len = filteredRegistry.length; i < len; ++i) {
        var r = filteredRegistry[i],
            callback = r.callback;
        if (isc.Class) {
            isc.Class.fireCallback(callback, ["id", "data"], [id, data]);

        } else {
            var args = [id, data];
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

isc._debugModules = (isc._debugModules != null ? isc._debugModules : []);isc._debugModules.push('History');isc.checkForDebugAndNonDebugModules();isc._moduleEnd=isc._History_end=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc.Log&&isc.Log.logIsInfoEnabled('loadTime'))isc.Log.logInfo('History module init time: ' + (isc._moduleEnd-isc._moduleStart) + 'ms','loadTime');delete isc.definingFramework;}else{if(window.isc && isc.Log && isc.Log.logWarn)isc.Log.logWarn("Duplicate load of module 'History'.");}

/*

  SmartClient Ajax RIA system
  Version v9.1p_2014-03-26/LGPL Deployment (2014-03-26)

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

