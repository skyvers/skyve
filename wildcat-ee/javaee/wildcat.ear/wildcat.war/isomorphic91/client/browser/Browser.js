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

//>	@classAttr	Browser.isOpera		(boolean : ? : R)
//		Are we in Opera ?
//<
isc.Browser.isOpera = (navigator.appName == "Opera" ||
					navigator.userAgent.indexOf("Opera") != -1);

//>	@classAttr	Browser.isNS (boolean : ? : R)
//		Are we in Netscape (including Navigator 4+, NS6 & 7, and Mozilla)
//      Note: Safari also reports itself as Netscape, so isNS is true for Safari.
//<
isc.Browser.isNS = (navigator.appName == "Netscape" && !isc.Browser.isOpera);	

//>	@classAttr	Browser.isIE		(boolean : ? : R)
//		Are we in Internet Explorer?
//<
isc.Browser.isIE = (navigator.appName == "Microsoft Internet Explorer" &&
                    !isc.Browser.isOpera) ||
                   navigator.userAgent.indexOf("Trident/") != -1;

//>	@classAttr	Browser.isMSN		(boolean : ? : R)
//      Are we in the MSN browser (based on MSIE, so isIE will be true in this case)
//<
isc.Browser.isMSN = (isc.Browser.isIE && navigator.userAgent.indexOf("MSN") != -1);


//>	@classAttr	Browser.isMoz		(boolean : ? : R)
//		Are we in any Mozilla-derived browser, that is, a browser based on Netscape's Gecko 
//      engine? (includes Mozilla and Netscape 6+)
//<
isc.Browser.isMoz = (navigator.userAgent.indexOf("Gecko") != -1) &&
    // NOTE: Safari sends "(like Gecko)", but behaves differently from Moz in many ways
     
    (navigator.userAgent.indexOf("Safari") == -1) && 
    (navigator.userAgent.indexOf("AppleWebKit") == -1) &&
    !isc.Browser.isIE;

//>	@classAttr	Browser.isCamino (boolean : false : R)
//  Are we in Mozilla Camino?
//<
isc.Browser.isCamino = (isc.Browser.isMoz && navigator.userAgent.indexOf("Camino/") != -1);

//>	@classAttr	Browser.isFirefox (boolean : false : R)
//  Are we in Mozilla Firefox?
//<
isc.Browser.isFirefox = (isc.Browser.isMoz && navigator.userAgent.indexOf("Firefox/") != -1);


//> @classAttr  Browser.isAIR    (boolean : ? : R)
// Is this application running in the Adobe AIR environment?
//<
isc.Browser.isAIR = (navigator.userAgent.indexOf("AdobeAIR") != -1);

//>	@classAttr	Browser.isWebKit (boolean : ? : R)
// Are we in a WebKit-based browser (Safari, Chrome, mobile Safari and Android, others).
//<
isc.Browser.isWebKit = navigator.userAgent.indexOf("WebKit") != -1;

//>	@classAttr	Browser.isSafari (boolean : ? : R)
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


//>	@classAttr Browser.minorVersion		(number : ? : R)
//		Browser version, with minor revision included (4.7, 5.5, etc).
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

//>	@classAttr	Browser.version		(number : ? : R)
//		Browser major version number (integer: 4, 5, etc).
//<
isc.Browser.version = parseInt(isc.Browser.minorVersion);

// actually means IE6 or earlier, which requires radically different optimization techniques
isc.Browser.isIE6 = isc.Browser.isIE && isc.Browser.version <= 6;


//>	@classAttr	Browser.caminoVersion (string : ? : R)
//		For Camino-based browsers, the Camino version number.  
//<
if (isc.Browser.isCamino) {
    // Camino Version is the last thing in the userAgent
    isc.Browser.caminoVersion = 
        navigator.userAgent.substring(navigator.userAgent.indexOf("Camino/") +7);
}

if (isc.Browser.isFirefox) {
//>	@classAttr	Browser.firefoxVersion (string : ? : R)
//		For Firefox-based browsers, the Firefox version number.  
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

//>	@classAttr	Browser.geckoVersion (integer : ? : R)
//		For Gecko-based browsers, the Gecko version number.  
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


//>	@classAttr	Browser.safariVersion (number : ? : R)
//		in Safari, what is is the reported version number
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

//>	@classAttr	Browser.isWin		(boolean : ? : R)
//		Is this a Windows computer ?
//<
isc.Browser.isWin = navigator.platform.toLowerCase().indexOf("win") > -1;
// NT 5.0 is Win2k, NT5.0.1 is Win2k SP1
isc.Browser.isWin2k = navigator.userAgent.match(/NT 5.01?/) != null;

//>	@classAttr	Browser.isMac		(boolean : ? : R)
//		Is this a Macintosh computer ?
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

//>	@classAttr	Browser.lineFeed	(string : ? : RA)
//		Linefeed for this platform
//<
isc.Browser.lineFeed = (isc.Browser.isWin ? "\r\n" : "\r");

//>	@classAttr	Browser._supportsMethodTimeout	(string : ? : RA)
//		setTimeout() requires text string parameter in MacIE or IE 4
//<
isc.Browser._supportsMethodTimeout = false;//!(isc.Browser.isIE && (isc.Browser.isMac || isc.Browser.version == 4));	

//>	@classAttr	Browser.isDOM (string : ? : RA)
//		Whether this is a DOM-compliant browser.  Indicates general compliance with DOM standards,
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

//>	@classAttr	Browser.allowsXSXHR	(boolean : ? : RA)
//	Traditionally, web browsers reject attempts to make an XmlHttpRequest of a server other than the origin
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


