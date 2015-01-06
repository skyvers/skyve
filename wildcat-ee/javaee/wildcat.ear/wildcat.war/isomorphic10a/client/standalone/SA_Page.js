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
