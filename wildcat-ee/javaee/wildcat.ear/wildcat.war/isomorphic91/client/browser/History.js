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

