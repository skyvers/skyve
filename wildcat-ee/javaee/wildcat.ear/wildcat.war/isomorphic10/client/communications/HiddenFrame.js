/*
 * Isomorphic SmartClient
 * Version v10.0p_2014-09-10 (2014-09-10)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */








//>	@class	HiddenFrame
// This class provides a hidden frame that you can use to send messages to (and receive data
// from) the server invisibly without requiring an external frameset.
//<

// create the class
isc.ClassFactory.defineClass("HiddenFrame");

// set up static properties
isc.HiddenFrame.addClassProperties({
    // handle to the window object inside the frame element
    //_windowHandle: null,

    // handle to the frame element (not available in IE 5.5 or earlier versions)
    //_frameDomHandle: null,

    _stats : {
        draws: 0,
        destroys: 0
    },

    // all currently instantiated hiddenFrames
    _hiddenFrames: [],

    
    _killLoadingIndicators : function () {
        // the create/destroy cycle must be done off the thread created by the onload handler
        // of the iframe that started the indicators - since this method is often called in
        // that thread, set up a timeout here.
        if (isc.Browser.isMoz) this.delayCall("_doKillLoadingIndicators");
    },

    _doKillLoadingIndicators : function () {
        var hf = isc.HiddenFrame.create();
        hf.draw();
        hf.destroy();
    }
});

// set up default instance properties
isc.HiddenFrame.addProperties({
    //>	@attr	hiddenFrame.text		(string : "nothing" : IRW)
	//			text written into the hidden frame when it is drawn.
	//		@group	appearance
	//<
	text:"&nbsp;",

	//>	@attr	hiddenFrame.location	(URL : null : IRW)
	//			URL to set the frame to, set the hiddenFrame.location
	//			to the URL you want to send to the server.
	//		@group appearance
	//<

    _generated: true,

    // see draw; used to keep track of actions that should be delayed until the
    // targetable frame is created (for some browsers, need to wait until page load for creation)
    _callbackQueue:[],

    // polling interval (in ms) for checking if the iframe has finished loading
    pollInterval:100,

    useHtmlfile: false
});

//!>Deferred

// give it some default methods
isc.HiddenFrame.addMethods({


//>	@method	hiddenFrame.init()	(A)
//      constructor
//
//		@param	[all arguments]	(object)	objects with properties to override from default
//		@param	props		(object)	properties for the frame
//<
init : function () {
	// get a unique, global ID for this HiddenFrame
	isc.ClassFactory.addGlobalID(this);

    isc.HiddenFrame._hiddenFrames.add(this);

    
},


//>	@method	hiddenFrame.getID()	(A)
//			Return the global ID of this object
//		@return	(string)	global identifier for this object
//<
getID : function () {
	return this.ID;
},


// callback is an optional argument; it's a string to be eval'ed after the frame is drawn.
draw : function (callback) {
    // if frame has already been drawn, eval the callback if any, and exit
    if (this.isDrawn()) {
        if (callback != null) this.fireCallback(callback);
        return this;
    }

    // if it's safe to draw the frame, go ahead, otherwise delay until page load, and add the
    // callback (if any) to the callback queue
    if (!this._safeToDraw()) {
        // set page load event to call draw if it hasn't been set before
        if (!this._markedForDrawing) {
            this._drawEvent = isc.Page.setEvent("load", this.getID() + ".draw()", isc.Page.FIRE_ONCE);
            this._markedForDrawing = true;
        }
        // add the callback (if any) to the callback queue
        if (callback != null) {
            this._callbackQueue[this._callbackQueue.length] = callback;
        }
    } else {
        this._draw();
        // iterate through the callbacks in the queue
        for (var i = 0; i < this._callbackQueue.length; i++) {
            this.fireCallback(this._callbackQueue[i]);
        }
        // perform the callback if there's one in the arguments
        if (callback != null) this.fireCallback(callback);
    }
    return this;
},


//>	@method	hiddenFrame._draw()	(A)
// Actually draw the hidden frame<br><br>
// Places a handle to the frame in the "_handle" variable
//
// @platformNotes	Very different manner of drawing for IE and Nav,
//				but the effect should be more or less the same
//<
_draw : function () {
    if (this._drawn) return this._windowHandle;

    // specifying the NAME attribute is necessary if we want to be able to target form submits
    // to this frame (targeting the ID attribute doesn't work)
    this.frameHTML = "<IFRAME ID=" + this.getName() +
        " NAME=" + this.getName() +
        " SRC=\"" + (this.location ? this.location : isc.Page.getBlankFrameURL()) +
        "\" STYLE='position:absolute;visibility:hidden;top:-1000px'" +
        "></IFRAME>";

    if (this.useHtmlfile) {
        try {
            // http://cometdaily.com/2007/11/18/ie-activexhtmlfile-transport-part-ii/
            this._transferDoc = new ActiveXObject("htmlfile");
        } catch (e) {
            // ActiveX may be disabled.
            this.useHtmlfile = false;
        }
    }
    if (!this.useHtmlfile) {
        var output = this.frameHTML;
        // add a new IFRAME to the body
        
        isc.Element.createAbsoluteElement(output);

        
        if (!(isc.Browser.isIE && isc.Browser.minorVersion < 5.5)) {
            // get the handle to the frame and the window object
            this._frameDomHandle = isc.Element.get(this.getName());
            if (!isc.Browser.isSafari) this._windowHandle = this._frameDomHandle.contentWindow;
        }

        if (this._windowHandle == null) {
            for (var i = 0; i < window.frames.length; i++) {
                var wh = window.frames[i];
                try {
                    if (wh.name == this.getName()) {
                        this._windowHandle = wh;
                        break;
                    }
                } catch (e) {
                    // The way to handle situations with denied access to frame
                }
            }
        }
        
    }

    // mark as drawn
    this._drawn = true;
    // and return it
    return this._windowHandle;
},


isDrawn : function () {
    return this._drawn;
},


_safeToDraw : function () {
    
    return isc.Page.isLoaded() || !isc.Browser.isMoz;
},


destroy : function () {
    //!OBFUSCATEOK
    // if there's a commCanvas associated with this hidden frame, destroy it.
    if (isc.isA.Canvas(this._commCanvas)) {
        this._commCanvas.destroy();
        delete this._commCanvas;
        isc.HiddenFrame._stats.destroys++;
    }

    // remove global pointer to this object
    if(window[this.ID]) window[this.ID] = null;

    isc.HiddenFrame._hiddenFrames.remove(this);

    // Don't attempt to draw on page load after we've been destroyed.
    if (this._markedForDrawing) {
        isc.Page.clearEvent("load", this._drawEvent);
    }

    if (this.useHtmlfile) {
        if (this._transferDoc) {
            this._transferDoc.parentWindow.isc = null;
            this._transferDoc = null;
            delete this._transferDoc;
            
            CollectGarbage();
        }
    } else {
        var handle = this.getHandle();
        if (!handle) return;
        
        if (isc.Browser.isSafari) handle.location = isc.Page.getBlankFrameURL();

        
        var canAccessWindowProps = this.evaluate("try{this.getHandle().name;true;}catch(e){false}");
        if (!canAccessWindowProps) {
            this.logDebug("Can't dispose of " + this.ID + " - property access denied.");
            return
        }

        
        if (isc.Browser.isIE) {
            if (document.domain == location.hostname) {
                handle.document.open();
                handle.document.write("");
                handle.document.close();
            } else {
                // XXX document.write() and document.close() throw exceptions if we've set
                // document.domain.  Can't be overcome by setting handle.document.domain here
                // manually, so use Safari workaround of targeting the blank url;
                handle.document.location.href = isc.Page.getBlankFrameURL();
            }
        }

        
        if (this._frameDomHandle != null) {
            isc.Element.clear(this._frameDomHandle);
            this._frameDomHandle = null;
        }
        if (this._windowHandle != null) this._windowHandle = null;
    }
    this.Super("destroy", arguments);

    this.logDebug("Destroyed " + this.ID);
},


//>	@method	hiddenFrame.getHandle()	(A)
//			return a handle to the frame
//		@return	(DOMObject)		handle to the frame
//<
getHandle : function () {
	return this._windowHandle;
},


//>	@method	hiddenFrame.getName()	(A)
//			get the name of this frame
//		@return	(string) name for the frame
//<
getName : function () {
	return this.getID();
},


//>	@method	hiddenFrame.getFrameDocument()	(A)
//			return a pointer to the document object of the frame
//		@return	(DOMObject)	pointer to the document object of the hidden frame
//<
getFrameDocument : function () {
    if (this.useHtmlfile) {
        return this._transferDoc;
    } else {
        
        var handle = (isc.Browser.isSafari && isc.Browser.safariVersion < 523.129
                        ? this._frameDomHandle : this._windowHandle);
        
        try {
            return handle.document;
        } catch (e) {
            return null;
        }
    }
},


//>	@method	hiddenFrame.getForm()	(A)
//			return a pointer to a form in the document of the frame
//
//		@param	[formID]	(number | string : 0)		identifier for the form, either a number or form name
//
//		@return	(form)	pointer to the form, or null if form not found
//<
getForm : function (formID) {
	if (formID == null) formID = 0;
    if (isc.Browser.isMoz) {
        return this.getFrameDocument().getElementById(formID);
    } else {
        return this.getFrameDocument().forms[formID];
    }
},


//>	@method	hiddenFrame.setInnerHTML()
//		Function to set the text of the frame to a block of text
//
//		@param	newText		(string)	text to put into the frame
//<
setInnerHTML : function (newText) {
	// remember the text for later
	if (newText) this.text = newText;

    var document = this.getFrameDocument();

    

    if (isc.Browser.isMoz) {
        var range = document.createRange();
        range.setStartBefore(document.body);
        var parsedHTML = range.createContextualFragment(this.text);
        document.body.appendChild(parsedHTML);
    } else {

    // open, write and close document
    
    document.open();
    // NOTE: current code does not write a complete document into the frame, just a FORM,
    // although all browsers seem to be happy with this.
    document.write(this.text);
    document.close();

    }
},




sendForm : function (formHTML, formName, fieldList) {
    if (formHTML != null) this.formHTML = formHTML;
    if (formName != null) this.formName = formName;
    if (fieldList != null) this.fieldList = fieldList;

    if (!this._safeToDraw()) {
        this.logDebug("delaying comm until page load");
        isc.Page.setEvent("load", this.getID()+".sendForm()", isc.Page.FIRE_ONCE);
        return;
    }

    this.draw();

    this.sendData();
},

sendData : function (isResend) {
    // somewhat hackish - we need RPCManager may resend() the form in some cases, and we need
    // to flag this for the server, but the form HTML has already been generated.  So use
    // regexp to hook a known part of the ACTION URL and write our flag in that way
    var formHTML = this.formHTML;
    if (isResend) {
        // note isc_rpc=1 appears more than once in the string
        formHTML = formHTML.replace(/isc_rpc=1/g, "isc_rpc=1&isc_resubmit=1");
    }

    
    if (this.getFrameDocument() == null)
    {
        //this.logWarn("delaying sendData because IFRAME document object isn't available yet");
        this.delayCall("sendData", [], 10);
        this.logDebug(this.getID() + ": sendData() - document not ready - deferring.");
        return;
    }

    var form;

    if (this.useHtmlfile) {
        var transferDoc = this._transferDoc;
        transferDoc.open();
        transferDoc.write("<html><body>");
        transferDoc.write(this.frameHTML);
        transferDoc.write(formHTML);
        transferDoc.write("</body></html>");
        transferDoc.close();
        transferDoc.parentWindow.isc = isc;

        form = transferDoc.getElementById(this.formName);
    } else {
        // In IE and Moz we write the form into a canvas and set its target to the ID of the iframe
        // managed by this hiddenFrame object.
        if (isc.isA.Canvas(this._commCanvas)) this._commCanvas.destroy();
        this._commCanvas = isc.Canvas.create({
            autoDraw: true,
            ID: this.getID() + "_commCanvas",
            visibility: "hidden",
            overflow:"ignore",
            top: -9999,
            width: 1,
            height: 1,
            contents: formHTML,
            _generated: true,
            // we don't want this canvas to show up in the log - so ignore stats for it
            _iscInternal: true
        });
        isc.HiddenFrame._stats.draws++;

        form = isc.Element.get(this.formName);
    }

    // Set the form element's `accept-charset' attribute if not set
    // https://developer.mozilla.org/en-US/docs/DOM/form.acceptCharset
    if (!form.acceptCharset) {
        form.acceptCharset = "UTF-8";
    }

    

    // assign the form values to the fields explicitly.
    
    for (var field in this.fieldList) {
        // need the null check here because otherwise we actually send an undefined
        if(this.fieldList[field] != null) form.elements[field].value = this.fieldList[field];
    }

    if (form) {
        form.submit();

        //>DEBUG
        this.logInfo("Form submitted to server");
        //<DEBUG

		// if a callback was set, start the timer for it
		if (this.callback) {
			this.convertToMethod("callback");
			this.delayCall("checkLoaded", [], this.pollInterval);
		}
    } else {
        // should never happend
        this.logWarn("couldn't get handle to comm form");
    }
},

// function for doing the form submit on a timeout.  Not currently used.
/*
submitForm : function () {
    var form = this.getForm(this.formName);
    form.submit();
},
*/

//> @method hiddenFrame.checkLoaded()
//<

checkLoaded : function () {
    var handle = this.getHandle();

    // see if the variable we're expecting has arrived in the iframe
    if (handle && handle[this.resultVarName]) {
		if (this.callback) this.callback(handle);
    } else {
        // var not yet available - wait for the server to respond
		this.delayCall("checkLoaded", [], this.pollInterval);
	}
}

}); // end addMethods()

// Register instance methods that can be defined as strings to eval
isc.HiddenFrame.registerStringMethods({
    callback:"frame"
});

//!<Deferred
