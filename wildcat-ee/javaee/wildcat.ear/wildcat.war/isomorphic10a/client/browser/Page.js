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

 








//>	@class	Page
//
// 	Provides information about the page you're loaded in.  We define "page" here to be roughly
// 	equivalent to the browser window or frame the libraries have been loaded in.
//
//  @treeLocation Client Reference/System
//  @visibility external
//<



//
//	Create the Page singleton object
//
isc.ClassFactory.defineClass("Page");


// define groups for documentation purposes
isc.Page.addClassProperties({
	
    _historyActions : [],

    // flag for whether page.onload has fired yet
	_doneLoading : false,
    
    //>	@classAttr	isc.Page.defaultUnsupportedBrowserURL   (URL : "[SKIN]/unsupported_browser.html" : IRWA)
	//      On a call to <code>Page.checkBrowserAndRedirect()</code>, if no explicit URL
    //      is passed in, and the browser is not supported by ISC, redirect to this URL.
	//
	// @group	files
    // @see Page.checkBrowserAndRedirect()
    // @visibility external
	//<
    //  Note all the default load_skin.js files will call this method and pass in an
    //  explicit URL for a page to redirect to.  This default is unlikely to be modified as 
    //  it's easier to just modify the explicit URL passed in when the method is called.
    defaultUnsupportedBrowserURL : "[SKIN]/unsupported_browser.html",
	
    //>	@classAttr	isc.Page._directoryCache	(array : [] : IRW)
	// 		URLs in our framework can have keywords embedded in them as 
	//		[SKIN] or [ISOMORPHIC], etc.  This is where we store the expanded directory names.
	//		Use Page.getURL() to figure that out.
	//
	//		@group	files
	// 		@see Page.setDirectories()
	//<
	_directoryCache : {},								

    //> @classAttr Page.protocolURLs (Array of String : [...] : IRW)
    // If a URL provided to various Page APIs begins with one of these Strings, it is treated
    // as an absolute URL.
    // <P>
    // The default of protocols is:
    // <pre>
    //     ["http://","https://","file://","mailto:", "app-resource:", "data:"]
    // </pre>
    // .. and can be replaced via +link{class.addClassProperties,Page.addClassProperties()} or
    // via setting the global variable isc_protocolURLs before SmartClient loads.
    //
    // @group files
    // @see Page.getURL()
    // @visibility external
    //<
    // "app-resource:" used by apollo
    // "data:" allows base64 encoded images to be specified directly, in recent browsers
	protocolURLs : window.isc_protocolURLs || ["http://","https://","file://","mailto:", "app-resource:", "data:"],	

	//>	@classAttr	isc.Page.textDirection	(TextDirection : (null) : IRW)
	//		What direction is text supposed to run?  
	//			LTR (left to right, eg: English) or RTL (right to left, eg: Arabic)
	//		@group	textDirection
	//		@platformNotes	IE only
	//<
	textDirection:null, // don't remove: initalized to null so we will look up the value set in
                        // the body tag if it hasn't been set in our framework.
    
    //> @classAttr   isc.Page.pollPageSize (boolean : null : IRWA)
    // Advanced attribute which will cause SmartClient to constantly check for the introduction
    // of scrollbars due to resizing of native content by direct DOM manipulation, and
    // automatically adjust percent sized widgets if external code causes the browser window to
    // be resized.
    //<
    // Polling for changes in page size will also catch orientation changes. We use this
    // in MobileWebKit browsers
    

	//>	@type	Page.TextDirection
	// Specifies RTL or LTR direction for text -- IE5+ and FF1.5+ only
	LTR:"ltr",									 		//	@value	isc.Page.LTR		Show text left-to-right (eg: English)
	RTL:"rtl",											//	@value	isc.Page.RTL		Show text right-to-left (eg: Arabic)
	//			@group	appearance
	//<

    //> @classAttr   isc.Page._addVersionToSkinCSS (boolean : false : IRWA)
    // Advanced attribute which will cause +link{classMethod:Page.loadStyleSheet()} to append
    // an "isc_version" parameter to the end of the url when loading a stylesheet.
    //
    // @group skins, files
    // @see Page.setAddVersionToSkinCSS()
    // @see Page.getAddVersionToSkinCSS()
    //<
    _addVersionToSkinCSS: false
});


isc.Page.addClassMethods({

//>	@classMethod	Page.finishedLoading()	(A)
// Routine called automatically by the EventHandler when the page finishes loading.
//		@group	eventHandling
//<
finishedLoading : function () {

	isc.Page._doneLoading = true;
    isc.Log.logInfo("isc.Page is loaded");

	// kick off the idle timer when the page starts
    isc.EH.startIdleTimer();

    
    if (isc.Browser.isSafari) isc.Canvas.clearCSSCaches();
    
    // Open the log window if it should be open
    if (!window.suppressAutoLogWindow) {
        var cookie = isc.LogViewer.getLogCookie();
        if (cookie != null && cookie.keepOpen) {
            // NOTE: wait until any existing log window has had time to reconnect before
            // auto-opening the log window
            isc.Timer.setTimeout("isc.Log.show(true)", 1000);
        }
    }
    
    
    // If deprecated UTCHoursOffset has been set for the page, respect it (but log a warning)
    
    if (isc.Time && isc.Time.UTCHoursOffset != null) {
        isc.logWarn("This application includes code to set the Time.UTCHoursOffset attribute. " +
            "This property will be respected but has been deprecated in favor of the " +
            "classMethod isc.Time.setDefaultDisplayTimezone().");
        // respect it anyway
        isc.Time.setDefaultDisplayTimezone(isc.Time.UTCHoursOffset.stringify() + ":00");
    }
    
    // If we're polling for page size changes, kick this off now.
    if (isc.Page.pollPageSize) {
        isc.EH._pageResize();
    
    // Otherwise we've seen a case in some browsers where the scroll size is not calculated
    // correctly on initial draw from clean cache - force a single pageResize event on
    // a delay to fix this if necessary
    
    } else {
        isc.EH.delayCall("_pageResize", [true],  200);
    }

    
    if (isc.Browser.isIE) { isc.Class.evaluate("1", null, false, true); }
},

//>	@classMethod	Page.isLoaded()
//		Has the page finished loading?
//
//		@return	(Boolean)		true == page is done loading
// @visibility external
//<
isLoaded : function () { return this._doneLoading },

// return the URL that should be used for an IFRAME that should be blank
getBlankFrameURL : function () {
    
    
    
    if (isc.Browser.isIE && ("https:" == window.location.protocol || document.domain != location.hostname )) {
        // In IE under HTTPS, using "about:blank" as the location for an IFRAME causes a bogus "Mix
        // of secure and insecure content" dialog, so instead fetch an empty html file.
        return this.getURL("[HELPERS]empty.html");
    }
    // known to work in IE, Moz, Safari
    return "about:blank";
},


//>	@classMethod	Page.setTitle()
//		Set the title of the page, which is typically shown as part of the browser window title
// @visibility external
//<
// Doesn't actually update the browser window title in IE.
setTitle : function (title) {
    document.title = title;
},

//>	@classMethod	Page.setDirectories()
//		Set any and all of the directories that the page keeps track of in a single call.
//
//		@param	[directories]	(object)	Object of {directory:URL} paths.
//								If not specified, we will try to get directories specified in the window objecs.
//		@group	files, images
//<
setDirectories : function (directories) {
	if (directories == null) {
		directories = {
			imgDir:window.imgDir,
			isomorphicDir:(window.isomorphicDir ? window.isomorphicDir : window.IsomorphicDir),
			isomorphicClientDir:window.isomorphicClientDir,
			isomorphicDocsDir:window.isomorphicDocsDir,
			skinDir:window.skinDir,
			helperDir:window.helperDir
		}
	}
	
	this._deriveAppDir();

	this.setIsomorphicDir(directories.isomorphicDir);
	this.setIsomorphicClientDir(directories.isomorphicClientDir);
	this.setIsomorphicDocsDir(directories.isomorphicDocsDir);
	this.setAppImgDir(directories.imgDir);
	this.setSkinDir(directories.skinDir);
	this.setHelperDir(directories.helperDir);
},


// derive the base URL of the application.  
_deriveAppDir : function () {
	
	// get the path to the current file and strip off any query params and leaf file names
	var filePath = window.location.href;
	// strip off anything after a "?"
	if (filePath.contains("?")) filePath = filePath.substring(0,filePath.indexOf("?"));
    // # references node IDs which, according to the W3C cannot have slashes in them, but in
    // the AJAX world, # refs are often used to provide back button support rather than
    // actually reference any node ids in the DOM, so it's best that we don't break if # refs
    // contain slashes in the value.
	if (filePath.contains("#")) filePath = filePath.substring(0,filePath.indexOf("#"));
	// strip off the leaf file name if one exists
	if (filePath.charAt(filePath.length-1) != "/") {
		filePath = filePath.substring(0, filePath.lastIndexOf("/") + 1);
	}
	
	this._directoryCache.APP = filePath;

	//>DEBUG
    if (this.logIsInfoEnabled()) {
    	this.logInfo("app dir is " + this._directoryCache.APP);
    }
	//<DEBUG	

	// call getAppImgDir() so it will change based on the changed app dir...
	this.setAppImgDir();
},

//>	@classMethod	Page.getAppDir()
// Returns the base URL of the application, which is the page URL minus the last non-directory 
// path component.  For example, if the page is loaded from
// <code>http://foo.com/bar/zoo.jsp</code>, appDir will be <code>http://foo.com/bar/</code>.
// <P>
// If other page-wide URLs such as +link{Page.setIsomorphicDir()} are specified as
// relative paths, they are considered relative to this URL.
//
//		@return	(string)	URL for page-specific files.
//		@group	files
// @visibility external
//<
getAppDir : function () {
	return this._directoryCache.APP;
},

//	Application-specific resource directories
// ---------------------------------------------------------------------------------------

//>	@classMethod	Page.setAppImgDir()
// Specify the directory for app-specific images.
// <P>
// This becomes the default location where any SmartClient component will load images from
// unless the special "[SKIN]" prefix is used to indicate that an image is part of a skin.
// <P>
// Default is "[APP]images/"
//
//		@param	[URL]		(string)	New imgDir URL.
//		@group	files, images
// @visibility external
//<
// NOTE: Caches the combined appDir + imgDir.
setAppImgDir : function (URL) {
    // If the URL passed in is not absolute, explicitly combine it with the app dir
    // This means if we generate Img HTML and end up showing it in another frame it'll still
    // pick up the correct image (required for EG printing support)
    this._directoryCache.APPIMG = 
            this.combineURLs(this.getAppDir(), URL != null ? URL : "[APP]images/");
},

//>	@classMethod	Page.getAppImgDir()
// Return the directory for app-specific images.
//
//		@return	(string)	URL for page-specific images.
//		@group	files, images
// @visibility external
//<
getAppImgDir : function (imgDir) {
    if (imgDir != null) {
        // expand any [SKIN], [SKINIMG], or similar prefix to the associated dir
        if (isc.startsWith(imgDir, this._$leftBracket)) imgDir = this.getURL(imgDir);
        // specifically check for an imgDir that has been specified as an absolute path.
        if (isc.startsWith(imgDir, isc.slash) || this.getProtocol(imgDir) != isc.emptyString) {
            return imgDir;
        }
        return this._directoryCache.APPIMG + imgDir;
    }
    return this._directoryCache.APPIMG; 
},

//>	@classMethod	Page.setAppFilesDir()
// Specify the directory for miscellaneous app-specific files <b>other than</b> images, such as
// +link{HTMLFlow.contentsURL,HTML fragments}, +link{ViewLoader,loadable views}, 
// XML or JSON flat data files, videos, etc.
// <P>
// This URL also becomes available via the prefix "[APPFILES]" for +link{rpcRequest.actionURL}.
// <P>
// Defaults to the value of +link{Page.getAppDir()}, that is, the current directory.
//
//		@param	[URL]		(string)	New app files URL.
//		@group	files, images
// @visibility external
//<
// NOTE: Caches the combined appDir + imgDir.
setAppFilesDir : function (URL) {
	this._directoryCache.APPFILES = this.combineURLs(this.getAppDir(), URL);
},

//>	@classMethod	Page.getAppFilesDir()
// Returns the directory for application-specific files (other than images).
//
//		@param	[URL]		(string)	New app files URL.
//		@group	files, images
// @visibility external
//<
getAppFilesDir : function (URL) {
	return this._directoryCache.APPFILES;
},

//	Isomorphic-supplied file locations
// ---------------------------------------------------------------------------------------

//>	@classMethod	Page.setIsomorphicDir()
//		Specify the root directory for Isomorphic-supplied files.
//
//		@param	[URL]		(string)	New IsomorphicDir URL.
//		@group	files
// @visibility external
//<
setIsomorphicDir : function (URL) {
	this._directoryCache.ISOMORPHIC = 
            this.combineURLs(this.getAppDir(), URL != null ? URL : "../isomorphic/");

	// call setSkinDir() and setHelperDir() to reset those cached values
	this.setIsomorphicClientDir();
	this.setIsomorphicDocsDir();
},

//>	@classMethod	Page.getIsomorphicDir()
//		Return the root directory for Isomorphic-specific files.
//
//		@return	(string)	IsomorphicDir URL.
//		@group	files
// @visibility external
//<
getIsomorphicDir : function () {
	return this._directoryCache.ISOMORPHIC;
},

// Note skins groupDef is in Canvas.js

//>	@classMethod	Page.setSkinDir()
//		Specify the URL for media that's part of the skin
//
//		@param	[URL]		(string)	New skinDir URL
//		@group	skins, files, images
// @visibility external
//<
setSkinDir : function (URL) {
	this._directoryCache.SKIN = 
            this.combineURLs(this.getAppDir(), URL != null ? URL : "[ISOMORPHIC]/skins/standard/");
	// remember the skin image directory
	this._directoryCache.SKINIMG = this._directoryCache.SKIN + "images/";
    
    if (isc.Canvas) isc.Canvas._blankTemplate = isc.Canvas._blankURL = null;
},

//>	@classMethod	Page.getSkinDir()
//		Return the directory for media that's part of the skin
//
//		@return	(string)	base URL for skin media
//		@group	files, images
// @visibility external
//<
getSkinDir : function () {
	return this._directoryCache.SKIN;
},


//>	@classMethod	Page.getSkinImgDir()
//		Return the directory for a skin image.
//
//		@param	[imgDir]	(URL)		Partial URL (relative to Page._skinDir) where the image lives.
//										If not supplied, will use "images/"
//		@return				(string)	URL for page-specific images.
//		@group	files, images
// @visibility external
//<
getSkinImgDir : function (imgDir) {
	if (imgDir == null) return this._directoryCache.SKINIMG;
	return this.combineURLs(this._directoryCache.SKIN, imgDir);
},

// Internal directory structures
// ---------------------------------------------------------------------------------------
// Applications need to tell us the relative path to the "isomorphic/" directory and may
// relocate the skin outside of the "isomorphic/" area, however, none of the rest of
// the structure under "isomorphic/" is really intended to be changed.  The most likely reason
// to actually do so might involve needing to move the helpers dir to password protect the log
// window.

//>	@classMethod	Page.setIsomorphicClientDir()
//		Specify the root directory for Isomorphic client files.
//
//		@param	[URL]		(string)	New URL for root of client files.
//		@group	files
//<
// NOTE: not visible: we don't actually want customers to relocate the client dir
setIsomorphicClientDir : function (URL) {
	this._directoryCache.ISOMORPHIC_CLIENT = 
            this.combineURLs(this.getAppDir(), URL != null ? URL : "[ISOMORPHIC]/system/");

	// call setSkinDir() and setHelperDir() to reset those cached values
	this.setSkinDir();
	this.setHelperDir();
},

//>	@classMethod	Page.getIsomorphicClientDir()
//		Return the root directory for Isomorphic client files.
//
//		@return	(string)	URL for root of client files.
//		@group	files
//<
getIsomorphicClientDir : function () {
	return this._directoryCache.ISOMORPHIC_CLIENT;
},

//>	@classMethod	Page.setIsomorphicDocsDir()
//		Specify the root directory for Isomorphic documentation and example files.
//
//		@param	[URL]		(string)	New URL for root of documentation and example files.
//		@group	files
//<
// NOTE: not visible: we don't actually want customers to restructure the SDK
setIsomorphicDocsDir : function (URL) {
	this._directoryCache.ISOMORPHIC_DOCS = 
            this.combineURLs(this.getAppDir(), URL != null ? URL : "[ISOMORPHIC]/system/reference/");
	this.setIsomorphicDocsSkinDir();
},

//>	@classMethod	Page.getIsomorphicDocsDir()
//		Return the root directory for Isomorphic documentation and example files.
//
//		@return	(string)	URL for root of documentation and example files.
//		@group	files
//<
getIsomorphicDocsDir : function () {
	return this._directoryCache.ISOMORPHIC_DOCS;
},

setIsomorphicDocsSkinDir : function (URL) {
	this._directoryCache.ISO_DOCS_SKIN = 
           this.combineURLs(this.getIsomorphicDocsDir(), URL != null ? URL : "skin/");
},
getIsomorphicDocsSkinDir : function () {
	return this._directoryCache.ISO_DOCS_SKIN;
},



//>	@classMethod	Page.setHelperDir()
//		Specify the directory for Isomorphic-specific helper files.
//
//		@param	[URL]		(string)	New helperDir URL.
//		@group	files, images
//<
// NOTE: not visible: we don't actually want customers to relocate the helpers dir
setHelperDir : function (URL) {
	this._directoryCache.HELPERS = 
            this.combineURLs(this.getAppDir(), URL != null ? URL : "[ISOMORPHIC_CLIENT]/helpers/");
},

//>	@classMethod	Page.getHelperDir()
//		Return the directory for Isomorphic supplied helper files.
//
//		@return	(string)	URL for Isomorphic supplied helper files.
//		@group	files, images
//<
getHelperDir : function () {
	return isc.Page._directoryCache.HELPERS;
},

// ---------------------------------------------------------------------------------------

_isRelativeURL : function (url) {
    return this.getProtocol(url) == isc._emptyString && !isc.startsWith(url, isc.slash);
},

//>	@classMethod	Page.getImgURL()
// Return the full URL for app-specific or skin image.
// <P>
// To use a skin image, start the URL with "[SKIN]".  Any other relative URL is assumed
// relative to the +link{Page.getAppImgDir(),appImgDir}.
//
//		@param	src			(SCImgURL)	Local file name for the image.
//		@param	[imgDir]	(string)	User-specified image directory,
//										local to 
//		@return				(string)	URL for the image.
//		@group	files, images
// @visibility external
//<

_skinPrefix : "[SKIN]",
_skinSlashPrefix : "[SKIN]/",
getImgURL : function (src, imgDir, assumeRelativeSrc) {
    

	// get the full URL for an image
    var baseDir;
	if (isc.startsWith(src, this._skinPrefix)) {
        baseDir = isc.Page.getSkinImgDir(imgDir);
		// NOTE: account for "[SKIN]/" as well; ignore the slash
        var trim = isc.startsWith(src, this._skinSlashPrefix) ? 7 : 6;
        src = src.substring(trim);
    } else {
        baseDir = assumeRelativeSrc ? null : isc.Page.getAppImgDir(imgDir);
    }
	return isc.Page.combineURLs(baseDir, src);
},

_$leftBracket : "[",
_$dotSlash : "./",

//>	@classMethod	Page.getURL()
// Return a full URL for a relative path that uses a special prefix such as "[APPFILES]" or
// "[SKIN]".
// <P>
// For images, use +link{Page.getImgURL()} instead.
//
//		@param	fileName	(string)	Local file name for the image.
//		@return				(string)	URL for the image.
//		@group	files, images
// @visibility external
//<
getURL : function (URL) {
    

    // check for a special prefix
	if (isc.startsWith(URL, this._$leftBracket)) {
		var endIndex = URL.indexOf("]");
		if (endIndex > 0) {
			var directoryName = URL.substring(1,endIndex).toUpperCase(),
				cachedDirectory = isc.Page._directoryCache[directoryName];
            // substitute if we hit a known prefix
			if (cachedDirectory != null) {
				URL = isc.Page.combineURLs(cachedDirectory, 
                                           URL.substring(endIndex+(URL.charAt(endIndex+1)!="/"?1:2)));
			//>DEBUG
			} else {
				this.logDebug("getURL("+URL+"): couldn't find cached directory " + directoryName);
			//<DEBUG
			}
		//>DEBUG
		} else {
			this.logDebug("getURL("+URL+"): didn't find matching ']' in URL");
		//<DEBUG
		}
	}
	return URL;
},

//>	@classMethod	Page.combineURLs()
//		Combine a "masterURL" and a "localURL" into a single URL.
//		If the localURL is a fully specified URL (starts with "http:", "https:" or "file:"),
//			we use that.
//
//		If the localURL is a relative URL, combine with the masterURL
//		to a single URL. 
//
//		@param	masterURL	(string)	Master URL.
//		@param	localURL	(string)	Local URL.
//		@return				(string)	Combined URL.
//		@group	files
//<
_$dotdot:"..",
combineURLs : function (masterURL, localURL) {
    
	if (!isc.isA.String(localURL)) return masterURL;

    

	if (isc.startsWith(localURL, this._$leftBracket)) {
        // URL appears to contain a special directory name
		return this.getURL(localURL);
	}

    var ns = isc._emptyString;

	// if local directory was specified as a full URL, simply return that
	if (masterURL == null || masterURL == ns || isc.Page.getProtocol(localURL) != ns) {
		return localURL;
	}
    
    var slash = isc.slash;
	
	
    
	var masterProtocol = isc.Page.getProtocol(masterURL);

    if (isc.startsWith(localURL, slash)) {
        

        // localURL is absolute; combine with just the protocol/hostName from masterURL 

        //if (isc.Log) isc.Log.logWarn("absolute local URL: " + localURL + 
        //                             ", base master is: " + masterURL +
        //                             ", masterProtocol: " + masterProtocol);

        if (isc.isAn.emptyString(masterProtocol)) {
            // if master URL has no protocol and hence no host, empty it so that we we will the
            // already absolute "localURL" unchanged
            masterURL = isc.emptyString;
        } else if (masterURL.indexOf(slash, masterProtocol.length) != -1) {
            // We want to chop off everything in the master URL after the first "/"
		    masterURL = masterURL.substring(0, masterURL.indexOf(slash, masterProtocol.length));
        }
            
    // eliminate any "./" entries in the localURL
	// go up a directory in the masterURL for any "../" in the localURL        
    } else 	if (localURL.indexOf(this._$dotSlash) > -1) {
        
        //alert("backups in local URL: " + localURL);
    
		// break up masterURL into protocol and directories
		// break up localURL into directories
		masterURL = masterURL.substring(masterProtocol.length, masterURL.length-1);
		var masterDirs = masterURL.split(slash),
			localDirs = localURL.split(slash)
		;
        // the first "dir" is actually the host
        var masterHost = masterDirs[0];
		masterDirs.shift();

		while (localDirs[0] == isc.dot || localDirs[0] == this._$dotdot) {
			// if ".", just skip it in the localDirs
			if (localDirs[0] == isc.dot) {
				localDirs.shift(); // take "." off front
				continue;
			}
	
			// otherwise it's ".." -- take ".." off front of local dir
			localDirs.shift();
            // and go up one directory in the master dir (if possible)
			if (masterDirs.length == 0) break;
			masterDirs.pop(); // take last segment off master dir
		}

		masterURL = masterProtocol + masterHost + slash;
        if (masterDirs.length > 0) masterURL += masterDirs.join(slash) + slash;
		localURL = localDirs.join(slash);
	}
	//return the combined URLs
	return masterURL + localURL;
},

//>	@classMethod	Page.getProtocol()
//		Return the protocol for a given URL.
//		Returns the full protocol (eg: "http://"), or
//		the empty string ("") if protocol was not understood.
//		@param	URL		(URL)		URL to get protocol for.
//		@return			(string)	Protocol for the URL, or "" if not found/recognized.
//		@group	files
//		@see	Page._protocolsURLs
//<
getProtocol : function (URL) {
    
	for (var i = 0; i < isc.Page.protocolURLs.length; i++) {
		if (isc.startsWith(URL, isc.Page.protocolURLs[i])) return isc.Page.protocolURLs[i];
	}
	return isc._emptyString;
},

getLastSegment : function (url) {
    if (url == null) return isc.emptyString;
    var slashIndex = url.lastIndexOf(isc.slash);
    if (slashIndex == -1) return url;
    return url.substring(slashIndex+1);
},

// XHTML
// ---------------------------------------------------------------------------------------

 
isXHTML : function () {
    if (this._isXHTML != null) return this._isXHTML;
    if (isc.Browser.isIE) return false;
    var wd = this.getWindow();
    return (this._isXHTML = (this.getDocument().constructor == this.getWindow().XMLDocument));
},


//	Text direction
// ---------------------------------------------------------------------------------------

//> @classMethod Page.isRTL()
//
// Return whether the page text direction is right to left.  If you set "DIR=RTL" in the BODY tag of
// the page, then this method will return true.  If you set "DIR=LTR" then this method will return
// false.
//
// @return (Boolean) true if Page text direction is RTL, false otherwise
// @group RTL
// @visibility external
//<

isRTL : function () { return this.getTextDirection() == isc.Canvas.RTL },

//>	@classMethod	Page.getTextDirection()
//		@group	textDirection
//		Return the text direction of the page for right-to-left 
//		language support.  Returned value will be:
//				* Page.LTR (left to right, eg: English), or
//				* Page.RTL (right to left, eg: Arabic) 
//<
_$body:"body", _$html:"html", _$direction:"direction",
getTextDirection : function () {
    // if the textDirection of the page has never been set, 
    if (this.textDirection == null) {
        var strDirection;
        var htmlTag = document.getElementsByTagName(this._$body)[0] || 
                      document.getElementsByTagName(this._$html)[0]
    
        if(document.defaultView && document.defaultView.getComputedStyle){
            var computedStyle = document.defaultView.getComputedStyle(
                            htmlTag, isc.emptyString);
            strDirection = computedStyle ? 
                            computedStyle.getPropertyValue(this._$direction) : null;
        } else if (htmlTag.currentStyle){
            strDirection = htmlTag.currentStyle[this._$direction];
        }
        
        // Didn't have the APIs to get a value or it returned null - back off to looking 
        // directly at the "dir" property on the body element
        
        var dontCache;
        if (strDirection == null) {
            dontCache = true;
            
            var docElement = document.documentElement,
                body = document.body;
                
            // check body before document element, since it overrides.  (NOTE: empty string
            // is false)
            strDirection = (body ? body.dir : null) || docElement.dir;
        }
        if (strDirection == null || strDirection == isc.emptyString) {
            strDirection = this.LTR;
        } else {
            strDirection = strDirection.toLowerCase();
        }
        // don't save direction as LTR unless the body has been created, in case
        // getTextDireciton() is called in an incomplete document    
        if (!dontCache && document.body != null) this.textDirection = strDirection;
        return strDirection;
    }
  
    // return the direction stored in the page object
    return this.textDirection;
},




//
//	Dynamic loading/writing of various page structures for you
//


//>	@classMethod	Page.loadStyleSheet()
//		Load a styleSheet for this application.  
//
//		The styleSheetURL parameter can use any special directories, eg:<br>
//      &nbsp;&nbsp;<code>Page.loadStylesheet("[SKIN]/skin_styles.css")</code><br>
//      or<br>
//      &nbsp;&nbsp;<code>Page.loadStylesheet("[APP]/app_styles.css")</code>.
//      <P>
//		If you don't specify a special directory, the app directory
//			will be assumed.
//		<P>
//		Note:   If the document's ONLOAD handler has already fired, this
//				will have no effect.
//
//		@param	styleSheetURL	(URL)	URL to the stylesheet.  
//										
//		@group	skins, files, images
//  @visibility external
//<
loadStyleSheet : function (styleSheetURL, wd, callback) {
    var url = isc.Page.getURL(styleSheetURL);

    // If Page.addVersionToSkinCSS has been set to true, lets append an isc_version parameter to
    // the url for caching/cache-busting.
    if (this._addVersionToSkinCSS) {
        var uriBuilder = isc.URIBuilder.create(url);
        uriBuilder.setQueryParam("isc_version", isc.versionNumber);
        url = uriBuilder.uri;
    }

    var html = "<link rel='stylesheet' type='text/css' href=\"" + url + "\"\/>";
    if (wd == null) wd = window;
	if (isc.Page.isLoaded() && wd == window) {
        if (isc.FileLoader) {
            // The FileLoader preemptively loads the css that load_skin.js loads via a call to
            // loadStyleSheet and stores a marker for us
            var loadedSkins = isc.FileLoader._loadedSkins;

            if (loadedSkins != null) {
                for (var i = 0; i < loadedSkins.length; i++) {
                    if (url.indexOf(loadedSkins[i]) != -1) {
                        this.logDebug("skin "+loadedSkins[i]
                                     +" already loaded by FileLoader - not loading css file");
                        return;
                    }
                }
            }

            isc.FileLoader.loadCSSFile(url, callback);
        } else {
    		//>DEBUG
	    	this.logWarn("isc.Page.loadStylesheet('"+styleSheetURL+
                         "') called after page load.  Stylesheet not loaded.");
    		//<DEBUG
        }
	} else {
        if (this.isXHTML()) {
            // XHTML: no document.write(), and body element may not exist yet.
            // Per XHTML spec <link> elements appear in <head>, so dynamically add
            // to <head> regardless of whether body exists
            var doc = this.getDocument(),
                head = doc.documentElement.firstChild,
                // NOTE: namespace is required or you create a valid <link> element that does
                // nothing because it's not treated as an HTMLLinkElement
                elem = doc.createElementNS(doc.documentElement.namespaceURI, "link");
            elem.rel = "stylesheet";
            elem.type = "text/css";
            elem.href = url;
            
            head.appendChild(elem);
            this.logWarn("added stylesheet DOM style");
        } else {
            wd.document.write(html);
        }
	}
},


//>	@classMethod	Page.resizeTo()
//		Resize the outer portion of the window to a specific width and height.
//		@group	sizing
//
//		@param	width	(number)	new width for the window
//		@param	height	(number)	new height for the window
// @visibility external
//<
resizeTo : function (width, height) {
	window.resizeTo(width, height);
},

//>	@classMethod	Page.moveTo()
//		Move the window to a specified top and left in screen coordinates.
//
//		@param	left	(number)	new left coordinate for window
//		@param	top		(number)	new top coordinate for window
// @visibility external
//<
moveTo : function (top, left) {
	window.moveTo(top, left);
},

//>	@classMethod	Page.scrollTo()
//		Scroll the window to a specified top and left coordinate.
//
//		@param	left	(number)	new left coordinate for window
//		@param	top		(number)	new top coordinate for window
// @visibility external
//<
scrollTo : function (top, left) {
	window.scroll(top, left);
},

//>	@classMethod	Page.getWidth()
// Get the width of the visible portion of the window, not including browser chrome or the
// scrollbar area.
// <P>
// See also +link{Page.getOrientation()}.
//		@group	sizing
//
//		@param	[wd]		(object)	the window object
//
//		@return	(number)	width of the page
// @visibility external
//<
leaveScrollbarGap : isc.Browser.isMoz && isc.Browser.geckoVersion < 20051107,

getWidth : (isc.Browser.isNS ? 
    // isNS: Netscape browsers (inc.Moz, Firefox) and also Safari
    
	function (wd, recalculate) {

		if (!wd) wd = window;
        
        
        recalculate = recalculate || (isc.Browser.isMoz && isc.Browser.geckoVersion >= 20080529);
        if (isc.Browser.isMoz && wd == window && !recalculate) {

            
            if (this.width != null) {
                return this.width - (this.leaveScrollbarGap ? 
                                      
                                      (isc.Element ? isc.Element.getNativeScrollbarSize() : 16) : 
                                      0
                                    );
            }
            
            // If we were unable to get the page width, return an arbitrary value (500)
            // this is probably due to the page not being fully loaded.
            // This method is commonly called in order to size percentage-sized widgets 
            // correctly - therefore fire a page-resized event on load in this case 
            // to fix any incorrectly sized percentage width widgets.

            //>DEBUG
            // Warn this at the 'info' level - Not much a developer can do about this, but it
            // can be valuable for us to see the issue.
			this.logInfo("NOTE: isc.Page.getWidth() unable to determine page width.  Returning 500", 
                         "sizing");
			//<DEBUG
            return 500;

        } else {
            // If the body has been written out, use body.clientWidth to ensure we get the
            // size inside any scrollbars
            
            var isFFPre15 = isc.Browser.geckoVersion != null && isc.Browser.geckoVersion < 20051111;
            var useClientWidth = !isc.Browser.isAndroid && !this.leaveScrollbarGap && !isFFPre15 && wd.document.body != null,
                width;

            if (useClientWidth) {
                
                var documentBody = isc.Browser.isStrict 
                                    ? wd.document.documentElement : wd.document.body;
                if (documentBody != null) width = documentBody.clientWidth;
            }

            // Catch the case where we didn't pick up a width from the body
            
            if (width == null || width == 0) {
                
                width = wd.innerWidth;
            }
            if (wd == window) this.width = width;
    		return width;
        }
	}
:// isc.Browser.isIE || isc.Browser.isOpera
	function (wd) {
		if (!wd) wd = window;
        var documentBody = wd.document.body;
        if (isc.Browser.isStrict && (!isc.Browser.isOpera || isc.Browser.minorVersion >= 9.5))
            documentBody = wd.document.documentElement;
        
		if (documentBody) {
            // NOTE: MacIE will show scrollbars if you draw within a (native) scrollbar width of
            // the edge of the page, so the available space is less than clientWidth.
			return documentBody.clientWidth        
		} else {
        
            // As in the Moz case, if we were unable to determine the page width, fire a 
            // page-resized event on load to fix any incorrectly sized percentage width
            // widgets.
            if (!isc.Page.isLoaded()) {
                isc.Page.setEvent("load", "isc.EH._pageResize()", isc.Page.FIRE_ONCE);
            }
			//>DEBUG
			this.logWarn("NOTE: isc.Page.getWidth() called before <BODY> tag was written out -- " +
                         "value cannot be determined.  Returning 500");
			//<DEBUG
			return 500;	
		}
	}		
),

// Does this page have body overflow explicitly set to hidden (suppresses scrollbars)?

getBodyOverflow : function () {
    if (this._bodyOverflow == null) {
        var overflow;
        var documentBody = document.body;
        if (documentBody && documentBody.style) {
            overflow = documentBody.style.overflow;
        }
        
        this._bodyOverflow = (overflow == null || overflow == "") ? "auto" : overflow.toLowerCase();
    }
    
    return this._bodyOverflow;
},


//>	@classMethod	Page.getHeight()
// Get the height of the visible portion of the window, not including browser chrome or the
// scrollbar area.
// <P>
// See also +link{Page.getOrientation()}.
//		@group	sizing
//
//		@param	[wd]		(object)	the window object
//
//		@return	(number)	height of the page
// @visibility external
//<
getHeight : (isc.Browser.isNS ? 
	function (wd, recalculate) {
		if (!wd) wd = window;
        // see comments in getWidth()
        recalculate = recalculate || (isc.Browser.isMoz && isc.Browser.geckoVersion >= 20080529);
        if (isc.Browser.isMoz && wd == window && !recalculate) {
            
            if (this.height != null) return this.height;
            return 500;
        } else {        
            // If the body has been written out, use body.clientWidth to ensure we get the
            // size inside any scrollbars
            
            

            
            var useClientHeight = !isc.Browser.isAndroid && isc.Browser.isMobileWebkit ||
                                  (!isc.Browser.isStrict && isc.Browser.geckoVersion >= 20051111) 
                                   && wd.document.body != null,
                height;
            if (useClientHeight) {
                height = (isc.Browser.isStrict ? wd.document.documentElement : wd.document.body).clientHeight;
            }
            if (height == null || height == 0) {
                
                height = wd.innerHeight;
            }

            
            if (isc.Browser.isIPad && isc.Browser.isMobileSafari && isc.Browser.iOSVersion == 7) {
                if (this.getOrientation() === "landscape") {
                    var pageZoom = this._getPageZoom();
                    //var heightOffset = (20 / pageZoom) << 0;
                    //height -= heightOffset;
                    height = ((692 - 20) / pageZoom) << 0;
                    document.body.style.height = height + "px";
                    document.documentElement.style.height = height + "px";
                } else {
                    document.body.style.height = "";
                    document.documentElement.style.height = "";
                }
            }

            
            if (isc.Browser.isTouch && (isc.Browser.isAndroid || isc.Browser.isRIM)) {
                if (this._oldHeight == null) {
                    this._oldHeight = height;
                    this._oldWidth = isc.Page.getWidth(wd, recalculate);
                    this._prevSizeOrientation = this.getOrientation();
                } else {
                    
                    var orientation = this.getOrientation();
                    if (height > this._oldHeight ||
                        Math.abs(height - this._oldHeight) <= 100 ||
                        orientation != this._prevSizeOrientation)
                    {
                        this._prevSizeOrientation = orientation;
                        this._oldHeight = height;
                        this._oldWidth = isc.Page.getWidth(wd, recalculate);
                    } else {
                        height = this._oldHeight;
                    }
                }
            }

            if (wd == window) this.height = height;
            return height;
        }
	}
: // isc.Browser.isIE || isc.Browser.isOpera
  function (wd) {
		if (!wd) wd = window;
        var documentBody = wd.document.body;
        // In Opera it appears that document.body.clientWidth / height returns the size
        // (inside scrollbars) even in strict mode
        if (isc.Browser.isStrict && (!isc.Browser.isOpera || isc.Browser.minorVersion >= 9.5))
            documentBody = wd.document.documentElement;        
		if (documentBody) {
			return documentBody.clientHeight;
		} else {
			//>DEBUG
			this.logWarn("NOTE: isc.Page.getHeight() called before <BODY> tag was written out -- value cannot be determined.  Returning 500");
			//<DEBUG
			return 500;	
		}
	}
),

//> @type PageOrientation
// Is this page being viewed in landscape or portrait orientation? Typically used with
// mobile devices.
//
// @value "landscape" Landscape orientation: page is wider than it is tall.
// @value "portrait" Portrait orientation: page is taller than it is wide.
// @visibility external
//<


//> @classMethod Page.getOrientation()
// Is the current page wider than it is tall ("landscape" orientation) or the reverse 
// ("portrait" orientation). Note that the +link{pageEvent,orientationChange page event}
// will be fired whenever the page orientation changes.
// <P>
// This method is typically useful for apps developed for display on mobile devices,
// though it will also return a valid value when running against a desktop browser.
// See also +link{group:mobileDevelopment,this discussion} on building applications
// for mobile devices
// @return (PageOrientation) current page orientation
// @group mobileDevelopment
// @visibility external
//<
getOrientation : function () {
    

    
    var width = this.getWidth();
    if (isc.EH != null && width == isc.EH._currentWidth) return isc.EH.currentOrientation;

    var isPortrait;
    if (isc.Browser.isIPhone || isc.Browser.isIPad) {
        isPortrait = window.orientation == 0 || window.orientation == 180;
    } else if ("matchMedia" in window) {
        
        var portraitOrientation = window.matchMedia("(orientation: portrait)");
        isPortrait = portraitOrientation && portraitOrientation.matches;
    }
    if (isPortrait == null) {
        isPortrait = this.getHeight() >= width;
    }
    return isPortrait ? "portrait" : "landscape";
},

_getViewportTag : function (dontCreate) {
    var head = document.head || document.getElementsByTagName("head")[0];
    if (head == null) {
        if (dontCreate) return null;
        head = document.createElement("head");
        var body = document.body || document.getElementsByTagName("body")[0];
        body.parentNode.insertBefore(head, body);
    }

    var metaTags = head.getElementsByTagName("meta"),
        vpTag = null;
    // Remove all but the last viewport <meta> tag and select the last one.
    for (var i = 0; i < metaTags.length; /*empty*/) {
        if (metaTags[i].name == "viewport") {
            if (vpTag != null) {
                vpTag.parentNode.removeChild(vpTag);
                vpTag = metaTags[i - 1];
            } else {
                vpTag = metaTags[i];
                ++i;
            }
        } else ++i;
    }
    if (vpTag == null && !dontCreate) {
        vpTag = document.createElement("meta");
        vpTag.name = "viewport";
        head.appendChild(vpTag);
    }

    return vpTag;
},

_applyDefaultViewport : function () {
    if (window.isc_useDefaultViewport == false) return;

    var extraVpProps = {};

    
    if (isc.Browser._supportsMinimalUI && window.isc_useMinimalUI != false) {
        extraVpProps["minimal-ui"] = null;

        isc.Canvas.defaultPageSpace = (isc.Page.getOrientation() === "landscape" ? 20 : 0);
        window.addEventListener("orientationchange", function () {
            
            window.scrollTo(0, 0);
            isc.Canvas.setDefaultPageSpace(isc.Page.getOrientation() === "landscape" ? 20 : 0);
        }, true);
    }

    this.updateViewport(1.0, "device-width", null, false, extraVpProps);
},

// http://dev.w3.org/csswg/css-device-adapt/#parsing-algorithm
_parseViewportOptions : function (viewportContent) {
    var vpProps = {};
    if (isc.isA.String(viewportContent) && !isc.isAn.emptyString(viewportContent)) {
        // Remove all whitespace (horizontal tab, newline, carriage return, or space) and then
        // split on the separators comma and semicolon.
        var vpPropPairs = viewportContent.replace(/[\t\n\r ]+/g, "").split(/,|;/);
        for (var i = 0, len = vpPropPairs.length; i < len; ++i) {
            var vpPropPair = vpPropPairs[i];
            var eqPos = vpPropPair.indexOf('=');

            // It could be a valueless parameter (e.g. iOS 7.1's 'minimal-ui')
            if (eqPos <= 0) {
                vpProps[vpPropPair] = null;

            } else {
                // since viewport property names are case-insensitively-matched, lower case the prop name
                vpProps[vpPropPair.substring(0, eqPos).toLowerCase()] = vpPropPair.substring(eqPos + 1);
            }
        }
    }
    return vpProps;
},

//> @classMethod Page.updateViewport()
// This method only applies to browsers that support the special viewport meta tag, such as
// Mobile Safari running on the iPhone.
// <P>
// This method will dynamically change the viewport configuration, allowing you to set an
// initial size or scale level and enable / disable user-scaling. Typically this method will
// be called with a value for scale, width or height rather than passing in values for all
// three properties.
// <P>
// See Apple's Safari Web Content Guide on configuring the viewport for more information:
// +externalLink{https://developer.apple.com/library/safari/documentation/AppleApplications/Reference/SafariWebContent/UsingtheViewport/UsingtheViewport.html}
// <P>
// <i>Note:</i> Modifying the width/height or initial scale of the viewport has two user-visible
// effects:
// <ul>
// <li>HTML elements may reflow to fit the specified size (or the implied size calculated
//     from the specified scale level and the native device size).</li>
// <li>If the user has not scaled the application explicitly, and no other scaling or sizing
//     attributes were specified via a viewport meta tag for this page, the application will
//     zoom to specified scale level (or the scale level required to fit the specified viewport
//     size to the device's screen).</li>
// </ul>
// @param [scale] (float) Desired scale level where 1 indicates no scaling (each css pixel 
//   will be displayed using 1px on the physical device). Pass in null to avoid setting
//   this property.
// @param [width] (Integer) Desired viewport width in pixels. This indicates how many pixels
//   should fit within the device screen. Pass in null to avoid setting this property.
// @param [height] (Integer) Desired viewport height in pixels. This indicates how many pixels
//   should fit within the device screen. Pass in null to avoid setting this property.
// @param [scalable] (Boolean) Should the user be able to scale the application (using
//   pinch gestures, double tapping, rotating the device, etc.)? Pass in null to avoid setting
//   this property.
// 
// @group mobileDevelopment
// @see group:mobileDevelopment
// @visibility external
//<

updateViewport : function (scale, width, height, scalable, extraVpProps) {
    var vpTag = this._getViewportTag();

    // Parse the existing viewport parameters so that we preserve any extra viewport parameters
    // added by the application that we won't be changing.
    var vpProps = isc.addProperties(this._parseViewportOptions(vpTag.content), extraVpProps);

    if (width == null) width = vpProps["width"];
    if (height == null) height = vpProps["height"];
    if (scalable == null) scalable = vpProps["user-scalable"] != "no";

    if (scale != null) {
        if (isc.isA.Number(scale)) scale = scale.toFixed(2);
        vpProps["initial-scale"] = scale;
    }
    if (width != null) vpProps["width"] = width;
    if (height != null) vpProps["height"] = height;
    if (scalable != null) {
        vpProps["user-scalable"] = (scalable == false ? "no" : "yes");
        // setting user-scalable to 'no' seems to reliably disable pinch zooming
        // However on pivot the iPhone zooms by default and this seems to still occur
        // with user-scalable set to 'no'. If a desired 'scale' was specified,
        // setting the min/max scale to it appears to really disable scale on pivot
        if (scalable == false && scale != null) {
            vpProps["minimum-scale"] = vpProps["maximum-scale"] = scale;
        }
    }

    var content = [];
    for (var vpPropName in vpProps) {
        var val = vpProps[vpPropName];
        if (val == null) {
            content.push(vpPropName);
        } else {
            content.push(vpPropName + "=" + val);
        }
    }
    content = content.join(", ");

    
    if (isc.Browser.isMobileFirefox) {
        vpTag.insertAdjacentHTML("afterend", "<meta name='viewport' content='" + String.asAttValue(content) + "'>");
        var oldVpTag = vpTag;
        vpTag = vpTag.nextSibling;
        oldVpTag.parentNode.removeChild(oldVpTag);

    } else {
        vpTag.content = content;
    }
},


_getPageZoom : function () {
    if (isc.Browser.isTouch) {
        var vpTag;

        var screenWidth = window.screen.width,
            screenHeight = window.screen.height;
        if (screenWidth <= document.documentElement.clientWidth) {
            if (this.getOrientation() === "portrait") {
                return Math.min(screenWidth, screenHeight) / window.innerWidth;
            } else {
                return Math.max(screenWidth, screenHeight) / window.innerWidth;
            }
        } else if ((vpTag = this._getViewportTag(true)) != null) {
            var vpProps = this._parseViewportOptions(vpTag.content);
            var scalable = vpProps["user-scalable"] != "no";
            if (!scalable) {
                // In Android WebViews (Android 4.3 or earlier), there is a bug where user-scalable=no
                // permanently sets the scale to 1.
                if (isc.Browser.isAndroidWebView && isc.Browser.androidMinorVersion < 4.4) {
                    return 1;
                } else {
                    var scale = parseFloat(vpProps["initial-scale"]);
                    if (!window.isNaN(scale)) return scale;

                    scale = parseFloat(vpProps["minimum-scale"]);
                    if (!window.isNaN(scale)) return scale;
                }
            } else {
                var minScale;
                if ("minimum-scale" in vpProps) {
                    minScale = parseFloat(vpProps["minimum-scale"]);
                } else {
                    minScale = parseFloat(vpProps["initial-scale"]);
                }

                if (!window.isNaN(minScale)) minScale;
            }
        }
    }

    return 1;
},

//>	@classMethod	Page.getScrollWidth()
//		Get the width of the window contents as they have been drawn.
//		If the page scrolls, this may be larger than the page.getWidth().
//		@group	sizing
//
//		@return	(number)	width of the page as drawn
// @visibility external
//<

getScrollWidth : function (theDoc) {	
    var theDoc = theDoc || document;
	if (theDoc == null || theDoc.body == null) return 500;
	if (isc.Browser.isIE && isc.Browser.version >= 6) {
		// in IE6 in standards compliant mode (DOCTYPE HTML 4 Transitional/Strict), IE hides the
		// window viewport size in window.document.documentElement, and document.body only
		// reports the size of the drawn content.
		var width = Math.max(Math.max(theDoc.body.scrollWidth, theDoc.documentElement.clientWidth))
		    
		// In IE9 with HTML5 doctype, in RTL mode the scroll width is reported under
		// documentElement.scrollWidth. 
		
		if (isc.Browser.isStrict) width = Math.max(width, theDoc.documentElement.scrollWidth);
		return width;
	}
    return theDoc.body.scrollWidth;
},

//>	@classMethod	Page.getScrollHeight()
//		Get the height of the window contents as they have been drawn.
//		If the page scrolls, this may be larger than the page.getHeight().
//		@group	sizing
//
//		@return	(number)	height of the page as drawn
// @visibility external
//<
getScrollHeight : function (theDoc) { 
        var theDoc = theDoc || document;
    
    if (theDoc == null || theDoc.body == null) {
        
        return this.getHeight();
    }
    
    // In most cases we can use document.body.scrollHeight
    // however in strict mode we have to look at document.documentElement instead.
    
        var scrollHeight = theDoc.body.scrollHeight;
    if (isc.Browser.isStrict) {
        var docElement = theDoc.documentElement;
        if (docElement) {
            var docScrollHeight = docElement.scrollHeight;
            if (!isc.isA.Number(docElement.scrollHeight)) {
                docScrollHeight = Math.max(docElement.offsetHeight, docElement.clientHeight);
	}	
            if (docScrollHeight && docScrollHeight > scrollHeight) {
                scrollHeight = docScrollHeight;
		}
	}
    }
    return scrollHeight;
},

//>	@classMethod	Page.getScrollLeft()
//		Get the amount that the browser window has been scrolled horizontally.
//		@group	sizing
//
//		@return	(number)	horizontal scroll amount
// @visibility external
//<

getScrollLeft : (isc.Browser.isNS ? 
	function (absCoordSpace) {
	    var left = window.pageXOffset;
	    
	    if (isc.Page.isRTL() && !absCoordSpace) {
	        left = isc.Canvas._adjustScrollLeftForRTL(left,
                        isc.Page.getScrollWidth(), isc.Page.getWidth(), 
                        // going from negative origin coords to sc (zero based) coords
                        false, 
                        // adjust for negative origin
                        true, false);
        }
        return left;
	}
: // isc.Browser.isIE
	function (absCoordSpace) {		
        if (document == null || document.body == null) return 0;
		// in IE6 in standards compliant mode (DOCTYPE HTML 4 Transitional/Strict)
        // document.body.scrollLeft and document.body.scrollTop are always zero while
        // document.documentElement.scrollLeft and scrollTop reflect the actual browser scrollDeltas
        var left = (isc.Browser.isStrict ? document.documentElement.scrollLeft : 
                                       document.body.scrollLeft);
	    
        if (isc.Page.isRTL()) {
            var inverseCoords = isc.Browser.isIE9 && isc.Browser.isStrict;
            var scrollWidth = (inverseCoords || absCoordSpace) ? isc.Page.getScrollWidth() : null,
                width = (inverseCoords || absCoordSpace) ? isc.Page.getWidth() : null;
                
            if (inverseCoords) {
                left = isc.Canvas._adjustScrollLeftForRTL(left, 
                            scrollWidth, width, 
                            false, !inverseCoords, inverseCoords);
            }
            // at this point we have zero origin -- if we're asked for abs-coords space, we'll have
            // to further modify
            if (absCoordSpace) {
                left = isc.Canvas._adjustScrollLeftForRTL(left, 
                        scrollWidth, width, 
                        // we want to go from zero origin coords to negative origin coords
                        true, true);
            }
        }
        return left;
    }
),

//>	@classMethod	Page.getScrollTop()
//		Get the amount that the browser window has been scrolled vertically.
//		@group	sizing
//
//		@return	(number)	vertical scroll amount
// @visibility external
//<
getScrollTop : (isc.Browser.isNS ? 
	function () {		return window.pageYOffset;			}
:// isc.Browser.isIE
	function () {		
        if (document == null || document.body == null) return 0;
		// in IE6 in standards compliant mode (DOCTYPE HTML 4 Transitional/Strict)
        // document.body.scrollLeft and document.body.scrollTop are always zero while
        // document.documentElement.scrollLeft and scrollTop reflect the actual browser scrollDeltas
        return (isc.Browser.isStrict ? document.documentElement.scrollTop : 
                                       document.body.scrollTop);		
    }
),

//>	@classMethod	Page.getScreenWidth()
//		Get the width of the user's screen, in pixels.
// @visibility external
//<
getScreenWidth : function () { return screen.width },

//>	@classMethod	Page.getScreenHeight()
//		Get the height of the user's screen, in pixels.
// @visibility external
//<
getScreenHeight : function () { return screen.height },

//>	@classMethod	Page.getWindowRect()
//			return the coordinates of the window wd, or current window if wd isn't specified
//			in IE this doesn't include scrollbar size (if any)
//		@group	positioning, sizing
//		
//		@param	[wd]		(object)	the window object
//
//		@return	(object)		{left:x, top:y, width:w, height:h}
//<
getWindowRect : function (wd) {
	if (!wd) wd = window;
	return {
		left:	(isc.Browser.isIE || isc.Browser.isOpera ? wd.screenLeft : wd.screenX),
		top:	(isc.Browser.isIE || isc.Browser.isOpera ? wd.screenTop : wd.screenY),
		width:	isc.Page.getWidth(wd),
		height:	isc.Page.getHeight(wd)
	};
},
	
setUnloadMessage : function (message) {
    if (message == null) window.onbeforeunload = null;
    else window.onbeforeunload = function () { return message; };
},

// --------------------------------------------------------------------------------------------

//>	@classMethod	Page.goBack()
// Go back in the browser's history.<br><br>
//
// If the history is empty and the window.opener is set, we assume we're a child window and just
// close the window.
//
// @visibility external
//<
goBack : function () {
	if (history.length == 0 && window.opener) {
		window.close();
	} else {
		history.back();
	}
},

//>	@classMethod	Page.print()
//		Print the window.  This brings up the print dialog and the user actually
//		starts printing.
//
//		Note: In IE4, you have to have already created a BODY tag in the window for this to work.
//
//		@param	wd	(window)	pointer to a window or frame to print
//								default is the current window
//<
print : function (wd) {
	// default to the current window
	if (!wd) wd = window;
	if (wd.print) {
		wd.print();
	} else {
		// get a pointer to the document of the window
		var doc = wd.document;
		// if not found, bail
		if (!doc || !doc.body) {
			//>DEBUG
			this.logError("isc.Page.print() called on a window that doesn't have a document.body defined.  Exiting.");
			//<DEBUG
			return;
		}
		// The following works in Windows IE only
		// insert a built-in active-x control that will do the printing for us
		if (isc.Browser.isWin) {
			doc.body.insertAdjacentHTML('beforeEnd', 
				'<OBJECT ID="printControl" WIDTH=0 HEIGHT=0 CLASSID="CLSID:8856F961-340A-11D0-A96B-00C04FD705A2"></OBJECT>'
			);
			// get a pointer to the printControl
			var control = doc.all.printControl;
			if (!control) {
				//>DEBUG
				this.logError("isc.Page.print() couldn't create or find print control.  Exiting.");
				//<DEBUG
				return;
			}
			
			// call the print command
			//	-- pass '2' below to skip the dialog box
			control.ExecWB(6, 1);
			// remove the control, since we don't need it anymore
			control.outerHTML = "";
		} else {
			alert("Choose 'Print...' from the File menu to print this page.");
		}
	}
},

// --------------------------------------------------------------------------------------------

// Observation from static scope; for comments, see class.observe
observe : function (object, methodName, action) {
    // create a dummy instance of Class so we can use it for calling "observe" statically
    var observer = isc.Class.create();
    return observer.observe(object, methodName, action);
},

//> @classMethod Page.waitFor()
// Wait for a method to fire on an object.
// <P>
// <code>waitFor</code> is similar +link{class.observe,observation}, but fires once only.
//
// @param object (Object) any SmartClient object, eg, a ListGrid
// @param methodName (String) name of a method on that object
// @param callback (Function) Callback to fire when the observed method completes
// @param [timeout] (Number) Optional timeout period (in milliseconds). If you want a timeout,
//                           you must also provide a timeoutCallback 
// @param [timeoutCallback] (Function) Callback to fire if the timeout period elapses before the 
//                                     observed method completes
// @return (boolean) whether observation succeeded.  Observation may fail due to null object,
//                   non-existent method or similar bad parameters
//
// @visibility external
//<
// waitFor: wait for a method to fire on another object
waitFor : function (object, method, callback, timeout, timeoutCallback) {
    var observer = isc.Class.create({
        _waitObject : object,
        _waitMethod : method,
        _waitCallback : callback,
        _fired : function (observed) {
            if (this._timerEvent) {
                isc.Timer.clear(this._timerEvent);
            }
            this.ignore(this._waitObject, this._waitMethod);
            this.fireCallback(this._waitCallback, "observed", [observed]);
            this.destroy();
        },
        _timeoutElapsed : function () {
            this.ignore(this._waitObject, this._waitMethod);
            this.fireCallback(this._timeoutCallback);
            this.destroy();
        }
    });
    isc.ClassFactory.addGlobalID(observer);
    var worked = observer.observe(object, method, function () {
        this._fired(object);
    });
    if (!worked) observer.destroy();

    if (timeout && timeoutCallback) {
        observer._timeoutCallback = timeoutCallback;
        observer._timerEvent = isc.Timer.setTimeout(function () { observer._timeoutElapsed(); }, timeout);
    }

    return worked;
},

//> @classMethod Page.waitForMultiple()
// Wait for methods to fire on multiple objects.
// <P>
// <code>waitForMultiple</code> is similar to +link{page.waitFor}, except that it does not fire
// its callback until all of the provided methods have fired.
//
// @param objects (Array) an array of objects, each of which consists of two properties:<br/>
//                        "object": any SmartClient object, eg a ListGrid<br/>
//                        "method": name of a method on that object
// @param callback (Function) Callback to fire when all observed methods have fired
// @param [timeout] (Number) Optional timeout period (in milliseconds). If you want a timeout,
//                           you must also provide a timeoutCallback 
// @param [timeoutCallback] (Function) Callback to fire if the timeout period elapses before all 
//                                     observed methods have fired
// @return (boolean) whether observation succeeded.  Observation may fail due to null objects,
//                   non-existent methods or similar bad parameters
//
// @visibility external 
//<
waitForMultiple : function (objects, callback, timeout, timeoutCallback) {

    var allWorked = true;

    var supervisor = isc.Class.create({ 
        _waitObjects : objects,
        _waitObservers : [],
        _waitCallback : callback,
        _observedMethodFired : function (observer) {
            this._waitObservers.remove(observer);
            if (this._waitObservers.isEmpty()) {
                if (this._timerEvent) {
                    isc.Timer.clear(this._timerEvent);
                }
                this.fireCallback(this._waitCallback);
                this.destroy();
            }
        },
        _timeoutElapsed : function () {
            var obs = this._waitObservers;
            for (var i = 0; i < obs.length; i++) {
                obs[i].ignore(obs[i]._waitObject, obs[i]._waitMethod);
                obs[i].destroy();
            }
            this.fireCallback(this._timeoutCallback);
            this.destroy();
        }
    });
    
    for (var i = 0; i < objects.length; i++) {
        var observer = isc.Class.create({
            _waitObject : objects[i].object,
            _waitMethod : objects[i].method,
            _supervisor : supervisor,
            _fired : function (observed) {
                this.ignore(this._waitObject, this._waitMethod);
                this._supervisor._observedMethodFired(this);
                this.destroy();
            }
        });
        isc.ClassFactory.addGlobalID(observer);
        var action = (function (observed) {
            return function () {
                this._fired(observed);
            };
        })(objects[i].object);
        var worked = observer.observe(objects[i].object, objects[i].method, action);
        if (worked) {
            supervisor._waitObservers.add(observer);
        } else {
            observer.destroy();
            allWorked = false;
        }
    }
    
    if (timeout && timeoutCallback) {
        supervisor._timeoutCallback = timeoutCallback;
        supervisor._timerEvent = isc.Timer.setTimeout(function () { supervisor._timeoutElapsed(); }, timeout);
    }
    
    return allWorked;
},
   

//> @classAttr Page.unsupportedBrowserAction (string : "continue" : IRA)
// Action to take when +link{Page.checkBrowserAndRedirect()} is called in a browser for which
// support is not guaranteed. Valid settings are:
// <ul>
// <li><code>"continue"</code> Load the page without notifying the user of potential issues</li>
// <li><code>"confirm"</code> Notify the user via a standard confirm dialog that their browser is
//  not supported. Provide options to continue anyway, or redirect to another page.
// <li><code>"redirect"</code> Automatically redirect to the another URL
// </ul>
// @see Page.checkBrowserAndRedirect()
// @see Page.defaultUnsupportedBrowserURL
// @visibility external
//<
unsupportedBrowserAction:"continue",

//> @classMethod Page.checkBrowserAndRedirect(URL)
// Check whether the browser is supported by the Isomorphic SmartClient system. Behavior depends
// upon the value of +link{Page.unsupportedBrowserAction}:
// <ul>
// <li><code>"continue"</code> Load the page without notifying the user of potential issues</li>
// <li><code>"confirm"</code> Notify the user via a standard confirm dialog that their browser is
//  not supported. Provide options to continue anyway, or redirect to another page. Text of the
//  confirm dialog is retrieved by calling +link{Page.getUnsupportedBrowserPromptString()}.</li>
// <li><code>"redirect"</code> Automatically redirect to the another URL</li>
// </ul>
// If redirecting to another page is necessary, and no explicit URL is provided we will use
// +link{Page.defaultUnsupportedBrowserURL}.
// <p>
// This method is commonly called as part of the +link{group:skinning,skinning} logic after page
// load.
//
// @param	[URL]   (string)	URL of redirect page. May include Isomorphic special directories
//      such as [SKIN].
// @see Page.unsupportedBrowserAction
// @see Page.getUnsupportedBrowserPromptString()
// @see Page.defaultUnsupportedBrowserURL
// @visibility external
//<
checkBrowserAndRedirect : function (URL) {
    if (!isc.Browser.isSupported) {
        if (isc.Log) {
            isc.Log.logWarn("Unsupported browser detected - userAgent:" + navigator.userAgent);
        }
        
        if (this.unsupportedBrowserAction == "continue") return;
        
        var noRedirect = this.unsupportedBrowserAction == "confirm" &&
                          confirm(this.getUnsupportedBrowserPromptString())
                          
        if (noRedirect) return;
    
        // if the browser is not supported we support three options:
        // - carry on and cross fingers
        // - prompt the user to carry on, or go to unsupported browser page
        // - redirect them to the "unsupported_browser.html" page
        if (URL == null) URL = isc.Page.defaultUnsupportedBrowserURL; 
        var UNSUPPORTED_BROWSER_DETECTED = true;
        window.location.replace(isc.Page.getURL(URL));
    }

},

//> @classMethod Page.getUnsupportedBrowserPromptString()
// Returns the text for the prompt shown to user from +link{Page.checkBrowserAndRedirect()}
// if they are accessing this page in an unsupported
// browser and +link{Page.unsupportedBrowserAction} is set to <code>"confirm"</code>. May be
// overridden to return a different message.
// @return (string) Unsupported browser message.
// @group i18nMessages
// @see Page.checkBrowserAndRedirect()
// @visibility external
//<
getUnsupportedBrowserPromptString : function () {
    var message = "This page uses the Isomorphic SmartClient web presentation layer " +
    "(Version" + isc.version + " - " +isc.buildDate +
    "). The web browser you are using is not supported by this version of SmartClient" +
    " and you may encounter errors on this page. Would you like to continue anyway?\n\n" +
    "(Reported userAgent string for this browser:"+ navigator.userAgent + ")";
    return message;
},

//>	@classMethod	Page.setAddVersionToSkinCSS()
// Setting this to true will cause +link{classMethod:Page.loadStyleSheet()} to append
// an "isc_version" parameter to the end of the url when loading a stylesheet.
//
//		@group	skins, files
//
//		@param	addVersionToSkinCss	(Boolean)	pass in true to turn on automatic adding of version
//                                              parameter to css urls.
// @visibility external
//<
setAddVersionToSkinCSS : function (addVersionToSkinCSS) {
   this._addVersionToSkinCSS = addVersionToSkinCSS;
},

//>	@classMethod	Page.isAddVersionToSkinCSS()
// Returns true if add version to skin CSS is currently turned on.
//
//      @group skins, files
//		@return	(Boolean)		true == add version to skin CSS is turned on
//
// @visibility external
//<
isAddVersionToSkinCSS : function () {
    return this._addVersionToSkinCSS;
}


});	// END isc.Page.addClassMethods()

if (isc.Page.isXHTML()) isc.nbsp = isc.xnbsp;

// set the default directories for files to what's set in the window, if anything
isc.Page.setDirectories();

// in Moz get the page size right now so it's available even before the Page is done loading
// without triggering the "zero width bug"
if (isc.Browser.isMoz) {
    isc.Page.getWidth(null, true);
    isc.Page.getHeight(null, true);
}


if (isc.Browser.isAndroid) {
    isc.Page.pollPageSize = true;
}
