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

 






//>	@class	Cookie
//
//	Singleton class to manage browser cookies automatically.
//	The "Cookie" object is automatically created by the system for you.
//
//	Note that there is a limit to the size of the data that you can store
//	in all cookies for a site; it is generally believed that 1024 characters
//	is the maximum amount you can safely store in all cookies for one site.
//
//	You access cookie functions by calling methods on Cookie directly:
//
//			var value = Cookie.get("myCookieName");
//			Cookie.set("myCookieName", 100);
//			Cookie.clear("myCookieName");
//
//<


isc.ClassFactory.defineClass("Cookie");


//
//	add class methods to the cookie object
//
isc.Cookie.addClassMethods({
//>	@classMethod		Cookie.init()	(A)
//		Initialize the cookies array.  This method is called automatically whenever cookies are
//      accessed to make sure they're always correct.
//<
init : function () {
	isc.Cookie.list = {};
	if (document.cookie == "") return;
	
	var list = ("" + document.cookie).split("; ");
	for (var i = 0, len = list.length, it; it = list[i], i < len; i++) {
		var equalChar = it.indexOf('='),
			name = (equalChar == -1 ? it : it.substring(0,equalChar))
		;
		isc.Cookie.list[name] = (equalChar == -1 ? '' : unescape(it.substring(equalChar+1)));
	}
},

//>	@classMethod		Cookie.get()
//		Get the value of a cookie by name.
//
//		@param	name	(string)	name of the cookie
//		@param			(string)	value of the cookie or null if not found
//<
get : function (name) {
	// call init again to refresh the list of cookies
	isc.Cookie.init();

	// get the value of the cookie
	return isc.Cookie.list[name];
},

//>	@classMethod		Cookie.set()
//		Set the value of a cookie.
//
//		@param	name			(string)		name of the cookie
//		@param	value			(string)		value for the cookie
//		@param	[path]			(string)		path to the cookie
//		@param	[domain]		(string)		domain of the cookie
//		@param	[expiration]	(date | string)	expiration date for the cookie
//<
set : function (name, value, path, domain, expiration) {
	// call init again to refresh the list of cookies
	isc.Cookie.init();

	// add the cookie
	document.cookie = name + "=" + escape(value)
							+ (path ? ";path=" + path : "")
							+ (domain ? ";domain=" + domain : "")
							+ (expiration ? ";expires=" + (isc.isA.String(expiration) ? expiration : expiration.toGMTString()) : "");
},

//>	@classMethod		Cookie.clear()
//		Clear a particular cookie.
//
//		@param	name			(string)		name of the cookie
//		@param	[path]			(string)		path to the cookie
//		@param	[domain]		(string)		domain of the cookie
//<
clear: function (name, path, domain) {
	// call init again to refresh the list of cookies
	isc.Cookie.init();		

	// set the cookie to empty and set the expiration time to a long time ago
	this.set(name, "", path, domain, "Thu, 01-Jan-70 00:00:01 GMT");
},

//>	@classMethod		Cookie.getList()
//		Return the names of all of the cookies,
//
//		@return		(string[])	array of the names of all the cookies
//<
getList : function () {
	isc.Cookie.init();
	return isc.getKeys(isc.Cookie.list);
}

});



