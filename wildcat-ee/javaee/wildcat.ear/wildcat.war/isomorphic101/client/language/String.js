/*
 * Isomorphic SmartClient
 * Version SNAPSHOT_v10.1p_2015-12-10 (2015-12-10)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */
//>DEBUG
// This lets us label methods with a name within addMethods
String.prototype.Class = "String";
  //<DEBUG 

//>	@class String
//	Generic extensions to JavaScript Strings.  You can call these on any String.
// @treeLocation Client Reference/System
// @visibility external
//<

isc._patchLocaleSupport = function () {
    
    var protos = [Array, Number, Date].getProperty("prototype");
    for (var i = 0; i < protos.length; i++) {
        var theProto = protos[i];
        if (theProto.toLocaleString == null) {
            theProto.toLocaleString = theProto.toString;
        }
    }

    // ensure String.toLocaleUpper/LowerCase are there so we can call them blindly
    var strProto = String.prototype;
    if (!strProto.toLocaleUpperCase) {
        strProto.toLocaleUpperCase = strProto.toUpperCase;
        strProto.toLocaleLowerCase = strProto.toLowerCase;
    }

    // Mozilla's String.toLocaleString() actually does the equivalent of Object.toString(),
    // which is to return [object String] instead of the string value, so we patch it to
    // simply return the equivalent of String.toString() since Strings are unicode by nature.
    if (isc.Browser.isMoz) {
        var string = "x",
            localeString = string.toLocaleString();
            if (localeString != string) {
                
                strProto.toBrowserLocaleString = strProto.toLocaleString;
                strProto.toLocaleString = strProto.toString;
            }
        // Patch Boolean as well
        string = true;
        localeString = string.toLocaleString();
        if (localeString != string + "") {
            
            Boolean.prototype.toBrowserLocaleString = Boolean.prototype.toLocaleString;
            Boolean.prototype.toLocaleString = Boolean.prototype.toString;   
        }
    }
}
isc._patchLocaleSupport();

isc.addProperties(String, {
    _singleQuoteRegex : new RegExp("'", "g"),
    _doubleQuoteRegex : new RegExp("\"", "g")
});

isc.addMethods(String.prototype, {

//>	@method	string.replaceAll()
//			Replace all occurances of 'find' string with 'replacement' string.
//			Uses a native method so is very efficient (and easier to use than grep).
//		@group	stringProcessing
//
//		@param	find		(string)	string to find
//		@param	replacement	(string)	string to replace each occurance of find with
//
//		@return				(string)	new string with replacements made
//<
replaceAll : function (find, replacement) {
    
    return isc.replaceAll(this, find, replacement);
},

//>	@method	string.contains()
//			Returns true if this string contains the specified substring.
//		@group	stringProcessing
//
//		@param	substring	(String)	string to look for
//		@return				(boolean)	true == this string contains the substring
// @visibility external
//<
contains : function (substring) {
    // support eg Numbers.  Note: only available with non-performance-critical version of API
    if (substring && !isc.isA.String(substring)) substring = substring.toString();

    
	return isc.contains(this, substring);
},

//>	@method	string.convertTags()	(A)
//			Convert all tag symbols ( &lt;  and &gt; ) into displayable HTML
//			by changing them to   &amp;lt;  and  &amp;gt;   respectively.
//		@group	stringProcessing
//
//		@param	[prefix]	(string)	text to tack onto the beginning of result (eg: "&lt;PRE&gt;")
//		@param	[suffix]	(string)	text to tack onto the end of result (eg: "&lt;/PRE&gt;")
//
//		@return				(string)	prefix + converted text + suffix as a single string
//<
convertTags : function (prefix,suffix){
	// use regular expressions to convert < and > characters
	return (prefix ? prefix : "") +
		this.replace(/</g, "&lt;").replace(/>/g, "&gt;") +
		(suffix ? suffix : "");
},

//>	@method	string.asHTML()
// Convert plain text into into displayable HTML.
// <p>
// This prevents HTML-special characters like &lt; and &gt; from being interpreted as tags, and
// preserves line breaks and extra spacing.
// <pre>
//    converts		   to
//    --------  		   ---------------------------
//    &				   &amp;
//    <				   &lt;
//    >				   &gt;
//    \r,\n,\r\n1space <BR>&nbsp;
//    \r,\n,\r\n	   <BR>
//    \t			   &nbsp;&nbsp;&nbsp;&nbsp;
//    2 spaces		   1space&nbsp;
// </pre>
//
// @group stringProcessing
// @return (string) string of HTML with tags in the original HTML escaped.
//<
asHTML : function (noAutoWrap) {
	var s = this.replace(/&/g, "&amp;")
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
},

asAttValue : function (doubleQuote, includeOuterQuotes) {
    
    return String.asAttValue(this, doubleQuote, includeOuterQuotes);
},

// revereses asHTML()
unescapeHTML : function () {
    // Note: in asHTML() we turn tabs into four &nbsp;, this reversal is lossy in that it turns
    // those into four spaces - but we really have no way of knowing whether there were four
    // spaces there before or a tab.
    return this.replace(/&nbsp;/g, " ")
               .replace(/<BR>/gi, "\n")
               .replace(/&gt;/g, ">")
               .replace(/&lt;/g, "<")
               .replace(/&amp;/g, "&");
},




//>	@method	string.toInitialCaps()
//			Convert A String To Initial Caps
//		@group	stringProcessing
//
//		@return				(string)	converted string
//<
toInitialCaps : function () {
	// lowercase the entire thing, then split by spaces
	var it = this.toLowerCase().split(" ");
	// for each word
	for (var i = 0; i < it.length; i++) {
		// uppercase the first letter, then add the rest (already lower case)
		it[i] = it[i].substring(0,1).toLocaleUpperCase() + it[i].substring(1);
	}
	return it.join(" ");
},


//>	@method	string.evalDynamicString()
//          Look for &#36;{expressions} in a string and evaluate them.  To escape, prepend a 
//          backslash to the dollar sign.  Note that in the event that you actually want
//          to display \&#36;{  you will have to escape the backslash as follows: \\&#36;{.  Note
//          also that if you're writing this in a JS string you must escape the backslash
//          again.
//		@group	dynamicString
//
//		@return				(string)	converted string
//<
evalDynamicString : function (target, evalVars) {
    // must toString() - otherwise strange object literal with slots is returned
	if (this.indexOf("${") < 0) return this.toString();
	var str = this, lastStart, start, end, evalBlock;

	// hand-coded for performance
	var accum = isc.StringBuffer.create();
	while ((start = str.indexOf("${")) != -1) {
			end = str.indexOf("}", start + 1);
			if (end == -1) break;

			// handle escapes
			if (str.charAt(start - 1) == '\\') {
				accum.append(str.slice(0, start - 1), str.slice(start, end + 1));
				str = str.substring(end + 1, str.length);
				continue;
			}
			var evalBlock = str.slice(start + 2, end);
            var evalResult;
            if (evalVars != null && evalVars[evalBlock]) {
                // shortcut to avoid evalWithVars, which creates a Function each time
                evalResult = evalVars[evalBlock];
            } else {
                try {
                    evalResult = isc.Class.evalWithVars(evalBlock, evalVars, target);
                } catch (e) {
                    // if a target has been supplied, use that for the log report
                    var logTarget = target ? target : isc.Log;
                    logTarget.logWarn("dynamicContents eval error - returning empty string for block -->${"
                                      + evalBlock + "}<-- error was: " + isc.Log.echo(e));
                    evalResult = isc.emptyString;                
                }
            }
			accum.append(str.slice(0, start), evalResult);
			str = str.substring(end + 1, str.length);
	}
	accum.append(str);
	return accum.release(false);
},


//>	@method	string.asSource()	(A)
// Return a new String that, evaluated as source code, would produce this String's value.
//		@group	stringProcessing
//
//		@return				(string)	new string
//<
asSource : function (singleQuote) {
    return String.asSource(this, singleQuote);
},

// String.cssToCamelCaps()
//  Converts a string in css dash syntax "foo-bar-baz" to camelCaps syntax "fooBarBaz".
// Non-alphabetic chars between the '-' and the lowercase letter are ignored,
// eg, 'test-234foo' -> 'test234Foo'.
cssToCamelCaps : function () {
    return this.replace(/-([^a-z]*)([a-z])/g,
                        function (str, p1, p2, offset, s) { return p1 + p2.toUpperCase(); });
}

});

// Concatenates the current string with itself `count' times. If `count' is 0, then an empty
// string is returned.

if (!String.prototype.repeat) {
    String.prototype.repeat = function (count) {
        count = count << 0;
        var str = String(this);
        if (str === "" || count == 0) return "";

        var repeated = "";
        for (;;) {
            if ((count & 1) == 1) {
                repeated += str;
            }
            count >>>= 1;
            if (count == 0) break;
            str += str;
        }
        return repeated;
    };
}

//> @method string.startsWith()
// Returns <code>true</code> if this string starts with another string, or if the other string
// occurs at the given <code>position</code> within this string.
//
// @param substring (String) other string to check
// @param [position] (int) optional position in this string. Defaults to 0.
// @return (boolean) <code>true</code> if <code>substring</code> occurs within this string at
// position <code>position</code>.
// @group stringProcessing
// @visibility external
//<

if (!String.prototype.startsWith) {
    String.prototype.startsWith = function (substring, position) {
        if (isc.isA.RegularExpression(substring)) {
            var a;
            a.throwNewTypeError();
        }

        // support eg Numbers.  Note: only available with non-performance-critical version of API
        substring = String(substring);

        var str = String(this);
        position = Math.min(Math.max(0, position << 0), str.length);
        if (position > str.length - substring.length) return false;
        if (position > 0) str = str.substring(position);

        
        return isc.startsWith(str, substring);
    };
}


//> @method string.endsWith()
// Returns <code>true</code> if this string ends with another string, or if the other string
// occurs in this string beginning at <code>position - substring.length</code>.
//
// @param substring (String) other string to check
// @param [position] (int) optional position in this string. Defaults to the length of this
// string.
// @return (boolean) <code>true</code> if <code>substring</code> occurs within this string
// ending with <code>position - 1</code>.
// @group stringProcessing
// @visibility external
//<

if (!String.prototype.endsWith) {
    String.prototype.endsWith = function (substring, position) {
        var undef;
        if (isc.isA.RegularExpression(substring)) {
            undef.throwNewTypeError();
        }

        // support eg Numbers.  Note: only available with non-performance-critical version of API
        substring = String(substring);

        var str = String(this);
        if (position !== undef) {
            position = Math.min(Math.max(0, position << 0), str.length);
            if (position < substring.length) return false;
            str = str.substring(0, position);
        }

        
        return isc.endsWith(str, substring);
    };
}

String.prototype.nativeTrim = String.prototype.trim;
String.prototype.trim = function (chars) {
    if (String.prototype.nativeTrim != null &&
        String.prototype.nativeTrim !== String.prototype.trim &&
        !chars)
    {
        return String.prototype.nativeTrim.call(this);

    } else {
        var str = String(this),
            removeChars = chars || " \t\n\r",
            l = str.length,
            start = 0,
            end = l - 1,
            i = 0;

        // find first character not in the removal list
        while (start < l && removeChars.contains(str.charAt(i++))) start++;

        // find last character not in the removal list
        i = l - 1;
        while (end >= 0 && end >= start && removeChars.contains(str.charAt(i--))) end--;

        return str.substring(start, end + 1);
    }
};


String._unicodeLPattern = "[\u0041-\u005a\u0061-\u007a\u00aa\u00b5\u00ba\u00c0-\u00d6\u00d8-\u00f6\u00f8-\u02c1\u02c6-\u02d1\u02e0-\u02e4\u02ec\u02ee\u0370-\u0374\u0376\u0377\u037a-\u037d\u0386\u0388-\u038a\u038c\u038e-\u03a1\u03a3-\u03f5\u03f7-\u0481\u048a-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05d0-\u05ea\u05f0-\u05f2\u0620-\u064a\u066e\u066f\u0671-\u06d3\u06d5\u06e5\u06e6\u06ee\u06ef\u06fa-\u06fc\u06ff\u0710\u0712-\u072f\u074d-\u07a5\u07b1\u07ca-\u07ea\u07f4\u07f5\u07fa\u0800-\u0815\u081a\u0824\u0828\u0840-\u0858\u08a0\u08a2-\u08ac\u0904-\u0939\u093d\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097f\u0985-\u098c\u098f\u0990\u0993-\u09a8\u09aa-\u09b0\u09b2\u09b6-\u09b9\u09bd\u09ce\u09dc\u09dd\u09df-\u09e1\u09f0\u09f1\u0a05-\u0a0a\u0a0f\u0a10\u0a13-\u0a28\u0a2a-\u0a30\u0a32\u0a33\u0a35\u0a36\u0a38\u0a39\u0a59-\u0a5c\u0a5e\u0a72-\u0a74\u0a85-\u0a8d\u0a8f-\u0a91\u0a93-\u0aa8\u0aaa-\u0ab0\u0ab2\u0ab3\u0ab5-\u0ab9\u0abd\u0ad0\u0ae0\u0ae1\u0b05-\u0b0c\u0b0f\u0b10\u0b13-\u0b28\u0b2a-\u0b30\u0b32\u0b33\u0b35-\u0b39\u0b3d\u0b5c\u0b5d\u0b5f-\u0b61\u0b71\u0b83\u0b85-\u0b8a\u0b8e-\u0b90\u0b92-\u0b95\u0b99\u0b9a\u0b9c\u0b9e\u0b9f\u0ba3\u0ba4\u0ba8-\u0baa\u0bae-\u0bb9\u0bd0\u0c05-\u0c0c\u0c0e-\u0c10\u0c12-\u0c28\u0c2a-\u0c33\u0c35-\u0c39\u0c3d\u0c58\u0c59\u0c60\u0c61\u0c85-\u0c8c\u0c8e-\u0c90\u0c92-\u0ca8\u0caa-\u0cb3\u0cb5-\u0cb9\u0cbd\u0cde\u0ce0\u0ce1\u0cf1\u0cf2\u0d05-\u0d0c\u0d0e-\u0d10\u0d12-\u0d3a\u0d3d\u0d4e\u0d60\u0d61\u0d7a-\u0d7f\u0d85-\u0d96\u0d9a-\u0db1\u0db3-\u0dbb\u0dbd\u0dc0-\u0dc6\u0e01-\u0e30\u0e32\u0e33\u0e40-\u0e46\u0e81\u0e82\u0e84\u0e87\u0e88\u0e8a\u0e8d\u0e94-\u0e97\u0e99-\u0e9f\u0ea1-\u0ea3\u0ea5\u0ea7\u0eaa\u0eab\u0ead-\u0eb0\u0eb2\u0eb3\u0ebd\u0ec0-\u0ec4\u0ec6\u0edc-\u0edf\u0f00\u0f40-\u0f47\u0f49-\u0f6c\u0f88-\u0f8c\u1000-\u102a\u103f\u1050-\u1055\u105a-\u105d\u1061\u1065\u1066\u106e-\u1070\u1075-\u1081\u108e\u10a0-\u10c5\u10c7\u10cd\u10d0-\u10fa\u10fc-\u1248\u124a-\u124d\u1250-\u1256\u1258\u125a-\u125d\u1260-\u1288\u128a-\u128d\u1290-\u12b0\u12b2-\u12b5\u12b8-\u12be\u12c0\u12c2-\u12c5\u12c8-\u12d6\u12d8-\u1310\u1312-\u1315\u1318-\u135a\u1380-\u138f\u13a0-\u13f4\u1401-\u166c\u166f-\u167f\u1681-\u169a\u16a0-\u16ea\u1700-\u170c\u170e-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176c\u176e-\u1770\u1780-\u17b3\u17d7\u17dc\u1820-\u1877\u1880-\u18a8\u18aa\u18b0-\u18f5\u1900-\u191c\u1950-\u196d\u1970-\u1974\u1980-\u19ab\u19c1-\u19c7\u1a00-\u1a16\u1a20-\u1a54\u1aa7\u1b05-\u1b33\u1b45-\u1b4b\u1b83-\u1ba0\u1bae\u1baf\u1bba-\u1be5\u1c00-\u1c23\u1c4d-\u1c4f\u1c5a-\u1c7d\u1ce9-\u1cec\u1cee-\u1cf1\u1cf5\u1cf6\u1d00-\u1dbf\u1e00-\u1f15\u1f18-\u1f1d\u1f20-\u1f45\u1f48-\u1f4d\u1f50-\u1f57\u1f59\u1f5b\u1f5d\u1f5f-\u1f7d\u1f80-\u1fb4\u1fb6-\u1fbc\u1fbe\u1fc2-\u1fc4\u1fc6-\u1fcc\u1fd0-\u1fd3\u1fd6-\u1fdb\u1fe0-\u1fec\u1ff2-\u1ff4\u1ff6-\u1ffc\u2071\u207f\u2090-\u209c\u2102\u2107\u210a-\u2113\u2115\u2119-\u211d\u2124\u2126\u2128\u212a-\u212d\u212f-\u2139\u213c-\u213f\u2145-\u2149\u214e\u2183\u2184\u2c00-\u2c2e\u2c30-\u2c5e\u2c60-\u2ce4\u2ceb-\u2cee\u2cf2\u2cf3\u2d00-\u2d25\u2d27\u2d2d\u2d30-\u2d67\u2d6f\u2d80-\u2d96\u2da0-\u2da6\u2da8-\u2dae\u2db0-\u2db6\u2db8-\u2dbe\u2dc0-\u2dc6\u2dc8-\u2dce\u2dd0-\u2dd6\u2dd8-\u2dde\u2e2f\u3005\u3006\u3031-\u3035\u303b\u303c\u3041-\u3096\u309d-\u309f\u30a1-\u30fa\u30fc-\u30ff\u3105-\u312d\u3131-\u318e\u31a0-\u31ba\u31f0-\u31ff\u3400-\u4db5\u4e00-\u9fcc\ua000-\ua48c\ua4d0-\ua4fd\ua500-\ua60c\ua610-\ua61f\ua62a\ua62b\ua640-\ua66e\ua67f-\ua697\ua6a0-\ua6e5\ua717-\ua71f\ua722-\ua788\ua78b-\ua78e\ua790-\ua793\ua7a0-\ua7aa\ua7f8-\ua801\ua803-\ua805\ua807-\ua80a\ua80c-\ua822\ua840-\ua873\ua882-\ua8b3\ua8f2-\ua8f7\ua8fb\ua90a-\ua925\ua930-\ua946\ua960-\ua97c\ua984-\ua9b2\ua9cf\uaa00-\uaa28\uaa40-\uaa42\uaa44-\uaa4b\uaa60-\uaa76\uaa7a\uaa80-\uaaaf\uaab1\uaab5\uaab6\uaab9-\uaabd\uaac0\uaac2\uaadb-\uaadd\uaae0-\uaaea\uaaf2-\uaaf4\uab01-\uab06\uab09-\uab0e\uab11-\uab16\uab20-\uab26\uab28-\uab2e\uabc0-\uabe2\uac00-\ud7a3\ud7b0-\ud7c6\ud7cb-\ud7fb\uf900-\ufa6d\ufa70-\ufad9\ufb00-\ufb06\ufb13-\ufb17\ufb1d\ufb1f-\ufb28\ufb2a-\ufb36\ufb38-\ufb3c\ufb3e\ufb40\ufb41\ufb43\ufb44\ufb46-\ufbb1\ufbd3-\ufd3d\ufd50-\ufd8f\ufd92-\ufdc7\ufdf0-\ufdfb\ufe70-\ufe74\ufe76-\ufefc\uff21-\uff3a\uff41-\uff5a\uff66-\uffbe\uffc2-\uffc7\uffca-\uffcf\uffd2-\uffd7\uffda-\uffdc]";
String._unicodeNlPattern = "[\u16ee-\u16f0\u2160-\u2182\u2185-\u2188\u3007\u3021-\u3029\u3038-\u303a\ua6e6-\ua6ef]";
String._unicodeMnPattern = "[\u0300-\u036f\u0483-\u0487\u0591-\u05bd\u05bf\u05c1\u05c2\u05c4\u05c5\u05c7\u0610-\u061a\u064b-\u065f\u0670\u06d6-\u06dc\u06df-\u06e4\u06e7\u06e8\u06ea-\u06ed\u0711\u0730-\u074a\u07a6-\u07b0\u07eb-\u07f3\u0816-\u0819\u081b-\u0823\u0825-\u0827\u0829-\u082d\u0859-\u085b\u08e4-\u08fe\u0900-\u0902\u093a\u093c\u0941-\u0948\u094d\u0951-\u0957\u0962\u0963\u0981\u09bc\u09c1-\u09c4\u09cd\u09e2\u09e3\u0a01\u0a02\u0a3c\u0a41\u0a42\u0a47\u0a48\u0a4b-\u0a4d\u0a51\u0a70\u0a71\u0a75\u0a81\u0a82\u0abc\u0ac1-\u0ac5\u0ac7\u0ac8\u0acd\u0ae2\u0ae3\u0b01\u0b3c\u0b3f\u0b41-\u0b44\u0b4d\u0b56\u0b62\u0b63\u0b82\u0bc0\u0bcd\u0c3e-\u0c40\u0c46-\u0c48\u0c4a-\u0c4d\u0c55\u0c56\u0c62\u0c63\u0cbc\u0cbf\u0cc6\u0ccc\u0ccd\u0ce2\u0ce3\u0d41-\u0d44\u0d4d\u0d62\u0d63\u0dca\u0dd2-\u0dd4\u0dd6\u0e31\u0e34-\u0e3a\u0e47-\u0e4e\u0eb1\u0eb4-\u0eb9\u0ebb\u0ebc\u0ec8-\u0ecd\u0f18\u0f19\u0f35\u0f37\u0f39\u0f71-\u0f7e\u0f80-\u0f84\u0f86\u0f87\u0f8d-\u0f97\u0f99-\u0fbc\u0fc6\u102d-\u1030\u1032-\u1037\u1039\u103a\u103d\u103e\u1058\u1059\u105e-\u1060\u1071-\u1074\u1082\u1085\u1086\u108d\u109d\u135d-\u135f\u1712-\u1714\u1732-\u1734\u1752\u1753\u1772\u1773\u17b4\u17b5\u17b7-\u17bd\u17c6\u17c9-\u17d3\u17dd\u180b-\u180d\u18a9\u1920-\u1922\u1927\u1928\u1932\u1939-\u193b\u1a17\u1a18\u1a56\u1a58-\u1a5e\u1a60\u1a62\u1a65-\u1a6c\u1a73-\u1a7c\u1a7f\u1b00-\u1b03\u1b34\u1b36-\u1b3a\u1b3c\u1b42\u1b6b-\u1b73\u1b80\u1b81\u1ba2-\u1ba5\u1ba8\u1ba9\u1bab\u1be6\u1be8\u1be9\u1bed\u1bef-\u1bf1\u1c2c-\u1c33\u1c36\u1c37\u1cd0-\u1cd2\u1cd4-\u1ce0\u1ce2-\u1ce8\u1ced\u1cf4\u1dc0-\u1de6\u1dfc-\u1dff\u20d0-\u20dc\u20e1\u20e5-\u20f0\u2cef-\u2cf1\u2d7f\u2de0-\u2dff\u302a-\u302d\u3099\u309a\ua66f\ua674-\ua67d\ua69f\ua6f0\ua6f1\ua802\ua806\ua80b\ua825\ua826\ua8c4\ua8e0-\ua8f1\ua926-\ua92d\ua947-\ua951\ua980-\ua982\ua9b3\ua9b6-\ua9b9\ua9bc\uaa29-\uaa2e\uaa31\uaa32\uaa35\uaa36\uaa43\uaa4c\uaab0\uaab2-\uaab4\uaab7\uaab8\uaabe\uaabf\uaac1\uaaec\uaaed\uaaf6\uabe5\uabe8\uabed\ufb1e\ufe00-\ufe0f\ufe20-\ufe26]";
String._unicodeMcPattern = "[\u0903\u093b\u093e-\u0940\u0949-\u094c\u094e\u094f\u0982\u0983\u09be-\u09c0\u09c7\u09c8\u09cb\u09cc\u09d7\u0a03\u0a3e-\u0a40\u0a83\u0abe-\u0ac0\u0ac9\u0acb\u0acc\u0b02\u0b03\u0b3e\u0b40\u0b47\u0b48\u0b4b\u0b4c\u0b57\u0bbe\u0bbf\u0bc1\u0bc2\u0bc6-\u0bc8\u0bca-\u0bcc\u0bd7\u0c01-\u0c03\u0c41-\u0c44\u0c82\u0c83\u0cbe\u0cc0-\u0cc4\u0cc7\u0cc8\u0cca\u0ccb\u0cd5\u0cd6\u0d02\u0d03\u0d3e-\u0d40\u0d46-\u0d48\u0d4a-\u0d4c\u0d57\u0d82\u0d83\u0dcf-\u0dd1\u0dd8-\u0ddf\u0df2\u0df3\u0f3e\u0f3f\u0f7f\u102b\u102c\u1031\u1038\u103b\u103c\u1056\u1057\u1062-\u1064\u1067-\u106d\u1083\u1084\u1087-\u108c\u108f\u109a-\u109c\u17b6\u17be-\u17c5\u17c7\u17c8\u1923-\u1926\u1929-\u192b\u1930\u1931\u1933-\u1938\u19b0-\u19c0\u19c8\u19c9\u1a19-\u1a1b\u1a55\u1a57\u1a61\u1a63\u1a64\u1a6d-\u1a72\u1b04\u1b35\u1b3b\u1b3d-\u1b41\u1b43\u1b44\u1b82\u1ba1\u1ba6\u1ba7\u1baa\u1bac\u1bad\u1be7\u1bea-\u1bec\u1bee\u1bf2\u1bf3\u1c24-\u1c2b\u1c34\u1c35\u1ce1\u1cf2\u1cf3\u302e\u302f\ua823\ua824\ua827\ua880\ua881\ua8b4-\ua8c3\ua952\ua953\ua983\ua9b4\ua9b5\ua9ba\ua9bb\ua9bd-\ua9c0\uaa2f\uaa30\uaa33\uaa34\uaa4d\uaa7b\uaaeb\uaaee\uaaef\uaaf5\uabe3\uabe4\uabe6\uabe7\uabe9\uabea\uabec]";
String._unicodeNdPattern = "[\u0030-\u0039\u0660-\u0669\u06f0-\u06f9\u07c0-\u07c9\u0966-\u096f\u09e6-\u09ef\u0a66-\u0a6f\u0ae6-\u0aef\u0b66-\u0b6f\u0be6-\u0bef\u0c66-\u0c6f\u0ce6-\u0cef\u0d66-\u0d6f\u0e50-\u0e59\u0ed0-\u0ed9\u0f20-\u0f29\u1040-\u1049\u1090-\u1099\u17e0-\u17e9\u1810-\u1819\u1946-\u194f\u19d0-\u19d9\u1a80-\u1a89\u1a90-\u1a99\u1b50-\u1b59\u1bb0-\u1bb9\u1c40-\u1c49\u1c50-\u1c59\ua620-\ua629\ua8d0-\ua8d9\ua900-\ua909\ua9d0-\ua9d9\uaa50-\uaa59\uabf0-\uabf9\uff10-\uff19]";
String._unicodePcPattern = "[\u005f\u203f\u2040\u2054\ufe33\ufe34\ufe4d-\ufe4f\uff3f]";
String._jsUnicodeEscapeSequencePattern = "\\u[0-9A-Fa-f]{4}";
String._jsIdentifierStartPattern = "(?:" + String._unicodeLPattern + "|[$_]|" + String._jsUnicodeEscapeSequencePattern + ")";
String._jsUnicodeCombiningMarkPattern = "(?:" + String._unicodeMnPattern + "|" + String._unicodeMcPattern + ")";
String._jsUnicodeDigitPattern = String._unicodeNdPattern;
String._jsUnicodeConnectorPunctuationPattern = String._unicodePcPattern;
// Zero-width non-joiner
String._zwnjPattern = "\u200c";
// Zero-width joiner
String._zwjPattern = "\u200d";
String._jsIdentifierPartPattern = "(?:" + String._jsIdentifierStartPattern + "|" + String._jsUnicodeCombiningMarkPattern + "|" + String._jsUnicodeDigitPattern + "|" + String._jsUnicodeConnectorPunctuationPattern + "|" + String._zwnjPattern + "|" + String._zwjPattern + ")";
String._jsIdentifierNamePattern = "^(?:" + String._jsIdentifierStartPattern + String._jsIdentifierPartPattern + "*)$";
String._jsIdentifierNameRegExp = new RegExp(String._jsIdentifierNamePattern);

String._jsKeywordPattern = "(?:break|case|catch|continue|debugger|default|delete|do|else|finally|for|function|if|in|instanceof|new|return|switch|this|throw|try|typeof|var|void|while|with)";
String._jsFutureReservedWordPattern = "(?:class|const|enum|export|extends|import|super)";
String._jsStrictModeFutureReservedWordPattern = "(?:implements|interface|let|package|private|protected|public|static|yield)";
String._jsReservedWordRegExp = new RegExp("^(?:" + String._jsKeywordPattern + "|" + String._jsFutureReservedWordPattern + "|" + String._jsStrictModeFutureReservedWordPattern + "|null|true|false)$");

String._htmlStringToStringTagBodyPattern = '(?:[^"\'>]|"[^"]*"|\'[^\']*\')*';

isc.addMethods(String, {

    //>	@classMethod	String.asSource()
    //			Static method to return a new String that, evaluated as source code, would produce 
    //          the passed in String's value.
    //		@group	dynamicString
    //		@param	string  (string)    string to convert
    //		@return			(string)	converted string
    //<
    
    asSource : function (string, singleQuote) {
        if (!isc.isA.String(string)) string = ""+string;

        var quoteRegex = singleQuote ? String._singleQuoteRegex : String._doubleQuoteRegex,
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

    // Escapes <code>str</code> as an +externalLink{http://www.w3.org/TR/xml11/#NT-AttValue,XML AttValue}.
    // @param str (string) the string to escape.
    // @param [doubleQuote] (boolean) <code>true</code> to use double-quotes; otherwise, use single-quotes.
    // @param [includeOuterQuotes] (boolean) <code>true</code> to prepend and append the outer
    // quote char; otherwise, the outer quote char is <em>not</em> prepended and appended in the resulting
    // string.
    _attValueSpecialCharsRegex: new RegExp("[<&\"']", ""),
    asAttValue : function (str, doubleQuote, includeOuterQuotes) {
        var inner;
        if (str == null) {
            inner = isc.emptyString;

        } else {
            str = String(str);

            // If there aren't any potentially special characters present in the string, then we can
            // skip the replace() calls.
            if (!this._attValueSpecialCharsRegex.test(str)) {
                inner = str;
            } else {
                var quoteRegex,
                    quoteReplacement;
                if (doubleQuote) {
                    quoteRegex = this._doubleQuoteRegex;
                    quoteReplacement = isc._$quot;
                } else {
                    quoteRegex = this._singleQuoteRegex;
                    quoteReplacement = isc._$39;
                }
                inner = str.replace(isc._RE_amp, isc._$amp)
                           .replace(isc._RE_lt, isc._$lt)
                           .replace(quoteRegex, quoteReplacement);
            }
        }

        if (includeOuterQuotes) {
            var outerQuote = doubleQuote ? "\"" : "'";
            return outerQuote + inner + outerQuote;
        } else {
            return inner;
        }
    },

    
    _cssDeclarationValueUnsafeCharsRegex: new RegExp("[^ _.,!#%a-zA-Z0-9-]+", ""),
    _asCSSDeclarationValue : function (str) {
        if (str == null) {
            return "";
        } else {
            return String(str).replace(this._cssDeclarationValueUnsafeCharsRegex, "");
        }
    },

    
    _fontFamilyEscapes: {
        "\b": "&#x5C;08&#x20;",
        "\t": "&#x5C;09&#x20;",
        "\n": "&#x5C;0A&#x20;",
        "\f": "&#x5C;0C&#x20;",
        "\r": "&#x5C;0D&#x20;",
        "'": "&#x5C;27&#x20;",
        "\"": "&#x5C;22&#x20;",
        "\\": "&#x5C;5C&#x20;",
        " ": "&#x20;",
        "&": "&#x5C;26&#x20;",
        "/": "&#x5C;2F&#x20;",
        "<": "&#x5C;3C&#x20;",
        ">": "&#x5C;3E&#x20;",
        "\u2028": "&#x5C;002028&#x20;",
        "\u2029": "&#x5C;002029&#x20;"
    },
    _fontFamilyEscapeRegExp: /[\b\t\n\f\r'"\\ &\/<>\u2028\u2029]/g,
    _fontFamilyEscapeReplace : function (c) {
        return String._fontFamilyEscapes[c];
    },
    _asFontFamilyValue : function (str) {
        var emptyString = isc.emptyString;
        if (str == null) {
            return emptyString;
        } else {
            str = String(str);
            if (str == emptyString) {
                return emptyString;
            } else {
                var r = String._fontFamilyEscapeRegExp;
                return (r.test(str) ? str.replace(r, String._fontFamilyEscapeReplace) : str);
            }
        }
    },

    // Removes comments, <script>- and <style>-looking blocks, and then removes tags. This can
    // be used to convert an HTMLString value to an attribute like 'aria-label' or the <input>
    // 'placeholder' attribute which do not support HTML content.
    //
    // NOTE: This is not to be used for security purposes (such as to avoid XSS). This function
    // assumes that the HTML is from a trusted source (HTMLString type).
    //
    // The resulting string will probably need to be run through asAttValue().
    
    _htmlStringToStringRegExp: new RegExp('<(?:' +
                                 // Comment
                                 '!--(?:(?:-*[^->])*--+|-?)' +
                                 // Special "raw text" elements
                                 // Use a "word boundary" assertion \b to avoid matching, for example, <scriptStuff>.
                                 '|script\\b' + String._htmlStringToStringTagBodyPattern + '>[\\s\\S]*?</script\\s*' +
                                 '|style\\b' + String._htmlStringToStringTagBodyPattern + '>[\\s\\S]*?</style\\s*' +
                                 // Regular tag name. HTML elements all have names that only use alphanumeric ASCII characters:
                                 // http://www.whatwg.org/specs/web-apps/current-work/multipage/syntax.html#syntax-tag-name
                                 '|/?[a-z0-9]' +
                                 String._htmlStringToStringTagBodyPattern +
                                 ')>', "ig"),
    htmlStringToString : function (html) {
        if (html == null) return "";

        html = String(html).replace(/<BR\s*>/ig, "\n");

        var oldHtml;
        do {
            oldHtml = html;
            html = html.replace(this._htmlStringToStringRegExp, "");
        } while (html.length != oldHtml.length);

        // If the HTMLString happens to end in a partial tag, remove that.
        // Exception: If there is whitespace after the '<' or it is preceded by another '<',
        // then keep it. E.g.: '<< Less', '< $monthName'
        var pos = html.search(/<(?!\s)[^>]*$/);
        if (pos >= 0 && (pos == html.length - 1 || html.charCodeAt(pos + 1) != 60)) html = html.substring(0, pos);

        var text;

        // Unescape HTML entities
        
        if (html.indexOf("&") >= 0) {
            var spanElem = document.createElement("span");
            spanElem.innerHTML = html;
            text = (spanElem.innerText || spanElem.textContent);
        } else {
            text = html;
        }

        return text;
    },

    //> @classMethod String.isValidID()
    // Tests whether the given string is a valid JavaScript identifier.
    //
    // @param string (string) the string to test.
    // @return (boolean) true if string is a valid JavaScript identifier; false otherwise.
    // @visibility external
    //<
    isValidID : function (string) {
        if (!isc.isA.String(string)) return false;
        // A JavaScript Identifier is an IdentifierName that is not a ReservedWord. (ECMA-262 Section 7.6)
        return (string.search(String._jsIdentifierNameRegExp) != -1 &&
                string.search(String._jsReservedWordRegExp) == -1);
    }
});






isc.addMethods(isc, {

// isc.replaceAll() [string helper]
//  Replace all occurances of 'find' string with 'replacement' string.
//  Uses a native method so is very efficient (and easier to use than grep).
replaceAll : function (source, find, replacement) {
	return source.split(find).join(replacement);
},

// isc.contains() [string helper]
//  Returns true if this string contains the specified substring.
contains : function (string1, substring) {
    if (string1 == null) return false;

	return string1.indexOf(substring) > -1;
},

// isc.startsWith() [string helper]
//  Returns true if this string starts with another string.
startsWith : function (string1, substring) {
    if (string1 == null) return false;

    return (string1.lastIndexOf(substring, 0) == 0);
},


// isc.endsWith() [string helper]
//  Returns true if this string ends with another string.
endsWith : function (string1, substring) {
    if (string1 == null) return false;

    var startPos = string1.length - substring.length;
    if (startPos < 0) return false; // substring longer than main string
    return (string1.indexOf(substring, startPos) == startPos);
},

// escapes special characters in XML values - so called 'unparsed data'
// " -> &quot;
// ' -> &apos;
// & -> &amp;
// < -> &lt;
// > -> &gt;
// \r -> &x000D;
//
// NOTE: in an XHTML document, this is baseline functionality.
// 
// NOTE: leave these functions at the end of the file because the quotes within regex's hose the
// obfuscator, causing it to continue to end of file
makeXMLSafe : function (string, amp, lt, gt, quot, apos, cr) {
    if (string == null) return isc.emptyString;
    else if (!isc.isA.String(string)) string = string.toString();

	if (amp != false) string = string.replace(this._RE_amp, this._$amp);
	if (lt != false) string = string.replace(this._RE_lt, this._$lt);
	if (gt != false) string = string.replace(this._RE_gt, this._$gt);
	if (quot != false) string = string.replace(String._doubleQuoteRegex, this._$quot);
    if (apos != false) string = string.replace(String._singleQuoteRegex, this._$apos);
    if (cr != false) string = string.replace(this._RE_cr, this._$escapedCR);
    return string;
},
xmlAttributeEscapeLF:true,
makeXMLSafeAttribute : function (string, amp, lt, gt, quot, apos, cr, lf) {
    // Ambiguity in spec/implementation on how to encode a LF (\n) in an attribute:
    //
    // http://stackoverflow.com/questions/2004386/how-to-save-newlines-in-xml-attribute
    //
    // Empirical testing with Reify shows that simply not escaping LFs in attributes doesn't work - we
    // get a space instead. This is exemplied in the breakage of the following autoTest due to
    // our marking FormItem.defaultValue as xmlAttribute="true" and then feeding a defaultValue
    // with a line break to the BMML import logic and expecting it to be preserved:
    //
    // http://localhost.smartclient.com:15011/isomorphic/QA/VisualBuilder/bmmlImporter/InVivoRequestsGridBmmlTest.test.jsp
    //
    // To be super safe we have this separate makeXMLSafeAttribute() method that
    // calls makeXMLSafe and then additionally escapes LFs and then use that anytime we want to
    // encode an attribute. 
    string = this.makeXMLSafe(string, amp, lt, gt, quot, apos, cr);
    if (lf != false && isc.xmlAttributeEscapeLF) string = string.replace(this._RE_lf, this._$escapedLF);
    return string;
},
_$amp:"&amp;",
_$lt:"&lt;",
_$gt:"&gt;",
_$quot:"&quot;",
_$apos:"&apos;",
_$39:"&#39;",
_$escapedCR:"&#x000D;",
_$escapedLF:"&#x000A;",
_RE_amp:/&/g,
_RE_lt:/</g,
_RE_gt:/>/g,
_RE_cr:/\r/g,
_RE_lf:/\n/g,

makeCDATA : function (string) {
    return "<![CDATA["+string.replace(/\]\]>/, "]]<![CDATA[>")+"]]>";
}

});
