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
//> @class URIBuilder
//<
isc.defineClass("URIBuilder").addClassMethods({

create : function (uri) {
    if (isc.isA.String(uri)) return this.Super("create", [{ uri: uri }], arguments);
    else return this.Super("create", arguments);
}

});

isc.URIBuilder.addProperties({

//> @attr URIBuilder.uri (URI : "" : IR)
// The current URI.
//<
uri: ""

});

isc.URIBuilder.addMethods({

init : function () {
    this.Super("init", arguments);
    if (this.uri == null) this.uri = "";
    else this.uri = String(this.uri);
    this._qsStartPos = this._getQsStartPos();
},

_getQsStartPos : function () {
    var uri = this.uri;
    var hashStartPos = uri.indexOf('#');
    if (hashStartPos == -1) {
        return uri.indexOf('?');
    } else {
        var qsStartPos = uri.indexOf('?');
        if (qsStartPos >= hashStartPos) {
            return -1;
        }
        return qsStartPos;
    }
},

appendPath : function (path) {
    if (path == null || path.length == 0) return;

    var encodedPath = encodeURI(path).replace('?', encodeURIComponent('?')).replace('#', encodeURIComponent('#'));

    if (this.uri.length == 0) {
        this.uri = encodedPath;
        
    } else {
        var pathEndPos = this._qsStartPos;
        if (pathEndPos == -1) {
            pathEndPos = this.uri.indexOf('#');
        }
        if (pathEndPos == -1) {
            pathEndPos = this.uri.length;
        }

        var tmp = this.uri.substring(0, pathEndPos);
        if (pathEndPos > 0 && this.uri.charAt(pathEndPos - 1) != '/' && path.charAt(0) != '/') {
            tmp += '/';
        }
        tmp += encodedPath;
        tmp += this.uri.substring(pathEndPos);
        this.uri = tmp;
        this._qsStartPos = this._getQsStartPos();
    }
},

_indexOfQueryParam : function (encodedName, pos) {
    if (pos == null) pos = this._qsStartPos;
    else pos = Math.max(this._qsStartPos, pos);

    if (pos < 0) return -1;

    var hashStartPos = this.uri.indexOf('#', this._qsStartPos + 1);
    var qsEndPos = hashStartPos == -1 ? this.uri.length : hashStartPos;
    for (; pos < qsEndPos && (pos = this.uri.indexOf(encodedName, pos)) != -1; pos += encodedName.length) {
        
        if (this.uri.charAt(pos - 1) == '&' || this.uri.charAt(pos - 1) == '?') {
            var pos2 = pos + encodedName.length;
            if (pos2 <= qsEndPos && (pos2 == qsEndPos ||
                                     this.uri.charAt(pos2) == '=' ||
                                     this.uri.charAt(pos2) == '&'))
            {
                return pos;
            }
        }
    }
    return -1;
},

containsQueryParam : function (name) {
    return name != null && this._indexOfQueryParam(encodeURIComponent(name)) != -1;
},

getQueryValue : function (paramName, firstOnly) {
    var encodedParamName = encodeURIComponent(paramName);
    var pos = this._indexOfQueryParam(encodedParamName);
    if (pos < 0) return null;

    var hashStartPos = this.uri.indexOf('#', this._qsStartPos == -1 ? 0 : this._qsStartPos + 1);
    if (hashStartPos == -1) hashStartPos = this.uri.length;

    var ampPos = this.uri.indexOf('&', pos + encodedParamName.length);
    var encodedValueStartPos = pos + (this.uri[pos + encodedParamName.length] === '=' ? encodedParamName.length + 1 : encodedParamName.length);
    var encodedValueEndPos = ampPos >= 0 ? Math.min(ampPos, hashStartPos) : hashStartPos;
    var encodedValue = this.uri.substring(encodedValueStartPos, encodedValueEndPos);

    if (firstOnly || (pos = this._indexOfQueryParam(encodedParamName, encodedValueEndPos)) < 0) {
        return decodeURIComponent(encodedValue);
    } else {
        var values = [decodeURIComponent(encodedValue)];

        do {
            ampPos = this.uri.indexOf('&', pos + encodedParamName.length);
            encodedValueStartPos = pos + (this.uri[pos + encodedParamName.length] === '=' ? encodedParamName.length + 1 : encodedParamName.length);
            encodedValueEndPos = ampPos >= 0 ? Math.min(ampPos, hashStartPos) : hashStartPos;
            encodedValue = this.uri.substring(encodedValueStartPos, encodedValueEndPos);
            values.add(decodeURIComponent(encodedValue));

            pos = this._indexOfQueryParam(encodedParamName, encodedValueEndPos);
        } while (pos >= 0);
        return values;
    }
},

_appendQueryParamHelper : function (prefix, value) {
    if (value == null) return;
    if (isc.isA.String(value)) {
        var hashStartPos = this.uri.indexOf('#', this._qsStartPos == -1 ? 0 : this._qsStartPos + 1);
        if (hashStartPos == -1) hashStartPos = this.uri.length;

        var tmp = this.uri.substring(0, hashStartPos);

        if (this._qsStartPos == -1) {
            this._qsStartPos = hashStartPos;
            tmp += '?';
        } else tmp += '&';
        tmp += prefix;
        tmp += encodeURIComponent(value.toString());
        tmp += this.uri.substring(hashStartPos);
        this.uri = tmp;
        
    } else if (isc.isAn.Array(value)) {
        for (var i = 0; i < value.length; ++i) {
            this._appendQueryParamHelper(prefix, value[i]);
        }
    } else {
        this._appendQueryParamHelper(prefix, String(value));
    }
},

appendQueryParam : function (name, value) {
    if (name == null) return;

    var encodedName = encodeURIComponent(name);
    var prefix = encodedName + '=';
    this._appendQueryParamHelper(prefix, value);
},

setQueryParam : function (name, value) {
    var encodedName = encodeURIComponent(name);
    var prefix = encodedName + '=';

    if (this._qsStartPos != -1) {
        var hashStartPos = this.uri.indexOf('#', this._qsStartPos + 1);
        var qsEndPos = hashStartPos == -1 ? this.uri.length : hashStartPos;
        var sb = "";
        sb += this.uri.substring(0, this._qsStartPos);
        var prevPos = this._qsStartPos, pos = this._qsStartPos;
        while (pos < qsEndPos && (pos = this.uri.indexOf(prefix, pos)) != -1) {
            
            var ampPos = this.uri.indexOf('&', pos + prefix.length);

            if (this.uri.charAt(pos - 1) == '&' || this.uri.charAt(pos - 1) == '?') {
                sb += this.uri.substring(prevPos, pos);
                if (ampPos != -1 && ampPos < qsEndPos) {
                    pos = ampPos + 1;
                } else {
                    pos = qsEndPos;
                    sb = sb.substring(0, sb.length - 1);
                }
            } else {
                pos = (ampPos != -1 && ampPos < qsEndPos ? ampPos + 1 : qsEndPos);
                sb += this.uri.substring(prevPos, pos);
            }
            prevPos = pos;
        }
        sb += this.uri.substring(prevPos, this.uri.length);
        this.uri = sb;
        this._qsStartPos = this._getQsStartPos();
    }

    this.appendQueryParam(name, value);
}

});
