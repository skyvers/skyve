/*
 * Isomorphic SmartClient
 * Version v11.0p_2016-03-31 (2016-03-31)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */
//> @type XMLDocument
// XMLDocument is the "parsed" or object form of XML, which allows XML to be navigated as
// a tree of nodes with attributes, namespaces and other metadata, as opposed to being
// manipulated as just a String.
// <P> 
// XMLDocument is a native object supplied directly by the browser.  The SmartClient-supported
// interfaces for this object are methods that take an XMLDocument as an argument (such as
// +link{XMLTools.selectNodes()}).  If you want to retrieve XML data and display it in a
// SmartClient component, read about +link{group:clientDataIntegration,XML Data Binding}.  To
// extract data as JavaScript Objects from XML, see +link{XMLTools.toJS()}.  Direct
// manipulation of XMLDocument is subject to cross-browser inconsistencies, bugs, memory leaks
// and performance issues.
//
// @visibility xmlBinding
//<

//> @type XMLElement
// An XMLElement represents one complete XML tag, including any subelements contained between
// the start and end tags.
// <P>
// XMLElement is a native object supplied directly by the browser.  The SmartClient-supported
// interfaces for this object include methods that take an XMLElement as an argument (such as
// +link{XMLTools.selectNodes()}).  If you want to retrieve XML data and display it in a
// SmartClient component, read about +link{group:clientDataIntegration,XML Data Binding}.  To
// extract data as JavaScript Objects from XML, see +link{XMLTools.toJS()}.  Direct
// manipulation of XMLElements objects is subject to cross-browser inconsistencies, bugs,
// memory leaks and performance issues.
//
// @visibility xmlBinding
//< 




isc.defineClass("XMLDoc").addMethods({
    addPropertiesOnCreate:false,
    init : function (xmlDoc, namespaces) {
        this.nativeDoc = xmlDoc;
        this.namespaces = namespaces;
        // the most common property access
        this.documentElement = this.nativeDoc.documentElement;
    },
    hasParseError : function () {
        if (isc.Browser.isIE) {
            var parseError = this.nativeDoc.parseError;
            // IE8 now returns zero for no error
            return parseError != null && parseError != 0;
        }
        return this.nativeDoc.documentElement && 
                this.nativeDoc.documentElement.tagName == "parsererror"; //FF
    },
    addNamespaces : function (namespaces) {
        this.namespaces = this._combineNamespaces(namespaces);
        // HACK: in the Comm watcher, for experimenting with XPath selection against XML
        // replies, it's key that any namespaces added to the XMLDoc are available.
        if (this._responseID) {
            var xmlResponse = isc.xml.xmlResponses.find("ID", this._responseID);
            if (xmlResponse) xmlResponse.xmlNamespaces = this.namespaces;
            //this.logWarn("looked up response: " + this._responseID + 
            //             " and grabbed namespaces : " + this.echo(this.namespaces));
        }
    },
    _combineNamespaces : function (namespaces) {
        if (namespaces == null) return this.namespaces;
        if (this.namespaces == null) return namespaces;
        return isc.addProperties({}, this.namespaces, namespaces);
    },
    // convenience methods
    selectNodes : function (xPath, namespaces, single) {
        return isc.xml.selectNodes(this.nativeDoc, xPath,
                                   this._combineNamespaces(namespaces), single);
    },
    selectString : function (xPath, namespaces) {
        return isc.xml.selectString(this.nativeDoc, xPath,
                                    this._combineNamespaces(namespaces));
    },
    selectNumber : function (xPath, namespaces) {
        return isc.xml.selectNumber(this.nativeDoc, xPath,
                                    this._combineNamespaces(namespaces));
    },
    selectScalar : function (xPath, namespaces, asNumber) {
        return isc.xml.selectScalar(this.nativeDoc, xPath, 
                                    this._combineNamespaces(namespaces), asNumber);
    },
    selectScalarList : function (xPath, namespaces) {
        return isc.xml.selectScalarList(this.nativeDoc, xPath,
                                        this._combineNamespaces(namespaces));
    },

    // passthroughs (consider writePassthroughFunctions() if this expands)
    getElementById : function (id) { return this.nativeDoc.getElementById(id) },
    getElementsByTagName : function (tagName) { 
        return this.nativeDoc.getElementsByTagName(tagName) 
    }
});
isc.XMLDoc.getPrototype().toString = function () {
    return "[XMLDoc <" + this.documentElement.tagName + ">]";
};

//> @class XMLTools
// Utility methods for dealing with XML elements, XML Schema, WSDL files, XSLT, and other
// XML-related functionality.
//
// @treeLocation Client Reference/Data Binding
// @visibility external
//<
isc.defineClass("XMLTools").addClassProperties({

// Retrieval and Parsing
// ---------------------------------------------------------------------------------------



//> @classMethod XMLTools.loadXML()
// Load an XML document from the origin server or from a foreign server by relaying through the
// origin server.  An asynchronous callback provides both the XML document and raw text of the
// response.
// <P>
// Relaying through the origin server requires that the ISC HttpProxyServlet be installed and
// accessible.
// 
// @param URL      (URL)       URL to load the schema from
// @param callback (callback)  callback to fire when the XML is loaded.  Signature is
//                             callback(xmlDoc, xmlText)
// @param [requestProperties] (RPCRequest) additional properties to set on the RPCRequest
//                                         that will be issued
//
// @visibility external
//<
loadXML : function (url, callback, requestProperties) {
    requestProperties = requestProperties || {};
    requestProperties.operationType = requestProperties.operationType || "loadXML";

    this.getXMLResponse(isc.addProperties({ 
        actionURL : url, 
        httpMethod:"GET",
        callback:callback
    }, requestProperties));
},

// getXMLResponse: like rpc.sendProxied(), but parses the result as XML and gives an
// XML-specific callback of (xmlDoc,xmlText)
getXMLResponse : function (request) {
    // do an indirect callback
    request._xmlIndirectCallback = request.callback;
    request.callback = { target : this, methodName : "_getXMLResponseReply" };

    // default to POST
    request.httpMethod = request.httpMethod || "POST";

    this.logInfo("loading XML from: " + request.actionURL, "xmlComm");
    isc.rpc.sendProxied(request);
},

xmlResponses : [],
_nextResponseID : 0,
// Undocumented attribute to handle the case where we check for an empty (whitespace-only)
// response. We don't want to "trim" a massive XML doc which could be expensive, so 
// avoid the call if the text exceeds this length.
trimEmptyXMLThreshold:100,
_getXMLResponseReply : function (rpcResponse, data, rpcRequest) {
    // if we already have a structured response don't try to parse it as XML
    // XXX - unless it's a RestRequest, in which case this is normal (the structured response
    // has been broken up into response specific XML substrings by RPCManager._performTransactionReply)
    // This can occur if an operation timed out
    if (rpcResponse.isStructured && !rpcRequest.isRestRequest) {        
        this.fireCallback(rpcRequest._xmlIndirectCallback, 
                      // NOTE: request/response probably only for internal callers
                      "xmlDoc,xmlText,rpcResponse,rpcRequest", 
                      [null,null,rpcResponse,rpcRequest]);
        return;
    }
    
    var xmlText = rpcRequest.isRestRequest ? rpcResponse.results : rpcResponse.httpResponseText,
        xmlDoc = this.parseXML(xmlText);

    if (this.logIsInfoEnabled("xmlComm")) {
        this.logInfo("XML reply with text: " + 
                     (this.logIsDebugEnabled("xmlComm") ? xmlText : this.echoLeaf(xmlText)), 
                    "xmlComm");
    }

	// if the parsedXML failed parsing, set a status of -1 and fire callback
	// Exception - allow a totally empty response to continue - this is useful for
	// REST dataSources where the request kicks off server logic but the service doesn't
	// actually return meaningful response text.
	var isEmptyResponse = false;
	if (xmlText == null || 
	    (xmlText.length < this.trimEmptyXMLThreshold && xmlText.trim() == ""))
    {
        isEmptyResponse = true;
    }
    
    if(!isEmptyResponse && (!xmlDoc || xmlDoc.getElementsByTagName("parsererror").length>0)) {
        rpcResponse.status = -1;
    } else {
		// retain last 5 responses in an Array for programmatic debugging
		var responses = this.xmlResponses;

		// NOTE: with the log window permanently open, you only need to enable xmlComm to catch
		// comm responses before page load
		if (this.logIsDebugEnabled("xmlComm") || 
			(isc.Page.isLoaded() && isc.debugMaster)) 
		{
			var responseID = this._nextResponseID++;
			responses.add({
				ID : responseID,
				text : xmlText
			});
			// HACK: label the XMLDoc created from this xmlText with the id of the response - this
			// allows the XMLDoc to tack xml namespaces onto the response later
			if (xmlDoc) xmlDoc._responseID = responseID;

			// keep a limited number of responses
			if (responses.length > 10) responses.shift();
		
			// update log window if showing
            isc.debugMaster.call("window.updateCommWatcher", [responses]);
		} else {
			responses.length = 0;
		}
	}
    this.fireCallback(rpcRequest._xmlIndirectCallback, 
                      // NOTE: request/response probably only for internal callers
                      "xmlDoc,xmlText,rpcResponse,rpcRequest", 
                      [xmlDoc,xmlText,rpcResponse,rpcRequest]);
},

useIEXMLHackaround: isc.Browser.isIE,

//> @classMethod XMLTools.disableIEXMLHackaround()
// Disables an Internet Explorer-specific work around for the MSXML bug that the 'xml' namespace
// prefix cannot be explicitly declared.
// <p>
// Though redundant,
// the +externalLink{http://www.w3.org/TR/REC-xml-names/#xmlReserved,Namespaces in XML spec allows}
// XML documents to explicitly declare namespace prefix 'xml' bound to namespace name
// <code>http://www.w3.org/XML/1998/namespace</code>; e.g.
// <pre>xmlns:xml="http://www.w3.org/XML/1998/namespace"</pre>
// MSXML does not allow the 'xml' namespace prefix to be declared, and will raise the XML
// parse error: The namespace prefix is not allowed to start with the reserved string "xml".
// Microsoft has disclosed this bug as a Normative Variation in MSXML:
// +externalLink{http://msdn.microsoft.com/en-us/library/ff460535(v=vs.85).aspx}.
// A framework-level work around is used by default in +link{XMLTools.parseXML()} where if the
// string <code>xmlns:xml="http://www.w3.org/XML/1998/namespace"</code> or
// <code>xmlns:xml='http://www.w3.org/XML/1998/namespace'</code> is found in the first 1000
// characters of the <code>xmlText</code> parameter to parseXML(), then these two strings are
// removed from <code>xmlText</code> wherever they appear. This work around may be disabled by
// calling disableIEXMLHackaround() at any time before parseXML() is called.
// @visibility external
//<
disableIEXMLHackaround : function () {
    this.useIEXMLHackaround = false;
},


//                    IE6                   IE5.5                we used to use this
xmlDOMConstructors : ["MSXML2.DOMDocument", "MSXML.DOMDocument", "Microsoft.XMLDOM"],


//> @classMethod XMLTools.parseXML()
// Parse XML text into an +link{XMLDocument}.  Parse errors, if any, are reported to the log.
// <p>
// <strong>NOTE:</strong> Internet Explorer's XML parser implementation, MSXML, has a bug in its
// handling of namespace name <code>http://www.w3.org/XML/1998/namespace</code>. Though redundant,
// the +externalLink{http://www.w3.org/TR/REC-xml-names/#xmlReserved,Namespaces in XML spec allows}
// XML documents to explicitly declare namespace prefix 'xml' bound to namespace name
// <code>http://www.w3.org/XML/1998/namespace</code>; e.g.
// <pre>xmlns:xml="http://www.w3.org/XML/1998/namespace"</pre>
// MSXML does not allow the 'xml' namespace prefix to be declared, and will raise the XML
// parse error: The namespace prefix is not allowed to start with the reserved string "xml".
// Microsoft has disclosed this bug as a Normative Variation in MSXML:
// +externalLink{http://msdn.microsoft.com/en-us/library/ff460535(v=vs.85).aspx}.
// A framework-level work around is used by default where if the string
// <code>xmlns:xml="http://www.w3.org/XML/1998/namespace"</code> or
// <code>xmlns:xml='http://www.w3.org/XML/1998/namespace'</code> is found in the first 1000
// characters of <code>xmlText</code>, then these two strings are removed from <code>xmlText</code>
// wherever they appear. This work around may be disabled by calling +link{XMLTools.disableIEXMLHackaround()}
// at any time before parseXML() is called.
//
// @param xmlText (String) XML text to be parsed
// @return (XMLDocument) resulting XMLDocument
// @visibility external
//<

mozAnchorBug : isc.Browser.isMoz && (isc.Browser.geckoVersion < 20080205)
                && window.location.href.indexOf("#") != -1,
_xmlNamespaceDeclarationRegExp: /xmlns:xml=(?:"http:\/\/www.w3.org\/XML\/1998\/namespace"|'http:\/\/www.w3.org\/XML\/1998\/namespace')/g,
parseXML : function (xml, suppressErrors) {
    if (xml == null) return; // Moz will actually throw exceptions in this case

    xml = this.trimXMLStart(xml);

    if (this.useIEXMLHackaround) {
        if (this._xmlNamespaceDeclarationRegExp.test(xml.substring(0, 1000))) {
            xml = xml.replace(this._xmlNamespaceDeclarationRegExp, "");
        }
    }

    var doc;
    if (!isc.Browser.isIE) {
        try {
            
            if ((this.mozAnchorBug || this.useAnchorWorkaround) && 
                this.useAnchorWorkaround !== false) 
            {
                var iframeHTML = "<IFRAME STYLE='position:absolute;visibility:hidden;top:-1000px'"
                                +" ID='isc_parseXMLFrame'></IFRAME>";
                if (!isc.Page.isLoaded()) {
                    document.write(iframeHTML);
                } else {
                    isc.Element.insertAdjacentHTML(document.getElementsByTagName("body")[0], "beforeEnd", iframeHTML)
                }
                var iframe = document.getElementById("isc_parseXMLFrame");
                var iframeWindow = iframe.contentWindow;

                
                window.isc.xmlSource = xml;
                iframeWindow.location.href = "javascript:top.isc.parsedXML="
                    + "new top.isc.XMLTools.getXMLParser().parseFromString(top.isc.xmlSource, 'text/xml')";
                doc = window.isc.parsedXML;
    
                isc.xmlSource = isc.parsedXML = null;

                // remove the iframe
                iframe.parentNode.removeChild(iframe);
            } else {
                doc = this.getXMLParser().parseFromString(xml, "text/xml");
            }
        } catch (e) {
            if (!suppressErrors) this._logParseError(this.echo(e));
            return null;
        }
        
        if (!suppressErrors && doc.documentElement && 
             doc.documentElement.tagName == "parsererror") 
        {
            this._logParseError(doc.documentElement.textContent);
            return null;
        }

        return isc.XMLDoc.create(doc);
    }

    doc = this.getXMLParser();
    if (!doc) {
        this._warnIfNativeXMLUnavailable("XMLTools.parseXML()");
        return;
    }

    doc.loadXML(xml);
    if (doc.parseError != 0) {
        var error = doc.parseError;
        if (!suppressErrors) {
            this._logParseError(
                "\rReason: " + error.reason +
                "Line number: " + error.line + ", character: " + error.linepos +
                "\rLine contents: " + error.srcText + 
                (!isc.isA.emptyString(error.url) ? "\rSource URL: " + error.url : ""));
        }
        return null;
    }
    return isc.XMLDoc.create(doc);
},

// NOTE: don't obfuscate "trimXMLStart", used by dev console
trimXMLStart : function (xml) {

    
    if (xml.indexOf("<?xml") != -1) 
    {
        
        var match = xml.match(new RegExp("^\\s*<\\?[^?]*\\?>"));
        if (match) {
    		xml = xml.substring(match[0].length);
	    	//this.logWarn("match is: " + this.echoAll(match) + ", trimming by: " + match[0].length);
    	}
    }

    
    if (isc.Browser.isIE && xml.indexOf("<!DOCTYPE") != -1) {
        var match = xml.match(new RegExp("^\\s*<!DOCTYPE .*>"));
        if (match) {
    		xml = xml.substring(match[0].length);
	    	//this.logWarn("match is: " + this.echoAll(match) + ", trimming by: " + match[0].length);
    	}
    }
    return xml;
},

_logParseError : function (errorText, xml) {
    this.logWarn("Error parsing XML: " + errorText +
                 (this.logIsDebugEnabled("parseXML") ? 
                  "\rXML was:\r" + xml + "\rTrace:" + this.getStackTrace() : ""), 
                 "parseXML"); 
},


getXMLParser : function () {
    // Moz/Firefox, Safari support a native DOMParser
    if (!isc.Browser.isIE) {
        if (!this._parser) this._parser = new DOMParser();
        return this._parser;
    }

    var parser;
    if (this._xmlDOMConstructor) {
        parser = new ActiveXObject(this._xmlDOMConstructor);
    } else {
        for (var i = 0; i < this.xmlDOMConstructors.length; i++) {
            try {
                var cons = this.xmlDOMConstructors[i];
                parser = new ActiveXObject(cons);
                if (parser) {
                    this.logInfo("Using XML DOM constructor: " + cons);
                    this._xmlDOMConstructor = cons;
                    break;
                }
            } catch (e) { }
        }
        if (!parser) {
            this.logWarn("Couldn't create XML DOM parser - tried the following"
                         +" constructors: "+this.echoAll(this.xmlDOMConstructors));
        }
    }
    return parser;
},

//> @classMethod XMLTools.nativeXMLAvailable() 
//
// Returns true if the current browser exposes an XML parser that can be used for SmartClient
// XML operations like web service bindings and XML processing.  See
// +link{group:platformDependencies} for more information on when the XML parser may not
// available and what features are impacted as a result.
//
// @return (boolean) true if native XML processing is available, false otherwise.
//
// @visibility external
//< 
nativeXMLAvailable : function () {
    
    if (isc.Browser.isSafari && !isc.Browser.isApollo && (isc.Browser.safariVersion < 522))
        return false;
    return this._parser != null || this.getXMLParser() != null;    
},


_warnIfNativeXMLUnavailable : function (featureName) {
    if (this.nativeXMLAvailable() || !this.logIsWarnEnabled()) return false;

    var message = "Feature " + featureName + " requires a native XML parser which is "
                  + "not available ";
    if (isc.Browser.isSafari) {
        message += "because this version of Safari does not support native XML processing.";
    } else {
        // IE ActiveX
        message += "because ActiveX is currently disabled.";
    }
    message += " Please see the 'Features requiring ActiveX or Native support'"
              +" topic in the client-side reference under Client Reference/System"
              +" for more information";
    this.logWarn(message);
    return true;
},

// Access / serverToJS
// ---------------------------------------------------------------------------------------

// call the server to do an XML2JS conversion, evals result
serverToJS : function (xmlString, callback, evalVars) {
    isc.DMI.callBuiltin({
        methodName: "xmlToJS", 
        callback: callback, 
        arguments: xmlString, 
        requestParams: {
            evalVars: evalVars,
            evalResult: true
        }
    });
},

// calls the server to do an XML2JS conversion, returns string as 'data' in callback
toJSCode : function (xmlString, callback) {
    isc.DMI.callBuiltin("xmlToJS", xmlString, callback);
},


// convert an XML or DOM element to a JavaScript object.  Attributes and subelements become
// properties.  Note: if nodeNames collide, last one wins.  Also note that getElementsByTagName(*)
// returns all children nodes recursively - not just the immediate children.  So you get a flat
// representation of a potentially nested object.  See the more expensive toJS() for a nested
// result.
_$star : "*",
elementToObject : function (element) {
    if (element == null) return null;

    var object = this.getAttributes(element);

    // transform subelements to properties
    var children = element.getElementsByTagName(this._$star);
    for (var i = 0; i < children.length; i++) {
        var child = children[i];
        object[child.tagName] = this.getElementText(child);
    }

    return object;
},

_$colon : ":",
// return the non-namespace-prefixed tagName, or #text / #comment / #cdata-section for text
// nodes
getLocalName : function (node) {
    // All: tagName/nodeName prefixed
    // Moz: localName is non-prefix'd
    if (!isc.Browser.isIE) {
        var localName = node.localName;
        if (localName == null) return node.nodeName; // for #text nodes in Moz
        return localName;
    }

    
    var name = node.nodeName,
        colonIndex = name.indexOf(this._$colon);
    if (colonIndex != -1) return name.substring(colonIndex + 1);
    return name;
},

//> @classMethod XMLTools.toJS()
// Translates an XML fragment to JavaScript collections.  This method works just like the
// server-side method XML.toJS(Element, Writer):
// <ul>
// <li>  Elements become JavaScript Objects with each attribute becoming a property
// <li>  Subelements with just text (no child elements or attributes) become properties
// <li>  Subelements with child elements or attributes become sub objects
// </ul>
// For example, if you pass the following fragment to this method:
// <pre>
// &lt;foo&nbsp;bar="zoo"&gt;
//     &nbsp;&nbsp;&lt;x&gt;y&lt;/x&gt;
// &lt;/foo&gt;
// </pre>
// You will get back the following JS structure: 
// <pre>
// { bar:"zoo", x:"y"}
// </pre>
// All atomic property values will be of String type.  Use +link{DataSource.recordsFromXML()}
// to do schema-driven XML to JS transform, which can produce correctly typed values.
// 
// @param element (XMLElement or XMLDocument) The element to transform to JS
// @return (Object) The resulting JavaScript collection.
//
// @visibility external
// @example xmlServerValidationErrors
//<
// deep transform XML to JS.  calls itself recursively.  subelements with just the value get
// collapsed, key collisions are turned into arrays.  There are almost certainly still
// differences from server-side transform but none are known at this time.
//
// You can supply an array of fieldNames to look for ("attrMask") - this limits the output data
// to those fields, and is substantially faster in IE for an XML representation that uses lots
// of attributes rather than subelements
_$List : "List",
_$xmlToJS : "xmlToJS",
toJS : function (element, attrMask, dataSource, widgetXML, context) {
    if (element == null) return null;
    if (isc.isAn.XMLDoc(element)) element = element.nativeDoc;
    if (element.documentElement) element = element.documentElement;
    context = context || isc.emptyObject;

    // handle arrays of elements
    if (isc.isAn.Array(element)) {
        var results = [];
        for (var i = 0; i < element.length; i++) {
            results[i] = this.toJS(element[i], attrMask, dataSource, widgetXML, context);
        }
        return results;
        
    }
    
    var object, 
        fieldNames;
        
    // check explicit type
    
    var type = this.getExplicitType(element, widgetXML);
    if (widgetXML || !dataSource || (dataSource && isc.DS.get(type) == null)) {
        

        if (widgetXML) {
            // for widgetXML, detect <Canvas ref="someId"/>, the official way of referring to other
            // components
            var refId = this.isRefElement(element);
            if (refId) 
            {   
                var canvas = isc.Canvas.getById(refId);
                //this.logWarn("ref: " + refId + ", found related Canvas: " + canvas);
                if (canvas != null) return canvas;
            }
            // detect indirect refs <Canvas ref="someId"/>.  Eg if this schema is Menu.ds.xml
            // invoked from Canvas.contextMenu, we're being invoked on the <contextMenu>
            // element, and we need to check if it has a single child <Menu ref="someId"/>
            var firstChild = this.firstElementChild(element),
                refId = firstChild ? this.isRefElement(firstChild) : null;
            if (refId && this.getElementChildren(element).length == 1)
            {
                var canvas = isc.Canvas.getById(refId);
                //this.logWarn("ref: " + refId + ", found related Canvas: " + canvas);
                if (canvas != null) return canvas;
            }

            if (!type) {
                // in widgetXML mode, if there's no explicitly specified type on the element,
                // but the tag name matches any known complexType, treat this as a type
                // override.  Allows eg <ListGrid> to override the "members" field type being
                // "Canvas"
                var tagName = element.tagName;
                if (tagName == this._$List || isc.DS.get(tagName)) type = element.tagName;
            }
        }
    

        // handle like field.multiple=true if List type explicitly specified.  In widgetXML
        // mode this includes a tag of <List>.
        if (type != null && type == this._$List) {
            var children = this.getElementChildren(element);
            return this.toJS(children, attrMask, dataSource, widgetXML, context);            
        }

        // otherwise if explicit type is set (to something we recognize), honor it
        if (type) {
            if (isc.DS.get(type) != null) {
                // NOTE: this case currently occurs only for widgetXML mode and schemaless
                // mode, otherwise, you don't get into this block
                dataSource = isc.DS.get(type);
            } else if (isc.SimpleType.getType(type) != null) {
                // known simple type
                return isc.SimpleType.validateValue(type, this.getElementText(element));
            }
        }
    }

    // respect custom xmlToJS transform
    if (dataSource && dataSource.xmlToJS) return dataSource.xmlToJS(element, context);

    if (this.elementIsNil(element)) return null;

    // if a dataSource was passed, ask the DataSource to derive the field values for any field
    // that defines a valueXPath or getFieldValue function
    if (dataSource) {

        fieldNames = attrMask || dataSource.getFieldNames();
        object = {};
        for (var i = 0; i < fieldNames.length; i++) {
            var fieldName = fieldNames[i],
                field = dataSource.getField(fieldName);

            if (field == null || 
                (field.valueXPath == null && field.getFieldValue == null)) continue;

            var value = dataSource.getFieldValue(element, fieldName, field);
            if (value != null) {
                if (this.logIsDebugEnabled(this._$xmlToJS)) {
                    this.logDebug("valueXPath / getFieldValue() field: " + 
                                  dataSource.ID + "." + fieldName + 
                                  " on element: " + this.echoLeaf(element) +
                                  " got value: " + value, "xmlToJS");
                }
                object[fieldName] = value;
            }
        }
        //this.logWarn("object is now: " + this.echo(object));
    }

    // get attributes (or if a dataSource was passed, add to existing object without
    // clobbering)
    object = this.getAttributes(element, attrMask, object, dataSource != null, dataSource);

    // no attributes, no element children: convert to simple value
    if (!this._hasDataAttributes(object) && !this.hasElementChildren(element))
    {
         return this.getElementText(element);
    }

    
    if (object[this._$xsiType] && object[this._$xsiType] == "xsd:Object") {
        delete object[this._$xsiType];
    }

    // transform subelements to properties
    var children = element.childNodes;

    if (this.logIsDebugEnabled(this._$xmlToJS)) {
        this.logDebug("using DataSource: " + dataSource +
                      " for complex element: " + this.echoLeaf(element) +
                      " childNodes: " + this.echoLeaf(children) +
                      " has attributes: " + this._hasDataAttributes(object)
                      ,"xmlToJS");
    }

    var hadElementChildren = false;
    for (var i = 0; i < children.length; i++) {
        var child = children[i];
        
        // we want the non-namespaced name
        var childName = this.getLocalName(child); 

        // skip text nodes, which in FF we get every time the source XML has CRs
        if (this.isTextNode(child)) continue; 

        hadElementChildren = true;

        // if we have an attrMask, ignore child elements not listed in the attrMask
        if (attrMask && !attrMask.contains(childName)) continue;
    
        var field = dataSource ? dataSource.getField(childName) : null;
        // if there was a field defined with this name that had a valueXPath or getFieldValue
        // function, ignore the subelement
        if (field && (field.valueXPath || field.getFieldValue)) continue;

        var childValue;
        if (this.logIsInfoEnabled(this._$xmlToJS)) {
            this.logInfo("dataSource: " + dataSource + 
                         ", field: " + this.echoLeaf(field) + 
                         (field != null ? " type: " + field.type : "") +
                         ", XML element: " + this.echoLeaf(child), "xmlToJS");
        }

        // field.multiple means the immediate child is just a container tag indicating
        // the property name - the indirect children are the elements that actually
        // conform to the declared type and provide the value
        var fieldValue = child;
        if (field && field.multiple) {
            // use the indirectChildren as the value, unless they're aren't any
            var indirectChildren = this.getElementChildren(child);
            if (indirectChildren.length > 0) fieldValue = indirectChildren;
        }

        
        if (!dataSource || field == null || field.type == null || field.type == "any") {
            if (this.logIsDebugEnabled(this._$xmlToJS)) {
                this.logDebug("applying schemaless transform at: " + 
                              (dataSource ? dataSource.ID : "[schemaless]") + "." + childName,
                              "xmlToJS");
            }
            // if we have no source of type, perform recursive schemaless transform.
            // Note we'll do this even if we have a field, but the field is typeless (the field
            // may, eg, specify multiple:true)
            childValue = this.toJS(fieldValue, null, null, widgetXML, context); 
        } else {

            var childSchema = dataSource.getSchema(field.type);
            // if the field is of dataSource type, do recursive transform
            if (childSchema != null) {   
                var childContext = field.propertiesOnly ? {propertiesOnly: true} : context;
                childValue = this.toJS(fieldValue, null, childSchema, widgetXML, childContext);

                if (this.logIsDebugEnabled(this._$xmlToJS)) {
                    this.logDebug("complexType field: " + this.echoLeaf(field) + 
                                  " got value: " + this.echoLeaf(childValue),
                                  "xmlToJS");
                }
            // otherwise, get the simple value
            } else {
                // NOTE: there's no way to use eg xsi:type to override an explicitly declared
                // simple type on a field (no known use case for this however)

                // field is multiple:true
                if (isc.isAn.Array(fieldValue)) {
                    childValue = [];
                    for (var j = 0; j < fieldValue.length; j++) {
                        childValue.add(
                            dataSource.validateFieldValue(field,
                                                          this.getElementText(fieldValue[j]))
                        );
                    }
                } else {
                    childValue = dataSource.validateFieldValue(field,
                                                               this.getElementText(fieldValue));
                }

                if (this.logIsDebugEnabled(this._$xmlToJS)) {
                    this.logDebug("simpleType field: " + this.echoLeaf(field) + 
                                  " got value: " + this.echoLeaf(childValue),
                                  "xmlToJS");
                }
            }
        }

        if (field && field.multiple) {
            // null or the empty string, the result of a nil or empty element
            // respectively, should be considered an empty array for a multiple field
            if (childValue == null || isc.isA.emptyString(childValue)) childValue = [];
            // anything else should be wrapped in an Array if it's singular
            else if (!isc.isAn.Array(childValue)) childValue = [childValue];
        }

        // collision on the tagName - make an array
        if (object[childName]) {
            if (!isc.isAn.Array(object[childName])) object[childName] = [object[childName]];

            if (field && field.multiple && isc.isAn.Array(childValue)) {
                // for "multiple" fields, avoid nested Arrays
                object[childName].addList(childValue);
            } else {
                object[childName].add(childValue);
            }
        } else {
            object[childName] = childValue;
        }
    }

    // for an element that has no element children, store it's textContent, if any, as a
    // special property.  Note that we still drop text content around element children, but
    // this is typically either whitespace/CRs used for formatting, or the relative order of
    // the text and subelements matters and hence a very different representation would be
    // needed.
    if (!hadElementChildren) {
        var textContent = this.getElementText(element),
            prop = context.textContentProperty || 
                      (dataSource ? dataSource.textContentProperty : "xmlTextContent");

        // if there's a field definition for textContent, validate against it
        if (dataSource) {
            field = dataSource.getTextContentField();
            //this.logWarn("validating against textContentField: " + this.echoLeaf(field) + 
            //             ", with validators: " + this.echoAll(field.validators));
            if (field) textContent = dataSource.validateFieldValue(field, textContent);
        }
        if (textContent != null && !isc.isAn.emptyString(textContent)) {
            object[prop] = textContent;
        }
    }


    // if we have a dataSource with a instanceConstructor property that maps to an existing
    // class, create a new one of those with the data we've mined off the XML
    
    if (widgetXML) {    
        if (!object._constructor && dataSource && (dataSource.instanceConstructor || dataSource.Constructor)) {
            object._constructor = dataSource.instanceConstructor || dataSource.Constructor;
        }

        if (!object.scClassName && dataSource && dataSource.scClassName) {
            object.scClassName = dataSource.scClassName;
        }

        if (
            object._constructor && 
            (context == null || !context.propertiesOnly) &&
            isc.ClassFactory.getClass(object._constructor)
        ) {
            var Constructor = object._constructor;
            delete object._constructor;
            //this.logWarn("toJS creating an instance of: " + Constructor +
            //             " with properties: " + isc.echo(object));
            return isc.ClassFactory.newInstance(Constructor, object);
        }
    } 

    return object;
},

// copied from BasicDataSource.getExplicitType() in server code
_$type : "type",
_$xsiType : "xsi:type",
getExplicitType : function (element, widgetXML) {
    if (element == null || this.isTextNode(element)) return;

    var type = this.getXSIAttribute(element, this._$type);
    if (type) {
        if (type.contains(isc.colon)) type = type.substring(type.indexOf(isc.colon)+1);
    } else if (widgetXML) {
        type = element.getAttribute("constructor");

        // Only honor constructor as a type if there is actually a DataSource
        // for it. Otherwise, specifying <ListGrid constructor="SomeSubclass" />
        // doesn't work property, because the attributes don't get validated
        // against the ListGrid type.
        if (type && !isc.DS.get(type)) type = null;
    }

    return type;
},

// for widgetXML, detect <Canvas ref="someId"/>, the official way of referring to other
// components
_$ref:"ref",
isRefElement : function (element) {
    if (element == null || this.isTextNode(element)) {
        return false;
    }
    var refId = element.getAttribute(this._$ref);
    if (refId && element.attributes.length == 1 && !this.hasElementChildren(element)) return refId;
},

// converts isomorphic:XML to components, complains about missing system schema
toComponents : function (xmlDoc, context) {
    
    if (isc.DS.get("Canvas") == null) {
        this.logWarn("Can't find schema for Canvas - make sure you've loaded"
                     +" component schema via <isomorphic:loadSystemSchema/> jsp tag"
                     +" or by some other mechanism");
    }

    // accept string or xml document
    if (isc.isA.String(xmlDoc)) {
        // accept non-well-formed documents that are just a bunch of ISC components with no
        // containing <isomorphicXML> tag by wrapping the components in an <isomorphicXML> tag
        // if initial parse run fails
        var doc = this.parseXML(xmlDoc, true);
        if (doc.hasParseError()) {
            this.logWarn("xml failed to parse xmlDoc, wrapping in root node");
            doc = this.parseXML("<isomorphicXML>"+xmlDoc+"</isomorphicXML>");
        }
        xmlDoc = doc;
    }

    return this.toJS(xmlDoc, null, null, true, context);
},


_$number : "number", 
getFieldValue : function (record, fieldName, field, dataSource, namespaces) {
    
    if (record.ownerDocument == null) return record[fieldName];

    // if a field is passed, it's from a UI component, and is the complete field definition
    // whether the UI component was databound or not because databound components merge their
    // fields against dataSource fields
    field = field || (dataSource ? dataSource.getField(fieldName) : isc.emptyObject);
    
    try {
        var value;
        if (field.valueXPath) {
            // if the field is of DataSource type, use the valueXPath to select nodes (not a
            // scalar value), and do recursive transform on those elements
            var fieldDS = (dataSource ? dataSource.getSchema(field.type) : 
                                        isc.DS.get(field.type));
            if (fieldDS) {
                var elements = isc.xml.selectNodes(record, field.valueXPath, namespaces),
                    records = isc.xml.toJS(elements, null, fieldDS);

                // selectNodes always returns an Array, but we only want an Array value if the
                // field is declared multiple or if multiple nodes really did match.
                if (!field.multiple && records.length == 1) records = records[0];
                return records;
            } else {
                // otherwise simple scalar value
                value = isc.xml.selectScalar(record, field.valueXPath, namespaces);
            }
        } else {
            value = isc.xml.getXMLFieldValue(record, fieldName);
        }
        // the value retrieved from XML is always just a string, so we need to convert it
        // to the proper type.  NOTE: call instance method to allow overrides.
        dataSource = dataSource || isc.DS.get("Object");
        value = dataSource.validateFieldValue(field, value);
        //this.logWarn("At field: " + dataSource.ID + "." + field.name + 
        //             " got value: " + this.echoLeaf(value));
        return value;
    } catch (e) {
       this.logWarn("error getting value for field: '" + fieldName + 
                    (field.valueXPath ? "', valueXPath: '" + field.valueXPath : "") +
                    "' in record: " + this.echo(record) + 
                    "\r: " + this.echo(e) + this.getStackTrace());
       return null;
    }
},

// given an XML element, get the value for the given fieldName by checking attributes, then
// subelements.  Returned value is always a String or null.
getXMLFieldValue : function (element, fieldName) {
    // attribute representation
    
    var attrValue = element.getAttribute(fieldName);
    if (attrValue != null) return attrValue;

    // subElement representation
    // NOTE: a tagName of "record" matches <foo:record> in Moz, but NOT in IE
    var subElement = element.getElementsByTagName(fieldName)[0];
    if (subElement == null) return null;
    return (isc.Browser.isIE ? subElement.text : subElement.textContent);
},

// whether the element has any attributes that should be considered data, as opposed to
// encoding directives like "xsi:type"
_hasDataAttributes : function (attributes) {
    for (var attrName in attributes) {
        if (attrName == this._$xsiType) continue;
        return true;
    }
    return false;
},


_$xmlnsColon: "xmlns:",
getAttributes : function (element, attrMask, object, dontClobber, dataSource) {
    // NOTE: hasAttributes() doesn't exist in IE

    // optionally add to existing object
    object = object || {};

    var undef;

    // look up the attributes specified by attrMask
    if (attrMask) {
        if (!isc.isAn.Array(attrMask)) attrMask = [attrMask];
        for (var i = 0; i < attrMask.length; i++) {
            var attrName = attrMask[i];
            if (dontClobber && object[attrName] !== undef) continue;
            var attrValue = element.getAttribute(attrName);
            if (attrValue == null || isc.isAn.emptyString(attrValue)) continue;

            if (dataSource && dataSource.getField(attrName)) {
                attrValue = dataSource.validateFieldValue(dataSource.getField(attrName),
                                                          attrValue);
            }
            object[attrName] = attrValue;
        }
        return object;
    }

    // transform attributes to properties
    var attrs = element.attributes;
    if (attrs != null) {
        for (var i = 0; i < attrs.length; i++) {
            var attr = attrs[i],
                attrName = attr.name;

            // Change constructor to _constructor, because constructor
            // has a special meaning for Javascript
            if (attrName == "constructor") attrName = "_constructor"; 

            if (dontClobber && object[attrName] !== undef) continue;

            // whether to include namespace declarations in JS data
            if (isc.startsWith(attrName, this._$xmlnsColon) && 
                dataSource && dataSource.dropNamespaceDeclarations) continue;

            var attrValue = attr.value;
            if (attrValue == null || isc.isAn.emptyString(attrValue)) continue;

            if (dataSource && dataSource.getField(attrName)) {
                attrValue = dataSource.validateFieldValue(dataSource.getField(attrName),
                                                          attrValue);
            }
            object[attrName] = attrValue;
        }
    }
    return object;
},



_$xsi : {
    nil : "xsi:nil",
    "null" : "xsi:null",
    type : "xsi:type"
},
xsiNamespaces : [
    "http://www.w3.org/2001/XMLSchema-instance", 
    "http://www.w3.org/1999/XMLSchema-instance"
],
getXSIAttribute : function (element, attrName) {
    var value;
    // Opera requires that we call getAttributeNS to get the xsi elements, using e.g: 
    // getAttribute("xsi:type") returns null, even if set.  Probably should use this code
    // with other browser as well, but it's slower (because we have to try all the
    // xsiNamespaces) and untested on other platforms.
    if (isc.Browser.isOpera) {
        for (var i = 0; i < this.xsiNamespaces.length; i++) {
            value = element.getAttributeNS(this.xsiNamespaces[i], attrName);
            if (value != null) return value;
        }
        return value;
    }

    return element.getAttribute(this._$xsi[attrName]);
},

_$nil : "nil",

_$null : "null",
_$false : "false",
_$zero : "0",
elementIsNil : function (element) {
    
    if (element == null || !isc.isA.XMLNode(element) || element.nodeType != 1) return false;

    
    var nilAttribute = this.getXSIAttribute(element, this._$nil);

    
    if (nilAttribute && nilAttribute != this._$false && nilAttribute != this._$zero) return true;

    var nilAttribute = this.getXSIAttribute(element, this._$null);
    if (nilAttribute && nilAttribute != this._$false && nilAttribute != this._$zero) return true;

    return false;
},


getElementText : function (element) {
    if (this.elementIsNil(element)) return null;
    if (!element) return null;
    var child = element.firstChild;
    if (!child) {
        // from Firefox 13 to Firefox 14 XML nodes representing attributes were changed to no
        // longer have a #text child.
        
        if ((isc.Browser.isMoz || isc.Browser.isChrome) && element.nodeType == 2) return element.value;
        return isc.emptyString; // empty element, but not marked nil
    }
    var text = child.data;
    
    if (isc.Browser.isMoz && text != null && text.length > 4000) return element.textContent;
    return text;
},

// whether an element is a text node
isTextNode : function (element) {
    if (element == null) return false;
    var nodeType = element.nodeType;
    // 3:text, 4:cdata, 8:comment
    return (nodeType == 3 || nodeType == 4 || nodeType == 8);
},

// whether an element has an element child (as opposed to only text children)
hasElementChildren : function (element) {
    return this.firstElementChild(element) != null;
},

// return the first element child (as opposed to text node child) if there is one, otherwise
// null
firstElementChild : function (element) {
    
    if (element == null || 
        (element.hasChildNodes != null && element.hasChildNodes() == false)) return null;

    var childNodes = element.childNodes;
    if (!childNodes) return null;
    var length = childNodes.length;
    for (var i = 0; i < length; i++) {
        var child = childNodes[i];
        if (!this.isTextNode(child)) return child;
    } 
    return null;
},

// JS -> XML
// ---------------------------------------------------------------------------------------
setAttributes : function (element, values) {
    var undef;
    for (var propName in values) {
        var value = values[propName];

        if (value == null) {
            element.removeAttribute(propName);
            continue;
        }
        
        // setAttribute("attr", true) in IE stores "-1" (yes, seriously)
        if (isc.Browser.isIE && (value === true || value === false)) {
            value = isc.emptyString + value;
        }

        element.setAttribute(propName, values[propName]);
    }
},

// XPath 
// ---------------------------------------------------------------------------------------

// make all namespaces declared on the document element available under the same prefix used in
// the document itself, and make the default namespace, if there is one, available as
// "default".  "namespaces" can be passed it to avoid redeclaration of existing prefixes.
_makeIEDefaultNamespaces : function (doc, namespaces) {

    var buffer = isc.SB.create(),
        docElement = doc.documentElement,
        namespaces = namespaces || isc.emptyObject,
        defaultNamespace;

    // if it hasn't been specified explicitly, try to figure out the default namespaces
    // NOTE: default is a keyword in IE
    if (!namespaces["default"]) {
        defaultNamespace = this._deriveDefaultNamespace(docElement);
        if (defaultNamespace) buffer.append('xmlns:default="', defaultNamespace, '" ');
    }

    // add all the namespaces on the document element
    var attrs = doc.documentElement.attributes;
    for (var i = 0; i < attrs.length; i++) {
        var attr = attrs[i],
            prefix = attr.prefix;
        // NOTE: attr.name is the full name of the attribute, including prefix, so for a
        // non-default namespace declaration attr.name will be eg xmlns:somePrefix
        if (prefix == "xmlns" && prefix != attr.name) {
            // don't redefine already defined selection namespaces
            // NOTE: baseName is IE-only
            if (namespaces[attr.baseName] != null) continue;
            buffer.append(attr.name, '="', attr.value, '" ');
        }
    }
    return buffer.release(false);
},


// Method to determine the "default" namespace by looking at the namespaceURI of the document
// element or (if appropriate) children of the documentElement.
// Used in IE as part of _makeIEDefaultNamespaces()
// 
// Note: This handles the common case where the document element declares a namespace, but 
// is itself in a different namespace
_deriveDefaultNamespace : function (docElement) {
    
    
    
    var shouldLog = this.logIsDebugEnabled("xmlSelect");
    if ((docElement.prefix == null || isc.isAn.emptyString(docElement.prefix)) && 
        docElement.namespaceURI) 
    {
        if (shouldLog) {
            this.logWarn("using docElement ns, prefix: " + docElement.prefix, 
                              "xmlSelect");
        }
        return docElement.namespaceURI;  
                      
                          
    } else if (docElement.firstChild) {
        var defaultNamespace
        for (var i = 0; i < docElement.childNodes.length; i++) {
            var childNode = docElement.childNodes[i];
            // text nodes show up in the childNodes collection in Safari
            if (childNode.nodeType == 3) continue;
            
            var nsURI = childNode.namespaceURI;                
            if (!nsURI) break;

            if (childNode.prefix == null || isc.isAn.emptyString(childNode.prefix)) {
                defaultNamespace = childNode.namespaceURI;
                break;
            }
        }
        if (defaultNamespace != null) {
            if (shouldLog) {
                this.logDebug("using default namespace detected on child: " + 
                              defaultNamespace, "xmlSelect");
            }
        }
        // if there is no default namespace, still define the namespace prefix "default" as
        // the document element's namespace, for conveniece
        if (defaultNamespace == null && docElement.namespaceURI) {
            defaultNamespace = docElement.namespaceURI;
            if (shouldLog) {
                this.logDebug("using document element's namespace as default namespace: " + 
                              defaultNamespace, "xmlSelect");
            }
        }
        // if no appropriate default namespace could be derived, still define one, so that
        // using "default:" doesn't create a JS error.  This allows an XPath like
        // "//default:item|//item" to handle both namespaced and non-namespaced RSS feeds
        if (!defaultNamespace) defaultNamespace = "http://openuri.org/defaultNamespace";
        return defaultNamespace;
    }
},


//> @classMethod xmlTools.selectObjects()   (A)
// Applies an XPath expression to JavaScript objects, returning matching objects.
// <P>
// Both child and attribute names are interpreted as property names, and array access notation
// can be used to select elements from Arrays.  For example:<pre>
//     var results = {
//        searchResults:[
//            { title:"Page One", relevance:6.3 },
//            { title:"Page Two", relevance:5.2, 
//              summary: "Summary of Page One" }
//        ]
//     };
//
//     // returns the "searchResults" two-item Array
//     isc.XMLTools.selectObjects(results, "/searchResults");
//
//     // returns the first item under "searchResults", in an Array (NOTE: in XPath, Array
//     // index starts at 1, not 0)
//     isc.XMLTools.selectObjects(results, "/searchResults[1]");
//
//     // returns ["Page One"]
//     isc.XMLTools.selectObjects(results, "/searchResults[1]/title");
//
//     // also returns ["Page One"]
//     isc.XMLTools.selectObjects(results, "/searchResults[1]@title");
// </pre>
// A limited form of XPath "predicates", that is, expressions with brackets that filter
// returned objects, is allowed.  A predicate can be either:
// <ul>
// <li> a number only, eg [5], for Array access
// <li> the XPath function call "last()", eg [last()], to retrieve the last item
// <li> a property name (*without* any leading "@"), meaning that the property contains a value
//      that is considered "true" in JavaScript.  For example: [summary]
// <li> a property name, comparison operator, and either a number or String literal, for
//      example, [name = "bob"].  In this case the property can also be the XPath function
//      position(), for example, [position() > 5]
// </ul>
// Some examples of using simple predicates with the sample data above:
// <pre>
//     // returns an Array with only the first result
//     isc.XMLTools.selectObjects(results, "/searchResults[relevance > 5.5]");
// 
//     // return an Array with only the second result, since the first has no summary
//     isc.XMLTools.selectObjects(results, "/searchResults[summary]");
// </pre>
// Details of the XPath -> Objects mapping:
// <ul> 
// <li> JavaScript Object properties are considered element children, and text children do not
//      exist (in the XML model, text children exist *between* element children, but nothing
//      exists between JavaScript properties)
// <li> The contents of Array-valued properties are considered immediate element children (this
//      is consistent with the predicate "[5]" acting like Array access)
// <li> "*" in XML selects all element children, so "*" in Object XPath selects the values of
//      all properties, that is, +link{classMethod:isc.getValues(),isc.getValues(object)}, except
//      that Array-valued properties are "flattened" into the returned list.
// </ul>
//
// @param object (Object) Object to select results from
// @param xPath (String) XPath expression
// @return (Array) Array of matching objects, or null for no match
// @visibility external
//<


_$leftBracket : "[",
selectObjects : function (object, xPath, singleValue) {
    if (isc.contains("|")) {
        var subExpressions = xPath.split(/|/),
            results = [];
        for (var i = 0; i < subExpressions.length; i++) {
            results.addList(this.selectObjects(subExpressions[i], object));
        }
        return results;
    }
    // canonicalize to an Array
    var objects = isc.isAn.Array(object) ? object : [object];

    if (xPath != isc.slash) {
        if (isc.startsWith(xPath, isc.slash)) {
            xPath = xPath.substring(1);
            
            if (isc.startsWith(xPath, isc.slash)) {
                this.logWarn("Selector \"\\\\\" does not supported. It was truncated.");
                xPath = xPath.substring(1);
            }
        }

        var segments = xPath.split(/[\/@]/);

        //this.logWarn("segments: " + this.echo(segments));

        objects = this._selectObjects(segments, objects, isc.slash);
    }

    // return the single value, or null on no match
    if (singleValue && objects.length <= 1) return objects[0];

    return objects;
},

_selectObjects : function (segments, objects, path) {

    var segment = segments[0];
    segments = segments.length > 1 ? segments.slice(1) : null;
    //this.logWarn("at path: " + path + 
    //             ", applying segment: '" + segment + 
    //             "' to " + this.echoLeaf(objects));

    if (objects == null) return null;

    // break segment into nodeTest and predicate
    var predicate, 
        nodeTest = segment,
        bracketIndex = segment.indexOf(this._$leftBracket);
        
    if (bracketIndex != -1) {
        nodeTest = segment.substring(0, bracketIndex);
        // extract predicate expression (NOTE: assume one only)
        predicate = segment.substring(bracketIndex + 1, segment.length-1);
        //this.logWarn("nodeTest: " + nodeTest + ", predicate: " + predicate);
    }

    // apply nodeTest to each node
    var resultObjects = [];
    for (var i = 0; i < objects.length; i++) {
        var object = objects[i];
    
        // apply the node test
        if (nodeTest != isc.star) {
            object = object[nodeTest];
        } else {
            var properties = isc.getValues(object);
            object = [];
            for (var i = 0; i < properties.length; i++) {
                if (!isc.isAn.Array(properties[i])) object.add(properties[i]);
                else object.addList(properties[i]);
            }
        }

        //this.logWarn("nodeTest: " + nodeTest + ", result: " + this.echoLeaf(object));

        if (object == null) continue;

        if (!isc.isAn.Array(object)) {
            resultObjects.add(object);
        } else {
            resultObjects.addList(object);
        }
    }

    // filter result by predicate
    if (predicate) {
        // canonicalize object to an Array if we have a predicate
        var predResult = this._applyPredicateExpression(resultObjects, predicate);        

        //this.logWarn("predicate expression: '" + predicate +
        //             "' applied to: " + this.echoLeaf(resultObjects) +
        //             ", with result: " + this.echoLeaf(predResult));
        resultObjects = predResult;
    }
    
    if (segments == null || segments.length == 0) return resultObjects;

    //this.logWarn("recursing with remaining path: " + segments.join("/"));

    // recurse if there are more segments
    path += segment + isc.slash;
    return this._selectObjects(segments, resultObjects, path);
},

_applyPredicateExpression : function (objects, expr) {
    // check for simple index (this will actually accept anything that starts with a
    // number)
    var index = parseInt(expr);
    if (!isNaN(index)) {
        // xPath indices are 1-based
        return [objects[index - 1]];
    }

    if (expr == "last()") return [objects.last()];

    // NOTE: not making property first char vs remaining chars distinction in XML "QName"
    // identifier.  Allowing () as a quick way to allow the position() function
    var parts = expr.match(/^([a-zA-Z_0-9:\-\.\(\)]*)\s*(<|>|!=|=|<=|>=|)\s*(.*)$/),
        property, operator, value;
    //this.logWarn("predicate parts: " + parts);

    if (parts == null) {
        // assume just an identifier
        if (!expr.match(/^[a-zA-Z_0-9:\-\.]*$/)) {
            this.logWarn("couldn't parse predicate expression: " + expr);
            return null;
        }
        property = expr;
    } else {
        property = parts[1], // parts[0] is the entire match
        operator = parts[2],
        value = parts[3];
    }

    // convert this simple expression to a JavaScript expression we can apply to each object
    

    // XPath uses single = operator
    if (operator == "=") operator = "==";

    // XPath uses functions for true and false literals
    if (value == "true()") value = true;
    else if (value == "false()") value = false;

    // support the position() function specially, by passing in params
    if (property == "position()") property = "position";

    //this.logWarn("property: " + property + ", operator: " + operator + ", value: " + value);
    var predFunc = isc._makeFunction("item,position", 
        "return " + 
            (property != "position" ? "item." : "") + property + 
            (operator ? operator + value : ""));

    // apply the function to each object
    var matchingObjects = [];
    //this.logWarn("predicate function: " + predFunc);
    for (var i = 0; i < objects.length; i++) {
        if (predFunc(objects[i], i+1)) matchingObjects.add(objects[i]);
    }
    return matchingObjects;
},

//> @classMethod XMLTools.selectNodes()
// Retrieve a set of nodes from an XML element or document based on an XPath expression.
// <P>
// If the target document is namespaced, namespace prefixes declared in the document element of
// the target document will be available, as well as the default namespace, if declared, under
// the prefix "default".
// <P>
// To declare your own namespace prefixes, provide a prefix to URI mapping as a simple JS
// Object, for example:
// <pre>
//   {
//      az : "http://webservices.amazon.com/AWSECommerceService/2005-03-23",
//      xsd : "http://www.w3.org/2001/XMLSchema"
//   }
// </pre>
// <P>
// <b>NOTE:</b> this API cannot be supported on the Safari web browser for versions earlier
// than 3.0.3.
//
// @param element (XMLElement or XMLDocument or String)  Native XMLElement,document, or xml string 
//                                                       to select from
// @param expression (XPath)   XPath expression to use to select nodes
// @param [namespaces] (prefix -> URI mapping) namespace mapping used by the expression
// @return (Array) list of nodes matching XPath
//
// @group xmlTransform
// @visibility xmlBinding
// @example xmlServerValidationErrors
//<
selectNodes : function (element, expression, namespaces, single) {
    if (isc.isA.String(element)) {
        element = this.parseXML(element);    
    }
    
    if (isc.Browser.isSafari && (isc.Browser.isApollo || (isc.Browser.safariVersion < 522)))
    {
        this._warnIfNativeXMLUnavailable("XPath");
        return this.safariSelectNodes(element, expression, namespaces, single);
    }
    
    if (isc.isAn.XMLDoc(element)) {
        return element.selectNodes(expression, namespaces, single);
    }
    var start = isc.timestamp();
    var returnValue = this._selectNodes(element, expression, namespaces, single);
    var end = isc.timestamp();
    
    if (this.logIsInfoEnabled("xmlSelect")) {
        this.logInfo("selectNodes: expression: " + expression + 
                     " returned " + this.echoLeaf(returnValue) +
                      ": " + (end-start) + "ms", "xmlSelect");
    }

    return returnValue;
},

// Do very crude emulation of XPath for older Safari and older WebKit (like Adobe AIR)
// where native XPath is not available.

safariSelectNodes : function (element, expression, namespaces, single) {

    var elements = [];
    
    if (!expression) {
        return null;        
    }
    var recordName = expression.substring(expression.indexOf(":")+1);
    var pickUpSubElements;
    if (recordName.endsWith("/*")) {
        pickUpSubElements = true;
        recordName = recordName.substring(0, recordName.indexOf("/*"));
    }
    
    // NOTE: a tagName of "record" matches <foo:record> in Moz, but NOT in IE
    var nodeList = element.getElementsByTagName(recordName);
    
    if (pickUpSubElements && nodeList.length > 0) {
        var parent = nodeList[0];
        nodeList = parent.childNodes;
    }
        

    for (var i = 0; i < nodeList.length; i++) {
        // don't pick up text nodes -- can happen when iterating through childNode array
        // to simulate /* xpath
        if (nodeList[i].nodeType == 3) continue;
        elements.add(nodeList[i]);
    }
    // don't create a spurious Array for the most common case of a singular body
    // element
    
    if (pickUpSubElements && elements.length == 1) elements = elements[0];
    return elements;
},
        

// namespaces: map from prefix -> namespaceURI
// prefixes: option list of prefixes 
_generateNamespaces : function (namespaces, prefixes, indent) {
    if (namespaces == null) return isc.emptyString;

    if (prefixes == null) prefixes = isc.getKeys(namespaces);

    var buffer = isc.SB.create(),
        indent = (indent != null ? "\n" + indent : "");
    for (var i = 0; i < prefixes.length; i++) {
        var prefix = prefixes[i];
        buffer.append(indent, " xmlns:", prefix, '="', namespaces[prefix], '"');
    }
    return buffer.release(false);
},

// called for Moz and Safari only - return a namespace to use for the "default:" prefix in
// XPath selection
_getDefaultNamespace : function (docElement) {
    // check for a default namespace on the document element.  Note this will get
    // the default namespace established by the document element, even if the
    // document element itself is in another namespace
    var docNS = docElement.lookupNamespaceURI("");
    
    if (isc.Browser.isSafari && (docNS == null || docNS == "")) {
        docNS = docElement.getAttribute("xmlns");
    }
    // fall back to the namespace of the document element, even if it's not a
    // default namespace
    if (docNS == null) docNS = docElement.namespaceURI;
    // fall back to non-namespaced elements
    if (docNS == null) docNS = "";

    return docNS
},

_selectNodes : function (element, expression, namespaces, single) {

    
    if (element == null) return;
    var doc = element.ownerDocument;
    if (doc == null && element.documentElement) {
        // a document object was passed
        doc = element;
        element = doc.documentElement;
    }
    // in the case of an empty response, doc could still be null, so return
    if (doc == null) return null;
    if (isc.Browser.isIE) {
        
        if (isc.Browser.version > 5.5) {
            doc.setProperty("SelectionLanguage", "XPath");

            var nsString = this._makeIEDefaultNamespaces(doc, namespaces);
            if (namespaces) nsString += this._generateNamespaces(namespaces);            
            if (this.logIsDebugEnabled("xmlSelect")) {
                this.logDebug("selectNodes: expression: " + expression + 
                              ", using namespaces: " + nsString, "xmlSelect");
            }
            doc.setProperty("SelectionNamespaces", nsString);
        }

        // if "single" was passed, select a single node
        if (single) return element.selectSingleNode(expression);

        // otherwise return an Array of nodes
        var nodes = element.selectNodes(expression);

        // convert native NodeList object to a JavaScript Array
        return this._nodeListToArray(nodes);
    }

    
    var baseResolver = doc.createNSResolver(doc.documentElement),
        defaultNamespace = this._getDefaultNamespace(doc.documentElement);
    if (this.logIsDebugEnabled("xmlSelect")) {
        this.logDebug("Using namespaces: " + isc.echo(namespaces) + 
                      ", defaultNamespace: '" + defaultNamespace + "'", "xmlSelect");
    }
    var resolver = function (prefix) {
        // supplied namespaces first
        if (namespaces && namespaces[prefix]) return namespaces[prefix];
        if (prefix == "default") return defaultNamespace;
        return baseResolver.lookupNamespaceURI(prefix);
    };
    // 0 is resultType = XPathResult.ANY_TYPE
    var results = doc.evaluate(expression, element, resolver, 0, null);

    // if "single" was passed, just return the first node
    if (single) return results.iterateNext();

    // convert native nodeList object to JavaScript Array
    return this._nodeListToArray(results);
},

// convert a native NodeList object to a JavaScript Array.
// NodeLists are returned by native selectNodes() in IE or document.evaluate(xpath) in other
// browsers.  xmlElement.childNodes is also a NodeList
_nodeListToArray : function (nodeList) {
    var output = [];

    
    if (isc.Browser.isIE || nodeList.iterateNext == null) {
        for (var i = 0; i < nodeList.length; i++) {
            output.add(nodeList.item(i));
            
        }
    } else {
        var resultNode;
        while (resultNode = nodeList.iterateNext()) {
            output.add(resultNode);
        }   
    }
    return output;
},

getElementChildren : function (element) {
    var output = [],
        childNodes = element.childNodes;
    for (var i = 0; i < childNodes.length; i++) {
        var child = childNodes[i];
        if (this.isTextNode(child)) continue;
        output.add(child);
    }
    return output;
},

//> @classMethod XMLTools.selectString() [A]
// Retrieve a string value from an XML element or document based on an XPath expression.
// <P>
// If more than one node matches, only the first node's value will be returned.
// <P>
// Namespacing works as described under +link{XMLTools.selectNodes()}
// <P>
// <b>NOTE:</b> this API cannot be supported on the Safari web browser for versions prior to
// 3.0.3.
//
// @param element (XMLElement or XMLDocument or String)  Native XMLElement,document, or xml string 
//                                                       to select from
// @param expression (XPath)   XPath expression to use to select nodes
// @param [namespaces] (prefix -> URI mapping) namespace mapping used by the expression
//
// @return (String) result of the XPath, in String form
//
// @group xmlTransform
// @visibility xmlBinding
// @example xmlServerValidationErrors
//<
selectString : function (element, expression, namespaces) {
    return this.selectScalar(element, expression, namespaces);
},

//> @classMethod XMLTools.selectNumber() [A]
// Retrieve a numeric value from an XML element or document based on an XPath expression.
// <P>
// If more than one node matches, only the first node's value will be returned.
// <P>
// Namespacing works as described under +link{XMLTools.selectNodes()}
// <P>
// <b>NOTE:</b> this API cannot be supported on the Safari web browser for versions prior to
// 3.0.3.
//
// @param element (XMLElement or XMLDocument or String)  Native XMLElement,document, or xml string 
//                                                       to select from
// @param expression (XPath)   XPath expression to use to select nodes
// @param [namespaces] (prefix -> URI mapping) namespace mapping used by the expression
//
// @return (Number) result of the XPath, in Number form
//
// @group xmlTransform
// @visibility xmlBinding
//<
selectNumber : function (element, expression, namespaces) {
    return this.selectScalar(element, expression, namespaces, true);
},

selectScalar : function (element, expression, namespaces, asNumber) {
    if (isc.isA.String(element)) element = this.parseXML(element);
    if (isc.isAn.XMLDoc(element)) return element.selectScalar(expression, namespaces, asNumber);

    // NOTE: the XPath standard allows you to ask for a specific "resultType", eg String or
    // Number, instead of a NodeSet.  Moz does implement this.  However, for both
    // resultType:String and resultType:Number, Moz returns identical results for absence of an
    // element vs empty element, whereas we'd like to be able to return null vs "" for these
    // two cases respectively.
    //
    // In IE we can get a singular node from selectSingleNode(), and we grab the and parseInt.
    //
    // NOTE also: in Moz at least, if we don't ask for a scalar type, Moz will never return it
    // for XPathResult.ANY: single text node results and single attribute results still return
    // unordered_node_iterator by default, with crashing stringValue and numberValue accessors
    
    // pass "true" to ask for a single node
    
    
    var value;
    if (isc.Browser.isSafari && isc.Browser.isApollo || (isc.Browser.safariVersion < 522)) {
        var name=expression.substring(expression.indexOf(":")+1);
        value = element.getElementsByTagName(name)[0];
    } else {
        value = this.selectNodes(element,expression,namespaces,true);
    }

    if (value == null) return null;
    var text = this.getElementText(value);
    return asNumber ? parseInt(text) : text;
},

// performs xpath select and returns array of strings that are the values of selected nodes.
//
// expects result of xpath select to be a list of Attribute or Text nodes.  Performs an in-place
// replacement on the list with the node values and returns that.
// XXX internal until this handles simple elements in addition to attributes
selectScalarList : function (element, expression, namespaces) {
    if (isc.isA.String(element)) element = this.parseXML(element);
    if (isc.isAn.XMLDoc(element)) return element.selectScalarList(expression, namespaces);

    var values = this.selectNodes(element, expression, namespaces);
    // Thank god attributes and text elements share the 'nodeValue' attribute that has the data.
    for (var i = 0; i < values.length; i++) {
        values[i] = values[i].nodeValue;
    }
    return values;
},

// XSLT
// ---------------------------------------------------------------------------------------

//> @classMethod XMLTools.transformNodes()
// Apply an XSLT Stylesheet to an XML Document.
// <P>
// This method cannot currently be supported on the Safari web browser versions prior to
// 3.0.3.
//
// @param inputDocument (XMLDocument) XML document to apply the transform to
// @param styleSheet    (XMLDocument) XSLT stylesheet to use for transform
// @return (String) stylesheet output
//
// @group xmlTransform
// @visibility xmlBinding
//<
transformNodes : function (inputDocument, styleSheet) {
    if (isc.isAn.XMLDoc(inputDocument)) inputDocument = inputDocument.nativeDoc;
    if (isc.isAn.XMLDoc(styleSheet)) styleSheet = styleSheet.nativeDoc;
    if (isc.Browser.isIE) {
        if (inputDocument) {
            return inputDocument.transformNode(styleSheet);
        }
        return null;
    }
    

    var processor = new XSLTProcessor();
    processor.importStylesheet(styleSheet);

    
    if (isc.Browser.isMoz && this.mozAnchorBug && isc.Browser.geckoVersion < 20051107) {
        var ownerDocument = document.implementation.createDocument("", "test", null);
        var newFragment = processor.transformToFragment(inputDocument, ownerDocument);
        return new XMLSerializer().serializeToString(newFragment);
    }

    if (inputDocument) {
        var outputDocument = processor.transformToDocument(inputDocument);
        return new XMLSerializer().serializeToString(outputDocument);
    }

    return null;

    // transformToFragment can produce something that is not a well-formed document, for
    // example, just a text node.  However this doesn't really mean that Mozilla supports XSLT
    // to text, since any tags within the fragment become actual elements.  XMLSerializer can
    // be used to convert to String, but for some reason in also converts tags to eg "&lt;"
    //var fragment = processor.transformToFragment(inputDocument, inputDocument);
    //return new XMLSerializer().serializeToString(fragment);
},

// XML Serialize
// ---------------------------------------------------------------------------------------
//> @classMethod XMLTools.serializeToString()
// Takes an XMLDocument and returns it as a String.
// <P>
// This method is not supported on the Safari web browser versions prior to 3.0.3.
//
// @param inputDocument (XMLDocument) XML document to apply the transform to
// @return (String) XML document as a String
//
// @group xmlTransform
// @visibility xmlBinding
//<
serializeToString : function (inputDocument) {
    this._serializerXSL = this._serializerXSL || isc.xml.parseXML(
'<xsl:stylesheet version=\'1.0\' xmlns:xsl=\'http://www.w3.org/1999/XSL/Transform\'>\r' +
'<xsl:output method="xml" indent="yes"/>\r' +
'<xsl:strip-space elements="*"/>\r' +
'<xsl:template match="/">\r' +
'  <xsl:copy-of select="."/>\r' +
'</xsl:template>\r' +
'</xsl:stylesheet>');

    return this.transformNodes(inputDocument, this._serializerXSL);
},

// Schema Translation
// ---------------------------------------------------------------------------------------

//> @classMethod XMLTools.loadXMLSchema()       [A]
// Load an XML file containing XML schema definitions and create DataSource and SimpleType
// objects to represent the schema.  You can use to loaded schema to bind ISC components,
// perform validation, create editing interfaces, and build other metadata-driven interfaces.
// You can also use +link{dataSource.inheritsFrom,schema inheritance} to overlay
// presentation-specific data (such as user-visible titles) on top of XML Schema.
// <p>
// In the loaded XML Schema, all &lt;xsd:complexType&gt; declarations become SmartClient
// DataSources, and all &lt;xsd:simpleType&gt; definitions become SmartClient
// +link{SimpleType,atomic type definitions}.  
// <p>
// By default, named complexType definitions and named element definitions containing
// complexTypes become global DataSources, that is, they can be fetched with
// +link{classMethod:DataSource.getDataSource()}.  Inline complexType definitions get
// automatically generated names.
// <p>
// Named simpleType declarations become global +link{SimpleType,atomic types}, that is,
// subsequently defined DataSources can use them for +link{attr:DataSourceField.type}.  XML
// schema "restrictions" for simple types are automatically translated to
// +link{attr:DataSourceField.valueMap} or +link{attr:DataSourceField.validators} as
// appropriate.
// <P>
// The created SchemaSet object is available in the callback as the single parameter
// "schemaSet", or can retrieved via <code>SchemaSet.get(schemaNamespace)</code>.
// <P>
// NOTE: unless you are building an application that dynamically loads XML Schema 
// without prior knowledge, instead of calling loadXMLSchema(), you should either:
// <ul>
// <li> use the +link{group:loadXMLSchemaTag} tag to eliminate the need for an asynchronous
// download of an XML Schema file as part of application startup, <b>OR</b>
// <li> use the "WSDL" tab in the Developer Console to obtain the XML Schema definition as a
// JavaScript file that can be loaded via a normal HTML &lt;SCRIPT SRC=&gt; tag and/or
// combined with other JavaScript files.  
// </ul>
// <P>
// NOTE: required fields: the XML Schema concept of "required" for an attribute or subelement, 
// expressed via use="required" (for an attribute) or minOccurs > 0 (for a subelement), is
// that the attribute or element must be present in the XML document <i>but can have any
// value</i>, including being empty or null.  The SmartClient notion of required means
// non-null.  You can express the SmartClient notion of required in XML Schema with the
// combination of minOccurs>0 and a minLength or length "restriction", and SmartClient 
// will recognize the field as SmartClient-required, with all of the behaviors that implies
// (eg, specially styled form titles, automatic validation, etc).
//
// @param schemaURL (URL) URL to load the schema from
// @param callback  (Callback) signature is callback(schemaSet)
// @param [requestProperties] (RPCRequest) additional properties to set on the RPCRequest
//                                         that will be issued
// @param autoLoadImports (boolean) if set, xsd:import statements will be processed
//                                  automatically to load dependent XSD files where a
//                                  "location" is specified.  The callback will not fire until
//                                  all dependencies have been loaded

//
// @group xmlSchema
// @visibility xmlBinding
// @example xmlSchemaImport
//<
loadXMLSchema : function (xmlSchemaURL, callback, requestProperties, autoLoadImports, wsProperties) {
    requestProperties = requestProperties || {};
    requestProperties.operationType = requestProperties.operationType || "loadXMLSchema";

    this.loadWSDL(xmlSchemaURL, callback, requestProperties, autoLoadImports, wsProperties, true);
},

//> @classMethod XMLTools.loadWSDL()      [A]
// Load a WSDL file and create an instance of WebService that allows invoking operations and
// binding DataSources to web service operations.
// <P>
// The created WebService object is available in the callback as the single parameter
// "service", or can be retrieved via <code>WebService.get(serviceNamespace)</code>.
// <P>
// XML Schema present in the WSDL file will also be processed as described in
// +link{XMLTools.loadXMLSchema()}.  However note that <b>imported</b> XML Schema
// (&lt;xs:import&gt; tag) will not be automatically loaded and must be loaded manually using
// +link{loadXMLSchema()} before the loaded service will be usable.  This is because the WSDL
// spec allows but does not require a valid URL to be provided for loading imported XML Schema.
// <P>
// NOTE: unless you are building an application that dynamically contacts WSDL web services
// without prior knowledge, instead of calling loadWSDL(), you should either:
// <ul>
// <li> use the +link{group:loadWSDLTag} tag to eliminate the need for an asynchronous
// download of a WSDL file as part of application startup, <b>OR</b>
// <li> use the "WSDL" tab in the Developer Console to obtain the WebService definition as a
// JavaScript file that can be loaded via a normal HTML &lt;SCRIPT SRC=&gt; tag and/or combined
// with other JavaScript files.
// </ul>
// Platform notes:
// <ul>
// <li> loadWSDL() is not supported in Safari 2.0 (but is supported in Safari 3.0.3 and greater)
// However, you can use either approach mentioned above (loadWSDLTag or JavaScript file) with 
// Safari pre 3.0.3.
// <li> if you are using a non-Java server, in order to obtain a JavaScript file representing a
// web service, you must run the Developer Console in the Java-based SmartClient SDK
// </ul>
//
// @param wsdlURL  (URL) URL to load the WSDL file from
// @param callback (Callback) signature is callback(service)
// @param [requestProperties] (RPCRequest) additional properties to set on the RPCRequest
//                                         that will be issued
// @param autoLoadImports (boolean) if set, xsd:import statements will be processed
//                                  automatically to load dependent XSD files where a
//                                  "location" is specified.  The callback will not fire until
//                                  all dependencies have been loaded
//
// @group xmlSchema
// @visibility xmlBinding
// @example WSDLDataSource
//<
// NOTE: returnSchemaSet is an internal parameter to allow the loadWSDL logic to also be used
// by loadXMLSchema()
loadWSDL : function (xmlSchemaURL, callback, requestProperties, autoLoadImports, wsProperties, returnSchemaSet) {
    // load the schema translator if it hasn't been loaded, not doing translation until it's
    // done loading
    if (!this._schemaTranslator) {
        var url = isc.Page.getIsomorphicClientDir() + "schema/schemaTranslator.xsl";
        // get rid of the http://, or loadXML will assume it has to use the HTTPProxy to load
        // the file
        url = url.replace(/https?:\/\/[^\/]*\//, "/");
        this._schemaTranslator = "LOADING"; // prevent multiple attempts to load translator
        isc.xml.loadXML(url, function (xmlDoc, xmlText, rpcResponse) {
            isc.xml.logDebug("schema translator loaded");
            
            if (isc.Browser.isMoz && rpcResponse.xmlHttpRequest &&
                rpcResponse.xmlHttpRequest.responseXML) 
            {
                isc.xml._schemaTranslator =
                        isc.XMLDoc.create(rpcResponse.xmlHttpRequest.responseXML);
            } else {
                isc.xml._schemaTranslator = xmlDoc;
            }
            isc.xml.loadWSDL(xmlSchemaURL, callback, requestProperties, 
                             autoLoadImports, wsProperties, returnSchemaSet);
        });
        return;
    }

    requestProperties = requestProperties || {};
    requestProperties.operationType = requestProperties.operationType || "loadWSDL";

    var context = { 
        location: xmlSchemaURL,
        callback : callback,
        autoLoadImports: autoLoadImports, 
        wsProperties: wsProperties || {},
        returnSchemaSet: returnSchemaSet
    };
    
    isc.xml.loadXML(xmlSchemaURL, function (xmlDoc,xmlText,rpcResponse,rpcRequest) {   
                        context.rpcResponse = rpcResponse;
                        context.rpcRequest = rpcRequest;
                        isc.xml._loadSchemaReply(xmlDoc, context);
                    },
                    requestProperties);
},

// load a WSDL service from XML text, an XMLDoc or XML elements that are already loaded
loadWSDLFromXML : function (xmlDoc, callback, autoLoadImports, wsProperties, returnSchemaSet) {
    if (isc.isA.String(xmlDoc)) xmlDoc = isc.xml.parseXML(xmlDoc);

    this._loadSchemaReply(xmlDoc, { callback:callback, autoLoadImports:autoLoadImports,
                                    wsProperties:wsProperties, returnSchemaSet:returnSchemaSet });
},

// whether to use client-side XML2JS to translate WSDL/XMLSchema definitions to live objects
useClientXML : true, 
_loadSchemaReply : function (xmlDoc, context) {
    // NOTE: check that translator is an XML doc, not null, since we use a loading marker
    if (!isc.isAn.XMLDoc(this._schemaTranslator)) { 
        this.logInfo("deferred schema translator, schema translator not loaded", "xmlComm");
        isc.Timer.setTimeout({ methodName: "_loadSchemaReply", target:this, 
                               args:[xmlDoc, context] });
        return;
    }
    this.logInfo("transforming schema: " + this.echoLeaf(xmlDoc) + 
                 " with translator " + this.echoLeaf(this._schemaTranslator), "xmlComm");
    var xmlText = this.transformNodes(xmlDoc, this._schemaTranslator);

    if (this.logIsDebugEnabled("xmlComm")) {
        this.logWarn("XML service definition is: \n" + xmlText);
    }

    var wsProperties = context.wsProperties,
        initiator = wsProperties.initiator;
    if (wsProperties.captureXML) {
        wsProperties.xmlSource = xmlText;
        if (initiator) initiator.addImportXMLSource(xmlText, context.location);
    }

    if (this.useClientXML) {
        var xmlDoc = isc.xml.parseXML(xmlText),
            elements = xmlDoc ? this._nodeListToArray(xmlDoc.documentElement.childNodes) : null,
            jsResult = elements ? this.toJS(elements, null, null, true) : null
        ;

        //this.logWarn("XML service definition is: \n" + xmlText);

        //this.logWarn("js result: \n" + this.echoFull(this.toJS(elements)));

        this._loadSchemaToJSReply(context);
        return;
    }

    this.logInfo("about to call serverToJS with: " + this.echoLeaf(xmlText) +
                 ", callback: " + this.echo(context.callback), "xmlComm");
    this.serverToJS(xmlText, function () {
                        isc.Log.logWarn("serverToJS returned");
                        isc.xml._loadSchemaToJSReply(context);
                    });
},
_loadSchemaToJSReply : function (context) {
    
    var loadedObject;
    if (context.returnSchemaSet) {
        loadedObject = isc.SchemaSet._lastLoaded;
    } else {
        // fallback covers us in case someone loads a WSDL that really don't have a workable
        // service definition but contains schema
        loadedObject = isc.WebService._lastLoaded || isc.SchemaSet._lastLoaded;
    }
    isc.WebService._lastLoaded = isc.SchemaSet._lastLoaded = null;

    // if we didn't load an object and willHandleError is false, return without firing the 
    // callback
    if (!loadedObject && context.rpcRequest.willHandleError == false) return;

    // tell the service/schemaSet the location it was loaded from - needed for knowing the
    // correct relative path to load imported schema
    if (loadedObject) loadedObject.location = context.location;

    if (context.wsProperties && loadedObject) loadedObject.setProperties(context.wsProperties);

    // tack on the RPCRequest - allows context to be kept across a loadWSDL call
    var argNames = (isc.isA.WebService(loadedObject) ? "service" : "schemaSet") + ",rpcRequest";
    var args = [loadedObject,context.rpcRequest];

    // if autoLoadImports was passed, tell the service/schemaSet to load imported schema
    if (context.autoLoadImports && loadedObject && loadedObject.loadImports) {
        var _this = this;
        loadedObject.loadImports(function () {
            _this._completeLoad(context.callback, argNames, args);
        });
    } else {
        this._completeLoad(context.callback, argNames, args);
    }
},

_completeLoad : function (callback, argNames, args) {
    //this.logWarn("firing callback: " + this.echo(callback) + 
    //             " with argName: " + argName +
    //             " value: " + arg);

    this.fireCallback(callback, argNames, args);
},

getCompleteSource : function (service, callback, asXML) {
    var importSources = service.importSources;
    if (!importSources) return "";

    importSources = importSources.getProperty("xmlText");
    // include the source of the webService or schemaSet that imported all of these
    // dependencies
    importSources.unshift(service.xmlSource);
    // remove <?xml directives
    importSources = this.map("trimXMLStart", importSources);
    var source = importSources.join("\n");
    
    //this.logWarn("sources: " + this.echo(importSources));

    if (asXML) {
        this.fireCallback(callback, "source", [source]);
        return;
    }
    
    this.toJSCode(source, function (rpcResponse, data) {
        this.fireCallback(callback, "source", [data]);
    })
}

// SmartClient Component XML
// ---------------------------------------------------------------------------------------

//> @groupDef componentXML
// <smartclient>
// As covered in the <i>QuickStart Guide</i> Chapter 4, <i>Coding</i>, SmartClient 
// components can be created in either XML or JavaScript format.  This section covers further 
// details of using the XML format, called "SmartClient component XML".
// <P>
// </smartclient>
// <smartgwt>
// Component XML is an XML format for declaring Smart GWT components and screen definitions.
// Available with Smart GWT Pro and above, Component XML is the same format used by Visual
// Builder to save screens.  
// <P>
// By allowing you to keep layout information and property settings in an XML format, Component
// XML enables non-developers to build and maintain portions of your application, either by
// editing screens within Visual Builder or by directly editing the XML itself.
// <P>
// Unlike the similar GWT "UIBinder" technology, Component XML does not require a compilation
// step.  XML screen definitions can be generated on the fly, modified at runtime, stored in a
// database, and in all other ways treated as a dynamic resource.  See the section "Dynamic
// Component XML" for details.
// <P>
// <h3>Basic Usage</h3>
// <P>
// To create a Smart GWT component in XML code, you create a tag with the component's class
// name. You can set that component's properties either as tag attributes:
// <pre>
//   &lt;Button title="Click me" width="200" /&gt;
// </pre>
// or in nested tags:
// <pre>
//   &lt;Button&gt; 
//     &lt;title&gt;Click me&lt;/title&gt;
//     &lt;width&gt;200&lt;/width&gt;
//   &lt;/Button&gt; 
// </pre>
// <P>
// To set a property that is an Array of simple types (like int, or String), repeat tags like
// so (for +link{dynamicForm.colWidths}):
// <P>
// <pre>
// &lt;DynamicForm&gt;
//     &lt;numCols&gt;2&lt;/numCols&gt;
//     &lt;colWidths&gt;250&lt;/colWidths&gt;
//     &lt;colWidths&gt;*&lt;/colWidths&gt;
// &lt;/DynamicForm&gt;
// </pre>
// To set a property that takes an Array of complex objects, use the property name as a
// container tag, then create further nested tags for the objects in the array, like so (for
// +link{listGrid.fields}):
// <P>
// <pre>
// &lt;ListGrid&gt;
//     &lt;fields&gt;
//         &lt;ListGridField name="accountName" ... /&gt;
//         &lt;ListGridField name="accountType" ... /&gt;
//     &lt;/fields&gt;
// &lt;/ListGrid&gt;
// </pre>
// <P>
// This same approach works for creating nested layouts, such as placing a ListGrid in a
// VLayout:
// <P> 
// <pre>
// &lt;VLayout&gt;
//     &lt;members&gt;
//         &lt;ListGrid .. /&gt;
//     &lt;/members&gt;
// &lt;VLayout&gt;
// </pre>
// <P>
// </smartgwt>
// <b>Referring to previously defined components</b>
// <P>
// To refer to another component by ID in XML, use &lt;Canvas ref=/&gt;.  For example:
// <pre>
// &lt;Canvas ID="myCanvas"/&gt;
// &lt;Canvas ID="myCanvas2"/&gt;
// &lt;VLayout&gt;
//     &lt;members&gt;
//         &lt;Canvas ref="myCanvas"/&gt;
//         &lt;Canvas ref="myCanvas2"/&gt;
//     &lt;/members&gt;
// &lt;/VLayout&gt;
// </pre>
// <P>
// <h3>Loading screens stored in Component XML</h3>
// <P>
// Save your Component XML as a file called <i>screenName</i>.ui.xml under
// <i>webroot</i>/shared/ui/.  Placing your .ui.xml file in this directory makes it visible to
// the system; the location of this directory can be configured in +link{server_properties,server.properties}
// by setting
// the <i>project.ui</i> property.  <i>screenName</i> can be any valid identifier (no spaces,
// dashes or periods - underscores OK).
// <P>
// If you have multiple top-level tags (eg, your code is similar to the example above under
// "Referring to previousy defined components") use &lt;isomorphicXML&gt; as a top-level
// container tag - this has no impact on processing and is just an idiom to make your file valid
// XML, since XML does not allow multiple top-level tags in a document.
// <P>
// Component XML screens are then loaded using the ScreenLoaderServlet.  The default SDK comes
// with this servlet already registered at 
// <smartclient><i>projectBase</i>/isomorphic/screenLoader</smartclient>
// <smartgwt><i>projectBase</i>/sc/screenLoader</smartgwt>.  If you've modified web.xml
// or only included some of the default servlets, you may need to add it now - see the 
// <smartclient>+link{group:iscInstall,Installation Instructions}</smartclient>
// <smartgwt>+link{group:sgwtEESetup,Installation Instructions}</smartgwt>.
// <P>
// To create an application that consists of <i>just</i> the imported mockup, just add a
// &lt;script src&gt; tag pointing to the ScreenLoader servlet and referring to the
// <i>screenName</i> you used when you saved your file.  
// <smartclient>
// For example, if your application launches from a directory next to the "isomorphic"
// directory from the SDK:
// <pre>
//    &lt;script src="../isomorphic/screenLoader?screenName=<i>screenName</i>"&gt;&lt;/script&gt;
// </pre>
// </smartclient>
// <smartgwt>
// For example, add the following to your bootstrap .html file:
// <pre>
//    &lt;script src="sc/screenLoader?screenName=<i>screenName</i>"&gt;&lt;/script&gt;
// </pre>
// </smartgwt>
// If you want to load screens dynamically, or if you want to load more than one screen, use
// +link{RPCManager.loadScreen()}.  See the section on "Multiple screens and global IDs"
// below.
// <P>
// <smartclient>
// <p>
// You can optionally enable "strict validation" for Component XML and DataSource files,
// which helps avoid typos, misspellings and other basic errors in your XML.  See
// the +link{strictMode,Strict Mode overview} for details.
// <h3>Embedding JavaScript code</h3>
// <P>
// To embed a JavaScript expression into component XML, use the &lt;JS&gt; tag.  For example:
// <pre>
// &lt;VLayout&gt;
//     &lt;width&gt;&lt;JS&gt;isc.Page.getWidth() - 20&lt;/JS&gt;&lt;/width&gt;
// &lt;/VLayout&gt;
// </pre>
// Note that, like all component XML properties, the <code>width</code> property can be
// specified either as an XML attribute or as a subelement.  Expressing it as a subelement, as
// shown above, allows the &lt;JS&gt; tag to be used.
// <P>
// <b>Embedding Methods</b>
// <P>
// For +link{group:stringMethods,StringMethods} such as +link{listGrid.recordClick()},
// JavaScript code can be used as an ordinary element value:
// <pre>
// &lt;ListGrid&gt;
//     &lt;recordClick&gt;if (record.age &gt; 65) doSomething()&lt;/recordClick&gt;
// &lt;/ListGrid&gt;
// </pre>
// To embed an actual function definition, use the &lt;JS&gt; tag described above.  For
// example:
// <pre>
// &lt;ListGrid&gt;
//     &lt;recordClick&gt;&lt;JS&gt;function (viewer, record, recordNum, field) {
//          if (record.age &gt; 65) doSomething();
//     }&lt;/JS&gt;&lt;/recordClick&gt;
// &lt;/ListGrid&gt;
// </pre>
// Unfortunately, characters commonly used in JavaScript code, such as ampersand (&amp;), are
// not legal inside XML element or attribute values.  For example, the expression "record !=
// null && record.age &gt; 65" must be written as shown below, or it is not considered valid XML:
// <P>
// <pre>
// &lt;ListGrid&gt;
//     &lt;recordClick&gt;
//         if (record.status != null &amp;amp;&amp;amp; record.age &gt; 65) doSomething()
//     &lt;/recordClick&gt;
// &lt;/ListGrid&gt;
// </pre>
// An alternative, for larger blocks of code, is to use the XML standard "CDATA" (character
// data) processing directive, which allows ampersand and other characters to be used without
// special notation:
// <pre>
// &lt;ListGrid&gt;
//     &lt;recordClick&gt;&lt;![CDATA[
//         if (record.status != null && record.age &gt; 65) doSomething()
//     ]]&gt;&lt;/recordClick&gt;
// &lt;/ListGrid&gt;
// </pre>
// <P>
// Overall, embedding code in XML can be awkward.  Isomorphic generally recommends that
// significant chunks of JavaScript code, such as non-trivial custom components, be moved to
// separate, purely JavaScript files, while code embedded in component XML is limited to simple
// expressions and short functions.  See the next section for details on keeping code in a
// separate file.
// </smartclient>
// <P>
// <h3>Event Handlers &amp; Scripting loaded components</h3>
// <P>
// You can retrieve the components in your loaded screen in order to add event handlers to
// them, call APIs on them, place them into layouts you programmatically create, and in general
// add dynamic behavior.  Retrieve the components via the +link{Canvas.getById()} API
// (note, when working with multiple screens, be sure to see the upcoming section about managing
// global IDs).
// <P>
// You can then add event handlers normally.  For example, say there is a ListGrid with ID
// "mainGrid" and a DynamicForm with ID "editForm" in the same screen, and you want to populate
// the form with whatever record is clicked on in the grid:
// <P>
// <smartclient>
// <pre>
//   isc.Canvas.getByID("mainGrid").addMethods({
//       recordClick : "editForm.editRecord(record)"
//   });
// </pre>
// </smartclient>
// <smartgwt>
// <pre>
//   ListGrid grid = (ListGrid)Canvas.getByID("mainGrid");
//   final DynamicForm form = (DynamicForm)Canvas.getByID("editForm");
//   grid.addRecordClickHandler(new RecordClickHandler() {
//       public void onRecordClick(RecordClickEvent event) {
//           form.editRecord(event.getRecord());  
//       }
//   });
// </pre>
// </smartgwt>
// <P>
// You can also add a loaded screen to an existing layout container.  For example, perhaps you've
// already written parts of the application via normal coding techniques, and now you want to take
// a screen defined in Component XML and place it in a particular Layout you've already created
// ("existingLayout" below) - just use +link{layout.addMember()} as usual:
// <smartgwt>
// <pre>
//    existingLayout.addMember(Canvas.getById("<i>componentId</i>"));
// </pre>
// </smartgwt>
// <smartclient>
// <pre>
//    existingLayout.addMember(isc.Canvas.getById("<i>componentId</i>"));
// </pre>
// </smartclient>
// Component XML files can also refer to components you have created programmatically, and incorporate
// them into layouts.  For example, if you have created a ListGrid component with ID "theGrid", you could
// refer to that grid using a <code>&lt;Canvas ref=""/&gt;</code> tag, which can be used anywhere a
// Canvas is expected.  For example:
// <pre>
// &lt;VLayout ... &gt;
//     &lt;members&gt;
//           &lt;Canvas ref="theGrid"/&gt;
//     &lt;/members&gt;
// &lt;/VLayout&gt;
// </pre>
// Note that this approach requires that the referenced component has been created <b>before</b>
// <code>loadScreen</code> is called.
// <P>
// <h3>Component XML and global IDs</h3>
// <P>
// A Component XML screen created in Visual Builder or via the
// +link{group:balsamiqImport,Balsamiq importer} will assign global IDs to all components
// generated from your mockup so that you can retrieve them by ID to add event handlers and
// call APIs.   However if you build an application out of multiple screens built at different
// times, these IDs can collide, which will cause components to overwrite each other as they
// each try to use the same ID.
// <P>
// To solve this, the +link{RPCManager.loadScreen()} API will <i>ignore</i> global IDs on loaded
// components, assigning them sequential generated IDs instead (which will never collide). 
// Instead of using global IDs, the callback for <i>loadScreen()</i> will automatically provide
// you with the outermost component of a loaded screen, and that outermost component will
// provide access to other components by their original IDs via +link{canvas.getByLocalId()}.
// <P>
// This allows you to add loaded screens to existing layouts, attach event handlers and take
// other programmatic actions, all without ever establishing global IDs.
// <P>
// <h3>Loading multiple screens</h3>
// <P>
// A typical application that uses screens stored in Component XML will have several such
// screens, or in some cases, hundreds or thousands.  +link{RPCManager.cacheScreens()} can be
// used to load a set of screen definitions without actually creating any UI components - a
// subsequent call to +link{RPCManager.createScreen()} is used to actually create the screen
// when needed.  These two APIs provide the same global ID management facilities as
// +link{RPCManager.loadScreen(),loadScreen()}.
// <P>
// As discussed in the +link{group:smartArchitecture,SmartClient Architecture overview}, screen
// definitions are typically very small, and should be loaded entirely up front or in very
// large batches.  Further, screen <i>definitions</i> have essentially negligible runtime
// overhead until the screen is actually created.
// <P>
// Therefore, use the following best practices for screen loading, even if you have very few or
// only one screen defined in Component XML:
// <ul>
// <li> at application startup, load all screens using +link{RPCManager.cacheScreens()}
// <li> create screens lazily (when they are about to be shown to the end user) using
//      +link{RPCManager.createScreen()}.
// <li> for applications with very very large numbers of screens where loading all screen
//      definitions up front creates a very large download, consider multiple calls to
//      <code>cacheScreens()</code>, loading sets of screens that are likely to be used
//      together.
// </ul>
// <P>
// <h3>Dynamic Component XML</h3>
// <P>
// Components can be dynamically provided on the server side by using the API defined in
// +externalLink{http://www.smartclient.com/smartgwtee/server/javadoc/com/isomorphic/servlet/ScreenLoaderServlet.html#addDynamicScreenGenerator(com.isomorphic.servlet.DynamicScreenGenerator),<code>ScreenLoaderServlet.addDynamicScreenGenerator()</code>}
// which allows adding DynamicScreenGenerators to the system for providing the .ui.xml files on the fly:
// <pre>
// ScreenLoaderServlet.addDynamicScreenGenerator(new DynamicScreenGenerator() {
//      public String getScreen(String id) {
//
//        if (id.equals("testDynamicScreenPrefix3")) {
//            return null;
//        }
//
//        id=id.replace("testDynamicScreen","");
//        return "<VLayout ID=\""+id+"\" border=\"1px solid blue\"/>";
//      }
//  }, "testDynamicScreenPrefix");
// </pre>
// <P>
// Whenever the system needs a screen in future, it will first call the registered DynamicScreenGenerator's
// <code>getScreen(String)</code> method for providing the given screen; Only if all queried 
// DynamicScreenGenerators returns <code>null</code>, will proceed to use the normal system for obtaining 
// the screen instances.
// <P>
// NOTE:<ul>
// <li>If this API is used, DynamicScreenGenerator will be called for <b>every</b> screen that
// the framework needs. Instead of this, the API contains alternative methods which will allow
// adding DynamicScreenGenerators only for a given string prefix or a regular expression.
// </ul>
// <P>
// In the provided example we register a DynamicScreenGenerator which will be called for each screen
// the system tries to load, if it starts with "testDynamicScreenPrefix", except the screen with id
// "testDynamicScreenPrefix3" for which we return null.
// <P> 
// While registering a DynamicScreenGenerator is the first choice since is compatible 
// with ScreenLoaderServlet's ability to load several screens in a single HTTPRequest, 
// there are two additional ways to load Component XML screens - you can create a .jsp that
// uses the JSP tags that come with the SDK:
// <pre>
//    &lt;%@ taglib uri="/WEB-INF/iscTaglib.xml" prefix="isomorphic" %&gt;
//    &lt;isomorphic:XML&gt;
//       ... Component XML ...
//    &lt;/isomorphic:XML&gt;
// </pre>
// <P>
// Or you can use the server-side API com.isomorphic.XML.toJS():
// <pre>
//     XML.toJS("&lt;isomorphicXML xmlns:xsi=\"nativeType\"&gt;" +
//                  componentXMLCode +                                 
//                  "&lt;/isomorphicXML&gt;");
// </pre>
// However these two approaches will allow to only load one screen at a time.
// The JSP code above and the programmatic call to XML.toJS() both return a JavaScript code,
// which is the response that +link{RPCManager.loadScreen()} expects.  The
// <code>XML.toJS()</code> API can be easily combined with 
// +link{group:standaloneDataSourceUsage,direct use of the server-side DataSource API} to build
// a version of the ScreenLoaderServlet that can retrieve Component XML from a database
// or any Java API.
// <P>
// For static Component XML screens (cannot be changed at runtime), you can optionally run the
// XML.toJS() process as a build step to save a small amount of runtime overhead in XML to JS
// translation.  Use +link{RPCManager.loadScreen()} to load the resulting JavaScript by
// overriding the +link{rpcRequest.actionURL} to point to the generated JavaScript file.
// Note that the overhead is minor enough that this is not worth doing unless you have a very
// large deployment and a very large number of static Component XML files.
// <P>
// <h3>Troubleshooting</h3>
// <P>
// XML parsing errors, which happen when XML is not well-formed and would be rejected by any
// standard XML parser, are reported in the server-side log, and, when possible, also in the
// client-side log within the "Results" tab of the Developer Console.
// <P>
// If you are loading a screen via the +link{RPCManager.loadScreen()} API, you can see the
// response from the server in the RPC tab of the Developer Console - this will show you issues
// such as a misplaced ScreenLoaderServlet (HTTP response code will be 404 - Not Found) or
// responses that contain server exception details instead of the expected JavaScript response.
// <P>
// <smartclient>
// Other issues with component XML can result from incorrect use of SmartClient component XML
// tags.  For example, you may specify a property and it may appear to have no effect even
// though it clearly works in other, JavaScript-based examples.  If you get this symptom, you
// can troubleshoot by looking at the JavaScript code SmartClient generates from component XML.
// <P>
// </smartclient>
// <P>
// You can also use the "Eval XML" section in the "Results" tab of the Developer Console to
// interactively experiment with Component XML ("Eval XML" button) and as a means of seeing the
// generated JavaScript ("Show JS" button).
// <P>
// <h3>Localization / Internationalization</h3>
// <P>
// Component XML files support embedding references to messages loaded from ResourceBundles via
// the same JSTL-like &lt;fmt&gt; syntax as is used for DataSource .ds.xml files.  See
// +link{group:dataSourceLocalization,DataSource localization for details}.
// <P>
// <smartclient>
// <h3>Custom Properties</h3>
// <P>
// If you specify a custom property on a component in XML, for example:
// <pre>
// &lt;Canvas myProperty="false"/&gt;
// </pre>
// The value of the property will be a JavaScript String.  In the above example, it would be
// the string "false", which is considered a boolean true value in the JavaScript language.
// If you want a different JavaScript type, you can force a property to be interpreted as a
// given type by using the "xsi:type" attribute:
// <pre>
// &lt;Canvas&gt;
//     &lt;myProperty xsi:type="xsd:boolean"&gt;false&lt;/myProperty&gt;
// &lt;/Canvas&gt;
// </pre>
// The same notation works when you want to declare that an entire subobject has a given type.
// For example, this would cause the custom property "myListGrid" to have a live
// +link{ListGrid} instance as it's value.  All of the properties on the &lt;myListGrid&gt; tag
// will be correctly interpreted as ListGrid properties and have the correct types.
// <pre>
// &lt;Canvas&gt;
//     &lt;myListGrid xsi:type="ListGrid" width="500" height="600"/&gt;
// &lt;/Canvas&gt;
// </pre>
// If you do not want an actual live ListGrid, but rather a JavaScript Object containing
// properties for later construction of a ListGrid, use the <code>propertiesOnly</code>
// attribute.  For example, this code would cause the property "listGridProperties" to be a
// JavaScript Object with properties "width" and "height", whose values would be JavaScript
// Numbers.
// <pre>
// &lt;Canvas&gt;
//     &lt;listGridProperties xsi:type="ListGrid" propertiesOnly="true" 
//                          width="500" height="600"/&gt;
// &lt;/Canvas&gt;
// </pre>
// For your reference: "xsi" stands for "XML Schema Instance"; this notation derives from XML
// Schema standards for explicitly specifying type inline.
// <P>
// <h3>Custom Components</h3>
// <P>
// If you use +link{classMethod:ClassFactory.defineClass(),defineClass()} to define a new component class
// "MyListGrid" which is a subclass of the built-in component ListGrid, you can create it in
// XML as shown below:
// <pre>
// &lt;ListGrid constructor="MyListGrid" width="500"/&gt;
// </pre>
// By using the &lt;ListGrid&gt; tag you advertise that properties should be interpreted 
// as <code>ListGrid</code> properties.  By specifying <code>constructor</code>
// you tell SmartClient what class to +link{Class.create,create()}.
// <P>
// <h3>Component Schema</h3>
// <P>
// Instead of using the <code>constructor</code> and <code>xsi:type</code> attributes for
// custom components and custom properties, you can create a +link{group:componentSchema} that
// describes the custom component.  Declaring a component schema allows you to use your
// component just like the built-in SmartClient components, and also allows your component to
// be used within +link{group:visualBuilder}.
// </smartclient>
//
// <smartgwt>
// <h3>Custom Components</h3>
// <p>
// If you define a new component class <code>com.mycompany.MyListGrid</code> which is a subclass of the
// built-in component <code>ListGrid</code>, and you +link{group:reflection,register your class for reflection},
// you can create it in XML as shown below:
// <pre>
//    &lt;ListGrid constructor="com.mycompany.MyListGrid" width="500"/&gt;
// </pre>
// <p>
// By using the &lt;ListGrid&gt; tag you advertise that properties should be interpreted 
// as <code>ListGrid</code> properties.  By specifying <code>constructor</code>
// you tell SmartGWT what class to create. 
// <p>
// <h3>Custom Properties</h3>
// <p>
// Your custom component (e.g. <code>com.mycompany.MyListGrid</code>) may have additional
// properties which are not present in the standard superclass (e.g. <code>ListGrid</code>).
// You can set such properties in XML as if they were pre-defined properties:
// <pre>
//    &lt;ListGrid constructor="com.mycompany.MyListGrid" myProperty="false"/&gt;
// </pre>
// <p>
// In this case, the {@link com.smartgwt.client.bean.BeanFactory} code
// will ultimately call <code>MyListGrid.setMyProperty(false);</code> in order
// to set the property. Since <code>BeanFactory</code> knows that the property
// takes a boolean, it will automatically convert the string value "false" to a
// boolean, using the type conversions described below.
// <p>
// Instead of relying on the automatic type conversions, you can force a
// property to be interpreted as a given type by using the "xsi:type"
// attribute:
// <pre>
// &lt;ListGrid&gt;
//     &lt;constructor&gt;com.mycompany.MyListGrid&lt;/constructor&gt;
//     &lt;myProperty xsi:type="xsd:boolean"&gt;false&lt;/myProperty&gt;
// &lt;/ListGrid&gt;
// </pre>
// <p>
// The same notation works when you want to declare that an entire subobject has a given type.
// For example, this would cause the custom property "myListGrid" to have a live
// +link{ListGrid} instance as its value.  All of the properties on the &lt;myListGrid&gt; tag
// will be correctly interpreted as ListGrid properties and have the correct types.
// <pre>
// &lt;Canvas&gt;
//     &lt;myListGrid xsi:type="ListGrid" width="500" height="600"/&gt;
// &lt;/Canvas&gt;
// </pre>
// <p>
// For your reference: "xsi" stands for "XML Schema Instance"; this notation derives from XML
// Schema standards for explicitly specifying type inline.
// <p>
// <h3>Component Schema</h3>
// <p>
// Instead of using the <code>constructor</code> and <code>xsi:type</code> attributes for
// custom components and custom properties, you can create a +link{group:componentSchema} that
// describes the custom component.  Declaring a component schema allows you to use your
// component just like the built-in SmartGWT components, and also allows your component to
// be used within +link{group:visualBuilder}.
// <p>
// <h3>Type Conversions</h3>
// <p>
// The {@link com.smartgwt.client.bean.BeanFactory} code
// uses a reflection-like mechanism to discern the type which a SmartGWT
// property requires, and automatically
// converts supplied values to the required type when possible. In cases where
// conversion is impossible, an {@link IllegalArgumentException} is
// thrown.
// <p>
// Where the setter for a property takes a <b>primitive type</b> (boolean,
// double, float, int, or long), any "null" value supplied will be converted to
// 0 (for the numeric types) or false (for boolean). Conversely, if the setter
// takes the boxed version of the type (Boolean, Double, etc.), any primitive
// value supplied will be auto-boxed. Note that byte, short and char properties
// are not currently handled.
// <p>
// Properties which take <b>numeric types</b> will convert other numeric
// types, as well as strings, using standard Java APIs (e.g.
// <code>Integer.valueOf()</code>).  Boolean "true" will be converted to 1, and false to 0.
// If the supplied value cannot be converted to the numeric type, an
// {@link IllegalArgumentException} will be thrown.
// <p>
// Properties which take a <b>Date</b> type will convert from strings using
// {@link com.smartgwt.client.util.DateUtil#parseInput(String)}.
// <p>
// Properties which take <b>Enum</b> types will convert from strings using <code>Enum.valueOf()</code>.
// However, any dashes ("-") in the string will be converted to underscores, and
// the string will be converted to upper-case if necessary. If the string does not match
// one of the <code>Enum</code> values, an {@link IllegalArgumentException} will be thrown.
// <p>
// Properties which take <b>Array</b> types will convert arrays where the individual
// values can be converted to the appropriate type. If a single value is supplied,
// it will be wrapped in an array.
// </smartgwt>
//
// @treeLocation Concepts/Visual Builder
// @title Component XML
// @visibility external
//<

// <smartgwt>
//> @groupDef reflection
//
// In order to specify a SmartGWT class as a constructor in
// +link{group:componentXML, Component XML} or 
// +link{group:componentSchema, Component Schema},
// or for other purposes, such as for 
// {@link com.smartgwt.client.docs.AutoChildUsage autoChildren} or for 
// {@link com.smartgwt.client.data.DataSourceField#setEditorType(Class)}, 
// you must first register the class with the 
// {@link com.smartgwt.client.bean.BeanFactory} reflection mechanism.
// <p>
// If you want to register {@link com.smartgwt.client.widgets.Canvas}
// and all its subclasses found in the classpath (including your custom subclasses), 
// you can use the {@link com.smartgwt.client.bean.BeanFactory.CanvasMetaFactory}
// interface to do this automatically:
// <blockquote><pre>
// GWT.create(BeanFactory.CanvasMetaFactory.class);</pre></blockquote>
// <p>
// Similarly, to register {@link com.smartgwt.client.widgets.form.fields.FormItem}
// and all its subclasses found in the classpath (including your custom subclasses),
// you can use the {@link com.smartgwt.client.bean.BeanFactory.FormItemMetaFactory}.
// <blockquote><pre>
// GWT.create(BeanFactory.FormItemMetaFactory.class);</pre></blockquote>
// <p>
// Alternatively, if only specific classes need to be instantiated and
// configured dynamically, you can register just those classes by annotating
// them with the {@link com.smartgwt.client.bean.BeanFactory.Generate}
// annotation instead. For instance:
// <blockquote><pre>
// {@literal @}BeanFactory.Generate
// public class MyCanvas extends Canvas {
//     ...
// }</pre></blockquote>
// <p>
// For framework classes (where you cannot annotate the class directly), you
// can supply an array of Class literals to the annotation. For instance:
// <blockquote><pre>
// {@literal @}BeanFactory.Generate({Canvas.class, TreeGrid.class})
// public interface EmptyInterface {
//     ...
// }</pre></blockquote>
// <p>
// When you supply an array of class literals, the class you annotate
// (here <code>EmptyInterface</code>) will <b>not</b> itself have a 
// BeanFactory generated for it. Thus, you can use an empty inner
// interface for this purpose.
// <p>
// If there are only a limited number of classes which require dynamic
// configuration, it will save code size to use the
// {@link com.smartgwt.client.bean.BeanFactory.Generate} annotation to generate
// factories for those specific types, rather than using 
// {@link com.smartgwt.client.bean.BeanFactory.CanvasMetaFactory} or
// {@link com.smartgwt.client.bean.BeanFactory.FormItemMetaFactory}. Once a factory
// is generated for a class, GWT's opportunities to prune dead code are more
// limited for that class, since it cannot know what properties will be set or
// retrieved at run-time.
//
// @title Registering Classes for Reflection
// @visibility external
//<
// </smartgwt>

//> @groupDef componentSchema
//
// A component schema is a special type of DataSource that describes a custom component.  
// <P>
// Declaring a component schema for your custom component allows you to:
// <ul>
// <li> use simpler XML when creating your custom component: avoid having to specify the
// <code>constructor</code> and <code>xsi:type</code> attributes as described under
// +link{group:componentXML}
// <li> use your custom component within +link{group:visualBuilder}
// </ul>
// <P>
// <b>Example of a Component Schema</b>
// <P>
// In its most basic form, a component schema for a custom subclass of ListGrid called
// "<smartgwt>com.mycompany.</smartgwt>MyListGrid" looks like this:
// <pre>
// &lt;DataSource serverType="component" ID="MyListGrid" 
//             inheritsFrom="ListGrid" instanceConstructor="<smartgwt>com.mycompany.</smartgwt>MyListGrid"/&gt;
// </pre>
// With this definition saved as "MyListGrid.ds.xml" in the project dataSources directory
// ([webroot]/shared/ds/ by default), you can now create an instance of 
// <smartgwt>com.mycompany.</smartgwt>MyListGrid with just:
// <pre>
// &lt;MyListGrid width="500"/&gt;
// </pre>
// Note: you may need to restart your servlet engine/J2EE container before this example will
// work.
// <P>
// The attributes set directly on the DataSource tag have special meaning for a component
// schema definition:
// <ul>
// <li>+link{dataSource.serverType,serverType}="component" indicates this DataSource describes
// a component, as opposed to a SQL table or other data provider
// <li>+link{dataSource.ID,ID} means the tagName that will be used to create your custom
// component.  This must match the first component of the filename. (ID="MyListGrid" means the filename 
// must be MyListGrid.ds.xml, and typically also matches the name of the class).
// <li>+link{dataSource.inheritsFrom}="ListGrid" inherits the ListGrid property definitions via
// +link{dataSource.inheritsFrom}.  
// <li>instanceConstructor="<smartgwt>com.mycompany.</smartgwt>MyListGrid" indicates the 
// <smartclient>SmartClient</smartclient><smartgwt>SmartGWT</smartgwt> class that
// <smartclient>+link{Class.create,create()}</smartclient> should be called on to construct 
// an instance.
// <smartclient>
// <li>showLocalFieldsOnly is a boolean that, when set to true, tells the +link{group:visualBuilder}
// to show only the fields declared in this schema in the component editor.  Otherwise fields
// inherited via +link{dataSource.inheritsFrom} (all the way up the chain) are also included.
// <li>showSuperClassEvents is a boolean that, like showLocalFieldsOnly, optionally restricts
// the list of events shown in the Events tab of the +link{group:visualBuilder} to those defined in
// this schema only.
// <li>showSuperClassActions is a boolean that optionally restricts the list of actions shown
// in the menu when you map a component Event to a component Action within +link{group:visualBuilder}
// to those defined in this schema only.
// </smartclient>
// </ul>
// <P>
// <b>Declaring custom properties</b>
// <P>
// Custom properties are declared via +link{dataSource.fields,fields} as for an ordinary
// +link{DataSource}.  As with ordinary DataSources, it is legal to redeclare inherited fields
// in order to modify properties such as +link{dataSourceField.editorType,field.editorType}.
// <P>
// The following DataSourceField properties have special significance when a component schema
// is used to process +link{group:componentXML,component XML}:
// <ul>
// <li> +link{dataSourceField.type,field.type} declares the type of the field, and hence the
// type of the <smartclient>JavaScript</smartclient> value your custom class will be 
// initialized with.  In order to declare subcomponents, can be set to the name of another component 
// (built-in or custom).  
// <li> +link{dataSourceField.multiple,field.multiple} declares that the field should always be
// array-valued even when only a single value is provided.  Also indicates that the field name
// should be used as a "wrapper tag" in the XML format for the component.
// <li> +link{dataSourceField.propertiesOnly,field.propertiesOnly}.  For fields that hold
// subcomponents, suppresses auto-construction to avoid +link{canvas.autoDraw,double drawing}
// and to allow subcomponents to be modified by their parent component before they are created
// and drawn
// </ul>
// When a component is edited within Visual Builder, the DataSource properties that normally
// influence databound forms will influence the Component Editor (for example, field.title,
// field.editorType).  In addition, the following properties have special significance in
// component editing and component drag and drop:
// <ul>
// <li> +link{dataSourceField.inapplicable,field.inapplicable} indicates that an inherited
// field is inapplicable in this component.
// <li> +link{dataSourceField.group,field.group} indicates what group the property should be
// placed in when editing in Visual Builder.
// <li> +link{dataSourceField.xmlAttribute,field.xmlAttribute}: indicates this field should
// serialize as an XML attribute.  Note that when constructing the component from XML, either
// an attribute or a subelement will continue to be accepted as means of specifying the field
// value, so this property is primarily set in order to make code generated by Visual Builder
// maximally compact or to make it conform to externally set standards.
// </ul>
// <P>
//
// <smartclient>
// <b>Declaring Events and Actions</b>
// <P>
// Events and Actions are declared via a methods array.  In order for a method to be considered
// an event, it needs to have a method definition in the methods array (or be publicly
// documented in the SmartClient reference) and have been added to
// the class as a +link{group:stringMethods,StringMethod} via +link{Class.registerStringMethods}.
// <p>
// In order for a method to be considered an action, it needs to have a method definition in
// the methods array and have the <code>action</code> property set to <code>true</code>.  For
// example, the following is a definition of the 'hide' action available on any Canvas, as
// copied from Canvas.ds.xml:
// <pre>
//     &lt;methods&gt;
//         &lt;method name="hide" title="Hide" action="true"/&gt;
//     &lt;/methods&gt;
// </pre>
// For custom component actions, an array of expected parameters may be specified using the
// <code>params</code> attribute. Each entry in this array should have a specified type.
// By doing this, you allow the visual builder to pass parameters through to actions when setting 
// up events that call actions (possibly on another component).
// For example if you had a component with a custom action that expected to be passed a single
// parameter of type +link{class:ListGridRecord} you could define it as follows:
// <pre>
//     &lt;method name="showRecordDetails" title="Show Record Details" action="true"&gt;
//         &lt;params&gt;
//             &lt;param type="ListGridRecord"/&gt;
//         &lt;params&gt;
//     &lt;/method&gt;
// </pre>
// If a user working within the visualBuilder then added ListGrid to the page and used the "+" icon
// to associate the +link{ListGrid.recordClick(),recordClick} event with this custom method, the
// Visual Builder logic would automatically associate the parameters available in the fired event
// (in this case <code>recordClick</code>) with the expected parameters for the action to fire 
// by type. So the <code>record</code> parameter of the event (known to be of type 
// <code>ListGridRecord</code>) would be passed through to this custom <i>showRecordDetails</i>
// action as the first parameter.
// </smartclient>
//
// @treeLocation Concepts/Visual Builder
// @title Component Schema
// @visibility external
//<

});

// shortcut
isc.xml = isc.XML = isc.XMLTools;
