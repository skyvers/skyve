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

 





//
//=	@object	XMLSerialize
//
//	xml serialize() methods for the comm package
//

// XXX this package must not be dependant on the Comm package, because serialization is a useful
// feature completely apart from Comm.  Unfortunately, the methods are currently expected to be on
// the Comm class, so if the Comm class doesn't exist we need to define it.
if (!isc.Comm) isc.defineClass("Comm");

isc.Comm.addClassProperties( {
	// prefixes for special object types

    //>	@classAttr	Comm.XML_BACKREF_PREFIX (String : "$$BACKREF$$:" : IR)
	//		@group	serialization
	//			Prefix for back-references in serialized object references.
	//<
	XML_BACKREF_PREFIX : "$$BACKREF$$:",
    
    _xmlIdentifierRegex : /^([_:A-Za-z])([_:.A-Za-z0-9]|-)*$/,
    // this property only applies to XMLSerialize
    serializeBackrefs : true
});


isc.Comm.addClassMethods( {

//>	@classMethod	Comm.xmlSerialize()
//			Serialize an object of any type into an xml string, in a form that
//			 can be read by server-side code via schemaless transform
//
//		Note: You should call this routine to serialize any type of object,
//				 rather than calling the custom routines on that object...
//		
//		@group	xml serialization
//		@param	name		(any)		name of object to be serialized (used for outer XML tag)
//		@param	object		(any)		object to be serialized
//		@param	[indent]	(boolean)	true == output should be indented for reading, 
//                                      false == no indentation
//
//		@return				(string)	serialization form of the object
//<
xmlSerialize : function (name, object, indent) {
	return isc.Comm._xmlSerialize(name, object, indent ? "" : null);
},
//>	@classMethod	Comm._xmlSerialize()	(IA)
//			Internal routine that actually does the serialization.
//		@group	serialization
//
//		@param	name	(string)	name of the object for XML tags
//		@param	object	(any)		object to serialize
//		@param	prefix	(string)	string to put before each line of serialization output
//		@param	context (object)	context tracking objects already serialized and path
//                                  traversed so far
//
//		@return	(string)			serialized object as a string
//<
_xmlSerialize : function (name, object, prefix, context) {

    // Avoid attempting to manipulate SGWT Java objects
    if (isc.Browser.isSGWT && window.SmartGWT.isNativeJavaObject(object)){
        
        if (object == null) object = null;
        // If the global flag has been set to warn when we hit an unconvertible
        // object, do this.
        else {
            if (window.SmartGWT.warnOnSerializeError) {
                window.SmartGWT.throwUnconvertibleObjectException(
                    object, window.SmartGWT.serializeErrorMessage 
                );
            }
            object = String.asSource(object + "");
        }
    }

    // record whether a name was explicitly passed
    var namePassed = name != null;

    // NOTE: allow context as a partial object, so eg isRoot can be set
	if (!context || !context.objRefs) {
		context = isc.addProperties({}, context);
		context.objRefs = {obj:[],path:[]};
		if (!context.objPath) {
			if (object && object.getID) context.objPath = object.getID();
			else context.objPath = "";
		}
		if (name == null) {
			if (isc.isA.Class(object)) name = object.getClassName();
			else if (isc.isAn.Array(object)) name = "Array";
			else if (isc.isA.Object(object)) name = object.$schemaId || "Object";
			else name = "ISC_Auto";
		}
	}

	// handle simple types

    // NOTE: in some use cases we need be able to send null, which potentially has a distinct
    // meaning from empty string (""), for example, nulling out a text field vs setting to
    // empty string.  In this case null is encoded distinctly by setting the attribute xsi:nil.
    // Note schema-driven serialization in DataSource.js does a similar thing but only for
    // fields marked nillable:true in schema.
	if (object == null) {
        if (isc.Comm.xmlSchemaMode || !isc.Comm._explicitNils) {
            return isc.Comm._xmlValue(name, "");
        } else {
            // send explicit null
            return isc.Comm._xmlValue(name, null, "nil");
        }
    }

	if (isc.isA.String(object))	{
        return isc.Comm._xmlValue(name, isc.makeXMLSafe(object),
                                  (isc.Comm.xmlSchemaMode ? "string" : null));
    }
	if (isc.isA.Function(object)) {
        if (object.iscAction) return isc.StringMethod._xmlSerializeAction(object.iscAction);
        return null;
    }

    if (object == window) {
        this.logWarn("Serializer encountered the window object at path: " + context.objPath
                    +" - returning null for this slot.");
        return null;
    }

    // XML comm supports strong typing of numbers and booleans, but JS comm does not (the type
    // information is not propagated). Preserving the type is useful, so we default to that - but
    // this can be disabled
    if (isc.RPCManager.preserveTypes) {
        // for numbers, distinguish between float and integer
        // NOTE: special numbers like NaN and Infinity aren't allowed in the XML Schema numeric
        // types - the XML schema approach here would be to declare a union type between a
        // numeric base type and an enum of NaN, Infinity, etc.
    	if (isc.isA.Number(object) || isc.isA.SpecialNumber(object)) {
            if (object.toString().contains(".")) 
                return isc.Comm._xmlValue(name, object, "double");
            return isc.Comm._xmlValue(name, object, "long");
        }
	    if (isc.isA.Boolean(object)) return isc.Comm._xmlValue(name, object, "boolean");
    } else {
        // old approach
    	if (isc.isA.Number(object) || isNaN(object)) {
            return isc.Comm._xmlValue(name, object);
        }
	    if (isc.isA.Boolean(object))	return isc.Comm._xmlValue(name, object);
    }

	// for complex types:
	
	// detect infinite loops by checking if we've seen this object before.
    // disambiguate between true loops vs the same leaf object being encountered twice
    // (such as a simple Array containing two Strings which appears in two spots).  Only
    // consider this a loop if the preceding occurrence of the object was some parent of
    // ours.  
	var prevPath = isc.JSONEncoder._serialize_alreadyReferenced(context.objRefs, object);
	if (prevPath != null && context.objPath.contains(prevPath)) {
        // Note: check that the first char after "prevPath" is a path separator char in order
        // to avoid false loop detection with "prop" and "prop2" having the same non-looping
        // object (since "prop2" contains "prop").
        var nextChar = context.objPath.substring(prevPath.length, prevPath.length+1);
        //this.logWarn("backref: prevPath: " + prevPath + ", current: " + context.objPath +
        //             ", char after prevPath: " + nextChar);
        if (nextChar == "." || nextChar == "[" || nextChar == "]") {
            if (this.serializeBackrefs) {
    	    	return isc.Comm._xmlOpenTag(name) + 
                                isc.Comm.XML_BACKREF_PREFIX + prevPath + 
                       isc.Comm._xmlCloseTag(name);
            }
            return isc.emptyString;
        }
	}
	
	// remember Objects and Arrays to avoid infinite loops
	isc.JSONEncoder._serialize_remember(context.objRefs, object, context.objPath);
	
	// if there is an xmlSerialize method associated with this object, call that
	if (isc.isA.Function(object._xmlSerialize)) {
        return object._xmlSerialize(name, null, null, prefix, context.objRefs, context.objPath);
    } else if (isc.isA.Class(object)) {
        this.logWarn("Attempt to serialize class of type: " + object.getClassName()
                     + " at path: " + context.objPath + " - returning null for this slot.");
        return null;
    }

    // we define the xsi namespace on the first nested object that we encounter.  The first such
    // object sets the value isRoot on the context to 'false' explicitly.  If it's not defined, then
    // it's true.
    var isRoot = context.isRoot == false ? false : true;

	// handle arrays as a special case
	if (isc.isAn.Array(object))	
        return isc.Comm._xmlSerializeArray(name, object, context.objPath, 
                                           context.objRefs, prefix, isRoot);

    var data;
	// if the object has a getSerializeableFields, use whatever it returns, otherwise just use the object
    if (object.getSerializeableFields) {
        
		data = object.getSerializeableFields([], []);
    } else {
        data = object;
    }

	return isc.Comm._xmlSerializeObject(name, data, context.objPath, 
                                        context.objRefs, prefix, isRoot);
},

//>	@classMethod	Comm._xmlSerializeArray()	(A)
//			Internal routine to serialize an array.
//
//		@group	serialization
//		@param	name	(string)	name of the object for XML tags
//		@param	object	(any)		object to serialize
//		@param	prefix	(string)	string to put before each line of serialization output
//		@param	objRefs	(object[])	array of objects that have been serialized already so
//									 we don't get into endless loops		
//		@param	objPath	(string)	global variable path to this object, for serializing object references
//
//		@return	(string)			serialized object as a string
//<
_xmlSerializeArray : function (name, object, objPath, objRefs, prefix, isRoot) {

	// open xml tag
	var result = isc.Comm._xmlOpenTag(name, "List", null, null, null, isRoot);

	// spin through the array and create <elem>value</elem> strings
	for (var i = 0, len = object.length; i < len; i++) {
		var value = object[i];
        var context = {
            objRefs : objRefs,
            objPath : isc.JSONEncoder._serialize_addToPath(objPath, i),
            isRoot : false
        };
		result = isc.StringBuffer.concat(
				result,
				(prefix != null ? isc.StringBuffer.concat("\r", prefix, "\t") : ""),
				isc.Comm._xmlSerialize((value != null ? value.$schemaId : null) || "elem", 
                                       value, 
									   (prefix != null ? isc.StringBuffer.concat(prefix, "\t") : null),
                                       context)
				);
	}

	// close xml tag
	result = isc.StringBuffer.concat(
			result,
			(prefix != null ? isc.StringBuffer.concat("\r", prefix) : ""),
			isc.Comm._xmlCloseTag(name)
			);

	return result;
},

_isValidXMLIdentifier : function (name) {
    // XMLSerialize is used to transform arbitrary JS structures, including object literals
    // with strings for keys.  XML accepts only a subset of characters that are valid in a
    // string.  We encode them in an attribute value and have the server reconstitute them via
    // the special _isc_name encoding.  But requests sent out of band of our server (direct
    // webservices for example) can't be helped in this manner.  For those, we simply punt and
    // expect users to provide valid identifiers.
    //
    // It would be useful to report bad identifiers to the DeveloperConsole.  Unfortunately,
    // the JS regexp character classes aren't powerful enough for us to do this without
    // recapitulating the unicode character ranges verbatim from the spec, which would be slow
    // and take up a lot of space.
    //
    // Note that our regexp matches a subset of the valid identifiers, but this is harmless
    // since our server reconstitues these.
    // 
    // Spec is here:
    // http://www.w3.org/TR/REC-xml/#NT-Letter
    return isc.Comm.xmlSchemaMode || name.match(this._xmlIdentifierRegex);
},

//>	@classMethod	Comm._xmlSerializeObject()	(A)
//			Internal routine to serialize an object.
//
//		@group	serialization
//		@param	object	(any)		object to serialize
//		@param	prefix	(string)	string to put before each line of serialization output
//		@param	objRefs	(object[])	array of objects that have been serialized already so
//									 we don't get into endless loops		
//		@param	objPath	(string)	global variable path to this object, for serializing object references
//
//		@return	(string)			serialized object as a string
//<
_xmlSerializeObject : function (name, object, objPath, objRefs, prefix, isRoot) {

    // if it's a class or has the special _constructor property, then the name is the class name
    // this allows us to hand the output of this method to the server-side xml parser and get back
    // a DataSource-validated object back.
	// Aug 2008 - moved this check before the call to isc.Comm._xmlOpenTag, to ensure that it
	// uses the correct name for non-Class objects with a _constructor - without this change,
	// it was returning mismatched open and close tags
    // April 2010 - added "RelativeDate" as a class-name to ignore, so that relative dates
    // can be sent up to the server as part of criteria without having their "value" property
    // renamed
    if (isc.isAn.Instance(object)) name = object.getClassName();
    else if (object._constructor && object._constructor != "AdvancedCriteria" &&
        object._constructor != "RelativeDate") name = object._constructor;

	// open xml tag
    //
    // NOTE: we do need to explicitly label the structure we're about to write out as an "Object",
    // because for a single-property object like { values : { locale : 10 } } we'd currently write:
    // <container>
    //   <values>
    //     <someProperty>10</someProperty>
    //   </values>
    // </container>
    // Without an explicit declaration that "values" is of Object type, this could be
    // interpreted as values being a subobject with a single property someProperty, or
    // <someProperty> being a *type name* which will be the value of the property "values".
    // Adding "Object" below causes us to write values as <values xsi:type="xsd:Object" .. >,
    // removing the ambiguity.
    var result = isc.Comm._xmlOpenTag(name, "Object", null, null, null, isRoot);

    var undef;

    object = isc.JSONEncoder._serialize_cleanNode(object);
		
	// for each key in the object
	for (var key in object) {

		if (key == null) continue;

        
		if (key == isc.gwtRef || key == isc.gwtModule) continue;

        // XML identifiers can't start with $ (parser crashes)
        if (key.startsWith('$')) continue;

		var value = object[key];

        // NOTE: null is a real value
        if (value === undef) continue;

		// if the value is a function, skip it
        // Exception - we can serialize actions by looking at function.iscAction - in this
        // case retain it
		if (isc.isA.Function(value) && !value.iscAction) continue;

		// convert the key to a string
		var keyStr = key.toString();
		
        var context = {
            objRefs: objRefs,
            objPath: isc.JSONEncoder._serialize_addToPath(objPath, key),
            isRoot: false
        };
    
		// transform the value
		result = isc.StringBuffer.concat(
				result,
				(prefix != null ? isc.StringBuffer.concat("\r", prefix, "\t") : ""),
				isc.Comm._xmlSerialize(keyStr, value,
									   (prefix != null ? isc.StringBuffer.concat(prefix, "\t") : null),
                                       context)
				);
	}
	
	// close xml tag
	result = isc.StringBuffer.concat(
			result,
			(prefix != null ? isc.StringBuffer.concat("\r", prefix) : ""),
			isc.Comm._xmlCloseTag(name)
			);

	return result;
},

_getPrefix : function (prefixes, namespace) { 
    if (prefixes[namespace] != null) {
        // re-use a declared prefix
        return prefixes[namespace];
    } else {
        // establish a new NSURI -> prefix mapping
        if (prefixes._nsCount == null) prefixes._nsCount = 0;
        return (prefixes[namespace] = "ns" + prefixes._nsCount++); 
    }
}, 


// helper method - returns an xml open tag with the (optional) type.
_xmlOpenTag : function (tagName, type, namespace, prefix, leaveOpen, isRoot) {

    var output = isc.SB.create();

    var writeNamespace = namespace != null;

    // if "prefix" is passed as an object, use it to accrue a map from namespace to namespace
    // prefix, but don't actually write out any namespaces, relying on the calling code to do
    // so 
    if (namespace != null && isc.isAn.Object(prefix)) {
        writeNamespace = false;
        prefix = this._getPrefix(prefix, namespace);
    }

    // encode the name in '_isc_name' if it's not a valid XML identifier
    var extraXML = '';
    if (!this._isValidXMLIdentifier(tagName)) {
        extraXML = ' _isc_name="' + isc.makeXMLSafe(tagName) + '"';
        tagName = "Object";
    }

    if (namespace) {
        prefix = prefix || "schNS";
        output.append("<", prefix, ":", tagName);
        if (writeNamespace) output.append(" xmlns:", prefix, "=\"", namespace, "\"");
    } else {
        output.append("<", tagName);
    }
    if (extraXML) output.append(extraXML);

    // if the object is root-level, we add the xsi namespace declaration to
    // allow usage of xsi types inline
    if (isRoot && !this.omitXSI) {
        output.append(" xmlns:xsi=\"http://www.w3.org/2000/10/XMLSchema-instance\"");
    }

    // if an xsi type is passed in for this object, mark the object with that type
    if (type && !this.omitXSI) {
        output.append(" xsi:type=\"xsd:", isc.makeXMLSafe(type), "\"");
    }

    if (!leaveOpen) output.append(">");

    return output.release(false);
},

// helper method - returns an xml close tag
_xmlCloseTag : function (name, namespace, prefix) {

    if (namespace != null && isc.isAn.Object(prefix)) {
        prefix = this._getPrefix(prefix, namespace);
    }

    if (!this._isValidXMLIdentifier(name)) name = "Object";

    if (namespace) {
        prefix = prefix || "schNS";
	    return isc.SB.concat("</", prefix, ":", name, ">");
    } else {
	    return isc.SB.concat("</", name, ">");
    }
},

// helper method - returns the passed in value verbatim, sandwiched between the outputs of
// _xmlOpenTag and _xmlClosetTag methods with the optional type.
_xmlValue : function (name, value, type, namespace, prefix) {
        if (type == "base64Binary") {
            value = "<xop:Include xmlns:xop=\"http://www.w3.org/2004/08/xop/include\" href=\""
                + value + "\"/>";
        }
        if (type == "nil") {
		    return isc.Comm._xmlOpenTag(name, null, namespace, prefix, true)
                        + " xsi:nil=\"true\"/>";

        }
		return isc.StringBuffer.concat(
				isc.Comm._xmlOpenTag(name, type, namespace, prefix),
				value,
				isc.Comm._xmlCloseTag(name, namespace, prefix)
				);
}

});	// END isc.addMethods(isc.Comm, {})
