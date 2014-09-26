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

 





isc.defineClass("StringMethod");

// Actual string value of the method is stored in the "value" property

isc.StringMethod.addMethods({

getValue : function () {
    return this.value;
},

// Helper method to get a 'display value' for the stringMethod
// Returns the expression / body of the function or, for actions, the title of the action
getDisplayValue : function () {
    var value = this.getValue();
    if (value == null || isc.isA.String(value)) return value;
    if (value.title != null) return "[" + value.title + "]"
    // If we were created with a string value, return the raw expression
    return value;
    
},
        
// not allowed to have ]]> in a CDATA block
cdata : function (string) {
    var index = string.indexOf("]]>");
    if (index == -1) return "<![CDATA[" + string + "]]>";
    return this.cdata(string.slice(0, index)) + "]]&gt;" + this.cdata(string.slice(index+3));
},

_xmlSerialize : function (name, type, namespace, prefix, refs, path) {
    var value = this.value;
    if (isc.isA.String(value)) return isc.Comm._xmlValue(name, this.cdata(value), 
                                      type || "stringMethod", namespace, prefix);  
    else 
        return isc.StringMethod._xmlSerializeAction(value, name, prefix, refs, path);
        
}

});


// NOTE: toString functions CANNOT be added by addMethods, because a property named "toString"
// will not be enumerated by for..in.  This is actually part of the ECMAScript standard!

isc.StringMethod.getPrototype().toString = function () {
    var value = this.getValue();
    if (value == null || isc.isA.String(value)) return value;
    return value.toString();
},

isc.StringMethod.addClassMethods({

_$Action:"Action",
_xmlSerializeAction : function (action, name, indent, refs, path) {

        var actionDS = isc.DataSource.get(this._$Action);   
        if (!actionDS) return isc.Comm._xmlSerializeObject(name, action, path, refs, indent);

        return [isc.Comm._xmlOpenTag(name),
                 actionDS.xmlSerialize(action, null, indent + "        ", this._$Action),
                 "\n", indent,
                 isc.Comm._xmlCloseTag(name)].join(isc.emptyString);

}

})
