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

 







//>	@object	isA
//
//	A library of functions for determining the types of other objects.<br><br>
//
//  The "isA" methods for the basic JavaScript types are much faster and more consistent across
//  platforms than JavaScript's "typeof" operator.<br><br>
//
//  An isA method is automatically created for every ISC Class and Interface definition, for
//  example, isA.Canvas().<br><br>
//
//	@example	<code>if (isA.Number(myVariable)) ...</code>
//
//	Note: <code>is</code> and <code>isAn</code> are synonyms of <code>isA</code> and can be used
//			interchangably when it looks better syntactically, eg:
//				<code>if (myObject == null) ...</code>
//			or
//				<code>if (isAn.Array(myObject)) ...</code>
// @treeLocation Client Reference/System
// @visibility external
//<
// create the "isA", "isAn" and "is" objects
isc.addGlobal("isA", {});
isc.addGlobal("isAn", isc.isA);
isc.addGlobal("is", isc.isA);

  //>DEBUG
// give it a class name so that methods added to it get labelled
isc.isA.Class = "isA";
  //<DEBUG 

isc.isA.isc = isc.isA; // so you can do isc.isA.isc.Canvas(object)


Function.__nativeType = 1;
Array.__nativeType = 2;
Date.__nativeType = 3;
String.__nativeType = 4;
Number.__nativeType = 5;
Boolean.__nativeType = 6;
RegExp.__nativeType = 7;
Object.__nativeType = 8;



Function.prototype.__nativeType = 1;


// add methods to determine the type of various simple objects
isc.addMethods(isc.isA, {
    useTypeOf : isc.Browser.isMoz || isc.Browser.isSafari,

	//>	@classMethod isA.emptyString()
	//
	//	Is <code>object</code> the empty string?<br><br>
	//	
	//	NOTE: if you prefer, you can call this as <code>isAn.emptyString()</code>
	//
	//	@param	object	(object)	object to test
	//	@return			(boolean)	true == <code>object</code> is a null string
	//	@visibility external
	//<
	emptyString : function (object) {return isc.isA.String(object) && object == isc.emptyString},

	
	//>	@classMethod isA.nonemptyString()
	//
	//	Is <code>object</code> a non-empty String?<br><br>
	//	
	//	@param	object	(object)	object to test
	//	@return			(boolean)	true == <code>object</code> is a non-empty string
	//	@visibility external
	//<
	nonemptyString : function (object) {return isc.isA.String(object) && object != isc.emptyString},

	
	//>	@classMethod isA.Object()
	// Returns whether the passed value is a non-null Object.
    // <p>
    // Returns false for values that are Numbers, Strings, Booleans, Functions or are null or
    // undefined.  
    // <p>
    // Returns true for Object, Array, Regular Expression, Date and other kinds of
    // native objects which are considered to extend from window.Object.
	//
	// @param object (any) value to test for whether it's an object
	// @return (boolean) whether passed value is an Object
	// @visibility external
	//<
    //  With the exception of returning false for the null value, this function's return value 
    //  matches the ECMA spec for the typeof operator.  It also seems to be a reasonable expected 
    //  implementation of this method as it guarantees the programmer can work with properties of 
    //  the object as with a standard Object returned by "new Object()".
    _$object:"object",
    _$String :"String",
	Object : function (object) {
        if (object == null) return false;

        
        if (isc.Browser.isIE && typeof object == this._$function) return false;

        
        if (this.useTypeOf) {
            var objType = typeof object;
            return (objType == "object" || objType == "array" || objType == "date" ||
            
                    (isc.Browser.isMoz && objType == "function" && isc.isA.RegularExpression(object)));
        }   
        
        if (object.constructor && object.constructor.__nativeType != null) {
            var type = object.constructor.__nativeType;
            if (type == 1) {
                
            } else {
                // Object, RegExp, Date, Array
                return (type == 8 || type == 7 || type == 3 || type == 2);
            }
        }

        // Workaround for a core GWT bug, fixed as of GWT 2.5.
        // http://code.google.com/p/google-web-toolkit/issues/detail?id=4301
        if (object.Class != null && object.Class == this._$String) return false;

        
        if (typeof object == this._$object) {
            if (isc.Browser.isIE && isc.isA.Function(object)) return false;
            else return true;
        } else return false;
    },
    
	//>	@classMethod isA.emptyObject()
	//
	// Is <code>object</code> an object with no properties (i.e.: <code>{}</code>)?
    // <P>
    // Note that an object that has properties with null values is considered non-empty, eg 
    // <code>{ propName:null }</code> is non-empty.
    // <P>
	// NOTE: if you prefer, you can call this as <code>isAn.emptyObject()</code>
	//
	//	@param	object	(object)	object to test
	//	@return			(boolean)	true == <code>object</code> is the empty object
	//	@visibility external
	//<
    emptyObject : function (object) {
        if (!isc.isAn.Object(object)) return false;
        for (var i in object) {
            // if we have a single property we're non-empty!
            return false;
        }
        return true;
    },
    
	//>	@classMethod isA.emptyArray()
	//
	// Is <code>object</code> an Array with no items?
    //
	//	@param	object	(object)	object to test
	//	@return			(boolean)	true == <code>object</code> is an empty array
	//	@visibility external
	//<
    emptyArray : function (object) {
        return isc.isAn.Array(object) && object.length == 0;
    },

	//>	@classMethod	isA.String()
	//
	//	Is <code>object</code> a String object?
	//
	//	@param	object	(object)	object to test
	//	@return			(boolean)	true == <code>object</code> is a String
	//	@visibility external
	//<
    // ==========================================================================================
    // IMPORTANT: If you update this function, also update its copy in FileLoader.js
    // ==========================================================================================
	String : function (object) {
        if (object == null) return false;

        
        if (this.useTypeOf) {
            return typeof object == "string" || 
                (object.Class != null && object.Class == this._$String);
        }

        
        //if (typeof object == this._$function) return false;
        if (object.constructor && object.constructor.__nativeType != null) {
            return object.constructor.__nativeType == 4;
        }

        // Workaround for a core GWT bug
        // http://code.google.com/p/google-web-toolkit/issues/detail?id=4301
        if (object.Class != null && object.Class == this._$String) return true;

        return typeof object == "string";
	},

	//>	@classMethod	isA.Array()
	//
	//	Is <code>object</code> an Array object?<br><br>
	//
	//	NOTE: if you prefer, you can call this as <code>isAn.Array()</code>
	//
	//	@param	object	(object)	object to test
	//	@return			(boolean)	true == <code>object</code> is an Array
	//	@visibility external
	//<
	Array : function (object) {
        if (object == null) return false;

        
        if (this.useTypeOf && typeof object == "array") return true;

        
        if (typeof object == this._$function) return false;
        if (object.constructor && object.constructor.__nativeType != null) {
            return object.constructor.__nativeType == 2;
        }

        

        if (isc.Browser.isSafari) {
            var spliceString = "" + object.splice;
            return (spliceString ==  "function splice() {\n    [native code]\n}" ||
                    spliceString == "(Internal function)");
        }
        return ""+object.constructor == ""+Array;
	},

	//>	@classMethod	isA.Function()
	//
	//	Is <code>object</code> a Function object?
	//
	//	@param	object	(object)	object to test
	//	@return			(boolean)	true == <code>object</code> is a Function
	//	@visibility external
	//<
    _$function : "function",
	Function : function (object) {
        if (object == null) return false;
        
        
        if (isc.Browser.isIE && typeof object == this._$function) return true;

        // In IE9, attempting to access the "constructor" attribute of a window
        // can lead to an odd crash. If we're passed a native window, return false immediately.
        
        if (isc.Browser.isIE && (
                (object == window) || 
                (object.document != null && (object.toString != null) && 
                    object.toString().contains("Window") )
            )
           )
        {
            return false;
        }
        
        var cons = object.constructor;
        if (cons && cons.__nativeType != null) {
            // eliminate known non-functions from an ISC frame
            if (cons.__nativeType != 1) return false;
            // eliminate functions from this frame
            if (cons === Function) return true;
            
        }

        
        //if (!object.constructor) isc.Log.logWarn("obj without cons: " + isc.Log.echo(object));
//        isc.logWarn("obj:" + object + "cons:" + isc.emptyString + object.constructor);
        
        return isc.Browser.isIE ? (isc.emptyString+object.constructor == Function.toString()) : 
                                  (typeof object == this._$function);
    },

	//>	@classMethod	isA.Number()
	//
	//	Is <code>object</code> a Number object?<br><br>
	//
	//	NOTE: this returns false if <code>object</code> is an invalid number (<code>isNaN(object) == true</code>)
	//
	//	@param	object	(object)	object to test
	//	@return			(boolean)	true == <code>object</code> is a Number
	//	@visibility external
	//<
	Number : function (object) {
        if (object == null) return false;
        
        
        if (this.useTypeOf && typeof object == "number") {
            // it's a number, now check if it's a valid number
            return !isNaN(object) && 
                object != Number.POSITIVE_INFINITY && 
                object != Number.NEGATIVE_INFINITY;
        }

        if (object.constructor && object.constructor.__nativeType != null) {
            if (object.constructor.__nativeType != 5) return false;
        } else {
            if (typeof object != "number") return false;
        }
        // it's a number, now check if it's a valid number
        return !isNaN(object) && 
            object != Number.POSITIVE_INFINITY && 
            object != Number.NEGATIVE_INFINITY;
    },

	SpecialNumber : function (object) {
        // NOTE: we do need to first determine if it's a number because isNaN({}) is true
        if (object == null) return false;
        if (object.constructor && object.constructor.__nativeType != null) {
            if (object.constructor.__nativeType != 5) return false;
        } else {
            if (typeof object != "number") return false;
        }
        return (isNaN(object) || object == Number.POSITIVE_INFINITY ||
                object == Number.NEGATIVE_INFINITY);
    },

	//>	@classMethod	isA.Boolean()
	//
	//	Is <code>object</code> a Boolean object?
	//
	//	@param	object	(object)	object to test
	//	@return			(boolean)	true == <code>object</code> is a Boolean
	//	@visibility external
	//<
	Boolean	: function (object) {
        if (object == null) return false;
        if (object.constructor && object.constructor.__nativeType != null) {
            return object.constructor.__nativeType == 6;
        }
        return typeof object == "boolean";
    },
	
	//>	@classMethod	isA.Date()
	//
	//	Is <code>object</code> a Date object?
	//
	//	@param	object	(object)	object to test
	//	@return			(boolean)	true == <code>object</code> is a Date
	//	@visibility external
	//<
	Date : function (object) {
        if (object == null) return false;
        if (object.constructor && object.constructor.__nativeType != null) {
            return object.constructor.__nativeType == 3;
        }
        return (""+object.constructor) == (""+Date) &&
                // if the Date constructor is passed a string it doesn't understand, it returns a
                // sort of pseudo date object, which returns bad values from getYear(), etc.
                object.getDate && isc.isA.Number(object.getDate());
    },
    
	//>	@classMethod	isA.RegularExpression()
	//
	//	Is <code>object</code> a Regular Expression (RegExp) object?
	//
	//	@param	object	(object)	object to test
	//	@return			(boolean)	true == <code>object</code> is a Boolean
	//	@visibility external
	//<
	RegularExpression : function (object) {
        if (object == null) return false;
        if (object.constructor && object.constructor.__nativeType != null) {
            return object.constructor.__nativeType == 7;
        }
        return (""+object.constructor) == (""+RegExp);
    },

    
    _$textXML : "text/xml",
    XMLNode : function (object) {
        if (object == null) return false;
        if (isc.Browser.isIE) {
            return object.specified != null && object.parsed != null && 
                   object.nodeType != null && object.hasChildNodes != null;
        }
        var doc = object.ownerDocument;
        if (doc == null) return false;
        return doc.contentType == this._$textXML;
    },
	

	// ---------------------------------------------------------------------------------------
    // NOTE: the following few functions are used strictly in expressionToFunction(), are not
    // i18n-safe, and should not be externally visible
	// ---------------------------------------------------------------------------------------

 	//> @classMethod isA.AlphaChar() 
 	// 
 	//  Is the character passed in an alpha character? 
 	// 
 	//  @param  char    (string)        character to test 
 	//  @return                 (boolean)       true == character is alpha 
 	//< 
 	AlphaChar : function (character) { 
 		// XXX: does not yet deal with unicode characters or extended ASCII characters. 
 		var code = character.charCodeAt(0);
 		return ((code >= 65 && 
 				 code <= 90) || 
 				(code >= 97 && 
 				 code <= 122)) 
 	}, 
 	
 	//> @classMethod isA.NumChar() 
 	// 
 	//  Is the character passed in a Decimal (0-9) character? 
 	// 
 	//  @param  char    (string)        character to test 
 	//  @return                 (boolean)       true == character is a decimal character 
 	//< 
 	NumChar : function (character) { 
 		// XXX: does not yet deal with unicode characters 
 		var code = character.charCodeAt(0) 
 		return (code >= 48 && 
 				code <= 57) 
 	}, 
 	
 	//> @classMethod isA.AlphaNumericChar() 
 	// 
 	//  Is the character passed in alphanumeric? 
 	// 
 	//  @param  char    (string)        character to test 
 	//  @return                 (boolean)       true == character is alphanumeric 
 	//< 
 	AlphaNumericChar : function (character) { 
        return (isc.isA.AlphaChar(character) || isc.isA.NumChar(character)) 
    }, 
 	
 	//> @classMethod isA.WhitespaceChar() 
 	// 
 	//  Is the character passed in a whitespace character? 
 	//  This method considers any ascii character from 0-32 to be a whitespace character. 
 	// 
 	//  @param  char    (string)        character to test 
 	//  @return                 (boolean)       true == character is a whitespace character 
 	//< 
 	WhitespaceChar : function (character) { 
 		// XXX: does not yet deal with unicode characters 
 		var code = character.charCodeAt(0) 
 		return (code >= 0 && 
    			code <= 32) 
 	},
    
    //>@classMethod isA.color
    //  Is this a valid css color.  Used by the isColor() validator
    //<
    
    color : function (object) {
        if (!isc.isA.String(object)) return false;
            
        if (!this._cssColorRegexp) {
            this._cssColorRegexp = new RegExp( 
                            // hex:         "#D3D3D3", etc                
                            "^(#([\\dA-F]{2}){3}|" +
                            // rgb:         "rgb(255,255,255)", etc.
                            
                                "rgb\\((\\s*[\\d]{1,3}\\s*,\\s*){2}\\s*[\\d]{1,3}\\s*\\)|" +
                            // colorname:   "white", "black", "pink", etc.
                            
                                "[a-z]+)$", 
                                
                            // Case insensitive
                            "i"
            );
        }
            
        return this._cssColorRegexp.test(object);
    },
 
    // Module Dependencies:
    // ResultSet / ResultTree are both defined as part of the Databinding module but are frequently
    // checked for within grids.
    // Implement default isA functions for these classes so we can check isc.isA.ResultSet() without
    // needing an explicit check for the function being present
    ResultSet : function (data) {
        return false;
    },
    ResultTree : function (data) {
        return false;
    },
    
    // Overridding isA.className methods:
    // We provide custom isc.isA implementations for the following class names which we don't
    // want to be clobberred when the class method is defined
    
    _customClassIsA:{
        SelectItem:true,
        Time:true
    },
    
    // SelectItem IsA Overrides   
    // ---------------------------------------------------------------------------------------

    // isc.isA.SelectItem() default implementation would come from the definition of the
    // selectItem class.
    // However we want this method to return true for NativeSelectItems (not a subclass of
    // SelectItem).
    SelectItem : function (item) {
        if (!item || !isc.isA.FormItem(item)) return false;
        var itemClass = item.getClass();
        return (itemClass == isc.SelectItem || itemClass == isc.NativeSelectItem);
    },

    // Support 'isA.SelectOtherItem()' to test for SelectItems or NativeSelectItems where
    // isSelectOther is true.
    SelectOtherItem : function (item) {
        if (!item || !isc.isA.FormItem(item)) return false;
        var itemClass = item.getClass();
        return ((itemClass == isc.SelectItem || itemClass == isc.NativeSelectItem) 
                && item.isSelectOther);
    },
    
    // SmartClient stores Times in JavaScript Date objects so make isA.Time a synonym for isA.Date
    Time : function (object) {
        return isc.isA.Date(object);
    }

});

if (Array.isArray) {
    isc.addMethods(isc.isA, {
        
        Array : Array.isArray
    });
}


//	@end @object isA



