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
isc.noOp = function isc_noOp() {};
isc.emptyObject = {};
isc._emptyArray = [];
// normal and obfuscatable name
isc.emptyString = isc._emptyString = "";
isc.space = " ";
isc.dot = ".";
isc.semi = ";";
isc.colon = ":";
isc.slash = "/";
isc.star = "*";
isc.apos = "'";
isc.auto = "auto";
isc.px = "px";
isc.nbsp = "&nbsp;";
isc.xnbsp = "&amp;nbsp;"; // XHTML
isc._false = "false";
isc._falseUC = "FALSE";
isc._underscore = "_";
isc._dollar = "$";
isc._obsPrefix = "_$observed_";
isc._superProtoPrefix = "_$SuperProto_";

isc.gwtRef = "__ref";
isc.gwtModule = "__module";

//> @classMethod isc.logWarn()
// Same as +link{classMethod:Log.logWarn}.
//
// @param message    (String)  message to log
// @param [category]   (String)  category to log in, defaults to "Log"
//
// @visibility external
//<
isc.logWarn = function isc_logWarn(message, category) { isc.Log.logWarn(message, category) };

//> @classMethod isc.echo()
// Same as +link{classMethod:Log.echo}.
//
// @param value    (any)  object to echo
// @return (string) a short string representation of the object
//
// @visibility external
//<
isc.echo = function isc_echo(value) { return isc.Log.echo(value) };

//> @classMethod isc.echoAll()
// Same as +link{classMethod:Log.echoAll}.
//
// @param value    (any)  object to echo
// @return (string) a short string representation of the object
//
// @visibility external
//<
isc.echoAll = function isc_echoAll(value) { return isc.Log.echoAll(value) };

//> @classMethod isc.echoLeaf()
// Same as +link{classMethod:Log.echoLeaf}.
//
// @param value    (any)  object to echo
// @return (string) a short string representation of the object
//
// @visibility external
//<
isc.echoLeaf = function isc_echoLeaf(value) { return isc.Log.echoLeaf(value) };

isc.echoFull = function isc_echoFull(value) { return isc.Log.echoFull(value) };

//> @classMethod isc.logEcho()
// Logs the echoed object (using +link{classMethod:isc.echo}) as a warning, prefixed with an
// optional message.
//
//     @param value    (any)  object to echo
//     @param message    (String)  message to log
//
// @see Log.logWarn() for logging info
// @visibility external
//<
isc.logEcho = function isc_logEcho(value, message) {
    if (message) message += ": ";
    isc.Log.logWarn((message || isc._emptyString) + isc.echo(value)) 
}

//> @classMethod isc.logEchoAll()
// Logs the echoed object (using +link{classMethod:isc.echoAll}) as a warning, prefixed with an
// optional message.
//
//     @param value    (any)  object to echo
//     @param message    (String)  message to log
//
// @see Log.logWarn() for logging info
// @visibility external
//<
isc.logEchoAll = function isc_logEchoAll(value, message) {
    if (message) message += ": ";
    isc.Log.logWarn((message || isc._emptyString) + isc.echoAll(value)) 
}

// OutputAsString / StackWalking / Tracing
// ---------------------------------------------------------------------------------------







isc._makeFunction = function isc__makeFunction(args, script) {
    
    var code = script || args;
    
    var returnVal;
    if (script == null) {
        returnVal = new Function(code);
        returnVal._argString = isc._emptyString;
    } else {
        returnVal = new Function(args, code);
    }
    return returnVal;
};


isc.doEval = function isc_doEval(code) {
    // transform code and eval inline
    if (isc.Browser.isMoz) return isc._transformCode(code);
    //return isc._transformCode(code);

    if (!isc._evalSet) isc._evalSet = [];
    isc._evalSet[isc._evalSet.length] = code;
    return null;
}
// called at module end
isc.finalEval = function isc_finalEval() {
    //!OBFUSCATEOK
    if (isc._evalSet) {
        if (isc.Browser.isMoz) {
            for (var i = 0; i < isc._evalSet.length; i++) {
                isc.eval(isc._evalSet[i]);
            }
        }
        var code = isc._evalSet.join("");

        if (isc.Browser.isSafari) code = isc._transformCode(code);
        // uncomment to use catch/rethrow stacks in IE as well
        //else if (isc.Browser.isIE) code = isc._transformCode(code);

        if (isc.Browser.isIE) {
            if (window.execScript != null) {
                window.execScript(code, "javascript");
            } else {
                window.eval(code);
            }

        // Safari 
        } else {
            isc.eval(code);
        }

        // Init pipelining: set a timeout to eval so that the module init time takes place
        // while the next module is being downloaded (except for the last module)
        // Can't be used for real until 
        /*
        var evalFunc = function () {
        if (isc.Browser.isIE) {
            if (window.execScript != null) {
                window.execScript(code, "javascript");
            } else {
                window.eval(code);
            }

        // Safari 
        } else {
            isc.eval(code);
        }
        };

        if (isc.module_DataBinding != 1) {
            //if (isc.Log) isc.Log.logWarn("delaying eval");
            setTimeout(evalFunc, 0)
        } else {
            evalFunc();
        }
        */
    }
    isc._evalSet = null;
}

//isc._eitherMarker = /\/\/\$[01]/;
isc._startMarker = "//$0";
isc._endMarker = "//$1";
isc._totalTransformTime = 0;
// code transform time, all modules
//    - Moz: about 140ms
//      - NOTE: overall init time rises by about 400ms, the balance is due to slower parsing
//        because of the added try/catch constructs.  This can be demonstrated by doing the
//        split/join, but just restoring the markers
//    - Safari: about 300ms
//    - IE: 266ms
// - NOTE: some key advantages of this approach as compared to server-side generation *aside
//   from* not hosing IE's ability to do full stack traces w/o try/catch:
//    - allows arbitrary start/end code to be added with only client-side changes
//    - can be conditional per load
//    - much smaller code size impact: could ship w/o local vars for production use

isc._addCallouts = true;
isc._transformCode = function isc__transformCode(code) {
    // set flag indicating stack walking is enabled so that we will also add try..catch to
    // generated functions
    isc._stackWalkEnabled = true; 

    var start = isc.timeStamp ? isc.timeStamp() : new Date().getTime();

    var startCode = isc._tryBlock, endCode = isc._evalFromCatchBlock;
    if (isc._addCallouts) startCode = isc._methodEnter + startCode;

    var chunks = code.split(isc._eitherMarker),
        finalCode = [];

    var chunks = code.split(isc._startMarker);
    code = chunks.join(startCode);
    chunks = code.split(isc._endMarker);
    code = chunks.join(endCode);

    if (isc._addCallouts) {
        chunks = code.split("//$2");
        code = chunks.join(isc._methodExit);
    }

    /*
    // approach of single split and join to cut down on String churn.
    // Problem is that because of nested functions, markers do not alternate.  Would need to
    // detect which kind of marker is needed for a given slot, by eg checking the next char
    // over, which might be expensive enough to wipe out any advantage; untested.
    var pos = 0;
    for (var i = 0; i < chunks.length; i++) {
        finalCode[pos++] = chunks[i];
        if (i < chunks.length-1) {
            finalCode[pos++] = i % 2 == 0 ? isc._tryBlock : isc._evalFromCatchBlock;
        }
    }
    finalCode = finalCode.join("");

    try {
        window.isc.eval(finalCode);
    } catch (e) {
        //if (!this._alerted) alert(finalCode.substring(0,5000));
        //this._alerted = true;
        document.write("chunks<br><TEXTAREA style='width:760px;height:400px'>" + 
                        chunks.join("\n***") + "</" + "TEXTAREA>");
        document.write("finalCode<br><TEXTAREA style='width:760px;height:400px'>" + 
                        finalCode + "</" + "TEXTAREA>");
        throw e;
    }
    //return finalCode;
    */

    var end = isc.timeStamp ? isc.timeStamp() : new Date().getTime();
    isc._totalTransformTime += (end-start);
    return code;
}

isc._evalFromCatchBlock = "}catch(_e){isc.eval(isc._handleError(";
isc._handleError = function isc__handleError(varList) {
    var code = "var _ = {";
    if (varList != "") {
        var varNames = varList.split(",");
        for (var i = 0; i < varNames.length; i++) {
            var varName = varNames[i];
            code += varName + ":" + varName;
            if (i < varNames.length-1) code += ",";
        }
    }
    code += "};";
    code += "if(isc.Log)isc.Log._reportJSError(_e,arguments,this,_);throw _e;";
    return code;
}



// fillList - utility to concat a number of individual arguments into an array
// ---------------------------------------------------------------------------------------
isc.fillList = function isc_fillList(array, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z) {

    if (array == null) array = [];
    else array.length = 0;

    var undef;
    // avoid touching the arguments object if possible
    
    if (X === undef && Y === undef && Z === undef) {
        array[0] = A;
        array[1] = B;
        array[2] = C;
        array[3] = D;
        array[4] = E;
        array[5] = F;
        array[6] = G;
        array[7] = H;
        array[8] = I;
        array[9] = J;
        array[10] = K;
        array[11] = L;
        array[12] = M;
        array[13] = N;
        array[14] = O;
        array[15] = P;
        array[16] = Q;
        array[17] = R;
        array[18] = S;
        array[19] = T;
        array[20] = U;
        array[21] = V;
        array[22] = W;
    } else {
        for (var i = 1; i < arguments.length; i++) {
            array[i-1] = arguments[i];
        }
    }
    
    return array;
}



//>	@classMethod isc.addProperties()
//
// Add all properties and methods from any number of objects to a destination object, 
// overwriting properties in the destination object.
// <p>
// Common uses of <code>addProperties</code> include creating a shallow copy of an object:<pre>
//
//     isc.addProperties({}, someObject);
//
// </pre>Combining settings in order of precedence:<pre>
//
//     isc.addProperties({}, defaults, overrides, skinOverrides);
//
// </pre>
// <P>
// <b>NOTES</b>:<ul>
// <li>Do not use <code>addProperties</code> to add defaults to an ISC class.
// Use +link{classMethod:Class.addProperties()}, as in: 
// <i>MyClassName</i><code>.addProperties()</code>.
// <li>You may find it more convenient to use the instance method +link{class.addProperties()},
// as in: <i>myClassInstance</i><code>.addProperties()</code>, but there's no functional
// difference from using this method.
// </ul>
//
// @see classMethod:Class.addProperties() 
// @see Class.addProperties()
//
//	@param	destination			(object)	object to add properties to
//	@param	[(arguments 1-N)]	(object)	objects to obtain properties from.  Properties of all 
//											arguments other than destination are applied in turn.
// @return (object) returns the destination object
// @visibility external
//<

/*
// code to count all methods according to what they are added to
isc.methodCount = 0;
isc.classMethodCount = 0;
isc.otherMethods = 0;
isc.otherMethodTargets = [];
*/

isc._sourceList = [];

isc.addGlobal(
"addProperties", function isc_addProperties(destination, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) {
    var undef,
        sourceList = isc._sourceList;

    if (X === undef && Y=== undef && Z === undef) {
        isc.fillList(sourceList, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z);

    } else {
        sourceList.length = 0;
        for (var i = 1; i < arguments.length; i++) {
            sourceList[i -1] = arguments[i];
        }
    }

    var result = isc.addPropertyList(destination, sourceList);
    // reset the sourceList so we don't hang onto the objects in memory unnecessarily
    sourceList.length = 0;
    return result;
});


isc._interfaceInstanceProps = {};
isc._interfaceClassProps = {};
isc._getInterfaceProps = function isc__getInterfaceProps(destination) {
    var className = destination.Class,
        props;
    if (isc.isA.ClassObject(destination)) {
        props = isc._interfaceClassProps[className] = 
                    isc._interfaceClassProps[className] || [];
    } else if (isc.isAn.InstancePrototype(destination)) {
        props = isc._interfaceInstanceProps[className] = 
                    isc._interfaceInstanceProps[className] || [];
    }
    return props;
}

//>	@method ClassFactory.addPropertyList() or isc.addPropertyList()
//
// Add all properties from any number of objects to a destination object.
//
// This differs from addProperties() in that it takes an array as it's second argument,
// applying each object in the array as properties in turn.  
//
//	@param	destination			(object)	object to add properties to
//	@param	sourceList			(array)		array of objects with properties to add
//  @return                     (object)    the object after properties have been added to it
//<
isc.addPropertyList = function isc_addPropertyList(destination, sourceList, warnAboutEmptySharedInstanceArrays) {
    // Don't JS error if passed a null destination
    
    if (destination == null) {
        if (isc.Log) isc.Log.logWarn("Attempt to add properties to a null object. " + 
                                     "Creating a new object for the list of properties."
                                     
                                     );
        destination = {};
    }

    var methods,
        // detect functions being added as properties.  Doesn't work until after "isA" has
        // initialized
        checkFunctions = (isc.isA != null),
        // get the registry of string methods on the destination object
        registry = (isc.isAn && isc.isAn.Instance(destination) ? 
                    destination.getClass()._stringMethodRegistry :
                    destination._stringMethodRegistry);
    if (registry == null) registry = isc.emptyObject;
 
    var props = destination._isInterface ? isc._getInterfaceProps(destination) : null;

    var undef;
    for (var i = 0, l = sourceList.length; i < l; i++) {
        
    	// add it's properties to the destination
		var source = sourceList[i];
		// if <code>source</code> is null, skip it.
		if (source == null) continue;
	
		// copy properties from source to destination
		for (var propName in source) {

            var propValue = source[propName];
			// if any of source's properties are functions 
            // or any of the source's properties are registered as stringMethods on the 
            //          destination object
            // use addMethods to copy these properties
            
            

            var propIsAFunction = checkFunctions && isc.isA.Function(propValue);
            // Check for functions / stringMethods as appropriate.
            
            if (registry[propName] !== undef || propIsAFunction)
            {
                if (methods == null) methods = {};
                methods[propName] = propValue;

			// don't copy an identical property
            // NOTE: unsafe: a subclass may wish to set a property to the same value as the 
            //       default for its superclass, and have the subclass value remain unchanged 
            //       if the superclass default is changed.            
			//} else if (!(source[property] === destination[property])) {
            } else {
                // property is not a function and this slot is not a StringMethod

                // for Interfaces, keep track of all properties added to them
                if (props != null) props[props.length] = propName;

                // check for clobbering a function with a non-function value, eg setting
                // Canvas.enable:false.
                var destinationProp = destination[propName];
                if (!propIsAFunction && destinationProp != null &&
                    isc.isA.Function(destinationProp) && !isc._allowDeleteFuncProperty)
                {
                    if (isc.Log != null) {
                        isc.Log.logWarn("method " + propName + " on " + destination + 
                                        " overridden with non-function: '" + propValue + "'");
                    }
                }
                
                destination[propName] = propValue;

            /*
            } else {

                if (destination.Class && isc.Log &&
                    (!isc.isAn.Instance(destination) || 
                     destination._scPrototype === destination))
                {
                    isc.Log.logWarn("needless override on class: " + destination.Class +
                                    ": " + propName + "->" + propValue);
                }

            */
			}
        }
    }
	if (methods != null) isc.addMethods(destination, methods);
	return destination;
}

//>	@method isc.addMethods()
//
//	Add all named methods from <code>source</code> to <code>destination</code> 
//
//	@see addProperties()
//
//	@param	destination	(object)	object to add methods to
//	@param	source		(object)	object to get methods from
//  @return             (object)    the object after methods have been added to it
//
//<
// NOTE: not externally documented since there is essentially no legitimate reason for author
// code to use this instead of Class.addMethods().

isc._$string = "string";
isc._$function = "function";
isc._$constructor = "constructor";
isc._$object = "object";
isc.addGlobal("addMethods", function isc_addMethods(destination, source) {
	if (!destination || !source) return destination;

    

    var props = destination._isInterface ? isc._getInterfaceProps(destination) : null;

	if (!isc.__remap) isc.__remap = {};

    for (var name in source) {
        
        if (props != null) props[props.length] = name;
		var method = source[name];
        
    	// if a method was specified as a string or an action-object, see if the 
        // destination defines this as a legal string method.
        // NOTE: check typeof object to support Actions, but check non-null because
        // typeof null == "object" and null specified for a method should wipe it out.
        if (isc.isA.instanceMethodsAdded && method != null &&
            (typeof method == isc._$string || typeof method == isc._$object)) 
        {
            var registry = (isc.isAn.Instance(destination) ? 
                                (destination.getClass != null ? 
                                    destination.getClass()._stringMethodRegistry : null) : 
                            destination._stringMethodRegistry);
            var undef; // check for undefined rather than null
            if (registry && !(registry[name] === undef) &&
                
                name != isc._$constructor) 
            {
                method = isc.Func.expressionToFunction(registry[name], source[name]);
            }
            // XXX If it's not a function or a stringMethod, assume it's ok to add it using the 
            // addMethods logic rather than booting back to addProperties
        }
        
        // If someone's observing this method, the actual method will be stored under a different 
        // name
        var observers = destination._observers,
            finalName = (observers != null && observers[name] != null ? isc._obsPrefix + name : name);

        // If the method is already in the correct slot, we're done.
		if (method !== destination[finalName]) {
        
            if (method != null) {
                //>DEBUG take the opportunity to label the function with a name for debug
                // purposes.
                this._nameMethod(method, name, destination) //<DEBUG
    
                

                
            }

            destination[finalName] = method;        
            
            if (method != null) {
    
                
                
    			// if the method was previously assigned an obfuscated name, make sure the function is
    			// available under the obfuscated name in the object it's being mixed into
    			if (isc.__remap[name]) {
                    // same check for observation applies here
                    var finalObfName = (destination._observers != null && 
                                        destination._observers[isc.__remap[name]] != null ?
                                        isc._obsPrefix + isc.__remap[name] : isc.__remap[name]);
                    destination[finalObfName] = method;
                }
            }			
		//} else {
        //    alert("skipped identical assignment in slot: " + finalName + " of " + method);
        }
    }
    
    return destination;
});

// Function naming
// ---------------------------------------------------------------------------------------
//>DEBUG _nameMethod: labels a function with a name for debug purposes.



isc._allFuncs = []
isc._allFuncs._maxIndex = 0;
isc._funcClasses = new Array(5000);

isc._nameMethod = function isc__nameMethod(method, name, destination) {

    if (typeof method != isc._$function) return;

    // if not being added to a class, just use the property name as the function name
    if (destination.Class == null) return method._name = name; 

    // destination is either:
    // - a class Object (eg isc.ListGrid)
    // - an instancePrototype (isc.ListGrid._instancePrototype)
    // - an instance
    // - a handful of other objects on which we've added the Class property, including isc.isA,
    //   ClassFactory, and native prototypes (eg window.Array)

    
    // only for instance prototypes and class objects, not for instances
    if (isc.isA != null && isc.isA.instanceMethodsAdded && 
        (isc.isAn.InstancePrototype(destination) || isc.isA.ClassObject(destination))) 
    {
        var allFuncs = isc._allFuncs;
        // NOTE: functions installed twice, eg interface methods, will appear twice with
        // different classnames, but the first entry will be the one used, so interface methods
        // retain the interface name even when added to other classes.
        allFuncs[allFuncs._maxIndex] = method;
        isc._funcClasses[allFuncs._maxIndex] = destination.Class;
        allFuncs._maxIndex++;
        return;
    }

    // debug: capture all non-Class/Instance methods (eg isA, String extensions, ClassFactory
    // and other bootstrap)
    //if (isc._otherFuncs == null) isc._otherFuncs = [];
    //isc._otherFuncs[isc._otherFuncs.length] = method;

    // special case isA because isA.Class is a method which detects class objects!
    // We need to use a property other than Class for the className.
    var className = (destination == isc.isA ? "isA" : destination.Class);
     
    method._className = className;

    
    if (isc[destination.Class] == null) method._name = name;
      
    if (isc.isA != null && isc.isA.instanceMethodsAdded && isc.isAn.Instance(destination) &&
        !isc.isAn.InstancePrototype(destination)) 
    {
        // instance methods need to be labelled with their name since we don't want to store a
        // list of instance IDs for function name lookups (it would grow indefinitely)
        method._name = name;
        // this method is an instance-specific override (using an instance as an anonymous
        // class).  Mark it as such.
        method._instanceSpecific = true;
        // if there's already a method on the destination with the same name,
        // this is also an override (as opposed to just a method that was added)
        if (destination[name] != null) method._isOverride = true;
    }
    // XXX Note: we could use a check like the following to detect and label class
    // methods vs instance methods
    // if (ClassFactroy.getClass(destination.Class) === destination) {
}

//<DEBUG







   


//> @type Object 
// An ordinary JavaScript as obtained by "new Object()" or via 
// +link{type:ObjectLiteral,Object Literal} syntax.
// <P>
// Methods that return Objects or take Objects as parameters make use of the ability of a
// JavaScript Object to contain an arbitrary set of named properties, without requiring
// declaration in advance.  This capability makes it possible to use a JavaScript Object much
// like a HashMap in Java or .NET, but without the need to call get() or set() to create and
// retrieve properties.
// <P>
// For example if you created an Object using +link{type:ObjectLiteral,Object Literal} syntax
// like so:
// <pre>
//    var request = {
//        actionURL : "/foo.do",
//        showPrompt:false
//    };
// </pre>
// You could then access it's properties like so:
// <pre>
//    var myActionURL = request.actionURL;
//    var myShowPrompt = request.showPrompt;
// </pre>
// .. and you could assign new values to those properties like so:
// <pre>
//    request.actionURL = "<i>newActionURL</i>";
//    request.showPrompt = <i>newShowPromptSetting</i>;
// </pre>
// Note that while JavaScript allows you to get and set properties in this way on any Object,
// SmartClient components require that if a setter or getter exists, it must be called, or no
// action will occur.  For example, if you had a +link{ListGrid} and you wanted to change the
// +link{listGrid.showHeader,showHeader} property:
// <pre>
//     myListGrid.setShowHeader(false); // correct
//     myListGrid.showHeader = false; // incorrect (nothing happens)
// </pre>
// All documented attributes have +link{group:flags,flags} (eg IRW) that indicate when direct
// property access is allowed or not.
//
// @visibility external
//<


// Utility methods for any JavaScript Object
// ---------------------------------------------------------------------------------------

//>	@classMethod isc.getKeys()
//
//	Return all keys (property names) of a given object
//
//	@param	object			(object)	object to get properties from
//	@return					(Array) String names of all properties.  NOTE: never null
// @visibility external
//<
isc.addGlobal("getKeys", function isc_getKeys(object) {
	var list = [];
	if (object != null) {
		for (var key in object) {
			list[list.length] = key;
		}
	}
	return list;
});

//> @classMethod isc.firstKey()
// Return the first property name in a given Object, according to for..in iteration order.
//
// @param object (Object) Object to get properties from
// @return (String) first property name, or null if Object has no properties
// @visibility external
//<
isc.addGlobal("firstKey", function isc_firstKey(object) {
    for (var key in object) return key;
});

//>	@classMethod isc.getValues()
//
//	Return all values of a given object
//
//	@param	object			(object) object to get properties from
//	@return					(Array) values of all properties.  NOTE: never null
// @visibility external
//<
isc.addGlobal("getValues", function isc_getValues(object) {
	var list = [];
	if (object != null) {
		for (var key in object) {
			list[list.length] = object[key];
		}
	}
	return list;
});

//> @classMethod isc.sortObject()
// Given a simple javascript object, return that object sorted by keys, such that when iterating
// through the properties of the object, they will show up in sorted order.<br>
// Usage example - may be used to sort a +link{FormItem.valueMap, formItem valueMap} defined
// as an object.
// @param object (object) Object to sort
// @param [comparator] (function) Comparator function to use when sorting the objects keys
// @return (object) sorted version of the object passed in.
// @visibility external
//<
isc.addGlobal("sortObject", function isc_sortObject(object, sortComparator) {
    if (!isc.isA.Object(object)) return object;
    if (isc.isAn.Array(object)) {
        if (sortComparator != null) return object.sort(sortComparator);
        return object.sort();
    }
    var keys = isc.getKeys(object);
    keys = (sortComparator == null ? keys.sort() : keys.sort(sortComparator));
    var sortedObject = {};
    for (var i = 0; i < keys.length; i++) {
        sortedObject[keys[i]] = object[keys[i]];

    }
    return sortedObject
});

//> @classMethod isc.sortObjectByProperties()
// Given a simple javascript object, return that object sorted by properties, such that when 
// iterating through the properties of the object, values will show up in sorted order.<br>
// Usage example - may be used to sort a +link{FormItem.valueMap, formItem valueMap} defined
// as an object by display value.
// @param object (object) Object to sort
// @param [comparator] (function) Comparator function to use when sorting the object properties
// @return (object) sorted version of the object passed in.
// @visibility external
//<
isc.addGlobal("sortObjectByProperties", function isc_sortObjectByProperties(object, sortComparator) {
    if (!isc.isA.Object(object)) return object;
    if (isc.isAn.Array(object)) {
        if (sortComparator != null) return object.sort(sortComparator);
        return object.sort();
    }
    var values = isc.getValues(object);
    values = (sortComparator == null ? values.sort() : values.sort(sortComparator));
    var sortedObject = {};

    for (var i = 0; i < values.length; i++) {
        var value = values[i];
        for (var key in object) {
            if (object[key] === value) {
                sortedObject[key] = object[key];
                continue;
            }
        }
    }
    return sortedObject
});

//> @classMethod isc.addDefaults()
//
// Copy any properties that do not already have a value in destination.  Null and zero values
// are not overwritten, but 'undef' values will be.
//
// @param destination (Object) Object to which properties will be added.
// @param source (Object) Object from which properties will be added.
// @return (Object) The destination object is returned.
// @visibility external
//< 
isc.addGlobal("addDefaults", function isc_addDefaults(destination, source) {
    if (destination == null) return;
    var undef;
    for (var propName in source) {
        if (destination[propName] === undef) destination[propName] = source[propName];
    }
    return destination;
});


//> @classMethod isc.addDefaultsRecursively()
//
// Copy any properties that do not already have a value in destination.  Null and zero values
// are not overwritten, but 'undef' values will be.  This function operates recursively,
// applying defaults in a "deep" fashion (ie, we recurse into sub-objects and apply defaults 
// at the lowest level, rather than applying the original sub-objects to the target object)
//
// @param destination (Object) Object to which properties will be added.
// @param source (Object) Object from which properties will be added.
// @return (Object) The destination object is returned.
// @visibility internal for now
//< 
isc.addGlobal("addDefaultsRecursively", function isc_addDefaultsRecursively(destination, source, dupList) {
    if (destination == null) return destination;
    if (source == null || isc.isAn.emptyObject(source)) return destination;
    
    var undef;

    if (isc.isAn.Array(source)) {
        if (!isc.isAn.Array(destination)) {
            isc.logWarn("Error during addDefaultsRecursively: source is an array but destination " +
                        "is not.  Cannot continue");
            return;
        }
        for (var i = 0; i < source.length; i++) {
            var entry = source[i];
            if (isc.isA.Function(entry)) continue;
            if (isc.isAn.Instance(entry) || isc.isA.Class(entry)) continue;
            
            if (entry == null || isc.isA.String(entry) || isc.isA.Boolean(entry) ||
                isc.isA.Number(entry))
            {
                if (destination[i] === undef) destination[i] = entry;
            } else if (isc.isA.Date(entry)) {
                if (destination[i] === undef) destination[i] = new Date(entry.getTime());
            } else if (isc.isAn.Object(entry)) {
                if (destination[i] === undef) destination[i] = {};
                if (!isc.isAn.Object(destination[i])) {
                    isc.logWarn("Error during addDefaultsRecursively: entry number " + i +
                                " in the source array is an object, but the existing entry " +
                                i + " in the destination is not an object.  Skipping");
                    continue;
                }
                destination[i] = isc.addDefaultsRecursively(destination[i], entry, dupList); 
            }
        }
        return destination;
    }

    var propertiesToSkip = {
        __ref: true,
        __module: true
    }; 
            
    if (!dupList) dupList = [];
    if (dupList.contains(source)) {
        destination = source;
        return destination;
    }
    dupList.add(source);

    for (var prop in source) {
        if (isc.isA.Function(source[prop])) continue;
        if (propertiesToSkip[prop] == true) continue;
        if (isc.isAn.Instance(source[prop]) || isc.isA.Class(source[prop])) continue;

        var propValue = source[prop];
        if (isc.isA.Date(propValue)) {
            if (destination[prop] === undef) {
                destination[prop] = propValue.duplicate();
            }
        } else if (isc.isAn.Array(propValue)) {
            if (destination[prop] === undef) destination[prop] = [];
            if (!isc.isAn.Array(destination[prop])) {
                isc.logWarn("Error during addDefaultsRecursively: source property '" +
                            prop + "' is an array, but the target object has an existing " +
                            "property of the same name that is not an array.  Skipping");
                continue;
            }
            if (dupList.contains(propValue)) {
                destination[prop] = propValue;
                continue;
            }
            dupList.add(propValue);
            isc.addDefaultsRecursively(destination[prop], propValue, dupList);
        } else if (isc.isAn.Object(propValue)) {
            if (dupList.contains(propValue)) {
                destination[prop] = propValue;
                continue;
            }
            if (destination[prop] === undef) destination[prop] = {};
            if (!isc.isAn.Object(destination[prop])) {
                isc.logWarn("Error during addDefaultsRecursively: source property '" +
                            prop + "' is a sub-object, but the target object has an existing " +
                            "property of the same name that is not an object.  Skipping");
                continue;
            }
            isc.addDefaultsRecursively(destination[prop], propValue, dupList);
        } else {
            if (destination[prop] === undef) destination[prop] = source[prop];
        }

    }
    return destination;
});


//>	@classMethod isc.propertyDefined()
//
//	Is some property specified on the object passed in?  This will return true if 
//  <code>object[propertyName]</code> has ever been set to any value, and not deleted.<br>
//  May return true even if <code>object[propertyName] === undefined</code> if the property 
//  is present on the object and has been explicitly set to undefined.
//
// @param object (object) Object to test 
// @param propertyName (string) Which property is being tested for?
// @return (boolean) true if property is defined
//  @visibility external
//<
isc.addGlobal("propertyDefined", function isc_propertyDefined(object, propertyName) {
    if (object == null) return false;

    var undef;
    if (object[propertyName] !== undef) return true;

    
	var properties = isc.getKeys(object);
    return (properties.contains(propertyName));
});

isc.addGlobal("objectsAreEqual", function isc_objectsAreEqual(object1, object2) {
    // match -> return true
    
    if (object1 === object2) return true;
    
    else if (isc.isAn.Object(object1) && isc.isAn.Object(object2)) {
        if (isc.isA.Date(object1)) {
            return isc.isA.Date(object2) && (Date.compareDates(object1,object2) == 0); 
        } else if (isc.isAn.Array(object1)) {
            if (isc.isAn.Array(object2) && object1.length == object2.length) {
                for (var i = 0; i < object1.length; i++) {
                    if (!isc.objectsAreEqual(object1[i], object2[i])) return false;
                }
                return true;
            }
            return false;
        } else {
            if (isc.isAn.Array(object2)) return false;
            var numProps = 0;
            for (var prop in object1) {
                if (prop == isc.gwtRef || prop == isc.gwtModule) continue;
                if (!isc.objectsAreEqual(object1[prop],object2[prop])) return false;
                numProps ++;
            }
            var numProps2 = 0;
            for (var prop2 in object2) {
                if (prop == isc.gwtRef || prop == isc.gwtModule) continue;
                numProps2++;
                if (numProps2 > numProps) return false;
            }
            if (numProps2 != numProps) return false;

            return true;
        }
    } else {
        return false;
    }
});


// combineObject() - like addProperties() except it handles nested object data structures
// so if an attribute of the source is an object, properties from that object will be
// combined across to the destination, rather than simply clobbering the previous attribute value
// for the field.
// Note the goal here isn't to avoid the destination pointing to the same objects as the source
// (like a duplicate), it's just to merge field values in for nested objects
isc.addGlobal("combineObjects", function isc_combineObjects(destination, source) {
    if (destination == null || !isc.isAn.Object(destination)) return source;
    if (source == null || !isc.isAn.Object(source)) return destination;

    for (var prop in source) {
        var destProp = destination[prop],
            sourceProp = source[prop];
        // If both the source and destination contain simple objects, iterate through the
        // attributes on the source property object and copy them across to the destination property
        // object
        if (isc.isAn.Object(destProp) && !isc.isAn.Array(destProp) && !isc.isA.Date(destProp) 
            && isc.isAn.Object(sourceProp) && !isc.isAn.Array(sourceProp) && 
            !isc.isA.Date(sourceProp))
        {
            isc.combineObjects(destProp, sourceProp); 
        // Otherwise we can just copy the value across as with standard addProperties
        } else {
            destination[prop] = sourceProp;
        }
        
    }
});


//> @method isc.applyMask()
// Create a copy of an Object or Array of Objects that has been trimmed to a specified set of
// properties.
// <p>
// <code>mask</code> is the list of properties to return.  Can be an array of strings or an object.
// If an object, the properties returned will be those that are present in the object.  NOTE: this
// includes properties that exist because they've been explicitly set to null.
// <p>
// If no mask is specified, returns a duplicate of the input
// If no inputs are specified, returns an empty object.
// 
// @param input   (Object or Array)   object to be masked
// @param mask    (Object or Array)   set of properties to limit output to
// 
//<
// NOTE: not external because behavior is a little odd:
// - returns non-null for null input
// - if mask is null and provided an Array, returns an Object instead of a dup'd Array
// we need to check out the framework uses of applyMask and makes sure changing the behavior is
// OK
//
// XXX if applyMask with the input as an empty Array, you will get an empty Array as output.
// So applyMask cannot be used to filter properties that exist on an Array instance.
isc.applyMask = function isc_applyMask(input, mask) {
	var output = {};

	// if no input passed in, return empty output
	if (input == null) return output;

	// if no mask passed in, return all fields from input
	if (mask == null) {
		return isc.addProperties(output, input);
	}

    var inputWasSingle = false;
	if (!isc.isAn.Array(input)) {
        inputWasSingle = true;
        input = [input];
    }

    // convert the mask to an Array of property names if it's an object
    if (!isc.isAn.Array(mask)) mask = isc.getKeys(mask);

    var output = [],
        inputObj, outputObj,
        key, undef;
    for (var i = 0; i < input.length; i++) {
        inputObj = input[i];
        outputObj = output[i] = {};
        // return only the specified properties
        for (var j = 0; j < mask.length; j++) {
            key = mask[j];
            if (inputObj[key] === undef) continue;
            outputObj[key] = inputObj[key];
        }
    }
    return (inputWasSingle ? output[0] : output);
}

isc.getProperties = function isc_getProperties(input, propertyList, output) {
    if (input == null) return null;

    output = output || {};
    if (propertyList == null) return output;
    for (var i = 0; i < propertyList.length; i++) {
        var propName = propertyList[i];
        output[propName] = input[propName];
    }
    return output;
}

isc._digits = {};
isc._floor = Math.floor;
isc._$minus = "-";

for (isc._iterator = 0; isc._iterator < 10; isc._iterator++) 
    isc._digits[isc._iterator] = isc._iterator.toString();

isc._fillNumber = function isc__fillNumber(template, number, startSlot, numSlots, nullRemainingSlots) {

    

    var lastSlot = startSlot + numSlots - 1,
        origNumber = number,
        didntFit = false,
        negative;

    if (number < 0) {
        negative = true;
        number = -number;
        template[startSlot] = this._$minus;
        startSlot += 1;
        numSlots -= 1;
    }
    
    while (number > 9) {
        // reduce by 10x, round off last digit and subtract to find what it was
        var newNumber = this._floor(number/10),
            lastDigit = number - (newNumber*10);
        // fill slots last first
        template[lastSlot] = this._digits[lastDigit];
        number = newNumber;
        
        if (lastSlot == (startSlot+1) && number > 9) {
            // number to large for allocated number of slots
            isc.Log.logWarn("fillNumber: number too large: " + origNumber +
                            isc.Log.getStackTrace());
            didntFit = true;
            break;
        }
        lastSlot -= 1;        
    }

    if (didntFit) {
        
        lastSlot = startSlot + numSlots - 1
        template[lastSlot--] = (!negative ? origNumber : -origNumber);
    } else {
        template[lastSlot--] = this._digits[number];
    }
    
    // null out remaining slots
    for (var i = lastSlot; i >= startSlot; i--) {
        template[i] = null;
    }
};
if (!isc.Browser.isIE || isc.Browser.version > 7) {
    
    isc._fillNumber = function isc__fillNumber(template, number, startSlot, numSlots, nullRemainingSlots) {
        template[startSlot] = number;
        if (nullRemainingSlots) {
            var endI = startSlot + numSlots;
            for (var i = startSlot + 1; i < endI; ++i) {
                template[i] = null;
            }
        }
    };
}


// try to interpolate different types as a boolean
//
// returns default if value is undefined or null
// returns false if value is
//   - the string "false" or "FALSE"
//   - the number 0
//   - the boolean value false
// otherwise returns true
isc.booleanValue = function isc_booleanValue(value, def) {
    // if the value is unset, return the specified default (so, 
    if (value == null) return def;
    
    if (isc.isA.String(value)) return value != isc._false && value != isc._falseUC;
    return value ? true : false;
}

// isc.objectToLocaleString()
// Centralized, customizable toLocaleString() formatter for objects. 
isc.iscToLocaleString = function isc_iscToLocaleString(object) {
    if (object != null) {
        return object.iscToLocaleString ? object.iscToLocaleString() :
                    (object.toLocaleString ? object.toLocaleString() :
                        (object.toString ? object.toString() : isc.emptyString + object));
    }
    return isc.emptyString + object;
}

isc._$toolSkinNames = ["ToolSkin","ToolSkinNative"];

isc.setCurrentSkin = function isc_setCurrentSkin(skinName) {
    // store the current skin so we can detect multiple skins being loaded
    if (isc.currentSkin && !isc._$toolSkinNames.contains(skinName)) {
        isc.logWarn("Detected loading of more than one skin - '" + skinName + "' was loaded " +
            "when '" + isc.currentSkin + "' was already loaded.  See the QuickStart Guide " +
            "for instructions on correctly changing the current skin");
    }
    isc.currentSkin = skinName;
}
