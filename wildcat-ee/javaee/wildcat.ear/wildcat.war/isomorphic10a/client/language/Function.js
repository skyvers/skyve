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

 




  //>DEBUG
// This lets us label methods with a name within addMethods
Function.prototype.Class = "Function";
  //<DEBUG 




// Utility methods for exploring and manipulating functions and methods
isc.ClassFactory.defineClass("Func");

isc.Func.addClassMethods({

    // create the static regular expression we use to parse out the name of a function
    _nameExpression : new RegExp("function\\s+([\\w$]+)\\s*\\("),
    parseFunctionName : function (func) {
        // derive the name from the function definition using a regular expression
        var match = isc.Func._nameExpression.exec(func.toString());
        if (match) return match[1];
        // if the regex didn't match, it's an anonymous function
        // NOTE that new Function().toString() is "function anonymous() { }" on both Moz and IE
        else return "anonymous";
    },

	// gets the name of a function as a string.  Uses 
	getName : function (func, dontReport) { 
        if (func == Function.prototype.apply) return "Function.apply";
        if (func == Function.prototype.call) return "Function.call";
        if (!func) {
            if (!arguments.callee || arguments.callee.caller === undefined) return "unknown";
            if (!arguments.callee.caller) return "none";
            func = arguments.callee.caller;
        }
		// if we've previously determined our name or been explicitly labelled with a name, return
        // that
		if (func._fullName == null) {
            
            if (func._className == null && isc._allFuncs) {
                var index = isc._allFuncs.indexOf(func);
                if (index != -1) {
                    for (var className = isc._funcClasses[index]; className == null; index--) {
                        className = isc._funcClasses[index];
                    }
                    func._className = className;
                } else {
                    // fallback approach uses the fact that we give a global name to all
                    // functions to figure out what they are - works for functions that somehow
                    // miss out on the _allFuncs index. 
                    var functionName = this.parseFunctionName(func);
                    //isc.logWarn("function: " + functionName + " not in index");
                    var isClassMethod;
                    if (functionName.startsWith("isc_c_")) {
                        functionName = functionName.substring(6);
                        isClassMethod = true;
                    } else {
                        functionName = functionName.substring(4);
                    }
                    className = functionName.substring(0, functionName.indexOf("_"));
                    methodName = functionName.substring(className.length+1); 
                    var clazz = isc.ClassFactory.getClass(className),
                        method = null;
                    if (clazz) {
                        method = isClassMethod ? 
                            clazz[methodName] : clazz.getInstanceProperty(methodName);
                    }
                    //if (method != null) {
                    //    isc.logWarn("lookup up method: " + this.echoLeaf(method) + 
                    //                " equals func: " + (method == func));
                    //}
                }
            }

            // if we have a className but no function name, search the class (and instance
            // prototype) for the function
            var name = func._name,
                isClassMethod;
            if (name == null && func._className != null) {
                var theProto;
                var classObj = isc.ClassFactory.getClass(func._className);
                if (classObj == null) {
                    // support lookups for non-Class singletons like isc.ClassFactory and
                    // isc.FileLoader, and native globals like Array and Function
                    //Log.logWarn("className is: " + func._className);
                    classObj = isc[func._className] || window[func._className];
                } else {
                    theProto = classObj.getPrototype();
                }
                // check instance methods first (more common)
                if (theProto != null) {
                    for (var methodName in theProto) {
                        if (theProto[methodName] === func) {
                            name = methodName;
                            break;
                        }
                    }   
                }
                // then class methods
                if (name == null && classObj != null) {
                    for (var methodName in classObj) {
                        if (classObj[methodName] === func) {
                            name = methodName;
                            isClassMethod = true;
                            break;
                        }
                    }
                    // if this is a native object, check the prototype methods as well
                    if (name == null && !isc.isA.Class(classObj) && classObj.prototype != null) {
                        for (var methodName in classObj.prototype) {
                            if (classObj.prototype[methodName] === func) {
                                name = methodName;
                                break;
                            }
                        }
                    }
                }
            }

            if (name != null) {
                func._fullName = (func._instanceSpecific ?
                                  (func._isOverride ? "[o]" : "[a]") : isc._emptyString) +
                                 (isClassMethod ? "[c]" : isc._emptyString) +
                                 (func._className ? func._className + isc.dot : isc._emptyString) +
                                  name;
            } else {
                if (func._isCallback) func._fullName = "callback";
                else {
                    func._fullName = isc.Func.parseFunctionName(func);
                }
            }
            //this.logWarn("function acquired _fullName: " + func._fullName);
        }
        
        return func._fullName; 
	},

	//>	@method	Func.getArgs()	(A)
	//
	// 	Gets the arguments to the function as an array of strings
	//
    //  @param  func (function) Function to examine
	//	@return	(array)	argument names for the function (array of strings)
	//					returns an empty array if the function has no arguments.
	//<
	getArgs : function (func) {
		var args = isc.Func.getArgString(func);
		if (args == "") return [];
		return args.split(",");
	},

	//>	@method	Func.getArgString()	(A)
	//
	// 	Gets the arguments to the function as a string of comma separated values
	//
    //  @param  func (function) Function to examine
	//	@return	(string)	argument names for the function separated by commas
	//					returns an empty string if the function has no arguments.
	//<
	getArgString : function (func) {
        if (func._argString != null) return func._argString;
		var string = func.toString(),
            lparenPosPlus1 = string.indexOf("(") + 1,
            args = string.substring(lparenPosPlus1, string.indexOf(")", lparenPosPlus1));
        args = args.replace(/\/\*.*?\*\/|\/\/.*$/gm, isc.space);
        args = args.replace(/\s+/g, isc.emptyString);
        func._argString = args;
        return args;
	},

	//>	@method	Func.getBody()	(A)
	//
	// 	Gets the body of the function as a string.<br><br>
    //
	//	NOTE: This is the body of the function after it has been parsed -- all comments will
	//			have been removed, formatting may be changed from the original text, etc.
	//	
    //  @param func (function) function to examine
	//	@return	(strings)	body of the function as a string, without leading "{" and trailing "}"
	//<
	getBody : function (func) {
		var string = func.toString();
		
		return string.substring(string.indexOf("{") + 1, string.lastIndexOf("}"));
	},


	//>	@method	Func.getShortBody()	(A)
	//
	// 	Gets the body of the function as a string, removing all returns so it's more
    // 	compact.<br><br>
	//
	//	NOTE: This is the body of the function after it has been parsed -- all comments will
	//	have been removed, formatting may be changed from the original text, etc.
	//	
    //  @param func (function) function to examine
	//	@return	(string)	body of the function as a string, without leading "{" and trailing "}"
	//<
	getShortBody : function (func) {
		var string = func.toString();
		
		return string.substring(string.indexOf("{") + 1, string.lastIndexOf("}")).replace(/[\r\n\t]*/g, "");
	}
});


// -----------------------------------------------------------------------------------------------
// function.apply()
// This is a native method in most browsers.
// If it's not already defined, supply the "apply" function.
// If it is already defined, patch it so it will not JS error if explicitly passed
// <code>null</code> as the arguments (2nd) parameter.






//>	@method	function.apply()	(A)
//
// Applies this function to <code>targetObject</code>, as if the function was originally
// defined as a method of the object.
//
//	@param	targetObject	(object)			target to apply the function to.  Within the context
//												of the function as it evaluates, <code>this</code> == <code>targetObject</code>
//	@param	args			(array of objects)	list of arguments to pass to the function
//
//	@return					(varies)			returns the normal return value of the function
//<
if (!Function.prototype.apply) {
    
    // temporary function number for generating a new function name
	isc.addMethods(Function.prototype, {
		apply :	function (targetObject, args) {

//!DONTOBFUSCATE
            if (targetObject == null) targetObject = window;
		    // generate a temporary function name
		    var tempFunctionName = "__TEMPF_" + Function.prototype._tempFuncNum++;
		    var returnValue;

		    // assign the function being apply'd (this) to the targetObject
		    targetObject[tempFunctionName]=this;


			// if no argments passed, set args to an empty array
			if (!args) args = [];
			
		    if (args.length <= 10) {
		        // Note any undefined properties of the args array will simply be
		        // undefined arguments of the function being invoked via apply, as
		        // they should be.  The arguments.length of the function will be off, but so be it
		        returnValue = targetObject[tempFunctionName](args[0],args[1],args[2],args[3],args[4],
		                                                     args[5],args[6],args[7],args[8],args[9]);
		    } else {
		        // The function is being called with more than ten arguments.

		        // Construct a string with the code necessary to call the function with
		        // however many arguments were passed, then eval() it.
		        var	functionString = 'targetObject[tempFunctionName](';
		        for (var i = 0; i < args.length; i++) {
		            functionString += "args" + '[' + i + ']';
		            if (i + 1 < args.length) {
		                functionString += ',';
		            }
		        }
		        functionString += ');';
				isc.eval('returnValue =' + functionString);
		    }
		    // remove the temporary function from the targetObject
		    delete targetObject[tempFunctionName];
			// and return the value returned by the function call
		    return returnValue;
		}
	});
	// counter which is used to generate unique names for functions to be applied
    Function.prototype._tempFuncNum = 0;
} 




// Add some static helper methods to the Func class
isc.Func.addClassMethods({
    
    // Helper properties     
    _commentDelimeters : [["//", "\n"], ["//", "\\n"], ["/*", "*/"]],
    _stringDelimeters : ["\"", "\'"],
    _complexIdentifiers : ["switch", "while", "if", "return", "for", "var"],
    _multiLineDelimeters : ["(", ")", "[", "]", "{", "}", ":", "?", "!", 
							"+", "-", "/", "*", "=", ">", "<","|", "&", ",", "\\"],

	//>     @method isc.Func.expressionToFunction() (A) 
	// 
	//      Given an expression or conditional as a string, convert it into 
	//              a Function object.   Used to create functions that need to return 
	//              values where the user specifies a string.  These were formerly done 
	//              via evals. 
	// 
	//      @params variables       (string)                Names of variables to pass into the new function 
	//      @params expression      (string)                String expression to evaluate return 
	//      
	//      @return (function)      function that returns the conditional value 
	//< 
	expressionToFunction : function (variables, expression, comment) { 
        

        

        var returnValue = this._expressionToFunction(variables, expression, comment);

        

        return returnValue;
    },
	_expressionToFunction : function (variables, expression, comment) { 
        
		
		if (expression == null) { 
			//>DEBUG 
			isc.Log.logInfo("makeFunctionExpression() called with empty expression"); 
			//<DEBUG 
			expression = ""; 
		}
        
        // Handle being passed an action type object.
        // This is an object of the format
        //   { target:"componentId", name:"fetchData", title:"click" }
        // or 
        //  { target: "someForm", name : "editRecord", title:"itemChanged",
        //    // action method param name -> expression to populate it
        //    mapping : {
        //        record : "record",
        //        callback : "someExpression()" // something use manually entered
        //    }
        //  }
        if (isc.isAn.Object(expression)) {
            if (isc.isA.StringMethod(expression)) expression = expression.getValue();
            
            else if (expression.Action && !expression.target) expression = expression.Action;


            var varsArray = variables;
            if (isc.isA.String(varsArray)) varsArray= variables.split(",");
            else if (isc.isAn.Array(varsArray)) {
                variables = varsArray.join();
            }
            if (!isc.isAn.Array(varsArray)) varsArray = [];

            var expressionArray = [
                // Warn if we can't find the target
                "if (!window.",                     // 0
                ,                                   // 1 (ID of target)
                "){var message='Component ID \"",  // 2
                ,                                   // 3 (ID of target)
                "\", target of action \"",          // 4
                ,                                   // 5 (action title)
                "\" does not exist';isc.Log.logWarn(message);if(isc.designTime)isc.say(message);}", // 6
                // Call the method on the target
                ,                                   // 7 target ID
                ".",                                // 8
                ,                                   // 9 method name
                "(",                                // 10
                ,                                   // 11 arguments [as a ',' separated string]
                ")"                                 // then close with ")" 
            ];    
            // Plug the ID of the target, and the method to call into the function string.
            expressionArray[1] = expressionArray[3] = expressionArray[7] = expression.target;
            expressionArray[9] = expression.name;
            if (expression.title) expressionArray[5] = expression.title;
            else expressionArray[5] = "[No title specified]"
            
            // mapping is an array of expressions to pass in as parameters
            var mapping = expression.mapping || [];
            if (!isc.isAn.Array(mapping)) mapping = [];
            expressionArray[11] = mapping.join();   // automatically puts commas between args 
            
            var expressionString = expressionArray.join(isc.emptyString);
            var theFunc;
            try {
                theFunc = isc._makeFunction(variables, expressionString);
            } catch (e) {
                this.logWarn("invalid code: " + expressionString + 
                             " generated from action: " + this.echo(expression));
                theFunc = function () {};
            }
            theFunc.iscAction = expression;

            return theFunc;

        }

		var complexIdStartChars = "swirfv";


		// if variables passed in as an array of strings, 
		// convert to a single string of vars separated by commas. 
		// 
		if (isc.isAn.Array(variables)) { 
			variables = variables.join(); 
		} 
		
        
		var isSimpleExpression = true; 
		
		// loop through expression character by character. if there is any 
		// indication that it contains more than one statement or a complex
		// statement, set isSimpleExpression to false and break. 

		var i = 0; // character index. 
		var commentDelimiters = this._commentDelimeters;
		var stringDelimiters = this._stringDelimeters;
		
		// strings that identify that a string is more than a simple expression 
		var complexIdentifiers = this._complexIdentifiers;
		
		// the set of characters that can end a line while allowing a statement to continue onto the
        // next line
		var multiLineDelimiters = this._multiLineDelimeters;
		
		// keeps track of whether we've seen a semicolon.  Once we've seen a semicolon, anything
        // other than whitespace and comments indicates a multi-statement expression
		var commentsOnly = false; 
	
        // set up some variables to avoid a bunch of string allocation during loops
        var nullString = isc._emptyString,
			commentStart = isc.slash,
            eol = "\n",
            backslash = "\\",
            plusSign = "+",
            semicolon = isc.semi;

		// keeps track of the last non-whitespace character, 
		// so we know what it was when we get to the end of a line. 
		var lastChar = nullString; 
		
		// keeps track of the next non-whitespace character. 
		var nextChar = nullString; 

		// loop through each character of the expression
		while (i < expression.length) {                         		
			var currentChar = expression.charAt(i);

			// check if we're in a comment by seeing if the current characters match any comment
            // openers
			if (currentChar == commentStart) {
			    for (var j = 0; j < commentDelimiters.length; j++) { 
			    	var delimiterSet = commentDelimiters[j], 
			    		opener = delimiterSet[0], 
			    		closer = delimiterSet[1] 
			    	; 
			    	//if (expression.substring(i, i + opener.length) == opener) { 
			    	if (expression.indexOf(opener, i) == i) { 
                        // we're in a comment.. skip until we find the comment closer
				    	var k = i + opener.length; 
				    	while (k < expression.length) { 
				    		if (expression.substring(k, k + closer.length) == closer) {
				    			k = k + closer.length;
				    			break; 
				    		}
				    		k++; 
				    	} 
				    	i = k; 
				    	lastChar = nullString;
				    	nextChar = this._getNextNonWhitespaceChar(expression, i);
				    } 
			    } 
            }
			
			// we've seen a semicolon.  From here on, if we find anything other than a comment or
            // whitespace, we've got a complex expression
			if (commentsOnly) {
				// if we only have whitespace until the end, we can break now.
				if (nextChar == nullString) {
					break;
				} else {
					if (isc.isA.WhitespaceChar(currentChar)) {
						i++;
						continue;
					} else {
						isSimpleExpression = false; 
						break;   
					}
				}
			}
			
			// check for the beginning of string
			for (var j = 0; j < stringDelimiters.length; j++) {     
			    var delim = stringDelimiters[j]
    			if (currentChar == delim) { 
                    // we're in a string; find the next unquoted delimeter of the same kind
					var k = i + 1; 
					while (k < expression.length) { 
						if (expression.charAt(k) == backslash) k = k + 2; // skip over escapes 
						if (expression.charAt(k) == delim) {
							k++;
							break; 
						}
						k++; 
					} 
					i = k; 
					lastChar = delim.charAt(0);
					nextChar = this._getNextNonWhitespaceChar(expression, i);
				}       
			}

			// check if we've reached the end of a line
			if (currentChar == eol) { 
                // see if the last character on the line is one that would allow the statement to
                // continue onto another line
				var isMLD = false;
				for (var j = 0; j < multiLineDelimiters.length; j++) { 
					if (lastChar == multiLineDelimiters[j]) { 
						isMLD = true;                                           
						break; 
					} 
				} 
				if (isMLD || nextChar == plusSign) { 
					lastChar = nullString; 
				} else { 
                    // the last character on this line closed a statement, and there's more
                    // characters, so this has to be a multi-statement expression
					isSimpleExpression = false; 
					break;                          
				} 
			} 
 
			// look for semicolon  
			if (currentChar == semicolon) {
                // set the commentsOnly flag to switch modes: from here on, if we find anything
                // other than a comment or whitespace, we've got a complex expression
				commentsOnly = true;
			}
			
			// check for keywords that indicate that this is not a simple expression 
            // Note: There's a bug in string.charAt() in IE4 whereby a negative index will 
            // return the first char of the string.
            // Therefore explicitly check whether the keyword is present and either at the
            // beginning or end of the string, or delimited by non AlphaNumeric chars.
            // (IE: it is the keyword, not just a substring of a non-keyword)

    		// _complexIdentifiers : ["switch", "while", "if", "return", "for", "var"],
			if (complexIdStartChars.indexOf(currentChar) != -1) {

			for (var j = 0; j < complexIdentifiers.length; j++) { 
				var word = complexIdentifiers[j],
					length = word.length;
                    
                if ( 
                     // Don't check if there are not enough characters for the keyword
                     (i + length <= expression.length) &&
                     // Is the keyword present?
                     (expression.substring(i, i+length) == word) &&
                     // Is it at the end of the string, or followed by a non Alpha char?
                     (i + length == expression.length ||
                      !isc.isA.AlphaNumericChar(expression.charAt(i + length))) &&
                     // Is it at the beginning of the string, or preceded by a non Alpha char? 
                     (i == 0 ||
                      !isc.isA.AlphaNumericChar(expression.charAt(i - 1))) 

                ) {
			    	isSimpleExpression = false; 
					break; 
				} 
			}
 
			}
			
			// if the current char isn't whitespace, set it as the last non-whitespace char
			if (!isc.isA.WhitespaceChar(currentChar)) lastChar = currentChar;
			
			// increment 
			i++; 
			
			// set up a new nextChar 
			nextChar = this._getNextNonWhitespaceChar(expression, i);
		} 
			
		if (isSimpleExpression) { 
			expression = "return " + expression; 
		} 
		// add a comment (if one was passed in) to the function 
		// this lets us label the functions if we want to 
		if (comment) expression = "/" + "/" + comment + "\r\n" + expression; 
		
		// now create the new function and return it. 
        var theFunc = isc._makeFunction(variables, expression);
        return theFunc;
	}, 		
		
	//>	@method	isc.Func._getNextNonWhitespaceChar()	(A)
	//
	// 	subroutine used by expressionToFunction(). gets the next non-whitespace character
	// 	after the given index.
	//
	//  @params expression      (string)        String expression to evaluate return 
	//	@params	index			(number)		index after which to look for nextChar
	//<
	_getNextNonWhitespaceChar : function (expression, index) {
		// set up a new nextChar 
		var nextChar = isc._emptyString;
		for (var j = (index + 1); j < expression.length; j++) { 
			if (!isc.isA.WhitespaceChar(expression.charAt(j))) { 
				nextChar = expression.charAt(j); 
				break; 
			} 
		} 
        // we searched to the end of the string.
		if (j >= expression.length) nextChar = isc._emptyString; 
		return nextChar;
	},		
	

    //>	@method	isc.Func.convertToMethod()
	//
    //  A static version of class.convertToMethod()
	//	This takes an object and the name of a property as parameters, and (if legal) 
    //  attempts to convert the property to a function.
    //  If the property's value is a function already, or the property is registered via 
    //  Class.registerStringMethods() as being a legitimate target to convert to a function, 
    //  return true.
    //  Otherwise return false
	//
	//  @param  object          (object)    object with property to convert
    //	@param	functionName 	(string)	name of the property to convert to a string.
	//
	//	@return					(boolean)   false if this is not a function and cannot be converted
    //                                      to one
	//
	//<
    convertToMethod : function (object, methodName) {
    
        // Handle bad parameters
        // XXX How to log this better - we know nothing about object, so can't do getID() or 
        //     whatever
        if (!isc.isAn.Object(object) || !isc.isA.nonemptyString(methodName)) {
            isc.Log.logWarn("convertToMethod() called with bad parameters.  Cannot convert " +
                            " property '" + methodName + "' on object " + object + 
                            " to a function.  Returning false.");
            return false;                            
        }
    
        // If the value of this property is already a function - we don't need to make any
        // changes, and can assume it's a legal property value.
        if (object[methodName] && isc.isA.Function(object[methodName])) return true;
        
        // By default the _stringMethodregistry map object is a static property on the Class
        // of the object passed in.
        // If the object passed in is not a member of a subclass of 'Class', this is not the case.
        // In these cases assume the _stringMethodRegistry map has been assigned to the object 
        // directly (for now)
        // XXX - Currently this is not really used anywhere in the code, but potentially could
        // be for stringMethods on (for example) the ListViewer data array.
        var registry = (isc.isAn.Instance(object) ? object.getClass()._stringMethodRegistry :
                                                object._stringMethodRegistry);
        // return false if there's no registry.
        if (registry == null) return false;

        var undefined;
        var methodParamsString = registry[methodName];
                                    
        // If the value is not in the map, return false - this property can't legally be
        // converted to a function by us, so don't attempt it!
        // triple "=" - check for identity not equivalence, as having the argument string be
        // null is legitimate.

        // If this method is not listed in the stringMethodRegistry, we can't convert the
        // property value to a method.
        if (methodParamsString === undefined) return false;

        // We're dealing with a valid string method - attempt to convert the property value.
        isc.Func.replaceWithMethod(object, methodName, methodParamsString);

        // and return true to indicate that this is a legal slot for a function and should now 
        // contain a function (if the conversion was possible).
        return true;
    },   


	//>	@method	isc.Func.replaceWithMethod()	(A)
	//
	// 	Given an object with a string property, convert the string to a function 
	//	and assign it to the same property name.
	//
	//	This is useful when you expect developers to pass a method (such as an event handler,
    //  etc) as a string, but you need to execute it as a function.
	//
	//	@params	object		(object)	Object containing the property
	//	@params	methodName	(string)	Names of the method to convert from string to a function
	//	@params	variables	(string)	Names of variables to pass into the new function
	//<
	replaceWithMethod : function (object, methodName, variables, comment) {
    
        // If no string has been provided for the stringMethod, create a function with the
        // correct signature.  Signature has to match so that you can observe an undefined
        // string method.
        if (object[methodName] == null) {
            object[methodName] = isc.is.emptyString(variables) 
                    ? isc.Class.NO_OP 
                    : isc._makeFunction(variables, isc._emptyString);
        }

        var stringMethod = object[methodName];

        // already converted
        if (isc.isA.Function(stringMethod)) return; 
        
        var convertedMethod;

        if (isc.isA.String(stringMethod) || isc.isA.Object(stringMethod)) {
            // expressionToFunction can handle stringMethods and 'action' type objects
            convertedMethod = isc.Func.expressionToFunction(variables, stringMethod, comment);
        } else { 
            
            isc.Log.logWarn("Property '" + methodName + "' on object " + object + " is of type " +
                            typeof stringMethod + ".  This can not be converted to a method.",
                            "Function");
    
            return;
        } 
        
        // add the converted function to the object:
		var temp = {};
		temp[methodName] = convertedMethod;
		isc.addMethods(object, temp);
	}

});
