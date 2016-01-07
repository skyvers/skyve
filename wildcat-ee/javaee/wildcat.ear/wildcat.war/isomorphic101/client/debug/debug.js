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
isc._debug = {};

isc.addProperties(isc._debug, { 

	// Stack Traces
	// --------------------------------------------------------------------------------------------
    // given the 'arguments' object from a function invocation, return a developer-readable summary,
    // complete with the names and values of the arguments
    
	//>	@method	Class.getCallTrace()
    // Returns a one-line summary of the current method call, showing method name and passed
    // arguments. 
    // This function is available as a static on every ISC Class and as an instance
    // method on every instance of an ISC Class.<br>
    // General best practice is to call the method as "this.getCallTrace(arguments)" whenever "this" is an
    // instance, or call the static classMethod on the +link{class:Log} class otherwise. 
    // <P>
    // Note the <code>arguments</code> object is required in most cases for this method to function. In some
    // browsers, it can be derived automatically, but developers looking to debug on multiple platforms
    // should not rely on this.
    //
    // @param [args]  (Arguments)  arguments object from the call to trace. If ommitted, where supported, 
    //   arguments will be derived from the calling function, or if this is not supported, the method
    //   will not function.
    //
	// @group debug
    // @visibility external
    //<
    //> @classMethod Class.getCallTrace()
    // @include method:class.getCallTrace
    // @visibility external
    //<
    // We also explicitly doc this method on the Log class (the only place it was doc'd prior to 7.0)
    //> @classMethod Log.getCallTrace()
    // @include method:class.getCallTrace
    // @visibility external
    //<
    getCallTrace : function (args, thisValue, showShortMethodBody, argNames, argValues, extensionTrace) {
        if (args == null) args = arguments.caller;
        if (args == null) return "[getCallTrace(): Error: couldn't get arguments object]";
        
        var output, func = args.callee;
        
        // determine function name from arguments.callee.  arguments.callee property is the Function
        // instance being invoked.
        if (func == null) { 
            // browser doesn't support args.callee? (should never happen)
            output = "[args.callee == null]";
        } else if (!isc.Func) { 
            output = "[Func utility class not loaded]";
        } else {
            output = isc.Func.getName(func, true);
        }
        
        // output a summary of the parameters 
        output += "(";
        
        // get the names of the parameters if available
        argNames = argNames || (func != null ? isc.Func.getArgs(func) : []);
        argValues = argValues || args;
        
        // iterate to the larger of the declared parameters or the passed parameters
        var length = Math.max(argValues.length, argNames.length);

        for (var i = 0; i < length; i++) {
            var argName = argNames[i],
                argValue = argValues[i];

            if (i > 0) output += ", ";
            if (argName != null) {
                // show the names of the parameters as eg
                // ListGrid.setRecordStyle(recordNum=>2, newStyle=>"cellDark");
                // Note that there may be no name for the parameter if a parameter was passed
                // where none was expected.
                output += argName + "=>";
            }
            output += this.echoLeaf(argValue);
        }
        output += ")";

        // there's no known way to generally derive the value of "this" from the arguments object -
        // but in ISC several key methods store it explicitly.  This is tremendously valuable in
        // trying to interpret long stacks with method calls weaving through several related
        // instances.a
        thisValue = thisValue || args.__this;
        if (thisValue) output += " on " + this.echoLeaf(thisValue);

        var showedCrashCode = false;
        

        // determine whether to show entire body of method in the trace
        if (showedCrashCode || (!showShortMethodBody && !func._showBodyInTrace)) return output;
    
        var body = this._getTrimmedMethodBody(func);
        if (!func._showBodyInTrace) {
            // if the function at the top of the stack (the one that crashed) or at the
            // bottom of the stack (the entry point) is a one-liner, show it's contents.
            // This is very useful when an anoynmous expression is being called on a timer.
            // NOTE: have to be careful here to avoid spitting out giant methods:
            // - with stripping all functions in ISC are one-liners, hence the crude length
            //   limit
            // - when using XML, very long functions are delivered as stringMethods
            // Could limit to anonymous functions like so:
            //     && args.callee.getName && args.callee.getName() == "anonymous") 
            var lines = body.split(/[\r\n]+/);
            if (lines.length > 1 || lines[0].length > 200) return output;
        }
        output += '\n        "' + body + '"';
    
        return output;
    },

    // for when stack walking is enabled, trim off the try..catch block added to all functions
    _getTrimmedMethodBody : function (func) {
        var body = isc.Func.getBody(func);  
        
        return body.trim();
    },

	//>	@method Class.getStackTrace()
    // Returns a "stack trace" - one line per method in the current call stack, showing the method
    // name and any parameters passed.
    // This function is available as a static on every ISC Class and as an instance
    // method on every instance of an ISC Class.<br>
    // General best practice is to call the method as "this.getStackTrace" whenever "this" is an
    // instance, or call the static classMethod on the +link{class:Log} class otherwise. 
    // <P>
    // Platform Notes: In Mozilla Firefox, if Firebug is enabled, a stack trace will be logged
    // to the firebug console in addition to the standard stack trace string returned by
    // this method.
    // <br>
    // In browsers other than Internet Explorer a complete stack trace may not be available - 
    // this occurs when a function is re-entrant (meaning it calls itself). In this case the
    // stack will terminate with text indicating where the recursive function call occurred.
    // <P>
    // See +link{group:debugging} for further information information.
    //
    // @return (String) stack trace.  Use eg +link{method:isc.Class.logWarn()} to log to the
    // Developer Console.
	// @group debug
    // @visibility external
    //<
    //> @classMethod Class.getStackTrace()
    // @include method:class.getStackTrace
    // @visibility external
    //<
    
    // We also explicitly doc this method on the Log class (the only place it was doc'd prior to 7.0)
    //> @classMethod Log.getStackTrace()
    // @include method:class.getStackTrace
    // @visibility external
    //<
    getStackTrace : function (args, ignoreLevels, maxLevels, skipFBugTrace, extensionTrace) {
        if (isc.Class.useChromeAPIToPrepareStackTrace && isc.Browser.isChrome) {
            
            var e = new Error;
            var origPrepareStackTrace = Error.prepareStackTrace;
            // https://code.google.com/p/v8/wiki/JavaScriptStackTraceApi#Customizing_stack_traces
            Error.prepareStackTrace = function (error, structuredStackTrace) {
                // when intentionally grabbing a trace, we haven't lost any frames
                structuredStackTrace.firstArgsIndex = 0;
                return structuredStackTrace;
            };
            Error.captureStackTrace(e, arguments.callee);
            isc._lastErrorCallSites = e.stack;
            Error.prepareStackTrace = origPrepareStackTrace;
            
            var stackTrace = isc.ChromeStackTrace._getLastErrorCallSitesParsedStack(this);
            if (ignoreLevels) {
                for (var i = 0; i < ignoreLevels; i++) {
                    stackTrace = stackTrace.substring(stackTrace.indexOf("\n") + 1);
                }
            }
            return "\r\n" + stackTrace;
        }
        
        var stack = "";
        

		// do not put getStackTrace method to stackTrace by default
        if (!ignoreLevels) {
            ignoreLevels = 1;
        }
        stack += this._getStackTraceFromArgs(args,ignoreLevels,maxLevels);
        
        // If Firebug is present we can show a stack trace in it directly - see fireBugTrace()
        if (this.hasFireBug() && !skipFBugTrace && isc.Log.showFireBugTrace != false) {
            isc.Log._fBugTrace = isc.Log._fBugTrace || 0;
            var traceId = "FBugTrace" + isc.Log._fBugTrace++;
            stack += "\r\n" + this.fireBugTrace(traceId);
        }
        
        return stack;
    },
    
    
    _getStackTraceFromArgs : function (args, ignoreLevels, maxLevels, extensionTrace) {
        // If we can't get at the properties necessary to do a stack walk just log a warning and 
        // quit
        if (!arguments || !arguments.callee || !arguments.callee.caller) {
            return " [Stack trace not supported in this browser]";
        }
        
        // if we are not passed a specific arguments objects, default to the arguments object of
        // the function that asked for the stack trace
        
        if (args == null) args = arguments.caller || arguments.callee.caller.arguments;
        var output = []; 
        
        // in earlier versions of IE we can use arguments.caller to walk up the stack
        // This actually allows us to get past recursive function calls in a way that
        // arguments.callee.caller does not - use it if available
        
        
        var useArgsCaller = extensionTrace || (isc.Browser.isIE && isc.Browser.version <= 5);

        // skip some of the stack (useful to eg, a logging subsystem) 
        if (ignoreLevels != null) {
            for (var i = 0; i < ignoreLevels; i++) {
                if (args == null) break;
                if (!useArgsCaller) {
                    args = args.callee.caller.arguments;
                } else {
                    args = args.caller;
                }
            }
        }


        // we ran out of stack trying to skip past the ignoreLevels
        if (args == null) {
            return "";
        }

        var func = args.callee;

        var seenFuncs = [];

        var top = true;
        if (maxLevels == null) maxLevels = Number.MAX_VALUE;
        var numLevels = 0;
        
        var message = "";

        while (func != null && args != null && numLevels < maxLevels) {

            if (args.timerTrace) {
                output.add("\nStack trace for setTimeout() call:   " + args.timerTrace);
                break;
            } else if (isc.Func.getName(func, true) == "[c]Timer._fireTimeout" &&
                isc.Timer.currentAction && isc.Timer.currentAction.timerTrace) {
                output.add("\nStack trace for setTimeout() call:   " + isc.Timer.currentAction.timerTrace);
                break;
            }

            if (!useArgsCaller) {
                if (seenFuncs.contains(func)) {
                    output.add("    ** recursed on " + isc.Func.getName(func, true));
                    break;
                }
                seenFuncs.add(func);
            }

            var showFuncText = (top || (args.callee != null && args.callee.caller == null));
            if (extensionTrace) {
                
            } else {
                output.add("    " + this.getCallTrace(args, null, showFuncText));
            }

            if (numLevels == 0 || isc.showLocalsInTraces) {
                var locals = args.__frame;
                
                var frameLocalsOutput = this._getFrameLocals(locals, numLevels != 0);
                if (frameLocalsOutput) output.add(frameLocalsOutput);            
            }
            
            func = args.callee;
            if (!useArgsCaller) {
                func = func.caller;
                if (func) args = func.arguments;
            } else args = args.caller; 
            top = false;
            numLevels++;

            // the extension currently provides the global scope as a stack frame for
            // completeness
            if (extensionTrace && args != null && args.callee == null) {
                output.add("    [global scope]");
                break;
            }
        }
        if (output.length == 0) return "";
        // skip a line at the beginning of the stack trace
        return "\r\n" + output.join("\r") + "\r";
    },

    // http://stackoverflow.com/questions/398111/javascript-that-detects-firebug
    // Note that console.exception was added to Firefox 28, so we need to check whether console.exception
    // is a native function or not.
    // https://developer.mozilla.org/en-US/docs/Web/API/console.error
    hasFireBug : function () {
        return (isc.Browser.isMoz &&
                window.console != null &&
                (window.console.firebug != null ||
                 (window.console.exception != null &&
                  (isc.Browser.version <= 27 || 
                   window.console.exception.toString().indexOf("[native code]") < 0))));
    },

    fireBugVersion : function () {
        return this.hasFireBug() ? window.console.firebug : null;
    },

    // this help function
    fireBugTrace : function (traceId) {
        window.console.trace(traceId);
        return " [Complete stack trace logged via Firebug: " + traceId + "]";
    },

    // get a report of local variable values from a frame (that is, a dump of local variable
    // values from the moment a function crashed)
    _getFrameLocals : function (frame, singleLine) {
        var output = isc.SB.create();
        var first = true;
        for (var varName in frame) {
            var varValue = frame[varName], undef;

            // avoid reporting variables that have not yet been declared or assinged to.  Note
            // this also catches values that have been explicitly assigned undef; we require
            // the developer to understand what the omission means.
            if (varValue === undef) continue;

            // ignore special values stored on the frame object (we assume no local variable
            // would be named with an _)
            if (isc.startsWith(varName, isc._underscore)) continue;

            if (singleLine) {
                if (!first) output.append(", ");
                else output.append("\n        locals: ");
                output.append(varName + "=>" + this.echoLeaf(varValue));
                first = false;
            } else {
                output.append("\n        " + varName + " = " + this.echoLeaf(varValue));
            }
        }
        return output.release(false);
    },

    // called in two circumstances (split these uses?)
    // - when trapping JS errors at top level entry points (eg events) in non-IE browsers
    // - with "stackwalking" try..catch blocks added to all methods, called from every catch
    //   block successively in order to walk the stack by catch..rethrow (any browser)
    _reportJSError : function (error, args, thisValue, frame, addedMessage) {
        

        // avoid reporting the same error twice
        if (error._reported) return;
        error._reported = true;
    
        var message = (addedMessage ? addedMessage + ": " : "") + error.toString();
        
        // Do an in-browser transform of the native stack to make it more readable.
        if (error.stack) {
            message += "\nStack from error.stack:\n";
            if (isc.Class.useChromeAPIToPrepareStackTrace && isc.Browser.isChrome) {
                message += isc.ChromeStackTrace._getLastErrorCallSitesParsedStack(this);
            } else {
                message += isc.StackTrace.fromNativeStack(error.stack).toString();
            }
        } else {
            message += "  [No error.stack available]";
        }

        

        this._reportJSErrorStack(message);
    },

    
    _reportJSErrorStack : function (message) {
        this.logWarn(message);
    },

    
    _onRethrowError : function (error) {
        if (isc.Class.useChromeAPIToPrepareStackTrace && error instanceof Error) {
            error.stack; // trigger lazy evaluation
            isc.ChromeStackTrace._resolveCallSiteFunctionArguments(this);
        }
    },

    

    

    // Shim for old function ... now implemented in debug/StackTrace.js
    transformMozStackTrace : function (trace) {
        return isc.StackTrace.fromNativeStack(trace).toString();
    },
    
    

    // Echoing Objects
    // --------------------------------------------------------------------------------------------
	
	//>	@method	Class.echoLeaf()
    // Return a very short (generally less than 40 characters) string representation of any object,
    // suitable for viewing by a developer for debugging purposes.
    // This function is available as a static on every ISC Class and as an instance
    // method on every instance of an ISC Class.<br>
    // General best practice is to call the method as "this.echoLeaf" whenever "this" is an
    // instance, or call the static classMethod on the +link{class:Log} class otherwise. 
    //
    // @param  obj  (any)  object to echo
    // @return (string) a short string representation of the object
    //
	// @group debug
    // @see class.echo()
    // @visibility external
    //<
    //> @classMethod Class.echoLeaf()
    // @include method:class.echoLeaf
    // @visibility external
    //<
    
    // We also explicitly doc this method on the Log class (the only place it was doc'd prior to 7.0)
    //> @classMethod Log.echoLeaf()
    // @include method:class.echoLeaf
    // @visibility external
    //<
    _echoLeafLimit: 40,                     
	echoLeaf : function (obj, longMode) {
        var output = "",
            undef;
        if (obj === undef) return "undef";
        try {
            // Avoid attempting to manipulate SGWT Java objects
            if (obj != null && isc.Browser.isSGWT && window.SmartGWT.isNativeJavaObject(obj)) 
                return obj.toString();

    		if (isc.isA.Class(obj)) {
    			// Always call toString() for instances of Classes.  We need handle this case
    			// specially, since typeof [instance of a Class] is "object", and we try to do
    			// special things for vanilla Objects below
    			output += obj.toString();
            } else if (isc.isAn.Array(obj)) {
    			output += "Array[" + obj.length + "]";
            } else if (isc.isA.Date(obj)) {
    			output += "Date(" + obj.toShortDate() + ")";
            } else if (isc.isA.Function(obj)) {            
                output += isc.Func.getName(obj, true) + "()";
    		} else {
    			switch (typeof obj) {
    			case "string" : 
                    var limit = this._echoLeafLimit;
                    // for shorter strings show the whole thing.  Also, in "longMode" don't
                    // shorten.
                    if (obj.length <= limit || longMode) {
                        output += '"' + obj + '"'; 
                        break;
                    }
    
                    // for long strings, show an elipsis and the strings full length
                    output += '"' + obj.substring(0, limit) + '..."[' + obj.length + ']';

                    // convert CR/LF to avoid spanning several lines
                    output = output.replaceAll("\n", "\\n").replaceAll("\r", "\\r");
                    break;
    			case "object" :
    				// typeof null is "object"
    				if (obj == null) { output += "null"; break; }
    
                    // DOM object
                    if (obj.tagName != null) {
                        output += "[" + obj.tagName + "Element]" + this.getIDText(obj);
                        break;
                    }
    
                    var toString = "[object]";
                    try {
                        toString = "" + obj;
                    } catch (e) {
                        
                        /*empty*/
                    }
    			    if (toString != "" && toString != "[object Object]" && 
                        toString != "[object]") 
                    {
                        // someone went through the trouble of making a better toString(), so
                        // use it.  NOTE: check for "" because in IE an XmlNodeList among
                        // others will toString() to ""
                        output += toString;
                        break;
                    }
    
    			    // return generic "Obj", plus any obvious ID/name property
                    output += "Obj" + this.getIDText(obj); 
                
    				break;
    			default: output += "" + obj; // invoke native toString()
    			}
    		}
    		return output;
        } catch (e) {
            var message = "[Error in echoLeaf: " + e + "]";
            output += message;
            this.logDebug(message, "Log");
            return output;
        }            
	},

    getIDText : function (obj) {
        // look for properties that may name the object

        // name
        var name = obj.name || (isc.isAn.XMLNode(obj) ? obj.getAttribute("name") : null);
        if (name != null && !isc.isAn.emptyString(name)) return "{name:" + name + "}";

        // ID or id
        var ID = obj.ID != null ? obj.ID :
                    obj.id != null ? obj.id : 
                      (isc.isAn.XMLNode(obj) ? obj.getAttribute("id") : null);
        if (ID != null && !isc.isAn.emptyString(ID)) return "{ID:" + ID + "}";

        // nodeName (HTML DOM)
        if (obj.nodeName != null && !isc.isAn.emptyString(obj.nodeName)) {
            return "{nodeName:" + obj.nodeName + "}";
        }
        
        // title (eg sections)
        var title = obj.title || (isc.isAn.XMLNode(obj) ? obj.getAttribute("title") : null);
        if (title != null && !isc.isAn.emptyString(title)) return "{title:" + title + "}";

        // type (eg validators)
        var type = obj.type || (isc.isAn.XMLNode(obj) ? obj.getAttribute("type") : null);
        if (type != null && !isc.isAn.emptyString(type)) return "{type:" + type + "}";

        // _constructor (defaults)
        var type = obj._constructor;
        if (type != null && !isc.isAn.emptyString(type)) return "{_constructor:" + type + "}";

        // random other objects that might have a "label" property (such as a TreeNode where
        // "label" is the titleField)
        var label = obj.label || (isc.isAn.XMLNode(obj) ? obj.getAttribute("label") : null);
        if (label != null && !isc.isAn.emptyString(label)) return "{label:" + label + "}";
        
        // className (defaults as captured by globalEvalWithCapture)
        var type = obj.className;
        if (type != null && !isc.isAn.emptyString(type)) return "{className:" + type + "}";

        // length: handy for recognizing XMLNodeLists and similar objects in IE, which aren't
        // Arrays and can't be enumerated 
        if (obj.length != null) return "{length:" + obj.length + "}";
        return "";
    },

	//>	@method	Class.echo()
    // Return a short string representation of any object, suitable for viewing by a developer for
    // debugging purposes.
    // <P>
    // If passed an object containing other objects, echo will not recurse into subobjects,
    // summarizing them instead via echoLeaf().
    // <P>
    // NOTE: echo() is used to generate the output shown in the Log window when evaluating an
    // expression.
    // <P>
    // This function is available as a static on every ISC Class and as an instance
    // method on every instance of an ISC Class.<br>
    // General best practice is to call the method as "this.echo()" whenever "this" is an
    // instance, or call the static classMethod on the +link{class:Log} class otherwise. 
    // 
    // @param obj (any) object to echo
    // @return (string) a short string representation of the object
    // 
	// @group debug
    // @see Log.echoAll()
    // @see Log.echoLeaf()
    // @visibility external
    //<
    
    //> @classMethod Class.echo()
    // @include method:class.echo
    // @visibility external
    //<
    
    // We also explicitly doc this method on the Log class (the only place it was doc'd prior to 7.0)
    //> @classMethod Log.echo()
    // @include method:class.echo
    // @visibility external
    //<
    echo : function (obj, multiLine, longArrays, showFunctions) {
        if (obj == null) return this.echoLeaf(obj);

        // Avoid attempting to manipulate SGWT Java objects
        if (isc.Browser.isSGWT && window.SmartGWT.isNativeJavaObject(obj)) return obj.toString();

        if (multiLine == null) multiLine = true;

        if (obj.tagName) return this.echoDOM(obj);

        // anything isn't an Array or Object should be handled by echoLeaf.  (Note that typeof
        // [] is "object").  Note we pass a flag telling echoLeaf it shouldn't try to shorten
        // it's result, since it's going to be the entirety of the output.
        if (typeof obj != "object" || isc.isA.Date(obj)) return this.echoLeaf(obj, true);

        // echo entire arrays rather than just their properties
        if (isc.isAn.Array(obj)) {
            var output = (longArrays ? "[\n" : "[");
            for (var i = 0; i < obj.length; i++) {
                // echo each item either as a leaf or as a full property map
                output += (longArrays ? this.echo(obj[i], multiLine) : this.echoLeaf(obj[i]));
                if (i + 1 < obj.length) output += (longArrays ? ",\n" : ", ");
            }
            output += "\n]";
            return output;
        }
        
        // echo only properties of this instance, as opposed to properties inherited from it's
        // superclass if any
        var output = "{";
		if (obj.getUniqueProperties != null) {
            output = obj.getClassName() + "{";
            obj = obj.getUniqueProperties();
            // avoid a blizzard of function definitions
            if (showFunctions == null) showFunctions = false;
        }
        // if this is not an ISC object, not a DOM element, not atomic (eg String or
        // Number), and not an Array, show its functions as it's something unusual where we'd
        // like to see everything
        if (showFunctions == null) showFunctions = true;

        // echo normal objects
        var propertyNames;
        try {
            propertyNames = isc.getKeys(obj);
        } catch (e) {
            // in IE several XML-related objects through exceptions if you try to for..in on
            // them
            return this.echoLeaf(obj);
        }

        if (isc.Browser.isSafari) {
            
            var isStyle = false,    
                styleDecl = "[object CSSStyleDeclaration]";
            try {
                // many objects JSError on attempts to toString() in Safari
                isStyle = (obj + "" == styleDecl);
            } catch (e) { }
            if (isStyle) {
                output = styleDecl + "{\n[standard props only]\n";
                propertyNames = isc.getKeys(isc.Canvas._getStylePropertyMask());  
                // add 'cssText' as that's not included by default
                propertyNames.add("cssText");
            }
        }

        
        for (var i = 0; i < propertyNames.length; i++) {
            var propertyName = propertyNames[i],
                value;
    
            try {
                // sometimes you can get permission denied on the property access rather than
                // on the attempt to toString() the value
                value = obj[propertyName];
            } catch (e) {
                value = "[error accessing property: " + e + "]";
            }
            if (!showFunctions && isc.isA.Function(value)) continue;
            // don't show internal properties when private identifier obfuscation is on
            if (propertyName.startsWith("$")) continue;

            var echoValue;
            if (propertyName == isc.gwtRef) {
                // don't try to echo references to GWT Java objects.  In hosted / dev mode, our
                // attempt to look for various identifying properties can cause the GWT engine
                // to wedge
                echoValue = "{GWT Java Obj}";
            } else if (propertyName == isc.gwtModule) {
                // Also don't echo references to the GWT module exported to SGWTFactory
                echoValue = "{GWT Module Obj}";
            } else {
                echoValue = this.echoLeaf(value); 
            }
            output += propertyName + ": " + echoValue;
            if (i + 1 < propertyNames.length) output += (multiLine ? ",\r" : ", ");
        }
        output += "}";
        return output;
    },

	//>	@method	Class.echoAll()
    // Like echo(), except that if passed an Array, echoAll() will echo() every element of the
    // Array.
    // This function is available as a static on every ISC Class and as an instance
    // method on every instance of an ISC Class.<br>
    // General best practice is to call the method as "this.echo()" whenever "this" is an
    // instance, or call the static classMethod on the +link{class:Log} class otherwise. 
    //
    // @param obj  (any)  object to echo
    // @return (string) a short string representation of the object
    //
	// @group debug
    // @see echo()
    // @visibility external
    //<
    //> @classMethod Class.echoAll()
    // @include method:class.echoAll
    // @visibility external
    //<
    
    // We also explicitly doc this method on the Log class (the only place it was doc'd prior to 7.0)
    //> @classMethod Log.echoAll()
    // @include method:class.echoAll
    // @visibility external
    //<
    echoAll : function (obj, multiLine) {
        return this.echo(obj, multiLine, true);
    },
    
    echoFull : function (obj) {
        return isc.JSON.encode(obj, {
            prettyPrint:true,
            showDebugOutput:true
        })
    },

    // variant of echo that will be compact: one line, don't recurse into arrays
    echoShort : function (obj) {
        return this.echo(obj, false, false);
    },

    // echoArray - writes out an array with numbered slots.
    echoArray : function (obj) {
        if (!isc.isAn.Array(obj)) return this.echo(obj);
        if (obj.length == 0) return "[empty array]";
        var result = ["["];
        for (var i = 0; i < obj.length; i++) {
            result.addList([i, ":", obj[i], "\n"]);
        }
        result.add("]");
        return result.join("");
    },

    // various properties we want to ignore when echoing DOM elements
    _DOMIgnoreProperties : {

        // we rarely care about the text value of a node
        outerText: false,
        innerText: false,

        // IE proprietary crap
        parentTextEdit: false,
        isTextEdit: false, 
        parentTextEdit: false,
        contentEditable: false,
        canHaveHTML: true, 
        isMultiLine: false, 
        filters: false,
        canHaveChildren: false,
        behaviorUrns: false,
        sourceIndex: false, 
        accelerator: false, 
        textDecorationUnderline: false,
        textDecorationNone: false

		// security exceptions in Moz
		//fullScreen: false, // window.fullScreen

		// error in IE6 (maybe other versions).  You cannot compare the values of these
        // properties to strings.  If you try (eg window.navigator == "") you get "object does
        // not support this property or method", presumably because some native code threw an
        // exception trying to compare against a JS string.
		//clientInformation: false, // window
		//external: false, // window
		//navigator: false, // window
    },

    // echo a DOM Node, avoiding outputting the many constants, functions, and other useless
    // things that appear on all DOM Nodes.  
    // NOTE: in IE, there's no real prototype for DOM elements, so we just suppress things by
    // hand.
    echoDOM : function (node) {
        return this.echoDelta(node, window.Node, node.tagName + this.getIDText(node));
    },

    echoEvent : function (event) {
		// NOTE: in Moz, some of the constants we'd like to omit are on window.KeyEvent and some are
		// on window.Event.  window.KeyEvent has more.
        return this.echoDelta(event, (isc.Browser.isMoz ? window.KeyEvent : window.Event));
    },

    echoDelta : function (obj, base, prefix) {
        if (obj == null) return null;
        
        if (isc.Browser.isIE && isc.isAn.XMLNode(obj)) {
            
            var output = "<" + obj.tagName + " [XMLNode] ";
            var attrs = obj.attributes;
            for (var i = 0; i < attrs.length; i++) {
                var attr = attrs[i];
                if (i > 0) output += " ";
                output += attr.name + "=" + this.echoLeaf(attr.value);
            }
            output += (i > 0 ? " [" : "") + obj.childNodes.length + " child nodes]>";
            return output;
        }

        var output = (prefix || isc.emptyString) + "{",
            propertyNames = isc.getKeys(obj);
        for (var i = 0; i < propertyNames.length; i++) {
            var propertyName = propertyNames[i];

            // skip useless properties found in DOM objects
            if (this._DOMIgnoreProperties[propertyName] != null) continue;

            // skip properties inherited from base class
            if (base != null && base[propertyName] != null) continue;

			// omit multi-letter properties in all caps (typically constants)
			if (propertyName.length > 3 && propertyName.toUpperCase() == propertyName) continue;

            try {
    			var value = obj[propertyName];

                // skip null/empty values
                if (value == null || value == "") continue;

                // skip functions
                if (isc.isA.Function(value)) continue;

                output += propertyName + ": " + this.echoLeaf(obj[propertyName]);
            } catch (e) {
                output += propertyName + ": " + this.echoLeaf(e);
            }
            if (i + 1 < propertyNames.length) output += ", ";
        }
        output += "}";
        return output;
    },

    // echo all the size-related properties of a DOM element.  Won't work in Nav4.
    echoElementSize : function (element) {
        var undef;
        return this.echo({
            scrollLeft : element.scrollLeft,
            scrollTop : element.scrollTop,
            scrollWidth : element.scrollWidth,
            scrollHeight : element.scrollHeight,
            clientWidth : undef,
            clientHeight : undef,
            offsetWidth : element.offsetWidth,
            offsetHeight : element.offsetHeight,
            styleLeft : element.style.left,
            styleTop : element.style.top,
            styleWidth : element.style.width,
            styleHeight : element.style.height,
            styleClip : element.style.clip
        });
    }
});

// make methods available on any class or instance
isc.Class.addProperties(isc._debug);
isc.Class.addClassProperties(isc._debug);


