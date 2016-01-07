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
//>	@object	ClassFactory
//
//	Sets up a real inheritance structure for Javascript objects.
//	We separate out class objects from prototypes, so each gets its own inheritance chain.  
//	This allows us to set up superclass calls, maintain class vs. instance variables and more!
//
//	The ClassFactory is a singleton object that holds the miscellaneous pieces of our inheritance
//	mechanism.
//
//	Your main interaction with the ClassFactory is to create new classes:
//		<code>ClassFactory.defineClass("MyClass", "mySuperClass");</code>
//
//	@see class:Class
//
//	@visibility external
// @treeLocation Client Reference/System
//<

//
//	create the ClassFactory singleton object
//
//  
isc.addGlobal("ClassFactory", {});

  //>DEBUG
// give it a class name so that methods added to it get labelled
isc.ClassFactory.Class = "ClassFactory"; 
  //<DEBUG 

// ClassFactory defines the notion of an "Instance", "ClassObject" and an "Interface".  Add methods
// to isA for recognizing these objects.
isc.addMethods(isc.isA, {
	//>	@classMethod	isA.Instance()
	//
	//	Is <code>object</code> an instance of some class?
	//
	//	@param	object	(object)	object to test
	//	@return			(boolean)	true == <code>object</code> is an instance of some class
	//	@visibility external
	//<
	Instance : function (object) {	return (object != null && object._scPrototype != null)},

	//>	@classMethod	isA.ClassObject()
	//
	//	Is <code>object</code> a class object?
	//
	//	@param	object	(object)	object to test
	//	@return			(boolean)	true == <code>object</code> is a Class Object
	//	@visibility external
	//<
	ClassObject : function (object) {	return (object != null && object._isClassObject == true)},

	//>	@classMethod	isA.Interface()
	//
	//	Is <code>object</code> an interface object?
	//
	//	@param	object	(object)	object to test
	//	@return			(boolean)	true == <code>object</code> is a Interface Object
	//	@visibility external
	//<
	Interface : function (object) {	return (object != null && object._isInterface == true)},

    InstancePrototype : function (object) { 
        return (isc.isAn.Instance(object) && object._scPrototype == object)
    }
});


isc.isA.instanceMethodsAdded = true;

//
// add methods to the ClassFactory
//
isc.addMethods(isc.ClassFactory, {
	//>	@classMethod	ClassFactory.defineClass()
	//
	// Create a new SmartClient class, which can then be used to create instances of this
    // object type, via +link{Class.create()}.
    // <P>
    // The new Class is returned by <code>defineClass</code>, is available as
    // <code>isc.<i>ClassName</i></code> and is also available in global scope if not in
    // +link{class:isc,portal mode}.  Typically, +link{classMethod:class.addProperties()} is then
    // called to establish different defaults in the new class, or to add methods.  For
    // example:
    // <pre>
    //    isc.defineClass("MyListGrid", "ListGrid").addProperties({
    //        headerHeight : 40, // change default for listGrid.headerHeight
    //
    //        // override listGrid.recordClick
    //        recordClick : function (viewer, record) { 
    //           isc.say(record.description);
    //        }
    //    })
    //    isc.MyListGrid.create(); // create an instance of the new class
    // </pre>
    // <P>
    // See also +link{class.Super,Super()} for calling superclass methods.
    // <P>
	// NOTE: <code>isc.defineClass()</code> also creates a new function
    // <code>+link{isA,class:isA}.<i>ClassName()</i></code> object for identifying instances of
    // this Class.
    //
	//	@param	className		(string)	Name for the new class.  
	//	@param	[superClass]	(Class)		Optional SuperClass Class object or name
	//	@return					(Class)		Returns the new Class object.
	//
	//	@visibility external
    //<
    // Internal notes:
	//  Every ClassObject has:
	//  {
	//	 Class : [string className],
	//	 _isClassObject : true,
	//	 _instancePrototype : [instance prototype for class],
	// 
	//	 _superClass : [pointer to superClass ClassObject (if this class is not a root class)]
	// 
	//	 _subClassConstructor : [constructor function that creates subclass ClassObjects]
	//  }
	//
	//  Every InstancePrototype (and Instance) has:
	//  {
	//	 Class : [string className]
	//	 _instanceConstructor : [constructor function that creates instances]
	//	 _classObject : [ClassObject for this class]
	//	._scPrototype : [the instance prototype (this same object)]
	//  }
	defineClass : function (className, superClass, interfaces, suppressSimpleNames) {
		return this._defineNonRootClass(className, superClass, interfaces, null, suppressSimpleNames);
	},

	//>	@classMethod	ClassFactory.overwriteClass()
	//
	// Intentionally clobber an existing SmartClient Class, if it already exists.  Works 
    // identically to +link{ClassFactory.defineClass}, except that no warning is logged to the
    // console.
    //
    // @visibility external
    //<
    overwriteClass : function (className, superClass, interfaces, suppressSimpleNames) {
		return this._defineNonRootClass(className, superClass, interfaces, null, suppressSimpleNames, true);
	},

	//>	@classMethod	ClassFactory.defineInterface()
	//
	//	An "Interface" is an API definition plus a skeletal implementation of that API.
	//  
	//  Interfaces are "mixed in" to another class in order to allow the target class to "support"
	//  the interface.  Interfaces typically require the target class to provide one or two core
	//  methods, and then the interface itself provides the many convenience methods and method
	//  variations that can be written in terms of the core methods.
	//
	//  For example, a List interface could require only get(index) and getLength() from the target
	//  class, and could provide getRange(), indexOf() and other standard List operations.  If the
	//  target class has a more efficient way of supporting getRange() than the generic
	//  implementation in the List interface, the target class can directly implement getRange(),
	//  and the target class' version of getRange() takes precedence.
	//
	//  Comparison to other languages:
	//  - in Java, an "interface" is just an API definition, with no implementation.  The SmartClient 
	//	notion of interfaces is closer to an "abstract class", except that in Java you can only 
	//	inherit from one abstract class, whereas in SmartClient you can mixin as many Interfaces
	//	as you want.  Also, in SmartClient an Interface can contain both instance and class (aka
	//	"static") methods.
	//  - in Ruby, a Mix-in module corresponds exactly to the SmartClient Interface concept.
	//
	//  Writing Interfaces:
	//  - If you are writing an interface and want to indicate that a method must be implemented in
	//	  the target class in order for your interface to work, use addMethods to add a method with
	//	  the special value ClassFactory.TARGET_IMPLEMENTS.  If the target class does not
	//	  implement the method and it gets called, an error will be logged.
	//  - you can subclass an interface to create another interface, but you can't use Super to
	//	  call superclass methods within the interface inheritance chain
	//  - you can define a special initInterface method and it will be called just prior to the
    //    init method on the class that the interface is mixed into
	//  - you can define a special destroyInterface method and it will be called by the destroy
    //    method on the class that the interface is mixed into.  Note that unlike other
    //    languages, javascript does not have a concept of a destructor.  You have to
    //    explicitly call destroy() in order for this logic to run.  But in many cases you
    //    don't have to worry about this because Canvas subclasses cascade the destroy() call
    //    automatically to all children/members/etc.
	//	- if you declare a method in an interface, and mix the interface into a class, you can't
	//	  call Super() and get the interface method -- the one you place in your instance will
	//	  override the one from the interface.  
	//
	//	  To make this work, you have to create an intermediate class, then subclass that.  Eg:
	//
	//		CF.defineInterface("face1");
	//		face1.addMethods({ foo:function() {} });
	//
	//		CF.defineClass("class1");
	//		CF.mixInInterface("class1", "face1");
	//
	//		class1.addMethods({
	//			foo : function () {
	//				// NOTE: a Super() call here will NOT go to the face1.foo method
	//			}
	//		})
	//
	//		CF.defineClass("class2", "class1");
	//		class2.addMethods({
	//			foo : function () {
	//				// NOTE: a Super() call WOULD go to the face1.foo method
	//				// 			(assuming class1.foo was not present)
	//			}
	//		})
	//
	//<
	defineInterface : function (className, superClass) {
		return this._defineNonRootClass(className, superClass, null, true);
	},
	 
	//>	@classMethod	ClassFactory.defineRootClass()
	//
	// 	Variant of defineClass for creating a root class (a class with no superclass).
	//
	//	@param	className		(string)	Name for the new class
	//<
	defineRootClass : function (className) {
		return this._defineClass(className, null);
	},

	//>	@classMethod	ClassFactory._defineNonRootClass()
	//
	//  Define a class or interface which is assumed not to be a root class, that is, either the
	//  superclass must be valid or there must be a valid ClassFactory.defaultSuperClass.
	//<
	_defineNonRootClass : function (className, superClass, interfaces, asInterface, suppressSimpleNames, overwrite) {
		// if no superClass was specified, use the default rootClass
		superClass = (superClass || isc.ClassFactory.defaultSuperClass);
		// if we didn't find a superClass, something went wrong -- bail
		if (!superClass) {
			//>DEBUG
			isc.Log.logWarn("isc.ClassFactory.defineClass(" + className + ") called with null"
						+ " superClass and no ClassFactory.defaultRootClass is defined.");
			//<DEBUG
			return null;
		}
		//If the super class is a framework class, then the child should also be marked as a framework class
		isc.overridingFrameworkClass = isc.isA.ClassObject(superClass) && superClass.isFrameworkClass;
		return this._defineClass(className, superClass, interfaces, asInterface, suppressSimpleNames, overwrite); 
	},

    
    _$Set: "Set",
    _$Window: "Window",
    _$Selection: "Selection",
    _$DataView: "DataView",
    _ignoredGlobalOverrides: {},
    _$simpleNamesWarning: "\nThis conflict would be avoided by disabling " +
                          "ISC Simple Names mode.  See documentation for " +
                          "further information.",
    _installIgnoredGlobalOverrides : function () {
        var browser = isc.Browser,
            ignored = this._ignoredGlobalOverrides;
        
        if (browser.isChrome || browser.isIE || browser.isMoz || browser.isSafari) {
            ignored[this._$Set]       = true;
            ignored[this._$Window]    = true;
            ignored[this._$Selection] = true;
            ignored[this._$DataView]  = true;
        }
    },

	//>	@classMethod	ClassFactory._defineClass()
	//
	// Internal method to actually create a class or interface.  <code>superclass</code> must
    // already be valid.
	//<
    _$iscPrefix: "isc.",
    _classTimes: {},
	_defineClass : function (className, superClass, interfaces, asInterface, suppressSimpleNames, overwrite) 
    {
        

        // Accept superClasses defined as strings rather than references to the class object
        superClass = this.getClass(superClass);

        var definingFramework = (isc.definingFramework == true || isc.overridingFrameworkClass == true);

        
        if (!definingFramework && superClass && superClass._vbOnly && !isc.isVisualBuilderSDK) {
            var hasDefaultSuperClass = !!(isc.ClassFactory.defaultSuperClass);

            var errorMsg = "The framework class " + superClass.getClassName() + " is only available for subclassing if " +
                "isc.licenseType is \"Enterprise\" or \"Eval\".  " +
                (hasDefaultSuperClass ? "Continuing with the default super class." :
                "Returning null as there is no ClassFactory.defaultSuperClass specified.");

            isc.logWarn(errorMsg);
            if (!superClass._vbOnlyWarning) {
                // Only present an alert once per superclass
                superClass._vbOnlyWarning = true;
                isc.warn(errorMsg);
            }

            if (hasDefaultSuperClass) {
                superClass = this.getClass(isc.ClassFactory.defaultSuperClass);
            } else {
                return null;
            }
        }

        // If we have an ID collision, and the caller didn't pass true for the "overwrite"
        // param, warn the user before clobbering the existing object
        var existingObject, inISCSpace,
            ignoreGlobalOverride = this._ignoredGlobalOverrides[className],
            useSimpleNames = (isc._useSimpleNames && !suppressSimpleNames);
        existingObject = isc[className];
        if (existingObject != null) inISCSpace = true
        else if (useSimpleNames && !ignoreGlobalOverride)  {
            existingObject = window[className];
        }

        if (existingObject != null 
            
            && className != "IButton"
            && overwrite != true
            ) 
        {

            var errorString = "New Class ID: '" + className + "' collides with ID of existing " +
                                // NOTE: this check is required in case there is a collision on
                                // window.Class.  At that moment, isc.isA.Class is not a
                                // function, but the String "isA"
                                (isc.isA && isc.isA.Function(isc.isA.Class) && isc.isA.Class(existingObject) ? 
                                    "Class object '" : 
                                    "object with value '") +
                                existingObject + "'.  Existing object will be replaced.";
            if (!inISCSpace) errorString += this._$simpleNamesWarning;

            // Note: If the Log class hasn't loaded yet, we don't warn about this collision.
            // This should be ok in almost every case as Log loads early during the smartClient
            // libs, but if this proves to be an issue, we could hang onto the error string and 
            // wait until after Log has loaded to log a warning.
            if (window.isc.Log) isc.Log.logWarn(errorString);
        }
        
		// create a new instance of the superClass to use as a prototype for this new class
		//	note: instancePrototype.init() is deliberately not called here
		var instancePrototype = 
			(superClass ? new superClass._instancePrototype._instanceConstructor() : {});

		// create the class object for the new class: an object whose lookup pointer is the
		// superclass' ClassObject.
		var classObject = this._makeSubClass(superClass);

		// a constructor function that creates objects whose lookup pointer will be
		// instancePrototype.  These created objects are instances of "subClass"
		instancePrototype._instanceConstructor = 
				this._getConstructorFunction(instancePrototype, className);

		// setup the class object
		classObject.Class = className;
		classObject._isClassObject = true;
		
		// Is this a core ISC class (defined during standard SmartClient init) or is this
		// a class added after the SC libraries have been loaded?
		// Useful for debugging / AutoTest locator APIs
		
		if (definingFramework) classObject.isFrameworkClass = true;
		else classObject.isFrameworkClass = false;
		if (!classObject.isFrameworkClass) {
		    var scClass = superClass;
		    while (scClass && !scClass.isFrameworkClass) {
		        scClass = scClass.getSuperClass();
		    }
		    if (scClass) classObject._scClass = scClass.Class;
		}
		
		if (!classObject._scClass) classObject._scClass = classObject.Class;
		
        // NOTE: important that we always assign _isInterface so that concrete subclasses of
        // interfaces have _isInterface:false
		classObject._isInterface = instancePrototype._isInterface = !!asInterface;
		classObject._superClass = superClass;
		// crosslink the instance prototype and class object
		classObject._instancePrototype = instancePrototype;

		// setup the instance prototype: these properties appear on all instances
		instancePrototype.Class = className;
		// crosslink the instance prototype and class object
		instancePrototype._classObject = classObject;
		// this exists mostly so that instances can reference their prototype
		instancePrototype._scPrototype = instancePrototype;
		
		// copy the scClass information across too
		instancePrototype.isFrameworkClass = classObject.isFrameworkClass;
		instancePrototype._scClass = classObject._scClass;
        
        // put all Classes in the special "isc" object
        isc[className] = classObject;
        // if we're in simple names mode (eg, not worried about name collisions), make the class
        // available as a global variable
        if (useSimpleNames) {
            if (ignoreGlobalOverride) {
                var success = this.tryBindingGlobalID(window, className, classObject);
                if (!success && window.isc.Log) {
                    isc.Log.logWarn("We expected to override global " + className + 
                                    " without any trouble, but were unable to replace it." +
                                    this._$simpleNamesWarning);
                }
            } else window[className] = classObject;
        }

        this.classList[this.classList.length] = className

		// create a function in the isA singleton object to tell if an object is an instance of
        // this Class, eg, isA.ListGrid()
        // Exception - the _customClassIsA object is used to track cases where isc.isA has
        // already been given a custom method which we don't want to clobber
        if (!(isc.isA._customClassIsA[className] && isc.isA[className])) {
            isc.isA[className] = this.makeIsAFunc(className);
        }
    
		// as a convenience, mix in a list of interfaces as part of the class definition
		if (interfaces != null) {
			if (!isc.isAn.Array(interfaces)) interfaces = [interfaces];
			for (var i = 0; i < interfaces.length; i++) {
				//alert("Mixing " + interfaces[i] + " into " + className);
				this.mixInInterface(className, interfaces[i]);
			}
		}

		return classObject;
	},

    
    makeIsAFunc : function (className) {
        

        return function (object) {
            if (object == null || object.isA == null || object.ns == null || object.ns.isA == null || object.isA === object.ns.isA) {
                return false;
            }
            return object.isA(className);
        };
    },

    // make a class object for a new subclass of superClass
    _makeSubClass : function (superClass) {
        if (!superClass) return {};

    	// get the superClass' subclass constructor.  The subclass constructor creates objects
        // whose lookup pointer will be superClass.  It is created on the fly the first time a
        // class acquires a subclass (otherwise all leaf classes would have unnecessary
        // subclass constructors)
        var superSuperClass = superClass._superClass,
            subClassConstructor = superClass._subClassConstructor;
        if (!
            // if the superClass already has a subClassConstructor that differs from the
            // super-super class, use it
            (subClassConstructor &&
             (superSuperClass == null ||
              subClassConstructor !== superSuperClass._subClassConstructor))
            ) 
        {
            // otherwise we make it
		    subClassConstructor = superClass._subClassConstructor = 
                    this._getConstructorFunction(superClass, (superClass.Class || "unknown") + "Class");
        }
        return new subClassConstructor();
    },

	//>	@classMethod	ClassFactory.getClass()
	//
	//	Given a class name, return a pointer to the Class object for that class
	//
	//	@param	className	(string)	name of a class
	//	@return				(Class)		Class object, or null if not found
	//	@visibility external
	//<
	getClass : function (className, warnOnFailure) {
		// if it's a string, assume it's a className
		if (isc.isA.String(className)) {
            // see if isc[className] holds a ClassObject or an SGWTFactory
            var classObject = isc[className];
            if (classObject) {
                if (isc.isA.ClassObject(classObject)) return classObject;
                // SGWTFactory might not be defined yet ...
                if (isc.isA.SGWTFactoryObject && isc.isA.SGWTFactoryObject(classObject)) return classObject;
            }
		}
		// if it's a class object or an SGWTFactory, just return it
		if (isc.isA.ClassObject(className)) return className;
        // SGWTFactory might not be defined yet ...
        if (isc.isA.SGWTFactoryObject && isc.isA.SGWTFactoryObject(className)) return className;

        // if it's an instance of some class, return the class object for the class
        if (isc.isAn.Instance(className)) return className._classObject;
        if (isc.Log && warnOnFailure) {
            isc.Log.logWarn("ClassFactory.getClass() couldn't find class: " + className +
                            "; defined classes are: " + this.classList.sort());
        }
		return null;
	},
	
	//>	@classMethod	ClassFactory.newInstance
	//
	// Given the name of a class, create an instance of that class.
	//	
	//		@param	className	(string)		Name of a class.
	//							(ClassObject)	Actual class object to use.
	//		@param	[props]		(object)		Properties to apply to the instance.
	//		@param	[props2]	(object)		More properties to apply to the instance.
	//		@param	[props3]	(object)		Yet more properties to apply to the instance.
	//
	//	@return				(class)		Pointer to the new class.
	//	@visibility external
	//<
    // NOTE: ability to pass _constructor not documented until we have a more reasonable name for
    // this property.
	newInstance : function (className, props, props2, props3, props4, props5) {

		var classObject = this.getClass(className);

		// if we didn't get a classObject from getClass above,
		// and the first parameter is an object,
		// see if any of the properties objects passed have a ._constructor property,
        // which we'll treat as the classname
		if (classObject == null && isc.isAn.Object(className)) {

            var cons;
            for (var i = 0; i < arguments.length; i++) {
                var propsObj = arguments[i];
                // Note: ._constructor is used rather than .constructor to resolve a
                // number of JS issues, as constructor is present by default on native
                // JS objects.
                // In the long run we want to rename this to something more elegant, like 'class'
                // and modify the css class-specific code to look for 'style' or 'baseStyle' rather
                // than className (or even getClass()).
                if (propsObj != null && propsObj._constructor != null) 
                {
                    cons = propsObj._constructor;
                }
            }

			// now fix up the props objects to include the first object 
			//	as a set of properties instead of just the class name
			props5 = props4;
			props4 = props3;
			props3 = props2;
			props2 = props;
			props = className;

			className = cons;

            // Safari and Mozilla both JS Error if the 'constructor' property set to a string
            // (typically because a user is trying to specify the className to use. (it's ok in IE)
            // Note: the 'constructor' property exists as a native function on a number of standard
            // JS objects, so we can't just check for constructor == null
            if (isc.isA.String(props.constructor)) {
                // If we don't yet have a constructor className, make use of this property - then
                // log a warning and remove it.
                if (className == null) className = props.constructor;
                isc.Log.logWarn("ClassFactory.newInstance() passed an object with illegal 'constructor' " +
                             "property - removing this property from the final object. " +
                             "To avoid seeing this message in the future, " +
                             "specify the object's class using '_constructor'.", "ClassFactory");
                props.constructor = null;
            }     

			classObject = this.getClass(cons);                
		}
    
		if (classObject == null) {
			//>DEBUG
			isc.Log.logWarn("newInstance(" + className + "): class not found", "ClassFactory");
            if (isc.isA.String(className) && className.contains(".")) {
                isc.Log.logWarn("Did you make the SmartGWT class reflectable? See http://www.smartclient.com/smartgwt/javadoc/com/smartgwt/client/docs/Reflection.html", "ClassFactory");
            }
			//<DEBUG
			return null;
		}
        
		return classObject.newInstance(props, props2, props3, props4, props5);
	},	
	
	//>	@classMethod	ClassFactory._getConstructorFunction
	//
	//	Given a <code>prototype</code> object, create a new constructor function that will
	//	reference this prototype.  This allows us to say <code>new constructor()</code> to
	//	create a new object that is effectively a subclass of the original <code>prototype</code>.
	//
	//	@param	proto	(object)	Object to use as the prototype for new objects.
	//	@param	[className]	(identifier)	Name of the class.
	//	@return			(function)	Function that can be used to create new objects
	//								based on the prototype.
	//<
	_getConstructorFunction : function (proto, className) {
        //!OBFUSCATEOK
        
        var cons;
        
        if (isc.Browser.isSafari) {
            cons = function () {};
        } else {
            
            cons = new Function();
        }
		cons.prototype = proto;
		return cons;
	},

    tryBindingGlobalID : function (wd, id, object) {
        try {
            wd[id] = object;
        } catch (e) {
            return false;
        }
        // attempting to override some keywords (for example window.document) will not
        // throw an error but simply fail to pick up the new value - catch this case as
        // well
        if (wd[id] != object) {
            return false;
        }
        return true;
    },

	//>	@classMethod	ClassFactory.addGlobalID()
	//
	// Given an <code>object</code>, declare a unique global variable and link it to object so
    // object can be addressed in the global scope.<br><br>
	// <P>
	// If the object already has an 'ID' property, it will be used. Note that if you pass an
    // object.ID, it's up to you to ensure it is unique in the global scope. If window[<i>ID</i>] 
    // is already assigned to something else a warning will be logged using the developer console,
    // and the existing reference will be replaced.
    // <P>
    // If the object does not have an explicitly specified ID property already, one will be
    // automatically generated. Note that automatically generated global IDs may be reused if
    // the instance they originally referenced has been +link{Class.destroy(),destroyed}.
    // 
	//	@param	object	(object)	Object to add global ID to.
	//<
    _reservedWords: {
        toolbar:true,
        parent:true,
        window:true,
        top:true,
        opener:true,
        event:true // due to window.event in IE
    },
    
	addGlobalID : function (object, ID, dontWarn) {
		// if an ID was passed, use that
		object.ID = ID || object.ID;

        var wd = this.getWindow();        

        // in keepGlobals mode only certain objects are allowed to actually keep their declared
        // global IDs.  Anything else is given the declared global ID temporarily, then retains
        // only its auto-generated global ID after the eval ends.
        if (isc.keepGlobals && object.ID != null) {
            if (!isc.keepGlobals.contains(object.ID) &&
                !(isc.DataSource && isc.isA.DataSource(object))) 
            {
                var tempID = object.ID;
                object.ID = null;
                isc.globalsSnapshot[tempID] = wd[tempID];
                wd[tempID] = object;
                // track temporary globals with auto-assigned IDs so IDs can be released later
                if (object._autoAssignedID) {
                    var className = object.AUTOIDClass || object.Class;
                    
                    isc.autoAssignedTempGlobals[tempID] = className;
                }
            }
        }

        if (object.ID == null) {
            object.ID = this.getNextGlobalID(object);
            object._autoAssignedID = true;
        }

        
        if (isc._loadingComponentXML && isc.createLevel == 1) {
            object._screenEligible = true;
        }

        // if the ID is already taken, log a warning
        var isKeyword, checkForKeyword;
        if (wd[object.ID] != null) {
            var instance = isc.isA.Canvas(wd[object.ID]);
            if (!(isc.isA.DataSource(wd[object.ID]) && wd[object.ID].componentSchema && isc.isA.DataSource(object))) {
                
                if (!dontWarn) {
                    isc.Log.logWarn("ClassFactory.addGlobalID: ID:'" + object.ID + 
                                    "' for object '" + object +
                                    "' collides with ID of existing object '" + wd[object.ID] + "'." +
                                    (instance ? " The pre-existing widget will be destroyed." : 
                                                " The global reference to this object will be replaced"));
                }
            }
            if (instance) wd[object.ID].destroy();
            // If the attribute is not a pointer to a widget instance it may be a
            // a reserved browser keyword or native window attribute which may be non overrideable.
            // Catch the cases we know about (stored in an explicit list)
            // Otherwise use a try...catch block when assigning the property to ensure we don't
            // crash
            
            if (!instance) {
                if (this._reservedWords[object.ID]) isKeyword = true;
                else                          checkForKeyword = true;
            }
        }
        
		// now assign the object under that ID globally so anyone can call it
        if (!isKeyword) {
            if (checkForKeyword) {
                if (!this.tryBindingGlobalID(wd, object.ID, object)) isKeyword = true
            } else {
                wd[object.ID] = object;
            }
        }
        // simple mechanism for instrumenting globals capture.  Simply set isc.globalsSnapshot to an
        // array and we'll fill it here.
         
        if (isc.globalsSnapshot) {
            if (isc.isAn.Array(isc.globalsSnapshot)) {
                // just store all globals that are established
                isc.globalsSnapshot.add(object.ID);
            } else {
                // store a mapping from new globals to original value to allow them to be
                // restored
                isc.globalsSnapshot[object.ID] = wd[object.ID];
            }
        }
        
        // refuse to use keywords and log a warning
        if (isKeyword) {
            var newID = this.getNextGlobalID(object);
            isc.logWarn("ClassFactory.addGlobalID: ID specified as:"+  object.ID + 
                         ". This is a reserved word in Javascript or a native property of the" +
                         " browser window object and can not be used as an ID." +
                         " Setting ID to " + newID + " instead."); 
            object.ID = newID;
            object._autoAssignedID = true;
            wd[object.ID] = object;
        }
    
	},

    _$isc_OID_ : "isc_OID_",
    _$isc_ : "isc_",
    _$underscore : "_",
    _joinBuffer : [],
    _perClassIDs:{},

    getNextGlobalID : function (object) {
        var classString;
        if (object != null && (object.AUTOIDClass != null || object.Class != null)) {
            classString = object.AUTOIDClass || object.Class;
        }
        return this.getNextGlobalIDForClass(classString);
    },
    getNextGlobalIDForClass : function (classString) {
        if (classString) {
            var freed = this._freedGlobalIDs[classString];
            if (freed && freed.length > 0) {
                var ID = freed[freed.length-1];
                freed.length = freed.length-1;
                return ID;
            }
            var idCount;
            if (this._perClassIDs[classString] == null) this._perClassIDs[classString] = 0;
            idCount = this._perClassIDs[classString]++;
            
            var buffer = this._joinBuffer;
            buffer[0] = this._$isc_;
            buffer[1] = classString;
            buffer[2] = this._$underscore;
            isc._fillNumber(buffer, idCount, 3,5);

            var result = buffer.join(isc.emptyString);
            return result;
        }
        return this._$isc_OID_ + this._globalObjectID++;
    },
    // dereferenceGlobalID()
    // - frees the window[ID] pointer to an object
    // - allows the global ID to be re-used within this page
    dereferenceGlobalID : function (object) {
        // remove the window.ID pointer to the object.
        // NOTE: don't destroy the global variable if it no longer points to this widget
        // (this might happen if you create a new widget with the same ID)
        if (window[object.ID] == object) {
            
            if (!isc.Browser.isIE || isc.Browser.isIE9) {
                try {
                    delete window[object.ID];
                } catch (e) {
                    isc.logWarn("ClassFactory.dereferenceGlobalID(): Failed to delete the '" +
                                object.ID + "' global.");
                }
                if (window[object.ID] != null) window[object.ID] = null;
            } else {
                window[object.ID] = null;
            }

            // update globals capture data structure
            
            if (isc.globalsSnapshot) {
                if (isc.isAn.Array(isc.globalsSnapshot)) isc.globalsSnapshot.remove(object.ID);
                else                                     delete isc.globalsSnapshot[object.ID];
            }

            if (object._autoAssignedID && (object.AUTOIDClass != null || object.Class != null)) {
                this.releaseGlobalID(object.AUTOIDClass || object.Class, object.ID);
            }

            // Don't actually delete the object.ID property - This method is typically called
            // as part of destroy() and if for some reason we have a pointer to a destroyed object
            // it's helpful to know the ID for debugging.
        }
    },

    // Maintain a pool of global IDs that are no longer in use due to destroy() calls
    // and reuse them rather than creating new IDs where possible
      
    
    // GlobalIDs are of the form isc_ClassName_int (isc_StaticTextItem_24, etc)
    // We maintain a cache of previously used global IDs indexed by className, set up each time we
    // call dereferenceGlobalID(). Then autoAssignGlobalID() can re-use IDs from the cache for
    // the appropriate object className
    reuseGlobalIDs:true,
    globalIDClassPoolSize:1000,
    _freedGlobalIDs:{
    },
    releaseGlobalID : function (className, ID) {
        
        if (!this.reuseGlobalIDs) return;
        var freed = this._freedGlobalIDs[className];
        if (!freed) this._freedGlobalIDs[className] = [ID];
        else if (freed.length <= this.globalIDClassPoolSize) {
            if (!freed.contains(ID)) freed[freed.length] = ID;
        }
    },

    _domIDCount:0,
    _$isc_:"isc_",
    _simpleDOMIDTemplate:[null, "_", null],
    
    // DOM ID Cacheing logic
    
    // Maintain a cache of generated DOM ID strings that are no longer in use and re-use them when
    // we need a new arbitrary DOM ID.
    // Canvii may notify us when DOM IDs are no longer in use by calling releaseDOMID()
    // Behavior may be disabled by setting reuseDOMIDs to false
    // Note that reuseDOMIDs may also be set to false on individual Canvii - see
    // Canvas._releaseDOMIDs
    reuseDOMIDs:false,
    DOMIDPoolSize:10000,
    _freedDOMIDs:[],
    releaseDOMID : function (ID) {
        if (!this.reuseDOMIDs || this._freedDOMIDs.length > this.DOMIDPoolSize) return;
        this._freedDOMIDs[this._freedDOMIDs.length] = ID;
    },
    
    // getDOMID() - return a unique string to be used as a DOM Id.
    // 
    // Has 2 modes:
    // If isc._longDOMIds is false (production mode), the returned IDs are arbitrary short
    // strings
    // If isc._longDOMIds is true (development mode), the IDs will be generated based on the
    // ID and suffix passed into this method - useful for debugging as the DOM IDs obviously relate
    // to the canvases that created them.
    getDOMID  : function (ID, suffix) {
        
        // By default we return a unique but uninformative ID like "isc_1A"
        
        if (!isc._longDOMIds || !ID || !suffix) {

            // by preference we'll reuse a DOM ID we know has been freed
            var freedIDs = this._freedDOMIDs.length;
            if (freedIDs > 0) {        
                var ID = this._freedDOMIDs[freedIDs-1];
                this._freedDOMIDs.length = freedIDs-1;
                return ID;
            }
            
            var number = this._domIDCount++;
            return this._convertToBase36(number, this._$isc_);
        }
        
        
        
        // In simpleDOMIDMode, create an ID that incorporates the ID / suffix passed to us
        // We're making an assumption that the ID / suffix passed in is already unique
        
        this._simpleDOMIDTemplate[0] = ID;
        this._simpleDOMIDTemplate[2] = suffix;
        return this._simpleDOMIDTemplate.join(isc.emptyString);
    },
    
    _base36Digits:["0","1","2","3","4","5","6","7","8","9","A","B","C","D","E","F","G","H","I","J","K",
                   "L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z"],
    _base36Arr:[],
    _convertToBase36 : function (number, prefix) {
        var digits = this._base36Digits,
            resultsArr = this._base36Arr;

        resultsArr.length = 0;

        // We use this to prefix with "isc_"
        if (prefix) resultsArr[0] = prefix;
        
        var totalDigits = 3;
        
        if (number > 46655) {
            while (Math.pow(36,totalDigits) <= number) totalDigits += 1;
        }
        
        // convert number to base 36
        while (number >= 36) {
            var remainder = number % 36;
            // always add to the end slot, so we get 100 rather than 001
            resultsArr[totalDigits-(prefix ? 0 : 1)] = digits[remainder];
            totalDigits -=1;
            
            number = Math.floor(number / 36);
        }
        resultsArr[totalDigits-(prefix ? 0 : 1)] = digits[number];
   
        return resultsArr.join(isc.emptyString);
        
    },
    
	//>	@classMethod	ClassFactory.mixInInterface()	(A)
	//
	// Add the methods of a given Interface to a Class so the class implements the methods.
    // If the class has already defined a method with the same name as the one specified
	// in the interface, the class' method will be retained.
	//
	//	@param	className		(String)	Name of the Class to add methods to.
	//	@param	interfaceName	(String)	Name of the Interface to get methods from.
	//<
	mixInInterface : function (className, interfaceName) {
		var theInterface = this.getClass(interfaceName),
			theClass = this.getClass(className)
		;
		if (!theInterface || !theClass) return null;
	
		if (!theInterface._isInterface) {
			//>DEBUG
			isc.Log.logWarn("ClassFactory.mixInInterface asked to mixin a class which was not"
						+ " declared as an Interface: "+interfaceName+ " onto "+className);
			//<DEBUG
            return;
		}

		// mark the class as implementing the interface
		if (!theClass._implements) theClass._implements = [];
        // ensure the interface doesn't apply to a superClass
        else theClass._implements = theClass._implements.duplicate();

        // install all properties and methods added to this interface, and any superInterfaces
        while (theInterface) {
    		// mix in class properties and methods
	    	this._mixInProperties(theInterface, theClass, true);
    		// mix in instance properties and methods
	    	this._mixInProperties(theInterface, theClass);

		    theClass._implements[theClass._implements.length] = interfaceName;

            theInterface = theInterface.getSuperClass();
            if (theInterface && !theInterface._isInterface) break;
        }
	},

    _initInterfaceMethodName: "initInterface",
    _destroyInterfaceMethodName: "destroyInterface",
	_mixInProperties : function (source, destination, asClassProperties) {
        var props,
             destinationClass = destination
        ;
		if (asClassProperties) { 
            props = isc._interfaceClassProps[source.Class];
		} else {
            props = isc._interfaceInstanceProps[source.Class];
			source = source.getPrototype();
			destination = destination.getPrototype();
        }

        if (props == null) return;

        for (var i = 0; i < props.length; i++) {
            var propName = props[i];

			// skip any properties already defined in the target
			if (destination[propName] != null) continue;
    
            var propValue = source[propName];

			// the interface declared that the target class must implement a method, and it's not
			// there
			if (isc.isA.String(propValue) && propValue == this.TARGET_IMPLEMENTS) {
				//>DEBUG
				var message = (asClassProperties ? "Class" : "Instance") + " method " 
					+ propName + " of Interface " + source.Class + " must be implemented by "
					+ "class " + destination.Class;
                // Don't complain about interface methods not being implemented b/c it's
                // perfectly normal to mix in interfaces before adding properties to the
                // class.  In fact that may be the case most of the time b/c showing the
                // interfaces at class definition is very useful 
                // (e.g: defineClass("Foo", "Bar", "SomeInterface")
				//
				//isc.Log.logWarn(message + ", is not yet implemented"); 

				// but it will be an error if this method is ever called, so install a function
                // that will complain
                destination[propName] = function () {
                    this.logError(message);
                };
				//<DEBUG 
			} else if (propName == this._initInterfaceMethodName && !asClassProperties) {
                // patch any initInterface() methods onto a special array on the classObject to
                // be called at class creation.
                if (destinationClass._initInterfaceMethods == null) destinationClass._initInterfaceMethods = [];
                destinationClass._initInterfaceMethods[destinationClass._initInterfaceMethods.length] = propValue;
			} else if (propName == this._destroyInterfaceMethodName && !asClassProperties) {
                // patch any destroyInterface() methods onto a special array on the classObject to
                // be called at class destruction.
                if (destinationClass._destroyInterfaceMethods == null) destinationClass._destroyInterfaceMethods = [];
                destinationClass._destroyInterfaceMethods[destinationClass._destroyInterfaceMethods.length] = propValue;
            } else {
                //isc.Log.logWarn("adding property " + propName + 
                //                " from interface " + source.Class);
				destination[propName] = propValue;
            }
        }
	},

	//>	@classMethod	ClassFactory.makePassthroughMethods()	(A)
	//
    // Create methods that call through to a related object stored under property
    // <code>propName</code>.  This enables easy implementation of the Delegate design
    // pattern, where one object implements part of its APIs by having another object respond
    // to them.
    // 
	//	@param	methodNames	(array of strings)	list of methods names
	//	@param	propName    (string)		    Property name where the target object is stored.
	//<
    _$argList : "a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p",
    makePassthroughMethods : function (methodNames, propName, addNullCheck, nullCheckWarning,
                                       inheritedProperty)
    {
        if (!propName) propName = "parentElement";
        
        var funcTemplate;
        if (!addNullCheck) {
            funcTemplate = this._funcTemplate;
            if (funcTemplate == null) {
                funcTemplate = this._funcTemplate = ["return this.",,".",,"("+this._$argList+")"];
            }
        } else {
            funcTemplate = this._nullCheckFuncTemplate;
            if (funcTemplate == null) {
                funcTemplate = this._nullCheckFuncTemplate = 
                    ["if(this.",,"==null){\n",
                     ,// optionally log a warning
                     "return;}\n",,"return this.",,".",,"("+this._$argList+")"];
            }
        }

        var methods = {};

		for (var i = 0; i < methodNames.length; i++) {
			var methodName = methodNames[i];
    
			// create a function that routes a function call to the target object
            if (addNullCheck) {
                funcTemplate[1] = propName;
                if (nullCheckWarning != null) {
                    var messageArgs = {
                        methodName:methodName,
                        propName:propName
                    };
                    var warning = nullCheckWarning.evalDynamicString(this, messageArgs);
                    
                    funcTemplate[3] = "isc.logWarn(\"" + warning + "\");";
                }
                if (inheritedProperty != null) {
                    funcTemplate[5] = "this." + propName + "." + inheritedProperty + "=" +
                                      "this." +                  inheritedProperty + ";\n";
                }
                funcTemplate[7] = propName;
                funcTemplate[9] = methodName;
            
            } else {
                funcTemplate[1] = propName;
                funcTemplate[3] = methodName;
            }
			methods[methodName] = 
                isc._makeFunction(this._$argList, funcTemplate.join(isc.emptyString));
		}
        
        return methods;
    },

	//>	@classMethod	ClassFactory.writePassthroughFunctions()	(A)
    //
    // Install methods in <code>destinationClass</code> which will call the same-named function
    // on a related object stored under the property name <code>memberName</code> on instances
    // of <code>destinationClass</code>.
    //
	//	@example	<code>ClassFactory.writePassthroughFunctions(
	//					ListGrid, "selection", ["select","selectAll",..."]
	//				);</code>
	//
	//				after this, you can call
	//					listGrid.selectRecord()
	//				rather than
	//					listGrid.selection.selectRecord()
    //<
	writePassthroughFunctions : function (destinationClass, memberName, methodNames) {
        var methods = this.makePassthroughMethods(methodNames, memberName);
        destinationClass.addMethods(methods);
    }

});	// END isc.addMethods(isc.ClassFactory)

//
// add properties to the ClassFactory object
//
isc.addProperties(isc.ClassFactory, {
	// when defining interfaces, use this constant as a marker value indicating that a method
    // must be implemented by any class your interface is mixed in to
	TARGET_IMPLEMENTS : "TARGET_IMPLEMENTS",

	//>	@attr	ClassFactory.defaultSuperClass  (Class : null : [IA])  
    // Class to use as the default superClass if none is specified
    //<
		
	// Counter which is used to generate unique object IDs
	_globalObjectID : 0,

	// Classes created with ClassFactory.defineClass
	classList : []
});

//> @classMethod isc.defineClass
// Shortcut for <code>isc.ClassFactory.defineClass()</code>.
// @include classMethod:ClassFactory.defineClass
// @see ClassFactory.defineClass()
// @visibility external
//<
isc.defineClass = function (className, superClass, interfaces, suppressSimpleName) {
    return isc.ClassFactory.defineClass(className, superClass, interfaces, suppressSimpleName);
}

//> @classMethod isc.overwriteClass
// Shortcut for <code>isc.ClassFactory.overwriteClass()</code>.
// @include classMethod:ClassFactory.overwriteClass
// @see ClassFactory.overwriteClass()
// @visibility external
//<
isc.overwriteClass = function (className, superClass, interfaces, suppressSimpleName) {
    return isc.ClassFactory.overwriteClass(className, superClass, interfaces, suppressSimpleName);
}

isc.defineInterface = function (className, superClass) {
    return isc.ClassFactory.defineInterface(className, superClass);
}

//> @type SCClassName
// Name of a SmartClient Class, that is, a Class that has been created via
// +link{classMethod:isc.defineClass()}, including Classes built into SmartClient, such as "ListGrid".
// 
// @visibility external
//<

isc.defer = function (code) {
    var lastClass = isc.ClassFactory.getClass(isc.ClassFactory.classList.last(), true),
        existingCode = lastClass._deferredCode;
    isc.Log.logDebug("deferred code being placed on class: " + lastClass);
    // first time
    if (!existingCode) lastClass._deferredCode = [code];
    // more times
    else existingCode.add(code);
}

// install names that are expected to collide
isc.ClassFactory._installIgnoredGlobalOverrides();
