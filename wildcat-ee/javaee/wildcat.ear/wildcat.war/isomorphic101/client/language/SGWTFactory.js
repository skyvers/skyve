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
// We add a few convenience methods on SmartClient instances so that they can
// reflect a little on their SmartGWT equivalents.
isc.Class.addProperties({
    // Returns the corresponding SmartGWT instance. 
    getSGWTInstance : function () {
        return this[isc.gwtRef];
    },

    // Returns the SGWTModule which corresponds to the module that created
    // the SmartGWT instance
    getSGWTModule : function () {
        return this[isc.gwtModule];
    },

    getSGWTFactory : function () {
        var module = this.getSGWTModule(); 
        if (!module) return null;

        var instance = this.getSGWTInstance();
        if (!instance) return null;

        return module.getSGWTFactory(instance);
    },

    // Returns the fully-qualified Java class name of the SmartGWT instance.
    getSGWTClassName : function () {
        var factory = this.getSGWTFactory();
        return factory ? factory.getClassName() : null;
    },

    // Sets properties via the SmartGWT object, rather than directly. Of
    // course, the SmartGWT object will often eventually call back to set
    // properties on the SmartClient object. If we can't find a SmartGWT
    // object, we fall back to setProperties. Thus, you can call this and know
    // that the properties will be set, one way or the other.
    setSGWTProperties : function (json) {
        var factory = this.getSGWTFactory();
        if (factory) {
            factory.setSGWTProperties(this.getSGWTInstance(), json);
        } else {
            this.setProperties(json);
        }
    },

    // Gets an array of the names of SmartGWT properties (but not values). If
    // we can't find the SmartGWT object, we fall back to getAttributes, which
    // does the equivalent for SmartClient objects.
    getSGWTAttributes : function () {
        var factory = this.getSGWTFactory();
        if (factory) {
            return factory.getSGWTAttributes();
        } else {
            // Adapted from language/Reflection.js
            var list = [];
            for (var property in this) {
                if (typeof this[property] == "function") continue;
                if (property.charAt(0) == "_") continue;
                if (property == property.toUpperCase()) continue;
                list[list.length] = property;
            }
            return list;
        }
    },

    // Gets a single attribute value
    getSGWTProperty : function (property) {
        var module = this.getSGWTModule();
        if (module) {
            return module.getProperty(this.getSGWTInstance(), property);
        } else {
            return this.getProperty(property);
        }
    },
    
    // Gets a single attribute value as a string. Can be equivalent to
    // getSGWTProperty(prop).toString(). But, if there is more than one getter,
    // it will prefer the one that actually returns a string (e.g.
    // getWidthAsString)
    getSGWTPropertyAsString : function (property) {
        var module = this.getSGWTModule();
        if (module) {
            return module.getPropertyAsString(this.getSGWTInstance(), property);
        } else {
            // Note that when we're not dealing with SGWT objects, we just call
            // toString rather than looking for alternate methods. This isn't
            // ideal, since sometimes we might want an alternative method.
            // However, it isn't trivial to figure out which methods to choose.
            var result = this.getProperty(property);
            return result ? result.toString() : result;
        }
    }
});

// A factory class which knows how to produce SmartGWT objects, apply
// properties to them, get lists of their property names, and get the
// corresponding SmartClient instance.  SmartGWT creates an SGWTFactory
// instance for each SmartGWT BeanFactory. The SGWTFactory is stored in the
// isc[] space, as if it were a regular SmartClient class. In fact, calls to
// isc.ClassFactory.getClass(fullyQualifiedSmartGWTClassName) will return the
// SGWTFactory. So, SGWTFactory is a kind of sister-class to Class. This
// permits the autoChild process and creation via _constructor to work mostly
// unchanged where the autoChild or _constructor is specified as a
// fully-qualified SmartGWT class name. Calling code does have to be aware of
// SGWTFactory if using createRaw() rather than create() -- see below.
isc.defineClass("SGWTFactory");

isc.SGWTFactory.addClassProperties({
    // A marker that we use to identify config blocks for the createRaw() /
    // init() | completeCreation() cycle
    CONFIG_BLOCK: "sgwtConfigBlock",

    // The name of the property on the config block where init() |
    // completeCreation() stores the real object when called after createRaw()
    SC_INSTANCE: "smartclientInstance",

    // A marker on hashes in the isc[] space, to indicate that they represent
    // parts of bean class names ... e.g. "com" in com.mycompany.MyListGrid.
    BEAN_CLASS_PARTS: "beanClassParts",

    // Note that this only gets factories by name, and only those which have
    // been explicitly created by GWT.create on the SmartGWT side (rather than
    // merely having been creating for superclasses of those classes).
    //
    // If you have an existing object, then call getSGWTFactory on it instead,
    // since that will query class objects from the appropriate SGWT module,
    // possibly loaded separately with classes that possibly have the same
    // name.
    // 
    // Thus, this function is really only for cases in which all you have is
    // a class name, and nothing else.
    getFactory : function (beanClassName) {
        if (!beanClassName) return null;
        var factory = isc[beanClassName];
        if (factory && isc.isA.SGWTFactoryObject(factory)) {
            return factory;
        } else {
            return null;
        }
    },

    // This is a convenience function to help with the createRaw / (init |
    // completeCreation) cycle ... see below
    extractFromConfigBlock : function (sgwtConfigBlock) {
        if (sgwtConfigBlock[isc.SGWTFactory.CONFIG_BLOCK]) {
            // if it is a config block, extract the instance
            return sgwtConfigBlock[isc.SGWTFactory.SC_INSTANCE];
        } else {
            // Otherwise, return the "config block" itself, so we can call this
            // unconditionally with things that might not be config blocks
            return sgwtConfigBlock;
        }
    }
});

// SGWTFactory instances get used as if they were Class objects (rather than
// instances). As an example, the SGWTFactory objects have an *instance* method
// create(), which functions like the *class* method create, except that it
// creates the SGWT object for the beanClassName (rather than creating another
// SGWTFactory). In order to make various schema-driven routines just work, we
// need to mimic various things that would normally apply to Class objects on
// the SGWTFactory instance -- essentially, pretend that each SGWTFactory
// *instance* is a Class object. (The alternative would be to actually make
// each SGWTFactory a class object via defineClass, but that would add a large
// number of classes. Also, in the case of multiple SGWT modules, we need to
// create more than one SGWTFactory with the same beanClassName.)
//
// One result of the mimicry is that it's hard to detect whether an SGWTFactory
// instance is actually an SGWTFactory instance (the normal isA.SGWTFactory won't
// work because it's fooled by the mimicry), so we have a special method.
isc.addMethods(isc.isA, {
    SGWTFactoryObject : function (object) {
        return object != null && object._isSGWTFactoryObject == true;
    }
});

isc.SGWTFactory.addProperties({
    // Marker for isc.isA.SGWTFactoryObject, since the normal isc.isA.SGWTFactory
    // won't work.
    _isSGWTFactoryObject: true,

    // --------------------------------------------------------------------
    // These properties will be set by the SGWT BeanFactory when it creates
    // this SGWTFactory.
    // --------------------------------------------------------------------

    // The fully-qualified SmartGWT class name: e.g. com.mycompany.MyListGrid
    // beanClassName: null,
    
    // The SGWT BeanFactory
    // beanFactory: null,

    // The SGWTModule that corresopnds to this factory
    // sgwtModule: null,
    
    getClassName : function () {
        return this.beanClassName;
    },

    getSGWTClassName : function () {
        return this.beanClassName;
    },

    getSuperClass : function () {
        return this._superClass;
    },

    init : function () {
        this.Super("init", arguments);

        // Do some things to the *instance* that ClassFactory would
        // normally do when defining a *class*.
        this.Class = this.beanClassName;
        this.isFrameworkClass = this.isSGWTFrameworkClass();

        if (!this.isFrameworkClass) {
            var scClass = this.getSGWTSuperClass();
            while (scClass && !scClass.isFrameworkClass) {
                scClass = scClass.getSGWTSuperClass();
            }
            if (scClass) this._scClass = scClass.Class;
        }
        if (!this._scClass) this._scClass = this.Class;

        this._superClass = this.getSGWTSuperClass();

        // We don't add the instance to isc.ClassFactory.classList, since it
        // doesn't seem to be used for anything we need.

        isc.isA[this.beanClassName] = isc.ClassFactory.makeIsAFunc(this.beanClassName);

        // Need to mimic the _isA_Canvas optimization
        if (this.isA("Canvas")) this._isA_Canvas = true;
    },
    
    // Registers the class name in the isc[] space, so that getFactory() will
    // return it, and the following idioms will work (where _constructor is a
    // fully-qualified SmartGWT class name):
    // 
    // isc[object._constructor].create(properties)
    // isc["com.mycompany.MyListGrid"].create(properties);
    //
    // Also, see the notes on createRaw() below -- this is also supported, but
    // the calling code needs to be aware of a few details.
    //
    // This is called from the SmartGWT side if GWT.create was used to make the
    // factory. It is not called if the factory was automatically created for
    // a superclass. That way, the developer controls which class names are
    // registered by which factories are explicitly instantiated by GWT.create.
    registerClassName : function () {
        // Wrap in an error handler, since we call this from SmartGWT
        try {
            var classObj = this;

            
            /*
            var isFrameworkClass = (isc.SGWTFactory._isSGWTFrameworkClass(this.beanClassName));
            if (isFrameworkClass) {
                var j = this.beanClassName.lastIndexOf("."),
                    scClassName = this.beanClassName.substring(j + 1);

                if (isc.isA.Class(isc[scClassName])) {
                    classObj = isc[scClassName];
                } else {
                    isc.logWarn("beanClassName '" + this.beanClassName + "' is supposed to " +
                               "be a built-in framework class, but isc['" + scClassName + "'] " +
                               "is not a Class.");
                }
            }
            */

            // Check for collisions within isc[] This should be extremely rare,
            // as there is nothing typically in isc[] that would look like a
            // fully-qualified java name. But better safe than sorry! Note that
            // here we store the beanClassName as a single entry in isc[] --
            // that is, if beanClassName is "com.mycompany.MyListGrid" that
            // produces one entry in the hash (at top-level), despite the dots.
            // Below we do a little extra work so that
            // isc.com.mycompany.MyListGrid will also work.
            //
            // Note that we don't try to support simpleNames here, since we
            // wouldn't expect the developer to access any of this directly. We
            // could support simpleNames, at the cost of some increased
            // possibility of collision.
            var existingObject = isc[this.beanClassName];
            if (existingObject) {
                if (isc.isA.ClassObject(existingObject)) {
                    this.logWarn("beanClassName '" + this.beanClassName + "' collides with existing native " + 
                                "SmartClient class with the same name. The bean will not be registered.");
                    return;
                } else if (isc.isA.SGWTFactoryObject(existingObject)) {
                    this.logWarn("beanClassName '" + this.beanClassName + "' has already been registered. " +
                                "The existing bean will be replaced.");
                } else {
                    this.logWarn("beanClassName '" + this.beanClassName + "' collides with the ID of an existing " +
                                "object with value '" + this.echo(existingObject) + 
                                "'. The bean will not be registered.");
                    return;
                }
            }

            // If the beanClassName has dots (which it should), then we also want
            // to make the following idiom work:
            //
            // isc.com.mycompany.MyListGrid.create(properties)
            //
            // Collisions within isc[] should still be rare. Generally speaking,
            // the first part of the java fully-qualified name will be a short word
            // with only lower-case letters. There are very few of those normally
            // within isc[] -- just:
            //
            // "ask", "auto", "clone", "colon", "confirm", "contains", "defer",
            // "dot", "echo", "eval", "is", "nbsp", "params", "px", "rpc", "say",
            // "semi", "slash", "star", "version", "warn", "xml", "xnbsp"
            //
            // Well, I suppose that's not such a short list. But none of them are
            // very likely candidates for the first part of a Java class name.
            //
            // An alternative would be to use "sgwt." as a prefix -- that way, we
            // would only reserve isc[sgwt]. Using "sgwt:" as the prefix would be
            // awkward, because then ...
            //
            // isc.sgwt:com.mycompany.MyListGrid.create() 
            //
            // ... would no longer be legal code.
            var beanClassParts = this.beanClassName.split(".");
            if (beanClassParts.length > 1) {
                var base = isc;
                var breadCrumbs = "isc";

                // Note we're stopping at the second-last element, not the last
                for (var i = 0; i < beanClassParts.length - 1; i++) {
                    var part = beanClassParts[i]
                    var existingObject = base[part]; 
                    breadCrumbs = breadCrumbs + "." + part;
                    
                    if (existingObject) {
                        // If it exists, make sure it's ours
                        if (!existingObject[isc.SGWTFactory.BEAN_CLASS_PARTS]) {
                            this.logWarn("beanClassName '" + this.beanClassName + 
                                        "' collides with existing object located at '" + breadCrumbs + 
                                        "' with value '" + this.echo(existingObject) + 
                                        "'. The bean will not be registered.");
                            return;
                        }
                    } else {
                        // If it doesn't exist, then create it and mark it as ours,
                        // so we can check for collisions.
                        base[part] = {};
                        base[part][isc.SGWTFactory.BEAN_CLASS_PARTS] = true;
                    }

                    base = base[part];
                }

                // There can't be a collision at this stage, since we would have
                // caught it when checking the isc object itself.
                var lastPart = beanClassParts[beanClassParts.length - 1];
                base[lastPart] = classObj;
            }
            
            // Wait until here to actually store the proxy in the isc object, in
            // case we bail out
            isc[this.beanClassName] = classObj;
        }
        catch (e) {
            this.logError(e.message);
        }
    },

    // Returns a new SmartGWT instance. Note that this is an instance method,
    // not a class method. However, the semantics are the same as
    // isc.Class.create -- that is, the properties are the entire
    // initialization config, and we return a SmartClient object.
    create : function (A,B,C,D,E,F,G,H,I,J,K,L,M) {
        //>EditMode 
        // Capture clean initialization data, and don't construct the actual
        // instance.  This is used to load a set of components for editing. 
        if (isc.captureDefaults) {
            
            var       level = isc.createLevel;
            isc.createLevel = isc.keepGlobals ? (level == null ? 1 : level + 1) : null;

            
            isc.captureDefaults = false;

            var component = {
                type: this.Class,
                defaults: isc.addProperties({}, A,B,C,D,E,F,G,H,I,J,K,L,M)
            }

            // Delete redundant _constructor if it was supplied -- see below
            delete component.defaults._constructor;

            if (!isc.capturedComponents) isc.capturedComponents = [];
            isc.capturedComponents.add(component);

            if (component.defaults.ID) {
                isc.ClassFactory.addGlobalID(component, component.defaults.ID);
                //isc.Log.logWarn("adding global component: " + component.defaults.ID);
            }

            // restore original value of isc.captureDefaults
            isc.captureDefaults = true;
            // restore original value of isc.createLevel
            isc.createLevel = level;

            return component;
        }
        //<EditMode
        
        var properties = isc.addProperties({}, A,B,C,D,E,F,G,H,I,J,K,L,M);

        // If _constructor has been supplied as a property, it is redundant,
        // since it has already been used to pick the desired SGWTFactory. So,
        // we delete it. If we left it in, the conversion of the properties to
        // their Java equivalent would actually construct the object, which
        // would be premature!
        delete properties._constructor;

        var sgwtInstance = this.sgwtModule.newInstance(this.beanClassName, properties);

        // Of course, the semantics of "create" are to return the Smartclient
        // object, not the opaque SmartGWT object. So, we get or create it. At
        // this point, it's appropriate to call getOrCreateJsObj, because the
        // properties supplied to create are, by definition, all of the
        // creation properties.
        return this.getOrCreateJsObj(sgwtInstance);
    },

    // createRaw would be called on a normal SmartClient class as a highly
    // efficient way of creating an object -- typically followed by directly
    // setting some properties and then callling init(), or completeCreation(),
    // or _completeCreationWithDefaults(). The usual semantics are that
    // createRaw returns the SmartClient native object *itself*, but with no
    // initialization having been done. That won't exactly work if we're
    // creating a SmartGWT object, because SmartGWT defers creating the native
    // SmartClient object until it has collected the configuration, in order to
    // preserve the distinction between configuration and later changes.
    //
    // Thus, we can't return the ultimate SmartClient object without
    // initialization, since SmartGWT doesn't want to create it before
    // initialization. So, we'll return a special object whose purpose is to
    // collect configuration properties and then apply them lazily when the
    // native SmartClient object is actually created. Unfortunately, code
    // calling createRaw does need to be aware of this, because it needs to
    // explicitly dereference the SmartClient object. Fortunately, there aren't
    // that many usages of createRaw.
    createRaw : function () {
        var factory = this;
        
        // We return an object which will accept property assignments and then
        // lazily create the real Javascript object and return it. This
        // respects the *semantics* of createRaw, though it does not retain its
        // actual efficiency if what we are creating is a SmartGWT object.
        // However, the code calling createRaw does need to check whether to
        // dereference the real object after it is created.
        var sgwtConfigBlock = {
            getClass : function () {
                return isc[factory.beanClassName]
            },

            init : function (A,B,C,D,E,F,G,H,I,J,K,L,M) {
                isc.addProperties(this, A,B,C,D,E,F,G,H,I,J,K,L,M);
                
                // Self-destruct, since we don't want to pass "init" or
                // "completeCreation" as a property to create(). Note that
                // "this" is deliberate -- we're referring to our anonymous
                // config object.
                delete(this.init);
                delete(this.completeCreation);
                delete(this.getClass);
                delete(this[isc.SGWTFactory.CONFIG_BLOCK]);

                // SectionStack sets __ref to null when creating a
                // SectionHeader, to deal with some differences between the way
                // that SmartClient and SmartGWT manage SectionStackSection and
                // SectionHeader. But that doesn't make sense in this code
                // path, because here it is SmartGWT that actually handles the
                // creation.  So, we'll delete a null __ref if provided, and
                // let SmartGWT provide its own.
                if (this[isc.gwtRef] === null) delete this[isc.gwtRef];

                //>EditMode 
                // Capture clean initialization data, and don't construct the actual
                // instance.  This is used to load a set of components for editing. 
                if (isc.captureDefaults) {
                    
                    var       level = isc.createLevel;
                    isc.createLevel = isc.keepGlobals ? (level == null ? 1 : level + 1) : null;

                    
                    isc.captureDefaults = false;

                    var component = {
                        type: factory.beanClassName,
                        defaults: isc.addProperties({}, this)
                    } 
                    if (!isc.capturedComponents) isc.capturedComponents = [];
                    isc.capturedComponents.add(component);

                    if (component.defaults.ID) {
                        isc.ClassFactory.addGlobalID(component, component.defaults.ID);
                        //isc.Log.logWarn("adding global component: " + component.defaults.ID);
                    }
                    this[isc.SGWTFactory.SC_INSTANCE] = component;
                    this[isc.SGWTFactory.CONFIG_BLOCK] = true;

                    // restore original value of isc.captureDefaults
                    isc.captureDefaults = true;
                    // restore originnal value of isc.createLevel
                    isc.createLevel = level;

                    return;
                }
                //<EditMode
                
                // Create the property that calling code will need to
                // dereference ... doesn't seem to be a way of avoiding that.
                // Again, note that "this" is deliberate.
                this[isc.SGWTFactory.SC_INSTANCE] = factory.create(this);

                // And add the marker back
                this[isc.SGWTFactory.CONFIG_BLOCK] = true;
            },

            // In the completeCreation case, we may get some extra properties
            // to apply. We just fall through to init above, after adding
            // the properties. Again, the use of "this" is deliberate.
            completeCreation : function (A,B,C,D,E,F,G,H,I,J,K,L,M) {
                this.init(A,B,C,D,E,F,G,H,I,J,K,L,M);
                return this[isc.SGWTFactory.SC_INSTANCE];
            }
        }

        // Add a marker so that we know it's a config block, rather than a
        // partially created SmartClient object
        sgwtConfigBlock[isc.SGWTFactory.CONFIG_BLOCK] = true;
        return sgwtConfigBlock;
    },

    // Apply json string or object properties to the specified sgwtInstance.
    setSGWTProperties : function (sgwtInstance, json) {
        if (json) {
            if (isc.isA.String(json)) {
                if (!(isc.startsWith(json, '(') && isc.endsWith(json, ')'))) {
                    json = '(' + json + ')';
                }
                json = isc.Class.evaluate(json);
            }

            for (var name in json) {
                this.sgwtModule.setProperty(sgwtInstance, name, json[name]);
            }
        }
    },

    // Returns the names of the properties of the beanClass as an array
    getSGWTAttributes : function () {
        return this.sgwtModule.getAttributes(this.beanClassName);
    },

    // Returns the property of the SGWT instance
    getSGWTProperty : function (sgwtInstance, property) {
        return this.sgwtModule.getProperty(sgwtInstance, property);
    },

    // Returns the property of the SGWT instance as a string, preferring
    // getters that natively return a string (if present)
    getSGWTPropertyAsString : function (sgwtInstance, property) {
        return this.sgwtModule.getPropertyAsString(sgwtInstance, property);
    },

    // If the passed object represents an SGWT instance, invoke it's
    // getOrCreateJsObj() method so that the SC object is returned.
    getOrCreateJsObj : function (sgwtInstance) {
        return this.sgwtModule.getOrCreateJsObj(sgwtInstance);
    },

    // If the passed object represents an SGWT instance, invoke its
    // setJsObj() method (or equivalent) to reset the JS object it refers to
    setJsObj : function (sgwtInstance, jsObj) {
        this.sgwtModule.setJsObj(sgwtInstance, jsObj);
    },

    getSGWTSuperClass : function () {
        return this.sgwtModule.getSGWTSuperClass(this);
    },

    isSGWTFrameworkClass : function () {
        return this.sgwtModule.isSGWTFrameworkClass(this);
    },

    getDefaultScClassName : function () {
        return this.sgwtModule.getDefaultScClassName(this);
    },

    getDefaultScClass : function () {
        return isc.ClassFactory.getClass(this.getDefaultScClassName());
    },

    isA : function (className) {
        if (className == null) return false;

        // First, we see whether our scClass equivalent isA clasName, since
        // what you'll get from create() is the scClass 
        var scClass = this.getDefaultScClass();
        if (scClass && scClass.isA(className)) return true;

        // If not, then see whether className is a superclass on the SGWT side.

        // handle being passed Class Objects and instances of classes
        if (!isc.isA.String(className)) {
            if (className.getSGWTClassName) className = className.getSGWTClassName();
            if (!isc.isA.String(className)) return false;
        }

        if (isc.startsWith(className, isc.ClassFactory._$iscPrefix)) {
            className = className.substring(4);
        }

        // walk the class object inheritance chain
        var superClass = this;
        while (superClass) {
            if (superClass.beanClassName == className) return true;
            superClass = superClass.getSGWTSuperClass();
        }

        return false;
    }
});

isc.SGWTFactory.addProperties({
    // Synonym for "create", because isc.ClassFactory calls it
    newInstance : isc.SGWTFactory.getInstanceProperty("create")
});
