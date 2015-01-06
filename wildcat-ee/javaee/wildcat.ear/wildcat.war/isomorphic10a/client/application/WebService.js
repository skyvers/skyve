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


//> @class WebService
// Class representing a WebService definition derived from a WSDL file.
// <P>
// A Web Service object allows you to invoke operations (via
// +link{method:WebService.callOperation(),callOperation()}), inspect schema declared in the
// WSDL file (+link{method:WebService.getSchema(),getSchema()}), and perform simple read-only
// databinding +link{method:WebService.getFetchDS()}.  
// <P>
// Once a WebService has been loaded, a DataSource can be declared with a
// +link{attr:DataSource.serviceNamespace} to connect it to the web service, allowing DataSource
// data to be loaded and saved to the web service using
// +link{class:OperationBinding,operationBindings}.
//
// @group webService
// @treeLocation Client Reference/Data Binding
// @visibility xmlBinding
//<


isc.defineClass("Schema", "DataSource").addProperties({
    dataFormat : "xml",

    // since the schema already knows the types and can be consulted for type information at
    // any time, when decoding data we drop all namespace declarations on nodes.
    dropNamespaceDeclarations:true,

    // NOTE: currently all subclasses of Schema are generated from various XML formats.  We
    // assume they shouldn't be global variables, which is really just intended as a
    // convenience for user-authored DataSources.
    addGlobalId:false
});

isc.defineClass("WSDLMessage", "Schema").addMethods({

    
    getWSOperation : function (dsRequest) {
        var service = this.getWebService(dsRequest);
        // being invoked by way of an entity DataSource performing an operation
        if (dsRequest && dsRequest.wsOperation) return service.getOperation(dsRequest.wsOperation);
        // being invoke standalone, eg, callOperation()
        else return service.getOperationForMessage(this.ID.substring(8));
    }
});

isc.defineClass("XSElement", "Schema");
isc.defineClass("XSComplexType", "Schema");

//> @class SchemaSet
// A set of schema derived from the &lt;xsd:schema&gt; element in a WSDL or XML schema file
// loaded by +link{XMLTools.loadWSDL()} or +link{XMLTools.loadXMLSchema()}.
//
// @treeLocation Client Reference/Data Binding
// @visibility xmlBinding
//<
isc.defineClass("SchemaSet").addMethods({
    //> @attr schemaSet.schemaNamespace (URI : null : R)
    // Namespace of this SchemaSet, derived from the <code>targetNamespace</code>
    // attribute of the <code>&lt;schema&gt;</code> element.
    //
    // @group webService
    // @visibility xmlBinding
    //<
    init : function () {
	    this.ns.ClassFactory.addGlobalID(this);

        // register the schemaSet globally with the SchemaSet class
        var schemaNamespace = this.schemaNamespace,
            registry = isc.SchemaSet.schemaSets,
            existingSchema = registry[schemaNamespace];

        // an xs:schema that contains only an xs:import generates an empty SchemaSet.  Don't
        // clobber an existing, non-empty schemaset with an empty version of the schemaset
        // loaded later
        if (existingSchema == null || 
            // empty existing schemaset
            ((existingSchema.schema == null && existingSchema.schema.length == 0) &&
            // non-empty new schemaset
            (this.schema != null && this.schema.length != 0)))
        {
            registry[schemaNamespace] = this;
        }

        // index all schema within this schemaset
        var serviceNamespace = this.serviceNamespace;
        if (this.schema) {
            this._typeIndex = {};
            this._elementIndex = {};
            this._simpleTypeIndex = {};

            for (var i = 0; i < this.schema.length; i++) {
                var schema = this.schema[i];
                // ensure all schema that belong to this set have their schemaNamespace
                // attribute set
                schema.serviceNamespace = serviceNamespace;
                schema.schemaNamespace = schemaNamespace;
                schema.location = this.location;

                // make an index of all the schema in this SchemaSet
                if (isc.isA.SimpleType(schema)) {
                    
                    if (schema.inheritsFrom && schema.inheritsFrom == schema.name &&
                        schema.xmlSource == "XSElement") continue;
                    this._simpleTypeIndex[schema.name] = schema;
                } else if (schema.ID) {
                    
                    if (isc.isAn.XSElement(schema)) {
                        this._elementIndex[schema.ID] = schema;
                    } else {
                        this._typeIndex[schema.ID] = schema;
                    }
                }
            }
        }
        // for loadXMLSchema() callback to return loaded SchemaSet
        isc.SchemaSet._lastLoaded = this; 
    },

    //> @method schemaSet.getSchema() [A]
    // Get the schema definition of any complexType or element of complexType defined within
    // the &lt;schema&gt; element this SchemaSet represents.
    //
    // @param schemaName (String) name of the schema to retrieve
    // @param [schemaType] (String) optional type of schema to return, either "element" for
    //                              xs:element definitions only or "type" for xs:complexType
    //                              definitions.  If unspecified, either will be returned,
    //                              with types preferred if names collide
    // @return (DataSource) the schema if found, or null
    // @visibility xmlBinding
    // @example xmlSchemaImport
    //<
    getSchema : function (schemaName, schemaType, alreadyVisited) {
        // xs:schema routinely import each other, so avoid looping getSchema calls
        if (!alreadyVisited) alreadyVisited = [this];
        else alreadyVisited.add(this);

        var schema;
    
        // try local, type-specific indexes 
        if (schemaType == isc.DS._$element) schema = this._elementIndex[schemaName];
        else if (schemaType == isc.DS._$type) schema = this._typeIndex[schemaName];

        // if schemaType wasn't specified take either type of schema locally
        if (schemaType == null) {
            schema = this._typeIndex[schemaName] || this._elementIndex[schemaName];
            if (schema != null) return schema;
        }

        // resolve all <xs:import> tags to find already loaded schemaSets we imported
        if (!this._lookedUpImports) {
            isc.SchemaSet.findLoadedImports(this);
            this._lookedUpImports = true;
        }

        // try imported schema if present
        var schemaSets = this._schemaSets;
        if (schemaSets != null) {
            for (var i = 0; i < schemaSets.length; i++) {
                var schemaSet = schemaSets[i];
                if (alreadyVisited.contains(schemaSet)) continue;
                schema = schemaSet.getSchema(schemaName, schemaType, alreadyVisited);
                if (schema != null) return schema;
            }
        }
    },

    getSimpleType : function (typeName, alreadyVisited) {
        // xs:schema routinely import each other, so avoid looping getSchema calls
        if (!alreadyVisited) alreadyVisited = [this];
        else alreadyVisited.add(this);

        var simpleType;
        if (this._simpleTypeIndex) {
            simpleType = this._simpleTypeIndex[typeName];
            if (simpleType) return simpleType;
        }

        if (this._schemaSets != null) {
            for (var i = 0; i < this._schemaSets.length; i++) {
                var schemaSet = this._schemaSets[i];
                if (alreadyVisited.contains(schemaSet)) continue;
                simpleType = schemaSet.getSimpleType(typeName, alreadyVisited);
                if (simpleType != null) return simpleType;
            }
        }
    },

    setLocation : function (location) {
        this.location = location;
        if (!this.schema) return;

        for (var i = 0; i < this.schema.length; i++) {
            var schema = this.schema[i];
            schema.location = location;
        }
    },

    loadImports : function (callback) {
        isc.SchemaSet.loadImports(callback, this);
    },

    // override point for changing how imported schema are loaded
    // NOTE: duplicated in WebService
    loadImport : function (namespace, location, callback, isWSDL) {
        return isc.SchemaSet.loadImport(namespace, location, callback, isWSDL, this);
    },
    doneImporting : function () {
        this.fireCallback(this._doneImportingCallback);
    },

    // when captureXML has been set in loadWSDL / loadXMLSchema, this method is called on the
    // initiator of a series of schema loads so that complete source is available
    addImportXMLSource : function (xmlText, location) {
        this.importSources = this.importSources || [];
        this.importSources.add({
            xmlText : xmlText,
            location : location
        })
    },

    addSchemaSet : function (schemaSet, namespace) {
        this._imports = this._imports || [];
        this._imports.add(schemaSet);
    }

});
isc.SchemaSet.addClassMethods({
    schemaSets : {},

    //> @classMethod SchemaSet.get() [A]
    // Retrieve a SchemaSet object by it's schemaNamespace.
    //
    // @param schemaNamespace (String) uri from the "targetNamespace" attribute of the
    // &lt;xsd:schema&gt; element from the XML Schema or WSDL file this SchemaSet was derived
    // from.
    // @return (SchemaSet) the requested SchemaSet, or null if not loaded
    // 
    // @visibility xmlBinding
    //<
    get : function (schemaNamespace) {
        return this.schemaSets[schemaNamespace];
    },

    // find already loaded SchemaSets to fulfill <xs:import>s from within a WSDL or XML Schema
    // file.  Note loadImports() is what would actually attempt to load imported files from
    // the "location" attribute provided on the <xs:import> tag (if any)
    // "loader" may be an instance of WebService or SchemaSet
    findLoadedImports : function (loader) {
        var imports = this.getAllImports(loader);

        if (!imports) return;

        var schemaSets = loader._schemaSets = loader._schemaSets || [];
        var webServices = loader._webServices = loader._webServices || [];

        for (var i = 0; i < imports.length; i++) {
            var importDef = imports[i],
                isWSDL = importDef.isWSDL,
                importNamespace = importDef.namespace;

            if (this._ignoreImports.contains(importNamespace)) continue;

            // we already have it
            if ((isWSDL && webServices.find("serviceNamespace", importNamespace)) ||
                (!isWSDL && schemaSets.find("schemaNamespace", importNamespace)))
                continue;

            var importObj = isWSDL ? 
                    isc.WebService.get(importNamespace) : isc.SchemaSet.get(importNamespace);
            if (importObj == null) {
                var preamble;
                if (isc.isA.WebService(loader)) {
                    preamble = "WebService with targetNamespace '" + loader.serviceNamespace;
                } else {
                    preamble = "SchemaSet with targetNamespace '" + loader.schemaNamespace;
                }
                // it's common for WSDL or XSD to contain a bunch of imports for which there is
                // no actual file, in a usage almost like "marker interfaces" in Java.  If
                // there's no location, don't consider this a warning.
                var logMethod = importDef.location ? "logWarn" : "logInfo";
                loader[logMethod](preamble + "' could not find " +
                                  (isWSDL ? "webService" : "SchemaSet") +
                                  " for namespace: '" + importNamespace + 
                                  "'. Pass autoLoadImports to loadWSDL()/loadXMLSchema() or " +
                                  "separately load via loadWSDL/loadXMLSchema jsp tag or method", 
                                   "schemaLoader");
                continue;
            }
            // any schemaSet that was independently loaded would have it's own location
            // already set.  If it's got no location assume it was loaded with this web
            // service.  XXX not true of schema loaded by JSP tags
            if (importObj.location == null) importObj.setLocation(loader.location);
            isWSDL ? webServices.add(importObj) : schemaSets.add(importObj);
        }
    },

    getAllImports : function (loader) {
        var imports = loader.schemaImports;
        if (loader.wsdlImports) {
            loader.wsdlImports.setProperty("isWSDL", true);
            imports = imports || [];
            imports = imports.concat(loader.wsdlImports);
        }
        return imports;
    },

    // fulfill <xs:import>s from within a WSDL or XML Schema by actually attempting to load 
    // the imported XML Schema from the "location" attribute provided on the <xs:import> tag
    // (if any)
    loadImports : function (callback, loader) {
        loader._doneImportingCallback = callback;

        var imports = this.getAllImports(loader);

        // if nothing to import, fire callback immediately
        if (!imports) return loader.doneImporting();

        loader._importCount = 0;

        //loader.logWarn("imports are: " + loader.echoFull(imports));

        for (var i = 0; i < imports.length; i++) {
            var importDef = imports[i],
                namespace = importDef.namespace;

            if (namespace) {
                var alreadyLoaded = (importDef.isWSDL ? 
                    isc.WebService.get(namespace) :
                    isc.SchemaSet.get(namespace));
                if (alreadyLoaded != null) { 
                    loader.logDebug("import already loaded: " + namespace + ", skipping",
                                    "schemaLoader");
                    continue;
                }
            }
            if (importDef.location && importDef.location != loader.location) {
                var requested = loader.loadImport(namespace, importDef.location, function (loadedObj) {
                    if (isc.isA.WebService(loadedObj)) {
                        loader.addWebService(loadedObj, namespace);
                    } else {
                        loader.addSchemaSet(loadedObj, namespace);
                    }
                    loader._importCount--;
                    loader.logInfo(loader + " loaded import: " + loadedObj +
                                    " as namespace: " + namespace +
                                    ", remaining imports: " + loader._importCount,
                                    "schemaLoader");
                    if (loader._importCount == 0) loader.doneImporting();
                }, importDef.isWSDL);
                if (requested) loader._importCount++;
            }
        }
        // no attempts to load imports, fire callback now
        if (loader._importCount == 0) loader.doneImporting();
    },
    loadImport : function (namespace, location, callback, isWSDL, loader) {
        // "location" attribute is intended to the location of the file doing the import, so
        // combine URLs
        var baseDir = loader.location.substring(0, loader.location.lastIndexOf("/"));
        if (!baseDir.endsWith("/")) baseDir += "/";
        var url = isc.Page.combineURLs(baseDir, location);

        // strangely XML schema files sometimes import themselves
        if (url == loader.location) {
            loader.logDebug("skipping self-reference import: " + url, "schemaLoader");
            return false;
        }

        // skip certain pedantic imports like importing the XML namespace
        if (this._ignoreImports.contains(url)) {
            loader.logDebug("skipping pedantic import: " + url, "schemaLoader");
            return false;
        }

        // skip definitely redundant loads (this page has already loaded from this URL)
        if (this._allImports.contains(url)) {
            loader.logDebug("skipping redundant import: " + url, "schemaLoader");
            return false;
        }
        this._allImports.add(url);

        loader.logInfo("loading import from: " + url +
                       "\nschema/service base dir: " + baseDir + 
                       "\nimport location: " + location, "schemaLoader");
        
        var method = isWSDL ? "loadWSDL" : "loadXMLSchema";
        isc.xml[method](url, function (schemaSet) {
            loader.fireCallback(callback, "schemaSet", [schemaSet]);
        }, null, true, {
            // track the initiator of any series of schema loads (first WebService / SchemaSet
            // to have loaded)
            initiator:loader.initiator || loader,
            // recursively capture XML if the original call asked for it
            captureXML:loader.captureXML 
        });
        return true;
    },
    _ignoreImports : [
        "http://www.w3.org/2001/xml.xsd",
        "http://www.w3.org/2001/XMLSchema",
        "http://www.w3.org/XML/1998/namespace"
    ],
    _allImports : []
});

isc.SchemaSet.getPrototype().toString = function () {
    return "[" + this.Class + " ns=" + this.echoLeaf(this.schemaNamespace) + 
        (this.location ? " location=" + isc.Page.getLastSegment(this.location) : "") + "]";
};

//> @class WSRequest
// A WSRequest (or "web service request") is an extended RPCRequest with additional properties
// applicable to WSDL/SOAP web services.
// <P>
// All properties which are legal on +link{class:RPCRequest} are legal on a WSRequest, in
// addition to the properties listed here.
//
// @treeLocation Client Reference/Data Binding
// @see RPCRequest
// @visibility external
//<

//> @attr wsRequest.wsOperation (String : null : IR)
// Name of the web service operation to invoke.
//
// @visibility external
//<

//> @attr wsRequest.data (any : null : IR)
// Data to be serialized to XML to form the SOAP body.
//
// @visibility external
//<

//> @attr wsRequest.useFlatFields (boolean : null : IR)
// @include dsRequest.useFlatFields
// @visibility external
//<

//> @attr wsRequest.xmlNamespaces (Object : null : IR)
// Optional object declaring namespace prefixes for use in evaluating the
// <code>resultType</code> parameter of +link{WebService.callOperation()}, if resultType is an
// XPath.
// <P>
// Format is identical to +link{operationBinding.xmlNamespaces}, and default namespaces
// bindings are also identical.
//
// @visibility external
//<

//> @attr wsRequest.xmlResult (boolean : false : IR)
// Valid only with +link{WebService.callOperation()}.  If set, do not transform XML results to
// JavaScript.  Instead just return the XML nodes selected by the passed XPath or recordName,
// or all nodes within the SOAP body if no XPath was passed.
//
// @visibility external
//<

//> @attr wsRequest.headerData (any : null : IR)
// Data to be serialized to form the SOAP headers, as a map from the header part name to the
// data.  For example, given WSDL like this:
// <pre>
//     &lt;soap:header part="SessionHeader" message="tns:HeaderMessage"/&gt;
//     &lt;soap:header part="CallOptions" message="tns:HeaderMessage/&gt;
// </pre>
// <code>headerData</code> like this might be provided:
// <pre>
//     dsRequest.headerData = 
//         { SessionHeader : <i>data</i>
//           CallOptions : <i>data</i> };
// </pre>
// The provided data will be serialized to XML by the
// +link{webService.getInputHeaderSchema,SOAP header schema} via
// +link{dataSource.xmlSerialize()} 
//
// @visibility external
//<



isc.defineClass("WebService").addMethods({
    //> @attr webService.serviceNamespace (URI : null : R)
    // Namespace of this WebService, derived from the <code>targetNamespace</code>
    // attribute of the <code>&lt;wsdl:definitions&gt;</code> element.
    //
    // @group webService
    // @visibility xmlBinding
    //<
    init : function () {
        // mark all messages with the service namespace
        var namespace = this.serviceNamespace;
        if (this.messages) {
            for (var i = 0; i < this.messages.length; i++) {
                this.messages[i].serviceNamespace = namespace;
            }
        }

        // register globally
        this.logInfo("registered service with serviceNamespace: " + namespace +
                     " service name: " + this.name);
        isc.WebService.services.add(this);

        // for loadWSDL() callback to return WebService
        isc.WebService._lastLoaded = this; 
    },

    loadImports : function (callback) {
        isc.SchemaSet.loadImports(callback, this);
    },

    // override point for changing how imported schema are loaded
    // NOTE: duplicated in SchemaSet
    loadImport : function (namespace, location, callback, isWSDL) {
        return isc.SchemaSet.loadImport(namespace, location, callback, isWSDL, this);
    },
    doneImporting : function () {
        this.fireCallback(this._doneImportingCallback);
    },

    addSchemaSet : function (schemaSet, namespace) {
        this._schemaSets = this._schemaSets || [];
        this._schemaSets.add(schemaSet);
    },

    addWebService : function (webService, namespace) {
        this._webServices = this._webServices || [];
        this._webServices.add(webService);
    },

    // when captureXML has been set in loadWSDL / loadXMLSchema, this method is called on the
    // initiator of a series of schema loads so that complete source is available
    addImportXMLSource : function (xmlText, location) {
        this.importSources = this.importSources || [];
        this.importSources.add({
            xmlText : xmlText,
            location : location
        })
    },

    getOperation : function (operationName, portTypeName) {
        if (isc.isAn.Object(operationName)) return operationName;

        // ensure we've looked up related WSDL/XMLSchema imports
        if (!this._lookedUpImports) {
            isc.SchemaSet.findLoadedImports(this);
            this._lookedUpImports = true;
        }

        // look up the binding and portType definitions
        var bindingOperation = this.getBindingOperation(operationName, portTypeName);
        var portTypeOperation = this.getPortTypeOperation(operationName, portTypeName);
        
        if (!bindingOperation && !portTypeOperation) {
            this.logWarn(this + ": no such operation: '" + operationName + "'" +
                         (portTypeName ? " in portType: '" + portTypeName + "'" : ""));
            return null;
        }

        // and combine into a structure that has everything we need to know
        return isc.addProperties({}, portTypeOperation, bindingOperation);
    },
    
    // find an operation from a list of bindings or portTypes
    findOperation : function (operationName, portTypeName, bindingList, isPortType) {
        if (!bindingList) return;

        // if portTypeName is specified, look in only <binding>s or <portType>s of that name
        if (portTypeName) bindingList = bindingList.findAll("portTypeName", portTypeName);
        if (!bindingList) return;

        // check imported web service definitions
        if (this._webServices) {
            for (var i = 0; i < this._webServices.length; i++) {
                var webService = this._webServices[i],
                    method = isPortType ? "getPortTypeOperation" : "getBindingOperation",
                    operation = webService[method](operationName, portTypeName);
                if (operation != null) return operation;
            }
        }

        // otherwise look in any of them, so that only operationName needs to be specified so
        // long as it's unique
        for (var i = 0; i < bindingList.length; i++) {
            var operations = bindingList[i].operation;
            if (!isc.isAn.Array(operations)) operations = [operations];
            var operation = operations.find("name", operationName);
            if (operation != null) return operation;
        }
    },

    // get an <operation> definition from a <portType>
    // portTypes contain the inputMessage and outputMessage
    getPortTypeOperation : function (operationName, portTypeName) {
        return this.findOperation(operationName, portTypeName, this.portTypes, true);
    },
    // get an <operation> definition from a <binding>
    // bindings contain the soapAction, parts (which parts of the message to use), soapEncoding
    // style, input and output namespace (for encodings that cause an element to be output
    // corresponding to the operation name)
    getBindingOperation : function (operationName, portTypeName) {
        return this.findOperation(operationName, portTypeName, this.bindings);
    },

    getOperationForMessage : function (messageName) {
        var operations = this.getOperations();
        if (!operations) return;
        var operation = operations.find("inputMessage", messageName);
        if (operation) return operation;
        operation = operations.find("outputMessage", messageName);
        if (operation) return operation;
    },

    //> @method webService.getOperationNames()
    // @return (Array) names of the available operations supported by this service (array of strings)
    // @group webService
    // @visibility xmlBinding
    //<
    getOperationNames : function () {
        // return cached list
        var operationNames = this.operationNames;
        if (operationNames) return operationNames;

        // ensure we've looked up related WSDL/XMLSchema imports to connect to separately
        // loaded portTypes
        if (!this._lookedUpImports) {
            isc.SchemaSet.findLoadedImports(this);
            this._lookedUpImports = true;
        }

        operationNames = this.operationNames = [];
        if (this.bindings) {
            for (var i = 0; i < this.bindings.length; i++) {
                var binding = this.bindings[i],
                    operations = binding.operation;
                if (!isc.isAn.Array(operations)) operations = [operations];
                operationNames.addList(operations.getProperty("name"));
                // find the corresponding portType operation and mark it as having a binding
                for (var j = 0; j < operationNames.length; j++) {
                    var ptOperation = this.getPortTypeOperation(operationNames[j], binding.portTypeName);
                    if (ptOperation) ptOperation.hasBinding = true;
                }
            }
        }
        // add all operations on portType that don't have a binding
        if (this.portTypes) {
            for (var i = 0; i < this.portTypes.length; i++) {
                var portType = this.portTypes[i],
                    operations = portType.operation;
                if (!isc.isAn.Array(operations)) operations = [operations];
                var unbound = operations.findAll("hasBinding", true);
                if (unbound) {
                    operations = operations.duplicate();
                    operations.removeAll(unbound);
                }
                operationNames.addList(operations.getProperty("name"));
            }
        }

        // expensive, so cache it
        return (this.operationNames = operationNames);
    },

    // get operation definitions for all of the operations supported by this web service
    getOperations : function (boundOnly) {
        var operationNames = this.getOperationNames(),
            operations = [];
        for (var i = 0; i < operationNames.length; i++) {
            var operation = this.getOperation(operationNames[i]);
            if (boundOnly && !operation.hasBinding) continue;
            operations.add(operation);
        } 
        return operations;
    },

    //> @method webService.getSchema()
    // Get the schema definition of any complexType or element of complexType defined in any
    // &lt;schema&gt; blocks in the WSDL file this WebService represents.
    // 
    // @param schemaName (String) name of type or element
    // @param [schemaType] (String) optional type of schema to return, either "element" for
    //                              xs:element definitions only or "type" for xs:complexType
    //                              definitions.  If unspecified, either will be returned,
    //                              with types preferred if names collide
    // @return (DataSource) requested schema
    // @group webService
    // @visibility xmlBinding
    //<
    getSchema : function (name, schemaType) {
        // look up all the schemaSets that the WSDL file referred to.
        // do this lazily so order of creation doesn't matter for SchemaSets and WebServices
        // loaded from one WSDL file
        if (!this._lookedUpImports) {
            isc.SchemaSet.findLoadedImports(this);
            this._lookedUpImports = true;
        }

        var schemaSets = this._schemaSets;
        if (schemaSets != null) {
            // look through each schemaSet for a schema of this name
            for (var i = 0; i < schemaSets.length; i++) {
                var schemaSet = schemaSets[i];
                var schema = schemaSet.getSchema(name, schemaType);
                if (schema) return schema;
            }
        }
        
        // finally, look globally.  This is key for discovering schema loaded from separate
        // files via separate calls to loadXMLSchema.
        return isc.DS.get(name, null, null, schemaType);
    },

    // get the request or response message schema
    getRequestMessage : function (operationName) {
        var operation = this.getOperation(operationName);
        return this.getMessage(operation.inputMessage);
    },
    getResponseMessage : function (operationName) {
        var operation = this.getOperation(operationName);
        return this.getMessage(operation.outputMessage);
    },

    getMessage : function (messageName) {
        var message = this.messages.find("ID", "message:" + messageName);
        if (message) return message;

        // ensure we're connected to any imported WSDL services, which may contain message
        // definitions
        if (!this._lookedUpImports) {
            isc.SchemaSet.findLoadedImports(this);
            this._lookedUpImports = true;
        }

        // look in imported services
        if (this._webServices) {
            for (var i = 0; i < this._webServices.length; i++) {
                var webService = this._webServices[i];
                message = webService.getMessage(messageName);
                if (message) return message;
            }
        }
    },

    getBodyPartNames : function (operationName, isOutput) {
        var operation = this.getOperation(operationName),
            bodyParts = isOutput ? operation.outputParts : operation.inputParts;
        if (bodyParts == null || isc.isAn.emptyString(bodyParts)) {
            // all body parts should be used
            var message = isOutput ? this.getResponseMessage(operationName) :
                                     this.getRequestMessage(operationName);
            return message.getFieldNames();
        } else {
            return bodyParts.split(" ");
        }
    },

    //> @attr webService.globalNamespaces (Object : ... : IRW)
    // @include dataSource.globalNamespaces
    //<
    globalNamespaces : {
        xsi: "http://www.w3.org/2001/XMLSchema-instance",
        xsd: "http://www.w3.org/2001/XMLSchema"
    },

    //> @method webService.callOperation()
    // Invoke a web service operation.
    // <P>
    // The <code>data</code> parameter will be serialized to XML to form the input message for
    // the operation, as described by +link{method:DataSource.xmlSerialize()}.  Namespacing,
    // element ordering, and SOAP encoding rules are automatically followed.  If the web
    // service you are trying to contact requires a complicated nested structure, consider
    // using +link{wsRequest.useFlatFields} to simplify the required JavaScript input data.
    // <P>
    // The <code>resultType</code> selects what part of the message should be decoded to
    // JavaScript and made available as the "data" variable in the callback.  The
    // <code>resultType</code> parameter can be either:
    // <ul>
    // <li> an XPath.  "data" will be always be an Array, containing the selected elements as
    // decoded by +link{XMLTools.toJS()}.  All properties will have String value.
    // <li> the name of an XML Schema type found somewhere in the response.  You can use the
    // WSDL tab of the Developer Console to analyze the WSDL file for an appropriate type name.
    // "data" will be an Array, containing the decoded elements as decoded by
    // +link{dataSource.recordsFromXML()}.  In this case, since the XML Schema type of the
    // selected data is known, properties will have correct type (eg "date" fields will
    // have JavaScript Date objects)
    // <li> null.  "data" will an Object representing the entire &lt;SOAP:Body&gt; as decoded
    // to JavaScript.  As above, properties will have correct type.
    // </ul>
    // In the callback, you also receive the XML document returned by the web service as
    // "xmlDoc".
    // <P>
    // NOTE: <code>callOperation()</code> is appropriate for simple operations that do not
    // involve DataBound Components, such as logging into a web service, or retrieving simple
    // String data.  <code>callOperation()</code> can also be used to retrieve small, read-only
    // datasets such as the option list for a SelectItem, but only if the dataset is guaranteed
    // to remain small enough for paging to be unnecessary.  For any larger datasets or
    // anything that will be edited, DataSource integration is more appropriate.
    //
    // @param operationName (String)    Name of the operation to invoke
    // @param data          (Object)    data to serialize as XML to form the inbound message of
    //                                  the operation
    // @param resultType    (Type or ElementName or XPath) Type, Element name, or XPath that
    //                                  should be selected from the result.  For XPaths, see
    //                                  +link{wsRequest.xmlNamespaces} for available namespace
    //                                  prefixes and how to add more.
    // @param callback      (Callback)  Callback to invoke on completion.  Signature
    //                                  callback(data, xmlDoc, rpcResponse, wsRequest)
    // @param requestProperties (WSRequest Properties) Additional properties for the WSRequest, such
    //                                      as HTTPHeaders
    // 
    // @group webService
    // @visibility xmlBinding
    // @example wsdlOperation
    //<
    
    callOperation : function (operationName, data, resultType, callback, requestProperties)
    {

        var operation = this.getOperation(operationName);
        if (operation == null) {
            this.logWarn("No such operation: " + operationName);
            return;
        }

        requestProperties = requestProperties || isc.emptyObject;

        var wsRequest = isc.addProperties({
            actionURL: this.getDataURL(operationName), 
            httpMethod: "POST",
            contentType: "text/xml",
            data : data,
            serviceNamespace : this.serviceNamespace,
            // NOTE: this ensures that all DataSources involved in serialization consistently
            // lookup the WebService instance that callOperation was called on, even if there
            // are multiple WSDL files that defined <WebService>s in a common namespace
            serviceName : this.name,
            wsOperation : operationName
        }, requestProperties);

        wsRequest.httpHeaders = isc.addProperties({}, 
                                                  { SOAPAction : operation.soapAction || '""' },
                                                  requestProperties.httpHeaders); 

        wsRequest.headerData = requestProperties.headerData || this.getHeaderData(wsRequest);

        // create the SOAP message based on the WSRequest
        wsRequest.data = this.getSoapMessage(wsRequest);


        wsRequest.internalClientContext = {
            _callOperationCallback : callback,
            _operationName : operationName,
            _resultType : resultType,
            // special flag to return selected XML nodes without JS translation
            _xmlResult : requestProperties.xmlResult
        };

        if (this.spoofResponses) {
            var sampleResponse = this.getSampleResponse(operationName);
            if (this.logIsDebugEnabled("xmlBinding")) {
                this.logDebug("spoofed response:\n" + sampleResponse, "xmlBinding");
            }
            this.delayCall("_callOperationReply",
                           [isc.xml.parseXML(sampleResponse), sampleResponse,
                            {status:0,
                             internalClientContext: wsRequest.internalClientContext,
                             httpResponseCode:200,
                             httpResponseText:sampleResponse}, wsRequest]);
            return;
        }

        wsRequest.callback = { target:this, methodName:"_callOperationReply" };
        isc.xml.getXMLResponse(wsRequest);
    },
    _callOperationReply : function (xmlDoc, xmlText, rpcResponse, rpcRequest) {
        var context = rpcRequest.internalClientContext,
            operationName = context._operationName,
            resultType = context._resultType;
        
        // If there was an error, we will only arrive here if willHandleError is true.
        // Just fire the user's callback and let them decide what to do.
        if (rpcResponse.status < 0) {
            this.fireCallback(context._callOperationCallback, 
                              "data,xmlDoc,rpcResponse,wsRequest", 
                              [rpcResponse.data,xmlDoc,rpcResponse,rpcRequest]);   
            return;
        }

        xmlDoc.addNamespaces(this.getOutputNamespaces(operationName));
        if (rpcRequest.xmlNamespaces) {
            xmlDoc.addNamespaces(rpcRequest.xmlNamespaces);
        }

        // we were passed a type (FIXME crude detection)
        var passedXPath = (resultType != null && resultType.contains("/")),
            xPath = (passedXPath ? resultType : null),
            data;

        if (passedXPath) {
            // apply XPath selector if passed one or passed resultType
            data = xmlDoc.selectNodes(xPath);
        } else if (resultType) {
            data = this.selectByType(xmlDoc, operationName, resultType);
        } else {
            // if no XPath or resultType was given, select the soap body
            data = xmlDoc.selectNodes("//s:Body/*", 
                                      { s:"http://schemas.xmlsoap.org/soap/envelope/" });
            // don't create a spurious Array for the most common case of a singular body
            // element
            if (data.length == 1) data = data[0];
        }
    
        if (this.logIsDebugEnabled()) {
            this.logDebug("selected response data is: " + this.echoFull(data));
        }
    
        if (context._xmlResult) {
            // just return the raw XML nodes
            this.fireCallback(context._callOperationCallback, 
                              "data,xmlDoc,rpcResponse,wsRequest", 
                              [data,xmlDoc,rpcResponse,rpcRequest]);
            return;
        }

        // transform to JS
        var schema;
        if (passedXPath) {
            // if an xpath was passed, we don't know the schema of the selected elements, just
            // use schemaless transform
            schema = null;
        } else if (resultType) {
            // if we were passed a resultType, use that as the schema to transform nodes with
            // correct typing
            schema = this.getSchema(context._resultType);
        } else {
            // passed neither an xPath nor a resultType, so we selected the whole SOAP body.
            // We can use the message schema to decode the entire SOAP body, with correct
            // typing.
            var messageSchema = 
                    this.getSchema("message:"+this.getOperation(operationName).outputMessage);
            
            if (this.getSoapStyle(operationName) != "document") {
                schema = messageSchema;
            } else {
                var firstField = messageSchema.getFieldNames().first();
                schema = messageSchema.getSchema(messageSchema.getField(firstField).type);
            }
        }
        //this.logWarn("transforming reply for operation: " + operationName + 
        //             " toJS using schema " + schema);
        data = isc.xml.toJS(data, null, schema);
        
        this.fireCallback(context._callOperationCallback, 
                          "data,xmlDoc,rpcResponse,wsRequest", 
                          [data,xmlDoc,rpcResponse,rpcRequest]);                          
    },
 
    // when applying an XPath selector to the output of a web service, our default namespacing
    // strategy of providing all the namespaces declared on the document element
    // generally fails because we just get SOAP-related namespaces.  Furthermore, the web
    // service may use auto-generated prefixes for namespaces, so in general we can't rely on
    // the returned document alone for reasonable namespace prefixes.  Instead, provide the
    // schema namespace from the outermost element, and the service namespace
    getOutputNamespaces : function (operation, namespaces) {
        var schema = this.getDefaultOutputDS(operation); 
        return isc.addProperties({
            "default" : schema.schemaNamespace || this.serviceNamespace,
            schema : schema.schemaNamespace,
            service : this.serviceNamespace
        }, namespaces);
    },

    getDataURL : function (operationName) {
        // NOTE: per-operation URLs can't be defined in WSDL, this is here for spoofing
        var operation = this.getOperation(operationName);
        if (operation && operation.dataURL) return operation.dataURL;
        return this.dataURL;
    },

    // SOAP message serialization
    // ---------------------------------------------------------------------------------------

    //> @method webService.getMessageSerializer() [A]
    // Get the schema used to serialize the entire request
    // 
    // @param operationName (String or WSRequest Properties) name of the web service operation,
    //                                            or a WSRequest specifying it
    // @param forResponse (boolean) whether a serializer is request for the response message,
    //                              as opposed to the request message (the default)
    // @return (DataSource) schema used for serialization
    //<
    getMessageSerializer : function (operationName, forResponse) {
        var serializer = forResponse ? this.getResponseMessage(operationName)
                                     : this.getRequestMessage(operationName);

        if (serializer == null) {
            this.logWarn("no " + (forResponse ? "response" : "request") +
                         " message definition found for operation: '" + operationName + "'");
            return;
        }

        // in rpc-style soap, the outermost element of the body is named after the message
        // name.  In document-style soap, there is no element that corresponds to the message
        // name, only it's contents.  Therefore for document-style SOAP if there is exactly one
        // subelement of the message (the most common style by far), use that as the input
        // schema.  This means that when a message is supposed to look like this:
        //    <login>
        //       <username>bob</username>
        //       <password>mebob</password>
        //    </login>
        // The JS data you need to pass is: 
        //    { username:"bob", password:"mebob" }
        // .. instead of the surprising and less obvious:
        //    { login : { username:"bob", password:"mebob" } }
        if (this.getSoapStyle(operationName) != "document") return serializer;

        var fieldNames = serializer.getFieldNames();
        if (fieldNames.length == 1 && serializer.fieldIsComplexType(fieldNames[0])) {
            var field = serializer.getField(fieldNames[0]);
            //this.logWarn("skipping message element and using field: " + this.echo(field));
            serializer = serializer.getSchema(field.type, 
                                              field.xsElementRef ? "element" : null);
            if (serializer == null) {
                this.logWarn("can't find schema: " + field.type + ", part of " +
                             (forResponse ? "response" : "request") + 
                             " message for operation '" + operationName + "'");
            }
        }
        return serializer;
    },

    // whether this operation uses simplified inputs, that is, does not expect data to contain
    // an object named after the message name, since the message name does not appear in the
    // generated message itself.  Useful for callers who form a data structure that exactly
    // corresponds to the message structure (ServiceOperation).
    useSimplifiedInputs : function (operationName, forResponse) {
        var normalSerializer = forResponse ? this.getResponseMessage(operationName)
                                           : this.getRequestMessage(operationName);
        return this.getMessageSerializer(operationName, forResponse) != normalSerializer;
    },

    //> @method webService.getSoapMessage() [A]
    // Return the SOAP message that will be formed from this WSRequest.
    // 
    // @param wsRequest (WSRequest Properties) web service request object
    // @return (String) SOAP message
    // @visibility xmlBinding
    //<
    getSoapMessage : function (wsRequest, flags) {
        wsRequest.serviceNamespace = wsRequest.serviceNamespace || this.serviceNamespace;
        var operationName = wsRequest.wsOperation; 

        if (this.getOperation(operationName) == null) {
            this.logWarn("no such operation: '" + operationName + 
                         "' in service: " + this.serviceNamespace);
            return "";
        }

        var messageSerializer = 
                this.getMessageSerializer(wsRequest.wsOperation, 
                                          flags && flags.generateResponse);

        // already warned about in getMessageSerializer
        if (messageSerializer == null) return "";

        
        wsRequest._webService = this;

        return messageSerializer.getXMLRequestBody(wsRequest, flags);
    },

    getSampleResponse : function (operationName, data, flags, returnRequest) {
        return this.getSoapMessage({
            wsOperation : operationName,
            data : data || {}
        }, isc.addProperties({
            spoofData:true, 
            generateResponse:!returnRequest
        }, flags));
    },

    getSampleRequest : function (operationName, data, flags) {
        return this.getSampleResponse(operationName, data, flags, true);
    },

    // get the soap style, "document" or "rpc", which can be specified per operation or for the 
    // service as a whole
    getSoapStyle : function (operationName) {
        return this.getOperation(operationName).soapStyle || this.soapStyle;
    },

    // ---------------------------------------------------------------------------------------

    


    //> @method webService.getInputDS()
    // Get a DataSource representing the input message to a web service operation.
    // <P>
    // This DataSource is suitable for use as
    // +link{DataBoundComponent.dataSource,form.dataSource} for a form that the user fills out
    // when providing inputs to call this web service operation.
    //
    // @param operationName (String) name of the web service operation whose inputs the
    //                               returned DataSource will represent
    // @return (DataSource) DataSource representing the input message of a web service
    //                      operation
    // @visibility xmlBinding
    // @example wsdlBinding
    //<
    getInputDS : function (operationName) {
        
        return this.getMessageSerializer(operationName);
    },

    getHeaderSchema : function (operationName, isInput) {
        var operation = this.getOperation(operationName),
            headers = isInput ? operation.inputHeaders : operation.outputHeaders;

        if (!headers) return null;
    
        var headerSchema = {};
        for (var i = 0; i < headers.length; i++) {
            var partName = headers[i].part,
                messageSchema = this.getSchema("message:"+headers[i].message);

            //this.logWarn("messageSchema: " + messageSchema);

            var partField = messageSchema.getPartField(partName);

            //this.logWarn("partField: " + this.echo(partField));
            // NOTE: simple type headers are legal, in which case we just return the field
            // definition
            headerSchema[partName] = this.getSchema(partField.type) || partField;
        }
        return headerSchema;
    },

    //> @method webService.getInputHeaderSchema()
    // Get the schema for each part of the SOAP header for the input message of a given
    // operation, as a mapping from part name to schema.  For example, given WSDL like:
    // <pre>
    //     &lt;soap:header part="SessionHeader" message="tns:HeaderMessage"/&gt;
    //     &lt;soap:header part="CallOptions" message="tns:HeaderMessage/&gt;
    // </pre>
    // The following schema would be returned:
    // <pre>
    //     { SessionHeader : <i>sessionHeaderPartSchema</i>,
    //       CallOptions : <i>callOptionsPartSchema</i> }
    // </pre>
    // The schema are instances of +link{DataSource} that can be inspected to discover the
    // elements and types that are legal in that header part, and can construct a valid SOAP
    // header part if +link{dataSource.xmlSerialize()} is invoked.
    // 
    // @param operationName (String) name of an operation from this web service
    // @return (Object) mapping from partName to schema
    // @visibility xmlBinding
    //<
    getInputHeaderSchema : function (operationName) {
        return this.getHeaderSchema(operationName, true);
    },

    //> @method webService.getOutputHeaderSchema()
    // Get the schema for each part of the SOAP header for the output message of a given
    // operation, as a mapping from part name to schema.  For example, given WSDL like:
    // <pre>
    //     &lt;soap:header part="SessionHeader"/&gt;
    //     &lt;soap:header part="CallOptions"/&gt;
    // </pre>
    // The following schema would be returned:
    // <pre>
    //     { SessionHeader : <i>sessionHeaderPartSchema</i>,
    //       CallOptions : <i>callOptionsPartSchema</i> }
    // </pre>
    // The schema are instances of +link{DataSource} that can be inspected to discover the
    // elements and types that are legal in that header part, and can construct a valid SOAP
    // header part if +link{dataSource.xmlSerialize()} is invoked.
    //
    // @param operationName (String) name of an operation from this web service
    // @return (Object) mapping from partName to schema
    // @visibility xmlBinding
    //<
    getOutputHeaderSchema : function (operationName) {
        return this.getHeaderSchema(operationName, false);
    },

    //> @method webService.getHeaderData()
    // Override this method to return data that should be serialized as SOAP headers for the
    // current operation, such as a sessionId.
    // <P>
    // Format of the returned data is the same as that documented for
    // +link{dsRequest.headerData}.
    // <P>
    // The object passed to this method will be a true DSRequest in the case of a DataSource
    // operation, or just an Object with a "data" property for web service operations
    // initiated by +link{webService.callOperation}.
    // <P>
    // If <code>headerData</code> is instead provided via either dsRequest.headerData or as
    // part of the <code>requestProperties</code> parameter to
    // +link{webService.callOperation,callOperation()}, this method will never be called.
    //
    // @param dsRequest (DSRequest)
    // @return (Object) data for SOAP headers
    //
    // @visibility xmlBinding
    //<
    getHeaderData : function (dsRequest) { },

    // create an XPath selector that will select objects of the targetSchema from the output
    // message of the specified web service operation.  
    // This is needed when we are interested in records of type "myObject", but which actually
    // have the tagName "records" in the result
    selectByType : function (xmlResponse, operationName, schemaName) {
        var operation = this.getOperation(operationName),
            outputMessage = this.getSchema("message:" + operation.outputMessage),
            targetSchema = this.getSchema(schemaName);

        if (targetSchema == null) {
            this.logWarn("selectByType: type '" + schemaName + 
                         "' not present in schema for message: " + operation.outputMessage);
            return null;
        }
    
        // find the tagName the target schema will appear as in the response message
        var tagLocation = outputMessage.findTagOfType(targetSchema.ID);

        if (tagLocation == null) {
            this.logWarn("selectByType: no tag of type '" + schemaName + 
                         "' could be found in message: " + operation.outputMessage);
            return null;
        }

        var tagLocationDS = tagLocation[0],
            tagName = tagLocation[1],
            parentSchema = tagLocation[2],
            parentSchemaTagName = tagLocation[3],
            field = tagLocationDS.getField(tagName);

        // if we couldn't find the tagName, use the type name as a fallback (this may indicate
        // a response message which is not completely specified in schema, eg xsd:any) 
        tagName = tagName || targetSchema.ID;

        // element definitions that were top-level in the WSDL file have a schemaNamespace
        // attribute and must be namespaced within the response message.  Non-top-level element
        // definitions must not be, unless the <schema> element declares
        // elementFormDefault="qualified", in which case everything must be qualified.
        
        var qualify = targetSchema.mustQualify,
            namespace = targetSchema.schemaNamespace,
            xpath = "//" + (qualify ? "ns0:" : "") + tagName;

        /*
        
        if (parentSchema && !isc.isA.WSDLMessage(parentSchema) && 
            targetSchema.getFieldNames().length == 1) 
        {
            qualify = parentSchema.mustQualify;
            namespace = parentSchema.schemaNamespace;
            xpath = "//" + (qualify ? "ns0:" : "") + parentSchemaTagName + "/*";
            this.logWarn("targetting parentSchema: " + parentSchema + 
                         " fieldName " + parentSchemaTagName + 
                         " namespace: " + namespace);
        }
        */

        // handle SOAP Array encoding, which specifies essentially that there is a container
        // tag whose children are of a specified type, which we represent as field.multiple
        if (field && field.multiple) xpath = xpath + "/*";
     
        var elements = isc.xml.selectNodes(xmlResponse, xpath, { ns0 : namespace });

        if (this.logIsDebugEnabled("xmlBinding")) {
            this.logDebug("selecting type: '" + targetSchema + 
                          "' within message '" + operation.outputMessage +
                          " via XPath: " + xpath + 
                          (qualify ? " using ns0: " + targetSchema.schemaNamespace : "") +
                          " got " + elements.length + " elements", "xmlBinding");
        }
        return elements;
    },

    // find the schema best suited for binding a grid or editor form to the results of a
    // web service operation.  Note this getInputDS() gives you the schema best suited for eg a
    // SearchForm.
    getDefaultOutputDS : function (operationName) {
        var schema = this.getResponseMessage(operationName);

        if (!schema) return null;
        // skip one level of pointless containment: a complexType with just one subelement,
        // which is also a complexType.
        var fieldNames = schema.getFieldNames();
        if (fieldNames.length == 1 && schema.fieldIsComplexType(fieldNames[0])) {
            return schema.getSchema(schema.getField(fieldNames[0]).type);
        }

        // improvements: find the first Array-like structure of elements containing simple type
        // fields.
        return schema;
    },

    //> @method webService.getFetchDS()
    // Retrieve a DataSource that provides read-only access to records returned by a web
    // service operation.
    // <P>
    // +link{interface:DataBoundComponent,DataBound Components} can be bound to the returned
    // DataSource, and the +link{ListGrid.fetchData(),fetchData()} method can be invoked
    // to retrieve data from the web service.
    // <P>
    // The returned DataSource is only capable of the "fetch" 
    // +link{group:dataSourceOperations,DataSource operation}, not "update", "add" or
    // "remove".  To create a DataSource capable of full read-write access, use
    // +link{DataSource.operationBindings} with the
    // +link{OperationBinding.wsOperation,wsOperation} property set to associate each
    // DataSource operation with a web service operation. 
    //
    // @param operationName (String) name of the web service operation to invoke to fetch
    //                               records
    // @param resultType    (String) tag or type name of the XML element to be returned as
    //                               DataSource records
    // @param [operationBindingProperties] (OperationBinding Properties) 
    // Optional additional properties for the operationType:"fetch" 
    // +link{OperationBinding,operationBinding} which this method automatically creates.  This
    // can be used to set properties such as +link{operationBinding.useFlatFields} or
    // +link{operationBinding.recordXPath}
    // 
    // @group webService
    // @visibility xmlBinding
    //<
    getFetchDS : function (operationName, resultType, operationBindingProperties) {

        // if no resultType is specified, pick the first non-trivial structure
        if (resultType == null) resultType = this.getDefaultOutputDS(operationName);
        resultType = isc.isA.Object(resultType) ? resultType.ID : resultType;

        if (resultType != null && this.getSchema(resultType) == null) {
            this.logWarn("getFetchDS: resultType: '" + resultType + 
                         "' not present in web service - missing XML files?");
        }
    
        // we subclass because we need operation-specific properties on this DataSource,
        // where it may be shared as the inputs or part of the inputs for another operation
        var fetchDS = isc.DS.create({
            // critical so this DS can find this WebService
            serviceNamespace : this.serviceNamespace,

            inheritsFrom : resultType,

            operationBindings : [
                isc.addProperties({
                    operationType: "fetch", 
                    wsOperation:operationName,
                    recordName:resultType
                }, operationBindingProperties)
            ]
        });

        return fetchDS;
    },

    //> @method webService.setLocation() [A]
    // Set location can be used when the actual URL where a service will be accessible isn't
    // known until runtime, or changes at runtime, hence can't be embedded in the service
    // definition.
    // <P>
    // With an operation parameter, <code>setLocation()</code> can be used to set a distinct
    // URL for each web service operation.  This is a development-time only feature that allows
    // XML flat files to be placed at various URLs on a server, to serve as spoofed responses
    // for each web service operation.
    //
    // @param location (URL) URL where web service can be contacted
    // @param [operation] (String) optional operation name to set the location for, for
    //                             debugging only
    // @group webService
    // @visibility xmlBinding
    //<
    setLocation : function (location, operation) { 
        if (operation) this.getBindingOperation(operation).dataURL = location;
        else this.dataURL = location; 
    }
});

isc.WebService.addClassMethods({
    // NOTE: we create one WebService per .wsdl file, however, two <wsdl:definition>s can
    // appear in two different files with different <wsdl:service> elements.  In this case
    // the different <wsdl:service>s can be distinguished by the @name attribute on the
    // <wsdl:service>.
    services : [],
    
    //> @classMethod WebService.get()
    // Retrieve a WebService object by the targetNamespace declared on the &lt;wsdl:definitions&gt;
    // element in the WSDL file from which the WebService was derived.
    // <P>
    // If you have more than one &lt;wsdl:service&gt; in the same target namespace, use
    // +link{classMethod:WebService.getByName} to disambiguate.
    //
    // @param serviceNamespace (String) uri from the "targetNamespace" attribute of the
    // &lt;wsdl:definitions&gt; element in the WSDL file
    // @return (WebService) the requested WebService, or null if not loaded
    //
    // @group webService
    // @visibility xmlBinding
    // @example wsdlBinding
    //<
    get : function (serviceNamespace) {
        return this.services.find("serviceNamespace", serviceNamespace);
    },

    //> @classMethod WebService.getByName()
    // Retrieve a WebService object by the name attribute declared on the &lt;wsdl:service&gt; tag.
    //
    // @param serviceName (String) name attribute from the &lt;wsdl:service&gt; tag
    // @param [serviceNamespace] (String) optional serviceNamespace if needed to disambiguate
    // @return (WebService) the requested WebService, or null if not loaded
    //
    // @group webService
    // @visibility xmlBinding
    //<
    getByName : function (serviceName, serviceNamespace) {
        if (serviceName == "") serviceName = null;
        if (serviceNamespace != null) {
            return this.services.find({name: serviceName, serviceNamespace: serviceNamespace});
        } else {
            return this.services.find("name", serviceName);
        }
    }
});

isc.WebService.getPrototype().toString = function () {
    return "[" + this.Class + " ns=" + this.echoLeaf(this.serviceNamespace) + 
        (this.location ? " location=" + isc.Page.getLastSegment(this.location) : "") + "]";
};

//> @groupDef wsdlBinding 
// SmartClient supports automated integration with WSDL-described web services.  This support
// consists of:
// <ul>
// <li> creation of SOAP XML messages from JavaScript application data, with automatic
// namespacing, and support for both "literal" and "encoded" SOAP messaging, and "document" and
// "rpc" WSDL-SOAP bindings
// <li> automatic decode of SOAP XML messages to JavaScript objects, with strong typing (eg an
// XML schema "date" type becomes a JavaScript Date object)
// <li> +link{XMLTools.loadXMLSchema,import of XML Schema} (contained in WSDL, or external),
// including translating XML Schema "restrictions" to ISC +link{Validator,Validators}
// </ul>
// <P>
// WSDL services can be contacted by using +link{XMLTools.loadWSDL()} or the
// +link{group:loadWSDLTag,&lt;isc:loadWSDL&gt; JSP tag} to load the service definition, then
// invoking methods on the resulting +link{WebService} object.
// <P>
// +link{WebService.callOperation()} can be used to manually invoke operations for
// custom processing (example using +explorerExample{wsdlOperation,public zipcode service},
// examples using .NET at
// +externalLink{/examples/databinding/dotNET/temperatureConvert.jsp,/examples/databinding/dotNET/temperatureConvert.jsp}).
// <P>
// <b>Fetch-only DataSource binding</b>
// <P>
// To bind a component to a web service operation, call
// <P>
// &nbsp;&nbsp;+link{WebService.getFetchDS(),WebService.getFetchDS(<i>operationName,elementName</i>)}
// <P>
// to obtain a DataSource which describes the structure of an XML element or XML Schema type
// named <i>elementName</i>, which appears in the response message for the operation named
// <i>operationName</i>.  A component bound to this DataSource will show fields corresponding
// to the structure of the chosen XML element or type, that is, one field per subelement or
// attribute.  +link{ListGrid.fetchData(),fetchData()} called on this DataSource (or on a
// component bound to it) will invoke the specified web service operation, using the
// +link{Criteria} passed to fetchData() to fill out the input message via
// +link{dataSource.xmlSerialize()}, and using the specified XML element from the response
// message as data.
// <P>
// Similarly, +link{WebService.getInputDS,WebService.getInputDS(<i>operationName</i>)} returns
// a DataSource suitable for binding to a form that a user will fill out to provide inputs to
// the specified web service operation.  Typical use is to let the user fill in the form, then
// pass the results of +link{dynamicForm.getValues(),form.getValues()} to
// +link{listGrid.fetchData(),fetchData()} as criteria.
// <P>
// If the input message to the web service has extra nesting, consider using
// the +link{operationBinding.useFlatFields,useFlatFields} property to simplify the inputs
// required for <code>fetchData()</code>, and/or to simplify form databinding via
// +link{dataBoundComponent.useFlatFields,component.useFlatFields}.
// <P>
// Note that the WSDL tab in the Developer Console can provide a clean, simplified view of any 
// WSDL file, making it easier to pick out the appropriate <code>operationName</code> and
// <code>elementName</code> parameters to pass to <code>getFetchDS()</code> and other
// +link{WebService} methods.
// <smartclient><P>
// Take a look at the +explorerExample{wsdlBinding,Google SOAP Search example} and the
// +externalLink{/examples/databinding/dotNET/customerSearch.jsp,.NET example}
// (/examples/databinding/dotNET/customerSearch.jsp).
// </smartclient>
// <P>
// <b>Binding with Customized Presentation</b>
// <P>
// Because XML Schema lacks key presentation metadata such as user-viewable titles, typically
// you cannot directly use the DataSources derived from XML Schema embedded in a WSDL file to
// drive visual component DataBinding in your final application.
// <P>
// You can create a DataSource that has custom fields <b>and</b> invokes a web
// service operation by setting +link{dataSource.serviceNamespace} to match the targetNamespace
// of the +link{WebService} (found on the <code>&lt;definitions&gt;</code> element from the
// WSDL file), and setting +link{operationBinding.wsOperation,wsOperation} to the name of the
// web service operation to invoke.  <code>fetchData()</code> called on such a DataSource will
// invoke the web service operation named by +link{operationBinding.wsOperation,wsOperation},
// just like a DataSource returned by +link{webService.getFetchDS()}.
// <P>
// In contrast to <code>getFetchDS()</code>, creating a DataSource in this way gives you the
// opportunity to:
// <ul>
// <li> declare arbitrary fields, with SmartClient presentation attributes such as titles and
// formatters
// <li> extract any data from the response message, via
// +link{operationBinding.recordXPath,operationBinding.recordXPath} and 
// +link{dataSourceField.valueXPath,field.valueXPath}, and transform it with
// +link{dataSource.transformResponse,transformResponse()}
// <li> transform the inbound data, if necessary, in order to add metadata such as
// +link{dsRequest.startRow} for paging, or a sessionId for a service requiring authentication
// </ul>
// These techniques are shown in the +explorerExample{wsdlBinding,Google SOAP Search example}.
// <P>
// <b>XML Schema Reuse</b>
// <P>
// Having loaded a WSDL file, all of the XML Schema definitions within the service definition
// get translated to SmartClient +link{DataSource,DataSources} and
// +link{SimpleType,SimpleTypes} via the rules described by +link{XMLTools.loadXMLSchema()},
// and are available to you via +link{webService.getSchema()} and +link{dataSourceField.type}. 
// <P>
// You can use the +link{dataSource.inheritsFrom} property to create DataSources that extend
// from XML schema definitions, then add presentation metadata not found in XML schema.
// <P>
// Even if you choose to declare all fields manually, you can leverage XML Schema
// &lt;simpleType&gt; definitions by setting +link{DataSourceField.type,field.type} to the name
// of an XML Schema simple type embedded in the WSDL file.
// <P>
// <b>Round Trip Binding [fetch -> edit -> save]</b>
// <P>
// For full read-write integration with a service that supports the basic
// +link{group:dataSourceOperations,DataSource operations} on persistent data, 
// +link{OperationBinding,OperationBindings} can be declared for each DataSource operation, and
// the +link{operationBinding.wsOperation,wsOperation} property can be used to to bind each
// +link{group:dataSourceOperations,DataSource operation} (fetch, update, add, remove) to a
// corresponding web service operation.
// <P>
// For example, this code accomplishes part of the binding to the 
// +externalLink{http://www.google.com/search?q=sforce+partner+wsdl,SalesForce partner web services}
// (additional code is required to handle authentication and other details):
// <pre>
// isc.DataSource.create({
//    serviceNamespace : "urn:partner.soap.sforce.com",
//    operationBindings : [
//        { operationType:"fetch", wsOperation:"query", recordName: "sObject" },
//        { operationType:"update", wsOperation:"update", recordName: "SaveResult" },
//        { operationType:"add", wsOperation:"create", recordName: "SaveResult" },
//        { operationType:"remove", wsOperation:"delete", recordName: "DeleteResult" }
//    ],
//    ...
// }); 
// </pre>
// NOTE: additional code is required to handle authentication and other details, see the
// complete code in smartclientSDK/examples/databinding/SalesForce.
// <P>
// In this usage, any DSRequest performed on this DataSource invokes the web service operation
// named by the <code>wsOperation</code> property on the corresponding operationBinding, and
// +link{dsRequest.data} is serialized via +link{dataSource.xmlSerialize()} to form the input
// message to send to the web service.  For example, if a +link{DynamicForm.saveData()} is
// invoked and triggers a DSRequest with operationType:"add", the DataSource above will invoke
// the "create" operation, and +link{DynamicForm.getValues(),form.values} will become
// +link{dsRequest.data} and be serialized to form the input message of the "create" web
// service operation.
// <P>
// Typical usage is:
// <ol>
// <li> declare a DataSource that represents the fields of the object as you want them
// represented in the UI.  This DataSource is considered the "entity DataSource".  It may
// extend from an XML Schema complex type via +link{dataSource.inheritsFrom}.
// <li> use +link{operationBinding,operationBindings} to configure the entity DataSource to
// call the appropriate web service operations for each DataSource operation, and extract
// results via
// +link{operationBinding.recordXPath,recordXPath}/+link{operationBinding.recordName,recordName}
// <li> bind components as follows:
// <ul>
// <li> bind +link{listGrid,grids} to the entity DataSource
// <li> bind +link{SearchForm,SearchForms} to the input message of the fetch operation
// (obtained via +link{WebService.getInputDS,webService.getInputDS("operationName")}.  This is
// done because search inputs are frequently unrelated to the structure of the objects being
// searched for
// <li> bind forms use for editing ("add" and "update" operations) to the entity DataSource
// </ul>
// <li> use
// +link{dataSource.transformRequest,transformRequest}/+link{dataSource.transformResponse,transformResponse}, 
// +link{operationBinding.useFlatFields} and +link{operationBinding.responseDataSchema} to
// handle inconsistencies between the WSDL operations and the data you want in the presentation
// layer.
// </ol>
// A complete example of binding to the SalesForce "partner" web service, including
// authentication via SOAP headers, saving data and cache sync, inline editing, validation
// error handling and data paging, can be found in [webroot]/examples/databinding/SalesForce.
// <P>
// This requires a SalesForce account.  SalesForce currently offers 
// +externalLink{http://www.google.com/search?hl=en&q=salesforce+developer+account,free developer accounts}.
// Please note: this application deals with <b>live data</b> and if you using inline editing
// <b>it will save to SalesForce</b>.
// <P>
// <b>Deployment</b>
// <P>
// For best performance, using the +link{group:loadWSDLTag,&lt;isc:loadWSDL&gt; JSP tag}
// is recommended, as it automatically caches a translated form of the WSDL file.  If you are
// not using the SmartClient server, the WSDL tab in the Developer Console allows you
// to save a .js file representing a WebService object, which can then be loaded and cached
// like a normal JavaScript file.
// <P>
// <B>Creating New WSDL Services</B>
// <P>
// If you have no existing WSDL web service but would like to use web services for integration,
// you can implement the "SmartClientOperations" web service described by the
// ${isc.DocUtils.externalLink(isc.Page.getIsomorphicDir()+"system/schema/SmartClientOperations.wsdl","WSDL file")} 
// included in the SDK.  This simple, 4 operation web service can support any number of
// DataSources.  In this case, you create your DataSources as client-side instances of
// +link{WSDataSource} (general client-side DataSource creation is described under
// +link{group:dataSourceDeclaration,Creating DataSources}).  To change the URL where ISC
// expects to find the SmartClientOperations web service, use +link{WebService.setLocation()}
// like so:<pre>
//      var service = isc.WebService.get("urn:operations.smartclient.com");
//      service.setLocation("myURL");
// </pre>
// <P>
// To implement a web service <b>starting from a WSDL file</b>:
// <ul>
// <li>In the .NET framework, you will use the Web Services Description Language Tool 
// +externalLink{http://www.google.com/search?q=wsdl.exe,(wsdl.exe)} to generate C# stubs that
// you will add business logic to
// <li>In Java, +externalLink{http://ws.apache.org/axis/,Apache Axis} can be used to generate
// Java stubs for implementing a web service
// <li>In Perl, the +externalLink{http://soaplite.com,SOAP:Lite} module can be used to
// implement web services without code generation
// <li>for PHP, the NuSoap module can likewise be used to implement web services without code
// generation
// </ul>
//
// @visibility xmlBinding
// @treeLocation Client Reference/Data Binding
// @title WSDL Binding
//<
