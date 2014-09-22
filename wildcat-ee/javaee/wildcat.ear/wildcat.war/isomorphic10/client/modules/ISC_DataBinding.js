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
// This script will load all of the Isomorhic SmartClient Application Framework libraries for you
//
// The idea is that in your app file you can just load the script "Isomorphic_SmartClient.js" which
// in a production situation would be all of the scripts jammed together into a single file.
//
// However, it's easier to work on the scripts as individual files, this file will load all of the
// scripts individually for you (with a speed penalty).
//		
var libs = 
	[
        "debug/version",  // check for module version mismatches

        "language/TextSettings",        // Settings related to ListGrid/DataSource text import/export
        "language/TextExportSettings",
        "language/TextImportSettings",

		"language/XMLSerialize",		// serialize an object as an xml string

        "language/XMLTools",

		"application/DataSource",		// representation of a server data source (databse table, etc)
        "application/WebService",       // WebService / WSDL 
		"application/RPCManager",	    // framework for editing/interacting with datasources
		"application/DMI",	            // Direct Method Invocation
		"application/ResultSet",        // data model for Lists loaded incrementally from a server
		"application/ResultTree",       // data model for Trees loaded incrementally from a server
		"application/ActionMethods",      // flow methods for databinding-capable components
        
        "application/MockDataSource",   // a client-only DataSource supporting data expressed
                                        // in a wiki-like text format

        "application/DataView",         // Self-contained application element, capable of loading 
                                        // its own config, components and data from webservices
        "application/ServiceOperation", // A webservice operation
        
        "application/Offline",          // Offline support

        
        
        "application/RulesEngine",      // Support for validation rules across multiple databound components
        
		"widgets/EditMode",             // support for an editing mode and editing container
        "widgets/editMode/EditProxy",
        "widgets/editMode/FormEditProxy",
        "widgets/editMode/GridEditProxy",
        "widgets/editMode/PortalEditProxy",
        "widgets/editMode/DrawingEditProxy",
		"widgets/PropertySheet",        // specialized, compact form

        "widgets/ListEditor",           // combination grid and form for editing a list of
                                        // records

		"widgets/ViewLoader",	    // manages components dynamically loaded from server
		"widgets/HTMLFlow",	        // a block of free-flowing HTML, with dynaload facilities

        "application/FacadeDataSource",
        "application/WSDataSource", // DataSource that works through ISC Web Service
        "application/RestDataSource", // Generic DataSource for arbitrary web servers (PHP / etc)

        // load schema needed to perform client-side XML to JS just for WSDL/XMLSchema
        // definitions produced by the schemaTranslator
        "schema/DataSource.ds.xml",
        "schema/DataSourceField.ds.xml",
        "schema/Validator.ds.xml",
        "schema/SimpleType.ds.xml",
        "schema/XSComplexType.ds.xml",
        "schema/XSElement.ds.xml",
        "schema/SchemaSet.ds.xml",
        "schema/WSDLMessage.ds.xml",
        "schema/WebService.ds.xml",
        "schema/WebServiceOperation.ds.xml",
        "schema/WSOperationHeader.ds.xml",


        "application/Operators",        // i18n naming object for AdvancedCriteria operators
		"widgets/form/FilterBuilder",	// advanced search form that allows the user to specify
                                        // individual fields and operators
        "widgets/MockupElement",       // placeholder for non-translatable widgets used by Reify
        "widgets/RuleEditor",          // widget for editing rules
        
                                        
        //>S3
        //"application/S3",
        //<S3
        
        "widgets/ScreenReader",
        
        "widgets/DataSourceEditor"
        
	];

//<STOP PARSING 

// The following code only executes if the script is being dynamically loaded.

// the following statement allows a page that is not in the standard location to take advantage of
// dynamically loaded scripts by explicitly setting the window.isomorphiDir variable itself.
if (! window.isomorphicDir) window.isomorphicDir = "../isomorphic/";

// dynamic loading
(function () {
    function loadLib(lib, hash) {
        if (hash == null) hash = "";
        document.write("<"+"script src='" + window.isomorphicDir + "client/" + lib + ".js" + hash + "' type='text/javascript' charset='UTF-8'><"+"/script>");
    }

    loadLib("language/startDefiningFramework", "#module=DataBinding");
    for (var i = 0, l = libs.length; i < l; ++i) {
        if (!libs[i]) continue;
        if (window.UNSUPPORTED_BROWSER_DETECTED == true) break;
        loadLib(libs[i]);
    }
    loadLib("language/stopDefiningFramework", "#module=DataBinding");
})();
