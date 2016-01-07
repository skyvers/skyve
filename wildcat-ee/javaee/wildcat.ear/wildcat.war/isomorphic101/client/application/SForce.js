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
//> @class SForce
// WebService object representing the SForce (aka AppForce) web service.
//
// @visibility sforce
//<

isc.SForce = isc.WebService.get("urn:partner.soap.sforce.com");

if (isc.SForce) {

isc.SForce.addMethods({
    getHeaderData : function (dsRequest) {
        var headerData = {};
        if (dsRequest.operationType == "fetch") headerData.QueryOptions = { batchSize : 75 };
        if (this.sessionId != null) headerData.SessionHeader = { sessionId:this.sessionId };
        return headerData;
    },
    
    //> @classMethod SForce.login()
    // Log in to SForce service.
    // @param userName (String) username to use
    // @param password (String) password to use
    // @param callback (Callback) callback to fire on completion.  Receives the loginData block
    //                            returned by the SForce service, as an Object, or boolean
    //                            false if login failed
    // @visibility sforce
    //<
    login : function (userName, password, callback) {
        this.callOperation("login", 
                           { username: userName,
                             password: password },
                           "//default:result",
                           { target : this, methodName : "loginReply" },
                           { willHandleError:true, _loginCallback:callback,
                             showPrompt:true, prompt:"Logging into SalesForce.." });
    },
    loginReply : function (data, xmlDoc, rpcResponse, wsRequest) {
        if (rpcResponse.status < 0) {
            return this.fireCallback(wsRequest._loginCallback, "loginData", [false]);
        }
        
        var loginData = data[0];
        this.logDebug("login data: " + this.echo(loginData));

        // change the URL to point to the dynamically returned server URL
        this.dataURL = loginData.serverUrl;

        // store the sessionId on the service - this causes all operations invoked through this
        // web service to send a sessionId header
        this.sessionId = loginData.sessionId;
        this.logInfo("got sessionID: " + this.sessionId);

        this.fireCallback(wsRequest._loginCallback, "loginData", [loginData]);
    },

    ensureLoggedIn : function (callback, dismissable, properties) {
        // NOTE: doesn't handle session timeout
        if (this.sessionId) return this.fireCallback(callback);

        var service = this;
        isc.showLoginDialog(function (credentials, dialogCallback) {
                              if (credentials == null) return; // dismissed
                              service.login(credentials.username, credentials.password, 
                                            function (loginData) {
                                                dialogCallback(loginData);
                                                if (loginData) isc.Class.fireCallback(callback);
                                            })
                         }, isc.addProperties({ 
                                title:"Please log in to SalesForce",
                                dismissable:dismissable}, 
                            properties)
                           );
    },
    
    //> @classMethod SForce.getEntityList()
    // Get the list of SForce entities accessible to currently logged in user.
    // @param callback (Callback) callback to fire on completion.  Receives the list of
    //                            entities as an Array of Strings as the variable "list"
    // @visibility sforce
    //<
    getEntityList : function (callback) {
        this._describeGlobalCallback = callback;
        this.callOperation("describeGlobal",
                           null,
                           "//default:types",
                           { target : this, methodName : "describeGlobalReply" });
                           
    },
    describeGlobalReply : function (data) {
        // NOTE: this callback exists only to rename "data" to the more specific "list"
        this.fireCallback(this._describeGlobalCallback, "list", [data]);
    },

    //> @classMethod SForce.getEntity()
    // Get a DataSource describing an SForce Object type (SObject).  The returned DataSource is
    // capable of all DataSource operations (fetch, add, update, remove).
    //
    // @param callback (Callback) callback to fire on completion.  Receives the DataSource
    //                            derived as the variable "schema"
    // @visibility sforce
    //<
    getEntity : function (objectType, callback) {
        var service = this;
        this.callOperation("describeSObjects",
                           { sObjectType : objectType },
                           null,
                           function (data) {
                               service.describeObjectReply(data, objectType, callback);
                           });
    },
    describeObjectReply : function (data, objectType, callback) {
        var sfSchema = data.result,
            sfFields = sfSchema.fields;
 
        var ds = this.convertSchema(sfSchema, objectType);
        if (this.logIsDebugEnabled()) {
            this.logDebug("converted schema: " + this.echoAll(ds.getFields()));
        }

        
        ds.sfFields = sfSchema.fields;

        this.fireCallback(callback, "schema", [ds]);
    },

    // convert SalesForce schema (which is returned in a custom format that doesn't match XML
    // Schema) to SmartClient DataSources
    detailFields : [ "Id", "Type", "ParentId", 
                     "LastModifiedDate", "LastModifiedById", "LastActivityDate", 
                     "CreatedDate", "CreatedById" ],
    hiddenFields : [ "SystemModstamp" ],
    convertSchema : function (sfSchema, ID) {
        var sfFields = sfSchema.fields,
            dsFields = [];
        // top level: 
        // - childRelationships
        //   - childSObject
        //   - field
        //   - cascadeDelete: boolean
        // - createable, custom, deleteable, activateable : boolean
        for (var i = 0; i < sfFields.length; i++) {
            var sfField = sfFields[i],
                dsField = {};
            // custom, createable, defaultedOnCreate, filterable, 
            dsField.name = sfField.name;

            if (this.detailFields.contains(dsField.name)) dsField.detail = true;
            if (this.hiddenFields.contains(dsField.name)) dsField.hidden = true;

            // type, soapType
            // type is sforce type, a mix of derived types (currency, url) and
            // editor types (picklist, textArea).
            // type:date and datetime exist, both mapping to soap dateTime
            var type = sfField.soapType;
            if (type.contains(":")) type = type.substring(type.indexOf(":") + 1);
            dsField.type = type;

            dsField.title = sfField.label;

            dsField.canEdit = sfField.updateable;

            
            if (sfField.type == "id") dsField.primaryKey = true;

            // bytelength? consistently 3x length for user-visible text, same as length for ids
            // length/byteLength is sent as 0 for numeric types
            if (sfField.length != 0) dsField.length = sfField.length;

            // digits, precision, scale
            // currency has precision 18

            // pickListValues -> valueMap
            // restrictedPicklist

            dsFields.add(dsField);
        }

/*
    <childRelationships>
     <cascadeDelete>false</cascadeDelete>
     <childSObject>Account</childSObject>
     <field>ParentId</field>
    </childRelationships>
*/
        var relations = sfSchema.childRelationships,
            childRelations = [];
        if (relations) {
            for (var i = 0; i < relations.length; i++) {
                var sfRelation = relations[i];
                childRelations.add({
                    dsName : sfRelation.childSObject,
                    fieldName : sfRelation.field
                });
            }
        }
        //this.logWarn(this.echoFull(sfSchema));
        return isc.SFDataSource.create({
            sfName:sfSchema.name,
            ID:ID,
            childRelations : childRelations,
            fields : dsFields
        });
    },
 
 
    //> @classMethod SForce.deploySControl()
    // Create and deploy an SForce Object (SObject) from the javascript code passed in.
    // @param name (string) Name for the SForce object to be created
    // @param code (string) Javascript code to create the control object.
    //
    // @visibility sforce
    //<
    deploySControl : function (name, code) {
    
        // If necessary get the SObject dataSource (asynchronous), and re-run this method
        if (this.SControlDS == null) {
            this.getEntity(
                "SControl", 
                function (ds) {
                    isc.SForce.SControlDS = ds;
                    isc.SForce.deploySControl(name, code);
                }
            );
            return;
        }
        var ds = this.SControlDS;
        
        ds.fetchData(
            {Name:name},
            function (dsResponse, data) {
                // name / code available due to JS closure behavior
                isc.SForce.installSControl(name, code, data, ds);
            }
        );
          
    },
    // installSControl()
    // Uses standard dataSource APIs to add or modify an SControl based on the code passed in
    installSControl : function (name, code, sControls, sControlDS) { 

        // Code passed in is raw JS - wrap in HTML header/ footer
        var html = this.getSControlHTML(code);
        
        if (sControls.length > 0) {
            sControlDS.updateData({
                Id : sControls[0].Id,
                HTMLWrapper : html
            });
        } else {
            sControlDS.addData({
                Name : name,
                HTMLWrapper : html
            });
        }
    },
    
    // getSControlHTML() 
    // Helper method to convert arbitrary Javascript code to SmartClient-enabled HTML
    getSControlHTML : function (code) {
        if (this.htmlPrefix == null) {
            var dir = this.controlIsomorphicDir;
            this.htmlPrefix = [
                "<HTML>\r<BODY>\r<SCRIPT>window.isomorphicDir = '", dir, "'</SCRIPT>\r",
                "<SCRIPT src='", dir, "system/modules/ISC_Core.js'></SCRIPT>\r",
                "<SCRIPT src='", dir, "system/modules/ISC_Foundation.js'></SCRIPT>\r",
                "<SCRIPT src='", dir, "system/modules/ISC_Containers.js'></SCRIPT>\r",
                "<SCRIPT src='", dir, "system/modules/ISC_Grids.js'></SCRIPT>\r",
                "<SCRIPT src='", dir, "system/modules/ISC_Forms.js'></SCRIPT>\r",
                "<SCRIPT src='", dir, "system/modules/ISC_DataBinding.js'></SCRIPT>\r",
                "<SCRIPT src='", dir, "system/modules/ISC_SalesForce.js'></SCRIPT>\r",
                "<SCRIPT src='", dir, "system/modules/ISC_Kapow.js'></SCRIPT>\r",
                "<SCRIPT src='", dir, "skins/", this.controlSkin, "/load_skin.js'></SCRIPT>\r",
                "<SCRIPT>\r",
                '   var service = isc.WebService.get("urn:partner.soap.sforce.com");\r',
                '   service.sessionId = "{!User_Session_ID}";\r',
                '   service.dataURL = "{!API_Partner_Server_URL_60}";\r\r'
            ].join("");

        }
        return this.htmlPrefix + code + '</SCRIPT>\r</BODY></HTML>';
    }

    
});

// use a custom ResultSet subclass to hold onto sForce's queryLocator, which is required when
// making paged requests 
isc.defineClass("SFResultSet", "ResultSet");

isc.SFResultSet.addMethods({
    transformData : function (newData, dsResponse) {
        // place the queryLocator into the context to cause it to be sent with the next
        // dsRequest
        this.context = this.context || {};
        this.context.queryLocator = dsResponse.queryLocator;
    },
    setCriteria : function (newCriteria) {
        var changed = this.Super("setCriteria", arguments);
        if (changed) {
            //this.logWarn("criteria changed");
            this.context = this.context || {};
            this.context.queryLocator = null;
        }
    }
});

isc.defineClass("SFDataSource", "DataSource");

isc.SFDataSource.addMethods({
    serviceNamespace : "urn:partner.soap.sforce.com",
    operationBindings : [
        { operationType:"fetch", wsOperation:"query", recordXPath: "//schema:records" },
        { operationType:"fetch", operationId:"queryMore", wsOperation:"queryMore", 
          recordXPath: "//schema:records" },
        { operationType:"update", wsOperation:"update", recordName: "SaveResult" },
        { operationType:"add", wsOperation:"create", recordName: "SaveResult" },
        { operationType:"remove", wsOperation:"delete", recordName: "DeleteResult" }
    ],

    resultSetClass : "SFResultSet",
    transformRequest : function (dsRequest) {
        var data = dsRequest.data;
        if (!isc.isAn.Array(data)) data = [data];

        // for removal, SForce wants only the ids of the objects, whereas because we support
        // multicolumn primary keys, we normally send an object per deleted object
        if (dsRequest.operationType == "remove") {
            return { ids : data.getProperty("Id") };
        }

        if (dsRequest.operationType != "fetch") {
            // SForce allows multiple objects of different types to be saved at once and
            // expects "type" on each
            data.setProperty("type", this.sfName || this.ID);
            return { sObjects : data };
        }

        // handling query vs queryMore:
        if (dsRequest.queryLocator) {
            // causes the "queryMore" operation to be used instead of "query"
            dsRequest.operationId = "queryMore";
            // pass the locator returned with the original "query" message in lieue of a query
            // string
            return { queryLocator : dsRequest.queryLocator };
        }
            
        // fetch operation: form SForce SOQL query string
        var criteria = dsRequest.data,
            queryString = "select " + this.getFieldNames().join(",") + " " +
                          "from " + (this.sfName || this.ID);
        if (criteria != null && !isc.isAn.emptyObject(criteria)) {
            queryString += " where ";
            for (var fieldName in criteria) {
                queryString += fieldName + "='" + criteria[fieldName] + "' ";
            }
        }
        return { queryString : queryString };
    },

    transformResponse : function (dsResponse, dsRequest, xmlData) {
        var operationType = dsRequest.operationType;
    
        if (operationType != "fetch") {
            // check for failure on saves and set appropriate status code
            var success = xmlData.selectString("//default:success");
            if (success != "true") {
                dsResponse.errors = this.convertValidationErrors(xmlData);
                this.logWarn("save failed, errors are: " + this.echo(dsResponse.errors));
                dsResponse.status = -1;
                return dsResponse;
            }

            //this.logWarn("original response data: " + this.echoAll(dsResponse.data));

            // NOTE: supporting singular add/update/remove only for now

            if (operationType != "remove") {
                // for add/update, since SForce backend doesn't send back updated data, use the
                // request data (NOTE this assumes no server formatting and no important
                // server-added values!)
                var updateData = isc.addProperties({}, 
                                                   dsRequest.oldValues,
                                                   dsRequest.data.sObjects[0]);

                // on an "add", pick up the id returned by the SForce backend
                if (operationType == "add") {
                    updateData.Id = dsResponse.data[0].id;
                }
                dsResponse.data = updateData;
            } else {
                // for remove, turn the returned id into a PK object
                // NOTE: deletion results aren't sObject typed; they return the deleted id as
                // the element "id" instead of "Id" as all SObjects use.
                var id = dsResponse.data[0].id;
                dsResponse.data = { Id : id };
                this.logWarn("cache sync data on remove: " + this.echo(dsResponse.data));
            }

            return dsResponse;
        }

        // paging: queryLocator is needed for 2nd+ requests with same criteria
        var queryLocator = xmlData.selectString("//default:queryLocator");
        if (queryLocator != null && !isc.isAn.emptyString(queryLocator)) {
            dsResponse.queryLocator = queryLocator;
        }
        // size = total number of records
        dsResponse.totalRows = xmlData.selectNumber("//default:size");

        // workaround: there seems to be a bonus ID element returned with entities
        //this.logWarn("removing duplicate Id field");
        var data = dsResponse.data;
        for (var i = 0; i < data.length; i++) {
            if (isc.isAn.Array(data[i].Id)) data[i].Id = data[i].Id[0];
        }

        return dsResponse;
    },

    // log in automatically when data is first fetched if autoLogin:true is set
    autoLogin : true,
    fetchData : function (criteria, callback, requestProperties, loggedIn) {
        var ds = this;
        if (this.autoLogin && !loggedIn) {
            isc.SForce.ensureLoggedIn(function () {
                ds.fetchData(criteria, callback, requestProperties, true);
            });
            return;
        }
        return this.Super("fetchData", arguments);
    },

    convertValidationErrors : function (xmlData) {
        var errors = xmlData.selectNodes("//default:errors"),
            iscErrors = {};
        for (var i = 0; i < errors.length; i++) {
            // convert each <errors> element to JS
            var error = errors[i];
            error = isc.xml.toJS(error);

            // transform to ISC error format ( { field : message } )
            iscErrors[error.fields] = error.message;
        }
        return iscErrors;
    }
});

}
