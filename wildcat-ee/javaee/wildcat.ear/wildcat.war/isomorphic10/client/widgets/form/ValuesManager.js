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

 


//>	@class	ValuesManager
//
// The ValuesManager manages data from multiple member forms.
// <P>
// If a single logical form needs to be separated into multiple DynamicForm instances for
// Layout purposes (for example, spanning one logical form across multiple Tabs), a
// ValuesManager can be used to make the forms act as one logical form, supporting all
// value-related APIs otherwise called on DynamicForm directly.
// <P>
// A ValuesManager has no visual representation - it is strictly a logical entity, and the
// member forms provide the user interface.  You can initialize a ValuesManager with a set of
// member forms (by setting +link{ValuesManager.members} at init) or add and remove member
// forms dynamically.
// <P>
// Calling +link{ValuesManager.setValues()} on a ValuesManager will automatically route new
// field values to whichever member form is showing an editor for that field.  Likewise,
// calling +link{ValuesManager.validate()} will validate all member forms, and
// +link{ValuesManager.saveData()} will initiate a save operation which aggregates values from
// all member forms.
// <P>
// Like a DynamicForm, a ValuesManager can be databound by setting
// +link{valuesManager.dataSource}.  In this case all member forms must also be bound to the
// same DataSource.
// <P>
// In general, when working with a ValuesManager and its member forms, call APIs on the
// ValuesManager whenever you are dealing with values that span multiple forms, and only call
// APIs on member forms that are specific to that form or its fields.
// <P>
// Note that, just as a DynamicForm can track values that are not shown in any FormItem, a
// ValuesManager may track values for which there is no FormItem in any member form.  However,
// when using a ValuesManager these extra values are only allowed on the ValuesManager itself.
// Member forms will not track values for which they do not have FormItems.
//
// @treeLocation Client Reference/Forms
// @visibility external
// @example formSplitting
//<
isc.ClassFactory.defineClass("ValuesManager");

isc.ValuesManager.addClassMethods({
    //> @classMethod valuesManager.getById()
    // Retrieve a ValuesManager by it's global +link{Canvas.ID,ID}.
    // @param ID (String) global ID of the ValuesManager
    // @return (ValuesManager) the ValuesManager, or null if not found
    // @visibility external
    //<
    getById : function (sId) {
        var vm = window[sId] || null;
        return vm ? (isc.isA.ValuesManager(vm) ? vm : null) : null;
    }
});

isc.ValuesManager.addProperties({

    //> @attr valuesManager.dataSource  (DataSource | String : null : [IRWA])
    // Specifies a dataSource for this valuesManager.  This dataSource will then be used for
    // validation and client-server flow methods.  Can be specified as a dataSource object or
    // an identifier for the dataSource.<br>
    // Note that member forms should have the same dataSource applied to them to allow their
    // items to inherit properties from the DataSource fields.
    // @visibility external
    // @see valuesManager.setDataSource()
    // @see valuesManager.getDataSource()
    //<
    //dataSource: null,

    //> @attr valuesManager.addOperation
    // +link{DSRequest.operationId,operationId} to use when performing add operations.
    // @include DataBoundComponent.addOperation
    //<

    //> @attr valuesManager.updateOperation
    // +link{DSRequest.operationId,operationId} to use when performing update operations.
    // @include DataBoundComponent.updateOperation
    //<

    //> @attr valuesManager.removeOperation
    // +link{DSRequest.operationId,operationId} to use when performing remove operations.
    // @include DataBoundComponent.removeOperation
    //<

    //> @attr valuesManager.validateOperation
    // +link{DSRequest.operationId,operationId} to use when performing validate operations.
    // @include DataBoundComponent.validateOperation
    //<

    //> @attr valuesManager.members (Array of DynamicForm : null : [IRW])
    // The set of member components for this valuesManager.  These can be specified at init time
    // via the <code>members</code> property, or updated at runtime via <code>addMember()</code>
    // and <code>removeMember()</code>.<br>
    // Note: Alternatively a DataBoundComponent can be initialized as a member of a valuesManager
    // by setting the <code>valuesManager</code> property of the component to a pre-defined 
    // valuesManager instance, or by calling <code>setValuesManager</code> on the component.
    // @visibility external
    // @see valuesManager.addMember()
    // @see valuesManager.removeMember()
    // @see Canvas.setValuesManager()
    //<
    //members : null,    
    
    //>	@attr valuesManager.unknownErrorMessage	(string : null : [IRW])
    // The error message for a failed validator that does not specify its own errorMessage.
    // <P>
    // If unset this value will be derived from the default 
    // +link{dataBoundComponent.unknownErrorMessage} when the valuesManager is initialized.
    //<
	unknownErrorMessage : null
    
    //> @attr valuesManager.disableValidation   (boolean : null : [IRWA])
    // @include DynamicForm.disableValidation
    //<
    
    //> @attr valuesManager.autoSynchronize   (boolean : null : [IRWA])
    // If explicitly set to false, prevents the ValuesManager from automatically propagating
    // data value changes to its members.  You can manually synchronize member data values 
    // at any time with a call to +link{synchronizeMembers}.
    // @visibility external
    //<

});

//!>Deferred
isc.ValuesManager.addMethods({
    // Allow a V.M to be initialized with member form(s)
    init : function () {
        // get a global ID so we can be called in the global scope
        this.ns.ClassFactory.addGlobalID(this);
        
        if (this.unknownErrorMessage == null) {
            this.unknownErrorMessage = isc.Canvas.getPrototype().unknownErrorMessage;
        }
        
        if (this.dataSource) this.bindToDataSource(this.dataSource);

        // Initialize this.values [and ensure it's a new object, so it can't be manipulated
        // externally]
        if (this.values == null) this.values = {};
        isc.DynamicForm._duplicateValues(this, this.values, {});
        
        // Set up values based on members / init values.
        if (this.members != null) {
            
            var members = this.members;
            this.members = null;
            if (!isc.isAn.Array(members)) members = [members];
            for (var i = 0; i < members.length; i++) {
                this.addMember(members[i]);
            }
        }
        // remember the current values for resetting
        this.rememberValues();
    },
    
    // on destroy
    // - disconnect from member forms (Don't destroy - they may want to be re-used in a 
    //   different VM)
    // - clear global ID
    destroy : function () {
        var members = this.members;
        if (members) {
            // iterate backwards so the changing length of the members array doesn't mess up
            // our loop
            for (var i = members.length-1; i >= 0; i--) {
                this.removeMember(members[i]);
            }
        }
        // clear the global ID
        window[this.getID()] = null;
        this.Super("destroy", arguments);
    },
    
    // This is a VM-specific override of the method specified on EditorActionMethods.  It is 
    // necessary because forms only edit flat structures (even if dataPaths mean that the 
    // values in those flat structures are derived from arbitrary places in a complex nested
    // structure), whereas ValuesManagers have to cope with any kind of data structure
    _saveDataReply : function (request, response, data) {
        if (!this.suppressServerDataSync && response && response.status >= 0 && data != null) {
            if (isc.isAn.Array(data)) data = data[0];
            if (request.data) request.data = isc.shallowClone(request.data);
            
            this.setValues(data);
            
            // If this was an add operation, drop the currently specified saveOperationType now
            // if the response included primary key data for the newly added record we're now
            // updating an existing record. We already have logic to catch this case in
            // getSaveOperationType().
            if (this.saveOperationType == "add") delete this.saveOperationType;
            // Also drop the saveOperatonType on member forms. This ensures
            // isNewRecord etc return the expected values on the DynamicForm.
            
            var memberForms = (this.members ? this.members.duplicate() : []);
            for (var i = 0; i < memberForms.length; i++) {
                var form = memberForms[i];
                if (!isc.isA.DynamicForm(form)) continue;
                form.saveOperationType = null;
            }
        }
        
        this._callbackState = {
            request: request,
            response: response,
            data: data
        };
        this.formSavedComplete();


    },
    
    // given a member with dataArity:"multiple", 
    _updateMultipleMemberValue : function (index, field, value, member) {
        field = (field != null) ? this._combineDataPaths(index,field) : index;
        return this._updateValue(field, value, member);
    },
    
    // _updateValue and _clearValue() -- called by member components to notify us of field
    // value changes
    _updateValue : function (fieldName, value, member) {
        if (this._synchronizingMembers) return;
        
        var isIndex = isc.isA.Number(fieldName);
           
        // warn on value with no associated items in dynamicForms
        if (!isIndex && isc.isA.DynamicForm(member) && member.getItem(fieldName) == null) {
            this._itemlessValueWarning(member, fieldName);
            return;
        }
        
        var isDataPath;
        // if the component has a dataPath, prepend it to the fieldName/dataPath passed in so
        // we store values hierarchically (unless the field's dataPath is fully-qualified)
        var dataPath = member.getFullDataPath();
        var field;
        if (member && member.getField) {
            field = member.getField(fieldName);
        }
        
        if (!isIndex) {
            if (fieldName != null && !isc.isA.String(fieldName)) {
                
                fieldName = fieldName.dataPath || fieldName.name;
            }
            if (fieldName) {
                // handle an item having an "absolute" dataPath
                if (dataPath == null || fieldName.startsWith(isc.Canvas._$slash)) {
                    dataPath = fieldName;
                } else {
                    dataPath = this._combineDataPaths(dataPath, fieldName);
                }
            }
            // If we didn't pick up a field from the item, grab it from our DS
            if (field == null) {
                var ds = this.getDataSource();
                if (ds) field = ds.getFieldForDataPath(dataPath);
            }
        } else { // We were passed a record index, not a field name
            if (!dataPath) {
                dataPath = fieldName;
            } else if (!dataPath.endsWith(isc.Canvas._$slash)) {
                dataPath += isc.Canvas._$slash + fieldName;
            } else {
                dataPath += fieldName;
            }
        }
        
        isc.DynamicForm._saveFieldValue(dataPath, field, value, this.values, member, true);
        var isDataPath = dataPath.contains(isc.Canvas._$slash);
        if (isDataPath && this.autoSynchronize !== false) {
            var elements = dataPath.split(isc.Canvas._$slash);
            if (parseInt(elements[elements.length-1]) == elements[elements.length-1]) {
                // The last element of the dataPath is an index, which implies that the updating
                // member is a selectionComponent.  Therefore, we refresh every member of the 
                // VM that has "member" as a selectionComponent
                this.synchronizeMembers(member);
            } else {
                var fields = this.getFieldsForDataPath(dataPath || fieldName);
                this._synchronizingMembers = true;
                for (var i = 0; i < fields.length; i++) {
                    if (fields[i].form == member) continue;
                    fields[i].saveValue(value);
                }
                delete this._synchronizingMembers;
            }
        }
    },
    
    //> @method ValuesManager.synchronizeMembers() 
    //   Update all of this ValuesManager's members to reflect the current values held by the
    //   ValuesManager.  It is not normally necesary to manually synchronize members, but you
    //   will need to do so if you switch off +link{autoSynchronize,automatic synchronization}.
    // @visibility external
    //<
    // Undocumented selComp param allows _updateValue to use this method for its own purposes 
    // (ie, refreshing just those members that have a particular selectionComponent)
    synchronizeMembers : function (selComp) {
        if (!this.members) return;
        this._synchronizingMembers = true;
        for (var i = 0; i < this.members.length; i++) {
            if (!selComp || this.members[i].selectionComponent == selComp) {
                this._setMemberValues(this.members[i]);
            }
        }
        delete this._synchronizingMembers;
    },
    
    _combineDataPaths : function (baseDP, fieldDP) {
        if (isc.isAn.Object(fieldDP)) {
            fieldDP = fieldDP.dataPath || fieldDP.name;
        }
        return isc.DynamicForm._combineDataPaths(baseDP, fieldDP);
    },
    // fieldName may be an array of field IDs
    _itemlessValueWarning : function (member, fieldName) {
        this.logWarn("Member Form: " + member +
                 " has explicitly specified value for field[s] '" + fieldName + "', but has" +
                 " no item associated with this fieldName. Ignoring this value. " +
                 "Values may be set for fields with no associated form item directly " + 
                 "on the valuesManager via valuesManager.setValues(), but not on " +
                 "member forms. See ValuesManager documentation for more info.");
    },

    _clearValue : function (field, form) {
        
        
        var dataPath = form.getFullDataPath();
        if (dataPath) field = this._combineDataPaths(dataPath, field);
        return isc.DynamicForm._clearFieldValue(field, this.values);
    },
    
    // ----------------------------------------------------------------------------------------
    // Databound functionality
    // ----------------------------------------------------------------------------------------
    
    //> @method ValuesManager.bindToDataSource() ([A])
    //   Associate this ValuesManager with a DataSource.  Allows the developer to inherit 
    //   properties from the DataSource fields to be applied to these values, such as validators,
    //   and to call data flow methods using this ValuesManager's data.
    // @param (dataSource)  Datasource object, or identifier
    // @visibility internal
    //<
    // For the public version of this method use 'setDataSource'
    bindToDataSource : function (ds) {

        
        if (!isc.isA.DataSource(ds)) ds = isc.DataSource.getDataSource(ds);
        if (ds != null) this.dataSource = ds;
    },
  
    //>@method  valuesManager.setDataSource() (A)
    // Specifies a dataSource for this valuesManager.  This dataSource will then be used for
    // validation and client-server flow methods.
    // @visibility external
    // @param dataSource (string | DataSource)  Datasource object or identifier to bind to.
    //<
    setDataSource : function (dataSource, fields) {
        // we don't use 'fields'
        this.bindToDataSource(dataSource);
    },
    
    //>@method  valuesManager.getDataSource() (A)
    // Returns the dataSource for this valuesManager.  Will return null if this is not a 
    // data-bound valuesManager instance.
    // @visibility external
    // @return (DataSource)  Datasource object for this valuesManager.
    //<
    getDataSource : function () {
        if (isc.isA.String(this.dataSource)) {
            if (this.serviceNamespace || this.serviceName) {
                this.dataSource = this.lookupSchema();
            } else {
                var ds = isc.DS.get(this.dataSource);
                if (ds != null) return ds;
        
                // support "dataSource" being specified as the name of a global, and if so, assign
                // that to this.dataSource
                ds = this.getWindow()[this.dataSource];
                if (ds && isc.isA.DataSource(ds)) return (this.dataSource = ds);
            }
        }
        return this.dataSource;
    },
    
    lookupSchema : function () {
        // see if we have a WebService instance with this serviceName / serviceNamespace
        var service;
        if (this.serviceName) service = isc.WebService.getByName(this.serviceName, this.serviceNamespace);
        else service = isc.WebService.get(this.serviceNamespace);
    
        if ((this.serviceNamespace || this.serviceName) && service == null) {
            this.logWarn("Could not find WebService definition: " +
                         (this.serviceName ? "serviceName: " + this.serviceName : "") +
                         (this.serviceNamespace ? "   serviceNamespace: " + this.serviceNamespace : "")
                         + this.getStackTrace());
        }
        
        // If this.dataSource is not a String, we shouldn't have ended up here
        if (!isc.isA.String(this.dataSource)) {
            this.logWarn("this.dataSource was not a String in lookupSchema");
            return;
        }
        
        if (service) return service.getSchema(this.dataSource);
    },
    
    // support retrieving a pointer to a field object defined in a dataSource by fieldID / dataPath
    
    getDataSourceField : function (fieldID) {
        var ds = this.getDataSource();
        if (!ds || !fieldID) return null;
        
        fieldID = fieldID.trim("/");
        var dataSource = this.getDataSource(),
            segments = fieldID.split("/"),
            field;
        for (var i = 0; i < segments.length; i++) {
            if (isc.isAn.emptyString(segments[i])) continue;
            var fieldId = segments[i];
            field = dataSource.getField(fieldId);
            dataSource = field ? isc.DataSource.getDataSource(field.type) : dataSource;
        }
        return field;
    },
    
    
    //>@method valuesManager.getItems()
    // Retrieves all form items contained within this valuesManager's member forms
    // @return (array of FormItems) form items present in this valuesManager
    //<
    getItems : function () {
        if (!this.members) return;
        var items = [];
        for (var i = 0; i < this.members.length; i++) {
            var form = this.members[i];
            if (!form.getItems) continue;
            items.addList(form.getItems());
        }
        return items;
    },
    // getFields() synonym of getItems
    getFields : function () {
        return this.getItems();
    },
    
    // Getting pointers to actual items
    //>@method ValuesManager.getItem()
    // Retrieve a +link{FormItem} from this ValuesManager.
    // <P>
    // Takes a field +link{formItem.name,name} or +link{type:DataPath}, and searches through the
    // members of this valuesManager for an editor for that field. If found the appropriate
    // formItem will be returned. If the "retrieveAll" parameter is true, this method will return 
    // all FormItems that are bound to the supplied name or dataPath (a dataPath can be bound
    // to more than one FormItem, as long as those FormItems are on different forms); if 
    // "retrieveAll" is false or unset, and there is more than one binding for the dataPath, 
    // this method just returns the first one it finds.<p>
    // Note that if a dataPath is passed in, it should be the full data path for the item, 
    // including any canvas level +link{canvas.dataPath,dataPath} specified on the member 
    // form containing this form item.
    // <br>Note: Unlike the <code>DynamicForm</code> class, this method will not return an 
    // item by index
    // @param itemID (fieldName | dataPath) item fieldName or dataPath identifier
    // @param [retrieveAll] (boolean)       If true, return the list of all FormItems that
    //                                      are bound to this name or dataPath on a member 
    //                                      form of this ValuesManager
    // @return (formItem) form item for editing/displaying the appropriate field, or null if 
    //  no formItem can be found for the field.
    // @visibility external
    //<
    getItem : function (name, retrieveAll) {
        return this._findMemberByField(name, true, retrieveAll);
    },
    
    getField : function (id) {
        return this.getItem(id);
    },
    
    getFieldsForDataPath : function (id) {
        return this.getItem(id, true);
    },
        
    //>@method  ValuesManager.getMembers()   
    //  Retrieves an array of pointers to all the members for this valuesManager.
    // @return (array)   array of member forms
    // @visibility external
    // @group members
    //<    
    getMembers : function () {
        return this.members;
    },
    
    //>@method  ValuesManager.getMember()
    //  Returns a pointer to a specific member.
    // @param   ID  (string)    ID of the member component to retrieve
    // @return (Canvas)   member (or null if unable to find a member with the 
    // specified ID).
    // @visibility external
    // @group members
    //<
    
    getMember : function (ID) {
        // Since the members are all DynamicForm instances, their IDs are global
        var member = window[ID];
        // sanity check
        if (this.members && this.members.contains(member)) return member;
        return null;
    },
    
    //>@method  ValuesManager.getMemberForField()
    // Given a fieldName or dataPath, this method will find the member responsible for
    // interacting with that field's value.
    // If no form is found, returns null.
    // @param fieldName (string) fieldName or dataPath to check
    // @return (Canvas) member responsible for displaying this field (may be null).
    // @group members
    // @visibility external
    //<
    getMemberForField : function (fieldName, retrieveAll) {
        return this._findMemberByField(fieldName, false, retrieveAll);
    },
    
    // helper for getItem() / getMemberForField()
    // Determines which member manages a fieldName or dataPath and returns the appropriate member
    // or item from within a member form
    // Handles cases where a dataPath is partially specified on an item and partially on 
    // a member form
    
    _findMemberByField : function (fieldName, getItem, retrieveAll) {
        
        if (!this.members || fieldName == null || isc.isAn.emptyString(fieldName)) return null;
        
        var trimmedFieldName = fieldName.trim(isc.Canvas._$slash);
        
        // determine whether the fieldName passed in was a dataPath once before
        // looping through members
        var dataPathSegments = trimmedFieldName.split(isc.Canvas._$slash);
        
        // retrieveAll parameter - implies we should return an array of components that match
        // the specified fieldName
        // We only really expect to see multiple components pointing at the same field if we have
        // a 'multiple:true' field with a dataArity:"multiple" component such as a listGrid and
        // a / some dataArity:"single" components such as forms for editing details of it.
        // Note that this is not true of FormItem-level bindings - it is legitimate for multiple
        // form fields to bind to the same "normal" (ie, non-multiple, non-DataSource-typed)
        // DS field as long as they are on different forms.  This allows, eg, binding a complex
        // structure to a set of DFs displayed in a tabSet, with some general information - 
        // customer name or order number, for example - displayed on every form
        var results = retrieveAll ? [] : null;
        for (var i = 0; i < this.members.length; i++) {
            var member = this.members[i],
                memberDataPath = member.getFullDataPath();
            
            // if a member has dataPath set to "/" work with the fieldName directly
            if (memberDataPath == isc.Canvas._$slash || isc.isAn.emptyString(memberDataPath)) {
                memberDataPath = null;
            } else if (memberDataPath != null) {
                memberDataPath = memberDataPath.trim(isc.Canvas._$slash);
            }
            
            if (dataPathSegments && dataPathSegments.length > 0 && memberDataPath != null) {
                
                var soFar = null;
                for (var ii = 0; ii < dataPathSegments.length; ii++) {
                    // we've split the dataPath into segments
                    // generate a string showing the partial dataPath up to this depth                    
                    soFar = !soFar ? dataPathSegments[ii] 
                                   : (soFar + isc.Canvas._$slash + dataPathSegments[ii]);
                    if (memberDataPath.endsWith(isc.Canvas._$slash)) {
                        memberDataPath = memberDataPath.substring(0,memberDataPath.length-1);
                    }
                    
                    // if we have a match, we still may need to check fields within the 
                    // member to ensure the fields match
                    // Example - a member may have dataPath "contacts"
                    // and an item in that member may have dataPath "address/email"
                    if (memberDataPath == soFar) {
                        // If the member has an explicit dataPath matching the
                        // dataPath passed in, just return it
                        
                        if (!getItem && (ii == dataPathSegments.length-1)) {
                            if (!retrieveAll) return member;
                            
                            results.add(member);
                            // break out of the inner loop and check the next member!
                            break;
                        }
                        if (member.getField) {
                            var remainingPath = dataPathSegments.slice(ii+1).join(isc.Canvas._$slash);
                            
                            // this'll catch the case where the item has a partial datapath
                            // or the last level of nesting is handled by fieldName
                            var item = member.getField(remainingPath);
                            if (item) {
                                if (getItem) {
                                    if (!isc.isA.FormItem(item)) item = null;
                                    if (retrieveAll) {
                                        if (item) results.add(item);
                                    } else {
                                        return item
                                    } 
                                } else {
                                    if (retrieveAll) results.add(member);
                                    else return member;
                                }
                            }
                        }
                    }
                }
            } else {
                // handle being passed (EG) "/someField" - this can happen if
                // a dataPath is specified on a component as explicit "/", or if a field has 
                // an absolute path
                // If the form had no expicit dataPath we can just use getItem() whether the
                // value passed in is a datapath or a fieldName
                if (this.members[i].getItem) {
                    var field = this.members[i].getField(fieldName);
                    if (!field) {
                        if (fieldName.startsWith(isc.Canvas._$slash)) {
                            field = this.members[i].getField(fieldName.substring(1));
                        }
                    }
                    if (field) {
                         if (getItem) {
                            if (!isc.isA.FormItem(field)) field = null;
                            if (retrieveAll) {
                                if (field) results.add(field);
                            } else {
                                return field;
                            }
                         } else {
                             if (retrieveAll) results.add(member);
                             else return member;
                         }
                    }
                }
            }
        }
        return retrieveAll ? results : null;
    },
    
    
    // How to handle fileItems?
    // Assume only one fileItem per member form - on saveData(), we'll grab the fileItemForm
    // from our member form and use it to submit all our values.
    
    getFileItemForm : function () {
        if (!this.members) return;
        var hasFileItemForm = false, fileItemForm;
        for (var i = 0; i < this.members.length; i++) {
            if (this.members[i].getFileItemForm == null) continue;
            var form = this.members[i].getFileItemForm();
            if (form) {
                if (hasFileItemForm) {
                    this.logWarn("ValuesManager defined with more than one member form " +
                            " containing a FileItem. This is not supported - binary data may " +
                            "only be uploaded from one FileItem when saving ValuesManager data");                              
                } else {
                    fileItemForm = form;
                    hasFileItemForm = true;
                }
            }
        }
        return fileItemForm;
    },
    
    // Validation:
    
    //> @method valuesManager.validate()
    // Validate the current set of values for this values manager against validators defined
    // in the member forms. For databound valuesManagers, also perform validation against any
    // validators defined on datasource fields.
    // <P>
    // Note that if validation errors occur for a value that is not shown in any member forms,
    // those errors will cause a warning and +link{handleHiddenValidationErrors()} will be
    // called.  This can occur if:<br>
    // - A datasource field has no corresponding item in any member form<br>
    // - The item in question is hidden<br>
    // - The member form containing the item is hidden.
    //
    // @return  (Boolean)   true if all validation passed
    // @visibility external
    // @example formSplitting
    //<
    
    validate : function (validateHiddenFields, ignoreDSFields, typeValidationsOnly, 
            checkValuesOnly, skipServerValidation, suppressShowErrors)
    {
        // Just bail if client-side validation is disabled.
        // Note that we'll still show the errors returned from a failed server save due to
        // 'setErrors' behavior
        if (this.disableValidation) return true;

        // skip validation if we're databound and our datasource has validation disabled
        if (this.dataSource && this.dataSource.useLocalValidators != null &&
            this.useLocalValidators == false) return true;
    
        var hadErrors = this.hasErrors();
    
        // clear hidden errors before starting any validation run
        this.clearHiddenErrors();
    
        // For databound valuesManagers, each member form will be responsible for validating
        // the fields it shows, and the valueManager will validate the rest.  Note that this 
        // works OK even for member forms that are attached to multiple: true fields, because
        // you cannot edit a value for one record and then switch records with the 
        // selectionComponent without losing your edits, unless you explicitly save (in other
        // words, the current values in a member form are the only ones that can have errors,
        // unless the data that came into the ValuesManager in the first place was in error)
        var returnVal = true,
            // fields are returned from ds in {fieldName:fieldObject} format
            dsFields = this.dataSource ? isc.addProperties({}, this.getDataSource().getFields()) 
                                       : null,
            validators = {},
            dataPaths = {},
            isDataPath = false;
            
        // Keep track of all possible dataPaths, so we know which ones have been validated by 
        // member components and which must be validated here
        this.buildDataPathsRecursively(dataPaths, "", this.getDataSource());

        // First go through all the member forms and perform validation.            
        if (this.members) {
            // Wrap field validation in a queue so that server validators are
            // sent as a single request.
            var wasAlreadyQueuing = isc.rpc ? isc.rpc.startQueue() : false;

            for (var i = 0; i < this.members.length; i++) {
                if (!isc.isA.DynamicForm(this.members[i])) continue;
                var form = this.members[i],
                    disableValidation = form.disableValidation,
                    items = this.members[i].getItems();     
                    
                if (!disableValidation) {
                    // we don't want any user-defined handleHiddenValidationErrors to fire on the 
                    // form - instead we'll fire this method at the valuesManager level only.
                    // Implement this by applying our own 'handleHiddenValidationErrors' method to
                    // the form that notifies us what the errors were.
                    if (form.handleHiddenValidationErrors != null) {
                        this.logInfo("form level 'handleHiddenValidationErrors' method suppressed " +
                                     "in favor of valuesManager level handler", "validation");
                        form._prevHHVE = form.handleHiddenValidationErrors;
                    }
                    form.handleHiddenValidationErrors = this._handleHiddenFormErrors;
                }
                
                for (var j = 0; j < items.length; j++) {
                    var fieldName = items[j].getFullDataPath() || items[j].getFieldName();
                    isDataPath = isDataPath || (fieldName && fieldName.contains(isc.Canvas._$slash));
                    // IF the form shares a dataSource with this VM instance, 
                    // remove the appropriate field from our copy of the dataSource fields - 
                    // we have already validated this value.
                    
                    if (dsFields && this.members[i].getDataSource() == this.getDataSource()) {
                        delete dsFields[fieldName];
                    }
                    if (dataPaths && fieldName) {
                        delete dataPaths[fieldName.trim(isc.Canvas._$slash)];
                    }
                }
                // Allow the form to perform its own validation.
                // Validate hidden fields (makes sense since we validate hidden forms!)
                // Pass the additional param to suppress validating DS fields for which there
                // are no items though, since we handle these at the VM level.
                // This will also display validation errors, or fire the method to handle
                // validation errors while hidden.
                var formSuccess = disableValidation ? true : 
                        // suppress showErrors here - we'll show them explicitly from 
                        // VM.showErrors
                        form.validate(true, true, typeValidationsOnly, 
                            checkValuesOnly, skipServerValidation, true);
                returnVal = (returnVal && formSuccess);

                if (!disableValidation) {
                    if (form._preHHVE) form.handleHiddenValidationErrors = form._preHHVE;
                    else delete form.handleHiddenValidationErrors;
                }
                if (!formSuccess) {
                    // If the form itself is hidden, add its full set of errors to our hidden
                    // form validation errors object.
                    if (!(form.isDrawn() && form.isVisible())) {
                        this.addHiddenErrors(form.errors, form);

                    // Otherwise, add just the hidden errors.
                    } else {
                        this.addHiddenErrors(form.getHiddenErrors(), form);
                    }
                }
            }

            // Submit server validation requests queue
            if (!wasAlreadyQueuing && isc.rpc) isc.rpc.sendQueue();
        }


        // we now have to perform validation on the DS fields not present in any member form
        var values = this.getValues(),
            errors = {},
            work = isDataPath ? dataPaths : dsFields;
            
        for (var fieldName in work) {
        
            var fieldObject = work[fieldName],
                validators = fieldObject.validators,
                value = isc.DynamicForm._getFieldValue(fieldName, fieldObject, values, null, true);

            if (validators != null) {
                // iterate through the validators again, this time actually checking each one
                for (var i = 0; i < validators.length; i++) {
                    // validators is an array of validator objects, each of which has the form
                    //    {type:"validatorType", errorMessage:"message", optionalParam:"", ...}
                    var validator = validators[i];
                    if (!validator) continue;
                    // Unless we're looking at a 'required' or 'requiredIf' field,
                    // don't try to validate null values.
                    
                    if (value == null
                        && validator.type != 'required' && validator.type != "requiredIf")
                    {
                        continue;
                    }
                    // we have no item, so pass the field object to processValidator()
                    // This roughly matches what we do in ListGrid validation
                    if (!this.processValidator(fieldObject, validator, value, null, values)) {
                        if (errors[fieldName] == null) errors[fieldName] = [];
                        var errorMessage = validator.errorMessage || this.unknownErrorMessage;
                        errors[fieldName].add(errorMessage);
                    }
                }
            }
            
            // for consistency with forms - if we got a single error, store as a string, not
            // a single element array
            if (errors[fieldName] && errors[fieldName].length == 1) errors[fieldName] = errors[fieldName][0];
            
        }       

        // add hidden errors from fields that are not associated with any form.        
        this.addHiddenErrors(errors);

        var hasErrors = this.hasErrors();
        // if we had errors before, or we have errors now, show errors
        
        if (hadErrors || hasErrors) {
            this.showErrors(true);
        }

        if (isc.getKeys(errors).length > 0)  returnVal = false;

        return returnVal;
    },
    
    buildDataPathsRecursively : function(dataPaths, parentDP, dataSource) {
        if (!isc.isA.DataSource(dataSource)) return;

        if (dataSource.__vmVisited) {
           this.logWarn("detected ds loop at: " + parentDP + ", refusing to recurse further");
           return;
        }
        dataSource.__vmVisited = true;

        var fields = dataSource.getFields();
        for (var key in fields) {
            dataPaths[parentDP + key] = fields[key];
            if (dataSource.fieldIsComplexType(key)) {
                var subDS = dataSource.getSchema(fields[key].type);
                this.buildDataPathsRecursively(dataPaths, parentDP + key + isc.Canvas._$slash, subDS);
            }
        }
        delete dataSource.__vmVisited;
    },
    
    //> @method valuesManager.getValidatedValues()
    // Call +link{valuesManager.validate()} to check for validation errors. If no errors are found,
    // return the current values for this valuesManager, otherwise return null.
    // @return (object|null) current values or null if validation failed.
    // @group errors
    // @visibility external
    //< 
    getValidatedValues : function () {
        if (!this.validate()) return null;
        return this.getValues();
    },     
    
    // Handler for hidden form validation errors. This method is applied directly to the 
    // member form
    _handleHiddenFormErrors : function (errors) {
        var vm = this.valuesManager;
        vm.addHiddenErrors(errors, this);
        return false;   // suppress the standard warning
    },
    
    
    clearHiddenErrors : function () {
        delete this.hiddenErrors;
    },

    // addHiddenErrors()
    // For a valuesManager, hidden validation errors may come from:
    // - a field in the dataSource not associated with any member form item
    // - a hidden item in a member form
    // - a hidden or undrawn member form.
    
    addHiddenErrors : function (errors, form) {
        if (errors == null || isc.isAn.emptyObject(errors)) return;
        
        if (!this.hiddenErrors) this.hiddenErrors = {};
        if (form) {
            if (isc.isA.Canvas(form)) form = form.getID();
        } else form = this.getID();
        
        if (!this.hiddenErrors[form]) this.hiddenErrors[form] = {};
            
        for (var fieldName in errors) {
            this.hiddenErrors[form][fieldName] = 
                this._addFieldErrors(this.hiddenErrors[form][fieldName], errors[fieldName]);
        }
    },
    
    // Returns the current snapshot of hidden errors in a flat list
    getHiddenErrors : function (suppressSynch) {

        if (!suppressSynch) {
            this.synchHiddenErrors();
        }
        
        if (!this.hiddenErrors) return null;
        var flatErrors = {};
        for (var form in this.hiddenErrors) {
            this.assembleHiddenErrorsRecursively(flatErrors, this.hiddenErrors[form]);
        }
        return flatErrors;
    },
    
    assembleHiddenErrorsRecursively : function (flatErrors, formErrors, key, index) {
        if (key == null) key = "";
        var outKey = key;
        if (index != null) outKey += "[" + index + "]";
        if (isc.isA.List(formErrors)) {
            for (var i = 0; i < formErrors.length; i++) {
                if (formErrors[i] !== null) {
                    if (isc.isAn.Object(formErrors[i])) { 
                        this.assembleHiddenErrorsRecursively(flatErrors, formErrors[i], outKey, i);
                    } else {
                    if (flatErrors[outKey] == null) flatErrors[outKey] = [];
                        flatErrors[outKey][i] = formErrors[i];
                    }
                }
            }
        } else if (isc.isAn.Object(formErrors)) {
            for (var objKey in formErrors) {
                if (isc.isAn.Object(formErrors[objKey])) {
                    if (outKey == "") {
                        this.assembleHiddenErrorsRecursively(flatErrors, formErrors[objKey], objKey);
                    } else {
                        this.assembleHiddenErrorsRecursively(flatErrors, formErrors[objKey], 
                                                         outKey + isc.Canvas._$slash + objKey);
                    }
                } else {
                    if (outKey == "") {
                        flatErrors[objKey] = formErrors[objKey];
                    } else {
                        flatErrors[outKey + isc.Canvas._$slash + objKey] = formErrors[objKey];
                    }
                }
            }
        } else {
            flatErrors[outKey] = formErrors;
        }
        return flatErrors;
    },
    
    
    // synchHiddenErrors()
    // This is a method to ensure that our 'hiddenErrors' object returned by getHiddenErrors()
    // and passed to handleHiddenValidationErrors is in synch with the current set of 
    // visible forms / items.
    // Required in the case where 
    // - setErrors() was called, 
    // - form/item visibility was changed, 
    // - showErrors() called.
    
    synchHiddenErrors : function () {
        
        var hiddenErrors = this.hiddenErrors,
            vmID = this.getID();
                    
        // Logic for errors that occurred on fields with no associated member form item 
        // (when errors were stored)
        if (hiddenErrors && hiddenErrors[vmID]) {
            for (var field in hiddenErrors[vmID]) {
                var errors = hiddenErrors[vmID][field],
                    item = this.getItem(field),
                    memberForm = item ? item.form : null;
                    
                // If there is now an associated member form item, we need to add the
                // field error to the form, and update this.hiddenErrors
                if (item) {
                    memberForm.addFieldErrors(field, errors);
                    // clear out the hidden error under the valuesManager's ID - the error
                    // is now associated with a form.
                    delete hiddenErrors[vmID][field];
                }
            }
        }
        
        // Update hidden errors for each form.
        // Quickest to just re-generate hidden errors per form rather than trying to synch with 
        // existing stored hiddenErrors object.
        var vmErrors = hiddenErrors[vmID];
        hiddenErrors = this.hiddenErrors = {};
        if (vmErrors) hiddenErrors[vmID] = vmErrors;
        // Now iterate through every member's errors and add to hidden members arrays if 
        // necessary
        if (this.members) {
            for (var i = 0; i< this.members.length; i++) {
                if (!isc.isA.DynamicForm(this.members[i])) continue;
                var member = this.members[i],
                    memberID = member.getID(),
                    memberErrors = member.errors;
                if (!memberErrors || isc.isAn.emptyObject(memberErrors)) continue;
                
                // shortcut - if the form is hidden always store all its errors. This may
                // overwrite an already up to date this.hiddenErrors[formID] but is quicker
                // than iterating through the errors doing comparisons.
                if (!member.isVisible() || !member.isDrawn()) {
                    memberErrors = isc.addProperties({}, memberErrors);
                    hiddenErrors[memberID] = memberErrors;
                } else {
                    // catch items that have been hidden or removed
                    for (var field in memberErrors) {
                        var item = member.getItem(field);
                        if (!item) {
                            if (!hiddenErrors[vmID]) hiddenErrors[vmID] = {};
                            hiddenErrors[vmID][field] = memberErrors[field];
                            // just delete the field from the form's errors object
                            delete memberErrors[field];
                            
                        } else if (!item.visible) {
                            if (!hiddenErrors[memberID]) hiddenErrors[memberID] = {};
                            hiddenErrors[memberID][field] = memberErrors[field];
                        }
                    }
                }
            }
        }
        
    },
    
    //>	@method	valuesManager.processValidator()	(A)
    //			process a single validator for a field.
    //
    //		@param	[item]		(object)	Form item displaying the value. May be null if no
    //                                      form item exists for the field.
    //		@param	validator	(object)	validation object
    //		@param	value		(string)	value for this field.
    //      @param  [type]      (string)    validator type. if not passed this is derived from
    //                                      the <code>type</code> property on the validation parameter
    // @param record (object) Field values for record being validated.
    //
    //		@return	(boolean)	true == passed validation, false == validation failed
    //		@group	validation
    //<
    processValidator : function (item, validator, value, type, record) {
        
        return isc.Validator.processValidator(item, validator, value, type, record);
    },

    // _handleHiddenValidationErrors()
    // Internal method to display validation errors when we can't show them in a form.
    // This is used to handle 
    // - errors coming from an undrawn or hidden member form
    // - errors coming from hidden items in a member form
    // - errors coming from a dataSource field for which we have no member form item.
    // Note these errors are all combined into a single object retrieved via this.getHiddenErrors()
    // if a developer needs to determine which form an error came from, they can use
    // getMemberForField()
    // Additional suppressSynch parameter - if we know the hidden errors are in synch with
    // the currently visible set of members / fields (IE this has been called directly from
    // setErrors() or validate()) we can skip the logic to ensure that this.hiddenErrors
    // is up to date.
    _handleHiddenValidationErrors : function (suppressSynch) {
        var errors = this.getHiddenErrors(suppressSynch);
        
        // bail if there were no errors on hidden fields
        if (errors == null || isc.getKeys(errors).length == 0) return;
        
        // If we have an implementation to handle the hidden validation errors, call it now.
        var returnVal;
        if (this.handleHiddenValidationErrors) {
            returnVal = this.handleHiddenValidationErrors(errors);
        }
        
        if (returnVal == false) return;
        
        // Log a warning unless this was suppressed by the handleHiddenValidationErrors method.
        var errorString = "Validation failed with the following errors:";
        var errorArray = isc.isAn.Array(errors) ? errors : [errors];
        for (var i = 0; i < errorArray.length; i++) {
            var theErrors = errorArray[i];
            for (var fieldName in theErrors) {
                var fieldErrors = errors[fieldName];
                if (!isc.isAn.Array(fieldErrors)) fieldErrors = [fieldErrors];
                if (fieldErrors.length == 0) continue;

                errorString += "\n" + fieldName + ":";
                for (var i = 0; i < fieldErrors.length; i++) {
                    errorString += (i == 0 ? "- " : "\n - ") + fieldErrors[i];
                }
            }
        }
        this.logWarn(errorString, "validation");
    },
    
    // Validation errors APIs
    
    //>	@method	valuesManager.setErrors() [A]
    // Sets validation errors for this valuesManager to the specified set of errors.
    // Errors should be of the format:<br>
    // <code>{field1:errors, field2:errors}</code><br>
    // where each <code>errors</code> object is either a single error message string or an
    // array of error messages.<br>
    // If <code>showErrors</code> is passed in, error messages will be displayed in the 
    // appropriate member form items. For fields with no visible form item, 
    // +link{valuesManager.handleHiddenValidationErrors()} will be fired instead.<br>
    // Note that if <code>showErrors</code> is false, errors may be shown at any time via
    // a call to +link{ValuesManager.showErrors()}.
    //
    // @param   errors  (object) list of errors as an object with the field names as keys
    // @param   showErrors (boolean) If true display errors now.
    // @group errors
    // @visibility external
    //<
    setErrors : function (errors, showErrors) {
        this.clearHiddenErrors();
        if (isc.isA.List(errors)) errors = errors[0];
        //errors = isc.DynamicForm.formatValidationErrors(errors);
        
        var memberForms = (this.members ? this.members.duplicate() : []);

        for (var i = 0; i < memberForms.length; i++) {
            if (!isc.isA.DynamicForm(memberForms[i])) continue;
            var form = memberForms[i],
                hiddenForm = !form.isVisible() || !form.isDrawn(),
                items = form.getItems(),
                formErrors = {},
                hiddenFormErrors = {},
                selectionChain = form.getSelectionChain();
            for (var j = 0;j < items.getLength(); j++) {
                var item = items[j],
                    itemDataPath = item.getFullDataPath(),
                    itemName = item.getFieldName(),
                    itemErrors = this.getItemErrors(errors, itemDataPath, selectionChain);
                if (itemErrors != null) {
                    formErrors[itemName] = itemErrors;

                    if (hiddenForm || !item.visible) {
                        hiddenFormErrors[itemName] = itemErrors;
                    }
                    this.deleteItemErrors(errors, itemDataPath, selectionChain);
                }
            }
            // suppress redraw and suppress form-level hiddenValidationError handling
            form.setErrors(formErrors, false);

            // hang onto the hidden form errors so we can fire the hiddenValidationErrors
            // handler.
            // Note: track hidden errors by form - see comments near
            // addHiddenErrors() / _getHiddenErrors() for more on this
            if (!isc.isAn.emptyObject(hiddenFormErrors)) 
                this.addHiddenErrors(hiddenFormErrors, form);
        }
        
        this.addHiddenErrors(errors);
        // We know stored hidden errors object is up to date
        if (showErrors) this.showErrors(true);
    },
    
    getItemErrors : function (errors, itemNameOrDataPath, selectionChain) {
        var dataPath = itemNameOrDataPath.trim(isc.Canvas._$slash),
            isDataPath = dataPath.contains(isc.Canvas._$slash);
        if (isc.isAn.Array(errors)) errors = errors[0];
        if (!isDataPath) {
            var serverErrors = errors[itemNameOrDataPath];
        } else {
            var elements = dataPath.split(isc.Canvas._$slash),
                serverErrors = errors,
                nestedListCount = 0;
            for (var i = 0; i < elements.length; i++) {
                serverErrors = serverErrors[elements[i]];
                if (isc.isAn.Array(serverErrors)) {
                    if (selectionChain.length > nestedListCount) {
                        serverErrors = serverErrors[selectionChain[nestedListCount++]];
                    } else {
                        // We have a missing or incomplete selection chain, so we have no real
                        // way to decide which entry in this list is the right one to use.  All
                        // we can do is select the first...
                        serverErrors = serverErrors[0];
                    }
                }
                
                // The error structure is sparse - bail as soon as we encounter a missing path
                if (!serverErrors) break;
            }
        }
        if (serverErrors) {
            if (!isc.isAn.Array(serverErrors)) serverErrors = [serverErrors];
            var clientErrors = [];
            for (var i = 0; i < serverErrors.length; i++) {
                if (serverErrors[i].errorMessage) {
                    clientErrors.add(serverErrors[i].errorMessage);
                } else {
                    clientErrors.add(serverErrors[i]);
                }
            }
            return clientErrors.length > 1 ? clientErrors : clientErrors[0];
        }
    },
    
    deleteItemErrors : function (errors, itemNameOrDataPath, selectionChain) {
        var dataPath = itemNameOrDataPath.trim(isc.Canvas._$slash),
            isDataPath = dataPath.contains(isc.Canvas._$slash);
        if (isc.isAn.Array(errors)) errors = errors[0];
        if (!isDataPath) {
            delete errors[itemNameOrDataPath];
        } else {
            var elements = dataPath.split(isc.Canvas._$slash);
            var serverErrors = errors,
                traversedObjects = [],
                nestedListCount = 0;
            for (var i = 0; i < elements.length; i++) {
                traversedObjects.add(serverErrors);
                serverErrors = serverErrors[elements[i]];
                if (isc.isAn.Array(serverErrors)) {
                    if (selectionChain.length > nestedListCount) {
                        serverErrors = serverErrors[selectionChain[nestedListCount++]];
                    } else {
                        serverErrors = serverErrors[0];
                    }
                }
                if (!serverErrors) break;
            }
            
            if (serverErrors) delete serverErrors;
            
            // Clean up any empty container objects
            for (var i = traversedObjects.length - 1; i >= 0; i--) {
                if (isc.isAn.emptyObject(traversedObjects[i])) {
                    delete traversedObjects[i];
                }
            }
        }
    },
    
    // little helper to combine errors into arrays
    // Returns the errors object to use
    _addFieldErrors : function (oldErrors, newErrors) { 
        if (!oldErrors) return newErrors;
        if (!newErrors) return oldErrors;
        if (!isc.isAn.Array(oldErrors)) oldErrors = [oldErrors];
        if (isc.isA.String(newErrors)) oldErrors.add(newErrors);
        else oldErrors.addList(newErrors);

        return oldErrors;
        
    },
    
    //> @method valuesManager.addFieldErrors()
    // Adds validation errors to the existing set of errors for the field in question.
    // Errors passed in should be a string (for a single error message) or an array of strings.
    // Pass in the showErrors parameter to immediately display the errors to the user by 
    // redrawing the appropriate member form item (or if no visible item is found for the field
    // firing +link{valuesManager.handleHiddenValidationErrors()}.
    // @param fieldName (string) name of field to apply errors to
    // @param errors (string | array of strings) error messages for the field
    // @param showErrors (boolean) should the error(s) be immediately displayed to the user?
    // @group errors
    // @visibility external
    //<
    addFieldErrors : function (fieldName, errors, showErrors) {
        var hidden = true;
        var form = this.getMemberForField(fieldName);
        if (form != null && isc.isA.DynamicForm(form)) {
            form.addFieldErrors(fieldName, errors, false);
            var item = form.getItem();
            if (form.isVisible() && form.isDrawn() && item && item.visible) {
                hidden = false;
            }
        }
        
        if (hidden) {    
            if (!this.hiddenErrors) this.hiddenErrors = {};
            var formName = form ? form.getID() : this.getID();
            if (!this.hiddenErrors[formName]) this.hiddenErrors[formName] = {};

            this.hiddenErrors[formName][fieldName] = 
                this._addFieldErrors(this.hiddenErrors[formName][fieldName], errors);

        }
        
        if (showErrors) this.showFieldErrors(fieldName);
    },
    
    //> @method valuesManager.setFieldErrors()
    // Sets validation errors for some field in the valuesManager.<br>
    // Errors passed in should be a string (for a single error message) or an array of strings.
    // Pass in the showErrors parameter to immediately display the errors to the user by 
    // redrawing the appropriate member form item (or if no visible item is found for the field
    // firing +link{valuesManager.handleHiddenValidationErrors()}.
    // @param fieldName (string) name of field to apply errors to
    // @param errors (string | array of strings) error messages for the field
    // @param showErrors (boolean) should the error(s) be immediately displayed to the user?
    // @group errors
    // @visibility external
    //<    
    setFieldErrors : function (fieldName, errors, showErrors) {
        var hidden = true;
        var form = this.getMemberForField(fieldName);
        if (form != null && isc.isA.DynamicForm(form)) {
            form.setFieldErrors(fieldName, errors, false);
            var item = form.getItem();
            if (form.isVisible() && form.isDrawn() && item && item.visible) {
                hidden = false;
            }
        }
        
        if (hidden) {
            if (!this.hiddenErrors) this.hiddenErrors = {};
            this.hiddenErrors[fieldName] = errors;
        }
        
        if (showErrors) this.showFieldErrors(fieldName);    
    },
    
    //>	@method	valuesManager.clearErrors()
    //			Clears all errors from member forms.
    //      @param  showErrors (boolean)    If true, clear any visible error messages.
    //		@group	errors
    //      @visibility external
    //<
    clearErrors : function (showErrors) {
        this.setErrors({}, showErrors);
    },
    
    //> @method valuesManager.clearFieldErrors()
    // Clear all validation errors associated with some field in this form
    // @param fieldName (string) field for which errors should be cleared
    // @param show (boolean) if true, and the field is present in one of our member forms, 
    //                       redraw it to clear any currently visible validation errors
    // @group errors
    // @visibility external
    //<
    clearFieldErrors : function (fieldName, show) {
        var form = this.getMemberForField(fieldName);
        if (form && isc.isA.DynamicForm(form)) form.clearFieldErrors(fieldName, show);
        
        if (this.hiddenErrors) delete this.hiddenErrors[fieldName];
    },

    //> @method valuesManager.getErrors()
    // Returns the set of errors for this valuesManager.
    // Errors will be returned as an object of the format <br>
    // <code>{field1:errors, field2:errors}</code><br>
    // Where each errors object is either a single error message or an array of error message
    // strings.
    // @return (object) Object containing mapping from field names to error strings. Returns null
    //                  if there are no errors for this valuesManager.
    // @group errors
    // @visibility external
    //<
    // Stored errors include those stored as "hiddenErrors", with no associated form (came from
    // a datasource field definition only, presumably), and errors from member forms
    getErrors : function () {
        // pick up stored hidden errors.
        // [don't bother to synch - we're not interested in whether they're truly hidden or not now]
        var errors = isc.addProperties({}, this.getHiddenErrors(true));
        // add errors from each member form                              
        
        if (this.members) {
            for (var i = 0; i < this.members.length; i++) {
                if (!isc.isA.DynamicForm(this.members[i])) continue;
                isc.addProperties(errors, this.members[i].getErrors());
            }
        }
        if (!isc.isA.emptyObject(errors)) return errors
        return null
    },
    
    //> @method valuesManager.getFieldErrors()
    // Returns any validation errors for some field in this valuesManager.
    // Errors will be returned as either a string (a single error message), or an array 
    // of strings. If no errors are present, will return null.
    // @param fieldName (string) fieldName to check for errors
    // @return (string | array of strings) error messages for the field passed in
    // @group errors
    // @visibility external
    //<
    getFieldErrors : function (fieldName) {
        var form = this.getMemberForField(fieldName)
        if (form && isc.isA.DynamicForm(form)) return form.getFieldErrors(fieldName);
        if (this.hiddenErrors && this.hiddenErrors[this.getID()]) 
            return this.hiddenErrors[this.getID()][fieldName];
    },
    
    //> @method valuesManager.hasErrors()
    // Are there any errors associated with any fields in this valuesManager?
    // @return (Boolean) returns true if there are any outstanding validation errors, false 
    //                  otherwise.
    // @group errors
    // @visibility external
    //<
    hasErrors : function () {
        if (this.hiddenErrors && !isc.isA.emptyObject(this.hiddenErrors)) {
            for (var form in this.hiddenErrors) {
                if (this.hiddenErrors[form] && !isc.isAn.emptyObject(this.hiddenErrors[form]))
                    return true;
            }
        }
        if (this.members == null) return false;
        for (var i = 0; i < this.members.length; i++) {
            if (isc.isA.DynamicForm(this.members[i]) && this.members[i].hasErrors()) return true;
        }
        return false;
    },
    
    //> @method valuesManager.hasFieldErrors()
    // Are there any errors associated with a field in this valuesManager?
    // @param fieldName (string) field to check for errors
    // @return (Boolean) returns true if there are any outstanding validation errors, false 
    //                  otherwise.
    // @group errors
    // @visibility external
    //<        
    hasFieldErrors : function (fieldName) {
        var form = this.getMemberForField(fieldName);
        if (form && isc.isA.DynamicForm(form) && form.hasFieldErrors(fieldName)) return true;
        var hiddenErrors = this.getHiddenErrors(true);
        if (hiddenErrors && hiddenErrors[fieldName] != null) return true;
        return false;
    },
    
    //> @method valuesManager.showErrors()
    // Method to explicitly show the latest set of validation errors present on this 
    // ValuesManager.<br>
    // Will redraw all member forms to display (or clear) currently visible errors, and
    // fire +link{valuesManager.handleHiddenValidationErrors()} to allow custom handling of
    // hidden errors.
    // @group errors
    // @visibility external
    //<
    // suppressHiddenErrorSynch parameter: indicates we know our stored hidden errors match the 
    // currently visible set of fields [so we just ran validate() or setErrors()].
    // passed through to _handleHiddenValidationErrors()
    showErrors : function (suppressHiddenErrorSynch) {
        if (this.members) {
            for (var i= 0; i < this.members.length; i++) {
                if (!isc.isA.DynamicForm(this.members[i])) continue;
                if (!this.members[i].isDrawn() || !this.members[i].isVisible()) continue;
                this.members[i]._suppressAutoFocusOnErrors = true;
                this.members[i].showErrors();
            }
        }
        
        if (this.hiddenErrors != null) {
            this._handleHiddenValidationErrors(suppressHiddenErrorSynch);
        }
    },
    
    //> @method valuesManager.showFieldErrors()
    // Method to explicitly show the latest set of validation errors present on some field 
    // within this ValuesManager.<br>
    // If the field in question is present as a visible item in a member form, the form item
    // will be redrawn to display the error message(s).
    // Otherwise +link{valuesManager.handleHiddenValidationErrors()} will be fired to allow 
    // custom handling of hidden errors.
    // @group errors
    // @visibility external
    //<
    showFieldErrors : function (fieldName) {
        var form = this.getMemberForField(fieldName);
        if (form && isc.isA.DynamicForm(form) && form.isVisible() && form.isDrawn()) {
            var item = form.getItem(fieldName);
            if (item && item.visible) {
                item.redraw("Validation errors modified");
                return;
            }
        }
        
        // At this point we know we have a hidden error for the field - fire the 
        // handleHiddenValidationErrors method. Of course that actually 'shows' the
        // errors for all hidden fields, not just this one.
        this._handleHiddenValidationErrors();
    },
    
    // Flow Methods:
    
    //> @method getFilterCriteria()
    // Return the set of filter criteria for this valuesManager based on its current set of 
    // values
    // @return (object) set of name:values pairs to be used as filter criteria
    //<
    getFilterCriteria : function () {
        // get filter criteria from all my members
        
        var crit = {};
        if (this.members) {
            for (var i =0; i < this.members.length; i++) {
                isc.addProperties(crit, this.members[i].getFilterCriteria());
            }
        }
        
        // Mix in any values we have that didn't come from member forms
        var values = this.getValues(),
            undef;
        for (var field in values) {
            if (crit[field] !== undef) delete values[field];
        }
        // filterCriteriaForFormValues will clear out null values, and handle arrays with an
        // empty entry (Implies a null criterion)
        isc.addProperties(crit, isc.DataSource.filterCriteriaForFormValues(values));
        
        return crit;
    },

    
    // ========================================================================================
    //  Values Management APIs
    // ========================================================================================
    
    //> @method valuesManager.getValues()   
    // Returns the current set of values for the values manager instance.  This includes the
    // values from any form managed by this manager, as well as any values explicitly applied
    // via +link{valuesManager.setValues()}.
    // @return (object) a map of the values for this manager
    // @group formValues
    // @visibility external
    //<
    getValues : function () {

        // if one of our member forms has focus, ensure its focus-item's value has been saved
        // out [which will update this.values]
        if (this.members != null) {
            var fc = isc.EH.getFocusCanvas();
            if (this.members.contains(fc) && fc.updateFocusItemValue) fc.updateFocusItemValue();
        }
        // Never let this.values be externally accessible.
            
        return isc.addProperties({}, this.values);
    },
    
    //> @method valuesManager.setValues()   
    // Replaces the current values of the ValuesManager and all member components with the 
    // values passed in.
    // <P>
    // Values should be provided as an Object containing the new values as properties, where
    // each propertyName is the name of a +link{items,form item} in one of the member forms,
    // and each property value is the value to apply to that form item via
    // +link{FormItem.setValue()}.
    // <P>
    // Values with no corresponding form item may also be passed, will be tracked by the
    // valuesManager and returned by subsequent calls to +link{getValues()}.
    // <P>
    // Any +link{FormItem} for which a value is not provided will revert to its
    // +link{formItem.defaultValue,defaultValue}.  To cause all FormItems to revert to default
    // values, pass null.
    // <P>
    // This method also calls +link{rememberValues()} so that a subsequent later call to
    // +link{resetValues()} will revert to the passed values.
    // 
    // @param   values  (object)    new set of values for this values manager.
    // @group formValues
    // @visibility external
    //<    
    setValues : function (values) {
        if (isc.isAn.Array(values)) {
            var useFirst = isc.isA.Object(values[0]);
            this.logWarn("values specified as an array." + 
                        (useFirst ? " Treating the first item in the array as intended values."
                                  : " Ignoring specified values."));
            if (useFirst) values = values[0];
            else values = null;                                  
        }
    
        
    
        // Duplicate the values object so we can manipulate it and apply it directly to 
        // this.values and modify without interfering with external code.
        // _duplicateValues does a recursive duplication based on dataPaths
        var clonedVals = {};
        isc.DynamicForm._duplicateValues(this, values, clonedVals);
        values = clonedVals;
        
        this.values = values;
        if (this.members) {
            for (var i = 0; i < this.members.length; i++) {
                // setMemberValues will update the members' items to display the values passed in
                // Note that for DynamicForms, it also explicitly calls 'clearValue()' on items
                // for which we have no member - this re-evaluates default values
                this._setMemberValues(this.members[i]);
            }
        }
        // remember values for resetting
        this.rememberValues();
        
    },
    
    //> @method valuesManager.setData()
    // Set the data (values) on this valuesManager [synonym for <code>setValues()</code>].
    //<
    // setData() is used in dataBinding rather than setValues.
    setData : function (values) {
        return this.setValues(values);
    },

    //> @method valuesManager.clearValues()   
    // Clear out all the values managed by this values manager.
    // @visibility external
    // @group formValues
    //<
    clearValues : function () {
        this.setValues({});
    },

    //> @method valuesManager.getMemberValues()   
    // Returns the subset of this valuesManager's values associated with some member form.
    //  
    // @param   ID  (string)    ID of the member form for which we want to retrieve the values.
    // @return (object) a map of the values for the appropriate member form.
    // @visibility external
    // @group formValues
    //<    
    getMemberValues : function (ID) {
        var member = this.getMember(ID);
        if (member != null) return member.getValues();
    },
    
    //> @method valuesManager.setMemberValues()
    // Set the values for some member form.
    // @param   ID  (string)    ID of the member form to update
    // @param   values  (object)    new values for the form
    // @visibility external
    // @group formValues
    //<    
    setMemberValues : function (ID, values) {
        var member = this.getMember(ID);
        if (member != null) return member.setValues(values);
    },
    
    
    //> @method valuesManager.rememberValues()
    // @include dynamicForm.rememberValues()
    //<
    // Values may change as a result of 
    // - adding a new member and picking up values for fields for which we previously had no 
    //   value
    // - the user modifying values in a field
    // - calls to 'setValue()' [not setValues as that will re-remember the values after setting]
    rememberValues : function () {
        
    	var values = this.getValues();
		
		var oldVals = {},
            rememberedDefault = [];
            
        // Recursively duplicate values so further edits won't manipulate the remembered values
        // directly.
        isc.DynamicForm._duplicateValues(this, values, oldVals, rememberedDefault);
        
        // Remember the duplicated values object
        this._oldValues = oldVals;
        // rememberedDefault array will contain dataPaths for every item that had its value
        // set to the default in the 'values' object we passed in.
        // We need this information so 'resetValues' can set these items to null and
        // potentially re-evaluate a dynamicDefault rather than resetting to whatever the
        // value is at this moment.
        // [still store the current val for valuesHaveChanged() checks]
        this._rememberedDefault = rememberedDefault;

    	return this._oldValues;
    },
    
    //> @method valuesManager.getOldValues()
    // @include dynamicForm.getOldValues()
    //<
    getOldValues : function () {
        var oldValues = {};
        isc.addProperties(oldValues, this._oldValues);
        return oldValues;
    },
    
    
    //> @method valuesManager.getChangedValues()
    // @include dynamicForm.getChangedValues()
    // @see getOldValues()
    // @visibility external
    //<
    getChangedValues : function () {
        return this.valuesHaveChanged(true);
    },
    
    
    //> @method valuesManager.resetValues()
    // @include dynamicForm.resetValues()
    //<
    resetValues : function () {
        // pull the values from form._oldValues into ValuesManager.values
        var values = {};
        isc.DynamicForm._duplicateValues(this, this._oldValues, values);
        // clear any remembered defaults so they get re-eval'd
        for (var i = 0; i < this._rememberedDefaults; i++) {
            isc.DynamicForm._clearFieldValue(this._rememberedDefaults[i], values, this);
        }
        
        this.setValues(values);
        
    },
    
    //> @method valuesManager.valuesHaveChanged()
    // @include dynamicForm.valuesHaveChanged()
    //<
    valuesHaveChanged : function (returnChangedVals) {
        var values = this.getValues();
	    var oldValues = this._oldValues || {};

        return isc.DynamicForm.valuesHaveChanged(this,returnChangedVals,values,oldValues);
    },

    //> @method valuesManager.getValue()
    // Returns the value for some field.
    // @param   fieldName   (string)    Which value to be returned
    // @param   [component] (Canvas)    Optional, the component for which we are trying to
    //                                  retrieve a value.  This is used to identify which of
    //                                  the potential records to use when the ValuesManager
    //                                  is managing a complex structure involving nested Lists
    // @return  (any)   current value of the appropriate field
    // @visibility external
    // @group formValues
    //<
    getValue : function (fieldName, component) {
        return isc.DynamicForm._getFieldValue(fieldName, this.getField(fieldName),
                                                this.values, component, true, "vm_getvalue");
    },
    
    //> @method valuesManager.setValue()
    // Set the value for some field.
    // @param   fieldName   (string)    Which field to set the value for
    // @param   newValue    (any)       New value for the field.
    // @visibility external
    // @group formValues
    //< 
    setValue : function (fieldName, newValue) {

        var valueSet = false,
            member,
            undef;
        if (this.members) {
            var items = this.getItem(fieldName, true);
            if (items && items.length > 0) {
                for (var i = 0; i < items.length; i++) {
                    var item = items[i];
                    if (item && item.setValue) {

                        // Handle this being a field with an 'opaque' data type with a get/set atomic
                        // value method
                        // If this is the case, extract the atomic value and pass it to the item.
                        
                        var type = item.type ? isc.SimpleType.getType(item.type) : null,
                            itemValue = newValue,
                            isUndef = (newValue === undef);
                        if (!isUndef && type && type.getAtomicValue && type.updateAtomicValue) {
                            // store the new atomic type on our values object
                            
                            // getFullDataPath will prepend any member-widget level dataPath if necessary
                            fieldName = item.getFullDataPath();
                            isc.DynamicForm._saveFieldValue(fieldName, null, newValue, this.values, null, true);
                            // extract the atomic value which we'll pass to item.setValue()
                            itemValue = type.getAtomicValue(newValue);
                        }

                        if (isUndef) item.clearValue();
                        else item.setValue(itemValue);
                        valueSet = true;
                    }
                }
            }
        }
        if (!valueSet) {
            if (newValue === undef) {
                isc.DynamicForm._clearFieldValue(fieldName, this.values);
            } else {
                
                isc.DynamicForm._saveFieldValue(fieldName, null, newValue, this.values, null, true);
            }
        }
        var members = this._findMemberByField(fieldName, false, true);
        if (members) {
            for (var i = 0; i < members.length; i++) {
                if (member && member.setData) {
                    var dataObjPath = fieldName;
                    if (fieldName.indexOf(isc.Canvas._$slash) != -1) {
                        dataObjPath = fieldName.substring(0, fieldName.lastIndexOf(isc.Canvas._$slash));
                        member.setData(isc.DynamicForm._getFieldValue(dataObjPath, null, 
                                                                      this.values, member, true));
                    }
                }
            }
        }
    },
    

    //> @method valuesManager.clearValue()
    // Clear the value for some field.
    // @param   fieldName   (string)    Which field to set the value for
    // @visibility external
    // @group formValues
    //< 
    clearValue : function (fieldName) {
        this.setValue(fieldName);
    },
    
    // ========================================================================================
    //  Member Management
    // ========================================================================================
    
    //> @method valuesManager.addMember()   
    //
    // Add a new member to this valuesManager.  Any +link{class:Canvas} can be a member of a
    // valuesManager, even components like +link{class:Layout} or +link{class:TabSet} that do
    // not actually have any values to manage.  When "valueless" components like these bind to
    // a ValuesManager, it is in order to provide their own child components with a shared
    // valuesManager so that complex data can be displayed and edited - see 
    // +link{DataPath} for more details.
    // <p>
    // For components like +link{class:DynamicForm} and +link{class:ListGrid}, which do have 
    // a set of values to manage, the component's values will subsequently be available through
    // this valuesManager.
    // <p>
    // Note on pre-existent values when the member component is a +link{class:DynamicForm}:<br>
    // If the valuesManager has a value specified for some field, for which the member form has
    // an item, this value will be applied to the member form.  This is true whether the item
    // has a value or not.<br>
    // However if the member form has a value for some field, and the ValuesManager does not
    // have a specified value for the same field, we allow the valuesManager to pick up the 
    // value from the member form.    
    //
    // @param   member  (DynamicForm | String) component (or ID of component) to add to 
    //                                          this valuesManager as a member.
    // @visibility external
    // @group members
    // @see method:ValuesManager.addMembers
    //<    
    addMember : function (member, fromDataPath) {
        // If passed an ID, assume it's a pointer to the form.
        if (isc.isA.String(member)) member = window[member];
        
        if (!isc.isA.Canvas(member)) {
            this.logWarn("addMember() passed invalid object: " + this.echo(member) + 
                         " - this should be a Canvas instance");
            return;
        }
        if (member.valuesManager != null) member.valuesManager.removeMember(member);
        
        if (this.members == null) this.members = [];
        
        // If the member has an explicit, different dataSource specified, log a warning.
        // Different dataSources are a problem, as datasource field properties (including
        // specified validators) will not be reflected in the form.
        // Don't catch the case where the member dataSource is unset, it may be using
        // datapath to pick up the appropriate dataSource field attributes.
        var memberDS = member.getDataSource();
        if (memberDS != null && !fromDataPath && memberDS != this.getDataSource()) {
            this.logWarn("addMember(): mismatched DataSources; new member form " + member + 
                         " has dataSource: '" + memberDS.ID + 
                         "', valuesManager has DataSource " + 
                         (this.getDataSource() != null ? "'"+this.getDataSource().ID+"'" : "[NONE]"));
        }
        
        // If any member forms are multipart, warn the developer - this implies that
        // they need direct submission.        
        if (this.getDataSource() != null && member.isMultipart && 
            member.isMultipart() && member.isMultipart()) 
        {
            this.logWarn("addMember(): new member form " + member +
                " is flagged as using multipart encoding. Multipart forms require direct form " +
                "submission to transfer uploaded files to the server - any uploaded files from " +
                "this member form will be dropped when saving values from this ValuesManager to " +
                "the server."
            );
        }
        
        // catch the case where the member is a dataArity singular component but is editing
        // a multiple:true field - in this case we auto attach a selectionComponent if possible
        if (member.dataArity == "single" && member.autoTrackSelection) {
            // Don't clobber a user-specified selectionComponent
            if (member.selectionComponent == null || member._autoSelectionComponent) {
                var dataPath = member.getFullDataPath(),
                    field = dataPath ? this.getDataSourceField(dataPath) : null,
                    newValues = isc.DynamicForm._getFieldValue(dataPath, null, this.values, member, true),
                    multiple = isc.isAn.Array(newValues) || (field && field.multiple);
                if (multiple) {                
                    var selectionComponents = this.getMemberForField(dataPath, true);
                    if (selectionComponents && selectionComponents.length > 0) {
                        for (var i = 0; i < selectionComponents.length; i++) {
                            var selectionComponent = selectionComponents[i];
                            if (selectionComponent.dataArity == "multiple") {
                                member.setSelectionComponent(selectionComponent);
                                member._autoSelectionComponent = true;
                                break;
                            }
                        }
                    }
                }
            }
        // also catch the case where a singular item was already added for a multiple:true field
        // and the selection component is added after the fact
        } else {
            var dataPath = member.getFullDataPath(),
                singularComponents = this.getMemberForField(dataPath, true);
            if (singularComponents && singularComponents.length > 0) {
                for (var i = 0; i < singularComponents.length; i++) {
                    if (singularComponents[i].dataArity == "single" &&
                        singularComponents[i].autoTrackSelection &&
                        (singularComponents[i].selectionComponent == null ||
                         singularComponents[i]._autoSelectionComponent == true)) 
                    {
                        singularComponents[i].setSelectionComponent(member);
                        singularComponents[i]._autoSelectionComponent = true;
                    }
                }
            }
        }

        // We also need to bind to a selectionComponent for components of dataArity "multiple"
        // that are editing a field which is a descendant of a multiple: true field
        if (member.dataArity == "multiple" && member.autoTrackSelection) {
            var dataPath = member.getFullDataPath(),
                isDataPath = dataPath && dataPath.contains(isc.Canvas._$slash);

            if (isDataPath) {
                var elements = dataPath.split(isc.Canvas._$slash);
                // Skip the very last element - that's pointing at this member's dataPath, and
                // we're only interested in ancestors for this purpose
                dataPath = "/";
                for (var i = elements.length - 2; i >= 0; i--) {
                    for (var j = 0; j <= i; j++) {
                        dataPath += elements[j];
                        if (j != i) dataPath += "/";
                    }
                    var field = this.getDataSourceField(dataPath),
                        newValues = isc.DynamicForm._getFieldValue(dataPath, null, this.values, member, true),
                        multiple = isc.isAn.Array(newValues) || (field && field.multiple);
                    if (multiple) break;
                }
            }
            
            if (multiple) {
                var selectionComponents = this.getMemberForField(dataPath, true);
                if (selectionComponents && selectionComponents.length > 0) {
                    for (var i = 0; i < selectionComponents.length; i++) {
                        var selectionComponent = selectionComponents[i];
                        if (selectionComponent.dataArity == "multiple") {
                            member.setSelectionComponent(selectionComponent);
                            member._autoSelectionComponent = true;
                            break;
                        }
                    }
                }
            }
            // also catch the case where the selection component is added second
            var dataPath = member.getFullDataPath();
            if (dataPath && dataPath != "") {
                var members = this.members;
                for (var i = 0; i < members.length; i++) {
                    if (members[i] == member) continue;
                    if (members[i].dataArity == "single") continue;
                    var otherDataPath = members[i].getFullDataPath();
                    if (otherDataPath && otherDataPath != dataPath 
                                      && otherDataPath.startsWith(dataPath)) 
                   {
                        // Ensure that the target component is not already bound to a better 
                        // selectionComponent (ie, one between it and this component in the 
                        // hierarchy), or one explicitly specified by the user
                        if (members[i].selectionComponent != null) {
                            if (members[i]._autoSelectionComponent) {
                                var existingDataPath = members[i].selectionComponent.getFullDataPath();
                                if (dataPath.length > existingDataPath.length) {
                                    members[i].setSelectionComponent(member);
                                    members[i]._autoSelectionComponent = true;
                                }
                            }
                        }
                    }
                }
            }
        } 

        this.members.add(member);
        
        member.valuesManager = this;
        
        // If the member dataSource is null, bind it to the VM's dataSource; this step will be
        // done anyway by DBC.setDataPath(), but setDataSource is a destructive operation (it
        // wipes out the DBC's values to ensure that they cannot mismatch with the new 
        // DataSource), and with certain orders of operation, we can end up with components
        // having their data cleared
        if (member.dataSource == null && this.dataSource != null && member.getFields) {
            var fields = isc.isA.DynamicForm(member) ? member._itemsConfig : member.getFields();
            fields = fields || member.getFields();
            var dataPath = member.getFullDataPath();
            var dataSource = this.getDataSource();
            // If the member has a 'dataPath', bind to the nested dataSource to which it refers
            if (dataPath) {
                var dataSource = dataSource.getDataSourceForDataPath(dataPath, true);
            }

            member.setDataSource(dataSource, fields);
        }
        
        // call _setMemberValues() to update the member data with values defined on this
        // VM.
        // Pass in the 'pickupValues' parameter - on initial add, we want to pick up any 
        // values present in the form for which we don't already have values
        // (and warn / replace where there are collisions)
        this._setMemberValues(member, true);

        // set a flag so we know if this was auto-attached as part of setDataPath()
        // This allows us to respect explicitly specified valuesManager if the dataPath changes
        // later, but recalculate derived ones.
        member._valuesManagerFromDataPath = fromDataPath;
        
        // We have directly manipulated the values object, so we should re remember it.
        this.rememberValues();

    },
    
    // _setMemberValues - updates the values of a member (form or other dataBoundComponent) based
    // on the valuesManager values.
    // if 'pickupMemberValues' is passed - for cases where the member has existing values 
    // (and the valuesManager doesn't) we pick up the field values from the member.
    // [if there are values specified on the vm and the member, the vm values will replace the
    //  members' values]
    // Called
    // - when a member is first added to this valuesManager
    // - from valuesManager.setValues()
    _setMemberValues : function (member, pickupMemberValues) {
        // Ignore inert members. Use the presence of 'getFields' as a rapid check for
        // data-aware components.
        if (member.getFields == null) return;
        
        // if a field is multiple, the values are expected to be an array.
        // Look at the dataArity of the databound member component to determine
        // whether we should display this array of values in the member
        var memberDataPath = member.getFullDataPath(),
            field = this.getField(memberDataPath),
            newValues = isc.DynamicForm._getFieldValue(memberDataPath, null, this.values, member, true),
            multiple = isc.isAn.Array(newValues) || (field && field.multiple),
            selComponent = member.selectionComponent;
        
        if (multiple) {
            if (member.dataArity == "single") {
                // Something that edits singular values is being assigned a multiple value.
                // - if a selectionComponent is set this means that this singular component is
                //   coordinating with a dataArity:multiple selectionComponent.  Ignore the
                //   update since the singular component is already observing the multiple
                //   component.
                if (selComponent != null) {
                    
                    // NOTE: The value derived here is *not* used for DynamicForms; forms are
                    // populated item-by-item, using the _getFieldValue() API (whioch knows how
                    // to derive the correct record for a multiple:true field)
                    var record = member._selectionComponentRecordPKs,
                        foundRecord = false;
                    for (var i = 0; i < newValues.length; i++) {
                        if (newValues[i] == record) {
                            foundRecord = true;
                            break;
                        }
                    }
                    if (foundRecord) {
                        newValues = newValues[i];
                    } else {
                        newValues = null;
                        // This is a normal condition, encountered during initial values
                        // setting, so only log it at debug level
                        this.logDebug("Unable to locate selectionComponent's selected record in " +
                                    "_setMemberValues.  Record is: " + isc.Comm.serialize(record));
                    }
                } else {
                    
                    // - if no selectionComponent is present it's tricky to know what the right behavior is
                    //   but default to editing the first record in the array of values.
                    if (isc.isAn.Array(newValues)) newValues = newValues[0];
                }
            } 
            // else: multiple component editing multiple values, as expected
        } else {
            // singular value for a multiple component: upconvert to an Array
            if (newValues != null && member.dataArity == "multiple") newValues = [newValues]
        }
        
        // if the member is not a dynamicForm, we'll just use 'setData()' to apply the appropriate
        // values to the member
        // This will apply values for all fields that match the dataPath of the object
        // (or possibly our top level values object) - differs from logic for forms where
        // we selectively apply values only to fields present in the form
        
        if (!isc.isA.DynamicForm(member)) {
            if (!member.setData) return;
            
            var dataPath = member.getFullDataPath(),
                // if pickupMemberValues is unset we don't care what the old values were
                oldValues = pickupMemberValues ? member.getData() : null;

            if (newValues == null) {
                
                if (pickupMemberValues) {
                    isc.DynamicForm._saveFieldValue(dataPath, null, oldValues, this.values, member, true);
                }
            } else {
                // if oldValues is anything other than
                // null, {} or [], it "has meaning" - log a warning that we're clobbering it rather
                // than picking it up.
                if (pickupMemberValues && 
                    oldValues != null && !isc.isAn.emptyObject(oldValues) &&
                    !isc.isAn.emptyArray(oldValues))
                {
                    this.logInfo("ValuesManager member:" + member.getID() +
                        " has existing values:" + this.echo(oldValues) +
                        ", replacing with values from this valuesManager:" + this.echo(newValues)); 
                }
                member.setData(newValues);
            }
            
        } else {
        
            // for dynamicForms only apply values for which the form is actually displaying
            // items, since we can split the values for a record across multiple forms and we
            // don't want to be maintaining multiple values objects
            var items = member.getItems(),
                hasChanges = false,
                undef;
            for (var i = 0; i < items.getLength(); i++) {
                var item = items[i];
                
                
                
                var itemPath = item.getTrimmedDataPath() || item.getFieldName();
                
                // Item with no name - just ignore it.
                if (itemPath == null) continue;
                var memberDataPath = member.getFullDataPath(),
                    isAbsolute = itemPath.startsWith(isc.Canvas._$slash),
                    fullFieldPath = isAbsolute ? itemPath
                                        : this._combineDataPaths(memberDataPath, itemPath);

                // Figure out the value for the field
                
                var newFieldValue = isc.DynamicForm._getFieldValue(fullFieldPath, null,
                                                                   this.values, 
                                                                   member, true);

                // for cases where we're looking inside the form - check for the
                // form already having a specified value for the field so we can notify
                // the developer we're clobbering it.
                
                if (!isAbsolute) {
                    var currentFieldValue = isc.DynamicForm._getFieldValue(fullFieldPath, 
                                                           null,
                                                           member.values, 
                                                           member, true);

                    if (currentFieldValue !== undef) {
                         this.logInfo("Member form " + member +
                                " has specified value for field '" + fullFieldPath +  
                                "' which collides with an already specified value in this " +
                                "ValuesManager. Resetting the value on the member form.");
                    }
                }
                
                if (newFieldValue !== undef) {
                    
                    member.setValue(itemPath, newFieldValue);
                } else {
                    // explicitly calling 'clearValues()' will cause dynamic defaults to be
                    // re-evaluated
                    if (!pickupMemberValues) member.clearValue(itemPath);
                }
                
                // Pick the value back up from the item
                // This will re-evaluate defaults on items, and potentially perform other
                // modification such as type-casting, so store the item's value again here 
                if (item.shouldSaveValue != false) {
                    
                    
                    var updatedFieldVal = member.getValue(itemPath);
                    
                    if (updatedFieldVal === undef) {
                        isc.DynamicForm._clearFieldValue(fullFieldPath, this.values, member, true);
                    } else {
                        
                        isc.DynamicForm._saveFieldValue(fullFieldPath, null,
                                                        updatedFieldVal,
                                                        this.values, member, true);
                    }

                    hasChanges = true;
                }
                
                
                if (item.formValuesChanged && isc.isA.Function(item.formValuesChanged)) 
                    item.formValuesChanged();

                if (hasChanges) {
                    // fire valuesChanged() on the form, if any values have actually changed
                    if (member.valuesChanged) member.valuesChanged();
                    // redraw the member - required to correctly update the HTML for 
                    // readOnlyDisplay: "static" TextItems
                    if (member.markForRedraw) member.markForRedraw();
                }
            }
            
            if (pickupMemberValues) {
                //>DEBUG
                this._findItemlessFormValues(member);
                //<DEBUG
            }
        }
    },
    
    //>DEBUG
    // findItemlessFormValues
    // When we first add a DynamicForm to a ValuesManager it may have values for fields
    // with no associated items
    // This is a helper method to find any values from form.getValues() with no associated
    // items.
    // We don't currently add them to this.values - just log a warning and clear them on the form
    // to avoid future confusion
    _findItemlessFormValues : function (form, values, dataPath, itemlessValues, dontWarn) {
        if (values == null) values = form.getValues();
        if (itemlessValues == null) itemlessValues = [];
        for (var prop in values) {
            var fieldID = dataPath ? this._combineDataPaths(dataPath, prop) : prop;
            if (!form.getItem(fieldID)) {
                var value = values[prop];
                if (!isc.isAn.Object(value) || isc.isA.Date(value) || isc.isAn.Array(value)) {
                    itemlessValues.add(fieldID);
                    // clear the value from the form so we avoid future confusion
                    form.clearValue(fieldID);
                    
                    // if we wanted to pick up these values and store them we could do so here
                    /*
                    var fullDataPath = form.dataPath 
                                            ? this._combineDataPaths(form.dataPath, fieldID) 
                                            : fieldID,
                        currentValue = isc.DynamicForm._getFieldValue(prop, null, this.values),
                        undef;
                    if (currentValue === undef) {
                        isc.DynamicForm._saveFieldValue(fullDataPath, null value, this.values);
                    }
                    */
                } else {
                    
                    // this will recursively iterate into objects until it reaches a dataPath with
                    // an associated item, or an atomic value which we can store directly
                    this._findItemlessFormValues(form, value, dataPath, itemlessValues, true);
                }
            }
        }
        
        if (!dontWarn && itemlessValues.length > 0) {
            this._itemlessValueWarning(form, itemlessValues);
        }
    },
    //<DEBUG
    
    //> @method valuesManager.addMembers()   
    //  Add multiple new member forms to this valuesManager.
    // @param   members  (Array of DynamicForm) array of forms to add to this valuesManager as members.
    // @group members
    // @see method:ValuesManager.addMember
    // @visibility external
    //<        
    addMembers : function (members) {
        if (!isc.isAn.Array(members)) this.addMember(members);
        else {
            for (var i = 0; i< members.length; i++) {
                this.addMember(members[i]);
            }
        }
    },
    
    //> @method valuesManager.removeMember()   
    //  Remove a member form from this valuesManager, so its values are no longer managed
    //  by this instance.
    //  This does not clear the values associated with the form from the valuesManager - they
    //  will still be available via <code>valuesManager.getValues()</code>, but will not be
    //  updated as the form is manipulated.
    // @param   member  (DynamicForm | String)   
    //      form (or ID of form) to remove from this valuesManager
    // @group members
    // @see method:ValuesManager.removeMembers()
    // @visibility external
    //<    
    removeMember : function (member) {
        
        if (isc.isA.String(member)) {
            member = isc.Class.getArrayItem(member, this.members);
            if (member == null) return;
        } else if (this.members && !this.members.contains(member)) return;
        
        if (this.members) this.members.remove(member);
        delete member.valuesManager;
    },
    
    //> @method valuesManager.removeMembers()   
    // Remove multiple member forms from this valuesManager.
    // @param members (Array of DynamicForm) array of forms to remove
    // @group members
    // @see method:ValuesManager.removeMember()
    // @visibility external
    //<    
    removeMembers : function (members) {
        if (!isc.isAn.Array(members)) this.removeMember(members);
        else {
            for (var i = 0; i< members.length; i++) {
                this.removeMember(members[i]);
            }
        }    
    },


    // ----------------------------------------------------------------------------------------
    // Display
    // ----------------------------------------------------------------------------------------
    // valuesManagers don't usually display their values directly - but support
    // getPrintHTML() so we can build reports from them.
    getPrintHTML : function () {
        var values = this.getValues(),
            printHTML = isc.StringBuffer.create();

        printHTML.append("<TABLE border=1><TR><TD align='center' style='font-weight:bold;'>Field</TD>",
                         "<TD align='center' style='font-weight:bold;'>Value</TD>");
        for (var fieldName in values) {
            printHTML.append("<TR><TD>",fieldName,"</TD><TD>", values[fieldName], "</TD></TR>");
        }
        printHTML.append("</TABLE>");
        return printHTML.release(false);
    },
    
    // recursively find all DataBoundComponents anywhere under any component bound to this VM
    getAllDBCs : function (child) {

        var dbcs = [];
    
        if (child == null) {
            for (var i = 0; i < this.members.length; i++) {
                dbcs.addAll(this.getAllDBCs(this.members[i]));
            }
            // It doesn't seem likely that this process could have created duplicates (generally
            // speaking, children can only belong to one parent), but let's get rid of any 
            // just in case
            var unique = [];
            for (var i = 0; i < dbcs.length; i++) {
                if (!unique.contains(dbcs[i])) unique.add(dbcs[i]);
            }
            return unique;
        }
                
        if (isc.isA.DataBoundComponent(child)) dbcs.add(child);
        var children = child.children;
        if (!children) return dbcs;

        for (var i = 0; i < children.length; i++) {
            var child = children[i];
            if (isc.isA.DataBoundComponent(child)) dbcs.add(child);
            dbcs.addAll(this.getAllDBCs(child));
        }
        return dbcs;
    }
        
});



isc.ValuesManager.registerStringMethods ({

     //> @method valuesManager.handleHiddenValidationErrors (A)
    // Method to display validation error messages for a valuesManager when there is not
    // currently visible form item displaying the errors.
    // This will be called when validation fails for<br>
    // - a field in a hidden or undrawn member form<br>
    // - a hidden field in a visible member form<br>
    // - for databound ValuesManagers, a datasource field with specified validators, but not
    //   associated item in any member.<br>
    // Implement this to provide custom validation error handling for these fields.<br>
    // By default hidden validation errors will be logged as warnings in the developerConsole.
    // Return false from this method to suppress that behavior.
    // @param   errors (object) The set of errors returned - this is an object of the form<br>
    //                      &nbsp;&nbsp;<code>{fieldName:errors}</code><br>
    //                      Where the 'errors' object is either a single string or an array
    //                      of strings containing the error messages for the field.
    // @return (boolean) false from this method to suppress that behavior
    // @visibility external
    //<
    handleHiddenValidationErrors:"errors",
    
    //>	@method valuesManager.submitValues()
    // Optional +link{group:stringMethods, StringMethod} to fire when +link{valuesManager.submit()} is called
    // on this valuesManager (or any form included in this valuesManager).
    // 
    // @param	values    (object)        the valuesManager values
    // @param	valuesManager      (ValuesManager)   the valuesManager being submitted
    // @group submitting
    // @see method:valuesManager.submit()
    // @visibility external
	//<
    submitValues : "values,valuesManager"
});

//!<Deferred
