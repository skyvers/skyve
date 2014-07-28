/*
 * Isomorphic SmartClient
 * Version v9.1p_2014-03-26 (2014-03-26)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */

 




isc.Canvas.addMethods({
    // create a DSRequest object in the context of a dataBound component, by gathering
    // various component-level settings that affect how an operation is performed, and 
    // creating a dsRequest object (called a "context" for legacy reasons)
    buildRequest : function (context, operationType, callback) {
		if (!context) context = {};

        // "afterFlowCallback": supported by the DataSource system, this is a second callback
        // that fires after dsRequest.callback.  This is a convenience for action methods which
        // need to call a basic dataSource method (eg ds.updateData()) and get called back to
        // do post-processing and *also* need to provide a callback to the end developer.
        // "afterFlowCallback" fires after the callback passed to dataSource methods like
        // addData().
        if (callback) context.afterFlowCallback = callback;
        
        // text match style: 
        // - support the legacy "filter" operationType.
        // - pick up setting on ListGrids and DynamicForms
        if (operationType == "filter") {
            operationType = "fetch";
            // If textMatchStyle was explicitly specified respect it - otherwise mark
            // as a substring match (filter rather than fetch)
            if (context.textMatchStyle == null) context.textMatchStyle = "substring";
        }
        if (this.textMatchStyle != null) context.textMatchStyle = this.textMatchStyle;
        
        // support old operationType synonyms
        operationType = isc.DS._getStandardOperationType(operationType);

        // pick up component.dataPageSize and component.dataFetchMode for fetches
        if (this.dataPageSize) context.dataPageSize = this.dataPageSize;
        if (this.dataFetchMode) context.dataFetchMode = this.dataFetchMode;

        // pick up an operation name from the component, according to the type of operation
        // being performed.  NOTE: if context.operation is already specified, give it
        // precedence, as this indicates manual invocation with an explicitly specified
        // operation.
        var operation = context.operationId || context.operation;
        if (operation == null) {
            switch (operationType) {
                // NOTE: "saveOperation", "deleteOperation" and the non-specific
                // "this.operation"  are backCompat
                case "fetch":
                    operation = this.fetchOperation; break;
                case "add":
                    operation = this.addOperation || this.saveOperation; break;
                case "update":
                    operation = this.updateOperation || this.saveOperation; break;
                case "remove":
                    operation = this.removeOperation || this.deleteOperation; break;
                case "validate":
                    operation = this.validateOperation; break;
            }
        }
        context.operation = operation || this.operation;
        
        // If we picked up the operation from component.fetchOperation et al, 
        // update the dsRequest operationId as well. This causes it to be displayed in the
        // Log window properly (and means if anyone checks dsRequest.operationId it'll be what
        // they expect to see).
        if (context.operationId == null) {
            context.operationId = context.operation
        }

        // add component ID
        context.componentId = this.ID;
        return isc.rpc.addDefaultOperation(context, this.dataSource, operationType);
    },
    
    // create a ResultTree dataModel based on the component's current config
    createResultTree : function (criteria, callback, requestProperties, type) {
        

        if (type == null) type = "fetch";
        this._setupResultTreeFetchCallback(requestProperties, callback);

        if (requestProperties == null) requestProperties = {};
        // put together Tree-specific properties for the data model we're auto-creating
        var tree = isc.addProperties({initialData: this.initialData},
                                     this.dataProperties,
                                     requestProperties.dataProperties
                                     //>!BackCompat 2006.10.19
                                     // Provide back-compat support for 'treeProperties' 
                                     ,this.treeProperties,
                                     requestProperties.treeProperties
                                     //<!BackCompat
                                     );
        // If the properties object had a specified ID, drop it.
        // This method can be called repeatedly and we don't want these generated
        // instances clobbering each other
        
        delete tree.ID;
        tree.criteria = criteria;
        if (requestProperties.operation != null) {
            tree.operation = requestProperties.operation;
        }
        tree.context = requestProperties;
        tree.dataSource = this.dataSource;
        tree.componentId = this.ID;
        // mark as autoCreated so it gets auto-destroyed, removing DS<->RS links
        tree._autoCreated = true;
        // pick up load data on demand for TreeGrids
        if (this.loadDataOnDemand != null) tree.loadDataOnDemand = this.loadDataOnDemand;
        // pick up filter settings for TreeGrids
        if (this.keepParentsOnFilter != null) tree.keepParentsOnFilter = this.keepParentsOnFilter;
        if (this.dataFetchMode != null) tree.fetchMode = this.dataFetchMode;
        if (this.serverFilterFields != null) tree.serverFilterFields = this.serverFilterFields;
        // copy treeRootValue across
        if (this.treeRootValue != null) tree.rootValue = this.treeRootValue;

        // for multi-DS trees: cross-DS relations
        if (this.treeDataRelations) tree.treeRelations = this.treeDataRelations;
        if (this.multiDSTree != null) tree.multiDSTree = this.multiDSTree;

        if (this.progressiveLoading === true || this.progressiveLoading === false) {
            isc.addProperties(tree, { progressiveLoading: this.progressiveLoading });
        }

        var resultTreeClass = this.getDataSource().resultTreeClass || "ResultTree";
        return isc.ClassFactory.getClass(resultTreeClass).create(tree);
    },

    _setupResultTreeFetchCallback : function (requestProperties, callback) {
        if ( requestProperties == null ) requestProperties = {};
        // The callback is passed in from fetchData() so should be fired when the server
        // responds with the requested nodes.  Hang it onto the request directly so it fires
        // only when that request returns.
        
        var context = requestProperties.internalClientContext = {
            _initialFetchCallback: callback
        };

        // By default when filterData is called the callback passed in is stored as the
        // afterFlowCallback for the request.
        // This is the callback we want to fire after initial fetch.
        // If this wasn't passed in explicitly look it up on the request.
        
        if (requestProperties.afterFlowCallback != null) {
            if (context._initialFetchCallback == null) {
                context._initialFetchCallback = requestProperties.afterFlowCallback;
            } else if (context._initialFetchCallback != requestProperties.afterFlowCallback) {
                
                this.logWarn("createResultTree called with request.afterFlowCallback:" + 
                    this.echo(requestProperties.afterFlowCallback) + 
                    " as well as explicit callback parameter " + 
                    this.echo(callback) + ". The request.afterFlowCallback will not be fired.");
            }
        }
        requestProperties.afterFlowCallback = {target:this, methodName:"_fireFetchCallback"};
    },

    // callback fired whenever we get new data from the server.
    // use this to fire the callback passed into fetchData if there was one [iff the
    // _isInitialFetch flag is present - set up by ResultTree when performing initial fetch only].
    _fireFetchCallback : function (dsResponse,data,dsRequest) {
        var context = dsRequest.internalClientContext;
        if (context && context._isInitialFetch && context._initialFetchCallback != null) {
            var callback = context._initialFetchCallback;
            this.fireCallback(callback, "dsResponse,data,dsRequest", arguments);
        }
    }
});

//>ValuesManager : add buildRequest to ValuesManager, which isn't a Canvas
if (isc.ValuesManager) {
    isc.ValuesManager.addMethods({
        buildRequest : isc.Canvas.getInstanceProperty("buildRequest")
    });
}
//<ValuesManager

// Many flow methods are identical on databound DynamicForms and ValuesManagers.
// Define these methods in an interface to mix into both classes.

isc.ClassFactory.defineInterface("EditorActionMethods");

isc.EditorActionMethods.addInterfaceMethods({
    
    // Editing and Saving
    //-------------------------------------------------------------------------------

    //>!BackCompat 2004.7.23
	save : function (context) { return this.saveData(context) },
	editSelected : function (selectionComponent, context) { 
        return this.editSelectedData(selectionComponent, context) 
    },
	editNew : function (initialValues, context) { 
        return this.editNewRecord(initialValues, context) 
    },
    //<!BackCompat
    
    // NOTE: editNewRecord / editRecord / editSelectedData et al do not go to the server,
    // although in a future implementation of editSelected() it may be necessary to go to the
    // server to get fields not being shown in the selection component.

    //>	@method dynamicForm.editNewRecord()
    //
    // Prepare to edit a new record by clearing the current set of values (or replacing them 
    // with initialValues if specified).
    // <br>
    // This method will also call +link{DynamicForm.setSaveOperationType()} to ensure
    // subsequent calls to <code>saveData()</code> will use an <code>add</code> rather than
    // an <code>update</code> operation.
    //
    // @param [initialValues] (Object | Record)
    //     initial set of values for the editor as a map of field names to their corresponding
    //     values
    // @see dynamicForm.saveData()
    //
    // @group dataBoundComponentMethods
    // @visibility external
    //<
	editNewRecord : function (initialValues) {
        this.setSaveOperationType("add");
		this._editRecord(initialValues);
	},
    
    //>	@method dynamicForm.editRecord()
    //
    // Edit an existing record.  Updates this editors values to match the values of the record 
    // passed in, via +link{setValues()}.
    // <P>
    // This method will also call +link{DynamicForm.setSaveOperationType()} to ensure 
    // subsequent calls to <code>saveData()</code> will use an <code>update</code> rather than
    // an <code>add</code> operation.
    //
    // @param record (Record)
    //     the record to be edited as a map of field names to their corresponding values
    // @see dynamicForm.saveData()
    //
    // @group dataBoundComponentMethods
    // @visibility external
    //<
	editRecord : function (record) {        
        var saveOperationType = (record == null ? "add" : "update");
        this.setSaveOperationType(saveOperationType);
        this._editRecord(record);
	},
    
    _editRecord : function (record) {
        delete this._editRecordNum;
        delete this._editList;
        var record = isc.addProperties({}, record);
        this.setData(record);

        
        if (this.getFileItemForm != null) {
            var fileItemForm = this.getFileItemForm();
            if (fileItemForm) {
                fileItemForm.editNewRecord();
                fileItemForm.clearErrors(true);
            }
        }
    },

    //>	@method dynamicForm.editSelectedData()
    //
    // Edit the record selected in the specified selection component (typically a
    // +link{ListGrid}).
    // <P>
    // Updates the values of this editor to match the selected record's values.
    // <P>
    // If this form has a dataSource, then saving via +link{saveData()} will use the 
    // "update" operation type.
    //
    // @param selectionComponent (ListGrid or ID)
    //     the ListGrid or ID of a +link{ListGrid} whose currently selected
    //     record(s) is/are to be edited
    // @see dynamicForm.saveData()
    //
    // @group dataBoundComponentMethods
    // @visibility external
    // @example updateOperation
    //<
    editSelectedData : function (selectionComponent) {
        // support being passed an ID
        if (isc.isA.String(selectionComponent)) selectionComponent = window[selectionComponent];
        if (!selectionComponent) return;
        
        var selection = selectionComponent.selection.getSelection();
        // Duplicate and clean the selection before editing
        if (selection && selection.length > 0) {
            var selectionList = [];
            for (var i = 0; i < selection.length; i++) {
                selectionList[i] = selectionComponent.getCleanRecordData(selection[i]);
            }
            this.editList(selectionList);
        }
	},			

	editList : function (recordList) {
        this.setSaveOperationType("update");
		this._editRecords(recordList);
	},
    
    // actually start editing a recordList (start with the first record in the list) 
    _editRecords : function (recordList) {
        this._editRecordNum = 0;
        this._editList = recordList;
        var record = isc.addProperties({},recordList[this._editRecordNum]);
        this.editRecord(record);
    },
    editNextRecord : function () {
        this.editOtherRecord(true);
	},

   	editPrevRecord : function () {
        this.editOtherRecord(false);
	},

    editOtherRecord : function (next) {
        // not valid if we never had a call to editList.
        if (!this._editList) return;
        
		if (this.isVisible() && this.valuesHaveChanged()) {
            // remember whether we're editing next or previous
            this._next = next;

            this.saveData({target:this, methodName:"editOtherReply"});
            return;
        };
        
        if (next && this._editRecordNum >= this._editList.length-1) {
            this.logWarn("Unable to edit next record - this is the last selected record");
            return false;
        }
        if (!next && this._editRecordNum <= 0) {
            this.logWarn("Unable to edit previous record - this is the first selected record");
            return false;
        }
        this._editRecordNum += (next ? 1 : -1);
        var nextRecord = isc.addProperties({}, this._editList[this._editRecordNum]);
        this.setData(nextRecord);
    },
	// reply to the 'save editor' call
	editOtherReply : function (response, data, request) {
		
        var next = this._next;
        delete this._next;
        
		// error occurred: the presence of response.errors indicates it's a validation error, which
		// we can handle.  Note we assume the developer meant a validation error if
        // response.errors was provided, regardless of whether you set the correct validation
        // error status code.
		if (response.status < 0 && response.errors) {
			return this.setErrors(response.errors, true);
		}

		// some error we weren't expecting occurred, bail with an error dialog
		if (response.status < 0) return isc.RPCManager._handleError(response, request);
        
        // remember the values in the form and in the _editlist...
        this.rememberValues();
        this._editList[this._editRecordNum] = this.getValues();
        
        // call editOther - to show the next record
        this.editOtherRecord(next)
		return true;
	},
    
    
    //> @method dynamicForm.validateData()
    //
    // Perform validation on the client and the server.
    //
    // @param [callback] (DSCallback) callback to invoke on completion
    // @param [requestProperties] (DSRequest Properties) additional properties to set on
    //                                                   the DSRequest that will be issued
    //
    // @group validation
    // @visibility external
    //<
    validateData : function (callback, requestProperties) {
        // do simple client side validation first 
        if (!this.validate()) return false;
        var values = this.getValues();
        
        // validate the data on the server
        // Note: As written, if a callback is not supplied we use 'saveDataReply' which will
        // redraw the form to show errors - otherwise we rely on the callback to apply errors
        // to the form.
        var context = this.buildRequest(requestProperties, "validate");
        context.editor = this;

        // valuesAsParams - also sends the DSRequest values as request parameters
        if (context.valuesAsParams) {
            if (!context.params) context.params = {};
            isc.addProperties(context.params, values);
        }

        var dataSource = this.getDataSource();
        return dataSource.validateData(
                   values, 
                   callback ? callback : {target:this, methodName:"saveEditorReply"}, 
                   context
               );
    },

    //>	@method	dynamicForm.reset()   ([])
    //
    // Resets values to the state it was the last time <code>setValues()</code> or
    // <code>rememberValues()</code> was called. If neither of those methods has been called,
    // values will be set back to their initial values at init time.
    //
    // @group formValues
    // @visibility external
    //<
    reset : function () {
        this.resetValues();
    },

    //> @method dynamicForm.cancel() ([])
    //
    // This method exists for clean integration with existing server frameworks that have a 'cancel'
    // feature which typically clears session state associated with the form.  When this method is
    // called, an RPC is sent to the server with a parameter named
    // +link{attr:DynamicForm.cancelParamName} with the value
    // +link{attr:DynamicForm.cancelParamValue}.<p>
    //
    // Note that no other form data is sent.  By default the current top-level page is replaced with the
    // reply.  If you wish to ignore the server reply instead, call this method like this:
    // <pre>
    // dynamicFormInstance.cancel({ignoreTimeout: true, target: null});
    // </pre>
    // @see dynamicForm.cancelEditing()
    // @param [requestProperties] (DSRequest)   additional properties to set on the RPCRequest
    //                                          that will be issued
    // @group submitting
    // @visibility external
    //<
    cancel : function (requestProps) {
        var cancelRPC = {
            actionURL: this.action,
            target: window,
            sendNoQueue: true,
            ignoreTimeout: true,
            useXmlHttpRequest: false,
            params: { }, // set below
            useSimpleHttp: true
        };
        cancelRPC.params[this.cancelParamName] = this.cancelParamValue;

        //>DEBUG
        if (!cancelRPC.actionURL) {
            this.logWarn("No actionURL defined for the cancel RPC - set 'action' on your form or"
                         + "provide an actionURL in the requestProperties to cancel()");
            return;
        }
        //<DEBUG

        isc.addProperties(cancelRPC, requestProps);
        
        isc.rpc.sendRequest(cancelRPC);
    },

    //> @attr dynamicForm.userTask (UserTask : null : IR)
    // Associated userTask if current dynamic form used along with workflow. 
    // See +link{UserTask, userTask} for more details.
    // @visibility workflow
    //<

    //> @method dynamicForm.cancelEditing() ([])
    // If the form or valuesManager has associated userTask workflow task than notify it about
    // cancelling the changes.  
    // @visibility external
    //<
    cancelEditing : function () {
        if (this.valuesManager != null) {
            this.valuesManager.cancelEditing();
        }
        if (this.userTask != null) {
            this.userTask.cancelEditing();
        }
    },

    //> @method dynamicForm.completeEditing() ([])
    // Finish editing and store edited values in +link{Process.state,process state}.  
    // @visibility external
    //<
    completeEditing : function () {
        if (this.valuesManager != null) {
            this.valuesManager.completeEditing();
        }
        if (this.userTask != null) {
            this.userTask.completeEditing();
        }
    },

    //>	@method	dynamicForm.submit()
    // <code>submit()</code> is automatically called when a +link{SubmitItem} included in the
    // form is clicked, or, if +link{dynamicForm.saveOnEnter,saveOnEnter} is set, when the
    // "Enter" key is pressed in a text input.  Submit can also be manually called.
    // <P>
    // If this form is part of a +link{valuesManager}, this method will simply fall through to
    // the submit method on the valuesManager. If not, and
    // +link{dynamicForm.submitValues(),form.submitValues()} exists, it will be called, and
    // no further action will be taken.
    // <P>
    // Otherwise, default behavior varies based on +link{dynamicForm.canSubmit,form.canSubmit}: if
    // <code>canSubmit</code> is false, +link{method:dynamicForm.saveData()} will be called to
    // handle saving via SmartClient databinding.  
    // <P>
    // If <code>canSubmit</code> is true, the form will be submitted like an ordinary HTML
    // form via +link{dynamicForm.submitForm()}.
    // <P>
    // The parameters to <code>submit()</code> apply only if <code>submit()</code> will be
    // calling +link{saveData()}.  If you override <code>submit()</code>, you can safely
    // ignore the parameters as SmartClient framework code does not pass them.
    // 
    // @param [callback]          (DSCallback)  callback to invoke on completion.
    //                                          [Ignored if this.canSubmit is true]
    // @param [requestProperties] (DSRequest)   additional properties to set on the DSRequest
    //                                          that will be issued
    //                                          [Ignored if this.canSubmit is true]    
    // @group dataBoundComponentMethods
    // @see method:dynamicForm.submitValues()
    // @visibility external
    //<
    submit : function (callback, requestProps) {
	if (this.saveToServer == false) return;

        if (this.valuesManager != null) {
            return this.valuesManager.submit(callback, requestProps);
        }
        if (this.submitValues != null) {
            return this.submitValues(this.getValues(), this);
        }
        if (this.canSubmit) return this.submitForm();
        else return this.saveData(callback, requestProps);
    },

    // returns true if calling saveData() will perform the add operation, false otherwise.
    // Note that the operationType can be overridden by the optional requestProperties arg to
    // saveData.
    
    saveOperationIsAdd : function () {
        if (this.saveOperationType) return this.saveOperationType == "add";
    
        if (this.dataSource) {
            var ds = isc.DataSource.getDataSource(this.dataSource);
            return !ds.recordHasAllKeys(this.getValues());
        }
        return false;
    },

    //> @attr dynamicForm.saveToServer (boolean : null : IRWA)
    // Setting <code>saveToServer</code> to false disables the functionality of a
    // +link{method:submit()} call on the form. This is useful when defining a form based on
    // a +link{DataSource} but the normal submit action (typically from a +link{SubmitItem})
    // should be bypassed. 
    //<
    

    
    //> @attr dynamicForm.suppressValidationErrorCallback (Boolean : false : IRWA)
    // When calling +link{saveData()} on a form or valuesManager, by default if the server
    // returns an error code, any callback passed into saveData() will not be fired.
    // If the error code returned by the server indicates a validation error, it will be
    // displayed to the user by updating the form items to show the error messages, and firing
    // any specified hiddenValidationErrors handler, otherwise the standard RPCManager
    // error handling logic would be invoked.
    // <P>
    // Developers who want to handle errors themselves can override this default by specifying
    // +link{rpcRequest.willHandleError,dsRequest.willHandleError} on the DSRequest. In this case the callback passed in 
    // will be fired even if the server returns an error status code.
    // <P>
    // If <code>suppressValidationErrorCallback</code> is set to true, if a save attempt returns
    // a <i>validation</i> error code, the user-specified callback will not be fired
    // <i>even if <code>willHandleError:true</code> was specified on the dsRequest</i> 
    // - though for other error codes, the callback would be fired if willHandleError is 
    // specified on the request.
    // Note that this is the historical behavior for
    // <smartclient>SmartClient builds 8.0 and earlier</smartclient>
    // <smartgwt>SmartGWT builds 4.0 and earlier</smartgwt>
    // @visibility external
    //<
    
    //>	@method dynamicForm.saveData()
    //
    // Validate and then save the form's current values to the +link{DataSource} this form is
    // bound to.
    // <p>
    // If client-side validators are defined, they are executed first, and if any errors are
    // found the save is aborted and the form will show the errors.
    // <p>
    // If client-side validation passes, a +link{DSRequest} will be sent, exactly as though
    // +link{dataSource.addData()} or +link{dataSource.updateData()} had been called with 
    // +link{dynamicForm.getValues(),the form's values} as data.  The
    // +link{dsRequest.operationType} will be either "update" or "add", depending on the
    // current +link{DynamicForm.saveOperationType}.
    // <P>
    // On either a client-side or server-side validation failure, validation errors will be
    // displayed in the form.  Visible items within a DynamicForm will be redrawn to display
    // errors. Validation failure occurring on hidden items, or DataSource fields with no 
    // associated form items may be handled via +link{DynamicForm.handleHiddenValidationErrors}
    // or +link{ValuesManager.handleHiddenValidationErrors}.
    // <P>
    // In the case of a validation error, the callback will <b>not</b> be called by default
    // since the form has already handled the failed save by displaying the validation errors
    // to the user.  If you need to do something additional in this case, you can set
    // +link{rpcRequest.willHandleError} via the <code>requestProperties</code> parameter to
    // force your callback to be invoked.  However, first consider:
    // <ul>
    // <li> if you are trying to customize display of validation errors, there are several
    // +link{dynamicForm.showErrorIcons,built-in modes} and +link{dynamicForm.showErrors()} may be a better
    // place to put customizations.
    // <li> for unrecoverable general errors (eg server is down),
    // +link{RPCManager.handleError,central error handling} in invoked, so consider placing
    // customizations there unless an unrecoverable error should be handled specially by this
    // specific form.
    // </ul>
    //
    // @param [callback]          (DSCallback)  callback to invoke on completion
    // @param [requestProperties] (DSRequest)   additional properties to set on the DSRequest
    //                                            that will be issued
    //
    // @group dataBoundComponentMethods
    // @visibility external
    // @example addOperation
    //<
    // NOTE: not documenting direct submit (If the editor is a multi-part encoded dynamicForm,
    // a direct submit will be performed.)
    saveData : function (callback, requestProperties, noValidation) {
        // If we have a 'selectionComponent', just tell it to pick up the changes and display
        // them
        if (this.selectionComponent != null) {
            var pks = this._selectionComponentRecordPKs;
            // check for 'setRecordValues' is a sanity check only - we should only allow
            // binding to components where this is supported.
            if (pks && this.selectionComponent.setRecordValues) {
                this.selectionComponent.setRecordValues(pks, this.getValues());
            }
            return;
        }
        
        if (this.dataSource == null) {
            this.logWarn("saveData() called on a non-databound " + this.Class + ". This is not supported. " +
                       " for information on databinding of components look at the documentation" +
                       " for the DataSource class.  " +
                       "If this was intended to be a native HTML form submission, set the " +
                       "canSubmit property to true on this form.");
            return;
        }

        // If both valuesManager and dataPath are set, we don't want to attempt to save this 
        // form's data direct - we should just update the ValuesManager
        if (isc.ValuesManager && isc.isA.ValuesManager(this.valuesManager) && this.dataPath) {
            var fields = this.getFields();
            for (var i = 0; i < fields.length; i++) {
                var fieldId;
                if (fields[i].getTrimmedDataPath) {
                    fieldId = fields[i].getTrimmedDataPath();
                }
                if (fieldId == null) {
                    fieldId = fields[i].dataPath || fields[i].name;
                }
                var value = this.getValue(fieldId);
                this.valuesManager._updateValue(fieldId, value, this);
            }
            return;
        }

        if (isc.Offline && isc.Offline.isOffline() && !this.dataSource.clientOnly) {
            isc.warn(this.offlineSaveMessage);
            return;
        }
       
        //>!BackCompat 2005.3.21 old signature: criteria, requestProperties
        if (requestProperties == null && isc.isAn.Object(callback) && 
            callback.methodName == null) 
        {
            // old style call, second param (callback) is really requestParams
            requestProperties = callback;
            callback = requestProperties.afterFlowCallback;
        } //<!BackCompat

        if (requestProperties == null) requestProperties = {};

        var operationType = this.getSaveOperationType(requestProperties);

        // send oldValues to allow long transactions
        // this._oldValues is set up on dynamicForms in rememberValues(), so represents
        // the data when editRecord() or setValues() was called.
        // Assume this is the underlying record object we're editing unless this is
        // an "add" type operation (in which case it was probably passed in as part of
        // editNewRecord(...))
        
        if (!requestProperties.oldValues) {
            requestProperties.oldValues = operationType != "add" ? this._oldValues : {};
        }
        
        // do server validation if validationURL is specified
        if (this.validationURL && !noValidation) {
            var validateProps = {};
            isc.addProperties(validateProps, requestProperties);
            isc.addProperties(validateProps, {
                actionURL: this.validationURL,
                valuesAsParams: true,
                sendNoQueue: true
            });
            validateProps._userProps = requestProperties;
            validateProps._userCallback = callback;            

            // set a special flag to prevent the validation run called by saveData() from
            // clearing any validation errors currently visible in the form if client-side
            // validation succeeds.  When we're in validationURL mode, typically the server
            // will be supplying these errors and it looks lame when the form redraws between
            // validation submits.
            this.performingServerValidation = true;

            this.validateData(this.getID()+"._saveFormValidateCallback(rpcRequest,rpcResponse,data)",
                              validateProps);
            return;
        }
        // If we're showing a fileItem, we'll have to perform a native commit of the fileItemForm
        // rather than submitting our values in the normal way
        var fileItemForm = this.getFileItemForm();
        // Note: if fileItemForm is undrawn we can't commit it!
        if (fileItemForm && fileItemForm.isDrawn()) {
            this.updateFileItemForm();
            // validators are not duplicated on the fileItem form so explicitly validate now
            if (!this.validate()) return false;
            return fileItemForm.saveData(callback, requestProperties, noValidation);
        }
   
        // hold on to end user callback, and pass our own to the RPC layer.  We do this to
        // provide the formSaved() mechanism that fires before the end user callback.
        this._userCallback = callback;
      
        callback = this.getID()+"._saveDataReply(dsRequest, dsResponse, data)";
		requestProperties = this.buildRequest(requestProperties, operationType, callback);

        // if the form specified an action different from the default, use it as the RPC target
        var doSubmit = false;
        
        if (this.submitParamsOnly) requestProperties.useSimpleHttp = true;

        // If this form has an explicitly specified action, submit to the main window rather than
        // to a hidden frame
        if (isc.DynamicForm && isc.isA.DynamicForm(this)) {
            if (this._explicitAction) {
                requestProperties.actionURL = this.action;
                requestProperties.target = this.target ? this.target : window;
                doSubmit = true;
            }   
    
            // override the httpMethod on the request if the user specified a custom 'method'
            if (this.method != isc.DynamicForm.getInstanceProperty("method")) {
                requestProperties.httpMethod = this.method;
            }
        }
        
        
        // If the disableValidation flag is set - clear errors before saving - server side validation
        // can still occur and will display new errors if appropriate.
        if (this.disableValidation) this.clearErrors(true);
        else {
            if (!this.validate(null, null, null, null, true)) return false
        }
        
        if (this.rulesEngine) {
            var rulesValidationResult = this.rulesEngine.processSubmit(this);
            // Rules are technically validators so may fail. If so suppress submission.
            if (rulesValidationResult == false) return;
        }
        
        // duplicate the values before passing them to save/submit method.
        // This avoids any potential issues where downstream code points to the actual live
        // values object
        
        var values = this.getDataSource()._cloneValues(this.getValues());
        
        // perform a direct submit if the form is multipart-encoded
        
        if ((isc.DynamicForm && isc.isA.DynamicForm(this) && this.isMultipart())
            || this.canSubmit || doSubmit)
        {
            return this.submitEditorValues(values, requestProperties.operation, 
                                           requestProperties.callback, requestProperties);
        } else {
            return this.saveEditorValues(values, requestProperties.operation,
                                         requestProperties.callback, requestProperties);
        }

	},
    
    //> @attr dynamicForm.selectionComponent (DataBoundComponent : null : IRW)
    // May be set to a databound component which displays multiple records and supports
    // selection, such as a ListGrid or TileGrid.<br>
    // If set, the values in this form will be automatically updated on selection change in
    // the selection component to display the (first) selected record's values, and 
    // +link{dynamicForm.saveData()} will update the record displayed in the selectionComponent
    // even if no dataSource is specified.
    // @visibility selectionComponent
    //<
    
    //> @method dynamicForm.setSelectionComponent()
    // setter for +link{dynamicform.selectionComponent}
    // @param component (ID | canvas) new selection component
    //< 
    // NOTE: Moved this method up to DataBoundComponent because it is required for ListGrids, 
    // DetailViewers, etc.  The partner methods selectionComponentSelectionChanged() and 
    // selectionComponentCellSelectionChanged() need DynamicForm-specific implementations, 
    // but they had to be moved from here (this is interface EditorActionMethods) and 
    // implemented directly on DynamicForm, because interface methods do not override base 
    // class impl's and hence we were always picking up the DBC versions of those two.
    
    
    // Helper method to prepare the fileItemForm for submission
    updateFileItemForm : function () {
        var fileItemForm = this.getFileItemForm();
        if (fileItemForm == null) return;
        // clear old values and update with our values
        var oldVals = fileItemForm.getValues(),
            vals = this.getValues(),
            uploadField = fileItemForm.getItem(0).getFieldName();
        for (var fieldName in oldVals) {
            if (fieldName == uploadField) continue;
            // set to explicit null rather than just clearValue
            
            fileItemForm.setValue(fieldName, null);
        }
        for (var fieldName in vals) {
            if (fieldName == uploadField) continue;
            fileItemForm.setValue(fieldName, vals[fieldName]);
        }
            
        // ensure action and datasource match
        
        if (this._explicitAction) fileItemForm.setAction(this.action);
        // Note: don't use setDataSource - we don't want to modify the set of fields on the
        // fileItemForm
        fileItemForm.dataSource = this.dataSource;
        fileItemForm.fileItemFormParentForm = this;
    },
    
    //> @attr dynamicForm.saveOperationType (DSOperationType : null : IRW)
    // Default +link{DSOperationType} to be performed when +link{DynamicForm.saveData()} is called.
    // This property is automatically set on a call to +link{DynamicForm.editRecord()} or
    // +link{DynamicForm.editNewRecord()}, or may be set directly via 
    // +link{DynamicForm.setSaveOperationType()}.
    // <P>
    // If <code>saveOperationType</code> is unset, the form will heuristically determine
    // whether an "add" or "update" operation is intended based on whether the primaryKey field
    // is present and editable.
    //
    // @visibility external
    // @getter getSaveOperationType()
    //<

    //> @method dynamicForm.isNewRecord()
    // Returns true if +link{saveOperationType} is currently "add".  See
    // +link{saveOperationType}.
    //
    // @return (Boolean) whether this form will use an "add" operation when saving
    // @visibility external
    //<
    isNewRecord : function () {
        return this.getSaveOperationType() == "add";
    },
    
    //> @method dynamicForm.setSaveOperationType()
    // Setter for the default +link{DSOperationType} when +link{DynamicForm.saveData()} is called.
    // Note that this property can also be set by calling +link{DynamicForm.editRecord()} or 
    // +link{DynamicForm.editNewRecord()}
    //
    // @param operationType (DSOperationType) Operation type to use as a default. Valid values are
    //  <code>"add"</code> or <code>"update"</code>.
    // @visibility external
    //<
    setSaveOperationType : function (operationType) {
        this.saveOperationType = operationType;
    },
    
    //> @method dynamicForm.getSaveOperationType()
    // Returns the +link{DSOperationType} to be performed when +link{DynamicForm.saveData()} is
    // called. Valid options are <code>"add"</code> or <code>"update"</code>.
    // <P>
    // If a +link{DSRequest} configuration object is passed in containing an explicit operationType
    // this will be returned. Otherwise +link{DynamicForm.saveOperationType} will be returned.
    // This attribute is automatically set via calls to data binding methods such as
    // +link{dynamicForm.editNewRecord}, or it may be set explicitly.
    // <P>
    // If no explicit saveOperationType is specified for this form, the system will 
    // look at the current values for the form. If the form has no value for
    // the +link{dataSource.getPrimaryKeyField(),primaryKey field}, or that field is
    // editable and has been modified we assume an add operation, otherwise an update.
    // If the form is a member of a +link{valuesManager}, the primary key field value
    // will be derived from the valuesManager's values object.
    //
    // @param [requestProperties] (DSRequest Properties) Optional DSRequest config block for the
    //  save operation
    // @return (DSOperationType) Operation type for the save request.
    // @visibility external
    //<
    getSaveOperationType : function (requestProperties) {

        var operationType;
        // If no operation was passed in, we're going to have to auto generate one.
        if (!requestProperties || !requestProperties.operation) {
            // Simplify code below to not require null checks
            if (!requestProperties) requestProperties = {};
            // Insert or Update?
            // If the operationType is not passed in, use this.saveOperationType (set by
            // editNew et al)
            operationType = requestProperties.operationType
                             ? requestProperties.operationType 
                             : this.saveOperationType;
            // If the saveOperatonType wasn't explicitly provided base it on whether the
            // primary keys for the record are present and whether they've been modified
            
            if (!operationType && this.dataSource != null) {

                var pkFields = isc.DataSource.getDataSource(this.dataSource).getPrimaryKeyFieldNames(),
                    values = this.getValues(),
                    undef;
                // If we have a valuesManager and we're editing a flat data object, we want
                // to return the operationType if the developer were to call 
                // saveData() on the VM, rather than on this form directly.
                if (this.valuesManager && this.dataPath == null) {
                    values = this.valuesManager.getValues();
                }

                for (var i = 0; i < pkFields.length; i++) {
                    var key = pkFields[i],
                        value = values[pkFields];
                    if (value == null) {
                        //this.logWarn('saveData(): has no value for a primary key field:' + key 
                        //              + ', assuming this is an add (pk will be genereated by server)');                    
                        operationType = "add";
                        break;
                    }
                    // checking _oldValues will catch the case where setValues() or 
                    // editRecord() [ultimately rememberValues]
                    // was called and passed a value for a field which has subsequently
                    // been modified
                    if (this._oldValues[key] !== undef && this._oldValues[key] != value) {
                        //this.logWarn("saveData(): primary key field:" + key + " has been modified" +
                        //             " assuming this is an add operation");
                        operationType = "add";
                    }

                    var item = this.getItem(key);
                    if (item && item.isVisible() && (item.shouldSaveValue && item.isEditable())) {
                        //this.logWarn("saveData(): value for primary key is visible and editable - assuming this is an add");
                        operationType = "add";
                        break;
                    }
                }
                // In this case, every primary key is present and either
                // - doesn't have a form item
                // - the form item is not editable (or not being saved out)
                // So we assume it's unchanged, making this an update of an existing record
                if (operationType == null) {
                    //this.logWarn("saveData(): all primary key fields are present for the record, " + 
                    //             "and not editable / edited, so assuming this is an update operation");        
                    operationType = "update";
                }
            }
        }

        
        return operationType;
    },

    // form.saveData() internal callback
    _saveDataReply : function (request, response, data) {
        
        // If a form contains a FileItem, the request submitted to the server originates from 
        // an inner form that wraps the native "upload" component in order to prevent redraws 
        // from clearing the upload value.  For this reason, the callback to sync the server's 
        // response data will be invoked on this inner form, which is meaningless.  Therefore,
        // intercept this case and hand the callback to the parent form.
        if (isc.isA.DynamicForm(this.fileItemFormParentForm)) {
            this.fileItemFormParentForm._saveDataReply(request, response, data);
        }

        // this var keeps the index of the next formItem that we need to call formSaved() on.
        this._formSavedIndex = 0;
        
        // If 'data' is passed back from the server, update this.values with the saved data, except
        // for fields that have subsequently been further updated
        // Exceptions 
        // - provide a non obfuscated flag to suppress this data synch
        // - If the server threw an error the data object may be a simple error message
        
        if (!this.suppressServerDataSync && response && response.status >= 0 && data != null) {
            if (isc.isAn.Array(data)) data = data[0];

            
            if (request.originalData) request.originalData =isc.shallowClone(request.originalData);
            if (request.data) request.data = isc.shallowClone(request.data);

            // Note: if request.originalData is present, use this rather than request.data
            // This handles the case where request.data may have been reformatted / modified before
            // sending to the server
            // [For example see restDataSource / postMessage dataProtocol where request.data will
            //  be a serialized block of XML]
            // request.originalData matches the values as it was retrieved from the form when
            // the save was kicked off.
            // For iscServer operations use request.data
            // - this object will already be in the "standard" format, and we don't save off
            //   request.originalData in this code-path 
            var submittedValues =(request.originalData || request.data),
                currentValues = this.getValues();
                
            var hasChanges = false,
                rememberValues = true,
                undef;
            // apply per-field changes from submitted to server-saved values to the values object.
            for (var i in data) {
                // If the value for this field is undefined in the submitted data, that probably
                // means it was stripped by the sparseUpdates logic, so we can't compare it to 
                // the current value.  However, we can compare it to the corresponding member of
                // _oldValues - the fact that it was stripped by sparseUpdates means that it was
                // unchanged, so if it is different now, it has changed since we sent the update
                // to the server
                var compareVal = submittedValues[i] === undef ? this._oldValues[i] : submittedValues[i];
                var field = this.getField(i);
                // check whether the form item has changed since submission
                if (this.fieldValuesAreEqual(field, currentValues[i], compareVal)) {
                    // if not, check whether the server changed the submitted value to
                    // something else
                    if (!this.fieldValuesAreEqual(field, compareVal, data[i])) {
                        currentValues[i] = data[i];
                        hasChanges = true;
                    }
                
                } else {
                    // value in the form has changed since being submitted
                    rememberValues = false;
                }
            }
            if (hasChanges) {
                // apply changed field values from data directly to this.values
                
                this._saveValues(currentValues);
            }
            // Loop through all the items and update them to reflect the changed values.
            // note: we can't just use the attribute names from 'data' - dataPaths applied
            // to items mean we may be reaching into a nested object on the response.
            // We also use this loop to determine whether any changes have been made to items
            // since submission (for fields that weren't present in the submitted values object)
            for (var i = 0; i < this.items.length; i++) {
                var item = this.items[i];
                if (isc.isAn.UploadItem(item)) continue;

                var path = item.dataPath || item.name;
                if (path == null) continue;
                
                var submittedVal = isc.DynamicForm._getFieldValue(path, item, submittedValues, this, true);
                
                if (submittedVal === undef || 
                    this.fieldValuesAreEqual(item, submittedVal, item.getValue())) 
                {
                    if (hasChanges) {
                        var serverVal = isc.DynamicForm._getFieldValue(path, item, data, this, true);
                        
                        if (!this.fieldValuesAreEqual(item, submittedVal, serverVal)) {
                            item.setValue(serverVal);
                        }
                    }
                } else {
                    rememberValues = false;
                }
            }
            
            // When the user modifies the values in the form before saving them
            // 'valuesHaveChanged' will return false.
            // Once the save actually completes, if the user hasn't further edited the values,
            // we're effectively editing the (unchanged) record again. At this point
            // re-remember values so valuesHaveChanged returns true.
            if (rememberValues)  {
                this.rememberValues();
            }
            
            // If this was a save operation, drop the currently specified saveOperationType now
            // if the response included primary key data for the newly added record we're now
            // updating an existing record. We already have logic to catch this case in
            // getSaveOperationType().
            if (this.saveOperationType == "add") delete this.saveOperationType;
        }
        
        this._callbackState = {
            request: request,
            response: response,
            data: data
        };
        this.formSavedComplete();
        
        
        var transactionItem = this.getItem("_transaction");
        if (transactionItem && isc.isA.HiddenItem(transactionItem)) {
            transactionItem.clearValue();
        }

    },
    
    // notify every FormItem that the form has finished saving.  Used to allow items such as
    // the MultiFileItem to save records which are related a newly created record created by
    // saving of the main form.
    formSavedComplete : function () {

        var fields = this.getFields();
        for (var i = this._formSavedIndex; i < fields.length; i++) {
            this._formSavedIndex++;
            var field = fields[i];
            // call formSaved on the formItem, if defined.  If formSaved() returns false, that
            // means it's going to do some async processing and call this method again when
            // complete.
            if (isc.isA.Function(field.formSaved) && 
                field.formSaved(this._callbackState.request, this._callbackState.response, 
                                this._callbackState.data) === false) return;
        }

        // the _userCallback is the original callback specified by the user to saveData().
        // Once we've completed all formSaved() calls, call the user back.
        if (this._userCallback) {
            this.fireCallback(this._userCallback, "dsResponse,data,dsRequest", 
                                            [this._callbackState.response,
                                             this._callbackState.data,
                                             this._callbackState.request]);
        }
        delete this._userCallbackState;
        delete this._userCallback;
    },
    
	// save the given values, displaying any server-side validation errors in the given editor
	saveEditorValues : function (values, saveOperation, callback, context) {
        
        var undefined;
		if (!context) context = {};
		
		isc.addProperties(context, {
			prompt:(context.prompt || isc.RPCManager.saveDataPrompt),
			editor:this
		});
		
	
		// willHandleError will have to be true so we can show validation errors.
		// However if the user didn't already specify this we need to hang onto the original
		// setting so we can fire default error handling
        
        context.internalClientContext = {
            _explicitWillHandleError: context.willHandleError
        };
        context.willHandleError = true;


        // valuesAsParams - also sends the DSRequest values as request parameters
        if (context.valuesAsParams) {
            if (!context.params) context.params = {};
            isc.addProperties(context.params, values);
        }

    	var dataSource = this.getDataSource();
        return dataSource.performDSOperation(
                    saveOperation.type, values,
                    callback ? callback : {target:this, methodName:"saveEditorReply"}, context);

	},
	
	// save the given values via direct submit, displaying any server-side validation errors in
    // the given editor
	submitEditorValues : function (values, saveOperation, callback, context) {
		
		if (!context) context = {};
		isc.addProperties(context, {
            directSubmit : true,
            submitForm : this
        });
        return this.saveEditorValues(values, saveOperation, callback, context);
	},    
    
    // reply to the 'save editor' call
	saveEditorReply : function (response, data, request) {
        if (request.internalClientContext) {
            request.willHandleError = request.internalClientContext._explicitWillHandleError;
        }                           

        // error occurred: the presence of results.errors indicates it's a validation error,
        // which we can handle.  XXX should really check for status == validation error constant
		if (response.status == isc.RPCResponse.STATUS_VALIDATION_ERROR && response.errors) {
            if (isc.isA.FileItem(this.targetItem)) 
                this.parentElement.setErrors(response.errors, true);
            else this.setErrors(response.errors, true);
            // returning false will avoid the end user callback being called
            return this.suppressValidationErrorCallback ? false : 
                    request.willHandleError == true; 
		}
        // Standard error handling
		if (response.status < 0 && !request.willHandleError)
		    return isc.RPCManager._handleError(response, request);
        
        // Return true to fire the callback
		return true;
	},    

    _saveFormValidateCallback : function (rpcRequest, rpcResponse, data) {
        if (rpcResponse.status == isc.RPCResponse.STATUS_SUCCESS) {
            this.performingServerValidation = false;
            this.markForRedraw("serverValidationSuccess");
            this.saveData(rpcRequest._userCallback, rpcRequest._userProps, true);
            rpcRequest._userCallback = null;
            rpcRequest._userProps = null;
        } else {
            this.setErrors(rpcResponse.errors, true);
        }
    }
    
});

if (isc.DynamicForm) {
    isc.ClassFactory.mixInInterface("DynamicForm", "EditorActionMethods");
    
    isc.DynamicForm.addProperties({
    
        // These overrides moved here from EditorActionMethods because there is a base class
        // implementation that will not be overridden by an interface impl
        selectionComponentSelectionChanged : function (selectionComponent, record, state) {
            if (!state) record = {};  // So the form is cleared when selections are cleared
            this._selectionComponentRecordPKs = selectionComponent.getPrimaryKeys(record);
            this.clearErrors(true);
            if (this.valuesManager && this.valuesManager._setMemberValues) {
                this.valuesManager._setMemberValues(this);
            } else {
                this.editRecord(isc.addProperties({}, record));
            }
        },
        
        selectionComponentCellSelectionChanged : function (selectionComponent, cellList) {
            for (var i = 0; i < cellList.length; i++) {
                var cell = cellList[i],
                    record = this.selectionComponent.getCellRecord(cell[0], cell[1]);
                if (selectionComponent.cellIsSelected(record)) break;
                record = null;
            }
            if (record) {
                this._selectionComponentRecordPKs = selectionComponent.getPrimaryKeys(record);
                if (this.valuesManager && this.valuesManager._setMemberValues) {
                    this.valuesManager._setMemberValues(this);
                } else {
                    this.editRecord(isc.addProperties({}, record));
                }
            }
        }
    });
}

// Overrides to existing methods on the DF class
isc._EditorFlowOverrides = {
    //>	@method DynamicForm.fetchData()
    // Retrieve data that matches the provided criteria, and edit the first record returned
    // 
    // @param [criteria]          (Criteria)	  search criteria
    // @param [callback]          (DSCallback)  callback to invoke on completion
    // @param [requestProperties] (DSRequest)   additional properties to set on the DSRequest
    //                                            that will be issued
    //
    // @group dataBoundComponentMethods
    // @visibility external
    //<
    fetchData : function (criteria, callback, requestProperties) {
        var ds = this.getDataSource();
        if (!ds) {
            this.logWarn("Ignoring call to fetchData() on a DynamicForm with no valid dataSource");
            return;
        }
        if (this._fetchDataCallbackArr == null) this._fetchDataCallbackArr = [];
        this._fetchDataCallbackArr.add(callback); 
        
        requestProperties = this.buildRequest(requestProperties, "fetch");
        
        ds.fetchData(criteria, {target:this, methodName:"fetchDataReply"}, requestProperties);
    },
    
    fetchDataReply : function (response, data, request) {
    
        if (data == null || isc.isAn.emptyObject(data) || 
                (isc.isAn.Array(data) && data.getLength() == 0))
        {
            if (response.status == isc.RPCResponse.STATUS_OFFLINE) {
                isc.say(this.offlineMessage);
            }
        }
    
        var record;
        if (isc.isAn.Array(data)) {
            record = data.get(0);    
        } else {
            record = data;    
        }
        //var record = data ? data.get(0) : null;
        if (response.status == isc.RPCResponse.STATUS_SUCCESS || 
            response.status == isc.RPCResponse.STATUS_VALIDATION_ERROR) 
        {
            this.editRecord(record);
        }
        var callback = this._fetchDataCallbackArr.pop();
        if (callback) this.fireCallback(callback, "dsResponse,data,dsRequest", [response,data,request]);
    },
    
    //>	@method DynamicForm.filterData()
    // Retrieve data that matches the provided criteria, and edit the first record returned.<br>
    // Differs from +link{DynamicForm.fetchData()} in that a case insensitive substring match
    // will be performed against the criteria to retrieve the data.
    // 
    // @param [criteria]          (Criteria)	  search criteria
    // @param [callback]          (DSCallback)  callback to invoke on completion
    // @param [requestProperties] (DSRequest)   additional properties to set on the DSRequest
    //                                            that will be issued
    //
    // @group dataBoundComponentMethods
    // @visibility external
    //<    
    filterData : function (criteria, callback, requestProperties) {
        var ds = this.getDataSource();
        if (!ds) {
            this.logWarn("Ignoring call to filterData() on a DynamicForm with no valid dataSource");
            return;
        }
        if (this._fetchDataCallbackArr == null) this._fetchDataCallbackArr = [];
        this._fetchDataCallbackArr.add(callback); 
        ds.filterData(criteria, {target:this, methodName:"fetchDataReply"}, requestProperties);
    }
}

if (isc.DynamicForm) isc.DynamicForm.addMethods(isc._EditorFlowOverrides);


//>ValuesManager
if (isc.ValuesManager) isc.ClassFactory.mixInInterface("ValuesManager", "EditorActionMethods");

if (isc.ValuesManager) isc.ValuesManager.addMethods(isc._EditorFlowOverrides);

// Pick up fieldValuesAreEqual from the DataBoundComponent methods.

if (isc.ValuesManager) {
    isc.ValuesManager.addProperties({
        // apply the standard fieldValuesAreEqual method to ValueMaps as well as Canvii
        fieldValuesAreEqual:isc.Canvas.getPrototype().fieldValuesAreEqual
    })
}

// Add JSDocs to the ValuesManager as well as the DynamicForm

// NOTE: filterData/clearCriteria are not documented because they are just
// convenience relative to summary.filterData/clearCriteria, and it's confusing/distracting
// for them to have the same names.   However these two methods were exposed briefly in
// 5.1. 

//>	@method valuesManager.doExport()
// @include dynamicForm.doExport()
//<

//>	@method valuesManager.editNewRecord()
// @include dynamicForm.editNewRecord()
//<

//>	@method valuesManager.editRecord()
// @include dynamicForm.editRecord()
//<

//> @attr valuesManager.saveOperationType
// @include dynamicForm.saveOperationType
//<

//> @method valuesManager.setSaveOperationType()
// @include dynamicForm.setSaveOperationType()
//<

//> @method valuesManager.isNewRecord()
// @include dynamicForm.isNewRecord()
//<

//> @method valuesManager.getSaveOperationType()
// @include dynamicForm.getSaveOperationType()
//<

//>	@method valuesManager.editSelectedData()
// @include dynamicForm.editSelectedData()
//<

//>	@method valuesManager.saveData()
// @include dynamicForm.saveData()
//<

//> @attr valuesManager.suppressValidationErrorCallback (Boolean : false : IRWA)
// @include dynamicForm.suppressValidationErrorCallback
//<

//>	@method valuesManager.submit()
// <code>submit()</code> is automatically called when a +link{SubmitItem} in a member form
// is clicked, or if +link{dynamicForm.saveOnEnter,saveOnEnter} is set for some member form,
// when the
// "Enter" key is pressed in a text input.  Submit can also be manually called.
// <P>
// If +link{valuesManager.submitValues(),valuesManager.submitValues()} exists, it
// will be called, and no further action will be taken.
// <P>
// Otherwise, +link{method:valuesManager.saveData()} will be called to
// handle saving via SmartClient databinding.  
// <P>
// The parameters to <code>submit()</code> apply only if <code>submit()</code> will be
// calling +link{saveData()}.  If you override <code>submit()</code>, you can safely
// ignore the parameters as SmartClient framework code does not pass them.
//
// @include dynamicForm.submit()
// @param [callback]          (DSCallback)  callback to invoke on completion.
// @param [requestProperties] (DSRequest)   additional properties to set on the DSRequest
//                                          that will be issued 
// @group dataBoundComponentMethods
// @see method:valuesManager.submitValues()
// @visibility external
//<

//>	@method valuesManager.cancel()
// @include dynamicForm.cancel()
//<

//> @method valuesManager.filterData()
// @include dynamicForm.filterData()
//<

//> @method valuesManager.fetchData()
// @include dynamicForm.fetchData()
//<

//<ValuesManager

if (isc.TreeGrid) {
    
isc.TreeGrid.addProperties({
    
    ignoreEmptyCriteria: true
});

isc.TreeGrid.addMethods({


    // TreeGrid.fetchData() / filterData() documented in DataboundComponent.js
    // Overridden to work with ResultTree (Hierarchical data) rather than ResultSet.
    
    
    alwaysCreateNewResultTree:false,
    useExistingDataModel : function (criteria, operation, context) {
        if (this.alwaysCreateNewResultTree) return false;
        return this.Super("useExistingDataModel", arguments);
    },

    createDataModel : function (criteria, operation, context) {
        return this.createResultTree(criteria, context.afterFlowCallback, context, null);
    },
    
    filterWithCriteria : function (criteria, operation, context) {
        var fireSynchronousCallback = false;
        if (this.useExistingDataModel(criteria, operation, context) &&
            isc.ResultTree && isc.isA.ResultTree(this.data))
        {
            // This method will shift the context.afterFlowCallback to the
            // special one-time "initialFetchCallback" slot so it fires once only
            this._setupResultTreeFetchCallback(context);
            
            delete this.data._performedInitialFetch;
        }
        return this.Super("filterWithCriteria", arguments);
    }    
    
});

}

// DETAIL VIEWING
// --------------------------------------------------------------------------------------------
if (isc.DetailViewer) {
    
isc.DetailViewer.addMethods({

    //>	@method detailViewer.viewSelectedData()
    //
    // Displays the currently selected record(s) of the selectionComponent widget (typically a
    // listGrid) in the detailViewer.
    //
    // @param selectionComponent (ListGrid or ID)
    //     the ListGrid or ID of a +link{ListGrid} whose currently selected
    //     record(s) is/are to be edited
    //
    // @group dataBoundComponentMethods
    // @visibility external
    //<
    // NOTE: technically, application.viewSelected() has a case where it will issue a DSRequest
    // to fetch the full set of fields, but we don't expose the capability to have a ListGrid
    // load less than the full set of fields right now.
    // @param [callback]         (DSCallback)    callback to invoke on completion
    // @param [requestProperties] (DSRequest)     additional properties to set on the
    //                                             DSRequest that will be issued
    viewSelectedData : function (selectionComponent, callback, requestProperties) {
        
        // support being passed an ID
        if (isc.isA.String(selectionComponent)) selectionComponent = window[selectionComponent];
    
        requestProperties = requestProperties || {};
        
        var selection = selectionComponent.selection.getSelection();
        if (selection && selection.length > 0) {
            
            // if we're not passed an operation, simply show the records from the selection in the
            // viewer
            if (!requestProperties.operation) {
                this.setData(selection);	
            
            
            } else {
    
                // We were passed an operation - perform it to get the record back from the server.
                // This would be required if (for example) the selection components operation is 
                // only getting a subset of fields
                
                // reduce the recordList to just the primary keys
                var operation = requestProperties.operation,
                    dataSource = this.getDataSource(),
                    keys = dataSource.filterPrimaryKeyFields(selection);
                
                if (requestProperties.prompt == null) 
                    requestProperties.prompt = isc.RPCManager.getViewRecordsPrompt;
                requestProperties.viewer = this;
                
                // actually perform the relevant operation       
                return dataSource.performDSOperation(
                                            operation.type, keys, 
                                            (callback ? callback : {target:this, methodName:"viewSelectedDataReply"}), 
                                            requestProperties
                        );
            }
        }
        return false;           
    },
    
    //>!BackCompat 2004.7.23
    viewSelected : function (selectionComponent, context) { 
        return this.viewSelectedData(selectionComponent, context)
    },
    //<!BackCompat
    
    // handle a reply from the viewSelectedData call
	viewSelectedDataReply : function (response, data, request) {
		this.setData(data);
	}
    
});

}
