/*
 * Isomorphic SmartClient
 * Version v11.0p_2016-03-31 (2016-03-31)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */
//> @class RuleEditor
// A user-interface component for creation and editing of a +link{Rule} or +link{Validator}.
// @treeLocation Client Reference/Rules
// @visibility rules
//<
isc.defineClass("RuleEditor", "VLayout");


isc.RuleEditor.addProperties({

    // ----
    // Basics / Attributes
    // ----
    
    // Default height to explicit size. This will give it "implicit height" and stop it
    // expanding in Layouts to fill available space.
    height:100,
    
    //> @attr ruleEditor.rule (Validator : null : IRW)
    // Rule to be edited by this ruleEditor. Use +link{setRule} and +link{getRule} to
    // update or retrieve this object at runtime.
    // @visibility rules
    //<
    
    //> @attr ruleEditor.validator (Validator : null : IRW)
    // Validator to be edited by this ruleEditor. Synonym for +link{ruleEditor.rule}.
    // Use +link{setRule} and +link{getRule} to update or retrieve this object at runtime.
    // @visibility rules
    //<
    
    //> @attr ruleEditor.fieldName  (String : null : IRW)
    // Name of the field to which the rule applies. If +link{fieldPicker} is visible, this may
    // be chosen by the user. The set of fieldNames displayed to the user will be derived from
    // +link{ruleEditor.dataSource} or +link{ruleEditor.dataSources}.
    // @visibility rules
    //<
    
    
    //> @attr ruleEditor.dataSource (DataSource : null : IR)
    // DataSource where this validator will be applied. The +link{fieldName} should refer
    // to a field within this dataSource. Should not be set in conjunction with +link{dataSources}.
    // @visibility rules
    //<
    
    //> @attr ruleEditor.dataSources (Array of DataSource : null : IR)
    // DataSources available to this rule when defining a rule. The +link{fieldName} should
    // refer to a field within these dataSource, using notation of the form
    // <code><i>dataSourceID</i>.<i>fieldName</i></code>. Should not be set in conjunction with
    // +link{dataSource}.
    // @visibility rules
    //<
    
    //> @attr ruleEditor.locator (AutoTestObjectLocator : null : IRW)
    // Optional locator to be stored as +link{rule.locator} for the rule being edited.
    // This locator may be absolute, or may be relative to a base component. If a relative
    // object locator is used, when the generated rule is applied to a +link{RulesEngine},
    // the RulesEngine would be expected to have +link{rulesEngine.baseComponent} set such
    // that the relative locator can be resolved to a live object at runtime.
    // If +link{fieldPicker} is visible, this may be chosen by the user from the options
    // provided as +link{ruleEditor.locatorMap}.
    // <P>
    // See +link{AutoTest.getObjectLocator()} and +link{AutoTest.getRelativeObjectLocator()} for
    // more on object locators.
    //
    // @visibility rules
    //<

    //> @attr ruleEditor.locatorMap (ValueMap : null : IRW)
    // Set of possible +link{type:AutoTestObjectLocator}s to be shown in the +link{fieldPicker} 
    // for this rule editor. This allows the user to specify the +link{rule.locator} for
    // the rule being edited.
    // <P>
    // This property is a ValueMap - it may be specified as an array of AutoTestObjectLocator
    // string, or as an object mapping of locator strings to display values.
    // <P>
    // See +link{AutoTest.getObjectLocator} and +link{AutoTest.getRelativeObjectLocator()} for
    // information on getting object locators from live SmartClient objects.
    //
    // @visibility rules
    //<
    
    //> @attr ruleEditor.triggerEvent (TriggerEvent : null : IRW)
    // If this ruleEditor is editing a rule to be applied via a +link{rulesEngine}, 
    // what +link{rule.triggerEvent} is assigned to the rule?
    // +link{triggerEventPicker}
    // @visibility rules
    //<
    // This may be specified programmatically or picked by the user via the triggerEventPicker
    
    //> @attr ruleEditor.validatorType (ValidatorType : null : IRW)
    // Type of validator being edited. If +link{showTypePicker} is true, this may be chosen
    // by the user.
    // @setter setValidatorType
    // @visibility rules
    //<

    //> @attr ruleEditor.validatorIsRule (boolean : true : IRA)
    // Is this ruleEditor editing a rule to be applied via a +link{rulesEngine}, or 
    // modifying a validator to be applied directly to a field.
    // @visibility rules
    //<
    
    validatorIsRule:true,
    
    //> @attr ruleEditor.availableTypes (Array of ValidatorType : [...] : IR)
    // List of available validator types.  Defaults to all validator types and rule types
    // that do not require input of a custom expression (eg "requiredIf"), excluding validators 
    // that just verify the field type and are usually implicit (isBoolean, isString, etc).
    // <P>
    // The special value "range" may be specified to indicate that the appropriate "range"
    // validator for the +link{ruleEditor.field,field type} (integerRange, dateRange, etc) should
    // be used.
    // @visibility rules
    //<
    
    availableTypes:[
        "matchesField",
        "isOneOf",
        "lengthRange",
        "contains",
        "doesntContain",
        "substringCount",
        "regexp",
        "mask",
        "floatPrecision",
        "readOnly",
        "isUnique",
        "hasRelatedRecord",
        "range",
        "message",
        "populate",
        "setRequired"        
    ],
    
    //> @attr ruleEditor.applyWhen (AdvancedCriteria : null : IRW)
    // Criteria indicating under what circumstances the rule should be applied.
    // @visibility rules
    //<
    
    //> @attr ruleEditor.nameItemTitle (string : "Name": IR)
    // Title of the name field.
    // @group i18nMessages
    // @visibility rules
    //<
    nameItemTitle:"Name",

    //> @attr ruleEditor.descriptionItemTitle (string : "Description": IR)
    // Title of the description field.
    // @group i18nMessages
    // @visibility rules
    //<
    descriptionItemTitle:"Description",

    //> @attr ruleEditor.triggerEventTitle (string : "On event": IR)
    // Title of the trigger event field.
    // @group i18nMessages
    // @visibility rules
    //<
    triggerEventTitle:"On event",

    //> @attr ruleEditor.fieldPickerTitle (string : "For fields": IR)
    // Title of the field picker field.
    // @group i18nMessages
    // @visibility rules
    //<
    fieldPickerTitle:"For field",

    //> @attr ruleEditor.applyWhenTitle (string : "If": IR)
    // Title of the applyWhen field.
    // @group i18nMessages
    // @visibility rules
    //<
    applyWhenTitle:"If",

    //> @attr ruleEditor.applyWhenPlaceholder (string : ".. conditional ..": IR)
    // Placeholder text displayed to right of the applyWhenTitle when unchecked.
    // @group i18nMessages
    // @visibility rules
    //<
    applyWhenPlaceholder:".. conditional ..",

    //> @attr ruleEditor.validatorTitle (string : "Do": IR)
    // Title of the validator (rule) field.
    // @group i18nMessages
    // @visibility rules
    //<
    validatorTitle:"Do",

    //> @attr ruleEditor.errorMessageTitle (string : "Message": IR)
    // Title of the errorMessage field.
    // @group i18nMessages
    // @visibility rules
    //<
    errorMessageTitle:"Message",

    
    // -------
    
    
    // default width to 400 - that's enough to accommodate the mainForm
    width:400,
    
    
    initWidget : function () {
        var initialRule = this.rule || this.validator;
        if (initialRule != null) {
            // this will derive fieldName etc from the rule object.
            this.setRule(initialRule, true);
        }

        // Default to hiding the typePicker if a validatorType is defined at init-time
        if (this.showTypePicker == null && this.validatorType != null) {
            this.showTypePicker = false;
        }
        
        // call addAutoChildren to build the UI. This will pick up dynamicDefaults from
        // the special 'getDynamicDefaults' method, and will handle custom UI being injected
        // into the layout.
        this.addAutoChildren(this.components);

        // set initial field values based on initial rule passed in.        
        if (this.nameForm != null && initialRule != null) {
            this.nameForm.setValue("name", initialRule.name);
            this.nameForm.setValue("description", initialRule.description);
        }
        if (this.mainForm) {
            this.fieldPicker = this.mainForm.getItem("fieldName");
            if (this.fieldName != null) this.fieldPicker.setValue(this.fieldName);
        
            this.triggerEventPicker = this.mainForm.getItem("event");
            if (this.triggerEvent) this.triggerEventPicker.setValue(this.triggerEvent);
            
            // If validatorIsRule we show neither the fieldName nor the eventPicker, so
            // hide that entire form so it doesn't take up space.
            if (!this.validatorIsRule) {
                this.mainForm.setVisibility(isc.Canvas.HIDDEN);
            }
        }
        
        if (this.applyWhenForm) {
            // conditionalForm - configures the "applyWhen" block of the validator
            if (this.applyWhen != null) {
                this.applyWhenForm.setValue("applyWhen", true);
                this.updateConditionalForm(true);
            }
        }
        if (this.validatorForm) {        
            this.typePicker = this.validatorForm.getItem("type");
            if (this.validatorType != null) this.typePicker.setValue(this.validatorType);
            
            
            if (this.validatorType != null) {
                this.updateValidatorType(this.validatorType, true);
            }
        }
        
        // Initialize 'errorMessage' value
        if (this.messageForm) {
            if (this.rule && this.rule.errorMessage) {
                this.messageForm.setValue("errorMessage", this.rule.errorMessage);
            }
        }

        // update the clause to show the initial 'value' field attributes etc if there
        // are any.
        if (initialRule != null) {
            this.setClauseAttributes(initialRule);
        }
        
        return this.Super("initWidget", arguments);
    },
    
    
    // ----
    // UI
    // ----
    
    
    //> @attr ruleEditor.components (Array of Object : [...] : IRA)
    // Member components of this rule editor. Default value is an array of auto-children
    // names (strings), but for custom UI, additional components may be explicitly added.
    // @visibility rules
    //<
    components:[
        "nameForm", "mainForm", "applyWhenForm", "validatorForm", "messageForm"
    ],
    
    getDynamicDefaults : function (childName) {
        switch (childName) {
            case "nameForm" : 
                // nameForm autoChild - configures name and description
                var nameTitleProperties = (this.nameItemTitle ? {title:this.nameItemTitle} : null),
                    descTitleProperties = (this.descriptionItemTitle ? {title:this.descriptionItemTitle} : null),
                    nameItem = isc.addProperties({name:"name"}, 
                                    this.nameItemDefaults, this.nameItemProperties, nameTitleProperties),
                    descriptionItem = isc.addProperties({name:"description"}, 
                                    this.descriptionItemDefaults, this.descriptionItemProperties, descTitleProperties);
                
                return {items:[nameItem,descriptionItem]};
        
            case "mainForm" : 
                
                // The picker fields follow the autoChildren pattern but we can't
                // use createAutoChild since the form will of course create the live item instances
                var fieldPickerTitleProperties = (this.fieldPickerTitle ? {title:this.fieldPickerTitle} : null),
                    triggerEventTitleProperties = (this.triggerEventTitle ? {title:this.triggerEventTitle} : null),
                    triggerEventItemValueMap = { valueMap:{
                        "editStart": this.editStartEventTitle,
                        "editStartAndChanged": this.editStartAndChangedEventTitle,
                        "editorEnter": this.editorEnterEventTitle,
                        "editorExit": this.editorExitEventTitle,
                        "changed": this.changedEventTitle,
                        "submit": this.submitEventTitle,
                        "manual": this.manualEventTitle
                    }},
                    fieldItem = isc.addProperties(
                        {creator:this, editorType:this.fieldPickerConstructor},
                         this.fieldPickerDefaults,
                         this.fieldPickerProperties,
                         fieldPickerTitleProperties
                    ),
                    triggerEventItem = isc.addProperties(
                        {creator:this, editorType:this.triggerEventPickerConstructor},
                        this.triggerEventPickerDefaults,
                        triggerEventItemValueMap,
                        this.triggerEventPickerProperties,
                        triggerEventTitleProperties
                    )
        
                return {
                    items:[fieldItem, triggerEventItem]
                };
            
            case "applyWhenForm" : 
                var titleProperties = (this.applyWhenTitle ? {title:this.applyWhenTitle} : null),
                    applyWhenItem = isc.addProperties({name:"applyWhen"}, 
                            this.applyWhenItemDefaults, this.applyWhenItemDefaults, titleProperties),
                    placeholderItem = {type:"StaticTextItem", name:"placeholder", showTitle:false, value:this.applyWhenPlaceholder},
                    conditionalItem = {type:"CanvasItem", showTitle:false, name:"conditionalItem", showIf:"false",
                            createCanvas:function () {
                                return this.form.creator.createConditionalForm()
                            }
                    };

                return {
                    items:[applyWhenItem,placeholderItem,conditionalItem]
                };
                
            // - validatorForm 
            //  o typeItem - for selecting the validator type
            //  o valuesForm (embedded in a CanvasItem) for configuring the validator.
            //    this is a filterClause
            case "validatorForm" :
                var titleProperties = (this.validatorTitle ? {title:this.validatorTitle} : null);
                var typeItem = isc.addProperties(
                        {creator:this, editorType:this.typePickerConstructor},
                         this.typePickerDefaults,
                         this.typePickerProperties,
                         titleProperties
                    );
                    
                var valuesItem = {
                    name:"valuesItem",
                    editorType:"CanvasItem",
                    showTitle:true, title:null,
                    showIf:"false",
                    canvas:this.getValuesForm(this.validatorType)
                }    
                
                return {
                    disabled:(this.fieldName == null),
                    items:[typeItem, valuesItem]
                };
                
            case "messageForm" :
                var titleProperties = (this.errorMessageTitle ? {title:this.errorMessageTitle} : null),
                    messageItem = isc.addProperties({name:"errorMessage"}, 
                                this.errorMessageItemDefaults, this.errorMessageItemDefaults, titleProperties);
            
                return {items:[messageItem]};
        }
    },

    
    //> @attr ruleEditor.showNameForm (boolean : false : IR)
    // Should we show the +link{ruleEditor.nameForm} for editing the <code>name</code> and
    // <code>description</code> attributes of the rule being edited?
    // @visibility rules
    //<
    showNameForm:false,
    
    //> @attr ruleEditor.nameForm (DynamicForm AutoChild : null : IR)
    // DynamicForm used to edit the <code>name</code> and <code>description</code> of the rule
    // being edited.
    // <P>
    // Contains the +link{ruleEditor.nameItem} and +link{ruleEditor.descriptionItem}
    //
    // @visibility rules
    //<
    // Name form - contains name and description
    nameFormConstructor:"DynamicForm",
    nameFormDefaults:{
        numCols:2
    },
    
    //> @attr ruleEditor.nameItem (TextItem AutoChild : {...} :IR)
    // Item for editing the +link{validator.name,name} of the rule being edited. Displayed
    // in the +link{ruleEditor.nameForm} (if +link{ruleEditor.showNameForm} is true).
    // Implemented as an autoChild, so may be customized via <code>nameItemProperties</code>
    // @visibility rules
    //<
    nameItemDefaults:{
        editorType:"TextItem"
    },
    
    //> @attr ruleEditor.descriptionItem (TextItem AutoChild : {...} :IR)
    // Item for editing the +link{validator.description,description} of the rule being edited.
    // Displayed in the +link{ruleEditor.nameForm} (if +link{ruleEditor.showNameForm} is true).
    // Implemented as an autoChild, so may be customized via <code>nameItemProperties</code>
    // @visibility rules
    //<
    descriptionItemDefaults:{
        editorType:"TextAreaItem"
    },
    
    
    // Main Form (FieldName / TriggerEvent)
    
    mainFormConstructor:"DynamicForm",
    mainFormDefaults:{
        numCols:2,
        height:20
    },
    
    //> @attr ruleEditor.fieldPicker (AutoChild FormItem : null : IR)
    // Field for picking +link{rule.fieldName}. This form item will only be visible if
    // the user is editing a rule (see +link{validatorIsRule}).
    // @visibility rules
    //<

    fieldPickerConstructor:"SelectItem",
    
    fieldPickerDefaults:{
        name:"fieldName",
        multiple:true,
        showIf:function () {
            return this.creator.shouldShowFieldPicker();
        },
        pickListProperties:{
            showHeader:true,
            canSelectAll:false
        },
        getClientPickListData:function () {
            var pickListData = [];
            var locatorMap = this.form.creator.locatorMap;
            if (locatorMap != null) {
                if (isc.isAn.Array(locatorMap)) {
                    for (var i = 0; i < locatorMap.length; i++) {
                        pickListData.add({
                            name:locatorMap[i],
                            title:locatorMap[i],
                            type:"locator"
                        });
                    }
                } else {
                    for (var locator in locatorMap) {
                        pickListData.add({
                            name:locator,
                            title:locatorMap[locator],
                            type:"locator"
                        });
                    }
                }
            }
            var fieldData = this.form.creator.getFieldData();
            pickListData.addList(fieldData);
            return pickListData;
        },
        valueField:"name",
        displayField:"title",
        
        
        pickListWidth:300,
        // show the type so its obvious what's going on when we filter by type.
        pickListFields:[
            {name:"title", title:"Target", width:80},
            {name:"type", title:"Type", width:80},
            {name:"name", title:"Identifier", autoFitWidth:true}
        ],
        changed : function (form,item,value) {
            // force a refilter to show only fields that match the specified type.
            if (this.pickList && this.pickList.isVisible()) this.filterPickList();
            var locatorValues = null,
                fieldValues = null;
            if (value != null) {
                if (!isc.isAn.Array(value)) value = [value];
                for (var i = 0; i < value.length; i++) {
                    
                    var field = this.form.creator.getField(value[i], true);
                    if (field == null) {
                        if (locatorValues == null) locatorValues = [];
                        locatorValues.add(value[i]);
                    } else {
                        if (fieldValues == null) fieldValues = [];
                        fieldValues.add(value[i]);
                    }
                }
            }
            
            if (this.creator.warnOnInvalidFieldSelection && 
                value != null && !isc.isAn.emptyArray(value) &&
                (this.creator.getSupportedTypes(fieldValues, locatorValues).length == 0))
            {
                isc.warn(this.creator.invalidFieldSelectionWarning);
                // if we did this on the change rather than changed handler we could return false
                // and suppress the change.
            }
            this.creator.updateFieldName(fieldValues, locatorValues);
        }
    },
    
    //> @attr ruleEditor.warnOnInvalidFieldSelection (boolean : true : IRW)
    // If the +link{fieldPicker} is showing and the user selects a combination of
    // field(s) and locator(s) that will not be supported by any
    // +link{availableTypes,specified rule type}, should we show a warning to the user?
    // +link{invalidFieldSelectionWarning} can be used to customize the warning text.
    // @visibility rules
    //<
    warnOnInvalidFieldSelection:true,
    
    //> @attr ruleEditor.invalidFieldSelectionWarning (string : "None of the available validators can be applied to the selected set of fields." : IRW)
    // If +link{warnOnInvalidFieldSelection} is true, and the user selects a combination of
    // field(s) and locator(s) that will not be supported by any
    // +link{availableTypes,specified rule type}, this warning string will be displayed to
    // the user in a dialog.
    // @group i18nMessages
    // @visibility rules
    //<
    invalidFieldSelectionWarning:"None of the available validators can be applied to the selected set of fields.",
        
    shouldShowFieldPicker : function () {
        if (this.showFieldPicker != null) return this.showFieldPicker;
        return this.validatorIsRule;
    },
    
    getField : function(fieldName, suppressWarning) {
        return this.dataSource ? this.dataSource.getField(fieldName) 
            : isc.DataSource.getFieldFromDataSources(fieldName ,this.dataSources, suppressWarning);
    },
    
    // Client side pickList Data (ValueMap) for the field picker
    getFieldData : function () {
        if (this._fieldNames == null) {
            var dataSources = this.dataSources;
            if (dataSources != null) {
                this._fieldNames = isc.DataSource.getCombinedDataSourceFields(this.dataSources);

            } else if (this.dataSource) {
                var dsFields = isc.getKeys(this.dataSource.getFields());
                this._fieldNames = dsFields.duplicate();
            }
            this._fieldData= [];
            for (var i = 0; i < this._fieldNames.length; i++) {
                var name = this._fieldNames[i],
                    field = this.getField(name);
                    
                this._fieldData[i] = {
                    name:name,
                    title:field.title || field.name,
                    type:field.type || "text"
                }
            }
        }
        return this._fieldData;
    },
    updateFieldName : function (fieldName, locator) {

        this.fieldName = fieldName;
        this.locator = locator;
        var hasFields = fieldName != null && (!isc.isAn.Array(fieldName) || fieldName.length > 0),
            hasLocators = locator != null && (!isc.isAn.Array(locator) || locator.length > 0),
            supportedTypes = (hasFields || hasLocators) ? this.getSupportedTypes(fieldName, locator)
                                                        : [],
            currentValidatorIsValid;
        
        if (this.validatorType != null && supportedTypes.length > 0) {
            currentValidatorIsValid = supportedTypes.contains(this.validatorType);
        }
            
        if (this.validatorForm) {
            if (!currentValidatorIsValid) {
                this.validatorForm.setValue("type", null);
                this.validatorType = null;
            }
            if (supportedTypes.length == 0) {
                this.validatorForm.setDisabled(true);
            } else {
                this.typePicker.setValueMap(supportedTypes);
                this.validatorForm.setDisabled(false);
            }
        }
        // (Re)Build the filter clause form items.
        // - required if we change validator type [may be entirely different set of value items]
        // - required if we change field [value items may be type-specific or show value map 
        //   of all other fields, etc]
        
        if ((hasLocators || hasFields) && this.validatorType != null) {
        
            var needsRebuild = false;
            if (!hasLocators) {
                var oldFieldNames = this._lastFieldNames;
                if (oldFieldNames == null) {
                    needsRebuild = true;
                } else {
                    needsRebuild = true;
                    for (var i = 0; i < oldFieldNames.length; i++) {
                        var oldFieldName = oldFieldNames[i];
                        if (isc.isAn.Array(fieldName) ? (fieldName.contains(oldFieldName)) 
                                               : (fieldName == oldFieldName) )
                        {
                            needsRebuild = false;
                            break;
                        }
                    }
                    
                }
                
            }
            this._lastFieldNames = (fieldName == null) ? null 
                                    :   (isc.isAn.Array(fieldName) ? fieldName : [fieldName]);

            if (this.valuesForm.clause.getValue("operator") != this.validatorType) {
                needsRebuild = true;
            }
            
            
            if (isc.isAn.Array(fieldName)) fieldName = fieldName[0];
            this.valuesForm.fieldName = fieldName;
            
            this.valuesForm.clause.setValue("fieldName", fieldName);
            this.valuesForm.clause.setValue("operator", this.validatorType);

            if (needsRebuild) {
    
                // We can't just call 'fieldNameChanged()' - that'll attempt to compare the
                // operatorType with an operator object using 'DataSource.getSearchOperator()' which
                // doesn't apply outside of Criteria editing. Insted call updateValueItems directly.
                var validatorDefinition = this.getValidatorDefinition(this.validatorType);
        
                this.valuesForm.updateValueItems(
                        this.valuesForm.getField(fieldName), validatorDefinition, fieldName);
            }
        } 
        this.updateValuesFormVisibility();
    },
    
    updateValuesFormVisibility : function () {
        
        if (this.valuesForm) {
            if ((this.fieldName == null && this.locator == null) || this.validatorType == null) {
                this.validatorForm.getItem("valuesItem").hide();
            } else {
                if (!this.valuesForm.isVisible()) {
                    this.validatorForm.getItem("valuesItem").show();
                }
            }
        }
    },

    //> @attr ruleEditor.triggerEventPicker (AutoChild FormItem : null : IR)
    // Field for picking +link{rule.triggerEvent}. This form item will only be visible if
    // the user is editing a rule (see +link{validatorIsRule}).
    // @visibility rules
    //<

    //> @attr ruleEditor.editStartEventTitle (string : "Edit start": IR)
    // User-friendly title for editStart event shown in triggerEventPicker.
    // @group i18nMessages
    // @visibility rules
    //<
    editStartEventTitle:"Edit start",

    //> @attr ruleEditor.editStartAndChangedEventTitle (string : "Edit start/changed": IR)
    // User-friendly title for editStartAndChanged event shown in triggerEventPicker.
    // @group i18nMessages
    // @visibility rules
    //<
    editStartAndChangedEventTitle:"Edit start/changed",

    //> @attr ruleEditor.editorEnterEventTitle (string : "Editor enter": IR)
    // User-friendly title for editorEnter event shown in triggerEventPicker.
    // @group i18nMessages
    // @visibility rules
    //<
    editorEnterEventTitle:"Editor enter",

    //> @attr ruleEditor.editorExitEventTitle (string : "Editor exit": IR)
    // User-friendly title for editorExit event shown in triggerEventPicker.
    // @group i18nMessages
    // @visibility rules
    //<
    editorExitEventTitle:"Editor exit",

    //> @attr ruleEditor.changedEventTitle (string : "Changed": IR)
    // User-friendly title for changed event shown in triggerEventPicker.
    // @group i18nMessages
    // @visibility rules
    //<
    changedEventTitle:"Changed",

    //> @attr ruleEditor.submitEventTitle (string : "Submit": IR)
    // User-friendly title for submit event shown in triggerEventPicker.
    // @group i18nMessages
    // @visibility rules
    //<
    submitEventTitle:"Submit",

    //> @attr ruleEditor.manualEventTitle (string : "Manual": IR)
    // User-friendly title for manual event shown in triggerEventPicker.
    // @group i18nMessages
    // @visibility rules
    //<
    manualEventTitle:"Manual",

    triggerEventPickerConstructor:"SelectItem",
    triggerEventPickerDefaults:{
        name:"event",
        startRow:true,
        changed : function (form,item,value) {
            this.creator.updateTriggerEvent(value);
        },
        showIf : function () {
            return this.creator.shouldShowTriggerEventPicker();
        }
    },
    
    shouldShowTriggerEventPicker : function () {
        if (this.showTriggerEventPicker != null) return this.showTriggerEventPicker;
        return this.validatorIsRule;
    },
    
    // ---
    // Conditional / applyWhen UI
    // ---

    // 'applyWhenForm' contains just the checkbox to show /hide the conditional criteria form
    applyWhenFormConstructor:"DynamicForm",
    applyWhenFormDefaults:{
        numCols:2,
        fixedColWidths:true,
        
        height:20
    },

    applyWhenItemDefaults:{
        showLabel:true, editorType:"CheckboxItem",
        width:20, showTitle:false, align:"right", vAlign:"top",
        changed:"this.form.creator.updateConditionalForm(value)",
        init:function() {
            // Simulate display as title with corresponding prefix/suffix handling
            if (this.form) {
                var form = this.form,
                    orient = form.getTitleOrientation(),
                    titlePrefix = (orient == "right" ? form.rightTitlePrefix : form.titlePrefix),
                    titleSuffix = (orient == "right" ? form.rightTitleSuffix : form.titleSuffix)
                ;
                this.title = titlePrefix + this.title + titleSuffix;
            }
            this.Super("init", arguments);
        }
    },

    //> @attr ruleEditor.filterTopOperatorAppearance (string : "radio" : IR)
    // Set the initial "If" section +link{FilterBuilder.topOperatorAppearance}. Note that
    // when an existing rule that has nested clauses in the <code>applyWhen</code> attribut
    // is edited by calling +link{setRule} the "If" section will be automatically switched
    // to the "bracket" setting. 
    // @visibility rules
    //<
    filterTopOperatorAppearance:"radio",

    //> @attr ruleEditor.conditionalForm (AutoChild FilterBuilder : null : IR)
    // Automatically generated filter-builder used to edit the +link{rule.applyWhen} attribute
    // when editing a rule. Only visible if +link{ruleEditor.validatorIsRule} is true.
    // @visibility rules
    //<
    
    conditionalFormConstructor:"FilterBuilder",
    conditionalFormDefaults:{
        showFieldTitles:false,
        fieldPickerProperties: {
        },
        showModeSwitcher:true,
        multiDSFieldFormat:"qualified"
    },
    
    createConditionalForm : function () {
        var topOperatorAppearance = this.filterTopOperatorAppearance || "radio";
        this.conditionalForm = this.createAutoChild("conditionalForm", 
            {dataSource:this.dataSource, dataSources:this.dataSources,topOperatorAppearance:topOperatorAppearance}
        );
        this.conditionalForm.fieldPickerProperties.pickListWidth = this.conditionalForm.getWidth();
        return this.conditionalForm;
    },
    
    updateConditionalForm : function (show) {
        var placeholder = this.applyWhenForm.getItem("placeholder"),
            item = this.applyWhenForm.getItem("conditionalItem");
        if (!show) {
            placeholder.show();
            item.hide();
        } else {
            var criteria = this.applyWhen || {};
            this.conditionalForm.setCriteria(criteria);
            placeholder.hide();
            item.show();
        }
        // When setting/clearing a rule set the filter to the simplest
        // for applicable for the criteria.
        this.conditionalForm.setTopOperatorAppearance(isc.DataSource.isFlatCriteria(criteria) ? "radio" : "bracket");
    },
    
    // ---
    // Validator Config UI: type picker and valuesForm
    // ---
    
    // validatorForm - contains the 'typePicker' and the valuesForm CanvasItem  
    validatorFormConstructor:"DynamicForm",
    validatorFormDefaults:{
        numCols:2,
        fixedColWidths:true,
        height:20
    },

    //> @attr ruleEditor.typePicker (AutoChild FormItem : null : IR)
    // Field for picking +link{validatorType}.
    // @visibility rules
    //<

    //> @attr ruleEditor.showTypePicker (boolean : null : IR)
    // Whether the +link{typePicker} is shown. If not explicitly specified, the typePicker will
    // be shown if +link{validatorType} is not specified at initialization time.
    // @visibility rules
    //<

    typePickerConstructor:"SelectItem",
    
    typePickerDefaults:{
        name:"type",
        width:"*",
        pickListProperties: { sortField: 0 },
        showIf:function () {
            var ruleEditor = this.form.creator;
            return ruleEditor.showTypePicker == false ? false : true;
        },
        getValueMap:function () {
            return this.creator.getTypeValueMap();
        },
        changed:function(form,item,value) {
            this.creator.updateValidatorType(value);
        }
    },
    
    // ValueMap for the validator type form item.
    // The available validator / rule types will vary depending on what the selected field
    // and locator is.
    getTypeValueMap : function () {
        return this.getSupportedTypes(this.fieldName, this.locator);
    },
    
    getSupportedTypes : function (fieldName, locators) {
        var types = this.availableTypes,
            supportedTypesValueMap = {};
            
        fieldName = fieldName || [];
        locators = locators || [];
        if (!isc.isAn.Array(fieldName)) fieldName = [fieldName];
        if (!isc.isAn.Array(locators)) locators = [locators];

        // if we have no selected fields/validators we won't show any options.        
        if (types.length == 0 || fieldName.length == 0 && locators.length ==0) {
            return [];
        }
        
        
        var rangeType = null,
            validRangeTypes = {date:true, "float":true, integer:true, time:true};
            
        for (var i = 0; i < types.length; i++) {
            var validator = this.getValidatorDefinition(types[i]) || {},
                showOption = true;
            
            if (locators.length > 0) {
                // only "rules" support locators.
                if (!validator.isRule) {
                    showOption = false;
                } else {
                    // AutoTest has a method to extract the target object type from an object
                    // locator (without having to resolve to a live object).
                    for (var ii = 0; ii < locators.length; ii++) {
                        // If the rule doesn't support the targetObjectType, skip it.
                        var targetObjectType = isc.AutoTest.getLocatorObjectType(locators[ii]),
                            validTargets = validator.supportedTargets || ["FormItem"];
                        if (!validTargets.contains(targetObjectType)) {
                            showOption = false;
                            break;
                        }
                    }
                }
            }
            
            // Our chosen locators may already disallow this validator type, so move onto the
            // next one.
            if (!showOption) continue;
            
            // For fields, validators are valid depending on data type.
            if (fieldName.length > 0) {
                for (var ii = 0; ii < fieldName.length; ii++) {
                    var field = this.getField(fieldName[ii]);
                    if (field == null) {
                        this.logWarn("unable to retrieve field for:" + fieldName[ii]);
                        continue;
                    }
                    var fieldDataType = isc.SimpleType.getBaseType(field.type || "text");
                    // Special-case range which maps to different validators depending on type
                    if (types[i] == "range") {
                        if (!validRangeTypes[fieldDataType] ||
                            (rangeType != null && rangeType != fieldDataType)) 
                        {
                            showOption = false;
                            // skip remaining fields
                            break;
                        } else {
                            // rangeType allows us to support only field type being chosen
                            // for ranges since more than one would imply we're generating
                            // multiple validators of different types.
                            rangeType = fieldDataType;
                        }
                    } else {
                        // dataType:"none" implies the validator doesn't care about the
                        // data-type of the target
                        if (validator.dataType != null && validator.dataType != "none"
                            && validator.dataType != fieldDataType) {
                            showOption = false;
                            // skip remaining fields
                            break;
                        }
                    }
                }
            }
            
            if (showOption) {
                var validatorDefinition = this.getValidatorDefinition(types[i]);
                var description = validatorDefinition.description || isc.DataSource.getAutoTitle(types[i]);
                supportedTypesValueMap[types[i]] = description;
            }
        }
        return supportedTypesValueMap;
        
    },

    // This method fired when the validator type changes.
    // Refreshes the valuesForm
    updateValidatorType : function (type, forceRebuild) {
        if (this.validatorType == type && !forceRebuild) return;
        this.validatorType = type;
        if (type != null) {
            var currentValuesForm = this.valuesForm,
                newValuesForm = this.getValuesForm(type);
            // Note that 'getValuesForm()' will actually update the valuesForm's valueItems
            
            if (currentValuesForm != newValuesForm) {
                this.valuesForm = newValuesForm;
                this.validatorForm.getItem("valuesItem").setCanvas(this.valuesForm);
                // (Don't destroy old validator form - we may want to reuse it
            }
        }
        // This'll actually hide the form if there's no selected validatorType
        this.updateValuesFormVisibility();
    },
    
    messageFormConstructor:"DynamicForm",
    messageFormDefaults:{
        numCols:2,
        width:"100%",
        height:20
    },
   
    //> @attr ruleEditor.errorMessageItem (TextItem AutoChild : {...} :IR)
    // Item for editing the +link{validator.errorMessage,errorMessage} of the rule being edited. Displayed
    // in the +link{ruleEditor.messageForm}. Implemented as an autoChild, so may be customized
    // via <code>errorMessageItemProperties</code>.
    // @visibility rules
    //<
    errorMessageItemDefaults:{
        editorType:"TextItem",
        width:"*"
    },

    //> @attr ruleEditor.valuesForm (AutoChild FilterClause : null : IR)
    // Form used for editing the attributes of a validator.
    // @visibility rules
    //<
    // This is a customized filterClause -- we use the class so it will derive the appropriate
    // form items to show based on available dataSource fields, field.type and validator.valueType 
    // but we make the following fundamental changes:
    // - suppress the "remove" icon
    // - suppress the "fieldPicker" field (shown directly in the RuleEditor instead)
    // - suppress the "operator" picker. The clause will be passed validator definition objects
    //   instead of criterion operator objects. We show an operator picker directly in the RuleEditor
    // - never call the standard 'getCriterion' method - we're building validators, not criteria.
    //   Instead we duplicate the relevant bits of this to extract the values from the value field(s)
    //   and for custom editors, call the special validator.getAttributesFromEditor() API
     
    valuesFormConstructor:"FilterClause",
    
    valuesFormDefaults:{
        // validatorAttribute / rangeStart/end attributes and getAttributesFromEditor may be
        // defined on the validator definitions.
        customGetValuesFunction:"getAttributesFromEditor",
        customSetValuesFunction:"setEditorAttributes",
        operatorAttribute:"type",
    
        // Don't show the field-picker item
        fieldPickerProperties:{
            showIf:"return false"
        },
        
        getEditorType : function (field, validatorType) {
            var validatorDefinition = this.creator.getValidatorDefinition(validatorType);
            if (validatorDefinition && validatorDefinition.valueType == "custom" && 
                validatorDefinition.editorType) 
            {
                return validatorDefinition.editorType;            
            }
            if (field && isc.SimpleType.inheritsFrom(field.type, "date")) return "RelativeDateItem";
            if (validatorType == "readOnly") {
                return "ReadOnlyRuleEditor";
            }
            // Return null - this'll back off to default behavior
            return null;
        }
        
    },
    
    // Helper to convert the "validatorType" understood by this widget
    // to the validatorType supported at the validator level.
    // This basically resolves "range" to "dateRange" / "integerRange" etc based
    // on field type.
    resolveValidatorType : function (type) {
    
        if (type == null) type = this.validatorType;
        if (type == null) return null;
        
        // special-case "range" - get the range for the field type
        if (type == "range") {
            var field = this.fieldName,
                fieldType;
            if (field != null) {
                if (!isc.isAn.Array(field)) field = [field];
                var typeMismatch = false;
                for (var i = 0; i < field.length; i++) {
                    var fieldObj = this.getField(field[i]),
                        
                        currentFieldType = fieldObj.type || "integer";
                    // Resolve to base type (so a custom subtype of "integer" still uses
                    // an integerRange, say)
                    currentFieldType = isc.SimpleType.getBaseType(currentFieldType);
                    
                    if (fieldType == null) {
                        fieldType = currentFieldType
                    } else {
                        if (currentFieldType != fieldType &&
                            // Special case date vs datetime (same range validator)
                            (currentFieldType != "date" && fieldType != "datetime" &&
                             currentFieldType != "datetime" && fieldType != "date"))
                        {
                            typeMismatch = true;
                            fieldType = "integer";
                        }
                    }
                }
                if (typeMismatch) {
                    this.logWarn("'range' validator for fields with differing types:"
                         + this.echo(this.fieldName) +
                         ". Defaulting to integer type data", "RuleEditor");
                } else {
                    this.logDebug("'range' validator for field[s]:"
                         + this.echo(this.fieldName) +
                         ". Assuming " + fieldType + " type data", "RuleEditor");
                 }
                
            // no field at all? Default to integer
            
            } else {
                this.logInfo("Attempting to get 'range' validator with no field type - defaulting to integer",
                    "RuleEditor");
                fieldType = "integer";
            }
                // IF we don't have a field, this is sorta invalid, but default to integerRange
             
            // All ranges:
            // integerRange
            // dateRange
            // timeRange
            // floatRange
            // - default to integerRange if its none of these!
            // ('lengthRange' is the only range that makes sense for strings, but it'd be
            // an odd behavior if the user picks just "range" on a string field).
            if (fieldType == "date" || fieldType == "datetime") {
                type = "dateRange";
            } else if (fieldType == "time") {
                type = "timeRange";
            } else if (fieldType == "float") {
                type = "floatRange"
            } else {
                type = "integerRange"
            }
                
        }
        return type;
    },
    
    // Helper to get a 'validatorDefinition' from a validatorType name
    getValidatorDefinition : function (type) {
        type = this.resolveValidatorType(type);
        return isc.Validator._validatorDefinitions[type];
    },
    
    
    getValuesForm : function (validatorType) {

        if (validatorType != null) {
            var validatorDefinition = this.getValidatorDefinition(validatorType),
                valueType = validatorDefinition.valueType;
            validatorDefinition.ID = validatorType;
        }
        
        var fieldName = this.fieldName;        
        
        if (isc.isAn.Array(fieldName)) fieldName = fieldName[0];
        if (this.valuesForm) {
            var field = fieldName ? this.valuesForm.getField(fieldName) : null;
            this.valuesForm.updateValueItems(field, validatorDefinition, fieldName);
            
            this.valuesForm.clause.setValue("operator", validatorType);
            
            return this.valuesForm;
        } else {
            
            var form = this.valuesForm = this.createAutoChild("valuesForm", {
                visibility:([this.fieldName || this.locator] ? "inherit" : "hidden"),
                showRemoveButton:false,
                // support multiple or singular dataSource
                dataSources:this.dataSources,
                dataSource:this.dataSource,
                fieldName:fieldName,
                operatorType:validatorType
            });
            
            // hide the operatorPicker in the clause - we have a separate item for this.
            
            var clauseForm = form.clause;
            clauseForm.getItem("operator").hide();
            // allow unknown values so we can set to 'validatorTypes' that aren't present in the
            // standard 'operators' valueMap
            
            clauseForm.getItem("operator").addUnknownValues = true;
            return form;
        }
    },
    
    // -----
    // End of UI
    // -----
    
    //> @method ruleEditor.setValidatorType()
    // Update the +link{ruleEditor.validatorType}
    // @param type (ValidatorType) validatorType
    // @visibility rules
    //<
    setValidatorType : function (type) {
        this.validatorForm.setValue("type", type);
        this.updateValidatorType(type);
    },
        
    //> @method ruleEditor.setFieldName()
    // Sets the fieldName applied to the rule.
    // @visibility rules
    //<
    // For validators managed by a rulesEngine,
    // rule.fieldName specifies what field the validator is attached to.
    // For normal forms the validators are defined as an attribute on the field.
    // We need to know the fieldName in order to show the correct UI - assume the calling code
    // will set this at init time or runtime.
    setFieldName : function (fieldName) {
        var locator = this.locator;
        if (this.fieldPicker) {
            var combinedValue = fieldName
            if (locator != null) {
                if (!isc.isAn.Array(locator)) combinedValue = [locator];
                else combinedValue = locator.duplicate();
                if (isc.isAn.Array(fieldName)) {
                    combinedValue.addList(fieldName);
                } else if (fieldName != null) {
                    combinedValue.add(fieldName);
                }
            }
            // fieldPicker displays both field and locator.
            this.fieldPicker.setValue(combinedValue);
        }
        this.updateFieldName(fieldName, this.locator);
    },

    //> @method ruleEditor.setLocator()
    // Sets the locator applied to the rule.
    // @visibility rules
    //<
    setLocator : function (locator) {
        if (this.fieldPicker) {
            var fieldName = this.fieldName,
                combinedValue = locator;
            if (fieldName != null) {
                if (!isc.isAn.Array(fieldName)) combinedValue = [fieldName];
                else combinedValue = fieldName.duplicate();
                if (isc.isAn.Array(locator)) {
                    combinedValue.addList(locator);
                } else if (locator != null) {
                    combinedValue.add(locator);
                }
            }
            // fieldPicker displays both field and locator.
            this.fieldPicker.setValue(combinedValue);
        }
        this.updateFieldName(this.fieldName, locator);
    },
    
    //> @method ruleEditor.setTriggerEvent()
    // Sets the +link{triggerEvent} for this ruleEditor
    // @param event (TriggerEvent) new trigger event
    // @visibility rules
    //<
    setTriggerEvent : function (event) {
        if (this.triggerEventPicker) {
            this.triggerEventPicker.setValue(event);
        }
        this.updateTriggerEvent(event);
    },
    updateTriggerEvent : function (event) {
        this.triggerEvent = event;
    },

    
    //> @method ruleEditor.setApplyWhen()
    // Sets the +link{applyWhen} attribute for this ruleEditor.
    // @param applyWhen (AdvancedCriteria) criteria indicating when the rule should be applied.
    // @visibility rules
    //<
    setApplyWhen : function (criteria) {
        this.applyWhen = criteria;
        this.applyWhenForm.setValue("applyWhen", (this.applyWhen != null));
        this.updateConditionalForm(criteria != null);
    },
    
    getApplyWhen : function () {
        if (this.applyWhenForm.getValue("applyWhen")) {
            this.applyWhen = this.conditionalForm.getCriteria();
        } else {
            this.applyWhen = null;
        }
        return this.applyWhen;
    },

    // attributes from the 'valuesForm'.
    // Typically this is just the single value/fieldName, but may include other fields
    // depending on the valueType / editorType etc of the validator.
    
    getAttributesFromClause : function () {
        var baseDef = this.getValidatorDefinition();
        var fieldName = this.fieldName,
            validatorClause = this.valuesForm;
        
        if (isc.isAn.Array(fieldName)) fieldName = fieldName[0];
        var validatorAttributes = validatorClause.getClauseValues(fieldName, baseDef);
        return validatorAttributes;
    },
    
    setClauseAttributes : function (attributes) {
        if (this.valuesForm == null) return;
        // update the "value" field[s] of the clause form
        // That's typically "value" or "start"/"end" but might call custom setter for some
        // validator types.
        // Note that this sill not update validatorType/fieldName -- that should already have
        // been handled via setRule() if necessary.
        var baseDef = this.getValidatorDefinition();
        var fieldName = this.fieldName;
        
        if (isc.isAn.Array(fieldName)) fieldName = fieldName[0];

        this.valuesForm.setClauseValues(fieldName, baseDef, attributes);
        
    },
    
    //> @method ruleEditor.getValidator()
    // synonym for +link{getRule()}.
    // @return (Validator) edited validator object
    // @visibility rules
    //<
    getValidator : function () {
        return this.getRule();
    },
    
    //> @method ruleEditor.getRule()
    // Get the rule (validator). Will return null if +link{fieldName} or +link{validatorType} are
    // not set.
    // @return (Validator) edited validator object
    // @visibility rules
    //<
    getRule : function () {
        if (this.validatorType == null || (this.fieldName == null && this.locator == null)) return null;
        var validator = {};
        // resolveValidatorType will convert "range" to "dateRange" (etc) based on field type.
        validator.type = this.resolveValidatorType(this.validatorType);
        
        if (this.nameForm != null) {
            var name = this.nameForm.getValue("name");
            if (name != null) validator.name = name;
            var description = this.nameForm.getValue("description");
            if (description != null) validator.description = description;
        }
                
        // attributes from the filterClause form
        if (this.valuesForm != null) {
            var validatorAttributes = this.getAttributesFromClause();
            for (var attr in validatorAttributes) {
                // Don't clobber the "type" - we already resolved that to a meaningful 
                // validatorType
                if (attr == "type") continue;
                
                validator[attr] = validatorAttributes[attr];
            }
        }
        
        
        if (this.validatorIsRule) {
            if (this.fieldName) validator.fieldName = this.fieldName;
            if (this.locator) validator.locator = this.locator;
            
            if (this.triggerEvent) validator.triggerEvent = this.triggerEvent;
        } else {
            delete validator.fieldName;
        }

        validator.errorMessage = this.messageForm.getValue("errorMessage");

        // applyWhen criteria for the validator        
        var applyWhen = this.getApplyWhen();
        if (applyWhen != null) validator.applyWhen = applyWhen;
        
        // If description is not shown in the editor, create a description
        // based on the entered values.
        if (!this.showNameForm) {
            validator.description = this.createRuleDescription(validator);
        }
        return validator;
    },
    
    
    
    createRuleDescription : function (rule) {
        if (!rule.type) return null;

        var validatorDefinition = this.getValidatorDefinition(rule.type),
            title = validatorDefinition.title || isc.DataSource.getAutoTitle(rule.type),
            eventValueMap = this.triggerEventPicker.valueMap,
            description = title + " for field " + rule.fieldName + " on " + eventValueMap[rule.triggerEvent]
        ;

        if (rule.applyWhen) {
            description += " if " + isc.DataSource.getAdvancedCriteriaDescription(rule.applyWhen, this.dataSources || this.dataSource);
        }
        
        return description;
    },
    
    //> @method ruleEditor.validate()
    // Validate the current set of values for the rule.
    // @return (boolean) true if validation passed for all component forms, false otherwise.
    // @visibility rules
    //<
    
    validate : function () {
        var failed = false;
        // if name/description have been marked as required, enforce this
        if (this.nameForm) failed = this.nameForm.validate() == false;
        if (this.mainForm) failed = (this.mainForm.validate() == false) || failed;
        if (this.applyWhenForm && this.applyWhenForm.getValue("applyWhen")) {
            failed = (this.conditionalForm.validate() == false) || failed;
        }
        if (this.validatorForm) {
            failed = (this.validatorForm.validate() == false) || failed;
            if (this.valuesForm) failed = (this.valuesForm.validate() == false) || failed;
        }
        if (this.messageForm) failed = (this.messageForm.validate() == false) || failed;
        return !failed;
    },
    
    //> @method ruleEditor.setRule()
    // Show the specified rule in this ruleEditor
    // @param rule (Validator) Rule to edit.
    // @visibility rules
    //<
    // initTime param used internally
    setRule : function (rule, initTime) {
        
        this.validator = this.rule = rule;

        if (initTime) {
            this.validatorType = rule.type;
            this.applyWhen = rule.applyWhen;
            this.fieldName = rule.fieldName;
            this.locator = rule.locator;
            this.triggerEvent = rule.triggerEvent;
            // errorMessage is applied lazily to the messageForm when its initialized.
        } else {
            if (this.nameForm) {
                this.nameForm.setValue("name", rule.name);
                this.nameForm.setValue("description", rule.description);
            }
            
            if (rule.fieldName != null || rule.locator != null) {
                // we have to call both methods even if the property is null,
                // as we need to clear existing fieldName / locator
                // as well as apply new values
                this.setFieldName(rule.fieldName);
                this.setLocator(rule.locator);
            }
            
            this.setTriggerEvent(rule.triggerEvent);
            this.setValidatorType(rule.type);
            this.setApplyWhen(rule.applyWhen);
            this.messageForm.setValue("errorMessage", rule.errorMessage);

            this.setClauseAttributes(rule);
        }
    },
    //> @method ruleEditor.setValidator()
    // Show the specified validator in this ruleEditor. Synonym for setRule().
    // @param rule (Validator) Rule to edit.
    // @visibility rules
    //<
    setValidator : function (validator) {
    
        this.setRule(validator);
    },
    
    //> @method ruleEditor.clearRule()
    // Clear the ruleEditor's values (dropping the current rule entirely). Note that this will
    // clear all settings for the rule, including <code>fieldName</code>.
    // @visibility rules
    //<
    clearRule : function () {
        this.rule = this.validator = null;
        this.setFieldName(null);
        this.setValidatorType(null);
        this.setApplyWhen(null);
        this.setTriggerEvent(null);
        if (this.nameForm) this.nameForm.clearValues();
        if (this.messageForm) this.messageForm.clearValue("errorMessage");
    },
    
    //> @method ruleEditor.clearValidator()
    // Clear the ruleEditor's values (dropping the current validator entirely).
    // @visibility rules
    //<
    clearValidator : function () {
        return this.clearRule();
    }

});


if (isc.DynamicForm) {


// Custom form item types for editing built-in validator definition objects
// These are referred to via the "validator.editorType" attribute 


isc.defineClass("SubstringCountEditor", "CanvasItem").addProperties({
    
    canvasConstructor:"DynamicForm",
    canvasDefaults:{
        numCols:3
    },
    
    substringFieldDefaults:{
        name:"substring",
        showTitle:false, type:"text", colSpan:"*", width:"*"
    },
    countFieldDefaults:{
        name:"count", showTitle:false, hint:"Count", showHintInField:true, 
        width:50, type:"integer"
    },
    operatorFieldDefaults:{
        name:"operator", title:"Operator", editorType:"SelectItem",
        width:50,
        defaultValue:"==", allowEmptyValue:false,
        valueMap:["==", "!=", "<", "<=", ">", ">=" ]
    },
    createCanvas : function (form,item) {
        
        var substringField = isc.addProperties({}, 
                this.substringFieldDefaults, this.substringFieldProperties),
            countField = isc.addProperties({},
                this.countFieldDefaults, this.countFieldProperties),
            operatorField = isc.addProperties({},
                this.operatorFieldDefaults, this.operatorFieldProperties);
        
        return this.canvas = this.createAutoChild(
            "canvas", 
            { items:[
                    substringField,
                    countField,
                    operatorField
                ]
            }
        );
    }
});

isc.defineClass("FloatRangeEditor", "CanvasItem").addProperties({
    
    canvasConstructor:"DynamicForm",
    canvasDefaults:{
        numCols:2
    },
    minFieldDefaults:{
        name:"min",
        showTitle:false, type:"float",
        hint:"Min", showHintInField:true
    },
    maxFieldDefaults:{
        name:"max", 
        showTitle:false, type:"float",
        hint:"Max", showHintInField:true
    },
    exclusiveFieldDefaults:{
        name:"exclusive", title:"Exclusive", 
        colSpan:"*",
        prompt:"Range is exclusive (does not include min/max values)",
        type:"boolean",
        editorType:"CheckboxItem", defaultValue:false
    },
    createCanvas : function (form,item) {
        
        var minField = isc.addProperties({}, 
                 this.minFieldDefaults, this.minFieldProperties),
            maxField = isc.addProperties({},
                this.maxFieldDefaults, this.maxFieldProperties),
            exclusiveField = isc.addProperties({},
                this.exclusiveFieldDefaults, this.exclusiveFieldProperties);
        
        return this.canvas = this.createAutoChild(
            "canvas", 
            { items:[
                    minField,
                    maxField,
                    exclusiveField
                ]
            }
        );
    }
});

isc.defineClass("FloatPrecisionEditor", "CanvasItem").addProperties({
    
    canvasConstructor:"DynamicForm",
    canvasDefaults:{
        numCols:1
    },
    
    precisionFieldDefaults:{
        name:"precision",
        showTitle:false, type:"float",
        hint:"Precision", showHintInField:true
    },
    roundFieldDefaults:{
        showTitle:false,
        name:"roundToPrecision", title:"Round to precision", 
        type:"boolean",
        editorType:"CheckboxItem", defaultValue:false
    },
    createCanvas : function (form,item) {
        
        var precisionField = isc.addProperties({}, 
                this.precisionFieldDefaults, this.precisionFieldProperties),
            roundField = isc.addProperties({},
                this.roundFieldDefaults, this.roundFieldProperties);
        
        return this.canvas = this.createAutoChild(
            "canvas", 
            { items:[
                    precisionField, roundField
                ]
            }
        );
    }
});

isc.defineClass("MaskRuleEditor", "CanvasItem").addProperties({
    // Needs 2 strings - mask (a regex), and transformTo
    canvasConstructor:"DynamicForm",
    canvasDefaults:{
        numCols:1
    },
    
    maskFieldDefaults:{
        name:"mask", editorType:"TextItem",
        showTitle:false,
        hint:"mask", showHintInField:true
    },
    transformFieldDefaults:{
        name:"transformTo", editorType:"TextItem",
        showTitle:false,
        hint:"transformTo", showHintInField:true
    },
    createCanvas : function (form,item) {
        
        var maskField = isc.addProperties({}, 
                this.maskFieldDefaults, this.maskFieldProperties),
            transformField = isc.addProperties({},
                this.transformFieldDefaults, this.transformFieldProperties);
        
        return this.canvas = this.createAutoChild(
            "canvas", 
            { items:[
                    maskField, transformField
                ]
            }
        );
    }    
});

isc.defineClass("PopulateRuleEditor", "BlurbItem").addProperties({

    emptyFormulaText:"Click the icon to select a formula",
    formulaVarsTitle:"Formula Variables:",
    formulaTitle:"Formula:",
    editFormulaPrompt:"Click to edit formula",
    
    formatValue:function (value,record,form,item) {
        // rule is an object containing "formula" (string) and "formulaVars" (map)
        if (value == null || value.formula == null) {
            return this.emptyFormulaText;
        }
        var formulaVars = value.formulaVars,
            keys = isc.getKeys(formulaVars).sort(),
            variables = ""
        ;
        for (var i = 0; i < keys.length; i++) {
            var key = keys[i];
            variables += key +":&nbsp;" + formulaVars[key] + "\n";
        }

        return "<table class=" + this.getTextBoxStyle() + "><tr><td>" + this.formulaVarsTitle + "</td><td>" 
                + variables + "</td></tr>" +
                "<tr><td>" + this.formulaTitle + "</td><td>" + value.formula + "</td></tr></table>";        
    },
    icons:[
        {click:"item.showFormulaWindow()"}
    ],
    
    init:function () {
        this.icons[0].prompt = this.editFormulaPrompt;
        return this.Super("init", arguments);
    },
    
    formulaWindowConstructor:"Window",
    formulaWindowDefaults:{
        title: "Formula Editor",
        showMinimizeButton: false, showMaximizeButton: false,
        isModal: true, 
        showModalMask:true, 
        autoSize: true,
        autoCenter: true,
        autoDraw: true,
        headerIconProperties: { padding: 1,
            src: "[SKINIMG]ListGrid/formula_menuItem.png"
        },
        
        closeClick: function () {
            // call the method to cancel editing on the formulaBuilder. That'll automatically
            // dismiss this window.
            this.items.get(0).completeEditing(true);
        }

    },
    
    formulaBuilderConstructor:"FormulaBuilder",
    formulaBuilderDefaults:{
        width:300,
        // FormulaBuilders typically edit formula fields for components - but we just want UI
        // for creating and testing formulae.
        // Hide any UI to do with formula fields / source component fields
        showTitleField:false,
        showAutoHideCheckBox:false,

        // no need for "save and add another"! We'll use save/cancel
        // (since we show the thing in a popup)
        showSaveAddAnotherButton:false,
        
        fireOnClose:function () {
            this.creator.userEditComplete(!this.cancelled);
        }
    },
    
    showFormulaWindow : function () {
        if (this.formulaBuilder == null) {
            this.formulaBuilder = this.createAutoChild(
                "formulaBuilder",
                
                {dataSource:this.form.creator.dataSource, dataSources:this.form.creator.dataSources,
                 mathFunctions: isc.MathFunction.getDefaultFunctionNames()}
            );
            
            this.formulaWindow = this.createAutoChild("formulaWindow", {items:[this.formulaBuilder]});
        }
        // Clear the current value in the window if there is one.
        
        this.formulaBuilder.setValue("");
        this.formulaWindow.show();
    },
    
    userEditComplete : function (saveValue) {
        if (saveValue) {
            var formulaObj = this.formulaBuilder.getBasicValueObject(),
                formula,
                formulaVars;
            if (formulaObj != null) {
                formula = formulaObj.text;
                formulaVars = formulaObj.formulaVars;
            }
            if (formula != null) {
                this.storeValue({formula:formula, formulaVars:formulaVars});
            } else {
                this.storeValue(null);
            }
            this.redraw();
        }
        this.formulaWindow.clear();
    }
    
});

isc.defineClass("ReadOnlyRuleEditor", "SelectItem").addProperties({
    defaultValue:isc.Validator.READONLY,
    valueMap:[
        isc.Validator.HIDDEN,
        isc.Validator.DISABLED,
        isc.Validator.READONLY
    ]
});

}   // End of check for DynamicForm being defined
