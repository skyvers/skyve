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


//> @class Rule
// A rule is a declaration that on some triggering event, some standard action will occur.
// Most commonly, a form will change the display or behavior of one or more of its fields.  
// Examples include displaying a warning message,
// disabling a field, or populating a field with a calculated value. Some Rules can also act
// upon other components in the application, for example the +link{validatorType,readOnly rule}
// can be used to hide components as well as just FormItems.
// <P>
// A rule is a special-case of a +link{Validator}: validators are designed so that they always
// make sense to run where data is being saved and can either accept or reject the saved data.
// Rules can be triggered by actions other than saving (including button presses) and rather than
// accepting or rejecting values via a +link{validator.condition}, they run some action when 
// their trigger event fires, such as displaying informational messages, or
// automatically calculating suggested values for fields.
// <P>
// A validator can be identified as a rule by setting the flag "isRule" on the validator definition.
// <P>
// As with other validators, Rules may be applied directly to fields, or may be applied to the
// +link{RulesEngine.rulesData,rulesData} array of a RulesEngine.
// 
//
// @inheritsFrom Validator
// @visibility rules
//<



//> @attr validatorDefinition.isRule (boolean : false : IR) 
// Flag indicating validators of this type are +link{Rule}s
// @visibility rules
//<

//> @attr rule.type (RuleType : null : IR)
// Type of rule - see +link{type:RuleType}.  Any +link{type:ValidatorType} is valid as a rule type
// as well.
// @visibility rules
//<


//> @type RuleType
// @value "message" displays an informational or warning message without marking a field invalid
// @value "populate" applies a calculated value to a field based on values in other fields
// @value "setRequired" marks a field as being required
// @visibility rules
//<


//> @attr rule.fieldName (String | Array of String : null : IR)
// This attribute may be used to specify a target field for validators and rules applied
// within a +link{RulesEngine}.
// <P>
// Name of the field that this rule effects.  If this rule affects multiple fields (for example, a
// "readOnly" rule that disables editing on a whole set of fields), either a comma-separated list
// of fieldNames or an Array of fieldNames can be specified.
// @visibility rules
//<


//> @attr rule.locator (AutoTestObjectLocator : null : IR)
// This attribute may be use to specify a target object for rules within a +link{RulesEngine}.
// Only applies to validators specified with the +link{validatorDefinition.isRule} attribute
// set to true.
// <P>
// Locator for target object effected by this rule. This will typically be a locator that resolves
// to a +link{FormItem}, but note that depending on +link{validatorDefinition.supportedTargets},
// other target objects, such as +link{Canvas} instances may be used.
// <P>
// This locator may be specified as an absolute locator derived from 
// +link{AutoTest.getObjectLocator()}, or if +link{RulesEngine.baseComponent}
// is specified, a relative locator derived from +link{AutoTest.getRelativeLocator()} 
// should be used, and the +link{rulesEngine.baseComponent} will be used to resolve this 
// to a live object at runtime.
// @visibility rules
//<


//> @type ValidatorTargetType
// Used by +link{validatorDefinition.supportedTargets}.
// If a rule is specified with a +link{validator.locator}, what object type(s) does the
// validatorDefinition support? The target object will be passed to the +link{ruleDefinition.action()} 
// method so that method should be able to handle any specified type being passed in.
// @value FormItem
//  Validator supports FormItem targets. In this case the FormItem and its containerWidget will
//  be passed to the validator action method.
// @value Canvas
//  Validator supports a Canvas as a target. In this case the target canvas will be passed to the
//  ation method. No targetContainer will be passed for simple canvases.
// @value Section
//  Validator supports a SectionStackSection as a target. In this case the target Section will be
//  passed to the action method, and the stack will be passed as the targetContainer parameter.
//
// @visibility rules
//<



//> @attr rule.supportedTargets (Array of String : ["FormItem"] : IR)
// If +link{validator.locator} is used to specify a target object, what target types are
// supported? When the validator or rule is executed, the locator will be resolved to the
// object in question. If the object is of a type listed here, the object will be passed to
// the +link{validator.action} for processing, along with its container if appropriate.
// @visibility rules
//<



//> @attr rule.triggerEvent (TriggerEvent : "submit" : IR)
// Event that triggers this rule.  See +link{TriggerEvent}.
// @visibility rules
//<

//> @type TriggerEvent
// @value "editStart" Rule is triggered each time the form is populated with data (inclusive of
//                    being initialized to +link{dynamicForm.editNewRecord,edit a new record},
//                    such that all fields show their +link{FormItem.defaultValue,default value})
// @value "editStartAndChange" Rule is triggered each time the form is populated with data and
//                    whenever a changed event happens on it's field.
// @value "editorEnter" Rule is triggered when focus enters the field
// @value "editorExit" Rule is triggered when focus leaves the field
// @value "changed" Rule is triggered whenever a changed event happens on it's field
// @value "submit" Rule is triggered when values in the form are being submitted.  This includes
//                 both saving in a form that edits a record, or being submitted as search
//                 criteria in a search form
// @value "manual" Rule is never automatically fired and must be programmatically triggered
// @visibility rules
//<


//> @attr rule.dependentFields (String | Array of String : null : IR)
// For rules that are triggered by user actions in other fields, a list of fieldNames that can
// trigger the rule.  If multiple fields trigger the rule, either a comma-separated list of
// fieldNames or an Array of fieldNames can be specified. Notes:
// <ul>
// <li>If +link{rule.applyWhen} is specified, any fields referenced in the applyWhen conditional
//     are automatically included as implicit dependent fields, in addition to any fields specified
//     here</li>
// <li>If unset, +link{ruleDefinition.getDependentFields()} may be implemented to derive
//     dependent fields directly from the rule. For example the +link{validatorTypes,"populate" rule}
//     has this method defined to derive dependent fields from the rule's specified formula.</li>
// <li>If unset and no <code>getDependentFields()</code> implementation exists, the 
//     trigger event will be assumed to come from the +link{rule.fieldName} field</li>
// </ul>
//
// @visibility rules
//<

//> @method validatorDefinition.getDependentFields()
// Optional method to derive dynamic +link{rule.dependentFields,dependentFields} from a rule
// at runtime. This method should return the field names to use.
// @param rule (rule) rule in question
// @param event (TriggerEvent) triggerEvent for the rule in question
// @return (Array of String) dependent field names
// @visibility rules
//<

//> @attr rule.applyWhen (AdvancedCriteria : null : IRA)
// Used to create a conditional rule based on +link{AdvancedCriteria,criteria}.  The rule
// will only be triggered if the criteria match the current form values.
// @visibility rules
//<

//> @class RulesEngine
// The rulesEngine class applies +link{Rule,Rules} to fields displayed across dataBoundComponents
// @visibility rules
//<
isc.defineClass("RulesEngine");

isc.RulesEngine.addProperties({
    init : function () {
        this.Super("init", arguments);
        isc.ClassFactory.addGlobalID(this);
        
        if (this.members == null) this.members = [];
        else {
            for (var i = 0; i < this.members.length; i++) {
                var member = this.members[i];
                
                if (isc.isA.String(member)) {
                    this.members[i] = member = window[member];
                }
                this._addMember(member, true);
            }
        }
        
    },

    //> @attr rulesEngine.baseComponent (Canvas : null : IRW)
    // When finding the target of a rule via +link{rule.locator}, the baseComponent is used as
    // the starting point for resolving the locator if the locator is a relative 
    // +link{AutoTestObjectLocator}.  Not required if not using locators, or if
    // locators are absolute.
    // @see AutoTest.getObjectLocator()
    // @see AutoTest.getRelativeObjectLocator()
    //
    // @visibility rules
    //<
    
    //> @attr rulesEngine.members (Array of DataBoundComponents : null : IRW)
    // Array of +link{DataBoundComponent}s associated with this rulesEngine. The +link{rulesData,rules}
    // for this engine can be triggered by events generated from this set of components and
    // will act upon them, using +link{rule.fieldName} to find the appropriate component and
    // field to interact with.
    // <P>
    // Note that developers may attach members to a rulesEngine either by setting
    // +link{dataBoundComponent.rulesEngine}, or by including the component in the members array
    // for a rulesEngine.
    //
    // @visibility rules
    //<
    
    //> @method rulesEngine.addMember()
    // Add a member to +link{rulesEngine.members}.
    // @param member (DataBoundComponent) new member to add
    // @visibility rules
    //<
    addMember : function (member) {
        if (!this.members.contains(member)) {
            this.members.add(member);
            this._addMember(member);
        }
    },
    
    _addMember : function (member) {
        if (member.rulesEngine != this) {
            
            if (member.rulesEngine != null) {
                member.rulesEngine.removeMember(member);
            }
            member.rulesEngine = this;
        }
        
        
    },
    
    // Notification method that one of our members started editing a new set of values - 
    // called from DynamicForm.setValues()
    processEditStart : function (component) {
        this._processComponentTriggerEvent("editStart", component);
        this._processComponentTriggerEvent("editStartAndChanged", component);
    },
    
    // Notification when a field gets focus (editor enter)
    processEditorEnter : function (component, field) {
        this._processFieldTriggerEvent("editorEnter", component, field);
    },

    // Notification method that a field changed
    // Fired in response to 'changed' not 'change' since we need to extract values from the
    // DBC. Therefore no way to (for example) cancel the change.
    processChanged : function (component, field) {
        this._processFieldTriggerEvent("changed", component, field);
        this._processFieldTriggerEvent("editStartAndChanged", component, field);
    },
    
    // Notification method that a field lost focus
    processEditorExit : function (component, field) {
        this._processFieldTriggerEvent("editorExit", component, field);
    },
    
    // Notification that one of our members was submitted (fires after validation at the form level)
    processSubmit : function (component) {
        // In this case we return the result of the validation run. This allows the
        // calling form to cancel submit.
        
        return this._processComponentTriggerEvent("submit", component);            
    },
    
    // Actual code to fire 'processRules' on for rules associated with a trigger-event.
    _processComponentTriggerEvent : function (eventType, component) {
        var rules = this.rulesData;
        if (!rules || rules.length == 0) return;
        var eventTypeRules = [];
        
        for (var i = 0; i < rules.length; i++) {
            if (rules[i].triggerEvent == eventType) {
                eventTypeRules[eventTypeRules.length] = rules[i];
            }
        }
        
        if (eventTypeRules.length > 0) return this.processRules(eventTypeRules);
        return null;

    },
    
    _processFieldTriggerEvent : function (eventType, component, field) {
        var rules = this.rulesData;
        if (!rules || rules.length == 0) return;
        var eventTypeRules = [];
        for (var i = 0; i < rules.length; i++) {
            if (rules[i].triggerEvent == eventType) {
                var rule = rules[i],
                    ruleDefinition = isc.Validator.getValidatorDefinition(rules[i].type),
                    sourceField = null;
                    
                // Don't crash if an invalid rule type is specified.
                if (!ruleDefinition) {
                    this.logWarn("RulesEngine unable to process rule with invalid type: " + rules[i].type);
                    continue;
                }

                if (rule.dependentFields != null) {
                    sourceField = rule.dependentFields;
                } else if (ruleDefinition.getDependentFields != null) {
                    sourceField = ruleDefinition.getDependentFields(rule, eventType);
                }
                if (sourceField == null || isc.isAn.emptyArray(sourceField)) {
                    // Avoid altering existing rule fieldName array
                    sourceField = (isc.isAn.Array(rule.fieldName) ? rule.fieldName.duplicate() : rule.fieldName);
                }
                
                if (sourceField != null && !isc.isAn.Array(sourceField)) {
                    sourceField = sourceField.split(",");
                }
                
                // By default combine "applyWhen" source fields - this ensures if the trigger
                // condition changes we re-evaluate
                if (rule.applyWhen != null) {
                    var includeApplyWhenFields = true;
                    if (rule.dependsOnApplyWhenFields != null) {
                        includeApplyWhenFields = rule.dependsOnApplyWhenFields;
                    } else if (ruleDefinition.dependsOnApplyWhenFields != null) {
                        includeApplyWhenFields = rule.dependsOnApplyWhenFields;
                    }
                    if (includeApplyWhenFields) {
                        var criteriaFields = isc.DataSource.getCriteriaFields(rule.applyWhen);
                        if (sourceField == null) {
                            sourceField = criteriaFields;
                        } else {
                            sourceField.addList(criteriaFields);
                        }
                    }
                }
                
                if (sourceField) {
                    for (var j = 0; j < sourceField.length; j++) {
                        var dotIndex = sourceField[j].indexOf("."),
                            ds = sourceField[j].substring(0, dotIndex),
                            dsField = sourceField[j].substring(dotIndex+1);
                    
                        if (component.getDataSource() == isc.DataSource.get(ds) &&
                            dsField == field.name) 
                        {
                            eventTypeRules.add(rules[i]);
                            // drop out of the inner for-loop
                            break;
                        }
                    }
                }
            }
        }
        if (eventTypeRules.length > 0) return this.processRules(eventTypeRules);
        return null;
    },
    
    // getValues() Assembles a record values type object comprised of values from all 
    // member forms. This will be used by rule / validator logic.
    // Note that for databound forms we store the form values under the dataSource name
    // as an attribute on this object.
    getValues : function () {
        var record = {};
        for (var i = 0; i < this.members.length; i++) {
            var member = this.members[i];
            var values = member.getValues(),
                dataSource = member.getDataSource(),
                dsID = dataSource ? dataSource.getID() : null;
            
            if (dsID != null) {
                record[dsID] = isc.addProperties(record[dsID] || {}, values);
            } else {
                
                record.addProperties(values);
            }
        }
        
        return record;
    },
    
    //> @method rulesEngine.processRules()
    // Process a set of the rules present in +link{rulesEngine.rulesData}.
    // This method is invoked by the system when the appropriate +link{rule.triggerEvent} occurs
    // on +link{members} of this engine. It may also be called manually, passing in the
    // array of rules to process. This is how rules with <code>triggerEvent</code> set to
    // <code>"manual"</code> are processed.
    // @param rules (Array of Rules) Rules to process
    // @visibility rules
    //<
    
    processRules : function (rules) {
        if (rules == null) return;
        
        var values = this.getValues(),
            result = null;
        if (!isc.isAn.Array(rules)) {
            rules = [rules]
        }
        for (var i = 0; i < rules.length; i++) {
//            this.logWarn("processing rule:" + this.echo(rules[i]));
            var rule = rules[i],
                fieldNames = rule.fieldName,
                locator = rule.locator,
                shouldApply = true;
            // If rule.applyWhen is specified we can test this against the fulll set of values,
            // before spinning through individual targets, running the 'performAction' et al.
            if (rule.applyWhen) {
                var criteria = rule.applyWhen;
                // use the static "applyFilter" since we're gathering values from forms in
                // multiple dataSources
                
                var matchingRows = isc.DataSource.applyFilter([values], criteria);
                if (matchingRows.length == 0) {
                    shouldApply = false;
                }
            }
            
            // rules can apply to a field (specified as someDS.someFieldName) or to 
            // a (relative) locator which will return a SC object -- a component, a
            // FormItem or a SectionStackSection
            
            // If a locator is used, we need to find the relevant object and call the
            // appropriate API on it.
            // Note that many rule types don't apply to anything other than FormItems
            
            if (locator != null) {
                // support for multiple locators
                if (isc.isA.String(locator)) locator = locator.split(",");
                
                for (var j = 0; j < locator.length; j++) {
                    var currentLocator = locator[j],
                        isRelativeLocator = isc.AutoTest.isRelativeLocator(currentLocator),
                        targetObject;
                    if (!isRelativeLocator) {
                        targetObject = isc.AutoTest.getObject(locator);
                    } else {
                        if (this.baseComponent == null) {
                            this.logWarn("RulesEngine has no specified baseComponent. Unable to" +
                                " process rule with specified relative locator:" + currentLocator);
                            continue;
                        }
                        
                        targetObject = isc.AutoTest.resolveRelativeObjectLocator(
                                                this.baseComponent, currentLocator);
                        
                    }
                    if (targetObject == null) {
                        this.logWarn("RulesEngine unable to resolve locator specified on rule. " +
                            (this.baseComponent ? "\nBase Component: " + this.baseComponent : "") +
                            "\nLocator in question:\n" + currentLocator);
                        continue;
                    }
                    
                    
                    // Only "rules" have locators (validators do not).
                    // The distinction is that the validator has no "condition" and so no need
                    // to call "processValidator()" - just call performAction.
                    // However - first verify the targetObject type is supported by the
                    // validator.
                    
                    var targetObjectType,
                        // note that JS requires us to explicitly default this to null or it'll
                        // retain value from the previous run through the loop 
                        // (despite being "declared" here)
                        targetContainer = null;
                    if (isc.isA.FormItem(targetObject)) {
                        targetObjectType = "FormItem";
                        targetContainer = targetObject.containerWidget;
                        
                    } else if (targetObject.isSectionHeader) {
                        targetObjectType = "Section";
                        // targetObject will probably be the Canvas but in case
                        // we ever get a pointer to the actual config object, resolve it:
                        if (targetObject.getSectionHeader) {
                            targetObject = targetObject.getSectionHeader();
                        }
                        
                        targetContainer = targetObject.parentElement;
                    } else if (isc.isA.Canvas(targetObject)) {
                        targetObjectType = "Canvas";
                        // no 'container' - we'll just hide the canvas.
                    }
                    
                    
                    isc.Validator.performAction(shouldApply ? true : null, 
                        targetObject, rule, values, targetContainer, targetObjectType);
                }
            }
                
            // Support fieldName being a single fieldName, an array of fieldName strings, or
            // a comma-separated string.
            // Normalize to an array first.
            if (isc.isA.String(fieldNames)) {
                fieldNames = fieldNames.split(",");
            // handle locator with no specified fieldName
            } else if (fieldNames == null) {
                fieldNames = [];
            }
            for (var j = 0; j < fieldNames.length; j++) {
                var fieldName = fieldNames[j],
                    dsName = fieldName.substring(0,fieldName.indexOf(".")),
                    ds = dsName ? isc.DataSource.get(dsName) : null,
                    dsFieldName = fieldName.substring(dsName.length+1),
    
                    value = isc.DataSource.getPathValue(values, fieldName),
    
                    componentInfo = this.getComponentInfo(fieldName),
                    component = componentInfo ? componentInfo.component: null,
                    field = componentInfo ? componentInfo.item : 
                                (ds ? ds.getField(dsFieldName) : null);
                ;
                
                
                
                if (component == null || field == null) {
                    this.logWarn("RulesEngine contains rule definition with specified fieldName:"
                            + fieldName + " - unable to find associated " + 
                             (component == null ? "member component" : "field") + " for this rule.");
                    continue;
                }

                var isValid = null;
                if (shouldApply) {
                    isValid = 
                        (isc.Validator.processValidator(field, rule, value, null, values) == true);
                }
                isc.Validator.performAction(isValid, field, rule, values, component);
                
                

                
                if (isValid == false) {
                    result = false;
                    var errorMessage = isc.Validator.getErrorMessage(rule);
                    component.addFieldErrors(field.name, errorMessage, true);
                    this.rememberRuleFieldError(rule, component, field, errorMessage);
                } else {
                    var currentError = this.getRuleFieldError(rule, component, field);
                    
                    if (currentError && component.clearFieldError) {
                        component.clearFieldError(field.name, currentError, true);
                        this.clearRememberedRuleFieldError(rule, component, field);
                    }
                    if (result == null) result = true;
                }
            }
        }
        return result;
    },
    
    rememberRuleFieldError : function (rule, component, field, errorMessage) {
        // Hang onto the applied error string. We'll selectively clear it below if
        // the triggerEvent runs and validation passes
        
        if (rule._currentErrors ==  null) {
            rule._currentErrors = {};
        }
        var componentID = component.getID();
        if (rule._currentErrors[componentID] == null) {
            rule._currentErrors[componentID] = {};
        }
        rule._currentErrors[componentID][field.name] = errorMessage;    
    },
    getRuleFieldError : function (rule, component, field) {
        var componentID = component.getID();
        if (rule._currentErrors && rule._currentErrors[componentID] 
            && rule._currentErrors[componentID][field.name] != null) 
        {
            return rule._currentErrors[componentID][field.name];
            
        }    
    },
    clearRememberedRuleFieldError : function (rule, component, field) {
        var componentID = component.getID();
        if (rule._currentErrors && rule._currentErrors[componentID] 
            && rule._currentErrors[componentID][field.name] != null) 
        {
            delete rule._currentErrors[componentID][field.name];
        }
    },
    
    // When a rulesEngine applies validators to a field (running in response to a triggerEvent), 
    // errors will be applied to the component.
    // If the component is then actually submitted or explicitly 'validated', all errors are
    // cleared and rebuilt from the field.validators
    // This is a notification fired by the DBC when this happens. It gives us a chance to
    // re-run validators for the field in question and re-apply errors if the value would
    // still fail validation.
    
    applyFieldValidators : function (errors, component) {
        var rules = this.rulesData;
        var values = this.getValues();
        var addedErrors = false;
        for (var i = 0; i < rules.length; i++) {
            var rule = rules[i],
                targetField = rule.fieldName;
            // Assertion: If there's no rule.fieldName this isn't a validator to
            // apply to an item so will be uneffected by the form's "validate()" method
            if (targetField == null) continue;
            
            var componentInfo = this.getComponentInfo(targetField);
            // Assertion: We are passed the component being validated. Ignore any rules
            // where the target field is not held by the component.
            if (componentInfo == null || componentInfo.component != component) continue;
            
            var field = componentInfo.item,
                value = isc.DataSource.getPathValue(values, targetField);
            if (!field) return;
            
            // Drop any "remembered" error that we've applied before. It was already cleared
            // by the logic in the DF code.
            this.clearRememberedRuleFieldError(rule, component, field);

            var shouldApply = true;
            if (rule.applyWhen) {
                var criteria = rule.applyWhen;
                var matchingRows = isc.DataSource.applyFilter([values], criteria);
                if (matchingRows.length == 0) {
                    shouldApply = false;
                }
            }
            // Don't do anything if 'shouldApply' is false. The normal form validation code
            // will have already cleared any error so we can just ignore the validator.
            if (!shouldApply) continue;
            
            var isValid = isc.Validator.processValidator(field, rule, value, null, values) == true;
            if (!isValid) {
                var errorMessage = isc.Validator.getErrorMessage(rule);
                if (errors[field.name] == null) errors[field.name] = errorMessage;
                else {
                    if (!isc.isAn.Array(errors[field.name])) {
                        errors[field.name] = [errors[field.name]];
                    }
                    errors[field.name].add(errorMessage);
                }
                // Remember the applied error message so we can selectively clear it if necessary.
                this.rememberRuleFieldError(rule, component, field, errorMessage);
                addedErrors = true;
            }
        }
        
        return addedErrors;
    },
    
    // Method to take a fieldName (including DS name) like "supplyItem.SKU" and find the
    // associated component and item.
    // Returns an object like {component:<dynamicFormInstance>, item:<formitemInstance>}
    getComponentInfo : function (fieldName) {
        var item,
            index = fieldName.indexOf("."),
            dataSource;
        if (index != -1) {
            dataSource = isc.DataSource.get(fieldName.substring(0, index));
            fieldName = fieldName.substring(index+1);
        }
        
        
        for (var i = 0; i < this.members.length; i++) {
            if (this.members[i].getDataSource() == dataSource) {
                item = this.members[i].getItem(fieldName);
                if (item != null) {
                    return {component:this.members[i], item:item};
                }
            }
        }
        
    },

    //> @method rulesEngine.removeMember()
    // Removes a dataBoundComponent from this engine's +link{members} array.
    // @param member (DataBoundComponent) member to remove
    // @visibility rules
    //<
    removeMember : function (member) {
        if (this.members.contains(member)) {
            this._removeMember(member);
        }
    },
    _removeMember : function (member) {
        this.members.remove(member);
        member.rulesEngine = null;
    },
    
    setRulesData : function (rulesData) {
        this.rulesData = rulesData;
    }
});
