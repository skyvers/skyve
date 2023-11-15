/*

  SmartClient Ajax RIA system
  Version v13.0p_2023-11-09/LGPL Deployment (2023-11-09)

  Copyright 2000 and beyond Isomorphic Software, Inc. All rights reserved.
  "SmartClient" is a trademark of Isomorphic Software, Inc.

  LICENSE NOTICE
     INSTALLATION OR USE OF THIS SOFTWARE INDICATES YOUR ACCEPTANCE OF
     ISOMORPHIC SOFTWARE LICENSE TERMS. If you have received this file
     without an accompanying Isomorphic Software license file, please
     contact licensing@isomorphic.com for details. Unauthorized copying and
     use of this software is a violation of international copyright law.

  DEVELOPMENT ONLY - DO NOT DEPLOY
     This software is provided for evaluation, training, and development
     purposes only. It may include supplementary components that are not
     licensed for deployment. The separate DEPLOY package for this release
     contains SmartClient components that are licensed for deployment.

  PROPRIETARY & PROTECTED MATERIAL
     This software contains proprietary materials that are protected by
     contract and intellectual property law. You are expressly prohibited
     from attempting to reverse engineer this software or modify this
     software for human readability.

  CONTACT ISOMORPHIC
     For more information regarding license rights and restrictions, or to
     report possible license violations, please contact Isomorphic Software
     by email (licensing@isomorphic.com) or web (www.isomorphic.com).

*/

if(window.isc&&window.isc.module_Core&&!window.isc.module_Workflow){isc.module_Workflow=1;isc._moduleStart=isc._Workflow_start=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc._moduleEnd&&(!isc.Log||(isc.Log && isc.Log.logIsDebugEnabled('loadTime')))){isc._pTM={ message:'Workflow load/parse time: ' + (isc._moduleStart-isc._moduleEnd) + 'ms', category:'loadTime'};
if(isc.Log && isc.Log.logDebug)isc.Log.logDebug(isc._pTM.message,'loadTime');
else if(isc._preLog)isc._preLog[isc._preLog.length]=isc._pTM;
else isc._preLog=[isc._pTM]}isc.definingFramework=true;





// --------------------------------------------------------------------------------------------
//> @class ProcessElement
// A ProcessElement is an abstract superclass for elements involved in a +link{Process}, such
// as a +link{Task} or +link{DecisionTask}.
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("ProcessElement");

isc.ProcessElement.addClassMethods({

    getTitle : function () {
        var title = this.getInstanceProperty("typeTitle");
        if (!title) {
            title = this.getClassName();
            if (title.endsWith("Task")) title = title.substring(0,title.length-4);
            title = isc.DataSource.getAutoTitle(title);
        }
        return title;
    },

    // Create init-time properties for ProcessElement from a given action.
    // Dynamic values passed from the event are mapped when the function is created.
    // See Function._workflowActionToExpressionString().
    createInitPropertiesFromAction : function (action, targetMethod, sourceComponent) {
        var actionMapping = this.actionMapping,
            properties = {}
        ;
        if (!actionMapping) return null;

        if (actionMapping.target) {
            properties[actionMapping.target] = action.target;
        }
        if (actionMapping.source) {
            properties[actionMapping.source] = sourceComponent && sourceComponent.ID;
        }

        if (action.mapping && actionMapping.paramMap) {
            var sourceMap = action.mapping,
                targetMap = actionMapping.paramMap,
                mapping = []
            ;
            for (var i = 0; i < action.mapping.length; i++) {
                if (sourceMap[i] && sourceMap[i] != "null" && targetMap[i]) {
                    mapping.add(sourceMap[i] + ":" + targetMap[i]);
                }
            }
            if (!mapping.isEmpty()) properties.mapping = mapping;
        }
        return properties;
    }
});

isc.ProcessElement.addProperties({
    //> @attr processElement.ID (String : null : IR)
    // Optional ID for this process element, allowing it to be referred to from
    // +link{MultiDecisionTask,Decisions}, or as the +link{process.startElement}.
    // See +link{ProcessSequence} and +link{Process} to understand when this is required
    // or can be omitted.
    // <P>
    // Unlike +link{Canvas.ID} a <code>processElement</code>'s is a not a globally unique
    // variable, it need only by unique within it's process.
    // <P>
    // When assigned an ID, a <code>processElement</code> can be retrieve via
    // +link{process.getElement()}.
    // @visibility workflow
    //<

    //> @attr processElement.nextElement (String : null : IR)
    // Next +link{process.sequences,sequence} or +link{process.elements,element} to execute
    // after this one completes.
    // <p>
    // <code>nextElement</code> does not need to be specified on most elements if you use
    // +link{Process.sequences,sequences}.
    // <p>
    // Note that if there is both a <code>sequence</code> and a normal <code>element</code>
    // with the same name in the current <code>Process</code>, the <code>sequence</code> will
    // be used.
    //
    // @visibility workflow
    //<

    //> @attr processElement.description (String : null : IR)
    // Optional description for this specific instance of process element.
    //
    // @visibility workflow
    //<

    //> @attr processElement.typeTitle (String : null : IR)
    // Optional short, descriptive title for this process element. Used by an editor as a title
    // for process elements of this type.
    //
    // @visibility workflow
    //<

    //> @attr processElement.classDescription (String : null : IR)
    // Optional description of the general nature of the kinds of tasks this this process
    // element performs. Not to be confused with +link{description,description} which describes
    // what the specific instance of the process element has been configured to do.
    // <P>
    // For example, the <code>classDescription</code> for a task to disable a field might be
    // "disables a field" whereas the <code>description</code> for a concrete instance might
    // be "disables the 'shipTo' field in the 'ordering' form".
    // <P>
    // Used by editor to display additional details along with +link{typeTitle,typeTitle}.
    //
    // @visibility workflow
    //<

    //> @attr processElement.mockMode (Boolean : null : IRW)
    // Enable mock mode on the task? If +link{process.mockMode} is enabled, setting this
    // property to <code>false</code> disables mockMode on this task only. Otherwise,
    // mock mode can be enabled on this task by setting it to <code>true</code>.
    // <p>
    // Note that it is up to each task determine what effect mock mode has.
    //
    // @visibility workflow
    //<

    isMockModeEnabled : function (process) {
        return ((process && process.mockMode && this.mockMode != false) || this.mockMode)
    },

    //> @attr processElement.bindOutput (String : null : IR)
    // When set, the output of the task will be automatically bound to the specified
    // value in the +link{process.state,process state}.
    // <P>
    // See +link{group:taskInputExpression,taskInputExpressions} for details on the
    // transient state outputs.
    //
    // @visibility workflow
    //<

    //> @attr processElement.passThruOutput (Boolean : true : IR)
    // Does this processElement pass through output from the last executed task
    // (i.e. transient state)?
    // <P>
    // See +link{group:taskInputExpression,taskInputExpressions} for details on the
    // transient state outputs.
    // <p>
    // Note that this property does not affect the task at all but is an indicator
    // to the user and to the workflow editor of the behavior of the task as coded
    // (See +link{process.passThruTaskOutput}).
    //
    // @visibility workflow
    //<
    passThruOutput: true,

    //> @attr processElement.supportsMultipleInputRecords (Boolean : null : IR)
    // Does this processElement support being called multiple times for multiple records
    // in the +link{process.setTaskOutput,last task output}?
    // <p>
    // By default a processElement is +link{executeElement,executed} exactly once, however,
    // for a task that can process records from the last task output it can be useful
    // to handle each incoming record individually. Setting this property indicates to the
    // +link{class:Process,process} that if the last task output is an array, it should
    // be executed once per value in the array. Normal processing of
    // +link{group:taskInputExpression,taskInputExpressions} or use of
    // +link{process.getLastTaskOutput} will have exactly one record except uses of the
    // output for criteria values where the full output is used at once.
    // <p>
    // Processing of the task can determine that multiple incoming records should not
    // result in multiple calls and set +link{forceSingle}. For example, a task that
    // uses last task output for a criteria or for values should set
    // <code>forceSingle=true</code> when a criteria is used because multiple calls do not
    // make sense.
    //
    // @visibility workflow
    //<

    //> @attr processElement.forceSingle (Boolean : null : IRW)
    // Should +link{supportsMultipleInputRecords,multiple record processing} be suppressed
    // for this task instance? This property can be set at any time is checked before
    // executing the task and after each execution during processing of multiple last task
    // output records.
    // <p>
    // Note that since this property applies to an instance of a task that could be used
    // multiple times in a process (by branching) care should be taken to restore the
    // property value after execution completes. See +link{completeElement} or
    // +link{reset}.
    //
    // @visibility workflow
    //<

    //> @attr processElement.editorType (String : null : IR)
    // Editor type used to edit instances of this type of process element.
    //
    // @visibility workflow
    //<

    //> @method processElement.getEditorType()
    // Returns the workflow task editor type to be used edit instances of this type of
    // process element. The default implementation returns <code>this.editorType</code> but
    // a custom override could determine an editor type based on the property values.
    //
    // @visibility workflow
    //<
    getEditorType : function () {
        return this.editorType;
    },

    //> @method processElement.executeElement()
    // Method called by +link{Process} to have the processElement perform its work. There is
    // no default implementation by ProcessElement, however, all of the system-provided
    // subclasses do implement this method. An implementation or override of this method is
    // one possible customization point. Some classes like +link{ScriptTask} perform other
    // means to add customization. For ScriptTask, custom code is expected to handle the
    // +link{scriptTask.execute} method instead.
    // <p>
    // Any implementation of this method must return <code>true</code> if all the work this
    // element needed to perform was completed. Return <code>false</code> if additional work
    // is being performed asynchronously and the process should be paused until element
    // restarts it. Once asynchronous work is complete the task must call
    // +link{process.start()} to restart the workflow with the next task.
    //
    // @param process (Process) the process that is handling the workflow
    // @return (Boolean) return true if all the work this element needed to perform was
    //                   completed. Return false if additional work is being performed
    //                   asynchronously and the process should be paused until element
    //                   restarts it.
    // @visibility workflow
    //<

    //> @method processElement.getElementDescription()
    // Returns a text description of the element derived from
    // the configuration.
    // <p>
    // If no override is provided by the concrete ProcessElement
    // implementation the +link{description} is
    // returned.
    //
    // @return (String) the derived element description
    // @visibility workflow
    //<
    getElementDescription : function () {
        return this.description;
    },

    // When serializing, if the element description has not been changed from the generated
    // one, suppress it
    getSerializeableFields : function () {
        var result = this.Super("getSerializeableFields", arguments);
        if (result.description && result.description == this.getElementDescription()) {
            delete result.description;
        }
        return result;
    },

    _resolveCriteriaExpressions : function (criteria, inputData, inputRecord, process) {
        // Clone the criteria so that another execution can
        // resolve dynamicExpressions based on the latest inputData
        criteria = isc.clone(criteria);
        if (isc.DS.isAdvancedCriteria(criteria)) {
            this._resolveAdvancedCriteriaExpressions(criteria, inputData, inputRecord, process);

            if (process.ruleScope) {
                var ruleScopeComponent = window[process.ruleScope];
                if (!ruleScopeComponent || ruleScopeComponent.destroyed) {
                    this.logWarn("Attempt to resolve ruleScope references in taskInputExpression but ruleScope not found: " + process.ruleScope);
                } else {
                    criteria = isc.DS.resolveDynamicCriteria(criteria, ruleScopeComponent.getRuleContext());
                }
            }

        } else {
            criteria = this._resolveObjectDynamicExpressions(criteria, inputData, inputRecord, process);
        }
        return criteria;
    },

    _resolveAdvancedCriteriaExpressions : function (criteria, inputData, inputRecord, process) {
        var operator = criteria.operator;
        if (operator == "and" || operator == "or" || operator == "not") {
            var innerCriteria = criteria.criteria;
            if (!isc.isAn.Array(innerCriteria)) innerCriteria = [innerCriteria];
            for (var i = 0; i < innerCriteria.length; i++) {
                this._resolveAdvancedCriteriaExpressions(innerCriteria[i], inputData, inputRecord, process);
            }
        } if (criteria.value != null) {
            var operator = criteria.operator && isc.DS._operators[criteria.operator],
                allowMultipleValues = operator && operator.valueType == "valueSet"
            ;
            criteria.value = this._resolveDynamicExpression(criteria.value, inputData, inputRecord, process, allowMultipleValues);
        }
    },


    _resolveObjectDynamicExpressions : function (object, inputData, inputRecord, process) {
        var newObject = {};
        for (var key in object) {
            newObject[key] = this._resolveDynamicExpression(object[key], inputData, inputRecord, process, null, 1);
        }
        for (var key in newObject) {
            newObject[key] = this._resolveDynamicExpression(newObject[key], inputData, inputRecord, process, null, 2);
        }
        return newObject;
    },

    _resolveDynamicExpression : function (value, inputData, inputRecord, process, allowMultipleValues, pass) {
        if ((pass == null || pass == 1) && isc.isA.String(value)) {
            if (value.startsWith("\\$")) {
                // Allow escape to prevent interpretation of the leading $ below. For example
                // $last.name might be a desired literal value. "\$last.name" would treat it
                // as such.
                value = value.substring(1);
            } else if (inputRecord && value.startsWith("$inputRecord")) {
                if (inputRecord) {
                    var dataPath = value.replace("$inputRecord", "state");
                    value = isc.Canvas._getFieldValue(dataPath, null, {state: inputRecord});
                }
            } else if (inputData && value.startsWith("$input")) {
                if (inputData) {
                    var dataPath = value.replace("$input", "state");
                    value = isc.Canvas._getFieldValue(dataPath, null, {state: inputData});
                }
            } else if (process && value.startsWith("$last")) {
                value = value.substring(5);
                var last;
                if (value.startsWith("[")) {
                    var key = value.substring(1, value.indexOf("]"));
                    value = value.substring(value.indexOf("]")+1);
                    last = process.getLastTaskOutput(key);
                } else {
                    last = process.getLastTaskOutput();
                }
                if (value.startsWith(".")) {
                    var dataPath = "state" + value;
                    if (allowMultipleValues && isc.isAn.Array(last)) {
                        var values = [];
                        for (var i = 0; i < last.length; i++) {
                            var lastRecord = last[i];
                            value = isc.Canvas._getFieldValue(dataPath, null, {state: lastRecord});
                            if (value != null) {
                                values.add(value);
                            } else {
                                // Determine if an intermediate section of the path is missing or
                                // just the final field. If part of the path is missing log a warning
                                var testPath = dataPath.substring(0, dataPath.lastIndexOf("."));
                                if (!isc.Canvas._fieldHasValue(testPath, null, {state: lastRecord})) {
                                    this.logWarn(this.getClassName() + " taskInputExpression path " + dataPath + " not found in previous task output");
                                }
                            }
                        }
                        value = values;
                    } else {
                        if (isc.isAn.Array(last)) {
                            last = last[0];
                        }
                        value = isc.Canvas._getFieldValue(dataPath, null, {state: last});
                        if (value == null) {
                            // Determine if an intermediate section of the path is missing or
                            // just the final field. If part of the path is missing log a warning
                            var testPath = dataPath.substring(0, dataPath.lastIndexOf("."));
                            if (!isc.Canvas._fieldHasValue(testPath, null, {state: last})) {
                                this.logWarn(this.getClassName() + " taskInputExpression path " + dataPath + " not found in previous task output");
                            }
                        }
                    }
                } else {
                    value = last;
                }
            } else if (value.startsWith("$ruleScope.") || value.startsWith("$scope.")) {
                if (!process.ruleScope) {
                    this.logWarn("Attempt to reference ruleScope in taskInputExpression but no ruleScope has been defined");
                    value = null;
                } else {
                    var ruleScopeComponent = window[process.ruleScope];
                    if (!ruleScopeComponent || ruleScopeComponent.destroyed) {
                        this.logWarn("Attempt to reference ruleScope in taskInputExpression but ruleScope not found: " + process.ruleScope);
                        value = null;
                    } else {
                        var dataPath = value.replace("$ruleScope", "").replace("$scope", "");
                        if (dataPath.startsWith(".")) dataPath = dataPath.substring(1);
                        value = ruleScopeComponent._getFromRuleContext(dataPath);
                    }
                }
            } else if (value == "$state") {
                value = process.state;
            } else if (value.startsWith("$state.")) {
                var dataPath = value.substring(1);
                value = isc.Canvas._getFieldValue(dataPath, null, {state: process.state});
            }
        } else if ((pass == null || pass == 2) && isc.isAn.Object(value) && value.text) {
            var ruleScopeComponent = window[process.ruleScope];
            if (!ruleScopeComponent || ruleScopeComponent.destroyed) {
                this.logWarn("Attempt to resolve ruleScope references in textFormula " +
                                "but ruleScope not found: " + process.ruleScope);
                // No translation possible
                value = null;
            } else {
                var lastTaskOutput = process.getLastTaskOutput(),
                    lastTaskOutputSchema = process.getLastTaskOutputSchema(null, true),
                    removeRuleContext = false
                ;
                if (lastTaskOutput != null && lastTaskOutputSchema) {
                    // Push last task output into ruleContext so it can be picked up by
                    // the formula creation below. Do not notify any observers of ruleContext
                    // changes because this is not meant to be seen by anyone else

                    // For an array lastTaskOutput only push the first value into the
                    // ruleContext. This occurs for the first call to a task that accepts
                    // multiple inputs so it can determine if it consumes all records at
                    // once or wants to be called multiple times. If we get to this point
                    // we will only be handling the first record.
                    if (isc.isAn.Array(lastTaskOutput)) {
                        lastTaskOutput = lastTaskOutput[0];
                    }
                    ruleScopeComponent.provideRuleContext(this._$lastTaskRuleContextPath,
                        lastTaskOutput, lastTaskOutputSchema, null, true);
                    removeRuleContext = true;
                }

                // lazily create and cache the summary function (the vars will not change post
                // create)
                if (value._func == null) {
                    // Use SummaryBuilder.generateFunction() to convert the summary object to a function.
                    var builderClass = (value.formulaVars ?
                            isc.ClassFactory.getClass("FormulaBuilder") :
                            isc.ClassFactory.getClass("SummaryBuilder"));
                    value._func = builderClass.generateRuleScopeFunction(
                        value,
                        ruleScopeComponent.getID(),
                        null,
                        null,
                        false
                    );
                }

                if (value._func) {
                    var formula = value,
                        text = formula.text
                    ;
                    // Default value to null unless the formula can be parsed and used
                    value = null;

                    // Summary function for ruleScope expects the ruleScope
                    // variables to be inlined instead of using a var mapping.
                    // If a mapping is provided, update the formula by expanding
                    // the mapping vars.
                    if (formula.summaryVars) {
                        var vars = formula.summaryVars;
                        for (var mappingKey in vars) {
                            var replace = "#{" + vars[mappingKey] + "}";
                            text = isc.FormulaBuilder.handleKeyExp(text, mappingKey, "escaped", replace);
                            text = isc.FormulaBuilder.handleKeyExp(text, mappingKey, "braced", replace);
                        }
                        formula.text = text;
                    }

                    var ruleContext = ruleScopeComponent.getRuleContext();
                    value = formula._func(ruleContext);
                }

                if (removeRuleContext) {
                    ruleScopeComponent.provideRuleContext(this._$lastTaskRuleContextPath,
                        null, null, null, true);
                }
            }
        }
        return value;
    },

    //> @method processElement.objectReferencesLastTaskOutput
    // Does the object have fields that reference the last task output (i.e. $last)?
    //
    // @param object (Object) object to be checked
    // @param process (Process) the enclosing process
    // @return (Boolean) true if any field in the object references $last
    // @visibility workflow
    //<
    objectReferencesLastTaskOutput : function (object, process) {
        var referencesLast;
        // To reference $last a process must be provided
        if (process) {
            for (var key in object) {
                var value = object[key];
                if (isc.isA.String(value) && value.startsWith("$last")) {
                    referencesLast = true;
                    break;
                }
            }
        }
        return referencesLast;
    },

    //> @method processElement.updateGlobalIDReferences()
    // Updates references to a global ID within the properties of this process element
    // (i.e. rename). This method is not called as part of workflow execution but is used
    // by +link{Reify} to keep workflow event handlers in sync with ID changes within the
    // screen.
    // <p>
    // Each processElement or Task that has properties that save global IDs (like a
    // component ID or criteria referencing +link{canvas.ruleScope,ruleContext}) must
    // be able to update its references on demand by overriding this method or defer to its
    // superclass.
    // <p>
    // There are a number of helper methods to make this easier listed below.
    //
    // @param oldId (Identifier) the ID being renamed
    // @param newId (Identifier) the new ID to be assigned
    // @return (Boolean) true if any references were updated; false otherwise
    // @see updateGlobalIDInValueProperty
    // @see updateGlobalIDInValues
    // @see updateGlobalIDInCriteria
    // @visibility workflow
    //<
    updateGlobalIDReferences : function (oldId, newId) {
        // Nothing to do for a ProcessElement. Overridden by subclasses when needed.
        return false;
    },

    //> @method processElement.updateGlobalIDInValueProperty()
    // Updates a +link{group:taskInputExpression} property value containing ruleScope references.
    // <p>
    // This method is a helper to implement task-specific +link{updateGlobalIDReferences()}.
    //
    // @param propertyName (String) the property name to be updated in this task
    // @param oldId (Identifier) the ID being renamed
    // @param newId (Identifier) the new ID to be assigned
    // @return (Boolean) true if any references were updated; false otherwise
    // @visibility workflow
    //<
    updateGlobalIDInValueProperty : function (propertyName, oldId, newId) {
        var changed = false;
        if (this[propertyName]) {
            var key = this[propertyName];
            if (isc.isA.String(key)) {
                var newKey = key.replace("$ruleScope." + oldId + ".", "$ruleScope." + newId + ".")
                    .replace("$scope." + oldId + ".", "$scope." + newId + ".")
                if (key != newKey) {
                    this[propertyName] = newKey;
                    changed = true;
                }
            }
        }
        return changed;
    },

    //> @method processElement.updateGlobalIDInValues()
    // Updates a set of +link{group:taskInputExpression} values containing ruleScope references.
    // <p>
    // This method is a helper to implement task-specific +link{updateGlobalIDReferences()}.
    //
    // @param values (Object) the object to be updated
    // @param oldId (Identifier) the ID being renamed
    // @param newId (Identifier) the new ID to be assigned
    // @return (Boolean) true if any references were updated; false otherwise
    // @visibility workflow
    //<
    updateGlobalIDInValues : function (values, oldId, newId) {
        var changed = false;
        if (values) {
            for (var key in values) {
                var value = values[key];
                if (value != null && isc.isA.String(value)) {
                    var newValue = value.replace("$ruleScope." + oldId + ".", "$ruleScope." + newId + ".")
                            .replace("$scope." + oldId + ".", "$scope." + newId + ".")
                    ;
                    if (value != newValue) {
                        values[key] = newValue;
                        changed = true;
                    }
                }
            }
        }
        return changed;
    },

    //> @method processElement.updateGlobalIDInCriteria()
    // Updates +link{AdvancedCriteria} +link{Criterion} +link{group:taskInputExpression} values
    // containing ruleScope references.
    // <p>
    // This method is a helper to implement task-specific +link{updateGlobalIDReferences()}.
    //
    // @param criteria (AdvancedCriteria) the criteria to be updated in place
    // @param oldId (Identifier) the ID being renamed
    // @param newId (Identifier) the new ID to be assigned
    // @return (Boolean) true if any references were updated; false otherwise
    // @visibility workflow
    //<
    updateGlobalIDInCriteria : function (criteria, oldId, newId) {
        if (!criteria) return false;
        var changes = [{
            pattern: new RegExp("^\\$ruleScope\\." + oldId + "\\."),
            replacement: "$ruleScope." + newId + "."
        },{
            pattern: new RegExp("^\\$scope\\." + oldId + "\\."),
            replacement: "$scope." + newId + "."
        }];
        return this._replaceCriteriaValues(criteria, changes);
    },

    _replaceCriteriaValues : function (criteria, changes) {
        var operator = criteria.operator,
            changed = false
        ;
        if (operator == "and" || operator == "or") {
            var innerCriteria = criteria.criteria;
            for (var i = 0; i < innerCriteria.length; i++) {
                if (this._replaceCriteriaValues(innerCriteria[i], changes)) {
                    changed = true;
                }
            }
        } else {
            for (var i = 0; i < changes.length; i++) {
                var change = changes[i];
                if (criteria.value != null && isc.isA.String(criteria.value)) {
                    var newValue = criteria.value.replace(change.pattern, change.replacement);
                    if (criteria.value != newValue) {
                        criteria.value = newValue;
                        changed = true;
                    }
                }
            }
        }
        return changed;
    },

    //> @method processElement.updateLastElementBindingReferences()
    // Update references to a binding $last within properties of this processElement.
    // This method is not called as part of workflow execution but is used
    // by the +link{WorkflowEditor} to adjust last task references as new tasks are inserted.
    // <p>
    // Each processElement or Task that has properties supporting
    // +link{group:taskInputExpression,taskInputExpressions} using the $last syntax must
    // be able to update its references on demand by overriding this method or defer to its
    // superclass.
    // <p>
    // There are a number of helper methods to make this easier listed below.
    //
    // @param oldId (Identifier) the ID being renamed
    // @param newId (Identifier) the new ID to be assigned
    // @return (Boolean) true if any references were updated; false otherwise
    // @see updateLastElementInValueProperty
    // @see updateLastElementInValues
    // @see updateLastElementInCriteria
    // @visibility workflow
    //<
    updateLastElementBindingReferences : function (taskType) {
        // Nothing to do for a ProcessElement. Overridden by subclasses when needed.
        return false;
    },

    //> @method processElement.updateLastElementInValueProperty()
    // Updates a +link{group:taskInputExpression} property value containing $last references.
    // Any implicit reference to the last task is updated to reference a last task of a
    // specified <code>taskType</code>.
    // <p>
    // For example, a value of "$last.sequenceNo" would be replaced with
    // "$last[fetch].sequenceNo" if the taskType is "fetch". Existing "$last[...]" references
    // are left as-is.
    // <p>
    // This method is a helper to implement task-specific +link{updateLastElementBindingReferences()}.
    //
    // @param propertyName (String) the property name to be updated in this task
    // @param taskType (String) the taskType to be used in new reference
    // @return (Boolean) true if any references were update; false otherwise
    // @visibility workflow
    //<
    updateLastElementInValueProperty : function (propertyName, taskType) {
        var changed = false;
        if (this[propertyName]) {
            var key = this[propertyName],
                newKey = key.replace("$last.", "$last[" + taskType + "].")
            ;
            if (key != newKey) {
                this[propertyName] = newKey;
                changed = true;
            }
        }
        return changed;
    },

    //> @method processElement.updateLastElementInValues()
    // Updates a set of +link{group:taskInputExpression} values containing $last references.
    // Any implicit reference to the last task is updated to reference a last task of a
    // specified <code>taskType</code>.
    // <p>
    // For example, a value of "$last.sequenceNo" would be replaced with
    // "$last[fetch].sequenceNo" if the taskType is "fetch". Existing "$last[...]" references
    // are left as-is.
    // <p>
    // This method is a helper to implement task-specific +link{updateLastElementBindingReferences()}.
    //
    // @param values (Object) the object to be updated
    // @param taskType (String) the taskType to be used in new reference
    // @return (Boolean) true if any references were update; false otherwise
    // @visibility workflow
    //<
    updateLastElementInValues : function (values, taskType) {
        var changed = false;
        if (values) {
            for (var key in values) {
                var value = values[key],
                    newValue = value.replace("$last.", "$last[" + taskType + "].")
                ;
                if (value != newValue) {
                    values[key] = newValue;
                    changed = true;
                }
            }
        }
        return changed;
    },

    //> @method processElement.updateLastElementInCriteria()
    // Updates +link{AdvancedCriteria} +link{Criterion} +link{group:taskInputExpression} values
    // containing $last references. Any implicit reference to the last task is updated to
    // reference a last task of a specified <code>taskType</code>.
    // <p>
    // For example, a value of "$last.sequenceNo" would be replaced with
    // "$last[fetch].sequenceNo" if the taskType is "fetch". Existing "$last[...]" references
    // are left as-is.
    // <p>
    // This method is a helper to implement task-specific +link{updateLastElementBindingReferences()}.
    //
    // @param criteria (AdvancedCriteria) the criteria to be updated in place
    // @param taskType (String) the taskType to be used in new reference
    // @return (Boolean) true if any references were update; false otherwise
    // @visibility workflow
    //<
    updateLastElementInCriteria : function (criteria, taskType) {
        if (!criteria) return false;
        var changes = [{
            pattern: new RegExp("^\\$last\\."),
            replacement: "$last[" + taskType + "]."
        }];
        var changed = this._replaceCriteriaFieldName(criteria, changes);
        return this._replaceCriteriaValues(criteria, changes) || changed;
    },

    _replaceCriteriaFieldName : function (criteria, changes) {
        var operator = criteria.operator,
            changed = false
        ;
        if (operator == "and" || operator == "or") {
            var innerCriteria = criteria.criteria;
            for (var i = 0; i < innerCriteria.length; i++) {
                if (this._replaceCriteriaValues(innerCriteria[i], changes)) {
                    changed = true;
                }
            }
        } else {
            for (var i = 0; i < changes.length; i++) {
                var change = changes[i];
                if (criteria.value != null) {
                    var newValue = criteria.fieldName.replace(change.pattern, change.replacement);
                    if (criteria.fieldName != newValue) {
                        criteria.fieldName = newValue;
                        changed = true;
                    }
                }
            }
        }
        return changed;
    },

    //> @method processElement.getDynamicValue()
    // Resolves a dynamic value as +link{group:taskInputExpression,taskInputExpressions} or
    // returns the value as-is.
    //
    // @param value (String) the value to resolve
    // @param process (Process) the current process
    // @return (String) the resolved value
    // @visibility workflow
    //<
    getDynamicValue : function (value, process) {
        if (value) {
            // Resolve any dynamicCriteria or taskInputExpressions
            var values = this._resolveObjectDynamicExpressions({ value: value }, null, null, process);
            value = values.value;
        }
        return value;
    },

    _getSummaryFunction : function (formulaObject, ruleScope, component) {
        // lazily create and cache the summary function (the vars will not change post
        // create)
        if (this._summaryFunction == null) {
            // Use SummaryBuilder.generateFunction() to convert the summary object to a function.
            this._summaryFunction = isc.SummaryBuilder.generateRuleScopeFunction(
                formulaObject,
                ruleScope,
                component
            );
        }
        return this._summaryFunction;
    },

    _$lastTaskRuleContextPath: isc._underscore + "lastTask",

    //> @method processElement.getTextFormulaValue()
    // Resolves a +link{UserSummary} value against the current
    // +link{canvas.ruleScope,rule context}.
    //
    // @param textFormula (UserSummary) the UserSummary value to resolve
    // @param process (Process) the current process
    // @return (String) the resolved value
    // @visibility workflow
    //<
    getTextFormulaValue : function (textFormula, process) {
        var value;
        if (textFormula) {
            var formula = textFormula,
                text = formula.text
            ;
            // Summary function for ruleScope expects the ruleScope
            // variables to be inlined instead of using a var mapping.
            // If a mapping is provided, update the formula by expanding
            // the mapping vars.
            if (formula.summaryVars) {
                var vars = formula.summaryVars;
                for (var mappingKey in vars) {
                    var replace = "#{" + vars[mappingKey] + "}";
                    text = isc.FormulaBuilder.handleKeyExp(text, mappingKey, "escaped", replace);
                    text = isc.FormulaBuilder.handleKeyExp(text, mappingKey, "braced", replace);
                }
                formula = { text: text };
            }

            var ruleScopeComponent,
                removeRuleContext = false
            ;
            if (process.ruleScope) {
                ruleScopeComponent = window[process.ruleScope];
                if (!ruleScopeComponent || ruleScopeComponent.destroyed) {
                    this.logWarn("Attempt to resolve ruleScope references in textFormula " +
                                 "but ruleScope not found: " + process.ruleScope);
                } else {
                    var lastTaskOutput = process.getLastTaskOutput(),
                        lastTaskOutputSchema = process.getLastTaskOutputSchema(null, true)
                    ;
                    if (lastTaskOutput != null && lastTaskOutputSchema) {
                        // Push last task output into ruleContext so it can be picked up by
                        // the formula creation below. Do not notify any observers of ruleContext
                        // changes because this is not meant to be seen by anyone else
                        ruleScopeComponent.provideRuleContext(this._$lastTaskRuleContextPath,
                            lastTaskOutput, lastTaskOutputSchema, null, true);
                        removeRuleContext = true;
                    }

                    var summaryFunction = this._getSummaryFunction(formula, process.ruleScope);
                    if (summaryFunction) {
                        var ruleContext = ruleScopeComponent.getRuleContext();
                        value = summaryFunction(ruleContext);
                    }
                    if (removeRuleContext) {
                        ruleScopeComponent.provideRuleContext(this._$lastTaskRuleContextPath,
                            null, null, null, true);
                    }
                }
            }
        }
        return value;
    }
});

isc.ProcessElement.registerStringMethods({
    //> @method processElement.completeElement()
    // StringMethod called when a processElement completes. Typically used to clear transient
    // state applied to the task while running like +link{processElement.forceSingle} so it
    // can be executed again later. See also +link{reset}.
    // @param process (Process) the containing process
    // @visibility workflow
    //<
    completeElement: "process",

    //> @method processElement.reset()
    // StringMethod called during +link{process.reset} giving the task a chance to reset
    // any internal state so it can be executed later. See also +link{completeElement}.
    // @visibility workflow
    //<
    reset: ""
});

// --------------------------------------------------------------------------------------------
//> @class ProcessSequence
// An Array of +link{ProcessElement}s involved in a +link{Process}.  A
// <code>ProcessSequence</code> is used to reduce the number of explicit
// +link{ProcessElement.ID}s that need to be assigned, by creating an implicit next element -
// the next in the sequence.
// <P>
// A sequence cannot be executed outside of a Process and has no state.
// @inheritsFrom ProcessElement
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("ProcessSequence", "ProcessElement");

isc.ProcessSequence.addProperties({
    //> @attr processSequence.elements (Array of ProcessElement : null : IR)
    // The +link{ProcessElement}s in this sequence.
    // @visibility workflow
    //<
});

// --------------------------------------------------------------------------------------------

//> @class Task
// A Task is an abstract superclass for +link{Process} and for all Task types that can be
// involved in a Process, such as a +link{ServiceTask}.
//
// @inheritsFrom ProcessElement
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<

isc.defineClass("Task", "ProcessElement");

isc.Task.addProperties({
    //> @attr task.inputField (String : null : IR)
    // Field in the +link{Process.state,process state} which is provided as input data to this
    // task.
    // See +link{group:taskIO}.
    // @group taskIO
    // @visibility workflow
    //<

    //> @attr task.inputFieldList (Array of String : null : IR)
    // List of multiple fields from the +link{Process.state,process state} which are provided
    // as input data to this task. See +link{group:taskIO}.
    // <P>
    // If +link{inputField} is also specified, it will be implicitly added to the
    // <code>inputFieldList</code> if it is not already present.
    // @group taskIO
    // @visibility workflow
    //<

    //> @attr task.outputField (String : null : IR)
    // Field in the +link{Process.state,process state} where this task writes outputs. See
    // +link{group:taskIO}.
    // @group taskIO
    // @visibility workflow
    //<

    //> @attr task.outputFieldList (Array of String : null : IR)
    // List of multiple fields in the +link{Process.state,process state} where this task will
    // write outputs. See +link{group:taskIO}.
    // <P>
    // If +link{outputField} is also specified, it will be implicitly added to the
    // <code>outputFieldList</code> if it is not already present.
    // @group taskIO
    // @visibility workflow
    //<

    //> @attr task.outputExpression (String : null : IR)
    // Special expression to write task output directly into a +link{DataBoundComponent}. See
    // +link{group:taskIO}.
    // @group taskIO
    // @visibility workflow
    //<

    //> @groupDef taskIO
    // Each task has "inputs", which are copied from the +link{process.state,Process state}
    // when the task is started, and "outputs", which are atomically applied to the Process
    // state when a task is completed.
    // <P>
    // Tasks can use +link{task.inputField} to specify the field from the Process state that
    // should be used as inputs, and +link{task.outputField} to specify the field in the
    // Process state that the task should write output.
    // <P>
    // More complex tasks can take multiple fields from the process state via
    // +link{task.inputFieldList} and write to multiple fields of the process state via
    // +link{task.outputFieldList}. In this case, the task is said to have an "input Record"
    // and/or "output Record", which is a copy of the process state Record
    // with only the fields listed in the <code>inputFieldList</code> copied.
    // <P>
    // When both <code>inputField</code> and <code>inputFieldList</code> are specified, the
    // inputField is considered the "primary" input field and will be used automatically by
    // various Task subclasses.
    // <P>
    // An additional option for output is provided in +link{task.outputExpression} to write
    // task output directly into another +link{DataBoundComponent} instead of or in addition
    // to the process state. See details below.
    // <P>
    // <h4>inputField and inputFieldList examples</h4>
    // <code>inputData</code> represents the result of the <code>inputField</code> processing
    // and <code>inputRecord</code> represents the result of the <code>inputFieldList</code>.
    // <P>
    // If the +link{process.state} represented in JSON is:
    // <pre>
    // {
    //    orderId:5,
    //    orderItems: [
    //       {name:"Pencils", quantity:3, itemId:2344}
    //    ],
    //    orderUser: { name:"Henry Winkle", address:"...", ... }
    // }
    // </pre>
    // Consider these input definitions and resulting <code>inputData</code> and <code>inputRecord</code>:
    // <ul>
    // <li>inputField: orderId
    //   <ul>
    //     <li>inputData: 5
    //     <li>inputRecord: { orderId: 5 }
    //   </ul>
    // <li>inputField: orderUser.name, inputFieldList: [ "orderUser" ]
    //   <ul>
    //     <li>inputData: "Henry Winkle"
    //     <li>inputRecord: { name: "Henry Winkle", orderUser: { name: "Henry Winkle", address: ... }
    //   </ul>
    // <li>inputField: orderUser
    //   <ul>
    //     <li>inputData: { name: "Henry Winkle", address: ... }
    //     <li>inputRecord: { orderUser: { name: "Henry Winkle", address: ... } }
    //   </ul>
    // <li>inputFieldList: [ "orderUser" ]
    //   <ul>
    //     <li>inputData: null
    //     <li>inputRecord: { orderUser: { name: "Henry Winkle", address: ... } }
    //   </ul>
    // </ul>
    // Notice that <code>inputField</code> is implicitly added to the <code>inputRecord</code>
    // as a field with the same name.
    // <P>
    // <h4>Field paths</h4>
    // Specifying an input or output field as a dataPath allows a hierarchical process state
    // to be flattened into the task input or expanded from the task output.
    // <P>
    // <h4>Output expressions</h4>
    // An +link{task.outputExpression} can be specified to write task output directly into
    // another +link{DataBoundComponent} instead of or in addition to the process state.
    // <P>
    // An output expression is a String prefixed with "$" followed by the DataBoundComponent
    // ID and optionally followed by a dot-separated field name. When no optional field name
    // is specified the task output is written to the target component using setValues() or
    // setData(). With the optional field name, the task output is written to the target
    // with setFieldValue() or setEditValue(). For a ListGrid the row is either the current
    // edit row or the one selected row.
    // <P>
    // As an example, consider a DynamicForm with ID of "orderHeader". By specifying an
    // <code>outputExpression</code> as "$orderHeader" for a fetch ServiceTask the response
    // record will be assigned directly to the DynamicForm.
    //
    // @title Task Input / Output
    // @visibility workflow
    //<


    //> @groupDef taskInputExpression
    // In some tasks, the input to the task needs to be passed to a service being called by the
    // task, to a user-visible form, or other consumers of the input data.
    // A TaskInputExpression can be used to do this declaratively.
    // <P>
    // A TaskInputExpression is a String prefixed with "$input", "$inputRecord", "$ruleScope",
    // "$state" or "$last" followed by an optional dot-separated hierarchical path,
    // which can specify either an atomic data value (String, Number) or Record from the
    // input data. For example, if the +link{process.state} represented in JSON were:
    // <pre>
    // {
    //    orderId:5,
    //    orderItems: [
    //       {name:"Pencils", quantity:3, itemId:2344}
    //    ],
    //    orderUser: { name:"Henry Winkle", address:"...", ... }
    // }
    // </pre>
    // .. and a task specified an <code>inputField</code> of "orderId" and an inputFieldList of
    // "orderItems","orderUser", then:
    // <ul>
    // <li>$input is the value 5
    // <li>$inputRecord.orderUser.name is "Henry Winkle"
    // <li>$inputRecord.orderItems[0] is the first orderItems Record ({name:"Pencils", ... })
    // </ul>
    // <p>
    // "$ruleScope" can be used to pull values from a +link{Canvas.ruleScope,ruleScope}
    // +link{Canvas.getRuleContext,ruleContext} when configured in +link{Process.ruleScope}.
    // <ul>
    // <li>$ruleScope.property references the ruleContext "property" field
    // </ul>
    // <p>
    // The other two sources of input are "$state" and "$last". The former references the
    // contents of the +link{Process.state} and the latter the
    // +link{Process.state,transient state}.
    // <ul>
    // <li>$state is the full contents of the process state
    // <li>$state.orderId is the "orderId" field of the process state (5 from the example above)
    // <li>$last is the full output of the previous task executed in the process
    // <li>$last.property is the "property" field of the previous task executed in the process
    // <li>$last[service].property or $last[ServiceTask].property references the last "ServiceTask"
    //     output in the "property" field
    // <li>$ruleScope.property references the ruleScope "property" field
    // </ul>
    // <p>
    // <h4>Transient state outputs</h4>
    // Most tasks pass the output from the <i>previous</i> task as their output (i.e. passed
    // through) making it easy to refer to earlier output without referencing the task type.
    // Tasks that work with records or interact with the user, however, typically provide
    // task-specific output as detailed below:
    // <ul>
    // <li><b>ServiceTask</b>: the contents of dsResponse.data.
    // <li><b>ScriptTask</b>: the result of +link{scriptTask.execute,execute()} or,
    //     for an asynchronous task, the value passed to
    //     +link{scriptTask.setOutputRecord,setOutputRecord()} or
    //     +link{scriptTask.setOutputData,setOutputData()}.
    // <li><b>StateTask</b>: the value assigned to the outputField.
    // <li><b>UserTask</b>: the values of the targetForm or targetVM when the task completes.
    // <li><b>AskForValueTask</b>: an object with "value" and "canceled" properties.
    // <li><b>FetchRelatedDataTask</b>: the first fetched record.
    // <li><b>GridFetchDataTask</b>: the contents of dsResponse.data.
    // <li><b>GridTransferSelectedTask</b>: the first transfered record.
    // <li><b>GridSelectRecordsTask</b>: on a select, the set of newly selected records,
    //     even if other records are also selected. On a deselect, the entire set of
    //     de-selected records.
    // <li><b>FetchRelatedDataTask</b>: the first fetched related record.
    // <li><b>FormSaveDataTask</b>: an object with "valuesValid" and "errors" properties.
    // <li><b>FormValidateValuesTask</b>: an object with "valuesValid" and "errors" properties.
    // <li><b>GetPropertiesTask</b>: an object with selected properties and values retrieved.
    // </ul>
    // @title Task Input Expressions
    // @visibility workflow
    //<

    editorType: "TaskEditor",

    _resolveInputField : function (value, process) {
        if (value == null) return null;
        var resolved;
        if (value.startsWith("$")) resolved = this._resolveDynamicExpression(value, null, null, process);
        else resolved = process.getStateVariable(value);
        return resolved;
    },

    _writeOutputExpression : function (data) {
        var expression = this.outputExpression;
        if (!expression) return;

        if (expression.startsWith("$")) {
            expression = expression.substring(1);
            var id = expression;
            var field;
            var fdi = id.indexOf(".");
            if (fdi > 0) {
                id = id.substring(0, fdi);
                field = id.substring(fdi+1);
            }
            var canvas = isc.Canvas.getById(id);
            if (canvas) {
                if (field) {
                    if (!isc.isAn.Array(data)) {
                        if (isc.isA.DynamicForm(canvas)) {
                            canvas.setFieldValue(field, data);
                        } else if (isc.isA.ListGrid(canvas) && canvas.canEdit) {
                            var editRow = canvas.getEditRow();
                            if (editRow != null) {
                                canvas.setEditValue(editRow, field, data);
                            } else {
                                var selection = canvas.getSelectedRecords();
                                if (selection != null && selection.length == 1) {
                                    var selectedRow = canvas.getRecordIndex(selection[0]);
                                    canvas.setEditValue(selectedRow, field, data);
                                }
                            }
                        } else {
                            this.logWarn("outputExpression target is not a supported DBC or is not editable - ignored: " + expression);
                        }
                    } else {
                        this.logWarn("Task output is not supported by outputExpression target - ignored: " + expression);
                    }
                } else {
                    if (canvas.setValues) {
                        var value = (isc.isAn.Array(data) ? data[0] : data);
                        if (isc.isAn.Object(value)) {
                            canvas.setValues(value);
                        } else {
                            this.logWarn("task output is not an object and cannot be written with outputExpression - ignored: " + expression);
                        }
                    } else if (canvas.setData) {
                        if (isc.isAn.Array(data) || isc.isAn.Object(data)) {
                            if (!isc.isAn.Array(data)) data = [data];
                            canvas.setData(data);
                        } else {
                            this.logWarn("task output is not an object and cannot be written with outputExpression - ignored: " + expression);
                        }
                    } else {
                        this.logWarn("outputExpression target is not a supported DBC - ignored: " + expression);
                    }
                }
            } else {
                this.logWarn("outputExpression DBC not found - ignored: " + expression);
            }
        } else {
            this.logWarn("Invalid outputExpression - ignored: " + expression);
        }
    },

    updateLastElementBindingReferences : function (taskType) {
        var changed = this.Super("updateLastElementBindingReferences", arguments);
        changed = this.updateLastElementInValueProperty("inputField", taskType) || changed;
        return changed;
    },

    updateGlobalIDReferences : function (oldId, newId) {
        var changed = this.Super("updateGlobalIDReferences", arguments);
        changed = this.updateGlobalIDInValueProperty("inputField", oldId, newId) || changed;
        return changed;
    }

});

//---------------------------------------------------------------------------------------

//> @method Callbacks.ProcessCallback
// A +link{type:Callback} to evaluate when a Process has been loaded via
// +link{Process.loadProcess()}.
//
// @param process (Process) the loaded Process
// @see class:Process
// @see class:RPCResponse
// @visibility workflow
//<

// --------------------------------------------------------------------------------------------
//> @class Process
// A instance of Process represents a stateful process executing a series of Tasks,
// which may be:
// <ul>
// <li> user interactions
// <li> calls to DataSources (hence: any database or web service)
// <li> arbitrary code
// <li> other Processes
// </ul>
// A Process is <i>stateful</i> in the sense that it maintains +link{process.state,state}
// across the different tasks that are executed.  This allows you to maintain context as you
// walk a user through a multi-step business process in your application, which may involve
// multiple operations on multiple entities.  Each Task that executes can use the Process state
// as inputs, and can output a result which is stored in the Process state - see
// +link{group:taskIO}.
// <P>
// A Process can have multiple branches, choosing the next Task to execute based on
// +link{Criteria} - see +link{DecisionTask} and +link{MultiDecisionTask}.
// <P>
// Because a Process may return to a previous Task in various situations, the data model of a
// Process is strictly speaking a <i>graph</i> (a set of nodes connected by arbitary
// interlinks). However, most processes have sequences of several tasks in a row, and the
// definition format allows these to be represented as simple Arrays called "sequences",
// specified via +link{process.sequences}.  This reduces the need to manually specify IDs and
// interlinks for Tasks that simply proceed to the next task in a sequence.
// <P>
// Processes follow all the standard rules for encoding as +link{group:componentXML}, however,
// note that the &lt;Process&gt; tag allows any kind of +link{ProcessElement} (tasks, decisions
// and sequences) to appear as a direct subelement of the &lt;Process&gt; tag without the need
// for an intervening &lt;elements&gt; or &lt;sequences&gt; tag.  The example below
// demonstrates this shorthand format.
// <pre>
// &lt;Process ID="<i>processId</i>"&gt;
//     &lt;ServiceTask ID="<i>serviceTaskId</i>" nextElement="<i>sequenceId</i>" ..&gt;
//         &lt;inputFieldList&gt;
//             &lt;value&gt;order.countryName&lt;/value&gt;
//         &lt;/inputFieldList&gt;
//         &lt;outputFieldList&gt;
//             &lt;value&gt;order.countryName&lt;/value&gt;
//             &lt;value&gt;order.continent&lt;/value&gt;
//         &lt;outputFieldList&gt;
//     &lt;/ServiceTask&gt;
//     &lt;sequence ID="<i>sequenceId</i>" &gt;
//         &lt;StateTask ../&gt;
//         &lt;StateTask ../&gt;
//         &lt;StateTask ../&gt;
//         &lt;StateTask nextElement="<i>userTaskId</i>" ../&gt;
//     &lt;/sequence&gt;
//     &lt;UserTask ID="<i>userTaskId</id>" ../&gt;
//     ...
// &lt;/Process&gt;
// </pre>
// <b>NOTE:</b> you must load the standard DataBinding module before you can use <code>Process</code>.
//
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("Process");

isc.Process.addClassProperties({
    _cache: {},

    //> @classAttr process.decisionPlaceholderSelection (String : "[placeholder]" : IR)
    // Value of <code>failureElement</code> in various +link{processElement,tasks} to
    // indicate a placeholder is being used. Also applies to +link{decisionBranch.targetTask}
    // and +link{multiDecisionTask.defaultElement}.
    //
    // @visibility workflow
    //<
    decisionPlaceholderSelection: "[placeholder]",

    //> @classMethod Process.loadProcess()
    // Loads an XML process definition stored in XML from the server.
    // <p>
    // This method requires server-side support included in SmartClient Pro Edition or better.
    // If you are using SmartClient LGPL, Processes must be defined programmatically
    // <smartclient>in JavaScript.</smartclient>
    // <smartgwt>in Java.</smartgwt>
    // <p>
    // Process files are stored as .proc.xml files in +link{group:componentXML,Component XML}
    // format, in the directory indicated by the <code>project.processes</code> setting in
    // +link{group:server_properties,server.properties}
    // (<code><i>webroot</i>/processes</code> by default).  To load a process
    // saved in a file <i>processId</i>.proc.xml, pass just <i>processId</i> to this method.
    // <P>
    //
    // @param processId (Identifier | Array of Identifier) process ID or IDs to load
    // @param callback (ProcessCallback) called when the process is loaded with argument
    //                            "process", the first process.  Other processes can be looked
    //                            up via +link{getProcess()}.
    //
    // @visibility workflow
    //<
    loadProcess : function (processId, callback) {
        var requestCallback = function (response, data, request) {


            var processes = data && isc.Class.evaluate(data),
                process;

            if (processes == null || processes.length == 0) {
                isc.logWarn("Unable to load process(es): " + processId + ". " +
                    "These should be defined in  *.proc.xml files at the search path " +
                    " specified by \"project.processes\".")
            } else {

                // Even if a single file was requested, the response data.screens attr will
                // be an array
                if (!isc.isAn.Array(processId)) processId = [processId];

                for (var i = 0; i < processes.length; i++) {
                    var p = isc.Class.evaluate(processes[i].source);
                    if (p.ID != processId[i]) p.setID(processId[i]);
                    if (i == 0) process = p; // Set up the process argument for the callback
                }

            }
            // If no callback was passed in, this isn't necessarily an error - the dev may later
            // call Tour.getTour()
            if (callback) this.fireCallback(callback, "process", [process])
        };
        // The servlet expects a comma separated array of processNames [or screenNames]
        var processIdString = isc.isAn.Array(processId) ? processId.join() : processId;
        return isc.RPCManager._loadComponentXML({screenName:processIdString,
                                                type:"processes", structuredResponse:true},
             requestCallback);

    },

    //> @classMethod Process.getProcess()
    // Get a Process instance by it's ID.  See +link{loadProcess()}.
    // @param processId (Identifier) process ID to retrieve
    // @return (Process) the process, or null if not loaded
    // @visibility workflow
    //<
    getProcess : function (processId) {
        return isc.Process._cache[processId];
    }
});

isc.Process.addProperties({
    init : function () {
        var res = this.Super("init", arguments);

        // Cache instance by ID for later retrieval by Process.getProcess
        this.cacheInstance();

        // If user specifies a single sequence as configuration, move it to sequences.
        // This allows for a cleaner XML representation.
        var sequence = this.sequence || this.Sequence;
        if (sequence) {
            if (!this.sequences) this.sequences = [];
            this.sequences.addAt({ _constructor: "ProcessSequence", elements: sequence }, 0);
            delete this.sequence;
            delete this.Sequence;
        }

        if (this.autoStart) this.start();
        return res;
    },

    destroy : function () {
        // Destroy elements
        if (this.elements) this.elements.callMethod("destroy");
        if (this.sequences) this.sequences.callMethod("destroy");

        // Remove cached instance
        if (this.ID) delete isc.Process._cache[this.ID];

        this.Super("destroy", arguments);
    },

    setID : function (id) {
        this.ID = id;
        this.cacheInstance();
    },

    // Cache instance by ID for later retrieval by Process.getProcess
    cacheInstance : function () {
        if (this.ID) {
            var cache = isc.Process._cache;
            var existingProcess = cache[this.ID];
            if (existingProcess != null && existingProcess.destroy) {
                existingProcess.destroy();
            }
            cache[this.ID] = this;
        }
    },

    // Called exactly once from start()
    instantiateElements : function () {
        if (this.elements) this.elements = this._instantiateElements(this.elements);
        if (this.sequences) this.sequences = this._instantiateElements(this.sequences);
    },

    _instantiateElements : function (elements) {
        var newElements = [];
        for (var i = 0; i < elements.length; i++) {
            var element = elements[i];
            newElements[i] = element;
            if (isc.isAn.Object(element)) {
                if (element._constructor && !isc.isAn.Instance(element)) {
                    newElements[i] = isc.ClassFactory.newInstance(element);
                }
                if (element.elements) {
                    newElements[i].elements = this._instantiateElements(element.elements);
                }
            }
        }
        return newElements;
    },

    //> @attr process.sequences (Array of ProcessSequence : null : IR)
    // Sequences of ProcessElements.  By defining a sequences of elements you can make the
    // +link{processElement.nextElement} implicit.
    // <P>
    // <smartclient>You do not have to explicitly create a +link{ProcessSequence},
    // you can instead use the shorthand:
    // <pre>
    // isc.Process.create({
    //     startElement:"firstSequence",
    //     sequences: [
    //         { ID:"something", elements: [ ... ] },
    //         { ID:"somethingElse", elements: [ ... ] },
    //         ...
    //     ]
    //     ...
    // });
    // </pre>
    // .. this is equivalent to ..
    // <pre>
    // isc.Process.create({
    //     startElement:"firstSequence",
    //     sequences: [
    //         isc.ProcessSequence.create({
    //              ID:"something",
    //              elements: [ ... ]
    //         }),
    //         isc.ProcessSequence.create({
    //              ID:"somethingElement",
    //              elements: [ ... ]
    //         }),
    //         ...
    //     ]
    //     ...
    // });
    // </pre>
    // </smartclient>
    // <smartgwt>
    // Example of using sequences:
    // <pre>
    // Process process = new Process();
    // process.setStartElement("firstSequence");
    // ProcessSequence innerSequence = new ProcessSequence(incTask, add2Task, incTask);
    // process.setSequences(
    //     new ProcessSequence("firstSequence", serviceTask, multiDecisionTask),
    //     new ProcessSequence("errorFlow", failureTask, userNotifyTask)
    // );
    // // standalone process elements not part of sequences
    // process.setElements(new ServiceTask(){...});
    // Record state = new Record();
    // state.setAttribute("someField", "someValue");
    // process.setState(state);
    // process.start();
    // </pre>
    // </smartgwt>
    // @visibility workflow
    //<

    //> @attr process.elements (Array of ProcessElement : null : IR)
    // Elements involved in this Process.  You can also group elements into +link{sequences}
    // to reduce the need to explicitly define IDs for elements and interlink them.
    // @visibility workflow
    //<

    //> @attr process.startElement (String : null : IR)
    // The ID of either a +link{sequences,sequence} or an +link{elements,element} which should
    // be the starting point of the process.  If not specified, the first sequence is chosen,
    // or if there are no sequences, the first element.
    // - log a warning and do nothing if there are neither sequences or elements
    // <smartclient>
    // - an example of how a Process would be defined
    // <pre>
    // isc.Process.create({
    //     startElement:"firstSequence",
    //     sequences: [
    //         {
    //            id:"firstSequence",
    //            elements : [
    //                isc.ServiceTask.create({ .. }),
    //                isc.MultiDecisionTask.create({ .. })
    //            ]
    //         },
    //         {
    //            id:"errorFlow",
    //            elements : [ ... ]
    //
    //         }
    //     ],
    //     elements: [
    //        // standalone process elements not part of sequences
    //        isc.ServiceTask.create({ .. })
    //     ],
    //     state : {
    //         someField:"someValue"
    //     }
    // })
    // </pre>
    // </smartclient>
    // @visibility workflow
    //<

    //> @attr process.wizard (Boolean : false : IR)
    // If wizard is set then current workflow will be handled as wizard. Every userTask will
    // hide associated form after user finished step.
    // @visibility workflow
    //<

    //> @attr process.mockMode (Boolean : null : IRW)
    // Enable mock mode on the workflow? By default, this setting does nothing but is available
    // for individual tasks to trigger special action. For example, a task that would normally
    // fail outside of its target environment can take an alternative action during testing.
    // <p>
    // mockMode can also be enabled or disabled for an individual task with
    // +link{processElement.mockMode}.
    //
    // @visibility workflow
    //<

    //> @attr process.containerId (GlobalId : null : IRW)
    // Identifier of canvas where UI elements created by using
    // +link{UserTask.inlineView,inline view} property should be added
    // using addMember.
    // @visibility workflow
    //<

    //> @attr process.views (Array of Canvas: null: IRW)
    // An inline definitions of the forms that could be used to encode form directly in process
    // xml.
    //<

    //> @method process.getElement()
    // Retrieve a +link{ProcessElement} by it's ID
    // @param ID (String) id of the process element
    // @return (ProcessElement) the indicated process element, or null if no such element
    // exists
    // @visibility workflow
    //<
    getElement : function (ID) {
        return this._searchElement(this, ID);
    },

    _searchElement : function (sequence, ID) {
        if (sequence.sequences) {
            for (var i = 0; i < sequence.sequences.length; i++) {
                var s = sequence.sequences[i];
                if (s.ID == ID) {
                    return s;
                } else if (s.sequences || s.elements) {
                    var res = this._searchElement(s, ID);
                    if (res) return res;
                }
            }
        }
        if (sequence.elements) {
            for (var i = 0; i < sequence.elements.length; i++) {
                var e = sequence.elements[i];
                if (e.ID == ID) {
                    return e;
                } else if (e.sequences || e.elements) {
                    var res = this._searchElement(e, ID);
                    if (res) return res;
                }
            }
        }
    },

    getAllElements : function (sequence, arr) {
        if (!this.initializedElements) {
            this.instantiateElements();
            this.initializedElements = true;
        }

        if (!sequence) sequence = this;
        if (!arr) arr = [];

        if (sequence.sequences) {
            for (var i = 0; i < sequence.sequences.length; i++) {
                var s = sequence.sequences[i];
                arr.add(s);
                if (s.sequences || s.elements) {
                    this.getAllElements(s, arr);
                }
            }
        }
        if (sequence.elements) {
            for (var i = 0; i < sequence.elements.length; i++) {
                var e = sequence.elements[i];
                arr.add(e);
                if (e.sequences || e.elements) {
                    this.getAllElements(e, arr);
                }
            }
        }

        return arr;
    },


    removeElement : function (element) {
        this._removeElement(this, element);
    },

    _removeElement : function (sequence, element) {
        if (sequence.sequences) {
            for (var i = 0; i < sequence.sequences.length; i++) {
                var s = sequence.sequences[i];
                if (s == element) {
                    sequence.sequences.removeAt(i);
                    return true;
                }
                if (s.sequences || s.elements) {
                    if (this._removeElement(s, element)) return true;
                }
            }
        }
        if (sequence.elements) {
            for (var i = 0; i < sequence.elements.length; i++) {
                var e = sequence.elements[i];
                if (e == element) {
                    sequence.elements.removeAt(i);
                    return true;
                }
                if (e.sequences || e.elements) {
                    if (this._removeElement(e, element)) return true;
                }
            }
        }
    },


    addElement : function (element, afterElement, beforeElement) {
        if (afterElement || beforeElement) {
            this._addElement(this, element, afterElement, beforeElement);
        } else {
            if (!this.elements) this.elements = [];
            this.elements.add(element);
        }
    },

    _addElement : function (sequence, element, afterElement, beforeElement) {
        if (sequence.sequences) {
            for (var i = 0; i < sequence.sequences.length; i++) {
                var s = sequence.sequences[i];
                if (afterElement && s == afterElement) {
                    var position = i+1;
                    sequence.sequences.add(element, position);
                    return true;
                } else if (beforeElement && s == beforeElement) {
                    var position = i;
                    sequence.sequences.add(element, position);
                    return true;
                }
                if (s.sequences || s.elements) {
                    if (this._addElement(s, element, afterElement, beforeElement)) return true;
                }
            }
        }
        if (sequence.elements) {
            for (var i = 0; i < sequence.elements.length; i++) {
                var e = sequence.elements[i];
                if (afterElement && e == afterElement) {
                    var position = i+1;
                    sequence.elements.add(element, position);
                    return true;
                } else if (beforeElement && e == beforeElement) {
                    var position = i;
                    sequence.elements.add(element, position);
                    return true;
                }
                if (e.sequences || e.elements) {
                    if (this._addElement(e, element, afterElement, beforeElement)) return true;
                }
            }
        }
    },

    //> @attr process.state (Record : null : IRW)
    // Current state of a process.  As with Records in general, any field of a Record may
    // contain a nested Record or Array of Records, so the process state is essentially a
    // hierarchical data structure.
    // <p>
    // <h4>Transient state</h4>
    // In addition to the explicit process state there is a "transient state." The transient
    // state represents the complete output of each of the last tasks of each type within the current
    // process execution. This allows easy reference to the previous task output with
    // +link{group:taskInputExpression,taskInputExpressions}.
    //
    // @setter setState
    // @visibility workflow
    //<


    //> @method process.setState()
    // Set process state for current process
    // @param state (Record) the new process state
    // @visibility workflow
    //<
    setState : function (state) {
        this.state = state;
    },

    // ruleScope
    //---------------------------------------------------------------------------------------

    //> @attr process.ruleScope (String : null : IR)
    // +link{canvas.ID} of the component that manages "rule context" for which
    // this process participates. The rule context can be used in
    // +link{group:taskInputExpression,taskInputExpression}.
    //
    // @see canvas.ruleScope
    // @visibility workflow
    //<

    //---------------------------------------------------------------------------------------

    //> @attr process.autoStart (Boolean : false : IR)
    // Cause the process to automatically call +link{start()} as soon as it is created.
    // @visibility workflow
    //<
    autoStart: false,

    //> @method process.start()
    // Starts this task by executing the +link{startElement}. Also used by asynchronous
    // tasks to restart the workflow.
    // @visibility workflow
    //<
    start : function () {
        if (!this.initializedElements) {
            // Instantiate elements/sequences if defined as propertiesOnly objects
            this.instantiateElements();

            // Start execution (nextElement) at startElement
            this._nextElement = this.startElement;

            this.initializedElements = true;
        }


        // Process can be async, so we continue it's execution no matter where we've stopped
        if (this.executionStack == null) {
            // start() is called to restart a process after an async task
            // finishes. Only log the process start on the initial call.
            if (this.logIsInfoEnabled("workflow")) {
                this.logInfo("Start process", "workflow");
            } else if (this.logIsDebugEnabled("workflow")) {
                this.logDebug("Start process: " + this.echo(this), "workflow");
            }
        }

        if (this.executionStack == null) {
            this.executionStack = [];
        }

        if (this.state == null) this.state = {};
        while (this._next()) {
            var currentTask = this._getFirstTask();
            // check for empty sequence
            if (currentTask) {
                // mark process as started, so we will be able to handle the situation with
                // no next or cancel element in elements queue
                this._started = true;

                // Some tasks support taking multiple records from the last task output
                // and processing each. These tasks are marked supportsMultipleInputRecords=true.
                // By iterating through the last task output records the task is called once
                // per record and only sees a single record in the last task output - the
                // current one in iteration.
                //
                // An instance can be marked forceSingle=true to prevent the expansion above
                // and therefore sees only the first record if there are multiple.
                //
                // For the initial call to executeElement() a task with either
                // supportsMultipleInputRecords == false or null or forceSingle == true
                // will only see the first record of a multiple last task output. Otherwise,
                // the first call will see all records. To prevent processing the remaining
                // records executeElement() can set forceSingle=false. If not, executeElement()
                // will be called for each additional record which is made available via
                // process.getLastTaskOutput().
                //
                // An example of a task that can accept multiple records but sometimes wants
                // to process all of them at once rather than iterating on the records is
                // GridEditRecordTask. If the task is configured to edit existing records
                // identified by criteria, all records are needed at once and no further
                // calls should be made to the task. forceSingle is set to true to enforce
                // this. The task, when configured to create a new record with initial values
                // checks if the values are bound to $last. If not, forceSingle is also set
                // to true because each call would result another record exactly the same.

                var iterator = this.getLastTaskOutputIterator(currentTask);
                while (iterator.hasNext()) {
                    iterator.next();
                    // every task should implement it's logic
                    if (!currentTask.executeElement(this)) {
                        iterator.done();
                        return;
                    }
                    // If execution forces single mode, stop now
                    if (currentTask.forceSingle) {
                        break;
                    }
                }
                iterator.done();
            }
        }
        this.destroyLastTaskOutputSchema();
        this.handleFinished();

        if (this.logIsInfoEnabled("workflow")) {
            this.logInfo("Process finished", "workflow");
        } else if (this.logIsDebugEnabled("workflow")) {
            this.logDebug("Process finished: " + this.echo(this), "workflow");
        }
    },

    getLastTaskOutputIterator : function (currentTask) {
        var lastOutput = this._getLastTaskOutput(),
            lastValue = (lastOutput ? lastOutput.value : null),
            lastArray = (isc.isAn.Array(lastValue) ? lastValue : [lastValue]),
            index = 0,
            length = lastArray.length
        ;

        if ((!currentTask.supportsMultipleInputRecords || currentTask.forceSingle) &&
            lastArray.length > 0)
        {
            // Force processing against only the first record
            lastArray = [lastArray[0]];
            length = 1;
        }

        return {
            hasNext : function () {
                return (index < length);
            },
            next : function () {
                if (index < length) {
                    // Set task output to the current iterator value
                    if (lastOutput && index > 0) {
                        lastOutput.value = lastArray[index];
                    }
                    // And return it...
                    return lastArray[index++];
                }
                return null;
            },
            done : function () {
                // Restore saved lastOutput to original (multiple) value
                if (lastOutput && index > 0) {
                    lastOutput.value = lastValue;
                }
            }
        }
    },

    //> @method process.reset()
    // Reset process to it's initial state, so process can be started again.
    // @param [state] (Record) new state of the process
    // @visibility workflow
    //<
    reset : function(state) {
        this.state = state;
        this.executionStack = null;
        this._nextElement = this.startElement;
        // Clear transient state
        this.setLastTaskClass(null);
        this._lastOutput = null;
        this._started = false;

        // Allow elements to reset of any transient state
        var allElements = this.getAllElements();
        for (var i = 0; i < allElements.length; i++) {
            var element = allElements[i];
            if (element.reset) {
                element.reset();
            }
        }
    },

    handleFinished : function () {
        if (this.finished) {
            delete this._nextElement;
            this.finished(this.state);
        }
    },


    finished : function () {
    },

    // If user didn't set ID or don't use nextElement property we will take next element
    // or sequence based on their order
    _next : function (skipLogEmptyMessage) {
        var currEl = this.executionStack.last();
        if (currEl == null) {
            // If a new nextElement was assigned this._currentElement will be stashed so
            // it can be completed before starting the next task.
            if (this._currentElement) {
                var el = this._currentElement;
                // Allow element to perform post-processing
                if (el.completeElement) {
                    el.completeElement(this);
                }
                delete this._currentElement;
            }

            // start processing
            if (this._nextElement) {
                var nextEl = this._gotoElement(this, this._nextElement);
                if (nextEl == null) {
                    isc.logWarn("unable to find task '" + this._nextElement + "' - process will be finished");
                }
                return nextEl;
            } else if (this._started) {
                // no startElement after an element, so we should finish the process
                return null;
            } else if (this.sequences && this.sequences.length > 0) {
                this.executionStack.add({el:this, sIndex: 0});
                return this.sequences[0];
            } else if (this.elements && this.elements.length > 0) {
                this.executionStack.add({el:this, eIndex: 0});
                return this.elements[0];
            } else if (!skipLogEmptyMessage) {
                isc.logWarn("There are neither sequences or elements. Nothing to execute.");
            }
        } else {
            var el = null;
            if (currEl.sIndex != null) {
                el = currEl.el.sequences[currEl.sIndex];
            } else if (currEl.eIndex != null) {
                el = currEl.el.elements[currEl.eIndex];
            }
            // Allow element to perform post-processing
            if (el.completeElement) {
                el.completeElement(this);
            }

            // Save ClassName of the last task for use in process.getLastTaskOutput()
            if (el.getClassName) this.setLastTaskClass(el.getClassName());

            if (el.nextElement) {
                this.executionStack = [];
                var nextEl = this._gotoElement(this, el.nextElement);
                if (nextEl == null) {
                    isc.logWarn("unable to find task '" + el.nextElement + "' - process will be finished");
                }
                return nextEl;
            } else {
                return this._findNextElement();
            }
        }
    },

    _gotoElement : function (sequence, ID) {
        var elData = {el: sequence};
        this.executionStack.add(elData);
        if (sequence.sequences) {
            for (var i = 0; i < sequence.sequences.length; i++) {
                var s = sequence.sequences[i];
                elData.sIndex = i;
                if (s.ID == ID) {
                    return s;
                } else if (s.sequences || s.elements) {
                    var res = this._gotoElement(s, ID);
                    if (res) return res;
                }
            }
        }
        delete elData.sIndex;
        if (sequence.elements) {
            for (var i = 0; i < sequence.elements.length; i++) {
                var e = sequence.elements[i];
                elData.eIndex = i;
                if (e.ID == ID) {
                    return e;
                } else if (e.sequences || e.elements) {
                    var res = this._gotoElement(e, ID);
                    if (res) return res;
                }
            }
        }
        this.executionStack.removeAt(this.executionStack.length - 1);
    },

    _findNextElement : function () {
        var elData = this.executionStack.last();
        if (elData.eIndex != null && elData.el != this) {
            if (elData.eIndex == elData.el.elements.length - 1) {
                this.executionStack.removeAt(this.executionStack.length - 1);
                if (elData.el == this) {
                    return;
                } else {
                    return this._findNextElement();
                }
            } else {
                elData.eIndex++;
                return elData.el.elements[elData.eIndex];
            }
        }
    },

    // recursively search for first non-sequence in element
    _getFirstTask : function (inner) {
        var lastElData = this.executionStack.last();

        var el = null;
        if (lastElData.sIndex != null) {
            el = lastElData.el.sequences[lastElData.sIndex];
        } else if (lastElData.eIndex != null) {
            el = lastElData.el.elements[lastElData.eIndex];
        }
        if (el.sequences == null && el.elements == null) {
            if (!inner) this.handleTraceElement(el);
            return el;
        }
        var elData = {el: el};
        this.executionStack.add(elData);
        if (el.sequences) {
            for (var i = 0; i < el.sequences.length; i++) {
                elData.sIndex = i
                var res = this._getFirstTask(el.sequences[i]);
                if (res) {
                    this.handleTraceElement(res);
                    return res;
                }
            }
        }
        if (el.elements) {
            for (var i = 0; i < el.elements.length; i++) {
                elData.eIndex = i
                var res = this._getFirstTask(el.elements[i]);
                if (res) {
                    // If first element of a sequence, trace that element too
                    if (elData.eIndex == 0) this.handleTraceElement(elData.el);
                    this.handleTraceElement(res);
                    return res;
                }
            }
        }
        this.executionStack.removeAt(this.executionStack.length - 1);
    },

    //> @method process.setNextElement()
    // Sets the task ID of the next task to execute after the current task finishes. If the
    // task is not found or <code>null</code> is passed as the nextElement, the current
    // process will be terminated instead.
    //
    // @param [nextElement] (String) ID of the next task execute or null to terminate process
    // @visibility workflow
    //<
    setNextElement : function (nextElement) {
        var lastElData = this.executionStack.last(),
            el = null
        ;
        if (lastElData.sIndex != null) {
            el = lastElData.el.sequences[lastElData.sIndex];
        } else if (lastElData.eIndex != null) {
            el = lastElData.el.elements[lastElData.eIndex];
        }
        // Save ClassName of the last task for use in process.getLastTaskOutput()
        this.setLastTaskClass(el.getClassName());

        this.executionStack = [];
        this._nextElement = nextElement;
        this._currentElement = el;
    },

    //> @method process.setStateVariable()
    // Sets a +link{process.state, process state} variable for later reference with
    // +link{getStateVariable} or more commonly with a +link{group:taskInputExpression}
    // property.
    //
    // @param stateVariablePath (String) path to variable in process state to set.
    //                                   segments are separated by a decimal point (.)
    // @param value (Any) the value to save
    // @visibility workflow
    //<
    setStateVariable : function (stateVariablePath, value) {
        if (!this.state) this.state = {};
        if (stateVariablePath.indexOf(".") < 0 || this.state[stateVariablePath]) {
            this.state[stateVariablePath] = value;
        } else {
            var segments = stateVariablePath.split(".");
            var obj = this.state;
            for (var i = 0; i < segments.length - 1; i++) {
                var nextObj = obj[segments[i]];
                if (nextObj == null) {
                    obj[segments[i]] = {}
                    nextObj = obj[segments[i]];
                }
                obj = nextObj;
            }
            obj[segments[i]] = value;
        }
    },

    //> @method process.getStateVariable()
    // Returns a variable value from the +link{process.state, process state}. Values can
    // be written into process state by +link{setStateVariable}, setting
    // +link{processElement.bindOutput}, or various task output settings (See
    // +link{group:taskIO}.)
    //
    // @param stateVariablePath (String) path to variable in process state to set.
    //                                   segments are separated by a decimal point (.)
    // @return (Any) the value found at the path
    // @visibility workflow
    //<
    getStateVariable : function (stateVariablePath) {
        if (stateVariablePath.indexOf(".") < 0 || this.state[stateVariablePath]) {
            return this.state[stateVariablePath];
        } else {
            var segments = stateVariablePath.split(".");
            var obj = this.state;
            for (var i = 0; i < segments.length - 1; i++) {
                obj = obj[segments[i]];
                if (obj == null) {
                    isc.logWarn("Unable to get state variable: " + stateVariablePath + " no such path")
                    return;
                }
            }
            return obj[segments[i]]
        }
    },

    // Transient state management

    setLastTaskClass : function (className) {
        this._lastTaskClassName = className;
    },

    getLastTaskClass : function () {
        return this._lastTaskClassName;
    },

    //> @method process.setTaskOutput()
    // Sets the task output of <code>task</code> in the +link{state,process state} so it
    // can be used by later tasks with +link{getLastTaskOutput} or more commonly with a
    // +link{group:taskInputExpression} property.
    // <p>
    // If the task sets <code>bindOutput</code> the output value is also written into that
    // +link{process.state, process state} variable.
    //
    // @param task (ProcessElement) the workflow task setting the output (i.e. this)
    // @param value (Any) the output value for task
    // @visibility workflow
    //<
    setTaskOutput : function (task, value, schemaTask) {
        if (!task) return;
        var className = task.getClassName().toLowerCase(),
            ID = task.ID
        ;
        schemaTask = schemaTask || task;
        // Save not just the value but also the task that is recording output value and the
        // task that originated the output value (i.e. schemaTask). The latter is used to
        // lookup the last task output schema during workflow processing if needed.
        var output = {
            task: task,
            value: value,
            schemaTask: schemaTask
        };

        if (!this._lastOutput) this._lastOutput = {};
        this._lastOutput[className] = output;
        if (ID != null) this._lastOutput[ID] = output;

        if (task.bindOutput) {
            var varPath = task.bindOutput;
            if (varPath.startsWith("$")) varPath = varPath.substring(1);
            this.setStateVariable(varPath, value);
        }
    },

    //> @method process.passThruTaskOutput()
    // Takes the +link{getLastTaskOutput,last task output} and sets it as the
    // +link{setTaskOutput,task output} for the <code>task</code>.
    // <p>
    // This method is not just a shortcut to set output of a pass-thru task but it
    // also records the correct schema of the passed-thru output so it can be
    // quickly looked up.
    //
    // @param task (ProcessElement) the workflow task setting the output (i.e. this)
    // @visibility workflow
    //<
    passThruTaskOutput : function (task) {
        var output = this._getLastTaskOutput();
        this.setTaskOutput(task, this.getLastTaskOutput(), output && output.schemaTask);
    },

    //> @method process.getLastTaskOutput()
    // Returns the task output of the last task executed. More commonly a
    // +link{group:taskInputExpression} property is used (see
    // +link{processElement.getDynamicValue}).
    //
    // @param [taskType] (String) the optional task type to lookup in last task output
    // @return (Any) the last task output or null if none is found
    // @visibility workflow
    //<
    getLastTaskOutput : function (taskType) {
        var output = this._getLastTaskOutput(taskType);
        return output && output.value;
    },

    _getLastTaskOutput : function (key) {
        if (!this._lastOutput) return null;
        var origKey = key;
        if (!key) key = this.getLastTaskClass();
        if (!key) return null;
        key = key.toLowerCase();
        if (origKey) origKey = origKey.toLowerCase();

        var output = this._lastOutput[key];
        // Allow shorthand for ClassName (ex. service for ServiceTask, decision for DecisionTask)
        if (origKey != null && output == null && !origKey.endsWith("task") && !origKey.endsWith("gateway")) {
            key = origKey + "task";
            output = this._lastOutput[key];
            if (output == null) {
                key = origKey + "gateway";
                output = this._lastOutput[key];
            }
        }
        return output;
    },

    // Returns the last task schema (with wrapper if desired)
    // For a wrappered return, the schema is expected to be used immediately and
    // will be destroyed on the next call if the last task schema changes.
    _$lastTaskDSID: isc._underscore + "lastTask",
    getLastTaskOutputSchema : function (key, wrap) {
        var output = this._getLastTaskOutput(key),
            schemaTask = output && output.schemaTask,
            schema = (schemaTask && schemaTask.getOutputSchema ? schemaTask.getOutputSchema() : null),
            outputSchema = schema
        ;
        if (schema && wrap) {
            outputSchema = isc.DS.get(this._$lastTaskDSID);
            if (!outputSchema || outputSchema.inheritsFrom != schema.getID()) {
                // Remove previous wrapper so we don't have log entries about replacing
                if (outputSchema) outputSchema.destroy();

                // Cannot use inheritsFrom for wrapper DS if schema has no ID (i.e. temporary)
                // Copy the fields instead.
                var fields;
                if (!schema.getID()) {
                    fields = [];
                    var fieldNames = schema.getFieldNames();
                    for (var i = 0; i < fieldNames.length; i++) {
                        var fieldName = fieldNames[i],
                            field = schema.getField(fieldName)
                        ;
                        fields.add(isc.clone(field));
                    }
                }

                // Create new wrapper DS
                outputSchema = isc.DS.create({
                    ID: this._$lastTaskDSID,
                    inheritsFrom: schema.getID(),
                    fields: fields,
                    clientOnly: true,
                    addGlobalId: false
                });
            }
        }
        return outputSchema;
    },

    destroyLastTaskOutputSchema : function () {
        var schema = isc.DS.get(this._$lastTaskDSID);
        if (schema && schema.destroy) {
            schema.destroy();
        }
    },

    handleTraceElement : function (element) {
        if (isc.isA.Class(element) &&
            (this.logIsInfoEnabled("workflow") || this.logIsDebugEnabled("workflow")))
        {
            var description = element.getElementDescription(),
                message = (this.traceElement ? "Trace element: " : "Execute element: ") +
                            element._constructor +
                            (description ? " '" + description + "'" : "");
            if (this.logIsDebugEnabled("workflow")) {
                this.logDebug(message + " - task detail: " + this.echo(element), "workflow");
            } else {
                this.logInfo(message, "workflow");
            }
        }
        if (this.traceElement) this.traceElement(element, this.traceContext);
    }

    //> @attr process.traceContext (Object : null : IRWA)
    // Context object to be passed to +link{traceElement} during process
    // execution.
    //
    // @visibility workflow
    //<
});

isc.Process.registerStringMethods({
    //> @method process.finished()
    // StringMethod called when a process completes, meaning the process executes a
    // ProcessElement with no next element.
    // @param state (Record) the final process state
    // @visibility workflow
    //<
    finished: "state",

    //> @method process.traceElement()
    // StringMethod called during process execution before each task
    // element is processed.
    // @param element (Task) the +link{Task} being executed
    // @param context (Object) the +link{traceContext}, if set
    // @visibility workflow
    //<
    traceElement: "element,context"
});

// --------------------------------------------------------------------------------------------

//> @class ServiceTask
// A ServiceTask is an element of a +link{Process} which calls a DataSource operation,
// optionally using part of the +link{Process.state,process state} as inputs or storing outputs
// in the process state. A special "export" +link{serviceTask.operationType,operationType} is
// supported to perform a server export based on criteria.
// <P>
// By default a ServiceTask takes the data indicated by +link{task.inputField,inputField} and/or
// +link{task.inputFieldList,inputFieldList} as detailed in +link{group:taskIO} and uses the
// <code>inputRecord</code> as +link{dsRequest.data}.  This means the input data becomes
// +link{Criteria} for "fetch" and "export" operations, new record values for an "add" operation, etc.
// For simplicity, if no <code>inputFieldList</code> is provided and <code>inputField</code>
// specifies an object, <code>inputData</code> is used as <code>dsRequest.data</code>.
// <P>
// Alternatively, you can set +link{serviceTask.criteria} for a "fetch" and "export" operations, or
// +link{serviceTask.values} for other operationTypes.  In both cases, you have the ability to
// use simple expressions like $input.<i>fieldName</i> to take portions of the input data and
// use it as part of the criteria or values.
// <P>
// OutputData and outputFieldList work as filters. You should determine which properties should
// be fetched into the process state. If you want to load all data without defining every
// property manually you can pass a name started with '$' and fetched record or records will be
// placed as a record or an array of records by the name without this specific symbol.
// <P>
// For example if you specify 'id' and 'name' in outputFieldList, only these properties will be
// fetched in the process state. If you pass '$orderHeader' in outputField a whole record will be
// stored in process state under the 'orderHeader' key.
//
// @inheritsFrom Task
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<



isc.defineClass("ServiceTask", "Task");

isc.ServiceTask.addClassProperties({
    // Indicates to WF Editor that task requires at least one DataSource to be edited
    requiresDataSources: true
});

isc.ServiceTask.addProperties({
    //> @attr serviceTask.dataSource (DataSource | GlobalId : null : IR)
    // DataSource ID or DataSource instance to be used.
    // @visibility workflow
    //<

    //> @attr serviceTask.operationType (DSOperationType : "fetch" : IR)
    // Type of operation to invoke. A special "export" operation type is
    // supported to perform a server export based on criteria.
    // @visibility workflow
    //<
    operationType: "fetch",

    //> @attr serviceTask.operationId (String : null : IR)
    // The +link{operationBinding.operationId,operationId} to invoke.
    // @visibility workflow
    //<

    //> @attr serviceTask.criteria (Criteria : null : IR)
    // Criteria (including AdvancedCriteria) to use for "fetch" and "export" operations.
    // <P>
    // Data values in this criteria prefixed with "$" will be treated as dynamic expressions
    // which can access the inputs to this task as $input - see
    // +link{group:taskInputExpression}.  Specifically, this means that for simple criteria,
    // any property value that is a String and is prefixed with "$" will be assumed to be an
    // expression, and for AdvancedCriteria, the same treatment will be applied to
    // +link{criterion.value}.
    // <P>
    // If any data value should not be treated as dynamic (for example, a "$" should be taken
    // as literal), you can place it in +link{fixedCriteria} instead.
    // <P>
    // Ignored for any operationType other than "fetch" and "export".  Update or delete operations should
    // place the primary key to update in +link{values}.
    // <p>
    // This property supports +link{group:dynamicCriteria} - use +link{criterion.valuePath}
    // to refer to values in the +link{Process.ruleScope}.
    // @group taskIO
    // @visibility workflow
    //<

    //> @attr serviceTask.values (Record : null : IR)
    // Values to be submitted for "update", "add" and "remove" operations.
    // <P>
    // Similar to +link{criteria}, data values prefixed with "$" will be treated as a
    // +link{group:taskInputExpression}.  Use +link{fixedValues} for any values that start with
    // "$" but should be treated as a literal.
    // @visibility workflow
    //<

    //> @attr serviceTask.fixedCriteria (Criteria : null : IR)
    // Criteria to be submitted as part of the DSRequest, regardless of inputs to the task.
    // Will be combined with the data from the +link{task.inputField} or with
    // +link{serviceTask.criteria} if specified, via +link{DataSource.combineCriteria()}.
    // @visibility workflow
    //<

    //> @attr serviceTask.fixedValues (Record : null : IR)
    // Values to be submitted as part of the DSRequest, regardless of inputs to the task. Will
    // be combined with the data from the +link{task.inputField} or with
    // +link{serviceTask.values} if specified, via simple copying of fields, with
    // <code>fixedValues</code> overwriting values provided by the <code>inputField</code>, but
    // explicitly specified +link{serviceTask.values} overriding <code>fixedValues</code>.
    // @visibility workflow
    //<

    //> @attr serviceTask.exportFormat (ExportFormat : "csv" : IR)
    // The format in which the data should be exported.  See +link{ExportFormat} for more
    // information.
    //
    // @visibility workflow
    //<

    //> @attr serviceTask.outputField (String : null : IR)
    // Field in the +link{Process.state,process state} where this task writes outputs. See
    // +link{group:taskIO}.
    // <p>
    // See +link{outputFieldList} for a shorthand method to save the full operation response
    // data.
    // @group taskIO
    // @visibility workflow
    //<

    //> @attr serviceTask.outputFieldList (Array of String : null : IR)
    // List of multiple fields in the +link{Process.state,process state} where this task will
    // write outputs. See +link{group:taskIO}.
    // <P>
    // If +link{outputField} is also specified, it will be implicitly added to the
    // <code>outputFieldList</code> if it is not already present.
    // <P>
    // In addition to pulling individual fields from the task operation result and placing
    // them into the process state the full response data can also be written into the
    // process state without specifying individual fields. Prefix a destination
    // field path with a "$" (ex. $orderHeader) causes the entire <code>dsResponse.data</code>
    // to be saved.
    // @group taskIO
    // @visibility workflow
    //<

    //> @attr serviceTask.failureElement (String : null : IR)
    // ID of the next sequence or element to proceed to if a failure condition arises
    // from DataSource operation.
    // @visibility workflow
    //<

    //> @attr serviceTask.passThruOutput (Boolean : false : IR)
    // @include processElement.passThruOutput
    //<
    passThruOutput: false,

    executeElement : function (process) {
        var ds = this.dataSource;
        if (ds.getClassName == null || ds.getClassName() != "DataSource") {
            ds = isc.DataSource.get(ds);
        }
        var requestData = this._createRequestData(process);
        if (this.operationType == "export") {
            var requestProperties = {
                exportAs: this.exportFormat,
                operationId: this.operationId
            };
            ds.exportData(requestData, requestProperties);
            // not async
            return true;
        }

        var params = isc.addProperties({}, this.requestProperties, { operationId: this.operationId });
        params.willHandleError = true;

        var task = this,
            operationType = this.operationType
        ;
        ds.performDSOperation(operationType, requestData, function(dsResponse, data, request) {
            var results = dsResponse.results,
                operation = request.operation;
            if (dsResponse.isStructured &&
                (!results || results.status < 0 || (results.status == null && dsResponse.status < 0)))
            {
                if (!isc.RPC.runDefaultErrorHandling(dsResponse, request, task.errorFormatter)) {
                    task.fail(process);
                    return;
                }
            }

            var output = data;
            if (isc.isAn.Array(data) && data.length > 0) {
                if (operationType == "fetch") {
                    var primaryKey = ds.getPrimaryKeyFieldName();
                    if (ds.isAdvancedCriteria(requestData)) {
                        var criterion = ds.getFieldCriterion(requestData, primaryKey);
                        if (criterion && criterion.operator == "equals") {
                            output = data[0];
                        }
                    } else if (ds.defaultTextMatchStyle == "equals" && requestData[primaryKey] != null) {
                        output = data[0];
                    }
                } else if (operationType != "custom") {
                    output = data[0];
                }
            }
            process.setTaskOutput(task, output);

            if (!isc.isAn.Array(data)) data = [data];

            if (data.length > 0) {
                var fieldsToProcess = [];
                if (task.outputFieldList) {
                    fieldsToProcess.addList(task.outputFieldList);
                }
                if (task.outputField) fieldsToProcess.add(task.outputField);
                for (var i = 0; i < fieldsToProcess.length; i++) {
                    var fieldName = fieldsToProcess[i];
                    if (fieldName.startsWith("$")) {
                        var value = data.length == 1 ? data[0] : data;
                        fieldName = fieldName.substring(1);
                        process.setStateVariable(fieldName, value);
                    } else {
                        var key = fieldName;
                        var ldi = key.lastIndexOf(".");
                        if (ldi > 0) {
                            key = key.substring(ldi + 1);
                        }
                        var value = data[0][key];
                        if (typeof value != 'undefined') {
                            if (data.length > 1) {
                                value = [value];
                                for (var i = 1; i < data.length; i++) {
                                  value.add(data[i][key])
                                }
                            }
                            process.setStateVariable(fieldName, value);
                        }
                    }
                };
                task._writeOutputExpression(data);
            }
            process.start();
        }, params);
        return false;
    },

    _createRequestData : function (process, skipDynamicExpressions) {
        var inputData;
        var inputRecord = {};
        if (this.inputFieldList) {
            for (var i = 0; i < this.inputFieldList.length; i++) {
                var key = this.inputFieldList[i];
                var ldi = key.lastIndexOf(".");
                if (ldi > 0) {
                    key = key.substring(ldi + 1);
                }
                inputRecord[key] = process.getStateVariable(this.inputFieldList[i]);
            };
        }
        if (this.inputField) {
            var key = this.inputField;
            if (!skipDynamicExpressions && key.startsWith("$")) {
                inputData = this._resolveInputField(key, process);
            }
            var ldi = key.lastIndexOf(".");
            if (ldi > 0) {
                key = key.substring(ldi + 1);
            }
            if (inputData == null) inputData = process.getStateVariable(this.inputField);
            inputRecord[key] = inputData;
        }

        var data = null;
        if (this.operationType == "fetch" || this.operationType == "export") {
            if (this.criteria && !skipDynamicExpressions) {
                // Resolve any dynamicCriteria or taskInputExpressions. Returns a copy of criteria.
                data = this._resolveCriteriaExpressions(this.criteria, inputData, inputRecord, process);
            } else if (this.criteria) {
                data = this.criteria;
            }
            if (this.fixedCriteria) {
                if (data == null && inputRecord == null) {
                    data = this.fixedCriteria
                } else {
                    var crit = isc.clone(this.fixedCriteria);
                    if (inputRecord) {
                        crit = isc.DataSource.combineCriteria(inputRecord, crit);
                    }
                    if (data) {
                        crit = isc.DataSource.combineCriteria(data, crit);
                    }
                    data = crit;
                }
            }
        }
        if (data == null) {
            data = (this.inputFieldList == null && isc.isAn.Object(inputData) ? inputData : inputRecord);
        }
        if (this.operationType != "fetch" && this.operationType != "export") {
            if (this.values) {
                data = this.values;
                if( !skipDynamicExpressions) {
                    // Resolve any dynamicCriteria or taskInputExpressions. Returns a copy of values.
                    data = this._resolveObjectDynamicExpressions(this.values, inputData, inputRecord, process);
                }
            }
            if (this.fixedValues) {
                for (var key in this.fixedValues) {
                    data[key] = this.fixedValues[key];
                }
            }
        }

        return data;
    },

    fail : function (process) {
        if (!this.failureElement) {
            this.logInfo("ServiceTask does not have a failureElement. Process is aborting.");
            // the call to setNextElement() below will cause the process to terminate automatically
        }
        if (this.failureElement == "next") {
            process.start();
        } else {
            process.setNextElement(this.failureElement);
        }
    },

    // "this" is not available
    errorFormatter : function (codeName, response, request) {
        if (codeName == "VALIDATION_ERROR") {
            var errors = response.errors,
                message = ["Server returned validation errors:<BR><UL>"]
            ;
            if (!isc.isAn.Array(errors)) errors = [errors];
            for (var i = 0; i < errors.length; i++) {
                var error = errors[i];
                for (var field in error) {
                    var fieldErrors = error[field];
                    message.add("<LI><B>" + field + ":</B> ");
                    if (!isc.isAn.Array(fieldErrors)) fieldErrors = [fieldErrors];
                    for (var j = 0; j < fieldErrors.length; j++) {
                        var fieldError = fieldErrors[j];
                        message.add((j > 0 ? "<BR>" : "") + (isc.isAn.Object(fieldError) ? fieldError.errorMessage : fieldError));
                    }
                    message.add("</LI>");
                }
            }
            message.add("</UL>");
            return message.join("");
        }
        return null;
    },

    getElementDescription : function () {
        if (!this.dataSource) return "";
        var operation = this.operationType.substring(0,1).toUpperCase() + this.operationType.substring(1);
        if (this.operationId) operation += " (" + this.operationId + ")";
        var description = operation + " " + this.dataSource,
            data = this._createRequestData({ getStateVariable : function (stateVariablePath) { return stateVariablePath; } }, true)
        ;

        if (this.operationType == "fetch" || this.operationType == "remove" || this.operationType == "export") {
            if (data && !isc.isAn.emptyObject(data)) {
                if (!isc.DS.isAdvancedCriteria(data)) {
                    data = isc.DS.convertCriteria(data, (this.operationType == "remove" ? "exact" : null));
                }
                var dsFields = isc.DecisionTask._processFieldsRecursively(data);
                // construct datasource for fields used in criteria
                var fieldsDS = isc.DataSource.create({
                    addGlobalId: false,
                    fields: dsFields
                });
                description += " where <ul>" + isc.DataSource.getAdvancedCriteriaDescription(data, fieldsDS, null, {prefix: "<li>", suffix: "</li>"}) + "</ul>";
                fieldsDS.destroy();
            } else {
                description = operation + " all " + this.dataSource + " records";
            }
        } else {
            description += " record";
        }

        return description;
    },

    typeTitle: "DataSource Fetch Data",
    editorType: "ServiceTaskEditor",

    getOutputSchema : function () {
        var ds = this.dataSource;
        if (ds && (ds.getClassName == null || ds.getClassName() != "DataSource")) {
            ds = isc.DataSource.get(ds);
        }
        return ds;
    },

    updateLastElementBindingReferences : function (taskType) {
        var changed = this.Super("updateLastElementBindingReferences", arguments);

        if (this.criteria && (this.operationType == "fetch" || this.operationType == "export")) {
            changed = this.updateLastElementInCriteria(this.criteria, taskType) || changed;
        }

        if (this.values && this.operationType != "fetch" && this.operationType != "export") {
            changed = this.updateLastElementInValues(this.values, taskType) || changed;
        }

        return changed;
    },

    updateGlobalIDReferences : function (oldId, newId) {
        var changed = this.Super("updateGlobalIDReferences", arguments);

        if (this.criteria && (this.operationType == "fetch" || this.operationType == "export")) {
            changed = this.updateGlobalIDInCriteria(this.criteria, oldId, newId) || changed;
        }

        if (this.values && this.operationType != "fetch" && this.operationType != "export") {
            changed = this.updateGlobalIDInValues(this.values, oldId, newId) || changed;
        }

        return changed;
    }
});

//> @class DSFetchTask
// A +link{ServiceTask,ServiceTask} configured to perform a fetch.
//
// @inheritsFrom ServiceTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("DSFetchTask", "ServiceTask").addProperties({
    typeTitle: "DataSource Fetch",
    classDescription: "Retrieve data from a DataSource which match specified criteria",
    editorType: "ServiceTaskEditor",
    editorProperties: { showOperationTypePicker: false },

    operationType: "fetch"
});

//> @class DSAddTask
// A +link{ServiceTask,ServiceTask} configured to perform an add.
//
// @inheritsFrom ServiceTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("DSAddTask", "ServiceTask").addProperties({
    typeTitle: "DataSource Add",
    classDescription: "Add a new record",
    editorType: "DSAddTaskEditor",
    editorProperties: { showOperationTypePicker: false },

    operationType: "add"
});

//> @class DSUpdateTask
// A +link{ServiceTask,ServiceTask} configured to perform a update.
//
// @inheritsFrom ServiceTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("DSUpdateTask", "ServiceTask").addProperties({
    typeTitle: "DataSource Update",
    classDescription: "Update an existing record",
    editorType: "DSUpdateTaskEditor",
    editorProperties: { showOperationTypePicker: false },

    operationType: "update"
});

//> @class DSRemoveTask
// A +link{ServiceTask,ServiceTask} configured to perform a remove.
//
// @inheritsFrom ServiceTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("DSRemoveTask", "ServiceTask").addProperties({
    typeTitle: "DataSource Remove",
    classDescription: "Remove an existing record",
    editorType: "ServiceTaskEditor",
    editorProperties: { showOperationTypePicker: false },

    operationType: "remove"
});


// --------------------------------------------------------------------------------------------

//> @class ScriptTask
// Task that executes arbitrary code, either synchronous or asynchronous.  Override the
// +link{scriptTask.execute(), execute()} method to provide custom logic.
//
// @inheritsFrom Task
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("ScriptTask", "Task");

isc.ScriptTask.addProperties({
    //> @attr scriptTask.passThruOutput (Boolean : false : IR)
    // @include processElement.passThruOutput
    //<
    passThruOutput: false,

    //> @method scriptTask.getProcess()
    // Get the process executing this task instance.
    // @return (Process) the owning process
    // @visibility workflow
    //<
    getProcess : function () {
        return this._process;
    },

    //> @method scriptTask.getInputData()
    // Get the inputs to this task as specified by +link{task.inputField}.
    // <P>
    // For a task with a +link{task.inputFieldList,inputFieldList}, use +link{getInputRecord}
    // to get access to other inputs.
    // @return (Any) input data
    // @group taskIO
    // @visibility workflow
    //<
    getInputData : function () {
        return this.inputData;
    },

    //> @method scriptTask.setOutputData()
    // Set the task output as specified by +link{task.outputField}.
    // <P>
    // NOTE: for an +link{scriptTask.isAsync,asychronous task}, calling
    // <code>setOutputData()</code> indicates the task is complete.  For a task with
    // +link{task.outputFieldList,multiple outputs}, call +link{setOutputRecord()} instead.
    // @param taskOutput (Any) task output
    // @group taskIO
    // @visibility workflow
    //<
    setOutputData : function (taskOutput) {
        this._finishTask(this._process, null, taskOutput);
    },

    //> @method scriptTask.getInputRecord()
    // Get all inputs to the task as specified by the
    // +link{task.inputFieldList,inputFieldList}, as a Record.
    // @return (Record) input data
    // @group taskIO
    // @visibility workflow
    //<
    getInputRecord : function () {
        return this.inputRecord;
    },

    //> @method scriptTask.setOutputRecord()
    // Set all outputs of the task as specified by the
    // +link{task.outputFieldList,outputFieldList}, by providing a Record.
    // @param outputRecord (Record) output record
    // @group taskIO
    // @visibility workflow
    //<
    setOutputRecord : function (outputRecord) {
        this._finishTask(this._process, outputRecord);
    },

    //> @attr scriptTask.isAsync (Boolean : false : IR)
    // Whether the script task is asynchronous.  A synchronous task is expected to return data
    // directly from execute() and is considered complete once the execute() method exits.
    // <P>
    // An asnychronous task is expected to start processing in execute(), and will not be
    // considered complete until either +link{setOutputData()} or +link{setOutputRecord} is
    // called.
    // @visibility workflow
    //<
    isAsync : false,

    executeElement : function (process) {
        // process input
        var inputData;
        var inputRecord = {};
        if (this.inputFieldList) {
            for (var i = 0; i < this.inputFieldList.length; i++) {
                var key = this.inputFieldList[i];
                var ldi = key.lastIndexOf(".");
                if (ldi > 0) {
                    key = key.substring(ldi + 1);
                }
                var value = process.getStateVariable(this.inputFieldList[i]);
                inputRecord[key] = (!isc.isA.Class(value) ? isc.clone(value) : value);
            };
        }
        if (this.inputField) {
            var key = this.inputField;
            if (key.startsWith("$")) {
                var value = this._resolveInputField(key, process);
                inputData = (!isc.isA.Class(value) ? isc.clone(value) : value);
            }
            var ldi = key.lastIndexOf(".");
            if (ldi > 0) {
                key = key.substring(ldi + 1);
            }
            if (inputData == null) {
                var value = process.getStateVariable(this.inputField);
                inputData = (!isc.isA.Class(value) ? isc.clone(value) : value);
            }
            inputRecord[key] = inputData;
        }

        // Save inputs so they can be referenced asynchronously
        this.inputData = inputData;
        this.inputRecord = inputRecord;
        this._process = process;

        try {
            var output = this.execute(inputData, inputRecord);
        } catch (e) {
            isc.logWarn("Error while executing ScriptTask: "+e.toString());
        }

        if (this.isAsync) {
            return false;
        }

        if (typeof output == 'undefined') {
            return true;
        }

        this._processTaskOutput(process, output);
        return true;
    },

    _processTaskOutput : function (process, output) {
        process.setTaskOutput(this, output);

        // process output
        if (this.outputFieldList) {
            for (var i = 0; i < this.outputFieldList.length; i++) {
                var key = this.outputFieldList[i];
                var ldi = key.lastIndexOf(".");
                if (ldi > 0) {
                    key = key.substring(ldi + 1);
                }
                var value = output[key];
                if (typeof value != 'undefined') {
                    process.setStateVariable(this.outputFieldList[i], value);
                }
            };
        }
        if (this.outputField) {
            if (this.outputFieldList == null) {
                if (typeof output != 'undefined') {
                    process.setStateVariable(this.outputField, output);
                }
            } else {
                var key = this.outputField;
                var ldi = key.lastIndexOf(".");
                if (ldi > 0) {
                    key = key.substring(ldi + 1);
                }
                var value = output[key];
                if (typeof value != 'undefined') {
                    process.setStateVariable(this.outputField, value);
                }
            }
        }
        this._writeOutputExpression(output);
    },

    _finishTask : function (process, outputRecord, outputData) {
        if (outputRecord == null) {
            this._processTaskOutput(process, outputData);
        } else {
            if (outputData) {
                var key = this.outputField;
                var ldi = key.lastIndexOf(".");
                if (ldi > 0) {
                    key = key.substring(ldi + 1);
                }
                outputRecord[key] = outputData;
            }
            this._processTaskOutput(process, outputRecord);
        }

        if (this.isAsync) {
            process.start();
        }
    },

    getCustomDefaults : function () {
        return { execute: isc.Func.getBody(this.execute) };
    }
});

isc.ScriptTask.registerStringMethods({
    //> @method scriptTask.execute()
    // Execute the task.
    // @param input (Any) the task input
    // @param inputRecord (Record) the task input record if an <code>inputFieldList</code> was
    // specified. See +link{group:taskIO}
    // @return (Any) the task output.  For multiple field output, call
    // +link{setOutputRecord()} instead, and return null
    // @visibility workflow
    //<
    execute: "input,inputRecord"
});

// --------------------------------------------------------------------------------------------

//> @class DecisionTask
// Chooses one or another next process element based on AdvancedCriteria applied to
// +link{process.state}.
// <P>
// If the AdvancedCriteria evaluate to true, the +link{decisionTask.nextElement,nextElement} is
// chosen, otherwise the +link{decisionTask.failureElement,failureElement}.
//
// @inheritsFrom ProcessElement
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<

isc.defineClass("DecisionTask", "ProcessElement");

isc.DecisionTask.addClassProperties({
    _processFieldsRecursivelyValuesOnly : function (criteria) {
        var dsFields = [];
        if (criteria.fieldName) {
            if (!dsFields.contains(criteria.fieldName)) {
                dsFields.add(criteria.fieldName);
            }
        } else if (criteria.criteria) {
            for (var i = 0; i < criteria.criteria.length; i++) {
                var fs = this._processFieldsRecursivelyValuesOnly(criteria.criteria[i]);
                for (var j = 0; j < fs.length; j++) {
                    if (!dsFields.contains(fs[j])) {
                        dsFields.add(fs[j]);
                    }
                }
            }
        } else {
            for (var key in criteria) {
                if (!dsFields.contains(key)) {
                    dsFields.add(key);
                }
            }
        }
        return dsFields
    },
    _processFieldsRecursively : function (criteria) {
        var res = [];
        var dsFields = isc.DecisionTask._processFieldsRecursivelyValuesOnly(criteria);
        for (var i = 0; i < dsFields.length; i++) {
            var fieldName = dsFields[i],
                splitFieldName = fieldName.split("."),
                title = isc.DS.getAutoTitle(splitFieldName[splitFieldName.length-1])
            ;
            res.add({
                name: fieldName,
                title: title
            });
        }
        return res;
    }
});

isc.DecisionTask.addProperties({
    //> @attr decisionTask.criteria (Criteria : null : IR)
    // Simple or +link{AdvancedCriteria} to be applied against the +link{process.state}.
    // <P>
    // Data values in this criteria prefixed with "$" will be treated as dynamic expressions
    // as detailed in +link{group:taskInputExpression}.  Specifically, this means that for
    // simple criteria, any property value that is a String and is prefixed with "$" will be
    // assumed to be an expression, and for AdvancedCriteria, the same treatment will be
    // applied to +link{criterion.value}.
    // <p>
    // Note that dynamic expressions starting with "$input" are not applicable
    // for an DecisionTask but "$inputRecord" can be used for direct reference to +link{Process.state}.
    // <p>
    // This property supports +link{group:dynamicCriteria} - use +link{criterion.valuePath}
    // to refer to values in the +link{Process.ruleScope}.
    //
    // @visibility workflow
    //<

    //> @attr decisionTask.nextElement (String : null : IR)
    // Next +link{process.sequences,sequence} or +link{process.elements,element} to execute
    // if the criteria match the process state.
    // <p>
    // <code>nextElement</code> does not need to be specified if this element is part of a
    // +link{Process.sequences,sequence} and has a next element in the sequence.
    // <p>
    // Note that if there is both a <code>sequence</code> and a normal <code>element</code>
    // with the same name in the current <code>Process</code>, the <code>sequence</code> will
    // be used.
    //
    // @visibility workflow
    //<

    //> @attr decisionTask.failureElement (String : null : IR)
    // ID of the next sequence or element to proceed to if the criteria do not match.
    // @visibility workflow
    //<

    typeTitle: "Decision",
    classDescription: "Choose the next task based on criteria",
    editorType: "DecisionTaskEditor",

    executeElement : function (process) {
        process.passThruTaskOutput(this);
        var lastTaskOutput = process.getLastTaskOutput();

        var criteria = this.criteria;
        if (criteria) {
            criteria = this._resolveCriteriaExpressions(criteria, process.state, process.state, process);
        }
        var data = [];
        if (process.state && !isc.isAn.emptyObject(process.state)) {

            data.add(process.state);
        }
        if (lastTaskOutput) {
            data.add({ "$last": lastTaskOutput });
        }
        if (process.ruleScope) {
            var ruleScopeComponent = window[process.ruleScope];
            if (ruleScopeComponent && !ruleScopeComponent.destroyed) {
                data.add(ruleScopeComponent.getRuleContext());
            }
        }
        if (criteria && isc.DS.applyFilter(data, criteria).length == 1) {
            if (this.nextElement) process.setNextElement(this.nextElement);
        } else {
            if (!this.failureElement) {
                this.logInfo("DecisionTask does not have a failureElement. Process is aborting.");
                // the call to setNextElement() below will cause the process to terminate automatically
            }
            process.setNextElement(this.failureElement);
        }
        return true;
    },

    getElementDescription : function () {
        var description = "No criteria - always fail";
        if (this.criteria) {
            var dsFields = isc.DecisionTask._processFieldsRecursively(this.criteria);
            // construct datasource for fields used in criteria
            var fieldsDS = isc.DataSource.create({
                addGlobalId: false,
                fields: dsFields
            });

            description = "when <ul>" + isc.DataSource.getAdvancedCriteriaDescription(this.criteria, fieldsDS, null, {prefix: "<li>", suffix: "</li>"}) + "</ul>";
            fieldsDS.destroy();
        }
        return description;
    },

    getPlaceholders : function () {
        return (this.failureElement == isc.Process.decisionPlaceholderSelection ? ["failureElement"] : null);
    },

    setPlaceholderId : function (placeholder, id) {
        if (placeholder == "failureElement") this.failureElement = id;
    },

    updateLastElementBindingReferences : function (taskType) {
        var changed = this.Super("updateLastElementBindingReferences", arguments);
        changed = this.updateLastElementInCriteria(this.criteria, taskType) || changed;
        return changed;
    },

    updateGlobalIDReferences : function (oldId, newId) {
        var changed = this.Super("updateGlobalIDReferences", arguments);
        changed = this.updateGlobalIDInCriteria(this.criteria, oldId, newId) || changed;
        return changed;
    }
});

//> @class XORGateway
// Chooses one or another next process element based on AdvancedCriteria applied to
// +link{process.state}.
// <P>
// If the AdvancedCriteria evaluate to true, the +link{decisionTask.nextElement,nextElement} is
// chosen, otherwise the +link{decisionTask.failureElement,failureElement}.
// <P>
// Note that "XOR" in <code>XORGateway</code> means "exclusive or" - only one next element is
// chosen.
//
// @inheritsFrom DecisionTask
// @treeLocation Client Reference/Workflow
// @deprecated In favor of +link{DecisionTask} as of <smartclient>SmartClient</smartclient>
// <smartgwt>SmartGWT</smartgwt> release 13.0
// @visibility workflow
//<
isc.defineClass("XORGateway", "DecisionTask");


// --------------------------------------------------------------------------------------------

//> @class UserConfirmationTask
// Chooses one or another next process element based on confirmation of a message shown to user.
// <P>
// If the user clicks OK, the +link{userConfirmationTask.nextElement,nextElement} is
// chosen, otherwise the choice is +link{userConfirmationTask.failureElement,failureElement}.
//
// @inheritsFrom ProcessElement
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("UserConfirmationTask", "ProcessElement");

isc.UserConfirmationTask.addProperties({
    //> @attr userConfirmationTask.message (String : null : IR)
    // Message to display to the user for confirmation. To display a dynamic message see
    // +link{textFormula}.
    // @visibility workflow
    //<

    //> @attr userConfirmationTask.textFormula (UserSummary : null : IR)
    // Formula to be used to calculate the message contents. Use +link{message} property
    // to assign a static message instead.
    // <p>
    // Available fields for use in the formula are the current +link{canvas.ruleScope,rule context}.
    //
    // @visibility workflow
    //<

    //> @attr userConfirmationTask.nextElement (String : null : IR)
    // Next +link{process.sequences,sequence} or +link{process.elements,element} to execute
    // if the criteria match the process state.
    // <p>
    // <code>nextElement</code> does not need to be specified if this gateway is part of a
    // +link{Process.sequences,sequence} and has a next element in the sequence.
    // <p>
    // Note that if there is both a <code>sequence</code> and a normal <code>element</code>
    // with the same name in the current <code>Process</code>, the <code>sequence</code> will
    // be used.
    //
    // @visibility workflow
    //<

    //> @attr userConfirmationTask.failureElement (String : null : IR)
    // ID of the next sequence or element to proceed to if the criteria do not match.
    // @visibility workflow
    //<

    typeTitle: "Confirm with user",
    classDescription: "Choose the next task based on user confirmation",
    editorType: "UserConfirmationTaskEditor",

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var message = this.getTextFormulaValue(this.textFormula, process) ||
                      this.getDynamicValue(this.message, process);

        var task = this;
        isc.confirm(message, function (value) {
            if (value) {
                if (task.nextElement) process.setNextElement(task.nextElement);
            } else {
                if (!task.failureElement) {
                    task.logInfo("User Confirmation Gateway does not have a failureElement. Process is aborting.");
                    // the call to setNextElement() below will cause the process to terminate automatically
                }
                process.setNextElement(task.failureElement);
            }
            process.start();
        });

        // processing confirmation asynchronously
        return false;
    },

    getElementDescription : function () {
        var message = (this.textFormula ? this.textFormula.text : this.message) || "",
            messageParts = message.split(" "),
            shortMessage = messageParts.getRange(0, 3).join(" ")
        ;
        if (shortMessage.length > 25) shortMessage = shortMessage.substring(0,25);
        if (shortMessage != message) shortMessage += " ...";

         "Confirm with user: <br>" + shortMessage;
    },

    getPlaceholders : function () {
        return (this.failureElement == isc.Process.decisionPlaceholderSelection ? ["failureElement"] : null);
    },

    setPlaceholderId : function (placeholder, id) {
        if (placeholder == "failureElement") this.failureElement = id;
    }
});

//> @class UserConfirmationGateway
// Chooses one or another next process element based on confirmation of a message shown to user.
// <P>
// If the user clicks OK, the +link{decisionTask.nextElement,nextElement} is
// chosen, otherwise the choice is +link{decisionTask.failureElement,failureElement}.
//
// @inheritsFrom ProcessElement
// @treeLocation Client Reference/Workflow
// @deprecated In favor of +link{UserConfirmationTask} as of <smartclient>SmartClient</smartclient>
// <smartgwt>SmartGWT</smartgwt> release 13.0
// @visibility workflow
//<
isc.defineClass("UserConfirmationGateway", "UserConfirmationTask");

// --------------------------------------------------------------------------------------------

//> @object DecisionBranch
// Identifies a potential branch within a +link{multiDecisionTask}. Each decision
// has a criteria and a target ProcessElement ID.
//
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<

//> @attr decisionBranch.criteria (Criteria : null : IR)
// Criteria identifying when the +link{targetTask} should be chosen within a
// +link{multiDecisionTask.decisionList}.
// <P>
// Data values in this criteria prefixed with "$" will be treated as dynamic expressions
// as detailed in +link{group:taskInputExpression}.  Specifically, this means that for
// simple criteria, any property value that is a String and is prefixed with "$" will be
// assumed to be an expression, and for AdvancedCriteria, the same treatment will be
// applied to +link{criterion.value}.
// <P>
// Note that dynamic expressions starting with "$input" are not applicable
// in this context but "$inputRecord" can be used for direct reference to +link{Process.state}.
// <p>
// This property supports +link{group:dynamicCriteria} - use +link{criterion.valuePath}
// to refer to values in the +link{Process.ruleScope}.
//
// @visibility workflow
//<

//> @attr decisionBranch.targetTask (String : null : IR)
// +link{ProcessElement.ID} of element to be used as next element if
// +link{decisionBranch.criteria,criteria} matches.
// @visibility workflow
//<

//> @object TaskDecision
// Identifies a potential decision (branch) within a +link{multiDecisionTask}. Each decision
// has a criteria and a target ProcessElement ID.
//
// @treeLocation Client Reference/Workflow
// @inheritsFrom DecisionBranch
// @deprecated In favor of +link{DecisionBranch} as of <smartclient>SmartClient</smartclient>
// <smartgwt>SmartGWT</smartgwt> release 13.0
// @visibility workflow
//<

//> @class MultiDecisionTask
// Chooses a next element in a +link{Process} by evaluating a series of criteria against the
// +link{process.state} and choosing the element associated with the criteria that matched, or
// a +link{multiDecisionTask.defaultElement, defaultElement} if none of the criteria match.
//
// @inheritsFrom ProcessElement
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<

isc.defineClass("MultiDecisionTask", "ProcessElement");

isc.MultiDecisionTask.addProperties({
    //> @attr multiDecisionTask.decisionList (Array of DecisionBranch : null : IR)
    // List of +link{decisionBranch,DecisionBranchs} to be processed to find the first with matching
    // criteria. The specified +link{decisionBranch.targetTask} is then used to identify the the next
    // element.
    // <P>
    // If no criteria is matched the next element is +link{defaultElement} or the workflow
    // is finished.
    // <p>
    // When providing a MultiDecisionTask in XML, the <code>decisionList</code> is expressed as:
    // <pre>
    //     &lt;MultiDecisionTask ID="continentDecision" description="Which continent?" defaultElement="summary"&gt;
    //         &lt;decisionList&gt;
    //             &lt;decisionBranch targetTask="europeVATTask"&gt;
    //                 &lt;criteria fieldName="order.continent" operator="equals" value="Europe" /&gt;
    //             &lt;/decisionBranch&gt;
    //             ...
    //         &lt;/decisionList&gt;
    //     &lt;MultiDecisionTask&gt;
    // </pre>
    // @visibility workflow
    //<

    //> @attr multiDecisionTask.defaultElement (String : null : IR)
    // Next element to pick if no criteria match.  If this decision is part of a
    // +link{process.sequences,sequence} and has a next element in the sequence, the
    // <code>defaultElement</code> is assumed to be the next element and does not need to be
    // specified.
    // @visibility workflow
    //<

    //> @attr multiDecisionTask.nextElement  (String : null : IR)
    // Not applicable to a MultiDecisionTask.
    // @see multiDecisionTask.defaultElement
    // @visibility workflow
    //<

    // Suppress an add element box in workflow editor after this element
    _canAddNextElement: false,

    typeTitle: "Multi Decision",
    classDescription: "Choose multiple possible next tasks based on criteria",
    editorType: "MultiDecisionTaskEditor",

    executeElement : function (process) {
        this._convertCriteriaMap();
        if (!this.decisionList) this.decisionList = [];

        var lastTaskOutput = process.getLastTaskOutput();
        process.passThruTaskOutput(this);

        for (var i = 0; i < this.decisionList.length; i++) {
            var decisionBranch = this.decisionList[i],
                criteria = decisionBranch.criteria
            ;
            if (criteria) {
                criteria = this._resolveCriteriaExpressions(criteria, process.state, process.state, process);
            }
            var dsFields = isc.DecisionTask._processFieldsRecursively(criteria);
            // construct datasource that will check all fields in process state
            var ds = isc.DataSource.create({
                fields: dsFields
            });

            var data = [];
            if (process.state && !isc.isAn.emptyObject(process.state)) {

                data.add(process.state);
            }
            if (lastTaskOutput) {
                data.add({ "$last": lastTaskOutput });
            }
            if (process.ruleScope) {
                var ruleScopeComponent = window[process.ruleScope];
                if (ruleScopeComponent && !ruleScopeComponent.destroyed) {
                    data.add(ruleScopeComponent.getRuleContext());
                }
            }

            if (ds.applyFilter(data, criteria).length == 1) {
                process.setNextElement(decisionBranch.targetTask);
                return true;
            }
        }
        if (this.defaultElement) process.setNextElement(this.defaultElement);
        return true;
    },

    _convertCriteriaMap : function () {
        if (!this.decisionList && this.criteriaMap) {
            // convert criteriaMap to decisionList
            var decisionList = [];
            for (var key in this.criteriaMap) {
                decisionList.add({
                    criteria: this.criteriaMap[key],
                    targetTask: key
                });
            }
            this.decisionList = decisionList;
        }
    },

    getElementDescription : function () {
        this._convertCriteriaMap();
        var description = "Multi-branch";
        if ((!this.decisionList || this.decisionList.length == 0) && this.defaultElement) {
            // An immediate jump (i.e. go to)
            description = "Go to " + this.defaultElement;
        }
        return description;
    },


    dropElementReferences : function (ID) {
        this._convertCriteriaMap();

        if (this.decisionList) {
            var decisionsToDrop = [];
            for (var i = 0; i < this.decisionList.length; i++) {
                var decisionBranch = this.decisionList[i];
                if (decisionBranch.targetTask == ID) decisionsToDrop.add(decisionBranch);
            }
            if (decisionsToDrop.length > 0) this.decisionList.removeList(decisionsToDrop);
        }
        if (this.defaultElement == ID) this.defaultElement = null;
    },


    updateElementReferences : function (oldID, newID) {
        this._convertCriteriaMap();

        if (this.decisionList) {
            for (var i = 0; i < this.decisionList.length; i++) {
                var decisionBranch = this.decisionList[i];
                if (decisionBranch.targetTask == oldID) decisionBranch.targetTask = newID;
            }
        }
        if (this.defaultElement == oldID) this.defaultElement = newID;
    },

    getPlaceholders : function () {
        this._convertCriteriaMap();

        var placeholders = [];
        if (this.decisionList) {
            for (var i = 0; i < this.decisionList.length; i++) {
                var decisionBranch = this.decisionList[i];
                if (decisionBranch.targetTask == isc.Process.decisionPlaceholderSelection) {
                    placeholders.add("" + i);
                }
            }
        }
        if (this.defaultElement == isc.Process.decisionPlaceholderSelection) {
            placeholders.add("defaultElement");
        }
        return placeholders;
    },

    setPlaceholderId : function (placeholder, id) {
        if (placeholder == "defaultElement") {
            this.defaultElement = id;
        } else {
            var index = parseInt(placeholder);
            this.decisionList[index].targetTask = id;
        }
    },

    updateLastElementBindingReferences : function (taskType) {
        var changed = this.Super("updateLastElementBindingReferences", arguments);
        if (this.decisionList) {
            for (var i = 0; i < this.decisionList.length; i++) {
                var decisionBranch = this.decisionList[i],
                    criteria = decisionBranch.criteria
                ;
                changed = this.updateLastElementInCriteria(this.criteria, taskType) || changed;
            }
        }
        return changed;
    },

    updateGlobalIDReferences : function (oldId, newId) {
        var changed = this.Super("updateGlobalIDReferences", arguments);
        if (this.decisionList) {
            for (var i = 0; i < this.decisionList.length; i++) {
                var decisionBranch = this.decisionList[i],
                    criteria = decisionBranch.criteria
                ;
                changed = this.updateGlobalIDInCriteria(criteria, oldId, newId) || changed;
            }
        }
        return changed;
    }
});

//> @class DecisionGateway
// Chooses a next element in a +link{Process} by evaluating a series of criteria against the
// +link{process.state} and choosing the element associated with the criteria that matched, or
// a +link{multiDecisionTask.defaultElement, defaultElement} if none of the criteria match.
//
// @inheritsFrom MultiDecisionTask
// @treeLocation Client Reference/Workflow
// @deprecated In favor of +link{MultiDecisionTask} as of <smartclient>SmartClient</smartclient>
// <smartgwt>SmartGWT</smartgwt> release 13.0
// @visibility workflow
//<
isc.defineClass("DecisionGateway", "MultiDecisionTask");

isc.DecisionGateway.addProperties({
    //> @attr decisionGateway.criteriaMap (Map<String,Criteria> : null : IR)
    // A Map from +link{ProcessElement.ID} to Criteria that will cause this ProcessElement to
    // be chosen as the next element if the criteria matches.
    // <P>
    // If no criteria is matched the next element is +link{defaultElement} or the workflow
    // is finished.
    // <P>
    // Data values in this criteria prefixed with "$" will be treated as dynamic expressions
    // as detailed in +link{group:taskInputExpression}.  Specifically, this means that for
    // simple criteria, any property value that is a String and is prefixed with "$" will be
    // assumed to be an expression, and for AdvancedCriteria, the same treatment will be
    // applied to +link{criterion.value}.
    // <P>
    // Note that dynamic expressions starting with "$input" are not applicable
    // for an decisionGateway but "$inputRecord" can be used for direct reference to +link{Process.state}.
    // <p>
    // This property supports +link{group:dynamicCriteria} - use +link{criterion.valuePath}
    // to refer to values in the +link{Process.ruleScope}.
    //
    // @visibility workflow
    // @deprecated In favor of +link{decisionList} as of <smartclient>SmartClient</smartclient>
    // <smartgwt>SmartGWT</smartgwt> release 12.1
    //<
});

// --------------------------------------------------------------------------------------------

//> @class UserTask
// A task that involves showing a user interface to the end user allowing the user to view and
// input data and press a button (or do some other UI gesture) to complete the task.
// <P>
// A UserTask takes the following steps:
// <ul>
// <li> Optionally show() or otherwise make visible the +link{userTask.targetView, targetView}
//      or +link{userTask.inlineView, inlineView}
// <li> Provide values to either a +link{DynamicForm} designated as the +link{userTask.targetForm, targetForm} or to
//      a +link{ValuesManager} designated as the +link{userTask.targetVM, targetVM}, via +link{ValuesManager.setValues(),setValues()}
// <li> Waits for notification of completion or cancellation.  The UserTask is notified of
//      completion if a +link{SubmitItem} is pressed in either the <code>targetForm</code> or
//      any form that is a member of the <code>targetVM</code>.  Likewise a +link{CancelItem}
//      triggers cancellation.  Direct calls to +link{dynamicForm.cancelEditing()} or
//      +link{dynamicForm.completeEditing()} achieve the same result.
// <li> if cancellation occurs, the process continues to the +link{userTask.cancelElement, cancelElement}
//      if specified. Otherwise the workflow is immediately finished.
// <li> if completion occurs, values are retrieved from the form or valuesManager and applied
//      to the process state based on +link{task.outputField,outputField},
//      +link{task.outputFieldList,outputFieldList} or +link{task.inputField,inputField}, in that order.
// </ul>
// @inheritsFrom Task
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<


isc.defineClass("UserTask", "Task");

isc.UserTask.addProperties({
    //> @attr userTask.targetView (Canvas | String : null : IR)
    // Widget that should be shown to allow user input.  If this widget is a DynamicForm,
    // it will also be automatically used as the +link{targetForm} unless either
    // <code>targetForm</code> or +link{targetVM} is set.
    // <P>
    // <code>UserTask</code> will automatically handle various scenarios of the
    // <code>targetView</code> not currently visible or draw()n, according to the following
    // rules:
    // <ul>
    // <li> if the view itself is marked hidden, it will be show()n
    // <li> if the view is inside a hidden parent, the parent will be show()n
    // <li> if the view is the +link{tab.pane} of a tab in a TabSet, the tab will be selected
    // <li> if the view is listed in +link{SectionStackSection.items} for a which is either
    //      collapsed or hidden section, the section will be shown and expanded
    // <li> if the view is listed in +link{Window.items} for a Window, the Window will be shown
    // <li> if any of these conditions apply to any parent of the targetView, the rules will be
    //      applied to that parent as well.  For example, the targetView is in a collapsed section
    //      inside a tab which is not selected, the section will be expanded <b>and</b> the tab
    //      selected
    // </ul>
    // @see inlineView
    // @visibility workflow
    //<

    //> @attr userTask.inlineView (Canvas : null: IRW)
    // An inline definition of the form. Can be used in place of +link{targetView} to encode form
    // directly in process xml.
    // @visibility workflow
    //<

    //> @attr userTask.targetForm (DynamicForm | String : null : IR)
    // DynamicForm that should be populated with data and that should provide the data for the task
    // outputs. If +link{targetView} is a DynamicForm and would also be the targetForm, the
    // targetForm attribute can be left unset.
    // <P>
    // Use +link{targetVM} to use a +link{ValuesManager} instead.
    // @visibility workflow
    //<

    //> @attr userTask.targetVM (ValuesManager | String : null : IR)
    // Optional ValuesManager which will receive task inputs and provide task outputs.
    // <p>
    // Use +link{targetForm} instead if you want to use a DynamicForm.
    // @visibility workflow
    //<

    //> @attr userTask.saveToServer (Boolean : false : IR)
    // If saveToServer is set then the associated form (+link{targetForm}) will perform the normal
    // +link{DynamicForm.submit()} actions when submitted (typically from a +link{SubmitItem}).
    // By default the form submit action is bypassed.
    // @visibility workflow
    //<

    //> @attr userTask.wizard (Boolean : false : IR)
    // If wizard is set then associated form will be hidden after user goes to next or prev
    // step of current workflow.
    // @visibility workflow
    //<

    //> @attr userTask.passThruOutput (Boolean : false : IR)
    // @include processElement.passThruOutput
    //<
    passThruOutput: false,

    //> @attr userTask.cancelElement (String : null : IR)
    // Next element to proceed to if the task is cancelled because the +link{targetForm} or
    // +link{targetVM} had <code>cancelEditing()</code> called on it.
    // <p>
    // if no value is provided the workflow immediately completes.
    // @visibility workflow
    //<

    //> @attr userTask.previousElement (String : null : IR)
    // Previous workflow +link{process.sequences,sequence} or +link{process.elements,element}
    // that is helpful for wizards. This element will be executed if +link{goToPrevious()}
    // method of userTask will be invoked. You can get userTask for attached form by using
    // +link{DynamicForm.userTask, userTask} property.
    // @visibility workflow
    //<

    //> @method userTask.goToPrevious() ([])
    // Set +link{previousElement} as next element of workflow. This method could be used to
    // create wizard-like UI behavior.
    // @visibility workflow
    //<
    goToPrevious : function () {
        if (this.previousElement == null) {
            isc.logWarn("PreviousElement is not set - unable to accomplish goToPrevious method.");
            return;
        }
        this._process.setNextElement(this.previousElement);
        this.completeEditing();
    },

    //> @method userTask.cancelEditing() ([])
    // Revert any changes made in a form and finish this userTask execution.
    // +link{cancelElement} will be proceed as the next element of current process.
    // @visibility workflow
    //<
    cancelEditing : function () {
        if (this._process) {
            if (this.wizard || this._process.wizard) {
                if (this.targetFormValue) {
                    this.targetFormValue.hide();
                } else if (this.targetViewValue) {
                    this.targetViewValue.hide();
                }
            }
            var process = this._process
            // should be deleted before continuing process execution to be able to invoke
            // userTask several times in workflow
            delete this._process;
            process.setNextElement(this.cancelElement);
            process.start();
        }
    },

    //> @method userTask.completeEditing() ([])
    // Finish editing and store edited values in +link{Process.state,process state}.
    // @visibility workflow
    //<
    completeEditing : function () {
        if (this._process) {
            var process = this._process;
            delete this._process;

            if (this.wizard || process.wizard) {
                if (this.targetFormValue) {
                    this.targetFormValue.hide();
                } else if (this.targetViewValue) {
                    this.targetViewValue.hide();
                }
            }
            var values;
            if (this.targetVMValue) {
                values = this.targetVMValue.getValues();
            } else if (this.targetFormValue) {
                values = this.targetFormValue.getValues();
            }
            if (values != null) process.setTaskOutput(this, values);

            if (this.outputField) {
                process.setStateVariable(this.outputField, values);
            } else if (this.outputFieldList) {
                for (var i = 0; i < this.outputFieldList.length; i++) {
                    var key = this.outputFieldList[i];
                    var ldi = key.lastIndexOf(".");
                    if (ldi > 0) {
                        key = key.substring(ldi + 1);
                    }
                    var value = values[key];
                    if (typeof value != 'undefined') {
                        process.setStateVariable(this.outputFieldList[i], value);
                    }
                }
            } else if (this.inputField) {
                process.setStateVariable(this.inputField, values);
            }
            this._writeOutputExpression(values);

            process.start();
        }
    },

    executeElement : function (process) {
        this._process = process;
        // convert from IDs to objects

        // View to show must be specified as either targetView or inlineView
        if (this.targetView && isc.isA.String(this.targetView)) {
            if (process.getStateVariable(this.targetView)) {
                this.targetViewValue = process.getStateVariable(this.targetView);
            } else {
                this.targetViewValue = window[this.targetView];
                if (this.targetViewValue == null && process.views) {
                    for (var i = 0; i < process.views.length; i++) {
                        if (process.views[i].ID == this.targetView) {
                            this.targetViewValue = isc[process.views[i]._constructor].create(process.views[i]);
                            if (this._process.containerId) {
                                window[this._process.containerId].addMember(this.targetViewValue);
                            }
                            break;
                        }
                    }
                }
                // check autoChildren
                if (this.targetViewValue == null) {
                    this.targetViewValue = this.addAutoChild(this.targetView);
                }
                if (this.targetViewValue == null) {
                    isc.logWarn("TargetView " + this.targetView + " was not found.");
                }
            }
        } else {
            if (this.targetView) {
                this.targetViewValue = this.targetView;
            } else if (this.inlineView){
                this.targetViewValue = isc[this.inlineView._constructor].create(this.inlineView);
                if (this._process.containerId) {
                    window[this._process.containerId].addMember(this.targetViewValue);
                }
            }
        }

        // targetVM or targetForm can be used to specify the target for values.
        // Useful when view is a composite.
        if (this.targetVM && isc.isA.String(this.targetVM)) {
            if (process.state[this.targetVM]) {
                this.targetVMValue = process.getStateVariable(this.targetVM);
            } else {
                this.targetVMValue = window[this.targetVM];
                if (this.targetVMValue == null) {
                    isc.logWarn("TargetVM " + this.targetVM + " was not found.");
                }
            }
        } else {
            this.targetVMValue = this.targetVM;
        }
        if (this.targetForm && isc.isA.String(this.targetForm)) {
            if (process.state[this.targetForm]) {
                this.targetFormValue = process.getStateVariable(this.targetForm);
            } else {
                this.targetFormValue = window[this.targetForm];
                if (this.targetFormValue == null) {
                    isc.logWarn("TargetForm " + this.targetForm + " was not found.");
                }
            }
        } else {
            this.targetFormValue = this.targetForm;
        }

        if (this.targetFormValue == null && isc.isA.DynamicForm(this.targetViewValue)) {
            this.targetFormValue = this.targetViewValue;
        }
        if (this.targetViewValue == null) {
            isc.logWarn("targetView, inlineView or targetForm should be set for UserTask");
            return true;
        }

        // Show targetView/inlineView
        this.targetViewValue.showRecursively();

        // Pull input values from process state
        var values = null;
        if (this.inputField) {
            if (this.inputField.startsWith("$")) {
                values = isc.clone(this._resolveInputField(this.inputField, process));
            } else {
                values = isc.clone(process.getStateVariable(this.inputField));
            }
        } else if (this.inputFieldList) {
            values = {};
            for (var i = 0; i < this.inputFieldList.length; i++) {
                var key = this.inputFieldList[i];
                var ldi = key.lastIndexOf(".");
                if (ldi > 0) {
                    key = key.substring(ldi + 1);
                }
                values[key] = isc.clone(process.getStateVariable(this.inputFieldList[i]));
            }
        }

        if (this.targetVMValue) {
            if (values) this.targetVMValue.setValues(values);
            this.targetVMValue.userTask = this;
        }
        if (this.targetFormValue) {
            if (values) this.targetFormValue.setValues(values);
            this.targetFormValue.saveToServer = (this.saveToServer == true);
            this.targetFormValue.userTask = this;
        }
        if (this.targetViewValue) {
            this.targetViewValue.userTask = this;
        }
        return false;
    },

    getElementDescription : function () {
        var showTarget = { type: "[nothing]" };

        if (this.targetView) {
            showTarget = { type: "targetView", ID: (isc.isA.String(this.targetView) ? this.targetView : null) };
        } else if (this.inlineView) {
            showTarget = { type: "inlineView" };
        }

        return "Show " + (showTarget.ID ? "'" + showTarget.ID + "' " : "") + showTarget.type + " and wait for input";
    },

    editorType: "UserTaskEditor"

});

//--------------------------------------------------------------------------------------------

//> @class StateTask
// StateTask can either copy fields of +link{process.state} to other fields, or apply hardcoded
// values to +link{process.state} via +link{stateTask.value}.
// <p>
// Some examples:
// <ul>
// <li>inputField: "a", outputField: "b" - copies "a" to "b"
// <li>inputField: "a", outputField: "b", type: "integer" - copies "a" to "b" converting "a" to an integer
// <li>inputFieldList: ["a","b"], outputField: ["c","d"] - copies "a" and "b" to "c" and "d" respectively.
// </ul>
// @inheritsFrom Task
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("StateTask", "Task");

//> @type ProcessValueType
// @value "string" values that are not already strings are converted via toString()
// @value "boolean" the strings "true" and "false" become boolean true and false.  All other
//                 Strings non-empty String values are true, all numbers are true except 0, and
//                 all other non-null values are true
// @value "decimal" values are converted via toString() and parsing as a decimal number.
//                 Invalid values trigger a transition to the +link{stateTask.failureElement}
// @value "integer" values are converted via toString(), parsing as a number, and rounding to
//                 nearest integer.  Invalid values trigger a transition to the
//                 +link{stateTask.failureElement}
// @value "record" any input which is not already a Record or Map becomes null
// @value "array" generic array type - will convert value to an array of the same type as the
//               existing value
// @visibility workflow
//<

isc.StateTask.addProperties({
    //> @attr stateTask.value (Any : null : IR)
    // If a stateTask does not declare +link{task.inputField,inputField}, it must declare a <code>value</code>
    // which should be assigned to the output field.
    // <p>
    // See +link{stateTask.type} for how the value is interpreted.
    // @visibility workflow
    //<

    //> @attr stateTask.type (ProcessValueType : null : IR)
    // Type of the value for stateTask.outputField.
    // <p>
    // This can be used in conjunction with +link{stateTask.value} to declare the type of the
    // value, or can be used to convert the type of the +link{task.inputField,inputField} to
    // the declared type.
    // <p>
    // If no type is declared, the value from an inputField is unchanged or provided via a call
    // to setValue() is unchanged.
    // <p>
    // A value specified for <code>stateTask.value</code> via an attribute in
    // +link{group:componentXML} (see +link{Process.loadProcess()}) is treated as a boolean if
    // it is the exact string "true" or "false", treated as a "decimal" or "integer" if it
    // parsable as a valid number, otherwise treated as a String.  If these heuristics don't
    // work in your case, just declare the type explicitly via <code>stateTask.type</code>.
    // <p>
    // A value of "record" type or "array" type can be declared in Component XML using the same
    // formats allowed for +link{DataSourceField.valueMap,valueMap}.  Each array value or
    // record attribute value undergoes the same heuristics as for +link{stateTask.value}
    // declared as an attribute.
    // <p>
    // +link{stateTask.type} is invalid to use with multiple outputFields.
    // @visibility workflow
    //<



    //> @attr stateTask.failureElement (String : null : IR)
    // ID of the next sequence or element to proceed to if a failure condition arises, such as
    // the output data not being convertible to the target +link{type}.
    // @visibility workflow
    //<

    //> @attr stateTask.outputExpression  (String : null : IR)
    // Not applicable to a StateTask.
    // @visibility workflow
    //<

    //> @attr stateTask.passThruOutput (Boolean : false : IR)
    // @include processElement.passThruOutput
    //<
    passThruOutput: false,

    executeElement : function (process) {
        if (this.value == null && this.inputField == null && this.inputFieldList == null) {
            isc.logWarn("StateTask: value, inputField or inputFieldList should be set.");
            return true;
        }
        if (this.value == null && this.inputField == null) {
            if (this.outputFieldList == null || this.outputFieldList.length != this.inputFieldList.length) {
                isc.logWarn("StateTask: outputFieldList should have same number of parameters as inputFieldList.");
                return;
            }
            if (this.type) {
                isc.logWarn("StateTask: type cannot be used with multiple outputFields");
            }
            for (var i = 0; i < this.inputFieldList.lenght; i++) {
                var value = process.getStateVariable(this.inputFieldList[i]);
                process.setStateVariable(this.outputFieldList[i], value);
            }
            return true;
        }
        var value = this.value || this._resolveInputField(this.inputField, process);
        value = this._executePair(value, this.type, process);
        process.setStateVariable(this.outputField, value);
        process.setTaskOutput(this, value);
        return true;
    },

    _executePair : function (value, type, process) {
        if (value == null) {
            isc.logWarn("StateTask: value is null. Unable to convert to " + type);
            this.fail(process);
            return null;
        }
        if ("string" == type) {
            // @value "string" values that are not already strings are converted via toString()
            return value.toString();
        } else if ("boolean" == type) {
            // @value "boolean" the strings "true" and "false" become boolean true and false.
            //        All other Strings non-empty String values are true, all numbers are true
            //        except 0, and all other non-null values are true
            if ("true" == value) return true;
            if ("false" == value) return false;
            if (isc.isA.String(value)) return value.length != 0;
            if (isc.isA.Number(value)) return value != 0;
            return value != null;
        } else if ("decimal" == type) {
            // @value "decimal" values are converted via toString() and parsing as a decimal
            // number.
            // Invalid values trigger a transition to the +link{stateTask.failureElement}
            var v = parseFloat(value.toString());
            if (isNaN(v)) {
                this.fail(process);
                return null;
            }
            return v;
        } else if ("integer" == type) {
            // @value "integer" values are converted via toString(), parsing as a number, and
            // rounding to nearest integer.  Invalid values trigger a transition to the
            // +link{stateTask.failureElement}
            var v = parseInt(value.toString());
            if (isNaN(v)) {
                this.fail(process);
                return null;
            }
            return v;
        } else if ("record" == type) {
            // @value "record" any input which is not already a Record or Map becomes null
            if (isc.isAn.Object(value) && !isc.isAn.Array(value) &&
                    !isc.isAn.RegularExpression(value) && !isc.isAn.Date(value))
            {
                return value;
            }
            return null;
        } else if ("array" == type) {
            // @value "array" generic array type - will convert value to an array of the same
            // type as the existing value
            if (isc.isAn.Array(value)) return value;
            return [value];
        } else {
            return value;
        }
    },

    fail : function (process) {
        if (this.failureElement == null) {
            isc.logInfo("There is no failureElement in stateTask");
        } else {
            process.setNextElement(this.failureElement);
        }
    },

    getElementDescription : function () {
        var description = "no-op";

        if (this.value != null) {
            // set <outputField> = <value>
            description = "Set " + this.outputField + "=" + this.value;
        } else if (this.type != null) {
            // set <outputField> = <inputField> as <type>
            description = "Set " + this.outputField + "=" + this.inputField + " as " + this.type;
        } else if (this.inputField || this.inputFieldList) {
            // copy <inputField>/<inputFieldList> to <outputField>/<outputFieldList>
            description = "Copy " + (this.inputField ? this.inputField : this.inputFieldList.join(",")) + " to " + (this.outputField ? this.outputField : this.outputFieldList.join(","));
        }

        return description;
    },

    editorType: "StateTaskEditor"

});

//--------------------------------------------------------------------------------------------

//> @class StartProcessTask
// Task that executes another +link{process} inside the current one. A process cannot be
// embedded within another process as a normal task element. Instead, a StartProcessTask
// is used to provide the input state, execute the inner process, then write the output
// back into the calling process state.
//
// @inheritsFrom ScriptTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("StartProcessTask", "ScriptTask");

isc.StartProcessTask.addProperties({
    //> @attr startProcessTask.process (Process : null : IR)
    // The +link{process} to be run by this task. Input state is created from
    // +link{inputFieldList} and container process state is updated from
    // the inner process state using +link{outputFieldList}.
    //
    // @visibility workflow
    //<

    //> @attr startProcessTask.isAsync (Boolean : true : [IRW])
    // Not applicable to StartProcessTask.
    // @visibility workflow
    //<
    isAsync: true,

    //> @method startProcessTask.execute()
    // Not applicable to StartProcessTask.
    // @visibility workflow
    //<

    execute : function (input, inputRecord) {
        if (!this.process) {
            this.logWarn("StartProcessTask with no process. Skipped");
            return;
        }
        var process = this.process,
            finished = process.finished,
            _this = this
        ;
        process.finished = function (state) {
            if (finished) finished (state);
            _this.setOutputRecord (state);
        }

        // make sure this setting is not overridden
        this.isAsync = true;

        process.setState(inputRecord);
        process.start();
    }
});

//--------------------------------------------------------------------------------------------

//> @class EndProcessTask
// Task that ends a workflow. This task is not necessary to end a workflow - having
// a task execute with no +link{processElement.nextElement} is sufficient to end
// the workflow.
// <p>
// This task is primarily used in the workflow editor to render a "no-op" task or as
// an explicit visual marker for the end of workflow.
//
// @inheritsFrom ProcessElement
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("EndProcessTask", "ProcessElement");

isc.EndProcessTask.addProperties({
    // render editor as an add element
    editorPlaceholder: true,

    executeElement : function (process) {
        // Nothing to do
    }
});

//--------------------------------------------------------------------------------------------

//> @type ShowMessageType
// Type of message to display in +link{ShowMessageTask}. Controls the display of the icon.
//
// @value "normal" Normal message
// @value "warning" Warning message
// @value "error" Error message
//
// @see showMessageTask.type
// @visibility workflow
//<

//> @class ShowMessageTask
// Show an informational message and wait for the user to acknowledge.
//
// @inheritsFrom ProcessElement
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("ShowMessageTask", "ProcessElement");

isc.ShowMessageTask.addProperties({
    classDescription: "Show a message in a modal dialog",
    editorType: "ShowMessageTaskEditor",

    //> @attr showMessageTask.type (ShowMessageType : "normal" : IR)
    // Message type.
    // @visibility workflow
    //<
    type: "normal",

    //> @attr showMessageTask.message (String : null : IR)
    // Message to display. To display a dynamic message see +link{textFormula}.
    // @visibility workflow
    //<

    //> @attr showMessageTask.textFormula (UserSummary : null : IR)
    // Formula to be used to calculate the message contents. Use +link{message} property
    // to assign a static message instead.
    // <p>
    // Available fields for use in the formula are the current +link{canvas.ruleScope,rule context}.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);
        var messageType = this.type,
            callback = function () { process.start() }
        ;

        var message = this.getTextFormulaValue(this.textFormula, process) ||
                      this.getDynamicValue(this.message, process);

        if (messageType == "normal") {
            isc.say(message, callback);
        } else if (messageType == "warning") {
            isc.warn(message, callback);
        } else if (messageType == "error") {
            isc.Dialog.create({
                message: message,
                icon: isc.Dialog.getInstanceProperty("errorIcon"),
                buttons : [
                    isc.Button.create({ title:"OK" })
                ],
                buttonClick : function (button, index) {
                    callback();
                    this.hide();
                    this.destroy();
                }
            });
        } else {
            // nothing to do
            return true;
        }
        // asynchronous op
        return false;
    },

    _typeDescriptionMap : {
        "normal":  "",
        "warning": "warning",
        "error":   "error"
    },

    getElementDescription : function () {
        var message = (this.textFormula ? this.textFormula.text : this.message) || "",
            messageParts = message.split(" "),
            shortMessage = messageParts.getRange(0, 3).join(" "),
            type = this.type || "message"
        ;
        if (shortMessage.length > 25) shortMessage = shortMessage.substring(0,25);
        if (shortMessage != message) shortMessage += " ...";

        return "Show " + this._typeDescriptionMap[type] + " message: <br>" + shortMessage;
    }
});

//--------------------------------------------------------------------------------------------

//> @class AskForValueTask
// Ask the user to input a value.
//
// @inheritsFrom UserConfirmationTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("AskForValueTask", "UserConfirmationTask");

isc.AskForValueTask.addProperties({
    typeTitle: "Ask for Value",
    classDescription: "Ask the user to input a value",
    editorType: "AskForValueTaskEditor",

    //> @attr askForValueTask.defaultValue (String : null : IR)
    // Default value.
    // @visibility workflow
    //<

    executeElement : function (process) {
        var properties = (this.defaultValue ? { defaultValue: this.defaultValue } : null);

        var message = this.getTextFormulaValue(this.textFormula, process) ||
                      this.getDynamicValue(this.message, process);

        var task = this;
        isc.askForValue(message, function (value) {
            if (value) {
                process.setTaskOutput(task, {
                    value: value,
                    canceled: false
                });
                if (task.nextElement) process.setNextElement(task.nextElement);
            } else {
                if (!task.failureElement) {
                    task.logInfo("Ask For Value Task does not have a failureElement. Process is aborting.");
                    // the call to setNextElement() below will cause the process to terminate automatically
                }
                process.setTaskOutput(task, {
                    canceled: true
                });

                if (task.failureElement != "next") {
                    process.setNextElement(task.failureElement);
                }
            }
            process.start();
        }, properties);

        // processing dialog asynchronously
        return false;
    },

    getOutputSchema : function () {
        if (!this._outputSchema) {
            var fields = [
                { name: "value", title: "Value" },
                { name: isc.Canvas._ruleScopeMetaFieldNamePrefix + "canceled", title: "[meta] canceled", type: "boolean", criteriaPath: "$last.canceled" }
            ];
            this._outputSchema = isc.DS.create({
                addGlobalId: false,
                clientOnly: true,
                fields: fields
            });
        }
        return this._outputSchema;
    },

    destroy : function () {
        if (this._outputSchema) this._outputSchema.destroy();
        this.Super("destroy", arguments);
    },

    getElementDescription : function () {
        var message = (this.textFormula ? this.textFormula.text : this.message) || "",
            messageParts = message.split(" "),
            shortMessage = messageParts.getRange(0, 3).join(" ")
        ;
        if (shortMessage.length > 25) shortMessage = shortMessage.substring(0,25);
        if (shortMessage != message) shortMessage += " ...";

        return "Ask for value: <br>" + shortMessage;
    }
});

//--------------------------------------------------------------------------------------------

//> @class ShowNotificationTask
// Show a message which fades out automatically using
// +link{class:Notify}.
//
// @inheritsFrom ProcessElement
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("ShowNotificationTask", "ProcessElement");

isc.ShowNotificationTask.addProperties({
    classDescription: "Show a message which fades out automatically",
    editorType: "ShowNotificationTaskEditor",

    //> @attr showNotificationTask.autoDismiss (Boolean : true : IR)
    // Auto-dismiss message after a short duration.
    // @visibility workflow
    //<
    autoDismiss: true,

    //> @attr showNotificationTask.message (String : null : IR)
    // Message to display. To display a dynamic message see +link{textFormula}.
    // @visibility workflow
    //<

    //> @attr showNotificationTask.textFormula (UserSummary : null : IR)
    // Formula to be used to calculate the message contents. Use +link{message} property
    // to assign a static message instead.
    // <p>
    // Available fields for use in the formula are the current +link{canvas.ruleScope,rule context}.
    //
    // @visibility workflow
    //<

    //> @attr showNotificationTask.position (String : "T" : IR)
    // Where to show the message, specified as an edge ("T", "B", "R", "L") similar to
    // +link{canvas.snapTo}, or "C" for center.  The message will be shown at the center of the
    // edge specified (or the very center for "C").
    // @visibility workflow
    //<
    position: "T",

    //> @attr showNotificationTask.notifyType (NotifyType : "message" : IR)
    // NotifyType for message.
    // @visibility workflow
    //<
    notifyType: "message",

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var notifyType = this.notifyType,
            settings = { position: this.position };
        if (!this.autoDismiss) {
            settings.duration = 0;
            settings.canDismiss = true;
        }

        var message = this.getTextFormulaValue(this.textFormula, process) ||
                      this.getDynamicValue(this.message, process);

        isc.Notify.addMessage(message, null, notifyType, settings);
        return true;
    },

    _notifyTypeDescriptionMap : {
        "message": "",
        "warn":    " warning",
        "error":   " error"
    },

    getElementDescription : function () {
        var message = (this.textFormula ? this.textFormula.text : this.message) || "",
            messageParts = message.split(" "),
            shortMessage = messageParts.getRange(0, 3).join(" "),
            notifyType = this.notifyType || "message"
        ;
        if (shortMessage.length > 25) shortMessage = shortMessage.substring(0,25);
        if (shortMessage != message) shortMessage += " ...";

        return "Show" + this._notifyTypeDescriptionMap[notifyType] + " notification: <br>" +
            shortMessage;
    }
});

//--------------------------------------------------------------------------------------------

//> @class StartTransactionTask
// Starts queuing all DataSource operations so they can be sent to the server all together
// as a transaction.
//
// @inheritsFrom ProcessElement
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("StartTransactionTask", "ProcessElement");

isc.StartTransactionTask.addProperties({
    classDescription: "Starts queuing all DataSource operations so they can be sent " +
        "to the server all together as a transaction",
    editorType: null,

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        isc.RPC.startQueue();

        return true;
    },

    getElementDescription : function () {
        return "Start queuing";
    }
});

//--------------------------------------------------------------------------------------------

//> @class SendTransactionTask
// Sends any currently queued DataSource operations, as a single transactional request to the server.
//
// @inheritsFrom ProcessElement
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("SendTransactionTask", "ProcessElement");

isc.SendTransactionTask.addProperties({
    classDescription: "Sends any currently queued DataSource operations " +
        "as a single transactional request to the server",
    editorType: null,

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        isc.RPC.sendQueue();

        return true;
    },

    getElementDescription : function () {
        return "Send queue";
    }
});

//--------------------------------------------------------------------------------------------

//> @class ComponentTask
// Base class for tasks that target <smartclient>SmartClient</smartclient><smartgwt>SmartGWT</smartgwt>
// UI-specific operations.
// <P>
// Note: This task is not for direct use - use one of the subclasses instead.
//
// @inheritsFrom ProcessElement
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("ComponentTask", "ProcessElement");

isc.ComponentTask.addClassProperties({

    // By default an action converted to a ComponentTask maps the target component
    // to the componentId property. A subclass can map additional properties by
    // providing a more specific actionMapping.
    actionMapping: {
        target: "componentId"
    },

    isApplicableComponent : function (component, className) {
        var clazz = (component && component.getClass ? component.getClass() : className && isc.ClassFactory.getClass(className));
        if (!clazz) return false;

        var baseClasses = this.getInstanceProperty("componentBaseClass"),
            requiresDataSource = this.getInstanceProperty("componentRequiresDataSource") || false
        ;
        baseClasses = (isc.isAn.Array(baseClasses) ? baseClasses : [baseClasses]);

        for (var i = 0; i < baseClasses.length; i++) {
            if (clazz.isA(baseClasses[i]) && (!requiresDataSource || component.dataSource)) {
                if (clazz.Class != "SectionHeader" || baseClasses[i] != "Canvas") {
                    return true;
                }
            }
        }
        return false;
    }
});

isc.ComponentTask.addProperties({
    //> @attr componentTask.componentId (GlobalId : null : IR)
    // ID of component targeted by this task.
    //
    // @visibility workflow
    //<


    //> @attr componentTask.componentBaseClass (String | Array of String : null : IR)
    // Base class of components that this task targets.
    //
    // @visibility workflow
    //<

    //> @attr componentTask.componentRequiresDataSource (boolean : null : IR)
    // Must target components of this task have a DataSource?
    //
    // @visibility workflow
    //<

    getComponentBaseClasses : function () {
        return (isc.isAn.Array(this.componentBaseClass) ? this.componentBaseClass : [this.componentBaseClass]);
    },

    //> @method componentTask.getTargetComponent()
    // Returns the actual component specified by +link{componentId} for this task. Unless
    // <code>skipValidation</code> is true, the component type is validated against
    // +link{componentBaseClass} and only returned if it matches. Null is returned otherwise.
    //
    // @param process (Process) the process that is handling the workflow
    // @param [skipValidation] (Boolean) skip validation against componentBaseClass?
    // @visibility workflow
    //<
    getTargetComponent : function (process, skipValidation) {
        var componentId = this.componentId,
            component
        ;
        if (!componentId) {
            this.logWarn("ComponentTask with no componentId. Task skipped");
            return null;
        }

        // Support providing an actual component instead of an ID
        if (isc.isA.Class(componentId)) return this.componentId;

        // Support a non-relative locator as a componentId
        if (componentId.startsWith("scLocator=//") || componentId.startsWith("//")) {
            component = isc.AutoTest.getObject(componentId);
            if (!component) {
                this.logWarn("Component not found for locator " + componentId + ". Task skipped");
                return null;
            }
        } else {

            if (process && process.screenComponent) {
                component = process.screenComponent.getByLocalId(componentId);
            }
            if (!component) {
                component = window[componentId];
                if (!component) {
                    this.logWarn("Component not found for ID " + componentId + ". Task skipped");
                    return null;
                }
            }
            if (isc.isA.ClassObject(component)) {
                this.logWarn("Component ID " + componentId + " resolved to a class object rather than a component instance. Task skipped");
                return null;
            }
        }

        if (skipValidation) return component;

        var baseClasses = this.getComponentBaseClasses();
        for (var i = 0; i < baseClasses.length; i++) {
            if (component.isA(baseClasses[i])) return component;
        }
        this.logWarn("Component type '" + component.getClassName() + "' is not supported for " + this.getClassName() + ". Task skipped");
        return null;
    },

    getLocalComponent : function (process, componentId) {
        if (!componentId) return null;

        // Support providing an actual component instead of an ID
        if (isc.isA.Class(componentId)) return componentId;

        var component = window[componentId];
        if (!component) {

            if (process && process.screenComponent) {
                component = process.screenComponent.getByLocalId(componentId);
            }
            if (!component) return null;
        }
        return component;
    },

    updateGlobalIDReferences : function (oldId, newId) {
        var changed = this.Super("updateGlobalIDReferences", arguments);

        if (this.componentId && this.componentId == oldId) {
            this.componentId = newId;
            changed = true;
        }
        return changed;
    }
});

//> @class SetTitleTask
// Sets the title of a tab, section, window, label, button or form control.
// <P>
// For a label, button or window the +link{setTitleTask.componentId,componentId}
// specifies everything necessary to identify the target.
// <P>
// For a tab, section or form control more information is needed. The
// +link{setTitleTask.componentId,componentId} identifies the container
// (i.e. TabSet, SectionStack or DynamicForm) and the individual component is specified as:
// <ul>
// <li>Tab - +link{setTitleTask.targetTabName,targetTabName} references the +link{tab.name}.</li>
// <li>Section - +link{setTitleTask.targetSectionName,targetSectionName} for
//               +link{sectionStackSection.name} or +link{setTitleTask.targetSectionTitle,targetSectionTitle}
//               for +link{sectionStackSection.title}.</li>
// <li>FormItem - +link{setTitleTask.targetFieldName,targetFieldName} for +link{formItem.name}.</li>
// </ul>
//
// @see tabSet.setTabTitle
// @see sectionStack.setSectionTitle
// @see window.setTitle
// @see label.setContents
// @see button.setTitle
// @see formItem.title
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("SetTitleTask", "ComponentTask");

isc.SetTitleTask.addProperties({
    componentBaseClass: ["Tab", "SectionStackSection", "Window", "Label", "Button", "FormItem"],

    classDescription: "Sets the title of a tab, section, window, label or form control",
    editorType: "SetTitleTaskEditor",

    //> @attr setTitleTask.title (HTMLString : null : IR)
    // Title to assign to component. To assign a dynamic value see +link{textFormula}.
    //
    // @visibility workflow
    //<

    //> @attr setTitleTask.textFormula (UserSummary : null : IR)
    // Formula to be used to calculate the component title. Use +link{title} property
    // to assign a static title instead.
    // <p>
    // Available fields for use in the formula are the current +link{canvas.ruleScope,rule context}.
    //
    // @visibility workflow
    //<

    //> @attr setTitleTask.targetTabName (TabName | GlobalId : null : IR)
    // If +link{componentTask.componentId,componentId} targets a TabSet, this property
    // specifies the name or ID of the target tab to assign new title.
    //
    // @visibility workflow
    //<

    //> @attr setTitleTask.targetFieldName (FieldName : null : IR)
    // If +link{componentTask.componentId,componentId} targets a DynamicForm, this property
    // specifies the name of the target field to assign new title.
    //
    // @visibility workflow
    //<

    //> @attr setTitleTask.targetSectionName (String : null : IR)
    // If +link{componentTask.componentId,componentId} targets a SectionStack, this property
    // specifies the name of the target section. Alternately, the target section can be
    // specified by using +link{targetSectionTitle}.
    //
    // @visibility workflow
    //<

    //> @attr setTitleTask.targetSectionTitle (String : null : IR)
    // If +link{componentTask.componentId,componentId} targets a SectionStack, this property
    // specifies the title of the target section. Alternately, the target section can be
    // specified by using +link{targetSectionName}.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var component = this.getTargetComponent(process, true);
        if (!component) return true;

        var title = this.getTextFormulaValue(this.textFormula, process) ||
                    this.getDynamicValue(this.title, process);

        if (isc.isA.Label(component)) {
            component.setContents(title);

        } else if (isc.isA.SectionStack(component)) {
            var sectionName = this.targetSectionName;
            if (!sectionName && this.targetSectionTitle) {
                var sectionNames = component.getSectionNames();
                for (var i = 0; i < sectionNames.length; i++) {
                    var sectionHeader = component.getSectionHeader(sectionNames[i]);
                    if (sectionHeader && sectionHeader.title == this.targetSectionTitle) {
                        sectionName = sectionNames[i];
                        break;
                    }
                }
            }

            if (!sectionName) {
                isc.logWarn("Target section not identified by targetSectionName or targetSectionTitle. Task skipped");
                return true;
            }

            component.setSectionTitle(sectionName, title);

        } else if (isc.isA.DynamicForm(component)) {
            var fieldName = this.targetFieldName;
            if (!fieldName) {
                isc.logWarn("Target field not identified by targetFieldName. Task skipped");
                return true;
            }

            var field = component.getField(fieldName);
            if (!field) {
                isc.logWarn("Target field, " + fieldName + ", not found. Task skipped");
                return true;
            }

            // A ButtonItem (CanvasItem) has a setTitle method
            if (field.setTitle) {
                field.setTitle(title);
            } else {
                field.title = title;
                field.redraw("title change");
            }

        } else if (isc.isA.TabSet(component)) {
            var tabName = this.targetTabName;
            if (!tabName) {
                isc.logWarn("Target tab not identified by targetTabName. Task skipped");
                return true;
            }

            var tab = component.getTab(tabName);
            if (!tab) {
                isc.logWarn("Target tab, " + tabName + ", not found. Task skipped");
                return true;
            }

            component.setTabTitle(tab, title);

        } else if (component.setTitle) {
            // Handle other canvas, button and window titles
            component.setTitle(title);
        }

        return true;
    },

    getElementDescription : function () {
        var target = this.componentId,
            sectionName = this.targetSectionName || this.targetSectionTitle,
            tabName = this.targetTabName,
            fieldName = this.targetFieldName
        ;
        if (sectionName) {
            target += ", section " + sectionName;
        } else if (tabName) {
            target += ", " + (tabName.toLowerCase().endsWith("tab") ? "" : "tab ") + tabName;
        } else if (fieldName) {
            target += ", field " + fieldName;
        }
        return "Set " + (target ? "'" + target + "'" : "") + "title";
    }
});

//> @class ShowHideTask
// Show or hide a component. When showing, reveals any hidden parents as well.
//
// @see canvas.show
// @see canvas.hide
// @see formItem.show
// @see formItem.hide
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("ShowHideTask", "ComponentTask");

isc.ShowHideTask.addProperties({
    componentBaseClass: ["Canvas","Tab","SectionHeader"],

    typeTitle: "Show / Hide",
    classDescription: "Show or hide a component. When showing, reveals any hidden parents as well.",
    editorType: "ShowHideTaskEditor",

    //> @attr showHideTask.targetTabName (TabName | GlobalId : null : IR)
    // If +link{componentTask.componentId,componentId} targets a TabSet, this property
    // specifies the name or ID of the target tab to assign new title.
    //
    // @visibility workflow
    //<

    //> @attr showHideTask.targetFieldName (FieldName : null : IR)
    // If +link{componentTask.componentId,componentId} targets a DynamicForm, this property
    // specifies the name of the target field to assign new title.
    //
    // @visibility workflow
    //<

    //> @attr showHideTask.targetSectionName (String : null : IR)
    // If +link{componentTask.componentId,componentId} targets a SectionStack, this property
    // specifies the name of the target section. Alternately, the target section can be
    // specified by using +link{targetSectionTitle}.
    //
    // @visibility workflow
    //<

    //> @attr showHideTask.targetSectionTitle (String : null : IR)
    // If +link{componentTask.componentId,componentId} targets a SectionStack, this property
    // specifies the title of the target section. Alternately, the target section can be
    // specified by using +link{targetSectionName}.
    //
    // @visibility workflow
    //<

    //> @attr showHideTask.hide (Boolean : null : IR)
    // Should the target form item be hidden?
    //
    // @visibility workflow
    //<

    //> @attr showHideTask.showRecursively (Boolean : null : IR)
    // Set to <code>false</code> to not show a component's parents.
    //
    // @visibility workflow
    //<

    //> @attr showHideTask.scrollIntoView (Boolean : null : IR)
    // Set to <code>false</code> to prevent scrolling the component into view when showing.
    //
    // @visibility workflow
    //<

    //> @attr showHideTask.moveFocusToTarget (Boolean : null : IR)
    // Should focus be moved to target component when showing?
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var canvas = this.getTargetComponent(process);
        if (!canvas) return true;

        var hide = this.hide;
        if (isc.isA.String(hide)) hide = (hide == "true");

        if (isc.isA.SectionStack(canvas)) {
            var sectionName = this.targetSectionName;
            if (!sectionName && this.targetSectionTitle) {
                var sectionNames = canvas.getSectionNames();
                for (var i = 0; i < sectionNames.length; i++) {
                    var sectionHeader = canvas.getSectionHeader(sectionNames[i]);
                    if (sectionHeader && sectionHeader.title == this.targetSectionTitle) {
                        sectionName = sectionNames[i];
                        break;
                    }
                }
            }

            if (!sectionName) {
                isc.logWarn("Target section not identified by targetSectionName or targetSectionTitle. Task skipped");
                return true;
            }

            if (hide) {
                canvas.hideSection(sectionName);
            } else {
                if (this.showRecursively != false) {
                    canvas.showRecursively();
                }
                canvas.showSection(sectionName);
                var sectionHeader = canvas.getSectionHeader(sectionName);
                if (this.scrollIntoView != false) {
                    this._scrollIntoView(sectionHeader);
                }
                if (this.moveFocusToTarget) {
                    var sectionItems = sectionHeader.items;
                    if (sectionItems && sectionItems.length > 0) {
                        canvas = sectionItems[0];
                    } else {
                        canvas = sectionHeader;
                    }
                    if (canvas.focus) {
                        canvas.delayCall("focus");
                    } else {
                        this.logWarn("moveFocusToTarget set but target does not support focus. Ignored.");
                    }
                }
            }

        } else if (isc.isA.DynamicForm(canvas)) {
            var targetFieldName = this.targetFieldName;
            if (!targetFieldName) {
                isc.logWarn("Target field not identified by targetFieldName. Task skipped");
                return true;
            }

            if (!canvas.getField(targetFieldName)) {
                isc.logWarn("Target field, " + targetFieldName + ", not found. Task skipped");
                return true;
            }

            if (hide) {
                canvas.hideItem(targetFieldName);
            } else {
                if (this.showRecursively != false) {
                    canvas.showRecursively();
                }
                canvas.showItem(targetFieldName);
                if (this.scrollIntoView != false) {
                    this._scrollIntoView(canvas.getItem(targetFieldName));
                }
                if (this.moveFocusToTarget) {
                    canvas.delayCall("focusInItem", targetFieldName);
                }
            }

        } else if (isc.isA.TabSet(canvas)) {
            var tabName = this.targetTabName;
            if (!tabName) {
                isc.logWarn("Target tab not identified by targetTabName. Task skipped");
                return true;
            }

            var tab = canvas.getTab(tabName);
            if (!tab) {
                isc.logWarn("Target tab, " + tabName + ", not found. Task skipped");
                return true;
            }

            if (hide) {
                canvas.hideTab(tabName);
            } else {
                if (this.showRecursively != false) {
                    canvas.showRecursively();
                }
                canvas.showTab(tabName);
                canvas = canvas.getTabPane(tabName) || canvas;
                if (this.scrollIntoView != false) {
                    this._scrollIntoView(canvas);
                }
                if (this.moveFocusToTarget) {
                    if (canvas.focus) {
                        canvas.delayCall("focus");
                    } else {
                        this.logWarn("moveFocusToTarget set but target does not support focus. Ignored.");
                    }
                }
            }

        } else {
            // Handle generic canvas
            if (hide) {
                canvas.hide();
            } else {
                if (this.showRecursively != false) {
                    canvas.showRecursively();
                } else {
                    canvas.show();
                }
                if (this.scrollIntoView != false) {
                    this._scrollIntoView(canvas);
                }
                if (this.moveFocusToTarget) {
                    if (canvas.focus) {
                        canvas.delayCall("focus");
                    } else {
                        this.logWarn("moveFocusToTarget set but target does not support focus. Ignored.");
                    }
                }
            }
        }
        return true;
    },

    // Scroll target canvas into view within container if it is outside the viewport vertically
    _scrollIntoView : function (target) {
        var parent = target.eventTarget || target.parentElement;
        if (parent == null) return;

        if (target.getTop() + target.getVisibleHeight() > parent.getViewportHeight()) {
            parent.scrollTo(0, target.getTop());
        }
        this._scrollIntoView(parent);
    },

    getEditorType : function () {
        return this.targetFieldName ? "ShowHideFieldTaskEditor" : this.editorType;
    },

    getElementDescription : function () {
        var hide = this.hide;
        if (isc.isA.String(hide)) hide = (hide == "true");
        var action = (hide ? "Hide" : "Show"),
            target = this.targetFieldName ||
                     this.targetSectionName ||
                     this.targetSectionTitle ||
                     this.targetTabName
        ;
        return action + " '" + this.componentId + (target ? "." + target + "'" : "'");
    },

    // When serializing as HideTask the "hide" property should not be written
    getSerializeableFields : function () {
        var result = this.Super("getSerializeableFields", arguments);
        delete result.hide;
        return result;
    },

    // Change task type when serializing to ShowTask/HideTask for simplicity
    getCustomDefaults : function () {
        var _constructor = this.hide ? "HideTask" : "ShowTask";
        return { _constructor: _constructor };
    }
});

// Trivial classes for simple serialization
// See ShowHideTask.getCustomDefaults and ShowHideTask.getSerializableFields to see how these
// classes are used for serialization.
isc.defineClass("ShowTask", "ShowHideTask");

isc.defineClass("HideTask", "ShowHideTask");
isc.HideTask.addProperties({
    hide: true
});

//> @class FormSetValuesTask
// Set form values.
//
// @see dynamicForm.setValues
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("FormSetValuesTask", "ComponentTask");

isc.FormSetValuesTask.addProperties({
    componentBaseClass: [ "DynamicForm", "ValuesManager" ],
    componentRequiresDataSource: true,

    classDescription: "Set form values",
    editorType: "FormSetValuesTaskEditor",

    //> @attr formSetValuesTask.values (Record : null : IR)
    // Values to be set on the form.
    // <P>
    // Data values prefixed with "$" will be treated as a
    // +link{group:taskInputExpression}.  Use +link{fixedValues} for any values that start with
    // "$" but should be treated as a literal.
    // @visibility workflow
    //<

    //> @attr formSetValuesTask.fixedValues (Record : null : IR)
    // Values to be combined with the data from the +link{serviceTask.values} if specified,
    // via simple copying of fields, with explicitly specified +link{serviceTask.values}
    // overriding <code>fixedValues</code>.
    // @visibility workflow
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        this.process = process;
        var form = this.getTargetComponent(process);
        if (!form) return true;

        var data = null;
        if (this.values) {
            // Resolve any dynamicCriteria or taskInputExpressions. Returns a copy of values.
            data = this._resolveObjectDynamicExpressions(this.values, null, null, process);
        }
        if (this.fixedValues) {
            for (var key in this.fixedValues) {
                data[key] = this.fixedValues[key];
            }
        }

        form.setValues(data);
        return true;
    },

    getElementDescription : function () {
        return "Set '" + this.componentId + "' values";
    },

    getOutputSchema : function () {
        var form = this.getTargetComponent(this.process);
        if (!form) return null;

        return form.getDataSource();
    },

    updateLastElementBindingReferences : function (taskType) {
        var changed = this.Super("updateLastElementBindingReferences", arguments);
        changed = this.updateLastElementInValues(this.values, taskType) || changed;
        return changed;
    },

    updateGlobalIDReferences : function (oldId, newId) {
        var changed = this.Super("updateGlobalIDReferences", arguments);
        changed = this.updateGlobalIDInValues(this.values, oldId, newId) || changed;
        return changed;
    }
});

//> @class FormSetFieldValueTask
// Put a value in just one field of a form.
//
// @see dynamicForm.setValue
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("FormSetFieldValueTask", "ComponentTask");

isc.FormSetFieldValueTask.addProperties({
    componentBaseClass: [ "DynamicForm", "ValuesManager" ],
    componentRequiresDataSource: true,

    classDescription: "Put a value into just one field of a form",
    editorType: "FormSetFieldValueTaskEditor",

    //> @attr formSetFieldValueTask.targetField (FieldName : null : IR)
    // Field to assign new value.
    //
    // @visibility workflow
    //<

    //> @attr formSetFieldValueTask.value (Any : null : IR)
    // Value to assign to +link{targetField}.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var form = this.getTargetComponent(process);
        if (!form) return true;

        var value = this.getDynamicValue(this.value, process);

        form.setValue(this.targetField, value);
        return true;
    },

    getElementDescription : function () {
        var target = (this.targetField ? "." + this.targetField : " field")
        return "Set '" + this.componentId + target + " value";
    },

    updateLastElementBindingReferences : function (taskType) {
        var changed = this.Super("updateLastElementBindingReferences", arguments);
        changed = this.updateLastElementInValueProperty("value", taskType) || changed;
        return changed;
    },

    updateGlobalIDReferences : function (oldId, newId) {
        var changed = this.Super("updateGlobalIDReferences", arguments);
        changed = this.updateGlobalIDInValueProperty("value", oldId, newId) || changed;
        return changed;
    }
});

//> @class FormClearValuesTask
// Clear form values and errors.
//
// @see dynamicForm.clearValues
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("FormClearValuesTask", "ComponentTask");

isc.FormClearValuesTask.addProperties({
    componentBaseClass: [ "DynamicForm", "ValuesManager" ],

    classDescription: "Clear form values and errors",
    editorType: "FormClearValuesTaskEditor",

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var form = this.getTargetComponent(process);
        if (!form) return true;

        form.clearValues();
        return true;
    },

    getElementDescription : function () {
        return "Clear '" + this.componentId + "' values";
    }
});

//> @class FormResetValuesTask
// Revert unsaved changes in a form.
//
// @see dynamicForm.reset
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("FormResetValuesTask", "ComponentTask");

isc.FormResetValuesTask.addProperties({
    componentBaseClass: [ "DynamicForm", "ValuesManager" ],

    classDescription: "Reset values in a form to defaults",
    editorType: "FormResetValuesTaskEditor",

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var form = this.getTargetComponent(process);
        if (!form) return true;

        form.resetValues();
        return true;
    },

    getElementDescription : function () {
        return "Reset '" + this.componentId + "' values";
    }
});

//> @class FormValidateValuesTask
// Validate a form and show errors to user.
//
// @see dynamicForm.validate
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("FormValidateValuesTask", "ComponentTask");

isc.FormValidateValuesTask.addProperties({
    componentBaseClass: [ "DynamicForm", "ValuesManager" ],

    classDescription: "Validate a form and show errors to user",
    editorType: "FormValidateValuesTaskEditor",

    //> @attr formValidateValuesTask.passThruOutput (Boolean : false : IR)
    // @include processElement.passThruOutput
    //<
    passThruOutput: false,

    executeElement : function (process) {
        this.process = process;
        var task = this,
            form = this.getTargetComponent(process)
        ;
        if (!form) return true;

        // form.validate() will trigger a server validation and asynchronous callback
        // when complete if there are any server validators. Hook the callback for the form
        // replacing the existing callback, if present. It will be restored when complete.
        var origHandleAsyncValidationReply = form.handleAsyncValidationReply;
        form.addMethods({
            handleAsyncValidationReply : function (success, errors, context) {
                form.handleAsyncValidationReply = origHandleAsyncValidationReply;
                // call original callback, if present
                if (form.handleAsyncValidationReply) {
                    form.handleAsyncValidationReply(success, errors, context);
                }

                // Set task output with response and restart the workflow
                var response = {
                    status: (success == false
                             ? isc.RPCResponse.STATUS_VALIDATION_ERROR
                             : isc.RPCResponse.STATUS_SUCCESS),
                    errors: form.getErrors()
                };
                process.setTaskOutput(task, task.createOutput(response));
                process.start();
            }
        });

        // Validate client validators and trigger server validations if necessary
        var validClientData = form.validate(),
            expectServerResponse = form.isPendingAsyncValidation()
        ;

        // If no server response is expected, restore the original callback method now
        // and set the task output now.
        if (!expectServerResponse) {
            form.handleAsyncValidationReply = origHandleAsyncValidationReply;

            // Create a mockup response so createOutput() can be used for client validation too
            var response = {
                status: (validClientData == false
                         ? isc.RPCResponse.STATUS_VALIDATION_ERROR
                         : isc.RPCResponse.STATUS_SUCCESS),
                errors: form.getErrors()
            };
            process.setTaskOutput(this, this.createOutput(response));
        } else {
            // output will be set when server response is processed
        }
        return !expectServerResponse;
    },

    createOutput : function (response) {
        var output = { valuesValid: true };
        if (response.status == isc.RPCResponse.STATUS_VALIDATION_ERROR) {
            output.valuesValid = false;
            output.errors = isc.DS.getSimpleErrors(response);
        }
        return output;
    },

    getOutputSchema : function () {
        if (!this._outputSchema) {
            var form = this.getTargetComponent(this.process);
            if (form) {
                var fields = [
                    { name: isc.Canvas._ruleScopeMetaFieldNamePrefix + "valuesValid", title: "[meta] valuesValid", type: "boolean", criteriaPath: "valuesValid" }
                ];
                var formDS = form.getDataSource();
                if (formDS) {
                    var fieldNames = formDS.getFieldNames();
                    for (var i = 0; i < fieldNames.length; i++) {
                        var fieldName = fieldNames[i],
                            field = { name: fieldName, type: "text", multiple: true }
                        ;
                        fields.add(field);
                    }
                }
                this._outputSchema = isc.DS.create({
                    addGlobalId: false,
                    clientOnly: true,
                    criteriaBasePath: "errors",
                    fields: fields
                });
            }
        }
        return this._outputSchema;
    },

    destroy : function () {
        if (this._outputSchema) this._outputSchema.destroy();
        this.Super("destroy", arguments);
    },

    getElementDescription : function () {
        return "Validate '" + this.componentId + "' values";
    }
});

//> @class FormSaveDataTask
// Saves changes made in a form (validates first).
//
// @see dynamicForm.saveData
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("FormSaveDataTask", "ComponentTask");

isc.FormSaveDataTask.addProperties({
    componentBaseClass: [ "DynamicForm", "ValuesManager" ],

    classDescription: "Save changes made in a form (validates first)",
    editorType: "FormSaveDataTaskEditor",

    //> @attr formSaveDataTask.showNotification (Boolean : null : IR)
    // Should a notification message (defined as notifyMessage) be shown after save completes
    // successfully?
    //
    // @visibility workflow
    //<

    //> @attr formSaveDataTask.notifyMessage (String : null : IR)
    // The message to be shown when save completes if +link{showNotification} is set. If
    // no message is configured a default message is used based on the whether the form
    // is bound to a DataSource or not: +link{boundNotifyMessage} or +link{unboundNotifyMessage}.
    //
    // @visibility workflow
    //<

    //> @attr formSaveDataTask.boundNotifyMessage (String : "${dsTitle} saved successfully" : IR)
    // The default message to be shown when a target form is bound to a DataSource.
    // @group i18nMessages
    // @visibility workflow
    //<
    boundNotifyMessage: "${dsTitle} saved successfully",

    //> @attr formSaveDataTask.unboundNotifyMessage (String : "Saved successfully" : IR)
    // The default message to be shown when a target form is not bound to a DataSource.
    // @group i18nMessages
    // @visibility workflow
    //<
    unboundNotifyMessage: "Saved successfully",

    //> @attr formSaveDataTask.notifyPosition (String : "T" : IR)
    // Where to show the message, specified as an edge ("T", "B", "R", "L") similar to
    // +link{canvas.snapTo}, or "C" for center.  The message will be shown at the center of the
    // edge specified (or the very center for "C").
    // @visibility workflow
    //<
    notifyPosition: "T",

    //> @attr formSaveDataTask.notifyType (NotifyType : "message" : IR)
    // NotifyType for +link{notifyMessage}.
    // @visibility workflow
    //<
    notifyType: "message",

    //> @attr formSaveDataTask.clearAfterSave (Boolean : true : IR)
    // The form will be +link{dynamicForm.clearValues,cleared} after a successful save unless
    // this property is set to <code>false</code>.
    //
    // @visibility workflow
    //<

    //> @attr formSaveDataTask.failureElement (String : null : IR)
    // ID of the next sequence or element to proceed to if a failure condition arises
    // from operation.
    // @visibility workflow
    //<

    //> @attr formSaveDataTask.requestProperties (DSRequest : null : IR)
    // Additional properties to set on the DSRequest that will be issued
    // to perform save.
    // <p>
    // Note that <code>willHandleError</code> will always be set <code>true</code>.
    //
    // @visibility workflow
    //<

    //> @attr formSaveDataTask.passThruOutput (Boolean : false : IR)
    // @include processElement.passThruOutput
    //<
    passThruOutput: false,

    executeElement : function (process) {
        this.process = process;
        var form = this.getTargetComponent(process);
        if (!form) return true;

        var params = isc.addProperties({}, this.requestProperties, { willHandleError: true });

        var task = this;
        form.saveData(function (dsResponse, data, request) {
            dsResponse = dsResponse || {};
            var results = dsResponse.results;
            if (dsResponse.isStructured &&
                (!results || results.status < 0 || (results.status == null && dsResponse.status < 0)))
            {
                if (!isc.RPC.runDefaultErrorHandling(dsResponse, request, task.errorFormatter)) {
                    process.setTaskOutput(task, task.createFailureOutput(dsResponse));
                    task.fail(process);
                    return;
                }
            } else {
                process.setTaskOutput(task, dsResponse.data);
            }
            // Clear form
            if (task.clearAfterSave || task.clearAfterSave == null) {
                form.clearValues();
            }
            // Show successful save notification
            if (task.showNotification) {
                var message = task.notifyMessage;
                if (!message) {
                    message = task.getNotifyMessage();
                }

                var notifyType = task.notifyType,
                    settings = { position: task.notifyPosition }
                ;
                isc.Notify.addMessage(message, null, notifyType, settings);
            }
            // Continue workflow
            process.start();
        }, params);

        if (form.hasErrors()) {
            process.setTaskOutput(task, {
                valuesValid: false,
                errors: form.getErrors()
            });

            if (this.failureElement != "next") {
                process.setNextElement(this.failureElement);
            }
            // Task is complete
            return true;
        }

        // Task is not complete - wait for server response
        return false;
    },

    fail : function (process) {
        if (!this.failureElement) {
            this.logInfo("FormSaveDataTask does not have a failureElement. Process is aborting.");
            // the call to setNextElement() below will cause the process to terminate automatically
        }
        if (this.failureElement == "next") {
            process.start();
        } else {
            process.setNextElement(this.failureElement);
        }
    },

    // "this" is not available
    errorFormatter : function (codeName, response, request) {
        if (codeName == "VALIDATION_ERROR") {
            var errors = response.errors,
                message = ["Server returned validation errors:<BR><UL>"]
            ;
            if (!isc.isAn.Array(errors)) errors = [errors];
            for (var i = 0; i < errors.length; i++) {
                var error = errors[i];
                for (var field in error) {
                    var fieldErrors = error[field];
                    message.add("<LI><B>" + field + ":</B> ");
                    if (!isc.isAn.Array(fieldErrors)) fieldErrors = [fieldErrors];
                    for (var j = 0; j < fieldErrors.length; j++) {
                        var fieldError = fieldErrors[j];
                        message.add((j > 0 ? "<BR>" : "") + (isc.isAn.Object(fieldError) ? fieldError.errorMessage : fieldError));
                    }
                    message.add("</LI>");
                }
            }
            message.add("</UL>");
            return message.join("");
        }
        return null;
    },

    createFailureOutput : function (response) {
        var output = { valuesValid: true };
        if (response.status == isc.RPCResponse.STATUS_VALIDATION_ERROR) {
            output.valuesValid = false;

            var form = this.getTargetComponent(this.process);
            if (form) {
                output.errors = isc.DS.getSimpleErrors(response);
            }
        }
        return output;
    },

    getOutputSchema : function () {
        var form = this.getTargetComponent(this.process);
        if (!form) return null;

        return form.getDataSource();
    },

    getFailureSchema : function () {
        if (!this._failureSchema) {
            var form = this.getTargetComponent(this.process);
            if (form) {
                var fields = [
                    { name: isc.Canvas._ruleScopeMetaFieldNamePrefix + "valuesValid", title: "[meta] valuesValid", type: "boolean", criteriaPath: "valuesValid" }
                ];
                var formDS = form.getDataSource();
                if (formDS) {
                    var fieldNames = formDS.getFieldNames();
                    for (var i = 0; i < fieldNames.length; i++) {
                        var fieldName = fieldNames[i],
                            field = { name: fieldName, type: "text", multiple: true }
                        ;
                        fields.add(field);
                    }
                }
                this._failureSchema = isc.DS.create({
                    addGlobalId: false,
                    clientOnly: true,
                    criteriaBasePath: "errors",
                    fields: fields
                });
            }
        }
        return this._failureSchema;
    },

    destroy : function () {
        if (this._failureSchema) this._failureSchema.destroy();
        this.Super("destroy", arguments);
    },

    getNotifyMessage : function () {
        var ds = this.getOutputSchema(),
            defaultMessageProperty = (ds != null ? "boundNotifyMessage" : "unboundNotifyMessage"),
            message = this[defaultMessageProperty]
        ;
        if (ds && message) {
            var dsTitle = ds.getTitle();
            if (dsTitle) {
                message = message.evalDynamicString(this, {
                    dsTitle: dsTitle
                });
            }
        }
        return message;
    },

    getElementDescription : function () {
        return "Save '" + this.componentId + "' data";
    }
});

//> @class FormEditNewRecordTask
// Start editing a new record.
//
// @see dynamicForm.editNewRecord
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("FormEditNewRecordTask", "ComponentTask");

isc.FormEditNewRecordTask.addClassProperties({
    actionMapping: {
        target: "componentId",
        // positional mapping of editNewRecord parameters to task properties
        paramMap: ["initialValues"]
    }
})

isc.FormEditNewRecordTask.addProperties({
    componentBaseClass: [ "DynamicForm", "ValuesManager" ],

    classDescription: "Start editing a new record",
    editorType: "FormEditNewRecordTaskEditor",

    //> @attr formEditNewRecordTask.initialValues (Record : null : IR)
    // Initial values for new edit record.
    // <p>
    // Data values prefixed with "$" will be treated as a +link{group:taskInputExpression}
    // excluding "$input" and "$inputRecord" references.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var form = this.getTargetComponent(process);
        if (!form) return true;

        var values;
        if (this.initialValues) {
            // Resolve any dynamicCriteria or taskInputExpressions. Returns a copy of initialValues.
            values = this._resolveObjectDynamicExpressions(this.initialValues, null, null, process);
        }

        form.editNewRecord(values);
        return true;
    },

    getElementDescription : function () {
        return "Edit '" + this.componentId + "' new record";
    },

    updateLastElementBindingReferences : function (taskType) {
        var changed = this.Super("updateLastElementBindingReferences", arguments);
        changed = this.updateLastElementInValues(this.initialValues, taskType) || changed;
        return changed;
    },

    updateGlobalIDReferences : function (oldId, newId) {
        var changed = this.Super("updateGlobalIDReferences", arguments);
        changed = this.updateGlobalIDInValues(this.initialValues, oldId, newId) || changed;
        return changed;
    }
});

//> @class FormEditRecordTask
// Edit a record currently showing in some other component. The source
// record is obtained as follows:
// <ul>
// <li> for a ListGrid: the first selected record or, if none is selected, the first record
// <li> for a DynamicForm: the form values
// <li> for a DetailViewer: the first record
// </ul>
//
// @see dynamicForm.editRecord
// @inheritsFrom FormEditNewRecordTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("FormEditRecordTask", "FormEditNewRecordTask");

isc.FormEditRecordTask.addProperties({
    componentBaseClass: [ "DynamicForm", "ValuesManager" ],
    componentRequiresDataSource: true,

    editorType: "FormEditRecordTaskEditor",

    //> @attr FormEditRecordTask.recordSourceComponent (GlobalId : null : IR)
    // Component to pull record for editing.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var form = this.getTargetComponent(process);
        if (!form) return true;

        var recordSourceComponentId = this.recordSourceComponent;
        if (!recordSourceComponentId) {
            this.logWarn("recordSourceComponent not specified on task. Task skipped.");
            return true;
        }
        var recordSourceComponent = this.getLocalComponent(process, recordSourceComponentId);
        if (!recordSourceComponent) {
            this.logWarn("recordSourceComponent '" + recordSourceComponentId + "' not found. Task skipped.");
            return true;
        }

        var values = this.initialValues;
        if (isc.isA.ListGrid(recordSourceComponent) && recordSourceComponent.anySelected()) {
            values = recordSourceComponent.getSelectedRecord();
        } else if (isc.isA.DynamicForm(recordSourceComponent)) {
            values = recordSourceComponent.getValues();
        } else if (isc.isA.ListGrid(recordSourceComponent)) {
            values = recordSourceComponent.getRecord(0);
        } else if (isc.isA.DetailViewer(recordSourceComponent)) {
            values = recordSourceComponent.data[0];
        }

        form.editRecord(values);
        return true;
    },

    getElementDescription : function () {
        var target = (this.recordSourceComponent ? "'" + this.recordSourceComponent + "'" : "other");
        return "Edit '" + this.componentId + "' from " + target + " record";
    },

    updateGlobalIDReferences : function (oldId, newId) {
        var changed = this.Super("updateGlobalIDReferences", arguments);

        if (this.recordSourceComponent && this.recordSourceComponent == oldId) {
            this.recordSourceComponent = newId;
            changed = true;
        }
        return changed;
    }
});

//> @class FormEditSelectedTask
// Edit a record currently selected in some other component. If nothing is selected
// a new record is edited.
//
// @see dynamicForm.editRecord
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("FormEditSelectedTask", "ComponentTask");

isc.FormEditSelectedTask.addClassProperties({
    actionMapping: {
        target: "componentId",
        source: "selectionComponentId"
    }
})

isc.FormEditSelectedTask.addProperties({
    componentBaseClass: [ "DynamicForm", "ValuesManager" ],
    componentRequiresDataSource: true,

    typeTitle: "Edit Selected Record",
    classDescription: "Edit a record currently showing in some other component",
    editorType: "FormEditSelectedTaskEditor",

    //> @attr formEditSelectedTask.selectionComponentId (GlobalId : null : IR)
    // Component to pull record for editing.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var form = this.getTargetComponent(process);
        if (!form) return true;

        var selectionComponentId = this.selectionComponentId;
        if (!selectionComponentId) {
            this.logWarn("selectionComponentId not specified on task. Task skipped.");
            return true;
        }
        var selectionComponent = this.getLocalComponent(process, selectionComponentId);
        if (!selectionComponent) {
            this.logWarn("selectionComponent '" + selectionComponentId + "' not found. Task skipped.");
            return true;
        }


        var values = null;
        if (selectionComponent.getSelectedRecord) {
            values = selectionComponent.getSelectedRecord();
        }

        form.editRecord(values);
        return true;
    },

    getElementDescription : function () {
        var selected = (this.selectionComponentId ? "'" + this.selectionComponentId + "' selected" : "selected");
        return "Edit '" + this.componentId + "' from " + selected + " record";
    },

    updateGlobalIDReferences : function (oldId, newId) {
        var changed = this.Super("updateGlobalIDReferences", arguments);

        if (this.selectionComponentId && this.selectionComponentId == oldId) {
            this.selectionComponentId = newId;
            changed = true;
        }
        return changed;
    }
});

//> @class FormDisableFieldTask
// Disable or enable a form field.
//
// @see formItem.setDisabled
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("FormDisableFieldTask", "ComponentTask");

isc.FormDisableFieldTask.addProperties({
    componentBaseClass: "DynamicForm",

    typeTitle: "Enable / Disable Field",
    classDescription: "Enable or disable a field of a form",
    editorType: "FormDisableFieldTaskEditor",

    //> @attr formDisableFieldTask.targetField (FieldName : null : IR)
    // Field to show/hide.
    //
    // @visibility workflow
    //<

    //> @attr formDisableFieldTask.disable (Boolean : null : IR)
    // Should the target form item be disabled?
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var form = this.getTargetComponent(process);
        if (!form) return true;

        var targetField = this.targetField;
        if (!targetField) {
            this.logWarn("targetField not specified on task. Task skipped.");
            return true;
        }

        var disable = this.disable;
        if (isc.isA.String(disable)) disable = (disable == "true");

        var field = form.getField(targetField);
        if (field) {
            field.setDisabled(disable);
        }

        return true;
    },

    getElementDescription : function () {
        var disable = this.disable;
        if (isc.isA.String(disable)) disable = (disable == "true");
        var action = (disable ? "Disable" : "Enable"),
            targetField = (this.targetField ? "." + this.targetField : "")
        ;
        return action + " '" + this.componentId + targetField + "'";
    }
});

//> @class GridFetchDataTask
// Fetch data matching specified criteria into grid.
//
// @see listGrid.fetchData
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("GridFetchDataTask", "ComponentTask");

isc.GridFetchDataTask.addProperties({
    componentBaseClass: ["ListGrid", "TileGrid", "DetailViewer", "FacetChart"],
    componentRequiresDataSource: true,

    classDescription: "Fetch data matching specified criteria or change the criteria currently applied",
    editorType: "GridFetchDataTaskEditor",

    //> @attr gridFetchDataTask.criteria (Criteria : null : IR)
    // Criteria to use for fetch. If criteria is <code>null</code> or empty, the filter
    // criteria of the grid will be cleared.
    //
    // @visibility workflow
    //<

    //> @attr gridFetchDataTask.applyToImplicitCriteria (Boolean : null : IR)
    // Should criteria be applied to the grid as
    // +link{ListGrid.implicitCriteria,implicit criteria}? When criteria is applied this way,
    // it is not shown to the user and cannot be changed by the user.
    //
    // @visibility workflow
    //<

    //> @attr gridFetchDataTask.requestProperties (DSRequest : null : IR)
    // Additional properties to set on the DSRequest that will be issued
    // to perform the fetch or clear criteria.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        this.process = process;
        var grid = this.getTargetComponent(process);
        if (!grid) return true;

        // A null criteria or an empty one is used to clear the filter on the grid
        var criteria = this.criteria;
        if (criteria && this._isEmptyCriteria(criteria)) {
            criteria = null;
        }
        if (criteria) {
            criteria = this._resolveCriteriaExpressions(criteria, process.state, process.state, process);
        }

        var task = this,
            continueSynchronously = false
        ;
        if (this.applyToImplicitCriteria) {
            // Don't invalidate the cache when setting the criteria. Issue a manual
            // refresh afterwards to guarantee an asynchronous response.
            grid.setImplicitCriteria(criteria, false);
            grid.refreshData(function (dsResponse) {
                process.setTaskOutput(task, task._getOutput(grid, criteria, dsResponse.data));

                process.start();
            });
        } else {
            continueSynchronously = !grid.willFetchData(criteria);
            grid.fetchData(criteria, function (dsResponse) {
                process.setTaskOutput(task, task._getOutput(grid, criteria, dsResponse.data));

                process.start();
            }, this.requestProperties);
        }
        return continueSynchronously;
    },

    _getOutput : function (grid, criteria, data) {
        // Return multiple or first record based on same criteria as DSFetchData (aka ServiceTask)
        if (!data || data.length == 0) {
            return null;
        }
        var ds = grid.getDataSource(),
            output = data[0]
        ;
        if (ds) {
            output = data;
            if (criteria) {
                if (ds.isAdvancedCriteria(criteria)) {
                    var primaryKey = ds.getPrimaryKeyFieldName(),
                        criterion = ds.getFieldCriterion(criteria, primaryKey)
                    ;
                    if (criterion && criterion.operator == "equals") {
                        output = data[0];
                    }
                } else if (ds.defaultTextMatchStyle == "equals" && criteria[primaryKey] != null) {
                    output = data[0];
                }
            }
        }
        return output;
    },

    getOutputSchema : function () {
        var grid = this.getTargetComponent(this.process);
        if (!grid) return null;
        var ds = grid.dataSource;
        if (ds && (ds.getClassName == null || ds.getClassName() != "DataSource")) {
            ds = isc.DataSource.get(ds);
        }
        return ds;
    },

    getElementDescription : function () {
        var criteria = this.criteria,
            emptyCriteria = this._isEmptyCriteria(criteria),
            description = (emptyCriteria ? "Clear criteria" : "Fetch data") +
                            " on '" + this.componentId + "'"
        ;

        if (!emptyCriteria) {
            if (!isc.DS.isAdvancedCriteria(criteria)) {
                criteria = isc.DS.convertCriteria(criteria);
            }
            var dsFields = isc.DecisionTask._processFieldsRecursively(criteria);
            // construct datasource for fields used in criteria
            var fieldsDS = isc.DataSource.create({
                addGlobalId: false,
                fields: dsFields
            });

            description += " where <ul>" + isc.DataSource.getAdvancedCriteriaDescription(criteria, fieldsDS, null, {prefix: "<li>", suffix: "</li>"}) + "</ul>";
            fieldsDS.destroy();
        }

        return description;
    },

    _isEmptyCriteria : function (criteria) {
        return (!criteria ||
                // Empty simple or advanced criteria
                isc.isAn.emptyObject(criteria) ||
                // Empty advanced criteria
                ((!criteria.fieldName && !criteria.criteria) ||
                    (!criteria.fieldName && isc.isAn.Array(criteria.criteria) &&
                        criteria.criteria.length == 0)));
    },

    updateLastElementBindingReferences : function (taskType) {
        var changed = this.Super("updateLastElementBindingReferences", arguments);
        changed = this.updateLastElementInCriteria(this.criteria, taskType) || changed;
        return changed;
    },

    updateGlobalIDReferences : function (oldId, newId) {
        var changed = this.Super("updateGlobalIDReferences", arguments);
        changed = this.updateGlobalIDInCriteria(this.criteria, oldId, newId) || changed;
        return changed;
    }
});

//> @class FetchRelatedDataTask
// Fetch data related to a record in another component.
//
// @see listGrid.fetchRelatedData
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("FetchRelatedDataTask", "ComponentTask");

isc.FetchRelatedDataTask.addClassProperties({
    actionMapping: {
        target: "componentId",
        source: "recordSourceComponent"
    }
})

isc.FetchRelatedDataTask.addProperties({
    componentBaseClass: ["DynamicForm", "DetailViewer", "ListGrid", "TileGrid", "DetailViewer"],
    componentRequiresDataSource: true,

    classDescription: "Fetch data related to a record shown in another component",
    editorType: "FetchRelatedDataTaskEditor",

    //> @attr fetchRelatedDataTask.recordSourceComponent (GlobalId : null : IR)
    // Component to pull record for locating related data.
    //
    // @visibility workflow
    //<

    //> @attr fetchRelatedDataTask.dataSource (DataSource | ID : null : IR)
    // The DataSource used with +link{recordSourceComponent} to pull related data.
    // If not specified, +link{recordSourceComponent} will be used to obtain the schema.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        var grid = this.getTargetComponent(process);
        if (!grid) return true;

        var recordSourceComponentId = this.recordSourceComponent;
        if (!recordSourceComponentId) {
            this.logWarn("recordSourceComponent not specified on task. Task skipped.");
            return true;
        }
        var recordSourceComponent = this.getLocalComponent(process, recordSourceComponentId);
        if (!recordSourceComponent) {
            this.logWarn("recordSourceComponent '" + recordSourceComponentId + "' not found. Task skipped.");
            return true;
        }

        var schema = this.dataSource || recordSourceComponent;

        var record = null,
            sourceIsGrid = isc.isA.ListGrid(recordSourceComponent) || isc.isA.TileGrid(recordSourceComponent)
        ;
        if (sourceIsGrid && recordSourceComponent.anySelected()) {
            record = recordSourceComponent.getSelectedRecord();
        } else if (isc.isA.DynamicForm(recordSourceComponent)) {
            record = recordSourceComponent.getValues();
        } else if (sourceIsGrid) {
            record = recordSourceComponent.getRecord(0);
        } else if (isc.isA.DetailViewer(recordSourceComponent)) {
            record = recordSourceComponent.data[0];
        }
        // If no selected record is found, there is nothing to do
        if (!record) return true;

        var task = this;
        // fetchRelatedData() is synchronous if filtering can be done on the client
        var willFetchData = grid.fetchRelatedData(record, schema, function (dsResponse) {
            var firstRecord = (dsResponse.data && dsResponse.data.length > 0 ? dsResponse.data[0] : null);
            process.setTaskOutput(task, firstRecord);

            process.start();
        }, null, true);
        if (!willFetchData) {
            var firstRecord = grid.getRecord(0);
            process.setTaskOutput(task, firstRecord);
        }
        // If fetching data pause workflow until callback above restarts it (async task)
        return !willFetchData;
    },

    getOutputSchema : function () {
        var grid = this.getTargetComponent(this.process);
        if (!grid) return null;
        var ds = grid.dataSource;
        if (ds && (ds.getClassName == null || ds.getClassName() != "DataSource")) {
            ds = isc.DataSource.get(ds);
        }
        return ds;
    },

    getElementDescription : function () {
        var target = (this.recordSourceComponent ? "in '" + this.recordSourceComponent + "'" : "");
        return "Fetch '" + this.componentId +
               "' data from related record " + target;
    },

    updateGlobalIDReferences : function (oldId, newId) {
        var changed = this.Super("updateGlobalIDReferences", arguments);

        if (this.recordSourceComponent && this.recordSourceComponent == oldId) {
            this.recordSourceComponent = newId;
            changed = true;
        }
        return changed;
    }
});

//> @class GridRemoveSelectedDataTask
// Remove data that is selected in a grid.
//
// @see listGrid.removeSelectedData
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("GridRemoveSelectedDataTask", "ComponentTask");

isc.GridRemoveSelectedDataTask.addProperties({
    componentBaseClass: ["ListGrid", "TileGrid"],

    classDescription: "Remove data that is selected in a grid",
    editorType: "GridRemoveSelectedDataTaskEditor",

    //> @attr gridRemoveSelectedDataTask.allowMultiRecordRemove (Boolean : null : IR)
    // Can multiple records be removed? If not and multiple records are selected,
    // the +link{refuseMultiRemoveMessage} is shown as a warning and the workflow is
    // terminated.
    // <p>
    // Default is true unless set to <code>false</code>.
    //
    // @visibility workflow
    //<

    //> @attr gridRemoveSelectedDataTask.refuseMultiRemoveMessage (String :  "Only one record may be removed at a time" : IR)
    // Message to be shown when multiple records are selected for removal but
    // +link{allowMultiRecordRemove} is not set.
    //
    // @group i18nMessages
    // @visibility workflow
    //<
    refuseMultiRemoveMessage: "Only one record may be removed at a time",

    //> @attr gridRemoveSelectedDataTask.showConfirmation (Boolean : null : IR)
    // Should we show a confirmation message before removal and allow the user to cancel the
    // operation? If user cancels the removal the workflow is terminated.
    // <p>
    // Message to be shown is defined in the properties: +link{confirmationMessage},
    // +link{unboundConfirmationMessage}, +link{multipleConfirmationMessage} and
    // +link{unboundMultipleConfirmationMessage}. The appropriate message is chosen
    // based on the grid configuration and the number of records being removed.
    //
    // @visibility workflow
    //<

    //> @attr gridRemoveSelectedDataTask.confirmationMessage (String : "Delete ${dsTitle} ${recordTitle}" : IR)
    // Message shown to user to confirm deleting a single record on a bound grid.
    // <P>
    // This is a dynamic string - text within <code>&#36;{...}</code> are dynamic variables and will
    // be evaluated as JS code when the message is displayed.
    // <P>
    // Only three dynamic variables, dsTitle; dsPluralTitle and recordTitle, are available and
    // represent the bound +link{dataSource.title} and +link{dataSource.pluralTitle} and
    // the value of the +link{dataSource.titleField} from the first selected record.
    //
    // @group i18nMessages
    // @visibility workflow
    //<
    confirmationMessage: "Delete ${dsTitle} ${recordTitle}?",

    //> @attr gridRemoveSelectedDataTask.unboundConfirmationMessage (String : "Delete record?" : IR)
    // Message shwon to user to confirm deleting a single record on an unbound grid.
    // <P>
    // This is a dynamic string - text within <code>&#36;{...}</code> are dynamic variables and will
    // be evaluated as JS code when the message is displayed.
    // <P>
    // Only one dynamic variable, recordTitle, is available and
    // represents the value of the +link{dataSource.titleField} from the first selected record.
    //
    // @group i18nMessages
    // @visibility workflow
    //<
    unboundConfirmationMessage: "Delete record?",

    //> @attr gridRemoveSelectedDataTask.multipleConfirmationMessage (String : "Delete multiple ${dsPluralTitle}?" : IR)
    // Message shown to user to confirm deleting a group of records on a bound grid.
    // <P>
    // This is a dynamic string - text within <code>&#36;{...}</code> are dynamic variables and will
    // be evaluated as JS code when the message is displayed.
    // <P>
    // Only two dynamic variables, dsTitle and dsPluralTitle, are available and
    // represent the bound +link{dataSource.title} and +link{dataSource.pluralTitle} .
    //
    // @group i18nMessages
    // @visibility workflow
    //<
    multipleConfirmationMessage: "Delete multiple ${dsPluralTitle}?",

    //> @attr gridRemoveSelectedDataTask.unboundMultipleConfirmationMessage (String : "Delete multiple records?" : IR)
    // Message shown to user to confirm deleting a group of records on an unbound grid.
    // <P>
    // This is a dynamic string - text within <code>&#36;{...}</code> are dynamic variables and will
    // be evaluated as JS code when the message is displayed.
    // <P>
    // Only one dynamic variable, recordTitle, is available and
    // represents the value of the +link{dataSource.titleField} from the first selected record.
    //
    // @group i18nMessages
    // @visibility workflow
    //<
    unboundMultipleConfirmationMessage: "Delete multiple records?",

    //> @attr gridRemoveSelectedDataTask.failureElement (String : null : IR)
    // ID of the next sequence or element to proceed to if a failure condition arises
    // from operation.
    // @visibility workflow
    //<

    //> @attr gridRemoveSelectedDataTask.requestProperties (DSRequest : null : IR)
    // Additional properties to set on the +link{listGrid.removeSelectedData,removeSelectedData}
    // call to perform removal.
    // <p>
    // Note that <code>willHandleError</code> will always be set <code>true</code>.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        this._process = process;
        process.passThruTaskOutput(this);

        var grid = this.getTargetComponent(process);
        if (!grid) return true;

        // If the grid has no selection, removeSelectedData() will not fire its callback
        // and there is nothing to do anyway. Just move on to next task.
        var selectionLength = grid.getSelection().length;
        if (selectionLength <= 0) return true;

        var multiRecordSelection = (selectionLength > 1);

        if (multiRecordSelection && this.allowMultiRecordRemove == false) {
            isc.warn(
                this.refuseMultiRemoveMessage,
                function () {
                    // abort the workflow
                    process.setNextElement(null);
                }
            );
            return;
        }

        if (this.showConfirmation) {
            var message = this.getMessage(grid),
                task = this
            ;
            isc.ask(
                message,
                function (value) {
                    if (value) {
                        // user confirmed removal; continue
                        task.removeSelectedData(process, grid);
                        return;
                    }
                    // abort the workflow
                    process.setNextElement(null);
                },
                {
                    buttons: [isc.Dialog.NO, isc.Dialog.YES],
                    autoFocusButton: 1
                }
            );
        } else {
            this.removeSelectedData(process, grid);
        }

        // not done - wait for dialog or remove to complete
        return false;
    },

    getMessage : function (grid) {
        var ds = grid.getDataSource(),
            selectionLength = grid.getSelection().length,
            multiRecordSelection = (selectionLength > 1),
            selectedRecord = grid.getSelectedRecord(),
            messageProperty = (ds != null ?
                (multiRecordSelection ? "multipleConfirmationMessage" : "confirmationMessage") :
                (multiRecordSelection ? "unboundMultipleConfirmationMessage" : "unboundConfirmationMessage")),
            message = this[messageProperty],
            context = {}
        ;
        if (ds) {
            context.dsTitle = ds.getTitle();
            context.dsPluralTitle = ds.getPluralTitle();
        }
        if (selectedRecord) {
            context.recordTitle = selectedRecord[ds.getTitleField()];
        }
        return message.evalDynamicString(this, context);
    },

    removeSelectedData : function (process, grid) {
        var params = isc.addProperties({}, this.requestProperties, { willHandleError: true }),
            task = this
        ;
        grid.removeSelectedData(null, params, function (responses) {
            // responses can be null when there is no DS on the grid or saveLocally:true
            if (responses && responses.length > 0) {
                for (var i = 0; i < responses.length; i++) {
                    var response = responses[i];
                    if (response.status < 0) {
                        task.fail(process);
                        return;
                    }
                }
            }
            process.start();
        });
    },

    fail : function (process) {
        if (!this.failureElement) {
            this.logInfo("GridRemoveSelectedDataTask does not have a failureElement. Process is aborting.");
            // the call to setNextElement() below will cause the process to terminate automatically
        }
        if (this.failureElement == "next") {
            process.start();
        } else {
            process.setNextElement(this.failureElement);
        }
    },

    getDefaultRefuseMessage : function () {
        return this.getClass().getInstanceProperty("refuseMultiRemoveMessage");
    },

    getDefaultMessage : function () {
        var grid = this.getTargetComponent(this._process),
            ds = grid && grid.getDataSource(),
            messageProperty = (ds != null ? "confirmationMessage" : "unboundConfirmationMessage"),
            message = this.getClass().getInstanceProperty(messageProperty),
            context = {}
        ;
        if (ds) {
            context.dsTitle = ds.getTitle();
            context.dsPluralTitle = ds.getPluralTitle();
        }
        return message.evalDynamicString(this, context);
    },

    getDefaultMultipleMessage : function () {
        var grid = this.getTargetComponent(this._process),
            ds = grid && grid.getDataSource(),
            messageProperty = (ds != null ? "multipleConfirmationMessage" : "unboundMultipleConfirmationMessage"),
            message = this.getClass().getInstanceProperty(messageProperty),
            context = {}
        ;
        if (ds) {
            context.dsTitle = ds.getTitle();
            context.dsPluralTitle = ds.getPluralTitle();
        }
        return message.evalDynamicString(this, context);
    },

    getElementDescription : function () {
        return "Remove '" + this.componentId + "' selected records";
    },

    // When serializing, if the messages have not been changed from the generated
    // ones, suppress them
    getSerializeableFields : function () {
        var result = this.Super("getSerializeableFields", arguments);
        if (result.allowMultiRecordRemove ||
            (result.refuseMultiRemoveMessage && result.refuseMultiRemoveMessage == this.getDefaultRefuseMessage()))
        {
            delete result.refuseMultiRemoveMessage;
        }

        var grid = this.getTargetComponent(this._process),
            ds = grid && grid.getDataSource()
        ;

        if (ds) {
            if (!result.showConfirmation ||
                (result.confirmationMessage && result.confirmationMessage == this.getDefaultMessage()))
            {
                delete result.confirmationMessage;
            }
            if (!result.showConfirmation ||
                (result.multipleConfirmationMessage && result.multipleConfirmationMessage == this.getDefaultMultipleMessage()))
            {
                delete result.multipleConfirmationMessage;
            }
            delete result.unboundConfirmationMessage;
            delete result.unboundMultipleConfirmationMessage;
        } else {
            delete result.confirmationMessage;
            delete result.multipleConfirmationMessage;
            if (!result.showConfirmation ||
                (result.unboundConfirmationMessage && result.unboundConfirmationMessage == this.getDefaultMessage()))
            {
                delete result.unboundConfirmationMessage;
            }
            if (!result.showConfirmation ||
                (result.unboundMultipleConfirmationMessage && result.unboundMultipleConfirmationMessage == this.getDefaultMultipleMessage()))
            {
                delete result.unboundMultipleConfirmationMessage;
            }
        }
        return result;
    }
});

//> @class GridViewSelectedDataTask
// View a record currently selected in some other component. Ignored if nothing is selected.
//
// @see detailViewer.viewSelectedData
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("GridViewSelectedDataTask", "ComponentTask");

isc.GridViewSelectedDataTask.addClassProperties({
    actionMapping: {
        target: "componentId",
        source: "selectionComponentId"
    }
})

isc.GridViewSelectedDataTask.addProperties({
    componentBaseClass: [ "DetailViewer", "DynamicForm", "TileGrid" ],

    typeTitle: "View Selected Data",
    classDescription: "View data already loaded in another grid, showing more fields",
    editorType: "GridViewSelectedDataTaskEditor",

    //> @attr gridViewSelectedDataTask.selectionComponentId (GlobalId : null : IR)
    // Component to pull record for viewing.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var grid = this.getTargetComponent(process);
        if (!grid) return true;

        var selectionComponentId = this.selectionComponentId;
        if (!selectionComponentId) {
            this.logWarn("selectionComponentId not specified on task. Task skipped.");
            return true;
        }
        var selectionComponent = this.getLocalComponent(process, selectionComponentId);
        if (!selectionComponent) {
            this.logWarn("selectionComponent '" + selectionComponentId + "' not found. Task skipped.");
            return true;
        }

        // just in case...
        if (selectionComponent.getSelection) {
            grid.viewSelectedData(selectionComponent);
        }
        return true;
    },

    getElementDescription : function () {
        var target = (this.selectionComponentId ? "'" + this.selectionComponentId + "' " : "");
        return "View " + target + "selected data in '" + this.componentId + "'";
    },

    updateGlobalIDReferences : function (oldId, newId) {
        var changed = this.Super("updateGlobalIDReferences", arguments);

        if (this.selectionComponentId && this.selectionComponentId == oldId) {
            this.selectionComponentId = newId;
            changed = true;
        }
        return changed;
    }
});

//> @class GridEditRecordTask
// Edit an existing record in a grid or start editing a new one. A new record is added
// unless +link{gridEditRecordTask.criteria,criteria} is specified. Alternatively, if
// +link{gridEditRecordTask.editFirstRecord,editFirstRecord} is specified, the first
// record is edited.
// <p>
// If criteria is provided and the criteria matches more than one record, the first matched
// record is edited. Additionally, if the record to be edited is not visible, the record will
// be scrolled into view.
// <p>
// Note that the record to be matched must already be loaded in the grid - no fetch will be
// performed.
//
// @see listGrid.editExistingRecord
// @see listGrid.startEditingNew
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("GridEditRecordTask", "ComponentTask");

isc.GridEditRecordTask.addClassProperties({
    actionMapping: {
        target: "componentId",
        // positional mapping of startEditingNew parameters to task properties
        paramMap: ["initialValues"]
    }
})

isc.GridEditRecordTask.addProperties({
    componentBaseClass: "ListGrid",

    classDescription: "Edit an existing record in a grid or start editing a new one",
    editorType: "GridEditRecordTaskEditor",

    supportsMultipleInputRecords: true,

    //> @attr gridEditRecordTask.initialValues (Record : null : IR)
    // Initial values for a new edit record.
    // <p>
    // Data values prefixed with "$" will be treated as a +link{group:taskInputExpression}
    // excluding "$input" and "$inputRecord" references.
    //
    // @visibility workflow
    //<

    //> @attr gridEditRecordTask.editFirstRecord (Boolean : null : IR)
    // When neither +link{initialValues} nor +link{criteria} are provided should the
    // first record in the grid be edited? If not set, a new record is added.
    //
    // @visibility workflow
    //<

    //> @attr gridEditRecordTask.criteria (Criteria : null : IR)
    // Criteria (including AdvancedCriteria) used to locate the record to be edited.
    // If criteria matches more than one record, the first record is edited.
    // <P>
    // Data values in this criteria prefixed with "$" will be treated as dynamic expressions
    // which can access the inputs to this task as $input - see
    // +link{group:taskInputExpression}.  Specifically, this means that for simple criteria,
    // any property value that is a String and is prefixed with "$" will be assumed to be an
    // expression, and for AdvancedCriteria, the same treatment will be applied to
    // +link{criterion.value}.
    // <p>
    // This property supports +link{group:dynamicCriteria} - use +link{criterion.valuePath}
    // to refer to values in the +link{Process.ruleScope}.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        // Save initial forceSingle state for later completeElement()
        if (this._forceSingle == null) {
            this._forceSingle = this.forceSingle;
        }
        var grid = this.getTargetComponent(process);
        if (!grid) return true;

        if (this.criteria || this.editFirstRecord) {
            // A null criteria or an empty one is used to clear the filter on the grid
            var criteria = this.criteria && this._resolveCriteriaExpressions(this.criteria, process.state, process.state, process);
            grid.editExistingRecord(criteria);
            this.forceSingle = true;
        } else {
            var values;
            if (this.initialValues) {
                // Resolve any dynamicCriteria or taskInputExpressions. Returns a copy of initialValues.
                values = this._resolveObjectDynamicExpressions(this.initialValues, null, null, process);
            }
            grid.startEditingNew(values);

            if (!values || !this.objectReferencesLastTaskOutput(this.initialValues, process)) {
                this.forceSingle = true;
            }
        }
        return true;
    },

    completeElement : function (process) {
        // forceSingle can be set during execution. Reset the value to its initial state
        // in case this task is executed again
        this.forceSingle = this._forceSingle;
        delete this._forceSingle;
    },

    getElementDescription : function () {
        var criteria = this.criteria,
            editFirstRecord = this.editFirstRecord,
            description = "Edit " +
                            (criteria ? "existing" : (editFirstRecord ? "first" : "new")) +
                            " record";
        ;
        if (criteria) {
            if (!isc.DS.isAdvancedCriteria(criteria)) {
                criteria = isc.DS.convertCriteria(criteria);
            }
            var dsFields = isc.DecisionTask._processFieldsRecursively(criteria);
            // construct datasource for fields used in criteria
            var fieldsDS = isc.DataSource.create({
                addGlobalId: false,
                fields: dsFields
            });

            description += " where <ul>" +
                isc.DataSource.getAdvancedCriteriaDescription(criteria, fieldsDS, null,
                                                {prefix: "<li>", suffix: "</li>"}) + "</ul>";
            fieldsDS.destroy();
        }

        return description;
    },

    updateLastElementBindingReferences : function (taskType) {
        var changed = this.Super("updateLastElementBindingReferences", arguments);
        changed = this.updateLastElementInValues(this.initialValues, taskType) || changed;
        changed = this.updateLastElementInCriteria(this.criteria, taskType) || changed;
        return changed;
    },

    updateGlobalIDReferences : function (oldId, newId) {
        var changed = this.Super("updateGlobalIDReferences", arguments);
        changed = this.updateGlobalIDInValues(this.initialValues, oldId, newId) || changed;
        changed = this.updateGlobalIDInCriteria(this.criteria, oldId, newId) || changed;
        return changed;
    }
});

//> @class GridSetEditValueTask
// Sets the edit value of a given field. The targeted row
// is determined in the following order:
// <ol>
// <li> the current edit row
// <li> the first selected row
// <li> the first row in the grid
// <li> a new edit row
// </ol>
//
// @see listGrid.setEditValue
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("GridSetEditValueTask", "ComponentTask");

isc.GridSetEditValueTask.addProperties({
    componentBaseClass: "ListGrid",
    componentRequiresDataSource: true,

    classDescription: "Set a value in an editable grid as if the user had made the edit",
    editorType: "GridSetEditValueTaskEditor",

    //> @attr gridSetEditValueTask.targetField (Number | String : null : IR)
    // Target field in current edit row to be updated
    //
    // @visibility workflow
    //<

    //> @attr gridSetEditValueTask.value (Any : null : IR)
    // Value to assign to +link{targetField}.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var grid = this.getTargetComponent(process);
        if (!grid) return true;

        if (this.targetField == null) {
            this.logWarn("targetField not specified on task. Task skipped.");
            return true;
        }
        var colNum = grid.getFieldNum(this.targetField);
        if (colNum < 0) {
            this.logWarn("targetField not found to target grid. Task skipped.");
            return true;
        }

        var editRow = grid.getEditRow(),
            rowNum = editRow,
            recordNum = process.getStateVariable("eventParams.recordNum")
        ;
        if (recordNum != null) {
            rowNum = recordNum;
        }
        if (rowNum == null) {
            var selectedRecord = grid.getSelectedRecord();
            if (selectedRecord) {
                rowNum = grid.getRecordIndex(selectedRecord);
            }
        }
        if (rowNum == null) {
            if (grid.getRecord(0) != null) {
                rowNum = 0;
            }
        }

        var value = this.getDynamicValue(this.value, process);

        if (rowNum >= 0) {
            if (rowNum != editRow) {
                grid.startEditing(rowNum, colNum);
            }
            if (rowNum >= 0) {

                var editItem = grid.getEditFormItem(colNum);
                if (editItem) editItem.clearValue();
                grid.setEditValue(rowNum, colNum, value);
            }
        } else {
            var initialValues = {};
            initialValues[this.targetField] = value;
            grid.startEditingNew(initialValues);
        }
        return true;
    },

    getElementDescription : function () {
        var targetField = (this.targetField ? "." + this.targetField : "");
        return "Set '" + this.componentId + targetField + "' edit value";
    },

    updateLastElementBindingReferences : function (taskType) {
        var changed = this.Super("updateLastElementBindingReferences", arguments);
        changed = this.updateLastElementInValueProperty("value", taskType) || changed;
        return changed;
    },

    updateGlobalIDReferences : function (oldId, newId) {
        var changed = this.Super("updateGlobalIDReferences", arguments);
        changed = this.updateGlobalIDInValueProperty("value", oldId, newId) || changed;
        return changed;
    }
});

//> @class GridSaveAllEditsTask
// Save all changes in a grid which has auto-saving disabled. Before the save, any pending
// edit rows are saved to the grid.
//
// @see listGrid.endEditing
// @see listGrid.saveAllEdits
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("GridSaveAllEditsTask", "ComponentTask");

isc.GridSaveAllEditsTask.addProperties({
    componentBaseClass: "ListGrid",

    classDescription: "Save all changes in a grid which has auto-saving disabled",
    editorType: "GridSaveAllEditsTaskEditor",


    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var grid = this.getTargetComponent(process);
        if (!grid) return true;

        // Make sure all active editors are closed
        grid.endEditing();

        // A grid with no changes to save will not trigger the callback to saveAllEdits()
        // so don't attempt save in that case and continue workflow
        if (grid.hasChanges()) {
            grid.saveAllEdits(null, function () {
                process.start();
            });
            return false;
        }
        return true;
    },

    getElementDescription : function () {
        return "Save all '" + this.componentId + "' edits";
    }
});

//> @class GridSelectRecordsTask
// Select or deselect one or more records as specified by criteria. Target records will also
// be scrolled into view or, for a tree, parent folders will be opened to reveal the node.
// <p>
// Task Output:
// <ul>
//   <li>On a select, the set of newly selected records is the output, even if other records
//       are also selected.
//   <li>On a deselect, the entire set of de-selected records is the output.
// </ul>
//
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<

isc.defineClass("GridSelectRecordsTask", "ComponentTask");

isc.GridSelectRecordsTask.addProperties({
    componentBaseClass: ["ListGrid", "TileGrid"],

    componentRequiresDataSource: true,

    classDescription: "Select record(s) or clear selection and scroll selection into view " +
        "(or reveal a hidden tree node)",
    editorType: "GridSelectRecordsTaskEditor",

    //> @attr gridSelectRecordsTask.criteria (Criteria : null : IR)
    // Criteria defining the records that should be selected or deselected. All records are
    // selected or deselected if not specified.
    // <P>
    // To target a single record just specify criteria for its primary key.
    //
    // @visibility workflow
    //<

    //> @attr gridSelectRecordsTask.select (Boolean : null : IR)
    // Set to <code>false</code> to clear selection.
    //
    // @visibility workflow
    //<

    //> @attr gridSelectRecordsTask.selectMultiple (Boolean : null : IR)
    // Should multiple records matching +link{criteria} be affected? If set to
    // <code>false</code> only the first matching record is affected.
    //
    // @visibility workflow
    //<

    //> @attr gridSelectRecordsTask.keepExistingSelection (Boolean : null : IR)
    // For grids that allow multiple selection, should any existing selection be retained?
    // Only applies when selecting records.
    //
    // @visibility workflow
    //<

    //> @attr gridSelectRecordsTask.scrollIntoView (Boolean : null : IR)
    // Set to <code>false</code> to prevent the first affected record from being scrolled
    // into view.
    //
    // @visibility workflow
    //<


    executeElement : function (process) {
        this.process = process;
        var targetGrid = this.getTargetComponent(process);
        if (!targetGrid) return true;

        var output;

        if (!this.criteria) {
            if (this.select != false) {
                if (this.selectMultiple) {
                    targetGrid.selectAllRecords();
                    output = targetGrid.getSelectedRecords();
                } else {
                    targetGrid.selectSingleRecord(0);
                    output = targetGrid.getRecord(0);
                }
            } else {
                output = targetGrid.getSelectedRecords();
                targetGrid.deselectAllRecords();
            }
            if (this.scrollIntoView != false) {
                targetGrid.scrollToRow(0);
            }
        } else {
            // Find matching records using Grid's DataSource if applicable
            var ds = targetGrid.getDataSource(),
                data = (isc.isAn.Array(targetGrid.data) ? targetGrid.data :
                        (isc.isA.Tree(targetGrid.data) ? targetGrid.data.getAllNodes() :
                         targetGrid.data.getAllCachedRows())),
                criteria = this._resolveCriteriaExpressions(this.criteria, process.state, process.state, process),
                selectedRecords
            ;
            if (ds) {
                selectedRecords = ds.applyFilter(data, criteria);
            } else {
                selectedRecords = isc.DS.applyFilter(data, criteria);
            }
            var selectedCount = selectedRecords.length;
            if (selectedCount > 0) {
                output = selectedRecords;
                // Found record(s) to select/deselect.
                // Use special case, simple methods for selection to take advantage of
                // optimizations.
                if (this.select != false) {
                    if (!this.keepExistingSelection && !this.selectMultiple && selectedCount > 0) {
                        targetGrid.selectSingleRecord(selectedRecords[0]);
                        output = selectedRecords[0];
                    } else {
                        if (!this.keepExistingSelection) {
                            targetGrid.deselectAllRecords();
                        }

                        targetGrid.selectRecords(selectedRecords);
                    }
                } else {
                    targetGrid.deselectRecords(selectedRecords);
                }

                if (this.scrollIntoView != false) {
                    // Make sure first selected node is visible because all parent nodes
                    // are open
                    var scrollToRecord = selectedRecords[0],
                        data = targetGrid.data
                    ;
                    if (isc.isA.Tree(data)) {
                        var parents = data.getParents(scrollToRecord);
                        if (parents) {
                            data.openFolders(parents);
                        }
                    }

                    // Scroll to first selected record
                    var rowNum = targetGrid.getRecordIndex(scrollToRecord);
                    targetGrid.scrollToRow(rowNum);
                }
            } else if (this.keepExistingSelection) {
                output = targetGrid.getSelectedRecords();
            }
        }
        process.setTaskOutput(this, output);

        return true;
    },

    getOutputSchema : function () {
        var grid = this.getTargetComponent(this.process);
        if (!grid) return null;
        var ds = grid.dataSource;
        if (ds && (ds.getClassName == null || ds.getClassName() != "DataSource")) {
            ds = isc.DataSource.get(ds);
        }
        return ds;
    },

    getElementDescription : function () {
        var target = (this.componentId ? "'" + this.componentId + "'" : ""),
            select = (this.select != false),
            selectMultiple = (this.selectMultiple != false)
        ;
        if (isc.isA.String(select)) select = (select == "true");
        var all = (!this.criteria ? (select && !selectMultiple ? "first " : "all ") : ""),
            recordPlural = (select && !selectMultiple ? "" : "s")
        ;

        return (select ? "S" : "Des") + "elect " + all + target + " record" + recordPlural;
    },

    updateGlobalIDReferences : function (oldId, newId) {
        var changed = this.Super("updateGlobalIDReferences", arguments);

        if (this.criteria) {
            changed = this.updateGlobalIDInCriteria(this.criteria, oldId, newId) || changed;
        }

        return changed;
    }
});

//> @class GridTransferDataTask
// Transfer selected records from one grid to another.
//
// @see listGrid.transferSelectedData
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("GridTransferDataTask", "ComponentTask");

isc.GridTransferDataTask.addProperties({
    componentBaseClass: ["ListGrid", "TileGrid"],

    classDescription: "Transfer selected records from one grid to another",
    editorType: "GridTransferDataTaskEditor",

    //> @attr gridTransferDataTask.sourceComponent (GlobalId : null : IR)
    //  Source component from which the record(s) will be transferred.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        this.process = process;
        var targetGrid = this.getTargetComponent(process);
        if (!targetGrid) return true;

        var sourceComponentId = this.sourceComponent;
        if (!sourceComponentId) {
            this.logWarn("sourceComponent not specified on task. Task skipped.");
            return true;
        }
        var sourceComponent = this.getLocalComponent(process, sourceComponentId);
        if (!sourceComponent) {
            this.logWarn("sourceComponent '" + sourceComponentId + "' not found. Task skipped.");
            return true;
        }

        var task = this;
        targetGrid.transferSelectedData(sourceComponent, null, function (records) {
            var firstRecord = (records && records.length > 0 ? records[0] : null);
            process.setTaskOutput(task, firstRecord);

            process.start();
        });
        return false;
    },

    getOutputSchema : function () {
        var grid = this.getTargetComponent(this.process);
        if (!grid) return null;
        var ds = grid.dataSource;
        if (ds && (ds.getClassName == null || ds.getClassName() != "DataSource")) {
            ds = isc.DataSource.get(ds);
        }
        return ds;
    },

    getElementDescription : function () {
        var target = (this.componentId ? " to '" + this.componentId + "'" : "");
        return "Transfer selected data from '" + this.sourceComponent + "'" + target;
    },

    updateGlobalIDReferences : function (oldId, newId) {
        var changed = this.Super("updateGlobalIDReferences", arguments);

        if (this.sourceComponent && this.sourceComponent == oldId) {
            this.sourceComponent = newId;
            changed = true;
        }
        return changed;
    }
});

//> @class GridExportDataTask
// Export data currently shown in a grid.
//
// @see dataBoundComponent.exportData
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("GridExportDataTask", "ComponentTask");

isc.GridExportDataTask.addProperties({
    componentBaseClass: ["ListGrid", "TileGrid", "DetailViewer"],
    componentRequiresDataSource: true,

    typeTitle: "Export Data (Server)",
    classDescription: "Export data currently shown in a grid",
    editorType: "GridExportDataTaskEditor",

    //> @attr gridExportDataTask.requestProperties (DSRequest : null : IR)
    // Additional properties to set on the DSRequest that will be issued
    // to perform server-side export.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var grid = this.getTargetComponent(process);
        if (!grid) return true;

        grid.exportData(this.requestProperties, function () {
            process.start();
        });
        return false;
    },

    getElementDescription : function () {
        return "Export '" + this.componentId + "' data (Server)";
    }
});

//> @class GridExportClientDataTask
// Export data currently shown in a grid keeping all grid-specific formatting.
//
// @see listGrid.exportClientData
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("GridExportClientDataTask", "ComponentTask");

isc.GridExportClientDataTask.addProperties({
    componentBaseClass: ["ListGrid", "TileGrid", "DetailViewer"],

    typeTitle: "Export Data (Client)",
    classDescription: "Export data currently shown in a grid keeping all grid-specific formatting",
    editorType: "GridExportClientDataTaskEditor",

    //> @attr gridExportClientDataTask.requestProperties (DSRequest : null : IR)
    // Additional properties to set on the DSRequest that will be issued
    // to perform client-side export.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var grid = this.getTargetComponent(process);
        if (!grid) return true;

        grid.exportClientData(this.requestProperties, function () {
            process.start();
        });
        return false;
    },

    getElementDescription : function () {
        return "Export '" + this.componentId + "' formatted data (Client)";
    }
});

//> @class SetScreenDataTask
// Sets an embedded screen's +link{canvas.dataContext,dataContext} with the
// +link{setScreenDataTask.dataContextBinding,dataContextBinding} evaluated
// in the scope of this task.
//
// @see screenLoader.setDataContextBinding
// @see canvas.setDataContext
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("SetScreenDataTask", "ComponentTask");

isc.SetScreenDataTask.addProperties({
    componentBaseClass: ["DataView", "ScreenLoader"],

    classDescription: "Set Screen Inputs",
    editorType: "SetScreenDataTaskEditor",

    //> @attr setScreenDataTask.dataContextBinding (DataContextBinding : null : IR)
    // A +link{DataContextBinding} to be applied to the screen via
    // +link{canvas.setDataContext()}.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var component = this.getTargetComponent(process, true);
        if (!component) return true;

        var dataContext,
            binding = this.dataContextBinding
        ;
        if (binding) {
            // binding is just a dataContext with possible dynamic values
            dataContext = isc.clone(binding);
            for (var dsName in dataContext) {
                var values = dataContext[dsName];
                for (var key in values) {
                    values[key] = this.getDynamicValue(values[key], process);
                }
            }
        }

        if (dataContext) {
            if (isc.isA.ScreenLoader(component)) {
                component.setDataContextBinding(dataContext);
            } else {
                component.setDataContext(dataContext);
            }
        }

        return true;
    },

    getElementDescription : function () {
        var target = this.componentId;
        return "Set Screen Inputs on '" + target + "'";
    }
});

//> @class GetPropertiesTask
// Gets the value properties from a component and makes them available within the workflow
// as the +link{group:taskInputExpression,last task output}. Using
// +link{getPropertiesTask.bindOutput,bindOutput}, these values can also be placed into
// the +link{process.state,process state}.
// <P>
// For a canvas the +link{getPropertiesTask.componentId,componentId}
// specifies everything necessary to identify the target.
// For a form control more information is needed. The
// +link{getPropertiesTask.componentId,componentId} identifies the container
// (i.e. DynamicForm) and the individual field is specified as
// +link{getPropertiesTask.targetFieldName,targetFieldName}.
// <P>
// The next task might be a save operation on a DataSource, or SetPropertiesTask
// to copy settings from one component to another.
// <p>
// GetPropertiesTask <em>is an advanced and rarely used task</em>.
// If you need a component property to be dynamic, you can configure that property as a
// +link{class.dynamicProperties,Dynamic Property} without the need for a
// Workflow.  <em>Dynamic Properties</em> created this way automatically
// update as your users make changes.
//
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("GetPropertiesTask", "ComponentTask");

isc.GetPropertiesTask.addProperties({
    componentBaseClass: ["Canvas", "FormItem"],

    classDescription: "Get properties of a component or form control",
    editorType: "GetPropertiesTaskEditor",

    //> @attr getPropertiesTask.properties (Array of String : null : IR)
    // Properties to be retrieved from +link{componentTask.componentId,componentId}.
    //
    // @visibility workflow
    //<

    //> @attr getPropertiesTask.targetFieldName (FieldName : null : IR)
    // If +link{componentTask.componentId,componentId} targets a DynamicForm, this property
    // specifies the name of the target field.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        var component = this.getTargetComponent(process, true);
        if (!component) return true;

        var properties = this.properties;
        if (!isc.isAn.Array(properties)) properties = [properties];

        var output = {};
        for (var i = 0; i < properties.length; i++) {
            var property = properties[i],
                undef,
                value
            ;
            try {
                value = this.getPropertyValue(component, property);
            } catch (ex) {
                // Use an explicit undefined value so something is written into the last task
                // output for the property
                value = undef;
            }
            output[property] = value;
        }

        process.setTaskOutput(this, output);
        return true;
    },

    getPropertyValue : function (component, property) {
        var propertyComponent = this.getPropertyComponent(component),
            value = propertyComponent && propertyComponent.getPropertyValue(property)
        ;
        return value;
    },

    getPropertyComponent : function (component) {
        var propertyComponent;

        if (isc.isA.DynamicForm(component)) {
            var fieldName = this.targetFieldName;
            if (!fieldName) {
                isc.logWarn("Target field not identified by targetFieldName. Task skipped");
                return null;
            }

            var field = component.getField(fieldName);
            if (!field) {
                isc.logWarn("Target field, " + fieldName + ", not found. Task skipped");
                return null;
            }

            propertyComponent = field;
        } else {
            propertyComponent = component;
        }
        return propertyComponent;
    },

    getOutputSchema : function () {
        if (!this._outputSchema) {
            var component = this.getTargetComponent(this.process, true);
            if (!component) return null;
            var propertyComponent = this.getPropertyComponent(component),
                schema = propertyComponent && isc.DS.getNearestSchema(propertyComponent)
            ;
            if (schema) {
                var properties = this.properties,
                    fields = []
                ;
                for (var i = 0; i < properties.length; i++) {
                    var property = properties[i],
                        field = schema.getField(property)
                    ;
                    if (field) {
                        fields.add(field);
                    }
                }
                // If no fields were found in the schema it is likely that no schema are
                // loaded which is normal for a running application
                if (fields.length == 0) {

                }
                if (fields.length > 0) {
                    for (var i = 0; i < fields.length; i++) {
                        var field = fields[i];
                        fields[i] = {
                            name: field.name, title: field.title, type: field.type
                        };
                    }
                    this._outputSchema = isc.DS.create({
                        addGlobalId: false,
                        clientOnly: true,
                        fields: fields
                    });
                }
            }
        }
        return this._outputSchema;
    },

    destroy : function () {
        if (this._outputSchema) this._outputSchema.destroy();
        this.Super("destroy", arguments);
    },

    getElementDescription : function () {
        var target = this.componentId,
            fieldName = this.targetFieldName,
            propList = (this.properties ? ": " + this.properties.join(", ") : "")
        ;
        if (fieldName) {
            target += ", field " + fieldName;
        }
        return "Get properties from '" + target + "'" + propList;
    }
});

//> @class SetPropertiesTask
// Gets the value properties from a component and makes them available within the workflow
// as the +link{group:taskInputExpression,last task output}.
// <P>
// For a canvas the +link{setPropertiesTask.componentId,componentId} specifies everything
// necessary to identify the target.
// For a form control more information is needed. The
// +link{setPropertiesTask.componentId,componentId} identifies the container
// (i.e. DynamicForm) and the individual field is specified as
// +link{setPropertiesTask.targetFieldName,targetFieldName}.
// <P>
// SetPropertiesTask <em>is an advanced and rarely used task</em>.
// If you need a component property to be dynamic, you can configure that property as a
// +link{class.dynamicProperties,Dynamic Property} without the need for a
// Workflow.  <em>Dynamic Properties</em> created this way automatically
// update as your users make changes.
//
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("SetPropertiesTask", "ComponentTask");

isc.SetPropertiesTask.addProperties({
    componentBaseClass: ["Canvas", "FormItem"],

    classDescription: "Set properties of a component or form control",
    editorType: "SetPropertiesTaskEditor",

    //> @object PropertyValue
    // Identifies a property name and value to be assigned to a component by the
    // +link{setPropertiesTask}.
    //
    // @treeLocation Client Reference/Workflow
    // @visibility workflow
    //<

    //> @attr propertyValue.name (String : null : IR)
    // Name of the property to be assigned the new value.
    // @visibility workflow
    //<

    //> @attr propertyValue.value (Any : null : IR)
    // Name of the property to be assigned the new value.
    // <p>
    // A value prefixed with "$" will be treated as a +link{group:taskInputExpression}
    // excluding "$input" and "$inputRecord" references.

    // @visibility workflow
    //<

    //> @attr setPropertiesTask.properties (Array of PropertyValue : null : IR)
    // Properties and associated values to be set on +link{componentTask.componentId,componentId}.
    //
    // @visibility workflow
    //<

    //> @attr setPropertiesTask.targetFieldName (FieldName : null : IR)
    // If +link{componentTask.componentId,componentId} targets a DynamicForm, this property
    // specifies the name of the target field.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var component = this.getTargetComponent(process, true);
        if (!component) return true;

        var propertyComponent = this.getPropertyComponent(component);
        if (!propertyComponent) return true;

        var properties = this.properties;
        if (!isc.isAn.Array(properties)) properties = [properties];

        var propertiesValues = {};
        for (var i = 0; i < properties.length; i++) {
            var property = properties[i],
                name = property.name,
                value = this.getDynamicValue(property.value, process)
            ;
            propertiesValues[name] = value;
        }
        try {
            propertyComponent.setProperties(propertiesValues);
        } catch (ex) {
            // Ignore bad application of property values
        }
        return true;
    },

    getPropertyComponent : function (component) {
        var propertyComponent = component;

        if (isc.isA.DynamicForm(component)) {
            var fieldName = this.targetFieldName,
                field = fieldName && component.getField(fieldName)
            ;
            if (fieldName && field) {
                propertyComponent = field;
            } else if (fieldName) {
                isc.logWarn("Target field, " + fieldName + ", not found. Task skipped");
                return null;
            }
        } else {
            propertyComponent = component;
        }
        return propertyComponent;
    },

    getElementDescription : function () {
        var target = this.componentId,
            fieldName = this.targetFieldName,
            propList = (this.properties ? ": " + this.properties.getProperty("name").join(", ") : "")
        ;
        if (fieldName) {
            target += ", field " + fieldName;
        }
        return "Set properties on '" + target + "'" + propList;
    }
});

//> @class PrintCanvasTask
// Print canvas by showing print preview.
//
// @see canvas.showPrintPreview
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("PrintCanvasTask", "ComponentTask");

isc.PrintCanvasTask.addProperties({
    componentBaseClass: ["Canvas"],

    typeTitle: "Print",
    classDescription: "Print canvas contents",
    editorType: "PrintCanvasTaskEditor",

    //> @attr printCanvasTask.printProperties (PrintProperties : null : IR)
    // PrintProperties object for customizing the print HTML output.
    //
    // @visibility workflow
    //<

    //> @attr printCanvasTask.printWindowProperties (PrintWindow : null : IR)
    // Properties to apply to the generated print window.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var canvas = this.getTargetComponent(process);
        if (!canvas) return true;

        isc.Canvas.showPrintPreview(canvas, this.printProperties, this.printCanvasTask, function () {
            process.start();
        });
        return false;
    },

    getElementDescription : function () {
        return "Print '" + this.componentId + "'";
    }
});

//> @class ShowNextToComponentTask
// Show a component next to some other component.
//
//@see canvas.showNextTo
//@inheritsFrom ComponentTask
//@treeLocation Client Reference/Workflow
//@visibility workflow
//<
isc.defineClass("ShowNextToComponentTask", "ComponentTask");

isc.ShowNextToComponentTask.addClassProperties({
    actionMapping: {
        target: "componentId",
        source: "nextToComponentId",
        // positional mapping of showNextTo parameters to task properties
        paramMap: [ null, "side", "canOcclude", "skipAnimation" ]
    }
})

isc.ShowNextToComponentTask.addProperties({
    componentBaseClass: "Canvas",

    typeTitle: "Show Next To",
    classDescription: "Show a component next to some other component",
    editorType: "ShowNextToComponentTaskEditor",

    //> @attr showNextToComponentTask.nextToComponentId (GlobalId : null : IR)
    // The other component where this component will show.
    //
    // @visibility workflow
    //<

    //> @attr showNextToComponentTask.side (String : null : IR)
    // Which side of the other canvas should we show? Options are
    // "top", "bottom", "left", "right". (Defaults to "right")
    //
    // @visibility workflow
    //<

    //> @attr showNextToComponentTask.canOcclude (Boolean : null : IR)
    // Can this component can be positioned on top of the other
    // component if there isn't room to show next to it?
    //
    // @visibility workflow
    //<

    //> @attr showNextToComponentTask.skipAnimation (Boolean : null : IR)
    // Set to <code>false</code> to not use animation to show component.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var canvas = this.getTargetComponent(process);
        if (!canvas) return true;

        var nextToComponentId = this.nextToComponentId;
        if (!nextToComponentId) {
            this.logWarn("nextToComponentId not specified on task. Task skipped.");
            return true;
        }
        var nextToComponent = this.getLocalComponent(process, nextToComponentId);
        if (!nextToComponent) {
            this.logWarn("nextToComponentId '" + nextToComponentId + "' not found. Task skipped.");
            return true;
        }

        canvas.showNextTo(nextToComponent, this.side, this.canOcclue, this.skipAnimation);
        return true;
    },

    getElementDescription : function () {
        var target = (this.nextToComponentId ? "'" + this.nextToComponentId + "'" : "component");
        return "Show '" + this.componentId + "' next to " + target;
    },

    updateGlobalIDReferences : function (oldId, newId) {
        var changed = this.Super("updateGlobalIDReferences", arguments);

        if (this.nextToComponentId && this.nextToComponentId == oldId) {
            this.nextToComponentId = newId;
            changed = true;
        }
        return changed;
    }
});

//> @class NavigateSplitPaneTask
// Causes the list pane component to load data and update its title based on the current
// selection in the source pane. Also shows the pane if it's not already visible.
//
// @see splitPane.navigatePane
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("NavigateSplitPaneTask", "ComponentTask");

isc.NavigateSplitPaneTask.addProperties({
    componentBaseClass: ["SplitPane", "TriplePane"],

    typeTitle: "Go to Pane",
    classDescription: "Move to a particular pane of a SplitPane or TriplePane, updating " +
        "data based on what's selected",
    editorType: "NavigateSplitPaneTaskEditor",

    //> @attr navigateSplitPaneTask.targetPane (CurrentPane : null : IR)
    // Pane target to be navigated from the current pane.
    //
    // @visibility workflow
    //<

    //> @attr navigateSplitPaneTask.title (String : null : IR)
    // Title to show instead of the automatically chosen one.
    //
    // @visibility workflow
    //<

    //> @attr navigateSplitPaneTask.showRecursively (Boolean : null : IR)
    // Set to <code>false</code> to not show a component's parents.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var splitPane = this.getTargetComponent(process);
        if (!splitPane) return true;

        var title = this.getDynamicValue(this.title, process);

        var targetPane = this.targetPane;
        splitPane.navigatePane(targetPane, title);

        if (this.showRecursively != false) {
            // If no targetPane is specified, default the same as NavigatePage does
            if (targetPane == null) {
                if (splitPane._hasListPane()) {
                    targetPane = "list";
                } else {
                    targetPane = "detail";
                }
            }
            if (targetPane) {
                var pane = splitPane[targetPane + "Pane"];
                if (pane) {
                    pane.showRecursively();
                }
            }
        }
        return true;
    },

    getElementDescription : function () {
        // Assumption is that this is only called from WorkflowEditor where targetPane is required
        var targetPane = this.targetPane.substring(0,1).toUpperCase() + this.targetPane.substring(1);
        return "Navigate " + targetPane + " Pane of '" + this.componentId + "'";
    }
});

//> @class AddScreenTask
// Adds a new screen instance to a Layout, as a new Tab in a TabSet or as a new Section in
// a SectionStack. When the target is a TabSet or SectionStack, a static
// +link{addScreenTask.title} or dynamic +link{addScreenTask.titleFormula} can be assigned
// for the new Tab or Section.
// <p>
// The new screen's +link{canvas.dataContext,dataContext} can be configured with
// +link{addScreenTask.dataContextBinding,dataContextBinding} evaluated
// in the scope of this task.
//
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("AddScreenTask", "ComponentTask");

isc.AddScreenTask.addProperties({
    componentBaseClass: ["Layout", "SectionStack", "TabSet"],

    classDescription: "Add screen to component as a new member, section or tab",
    editorType: "AddScreenTaskEditor",

    //> @attr addScreenTask.title (String : null : IR)
    // Title of new SectionStackSection or TabSet when +link{componentId} targets a SectionStack
    // or TabSet. To use a dynamic title see +link{titleFormula}.
    //
    // @visibility workflow
    //<

    //> @attr addScreenTask.titleFormula (UserSummary : null : IR)
    // Formula to be used to calculate the title contents. Use +link{title} property
    // to assign a static title instead.
    // <p>
    // Available fields for use in the formula are the current +link{canvas.ruleScope,rule context}.
    //
    // @visibility workflow
    //<

    //> @attr addScreenTask.canClose (Boolean : null : IR)
    // Can the created SectionStackSection or TabSet be closed by the user?
    //
    // @visibility workflow
    //<

    //> @attr addScreenTask.screenName (String : null : IR)
    // Name of screen to be added.
    //
    // @visibility workflow
    //<

    //> @attr addScreenTask.dataContextBinding (DataContextBinding : null : IR)
    // A +link{DataContextBinding} to be applied to the created screen via
    // +link{canvas.setDataContext()}.
    //
    // @visibility workflow
    //<

    errorBorder: "1px solid red",
    errorText: "Screen '${screenName}' could not be loaded",
    errorTextColor: "red",

    errorScreenCrashed: "Screen crashed on creation",

    errorLabelDefaults: {
        _constructor: isc.Label,
        width: "100%",
        height: "100%",
        align: "center"
    },

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var component = this.getTargetComponent(process, true);
        if (!component) return true;

        var title = this.getTextFormulaValue(this.titleFormula, process) ||
                    this.getDynamicValue(this.title, process);

        var dataContext = this.resolveDataContextBinding(this.dataContextBinding, process);

        if (isc.isA.TabSet(component)) {
            var tab = {
                title: title,
                canClose: this.canClose
            };
            component.addTab(tab);
            component.selectTab(tab);
            this.createScreen(this.screenName, dataContext, function (screen) {
                component.setTabPane(tab, screen);
            });

        } else if (isc.isA.SectionStack(component)) {
            component.addSection({
                title: title,
                canClose: this.canClose
            });
            var sectionNames = component.getSectionNames(),
                addedSectionName = sectionNames[sectionNames.length-1]
            ;
            this.createScreen(this.screenName, dataContext, function (screen) {
                component.setItems(addedSectionName, screen);
                component.expandSection(addedSectionName);
            });

        } else {
            this.createScreen(this.screenName, dataContext, function (screen) {
                component.addMember(screen);
            });
        }

        return true;
    },

    resolveDataContextBinding : function (binding, process) {
        if (!binding) return null;

        // binding is just a dataContext with possible dynamic values
        var dataContext = isc.clone(binding);
        for (var dsName in dataContext) {
            var values = dataContext[dsName];
            for (var key in values) {
                values[key] = this.getDynamicValue(values[key], process);
            }
        }
        return dataContext;
    },

    createScreen : function (screenName, dataContext, callback) {
        var task = this;
        isc.ScreenLoader.loadScreen(screenName, function (error) {
            var screen;
            if (error) {
                screen = task.createErrorScreen(screenName, error);
            }
            if (!screen) {
                try {
                    screen = isc.ScreenLoader.createScreen(screenName);
                } catch (ex) {
                    screen = task.createErrorScreen(screenName, task.errorScreenCrashed);
                }
                if (dataContext && screen && screen.setDataContext) {
                    screen.setDataContext(dataContext);
                }
            }
            task.fireCallback(callback, "screen", [screen]);
        });
    },

     createErrorScreen : function (screenName, message) {
        var errorText = "<span style='color: " + this.errorTextColor + "'>" +
                        this.errorText.evalDynamicString(this, { screenName: screenName }) +
                        "<p>" +
                        message +
                        "</span>";
        return this.createAutoChild("errorLabel", { contents: errorText, border: this.errorBorder });
    },

    getElementDescription : function () {
        var target = this.componentId,
            screenName = this.screenName
        ;
        return "Add screen " +
                (screenName ? "'" + screenName + "'" : "") +
                " to '" + target + "'";
    }
});

//> @class LogOutTask
// Logs out the current user by opening the +link{Auth.logOutURL} in another tab or window.
//
// @inheritsFrom ProcessElement
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("LogOutTask", "ProcessElement");

isc.LogOutTask.addProperties({
    classDescription: "Logs the current user out of the application",
    editorType: null,

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var url = (isc.Auth ? isc.Auth.logOutURL : null);
        if (!url) {
            isc.logWarn("logOutURL not specified on Auth class. Task skipped");
            return true;
        }

        isc.Auth.logOut();
        return true;
    },

    getElementDescription : function () {
        return "Log out the current user";
    }
});

//> @class ResetPasswordTask
// Show user password reset dialog by opening the +link{Auth.resetPasswordURL}
// in another tab or window.
//
// @inheritsFrom ProcessElement
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("ResetPasswordTask", "ProcessElement");

isc.ResetPasswordTask.addProperties({
    classDescription:  "Sends the user to a screen to reset their password",
    editorType: null,

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var url = (isc.Auth ? isc.Auth.resetPasswordURL : null);
        if (!url) {
            isc.logWarn("resetPasswordURL not specified on Auth class. Task skipped");
            return true;
        }

        isc.Auth.resetPassword();
        return true;
    },

    getElementDescription : function () {
        return "Reset user password";
    }
});

//> @class SendEmailTask
// Sends an email using the +link{group:reifyMessaging} <code>isc_sendEmail</code> DataSource.
// Refer to +link{Mail, Mail overview} to know how to set up access to an SMTP server.
// <p>
// There is a matching Reify Workflow Editor task editor, +link{SendEmailTaskEditor}, that
// can be enabled by +link{reify.enableSendEmailTaskEditor}.
// <p>
// If +link{process.mockMode, mock mode} is enabled, instead of sending an email a
// +link{sendEmailTask.mockModeNotifyMessage, notification message} is shown instead.
//
// @inheritsFrom ProcessElement
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("SendEmailTask", "ProcessElement");

isc.SendEmailTask.addProperties({

    // required by WorkflowEditor to be able to edit existing task
    editorType: "SendEmailTaskEditor",

    //> @attr sendEmailTask.to (String : null : IR)
    // Email recipient address(es) separated by commas.
    // @visibility workflow
    //<

    //> @attr sendEmailTask.subject (String : null : IR)
    // Email subject. To assign a dynamic subject see +link{subjectFormula}.
    // @visibility workflow
    //<

    //> @attr sendEmailTask.subjectFormula (UserSummary : null : IR)
    // Formula to be used to calculate the subject contents. Use +link{subject} property
    // to assign a static subject instead.
    // <p>
    // Available fields for use in the formula are the current +link{canvas.ruleScope,rule context}.
    //
    // @visibility workflow
    //<

    //> @attr sendEmailTask.message (String : null : IR)
    // Message to be sent to recipients.
    // @visibility workflow
    //<

    //> @attr sendEmailTask.mockModeNotifyMessage (String : "Workflow Mock Mode - in live mode, would send Email<p>To: ${to}<br>Subject: ${subject}<br>Message: ${message}" : IR)
    // Message displayed by +link{Notify} in lieu of sending an actual email when the workflow
    // is in +link{process.mockMode, mock mode}.
    // <P>
    // This is a dynamic string - text within <code>&#36;{...}</code> are dynamic variables and will
    // be evaluated as JS code when the message is displayed.
    // <P>
    // The following dynamic variables are available:
    // <ul>
    // <li>to</li>
    // <li>subject</li>
    // <li>message</li>
    // </ul>
    //
    // @group i18nMessages
    // @visibility workflow
    //<
    mockModeNotifyMessage: "Workflow Mock Mode - in live mode, would send Email<p>To: ${to}<br>Subject: ${subject}<br>Message: ${message}",

    //> @attr sendEmailTask.failureElement (String : null : IR)
    // ID of the next sequence or element to proceed to if a failure condition arises
    // from operation.
    // @visibility workflow
    //<

    //> @attr sendEmailTask.requestProperties (DSRequest : null : IR)
    // Additional properties to set on the DSRequest that will be issued
    // to perform send.
    // <p>
    // Note that <code>operationId</code> will always be set <code>email</code> and
    // <code>willHandleError</code> will always be set <code>true</code>.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var to = this.getDynamicValue(this.to, process),
            subject = this.getTextFormulaValue(this.subjectFormula, process) ||
                      this.getDynamicValue(this.subject, process),
            message = this.getDynamicValue(this.message, process)
        ;

        if (this.isMockModeEnabled(process)) {
            var notifyMessage = this.mockModeNotifyMessage.evalDynamicString(this, {
                to: to,
                subject: subject,
                message: message
            });
            isc.Notify.addMessage(notifyMessage);

            // task complete
            return true;
        }

        var ds = isc.DS.get("isc_sendEmail");
        if (!ds) {
            this.logWarn("isc_sendEmail DataSource not found. Cannot send email.");
            return;
        }

        var params = isc.addProperties({}, this.requestProperties, {
            operationId: "email",
            willHandleError: true
        });

        // Send email by adding record to DataSource
        var data = {
            to: to,
            subject: subject,
            message: message
        };

        var task = this;
        ds.addData(data, function(dsResponse, data, request) {
            dsResponse = dsResponse || {};
            var results = dsResponse.results;
            if (dsResponse.isStructured &&
                (!results || results.status < 0 || (results.status == null && dsResponse.status < 0)))
            {
                if (!isc.RPC.runDefaultErrorHandling(dsResponse, request, task.errorFormatter)) {
                    task.fail(process);
                    return;
                }
            }
            // Continue workflow
            process.start();
        }, params);

        // Task is not complete - wait for server response
        return false;
    },

    fail : function (process) {
        if (!this.failureElement) {
            this.logInfo("SendEmailTask does not have a failureElement. Process is aborting.");
            // the call to setNextElement() below will cause the process to terminate automatically
        }
        if (this.failureElement == "next") {
            process.start();
        } else {
            process.setNextElement(this.failureElement);
        }
    },

    // optional method to generate description for task used in Workflow Editor
    getElementDescription : function () {
        return (this.to ? "Send email to " + this.to : "");
    }
});

//> @class SendSMSTask
// Sends an SMS message using the +link{group:reifyMessaging} <code>isc_sendSMS</code> DataSource.
// <p>
// There is a matching Reify Workflow Editor task editor, +link{SendSMSTaskEditor}, that
// can be enabled by +link{reify.enableSendSMSTaskEditor}.
// <p>
// If +link{process.mockMode, mock mode} is enabled, instead of sending an SMS message a
// +link{sendSMSTask.mockModeNotifyMessage, notification message} is shown instead.
//
// @inheritsFrom ProcessElement
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("SendSMSTask", "ProcessElement");

isc.SendSMSTask.addProperties({

    // required by WorkflowEditor to be able to edit existing task
    editorType: "SendSMSTaskEditor",

    //> @attr sendSMSTask.to (String : null : IR)
    // SMS recipient phone number(s) separated by commas.
    // @visibility workflow
    //<

    //> @attr sendSMSTask.message (String : null : IR)
    // Message to be sent to recipients.
    // @visibility workflow
    //<

    //> @attr sendSMSTask.mockModeNotifyMessage (String : "Workflow Mock Mode - in live mode, would send Email<p>To: ${to}<br>Subject: ${subject}<br>Message: ${message}" : IR)
    // Message displayed by +link{Notify} in lieu of sending an actual email when the workflow
    // is in +link{process.mockMode, mock mode}.
    // <P>
    // This is a dynamic string - text within <code>&#36;{...}</code> are dynamic variables and will
    // be evaluated as JS code when the message is displayed.
    // <P>
    // The following dynamic variables are available:
    // <ul>
    // <li>to</li>
    // <li>message</li>
    // </ul>
    //
    // @group i18nMessages
    // @visibility workflow
    //<
    mockModeNotifyMessage: "Workflow Mock Mode - in live mode, would send SMS<p>To: ${to}<br>Message: ${message}",

    //> @attr sendSMSTask.failureElement (String : null : IR)
    // ID of the next sequence or element to proceed to if a failure condition arises
    // from operation.
    // @visibility workflow
    //<

    //> @attr sendSMSTask.requestProperties (DSRequest : null : IR)
    // Additional properties to set on the DSRequest that will be issued
    // to perform send.
    // <p>
    // Note that <code>operationId</code> will always be set <code>sms</code> and
    // <code>willHandleError</code> will always be set <code>true</code>.
    //
    // @visibility workflow
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var to = this.getDynamicValue(this.to, process),
            message = this.getDynamicValue(this.message, process)
        ;

        if (this.isMockModeEnabled(process)) {
            var notifyMessage = this.mockModeNotifyMessage.evalDynamicString(this, {
                to: to,
                message: message
            });
            isc.Notify.addMessage(notifyMessage);

            // task complete
            return true;
        }

        var ds = isc.DS.get("isc_sendSMS");
        if (!ds) {
            this.logWarn("isc_sendSMS DataSource not found. Cannot send message.");
            return;
        }

        var params = isc.addProperties({}, this.requestProperties, {
            operationId: "sms",
            willHandleError: true
        });

        // Send email by adding record to DataSource
        var data = {
            to: to,
            message: message
        };

        var task = this;
        ds.addData(data, function(dsResponse, data, request) {
            dsResponse = dsResponse || {};
            var results = dsResponse.results;
            if (dsResponse.isStructured &&
                (!results || results.status < 0 || (results.status == null && dsResponse.status < 0)))
            {
                if (!isc.RPC.runDefaultErrorHandling(dsResponse, request, task.errorFormatter)) {
                    task.fail(process);
                    return;
                }
            }
            // Continue workflow
            process.start();
        }, params);

        // Task is not complete - wait for server response
        return false;
    },

    fail : function (process) {
        if (!this.failureElement) {
            this.logInfo("SendSMSTask does not have a failureElement. Process is aborting.");
            // the call to setNextElement() below will cause the process to terminate automatically
        }
        if (this.failureElement == "next") {
            process.start();
        } else {
            process.setNextElement(this.failureElement);
        }
    },

    // optional method to generate description for task used in Workflow Editor
    getElementDescription : function () {
        return (this.to ? "Send SMS to " + this.to : "");
    }
});

// ----------------------------------------------------------------------------------------

//>!BackCompat 2020.06.24
// The following tasks were consolidated into other, combined tasks to make the use and
// documentation cleaner. The tasks are retained but the docs are removed (no visibility).

//> @class SetSectionTitleTask
// Sets the title of a SectionStack section. The section is identified by
// specifying either the +link{setSectionTitleTask.targetSectionName} or
// +link{setSectionTitleTask.targetSectionTitle}.
//
// @see sectionStack.setSectionTitle
// @inheritsFrom ComponentTask
//<
isc.defineClass("SetSectionTitleTask", "ComponentTask");

isc.SetSectionTitleTask.addProperties({
    componentBaseClass: "SectionStack",

    classDescription: "Sets the title of a section in a SectionStack",
    editorType: "SetSectionTitleTaskEditor",

    //> @attr setSectionTitleTask.title (HTMLString : null : IR)
    // Title to assign to section. To assign a dynamic value see +link{textFormula}.
    //<

    //> @attr setSectionTitleTask.textFormula (UserSummary : null : IR)
    // Formula to be used to calculate the section title contents. Use +link{title} property
    // to assign a static title instead.
    // <p>
    // Available fields for use in the formula are the current +link{canvas.ruleScope,rule context}.
    //<

    //> @attr setSectionTitleTask.targetSectionName (String : null : IR)
    // The name of the target section.
    //<

    //> @attr setSectionTitleTask.targetSectionTitle (String : null : IR)
    // The current title of the target section.
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var sectionStack = this.getTargetComponent(process);
        if (!sectionStack) return true;

        var sectionName = this.targetSectionName;
        if (!sectionName && this.targetSectionTitle) {
            var sectionNames = sectionStack.getSectionNames();
            for (var i = 0; i < sectionNames.length; i++) {
                var sectionHeader = sectionStack.getSectionHeader(sectionNames[i]);
                if (sectionHeader && sectionHeader.title == this.targetSectionTitle) {
                    sectionName = sectionNames[i];
                    break;
                }
            }
        }

        if (!sectionName) {
            isc.logWarn("Target section not identified by targetSectionName or targetSectionTitle. Task skipped");
            return true;
        }

        var title = this.getTextFormulaValue(this.textFormula, process) ||
                    this.getDynamicValue(this.title, process);

        sectionStack.setSectionTitle(sectionName, title);
        return true;
    },

    getElementDescription : function () {
        return "Set '" + this.componentId + "' section title";
    }
});

//> @class SetButtonTitleTask
// Sets the title of a button or window.
//
// @see button.setTitle
// @inheritsFrom ComponentTask
//<
isc.defineClass("SetButtonTitleTask", "ComponentTask");

isc.SetButtonTitleTask.addProperties({
    componentBaseClass: [ "Button", "Window" ],

    classDescription: "Sets the title of a button or window",
    editorType: "SetButtonTitleTaskEditor",

    //> @attr setButtonTitleTask.title (HTMLString : null : IR)
    // Title to assign to button. To assign a dynamic value see +link{textFormula}.
    //<

    //> @attr setButtonTitleTask.textFormula (UserSummary : null : IR)
    // Formula to be used to calculate the button title contents. Use +link{title} property
    // to assign a static value instead.
    // <p>
    // Available fields for use in the formula are the current +link{canvas.ruleScope,rule context}.
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var button = this.getTargetComponent(process);
        if (!button) return true;

        var title = this.getTextFormulaValue(this.textFormula, process) ||
                    this.getDynamicValue(this.title, process);

        button.setTitle(title);
        return true;
    },

    getElementDescription : function () {
        var description = "Set '" + this.componentId + "' title";

        return description;
    }
});

//> @class SetLabelTextTask
// Sets the text of a label.
//
// @see label.setContents
// @inheritsFrom ComponentTask
//<
isc.defineClass("SetLabelTextTask", "ComponentTask");

isc.SetLabelTextTask.addProperties({
    componentBaseClass: "Label",

    classDescription: "Sets the text of a label",

    //> @attr setLabelTextTask.value (HTMLString : null : IR)
    // Value to assign to label text contents. To assign a dynamic value see +link{textFormula}.
    //<

    //> @attr setLabelTextTask.textFormula (UserSummary : null : IR)
    // Formula to be used to calculate the label text contents. Use +link{value} property
    // to assign a static value instead.
    // <p>
    // Available fields for use in the formula are the current +link{canvas.ruleScope,rule context}.
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var label = this.getTargetComponent(process);
        if (!label) return true;

        var value = this.getTextFormulaValue(this.textFormula, process) ||
                    this.getDynamicValue(this.value, process);

        label.setContents(value);
        return true;
    },

    getElementDescription : function () {
        return "Set '" + this.componentId + "' text";
    }
});

//> @class ShowComponentTask
// Show a currently hidden component.
//
// @see canvas.show
// @inheritsFrom ComponentTask
// @treeLocation Client Reference/Workflow
// @visibility workflow
//<
isc.defineClass("ShowComponentTask", "ComponentTask");

isc.ShowComponentTask.addProperties({
    componentBaseClass: "Canvas",

    typeTitle: "Show",
    classDescription: "Show a currently hidden component",
    editorType: "ShowComponentTaskEditor",

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var canvas = this.getTargetComponent(process);
        if (!canvas) return true;

        canvas.show();
        return true;
    },

    getElementDescription : function () {
        return "Show " + this.componentId;
    }
});

//> @class HideComponentTask
// Hide a component.
//
// @see canvas.hide
// @inheritsFrom ComponentTask
//<
isc.defineClass("HideComponentTask", "ComponentTask");

isc.HideComponentTask.addProperties({
    componentBaseClass: "Canvas",

    typeTitle: "Hide",
    classDescription: "Hide a component",
    editorType: "HideComponentTaskEditor",

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var canvas = this.getTargetComponent(process);
        if (!canvas) return true;

        canvas.hide();
        return true;
    },

    getElementDescription : function () {
        return "Hide " + this.componentId;
    }
});

//> @class FormHideFieldTask
// Hide or show a form field.
//
// @see formItem.show
// @see formItem.hide
// @inheritsFrom ComponentTask
//<
isc.defineClass("FormHideFieldTask", "ComponentTask");

isc.FormHideFieldTask.addProperties({
    componentBaseClass: "DynamicForm",

    typeTitle: "Show / Hide Field",
    classDescription: "Show or hide a field of a form",
    editorType: "FormHideFieldTaskEditor",

    //> @attr formHideFieldTask.targetField (FieldName : null : IR)
    // Field to show/hide.
    //<

    //> @attr formHideFieldTask.hide (Boolean : null : IR)
    // Should the target form item be hidden?
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var form = this.getTargetComponent(process);
        if (!form) return true;

        var targetField = this.targetField;
        if (!targetField) {
            this.logWarn("targetField not specified on task. Task skipped.");
            return true;
        }

        var hide = this.hide;
        if (isc.isA.String(hide)) hide = (hide == "true");

        if (hide) form.hideItem(targetField);
        else form.showItem(targetField);

        return true;
    },

    getElementDescription : function () {
        var hide = this.hide;
        if (isc.isA.String(hide)) hide = (hide == "true");
        var action = (hide ? "Hide" : "Show");
        return action + " '" + this.componentId + "." + this.targetField + "'";
    }
});

//> @class NavigateListPaneTask
// Causes the list pane component to load data and update its title based on the current
// selection in the source pane. Also shows the pane if it's not already visible.
//
// @see splitPane.navigateListPane
// @inheritsFrom ComponentTask
//<
isc.defineClass("NavigateListPaneTask", "ComponentTask");

isc.NavigateListPaneTask.addProperties({
    componentBaseClass: "TriplePane",

    classDescription: "Navigate to the List pane in a TriplePane, using the selection " +
        "in the Navigation pane to refresh the list, if applicable",

    //> @attr navigateListPaneTask.title (String : null : IR)
    // Title to show instead of the automatically chosen one.
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var triplePane = this.getTargetComponent(process);
        if (!triplePane) return true;

        var title = this.getDynamicValue(this.title, process);

        triplePane.navigateListPane(title);
        return true;
    },

    getElementDescription : function () {
        return "Navigate '" + this.componentId + "' list pane";
    }
});

//> @class NavigateDetailPaneTask
// Causes the detail pane component to load data and update its title based on the current
// selection in the source pane. Also shows the pane if it's not already visible.
//
// @see splitPane.navigateDetailPane
// @inheritsFrom ComponentTask
//<
isc.defineClass("NavigateDetailPaneTask", "ComponentTask");

isc.NavigateDetailPaneTask.addProperties({
    componentBaseClass: [ "SplitPane", "TriplePane" ],

    classDescription: "Navigate to the Detail pane in a SplitPane or TriplePane, " +
        "using the selection in the Navigation pane (for SplitPane) or List Pane (for TriplePane)",

    //> @attr navigateDetailPaneTask.title (String : null : IR)
    // Title to show instead of the automatically chosen one.
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var splitPane = this.getTargetComponent(process);
        if (!splitPane) return true;

        var title = this.getDynamicValue(this.title, process);

        splitPane.navigateDetailPane(title);
        return true;
    },

    getElementDescription : function () {
        return "Navigate '" + this.componentId + "' detail pane";
    }
});

//> @class GridStartEditingTask
// Start editing a new record.
//
// @see listGrid.startEditingNew
// @inheritsFrom ComponentTask
//<
isc.defineClass("GridStartEditingTask", "GridEditRecordTask");

isc.GridStartEditingTask.addProperties({

    //> @attr gridStartEditingTask.initialValues (Record : null : IR)
    // Initial values for new edit record.
    // <p>
    // Data values prefixed with "$" will be treated as a +link{group:taskInputExpression}
    // excluding "$input" and "$inputRecord" references.
    //<

    executeElement : function (process) {
        process.passThruTaskOutput(this);

        var grid = this.getTargetComponent(process);
        if (!grid) return true;

        var values;
        if (this.initialValues) {
            // Resolve any dynamicCriteria or taskInputExpressions. Returns a copy of initialValues.
            values = this._resolveObjectDynamicExpressions(this.initialValues, null, null, process);
        }

        grid.startEditingNew(values);
        return true;
    },

    getElementDescription : function () {
        return "Edit '" + this.componentId + "' new record";
    },

    updateLastElementBindingReferences : function (taskType) {
        var changed = this.Super("updateLastElementBindingReferences", arguments);
        changed = this.updateLastElementInValues(this.initialValues, taskType) || changed;
        return changed;
    },

    updateGlobalIDReferences : function (oldId, newId) {
        var changed = this.Super("updateGlobalIDReferences", arguments);
        changed = this.updateGlobalIDInValues(this.initialValues, oldId, newId) || changed;
        return changed;
    }
});

//> @class GridFetchRelatedDataTask
// Fetch data related to a record in another grid.
//
// @see listGrid.fetchRelatedData
// @inheritsFrom ComponentTask
//<
isc.defineClass("GridFetchRelatedDataTask", "ComponentTask");

isc.GridFetchRelatedDataTask.addClassProperties({
    actionMapping: {
        target: "componentId",
        source: "recordSourceComponent"
    }
})

isc.GridFetchRelatedDataTask.addProperties({
    componentBaseClass: ["DynamicForm", "DetailViewer", "ListGrid", "TileGrid", "DetailViewer"],
    componentRequiresDataSource: true,

    classDescription: "Fetch data related to a record shown in another component",
    editorType: "FetchRelatedDataTaskEditor",

    //> @attr gridFetchRelatedDataTask.recordSourceComponent (GlobalId : null : IR)
    // Component to pull record for locating related data.
    //<

    //> @attr gridFetchRelatedDataTask.dataSource (DataSource | ID : null : IR)
    // The DataSource used with +link{recordSourceComponent} to pull related data.
    // If not specified, +link{recordSourceComponent} will be used to obtain the schema.
    //<

    executeElement : function (process) {
        this.process = process;
        var grid = this.getTargetComponent(process);
        if (!grid) return true;

        var recordSourceComponentId = this.recordSourceComponent;
        if (!recordSourceComponentId) {
            this.logWarn("recordSourceComponent not specified on task. Task skipped.");
            return true;
        }
        var recordSourceComponent = this.getLocalComponent(process, recordSourceComponentId);
        if (!recordSourceComponent) {
            this.logWarn("recordSourceComponent '" + recordSourceComponentId + "' not found. Task skipped.");
            return true;
        }

        var schema = this.dataSource || recordSourceComponent;

        var record = null,
            sourceIsGrid = isc.isA.ListGrid(recordSourceComponent) || isc.isA.TileGrid(recordSourceComponent)
        ;
        if (sourceIsGrid && recordSourceComponent.anySelected()) {
            record = recordSourceComponent.getSelectedRecord();
        } else if (isc.isA.DynamicForm(recordSourceComponent)) {
            record = recordSourceComponent.getValues();
        } else if (sourceIsGrid) {
            record = recordSourceComponent.getRecord(0);
        } else if (isc.isA.DetailViewer(recordSourceComponent)) {
            record = recordSourceComponent.data[0];
        }
        // If no selected record is found, there is nothing to do
        if (!record) return true;

        var task = this;
        // fetchRelatedData() is synchronous if filtering can be done on the client
        var willFetchData = grid.fetchRelatedData(record, schema, function (dsResponse) {
            var firstRecord = (dsResponse.data && dsResponse.data.length > 0 ? dsResponse.data[0] : null);
            process.setTaskOutput(task, firstRecord);

            process.start();
        }, null, true);
        if (!willFetchData) {
            var firstRecord = grid.getRecord(0);
            process.setTaskOutput(task, firstRecord);
        }
        // If fetching data pause workflow until callback above restarts it (async task)
        return !willFetchData;
    },

    getOutputSchema : function () {
        var grid = this.getTargetComponent(this.process);
        if (!grid) return null;
        var ds = grid.dataSource;
        if (ds && (ds.getClassName == null || ds.getClassName() != "DataSource")) {
            ds = isc.DataSource.get(ds);
        }
        return ds;
    },

    getElementDescription : function () {
        var target = (this.recordSourceComponent ? "in '" + this.recordSourceComponent + "'" : "");
        return "Fetch '" + this.componentId +
               "' data from related record " + target;
    },

    updateGlobalIDReferences : function (oldId, newId) {
        var changed = this.Super("updateGlobalIDReferences", arguments);

        if (this.recordSourceComponent && this.recordSourceComponent == oldId) {
            this.recordSourceComponent = newId;
            changed = true;
        }
        return changed;
    }
});

//<!BackCompat









isc._debugModules = (isc._debugModules != null ? isc._debugModules : []);isc._debugModules.push('Workflow');isc.checkForDebugAndNonDebugModules();isc._moduleEnd=isc._Workflow_end=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc.Log&&isc.Log.logIsInfoEnabled('loadTime'))isc.Log.logInfo('Workflow module init time: ' + (isc._moduleEnd-isc._moduleStart) + 'ms','loadTime');delete isc.definingFramework;if (isc.Page) isc.Page.handleEvent(null, "moduleLoaded", { moduleName: 'Workflow', loadTime: (isc._moduleEnd-isc._moduleStart)});}else{if(window.isc && isc.Log && isc.Log.logWarn)isc.Log.logWarn("Duplicate load of module 'Workflow'.");}
/*

  SmartClient Ajax RIA system
  Version v13.0p_2023-11-09/LGPL Deployment (2023-11-09)

  Copyright 2000 and beyond Isomorphic Software, Inc. All rights reserved.
  "SmartClient" is a trademark of Isomorphic Software, Inc.

  LICENSE NOTICE
     INSTALLATION OR USE OF THIS SOFTWARE INDICATES YOUR ACCEPTANCE OF
     ISOMORPHIC SOFTWARE LICENSE TERMS. If you have received this file
     without an accompanying Isomorphic Software license file, please
     contact licensing@isomorphic.com for details. Unauthorized copying and
     use of this software is a violation of international copyright law.

  DEVELOPMENT ONLY - DO NOT DEPLOY
     This software is provided for evaluation, training, and development
     purposes only. It may include supplementary components that are not
     licensed for deployment. The separate DEPLOY package for this release
     contains SmartClient components that are licensed for deployment.

  PROPRIETARY & PROTECTED MATERIAL
     This software contains proprietary materials that are protected by
     contract and intellectual property law. You are expressly prohibited
     from attempting to reverse engineer this software or modify this
     software for human readability.

  CONTACT ISOMORPHIC
     For more information regarding license rights and restrictions, or to
     report possible license violations, please contact Isomorphic Software
     by email (licensing@isomorphic.com) or web (www.isomorphic.com).

*/

