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
isc.ClassFactory.defineInterface("DataBoundComponent");

//> @interface DataBoundComponent
// A DataBoundComponent is a widget that can configure itself for viewing or editing objects which
// share a certain schema by "binding" to the schema for that object (called a "DataSource").
// <P>
// A schema (or DataSource) describes an object as consisting of a set of properties (or
// "fields").
// <P>
// DataBoundComponents have a +link{dataBoundComponent.dataSource,common set of APIs} for
// dealing with binding to DataSources, 
// +link{dataBoundComponent.fields,overriding or augmenting} the schema information
// provided by a DataSource, and manipulating objects or sets of object from the DataSource.
// <P>
// The following visual components currently support databinding:<pre>
//   +link{class:DynamicForm}
//   +link{class:DetailViewer}
//   +link{class:ListGrid}
//   +link{class:TreeGrid}
//   +link{class:TileGrid}
//   +link{class:ColumnTree}
//   +link{class:CubeGrid}
// </pre>
// The following non-visual components also support databinding:<pre>
//   +link{class:ValuesManager}
//   +link{class:ResultSet}
//   +link{class:ResultTree}
// </pre>
//
// @treeLocation Client Reference/Data Binding
// @visibility external
//<
// Currently the DataBinding APIs are present on all Canvii.
// Documented as a separate, intervening class, to separate functionality (DataBoundComponent) 

isc.Canvas.addClassProperties({
    //> @type DragDataAction
	// What do we do with data that's been dropped into another list?
    // @value "none" Don't do anything, resulting in the same data being in both lists. 
    // @value isc.Canvas.COPY Copy the data leaving the original in our list.
    COPY:"copy",		
    // @value isc.Canvas.MOVE Remove the data from this list so it can be moved into the other list.
	MOVE:"move",		
	// @group dragdrop
	// @visibility external
	//<
    // Backcompat only: docs removed in 5.5 release in favor of "copy"
	CLONE:"clone",

    
    validateFieldNames: false,

     
    _dbcTypeDetails : {
        "DynamicForm":  { titleSuffix: "Form",        criteriaBasePathSuffix: "values"        , metaFields: ["focusField","hasChanges"] },
        "ListGrid":     { titleSuffix: "Grid",        criteriaBasePathSuffix: "selectedRecord", metaFields: ["focusField","isGrouped","anySelected","multiSelected","numSelected"] },
        "TreeGrid":     { titleSuffix: "Tree",        criteriaBasePathSuffix: "selectedRecord", metaFields: ["focusField","anySelected","multiSelected","numSelected"] },
        "TileGrid":     { titleSuffix: "Tile Grid",   criteriaBasePathSuffix: "selectedRecord" },
        "CubeGrid":     { titleSuffix: "Cube",        criteriaBasePathSuffix: "selectedRecord" },
        "ColumnTree":   { titleSuffix: "Column Tree", criteriaBasePathSuffix: "selectedRecord" },
        "DetailViewer": { titleSuffix: "Details",     criteriaBasePathSuffix: "values"         }
    },

    _dbcTypeMetaFieldTypes : {
        "focusField":   "text",
        "hasChanges":   "boolean",
        "isGrouped":    "boolean",
        "anySelected":  "boolean",
        "multiSelected":"boolean",
        "numSelected":  "integer"
    },

    _ruleScopeMetaFieldNamePrefix: "_meta_",

    maxNumInvalidFieldNameWarnings: 1,
    _numInvalidFieldNameWarningsShown: 0
});

isc.Canvas.addClassMethods({

_validateFieldNames : function (fields, caller) {
    var isForm = isc.isAn.Instance(caller) && caller.getClass().isA(isc.DynamicForm),
        i, field, message;
    for (i = 0; i < fields.length; i++) {
        field = fields[i];

        // Every field must have a name that is a valid JavaScript identifier (except that
        // if the fields are from a DynamicForm (and hence the fields are FormItems) then a
        // field does not need to have a name if its shouldSaveValue property is set to false).
        var valid = ((isForm && field.shouldSaveValue === false && field.name == null) || String.isValidID(field.name));

        if (!valid && isc.Canvas._numInvalidFieldNameWarningsShown++ < isc.Canvas.maxNumInvalidFieldNameWarnings) {
            message = "'" + field.name + "' is not a valid JavaScript identifier. DataSource and " +
                      "DataBoundComponent field names are required to be valid JavaScript identifiers, " +
                      "the syntax for which is specified by ECMA-262 Section 7.6. " +
                      "Note: The String.isValidID(string) function can be used to test whether a string " +
                      "is a valid identifier.";
            if (caller != null) {
                var callerID = isc.isAn.Instance(caller)
                        // For instances, rely on our standard toString() to indicate
                        // caller class and ID.
                        ? "" + caller
                        : isc.Log.echoLeaf(caller);
                message += " This field applied to " + caller + ".";
            }
            if (isc.Canvas._numInvalidFieldNameWarningsShown == isc.Canvas.maxNumInvalidFieldNameWarnings) {
                message += " This will be the last warning.";
            }
            isc.logWarn(message);
        }
    }
},

_makeRuleScopeMetaFieldName : function (fieldName) {
    if (fieldName == null) return null;
    if (!fieldName.contains(".")) return isc.Canvas._ruleScopeMetaFieldNamePrefix + fieldName;
    var lastDot = fieldName.lastIndexOf(".") + 1,
        leader = fieldName.substring(0,lastDot),
        trailer = fieldName.substring(lastDot)
    ;
    return leader + 
        (!trailer.startsWith(isc.Canvas._ruleScopeMetaFieldNamePrefix) ? isc.Canvas._ruleScopeMetaFieldNamePrefix : "") +
        trailer;
},

// Returns the preferred owner from two components for contributing to ruleContext.
// For any collision, an editable display (such as a form or editable grid) wins over a
// static display (such as a non-editable grid with a selection). Hidden components have
// lowest priority even if editable. For two editable components the first becomes
// the owner.
getRuleContextPreferredOwnerID : function (owner, dbc) {
    // Prefer visible component
    if (owner.isVisible && !owner.isVisible() && dbc.isVisible && dbc.isVisible()) {
        return dbc.getID();
    }

    // Prefer editable component
    // This is harder than it seems because there are multiple ways to
    // disable editing on a component and no single way to check it.
    var isEditable = function (component) {
        if (component.isDisabled()) return false;
        if (isc.isA.DynamicForm(component)) {
            // A form is editable unless canEdit:false
            return (component.canEdit != false); 
        } else if (isc.isA.ListGrid(component)) {
            // A grid is editable if canEdit:true
            return component.canEdit; 
        }
        return false;
    };
    if (!isEditable(owner) && isEditable(dbc)) {
        return dbc.getID();
    }
    // New dbc is not preferred from an editibility standpoint.
    // Prefer a DynamicForm over other components.
    if (!isc.isA.DynamicForm(owner) && isc.isA.DynamicForm(dbc)) {
        return dbc.getID();
    }

    return owner.getID();
},

// Get source for ruleContext contribution from component.
// "values" for a DynamicForm, "selectedRecord" from grid, etc.
getRuleScopeSourceFromComponent : function (component) {
    for (var className in isc.Canvas._dbcTypeDetails) {
        if (component.isA(className)) {
            return isc.Canvas._dbcTypeDetails[className].criteriaBasePathSuffix;
        }
    }
    return null;
},

getFieldImageDimensions : function (field, record) {
    var width, height;

    // if any of field.imageWidth/Height/Size are set as strings, assume they are property
    // names on the record
    var imageWidthProperty, imageHeightProperty, imageSizeProperty;
    if (isc.isA.String(field.imageWidth)) {
        imageWidthProperty = field.imageWidth;
    } else {
        width = field.imageWidth;
    }
    if (isc.isA.String(field.imageHeight)) {
        imageHeightProperty = field.imageHeight;
    } else {
        height = field.imageHeight;
    }
    if (isc.isA.String(field.imageSize)) {
        imageSizeProperty = field.imageSize;
    } else {
        width = width || field.imageSize;
        height = height || field.imageSize;
    }

    if (record != null) {
        width = width || record[imageWidthProperty] || record[imageSizeProperty];
        height = height || record[imageHeightProperty] || record[imageSizeProperty];
    }

    return { width: width, height: height };
},

// Generic values management for mapping fieldNames or dataPaths to values within a values object.
// Implemented at the DBC level as static methods as this is used across dataBoundComponents,
// and also by ValuesManager.  NOTE: refactored the three methods _get, _save and _clear so 
// that they all work via the common method _performActionOnValue.  This isn't a perfect 
// factoring, but it avoids triplication of the code required to navigate a dataPath to
// locate the value to operate on (refactored at the same time that navigation code was made
// rather more complicated, to cope with lists-within-lists)
// Params:
// action - one of 'get', 'clear', 'save', or 'hasValue'
// fieldName - field name or dataPath pointing to the entry
// field - field object - may be null. If present used to extract type info for custom
//         simple type data manipulation
// values - values object we're acting upon
// component - component containing this values object. Used to customize some behavior
//             with valuesManagers as documented inline
// useFirstEntryImplicitly - boolean flag indicating if we're getting an object and it's
//             embedded in an array but either:
//             - we're expecting a single object, or 
//             - we're expecting to find a a valid selection in a bound selectionComponent, 
//               and there isn't one
//             then we should just use the first entry.  Note, this is accomplished by calling
//             the static method isc.Canvas.deriveImplicitArrayEntry, the default impl of which
//             simply returns 0.  It is possible to achieve more sophisticated list entry 
//             derivation by overriding this method.
// value - new value (if action is "set")
// skipExtraCheck - flag to avoid an endless loop 

_performActionOnValue : function(action, fieldName, field,
                                values, component, 
                                 useFirstEntryImplicitly, value, skipExtraCheck, 
                                 record, reason)
{
    if (!values || fieldName == null || isc.isAn.emptyString(fieldName)) return;

    //this.logWarn("_performActionOnValue with fieldName: " + isc.echoFull(fieldName));

    var originalValues = values;

    var separator = this._$slash;
    var isDataPath = fieldName.contains(separator);
    
    if (!isDataPath) {
        separator = this._$dot;
        isDataPath = fieldName.contains(separator);
    }
    if (isDataPath) {
        fieldName = fieldName.trim(separator);
        var segments = fieldName.split(separator),
            nestedVals = [],
            undef;

        
        // NOTE: The skipExtraCheck flag is to ensure that we don't go into an endless 
        // recursion loop in the case where the dataPath and the supplied value just don't 
        // match
        if (segments[0] && values[segments[0]] === undef && 
            (action == "get" || action == "hasValue") && !skipExtraCheck)
        {
            if (component && isc.ValuesManager && isc.isA.ValuesManager(component.valuesManager))
            {
                return this._performActionOnValue(action, fieldName, field,
                                                  component.valuesManager.getValues(),
                                                  component,
                                                  useFirstEntryImplicitly, value, true,
                                                  record || values, reason);
            }
        }
        if (isc.isAn.emptyString(segments.last())) segments.length -= 1;
        for (var i = 0; i < segments.length; i++) {
            if (isc.isAn.emptyString(segments[i])) continue;
            // handle the case where we don't have a nested value for this path
            if (values == null) {
                nestedVals.length = 0;
                break;
            }
            nestedVals.add(values);
            // If we've reached the end of the path, act upon the result.
            if (i == segments.length-1) {
                if (action == "get") {
                    if (field != null && field.type != null) {
                        var simpleType = isc.SimpleType.getType(field.type);
                        if (simpleType && simpleType.getAtomicValue && 
                                values[segments[i]] !== undef) 
                        {
                            return simpleType.getAtomicValue(values[segments[i]], reason);
                        }
                    }
                    return values[segments[i]];
                } else if (action == "clear") {
                    delete values[segments[i]];
                } else if (action == "save") {
                    if (field != null && field.type != null) {
                        var simpleType = isc.SimpleType.getType(field.type);
                        if (simpleType && simpleType.updateAtomicValue) {
                            var updateResult = simpleType.updateAtomicValue(value, values[segments[i]], reason);
                            if (updateResult != null) return updateResult;
                        }
                    }

                    values[segments[i]] = value;
                } else if (action == "hasValue") {
                    return segments[i] in values;
                }

            // Otherwise reach into the next nested object, then continue in the for-loop
            } else {
                var newValues = values[segments[i]];

                // If the value is undefined, we have been asked to get or set a dataPath
                // that doesn't exist.  For get or clear, just return null.  For save, we need
                // to build up the missing dataPath as we go.
                if (newValues == undef) {
                    if (action == "get") {
                        return undef;
                    } else if (action == "clear") {
                        return;
                    } else if (action == "save") {
                        newValues = values[segments[i]] = {};
                    } else if (action == "hasValue") {
                        return false;
                    }
                }
                values = newValues;

                // If the value is an array, we are about to traverse a multiple: true part of
                // the object hierarchy, so we need to decide which of the multiple items to 
                // follow
                if (isc.isAn.Array(values)) {
                    var index = null;
                    // If the next element in the dataPath is an index, use that
                    var nextIsIndex = (parseInt(segments[i+1]) == segments[i+1]);
                    if (nextIsIndex) {
                        index = parseInt(segments[i+1]);
                        segments.removeAt(i+1);
                    // Otherwise, try to derive a selected record from the chain of 
                    // selectionComponents
                    } else if (component && component.selectionComponent) {
                        var path = separator,
                            selComponent = component,
                            foundNullSelection;
                        for (var j = 0; j <= i; j++) {
                            path += segments[j] + separator;
                        }
                        path = path.trim(separator);
                        selComponent = component.selectionComponent;
                        while (selComponent) {
                            var componentPath = selComponent.dataPath;
                            if (componentPath) componentPath = componentPath.trim(separator);
                            if (path == componentPath) {
                                var sel = selComponent.getSelectedRecord();
                                if (sel) {
                                    
                                    for (var k = 0; k < values.length; k++) {
                                        if (sel == values[k]) {
                                            index = k;
                                            break;
                                        }
                                    }
                                    if (index == null) {
                                        // ASSERT: This should never happen
                                        isc.logWarn("At dataPath " + fieldName + ", there was " +
                                            "a selectionComponent with a valid selected record " +
                                            "but we could not find that record in the VM's " +
                                            "data.  Falling back to row derivation behavior");
                                    }
                                } else {
                                    // If we get here, we have found a valid path through the 
                                    // data model, based on a selectionComponent, but 
                                    // currently there is nothing selected in that component.
                                    // In this case, we want downstream components to reflect
                                    // that by containing nothing; it's only if we couldn't
                                    // find a valid path through the data model that we fall 
                                    // back to using the first record
                                    foundNullSelection = true;
                                }
                                break;
                            }
                            selComponent = selComponent.selectionComponent;
                        }

                        if (index == null) {
                            if (!foundNullSelection && useFirstEntryImplicitly) {
                                index = isc.Canvas.deriveImplicitArrayEntry(action, fieldName, 
                                            field, values, component, segments, i, record);
                            } else {
                                return;
                            }
                        }
                    } else {
                        // Nothing else for it...
                        if (useFirstEntryImplicitly) {
                            index = 0;
                        } else {
                            return;
                        }
                    }
                    values = values[index];
                }
            }
        }
        // In clear mode, if we have a nested values object like this:
        //  {foo:{ moo: {zoo:"a"} } }
        // in addition to deleting the zoo attribute from the moo object we may as well clear up
        // the empty object stored under foo.moo
        if (action == "clear") {
            for (var i = nestedVals.length-1; i > 0; i--) {            
                if (isc.isAn.emptyObject(nestedVals[i])) {
                    delete nestedVals[i-1][segments[i-1]];
                }
            }
        }
    } else {
        if (action == "get") {
            if (field != null && field.type != null) {
                var simpleType = isc.SimpleType.getType(field.type);
                if (simpleType && simpleType.getAtomicValue  && 
                     values[fieldName] !== undef) 
                {
                    return simpleType.getAtomicValue(values[fieldName], reason);
                }
            }
            return values[fieldName];
        } else if (action == "clear") {
            delete values[fieldName];
        } else if (action == "save") {
            if (field != null && field.type != null) {
                var simpleType = isc.SimpleType.getType(field.type);
                if (simpleType && simpleType.updateAtomicValue) {
                    var updateResult = simpleType.updateAtomicValue(value, values[fieldName], reason);
                    if (updateResult != null) return updateResult;
                }
            }

            values[fieldName] = value;
        } else if (action == "hasValue") {
            return fieldName in values;
        }
    }
},

deriveImplicitArrayEntry : function(action, fieldName, field, values, component, segments, i, record) {
    return 0;
},

// _clearValue
// Clears the value for some field from a values object
// Handles datapath / nested values

_$slash:"/",
_$dot:".",
_clearFieldValue : function (field, values, component, useFirstEntryImplicitly) 
{
    var dataPath = field;
    if (isc.isAn.Object(field) && !isc.isA.Date(field)) {
        dataPath = this._getDataPathFromField(field, component);
    } else {
        // If we were passed a string, don't pass it into pAOV as the field param - otherwise
        // that method would have to check for the param being a non-null non-object.
        field = null;
    }
    this._performActionOnValue("clear", dataPath, field, values, component, useFirstEntryImplicitly,
            null, false);
},

// _saveValue
// Updates some values object with a new field value.

_saveFieldValue : function (dataPath, field, value, values, component, useFirstEntryImplicitly, reason) {
    // if dataPath wasn't explicitly passed, pick it up from the field object
    if (dataPath == null && field != null) dataPath = this._getDataPathFromField(field, component);
    
    this._performActionOnValue("save", dataPath, field, values, component, useFirstEntryImplicitly,
        value, false, null, reason);
    
    return values;
},

_getDataPathFromField : function (field, component) {
    var dataPath;
    if (field.dataPath) {
        dataPath = field.dataPath;
        
        if (component) {
            dataPath = this._trimDataPath(field.dataPath, component);
        }
    } else {
        dataPath = field.name;
    }
    return dataPath;
},

// _getFieldValue() retrieves a field value from some values object
// handles being passed a datapath to navigate nested values objects
// We also pass in the component that is bound to / trying to bind to the value in question.
// This is necessary in the case of lists-within-lists, because such data structures can 
// only sensibly be said to have a value if we know which of the items in the outer list 
// is currently selected - passing in the component allows us to walk up its chain of 
// selectionComponents (assuming item 0 where there is none)
_getFieldValue : function (dataPath, field, values, component, useFirstEntryImplicitly, reason) 
{
    // if dataPath wasn't explicitly passed, pick it up from the field object
    if (dataPath == null && field != null) dataPath = this._getDataPathFromField(field, component);
    return this._performActionOnValue("get", dataPath, field, values, component,
            useFirstEntryImplicitly, null, false, null, reason);
},

_fieldHasValue : function (dataPath, field, values, component, useFirstEntryImplicitly, reason)
{
    // if dataPath wasn't explicitly passed, pick it up from the field object
    if (dataPath == null && field != null) dataPath = this._getDataPathFromField(field, component);
    return this._performActionOnValue("hasValue", dataPath, field, values, component,
            useFirstEntryImplicitly, null, false, null, reason);
},

// This method trims the component's dataPath off of the field, if the field dataPath reiterates
// it.  This puts the field dataPath in the correct context, since this ListGrid's data member
// will be the List of records corresponding to a multiple:true subfield of the overall data
// structure, NOT the overall data structure itself
_trimDataPath : function (dataPathParam, component) {
    if (!dataPathParam) return dataPathParam;
    var dataPath = dataPathParam.trim(isc.Canvas._$slash);
    if (!dataPath.contains(isc.Canvas._$slash)) return dataPathParam;
    var compDataPath = component.getFullDataPath();
    if (compDataPath == null || compDataPath == "") return dataPath;
    compDataPath = compDataPath.trim(isc.Canvas._$slash);
    var left = compDataPath.split(isc.Canvas._$slash);
    var right = dataPath.split(isc.Canvas._$slash);
    for (var i = 0; i < left.length; i++) {
        if (left[i] != right[i]) {
            break;
        }
    }

    if (i == 0) return dataPathParam;

    var trimmedDataPath = "";
    for (var j = i; j < right.length; j++) {
        trimmedDataPath += right[j];
        trimmedDataPath += "/";
    }
    // Remove the trailing slash from the return value
    return trimmedDataPath.substring(0, trimmedDataPath.length-1);
},

_combineDataPaths : function (baseDP, dp) {

    if (baseDP == null && dp == null) return null;

    // If dp starts with a slash, it is absolute and standalone
    if (isc.isA.String(dp) && dp.startsWith(this._$slash)) return dp;
    
    // if either param is empty just typecast the other to a string (may be required for
    // index within an array) and return!
    if (baseDP == null) return "" + dp;
    if (dp == null) return baseDP + "";
    
    if (isc.isA.String(baseDP) && baseDP.endsWith(this._$slash)) {
        return baseDP + dp;
    } else {
        return baseDP + this._$slash + dp;
    }
},

//> @attr dataBoundComponent.deepCloneOnEdit (Boolean : null : IRWA)
// Before we start editing values in this DataBoundComponent, should we perform a deep clone 
// of the underlying values.  See +link{dataSource.deepCloneOnEdit} for details of what this means.
// <p>
// If this value is not explicitly set, it defaults to the value of +link{dataSource.deepCloneOnEdit}.
// This value can be overridden per-field with +link{dataSourceField.deepCloneOnEdit}.
// <p>
// Like the other <code>deepCloneOnEdit</code> settings, this flag only has an effect if you are 
// editing a values object that contains nested objects or arrays, using 
// +link{Canvas.dataPath,dataPath}s.
//
// @see canvas.dataPath
// @see formItem.dataPath
// @see dataSourceField.deepCloneOnEdit
// @see dataSource.deepCloneOnEdit
// @visibility external
//< 

// _duplicateValues(): Take a values object and duplicate it
// This is a recursive duplication following dataPaths to duplicate nested objects.
// We do this when we start editing a record in DF or VM. 
// This means that when the user makes changes to the values for items we don't update the
// original values object passed in directly.

// Params:
// - component - component doing the editing - DynamicForm or ValuesManager.  This can be 
//               null, to allow safe recursive duplication of data structures that are not
//               currently linked to a component
// - values - record to duplicate
// - targetVals - object to drop the cloned values into - basically an empty object that the
//   calling code would apply as this.values or this._oldValues or whatever
// - defaultPaths - array of strings.
//   Used when "rememberValues" is running -- for each value in the
//   values object passed in, test whether its actually set to the default for some item in the
//   edit-component. If so hang onto the dataPath for this item in the "defaultPaths" array.
//   If defaultPaths is not passed in we don't hang onto this information.


_duplicateValues : function (component, values, targetVals, defaultPaths) {
    var ds = component ? component.getDataSource() : null;
    // no need for a returnVal - we've updated the targetVals object directly.
    this._cloneComponentValues(component, targetVals, values, ds, null, defaultPaths);
},

_cloneComponentValues : function (component, storedValues, values, dataSource, 
                                  dataPath, defaultPaths, dupList) {
                                  
    if (values == null) return;
    var getDefaults = (defaultPaths != null);

    var dsDeepClone = component ? component.deepCloneOnEdit : null;
    if (dsDeepClone == null) dsDeepClone = dataSource ? dataSource.deepCloneOnEdit : null;
    var deepClone = dsDeepClone == null ? 
                        
                        (isc.DataSource ? isc.DataSource.deepCloneOnEdit : dsDeepClone)
                                          : dsDeepClone;

    // handle being passed an array as both the 'stored' object and the 'values' object
    // This allows recursive, safe cloning of arrays
    if (isc.isAn.Array(values)) {
        for (var i = 0; i < values.length; i++) {
            var value = values[i];
            if (isc.isA.Function(value)) continue;
            if (isc.isAn.Instance(values[prop]) || isc.isA.Class(values[prop])) continue;
            
            if (value == null || isc.isA.String(value) || isc.isA.Boolean(value) ||
                isc.isA.Number(value))
            {
                storedValues[storedValues.length] = value;
            
            } else if (isc.isA.Date(value)) {
                storedValues[storedValues.length] = new Date(value.getTime());
                
            } else if (isc.isAn.Object(value)) {
                var targetObj;
                if (isc.isAn.Array(value)) {
                    targetObj = storedValues[storedValues.length] = [];
                } else {
                    targetObj = storedValues[storedValues.length] = {};
                }
                this._cloneComponentValues(
                    component, 
                    targetObj, 
                    value, 
                    dataSource, 
                    (getDefaults ? dataPath : null), 
                    defaultPaths
                );
            }
        }
        return;
    }

    
    if (values._isc_tree != null) {
        values = isc.JSONEncoder._serialize_cleanNode(values);
    }
    
            
    
    var propertiesToSkip = {
        __ref: true,
        __module: true
    };
            
    if (isc.DataSource && isc.DataSource.cloneValuesSafely) {
        if (!dupList) dupList = [];
        if (dupList.contains(values)) {
            storedValues = values;
            return;
        }
        dupList.add(values);
    }

    for (var prop in values) {
        if (isc.isA.Function(values[prop])) continue;

        if (propertiesToSkip[prop] == true) continue;

        
        if (isc.Browser.isSGWT && window.SmartGWT.isNativeJavaObject(values[prop])) continue;
        
        // Skip instances and classes
        
        if (isc.isAn.Instance(values[prop]) || isc.isA.Class(values[prop])) continue;
        
        // When this is running in rememberValues we want to remember defaults
        // Otherwise we can skip the dataPath stuff entirely here
        var fullDataPath;
        if (getDefaults) {
            
            if (dataPath) {
                fullDataPath = dataPath + prop;
            } else {
                fullDataPath = prop;
            }
            
            var item;
            
            if (component && component.getItem) item = component.getItem(fullDataPath);
            if (item && item.isSetToDefaultValue()) {            
                defaultPaths.add(fullDataPath);
            }
        }
            
        // Special case for dates - duplicate them rather than copying the object 
        // across
        var propValue = values[prop];
        if (isc.isA.Date(propValue)) {
            storedValues[prop] = propValue.duplicate();
            
        } else if (isc.isAn.Object(propValue) && !isc.isAn.Array(propValue)) {

                
        
            // Shallow-clone objects that do not have a corresponding DataSourceField.
            var field = dataSource ? dataSource.getField(prop) : null;
            if (!field) {
                storedValues[prop] = values[prop];

            } else {
                // If a field is of a SimpleType with a custom "duplicate" method call it to
                // duplicate the value.
                // This will allow for duplication / editing of "opaque" types where
                // a getAtomicValue / updateAtomicValue method is present.          
                var type = isc.SimpleType.getType(field.type);
                if (type && type.duplicate) {
                    storedValues[prop] = type.duplicate(values[prop]);
                
                // Only deep-clone objects corresponding to fields that have deepCloneOnEdit 
                // set (or inherited from the DataSource or from the static setting on 
                // isc.DataSource)
                } else if (field.deepCloneOnEdit == true || 
                       (field.deepCloneOnEdit == null && deepClone))
                {
                    // If the global DataSource flag "cloneValuesSafely" is set, keep track
                    // of objects we've seen before and shallow clone any duplicates
                    if (isc.DataSource && isc.DataSource.cloneValuesSafely) {
                        if (dupList.contains(propValue)) {
                            storedValues[prop] = values[prop];
                            continue;
                        }
                        dupList.add(propValue);
                    }
                    storedValues[prop] = {};
                    this._cloneComponentValues(component, storedValues[prop], propValue, 
                        isc.DataSource ? isc.DataSource.get(field.type) : null,
                        getDefaults ? (fullDataPath + isc.Canvas._$slash) : null,
                        defaultPaths, dupList);
                } else {
                    storedValues[prop] = values[prop];
                }
            }
                
        
        } else if (isc.isAn.Array(propValue)) {
            var field = dataSource ? dataSource.getField(prop) : null;
            if (!field) {
                storedValues[prop] = values[prop];
            } else {
                // handle an array of opaque SimpleType objects with a custom "duplicate" method
                var type = isc.SimpleType.getType(field.type);
                if (type && type.duplicate) {
                    var newArray = [];
                    for (var i = 0; i < storedValues[prop]; i < storedValues[prop].length) {
                        newArray[i] = type.duplicate(storedValues[prop][i]);
                    }

                } else if (field.deepCloneOnEdit == true || 
                       (field.deepCloneOnEdit == null && deepClone))
                {
                    if (isc.DataSource && isc.DataSource.cloneValuesSafely) {
                        if (dupList.contains(propValue)) {
                            storedValues[prop] = values[prop];
                            continue;
                        }
                        dupList.add(propValue);
                    }

                    storedValues[prop] = [];
                    this._cloneComponentValues(component, storedValues[prop], propValue, 
                        isc.DataSource ? isc.DataSource.get(field.type) : null,
                        getDefaults ? (fullDataPath + isc.Canvas._$slash) : null,
                        defaultPaths, dupList);
                } else {
                    storedValues[prop] = values[prop];
                }
            }
        } else {
            storedValues[prop] = values[prop];
        }
    }
},

// evalViewState
// defined at the class level so we can call it from anywhere (Used in ResultTree)
evalViewState : function (state, stateName, suppressWarning, target) {
    //!OBFUSCATEOK    
    if (isc.isA.String(state)) {
        var origState = state;
        try {
            state = isc.eval(state);
        } catch (e) {
            if (!suppressWarning) {
                var warning = "Unable to parse " + stateName + " object passed in: " + 
                              isc.Log.echo(origState) + " Ignoring."
                if (!target || target.logWarn == null) {
                    if (target) warning += " [target:" + isc.Log.echo(target) + "]";
                    this.logWarn(warning);
                } else {
                    target.logWarn(warning);
                }
            }
            return;
        }
    }
    return state;
},

// Format a number, 'value', as a String.  This method implements formatting
// for data source fields of type "float" or "integer", considering the specified
// precision property set on the field.
// @param value (any) the value to be formatted
// @param [precision] (number) an integer to limit the number of significant digits
// @param [type] (string) type of the field
// @param [dontUseDefault] (boolean) whether to return null if `value` is not specified or
// return a formatted, default value
// @see dataSourceField.precision
getNumberValueAsString : function (value, precision, type, dontUseDefault) {
    // Assign default values to the arguments if they are invalid
    if (!isc.isA.Number(value)) {
        if (dontUseDefault) {
            return null;
        }
        value = isc.DataSource._getDefaultValueForFieldType(type);
    }
    if (!isc.isA.Number(precision) || precision <= 0) {
        precision = null;
    }
    if (precision != null) {
        precision = this._filterFieldValueAndWarn(precision, 1, 21, "precision");
        
        return isc.NumberUtil._expandExponent(value.toPrecision(precision));
    } else {
        return isc.DataSource._getTypedValueAsString(value, type);
    }
},

// Format a floating point number, `value`, as a string.  This method implements formatting
// for data source fields of type "float", considering the specified decimalPrecision and
// decimalPad properties set on the field.
// @param value (any) the value to be formatted
// @param [decimalPrecision] (number) an integer to limit the number of decimal digits
// @param [decimalPad] (number) an integer to fix the number of decimal digits
// @param [dontUseDefault] (boolean) whether to return null if `value` is not specified or
// return a formatted, default value
// @see dataSourceField.decimalPrecision
// @see dataSourceField.decimalPad
getFloatValueAsString : function (value, decimalPrecision, decimalPad, dontUseDefault) {
    // Assign default values to the arguments if they are invalid
    if (!isc.isA.Number(value)) {
        if (dontUseDefault) {
            return null;
        }
        value = isc.DataSource._getDefaultValueForFieldType("float");
    }
    if (!isc.isA.Number(decimalPrecision) || decimalPrecision < 0) {
        decimalPrecision = null;
    }
    if (!isc.isA.Number(decimalPad) || decimalPad < 0) {
        decimalPad = null;
    }

    if (decimalPrecision != null && decimalPad != null) {
        // Pad to the shorter of decimalPrecision and decimalPad if they are both
        // specified.
        if (decimalPrecision < decimalPad) {
            decimalPad = decimalPrecision;
        }
        decimalPrecision = null;
    }

    if (decimalPad != null) {
        decimalPad = this._filterFieldValueAndWarn(decimalPad, 0, 20, "decimalPad");
        return value.toFixed(decimalPad);
    } else if (decimalPrecision != null) {
        var pow10 = Math.pow(10, decimalPrecision);
        var roundedValue = Math.round(value * pow10)/pow10;
        return isc.DataSource._getTypedValueAsString(roundedValue, "float");
    } else {
        return isc.DataSource._getTypedValueAsString(value, "float");
    }
},

_filterFieldValueAndWarn : function (value, min, max, fieldName) { 
    var limit;
    if      (value < min) limit = min;
    else if (value > max) limit = max;
    else return value;
    this.logWarn("Ignoring invalid value " + value + " for " + fieldName);
    return limit;
},

// Returns a list of DataSources from the ruleScope component. Uses
// DS of DBC which is databound and auto-generates a DS for non-databound
// components. The DS or auto-gen'd DS for the targetRuleScope component
// is excluded.
getRuleScopeDataSources : function (targetRuleScope) {
    if (!targetRuleScope) return [];
    targetRuleScope = (isc.isA.String(targetRuleScope) ? window[targetRuleScope] : targetRuleScope);
    if (!targetRuleScope.getRuleScopeDataBoundComponents) return [];
    var dataSources = [],
        dbcList = targetRuleScope.getRuleScopeDataBoundComponents()
    ;
    for (var i = 0; i < dbcList.length; i++) {
        if (dbcList[i] == targetRuleScope) {
            continue;
        }
        if (dbcList[i].dataSource) {
            if (!dataSources.contains(dbcList[i].dataSource)) {
                dataSources.add(dbcList[i].dataSource);
            }
        }
        if (dbcList[i] != targetRuleScope) {
            // Auto-generate
            dataSources.add(dbcList[i].makeDataSourceFromFields(dbcList[i].ID));
        }
    }
    return dataSources;
},

// Same as getRuleScopeDataSource except targetRuleScope component DS
// is included in list.
getAllRuleScopeDataSources : function (targetRuleScope) {
    var currentForm = (isc.isA.String(targetRuleScope) ? window[targetRuleScope] : targetRuleScope),
        currentFormDS = (currentForm.getDataSource ? currentForm.getDataSource() : null)
    ;
    if (!currentFormDS && isc.isA.DataBoundComponent(currentForm)) {
        currentFormDS = currentForm.makeDataSourceFromFields();
    }

    var dataSources = isc.Canvas.getRuleScopeDataSources(currentForm);
    if (currentFormDS) {
        dataSources.addAt(currentFormDS, 0);
    }
    return dataSources;
},

// Returns map of ruleScope dataSources->ownerID. Applies same rules
// for choosing an owner from conflicting contributors as writing to
// ruleContext.
getRuleScopeDataSourceOwners : function (targetRuleScope) {
    if (!targetRuleScope) return {};
    targetRuleScope = (isc.isA.String(targetRuleScope) ? window[targetRuleScope] : targetRuleScope);
    if (!targetRuleScope.getRuleScopeDataBoundComponents) return {};
    var dataSources = [],
        dbcList = targetRuleScope.getRuleScopeDataBoundComponents(),
        owners = {}
    ;
    for (var i = 0; i < dbcList.length; i++) {
        if (dbcList[i] != targetRuleScope && dbcList[i].dataSource) {
            var dbc = dbcList[i],
                dbcID = dbc.getID(),
                owner = owners[dbc.dataSource.ID]
            ;
            if (owner) {
                // Have seen this DS before. Resolve conflict to identify which DBC is the owner
                if (isc.Canvas.getRuleContextPreferredOwnerID(owner, dbc) == dbcID) {
                    owners[dbc.dataSource.ID] = dbc;
                }
            } else {
                // Initial owner
                owners[dbc.dataSource.ID] = dbc;
            }
        }
    }
    return owners;
}


});

isc.Canvas.addProperties({

_resolveEmptyDisplayValue : function (field) {
    
    var emptyVal = field.emptyCellValue;
    if (emptyVal == null) emptyVal = field.emptyDisplayValue;
    
    // Back off to the empty value for the widget as a whole
    if (emptyVal == null) emptyVal = this.emptyCellValue;
  
    return emptyVal;
},

//>	@attr dataBoundComponent.dataSource		(DataSource or ID : null : IRW)
// The DataSource that this component should bind to for default fields and for performing
// +link{DSRequest,DataSource requests}.
// <P>
// Can be specified as either a DataSource instance or the String ID of a DataSource.
//
// @group databinding
// @visibility external
// @example dataSourceFields
//<										

//> @attr dataBoundComponent.dataFetchMode (FetchMode : "paged" : IR)
// How to fetch and manage records retrieve from the server.  See +link{type:FetchMode}.
// <P>
// This setting only applies to the +link{ResultSet} automatically created by calling
// +link{listGrid.fetchData,fetchData()}.  If a pre-existing ResultSet is passed to setData() instead, it's
// existing setting for +link{resultSet.fetchMode} applies.
//
// @group databinding
// @visibility external
//< 

//> @attr dataBoundComponent.dataPageSize (integer : null : IRW)
// When using +link{dataFetchMode,data paging}, how many records to fetch at a time.  If set to
// a positive integer, <code>dataPageSize</code> will override the default 
// +link{resultSet.resultSize,resultSize} for ResultSets automatically created when you call
// +link{fetchData()} (and similarly for the +link{resultTree.resultSize,resultSize} of
// ResultTrees).  Leaving <code>dataPageSize</code> at its default means to just use the default page
// size of the data container.
// <P>
// <b>Note</b> that regardless of the <code>dataPageSize</code> setting, a component will always fetch
// all of data that it needs to draw.  Settings such as
// +link{listGrid.showAllRecords,showAllRecords:true},
// +link{listGrid.drawAllMaxCells,drawAllMaxCells} and
// +link{listGrid.drawAheadRatio,drawAheadRatio} can cause more rows than the configured
// <code>dataPageSize</code> to be fetched.
//
// @group databinding
// @see ResultSet.resultSize
// @visibility external
//< 

//>	@attr dataBoundComponent.fields            (Array of Field : null : IRW)
// A DataBoundComponent manipulates records with one or more fields, and
// <code>component.fields</code> tells the DataBoundComponent which fields to present, in what
// order, and how to present each field.
// <p>
// When both <code>component.fields</code> and 
// <code>+link{dataBoundComponent.dataSource,component.dataSource}</code> are set,
// any fields in <code>component.fields</code> with the same name as a DataSource field
// inherit properties of the DataSource field.  This allows you to centralize data model
// information in the DataSource, but customize presentation of DataSource fields on a
// per-component basic.  For example, in a ListGrid, a shorter title or format for a field
// might be chosen to save space.
// <p>
// By default, only fields specified on the component are shown, in the order specified on
// the component.  The +link{useAllDataSourceFields} flag can be set to show all fields
// from the DataSource, with <code>component.fields</code> acting as field-by-field
// overrides and/or additional fields.
// <p>
// If a DataBoundComponent is given a DataSource, but no <code>component.fields</code>, the
// "default binding" is used: fields are shown in DataSource order, according
// to the properties <code>+link{showHiddenFields}</code> and 
// <code>+link{showDetailFields}</code>.
//
// @group databinding
// @visibility external
// @example mergedFields
// @example validationFieldBinding
//<

//>	@attr dataBoundComponent.useAllDataSourceFields		(boolean : false : IRW)
// If true, the set of fields given by the "default binding" (see 
// +link{attr:DataBoundComponent.fields}) is used, with any fields specified in
// <code>component.fields</code> acting as overrides that can suppress or modify the
// display of individual fields, without having to list the entire set of fields that
// should be shown.
// <P>
// If <code>component.fields</code> contains fields that are not found in the DataSource,
// they will be shown after the most recently referred to DataSource field.  If the new
// fields appear first, they will be shown first.
// <P>
// +explorerExample{validationFieldBinding,This example} shows a mixture of component
// fields and DataSource fields, and how they interact for validation.
// <P>
// This setting may be cleared if a +link{FieldPicker} is used to edit the component's field
// order.
//
// @group databinding
// @visibility external
// @example validationFieldBinding
// @see fieldPicker.dataBoundComponent
//<

//>	@attr dataBoundComponent.showHiddenFields (boolean : false : IRW)
// Whether to show fields marked <code>hidden:true</code> when a DataBoundComponent is given a
// DataSource but no <code>component.fields</code>.
// <p>
// The <code>hidden</code> property is used on DataSource fields to mark fields that are
// never of meaning to an end user.
//  
// @group databinding
// @visibility external
//<

//>	@attr dataBoundComponent.showDetailFields (boolean : false : IRW)
// Whether to show fields marked <code>detail:true</code> when a DataBoundComponent is 
// given a DataSource but no <code>component.fields</code>.
// <p>
// The <code>detail</code> property is used on DataSource fields to mark fields that 
// shouldn't appear by default in a view that tries to show many records in a small space.
// 
// @group databinding
// @visibility external
//<

//>	@attr dataBoundComponent.showComplexFields (boolean : true : IRWA)
// Whether to show fields of non-atomic types when a DataBoundComponent is given a
// DataSource but no <code>component.fields</code>.
// <p>
// If true, the component will show fields that declare a complex type, for example, a
// field 'shippingAddress' that declares type 'Address', where 'Address' is the ID of a
// DataSource that declares the fields of a shipping address (city, street name, etc).
// <P>
// Such fields may need custom formatters or editors in order to create a usable interface,
// for example, an Address field in a ListGrid might use a custom formatter to combine the
// relevant fields of an address into one column, and might use a pop-up dialog for
// editing.
// 
// @group databinding
// @visibility external
//<
showComplexFields:true,

//>	@attr dataBoundComponent.fetchOperation (String : null : IRW)
// +link{dsRequest.operationId,operationId} this component should use when performing fetch operations.
// @group operations
// @visibility external
//<
setFetchOperation : function(operationId) {
    this.fetchOperation = operationId;
    // This is specific to DynamicForm
    if (this._propagateOperationsToFileItem) this._propagateOperationsToFileItem();
},

//>	@attr dataBoundComponent.updateOperation (String : null : IRW)
// +link{dsRequest.operationId,operationId} this component should use when performing update operations.
// @group operations
// @visibility external
//<
setUpdateOperation : function(operationId) {
    this.updateOperation = operationId;
    if (this._propagateOperationsToFileItem) this._propagateOperationsToFileItem();
},

//>	@attr dataBoundComponent.addOperation (String : null : IRW)
// +link{dsRequest.operationId,operationId} this component should use when performing add operations.
// @group operations
// @visibility external
//<
setAddOperation : function(operationId) {
    this.addOperation = operationId;
    if (this._propagateOperationsToFileItem) this._propagateOperationsToFileItem();
},

//>	@attr dataBoundComponent.removeOperation (String : null : IRW)
// +link{dsRequest.operationId,operationId} this component should use when performing remove operations.
// @group operations
// @visibility external
//<
setRemoveOperation : function(operationId) {
    this.removeOperation = operationId;
    if (this._propagateOperationsToFileItem) this._propagateOperationsToFileItem();
},

//> @attr dataBoundComponent.validateOperation (String : null : IRW)
// +link{dsRequest.operationId,operationId} this component should use when performing validate operations.
// @group operations
//<

//> @attr dataBoundComponent.exportFields (Array of String : null : IRW)
// The list of field-names to export.  If provided, the field-list in the exported output is 
// limited and sorted as per the list.
// <P>
// If exportFields is not provided, the exported output includes all visible fields 
// from this component, sorted as they appear.
//
// @visibility external
//<

//> @attr dataBoundComponent.exportAll (boolean : false : IRW)
// Setting exportAll to true prevents the component from passing it's list of fields to the 
// export call.  The result is the export of all visible fields from +link{dataSource.fields}.
// <P>
// If exportAll is false, an export operation will first consider 
// +link{dataBoundComponent.exportFields}, if it's set, and fall back on all visible fields from
// +link{dataSource.fields} otherwise.
//
// @visibility external
//<

//> @attr dataBoundComponent.exportIncludeSummaries (boolean : true : IRW)
// If Summary rows exist for this component, whether to include them when exporting client data.
//
// @visibility external
//<
exportIncludeSummaries: true,


ignoreEmptyCriteria: true,

//> @type RecategorizeMode
// Type for controlling when a "recategorize" is applied to records being dropped
// on a databound component from another databound component.
//
// @value "always" recategorize is always applied
// @value "checked" recategorize if normal checks pass
// @value "never" never recategorize
// @visibility external
//<

//> @attr dataBoundComponent.dragRecategorize (RecategorizeMode : "checked" : IRW)
// Flag controlling when to recategorize records being dropped on a databound
// component from another databound component.
// @visibility external
//<

dragRecategorize: "checked",

//> @attr dataBoundComponent.preventDuplicates (boolean : null : IR)
// If set, detect and prevent duplicate records from being transferred to this component, either via
// drag and drop or via +link{transferSelectedData()}.  When a duplicate transfer is detected,
// a dialog will appear showing the +link{duplicateDragMessage}.
// <P>
// If the component either does not have a +link{DataSource} or has a DataSource with no
// +link{dataSourceField.primaryKey,primaryKey} declared, duplicate checking is off by
// default.  If duplicate checking is enabled, it looks for an existing record in the dataset
// that has <b>all</b> of the properties of the dragged record, and considers that a duplicate.
// <P>
// For +link{dragDataAction}:"copy" where the target DataSource is related to the source
// DataSource by foreignKey, a duplicate means that the target list, as filtered by the current
// criteria, already has a record whose value for the foreignKey field matches the
// primaryKey of the record being transferred.
// <P>
// For example, consider dragging "employees" to "teams", where "teams" has a field
// "teams.employeeId" which is a foreignKey pointing to "employees.id", and the target
// grid has search criteria causing it to show all the members of one team.  A duplicate -
// adding an employee to the same team twice - is when the target grid's dataset contains an
// record with "employeeId" matching the "id" field of the dropped employee.
// 
// @visibility external
//<

//> @attr dataBoundComponent.duplicateDragMessage (String : "Duplicates not allowed" : IR)
// Message to show when a user attempts to transfer duplicate records into this component, and
// +link{preventDuplicates} is enabled.
// <P>
// If set to null, duplicates will not be reported and the dragged duplicates will not be
// saved.
//
// @group i18nMessages
// @visibility external
//<
duplicateDragMessage: "Duplicates not allowed",

//> @attr dataBoundComponent.showOfflineMessage (boolean : true : [IRW])
// Indicates whether the text of the offlineMessage property should be displayed if no data is
// available because we do not have a suitable offline cache
// @visibility external
// @group offlineGroup
// @see offlineMessage
//<
showOfflineMessage:true,
    

//>	@attr dataBoundComponent.offlineMessage (string : "This data not available while offline" : [IRW])
// Message to display when this DataBoundComponent attempts to load data that is not available
// because the browser is currently offline.  Depending on the component, the message is either
// displayed in the component's body, or in a pop-up warning dialog.
// @example offlineSupport
// @group i18nMessages, offlineGroup
// @visibility external
//<
offlineMessage:"This data not available while offline",

//>	@attr listGrid.offlineMessageStyle (CSSStyleName : "offlineMessage" : [IRW])
// The CSS style name applied to the +link{dataBoundComponent.offlineMessage,offlineMessage} if displayed.
// @group offlineGroup
// @visibility external
//<
offlineMessageStyle:"offlineMessage",

//>	@attr dataBoundComponent.offlineSaveMessage (string : "Data cannot be saved because you are not online" : [IRW])
// Message to display when this DataBoundComponent attempts to save data while the application
// is offline.
// @example offline
// @group i18nMessages, offlineGroup
// @visibility external
//<
offlineSaveMessage:"Data cannot be saved because you are not online",


//>	@attr	dataBoundComponent.addDropValues		(Boolean : true : IRW)
//          Indicates whether to add "drop values" to items dropped on this component, if both 
//          the source and target widgets are databound, either to the same DataSource or 
//          to different DataSources that are related via a foreign key.  "Drop values" are 
//          properties of the dropped item that you wish to change (and persist) as a 
//          result of the item being dropped on this grid.
//          <P>
//          If this value is true and this component is databound, +link{getDropValues()} will 
//          be called for every databound item dropped on this grid, and an update performed
//          on the item
//
//      @group  dragging
//      @visibility external
//      @example listRecategorize
//<
addDropValues: true,

//>	@attr	dataBoundComponent.dropValues		(Object : null : IRWA)
//          When an item is dropped on this component, and +link{addDropValues} is true and both 
//          the source and target widgets are databound, either to the same DataSource or 
//          to different DataSources that are related via a foreign key, this object 
//          provides the "drop values" that SmartClient will apply to the dropped object 
//          before updating it.
//          <P>
//          If this property is not defined, SmartClient defaults to returning the selection
//          criteria currently in place for this component.  Thus, any databound items (for example, 
//          rows from other grids bound to the same DataSource) dropped on the grid will,
//          by default, be subjected to an update that makes them conform to the grid's 
//          current filter criteria.
//
//      @group  dragging
//      @visibility external
//      @example listRecategorize
//<
   


// Property to be used as field identifier on field objects.
// The ID of the field is also the property in each record which holds the value 
// for that field.
fieldIdProperty:"name",


//> @method dataBoundComponent.dragComplete()
// This method is invoked on the source component whenever a drag operation or 
// +link{transferSelectedData()} completes.  This method is called when the entire chain of 
// operations - including, for databound components, server-side updates and subsequent 
// integration of the changes into the client-side cache - has completed.<p>
// There is no default implementation of this method; you are intended to override it if you 
// are interested in being notified when drag operations complete.
//
// @see dropComplete()
// @group  dragging
// @visibility external
//<

//> @method dataBoundComponent.dropComplete()
// This method is invoked whenever a drop operation or +link{transferSelectedData()} 
// targeting this component completes.  A drop is considered to be complete when all the client-
// side transfer operations have finished.  This includes any server turnarounds SmartClient 
// needs to make to check for duplicate records in the target component; it specifically does 
// not include any add or update operations sent to the server for databound components.  If 
// you want to be notified when the entire drag operation - including server updates and cache
// synchronization - has completed, override +link{dataBoundComponent.dragComplete,dragComplete}
// on the source component.<p>
// There is no default implementation of this method; you are intended to override it if you 
// are interested in being notified when drop operations complete.
//
// @param transferredRecords (List of Records) The list of records actually transferred to
//                    this component (note that this is not necessarily the same thing as the
//                    list of records dragged out of the source component because it doesn't
//                    include records that were excluded because of collisions with existing
//                    records)
// @see dragComplete()
// @group  dragging
// @visibility external
//<


//> @type DataPath
// String specifying a nested data field structure.
// <P>
// Each dataPath string is a slash-delimited set of field identifiers, for example
// <code>"id1/id2/id3"</code>. DataPaths may be applied directly to a
// +link{canvas.dataPath,component}, and/or to a databound component field specification.
// A datapath denotes a path to a nested field value in a hierarchical structure, giving
// developers the opportunity to easily view or edit nested data structures.
// Specifically:
// <ul><li>if the component is viewing or editing a record, the value for fields 
//         will be derived from a nested structure of records</li>
//     <li>if the component is bound to a dataSource, field attributes may be picked up by
//         following the dataPath to a field definition on another dataSource</li></ul>
// <b>Examples:</b><br>
// If a dynamicForm is defined with the following fields:
// <pre>
//    [
//      { name:"name" },
//      { name:"street", dataPath:"address/street" }
//    ]
// </pre>
// If the <code>"name"</code> field is set to <i>"Joe Smith"</i> and the <code>"street"</code> field
// is set to <i>"1221 High Street"</i>, when the values for this form are retrieved via a
// <code>getValues()</code> call they will return an object in the following format:
// <pre>
//    {name:"Joe Smith", address:{street:"1221 High Street"}}
// </pre>
// <P>
// For databound components, dataPath also provides a way to pick up field attributes from nested
// dataSources. Given the following dataSource definitions:
// <pre>
//  isc.DataSource.create({
//      ID:"contacts",
//      fields:[
//          {name:"name"},
//          {name:"email"},
//          {name:"organization"},
//          {name:"phone"},
//          {name:"address", type:"Address"}
//      ]
//  });
// 
//  isc.DataSource.create({
//      ID:"Address",
//      fields:[
//          {name:"street"},
//          {name:"city"},
//          {name:"state"},
//          {name:"zip"}
//      ]
//  });
//  </pre>
// and a databound component bound to the 'contacts' dataSource, specifying a field with a dataPath
// of <code>"address/street"</code> would ensure the field attributes were derived from the 
// "street" field of the 'Address' dataSource.
// <P>
// dataPaths are also cumulative. In other words if a component has a specified dataPath, 
// the dataPath of any fields it contains will be appended to that component level path when
// accessing data. For example the following form:
// <pre>
// isc.DynamicForm.create({
//     dataPath:"contact",
//     fields:[
//          {dataPath:"address/email"}
//     ]
// });
// </pre>
// Might be used to edit a data structure similar to this:
// <pre>{contact:{name:'Ed Jones', address:{state:"CA", email:"ed@ed.jones.com"}}}</pre>
// Nested canvases can also have dataPaths specified, which will similarly be combined. See
// the +link{canvas.dataPath} attribute for more information and examples of this.
// @visibility external
//<

//> @attr   DataBoundComponent.dataArity    (string : "multiple" : IRWA)
// Does this component represent singular or multiple "records" objects?
// Options are "multiple" or "single", or "either"
// @visibility external
//<
dataArity:"multiple",

//> @attr   DataBoundComponent.autoTrackSelection (boolean : true : IRWA)
// If set, for dataArity:"single" components bound to a multiple:true field in this ValuesManager
// automatically check for the presence of a dataArity:"multiple" component bound to the same path
// and set this up as the +link{dataBoundComponent.selectionComponent}.  Note that this property only 
// applies to dataArity:"single" components; if you wish to auto-track selections for a 
// component that is ordinarily of dataArity:"either" (for example, DetailViewer), you must 
// explicitly override its dataArity to "single".
// @visibility selectionComponent
//<
autoTrackSelection:true,


//> @attr canvas.valuesManager (ValuesManager : null : IRWA)
// +link{ValuesManager} for managing values displayed in this component.
// If specified at initialization time, this component will be added to the valuesManager via
// +link{valuesManager.addMember()}.
// <P>
// ValuesManagers allow different fields of a single object to be displayed or edited
// across multiple UI components. Given a single values object, a valuesManager will handle
// determining the appropriate field values for its member components and displaying them / 
// responding to edits if the components support this.
// <P>
// Data may be derived simply from the specified fieldNames within the member components, or for
// complex nested data structures can be specified by both component and field-level
// +link{dataPath}.
// <P>
// Note that components may be automatically bound to an existing valuesManager attached to a 
// parent component if dataPath is specified. See +link{canvas.dataPath} for more information.
// Also note that if a databound component has a specified dataSource and dataPath but no specified
// valuesManager object one will be automatically generated as part of the databinding process
// @visibility external
//<


//> @attr DataBoundComponent.progressiveLoading (boolean : null : IRW)
// Indicates whether or not this component will load its data 
// +link{DataSource.progressiveLoading,progressively}.
// @see attr:DataSource.progressiveLoading
// @see attr:ResultSet.progressiveLoading
// @visibility external
// @group progressiveLoading

//<
setProgressiveLoading : function(value) {
    this.progressiveLoading = value;
    if ((isc.ResultSet && isc.isA.ResultSet(this.data)) ||
        (isc.ResultTree && isc.isA.ResultTree(this.data)))
    {
        this.data.progressiveLoading = value;
    }
},


//> @method  canvas.setValuesManager()
// Setter for the +link{canvas.valuesManager} attribute. This method may be called directly at 
// runtime to set the ValuesManager for a component; it has the same effect as calling 
// +link{ValuesManager.addMember()}, passing in this DataBoundComponent.
// @param dataPath (dataPath) new dataPath
// @visibility external
//<
setValuesManager : function(valuesManager) {
    if (valuesManager) valuesManager.addMember(this);
},

// This method is fired as part of setDataPath - it generates an automatic valuesManager if
// necessary based on this.dataSource
initializeValuesManager : function () {
    var vM = this.valuesManager;
    delete this.valuesManager;
    
   if (vM != null) {
        if (isc.ValuesManager == null) {
            this.logWarn("Widget initialized with specified 'valuesManager' property but " +
                "ValuesManager class is not loaded. This functionality requires the " +
                "Forms module.");
            return;
        }
        
        if (isc.isA.ValuesManager(vM)) {
            vM.addMember(this);
        } else if (isc.isA.ValuesManager(window[vM])) {
            window[vM].addMember(this);
            
        // If it's a string, create a new VM with that ID;
        } else if (isc.isA.String(vM)) {
            isc.ValuesManager.create({
                ID:vM,
                dataSource:this.dataSource,
                members:[this]
            });
        } else {
            this.logWarn("Widget initialized with invalid 'valuesManager' property:"
                         + isc.Log.echo(vM) + ", clearing this property out");
        }
    }
},

//> @attr canvas.dataPath (DataPath : null : IRWA)
// A dataPath may be specified on any canvas. This provides a straightforward way to display or
// edit complex nested data.
// <P>
// For components which support displaying or editing data values, (such as +link{DynamicForm} or
// +link{ListGrid} components), the dataPath may be set to specify how the components data is
// accessed. In this case the dataPath essentially specifies a nested object to edit - typically
// a path to a field value within a dataSource record. Note that a ValuesManager will be required
// to handle connecting the dataBoundcomponent to the appropriate sub object. This may be explicitly
// specified on the component, or a parent of the component, or automatically generated
// if a DataSource is specified on either the component or a parent thereof.
// <P>
// To provide a simple example - if a complex object existed with the following format:
// <pre>
// { companyName:"Some Company",
//   address:{    street:"123 Main Street", city:"New York", state:"NY"  }
// }
// </pre>
// a developer could specify a DynamicForm instance with 'dataPath' set to "address" to edit
// the nested address object:
// <pre>
// isc.ValuesManager.create({
//      ID:'vm',
//      values: { companyName:"Some Company",
//              address:{    street:"123 Main Street", city:"New York", state:"NY"  }
//      }
// });
//
// isc.DynamicForm.create({
//      valuesManager:"vm",
//      dataPath:"address",
//      items:[{name:"street"}, {name:"city"}, {name:"state"}]
// });
// </pre>
// If a component is specified with a <code>dataPath</code> attribute but does not have an
// explicitly specified valuesManager, it will check its parent element chain for a specified
// valuesManager and automatically bind to that. This simplifies binding multiple components used
// to view or edit a nested data structure as the valuesManager needs only be defined once at a
// reasonably high level component. Here's an example of this approach:
// <pre>
// isc.ValuesManager.create({
//      ID:'vm',
//      values: { companyName:"Some Company",
//              address:{    street:"123 Main Street", city:"New York", state:"NY"  }
//      }
// });
//
// isc.Layout.create({
//      valuesManager:vm,
//      members:[
//          isc.DynamicForm.create({
//              dataPath:"/",
//              items:[{name:"companyName"}]
//          }),
//          isc.DynamicForm.create({
//              dataPath:"address",
//              items:[{name:"street"}, {name:"city"}, {name:"state"}]
//          })
//      ]
// });
// </pre>
// Note that in this case the valuesManager is specified on a Layout, which has no 'values'
// management behavior of its own, but contains items with a specified dataPath which do. In this
// example you'd see 2 forms allowing editing of the nested data structure.
// <P>
// dataPaths from multiple nested components may also be combined. For example:
// <pre>
// isc.ValuesManager.create({
//      ID:'vm',
//      values: { companyName:"Some Company",
//              address:{    street:"123 Main Street", city:"New York", state:"NY"  }
//              parentCompany:{
//                  companyName:"Some Corporation",
//                  address:{   street:"1 High Street", city:"New York", state:"NY" }
//              }
//      }
// });
//
// isc.Layout.create({
//      valuesManager:vm,
//      members:[
//          isc.DynamicForm.create({
//              dataPath:"/",
//              items:[{name:"companyName"}]
//          }),
//          isc.DynamicForm.create({
//              dataPath:"address",
//              items:[{name:"street"}, {name:"city"}, {name:"state"}]
//          }),
//          isc.Layout.create({
//              dataPath:"parentCompany",
//              members:[
//                  isc.DynamicForm.create({
//                      dataPath:"/",
//                      items:[{name:"companyName", type:"staticText"}]
//                  }),
//                  isc.DetailViewer.create({
//                      dataPath:"address",
//                      fields:[{name:"street", name:"city", name:"state"}]
//                  })
//              ]
//          })
//      ]
// });
// </pre>
// In this example the detailViewer will display data from the <code>parentCompany.address</code>
// object within the base record.
// <P>
// Note that if a component has a specified  dataSource and shows child components with a
// specified dataPath, there is no need to explicitly declare a valuesManager at all. If a component
// with a dataPath has a dataSource, or an ancestor with a dataSource specified, it will, a
// valuesManager will automatically be generated on the higher level component (and be available as
// <code>component.valuesManager</code>).
// @visibility external
//<

//> @method  canvas.setDataPath()
// Setter for the +link{canvas.dataPath} attribute. This method may be called directly at runtime
// to set the dataPath on a component, and will also be re-run automatically whenever a canvas'
// parent changes due to a call to addChild(). This method handles automatically binding
// the component to the appropriate valuesManager if necessary.
// @param dataPath (DataPath) new dataPath
// @visibility external
//<
setDataPath : function (dataPath) {

    this.dataPath = dataPath;
    
    // we run this on every change of widget hierarchy (addChild etc), allowing us to
    // pick up a valuesManager based on a values manager applied at some ancestor widget level.
    // detect true "databound" components by the presence of fields - if we have no fields
    // just bail here
    
    if (this.getFields == null || this.getFields() == null) return;
    
    // clearing dataPath? Disconnect from any dataPath-derived valuesManager, and bail
    if (dataPath == null) {
        delete this._fullDataPath;
        if (this.valuesManager && this._valuesManagerFromDataPath) {
            this.valuesManager.removeMember(this);
            delete this._valuesManagerFromDataPath;
        }
        return;
    }
    
    // If we have a dataSource applied directly to us we don't need to attach ourselves to another
    // valuesManager, etc
    // Note:
    // We support 'cumulative' dataPaths
    // In other words a valuesManager may be defined on a Layout
    // This can contain another layout with a specified dataPath, which in turn contains a form
    // with a specified dataPath.
    // In this case the forms data would be derived from the valuesManager on the top level layout
    // using a full dataPath combined from both the DynamicForm and the Layout's dataPath 
    // Set up this 'fullDataPath' here - retrieved from 'getFullDataPath'
    var fullDataPath;
    var dataPathComponent = this;
    while (dataPathComponent && 
            (!dataPathComponent.valuesManager || dataPathComponent._valuesManagerFromDataPath) &&
             !dataPathComponent.dataSource)
    {
        if (dataPathComponent.dataPath) {
            if (fullDataPath) {
                fullDataPath = isc.Canvas._combineDataPaths(dataPathComponent.dataPath,
                                                            fullDataPath);
            } else {
                fullDataPath = dataPathComponent.dataPath;
            }
        }
        dataPathComponent = dataPathComponent.parentElement;
    }
    this._fullDataPath = fullDataPath;
    // If we have a valuesManager and/or dataSource specified directly on this component
    // no need to attach to another one!
    
    if (dataPathComponent) {
        if (dataPathComponent != this || !this.dataSource) {
            // assertion - the datapathComponent has a valuesManager already, or a dataSource
            // (in which case we can create a new valuesManager automatically)
            if (dataPathComponent.valuesManager == null) {
                dataPathComponent.createDefaultValuesManager();
            }
            // Assert - this component is not bound to a dataSource and should be bound to 
            // the dataSource associated with the valuesManager we're about to add it to
            // (has to be done in this order, or the databinding step wipes out any values
            // that the VM might have assigned to the DBC when it was added)
            var fields = isc.isA.DynamicForm(this) ? this._itemsConfig : this.getFields();
            fields = fields || this.getFields();
            
            
            // The VM may not have been bound to a dataSource yet
            if (dataPathComponent.valuesManager.getDataSource()) {
                var dataSource = dataPathComponent.valuesManager.getDataSource(),
                    dataPath = this._fullDataPath;
                if (dataPath) {
                    dataSource = dataSource.getDataSourceForDataPath(dataPath, true);
                }
                this.setDataSource(dataSource, fields);
            }
            
            // second param ensures the _valuesManagerFromDataPath attr gets set.
            dataPathComponent.valuesManager.addMember(this, true);
        }
    }
},

//> @method canvas.getFullDataPath()
// Returns a fully qualified +link{type:DataPath} for this canvas. This is calculated by combining
// the canvas' specified +link{type:DataPath} with the <code>dataPath</code> of any parent 
// canvases up to whichever canvas has a specified +link{canvas.valuesManager} specified to actually
// manage values from this component.
// @return (DataPath) fully qualified dataPath for this component
// @visibility external
//<
getFullDataPath : function () {
    var thisDP = this._fullDataPath || this.dataPath;
    // If both are undefined and we have a master element (eg, we are a summary row in a ListGrid),
    // return the master's dataPath
    if (!thisDP && this.masterElement) {
        return this.masterElement._fullDataPath || this.masterElement.dataPath;
    }
    return thisDP;
},

buildFieldDataPath : function (componentDataPath, field) {
    var dataPath = field.dataPath || field.name;
    if (componentDataPath != null) {
        dataPath = isc.Canvas._combineDataPaths(componentDataPath, dataPath);
    }
    // Strip any leading slashes off - we need to support them being entered because it 
    // allows users to specify absolute dataPaths, which is important for some use cases, but
    // we don't want to send them to methods that process dataPath strings because they have
    // been written to not expect them.
    return !dataPath ? null : dataPath.replace(/^\/*/, "");
},

createDefaultValuesManager : function (defaultMembers) {
    if (!defaultMembers) defaultMembers = [];
    defaultMembers.add(this);
    
    isc.ValuesManager.create({
        members:defaultMembers,
        ID:this.getID() + "_valuesManager",
        dataSource:this.dataSource
    });
},

// This method trims the component's dataPath off of a field's dataPath, if the field dataPath reiterates
// it.  This puts the field dataPath in the correct context, since this component's data values
// will be the (List of) record(s) corresponding to a subfield of the overall data
// structure, NOT the overall data structure itself
_trimDataPath : function (dataPathParam) {
    return isc.Canvas._trimDataPath(dataPathParam, this);
},

//> @method dataBoundComponent.getDataPathField()
// For a component with a specified +link{DataSource}, find the associated dataSource field object
// from a specified +link{type:DataPath,dataPath}.
// @param dataPath (DataPath) dataPath for which the field definition should be returned.
// @visibility external
//<
getDataPathField : function (dataPath) {
    if (!dataPath) return null;
    var dataSource;
    // We're passed the full dataPath for the field.
    // this component may have a specified dataPath (in which case we'll be bound to the
    // dataSource that represents that). However the field dataPath may be absolute
    // so look at the dataSource on the valuesManager and apply the full dataPath to that
    // rather than looking at our dataSource and applying a partial dataPath.
    
    if (this.valuesManager && this.valuesManager.getDataSource) {
        dataSource = this.valuesManager.getDataSource();
        
    } else if (this.grid && this.grid.valuesManager && this.grid.valuesManager.getDataSource) {
        dataSource = this.grid.valuesManager.getDataSource();
    } else {
        dataSource = this.getDataSource();
    }

    var segments = dataPath.split(isc.slash),
        rtnField;        
    if (!dataSource) return;
    
    for (var i = 0; i < segments.length; i++) {
        var fieldId = segments[i],
        field = dataSource.getField(fieldId);
        dataSource = field ? (dataSource.getSchema(field.type)  || dataSource) : dataSource;
        
        if (field == null) {
            this.logWarn("Unable to find dataSource field matching specified dataPath: '" +
                         dataPath + "'");
            return;
        }
    }
    return field;
},

registerWithDataView : function (dataView) {
    if (!this.inputDataPath) return;
    
    dataView = this.parentElement;
    while (dataView && !isc.isA.DataView(dataView)) dataView = dataView.parentElement;
    
    if (!dataView) {
        this.logWarn("Component initialized with an inputDataPath property, but no DataView " +
                     "was found in the parent hierarchy. inputDataPath is only applicable to " +
                     "DataBoundComponents and FormItems being managed by a DataView");
        return;
    }
    
    dataView.registerItem(this);
},

//>	@method	dataBoundComponent.bindToDataSource()
// Combine component's fields specifications with the fields specifications from the
// datasource the component works with (specified indirectly by component.operation).
// - check if fields property and dataSource property are specified
// - if just dataSource, then use dataSource fields
// - if just fields property, then default behavior
// - if both, then use fields, with each field using defaults of dataSource<br>
//   calls setFields() when finished
//		@group	data
//<
// Extra parameter 'hideExtraDSFields' used by ListGrid for the case where
// useAllDataSourceFields is false but we want to include fields picked up from the DataSource
// but mark them as not visible in the grid. This is used to achieve the
// +link{listGrid.canPickOmittedFields} behavior.
_dateEditorTypes:{date:true,DateItem:true},
bindToDataSource : function (fields, hideExtraDSFields) {

    //this.logWarn("bindToDataSource called with fields " + this.echoLeaf(fields));
    // call 'setDataPath' to ensure if we have a dataPath specified we bind to the correct
    // valuesManager
    if (this.dataPath) this.setDataPath(this.dataPath);
	// Most components operate on a datasource, displaying or otherwise manipulating fields from
	// that datasource.  We don't want to duplicate all the information about a field that is
	// specified in the datasource (type, title, etc) in each component that needs to display
	// that field.  So, we allow the component's field specifications to refer to the datasource
	// field by name, and combine the field specification from the component with the field
	// specification from the datasource.

    // pick up the dataSource of our dataset if it has one and we weren't given one
    if (this.dataSource == null && this.data != null) this.dataSource = this.data.dataSource;

    
    var origFields = this.fields || this.items;
    if (isc.isAn.Array(origFields)) this.originalFields = origFields.duplicate();

	// get the datasource versions of the field specifications.  NOTE: this method may be
    // called in a build that does not include DataSource
	var	ds = this.getDataSource();
    if (ds == null && this.valuesManager && this.valuesManager.getDataSource) {
        ds = this.valuesManager.getDataSource();
    }
    if (ds != null && isc.isA.String(ds)) {
        this.logWarn("unable to look up DataSource: " + ds + ", databinding will not be used");
        return fields;
    }

    // Shorthand - treat fields being null or an empty array as the same case - no (meaningful) 
    // fields were passed in
    var noSpecifiedFields = (fields == null || fields.length == 0),
        dsFields;
    // get fields from the DataSource if we have one
    if (ds) {
        // flatten fields if so configured
        var flatten = this.useFlatFields;
        if (flatten == null) flatten = ds.useFlatFields;
        dsFields = flatten ? ds.getFlattenedFields() : ds.getFields();
    }

    if (!noSpecifiedFields && isc.Canvas.validateFieldNames) {
        // loop through each field and check that each field name is a valid JavaScript identifier.
        isc.Canvas._validateFieldNames(fields, this);
    }

    // Case 1: no dataSource specified
    // This widget isn't associated with a datasource - all fields are full specifications
    // intended for the underlying widget.  The fields property is thus left untouched.
    if (ds == null || dsFields == null) {
        //this.logWarn("No DataSource fields");
        if (fields != null && isc.SimpleType) {
            // type defaults are auto-applied to DS fields and combined fields, but we need to
            // do it here for any field that doesn't appear in the DataSource
            for (var i = 0; i < fields.length; i++) {
                
                if (fields[i] == null) continue;
                // For items with editorType set to DateItem or date, default the data type
                // to date also so we pick up type validators etc.
                
                if (fields[i].type == null &&
                    this._dateEditorTypes[fields[i].editorType] == true) 
                {
                    fields[i].type = "date";
                }
                
                if (fields[i].type == null) {
                    var className = null;
                    if (fields[i]._constructor && isc[fields[i]._constructor]) {
                        className = fields[i]._constructor;
                    }
                    if (fields[i].editorType && isc[fields[i].editorType]) {
                        className = fields[i].editorType;
                    }
                    if (className && isc[className] && isc[className].getInstanceProperty) {
                        // if the field has no type, only use the type from the specified editorType
                        // if there is no valueMap available - if there's a valueMap, 
                        // assume type: "text"
                        if (fields[i].valueMap || fields[i].editorValueMap) fields[i].type = "text";
                        else {
                            fields[i].type = 
                                isc[className].getInstanceProperty("type") ||
                                isc[className].getInstanceProperty("defaultType");
                        }
                    }
                }
                isc.SimpleType.addTypeDefaults(fields[i]);
                if (fields[i].type) {
                    var type = isc.SimpleType.getType(fields[i].type);
                    fields[i] = this.addTypeFieldProperties(fields[i], type);

                    if (!fields[i].format && type && type.format) {
                        fields[i].format = type.format;
                    }
                }
            }
        }
        this.addFieldValidators(fields);
        return fields;
    }

    // Case 2: dataSource specified, but no fields specified
    if (this.doNotUseDefaultBinding) return [];
    // The widget will show all DataSource fields, applying reasonable defaults.
    if (ds != null && noSpecifiedFields) {
        
        
        if (this.suppressAllDSFields) return [];
        
        //this.logWarn("No Specified fields, use DS fields only");
        // NOTE we generally have to create a copy of the DataSource fields rather than having
        // everyone use the same objects, because widgets tend to scribble things into this.fields,
        // such as widths derived by a sizing policy.
        fields = [];
        for (var fieldName in dsFields) {
            var field = dsFields[fieldName];
            
            if (!this.shouldUseField(field, ds)) continue;
            
            var componentField = isc.addProperties({}, field)
            // modify 'canEdit' to match our canEditAttribute if necessary.
            var canEdit = this.getDefaultCanEdit(field);
            
            var undef;
            if (canEdit === undef) {
                delete componentField.canEdit;
            } else {
                componentField.canEdit = canEdit;
            }
            
            // If "showHiddenFields" is true we'll show fields which are marked as
            // hidden:true in the dataSource.
            // However we don't want that property picked up in the local field object, 
            // since that would be an equivalent of 'showIf' false, meaning the
            // field would be present but hidden. If the dev wants that they'll have
            // to explicitly add a field definition to the component for this field
            if (componentField.hidden) delete componentField.hidden;
            
            fields.add(componentField);
        }
        this.addFieldValidators(fields);
        return fields;                                               
    }

	// Case 3: dataSource and fields specified
    // fields provided to this instance act as an overlay on DataSource fields
    if (ds != null && !noSpecifiedFields) {
        //this.logWarn("Combining specified fields with dataSource fields");
        // Loop through local fields and apply type defaults.
        // This allows local fields to specify a type which takes precedence over 
        // the DS field type.
        // Also ensure that any specified field with editorType set to "DateItem" has type
        // set to date if type isn't explicitly defined on either the item or the
        // corresponding dataSource field.
        
        var fieldCanEditMap = {};
        for (var i = fields.length - 1; i >= 0; i--) {
            var field = fields[i];            
            if (field == null) continue;
            // Drop any field marked canView: false from the DBC, even if it was explicitly 
            // included.  Fields that have been manually marked in this way are intended as 
            // "server-only" fields; also, the security system can mark fields canView: false,
            // to ensure that client-side components do not attempt to display values the 
            // user is not authorized to see (they would only see blank cells anyway, because
            // the server will also strip data values out)
            var dsField = (field.name != null) ? ds.getField(field.name) : null;
            if (dsField && dsField.canView === false) {
                this.logInfo("Dropping explicitly-named field " + field.name + 
                             " because it is marked canView: false");
                fields.removeAt(i);
                continue;
            }

            
            if (field.type == null && this._dateEditorTypes[field.editorType] == true) {
                var name = field.name;
                var dsField = (name != null) ? ds.getField(name) : null;
                if (dsField == null || dsField.type == null) {
                    field.type = "date";
                }
            }
            if (isc.SimpleType) {
                isc.SimpleType.addTypeDefaults(field);
                var fieldType = field.type;
                if (fieldType == null) {
                    var dsField = (field.name != null) ? ds.getField(field.name) : null;
                    if (dsField) fieldType = dsField.type;
                }
                if (fieldType) {
                    var type = isc.SimpleType.getType(fieldType);

                    fields[i] = this.addTypeFieldProperties(fields[i], type);
                }
            }
            // set up field.canEdit based on settings on the DS field            
            if (dsField) {
                var canEdit = field.canEdit;
                // if canEdit is set at the component field level, respect it
                
                if (canEdit == null) {
                    canEdit = this.getDefaultCanEdit(dsField);
                } else {
                    //>DEBUG
                    this.logDebug("DataBoundComponent respecting explicit 'canEdit' on target field " + dsField.name,
                        "canEditField");    
                    //<DEBUG
                }
                // remember the default on a map - we'll apply it *after* we've copied the
                // dsField defaults onto the field object
                
                fieldCanEditMap[field.name] = canEdit;
                
            } else if (field.includeFrom != null) {
                // Disallow editing of includeFrom fields by default.
                var canEdit = field.canEdit;
                if ((canEdit == null) && (this.canEditIncludeFromFields() == false)) {
                    canEdit = false;
                }
                
                fieldCanEditMap[field.includeFrom] = canEdit;
            
            // No DS field, don't modify canEdit
            
            } else {
                fieldCanEditMap[field.name] = field.canEdit;
            }

            
            // Always apply type defaults to the local fields. This allows 
            // local field.type to be specified and override ds field.type.
            // addTypeDefaults will bail immediately if it's already been applied
            if (field.type != null) {
                isc.SimpleType.addTypeDefaults(field);
            }
        }
        if (this.useAllDataSourceFields || hideExtraDSFields) {
            var canvas = this;
            var bothFields = ds.combineFieldOrders(
                        dsFields, fields, 
                        function (field, ds, isLocal) { 
                            return canvas.shouldUseField(field, ds, isLocal) 
                        });
 
            // Loop through the combined fields:
            // - if hideExtraDSFields is true, hide any fields picked up from the
            //   DS that weren't explicitly specified
            // - handle any fields that should pick up defaults from another DS
            //   (where field.includeFrom is set).
            for (var i = 0; i < bothFields.length; i++) {
                var field = bothFields[i];
                if (!fields.containsProperty("name", field.name)) {
                    if (hideExtraDSFields && field.showIf == null) {
                        field.showIf = "return false";
                    }
                
                } else {
                    if (field.includeFrom != null && ds.getField(field.name) == null) {
                        this._combineIncludeFromFieldData(field);
                    }
                }
                
                // DS fields that weren't in the fields array need to have 'canEdit' updated
                var canEdit;
                if (fields.contains(field)) {
                    canEdit = field.includeFrom ? fieldCanEditMap[field.includeFrom] 
                                                : fieldCanEditMap[field.name];
                } else {
                    canEdit = this.getDefaultCanEdit(field);
                }
                
                var undef;
                if (canEdit === undef) {
                    delete field.canEdit;
                } else {
                    field.canEdit = canEdit;
                }
            }
            this.addFieldValidators(bothFields);
            return bothFields;
        } else {
            // only the fields declared on the component will be shown, in the order specified on
            // the component
            for (var i = 0; i < fields.length; i++) {
                var field = fields[i];
                if (!field) continue;
                field = this.combineFieldData(field);
                field.canEdit = field.includeFrom ? fieldCanEditMap[field.includeFrom] 
                                                  : fieldCanEditMap[field.name];
                if (field.canEdit == null) {
                    // need to delete field.canEdit, or we don't pick up class defaults later
                    delete field.canEdit;
                }
            }
            this.addFieldValidators(fields);
            // return the original fields array, with properties added to the field objects
            return fields;
        }
    }
},
   
// helper to apply duplicated 'fieldProperties' from type object to field

addTypeFieldProperties : function (field, type) {
    if (type && type.fieldProperties) {
        var finalField = {};
        for (var property in type.fieldProperties) {
            // Duplicate editorProperties - we manipulated it directly on the
            // widget and don't want to pollute the version on the type object
            
            if (property == "editorProperties") {
                finalField[property] = isc.addProperties({}, type.fieldProperties[property]);
            } else {
                finalField[property] = type.fieldProperties[property];
            }
        }
        // Allow explicit entries to clobber those picked up from the type.
        return isc.addProperties(finalField, field);
    }
    return field;
},
    
    
//> @attr dataBoundComponent.canEditFieldAttribute (identifier : "canEdit" : IRA)
// If this component is bound to a dataSource, this attribute may be specified to customize
// what fields from the dataSource may be edited by default. For example the +link{SearchForm}
// class has this attribute set to <code>"canFilter"</code> which allows search forms to edit
// dataSource fields marked as <code>canEdit:false</code> (but not those marked as
// <code>canFilter:false</code>).
// <P>
// Note that if <code>canEdit</code> is explicitly specified on a field in 
// the +link{DataBoundComponent.fields} array, that property will be respected in preference to 
// the canEditAttribute value. (See +link{FormItem.canEdit}, +link{ListGridField.canEdit}).
// Also note that individual dataBoundComponents may have additional logic around whether a field
// can be edited - for example +link{listGrid.canEditCell()} may be overridden.
// @visibility external
//<

// if field.canEdit is not explicitly set (at the item level), derive it from the
// dataSource field value for our canEditAttribute
getDefaultCanEdit : function (dsField) {
    
    var canEditAttribute = this.canEditFieldAttribute;
    if (canEditAttribute == null) canEditAttribute = "canEdit";
    var canEditValue = dsField[canEditAttribute];
    // If null check 'canSave' - this should be respected if no explicit canEdit is set
    if (canEditValue == null) {
        if (dsField.canSave == false && !this._canEditUnsaveableFields) {
            canEditValue = false;
        }
    }
    
    //>DEBUG
    this.logDebug("DataBoundComponent using canEditFieldAttribute:" + canEditAttribute + 
                  " setting 'canEdit' to " + canEditValue + " on target field " + dsField.name,
                  "canEditField");
    //<DEBUG

    return canEditValue;

},

//> @method dataBoundComponent.fieldIsEditable()
// Can the field be edited?  This base method always returns false, but it's
// overridden by subclasses such as +link{DynamicForm} and +link{ListGrid}.
//
// @param field (object | number | string)  field object or identifier
// @return      (boolean)                   whether field can be edited
//
// @group editing
// @see listGrid.fieldIsEditable
// @see dynamicForm.fieldIsEditable
// @visibility external
//<
fieldIsEditable : function (field) {
    return false;
},

// If a field is inherited from another DS via 'includeFrom', should it be editable?
// We want to allow this for filtering, but not for record-editing (for db saving) by 
// default.
// Rely on the fact that the 'canEditFieldAttribute' will be set to "canFilter" in
// the case where we're filtering fields.
_$canEdit:"canEdit",
canEditIncludeFromFields : function () {
    var canEditAttribute = this.canEditFieldAttribute;
    if (canEditAttribute == null || canEditAttribute == this._$canEdit) {
        return false;
    }
    return true;
},

combineFieldData : function (field) {
    var ds = this.getDataSource();

    // specified dataPath -- will pick up defaults from another (nested) ds field 
    if (this.getFullDataPath() || field.dataPath) {
    
        var dataPath = this.buildFieldDataPath(this.getFullDataPath(), field);
        isc.DataSource.combineFieldData(field, this.getDataPathField(dataPath));
        return field;
    // specified ds field -- will pick up defaults from field in this dataSource
    } else if (ds != null && ds.getField(field.name)) {
                        
        // combine the component field specification with the datasource field
        // specification - component fields override so that you can eg, retitle a field
        // within a summary
        return ds.combineFieldData(field);
        
    
        
    // specified 'includeFrom' field -- will pick up defaults from field in another dataSource
    } else if (field.includeFrom != null) {
        return this._combineIncludeFromFieldData(field);
    }
        
    return field;
},

_combineIncludeFromFieldData : function (field) {

    var split = field.includeFrom.split(".");
    if (split == null || split.length != 2) {
        this.logWarn("This component includes a field with includeFrom set to:"
            + field.includeFrom + ". Format not understood.");
    } else {
        var relatedDS = isc.DataSource.get(split[0]),
            fieldName = split[1];
        if (relatedDS == null) {
            this.logWarn("Field specifies includeFrom:" + field.includeFrom +
                ". Unable to find dataSource with ID:" + split[0]);
        } else {
            // default the field name to the includeField's name if not explicitly set.
            if (field.name == null) field.name = fieldName;
            return relatedDS.combineFieldData(field, relatedDS.getField(fieldName));
        }
    }
},

// return whether this component wants to use the field when binding to a DataSource
shouldUseField : function (field, ds, isLocal) { 
    // canView: false means this field should never be shown to a user, even if explicitly 
    // declared on a DBC
    if (field.canView === false) return false;
    // hidden at the DS level means don't include in the component.
    // If this is a local field (no ds passed in, or has explicit "isLocal" flag indicating
    // we're looking at a local field definition object, ignore this flag
    // (Hidden at the widget-field level is less strong - equivalent to just showIf:false.
    // See ListGridField.hidden docs)
    if (ds && !isLocal && !this.showHiddenFields) {
        if (field.hidden) return false;
    }
    if (field.canFilter == false && this.showFilterFieldsOnly) {
        return false;            
    }
    
    // don't use the field if the field is marked as a detail field and the component is not a
    // detail component 
    
    if (field.detail && !this.showDetailFields) return false;

    if (!this.showComplexFields && ds.fieldIsComplexType(field.name)) return false;

    return true;
},

// Add validators that replace basic field properties (ex. required)
addFieldValidators : function (fields) {
    if (fields == null) return;
    

    for (var i = 0; i < fields.length; i++) {
        var field = fields[i];
        // Ensure we don't share validators array instances across different components etc
        if (field.validators != null) field.validators = field.validators.duplicate();

        if (field.required) {
            var validator = this.getRequiredValidator(field),
                message = validator.errorMessage;
            
            // Add validator to field
            if (!field.validators) {
                field.validators = [validator];
            } else {
                if (!isc.isAn.Array(field.validators)) {
                    field.validators = [field.validators];
                }
                // See if we already have a required validator.
                // If so, we need to make sure the errorMessage is correct.
                // If not, add a new required validator.
                if (!field.validators.containsProperty("type", validator.type) &&
                    !field.validators.containsProperty("_constructor", validator.type))
                {
                    // if the field is using the shared, default validators for the type, 
                    // make a copy before modifying
                    if (field.validators._typeValidators) {
                        field.validators = field.validators.duplicate();
                    }
                    field.validators.add(validator);
                } else if (message != null) {
                    var ds = this.getDataSource(),
                        v = (field.validators.find("type", validator.type) ||
                             field.validators.find("_constructor", validator.type))
                    ;
                    // See if our error message should override current one
                    // created on the DataSource.
                    if (v.errorMessage == null || (ds && v.errorMessage == ds.requiredMessage)) {
                        v.errorMessage = message;
                    }
                }
            }
        // If a required validator is present on the field but field.required is explicitly false,
        // required must be true at the type level (set on the DS field probably), but have
        // been overridden at the component field level.
        // In this case, remove the required validator from the component field.
        } else if (field.required == false) {
            var validators= field.validators;
            if (field.validators != null) {
                var requiredValidatorIndex = field.validators.findIndex("type", "required");
                if (requiredValidatorIndex != -1) {
                    field.validators.removeAt(requiredValidatorIndex);
                }
            }
        }

        // If field.length is specified, apply a length validator
        
        if (this.applyLengthValidators && field.length != null) {
            var validator = this.getMaxLengthValidator(field);
            
            // Add validator to field
            if (!field.validators) {
                field.validators = [validator];
            } else {
                if (!isc.isAn.Array(field.validators)) {
                    field.validators = [field.validators];
                }
                // See if we already have a length range validator
                // If we do, and it has a specified max which is <= this.length,
                // don't add this validator.
                var hasLengthValidator = false;
                for (var ii = 0 ; ii < field.validators.length; ii++) {
                    var existingValidator = field.validators[ii];
                    if ((existingValidator.type == validator.type ||
                         existingValidator._constructor == validator.type) &&
                        existingValidator.max != null && existingValidator.max <= validator.max)
                    {
                        hasLengthValidator = true;
                        break;
                    }
                }
                if (!hasLengthValidator) {
                    field.validators.add(validator);
                }
            }
        }

        if (field.maxFileSize != null) {
            var validator = this.getMaxFileSizeValidator(field),
                message = validator.errorMessage;

            if (!field.validators) {
                field.validators = [validator];
            } else {
                if (!isc.isAn.Array(field.validators)) {
                    field.validators = [field.validators];
                }

                // See if there is already a 'maxFileSize' validator.
                // If so, see if we need to set the errorMessage.
                if (!field.validators.containsProperty("type", validator.type) &&
                    !field.validators.containsProperty("_constructor", validator.type))
                {
                    if (field.validators._typeValidators) {
                        field.validators = field.validators.duplicate();
                    }
                    field.validators.add(validator);
                } else if (message != null) {
                    var v = (field.validators.find("type", validator.type) ||
                             field.validators.find("_constructor", validator.type))
                    ;
                    if (v.errorMessage == null) {
                        v.errorMessage = message;
                    }
                    v.maxFileSize = validator.maxFileSize;
                }
            }
        }

        // For multiple:true fields, default to validating each selected value 
        // individually.
        // This is required to ensure that (for example) type validators don't auto-convert
        // from an array of strings to a single comma-separated string
        
        if (field.multiple && field.validateEachItem == null) field.validateEachItem = true;
    }
},


getRequiredValidator : function (field) {
    var requiredValidator = {
            type: "required"
        },
        message = field.requiredMessage || this.requiredMessage;

    if (message != null) requiredValidator.errorMessage = message;
    return requiredValidator;
},

// If field.length is specified, apply a lengthRange validator to the field.
// We also do this on the server side
applyLengthValidators:true,
getMaxLengthValidator : function (field) {
    var lengthValidator = {
        type:"lengthRange",
        max:field.length
    }
    return lengthValidator;
},

getMaxFileSizeValidator : function (field) {
    var maxFileSizeValidator = {
            type: "maxFileSize",
            maxFileSize: field.maxFileSize
        },
        message = field.maxFileSizeMessage;

    if (message != null) maxFileSizeValidator.errorMessage = message;
    return maxFileSizeValidator;
},

// doc'd at ListGrid level
getAllFields : function () {
    return this.completeFields || this.fields;
},

//>	@method	dataBoundComponent.getField()	
// Return a field by a field index or field name.
//
// @param fieldID (String || Number) field index or field.name
//
// @return (object) Field description
// @visibility external
//<
getField : function (fieldId) {
    if (!this.fields) return null;
    return isc.Class.getArrayItem(fieldId, this.fields, this.fieldIdProperty);
},


// get a reference to a field by name - this might be in fields, completeFields or ds.fields
getUnderlyingField : function (fieldId) {
    if (!this.fields && !this.completeFields && !this.dataSource) {
        this.logWarn("fields and completeFields are null and there is no DataSource");
        return null;
    }
    var item = null;
    if (this.fields) {
        // see if there's a valid field - use getField() rather than isc.Class.getArrayItem()
        // because: 
        // 1) getField() is overridden to support dataPath in LG, and to return formItems in DF
        // 2) the default implementation, in DBC.getField(), calls getArrayItem() anyway
        item = this.getField(fieldId);
    }
    if (!item && this.completeFields) {
        item = isc.Class.getArrayItem(fieldId, this.completeFields, this.fieldIdProperty);
    }
    if (!item && this.dataSource) {
        if (!isc.isA.DataSource(this.dataSource)) this.dataSource = this.getDataSource(this.dataSource);
        item = this.dataSource.getField(fieldId);
    }
    return item;
},

//> @method dataBoundComponent.getFieldNum()	
// Find the index of a currently visible field.
//
// @param fieldID (String || Field) field name or field
// @return (int) index of field within currently visible fields, or -1 if not found.
// @visibility external
//<
getFieldNum : function (fieldId) {
    if (!this.fields) return -1;
    // handle being passed a field object (or a clone of a field object)
    if (isc.isA.Object(fieldId) && (fieldId[this.fieldIdProperty] != null)) {
        fieldId = fieldId[this.fieldIdProperty];
    }
    return isc.Class.getArrayItemIndex(fieldId, this.fields, this.fieldIdProperty);
},

// Whether a field derived from XML Schema is considered structurally required.
// <P>
// A field is considered required if the field itself must be present within it's complexType
// *and* the complexType and all parent complexTypes are required.
// <P>
// Note that this is relative to how much of a given structure this component edits.  If you
// bind a component to a DataSource representing an entire WSDLMessage, a field may not be
// considered required because it has an optional parent, whereas if you instead bind to a
// particular sub-part of the message the field could be considered required since no optional
// parent elements are in play.  This is the correct behavior but it does mean that to get
// correct "required" behavior you want to coordinate all of your components to use a
// ValuesManager that actually represents the *whole* structure they are meant to be editing.
// <P>
// NOTE that a more complete implementation might dynamically check the current values to check
// whether at least one entry had been added to a structure that is otherwise optional; at that
// point the rest of the values should be considered required as well
isXMLRequired : function (field) {

    if (!field || !this.useXMLRequired || !field.xmlRequired) return false;

    if (!field.dataPath) return true;

    var dataSource = this.getDataSource();
    if (!dataSource) return true;

    //this.logWarn("field: " + this.echoLeaf(field) + " has path: " + field.dataPath);

    var segments = field.dataPath.split(isc.slash),
        field;
    for (var i = 0; i < segments.length; i++) {
        var fieldId = segments[i];

        //this.logWarn("checking segment: " + fieldId + " against DataSource: " + dataSource);

        // invalid dataPath, but will be warned about elsewhere.  The field's individual
        // xmlRequired status should be considered authoritative
        if (!dataSource) return true;

        field = dataSource.getField(fieldId);

        // invalid dataPath again
        if (!field) return true;

        // a parent XML structure is not required, so the field should not be
        if (field.xmlMinOccurs != null && field.xmlMinOccurs < 1) {
            //this.logWarn("optional field found: " + fieldId);
            return false;
        }

        dataSource = dataSource.getSchema(field.type);
        
    }
    return true;
    
},

// Field State management
// ---------------------------------------------------------------------------------------
// Retrieve and restore metadata about fields of a DataBoundComponent such as visibility,
// width or other user-settable display settings.

// Helper method to evaluate the various viewState objects (stored as strings)
evalViewState : function (state, stateName, suppressWarning) {
    return isc.Canvas.evalViewState(state, stateName, suppressWarning, this);
},

// documented in ListGrid.js
canEditTitles:false,

shouldIncludeTitleInFieldState : function () {
    return this.canEditTitles;
},

// DBC-level fieldState methods
getFieldState : function (returnObject) {
    var includeTitle = this.shouldIncludeTitleInFieldState();
    var fieldStates = [];
    var allFields = this.getAllFields();
    if (allFields) {
        for (var i = 0; i < allFields.length; i++) {
            var field = allFields[i];

            // defensive null check
            if (!field || field.excludeFromState) continue;
            
            var fieldName = field[this.fieldIdProperty],
                fieldState = this.getStateForField(fieldName, includeTitle)
            ;
            fieldStates.add(fieldState);
        }
    }

    if (returnObject) return fieldStates;
    return isc.Comm.serialize(fieldStates, false);
},

// get the state for a given field by name
getStateForField : function (fieldName, includeTitle) {
    var field = this.getAllFields().find(this.fieldIdProperty, fieldName),
        fieldState = { name:fieldName };

    // defensive null check
    if (!field) return null;

    if (field.frozen == true) fieldState.frozen = true;

    if (!this.fieldShouldBeVisible(field, this.getFieldNum(fieldName))) fieldState.visible = false;
    // store the userFormula if this is a formula field
    if (field.userFormula) {
       fieldState.userFormula = field.userFormula;
       // also persist type because it's set to 'float' by the FormulaBuilder - and this value
       // is required for e.g. decimalPrecision - and frequently formula fields are added by
       // the end user such that there's no matching field in the ds to define the type
       fieldState.type = field.type;
    }
    // store the userSummary if one is present
    if (field.userSummary) fieldState.userSummary = field.userSummary;

    // auto-persist title for formula / summary fields, since it's user entered
    if (includeTitle || field.userSummary || field.userFormula) {
    	fieldState.title = field.title;
    }

    var undef;
    // for these fields, a value of null is meaningful vs undefined.  We want any such
    // meaningful value to override field defaults as supplied by the component or
    // datasource
    if (field.autoFitWidth !== undef) fieldState.autoFitWidth = field.autoFitWidth;
    //
    // these may also be set by the user via the FieldPicker
    if (field.precision !== undef) fieldState.precision = field.precision;
    if (field.decimalPrecision !== undef) fieldState.decimalPrecision = field.decimalPrecision;
    if (field.decimalPad !== undef) fieldState.decimalPad = field.decimalPad;
 
    if (this.getSpecifiedFieldWidth) {
        // don't write out null widths - retains pre-specified percentage widths
        var width = this.getSpecifiedFieldWidth(field);
        if (width != null) fieldState.width = width;
    }

    return fieldState;
},

// internal method that modifies this.completeFields according to the fieldState argument
// doesn't redraw the LG; call setFieldState instead.
// -- DetailViewer has no way of removing unwanted fields from the fields array, so add an
// optional param hideExtraDSFields to add the additional fields from the DS with showIf:"false"
_setFieldState : function (fieldState, hideExtraDSFields) {
    if (fieldState == null) return this.getAllFields();
    var allFields = this.getAllFields();
    var remainingFields = allFields.getProperty(this.fieldIdProperty),
        completeFields = []
    ;
    
    // set visibility and width according to fieldState    
    for (var i = 0; i < fieldState.length; i++) {
        var state = fieldState[i],
            field = allFields.find(this.fieldIdProperty, state.name)
        ;
        // if a field is specified in fieldState which is not present in the grid, check if its
        // a formula or summary field and add a field-def for it
        if (field == null) {
            if (state.userFormula || state.userSummary) {
                field={};
                field[this.fieldIdProperty] = state.name;
            } else continue;
        }
        remainingFields.remove(state.name);
        if (state.visible == false) {
            field.showIf = this._$false;
            field.hidden = true;
        } else {
            field.showIf = null;
            // set field.detail to false if the field is visible. This makes sure that
            // ds.combineFieldData skips setting detail to true on this field if the
            // field has been set to visible by the user.
            field.detail = false;
            field.hidden = false;
        }
        if (state.width != null && (!isNaN(state.width) || state.width=="*")) field.width = state.width;

        field.frozen = state.frozen;

        var undef;
        if (state.title) field.title = state.title;
        // restore state for userFomula and userSummary
        if (state.userFormula != null) field.userFormula = state.userFormula;
        if (state.userSummary != null) field.userSummary = state.userSummary;
        if (state.type != null) field.type = state.type;

        // for these fields, a value of null is meaningful vs undefined.  We want any such
        // meaningful value to override field defaults as supplied by the component or
        // datasource
        if (state.autoFitWidth !== undef && state.autoFitWidth != field.autoFitWidth) field.autoFitWidth = state.autoFitWidth;
        if (state.precision !== undef && state.precision != field.precision) field.precision = state.precision;
        if (state.decimalPrecision !== undef && state.decimalPrecision != field.decimalPrecision) field.decimalPrecision = state.decimalPrecision;
        if (state.decimalPad !== undef && state.decimalPad != field.decimalPad) field.decimalPad = state.decimalPad;
        completeFields.add(field);
    }
    
    // if a field is specified for the grid for which there is no entry in fieldState
    //   check for a preceding field in the grid's fields which is specified in the fieldState
    //    and put it after that one
    //   otherwise, place it after the last visible field if it's visible, or last field
    //    altogether if not
    // Undocumented feature "defaultFieldState" - if this exists and has an entry for any fields
    // not included in the explicit field state, apply it.
    // We use this in the ListGrid to re-apply the initial field settings from when "setFields"
    // was first called.
    var defaultFieldState = this.defaultFieldState;
    if (defaultFieldState != null) defaultFieldState = this.evalViewState(defaultFieldState, "fieldState");
    
    for (var i = 0; i < remainingFields.length; i++) {
        var name = remainingFields[i], 
            index = allFields.findIndex(this.fieldIdProperty, name), 
            field = allFields[index], 
            precedingField = allFields[index - 1];

        var defaultState = defaultFieldState ? defaultFieldState.find("name", name) : null;
        // don't modify fields where there was no default state
        // In this case we want fields to remain at their current size, visibility, etc
        if (defaultState != null) {
        
            var hidden = defaultState.visible == false;
            if (hidden) {
                field.showIf = this._$false;
            } else {
                field.showIf = null;
            
                // set field.detail to false if the field is visible. This makes sure that
                // ds.combineFieldData skips setting detail to true on this field if the
                // field has been set to visible by the user.
                field.detail = false;
            }
            
            if (defaultState.width != null 
                && (!isNaN(defaultState.width) || defaultState.width=="*")) 
            {
                field.width = defaultState.width;
            }
            field.frozen = defaultState.frozen;

            if (defaultState.title) field.title = defaultState.title;
            // restore state for userFomula and userSummary
            if (defaultState.userFormula != null) field.userFormula = defaultState.userFormula;
            if (defaultState.userSummary != null) field.userSummary = defaultState.userSummary;
            if (defaultState.autoFitWidth != field.autoFitWidth) field.autoFitWidth = defaultState.autoFitWidth;
        }
        
        if (precedingField != null) {
            var precedingIndex = completeFields.indexOf(precedingField);
            if (precedingIndex != -1) {
                completeFields.addAt(field, precedingIndex + 1);
                continue;
            }
        }

        if (this.fieldShouldBeVisible(field, index) && !hideExtraDSFields) {
            completeFields.addAt(field, this._lastVisibleFieldIndex(completeFields) + 1);
        } else {
            completeFields.add(field);
        }
    }
    //this.completeFields = completeFields;
    return completeFields;
},

// observe this method to be notified on column resize or reorder and show/hide/freeze field
fieldStateChanged : function () {},

// returns the last visible field in an array of fields
_lastVisibleFieldIndex : function (fields) {
    if (fields == null) fields = this.completeFields;
    var visibleFields = this.getVisibleFields(fields);
    if (visibleFields.length == 0) return -1;
    return fields.lastIndexOf(visibleFields.last());
},

// determine which of the passed fields should be shown, and return them as a new array
getVisibleFields : function (fields) {
    var visibleFields = [];
	for (var i = 0; i < fields.length; i++) {
        var field = fields[i];
    	// make sure we don't have any null fields
		if (field == null) continue;

        if (this.fieldShouldBeVisible(field, i)) visibleFields.add(field);
	}
    return visibleFields;
},

// fieldShouldBeVisible: intended as a possible advanced override point for a field visibility
// policy not easily expressed via showIf()
_$falseSemi:"false;",
_$false:"false",
_$true:"true",
fieldShouldBeVisible : function (field, fieldNum) {
    // evaluate a showIf expression if present
    if (field.showIf != null) {
        // CALLBACK API:  available variables:  "list,field,fieldNum"
        // Convert a string callback to a function
        
        
        if (field.showIf == this._$false || field.showIf == this._$falseSemi) return false;
        if (field.showIf == this._$true) return true;
        isc.Func.replaceWithMethod(field, "showIf", "list,field,fieldNum");
        if (!field.showIf(this, field, fieldNum)) return false;
    } else if ((this.fields != null || !this.showHiddenFields) && field.hidden) {
        return false;
    }
    return true;
},

// ---------------------------------------------------------------------------------------

//>	@method	dataBoundComponent.setFieldValueMap()	
// Set the valueMap for a field
//
// @param fieldID (String | int) name or index of the field to update
// @param valueMap (object) ValueMap for the field with the passed fieldID
//<
setFieldValueMap : function (field, valueMap) {
    
    if (!isc.isAn.Object(field)) field = this.getField(field);
    if (!field) return;

    field.valueMap = valueMap;
},

//> @method dataBoundComponent.find()
// This API is equivalent to +link{List.find()} but searches for a matching record among already-loaded data only.  
// Use +link{listGrid.fetchData,fetchData} to load data from the server.
// @param advancedCriteria  (AdvancedCriteria)  AdvancedCriteria to use with 
// @return (Object) first matching object or null if not found
//  
// @visibility external
//<
find : function (advancedCriteria) {
   return this.data.find(advancedCriteria);
},

//> @method dataBoundComponent.findAll()
// This API is equivalent to +link{List.findAll()} but searches for a matching record among already-loaded data only.  
// Use +link{listGrid.fetchData,fetchData} to load data from the server.
// @param advancedCriteria  (AdvancedCriteria)  AdvancedCriteria to use with 
// @return (Array) all matching Objects or null if none found
// 
// @visibility external
//<
findAll : function (advancedCriteria) {
    return this.data.findAll(advancedCriteria);
},
//> @method dataBoundComponent.findIndex()
// This API is equivalent to +link{List.findIndex()} but searches for a matching record among already-loaded data only.  
// Use +link{listGrid.fetchData,fetchData} to load data from the server.
// @param advancedCriteria  (AdvancedCriteria)  AdvancedCriteria to use with
// @return (int) index of the first matching Object or -1 if not found 
// 
// @visibility external
//<
findIndex : function (advancedCriteria) {
    return this.data.findIndex(advancedCriteria);
},
       
//> @method dataBoundComponent.findNextIndex()
// This API is equivalent to +link{List.findNextIndex()} but searches for a matching record among already-loaded data only.  
// Use +link{listGrid.fetchData,fetchData} to load data from the server.
// @param startIndex  (int)  first index to consider
// @param advancedCriteria  (AdvancedCriteria)  AdvancedCriteria to use with
// @param [endIndex]  (int) last index to consider
// @return (int) index of the first matching Object or -1 if not found
// 
// @visibility external
//<
findNextIndex : function (startIndex, advancedCriteria, endIndex) {
    return this.data.findNextIndex(startIndex, advancedCriteria, null, endIndex);
},

//> @method dataBoundComponent.setDataSource()
// Bind to a new DataSource.
// <P>
// Like passing the "dataSource" property on creation, binding to a DataSource means that the
// component will use the DataSource to provide default data for its fields.
// <P>
// When binding to a new DataSource, if the component has any existing "fields" or has a dataset,
// these will be discarded by default, since it is assumed the new DataSource may represent a
// completely unrelated set of objects.  If the old "fields" are still relevant, pass them to
// setDataSource().
// 
// @param dataSource  (ID or DataSource)  DataSource to bind to 
// @param fields      (Array of Fields)  optional array of fields to use
// 
// @visibility external
// @example WSDLDataSource
//<
setDataSource : function (dataSource, fields) {
    if (isc._traceMarkers) arguments.__this = this;

	// if passed in value is null then bind() will then work on the declared ds.
	this.dataSource = dataSource || this.dataSource;

    if (this.dataSource == null && !this.skipNullDataSourceCheck) {
    	this.logWarn("Invalid call to setDataSource() passing null.  (Set the property " +
            "'skipNullDataSourceCheck' on the component to avoid this warning.)  If you're " +
            "having trouble with loading DataSources, please see the following FAQ: " + 
            "http://forums.smartclient.com/showthread.php?t=8159#aDSLoad");
    }
	
    // NOTE: actual dataBinding, meaning picking up dataSource field data, is done by
    // "bindToDataSource".  This call *must* be within setFields() because setFields() may be
    // called again after binding, and must pick up DataSource field data at that time too.
	if (this.setFields) this.setFields(fields);

	// since we've (re)bound this widget, clear any data it may have as it may no longer be
    // valid.
    if (this.dataSource) {
        
        if (this.isA("DynamicForm")) this.setData({});
        else this.setData([]);
    }
    this.markForRedraw("bind");
},
// backCompat
bind : function (dataSource, fields) {
	this.setDataSource(dataSource, fields);
},

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

makeDataSourceFromFields : function (id) {
    if (id == null) id = this.ID;

    var titleSuffix = "",
        criteriaBasePathSuffix,
        metaFields
    ;
    for (var className in isc.Canvas._dbcTypeDetails) {
        if (this.isA(className)) {
            titleSuffix = isc.Canvas._dbcTypeDetails[className].titleSuffix;
            criteriaBasePathSuffix = isc.Canvas._dbcTypeDetails[className].criteriaBasePathSuffix;
            metaFields = isc.Canvas._dbcTypeDetails[className].metaFields;
        }
    }
    if (criteriaBasePathSuffix) {
        criteriaBasePathSuffix = id + "." + criteriaBasePathSuffix;
    }
    var title = id + " " + titleSuffix,
        dsID = id + "_values"
    ;
    if (isc.DataSource.get(dsID)) {
        // This really shouldn't occur unless the user explicitly assigns the
        // same ID to two DBCs.
        var count = 2,
            testDsID;
        do {
            testDsID = dsID + count++;
        } while (isc.DataSource.get(testDsID));
        dsID = testDsID;
    }
    var properties = { ID: dsID, clientOnly: true, criteriaBasePath: criteriaBasePathSuffix, title: title, pluralTitle: title };

    var fields = this.fields || this.items;
    if (fields) {
        var dsFields = [];
        for (var i = 0; i < fields.length; i++) {
            var field = fields[i];

            // defensive null check
            if (!field) continue;

            var fieldName = field[this.fieldIdProperty],
                fieldType = (isc.isA.FormItem(field) ? field.getType() : field.type) || "text"
            ;
            if (fieldType == "select") fieldType = "text";

            dsFields.push({ name: fieldName, type: fieldType })
        }
        properties.fields = dsFields;
    }

    if (metaFields) {
        var dsFields = [];
        for (var i = 0; i < metaFields.length; i++) {
            var fieldName = metaFields[i],
                fieldType = isc.Canvas._dbcTypeMetaFieldTypes[fieldName]
            ;
            if (fieldType) {
                dsFields.push({ name: isc.Canvas._makeRuleScopeMetaFieldName(fieldName),
                    title: "[meta] " + fieldName,
                    type: fieldType,
                    criteriaPath: id + "." + fieldName
                });
            }
        }
        if (dsFields.length > 0) properties.fields.addList(dsFields);
    }

    return isc.DS.create(properties);
},

getDefaultData : function () { return []; },

setData : function (data) { this.data = data },

lookupSchema : function () {
    // see if we have a WebService instance with this serviceName / serviceNamespace
    var service;
    if (this.serviceName) service = isc.WebService.getByName(this.serviceName, this.serviceNamespace);
    else service = isc.WebService.get(this.serviceNamespace);

    if ((this.serviceNamespace || this.serviceName) && service == null) {
        this.logWarn("Could not find WebService definition: " +
                     (this.serviceName ? "serviceName: " + this.serviceName : "") +
                     (this.serviceNamespace ? "   serviceNamespace: " + this.serviceNamespace : ""));
    }
    
    // If this.dataSource is not a String, we shouldn't have ended up here
    if (!isc.isA.String(this.dataSource)) {
        this.logWarn("this.dataSource was not a String in lookupSchema");
        return;
    }
 
    var ds; 
    if (service) ds = service.getSchema(this.dataSource);
    // note return this.dataSource if the lookup failed so that this.dataSource is still set to
    // the String value, even if we failed to look up the DataSource, since the service may
    // load later
    return ds || this.dataSource; 
},


//>@method DataBoundComponent.fieldValuesAreEqual()
// Compares two values in the context of the passed field and returns true if they're equal.  
// Used by components that need to check whether edited values are equivalent to saved values.
// <P>
// If passed a string field-name, a field with that name must be available either in the DBC 
// directly, or in the associated +link{dataBoundComponent.dataSource, dataSource}.  If a field
// can be found, it's data type is used as the context for comparison.  Otherwise, a simple 
// JavaScript comparison (a == b) is used, except in the case of Date values, which are 
// compared as logical dates or times if either value is flagged as logical, or as milliseconds
// otherwise.
// @param field (object | String) field object or name of the field in the context of which the
//                                values should be compared
// @param value1 (any) first value to be compared
// @param value2 (any) second value to be compared
// @visibility internal
//<
// Leave visibility internal, but non obfuscated - we may allow developers to override this for
// custom field types
// Used by the saveData flow to compare updated values (from the server) with 
// submitted values


fieldValuesAreEqual : function (field, value1, value2) {
    if (field != null) {
        // if passed field isn't an object, try to find one in fields, completeFields or DS 
        if (!isc.isAn.Object(field)) field = this.getUnderlyingField(field) || field;

        if (field.type != null) {
            // If the type is a SimpleType with a compareValues() impl, use that first
            var simpleType = isc.SimpleType.getType(field.type);
            if (simpleType && simpleType.compareValues) {
                return simpleType.compareValues(value1, value2, field) == 0;
            }
            if (isc.SimpleType.inheritsFrom(field.type, "datetime")) {
                if (isc.isA.Date(value1) && isc.isA.Date(value2)) {
                    return (Date.compareDates(value1, value2) == 0);
                }
            } else if (isc.SimpleType.inheritsFrom(field.type, "date")) {
                if (isc.isA.Date(value1) && isc.isA.Date(value2)) {
                    return (Date.compareLogicalDates(value1, value2) == 0);
                }
    
            
            } else if (field.type == "valueMap") {
                if (isc.isAn.Array(value1) && isc.isAn.Array(value2)) {
                    return value1.equals(value2)
                
                } else if (isc.isAn.Object(value1) && isc.isAn.Object(value2)) {
                    for (var i in value1) {
                        if (value2[i] != value1[i]) return false;
                    }
                    
                    for (var j in value2) {
                        if (value1[j] != value2[j]) return false;
                    }
                
                    // everything matched
                    return true;
                }
            }
        }
    }
    
    if (!isc.isAn.Object(field)) {
        // If no field was detected and both values are Date instances, compare them according
        // to whether they are logical dates or times, or do a basic millisecond comparison
        // otherwise.  If this behavior misfires for whatever reason, the developer should 
        // provide a field.
        if (isc.isA.Date(value1) && isc.isA.Date(value2)) {
            if (value1.logicalDate || value2.logicalDate) 
                return isc.Date.compareLogicalDates(value1, value2) == 0;
            else if (value1.logicalTime || value2.logicalTime)
                return isc.Time.compareLogicalTimes(value1, value2) == 0;
            else return value1.getTime() == value2.getTime();
        }
    }
   
    // no matter what the type, if we get this far, the field type had no custom comparison 
    // mechanism - just rely on the "==" comparison
    
    if (value1 == value2) return true;
    
    // return false
    return false;
},

//> @attr dataBoundComponent.useFlatFields (boolean : null : IR)
// The <code>useFlatFields</code> flag causes all simple type fields anywhere in a nested
// set of DataSources to be exposed as a flat list for form binding.  
// <P>
// <code>useFlatFields</code> is typically used with imported metadata, such as 
// +link{XMLTools.loadXMLSchema,XML Schema} from a 
// +link{XMLTools.loadWSDL,WSDL-described web service}, as a means of eliminating levels of XML
// nesting that aren't meaningful in a user interface, without the cumbersome and fragile
// process of mapping form fields to XML structures.
// <P>
// For example, having called +link{webService.getInputDS()} to retrieve the input message
// schema for a web service operation whose input message looks like this:
// <pre>
// &lt;FindServices&gt;
//     &lt;searchFor&gt;search text&lt;/searchFor&gt;
//     &lt;Options&gt;
//         &lt;caseSensitive&gt;false&lt;/caseSensitive&gt;
//     &lt;/Options&gt;
//     &lt;IncludeInSearch&gt;
//         &lt;serviceName&gt;true&lt;/serviceName&gt;
//         &lt;documentation&gt;true&lt;/documentation&gt;
//         &lt;keywords&gt;true&lt;/keywords&gt;
//     &lt;/IncludeInSearch&gt;
// &lt;/FindServices&gt;
// </pre>
// Setting <code>useFlatFields</code> on a +link{DynamicForm} that is bound to this input
// message schema would result in 5 +link{FormItem,FormItems} reflecting the 5 simple type
// fields in the message.
// <P>
// For this form, the result of +link{dynamicForm.getValues(),form.getValues()} might look
// like:
// <P>
// <pre>{
//    searchFor: "search text",
//    caseSensitive: false,
//    serviceName: true,
//    documentation : true,
//    keywords : true
// }</pre>
// When contacting a +link{WebService,WSDL web service}, these values can be automatically
// mapped to the structure of the input message for a web service operation by setting
// +link{wsRequest.useFlatFields} (for use with +link{webService.callOperation()}) or by setting
// +link{dsRequest.useFlatFields} (for use with a +link{DataSource} that is
// +link{group:wsdlBinding,bound to a WSDL web service} via
// +link{operationBinding.wsOperation}).  
// <P>
// Using these two facilities in conjunction (component.useFlatFields and
// request.useFlatFields) allows gratuitous nesting to be consistently bypassed in both the user
// presentation and when providing the data for XML messages.
// <P>
// You can also set +link{operationBinding.useFlatFields} to automatically enable 
// "flattened" XML serialization (request.useFlatFields) for all DataSource requests of a
// particular operationType.
// <P>
// Note that <code>useFlatFields</code> is not generally recommended for use with structures
// where multiple simple type fields exist with the same name, however if used with such a
// structure, the first field to use a given name wins.  "first" means the first field
// encountered in a depth first search.  "wins" means only the first field will be present as a
// field when data binding.
// 
// @visibility external
//<

//> @attr dataBoundComponent.showFilterFieldsOnly (boolean : null : IRWA)
// If this attribute is true any +link{dataSourceField.canFilter,canFilter:false} fields
// specified on the dataSource will not be shown unless explicitly included in this component's
// +link{dataBoundComponent.fields,fields array}
//<
// Exposed and defaulted to true on SearchForm 

// minimal implementation of setFields()
setFields : function (fields) {
	// combine specified "fields" with reference declarations in the dataSource
	fields = this.bindToDataSource(fields);
    this.updateFieldDependencies();
    this.fields = fields;
},

// common routine; call in all component implementations
updateFieldDependencies : function () {
    
    this.invalidateUserCache();
    // recreate common varMaps for the component from the varMaps of the individual fields
    this.rebuildAllFieldsFormulaVarMaps();
},

getSerializeableFields : function (removeFields, keepFields) {
    removeFields = removeFields || []; 

	// data may actually be valid in some cases - but removing it is a good default.
	removeFields.addList(["zIndex", "data"]);
		
	// don't save ID if it's auto-generated
	if (this.ID && this.ID.startsWith("isc_")) removeFields.add("ID");

	// if this component is bound to a datasource, don't serialize its fields or items
    
	if (this.dataSource) removeFields.addList(["fields", "items"]);

	// we only want to serialize children created explicitly by a developer - not children
    // auto-created by an ISC component (such as the ListGrid header) 
    
	if (this.getClassName() != "Canvas" && this.getClassName() != "Layout") {
        removeFields.add("children");
    }

	return this.Super("getSerializeableFields", [removeFields, keepFields], arguments);
},


addField : function (field, index, fields) {
    if (field == null) return;

    if (fields == null) fields = (this.fields || this.items || isc._emptyArray);
    fields = fields.duplicate();
 
    // if this field already exists, replace it
    
    var existingField = isc.Class.getArrayItem(field.name, this.getAllFields(), this.fieldIdProperty);
    if (existingField) fields.remove(existingField);
   
    // If index wasn't passed, add at the end (Array.addAt() defaults to the beginning)
    // Also, if the requested index is greater than the size of the array, just add to
    // the end.  This is a corner case that can happen in VB, where the same index is 
    // being used for two different things (index into the list of a DBC's fields and 
    // index into the list of a DBC's children in the componentTree - sometimes the same 
    // thing, but not necessarily so)
    if (index == null || index > fields.length) index = fields.length;
    fields.addAt(field, index);
    this.setFields(fields);
},

removeField : function (fieldName, fields) {
    if (fields == null) fields = (this.fields || this.items || isc._emptyArray);
    fields = fields.duplicate();
    
    // Cope with being passed an object rather than a name
    var name = fieldName.name ? fieldName.name : fieldName;
    fields.remove(fields.find("name", name));
    this.setFields(fields);
},

// DataBound Component Methods
// --------------------------------------------------------------------------------------------
//> @groupDef dataBoundComponentMethods
// An Action Method initiates an orchestrated client-server flow that stores or retrieves data
// and updates one or more components.
// <P>
// For example, the +link{DynamicForm.saveData(),editor.saveData()} Action Method saves the
// record currently being edited in the form, transparently handling the trip to the server,
// standard error conditions such as validation errors (whether the validation error
// happens on the client or server), and update of client-side caches.
// <P>
// Action Methods are available on DataBoundComponents.
//
// @treeLocation Client Reference/Data Binding
// @see interface:DataBoundComponent
// @title DataBound Component Methods
// @visibility external
//<

// NOTE: the DataBound Component Methods are mostly implemented directly on Canvas, and act
// as a basic framework for building a DataBound widget, however, we document them as existing
// on the specific components where it actually makes sense to call them.

//> @method listGrid.fetchData()
// @include dataBoundComponent.fetchData()
// @group dataBoundComponentMethods
// @visibility external
// @example databoundFetch
//<

//> @method listGrid.exportData()
// @include dataBoundComponent.exportData()
// @group dataBoundComponentMethods
// @visibility external
//<

//>	@attr listGrid.autoFetchData       (boolean : false : IR)
// @include dataBoundComponent.autoFetchData
// @group databinding
// @visibility external
// @example fetchOperation
//<

// Note: listGrid.autoFetchTextMatchStyle overridden and documented in ListGrid.js

//> @attr listGrid.initialCriteria   (Criteria : null :IR)
// @include dataBoundComponent.initialCriteria
// @group dynamicCriteria
// @visibility external
//<

//> @method listGrid.filterData()
// @include dataBoundComponent.filterData()
// @group dataBoundComponentMethods
// @visibility external
// @example databoundFilter
//<

//> @method listGrid.fetchRelatedData()
// @include dataBoundComponent.fetchRelatedData()
// @group dataBoundComponentMethods
// @visibility external
//<

//> @method listGrid.clearCriteria()
// @include dataBoundComponent.clearCriteria()
// @group dataBoundComponentMethods
// @visibility external
// @example databoundFilter
//<

//> @method listGrid.getCriteria()
// Retrieves a copy of the current criteria for this component (may be null).
// <P>
// Note: if +link{listGrid.showFilterEditor} is true, the criteria returned by this method may not
// match the values currently displayed in the filter editor, since the user may have entered
// values which have not yet been applied to our data. +link{listGrid.getFilterEditorCriteria()}
// may be used to retrieve the current criteria displayed in the filterEditor.
// @include dataBoundComponent.getCriteria()
// @group dataBoundComponentMethods
// @visibility external
//<
//> @method listGrid.setCriteria()
// Sets this component's filter criteria.
// Default implementation calls this.data.setCriteria().
// <P>
// Note: if +link{listGrid.showFilterEditor} is true, the +link{listGrid.setFilterEditorCriteria()}
// method may be used to update the values displayed in the filter editor without effecting the
// data object.
// @include dataBoundComponent.setCriteria()
// @group dataBoundComponentMethods
// @visibility external
//<
// Overridden in ListGrid.js to apply the new criteria to the filter editor if it is showing


//> @method listGrid.invalidateCache()
// @include dataBoundComponent.invalidateCache()
// @group dataBoundComponentMethods
// @visibility external
//<

//> @method listGrid.refreshData()
// @include dataBoundComponent.refreshData()
// @group dataBoundComponentMethods
// @visibility external
//<

//> @method listGrid.willFetchData()
// @include dataBoundComponent.willFetchData()
// @visibility external
//<


//> @method listGrid.addData()
// @include dataBoundComponent.addData()
// @group dataBoundComponentMethods
// @visibility external
// @example databoundAdd
//<

//> @method listGrid.updateData()
// @include dataBoundComponent.updateData()
// @group dataBoundComponentMethods
// @visibility external
// @example databoundUpdate
//<

//> @method listGrid.removeSelectedData()
// @include dataBoundComponent.removeSelectedData()
// @group dataBoundComponentMethods
// @visibility external
// @example removeOperation
//<

//> @method listGrid.getSelectedRecord()
// Returns the first selected record in this grid.
// <p>
// This method is appropriate if the +link{attr:selectionType,selectionType} is
// <smartclient>"single",</smartclient>
// <smartgwt>{@link com.smartgwt.client.types.SelectionStyle#SINGLE},</smartgwt>
// or if you only care about the first selected record in a multiple-record selection. To access
// all selected records, use +link{method:getSelection()} instead.
// <p>
// <strong>NOTE:</strong> If a record is returned, it should be treated as read-only and not
// modified.
//      @group  selection
//      @return (ListGridRecord) the first selected record, or null if no record is selected.
// @visibility external
// @example databoundRemove
//<

//> @method listGrid.getSelectedRecords()
// Returns all selected records in this grid.
// <p>
// <strong>NOTE:</strong> Records in the returned array should be treated as read-only and not
// modified.
// @param [excludePartialSelections] (boolean) When true, partially selected records will not be returned.
//                                   Otherwise, both fully and partially selected records are
//                                   returned.
// @return (Array of ListGridRecord) array of selected records, which will be empty if no record
// is selected.
// @group  selection
// @visibility external
//<

//> @method treeGrid.fetchData()
// Uses a "fetch" operation on the current +link{DataSource,grid.dataSource} to retrieve data
// that matches the provided criteria, and displays the matching data in this component as a
// tree.
// <P>
// This method will create a +link{ResultTree} to manage tree data, which will
// subsequently be available as <code>treeGrid.data</code>.  DataSource records
// returned by the "fetch" operation are linked into a tree structure according to
// +link{dataSourceField.primaryKey,primaryKey} and
// +link{dataSourceField.foreignKey,foreignKey} declarations on DataSource fields.  See the
// +link{group:treeDataBinding} topic for complete details.
// <P>
// By default, the created ResultTree will use folder-by-folder load on demand, asking the
// server for the children of each folder as the user opens it.
// <P>
// The +link{ResultTree} created by <code>fetchData()</code> can be customized by setting
// +link{listGrid.dataProperties} to an Object containing properties and methods to apply to
// the created ResultTree.  For example, the property that determines whether a node is a
// folder (+link{Tree.isFolderProperty,isFolderProperty}) can be customized, or
// level-by-level loading can be disabled via
// +link{resultTree.loadDataOnDemand,loadDataOnDemand:false}.
// <P>
// If +link{loadDataOnDemand} is true, this grid will issue fetch requests each time the
// user opens a folder to load its child data.<br>
// The criteria on this fetch request will consist of the appropriate value for the
// foreignKey field, combined with the criteria passed to <code>fetchData()</code>
// when the data was first loaded.
// This allows you to retrieve multiple different tree structures from the same DataSource.
// However note that the server is expected
// to always respond with an intact tree - returned nodes which do not have parents are dropped
// from the dataset and not displayed.
// <P>
// The callback passed to <code>fetchData</code> will fire once, the first time that data is
// loaded from the server.  If using folder-by-folder load on demand, use the
// +link{resultTree.dataArrived()} notification to be notified each time new nodes are loaded.
// <P>
// Note that when calling 'fetchData()', changes to criteria may or may not result in a
// DSRequest to the server due to client-side filtering (see +link{ResultTree.fetchMode}. 
// You can call willFetchData(criteria) to determine if new criteria will result in a 
// server fetch.
// <P>
// If you need to force data to be re-fetched, you can call invalidateCache() and 
// new data will automatically be fetched from the server using the current criteria 
// and sort direction.<br>
// When using invalidateCache() there is no need to also call fetchData() and 
// in fact this could produce unexpected results.
// <P>
//
// @include dataBoundComponent.fetchData()
// @group dataBoundComponentMethods
// @visibility external
//<

//> @method treeGrid.filterData()
// Retrieves data that matches the provided criteria and displays the matching data in this
// component.
// <P>
// This method behaves exactly like +link{treeGrid.fetchData()} except that
// +link{dsRequest.textMatchStyle} is automatically set to "substring" so that String-valued
// fields are matched by case-insensitive substring comparison.
//
// @include dataBoundComponent.filterData()
// @group dataBoundComponentMethods
// @visibility external
//<

//> @method tileGrid.exportData()
// @include dataBoundComponent.exportData()
// @group dataBoundComponentMethods
// @visibility external
//<

//> @attr detailViewer.initialCriteria   (Criteria : null :IR)
// @include dataBoundComponent.initialCriteria
// @visibility external
//<

//> @attr detailViewer.autoFetchData       (boolean : false : IR)
// @include dataBoundComponent.autoFetchData
// @group databinding
// @visibility external
//<

//> @method detailViewer.exportData()
// @include dataBoundComponent.exportData()
// @group dataBoundComponentMethods
// @visibility external
//<

//>	@attr dynamicForm.autoFetchData       (boolean : false : IR)
// @include dataBoundComponent.autoFetchData
// @group databinding
// @visibility external
//<

//>	@attr dynamicForm.autoFetchTextMatchStyle       (TextMatchStyle : null : IR)
// @include dataBoundComponent.autoFetchTextMatchStyle
// @group databinding
// @visibility external
//<

//> @attr dynamicForm.initialCriteria   (Criteria : null :IR)
// @include dataBoundComponent.initialCriteria
// @visibility external
//<



// Filtering
// -----------------------------------------------------------------------------

// whether this control should show end-user editing controls (if it is capable of doing so).
setCanEdit : function (newValue) {
    this.canEdit = newValue;
},

//>	@method dataBoundComponent.filterData()
// Retrieves data that matches the provided criteria and displays the matching data in this
// component.
// <P>
// This method behaves exactly like +link{listGrid.fetchData()} except that
// +link{dsRequest.textMatchStyle} is automatically set to "substring" so that String-valued
// fields are matched by case-insensitive substring comparison.
//
// @param [criteria]          (Criteria)	  Search criteria. 
//                      If a +link{DynamicForm} is passed in as this argument
//                      instead of a raw criteria object, will be derived by calling
//                      +link{DynamicForm.getValuesAsCriteria()}
// @param [callback]          (DSCallback)  callback to invoke when a fetch is complete.  Fires
//                                          only if server contact was required; see
//                                          +link{listGrid.fetchData,fetchData()} for details
// @param [requestProperties] (DSRequest)   for databound components only - optional 
//                           additional properties to set on the DSRequest that will be issued
//
// @see dataBoundComponent.willFetchData()
// @group dataBoundComponentMethods
// @visibility internal
//<
filterData : function (criteria, callback, requestProperties) {
    this._filter("filter", criteria, callback, requestProperties);
},

//> @method dataBoundComponent.fetchData()
// Retrieves data from the DataSource that matches the specified criteria.
// <p>
// When <code>fetchData()</code> is first called, if data has not already been provided via
// +link{listGrid.setData(),setData()}, this method will create a +link{class:ResultSet}, which will be
// configured based on component settings such as +link{attr:dataBoundComponent.fetchOperation}
// and +link{attr:dataBoundComponent.dataPageSize}, as well as the general purpose
// +link{listGrid.dataProperties}.  The created ResultSet will automatically send a DSRequest
// to retrieve data from +link{listGrid.dataSource,listGrid.dataSource}, and from then on will  
// automatically manage paging through large datasets, as well as performing filtering and
// sorting operations inside the browser when possible - see the +link{ResultSet} docs for
// details.
// <p>
// <b>NOTE:</b> do not use <b>both</b> +link{dataBoundComponent.autoFetchData,autoFetchData:true} <b>and</b> a
// call to <code>fetchData()</code> - this may result in two DSRequests to fetch data.  Use
// either +link{dataBoundComponent.autoFetchData,autoFetchData} and +link{criteria} <b>or</b> a manual call to fetchData()
// passing criteria.
// <p>
// Whether a ResultSet was automatically created or provided via +link{listGrid.setData(),setData()}, subsequent
// calls to fetchData() will simply call +link{resultSet.setCriteria()}.
// <p>
// Changes to criteria may or may not result in a DSRequest to the server due to
// +link{resultSet.useClientFiltering,client-side filtering}.  You can call
// +link{dataBoundComponent.willFetchData,willFetchData(criteria)} to determine if new criteria will result in a
// server fetch.
// <P>
// If you need to force data to be re-fetched, you can call
// +link{ListGrid.invalidateCache,invalidateCache()} and new data will automatically be fetched
// from the server using the current criteria and sort direction.  <b>NOTE:</b> when using
// <code>invalidateCache()</code> there is no need to <b>also</b> call <code>fetchData()</code>
// and in fact this could produce unexpected results.
// <p>
// This method takes an optional callback parameter (set to a +link{DSCallback}) to fire when
// the fetch completes. Note that this callback will not fire if no server fetch is performed.
// In this case the data is updated synchronously, so as soon as this method completes you
// can interact with the new data. If necessary, you can use
// +link{dataBoundComponent.willFetchData,willFetchData()} to determine whether or not a server
// fetch will occur when <code>fetchData()</code> is called with new criteria.
// <p>
// In addition to the callback parameter for this method, developers can use 
// +link{ListGrid.dataArrived(),dataArrived()} to be notified every time data is loaded.
// <p>
// By default, this method assumes a +link{textMatchStyle} of "exact"; that can be overridden
// by supplying a different value in the requestProperties parameter. 
// See +link{dataBoundComponent.willFetchData()};
//
// @param [criteria]          (Criteria)    Search criteria. If a +link{DynamicForm} is passed
//                                          in as this argument instead of a raw criteria 
//                                          object, will be derived by calling
//                                          +link{DynamicForm.getValuesAsCriteria()}
// @param [callback]          (DSCallback)  callback to invoke when a fetch is complete. Fires
//                                          only if server contact was required
// @param [requestProperties] (DSRequest)   additional properties to set on the DSRequest
//                                          that will be issued
//
// @group dataBoundComponentMethods
// @visibility internal
// @see listGrid.refreshData
//<
// NOTE: this doc is marked internal because listGrid.fetchData() @includes it and makes it
// external
fetchData : function (criteria, callback, requestProperties) {
    if (!requestProperties) requestProperties = {};
    if (!requestProperties.textMatchStyle) requestProperties.textMatchStyle = "exact";
    this._filter("fetch", criteria, callback, requestProperties);
},

_canExportField : function (field) {
    return (this.canExport != false && field.canExport != false &&
            !field.hidden)
    ;
},

//>	@method dataBoundComponent.exportData()
// Sends the current filter criteria and sort direction to the server, then exports data in the
// requested +link{dsRequest.exportAs,exportFormat}.
// <P>
// A variety of DSRequest settings, such as 
// +link{dsRequest.exportAs,exportAs} and +link{dsRequest.exportFilename}, affect the 
// exporting process: see +link{dsRequest.exportResults, exportResults} for further detail.
// <P>
// Note that data exported via this method does not include any client-side formatting and
// relies on both the SmartClient server and server-side DataSources.  To export client-data 
// with formatters applied, 
// see +link{listGrid.exportClientData, exportClientData}, which still requires the
// SmartClient server but does not rely on server-side DataSource definitions (.ds.xml files).
// <P>
// For more information on exporting data, see +link{dataSource.exportData()}.
//
// @param [requestProperties] (DSRequest)   additional properties to set on the DSRequest
//                                            that will be issued
// @param [callback] (DSCallback)   callback to invoke on completion.  Note that this parameter
//                                  only applies where +link{dsRequest.exportToClient} is 
//                                  explicitly set to false, because  file downloads do not 
//                                  provide ordinary SmartClient callbacks 
//
// @group dataBoundComponentMethods
// @visibility external
//<
exportData : function (requestProperties, callback) {
    if (!requestProperties) requestProperties = {};

    var sort = this.getSort();
    if (sort) {
        requestProperties.sortBy = isc.DS.getSortBy(sort);
    } else if (this.sortField) {
        requestProperties.sortBy = (Array.shouldSortAscending(this.sortDirection) ? "" : "-") 
            + this.sortField;
    }

    if (!requestProperties.textMatchStyle) {
        // if not provided, set the textMatchStyle to that already in use in this component
        var context = this.data.context;
        if (context && context.textMatchStyle) {
            requestProperties.textMatchStyle = context.textMatchStyle;
        }
    }

    if (!this.exportAll && !requestProperties.exportFields) {
        // pass up only visible fields
        var vFields = this.exportFields,
            outputs = "",
            ds = this.getDataSource()
        ;

        if (!vFields) {
            vFields = [];
            for (var i = 0; i < this.fields.length; i++) {
                var field = this.fields.get(i),
                    dsField = ds ? ds.getField(field.name) : null
                ;

                if (this._canExportField(field)) {
                    if (field.includeFrom || (dsField && dsField.includeFrom)) {
                        var includeFrom = field.includeFrom ? field.includeFrom : 
                                dsField.includeFrom;

                        outputs += field.name + "!" + includeFrom + ",";
                        vFields.add(field.name);
                    } else {
                        vFields.add(field.name);
                    }

                    if (field.displayField && !field.optionDataSource) {
                        vFields.add(field.displayField);
                    }
                }
            }
        }
        if (outputs.length > 1) {
            if (outputs.endsWith(",")) {
                outputs = outputs.substring(0, outputs.length-1);
            }
            requestProperties.additionalOutputs = outputs;
        }        
        if (vFields && vFields.length > 0) requestProperties.exportFields = vFields;
    }

    var wkFields = requestProperties.exportFields || this.exportFields || this.fields,
        exportFieldTitles = {},
        ds = this.getDataSource();
    
    for (var i = 0; i < wkFields.length; i++) {
        var field = wkFields[i];
        var fieldName;
        if (isc.isA.String(field)) {
            fieldName = field;
            field = this.getField(fieldName);
            if (!field && ds != null) field = ds.getField(fieldName);
        }
        if (field) {
            exportFieldTitles[field.name] = this.htmlUnescapeExportFieldTitle(field.exportTitle || field.title);
        } else {
            exportFieldTitles[fieldName] = this.htmlUnescapeExportFieldTitle(fieldName);
        }
    }
    requestProperties.exportFieldTitles = exportFieldTitles;

    if (requestProperties.exportRawValues == null) {
        requestProperties.exportRawValues = true;
    }

    if (this.headerHeight && this.exportHeaderHeights) {
        requestProperties.exportHeaderHeight = this.headerHeight;
    }

    // header spans
    var exportTitles = {};
    if (this.headerSpans && requestProperties.exportShowHeaderSpanTitles !== false) {
        requestProperties.exportHeaderSpans = 
            this.prepareHeaderSpansForExport(this.headerSpans, this.getAllFields(), exportTitles);
    }

    // non-spanned fields
    // 'exportOtherFields' is passed to the server so that it knows what titles to use for
    // non-spanned fields.
    requestProperties.exportOtherFields = {};
    for (var i = 0; i < wkFields.length; ++i) {
        var fieldName = wkFields[i];
        if (!exportTitles.hasOwnProperty(fieldName)) {
            requestProperties.exportOtherFields[fieldName] = exportFieldTitles[fieldName];
        }
    }

    if (this.exportFieldWidths && isc.isAn.Array(this.fields) && this.getFieldWidth) {
        requestProperties.exportFieldPixelWidths = this.getFieldPixelWidths();
		requestProperties.exportWidthScale = this.exportWidthScale;
    }

    if (requestProperties.exportWrapHeaderTitles == null) {
        requestProperties.exportWrapHeaderTitles = this.exportWrapHeaderTitles;
    }

    if (this.exportFieldAlignments && isc.isAn.Array(this.fields)) {
        requestProperties.exportAlignments = this.getFieldAlignments();
    }
    
    if (requestProperties.exportPropertyIdentifier == null) {
        requestProperties.exportPropertyIdentifier = "name";
    }

    this.getDataSource().exportData(this.getCriteria(), requestProperties, callback, this);
},

getFieldPixelWidths : function() {
	var widths = [];
	for (var i = 0; i < this.fields.length; i++) {
        if (this.fields[i].exportFieldWidth === false) {
            widths[i] = -1;
        } else {
            widths[i] = this.getFieldWidth(i);
        }
	}
	return widths;
},

//> @method dataBoundComponent.getFieldAlignments()
// Returns an array of +link{type:Alignment,field alignments} for this grid
// @return (Array of Alignment) 
// @visibility external
//<
getFieldAlignments : function() {
	var alignments = [];
	for (var i = 0; i < this.fields.length; i++) {
        alignments[i] = []
        // If alignments are not explicitly stated, we force SmartClient's default values 
        // for export rather than allow Excel or whatever to make the decision.  We do this
        // so that there is the same consistent columnar alignment seen in the browser, rather
        // than allowing Excel to make its own decision about each cell
        var field = this.fields[i],
            type = isc.SimpleType.getBaseType(field.type);
        if (field.align) {
            alignments[i][0] = field.align;
        } else if (type == "integer" || type == "float" || type == "date" || type == "time") {
            alignments[i][0] = "right";
        } else {
            alignments[i][0] = "left";
        }
        if (field.cellAlign) {
            alignments[i][1] = field.cellAlign;
        } else {
            alignments[i][1] = alignments[i][0];
        }
	}
	return alignments;
},

//> @method dataBoundComponent.setCriteria()
// Sets this component's filter criteria.
// Default implementation calls this.data.setCriteria().
// @param (Criteria or AdvancedCriteria) new criteria to show
//<
setCriteria : function (criteria) {
    if (this.data && this.data.setCriteria) {
        this.data.setCriteria(criteria);
    } else {
        // if there is no data yet, set initial criteria to parameter criteria
        this.initialCriteria = criteria;
    }
},

//> @method dataBoundComponent.getCriteria()
// Retrieves a copy of the current criteria for this component, excluding criteria specified by
// +link{dataBoundComponent.implicitCriteria}.  May return null.
// @return (Criteria) current filter criteria
//<
// Overridden for CubeGrids
getCriteria : function (excludeImplicit) {
    var result;
    if (!this.isDrawn() && (!this.data || this.data.getLength() == 0)) {
        result = isc.shallowClone(this.initialCriteria);
    } else if (this.data && this.data.getCriteria) {
        if (isc.isA.Tree(this.data)) {
            
            result = isc.shallowClone(this.data.getCriteria(this.getDataSource()));
        } else {
            result = isc.shallowClone(this.data.getCriteria());
        }
    } else result = null;

    return result;
},

//>	@attr dataBoundComponent.autoFetchData (boolean : false : IR)
// If true, when this component is first drawn, automatically call <code>this.fetchData()</code>.
// Criteria for this fetch may be picked up from +link{initialCriteria}, and textMatchStyle may
// be specified via +link{listGrid.autoFetchTextMatchStyle,autoFetchTextMatchStyle}.
// <P>
// <span style='color:red'>NOTE:</span> if <code>autoFetchData</code> is set, calling
// +link{listGrid.fetchData(),fetchData()} before draw will cause two requests to be issued, one from the manual
// call to fetchData() and one from the autoFetchData setting.  The second request will use
// only +link{initialCriteria} and not any other criteria or settings from the first request.
// Generally, turn off autoFetchData if you are going to manually call +link{listGrid.fetchData(),fetchData()} at any time.
//
// @group dataBoundComponentMethods
// @visibility external
// @see listGrid.fetchData()
//<

// Called at draw() - if we are databound, and autoFetchData is true, do a one time fetch on initial draw.
doInitialFetch : function () {
    var fetchQueued = false;
    if (this.autoFetchData && !this._initialFetchFired && this.fetchData) {
 
        if (!this.dataSource) {
            this.logWarn("autoFetchData is set, but no dataSource is specified, can't fetch");
        } else {
            // Queue the fetch - this means we can batch up any requests our children make on draw
            // and send them all off together
            // Specific use case: this means if a ListGrid is autoFetchData:true and has a field
            // with an optionDataSource we can use the same transaction to fetch the valid options
            // as to fetch the LG data
            fetchQueued = !isc.RPCManager.startQueue();
            // getInitialCriteria() picks up this.initialCriteria
            // getInitialFetchContext() picks up this.autoFetchTextMatchStyle            
            this.fetchData(this.getInitialCriteria(), null, this.getInitialFetchContext());
            
            this._initialFetchFired = true;
        }        
    }
    return fetchQueued;
},

// getInitialCriteria() - used to retrieve the initialCriteria when performing auto-fetch of data
getInitialCriteria : function () {
    if (!this.initialCriteria) return null;
    return isc.shallowClone(this.initialCriteria);
},

getInitialFetchContext : function () {
    var context = {};
    context.textMatchStyle = this.autoFetchTextMatchStyle;
    return context;
},

//> @attr dataBoundComponent.autoFetchTextMatchStyle (TextMatchStyle : null : IR)
// If +link{autoFetchData} is <code>true</code>, this attribute allows the developer to
// specify a textMatchStyle for the initial +link{listGrid.fetchData(),fetchData()} call.
// @group dataBoundComponentMethods
// @visibility internal
//<

//> @attr dataBoundComponent.initialCriteria (Criteria : null : IR)
// Criteria to be used when +link{autoFetchData} is set.
// @visibility external
//<

//> @attr dataBoundComponent.implicitCriteria (Criteria : null : IRW)
// Criteria that are never shown to or edited by the user and are cumulative with any criteria
// provided via +link{dataBoundComponent.initialCriteria}, +link{dataBoundComponent.setCriteria}
// etc.
// @visibility external
//<

getImplicitCriteria : function () {
    if (!this.implicitCriteria) return null;
    return isc.shallowClone(this.implicitCriteria);
},

//> @method dataBoundComponent.fetchRelatedData()
// Based on the relationship between the DataSource this component is bound to and the
// DataSource specified as the "schema" argument, call fetchData() to retrieve records in this
// grid that are related to the passed-in record.
// <P>
// Relationships between DataSources are declared via +link{dataSourceField.foreignKey}.
// <P>
// For example, given two related DataSources "orders" and "orderItems", where we want to fetch
// the "orderItems" that belong to a given "order".  "orderItems" should declare a field that
// is a +link{dataSourceField.foreignKey,foreignKey} to the "orders" table (for example, it
// might be named "orderId" with foreignKey="orders.id").  Then, to load the records related to
// a given "order", call fetchRelatedData() on the component bound to "orderItems", pass the
// "orders" DataSource as the "schema" and pass a record from the "orders" DataSource as the
// "record" argument.
//
// @param record              (ListGridRecord) DataSource record
// @param schema              (Canvas or DataSource or ID) schema of the DataSource record, or
//                            DataBoundComponent already bound to that schema
// @param [callback]          (DSCallback)  callback to invoke on completion
// @param [requestProperties] (DSRequest)   additional properties to set on the DSRequest
//                                            that will be issued
//
// @visibility internal
//<
fetchRelatedData : function (record, schema, callback, requestProperties) {
    var otherDS = isc.isA.DataSource(schema) ? schema : 
            isc.isA.String(schema) ? isc.DS.get(schema) :
            isc.isA.Canvas(schema) ? schema.dataSource : null;
    if (!otherDS) {
        this.logWarn("schema not understood: " + this.echoLeaf(schema));
        return;
    }
    var relationship = this.getDataSource().getTreeRelationship(otherDS);

	// form criteria to find related records
    var criteria = {};
    criteria[relationship.parentIdField] = record[relationship.idField];

    this.fetchData(criteria, callback, requestProperties);
},

//>	@method dataBoundComponent.clearCriteria()
// Clear the current criteria used to filter data.
//
// @param [callback]          (DSCallback)  callback to invoke on completion
// @param [requestProperties] (DSRequest)   additional properties to set on the DSRequest
//                                            that will be issued
//
// @see listGrid.fetchData()
//
// @group dataBoundComponentMethods
// @visibility internal
//<
clearCriteria : function (callback, requestProperties) {
    this._filter("filter", null, callback, requestProperties);
},

_filter : function (type, criteria, callback, requestProperties) {
    if (isc._traceMarkers) arguments.__this = this;
    
    requestProperties = this.buildRequest(requestProperties, type, callback);

    // Resolve dynamicCriteria except for PickListMenus. A PickListMenu
    // is provided criteria from optionCriteria handled by the containing
    // FormItem.
    if (criteria && (!isc.isA.PickListMenu || !isc.isA.PickListMenu(this))) {

        if (!isc.DS.isAdvancedCriteria(criteria)) {
            var criteria = isc.addProperties({}, criteria);

            if (criteria.operator && criteria.criteria) {
                // Advanced format but missing constructor
                criteria._constructor = "AdvancedCriteria";
            } else if (criteria.fieldName && criteria.operator) {
                // Shorthand format
                criteria = {
                        _constructor: "AdvancedCriteria",
                        operator: "and",
                        criteria: isc.isAn.Array(criteria) ? criteria : [criteria]
                };
            }
        }

        if (isc.DS.isAdvancedCriteria(criteria)) {
            criteria = isc.DataSource.resolveDynamicCriteria(criteria, this.getRuleContext());
        }
    }

    // notification method fired when the user modifies the criteria in the filter editor
    // and hits the filter button / enter key.
    
    if (this.onFetchData != null) {
        this.onFetchData(criteria, requestProperties);
    }


    // support for dataBoundComponentField.includeFrom:<dataSourceID>.<fieldName>
    // for fields that are not in the dataSource but pick up their value from
    // a related dataSource
    // In this case simply update the outputs property of the request -- the
    // server will be responsible for actually getting the value from the other 
    // dataSource
    var completeFields = this.getAllFields();
    if (completeFields != null) {
        for (var i = 0; i < completeFields.length; i++) {
            if (completeFields[i].includeFrom != null && 
                this.getDataSource().getField(completeFields[i].name) == null) 
            {
                if (requestProperties.additionalOutputs == null) requestProperties.additionalOutputs = "";
                else requestProperties.additionalOutputs += ",";
                requestProperties.additionalOutputs += [
                        completeFields[i].name,
                        completeFields[i].includeFrom].join("!")
            
            }
        }
    }

    // handle being passed a criteria object (map of keys to values), or a filter-component
    if (criteria == null) {
        criteria = {};
    } else if (isc.isA.Class(criteria)) {
        // otherwise assume "filter" is something we can ask for filter criteria
        // (DynamicForm or ValuesManager)
        criteria = isc.DynamicForm.getFilterCriteria(criteria);
    }

    this.filterWithCriteria(criteria, requestProperties.operation, requestProperties);
},

filterWithCriteria : function (criteria, operation, context) {
    context.prompt = (context.prompt || isc.RPCManager.fetchDataPrompt);
    
    // get rid of empty criteria that come from raw form values
    var filterCriteria = criteria;
    if ( this.ignoreEmptyCriteria ) {
       filterCriteria = isc.DataSource.filterCriteriaForFormValues(criteria);
       
    // duplicate the criteria to ensure any downstream modification (EG touching dsRequest.data in
    // transformRequest) doesn't modify live widget criteria objects.
    // [filterCriteriaForFormValues already performs a duplication step].
    } else {
        filterCriteria = isc.addProperties({},filterCriteria);
    }
    
    filterCriteria = isc.DS.checkEmptyCriteria(filterCriteria);

    var dataModel = this.getData();

    // if not already viewing a result set/tree for this operation, create one for it
    
    if (this.useExistingDataModel(criteria, operation, context)) {
        var updatedModel = this.updateDataModel(filterCriteria, operation, context);
        if (updatedModel != null) dataModel = updatedModel;
    } else {
        dataModel = this.createDataModel(filterCriteria, operation, context);
    }
    // we will ask the result set for the data we currently need to display,
    // which will cause data to be fetched
    this.setData(dataModel);
        
    
    var data = this.data;
    if (!context._suppressFetch && this.requestVisibleRows != null && data != null) {
        var fetchDelay = data.fetchDelay;
        
        data.fetchDelay = 0;
        this.requestVisibleRows();
        data.fetchDelay = fetchDelay;
    }
},
    
shouldFilterLocalData : function () {
    if (this.filterLocalData != null) {
        return this.filterLocalData;
    } else {
        return this.dataPath != null;
    }
},


useExistingDataModel : function (criteria, operation, context) {
    var resultSet = this.getData();
    if (!this.dataObjectSupportsFilter(resultSet)) {
        resultSet = this.originalData;
        if (resultSet == null) return false;
        if (!this.dataObjectSupportsFilter(resultSet)) {
            return false;
        }
    }
    // at this point we know the data is a ResultSet or ResultTree and have its
    // criteria updated unless there's a custom operation mismatch.
    var resultSetOperation = resultSet.getOperationId("fetch");
    var opID;
    if (isc.isA.String(operation)) opID = operation;
    else if (operation) opID = operation.ID;
    if (opID == null) {
        var dataSource = resultSet.getDataSource(),
            dataSourceID = dataSource ? dataSource.ID : null,
            opType = operation ? operation.type : null;
            if (opType == null) opType = "fetch";
        if (dataSourceID != null) opID = dataSourceID + "_" + opType;            
    }
    return resultSetOperation == null || resultSetOperation == opID;
}, 


createDataModel : function (filterCriteria, operation, context) {
    //>DEBUG
    if (this.logIsInfoEnabled("ResultSet")) {
        this.logInfo("Creating new isc.ResultSet for operation '" + operation.ID + 
                      "' with filterValues: " + this.echoFull(filterCriteria), "ResultSet");
    }
    //<DEBUG
    var dataSource = this.getDataSource();

    if (!isc.isA.DataSource(dataSource)) {
        this.logWarn("No DataSource or invalid DataSource specified, can't create data model" +
                     this.getStackTrace());
        return null;
    }

    var resultSet = isc.addProperties({}, this.dataDefaults, this.dataProperties);
    
    // if context is included as part of dataProperties, combine it with any passed context
    // because we'll overwrite it on resultSet below
    if (resultSet.context) context = isc.addProperties({}, resultSet.context, context);

    if (this.dataFetchDelay) resultSet.fetchDelay = this.dataFetchDelay;

    isc.addProperties(resultSet, { operation:operation, filter:filterCriteria, context:context,
        componentId: this.ID});
        
    if (this.progressiveLoading === true || this.progressiveLoading === false) {
        isc.addProperties(resultSet, {progressiveLoading: this.progressiveLoading});
    }

    if (this.getSort != null) {
        // getSort will normalize specified sortField / initialSort to
        // this._sortSpecifiers
        // We run this as part of setData(), but by also doing this here we initialize the
        // ResultSet with the appropriate sort, meaning it will already be sorted / won't
        // need to re-fetch when setData() runs and sets up the sortSpecifiers on the ListGrid
        var sortSpecifiers = this.getSort();
        if (sortSpecifiers != null && sortSpecifiers.length > 0) {
            resultSet._sortSpecifiers = sortSpecifiers;
            resultSet._serverSortBy = isc.DS.getSortBy(resultSet._sortSpecifiers);
        }
    }

    if (this.shouldFilterLocalData()) {
        var data = this._originalData != null ? this._originalData : this.data;
        if (isc.isAn.Array(data)) {
            resultSet.allRows = data;
            resultSet.fetchMode = "local";
            resultSet.useClientFiltering = true;
            resultSet.useClientSorting = true;
            resultSet.disableCacheSync = true;
            resultSet.neverDropCache = true;
        } else {
            isc.logWarn("createDataModel method: data should be an array.");
        }
    }
    return dataSource.getResultSet(resultSet);
},

// updateDataModel() - apply criteria to our dataModel
// Default implementation assumes a resultSet - override if necessary

updateDataModel : function (filterCriteria, operation, context) {

    // tell the ResultSet/ResultTree the filter changed
    if (this.logIsDebugEnabled()) {
        this.logDebug("Setting filter to: " + this.echoFull(filterCriteria));
    }
      
    // update the context - this allows requestProperties like "showPrompt" / textMatchStyle
    // to change
    var resultSet = this.getData();
    // Handle the grid being grouped
    if (!this.dataObjectSupportsFilter(resultSet)) resultSet = this.originalData;
    
    if (!this.dataObjectSupportsFilter(resultSet)) {
        return resultSet;
    }
    resultSet.setContext(context);
    // if the ResultSet won't kick off an immediate fetch, kill the afterFlowCallback
    // This is the callback passed into fetchData(...) and would normally be cleared by
    // ResultSet.fetchDataReply()
    // If we don't clear it here, the next time a fetch occurs (EG via 'invalidateCache()') the
    // callback will occur (once) when that fetch completes.
    if (!resultSet.willFetchData(filterCriteria)) delete context.afterFlowCallback;
    resultSet.setCriteria(filterCriteria);
    
    return resultSet;
},

dataObjectSupportsFilter : function (dataObject) {
    return (isc.ResultSet && isc.isA.ResultSet(dataObject)) ||
            (isc.ResultTree && isc.isA.ResultTree(dataObject));
},

// add this here so that all dataBoundComponents have data available by default.
requestVisibleRows : function () {
    return this.data.get(0);
},

// Helper to return this widget's data as a list - singular data objects
// (eg: DynamicForm record) will be wrapped in a single element array.

getDataAsList : function () {
    var data = this.getData ? this.getData() : this.data;
    if (data == null) return null;
    
    if (!isc.isA.List(data)) data = [data];
    return data;  
},

//> @method dataBoundComponent.invalidateCache()
// Invalidate the current data cache for this databound component via a call to
// the dataset's <code>invalidateCache()</code> method, for example,
// +link{ResultSet.invalidateCache()}.
// <P>
// <b>NOTE:</b> there is no need to call <code>invalidateCache()</code> when a save operation
// is performed on a DataSource.  Automatic cache synchronization features will automatically
// update caches - see +link{ResultSet} for details.  If automatic cache synchronization isn't
// working, troubleshoot the problem using the steps suggested 
// +externalLink{http://forums.smartclient.com/showthread.php?t=8159#aGrid,in the FAQ} rather
// than just calling invalidateCache().  Calling <code>invalidateCache()</code> unnecessarily
// causes extra server load and added code complexity.
// <P>
// Calling <code>invalidateCache()</code> will automatically cause a new fetch to 
// be performed with the current set of criteria if data had been previously fetched and the
// component is currently drawn with data visible - there is no need to manually call
// fetchData() after invalidateCache() and this could result in duplicate fetches.  
// <P>
// While data is being re-loaded after a call to <code>invalidateCache()</code>, the widget is
// in a state similar to initial data load - it doesn't know the total length of the dataset
// and any APIs that act on records or row indices will necessarily fail and should not be
// called.  To detect that the widget is in this state, call +link{ResultSet.lengthIsKnown()}.
// <P>
// <code>invalidateCache()</code> only has an effect if this components dataset is a data
// manager class that manages a cache (eg ResultSet or ResultTree).  If data was provided as a
// simple Array or List, invalidateCache() does nothing.
// 
// @group dataBoundComponentMethods
// @visibility internal
// @see listGrid.refreshData
//<
invalidateCache : function () {
    if (this.data && this.data.invalidateCache != null) return this.data.invalidateCache();
    else if (this.isGrouped && isc.isA.ResultSet(this.originalData)) {
        // currently only valid for ListGrid: data is currently a Tree and has no
        // invalidateCache() - in order to preserve criteria, textMatchStyle, sort, etc, we
        // need to have the ResultSet from which this tree refetch.  Calling regroup right
        // after the cache is cleared sets us up to regroup when the data arrives
        
        this.originalData.setSort(this.data.getSort(), true);
        this.originalData.invalidateCache();
        this.regroup();
    }
},

//> @method dataBoundComponent.refreshData(callback)
// Unlike +link{listGrid.invalidateCache,invalidateCache} this will perform an asynchronous
// (background) refresh of this components data and then call the provided callback method on
// completion.
// <p>
// If refreshData is called while the grid is waiting for a response from +link{listGrid.fetchData}
// the refreshData call will be aborted. This is because the fetch has higher priority.
// <p>
// If +link{listGrid.fetchData} is called while the grid is waiting for a response from refreshData
// and the fetchData call has altered the criteria or sort specifiers, the refreshData call will
// be aborted.
// <p>
// If data is being edited or has been edited without being saved when refreshData is called, the
// data will be retained so you can save it after the refresh is complete. If you however want
// to throw away your edited but unsaved data when calling refreshData you first need to call
// +link{listGrid.discardAllEdits} which will discard any unsaved edited data.
// <p>
// In order to use refreshData() this grid needs to have a +link{DataSource} associated with it.
//
// @param [callback]    (DSCallback) callback method to run once the refresh completes.
//
// @group dataBoundComponentMethods
// @visibility internal
// @see listGrid.fetchData
// @see listGrid.invalidateCache
//<
refreshData : function (callback) {
    if (!this.getDataSource()) {
        this.logError("In order to refresh data a dataSource has to be specified for the component '" + this.ID + "'.");
        return;
    }

    if (this.data && this.data.fetchIsPending && this.data.fetchIsPending()) {
        this.logWarn("A fetch for this component is currently pending, please try again later.");
        return;
    }

    var dataSource = this.getDataSource(),
        visibleRows = this.getVisibleRows(),
        selectedState = this.getSelectedState();

    // request one page's worth of data on either side of the current viewport
    var startRow = visibleRows[0] - this.data.resultSize,
        endRow = visibleRows[1] + this.data.resultSize;

    if (startRow < 0) {
        startRow = 0;
    }

    var request = {
        startRow: startRow,
        endRow: endRow,
        sortBy: this.getSort(),
        showPrompt: false, 
        componentId: this.getID()
    };

    var context = this.data.context;
    if (context && context.textMatchStyle) request.textMatchStyle = context.textMatchStyle;
    if (context && context.operationId) request.operationId = context.operationId;

    var oldCriteria = isc.clone(this.data.getCriteria());
    var oldSort = isc.clone(this.data.getSort());

    dataSource.fetchData(this.getCriteria(), function (dsResponse, data, dsRequest) {
        var newCriteria = this.data.getCriteria();
        var newSort = this.data.getSort();
        var criteriaOrSortChanged = this.data.compareCriteria(newCriteria, oldCriteria) === 0 || this.data.compareSort(newSort, oldSort);

        // If we've reached this point and a fetch is pending on the previous ResultSet then
        // the user has most likely asked for a fetch after refreshData was called. In this
        // case lets abort this refreshData call.
        if (this.data && this.data.fetchIsPending && this.data.fetchIsPending() && criteriaOrSortChanged) {
            this.logDebug("refreshData aborted as a fetch had been issued while waiting for refreshData to complete.");
            return;
        }

        var result = dsResponse.data,
            initialData = [];

        // correctly position the result in the resultset's cache
        initialData.length = dsResponse.totalRows;

        // Copy results
        var start = dsResponse.startRow || 0;
        for (var i = 0; i < result.length; i++) {
            initialData.set(start + i, result.get(i));
        }

        var resultSet = dataSource.getResultSet({
            dataSource: this.getDataSource(),
            initialLength: dsResponse.totalRows,
            initialData: initialData,
            sortSpecifiers: this.getSort(),
            criteria: this.getCriteria(),
            context : this.data && isc.isA.ResultSet(this.data) ? this.data.context : null
        });

        // Lets temporarily enable this component to preserve edits on setData call. This means
        // that if a cell or row edit is in place with an edited value, this value will be retained
        // after the refreshData call has completed.
        var originalPreserveEditsOnSetData = this.preserveEditsOnSetData;
        this.preserveEditsOnSetData = true;
        this.setData(resultSet);
        this.preserveEditsOnSetData = originalPreserveEditsOnSetData;
        this.setSelectedState(selectedState);

        if (callback) {
            callback(dsResponse, data, dsRequest);
        }
    }.bind(this), request);
},

//> @method dataBoundComponent.willFetchData()
// Compares the specified criteria with the current criteria applied to this component's
// data object and determines whether the new criteria could be satisfied from the currently
// cached set of data, or if a new filter/fetch operation will be required.
// <P>
// This is equivalent to calling <code>this.data.willFetchData(...)</code>.
// Always returns true if this component is not showing a set of data from the dataSource.
// <p>
// Note that to predict correctly the decision that will be made by filter/fetch, you'll need to
// pass the same +link{textMatchStyle} that will be used by the future filter/fetch.  Fetching
// manually (e.g. +link{listGrid.fetchData()}) will by default use "exact" while filtering
// (e.g. +link{listGrid.filterData()}) will by default use "substring".  If the component
// is configured for autofetch (i.e. +link{listGrid.autoFetchData}: true), that will
// use +link{listGrid.autoFetchTextMatchStyle}, which defaults to "substring".  If nothing/null
// is passed for the style, this method assumes you want the style from the last filter/fetch.
// <p>
// To determine what +link{textMatchStyle} is being used, check the RPC Tab of the
// +link{group:debugging,SmartClient Developer Console} and check the relevant +link{DSRequest}. 
// 
// @param newCriteria (Criteria) new criteria to test.
// @param [textMatchStyle] (TextMatchStyle) New text match style. If not passed assumes 
//      textMatchStyle will not be modified.
// @return (Boolean) true if server fetch would be required to satisfy new criteria.
//
// @group dataBoundComponentMethods
// @visibility external
//<
willFetchData : function (newCriteria, textMatchStyle) {
    var data = this.data;
    if (data && data.willFetchData == null && this.originalData != null) data = this.orginalData;
    if (data && data.willFetchData != null) {
        return data.willFetchData(newCriteria, textMatchStyle);
    }
    return !this.shouldFilterLocalData();
},

//> @method dataBoundComponent.findByKey()
// @include resultSet.findByKey()
//<
findByKey : function(keyValue) {
    if (isc.isA.ResultSet(this.data)) return this.data.findByKey(keyValue);
    else if (isc.isA.Tree(this.data)) return this.data.findById(keyValue);
    else return null;
},

// Persistence
// -----------------------------------------------------------------------------

// This method factored up from ListGrid, July 2011
shouldSaveLocally : function () {
    return (!this.dataSource || this.getFullDataPath() != null || this.saveLocally ||
            this.shouldFilterLocalData());
},



//> @method dataBoundComponent.addData()
// Perform a DataSource "add" operation to add new records to this component's DataSource.
//
// @param newRecord (Record)	        new record
// @param [callback] (DSCallback)  method to call on operation completion
// @param  [requestProperties] (DSRequest Properties)   additional properties to set on the DSRequest
//                                          that will be issued
//
// @group dataBoundComponentMethods
// @visibility internal
//<
addData : function (newRecord, callback, requestProperties) {
    return this._performDSOperation("add", newRecord, callback, requestProperties);
},

//> @method dataBoundComponent.updateData()
// Perform a DataSource "update" operation to update existing records in this component's
// DataSource.
//
// @param updatedRecord  (Record)	        updated record
// @param [callback]          (DSCallback)  method to call on operation completion
// @param [requestProperties] (DSRequest Properties)   additional properties to set on the DSRequest
//                                          that will be issued
//
// @group dataBoundComponentMethods
// @visibility internal
//<
updateData : function (updatedRecord, callback, requestProperties) {
    return this._performDSOperation("update", updatedRecord, callback, requestProperties);
},

//> @method dataBoundComponent.removeData()
// Perform a DataSource "remove" operation to remove records from this component's
// DataSource.
//
// @param data (Record)	        primary key values of record to delete, 
//                                          (or complete record)
// @param [callback] (DSCallback)  method to call on operation completion
// @param [requestProperties] (DSRequest Properties)   additional properties to set on the DSRequest
//                                          that will be issued
//
// @group dataBoundComponentMethods
// @visibility internal
//<
removeData : function (recordKeys, callback, requestProperties) {
    return this._performDSOperation("remove", recordKeys, callback, requestProperties);
},

_performDSOperation : function (operationType, data, callback, requestProperties) {
    if (isc._traceMarkers) arguments.__this = this;

    if (this.shouldSaveLocally() || this.getDataSource() == null) {
        return this._performDSOperationInner(operationType, data);
    }
    
    // Call buildRequest - this will hang the default operationID (as well as various other
    // properties) onto the request.
    // We're passing the callback into performDSOperation directly so no need to hang it onto
    // the request in buildRequest
    requestProperties = this.buildRequest(requestProperties, operationType);
    
    return this.getDataSource().performDSOperation(operationType, data, 
                                                   callback, requestProperties);
},

_performDSOperationInner : function (operationType, data) {
    if (operationType == "update") {
        var ds = this.getDataSource();
        if (!ds) {
            isc.logWarn("Update by primary key cannot be performed without a DataSource." +  
                        "Modify the record directly instead");
            return;
        } 
        if (this.originalData) {
            // grouped - look up the original record by PK and update it
            var origRecord = this.originalData.get(ds.findByKeys(data, this.originalData));
            isc.addProperties(origRecord, data);
        }
        // look up the record by PK and update it
        var record = this.data.get(ds.findByKeys(data, this.data));
        isc.addProperties(record, data);

        if (this.originalData) {
            this.dataChanged("update", null, null, record);
        } else this.data.dataChanged();
        
        return;
    } else if (operationType == "add") {
        // for listGrid grouping, add record to original data and regroup
        if (this.originalData) { 
            this.originalData.add(data);
            this.dataChanged("add", null, null, data);
        } else {
            // dataChanged fires automatically
            if (isc.isA.Tree(this.data)) {
                var parent = this.data.getParent(data) || this.data.getRoot();
                this.data.add(data, parent);
            } else if (isc.ResultSet && isc.isA.ResultSet(this.data)) {
                if (this.data.allRows != null) {
                    this.data.allRows.add(data);
                    this.data.filterLocalData();
                } else {
                    isc.logWarn("Unable to add data to resultSet - allRows is not set");
                }
            } else {
                this.data.add(data);                        
            }
        }
        return;
    } else if (operationType == "remove") {
        // for listGrid grouping, remove record from original data and regroup
        if (this.originalData) { 
            this.originalData.remove(data);
            this.dataChanged("remove", null, null, data);
        } else {
            // dataChanged fires automatically
            if (isc.ResultSet && isc.isA.ResultSet(this.data)) {
                if (this.data.allRows != null) {
                    this.data.allRows.remove(data);
                    this.data.filterLocalData();
                } else {
                    isc.logWarn("Unable to remove data from resultSet - allRows is not set");
                }
            } else {
                this.data.remove(data);                        
            }
        }
        return;
    }
},

//>	@method dataBoundComponent.removeSelectedData()
// Remove the currently selected records from this component.
// If this is a databound grid, the records will be removed directly from the DataSource.
// <P>
// If no records are selected, no action is taken. The grid will automatically be
// updated if the record deletion succeeds.
//
// @param [callback] (DSCallback) callback to fire when the data has been removed
// @param [requestProperties] (DSRequest)   additional properties to set on the DSRequest
//                                          that will be issued
//
// @group dataBoundComponentMethods
// @visibility internal
//<
removeSelectedData : function (callback, requestProperties) {

    var selection = this.getSelection(),
        selectionLength = selection.length;

    // In an editable ListGrid, you can't select unsaved data.  If we are editing and
    // selectOnEdit is set and this is an unsaved row, call discardEdits() as an equivalent to
    // removeSelectedData()
    if (isc.isA.ListGrid(this) && this.canEdit && this.selectOnEdit &&
        selectionLength == 0 &&
        this.getEditRow() != null && this.getRecord(this.getEditRow()) == null)
    {
        this.discardEdits(this.getEditRow());
        return;
    }

    // if this is not a databound grid or we are working with local-only data (an Array)
    if (this.dataSource == null || this.shouldSaveLocally()) {
        if (this.data) {
            if (isc.ResultSet && isc.isA.ResultSet(this.data) && this.shouldSaveLocally()) {
                if (this.data.allRows != null) {
                    this.data.allRows.removeList(selection);
                    this.data.filterLocalData();
                } else {
                    isc.logWarn("Unable to remove data from resultSet - allRows is not set");
                }
            } else {
                this.data.removeList(selection);
            }
            if (callback) this.fireCallback(callback);
        }
        return;
    }

    var context = this.buildRequest(requestProperties, "remove", callback),
        dataSource = this.getDataSource();

    if (selectionLength > 0) {
        this.deleteRecords(selection, context.operation, context, dataSource);
    }
    // notify that they have to select something to delete first... ???
},

// delete a specific list of records from the server
deleteRecords : function (records, deleteOperation, context, dataSource) {
    isc.addProperties(context, {
        prompt:(context.prompt || isc.RPCManager.removeDataPrompt)
    });

    var keyFieldNames = dataSource.getPrimaryKeyFieldNames(),
        fieldNames = dataSource.getFieldNames();

    // perform the delete as a multi-op, one per record
    var wasAlreadyQueuing = isc.RPCManager.startQueue();
    if (!isc.isAn.Array(records)) records = [records];
    for (var i = 0; i < records.length; i++) {
        var record = records[i];
        if (record._isGroup) continue;
        // Apply a mask to remove any non primary key fields before sending request in order
        // to stay consistent with other remove operations such as DataSource.removeData().
        var recordKeys = isc.applyMask(record, keyFieldNames);
        context.oldValues = isc.applyMask(record, fieldNames);
        dataSource.performDSOperation(deleteOperation.type, recordKeys, null, context);
    }

    // don't kickoff the transaction unless this flow initiated queuing, in case caller
    // wants to include other operations
    if (!wasAlreadyQueuing) isc.RPCManager.sendQueue();
},


// Selection
// ---------------------------------------------------------------------------------------

//> @type CellRecordMode  
// When +link{canSelectCells} is true, whether +link{getSelection} on a 
// +link{class:DataBoundComponent} or individual +link{class:CellSelection} should return one 
// record per selected cell, as with +link{class:CubeGrid}, or one record per row that has any 
// selected cells, as with +link{class:ListGrid}.
// @value "cell" getSelection returns one record for each selected cell via +link{getCellRecord}
// @value "row" getSelection returns distinct records for each row with one or more selected cells
// @visibility @external
// @group selection
//<

//> @attr dateBoundComponent.cellRecordMode (CellRecordMode : null : IRW)
// Whether calling +link{getSelection} on this class should return one record per cell, the 
// default behavior and that used by, eg, +link{class:CubeGrid}, or a list of distinct records, 
// as required by +link{class:ListGrid}.
// @visibility @internal
//<

//> @method dataBoundComponent.createSelectionModel()
// Creates the selection object for this +link{DataBoundComponent}
//
// @return (Selection | CellSelection) null
// @group  selection
// @visibility internal
//<
createSelectionModel : function (extraParams) {
    // clean up old selection object before creating new selection, if we have one.
    if (this.selection) this.destroySelectionModel();
    
    if (this.canSelectCells && this.cellDataModel) {
    	
        var data = [];
        if (this.numRows != null) {
            for (var i = 0; i < this.numRows; i++) {
                data[i] = {};
            }
        }
    } else {
        var data = this.data;
    }
		
    var selection,
        params = {ID:this.getID()+"_selection", 
                  data:data,
                  
                  target: this,
                  selectionProperty:this.selectionProperty,
                  simpleDeselect : this.simpleDeselect,
                  dragSelection : this.canDragSelect
                };

    
    if (this.canSelectCells && this.fields != null) params.numCols = this.fields.length;
    
    // pass "reselectOnUpdate" through to the selection model, and override the
    // method to perform this reselection, so we can customize what notifications
    // get fired.
    if (!this.canSelectCells) {
        isc.addProperties(params, {
            reselectOnUpdate : this.reselectOnUpdate,
            performReselectOnUpdate:function (modifiedRecord) {
                if (this.target && this.target.performReselectOnUpdate) {
                    this.target.performReselectOnUpdate(modifiedRecord);
                } else {
                    this.Super("performReselectOnUpdate", arguments);
                }
            }
        });
    }

    // Copy our "enabled" property across if we have one.
    if (this.recordEnabledProperty != null) params.enabledProperty = this.recordEnabledProperty;
    
    // Copy our selection properties
    if (this.recordCanSelectProperty != null) params.canSelectProperty = this.recordCanSelectProperty;
    var cascade = this._shouldCascadeSelection();
    if (cascade != null) params.cascadeSelection = cascade;
    // if the data object supports a special selection class, use it
    if (this.data.getNewSelection) {
        selection = this.data.getNewSelection(params);
    }
    if (selection == null) {
    	// otherwise use the default Selection or CellSelection class
        if (this.canSelectCells) {
            if (this.cellRecordMode) params.cellRecordMode = this.cellRecordMode;
            selection = isc.CellSelection.create(params);
        } else {
            selection = isc.Selection.create(params);
        }
    }
	
    this.selection = selection;
    
},


_shouldCascadeSelection : function () {
    return this.cascadeSelection;
},

// destroySelectionModel: Decouple from selection object and destroy it.  
destroySelectionModel : function () {
    if (!this.selection) return;
    if (this.selection.destroy) this.selection.destroy();
    delete this.selection;
}, 

// undoc'd utility method to remove the selection-property applied to selected-rows
removeSelectionMarkers : function (data) {
    var returnArray = true;
    if (!isc.isAn.Array(data)) {
        data = [data];
        returnArray = false;
    }
    data.clearProperty(this.selectionProperty || this.selection ? this.selection.selectionProperty : null);
    return returnArray ? data : data[0];
},

//> @method dataBoundComponent.getSelection()
// Returns all selected records, as an Array.
//
// @param [excludePartialSelections] (Boolean) When true, partially selected records will not be returned.
//                                   Otherwise, both fully and partially selected records are
//                                   returned.
// @return (Array of ListGridRecord) list of records, empty list if nothing selected
// @group  selection
// @visibility internal
// @example databoundRemove
//<

getSelection : function (excludePartialSelections, internalParam, dontSort) {
    if (!this.selection) return [];

    if (this.canSelectCells) {
        return this.selection.getSelection(null, dontSort);
    } else {
        return this.selection.getSelection(excludePartialSelections, dontSort);
    }
},

//> @method dataBoundComponent.getSelectedRecords()
// Returns all selected records in this component.
// <p>
// <strong>NOTE:</strong> Records in the returned array should be treated as read-only and not
// modified.
// @param [excludePartialSelections] (Boolean) When true, partially selected records will not be returned.
//                                   Otherwise, both fully and partially selected records are
//                                   returned.
// @return (Array of ListGridRecord) array of selected records, which will be empty if no record
// is selected.
// @group  selection
// @visibility internal
//<
// This is a (better) synonym for getSelection(). It's documented at the ListGrid level.
getSelectedRecords : function (excludePartialSelection) {
    return this.getSelection(excludePartialSelection);
},

//> @method dataBoundComponent.getSelectedRecord()
// Returns the first selected record in this component.
// <p>
// <strong>NOTE:</strong> If a record is returned, it should be treated as read-only and not
// modified.
// @return (ListGridRecord) the first selected record, or null if no record is selected.
// @visibility internal
// @group selection
// @example databoundRemove
//<
getSelectedRecord : function() {
    if (!this.selection) return null;
    return this.selection.getSelectedRecord();
},

//> @method dataBoundComponent.getSelectionObject()
// Return the dataBoundComponent's underlying +link{Selection} object.  Note that this differs
// from +link{dataBoundComponent.getSelection}, which returns an array containing the actual
// selected objects
//      @group  selection
//      @return (Selection) This dataBoundComponent's underlying +link{Selection} object
// @visibility internal
//<
getSelectionObject : function() {
    return this.selection;
},

//> @method listGrid.isSelected()
// Returns true if the record is selected.
// 
// @param record (ListGridRecord) record to check
// @return (Boolean) true if record is selected; false otherwise
// @group selection
// @visibility external
//<
isSelected : function (record) {
    if (!record || !this.selection) return false;
    return this.selection.isSelected(record);
},

//> @method listGrid.isPartiallySelected()
// When using tree-oriented selection modes like +link{treeGrid.cascadeSelection}, returns true
// if the record is considered partially selected because only some of it's children are
// selected.
// 
// @param record (ListGridRecord) record to check
// @return (Boolean) true if record is partially selected; false otherwise
// @group selection
// @visibility external
//<
isPartiallySelected : function (record) {
    if (!record || !this.selection) return false;
    return this.selection.isPartiallySelected(record);
},

//> @groupDef selection
// APIs for marking +link{Record}s as selected and retrieving the selected record or records.
// <P>
// Only applicable to a +link{DataBoundComponent} that manages a list of Records, or manages a
// data model that can be viewed as a list (for example, the current list of visible nodes
// on a tree can be treated as a list for selection purposes).
// 
// @title Selection
// @visibility external
//<




//> @method dataBoundComponent.selectRecord()
//
// Select/deselect a +link{Record} passed in explicitly, or by index.
//
// @param record (Record | number) record (or row number) to select
// @param [newState] (boolean) new selection state (if null, defaults to true)
//
// @group selection
// @visibility external
//<
selectRecord : function (record, state, colNum) {
    this.selectRecords(record, state, colNum);
},

//> @method dataBoundComponent.selectSingleRecord()
// Select a single +link{Record} passed in explicitly, or by index, and deselect everything else.
// When programmatic selection of records is a requirement and 
// +link{listGrid.selectionType, selectionType()} is "single", use this method rather than 
// +link{dataBoundComponent.selectRecord(), selectRecord()} to 
// enforce mutually-exclusive record-selection.
//
// @param record (Record | number) record (or row number) to select
// 
// @group selection
// @visibility external
//<
selectSingleRecord : function (record) {
    
    this.selection.deselectAll();
    this.selectRecord(record);
},

//> @method dataBoundComponent.deselectRecord()
//
// Deselect a +link{Record} passed in explicitly, or by index.
// <P>
// Synonym for <code>selectRecord(record, false)</code>
//
// @param record (Record | number) record (or row number) to deselect
//
// @group selection
// @visibility external
//<
deselectRecord : function (record, colNum) {
    this.selectRecord(record, false, colNum);
},

//> @method dataBoundComponent.selectRecords()
//
// Select/deselect a list of +link{Record}s passed in explicitly, or by index.
// <P>
// Note that developers may wish to use +link{selectRange()} to select a single
// contiguous range.
//
// @param records (Array of Record | numbers) records (or row numbers) to select
// @param [newState]  (boolean) new selection state (if null, defaults to true)
//
// @group selection
// @visibility external
//<
selectRecords : function (records, state, colNum) {
    if (state == null) state = true;
    if (!isc.isAn.Array(records)) records = [records];

    if (isc.isA.ResultSet(this.data) && !this.data.lengthIsKnown()) {
        this.logWarn("ignoring attempt to select records while data is loading");
        return;
    }
    
    for (var i = 0; i < records.length; i++) {
        
        if (records[i] == null) continue;

        // assume any number passed is a rownum
        if (isc.isA.Number(records[i])) {
            var index = records[i];
            records[i] = this.getRecord(index, colNum);
        }
    }
    
    var selObj = this.getSelectionObject(colNum);
    if (selObj) {
        
        selObj.selectList(records, state, null, this);
        this.fireSelectionUpdated();
    }
},

//> @method dataBoundComponent.deselectRecords()
//
// Deselect a list of +link{Record}s passed in explicitly, or by index.
// <P>
// Synonym for <code>selectRecords(records, false)</code>
// <P>
// Note that developers may wish to use +link{deselectRange()} to select a single
// contiguous range.
//
// @param records (Array of Record | numbers) records (or row numbers) to deselect
//
// @group selection
// @visibility external
//<
deselectRecords : function (records, colNum) {
    this.selectRecords(records, false, colNum);
},

//> @method dataBoundComponent.selectAllRecords()
// Select all records
//
// @group selection
// @visibility external
//<
selectAllRecords : function () {
    this.selection.selectAll();
    this.fireSelectionUpdated();
},

//> @method dataBoundComponent.deselectAllRecords()
//
// Deselect all records
//
// @group selection
// @visibility external
//<
deselectAllRecords : function () {
    this.selection.deselectAll();
    this.fireSelectionUpdated();
},

//> @method dataBoundComponent.selectRange()
// Select a contiguous range of records by index
// @param startRow (int) start of selection range
// @param endRow (int) end of selection range (non-inclusive)
// @param [newState]  (boolean) new selection state (if null, defaults to true)
// @visibility external
//<
selectRange : function (startRow, endRow, newState) {
    this.selection.selectRange(startRow, endRow, newState);
    this.fireSelectionUpdated();
},

//> @method dataBoundComponent.deselectRange()
// Deselect a contiguous range of records by index.
// <P>
// This is a synonym for <code>selectRange(startRow, endRow, false);</code>
// @param startRow (int) start of selection range
// @param endRow (int) end of selection range (non-inclusive)
// @visibility external
//<
deselectRange : function (startRow, endRow) {
    this.selection.selectRange(startRow, endRow);
    this.fireSelectionUpdated();
},

//> @method dataBoundComponent.anySelected()
// @include selection.anySelected()
//<
anySelected : function () {
    return this.selection && this.selection.anySelected();
},

//> @method dataBoundComponent.getSelectionLength()
// @include selection.getLength()
//<
getSelectionLength : function () {
    return this.selection ? this.selection.getLength() : 0;
},

getRecord : function (index, column) {
    var recordContainer = isc.isA.List(this.data) || isc.isA.ResultSet(this.data);
    return recordContainer ? this.data.get(index) : this.data;
},

fireSelectionUpdated : function () {
    var ruleScopeComponent = (this.getRuleScopeComponent ? this.getRuleScopeComponent() : null);

    
    if (this.selectionUpdated || (ruleScopeComponent != null && (ruleScopeComponent.ruleScope || ruleScopeComponent.isRuleScope))) {
        
        var recordList = this.getSelection(null, null, true),
            record = (recordList.length > 0 ? recordList[0] : null)
        ;
        if (this.selectionUpdated) this.selectionUpdated(record, recordList);

        
        if (ruleScopeComponent != null && isc.isA.DataBoundComponent(this)) {
            var grid = this,
                ds = grid.getDataSource(),
                id = grid.getLocalId(),
                hasStableID = grid.hasStableLocalID() || (grid.editNode != null)
            ;

            // Remove metadata from record for ruleContext
            if (record) {
                record = this.getCleanRecordData(record);
                delete record._ignoreStyleUpdates;
            }

            if (ds) ruleScopeComponent.provideRuleContext(ds.getID(), record, this, hasStableID);
            if (hasStableID) {
                ruleScopeComponent.provideRuleContext(id + ".selectedRecord", record, this, true);
                ruleScopeComponent.provideRuleContext(id + ".anySelected", (record != null), this, true);
                ruleScopeComponent.provideRuleContext(id + ".multiSelected", (record ? recordList.length > 1 : false), this, true);
                ruleScopeComponent.provideRuleContext(id + ".numSelected", (record ? recordList.length : 0), this, false);
            }
        }
    }
},

// Hiliting
// ---------------------------------------------------------------------------------------

//> @groupDef hiliting
// Hiliting means special visual styling which is applied to specific data values that meet
// certain criteria.
// <P>
// A +link{Hilite} definition contains styling information such as +link{hilite.cssText} and
// +link{hilite.htmlBefore} that define what the hilite looks like, as well as properties
// defining where the hilite is applied.  If you create hilites manually, they should ideally
// specify +link{hilite.textColor, textColor} and/or 
// +link{hilite.backgroundColor, backgroundColor} in order to be editable in a 
// +link{class:HiliteEditor}.  If these are not provided, however, note that they will be 
// manufactured automatically from the +link{hilite.cssText, cssText} attribute if it is present.
// <P>
// A hilite can be applied to data <b>either</b> by defining +link{hilite.criteria,criteria}
// or by explicitly including markers on the data itself.  
// <P>
// Hiliting rules such as hiliting different ranges of values with different colors can be
// accomplished entirely client-side by defining +link{AdvancedCriteria} in hilite definitions
// that pick out values to be highlighted.
// <P>
// Hiliting rules that require server-side calculations can be achieved by assigning a
// +link{hilite.id} to a hilite definition, and setting the
// +link{dataBoundComponent.hiliteProperty} on the records that should show that highlight.
// This can be used, for example, to hilite the record with the maximum value for a dataset
// that the application will load incrementally.
//
// @title Hiliting
// @visibility external
//<

// Hilite Declarations
// ---------------------------------------------------------------------------------------

//> @object Hilite
// An object representing a user-created and user-modifiable hilite, which can be created and
// edited with a +link{class:HiliteEditor} either directly or via the
// +link{ListGrid.canEditHilites} behavior.
// <P>
// See +link{group:hiliting} for an overview.
//
// @treeLocation Client Reference/Grids/ListGrid
// @visibility external
// @group hiliting
//< 

//> @attr hilite.id (String : null : IR)
// Unique id for this hilite definition.  
// <P>
// For hilites that include +link{hilite.criteria} this is not required.
// <P>
// If you are explicitly marking records for hiliting, set
// +link{dataBoundComponent.hiliteProperty} on the record to this id.  
//
// @visibility external
// @group hiliting
//< 

//> @attr hilite.cssText (CSSText : null : IR)
// CSS text to be applied to cells where this hilite is applied, for example,
// "background-color:#FF0000"
//
// @visibility external
// @group hiliting
//< 

//> @attr hilite.fieldName (identifier : null : IR)
// Name of the field, or array of fieldNames, this hilite should be applied to.  
// <P>
// If unset, hilite is applied to every field of the record.
//
// @visibility external
// @group hiliting
//< 

//> @attr hilite.criteria (Criteria or AdvancedCriteria : null : IR)
// Criteria defining what records this hilite should apply to.
// 
// @visibility external
// @group hiliting
//<

//> @attr hilite.htmlBefore (HTML : null : IR)
// HTML to pre-pend to cell values where this hilite is applied.
//
// @visibility external
// @group hiliting
//<

//> @attr hilite.htmlAfter (HTML : null : IR)
// HTML to append to the end of cell values where this hilite is applied.
//
// @visibility external
// @group hiliting
//<

//> @attr hilite.htmlValue (String : null : IR)
// Value to show <b>in place of</b> the actual value from the record, for a record that matches
// this hilite.
// <P>
// This can be used to take ranges of numeric values and simplify them to "Low", "Medium",
// "High" or similar textual values, translate very small or very large values to "Outlier" or
// "Negligible", and similar use cases.
//
// @deprecated <code>htmlValue</code> is deprecated in favor of +link{hilite.replacementValue}. 
//  Note that unlike <code>replacementValue</code>, this property does not respect
//  +link{hilite.disabled}, and will be applied even if <code>disabled</code> is set to
//  <code>true</code>
//
// @visibility external
// @group hiliting
//<

//> @attr hilite.disabled (Boolean : false : IRW)
// Whether this hilite is currently disabled.
// <P>
// Hilites can be programmatically enabled and disabled via +link{dataBoundComponent.enableHilite()}.
//
// @visibility external
// @group hiliting
//<

//> @attr hilite.canEdit (Boolean : null : IR)
// Can highlight be edited from header context menu? Setting attribute to
// <code>false</code> prevents editing. A <code>null</code> or <code>true</code>
// value allows editing.
// 
// @visibility external
// @group hiliting
//<

//> @attr hilite.title (String : null : IRW)
// User-visible title for this hilite.  Used for interfaces such as menus that can enable or
// disable hilites.
//
// @visibility external
// @group hiliting
//<


//> @attr hilite.textColor (String : null : IRW)
// When edited via a +link{class:HiliteEditor}, the value for the foreground color of this 
// hilite.  If this is omitted, it will be automatically derived from the <i>textColor</i>
// attribute of +link{hilite.cssText}.  When a hilite is saved in a HiliteEditor, both 
// attributes are set automatically.
//
// @visibility external
// @group hiliting
//<

//> @attr hilite.backgroundColor (String : null : IRW)
// When edited via a +link{class:HiliteEditor}, the value for the background color of this 
// hilite.  If this is omitted, it will be automatically derived from the <i>backgroundColor</i>
// attribute of +link{hilite.cssText}.  When a hilite is saved in a HiliteEditor, both 
// attributes are set automatically.
//
// @visibility external
// @group hiliting
//<

//> @attr hilite.icon (SCImgURL : null : IR)
// URL of an icon to show when this hilite is applied to a cell.  Position of the icon 
// is controlled by +link{dataBoundComponent.hiliteIconPosition} or 
// +link{listGridField.hiliteIconPosition}.
//
// @visibility external
// @group hiliting
//<

//> @attr hilite.replacementValue (HTML : null : IR)
// HTML which replaces the cell's textual value where this hilite is applied.
// <p>
// Note that sorting, filtering, etc behavior will still operate on the underlying value.
// For example, if there is a date field with the FilterEditor enabled, the default search
// interface will still offer date-range based filtering even if hilites have caused values
// to be displayed as text such as "current" or "past due".
//
// @visibility external
// @group hiliting
//<
    
 
styleOpposite:"cellHiliteOpposite",

// Hilites
// ---------------------------------------------------------------------------------------

//> @attr dataBoundComponent.canEditHilites (boolean : false : [IRW])
// Adds an item to the header context menu allowing users to launch a dialog to define
// grid hilites using the +link{class:HiliteEditor}.
// <P>
// User-added hilites can be persisted via +link{dataBoundComponent.getHiliteState()} and 
// +link{dataBoundComponent.setHiliteState()}.
//
// @visibility external
// @group hiliting
//<
canEditHilites:false,

//> @attr dataBoundComponent.hilites (Array of Hilite : null : [IRW])
// Hilites to be applied to the data for this component.  See +link{group:hiliting}.
//
// @visibility external
// @group hiliting
//<

//> @attr dataBoundComponent.hiliteIcons (Array of String : ["[SKINIMG]/Dialog/notify.png", "[SKINIMG]/Dialog/warn.png", "[SKINIMG]/actions/approve.png"] : IR)
// Specifies a list of icons that can be used in +link{dataBoundComponent.editHilites(),hilites}.
// <P>
// <code>hiliteIcons</code> should be specified as an Array of +link{SCImgURL}.
// When present, the hilite editing interface shown when +link{dataBoundComponent.editHilites()} is called 
// will offer the user a drop down for picking one of these icons when defining either a 
// simple or advanced hilite rule.
// <P>
// If the user picks an icon, the created hiliting rule will have +link{hilite.icon} set to 
// the chosen icon.  +link{dataBoundComponent.hiliteIconPosition} controls where the icon will 
// appear for that field -- the default is that it appears in front of the normal cell content.
// This can also be overridden at the field level.
//
// @visibility external
// @group hiliting
//<
hiliteIcons: [
    "[SKINIMG]/Dialog/notify.png",
    "[SKINIMG]/Dialog/warn.png",
    "[SKINIMG]/actions/approve.png"
],

//> @type HiliteIconPosition
// Where a +link{dataBoundComponent.hiliteIcons,hilite icon} will be placed relative to 
// normal cell content.
// 
// @value "before" icon will be placed before the normal cell contents
// @value "after" icon will be placed after the normal cell contents
// @value "replace" icon will be shown instead of the normal cell contents
//
// @visibility external
// @group hiliting
//<

//> @attr dataBoundComponent.hiliteIconPosition (HiliteIconPosition : "before" : IR)
// When +link{hiliteIcons,hiliteIcons} are present, where the hilite icon will be placed 
// relative to the field value.  See +link{type:HiliteIconPosition}. Can be overridden at the
// field level.
//
// @visibility external
// @group hiliting
//<
hiliteIconPosition: "before",

//> @attr dataBoundComponent.hiliteIconSize (number : 12 : IRW)
// Default width and height of +link{hiliteIcons,hilite icons} for this component.
// Can be overridden at the component level via explicit 
// +link{hiliteIconWidth, hiliteIconWidth} and
// +link{hiliteIconHeight, hiliteIconHeight}, or at the field level via 
// +link{ListGridField.hiliteIconSize, hiliteIconSize},
// +link{ListGridField.hiliteIconWidth, hiliteIconWidth} and 
// +link{ListGridField.hiliteIconHeight, hiliteIconHeight}
// @group hiliting
// @see DataBoundComponent.hiliteIconWidth
// @see DataBoundComponent.hiliteIconHeight
// @see ListGridField.hiliteIconSize
// @visibility external
//<
hiliteIconSize: 12,
     
//> @attr dataBoundComponent.hiliteIconWidth (number : null : IRW)
// Width for hilite icons for this component.
// Overrides +link{hiliteIconSize, hiliteIconSize}.
// Can be overridden at the field level.
// @group hiliting
// @visibility external
//<

//> @attr dataBoundComponent.hiliteIconHeight (number : null : IRW)
// Height for hilite icons for this listGrid.
// Overrides +link{hiliteIconSize, hiliteIconSize}.
// Can be overridden at the field level
// @group hiliting
// @visibility external
//<
    
//> @attr   dataBoundComponent.hiliteIconLeftPadding (number : 2 : IRW)
// How much padding should there be on the left of +link{hiliteIcons, hilite icons} by default?
// Can be overridden at the field level
// @group hiliting
// @visibility external
//<
hiliteIconLeftPadding: 2,

//> @attr   dataBoundComponent.hiliteIconRightPadding (number : 2 : IRW)
// How much padding should there be on the right of +link{hiliteIcons, hilite icons} by default?
// Can be overridden at the field level    
// @group hiliting
// @visibility external
//<
hiliteIconRightPadding: 2,
  
// Helpers to get hiliteIcon attributes from field level or component level
getHiliteIconLeftPadding : function (field) {
    return (field.hiliteIconLeftPadding != null ? field.hiliteIconLeftPadding 
            : this.hiliteIconLeftPadding || 0);
},

getHiliteIconRightPadding : function (field) {
    return (field.hiliteIconRightPadding != null ? field.hiliteIconRightPadding 
            : this.hiliteIconRightPadding || 0);
},

getHiliteIconWidth : function (field) {
    return (field.hiliteIconWidth != null ? field.hiliteIconWidth  :
                (field.hiliteIconSize != null ? field.hiliteIconSize : 
                    (this.hiliteIconWidth != null ? this.hiliteIconWidth : this.hiliteIconSize)));
},

getHiliteIconHeight : function (field) {
    return (field.hiliteIconHeight != null ? field.hiliteIconHeight  :
                (field.hiliteIconSize != null ? field.hiliteIconSize : 
                    (this.hiliteIconHeight != null ? this.hiliteIconHeight : this.hiliteIconSize)));
},

getHiliteIconPosition : function (field) {
    return field.hiliteIconPosition != null ? field.hiliteIconPosition : this.hiliteIconPosition;
},

getHiliteIconHTML : function (icon, field) {
    var prefix = field.imageURLPrefix,
        width = this.getHiliteIconWidth(field),
        height = this.getHiliteIconHeight(field),
        leftPad = this.getHiliteIconLeftPadding(field),
        rightPad = this.getHiliteIconRightPadding(field);
    
    var iconHTML = isc.Canvas._getValueIconHTML(icon, prefix, width, height, leftPad, rightPad, null, this);
    return iconHTML;
},


//> @attr dataBoundComponent.hiliteProperty (string : "_hilite" : [IRW])
// Marker that can be set on a record to flag that record as hilited.  Should be set to a value
// that matches +link{hilite.id} for a hilite defined on this component.
//
// @visibility external
//<
// NOTE: not the same as hiliteMarker, which is an internal property used to track generated
// hilites 
hiliteProperty:"_hilite",

    
// Hilite APIs

// user: 
//   component.hilites && setHilites()
//   record[hiliteProperty] (CubeGrid only)
// component/framework: 
//   note: setup is automatic on first call any of the below, or setHilites()
//   applyHilites() (to data)
//   getHiliteCSSText() / addHiliteCSSText()


//> @attr dataBoundComponent.hiliteState (String : null : IRW)
// Initial hilite state for the grid. 
// <P>
// +link{listGrid.viewState} can be used to initialize all view properties of the grid.
// When doing so, <code>hiliteState</code> is not needed because <code>viewState</code>
// includes it as well. If both are provided, <code>hiliteState</code> has priority for
// hilite state.
// <smartclient>
// <P>
// To retrieve current state call +link{DataBoundComponent.getHiliteState,getHiliteState}.
// </smartclient>
//
// @group viewState
// @visibility external
//<

//>	@method dataBoundComponent.getHilites()
// Return the set of hilite-objects currently applied to this DataBoundComponent.  These
// can be serialized for storage and then restored to a component later via 
// +link{dataBoundComponent.setHilites, setHilites()}.
//
// @visibility external
// @return (Array) Array of hilite objects
// @group  hiliting
//<
getHilites : function () {
    return this.hilites;
},

// property used to store hilite state for generated hilites
hiliteMarker:"_hmarker",
_hiliteCount: 0,

//>	@method dataBoundComponent.setHilites()
// Accepts an array of hilite objects and applies them to this DataBoundComponent.  See also
// +link{dataBoundComponent.getHilites, getHilites()} for a method of retrieving the hilite
// array for storage, including hilites manually added by the user.
//
// @param hilites (Array of Hilite) Array of hilite objects
// @group hiliting
// @visibility external
//<
setHilites : function (hilites) {

    this.hilites = hilites;
    this._setupHilites(this.hilites);

}, 

//>	@method dataBoundComponent.getHiliteState()
// Get the current hilites encoded as a String, for saving.
//
// @return (String) hilites state encoded as a String
// @group  viewState
// @visibility external
//<
getHiliteState : function (returnObject) {
    var hilites = this.getHilites();
    if (hilites == null) return null;
    if (returnObject) return hilites;
    return "(" + isc.JSON.encode(hilites, {dateFormat:"logicalDateConstructor", prettyPrint:false}) + ")";
},

//>	@method dataBoundComponent.setHiliteState()
// Set the current hilites based on a hiliteState String previously returned from
// +link{getHiliteState()}.
// @param hiliteState (String) hilites state encoded as a String
// @group viewState
// @visibility external
//<
setHiliteState : function (hilitesState) {
    //!OBFUSCATEOK    
    if (hilitesState == null) this.setHilites(null);
    var hilites = eval(hilitesState);
    this.setHilites(hilites);
},

// factored so it can also get called lazily the first time getHilite() is called
_setupHilites : function (hilites, dontApply) {
    // auto-assign ids if unset
    if (hilites != null) {
        this._lastHiliteId = this._lastHiliteId || 0;
        var currentIds = {},
            needsId = [];
        for (var i = 0; i < hilites.length; i++) {
            var id = hilites[i].id;
            if (id != null) {
                if (currentIds[id]) {
                    this.logWarn("This component has more than one hilite object with " +
                        "the same specified id:" + id +
                        ". Hilite IDs must be unique within a component.", "hiliting");
                    // We could tweak the ID to fix it perhaps, but instead we'll just
                    // pass in the param to 'makeIndex' to cause collisions to just
                    // clobber each other, rather than making an array which we don't
                    // handle. User visible effect - one of the hilites just won't show
                    // up.
                } else {
                    currentIds[id] = true;
                    var numericID = parseInt(id);
                    if (id == numericID) {
                        this._lastHiliteId = Math.max(this._lastHiliteId, (numericID+1));
                    }
                }                
            } else {
                needsId.add(hilites[i]);
            }
        }
        for (var  i= 0; i < needsId.length; i++) {
            needsId[i].id = this._lastHiliteId++;
        }

        // for quick hilite lookups
        this._hiliteIndex = hilites.makeIndex("id", -1);
    }    
    if (!dontApply) this.applyHilites();
},

// update the user formula fields present in the component and refresh their value
_storeFormulaFieldValues : function (data, fields, skipGroupRecord, oldFormulaFields) {
    var formulaFields = [],
        formulaFunctions = [];
    for (var i = 0; i < fields.length; i ++) {
        var formulaFunction = this.getFormulaFunction(fields[i]);
        if (formulaFunction) {
            formulaFields.add(fields[i]);
            formulaFunctions.add(formulaFunction);
        }
    }
    if ((oldFormulaFields && isc.getKeys(oldFormulaFields).length > 0) ||
        formulaFields.length > 0) 
    {
        for (var j=0; j<data.length; j++) {
            var record = data[j];
            for (var i=0; i<formulaFields.length; i++) {
                var field = formulaFields[i],
                    fieldName = field[this.fieldIdProperty];
            
                if (this.shouldApplyUserFormulaAfterSummary(field) &&
                    this.shouldShowUserFormula(field, record)) 
                {
                    if (!skipGroupRecord || !record._isGroup) {
                        this.storeFormulaFieldValue(field, record, formulaFunctions[i]);
                    }
                    if (j == 0) {
                        delete oldFormulaFields[fieldName];
                    }
                }
            }
            for (var oldFormula in oldFormulaFields) {
                delete record[oldFormula];
            }
        }
    }
    // update the metadata indicating what calculated formula field
    // have had values applied to our data
    for (var oldFormula in oldFormulaFields) {
        delete this._storedFormulaFields[oldFormula];
    }
},

// update the user summary fields present in the component and refresh their value
_storeSummaryFieldValues : function (data, fields, skipGroupRecord, oldSummaryFields) {
    var summaryFields = [],
        summaryFunctions = [];
    for (var i = 0; i < fields.length; i++) {
        if (fields[i].userSummary != null) {
            summaryFields.add(fields[i]);
            summaryFunctions.add(this.getSummaryFunction(fields[i]));
        }
    }
    if ((oldSummaryFields && isc.getKeys(oldSummaryFields).length > 0) ||
        summaryFields.length > 0)
    {
        for (var j=0; j<data.length; j++) {
            var record = data[j];
            for (var i=0; i<fields.length; i++) {
                var field = fields[i],
                fieldName = field[this.fieldIdProperty];
                if (field.userSummary) {
                    if (!skipGroupRecord || !record._isGroup) {
                        this.storeSummaryFieldValue(field, record, summaryFunctions[i]);
                    }
                    if (j == 0) {
                        delete oldSummaryFields[fieldName];
                    }
                }
            }
            for (var oldSummary in oldSummaryFields) {
                delete record[oldSummary];
            }
        }
    }
    // update the metadata indicating what calculated summary fields
    // have had values applied to our data
    for (var oldSummary in oldSummaryFields) {
        delete this._storedSummaryFields[oldSummary];
    }
},


             _$nonSummaryAffectingHiliteRule:              "nonSummaryAffecting",
          _$simpleSummaryAffectingHiliteRule:           "simpleSummaryAffecting",
_$summaryDependentSummaryAffectingHiliteRule: "summaryDependentSummaryAffecting",

_partitionSummaryHiliteRules : function (hiliteRules, summaryFieldsToRecalculate) {

    var partition = {
                     nonSummaryAffecting: [], 
                  simpleSummaryAffecting: [], 
        summaryDependentSummaryAffecting: []
    };

    // nothing to do if there are no hilite rules or no defined fields
    if (hiliteRules == null || this.fields == null) return partition;

    var component = this,
        dependencyTable = this._getFieldDependencyTable();

    for (var i = 0; i < hiliteRules.length; i++) {
        var hilite = this.getHilite(hiliteRules[i]);
        if (!hilite || hilite.disabled) continue;

        var targetFields = hilite.fieldName || 
            (this.fields ? this.fields.getProperty("name") : []);
        if (!isc.isAn.Array(targetFields)) targetFields = [targetFields];

        var criteriaFields = isc.DataSource ? 
            isc.DataSource.getCriteriaFields(hilite.criteria, this.dataSource) : null;
        if (criteriaFields) criteriaFields = criteriaFields.filter(function (field) { 
            var field = component.getField(field);
            return field && field.userSummary != null;
        });

        // If a target field of the rule affects a summary field, the rule is not a
        // nonSummaryAffecting rule; if furthermore any summary fields are in the rule criteria,
        // or we can't determine the criteria fields, it's not a simpleSummaryAffecting rule.
        var ruleType = this._$nonSummaryAffectingHiliteRule;

        for (var j = 0; j < targetFields.length; j++) {
            var dependentFields = dependencyTable[targetFields[j]] || {};
            for (var fieldName in dependentFields) {
                var dependentField = dependentFields[fieldName];
                if (dependentField.userSummary == null ||
                    !this.shouldIncludeHiliteInSummaryField(fieldName, targetFields[j]))
                {
                    continue; // nonSummaryAffecting rule
                }
                if (ruleType == this._$nonSummaryAffectingHiliteRule) {
                    ruleType = this._$simpleSummaryAffectingHiliteRule;
                }
                if (!criteriaFields  || criteriaFields.length > 0) {
                    ruleType = this._$summaryDependentSummaryAffectingHiliteRule;
                    summaryFieldsToRecalculate[fieldName] = dependentField;
                }
            }
        }
        partition[ruleType].push(hilite);
    }
    return partition;
},

applyHilites : function (suppressRedraw) {
    var hilites = this.hilites,
        data = this.data;
    if (hilites && !this._hiliteIndex) this._setupHilites(hilites, true);

    // wipe all existing hilite markers  
    if (isc.isA.ResultSet(data)) data = data.getAllLoadedRows();
    if (isc.isA.Tree(data)) data = data.getAllItems();
    data.setProperty(this.hiliteMarker, null);

    var fields = this.getAllFields();
    if (fields == null) fields = [];
    
    // clear the _hilites flag
    
    fields.setProperty("_hilites", null);

    var skipGroupRecord = this.isGrouped && !this.showGroupSummaryInHeader;

    // refresh formula fields and content
    var oldFormulaFields = isc.addProperties({}, this._storedFormulaFields);
    
    this._storeFormulaFieldValues(data, fields, skipGroupRecord, oldFormulaFields);
    var component = this,
        summaryFieldsToRecalculate = {},
        applyHiliteRule = function (hilite) { component.applyHilite(hilite, data); },
        partition = this._partitionSummaryHiliteRules(hilites, summaryFieldsToRecalculate);

    // apply rules that don't depend on summary fields but are summary field inputs
    partition.simpleSummaryAffecting.map(applyHiliteRule);
    // refresh summary fields and content
    var oldSummaryFields = isc.addProperties({}, this._storedSummaryFields);
    this._storeSummaryFieldValues(data, fields, skipGroupRecord, oldSummaryFields);

    // apply all rules for target fields that are not inputs to summary fields
    partition.nonSummaryAffecting.map(applyHiliteRule);

    // recalculate summary fields for complicated rules that both are applied 
    // using summary field-based criteria and also affect inputs to summary fields
    if (partition.summaryDependentSummaryAffecting.length > 0) {
        partition.summaryDependentSummaryAffecting.map(applyHiliteRule);
        this.invalidateUserCache(null, isc.getValues(summaryFieldsToRecalculate));
        this._storeSummaryFieldValues(data, fields, skipGroupRecord, {});
    }
    

    if (!suppressRedraw) this.redrawHilites();
},


// Store a calculated formula field value on a record in our data array.
// This also sets up some metadata so we can clear such values if the formula field
// is removed.
storeFormulaFieldValue : function (field, record, formulaFunction) {
    var fieldName = field[this.fieldIdProperty];
    if (this._storedFormulaFields == null) this._storedFormulaFields = {};
    if (!this._storedFormulaFields[fieldName]) this._storedFormulaFields[fieldName] = true;
    
    this.getFormulaFieldValue(field, record, formulaFunction);
},
storeSummaryFieldValue : function (field, record, summaryFunction) {
    var fieldName = field[this.fieldIdProperty];
    if (this._storedSummaryFields == null) this._storedSummaryFields = {};
    if (!this._storedSummaryFields[fieldName]) this._storedSummaryFields[fieldName] = true;
    
    this.getSummaryFieldValue(field, record, summaryFunction);
},

//> @type FieldNamingStrategy
// The strategy to use when generating field names - for example, for new formula or summary 
// fields created using the built-in editors.
// @value "simple" generate names in the format fieldTypeX, where field type might be 
//         "formulaField" and X is an index specific to the field-type and component instance
// @value "uuid" generates a UUID for all generated field names
// @visibility external
//<

//> @attr dataBoundComponent.fieldNamingStrategy (FieldNamingStrategy : "simple" : IRW)
// The strategy to use when generating names for new fields in this component.  The default 
// strategy, "simple", combines the field-type with an index maintained by field-type and 
// component instance.  For example, "formulaField1".
// @visibility external
//<
fieldNamingStrategy: "simple",
fieldNameGenerator: {},

getHilite : function (hiliteId) {
    if (isc.isAn.Object(hiliteId)) return hiliteId;

    if (this.hilites == null) return null;
    
    if (!this._hiliteIndex && this.hilites) {
        this._setupHilites(this.hilites);
    }

    var hilite = this._hiliteIndex[hiliteId];

    // try hiliteId as an array index
    if (hilite == null) hilite = this.hilites[hiliteId];

    return hilite;
},


applyHilite : function (hilite, data, fieldName) {
    
    var matches = this.getRecordsMatchingHilite(hilite, data);
    if (matches == null || matches.length == 0) return;

    hilite = this.getHilite(hilite);
    // recordsMatchingHilite will have eliminated disabled hilites already.
    // if (hilite.disabled) return;

    var fieldName = fieldName || hilite.fieldName;

    // hilite all fields if no field is specified
    if (fieldName == null) fieldName = this.fields ? this.fields.getProperty("name") : [];

    var fieldNames = isc.isAn.Array(fieldName) ? fieldName : [fieldName];

    if (this.logIsDebugEnabled("hiliting")) {
        this.logDebug("applying hilite: " + this.echo(hilite) + 
                      ", to fields: " + fieldNames, "hiliting");
    }

    for (var j = 0; j < fieldNames.length; j++) {
        var field = this.getField(fieldNames[j]);
        for (var i = 0; i < matches.length; i++) {
            var record = matches[i];
            this.hiliteRecord(record, field, hilite);
        }
    }   
},

// Apply filter to find the subset of data that matches a specified hilite's criteria.
getRecordsMatchingHilite : function (hilite, data) {
    hilite = this.getHilite(hilite);

    // hilite may be applied in some other way, eg manual calls
    if (!hilite.criteria) return;

    if (hilite.disabled) return;

    var matches = [],
        dataSource = this.getDataSource();

    if (dataSource) {
        var drop = dataSource.dropUnknownCriteria;
        var strictSQLFiltering = dataSource.strictSQLFiltering;

        
        dataSource.dropUnknownCriteria = false;
        var undef;
        if (this.strictSQLFilteringForHilites !== undef) dataSource.strictSQLFiltering = this.strictSQLFilteringForHilites;
        matches = this.getDataSource().applyFilter(data, hilite.criteria);
        dataSource.dropUnknownCriteria = drop;
        if (this.strictSQLFilteringForHilites !== undef) dataSource.strictSQLFiltering = strictSQLFiltering;
    } else {
        // Call a local DBC version of DS.applyFilter which provides the same facilities but
        // against array data
        matches = this.unboundApplyFilter(data, hilite.criteria);
    }
    if (this.logIsDebugEnabled("hiliting")) {
        this.logDebug("applying filter: " + this.echoFull(hilite.criteria) + 
                      ", produced matches: " + isc.echoLeaf(matches), "hiliting");
    }

    return matches;
},


// Utility method to provide searching by criteria/AdvancedCriteria in the absence of a DS
unboundApplyFilter : function (data, criteria) {
    var matches = [];

    if (data) {
        if (criteria) {
            for (var idx = 0; idx < data.length; idx++) {
                // The AdvancedCriteria system makes this very easy - just call evaluateCriterion
                // on the top-level criterion, and it handles all the recursion and evaluation of
                // sub-criteria that it needs to do automatically.
                if (!criteria.operator || this.evaluateCriterion(data[idx], criteria)) {
                    matches.add(data[idx]);
                }
            }
        } else {
            matches = data;
        }
    }

    return matches;
},
evaluateCriterion : function (record, criterion) {

    var op = isc.DataSource._operators[criterion.operator];
    if (op == null) {
        isc.logWarn("Attempted to use unknown operator " + criterion.operator);
        return false;
    }
    
    var ds = this.getDataSource();

    var field = this.getField(criterion.fieldName);
    var fieldValue = isc.DataSource.getPathValue(record, criterion.fieldName, field);
    var isDateField = field && (isc.SimpleType.inheritsFrom(field.type, "date") || isc.SimpleType.inheritsFrom(field.type, "datetime"));
    //criterion.fieldName, fieldValueToTest, criterionValues, this, isDateField

    var convertToAbsoluteDateOrKeepOriginalValue = function(value) {
        if (isDateField && !isc.isA.Date(value) && isc.DateUtil.isRelativeDate(value)) {
            return isc.DateUtil.getAbsoluteDate(value);
        }

        return value;
    }

    var criterionValues = {
        value: convertToAbsoluteDateOrKeepOriginalValue(criterion.value),
        start: convertToAbsoluteDateOrKeepOriginalValue(criterion.start),
        end: convertToAbsoluteDateOrKeepOriginalValue(criterion.end)
    };

    if(op.valueType === "fieldName") {
        var otherField = this.getField(criterionValues.value);
        var otherFieldValue = isc.DataSource.getPathValue(record, criterionValues.value, otherField);
        criterionValues.otherValue = convertToAbsoluteDateOrKeepOriginalValue(otherFieldValue);
    } else if (op.valueType === "criteria") {
        // If we're dealing with a criteria operator such as AND, NOT & OR, lets add the criterion
        // to the criterionValues object.
        criterionValues.criterion = criterion;
        criterionValues.record = record;
    }

    var condition = op.condition(criterion.fieldName, fieldValue, criterionValues, ds || this, isDateField);

    // If the operator is negated, return a negated condition
    return (op.negate ? !condition : condition);
},

compareValues : function (value1, value2, fieldName, ignoreCase) {
    if (isc.isA.Date(value1) && isc.isA.Date(value2)) {
        if (value1.logicalDate || value2.logicalDate) {
            return Date.compareLogicalDates(value1, value2);
        } else {
            return Date.compareDates(value1, value2);
        }
    } else {
        var v1 = ignoreCase && value1.toLowerCase ? value1.toLowerCase() : value1, 
            v2 = ignoreCase && value2.toLowerCase ? value2.toLowerCase() : value2;
        // Javascript does not consider null to be less than "some string", though it does
        // consider null to be less than "1".  Work around this anomalous behavior.
        if (v1 == null && v2 != null) return 1;
        if (v1 != null && v2 == null) return -1;
        // NOTE: The special return value 2 means that we've been asked to compare two values 
        // that are not equal but also are not sensibly "greater than" or "less than" 
        // one another - for example "Blink" and 182.
        return v1 > v2 ? -1 : (v1 < v2 ? 1 : (v1 == v2 ? 0 : 2))
    }
},

// hiliteRecord(): Given a record, field, hilite, hang markers on the record/field
// objects such that we can rapidly look up the hilite from the data
// TODO: make external version that checks params

hiliteRecord : function (record, field, hilite) {

	if (!field) return;

    var hiliteCount = record[this.hiliteMarker];
    if (hiliteCount == null) hiliteCount = record[this.hiliteMarker] = this._hiliteCount++;

    var fieldHilites = field._hilites = field._hilites || {}, // XXX wipe these in setFields or
                                                              // similar
        existingHilite = fieldHilites[hiliteCount];

    if (existingHilite == null) fieldHilites[hiliteCount] = hilite.id;
    else if (isc.isAn.Array(existingHilite)) existingHilite.add(hilite.id);
    else fieldHilites[hiliteCount] = [existingHilite, hilite.id];
},

getHiliteCSSText : function (hilite) {
    var hilite = this.getHilite(hilite);
    if (hilite == null) return;
    
    var cssText = hilite.cssText || "";
    if (cssText == "") {
        if (hilite.textColor) cssText += "color:" + hilite.textColor + ";";
        if (hilite.backgroundColor) cssText += "background-color:" + hilite.backgroundColor + ";";
        if (cssText == "") cssText == null;
    }

    // .style is backcompat for old CubeGrid hilites
    return cssText || hilite.style;
},

_hiliteIterator : [],
// addHiliteCSSText(): Given a record and a field, look up the hilite(s) for the
// cell and return appropriate cssText.
// This applies to hilites with a specified criteria that match the record in question,
// and makes use of the markers set up by applyHilites() / hiliteRecord()
addHiliteCSSText : function (record, field, cssText) {
    if (!record) return cssText;

    var hiliteCount = record[this.hiliteMarker],
        field = this.getField(field);

    if (!field || !field._hilites) return cssText;

    var hiliteIds = field._hilites[hiliteCount];
    if (hiliteIds == null) return cssText;

    //this.logWarn("add hiliteCSS: hiliteCount: " + hiliteCount + 
    //             " on field:" + field.name + ", hiliteIds: " + hiliteIds);

    // convert to Array
    if (!isc.isAn.Array(hiliteIds)) {
        this._hiliteIterator[0] = hiliteIds;
        hiliteIds = this._hiliteIterator;
    }
     
    // multiple hilites apply to cell
    for (var i = 0; i < hiliteIds.length; i++) {
        var hiliteCSSText = this.getHiliteCSSText(hiliteIds[i]);
        if (hiliteCSSText != null) {
            cssText = cssText ? cssText + isc.semi + hiliteCSSText : hiliteCSSText;
        }
    }

    return cssText;
},

// addObjectHilites(): Given an object with a specified value for the 
// 'hiliteProperty', return the cssText for the matching hilite.
// Allows developers to specify record._hilite for explicit custom hiliting rather than
// using hilite.criteria

addObjectHilites : function (object, cellCSSText, field) {
    if (!this.hilites || !object) return cellCSSText;

    var objArr;
    if (!isc.isAn.Array(object)) {
        this._hiliteIterator[0] = object;
        objArr = this._hiliteIterator;
    }

    if (objArr && objArr.length>0) {
        for (var i = 0; i < objArr.length; i++) {
            var hiliteID, hilite, hiliteCSSText;
            var currObj = objArr[i];
            
            if (isc.isA.String(currObj)) hiliteID = currObj;
            else hiliteID = (currObj != null ? currObj[this.hiliteProperty] : null);
            
            // get the hilite object (ENH: could support arrays of multiple hilite objects)
            hilite = this.getHilite(hiliteID);
            if (hilite != null && !hilite.disabled) { // we have a hilite object
                // NOTE: "style" is backcompat
                hiliteCSSText = hilite.cssText || hilite.style;
                // make sure that hilites that spec a fieldName are respected

                var fieldNames = [];
                if (hilite)
                    fieldNames = isc.isAn.Array(hilite.fieldName) ? hilite.fieldName : [hilite.fieldName];

                var matchesField = (!hilite.fieldName || !field || fieldNames.contains(field.name));
                if (hiliteCSSText != null && hiliteCSSText != isc.emptyString && matchesField) {
                    // we have a hilite style
                    if (cellCSSText == null) cellCSSText = hiliteCSSText;
                    // NOTE: add a semicolon, even though it may be redundant
                    else cellCSSText += isc.semi + hiliteCSSText;
                }
            }
        }
    }
	return cellCSSText;    
},

// getFieldHilites() returns the actual hilite object(s) for a cell

getFieldHilites : function (record, field) {

    if (!record || !field) return null;

    if (record[this.hiliteProperty] != null) {
        var hilite = this.getHilite(record[this.hiliteProperty]),
            fieldNames;
        if (hilite)
            fieldNames = isc.isAn.Array(hilite.fieldName) ? hilite.fieldName : [hilite.fieldName];
        if (fieldNames && fieldNames.contains(field.name)) return [hilite];
        else return null;
    }
    
    if (record[this.hiliteMarker] != null) {
        var hiliteCount = record[this.hiliteMarker];
        if (!field._hilites) return null;
        else return field._hilites[hiliteCount];
    }
},

applyHiliteHTML : function (hiliteIDs, valueHTML) {
    if (!this.hilites) return valueHTML;
	var hilite, hiliteHTML, hiliteID;
    // convert to Array
    if (!isc.isAn.Array(hiliteIDs)) {
        this._hiliteIterator[0] = hiliteIDs;
        hiliteIDs = this._hiliteIterator;
    }

    for (var i = 0; i< hiliteIDs.length; i++) {
        hiliteID = hiliteIDs[i];
        // get the hilite object
        
        hilite = this.getHilite(hiliteID);
        if (hilite != null) {
            if (hilite.htmlValue != null) valueHTML = hilite.htmlValue;
            if (!hilite.disabled) { // we have a hilite object, not disabled
                if (hilite.replacementValue) {
                    valueHTML = hilite.replacementValue;
                }
                hiliteHTML = hilite.htmlBefore;
                if (hiliteHTML != null && hiliteHTML.length > 0) { // we have hilite htmlBefore, so pre-pend it
                    valueHTML = hiliteHTML + valueHTML;
                }
                hiliteHTML = hilite.htmlAfter;
                if (hiliteHTML != null && hiliteHTML.length > 0) { // we have hilite htmlAfter, so append it
                    valueHTML = valueHTML + hiliteHTML;
                }
        
                // position a special glyph of some sort (eg an image or small text code) opposite the
                // cell value.  NOTE name "htmlOpposite" reflects future support for automatically
                // flipping direction column align and/or RTL.
                var oppositeContent = hilite.htmlOpposite,
                    style = hilite.styleOpposite || this.styleOpposite;
                if (oppositeContent) {
                    if (!isc.Browser.isIE) {
                        // in browsers other than IE, <nobr> works even when surrounding a mixture of
                        // floating and non-floating content
                        valueHTML = "<nobr><div class='" + style + "' style='float:left'>&nbsp;" +
                                 oppositeContent + "&nbsp;</div>" + valueHTML + "</nobr>";
                    } else {
                        
                        valueHTML = "<nobr><table role='presentation' align=left><tr><td class='" + style + "'>" +
                                 oppositeContent + "</td></tr></table>" + valueHTML + "</nobr>";
                    }
                }
            }
        }
    }
	return valueHTML;
},

applyHiliteIcon : function (hiliteIDs, field, cellValue) {
    if (!this.hilites || !field) return cellValue;
    var hilite, hiliteHTML, hiliteID;

    // convert to Array
    if (!isc.isAn.Array(hiliteIDs)) {
        this._hiliteIterator[0] = hiliteIDs;
        hiliteIDs = this._hiliteIterator;
    }

    for (var i = 0; i < hiliteIDs.length; i++) {
        hiliteID = hiliteIDs[i];
        // get the hilite object
        
        hilite = this.getHilite(hiliteID);
        if ((hilite != null) && hilite.icon) {
            hiliteHTML = this.getHiliteIconHTML(hilite.icon, field);
            var hiliteIconPosition = this.getHiliteIconPosition(field);
            if (hiliteIconPosition == "after") {
                cellValue = cellValue + hiliteHTML;
            } else if (hiliteIconPosition == "replace") {
                cellValue = hiliteHTML;
            } else {
                // default is "before"
                cellValue = hiliteHTML + cellValue;
            }
        }
    }

    return cellValue;
},

//>	@method dataBoundComponent.enableHilite()
// Enable / disable a +link{dataBoundComponent.hilites,hilite}
//
// @visibility external
// @group  hiliting
//
// @param  hiliteID    (string)    ID of hilite to enable
// @param  [enable]    (boolean)   new enabled state to apply - if null, defaults to true
//<
enableHilite : function (hiliteID, enable) {
    if (enable == null) enable = true;
    var hilite = this.getHilite(hiliteID);
    if (hilite == null) return;
    hilite.disabled = !enable;
    // redraw to show hilite / lack of hilite
    this.redrawHilites();
},

//>	@method dataBoundComponent.disableHilite()
// Disable a hilite
//
// @visibility external
// @group  hiliting
//
// @param  hiliteID    (string)    ID of hilite to disable
//<
disableHilite : function (hiliteID) { this.enableHilite(hiliteID, false); },

//>	@method dataBoundComponent.enableHiliting()
// Enable all hilites.
//
// @visibility external
// @group  hiliting
//
// @param  [enable]    (boolean)   new enabled state to apply - if null, defaults to true
//<
enableHiliting : function (enable) {
    if (enable == null) enable = true;
    if (this.hilites) this.hilites.setProperty("disabled", !enable);
    this.redrawHilites();
},

//>	@method dataBoundComponent.disableHiliting()
// Disable all hilites.
//
// @visibility external
// @group  hiliting
//<
disableHiliting : function () { this.enableHiliting(false) },

redrawHilites : function () {
    this.markForRedraw();
},

// Returns the fields shown in the hiliteEditor
getHiliteCriteriaFields : function () {
    var fields = this.getAllFields();

    if (!fields) return;

    // if any fields are specifically marked as canHilite: false, remove them from the list -
    // we use this for special listGrid fields, like rowNumber and expansion fields
    var invalidFields = fields ? fields.findAll("canHilite", false) : null;

    if (invalidFields && invalidFields.length > 0) {
        fields.removeList(invalidFields);
    }
    
    for (var i = 0; i < fields.length; i++) {
        if (fields[i].dataPath) {
            fields[i] = isc.addProperties({}, fields[i], 
                {dataPath:this._trimDataPath(fields[i].dataPath)} );                
        }
    }
    return fields;
},

//> @attr dataBoundComponent.editHilitesText (String : "Edit Highlights..." : IRW)
// Text for a menu item allowing users to edit grid highlights.
//
// @group i18nMessages
// @visibility external
//<
editHilitesText: "Edit Highlights...",

//> @attr dataBoundComponent.editHilitesDialogTitle (String : "Edit Highlights" : IR)
// The title for the +link{dataBoundComponent.editHilites, Hilite Editor} dialog.
//
// @group i18nMessages
// @visibility external
//<
editHilitesDialogTitle: "Edit Highlights",

//> @attr dataBoundComponent.hiliteWindow (AutoChild Window : null : R)
// The +link{Window} containing this databound component's +link{DataBoundComponent.hiliteEditor,hiliteEditor}.
// <p>
// The following +link{autoChildUsage,passthroughs} apply:
// <ul>
// <li>+link{DataBoundComponent.editHilitesDialogTitle} for +link{Window.title}</li>
// </ul>
//
// @group hiliting
//<
hiliteWindowConstructor: "Window",

hiliteWindowDefaults: {
    autoDraw: false,
    autoParent: "none",
    height: 400,
    width: 875,
    autoCenter: true,
    overflow: "visible",
    canDragResize: true,
    keepInParentRect:true,
    isModal: true,
    showModalMask: true,
    bodyProperties : { 
        layoutMargin: 8,
        membersMargin: 8,
        overflow: "visible"
    },
    closeClick : function () {
        this.hide();
    }
},

//> @attr dataBoundComponent.fieldEditorWindowTitle (HTMLString : "${builderType} Editor [${fieldTitle}]" : IRWA)
// The title for the +link{dataBoundComponent.fieldEditorWindow, Window} used to edit calculated
// fields.
// <P>
// This is a dynamic string - text within <code>&#36;{...}</code> are dynamic variables and will
// be evaluated as JS code whenever the message is displayed.
// <P>
// Two dynamic variables are available - "builderType", either Formula or Summary, and 
// "fieldTitle", which is the title of the calculated field being edited.
// @visibility external
//<
fieldEditorWindowTitle: "${builderType} Editor [${fieldTitle}]",

//> @attr dataBoundComponent.fieldEditorWindow (AutoChild Window : null : R)
// The +link{Window} used to edit calculated fields for this component.
//
// @visibility external
//<
fieldEditorWindowConstructor: "Window",
fieldEditorWindowDefaults: {
    keepInParentRect:true,
    showMinimizeButton: false, showMaximizeButton: false,
    autoDraw: false,
    isModal: true, 
    showModalMask:true, 
    width: 400,
    height: 400,
    overflow: "visible",
    autoCenter: true,
    bodyProperties: {
        overflow: "visible"
    },
    canDragResize: true,
    headerIconProperties: { padding: 1,
        src: "[SKINIMG]ListGrid/formula_menuItem.png"
    },
    closeClick: function () {
        this.items.get(0).completeEditing(true);
        return this.Super('closeClick', arguments);
    }
},

//> @attr dataBoundComponent.hiliteEditor (AutoChild HiliteEditor : null : R)
// This component's HiliteEditor instance used to allow the user to create, modify, or delete
// hilites.
// <p>
// The following +link{group:autoChildUsage,passthroughs} apply:
// <ul>
// <li>+link{DataBoundComponent.hiliteIcons} for +link{HiliteEditor.hiliteIcons}</li>
// </ul>
//
// @group hiliting
// @visibility external
//<
// <li>+link{DataBoundComponent.hiliteCanReplaceValue} for +link{HiliteEditor.hiliteCanReplaceValue}</li>
// <li>+link{DataBoundComponent.hiliteReplaceValueFieldTitle} for +link{HiliteEditor.hiliteReplaceValueFieldTitle}</li>
hiliteEditorConstructor: "HiliteEditor",

hiliteEditorDefaults: {
    autoDraw: false,
    autoParent: "none",
    callback : function (hilites) {
        var grid = this.creator;
        if (hilites != null) grid._handleHilitesChanged(hilites);
        grid.hiliteWindow.hide();
    }
},

//>	@method dataBoundComponent.editHilites()
// Shows a +link{class:HiliteEditor, HiliteEditor} interface allowing end-users to edit
// the data-hilites currently in use by this DataBoundComponent.
//
// @visibility external
// @group  hiliting
//<
editHilites : function () {

    var thisDS = this.getDataSource(),
        fields = thisDS ? isc.getValues(thisDS.getFields()).duplicate() : [],
        dbcFields = (this.getHiliteCriteriaFields() || []).duplicate(),
        includeAsSummary = []
    ;

    // build a list of fields that are used by any visible summary fields
    for (var i=0; i<dbcFields.length; i++) {
        var dbcField = dbcFields[i];
        if (dbcField.userSummary && this.fieldIsVisible(dbcField.name)) {
            var vars = dbcField.userSummary.summaryVars;
            for (var key in vars) {
                var fieldName = vars[key];
                if (!includeAsSummary.contains(fieldName)) includeAsSummary.add(fieldName);
            }
        }
    }

    fields.setProperty("showInSimpleEditor", false);
    dbcFields.setProperty("showInSimpleEditor", true);
    for (var i=0; i<dbcFields.length; i++) {
        var dbcField = dbcFields[i],
            fieldVisible = this.fieldIsVisible(dbcField.name), // visible in the DBC
            fieldUsedInSummary = includeAsSummary.contains(dbcField.name), // used in a visible summary
            field = fields.find("name", dbcField.name)
        ;
        if (!field) {
            field = isc.addProperties({}, dbcField);
            fields.add(field);
        }
        
        if (dbcField.canHilite != null) field.canHilite = dbcField.canHilite;
        
        field.title = this.getFieldTitle ? this.getFieldTitle(dbcField) : 
                            dbcField.title || dbcField.name;
        // show the field in the simple list if it is either visible or in use by a visible 
        // summary field
        field.showInSimpleEditor = fieldVisible || fieldUsedInSummary;
    }

    // Define dataSource dynamically based on our fields. Will include Formula fields etc.
    var ds = isc.DataSource.create({
        // inheritsFrom is required to pick up fields where the component has a
        // dataPath specified to navigate nested data objects, so we need the fields
        // from the underlying dataSource present in the "hilites" criteria source DS.
        inheritsFrom:this.getDataSource(),
        isHiliteCriteriaDS:true,
        fields: fields
        
    });

    if (this.hiliteWindow) {
        this.hiliteEditor.setDataSource(ds);
        // rootDataSource used by the hiliteEditor getDefaultOptionDataSource stuff
        this.hiliteEditor.rootDataSource = this.getDataSource();
        this.hiliteEditor.clearHilites();
        this.hiliteEditor.setHilites(this.getHilites());
        this.hiliteEditor.setHiliteIcons(this.hiliteIcons);
        this.hiliteWindow.show();
        return;
    }
    var grid = this,
        hiliteEditor = this.addAutoChild("hiliteEditor", {
            dataSource:ds,
            // If a field has a displayField set, and no explicit optionDataSource,
            // we typically fetch options from the component-datasource
            // Pass this to the hiliteEdior so it perform this fetch if necessary
            // (a fetch against the specified "ds" would fail as it is just
            // a schema with no data management set up)
            rootDataSource:this.getDataSource(),
            hilites:this.getHilites(),
            hiliteIcons:this.hiliteIcons,
            hiliteCanReplaceValue:this.hiliteCanReplaceValue,
            hiliteReplaceValueFieldTitle:this.hiliteReplaceValueFieldTitle
        }),
        theWindow = this.addAutoChild("hiliteWindow", {
            title: this.editHilitesDialogTitle,
            items: [ hiliteEditor ]
        });
    theWindow.centerInPage();
    theWindow.show();
    return theWindow;
},

_handleHilitesChanged : function (hilites) {
    this.setHilites(hilites);
},

//
// Drag & Drop
// -----------------------------------------------------------------------------

// These methods are factored up from ListGrid, to make them available to TileGrid.
// They are only applicable to list-type components (as of Oct 2008, ListGrid, TreeGrid 
// and TileGrid).  Although they are here in DataBoundComponent, they also work in the 
// case of non-databound components (as source, target or both).

//> @method dataBoundComponent.transferRecords()
//
// Transfer a list of +link{Record}s from another component (does not have to be a databound
// component) into this component.  This method is only applicable to list-type components,
// such as +link{ListGrid,listGrid}, +link{TreeGrid,treeGrid} or +link{TileGrid,tileGrid}
// <P>
// This method implements the automatic drag-copy and drag-move behaviors of components like
// +link{ListGrid}, and calling it is equivalent to completing a drag and drop of the
// <code>dropRecords</code>.
// <P>
// Note that this method is asynchronous - it may need to perform server turnarounds to prevent
// duplicates in the target component's data.  If you wish to be notified when the transfer 
// process has completed, you can either pass the optional callback to this method or implement
// the +link{dropComplete()} method on this component.
// <P>
// See also +link{transferSelectedData}.
//
// @param dropRecords (Array of Record) Records to transfer to this component
// @param targetRecord (Record) The target record (eg, of a drop interaction), for context
// @param index (integer) Insert point in this component's data for the transferred records
// @param sourceWidget (Canvas) The databound or non-databound component from which the records
//                            are to be transferred. 
// @param [callback] (Callback) optional callback to be fired when the transfer process has completed
//
// @group dragdrop
// @visibility external
//<
transferRecords : function (dropRecords, targetRecord, index, sourceWidget, callback) {
    // storeTransferState returns false if a prior transfer is still running, in which case
    // we just bail out (transferRecords() will be called again when the first transfer 
    // completes, so we aren't abandoning this transfer, just postponing it) 
    if (!this._storeTransferState("transferRecords", dropRecords, targetRecord, index, 
                                  sourceWidget, callback)) {
        return;
    }

    // If this component is databound but has not yet issued a fetchData(), we need to 
    // initialize the ResultSet before adding records, otherwise cache sync will not be in
    // place and it will look to the user like the records haven't been added.  We 
    // initialize the ResultSet with a special call to fetchData() that creates the 
    // ResultSet but suppresses the actual server visit.
    if (isc.isAn.Array(this.data) && this.data.length == 0 && 
        this.dataSource && !this.shouldSaveLocally()) 
    {
        this.fetchData(null, null, {_suppressFetch:true});
        this.data.setFullLength(0);
    }    

	// if reordering records from this list
    if (sourceWidget == this) {
    	// slide them into their new home, if no grouping is applied
        
        if (index != null && !this.isGrouped) this.data.slideList(dropRecords, index);
        
	} else { 

        var dataSource = this.getDataSource();
        var sourceDS = sourceWidget.getDataSource();
        
        // If we're bound to the same dataSource as the source widget and doing a move, apply
        // an update to the source nodes - by default, changing them to match the current 
        // filter criteria of this grid
        if (dataSource && dataSource == sourceDS && 
            sourceWidget.dragDataAction == isc.Canvas.MOVE && 
            !(sourceWidget.shouldSaveLocally() || this.shouldSaveLocally())) 
        {
            var wasAlreadyQueuing = isc.rpc.startQueue();
            for (var i = 0; i < dropRecords.length; i++) {
                var record = {};
                var pks = dataSource.getPrimaryKeyFieldNames();
                for (var j = 0; j < pks.length; j++) {
                    record[pks[j]] = dropRecords[i][pks[j]];
                }
                isc.addProperties(record, this.getDropValues(record, sourceDS, 
                                          targetRecord, index, sourceWidget));
                this.updateDataViaDataSource(record, sourceDS, null, sourceWidget);                          
            }
            if (!wasAlreadyQueuing) isc.rpc.sendQueue();
        } else {
    		if (!isc.isAn.Array(dropRecords)) dropRecords = [dropRecords];

    		// select the stuff that's being dropped
    		// (note: if selectionType == SINGLE we only select the first record)
            
           
            
            var selectRecords = true;
            // If we're dropping between 2 dataSources and the pkField doesn't exist
            // on the source dataSource, don't attempt to select records immediately as
            // they'll likely have no primary key yet meaning we can't perform
            // a selection immediately (this is likely to occur for 
            // primary keys generated by the server - for example 'sequence' type fields)
            if (sourceDS != null && dataSource != null) {
                var pkFields = dataSource.getPrimaryKeyFieldNames(),
                    selectRecords = true;
                if (pkFields) {
                    for (var i = 0; i < pkFields.length; i++) {
                        if (sourceDS.getField(pkFields[i]) == null) {
                            selectRecords = false;
                            break;
                        }
                    }
                }
            }
            if (selectRecords) {
                if (this.selectionType == isc.Selection.MULTIPLE || 
                    this.selectionType == isc.Selection.SIMPLE) 
                {
                    this.selection.deselectAll();
                
                    this.selection.selectList(dropRecords, true, null, this);
                } else if (this.selectionType == isc.Selection.SINGLE) {
                    this.selection.selectSingle(dropRecords[0]);
                }
                this.fireSelectionUpdated();
            }

            
            if (dataSource) {
                this._wasAlreadyQueuing = isc.rpc.startQueue();
                for (var i = 0; i < dropRecords.length; i++) {
                    // groups contain circular references which will hang at clone - skip
                    if (dropRecords[i]._isGroup) continue;
                    var record = {};
                    isc.addProperties(record, dropRecords[i]);
                    isc.addProperties(record, this.getDropValues(record, sourceDS, 
                                            targetRecord, index, sourceWidget));
                    if (dataSource != sourceDS) {
                        // If there is a foreign key relationship from the target DS to the 
                        // source DS, populate the foreignKey field on the record we're 
                        // dropping with the contents of the field the foreignKey points to.
                        var fks = dataSource.getForeignKeysByRelation(record, sourceDS);
                        var cannotRecat = false;
                        isc.addProperties(record, fks);

                        // If we have explicitly defined titleFields and the target one is not 
                        // going to be populated, populate it with the value in the source one
                        if (dataSource.titleField && sourceDS && sourceDS.titleField && 
                                dataSource.titleField != sourceDS.titleField) {
                            var undef;
                            if (record[dataSource.titleField] === undef) {
                                record[dataSource.titleField] = record[sourceDS.titleField];
                            }
                        }
                    }
                                            
                    this._addIfNotDuplicate(record, sourceDS, sourceWidget, fks);
                }
            } else { // target grid does not have a DataSource
                // handle grouping
                if (this.isGrouped) {
                    // add to tree
                    for (var i = 0; i < dropRecords.length; i++) {
                        var record = {};
                        isc.addProperties(record, dropRecords[i]);
                        isc.addProperties(record, this.getDropValues(record, sourceDS, 
                                            targetRecord, index, sourceWidget));
                        if (!this._isDuplicateOnClient(record)) {
                            this._addRecordToGroup(this.groupTree, record, true);
                            
                            // add to originalData
                            // Ignore the index in this case - it will refer to the position within
                            // the tree which doesn't map to a position within the original data
                            // array
                            this.originalData.add(record);
                        }
                    }
                    // add to originalData
                    //if (index != null) this.originalData.addListAt(dropRecords, index);
                    //else this.originalData.addList(dropRecords);
                   
                } else {
                    // If we've been passed an index respect it - this will happen if canReorderRecords
                    // is true
                    
                    for (var i = 0; i < dropRecords.length; i++) {
                        var record = {};
                        isc.addProperties(record, dropRecords[i]);
                        isc.addProperties(record, this.getDropValues(record, sourceDS, 
                                                targetRecord, index, sourceWidget));
                        if (index != null) {
                        
                            // Although _addIfNotDuplicate is an asynchronous method, we know
                            // that this particular invocation of it will be synchronous (because
                            // there's no DataSource and thus no server contact), so if it returns
                            // false, we know authoritatively that no data was added and thus 
                            // index should not be incremented
                            if (this._addIfNotDuplicate(record, null, sourceWidget, 
                                                                null, index)) {
                                // Because we're adding one-at-a-time, increment the index - otherwise,
                                // the effect will be to insert into the grid in reverse order
                                index++;
                            }
                        } else {
                            this._addIfNotDuplicate(record, null, sourceWidget);
                        }
                    }  
                }
        		
            }
        }
	}

    // unsort if we were sorted and records were just placed at an explicit position
    if (this.canReorderRecords && this.getSort) {
        var sort = this.getSort();
        if (sort && sort.length > 0) this.unsort();
    }

    // If this._transferDuplicateQuery is undefined or 0, we didn't need to fire any server 
    // queries, so we can call transferDragData to complete the transfer and send the queue 
    // of updates to the server 
    if (!this._transferDuplicateQuery) {
        isc.Log.logDebug("Invoking transferDragData from inside transferRecords - no server " +
                         "queries needed?", "dragDrop");
        sourceWidget.transferDragData(this._transferExceptionList, this);
        if (dataSource) {
            // send the queue unless we didn't initiate queuing
            if (!this._wasAlreadyQueuing) isc.rpc.sendQueue();
        }
    }
    
    this._transferringRecords = false;
    
},

// Store the details of a transfer in the _dropRecords queue on this component.  We work via
// a queue so that, if we get a transfer request when one is already running (this can happen
// because server-side duplicate checking makes the process asynchronous), we can postpone it
// and run it later as part of the first transfer's cleanup.
_storeTransferState : function (impl, dropRecords, targetRecord, index, sourceWidget, callback) {
    if (!isc.isAn.Array(this._dropRecords)) this._dropRecords = [];

    // If the transfer must wait its turn, add it to the end of the queue.  transferDragData()
    // will re-invoke anything put on the queue when it is its turn
    if (this._transferDuplicateQuery && this._transferDuplicateQuery != 0) {
        isc.logWarn("transferRecords was invoked but the prior transfer is not yet complete - \
                     the transfer will be queued up to run after the current transfer");
        this._dropRecords.add({
            implementation: impl,
            dropRecords: dropRecords,
            targetRecord: targetRecord,
            index: index,
            sourceWidget: sourceWidget,
            callback: callback
        });
        return false;
    }

    // If there's nothing in the way, it's this transfer's turn, so add it to the front of the
    // queue for later reading in transferDragData()
    this._dropRecords.addAt({
        implementation: impl,
        dropRecords: dropRecords,
        targetRecord: targetRecord,
        index: index,
        sourceWidget: sourceWidget,
        callback: callback
    }, 0);

    this._transferringRecords = true;
    this._transferExceptionList = [];
    this._transferDuplicateQuery = 0;
    
    return true;
},


updateDataViaDataSource : function(record, ds, updateProperties, sourceWidget) {

    var _listGrid = this;
    
    // Use updateOperation if applicable
    if (this.updateOperation) {
        if (updateProperties == null) updateProperties = {};
        isc.addProperties(updateProperties, {operationId: this.updateOperation});
    }
    
    if (!this.preventDuplicates) {
        if (!sourceWidget._updatesSent) sourceWidget._updatesSent = 0;
        sourceWidget._updatesSent++;
        ds.updateData(record, function (dsResponse, data, dsRequest) {
            sourceWidget._updateComplete(dsResponse, data, dsRequest);
        }, updateProperties); 
        return;
    }
    
    var criteria = this.getCleanRecordData(record);
    
    if (this.data.find(criteria, null, Array.DATETIME_VALUES)) {
        
        isc.Log.logDebug("Found client-side duplicate, skipping update for '" + 
                     record[isc.firstKey(record)] + "'", "dragDrop"); 
        this._transferExceptionList.add(this.getCleanRecordData(record));
    } else {
        // If we have a full cache, we can go ahead and update now
        if (this.data.allMatchingRowsCached()) {
        if (!sourceWidget._updatesSent) sourceWidget._updatesSent = 0;
            sourceWidget._updatesSent++;
            ds.updateData(record, function (dsResponse, data, dsRequest) {
                sourceWidget._updateComplete(dsResponse, data, dsRequest);
            }, updateProperties); 
        } else { 
            // Cache is incomplete, we'll have to ask the server
            isc.Log.logDebug("Incrementing dup query count: was " + 
                             _listGrid._transferDuplicateQuery, "dragDrop");
            this._transferDuplicateQuery++;
            ds.fetchData(criteria, 
                function (dsResponse, data, dsRequest) {
                    if (data && data.length > 0) {
                        
                        isc.Log.logDebug("Found server-side duplicate, skipping update for '" + 
                                     record[isc.firstKey(record)] + "'", "dragDrop"); 
                        _listGrid._transferExceptionList.add(_listGrid.getCleanRecordData(data[0]));
                    } else {
                        if (!sourceWidget._updatesSent) sourceWidget._updatesSent = 0;
                        sourceWidget._updatesSent++;
                        ds.updateData(record, function (dsResponse, data, dsRequest) {
                            sourceWidget._updateComplete(dsResponse, data, dsRequest);
                        }, updateProperties); 
                    }
                    // If there are no further duplicate queries pending, we can finish up this
                    // transfer and send the queue of updates to the server
                    isc.Log.logDebug("Decrementing dup query count: was " + 
                                     _listGrid._transferDuplicateQuery, "dragDrop");
                    if (--_listGrid._transferDuplicateQuery == 0 && 
                        !_listGrid._transferringRecords) {
                        if (sourceWidget.dragDataAction == isc.Canvas.MOVE) {
                            isc.Log.logDebug("Invoking transferDragData from inside callback", "dragDrop");
                            sourceWidget.transferDragData(_listGrid._transferExceptionList, _listGrid);
                            delete _listGrid._transferExceptionList;
                            // send the queue unless we didn't initiate queuing
                            if (!_listGrid._wasAlreadyQueuing) isc.rpc.sendQueue();
                        }
                    }
                },
                {sendNoQueue: true});
        }
    }

},


_addIfNotDuplicate : function (record, sourceDS, sourceWidget, foreignKeys, index, folder) {

    var ds = this.getDataSource(), 
        pks,
        _listGrid = this,
        addProps = {};
        
    if (this.addOperation) {
        isc.addProperties(addProps, {operationId: this.addOperation});
    }
        
    if (ds) pks = ds.getPrimaryKeyFields();

    // If we have a target datasource and we have a PK, and at least one of the PK fields is 
    // a sequence, we don't need to check for duplicates because we can assume the server 
    // arranges for a unique value as part of the create process.  This is the only
    // circumstance in which we have a dataSource but don't need to check the server.  Note
    // that this special case code is duplicated in _isDuplicateOnClient() because that method 
    // is called from other places.
    //
    // Note that we do this special check even before the simple check on this.preventDuplicates
    // because we need special key handling in this circumstance, even if the duplicate check 
    // was going to pass anyway because we haven't set preventDuplicates.
    //
    
    if (ds) {
        var proceed;
        if (pks && isc.firstKey(pks) != null) {
            for (var field in pks) {
                if (pks[field].type == "sequence") {
                    proceed = true;
                    break;
                }
            }
        }
        
        if (proceed) {
            // Clear the primary key field(s) before calling to the server, otherwise the add 
            // works but we get sent back the original keys and it confuses the client-side
            var undef;
            for (var field in pks) {
                record[field] = undef;
            }
            
            if (!sourceWidget._updatesSent) sourceWidget._updatesSent = 0;
            sourceWidget._updatesSent++;
            this.addData(record, function (dsResponse, data, dsRequest) {
                sourceWidget._updateComplete(dsResponse, data, dsRequest);
            });
            return true;
        }
    }

    if (!this.preventDuplicates) {
        if (ds) {
            if (!sourceWidget._updatesSent) sourceWidget._updatesSent = 0;
            sourceWidget._updatesSent++;

            this.addData(record, function (dsResponse, data, dsRequest) { 
                sourceWidget._updateComplete(dsResponse, data, dsRequest); 
            }, addProps);
        } else {
            if (isc.Tree && isc.isA.Tree(this.data)) {
                this.data.add(record, folder, index);
            } else if (isc.ResultSet && isc.isA.ResultSet(this.data)) {
                if (this.data.allRows != null) {
                    if (index != null) this.data.allRows.addAt(record, index);
                    else this.data.allRows.add(record);
                    this.data.filterLocalData();
                } else {
                    isc.logWarn("Unable to add data to resultSet - allRows is not set");
                }
            } else {
                if (index != null) this.data.addAt(record, index);
                else this.data.add(record);
            }
        }
        return true;
    }        
 
    if (this._isDuplicateOnClient(record, sourceDS, foreignKeys)) {
        if (this.duplicateDragMessage != null) isc.warn(this.duplicateDragMessage);
        isc.Log.logDebug("Found client-side duplicate, adding '" + 
                         record[isc.firstKey(record)] + 
                         "' to exception list", "dragDrop");
        this._transferExceptionList.add(this.getCleanRecordData(record));
        return false;
    } else {
        if (!ds) {
            // Simplest case - no DS and no dup on client-side, so go ahead and add the record to
            // the underlying data model
            if (isc.Tree && isc.isA.Tree(this.data)) {
                this.data.add(record, folder, index);
            }  else if (isc.ResultSet && isc.isA.ResultSet(this.data)) {
                if (this.data.allRows != null) {
                    if (index != null) this.data.allRows.addAt(record, index);
                    else this.data.allRows.add(record);
                } else {
                    isc.logWarn("Unable to add data to resultSet - allRows is not set");
                }
            } else {
                if (index != null) this.data.addAt(record, index);
                else this.data.add(record);
            }
            return true;
        } else { 
            if (!isc.ResultSet || !isc.isA.ResultSet(this.data)) {
                
                if (!sourceWidget._updatesSent) sourceWidget._updatesSent = 0;
                sourceWidget._updatesSent++;
                this.addData(record, function (dsResponse, data, dsRequest) { 
                    sourceWidget._updateComplete(dsResponse, data, dsRequest); 
                }, addProps);
                return true
            } else {
                // If we're dropping in a grid bound to a DS different from the source DS
                // and the two are related by foreignKey(s) (ie, the fks object is non-null), this is a 
                // different scenario from a normal copy because it's enough to to know that the dropped 
                // item doesn't exist in the current filtered view of this ListGrid.  So, if we have a 
                // complete cache for the current filter criteria, we don't need to query the server.
                // This is not true for other copying scenarios, where we need a complete, unfiltered
                // cache to avoid the server query.
                if (this.data.allRowsCached() || 
                    (foreignKeys && isc.firstKey(foreignKeys) && this.data.allMatchingRowsCached())) {
                    if (!sourceWidget._updatesSent) sourceWidget._updatesSent = 0;
                    sourceWidget._updatesSent++;
                    this.addData(record, function (dsResponse, data, dsRequest) { 
                        sourceWidget._updateComplete(dsResponse, data, dsRequest); 
                    }, addProps);
                    return true;
                }
                // We have a dataSource and client-side search failed to find a duplicate.  We need a 
                // server turnaround to know for sure whether we're proposing to add a duplicate
                if (ds && sourceDS == ds) {
                    if (pks && isc.firstKey(pks) != null) {
                        // Source DS and target DS are the same and we have a primary key
                        var criteria = isc.applyMask(record, pks);
                    } else {
                        // Source DS and target DS are the same and we have no primary key
                        criteria = this.getCleanRecordData(record);
                    }
                } else if (foreignKeys && isc.firstKey(foreignKeys)) {
                    // Source DS and target DS are different but related via a foreign key
                    criteria = isc.addProperties({}, this.data.getCriteria());
                    isc.addProperties(criteria, foreignKeys);
                } else if (ds && pks && isc.firstKey(pks) != null) {
                    // Target DS exists and has PKs defined, but either there is no source DS, or the 
                    // source DS is different.  Report duplicate if there is a PK collision
                    criteria = isc.applyMask(record, pks);
                } else {
                    // Either the target grid is not bound to a DS, or the target DS has no PKs
                    criteria = this.getCleanRecordData(record);
                }
                isc.Log.logDebug("Incrementing dup query count: was " + 
                                 _listGrid._transferDuplicateQuery, "dragDrop");
                this._transferDuplicateQuery++;
                ds.fetchData(criteria, function (dsResponse, data, dsRequest) {
                    if (data && data.length > 0) {

                        if (_listGrid.duplicateDragMessage != null) isc.warn(_listGrid.duplicateDragMessage);
                        isc.Log.logDebug("Found server-side duplicate, adding '" + 
                                     record[isc.firstKey(record)] + 
                                     "' to exception list", "dragDrop");
                        _listGrid._transferExceptionList.add(_listGrid.getCleanRecordData(record));
                    } else {
                        if (!sourceWidget._updatesSent) sourceWidget._updatesSent = 0;
                        sourceWidget._updatesSent++;
                        ds.addData(record, function (dsResponse, data, dsRequest) { 
                            sourceWidget._updateComplete(dsResponse, data, dsRequest); 
                        }, addProps);
                    }
                    // If there are no further duplicate queries pending, we know exactly which
                    // attempted transfers were duplicates (if any), so we're in a position to 
                    // remove the source records if this was a MOVE, and to send the queue of 
                    // updates to the server
                    isc.Log.logDebug("Decrementing dup query count: was " + 
                                     _listGrid._transferDuplicateQuery, "dragDrop");
                    if (--_listGrid._transferDuplicateQuery == 0 && 
                        !_listGrid._transferringRecords) {
                        if (sourceWidget.dragDataAction == isc.Canvas.MOVE) {
                            isc.Log.logDebug("Invoking transferDragData from inside callback", "dragDrop");
                            sourceWidget.transferDragData(_listGrid._transferExceptionList, _listGrid);
                            delete _listGrid._transferExceptionList;
                            // send the queue unless we didn't initiate queuing
                            if (!_listGrid._wasAlreadyQueuing) isc.rpc.sendQueue();
                        }
                    }
                    
                    },
                    {sendNoQueue: true});
            }
        }
    }
},

// Returns true if the passed-in record is a duplicate - according to the rules described in the 
// discussion above _addIfNotDuplicate() - in the currently-known client data.  Handles both
// dataSource and non-dataSource cases.  Note that this function can return false even if the 
// record is a duplicate - for example, if this.preventDuplicates is false.
_isDuplicateOnClient : function (record, sourceDS, foreignKeys) {
    var ds = this.getDataSource(), 
        pks;
    
    if (!this.preventDuplicates) return false;
        
    if (ds) pks = ds.getPrimaryKeyFields();

    // If the source and target datasource are the same, and we have a PK, and at least one of
    // the PK fields is a sequence, we don't need to check for duplicates because we can assume 
    // the server arranges for a unique value as part of the create process.  Note that
    // this logic is duplicated from _addIfNotDuplicate() because this method is called from 
    // other places.
    if (ds && ds == sourceDS) {
        if (pks && isc.firstKey(pks) != null) {
            for (var field in pks) {
                if (pks[field].type == "sequence") {
                    return false;
                }
            }
        }
    }

    if (!ds) {
        // No DS - a duplicate is one that is identical in every property
        var criteria = this.getCleanRecordData(record);
    } else if (ds && sourceDS == ds) {
        if (pks && isc.firstKey(pks) != null) {
            // Source DS and target DS are the same and we have a primary key - compare PK fields
            criteria = isc.applyMask(record, pks);
        } else {
            // Source DS and target DS are the same and we have no primary key - compare all fields
            criteria = this.getCleanRecordData(record);
        }
        // no foreignKeys is supplied as {} rather than null, hence the firstKey check
    } else if (foreignKeys && isc.firstKey(foreignKeys)) {
        // Source DS and target DS are different but related via a foreign key - check for a record
        // that matches for the combination of the foreign key values and current filter criteria
        criteria = {};
        var tempCrit = this.data.getCriteria();
        if (!ds.isAdvancedCriteria(tempCrit)) {
            var context = this.data.context;
            if (!context || (context.textMatchStyle == null || context.textMatchStyle == "exact")) {
                isc.addProperties(criteria, tempCrit);
            }
        }
        isc.addProperties(criteria, foreignKeys);
    } else if (ds && pks && isc.firstKey(pks) != null) {
        // Target DS exists and has PKs defined, but either there is no source DS, or the 
        // source DS is different.  Report duplicate if there is a PK collision
        
        criteria = isc.applyMask(record, pks);
    } else {
        // Either the target grid is not bound to a DS, or the target DS has no PKs
        criteria = this.getCleanRecordData(record);
    }

    // check the originalData if it's there, because rows in closed groups may not be in the data
    var data = this.originalData || this.data;
    if (data.findIndex(criteria, null, Array.DATETIME_VALUES) >= 0) return true;
    else return false;
},

getCleanRecordData : function (record) {
    if (isc.Tree && isc.isA.Tree(this.data)) {
        return this.data.getCleanNodeData(record, false);
    }
    var clean = {};
    for (var key in record) {
        // These are just the properties that LG scribbles onto its records. If you have others, it's 
        // safe to exclude them in-place below, or just override this method.
        if (key.startsWith("_selection_")) continue;
        
        clean[key] = record[key];
    }
    
    return clean;
},

_updateComplete : function (dsResponse, data, dsRequest) {
    if (this._updatesSent) {
        isc.Log.logDebug("Decrementing update count - was " + this._updatesSent, "dragDrop");
        this._updatesSent -= 1;
    }
    if (!this._updatesSent) {
        isc.Log.logDebug("All updates complete, calling dragComplete()", "dragDrop");
        if (isc.isA.Function(this.dragComplete)) this.dragComplete();
    }
},

//> @method dataBoundComponent.getDropValues()
// Returns the "drop values" to apply to a record dropped on this component prior to update.  Only
// applicable to databound components - see +link{dropValues} for more details.  If multiple records 
// are being dropped, this method is called for each of them in turn.
// <P>
// The default implementation of this method returns the following:
// <UL>
// <LI>Nothing, if +link{addDropValues} is false</LI>
// <LI>dropValues, if that property is set.  If the component's criteria object is applicable (as explained
// in the next item), it is merged into dropValues, with properties in dropValues taking precedence.</LI>
// <LI>The component's criteria object, if the most recent textMatchStyle for the component was "exact" 
//     and it is simple criteria (ie, not an AdvancedCriteria object)</LI>
// <LI>Otherwise nothing</LI>
// </UL>
// <P>
// You can override this method if you need more complex setting of drop values than can be 
// provided by simply supplying a dropValues object.
// 
// @param record (Record) record being dropped
// @param sourceDS (DataSource) dataSource the record being dropped is bound to
// @param targetRecord (Record) record being dropped on
// @param index (int) index of record being dropped on
// @param sourceWidget (Canvas) widget where dragging began
// @return (object) dropValues, as described above.
// 
// @visibility external
//<
getDropValues : function (record, sourceDS, targetRecord, index, sourceWidget, droppedRecords) {
    if (!this.addDropValues) return;
    
    var criteria = {},
        recordDS;
    
    // At the moment, only trees can contain records (nodes) that have their own dataSource
    if (this.data && this.data.getNodeDataSource) {
        recordDS = this.data.getNodeDataSource(targetRecord);
    }
    // recordDS may be null at this point:
    // - we may have never been populated with data (no filter)
    // - getNodeDataSource returns null if you pass the root node in - this appears to be
    //    intentional, so we'll cope with it here rather than risk breaking something
    if (!recordDS) {
        recordDS = this.getDataSource();
    }
    
    // Use original data for a grouped grid for correct criteria and context
    var data = (this.isGrouped ? this.getOriginalData() : this.data);

    // Passing the recordDS parameter is only applicable to trees, but does no harm for lists
    if (data && data.getCriteria) criteria = data.getCriteria(recordDS);

    var merged;
    // If we have an empty object we know it's not 'advanced' criteria
    if (isc.isAn.emptyObject(criteria) || (recordDS && !recordDS.isAdvancedCriteria(criteria))) {
        var context = data.context;
        if (context && (context.textMatchStyle == null || context.textMatchStyle == "exact")) {
            merged = isc.addProperties({}, criteria);
            if (this.dropValues) {
                merged = isc.addProperties(merged, this.dropValues);
            }    
            return merged;
        }
    }
    
    return this.dropValues;   
},

//>	@method	dataBoundComponent.transferDragData()	(A)
//
// During a drag-and-drop interaction, this method is called to transfer a set of records that
// were dropped onto some other component.  This method is called after the set of records has
// been copied to the other component.  Whether or not this component's data is modified is 
// determined by the value of +link{dataBoundComponent.dragDataAction}.
// <P>
// With a <code>dragDataAction</code> of "move", a databound component will issue "remove"
// dsRequests against its DataSource to actually remove the data, via
// +link{dataSource.removeData()}.
//
// @return		(Array)		Array of objects that were dragged out of this ListGrid.
// 
// 
// @see DataBoundComponent.getDragData()
// @see ListGrid.willAcceptDrop();
//
// @visibility external
//<

transferDragData : function (transferExceptionList, targetWidget) {
    var selection = [],
        workSelection,
        callback,
        data;

    if (targetWidget && targetWidget._dropRecords != null && !targetWidget._dropRecords.isEmpty()) {
        data = targetWidget._dropRecords.shift();
        workSelection = data.dropRecords;
        callback = data.callback;
    } else {
        workSelection = this.getDragData();
        data = {};
    }

    if (workSelection == null) workSelection = [];

    // Filter the entries in the exception list out of the selection - we're not going to do
    // anything with them whatever the circumstances
    for (var i = 0; i < workSelection.length; i++) {
        var clean = this.getCleanRecordData(workSelection[i]);
        if (!transferExceptionList || !transferExceptionList.find(clean, null, Array.DATETIME_VALUES)) {
            // Include the dirty version of the record - it will likely have _selection_
            // scribbles on it that are required for an exact match lookup in the underlying
            // dataset
            selection.add(workSelection[i]);
        }
    }

	if (this.dragDataAction == isc.Canvas.MOVE && targetWidget != this && !data.noRemove) {

        if (this.dataSource && !this.shouldSaveLocally()) {

            // In the special case of a MOVE between two components bound to the same dataSource,
            // transferRecords() handles the transfer with update operations rather than removing
            // and adding. So in that case, we don't want to remove anything from the source 
            // component (since it's databound, it will be sync'd automatically)
            var targetDS = targetWidget.getDataSource();
            if (targetDS != this.getDataSource()) {
                var wasAlreadyQueuing = isc.rpc.startQueue();
                for (var i = 0; i < selection.length; i++) {
                    this.getDataSource().removeData(selection[i]);
                }
                // send the queue unless we didn't initiate queuing
                if (!wasAlreadyQueuing) isc.rpc.sendQueue();
            }
        } else if (this.data) {
            var removeFromAllRows = isc.ResultSet && isc.isA.ResultSet(this.data) && this.shouldSaveLocally();
            for (var i = 0; i < selection.length; i++) {
                if (removeFromAllRows) {
                    if (this.data.allRows != null) {
                        this.data.allRows.remove(selection[i]);
                    } else {
                        isc.logWarn("Unable to remove data from resultSet - allRows is not set");
                    }
                } else {
                    this.data.remove(selection[i]);
                }
                if (this.isGrouped) {
                    this.originalData.remove(selection[i]);
                }
            }
            if (removeFromAllRows) this.data.filterLocalData();
        }
        // de-select the selection in the context of this list
        // so if it is dragged *back* into the list, it won't already be selected!
        if (this.selection && this.selection.deselectList) {
            
            this.selection.deselectList(workSelection, this);
        }
    }

    if (targetWidget) {
        // Invoke the user event, if one is implemented
        if (isc.isA.Function(targetWidget.dropComplete)) targetWidget.dropComplete(selection);

        // Fire the callback, if one was provided
        if (callback) {
            this.fireCallback(callback, "records", [selection]);
        }

        // If the target widget's _dropRecords member still has entries, we've got drag and drop
        // transactions queuing up for it, so schedule the next one before ending.
        if (targetWidget._dropRecords && targetWidget._dropRecords.length > 0) {
            var next = targetWidget._dropRecords.shift();
            isc.Timer.setTimeout(function () {
                if (next.implementation == "transferNodes") {
                    targetWidget.transferNodes(next.dropRecords, next.targetRecord, next.index, 
                                               next.sourceWidget, next.callback);
                } else {
                    targetWidget.transferRecords(next.dropRecords, next.targetRecord, next.index, 
                                                 next.sourceWidget, next.callback);
                }
            }, 0);
        }
    }

	return selection;
},

//>	@method	dataBoundComponent.getDragData()	(A)
//
// During a drag-and-drop interaction, this method returns the set of records being dragged out
// of the component.  In the default implementation, this is the list of currently selected
// records.<p>
// 
// This method is consulted by +link{ListGrid.willAcceptDrop()}.

// @param source (DataBoundComponent) source component from which the records will be transferred
// 
// @group	dragging, data
//
// @return	(Array of Record)		Array of +link{Record}s that are currently selected.
// 
// @visibility external
//<
getDragData : function () {
    var selection = (this.selection && this.selection.getSelection) ?
                                        this.selection.getSelection() : null;

    return selection;
},

//>	@method	dataBoundComponent.cloneDragData()	(A)
//
// During a drag-and-drop interaction, this method returns the set of records being dragged out
// of the component.  It differs from +link{dataBoundComponent.getDragData()} in that some extra
// preparation is done to the set of records, making them suitable for passing to the method 
// that actually carries out the transfer (+link{dataBoundComponent.transferRecords()}.  Note that,
// despite the name, records are not always cloned - sometimes they new, cleaned versions of the
// selected records and sometimes (if we're doing a move rather than a copy) we return the 
// selected records themselves.
// 
// This method is called by functions that commence the actual record transfer process:  
// +link{dataBoundComponent.transferSelectedData() and the drop() methods of record-based,
// databound classes like +link{class:ListGrid}

// @param source (DataBoundComponent) source component from which the records will be transferred
// 
// @group	dragging, data
//
// @return	(Array of Record)		Array of +link{Record}s that are currently selected.
// 
// @see DataBoundComponent.getDragData
// @visibility internal
//<
cloneDragData : function () {
    var selection = this._selectionAtDragStart;
    if (selection == null) { 
        selection = this.getDragData();
    }
    this._selectionAtDragStart = null;
    
    var copyData = this.dragDataAction == isc.Canvas.COPY || 
                   this.dragDataAction == isc.Canvas.CLONE;

    var oldComponents = []

    if (copyData && selection) {
        if (isc.isA.Tree(this.data)) {
            selection = this.data.getCleanNodeData(selection);
        } else {
            if (!isc.isAn.Array(selection)) selection = [selection];

            var clonedSelection = [];
            for (var i=0; i<selection.length; i++) {
                // clear any embedded components as part of cloning
                clonedSelection[i] = this.getCleanRecordData(selection[i]);
            }

            selection = clonedSelection;
        }
    }

    return selection;
},

//>	@attr dataBoundComponent.dragDataAction (DragDataAction : isc.Canvas.MOVE : IRW)
// Indicates what to do with data dragged into another DataBoundComponent. See
// DragDataAction type for details.
// @group dragging
// @visibility external
// @example gridsDragMove
// @example gridsDragCopy
//<

dragDataAction: isc.Canvas.MOVE,

//> @method dataBoundComponent.transferSelectedData()
// Simulates a drag / drop type transfer of the selected records in some other component to this
// component, without requiring any user interaction.  This method acts on the dropped records 
// exactly as if they had been dropped in an actual drag / drop interaction, including any 
// special databound behavior invoked by calling
// +link{DataBoundComponent.getDropValues,getDropValues} for each dropped record.
// <P>
// To transfer <b>all</b> data in, for example, a +link{ListGrid}, call grid.selection.selectAll() first.
// <P>
// Note that drag/drop type transfers of records between components are asynchronous operations:
// SmartClient may need to perform server turnarounds to establish whether dropped records 
// already exist in the target component.  Therefore, it is possible to issue a call to 
// transferSelectedData() and/or the +link{listGrid.drop(),drop()} method of a databound 
// component whilst a transfer is still active.  When this happens, SmartClient adds the 
// second and subsequent transfer requests to a queue and runs them one after the other.  If 
// you want to be notified when a transfer process has actually completed, either provide a 
// callback to this method or implement +link{dataBoundComponent.dropComplete()}.
// <P>
// See the +link{group:dragging} documentation for an overview of list grid drag/drop data
// transfer.
// 
// @param source (DataBoundComponent) source component from which the records will be transferred
// @param [index] (integer) target index (drop position) of the rows within this grid.
// @param [callback] (Callback) optional callback to be fired when the transfer process has 
//                       completed.  The callback will be passed a single parameter "records",
//                       the list of records actually transferred to this component.
// @group dragdrop
// @example dragListMove
// @visibility external
//<
transferSelectedData : function (source, index, callback) {
    
    if (!this.isValidTransferSource(source)) {
        if (callback) this.fireCallback(callback);
        return;
    }
            
    // don't check willAcceptDrop() this is essentially a parallel mechanism, so the developer 
    // shouldn't have to set that property directly.
    if (index != null) index = Math.min(index, this.data.getLength());
        
    // Call cloneDragData to pull the records out of our dataset
    
    
    

    var dropRecords = source.cloneDragData();
    var targetRecord;
    if (index != null) targetRecord = this.data.get(index);
    
    this.transferRecords(dropRecords, targetRecord, index, source, callback);
},

// helper for transferSelectedData()
isValidTransferSource : function (source) {
    if (!source || !source.transferDragData) {
        this.logWarn("transferSelectedData(): " + (source ? "Invalid " : "No ") + 
                     "source widget passed in - " + (source || "") + 
                     " taking no action.");
        return false;
    }
    if (source == this) {
        this.logWarn("transferSelectedData(): target parameter contains a pointer back to this grid - ignoring");
        return false;
    }
    return true;
},

// -----------------------------------------------------------------------------------
// Drag tracker and drag line

//>@method  dataBoundComponent.setDragTracker()
// Sets the custom tracker HTML to display next to the mouse when the user initiates a drag
// operation on this component. Default implementation will examine +link{listGrid.dragTrackerMode}
// and set the custom drag tracker to display the appropriate HTML based on the selected record.
// <br>
// To display custom drag tracker HTML, this method may be overridden - call 
// +link{EventHandler.setDragTracker()} to actually update the drag tracker HTML.
// @return (boolean) returns false by default to suppress 'setDragTracker' on any ancestors
//                   of this component.
// @group dragTracker
// @visibility external
//<
setDragTracker : function () {
    var EH = isc.EH, dragTrackerMode = this.dragTrackerMode;

    // When canDragSelectText:true no tracker should be shown. This option is mutually
    // exclusive with other drag actions.
    if (dragTrackerMode == "none" || EH.dragOperation == EH.DRAG_SCROLL || this.canDragSelectText) {
        // we can't just not call setDragTracker(), or the dragTracker will be set to the
        // default canvas tracker image.
        EH.setDragTracker("");
        return false;
    } else if (dragTrackerMode == "icon") {
        var selection = this.getSelection(),
            icon = this.getDragTrackerIcon(selection);

            EH.setDragTracker(this.imgHTML(icon), null,null,null,null, this.getDragTrackerProperties());
            return false;
    } else {
        
        var record = this.getSelectedRecord(),
            rowNum = record && this.data ? this.data.indexOf(record) : -1;

        // can happen on grids with no selection enabled
        if (record == null) return false;

        if (dragTrackerMode == "title") {
            var title = this.getDragTrackerTitle(record, rowNum);
            EH.setDragTracker(title,  null,null,null,null, this.getDragTrackerProperties());
            return false;   
        } else if (dragTrackerMode == "record") {
            var rowHTML = this.body.getTableHTML([0, this.fields.length-1], rowNum, rowNum+1);
            //this.logWarn("row html:"+ rowHTML);
            EH.setDragTracker(rowHTML,  null,null,null,null, this.getDragTrackerProperties());
            return false;
        }            
    }
    // If dragTrackerMode is unrecognized, let the normal tracker show up.
},	

//> @method dataBoundComponent.getDragTrackerProperties()
// Return properties to apply to the drag tracker when the user drags some record.<br>
// Default implementation returns an object with attribute <code>opacity</code> set 
// to <code>50</code> if +link{listGrid.dragTrackerMode} is set to <code>"record"</code>, 
// otherwise returns null.
// @group dragTracker
// @return (object | null) Properties apply to the drag tracker 
//<
getDragTrackerProperties : function () {
    var props = isc.addProperties({}, this.dragTrackerProperties);
    props.styleName = this.dragTrackerStyle;
    if (this.dragTrackerMode == "record") props.opacity = 50;
    return props;
},

//> @attr dataBoundComponent.dragTrackerStyle (CSSStyleName : "gridDragTracker" : IRW)
// CSS Style to apply to the drag tracker when dragging occurs on this component.
// @visibility external
//<
dragTrackerStyle:"gridDragTracker",

//>	@method	dataBoundComponent.makeDragLine()	(A)
//		@group	dragging, drawing
//			make the dragLine 
//		@return	(boolean)	false if this._dragLine already exists
//<
makeDragLine : function () {
	if (this._dragLine) return false;
	
	// create the dragLine and move it to the front
	
    var dragLine = {
        ID:this.getID()+"_dragLine",
		width:2,
		height:2,
		overflow:isc.Canvas.HIDDEN,
        visibility:isc.Canvas.HIDDEN,
        isMouseTransparent:true, // to prevent dragline occlusion of drop events
        dropTarget:this, // delegate dropTarget
		redrawOnResize:false,
        styleName:"dragLine"
        //,backgroundColor:"black"
	};
    //>!BackCompat 2005.01.01 XXX old skin files didn't define a drag line style, so ensure the
    // line shows up.
    if (this.ns.Element.getStyleEdges(dragLine.styleName) == null) {
        dragLine.backgroundColor = "black";
    } //<!BackCompat
    isc.addProperties(dragLine, this.dragLineDefaults, this.dragLineProperties);
	this._dragLine = this.ns.Canvas.create(dragLine);
	
	return true;
},

//>	@method	dataBoundComponent.hideDragLine()	(A)
//		@group	dragging, drawing
//			hide the dragLine
//<
hideDragLine : function () {
	if (this._dragLine) {
	    this._dragLine.hide();
	    // shift it offscreen too so it doesn't take up any scroll space!
	    this._dragLine.moveTo(0, -9999);
    }
},

// Properties related to panelHeader Actions
canExport: true,
canPrint: true,

panelControls: ["action:edit", "action:editNew", "action:sort", "action:export", "action:print"],

dbcProperties: ["autoFetchData", "autoFetchTextMatchStyle", "autoFetchAsFilter", "dataSource"],

// Core facility to configure one DBC from another (initially for use in MultiView)
configureFrom : function (existingDBC) {
    var props = this.dbcProperties;

    for (var i=0; i<props.length;i++) {
        this[props[i]] = existingDBC[props[i]];
        if (props[i] == "dataSource") {
            var fetchData = this.autoFetchData;
            this.autoFetchData = false;
            this.setDataSource(isc.DS.getDataSource(this.dataSource));
            this.autoFetchData = fetchData;
        }
    }

    
    this.setCriteria(existingDBC.getCriteria());
    this.setData(existingDBC.getData());
},

// Formula/Summary Builders
// -----------------------------------------------------------------------------------

//>	@attr dataBoundComponent.badFormulaResultValue		(String : "." : IRW)
// If the result of a formula evaluation is invalid (specifically, if isNaN(result)==true),
// badFormulaResultValue is displayed instead.  The default value is ".".
//
// @group formulaFields
// @visibility external
//<
badFormulaResultValue: ".",

//>	@attr dataBoundComponent.missingSummaryFieldValue		(String : "-" : IRW)
// If a summary format string contains an invalid field reference, replace the reference
// with the missingSummaryFieldValue. The default value is "-".
//
// @group summaryFields
// @visibility external
//<
missingSummaryFieldValue: "-",

//>	@attr dataBoundComponent.missingFormulaFieldValue (String : "-" : IRW)
// If a formula format string contains an invalid field reference, replace the reference
// with the missingFormulaFieldValue. The default value is "-".
//
// @group formulaFields
//<
missingFormulaFieldValue: "-",

//> @attr dataBoundComponent.canAddFormulaFields (boolean : false : IRW)
// Adds an item to the header context menu allowing users to launch a dialog to define a new
// field based on values present in other fields, using the +link{FormulaBuilder}.
// <P>
// User-added formula fields can be persisted via +link{listGrid.getFieldState()} and 
// +link{listGrid.setFieldState()}.
// 
// @group formulaFields
// @visibility external
//<
canAddFormulaFields:false,

//> @attr dataBoundComponent.addFormulaFieldText (String : "Add formula column..." : IRW)
// Text for a menu item allowing users to add a formula field
//
// @group i18nMessages
// @visibility external
//<
addFormulaFieldText: "Add formula column...",

//> @method dataBoundComponent.addFormulaField
// Convenience method to display a +link{FormulaBuilder} to create a new Formula Field.  This 
// is equivalent to calling +link{dataBoundComponent.editFormulaField, editFormulaField()} with 
// no parameter.
//
// @group formulaFields
// @visibility external
//<
addFormulaField : function () {
    this.editFormulaField();
},

//> @attr dataBoundComponent.editFormulaFieldText (String : "Edit formula..." : IRW)
// Text for a menu item allowing users to edit a formula field
//
// @group i18nMessages
// @visibility external
//<
editFormulaFieldText: "Edit formula...",

//> @attr dataBoundComponent.removeFormulaFieldText (String: "Remove formula" : IRW)
// Text for a menu item allowing users to remove a formula field
//
// @group i18nMessages
// @visibility external
//<
removeFormulaFieldText: "Remove formula",


_editComputedField : function (field, builderType) {
    // return if FormulaBuilder isn't available
    if (isc.FormulaBuilder == null) return;

    var component = this,
        editMode = !field ? false : true,
        lowercaseBuilderType = builderType.toLowerCase();

    if (isc.isA.String(field)) {
        field = this.getField(field);
    }

    var builder = isc[builderType + "Builder"].create({ 
        autoDraw: false,
        overflow: "visible",
        component: component, dataSource: component.getDataSource(), 
        editMode: editMode, field: field,
        mathFunctions: builderType == "Formula" ? 
            isc.MathFunction.getDefaultFunctionNames() : null,
        headerSpans: this.headerSpans,
        showHeaderSpanTitles: this.showHeaderSpanTitlesInFormulaBuilder,
        spanTitleSeparator: this.formulaBuilderSpanTitleSeparator,
        fieldKeyProperties: {
            // star height ensures the grid expands vertically to fill all available
            // space if window is resized, even if there are no more records to show.
            // This looks better than having a bunch of blank space at bottom of window
            height: "*"
        },
        fireOnClose: function(){
            component.userFieldCallback(this);
        }
    }, this[lowercaseBuilderType + "BuilderProperties"]);


    this.fieldEditorWindow = this.createAutoChild("fieldEditorWindow", isc.addProperties({}, {
            title: this.fieldEditorWindowTitle.evalDynamicString(this, 
                { builderType: builderType, fieldTitle: builder.field.title }),
            items: [builder]
        }, this[lowercaseBuilderType + "EditorProperties"])
    );

    this.fieldEditorWindow.show();
},

//> @method dataBoundComponent.editFormulaField
// Method to display a +link{FormulaBuilder} to edit a formula Field.  If the function is called
// without a parameter, a new field will be created when the formula is saved.
//
// @param	field	   (Field)	Field to edit or null to add a new formula field
// @group formulaFields
// @visibility external
//<
editFormulaField : function (field) {
    return this._editComputedField(field, "Formula");
},

//> method dataBoundComponent.invalidateUserCache
// Marks the dataBoundComponent as having no cached values,
// effectively clearing them with respect to any client code
// (or removes some of the cached values if called with parameters).
// @param records (Array of Record) records whose cache should be cleared
// @param fields  (Array of Field)  fields whose cache should be cleared
//<

_cacheOrdinal: 0,
invalidateUserCache : function (records, fields) {

    // handle the O(1) "clear everything" operation, as well as cases where
    // the caller has not supplied any records, but wants certain fields targeted
    if (!records) {
        if (!fields) { 
            this._cacheOrdinal++; 
            return; 
        }
        records = this.data || [];
        if (isc.ResultSet && isc.isA.ResultSet(records)) records = records.getAllLoadedRows();
        if (isc.Tree      && isc.isA.Tree     (records)) records = records.getAllItems();
    }
    if (!records) return;

    // promote single record to an array of records
    if (!isc.isAn.Array(records)) records = [records];

    if (fields != null) {
        // promote single field to an array of fields
        if (!isc.isAn.Array(fields)) fields = [fields];
        // remove non-user formula/summary fields
        fields = this.getCacheableFields(fields);
    }

    // clear the requested cache bits based on supplied records/field names
    for (var i = 0; i < records.length; i++) {
        if (!records[i]) continue;

        var cache = records[i]["_cache_" + this.ID];
        if (!cache) continue;
        
        if (fields == null) delete records[i]["_cache_" + this.ID]
        else {
            for (var j = 0; j < fields.length; j++) {
                delete cache[fields[j].name];
            }
        }
    }
},


_addDependentUserFields : function (fields) {
    var result = {},
        dependencies = this._getFieldDependencyTable();

    for (var i = 0; i < fields.length; i++) {
        var dependentFields = dependencies[fields[i].name];
        if (dependentFields) isc.addProperties(result, dependentFields);
        result[fields[i].name] = fields[i];
    }
    return isc.getValues(result);
},
_getUserFieldInputFields : function (field) {
    var fields;
    if (field.userFormula != null) {
        fields = field.userFormula.formulaVars;
    } else if (field.userSummary != null) {
        fields = field.userSummary.summaryVars;
    }
    if (!fields) return {};
    else fields = isc.getValues(fields);

    var result = {};
    for (var i = 0; i < fields.length; i++) {
        var inputField = this.getField(fields[i]);
        if (inputField) {
            result[inputField.name] = inputField;
            isc.addProperties(result, this._getUserFieldInputFields(inputField));
        }
    }
    return result;
},
_getFieldDependencyTable : function () {
    
    if (this._fieldDependencyTable == null) {
        var dependencyTable = this._fieldDependencyTable = {};

        var fields = this.fields;
        for (var i = 0; i < fields.length; i++) {
            var inputFieldNames = isc.getKeys(this._getUserFieldInputFields(fields[i]));
            for (var j = 0; j < inputFieldNames.length; j++) {
                var fieldName = inputFieldNames[j];
                if (dependencyTable[fieldName] == null) {
                    dependencyTable[fieldName] = {};
                }
                dependencyTable[fieldName][fields[i].name] = fields[i];
            }
        }
    }
    return this._fieldDependencyTable;
},
_clearFieldDependencyTable : function () {
    delete this._fieldDependencyTable;
},
        
// provide consistent map between variables and fields across component
rebuildAllFieldsFormulaVarMaps : function () {
    var fields = this.getAllFields(),
        varToFieldName = {},
        fieldNameToVar = {};

    if (fields == null) return;

    // the first binding of a field to a variable in the field list wins
    for (var i = 0; i < fields.getLength(); i++) {
        var item = fields.get(i);
        if (item.userFormula) {
            var formula = item.userFormula,
                formulaVars = formula.formulaVars,
                keys = isc.getKeys(formulaVars);
            for (var j = 0; j < keys.length; j++) {
                var formulaVar = keys[j],
                    fieldName = formulaVars[formulaVar];
                if (!fieldNameToVar[fieldName]) {
                    fieldNameToVar[fieldName] = formulaVar;
                    varToFieldName[formulaVar] = fieldName;
                }
            }
        }
    }

    // store both variable => fieldName and fieldName => variable maps
    this._allFieldsFormulaVarMaps = {varToFieldName: varToFieldName,
                                     fieldNameToVar: fieldNameToVar};
},

// define versions of these APIs for +link{DataBoundComponent} to simplify the logic;
// they are meaningful mostly in +link{ListGrid} where summary records are possible
shouldApplyUserFormulaAfterSummary : function (field) {
    return field && field.userFormula != null;
},
shouldShowUserFormula : function (field, record) {
    return true;
},

// provide an API to return all fields that we are capable of caching
getCacheableFields : function (fields) {
    var fields = fields != null ? fields : this.getFields();
    if (fields == null) return []; // allow Array.getProperty()
    return fields.filter(function (field) { 
        return field.userFormula != null || field.userSummary != null; 
    });
},

//> @method dataBoundComponent.getFormulaFieldValue()
// Get the computed value of a +link{canAddFormulaFields,formula field}.
// @param field (Field) field that has a formula
// @param record (Record) record to use to compute formula value
// @return (Double or String) formula result if a valid number or
// +link{dataBoundComponent.badFormulaResultValue} if invalid
// @visibility external
//<
getFormulaFieldValue : function (field, record, formulaFunction) {
    if (!isc.isAn.Object(field)) field = this.getField(field);

    if (record && record["_cache_" + this.ID] && field && field.name &&
        record["_cache_" + this.ID][field.name] == this._cacheOrdinal)
    {
        return record[field.name];
    }
    
    if (formulaFunction == null) formulaFunction = this.getFormulaFunction(field);
    if (!formulaFunction) return null;

    var result = formulaFunction(record, this);

    if (record && !record._noCache) {
        if (!record["_cache_" + this.ID]) record["_cache_" + this.ID] = {};
        record["_cache_" + this.ID][field.name] = this._cacheOrdinal;
        record[field.name] = result;
    }

    return result;
},

// for a field with a userFormula, get the function that will generate formula output for a
// record
getFormulaFunction : function (field) {
    if (!field || !field.userFormula) return null;
    var func = field._generatedFormulaFunc;
    if (func != null && func._userFormula == field.userFormula) return func;
    // first use of formula field - generate the formula function and install as sortNormalizer
    // too 
    func = field._generatedFormulaFunc =
            isc.FormulaBuilder.generateFunction(field.userFormula, this.getAllFields(), this);
    func._userFormula = field.userFormula;

    

    return func;
},

//> @attr dataBoundComponent.canAddSummaryFields (boolean : false : IRW)
// Adds an item to the header context menu allowing users to launch a dialog to define a new
// text field that can contain both user-defined text and the formatted values present in other 
// fields, using the +link{SummaryBuilder}.
// <P>
// User-added summary fields can be persisted via +link{listGrid.getFieldState()} and 
// +link{listGrid.setFieldState()}.
// 
// @group summaryFields
// @visibility external
//<
canAddSummaryFields:false,

//> @attr dataBoundComponent.addSummaryFieldText (String : "Add summary column..." : IRW)
// Text for a menu item allowing users to add a formula field
//
// @group i18nMessages
// @visibility external
//<
addSummaryFieldText: "Add summary column...",

//> @method dataBoundComponent.addSummaryField
// Convenience method to display a +link{SummaryBuilder} to create a new Summary Field.  This 
// is equivalent to calling +link{dataBoundComponent.editSummaryField, editSummaryField()} with 
// no parameter.
//
// @group summaryFields
// @visibility external
//<
addSummaryField : function () {
    this.editSummaryField();
},

//> @attr dataBoundComponent.editSummaryFieldText (String : "Edit summary format..." : IRW)
// Text for a menu item allowing users to edit the formatter for a field
//
// @group i18nMessages
// @visibility external
//<
editSummaryFieldText: "Edit summary format...",

//> @attr dataBoundComponent.removeSummaryFieldText (String: "Remove summary format..." : IRW)
// Text for a menu item allowing users to remove a summary field
//
// @group i18nMessages
// @visibility external
//<
removeSummaryFieldText: "Remove summary column..",

//> @method dataBoundComponent.editSummaryField
// Method to display a +link{SummaryBuilder} to edit a Summary Field.  If the function is called
// without a parameter, a new field will be created when the summary is saved.
//
// @param	field	   (Field)	Field to edit or null to add a new summary column
// @group summaryFields
// @visibility external
//<
editSummaryField : function (field) {
    return this._editComputedField(field, "Summary");
},

// after a FormulaBuilder or SummaryBuilder completes, add the new field (or update the field) 
userFieldCallback : function (builder) {
    if (!builder) return;
    
    var editorWindow = this.fieldEditorWindow;

    if (builder.cancelled) {
        editorWindow.destroy();
        return;
    }

    var field = builder.getUpdatedFieldObject();
    
    // If this is a new field (rather than an edit of an existing Summary / Formula field),
    // base the unique field name on the title if this is easy to do.
    
    var fieldName = field.name;
    if (this.getField(fieldName) == null) {
        var title = field.title;
        if (title != null && !isc.isA.emptyString(title) 
            && title != builder.defaultNewFieldTitle) 
        {
            var namePrefix = title;
            
            namePrefix = namePrefix.replace(/ |\.|-/g, "_");
            if (namePrefix.match(/^[0-9]/)) namePrefix = "_" + namePrefix;
            if (String.isValidID(namePrefix)) {
                field.name = builder.getNewUniqueFieldName(namePrefix);
            } else {
                this.logInfo(
                    "User-created field: unable to create fieldName based on specified title " +
                    title + ", using default name:" + field.name,
                    "summaryField"
                );
            }
        } else {
            this.logDebug(
                "User-created field: not attempting to create fieldName based on specified title " +
                title + ", using default name:" + field.name,
                "summaryField"
            );
        }
        
        var undef, first = builder.getUsedFields(true).first();
        if (field.summaryFunction === undef && first && first.summaryFunction != null) {
            field.summaryFunction = first.summaryFunction;
        }
    }
    
    // Fire a notification method here - this will allow the developer to modify the
    // added field 
    if (this.userAddedField && this.userAddedField(field) == false) {
        editorWindow.destroy();
        return;
    }
    
    if (this.formulaUpdated && builder.builderTypeText == "Formula") {
        this.formulaUpdated(field, field.userFormula);
    }

    if (this.summaryUpdated && builder.builderTypeText == "Summary") {
        this.summaryUpdated(field, field.userSummary);
    }

    if (this.hideField && builder.shouldHideUsedFields()) {
        var usedFields = builder.getUsedFields();
        for (var i = 0; i < usedFields.length; i++) {
            var item = usedFields.get(i);
            if (item.canHide != false) this.hideField(item.name);
        }
    }
 
    
    var allFields = this.getAllFields();

    // if we edited a pre-existing field object (eg modified a pre-existing formula), find
    // and replace that field
    var fieldNum = isc.Class.getArrayItemIndex(field.name, allFields, this.fieldIdProperty);
    if (fieldNum >= 0) allFields[fieldNum] = field;
    // otherwise add as last visible field
    else allFields.addAt(field, this.getFields().length); 

    this.setFields(allFields);

    if (this.markForRedraw) this.markForRedraw();

    var restart = builder.restartBuilder,
        type = builder.builderTypeText;

    editorWindow.destroy();

    if (restart) {
        if (type == "Formula") this.addFormulaField();
        else this.addSummaryField();
    }
    
    // if the grid is grouped on this formula or summary field, regroup() it now
    if (this.isGrouped && this.getGroupByFields && this.getGroupByFields().contains(field.name)) {
        this.regroup();
    }
    
    // if the DBC supports resorting, do that now 
    if (this.resort) this.resort();
},

// for a field with a userSummary, get the function that will generate summary output for a
// record
getSummaryFunction : function (field) {
    if (!field || !field.userSummary) return null;
    var func = field._generatedSummaryFunc;
    if (func != null) return func;
    // first use of summary field - generate the summary function and install as sortNormalizer
    // too 
    func = field._generatedSummaryFunc =
            isc.SummaryBuilder.generateFunction(field.userSummary, this.getAllFields(), this)
    ;
    

    return func;
},

//> @method dataBoundComponent.getSummaryFieldValue()
// Get the computed value of a +link{canAddSummaryFields,summary field}.
// @param field (Field) field that has a summary format
// @param record (Record) record to use to compute formula value
// @return (String) formula result
// @visibility external
//<
getSummaryFieldValue : function (field, record, summaryFunction) {
    if (!isc.isAn.Object(field)) field = this.getField(field);

    if (record && record["_cache_" + this.ID] && field && field.name &&
        record["_cache_" + this.ID][field.name] == this._cacheOrdinal) 
    {
        return record[field.name];
    }

    if (summaryFunction == null) summaryFunction = this.getSummaryFunction(field);
    if (!summaryFunction) return null;

    var result = summaryFunction(record, field[this.fieldIdProperty], this);

    if (record) {
        if (!record["_cache_" + this.ID]) record["_cache_" + this.ID] = {};
        record["_cache_" + this.ID][field.name] = this._cacheOrdinal;
        record[field.name] = result;
    }

    return result;
},

//> @method dataBoundComponent.shouldIncludeHiliteInSummaryField()
// When assembling a value for a +link{canAddSummaryFields,summary field}, if a referenced
// field is hilited, should the hilite HTML be included in the summary field value?
// <P>
// Example use case: Consider a grid containing a numeric field, and a summary field
// which contains some string value, plus the contents of the numeric field.
// If a hilite is defined for the grid which turns the numeric field text red when
// the value is negative, this property will govern whether the number will also be
// rendered in red within the summary field cells. Any other text in the summary field
// cells would not be effected by this hilite.
// <P>
// Default implementation returns +link{dataBoundComponent.includeHilitesInSummaryFields,
// includeHilitesInSummaryFields}.
// <P>
// To control hilites showing in group summaries, see +link{listGrid.showHilitesInGroupSummary,
// showHilitesInGroupSummary}.
//
// @param summaryFieldName (string) name of the summary field
// @param usedFieldName (string) name of the field referenced by this summary
// @return (boolean) Return true to include hilites from the used field in the generated
//   summary field value.
// @visibility external
//<
shouldIncludeHiliteInSummaryField : function (summaryFieldName, usedFieldName) {
    return this.includeHilitesInSummaryFields
},

//> @attr dataBoundComponent.includeHilitesInSummaryFields (boolean : true : IRWA)
// When assembling a value for a +link{canAddSummaryFields,summary field}, if a referenced
// field is hilited, should the hilite HTML be included in the summary field value?
// <P>
// To control hilites showing in group summaries, see +link{listGrid.showHilitesInGroupSummary,
// showHilitesInGroupSummary}.
//
// @see shouldIncludeHiliteInSummaryField
// @visibility external
//<
includeHilitesInSummaryFields: true,


_setUserField : function (field, property, value, redrawFunc) {
    var undef;
    if (isc.isA.String(field)) field = this.getField(field);
    if (value !== undef) field[property] = value;
    this.invalidateUserCache(null, field);
    if (property == "userFormula") this.rebuildAllFieldsFormulaVarMaps();
    if (redrawFunc) redrawFunc(field);
},
_setUserFieldText : function (field, property, text, redrawFunc) {
    var undef;
    if (isc.isA.String(field)) field = this.getField(field);

    var value = field[property];
    if (value == null) {
        this.logWarn("Cannot set text for " + property + " of field '" + field.name +
                     "' since that user field is null");
        return;
    }
    if (text !== undef) value.text = text;
    this.invalidateUserCache(null, field);
    switch (property) { // ensure new function is generated
        case "userFormula": delete field._generatedFormulaFunc; break;
        case "userSummary": delete field._generatedSummaryFunc; break;
    }
    if (redrawFunc) redrawFunc(field);
},

//> @method dataBoundComponent.getRecordIndex()
// Get the index of the provided record.
// <P>
// Override in subclasses to provide more specific behavior, for instance, when data holds a
// large number of records
//
// @param record (Record) the record whose index is to be retrieved
// @return index (Number) index of the record, or -1 if not found
// @visibility external
//<
getRecordIndex : function (record) {
    return this.data.indexOf(record);
},

//> @method dataBoundComponent.getTitleFieldValue()
// Get the value of the titleField for the passed record
// <P>
// Override in subclasses 
//
// @param record (Record) the record whose index is to be retrieved
// @return value (String) the value of the titleField for the passed record
// @visibility external
//<
getTitleFieldValue : function (record) {},



//> @attr dataBoundComponent.titleField (string : null : IR)
// Best field to use for a user-visible title for an individual record from this
// component.
// <P>
// This attribute has the same function as +link{DataSource.iconField} but can be
// set for a component with no dataSource, or can be used to override the dataSource setting.
//
// @visibility external
//<

//> @attr dataBoundComponent.iconField (string : null : IR)
// Designates a field of +link{FieldType,type}:"image" as the field to use when rendering a
// record as an image, for example, in a +link{TileGrid}. 
// <P>
// This attribute has the same function as +link{DataSource.iconField} but can be
// set for a component with no dataSource, or can be used to override the dataSource setting.
// 
// @visibility external
//<

//> @attr dataBoundComponent.infoField (String : null : IR)
// Name of the field that has the second most pertinent piece of textual information in the
// record, for use when a +link{DataBoundComponent} needs to show a short summary of a record.
// <P>
// This attribute has the same function as +link{DataSource.infoField} but can be
// set for a component with no dataSource, or can be used to override the dataSource setting.
//
// @visibility external
//<


//> @attr dataBoundComponent.dataField (String : null : IR)
// Name of the field that has the most pertinent numeric, date, or enum value, for use when a
// +link{DataBoundComponent} needs to show a short summary of a record.
// <P>
// This attribute has the same function as +link{DataSource.dataField} but can be
// set for a component with no dataSource, or can be used to override the dataSource setting.
//
// @visibility external
//<

//> @attr dataBoundComponent.descriptionField (String : null : IR)
// Name of the field that has a long description of the record, or has the primary text data
// value for a record that represents an email message, SMS, log or similar.
// <P>
// This attribute has the same function as +link{DataSource.descriptionField} but can be
// set for a component with no dataSource, or can be used to override the dataSource setting.
//
// @visibility external
//<




//> @method dataBoundComponent.getTitleField()
// Method to return the fieldName which represents the "title" for records in this
// Component.<br>
// If this.titleField is explicitly specified it will always be used.
// Otherwise, default implementation will check +link{dataSource.titleField} for databound
// compounds.<br>
// For non databound components returns the first defined field name of <code>"title"</code>,
// <code>"name"</code>, or <code>"id"</code> where the field is visible. If we don't find any
// field-names that match these titles, the first field in the component will be used instead.
// @return (string) fieldName for title field for this component.
// @visibility external
//<
getTitleField : function () {
    if (this.titleField != null) return this.titleField;

    if (this.dataSource != null) {
        var field = this.getDataSource().getTitleField(),
            fieldDef = this.getField(field);
        if (!fieldDef || fieldDef.excludeFromState) {
            var fields = this.getFields(),
                foundVisibleField = false;
            for (var i = 0, numFields = fields.length; !foundVisibleField && i < numFields; ++i) {
                fieldDef = fields[i];
                if (!fieldDef.excludeFromState) {
                    // Save the first field not excluded from state.
                    if (field == null) field = fieldDef[this.fieldIdProperty];

                    if (this.fieldIsVisible(fieldDef) && this.shouldUseField(fieldDef)) {
                        field = fieldDef[this.fieldIdProperty];
                        foundVisibleField = true;
                    }
                }
            }
            if (field == null) {
                field = fields[0][this.fieldIdProperty];
            }
        }

        this.titleField = field;
    } else {
        // if a title field hasn't been explicitly specified, take a guess.
        // Also, remember the guess (this is an inner loop)
        var fields = this.getFields(),
            guesses = ["title", "label", "name", "id"],
            foundVisibleField = false;

        for (var i = 0; !foundVisibleField && i < guesses.length; ++i) {
            var guess = guesses[i],
                field = fields.find(this.fieldIdProperty, guess);
            if (field && this.fieldIsVisible(field) && this.shouldUseField(field) &&
                !field.excludeFromState)
            {
                this.titleField = guess;
                foundVisibleField = true;
            }
        }
        if (!foundVisibleField) {
            this.titleField = fields.first()[this.fieldIdProperty];

            // Search for the first field not excluded from state to use as the default.
            for (var i = 0; i < fields.length; ++i) {
                var field = fields[i];
                if (!field.excludeFromState) {
                    this.titleField = field[this.fieldIdProperty];
                    break;
                }
            }
        }
   }
   return this.titleField;
},

//> @method dataBoundComponent.getRecordHiliteCSSText()
// Return all CSS style declarations associated with the hilites of a record's field.
// @param record (Record)
// @param cssText (String) if set, returned CSS will be appended to this text
// @param field (Field) field object identifying whose CSS is to be returned
// @return value (String) CSS style declarations for this record and field
// @visibility external
//<
getRecordHiliteCSSText : function (record, cssText, field, visibleRecord) {
    if (record == null) return cssText;
    if (record.isGroupSummary && (!this.showHilitesInGroupSummary || field.showHilitesInGroupSummary == false)) return cssText; 

    // addObjectHilites() will check for explicit record[this.hiliteProperty], find
    // the associated hilite and return the cssText for it (added to the cssText passed in)
    // The 'field' param allows that code to skip hilites that are specified for some
    // other field.
    cssText = this.addObjectHilites(record, cssText, field);
    
    
    if (visibleRecord == null) {
        visibleRecord = 
            this.getFields().contains(field) && 
                ((isc.isA.List(this.data) && this.getRecordIndex(record) != -1) 
                 || this.data == record);
    }
    // addHiliteCSSText() picks up cssText for hilites that apply to the cell due
    // to criteria/fieldName
    // Only works for records/fields that are present in the component's data/fields array
    
    if (visibleRecord) {
        cssText = this.addHiliteCSSText(record, field, cssText);
        
    // Handle the case where the record isn't part of data or the field isn't
    // one of the fields in the component by explicitly running the filter logic and
    // calculating hilite cssText.
    } else {
        var hilites = this.hilites;
        if (hilites) {
            for (var i = 0; i < hilites.length; i++) {
                if (field && field.name) {
                    var hilitedField = hilites[i].fieldName;
                    if (hilitedField && 
                        (isc.isAn.Array(hilitedField) ? !hilitedField.contains(field.name) 
                                                      : hilitedField != field.name))
                    {
                        continue;
                    }
                }
                var matches = this.getRecordsMatchingHilite(hilites[i], [record]);
                if (matches && matches.length != 0) {
                    if (cssText == null) {
                        cssText = this.getHiliteCSSText(hilites[i]);
                    } else {
                        cssText += this.getHiliteCSSText(hilites[i]);
                    }
                }
            }
        }
    }
    if (this.logIsDebugEnabled("hiliting") && cssText != null) {
        this.logDebug("getRecordHiliteCSSText for field:" + field.name + 
            " on record:" + this.echo(record) 
            + "\n- gives back value: " + cssText, "hiliting");
    }
    return cssText;
},

//> @method dataBoundComponent.convertCSSToProperties()
// Convert a string containing CSS declarations into an object mapping CSS
// camelCaps property names with the declared values.
// @param css (string) Block of CSS style text
// @param allowedProperties (Array) optional array of CSS property names (camelCaps format)
//        constraining the allowed properties to be returned
// @return value (Object) CSS property-value pairs in camelCaps notation,
//         or null if no CSS was found
//<
convertCSSToProperties : function (css, allowedProperties) {
    if (css == null) return null;

    var statementList = css.split(";"),  // split into [name, value] pairs
        propertyList;
        
    statementList.map(function (decl) {
        var pair = decl.split(":");          // [ name, value ]
        if (pair.length != 2) return null;
        
        // Convert name to camelCaps. Trim whitespace from both name and value.
        var trimRe = /^\s*(\S*)\s*$/,
            name  = pair[0].cssToCamelCaps().replace(trimRe, "$1"),
            value = pair[1]                 .replace(trimRe, "$1");
        
        if (!allowedProperties || allowedProperties.contains(name)) {
            if (!propertyList) propertyList = {};
            propertyList[name] = value;
        }
    });
    
    return propertyList;
},
// Overridable method to return the exportable value of a record's field. 
// By default, the display value is returned (via getStandaloneFieldValue),
// stripped of HTML tags.
getExportFieldValue : function (record, fieldName, fieldIndex) {
    return this.htmlUnescapeExportFieldValue(
        this.getStandaloneFieldValue(record, fieldName, false));
},

// Overridable method to store the exportable value of a record's field, including
// its style information, in exportObject[exportProp]. If the field is unstyled then
// exportObject is not modified. The exportable value is in one of two formats, depending
// on if the style information applies to the entire cell, or a part of the cell (eg
// if cell used in a summary has hiliting applied to it):
//
// * Cell-wide style: { backgroundColor: "#f00000" }
//
// * Sub-cell style:
//   [
//     { value: "1",
//       style: { backgroundColor: "#f00000" }
//     },
//     { value: " --- baz" }
//   ]
addDetailedExportFieldValue : function(exportObject, exportProp, record, exportField, 
                                       exportFieldIndex, settings, rowIndex)
{
	var allowedProperties            = settings.allowedProperties,
        propagateInputHilites        = settings.propagateInputHilites,
        alwaysExportExpandedStyles   = settings.alwaysExportExpandedStyles,
        exportDatesAsFormattedString = settings.exportDatesAsFormattedString;

    var exportFieldName = exportField.name,
        exportFieldCSS = this.getRecordHiliteCSSText(record, null, exportField),
        simpleValue,
        formatProperties = {};
 
    // Inject background color explicitly defined for cell, if any
    
    var backgroundColor = this.getExportBGColor(rowIndex, exportFieldIndex, record);
	if (rowIndex != null && backgroundColor) {
	   exportFieldCSS += "; backgroundColor: " + backgroundColor; 	
	}
    
    var declarativeFormat = this.getDeclarativeFormat(exportField);
    if (declarativeFormat) {
        formatProperties.rawValue = record[exportField.name];
        formatProperties.format = declarativeFormat;
    } else if (isc.isA.Date(record[exportField.name]) && !exportDatesAsFormattedString) {
        formatProperties = this.getDateFormattingProperties(exportField, record[exportField.name],
                                                                exportObject[exportField.title]);
    }
    
    // server-side performs conversion of "strings that look like numbers" if unformatted numeric 
    // values are exported through POI (Excel), which may lead to rawValue (ID) replacing the 
    // displayValue (VALUE) in case of valueMap set on a field. Docs say, that exportClientData 
    // will always export displayValue for fields with valueMap despite any other settings.
    // This "hasValueMap" flag is preventing these replacements (search ExcelDataExport.java).
    // This code is executed only if exportClientData was called, so other exports are untouched.
    if (exportField.valueMap) {
        formatProperties.hasValueMap = true;
    }
    
    if (isc.isA.Number(record[exportField.name])) {
        formatProperties.rawValue = record[exportField.name];
    }
    
    if (formatProperties && isc.getKeys(formatProperties).length == 0) {
        formatProperties = null;
    }

    if (exportField.exportRawValues || (this.exportRawValues && exportField.exportRawValues != false))
        simpleValue = record[exportField[this.fieldIdProperty]];
    else
        simpleValue = this.getExportFieldValue(record, exportField.name, exportFieldIndex);

    if (!exportField.userSummary || !propagateInputHilites) {
        if (exportFieldCSS || formatProperties) {
            var props = this.convertCSSToProperties(exportFieldCSS, allowedProperties);
            if (formatProperties) {
                if (!props) props = {};
                isc.addProperties(props, formatProperties);
            }
            if (props) {
                if (alwaysExportExpandedStyles)
                    exportObject[exportProp] = [{value: simpleValue, style: props }];
                else
                    exportObject[exportProp] = props;
            }
        }
        return;
    }

    if (!exportField.userSummary.text) this.logError("Summary field does not have text format");
    
    // Code below generally adapted from SummaryBuilder.getFieldDetailsFromValue, generateFunction
    var missingFields = [], usedFields = {}, usedFieldsCSS = {};
    var cssFound = (exportFieldCSS && exportFieldCSS != "");
        
    // compile lists of used and missing fields and save off used field CSS for later
    for (var key in exportField.userSummary.summaryVars) {
        var varFieldName = exportField.userSummary.summaryVars[key],
            varField = this.getField(varFieldName);
        if (!varField) missingFields.add(varFieldName);
        else {
            usedFields[key] = varField;
            
            var varCSS = this.getRecordHiliteCSSText(record, null, varField);
            if (varCSS) {
                usedFieldsCSS[key] = varCSS;
                cssFound=true;
            }
        }
    }
    
    // if there's no style info, there's no need for a $style entry.
    if (!cssFound) return;
    
    // missing fields fail the method and probably ought to be styled
    if (missingFields.length != 0 && exportFieldCSS) {
        if (alwaysExportExpandedStyles) {
            exportObject[exportProp] = {
                style: this.convertCSSToProperties(exportFieldCSS, allowedProperties),
                value: simpleValue
            };
        } else {
            exportObject[exportProp] = this.convertCSSToProperties(
                exportFieldCSS, allowedProperties);
        }
        return;
    }
    
    // substrings of summary value are stored in currentFragment along with its associated
    // CSS in currentCSS, before they are combined into a single object and appended to output
    // array detailedValues. Consecutive fragments with equal css strings are merged.
    var currentFragment = null, currentCSS = null, detailedValue = [];
    
    // addToOutput(): helper function for outputting value/css pairs. 
    var _this=this;
    var addToOutput = function (value, css) {
        if (value) {
            value = _this.htmlUnescapeExportFieldValue(value);
            
            if (currentFragment && currentCSS == css) {
                currentFragment.value += value; // merge if styles are equal
            } else {
                // add current fragment to output array and create new fragment
                if (currentFragment) detailedValue.push(currentFragment);

                currentFragment = {value: value};
                currentCSS = css;
                if (css) currentFragment.style = _this.convertCSSToProperties(
                    css, allowedProperties);
            }
        }
    };

    // Split summary format on formula alias prefix "#" and consider each substring a
    // potential formula alias. The "#X" alias form is attempted first then "#{ABC}".
    var splitFmt = exportField.userSummary.text.split("#"),
        braceRegexp = /^\{([A-Z]+)\}/;
    
    // If format started with literal text, add it to output
    if (splitFmt[0]) addToOutput(splitFmt[0], exportFieldCSS);
    for (var i=1; i<splitFmt.length; i++) {
        var fragment = splitFmt[i], braceRegexpMatch, matchField, matchKey, fieldValue, 
            fieldCSS, textAfterField;
            
        matchKey = fragment.charAt(0);
        matchField = usedFields[matchKey];
        
        if (matchField) textAfterField = fragment.substr(1); // #X
        else if (braceRegexpMatch = fragment.match(braceRegexp)) {
            textAfterField = fragment.substr(braceRegexpMatch[0].length); // #{XXX}
            matchKey = braceRegexpMatch[1];
            matchField = usedFields[matchKey];
            
            // always assume #{..} is meant to be an alias, so fail this out
            if (!matchField) textAfterField = this.missingSummaryFieldValue + textAfterField;
        } else textAfterField = "#" + fragment; // possibly not an alias
        
        // If a field matched, get its value and style; merge style with summary-wide
        // style as appropriate
        if (matchField) {
            fieldValue = this.getExportFieldValue(record, matchField.name, 
                this.getFieldNum(matchField.name));
            fieldCSS=null;
            if (exportFieldCSS) fieldCSS = (fieldCSS||"") + exportFieldCSS;
            if (usedFieldsCSS[matchKey]) fieldCSS = (fieldCSS||"") + usedFieldsCSS[matchKey];
        }
        // add possible fragments for formula alias and the literal text following it
        addToOutput(fieldValue, fieldCSS);
        addToOutput(textAfterField, exportFieldCSS);
    }
    // Above loop leaves last fragment not added to output: add it now
    if (currentFragment) detailedValue.push(currentFragment);
    
    exportObject[exportProp] = detailedValue;
},

getDeclarativeFormat : function(field) {
    return !field ? null : field.exportFormat || field.format;
},


//> @method dataBoundComponent.getClientExportData()
// Export visual description of component data into a JSON form suitable for export.
// @param settings (Object) contains configuration settings for the export, including:<br/>
//        includeHiddenFields (Boolean) - controls if hidden fields should be exported<br/>
//        allowedProperties (Array) optional array of CSS property names (camelCaps format)
//             constraining the allowed properties to be returned<br/>
//        includeCollapsedNodes (Boolean) - if true, when exporting a TreeGrid, include tree
//             nodes underneath collapsed folders in the export output<br/>
//        propagateInputHilites - controls whether to propagate hilites defined on inputs
//             of user summaries to the summaries themselves (unset means don't propagate)
// @param callback (Callback) callback to fire when data is ready
// @return exportData (Object) exported data
//<
// * Data is exported as an array of objects, with one object per record (visual row) 
//   of the grid.
// * The name of each exported field of the component is mapped to a property
//   of a record's object. Correspondingly, the value of each exported field in a record is
//   mapped to each value of a record's object.
// * If CSS hiliting styles are present on a field, style information is stored in property 
//   "<field name>$style". This contains an array of objects. Each object has a
//   'value' property containing a fragment or substring of the field value. If that
//   value fragment is styled, the CSS text is converted into an object mapping CSS
//   properties in camelCaps format to CSS values, and the object is stored in the 'style'
//   property.
// * Null record values are converted to empty strings.
//
// For instance, suppose a record has a field "Foo_Fighter" equal to 1 with a
// backgroundColor set through hiliting, a field "bar" set to "baz", a field
// "xyzzy" set to null, and a summary field with the format "#A -- #B", with
// #A referring to "Foo_Fighter" and #B referring to "bar". The return value would be:
//
// [
//     { 
//         Foo_Fighter: "1",
//         Foo_Fighter$style: 
//         [
//             { 
//                 value: "1",
//                 style: 
//                 { 
//                     backgroundColor: "#f00000" 
//                 }
//             }
//         ],
//         bar: "baz",
//         xyzzy: "",
//         summaryField1: "1 --- baz",
//         summaryField1$style: 
//         [
//             {
//                 value: "1",
//                 style: 
//                 { 
//                     backgroundColor: "#f00000"
//                 }
//             },
//             {
//                 value: " --- baz"
//             }
//         ]
//     }, /* other records... */
// ]
exportDataChunkSize: 50,
getClientExportData : function (settings, callback) {
    var data = this.originalData || this.data,
        exportData = [],
        fields = this.getClientExportFields(settings),
        includeHiddenFields,
        allowedProperties,
        includeCollapsedNodes,
        alwaysExportExpandedStyles,
        exportFieldsSpecified = settings && settings.exportFields
    ;

    if (settings == null) settings = {};
    
    if (settings.exportData != null) data = settings.exportData;
    
    includeHiddenFields = settings.includeHiddenFields;
    allowedProperties = settings.allowedProperties;
    includeCollapsedNodes = settings.includeCollapsedNodes;
    alwaysExportExpandedStyles = settings.alwaysExportExpandedStyles;
    // support export fields as per server-side export
    if (exportFieldsSpecified) {
        // when exportFields is specified and unless includeHiddenFields is explicitly set to
        // false, assume that the user actually wants to see the fields that he provided via
        // exportFields.
        if (includeHiddenFields !== false) includeHiddenFields = true;
    }

    
    if (isc.isA.ResultSet(data)) data = data.getAllLoadedRows();
    if (isc.isA.Tree(data)) {
        if (includeCollapsedNodes) data = data.getAllNodes();
        else data = data.getOpenList();
    }

    var context = {
        settings: settings,
        callback: callback,
        chunkSize: this.exportDataChunkSize,
        data: data,
        exportData: exportData,
        fields: fields,
        includeHiddenFields: includeHiddenFields,
        allowedProperties: allowedProperties,
        includeCollapsedNodes: includeCollapsedNodes,
        alwaysExportExpandedStyles: alwaysExportExpandedStyles,
        totalRows: data.getLength(),
        startRow: 0,
        endRow: Math.min(this.exportDataChunkSize, data.getLength()),
        exportFieldsSpecified: exportFieldsSpecified
    };
    
    settings.exportRowBGColors = {};
    settings.exportColumnBGColors = {};
    
    context.firstTimeStamp = context.thisTimeStamp = isc.timeStamp();

    this.logInfo("starting export chunking process - "+context.firstTimeStamp, "export");
    this.getClientExportDataChunk(context);

    return;
},

getClientExportDataChunk : function (context) {
    var settings = context.settings,
        data = context.data,
        exportData = context.exportData,
        fields = context.fields,
        includeHiddenFields = context.includeHiddenFields,
        includeCollapsedNodes = context.includeCollapsedNodes,
        totalRows = context.totalRows,
        startRow = context.startRow,
        endRow = context.endRow,
        exportValueFields = settings.exportValueFields,
        exportFieldsSpecified = context.exportFieldsSpecified,
        exportRowBGColors = settings.exportRowBGColors,
        exportColumnBGColors = settings.exportColumnBGColors
    ;
    
    // Generate a separate object for each row of data
    for (var dataRow = startRow; dataRow < endRow; dataRow++) {
        var record = data[dataRow],
        
            exportObject = this.getRecordExportObject(record, fields, includeHiddenFields,
                includeCollapsedNodes, exportValueFields, exportFieldsSpecified, settings,
                dataRow);
        ;
        
        exportData.push(exportObject);
        
        // Get background color explicitly defined by component for row
        var rowColor = this.getExportRowBGColor(dataRow, record);
        if (rowColor) {
        	exportRowBGColors[dataRow] = rowColor;
        }
        
    }

    // Iterate through all fields, again
    for (var fieldIndex = 0; fieldIndex < fields.length; fieldIndex++) {
        var field = fields[fieldIndex];

        // Skip field if it's hidden, again
        if ((!this.fields.contains(field)) && !includeHiddenFields) continue;
        var fieldNum = this.getFieldNum(field.name);
        
        // Set background colors explicitly defined by component for column
        var columnColor = this.getExportColumnBGColor(fieldNum);
        if (columnColor) {
        	exportColumnBGColors[fieldNum] = columnColor;
        }
    }
    
    if (context.endRow < context.totalRows) {
        context.lastTimeStamp = context.thisTimeStamp;
        context.thisTimeStamp = isc.timeStamp();
        if (this.logIsInfoEnabled("export")) {
            this.logInfo("processed "+context.endRow+" rows - starting next chunk - "+
                ((context.thisTimeStamp-context.lastTimeStamp)/1000), "export");
        }
        // more rows remain - delayCall() this method again to process the next chunk
        context.startRow = context.endRow;
        context.endRow = Math.min(context.startRow + context.chunkSize, context.totalRows);
        return this.delayCall("getClientExportDataChunk", [context], 0);
    }

    if (this.showGridSummary && this.summaryRow && this.exportIncludeSummaries) {
        // append the summaries for this component if it has them
        var summaryRow = this.summaryRow,
            data = this.getGridSummaryData(true)
        ;

        for (var dataRow = 0; dataRow < data.getLength(); dataRow++) {
            var record = data[dataRow],
                exportObject = this.getRecordExportObject(record, fields, includeHiddenFields,
                    includeCollapsedNodes, exportValueFields, exportFieldsSpecified, settings,
                    dataRow);
            ;

            exportData.push(exportObject);
        }
    }
    
    if (context.callback) {
        var data = context.exportData;
        if (this.logIsInfoEnabled("export")) {
            this.logInfo("finished processing "+context.endRow+
                " rows - about to export - "+isc.timestamp(), "export");
        }
        this.fireCallback(context.callback, "data,context", [data,context.settings]);
    }
},

getClientExportFields : function (settings) {
    var fields = this.getAllFields();

    // support export fields as per server-side export
    var newFields = [],
        i,
        fieldsLen = fields.length,
        field;
    if (isc.isAn.Object(settings) && settings.exportFields) {
        for (i = 0; i < fieldsLen; ++i) {
            field = fields[i];
            if (settings.exportFields.contains(field.name)) newFields.add(field);
        }
        fields = newFields;
    } else {
        for (i = 0; i < fieldsLen; ++i) {
            field = fields[i];
            if (this._canExportField(field)) {
                newFields[newFields.length] = field;
            }
        }
        fields = newFields;
    }

    return fields;
},
getRecordExportObject : function (record, fields, includeHiddenFields, includeCollapsedNodes,
                                  exportValueFields, exportFieldsSpecified, settings, rowNum)
{
    var exportObject = {};

    // Iterate through all fields
    for (var fieldIndex = 0; fieldIndex < fields.length; fieldIndex++) {
        var field = fields[fieldIndex];

        // Skip field if it's hidden
        if ((!this.fields.contains(field)) && !includeHiddenFields) continue;
        var fieldNum = this.getFieldNum(field.name),
            exportProp=field.name,
            styleProp=exportProp+"$style",
            value;
            
        if (field.exportRawValues || (this.exportRawValues && field.exportRawValues != false)) 
            value = record[field[this.fieldIdProperty]];
        else 
            value = this.getExportFieldValue(record, field.name, fieldNum);

        //var value = this.getExportFieldValue(record, field.name, fieldNum);
        
        if (value == null || value == "&nbsp;") value = "";
        
        if (!exportFieldsSpecified) {
            if (exportValueFields) {
                if (field.displayField) {
                    var key = field.name;
                    if (key == exportProp) key += "_value";
                    exportObject[key] = record[field.name];
                }
            }
        }

        exportObject[exportProp] = value;
        this.addDetailedExportFieldValue(exportObject, styleProp, record, field, fieldNum,
                                         settings, rowNum);
    }
    return exportObject;
},
htmlUnescapeExportFieldTitle : function (fieldName) {
    return this.htmlUnescapeExportFieldValue(fieldName);
},
htmlUnescapeExportFieldValue : function (value) {
    // convert basic HTML like &nbsp; and <br> into normal text equivalents and escape all
    // other HTML
    if (isc.isA.String(value)) return value.unescapeHTML().replace(/<.*?>/g, isc.emptyString);
    return value;
},
// Takes a formatted value and, if hilites apply to the value, adds hilite styling via adding
// a surround <span> tag with a STYLE attribute.  Otherwise returns the value unchanged.
addHiliteSpan : function(record, field, value) {
    var fieldCss = this.getRecordHiliteCSSText(record, null, field);
    if (fieldCss) return "<span style=\"" + fieldCss + "\">" + value + "</span>";
    else return value;
},

// Get the "raw" value for a record/field.
// (Overridden by ListGrid)
// Used by FormulaBuilder
getRawValue : function (record, field) {
    if (!record || !field) return null;

    // Canvas._getFieldValue() tries the `dataPath` argument, then `field.dataPath`,
    // then `field.name` to find a non-null "dataPath" to use.  Using
    // `field.displayField` as the first argument only if `field.dataPath` is not set
    // means that `field.dataPath` overrides `field.displayField` and the latter overrides
    // `field.name`, when getting a value of a record.
    var dataPath = field.dataPath == null ? field.displayField 
                                          : isc.Canvas._getDataPathFromField(field, this);
        
    return isc.Canvas._getFieldValue(dataPath, field, record, this);
},

// This is overridden by ListGrid / DetailViewer
// Used by formulaBuilder / TableView
// Default implementation just returns the raw value passed in.
getFormattedValue : function (record, fieldName, value) {
    return value;
},

fieldIsVisible : function (field) {
    return true;
},

getSpecifiedField : function (fieldName) {
    return this.getField(fieldName);
},

// Returns the formatted record/field value enclosed in any Hilite HTML.
// used in exportFieldValue dataPath as well as in the TileGrid.
getStandaloneFieldValue : function (record, fieldName, unformatted) {
    var field = this.getSpecifiedField(fieldName),
        value;

    if (!field) return;
    
    if      (field.userFormula) value = this.getFormulaFieldValue(field, record);
    else if (field.userSummary) value = this.getSummaryFieldValue(field, record);
    else {
        if (this._useDisplayFieldValue && this._useDisplayFieldValue(field)) {
            value = record[field.displayField];
        } else {
            value = this.getRawValue(record, fieldName);
        }
        if (!unformatted) value = this.getFormattedValue(record, fieldName, value);
    }
    
    var ret = this.addHiliteSpan(record, field, value);
    return ret;
},

// For client-driven exports (exportClientData()), if a date or datetime field is being
// formatted by a standard built-in formatter, send the raw Date value and the name of the
// built-in formatter to the server, as part of the $style information for the cell, like so:
//    independence:"Fri Jan 01 1238 12:00:00 GMT-0800 (Pacific Standard Time)",
//    independence$style:{
//        rawValue:new Date(-23099011200000),
//        dateFormatter:"toUSShortDate"
//    },
// This allows the server to generate a corresponding XLS/OOXML format for the
// spreadsheet cell, so that formatting is preserved, but Excel knows the value is a date and
// will offer appropriate features. 

getDateFormattingProperties : function (field, value, formattedValue) {
    if (!isc.SimpleType.inheritsFrom(field.type, "date")) return;
    if (!isc.isA.Date(value)) return;
    
    var isDatetime = isc.SimpleType.inheritsFrom(field.type, "datetime");
    
    var dateFormatter;

    if (field.dateFormatter && isc.isA.Function(Date.prototype[field.dateFormatter])) {
        dateFormatter = field.dateFormatter;
    } else if (field.displayFormat && isc.isA.Function(Date.prototype[field.displayFormat])) {
        dateFormatter = field.displayFormat;
    }
    
    // Probably no need to check this because it should have been copied onto the LGF, but 
    // it does no harm
    if (!dateFormatter) {
        var dataSource = this.getDataSource(),
            dsField = dataSource ? dataSource.getField(field.name) : null,
            dsFormat = dsField ? dsField.dateFormatter || dsField.displayFormat : null;
        if (dsFormat && isc.isA.Function(Date.prototype[dsFormat])) {
            dateFormatter = dsFormat;
        }
    }
    
    // Defaults from the DBC
    if (!dateFormatter) {
        var dbcFormat;
        if (this.datetimeFormatter != null && isDatetime) {
            dbcFormat = this.datetimeFormatter;
        } else {
            dbcFormat = this.dateFormatter;
        }
        if (dbcFormat && isc.isA.Function(Date.prototype[dbcFormat])) {
            dateFormatter = dbcFormat;
        }
    }
    
    
    // ListGrid defaults to the default short date or datetime formatter, not the "normal"
    // format
    if (!dateFormatter) {
        var shortFormat = !isDatetime ? Date.prototype._shortFormat 
                                               : Date.prototype._shortDatetimeFormat; 
        if (shortFormat && isc.isA.Function(Date.prototype[shortFormat])) {
            dateFormatter = shortFormat;
        }
    }
    
    var dateProps = {
        rawValue: value,
        dateFormatter: dateFormatter
    };

    return dateProps;
},

//> @groupDef exportBGColor
// Several APIs and settings influence the background color which will be used for spreadsheet
// cells when exporting to Excel/OpenOffice formats using +link{listGrid.exportData()} or
// +link{listGrid.exportClientData()}.  The following APIs are called in the order 
// shown, so <code>hilite.backgroundColor</code> takes precedence over 
// <code>exportDefaultBGColor</code>, for example.
// <ol>
// <li> +link{listGrid.getExportBGColor,getExportBGColor(rowNum, colNum, record)}
// <li> +link{hilite.backgroundColor}
// <li> +link{listGrid.getExportRowBGColor,getExportRowBGColor(rowNum, record)}
// <li> +link{listGrid.getExportColumnBGColor,getExportColumnBGColor(colNum)}
// <li> +link{listGrid.exportAlternateRowBGColor,exportAlternateRowBGColor}
// <li> +link{listGrid.exportDefaultBGColor,exportDefaultBGColor}
// </ol>
// If overriding any of the above methods, return null to allow methods later in the precedence
// order to influence background color.  For example, if you want certain rows to have a
// special background color but also want to show alternating colors per row, override
// getExportRowBGColor and return null for all rows that should just show normal alternating
// colors, and not a special color.
//
// @title Exports &amp; Cell Background Color
// @visibility external
//<

//> @attr listGrid.exportDefaultBGColor (CSSColor : null : IR)
// Default background color to use when exporting data to Excel/OpenOffice format using
// +link{exportData(),exportData()} or 
// +link{exportClientData(),exportClientData()}.
// <p>
// If unset (the default), cells that are not provided a background color by more specific APIs
// will be the default background color used by the spreadsheet program where they are viewed.
// <p>
// See +link{group:exportBGColor} for an overview.
// @group exportBackgroundColor
// @visibility external
//<

//> @attr listGrid.exportAlternateRowBGColor (CSSColor : null : IR)
// When exporting data to Excel/OpenOffice format using +link{exportData(),exportData()} or
// +link{exportClientData(),exportClientData()}, background color to use 
// for even-numbered rows, to create a "banded" or "ledger" effect.  Odd-numbered rows will 
// use the +link{exportDefaultBGColor}.
// <p>
// See +link{group:exportBGColor} for an overview.
// @group exportBackgroundColor
// @visibility external
//<

//> @method listGrid.getExportBGColor()
// When exporting data to Excel/OpenOffice format using +link{exportData(),exportData()} or
// +link{exportClientData(),exportClientData()}, background color to use 
// for the cell at the given rowNum and colNum.
// <p>
// See +link{group:exportBGColor} for an overview.
//
// @param rowNum (int) row number of cell
// @param colNum (int) column number of cell
// @param record (Record) the record object behind the row being exported
// @return (CSSColor) background color to use for the cell, or null to use the default
//                    background color
//
// @group exportBackgroundColor
// @visibility external
//< 
getExportBGColor: function (rowNum, colNum, record) {
	return null;
},

//> @method listGrid.getExportRowBGColor()
// When exporting data to Excel/OpenOffice format using +link{exportData(),exportData()} or
// +link{exportClientData(),exportClientData()}, background color to use 
// for the given rowNum.
// <p>
// See +link{group:exportBGColor} for an overview.
//
// @param rowNum (int) row number
// @param record (Record) the record object behind the row being exported
// @return (CSSColor) background color to use for the row, or null to use the default
//                    background color
//
// @group exportBackgroundColor
// @visibility external
//< 
getExportRowBGColor: function (rowNum, record) {
	return null;
},

//> @method listGrid.getExportColumnBGColor()
// When exporting data to Excel/OpenOffice format using +link{exportData(),exportData()} or
// +link{exportClientData(),exportClientData()}, background color to use 
// for the given colNum.
// <p>
// See +link{group:exportBGColor} for an overview.
//
// @param colNum (int) column number
// @return (CSSColor) background color to use for the column, or null to use the default
//                    background color
//
// @group exportBackgroundColor
// @visibility external
//< 
getExportColumnBGColor: function (colNum) {
	return null;
},

//> @attr listGrid.exportHeaderHeights (boolean : false : IRW)
// When exporting data to Excel/OpenOffice format using +link{exportData()} or
// +link{exportClientData()}, causes the +link{listGrid.headerHeight} and
// +link{headerSpan.height,headerSpan heights} to be applied to the corresponding cells in the
// spreadsheet.
//
// @visibility external
//<

//> @attr listGrid.exportFieldWidths (boolean : false : IRW)
// When exporting data to Excel/OpenOffice format using +link{exportData()} or
// +link{exportClientData()}, whether widths of fields should be replicated
// in the resulting spreadsheet.
// <p>
// Because Excel's unit of measurement for field widths is based on the default system font,
// there is no exact way to translate field widths in pixels to Excel column widths.  The
// +link{exportWidthScale} property can be set to adjust scaling; it's default value errs on
// the side of making Excel's columns slightly wider than the ListGrid field's actual width to
// avoid clipping.
// <p>
// Note that you can switch off width export for individual fields with the 
// +link{listGridField.exportFieldWidth} flag.
//
// @visibility external
//<

//> @attr listGridField.exportFieldWidth (boolean : true : IRW)
// When exporting data to Excel/OpenOffice format using +link{listGrid.exportData()} or
// +link{listGrid.exportClientData()} with +link{listGrid.exportFieldWidths} set,
// set this flag false to cause this field to "opt out" of width export.  Fields that opt out
// in this way have the corresponding spreadsheet column autosized (ie, made just wide enough
// that content is not clipped).
// <p>
// This setting has no effect if <code>listGrid.exportFieldWidths</code> is not set.
//
// @visibility external
//<

//> @attr listGrid.exportWidthScale (float : 0.12 : IRW)
// Scaling factor to translate from ListGrid field widths in pixels to Excel/OpenOffice units
// for field width, which are 1/256th of the width of the widest digit character in the default
// font for the spreadsheet.  See +link{exportFieldWidths} for where this is used.
//
// @visibility external
//< 
exportWidthScale : 0.12,

//> @attr listGrid.exportWrapHeaderTitles (boolean : false : IRW)
// When exporting data to Excel/OpenOffice format using +link{exportData()} or
// +link{exportClientData()}, whether titles in the
// +link{listGrid.header,ListGrid header} and +link{listGrid.headerSpans,headerSpans} should be
// allowed to wrap.
// <p>
// Excel will wrap at the column boundary automatically; for explicit control over wrapping,
// insert "<br>" tags into your titles.
// <p>
// See also +link{exportFieldWidths} for replicating the widths of fields in the exported
// spreadsheet.
//
// @visibility external
//< 

//> @attr listGrid.exportFieldAlignments (boolean : false : IRW)
// When exporting data to Excel/OpenOffice format using +link{exportData()} or
// +link{exportClientData()}, whether field
// +link{listGridField.align,horizontal header alignments} and 
// +link{listGridField.cellAlign,data value alignments} should be replicated in the resulting 
// spreadsheet. 
// <p>
// If this attribute is not set, cells will be assigned a default alignment by the spreadsheet,
// which is typically right-aligned for numeric and date values, and left-aligned for 
// everything else (including dates and numbers that have been exported as strings, as would
// be the case, for example, if +link{dsRequest.exportDatesAsFormattedString} is set)
//
// @visibility external
//< 




//> @method listGrid.exportClientData()
// Exports this component's data with client-side formatters applied, so is suitable for direct
// display to users.  This feature requires the SmartClient server.
// <P>
// Ordinarily, calls to this method go through the static classMethod 
// +link{classMethod:DataSource.exportClientData}.  In this case, no server-side DataSources
// are required.  However, if this component is 
// +link{DataBoundComponent.setDataSource(),databound} and you specify a valid 
// +link{dsRequest.operationId,operationId} in the properties passed to this method, the call
// will go through the instance method +link{dataSource.exportClientData} instead.  As the 
// documentation for that method explains, this allows you more control on the server side.  
// This approach requires both the SmartClient server and server-side DataSource definitions.
// <P>
// If your ListGrid has custom formatters, formatted values will be exported by default, with
// HTML normalized to text where possible.  Since some levels of HTML normalizing aren't 
// possible, this may result in missing or incorrect export values.  In this case, you have 
// two options:<ul>
// <li>Set +link{listGridField.exportRawValues,exportRawValues} on the field.  This will export
//     the raw underlying value of the field; your formatter will not be called</li>
// <li>Have your formatter call +link{listGrid.isExportingClientData(),isExportingClientData()}
//     and perform whatever alternative formatting you require if that method returns true</li>
// </ul>
// <P>
// To export data from this component's dataSource, 
// see +link{dataBoundComponent.exportData, exportData}, which does not include client-side 
// formatters, but <b>does</b> include formatters declared in the <code>.ds.xml</code> file.
// <code>exportData()</code> relies on both the SmartClient server and server-side DataSources.
// @param [requestProperties] (DSRequest Properties) Request properties for the export.
//  Note that specifying +link{DSRequest.exportData,exportData} on the request properties
//  allows the developer to pass in an explicit data set to export.
// @param [callback] (RPCCallback) Optional callback.  If
//  you specify +link{DSRequest.exportToClient,exportToClient}: false in the request
//  properties, this callback will fire after export completes.  Otherwise the callback will
//  fire right before the download request is made to the server.
// @see dataSource.exportClientData
// @visibility external
//<
exportClientData : function (requestProperties, callback) {
    this._exportingClientData = true;
    if (callback) requestProperties.__callback = callback;
    this.getClientExportData(requestProperties, 
        this.getID()+".exportClientDataReply(data,context)");
    return;
},

//>	@method listGrid.isExportingClientData()
// Returns true if this component is currently 
// +link{listGrid.exportClientData(), exporting client data}.  This method can be called from
// custom cell formatters if you need to return a different formatted value for an export
// than for a live ListGrid
// @return (boolean)  returns true if this component is currently exporting client data
// @see listGrid.exportClientData
// @visibility external
//<
_exportingClientData: false,
isExportingClientData : function() {
    return !!this._exportingClientData;
},

//>	@method treeGrid.exportClientData()
// Exports this component's data with client-side formatters applied, so is suitable for direct
// display to users.  See +link{listGrid.exportClientData()} for details of the general 
// requirements and restrictions when exporting client data.
// <p>
// The following notes apply when exporting client data from TreeGrids:<ul>
// <li>Export only works correctly if you specify +link{fields,fields}; if you allow it to
//     generate a +link{createDefaultTreeField,default field}, nothing will be exported</li>
// <li>Only visible nodes are exported; if you close a node, its children are not exported 
//     even if they are loaded and known to the client</li>
// <li>Tree nodes are exported as a flat list, in the same order they are displayed in the 
//     TreeGrid</li>
// </ul>
// <P>
// If your TreeGrid has custom formatters, formatted values will be exported by default, with
// HTML normalized to text where possible.  Since some levels of HTML normalizing aren't 
// possible, this may result in missing or incorrect export values.  In this case, you have 
// two possible approaches:<ul>
// <li>Set +link{listGridField.exportRawValues,exportRawValues} on the field.  This will export
//     the raw underlying value of the field; your formatter will not be called</li>
// <li>Have your formatter call +link{treeGrid.isExportingClientData(),isExportingClientData()}
//     and perform whatever alternative formatting you require if that method returns true</li>
// </ul>
// @param [requestProperties] (DSRequest Properties) Request properties for the export.
//  Note that specifying +link{DSRequest.exportData,exportData} on the request properties
//  allows the developer to pass in an explicit data set to export.
// @param [callback] (RPCCallback) Optional callback.  If
//  you specify +link{DSRequest.exportToClient,exportToClient}: false in the request
//  properties, this callback will fire after export completes.  Otherwise the callback will
//  fire right before the download request is made to the server.
// @see listGrid.exportClientData
// @visibility external
//<

//>	@method treeGrid.isExportingClientData()
// Returns true if this component is currently 
// +link{treeGrid.exportClientData(), exporting client data}.  This method can be called from
// custom cell formatters if you need to return a different formatted value for an export
// than for a live TreeGrid
// @return (boolean)  returns true if this component is currently exporting client data
// @see treeGrid.exportClientData
// @visibility external
//<

//>	@method detailViewer.exportClientData()
// Exports this component's data with client-side formatters applied, so is suitable for direct
// display to users.  See +link{listGrid.exportClientData()} for details of the general 
// requirements and restrictions when exporting client data.
// <p>
// The following notes apply when exporting client data from DetailViewers:<ul>
// <li>Data is exported in "grid" format, with each record shown in a single row and each 
//     column representing a single field. This is quite different from the way DetailViewers
//     display records in the browser</li>
// </ul>
// <P>
// If your detailViewer has custom formatters, formatted values will be exported by default, with
// HTML normalized to text where possible.  Since some levels of HTML normalizing aren't 
// possible, this may result in missing or incorrect export values.  In this case, you have 
// two possible approaches:<ul>
// <li>Set +link{detailViewerField.exportRawValues,exportRawValues} on the field.  This will export
//     the raw underlying value of the field; your formatter will not be called</li>
// <li>Have your formatter call +link{detailViewer.isExportingClientData(),isExportingClientData()}
//     and perform whatever alternative formatting you require if that method returns true</li>
// </ul>
// @param [requestProperties] (DSRequest Properties) Request properties for the export.
//  Note that specifying +link{DSRequest.exportData,exportData} on the request properties
//  allows the developer to pass in an explicit data set to export.
// @param [callback] (RPCCallback) Optional callback.  If
//  you specify +link{DSRequest.exportToClient,exportToClient}: false in the request
//  properties, this callback will fire after export completes.  Otherwise the callback will
//  fire right before the download request is made to the server.
// @see listGrid.exportClientData
// @visibility external
//<


//>	@method detailViewer.isExportingClientData()
// Returns true if this component is currently 
// +link{detailViewer.exportClientData(), exporting client data}.  This method can be called from
// custom cell formatters if you need to return a different formatted value for an export
// than for a live detailViewer
// @return (boolean)  returns true if this component is currently exporting client data
// @see listGrid.exportClientData
// @visibility external
//<

//> @attr dataBoundComponent.emptyExportMessage (string : "You are attempting to export an empty dataset" : [IRW])
// The message to display to the user if an export of a DataBoundComponent's data is attempted
// while the DataBoundComponent's data is null or an empty list.
// @see listGrid.exportClientData
// @group i18nMessages
// @visibility external
//<
emptyExportMessage: "You are attempting to export an empty dataset",


defaultExportAs:"csv",
defaultExportFilename:"export",
defaultExportDisplay:"download",

exportClientDataReply : function (data, context) {

    if (data == null || data.length == 0) {
        // if there's no client-data to export, show a dialog and bail
        isc.warn(this.emptyExportMessage);
        return;
    }

    var props = context || {},
        format = props.exportAs ? props.exportAs : this.defaultExportAs,
        fileName = props.exportFilename ? props.exportFilename : this.defaultExportFilename,
        exportDisplay = props.exportDisplay ? props.exportDisplay : this.defaultExportDisplay,
        downloadToNewWindow = props.downloadToNewWindow == null 
                            ? (exportDisplay == "window") : props.downloadToNewWindow
    ;

    var serverProps = {
        operationId: props.operationId,
        showPrompt:false,
        transport: props.exportToClient === false ? "xmlHttpRequest" : "hiddenFrame",
        exportResults: true,
        downloadResult: !(props.exportToClient === false),
        download_filename: (exportDisplay == "window" ? fileName : null),
        params:props.params,
        xmlHttpRequestResponseType:props.xmlHttpRequestResponseType
    };

    if (props.exportRawValues == null) {
        props.exportRawValues = false;
    }

    var settings = {
        
        targetMainWindow: props.targetMainWindow,
        downloadToNewWindow: downloadToNewWindow,
        
        exportDisplay: props.exportDisplay,
        exportAs: props.exportAs,
        exportToClient: props.exportToClient,
        exportToFilesystem: props.exportToFilesystem,
        exportPath: props.exportPath,
        exportFilename: fileName,
        exportDelimiter: props.exportDelimiter,
        exportHeader: props.exportHeader,
        exportHeaderless: props.exportHeaderless,
        exportFooter: props.exportFooter,
        exportDefaultBGColor: props.exportDefaultBGColor,
        exportAlternateRowBGColor: props.exportAlternateRowBGColor,
        exportRowBGColors: props.exportRowBGColors,
        exportColumnBGColors: props.exportColumnBGColors,
        exportWrapHeaderTitles: props.exportWrapHeaderTitles,
        exportTitleSeparatorChar: props.exportTitleSeparatorChar,
        exportSpanTitleSeparator: props.exportSpanTitleSeparator,
        exportShowHeaderSpanTitles: props.exportShowHeaderSpanTitles,
        exportValueFields: props.exportValueFields,
        lineBreakStyle: props.lineBreakStyle,
        exportDatesAsFormattedString: props.exportDatesAsFormattedString,
        exportHeaderBGColor: props.exportHeaderBGColor,
        exportHeaderTextColor: props.exportHeaderTextColor,
        exportRawValues: props.exportRawValues,
        exportStreaming: props.exportStreaming,
        exportCurrencySymbol: isc.NumberUtil.currencySymbol,
        params:props.params
    };

    
    var exportFields = data._exportFields,
        exportTitles = data._exportTitles;

    if (exportFields == null) {
        // call getClientExportFields() to pick up fields from this component if necessary
        // (omitting canExport:false fields as appropriate).  This method already handles the
        // fields being defined on the "props" block directly.  Apply these explicitly to the
        // "settings" block so the server doesn't attempt to look at dataSource fields.
        exportFields = this.getClientExportFields(props);
        // Convert to strings
        for (var i = 0; i < exportFields.length; i ++) {
            if (isc.isAn.Object(exportFields[i])) exportFields[i] = exportFields[i].name
        }
        // validate that each field actually exists on the DBC
        
        var validExportFields = [];
        for (var i = 0; i < exportFields.length; ++i) {
            if (this.getField(exportFields[i]) != null) {
                validExportFields.add(exportFields[i]);
            }
            if (settings.exportValueFields && this.getField(exportFields[i]).displayField) {
                validExportFields.add(exportFields[i] + "_value");
            }
        }                                  
        exportFields = validExportFields;
    }
    settings.exportFields = exportFields;

    var allFields = this.getAllFields();

    var formulaFields = [];
    for (var i = 0; i < allFields.length; ++i) {
        var field = allFields[i];
        if (field.userFormula != null) {
            formulaFields[formulaFields.length] = field.userFormula;
            formulaFields[formulaFields.length - 1].name = field.name;
            formulaFields[formulaFields.length - 1].title = field.title;
        }
    }

    if (formulaFields.length > 0) {
        var formulaRemap = [];
        for (var u = 0; u < allFields.length; ++u) {
            formulaRemap[u] = {
                name: allFields[u].name,
                title: allFields[u].title
            };
        }
        settings.formulaFields = formulaFields;
        settings.formulaRemap = formulaRemap;
    }

    if (exportTitles == null) {
        // header spans
        var exportTitles = {};
        if (this.headerSpans && props.exportShowHeaderSpanTitles !== false) {
            settings.exportHeaderSpans = 
                this.prepareHeaderSpansForExport(this.headerSpans, allFields, exportTitles);
        }

        // non-spanned fields
        if (exportFields) {
            // `exportOtherFields' is passed to the server so that it knows what titles to use
            // for non-spanned fields.
            settings.exportOtherFields = {};
            for (var i = 0; i < exportFields.length; ++i) {
                var fieldName = exportFields[i];
                if (!exportTitles.hasOwnProperty(fieldName)) {
                    var field = this.getField(fieldName);
                    settings.exportOtherFields[fieldName] =
                        this.htmlUnescapeExportFieldTitle(field == null ? fieldName : 
                                              field.exportTitle || field.title || field.name);
                }
            }
        }
    } else {
        settings.exportOtherFields = exportTitles;
    }

    settings.exportDefaultBGColor = this.exportDefaultBGColor;
    settings.exportAlternateRowBGColor = this.exportAlternateRowBGColor;
    if (isc.isA.Function(this.getExportRowBGColors)) {
    	settings.exportRowBGColors = this.getExportRowBGColors();
    }
    if (isc.isA.Function(this.getExportColumnBGColors)) {
    	settings.exportColumnBGColors = this.getExportColumnBGColors();
    }
    
    if (this.headerHeight && this.exportHeaderHeights) {
        settings.exportHeaderHeight = this.headerHeight;
    }
    
    if (this.exportFieldWidths && isc.isAn.Array(this.fields) && this.getFieldWidth) {
        settings.exportFieldPixelWidths = this.getFieldPixelWidths();
		settings.exportWidthScale = this.exportWidthScale;
    }
    
        
    if (settings.exportWrapHeaderTitles == null) {
        settings.exportWrapHeaderTitles = this.exportWrapHeaderTitles;
    }
    
    if (this.exportFieldAlignments && isc.isAn.Array(this.fields)) {
        settings.exportAlignments = this.getFieldAlignments();
    }

    if (props.exportPropertyIdentifier) {
        settings.exportPropertyIdentifier = props.exportPropertyIdentifier;
    }


    if (this.getDataSource()) {
        isc.addProperties(serverProps, {exportContext: settings});
        this.getDataSource().exportClientData(data, serverProps, context.__callback);
    } else {
        isc.DMI.callBuiltin({
            methodName: "downloadClientExport",
            arguments: [ data, format, fileName, exportDisplay, settings ],
            requestParams: serverProps,
            callback: context.__callback
        });
    }
    
    delete this._exportingClientData;

    // can't fire callback on the DMI response because it's a download - fire now instead
    if (context.__callback && serverProps.downloadResult) this.fireCallback(context.__callback);
},

prepareHeaderSpansForExport : function (spans, fields, exportTitles) {
    exportTitles = exportTitles || {};
    var output = [];
    for (var i = 0; i < spans.length; i++) {
        var spanExportTitle = spans[i].exportTitle || spans[i].title || isc.emptyString;
        output[i] = {title: this.htmlUnescapeExportFieldTitle(spanExportTitle)};
        if (spans[i].height != null) {
            output[i].height = spans[i].height;
        }
        if (spans[i].spans) {
            output[i].spans = this.prepareHeaderSpansForExport(spans[i].spans, fields, exportTitles);
        } else {
            output[i].fields = [];
            for (var j = 0; j < spans[i].fields.length; j++) {
                var fieldName = spans[i].fields[j],
                    field = fields.find("name", fieldName);
                // field not present or hidden
                if (!field || this.getFieldNum(fieldName) == -1) continue;   
                var fieldTitle = field.exportTitle || field.title || field.name;

                if (fieldTitle) {
                    var escapedTitle = this.htmlUnescapeExportFieldTitle(fieldTitle);
                    output[i].fields.push({
                        name: fieldName,
                        title: escapedTitle
                    });
                    exportTitles[fieldName] = escapedTitle;
                }
            }
        }
    }
    return output;
},

//> @method dataBoundComponent.getSort()
// Return the +link{SortSpecifier}s representing the current sort configuration of this
// component.
// @return sortSpecifiers (Array of SortSpecifier) The current sort specification for this component
// @visibility external
//<
getSort : function () {
    return this._sortSpecifiers ? this.removeSortSpecifierMarkers(isc.shallowClone(this._sortSpecifiers)) : null;
},
// remove internal scribbles such as 'context', 'sortIndex', etc
removeSortSpecifierMarkers : function (sortSpecifiers) {
   if (sortSpecifiers == null) return null;
   sortSpecifiers.clearProperty("context");
   sortSpecifiers.clearProperty("sortIndex");
   return sortSpecifiers;
},

//> @method dataBoundComponent.setSort()
// Sort this component by a list of +link{SortSpecifier}s.  If the component's data is not a 
// +link{ResultSet}, only the first specifier is applied.
// 
// @param sortSpecifiers (Array of SortSpecifier)  List of +link{SortSpecifier} objects, one 
//   per sort-field and direction
// @visibility external
//<
setSort : function (sortSpecifiers) {
    this._sortSpecifiers = isc.shallowClone(sortSpecifiers);
    if (this.data && this._sortSpecifiers && this._sortSpecifiers.length>0) {
        for (var i = 0; i < this._sortSpecifiers.length; i++) {
            var item = this._sortSpecifiers[i];
            if (!item.context) item.context = this;
        }
        if (this.data.setSort) this.data.setSort(this._sortSpecifiers);
        else if (this.data.sortByProperty) {
            var item = this._sortSpecifiers[0];
            this.data.sortByProperty(
                item.property, 
                Array.shouldSortAscending(item.direction),
                item.normalizer,
                item.context
            );
        }
    }
},

//> @attr dataBoundComponent.multiSortDialogProperties (MultiSortDialog Properties : null : IR)
// Properties to apply to the +link{MultiSortDialog} which gets automatically generated
// when +link{dataBoundComponent.askForSort()} is called.
// <P>
// See also +link{listGrid.showHeaderSpanTitlesInSortEditor} and +link{listGrid.sortEditorSpanTitleSeparator}
//
// @visibility external
//<

//> @attr dataBoundComponent.multiSortDialogDefaults (MultiSortDialog Properties : null : IR)
// Class level defaults to apply to the +link{MultiSortDialog} which gets automatically
// generated when +link{dataBoundComponent.askForSort()} is called.
// <P>
// See also +link{listGrid.showHeaderSpanTitlesInSortEditor} and +link{listGrid.sortEditorSpanTitleSeparator}
//
// @visibility external
//<


//> @method dataBoundComponent.askForSort()
// Show a dialog to configure the sorting of multiple fields on this component.  Calls through
// to +link{MultiSortDialog.askForSort()}, passing this component as the fieldSource and the
// current +link{dataBoundComponent.getSort, sort-specification} if there is one.
// <P>
// The generated multiSortDialog can be customized via +link{multiSortDialogDefaults}, 
// +link{multiSortDialogProperties}.
//
// @visibility external
//<
askForSort : function (fieldName) {
    if (isc.MultiSortDialog && this.canMultiSort != false) {
        var sortSpecifiers = this.getSort();
        if (fieldName && (!sortSpecifiers || sortSpecifiers.length == 0)) {
            sortSpecifiers = [{ property: fieldName, direction: "ascending" }];
        }
        isc.MultiSortDialog.askForSort(
        	this, 
        	sortSpecifiers, 
        	{target:this, methodName:"multiSortReply"},
			isc.addProperties({}, 
				this.multiSortDialogDefaults, 
				this.multiSortDialogProperties)
        );
    }
},
multiSortReply : function (sortLevels) {
    if (sortLevels != null) {
        this.setSort(sortLevels);
    }
},

askForGrouping : function () {
	var groupFields = this.getGroupByFields();
    isc.MultiGroupDialog.askForGrouping(this, groupFields, this.getID()+".multiGroupReply(groupLevels)");
},
multiGroupReply : function (groupLevels) {
 	if (groupLevels) this.groupBy(groupLevels);
},


editCriteria : function () {
    var filterBuilder = isc.FilterBuilder.create({
        dataSource:this.dataSource,
        criteria: this.data && this.data.getCriteria ? this.data.getCriteria() : this.initialCriteria 
    });
    var _this = this;
    var theWindow = isc.Window.create({
        autoDraw:true,
        autoSize:true, width:600,
        autoCenter:true, isModal:true, 
        title:"Define Filter",
        bodyProperties : { layoutMargin:5, membersMargin:5 },
        items : [
            isc.HTMLFlow.create({
                width:"100%",
                isGroup:true,
                groupTitle:"Instructions",
                padding:5,
                contents:"Define field by field filter criteria below"
            }),
            filterBuilder,
            isc.IButton.create({
                title:"Save",
                click : function () {
                    var criteria = filterBuilder.getCriteria();
                    _this.editCriteriaReply(criteria);
                    this.parentElement.parentElement.closeClick();
                }
            })
        ]
    });
},
editCriteriaReply : function (criteria) {
    this.setCriteria(criteria);
},

//> @method dataBoundComponent.addValidationError()  (A)
// Helper method to add a validation error (or array of errors) to a list of existing errors 
// (passed in).
// Avoids duplicating errors.
// @group validation
//
// @param errors       (object)  current set of errors
//                               {itemName:"error", itemName:["error 1", "error 2"]}
// @param itemName     (string)  name of the item that has the error
// @param errorMessage (string)  actual error message
//
// @return (boolean)  returns true if error is not a duplicate
// @visibility internal
//<
// Not intended for public use - this is for directly updating an errors object.
addValidationError : function (errors, itemName, errorMessage) {
    var addedError = false;

    if (isc.isAn.Array(errorMessage)) {
        for (var i = 0; i < errorMessage.length; i++) {
            addedError = this.addValidationError(errors, itemName, errorMessage[i]) || addedError;
        }
        return addedError;
    }
    
    var isDataPath = itemName.contains(this._$slash);
    if (isDataPath) {
        var work = errors,
            elements = itemName.trim(this._$slash).split();
        for (var i = 0; i < elements.length; i++) {
            if (!work[elements[i]]) {
                if (i < elements.length - 1) {
                    if (parseInt(elements[i+1]) == elements[i+1]) {
                        work[elements[i]] = [];
                    } else {
                        work[elements[i]] = {};
                    }
                } else {
                    work[elements[i]] = errorMessage;
                    addedError = true;
                }
            }
            work = work[elements[i]];
        }
    } else {
        if (!errors[itemName]) {
            errors[itemName] = errorMessage;
            addedError = true;
        } else {
            if (!isc.isAn.Array(errors[itemName])) errors[itemName] = [errors[itemName]];
            
            if (!errors[itemName].contains(errorMessage)) {
                errors[itemName].add(errorMessage);
                addedError = true;
            }
        }
    }
    // Let caller know if we saved a new error message
    return addedError;
},

// Is <field> dependent on <fieldName>?
isFieldDependentOnOtherField : function (field, fieldName) {
    if (!field.validators) return false;

    var ds = this.getDataSource();

    for (var i = 0; i < field.validators.length; i++) {
        var validator = field.validators[i];
        if (!validator) continue;

        // Cache derived dependencies, if any.
        // Cannot derive dependencies unless we have a data source.
        if (!validator._derivedDependentFields && validator.applyWhen && ds != null) {
            validator._derivedDependentFields = ds.getCriteriaFields (validator.applyWhen);
        }

        // Explicit dependency?
        if (validator.dependentFields && validator.dependentFields.contains(fieldName)) {
            return true;
        }
        // ApplyWhen dependency?
        if (validator._derivedDependentFields &&
            validator._derivedDependentFields.length > 0 &&
            validator._derivedDependentFields.contains(fieldName))
        {
            return true;
        }
    }
    return false;
},

// Return dependencies for field (i.e. what fields it is dependent on)
getFieldDependencies : function (field) {
    if (!field.validators) return null;

    var ds = this.getDataSource(),
        dependencies = []
    ;

    for (var i = 0; i < field.validators.length; i++) {
        var validator = field.validators[i];
        if (!validator) continue;

        // Cache derived dependencies, if any.
        // Cannot derive dependencies unless we have a data source.
        if (!validator._derivedDependentFields && validator.applyWhen && ds != null) {
            validator._derivedDependentFields = ds.getCriteriaFields (validator.applyWhen);
        }

        // Explicit dependencies
        if (validator.dependentFields) {
            if (!isc.isAn.Array(validator.dependentFields)) {
                validator.dependentFields = [validator.dependentFields];
            }
            for (var j = 0; j < validator.dependentFields.length; j++) {
                dependencies.add(validator.dependentFields[j]);
            }
        }

        // ApplyWhen dependencies
        if (validator._derivedDependentFields &&
            validator._derivedDependentFields.length > 0)
        {
            dependencies.addList (validator._derivedDependentFields);
        }
    }
    return (dependencies.length == 0 ? null : dependencies);
},


//> @method dataBoundComponent.validateFieldAndDependencies() (A)
// Validate the field value against any validators defined on the field
// where validateOnChange is true and validate any fields that are dependent
// on the field.
//
// @param  field      (object)    pointer to the field descriptor object
// @param  validators (array)     Validators to be applied to field
// @param  newValue   (any)       value to be validated
// @param  record     (object)    copy of the record object
// @param  options    (object)    options object to control the validation process
//                  in the format {dontValidateNullValue: true/false,
//                                 typeValidationsOnly: true/false,
//                                 unknownErrorMessage: value or null,
//                                 changing: true/false,
//                                 serverValidationMode: "full"/"partial"}
// @return (object) null if no validation was performed, or validation result object
//                  in the format {valid: true/false,
//                                 errors: null or {fieldName: ["error", ...], ...}
//                                 resultingValue: value or null,
//                                 stopOnError: true/false}
//                  Note that if a dependent field has no errors an entry in the errors
//                  object will still exist but be null. This lets the caller know the
//                  field was validated and it is valid.
//<

validateFieldAndDependencies : function (field, validators, newValue, record, options) {

    var errors = {},
        validated = false,
        result = {valid: true,
                  errors: null,
                  resultingValue: null}
    ;

    // Apply newValue to record so that dependencies can reference it
    // If a validator changes newValue, the new value will overwrite this one.
    record[field.name] = newValue;

    // Process all validators for this field
    var fieldResult = this.validateField(field, field.validators, newValue, record, options);
    if (fieldResult != null) {
        result.valid = fieldResult.valid;
        result.stopOnError = fieldResult.stopOnError;
        if (fieldResult.errors != null) {
            this.addValidationError (errors, field.name||field.dataPath, fieldResult.errors);
        }
        if (fieldResult.resultingValue != null) {
            result.resultingValue = fieldResult.resultingValue;
            record[field.name] = fieldResult.resultingValue;
        }
        validated = true;
    }

    // Validate other fields that are dependent on this one.
    
    var fieldName = field.name || field.dataPath,
        fields = this.getFields() || []
    ;

    for (var i = 0; i < fields.length; i++) {
        
        var depField = fields[i];
        if (depField.name != fieldName  && depField.dataPath != fieldName &&
            this.isFieldDependentOnOtherField(depField, fieldName)) 
        {
            fieldResult = this.validateField(depField, depField.validators,
                                              record[depField.name], record, options);
            if (fieldResult != null ) {
                if (fieldResult.errors != null) {
                    this.addValidationError (errors, depField.name || depField.dataPath,
                                            fieldResult.errors);
                } else {
                    // Record the field in the errors object even though there is no error.
                    // This lets the caller know the field was validated _and_ it is valid.
                    this.addValidationError (errors, depField.name || depField.dataPath, null);
                }
                if (fieldResult.resultingValue != null) {
                    record[depField.name] = fieldResult.resultingValue;
                }
            }
        }
    }

    result.errors = errors;
    return (validated ? result : null);
},


//>	@attr dataBoundComponent.unknownErrorMessage (HTMLString : "Invalid value" : [IRW])
// For databound components that support editing, the error message for a failed validator
// that does not specify its own errorMessage.
// @group validation, i18nMessages
// @visibility external
//<
unknownErrorMessage : "Invalid value",

_$typeValidators: ["isInteger", "isFloat", "isBoolean", "isString"],

//> @method dataBoundComponent.validateField() (A)
// Validate the field value against any validators defined on the field.
//
// @param  field      (object)    pointer to the field descriptor object
// @param  validators (array)     Validators to be applied to field
// @param  value      (any)       Value to be validated
// @param  record     (object)    pointer to the record object
// @param  options    (object)    options object to control the validation process
//                  in the format {dontValidateNullValue: true/false,
//                                 typeValidationsOnly: true/false,
//                                 unknownErrorMessage: value or null,
//                                 changing: true/false,
//                                 serverValidationMode: "full"/"partial",
//                                 skipServerValidation: true/false,
//                                 deferServerValidation: true/false}
// @return (object) null if no validation was performed, or validation result object
//                  in the format {valid: true/false,
//                                 errors: null or {fieldName: ["error", ..], ...}
//                                 resultingValue: value or null,
//                                 stopOnError: true/false,
//                                 needsServerValidation: true/false}
//<
_$partial: "partial",
validateField : function (field, validators, value, record, options) {

    // If there are no validators for this field, we are done
    if (!validators) return null;

    var errors = [],
        validated = false,
        stopOnError = null,
        result = {valid: true,
                  errors: null,
                  resultingValue: null},
        needsServerValidation = false,
        forceShowPrompt = false
    ;

    if (!isc.isAn.Array(validators)) {
        validators = [validators];
    }

    // loop through validators
    for (var i = 0; i < validators.length; i++) {
        var validator = validators[i];
        if (!validator) continue;

        // If we're validating type only (eg, for a filter field), ignore other types
        // of validator
        var valType = isc.Validator.getValidatorType(validator);
        if (options && options.typeValidationsOnly && 
            !this._$typeValidators.contains(valType))
        {
            continue;
        }
                
        // Unless we're looking at a 'required' or  'requiredIf' field, don't try to validate
        // null values.
        
        if (options && options.dontValidateNullValue && 
            value == null && valType != "required" && valType != 'requiredIf')
        {
            continue;
        }

        // If we are processing all validators
        // OR only validateOnChange ones and settings allow
        if (!options || !options.changing || 
            (validator.validateOnChange != false &&
             (validator.validateOnChange || field.validateOnChange || this.validateOnChange)))
        {
            // Postpone server validations until we complete client-side ones
            if (isc.Validator.isServerValidator(validator)) {
                needsServerValidation = true;
                // If any server validator has stopOnError set, force synchronous mode
                if (validator.stopOnError) forceShowPrompt = true;
                continue;
            }

            if (validator.applyWhen) {
                var ds = this.getDataSource(),
                    criteria = validator.applyWhen
                ;
                if (ds == null) {
                    isc.logWarn("Conditional validator criteria ignored because form has no dataSource");
                } else {
                    var matchingRows = ds.applyFilter([record], criteria);
                    // Skip validator if condition does not apply
                    if (matchingRows.length == 0) {
                        // Use result of null to let validator know it was skipped
                        isc.Validator.performAction(null, field, validator, record, this);
                        continue;
                    }
                }
            }

            // process the validator
            validated = true;
            
            var isValid = (isc.Validator.processValidator(field, validator, value, null, record) == true);
            isc.Validator.performAction(isValid, field, validator, record, this);
            if (!isValid) {
                var errorMessage = isc.Validator.getErrorMessage(validator);
                if (errorMessage == null) {
                    if (options && options.unknownErrorMessage) {
                        errorMessage = options.unknownErrorMessage;
                    } else {
                        errorMessage = this.unknownErrorMessage;
                    }
                }
                errors.add(errorMessage);

                // Update stopOnError status based on the validator
                if (validator.stopOnError) stopOnError = true;
            }

            // if the validator returned a resultingValue, use that as the new value
            // whether the validator passed or failed.  This lets us transform data
            // (such as with the mask validator).
            if (validator.resultingValue != null) {
                result.resultingValue = validator.resultingValue;

                // Save resulting value for remaining validators
                value = validator.resultingValue;
            }
            // if the validator failed and we're supposed to stop on a false validator, bail!
            if (!isValid && validator.stopIfFalse) break;
        }
    }
    if (needsServerValidation == true && !validated) validated = true;

    // Process server-side validators
    if (needsServerValidation && options && options.deferServerValidation) {
        result.needsServerValidation = true;
    } else if (needsServerValidation && (!options || options.skipServerValidation != true)) {
        // If field or form has stopOnError set, we must show prompt for synchronous operation
        forceShowPrompt = this._resolveStopOnError(forceShowPrompt, field.stopOnError,
                                                   this.stopOnError);

        // Default to partial validation unless overridden by the caller
        var validationMode = ((options && options.serverValidationMode)
                              ? options.serverValidationMode
                              : this._$partial),
            showPrompt = (forceShowPrompt || field.synchronousValidation ||
                          this.synchronousValidation || false)
        ;
        var values;
        if (this.getDataSource()) {
            values = this.getDataSource()._cloneValues(record);
        } else {
            values = isc.addProperties({}, record);
        }
        var pendingAdd = this.getSaveOperationType && this.getSaveOperationType() == "add";
        // Make sure if local validators have converted the value, the converted value is sent
        var dataPath = isc.Canvas._getDataPathFromField(field, this);
        isc.DynamicForm._saveFieldValue(dataPath, field, value, values, this, true, "validate");
        // send validation request to server
        this.fireServerValidation(field, values, validationMode, showPrompt, options.rowNum, 
                                  pendingAdd);
        
        validated = true;                                  
    }

    // If validation failed and focus should be retained in field, let caller know
    result.stopOnError = (errors.length > 0 && 
                          this._resolveStopOnError(stopOnError, field.stopOnError,
                                                   this.stopOnError));

    // Populate remainder of result object
    result.errors = (errors.length == 0 ? null : errors);
    result.valid = (errors.length == 0);
    return (validated ? result : null);
},

validateFieldsOnServer : function (fields, record, options) {
    if (!isc.isAn.Array(fields)) fields = [fields];

    var primaryField = null,
        forceShowPrompt = false
    ;

    // loop through fields/validators to determine 
    for (var i = 0; i < fields.length; i++) {
        var field = fields[i],
            validators = field.validators
        ;

        // loop through validators
        for (var j = 0; j < validators.length; j++) {
            var validator = validators[j];
            if (!validator) continue;

            // Skip non-server validators
            if (!isc.Validator.isServerValidator(validator)) continue;

            // If any server validator has stopOnError set, force synchronous mode
            if (validator.stopOnError) {
                primaryField = field;
                forceShowPrompt = true;
            }
        }
    }

    // If field or form has stopOnError set, we must show prompt for synchronous operation
    forceShowPrompt = this._resolveStopOnError(forceShowPrompt, field.stopOnError,
                                               this.stopOnError);

    // Default to partial validation unless overridden by the caller
    var validationMode = ((options && options.serverValidationMode)
                          ? options.serverValidationMode
                          : this._$partial),
        showPrompt = (forceShowPrompt || field.synchronousValidation ||
                      this.synchronousValidation || false)
    ;
    var values;
    if (this.getDataSource()) {
        values = this.getDataSource()._cloneValues(record);
    } else {
        values = isc.addProperties({}, record);
    }
    var pendingAdd = this.getSaveOperationType && this.getSaveOperationType() == "add";
    // send validation request to server
    if (!primaryField) primaryField = fields[0];
    this.fireServerValidation(primaryField, values, validationMode, showPrompt, options.rowNum, 
                              pendingAdd);
},

// stopOnError is resolved validator value
_resolveStopOnError : function(stopOnError, fieldStopOnError, formStopOnError) {
    if (stopOnError != null) return stopOnError;
    return (fieldStopOnError == null && formStopOnError) || fieldStopOnError || false;
},

fireServerValidation : function (field, record, validationMode, showPrompt, rowNum, pendingAdd) {
    var ds = this.getDataSource();
    if (ds == null) return;

    var requestProperties = {showPrompt: showPrompt, 
                             prompt: isc.RPCManager.validateDataPrompt,
                             validationMode: validationMode,
                             internalClientContext: {
                                 component: this,
                                 fieldName: field.name,
                                 rowNum: rowNum }
                             };
    if (pendingAdd) requestProperties.pendingAdd = true;

    // Drop null values if validating in "partial" mode
    if (validationMode == this._$partial) {
        for (var fieldName in record) {
            if (record[fieldName] === null) delete record[fieldName];
        }
    }

    // If processing asynchronously, we must keep a list of outstanding requests
    // so that the DBC can check for dependencies before editing a field.
    if (!showPrompt) {
        var pendingFields = this._registerAsyncValidation(field);
        requestProperties.internalClientContext.pendingFields = pendingFields;
    }
    ds.validateData(record, 
                    this._handleServerValidationReply,
                    requestProperties);
},

_handleServerValidationReply : function (dsResponse, data, dsRequest) {
    if (dsResponse.status == isc.DSResponse.STATUS_FAILURE) {
        isc.logWarn("Server-side validation failed: " + dsResponse.data);
        isc.say(dsResponse.data);
    }
    var context = dsResponse.internalClientContext,
        component = context.component,
        pendingFields = context.pendingFields,
        errors = dsResponse.errors == null ? null : isc.DynamicForm.getSimpleErrors(dsResponse.errors);

    if (dsResponse.errors) {
        // Show server errors
        for (var fieldName in errors) {
            var fieldErrors = errors[fieldName],
                field = component.getField(fieldName);

            if (fieldErrors != null && field != null) {
                // Avoid changing focus by delaying update until redraw
                if (!isc.isAn.Array(fieldErrors)) fieldErrors = [fieldErrors];
                var stopOnError = null;
                for (var i = 0; i < fieldErrors.length; i++) {
                    component.addFieldErrors(fieldName, fieldErrors[i].errorMessage, false, context.rowNum);
                    if (fieldErrors[i].stopOnError) stopOnError = true;
                }
                if (field.redraw) field.redraw();

                stopOnError = component._resolveStopOnError(stopOnError, field.stopOnError,
                                                            component.stopOnError);

                // Restore focus to primary field if stopOnError
                if (fieldName == context.fieldName && stopOnError == true && !field.hasFocus) {
                    if (!field.synchronousValidation && !component.synchronousValidation) {
                        isc.logWarn("Server validation for " + fieldName +
                                    " signaled stopOnError but validation is not set for" +
                                    " synchronousValidation:true - stop ignored.");
                    } else {
                        component.focusInItem (field);
                    }
                }
            }
        }
    }

    // If request marked pending fields, clear them now.
    if (pendingFields) {
        component._clearAsyncValidation(pendingFields);
    }
    
    // support a callback method to fire when remote validation completes
    if (component && component.handleAsyncValidationReply != null) {
        
        if (errors != null) {
            errors = isc.DynamicForm.formatValidationErrors(errors);
        }
        component.handleAsyncValidationReply(errors == null, errors);
    }
},

//> @method dynamicForm.handleAsyncValidationReply()
// Notification fired when an asynchronous validation completes.
// @param success (boolean) true if validation succeeded, false if it failed
// @param errors (object) Map of errors by fieldName. Will be null if validation succeeded.
// @visibility external
//<
handleAsyncValidationReply : function (success, errors) {
},

//> @method dynamicForm.isPendingAsyncValidation()
// Is this component waiting for an asynchronous validation to complete?
// This method will return true after +link{dynamicForm.validate()} is called on a component with
// server-side validators for some field(s), until the server responds.
// <P>
// Note that the notification method +link{dynamicForm.handleAsyncValidationReply} will be fired
// when validation completes.
// @return (Boolean) true if this widget has pending asynchronous validations in process
// @visibility external
//<
isPendingAsyncValidation : function () {
    return !isc.isAn.emptyObject(this._pendingAsyncValidations);
},

// Pending asynchronous validations
// Format: <field>: <outstandingRequestCount>,
//         ...
_pendingAsyncValidations: {},

// Register async validation request for <field>.
// Returns: array of fields affected by this validation. Includes <field>.
_registerAsyncValidation : function (field) {
    var fields = this.getFields() || [],
        pendingFields = [field.name],
        fieldName = field.name
    ;

    // Register pending on field being validated
    this._pendingAsyncValidations[fieldName] = 
        (this._pendingAsyncValidations[fieldName] == null
            ? 1
            : this._pendingAsyncValidations[fieldName]++);

    // Register pending on fields dependent on field being validated
    for (var i = 0; i < fields.length; i++) {
        var depField = fields[i];
        if (depField.name != fieldName && this.isFieldDependentOnOtherField(depField, fieldName)) {
            var depFieldName = depField.name;
            pendingFields.add(depFieldName);

            this._pendingAsyncValidations[depFieldName] = 
                (this._pendingAsyncValidations[depFieldName] == null
                    ? 1
                    : this._pendingAsyncValidations[depFieldName]++);
        }
    } 
    return pendingFields;
},

// Clear pending validation for <fieldNames> array.
// If a pending UI interaction is blocked by a showPrompt, clear that.
_clearAsyncValidation : function (fieldNames) {
    var clearedAField = false;
    for (var i = 0; i < fieldNames.length; i++) {
        this._pendingAsyncValidations[fieldNames[i]]--;
        if (this._pendingAsyncValidations[fieldNames[i]] == 0) {
            delete this._pendingAsyncValidations[fieldNames[i]];
            clearedAField = true;
        }
    }
    // If any field was cleared see if we have a blocking focus to continue
    if (clearedAField && this._blockingFocus != null) {
        var unblock = true;
        for (var i = 0; i < this._blockingFocus; i++) {
            if (this._pendingAsyncValidations[this._blockingFocus[i]] > 0) {
                unblock = false;
                break;
            }
        }

        if (unblock) {
            this._blockingFocus = null;
            isc.clearPrompt();
        }
    }
},

// Array of field names which must be cleared from pending validations
// before unblocking focus.
_blockingFocus: null,

//> @method dataBoundComponent.blockOnFieldBusy
// Block UI activity by displaying showPrompt if validation is pending for specified field
// or any dependency. If shown the prompt will be removed automatically when responses
// are received.
//
// @param field (FormItem) Field being entered.
// @return (boolean) True if prompt was shown
//
// @visibility internal
//<
blockOnFieldBusy : function (field) {
    // If already blocking, nothing more to do. Let caller know we are blocked.
    if (this._blockingFocus != null) return true;

    // See if any requests are pending to matter
    var havePendingRequest = false;
    for (var fieldName in this._pendingAsyncValidations) {
        havePendingRequest = true;
        break;
    }
    if (!havePendingRequest) return false;

    // Get the list of fields we should check
    var dependentOnFields = this.getFieldDependencies(field) || [];
    dependentOnFields.add(field.name);

    // Determine which fields are still pending, if any
    var waitForFieldNames = [];
    for (var i = 0; i < dependentOnFields.length; i++) {
        var depFieldName = dependentOnFields[i];
        if (this._pendingAsyncValidations[depFieldName] > 0) {
            waitForFieldNames.add(depFieldName);
        }
    }
    if (waitForFieldNames.length > 0) {
        // We have at least one of our dependent fields pending a response - we have to block.
        this._blockingFocus = waitForFieldNames;
        
        
        this.delayCall("showValidationBlockingPrompt");
        return true;
    }
    return false;
},

// Called on a delay so execution occurs outside the "focus" thread.
// Don't show the prompt if this._blockingFocus has already been cleared
showValidationBlockingPrompt : function () {
    if (this._blockingFocus) isc.showPrompt(isc.RPCManager.validateDataPrompt);

},

//> @attr DataBoundComponent.rulesEngine (RulesEngine : null : IR)
// +link{RulesEngine} associated with this dataBoundComponent. The rulesEngine will associate
// a set of specified +link{Rule} objects with dataBoundComponents.
// <P>
// To apply a rulesEngine at runtime, use +link{rulesEngine.addMember()}
// @visibility rules
//<

// The following methods should be overridden by DBC implementations.
// These are used in validatorDefinition.action() methods to set the
// appearance of a field.
enableField : function (fieldName) {
    if (fieldName == null || isc.isAn.emptyString(fieldName)) return;
 
    var field = this.getField(fieldName);
    if (field) {
        field.disabled = false;
        this.redraw();
    }
},

disableField : function (fieldName) {
    if (fieldName == null || isc.isAn.emptyString(fieldName)) return;
 
    var field = this.getField(fieldName);
    if (field) {
        field.disabled = true;
        this.redraw();
    }
},

showField : function (fieldName) {
    if (fieldName == null || isc.isAn.emptyString(fieldName)) return;
 
    var field = this.getField(fieldName);
    if (field) field.show();
},

hideField : function (fieldName) {
    if (fieldName == null || isc.isAn.emptyString(fieldName)) return;
 
    var field = this.getField(fieldName);
    if (field) field.hide();
},


setFieldCanEdit : function (fieldName, canEdit) {
    if (fieldName == null || isc.isAn.emptyString(fieldName)) return;
 
    var field = this.getField(fieldName);
    if (field) {
        field.canEdit = canEdit;
        this.redraw();
    }
},

//> @method dataBoundComponent.isOffline()
// Returns true if the component's current data model is marked as offline.  This does not 
// necessarily mean that the component has no data; it may have data that was supplied from
// the +link{class:Offline,offline cache}.
// @return (boolean) Offline if true
// @group offlineGroup
// @visibility external
//<
isOffline : function () {
    if (this.data && this.data._offline) return true;
    return false;
},

//> @method dataBoundComponent.setSelectionComponent()
// setter for +link{dataBoundComponent.selectionComponent}
// @param component (ID | canvas) new selection component
// @visibility selectionComponent
//<
setSelectionComponent : function (component, init) {
    if (!component) {
        if (this.selectionComponent != null) {
            this.ignore(this.selectionComponent, "selectionChanged");
            this.ignore(this.selectionComponent, "cellSelectionChanged");
        }
        delete this.selectionComponent;
        if (this.valuesManager) {
            this.ignore(this.valuesManager, "_updateMultipleMemberValue");
        }
    } else {
        var specifiedComponent = component;
        if (isc.isA.String(component)) component = window[component];
        if (!component || !isc.isA.Canvas(component) || component.dataArity != "multiple") {
            this.logWarn("setSelectionComponent() - selection component specified as:" + 
                specifiedComponent + " this is not a valid component");
            return;
        }
        
        if (!component.getSelection) {
            this.logWarn("setSelectionComponent() - specified selection component:" + component + 
              " does not support selection - ignoring");
            return;
        }
        // if we had a previous selection component, clear up observations
        if (!init && this.selectionComponent) {
            // already pointing at the sc - we're done!
            if (this.selectionComponent == component) return
            if (this.isObserving(this.selectionComponent, "selectionChanged")) {
                this.ignore(this.selectionComponent, "selectionChanged");
            }
            if (this.isObserving(this.selectionComponent, "cellSelectionChanged")) {
                this.ignore(this.selectionComponent, "cellSelectionChanged");
            }
        } 
        this.selectionComponent = component;
        
        // Possibilities for ListGrid:
        // selectionChanged - record, state
        // cellSelectionChanged - cellList
        
        if (!this.selectionComponent.useCellRecords) {
            this.observe(this.selectionComponent, "selectionChanged",
                         "observer.selectionComponentSelectionChanged(observed, record,state)");
        } else {
            this.observe(this.selectionComponent, "cellSelectionChanged",
                         "observer.selectionComponentCellSelectionChanged(observed, cellList)");
        }
    }
},

// selectionChanged / cellSelectionChanged on the selectionComponent.  This is a generic 
// handler for anything that has a setData() method - note that DynamicForm has a custom
// version
selectionComponentSelectionChanged : function (selectionComponent, record, state) {
    if (!state) {
        if (this.dataArity == "single") {
            record = null;
        } else {
            return;
        }
    }
    
    if (this.dataArity == "single") {
        this.setData(record);
    } else {
        var elements = this.dataPath.split("/");
        this.setData(record[elements[elements.length - 1]]);
        // For a multi-record component, clear the current selection - if this component is acting
        // as the selectionComponent for a lower-level single-record component, this will cause 
        // that item to be cleared (which is what we want)
        if (this.dataArity == "multiple" && isc.isA.Function(this.deselectAllRecords)) {
            this.deselectAllRecords();
        }
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
        this.editRecord(record);
    }
},

// Build the MenuItem that DataBoundComponents will use to
// launch the FieldPickerWindow to edit the order of their fields.
createFieldPickerWindowMenuItem : function (title) {
    return {
        title: title,
        component: this,
        click: function () { 
            this.component.editFields();
        }
    }
},


//>	@method dataBoundComponent.editFields()
// Shows a +link{class:FieldPicker, FieldPicker} interface allowing end-users to edit
// the fields currently shown by this DataBoundComponent.
//
// @visibility external
//<
editFields : function () {
    var pickerWindow = this.fieldPickerWindow;
    if (pickerWindow) pickerWindow.show();
    else {
        var windowProperties = this.fieldPickerWindowProperties||{},
            pickerProperties = windowProperties.fieldPickerProperties||{};

        this.fieldPickerWindow = this.createAutoChild("fieldPickerWindow", {
            fieldPickerProperties: isc.addProperties(pickerProperties, {
                dataBoundComponent: this,
                canEditTitles:this.canEditTitles
            })
        });
        
        this.fieldPickerWindow.show();
    }
},

//> @classMethod dataBoundComponent.requestsArePending()
// Returns whether there are any pending +link{DSRequest}s initiated by this
// +link{dataBoundComponent}.  May not include any requests sent by directly calling the
// +link{DataSource} APIs (rather than the DataBoundComponent APIs).
// <P>
// @return (Boolean) true if one or more requests are pending, false otherwise.
// @visibility external
//<
_pendingRpcs: 0,
requestsArePending : function () {
    return this._pendingRpcs != 0;
}

});


// ------------------------------------------------------------------------------------------

//> @class MathFunction
// The definition of a function for use in the +link{FormulaBuilder}.  A function consists of 
// a name (what the user actually types to use the function), a description (shown in help) and 
// an actual JavaScript function that executes the calculation.
// <P>
// The built-in functions cover all static functionality on the JavaScript Math object:
// <ul>
// <li><b>max(val1,val2)</b>: Maximum of two values</li>
// <li><b>min(val1,val2)</b>: Minimum of two values</li>
// <li><b>round(value,decimalDigits)</b>: Round a value up or down, optionally providing 
//     <i>decimalDigits</i> as the maximum number of decimal places to round to.  For fixed 
//     or precision rounding, use <i>toFixed()</i> and <i>toPrecision()</i> respectively.
// </li>
// <li><b>ceil(value)</b>: Round a value up</li>
// <li><b>floor(value)</b>: Round a value down</li>
// <li><b>abs(value)</b>: Absolute value</li>
// <li><b>pow(value1,value2)</b>: value1 to the power of value2</li>
// <li><b>sqrt(value)</b>: Square root of a value</li>
// <li><b>dateAdd(value,interval,amount)</b>: Excel&trade;-compatible dataAdd function: adds  
//     quantities of a time interval to a date value.  Also supports being passed interval 
//     names, like "hour" or "week".
// </li>
// <li><b>toPrecision(value,precision)</b>: Format a number to a length of <i>precision</i> digits, 
//     rounding or adding a decimal point and zero-padding as necessary.  Note that the 
//     values 123, 12.3 and 1.23 have an equal precision of 3.  Returns a formatted string 
//     and should be used as the outermost function call in a formula. For rounding, use 
//     <i>round()</i>.
// </li>
// <li><b>toFixed(value,digits)</b>: Round or zero-pad a number to <i>digits</i> decimal places.  
//     Returns a formatted string and should be used as the outermost function call in a 
//     formula.  To round values or restrict precision, use <i>round()</i> and 
//     <i>toPrecision()</i> respectively.
// </li>
// <li><b>sin(value)</b>: Sine of a value</li>
// <li><b>cos(value)</b>: Cosine of a value</li>
// <li><b>tan(value)</b>: Tangent of a value</li>
// <li><b>ln(value)</b>: natural logarithm of a value</li>
// <li><b>log(base,value)</b>: logarithm of a value with the specified <i>base</i></li>
// <li><b>asin(value)</b>: Arcsine of a value</li>
// <li><b>acos(value)</b>: Arccosine of a value</li>
// <li><b>atan(value)</b>: Arctangent of a value (-PI/2 to PI/2 radians)</li>
// <li><b>atan2(value1,value2)</b>: Angle theta of a point (-PI to PI radians)</li>
// <li><b>exp(value)</b>: The value of E<sup>value</sup></li>
// <li><b>random()</b>: Random number between 0 and 1</li>
// </ul>
//
// @treeLocation Client Reference/Data Binding/FormulaBuilder
// @group formulaFields
// @visibility external
//<
isc.ClassFactory.defineClass("MathFunction", "Class");

// static properties and methods
isc.MathFunction.addClassProperties({
    

	_functions : {}                 // internal array to hold the list of registered functions
});


isc.MathFunction.addClassMethods({

//> @classMethod MathFunction.registerFunction()
// Registers a new math function for use with FormulaFields. Mixed-case names are allowed,
// and as a convenience, the following aliases are also available:<ul>
// <li> name in all lowercase 
// <li> name in all uppercase
// <li> name with first letter uppercase, and the rest unchanged</ul>
//
// Note: The aliases are shallow copies of each other, so be aware that if +link{jsFunction}
// depends on instance state, objects accessed by instance properties will be shared by all
// copies.
//
// @param newFunction (MathFunction)
// 
// @group formulaFields
// @visibility external
//<
registerFunction : function (newFunction) {
    if (!this._functions[newFunction.name]) {
        this._functions[newFunction.name] = newFunction;
    }
    // the following lines will add the math function with all lowercase name
    var newFunctionLowerCase = newFunction._copy();
    newFunctionLowerCase.name = newFunction.name.toLowerCase();
    newFunctionLowerCase.defaultSortPosition = -1;
    if (!this._functions[newFunctionLowerCase.name]) {
        this._functions[newFunctionLowerCase.name] = newFunctionLowerCase;
    }
    // the following lines will add the math function with all uppercase name
    var newFunctionUpperCase = newFunction._copy();
    newFunctionUpperCase.name = newFunction.name.toUpperCase();
    newFunctionUpperCase.defaultSortPosition = -1;
    if (!this._functions[newFunctionUpperCase.name]) {
        this._functions[newFunctionUpperCase.name] = newFunctionUpperCase;
    }
    // the following lines will add the math function with initial uppercase name
    var newFunctionInitialUpperCase = newFunction._copy();
    newFunctionInitialUpperCase.name = (newFunction.name.substr(0, 1).toUpperCase() +
                                        newFunction.name.substr(1));
    newFunctionInitialUpperCase.defaultSortPosition = -1;
    if (!this._functions[newFunctionInitialUpperCase.name]) {
        this._functions[newFunctionInitialUpperCase.name] = newFunctionInitialUpperCase;
    }
},

// Returns a list of all registered function-names
getRegisteredFunctionNames : function () {
    return isc.getKeys(this._functions);
},

// Returns a list of default function-names, sorted by defaultSortPosition
getDefaultFunctionNames : function () {
    var funcs = this.getDefaultFunctions(),
        index = funcs.makeIndex("name", false);
    return isc.getKeys(index);
},

// Returns a list of all registered functions
getRegisteredFunctions : function () {
    return isc.getValues(this._functions);
},

// Returns a list of default functions, order by defaultSortPosition
getDefaultFunctions : function () {
    var allFuncs = this.getRegisteredFunctions(),
        nonDefaults = allFuncs.findAll("defaultSortPosition", -1) || []
    ;

    for (var i=0; i<nonDefaults.length; i++) {
        var item = nonDefaults[i];
        allFuncs.remove(item);
    }

    allFuncs.sortByProperties(["defaultSortPosition"], ["true"]);
    return allFuncs;
},


//> @classMethod MathFunction.getRegisteredFunctionIndex()
// Returns an index of all registered functions by name
// 
// @return (Index)
// @group formulaFields
// @visibility external
//<
getRegisteredFunctionIndex : function () {
    var x = this.getRegisteredFunctions();
    var xIndex = x.makeIndex("name", false);
    return xIndex;
},

//> @classMethod MathFunction.getDefaultFunctionIndex()
// Returns an index of all default registered functions by name, ordered by 
// +link{mathFunction.defaultSortPosition}.  (Also includes those user-registered
// functions with non-default (&gt;= 0) values for that property.)
// 
// @return (Index)
//
// @see Array.makeIndex
// @see defaultSortPosition
// @group formulaFields
// @visibility external
//<
getDefaultFunctionIndex : function () {
    return this.getDefaultFunctions().makeIndex("name", false);
},

// Returns true if the named function is registered, false otherwise
isRegistered : function (name) {
    if (this._functions[name]) return true;
    return false;
}


});

isc.MathFunction.addProperties({
// attributes 
//> @attr mathFunction.name (identifier : null : IR)
// Name of the function (what the user actually types).  For example, a name of "min" would
// indicate that the user types "min(someValue)" to use this function.
// <P>
// Mixed-case names may be used.  As a convenience, a few aliases are registered by 
// +link{registerFunction} (see that method for details).
// 
// @see registerFunction
// @group formulaFields
// @visibility external
//<

//> @attr mathFunction.description (String : null : IR)
// A short description of this function
// 
// @group formulaFields
// @visibility external
//<

//> @attr mathFunction.jsFunction (Function : null : IR)
// Javascript method to perform the calculation associated with this function
// 
// @group formulaFields
// @visibility external
//<

//> @attr mathFunction.defaultSortPosition (integer : -1 : IR)
// Indicates the sort-order of this +link{MathFunction} in an index returned from static method
// +link{MathFunction.getDefaultFunctionIndex()}.  A lower value (&gt;= 0) will cause a function
// to appear before a +link{MathFunction} with a higher value of the property.  The default
// of -1 means to exclude the MathFunction from the index entirely.
// 
// @group formulaFields
// @see classMethod:MathFunction.getDefaultFunctionIndex()
// @visibility external
//<
defaultSortPosition: -1,

// copy a MathFunction instance

_copy : function (newProperties) {
    var instanceProperties = {};
    for (var property in this) {
        if (this.hasOwnProperty(property)) {
            instanceProperties[property] = this[property];
        }
    }
    return this.getClass().create(instanceProperties, newProperties);
}

});

// register some built in functions
// This first bunch are default ones that appear in the help list in FormulaBuilders
isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "max",
        description: "Maximum of two values",
        usage: "max(value1, value2)",
        defaultSortPosition: 1,
        jsFunction: function (value1, value2) {
            return Math.max(value1, value2);
        }
    })
);
isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "min",
        description: "Minimum of two values",
        usage: "min(value1, value2)",
        defaultSortPosition: 2,
        jsFunction: function (value1, value2) {
            return Math.min(value1, value2);
        }
    })
);
isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "clamp",
        description: "Value clamped to range specified",
        usage: "clamp(value1, value2)",
        defaultSortPosition: 3,
        jsFunction: function (value, min, max) {
            return isc.Math.clamp(value, min, max);
        }
    })
);
isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "round",
        description: "Round a value up or down, optionally providing <i>decimalDigits</i> " +
            "as the maximum number of decimal places to round to.  For fixed or precision " +
	        "rounding, use <i>toFixed()</i> and <i>toPrecision()</i> respectively.",
        usage: "round(value,decimalDigits)",
        defaultSortPosition: 4,
        jsFunction: function (value, decimalDigits) {
            if (decimalDigits) {
                var multiplier = Math.pow(10, decimalDigits),
                    result = Math.round(value * multiplier) / multiplier;

                return result;
            } 
            return Math.round(value);
        }
    })
);
isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "ceil",
        description: "Round a value up",
        usage: "ceil(value)",
        defaultSortPosition: 5,
        jsFunction: function (value) {
            return Math.ceil(value);
        }
    })
);
isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "floor",
        description: "Round a value down",
        usage: "floor(value)",
        defaultSortPosition: 6,
        jsFunction: function (value) {
            return Math.floor(value);
        }
    })
);
isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "abs",
        description: "Absolute value",
        usage: "abs(value)",
        defaultSortPosition: 7,
        jsFunction: function (value) {
            return Math.abs(value);
        }
    })
);
isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "pow",
        description: "Value1 to the power of Value2",
        usage: "pow(value1, value2)",
        defaultSortPosition: 8,
        jsFunction: function (value1, value2) {
            return Math.pow(value1, value2);
        }
    })
);
isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "sqrt",
        description: "Square root of a value",
        usage: "sqrt(value)",
        defaultSortPosition: 9,
        jsFunction: function (value) {
            return Math.sqrt(value);
        }
    })
);
isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "dateAdd",
        description: "Excel&trade;-compatible dataAdd function: adds a specified time interval to a date value",
        usage: "dateAdd(Date value, TimeUnit interval, number amount)",
        defaultSortPosition: 10,
        jsFunction: function (value, interval, amount) {
            if (value == null || !isc.isA.Date(value)) return null;
            // DateUtil.dateAdd is mutable but the MathFunction dateAdd should not be
            var localValue = value.duplicate();
            isc.DateUtil.dateAdd(localValue, interval, amount, 1, value.logicalDate);
            return localValue;
        }
    })
);
isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "toPrecision",
        description: "Format a number to a length of <i>precision</i> digits, rounding or " +
            "adding a decimal point and zero-padding as necessary.  Note that the values " +
            "123, 12.3 and 1.23 have an equal precision of 3.  Returns a formatted " +
            "string and should be used as the outermost function call in a formula. " +
            "For rounding, use <i>round()</i>.",
        usage: "toPrecision(value,precision)",
        defaultSortPosition: 11,
        jsFunction: function (value, precision) {
            var localValue=value;
            if (isc.isA.String(localValue)) localValue = parseFloat(localValue);
            if (isNaN(localValue)) return value;
            return localValue.toPrecision(precision);
        }
    })
);

isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "toFixed",
        description: "Round or zero-pad a number to <i>digits</i> decimal places.  Returns " +
            "a formatted string and should be used as the outermost function call in a " +
            "formula.  To round values or restrict precision, use <i>round()</i> and " +
            "<i>toPrecision()</i> respectively.",
        usage: "toFixed(value,digits)",
        defaultSortPosition: 12,
        jsFunction: function (value, digits) {
            var localValue=value;
            if (isc.isA.String(localValue)) localValue = parseFloat(localValue);
            if (isNaN(localValue)) return value;
            return localValue.toFixed(digits);
        }
    })
);

isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "sin",
        description: "Sine of a value",
        usage: "sin(value)",
        defaultSortPosition: 13,
        jsFunction: function (value) {
            return Math.sin(value);
        }
    })
);
isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "cos",
        description: "Cosine of a value",
        usage: "cos(value)",
        defaultSortPosition: 14,
        jsFunction: function (value) {
            return Math.cos(value);
        }
    })
);
isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "tan",
        description: "Tangent of a value",
        usage: "tan(value)",
        defaultSortPosition: 15,
        jsFunction: function (value) {
            return Math.tan(value);
        }
    })
);
isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "ln",
        description: "Natural logarithm of a value",
        usage: "ln(value)",
        defaultSortPosition: 16,
        jsFunction: function (value) {
            return Math.log(value);
        }
    })
);
isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "log",
        description: "logarithm of a value with the specified <i>base</i>",
        usage: "log(base, value)",
        defaultSortPosition: 17,
        jsFunction: function (base, value) {
            return Math.log(value) / Math.log(base);
        }
    })
);

// non-default functions (don't appear in the help list in FormulaBuilders)
isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "asin",
        description: "Arcsine of a value",
        usage: "asin(value)",
        defaultSortPosition: 18,
        jsFunction: function (value) {
            return Math.asin(value);
        }
    })
);
isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "acos",
        description: "Arccosine of a value",
        usage: "acos(value)",
        defaultSortPosition: 19,
        jsFunction: function (value) {
            return Math.acos(value);
        }
    })
);
isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "atan",
        description: "Arctangent of a value (-PI/2 to PI/2 radians)",
        usage: "atan(value)",
        defaultSortPosition: 20,
        jsFunction: function (value) {
            return Math.atan(value);
        }
    })
);
isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "atan2",
        description: "Angle theta of a point (-PI to PI radians)",
        usage: "atan2(value1,value2)",
        defaultSortPosition: 21,
        jsFunction: function (value1, value2) {
            return Math.atan2(value1, value2);
        }
    })
);
isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "exp",
        description: "The value of E<sup>value</sup>",
        usage: "exp(value)",
        defaultSortPosition: 22,
        jsFunction: function (value) {
            return Math.exp(value);
        }
    })
);
isc.MathFunction.registerFunction(
    isc.MathFunction.create({
        name: "random",
        description: "Random number between 0 and 1",
        usage: "random()",
        defaultSortPosition: 23,
        jsFunction: function () {
            return Math.random();
        }
    })
);


//> @object TestFunctionResult
// A TestFunctionResult is an ordinary JavaScript Object with properties that indicate the 
// status of an attempt to generate and execute a function for +link{FormulaBuilder} and 
// it's subclasses.
// <P>
// Because TestFunctionResult is always an ordinary JavaScript Object, it supports the
// normal behaviors of JavaScript Objects, including accessing and assigning to properties
// via dot notation:
// <pre>
//     var propValue = testFunctionResult.<i>propName</i>;
//     testFunctionResult.<i>propName</i> = newValue;
// </pre>
// <P>
// 
// @treeLocation Client Reference/Data Binding/FormulaBuilder
// @group formulaFields
// @visibility external
//<

//> @attr testFunctionResult.failedGeneration (boolean : false : IRW)
// Set to true if there is a syntax error in the formula or summary being checked.
// <P>
// When set to true, +link{testFunctionResult.errorText} contains the exception message.
//
// @group formulaFields
// @visibility external
//<

//> @attr testFunctionResult.failedExecution (boolean : false : IRW)
// Set to true if calling the formula or summary format resulted in a JavaScript Error.
// This would generally indicate a reference to non-existent data values.  See 
// +link{testFunctionResult.failedGeneration} for other types of failure.
// <P>
// When set to true, +link{testFunctionResult.errorText} contains the exception message.
//
// @group formulaFields
// @visibility external
//<

//> @attr testFunctionResult.emptyTestValue (boolean : false : IRW)
// Set to true if the formula or summary definition passed in was empty.
//
// @group formulaFields
// @visibility external
//<

//> @attr testFunctionResult.errorText (string : null : IRW)
// If the formula or summary format caused a JavaScript error, this contains the JavaScript error text.
//
// @group formulaFields
// @visibility external
//<

//> @attr testFunctionResult.result (string : null : IRW)
// When a formula or summary format is valid, <i>result</i> contains the result returned by the
// generated function when it was executed.
//
// @group formulaFields
// @visibility external
//<

//> @attr testFunctionResult.record (Record : null : IRW)
// Set to the record that was used when testing the generated function.  This is the record
// selected by +link{formulaBuilder.getTestRecord()}.
//
// @group formulaFields
// @visibility external
//<

isc.Canvas.registerStringMethods({
    //> @method databoundComponent.userAddedField
    // Notification method fired when a user-generated field is added to this component via
    // +link{editFormulaField()} or +link{editSummaryField()}.
    // <P>
    // Returning false from this method will prevent the field being added at all. Note that
    // this also provides an opportunity to modify the generated field object - any changes
    // made to the field parameter will show up when the field is displayed in the ListGrid.
    //
    // @param	field	   (ListGridField)	User generated summary or formula field
    // @return (boolean) Return false to cancel the addition of the field
    // @group formulaFields
    // @group summaryFields
    // @visibility external
    //<
    
    userAddedField:"field",

    //> @method dataBoundComponent.selectionUpdated()
    // Called when the selection changes. Note that this method fires exactly once for any given
    // change to the selection unlike the +link{ListGrid.selectionChanged,selectionChanged} event.
    // <P>
    // This event is fired once after selection/deselection has completed. The result is
    // one event per mouse-down event. For a drag selection there will be two events fired:
    // one when the first record is selected and one when the range is completed.
    // <P>
    // This event is also fired when selection is updated by a direct call to one of the
    // <code>DataBoundComponent</code> select/deselect methods. Calls on the +link{class:Selection}
    // object <b>do not</b> trigger this event.
    //
    // @param record        (object)                 first selected record in the selection, if any,
    // which may or may not be the first record in sort order if the <code>DataBoundComponent</code>
    // is sorted.  This parameter is typically used when only one record can be selected at a time.
    // @param recordList    (Array of Object)        List of records that are now selected
    // @group selection
    // @visibility external
    //<
    selectionUpdated : "record,recordList",

    //> @method dataBoundComponent.onFetchData()
    // Optional notification stringMethod fired on fetchData() or filterData()
    // the filter editor criteria.
    // @param criteria (Criteria) criteria passed to fetchData() / filterData()
    // @param requestProperties (DSRequest) request config passed to the filter/fetch request 
    // @visibility sgwt
    //<
    
    onFetchData:"criteria,requestProperties"

});
