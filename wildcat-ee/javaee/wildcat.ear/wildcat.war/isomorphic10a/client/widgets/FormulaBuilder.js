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




// ----------------------------------------------------------------------------------------

//> @class FormulaBuilder 
// Shows an interface allowing a user to enter simple formulas by typing them into a text
// field.
// <P>
// Available values for the formula are determined by the DataSource fields, and are given
// simple single-letter aliases (such as "A", "B", ...) similar to column names in Excel.
// The set of available values is shown in the +link{formulaBuilder.fieldKey} as a simple
// mapping between the +link{dataSourceField.title,field title} and it's short name.
// <P>
// By default, available math functions are shown in a hover from the
// +link{formulaBuilder.helpIcon,helpIcon} that appears after the formula field.
//
// @treeLocation Client Reference/Data Binding
// @group formulaFields
// @visibility external
//<
isc.ClassFactory.defineClass("FormulaBuilder", "VLayout");

isc.FormulaBuilder.addProperties({
// attributes 
vertical: true,
padding: 10,

//> @attr formulaBuilder.dataSource (DataSource or String : null : IRW)
// DataSource providing the available fields for the formulaBuilder.
// <P>
// By default the formulaBuilder will include <b>only</b> fields of numeric type or derived
// from a numeric type.  Set +link{formulaBuilder.fields} to override this.
//
// @group formulaFields
// @visibility external
//<

//> @attr formulaBuilder.dataSources (Array of DataSource : null : IR)
// Set of DataSources providing the available fields for the formulaBuilder.
// <P>
// By default the formulaBuilder will include <b>only</b> fields of numeric type or derived
// from a numeric type.  Set +link{formulaBuilder.fields} to override this.
// <P>
// Note that when multiple dataSources are supplied via the <code>dataSources</code> attribute,
// values to be used in the formula are expected to be nested in the data to which the
// formula is applied using dataSource IDs as prefixes. For example, for a formulaBuilder with
// dataSources set to ["countryDS", "worldDS"], where both countryDS and worldDS have a numeric
// field called "area", a valid record object might look like:
// <pre>
//    {countryDS:{countryName:"United Kingdom", area:243610},
//     worldDS:{area:510072000, landArea:148940000, oceanArea:361132000}}
// </pre>
// This allows the user to include field data from either dataSource in calculations (so in this
// case it would be easy to show percentage of total world surface area, for example).
//
// @group formulaFields
// @visibility rules
//<


//> @attr formulaBuilder.fields (Array of Field : null : IRW)
// Set this to override the underlying set of available fields.
//
// @group formulaFields
// @visibility external
//<

//> @attr formulaBuilder.field (Field : null : IR)
// The Field object representing the field being created or edited.
//
// @group formulaFields
// @visibility external
//<

//> @attr formulaBuilder.editMode (boolean : false : IR)
// Are we editing an existing field?
//
// @group formulaFields
// @visibility external
//<

//> @attr formulaBuilder.defaultSummaryFunction (SummaryFunction or Array of SummaryFunction : undefined : IR)
// The value to supply for a new field's summaryFunction if set; otherwise, if undefined,
// the summaryFunction for a new field is taken from the first input field (if any) of 
// the formula or summary (see +link{listGridField.summaryFunction}).
//<

//> @attr formulaBuilder.formulaField (AutoChild TextItem : null : IR)
// TextItem that users type into when entering a formula.
//
// @group formulaFields
// @visibility external
//<
showFormulaField: true,
formulaFieldDefaults: {
    type: "text", 
    formItemType: "AutoFitTextAreaItem",
    height: 20,
    width: "*",
    hoverWidth: 300,
    keyPress : function () {
        if (this.form.creator.autoTest) {
            this.fireOnPause("autoTest", {
                target: this.form.creator,
                methodName: "testFunction"
            }, this.form.creator.autoTestDelay);
        }
    }
},

//> @attr formulaBuilder.titleField (AutoChild TextItem : null : IR)
// TextItem that allows users to set the title for this field.
//
// @group formulaFields
// @visibility external
//<
showTitleField: true,
titleFieldDefaults: {
    selectOnFocus: true,
    type: "text", 
    width: "*"
},

//> @attr formulaBuilder.showHelpIcon (boolean : true : IR)
// Whether to show the help icon that appears after the +link{formulaField}.
//
// @group formulaFields
// @visibility external
//<
showHelpIcon: true,

//> @attr formulaBuilder.helpIcon (AutoChild FormItemIcon : null : IRA)
// Icon that appears after the +link{formulaField}, showing help on hover.
//
// @group formulaFields
// @visibility external
//<
helpIconDefaults: { src: "[SKIN]actions/help.png"
},

//> @attr formulaBuilder.autoHideCheckBoxLabel (String : "Auto hide fields used in formula" : IRW)
// Text label for the checkbox that allows the user to automatically hide the
// fields used in the formula.
//
// @group i18nMessages
// @visibility external
//<
autoHideCheckBoxLabel: "Auto hide fields used in formula",

//> @attr formulaBuilder.showAutoHideCheckBox (boolean : true : IR)
// Whether to show a checkbox offering the user the ability to automatically
// hide any fields involved in the formula.
//
// @group formulaFields
// @visibility external
//<
showAutoHideCheckBox: true,

//> @attr formulaBuilder.autoHideCheckBox (AutoChild TextItem : null : IR)
// CheckBox that, when selected, hides columns in the component that are used in this formula.
//
// @group formulaFields
// @visibility external
//<
autoHideCheckBoxDefaults: { type: "boolean", align: "right"
},

//> @attr formulaBuilder.builderTypeText (String : "Formula" : IR)
// Indicates whether to use "formula" or some other keyword in various captions and text
//
// @group i18nMessages
// @visibility external
//<
builderTypeText: "Formula",

//> @attr formulaBuilder.helpTextIntro (String : "For basic arithmetic, type in symbols (+-/%) directly.<P>The following functions are also available:" : IR)
// Text that appears in the hover from the +link{helpIcon}, as a pre-amble to the list of
// available functions.
//
// @group i18nMessages
// @visibility external
//<
helpTextIntro: "For basic arithmetic, type in symbols (+-/%) directly.<P>The following functions are also available:",

//> @attr formulaBuilder.mathFunctions (Array of String : null : IR)
// The list of math functions available in this FormulaBuilder, as an array of 
// +link{MathFunction, MathFunction} names.
// <P>
// The following function list is supported in FormulaBuilders by default: min(), max(), 
// round(), ceil(), floor(), abs(), pow(), sin(), cos(), tan(), ln() and log().
//
// @group formulaFields
// @visibility external
//<

//> @attr formulaBuilder.cancelled (Boolean : false : R)
// Was the builder operation cancelled?  Set to true when the user cancels with the cancel
// button or the dialog's close-button.
//
// @group formulaFields
// @visibility external
//<

//> @attr formulaBuilder.valuePrefix (String : null : IR)
// The prefix to apply to the variable that is inserted in response to a record click in the
// grid that shows the list of available fields.
//
// @group formulaFields
// @visibility external
//<

//> @attr formulaBuilder.valueSuffix (String : null : IR)
// The suffix to apply to the variable that is inserted in response to a record click in the
// grid that shows the list of available fields.
//
// @group formulaFields
// @visibility external
//<

// ------------------------------------------
// autoChildren
//
//> @attr formulaBuilder.fieldKey (AutoChild ListGrid : null : IR)
// ListGrid displaying the list of available fields and their corresponding formula keys.
//
// @group formulaFields
// @visibility external
//<
fieldKeyDefaults: {_constructor: "ListGrid",
    leaveScrollbarGap: false,  
    autoFitData: "both",
    autoFitMaxRecords: 8,
    autoFitMaxWidth: 500,
    autoFetchData: true,
    // show a rollover to make it clear which field will be pasted when clicked, but no
    // selection as there's nothing showing detail for a selection
    showRollOver:true, selectionType:"none", 
    // Allow the full title of fields to show (may introduce h-scrolling of course)
    autoFitFieldWidths:true,
    autoFitWidthApproach:"both",
    autoFitExpandField:"title",
    detailField: "title",
    canHover: true,
    hoverMode: "detailField",
    defaultFields: [
        {name: "mappingKey", width: 40},
        {name: "title", width: "*"},
        {name: "sourceDS", width: "*", showIf: "list.creator.dataSources != null"},
        {name: "name", showIf: "false"},
        {name: "type", showIf: "false"},
        {name: "length", showIf: "false"}
    ],
    // It's intuitive to attempt to click on the key grid to input entries.
    // Therefore allow this.
    recordClick : function (viewer, record) {
        var formulaField = this.creator.formulaField;
        if (formulaField) {
            // force focus in item before attempting to get the selectionRange otherwise it
            // will return null regardless of selection/caret position
            formulaField.focusInItem();
            var insertValue = this.creator.getInsertValueForRecord(record);
            if (insertValue != null) {
                // apply valuePrefix/valueSuffix
                if (this.creator.valuePrefix != null) insertValue = this.creator.valuePrefix+insertValue;
                if (this.creator.valueSuffix != null) insertValue += this.creator.valueSuffix;
                var value = formulaField.getEnteredValue() || "";
                var selectionRange = formulaField.getSelectionRange(),
                    caretPos;
                if (selectionRange != null) {
                    value = value.substring(0,selectionRange[0]) 
                        + insertValue + value.substring(selectionRange[1]);
                    caretPos = selectionRange[0] + insertValue.length;
                } else {
                    value += insertValue;
                    caretPos = insertValue.length;
                }
                formulaField.setValue(value);
                formulaField.focusInItem();
                // Ensure the cursor ends up after the newly added formula value.
                formulaField.setSelectionRange(caretPos,caretPos);
                if (this.creator.autoTest) {
                    this.fireOnPause("autoTest", {
                        target: this.creator,
                        methodName: "testFunction"
                    }, this.creator.autoTestDelay);
                }
            }
        }
    }
},

getInsertValueForRecord : function (record) {
    if (record == null) return "";
    var key = record.mappingKey;
    var escapeKey = this.insertEscapedKeys;
    if (escapeKey == null) escapeKey = this.allowEscapedKeys;
    if (escapeKey) {
        // #A is a synonym for #{A} - the latter is preferable for cases where there are 
        // enough fields that #AA would be technically ambiguous.
        key = "#{" + key + "}";
    }
    return key;
},


//> @attr formulaBuilder.instructionsTextStart (String : "The following fields are available for use in this \${builderType}": IRWA)
// The text to display as a preamble to the instruction text that appears in the 
// +link{formulaBuilder.instructions, instructions label}.
// <P>
// This is a dynamic string - text within <code>\${...}</code> are dynamic variables and will 
// be evaluated as JS code when the message is displayed.
// <P>
// Only one dynamic variable, builderType, is available and represents the type of this
// builder - either Formula or Summary.
// <P>
// Default value returns <P>
// <code>
// <i>The following fields are available for use in this [Formula/Summary]</i>
// </code>
// @group i18nMessages
// @visibility external
//<
instructionsTextStart: "The following fields are available for use in this \${builderType}",

//> @attr formulaBuilder.instructions (AutoChild Label : null : IR)
// Label displaying the instruction text above the fieldKey grid.
//
// @visibility external
//<
instructionsDefaults: {
    _constructor: "Label",
    height: 1,
    extraSpace: 10,
    overflow: "visible"
},

// defaults for dynamicForms that host the various controls
titleFormDefaults: { _constructor: "DynamicForm", extraSpace: 5, autoFocus: true
},
formulaFormDefaults: { _constructor: "DynamicForm", extraSpace: 5
},
hideFieldsFormDefaults: { _constructor: "DynamicForm", extraSpace: 5
},

sampleHeaderDefaults : { _constructor: "Label",
    height: 15,
    extraSpace: 5
},
sampleLabelDefaults : { _constructor: "Canvas",
    height: 40,
    width: "100%", 
    align: "center", 
    valign: "top",
    extraSpace: 10,
    showHover: true,
    overflow:"hidden",
    styleName: "sampleOutput"
},

//> @attr formulaBuilder.messageLabel (AutoChild Label : null : IR)
// Label used for displaying messages related to the validity of the current formula.
//
// @group formulaFields
// @visibility external
//<
messageLabelDefaults : { _constructor: "Label",
    height: 20, 
    width: "100%", 
    align: "right", 
    valign: "center",
    overflow:"hidden",
    showHover: true
},
        
// buttonLayout - HLayout to organise the buttons
buttonLayoutDefaults: { _constructor: "HLayout",
    width: "100%",
    height: 20,
    align: "right"
},

//> @attr formulaBuilder.cancelButton (AutoChild Button : null : IR)
// Button to Cancel this FormulaBuilder.  The formula is not tested, formulaBuilder.cancelled
// is set to true and formulaBuilder.fireOnClose is fired.
//
// @group formulaFields
// @visibility external
//<
cancelButtonDefaults: {_constructor: "IButton",
    autoParent: "buttonLayout",
    autoFit:true,
    extraSpace: 10,
    click: function () {
        this.creator.completeEditing(true);
    }
},

//> @attr formulaBuilder.testButton (AutoChild Button : null : IR)
// Button to Test the formula by generating it's function and executing it
//
// @group formulaFields
// @visibility external
//<
testButtonDefaults: {_constructor: "IButton", 
    autoParent: "buttonLayout",
    autoFit:true,
    extraSpace: 10,
    click: function () {
        this.creator.testFunction();
    }
},

//> @attr formulaBuilder.saveAddAnotherButton (AutoChild Button : null : IR)
// Button to Save the formula, by generating it's function, testing it and firing 
// formulaBuilder.fireOnClose, and then start editing another, new one.
//
// @group formulaFields
// @visibility external
//<
saveAddAnotherButtonDefaults: {_constructor: "IButton", 
    autoParent: "buttonLayout",
    autoFit: true, 
    extraSpace: 10,
    click: function () {
        if (!this.creator.showTitleForm || this.creator.titleForm.validate()) this.creator.saveAddAnother();
    }
},


//> @attr formulaBuilder.saveButton (AutoChild Button : null : IR)
// Button to Save the formula, by generating it's function, testing it and firing 
// formulaBuilder.fireOnClose
//
// @group formulaFields
// @visibility external
//<
saveButtonDefaults: {_constructor: "IButton", 
    autoParent: "buttonLayout",
    autoFit: true, 
    click: function () {
        // only validate the titleForm if its showing, of course!
        if (!this.creator.showTitleForm || this.creator.titleForm.validate()) this.creator.save();
    }
},

fieldType:"float", 
// when true, allow #A syntax as well as A
allowEscapedKeys: false,

//> @attr formulaBuilder.invalidBuilderPrompt (string : "Invalid \${builderType}: \${errorText}" : IRWA)
// When +link{formulaBuilder.testFunction, testFunction} reports an invalid formula, 
// this attribute provides the error-text to display in the
// +link{formulaBuilder.messageLabel, message-label}.
// <P>
// This is a dynamic string - text within <code>\${...}</code> are dynamic variables and will 
// be evaluated as JS code when the message is displayed.
// <P>
// The dynamic variables in this case, builderType and errorText, represent the type of this 
// builder, either Formula or Summary, and the description of the error, respectively.
// <P>
// The default output is:<P>
// <code>
// <i>Invalid [Formula/Summary]: + the description of the error detected </i>
// </code>
// @group i18nMessages
// @visibility external
//<
invalidBuilderPrompt: "Invalid \${builderType}: \${errorText}",

//> @attr formulaBuilder.defaultErrorText (String : "[No Explicit Error]" : IRW)
// If an invalid builder prompt is displayed, but no explicit error message was returned when
// attempting to evaluate the formula, this string will be used as a default.
// @see invalidBuilderPrompt
// @group i18nMessages
// @visibility external
//<
defaultErrorText : "[No Explicit Error]",


//> @attr formulaBuilder.invalidBlankPrompt (string : "Invalid blank \${builderType}" : IRWA)
// When +link{formulaBuilder.testFunction, testFunction} reports an empty formula, 
// this attribute provides the error-text to display in the
// +link{formulaBuilder.messageLabel, message-label}.
// <P>
// This is a dynamic string - text within <code>\${...}</code> are dynamic variables and will 
// be evaluated as JS code when the message is displayed.
// <P>
// Only one dynamic variable, builderType, is available and represents the type of this 
// builder, either Formula or Summary.
// <P>
// The default output is:<P>
// <code>
// <i>Invalid blank [Formula/Summary] </i>
// </code>
// @group i18nMessages
// @visibility external
//<
invalidBlankPrompt: "Invalid blank \${builderType}",

//> @attr formulaBuilder.validBuilderPrompt (string : "Valid \${builderType}" : IRWA)
// When +link{formulaBuilder.testFunction, testFunction} reports a valid formula and no other 
// errors, this attribute provides the error-text to display in the
// +link{formulaBuilder.messageLabel, message-label}.
// <P>
// This is a dynamic string - text within <code>\${...}</code> are dynamic variables and will 
// be evaluated as JS code when the message is displayed.
// <P>
// Only one dynamic variable, builderType, is available and represents the type of this 
// builder, either Formula or Summary.
// <P>
// The default output is:<P>
// <code>
// <i>Valid [Formula/Summary] </i>
// </code>
// @group i18nMessages
// @visibility external
//<
validBuilderPrompt: "Valid \${builderType}",

//> @attr formulaBuilder.helpWindowTitle (string : "\${builderType} Help" : IRWA)
// The title for the window that opens when the +link{formulaBuilder.helpIcon, Help icon}
// is clicked.
// <P>
// This is a dynamic string - text within <code>\${...}</code> are dynamic variables and will 
// be evaluated as JS code when the message is displayed.
// <P>
// Only one dynamic variable, builderType, is available and represents the type of this 
// builder, either Formula or Summary.
// <P>
// The default output is:<P>
// <code>
// <i>[Formula/Summary] Help</i>
// </code>
// @group i18nMessages
// @visibility external
//<
helpWindowTitle: "\${builderType} Help",

//> @attr formulaBuilder.titleFieldTitle (string : "Title" : IRWA)
// The text to display next to the "Title" field.
// @group i18nMessages
// @visibility external
//<
titleFieldTitle: "Title",

//> @attr formulaBuilder.defaultNewFieldTitle (string : "New Field" : IRWA)
// The default value for new Formula and Summary fields.
// @group i18nMessages
// @visibility external
//<
defaultNewFieldTitle: "New Field",

//> @attr formulaBuilder.keyColumnTitle (string : "Key" : IRWA)
// The default title for the "Key" column in +link{formulaBuilder.fields}.
// @group i18nMessages
// @visibility external
//<
keyColumnTitle: "Key",

//> @attr formulaBuilder.sourceFieldColumnTitle (string : "Source Field" : IRWA)
// The default title for the "Source Field" column in in +link{formulaBuilder.fields}.
// @group i18nMessages
// @visibility external
//<
sourceFieldColumnTitle: "Source Field",

//> @attr formulaBuilder.sourceDSColumnTitle (string : "Source DataSource" : IRWA)
// The default title for the "Source DataSource" column in in
// +link{formulaBuilder.fields}. Only shown if +link{formulaBuilder.dataSources} is used to
// specify multiple dataSources.
//
// @group i18nMessages
// @visibility rules
//<
sourceDSColumnTitle: "Source DataSource",

//> @attr formulaBuilder.cancelButtonTitle (string : "Cancel" : IRWA)
// The default title for the "Cancel" button.
// @group i18nMessages
// @visibility external
//<
cancelButtonTitle: "Cancel",

//> @attr formulaBuilder.saveAddAnotherButtonTitle (string : "Save /& Add Another" : IRWA)
// The default title for the "Save & Add Another" button.
// @group i18nMessages
// @visibility external
//<
saveAddAnotherButtonTitle: "Save & Add Another",

//> @attr formulaBuilder.saveButtonTitle (string : "Save" : IRWA)
// The default title for the "Save" button.
// @group i18nMessages
// @visibility external
//<
saveButtonTitle: "Save",

//> @attr formulaBuilder.saveConfirmationPrompt (string : "Save changes to this \${builderType}?" : IRWA)
// The text to display in the dialog that opens when there are unsaved changes and the user 
// cancels the builder.
// <P>
// This is a dynamic string - text within <code>\${...}</code> are dynamic variables and will 
// be evaluated as JS code when the message is displayed.
// <P>
// Only one dynamic variable, builderType, is available and represents the type of this 
// builder, either Formula or Summary.
// <P>
// The default output is:<P>
// <code>
// <i>Save changes to this [Formula/Summary]?</i>
// </code>
// @group i18nMessages
// @visibility external
//<
saveConfirmationPrompt: "Save changes to this \${builderType}?",

//> @attr formulaBuilder.invalidGeneratedFunctionPrompt (string : "The generated function is invalid - Check your \${builderType} and retry." : IRWA)
// When +link{formulaBuilder.testFunction, testFunction} reports an attempt to generate a 
// function from an invalid formula, this is the text to display in the 
// +link{formulaBuilder.messageLabel, message-label}.
// <P>
// This is a dynamic string - text within <code>\${...}</code> are dynamic variables and will 
// be evaluated as JS code when the message is displayed.
// <P>
// Only one dynamic variable, builderType, is available and represents the type of this 
// builder, either Formula or Summary.
// <P>
// The default output is:<P>
// <code>
// <i>The generated function is invalid - Check your [Formula/Summary] and retry.</i>
// </code>
// @group i18nMessages
// @visibility external
//<
invalidGeneratedFunctionPrompt: "The generated function is invalid - Check your \${builderType} and retry.",

//> @attr formulaBuilder.sampleHeaderTitle (string : "Sample:" : IRWA)
// The default title for the "Sample" panel, which displays a sample result for the formula.
// @group i18nMessages
// @visibility external
//<
sampleHeaderTitle: "Sample:",

//> @attr formulaBuilder.testButtonTitle (string : "Test" : IRWA)
// The default title for the "Test" button.
// @group i18nMessages
// @visibility external
//<
testButtonTitle: "Test"

});

isc.FormulaBuilder.addMethods({
// methods
getValue : function () {
    return this.formulaField ? this.formulaField.getValue() : null;
},

setValue : function (newValue) {
    if (this.formulaField) {
        this.formulaField.setValue(newValue);
    }
},

//> @method formulaBuilder.setFormula()
// Call to set the formula-string in this FormulaBuilder.
// <P>
// Note that calling setFormula() will update the UI, generate the formula's function and 
// test it automatically.
//
// @param (String) The new formula-string for this builder
// @group formulaFields
// @visibility external
//<
setFormula : function (newValue) {
    this.setValue(newValue);
},

getFieldIdProperty : function () {
    return this.getClass().getFieldIdProperty(this.component);
},

getTitle : function () {
    return this.titleField ? this.titleField.getValue() : null;
},

setTitle : function (newTitle) {
    if (this.titleField) {
        this.titleField.setValue(newTitle);
    }
},

getFieldFromMappingKey : function (mappingKey) {
    var fields = this.getAvailableFields();

    for (var i=0; i<fields.length; i++) {
        var item = fields.get(i);
        if (item.mappingKey == mappingKey) return item;
    }
    return null;
},

getFields : function () {
    if (this.fields) return this.fields;

    if (this.component) return this.component.getAllFields();
    var values;
    if (this.dataSources) {
        values = [];
        for (var i =0; i < this.dataSources.length; i++) {
            var ds = this.dataSources[i],
                fields = ds.getFields();
            for (var field in fields) {
                var fbField = isc.addProperties({}, fields[field], 
                    {name:ds.getID() + "." + field,
                     sourceDS:ds.getID()});
                values.add(fbField);
            }
        }
    } else {
        values = isc.getValues(this.dataSource.getFields());
    }
    return values;
},

shouldHideUsedFields : function () {
    if (this.showAutoHideCheckBox && this.autoHideCheckBox && this.autoHideCheckBox.getValue()) {
        return this.autoHideCheckBox.getValue();
    } else return false;
},

//> @method formulaBuilder.getHelpText()
// Call to retrieve the text the FormulaBuilder would show by default for help, or override to
// provide alternate help text.
//
// @return (String) The results of getHoverText()
// @group formulaFields
// @visibility external
//<
getHelpText : function () {
    return this.getHoverText();
},

initWidget : function () {
    this.Super("initWidget", arguments);

    // get the dataSource so we know what fields to support
    if (this.dataSource) this.dataSource = isc.DataSource.get(this.dataSource);
    if (this.dataSources) {
        var liveDSs = [];
        for (var i = 0; i < this.dataSources.length; i++) {
            liveDSs[i] = isc.DataSource.get(this.dataSources[i]);
        }
    }

    var availableFields = this.getAvailableFields();

    if (!this.field) {
        this.field = {
            name: this.getUniqueFieldName(),
            title: this.defaultNewFieldTitle,
            type: this.fieldType,
            width: "50",
            canFilter: false,
            canSortClientOnly: true,
            originalOrder: this.availableFields.length,
            summaryFunction: this.defaultSummaryFunction
        };
    }
    // --------------
    // draw the layout

    // add the fieldKey that displays the list of available fields
    this.instructions = this.createAutoChild("instructions", {
        contents: this.instructionsTextStart.evalDynamicString(this, { builderType: this.builderTypeText })
    });
    this.addMember(this.instructions);        

    this.fieldKeyDS = isc.DataSource.create({
        ID: this.getID()+"DS",
        clientOnly: true,
        testData: availableFields,
        fields: [
            {name: "mappingKey", title: this.keyColumnTitle, width: 40},
            {name: "title", title: this.sourceFieldColumnTitle, width: "*"},
            {name:"sourceDS", title: this.sourceDSColumnTitle,
                // showIf overridden in fieldKeyDefaults
                showIf:"false"},

            {name: "name", showIf: "false", primaryKey: true},
            {name: "type", showIf: "false"},
            {name: "length", showIf: "false"}
        ]
    });

    this.fieldKey = this.createAutoChild("fieldKey", { 
        dataSource: this.fieldKeyDS
    });
    if (this.fieldKey.showFilterEditor !== false && this.fieldKey.autoFitMaxRecords && 
        availableFields.length > this.fieldKey.autoFitMaxRecords) 
    {
        this.fieldKey.setShowFilterEditor(true);
    }
    this.addMember(this.fieldKey);

    // add the titleField that allows the user to re-caption the Field
    if (this.showTitleField) {
        this.addAutoChild("titleForm", {
            fields: [isc.addProperties(
                this.titleFieldDefaults, 
                this.titleFieldProperties,
                { title: this.titleFieldTitle, name: "titleField" }
            )]
        });
        this.titleField = this.titleForm.getField("titleField");
        this.setTitle(this.field.title || isc.DataSource.getAutoTitle(this.field.name));
    }

    // add the formulaField TextItem - maybe override visibility of this because FormulaBuilder
    // is useless without a formula!
    if (this.showFormulaField) {
        this.addAutoChild("formulaForm", {
            fields: [isc.addProperties({ title: this.builderTypeText }, 
                this.formulaFieldDefaults, this.formulaFieldProperties, 
                this.showHelpIcon ? {
                icons: [isc.addProperties({ prompt: this.getHelpText() },
                    this.helpIconDefaults, this.helpIconProperties,
                    { click: "form.creator.showHelpWindow();" }
                )]
                } : {}, 
                { name: "formulaField"}
            )]
        });
        this.formulaField = this.formulaForm.getField("formulaField");
        if (this.showHelpIcon) this.helpIcon = this.formulaField.icons[0];
    }

    // display the test status or error here following a call to testFunction()
    this.addAutoChild("messageLabel");
    // display the test-case here following a call to testFunction()
    this.addAutoChild("sampleHeader", { contents: this.sampleHeaderTitle });
    this.addAutoChild("sampleLabel");

    // add the checkbox that allows hiding of fields used in the formula
    if (this.showAutoHideCheckBox) {
        this.addAutoChild("hideFieldsForm", {  
            fields:[ isc.addProperties( { title: this.autoHideCheckBoxLabel },
                this.autoHideCheckBoxDefaults, 
                this.autoHideCheckBoxProperties,
                { name: "autoHide" }
            )]
        });
        this.autoHideCheckBox = this.hideFieldsForm.getField("autoHide");
    }

    // show the buttons in a layout
    this.addAutoChild("buttonLayout");
    this.addAutoChild("cancelButton", { title: this.cancelButtonTitle});
    if (!this.autoTest) this.addAutoChild("testButton", { title: this.testButtonTitle});
    this.addAutoChild("saveAddAnotherButton", { title: this.saveAddAnotherButtonTitle });
    this.addAutoChild("saveButton", { title: this.saveButtonTitle });

    if (this.showTitleField) this.titleForm.focusInItem(this.titleField);
    else this.formulaForm.focusInItem(this.formulaField);
    
    // set the initialValue specific to FormulaBuilder.  Override in subclasses
    this.setInitialValue();

    if (this.editMode && this.autoTest) this.testFunction();
},

getUniqueFieldName : function () {
    return this.getNewUniqueFieldName("formulaField");
},

getNewUniqueFieldName : function (namePrefix) {
    // assume return values in the format "fieldXXX" if namePrefix isn't passed
    if (!namePrefix || namePrefix == "") namePrefix = "field";
    var component = this.component,
        useUUID = (component && component.fieldNamingStrategy || "simple") == "uuid",
        fields = this.getFields(),
        keyLength = namePrefix.length,
        result
    ;

    if (useUUID) {
        result = namePrefix + isc.Math.randomUUID();
    } else {
        var attrName = namePrefix + "Index";
        // if there's a component instance, maintain a count of namePrefix uses on it do we don't duplicate
        if (component && !component.fieldNameGenerator[attrName]) component.fieldNameGenerator[attrName] = 1;
        var maxIncrement = component ? component.fieldNameGenerator[attrName] : 1;

        // find the next available increment for the namePrefix
        for (var i = 0; i<fields.length; i++) {
            var item = fields.get(i);
            if (item.name.startsWith(namePrefix)) {
                var suffix = item.name.substr(keyLength),
                    increment = new Number(suffix);
                if (increment && increment >= maxIncrement) maxIncrement = increment + 1;
            }
        }
        if (component) component.fieldNameGenerator[attrName] = maxIncrement;
        result = namePrefix + maxIncrement;
    }
    // return the new fieldName
    return result;
},

destroy : function () {
    if (this.fieldKeyDS) this.fieldKeyDS.destroy();
    this.Super("destroy", arguments);
},

// set the initialValue specific to FormulaBuilder (field.userFormula). Override in subclasses
setInitialValue : function () {
    if (this.editMode && this.field.userFormula) {
        this.initialValue = this.field.userFormula.text;
        if (this.field.userFormula.allowEscapedKeys) {
            this.allowEscapedKeys = this.field.userFormula.allowEscapedKeys;           
        }
    }

    this.initialValue = this.initialValue || "";

    this.setValue(this.initialValue);
},

showHelpWindow : function () {
    var window = this.locatorParent,
        top = window ? window.getTop() : this.top,
        left = window ? window.getRight() : this.left,
        width = window ? window.getVisibleWidth() : this.width,
        height = window ? window.getVisibleHeight() : this.getVisibleHeight();

    if (this.helpWindow && this.helpWindow != null) {
        this.hideHelpWindow();
    } else {
        this.helpIcon.prompt = null;
        this.formulaField.stopHover();

        var _this = this;
        this.helpWindow = isc.Window.create({
            title: this.helpWindowTitle.evalDynamicString(this, { builderType: this.builderTypeText }),
            showMinimizeButton: false,
            showMaximizeButton: false,
            isModal: false,
            closeClick : function () {
                _this.hideHelpWindow();
            },
            headerIconProperties: {
                src: "[SKIN]actions/help.png"
            },

            items: [isc.Label.create({
                contents: this.getHelpText(),
                canSelectText: true, // for copy/paste of function names
                padding: 10
            })]
        });
        // stay on top of the modal mask created by the calling window so that e.g. moving or
        // clicking on the calling window doesn't push the help window behind the click mask
        // (making it unreachable)
        this.helpWindow.observe(window, "bringToFront", "observer.bringToFront()");

        if (window) {
            var neededSpace = window.getRight()+width;
            var overflow = neededSpace-isc.Page.getWidth();
            if (overflow > 0) {
                window.setLeft(window.getLeft()-overflow);
                left = window.getRight();
            }
        }
        this.helpWindow.resizeTo(width, height);
        this.helpWindow.moveTo(left, top);
        this.helpWindow.show();    
        if (window) {
                    
        }        
    }
},

hideHelpWindow : function () {
    if (this.helpWindow) {
        this.helpWindow.destroy();
        this.helpWindow = null;
    }
    this.helpIcon.prompt = this.getHelpText();
    this.formulaField.stopHover();
},

// Internal method that provides the default help-text when hovering over the helpIcon
getHoverText : function () {
	var output = isc.SB.create();

    output.append("<b>", this.helpTextIntro, "</b> <P>");
    output.append("<ul>");
    var index = isc.MathFunction.getRegisteredFunctionIndex(),
        functions = this.mathFunctions
    ;
    
    if (functions && functions.length > 0) {
        for (var i=0; i< functions.length; i++) {
            var item = index[functions[i]];
            output.append("<li> <b>", item.name, ": </b> ", item.description, "<p>");
            output.append("<i>usage: ", item.usage, "</i> </li>");
        }
    }
    output.append("</ul>");

    return output.release(false);
},

// Get an array of those fields available for use in the formula
// (based on visibility and numeric type)
getAvailableFields : function () {
    if (this.availableFields) return this.availableFields;

    var availableFields = this.availableFields = [],
        currentField = this.field,
        fields = this.getFields(),
        j=0;

    if (!fields) return availableFields;

    for (var i = 0; i < fields.getLength(); i++) {
        var item = isc.addProperties({}, fields.get(i)),
            type = item.type;

        item.originalOrder = i;

        if (currentField) {
            if (currentField.name == item.name) continue;
            // Disallow circular
            // formula fields (one refers to another, which refers back to it!)            
            if (item.userFormula) {
                var usedVar = false,
                    vars = item.userFormula.formulaVars || {};
                for (var fieldKey in vars) {
                    if (vars[fieldKey] == currentField.name) {
                        usedVar = true;
                        break;
                    }
                }
                
                if (usedVar) continue;
            }
        }

        if (item.userFormula ||
            isc.SimpleType.inheritsFrom(type, "integer") || 
            isc.SimpleType.inheritsFrom(type, "float"))
        {
            item.mappingKey = isc.FormulaBuilder.mappingKeyForIndex(j++);
            if (!item.title) item.title = isc.DataSource.getAutoTitle(item.name);
            availableFields.add(item);
        }
    }
    
    // now, move field mappingKeys around according to those keys already used in the formula
    var vars = currentField && currentField.userFormula ? 
                currentField.userFormula.formulaVars : {}
    ;

    
    var badVars = [];
    for (var key in vars) {
        var mappedField = availableFields.find("mappingKey", key),
            actualField = availableFields.find("name", vars[key])
        ;
        if (actualField == null) { badVars.add(key); continue; }
        // if there's a field already associated with the key bound to the actual field,
        // give that field the key we originally wanted to assign to the actual field
        if (mappedField) mappedField.mappingKey = actualField.mappingKey;
        actualField.mappingKey = key;
    }
    if (badVars.length > 0) {
        var missingMarker = this.component && this.component.missingFormulaFieldValue || "-";
        isc.FormulaBuilder.remapBadVars(badVars, vars, currentField.userFormula, missingMarker);
    }

    isc.FormulaBuilder.sortFields(availableFields, true);
    isc.FormulaBuilder.applyHeaderSpanTitles(this, availableFields);
    return availableFields;
},

// Get an array of used-fields from those fields available for use in the formula
getUsedFields : function (up) {

    var usedFields = [],
        formula = this.getValue(),
        availableFields = this.getAvailableFields().duplicate();

    if (!formula) return usedFields;

    isc.FormulaBuilder.sortFields(availableFields, !!up);

    for (var i = 0; i < availableFields.length; i++) {
        var item = availableFields.get(i);
        if (isc.FormulaBuilder.fieldIsUsed(formula, item.mappingKey, this.allowEscapedKeys)) {
            usedFields.add(item);
        }
    }
    return usedFields;
},

getCompleteValueObject : function () {
    var usedFields = this.getUsedFields(),
        func = this.generateFunction(),
        
        properties = { _generatedFormulaFunc: func,
            type: this.fieldType,
            userFormula : { text: this.getValue(), formulaVars: {} }
        },
	    fieldIdProperty = this.getFieldIdProperty();


    if (this.allowEscapedKeys) properties.userFormula.allowEscapedKeys = true;

    for (var i=0; i<usedFields.length; i++) {
        var item = usedFields.get(i);
        properties.userFormula.formulaVars[item.mappingKey] = item[fieldIdProperty];
    }

    return properties;    
},

getBasicValueObject : function () {
    var usedFields = this.getUsedFields(),
        userFormula = { text: this.getValue(), formulaVars: {} },
	    fieldIdProperty = this.getFieldIdProperty();

    if (this.allowEscapedKeys) userFormula.allowEscapedKeys = true;

    for (var i=0; i<usedFields.length; i++) {
        var item = usedFields.get(i);
        userFormula.formulaVars[item.mappingKey] = item[fieldIdProperty];
    }

    return userFormula;    
},

//> @method formulaBuilder.getUpdatedFieldObject()
// Returns the entire property-set for the updated field, including title and formula-related
// properties
// 
// @return (Field) The original field along with the updated title and formula
// @group formulaFields
// @visibility external
//<
getUpdatedFieldObject : function () {
    return isc.addProperties( this.field, 
        { title: this.getTitle() }, 
        this.getCompleteValueObject() 
    );
},

//> @method formulaBuilder.testFunction()
// Test the formula by generating it's function and trying to run it.
// @return (string) result of the function
// @group formulaFields
// @visibility external
//<
testFunction : function () {
    var result = this.getClass().testFunction(this.field, this.getBasicValueObject(), 
        this.component, 
        this.getFields(),
        this.testRecord                                              
    );
    
    var testMessage = "",
        errorText = result.errorText || this.defaultErrorText;

    if (result.failedGeneration || result.failedExecution) {
        testMessage = this.invalidBuilderPrompt.evalDynamicString(this, { 
                        builderType: this.builderTypeText,
                        errorText: errorText
                    });
    } else if (result.emptyTestValue) {
        testMessage = this.invalidBlankPrompt.evalDynamicString(this, { 
                        builderType: this.builderTypeText
                    });
    } else {
        testMessage = this.validBuilderPrompt.evalDynamicString(this, { 
                        builderType: this.builderTypeText
                    });
    }
    
    this.setTestMessage(testMessage);
    this.setSamplePrompt(this.getSamplePrompt(result));

    return result;
},

//> @method formulaBuilder.getTestRecord()
// Gets the +link{formulaBuilder.testRecord, test record} for this formula.
// @return (Object) the +link{formulaBuilder.testRecord, testRecord} for this formula
// @group formulaFields
// @visibility external
//<
getTestRecord : function () {
    if (this.testRecord) return this.testRecord;
    return this.getClass().getTestRecord(this.component, this.getAvailableFields());
},

setTestMessage: function (message) {
    this.messageLabel.setContents(message);
},

setSamplePrompt: function (message) {
    this.sampleLabel.setContents("<center>"+message+"</center>");
},

// Create a function to wrap a calculation
//      * script local vars for all used fields
//      * script local vars for all mapped MathFunctions
//      * return the result of the formula
generateFunction : function () {
    return this.getClass().generateFunction(this.getBasicValueObject(), this.getUsedFields(),
        this.component);
},

//> @method formulaBuilder.saveAddAnother()
// Call to finish working, test the formula and call 
// +link{FormulaBuilder.fireOnClose(), fireOnClose()}.  If the formula saves ok, don't close
// the builder but instead reset it, ready to add a new formula-field.
//
// @group formulaFields
// @visibility external
//<
saveAddAnother : function () {
    this.restartBuilder = true;
    this.save();
},

//> @attr formulaBuilder.warnDuplicateTitles (string : null : IRWA)
// Should the user be showed a warning when the entered Title value already exists?
// @visibility external
//<

//> @attr formulaBuilder.warnDuplicateTitlesMessage (string : "Another field already has the title '${fieldTitle}'.  Continue anyway?" : IRWA)
// The message to display when warnDuplicateTitles is true 
// This is a dynamic string - text within <code>\${...}</code> will be evaluated as JS code
// when the message is displayed.
// @group i18nMessages
// @visibility external
//<
warnDuplicateTitlesMessage : "Another field already has the title '${fieldTitle}'.  Continue anyway?",

fieldTitleIsUnique : function (title) {
    var allFields = this.component ? this.component.getAllFields() : null,
        fields = allFields ? allFields.findAll({"title": title}) : null,
        isUnique = true
    ;

    if (fields && fields.length > 0) {
        for (var i=0; i<fields.length; i++) {
            if (fields[i].name != this.field.name) {
                isUnique = false;
                break;
            }
        }
    }

    return isUnique;
},

//> @method formulaBuilder.save()
// Call to finish working, test the formula and call 
// +link{FormulaBuilder.fireOnClose(), fireOnClose()}.  Called automatically
// when the Save button is clicked.
//
// @group formulaFields
// @visibility external
//<
save : function () {
    var result = this.testFunction();

    if (this.warnDuplicateTitles && !this.duplicateTitleAccepted) {
        var fieldTitle = this.getTitle();
        if (!this.fieldTitleIsUnique(fieldTitle)) {
            var msg = this.warnDuplicateTitlesMessage.evalDynamicString(this, { 
                fieldTitle: fieldTitle 
            });

            var _this = this;
            
            isc.confirm(msg,
                function (value) {
                    if (value) {
                        _this.duplicateTitleAccepted = true;
                        _this.delayCall("save");
                    } else {
                        _this.restartBuilder = false;
                    }
                }
            );
            return null;
        }
    }
    
    delete this.duplicateTitleAccepted;

    if (result.emptyTestValue) {
        isc.warn(this.invalidBlankPrompt.evalDynamicString(this, { builderType: this.builderTypeText }));
        return;
    } else if (result.failedGeneration || result.failedExecution) {
        isc.warn(this.invalidGeneratedFunctionPrompt.evalDynamicString(this, { builderType: this.builderTypeText }));
        return;
    }

    this.completeEditing(false);
},

// call this to finish working with the builder
completeEditing : function (cancelled, ignoreSaveCheck) {
    this.cancelled = cancelled;
    if (cancelled) {
        if (this.editMode && !ignoreSaveCheck) {
            if (this.getValue() != this.initialValue) {
                var _this = this;
                var message = this.saveConfirmationPrompt.evalDynamicString(this, { builderType: this.builderTypeText });
                isc.confirm(message, 
                    function (shouldSave) {
                        if (shouldSave) {
                            _this.save();
                        } else {
                            _this.completeEditing(true, true);
                        }
                    }
                );
                return;
            }
        }
    }
    if (this.helpWindow) this.hideHelpWindow();
    
    if (this.availableFields) {
        // restore the original field-order
        isc.FormulaBuilder.sortFields(this.availableFields, true);
        this.availableFields.clearProperty("originalOrder");
    }
    
    this.fireOnClose();
},

//>	@method	formulaBuilder.fireOnClose()	(A)
// Override to execute a callback function when the Formula is Cancelled or Saved.
// 
// @group formulaFields
// @visibility external
//<
fireOnClose : function () {},

//> @attr formulaBuilder.autoTest (boolean : true : IRWA)
// When set to true, automatically tests the formula by calling 
// +link{formulaBuilder.testFunction(), testFunction()} whenever typing into the 
// +link{formulaBuilder.formulaField, formulaField} pauses.
// <P>
// The default is true.
// 
// @group formulaFields
// @visibility external
//<
autoTest : true,

//> @attr formulaBuilder.autoTestDelay (integer : 200 : IRWA)
// When +link{formulaBuilder.autoTest} is true, this property indicates the delay in 
// milliseconds between a user pausing and +link{formulaBuilder.testFunction(), testFunction()}
// being called.
// <P>
// The default is 200 milliseconds.
// 
// @group formulaFields
// @visibility external
//<
autoTestDelay : 200,

//> @attr formulaBuilder.testRecord (Record : null : IRA)
// Record to use when testing the formula dynamically (if +link{formulaBuilder.autoTest} is enabled) or when
// showing samples of formula output.
// <P>
// If not specified, the selected record in the component that launched the FormulaBuilder will
// be used, or if there's no selection, the first visible row, or with no component, a dummy
// data row derived automatically from the provided DataSource.
//
// @group formulaFields
// @visibility external
//<

//> @attr formulaBuilder.samplePrompt (HTMLString : "<nobr>For record: \${title}</nobr><br><nobr>Output: \${output}</nobr>" : IRWA)
// This is a dynamic string - text within <code>\${...}</code> will be evaluated as JS code
// when the message is displayed.
// <P>
// Default value returns <P>
// <code>
// <i>For Record: + the value of the rows title-field <br>
// Output: + the result of he generated function<br>
// </i>
// </code>
// @group i18nMessages
// @visibility external
//<
samplePrompt : "<nobr>For Record: ${title}</nobr><br><nobr>Output: ${output}</nobr>",

//> @method formulaBuilder.getSamplePrompt()
// Evaluates and returns the dynamic +link{formulaBuilder.samplePrompt} string which is 
// displayed beneath the formulaField and updated when typing pauses.
// 
// @param (TestFunctionResult) The return value from a call to testFunction().
// @return (string) Caption displaying dynamic row-title and the result of the formula
// @group i18nMessages
// @visibility external
//<
getSamplePrompt : function (result) {
    // if there's no DS, we can't use getTitleField() - instead, just get the first key from
    // the record object and use the value of record[firstKey]
    
    var titleField = this.dataSource ? this.dataSource.getTitleField() : isc.firstKey(result.record),
        output = result.result != null ? result.result : 
            this.invalidBuilderPrompt.evalDynamicString(
                this, 
                { builderType: this.builderTypeText, 
                  errorText:result.errorText || this.defaultErrorText }),
        title = result.record[titleField];
    return this.samplePrompt.evalDynamicString(this, { title: title, output: output });
}

});

isc.FormulaBuilder.addClassMethods({

// 0 = A, 1 = B, etc.
mappingKeyForIndex : function (index) {
    // Use ascii table for range of A-Z, then repeat as AA, AB, etc.
    // A = 65, Z = 90
    var key = "",
        outerDiv = Math.floor(index/(26*27)),
        innerVal = index % (26*26),
        div = Math.floor( innerVal /26);

    if (outerDiv >= 1) key+= String.fromCharCode(65+(outerDiv-1));

    if (div >= 1) {
        if (outerDiv >= 1) {
            key += String.fromCharCode(65+(div-1));
            key += String.fromCharCode(65+(index-(26*27))%26);
        } else {
            key += String.fromCharCode(65+(div-1));
            key += String.fromCharCode(65+index%26);
        }
    } else {
        if (outerDiv >=1) {
            key += String.fromCharCode(65);
        }
        key += String.fromCharCode(65+index%26);
    }

    return key;
},

getFieldIdProperty : function (component) {
    return component ? component.fieldIdProperty : "name";
},

applyHeaderSpanTitles : function (builder, fields, spans, paramTitle) {
    if (!builder.showHeaderSpanTitles) return;
    spans = spans || builder.headerSpans;
    if (!spans) return;
    for (var i = 0; i < spans.length; i++) {
        var title = paramTitle || "";
        var span = spans[i];
        title += span.title + builder.spanTitleSeparator;
        if (span.spans) {
            this.applyHeaderSpanTitles(builder, fields, span.spans, title);
        } else if (span.fields) {  // ASSERT: Should always be one or the other
            for (var j = 0; j < span.fields.length; j++) {
                var fieldName = span.fields[j];
                for (var k = 0; k < fields.length; k++) {
                    if (fields[k].name == fieldName) {
                        fields[k].title = title + fields[k].title;
                        break;
                    }
                }
            }
        }
    }
},




sortFields : function (fields, direction) {
    fields.sortByProperty("mappingKey", direction, 
        function (item, propertyName, context) {
            var result = item[propertyName];
            if      (result.length == 1) result = '99' + result;
            else if (result.length == 2) result = '9'  + result;
            return result;
        });
},


handleKeyExp : function (text, key, mode, replace) {
    switch(mode) {
    case "braced":
        if (replace) return text.replaceAll("#{" + key + "}", replace);
        else         return text.indexOf   ("#{" + key + "}") >= 0;
    case "escaped": 
        var regex = new RegExp("#" + key + "(?=$|[^A-Z]+)", "g");
        if (replace) return text.replace(regex, replace);
        else         return regex.test(text);
    default:
    case "simple":
        var regex = new RegExp("(^|[^A-Z]+)" + key + "(?=$|[^A-Z]+)", "g");
        if (replace) return text.replace(regex, "$1" + replace);
        else         return regex.test(text);
    }
},

fieldIsUsed : function (formula, mappingKey, allowEscapedKeys) {
    if (!formula || !mappingKey) return false;

    if (this.handleKeyExp(formula, mappingKey)) return true;

    if (!allowEscapedKeys) return false;

    return this.handleKeyExp(formula, mappingKey, "escaped") ||
           this.handleKeyExp(formula, mappingKey, "braced");
},

// Get an array of those fields used in the formula-string
getFieldDetailsFromValue : function (formula, vars, fields, component, searchOption) {
    var used = isc.shallowClone(vars),
        fieldIdProperty = this.getFieldIdProperty(component),
        fieldDetails = { usedFields: [], missingFields: [] }
    ;
    for (var key in used) {
        var item = used[key],
            isUsed = this.fieldIsUsed(formula, key, searchOption),
            fieldID = fields.findIndex(fieldIdProperty, item);

        if (!fields[fieldID]) {
            if (!isUsed) {
                isc.logWarn("Field " + item + " is not in the list of available fields.  " +
                    "However, it is not used in the formula either - ignoring.");
                delete vars[key];
            } else {
                isc.logWarn("Field " + item + " is not in the list of available-fields");
                fieldDetails.missingFields.add(item);
            }
        } else {
            var field = isc.addProperties({}, fields[fieldID]);
            field.mappingKey = key;
            fieldDetails.usedFields.add(field);
        }
    }

    return fieldDetails;
},

// Test the formula by generating it's function and trying to run it
testFunction : function (field, userFormula, component, usedFields, testRecord) {
    var result = {};
    try {
        result.component = component;
        result.record = this.getTestRecord(component, usedFields, testRecord);
        if (!userFormula.text || userFormula.text == "") {
            result.emptyTestValue = true;
            return result;
        }
        result.jsFunction = this.generateFunction(userFormula, usedFields, component);
        result.result = result.jsFunction(result.record, component);
    } catch (err) {
        if (!result.jsFunction) result.failedGeneration = true;
        result.failedExecution = true;
        result.errorText = err.message;
    }
    return result;
},

getTestRecord : function (component, fields, testRecord) {
    var fieldIdProperty = this.getFieldIdProperty(component),
        record;

    if (testRecord) return testRecord;

    if (component) {
    	record = component.getSelectedRecord();

        if (!record) {
            if (component.body) {
                var visibleRows = component.body.getVisibleRows();
                record = visibleRows ? component.getRecord(visibleRows[0]) : component.data.get(0);
            } else {
                record = component.data.get(0);
            }
        }
    }
    if (!record && fields) {
        // no data to use, build a dummy record from the passed fields
        record = {};
        for (var i = 0; i < fields.length; i++) {
            var item = fields.get(i);
            
            if (item.userFormula) {
                item._generatedFormulaFunc = 
                    isc.FormulaBuilder.generateFunction(item.userFormula, fields, component);
                var func = item._generatedFormulaFunc;
                item.sortNormalizer = function (record, field, context) {
                    return func(record, context);
                };
            }
            
            if (item._generatedFormulaFunc) {
                // this is a formula - get the value of its _generatedFormulaFunc()
                isc.DataSource.setPathValue(record, item[fieldIdProperty], null,
                    item._generatedFormulaFunc(record, component));
            } else if (item.type) 
                if (isc.SimpleType.inheritsFrom(item.type, "integer") ||
                    isc.SimpleType.inheritsFrom(item.type, "float"))
                {
                    isc.DataSource.setPathValue(record, item[fieldIdProperty], null, 1);
                } else {
                    isc.DataSource.setPathValue(record, item[fieldIdProperty], null, item[fieldIdProperty]);
                }
            else {
                isc.DataSource.setPathValue(record, item[fieldIdProperty], null, item[fieldIdProperty]);
           }
        }
    }
    return record;
},


// Creates a function to wrap the calculation of a formula.  userFormula contains the
// properties held in field.userFormula.

generateFunction : function (userFormula, fields, component, allowNonNumeric, catchErrors) {

    // Default to using a try...catch block to catch errors
    // Note there are 2 cases we need to catch
    // - Syntax error in the function. We have to catch this here as the "new Function()"
    //   call will fail
    // - Logic error in the function. We catch that within the function itself
    if (catchErrors == null) catchErrors = true;

	var output = isc.SB.create(),
        formula = userFormula.text,
        fieldIdProperty = this.getFieldIdProperty(component),
        fieldDetails = this.getFieldDetailsFromValue(formula, userFormula.formulaVars, fields, 
                                                     component, userFormula.allowEscapedKeys),
        usedFields = fieldDetails.usedFields,
        missingFields = fieldDetails.missingFields
    ;

    isc.FormulaBuilder.sortFields(usedFields, false);

    if (missingFields.length == 0) {
        output.append("var nullVars = [];\n");

        if (usedFields.length > 0) {
            // script local vars for record-values
            for (var i = 0; i < usedFields.length; i++) {
                var item = usedFields.get(i),
                    mappingKey = item.mappingKey;
                // The array of field objects available here should have the 'mappingKey' 
                // and fieldIdProperty set.
                
                var fieldName = item[fieldIdProperty],
                    pathCode = "isc.DataSource.getPathValue(record,'" + fieldName + "', field)"
                ;

                
                var skipInSummary = item.userFormula && component &&
                    !component.shouldApplyUserFormulaAfterSummary(item)

                // Code to extract the appropriate value from the record based on 
                // fieldName.
                // Note that if possible we use the DBC._getFieldValue() method -- this
                // handles dataPaths, and "getAtomicType()" logic if present.
                // If we don't have a component, back off to getPathValue() which 
                // will still navigate nested objects using dataPath
                
                output.append("var ");
                output.append(
                    "field=component==null?null:component.getField('",fieldName,"');",
                    "if (field==null && component && component.completeFields)",
                        "field=component.completeFields.find('name','",fieldName,"');",
                    "var ", mappingKey,
                    // use getPathValue so we can handle being passed a dataPath to navigate a
                    // nested structure. Used by the RulesEngine / populate rule code-path.
                    //"=isc.DataSource.getPathValue(record,'", fieldName, "', field)\n;"

                    item.userFormula ? "=component" + 
                        (skipInSummary ? "&&!component._isSummaryRecord(record)" : "") +
                                  "?component.getFormulaFieldValue(field, record):" + pathCode
                        : item.userSummary ?
                        "=component?component.getSummaryFieldValue(field, record):" + pathCode
                        : "=" + pathCode,
                    "\n;"
                );
                
                output.append("if (", mappingKey, " == null || (component && ", 
                    mappingKey, " == component.badFormulaResultValue) || (!component && ", 
                    mappingKey, " == '.')) nullVars.add('", mappingKey, "');");

                if (userFormula.allowEscapedKeys) {
                    formula = this.handleKeyExp(formula, mappingKey, "escaped", mappingKey);
                    formula = this.handleKeyExp(formula, mappingKey, "braced", mappingKey);
                }
            }
            output.append("\n");
        }

        // script local vars for MathFunction-pointers
        var functions = isc.MathFunction.getRegisteredFunctions();
        if (functions && functions.length > 0) {
            output.append("var functions=isc.MathFunction.getRegisteredFunctionIndex(),\n");
            for (var i = 0; i < functions.length; i++) {
                var item = functions.get(i);
                output.append("        ");
                output.append(item.name, "=", "functions.", item.name, ".jsFunction");
                output.append(i == functions.length - 1 ? ";" : ",", "\n");
            }
            output.append("\n");
        }

        if (catchErrors) {
            output.append("try{\n"); 
        }

        // If NaN, use badFormulaResultValue
        output.append("var value=" , formula , ";");
        
        if (catchErrors) {
            var errorMessage = "Attempt to evaluate formulaFunction " + formula + 
                " failed. Error message:";

            output.append("\n} catch (e) { (component||isc).logWarn(",
                                errorMessage.asSource(true)," + e.message); }\n");
            
        }
        
        output.append(
            (allowNonNumeric ? null :
                
                "if (!isFinite(value) || nullVars.length > 0) " +
                "return (component && component.badFormulaResultValue) || '.'; "),
            "return value;");
    } else {
        this.logWarn("Formula failed due to missing fields: " + missingFields.join(", ") + ".");
        var result = (component && component.badFormulaResultValue) || ".";
        if (result) result = "'" + result + "'";
        output.append("return ", result, ";");
    }

	// return the wrapped function
    var content = output.release(false);
    //this.logWarn("content\n:" + content);
    var func;
    if (catchErrors) {
        try {
            func = isc._makeFunction("record,component", content);
        } catch (e) {
            this.logWarn("Error attempting to convert formula text '" + formula + 
                "' to a function:" + e.message);
            func = isc._makeFunction("record,component", "return null;");
        }
    } else {
        func = isc._makeFunction("record,component", content);
    }

	return func;

},

remapBadVars : function (badVars, vars, formula, missingMarker) {
    for (var i = 0; i < badVars.length; i++) {
        var key = badVars[i],
            text = formula.text || "";
        formula.text = this.handleKeyExp(text, key, "simple", missingMarker);
        if (formula.allowEscapedKeys) {
            formula.text = this.handleKeyExp(formula.text, key, "escaped", missingMarker);
            formula.text = this.handleKeyExp(formula.text, key, "braced",  missingMarker);
        }
        isc.logWarn("Formula variable " + key + " refers to missing field " + vars[key]);
        delete vars[key];
    }
}

});

// -----------------------------------------------------------------------------------------

//> @class SummaryBuilder 
// Shows an interface allowing a user to create or edit fields by typing simple
// format-strings into a text field.  The format-strings can include the values of other fields
// and additional text as required.
// <P>
// Available values for the format-string are determined by the DataSource fields, and are given
// simple single-letter aliases (such as "A", "B", ...) similar to column names in Excel.
// The set of available values is shown in the +link{formulaBuilder.fieldKey} as a simple
// mapping between the +link{dataSourceField.title,field title} and it's short name.
// <P>
// To include a field in the format-string, prefix it with a hash sign (#).
//
// @treeLocation Client Reference/Data Binding
// @group summaryFields
// @visibility external
//<
isc.ClassFactory.defineClass("SummaryBuilder", "FormulaBuilder");

isc.SummaryBuilder.addProperties({
// attributes

//> @attr summaryBuilder.builderTypeText (String : "Summary" : IR)
// Indicates whether to use "summary" or some other keyword in various captions and text
//
// @group i18nMessages
// @visibility external
//<
builderTypeText: "Summary",

fieldType:"text", 

//> @attr summaryBuilder.dataSource (DataSource or ID : null : IRW)
// @include formulaBuilder.dataSource
// @group summaryFields
// @visibility external
//<

//> @attr summaryBuilder.dataSources (Array of DataSource : null : IR)
// @include formulaBuilder.dataSources
// @group summaryFields
// @visibility rules
//<

//> @attr summaryBuilder.fields (Array of Field : null : IRW)
// DataSource providing the available fields for the SummaryBuilder.
// <P>
// By default the SummaryBuilder will include all fields.  Set +link{summaryBuilder.fields} to 
// override this.
//
// @group summaryFields
// @visibility external
//<

//> @attr summaryBuilder.editMode (boolean : false : IR)
// @include formulaBuilder.editMode
// @group summaryFields
// @visibility external
//<

//> @attr summaryBuilder.field (Field : null : IR)
// @include formulaBuilder.field
// @group summaryFields
// @visibility external
//<

//> @attr summaryBuilder.formulaField (AutoChild TextItem : null : IR)
// @include formulaBuilder.formulaField
// @group summaryFields
// @visibility external
//<

//> @attr summaryBuilder.titleField (AutoChild TextItem : null : IR)
// @include formulaBuilder.titleField
// @group summaryFields
// @visibility external
//<

//> @attr summaryBuilder.showHelpIcon (boolean : true : IR)
// @include formulaBuilder.showHelpIcon
// @group summaryFields
// @visibility external
//<

//> @attr summaryBuilder.helpIcon (AutoChild FormItemIcon : null : IRA)
// @include formulaBuilder.helpIcon
// @group summaryFields
// @visibility external
//<

//> @attr summaryBuilder.showAutoHideCheckBox (boolean : true : IR)
// @include formulaBuilder.showAutoHideCheckBox
// @group summaryFields
// @visibility external
//<

//> @attr summaryBuilder.autoHideCheckBox (AutoChild TextItem : null : IR)
// @include formulaBuilder.autoHideCheckBox
// @group summaryFields
// @visibility external
//<

//> @attr summaryBuilder.testRecord (Record : null : IRA)
// Record to use when showing sample output for the format string.
// <P>
// If not specified, the selected record in the component that launched the SummaryBuilder will
// be used, or if there's no selection, the first visible row, or with no component, a dummy
// data row derived automatically from the provided DataSource.
//
// @group formulaFields
// @visibility external
//<

//> @attr summaryBuilder.autoHideCheckBoxLabel (String : "Auto hide fields used in summary" : IRW)
// Text label for the checkbox that allows the user to automatically hide the
// fields used in the summary format.
//
// @group i18nMessages
// @visibility external
//<
autoHideCheckBoxLabel: "Auto hide fields used in Summary",

//> @attr summaryBuilder.helpTextIntro (String : "The following functions are available:" : IR)
// Text that appears in the hover from the +link{helpIcon}, as a pre-amble to the list of
// available format-tokens.
//
// @group i18nMessages
// @visibility external
//<
helpTextIntro: "Building Summary Columns",

// when true, allow #AA syntax for multi-char keys, as well as #{AA}
allowBasicMultiCharKeys: false

});

isc.SummaryBuilder.addMethods({

//> @method summaryBuilder.setSummary()
// Call to set the format-string in this SummaryBuilder.
// <P>
// Note that calling setSummary() will update the UI, generate the summary's function and 
// test it automatically.
//
// @param (String) The new format-string for the summary
// @group formulaFields
// @visibility external
//<
setSummary : function (newValue) {
    this.setValue(newValue);
},

//> @method summaryBuilder.getHelpText()
// Call to retrieve the text the SummaryBuilder would show by default for help, or override to
// provide alternate help text.
//
// @return (String) By default, the results of getHoverText()
// @group summaryFields
// @visibility external
//<

// set initialValue and then call this.Super to do the internal work
setInitialValue : function () {
    if (this.editMode && this.field.userSummary) {
        this.initialValue = this.field.userSummary.text;
        if (this.field.userSummary.allowBasicMultiCharKeys) {
            this.allowBasicMultiCharKeys = this.field.userSummary.allowBasicMultiCharKeys;
        }
    }
    this.initialValue = this.initialValue || "";
    this.setValue(this.initialValue);
},

getUniqueFieldName : function () {
    return this.getNewUniqueFieldName("summaryField");
},

// Override providing help-text specific to building Summary columns
getHoverText : function () {
	var output = isc.SB.create(),
            record = this.getTestRecord(), 
            fieldIdProperty = this.getFieldIdProperty(),
            fieldA = this.getFieldFromMappingKey("A"),
            fieldAName = fieldA[fieldIdProperty],
            fieldATitle = fieldA ? fieldA.title || fieldA.name : null,
            fieldB = this.getFieldFromMappingKey("B"),
            fieldBName = fieldB ? fieldB[fieldIdProperty] : null,
            fieldBTitle = fieldB ? fieldB.title || fieldB.name : null
        ;

    output.append("<b>", this.helpTextIntro, "</b> <P>");
    output.append("Summary columns are user-created fields that combine dynamic-values " +
        "from other fields in the current record with static text specified by the user.<P>");
    output.append("Dynamic-values are specified by prefixing a mapping-key from the table " +
        "opposite with #");
    if (this.getFields().length > 26) output.append(", or by using #{key} when the key " +
        "is 2 or more characters long,");
    output.append(" and everything else is copied directly into the output.<P>");

    if (this.dataSource) {
        output.append("For example, in the current DataSource, key <b>A</b> maps to field <i>",
            fieldATitle,"</i> and <b>B</b> is <i>", !fieldB ? "missing" : fieldBTitle, "</i>.<P>");
        output.append("So, if we enter the Summary format-string as:<P>",
            "<i>#A is relative to #B</i><P>", 
            "then example output using the current data would look like:<P>");
        
        if (record) {
            var fieldAValue, fieldBValue;
            if (!fieldB) fieldBValue = "{missing}";
            if (!this.component) {
                fieldAValue = record[fieldAName];
                if (fieldB) fieldBValue = record[fieldBName];
            } else {
                var component = this.component,
                    
                    includeHilitesA = 
                        component.shouldIncludeHiliteInSummaryField("sample",fieldAName);
                fieldAValue  = includeHilitesA ? 
                        component.getStandaloneFieldValue(record, fieldAName) : 
                        component.getFormattedValue(record, fieldAName,
                                 component.getRawValue(record, fieldAName));
                if (fieldB) {
                    var includeHilitesB = 
                            component.shouldIncludeHiliteInSummaryField("sample",fieldBName);
                    fieldBValue  = includeHilitesB ? 
                            component.getStandaloneFieldValue(record, fieldBName) : 
                            component.getFormattedValue(record, fieldBName,
                                     component.getRawValue(record, fieldBName));
                 }
            }  
            output.append("<i>", fieldAValue, " is relative to ", fieldBValue, "</i><P>");
        }
    }

    return output.release(false);
},

// When clicking on a record to insert a key value, always escape it.
insertEscapedKeys: true,

// Get an array of those fields available for use in the summary based on visibility.
getAvailableFields : function () {
    if (this.availableFields) return this.availableFields;

    var availableFields = this.availableFields = [],
        currentField = this.field,
        fields = this.getFields();

    if (!fields) return availableFields;

    for (var i = 0, j = 0; i < fields.getLength(); i++) {
        var item = isc.addProperties({}, fields.get(i));
        
        if (currentField) {
            if (currentField.name == item.name) continue;
            if (item.userSummary) {
                var usedVar = false,
                    vars = item.userSummary.summaryVars || {};
                for (var fieldKey in vars) {
                    if (vars[fieldKey] == currentField.name) {
                        usedVar = true;
                        break;
                    }
                }
                if (usedVar) continue;
            }
        }

        item.originalOrder = i;

        //if (!item.userSummary) {
            item.mappingKey = isc.FormulaBuilder.mappingKeyForIndex(j++);
            if (!item.title) item.title = isc.DataSource.getAutoTitle(item.name);
            availableFields.add(item);
        //}
    }

    // now, move field mappingKeys around according to those keys already used in the formula
    var vars = currentField && currentField.userSummary ? 
                currentField.userSummary.summaryVars : {}
    ;

    
    var badVars = [];
    for (var key in vars) {
        var mappedField = availableFields.find("mappingKey", key),
            actualField = availableFields.find("name", vars[key])
        ;
        if (actualField == null) { badVars.add(key); continue; }
        // if there's a field already associated with the key bound to the actual field,
        // give that field the key we originally wanted to assign to the actual field
        if (mappedField) mappedField.mappingKey = actualField.mappingKey;
        actualField.mappingKey = key;
    }
    if (badVars.length > 0) {
        var missingMarker = this.component && this.component.missingSummaryFieldValue || "-";
        isc.SummaryBuilder.remapBadVars(badVars, vars, currentField.userSummary, missingMarker);
    }

    isc.FormulaBuilder.sortFields(availableFields, true);
    isc.SummaryBuilder.applyHeaderSpanTitles(this, availableFields);
    return availableFields;
},

// Get an array of used-fields from those fields available for use in the Summary
getUsedFields : function (up) {

    var usedFields = [],
        formula = this.getValue(),
        allowBasicMultiCharKeys = this.allowBasicMultiCharKeys,
        availableFields = this.getAvailableFields().duplicate();


    isc.FormulaBuilder.sortFields(availableFields, !!up);

    for (var i = 0; i < availableFields.length; i++) {
        var item = availableFields.get(i);
        
        if (isc.SummaryBuilder.fieldIsUsed(formula, item.mappingKey, allowBasicMultiCharKeys)) {
            usedFields.add(item);
        }
    }
    return usedFields;
},

// return the complete set of properties for the builder-type, including functions
getCompleteValueObject : function () {
    var usedFields = this.getUsedFields(),
        func = this.generateFunction(),
        fieldIdProperty = this.getFieldIdProperty(),
        
        properties = {_generatedSummaryFunc: func,
            type: this.fieldType,
            userSummary : { text: this.getValue() }
        };

    if (this.allowBasicMultiCharKeys) properties.userSummary.allowBasicMultiCharKeys = true;

    if (usedFields && usedFields.length > 0) {
        properties.userSummary.summaryVars = {};
        for (var i = 0; i < usedFields.length; i++) {
            var item = usedFields.get(i);
            properties.userSummary.summaryVars[item.mappingKey] = 
                item[fieldIdProperty];
        }
    }

    return properties;
},

// return the basic set of properties for the builder-type, excluding field-title and functions
getBasicValueObject : function () {
    var usedFields = this.getUsedFields(),
        fieldIdProperty = this.getFieldIdProperty(),
        userSummary = { text: this.getValue(), summaryVars: {} };

    if (this.allowBasicMultiCharKeys) userSummary.allowBasicMultiCharKeys = true;

    for (var i=0; i<usedFields.length; i++) {
        var item = usedFields.get(i);
        userSummary.summaryVars[item.mappingKey] = item[fieldIdProperty];
    }

    return userSummary;    
},

// Call the ClassMethod to generate the function for this Format
generateFunction : function (){
    return this.getClass().generateFunction(this.getBasicValueObject(), this.getUsedFields(),
        this.component
    );
},

initWidget: function(){
    this.Super("initWidget", arguments);
}

//>	@method	summaryBuilder.fireOnClose()	(A)
// Override to execute a callback function when the Format is Cancelled or Saved.
// 
// @group summaryFields
// @visibility external
//<

//> @method summaryBuilder.save()
// @include formulaBuilder.save
// @visibility external
//<

});

isc.SummaryBuilder.addClassMethods({

// Test the format-string by generating it's function and trying to run it.
// "userSummary" is the properties in field.userSummary
testFunction : function (field, userSummary, component, usedFields, testRecord){
    var result = {},
        fieldIdProperty = this.getFieldIdProperty(component);
    try {
        result.component = component;
        result.record = this.getTestRecord(component, usedFields, testRecord);
        if (userSummary.text == "") {
            result.emptyTestValue = true;
            return result;
        }
        result.jsFunction = this.generateFunction(userSummary, usedFields, component);
        result.result = result.jsFunction(result.record, field[fieldIdProperty], component);

    } catch (err) {
        if (!result.jsFunction) result.failedGeneration = true;
        result.failedExecution = true;
        result.errorText = err.message;
    }
    return result;
},

fieldIsUsed : function (summary, mappingKey, allowBasicMultiCharKeys) {
    if (!summary || !mappingKey) return false;

    if (this.handleKeyExp(summary, mappingKey, "braced")) return true;

    if (mappingKey.length > 1 && !allowBasicMultiCharKeys) return false;

    return this.handleKeyExp(summary, mappingKey, "escaped");
},

// Create a function to produce a summary-value according to the format-string supplied
generateFunction : function (userSummary, fields, component) {
	var output = isc.SB.create(),
        format = userSummary.text,
        fieldIdProperty = this.getFieldIdProperty(component),
        allowBasicMultiCharKeys = userSummary.allowBasicMultiCharKeys,
        fieldDetails = this.getFieldDetailsFromValue(format, userSummary.summaryVars, fields,
                                                     component, allowBasicMultiCharKeys),
        usedFields = fieldDetails.usedFields,
        missingFields = fieldDetails.missingFields
    ;

    isc.FormulaBuilder.sortFields(usedFields, false);

    if (usedFields.length > 0) {
        // script local vars for record-values
        for (var i = 0; i < usedFields.length; i++) {
            var item = usedFields.get(i),
                fieldName = item[fieldIdProperty],
                pathCode = "isc.DataSource.getPathValue(record,'" + fieldName + "')"
            ;
            output.append(
                "var includeHilites = component ? ",
                    // "fieldName" local var within the generated function is the name of the 
                    // summary field itself (passed to the function as a param)
                    // "fieldName" var within this for... loop is the name of the used
                    // field, so we quote that to supply it as a hardcoded string within
                    // the generated function.
                    "component.shouldIncludeHiliteInSummaryField(fieldName,'",fieldName,"') :",
                    "false;\n"
            );
            
            output.append("var ");
            output.append(
                item.mappingKey,
                "= (component ?",
                    "(includeHilites ? ",
                        "component.getStandaloneFieldValue(record, '", fieldName, "') :",
                        "component.getFormattedValue(record, '",fieldName,
                             "',component.getRawValue(record,'",fieldName,"'))) :", 
                // No component:
                // use getPathValue so we can handle being passed a dataPath to navigate a
                // nested structure. Used by the RulesEngine / populate rule code-path.
                //" : isc.DataSource.getPathValue(record,'", fieldName, "'));"
                item.userFormula ? 
                    "component?component.getFormulaFieldValue(field, record):" + pathCode
                    : item.userSummary ? 
                        "component?component.getSummaryFieldValue(field, record):" + pathCode
                    : pathCode,
                ")\n"
                );
            output.append(";\n");

            // first replace tokens in the format #{key}, then in the format #key
            var replaceText = "'+" + item.mappingKey + "+'";
            format = this.handleKeyExp(format, item.mappingKey, "braced", replaceText);
            if (item.mappingKey.length == 1 || userSummary.allowBasicMultiCharKeys) {
                format = this.handleKeyExp(format, item.mappingKey, "escaped", replaceText);
            }
        }
        output.append("\n");
    }

    // Replace disallowed field aliases with component.missingSummaryFieldValue
    var missingMarker = component && component.missingSummaryFieldValue || "-";
    format = format.replace(/#(\{[A-Z][A-Z]?\}|[A-Z])/g, missingMarker);
    if (userSummary.allowBasicMultiCharKeys) {
        format = format.replace(/#([A-Z][A-Z]?)/g, missingMarker);
    }
        
    // ensure the format-string is properly formed following field-token replacement
    if (format.substr(0, 2) == "'+") { // usedField starts the string (strip leading '+)
        format = format.substr(2);
    } else if (format.substr(0, 1) != "'") { // otherwise start the string (prefix with ')
        format = "'" + format;
    }
    if (format.substr(format.length - 2) == "+'") { // usedField ends the string (strip trailing +')
        format = format.substr(0, format.length - 2);
    } else if (format.substr(format.length - 1) != "'") { // otherwise terminate the string
        format = format + "'";
    }

    output.append("return ", format, ";");

	// return the wrapped function
    return isc._makeFunction("record,fieldName,component", output.release(false));
},

remapBadVars : function (badVars, vars, summary, missingMarker) {
    for (var i = 0 ; i < badVars.length; i++) {
        var key = badVars[i],
            text = summary.text || "";
        summary.text = this.handleKeyExp(text, key, "braced", missingMarker);
        if (key.length == 1 || summary.allowBasicMultiCharKeys) {
            summary.text = this.handleKeyExp(summary.text, key, "escaped", missingMarker);
        }
        isc.logWarn("Summary variable " + key + " refers to missing field " + vars[key]);
        delete vars[key];
    }
}

});

