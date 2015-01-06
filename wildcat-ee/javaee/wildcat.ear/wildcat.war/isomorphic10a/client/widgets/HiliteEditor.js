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

 




// Hilite Rule
// ---------------------------------------------------------------------------------------
// Interface for defining and editing a single grid hilite Rule

//>	@class HiliteRule
// A widget for editing the criteria of a single +link{class:DataBoundComponent} hilite.  
// The default implementation presents a series of +link{class:FormItem, formItems}
// for selecting the various elements of a simple criterion and a foreground or background
// color.  To specify more complex criteria, specify both foreground and background colors or
// to apply the hilite to multiple fields, you can create an 
// +link{class:AdvancedHiliteEditor, advanced hilite rule}.
// <P>
// <i><b>Important Note:</b> this class should not be used directly - it is exposed purely for
// +link{group:i18nMessages, i18n reasons.}</i>
//
// @treeLocation Client Reference/Grids/ListGrid
// @visibility external
//<
isc.defineClass("HiliteRule", "HLayout");

isc.HiliteRule.addProperties({
    height: 1,
    overflow: "visible",

    //> @attr hiliteRule.clause (AutoChild FilterClause : null : IR)
    // AutoChild +link{class:FilterClause} displaying the +link{class:FormItem, formItems} used to 
    // specify the criteria for this HiliteRule.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>hiliteRule.clauseProperties</code>.
    //
    // @visibility external
    //<
    clauseConstructor: "FilterClause",

	clauseDefaults: {
		fieldPickerWidth: "*",
        overflow: "visible",
		
		clauseDefaults: {
            // don't disallow editing of canEdit:false, or canFilter:false fields.
            canEditField:function () {
                return true;
            }
        },
		operatorPickerWidth: 140,
		valueItemWidth: 130,
        excludeNonFilterableFields: false,

        // If a field has a specified displayField with no explicit optionDataSource,
        // pick up the rootDataSource (set up in DBC.editHilites())
        getDefaultOptionDataSource : function (field) {
            if (this.creator && this.creator.rootDataSource) {
                return this.creator.rootDataSource;
            }
            var ds = this.dataSource;
            // Always avoid attempting to fetch against the hiliteCriteria DS
            if (ds && ds.isHiliteCriteriaDS) return null;
            return ds;
        }
	},

    //> @attr hiliteRule.hiliteForm (AutoChild DynamicForm : null : IR)
    // AutoChild +link{class:DynamicForm} displaying the +link{class:FormItem, formItems} used to 
    // specify the hiliting properties of this rule.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>hiliteRule.hiliteFormProperties</code>.
    //
    // @visibility external
    //<
    hiliteFormDefaults: {
        _constructor: "DynamicForm",
        numCols: 6,
        colWidths: [60, 60, 60, 60, 60, 40],
        width: 300
    },
    
    //> @attr hiliteRule.colorFieldTitle (string : "Color" : IR) 
    // The title for the Color picker field.
    //
    // @group i18nMessages 
    // @visibility external
    //<
    colorFieldTitle: "Color",

    advancedClauseLayoutDefaults: {
        _constructor: "HLayout",
        height: 1,
        overflow: "visible"
    },

    //> @attr hiliteRule.advancedClauseLabel (AutoChild Label : null : IR)
    // AutoChild +link{class:Label} displaying the human-readable description of an advanced
    // hilite-rule.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>hiliteRule.advancedClauseLabelProperties</code>.
    //
    // @visibility external
    //<
    advancedClauseLabelDefaults: {
        _constructor: "Label",
		autoParent: "advancedClauseLayout",
        width: "*",
        overflow: "hidden",
        height: 18,
        valign: "center",
        wrap: false,
        padding: 1
    },

    //> @attr hiliteRule.advancedClauseEditButton (AutoChild ImgButton : null : IR)
    // AutoChild +link{class:ImgButton} displayed by an advanced hilite-rule and used to open
    // it for editing in an +link{class:AdvancedHiliteEditor, advanced hilite editor}.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>hiliteRule.advancedClauseEditButtonProperties</code>.
    //
    // @visibility external
    //<
    advancedClauseEditButtonDefaults: {
        _constructor: "ImgButton",
		autoParent: "advancedClauseLayout",
        width: 18, height: 18, layoutAlign: "center",
        src: "[SKINIMG]/actions/edit.png", 
        showRollOver:false, showDown:false, showDisabled:false, 
        click: function () { this.creator.editAdvancedRule(); }
    },

    //> @attr HiliteRule.showRemoveButton (boolean : true : IR) 
    // If true, show a +link{hiliteRule.removeButton, button} for this HiliteRule, allowing it 
    // to be removed. 
    //
    // @visibility external
    //<
    showRemoveButton:true,

    //> @attr hiliteRule.removeButtonPrompt (string : "Remove" : IR) 
    // The hover prompt text for the +link{hiliteRule.removeButton, remove button}.
    //
    // @group i18nMessages 
    // @visibility external
    //<
    removeButtonPrompt: "Remove",

    //> @attr hiliteRule.removeButton (AutoChild ImgButton : null : IR) 
    // The Hilite removal ImgButton that appears before this Hilite if +link{showRemoveButton} 
    // is set.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>hiliteRule.removeButtonProperties</code>.
    //
    // @visibility external
    //<
    removeButtonDefaults : {
        _constructor:isc.ImgButton,
        width:18, height:18, layoutAlign:"center",
        src:"[SKIN]/actions/remove.png",
        showRollOver:false, showDown:false, showDisabled:false, 
        hoverWidth:80,
        click: function () { this.creator.remove(); }
    },

    //> @attr hiliteRule.foregroundColorTitle (string : "Text" : IR)
    // The +link{FormItem.title,title} of the 'Text' color picker.
    //
    // @group i18nMessages 
    // @visibility external
    //<
    foregroundColorTitle: "Text",

    //> @attr hiliteRule.backgroundColorTitle (string : "Background" : IR) 
    // The +link{FormItem.title,title} of the 'Background' color picker.
    //
    // @group i18nMessages 
    // @visibility external
    //<
    backgroundColorTitle: "Background",

    //> @attr hiliteRule.iconFieldTitle (string : "Icon" : IR)
    // The +link{FormItem.title,title} of the 'Icon' picker.
    //
    // @group i18nMessages 
    // @visibility external
    //<
    iconFieldTitle: "Icon",

    
    //> @attr hiliteRule.iconField (AutoChild SelectItem : null : IR)
    // The 'Icon' picker field.
    // <p>
    // +link{iconFieldTitle} is a +link{group:autoChildUsage,passthrough} for the picker's
    // +link{FormItem.title,title}.
    //<
    iconFieldDefaults: {
        type: "SelectItem",
        width: "*",
        defaultValue: "",
        // Always allow a blank value ... we'll add others
        valueMap: {"": ""},
        valueIcons: {"": "[SKINIMG]/blank.gif"},
        // Disable by default -- we'll enable if a real choice is added
        disabled: true,
        showIf : function () {
            return !this.isDisabled();
        }
    }
});

isc.HiliteRule.addMethods({

    initWidget: function () {
    
        if (!this.isAdvanced && this.hilite) {
            var criteria = this.hilite.criteria;
            if (criteria && criteria.criteria && isc.isAn.Array(criteria.criteria))
                // the criterion we were passed is really an advancedCriteria - switch on 
                // this.isAdvanced so we show an appropriate UI
                this.isAdvanced = true;
        }
        if (isc.isA.String(this.dataSource)) this.dataSource = isc.DS.getDataSource(this.dataSource);

        // make sure cssText and textColor/backgroundColor attributes are in sync
        if (this.hilite) this.checkHiliteProperties(this.hilite);

        if (this.isAdvanced) {
            // need to show a removeButton, label and editButton here instead of an isc.FilterClause
            var description = isc.DataSource.getAdvancedCriteriaDescription(
                this.hilite.criteria, 
                this.dataSource
            );
            var missingField = (description.indexOf(isc.FilterBuilder.missingFieldPrompt) >= 0);

            this.membersMargin = 2;
			this.addAutoChild("advancedClauseLayout");
            this.addAutoChild("removeButton", 
                { 
                    autoParent: "advancedClauseLayout" 
                }
            );
            this.addAutoChild("advancedClauseLabel", 
                {
                    contents: description,
                    prompt: description,
                    disabled: missingField ? true : false
                }
            );
            this.addAutoChild("advancedClauseEditButton", { disabled: missingField ? true : false }); 
        } else {
            var dsField = this.dataSource.getField(this.fieldName);
            if (dsField == null) dsField = this.dataSource.getFieldForDataPath(this.fieldName);
            var missingField = (dsField == null);
            this.addAutoChild("clause", 
                { 
                    dataSource: this.dataSource,
                    fieldDataSource: this.fieldDataSource,
                    field: this.dataSource.getField(this.fieldName),
                    fieldName: this.fieldName,
                    criterion: this.hilite ? this.hilite.criteria : null,
                    showRemoveButton: this.showRemoveButton,
                    missingField: missingField,
                    remove : function () {
                        this.creator.remove();
                    }
                }
            );
            this.addMember(this.clause);
            this.addAutoChild("hiliteForm", { disabled: missingField ? true : false,         
                items: [
                    { name: "textColor", title: this.foregroundColorTitle, type: "ColorItem", 
                        width: "*" 
                    },
                    { name: "backgroundColor", title: this.backgroundColorTitle, 
                        type: "ColorItem", width: "*" 
                    }
                ]
            });
            if (this.hilite) {
                this.hiliteForm.setValues(
                    { 
                        textColor: this.hilite.textColor,
                        backgroundColor: this.hilite.backgroundColor
                    }
                );
            }
            
            var field = this.clause.field;
            if (field && field.type == "boolean") {
                var item = this.hiliteForm.getItem("textColor");
                item.disable();
            }

            var hiliteIconItem = isc.addProperties({}, this.iconFieldDefaults, this.iconFieldProperties, {
                name: "icon",
                title: this.iconFieldTitle
            });

            // Add hiliteIcons to valueMap
            if (this.hiliteIcons) {
                this.hiliteIcons.map(function (icon) {
                    hiliteIconItem.disabled = false;
                    // Map all values to "", to show the icon only. There may be a more elegant
                    // way to do this with pickListProperties.
                    hiliteIconItem.valueMap[icon] = "";
                    // This actually provides the icon for the select item
                    hiliteIconItem.valueIcons[icon] = icon;
                });
            }

            // Add this.hilite.icon to valueMap if not already there
            var icon = this.hilite ? this.hilite.icon : null;
            if (icon && !hiliteIconItem.valueMap[icon]) {
                hiliteIconItem.disabled = false;
                hiliteIconItem.valueMap[icon] = "";
                hiliteIconItem.valueIcons[icon] = icon;
            }

            this.hiliteForm.addItem(hiliteIconItem);
            this.hiliteForm.setValue("icon", this.hilite ? this.hilite.icon : "");

            this.addMember(this.hiliteForm);
            this.iconField = this.hiliteForm.getField("icon");
        }
    },

    checkHiliteProperties : function (hilite) {
        if (!hilite) return;
        
        if (hilite.cssText) {
            // the hilite has cssText - ensure it coincides with the direct textColor and
            //  backgroundColor attributes
            var cssElements = hilite.cssText.split(";");

            for (var i=0; i<cssElements.length; i++) {
                var item = cssElements[i],
                    parts = item.split(":")
                ;

                if (parts[0] == "textColor" && !hilite.textColor)
                    hilite.textColor = parts[1];
                else if (parts[0] == "backgroundColor" && !hilite.backgroundColor)
                    hilite.backgroundColor = parts[1];
            }
        } else if (hilite.textColor || hilite.backgroundColor) {
            // no cssText but color attributes are set - build cssText now
            hilite.cssText = "";
            if (hilite.textColor) 
                hilite.cssText += "color:" + hilite.textColor + ";";
            if (hilite.backgroundColor) 
                hilite.cssText += "background-color:" + hilite.backgroundColor + ";";
            
            //alert(hilite.cssText);
        }
    },
    
//> @method hiliteRule.remove()
// Remove this HiliteRule.  Default implementation calls markForDestroy(). 
//
// @visibility external
//<
    remove : function () {
        this.markForDestroy();
    },

//> @method hiliteRule.getHilite()
// Return the hilite definition being edited, including criteria and hilite properties.
//
// @return (Hilite) the hilite 
// @visibility external
//<
    getHilite : function () {

        if (this.isAdvanced) {
            // externally edited in advanced editor
            return this.hilite;
        }
        
        if (this.missingField) return this.hilite;
        
        var hilite = this.hilite = 
                isc.addProperties(this.hilite || {}, { fieldName: this.fieldName }),
            icon = this.hiliteForm.getValue("icon"),
            textColorValue = this.hiliteForm.getValue("textColor"),
            backgroundColorValue = this.hiliteForm.getValue("backgroundColor"),
            criterion = this.clause.getCriterion()
        ;

        if (!criterion && !icon && !textColorValue && !backgroundColorValue) {
            return null;
        }

        // always mark the criteria as advanced, since it always will be
        if (criterion && !criterion._constructor) criterion._constructor = "AdvancedCriteria";
        hilite.criteria = criterion;
        hilite.icon = icon;

        // Note: we need to be idempotent because we're scribbling on a cached object, so
        // multiple calls to getHilite() with e.g. the user twiddling the foreground/background
        // pulldown in between need to return correct data, so be careful here.
        if (textColorValue != null || backgroundColorValue != null) hilite.cssText = "";
        if (textColorValue != null) {
            hilite.textColor = textColorValue;
            hilite.cssText += "color:" + textColorValue + ";";
        } 
        if (backgroundColorValue != null) {
            hilite.backgroundColor = backgroundColorValue;
            hilite.cssText += "background-color:" + backgroundColorValue + ";";
        }

        if (this.hilite && this.hilite.id) hilite.id = this.hilite.id;

        return hilite;
    },

//> @method hiliteRule.editAdvancedRule()
// Show an +link{class:AdvancedHiliteEditor} to edit this advanced rule.
//
// @visibility hiliteEditor
//<
    editAdvancedRule : function () {
        var callback = this.getID()+".editAdvancedRuleReply(hilite)";
        this.advancedHiliteDialog = isc.Window.create({
            title: isc.AdvancedHiliteEditor.getInstanceProperty("title"),
            width: Math.round(isc.Page.getWidth()/2),
            height: 1,
            isModal: true,
            showModalMask: true,
            showResizer: true,
            autoSize: true,
            autoCenter: true,
            items: [
                isc.AdvancedHiliteEditor.create({
                    width: "100%", height: "100%",
                    dataSource: this.fieldDataSource ? null : this.dataSource,
                    fieldDataSource: this.fieldDataSource,
                    // inherit rootDataSource - used as default optionDataSource
                    rootDataSource:this.rootDataSource,                    
                    hilite: this.hilite,
                    hiliteIcons: this.hiliteIcons,
                    hiliteCanReplaceValue: this.hiliteCanReplaceValue,
                    hiliteReplaceValueFieldTitle: this.hiliteReplaceValueFieldTitle,
                    callback: callback
                })
            ]
        });

        this.advancedHiliteDialog.show();
    },

    editAdvancedRuleReply : function (hilite) {
        this.advancedHiliteDialog.hide();
        this.advancedHiliteDialog.markForDestroy();
        
        if (hilite) {

            this.hilite = hilite;

            var description = isc.DataSource.getAdvancedCriteriaDescription(
                this.hilite.criteria, 
                this.dataSource
            );

            this.advancedClauseLabel.setContents(description);
            this.advancedClauseLabel.setPrompt(description);
        }
    }

});


// Hilite Editor
// ---------------------------------------------------------------------------------------
// Interface for defining and editing grid hilites 

//>	@class HiliteEditor
// A widget for defining and editing a set of +link{class:HiliteRule, hilite rules} for use by 
// +link{class:DataBoundComponent, dataBoundComponents}.  Presents a list of available fields 
// and allows editing of simple hilites directly and more complex hilites via  
// +link{class:AdvancedHiliteEditor}s. 
// <P>
// <i><b>Important Note:</b> this class should not be used directly - it is exposed purely for
// +link{group:i18nMessages, i18n reasons.}</i>
//
// @treeLocation Client Reference/Grids/ListGrid
// @visibility external
//<
isc.defineClass("HiliteEditor", "VLayout");

isc.HiliteEditor.addProperties({

    membersMargin: 5,

    mainLayoutDefaults : {
        _constructor:"HLayout",
        membersMargin: 5,
        overflow: "visible"
    },

    fieldLayoutDefaults: {
        _constructor: "VLayout",
        autoParent: "mainLayout",
        width: 1,
        overflow: "visible"
    },
    
    //> @attr hiliteEditor.fieldList (AutoChild ListGrid : null : IR)
    // AutoChild +link{class:ListGrid} showing the list of fields to create hilites for.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>hiliteEditor.fieldListProperties</code>.
    //
    // @visibility external
    //<
    fieldListDefaults: {
        _constructor: "ListGrid",
        height: "*",
        autoFitData: "both",
        autoFitMaxWidth: 300,
        autoFitMaxRecords: 5,
        selectionType: "none",
        showRollOver: true,
        width: 1,
        autoFitFieldWidths: true,
        autoFitWidthApproach: "both",
        autoFitExpandField: "title",
        autoParent: "fieldLayout",
        showHoverComponents: true,
        detailField: "prompt",
        hoverMode: "detailField",
        canHover: true,
        recordClick : function (grid, record) {
            this.creator.addRule(record);
        }
    },

    //> @attr hiliteEditor.availableFieldsColumnTitle (string : "Available Fields" : IR) 
    // The title for the 'Available Fields' column in the 
    // +link{hiliteEditor.fieldList, fieldList}.
    //
    // @group i18nMessages 
    // @visibility external
    //<
    availableFieldsColumnTitle: "Available Fields",

    ruleLayoutDefaults : {
        _constructor:"VLayout",
        membersMargin: 1,
        padding: 1,
        overflow: "auto",
        autoParent: "mainLayout"
    },

    //> @attr hiliteEditor.hiliteIcons (Array of SCImgURL : null : IRW)
    // Specifies a list of icons that can be used in +link{Hilite,hilites}.
    // <P>
    // <code>hiliteIcons</code> should be specified as an Array of +link{SCImgURL}.
    // When present, +link{class:HiliteRule}s
    // will offer the user a drop down for picking one of these icons.
    // <P>
    // If the user picks an icon, the created hiliting rule will have +link{hilite.icon} set to 
    // the chosen icon.  +link{listGridField.hiliteIconPosition} controls where the icon will 
    // appear for that field -- the default is that it appears in front of the normal cell content.
    //
    // @visibility external
    //<

    //> @attr hiliteEditor.hiliteRule (AutoChild HiliteRule : null : IR)
    // AutoChild +link{class:HiliteRule} used to create new simple hilites.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>hiliteEditor.hiliteRuleProperties</code>.
    //
    // @visibility external
    //<
    hiliteRuleDefaults: {
        _constructor: "HiliteRule"
    },

    hiliteButtonsDefaults : {
        layoutAlign: "right",
        _constructor:"HLayout", 
        membersMargin:8, height:1
    },

    hiliteButtonsSpacerDefaults : {
        _constructor: "LayoutSpacer",
        width: "*",
        autoParent: "hiliteButtons" 
    },

    //> @attr hiliteEditor.addAdvancedRuleButton (AutoChild StatefulCanvas : null : IR)
    // AutoChild +link{class:IButton} that opens an +link{AdvancedHiliteEditor} to create a new
    // advanced rule.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>hiliteEditor.addAdvancedRuleButtonProperties</code>.
    //
    // @visibility external
    //<
    addAdvancedRuleButtonDefaults: {
        _constructor: "IAutoFitButton",
        autoParent: "hiliteButtons",
        align: "center",
        height: 22,
        click: function () {
            this.creator.addAdvancedRule();
        }
    },

    //> @attr hiliteEditor.addAdvancedRuleButtonTitle (string : "Add Advanced Rule" : IR) 
    // The title text for the +link{hiliteEditor.addAdvancedRuleButton, add advanced rule}
    // button.
    //
    // @group i18nMessages 
    // @visibility external
    //<
    addAdvancedRuleButtonTitle: "Add Advanced Rule",

    //> @attr hiliteEditor.saveButton (AutoChild StatefulCanvas : null : IR)
    // AutoChild +link{class:ImgButton} that saves the hilites in this editor and fires the 
    // +link{hiliteEditor.callback, callback}.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>hiliteEditor.saveButtonProperties</code>.
    //
    // @visibility external
    //<
    saveButtonDefaults : {
        _constructor:"IAutoFitButton", 
        autoParent:"hiliteButtons",
        click : function () {
            this.creator.saveHilites();
        }
    },

    //> @attr hiliteEditor.saveButtonTitle (string : "Save" : IR) 
    // The title text for the +link{hiliteEditor.saveButton, saveButton}.
    //
    // @group i18nMessages 
    // @visibility external
    //<
    saveButtonTitle: "Save",

    //> @attr hiliteEditor.cancelButton (AutoChild StatefulCanvas : null : IR)
    // AutoChild +link{class:ImgButton} that cancels this editor without saving 
    // any changes, firing the +link{hiliteEditor.callback, callback} with a null
    // parameter.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>hiliteEditor.cancelButtonProperties</code>.
    //
    // @visibility external
    //<
    cancelButtonDefaults : {
        _constructor:"IAutoFitButton", 
        autoParent:"hiliteButtons",
        click : function () {
            this.creator.completeEditing();
        }
    },

    //> @attr hiliteEditor.cancelButtonTitle (string : "Cancel" : IR) 
    // The title text for the +link{hiliteEditor.cancelButton, cancel button}.
    //
    // @group i18nMessages 
    // @visibility external
    //<
    cancelButtonTitle: "Cancel",

    // overall layout
    // ---------------------------------------------------------------------------------------
    defaultWidth:900, defaultHeight:300

    //> @attr hiliteEditor.callback (Callback : null : IR)
    // The callback to fire when +link{hiliteEditor.saveHilites} is called.
    //
    // @visibility external
    //<
});

isc.HiliteEditor.addMethods({

    initWidget : function () {
    
        this.Super("initWidget", arguments);
        if (isc.Browser.isDesktop) {
            this.addAutoChildren(["mainLayout", "fieldLayout"]);
            this.addAutoChild("fieldList", { fields: [
                { name: "name", showIf: "false" },
                { name: "title", title: this.availableFieldsColumnTitle}
            ]});
        } else {
            var _this = this,
                pickListProperties = isc.addProperties(
                    {
                        showFilterEditor: true
                    },
                    this.fieldListProperties, this.fieldListDefaults,
                    {
                        recordClick : function (grid, record) {
                            _this.addRule(record);
                            this.hide();
                        }
                    }
                );
            this.fieldList = isc.DynamicForm.create({
                fields: [
                    {name: "fieldList", showTitle: false, type: "SelectItem", pickListProperties: pickListProperties}
                ]
            });
            this.addMember(this.fieldList);
            this.addAutoChildren(["mainLayout", "fieldLayout"]);
        }

        this.addAutoChildren(["ruleLayout", "hiliteButtons", "hiliteButtonsSpacer"]);

        this.addAutoChild("addAdvancedRuleButton", { title: this.addAdvancedRuleButtonTitle});
        this.addAutoChild("saveButton", { title: this.saveButtonTitle});
        this.addAutoChild("cancelButton", { title: this.cancelButtonTitle});

        this.setDataSource(this.dataSource);

        this.setHilites(this.hilites);
    },

    setHiliteIcons : function (hiliteIcons) {
        this.hiliteIcons = hiliteIcons;
    },

    setDataSource : function (ds) {
        this.dataSource = ds;
        if (this.fieldDataSource && !this.fieldDataSource._autoDerived) {
            this.setupFieldList();
        } else if (this.dataSource) {
            this.getClientOnlyFieldDS();
        } else {
            this.logWarn("No DataSource present, can't edit hilites");
        }
        this.fieldList.markForRedraw();
    },

    setFieldDataSource : function (ds) {
        this.fieldDataSource = ds;
        this.setupFieldList();
    },

    // override point - if showFieldList is false, override this method to set up data for 
    // whatever replacement list is provided
    setupFieldList : function () {
        var fieldList = this.fieldList;
        var fields = [
            { name: "name", showIf: "false" },
            { name: "title", title: this.availableFieldsColumnTitle },
            { name: "type", showIf: "false" },
            { name: "showInSimpleEditor", showIf: "false" }
        ];
        if (isc.ListGrid.isA(fieldList)) {
            fieldList.showFilterEditor = true;
            fieldList.setDataSource(this.fieldDataSource);
            fieldList.setFields(fields);
        } else {
            fieldList = this.fieldList.getField("fieldList");
            fieldList.setOptionDataSource(this.fieldDataSource);
            fieldList.pickListFields = fields;
        }
        var crit = { _constructor: "AdvancedCriteria", operator: "or", 
            criteria: [
                { fieldName: "showInSimpleEditor", operator: "equals", value: true },
                { fieldName: "showInSimpleEditor", operator: "isNull" }
            ]
        };
        fieldList.fetchData(crit);
    },

    // This dataSource is used to pick fields to which hilights can be applied
    getClientOnlyFieldDS : function () {
        var sourceFields = isc.getValues(this.dataSource.getFields());
        var fields = [];
        for (var i = 0; i < sourceFields.length; i++) {
            var field = sourceFields[i];
            // Skip hidden fields or inherited fields
            
            if (!field.hidden && field.canHilite != false && 
                    this.dataSource.fields[field.name] != null
                )
            {
                if (field.summaryTitle != null) {
                     field = isc.addProperties({}, field, {title: field.summaryTitle});
                }
                fields.add(field);
            }
        }
        this.fieldDataSource = isc.DataSource.create({
            _autoDerived:true,
            // Hang a flag on this DS to simplify debugging
            isClientOnlyFieldDS:true,
            fields: [
                { name: "name", showIf: "false" },
                { name: "title", title: this.availableFieldsColumnTitle },
                { name: "type", showIf: "false" },
                { name: "showInSimpleEditor", type: "boolean", showIf: "false" }
            ],
            cacheData: fields,
            clientOnly: true
        });

        this.setupFieldList();
    },
    
    //> @method hiliteEditor.addRule()
    // Adds a new HiliteRule for a passed record.
    //
    // @param record (Record) the record containing the name or datapath of the field to which 
    //                        this hilite applies
    // @visibility hiliteEditor
    //<
    addRule : function (record) {
        var newRule = this.createAutoChild("hiliteRule", {
            // pass the rootDataSource through, if present, so 
            // fields with a specified displayField and no explicit optionDataSource
            // can retrieve options
            rootDataSource:this.rootDataSource,
            width: "100%",
            
            fieldName: record.dataPath || record.name,
            dataSource: this.dataSource,
            fieldDataSource: this.fieldDataSource,
            hiliteIcons: this.hiliteIcons
        });
      
        this.showNewHilite(newRule);
    },

    //> @method hiliteEditor.removeRule()
    // Removes the passed +link{class:HiliteRule, HiliteRule} from this editor.
    //
    // @param hiliteRule (HiliteRule) the hiliteRule to remove
    // @visibility external
    //<
    removeRule : function (hiliteRule) {
        if (isc.isA.Number(hiliteRule)) hiliteRule = this.ruleLayout.getMember(hiliteRule);
        if (!isc.isA.HiliteRule(hiliteRule)) return;

        this.ruleLayout.members.remove(hiliteRule);
        hiliteRule.destroy();
    },

    showNewHilite : function (newRule) {
        this.ruleLayout.addMember(newRule);
    },

    //> @method hiliteEditor.addAdvancedRule()
    // Shows a +link{class:AdvancedHiliteEditor, dialog} to add a new Advanced 
    // +link{class:HiliteRule,HiliteRule}.
    //
    // @visibility hiliteEditor
    //<
    addAdvancedRule : function () {
        var callback = this.getID()+".addAdvancedRuleReply(hilite)";
        this.advancedHiliteDialog = isc.Window.create({
            title: isc.AdvancedHiliteEditor.getInstanceProperty("title"),
            width: Math.round(isc.Page.getWidth()/2),
            height: 1,
            isModal: true,
            showModalMask: true,
            showResizer: true,
            canDragResize: true,
            autoSize: true,
            autoCenter: true,
            items: [
                isc.AdvancedHiliteEditor.create({
                    width: "100%", height: "100%",
                    // inherit rootDataSource (used for default optionDataSource)
                    rootDataSource: this.rootDataSource,                    
                    // for the advanced editor, we want to show inherited fields as well local
                    // fields because the user may need to specify criteria using fields that
                    // are not defined in the immediate DS (but that target a visible locally
                    // defined field with a hilite).  So only pass the fieldDataSource to the
                    // AdvancedHiliteEditor if one was explicitly passed to us as opposed to
                    // one we auto-derived
                    dataSource: this.fieldDataSource && !this.fieldDataSource._autoDerived ? null : this.dataSource,
                    fieldDataSource: this.fieldDataSource,
                    hiliteIcons: this.hiliteIcons,
                    hiliteCanReplaceValue: this.hiliteCanReplaceValue,
                    hiliteReplaceValueFieldTitle: this.hiliteReplaceValueFieldTitle,
                    callback: callback
                })
            ]
        });

        this.advancedHiliteDialog.show();
    },

    addAdvancedRuleReply : function (hilite) {
        this.advancedHiliteDialog.hide();
        this.advancedHiliteDialog.markForDestroy();

        if (!hilite) return;

        var newRule = this.createAutoChild("hiliteRule", {
            rootDataSource:this.rootDataSource,        
            width: "100%",
            isAdvanced: true,
            dataSource: this.dataSource,
            fieldDataSource: this.fieldDataSource,
            fieldName: hilite.fieldName,
            hilite : hilite,
            hiliteIcons: this.hiliteIcons,
            hiliteCanReplaceValue: this.hiliteCanReplaceValue,
            hiliteReplaceValueFieldTitle: this.hiliteReplaceValueFieldTitle
        });

        this.showNewHilite(newRule);
    },

    //> @method hiliteEditor.clearHilites()
    // Clear all Hilites from the editor.
    //
    // @visibility external
    //<
    clearHilites : function () {
        for (var i = this.ruleLayout.members.length-1; i >= 0; i--)
            this.removeRule(this.ruleLayout.getMember(i));
    },
    
    //> @method hiliteEditor.setHilites()
    // Initialize this editor with a set of Hilites.
    //
    // @param hilites (Array of Hilite) the array of hilite objects to apply
    // @visibility external
    //<
    setHilites : function (hilites) {

        hilites = this.hilites = hilites || [];

        this._loadedHilites = this.hilites.duplicate();

        for (var i=0; i<hilites.length; i++) {
            var hilite = hilites[i],
                css = hilite.cssText ? hilite.cssText.replaceAll(" ", "") : null,
                cssArray = css ? css.split(";") : [""],
                fgColor = hilite.textColor,
                bgColor = hilite.backgroundColor
            ;

            if (css) {
                for (var j=0; j<cssArray.length; j++) { 
                    var setting = cssArray[j],
                        subArray = setting && setting.length > 0 ? setting.split(":") : []
                    ;
                    if (subArray && subArray[0] == "background-color" && !bgColor) {
                        hilite.backgroundColor = subArray[1];
                    }
                    if (subArray && subArray[0] == "color" && !fgColor) {
                        hilite.textColor = subArray[1];
                    }
                }
            }

            var missingField = false;
            if (this.dataSource) {
                var desc = isc.DataSource.getCriterionDescription(hilite.criteria, this.dataSource);
                missingField = desc.contains(isc.FilterBuilder.missingFieldPrompt);
            }
            
            // Hilite can be edited unless explicity marked !canEdit or if in editMode.
            var canEdit = (hilite.canEdit != false || this.creator.editingOn);
            var newRule = this.createAutoChild("hiliteRule", 
                {
                    rootDataSource:this.rootDataSource,
                    fieldName: hilite.fieldName,
                    hilite: hilite,
                    dataSource: this.dataSource,
                    fieldDataSource: this.fieldDataSource,
                    missingField: missingField,
                    hiliteIcons: this.hiliteIcons,
                    showRemoveButton: canEdit
                }
            );
            if (!canEdit) newRule.disable();

            this.showNewHilite(newRule);
        }
    },

    //> @method hiliteEditor.saveHilites()
    // Save the set of Hilites and fire the +link{hiliteEditor.callback, callback}.
    //
    // @param callback (Callback) the function to call when saving is complete
    // @visibility external
    //<
    saveHilites : function (callback) {
        var hilites = this.getHilites();
        if ((!hilites || hilites.length == 0) && 
                (!this._loadedHilites || this._loadedHilites.length == 0)) return;
        this.completeEditing(hilites);
    },

    getHilites : function () {
        var rules = this.ruleLayout.members,
            hilites = []
        ;

        for (var i = 0; i < rules.length; i++) {
            var rule = rules[i],
                hilite = rule.getHilite();
            if (hilite == null) continue;
            hilites.add(hilite);
        }

        return hilites;
    },
 
    getHiliteState : function () {
        var hilites = this.getHilites();
        if (hilites == null || hilites.length == 0) return null;
        return "(" + isc.JSON.encode(hilites, {dateFormat:"logicalDateConstructor"}) + ")";
    },
 
    completeEditing : function (hilites) {
        if (this.logIsInfoEnabled()) this.logInfo("returning hilites: " + isc.echoFull(hilites));
        if (this.callback) this.fireCallback(this.callback, "hilites", [hilites]);
    }    
});


//>	@class AdvancedHiliteEditor 
// A widget for editing a single, advanced +link{class:HiliteRule, hilite rule} for use by  
// +link{class:DataBoundComponent, dataBoundComponents}.  Where a simple hilite provides  
// configuration of a single criterion and either foreground or background color for  
// application to a single field, an advanced hilite can specify more complex criteria which can 
// both test and affect multiple fields and allow both background and foreground colors to 
// be specified in a single rule. 
// <P>
// <i><b>Important Note:</b> this class should not be used directly - it is exposed purely for
// +link{group:i18nMessages, i18n reasons.}</i>
//
// @treeLocation Client Reference/Grids/ListGrid
// @visibility external
//<
isc.defineClass("AdvancedHiliteEditor", "VStack");

isc.AdvancedHiliteEditor.addProperties({
    // editor for advanced  highlights
    // ---------------------------------------------------------------------------------------

    padding: 10,
    membersMargin: 10,

    //> @attr advancedHiliteEditor.filterBuilder (AutoChild FilterBuilder : null : IR)
    // AutoChild +link{class:FilterBuilder} for configuring the criteria for this Hilite.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>advancedHiliteEditor.filterBuilderProperties</code>.
    //
    // @visibility external
    //<
    filterBuilderDefaults : {
        _constructor:"FilterBuilder",
        isGroup:true,
        padding:8,
        maxHeight: 200,
        overflow: "visible"
    },
    
    //> @attr advancedHiliteEditor.filterGroupTitle (string : "Filter" : IR) 
    // The title for the Filter group.
    //
    // @group i18nMessages 
    // @visibility external
    //<
    filterGroupTitle: "Filter",

    //> @attr advancedHiliteEditor.hiliteIcons (Array of SCImgURL : null : IRW)
    // Specifies a list of icons that can be used in hilites.
    // <P>
    // <code>hiliteIcons</code> should be specified as an Array of +link{SCImgURL}.
    // When present, +link{class:HiliteRule, hilite rules}
    // will offer the user a drop down for picking one of these icons.
    // <P>
    // If the user picks an icon, the created hiliting rule will have +link{hilite.icon} set to 
    // the chosen icon.  +link{listGridField.hiliteIconPosition} controls where the icon will 
    // appear for that field -- the default is that it appears in front of the normal cell content.
    //
    // @visibility external
    //<

    //> @attr advancedHiliteEditor.hiliteForm (AutoChild DynamicForm : null : IR)
    // AutoChild +link{class:DynamicForm} for configuring the details of this Hilite.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>advancedHiliteEditor.hiliteFormProperties</code>.
    //
    // @visibility external
    //<
    hiliteFormDefaults : {
        _constructor:"DynamicForm",
        isGroup:true,
        extraSpace:4,
        padding:8,
        width:"100%",
        numCols: 8,
        colWidths:[200,150,100,150,100,150,40,50]
    },

    //> @attr advancedHiliteEditor.appearanceGroupTitle (string : "Appearance" : IR) 
    // The title for the Appearance group.
    //
    // @group i18nMessages 
    // @visibility external
    //<
    appearanceGroupTitle: "Appearance",

    //> @attr advancedHiliteEditor.targetFieldsItemTitle (string : "Target Field(s)" : IR) 
    // The title for the Target Field(s) picker.
    //
    // @group i18nMessages 
    // @visibility external
    //<
    targetFieldsItemTitle: "Target Field(s)",

    hiliteButtonsDefaults : {
        _constructor:isc.HLayout, 
        membersMargin:8, height:1
    },

    hiliteButtonsSpacerDefaults : {
        _constructor: "LayoutSpacer",
        width: "*",
        autoParent: "hiliteButtons" 
    },

    //> @attr advancedHiliteEditor.saveButton (AutoChild StatefulCanvas : null : IR)
    // AutoChild +link{class:ImgButton} that accepts this Hilite and fires the 
    // +link{advancedHiliteEditor.callback, callback}.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>advancedHiliteEditor.saveButtonProperties</code>.
    //
    // @visibility external
    //<
    saveButtonDefaults : {
        _constructor:"IAutoFitButton", 
        autoParent:"hiliteButtons",
        click : function () {
            this.creator.saveHilite();
        }
    },

    //> @attr advancedHiliteEditor.saveButtonTitle (string : "Save" : IR) 
    // The title text for the +link{advancedHiliteEditor.saveButton,saveButton}.
    //
    // @group i18nMessages 
    // @visibility external
    //<
    saveButtonTitle: "Save",

    //> @attr advancedHiliteEditor.cancelButton (AutoChild StatefulCanvas : null : IR)
    // AutoChild +link{class:ImgButton} that cancels this AdvancedHiliteEditor without saving 
    // any changes, firing the +link{advancedHiliteEditor.callback, callback} with a null
    // parameter.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>advancedHiliteEditor.cancelButtonProperties</code>.
    //
    // @visibility external
    //<
    cancelButtonDefaults : {
        _constructor:"IAutoFitButton", 
        autoParent:"hiliteButtons",
        click : function () {
            this.creator.cancelEditing();
        }
    },

    //> @attr advancedHiliteEditor.cancelButtonTitle (string : "Cancel" : IR) 
    // The title text for the +link{advancedHiliteEditor.cancelButton,cancelButton}.
    //
    // @group i18nMessages 
    // @visibility external
    //<
    cancelButtonTitle: "Cancel",

    // overall layout
    // ---------------------------------------------------------------------------------------
    defaultWidth:900, defaultHeight:600,
    visibilityMode:"multiple",

    //> @attr advancedHiliteEditor.invalidHilitePrompt (string : "Enter at least one rule, a color or icon, and a target field, or press 'Cancel' to abandon changes." : IR) 
    // The message to show when the user clicks "Save" without entering any criteria. 
    //
    // @group i18nMessages 
    // @visibility external
    //<
    invalidHilitePrompt: "Enter at least one rule, a color or icon, and a target field, or press 'Cancel' to abandon changes.",

    //> @attr advancedHiliteEditor.title (string : "Advanced Hilite Editor" : IR) 
    // The title text shown in the header bar of this editor's dialog.
    //
    // @group i18nMessages 
    // @visibility external
    //<
    title: "Advanced Highlight Editor",

    //> @attr advancedHiliteEditor.callback (Callback : null : IR)
    // The callback to fire when the +link{advancedHiliteEditor.saveButton,saveButton} is clicked.
    //
    // @visibility external
    //<

    iconFieldDefaults: isc.HiliteRule.getInstanceProperty("iconFieldDefaults")
});

// Copy i18nMessages from HiliteRule.
// This is deferred until after the locale has loaded.
isc.defer("isc.AdvancedHiliteEditor.addProperties({" +
"foregroundColorTitle: isc.HiliteRule.getInstanceProperty('foregroundColorTitle')," +
"backgroundColorTitle: isc.HiliteRule.getInstanceProperty('backgroundColorTitle')," +
"iconFieldTitle: isc.HiliteRule.getInstanceProperty('iconFieldTitle')" +
"})");

isc.AdvancedHiliteEditor.addMethods({

    initWidget : function () {
        this.Super("initWidget", arguments);

        var ds = this.getDataSource(),
            _this = this;

        this.addAutoChild("filterBuilder", 
            { groupTitle: this.filterGroupTitle,
              dataSource: ds, fieldDataSource: this.fieldDataSource,
                
              // Override the defaultOptionDataSource logic on every FilterClause and
              // nested FilterClause created inside subclauses to avoid fetching against
              // the special hilite criteria DS.
              inheritedClauseProperties : {
                rootDataSource:this.rootDataSource,
                // If a field has a specified displayField with no explicit optionDataSource,
                // pick up the rootDataSource (set up in DBC.editHilites())
                getDefaultOptionDataSource : function (field) {
                    
                    if (this.rootDataSource) {
                        return this.rootDataSource;
                    }
                    var ds = this.Super("getDefaultOptionDataSource", arguments);
                    // Always avoid attempting to fetch against the hiliteCriteria DS
                    if (ds && ds.isHiliteCriteriaDS) return null;
                    return ds;
                    
                }
              },
              fieldNameChanged : function (filterClause) {
                  this.Super("fieldNameChanged", arguments);
                  _this.fieldChosen(filterClause.getFieldName(true));
              }
             }
        );

        var hiliteIconItem = isc.addProperties({}, this.iconFieldDefaults, this.iconFieldProperties, {
            name: "icon",
            title: this.iconFieldTitle
        });

        // Add hiliteIcons to valueMap
        if (this.hiliteIcons) {
            this.hiliteIcons.map(function (icon) {
                hiliteIconItem.disabled = false;
                // Map all values to "", to show the icon only. There may be a more elegant
                // way to do this with pickListProperties.
                hiliteIconItem.valueMap[icon] = "";
                // This actually provides the icon for the select item
                hiliteIconItem.valueIcons[icon] = icon;
            });
        }

        // Add this.hilite.icon to valueMap if not already there
        var icon = this.hilite ? this.hilite.icon : null;
        if (icon && !hiliteIconItem.valueMap[icon]) {
            hiliteIconItem.disabled = false;
            hiliteIconItem.valueMap[icon] = "";
            hiliteIconItem.valueIcons[icon] = icon;
        }


        var items = [
            {title:this.targetFieldsItemTitle, name:"fieldName", multiple:true, allowMultiSelect: true,
             type:"select"
            },
            {title:this.foregroundColorTitle, name:"textColor", type:"color" },
            {title:this.backgroundColorTitle, name:"backgroundColor", type:"color" },
            hiliteIconItem
        ];
        var hiliteFormProperties = { groupTitle: this.appearanceGroupTitle };
        if (this.hiliteCanReplaceValue) {
        	hiliteFormProperties.numCols = 10;
        	hiliteFormProperties.colWidths = [200,150,100,150,150,150,100,150,40,50];
            if (this.hiliteReplaceValueFieldTitle == null) {
                this.hiliteReplaceValueFieldTitle = 
                    isc.ListGrid.getInstanceProperty("hiliteReplaceValueFieldTitle");
            }
        	items.addAt({title:this.hiliteReplaceValueFieldTitle, name:"replacementValue", type:"text" }, 2);
        }

        var hiliteForm = this.addAutoChild("hiliteForm", hiliteFormProperties);
        this.iconField = hiliteForm.getField("icon");

        if (this.fieldDataSource) {
            items[0] = isc.addProperties({}, items[0], {
                valueField: "name",
                displayField: "title",
                optionDataSource: this.fieldDataSource,
                optionCriteria: { _constructor: "AdvancedCriteria", operator: "or",
                    criteria: [
                        { fieldName: "showInSimpleEditor", operator: "equals", value: true },
                        { fieldName: "showInSimpleEditor", operator: "isNull" }
                    ]
                }
            });
            delete items[0].defaultDynamicValue;
            this.hiliteForm.addItems(items);
        } else {
            var fieldNames = this.fieldNames || ds.getFieldNames(),
                fieldMap = this.fieldMap = {};
            for (var i = 0; i < fieldNames.length; i++) {
                var fieldName = fieldNames[i],
                    field = ds.getField(fieldName),
                    fieldTitle = field.title;
                if (field.hidden) continue;
                fieldTitle = fieldTitle ? fieldTitle : fieldName;
                fieldMap[fieldName] = fieldTitle;
            }
            this.fieldMap = fieldMap;
            items[0].valueMap = fieldMap;
            this.hiliteForm.addItems(items);
        }

        this.addAutoChildren(["hiliteButtons", "hiliteButtonsSpacer"]);
        this.addAutoChild("saveButton", { title: this.saveButtonTitle });
        this.addAutoChild("cancelButton", { title: this.cancelButtonTitle });

        this.addMembers([this.filterBuilder, this.hiliteForm, this.hiliteButtons]);

        if (this.hilite != null) {
            // we're editing an existing hilite
            this.filterBuilder.setCriteria(this.hilite.criteria);
            this.hiliteForm.editRecord(this.hilite);
        }

    },

    // the first time a field is chosen when defining criteria, default the target field to
    // that field.
    fieldChosen : function (fieldName) {
        if (fieldName && this.hiliteForm.getValue("fieldName") == null) {
            this.hiliteForm.setValue("fieldName", fieldName);
        }
    },

    //> @method advancedHiliteEditor.saveHilite()
    // Save changes and fire the +link{advancedHiliteEditor.callback, callback}.
    //
    // @visibility external
    //<
    saveHilite : function () {
        this.hiliteForm.setValue("criteria", this.filterBuilder.getCriteria());
        var hilite = this.hiliteForm.getValues();

        if (hilite.criteria.criteria == null || hilite.criteria.criteria.length == 0 ||
            (!hilite.textColor && !hilite.backgroundColor && !hilite.icon && !hilite.replacementValue) ||
            hilite.fieldName == null) 
        {
            isc.say(this.invalidHilitePrompt);
            return;
        }

        var cssText = "";

        if (hilite.textColor && hilite.textColor != "") {
            cssText += "color:"+hilite.textColor+";";
        } 
        if (hilite.backgroundColor && hilite.backgroundColor != "") {
            cssText += "background-color:"+hilite.backgroundColor+";";
        }

        hilite.cssText = cssText;
        if (this.hilite && this.hilite.id) hilite.id = this.hilite.id;
        
        this.completeEditing(hilite);
    },
    
    //> @method advancedHiliteEditor.cancelEditing()
    // Discard changes and fire the +link{advancedHiliteEditor.callback, callback} with a null 
    // parameter.
    //
    // @visibility external
    //<
    cancelEditing : function () {
        this.completeEditing(null);        
    },

    completeEditing : function (result) {
        if (this.callback) this.fireCallback(this.callback, ["hilite"], [result]);
    }    
});

