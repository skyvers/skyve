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

 





//> @class NestedEditorItem
// Form item which renders a single complex sub-object in an embedded component.  By default,
// the embedded component is a +link{class:DynamicForm}
// @treeLocation Client Reference/Forms/Form Items
// @visibility internal
//<
isc.ClassFactory.defineClass("NestedEditorItem", "CanvasItem");
isc.NestedEditorItem.addProperties({

    shouldSaveValue: true,
    
    
    isCriteriaEditor:false,

  	//> @attr	nestedEditorItem.editor		(AutoChild DynamicForm : null : [IRW])
    //
    // The editor that will be rendered inside this item.  Unless overridden, the editor will be
    // an instance of +link{class:DynamicForm}. It will be created using the overrideable 
    // defaults standard to the +link{group:autoChildren,AutoChild} subsystem - editorConstructor 
    // and editorProperties.
    //
    //  @visibility internal
	//<
    editorConstructor: "DynamicForm",
    editorDefaults: {
        itemChanged : function (item, newValue) {
            var values = this.creator.isCriteriaEditor ? this.getValuesAsCriteria() 
                            : this.getValues();
        	this.creator.updateValue(values);
        }
    }
    
});

isc.NestedEditorItem.addMethods({
    init : function () {
        this._createEditor();
        this.Super("init", arguments);
    },
    
    isEditable : function () {
        return true;
    },

    _createEditor: function(){

        var ds;
        var dynProps = {};
        
        if (this.form.dataSource) { // Should be, otherwise how have we ended up with a complex field?
            ds = isc.DataSource.getDataSource(this.form.dataSource);
            var field = ds.getField(this.name);
            if (field) {
                dynProps.dataSource = ds.getFieldDataSource(field);
            }
        }

        if (this.form && this.form.showComplexFieldsRecursively) {
            dynProps.showComplexFields = true;
            dynProps.showComplexFieldsRecursively = true;
        } else {
            dynProps.showComplexFields = false;
        }
        
        this.addAutoChild("editor", dynProps);
        this.canvas = this.editor;        
    
    },
    
    setValue : function(value) {
        this.editor.setValues(value);
    },
    
    updateValue : function(data) {
        this._updateValue(data);
    },
    
    _shouldAllowExpressions : function () {
        return false;
    }

});

