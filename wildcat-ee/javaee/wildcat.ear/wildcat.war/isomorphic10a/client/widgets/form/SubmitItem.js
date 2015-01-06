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

 






//>	@class	SubmitItem
// Button that saves the data in the form, by calling +link{DynamicForm.submit()} when clicked.
// +link{DynamicForm.submit()} for details on how to control what happens when a form is
// submitted.
//
// @see group:operations
//
// @visibility external
//<
isc.ClassFactory.defineClass("SubmitItem", "ButtonItem");
isc.SubmitItem.addProperties({
    //>@attr    SubmitItem.title    (String : "Submit" : IRW)
    // SubmitItems show a title of <code>"Submit"</code> by default. May be overridden.
    // @visibility external
    //<
    title:"Submit"
});

isc.SubmitItem.addMethods({
    
    handleClick : function () {
        if (this.Super("handleClick", arguments) == false) return false;
        // note that submit() is implemented to perform a native submission (via submitForm())
        // iff form.canSubmit is true - otherwise it falls through to dataBoundComponent 
        // 'saveData()' method
        this.form.submit();
        this.form.completeEditing();
    }
});

