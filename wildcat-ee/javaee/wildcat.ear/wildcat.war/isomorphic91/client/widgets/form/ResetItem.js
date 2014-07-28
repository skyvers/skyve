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

 






//>	@class	ResetItem
// Button that resets the form to default values, by calling
// <code>DynamicForm.resetValues()</code>
// 
// If you define a click handler on this item, you can return false to cancel the reset.
//
// @visibility external
//<

isc.ClassFactory.defineClass("ResetItem", "ButtonItem");
isc.ResetItem.addProperties({
    // default the title to "Reset"
    title:"Reset"
});

isc.ResetItem.addMethods({
    // override handleClick to reset the form unless a user defined click-handler suppresses
    // the reset.    
    handleClick : function () {
        if (this.Super("handleClick", arguments) == false) return false;
        this.form.resetValues();
    }
});

