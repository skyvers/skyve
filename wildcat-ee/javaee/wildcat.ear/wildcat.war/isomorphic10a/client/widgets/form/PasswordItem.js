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

 





//>	@class	PasswordItem
// FormItem for password fields, where text input by the user should not be shown in readable text.
// @visibility external
//<
isc.ClassFactory.defineClass("PasswordItem", "TextItem").addClassProperties({
    _bullets:"\u2022\u2022\u2022\u2022\u2022\u2022\u2022\u2022"
});
isc.PasswordItem.addProperties({
    _elementType:"PASSWORD",

    showClippedValueOnHover: false
});

isc.PasswordItem.addMethods({

valueHoverHTML : function () {
    var elementValue = this.getElementValue();
    if (elementValue == null) elementValue = "";
    var bullets = isc.PasswordItem._bullets;
    while (bullets.length < elementValue.length) isc.PasswordItem._bullets = bullets += bullets;
    return bullets.substring(0, elementValue.length);
}

});

