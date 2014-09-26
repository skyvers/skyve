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

 






isc.defineClass("ExpressionItem", "PopUpTextAreaItem");

isc.ExpressionItem.addMethods({
    mapValueToDisplay : function (value) {
        if (isc.isA.StringMethod(value)) return value.getDisplayValue();
        else if (isc.isA.Function(value)) {
            if (value.iscAction) {
                return "[" + value.iscAction.title + "]";
            }
            return isc.Func.getBody(value);
        }
        else return this.Super("mapValueToDisplay", arguments);
    },
    
    getValue : function () {
        var value = this.Super("getValue");
        if (isc.isA.Function(value)) return isc.Func.getBody(value);
        else return value;
    },
    textAreaWidth:400,

    // ---------------------------------------------------------------------------------------

    showActionIcon:true,
    actionIconSrc:"[SKIN]/actions/add.png",
    actionIconWidth:20,
    actionIconHeight:20,
    
    // Where should the actionIcon show up in the icons array?
    
    actionIconPosition:1,
    _setUpIcons : function () {
        this.Super("_setUpIcons", arguments);
        if (this.showActionIcon) {
            if (this.icons == null) this.icons = [];
            var position = this.actionIconPosition;
            this.icons.addAt({
                name:"action",
                src:this.actionIconSrc,
                showOver:false,
                width:this.actionIconWidth,
                height:this.actionIconHeight,
                // return false from click handler so we don't fire standard cellclick
                // which would show the pop up (unless we also set popUpOnAnyClick to false)
                click:this.getID() + ".showActionMenu();return false;"
            }, position);
            
            // have to explicitly set the required icon properties - usually handled by
            // setUpIcons, but that's already run at this point.
            this._setUpIcon(this.icons[position]);
        }
    },
    
    // Override updateAppearance to refresh our element value. Required since
    // the element value isn't edited directly.
    updateAppearance : function (newValue) {    
        this.setElementValue(this.mapValueToDisplay(newValue));
    },

    showActionMenu : function () {
        var item = this,
            menu = isc.ActionMenu.create({
            sourceComponent : this.form.currentComponent,
            sourceMethod : this.name,
            components: this.form.allComponents,
            bindingComplete : function (binding) {
                // binding may be null - if the user selected top level "[None]" option
                item._updateValue(binding);
            }
        });
        // Need to draw the menu first or placeNear() might not have the correct dimensions
        menu.show();
        var iconRect = this.getIconPageRect(this.icons[1]); 
        menu.placeNear(iconRect[0] + this.actionIconWidth, 
                       iconRect[1] + this.actionIconHeight - this.containerWidget.getScrollTop());
    }

    
});
