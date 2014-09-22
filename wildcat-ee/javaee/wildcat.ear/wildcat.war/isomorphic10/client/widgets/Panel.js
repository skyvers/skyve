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

 







//> @class PanelHeader
// A PanelHeader 
// 
// @visibility panels
//<

isc.defineClass("PanelHeader", "ImgSectionHeader").addProperties({

    width: "100%",
    height: 22,

    // baseStyle
    //----------
    //> @attr panelHeader.baseStyle (CSSStyleName : "sectionHeader" : IRW)
    // @visibility panels
    //<    
    baseStyle:"sectionHeader"

    //> @method panelHeader.setBaseStyle()
    // @include statefulCanvas.setBaseStyle
    // @visibility panels
    //<

    //> @attr panelHeader.titleStyle (CSSStyleName : null : IRW)
    // CSS style applied to the title text only.  Defaults to +link{baseStyle} when unset.
    // <P>
    // With a separate <code>titleStyle</code> and +link{baseStyle} set, you can provide a
    // backgroundColor via <code>baseStyle</code> that will allow translucent .png media to be
    // "tinted" by the underlying background color, so that a single set of media can provide
    // range of color options.  In this usage, the <code>titleStyle</code> should generally not
    // specify a background color as this would block out the media that appears behind the
    // title.   
    // 
    // @visibility panels
    //<        

    //>    @method panelHeader.setTitleStyle()
    // Sets the +link{titleStyle}, which is applied to the title text.
    // @param style (CSSStyleName) new title style
    // @visibility panels
    //<        

});

isc.PanelHeader.addMethods({
    showSelectedIcon: false,
    showRollOverIcon: false,
    showDisabledIcon: false,
    showDownIcon: false,
    showFocusedIcon: false,
    initWidget : function () {
        this.setPanelTitleFromCanvas();
    },
    setPanelTitleFromCanvas : function () {
        if (this.canvas) {
            if (this.canvas.title) this.title = this.canvas.title;
            if (this.canvas.icon) this.icon = this.canvas.icon;
        }
    }
});

isc.PanelHeader.registerStringMethods({
    //> @method panelHeader.iconClick()
    // If this button is showing an +link{buttonItem.icon, icon}, a separate click
    // handler for the icon may be defined as <code>this.iconClick</code>.
    // Returning false will suppress the standard button click handling code.
    // @group buttonIcon    
    // @visibility panels
    //<
    iconClick:""
});


isc.Canvas.addProperties({
    //> @attr canvas.panelHeader (AutoChild PanelHeader : null : IRW)
    //
    //<

    //> @attr canvas.showPanelHeader (boolean : false : IRW)
    // Should a PanelHeader be shown attached to this canvas?
    //<
    
    //> @attr canvas.panelHeaderDefaults (PanelHeader Properties : null : IRW)
    //
    //<
    panelHeaderDefaults: {
        _constructor: "PanelHeader"
    },

    //> @attr canvas.panelHeaderProperties (PanelHeader Properties : null : IRW)
    //
    //<
    panelHeaderProperties: {
    },

    //> @type PanelHeaderPlacement
    // 
    // @value "peer"
    // Add the +link{PanelHeader} as a peer of this Canvas.
    //
    // @value "member"
    // Add the PanelHeader as the first member of this Canvas.
    //
    // @value "custom"
    // Don't attempt to place the PanelHeader, assuming that the component will do so itself.
    // 
    // @visibility panels
    //<

    panelHeaderPlacement: "peer",

    setupPanelHeader : function () {
        if (!this.showPanelHeader) return;

        this.panelHeader = this.createAutoChild("panelHeader", { canvas: this, snapTo: "T"});
            
        if (isc.isA.Layout(this)) this.panelHeaderPlacement = "member";
        else this.panelHeaderPlacement = "peer";

        // wangle this to always add the panelHeader as a peer for now (addMember
        // in an HLayout looks terrible!)
        this.panelHeaderPlacement = "peer";
        
        if (this.panelHeaderPlacement == "member") {
            this.addMember(this.panelHeader, 0);
        }
        else if (this.panelHeaderPlacement == "peer") {
            this.addPeer(this.panelHeader);
            this.panelHeader.moveAbove(this);

            this._registerAttachedPeer(this.panelHeader, isc.Canvas.TOP);
        }
        else if (this.panelHeaderPlacement == "custom") {}
    }
});


// Actions for panelHeaders

isc.Canvas.addMethods({
    panelActionControls: [],
    
    refreshPanelControls : function () {
        var controls = this.panelControls || [];
        
        if (!this.panelHeader.controls) this.panelHeader.controls = [];
        else this.panelHeader.controls.setLength(0);

        for (var i=0; i<controls.length; i++) {
            var item = controls.get(i),
                itemToAdd = null;

            if (isc.isAn.Object(item)) {
                // item is a control
                itemToAdd = isc.addProperties({}, item);
            } else if (item.startsWith("action:")) {
                // item is an action-def
                var actionName = item.substring(7, item.length),
                    action = isc.Canvas.getRegisteredAction(actionName);

                if (this.canPerformAction(action)) {
                    if (this.showActionInPanel(action)) 
                        itemToAdd = this.getPanelActionControl(action);
                }
            } else {
                // item is an autochild-name
                itemToAdd = this.createAutoChild(item);
            }

            if (itemToAdd) {
                this.panelHeader.controls.add(itemToAdd);
            } else {
            }
        }
//        this.panelHeader.controlsLayout.members = [];
        var header = this.panelHeader;
        header.addControls();
//        this.panelHeader.markForRedraw();
    },

    canPerformAction : function (action) {
        var actionName = action.name,
            propertyName = action.enableProperty || 
                "can"+ actionName.substring(0,1).toUpperCase() +
                actionName.substring(1,actionName.length)
        ;

        return this[propertyName] && this[propertyName] == true ? true : false;
    },
    showActionInPanel : function (action) {
        return action.showInPanel;
    },
    getPanelActionControl : function (action) {
        var control = this.panelActionControls[action.name],
            className = action.controlConstructor || "ImgButton";

// TODO: this seems to return the same control instances across multiple canvii,
// like the cached _panelActionControls array is static, which it isn't
//        if (control) return control;

        control = isc.ClassFactory.newInstance(
            className, {
                ID: this.getID()+"_"+action.name,
                width: 18, height:18,
                src: action.icon, 
                showRollOver:false, 
                showDown:false,
                showDisabled:false, 
                showFocused:false,
                actionTarget: this, 
                actionObject: action,
                prompt: action.tooltip,
                click : function () {
                    this.actionTarget[this.actionObject.methodName]();
                }
            }
        );

        this.panelActionControls[action.name] = control;
        return this.panelActionControls[action.name];
    },

    // helper method to allow Canvas instances to print themselves - means the
    // method can be called as just canvas.showPrintPreview() without params if 
    // necessary (like in the MultiView)
    showPrintPreview : function (printProperties, previewProperties, callback,
                                  separator)
    {
        isc.Canvas.showPrintPreview(this, printProperties, previewProperties,
            callback, separator);
    }

});

isc.Canvas.addClassProperties({
    _registeredActions : {}                 // internal array of  registered actions
});

isc.Canvas.addClassMethods({
    registerAction : function (newAction) {
        if (!this._registeredActions[newAction.name]) {
            this._registeredActions[newAction.name] = newAction;
        }
    },
    getRegisteredActionNames : function () {
        return isc.getKeys(this._registeredActions);
    },
    getRegisteredActions : function () {
        return isc.getValues(this._registeredActions);
    },
    getRegisteredActionIndex : function () {
        var actions = this.getRegisteredActions(),
            actionIndex = actions.makeIndex("name", false);
        return actionIndex;
    },
    getRegisteredAction : function (actionName) {
        return this._registeredActions[actionName];
    },
    isActionRegistered : function (actionName) {
        return !this._registeredActions[actionName] ? false : true;
    }
});

isc.defineClass("Action", "Class").addProperties({
    name: null,
    title: null,
    icon: null,
    tooltip: null,
    methodName: null,
    controlConstructor: "ImgButton",
    enableProperty: null,
    showInPanel: true
});

isc.Canvas.registerAction(
    isc.Action.create({
        name: "edit",
        title: "Edit",
        icon: "[SKINIMG]/actions/edit.png",
        tooltip: "Put the component into Edit mode",
        methodName: "startEditing",
        showInPanel: false
    })
);

isc.Canvas.registerAction(
    isc.Action.create({
        name: "editNew",
        title: "Edit New",
        icon: "[SKINIMG]/SectionHeader/opener_closed.png",
        tooltip: "Add a new Record to the component",
        methodName: "startEditingNew",
        showInPanel: false
    })
);

isc.Canvas.registerAction(
    isc.Action.create({
        name: "sort",
        title: "Sort",
        icon: "[SKINIMG]/actions/sort_ascending.png",
        tooltip: "Sort the records in the component",
        methodName: "sort",
        controlConstructor: "SortActionSelector",
        enableProperty: "canSortFields",
        showInPanel: false
    })
);

isc.Canvas.registerAction(
    isc.Action.create({
        name: "export",
        title: "Export",
        icon: "[SKINIMG]/actions/redo.png",
        tooltip: "Export the data in the component",
        methodName: "exportData",
        showInPanel: true
    })
);

isc.Canvas.registerAction(
    isc.Action.create({
        name: "print",
        title: "Print",
        icon: "[SKINIMG]/actions/print.png",
        tooltip: "Print the data in the component",
        methodName: "showPrintPreview",
        showInPanel: true
    })
);

isc.defineClass("SortActionSelector", "DynamicForm").addProperties({
    width: 100,
    height: 20,
    numCols: 4,
    fields: [
        {
            name: "sortField", showTitle: false, colSpan: 2,
            type: "select",
            width: 80,
            prompt: "Sort Field",
            startRow: false, endRow: false,
            changed: function (form, item, value) {
                var up = form.getField("sortDirection").getValue(),
                    sortDirection = up ? "ascending" : "descending";
                if (form.actionTarget.sort) form.actionTarget.sort(value, sortDirection);
                else form.actionTarget.data.sortByProperty(value, up);
            }
        },{
            name: "sortDirection", showTitle: true, showLabel: false, 
            type: "checkbox", 
            width: 20,
            prompt: "Sort Direction: Checked is Ascending",
            startRow: false, endRow: false,
            changed: function (form, item, value) {
                var up = value,
                    sortDirection = up ? "ascending" : "descending",
                    sortField = form.getField("sortField").getValue();
                if (form.actionTarget.sort) form.actionTarget.sort(sortField, sortDirection);
                else form.actionTarget.data.sortByProperty(sortField, up);
            }
        }
    ],
    initWidget : function () {
        this.Super("initWidget", arguments);
    },
    draw : function () {
        this.Super("draw", arguments);
        var ds = this.actionTarget.getDataSource(),
            fields = ds ? isc.getValues(ds.getFields()) : [],
            valueMap = {};
        
        for (var i=0; i<fields.length; i++) {
            var item = fields.get(i);
            valueMap[item.name] = item.title;
        }
        this.getField("sortField").setValueMap(valueMap);
    }
});

