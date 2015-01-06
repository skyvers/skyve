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

 





//>	@class	DataPathItem
//
// TextItem subclass for managing a DataPath
//
// @visibility external
//<
isc.defineClass("DataPathItem", "TextItem").addProperties({
    operationsTreeDefaults: {
        _constructor: "TTreeGrid",
        autoDraw: false,
        recordDoubleClick: function () {
            this.creator.operationSelected();
        },
        getIcon : function (node) {
            var serviceContainer = this.creator.form.creator,
                icon = (serviceContainer && serviceContainer.getServiceElementIcon) ?
                    serviceContainer.getServiceElementIcon(node) : null
            ;
            if (icon) return icon;
            return this.Super("getIcon", arguments);
        }
    },

    operationsTreeSelectButtonDefaults: {
        _constructor: "TButton",
        autoDraw: false,
        title: "Select",
        click: function () {
            if (this.creator.operationsTree.anySelected())
                this.creator.operationSelected();
        }
    },

    defaultIcons: [
        {
            src: "[SKINIMG]/actions/edit.png",
            click: "item.showOperationsTreeData()",
            width:16, height:16
        },
        {
            src: "[SKINIMG]/actions/remove.png",
            click: "item.clearFormValues()",
            width:16, height:16
        }
    ],
    
    baseManagedProperties: [
        "dataPath", "schemaDataSource", "serviceName", "serviceNamespace"
    ],

    // methods
    getPropertyName : function (prop) {
        if (this.isInput) 
            return "input" + prop.substring(0,1).toUpperCase() + prop.substring(1);
        else return prop;
    },
    initManagedProperties : function () {
        this.managedProperties = [];
        var baseProps = this.baseManagedProperties;
        for (var i = 0; i < baseProps.length; i++) {   
            this.managedProperties.add(this.getPropertyName(baseProps[i]));
        }
    },
    
    keyPress : function (item, form, keyName) {
        if (keyName != "Arrow_Left" && keyName != "Arrow_Right" &&
            keyName != "Home" && keyName != "End") return false;
        this.Super("keyPress", arguments);
    },

    init : function () {
        this.icons = isc.clone(this.defaultIcons);

        this.initManagedProperties();

        this.Super("init", arguments);

        if (this.operationsTreeData) {
            this.addAutoChildren(["operationsTree", "operationsTreeSelectButton"]);
        }
    },

    showOperationsTreeData : function () {
        if (!this.operationsTreeData) return;
        if (!this.operationsTree) 
            this.addAutoChildren(["operationsTree", "operationsTreeSelectButton"]);

        var theTree;
        if (isc.isA.Tree(this.operationsTreeData)) {
            theTree = this.operationsTreeData;
        } else {
            theTree = isc.Tree.create({
                modelType: "children",
                root: {children: this.operationsTreeData},
                nameProperty: "name",
                childrenProperty: "children"
            });
            theTree.openAll();
        }
        this.operationsTree.setData(theTree);

        // find and select the node corresponding to the current dataPath value, if there is one
        var prefix = this.isInput ? "formInputs" : "formOutputs";

        var dataPath = prefix + "/" + this.getValue(),
            node = theTree.find(dataPath)
        ;

        if (node) this.operationsTree.selectRecord(node);

        if (!this.schemaDialog) {
            this.schemaDialog = isc.TWindow.create({
                title:"Select element from message",
                autoCenter:true,
                height:"90%", width:"60%",
                isModal: true,
                showModalMask: true,
                items: [ 
                    isc.VLayout.create({
                        width: "100%",
                        height: "100%",
                        members: [this.operationsTree, this.operationsTreeSelectButton ]
                    })
                ]
            });
        } else this.schemaDialog.show();
    },

    // the user has picked a paletteNode generated from one of the available input messages
    operationSelected : function () {
        var palette = this.operationsTree,
            tree = palette.data,
            paletteNode = palette.getSelectedRecord()
        ;

        this.schemaDialog.hide();
        this.setDataPathProperties(paletteNode);
    },

    setDataPathProperties : function (editNode) {
        // handle either an editNode (live editing) or a paletteNode (picked from schema tree)
        var defaults = editNode.defaults;

        this.dataPathProps = isc.applyMask(defaults, this.managedProperties);
        if (this.logIsInfoEnabled()) {
            this.logInfo("setDPProps, editNode: " + this.echoAll(editNode) + 
                         " defaults: " + this.echo(defaults) + 
                         ", managedProps: " + this.managedProperties +
                         ", props: " + this.echo(this.dataPathProps));
        }
        this.saveFormValues();
    },
    
    saveFormValues : function () {
        for (var i = 0; i < this.managedProperties.length; i++) {
            var prop = this.managedProperties[i],
                value = this.dataPathProps[prop];
    
            this.form.setValue(prop, value);
        }
        // mild hack: only active with ComponentEditor as specialized by VisualBuilder,
        // where saveProperties exists.
        if (this.form.saveProperties) {
            this.form.saveProperties(this.dataPathProps, this.form.currentComponent);
        }
    },
    clearFormValues : function () {
        for (var i = 0; i < this.managedProperties.length; i++) {
            this.form.setValue(this.managedProperties[i], null);
        }
        // mild hack: only active with ComponentEditor as specialized by VisualBuilder,
        // where saveProperties exists.
        if (this.form.saveProperties) {
            this.form.saveProperties(this.dataPathProps, this.form.currentComponent);
        }
    }

    
});

