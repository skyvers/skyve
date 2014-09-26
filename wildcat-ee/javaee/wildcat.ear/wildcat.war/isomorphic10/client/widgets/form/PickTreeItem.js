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


// NOTE: This class is Tree-based and thus does not support composite primary keys 
 

// Class will not work without the ListGrid
if (isc.ListGrid) {



//>	@class	PickTreeItem
// FormItem that allows picking a value from a hierarchical data model.
//
// @inheritsFrom CanvasItem
// @treeLocation Client Reference/Forms/Form Items
// @visibility external
// @example pickTree
//<
isc.ClassFactory.defineClass("PickTreeItem", "CanvasItem");

isc.PickTreeItem.addClassProperties({
    // create a single 'itemSelected' method to be applied to our menu buttons.
    // (fired in the scope of the button
    _itemSelected : function (item) {
        return this.canvasItem._itemSelected(item);
    },
    
    // getTitle() method for our TreeMenuButton - overridden to handle the case where the
    // form item value represents an as-yet-unloaded node (so 'getTitle()' is not available).
    _getButtonTitle : function () {
        var selection = this.getSelectedItem();
        if (selection == null) {
            // If a value is selected in the item, but we don't have the same node selected in
            // this TreeMenuButton widget, check the static valueMap for a title to display.
            var item = this.canvasItem, value = this.canvasItem.getValue();
            if (value != null) return item.mapValueToDisplay(value);
        }
        return this.Super("getTitle", arguments);
    },
    
    
    // If this item's menu is databound, and loadDataOnDemand is false, this method will be
    // fired when the entire tree is loaded (happens around init time).
    // We want to ensure that any value we have is associated with the appropriate node at this
    // point. This allows us to call setSelectedItem() on the TreeMenuButton, which hilights
    // the path to the node for our selected value.
    
    _treeDataLoaded : function () {
        var item = this.canvasItem;
        item.setValue(item.getValue());
    }

});

isc.PickTreeItem.addProperties({

     // Override canFocus -- even though buttons have no data element, they can accept focus.
    canFocus:true,
    
    // We will be saving out the selected values
    shouldSaveValue:true,
    
    // Class level defaults to apply to the button we create.
    // Picked up via the addAutoChild mechanism.
    
    buttonDefaults : {
        height:19
    },

    //> @attr pickTreeItem.button (AutoChild Canvas : null : R)
    // The visible button created by a PickTreeItem is an +link{AutoChild} of type
    // +link{TreeMenuButton} by default.
    // @visibility external
    //<
    buttonConstructor:"TreeMenuButton",

    //> @attr pickTreeItem.dataSource (Datasource | String : null : IRA)
    // If specified, the tree of possible options will be derived from the dataSource as a
    // ResultTree, rather than using this.valueTree.  Options can be loaded on demand or up
    // front according tp +link{attr:PickTreeItem.loadDataOnDemand}.
    // @visibility external
    //<

    //> @attr pickTreeItem.dataProperties (Tree Properties : null : IR)
    // For a <code>PickTreeItem</code> that uses a DataSource, these properties will be passed to
    // the automatically-created ResultTree.  This can be used for various customizations such as
    // modifying the automatically-chosen +link{tree.parentIdField}.
    // @group databinding
    // @visibility external
    //<

    //> @attr pickTreeItem.valueTree (Tree : null : IR)
    // A +link{class:Tree} of options from which the user can select.
    // @visibility external
    // @example pickTree
    //<
    
    //> @attr pickTreeItem.loadDataOnDemand (Boolean : null : IRA)
    // If this is a databound item, should the load our set of possible options be loaded
    // on demand (as submenus are displayed), or upfront?
    // @visibility external
    //<
    
    //loadDataOnDemand : false,
    
    //> @attr pickTreeItem.displayField (String : null : IR) 
    // Specifies an alternative field from which display values should be retrieved for this
    // item.
    // <p>
    // If this item is not databound (+link{attr:PickTreeItem.dataSource} is unset), this is
    // implemented by picking up the value of the specified field from the
    // +link{attr:valueTree}.
    // <p>
    // Otherwise this item will attempt to map its underlying value to a display value
    // by retrieving a record from the +link{attr:PickTreeItem.dataSource} where the 
    // +link{PickTreeItem.valueField} matches this item's value, and displaying the 
    // <code>displayField</code> value from that record.
    // @visibility external
    //<

    //> @attr pickTreeItem.valueField (String : null : IR)
    // Which field in the tree-data should be returned as this item's value?
    // If unspecified, the path will be used
    // @visibility external
    // @example pickTree
    //<
    

    //> @attr pickTreeItem.emptyMenuMessage (String : "No items to display" : IRA)
    // This message will be displayed as a single, disabled option in any empty menu/submenu
    // created from this item's data tree.
    // @visibility external
    // @example pickTree
    //<
    emptyMenuMessage:"No items to display",

    //> @attr pickTreeItem.emptyDisplayValue (String : null : IRW)
    // Text to display when this form item has a null or undefined value.
    // <P>
    // If the formItem has a databound pickList, and its +link{formItem.displayField} or
    // +link{formItem.valueField} (if the former isn't set) has an undefined emptyCellValue
    // field, that field will automatically be set using the emptyDisplayValue property.
    // <P>
    // If the emptyDisplayValue is null (the default) then this item will use the standard title
    // of the tree menu button that is shown when no values are selected.
    // @include formItem.emptyDisplayValue
    // @group display_values
    // @visibility external
    //<
    emptyDisplayValue: undefined,
    
    //> @attr pickTreeItem.readOnlyDisplay (ReadOnlyDisplayAppearance : "disabled" : IRW)
    // If +link{formItem.canEdit} is set to <code>false</code>, how should this item
    // be displayed to the user?
    // <P>
    // The default value for PickTreeItems is <code>disabled</code> - note that 
    // <code>readOnly</code> and <code>static</code> appearances have no effect for 
    // PickTreeItems.
    //
    // @visibility external
    //<
    readOnlyDisplay: "disabled"

    //> @attr pickTreeItem.canSelectParentItems (Boolean : null : IRW)
    // @include menu.canSelectParentItems
    // @group selection
    // @visibility external
    // @example treesEditing
    //<
});

isc.PickTreeItem.addMethods({
    init : function () {
        this.Super("init", arguments);
        // optionDataSource is a synonym of dataSource
        if (this.optionDataSource == null && this.dataSource == null && this.valueTree == null) {
            this.logWarn("This form item requires a 'valueTree'.");
        }
    },

    // override getOptionDataSource to pick up this.dataSource if specified.
    // This is required to allow standard 'fetchMissingValues' logic to show a display value
    // for databound trees with a default value that has not yet loaded.
    getOptionDataSource : function () {
        var ds = this.optionDataSource || this.dataSource;
        if (ds != null) ds = isc.DataSource.get(ds);
        return ds;
    },
    
    // Override getDisplayValue so if a developer calls it it returns the current display
    // value -- IE the title of the button
    // *Remember - this method isn't used internally, it's simply an accessor for developers
    getDisplayValue : function () {
        return this.canvas.getTitle();
    },
    
    //> @method pickTreeItem.setValueTree()
    // Setter to change the +link{pickTreeItem.valueTree} at runtime
    // @param valueTree (Tree) new value tree for the item
    // @visibility external
    //<
    setValueTree : function (valueTree) {
        this.valueTree = valueTree;
        if (this.canvas) {
            this.canvas.setData(this.valueTree);
        }
    },
    
    // Override _createCanvas to set up a TreeMenuButton as this item's canvas
    _createCanvas : function () {

        var dynamicButtonProperties = {
                // override getTitle to check for the case where the form item value represents
                // a currently unselected node.
                getTitle : isc.PickTreeItem._getButtonTitle,

                canFocus : this._canFocus(),

                // Allow the tree of options to be specified as item.dataSource or item.data.
                dataSource : this.getOptionDataSource(),
                // Pass optionCriteria through to the menuButton as 'criteria'
                criteria:this.optionCriteria,

                // Set the operationId used by the tree menu's ResultTree to fetch data
                // from the data source.
                menu: { _requestOperationId: this.optionOperationId },

                data : this.valueTree,
                
                // canSelectParentItems should be inherited from this item's setting
                canSelectParentItems : this.canSelectParentItems,
                
                itemSelected : isc.PickTreeItem._itemSelected,
                
                emptyMenuMessage:this.emptyMenuMessage,
                unselectedTitle: this._getEmptyDisplayValue(),

                // If databound, should we load the entire set of data up-front?
                loadDataOnDemand : this.loadDataOnDemand,
                treeDataLoaded : isc.PickTreeItem._treeDataLoaded,
                
                // setting the displayField on the button ensures it gets picked up by
                // SelectionTreeMenu.getItemTitle()
                displayField:this.displayField,

                // EDD
                dataProperties: (this.dataProperties == null ? {} : this.dataProperties)
            };
            
        // If the item has a specified width, and the class doesn't, pick up the width
        // from the item.    
        if (dynamicButtonProperties.width == null && this.width != null) {
            var policyWidth = this.getInnerWidth();
            
            var iconWidth = 0;
            if (this.icons && this.icons.length > 0) {
                for (var i=0; i<this.icons.length; i++) {
                    iconWidth += this.getIconWidth(this.icons[i]) + this.iconHSpace;
                }
            }

            if (!isc.isA.Number(policyWidth)) {
                dynamicButtonProperties.width = policyWidth;
            } else {
                // don't allow zero or negative width
                dynamicButtonProperties.width = Math.max(1, policyWidth - iconWidth);
            }
        }

        // Use 'addAutoChild' - this will handle applying the various levels of defaults
        this.canvas = this.addAutoChild("button", dynamicButtonProperties, 
                                        this.buttonConstructor, this.container);
        // set autoDestroy to true so the button gets destroyed when this item gets destroyed
        this.autoDestroy = true;                                        
        this.Super("_createCanvas", arguments);
        
        
        if (this._value != null) this.setValue(this._value);
    },
    
    // Don't have a default width for this item type as a class - pick it up from
    // the PickTreeItem.
    // If a developer specifies a width, we'll respect it.
    width:null,

   
    // Values management.
    // We're selecting nodes from a tree data structure.
    // The method 'getSelectedNode()' is available to return a pointer to the actual selected
    // node object, but this is not the 'value' of this form item.
    // If this.valueField is specified, the value of this item will be node[this.valueField].
    // Otherwise:
    // - if this is a databound tree, the value will be the primary key of this node (assumed
    //   to have a single primary key)
    // - if this is a client-only tree, and the modelType of the tree is "parent", the 
    //   idProperty for the selected node will be returned as item's value.
    // - If the modelType is not parent, the path of the node will be returned.
    
    
    
    // User selection of value will triger this._itemSelected();
    _itemSelected : function (item) {
        //!DONTCOMBINE
        var value = this._mapNodeToValue(item);
        // updateValue() will store the new value
        // If the change handler didn't return false, also keep a pointer to the node.
        this._userPickedNode= item;
        var rv = this._updateValue(value);
        if (rv == false) delete this._userPickedNode;
        // if a change/changed handler actually called setValue() on the item, return
        // false so we don't clobber the new item value with the user-picked value
        // in the button title, etc.
        if (this.getValue() != value) rv = false;
        // returning false will avoid the item getting marked as selected in the tree.
        return rv;
    },
    
    // Override 'saveValue()' so that 'getSelectedNode()' returns the node associated with the value
    
    saveValue : function (value, isDefault) {
        // userPickedNode set up as part of the change flow - saves us having to look up the
        // node in the tree.
        if (this._userPickedNode != null) {
            this._selectedNode = this._userPickedNode;
            delete this._userPickedNode;
        } else {
            
            this._selectedNode = this._getNode(value);
            if (this.canvas) {
                // If we couldn't find a node, this will update the button title to reflect either 
                // the empty message, or the valuemapped version of this item's value.
                this.canvas.setSelectedItem(this._selectedNode);
            }

        }
        return this.Super("saveValue", arguments);
    },

    // Override CanvasItem.showValue() to set the title of the TreeMenuButton after the value
    // of this form item is set.  The displayValue passed into this method is from the value
    // map defined by FormItem.  It might not be an actual, valid display value, if the record
    // is not cached, so this method tries to find the data record in the ResultTree of the
    // button's Menu.  If such a tree node/record exists in the ResultTree then it is selected.
    // Otherwise this method checks the displayValue to see if it is valid before setting it
    // as the button title.
    showValue : function (displayValue, dataValue) {
        var node = this._getNode(dataValue);
        if (node != null) {
            this.canvas.setSelectedItem(node);
        } else {
            // _mapKey() is an internal method of FormItem.  The second argument set to true forces
            // _mapKey() to return null if the value is not in the valueMap.
            var ds = this.optionDataSource || this.dataSource;
            var isValueInCache = !ds || (this._mapKey(dataValue, true) !== null),
                title = isValueInCache ? displayValue : "";
            this.canvas.setTitle(title);
        }
    },

    // Given a selected node, what is the 'value' that we'll pass back to a 'getValue()' call
    // (same value should work in a 'setValue()' call to select the node in question).
    _mapNodeToValue : function (node) {

        if (this._usePathAsId()) return this.valueTree.getPath(node);
        
        return node[this._getValueFieldName()];
    },
    
    // _usePathAsId - if we're using a tree with the relationships specified as paths, 
    // we use the paths as IDs by default.
    _usePathAsId : function () {
        return (!this.valueField && this.valueTree && (this.valueTree.modelType != "parent"));
    },

    // If we're using a field-name property on the node as a unique node ID, what is it?    
    _getValueFieldName : function () {
        var fieldName = this.valueField;
        if (!fieldName) {
            fieldName = this.valueTree ? this.valueTree.idField 
                                       : this._getPrimaryKeyFieldName();
        }
        
        return fieldName;       
    },
    
    // For databound items, this method determines the primary key fieldName (used to determine
    // the 'value' property based on the selected node).
    _getPrimaryKeyFieldName : function () {
        
        if (!this.getOptionDataSource()) return null;

        if (!this._pkfield) {
            
            var ds = isc.DataSource.getDataSource(this.getOptionDataSource()),
                pkArray = ds.getPrimaryKeyFieldNames(),
                pk = isc.isAn.Array(pkArray) ? pkArray[0] : pkArray;
            
            if (isc.isAn.Array(pkArray) && pkArray.length > 1) {
                this.logWarn("Multiple primary key fields not supported by PickTreeItem - using '" +
                             pk + "' as single primary key field");
            }
            
            this._pkfield = pk;
        }        
        
        return this._pkfield;
    },

    
    // No need for override to 'getValue()' - update value will have already saved out the 
    // selected value
    
    // Special 'getSelectedNode()' method to give us a pointer to the selected object
    //>@method  PickTreeItem.getSelectedNode()  
    //  Returns a pointer to the currently selected node within this item's tree   
    //<
    getSelectedNode : function () {
        // this property is set up when selection changes (or on setValue() calls)
        return this._selectedNode;
    },
    
    // have getSelectedRecord simply fall through to getSelectedNode
    
    getSelectedRecord : function () {
        return this.getSelectedNode();
    },
        
    // on updateValueMap(), refresh our canvas to show the new display value.
    updateValueMap:function (needsRefresh) {
        this.Super("updateValueMap", arguments);
        if (needsRefresh) this.canvas.markForRedraw();
    },
    
    // Given a specified 'value' for this item, find the approprate node in our data tree.
    _getNode : function (value) {
        if (!value) return null;
        
        // If we have a dataSource we're going to be looking up the item in the
        // ResultTree derived from the dataSource - available as this.canvas.getTree().
        var tree = (this.getOptionDataSource() ? this.canvas.getTree() : this.valueTree);
        if (this._usePathAsId()) return this.valueTree.find(value);
        
        return tree.find(this._getValueFieldName(), value);

    },
    
    _shouldAllowExpressions : function () {
        return false;
    },

    
    //> @attr pickTreeItem.optionDataSource (DataSource : null : IRW)
    // @include formItem.optionDataSource
    // @visibility external
    //<

    //> @method pickTreeItem.fetchData()
    // Only applies to databound items (see +link{pickTreeItem.optionDataSource}).<br>
    // Performs a fetch type operation on this item's DataSource to retrieve/refresh the tree
    // of data displayed as rows in this items menu.
    // @visibility external
    //<
    
    fetchData : function () {
        var ods = this.getOptionDataSource();
        if (ods == null) {
            this.logWarn("fetchData() called on pickTree item with no option data source. Ignoring.");
            return;
        }
    
        
        var resultTree = this.canvas.getTree();
        if (!resultTree || !resultTree.invalidateCache) {
            return;
        }
        // The menu reacts to invalidateCache on the resultTree by destroying submenus and
        // regenerating top level items.
        resultTree.invalidateCache();
    },

    _getEmptyDisplayValue : function () {
        if (this.emptyDisplayValue != null) {
            return this.emptyDisplayValue;
        } else {
            // If the emptyDisplayValue property is null then use the default value of
            // the unselectedTitle property of the tree menu button.
            var cl = isc[this.buttonConstructor],
                unselectedTitle = cl && cl.getInstanceProperty("unselectedTitle");
            return unselectedTitle || "";
        }
    },

    //> @method pickTreeItem.setEmptyDisplayValue()
    // Setter for +link{pickTreeItem.emptyDisplayValue}.
    // @param emptyDisplayValue (string)
    // @group display_values
    // @visibility external
    //<
    setEmptyDisplayValue : function (emptyDisplayValue) {
        this.emptyDisplayValue = emptyDisplayValue;
        if (this.button != null) {
            this.button.unselectedTitle = this._getEmptyDisplayValue();
        }
    }
});



//>	@class IPickTreeItem
// Subclass of +link{PickTreeItem} which shows an +link{IMenuButton} rather than a
// simple +link{MenuButton} as it's main button.
//
// @treeLocation Client Reference/Forms/Form Items
// @visibility external
//<
isc.defineClass("IPickTreeItem", "PickTreeItem");
isc.IPickTreeItem.addProperties({

    //> @attr IPickTreeItem.button (AutoChild Canvas : null : R)
    // This item is an +link{AutoChild} generated +link{class:Canvas} displayed by
    // the IPickTreeItem and is an instance of +link{class:ITreeMenuButton} by default.
    // @visibility external
    //<
    buttonConstructor:"ITreeMenuButton"
});

}
