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

 



// Button which will create a hierachichal menu structure based on its own 
// data or dataSource property.
// Values can be selected in the menu and will be displayed in the button's title.
// Path to the selected value will also be hilited in the menu.


// SelectionTreeMenu - simple subclass of menu created by the TreeMenuButton.
// Shows the selected node's path in a custom style.
isc.ClassFactory.defineClass("SelectionTreeMenu", "Menu") 

isc.SelectionTreeMenu.addMethods({

    // On click fire the click handler on our button.
    itemClick : function (item) {  this.inheritedProperties.button._itemSelected(item); },

    // Show a custom base / over style for the selected record's path    
    
    getBaseStyle : function (record, rowNum, colNum) {
        var button = this.inheritedProperties.button;
        if (button._inSelectionPath(record)) return button.selectedBaseStyle;
        return this.Super("getBaseStyle", arguments);
    },
    
    // Override show() to ensure we show the updated selection path by resetting our
    // rows' styles
    show : function () {
        if (this.body) {
            for (var i = 0; i < this.getTotalRows(); i++) {
                this.body.setRowStyle(i);
            }
        }
        return this.Super("show", arguments);
    },
    
    getItemTitle : function (item, a,b,c,d) {
        // Specifying a display field on the button ensures it is used to display items' titles
        var button = this.inheritedProperties.button;
        if (button.displayField) {
            return item[button.displayField];
        }
        // Otherwise pick up the title field from the data-source as normal
        return this.invokeSuper(isc.SelectionTreeMenu, "getItemTitle", item, a,b,c,d);
    }
});


//> @class TreeMenuButton
//
//  Button used to display a hierarchical Menu group for representing / selecting tree data.
// <P>
// <i><b>Important Note:</b> this class should not be used directly - it is exposed purely for
// +link{group:i18nMessages, i18n reasons.}</i>
//
// @inheritsFrom MenuButton
// @treeLocation Client Reference/Control
// @visibility external
//<

// Used by the TreeMenuItem class.
// Supports selection: 
// - Displays the selected value as the button's title by default.
// - Hilites the path to the selected record in its menu

isc.ClassFactory.defineClass("TreeMenuButton", "MenuButton");

//> @class ITreeMenuButton
//
//  Button used to display a hierarchical Menu group for representing / selecting tree data.
//  This is derived from the +link{class:IMenuButton} and is +link{class:StretchImgButton} based.
// <P>
// <i><b>Important Note:</b> this class should not be used directly - it is exposed purely for
// +link{group:i18nMessages, i18n reasons.}</i>
//
// @treeLocation Client Reference/Control
// @visibility external
//<

isc.ClassFactory.defineClass("ITreeMenuButton", "IMenuButton");

isc._treeMenuButtonProps = {
    
    //> @attr treeMenuButton.title (string : null : IRW)
    // Title for this button. If not specified, the selected value from the tree will
    // be displayed instead.
    // @visibility external
    //<
    title:null,
    
    //> @attr treeMenuButton.unselectedTitle (string : "Choose a value" : IRW)
    // If +link{treeMenuButton.title, title} is null, this value will be displayed as a title 
    // when the user has not selected any value from the hierachichal menu.
    // @visibility external
    // @group i18nMessages
    //<
    unselectedTitle:"Choose a value",
    
    //> @attr treeMenuButton.emptyMenuMessage (string : null : IRW)
    // If this button's menu (or any of its submenus) are empty, this property can be used
    // to specify the message to display (as a disabled item) in the empty menu.
    // @visibility external
    //<
    //emptyMenuMessage : null,

    //> @attr treeMenuButton.showPath (boolean : false : IRW)
    // If +link{treeMenuButton.title, title} is null, when the user selects an item, should we 
    // show the full path to the item, or just the item's title as the button's title?
    // @visibility external
    //<    
    showPath:false,
    
    //> @attr treeMenuButton.pathSeparatorString (HTML : "&nbsp;&gt;&nbsp;" : IRW)
    // If +link{treeMenuButton.showPath, showPath} is true, this property specifies what will 
    // appear between the folders in the selected value's path.
    // @visibility external
    //<
    pathSeparatorString : "&nbsp;&gt;&nbsp;",
    
    //> @attr treeMenuButton.selectedBaseStyle (CSSStyleName: "treeMenuSelected" : IRW)
    // Base style to apply to the selected path within the menu. (The "over" version of this
    // style should also be defined in the stylesheet applied to this widget).
    // @visibility external
    //<
    selectedBaseStyle : "treeMenuSelected",
    
    // The title is going to keep changing width, so allow overflow.
    overflow:isc.Canvas.VISIBLE,

    // Override the menuConstructor so our menu is created as a SelectionTreeMenu
    menuConstructor : isc.SelectionTreeMenu,
    
    //> @attr treeMenuButton.loadDataOnDemand (boolean : null : IRW)
    // If this button is showing a databound treeMenu, this attribute dictates whether the data 
    // should the data be loaded on demand or upfront.  The default is to load on demand.
    //<
    //loadDataOnDemand : true,

    //> @attr treeMenuButton.dataProperties (Tree : null : IR)
    // For a <code>TreeMenuButton</code> that uses a DataSource, these properties will be passed to
    // the automatically-created ResultTree.  This can be used for various customizations such as
    // modifying the automatically-chosen +link{tree.parentIdField}.
    // @group databinding
    // @visibility external
    //<
    
    // Optional class-level defaults for the menu
    //menuDefaults : {}
    
    
    // METHODS:
    
    // The title of the button should reflect the selected value (if there is one)
    getTitle : function () {
        // Allow the developer to specify an explicit (static) title.
        if (this.title) return this.title;
      
        var selection = this.getSelectedItem();
        if (selection) {
            
            if (!this.showPath) {
                if (!isc.isA.Menu(this.menu)) this._createMenu(this.menu);
                return this.menu.getItemTitle(selection);
            } else {
                // calling getTree automatically creates this.menu
                var tree = this.getTree();
                var parents = tree.getParents(selection),
                    titleArray = [];
                    
                for (var i = parents.length-1; i >=0; i--) {
                    if (!tree.showRoot && i == parents.length -1) continue;
                    titleArray.add(this.menu.getItemTitle(parents[i]));
                }
                titleArray.add(this.menu.getItemTitle(selection));
                return titleArray.join(this.pathSeparatorString);
            }
        } else {
            return this.unselectedTitle;
        }
    },
    
    // Override the method to actually create the menu (called lazily when the menu is to be 
    // shown for the first time).
    _createMenu : function (properties) {
        properties = isc.addProperties(this.menuDefaults || {}, properties, {
                        
            // All the submenus should have a pointer back to this item.
            inheritedProperties : {
                button:this
            },
            submenuConstructor:this.menuConstructor,

            // Don't want to have to set this property at the menu level.
            canSelectParentItems : this.canSelectParentItems,
            
            dataSource:this.dataSource,
            // If criteria are specified at the button level, pass these through to
            // the actual Menu
            criteria:this.criteria,
            
            data:this.data,

			// EDD
			dataProperties:this.dataProperties

    	});
        
        // If we have one, apply a custom messge for empty submenus to the menu
        if (this.emptyMenuMessage) properties.emptyMessage = this.emptyMenuMessage;
        if (this.loadDataOnDemand != null) properties.loadDataOnDemand = this.loadDataOnDemand;

        var returnVal = this.Super("_createMenu", [properties]);

        this.observe(this.menu, "treeDataLoaded", "observer._treeDataLoaded()");
        
        return returnVal;

    },

    // _treeDataLoaded. If our menu gets its data from a ResultTree, and loadDataOnDemand is
    // false (so all the data is loaded upfront), this method will be fired on initial load.
    _treeDataLoaded : function () {
        //!DONTCOMBINE
        if (this.treeDataLoaded) this.treeDataLoaded();
    },
    
    // getTree method to retrieve a pointer to our tree data object
    getTree : function () {

        if (!isc.isA.Menu(this.menu)) this._createMenu(this.menu);
        // Use this.menu.treeData - this avoids us having to create our own ResultTree if
        // we are databound.
        return this.menu._treeData;
        
    },
    
    // Helper to update the menu data at runtime
    setData : function (data) {
        this.data = data;
        if (this.menu != null) {
            
            if (!isc.isA.Menu(this.menu)) this._createMenu(this.menu);
            this.menu.setData(data);
        }
    },
    
    // On selection we want to:
    // - fire any developer specified handler
    // - change our title to display the selected item
    // - hilite the selection item path when the menu is visible
    _itemSelected : function (item) {
        //!DONTCOMBINE
        // If the developer's change handler returns false, cancel the selection.
        if (this.itemSelected && this.itemSelected(item, this._selectedItem) == false) 
            return;
            
        this.setSelectedItem(item);
    },
    
    setSelectedItem : function (item) {
        // We don't need a full selection object - simply hang onto the last clicked node.
        this._selectedItem = item;
        // setTitle will dynamically recalc the title and update
        // (Note that redraw() will not, if we're showing a label. Noted in StatefulCanvas).
        this.setTitle();

    },
    
    // getSelectedItem() returns the selected item
    getSelectedItem : function () {
        return this._selectedItem;
    },

    // Is some node an ancestor of the current selection?
    // Used for hiliting the current selection in our menus.    
    _inSelectionPath : function (node) {
        var selection = this.getSelectedItem(),
            tree = this.getTree();

        while (selection) {
            if (node == selection) return true;
            selection = tree.getParent(selection);
        }
        
        return false;
    }
    
};

isc.TreeMenuButton.addProperties(isc._treeMenuButtonProps);
isc.ITreeMenuButton.addProperties(isc._treeMenuButtonProps);
    
    
isc.TreeMenuButton.registerStringMethods({
    // itemSelected - handler fired when the user changes the selection.
    itemSelected : "item, oldItem"
});

isc.ITreeMenuButton.registerStringMethods({
    // itemSelected - handler fired when the user changes the selection.
    itemSelected : "item, oldItem"
});


