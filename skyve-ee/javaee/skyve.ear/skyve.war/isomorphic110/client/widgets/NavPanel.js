/*
 * Isomorphic SmartClient
 * Version v11.0p_2016-03-31 (2016-03-31)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */
//> @class NavPanel
// Provides a list or tree of +link{NavItem,navigation items}, each of which specifies a
// component to be displayed in a mutually exclusive fashion in the +link{navPanel.navDeck,navDeck}.
// <p>
// A NavPanel can either have a flat list of <code>NavItems</code> or a hierarchy via
// +link{NavItem.items} - use +link{navPanel.isTree} to explicitly control this.
// <p>
// Because NavPanel extends +link{SplitPane}, it automatically shifts between side-by-side vs
// single panel display on handset-sized devices.  Specifically, the +link{navPanel.navGrid} is
// set as the +link{splitPane.navigationPane} and the +link{navPanel.navDeck} is set as the
// +link{splitPane.detailPane}.
// <p>
// Note that <code>NavPanel</code> is a fairly simple component to replicate by composing other
// SmartClient widgets.  If you need a component that looks roughly like a
// <code>NavPanel</code> but will require lots of visual and behavioral customization, consider
// using the underlying components directly instead of deeply customizing the
// <code>NavPanel</code> class.  A <code>NavPanel</code> is essentially just a +link{TreeGrid}
// and +link{Deck} in a +link{SplitPane}, with a +link{listGrid.recordClick,recordClick}
// handler to call +link{deck.setCurrentPane()} with a component ID stored as an attribute of
// each Record.
//
// @treeLocation Client Reference/Layout
// @visibility external
//<
isc.defineClass("NavPanel", "SplitPane").addClassProperties({

    // Returns the ID of the given NavItem.
    _getItemId : function (item) {
        if (item.id != null) {
            return item.id;

        // The pane is ignored for header and separator items.
        } else if (item.isHeader || item.isSeparator) {
            return null;

        } else if (isc.isA.Canvas(item.pane)) {
            return item.pane.getID();

        // NavItem.pane may be the pane ID.
        } else {
            return item.pane;
        }
    },

    _flattenNavItemTree : function (items) {
        var res = [];
        for (var i = 0, numItems = items == null ? 0 : items.length; i < numItems; ++i) {
            var item = items[i];
            res.add(item);
            if (isc.isAn.Array(item.items)) {
                res.addList(this._flattenNavItemTree(item.items));
            }
        }
        return res;
    }
});

isc.NavPanel.addProperties({

    //> @attr navPanel.isTree (Boolean : null : IR)
    // Whether the +link{NavItem}s form a +link{Tree} or are just a flat list.  If
    // <code>isTree</code> is false, +link{treeGrid.showOpener} will be set false on the
    // +link{navGrid} so that space isn't wasted.
    // <p>
    // The setting for <code>isTree</code> is defaulted immediately before initial draw, based
    // on whether any +link{NavItem} has a list of subitems specified via +link{navItem.items}.
    // If no +link{NavItem}s are provided before draw, <code>isTree</code> defaults to
    // <code>true</code>. Auto-detection is never attempted again even if all
    // <code>NavItems</code> are replaced.
    // <p>
    // Set <code>isTree</code> explicitly if auto-detection doesn't yield the correct result
    // for your application.
    // @visibility external
    //<

    //> @attr navPanel.navGrid (AutoChild TreeGrid : null : IR)
    // The +link{TreeGrid} used to display +link{NavItem}s.
    // @visibility external
    //<
    navGridDefaults: {
        showHeader : false,
        leaveScrollbarGap:false,
        defaultFields: [
            {name: "title"}
        ],
        //>EditMode
        // In edit mode, the separator items need to be enabled so that clicking on them in VB
        // will bring them up in the Component Editor.
        recordIsEnabled : function (record, row, col) {
            var navPanel = this.creator;
            if (navPanel.editingOn && record != null && record.isSeparator) return true;
            return this.Super("recordIsEnabled", arguments);
        },
        //<EditMode
        recordClick : function (treeGrid, record, recordNum, field, fieldNum, value, rawValue) {
            var navPanel = this.creator;

            //>EditMode
            if (navPanel.editingOn) {
                navPanel.setCurrentItem(!record.isHeader && !record.isSeparator && record.canSelect != false ? record : null);

                navPanel.editContext.selectSingleComponent(record);
                // Return false to stop bubbling up, as otherwise the NavPanelEditProxy's
                // click() implementation will be invoked, which will set the edit context's
                // selection to the NavPanel when we want the edit context selection to be
                // the clicked NavItem.
                return false;
            }
            //<EditMode

            if (!record.isHeader && !record.isSeparator && record.canSelect != false) {
                navPanel.setCurrentItem(record);
            }
        },
        recordDoubleClick : function (treeGrid, record, recordNum, field, fieldNum, value, rawValue) {
            //>EditMode
            var navPanel = this.creator;
            if (navPanel.editingOn && !record.isSeparator &&
                navPanel.editProxy.supportsInlineEdit && navPanel.editContext.enableInlineEdit)
            {
                navPanel.editProxy.startItemInlineEditing(record, recordNum);
            }
            //<EditMode
        },
        getIcon : function (record) {
            if (record == null || record.isHeader || record.isSeparator) return null;
            return this.Super("getIcon", arguments);
        }
    },

    navGridConstructor: "TreeGrid",

    // Don't create the listToolStrip by default because a NavPanel does not have a list pane.
    showListToolStrip: false,

    //> @attr navPanel.navDeck (AutoChild Deck : null : IR)
    // The +link{Deck} area where components specified via +link{navItem.pane} are displayed.
    // @visibility external
    //<
    navDeckDefaults: {
        currentPane: null,

        currentPaneChanged : function (currentPane) {
            var navPanel = this.creator;
            if (navPanel._ignoreCurrentPaneChanged) return;

            if (currentPane == null) {
                navPanel.setCurrentItem(null);
                return;
            }

            // Find the NavItem for the new currentPane.
            var items = isc.NavPanel._flattenNavItemTree(navPanel.navItems);
            for (var i = 0, numItems = items.length; i < numItems; ++i) {
                var item = items[i];
                if (item.isHeader || item.isSeparator || item.canSelect == false) continue;

                var pane = item.pane;
                if (pane) {
                    if (isc.isA.String(pane) && isc.isA.Canvas(window[pane])) {
                        pane = window[pane];
                    }
                    if (currentPane === pane) {
                        navPanel.setCurrentItem(item);
                        return;
                    }
                }
            }
            navPanel.logWarn("navDeck.currentPaneChanged(): Failed to find the selectable NavItem corresponding to " + isc.echo(currentPane));
        }
    },

    navDeckConstructor: "Deck",

    //> @attr navPanel.headerStyle (CSSStyleName : "navItemHeader" : IR)
    // CSS style used when +link{NavItem.isHeader} is set on an item.
    // May be overridden for a specific header item by +link{NavItem.customStyle}.
    // @visibility external
    //<
    headerStyle: "navItemHeader",

    //> @attr navPanel.navItems (Array of NavItem : null : IRW)
    // Top-level navigation items to display.  You can optionally specify a tree of items using
    // +link{navItem.items}.
    // <p>
    // A separator between navigation items can be created by setting +link{NavItem.isSeparator},
    // and a header can be created via +link{NavItem.isHeader}.
    // <p>
    // Each non-separator and non-header <code>NavItem</code> specifies a component to be displayed
    // in the +link{NavPanel.navDeck} via +link{NavItem.pane}.
    // <p>
    // <code>NavItem</code>s can also be individually styled via +link{ListGridRecord._baseStyle}
    // or +link{NavItem.customStyle}.
    // @visibility external
    //<

    //> @attr navPanel.currentItem (NavItem : null : IRW)
    // The current +link{NavItem} whose +link{NavItem.pane,pane} is showing in the
    // +link{NavPanel.navDeck,navDeck}.  This must be an item of this <code>NavPanel</code> if
    // set.
    // @visibility external
    //<

    //> @attr navPanel.currentItemId (identifier : null : IRW)
    // The ID of the current +link{NavItem} whose +link{NavItem.pane,pane} is showing in the
    // +link{NavPanel.navDeck,navDeck}.  The <code>NavItem</code> must be an item of this
    // <code>NavPanel</code> if set.
    // <p>
    // The ID of a <code>NavItem</code> is the item's +link{NavItem.id} if set; otherwise, it
    // is the ID of the item's +link{NavItem.pane}, though <code>currentItemId</code> may be
    // initialized to either identifier.
    // @visibility external
    //<

    //> @object NavItem
    // Properties for a navigation item in a +link{NavPanel}.
    // @inheritsFrom TreeNode
    // @treeLocation Client Reference/Layout/NavPanel
    // @visibility external
    //<

    //> @attr navItem.id (identifier : null : IR)
    // An optional ID for this <code>NavItem</code>.  If specified, this must be unique within
    // the <code>NavPanel</code>.
    // @visibility external
    //<

    //> @attr navItem.title (HTMLString : null : IR)
    // Title to show for this <code>NavItem</code>.
    // @visibility external
    //<

    //> @attr navItem.icon (SCImgURL : null : IR)
    // Icon to show for this <code>NavItem</code>.  If not specified, the
    // +link{TreeGrid.folderIcon,navGrid's folderIcon} is used.
    // @visibility external
    //<

    //> @attr navItem.items (Array of NavItem : null : IR)
    // Optional subitems of this <code>NavItem</code>.
    // @visibility external
    //<

    //> @attr navItem.isHeader (Boolean : null : IR)
    // If set, this <code>NavItem</code> will be styled like a header.  In this case +link{navItem.pane}
    // is ignored and nothing happens when the header is clicked.  However, +link{navItem.items} can
    // still be configured to place items hierarchically under the header.
    // @visibility external
    //<

    //> @attr navItem.customStyle (CSSStyleName : null : IR)
    // CSS style name used for this <code>NavItem</code>.  If set and this <code>NavItem</code>
    // is a +link{NavItem.isHeader,header}, this overrides the <code>NavPanel</code>'s
    // +link{NavPanel.headerStyle}.
    // @visibility external
    //<

    //> @attr navItem.isSeparator (Boolean : null : IR)
    // If set, this <code>NavItem</code> will be styled as a separator.  A separator does not
    // have a +link{NavItem.pane,pane} and nothing happens when the separator is clicked.
    // @visibility external
    //<

    //> @attr navItem.pane (Canvas | identifier : null : IR)
    // Component to display in the +link{navPanel.navDeck} when this <code>NavItem</code> is
    // selected.
    // <p>
    // A component can be provided directly, or its ID can be provided.
    // @visibility external
    //<

    initWidget : function () {
        this.navigationPane = this.navGrid = this.createAutoChild("navGrid", {
            isSeparatorProperty: "isSeparator"
        });

        var navDeckDynamicProps = {
            currentPane: null,
            panes: null
        };
        var currentItem = this.currentItem,
            currentItemId = this.currentItemId;
        if (currentItem == null && currentItemId) currentItem = this.currentItem = this._findItemById(currentItemId);
        if (currentItem != null) {
            navDeckDynamicProps.currentPane = currentItem.pane;
            navDeckDynamicProps.panes = [currentItem.pane];

            currentItemId = isc.NavPanel._getItemId(currentItem);
            this.currentItemId = currentItemId;

            // This SplitPane should initially display the navDeck/"detail" pane.
            this.currentPane = "detail";
            this.detailTitle = currentItem.title;
        }
        this.detailPane = this.navDeck = this.createAutoChild("navDeck", navDeckDynamicProps);

        this.Super("initWidget", arguments);
    },

    _processTreeAndReturnNavItemsPanes : function (items) {
        items = isc.NavPanel._flattenNavItemTree(items);
        var res = [];
        for (var i = 0, numItems = items.length; i < numItems; ++i) {
            var item = items[i];
            if (item.isHeader && !item.customStyle) {
                item.customStyle = this.headerStyle;
            }
            if (item.isSeparator || item.canSelect == false) continue;
            if (item.pane) {
                if (isc.isA.String(item.pane) && isc.isA.Canvas(window[item.pane])) {
                    res.add(window[item.pane]);
                } else {
                    res.add(item.pane);
                }
            }
        }
        return res;
    },

    setNavItems : function (navItems) {
        this.navItems = navItems;
        this.dataChanged(navItems);
    },

    //> @method navPanel.setCurrentItem()
    // Setter for +link{NavPanel.currentItem}.  Note that +link{NavPanel.currentItemId} is also
    // updated by this setter.
    // @param [newCurrentItem] (NavItem) the new <code>currentItem</code>.  May be <code>null</code>
    // to hide the current item.  If <code>newCurrentItem</code> is a separator or header item,
    // then setCurrentItem() has no effect.
    // @visibility external
    //<
    setCurrentItem : function (newCurrentItem) {
        if ((newCurrentItem == null && this.currentItem == null) ||
            newCurrentItem === this.currentItem)
        {
            return;
        }

        if (newCurrentItem != null) {
            if (newCurrentItem.isHeader || newCurrentItem.isSeparator) {
                return;
            }

            this.currentItem = newCurrentItem;
            this.currentItemId = isc.NavPanel._getItemId(newCurrentItem);

        } else {
            this.currentItem = null;
            this.currentItemId = null;
        }

        //>EditMode
        if (this.editingOn) {
            this.editContext.setNodeProperties(this.editNode, { currentItemId: this.currentItemId });
        }
        //<EditMode

        this._ignoreCurrentPaneChanged = true;
        if (newCurrentItem != null && newCurrentItem.pane != null) {
            this.navDeck.setCurrentPane(newCurrentItem.pane);
            this.showDetailPane(newCurrentItem.title, null, "forward");
            this.navGrid.selectSingleRecord(newCurrentItem);
        } else {
            this.navDeck.hideCurrentPane();
            this.showNavigationPane("back");
            this.setDetailTitle(null);
            this.navGrid.deselectAllRecords();
        }
        this._ignoreCurrentPaneChanged = false;
    },

    _findItemById : function (itemId) {
        if (!itemId) return null;

        var itemsByIdCache = this._itemsByIdCache;
        if (itemsByIdCache != null) return itemsByIdCache[itemId];

        itemsByIdCache = this._itemsByIdCache = {};
        if (this.navItems == null) return null;

        var origItemId = itemId;

        // Build a complete cache.
        var itemsArrays = [this.navItems];
        for (var i = 0; i < itemsArrays.length; ++i) {
            var items = itemsArrays[i];
            
            for (var j = 0, numItems = items.length; j < numItems; ++j) {
                var item = items[j];
                itemId = isc.NavPanel._getItemId(item);
                if (itemId) {
                    if (itemsByIdCache.hasOwnProperty(itemId)) {
                        this.logWarn("This NavPanel has two or more items with the same ID:'" + itemId + "'.");
                    } else {
                        itemsByIdCache[itemId] = item;
                    }
                }
                var subitems = item.items;
                if (isc.isAn.Array(subitems)) itemsArrays.add(subitems);
            }
        }
        // Go through all items arrays again, adding the items to the cache by their panes' IDs.
        for (var i = 0; i < itemsArrays.length; ++i) {
            var items = itemsArrays[i];
            
            for (var j = 0, numItems = items.length; j < numItems; ++j) {
                var item = items[j];
                if (item.isHeader || item.isSeparator) continue;
                var paneID;
                if (isc.isA.Canvas(item.pane)) paneID = item.pane.getID();
                else paneID = item.pane;
                if (paneID && !itemsByIdCache.hasOwnProperty(paneID)) {
                    itemsByIdCache[paneID] = item;
                }
            }
        }

        return itemsByIdCache[origItemId];
    },

    //> @method navPanel.setCurrentItemId()
    // Setter for +link{NavPanel.currentItemId}.  Note that +link{NavPanel.currentItem} is also
    // updated by this setter and <code>this.currentItemId</code> may be normalized to a different
    // identifier.
    // @param [newCurrentItemId] (identifier) the ID of the new current item, which may be either
    // the item's +link{NavItem.id} or the ID of the item's +link{NavItem.pane}.  May be
    // <code>null</code> or an empty string to hide the current item.  If the item with ID
    // <code>newCurrentItemId</code> is a separator or header item, then setCurrentItemId() has no effect.
    // @visibility external
    //<
    setCurrentItemId : function (newCurrentItemId) {
        if (this.currentItemId == newCurrentItemId) return;
        this.setCurrentItem(this._findItemById(newCurrentItemId));
    },

    observeData : function (data, obsToRemove) {
        obsToRemove.remove(data);
        if (!this.isObserving (data, "dataChanged")) {
            if (!this._addedObservers) this._addedObservers = [];
            if (!this._addedObservers.contains(data)) this._addedObservers.add(data);
            this.observe(data, "dataChanged", "observer.dataChanged(observed)");
        }
        for (var i = 0; i < data.length; i++) {
            if (!data[i].items) data[i].items = [];
            this.observeData(data[i].items, obsToRemove);
        }
    },

    dataChanged : function (data) {
        this._itemsByIdCache = null;
        this.navDeck.setPanes(this._processTreeAndReturnNavItemsPanes(this.navItems));

        //>EditMode
        // If this NavPanel is being edited and an item is added or removed, then
        // check whether we should update the isTree default.
        // This allows isTree/showOpener to be updated at runtime so that dragging
        // a NavItem into a top-level item will leave space for the opener icon.
        // Similarly, removing the last subitem of a top-level item will hide the
        // opener.
        if (this.editingOn) {
            var isTree = false;
            var navItems = this.navItems;
            for (var i = 0, numNavItems = navItems.length; i < numNavItems; ++i) {
                var item = navItems[i];
                if (isc.isAn.Array(item.items) && item.items.length > 0) {
                    isTree = true;
                    break;
                }
            }
            if (this.isTree != isTree) {
                this.editContext.setNodeProperties(this.editNode, { isTree: isTree });
                this.navGrid.showOpener = isTree;
            }
        }
        //<EditMode

        var newData = isc.Tree.create({
            modelType: "children",
            nameProperty: "title",
            childrenProperty: "items",
            root: {items: this.navItems},
            isFolder : function (node) {
                // all nodes should be folders - this needs to be able to drop navItems to 
                // leafNodes
                return true;
            }
        });
        this.navGrid.setData(newData);
        newData.openAll();
        var obsToRemove = this._addedObservers ? isc.shallowClone(this._addedObservers) : [];
        this.observeData(this.navItems, obsToRemove);
        this._addedObservers.removeList(obsToRemove)
        for (var i = 0; i < obsToRemove.length; i++) {
            this.ignore(obsToRemove[i], "dataChanged");
        }

        if (this.currentItem != null && !newData.contains(this.currentItem)) {
            this.setCurrentItem(null);
        }
    },

    draw : function () {
        if (!this._navItemsInitialised) {
            if (this.navItems && this.navItems.length > 0) {
                if (this.isTree == null) {
                    this.isTree = false;
                    for (var i = 0; i < this.navItems.length; i++) {
                        if (this.navItems[i].items && this.navItems[i].items.length > 0) {
                            this.isTree = true;
                            break;
                        }
                    }
                }
            } else {
                if (this.isTree == null) this.isTree = true;
                this.navItems = [];
            }
            this.navGrid.showOpener = this.isTree;
            this.setNavItems(this.navItems);
            this._navItemsInitialised = true;
        }
        this.Super("draw", arguments);
    }

    //>EditMode
    ,
    // This is called by NavPanelEditProxy.onFolderDrop().
    setItemPane : function (item, pane) {
        this._itemsByIdCache = null;
        item.pane = pane;
        this.navDeck.addPane(pane);
        if (this.currentItem === item) {
            this._ignoreCurrentPaneChanged = true;
            this.navDeck.setCurrentPane(pane);
            this._ignoreCurrentPaneChanged = false;
            this.currentItem = item;
            this.currentItemId = isc.NavPanel._getItemId(item);
        }
    },

    setDescendantEditableProperties : function (item, properties, editNode, editContext, level) {
        
        this.Super("setDescendantEditableProperties", arguments);

        if (item === this.currentItem) {
            // If the item's isHeader or isSeparator changed, see if the item was made into a
            // header or separator item. If so, then clear the current item because a header
            // or separator item is not able to be the current item.
            if ((properties.hasOwnProperty("isHeader") || properties.hasOwnProperty("isSeparator")) &&
                (item.isHeader || item.isSeparator))
            {
                this.setCurrentItem(null);

            } else {
                // If the item's ID was changed, update the NavPanel's currentItemId.
                if (properties.hasOwnProperty("id")) {
                    var currentItemId = this.currentItemId = isc.NavPanel._getItemId(item);
                    this.editContext.setNodeProperties(this.editNode, { currentItemId: currentItemId });
                }

                // If the item's title was changed and the item has a pane, then update the
                // NavPanel's detailTitle.
                if (properties.hasOwnProperty("title") && item.pane != null) {
                    this.setDetailTitle(item.title);
                }
            }
        }

        
        // Restore the item's customStyle from the defaults. This is needed because when a NavItem
        // is turned into a header item, the item's customStyle is overwritten with the
        // NavPanel.headerStyle if a customStyle is not set. If the NavItem is subsequently
        // turned into a regular item, or a separator item, the headerStyle might still be set
        // to the NavPanel.headerStyle from when it was a header item.
        item.customStyle = editNode.defaults.customStyle;

        // The properties of a NavItem object are read-only. If they are changed, we need to
        // set the navItems again.
        this.setNavItems(this.navItems.duplicate());
    }
    //<EditMode
});
