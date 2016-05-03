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
//> @class FormEditProxy
// +link{EditProxy} that handles +link{DynamicForm,DynamicForms} when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
isc.defineClass("FormEditProxy", "CanvasEditProxy").addMethods({

    defaultDropMargin: 5,
    dropMargin: 5,

    setEditMode : function (editingOn) {
        this.Super("setEditMode", arguments);

        // Throw away anything the user might have typed in edit or live mode
        this.creator.resetValues();
    },

    getOverrideProperties : function () {
        var properties = this.Super("getOverrideProperties", arguments);
        properties = isc.addProperties({}, properties, {
            // Add ability to drop items / add columns
            canDropItems: true,
            canAddColumns: true
        });
        if (this.isAbsoluteLayout()) {
            properties.childrenSnapToGrid = true;
            properties.childrenSnapResizeToGrid = true;
        }
        return properties;
    },

    willAcceptDrop : function (changeObjectSelection) {
        var liveObject = this.creator;

        // Prevent accepting drop of form onto itself
        var source = liveObject.ns.EH.dragTarget;
        if (liveObject == source) {
            return false;
        }

        return this.Super("willAcceptDrop", arguments);
    },

    dropOver : function () {
        var liveObject = this.creator;

        if (liveObject.canDropItems != true) return false;
        if (!this.willAcceptDrop()) {
            if (liveObject == liveObject.ns.EH.dragTarget) {
                return;
            }
            return false;
        }
        this._lastDragOverItem = null;
        // just to be safe
        liveObject.hideDragLine();

        // If this component will not accept the because it would
        // be hierarchical, allow the event to continue bubbling
        // up. This lets a parent component properly show a snap
        // grid if needed.
        if (!this.canDropAtLevel()) return;

        // Show snap grid
        this._showSnapGrid(true);

        return isc.EH.STOP_BUBBLING;        
    },

    dropMove : function () {
        var liveObject = this.creator;

        if (!liveObject.ns.EH.getDragTarget()) return false;
        if (liveObject.canDropItems != true) return false;
        if (!this.willAcceptDrop()) return false;

        // Need to know details on the drag rect position to use if for absolute positioning.
        // This information is not available during drop() event.
        this._lastDragRect = liveObject.ns.EH.getDragRect();

        // DataSource is a special case - we accept drop, but show no drag line
        var item = liveObject.ns.EH.getDragTarget().getDragData();
        if (isc.isAn.Array(item)) item = item[0];
        if (item && (item.type || item.className) == "DataSource" || this.isAbsoluteLayout()) {
            liveObject.hideDragLine();
            return isc.EH.STOP_BUBBLING;
        }

        // If drop will be passed through, don't show the drag line at all
        if (this.shouldPassDropThrough()) {
            liveObject.hideDragLine();
            return;
        }
        
        // If the form has no items, indicate insertion at the left of the form
        if (liveObject.getItems().length == 0) {
            isc.EditContext.hideAncestorDragDropLines(liveObject);
            liveObject.showDragLineForForm();
            return isc.EH.STOP_BUBBLING;
        }

        var event = liveObject.ns.EH.lastEvent,
            overItem = liveObject.getItemAtPageOffset(event.x, event.y),
            dropItem = liveObject.getNearestItem(event.x, event.y);

        //if (this._lastDragOverItem && this._lastDragOverItem != dropItem) {
            // still over an item but not the same one
        //}

        if (dropItem) {
            isc.EditContext.hideAncestorDragDropLines(liveObject);
            liveObject.showDragLineForItem(dropItem, event.x, event.y);
        } else {
            liveObject.hideDragLine();
        }

        this._lastDragOverItem = dropItem;

        return isc.EH.STOP_BUBBLING;        
    },

    dropOut : function () {
        this.creator.hideDragLine();

        // If this component will not accept the because it would
        // be hierarchical, allow the event to continue bubbling
        // up. This lets a parent component properly show a snap
        // grid if needed.
        if (!this.canDropAtLevel()) return;

        this.showSelectedAppearance(false);

        // Hide snap grid
        this._showSnapGrid(false);
    },

    drop : function () {
        // DataSource is a special case - it's the only non-visual property that users can drag
        // and drop and a position within the form doesn't make sense
        var liveObject = this.creator,
            dropItem = liveObject.ns.EH.getDragTarget().getDragData();
        if (isc.isAn.Array(dropItem)) dropItem = dropItem[0];
        if ((dropItem && (dropItem.type || dropItem.className) == "DataSource") ||
                (!this.isAbsoluteLayout() && liveObject.getItems().length == 0))     // special case of empty for in normal layout
        {
            if (this.shouldPassDropThrough()) {
                liveObject.hideDragLine();
                return;
            }
            this.itemDrop(liveObject.ns.EH.getDragTarget(), 0, 0, 0);
            return isc.EH.STOP_BUBBLING;
        }

        if (this.isAbsoluteLayout()) {
            // Assign position based on the dragRect because the mouse pointer is
            // likely offset from there into what was the dragHandle and we want
            // the drop to occur where the target outline shows
            var dragRect = this._lastDragRect,
                left = (dragRect ? dragRect[0] - liveObject.getPageLeft() : liveObject.getOffsetX()),
                top = (dragRect ? dragRect[1] - liveObject.getPageTop() : liveObject.getOffsetY())
            ;
            this.itemAbsoluteDrop(liveObject.ns.EH.getDragTarget(), liveObject.getItemDropIndex(item), top, left);
            return isc.EH.STOP_BUBBLING;
        }

        if (!this._lastDragOverItem) {
            isc.logWarn("lastDragOverItem not set, cannot drop", "dragDrop");
            return;
        }
        
        var item = this._lastDragOverItem,
            dropOffsets = liveObject.getItemTableOffsets(item),
            side = item.dropSide,
            index = item._dragItemIndex,
            insertIndex = liveObject.getItemDropIndex(item, side);

        this._lastDragOverItem = null;
        if (this.shouldPassDropThrough()) {
            liveObject.hideDragLine();
            return;
        }

        if (insertIndex != null && insertIndex >= 0) {

            if (liveObject.parentElement) {
                if (liveObject.parentElement.hideDropLine) liveObject.parentElement.hideDropLine();
            }
        
            // Note that we cache a copy of _rowTable because the modifyFormOnDrop() method may
            // end up invalidating the table layout, and thus clearing _rowTable in the middle of
            // its processing
            var rowTable = liveObject.items._rowTable.duplicate();
            this.modifyFormOnDrop(item, dropOffsets.top, dropOffsets.left, side, rowTable);
        }

        liveObject.hideDragLine();
        return isc.EH.STOP_BUBBLING;        
    },

    // is DynamicForm in absolute layout mode?
    isAbsoluteLayout : function () {
        return this.creator._absPos();
    },

    itemDrop : function (item, itemIndex, rowNum, colNum, side, callback) {
        var liveObject = this.creator;
            
        var source = item.getDragData();
        // If source is null, this is probably because we are drag-repositioning an existing
        // item within a DynamicForm (or from one DF to another) - the source is the component 
        // itself
        if (source == null) {
            source = isc.EH.dragTarget;
            if (isc.isA.FormItemProxyCanvas(source)) {
                this.logInfo("The dragTarget is a FormItemProxyCanvas for " + 
                            source.formItem, "editModeDragTarget");
                source = source.formItem;
            }
        }

        // Don't allow form to be dropped onto itself
        if (liveObject == source) return;

        if (!item.isA("Palette")) {
            var tree = liveObject.editContext.getEditNodeTree(),
                oldParent = tree.getParent(source.editNode),
                oldIndex = tree.getChildren(oldParent).indexOf(source.editNode),
                editNode = source.editNode;
            
            editNode = this.itemDropping(editNode, itemIndex, true);
            if (!editNode) return;

            liveObject.editContext.removeNode(editNode);
            
            // If we've moved the child component to a slot further down in the same parent, 
            // indices will now be off by one because we've just removed it from its old slot
            if (oldParent == liveObject.editNode && itemIndex > oldIndex) itemIndex--;

            var node = liveObject.editContext.addNode(source.editNode, liveObject.editNode, itemIndex);
            if (node && node.liveObject) {
                isc.EditContext.delayCall("selectCanvasOrFormItem", [node.liveObject, true], 200);
            }
            
            return node;
        } else {
            // We're dealing with a drag of a new item from a component palette
            var paletteNode = item.transferDragData();
            if (isc.isAn.Array(paletteNode)) paletteNode = paletteNode[0];
            var editNode = liveObject.editContext.makeEditNode(paletteNode);

            // loadData() operates asynchronously, so we'll have to finish the item drop off-thread
            if (editNode.loadData && !editNode.isLoaded) {
                var editProxy = this;
                editNode.loadData(editNode, function (loadedNode) {
                    loadedNode = loadedNode || editNode
                    loadedNode.isLoaded = true;
                    editProxy.completeItemDrop(loadedNode, itemIndex, rowNum, colNum, side, callback);
                    loadedNode.dropped = editNode.dropped;
                });
                return;
            }

            this.completeItemDrop(editNode, itemIndex, rowNum, colNum, side, callback);
        }
    },

    itemAbsoluteDrop : function (item, itemIndex, top, left, callback) {
        var liveObject = this.creator;

        var source = item.getDragData();
        // If source is null, this is probably because we are drag-repositioning an existing
        // item within a DynamicForm (or from one DF to another) - the source is the component 
        // itself
        if (source == null) {
            source = isc.EH.dragTarget;
            if (isc.isA.FormItemProxyCanvas(source)) {
                this.logInfo("The dragTarget is a FormItemProxyCanvas for " + 
                            source.formItem, "editModeDragTarget");
                source = source.formItem;
            }
        }

        // Don't allow form to be dropped onto itself
        if (liveObject == source) return;

        if (!item.isA("Palette")) {
            var tree = liveObject.editContext.getEditNodeTree(),
                oldParent = tree.getParent(source.editNode),
                oldIndex = tree.getChildren(oldParent).indexOf(source.editNode),
                editNode = source.editNode,
                node;

            editNode = this.itemDropping(editNode, itemIndex, true);
            if (!editNode) return;

            if (oldParent.liveObject != liveObject) {
                // Moving node from another parent
                liveObject.editContext.removeNode(editNode);

                // If we've moved the child component to a slot further down in the same parent, 
                // indices will now be off by one because we've just removed it from its old slot
                if (oldParent == liveObject.editNode && itemIndex > oldIndex) itemIndex--;

                var parentProperty = (!isc.isA.FormItemProxyCanvas(item) ? "children" : null);

                node = liveObject.editContext.addNode(editNode, liveObject.editNode, itemIndex, parentProperty);
            } else {
                // Moving node on same parent - repositioning
                node = editNode;
            }
            if (node && node.liveObject) {
                node.liveObject.editContext.setNodeProperties(node, {
                    top: top,
                    left: left
                });

                isc.EditContext.delayCall("selectCanvasOrFormItem", [node.liveObject, true], 200);
            }
            return node;
        } else {
            // We're dealing with a drag of a new item from a component palette
            var paletteNode = item.transferDragData();
            if (isc.isAn.Array(paletteNode)) paletteNode = paletteNode[0];
            var editNode = liveObject.editContext.makeEditNode(paletteNode);

            // loadData() operates asynchronously, so we'll have to finish the item drop off-thread
            if (editNode.loadData && !editNode.isLoaded) {
                var editProxy = this;
                editNode.loadData(editNode, function (loadedNode) {
                    loadedNode = loadedNode || editNode
                    loadedNode.isLoaded = true;
                    editProxy.completeItemAbsoluteDrop(loadedNode, itemIndex, top, left, callback)
                    loadedNode.dropped = editNode.dropped;
                });
                return;
            }

            this.completeItemAbsoluteDrop(editNode, itemIndex, top, left, callback);
        }
    },

    completeItemDrop : function (editNode, itemIndex, rowNum, colNum, side, callback) {
        var liveObject = this.creator,
            sourceObject = editNode.liveObject,
            skipTitleEdit,
            canvasEditNode;
        if (isc.isA.Button(sourceObject) || isc.isAn.IButton(sourceObject)) {
            // Special case - Buttons become ButtonItems
            editNode = liveObject.editContext.makeEditNode({
                type: "ButtonItem", 
                title: sourceObject.title,
                defaults : editNode.defaults
            })
        } else if (isc.isA.Canvas(sourceObject)) {
            skipTitleEdit = true;
            canvasEditNode = editNode;
            editNode = liveObject.editContext.makeEditNode({type: "CanvasItem"});
            isc.addProperties(editNode.defaults, {
                canvas: sourceObject,
                showTitle: false,
                startRow: true,
                endRow: true,
                width: "*",
                colSpan: "*"
            });
        }
        editNode.dropped = true;
        
        editNode = this.itemDropping(editNode, itemIndex, true);
        if (!editNode) return;

        var nodeAdded = liveObject.editContext.addNode(editNode, liveObject.editNode, itemIndex);
        
        if (nodeAdded) {

            isc.EditContext.clearSchemaProperties(nodeAdded);
        
            if (canvasEditNode) {
                nodeAdded = liveObject.editContext.addNode(canvasEditNode, nodeAdded, 0);
                

                // FIXME: Need a cleaner factoring here (see also Layout.dropItem())
                if (isc.isA.TabSet(sourceObject)) {
                    
                    sourceObject.delayCall("showAddTabEditor", [], 1000);
                }
            }
            
            // Make sure nodeAdded.liveObject is the actual object and not
            // just a template
            liveObject.editContext.getLiveObject(nodeAdded);

            // If we've just dropped a palette node that contained a reference to a dataSource,
            // do a forced set of that dataSource on the liveObject.  This will take it through
            // any special editMode steps - for example, it will cause a DynamicForm to have a 
            // set of fields generated for it and added to the project tree
            if (nodeAdded.liveObject.dataSource) {
                //this.logWarn("calling setDataSource on: " + nodeAdded.liveObject);
                nodeAdded.liveObject.editProxy.setDataSource(nodeAdded.liveObject.dataSource, null, true);
            }

            if (liveObject.editingOn) {
                var item = nodeAdded.liveObject;
                if (item.setEditMode) item.setEditMode(true, item.editContext, item.editNode);
            }

            isc.EditContext.delayCall("selectCanvasOrFormItem", [editNode.liveObject, true], 200);
            
            
            if (skipTitleEdit) {
                if (nodeAdded.liveObject.inlineEditOnDrop) {
                    nodeAdded.liveObject.editProxy.delayCall("startInlineEditing");
                }                    
            } else if (nodeAdded.showTitle != false && nodeAdded.liveObject.editProxy) {
                nodeAdded.liveObject.editProxy.delayCall("editTitle");
            }
        }
        if (callback) this.fireCallback(callback, "node", [nodeAdded]);
    },

    completeItemAbsoluteDrop : function (editNode, itemIndex, top, left, callback) {
        var liveObject = this.creator,
            sourceObject = editNode.liveObject,
            parentProperty
        ;
        if (isc.isA.Canvas(sourceObject)) {
            // A canvas dropped onto an absolute layout DynamicForm is added to the
            // form.children property
            parentProperty = "children";
        }
        editNode.dropped = true;

        editNode = this.itemDropping(editNode, itemIndex, true);
        if (!editNode) return;

        var nodeAdded = liveObject.editContext.addNode(editNode, liveObject.editNode, itemIndex, parentProperty);
        
        if (nodeAdded) {

            isc.EditContext.clearSchemaProperties(nodeAdded);
        
            // Make sure nodeAdded.liveObject is the actual object and not
            // just a template
            liveObject.editContext.getLiveObject(nodeAdded);

            // If we've just dropped a palette node that contained a reference to a dataSource,
            // do a forced set of that dataSource on the liveObject.  This will take it through
            // any special editMode steps - for example, it will cause a DynamicForm to have a 
            // set of fields generated for it and added to the project tree
            if (nodeAdded.liveObject.dataSource) {
                //this.logWarn("calling setDataSource on: " + nodeAdded.liveObject);
                nodeAdded.liveObject.editProxy.setDataSource(nodeAdded.liveObject.dataSource, null, true);
            }

            // Set position of newly dropped object
            liveObject.editContext.setNodeProperties(nodeAdded, {
                top: top,
                left: left
            });

            if (liveObject.editingOn && nodeAdded.liveObject.setEditMode) {
                var item = nodeAdded.liveObject;
                item.setEditMode(true, item.editContext, item.editNode);
            }

            isc.EditContext.delayCall("selectCanvasOrFormItem", [editNode.liveObject, true], 200);
        }
        if (callback) this.fireCallback(callback, "node", [nodeAdded]);
    },

    // Modifies the form to accommodate the pending drop by adding columns and/or SpacerItems as 
    // necessary, then performs the actual drop
    modifyFormOnDrop : function (item, rowNum, colNum, side, rowTable) {
        var liveObject = this.creator;

        if (liveObject.canAddColumns == false) return;

        var dropItem = liveObject.ns.EH.getDragTarget().getDragData(),
            dropItemCols,
            draggingFromRow,
            draggingFromIndex;
        
        if (!dropItem) {
            // We're drag-positioning an existing item
            dropItem = liveObject.ns.EH.getDragTarget();
            if (!isc.isA.FormItemProxyCanvas(dropItem)) {
                this.logWarn("In modifyFormOnDrop the drag target was not a FormItemProxyCanvas");
                return;
            }
            dropItem = dropItem.formItem;
            var lastIndex = -1;
            // If the item we're dragging is in this form, note its location so that we can clean
            // up where it came from
            for (var i = 0; i < rowTable.length; i++) {
                for (var j = 0; j < rowTable[i].length; j++) {
                    if (rowTable[i][j] == lastIndex) continue;
                    lastIndex = rowTable[i][j];
                    if (liveObject.items[lastIndex] == dropItem) {
                        draggingFromRow = i;
                        draggingFromIndex = lastIndex;
                        break;
                    }
                }
            }
            var dragPositioning = true;
        } else {
            // Manually create a FormItem using the config that will be used to create the real 
            // object.  We need to do this because we need to know things about that object that
            // can only be easily discovered by creating and then inspecting it - eg, colSpan, 
            // title attributes and whether startRow or endRow are set
            if (isc.isAn.Array(dropItem)) dropItem = dropItem[0];
            var type = dropItem.type || dropItem.className;
            var theClass = isc.ClassFactory.getClass(type);
            if (isc.isA.FormItem(theClass)) {
                dropItem = liveObject.createItem(dropItem, type);
            } else {
                // This is not completely accurate, but it gives us enough info for placement and 
                // column occupancy calculation.  dropItem() differentiates between Buttons and 
                // other types of Canvas, but for our purposes here it's enough to know that non-
                // FormItem items will occupy one cell and don't have endRow/startRow set
                dropItem = liveObject.createItem({type: "CanvasItem", showTitle: false}, "CanvasItem");
            }
            var dragPositioning = false;
        }

        dropItemCols = this.getAdjustedColSpan(dropItem);

        // If we've previously set startRow or endRow on the item we're dropping, clear them
        if ((dropItem.startRow && dropItem._startRowSetByBuilder) || 
            (dropItem.endRow && dropItem._endRowSetByBuilder)) {
            dropItem.editContext.setNodeProperties(dropItem.editNode, {
                startRow: null, 
                _startRowSetByBuilder: null,
                endRow: null, 
                _endRowSetByBuilder: null
            });
        }
        
        // If we're in drag-reposition mode and the rowNum we're dropping on is not the row we're 
        // dragging from, we could end up with a situation where a row contains nothing but spacers.
        // Detect when this situation is about to arise and mark the spacers for later deletion
        var spacersToDelete = [];
        if (dragPositioning && draggingFromRow) {
            var fromRow = rowTable[draggingFromRow],
                lastIndex = -1;
            for (var i = 0; i < fromRow.length; i++) {
                if (fromRow[i] != lastIndex) {
                    lastIndex = fromRow[i];
                    if (liveObject.items[lastIndex] == dropItem) continue; 
                    if (isc.isA.SpacerItem(liveObject.items[lastIndex]) && 
                            liveObject.items[lastIndex]._generatedByBuilder)
                    {
                        this.logDebug("Marking spacer " + liveObject.items[lastIndex].name + " for removal", 
                                      "formItemDragDrop");
                        spacersToDelete.add(liveObject.items[lastIndex]);
                        continue;
                    }
                    this.logDebug("Found a non-spacer item on row " + draggingFromRow +  
                                  ", no spacers will be deleted", "formItemDragDrop");
                    spacersToDelete = null;
                    break;
                }
            }
        }
        
        var delta = 0;
        
        if (side == "L" || side == "R") {
            
            var addColumns = true;
            // If the item is flagged startRow: true, we don't need to add columns
            if (dropItem.startRow) addColumns = false;
            // If the item is flagged endRow: true and we're not dropping in the rightmost
            // column, we don't need to add columns (NOTE: this isn't strictly true, we need 
            // to revisit this to cope with the case of an item with a larger colSpan than
            // the number of columns remaining to the right)
            if (dropItem.endRow && (side == "L" || colNum < rowTable[rowNum].length)) {
                addColumns = false;
            }
            // If we're repositioning an item and it came from this row in this form, we don't
            // need to add columns
            if (dragPositioning && draggingFromRow == rowNum) addColumns = false;
            
            // Need to add column(s) and move the existing items around accordingly
            if (addColumns) {
                var cols = dropItemCols;
            
                // If we're dropping onto a SpacerItem that we created in the first place, we only 
                // need to add columns if the colSpan of the dropped item is greater than the 
                // colSpan of the spacer (FIXME: and any adjacent spacers)
                var insertIndex = rowTable[rowNum][colNum];
                //if (side == "R") insertIndex++;
                if (rowTable[rowNum].contains(insertIndex)) {
                    var existingItem = liveObject.items[insertIndex];
                    
                    // If the item being dropped upon is not a spacer, check the item immediately 
                    // adjacent on the side of the drop
                    if (!isc.isA.SpacerItem(existingItem) || !existingItem._generatedByBuilder) {
                        insertIndex += side =="L" ? -1 : 1;
                        existingItem = liveObject.items[insertIndex];
                    }

                    if (rowTable[rowNum].contains(insertIndex)) {
                        
                        if (isc.isA.SpacerItem(existingItem) && existingItem._generatedByBuilder) {
                            if (existingItem.colSpan && existingItem.colSpan > cols) {
                                existingItem.editContext.setNodeProperties(existingItem.editNode, 
                                                {colSpan: existingItem.colSpan - cols});
                                cols = 0;
                            } else {
                                cols -= existingItem.colSpan;
                                existingItem.editContext.removeNode(existingItem.editNode);
                                if (side == "R") delta = -1;
                            }
                        }
                    }
                }

                if (cols <= 0) {
                    addColumns = false;
                    
                // If we get this far, we are going to insert "dropItemCols" columns to the form.
                // It may be that the form is already wide enough to accommodate those columns in 
                // this particular row (the grid has a ragged right edge because we use endRow and
                // startRow to control row breaking rather than unnecessary spacers)
                } else if (rowTable[rowNum].length + dropItemCols <= liveObject.numCols) {
                    addColumns = false;
                } else  {
                    // Otherwise widen the entire form
                    liveObject.editContext.setNodeProperties(liveObject.editNode, {numCols: liveObject.numCols + cols});
                }
            }
            
            // We're inserting a whole new column to the "grid" that the user sees.  This may not
            // be the desired action - maybe the user just wanted to insert an extra cell in this
            // row?  Leaving as is for now - prompting the user would make this and everything 
            // downstream of it asynchronous
            for (var i = 0; i < rowTable.length; i++) {
                var insertIndex = rowTable[i][colNum];
                if (insertIndex == null) insertIndex = liveObject.items.length;
                else insertIndex += delta + (side == "L" ? 0 : 1);
                if (i != rowNum) {
                    if (!addColumns) continue;
                    
                    // If we're dragging an item to a row higher up the form, we'll have stepped the
                    // delta forward when we inserted the dragged item; when we reach the row it 
                    // used to be on, we need to retard the delta by one to get the insert index 
                    // back in line
                    if (dragPositioning && draggingFromRow && 
                        rowNum < draggingFromRow && i == draggingFromRow) 
                    {
                        delta--;
                    }
                    
                    // If spacersToDelete contains anything, we detected up front that this drop-
                    // reposition will leave the from row empty of everything except spacer items 
                    // that we added in the first place.  Those spacers are marked for deletion at
                    // the end of this process; we certainly don't want to add any more!
                    if (spacersToDelete && spacersToDelete.length > 0 && i == draggingFromRow) {
                        continue;
                    }
                    // Look to see if the new column is to the right of an item with endRow: true, 
                    // because in that circumstance the spacer will break the layout
                    if (insertIndex > 0) {
                        var existingItem = liveObject.items[insertIndex - 1];
                        if (!existingItem || existingItem == dropItem || existingItem.endRow) {
                            continue;
                        }
                    }
                    // If the column just added is the rightmost one, we should retain form
                    // coherence by marking the right-hand item on each row as endRow: true instead
                    // of creating unnecessary spacers
                    var existingItemCols = this.getAdjustedColSpan(existingItem);
                    if (side == "R" && colNum + existingItemCols >= rowTable[i].length) {
                        if (!existingItem.endRow) {
                            existingItem.editContext.setNodeProperties(existingItem.editNode, 
                                        {endRow: true, _endRowSetByBuilder: true});
                        }
                        continue;
                    }
                    
                    var paletteNode = liveObject.editContext.makeEditNode({type: "SpacerItem"}); 
                    isc.addProperties(paletteNode.defaults, {
                        colSpan: cols, 
                        height: 0,
                        _generatedByBuilder: true
                    });
                    var nodeAdded = liveObject.editContext.addNode(paletteNode, liveObject.editNode,
                                                             insertIndex);
                    // Keep track of how many new items we've added to the form, because we need 
                    // to step the insert point on for any later adds
                    delta++;
                } else {
                    if (side == "L") {
                        // We're dropping to the left of an item, so we know there is an item to 
                        // our right.  If it specifies startRow, clear that out
                        var existingItem = liveObject.items[insertIndex];
                        if (existingItem && existingItem.startRow && existingItem._startRowSetByBuilder) {
                            existingItem.editContext.setNodeProperties(existingItem.editNode, 
                                {startRow: null, _startRowSetByBuilder: null});
                        }
                    } else {
                        // We're dropping to the right of an item, so we know there is an item to 
                        // our left.  If it specifies endRow, clear that out
                        var existingItem = liveObject.items[insertIndex - 1];
                        if (existingItem && existingItem.endRow && existingItem._endRowSetByBuilder) {
                            existingItem.editContext.setNodeProperties(existingItem.editNode, 
                                {endRow: null, _endRowSetByBuilder: null});
                        }
                    }
                    
                    this.itemDrop(liveObject.ns.EH.getDragTarget(), insertIndex, i, colNum, side, 
                        function (node) {
                            liveObject._nodeToSelect = node;
                        });
                    if (draggingFromRow == null || rowNum < draggingFromRow) delta++;
                }
            }
        } else {  // side was "T" or "B"
            var row, 
                currentItemIndex;
            // We don't want to drop "above" or "below" a spacer we put in place; we want to 
            // replace it
            if (isc.isA.SpacerItem(item) && item._generatedByBuilder) {
                row = rowNum;
            } else {
                row = rowNum + (side == "B" ? 1 : 0);
            }
            if (rowTable[row]) currentItemIndex = rowTable[row][colNum];
            
            var rowStartIndex;
            if (row >= rowTable.length) rowStartIndex = liveObject.items.length;
            else rowStartIndex = rowTable[row][0];
            
            var currentItem = currentItemIndex == null ? null : liveObject.items[currentItemIndex];
            if (currentItem == null || 
                    (isc.isA.SpacerItem(currentItem) && currentItem._generatedByBuilder)) {
                if (row > rowTable.length - 1 || row < 0) {
                    // Dropping past the end or before the beginning of the form - in both cases 
                    // rowStartIndex will already have been set correctly, so we can just go 
                    // ahead and add the component, plus any spacers we need
                    if (colNum != 0 && !dropItem.startRow) {
                        var paletteNode = liveObject.editContext.makeEditNode({type: "SpacerItem"});
                        isc.addProperties(paletteNode.defaults, {
                            colSpan: colNum, 
                            height: 0,
                            _generatedByBuilder : true
                        });
                        liveObject.editContext.addNode(paletteNode, liveObject.editNode, rowStartIndex);
                    }
                    this.itemDrop(liveObject.ns.EH.getDragTarget(), 
                                    rowStartIndex + (colNum != 0 ? 1 : 0), row, colNum, side, 
                                    function (node) {
                                        liveObject._nodeToSelect = node;
                                    });
                    // We have just created an empty line for this item, so we know for sure that
                    // it is the only item on the line (except for any spacers we created).  
                    // Therefore, we mark it endRow: true
                } else if (currentItem == null) {
                    // This can only happen if we're dropping on an existing row to the right of 
                    // a component that specifies endRow: true, or where the first item in the 
                    // next row specifies startRow: true.  If the reason is a trailing startRow, 
                    // that's fine and we don't need to do anything special.  If the reason is a 
                    // leading endRow, that presents a problem.  For now, we assume that the 
                    // endRow was set by VB, and just change it to suit ourselves.  This will 
                    // change so that we look to see whether the startRow/endRow attr was set by
                    // VB or the user.  If it was set by VB, we just can it as now; if it was set
                    // by the user we attempt to honor that by inserting a whole new row and 
                    // padding on the left, such that the item is dropped immediately above or 
                    // below the item hilited by the dropline, and the item that specified endRow
                    // remains as the last item in its row.
                    var leftCol = rowTable[row].length - 1;
                    if (leftCol < 0) {
                        isc.logWarn("Found completely empty row in DynamicForm at position (" + 
                                        row + "," + (colNum) + ")");
                        return;
                    }
                    var existingItemIndex = rowTable[row][leftCol];
                    var existingItem = liveObject.items[existingItemIndex];
                    if (existingItem == null) {
                        isc.logWarn("Null item in DynamicForm at position (" + row + "," + (colNum-1) + ")");
                        return;
                    }
                    // Special case - don't remove the endRow flag from the existing item if the 
                    // existing item is also the item we're dropping (as would be the case if the 
                    // if the user picks up a field and drops it further to the right in the 
                    // same column)
                    if (existingItem.endRow && existingItem != dropItem) {
                        existingItem.editContext.setNodeProperties(existingItem.editNode, {endRow: false});
                    }
                    var padding = (colNum - leftCol) - 1;
                    // Special case - the item to our left is actually the item we're dropping, 
                    // so we need to replace it with a spacer or the drop won't appear to have
                    // have had any effect
                    if (dragPositioning && existingItem == dropItem) {
                        padding += dropItemCols;
                    }
                    if (padding > 0) {
                        var paletteNode = liveObject.editContext.makeEditNode({type: "SpacerItem"});
                        isc.addProperties(paletteNode.defaults, {
                            colSpan: padding, 
                            height: 0,
                            _generatedByBuilder: true
                        });
                        liveObject.editContext.addNode(paletteNode, liveObject.editNode, existingItemIndex + 1);
                    }
                    this.itemDrop(liveObject.ns.EH.getDragTarget(), 
                                    existingItemIndex + (padding > 0 ? 2 : 1), row, colNum, side, 
                                    function (node) {
                                        liveObject._nodeToSelect = node;
                                    });
                } else {
                    // Where the user wants to drop there is currently a SpacerItem that we created
                    // to maintain form coherence.  So we do the following:
                    // - If the item being dropped is narrower than the spacer, we adjust the 
                    //   spacer's colSpan accordingly and drop the item in before it
                    // - If the item and the spacer are the same width, we remove the spacer and 
                    //   insert the item in its old position
                    // - If the item is wider than the spacer then for now we just replace the 
                    //   spacer with the item, like we would if they were the same width.  This 
                    //   may well cause the form to reflow in an ugly way.  To fix this, we will 
                    //   change this code to look for other spacers in the target row, and 
                    //   attempt to remove them to make space for the item; if all else fails, we
                    //   must add columns to the form and fix up as required to ensure that we 
                    //   don't get any reflows that break the form's coherence
                    
                    var oldColSpan = currentItem.colSpan ? currentItem.colSpan : 1,
                        newColSpan = dropItemCols;
                    if (oldColSpan > newColSpan) {
                        currentItem.editContext.setNodeProperties(currentItem.editNode, 
                                        {colSpan: oldColSpan - newColSpan});
                        this.itemDrop(liveObject.ns.EH.getDragTarget(), currentItemIndex, row, 
                                      colNum, side, 
                                      function (node) {
                                          liveObject._nodeToSelect = node;
                                      });
                    } else {
                        this.itemDrop(liveObject.ns.EH.getDragTarget(), currentItemIndex, row, 
                                      colNum, side, 
                                      function (node) {
                                          liveObject._nodeToSelect = node;
                                      });
                        currentItem.editContext.removeNode(currentItem.editNode);
                    }
                }
            } else {
                // Something is in the way.  We could either insert an entire new row or just push
                // the contents of this one column down a row.  Both of these seem like valid use
                // cases; for now, we're just going with inserting a whole new row
                if (colNum != 0) {
                    var paletteNode = liveObject.editContext.makeEditNode({type: "SpacerItem"}); 
                    isc.addProperties(paletteNode.defaults, {
                        colSpan: colNum, 
                        height: 0,
                        _generatedByBuilder : true
                    });
                    liveObject.editContext.addNode(paletteNode, liveObject.editNode, rowStartIndex);
                }
                this.itemDrop(liveObject.ns.EH.getDragTarget(), rowStartIndex + (colNum == 0 ? 0 : 1), 
                    row, colNum, side, function (node) {
                        if (node && node.liveObject && node.liveObject.editContext) {
                            node.liveObject.editContext.setNodeProperties(node, 
                                        {endRow: true, _endRowSetByBuilder: true});
                        }
                        liveObject._nodeToSelect = node;
                    });
            }
        }

        if (dragPositioning && spacersToDelete) {
            for (var i = 0; i < spacersToDelete.length; i++) {
                this.logDebug("Removing spacer item " + spacersToDelete[i].name, "formItemDragDrop");
                spacersToDelete[i].editContext.removeNode(spacersToDelete[i].editNode);
            }
        }
        
        if (!dragPositioning) dropItem.destroy();

        if (liveObject._nodeToSelect && liveObject._nodeToSelect.liveObject) {
            isc.EditContext.delayCall("selectCanvasOrFormItem", [liveObject._nodeToSelect.liveObject], 200);
        }
        
    },

    getAdjustedColSpan  : function(item) {
        if (!item) return 0;
        var cols = item.colSpan != null ? item.colSpan : 1;
        // colSpan of "*" makes no sense for the purposes of this calculation, which is trying to
        // work out how many columns an item we're dropping needs to take up.  So we'll call it 1.
        if (cols == "*") cols = 1;
        if (item.showTitle != false && (item.titleOrientation == "left" ||
                                        item.titleOrientation == "right" ||
                                        item.titleOrientation == null))
        {
            cols++
        }

        return cols;
    },

    // Override of EditProxy.canAdd - DynamicForm will accept a drop of a Canvas in addition to the
    // FormItems advertised in its schema
    canAdd : function (type) {
        if (!this.canDropAtLevel()) return false;
        if (this.creator.getObjectField(type) != null) return true;
        var classObject = isc.ClassFactory.getClass(type);
        if (classObject && classObject.isA("Canvas")) return true;
        return null;
    },

    // This undocumented method is called from itemDrop() just before the editNode is   
    // inserted into the editContext.  This function should return the editNode to actually
    // insert - either the passed node if no change is required, or some new value.  Note that 
    // the "isAdded" parameter will be false if the item was dropped after being dragged from 
    // elsewhere, as opposed to a drop of a new item from a component palette
    itemDropping : function (editNode, insertIndex, isAdded) {
        var liveObject = this.creator,
            item = editNode.liveObject,
            schemaInfo = isc.EditContext.getSchemaInfo(editNode);
        
        // Case 0: there is no schema information to compare, so nothing to do
        if (!schemaInfo.dataSource) return editNode;

        // Case 1: this is an unbound (so presumably empty) form.  Bind it to the top-level 
        // schema associated with this item
        if (!liveObject.dataSource) {
            liveObject.editProxy.setDataSource(schemaInfo.dataSource);
            liveObject.serviceNamespace = schemaInfo.serviceNamespace;
            liveObject.serviceName = schemaInfo.serviceName;
            return editNode;
        }
        
        // Case 2: this form is already bound to the top-level schema associated with this item,
        // so we don't need to do anything
        if (schemaInfo.dataSource == isc.DataSource.getDataSource(liveObject.dataSource).ID &&
            schemaInfo.serviceNamespace == liveObject.serviceNamespace && 
            schemaInfo.serviceName == liveObject.serviceName) {
            return editNode;
        }
        
        // Case 3: this form is already bound to some other schema.  We need to wrap this item
        // in its own sub-form
        var canvasItemNode = liveObject.editContext.makeEditNode({
            type: "CanvasItem",
            defaults: {
                cellStyle: "nestedFormContainer"
            }
        });
        isc.addProperties(canvasItemNode.defaults, {showTitle: false, colSpan: 2});
        canvasItemNode.dropped = true;
        liveObject.editContext.addNode(canvasItemNode, liveObject.editNode, insertIndex);
        
        var dfNode = liveObject.editContext.makeEditNode({
            type: "DynamicForm",
            defaults: {
                numCols: 2,
                canDropItems: false,
                dataSource: schemaInfo.dataSource,
                serviceNamespace: schemaInfo.serviceNamespace,
                serviceName: schemaInfo.serviceName,
                doNotUseDefaultBinding: true
            }
        });
        dfNode.dropped = true;
        liveObject.editContext.addNode(dfNode, canvasItemNode, 0);
        
        var nodeAdded = liveObject.editContext.addNode(editNode, dfNode, 0);
        isc.EditContext.clearSchemaProperties(nodeAdded);
    },

    makeFieldPaletteNode : function (field, dataSource) {
        var editorType = this.creator.getEditorType(field);
        editorType = editorType.substring(0,1).toUpperCase() + editorType.substring(1) + "Item";

        var paletteNode = {
            type: editorType,
            autoGen: true,
            defaults: {
                name: field.name
            }
        };

        // Only assign a title when explicitly provided and it is different
        // from the auto-generated version
        var autoTitle = dataSource.getAutoTitle(field.name);
        if (field.title != null && field.title != autoTitle) {
            paletteNode.defaults.title = field.title;
        }

        return paletteNode;
    },

    // Edit Mode extras for FormItem and its children
    // -------------------------------------------------------------------------------------------
    changed : function (form, item, value) {
        this.creator.editContext.setNodeProperties(this.creator.editNode, {defaultValue: value});
    },

    // FormItem selection
    // -------------------------------------------------------------------------------------------

    //> @attr formEditProxy.selectItemsMode (SelectItemsMode : "item" : IRW)
    // Controls which parts of a +link{FormItem,FormItem} respond to a click
    // and result in selecting the component.
    //
    // @visibility external
    //<
    selectItemsMode: "item",

    //> @type SelectItemsMode
    // Controls whether and when individual items are selected when clicking on a form in editMode.
    // @value "item" select an individual item if the item itself it clicked on, but not its title cell
    // @value "itemOrTitle" select an individual item if either the item or its title cell is clicked on.
    // NOTE: this mode is not the default because it can be make it difficult to select the form as a whole
    // @value "never" never allow selection of an individual item
    // @visibility external
    //<

    // Select FormItem on click
    click : function () {
        var item = this.getClickedFormItem(true);
        if (item && item.editProxy) {
            if (item.editProxy.click) {
                return item.editProxy.click(this.creator,item);
            }
        }
        this.Super("click", arguments);
        return isc.EH.STOP_BUBBLING;
    },

    // Edit title of FormItem on doubleClick
    doubleClick : function () {
        var item = this.getClickedFormItem(true);
        if (item && item.editProxy) {
            if (item.editProxy.doubleClick) {
                return item.editProxy.doubleClick(this.creator,item);
            }
        }
        this.Super("doubleClick", arguments);
        return isc.EH.STOP_BUBBLING;
    },

    getClickedFormItem : function (allModes) {
        var itemInfo =  this.creator._getEventTargetItemInfo(),
            item = (itemInfo ? itemInfo.item : null)
        ;
        // Target returned for containerItem-based FormItems is
        // the inner field. We need the outer field instead.
        if (item && item.parentItem) item = item.parentItem;

        return (item && (allModes || this.selectItemsMode == "itemOrTitle" || !itemInfo.overTitle) ? item : null);
    },
    
    // Component editor handling
    // ---------------------------------------------------------------------------------------

    // A DynamicForm doesn't really support inline editing but there is
    // a special case of a DynamicForm with a single FormItem. If the form
    // is masked the DynamicForm is placed into edit mode. We just defer that
    // edit start down to the FormItem.
    supportsInlineEdit: true,

    startInlineEditing : function (append) {
        var fields = this.creator.getFields();
        if (fields && fields.length == 1 && fields[0].editProxy && fields[0].editProxy.supportsInlineEdit) {
            fields[0].editProxy.startInlineEditing();
        }
    }
});

//> @class FormItemEditProxy
// +link{EditProxy} that handles +link{FormItem}s when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
isc.defineClass("FormItemEditProxy", "EditProxy").addMethods({

    getOverrideProperties : function () {
        var properties = this.Super("getOverrideProperties", arguments);
        properties = isc.addProperties({}, properties, {
            handleChanged: this.handleChanged,
            setOptionDataSource: this.setOptionDataSource
        });
        return properties;
    },

    handleChanged : function (value) {
        // Called in the context of the FormItem itself (this == FormItem)
        var editContext = this.editContext;
        // Save entered value to FormItem defaultValue
        editContext.setNodeProperties(this.editNode, { defaultValue: value });

        this.Super("handleChanged", arguments);
    },

    setOptionDataSource: function (datasource) {
        // Call actual implementation
        this.creator.Super("setOptionDataSource", arguments);

        var liveObject = this.creator,
            editNode = liveObject.editNode,
            name = (editNode.defaults ? editNode.defaults.name : null)
        ;
        if (name && name.startsWith(editNode.type)) {
            // - If name has never been changed by the user (still just an auto-generated name), it should
            //   be changed to the name of the PK field in the provided DataSource
            // - valueField should be auto-populated to the primaryKey, if there is one
            // - displayField should be set to the result of dataSource.getTitleField()
            var ds = isc.DS.get(datasource);
            if (ds) {
                var pk = ds.getPrimaryKeyFieldName();
                if (pk) {
                    // Push node update into another thread. Current thread is
                    // part of the DS node addition and needs to complete before
                    // updating the parent.
                    var editContext = liveObject.editContext,
                        tree = editContext.getEditNodeTree(),
                        siblings = tree.getChildren(tree.getParent(editNode)),
                        newName = pk,
                        duplicate = false,
                        counter = 1
                    ;
                    do {
                        duplicate = false;
                        for (var i = 0; i < siblings.length; i++) {
                            if (siblings[i].name == newName) {
                                duplicate = true;
                                break;
                            }
                        }
                        if (duplicate) {
                            newName = pk + counter++;
                        }
                    } while (duplicate);

                    isc.Timer.setTimeout(function () {
                        liveObject.editContext.setNodeProperties(editNode, {
                            name: newName,
                            valueField: pk,
                            displayField: ds.getTitleField()
                        });
                    }, 0);
                }
            }
        }
    },

    getResizeEdges : function () {
        // Allow a FormItem on a itemLayout:"absolute" form to be resized from the outline
        return (this.creator.form && this.creator.form._absPos() ? ["R","B"] : null);
    },

    click : function (form, item) {
        if (form.editProxy.selectItemsMode != "never") {
            isc.EditContext.selectCanvasOrFormItem(item, true);
        }
        return isc.EH.STOP_BUBBLING;
    },

    doubleClick : function (form, item) {
        if (this.wasFormItemClickOnTitle(form)) {
            this.editTitle();
            return isc.EH.STOP_BUBBLING;
        }
        this.Super("doubleClick", arguments);
        return isc.EH.STOP_BUBBLING;
    },

    wasFormItemClickOnTitle : function (form) {
        var itemInfo =  form._getEventTargetItemInfo(),
            item = (itemInfo ? itemInfo.item : null)
        ;
        // Target returned for containerItem-based FormItems is
        // the inner field. We need the outer field instead.
        if (item && item.parentItem) item = item.parentItem;
    
        return (item && itemInfo.overTitle);
    },

    // Title editor
    // ---------------------------------------------------------------------------------------

    editTitle : function (titleField) {
        var liveObject = this.creator,
            left,
            width,
            top,
            height;

        if (isc.isA.ButtonItem(liveObject)) {
            left = liveObject.canvas.getPageLeft();
            width = liveObject.canvas.getVisibleWidth();
            top = liveObject.canvas.getPageTop();
            height = liveObject.canvas.getHeight();
        } else {
            if (isc.isA.StaticTextItem(liveObject) && titleField == "defaultValue") {
                // Editing the value of a StaticTextItem so editor must
                // be placed over that part of the field
                left = liveObject.getPageLeft();
                width = liveObject.getVisibleWidth();
            } else {
                left = liveObject.getTitlePageLeft();
                width = liveObject.getVisibleTitleWidth();
            }
            var titleTop,
                titleHeight;

            titleTop = liveObject.getTitlePageTop();
            titleHeight = liveObject.getTitleVisibleHeight();
            height = liveObject.getVisibleHeight();

            // An example item without title height is a BlurbItem
            if (titleHeight == 0) {
                titleHeight = height;
                titleTop = top;
            }

            top = (titleHeight == height ? titleTop : titleTop + ((titleHeight - height) / 2));
        }

        isc.EditContext.manageTitleEditor(liveObject, left, width, top, height, null, titleField);
    },

    // Component editor handling
    // ---------------------------------------------------------------------------------------

    supportsInlineEdit: false,

    getInlineEditText : function () { },
    
    setInlineEditText : function (newValue) { },

    //> @attr formItemEditProxy.valueMapSeparatorChar (String : "," : IR)
    // If +link{inlineEditEvent,inline editing} for this FormItem edits the
    // +link{formItem.valueMap}, character that should be used as a separator between
    // values, or between pairs of stored vs display values if the user is entering such
    // a +link{ValueMap} using the +link{valueMapDisplaySeparatorChar,valueMapDisplaySeparatorChar}.
    // <p>
    // If +link{editProxy.inlineEditMultiline} is enabled, newlines will be used as value
    // separators and the <code>valueMapSeparatorChar</code>
    // <p>
    // The +link{valueMapEscapeChar,valueMapEscapeChar} can be used to enter the separator
    // char as part of a valueMap value.
    //
    // @visibility external
    //<
    valueMapSeparatorChar: ",",

    //> @attr formItemEditProxy.valueMapDisplaySeparatorChar (String : ":" : IR)
    // If +link{inlineEditEvent,inline editing} for this FormItem edits the
    // +link{formItem.valueMap}, character that should be used as a separator for
    // entering +link{ValueMap}s that map from a stored value to a user-displayed value.
    // <p>
    // With the default of ":", the following input:
    // <pre>
    //      1:Fixed, 2:Won't Fix, 3:Resolved
    // </pre>
    // Would be assumed to be a mapping like this (expressed in JSON):
    // <pre>
    //   {
    //      "1" : "Fixed",
    //      "2" : "Won't Fix",
    //      "3" : "Resolved"
    //   }
    // </pre>
    // <p>
    // If the input has inconsistent use of the separator char, the input will be assumed
    // to be stored-to-displayed mapping if the separator char is present in a majority
    // of values, and any values that lack a separator will use the same value for
    // storage and display.  For example, for this input:
    // <pre>
    //       Fixed:Reported Fixed, WontFix:Won't Fix, Resolved
    // </pre>
    // The resulting <code>valueMap</code> would be:
    // <pre>
    //   {
    //      "Fixed" : "Reported Fixed",
    //      "WontFix" : "Won't Fix",
    //      "Resolved" : "Resolved"
    //   }
    // </pre>
    // <p>
    // The +link{valueMapEscapeChar,valueMapEscapeChar} can be used to enter literal colon characters.
    // <p>
    // Set <code>valueMapDisplaySeparatorChar</code> to null to prevent entry of stored
    // vs displayed values - user input will always be treated as just a list of legal
    // values.
    //
    // @visibility external
    //<
    valueMapDisplaySeparatorChar: ":",

    //> @attr formItemEditProxy.valueMapSelectedChar (String : "*" : IR)
    // If +link{inlineEditEvent,inline editing} for this FormItem edits the
    // +link{formItem.valueMap}, character that can be used to mark the default selected
    // option.  Can appear before or after a value, for example, with this input:
    // <pre>
    //     Fixed,Won't Fix,Resolved*
    // </pre>
    // "Resolved" would be the default selected value.
    // <p>
    // If multiple values are marked selected for a SelectItem,
    // +link{SelectItem.multiple} will automatically be set.
    // <p>
    // The +link{valueMapEscapeChar,valueMapEscapeChar} can be used to allow the
    // <code>valueMapSelectedChar</code> to appear at the beginning or end of a  
    // valueMap value.
    //
    // @visibility external
    //<
    valueMapSelectedChar: "*",

    //> @attr formItemEditProxy.valueMapEscapeChar (String : "\" : IR)
    // If +link{inlineEditEvent,inline editing} for this FormItem edits the
    // +link{formItem.valueMap}, character that can be used to enter literal separator
    // chars (such as the +link{valueMapSeparatorChar,valueMapSeparatorChar}) or literal
    // leading or trailing whitespace.
    // <p>
    // Repeat this character twice to enter it literally.  For example, with the default
    // of "\", inputting "\\" would result in a literal backslash in the value.
    //
    // @visibility external
    //<
    valueMapEscapeChar: "\\"
});

//> @class TextItemEditProxy
// +link{EditProxy} that handles +link{TextItem,TextItems}, +link{StaticTextItem,StaticTextItems}
// and +link{BlurbItem,BlurbItems} when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
// Currently used by TextItem, StaticTextItem and BlurbItem
isc.defineClass("TextItemEditProxy", "FormItemEditProxy").addProperties({

    // Component editor handling
    // ---------------------------------------------------------------------------------------

    supportsInlineEdit: true,

    //> @method textItemEditProxy.getInlineEditText()
    // Returns the text based on the current component state to be edited inline.
    // Called by the +link{editProxy.inlineEditForm} to obtain the starting edit value.
    // <p>
    // Returns the component's <code>defaultValue</code>.
    //
    // @visibility external
    //<
    getInlineEditText : function () {
        return this.creator.defaultValue;
    },
    
    //> @method textItemEditProxy.setInlineEditText()
    // Save the new value into the component's state. Called by the
    // +link{editProxy.inlineEditForm} to commit the change.
    // <p>
    // Updates the component's <code>defaultValue</code>.
    //
    // @param newValue (String) the new component defaultValue
    //
    // @visibility external
    //<
    setInlineEditText : function (newValue) {
        this.creator.editContext.setNodeProperties(this.creator.editNode, { defaultValue: newValue });
    }
});

//> @class TextAreaItemEditProxy
// +link{EditProxy} that handles +link{TextAreaItem,TextAreaItems} when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
//Currently used by TextItem, StaticTextItem and BlurbItem
isc.defineClass("TextAreaItemEditProxy", "TextItemEditProxy").addProperties({

    // Component editor handling
    // ---------------------------------------------------------------------------------------

    inlineEditMultiline: true
});

//> @class SelectItemEditProxy
// +link{EditProxy} that handles +link{SelectItem,SelectItems}, +link{ComboBoxItem,ComboBoxItems}
// and +link{RadioGroupItem,RadioGroupItems} when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
// Currently used by SelectItem, ComboBoxItem and RadioGroupItem
isc.defineClass("SelectItemEditProxy", "FormItemEditProxy").addMethods({

    // Component editor handling
    // ---------------------------------------------------------------------------------------

    supportsInlineEdit: true,
    inlineEditMultiline: true,
    inlineEditInstructions: "Enter options, one per line. Use \"*\" to mark the selected option. " +
        "Use \"StoredValue:Display Value\" to create a mapping between stored values and " +
        "values displayed to the user.",

    //> @method selectItemEditProxy.getInlineEditText()
    // Returns the text based on the current component state to be edited inline.
    // Called by the +link{editProxy.inlineEditForm} to obtain the starting edit value.
    // <p>
    // Returns the component's valueMap one-per-line as specified in
    // +link{formItemEditProxy.valueMapDisplaySeparatorChar}. Current value(s)
    // is indicated with +link{formItemEditProxy.valueMapSelectedChar}.
    //
    // @visibility external
    //<
    getInlineEditText : function () {
        var liveObject = this.creator;

        if (!liveObject.valueMap) return null;

        var valueMap = liveObject.valueMap,
            selectedValues = liveObject.getValue() || [],
            separatorChar = (this.inlineEditMultiline ? "\n" : this.valueMapSeparatorChar),
            string = ""
        ;
        if (!isc.isAn.Array(selectedValues)) selectedValues = [selectedValues];

        if (isc.isAn.Array(valueMap)) {
            for (var i = 0; i < valueMap.length; i++) {
                var value = valueMap[i],
                    selected = selectedValues.contains(value)
                ;
                value = value.replace(separatorChar, "\\" + separatorChar);
                // Escape leading or trailing spaces
                value = value.replace(/^\s+/g, "\\ ").replace(/\s+$/g, "\\ ");
                string = string + (string.length > 0 ? separatorChar : "") + 
                    value +
                    (selected ? this.valueMapSelectedChar : ""); 
            }
        } else {
            for (var key in valueMap) {
                var value = valueMap[key],
                    selected = selectedValues.contains(key)
                ;
                key = key.replace(separatorChar, "\\" + separatorChar);
                value = value.replace(separatorChar, "\\" + separatorChar);
                // Escape leading or trailing spaces
                key = key.replace(/^\s+/g, "\\ ").replace(/\s+$/g, "\\ ");
                value = value.replace(/^\s+/g, "\\ ").replace(/\s+$/g, "\\ ");
                string = string + (string.length > 0 ? separatorChar : "") +
                    key + this.valueMapDisplaySeparatorChar + value +
                    (selected ? this.valueMapSelectedChar : "");
            }
        }
        return string;
    },
    
    //> @method selectItemEditProxy.setInlineEditText()
    // Save the new value into the component's state. Called by the
    // +link{editProxy.inlineEditForm} to commit the change.
    // <p>
    // Updates the component's valueMap and current value.
    //
    // @param newValue (String) the new component valueMap
    //
    // @visibility external
    //<
    setInlineEditText : function (newValue) {
        var separatorChar = (this.inlineEditMultiline ? "\n" : this.valueMapSeparatorChar);
        var value = isc.EditProxy.parseStringValueMap(newValue,
                separatorChar,
                this.valueMapEscapeChar,
                this.valueMapDisplaySeparatorChar,
                this.valueMapSelectedChar,
                true);
        this.creator.editContext.setNodeProperties(this.creator.editNode, { valueMap: value.valueMap, value: value.value });
    },

    // In VB only, if Select/Combo has a real, non-Mock, DataSource show edit form with
    // edit buttons instead of text inline edit form.
    createInlineEditForm : function () {
        if (this.creator.editContext.isVisualBuilder &&
            this.creator.optionDataSource &&
            !isc.isA.MockDataSource(this.creator.optionDataSource))
        {
            var dataSource = isc.DS.get(this.creator.optionDataSource),
                // FormItem->EditContext->EditTree->VB Layout
                vb = this.creator.editContext.creator.creator
            ;

            var fields = [{
                name: "editDataSourceButton",
                type: "button",
                title: "View / Edit DataSource",
                endRow: false,
                click : function (form, item) {
                    if (vb.showDSEditor) vb.showDSEditor(dataSource);
                    form.dismissEditor();
                }
// TODO: Waiting on DataImportDialog to be integrated into VB
//            },{
//                name: "importDataButton",
//                type: "button",
//                title: "Import Data",
//                startRow: false,
//                click : function (form, item) {
//                    form.dismissEditor();
//                }
            }];

            var form = this.createAutoChild("inlineEditForm", {
                margin: 0, padding: 10, cellPadding: 0,
                fields: fields,
                numCols: 2,
                click : function () {
                    this.dismissEditor();
                },
                dismissEditor : function () {
                    // Automatic blur event will save value if needed
                    this.hide();
                }
            });

            return form;
        } else {
            return this.Super("createInlineEditForm", arguments);
        }
    }
});

//> @class CheckboxItemEditProxy
// +link{EditProxy} that handles +link{CheckboxItem} when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
isc.defineClass("CheckboxItemEditProxy", "FormItemEditProxy").addMethods({

    // Component editor handling
    // ---------------------------------------------------------------------------------------

    supportsInlineEdit: true,

    //> @method checkboxItemEditProxy.getInlineEditText()
    // Returns the text based on the current component state to be edited inline.
    // Called by the +link{editProxy.inlineEditForm} to obtain the starting edit value.
    // <p>
    // Returns the component's value as [ ] or [x].
    //
    // @visibility external
    //<
    getInlineEditText : function () {
        var liveObject = this.creator;

        return (liveObject.value ? "[x]" : "[ ]");
    },

    //> @method checkboxItemEditProxy.setInlineEditText()
    // Save the new value into the component's state. Called by the
    // +link{editProxy.inlineEditForm} to commit the change.
    // <p>
    // Updates the component's value.
    //
    // @param newValue (String) the new component value
    //
    // @visibility external
    //<
    setInlineEditText : function (newValue) {
        newValue = newValue.replace(/\s+/g, "").toLowerCase();

        var checked = ("[x]" == newValue || 
                "true" == newValue || "t" == newValue || 
                "yes" == newValue || "y" == newValue || 
                "checked" == newValue);

        this.creator.editContext.setNodeProperties(this.creator.editNode, { value: checked });
    }
});

//> @class DateItemEditProxy
// +link{EditProxy} that handles +link{DateItem} when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
isc.defineClass("DateItemEditProxy", "FormItemEditProxy").addMethods({

    // Component editor handling
    // ---------------------------------------------------------------------------------------

    supportsInlineEdit: true,

    //> @method dateItemEditProxy.getInlineEditText()
    // Returns the text based on the current component state to be edited inline.
    // Called by the +link{editProxy.inlineEditForm} to obtain the starting edit value.
    // <p>
    // Returns the component's value using +link{Date.toShortDate}.
    //
    // @visibility external
    //<
    getInlineEditText : function () {
        var liveObject = this.creator,
            value = liveObject.defaultValue
        ;

        if (!value) return null;
        if (isc.isA.Date(value)) {
            value = value.toShortDate();
        }
        return value;
    },

    //> @method dateItemEditProxy.setInlineEditText()
    // Save the new value into the component's state. Called by the
    // +link{editProxy.inlineEditForm} to commit the change.
    // <p>
    // Updates the component's <code>defaultValue</code>.
    //
    // @param newValue (String) the new component default value
    //
    // @visibility external
    //<
    setInlineEditText : function (newValue) {
        // If date can be parsed, store the real date value
        var date = Date.parseInput(newValue);
        if (date) {
            newValue = date;
        }

        this.creator.editContext.setNodeProperties(this.creator.editNode, { defaultValue: newValue });
    }
});
