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



// Resize thumbs
// --------------------------------------------------------------------------------------------

isc.Canvas.addClassProperties({
    resizeThumbConstructor:isc.Canvas,
    resizeThumbDefaults:{
        width:8, 
        height:8, 
        overflow:"hidden", 
        styleName:"resizeThumb",
        canDrag:true,
        canDragResize:true,
        // resizeEdge should be the edge of the target, not the thumb
        getEventEdge : function () { return this.edge; },
        autoDraw:false
    },
    minimumDropMargin: 2,
    minimumDropTargetSize: 10
});

isc.Canvas.addClassMethods({
    // NOTE: Canvas thumbs vs one-piece mask?
    // - since we reuse the same set of thumbs, there's no real performance issue
    // - one-piece mask implementations: 
    //   - if an image with transparent regions, thumbs would scale 
    //   - if a table
    //     - event handling iffy - transparent table areas may or may not intercept
    //     - would have to redraw on resize
    //   - transparent Canvas with absolutely-positioned DIVs as content
    //     - event handling might be iffy
    // - would have bug: when thumbs are showing, should be able to click between them to hit
    //   something behind the currently selected target
    // - when thumbs are not showing, mask still needs to be there, but would need to shrink and not
    //   show thumbs
    _makeResizeThumbs : function () {
        var edgeCursors = isc.Canvas.getInstanceProperty("edgeCursorMap"),
            thumbs = {},
            thumbClass = isc.ClassFactory.getClass(this.resizeThumbConstructor);
        for (var thumbPosition in edgeCursors) {
           // NOTE: can't use standard autoChild creation because we are in static scope -
           // thumbs are globally shared
           thumbs[thumbPosition] = thumbClass.create({
                ID:"isc_resizeThumb_" + thumbPosition,
                edge:thumbPosition
           }, this.resizeThumbDefaults, this.resizeThumbProperties)
        }
        isc.Canvas._resizeThumbs = thumbs;
    },

    showResizeThumbs : function (target) {
        if (!target) return;
        
        if (!isc.Canvas._resizeThumbs) isc.Canvas._makeResizeThumbs();

        var thumbSize = isc.Canvas.resizeThumbDefaults.width,
            thumbs = isc.Canvas._resizeThumbs;
     
        // place the thumbs along the outside of the target
        var rect = target.getPageRect(),
            left = rect[0],
            top = rect[1],
            width = rect[2],
            height = rect[3],
    
            midWidth = Math.floor(left + (width/2) - (thumbSize/2)),
            midHeight = Math.floor(top + (height/2) - (thumbSize/2));

        thumbs.T.moveTo(midWidth, top - thumbSize);
        thumbs.B.moveTo(midWidth, top + height);
        thumbs.L.moveTo(left - thumbSize, midHeight);
        thumbs.R.moveTo(left + width, midHeight);

        thumbs.TL.moveTo(left - thumbSize, top - thumbSize);
        thumbs.TR.moveTo(left + width, top - thumbSize);
        thumbs.BL.moveTo(left - thumbSize, top + height);
        thumbs.BR.moveTo(left + width, top + height);
        
        for (var thumbName in thumbs) {
            var thumb = thumbs[thumbName];
            // set all the thumbs to drag resize the canvas we're masking
            thumb.dragTarget = target;
            // show all the thumbs    
            thumb.bringToFront();        
            thumb.show();
        }

        this._thumbTarget = target;
    },
    
    hideResizeThumbs : function () {
        var thumbs = this._resizeThumbs;
        for (var thumbName in thumbs) {
            thumbs[thumbName].hide();
        }
        this._thumbTarget = null;
    }

});

// Edit Mask
// --------------------------------------------------------------------------------------------

// At the Canvas level the Edit Mask provides moving, resizing, and standard context menu items.
// The editMask should be extended on a per-widget basis to add things like drop behaviors or
// additional context menu items.  Any such extensions should be delineated with 
//>EditMode 
//<EditMode
// .. markers so it can be eliminated from normal builds.

isc.Canvas.addProperties({
    editMaskDefaults:{

        // Thumb handling
        // ---------------------------------------------------------------------------------------
        draw : function () {
            this.Super("draw", arguments);

            // stay above the master
            this.observe(this.masterElement, "setZIndex", "observer.moveAbove(observed)");
            // show thumbs on the master as soon as we're draw()n
            isc.Canvas.showResizeThumbs(this);

            // match the master's prompt (native tooltip).  Only actually necessary in Moz since IE
            // considers the eventMask transparent with respect to determining the prompt.
            this.observe(this.masterElement, "setPrompt", "observer.setPrompt(observed.prompt)");

            return this;
        },
        parentVisibilityChanged : function () {
            this.Super("parentVisibilityChanged", arguments);
            if (isc.Canvas._thumbTarget == this) isc.Canvas.hideResizeThumbs();
        },

        // show thumbs when clicked on.  NOTE: since there's only one set of thumbs, this implicitly
        // accomplishes the goal of having only one selected widget
        click : function () {
            isc.Canvas.showResizeThumbs(this);
            return isc.EH.STOP_BUBBLING;
        },

        // Event Bubbling
        // ---------------------------------------------------------------------------------------

        // XXX FIXME: this is here to maintain z-order on dragReposition.  EH.handleDragStop()
        // brings the mask to the front when we stop dragging - which is not what we want, so we
        // suppress it here.
        bringToFront : function () { },
    
        // prevent bubbling to the editor otherwise we'll start a selection while trying to
        // select/move a component
        mouseDown : function () {
            this.Super("mouseDown", arguments);
            return isc.EH.STOP_BUBBLING;
        },

        mouseUp : function () {
            this.Super("mouseUp", arguments);
            return isc.EH.STOP_BUBBLING;
        },

        doubleClick : function () {
            this._maskTarget.bringToFront();
            return this.click();
        },

        // Drag and drop move and resize
        // ---------------------------------------------------------------------------------------
        // D&D: some awkwardness
        // - if we set dragTarget to the masterElement, it will get the setDragTracker(), 
        //   dragRepositionMove() etc events, which it may have overridden, whereas we want just a
        //   basic reposition or resize, so we need to be the dragTarget
        // - to be in the right parental context, and to automatically respond to programmatic
        //   manipulation of the parent's size and position, we want to be a peer, but at the end of
        //   drag interactions we also need to move/resize the master, which would normally cause
        //   the master to move us, so we need to switch off automatic peer behaviors while we move
        //   the master

        // allow the mask to be moved around (only the thumbs allow resize)
        canDrag:true,
        canDragReposition:true,
    
        // don't allow setDragTracker to bubble in case some parent tries to set it innapropriately
        setDragTracker: function () { return isc.EH.STOP_BUBBLING },

        // when we're moved or resized, move/resize the master and update thumb positions
        moved : function () {
            this.Super("moved", arguments);

            var masked = this.masterElement;
            if (masked) {
                // calculate the amount the editMask was moved
                var deltaX = this.getOffsetLeft() - masked.getLeft();
                var deltaY = this.getOffsetTop() - masked.getTop();

                // relocate our master component (avoiding double notifications)
                this._moveWithMaster = false;
                masked.moveTo(this.getOffsetLeft(), this.getOffsetTop());
                this._moveWithMaster = true;
            }

            if (isc.Canvas._thumbTarget == this) isc.Canvas.showResizeThumbs(this);
        },

        resized : function () {
            this.Super("resized", arguments);

            // Recalculate dropMargin based on new visible size
            this.dropMargin = this.getEditDropMargin(this.defaultDropMargin);

            // don't loop if we resize master, master overflows, and we resize to overflow'd size
            if (this._resizingMaster) return;
            this._resizingMaster = true;

            var master = this.masterElement;
            if (master) {

                // resize the widget we're masking (avoiding double notifications)
                this._resizeWithMaster = false;
                master.resizeTo(this.getWidth(), this.getHeight());
                this._resizeWithMaster = true;

                // the widget we're masking may overflow, so redraw if necessary to get new size so,
                // and match its overflow'd size
                master.redrawIfDirty();
                this.resizeTo(master.getVisibleWidth(), master.getVisibleHeight());
            }

            // update thumb positions
            isc.Canvas.showResizeThumbs(this);

            this._resizingMaster = false;
        },

        // Editing Context Menus
        // ---------------------------------------------------------------------------------------
        // standard context menu items plus the ability to add "editMenuItems" on the master
        showContextMenu : function () {
        
            if (!this.editContext) return;
        
            var edited = this.masterElement,
                menuItems;

            if (this.editContext.selectedComponents.length > 0) { // multi-select
                menuItems = 
                        (edited.editMultiMenuItems || []).concat(this.multiSelectionMenuItems);
            } else {
                menuItems = (edited.editMenuItems || []).concat(this.standardMenuItems);
            }

            if (!this.contextMenu) this.contextMenu = this.getMenuConstructor().create({});
            this.contextMenu.setData(menuItems);

            // NOTE: show the menu on our masterElement (the widget we're masking) so that "target"
            // in click methods will be the masterElement and not the mask.
            this.contextMenu.showContextMenu(edited);
            return false;
        },
        standardMenuItems:[
            {title:"Remove", click:"target.destroy()"},
            {title:"Bring to Front", click:"target.bringToFront()"},
            {title:"Send to Back", click:"target.sendToBack()"}
        ],
        multiSelectionMenuItems: [{
            title: "Remove Selected Items",
            click : function (target) {
                if (target != null && isc.isAn.EditPane(target.editContext)) {
                    target.editContext.removeSelection(target);
                }
            }
        }]
    },

    // Enabling EditMode
    // ---------------------------------------------------------------------------------------

    // A hook which subclasses can use if they need to know when they have been added to an editContext
    addedToEditContext : function (editContext, editNode, parentNode, index) {

    },

    //> @method Canvas.updateEditNode()
    // A callback invoked for each +link{EditNode.liveObject,liveObject} by
    // +link{EditContext} when the EditContext is being serialized. The liveObject
    // may make any updates needed to the +link{EditNode} (or the +link{EditContext} as
    // a whole) in order to be able to later recreate the objects.
    // @param editContext (EditContext) the EditContext
    // @param editNode (EditNode) the EditNode
    // @see EditContext.serializeAllEditNodes()
    // @see EditContext.serializeEditNodes()
    // @visibility external
    //<
    updateEditNode : function (editContext, editNode) {

    },

    // A hook called from EditContext.addNode(), allowing the liveParent to wrap a newNode in
    // some additional structure. Return the parentNode that the newNode should be added to.
    // By default, just returns the parentNode supplied.
    
    wrapChildNode : function (editContext, newNode, parentNode, index) {
        // Add an event mask if so configured
        if (newNode.useEditMask == null && this.autoMaskComponents) {
            newNode.useEditMask = true;
        }
        return parentNode;
    },

    useEditMask:true,
    setEditMode : function (editingOn, editContext, editNode) {
        if (editingOn == null) editingOn = true;
        if (this.editingOn == editingOn) return;
        this.editingOn = editingOn;

        if (this.editingOn) {
            this.editContext = editContext;
        } else {
            this.hideEditMask();
        }
        
        this.editNode = editNode;

        // If we're going into edit mode, re-route various methods
        if (this.editingOn) {
            this.saveToOriginalValues(["click", "doubleClick", "willAcceptDrop",
                                       "clearNoDropIndicator", "setNoDropCursor", "canAcceptDrop",  
                                       "canDropComponents", "drop", "dropMove", "dropOver",
                                       "setDataSource"]);
            this.setProperties({
                click: this.editModeClick,
                doubleClick: this.editModeDoubleClick,
                willAcceptDrop: this.editModeWillAcceptDrop,
                clearNoDropIndicator: this.editModeClearNoDropIndicator,
                setNoDropIndicator: this.editModeSetNoDropIndicator,
                canAcceptDrop: true,
                canDropComponents: true,
                
                drop: this.editModeDrop,
                dropMove: this.editModeDropMove,
                dropOver: this.editModeDropOver,
                baseSetDataSource: this.setDataSource,
                setDataSource: this.editModeSetDataSource
            });
            // Calculate dropMargin based on visible size
            this.dropMargin = this.getEditDropMargin(this.defaultDropMargin);
        } else {
            this.restoreFromOriginalValues(["click", "doubleClick", "willAcceptDrop",
                                            "clearNoDropIndicator", "setNoDropCursor", "canAcceptDrop",  
                                            "canDropComponents", "drop", "dropMove", "dropOver",
                                            "setDataSource"]);
        }
        
        // In case anything visual has changed, or the widget has different drag-and-drop
        // behavior in edit mode (register/unregisterDroppableItem is called from redraw)
        this.markForRedraw();
    },
    

    showEditMask : function () {

        var svgID = this.getID() + ":<br>" + this.src;

        // create an edit mask if we've never created one
        if (!this._editMask) {

            // special SVG handling
            // FIXME: move all SVG-specific handling to SVG.js
            var svgProps = { };
            if (isc.SVG && isc.isA.SVG(this) && isc.Browser.isIE) {
                isc.addProperties(svgProps, {
                    backgroundColor : "gray",
                    mouseOut : function () { this._maskTarget.Super("_hideDragMask"); },
                    contents : isc.Canvas.spacerHTML(10000,10000, svgID)
                });
            }
    
            var props = isc.addProperties({}, this.editMaskDefaults, this.editMaskProperties, 
                                          // assume the editContext is the parent if none is
                                          // provided
                                          {editContext:this.editContext || this.parentElement, 
                                           keepInParentRect: this.keepInParentRect},
                                          svgProps);
            this._editMask = isc.EH.makeEventMask(this, props);
        }
        this._editMask.show();

        // SVG-specific
        if (isc.SVG && isc.isA.SVG(this)) {
            if (isc.Browser.isIE) this.showNativeMask();
            else {
                this.setBackgroundColor("gray");
                this.setContents(svgID);
            }
        }
    },
    hideEditMask : function () {
        if (this._editMask) this._editMask.hide();
    },
    
    editModeClick : function () {
        if (this.editNode) {
            isc.EditContext.selectCanvasOrFormItem(this, true);
            return isc.EH.STOP_BUBBLING;
        }
    },
    
    editModeDoubleClick : function () {
        // No default impl
    },
    
    // XXX - Need to do something about Menus in the drop hierarchy - they aren't Class-based
    
    editModeWillAcceptDrop : function (changeObjectSelection) {
        this.logInfo("editModeWillAcceptDrop for " + this.ID, "editModeDragTarget");
	    var dragData = this.ns.EH.dragTarget.getDragData(),
            dragType,
            draggingFromPalette = true;

        // If dragData is null, this is probably because we are drag-repositioning a component
        // in a layout - the dragData is the component itself
        if (dragData == null || (isc.isAn.Array(dragData)) && dragData.length == 0) {
            draggingFromPalette = false;
            this.logInfo("dragData is null - using the dragTarget itself", "editModeDragTarget");
            dragData = this.ns.EH.dragTarget;
            if (isc.isA.FormItemProxyCanvas(dragData)) {
                this.logInfo("The dragTarget is a FormItemProxyCanvas for " + dragData.formItem,
                                "editModeDragTarget");
                dragData = dragData.formItem;
            }
            dragType = dragData._constructor || dragData.Class;
        } else {
            if (isc.isAn.Array(dragData)) dragData = dragData[0];
            dragType = dragData.type || dragData.className;
        }
        this.logInfo("Using dragType " + dragType, "editModeDragTarget");

        if (!this.canAdd(dragType)) {
            this.logInfo(this.ID + " does not accept drop of type " + dragType, "editModeDragTarget");
            // Can't drop on this widget, so check its ancestors
            var ancestor = this.parentElement;
            while (ancestor && !ancestor.editorRoot) {
                if (ancestor.editingOn) {
                    var ancestorAcceptsDrop = ancestor.editModeWillAcceptDrop();
                    if (!ancestorAcceptsDrop) {
                        this.logInfo("No ancestor accepts drop", "editModeDragTarget");
                        if (changeObjectSelection != false) {
                            if (draggingFromPalette) isc.EditContext.hideDragHandle();
                            isc.SelectionOutline.hideOutline();
                            this.setNoDropIndicator();
                        }
                        return false;
                    }
                    this.logInfo("An ancestor accepts drop", "editModeDragTarget");
                    return true;
                }
                // Note that the effect of the return statements in the
                // condition above is that we'll stop walking
                // the ancestor tree at the first parent where editingOn is true ...
                // at that point, we'll re-enter editModeWillAcceptDrop
                ancestor = ancestor.parentElement;
            }

            // Given the return statements in the while condition above, we'll only get
            // here if no ancestor had editingOn: true
            this.logInfo(this.ID + " has no parentElement in editMode", "editModeDragTarget");
            if (changeObjectSelection != false) {
                if (draggingFromPalette) isc.EditContext.hideDragHandle();
                isc.SelectionOutline.hideOutline();
                this.setNoDropIndicator();
            }
            return false;
        }
        
        // This canvas can accept the drop, so select its top-level parent (in case it's a 
        // sub-component like a TabSet's PaneContainer)
        this.logInfo(this.ID + " is accepting the " + dragType + " drop", "editModeDragTarget");
        var hiliteCanvas = this.findEditNode(dragType);
        if (hiliteCanvas) {
            if (changeObjectSelection != false) {
                this.logInfo(this.ID + ": selecting editNode object " + hiliteCanvas.ID);
                if (draggingFromPalette) isc.EditContext.hideDragHandle();
                isc.SelectionOutline.select(hiliteCanvas, false);
                hiliteCanvas.clearNoDropIndicator();
            }
            return true;
        } else {
            this.logInfo("findEditNode() returned null for " + this.ID, "editModeDragTarget");
        }
        
        
        if (changeObjectSelection != false) {
            this.logInfo("In editModeWillAcceptDrop, '" + this.ID + "' was willing to accept a '" + 
                     dragType + "' drop but we could not find an ancestor with an editNode");
        }
    }, 
    
    // Override to provide special editNode canvas selection (note that this impl does not 
    // care about dragType, but some special implementations - eg, TabSet - return different
    // objects depending on what is being dragged)
    findEditNode : function (dragType) {
        if (!this.editNode) {
            this.logInfo("Skipping '" + this + "' - has no editNode", "editModeDragTarget");
            if (this.parentElement && this.parentElement.findEditNode) {
                return this.parentElement.findEditNode(dragType);
            } else {
                return null;
            }
        }
        return this;
    },

    // Tests whether this Canvas can accept a child of type "type".  If it can't, and "type"
    // names some kind of FormItem, then we'll accept it if this Canvas is willing to accept
    // a child of type "DynamicForm" -- we'll cope with this downstream by auto-wrapping the dropped
    // FormItem inside a DynamicForm that we create for that very purpose.  Similarly, if
    // the type represents some type of DrawItem then we'll accept the child if this Canvas
    // can a DrawPane.
    canAdd : function (type) {
        if (this.getObjectField(type) == null) {
            var clazz = isc.ClassFactory.getClass(type);
            if (clazz) {
                if (clazz.isA("FormItem")) {
                    return (this.getObjectField("DynamicForm") != null);
                } else if (clazz.isA("DrawItem")) {
                    return (this.getObjectField("DrawPane") != null);
                }
            }
            return false;
        } else {
            return true;
        }
    },

    // Canvas.clearNoDropindicator no-ops if the internal _noDropIndicator flag is null.  This
    // isn't good enough in edit mode because a canvas can be dragged over whilst the no-drop
    // cursor is showing, and we want to revert to a droppable cursor regardless of whether 
    // _noDropIndicatorSet has been set on this particular canvas. 
    editModeClearNoDropIndicator : function (type) {
        if (this._noDropIndicatorSet) delete this._noDropIndicatorSet;
        this._updateCursor();
        
        // XXX May need to add support for no-drop drag tracker here if we ever implement 
        // such a thing in Visual Builder
    },

    // Special editMode version of setNoDropCursor - again, because the base version no-ops in 
    // circumstances where we need it to refresh the cursor.
    editModeSetNoDropIndicator : function () {
        this._noDropIndicatorSet = true;
        this._applyCursor(this.noDropCursor);
    },

    

    defaultDropMargin: 10,
    dropMargin: 10,
    getEditDropMargin : function (dropMargin) {
        // Fix up the dropMargin to prevent not-very-tall canvas from passing *every* drop 
        // through to parent layouts
        var newDropMargin = dropMargin;
        if (dropMargin * 2 > this.getVisibleHeight() - isc.Canvas.minimumDropTargetSize) {
            newDropMargin = Math.round((this.getVisibleHeight() - isc.Canvas.minimumDropTargetSize) / 2);
            if (newDropMargin < isc.Canvas.minimumDropMargin) newDropMargin = isc.Canvas.minimumDropMargin; 
        }
        return newDropMargin;
    },

    shouldPassDropThrough : function () {

        var source = isc.EH.dragTarget,
            paletteNode,
            dropType;

        if (!source.isA("Palette")) {
            dropType = source.isA("FormItemProxyCanvas") ? source.formItem.Class
                                                         : source.Class;
        } else {
            paletteNode = source.getDragData();
            if (isc.isAn.Array(paletteNode)) paletteNode = paletteNode[0];
            dropType = paletteNode.type || paletteNode.className;
        }
        
        this.logInfo("Dropping a " + dropType, "formItemDragDrop");
        
        if (!this.canAdd(dropType)) {
            this.logInfo("This canvas cannot accept a drop of a " + dropType, "formItemDragDrop");
            return true;
        }
        
        if (this.parentElement && !this.parentElement.editModeWillAcceptDrop(false)) {
            this.logInfo(this.ID + " is not passing drop through - no ancestor is willing to " + 
                        "accept the drop", "editModeDragTarget");
            return false;
        }
        
        var x = isc.EH.getX(),
            y = isc.EH.getY(),
            work = this.getPageRect(),
            rect = {
                left: work[0], 
                top: work[1], 
                right: work[0] + work[2], 
                bottom:work[1] + work[3]
            }
            
        if (!this.orientation || this.orientation == "vertical") {
            if (x < rect.left + this.dropMargin  || x > rect.right - this.dropMargin) {
                this.logInfo("Close to right or left edge - passing drop through to parent for " +
                            this.ID, "editModeDragTarget");
                return true;
            }
        }
        if (!this.orientation || this.orientation == "horizontal") {
            if (y < rect.top + this.dropMargin  || y > rect.bottom - this.dropMargin) {
                this.logInfo("Close to top or bottom edge - passing drop through to parent for " + 
                            this.ID, "editModeDragTarget");
                return true;
            }
        }

        this.logInfo(this.ID + " is not passing drop through", "editModeDragTarget");
        return false;
    },
    
    
    editModeDrop : function () {
        if (this.shouldPassDropThrough()) {
            return;
        }
    
        var source = isc.EH.dragTarget,
            paletteNode,
            dropType;
    
        if (!source.isA("Palette")) {
            if (source.isA("FormItemProxyCanvas")) {
                source = source.formItem;
            }
            dropType = source._constructor || source.Class;
        } else {
            paletteNode = source.transferDragData();
            if (isc.isAn.Array(paletteNode)) paletteNode = paletteNode[0];
            paletteNode.dropped = true;
            dropType = paletteNode.type || paletteNode.className;
        }
        
        // if the source isn't a Palette, we're drag/dropping an existing component, so remove the 
        // existing component and re-create it in its new position
        if (!source.isA("Palette")) {
            if (isc.EditContext._dragHandle) isc.EditContext._dragHandle.hide();
            if (source == this) return;  // Can't drop a component onto itself
            var tree = this.editContext.getEditNodeTree(),
                oldParent = tree.getParent(source.editNode);
            this.editContext.removeNode(source.editNode);
            var node;
            if (source.isA("FormItem")) {
                if (source.isA("CanvasItem")) {
                    node = this.editContext.addNode(source.canvas.editNode, this.editNode);
                } else {
                    node = this.editContext.addWithWrapper(source.editNode, this.editNode);
                }
            } else if (source.isA("DrawItem")) {
                node = this.editContext.addWithWrapper(source.editNode, this.editNode, true);
            } else {
                node = this.editContext.addNode(source.editNode, this.editNode);
            }
            if (node && node.liveObject) {
                isc.EditContext.selectCanvasOrFormItem(node.liveObject, true);
            }
        } else {
            // loadData() operates asynchronously, so we'll have to finish the item drop off-thread
            if (paletteNode.loadData && !paletteNode.isLoaded) {
                var thisCanvas = this;
                paletteNode.loadData(paletteNode, function (loadedNode) {
                    loadedNode = loadedNode || paletteNode;
                    loadedNode.isLoaded = true;
                    thisCanvas.completeItemDrop(loadedNode)
                    loadedNode.dropped = paletteNode.dropped;
                });
                return isc.EH.STOP_BUBBLING;
            }

            this.completeItemDrop(paletteNode);
            return isc.EH.STOP_BUBBLING;
        }
    },

    completeItemDrop : function (paletteNode) {
        
        if (!this.editContext) return;
        
        var nodeType = paletteNode.type || paletteNode.className;
        var clazz = isc.ClassFactory.getClass(nodeType);
        if (clazz && clazz.isA("FormItem")) {
            this.editContext.addWithWrapper(paletteNode, this.editNode);
        } else if (clazz && clazz.isA("DrawItem")) {
            this.editContext.addWithWrapper(paletteNode, this.editNode, true);
        } else {
            this.editContext.addNode(paletteNode, this.editNode);
        }
    },
    
    editModeDropMove : function () {
        if (!this.editModeWillAcceptDrop()) return false;
        if (!this.shouldPassDropThrough()) {
            this.Super("dropMove", arguments);
            if (this.parentElement && this.parentElement.hideDropLine) {
                this.parentElement.hideDropLine();
                if (this.parentElement.isA("FormItem")) {
                    this.parentElement.form.hideDragLine();
                } else if (this.parentElement.isA("DrawItem")) {
                    this.parentElement.drawPane.hideDragLine();
                }
            }
            return isc.EH.STOP_BUBBLING;        
        }
    },
    
    editModeDropOver : function () {
        if (!this.editModeWillAcceptDrop()) return false;
        if (!this.shouldPassDropThrough()) {
            this.Super("dropOver", arguments);        
            if (this.parentElement && this.parentElement.hideDropLine) {
                this.parentElement.hideDropLine();
                if (this.parentElement.isA("FormItem")) {
                    this.parentElement.form.hideDragLine();
                } else if (this.parentElement.isA("DrawItem")) {
                    this.parentElement.drawPane.hideDragLine();
                }
            }
            return isc.EH.STOP_BUBBLING;        
        }
    },


// DataBoundComponent functionality
// ---------------------------------------------------------------------------------------

// In editMode, when setDataSource is called, generate editNodes for each field so that the
// user can modify the generated fields.
// On change of DataSource, remove any auto-gen field that the user has not changed.

editModeSetDataSource : function (dataSource, fields, forceRebind) {
    //this.logWarn("editMode setDataSource called" + isc.Log.getStackTrace());
    
    // _loadingNodeTree is a flag set by Visual Builder - its presence indicates that we are 
    // loading a view from disk.  In this case, we do NOT want to perform the special 
    // processing in this function, otherwise we'll end up with duplicate components in the
    // componentTree.  So we'll just fall back to the base impl in that case.
    if (isc._loadingNodeTree) {
        this.baseSetDataSource(dataSource, fields);
        return;
    }
    
    if (dataSource == null) return;
    if (dataSource == this.dataSource && !forceRebind) return;
        
    var fields = this.getFields(),
        keepFields = [],
        removeNodes = [];
    
    // remove all automatically generated fields that have not been edited by the user
    
    if (fields) {
        var tree = this.editContext.getEditNodeTree(),
            parentNode = tree.findById(this.ID),
            children = tree.getChildren(parentNode)
        ;
        for (var i = 0; i < fields.length; i++) {
            var field = fields[i],
                editNode = null
            ;
            for (var j = 0; j < children.length; j++) {
                var child = children[j];
                if (field.name == child.name) {
                    editNode = child;
                    break;
                }
            }

            if (editNode && editNode.autoGen && !this.fieldEdited(this, editNode)) {
                removeNodes.add(editNode);
            } else if (editNode) {
                keepFields.add(field);
            }
        }
        this.setFields(keepFields);
        for (var i = 0; i < removeNodes.length; i++) {
            this.editContext.removeNode(removeNodes[i], true);
        }
    }
    
    
    
    // If this dataSource has a single complex field, use the schema of that field in lieu
    // of the schema that was dropped.
    var schema,
        fields = dataSource.fields;
    if (fields && isc.getKeys(fields).length == 1 &&
        dataSource.fieldIsComplexType(fields[isc.firstKey(fields)].name))
    {
        schema = dataSource.getSchema(fields[isc.firstKey(fields)].type);
    } else {
        schema = dataSource;
    }
        
    // add one editNode for every field in the DataSource that the component would normally
    // display or use.  
    

    var allFields = schema.getFields();
    fields = {};
    
    for (var key in allFields) {
        var field = allFields[key];
        if (!this.shouldUseField(field, dataSource)) continue;
        fields[key] = allFields[key];
        // duplicate the field on the DataSoure - we don't want to have the live component
        // sharing actual field objects with the DataSource
        fields[key] = isc.addProperties({}, allFields[key]);
    }
    
    // Merge the list of fields to keep (because they were manually added, or changed after 
    // generation) with the list of fields on the new DataSource.  Of course, the "list of 
    // fields to keep" could well be the empty list (and always will be if this is the first
    // time we're binding this DataBoundComponent and the user has not manually added fields)
    keepFields.addList(isc.getValues(fields));
    this.baseSetDataSource(dataSource, keepFields);
    
    for (var key in fields) {
        var field = fields[key];
        
        // What constitutes a "field" varies by DBC type
        var fieldConfig = this.getFieldEditNode(field, schema);
        var editNode = this.editContext.makeEditNode(fieldConfig);
        //this.logWarn("editMode setDataSource adding field: " + field.name);
        this.editContext.addNode(editNode, this.editNode, null, null, true);
    }
    //this.logWarn("editMode setDataSource done adding fields");
},

// whether a field has been edited
// Strategy: An edited field will likely have more properties than just
// the base "name" and "title". Therefore if there are more properties
// consider the field edited. Otherwise, if the title is different from
// the auto-generated title or from the original DataSource field title
// then the field title has been edited.
fieldEdited : function (parentCanvas, editNode) {
    var edited = false;
    if (editNode.defaults) {
        var defaults = editNode.defaults,
            hasNonBaseProperties = false
        ;
        for (var key in defaults) {
            if (key == "name" || key == "title" || key.startsWith("_")) continue;
            hasNonBaseProperties = true;
            break;
        }
        if (!hasNonBaseProperties) {
            var name = defaults["name"],
                title = defaults["title"]
            ;
            if (title) {
                var dsTitle;
                if (parentCanvas && parentCanvas.dataSource) {
                    var ds = parentCanvas.dataSource;
                    if (isc.isA.String(ds)) ds = isc.DS.getDataSource(ds);
                    if (ds) {
                        var dsField = ds.getField(name)
                        if (dsField) dsTitle = dsField.title;
                    }
                }
                if ((!dsTitle && title != isc.DataSource.getAutoTitle(name)) || 
                        (dsTitle && title != dsTitle)) 
                {
                    edited = true;
                }
            }
        } else {
            edited = true;
        }
    }
    return edited;
},

// get an editNode from a DataSourceField
getFieldEditNode : function (field, dataSource) {
    // works for ListGrid, TreeGrid, DetailViewer, etc.  DynamicForm overrides
    var fieldType = this.Class + "Field";
    var editNode = {
        type: fieldType,
        autoGen: true,
        defaults: {
            name: field.name,
            // XXX this makes the code more verbose since the title could be left blank and be
            // inherited from the DataSource.  However if we don't supply one here, currently
            // the process of creating an editNode and adding to the editTree generates a title
            // anyway, and without using getAutoTitle().
            title: field.title || dataSource.getAutoTitle(field.name)
        }
    }
    
    return editNode;
}

});



isc.Class.addMethods({
    getSchema : function () {
        // NOTE: this.schemaName allows multiple classes to share a single role within editing,
        // eg the various possible implementations of tabs, section headers, etc
        if (this.schemaName) return isc.DS.get(this.schemaName);
        
        // If we have an SGWT class name, then try to get that schema
        var sgwtClassName = this.getSGWTClassName();
        if (sgwtClassName) {
            var schema = isc.DS.get(sgwtClassName);
            if (schema) return schema;
        }

        // If not available, then get the SmartClient class schema
        return isc.DS.get(this.Class);
    },
    getSchemaField : function (fieldName) {
        return this.getSchema().getField(fieldName);
    },
    getObjectField : function (type) {
        if (!isc.SGWTFactory.getFactory(type) && type.contains(".")) type = type.split(/\./).pop();

        // cache lookups, but only on Canvases.  FIXME: we should really cache lookups only for
        // framework DataSources
        var objectFields = this._objectFields;
        if (isc.isA.Canvas(this)) {
            var undef;
            if (objectFields && objectFields[type] !== undef) {
                //this.logWarn("cache hit: " + type);
                return objectFields[type];
            }
        }

        var schema = this.getSchema();
        if (!schema) {
            this.logWarn("getObjectField: no schema exists for: " + this);
            return;
        }
        var fieldName = schema.getObjectField(type);

        if (isc.isA.Canvas(this)) {
            if (!objectFields) this._objectFields = objectFields = {};
            objectFields[type] = fieldName;
        }

        return fieldName;
    },
    addChildObject : function (newChildType, child, index, parentProperty) {
        return this._doVerbToChild("add", newChildType, child, index, parentProperty);
    },
    removeChildObject : function (childType, child, parentProperty) {
        return this._doVerbToChild("remove", childType, child, parentProperty);
    },

    _doVerbToChild : function (verb, childType, child, index, parentProperty) {
        var fieldName = parentProperty || this.getObjectField(childType);
        var field = this.getSchemaField(fieldName);

        // for fields that aren't set to multiple, call setProperties to add the object, which
        // will look up and use the setter if there is one 
        // (eg field "contextMenu", "setContextMenu")
        if (!field.multiple) {
            var props = {};
            if (verb == "remove") {
                props[fieldName] = null;
            } else {
                props[fieldName] = child;
            }
            this.logInfo(verb + "ChildObject calling setProperties for fieldName '" + fieldName +
                         "'", "editing");
            this.setProperties(props);
            return true;
        }

        var methodName = this.getFieldMethod(childType, fieldName, verb);
        if (methodName != null) {
            this.logInfo("calling " + methodName + "(" + this.echoLeaf(child) + 
                         (index != null ? "," + index + ")" : ")"),
                         "editing");
            this[methodName](child, index);
            return true;
        }

        return false;
    },

    getChildObject : function (type, id, parentProperty) {
        var fieldName = parentProperty || this.getObjectField(type), 
            field = this.getSchemaField(fieldName);

        if (field == null) {
            if (parentProperty) {
                this.logWarn("getChildObject: no such field '" + parentProperty + 
                             "' in schema: " + this.getSchema());
            } else {
                this.logWarn("getChildObject: schema for Class '" + this.Class + 
                             "' does not have a field accepting type: " + type);
            }
            return null;
        }

        // if the field is not array-valued, just use getPropertyValue, which will auto-discover
        // getters
        if (!field.multiple) return this.getPropertyValue(fieldName);

        // otherwise, look for a getter method and call it with the id
        var methodName;
        
        if (isc.isA.ListGrid(this) && fieldName == "fields") {
            methodName = "getSpecifiedField";
        } else {
            methodName = this.getFieldMethod(type, fieldName, "get");
        }

        if (methodName == null) var methodName = this.getFieldMethod(type, fieldName, "get");
        if (methodName && this[methodName]) {
            this.logInfo("getChildObject calling: " + methodName + "('"+id+"')", "editing");
            return this[methodName](id);
        } else {    
            // if there's no getter method, search the Array directly for something with
            // matching id
            this.logInfo("getChildObject calling getArrayItem('"+id+"',this." + fieldName + ")",
                         "editing");
            return isc.Class.getArrayItem(id, this[fieldName]);
        }
    },

    // get a method that can perform verb "verb" for an object of type "type" being added to a
    // field named "fieldName", eg, "add" (verb) a "Tab" (type) to field "tabs".
    // Uses naming conventions to auto-discover methods.  Subclasses may need to override for
    // non-discoverable methods, eg, canvas.addChild() is not discoverable from the field name
    // ("children") or type ("Canvas").
    getFieldMethod : function (type, fieldName, verb) {
        // NOTE: number of args checks: whether it's an add, remove or get, we're looking for
        // something takes arguments, and we don't want to be fooled by eg Class.getWindow()

        var funcName = verb+type;
        // look for add[type] method, e.g. addTab
        if (isc.isA.Function(this[funcName]) && 
            isc.Func.getArgs(this[funcName]).length > 0) 
        {
            return funcName;
        }

        // look for add[singular form of field name] method, e.g. addMember
        if (fieldName.endsWith("s")) {
            funcName = verb + this._withInitialCaps(fieldName.slice(0,-1));
            if (isc.isA.Function(this[funcName]) && 
                isc.Func.getArgs(this[funcName]).length > 0)
            {
                return funcName;
            }
        }
    },

    // Returns a copy of a string with the first character uppercased.
    _withInitialCaps : function (s) {
        // Uppercase the first letter, then add the rest.
        return s.substring(0,1).toLocaleUpperCase() + s.substring(1);
    },
    
    // EditMode OriginalValues
    // ---------------------------------------------------------------------------------------
    // When a component enters editMode it may change appearance or change interactive
    // behavior, for example, a Tab becomes closable via setting canClose.  However if the tab
    // is not intended to be closeable in the actual application, when we edit the tab we want
    // to show canClose as false and if the user changes the value, we want to track that they
    // have changed the value separately from it's temporary setting due to editMode.
    //
    // get/setEditableProperties allows the component to provide specialized properties to a
    // component editor, and saveTo/restoreFromOriginalValues are helpers for a component to
    // track it true, savable state from it's temporary editMode settings

    getEditableProperties : function (fieldNames) {
        var properties = {},
            undef;
        if (!this.editModeOriginalValues) this.editModeOriginalValues = {}; 
        if (!isc.isAn.Array(fieldNames)) fieldNames = [fieldNames];
        for (var i = 0; i < fieldNames.length; i++) {
            // Just in case we're passed fields rather than names
            var fieldName = isc.isAn.Object(fieldNames[i]) ? fieldNames[i].name : fieldNames[i];
            
            var value = null;
            if (this.editModeOriginalValues[fieldName] === undef) {
                this.logInfo("Field " + fieldName + " - value [" + this[fieldName] + "] is " + 
                        "coming from live values", "editModeOriginalValues");
                value = this[fieldName];
                // If this is an observation notification function, pick up the thing being observed,
                // not the notification function!
                
                if (isc.isA.Function(value) && value._isObservation) {
                    value = this[value._origMethodSlot];
                }

            } else {
                this.logInfo("Field " + fieldName + " - value [" + 
                        this.editModeOriginalValues[fieldName] + "] is coming from " + 
                        "original values", "editModeOriginalValues");
                value = this.editModeOriginalValues[fieldName];
            }
            properties[fieldName] = value;
        }
        
        return properties;
    },
    
    // called to apply properties to an object when it is edited in an EditContext (eg Visual
    // Builder) via EditContext.setNodeProperties().
    setEditableProperties : function (properties) {
        var undef;
        if (!this.editModeOriginalValues) this.editModeOriginalValues = {};
        for (var key in properties) {
            if (this.editModeOriginalValues[key] === undef) {
                this.logInfo("Field " + key + " - value is going to live values", 
                        "editModeOriginalValues");
                this.setProperty(key, properties[key]);
            } else {
                this.logInfo("Field " + key + " - value is going to original values", 
                        "editModeOriginalValues");
                this.editModeOriginalValues[key] = properties[key];
            }
        }
        this.editablePropertiesUpdated();
    },

    // called when a child object that is not itself an SC class is having properties applied
    // to it in an EditContext.  Enables cases like a ListGrid handling changes to it's
    // ListGridFields
    setChildEditableProperties : function (liveObject, properties, editNode, editContext) {
        isc.addProperties(liveObject, properties);
    },
    
    saveToOriginalValues : function (fieldNames) {
        var undef;
        if (!this.editModeOriginalValues) this.editModeOriginalValues = {};
        for (var i = 0; i < fieldNames.length; i++) {
            // Just in case we're passed fields rather than names
            var fieldName = isc.isAn.Object(fieldNames[i]) ? fieldNames[i].name : fieldNames[i];
            if (this[fieldName] === undef) {
                // We'll have to store it as explicit null, otherwise the downstream code won't
                // realize we took a copy
                this.editModeOriginalValues[fieldName] = null;
            } else {
                if (this[fieldName] && this[fieldName]._isObservation) {
                    // Pick up the original method, not the notification function set up by
                    // observation.
                    // If we ever restore the method we want to be restoring the underlying functionality
                    // and not restoring a notification function which may no longer be valid.
                    var origMethodName = isc._obsPrefix + fieldName;
                    this.editModeOriginalValues[fieldName] = this[origMethodName];
                } else {
                    this.editModeOriginalValues[fieldName] = this[fieldName];
                }
            }
        }
    },
    
    restoreFromOriginalValues : function (fieldNames) {
        var undef;
        if (!this.editModeOriginalValues) this.editModeOriginalValues = {};
        var logString = "Retrieving fields from original values:"
        var changes = {};
        for (var i = 0; i < fieldNames.length; i++) {
            // Just in case we're passed fields rather than names
            var fieldName = isc.isAn.Object(fieldNames[i]) ? fieldNames[i].name : fieldNames[i];
            if (this.editModeOriginalValues[fieldName] !== undef) {
                changes[fieldName] = this.editModeOriginalValues[fieldName];
                
                // Zap the editModeOriginalValues copy so that future queries will return 
                // the live value
                delete this.editModeOriginalValues[fieldName];
            } else {
            }
        }
        // Note use setProperties() rather than just hanging the attributes onto the live
        // widget blindly.
        // Required because:
        // - StringMethods need to be converted to live methods
        // - Observation will be left intact (setProperties/addProperties will correctly update
        //   the renamed underlying method rather than the notification method sitting in its slot)
        // - setProperties will fire propertyChanged which we use in some cases (For example
        //   to update "canDrag" when "canDragRecordsOut" is updated on a ListGrid)
        
        this.setProperties(changes);
    },
    
    propertyHasBeenEdited : function (fieldName) {
        var undef;
        if (!this.editModeOriginalValues) return false;
        // Just in case we're passed a field rather than a field name
        if (isc.isAn.Object(fieldName)) fieldName = fieldName.name;
        if (this.editModeOriginalValues[fieldName] !== undef) {
            if (isc.isA.Function(this.editModeOriginalValues[fieldName])) return false;
            if (this.editModeOriginalValues[fieldName] != this[fieldName]) return true;
        }
        return false;
    },
    
    // Override if you have a class that needs to be notified when editor properties have 
    // potentially changed
    editablePropertiesUpdated : function () {
        if (this.parentElement) this.parentElement.editablePropertiesUpdated();
    }

});



isc.DataSource.addClassMethods({

    // Given a parent object and child type, use schema to find out what field children
    // of that type are kept under
    // ---------------------------------------------------------------------------------------
    getSchema : function (object) {
        if (isc.isA.Class(object)) return object.getSchema();
        return isc.DS.get(object.schemaName || object._constructor || object.Class);
    },
    getObjectField : function (object, type) {
        if (object == null) return null;
        if (isc.isA.Class(object)) return object.getObjectField(type);

        var schema = isc.DS.getSchema(object);
        if (schema) return schema.getObjectField(type);
    },
    getSchemaField : function (object, fieldName) {
        var schema = isc.DS.getSchema(object);
        if (schema) return schema.getField(fieldName);
    },

    // Add/remove an object to another object, automatically detecting the appropriate field,
    // and calling add/remove functions if they exist on the parent
    // ---------------------------------------------------------------------------------------
    addChildObject : function (parent, newChildType, child, index, parentProperty) {
        return this._doVerbToChild(parent, "add", newChildType, child, index, parentProperty);
    },
    removeChildObject : function (parent, childType, child, parentProperty) {
        return this._doVerbToChild(parent, "remove", childType, child, parentProperty);
    },
    _doVerbToChild : function (parent, verb, childType, child, index, parentProperty) {
        var fieldName = parentProperty || isc.DS.getObjectField(parent, childType);

        if (fieldName == null) {
            this.logWarn("No field for child of type " + childType);
            return false;
        }

        this.logInfo(verb + " object " + this.echoLeaf(child) + 
                     " in field: " + fieldName +
                     " of parentObject: " + this.echoLeaf(parent), "editing");
        var field = isc.DS.getSchemaField(parent, fieldName);

        // if it's a Class, call doVerbToChild on it, which will look for a method that
        // modifies the field
        if (isc.isA.Class(parent)) {
            // if that worked, we're done
            if (parent._doVerbToChild(verb, childType, child, index, parentProperty)) return true;
        }

        // either it's not a Class, or no appropriate method was found, we'll just directly
        // manipulate the properties

        if (!field.multiple) {
            // simple field: "add" is assignment, "remove" is deletion
            if (verb == "add") parent[fieldName] = child;
            else if (verb == "remove") {
                // NOTE: null check avoids creating null slots on no-op removals
                if (parent[fieldName] != null) delete parent[fieldName];
            } else {
                this.logWarn("unrecognized verb: " + verb);
                return false;
            }
            return true;
        }

        this.logInfo("using direct Array manipulation for field '" + fieldName + "'", "editing");

        // Array field: add or remove at index
        var fieldArray = parent[fieldName];
        if (verb == "add") {
            if (fieldArray != null && !isc.isAn.Array(fieldArray)) {
                this.logWarn("unexpected field value: " + this.echoLeaf(fieldArray) +
                             " in field '" + fieldName + 
                             "' when trying to add child: " + this.echoLeaf(child));
                return false;
            }
            if (fieldArray == null) parent[fieldName] = fieldArray = [];
            if (index != null) fieldArray.addAt(child, index);
            else fieldArray.add(child);
        } else if (verb == "remove") {
            if (!isc.isAn.Array(fieldArray)) return false;
            if (index != null) fieldArray.removeAt(child, index);
            else fieldArray.remove(child);
        } else {
            this.logWarn("unrecognized verb: " + verb);
            return false;
        }

        return true;
    },

    getChildObject : function (parent, type, id, parentProperty) {
        if (isc.isA.Class(parent)) return parent.getChildObject(type, id, parentProperty);

        var fieldName = isc.DS.getObjectField(parent, type), 
            field = isc.DS.getSchemaField(parent, fieldName);


        var value = parent[fieldName];
        //this.logWarn("getting type: " + type + " from field: " + fieldName +
        //             ", value is: " + this.echoLeaf(value));
        if (!field.multiple) return value;

        if (!isc.isAn.Array(value)) return null;
        return isc.Class.getArrayItem(id, value);
    },

    // AutoId: field that should have some kind of automatically assigned ID to make the object
    // referenceable in a builder environment
    // ---------------------------------------------------------------------------------------
    getAutoIdField : function (object) {
        var schema = this.getNearestSchema(object);
        return schema ? schema.getAutoIdField() : "ID";
    },

    getAutoId : function (object) {
        var fieldName = this.getAutoIdField(object);
        return fieldName ? object[fieldName] : null;
    }
});

isc.DataSource.addMethods({
    getAutoIdField : function () {
        return this.getInheritedProperty("autoIdField") || "ID";
    },

    // In the Visual Builder, whether a component should be create()d before being added to
    // it's parent.
    // ---------------------------------------------------------------------------------------
    shouldCreateStandalone : function () {
        if (this.createStandalone != null) return this.createStandalone;
        if (!this.superDS()) return true;
        return this.superDS().shouldCreateStandalone();
    }
});


// Overrides for components that support in-place editing of title
var sharedEditModeFunctions = {
    editModeClick : function () {
        if (isc.VisualBuilder && isc.VisualBuilder.titleEditEvent == "click") this.editClick();
        return this.Super("editModeClick", arguments);
    },
    editModeDoubleClick : function () {
        if (isc.VisualBuilder && isc.VisualBuilder.titleEditEvent == "doubleClick") this.editClick();
        return this.Super("editModeDoubleClick", arguments);
    }
}

isc.Button.addProperties(sharedEditModeFunctions);
isc.ImgButton.addMethods(sharedEditModeFunctions);
isc.StretchImgButton.addMethods(sharedEditModeFunctions);
isc.SectionHeader.addMethods(sharedEditModeFunctions);
isc.ImgSectionHeader.addMethods(sharedEditModeFunctions);

isc.ImgSectionHeader.addMethods({

    setEditMode : function(editingOn, editContext, editNode) {

        if (editingOn == null) editingOn = true;
        if (editingOn == this.editingOn) return;

        this.invokeSuper(isc.TabSet, "setEditMode", editingOn, editContext, editNode);

        if (this.editingOn) {
            // "background" doesn't yet exist - is presumably created asynchronously
            var sectionHeader = this;
            isc.Timer.setTimeout(function () {
                sectionHeader.saveToOriginalValues(["background"]);
                sectionHeader.background.setProperties({
                    iconClick: sectionHeader.editModeIconClick
                })
            }, 0);
        } else {
            this.restoreFromOriginalValues(["background"]);
        }
        
    },
    editModeIconClick : function () {
        var header = this.creator;
        if (header) {
            var stack = header.layout;
            if (stack.sectionIsExpanded(header)) stack.collapseSection(header);
            else stack.expandSection(header);
            var ctx = header.editContext;
            if (ctx) {
                ctx.setNodeProperties(header.editNode, 
                                      {"expanded" : stack.sectionIsExpanded(header)});
            }
        }
        return this.Super("editModeClick", arguments);
    },
    editClick : function () {
            
        var left = this.getPageLeft() + this.getLeftBorderSize() + this.getLeftMargin() + 1 
                                                  - this.getScrollLeft(),
            width = this.getVisibleWidth() - this.getLeftBorderSize() - this.getLeftMargin() 
                                 - this.getRightBorderSize() - this.getRightMargin() - 1;

        isc.Timer.setTimeout({target: isc.EditContext,
                              methodName: "manageTitleEditor", 
                              args: [this, left, width]}, 100);
    }
});


isc.StatefulCanvas.addMethods({
    editClick : function () {

        var left, width;
            
        if (isc.isA.Button(this)) {  // This includes Labels and SectionHeaders
            left = this.getPageLeft() + this.getLeftBorderSize() + this.getLeftMargin() + 1 
                                                  - this.getScrollLeft(); 
            width = this.getVisibleWidth() - this.getLeftBorderSize() - this.getLeftMargin() 
                               - this.getRightBorderSize() - this.getRightMargin() - 1;
        } else if (isc.isA.StretchImgButton(this)) {
            left = this.getPageLeft() + this.capSize;
            width = this.getVisibleWidth() - this.capSize * 2;
        } else {
            isc.logWarn("Ended up in editClick with a StatefulCanvas of type '" + 
                        this.getClass() + "'.  This is neither a Button " +
                        "nor a StretchImgButton - editor will work, but will hide the " +
                        "entire component it is editing");
            left = this.getPageLeft();
            width = this.getVisibleWidth();
        }

        isc.Timer.setTimeout({target: isc.EditContext,
                              methodName: "manageTitleEditor", 
                              args: [this, left, width]}, 0);

    },

    // This function is only called for ImgTabs that need to be scrolled into view
    repositionTitleEditor : function () {
        var left = this.getPageLeft() + this.capSize,
            width = this.getVisibleWidth() - this.capSize * 2;
        
        isc.EditContext.positionTitleEditor(this, left, width);
    }
        

});


// Edit Mode impl for TabSet
// -------------------------------------------------------------------------------------------
if (isc.TabSet) {
isc.TabSet.addClassProperties({
    addTabEditorHint: "Enter tab titles (comma separated)"
});

isc.TabSet.addProperties({

defaultPaneDefaults: {
    _constructor: "VLayout"
},

setEditMode : function(editingOn, editContext, editNode) {

    if (editingOn == null) editingOn = true;
    if (editingOn == this.editingOn) return;

    this.invokeSuper(isc.TabSet, "setEditMode", editingOn, editContext, editNode);
    // If we're going into edit mode, add close icons to every tab
    if (this.editingOn) {
        for (var i = 0; i < this.tabs.length; i++) {
            var tab = this.tabs[i];
            this.saveOriginalValues(tab);
            this.setCanCloseTab(tab, true);
        }
        this.closeClick = function(tab) {
            this.editContext.removeNode(tab.editNode);
            var tabSet = this;
            isc.Timer.setTimeout(function() {tabSet.manageAddIcon()}, 200);
        }
    } else {
        // If we're coming out of edit mode, revert to whatever was on the init data
        for (var i = 0; i < this.tabs.length; i++) {
            var tab = this.tabs[i];
            this.restoreOriginalValues(tab);
            var liveTab = this.getTab(tab);
            this.setCanCloseTab(tab, liveTab.editNode.defaults.canClose);
        }
    }
    
    // Set edit mode on the TabBar and PaneContainer.  Note that we deliberately pass null as
    // the editNode - this allows the components to pick up the special editMode method 
    // overrides, but prevents them from actually being edited
    this.tabBar.setEditMode(editingOn, editContext, null);
    this.paneContainer.setEditMode(editingOn, editContext, null);
    
    this.manageAddIcon();
    
},

saveOriginalValues : function (tab) {
    var liveTab = this.getTab(tab);
    if (liveTab) {
        liveTab.saveToOriginalValues(["closeClick", "canClose", "icon", "iconSize",
                                      "iconOrientation", "iconAlign", "disabled"]);
    }
},

restoreOriginalValues : function (tab) {
    var liveTab = this.getTab(tab);
    if (liveTab) {
        liveTab.restoreFromOriginalValues(["closeClick", "canClose", "icon", "iconSize",
                                           "iconOrientation", "iconAlign", "disabled"]);
    }
},

showAddTabEditor : function () {
        
    var pos = this.tabBarPosition,
        align = this.tabBarAlign,
        top, left, 
        height, width, 
        bar = this.tabBar;
    
    if (pos == isc.Canvas.TOP || pos == isc.Canvas.BOTTOM) {
        // Horizontal tabBar
        top = this.tabBar.getPageTop();
        height = this.tabBar.getHeight();
        if (align == isc.Canvas.LEFT) {
            left = this.addIcon.getPageLeft();
            width = this.tabBar.getVisibleWidth() - this.addIcon.left;
            if (width < 150) width = 150;
        } else {
            width = this.tabBar.getVisibleWidth();
            width = width - (width - (this.addIcon.left + this.addIcon.width));
            if (width < 150) width = 150;
            left = this.addIcon.getPageLeft() + this.addIcon.width - width;
        }
    } else {
        // Vertical tabBar
        left = this.tabBar.getPageLeft();
        width = 150;
        top = this.addIcon.getPageTop();
        height = 20;
    }
    
    this.manageAddTabEditor(left, width, top, height);
},
        
manageAddIcon : function () {

    if (this.editingOn) {
        if (this.addIcon == null) {
            this.addIcon = isc.Img.create({
                autoDraw: false, width: 16, height: 16,
                cursor: "hand",
                tabSet: this,
                src: "[SKIN]/actions/add.png",
                click: function() {this.tabSet.showAddTabEditor();}
            });
            this.tabBar.addChild(this.addIcon);
        }

        var lastTab = this.tabs.length == 0 ? null : this.getTab(this.tabs[this.tabs.length-1]);
        var pos = this.tabBarPosition,
            align = this.tabBarAlign,
            addIconLeft,
            addIconTop;

        if (lastTab == null) {
            // Empty tabBar
            if (pos == isc.Canvas.TOP || pos == isc.Canvas.BOTTOM) {
                // Horizontal tabBar
                if (align == isc.Canvas.LEFT) {
                    addIconLeft = this.tabBar.left + 10;
                    addIconTop = this.tabBar.top + (this.tabBar.height/2) - (8);
                } else {
                    addIconLeft = this.tabBar.left + this.tabBar.width - 10 - (16);  // 16 = icon width
                    addIconTop = this.tabBar.top + (this.tabBar.height/2) - (8);
                }
            } else {
                // Vertical tabBar
                if (align == isc.Canvas.TOP) {
                    addIconLeft = this.tabBar.left + (this.tabBar.width/2) - (8);
                    addIconTop = this.tabBar.top + 10;
                } else {
                    addIconLeft = this.tabBar.left + (this.tabBar.width/2) - (8);
                    addIconTop = this.tabBar.top + this.tabBar.height - 10 - (16)
                }
            }
        } else {
            if (pos == isc.Canvas.TOP || pos == isc.Canvas.BOTTOM) {
                // Horizontal tabBar
                if (align == isc.Canvas.LEFT) {
                    addIconLeft = lastTab.left + lastTab.width + 10;
                    addIconTop = lastTab.top + (lastTab.height/2) - (8);
                } else {
                    addIconLeft = lastTab.left - 10 - (16);  // 16 = icon width
                    addIconTop = lastTab.top + (lastTab.height/2) - (8); // 8 = half icon height
                }
            } else {
                // Vertical tabBar
                if (align == isc.Canvas.TOP) {
                    addIconLeft = lastTab.left + (this.width/2) - (8);
                    addIconTop = lastTab.top + (lastTab.height) + 10;
                } else {
                    addIconLeft = lastTab.left + (this.width/2) - (8);
                    addIconTop = lastTab.top + (lastTab.height/2) - (8); 
                }
            }
        }
    
        this.addIcon.setTop(addIconTop);
        this.addIcon.setLeft(addIconLeft);
        this.addIcon.show();
    } else {
        if (this.addIcon && this.addIcon.hide) this.addIcon.hide();
    }
},

manageAddTabEditor : function (left, width, top, height) {
        
    if (!isc.isA.DynamicForm(isc.TabSet.addTabEditor)) {
        isc.TabSet.addTabEditor = isc.DynamicForm.create({
            autoDraw: false,
            margin: 0, padding: 0, cellPadding: 0,
            fields: [
                { 
                    name: "addTabString", type: "text", 
                    hint: isc.TabSet.addTabEditorHint,
                    showHintInField: true,
                    showTitle: false,
                    keyPress : function (item, form, keyName) {
                        if (keyName == "Escape") {
                            form.discardUpdate = true;
                            form.hide();
                            return
                        }
                        if (keyName == "Enter") item.blurItem();
                    }, 
                    blur : function (form, item) {
                        if (!form.discardUpdate) {
                            form.targetComponent.editModeAddTabs(item.getValue());
                        }
                        form.hide();
                    }
                }
            ]
        });
    }
    
    var editor = isc.TabSet.addTabEditor;
    editor.addProperties({targetComponent: this});
    editor.discardUpdate = false;
    
    var item = editor.getItem("addTabString");
    item.setHeight(height);
    item.setWidth(width);
    item.setValue(item.hint);
    
    editor.setTop(top);
    editor.setLeft(left);
    editor.show();
    item.focusInItem();
    item.delayCall("selectValue", [], 100);
},

editModeAddTabs : function (addTabString) {
    if (!addTabString || addTabString == isc.TabSet.addTabEditorHint) return;
    var titles = addTabString.split(",");
    for (var i = 0; i < titles.length; i++) {
        var tab = {
            type: "Tab",
            defaults: {
                title: titles[i]
            }
        };
        var node = this.editContext.addNode(this.editContext.makeEditNode(tab), 
                                                 this.editNode);
        this.editModeAddDefaultPane(node);
    }
},

editModeAddDefaultPane : function (tabNode) {
    if (!tabNode) return;
    var defaultPane = isc.addProperties({}, this.defaultPaneDefaults);
    if (!defaultPane.type && !defaultPane.className) {
        defaultPane.type = defaultPane._constructor;
    }
    this.editContext.addNode(this.editContext.makeEditNode(defaultPane), tabNode);
},

// Extra stuff to do when tabSet.addTabs() is called when the tabSet is in an editable context
// (though not necessarily actually in editMode)
addTabsEditModeExtras : function (newTabs) {

    // Put this on a delay, to give the new tab chance to draw before we start querying its 
    // drawn size and position
    this.delayCall("manageAddIcon");
    
    // If the TabSet is in editMode, put the new tab(s) into edit mode too
    if (this.editingOn) {
        for (var i = 0; i < newTabs.length; i++) {
            this.saveOriginalValues(newTabs[i]);
            this.setCanCloseTab(newTabs[i], true);
        }
    }
},

// Extra stuff to do when tabSet.removeTabs() is called when the tabSet is in an editable 
// context (though not necessarily actually in editMode)
removeTabsEditModeExtras : function () {

    // Put this on a delay, to give the new tab chance to draw before we start querying its 
    // drawn size and position
    this.delayCall("manageAddIcon");
},

//Extra stuff to do when tabSet.reorderTab() is called when the tabSet is in an editable 
//context (though not necessarily actually in editMode)
reorderTabsEditModeExtras : function (originalPosition, moveToPosition) {
    if (this.editContext && this.editContext.reorderNode) {
        this.editContext.reorderNode(this.editNode, originalPosition, moveToPosition);
    }
},

// Override editablePropertiesUpdated() to invoke manageAddIcon() when things change
editablePropertiesUpdated : function () {
    this.delayCall("manageAddIcon");
    this.invokeSuper(isc.TabSet, "editablePropertiesUpdated");
},

tabScrolledIntoView : function () {
    if (!this.editingOn) return;
    for (var i = 0; i < this.tabs.length; i++) {
        var liveTab = this.getTab(this.tabs[i]);
        if (liveTab.titleEditor && liveTab.titleEditor.isVisible()) {
            liveTab.repositionTitleEditor();
        }
    }
},

// Override of Canvas.findEditNode.  If the item being dragged is a Tab, falls back to the 
// Canvas impl (which will return the TabSet itself).  If the item being dragged is not a 
// Tab, returns the currently selected Tab if it has an editNode, otherwise the first Tab 
// with an editNode, otherwise returns the result of calling the parent element's 
// findEditNode(), because this is a TabSet with no tabs in edit mode
findEditNode : function (dragType) {
    this.logInfo("In TabSet.findEditNode, dragType is " + dragType, "editModeDragTarget");
    if (dragType != "Tab") {
        var tab = this.getTab(this.getSelectedTabNumber());
        if (tab && tab.editNode) return tab;
        for (var i = 0; i < this.tabs.length; i++) {
            tab = this.getTab(i);
            if (tab.editNode) return tab;
        }
        if (this.parentElement) return this.parentElement.findEditNode(dragType);
    }
    return this.Super("findEditNode", arguments);
},

// Override completeItemDrop() to add the default pane to tabs (and drop into 
// edit-title)
completeItemDrop : function (paletteNode, itemIndex, rowNum, colNum, side, callback) {
    this.Super("completeItemDrop", arguments);
    if (paletteNode && (paletteNode.type || paletteNode.className) == "Tab") {
        var liveObj = paletteNode.liveObject;
        this.editModeAddDefaultPane(paletteNode);
        this.selectTab(liveObj);
        liveObj.delayCall("editClick"); 
    }
}

});

isc.TabBar.addMethods({
    findEditNode : function (dragType) {
        
        if (dragType == "Tab") {
            // Delegate to the TabSet's findEditNode()
            return this.parentElement.findEditNode(dragType);
        } else if (this.parentElement && isc.isA.Layout(this.parentElement.parentElement)) {
            return this.parentElement.parentElement.findEditNode(dragType);
        }
        
        return this.Super("findEditNode", arguments);
    }
});
}
// Edit Mode impl for Layout
// -------------------------------------------------------------------------------------------

isc.Layout.addMethods({

// Note that setEditMode() at the Canvas level applies editModeDrop et al to the live canvas.
editModeDrop : function () {

    if (this.shouldPassDropThrough()) {
        this.hideDropLine();
        return;
    }

    isc.EditContext.hideAncestorDragDropLines(this);

    var source = isc.EH.dragTarget,
        paletteNode,
        dropType;

    if (!source.isA("Palette")) {
        if (source.isA("FormItemProxyCanvas")) {
            source = source.formItem;
        }
        dropType = source._constructor || source.Class;
    } else {
        paletteNode = source.transferDragData();
        if (isc.isAn.Array(paletteNode)) paletteNode = paletteNode[0];
        paletteNode.dropped = true;
        dropType = paletteNode.type || paletteNode.className;
    }
     
    // Establish the actual drop node (this may not be the canvas accepting the drop - for a
    // composite component like TabSet, the dropped-on canvas will be the tabBar or 
    // paneContainer)
    var dropTargetNode = this.findEditNode(dropType);
    if (dropTargetNode) {
        dropTargetNode = dropTargetNode.editNode;
    }
    
    // modifyEditNode() is a late-modify hook for components with unusual drop requirements
    // that don't fit in with the normal scheme of things (SectionStack only, as of August 09).
    // This method can be used to modify the editNode that is going to be the parent - or 
    // replace it with a whole different one 
    if (this.modifyEditNode) {
        dropTargetNode = this.modifyEditNode(paletteNode, dropTargetNode, dropType);
        if (!dropTargetNode) {
            this.hideDropLine();
            return isc.EH.STOP_BUBBLING;
        }
    }
            
    
    // if the source isn't a Palette, we're drag/dropping an existing component, so remove the 
    // existing component and re-create it in its new position
    if (!source.isA("Palette")) {
        if (isc.EditContext._dragHandle) isc.EditContext._dragHandle.hide();
        if (source == this) return;  // Can't drop a component onto itself
        var tree = this.editContext.getEditNodeTree(),
            oldParent = tree.getParent(source.editNode),
            oldIndex = tree.getChildren(oldParent).indexOf(source.editNode),
            newIndex = this.getDropPosition(dropType);
        this.editContext.removeNode(source.editNode);
        
        // If we've moved the child component to a slot further down in the same parent, 
        // indices will now be off by one because we've just removeed it from its old slot
        if (oldParent == this.editNode && newIndex > oldIndex) newIndex--;
        var node;
        if (source.isA("FormItem")) {
            // If the source is a CanvasItem, unwrap it and insert the canvas into this Layout
            // directly; otherwise, we would end up with teetering arrangments of Canvases in
            // inside CanvasItems inside DynamicForms inside CanvasItems inside DynamicForms...
            if (source.isA("CanvasItem")) {
                node = this.editContext.addNode(source.canvas.editNode, dropTargetNode, newIndex);
            } else {
                // Wrap the FormItem in a DynamicForm
                node = this.editContext.addWithWrapper(source.editNode, dropTargetNode);
            }
        } else if (source.isA("DrawItem")) {
            // Wrap the DrawItem in a DrawPane
            node = this.editContext.addWithWrapper(source.editNode, dropTargetNode, true);
        } else {
            node = this.editContext.addNode(source.editNode, dropTargetNode, newIndex);
        }
        if (isc.isA.TabSet(dropTargetNode.liveObject)) {
            dropTargetNode.liveObject.selectTab(source);
        } else if (node && node.liveObject) {
            isc.EditContext.delayCall("selectCanvasOrFormItem", [node.liveObject, true], 200);
        }
    } else {

        var nodeAdded;
        var clazz = isc.ClassFactory.getClass(dropType);
        if (clazz && clazz.isA("FormItem")) {
            // Create a wrapper form to allow the FormItem to be added to this Canvas
            nodeAdded = this.editContext.addWithWrapper(paletteNode, dropTargetNode);
        } else if (clazz && clazz.isA("DrawItem")) {
            // Create a wrapper form to allow the DrawItem to be added to this Canvas
            nodeAdded = this.editContext.addWithWrapper(paletteNode, dropTargetNode, true);
        } else {
            nodeAdded = this.editContext.addNode(paletteNode, dropTargetNode,
                                          this.getDropPosition(dropType));
        }
        // FIXME - this is almost hackery, needs to be factored more cleanly
        if (nodeAdded != null) {
            var liveObj = paletteNode.liveObject;
            if (isc.isA.TabSet(liveObj)) {
                liveObj.delayCall("showAddTabEditor");
            } else if (isc.isA.ImgTab(liveObj) ||
                       isc.isA.Button(liveObj) ||
                       isc.isA.StretchImgButton(liveObj) ||
                       isc.isA.SectionHeader(liveObj) ||
                       isc.isA.ImgSectionHeader(liveObj)) {
                // Give the object a chance to draw before we start the edit, otherwise the 
                // editor co-ordinates will be wrong
                liveObj.delayCall("editClick");
            }
        }
    }
    
    this.hideDropLine();
    return isc.EH.STOP_BUBBLING;
    
},

editModeDropMove : function () {
    if (!this.editModeWillAcceptDrop()) return false;
    if (!this.shouldPassDropThrough()) {
        this.Super("dropMove", arguments);
        if (this.parentElement && this.parentElement.hideDropLine) {
            this.parentElement.hideDropLine();
            if (this.parentElement.isA("FormItem")) {
                this.parentElement.form.hideDragLine();
            } else if (this.parentElement.isA("DrawItem")) {
                this.parentElement.drawPane.hideDragLine();
            }
        }
        return isc.EH.STOP_BUBBLING;        
    } else {
        this.hideDropLine();
    }
},

editModeDropOver : function () {
    if (!this.editModeWillAcceptDrop()) return false;
    if (!this.shouldPassDropThrough()) {
        this.Super("dropOver", arguments);        
        if (this.parentElement && this.parentElement.hideDropLine) {
            this.parentElement.hideDropLine();
            if (this.parentElement.isA("FormItem")) {
                this.parentElement.form.hideDragLine();
            } else if (this.parentElement.isA("DrawItem")) {
                this.parentElement.drawPane.hideDragLine();
            }
        }
        return isc.EH.STOP_BUBBLING;        
    } else {
        this.hideDropLine();
    }
}

});    


// Edit Mode impl for PortalLayout and friends
// -------------------------------------------------------------------------------------------
//
// Note that PortalLayout and friends have some special features with respect to EditMode.
//
// 1. Even in "live" mode (rather than just "edit" mode), you can drag nodes from a Palette to
//    a PortalLayout and it will do the right thing -- it will create the liveObject from the node,
//    and, if necessary, wrap it in a Portlet. Of course, you have to be in "edit" mode to edit
//    the contents of a Portlet.
//
// 2. The normal user interface of PortalLayout allows the user to adjust the number of columns,
//    move columns around, move Portlets around, etc. Even in "live" mode, the code will adjust
//    the editNodes so that they correspond to the user's actions. You can see this in 
//    Visual Builder, for instance, by creating a PortalLayout with some Portlets in "edit" mode,
//    and then switching to "live" mode and moving the Portlets around -- the editNodes will follow.
//
// In order to make this work, there are some bits of code in Portal.js that take account of
// edit mode, but the larger pieces that can be broken out separately are here.

isc.Portlet.addProperties({
    canAdd : function (type) {
        // Don't let Portlets be added directly to Portlets, because it is almost never what
        // would be wanted.
        if (type == "Portlet") return false;
        return this.Super("canAdd", arguments);
    },
    
    updateEditNode : function (editContext, editNode) {
        if (editContext.persistCoordinates) {
            // We only save if the user has specified a width
            var width = this._percent_width || this._userWidth;
            if (width) {
                editContext.setNodeProperties(editNode, {
                    width: width
                }, true);
            } else {
                editContext.removeNodeProperties(editNode, "width");
            }
        }
    }
});

isc.PortalRow.addProperties({
    updateEditNode : function (editContext, editNode) {
        if (editContext.persistCoordinates) {
            // We only save if the user has specified a height
            var height = this._percent_height || this._userHeight;
            if (height) {
                editContext.setNodeProperties(editNode, {
                    height: height
                }, true);
            } else {
                editContext.removeNodeProperties(editNode, "height");
            }
        }
    },

    wrapChildNode : function (editContext, newNode, parentNode, index) {
        var liveObject = newNode.liveObject;

        if (isc.isA.Portlet(liveObject)) {
            // If it's a portlet, then we're fine
            return parentNode;
        } else {
            // If it's something else, we'll wrap it in a Portlet
            var portletNode = editContext.makeEditNode({
                type: "Portlet",
                defaults: {
                    title: newNode.title,
                    destroyOnClose: true
                }
            });

            editContext.addNode(portletNode, parentNode, index);
            return portletNode;
        }
    },

    // Called from getDropComponent to deal with drops from palettes
    handleDroppedEditNode : function (dropComponent, dropPosition) {
        var editContext = this.editContext;
        var editNode = this.editNode;

        if (isc.isA.Palette(dropComponent)) {
            // Drag and drop from palette
            var data = dropComponent.transferDragData(),
                component = (isc.isAn.Array(data) ? data[0] : data);
        
            if (editContext && editNode) {
                // If we have an editContext and editNode, just use them. The wrapping
                // is handled by wrapChildNode in this case. We return false to cancel the drop,
                // since addNode will have taken care of it.
                editContext.addNode(component, editNode, dropPosition);
                return false;
            } else {
                // If we don't have an editContext and editNode. then we'll wrap the liveObject
                // in a Portlet if necessary.
                if (isc.isA.Portlet(component.liveObject)) {
                    // If it's a Portlet, we're good
                    dropComponent = component.liveObject;
                } else {
                    // If not, we'll wrap it in one
                    dropComponent = isc.Portlet.create({
                        autoDraw: false,
                        title: component.title,
                        items: [component.liveObject],
                        destroyOnClose: true
                    });
                }
            }
        }

        return dropComponent;
    }
});

isc.PortalColumnBody.addProperties({
    
    // Called from getDropComponent to deal with drops from palettes
    handleDroppedEditNode : function (dropComponent, dropPosition) {
        var editContext = this.creator.editContext;
        var editNode = this.creator.editNode;

        if (isc.isA.Palette(dropComponent)) {
            // Drag and drop from palette
            var data = dropComponent.transferDragData(),
                component = (isc.isAn.Array(data) ? data[0] : data);

            if (editContext && editNode) {
                // If we have an editContext and editNode, just use them. The wrapping
                // is handled by wrapChildNode in this case. We return false to cancel the drop,
                // since addNode will have taken care of it.
                editContext.addNode(component, editNode, dropPosition);
                return false;
            } else {
                // If we don't have an editContext and editNode, then wrap the liveObject
                // in a Portlet if necessary.
                if (isc.isA.Portlet(component.liveObject)) {
                    // If it's a Portlet, we're good
                    dropComponent = component.liveObject;
                } else {
                    // If not, we'll wrap it in one
                    dropComponent = isc.Portlet.create({
                        autoDraw: false,
                        title: component.title,
                        items: [component.liveObject],
                        destroyOnClose: true
                    });
                }
            }
        }
     
        if (dropComponent) {
            // We need to check whether the dropComponent is already the only portlet
            // in an existing row. If so, we can simplify by just dropping
            // the row -- that is what the user will have meant. 
            var currentRow = dropComponent.portalRow;
            if (currentRow && currentRow.parentElement == this && currentRow.getMembers().length == 1) {
                // Check whether we need to adjust the editNodes
                if (editContext && editNode && currentRow.editNode) {
                    var currentIndex = this.getMemberNumber(currentRow);

                    // Check if we're not really changing position
                    if (dropPosition == currentIndex || dropPosition == currentIndex + 1) return;
                    editContext.removeNode(currentRow.editNode);
                    
                    // Adjust dropPosition if we are dropping after the currentIndex
                    if (currentIndex < dropPosition) dropPosition -= 1;
                    editContext.addNode(currentRow.editNode, editNode, dropPosition); 
                    
                    return null;
                }
            } else {
                // If we're not moving a whole current row, then we add the new portlet, creating a new row
                if (editContext && editNode && dropComponent.editNode) {
                    editContext.addNode(dropComponent.editNode, editNode, dropPosition);
                    return null;
                }
            }
        }

        // We'll get here if we're not doing something special with the dropComponent's editNode ...
        // in that case, we can return it and getDropComponent can handle it. 
        return dropComponent;
    }
});

isc.PortalColumn.addProperties({
    wrapChildNode : function (editContext, newNode, parentNode, index) {
        var liveObject = newNode.liveObject;

        if (isc.isA.PortalRow(liveObject) || newNode.type == "PortalRow") {
            // If it's a PortalRow, then we're fine
            return parentNode;
        } else if (isc.isA.Portlet(liveObject)) {
            // If it's a portlet, then we'll wrap it in a row
            var rowNode = editContext.makeEditNode({
                type: this.rowConstructor,
                defaults: {}
            });
            editContext.addNode(rowNode, parentNode, index);
            return rowNode;
        } else {
            // If it's something else, we'll wrap it in a Portlet
            var portletNode = editContext.makeEditNode({
                type: "Portlet",
                defaults: {
                    title: newNode.title,
                    destroyOnClose: true
                }
            });
            // Note that when we add the Portlet node, we'll eventually
            // get back here to wrap it in a PortalRow, so we don't need
            // to take care of that explicitly (though we could).
            editContext.addNode(portletNode, parentNode, index);
            return portletNode;
        }
    },
    
    updateEditNode : function (editContext, editNode) {
        if (editContext.persistCoordinates) {
            // We only save if the user has specified a width
            var width = this._percent_width || this._userWidth;
            if (width) {
                editContext.setNodeProperties(editNode, {
                    width: width
                }, true);
            } else {
                editContext.removeNodeProperties(editNode, "width");
            }
        }
    },

    // We don't actually want to add anything via drag & drop ... that will be
    // handled by PortalColumnBody
    canAdd : function (type) {
        return false;
    },
    
    setEditMode : function (editingOn, editContext, editNode) {
        this.Super("setEditMode", arguments);

        // We need to put the body in editMode, because the drag/drop behaviours belong there.  
        this.rowLayout.setEditMode(editingOn, editContext, null); 
    }
});

isc.PortalLayout.addProperties({
    // We need to do some special things when we learn of our EditContext and EditNode
    addedToEditContext : function (editContext, editNode) {
        // We may need to add our PortalColumns to the EditContext, since they may have already been created.
        for (var i = 0; i < this.getNumColumns(); i++) {
            var column = this.getPortalColumn(i);
            
            if (!column.editContext) {
                // Create the editNode, supplying the liveObject
                var node = editContext.makeEditNode({
                    type: this.columnConstructor,
                    liveObject: column, 
                    defaults: {
                        ID: column.ID,
                        _constructor: this.columnConstructor
                    }
                });
                
                // Add it to the EditContext, without adding the liveObject to the parent, since it's
                // already there.
                editContext.addNode(node, editNode, i, null, true); 
            }
        }

        // And we should change our defaults to specify numColumns: 0, because otherwise we'll
        // initialize the default 2 columns when restored, which isn't what will be wanted
        editNode.defaults.numColumns = 0;
    }
});


// Edit Mode impl for DynamicForm
// -------------------------------------------------------------------------------------------
if (isc.DynamicForm) {
    
isc.DynamicForm.addProperties({

setEditMode : function(editingOn, editContext, editNode) {

    if (editingOn == null) editingOn = true;
    if (editingOn == this.editingOn) return;

    this.invokeSuper(isc.DynamicForm, "setEditMode", editingOn, editContext, editNode);
    
    // Canvas level implementation already switched on ability to drop components.
    // Add ability to drop items / add columns
    if (this.editingOn) {
        this.saveToOriginalValues(["canDropItems", "canAddColumns", "dropOut"]);
        this.setProperties({
            canDropItems: true,
            canAddColumns: true,
            dropOut: this.editModeDropOut
        });
        // Throw away anything the user might have typed in live mode
        this.resetValues();
    } else {
        // If we're coming out of edit mode, revert to whatever we saved
        this.restoreFromOriginalValues(["canDropItems", "canAddColumns", "dropOut"]);
        // Set default values from whatever the user typed into the formItems in edit mode
        this.resetValues();
    }
},

// editModeDropOver et al picked up by Canvas level setEditMode() implementation.
editModeDropOver : function () {
    if (this.canDropItems != true) return false;
    if (!this.editModeWillAcceptDrop()) return false;
    this._lastDragOverItem = null;
    // just to be safe
    this.hideDragLine();
    return isc.EH.STOP_BUBBLING;        
},

editModeDropMove : function () {

    if (!this.ns.EH.getDragTarget()) return false;
    if (this.canDropItems != true) return false;
    if (!this.editModeWillAcceptDrop()) return false;
    
    // DataSource is a special case - we accept drop, but show no drag line
    var item = this.ns.EH.getDragTarget().getDragData();
    if (isc.isAn.Array(item)) item = item[0];
    if (item && (item.type || item.className) == "DataSource") {
        this.hideDragLine();
        return isc.EH.STOP_BUBBLING;
    }

    // If the form has no items, indicate insertion at the left of the form
    if (this.getItems().length == 0) {
        if (this.shouldPassDropThrough()) {
            this.hideDragLine();
            return;
        }
        
        isc.EditContext.hideAncestorDragDropLines(this);
        this.showDragLineForForm();
        return isc.EH.STOP_BUBBLING;
    }

    var event = this.ns.EH.lastEvent,
        overItem = this.getItemAtPageOffset(event.x, event.y),
        dropItem = this.getNearestItem(event.x, event.y);

    //if (this._lastDragOverItem && this._lastDragOverItem != dropItem) {
        // still over an item but not the same one
    //}

    // We only consider passing the drop through if the cursor is not over an actual item
    if (overItem) {
        isc.EditContext.hideAncestorDragDropLines(this);
        this.showDragLineForItem(dropItem, event.x, event.y);
    } else {
        if (this.shouldPassDropThrough()) {
            this.hideDragLine();
            return;
        }
        if (dropItem) {
            isc.EditContext.hideAncestorDragDropLines(this);
            this.showDragLineForItem(dropItem, event.x, event.y);
        } else {
            this.hideDragLine();
        }
    }

    this._lastDragOverItem = dropItem;

    return isc.EH.STOP_BUBBLING;        
},

editModeDropOut : function () {
    this.hideDragLine();
    return isc.EH.STOP_BUBBLING;        
},

editModeDrop : function () {
    // DataSource is a special case - it's the only non-visual property that users can drag
    // and drop and a position within the form doesn't make sense
    var dropItem = this.ns.EH.getDragTarget().getDragData();
    if (isc.isAn.Array(dropItem)) dropItem = dropItem[0];
    if ((dropItem && (dropItem.type || dropItem.className) == "DataSource") ||
        this.getItems().length == 0)                       // Empty form is also a special case
    {
        if (this.shouldPassDropThrough()) {
            this.hideDragLine();
            return;
        }
        this.itemDrop(this.ns.EH.getDragTarget(), 0, 0, 0);
        return isc.EH.STOP_BUBBLING;
    }

    if (!this._lastDragOverItem) {
        isc.logWarn("lastDragOverItem not set, cannot drop", "dragDrop");
        return;
    }
    
    var item = this._lastDragOverItem,
        dropOffsets = this.getItemTableOffsets(item),
        side = item.dropSide,
        index = item._dragItemIndex,
        insertIndex = this.getItemDropIndex(item, side);

    this._lastDragOverItem = null;
    if (this.shouldPassDropThrough()) {
        this.hideDragLine();
        return;
    }

    if (insertIndex != null && insertIndex >= 0) {

        if (this.parentElement) {
            if (this.parentElement.hideDragLine) this.parentElement.hideDragLine();
            if (this.parentElement.hideDropLine) this.parentElement.hideDropLine();
        }
    
		// Note that we cache a copy of _rowTable because the modifyFormOnDrop() method may
		// end up invalidating the table layout, and thus clearing _rowTable in the middle of
        // its processing
        var rowTable = this.items._rowTable.duplicate();
        this.modifyFormOnDrop(item, dropOffsets.top, dropOffsets.left, side, rowTable);
    }

    this.hideDragLine();
    return isc.EH.STOP_BUBBLING;        
},

itemDrop : function (item, itemIndex, rowNum, colNum, side, callback) {
        
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

    if (!item.isA("Palette")) {
        if (isc.EditContext._dragHandle) isc.EditContext._dragHandle.hide();
        var tree = this.editContext.getEditNodeTree(),
            oldParent = tree.getParent(source.editNode),
            oldIndex = tree.getChildren(oldParent).indexOf(source.editNode),
            editNode = source.editNode;
        
        if (isc.isA.Function(this.itemDropping)) {
            editNode = this.itemDropping(editNode, itemIndex, true);
            if (!editNode) return;
        }

        this.editContext.removeNode(editNode);
        
        // If we've moved the child component to a slot further down in the same parent, 
        // indices will now be off by one because we've just removed it from its old slot
        if (oldParent == this.editNode && itemIndex > oldIndex) itemIndex--;

        var node = this.editContext.addNode(source.editNode, this.editNode, itemIndex);
        if (node && node.liveObject) {
            isc.EditContext.delayCall("selectCanvasOrFormItem", [node.liveObject, true], 200);
        }
        
        return node;
    } else {
        // We're dealing with a drag of a new item from a component palette
        var paletteNode = item.transferDragData();
        if (isc.isAn.Array(paletteNode)) paletteNode = paletteNode[0];
        
        // loadData() operates asynchronously, so we'll have to finish the item drop off-thread
        if (paletteNode.loadData && !paletteNode.isLoaded) {
            var thisForm = this;
            paletteNode.loadData(paletteNode, function (loadedNode) {
                loadedNode = loadedNode || paletteNode
                loadedNode.isLoaded = true;
                thisForm.completeItemDrop(loadedNode, itemIndex, rowNum, colNum, side, callback)
                loadedNode.dropped = paletteNode.dropped;
            });
            return;
        }

        this.completeItemDrop(paletteNode, itemIndex, rowNum, colNum, side, callback)
    }
},

completeItemDrop : function (paletteNode, itemIndex, rowNum, colNum, side, callback) {

    var liveObject = paletteNode.liveObject,
        canvasEditNode;
    if (!isc.isA.FormItem(liveObject)) {
        if (isc.isA.Button(liveObject) || isc.isAn.IButton(liveObject)) {
            // Special case - Buttons become ButtonItems
            paletteNode = this.editContext.makeEditNode({
                type: "ButtonItem", 
                title: liveObject.title,
                defaults : paletteNode.defaults
            })
        } else if (isc.isA.Canvas(liveObject)) {
            canvasEditNode = paletteNode;
            paletteNode = this.editContext.makeEditNode({type: "CanvasItem"});
            isc.addProperties(paletteNode.defaults, {
                showTitle: false,
                startRow: true,
                endRow: true,
                width: "*",
                colSpan: "*"
            });
        }
    }
    paletteNode.dropped = true;
    
    if (isc.isA.Function(this.itemDropping)) {
        paletteNode = this.itemDropping(paletteNode, itemIndex, true);
        if (!paletteNode) return;
    }

    var nodeAdded = this.editContext.addNode(paletteNode, this.editNode, itemIndex);
    
    if (nodeAdded) {

        isc.EditContext.clearSchemaProperties(nodeAdded);
    
        if (canvasEditNode) {
            nodeAdded = this.editContext.addNode(canvasEditNode, nodeAdded, 0);
            

            // FIXME: Need a cleaner factoring here (see also Layout.dropItem())
            if (isc.isA.TabSet(liveObject)) {
                
                liveObject.delayCall("showAddTabEditor", [], 1000);
            }
        }
        
        // If we've just dropped a palette node that contained a reference to a dataSource,
        // do a forced set of that dataSource on the liveObject.  This will take it through
        // any special editMode steps - for example, it will cause a DynamicForm to have a 
        // set of fields generated for it and added to the project tree
        if (nodeAdded.liveObject.dataSource) {
            //this.logWarn("calling setDataSource on: " + nodeAdded.liveObject);
            nodeAdded.liveObject.setDataSource(nodeAdded.liveObject.dataSource, null, true);
        }
        
        isc.EditContext.delayCall("selectCanvasOrFormItem", [paletteNode.liveObject, true], 200);
        
        if (nodeAdded.showTitle != false) {
            paletteNode.liveObject.delayCall("editClick");
        }
    }
    if (callback) this.fireCallback(callback, "node", [nodeAdded]);

},

// Modifies the form to accommodate the pending drop by adding columns and/or SpacerItems as 
// necessary, then performs the actual drop
modifyFormOnDrop : function (item, rowNum, colNum, side, rowTable) {
    if (this.canAddColumns == false) return;
    
    var dropItem = this.ns.EH.getDragTarget().getDragData(),
    dropItemCols,
    draggingFromRow,
    draggingFromIndex,
    _this = this;
    
    if (!dropItem) {
        // We're drag-positioning an existing item
        dropItem = this.ns.EH.getDragTarget();
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
                if (this.items[lastIndex] == dropItem) {
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
            dropItem = this.createItem(dropItem, type);
        } else {
            // This is not completely accurate, but it gives us enough info for placement and 
            // column occupancy calculation.  dropItem() differentiates between Buttons and 
            // other types of Canvas, but for our purposes here it's enough to know that non-
            // FormItem items will occupy one cell and don't have endRow/startRow set
            dropItem = this.createItem({type: "CanvasItem", showTitle: false}, "CanvasItem");
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
                if (this.items[lastIndex] == dropItem) continue; 
                if (isc.isA.SpacerItem(this.items[lastIndex]) && 
                    this.items[lastIndex]._generatedByBuilder)
                {
                    this.logDebug("Marking spacer " + this.items[lastIndex].name + " for removal", 
                                  "formItemDragDrop");
                    spacersToDelete.add(this.items[lastIndex]);
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
                var existingItem = this.items[insertIndex];
                
                // If the item being dropped upon is not a spacer, check the item immediately 
                // adjacent on the side of the drop
                if (!isc.isA.SpacerItem(existingItem) || !existingItem._generatedByBuilder) {
                    insertIndex += side =="L" ? -1 : 1;
                    existingItem = this.items[insertIndex];
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
            } else if (rowTable[rowNum].length + dropItemCols <= this.numCols) {
                addColumns = false;
            } else  {
                // Otherwise widen the entire form
                this.editContext.setNodeProperties(this.editNode, {numCols: this.numCols + cols});
            }
        }
        
        // We're inserting a whole new column to the "grid" that the user sees.  This may not
        // be the desired action - maybe the user just wanted to insert an extra cell in this
        // row?  Leaving as is for now - prompting the user would make this and everything 
        // downstream of it asynchronous
        for (var i = 0; i < rowTable.length; i++) {
            var insertIndex = rowTable[i][colNum];
            if (insertIndex == null) insertIndex = this.items.length;
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
                    var existingItem = this.items[insertIndex - 1];
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
                
                var paletteNode = this.editContext.makeEditNode({type: "SpacerItem"}); 
                isc.addProperties(paletteNode.defaults, {
                    colSpan: cols, 
                    height: 0,
                    _generatedByBuilder: true
                });
                var nodeAdded = this.editContext.addNode(paletteNode, this.editNode,
                                                         insertIndex);
                // Keep track of how many new items we've added to the form, because we need 
                // to step the insert point on for any later adds
                delta++;
            } else {
                if (side == "L") {
                    // We're dropping to the left of an item, so we know there is an item to 
                    // our right.  If it specifies startRow, clear that out
                    var existingItem = this.items[insertIndex];
                    if (existingItem && existingItem.startRow && existingItem._startRowSetByBuilder) {
                        existingItem.editContext.setNodeProperties(existingItem.editNode, 
                            {startRow: null, _startRowSetByBuilder: null});
                    }
                } else {
                    // We're dropping to the right of an item, so we know there is an item to 
                    // our left.  If it specifies endRow, clear that out
                    var existingItem = this.items[insertIndex - 1];
                    if (existingItem && existingItem.endRow && existingItem._endRowSetByBuilder) {
                        existingItem.editContext.setNodeProperties(existingItem.editNode, 
                            {endRow: null, _endRowSetByBuilder: null});
                    }
                }
                
                this.itemDrop(this.ns.EH.getDragTarget(), insertIndex, i, colNum, side, 
                    function (node) {
                        _this._nodeToSelect = node;
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
        if (row >= rowTable.length) rowStartIndex = this.items.length;
        else rowStartIndex = rowTable[row][0];
        
        var currentItem = currentItemIndex == null ? null : this.items[currentItemIndex];
        if (currentItem == null || 
                (isc.isA.SpacerItem(currentItem) && currentItem._generatedByBuilder)) {
            if (row > rowTable.length - 1 || row < 0) {
                // Dropping past the end or before the beginning of the form - in both cases 
                // rowStartIndex will already have been set correctly, so we can just go 
                // ahead and add the component, plus any spacers we need
                if (colNum != 0 && !dropItem.startRow) {
                    var paletteNode = this.editContext.makeEditNode({type: "SpacerItem"});
                    isc.addProperties(paletteNode.defaults, {
                        colSpan: colNum, 
                        height: 0,
                        _generatedByBuilder : true
                    });
                    this.editContext.addNode(paletteNode, this.editNode, rowStartIndex);
                }
                this.itemDrop(this.ns.EH.getDragTarget(), 
                                rowStartIndex + (colNum != 0 ? 1 : 0), row, colNum, side, 
                                function (node) {
                                    _this._nodeToSelect = node;
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
                var existingItem = this.items[existingItemIndex];
                if (existingItem == null) {
                    isc.logWarn("Null item in DynamicForm at position (" + row + "," + (colNum-1) + ")");
                    return;
                }
                // Special case - don't remove the endRow flag from the existing item if the 
                // existing item is also the item we're dropping (as would be the case if the 
                // if the user piacks up a field and drops it further to the right in the 
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
                    var paletteNode = this.editContext.makeEditNode({type: "SpacerItem"});
                    isc.addProperties(paletteNode.defaults, {
                        colSpan: padding, 
                        height: 0,
                        _generatedByBuilder: true
                    });
                    this.editContext.addNode(paletteNode, this.editNode, existingItemIndex + 1);
                }
                this.itemDrop(this.ns.EH.getDragTarget(), 
                                existingItemIndex + (padding > 0 ? 2 : 1), row, colNum, side, 
                                function (node) {
                                    _this._nodeToSelect = node;
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
                    this.itemDrop(this.ns.EH.getDragTarget(), currentItemIndex, row, 
                                  colNum, side, 
                                  function (node) {
                                      _this._nodeToSelect = node;
                                  });
                } else {
                    this.itemDrop(this.ns.EH.getDragTarget(), currentItemIndex, row, 
                                  colNum, side, 
                                  function (node) {
                                      _this._nodeToSelect = node;
                                  });
                    currentItem.editContext.removeNode(currentItem.editNode);
                }
            }
        } else {
            // Something is in the way.  We could either insert an entire new row or just push
            // the contents of this one column down a row.  Both of these seem like valid use
            // cases; for now, we're just going with inserting a whole new row
            if (colNum != 0) {
                var paletteNode = this.editContext.makeEditNode({type: "SpacerItem"}); 
                isc.addProperties(paletteNode.defaults, {
                    colSpan: colNum, 
                    height: 0,
                    _generatedByBuilder : true
                });
                this.editContext.addNode(paletteNode, this.editNode, rowStartIndex);
            }
            this.itemDrop(this.ns.EH.getDragTarget(), rowStartIndex + (colNum == 0 ? 0 : 1), 
                row, colNum, side, function (node) {
                    if (node && node.liveObject && node.liveObject.editContext) {
                        node.liveObject.editContext.setNodeProperties(node, 
                                    {endRow: true, _endRowSetByBuilder: true});
                    }
                    _this._nodeToSelect = node;
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

    if (this._nodeToSelect && this._nodeToSelect.liveObject) {
        isc.EditContext.delayCall("selectCanvasOrFormItem", [this._nodeToSelect.liveObject], 200);
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

// Override of Canvas.canAdd - DynamicForm will accept a drop of a Canvas in addition to the
// FormItems advertised in its schema
canAdd : function (type) {
    if (this.getObjectField(type) != null) return true;
    var classObject = isc.ClassFactory.getClass(type);
    if (classObject && classObject.isA("Canvas")) return true;
    return false;
},

setEditorType : function (item, editorType) {

    if (!item.editContext) return;

    var tree = item.editContext.getEditNodeTree(),
        parent = tree.getParent(item.editNode),
        index = tree.getChildren(parent).indexOf(item.editNode),
        ctx = item.editContext,
        paletteNode = { type: editorType, defaults: item.editNode.defaults }, 
        editNode = ctx.makeEditNode(paletteNode);
        
    ctx.removeNode(item.editNode);
    ctx.addNode(editNode, parent, index);
},

// This undocumented method is called from DF.itemDrop() just before the editNode is   
// inserted into the editContext.  This function should return the editNode to actually
// insert - either the passed node if no change is required, or some new value.  Note that 
// the "isAdded" parameter will be false if the item was dropped after being dragged from 
// elsewhere, as opposed to a drop of a new item from a component palette
itemDropping : function (editNode, insertIndex, isAdded) {
    
    var item = editNode.liveObject,
        schemaInfo = isc.EditContext.getSchemaInfo(editNode);
    
    // Case 0: there is no schema information to compare, so nothing to do
    if (!schemaInfo.dataSource) return editNode;

    // Case 1: this is an unbound (so presumably empty) form.  Bind it to the top-level 
    // schema associated with this item
    if (!this.dataSource) {
        this.setDataSource(schemaInfo.dataSource);
        this.serviceNamespace = schemaInfo.serviceNamespace;
        this.serviceName = schemaInfo.serviceName;
        return editNode;
    }
    
    // Case 2: this form is already bound to the top-level schema associated with this item,
    // so we don't need to do anything
    if (schemaInfo.dataSource == isc.DataSource.getDataSource(this.dataSource).ID &&
        schemaInfo.serviceNamespace == this.serviceNamespace && 
        schemaInfo.serviceName == this.serviceName) {
        return editNode;
    }
    
    // Case 3: this form is already bound to some other schema.  We need to wrap this item
    // in its own sub-form
    var canvasItemNode = this.editContext.makeEditNode({
        type: "CanvasItem",
        defaults: {
            cellStyle: "nestedFormContainer"
        }
    });
    isc.addProperties(canvasItemNode.defaults, {showTitle: false, colSpan: 2});
    canvasItemNode.dropped = true;
    this.editContext.addNode(canvasItemNode, this.editNode, insertIndex);
    
    var dfNode = this.editContext.makeEditNode({
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
    this.editContext.addNode(dfNode, canvasItemNode, 0);
    
    var nodeAdded = this.editContext.addNode(editNode, dfNode, 0);
    isc.EditContext.clearSchemaProperties(nodeAdded);
    
},

getFieldEditNode : function (field, dataSource) {
    var editorType = this.getEditorType(field);
    editorType = editorType.substring(0,1).toUpperCase() + editorType.substring(1) + "Item";

    var editNode = {
        type: editorType,
        autoGen: true,
        defaults: {
            name: field.name,
            title: field.title || dataSource.getAutoTitle(field.name)
        }
    }
    
    return editNode;
}


});

// Edit Mode extras for FormItem and its children
// -------------------------------------------------------------------------------------------

isc.FormItem.addMethods({

// Note: this impl contains code duplicated from Canvas.setEditMode because FormItem does not 
// extend Canvas.  
setEditMode : function(editingOn, editContext, editNode) {
    
        if (editingOn == null) editingOn = true;
        if (this.editingOn == editingOn) return;
        this.editingOn = editingOn;

        if (this.editingOn) {
            this.editContext = editContext;
        }

        this.editNode = editNode;

        // If we're going into edit mode, re-route various methods
        if (this.editingOn) {
            this.saveToOriginalValues(["click", "doubleClick", "changed"]);
            this.setProperties({
                click: this.editModeClick,
                doubleClick: this.editModeDoubleClick,
                changed: this.editModeChanged
            });
        } else {
            this.restoreFromOriginalValues(["click", "doubleClick", "changed"]);
        }
},

editModeChanged : function (form, item, value) {
    this.editContext.setNodeProperties(this.editNode, {defaultValue: value});
},

setEditorType : function (editorType) {
    if (this.form) this.form.setEditorType(this, editorType);
}
});

isc.ButtonItem.addMethods({

    editClick : function () {
        var left = this.canvas.getPageLeft(),
            width = this.canvas.getVisibleWidth(),
            top = this.canvas.getPageTop(),
            height = this.canvas.getHeight();
            
        isc.EditContext.manageTitleEditor(this, left, width, top, height);
    }
});

}

// Edit Mode impl for SectionStack
// -------------------------------------------------------------------------------------------

isc.SectionStack.addMethods({

canAdd : function (type) { 
    // SectionStack is a special case for DnD - although it is a VLayout, its schema marks
    // children, peers and members as inapplicable.  However, anything can be put into a 
    // SectionStackSection.  Therefore, we accept drop of any canvas, and handle adding it 
    // to the appropriate section in the drop method.
    // We also accept a drop of a FormItem; this will be detected downstream and handled by
    // wrapping the FormItem inside an auto-created DynamicForm.  Similarly a DrawItem
    // can be accepted because it will be wrapped inside an auto-created DrawPane.
    if (type == "SectionStackSection") return true;
    var classObject = isc.ClassFactory.getClass(type);
    if (classObject &&
        (classObject.isA("Canvas") || classObject.isA("FormItem") || classObject.isA("DrawItem")))
    {
        return true;
    }
    return false;
},

// Return the modified editNode (or a completely different one); return false to abandon 
// the drop
modifyEditNode : function (paletteNode, newEditNode, dropType) {
    if (dropType == "SectionStackSection") return newEditNode;
    var dropPosition = this.getDropPosition();
    if (dropPosition == 0) {
        isc.warn("Cannot drop before the first section header");
        return false;
    }
    
    var headers = this._getHeaderPositions();
    for (var i = headers.length-1; i >= 0; i--) {
        if (dropPosition > headers[i]) {
            // Return the edit node off the section header
            return this.getSectionHeader(i).editNode;
        }
    }
    // Shouldn't ever get here
    return newEditNode;
},

// getEditModeDropPosition() - explicitly called from getDropPosition if the user isn't doing
// a drag reorder of sections.
getEditModeDropPosition : function (dropType) {
    var pos = this.invokeSuper(isc.SectionStack, "getDropPosition");
    if (!dropType || dropType == "SectionStackSection") {
        return pos;
    }
    
    var headers = this._getHeaderPositions();
    for (var i = headers.length-1; i >= 0; i--) {
        if (pos > headers[i]) {
            return pos - headers[i] - 1;
        }
    }

    return 0;
},

_getHeaderPositions : function () {
    var headers = [],
        j = 0;
    for (var i = 0; i < this.getMembers().length; i++) {
        if (this.getMember(i).isA(this.sectionHeaderClass)) {
            headers[j++] = i;
        }
    }
    return headers;
}


}); 


// Edit Mode impl for ListGrid
// -------------------------------------------------------------------------------------------
if (isc.ListGrid != null) {
    
isc.ListGrid.addMethods({

setEditMode : function(editingOn, editContext, editNode) {

    if (editingOn == null) editingOn = true;
    if (editingOn == this.editingOn) return;

    this.invokeSuper(isc.ListGrid, "setEditMode", editingOn, editContext, editNode);

    if (this.editingOn) {
        this.saveToOriginalValues(["setNoDropIndicator", "clearNoDropIndicator", 
                                   "headerClick"]); 
        this.setProperties({
            setNoDropIndicator: this.editModeSetNoDropIndicator,
            clearNoDropIndicator: this.editModeClearNoDropIndicator,
            headerClick: this.editModeHeaderClick
        });
    } else {
        // If we're coming out of edit mode, revert to whatever we saved
        this.restoreFromOriginalValues(["setNoDropIndicator", "clearNoDropIndicator",
                                        "headerClick"]);
    }
},

// Canvas.clearNoDropindicator no-ops if the internal _noDropIndicator flag is null.  This
// isn't good enough in edit mode because a canvas can be dragged over whilst the no-drop
// cursor is showing, and we want to revert to a droppable cursor regardless of whether 
// _noDropIndicatorSet has been set on this particular canvas. 
editModeClearNoDropIndicator : function (type) {
    this.Super("clearNoDropIndicator", arguments);
    this.body.editModeClearNoDropIndicator();
},

// Special editMode version of setNoDropCursor - again, because the base version no-ops in 
// circumstances where we need it to refresh the cursor.
editModeSetNoDropIndicator : function () {
    this.Super("setNoDropIndicator", arguments);
    this.body.editModeSetNoDropIndicator();
},

editModeHeaderClick : function (fieldNum) {
    // Select the corresponding ListGridField
    var tree = this.editContext.getEditNodeTree(),
        children = tree.getChildren(tree.findById(this.ID)),
        field = this.getField(fieldNum),
        node
    ;
    // Note that a non-field object could be a child
    // of the ListGrid node so we cannot just index
    // into the child array by the fieldNum. 
    for (var i = 0; i < children.length; i++) {
        var child = children[i];
        if (child.name == field.name) {
            node = child;
            break;
        }
    }

    if (node) {
        node.liveObject._visualProxy = this.header.getButton(fieldNum);
        isc.EditContext.selectCanvasOrFormItem(node.liveObject);
    }

    this._headerClickFired = true;
    return isc.EH.STOP_BUBBLING;
},

// HACK: We ideally want a header click to stop event bubbling at that point, but it seems 
// that returning STOP_BUBBLING from the headerClick() method does not prevent the ListGrid's
// click event from firing, so the object selection is superseded.  To work around this, we 
// maintain a flag on the LG that headerClick has been fired, which this click() impl tests
// and then clears
editModeClick : function () {
    if (this.editNode) {
        if (this._headerClickFired) delete this._headerClickFired;
        else isc.EditContext.selectCanvasOrFormItem(this, true);
        return isc.EH.STOP_BUBBLING;
    }
}


});

}

// Edit Mode impl for TreeGrid
// -------------------------------------------------------------------------------------------
if (isc.TreeGrid != null) {
    
isc.TreeGrid.addMethods({

    setEditMode : function(editingOn, editContext, editNode) {

        if (editingOn == null) editingOn = true;
        if (editingOn == this.editingOn) return;

        this.invokeSuper(isc.TreeGrid, "setEditMode", editingOn, editContext, editNode);

        if (this.editingOn) {
            // If the TG was not databound and had an empty fieldset at initWidget time, it 
            // will have created a default treeField which now appears in its fields property
            // as if it were put there by user code.  We need to detect this circumstance and
            // create a TreeGridField node in the projectComponents tree so the user can 
            // manipulate this auto-generated field
            this.editModeCreateDefaultTreeFieldEditNode();
            this.saveToOriginalValues(["addField"]); 
            this.setProperties({
                addField: this.editModeAddField
            });
        } else {
            // If we're coming out of edit mode, revert to whatever we saved
            this.restoreFromOriginalValues(["addField"]);
        }
    },
    
    editModeCreateDefaultTreeFieldEditNode : function () {
    
        // If we're loading a view, the default nodeTitle is going to be destroyed before the
        // user sees it, so just bail
        if (isc._loadingNodeTree) return;
        
        // If this TG is databound, we presumably haven't created a default nodeTitle; this 
        // being the case, let's bail now so that we don't remove a real user field just 
        // because it happens to be called "nodeTitle"
        if (this.dataSource) return;
        
        var fields = this.fields;
        for (var i = 0; i < fields.length; i++) {
            if (fields[i].name == "nodeTitle") {
                var config = {
                    type: "TreeGridField",
                    autoGen: true,
                    defaults: {
                        name: fields[i].name,
                        title: fields[i].title
                    }
                };
                var editNode = this.editContext.makeEditNode(config);
                this.editContext.addNode(editNode, this.editNode, null, null, true);
                return;
            }
        }
    },
    
    // Overriding the DBC implementation because we need to treat the field being added as a
    // special case if it has treeField set - there can only be one treeField, so we must 
    // remove the extant one.  This could only really happen during Load View (unless we were
    // to change the default for treeField to true in the component palette), so we will just
    // hand the call on if we're not in loading mode
    editModeAddField : function (field, index) {
        this.Super("addField", arguments);

        if (isc._loadingNodeTree) {
            if (field.treeField) {
                var fields = this.getFields();
                for (var i = 0; i < fields.length; i++) {
                    if (fields[i].name != field.name && fields[i].treeField) {
                        this.removeField(fields[i]);
                        break;
                    }
                }
            }
        }

    },


    // TreeGrid needs a special implementation of this method because binding a TreeGrid really 
    // means binding the one field in the DataSource that represents the tree; with other DBC's,
    // we bind all the visible fields
    editModeSetDataSource : function (dataSource, fields, forceRebind) {
        //this.logWarn("editMode setDataSource called" + isc.Log.getStackTrace());
        
        // _loadingNodeTree is a flag set by Visual Builder - its presence indicates that we are 
        // loading a view from disk.  In this case, we do NOT want to perform the special 
        // processing in this function, otherwise we'll end up with duplicate components in the
        // componentTree.  
        // However, TreeGrid needs special treatment because it auto-creates a treeField if it 
        // is not passed a list of fields to use.  Since we'll be adding the fields one at a 
        // time during View Load, we start out with no fields, so a default will be created.
        // 
        if (isc._loadingNodeTree) {
            this.baseSetDataSource(dataSource, fields);
            return;
        }
        
        if (dataSource == null) return;
        if (dataSource == this.dataSource && !forceRebind) return;
            
        var fields = this.getFields();
        
        // remove just the field currently marked treeField: true - in many use cases, this
        // will be the only field in the TreeGrid anyway
        if (fields) {
            for (var i = 0; i < fields.length; i++) {
                var field = fields[i];
                if (field.treeField) {
                    field.treeField = null;
                    var nodeToRemove = field.editNode;
                    break;
                }
            }
        }
        
        var existingFields = this.getFields();
        existingFields.remove(field);
        
        // If this dataSource has a single complex field, use the schema of that field in lieu
        // of the schema that was dropped.
        var schema,
            fields = dataSource.fields;
        if (fields && isc.getKeys(fields).length == 1 &&
            dataSource.fieldIsComplexType(fields[isc.firstKey(fields)].name))
        {
            schema = dataSource.getSchema(fields[isc.firstKey(fields)].type);
        } else {
            schema = dataSource;
        }
        
            
        // add one editNode for the single field in the DataSource that is named as the 
        // "titleField"; if there is no such field, just use the first

        var fields = schema.getFields(),
            titleFieldName = dataSource.titleField;
        
        if (!isc.isAn.Array(fields)) fields = isc.getValues(fields);
        
        for (var ix = 0; ix < fields.length; ix++) {
            if (!this.shouldUseField(fields[ix], dataSource)) continue;
            if (titleFieldName == null || titleFieldName == fields[ix].name) {
                var titleField = fields[ix];
                break;
            }
        }

        if (titleField) existingFields.addAt(titleField, 0);
        
        this.baseSetDataSource(dataSource, existingFields);
        
        var fieldConfig = this.getFieldEditNode(titleField, schema);
        fieldConfig.defaults.treeField = true;
        var editNode = this.editContext.makeEditNode(fieldConfig);
        this.editContext.addNode(editNode, this.editNode, 0, null);
        // Deferred node removal to here as it avoids leaving the TG with an empty fieldset,
        // because this situation triggers the creation of a default treeField in various 
        // places in the TG code
        if (nodeToRemove) this.editContext.removeNode(nodeToRemove, true);
        //this.logWarn("editMode setDataSource done adding fields");
    }


});

}

// Edit Mode impl for ServiceOperation and ValuesMap.  Both of these are non-visual classes
// that can nevertheless appear in a VB app - kind of like DataSources, but they're added to
// the project as a side effect of adding a web service binding.
// -------------------------------------------------------------------------------------------

var basicSetEditMode = function (editingOn, editContext, editNode) {
        if (editingOn == null) editingOn = true;
        if (this.editingOn == editingOn) return;
        this.editingOn = editingOn;

        if (this.editingOn) this.editContext = editContext;
        
        this.editNode = editNode;
}

isc.ServiceOperation.addMethods({
    setEditMode : basicSetEditMode,
    getActionTargetTitle : function () {
        return "Operation: [" + this.operationName + "]";
    }
});

if (isc.ValuesManager != null) {
    isc.ValuesManager.addMethods({
        setEditMode : basicSetEditMode
    });
}
        

// EditNode
// ---------------------------------------------------------------------------------------

//> @object EditNode
// An object representing a component that is currently being edited within an
// +link{EditContext}.
// <P>
// An EditNode is essentially a copy of a +link{PaletteNode}, initially with the same properties
// as the PaletteNode from which it was generated.  However unlike a PaletteNode, an EditNode 
// always has a +link{editNode.liveObject,liveObject} - the object created from the 
// +link{paletteNode.defaults} or other properties defined on a paletteNode.
// <P>
// Like a Palette, an EditContext may use properties such as +link{paletteNode.icon} or 
// +link{paletteNode.title} to display EditNodes.
// <P>
// An EditContext generally offers some means of editing EditNodes and, as edits are made,
// updates +link{editNode.defaults} with the information required to re-create the component.
// 
// @inheritsFrom PaletteNode
// @treeLocation Client Reference/Tools
// @visibility external
//<

//> @attr editNode.defaults (Properties : null : IR)
// Properties required to recreate the current +link{editNode.liveObject}.
// @visibility external
//<

//> @attr editNode.type (SCClassName : null : IR)
// +link{SCClassName} of the <smartclient>+link{liveObject}</smartclient>
// <smartgwt>+link{canvasLiveObject}</smartgwt>, for example, "ListGrid".
// @visibility external
//<

//> @attr editNode.liveObject (Object : null : IR)
// Live version of the object created from the +link{editNode.defaults}.  For example, 
// if +link{editNode.type} is "ListGrid", <code>liveObject</code> would be a ListGrid.
// @visibility external
//<


//> @attr editNode.editDataSource (DataSource : null : IR)
// DataSource to use when editing the properties of this component.  Defaults to
// +link{editContext.dataSource}, or the DataSource named after the component's type.
//
// @visibility internal
//<


// EditContext
// --------------------------------------------------------------------------------------------

//> @interface EditContext
// An EditContext provides an editing environment for a set of components.
// <P>
// An EditContext is typically populated by adding a series of +link{EditNode,EditNodes} created via a
// +link{Palette}, either via drag and drop creation, or when loading from a saved version,
// via +link{EditContext.addFromPaletteNode(),addFromPaletteNode()} or 
// +link{EditContext.addPaletteNodesFromXML(),addPaletteNodesFromXML()}.
// <P>
// An EditContext then provides interfaces for further editing of the components represented
// by EditNodes.  
// 
// @group devTools
// @treeLocation Client Reference/Tools
// @visibility external
//<
isc.ClassFactory.defineInterface("EditContext");



//> @attr EditContext.editDataSource   (DataSource : null : IR)
// Default DataSource to use when editing any component in this context.  Defaults to the
// DataSource named after the component's type.  Can be overridden per-component via
// +link{editedItem.editDataSource}.
//
// @group devTools
//<

isc.EditContext.addClassProperties({
_dragHandleHeight: 18,
_dragHandleWidth: 18,
_dragHandleXOffset: -18,
_dragHandleYOffset: 0 
});

isc.EditContext.addClassMethods({

    // Title Editing (for various components: buttons, tabs, etc)
    // ---------------------------------------------------------------------------------------
    manageTitleEditor : function (targetComponent, left, width, top, height) {
        if (!isc.isA.DynamicForm(this.titleEditor)) {
            this.titleEditor = isc.DynamicForm.create({
                autoDraw: false,
                margin: 0, padding: 0, cellPadding: 0,
                fields: [
                    { 
                        name: "title", type: "text", 
                        showTitle: false,
                        keyPress : function (item, form, keyName) {
                            if (keyName == "Escape") {
                                form.discardUpdate = true;
                                form.hide();
                                return
                            }
                            if (keyName == "Enter") item.blurItem();
                        }, 
                        blur : function (form, item) {
                            // WWW this.logWarn("Blurring...");
                            //this.logWarn(this.getStackTrace());
                            if (!form.discardUpdate) {
                                var widget = form.targetComponent,
                                ctx = widget.editContext;
                                if (ctx) {
                                    ctx.setNodeProperties(widget.editNode, 
                                                           {"title" : item.getValue()});
                                    ctx.nodeClick(ctx, widget.editNode);
                                }
                            }
                            form.hide();
                        }
                    }
                ]
            });
        }
        
        var editor = this.titleEditor;
        editor.setProperties({targetComponent: targetComponent});
        editor.discardUpdate = false;
        
        var item = editor.getItem("title");
        var title = targetComponent.title;
        if (!title) {
            title = targetComponent.name;
        }
        item.setValue(title);

        this.positionTitleEditor(targetComponent, left, width, top, height);
        
        editor.show();
        item.focusInItem();
        item.delayCall("selectValue", [], 100);
        // WWW this.logWarn("Showing editor...");
    },
    
    positionTitleEditor : function (targetComponent, left, width, top, height) {
        if (top == null) top = targetComponent.getPageTop();
        if (height == null) height = targetComponent.height;
        if (left == null) left = targetComponent.getPageLeft(); 
        if (width == null) width = targetComponent.getVisibleWidth();

        var editor = this.titleEditor;
        var item = editor.getItem("title");
        item.setHeight(height);
        item.setWidth(width);

        editor.setTop(top);
        editor.setLeft(left);
    },

    // Selection and Dragging of EditNodes
    // ---------------------------------------------------------------------------------------
    
    deselect : function () {
        isc.SelectionOutline.deselect();
        this.hideDragHandle();
    },
  
    setEditMode : function (editingOn) {
        var selectedComponent = isc.SelectionOutline.getSelectedObject();
        if (selectedComponent == null) return;
        
        if (editingOn) {
            this.setupDragProperties(selectedComponent);
            this.showSelectedObjectDragHandle();
            isc.SelectionOutline.showOutline();
        } else {
            this.resetDragProperties(selectedComponent);
            this.hideDragHandle();
            isc.SelectionOutline.hideOutline();
        }
    },
    
    // In editMode, we allow dragging the selected canvas using the drag-handle
    // This involves overriding some default behaviors at the widget level.
    setupDragProperties : function (component) {
    
        if (component.saveToOriginalValues) {
            component.saveToOriginalValues([
                                            "canDrag", 
                                            "canDrop",
                                            "dragAppearance",
                                            "dragStart",
                                            "dragMove",
                                            "dragStop",
                                            "setDragTracker"
                                            ]);
        }
        
        if (component.setProperties) {
            component.setProperties({
                canDrop: true,
                dragAppearance: "outline",
                // These method overrides are to clobber special record-based drag handling
                // implemented by ListGrid and its children
                dragStart : function () { return true; },
                dragMove : function () { return true; },
                setDragTracker : function () {isc.EH.setDragTracker(""); return false; },
                dragStop : function () {
                    isc.EditContext.hideProxyCanvas();
                    isc.EditContext.positionDragHandle();
                }
            });
        }
        
    },
    resetDragProperties : function (component) {
        
        if (this.observer) this.observer.ignore(component, "dragMove");
        component.restoreFromOriginalValues([
            "canDrag", 
            "canDrop",
            "dragAppearance",
            "dragStart",
            "dragMove",
            "dragStop",
            "setDragTracker"
        ]);
    },
    
    selectCanvasOrFormItem : function (object, hideLabel) {
    
        // Make sure we're not being asked to select a non-visual object like a DataSource 
        // or ServiceOperation.  We also support the idea of a visual proxy for a non-widget
        // object - for example, ListGridFields are represented visually by the corresponding
        // button in the ListGrid header.
        if (!isc.isA.Canvas(object) && !isc.isA.FormItem(object) && !object._visualProxy) {
            return;
        }
        // Or a Menu (ie, a context menu which has no visibility until an appropriate object 
        // is right-clicked by the user)
        if (isc.isA.Menu(object)) {
            return;
        }
    
        if (this._dragHandle) this._dragHandle.hide();
        
        var selectedObject = isc.SelectionOutline.getSelectedObject();
        if (selectedObject) this.resetDragProperties(selectedObject);
        
        var underlyingObject,
            overrideLabel;
        if (object._visualProxy) {
            var type = object.type || object._constructor;
            overrideLabel = "[" + type + " " + (object.name ? "name:" : "ID");
            overrideLabel += object.name || object.ID;
            overrideLabel += "]"
            underlyingObject = object;
            object = object._visualProxy;
        }
        
        var editContext = underlyingObject ? underlyingObject.editContext : object.editContext;
        if (!editContext) return;
        
        // If parent component is a H/VLayout or Stack configure the highlight to
        // allow resizing of the component from along the length axis.
        var node = object.editNode,
            parentNode = editContext.getEditNodeTree().getParent(node),
            resizeFrom
        ;
        if (parentNode) {
            var parentLiveObject = parentNode.liveObject;
            if (parentLiveObject) {
                if (isc.isA.Layout(parentLiveObject)) {
                    var vertical = parentLiveObject.vertical,
                        fill = ((vertical ? parentLiveObject.vPolicy : parentLiveObject.hPolicy) == isc.Layout.FILL),
                        childCount = parentLiveObject.getMembers().length,
                        objectIndex = parentLiveObject.getMemberNumber(object),
                        lastMember = (objectIndex == (childCount-1)),
                        canResize = (!fill || !lastMember)
                    ;
                    if (canResize) {
                        resizeFrom = (vertical ? "B" : "R");
                    }
                }
            }
        }

        var vb = editContext.creator;
        isc.SelectionOutline.select(object, false, 
                                    !(hideLabel && vb && vb.hideLabelWhenSelecting),
                                    overrideLabel,
                                    resizeFrom);
        
        // For conceptual objects that needed a visual proxy, now we've done the physical 
        // on-screen selection we need to flip the object back to the underlying one
        if (underlyingObject) object = underlyingObject;
        
        if (object.editingOn) {
            this.setupDragProperties(object);
            this.showSelectedObjectDragHandle();

            var ctx = object.editContext;

            if (ctx.selectRecord) {
                ctx.deselectAllRecords();
                var ctxData = ctx.getEditNodeTree();
                if (isc.isA.Canvas(object)) {
                    // Canvas objects are created with reasonably friendly IDs that appear
                    // as visual identifiers in the component tree
                    // (Except for section header objects, that is...)
                    if (isc.isA.SectionHeader(object) || isc.isA.ImgSectionHeader(object)) {
                        ctx.selectRecord(ctxData.findById(object._ID));
                    } else {
                        ctx.selectRecord(ctxData.findById(object.ID));
                    }
                } else {
                    // FormItems have standard system-assigned IDs like isc_TextItem_1234.
                    // They are identified in the component tree by name.
                    ctx.selectRecord(ctxData.find({ID: object.name}));
                }
            }
            if (ctx.creator && ctx.creator.editComponent) ctx.creator.editComponent(object.editNode, object);
        }
    },
    
    showSelectedObjectDragHandle : function () {
        if (!this._dragHandle) {
            var _this = this;
            this._dragHandle = isc.Img.create({
                src: "[SKIN]/../../ToolSkin/images/controls/dragHandle.gif",
                prompt:"Grab here to drag component",
                width: this._dragHandleWidth, height: this._dragHandleHeight,
                cursor:"move",
                backgroundColor:"white",
                opacity: 80,
                canDrag: true,
                canDrop: true,
                isMouseTransparent: true,
                mouseDown : function () {
                    // Remember the offset from the top-left corner of the target widget when
                    // a drag starts (OK, this is mouseDown, but all drags start from the 
                    // co-ords of the most recent mouseDown, so it works)
                    this.dragIconOffsetX = isc.EH.getX() - 
                                              isc.EditContext.draggingObject.getPageLeft();
                    this.dragIconOffsetY = isc.EH.getY() - 
                                              isc.EditContext.draggingObject.getPageTop();
                    _this._mouseDown = true;
                    this.Super("mouseDown", arguments);
                },
                mouseUp : function () {
                    _this._mouseDown = false;
                }
            });
        }
        if (this.draggingObject) {
            this.observer.ignore(this.draggingObject, "dragMove");
            this.observer.ignore(this.draggingObject, "dragStop");
            this.observer.ignore(this.draggingObject, "hide");
            this.observer.ignore(this.draggingObject, "destroy"); 
        }
        
        var dragTarget = isc.SelectionOutline.getSelectedObject();
        if (isc.isA.FormItem(dragTarget)) {
            // dragTarget must be a canvas, so wrap the formItem in a proxy canvas
            if (!this._dragTargetProxy) {
                this._dragTargetProxy = isc.FormItemProxyCanvas.create();
            }
            this._dragTargetProxy.delayCall("setFormItem", [dragTarget]);
            dragTarget = this._dragTargetProxy;
        }
        
        this._dragHandle.setProperties({dragTarget: dragTarget});
        isc.Timer.setTimeout("isc.EditContext.positionDragHandle()", 0);

        if (!this.observer) this.observer = isc.Class.create();
        
        this.draggingObject = dragTarget;
        this.observer.observe(this.draggingObject, "dragMove",
                    "isc.EditContext.positionDragHandle(true)");
        this.observer.observe(this.draggingObject, "dragStop",
                    "isc.EditContext._mouseDown = false");
        this.observer.observe(this.draggingObject, "hide",
                    "isc.EditContext._dragHandle.hide()");
        this.observer.observe(this.draggingObject, "destroy",
                    "isc.EditContext._dragHandle.hide()");
        this._dragHandle.show();
    },
    
    hideProxyCanvas : function () {
        if (this._dragTargetProxy) this._dragTargetProxy.hide();
    },
    
    positionDragHandle : function (offset) {
        if (!this._dragHandle) return;

        var selected = this.draggingObject;
        
        if (selected.destroyed || selected.destroying) {
            this.logWarn("target of dragHandle: " + isc.Log.echo(selected) + " is invalid: " + 
                         selected.destroyed ? "already destroyed" 
                                            : "currently in destroy()");
            return;
        }    
    
        var height = selected.getVisibleHeight();
        if (height < this._dragHandleHeight * 2) {
            // Center the drag handle next to the item (the -1 makes it look slightly more 
            // correct, because the image has a completely white line 1px thick at the top,
            // giving the impression on a white background that it's lower down than it 
            // actually is)
            this._dragHandleYOffset = Math.round((height - this._dragHandle.height) / 2) - 1;
        } else {
            // Place the drag handle at the top-left corner of a taller item
            this._dragHandleYOffset = -1;
        }

        if (selected.isA("FormItemProxyCanvas") && !this._mouseDown) {
            selected.syncWithFormItemPosition();
        }
        if (!selected) return;
        var left = selected.getPageLeft() + this._dragHandleXOffset;
        if (offset) {
            left += selected.getOffsetX() - this._dragHandle.dragIconOffsetX;
        }
        this._dragHandle.setPageLeft(left);

        var top = selected.getPageTop() + this._dragHandleYOffset;
        if (offset) {
            top += selected.getOffsetY() - this._dragHandle.dragIconOffsetY;
        }
        this._dragHandle.setPageTop(top);

        this._dragHandle.bringToFront();
    },
    
    hideDragHandle : function () {
        if (this._dragHandle) this._dragHandle.hide();
    },
    
    showDragHandle : function () {
        if (this._dragHandle) this._dragHandle.show();
    },
    
    hideAncestorDragDropLines : function (object) {
        while (object && object.parentElement) {
            if (object.parentElement.hideDragLine) object.parentElement.hideDragLine();
            if (object.parentElement.hideDropLine) object.parentElement.hideDropLine();
            object = object.parentElement;
            if (isc.isA.FormItem(object)) object = object.form;
        }
    },
    
    getSchemaInfo : function (editNode) {
        var schemaInfo = {},
            liveObject = editNode.liveObject;
            
        if (!liveObject) return schemaInfo;
            
        if (isc.isA.FormItem(liveObject)) {
            if (liveObject.form && liveObject.form.dataSource) {
                var form = liveObject.form;
                schemaInfo.dataSource = isc.DataSource.getDataSource(form.dataSource).ID;
                schemaInfo.serviceName = form.serviceName;
                schemaInfo.serviceNamespace = form.serviceNamespace;
            } else {
                schemaInfo.dataSource = liveObject.schemaDataSource;
                schemaInfo.serviceName = liveObject.serviceName;
                schemaInfo.serviceNamespace = liveObject.serviceNamespace;
            }
        } else if (isc.isA.Canvas(liveObject)) {
                schemaInfo.dataSource = isc.DataSource.getDataSource(liveObject.dataSource).ID;
                schemaInfo.serviceName = liveObject.serviceName;
                schemaInfo.serviceNamespace = liveObject.serviceNamespace;
        } else {
            // If it's not a FormItem or a Canvas, then we must presume it's a config object.
            // This can happen on drop of new components
            schemaInfo.dataSource = liveObject.schemaDataSource;
            schemaInfo.serviceName = liveObject.serviceName;
            schemaInfo.serviceNamespace = liveObject.serviceNamespace;
        }
        
        return schemaInfo;
    },

    clearSchemaProperties : function (node) {
        if (node && node.defaults && isc.isA.FormItem(node.liveObject)) {
            delete node.defaults.schemaDataSource;
            delete node.defaults.serviceName;
            delete node.defaults.serviceNamespace;
            var form = node.liveObject.form;
            if (form && form.inputSchemaDataSource &&
                isc.DataSource.get(form.inputSchemaDataSource).ID == node.defaults.inputSchemaDataSource &&
                form.inputServiceName == node.defaults.inputServiceName &&
                form.inputServiceNamespace == node.defaults.inputServiceNamespace)
            {
                delete node.defaults.inputSchemaDataSource;
                delete node.defaults.inputServiceName;
                delete node.defaults.inputServiceNamespace;
            }
        }
    },

    // XML source code generation
    // ---------------------------------------------------------------------------------------

    // serialize a set of component definitions to XML code, that is, essentially the
    // editNode.defaults portion ( { _constructor:"Something", prop1:value, ... } )
    serializeDefaults : function (defaults) {
        if (defaults == null) return null;
    
        if (!isc.isAn.Array(defaults)) defaults = [defaults];

        var output = isc.SB.create();

        isc.Comm.omitXSI = true;
        for (var i = 0; i < defaults.length; i++) {
            var obj = defaults[i],
                tagName = obj._tagName,
                schema = isc.DS.getNearestSchema(obj);

            // The tag name outputted by the XML serialization will be tagName, if set.
            // Otherwise it will be the tag name implied by the schema.
            // Note that this effectively reserves the attribute name "_tagName".

            output.append(schema.xmlSerialize(obj, null, null, tagName), "\n\n");
        }
        isc.Comm.omitXSI = null;

        return output.toString();
    },

    convertActions : function (node, defaults, classObj) { 
        // Convert actions defined as a raw object to StringMethods so they can be
        // serialized correctly.
        
        // This is a bit of a pain to achieve as there's nothing in the component's defaults 
        // that makes it clear that this is a StringMethod object rather than some other 
        // simple object and there are no dataSource field definitions for most stringMethods
        // - We could examine the registered stringMethod for the class, but this wouldn't
        //   work for non instance object fields, such as stringMethods on ListGridFields
        // - We could just examine the object - if it's a valid format (has target, name attrs)
        //   we could assume it's an action - but this would catch false positives in some 
        //   cases
        // For now - look at the value on the live-instance and see if it's a function produced
        // from an Action (check for function.iscAction).
        // This will work as long as the live-object has actually been instantiated (may not be
        // a valid test in all cases - EG anything that's lazily created on draw or when called
        // may not yet have converted it to a function).
        
        for (var field in defaults) {
            var value = defaults[field];
            // if it's not an object or is already a StringMethod no need to convert to one
            if (!isc.isAn.Object(value) || isc.isA.StringMethod(value)) continue;
            
            // If it has a specified field-type, other than StringMethod - we don't need 
            // to convert
            // Note: type Action doesn't need conversion to a StringMethod as when we serialize
            // to XML, the ActionDataSource will do the right thing
            var fieldType;
            if (classObj && classObj.getField) fieldType = classObj.getField(field).type;
            if (fieldType && (fieldType != "StringMethod")) continue;
            
            var liveValue = node.liveObject[field],
                liveAction = liveValue ? liveValue.iscAction : null,
                convertToSM
            ;
            if (liveAction) convertToSM = true;
            /*
            // We could add a sanity check that the value will convert to a function successfully
            // in case a function has been added since init or something.
            try {
                isc.Func.expressionToFunction("", defaults[field]);
            } catch (e) {
                convertToSM = false;
            }
            */
            if (convertToSM) defaults[field] = isc.StringMethod.create({value:value});
        }
        // no need to return anything we've modified the defaults object directly.
    }
});


isc.EditContext.addInterfaceProperties({
    //> @attr editContext.defaultPalette (Palette : null : IRW)
    // +link{Palette} to use when an +link{EditNode} is being created directly by this EditContext,
    // instead of being created due to a user interaction with a palette (eg dragging from
    // a +link{TreePalette}, or clicking on +link{MenuPalette}).
    // <P>
    // If no defaultPalette is provided, the EditContext uses an automatically created
    // +link{HiddenPalette}.
    //
    // @visibility external
    //<
    // defaultPalette: null,

    //> @method editContext.getDefaultPalette()
    // @include editContext.defaultPalette
    // @return (Palette) the default Palette
    // @visibility external
    //<
    getDefaultPalette : function () {
        if (this.defaultPalette) return this.defaultPalette;
        return (this.defaultPalette = isc.HiddenPalette.create());
    },

    //> @method editContext.setDefaultPalette()
    // @include editContext.defaultPalette
    // @param palette (Palette) the default Palette
    // @visibility external
    //<
    setDefaultPalette : function (palette) {
        this.defaultPalette = palette;
        
        // If the palette has no defaultEditContext, then set it
        if (palette && !palette.defaultEditContext) {
            palette.defaultEditContext = this;
        }
    },

    //> @attr editContext.extraPalettes (Array of Palette : null : IRW)
    // Additional +link{Palette,Palettes} to consult for metadata when
    // deserializing +link{EditNode,Edit Nodes}. Note that the
    // +link{defaultPalette,defaultPalette} is always consulted and need not be
    // provided again here.
    //
    // @visibility external
    //<
    // extraPalettes: null,
    
    //> @attr EditContext.persistCoordinates (boolean : true : IRW)
    // If enabled, changes to a +link{EditNode.liveObject,liveObject}'s position
    // and size will be persisted to their +link{EditNode,EditNodes}.  This
    // applies to both programmatic calls and user interaction (drag reposition
    // or drag resize).
    // 
    // @visibility external
    //<
    persistCoordinates: true,

    // Finds a palette node in the defaultPalette or other palettes provided
    findPaletteNode : function (fieldName, value) {
        // Try the default palette first
        var paletteNode = this.getDefaultPalette().findPaletteNode(fieldName, value);
        if (paletteNode) return paletteNode;

        if (this.extraPalettes) {
            if (!isc.isAn.Array(this.extraPalettes)) this.extraPalettes = [this.extraPalettes];

            // If not found, try any other palettes provided
            for (var i = 0; i < this.extraPalettes.length; i++) {
                paletteNode = this.extraPalettes[i].findPaletteNode(fieldName, value);
                if (paletteNode) return paletteNode;
            }
        }

        // If not found anywhere, return null
        return null;
    },

    //> @method editContext.addNode()
    // Add a new +link{EditNode} to the EditContext, under the specified parent.
    // <P>
    // The EditContext will interrogate the parent and new nodes to determine what field 
    // within the parent allows a child of this type, and to find a method to add the newNode's 
    // liveObject to the parentNode's liveObject.  The new relationship will then be stored
    // in the tree of EditNodes.
    // <P>
    // For example, when a Tab is dropped on a TabSet, the field TabSet.tabs is discovered as
    // the correct target field via naming conventions, and the method TabSet.addTab() is likewise 
    // discovered as the correct method to add a Tab to a TabSet.
    //
    // @param newNode (EditNode) new node to be added
    // @param [parentNode] (EditNode) parent to add the new node under.
    // @param [index] (integer) index within the parent's children array
    // @param [parentProperty] (string) the property of the liveParent to which the new node should
    //                                  be added, if not auto-discoverable from the schema
    // @param [skipParentComponentAdd] (Boolean) whether to skip adding the liveObject to the liveParent
    //                                           (default false)
    // @return newNode (EditNode) node added
    // @visibility external
    //<
    addNode : function (newNode, parentNode, index, parentProperty, skipParentComponentAdd) {
    	var iscClass = isc.ClassFactory.getClass(newNode.type);
    	if (iscClass && iscClass.isA(isc.DataSource)) {
            // If we're adding a datasource that must be loaded, then defer the addNode
            // until the datasource is loaded
            if (newNode.loadData && !newNode.isLoaded) {
    	        var self = this;
                newNode.loadData(newNode, function () {
                    self.addNode(newNode, parentNode, index, parentProperty, skipParentComponentAdd);
                });
                return;
            }
        }

        var data = this.getEditNodeTree();

        if (parentNode == null) parentNode = this.getDefaultParent(newNode);

        var liveParent = this.getLiveObject(parentNode);
        this.logInfo("addNode will add newNode of type: " + newNode.type +
                     " to: " + this.echoLeaf(liveParent), "editing");

        if (liveParent.wrapChildNode) {
            parentNode = liveParent.wrapChildNode(this, newNode, parentNode, index);
            if (!parentNode) return;
            liveParent = this.getLiveObject(parentNode);
        }

        // find what field in the parent can accommodate a child of this type (prefer the 
        // passed-in name over a looked-up one, so the user can override in the case of 
        // multiple valid parent fields)
        var fieldName = parentProperty || isc.DS.getObjectField(liveParent, newNode.type);
        var field = isc.DS.getSchemaField(liveParent, fieldName);

        if (!field) {
            this.logWarn("can't addNode: can't find a field in parent: " + liveParent +
                         " for a new child of type: " + newNode.type + ", parent property:" + fieldName + 
                         ", newNode is: " +
                         this.echo(newNode));
            return;
        }

        // for a singular field (eg listGrid.dataSource), remove the old node first
        if (!field.multiple) {
            var existingChild = isc.DS.getChildObject(liveParent, newNode.type, parentProperty);
            if (existingChild) {
                var existingChildNode = data.getChildren(parentNode).find("ID", isc.DS.getAutoId(existingChild));
                this.logWarn("destroying existing child: " + this.echoLeaf(existingChild) +
                             " in singular field: " + fieldName);
                data.remove(existingChildNode);
                if (isc.isA.Class(existingChild) && !isc.isA.DataSource(existingChild)) existingChild.destroy();
            }
        }

        // NOTE: generated components and remove/add cycles: some widgets convert config
        // objects into live objects (eg formItem properties to live FormItem, tab -> ImgTab,
        // section -> SectionHeader, etc).  When we are doing an add/remove cycle for these
        // kinds of generated objects:
        // - rebuild based on defaults, rather than trying to re-add the liveObject, which will
        //   be a generated component that the parent will have destroyed
        // - preserve Canvas children of the generated component, such as tab.pane,
        //   section.items, which have not been added to the defaults.  We do this by using
        //   part of the serialization logic (serializeChildData)
        // - ensure removal of the tab, item, or section does not destroy these Canvas children
        //   (a special flag is passed to at least TabSets to avoid this)

        // Optimization for add/remove cycles: check for methods like "reorderMember" first.
        // Note this doesn't remove the complexity discussed above because a generated
        // component might be moved between two parents.
        var childObject;
        if (newNode.generatedType) {
            // copy to avoid property scribbling that is currently done by TabSets and
            // SectionStacks at least
            childObject = isc.addProperties({}, newNode.defaults);
            this.serializeChildData(childObject, data.getChildren(newNode));
        } else {
            childObject = newNode.liveObject;
        }

        // Let the liveObject know about the editContext and editNode. We used
        // to do this for some objects in addedToEditContext, but that isn't
        // sufficient for liveObjects that are in fact config blocks (since they
        // don't have the callback).
        childObject.editContext = this;
        childObject.editNode = newNode;

        if (!skipParentComponentAdd) {
            var result = isc.DS.addChildObject(liveParent, newNode.type, childObject, index, parentProperty);
            if (!result) {
                this.logWarn("addChildObject failed, returning");
                return;
            }
        }

        // fetch the liveObject back from the parent to handle it's possible conversion from
        // just properties to a live instance.
        // NOTE: fetch object by ID, not index, since on a reorder when a node is dropped after
        // itself the index is one too high
        if (!newNode.liveObject) newNode.liveObject = isc.DS.getChildObject(liveParent, newNode.type, 
                                                                            isc.DS.getAutoId(newNode.defaults), parentProperty);

        this.logDebug("for new node: " + this.echoLeaf(newNode) + 
                      " liveObject is now: " + this.echoLeaf(newNode.liveObject),
                      "editing");

        if (newNode.liveObject == null) {
            this.logWarn("wasn't able to retrieve live object after adding node of type: " +
                         newNode.type + " to liveParent: " + liveParent + 
                         ", does liveParent have an appropriate getter() method?");
        }

        // add the node representing the component to the project tree
        data.add(newNode, parentNode, index);
        // gets rid of the spurious opener icon that appears because all nodes are regarded as
        // folders and dropped node is unloaded, hence might have children
        data.openFolder(newNode);

        this.logInfo("added node " + this.echoLeaf(newNode) + 
                     " to EditTree at path: " + data.getPath(newNode) + 
                     " with live object: " + this.echoLeaf(newNode.liveObject), "editing");
    
        // Call hook in case the EditContext wants to do further processing ... useful to avoid
        // problem with calling Super with an interface method
        if (this.nodeAdded) this.nodeAdded(newNode);

        // Call hook in case the live object wants to know about being added
        if (newNode.liveObject.addedToEditContext) newNode.liveObject.addedToEditContext(this, newNode, parentNode, index);
        
        return newNode;    
    },

    //>!BackCompat 2011.06.25
    addComponent : function (newNode, parentNode, index, parentProperty, skipParentComponentAdd) {
        return this.addNode(newNode, parentNode, index, parentProperty, skipParentComponentAdd);
    },
    //<!BackCompat

    //> @method editContext.reorderNode()
    // Moves an +link{EditNode} from one child index to another in the EditContext under the specified parent.
    // <P>
    // No changes are made to the live objects.
    //
    // @param parentNode (EditNode) parent to reorder child nodes
    // @param index (integer) index within the parent's children array to be moved
    // @param moveToIndex (integer) index within the parent's children array at which to place moved node
    // @visibility devTools
    //<
    reorderNode : function (parentNode, index, moveToIndex) {
        var data = this.getEditNodeTree();

        // Locate child node that has moved
        var childNode = data.getChildren(parentNode).get(index);

        // Remove the child node from the tree and insert it back at the new location
        data.remove(childNode);
        data.add(childNode, parentNode, moveToIndex);
    },

    //> @method editContext.nodeAdded()
    // Notification fired when an +link{EditNode} has been added to the EditContext
    //
    // @param newNode (EditNode) node that was added
    // @visibility external
    //<
    // Empty function in case someone wants to observe.
    nodeAdded : function (newNode) {},

    getDefaultParent : isc.ClassFactory.TARGET_IMPLEMENTS,

    //> @method editContext.addFromPaletteNode()
    // Creates a new EditNode from a PaletteNode, using the
    // +link{defaultPalette}.  If you have an array of possibly inter-related
    // PaletteNodes, then you should use
    // +link{addFromPaletteNodes(),addFromPaletteNodes()} on the array instead,
    // in order to preserve the relationships.
    //
    // @param paletteNode (PaletteNode) the palette node to use to create the new node
    // @param [parentNode] (EditNode) optional the parent node if the new node should appear
    //                                under a specific parent
    // @return (EditNode) the EditNode created from the paletteNode
    // @see addFromPaletteNodes()
    // @visibility external
    //< 
    addFromPaletteNode : function (paletteNode, parentNode) {
        
        var editNode = this.makeEditNode(paletteNode, parentNode);
        return this.addNode(editNode, parentNode);    
    },
    
    //> @method editContext.makeEditNode()
    // Creates and returns an EditNode using the +link{defaultPalette}.  Does not add the newly
    // created EditNode to the EditContext.
    // 
    // @param paletteNode (PaletteNode) the palette node to use to create the new node
    // @return (EditNode) the EditNode created from the paletteNode
    // @visibility external
    //<
    makeEditNode : function (paletteNode) {
        var palette = this.getDefaultPalette();
        return palette.makeEditNode(paletteNode);
    },

    // alternative to just using node.liveObject
    // exists because forms used to rebuild *all* items when any single item is added, hence
    // making the liveObject stale for siblings of an added item
    getLiveObject : function (node) {
        var data = this.getEditNodeTree();
        var parentNode = data.getParent(node);

        // at root, just use the cached liveObject (a formItem can never be at root)
        if (parentNode == null) return node.liveObject; 

        
        var liveParent = parentNode.liveObject;
        var liveObject = isc.DS.getChildObject(liveParent, node.type, isc.DS.getAutoId(node));
                                  
        if (liveObject) node.liveObject = liveObject;
        return node.liveObject;
    },

    // wizard handling
    requestLiveObject : function (newNode, callback, palette) {
        var _this = this;

        // handle deferred nodes (don't load or create their liveObject until they are actually
        // added).  NOTE: arguably the palette should handle this, and makeEditNode()
        // should be asynchronous in this case.
        if (newNode.loadData && !newNode.isLoaded) {
            newNode.loadData(newNode, function (loadedNode) {
                loadedNode = loadedNode || newNode
                loadedNode.isLoaded = true;
                // preserve the "dropped" flag
                loadedNode.dropped = newNode.dropped;
                _this.fireCallback(callback, "node", [loadedNode]);
            }, palette);
            return;
        }

        if (newNode.wizardConstructor) {
            this.logInfo("creating wizard with constructor: " + newNode.wizardConstructor);
            var wizard = isc.ClassFactory.newInstance(newNode.wizardConstructor,
                                                      newNode.wizardDefaults);
            // ask the wizard to go through whatever steps 
            wizard.getResults(newNode, function (results) {
                // accept either a paletteNode or editNode (detect via liveObject)
                if (!results.liveObject) {
                    results = palette.makeEditNode(results);
                }
                _this.fireCallback(callback, "node", [results]);
            }, palette);
            return;
        }

        this.fireCallback(callback, "node", [newNode]);
    },

    // Gets the tree of editNodes being edited by this editContext. We create the tree
    // here, since the interface can't do it in initWidget.
    getEditNodeTree : function () {
        if (!this.editNodeTree) {
            // NOTE: there really is no reasonable default for rootComponent, since its type
            // determines what can be dropped.  This default will create a tree that won't accept
            // any drops, but won't JS error
            var rootComponent = this.rootComponent || { _constructor: "Object" }, 
                rootType = isc.isA.Class(rootComponent) ? rootComponent.Class :
                                                          rootComponent._constructor,
                rootLiveObject = this.rootLiveObject || rootComponent;

            var rootNode = {
                type: rootType,
                _constructor: rootType,
                defaults : rootComponent,
                liveObject: rootLiveObject
            };
            
            this.editNodeTree = isc.Tree.create({
                idField:"ID",
                root : rootNode,
                // HACK: so that all nodes can be targetted for D&D
                isFolder : function () { return true; }
            });
        }

        return this.editNodeTree;
    },

    getEditNodeArray : function () {
        return this.getEditNodeTree().getAllNodes();
    },

    //>!BackCompat 2011.06.25 
    getEditComponents : function () {
        return this.getEditNodeArray();
    },
    //<!BackCompat

    // tests whether the targetNode can accept a newNode of type "type"
    canAddToParent : function (targetNode, type) {
        var liveObject = targetNode.liveObject;
        if (isc.isA.Class(liveObject)) {
            return (liveObject.getObjectField(type) != null);
        }
        // still required for MenuItems and ListGridFields, where the live object is not a Class
        return (isc.DS.getObjectField(targetNode, type) != null);
    },

    //> @method EditContext.removeAll()
    // Removes all +link{EditNode,EditNodes} from the EditContext, but does not destroy 
    // the +link{EditNode.liveObject,liveObjects}.
    // @visibility external
    //<
    removeAll : function () {
        var data = this.getEditNodeTree();
        var rootChildren = data.getChildren(data.getRoot()).duplicate();
        for (var i = 0; i < rootChildren.length; i++) {
            this.removeNode(rootChildren[i]);
        }
    },

    //> @method EditContext.destroyAll()
    // Removes all +link{EditNode,EditNodes} from the EditContext, and calls
    // +link{Canvas.destroy(),destroy()} on the
    // +link{EditNode.liveObject,liveObjects}.
    // @visibility external
    //<
    destroyAll : function () {
        var data = this.getEditNodeTree();
        var rootChildren = data.getChildren(data.getRoot()).duplicate();
        for (var i = 0; i < rootChildren.length; i++) {
            this.destroyNode(rootChildren[i]);
        }
    },

    // remove an editNode from the tree
    removeNode : function (editNode, skipLiveRemoval) {
        var data = this.getEditNodeTree();
        

        // remove the corresponding component from the object model
        var parentNode = data.getParent(editNode);
        var liveChild = this.getLiveObject(editNode);
        var liveParent = this.getLiveObject(parentNode);

        // remove the node from the tree
        data.remove(editNode);

        if (skipLiveRemoval) return;
        
        if (liveParent && liveChild) {

            //this.logWarn("removing with defaults: " + this.echo(editNode.defaults));

            isc.DS.removeChildObject(liveParent, editNode.type, liveChild);
        }
    },

    //>!BackCompat 2011.06.25
    removeComponent : function (editNode, skipLiveRemoval) { // old name
        return this.removeNode(editNode, skipLiveRemoval);
    },
    //<!BackCompat

    // destroy an editNode in the tree, including it's liveObject
    destroyNode : function (editNode) {
        var liveObject = this.getLiveObject(editNode);
        this.removeNode(editNode);
        // if it has a destroy function, call it.  Otherwise we assume garbage collection will
        // work
        if (liveObject.destroy) liveObject.destroy();
    },
    
    //>!BackCompat 2011.06.25
    destroyComponent : function (editNode) { // old name
        return this.destroyNode(editNode);
    },
    //<!BackCompat

    // EditFields : optional lists of fields that can be edited in an EditContext
    // ---------------------------------------------------------------------------------------

    getEditDataSource : function (canvas) {
        return isc.DataSource.getDataSource(canvas.editDataSource || canvas.Class || 
                                            this.editDataSource);
    },

    // fields to edit:
    // - application-specific: two different editing applications may edit the same type of
    //   component (eg a ListViewer) exposing different sets of properties
    //   - the DataSource may not even represent the full set of properties, but regardless,
    //     can act as a default list of fields and reference properties for those fields
    // - on an application-specific basis, should be able to have a base set of fields, plus
    //   additions
    
    // get list of editable fields for a component.  May be a mix of string field names and
    // field objects
    _getEditFields : function (canvas) {
        // combine the baseEditFields and editFields properties
        var fields = [];
        fields.addList(canvas.baseEditFields);
        
        fields.addList(canvas.editFields);
 
        // HACK: set any explicitly specified fields to be visible, since many fields in the
        // current widget DataSources are set to visible=false to suppress them in editing
        // demos.  If a field is explicitly specified in editFields, we want it to be shown
        // unless they've set "visible" explicitly
        for (var i = 0; i < fields.length; i++) {
            var field = fields[i];
            if (field.visible == null) field.visible = true;
        }

        // if this is an empty list, take all the fields from the DataSource
        if (fields.length == 0) {
            fields = this.getEditDataSource(canvas).getFields();
            fields = isc.getValues(fields);
        }
        return fields;
    },
    
    // get the list of editable fields as an Array of Strings
    getEditFieldsList : function (canvas) {
        var fieldList = [],
            fields = this._getEditFields(canvas);
        // return just the name for any fields specified as objects
        for (var i = 0; i < fields.length; i++) {
            var field = fields[i];
            if (isc.isAn.Object(field)) {
                fieldList.add(field.name);
            } else {
                fieldList.add(field);
            }
        }
        return fieldList;
    },

    // get the edit fields, suitable for passing as "fields" to a dataBinding-aware component
    getEditFields : function (canvas) {
        var fields = this._getEditFields(canvas);
        // make any string fields into objects
        for (var i = 0; i < fields.length; i++) {
            var field = fields[i];
            if (isc.isA.String(field)) field = {name:field};
            // same hack as above to ensure visibility of explicitly specified fields, for
            // fields specified as just Strings
            if (field.visible == null) field.visible = true;
            fields[i] = field;
        }

        return fields;
    },

    // Serializing
    // --------------------------------------------------------------------------------------------
    // Take a tree of editNodes and produce a data structure that can be serialized to produce
    // actual XML or JSON source code

    
    
    //>!BackCompat 2013.09.27
    serializeComponents : function (serverless, includeRoot) {
        return this.serializeAllEditNodes(serverless, includeRoot);
    },
    //<!BackCompat

    //> @method editContext.serializeAllEditNodes()
    // Serialize the tree of +link{EditNode,EditNodes} to an XML representation
    // of +link{PaletteNode,PaletteNodes}. The result can be supplied to 
    // +link{addPaletteNodesFromXML(),addPaletteNodesFromXML()} to recreate
    // the EditNodes.
    //
    // @param [serverless] (Boolean) If true, specify DataSources in full rather than
    //                               assuming they can be downloaded from the server.
    //                               Defaults to false.
    // @return (String) an XML representation of PaletteNodes which can be used to
    //                  recreate the tree of EditNodes.
    // @see addPaletteNodesFromXML
    // @visibility external
    //<
    serializeAllEditNodes : function (serverless, includeRoot) {
        // we flatten the Tree of objects into a flat list of top-level items
        // to serialize.  Nesting (eg grid within Layout) is accomplished by
        // having the Layout refer to the grid's ID.
        var data = this.getEditNodeTree();
        var nodes = includeRoot ? [data.root] : data.getChildren(data.root).duplicate();
        return this.serializeEditNodes(nodes, serverless);
    },

    //> @method editContext.serializeAllEditNodesAsJSON()
    // Encode the tree of +link{EditNode,EditNodes} to a JSON representation
    // of +link{PaletteNode,PaletteNodes}. The result can be supplied to 
    // +link{addPaletteNodesFromJSON(),addPaletteNodesFromJSON()} to recreate
    // the EditNodes.
    //
    // @param [serverless] (Boolean) If true, specify DataSources in full rather than
    //                               assuming they can be downloaded from the server.
    //                               Defaults to false.
    // @return (String) a JSON representation of PaletteNodes which can be used to
    //                  recreate the tree of EditNodes.
    // @see addPaletteNodesFromJSON
    // @visibility external
    //<
    serializeAllEditNodesAsJSON : function (serverless, includeRoot) {
        // we flatten the Tree of objects into a flat list of top-level items
        // to serialize.  Nesting (eg grid within Layout) is accomplished by
        // having the Layout refer to the grid's ID.
        var data = this.getEditNodeTree();
        var nodes = includeRoot ? [data.root] : data.getChildren(data.root).duplicate();
        return this.serializeEditNodesAsJSON(nodes, serverless);
    },


    //> @method editContext.serializeEditNodes()
    // Serialize the provided +link{EditNode,EditNodes} to an XML
    // representation of +link{PaletteNode,PaletteNodes}. Note that the
    // EditNodes must have been added to this EditContext. The result can be
    // supplied to +link{addPaletteNodesFromXML(),addPaletteNodesFromXML()} to
    // recreate the EditNodes.
    //
    // @param nodes (Array of EditNode) EditNodes to be serialized 
    // @param [serverless] (Boolean) If true, specify DataSources in full rather than
    //                               assuming they can be downloaded from the server.
    //                               Defaults to false.
    // @return (String) an XML representtion of the provided EditNodes
    // @visibility external
    //<
    // NOTE: the "nodes" passed to this function need to be part of the Tree that's available
    // as this.getEditNodeTree().  TODO: generalized this so that it takes a Tree, optional nodes, and
    // various mode flags like serverless.
    serializeEditNodes : function (nodes, serverless) {
        if (!nodes) return null;

        return this._serializeEditNodes(nodes, serverless);
    },

    //> @method editContext.serializeEditNodesAsJSON()
    // Serialize the provided +link{EditNode,EditNodes} to a JSON
    // representation of +link{PaletteNode,PaletteNodes}. Note that the
    // EditNodes must have been added to this EditContext. The result can be
    // supplied to +link{addPaletteNodesFromJSON(),addPaletteNodesFromJSON()} to
    // recreate the EditNodes.
    //
    // @param nodes (Array of EditNode) EditNodes to be serialized 
    // @param [serverless] (Boolean) If true, specify DataSources in full rather than
    //                               assuming they can be downloaded from the server.
    //                               Defaults to false.
    // @return (String) a JSON representtion of the provided EditNodes
    // @visibility external
    //<
    serializeEditNodesAsJSON : function (nodes, serverless) {
        if (!nodes) return null;

        return this._serializeEditNodes(nodes, serverless, "json");
    },

    _serializeEditNodes : function (nodes, serverless, format) {        
        if (!isc.isAn.Array(nodes)) nodes = [nodes];

        // add autoDraw to all non-hidden top-level components
        for (var i = 0; i < nodes.length; i++) {
            var node = nodes[i] = isc.addProperties({}, nodes[i]),
                iscClass = isc.ClassFactory.getClass(node.type),
                defaults = node.defaults = isc.addProperties({}, node.defaults);
    
            //this.logWarn("considering node: " + this.echo(topNode) +
            //             " with defaults: " + this.echo(defaults));
            if (iscClass && iscClass.isA("Canvas") && defaults && 
                defaults.visibility != isc.Canvas.HIDDEN && defaults.autoDraw !== false) 
            {
                defaults.autoDraw = true;
            }
        }

        // if serverless is set we will actually output DataSources in their entirety.
        // Otherwise, we'll just output a special tag that causes the DataSource to be loaded
        // as the server processes the XML format.
        this.serverless = serverless;

        this.defaultsBlocks = [];
        this.map("getSerializeableTree", nodes);

        this.serverless = null;

        var result = (format == "json" ? isc.JSON.encode(this.defaultsBlocks) : isc.EditContext.serializeDefaults(this.defaultsBlocks));

        return result;
    },

    // arrange the initialization data into a structure suitable for XML serialization.  This
    // will:
    // - grab just the defaults portion of each editNode (what we serialize)
    // - flatten hierarchies: all Canvas-derived components will be at top-level,
    //   members/children arrays will contain references to these children
    // - ensure DataSources are only listed once since multiple components may refer to the
    //   same DataSource
    getSerializeableTree : function (node, dontAddGlobally) {
        // Give the liveObject a chance to update the editNode
        var liveObject = node.liveObject;
        if (liveObject && liveObject.updateEditNode && liveObject.editContext && liveObject.editNode) {
            node.liveObject.updateEditNode(node.liveObject.editContext, node.liveObject.editNode);
        }
        
        var type = node.type,
            // copy defaults for possible modification
            defaults = isc.addProperties({}, node.defaults);   
        // if this node is a DataSource (or subclass of DataSource)
        var classObj = isc.ClassFactory.getClass(type);

        this.logInfo("node: " + this.echoLeaf(node) + " with type: " + type);

        if (classObj && classObj.isA("DataSource") && !classObj.isA("MockDataSource")) {
            // check for this same DataSource already being saved out
            if (this.defaultsBlocks) {
                var existingDS = this.defaultsBlocks.find("ID", defaults.ID) ||
                                 this.defaultsBlocks.find("loadID", defaults.ID);
                if (existingDS && existingDS.$schemaId == "DataSource") return;
            }

            if (!this.serverless) {
                // when serializing a DataSource, just output the loadID tag so that the
                // server outputs the full definition during XML processing on JSP load
                defaults = {
                    _constructor: "DataSource",
                    $schemaId: "DataSource",
                    loadID: defaults.ID
                };
            } else {
                // if running serverless, we can't rely on the server to fetch the definition
                // as part of XML processing during JSP load, so we have to write out a full
                // definition.  This works only for DataSources that don't require the server
                // to fetch and update data.
                // NOTE: since all DataSources in Visual Builder are always saved to the
                // server, an alternative approach would be to load the DataSource and capture
                // its defaults, as we do when we edit an existing DataSource.  However we
                // would still depend on getSerializeableFields() being correct, as we also use
                // it to obtain clean data when we begin editing a dynamically created
                // DataSource obtained from XML Schema (eg SFDataSource)
                var liveDS = node.liveObject;
                defaults = liveDS.getSerializeableFields();
                defaults._constructor = liveDS.Class;
                defaults.$schemaId = "DataSource";
            }
        }
        
        // Actions
        // By default these will be defined as simple objects in JS, but for saving in XML 
        // we need to enclose them in <Action>...</Action> tags
        // (ensures that any specified mappings are rendered out as an array)
        // Catch these cases and store as a StringMethod object rather than the raw action
        // object - this will serialize correctly.
        isc.EditContext.convertActions(node, defaults, classObj);
        
        var treeChildren = this.getEditNodeTree().getChildren(node);
        if (!treeChildren) {
            if (this.defaultsBlocks) this.defaultsBlocks.add(defaults); // add as a top-level node
            return;
        }

        this.serializeChildData(defaults, treeChildren);
            
        // if we're not supposed to be global, return out defaults
        if (dontAddGlobally) return defaults;
        // otherwise add this node's data globally (we list top-most parents last)
        if (this.defaultsBlocks) this.defaultsBlocks.add(defaults);
    },

    //>!BackCompat 2013.09.25
    addChildData : function (parentData, childNodes) {
        return this.serializeChildData(parentData, childNodes);
    },
    //<!BackCompat

    serializeChildData : function (parentData, childNodes) {
        var ds = isc.DS.get(parentData._constructor);
        for (var i = 0; i < childNodes.length; i++) {
            var child = childNodes[i],
                childType = child.defaults._constructor,
                // copy defaults for possible modification
                childData = isc.addProperties({}, child.defaults),
                parentFieldName = childData.parentProperty || ds.getObjectField(childType),
                parentField = ds.getField(parentFieldName);

            this.logInfo("serializing: child of type: " + childType + 
                         " goes in parent field: " + parentFieldName,
                         "editing");

            // all Canvii output individually, and their parents just output the Canvas ID.
            // NOTE: don't do this for _generated components, which include TabSet tabs and
            // SectionStack sections.
            if ((isc.isA.Canvas(child.liveObject) && !child.liveObject._generated) || 
                isc.isA.DataSource(child.liveObject)) 
            {
                if (isc.isA.DataSource(child.liveObject) && parentFieldName == "dataSource") {
                    // Don't add the "ref:" if the parentFieldName is "dataSource", since
                    // the dataSource field always takes a String ID. (The "ref:" used
                    // to be stripped off later, so just don't add it).
                    childData = childData.ID;
                } else {
                    childData = "ref:" + childData.ID;
                }
                this.getSerializeableTree(child);
            } else {
                // otherwise, serialize this child without adding it globally
                childData = this.getSerializeableTree(child, true);
            }

            var existingValue = parentData[parentFieldName];
            if (parentField.multiple) {
                // force multiple fields to Arrays
                if (!existingValue) existingValue = parentData[parentFieldName] = [];
                existingValue.add(childData);
            } else {
                parentData[parentFieldName] = childData;
            }
        }
    },
   
    //>!BackCompat 2013.09.25
    serializeEditComponents : function () {
        return this.serializeLiveObjects();
    },
    //<!BackCompat

    // get serializable data as an Array of Objects for the editNodes in this context, via
    // getting properties from the liveObjects and stripping it down to editFields (fields that
    // are allowed to be edited in the context), or the DataSource fields if no editFields were
    // declared.
    
    serializeLiveObjects : function () {
        // get all the widgets being edited
        var widgets = this.getEditNodeArray(),
            output = [];

        if (!widgets) return [];

        for (var i = 0; i < widgets.length; i++) {
            var child = widgets[i].liveObject,
                // get all properties that don't have default value
                props = child.getUniqueProperties(),
                editFields = this.getEditFieldsList(child);

            // add in the Class, which will be needed to recreate the widget, but which could never
            // have non-default value
            props._constructor = child.Class;

            // limit the data to just the fields listed in the DataSource
            props = isc.applyMask(props, editFields);
            
            output.add(props);
        } 
        return output;
    },

    //>!BackCompat 2013.09.25
    loadNodeTreeFromXML : function (xmlString) {
        this.addPaletteNodesFromXML(xmlString);
    },
    //<!BackCompat 2013.09.25

    //> @method editContext.addPaletteNodesFromXML()
    // Recreate +link{EditNode,EditNodes} from an XML representation of 
    // +link{PaletteNode,PaletteNodes} (possibly created by calling
    // +link{serializeAllEditNodes()} or +link{serializeEditNodes()}.
    // <P>
    // By default, components that have +link{Canvas.ID,global IDs} will not
    // actually be allowed to take those global IDs - instead, only widgets that have one of the
    // global IDs passed as the <code>globals</code> parameter will actually receive their global
    // IDs.  To override this behavior, pass the special value +link{RPCManager.ALL_GLOBALS}
    // for the <code>globals</code> parameter.
    //
    // @param xmlString (String) XML string
    // @param [parentNode] (EditNode) parent node (defaults to the root)
    // @param [globals] (Array of String) widgets to allow to take their global IDs
    // @see serializeAllEditNodes()
    // @see serializeEditNodes()
    // @visibility external
    //<
    addPaletteNodesFromXML : function (xmlString, parentNode, globals, callback) {
        var self = this;

        //isc.logWarn(isc.echo(xmlString));
        
        isc.DMI.callBuiltin({
            methodName: "xmlToJS",
            arguments: [xmlString],
            callback: function (rpcResponse) {
                self.addPaletteNodesFromJS(rpcResponse.data, parentNode, globals, callback);
            }
        });
    },

    //> @method editContext.addPaletteNodesFromJSON()
    // Recreate +link{EditNode,EditNodes} from a JSON representation of 
    // +link{PaletteNode,PaletteNodes} (possibly created by calling
    // +link{serializeAllEditNodesAsJSON()} or +link{serializeEditNodesAsJSON()}.
    // <P>
    // By default, components that have +link{Canvas.ID,global IDs} will not
    // actually be allowed to take those global IDs - instead, only widgets that have one of the
    // global IDs passed as the <code>globals</code> parameter will actually receive their global
    // IDs.  To override this behavior, pass the special value +link{RPCManager.ALL_GLOBALS}
    // for the <code>globals</code> parameter.
    //
    // @param jsonString (String) JSON string
    // @param [parentNode] (EditNode) parent node (defaults to the root)
    // @param [globals] (Array of String) widgets to allow to take their global IDs
    // @see serializeAllEditNodesAsJSON()
    // @see serializeEditNodesAsJSON()
    // @visibility external
    //<
    addPaletteNodesFromJSON : function (jsonString, parentNode, globals, callback) {
        if (globals == null) globals = [];
        else if (!isc.isAn.Array(globals)) globals = [globals];

        isc.captureDefaults = true;

        var jsClassDefs = isc.JSON.decode(jsonString);

        
        var keepAllGlobals = (globals.length == 1 && globals[0] == isc.RPC.ALL_GLOBALS);
        for (var i = 0; i < jsClassDefs.length; i++) {
            var def = jsClassDefs[i],
                className = def._constructor
            ;
            this._replaceRefs(def, globals);
            if (className) {
                delete def._constructor;
                if (def.ID && !keepAllGlobals && !globals.contains(def.ID)) def.ID = "_" + def.ID;
                var instance = isc.ClassFactory.newInstance(className, def);
            }
        }

        isc.captureDefaults = null;
        var capturedComponents = this.getCapturedComponents();

        // Remove IDs that represent globals that should not be kept
        this._removeIDs(capturedComponents, globals);

        this.addFromPaletteNodes(capturedComponents, parentNode);
        this.fireCallback(callback, ["paletteNodes"], [capturedComponents]);
    },

    // Replace values of type "ref:<ID>" with actual instance
    _replaceRefs : function (def, keepGlobals) {
        var keepAllGlobals = (keepGlobals.length == 1 && keepGlobals[0] == isc.RPC.ALL_GLOBALS);

        for (var key in def) {
            var value = def[key];
            if (isc.isAn.Array(value)) {
                for (var i = 0; i < value.length; i++) {
                    if (isc.isA.String(value[i]) && value[i].startsWith("ref:")) {
                        var ref = value[i].replace("ref:", "");
                        if (!keepAllGlobals && !keepGlobals.contains(ref)) ref = "_" + ref;
                        value[i] = window[ref];
                    } else if (isc.isAn.Object(value[i])) {
                        this._replaceRefs(value[i], keepGlobals);
                    }
                }
            } else if (isc.isAn.Object(value)) {
                this._replaceRefs(value, keepGlobals);
            }
        }
    },

    // Remove ID attributes whose value is not listed in keepGlobals
    _removeIDs : function (def, keepGlobals) {
        var keepAllGlobals = (keepGlobals.length == 1 && keepGlobals[0] == isc.RPC.ALL_GLOBALS);

        if (def.ID && !keepAllGlobals && !keepGlobals.contains(def.ID)) delete def.ID;

        for (var key in def) {
            var value = def[key];
            if (isc.isAn.Array(value)) {
                for (var i = 0; i < value.length; i++) {
                    if (isc.isAn.Object(value[i])) {
                        this._removeIDs(value[i], keepGlobals);
                    }
                }
            } else if (isc.isAn.Object(value)) {
                this._removeIDs(value, keepGlobals);
            }
        }
    },

    //> @method Callbacks.PaletteNodeCallback
    // Callback fired with the +link{PaletteNode,PaletteNodes} obtained asynchronously.
    // @param paletteNodes (Array of PaletteNode) an array of PaletteNodes
    // @visibility external
    //<

    //> @method editContext.getPaletteNodesFromXML()
    // Obtain +link{PaletteNode,PaletteNodes} from an XML representation,
    // but do not add them to the EditContext.
    //
    // @param xmlString (String) XML string
    // @param callback (PaletteNodeCallback) Callback used to return the PaletteNodes
    // @see Callbacks.PaletteNodeCallback()
    // @see serializeAllEditNodes()
    // @see serializeEditNodes()
    // @visibility external
    //<
    getPaletteNodesFromXML : function (xmlString, callback) {
        var self = this;

        //isc.logWarn(isc.echo(xmlString));
        
        isc.DMI.callBuiltin({
            methodName: "xmlToJS",
            arguments: [xmlString],
            callback: function (rpcResponse) {
                self.getPaletteNodesFromJS(rpcResponse.data, callback);
            }
        });
    },

    //>!BackCompat 2013.09.25
    loadNodeTreeFromJS : function (jsString) {
        return this.addPaletteNodesFromJS(jsString);
    },
    //<!BackCompat

    //> @method editContext.addPaletteNodesFromJS()
    // Add +link{PaletteNode,PaletteNodes} from a JavaScript source representation.
    // <P>
    // By default, components that have +link{Canvas.ID,global IDs} will not
    // actually be allowed to take those global IDs - instead, only widgets that have one of the
    // global IDs passed as the <code>globals</code> parameter will actually receive their global
    // IDs.  To override this behavior, pass the special value +link{RPCManager.ALL_GLOBALS}
    // for the <code>globals</code> parameter.
    //
    // @param jsCode (String) JavaScript code to eval.
    // @param [parentNode] (EditNode) parent node (defaults to the root)
    // @param [globals] (Array of String) widgets to allow to take their global IDs
    // @visibility external
    //<
    addPaletteNodesFromJS : function (jsCode, parentNode, globals, callback) {
        if (globals == null) globals = [];
        else if (!isc.isAn.Array(globals)) globals = [globals];

        var self = this;
        this.getPaletteNodesFromJS(jsCode, function (paletteNodes) {
            // Remove IDs that represent globals that should not be kept
            this._removeIDs(paletteNodes, globals);

            self.addFromPaletteNodes(paletteNodes, parentNode);
            self.fireCallback(callback, ["paletteNodes"], [paletteNodes]);
        }, globals);
    },
    
    //> @method editContext.getPaletteNodesFromJS()
    // Obtain +link{PaletteNode,PaletteNodes} from a JavaScript source representation.
    // <P>
    // By default, components that have +link{Canvas.ID,global IDs} will not
    // actually be allowed to take those global IDs - instead, only widgets that have one of the
    // global IDs passed as the <code>globals</code> parameter will actually receive their global
    // IDs.  To override this behavior, pass the special value +link{RPCManager.ALL_GLOBALS}
    // for the <code>globals</code> parameter.
    //
    // @param jsCode (String) JavaScript code to eval.
    // @param callback (PaletteNodeCallback) Callback used to return the PaletteNodes
    // @param [globals] (Array of String) widgets to allow to take their global IDs
    // @see Callbacks.PaletteNodeCallback()
    // @visibility external
    //<
    getPaletteNodesFromJS : function (jsCode, callback, keepGlobals) {
        if (keepGlobals == null) keepGlobals = [];
        else if (!isc.isAn.Array(keepGlobals)) keepGlobals = [keepGlobals];

        var self = this;
        isc.captureDefaults = true;

        if (keepGlobals.length == 1 && keepGlobals[0] == isc.RPC.ALL_GLOBALS) {
            // suppress reportErrors
            isc.Class.globalEvalWithCapture(jsCode, function (globals, error) {
                // Note: this must happen first, before any other components are
                // created - otherwise we will trap them..
                isc.captureDefaults = null;
                var capturedComponents = self.getCapturedComponents(error);
                self.fireCallback(callback, ["paletteNodes"], [capturedComponents]);
            }, null, false);
        } else {
            // suppress reportErrors
            isc.Class.globalEvalAndRestore(jsCode, keepGlobals, function (globals, error) {
                // Note: this must happen first, before any other components are
                // created - otherwise we will trap them..
                isc.captureDefaults = null;
                var capturedComponents = self.getCapturedComponents(error);
                self.fireCallback(callback, ["paletteNodes"], [capturedComponents]);
            }, null, false);
        }

        isc.captureDefaults = null;
    },

    getCapturedComponents : function (error) {
        if (error) {
            isc.warn(
                "The following error occurred during loading of your view<br><br>: " + error + 
                ".<br><br>  The portion of the view that loaded succesfully will be shown."
            );
        }                

        var captured = isc.capturedComponents;
        isc.capturedComponents = null;

        var capturedIDs = (captured ? captured.getProperty("defaults").getProperty("ID") : null);
        this.logInfo("capturedComponents are: " + capturedIDs, "loadProject");

        return captured;
    },

    //>!BackCompat 2013.09.27
    addNodeTree : function (paletteNodes) {
        this.addFromPaletteNodes(paletteNodes);
    },
    //<!BackCompat

    //> @method EditContext.addFromPaletteNodes
    // Add the supplied +link{PaletteNode,PaletteNodes} to the parentNode, preserving internal
    // references from one supplied PaletteNode to another. This method should
    // be used with an array of possibly inter-related PaletteNodes (for
    // instance, those produced as a result of serialization via
    // +link{serializeAllEditNodes(),serializeAllEditNodes()}, rather than
    // calling +link{addFromPaletteNode(),addFromPaletteNode()} on each
    // individual PaletteNode.
    //
    // @param paletteNodes (Array of PaletteNode) array of PaletteNodes
    // @param [parentNode] (EditNode) parent to add to (defaults to the root)
    // @return (Array of EditNode) an array of the EditNodes added to the parentNode
    // @see addFromPaletteNode()
    // @visibility external
    //<
    addFromPaletteNodes : function (paletteNodes, parentNode) {
        //this.logWarn("paletteNodes: " + this.echoFull(paletteNodes), "loadProject");

        var data = this.getEditNodeTree();
        if (!parentNode) parentNode = data.getRoot();

        // When we evalWithCapture(), create() makes palette nodes instead of actual
        // instances.  This is a necessity so that initialization data can be captured cleanly.
        // 
        // These palette nodes are arranged in a tree just like live components would be (eg
        // layout.members contains palette nodes for children).
        //
        // We need to traverse this tree and make a series of calls to
        // Palette.makeEditNode() and EditContext.addNode() to actual create live
        // components and editNodes from this captured data.

        this.componentsToCreate = [];
        this.addComponentCalls = [];
        this.requiredDataSources = [];

        // traverse all captured components (components that called create()), finding all
        // subcomponents that need to be represented as separate tree nodes (eg Tabs, which do
        // not directly call create, but should appear in the editTree).
        for (var i = 0; i < paletteNodes.length; i++) {
            this.findChildPaletteNodes(null, paletteNodes[i], null, paletteNodes);
        }

        // second traversal: paletteNodes is a flattened list of all components that would call
        // create(), and in the previous traversal we marked any component that was found in
        // the subtree of any other component as hasParent:true.  Any remaining paletteNodes
        // with no hasParent:true marker must be children of root
        for (var i = 0; i < paletteNodes.length; i++) {
            if (!paletteNodes[i].hasParent) {
                this.findChildPaletteNodes(parentNode, paletteNodes[i], null, paletteNodes);
            }
        }

        // preserve init order for the best chance of allowing application logic to function

        var pNode, parentPNode;
        // captured components (those that directly called create) are first, in order of
        // create() calls (which is leaf nodes first)
        for (var i = 0; i < paletteNodes.length; i++) {
            pNode = paletteNodes[i];
            pNode.component = this.makeEditNode(pNode);
        }

        // create all other components in tree traversal order
        for (var i = 0; i < this.componentsToCreate.length; i++) {
            pNode = this.componentsToCreate[i];
            if (!pNode.component) {
                pNode.component = this.makeEditNode(pNode);
            }
        }

        // lastly, link components into the project tree.  Because of the way we do our
        // traversal, these are not in an order that is ready for tree adds, that is, children
        // can appear before their parents because objects that directly call create() can
        // appear before the pseudo-objects (eg Sections) that they belong to (eg ListGrid can
        // appear before the Tab it should be added to).  However in order to eg, not reverse
        // Section Stack or FormItem order, we need to generally follow the order of traversal
        // that put together the addComponentCalls.
        // Approach: keep traversing the list trying to add children to parents until all nodes
        // have been added
        var oldLength = -1,
            calls = this.addComponentCalls,
            newCallOrder = []; // just for debugging
            
        // Set a flag to indicate to the special editModeSetDataSource() override that we are 
        // loading a node tree from disk, and should fall back to the ordinary setDataSource()
        // method - otherwise, we'll end up with duplicates in the projectComponents tree
        // Also, disables markDirty while true.
        isc._loadingNodeTree = true;

        var nodesAddedToParentNode = [];
            
        while (calls.length > 0 && oldLength != calls.length) {
            oldLength = calls.length;
            var callsToTry = calls.duplicate();
            for (var i = 0; i < callsToTry.length; i++) {
                var call = callsToTry[i],
                    parentPNode = call[1],
                    pNode = call[0],
                    parentProperty = call[2];
                if (parentPNode.name == "/") {
                    var nodedAdded = this.addNode(pNode.component, parentNode);
                    nodesAddedToParentNode.add(nodeAdded);
                    calls.remove(call);
                    newCallOrder.add(call);
                } else if (data.contains(parentPNode.component)) {
                    var childComponent = pNode.component;
                    if (data.contains(childComponent)) {
                        // we've already added this child to the tree elsewhere.  This occurs
                        // for singletons like a DataSource which are shared between multiple
                        // components.  It's valid and intended in this case that the
                        // liveObject be shared, but we need a distinct Tree node, so make a
                        // copy
                        childComponent = isc.addProperties({}, childComponent);
                    }
                    var nodeAdded = this.addNode(childComponent, parentPNode.component, null, parentProperty);
                    if (parentPNode.component == parentNode) {
                        nodesAddedToParentNode.add(nodeAdded);
                    }
                    calls.remove(call);
                    newCallOrder.add(call);
                }
            }
        }
        
        delete isc._loadingNodeTree;

        // report the order of addComponent calls
        if (this.logIsDebugEnabled("loadProject")) {
            this.logDebug("addComponent() calls during project loading:", "loadProject");
            for (var i = 0; i < newCallOrder.length; i++) {
                var call = newCallOrder[i],
                    parentPNode = call[1],
                    pNode = call[0];
                this.logDebug(
                    "addComponent(" + this.echoLeaf(pNode) + "," + this.echoLeaf(parentPNode), 
                    "loadProject"
                );
            }
        }
        
        if (calls.length > 0) {
            this.logWarn(
                "the following components could not be added to the project tree: " + 
                isc.echoAll(calls.getProperty("0"))
            );
        }

        return nodesAddedToParentNode;
    },

    // create a paletteNode that will load the named DataSource dynamically
    makeDSPaletteNode : function (dsName, dsType) {
        var node = {
            ID: dsName,

            // for controlling drag and drop
            // XXX would be good to get actual type in case a component
            // declares that it can only bind to a specific DataSource, however,
            // "getDefinedDataSources" RPC does not currently return this.
            type: "DataSource",

            // for display in DataSources palette
            dsType: dsType || "DataSource",
            
            // for display in project tree
            title: dsName,
            icon: "DataSource.png",
            iconSize: 16,

            // set up deferred loading
            loadData: function (node, callback) {
                isc.DS.get(node.ID, function (ds) {
                    node.liveObject = ds;
                    // minimal information for serializing the DataSource.  See
                    // getSerializeableTree()
                    node.defaults = {
                        _constructor: "DataSource", 
                        ID: ds.ID
                    };
                    node.isLoaded = true;
                    isc.Class.fireCallback(callback, "", [node]);
                });
            }
        };
        
        return node;
    },

    // recursively traverse a structure captured via evalWithCapture, modifying data so that it
    // is ready for addComponent().  
    // - detect anywhere that a component is being initialized with data that should be
    //   represented as a separate component in the EditTree (eg a Layout member or TabSet tab)
    // - remove the subcomponent from the initialization data and create a separate
    //   paletteNode for it.  The cases are:
    //   - palette nodes captured by evalWithCapture, from components that called create()
    //   - tabs, sectionItems and other pseudo-objects that we represent in the editTree,
    //     detected because they are in a field whose type appears in the palette
    //   - for code that *was not* generated by Visual Builder, we may find eg a Layout member
    //     represented as an object with a _constructor property, as happens when you declare
    //     nested components in XML instead of breaking all components into independant
    //     top-level declarations.  Note these subcomponents will not be paletteNodes because
    //     create() was never called for them.  Instead, their format is similar to a TabSet
    //     tab or other pseudo-object
    // - generate and store the list of addComponent() calls needed to construct the tree.  We
    //   do these later in order to detect top-level components, and to maximally preserve
    //   initialization order.
    findChildPaletteNodes : function (parent, componentData, parentProperty, paletteNodes) {
        var componentType = componentData.type || componentData.className;

        var logEnabled = this.logIsInfoEnabled("loadProject"),
            logDebugEnabled = this.logIsDebugEnabled("loadProject");
    
        if (logEnabled) {
            this.logInfo(
                "inspecting defaults of component: " + this.echoLeaf(componentData) + " of type: " + componentType, 
                "loadProject"
            );
        }

        var defaults = componentData.defaults,
        	loader = this;

        // search for child components that should also be added to the project tree
        var childComponents = [],
            singleArray = [],
            componentDS = isc.DS.get(componentType);
        for (var propName in defaults) {
            var propValues = defaults[propName];

            if (!isc.isAn.Array(propValues)) {
                singleArray[0] = propValues;
                propValues = singleArray;
            } else if (logDebugEnabled) {
                this.logDebug(
                    "checking Array property: " + propName + ", value: " + this.echoLeaf(propValues) +
                    (fieldSchema ? " with schema: " + fieldSchema : ""), 
                    "loadProject"
                );
            }

            var field = componentDS ? componentDS.getField(propName) : null,
                fieldType = field ? field.type : null,
                fieldSchema = isc.DS.get(fieldType),
                foundChildren = false;

            for (var i = 0; i < propValues.length; i++) {
                var propValue = propValues[i];
                if (logDebugEnabled) {
                    this.logDebug(
                        "checking property: " + propName + ", value: " + this.echoLeaf(propValue), 
                        "loadProject"
                    );
                }

                // found a component captured by evalWithCapture (called create())
                if (paletteNodes.contains(propValue)) {
                    if (logEnabled) {
                        this.logInfo(
                            "found capturedComponent: " + this.echoLeaf(propValue) + " under property: " + 
                            propName + " of component: " + this.echoLeaf(componentData),
                            "loadProject"
                        );
                    }
                    childComponents.add([propName, propValue]);
                    foundChildren = true;
                    continue;
                } 

                if (propValue == null) {
                    this.logInfo("null property: " + propName + " on component: " + this.echoLeaf(componentData));
                }
                                    
                // detect pseudo-objects (eg tabs):
                // if the field is declared as complex type *and* items of this class can be
                // created from the palette (so clearly it is represented in the component tree).
                // Note that this means different editors may treat different objects as tree
                // nodes, for example, fields of a ListGrid.
                var childType = (propValue ? propValue._constructor : null) || fieldType,
                    childClass = isc.ClassFactory.getClass(childType);

                if (
                    fieldSchema && (
                        (childClass && childClass.isA(isc.Canvas)) ||
                        (childClass && childClass.isA(isc.DataSource)) ||
                        (childClass && childClass.isA(isc.FormItem)) ||
                        (this.findPaletteNode("type", childType)) ||
                        (this.findPaletteNode("className", childType))
                    )
                ) {
                    if (logEnabled) {
                        this.logInfo(
                            "found palettized component: " + this.echoLeaf(propValue) + 
                            " of type: " + childType + " under property: " + propName + 
                            " of component: " + this.echoLeaf(componentData),
                            "loadProject"
                        );
                    }

                    // A String in an Object slot should be the ID of a component that was
                    // already created.  NOTE: tab.pane can be a String that refers to a
                    // component that was created *after* the TabSet, however this code does
                    // handle that case since capturedComponents contains all components that
                    // called create()
                    if (isc.isA.String(propValue)) {
                        var refComponent = paletteNodes.find("ID", propValue);
                        if (refComponent == null) {
                            // detect fields of DataSource type with String values referring to
                            // DataSources that don't exist in the file.  This can happen with
                            // code not generated by Visual Builder.  If these DataSources are
                            // known (they appear in the dataSourceList loaded from the
                            // server), create a paletteNode that will load them automagically.
                            if (isc.DataSource.isA(fieldType)) {
                                var knownDS = this.findPaletteNode("ID", propValue);
                                if (true) {
                                    refComponent = this.makeDSPaletteNode(propValue);
                                }
                            }
                            if (refComponent == null) continue;
                        }
                        childComponents.add([propName, refComponent]);
                    } else {
                        var childDefaults = propValue;
                        childComponents.add([propName, {
                            ID : childDefaults.ID,
                            name : childDefaults.name,
                            type : childType,
                            defaults : childDefaults
                        }]); 
                    }

                    foundChildren = true;
                }
            }

            if (foundChildren) delete defaults[propName];
        }
    
        // find the existing palette node for this class, if any, in order to pick up the icon
        // to use in the project tree
        var pNode = this.findPaletteNode("type", componentType) || this.findPaletteNode("className", componentType);

        if (pNode) {
            componentData.icon = componentData.icon || pNode.icon; 
            componentData.iconSize = componentData.iconSize || pNode.iconSize; 
            componentData.showDropIcon = componentData.showDropIcon || pNode.showDropIcon; 
        }
    
        // collect all the components that should be created and the calls to addComponent()
        // that need to happen
        this.componentsToCreate.add(componentData);
        if (parent != null) {
            componentData.hasParent = true;
            this.addComponentCalls.add([componentData, parent, parentProperty]);
        }

        // recurse to handle the children of this component
        if (childComponents.length > 0) {
            for (var i = 0; i < childComponents.length; i++) {
                this.findChildPaletteNodes(componentData, childComponents[i][1], childComponents[i][0], paletteNodes);
            }
        }
    },

    // ---------------------------------------------------------------------------------------
    
    enableEditing : function (editNode) {
        var liveObject = editNode.liveObject;
        if (liveObject.setEditMode) {
            liveObject.setEditMode(true, this, editNode);
        } else {
            // We're trying enable editing on something that isn't a Canvas or a FormItem.
            // Assume that it needs no special logic beyond setting the editNode, editContext
            // and editingOn flag
            liveObject.editContext = this;
            liveObject.editNode = editNode;
            liveObject.editingOn = true;
        }
    },

    // Applying Properties to EditNodes
    // ---------------------------------------------------------------------------------------
   
    //> @method editContext.setNodeProperties()
    // Update an editNode's serializable "defaults" with the supplied properties. If you
    // wish to remove a property from the defaults (rather than setting it to null), then
    // use +link{removeNodeProperties(),removeNodeProperties()} instead.
    // @param editNode (EditNode) the editNode to update
    // @param properties (Canvas Properties) the properties to apply
    // @param [skipLiveObjectUpdate] (Boolean) whether to skip updating the
    //                                         +link{EditNode.liveObject,liveObject},
    //                                         e.g. if you have already updated the liveObject
    // @see removeNodeProperties
    // @visibility external
    //<
    
    setNodeProperties : function (editNode, properties, skipLiveObjectUpdate) {
        
 
        if (this.logIsDebugEnabled("editing")) {
            this.logDebug("with editNode: " + this.echoLeaf(editNode) + 
                          " applying properties: " + this.echo(properties), "editing");
        }
  
        if (!editNode.defaults) editNode.defaults = {}

        // update the initialization / serializeable data
        isc.addProperties(editNode.defaults, properties);
        
        // update the component node with the new ID
        if (editNode.defaults.ID != null) editNode.ID = editNode.defaults.ID;
    
        // update the live object, unless we're skipping that
        var targetObject = editNode.liveObject;
        if (targetObject && !skipLiveObjectUpdate) {
            // Name property changes must force a remove/add of the node (such as name
            // on a FormItem). This is specified in the "rebuildOnChange" property of the
            // parent property.
            var theTree = this.getEditNodeTree(),
                parentComponent = this.getEditNodeTree().getParent(editNode),
                parentSchema = (parentComponent ? isc.DS.get(parentComponent.type) : null),
                parentLiveObject = (parentComponent ? parentComponent.liveObject : null),
                parentFieldName = (parentLiveObject ? isc.DS.getObjectField(parentLiveObject, editNode.type) : null),
                parentField = (parentFieldName ? parentSchema.fields[parentFieldName] : null)
            ;
            if (properties.name != null && parentField && parentField.rebuildOnChange && parentField.rebuildOnChange.toLowerCase() == "true") {
                var index = theTree.getChildren(parentComponent).findIndex(editNode);

                this.logInfo("using remove/re-add cycle to modify liveObject: " +
                            isc.echoLeaf(targetObject) + " within parent node " +
                            isc.echoLeaf(parentComponent));
                    
                this.removeNode(editNode);

                // update the node with the new name and add it
                editNode.name = editNode.ID = properties.name;
                delete properties.name;

                this.addNode(editNode, parentComponent, index);

                // collect the newly created live object
                targetObject = this.getLiveObject(editNode);
            }

            // update the live object
            if (targetObject.setEditableProperties) {
                // instance of an SC class (or something else that implements a
                // setEditableProperties API)
                targetObject.setEditableProperties(properties);
                if (targetObject.markForRedraw) targetObject.markForRedraw();
                // NOTE: for FormItems, causes parent redraw
                else if (targetObject.redraw) targetObject.redraw();
            } else {
                // for objects that never become ISC classes (MenuItems, ListGrid fields), 
                // call an overridable method on the parent if it exists
                var parentComponent = this.getEditNodeTree().getParent(editNode),
                    parentLiveObject = parentComponent ? parentComponent.liveObject : null;
                if (parentLiveObject && parentLiveObject.setChildEditableProperties) 
                {
                    parentLiveObject.setChildEditableProperties(targetObject, properties, 
                                                                editNode, this);
                } else {
                    // fall back to just applying the properties
                    isc.addProperties(targetObject, properties);
                }
            }
        
            this.markForRedraw();
        } // skipLiveObjectUpdate
    },

    //> @method editContext.removeNodeProperties()
    // Removes the specified properties from an editNode's serializable "defaults".
    // Note that the +link{EditNode.liveObject,liveObject} is <u>not</u> updated by this method. 
    // To set a property to null (rather than removing it), use
    // +link{setNodeProperties(),setNodeProperties()} instead.
    // @param editNode (EditNode) the editNode to update
    // @param properties (Array of String) an array of property names to remove
    // @see setNodeProperties()
    // @visibility external
    //<
    removeNodeProperties : function (editNode, properties) {
        if (!isc.isAn.Array(properties)) properties = [properties];
        properties.map (function (property) {
            delete editNode[property];
        });
    },

    // ---------------------------------------------------------------------------------------

    // The "wrapperForm" is a DynamicForm that we auto-create as a container for a FormItem dropped 
    // directly onto a Canvas, Layout or whatever.  We're using autoChild-like semantics here so 
    // that you can provide your own settings for the generated form.  addWithWrapper() is also
    // used to wrap DrawItems in a DrawPane, and the third argument, wrapDrawPane, is a boolean
    // flag to distinguish the desired wrapper.
    wrapperFormDefaults: {
        _constructor: "DynamicForm"
    },
    wrapperDrawPaneDefaults: {
        _constructor: "DrawPane"
    },
    addWithWrapper : function (childNode, parentNode, wrapDrawPane) {
        var wrapForm = !wrapDrawPane,
            wrapperDefaults = (wrapDrawPane ? this.wrapperDrawPaneDefaults : this.wrapperFormDefaults),
            defaults = isc.addProperties({}, wrapperDefaults),
            paletteNode = {
                type: wrapperDefaults._constructor,
                defaults : defaults
            };

        // if this FormItem belongs to a DataSource, the wrapper form needs to use it too
        if (wrapForm && childNode.liveObject.schemaDataSource) {
            var item = childNode.liveObject;
            defaults.doNotUseDefaultBinding = true;
            defaults.dataSource = item.schemaDataSource;
            defaults.serviceNamespace = item.serviceNamespace;
            defaults.serviceName = item.serviceName;
        }
        var wrapperNode = this.makeEditNode(paletteNode);

        // add the wrapper to the parent
        this.addNode(wrapperNode, parentNode);
        // add the child node to the wrapper
        return this.addNode(childNode, wrapperNode);
    }
});

//> @groupDef devTools
// The Dashboards & Tools framework enables you to build interfaces in which a set of UI
// components can be edited by end users, saved and later restored.
// <P>
// This includes interfaces such as:
// <ul> 
// <li> <b>Dashboards</b>: where a library of possible widgets can be created &
// configured then stored for future use and shared with other users
// <li> <b>Diagramming & Flowchart tools</b>: tools similar to Visio&trade; which allows users
// to use shapes and connectors to create a flowchart or diagram
// <li> <b>Form Builders &amp; Development Tools</b>: tools which enable end users to create
// new forms or entirely new screens and add them to the application on the fly
// </ul>
// <P>
// The basic building blocks of the Dashboards & Tools framework are:
// <ul>
// <li> <i>Palettes</i>: palettes create UI components on the fly from stored data.  Palettes
// come in a variety of flavors which implement different UI gestures for creating components
// (e.g. drag from tree, pick from menu, etc)
// <li> <i>EditContexts</i>: an edit context tracks a set of components that are being edited.
// Different EditContexts offer different built-in user interactions for editing the
// components they track, for example, a tree-based EditContext can provide an interface for
// managing relationships between components via drag and drop, and a Canvas-based EditContext
// can automatically store changes to position and size.
// </ul>
// <h3>Defaults vs LiveObject</h3>
// The Dashboards & Tools framework is careful to maintain a distinction between the current state
// of the live UI component and the data that should actually be persisted and used to
// re-create the component.  For example:
// <ul>
// <li> a portlet may be showing a border or brightened background color to hilite it within
// the tool, but this should not be saved as part of the portlet state
// <li> a component may have a current width of 510 pixels when viewed within a tool, but what
// should persist is the configured width of 40% of available space
// <li> in a development tool, a component such a Window automatically creates subcomponents
// such as a header - these should not persist because the Window knows how to create them
// without any additional data.  Only components specifically dragged into the Window by the
// end user should be persisted
// </ul>
// The data that will be saved and used to re-create the component is called the
// <b>defaults</b>.  A Palette, which creates a component from stored data, minimally requires
// the +link{SCClassName,type} of the component to create, and the defaults.  This
// information is captured by a +link{PaletteNode} (along with other options) - all types of
// Palettes work with a set or tree of paletteNodes.
// <P>
// When a component is created from a paletteNode, a "live object" is created from the
// defaults.  One PaletteNode may generate any number of live objects.  An +link{EditNode} 
// combines the +link{editNode.liveObject,live object} along with the type and defaults
// needed to recreate the live object, and is used to track the created live object as it is
// further edited.  Essentially an EditNode is a copy of the PaletteNode in which a live object
// has been created and the editNode.defaults may now begin to change as the user edits the
// object they have created.
//
// <h3>Module requirements</h3>
// <b>NOTE:</b> you must load the Tools +link{group:loadingOptionalModules,Optional Module} 
// for this framework.
// <P>
// Any tools that work with hierarchies of system components or derivations
// of them will also need the system schema which can be loaded by either of the
// following:
// <P>
// <i>JSP tag:</i> <pre>&lt;script&gt;&lt;isomorphic:loadSystemSchema /&gt;&lt;/script&gt;</pre>
// <P>
// <i>HTML tag:</i> <pre>&lt;SCRIPT SRC="../isomorphic/DataSourceLoader?dataSource=$systemSchema"&gt;&lt;/SCRIPT&gt;</pre>
//
// @title Dashboards & Tools Framework Overview
// @treeLocation Client Reference/Tools
// @visibility external
//<





//> @object PaletteNode
// An object representing a component which the user may create dynamically within an
// application.
// <P>
// A PaletteNode expresses visual properties for how the palette will display it (eg
// +link{paletteNode.title,title}, +link{paletteNode.icon,icon}) as well as instructions for
// creating the component the paletteNode represents (+link{paletteNode.type},
// +link{paletteNode.defaults}).
// <P>
// Various types of palettes (+link{ListPalette}, +link{TreePalette}, +link{MenuPalette},
// +link{TilePalette}) render a PaletteNode in different ways, and allow the user to trigger
// creation in different ways (eg drag and drop, or just click).  All share a common pattern
// for how components are created from palettes.
// <P>
// Note that in a TreePalette, a PaletteNode is essentially a +link{TreeNode} and can have
// properties expected for a TreeNode (eg,
// +link{TreeGrid.customIconDropProperty,showDropIcon}.  Likewise
// a PaletteNode in a MenuPalette can have the properties of a +link{MenuItem}, such as
// +link{MenuItem.enableIf}.
// 
// @treeLocation Client Reference/Tools
// @visibility external
//<

//> @attr paletteNode.icon (SCImgURL : null : IR)
// Icon for this paletteNode.
//
// @visibility external
//<

//> @attr paletteNode.title (String : null : IR)
// Textual title for this paletteNode.
//
// @visibility external
//<

//> @attr paletteNode.type (SCClassName : null : IR)
// +link{SCClassName} this paletteNode creates, for example, "ListGrid".
//
// @visibility external
//<


//> @attr paletteNode.defaults (Properties : null : IR)
// Defaults for the component to be created from this palette.  
// <P>
// For example, if +link{paletteNode.type} is "ListGrid", properties valid to pass to
// +link{Class.create,ListGrid.create()}.
//
// @visibility external
//<

//> @attr paletteNode.liveObject (Object : null : IR)
// For a paletteNode which should be a "singleton", that is, always provides the exact same
// object (==) rather than a dynamically created copy, provide the singleton object as
// <code>liveObject</code>.
// <P>
// Instead of dynamically creating an object from defaults, the <code>liveObject</code> will
// simply be assigned to +link{editNode.liveObject} for the created editNode.
//
// @visibility external
//<

//> @attr paletteNode.wizardConstructor (PaletteWizard : null : IR)
// A paletteNode that requires user input before component creation can occur 
// may provide a <code>wizardConstructor</code> and +link{wizardDefaults} for the creation of
// a "wizard" component.
// <P>
// Immediately after creation, the wizard will then have the +link{paletteWizard.getResults()}
// method called on it, dynamically produced defaults.
//
// @visibility internal
//<

//> @attr paletteNode.wizardDefaults (PaletteWizard Properties : null : IR)
// Defaults for the wizard created to gather user input before a component is created from
// this PaletteNode.  See +link{wizardConstructor}.
//
// @visibility internal
//<



// PaletteWizard
// ---------------------------------------------------------------------------------------

//> @interface PaletteWizard
// Interface to be fulfilled by a "wizard" specified on a +link{PaletteNode} via
// +link{paletteNode.wizardConstructor}.
// @visibility internal
//<

//> @method paletteWizard.getResults()
// Single function invoked on paletteWizard.  Expects defaults to be asynchronously returned,
// after user input is complete, by calling the +link{Callback} provided as a parameter.
// 
// @param callback (Callback) callback to be fired once this wizard completes.  Expects a
//                            single argument: the defaults
// @param paletteNode (PaletteNode) the paletteNode that specified this wizard
// @param palette (Palette) palette where creation is taking place
//
// @visibility internal
//<

//> @interface Palette
// An interface that provides the ability to create components from a +link{PaletteNode}.  
//
// @treeLocation Client Reference/Tools
// @group devTools
// @visibility external
//<

isc.ClassFactory.defineInterface("Palette");

isc.Palette.addInterfaceProperties({
    //> @attr palette.defaultEditContext (EditContext : null : IRW)
    // Default EditContext that this palette should use.  Palettes generally create components via
    // drag and drop, but may also support creation via double-click or other UI idioms when a
    // defaultEditContext is set.
    // @visibility external
    //<

    //> @method palette.setDefaultEditContext()
    // Sets the default EditContext that this palette should use.  Palettes generally create components via
    // drag and drop, but may also support creation via double-click or other UI idioms when a
    // defaultEditContext is set.
    // @param defaultEditContext (EditContext) the default EditContext used by this Palette
    // @visibility external
    //<
    setDefaultEditContext : function (defaultEditContext) {
        this.defaultEditContext = defaultEditContext;

        // If the defaultEditContext does not have a defaultPalette, then set it
        if (defaultEditContext && !defaultEditContext.defaultPalette) {
            defaultEditContext.defaultPalette = this;
        }
    },

    //> @method palette.makeEditNode()
    // Given a +link{PaletteNode}, make an +link{EditNode} from it by creating a 
    // +link{editNode.liveObject,liveObject} from the +link{paletteNode.defaults}
    // and copying presentation properties (eg +link{paletteNode.title,title}
    // to the editNode. If <code>editNodeProperties</code> is specified as an object on
    // on the paletteNode, each property in this object will also be copied across to
    // the editNode.
    // @param paletteNode (PaletteNode) paletteNode to create from
    // @return (EditNode) created EditNode
    //
    // @visibility external
    //<
    makeEditNode : function (paletteNode) {
        if (!paletteNode) paletteNode = this.getDragData();
        if (isc.isAn.Array(paletteNode)) paletteNode = paletteNode[0];
    
        var type = paletteNode.type || paletteNode.className;
        if (!isc.SGWTFactory.getFactory(type) && type.contains(".")) type = type.split(/\./).pop();

        var componentNode = {
            type : type,
            _constructor : type, // this is here just to match the defaults
            // for display in the target Tree
            title : paletteNode.title,
            icon : paletteNode.icon,
            iconSize : paletteNode.iconSize,
            showDropIcon : paletteNode.showDropIcon,
            useEditMask : paletteNode.useEditMask,
            autoGen : paletteNode.autoGen   
        };
        
        // support arbitrary properties on the generated edit node
        // This allows 'loadData' to get at properties that might not otherwise be copied
        // across to the editNode from the paletteNode
        if (isc.isAn.Object(paletteNode.editNodeProperties)) {
            for (var prop in paletteNode.editNodeProperties) {
                componentNode[prop] = paletteNode.editNodeProperties[prop];
            }
        }

        // allow a maker function on the source data (synchronous)
        if (paletteNode.makeComponent) {
            componentNode.liveObject = paletteNode.makeComponent(componentNode);
            return componentNode;
        }

        // NOTE: IDs
        // - singletons may have an ID on the palette node.  
        // - an ID may appear in defaults because palette-based construction is used to reload
        //   views, and in this case the palette node will be used once ever
        var defaults = paletteNode.defaults;
        componentNode.ID = paletteNode.ID || 
                (defaults ? isc.DS.getAutoId(defaults) : null);
                
        var clobberDefaults = true;

        if (paletteNode.loadData) {
            // deferred load node.  No creation happens for now; whoever receives this node is
            // expected to call the loadData function
            componentNode.loadData = paletteNode.loadData;
        } else if (paletteNode.wizardConstructor) {
            // wizard-based deferred construction
            componentNode.wizardConstructor = paletteNode.wizardConstructor;
            componentNode.wizardDefaults = paletteNode.wizardDefaults;
        } else if (paletteNode.liveObject) {
            // singleton, or already created component.  This means that rather than a new
            // object being instantiated each time, the same "liveObject" should be reused,
            // because multiple components will be accessing a shared object.
            var liveObject = paletteNode.liveObject;
            // handle global IDs
            if (isc.isA.String(liveObject)) liveObject = window[liveObject];
            componentNode.liveObject = liveObject
        } else {
            // create a live object from defaults
            componentNode = this.createLiveObject(paletteNode, componentNode);
            clobberDefaults = false;
        }

        // also pass the defaults. Note that this was overwriting a more detailed set of defaults
        // derived by the createLiveObject method; hence the introduction  of the condition
        if (clobberDefaults) {
            componentNode.defaults = isc.addProperties({}, paletteNode.defaults);
            delete componentNode.defaults[isc.gwtRef];
            delete componentNode.defaults[isc.gwtModule];
            delete componentNode.defaults["xsi:type"];
        }

        return componentNode;
    },
    
    //>!BackCompat 2013.09.20
    makeNewComponent : function (sourceData) {
       return this.makeEditNode(sourceData);
    },
    //<!BackCompat
    
    //> @attr palette.generateNames   (boolean : true : IR)
    // Whether created components should have their "ID" or "name" property automatically set
    // to a unique value based on the component's type, eg, "ListGrid0".
    //
    // @group devTools
    // @visibility external
    //<
    generateNames : true,

    typeCount : {},
    // get an id for the object we're creating, by type
    getNextAutoId : function (type) {
        if (type == null) {
            type = "Object";
        } else {
            // Use short IDs for objects created via SGWT reflection
            if (type.contains(".")) type = type.split(/\./).pop(); 
        }
        var autoId;
        this.typeCount[type] = this.typeCount[type] || 0;
        while (window[(autoId = type + this.typeCount[type]++)] != null) {}
        return autoId;
    },

    findPaletteNode : function (fieldName, value) {
        return null;
    },

    createLiveObject : function (paletteNode, editNode) {

        // put together an initialization data block
        var type = paletteNode.type || paletteNode.className;
        if (type.contains(".") && !isc.SGWTFactory.getFactory(type)) type = type.split(/\./).pop();

        var classObject = isc.ClassFactory.getClass(type),
            schema = isc.DS.getNearestSchema(type),
            defaults = {},
            // assume we should create standalone if there's no schema (won't happen anyway if
            // there's no class)
            createStandalone = (schema ? schema.shouldCreateStandalone() : true),
            paletteNodeDefaults = paletteNode.defaults || {};

        // suppress drawing for widgets
        if (classObject && classObject.isA("Canvas")) defaults.autoDraw = false;

        // If a title was explicitly passed in the sourceData, use it
        if (paletteNodeDefaults.title) {
            defaults.title = paletteNodeDefaults.title;
        }

        if (this.generateNames) {
            // generate an id if one wasn't specified
            var ID = editNode.ID || paletteNodeDefaults[schema.getAutoIdField()];
            if (ID == null) {
                ID = this.getNextAutoId(type);
                
                if (isc.isA.Class(classObject)) {
                    defaults.hasStableID = function () { return false; };
                }
            }
            editNode.ID = ID;

            // give the object an autoId in defaults
            defaults[schema.getAutoIdField()] = ID;
    
            // don't supply a title for contexts where the ID or name will automatically be
            // used as a title (currently just formItems), otherwise, it will be necessary to
            // change both ID/name and title to get rid of the auto-gen'd id 
            if (
                schema && 
                schema.getField("title") && 
                !isc.isA.FormItem(classObject) &&
                !defaults.title
            ) {
                defaults.title = ID;
            }
        }

        defaults = editNode.defaults = isc.addProperties(
            defaults,
            this.componentDefaults,
            paletteNodeDefaults
        );
        delete defaults[isc.gwtRef];
        delete defaults[isc.gwtModule];
        // An xsi:type property in defaults should be dropped to avoid serializing because
        // it won't be valid without proper includes.
        delete defaults["xsi:type"];
        defaults._constructor = type;

        
        var classObject = isc.ClassFactory.getClass(type);
        for (var prop in defaults) {
            var val = defaults[prop];
            if (
                isc.isAn.Array(val) &&
                (!classObject || classObject.getInstanceProperty(prop) !== val)
            ) {
                val = defaults[prop] = val.duplicate();

                // Check for arrays of arrays.
                for (var i = val.length; i--; ) {
                    if (isc.isAn.Array(val[i])) {
                        val[i] = val[i].duplicate();
                    }
                }
            }
        }

        // create the live object from the init data
        // NOTE: components generated from config by parents (tabs, sectionStack sections,
        // formItems).  These objects:
        // - are created as an ISC Class by adding to a parent, and not before
        //   - in makeEditNode, don't create if there is no class or if the schema sets
        //     createStandalone=false
        // - destroyed by removal from the parent, then re-created by a re-add
        //   - re-add handled by addComponent by checking for destruction
        // - serialized as sub-objects rather than independent components
        //   - handled by checking for _generated during serialization
        //   - should be a default consequence of not having a class or setting
        //     createStandalone=false
        // The various checks mentioned above are redundant and should be unified and make able
        // to be declared in component schema

        // if there's no class for the item, or schema.createStandalone has been set false,
        // don't auto-create the component - assume the future parent of the component will
        // create it from data.  The explicit flag (createStandalone:false) is needed for
        // FormItems.  In particular, canvasItems require item.containerWidget to be defined
        // during init.
        var liveObject;
        if (classObject && createStandalone) {
            liveObject = isc.ClassFactory.newInstance(defaults);
        } else {
            // for the live object, just create a copy (NOTE: necessary because widgets
            // generally assume that it is okay to add properties to pseudo-objects provided as
            // init data)
            editNode.generatedType = true;
            liveObject = isc.shallowClone(defaults);
        }

        // store the new live object
        editNode.liveObject = liveObject;
        this.logInfo("palette created component, type: " + type +
                     ", ID: " + ID +
                     (this.logIsDebugEnabled("editing") ?
                         ", defaults: " + this.echo(defaults) : "") + 
                     ", liveObject: " + this.echoLeaf(liveObject), "editing");
        return editNode;
    }
});

//> @class HiddenPalette
// A Palette with no visible representation that handles programmatic creation of components.
//
// @implements Palette
// @group devTools
// @treeLocation Client Reference/Tools/Palette
// @visibility external
//<
isc.defineClass("HiddenPalette", "Class", "Palette");

// ---------------------------------------------------------------------------------------

//> @class TreePalette
// A TreeGrid that implements the Palette behavior, so it can be used as the source for 
// drag and drop instantiation of components when combined with an +link{EditContext} as 
// the drop target.
// <P>
// Each +link{TreeNode} within +link{treeGrid.data} can be a +link{PaletteNode}.
//
// @implements Palette
// @group devTools
// @treeLocation Client Reference/Tools/Palette
// @visibility external
//<

// Class will not work without TreeGrid
if (isc.TreeGrid) {

isc.defineClass("TreePalette", "TreeGrid", "Palette");

isc.TreePalette.addMethods({
    //> @attr treePalette.componentDefaults    (Object : null : IR)
    // Defaults to apply to all components originating from this palette.
    // @group devTools
    // @visibility external
    //<
    

    canDragRecordsOut:true,
    // add to defaultEditContext (if any) on double click 
    recordDoubleClick : function () {
        var target = this.defaultEditContext;
        if (target) {
            if (isc.isA.String(target) && this.creator) target = this.creator[target];
            if (isc.isAn.EditContext(target)) {
                var node = this.makeEditNode(this.getDragData());
                if (node) {
                    if (target.getDefaultParent(node, true) == null) {
                        isc.warn("No default parent can accept a component of this type");
                    } else {
                        target.addNode(node);
                        isc.EditContext.selectCanvasOrFormItem(node.liveObject, true);
                    }
                }
            }
        }
    },

    findPaletteNode : function (fieldName, value) {
        return this.data ? this.data.find(fieldName, value) : null;
    },

    // NOTE: we can't factor this up to the Palette interface because it wouldn't override the
    // built-in implementation of transferDragData.
    transferDragData : function (targetFolder) {
        return [this.makeEditNode(this.getDragData())];
    }
});

}

// --------------------------------------------------------------------------------------------
//> @class ListPalette
// A ListGrid that implements the +link{Palette} behavior, so it can be used as the source for 
// drag and drop instantiation of components when combined with an +link{EditContext} as 
// the drop target.
// <P>
// Each +link{ListGridRecord} can be a +link{PaletteNode}.
//
// @implements Palette
// @group devTools
// @treeLocation Client Reference/Tools/Palette
// @visibility external
//<

// Class will not work without ListGrid
if (isc.ListGrid) {

isc.defineClass("ListPalette", "ListGrid", "Palette");

isc.ListPalette.addMethods({
    canDragRecordsOut:true,
    defaultFields : [ { name:"title", title:"Title" } ],
    
    // add to defaultEditContext (if any) on double click 
    recordDoubleClick : function () {
        // NOTE: dup'd in TreePalette
        var target = this.defaultEditContext;
        if (target) {
            if (isc.isA.String(target)) target = isc.Canvas.getById(target);
            if (isc.isAn.EditContext(target)) {
                target.addNode(this.makeEditNode(this.getDragData()));
            }
        }
    },
    
    findPaletteNode : function (fieldName, value) {
        return this.data ? this.data.find(fieldName, value) : null;
    },
    
    // NOTE: we can't factor this up to the Palette interface because it wouldn't override the
    // built-in implementation of transferDragData.
    transferDragData : function () {
        return [this.makeEditNode(this.getDragData())];
    }
});

}

// --------------------------------------------------------------------------------------------
//> @class TilePalette
// A +link{TileGrid} that implements the +link{Palette} behavior, so it can be used as the source for 
// drag and drop instantiation of components when combined with an +link{EditContext} as 
// the drop target.
// <P>
// Each +link{TileGrid.tile} can be a +link{PaletteNode}.
//
// @implements Palette
// @group devTools
// @treeLocation Client Reference/Tools/Palette
// @visibility external
//<

// Class will not work without TileGrid
if (isc.TileGrid) {

isc.defineClass("TilePalette", "TileGrid", "Palette");

isc.TilePalette.addMethods({
    canDragRecordsOut: true,
    defaultFields: [
        {name: "title", title: "Title"}
    ],
    
    // add to defaultEditContext (if any) on double click 
    recordDoubleClick : function () {
        var target = this.defaultEditContext;
        if (target) {
            if (isc.isA.String(target)) target = isc.Canvas.getById(target);
            if (isc.isAn.EditContext(target)) {
                target.addNode(this.makeEditNode(this.getDragData()));
            }
        }
    },

    findPaletteNode : function (fieldName, value) {
        return this.data ? this.data.find(fieldName, value) : null;
    },
    
    // NOTE: we can't factor this up to the Palette interface because it wouldn't override the
    // built-in implementation of transferDragData.
    transferDragData : function () {
        return [this.makeEditNode(this.getDragData())];
    }
});

}

// --------------------------------------------------------------------------------------------
//> @class MenuPalette
// A Menu that implements the +link{Palette} behavior, so it can be used as the source for 
// drag and drop instantiation of components when combined with an +link{EditContext} as 
// the drop target.
// <P>
// Each +link{MenuItem} can be a +link{PaletteNode}.
//
// @implements Palette
// @group devTools
// @treeLocation Client Reference/Tools/Palette
// @visibility external
//<

// Class will not work without Menu
if (isc.Menu) {

isc.defineClass("MenuPalette", "Menu", "Palette");

isc.MenuPalette.addMethods({
    canDragRecordsOut: true,
    
    // needed because the selection is what's dragged, and menus do not normally track a
    // selection
    selectionType: "single",

    // add to defaultEditContext (if any) on click 
    itemClick : function (item) {
        var target = this.defaultEditContext;
        if (target) {
            if (isc.isA.String(target)) target = isc.Canvas.getById(target);
            if (isc.isAn.EditContext(target)) {
                target.addNode(this.makeEditNode(this.getDragData()));
            }
        }
    },
    
    findPaletteNode : function (fieldName, value) {
        return this.data ? this.data.find(fieldName, value) : null;
    },
    
    // NOTE: we can't factor this up to the Palette interface because it wouldn't override the
    // built-in implementation of transferDragData.
    transferDragData : function () {
        return [this.makeEditNode(this.getDragData())];
    }
});

}




// ---------------------------------------------------------------------------------------

//> @class EditPane
// A container that allows drag and drop instantiation of visual components from a
// +link{Palette}, and direct manipulation of the position and size of those components.
// <P>
// Any drag onto an EditPane from a Palette will add an EditNode created from the dragged
// PaletteNode.
//
// @group devTools
// @implements EditContext
// @treeLocation Client Reference/Tools/EditContext
// @visibility external
//<

// Schema definition for the EditPane class, in case we have not loaded the system schema.

if (!isc.DataSource.get("EditPane")) {
    isc.DataSource.create({
        ID: "EditPane",
        Contructor: "EditPane",
        addGlobalId:false,
        fields: [
            {name: "children", type: "Canvas", multiple: true}
        ]
    });
}

isc.ClassFactory.defineClass("EditPane", "Canvas", "EditContext");

isc.EditPane.addProperties({
    canAcceptDrop:true,
    contextMenu : {
        autoDraw:false,
        data : [{title:"Clear", click: "target.removeAll()"}]
    },

    editingOn:true,

    // drag selection properties
    canDrag:true,
    dragAppearance:"none",
    overflow:"hidden",
    selectedComponents: [],
    
    initWidget : function () {
        // We'll be the live object for the root node ... this gets picked up 
        // later in getEditNodeTree
        this.rootLiveObject = this;
        this.rootComponent = {
            _constructor: "EditPane"
        };
        this.Super("initWidget", arguments);
    },

    // Component creation
    // ---------------------------------------------------------------------------------------

    // This is needed if the system schema has not been loaded
    getObjectField : function (type) {
        var classObject = isc.ClassFactory.getClass(type);
        if (isc.isA.Canvas(classObject)) {
            return "children";
        } else {
            return null;
        }
    },

    // on drop from a palette, add a new component 
    drop : function () {
        var source = isc.EH.dragTarget;

        // if the source isn't a Palette, do standard drop interaction
        if (!source.isA("Palette")) return this.Super("drop", arguments);
        
        var data = source.transferDragData(),
        editNode = (isc.isAn.Array(data) ? data[0] : data);
        if (!editNode) return false;

        var editContext = this;
        this.requestLiveObject(editNode, function (editNode) {
            if (editNode) editContext.addNodeAtCursor(editNode);
        }, source)
        
        return isc.EH.STOP_BUBBLING;
    },

    nodeAdded : function (newNode) {
        
        // Flip it into edit mode depending on the setting on the VB instance
        if (this.creator && this.creator.editingOn) this.enableEditing(newNode);

        // Add an event mask if so configured
        if (newNode.useEditMask) newNode.liveObject.showEditMask();
    },

    // add a new component at the current mouse position
    addNodeAtCursor : function (component) {
        this.addNode(component);
        var liveObject = component.liveObject;
        liveObject.moveTo(this.getOffsetX(), this.getOffsetY());                
    },

    // The EditPane itself is the default parent for added nodes
    getDefaultParent : function (newNode, returnNullIfNoSuitableParent) {
        return this.getEditNodeTree().getRoot();
    },

    // Component removal / destruction
    // ---------------------------------------------------------------------------------------
    
    // if a child is removed that is being edited, remove it from the list of edit components
    removeChild : function (child, name) {
        this.Super("removeChild", arguments);
        var node = this.getEditNodeArray().find("liveObject", child);
        if (node) {
            this.removeNode(node, true); // skip live removal, since that's been done
        }
        this.selectedComponents.remove(child);
    },
    
    removeSelection : function (target) {
        if (this.selectedComponents.length > 0) {
            while (this.selectedComponents.length > 0) {
                this.selectedComponents[0].destroy();
            }
        } else {
            target.destroy();
        }
    },

    // Thumbs, drag move and resize
    // ---------------------------------------------------------------------------------------

    click : function () { 
        isc.Canvas.hideResizeThumbs(); 
    },

    // enable editing mode for the entire EditPane: turn editing on for all edit components
    setEditMode : function (editingOn) {
        if (editingOn == null) editingOn = true;
        if (this.editingOn == editingOn) return;
        this.editingOn = editingOn;

        var liveObjects = this.getEditNodeArray().getProperty("liveObject");
        liveObjects.map("setEditMode", editingOn, this);
    },

    // save new coordinates to defaults on resize or move
    childResized : function (liveObject) {
        var result = this.Super("childResized", arguments);

        this.saveCoordinates(liveObject);

        return result;
    },

    childMoved : function (liveObject, deltaX, deltaY) {
        var result = this.Super("childMoved", arguments);

        this.saveCoordinates(liveObject);

        if (!this._movingSelection) {
            // if this component is part of a selection, move the rest of the selected
            // components by the same amount   
            var selection = this.selectedComponents;
            if (selection.length > 0 && selection.contains(liveObject)) {
                this._movingSelection = true;
                for (var i = 0; i < selection.length; i++) {
                    if (selection[i] != liveObject) {
                        selection[i].moveBy(deltaX, deltaY);
                    }
                }
                this._movingSelection = false;
            }
        }

        return result;
    },
    
    saveCoordinates : function (liveObject) {
        if (!this.persistCoordinates) return;

        //this.logWarn("saveCoordinates for: " + liveObject + 
        //             ", editComponents are: " + this.echoAll(this.getEditNodeArray()));
        var component = this.getEditNodeArray().find("liveObject", liveObject);

        // can happen if we get a resized or moved notification while a component is being
        // added or removed
        if (!component) return; 

        this.setNodeProperties(component, {
            left: liveObject.getLeft(),
            top: liveObject.getTop(),
            // Use percentage width or "*" if supplied
            width: liveObject._percent_width || liveObject._userWidth || liveObject.getWidth(),
            height: liveObject._percent_height || liveObject._userHeight || liveObject.getHeight()
        }, true);
    },
    
    // Serialization
    // ---------------------------------------------------------------------------------------

    //> @method editPane.getSaveData()
    // Returns an Array of +link{PaletteNode}s representing all current +link{EditNode}s in this
    // pane, suitable for saving and restoring via passing each paletteNode to +link{EditContext.addNode(),addNode()}.
    // @return (Array of PaletteNode) paletteNodes suitable for saving for subsequent restoration 
    //
    // @visibility external
    //<
    getSaveData : function () {
        // get all the components being edited
        var data = this.getEditNodeTree(),
            editComponents = data.getChildren(data.getRoot()),
            allSaveData = [];
        for (var i = 0; i < editComponents.length; i++) {
            var component = editComponents[i],
                liveObject = component.liveObject;
            // save off just types and initialization data, not the live objects themselves
            var saveData = {
                type : component.type,
                defaults : component.defaults
            };
            // let the object customize it
            if (liveObject.getSaveData) saveData = liveObject.getSaveData(saveData);
            allSaveData.add(saveData);
        }
        return allSaveData;
    },

    // Hoop selection
    // --------------------------------------------------------------------------------------------

    // create selector hoop
    canMultiSelect:true,
    mouseDown : function () {
        if (!this.editingOn || !this.canMultiSelect ||
            // don't start hoop selection unless the mouse went down on the EditPane itself, as
            // opposed to on one of the live objects
            isc.EH.getTarget() != this) return;
        
        var target = isc.EH.getTarget();
        if (this.selector == null) {
            this.selector = isc.Canvas.create({
                autoDraw:false,
                keepInParentRect: true,
                left: isc.EH.getX(),
                top: isc.EH.getY(),
                redrawOnResize:false,
                overflow: "hidden",
                border: "1px solid blue"
            });
            this.addChild(this.selector);
        }
        this.startX = this.getOffsetX();
        this.startY = this.getOffsetY();

        this.resizeSelector();
        this.selector.show();
    },

    // resize hoop on dragMove
    dragMove : function() {
        if (this.selector) this.resizeSelector();
    },

    // hide selector hoop on mouseUp or dragStop
    mouseUp : function () {
        if (this.selector) this.selector.hide();
    },
    dragStop : function() {
        if (this.selector) this.selector.hide();
    },

    outlineBorderStyle : "2px dashed red",
    // add an outline, indicating selection to a set of components
    setOutline : function (components) {
        if (!components) return;
        if (!isc.isAn.Array(components)) components = [components];
        for (var i = 0; i < components.length; i++) {
            components[i]._editMask.setBorder(this.outlineBorderStyle);
        }
    },

    // clear outline on a set of components
    clearOutline : function (components) {
        if (!components) return;
        if (!isc.isAn.Array(components)) components = [components];
        for (var i = 0; i < components.length; i++) {
            components[i]._editMask.setBorder("none");
        }
    },

    // figure out which components intersect the selector hoop, and show the selected outline on
    // those
    updateCurrentSelection : function () {
        if (!this.children) return;
        var oldSelection = this.selectedComponents;

        // make a list of all the children which currently intersect the selection hoop
        this.selectedComponents = [];
        for (var i = 0; i < this.children.length; i++) {
            var child = this.children[i];
            if (this.selector.intersects(child)) {
                child = this.deriveSelectedComponent(child);
                if (child && !this.selectedComponents.contains(child)) {
                    this.selectedComponents.add(child);
                }
            }
        }

        // set outline on components currently within the hoop
        this.setOutline(this.selectedComponents);
    
        // de-select anything that is no longer within the hoop
        oldSelection.removeList(this.selectedComponents);
        this.clearOutline(oldSelection);
    
        // show selection in window.status
        
        var selection = this.selectedComponents.getProperty("ID");
        window.status = selection.length ? "Current Selection: " + selection : "";
    },

    // given a child in the editPane, derive the editComponent if there is one
    deriveSelectedComponent : function (comp) {
        // if the component has a master, it's either an editMask or a peer of some editComponent
        if (comp.masterElement) return this.deriveSelectedComponent(comp.masterElement);
        if (!comp.parentElement || comp.parentElement == this) {
            // if it has an event mask, it's an edit component
            if (comp._editMask) return comp;
            // otherwise it's a mask
            return null;
        }
        // XXX does this case exist?  how can a direct child have a parent element other than its
        // parent?
        return this.deriveSelectedComponent(comp.parentElement);
    },

    // resize selector to current mouse coordinates
    resizeSelector : function () {
        var x = this.getOffsetX(),
            y = this.getOffsetY();

        if (this.selector.keepInParentRect) {
            if (x < 0) x = 0;
            var parentHeight = this.selector.parentElement.getVisibleHeight();
            if (y > parentHeight) y = parentHeight;
        }
    
        // resize to the distances from the start coordinates
        this.selector.resizeTo(Math.abs(x-this.startX), Math.abs(y-this.startY));

        // if we are above/left of the origin set top/left to current mouse coordinates,
        // otherwise to start coordinates.
        if (x < this.startX) this.selector.setLeft(x);
        else this.selector.setLeft(this.startX);

        if (y < this.startY) this.selector.setTop(y);
        else this.selector.setTop(this.startY);

        // figure out which components are now in the selector hoop
        this.updateCurrentSelection();
    },
    
    // external, safe getter for selected components
    getSelectedComponents : function () {
        return this.selectedComponents.duplicate()
    }
});


//> @class EditTree
// A TreeGrid that allows drag and drop creation and manipulation of a tree of 
// objects described by DataSources.
// <P>
// Nodes can be added via drag and drop from a +link{Palette} or may be programmatically 
// added via +link{EditContext.addNode(),addNode()}.  Nodes may be dragged within the tree to reparent 
// them.
// <P>
// Eligibility to be dropped on any given node is determined by inspecting the
// DataSource of the parent node.  Drop is allowed only if the parent schema has
// a field which accepts the type of the dropped node.
// <P>
// On successful drop, the newly created component will be added to the parent node under the
// detected field.  Array fields, declared by setting
// <code>dataSourceField.multiple:true</code>, are supported.  
// <P>
// An EditTree is initialized by setting +link{EditTree.rootComponent}.  EditTree.data (the
// Tree instance) should never be directly set or looked at.
//
// @treeLocation Client Reference/Tools/EditContext
// @implements EditContext
// @group devTools
// @visibility external
//<



// Class will not work without TreeGrid
if (isc.TreeGrid) {

isc.ClassFactory.defineClass("EditTree", "TreeGrid", "EditContext");

isc.EditTree.addProperties({
    //> @attr EditTree.rootComponent    (Object : null : IR)
    // Root of data to edit.  Must contain the "_constructor" property, with the name of a
    // valid +link{DataSource,schema} or nothing will be able to be dropped on this EditTree.
    // <P>
    // Can be retrieved at any time.
    //
    // @group devTools
    // @visibility external
    //<

    canDragRecordsOut: false,
	canAcceptDroppedRecords: true,
    canReorderRecords: true,

	fields:[
	    {name:"ID", title:"ID", width:"*"},
	    {name:"type", title:"Type", width:"*"}
	],

	selectionType:isc.Selection.SINGLE,

    // whether to automatically show parents of an added node (if applicable)
    autoShowParents:true
});

isc.EditTree.addMethods({
    initWidget : function () {
        this.Super("initWidget", arguments);
        this.setData(this.getEditNodeTree());
    },

    // Adding / Removing components in the tree
	// --------------------------------------------------------------------------------------------

    willAcceptDrop : function () {
        if (!this.Super("willAcceptDrop",arguments)) return false;
	    var recordNum = this.getEventRow(),
		    dropTarget = this.getDropFolder(),
            dragData = this.ns.EH.dragTarget.getDragData()
        ;
        
        if (dragData == null) return false;
        if (isc.isAn.Array(dragData)) {
            if (dragData.length == 0) return false;
            dragData = dragData[0];
        }
        
        if (dropTarget == null) dropTarget = this.data.getRoot();
        var dragType = dragData.type || dragData.className;

        this.logInfo("checking dragType: " + dragType + 
                     " against dropLiveObject: " + dropTarget.liveObject, "editing");

        return this.canAddToParent(dropTarget, dragType)
    },

    folderDrop : function (nodes, parent, index, sourceWidget) {
        if (sourceWidget != this && !sourceWidget.isA("Palette")) {
            // if the source isn't a Palette, do standard drop interaction
            return this.Super("folderDrop", arguments);
        }

        if (sourceWidget != this) {
            // this causes component creation since the drop is from a Palette
            nodes = sourceWidget.transferDragData();
        }

        var newNode = (isc.isAn.Array(nodes) ? nodes[0] : nodes);

        // flag that this node was dropped by a user
        newNode.dropped = true;

        this.logInfo("sourceWidget is a Palette, dropped node of type: " + newNode.type,
                     "editing");

        var editTree = this;
        this.requestLiveObject(newNode, function (node) {
            if (node == null) return;
            // self-drop: remove component from old location before re-adding
            var selfDrop = sourceWidget == editTree;
            if (selfDrop) {
                // If we're self-dropping to a slot further down in the same parent, this will
                // cause the index to become off by one
                var oldParent = this.data.getParent(newNode);
                if (parent == oldParent) {
                    var oldIndex = this.data.getChildren(oldParent).indexOf(newNode);
                    if (oldIndex != null && oldIndex <= index) index--;
                }
                editTree.removeNode(newNode);
            }
            
            editTree.addNode(node, parent, index);

            // special case tabs to add default pane
            if (
                !selfDrop && node && parent && 
                (node.type || node.className) == "Tab" && 
                (parent.type || parent.className) == "TabSet"
            ) {
                var liveTabSet = parent.liveObject;
                if (liveTabSet) liveTabSet.editModeAddDefaultPane(node);
            }
        }, sourceWidget)
        
    },

    nodeAdded : function (newNode) {
        this.selection.selectSingle(newNode);
        if (this.autoShowParents) this.showParents(newNode);
        
        // Flip it into edit mode depending on the setting on the VB instance
        
        if (this.creator && this.creator.editingOn) this.enableEditing(newNode);
    },

    // for a node being added without a parent, find a plausible default node to add to.
    // In combination with palette.defaultEditContext, allows double-click (tree, list
    // palettes) as an alternative to drag and drop.
    getDefaultParent : function (newNode, returnNullIfNoSuitableParent) {
        // rules:
        // Start with the selected node. We select on drop / create, so this is typically
        // the last added node, but the user can select something else to take control of
        // where the double-click add goes
        // If this node accepts this type as a child, use that.
        // - handles most layout nesting, DataSource for last form, etc
        // Otherwise, go up hierarchy from this node
        // - handles a series of components that should not nest being placed adjacent instead,
        //   eg ListGrid then DynamicForm
        var type = newNode.type || newNode.className,
            node = this.getSelectedRecord();
        
        while (node && !this.canAddToParent(node, type)) node = this.data.getParent(node);
        
        var root = this.data.getRoot()
        if (returnNullIfNoSuitableParent) {
            if (!node && this.canAddToParent(root, type)) return root;
            return node;
        }
        return node || root;
    },
    
    // give a newNode, ensure all of it's parents are visible
    showParents : function (newNode) {
        // if something is dropped under a tab, ensure that tab gets selected
        var parents = this.data.getParents(newNode), 
            tabNodes = parents.findAll("type", "Tab");
        //this.logWarn("detected tab parents: " + tabNodes);
        if (tabNodes) {
            for (var i = 0; i < tabNodes.length; i++) {
                var tabNode = tabNodes[i],
                    tabSetNode = this.data.getParent(tabNode),
                    tab = this.getLiveObject(tabNode),
                    tabSet = this.getLiveObject(tabSetNode);
                tabSet.selectTab(tab);
            }
        }
    }
});

//> @groupDef visualBuilder
// The SmartClient Visual Builder tool is intended for:
// <ul>
// <li> business analysts and others doing functional application design, who want to create
// functional prototypes in a codeless, "what you see is what you get" environment
// <li> developers new to SmartClient who want to get a basic familiarity with component
// layout, component properties and SmartClient code structure
// <li> developers building simple applications that can be completed entirely within Visual
// Builder
// </ul>
// <P>
// <b>Visual Builder for Functional Design</b>
// <P>
// Visual Builder has several advantages over other tools typically used for functional design:
// <ul>
// <li> Visual Builder allows simple drag and drop manipulation of components, form-based
// editing of component properties, and simple connection of events to actions - all without
// requiring any code to be written.  It is actually simpler to use than
// DreamWeaver or other code-oriented prototyping tools
// <li> because Visual Builder generates clean code, designs will not have to be converted to
// another technology before development can proceed.  This reduces both effort and the
// potential for miscommunication
// <li> developers can add custom skinning, components with custom behaviors, and custom
// DataSources with sample datasets to Visual Builder so that the design environment is an even
// closer match to the final application.  This helps eliminate many types of unimplementable
// designs 
// <li> because Visual Builder is built in SmartClient itself, Visual Builder is simply a 
// web page, and does not require installation.  Visual Builder can be deployed to 
// an internal network to allow teams with a mixture of technical and semi-technical 
// users to collaboratively build and share prototypes of SmartClient-based applications.  
// </ul>
// <P>
// <h4>Launching &amp; Using Visual Builder</h4>
// <P>
// <smartclient>The SmartClient SDK already has Visual Builder installed - access
// it from the SDK Explorer under Tools -&gt; Visual Builder (see QuickStart Guide for how to
// access the SDK Explorer).
// </smartclient>
// <smartgwt>Instructions for launching Visual Builder are in the 
// +externalLink{http://forums.smartclient.com/showthread.php?t=8159#aVisualBuilder,Smart GWT FAQ}.
// </smartgwt>
// <P>
// Basic usage instructions are embedded in Visual Builder itself, in the "About Visual
// Builder" pane.  Click on it to open it.
// <P>
// <b>Loading and Saving</b>
// <P>
// The "Project" pane within Visual Builder allows screens to be saved and reloaded for further
// editing.  Saved screens <b>can</b> be edited outside of Visual Builder and successfully
// reloaded, however, as with any design tool that provides a drag and drop, dialog-driven
// approach to screen creation, Visual Builder cannot work with entirely free-form code.  In
// particular, when a screen is loaded and then re-saved:
// <ul>
// <li> any indenting or spacing changes are not preserved 
// <li> order of property or method definitions will revert to Visual Builder's default
// <li> while method definitions on components are preserved, any code <b>outside of</b>
//      component definitions will be dropped (in some cases adding such code will cause
//      loading to fail)
// <li> each Canvas-based component will be output separately, in the order these components
//      appear in the project tree, deepest first
// </ul>
// Generally speaking, screen definitions that you edit within Visual Builder should consist of
// purely declarative code.  Rather than appearing in screen definitions, custom components and
// JavaScript libraries should be added to Visual Builder itself via the customization
// facilities described below.
// <P>
// <smartclient>
// <!-- applies only to SmartClient since SmartGWT has a GWT module listing these resources -->
// <h4>Installing Visual Builder</h4>
// <P>
// Visual Builder comes already installed and working in the SDK, and can be used from there out
// of the box.  This is the simplest thing to do during initial prototyping.
// <P>
// Further on in the development cycle, it may be advantageous to have Visual Builder available 
// outside the SDK, for example in your test environment.  Installing Visual Builder into 
// such an environment is very easy:
// <ul>
// <li>Perform a normal installation procedure, as discussed +link{group:iscInstall,here}</li>
// <li>Copy the following .jar files from the SDK <code>lib</code> folder to the target 
// <code>WEB-INF/lib</code> folder: 
// <ul>
// <li><code>isomorphic_tools.jar</code></li>
// <li><code>isomorphic_sql.jar</code></li>
// <li><code>isomorphic_hibernate.jar</code></li>
// </ul></li>
// <li>Copy the SDK <code>tools</code> folder to the target application root</li>
// </ul>
// Note that it is safe to include Visual Builder even in a production environment, so long 
// as you ensure that the <code>tools</code> folder is protected with any normal HTTP
// authentication/authorization mechanism - for example, an authentication filter.
// </smartclient>
// <P>
// <h4>Customizing Visual Builder</h4>
// <P>
// The rest of this topic focuses on how Visual Builder can be customized and deployed by
// developers to make it more effective as a functional design tool for a particular
// organization.
// <P>
// <b>Adding Custom DataSources to Visual Builder</b>
// <P>
// DataSources placed in the project dataSources directory ([webroot]/shared/ds by default)
// will be detected by Visual Builder whenever it is started, and appear in the DataSource
// listing in the lower right-hand corner automatically.
// <P>
// If you have created a custom subclass of DataSource (eg, as a base class for several
// DataSources that contact the same web service), you can use it with Visual Builder by:
// <ul>
// <li> creating an XML version of the DataSource using the XML tag &lt;DataSource&gt; and the
// <code>constructor</code> property set to the name of your custom DataSource subclass (as
// described +link{group:componentXML} under the heading <i>Custom Components</i>)
// <li> modifying [webroot]/tools/visualBuilder/globalDependencies.xml to load the JavaScript
// code for your custom DataSource class.  See examples in that file.
// </ul>
// <P>
// <b>Adding Custom Components to Visual Builder</b>
// <P>
// The Component Library on the right hand side of Visual Builder loads component definitions
// from two XML files in the [webroot]/tools/visualBuilder directory: customComponents.xml and
// defaultComponents.xml.  customComponents.xml is empty and is intended for developers to add
// their own components.  defaultComponents.xml can also be customized, but the base version
// will change between SmartClient releases.
// <P>
// As can be seen by looking at defaultComponents.xml, components are specified using a tree
// structure similar to that shown in the 
// +explorerExample{treeLoadXML,tree XML loading example}.  The properties that can be set on
// nodes are:
// <ul>
// <li> <code>type</code>: name of the SmartClient Class on which +link{Class.create,create()} will be
// called in order to construct the component.  <code>type</code> can be omitted to create
// a folder that cannot be dropped
// <li> <code>title</code>: title for the node
// <li> <code>defaults</code>: an Object specifying defaults to be passed to
// +link{Class.create,create()}.
// For example, you could add an "EditableGrid" node by using <code>type: "ListGrid"</code>
// and specifying:
// <pre>
// &lt;defaults canEdit="true"/&gt;</pre>
// NOTE: if you set any defaults that are not Canvas properties, you need to provide explicit
// type as documented under <i>Custom Properties</i> for +link{group:componentXML}.
// <li> <code>children</code>: components that should appear as children in the tree under this
// node
// <li> <code>icon</code>: icon to show in the Visual Builder component tree (if desired)
// <li> <code>iconWidth/Height/Size</code>: dimensions of the icon in pixels ("iconSize" sets
// both)
// <li> <code>showDropIcon</code>: for components that allow children, whether to show a
// special drop icon on valid drop (like +link{treeGrid.showDropIcons}).
// </ul>
// <P>
// In order to use custom classes in Visual Builder, you must modify
// <code>[webroot]/tools/visualBuilder/globalDependencies.xml</code> to include:
// <ul>
// <li> the JavaScript class definition for the custom class (in other words, the
// +link{classMethod:isc.defineClass(),defineClass()} call)
// <li> a +link{group:componentSchema,component schema} for the custom component
// </ul>
// See globalDependencies.xml for examples.
// <P>
// <h4>Component Schema and Visual Builder</h4>
// <P>
// When you provide +link{group:componentSchema,custom schema} for a component, Visual Builder
// uses that schema to drive component editing (Component Properties pane) and to drive drag
// and drop screen building functionality.
// <P>
// <b>Component Editing</b>
// <P>
// Newly declared fields will appear in the Component Editor in the "Other" category at the
// bottom by default.  You can create your own category by simply setting field.group to the
// name of a new group and using this on multiple custom fields.
// <P>
// The ComponentEditor will pick a FormItem for a custom field by the
// +link{type:FormItemType,same rules} used for ordinary databinding, including the ability to
// set field.editorType to use a custom FormItem.
// <P>
// When the "Apply" button is clicked, Visual Builder will look for an appropriate "setter
// function" for the custom field, for example, for a field named "myProp", Visual Builder will
// look for "setMyProp".  The target component will also be +link{canvas.redraw,redrawn}.
// <P>
// <b>Event -&gt; Action Bindings</b>
// <P>
// The Component Properties pane contains an Events tab that allows you wire components events
// to actions on any other component currently in the project.
// <P>
// Events are simply +link{group:stringMethods,StringMethods} defined on the component.  In
// order to be considered events, method definitions must have been added to the class via
// +link{Class.registerStringMethods} and either be publicly documented SmartClient methods or,
// for custom classes, have a methods definition in the +link{group:componentSchema,component schema}.
// Examples of events are: +link{listGrid.recordClick} and +link{dynamicForm.itemChange}.
// <P>
// Actions are methods on any component that have a method definition in the
// +link{group:componentSchema,component schema} and specify action="true".
// <P>
// All available events (stringMethods) on a component are shown in the Events tab of the
// Component Editor.  Clicking the plus (+) sign next to the event name brings up a menu that
// shows a list of all components currently in the project and their available actions.
// Selecting an action from this submenu binds the action to the selected event.  When an event
// is bound to an action in this manner, automatic type matching is performed to pass arguments
// from the event to the action as follows:
// <ul>
// <li>Only non-optional parameters of the action are bound.
// <li>For each non-optional parameter of the action method, every parameter of the
// event method is inspected in order to either directly match the type (for non-object types)
// or to match an isAssignableFrom type check via a SmartClient schema inheritance check.
// <li>The 'type' of a parameter is determined from the type documented in the SmartClient
// reference for built-in components, or from the <code>type</code> attribute on the method
// param in the +link{group:componentSchema,component schema} definition of a custom component.
// <li>When a matching parameter is found, it is assigned to the current slot of the action and
// not considered for further parameter matching.
// <li>The above pattern is repeated until all non-optional parameters are exhausted, all
// event parameters are exhausted, or until no further type matches can be inferred.
// </ul>
// The "actionBinding" log category can be enabled in the Developer Console to troubleshoot
// issues with automatic binding for custom methods.
// <P>
// <b>Component Drag and Drop</b>
// <P>
// Visual Builder uses component schema to determine whether a given drop is allowed and what
// methods should be called to accomplish the drop.  For example, any Canvas-based component
// can be dropped on a VLayout because VLayout has a "members" field of type "Canvas", and an 
// +link{Layout.addMember,addMember()} function.
// <P>
// Because of these rules, any subclass of Canvas will be automatically eligible to be dropped
// into any container that accepts a Canvas (eg, a Layout or Tab).  Any subclass of a FormItem
// will be, likewise, automatically eligible to be dropped into a DynamicForm.
// <P>
// You can declare custom containment relations, such as a custom class "Wizard" that accepts
// instances of the custom class "Pane" by simply declaring a
// +link{group:componentSchema,component schema} that says that Wizard has a property called
// "panes" of type "Pane".  Then, provide methods that allow components to be added and removed:
// <ul>
// <li> for a +link{dataSourceField.multiple,multiple} field, provide "add" and "remove"
// functions based on the name of the field.  For example, for a field "panes" of type "Pane",
// provide "addPane()" that takes a Pane instance, and "removePane()" that takes a pane
// instance or pane ID 
// <li> for a singular field (such as +link{Canvas.contextMenu} or +link{Tab.pane}), provide a
// setter method named after the field (eg setContextMenu()) that takes either an instance of
// the component or null for removal
// </ul>
// <P>
// The "editing" log category can be enabled in the Developer Console to troubleshoot issues
// with schema-driven drag and drop and automatic lookup of getter/setter and adder/remover
// methods.
// <P>
// <B>NOTE:</B> after modifying component schema, it may be necessary to restart the servlet
// engine and reload Visual Builder
// <P>
// <b>Presenting simplified components</b>
// <P>
// SmartClient components expose many methods and properties.  For some environments, it is
// more appropriate to provide a simplified list of properties, events, and actions on either
// built-in SmartClient components or your custom components.  This can be done by providing a
// custom +link{group:componentSchema,component schema} for an existing component that exposes
// your minimal set.  You also need to provide a trivial subclass of the class you're exposing
// so that it can be instantiated.
// <P>
// For example, let's say you want to make a simplified button called EButton that exposes only
// the 'title' property and the 'click' event of a standard Button.  The following steps will
// accomplish this:
// <p>
// 1. Edit /tools/visualBuilder/customComponents.xml and add a block similar to the following
// to make your custom component appear in the Component Library:
// <pre>
// &lt;PaletteNode&gt;
//     &lt;title&gt;EButton&lt;/title&gt;
//     &lt;type&gt;EButton&lt;/type&gt;
//     &lt;icon&gt;button.gif&lt;/icon&gt;
// &lt;/PaletteNode&gt;
// </pre>
// 2. Next, create a custom schema: /isomorphic/system/schema/EButton.ds.xml as follows:
// <pre>
// &lt;DataSource ID="EButton" inheritsFrom="Button" Constructor="EButton"
//             showLocalFieldsOnly="true" showSuperClassActions="false"
//             showSuperClassEvents="false"&gt;
// 	   &lt;fields&gt;
//         &lt;field name="title"  type="HTML"/&gt;
//     &lt;/fields&gt;
//     &lt;methods&gt;
//         &lt;method name="click"&gt;
//             &lt;description&gt;Fires when this button is clicked.&lt;/description&gt;
//         &lt;/method&gt;
//     &lt;/methods&gt;
// &lt;/DataSource&gt;
// </pre>
// See documentation above and also +link{group:componentSchema,component schema} for what the
// properties above do.
// 3.  Finally, you'll need to define an EButton class as a simple subclass of Button, as
// follows:
// <pre>
// isc.defineClass("EButton", "Button");
// </pre>
// To make sure that the Visual Builder will load the above definition, you'll need to place it
// into a JavaScript file being loaded by the Visual Builder.  If you do not already have
// such a file, you can create one and add it to the list of Visual Builder dependencies by
// adding an entry in /tools/visualBuilder/globalDependencies.xml.  See examples in that file
// for specifics.
//
// @see group:toolsDeployment
//
// @treeLocation Concepts
// @title Visual Builder
// @visibility external
//<

} // end if (isc.TreeGrid)


// -----------------------------------------------------------------------------------------
// DynamicForm.rolloverControls

// INCOMPLETE IMPLEMENTATION - commented out for now
/*
isc.DynamicForm.addProperties({
    rolloverControlsLayoutDefaults: [],
    rolloverControls: []
    
});

isc.DynamicForm.addMethods({
    showRolloverControls : function (item) {
        var controls = this.getRolloverControls(item),
            layout = this.rolloverControlsLayout;
        layout.item = item;
        layout.setPageLeft();
        layout.moveTo(item.getPageLeft()+item.getPageWidth(), item.getPageTop());
    },
    hideRolloverControls : function (item) {
        this.rolloverControlsLayout.hide();
    },
    getRolloverControls : function (item) {
        if (!this.rolloverControlsLayout) {
            this.createRolloverControls(item);
        }

        return this.rolloverControls;
    },
    createRolloverControls : function (item) {
        this.addAutoChild("rolloverControlsLayout");
        this.createRolloverControls(item);
    }
});
*/

// This is a marker class for FormItem drag-and-drop in edit mode.  We use an instance of 
// this class (for efficiency, we just keep one cached against the EditContext class) so 
// that the DnD code knows we're really dragging a FormItem, which will be present on this 
// proxy canvas as property "formItem".
isc.ClassFactory.defineClass("FormItemProxyCanvas", "Canvas");

isc.FormItemProxyCanvas.addProperties({
    autoDraw: false,
    canDrop: true,
    setFormItem : function (formItem) {
        this.formItem = formItem;
        this.syncWithFormItemPosition();
        this.sendToBack();
        this.show();
    },
    syncWithFormItemPosition : function () {
        if (!this.formItem || !this.formItem.form) return; // formItem not yet part of a form?
        this.setPageLeft(this.formItem.getPageLeft());
        this.setPageTop(this.formItem.getPageTop());
        this.setWidth(this.formItem.getVisibleWidth());
        this.setHeight(this.formItem.getVisibleHeight());
    }
});




//> @attr paletteNode.canvasDefaults (Canvas Properties : null : IR)
// @include paletteNode.defaults
// @visibility sgwt
//<
//> @attr paletteNode.formItemDefaults (FormItem Properties : null : IR)
// @include paletteNode.defaults
// @visibility sgwt
//<
//> @attr paletteNode.drawItemDefaults (DrawItem Properties : null : IR)
// @include paletteNode.defaults
// @visibility sgwt
//<
//> @attr paletteNode.canvasLiveObject (Canvas : null : IR)
// @include paletteNode.liveObject
// @visibility sgwt
//<
//> @attr paletteNode.formItemLiveObject (FormItem : null : IR)
// @include paletteNode.liveObject
// @visibility sgwt
//<
//> @attr paletteNode.drawItemLiveObject (DrawItem : null : IR)
// @include paletteNode.liveObject
// @visibility sgwt
//<

//> @attr editNode.canvasDefaults (Canvas Properties : null : IR)
// @include editNode.defaults
// @visibility sgwt
//<
//> @attr editNode.formItemDefaults (FormItem Properties : null : IR)
// @include editNode.defaults
// @visibility sgwt
//<
//> @attr editNode.drawItemDefaults (DrawItem Properties : null : IR)
// @include editNode.defaults
// @visibility sgwt
//<
//> @attr editNode.canvasLiveObject (Canvas : null : IR)
// @include editNode.liveObject
// @visibility sgwt
//<
//> @attr editNode.formItemLiveObject (FormItem : null : IR)
// @include editNode.liveObject
// @visibility sgwt
//<
//> @attr editNode.drawItemLiveObject (DrawItem : null : IR)
// @include editNode.liveObject
// @visibility sgwt
//<



if (!(isc.licenseType == "Enterprise" || isc.licenseType == "Eval" ||
      isc.licenseType == "AllModules" || isc.licenseType.contains("licenseType")))
{
    
    [
        "EditContext", "Palette", "HiddenPalette", "TreePalette", "ListPalette", "TilePalette",
        "MenuPalette", "EditPane", "EditTree", "FormItemProxyCanvas"
    ].map(function (editModeClass) {
        isc[editModeClass]._vbOnly = true;
    });
}

//> @groupDef toolsDeployment
// SmartClient provides a number of tools:
// <ul> 
// <li> +link{group:adminConsole}
// <li> +link{group:visualBuilder}
// <li> +link{group:balsamiqImport}
// </ul>
// <P>
// To deploy the tools simply copy the <code>tools</code> directory into your deployment. There are no
// additional settings to configure.
// <P>
// <h4>Security</h4>
// <P>
// These tools are, by default, available to anyone and enable access to all "BuiltinRPCs"
// and the Filesystem DataSource so they should only be deployed into a trusted environment.
// Alternately, the tools can easily be restricted to administrators or end users
// by protecting the <code>tools</code> path with normal authentication and authorization
// mechanisms on the web server.
// <P>
// More fine-grained access control can be installed by updating each tool's <code>xxxOperations.jsp</code>
// file (ex. tools/adminConsoleOperations.jsp, tools/visualBuilder/vbOperations.jsp). These files are
// responsible for enabling builtinRPC and FileSystem DataSource access. Individual
// BuiltinRPC methods can be restricted, for example, such that some users are allowed to load screens but
// not save any changes. See comments within each file for an example of restricting this access.
// See the server-side Javadocs for methods provided by <code>BuiltinRPC</code>.
// <P>
// Note that the tools provides a "live" interface to the provided DataSources. In
// other words, if a DataSource supports saving and a tool enables editing, real saves will be
// initiated. 
//
// @title Tools Deployment
// @treeLocation Concepts/Deploying SmartClient
// @visibility external
//<
